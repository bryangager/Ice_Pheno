#######################################
# Random Forest and XGB Modeling  
#######################################

# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

    # Load any necessary packages 
        library(dplyr) 
        library(ggplot2)
        library(dplyr)
        library(tree)
        library(randomForest)
        library(gbm)
        library(xgboost)
        library(precrec) #for AUC

        source("source/random_partitions.R")

    # Load in data from BDG  
        loch_raw <- read.csv("data/derived_data/imputed_data_trimmed_14_23.csv") # 2014 - 2023 imputed daily data trimmed to only spring (ice off)
        loch_raw$Date <- as.POSIXct(loch_raw$Date)
        str(loch_raw)

        full_timeseries <- read.csv("data/derived_data/flow_temp_cond_impute_full.csv")

        # Data for fall (to predict ice on )
            # imputed_data_trimmed_14_23_winter <- read.csv( "data/derived_data/imputed_data_trimmed_14_23_winter.csv")
            
    # Looking at data 

        # plot individual predictors 
        # loch_out %>%
        #     ggplot(aes(x = wy_doy, y = temperature_C_impute)) + 
        #     geom_point(alpha = 0.5) + 
        #     theme_minimal() + 
        #     facet_wrap(~waterYear,scales = "free_x")

    # plot all together 
        # loch_raw %>%
        #     mutate(
        #     cond_scaled = scales::rescale(cond_uScm_impute, to = range(ice_or_no, na.rm = TRUE)),
        #     temp_scaled = scales::rescale(temperature_C_impute, to = range(ice_or_no, na.rm = TRUE)), 
        #     cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(ice_or_no, na.rm = TRUE)), 
        #     q_scaled = scales::rescale(Flow, to = range(ice_or_no, na.rm = TRUE))
        #     ) %>%
        #     ggplot(aes(x= Date)) +
        #     geom_point(aes(y = ice_or_no), color = "skyblue3", alpha = 0.75) + 
        #     geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
        #     geom_point(aes(y = temp_scaled), color = "salmon3", alpha = 0.75) + 
        #     geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) + 
        #     geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) + 
        #     theme_minimal() + 
        # facet_wrap(~waterYear, scales = "free")

# __________________________________________________
# Feature Engineering 
# __________________________________________________

    # Organize columns a bit first 
    loch_out <- subset(loch_raw, select = c("waterYear", "wy_doy", "Date", "ice_or_no", "temperature_C_impute", "cumulative_dis", "cond_uScm_impute", "Flow"))
    names(loch_out)[names(loch_out) == "ice_or_no"] <- "ice"
    loch_out$ice <- as.factor(loch_out$ice)

    # Remove any rows with NA in the temp_change_3day column 
    loch_out <- loch_out %>%
    tidyr::drop_na()

    # Add a column that counts the number of days that the water temp has been above thresholds 
    loch_out <- loch_out %>%
    group_by(waterYear) %>% # group by water year because we want this count within each water year 
    arrange(Date, .by_group = TRUE) %>% # make sure everything is in order by day 
    mutate(
        cum_days_temp_above2 = cumsum(temperature_C_impute > 2),
        cum_days_temp_above4 = cumsum(temperature_C_impute > 4), 
        cum_days_temp_above6 = cumsum(temperature_C_impute > 6),
    ) %>% #calculate the cumulative sum of rows (days) when water temp was above 4C 
    ungroup()

    # Add rates of change at different time intervals for temp, conductivity, and cumulative discharge 
    loch_out <- loch_out %>%
    group_by(waterYear) %>%
    arrange(Date, .by_group = TRUE) %>% #arrange by date within the groups 
    mutate(
        # Note this is set up so that the difference in temp is since the first day of the water year for the early days 
        # Rate of change in temperature 
        temp_change_3day = (temperature_C_impute - lag(temperature_C_impute, n = 3, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
        temp_change_5day = (temperature_C_impute - lag(temperature_C_impute, n = 5, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
        temp_change_10day = (temperature_C_impute - lag(temperature_C_impute, n = 10, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
        temp_change_15day = (temperature_C_impute - lag(temperature_C_impute, n = 15, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
        temp_change_20day = (temperature_C_impute - lag(temperature_C_impute, n = 20, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 
        # Rate of change in cumulative flow 
        cumq_change_3day = (cumulative_dis - lag(cumulative_dis, n = 3, default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
        cumq_change_5day = (cumulative_dis - lag(cumulative_dis, n = 5,  default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
        cumq_change_10day = (cumulative_dis - lag(cumulative_dis, n = 10, default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
        cumq_change_15day = (cumulative_dis - lag(cumulative_dis, n = 15,  default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
        cumq_change_20day = (cumulative_dis - lag(cumulative_dis, n = 20,  default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 
        # Rate of change in conductivity
        cond_change_3day = (cond_uScm_impute - lag(cond_uScm_impute, n = 3, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
        cond_change_5day = (cond_uScm_impute - lag(cond_uScm_impute, n = 5, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
        cond_change_10day = (cond_uScm_impute - lag(cond_uScm_impute, n = 10, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
        cond_change_15day = (cond_uScm_impute - lag(cond_uScm_impute, n = 15, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
        cond_change_20day = (cond_uScm_impute - lag(cond_uScm_impute, n = 20, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 

    ) %>%
    ungroup() # %>%
    # slice_sample(prop = 1) # remove the arrange by date 

    str(loch_out)

    # Look at the NAs 
    na_summary <- loch_out %>%
      group_by(waterYear) %>%
      summarize(
        across(
          everything(),
          ~ sum(is.na(.))
        )
      )
  # Remove any rows with NA in the temp_change_3day column 
    loch_out <- loch_out %>%
    tidyr::drop_na()



    # check for class imbalance 
    # --> need to deal with slight class imbalance  in the modeling, we have fewer data points with no ice than with ice  
    balance_count <- loch_out %>%
    count(waterYear, ice) %>%
    tidyr::pivot_wider(
        names_from = ice,
        values_from = n,
        names_prefix = "ice_"
    )
    balance_count$proportion_0 <- balance_count$ice_0 /(balance_count$ice_0 + balance_count$ice_1)

    loch_out %>%
    count(ice)

    # loch_out %>%
    #         mutate(
    #         cond_scaled = scales::rescale(cond_uScm_impute, to = range(c(1,2), na.rm = TRUE)),
    #         temp_scaled = scales::rescale(temperature_C_impute, to = range(c(1,2), na.rm = TRUE)), 
    #         cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(c(1,2), na.rm = TRUE)), 
    #         q_scaled = scales::rescale(Flow, to = range(c(1,2), na.rm = TRUE))
    #         ) %>%
    #         ggplot(aes(x= Date)) +
    #         geom_point(aes(y = as.numeric(ice)), color = "skyblue3", alpha = 0.75) + 
    #         geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
    #         geom_point(aes(y = temp_scaled), color = "salmon3", alpha = 0.75) + 
    #         geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) + 
    #         geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) + 
    #         theme_minimal() + 
    #         facet_wrap(~waterYear, scales = "free")

# __________________________________________________
# Leave one Year out Accuracy for Random Forest 
# __________________________________________________

    # initialize i to step through for loop 
    i <- 3 

    # Seperate your data into partitions <-- you don't actually need to do this becase your data is already in years and that is what you want to use
        # create an object that holds all of the waterYears in the full dataset 
        years <- unique(loch_out$waterYear) 

    # Create an obect to hold the out of sample accuracy for each year 
        accuracy_rf <- rep(NA, length(years))
        ice_off_diff_rf <- rep(NA, length(years))

    # for each year in your list of years 
    for (i in 1:length(years)){
        # seperate into train and test data 
        test_year <- years[i]
        training_data <- loch_out[loch_out$waterYear != test_year, ]
        test_data <- loch_out[loch_out$waterYear == test_year, ]
      
        # train a random forest model on training data
        n_abs <- sum(training_data$ice == 0) # get the number of days when ice was absence to use to account for class imbalance 
        trained_rf_model <- randomForest(ice ~ ., data=training_data[, -c(1:3)], ntree = 500, sampsize=c(n_abs, n_abs))
      
        # use the trained andom forest model to predict the presence or absence of ice in the test data 
        predicted_ice_prob_rf <- predict(trained_rf_model, newdata=test_data[, -c(1:4)], type="prob")[,2] # the 2 is because we only want the probability of ice presence (2nd column) not the probability of absence 
      
        # Convert the probability into a prediction 
        threshold <- 0.5 # Set the threshold probability of when you call ice presence present 
        predicted_ice_rf <- 1 * (predicted_ice_prob_rf > threshold) # if predicted probability is greater than the threshold then set to present 
      
        # Calculate the accuract of those predictions and save into the object you made to hold accuracy
        accuracy_rf[i] <-  mean(predicted_ice_rf == test_data$ice)
      
        # Calculate the number of days away from observed ice off the 
      
            # extract the day when we first observed no ice 
            ice_off_obs <- which(test_data$ice == 0)[1]
      
            # extract the day when the model first predicted no ice 
            ice_off_pred <- which(predicted_ice_rf == 0)[1] %>% 
              as.numeric()
      
            # take the difference betweent those two days and save it in the days_off_rf 
            ice_off_diff_rf[i] <- ice_off_obs -  ice_off_pred
      
    }

    # Look at the number of days off from predicted ice off each of your predictions are 
    ice_off_diff_rf_df <- as.data.frame(ice_off_diff_rf)
    ggplot(data = ice_off_diff_rf_df, aes(x = ice_off_diff_rf)) + 
        geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
        labs(
            x = "Observed - Predicted Ice Off Day"
        ) +
        theme_minimal()
    mean(ice_off_diff_rf)
    mean(abs(ice_off_diff_rf)) # on average how far away from zero are you

    mean(accuracy_rf)

    # Take a look at accuracy over each year for rf models 
    accuracy_yr_summary <- cbind(years, accuracy_rf) %>% as.data.frame()
    accuracy_yr_summary %>%
      ggplot(aes(x = years, y = accuracy_rf)) + 
      geom_point(color = "forestgreen", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "Random Forest Out of Sample Accuracy for each Year"
      )

# __________________________________________________
# Leave one Year out Accuracy for XGBoost 
# __________________________________________________

    # initialize i to step through for loop 
    i <- 4

    # Seperate your data into partitions <-- you don't actually need to do this becase your data is already in years and that is what you want to use
        # create an object that holds all of the waterYears in the full dataset 
        years <- unique(loch_out$waterYear) 

    # Create an obect to hold the out of sample accuracy for each year 
        accuracy_xgb <- rep(NA, length(years))
        ice_off_diff_xgb <- rep(NA, length(years))


    # for each year in your list of years 
    for (i in 1:length(years)){
        # seperate into train and test data 
        test_year <- years[i]
        training_data <- loch_out[loch_out$waterYear != test_year, ]
        test_data <- loch_out[loch_out$waterYear == test_year, ]
      
        # Calculate the ratio of absence to presence to deal with class imbalance 
        n_pres <- sum(training_data$ice == 1) # get the numer of presence 
        n_abs <- sum(training_data$ice == 0) # get the number of days when ice was absence 
        ab_pr_ratio <-  n_abs / n_pres # ratio of abs to presence

        # Train with xgboost model 
        trained_xgb_model <- xgboost(x=training_data[, -c(1:4)], y=training_data$ice,# x is all the columns in the dataset except the first one, y is the occupancy column 
                            learning_rat=0.01, subsample=0.5, nrounds=10000, print_every_n=1000, 
                            scale_pos_weight= ab_pr_ratio, nthread=2,# weighting parameter, scale_pos_weight, equal to the ratio of absences to presences. This is often a good starting point but this is a tunable parameter.
                            objective="binary:logistic")

        # Use the trained xgboost model to predict ice probability on the test data
        predicted_ice_prob_xgb <- predict(trained_xgb_model, newdata=test_data[, -c(1:4)])

        # use a threshold to covert from probability of ice to a binary ice or no ice 
        threshold <- 0.5
        predicted_ice_xgb <- 1 * (predicted_ice_prob_xgb > threshold)
      
        # Calculate the accuract of those predictions and save into the object you made to hold accuracy
        accuracy_xgb[i] <-  mean(predicted_ice_xgb == test_data$ice)
      
        # Calculate the number of days away from observed ice off the 
      
            # extract the day when we first observed no ice 
            ice_off_obs <- which(test_data$ice == 0)[1]
      
            # extract the day when the model first predicted no ice 
            ice_off_pred <- which(predicted_ice_xgb == 0)[1] %>% 
              as.numeric()
      
            # take the difference betweent those two days and save it in the days_off_rf 
            ice_off_diff_xgb[i] <- ice_off_obs -  ice_off_pred
    }

    mean(accuracy_xgb)
    mean(accuracy_rf)

    # Look at the number of days off from predicted ice off each of your predictions are 
        ice_off_diff_xgb_df <- as.data.frame(ice_off_diff_xgb)
        ggplot(data = ice_off_diff_xgb_df, aes(x = ice_off_diff_xgb)) + 
            geom_histogram(binwidth = 1, fill = "violetred3", color = "white") +
            labs(
                x = "Observed - Predicted Ice Off Day"
            ) +
            theme_minimal()
        mean(ice_off_diff_xgb)
        mean(abs(ice_off_diff_xgb)) # on average how far away from zero are you


    # Take a look at accuracy over each year for both models: Do they struggle with the same years?  
    accuracy_mod_summary <- cbind(years, accuracy_rf, accuracy_xgb) %>% # bind together the accuracy for each year 
      as.data.frame()

    names(accuracy_mod_summary) <- c("year", "rf", "xgb") # change names to make it easier to work with 

    # pivot longer to be able to plot 
    accuracy_mod_summary <- accuracy_mod_summary %>%
        tidyr::pivot_longer(
            cols = c(rf, xgb), # designate the columns that you want to pivot longer
            names_to = "model", # take the names of the current columns and put them in a new column called model
            values_to = "accuracy" # the values in those columns go to a column called 
        )

    # plot out of sample accuracy for each year for each model 
    accuracy_mod_summary %>%
      ggplot(aes(x = year, y = accuracy, color = model, shape = model)) + 
      geom_jitter(
        width = 0.10,   # horizontal jitter
        height = 0,     # no vertical jitter
        alpha = 0.75,
        size = 3
      ) +
      scale_color_manual(values= c(
        "rf" = "forestgreen", 
        "xgb" = "maroon"
      )) +
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "Out of Sample Accuracy for Each Year for Untuned Models "
      )

# __________________________________________________
#  Randomly generate parameters to use for tuning XGB
# __________________________________________________

    # Tune hyperparameters by random search
    # This script shows examples of generating random sets of parameters

    # Randomly draw positive integers on a log scale within a range
    # n: number to draw (integer)
    # lower: lower limit (integer)
    # upper: upper limit of the range (integer)
    #
    rand_int_log <- function(n, lower, upper) {
        if ( lower < 0 ) {
            stop("lower must be >= 0")
        }
        if ( lower == 0 ) {
            lower <- 0.5
        } 
        upper <- upper + 1
        ints <- runif(n, log(lower), log(upper)) |>
            exp() |>
            floor()
        return(ints)
    }

    # Randomly draw positive integers on a natural scale within a range
    # n: number to draw (integer)
    # lower: lower limit (integer)
    # upper: upper limit of the range (integer)
    #
    rand_int_nat <- function(n, lower, upper) {
        upper <- upper + 1
        ints <- runif(n, lower, upper) |>
            floor()
        return(ints)
    }

    # Generate random sets of xgboost parameters of different types

    # Number of parameter sets to generate (50-150 is often good)
    n <- 50 # you can set this back to 20 if it breaks 

    # integer with early stopping (i.e. auto tuned)
    p_nrounds <- round(runif(n, min=5000, max=15000)) # you can set this back to 3000 if it breaks 
    # p_early_stopping_rounds <- 200

    # continuous on log scale
    p_learning_rate <- exp(runif(n, min=log(0.001), max=log(0.1)))
                          
    # continuous on natural scale
    p_subsample <- runif(n, min=0, max=1)
                          
    rand_pars <- data.frame(p_nrounds, 
                            p_learning_rate, 
                            p_subsample)

    print(rand_pars)

    # We now run xgboost for each row of this dataframe. Which combination of parameters predicts best?
    # n = 50-150 usually competes well with more sophisticated optimization algorithms


# __________________________________________________
#  Nested K- Fold tuning for XGB 
# __________________________________________________

    # Hold out one year as you test data (cylce through to hold each year to use to test accuracy)

    # create an object that holds all of the waterYears in the full dataset 
    years_in_full_data <- unique(loch_out$waterYear) 

    # Create an object to hold the accuracy and optimized parameters 
    opt_param_and_accuaracy <- vector("list", length( years_in_full_data))

    # initialize to step through the loop 
    # k <- 5

    for(k in 1:length(years_in_full_data)){

        # Define your test year, test data, and the training/tuning data
        test_year <- years_in_full_data[k]
        test_data <- loch_out[loch_out$waterYear == test_year, ]
        train_tune_data <- loch_out[loch_out$waterYear != test_year, ] 

        # Create an object to hold all of the years including in your training and tuning dataset 
        years_in_train_tune <- unique(train_tune_data$waterYear)

        # create an empty list to hold each of the error vectors 
        e_by_year <- vector("list", length(years_in_train_tune)) # results of all model realizations 

        # Calculate error across all the tuning parameters using each year in turn as the hold out tuning dataset --------------------------
        # For each year in the years in train tune 
        for(j in 1:length(years_in_train_tune)){

            # Seperate your training and tuning data 
            tuning_year <- years_in_train_tune[j]
            tuning_data <- train_tune_data[train_tune_data$waterYear == tuning_year , ]
            training_data <- train_tune_data[train_tune_data$waterYear != tuning_year , ]

            # Caclulate Error across a set of tuning parameters using one year as your hold out tuning dataset --------------------------
                # create an object to hold the error of each row of parameters (in the random parameter df) for that year 
                e <- rep(NA, nrow(rand_pars))

                # for each row in your df od randomly generated hyper parameters, use those hyper parameters to tune the xgb model and then calculate the out of sample error of that model 
                for(i in 1:nrow(rand_pars)){

                    # train xgb model using 8 yrs of data (not 2014 or 2015) and the hyper parameters from the ith row of the rand hyper par df
                        # Calculate the ratio of absence to presence to deal with class imbalance 
                        n_pres <- sum(training_data$ice == 1) # get the numer of presence 
                        n_abs <- sum(training_data$ice == 0) # get the number of days when ice was absence 
                        ab_pr_ratio <-  n_abs / n_pres # ratio of abs to presence

                        # Train with xgboost model 
                        trained_xgb_model <- xgboost(
                                            x=training_data[, -c(1:4)], # x is all of the columns that you want to use to predict y 
                                            y=training_data$ice,# y is the column with the response variable
                                            learning_rat= rand_pars[i, "p_learning_rate"] , # use the ith row entry of p_learning rate
                                            subsample= rand_pars[i, "p_subsample"], 
                                            nrounds= rand_pars[i, "p_nrounds"], 
                                            print_every_n= 1000, 
                                            scale_pos_weight= ab_pr_ratio, 
                                            nthread=2,# weighting parameter, scale_pos_weight, equal to the ratio of absences to presences. This is often a good starting point but this is a tunable parameter.
                                            objective="binary:logistic"
                                        )

                    # Use the trained xgboost model to predict ice probability for the tuning year  # use trained model to predict ice  (tuning year)
                        predicted_ice_prob_xgb <- predict(trained_xgb_model, newdata=tuning_data[, -c(1:4)])

                    # use a threshold to covert from probability of ice to a binary ice or no ice 
                    threshold <- 0.5
                    predicted_ice_xgb <- 1 * (predicted_ice_prob_xgb > threshold)
                
                    # Calculate the error of that tuned model on the tuning data and save the error in the e object
                    e[i] <-  mean(predicted_ice_xgb != tuning_data$ice) 
                        # e is a vector containing the out of sample error for the jth tuning year using the ith row in the random hyper parameters df 

                }

            # store the error for each set of tuning parameters for that year 
            e_by_year[[j]] <- e
        }

        # save hyperparameters and the error associated with the minimmum error across years 
            # bind together the error vectors for each year and name the columns 
            error_by_year <- do.call(cbind, e_by_year) %>%
            as.data.frame()
            names(error_by_year) <- years_in_train_tune # rename columns by year 

            # Calculate the average error across all years for each set of hyperparameters 
            avg_error <- rowMeans(error_by_year)

            # merge the average error with the random hyper parameters df  
            error_by_parameter <- cbind(rand_pars, avg_error)

            # find the row with the minimum average error
            min_row <- which.min(error_by_parameter$avg_error)

            # save optimal parameter as an object 
            min_error <- error_by_parameter[min_row, "avg_error"]
            opt_nrounds <- error_by_parameter[min_row, "p_nrounds"]
            opt_learning_rate <- error_by_parameter[min_row, "p_learning_rate"]
            opt_subsamp <- error_by_parameter[min_row, "p_subsample"]

    # Calculate out of sample accuracy using the optimized hyper parameters and the held out test data --------------------------

        # Train a model on all of the trianing and tuning data (9 years) and using the optimal parameters 
        
            # Calculate the ratio of absence to presence to deal with class imbalance 
            n_pres <- sum(train_tune_data$ice == 1) # get the numer of presence 
            n_abs <- sum(train_tune_data$ice == 0) # get the number of days when ice was absence 
            ab_pr_ratio <-  n_abs / n_pres # ratio of abs to presence

            # Train with xgboost model 
            trained_xgb_model <- xgboost(
                                x=train_tune_data[, -c(1:4)], # x is all of the columns that you want to use to predict y 
                                y=train_tune_data$ice,# y is the column with the response variable
                                learning_rat= opt_learning_rate , # mess with this to change 
                                subsample= opt_subsamp, 
                                nrounds= opt_nrounds, 
                                print_every_n= 1000, 
                                scale_pos_weight= ab_pr_ratio, 
                                nthread=2, # weighting parameter, scale_pos_weight, equal to the ratio of absences to presences. This is often a good starting point but this is a tunable parameter.
                                objective="binary:logistic"
                                )

            # Use the trained xgboost model to predict ice probability for the tuning year  # use trained model to predict ice  (tuning year)
                predicted_ice_prob_xgb <- predict(trained_xgb_model, newdata=test_data[, -c(1:4)])

            # use a threshold to covert from probability of ice to a binary ice or no ice 
                threshold <- 0.5
                predicted_ice_xgb <- 1 * (predicted_ice_prob_xgb > threshold)
            
            # Calculate the error of that tuned model on the tuning data and save the error in the e object
            accuracy <-  mean(predicted_ice_xgb == test_data$ice) 

    # Save accuracy and optimal parameters 
    opt_param_and_accuaracy[[k]] <- c(accuracy, opt_nrounds,  opt_learning_rate,  opt_subsamp)

    }

    ### Interpret output
    # opt_param_and_accuaracy_bombshelter <- opt_param_and_accuaracy 

        opt_output <- do.call(rbind, opt_param_and_accuaracy) %>%
        as.data.frame()
        names(opt_output) <- c("accuracy", "opt_nrounds",  "opt_learning_rate", "opt_subsamp")

        # this is your xgb output after the model has been optimized for each year, add a year column and then add these accuracy values to yout plot comparing to the not optimized model and rf 
        opt_output <- cbind(years_in_full_data, opt_output)
        names(opt_output)[names(opt_output) == "years_in_full_data" ] <- "year"

    # Plot the accuracy of the tuned xgb model with the untuned xgb and rf --------

    accuracy_xgbtuned <- opt_output$accuracy
    mean(accuracy_xgbtuned)

    # Take a look at accuracy over each year for both models: Do they struggle with the same years?  
        accuracy_mod_summary <- cbind(years, accuracy_rf, accuracy_xgb, accuracy_xgbtuned) %>% # bind together the accuracy for each year 
        as.data.frame()

        names(accuracy_mod_summary) <- c("year", "rf", "xgb", "xgb_tuned") # change names to make it easier to work with 

        # pivot longer to be able to plot 
        accuracy_mod_summary <- accuracy_mod_summary %>%
            tidyr::pivot_longer(
                cols = c(rf, xgb, xgb_tuned), # designate the columns that you want to pivot longer
                names_to = "model", # take the names of the current columns and put them in a new column called model
                values_to = "accuracy" # the values in those columns go to a column called 
            )

        # plot out of sample accuracy for each year for each model 
        accuracy_mod_summary %>%
        ggplot(aes(x = year, y = accuracy, color = model, shape = model)) + 
        geom_jitter(
            width = 0.10,   # horizontal jitter
            height = 0,     # no vertical jitter
            alpha = 0.75,
            size = 3
        ) +
        scale_color_manual(values= c(
            "rf" = "forestgreen", 
            "xgb" = "maroon", 
            "xgb_tuned" = "darkorange3"
        )) +
        theme_minimal(base_size = 16) + 
        labs(
            x = "Year Held Out", 
            y = "Accuracy", 
            title = "Out of Sample Accuracy for Each Year for All Models "
        )


    # Save the optimal parameters for XGB even though they are a bummer 
    write.csv(accuracy_mod_summary, "data/derived_data/accuracy_mod_summary.csv")
    write.csv(opt_output, "data/derived_data/opt_output.csv")

    # Then I think use the optimal parameters for the year with the maximum accuracy (in this case 2017 )

# __________________________________________________
# Hindcast   
# __________________________________________________

# Trim data for hindcasting  ---------------------

    # Set date as POSIXct 
    full_timeseries$Date <- as.POSIXct(full_timeseries$Date)
    names(full_timeseries)

    # Trim full time series to only 
    hind_data <- full_timeseries %>%
      select(-c("temperature_C_weekly", "cond_uScm_weekly")) %>% # remove weekly columns because they have NAs
      filter(Date < min(loch_out$Date)) %>% #include the timepoints prior to the start of the training data
      filter(wy_doy >= min(loch_out$wy_doy) & wy_doy <= max(loch_out$wy_doy)) %>% # trim full time series to only include timepoints in the spring 
      tidyr::drop_na() # remove any rows with na values in any column 

    # Plot your hindcast data 
    # hind_data %>%
    #     # filter(waterYear == 2023) %>%
    #     ggplot(aes(x = Date, y = Flow)) + 
    #     geom_point(alpha = 0.5) + 
    #     theme_minimal() + 
    #     facet_wrap(~waterYear, scales = "free_x")

        hind_data %>%
            mutate(
                cond_scaled = scales::rescale(cond_uScm_impute, to = range(temperature_C_impute, na.rm = TRUE)),
                cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(temperature_C_impute, na.rm = TRUE)), 
                q_scaled = scales::rescale(Flow, to = range(temperature_C_impute, na.rm = TRUE))
            ) %>%
            ggplot(aes(x= Date)) +
            geom_point(aes(y = temperature_C_impute), color = "salmon3", alpha = 0.75) + 
            geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
            geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) +
            geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) +  
            theme_minimal() + 
        facet_wrap(~waterYear, scales = "free")

# Feature Engineering on Hindcast data  ---------------------

    # Organize columns a bit first 
    hind_data <- subset(hind_data, select = c("waterYear", "wy_doy", "Date", "temperature_C_impute", "cumulative_dis", "cond_uScm_impute", "Flow"))

    # Add a column that counts the number of days that the water temp has been above thresholds 
    hind_data <- hind_data %>% # HERE 
        group_by(waterYear) %>% # group by water year because we want this count within each water year 
        arrange(Date, .by_group = TRUE) %>% # make sure everything is in order by day 
        mutate(
            cum_days_temp_above2 = cumsum(temperature_C_impute > 2),
            cum_days_temp_above4 = cumsum(temperature_C_impute > 4), 
            cum_days_temp_above6 = cumsum(temperature_C_impute > 6),
        ) %>% #calculate the cumulative sum of rows (days) when water temp was above 4C 
        ungroup()

    # Add rates of change at different time intervals for temp, conductivity, and cumulative discharge 
    hind_data <- hind_data %>%
        group_by(waterYear) %>%
        arrange(Date, .by_group = TRUE) %>% #arrange by date within the groups 
        mutate(
            # Note this is set up so that the difference in temp is since the first day of the water year for the early days 
            # Rate of change in temperature 
            temp_change_3day = (temperature_C_impute - lag(temperature_C_impute, n = 3, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
            temp_change_5day = (temperature_C_impute - lag(temperature_C_impute, n = 5, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
            temp_change_10day = (temperature_C_impute - lag(temperature_C_impute, n = 10, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
            temp_change_15day = (temperature_C_impute - lag(temperature_C_impute, n = 15, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
            temp_change_20day = (temperature_C_impute - lag(temperature_C_impute, n = 20, default = first(temperature_C_impute))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 
            # Rate of change in cumulative flow 
            cumq_change_3day = (cumulative_dis - lag(cumulative_dis, n = 3, default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
            cumq_change_5day = (cumulative_dis - lag(cumulative_dis, n = 5,  default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
            cumq_change_10day = (cumulative_dis - lag(cumulative_dis, n = 10, default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
            cumq_change_15day = (cumulative_dis - lag(cumulative_dis, n = 15,  default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
            cumq_change_20day = (cumulative_dis - lag(cumulative_dis, n = 20,  default = first(cumulative_dis))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 
            # Rate of change in conductivity
            cond_change_3day = (cond_uScm_impute - lag(cond_uScm_impute, n = 3, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 3, default = first(Date))), 
            cond_change_5day = (cond_uScm_impute - lag(cond_uScm_impute, n = 5, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 5, default = first(Date))), 
            cond_change_10day = (cond_uScm_impute - lag(cond_uScm_impute, n = 10, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 10, default = first(Date))), 
            cond_change_15day = (cond_uScm_impute - lag(cond_uScm_impute, n = 15, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 15, default = first(Date))), 
            cond_change_20day = (cond_uScm_impute - lag(cond_uScm_impute, n = 20, default = first(cond_uScm_impute))) / as.numeric(Date - lag(Date, n = 20, default = first(Date))), 

        ) %>%
        ungroup()

    str(hind_data)

    # Remove any rows with NA in the temp_change_3day column 
    hind_data <- hind_data %>%
        tidyr::drop_na()

# Random Forest Hindcast model ----------------------------------------------

        # to account for a slight class imbalance we want to make that when training the model, it is grabbing the same number as days with ice and without ice 
        n_abs <- sum(loch_out$ice == 0) # get the number of days when ice was absence 

        # Train the random forest model using all the data we have ice presence data for 
        trained_hind_rf_model <- randomForest(ice ~ ., data=loch_out[, -c(1:3)], ntree = 500, sampsize=c(n_abs, n_abs))
                # input the training data but remove the first 3 columns (date, water year, and day of water year)
                # this says rpredict ice based on every other column in this data, run an ensamble of 500 trees and give me the outcome

        # Use the trained random forest model to predict the probability of ice on for each day in the test dataset
            predicted_hind_prob_rf <- predict( trained_hind_rf_model, newdata=hind_data[, -c(1:3)], type="prob")[,2] # the 2 is because we only want the probability of ice presence (2nd column) not the probability of absence 

        # Set the threshold probability of when you call ice presence present 
            threshold <- 0.5

        # The output of all of these models is the probability of presence, in order to get a charcterization of presence absence set a threshold 
            hind_ice_rf <- 1 * (predicted_hind_prob_rf > threshold)

# XGBoost Hindcast model ----------------------------------------------

        # Calculate the ratio of absence to presence to deal with class imbalance 
        n_pres <- sum(loch_out$ice == 1) # get the numer of presence 
        n_abs <- sum(loch_out$ice == 0) # get the number of days when ice was absence 
        ab_pr_ratio <-  n_abs / n_pres # ratio of abs to presence

        # Train with xgboost
        trained_xgb_model <- xgboost(x=loch_out[, -c(1:4)], y=loch_out$ice,# x is all the columns in the dataset except the first one, y is the occupancy column 
                            learning_rat=0.01, subsample=0.5, nrounds=10000, print_every_n=1000, 
                            scale_pos_weight= ab_pr_ratio, nthread=2,# weighting parameter, scale_pos_weight, equal to the ratio of absences to presences. This is often a good starting point but this is a tunable parameter.
                            objective="binary:logistic")

        # Use the trained xgboost model to predict ice probability on the test data
        predicted_ice_prob_xgb <- predict(trained_xgb_model, newdata=hind_data[, -c(1:3)])

        # use a threshold to covert from probability of ice to a binary ice or no ice 
        threshold <- 0.5
        hind_ice_xgb <- 1 * (predicted_ice_prob_xgb > threshold)

# Add Hindcasts to df and visualize ----------------------------------------------
        hind_data$ice_rf <- hind_ice_rf
        hind_data$ice_xgb <- hind_ice_xgb

        hind_data %>%
            mutate(
            cond_scaled = scales::rescale(cond_uScm_impute, to = range(ice_rf, na.rm = TRUE)),
            temp_scaled = scales::rescale(temperature_C_impute, to = range(ice_rf, na.rm = TRUE)), 
            cumulative_q_scaled = scales::rescale(cumulative_dis, to = range(ice_rf, na.rm = TRUE)), 
            q_scaled = scales::rescale(Flow, to = range(ice_rf, na.rm = TRUE))
            ) %>%
            ggplot(aes(x= Date)) +
            geom_point(aes(y = cond_scaled), color = "olivedrab4", alpha = 0.75) + 
            geom_point(aes(y = temp_scaled), color = "salmon3", alpha = 0.75) + 
            geom_point(aes(y = cumulative_q_scaled), color = "mediumpurple4", alpha = 0.75) + 
            geom_point(aes(y = q_scaled), color = "mediumpurple1", alpha = 0.75) + 
            geom_point(aes(y = ice_rf), color = "skyblue3", alpha = 0.75) + 
            theme_minimal() + 
            labs(
                x = "Date", 
                title = "Random Forest Ice Predictions"
            ) + 
        facet_wrap(~waterYear, scales = "free")

# First Day of Ice Off  ----------------------------------------------

    # Extract the first day in each water year when the model predicts ice off
        hind_results <- subset(hind_data, select = c("waterYear" , "wy_doy", "Date", "ice_rf" ,  "ice_xgb"))
        head(hind_results)

        hind_summary <- hind_results %>%
        group_by(waterYear) %>%
        arrange(Date, .by_group = TRUE) %>%
        summarize(
            # rf_ice_off_date = first(Date[ice_rf == 0]), 
            rf_ice_off_dowy = first(wy_doy[ice_rf == 0]),
            # xgb_ice_off_date = first(Date[ice_xgb == 0]), 
            xgb_ice_off_dowy = first(wy_doy[ice_xgb == 0])
        )
        names(hind_summary)[names(hind_summary)== "rf_ice_off_dowy"] <- "rf"
        names(hind_summary)[names(hind_summary)== "xgb_ice_off_dowy"] <- "xgb"

    # Pivot hindcasted days longer 
        hind_summary <- hind_summary %>%
            tidyr::pivot_longer(
                cols = c(rf, xgb), # designate the columns that you want to pivot longer
                names_to = "model", # take the names of the current columns and put them in a new column called model
                values_to = "wy_doy" # the values in those columns go to a column called 
            )

    # Format the observed first day of ice off to go with the modeled 
        loch_out_summary <- loch_out %>%
            group_by(waterYear) %>%
            arrange(Date, .by_group = TRUE) %>%
            summarize(
                wy_doy = first(wy_doy[ice == 0])
            )
        loch_out_summary$model <- "obs"
        loch_out_summary <- subset(loch_out_summary, select = c("waterYear", "model", "wy_doy"))

    # put together observed and modeled ice off doy 
        ice_off_summary <- rbind(loch_out_summary, hind_summary)

    # plot ice off date over time
            set.seed(123)  # makes the jitter reproducible

        ice_off_summary %>%
          
        filter(waterYear != 1987) %>%
        
        mutate(
            error_days = case_when(
            model == "rf"  ~ 5,
            model == "xgb" ~ 9,
            TRUE ~ 0
            ),
            
            ymin = wy_doy - error_days,
            ymax = wy_doy + error_days,
            
            # manually jitter the x positions
            waterYear_jitter = waterYear + runif(n(), -0.25, 0.25)
        ) %>%
        
        ggplot(aes(x = waterYear_jitter, y = wy_doy, color = model)) +
        
        geom_errorbar(
            aes(ymin = ymin, ymax = ymax),
            width = 0.15,
            alpha = 0.5
        ) +
        
        geom_point(
            alpha = 0.75,
            size = 3
        ) +
        
        scale_color_manual(values = c(
            "rf" = "forestgreen",
            "xgb" = "maroon",
            "obs" = "cornflowerblue"
        )) +
        
        theme_minimal(base_size = 16) +
        
        labs(
            y = "Ice Off Date (day of water year)",
            x = "Water Year"
        )