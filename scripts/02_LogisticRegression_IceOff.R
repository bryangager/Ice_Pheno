###################################################
# Leave one Year out Accuracy for Logistic Regression  -  Ice OFF
###################################################


# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

# Load any necessary packages and functions 
source("source/00_libraries.R")
########### OG load in data 
# # Load in data   
# 
# # Ice presence, conductivity, water temperature, and flow for full time series 
# full_timeseries <- read.csv("derived_data/00_imputed_data_trimmed_spring.csv")
# full_timeseries$Date <- as.POSIXct(full_timeseries$Date)
# 
# # Create another data frame that contains just the years for which we also have ice observations 
# loch_raw <- full_timeseries %>%
#   filter(waterYear >= 2014)

############### Katie's data load-in process:
# Load in data   
met_only <- read.csv("derived_data/00_met_daily_fullyr.csv") %>% select(-X)
hydro_only <- read.csv("derived_data/00_hydro_daily_fullyr.csv")  %>% select(-X) # This has 239 duplicate dates
ice_only <- read.csv("derived_data/00_ice_daily_fullyr.csv") %>% select(-X)

# fix hydro_only

hydro_only_fixed <- hydro_only %>%
  group_by(Date) %>%
  summarise(
    calYear = first(na.omit(calYear)),
    waterYear = first(na.omit(waterYear)),
    wy_doy = max(wy_doy, na.rm = TRUE),
    cond_uScm = first(na.omit(cond_uScm)),
    water_temp_C = first(na.omit(water_temp_C)),
    Flow = first(na.omit(Flow)),
    cumulative_dis = first(na.omit(cumulative_dis)),
    .groups = "drop"
  )

anyDuplicated(hydro_only_fixed$Date)
# Add Ice data to create the three data frames you are going to work with 
met_data_full_timeseries <- full_join(ice_only, met_only, by = "Date") %>% select(-c(calYear.x,waterYear.x,wy_doy.x)) %>% 
  rename(c(calYear = calYear.y, waterYear=waterYear.y, wy_doy=wy_doy.y))
hydro_data_full_timeseries <- full_join(ice_only, hydro_only_fixed, by = "Date") %>% select(-c(calYear.x,waterYear.x,wy_doy.x)) %>% 
  rename(c(calYear = calYear.y, waterYear=waterYear.y, wy_doy=wy_doy.y))
sink_data_full_timeseries <- full_join(hydro_data_full_timeseries, met_data_full_timeseries, by = "Date") %>% select(-c(calYear.x,waterYear.x,wy_doy.x, ice.x)) %>% 
  rename(c(calYear = calYear.y, waterYear=waterYear.y, wy_doy=wy_doy.y, ice=ice.y))

# Trim data frames to only spring and only since 2014
met_data <- filter_by_year_and_doy(met_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
  filter(waterYear >= 2014 & waterYear <=2024)

hydro_data <- filter_by_year_and_doy(hydro_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
  filter(waterYear >= 2014 & waterYear <=2024)

sink_data <- filter_by_year_and_doy(sink_data_full_timeseries, c(170,288))  %>% # March 18 - July 15
  filter(waterYear >= 2014 & waterYear <= 2024)


# __________________________________________________
# 01. For loop for Leave one Year out Accuracy for Logistic Regression -- Ice OFF -- Sink
# __________________________________________________


  # initialize i to step through for loop 
  i <- 3 


  # create an object that holds all of the waterYears in the full dataset 
    years <- unique(sink_data$waterYear)

# Create an obect to hold the out of sample accuracy for each year 
    accuracy_log <- rep(NA, length(years))
    ice_off_diff_log <- rep(NA, length(years))

# for each year in your list of years 
    for (i in 1:length(years)){

       # seperate into train and test data 
        test_year <- years[i]
        training_data <- sink_data[sink_data$waterYear != test_year, ]
        test_data <- sink_data[sink_data$waterYear == test_year, ]

      
      # Train a logistic regression model on training data 
        trained_log_model <- glm(ice ~ swe + precip + precip_cumulative + airT_min + airT_max + airT_mean + 
                                   wind_10m_mean + wind_10m_max+ Flow + cumulative_dis + water_temp_C + cond_uScm, 
          data = training_data, 
          family = binomial)
        
      # use the trained logistic regression model to predict the presence or absence of ice in the test data 
        predicted_ice_prob_log <- predict(trained_log_model, newdata = test_data, type = "response")  # do I need something here that selects the column for ice presence like in Katie's code? (column 2)
        
        # Convert the probability into a prediction 
        predicted_ice_log <- ifelse(predicted_ice_prob_log > 0.5, 1, 0)
        
        # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
        accuracy_log[i] <- mean(predicted_ice_log == test_data$ice, na.rm = TRUE)
        
        # Calculate the number of days away from observed ice off the 
        
        # extract the day when we first observed no ice 
        ice_off_obs <- which(test_data$ice == 0)[1]
        
        # extract the day when the model first predicted no ice 
        ice_off_pred <- which(predicted_ice_log == 0)[1] %>% 
          as.numeric()
        
        # take the difference betweent those two days and save it in the days_off_log 
        ice_off_diff_log[i] <- ice_off_obs -  ice_off_pred

    }
    
    
    # Look at the number of days off from predicted ice off each of your predictions are 
    ice_off_diff_log_df <- as.data.frame(ice_off_diff_log)
    ggplot(data = ice_off_diff_log_df, aes(x = ice_off_diff_log)) + 
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
      labs(
        x = "Observed - Predicted Ice Off Day"
      ) +
      theme_minimal()
    mean(ice_off_diff_log)
    mean(abs(ice_off_diff_log)) # on average how far away from zero are you
    
    mean(accuracy_log, na.rm = TRUE)
    
    sink_ice_off_diff_df <- ice_off_diff_log_df
    sink_ice_off_diff_df <- sink_ice_off_diff_df %>% mutate(model = "sink")
    
    # Take a look at accuracy over each year for log model
    accuracy_yr_summary <- cbind(years, accuracy_log) %>% 
      as.data.frame()
    accuracy_yr_summary %>%
      ggplot(aes(x = years, y = accuracy_log)) + 
      geom_point(color = "forestgreen", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "LR Out of Sample Accuracy - Sink Ice Off"
      )
    
    sink_accuracy_df <- accuracy_yr_summary
    sink_accuracy_df <- sink_accuracy_df %>% mutate(model = "sink")
    
    
    # __________________________________________________
    # 02. For loop for Leave one Year out Accuracy for Logistic Regression -- Ice OFF -- MET Data
    # __________________________________________________
    
    
    # initialize i to step through for loop 
    i <- 3 
    
    
    # create an object that holds all of the waterYears in the full dataset 
    years <- unique(met_data$waterYear)
    
    # Create an obect to hold the out of sample accuracy for each year 
    accuracy_log <- rep(NA, length(years))
    ice_off_diff_log <- rep(NA, length(years))
    
    # for each year in your list of years 
    for (i in 1:length(years)){
      
      # seperate into train and test data 
      test_year <- years[i]
      training_data <- met_data[met_data$waterYear != test_year, ]
      test_data <- met_data[met_data$waterYear == test_year, ]
      
      
      # Train a logistic regression model on training data 
      trained_log_model <- glm(ice ~ swe + precip + precip_cumulative + airT_min + airT_max + airT_mean + wind_10m_mean + wind_10m_max, 
                               data = training_data, 
                               family = binomial)
      
      # use the trained logistic regression model to predict the presence or absence of ice in the test data 
      predicted_ice_prob_log <- predict(trained_log_model, newdata = test_data, type = "response")  # do I need something here that selects the column for ice presence like in Katie's code? (column 2)
      
      # Convert the probability into a prediction 
      predicted_ice_log <- ifelse(predicted_ice_prob_log > 0.5, 1, 0)
      
      # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
      accuracy_log[i] <- mean(predicted_ice_log == test_data$ice, na.rm = TRUE)
      
      # Calculate the number of days away from observed ice off the 
      
      # extract the day when we first observed no ice 
      ice_off_obs <- which(test_data$ice == 0)[1]
      
      # extract the day when the model first predicted no ice 
      ice_off_pred <- which(predicted_ice_log == 0)[1] %>% 
        as.numeric()
      
      # take the difference betweent those two days and save it in the days_off_log 
      ice_off_diff_log[i] <- ice_off_obs -  ice_off_pred
      
    }
    
    
    # Look at the number of days off from predicted ice off each of your predictions are 
    ice_off_diff_log_df <- as.data.frame(ice_off_diff_log)
    ggplot(data = ice_off_diff_log_df, aes(x = ice_off_diff_log)) + 
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
      labs(
        x = "Observed - Predicted Ice Off Day"
      ) +
      theme_minimal()
    mean(ice_off_diff_log)
    mean(abs(ice_off_diff_log)) # on average how far away from zero are you
    
    mean(accuracy_log, na.rm = TRUE)
    
    met_ice_off_diff_df <- ice_off_diff_log_df
    met_ice_off_diff_df <- met_ice_off_diff_df %>% mutate(model = "met")
    
    # Take a look at accuracy over each year for log model
    accuracy_yr_summary <- cbind(years, accuracy_log) %>% 
      as.data.frame()
    accuracy_yr_summary %>%
      ggplot(aes(x = years, y = accuracy_log)) + 
      geom_point(color = "forestgreen", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "LR Out of Sample Accuracy - Met Ice Off"
      )
    met_accuracy_df <- accuracy_yr_summary
    met_accuracy_df <- met_accuracy_df %>% mutate(model = "met")
    
    
    # __________________________________________________
    # 03. For loop for Leave one Year out Accuracy for Logistic Regression -- Ice OFF -- Hydro Data
    # __________________________________________________
    
    
    # initialize i to step through for loop 
    i <- 3 
    
    
    # create an object that holds all of the waterYears in the full dataset 
    years <- unique(hydro_data$waterYear)
    
    # Create an obect to hold the out of sample accuracy for each year 
    accuracy_log <- rep(NA, length(years))
    ice_off_diff_log <- rep(NA, length(years))
    
    # for each year in your list of years 
    for (i in 1:length(years)){
      
      # seperate into train and test data 
      test_year <- years[i]
      training_data <- hydro_data[hydro_data$waterYear != test_year, ]
      test_data <- hydro_data[hydro_data$waterYear == test_year, ]
      
      
      # Train a logistic regression model on training data 
      trained_log_model <- glm(ice ~ Flow + cumulative_dis + water_temp_C + cond_uScm, 
                               data = training_data, 
                               family = binomial)
      
      # use the trained logistic regression model to predict the presence or absence of ice in the test data 
      predicted_ice_prob_log <- predict(trained_log_model, newdata = test_data, type = "response")  # do I need something here that selects the column for ice presence like in Katie's code? (column 2)
      
      # Convert the probability into a prediction 
      predicted_ice_log <- ifelse(predicted_ice_prob_log > 0.5, 1, 0)
      
      # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
      accuracy_log[i] <- mean(predicted_ice_log == test_data$ice, na.rm = TRUE)
      
      # Calculate the number of days away from observed ice off the 
      
      # extract the day when we first observed no ice 
      ice_off_obs <- which(test_data$ice == 0)[1]
      
      # extract the day when the model first predicted no ice 
      ice_off_pred <- which(predicted_ice_log == 0)[1] %>% 
        as.numeric()
      
      # take the difference betweent those two days and save it in the days_off_log 
      ice_off_diff_log[i] <- ice_off_obs -  ice_off_pred
      
    }
    
    
    # Look at the number of days off from predicted ice off each of your predictions are 
    ice_off_diff_log_df <- as.data.frame(ice_off_diff_log)
    ggplot(data = ice_off_diff_log_df, aes(x = ice_off_diff_log)) + 
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
      labs(
        x = "Observed - Predicted Ice Off Day"
      ) +
      theme_minimal()
    mean(ice_off_diff_log)
    mean(abs(ice_off_diff_log)) # on average how far away from zero are you
    
    mean(accuracy_log, na.rm = TRUE)
    
    hydro_ice_off_diff_df <- ice_off_diff_log_df
    hydro_ice_off_diff_df <- hydro_ice_off_diff_df %>% mutate(model = "hydro")
    
    # Take a look at accuracy over each year for log model
    accuracy_yr_summary <- cbind(years, accuracy_log) %>% 
      as.data.frame()
    accuracy_yr_summary %>%
      ggplot(aes(x = years, y = accuracy_log)) + 
      geom_point(color = "forestgreen", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "LR Out of Sample Accuracy - Hydro Ice Off"
      )
   hydro_accuracy_df <- accuracy_yr_summary
   hydro_accuracy_df <- hydro_accuracy_df %>% mutate(model = "hydro")
    
    
#------------------------------------------------
### Combining accuracies from all three models
#------------------------------------------------
   
### All three difference from observed
all_three_diff_df <- rbind(sink_ice_off_diff_df,met_ice_off_diff_df,hydro_ice_off_diff_df)
    
 
   ggplot(all_three_diff_df, aes(x = ice_off_diff_log, fill = model)) +
     geom_histogram(
       bins = 30,
       position = "dodge"
     ) +
     labs(
       x = "Ice-off difference (log)",
       y = "Density",
       fill = "Model"
     ) +
     scale_fill_discrete(
       labels = c(
         "met" = "Meteorological",
         "hydro" = "Hydrological",
         "sink" = "Both"
       )
     ) +
     theme_minimal()
   
   ggplot(all_three_diff_df, aes(x = ice_off_diff_log)) +
     geom_histogram(
       aes(fill = model),
       bins = 30
     ) +
     facet_wrap(~ model, ncol = 1) +
     labs(
       x = "Ice-off difference (log)",
       y = "Count"
     ) +
     scale_fill_manual(
       values = c(
         "met" = "orange",
         "hydro" = "steelblue",
         "sink" = "forestgreen"
       )
     ) +
     theme_minimal()
   
#### Accuracy
   all_three_accuracy_df <- rbind(sink_accuracy_df, met_accuracy_df, hydro_accuracy_df)

   ggplot(all_three_accuracy_df,
          aes(x = years, y = accuracy_log, color = model)) +
     geom_point(size = 3) +
     scale_color_manual(values = c(
       "met" = "orange",
       "hydro" = "steelblue",
       "sink" = "forestgreen"
     )) +
     theme_minimal(base_size = 16) +
     labs(x = "Year Held Out", y = "Accuracy", title = "LR Out of Sample Accuracy - Ice Off")
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
   
      
    
############## Explaining years that aren't as accurate by looking at missing data: (log reg. by default eliminates an entire row if there's any NAs)
    sum(complete.cases(sink_data[, c("ice_presence", "Flow", "cumulative_dis", "water_temp_C", "cond_uScm")]))
    # only 1160 rows with no NAs
    nrow(sink_data)
    # 1309 rows total = 149 rows omitted from model
    
    # Pulling out rows with NAs:
    na_plot <- sink_data %>%
      select(
        waterYear,
        wy_doy,
        ice_presence,
        Flow,
        cumulative_dis,
        water_temp_C,
        cond_uScm
      ) %>%
      pivot_longer(
        cols = c(
          ice_presence,
          Flow,
          cumulative_dis,
          water_temp_C,
          cond_uScm
        ),
        names_to = "Variable",
        values_to = "Value"
      ) %>%
      mutate(Missing = is.na(Value))
    
    ggplot(
      na_plot,
      aes(x = wy_doy,
          y = factor(waterYear),
          fill = Missing)
    ) +
      geom_tile() +
      facet_wrap(~Variable, ncol = 1) +
      scale_fill_manual(
        values = c(
          "FALSE" = "white",
          "TRUE" = "red"
        ),
        labels = c("Present", "Missing")
      ) +
      labs(
        x = "Water Year Day",
        y = "Water Year",
        fill = ""
      ) +
      theme_bw()
    
    
    
  sink_data_full_timeseries <- filter_by_year_and_doy(sink_data_full_timeseries, c(170,288)) # March 18 - July 15
    
    
    ############## Hindcasting ice-off
    hindcast_preds <- predict(trained_log_model,
                              newdata = sink_data_full_timeseries,
                              type = "response")
    
    # making the probability a binary using the threshold derived from the average of daily temp model probabilities
    hindcast_probs_0_1 <- ifelse(hindcast_preds < 0.5,
                                 1,
                                 0
    )
    # changing the labels of the probabilities
    hindcast_prob_factor <- factor(hindcast_probs_0_1, levels = c(1, 0),labels = c("ice", "no ice"))
    
    # adding the binary to the main df
    hindcast_imputed_df <- sink_data_full_timeseries
    hindcast_imputed_df$predicted_ice <- hindcast_prob_factor
    
    
    # Initialize an empty data frame to store the results
    hindcasted_ice_off_dates <- data.frame()
    
    # Iterate through each unique waterYear
    for(year in unique(hindcast_imputed_df$waterYear)) {
      
      # Filter the data for the current year where predicted_ice == "no ice"
      year_data <- hindcast_imputed_df %>% 
        filter(waterYear == year, predicted_ice == "no ice") %>%
        arrange(wy_doy)  # Sort by wy_doy to find the first day
      
      # If there is any day where predicted_ice == "no ice"
      if (nrow(year_data) > 0) {
        # Get the first day (earliest day) in that year where predicted_ice == "no ice"
        first_no_ice_wy_doy <- year_data %>% slice(1)
        
        # Create a new row with the waterYear and first wy_doy
        result_row <- data.frame(
          waterYear = year,
          first_no_ice_wy_doy = first_no_ice_wy_doy$wy_doy
        )
        
        # Append the result_row to the result_df
        hindcasted_ice_off_dates <- bind_rows(hindcasted_ice_off_dates, result_row)
      }
    }
    
    # View the result data frame
    print(hindcasted_ice_off_dates)
    
  hindcasted_ice_off_dates |> hindcasted_ice_off_dates$model -> "LogReg" 
  hindcasted_ice_off_dates |> mutate(logreg_ice_off_dowy = first_no_ice_wy_doy)
    