###################################################
# Leave one Year out Accuracy for Logistic Regresssion 
###################################################


# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

    # Load any necessary packages 
        library(dplyr) 
        library(ggplot2)
        library(dplyr)

        source("Input_Files/01_Data_Input.R")

    # Rename file (to make it more confusing) to match KAG scripts 
        loch_out <- imputed_data_trimmed_14_23

# __________________________________________________
# 01. For loop for Leave one Year out Accuracy for Logistic Regression -- Ice OFF
# __________________________________________________


  # initialize i to step through for loop 
  i <- 5 


  # create an object that holds all of the waterYears in the full dataset 
    years <- unique(loch_out$waterYear) 

# Create an obect to hold the out of sample accuracy for each year 
    accuracy_log <- rep(NA, length(years))
    ice_off_diff_log <- rep(NA, length(years))

# for each year in your list of years 
    for (i in 1:length(years)){

       # seperate into train and test data 
        test_year <- years[i]
        training_data <- loch_out[loch_out$waterYear != test_year, ]
        test_data <- loch_out[loch_out$waterYear == test_year, ]

      
      # Train a logistic regression model on training data 
        trained_log_model <- glm( ice_or_no ~ Flow + cumulative_dis + temperature_C_impute + cond_uScm_impute, 
          data = training_data, 
          family = binomial)
        
      # use the trained logistic regression model to predict the presence or absence of ice in the test data 
        predicted_ice_prob_log <- predict(trained_log_model, newdata = test_data, type = "response")  
        
        # Convert the probability into a prediction 
        predicted_ice_log <- ifelse(predicted_ice_prob_log > 0.5, 1, 0)
        
        # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
        accuracy_log[i] <- mean(predicted_ice_log == test_data$ice_or_no, na.rm = TRUE)
        
        # Calculate the number of days away from observed ice off the 
        
        # extract the day when we first observed no ice 
        ice_off_obs <- which(test_data$ice_or_no == 0)[1]
        
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
    
    mean(accuracy_log)
    
    # Take a look at accuracy over each year for log model
    accuracy_yr_summary <- cbind(years, accuracy_log) %>% 
      as.data.frame()
    accuracy_yr_summary %>%
      ggplot(aes(x = years, y = accuracy_log)) + 
      geom_point(color = "goldenrod", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "Logistic Regression Out of Sample Accuracy for each Year"
      )
    
    
    
    # __________________________________________________
    # 02. For loop for Leave one Year out Accuracy for Logistic Regression -- Ice ON
    # __________________________________________________

      
    
    # Pull in ice on data:
    ice_on_data <- read.csv("Input_Files/met_hydro_winter_ice_on.csv")
    
    # initialize i to step through for loop 
    i <- 3 
    
    
    # create an object that holds all of the waterYears in the full dataset 
    years <- unique(ice_on_data$waterYear) 
    
    # Create an obect to hold the out of sample accuracy for each year 
    ice_on_accuracy_log <- rep(NA, length(years))
    ice_on_diff_log <- rep(NA, length(years))
    
    # for each year in your list of years 
    for (i in 1:length(years)){
      
      # seperate into train and test data 
      test_year <- years[i]
      training_data <- ice_on_data[ice_on_data$waterYear != test_year, ]
      test_data <- ice_on_data[ice_on_data$waterYear == test_year, ]
      
      
      # Train a logistic regression model on training data 
      trained_log_model <- glm(ice_or_no ~ temp_7day_mean + z_cond_uScm, 
                                data = training_data, 
                                family = binomial)
      
      # use the trained logistic regression model to predict the presence or absence of ice in the test data 
      predicted_ice_prob_log <- predict(trained_log_model, newdata = test_data, type = "response")  # do I need something here that selects the column for ice presence like in Katie's code? (column 2)
      
      # Convert the probability into a prediction 
      predicted_ice_log <- ifelse(predicted_ice_prob_log > 0.5, 1, 0)
      
      # Calculate the accuracy of those predictions and save into the object you made to hold accuracy
      ice_on_accuracy_log[i] <- mean(predicted_ice_log == test_data$ice_or_no, na.rm = TRUE)
      
      # Calculate the number of days away from observed ice off the 
      
      # extract the day when we first observed no ice 
      ice_on_obs <- which(test_data$ice_or_no == 1)[1]
      
      # extract the day when the model first predicted no ice 
      ice_on_pred <- which(predicted_ice_log == 1)[1] %>% 
        as.numeric()
      
      # take the difference betweent those two days and save it in the days_off_log 
      ice_on_diff_log[i] <- ice_on_obs -  ice_on_pred
      
    }
    
    
    # Look at the number of days off from predicted ice off each of your predictions are 
    ice_on_diff_log_df <- as.data.frame(ice_on_diff_log)
    ggplot(data = ice_on_diff_log_df, aes(x = ice_on_diff_log)) + 
      geom_histogram(binwidth = 1, fill = "#69b3a2", color = "white") +
      labs(
        x = "Observed - Predicted Ice On Day"
      ) +
      theme_minimal()
    mean(ice_on_diff_log)
    mean(abs(ice_on_diff_log)) # on average how far away from zero are you
    
    mean(ice_on_accuracy_log)
    
    # Take a look at accuracy over each year for log model
    ice_on_accuracy_yr_summary <- cbind(years, ice_on_accuracy_log) %>% 
      as.data.frame()
    ice_on_accuracy_yr_summary %>%
      ggplot(aes(x = years, y = ice_on_accuracy_log)) + 
      geom_point(color = "forestgreen", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(
        x = "Year Held Out", 
        y = "Accuracy", 
        title = "Logistic Regression Out of Sample Accuracy for each Year"
      )
    
    


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    # #Extract P values (unneeded for now)
#     flow_pval <- summary(model_1)$coefficients["Flow", "Pr(>|z|)"]
#     cumulative_dis_pval <- summary(model_1)$coefficients["cumulative_dis", "Pr(>|z|)"]
#     temperature_C_impute_pval <- summary(model_1)$coefficients["temperature_C_impute", "Pr(>|z|)"]
#     cond_uScm_impute_pval <- summary(model_1)$coefficients["cond_uScm_impute", "Pr(>|z|)"]
#     
#     flow_pval >= 0.5