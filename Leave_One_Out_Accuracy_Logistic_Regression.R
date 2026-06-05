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
# 01. For loop for Leave one Year out Accuracy for Logistic Regresssion 
# __________________________________________________


  # initialize i to step through for loop 
  i <- 3 


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
        model_1 <- glm( ice_or_no ~ Flow + cumulative_dis + temperature_C_impute + cond_uScm_impute, 
          data = training_data, 
          family = binomial)

        flow_pval <- summary(model_1)$coefficients["Flow", "Pr(>|z|)"]
        cumulative_dis_pval <- summary(model_1)$coefficients["cumulative_dis", "Pr(>|z|)"]
        temperature_C_impute_pval <- summary(model_1)$coefficients["temperature_C_impute", "Pr(>|z|)"]
        cond_uScm_impute_pval <- summary(model_1)$coefficients["cond_uScm_impute", "Pr(>|z|)"]

        flow_pval >= 0.5

    }