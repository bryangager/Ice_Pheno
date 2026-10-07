###################################################
# Leave one Year out Accuracy for Logistic Regression  -  Ice ON
###################################################


# __________________________________________________
# 0. Set Up R Environment and data munging 
# __________________________________________________

# Load any necessary packages and functions 
source("source/00_libraries.R")

# Load in data   

# # Ice presence, conductivity, water temperature, and flow for full time series 
# full_timeseries <- read.csv("derived_data/00_imputed_data_trimmed_winter.csv")
# full_timeseries$Date <- as.POSIXct(full_timeseries$Date)
# 
# # Create another data frame that contains just the years for which we also have ice observations 
# loch_raw <- full_timeseries %>%
#   filter(waterYear >= 2014)
# 
# # Met data from Bear Lake SnoTel site 322
# full_met <- read.csv("derived_data/00_snotel_322.csv")
# full_met$date <- as.POSIXct(full_met$date)


############### NEW load in data from Katie's script + my fix for duplicates in hydro_data because of wy_doy discrepencies
# Load in data   
met_only <- read.csv("derived_data/00_met_daily_fullyr.csv") %>% select(-X)
hydro_only <- read.csv("derived_data/00_hydro_daily_fullyr.csv")  %>% select(-X) # This has 239 duplicate dates
ice_only <- read.csv("derived_data/00_ice_daily_fullyr.csv") %>% select(-X)

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

# Add Ice data to create the three data frames you are going to work with 
met_data_full_timeseries <- full_join(ice_only, met_only)
hydro_data_full_timeseries <- full_join(ice_only, hydro_only_fixed)
sink_data_full_timeseries <- full_join(hydro_data_full_timeseries, met_data_full_timeseries)
sink_data_real_full_time_series <- full_join(met_only,hydro_only_fixed) # Including ice_only trims it to 2013

# Trim data frames to only spring and only since 2014
met_data <- filter_by_year_and_doy(met_data_full_timeseries, c(1,76))  %>% # October 1 - December 15
  filter(waterYear >= 2014 & waterYear <= 2024)

hydro_data <- filter_by_year_and_doy(hydro_data_full_timeseries, c(1,76))  %>% # October 1 - December 15
  filter(waterYear >= 2014 & waterYear <= 2024)

sink_data <- filter_by_year_and_doy(sink_data_full_timeseries, c(1,76))  %>% # October 1 - December 15
  filter(waterYear >= 2014 & waterYear <= 2024)

# # # Pull in ice on data:
# ice_on_data <- read.csv("Input_Files/met_hydro_winter_ice_on.csv")

# __________________________________________________
# 01. For loop for Leave one Year out Accuracy for Logistic Regression -- Sink Data
# __________________________________________________

# initialize i to step through for loop 
i <- 3 


# create an object that holds all of the waterYears in the full dataset 
years <- unique(sink_data$waterYear) 

# Create an obect to hold the out of sample accuracy for each year 
ice_on_accuracy_log <- rep(NA, length(years))
ice_on_diff_log <- rep(NA, length(years))

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
  ice_on_accuracy_log[i] <- mean(predicted_ice_log == test_data$ice, na.rm = TRUE)
  
  # Calculate the number of days away from observed ice off the 
  
  # extract the day when we first observed no ice 
  ice_on_obs <- which(test_data$ice == 1)[1]
  
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

sink_ice_on_diff_df <- ice_on_diff_log_df
sink_ice_on_diff_df <- sink_ice_on_diff_df %>% mutate(model = "sink")

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
    title = "LR Out of Sample Accuracy - Sink Ice On"
  )

sink_accuracy_df <- ice_on_accuracy_yr_summary
sink_accuracy_df <- sink_accuracy_df %>% mutate(model = "sink")

# __________________________________________________
# 02. For loop for Leave one Year out Accuracy for Logistic Regression -- Met Data
# __________________________________________________

# initialize i to step through for loop 
i <- 3 


# create an object that holds all of the waterYears in the full dataset 
years <- unique(met_data$waterYear) 

# Create an obect to hold the out of sample accuracy for each year 
ice_on_accuracy_log <- rep(NA, length(years))
ice_on_diff_log <- rep(NA, length(years))

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
  ice_on_accuracy_log[i] <- mean(predicted_ice_log == test_data$ice, na.rm = TRUE)
  
  # Calculate the number of days away from observed ice off the 
  
  # extract the day when we first observed no ice 
  ice_on_obs <- which(test_data$ice == 1)[1]
  
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

met_ice_on_diff_df <- ice_on_diff_log_df
met_ice_on_diff_df <- met_ice_on_diff_df %>% mutate(model = "met")

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
    title = "LR Out of Sample Accuracy - Met Ice On"
  )

met_accuracy_df <- ice_on_accuracy_yr_summary
met_accuracy_df <- met_accuracy_df %>% mutate(model = "met")

# __________________________________________________
# 03. For loop for Leave one Year out Accuracy for Logistic Regression -- Hydro Data
# __________________________________________________

# initialize i to step through for loop 
i <- 3 


# create an object that holds all of the waterYears in the full dataset 
years <- unique(hydro_data$waterYear) 

# Create an obect to hold the out of sample accuracy for each year 
ice_on_accuracy_log <- rep(NA, length(years))
ice_on_diff_log <- rep(NA, length(years))

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
  ice_on_accuracy_log[i] <- mean(predicted_ice_log == test_data$ice, na.rm = TRUE)
  
  # Calculate the number of days away from observed ice off the 
  
  # extract the day when we first observed no ice 
  ice_on_obs <- which(test_data$ice == 1)[1]
  
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

hydro_ice_on_diff_df <- ice_on_diff_log_df
hydro_ice_on_diff_df <- hydro_ice_on_diff_df %>% mutate(model = "hydro")

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
    title = "LR Out of Sample Accuracy - Hydro Ice On"
  )

hydro_accuracy_df <- ice_on_accuracy_yr_summary
hydro_accuracy_df <- hydro_accuracy_df %>% mutate(model = "hydro")

#------------------------------------------------
### 04. Combining accuracies from all three models
#------------------------------------------------

### All three difference from observed

all_three_diff_df <- rbind(sink_ice_on_diff_df,met_ice_on_diff_df,hydro_ice_on_diff_df)


ggplot(all_three_diff_df, aes(x = ice_on_diff_log, fill = model)) +
  geom_histogram(
    bins = 30,
    position = "dodge"
  ) +
  labs(
    x = "Ice-on difference (log)",
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

ggplot(all_three_diff_df, aes(x = ice_on_diff_log)) +
  geom_histogram(
    aes(fill = model),
    bins = 30
  ) +
  facet_wrap(~ model, ncol = 1) +
  labs(
    x = "Ice-on difference (log)",
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
  labs(x = "Year Held Out", y = "Accuracy", title = "LR Out of Sample Accuracy - Ice On")


#----------------------------------------------------------------------
#  05. Hindcasting - getting ready
#----------------------------------------------------------------------

#Check for class imbalance:
table(sink_data$ice)
prop.table(table(sink_data$ice))
# .485 vs .515 - no need to accommodate for this

sink_train_data <- sink_data

# Trimming full time series
sink_data_real_full_time_series_trim <- filter_by_year_and_doy(sink_data_real_full_time_series, c(1,76))

#----------------------------------------------------------------------
#  05a. Hindcasting - sink model
#----------------------------------------------------------------------
# Sink Model:
trained_sink_model <- glm(ice ~ swe + precip + precip_cumulative + airT_min + airT_max + airT_mean + 
                           wind_10m_mean + wind_10m_max+ Flow + cumulative_dis + water_temp_C + cond_uScm,
                         data = sink_train_data, 
                         family = binomial)

ice_on_hind_preds <- predict(trained_sink_model,
                          newdata = sink_data_real_full_time_series_trim,
                          type = "response")

# making the probability a binary using the threshold derived from the average of daily temp model probabilities
hindcast_probs_0_1 <- ifelse(ice_on_hind_preds < 0.5,
                             0,
                             1
)
# changing the labels of the probabilities
hindcast_prob_factor <- factor(hindcast_probs_0_1, levels = c(1, 0),labels = c("ice", "no ice"))

# adding the binary to the main df
hindcast_imputed_df <- sink_data_real_full_time_series_trim  
hindcast_imputed_df$predicted_ice <- hindcast_prob_factor
# adding the daily probabilities to the main df
hindcast_imputed_df$hindcasted_probability <- hindcast_preds

# Initialize an empty data frame to store the results
hindcasted_ice_on_dates_sink <- data.frame()

# Iterate through each unique waterYear
for(year in unique(hindcast_imputed_df$waterYear)) {
  
  # Filter the data for the current year where predicted_ice == "no ice"
  year_data <- hindcast_imputed_df %>% 
    filter(waterYear == year, predicted_ice == "ice") %>%
    arrange(wy_doy)  # Sort by wy_doy to find the first day
  
  # If there is any day where predicted_ice == "no ice"
  if (nrow(year_data) > 0) {
    # Get the first day (earliest day) in that year where predicted_ice == "no ice"
    first_ice_wy_doy <- year_data %>% slice(1)
    
    # Create a new row with the waterYear and first wy_doy
    result_row <- data.frame(
      waterYear = year,
      first_ice_wy_doy = first_ice_wy_doy$wy_doy
    )
    
    # Append the result_row to the result_df
    hindcasted_ice_on_dates_sink <- bind_rows(hindcasted_ice_on_dates_sink, result_row)
  }
}

# View the result data frame
print(hindcasted_ice_on_dates_sink)
hindcasted_ice_on_dates_sink <- hindcasted_ice_on_dates_sink %>% mutate(model="sink")
# save the dates as a csv for met data analysis:
#write.csv(hindcasted_ice_on_dates_sink, "Input_Files/hindcasted_ice_on_dates_sink")

#----------------------------------------------------------------------
#  05b. Hindcasting - met model
#----------------------------------------------------------------------
# Met Model:
trained_met_model <- glm(ice ~ swe + precip + precip_cumulative + airT_min + airT_max + airT_mean + wind_10m_mean + wind_10m_max,
                          data = sink_train_data, 
                          family = binomial)

ice_on_hind_preds <- predict(trained_met_model,
                             newdata = sink_data_real_full_time_series_trim,
                             type = "response")

# making the probability a binary using the threshold derived from the average of daily temp model probabilities
hindcast_probs_0_1 <- ifelse(ice_on_hind_preds < 0.5,
                             0,
                             1
)
# changing the labels of the probabilities
hindcast_prob_factor <- factor(hindcast_probs_0_1, levels = c(1, 0),labels = c("ice", "no ice"))

# adding the binary to the main df
hindcast_imputed_df <- sink_data_real_full_time_series_trim  
hindcast_imputed_df$predicted_ice <- hindcast_prob_factor
# adding the daily probabilities to the main df
hindcast_imputed_df$hindcasted_probability <- hindcast_preds

# Initialize an empty data frame to store the results
hindcasted_ice_on_dates_met <- data.frame()

# Iterate through each unique waterYear
for(year in unique(hindcast_imputed_df$waterYear)) {
  
  # Filter the data for the current year where predicted_ice == "no ice"
  year_data <- hindcast_imputed_df %>% 
    filter(waterYear == year, predicted_ice == "ice") %>%
    arrange(wy_doy)  # Sort by wy_doy to find the first day
  
  # If there is any day where predicted_ice == "no ice"
  if (nrow(year_data) > 0) {
    # Get the first day (earliest day) in that year where predicted_ice == "no ice"
    first_ice_wy_doy <- year_data %>% slice(1)
    
    # Create a new row with the waterYear and first wy_doy
    result_row <- data.frame(
      waterYear = year,
      first_ice_wy_doy = first_ice_wy_doy$wy_doy
    )
    
    # Append the result_row to the result_df
    hindcasted_ice_on_dates_met <- bind_rows(hindcasted_ice_on_dates_met, result_row)
  }
}

# View the result data frame
print(hindcasted_ice_on_dates_met)
hindcasted_ice_on_dates_met <- hindcasted_ice_on_dates_met %>% mutate(model="met")
# save the dates as a csv for met data analysis:
#write.csv(hindcasted_ice_on_dates_met, "Input_Files/hindcasted_ice_on_dates_met")

#----------------------------------------------------------------------
#  05c. Hindcasting - hydro model
#----------------------------------------------------------------------
# Hydro Model:
trained_hydro_model <- glm(ice ~ Flow + cumulative_dis + water_temp_C + cond_uScm,
                         data = sink_train_data, 
                         family = binomial)

ice_on_hind_preds <- predict(trained_hydro_model,
                             newdata = sink_data_real_full_time_series_trim,
                             type = "response")

# making the probability a binary using the threshold derived from the average of daily temp model probabilities
hindcast_probs_0_1 <- ifelse(ice_on_hind_preds < 0.5,
                             0,
                             1
)
# changing the labels of the probabilities
hindcast_prob_factor <- factor(hindcast_probs_0_1, levels = c(1, 0),labels = c("ice", "no ice"))

# adding the binary to the main df
hindcast_imputed_df <- sink_data_real_full_time_series_trim  
hindcast_imputed_df$predicted_ice <- hindcast_prob_factor
# adding the daily probabilities to the main df
hindcast_imputed_df$hindcasted_probability <- hindcast_preds

# Initialize an empty data frame to store the results
hindcasted_ice_on_dates_hydro <- data.frame()

# Iterate through each unique waterYear
for(year in unique(hindcast_imputed_df$waterYear)) {
  
  # Filter the data for the current year where predicted_ice == "no ice"
  year_data <- hindcast_imputed_df %>% 
    filter(waterYear == year, predicted_ice == "ice") %>%
    arrange(wy_doy)  # Sort by wy_doy to find the first day
  
  # If there is any day where predicted_ice == "no ice"
  if (nrow(year_data) > 0) {
    # Get the first day (earliest day) in that year where predicted_ice == "no ice"
    first_ice_wy_doy <- year_data %>% slice(1)
    
    # Create a new row with the waterYear and first wy_doy
    result_row <- data.frame(
      waterYear = year,
      first_ice_wy_doy = first_ice_wy_doy$wy_doy
    )
    
    # Append the result_row to the result_df
    hindcasted_ice_on_dates_hydro <- bind_rows(hindcasted_ice_on_dates_hydro, result_row)
  }
}

# View the result data frame
print(hindcasted_ice_on_dates_hydro)
hindcasted_ice_on_dates_hydro <- hindcasted_ice_on_dates_hydro %>% mutate(model="hydro")
# save the dates as a csv for met data analysis:
#write.csv(hindcasted_ice_on_dates_hydro, "Input_Files/hindcasted_ice_on_dates_hydro")

#----------------------------------------------------------------------
#  05d. Hindcasting - combining all the ice_on dates from the model with each other and observed ice on dates
#----------------------------------------------------------------------

#### Pulling out observed ice-on dates
# Initialize an empty data frame to store the results
ice_on_observed <- data.frame()

# Iterate through each unique waterYear
for(year in unique(ice_only$waterYear)) {
  
  # Filter the data for the current year where predicted_ice == "no ice"
  year_data <- ice_only %>% 
    filter(waterYear == year, ice == 1) %>%
    arrange(wy_doy)  # Sort by wy_doy to find the first day
  
  # If there is any day where predicted_ice == "no ice"
  if (nrow(year_data) > 0) {
    # Get the first day (earliest day) in that year where predicted_ice == "no ice"
    first_ice_wy_doy <- year_data %>% slice(1)
    
    # Create a new row with the waterYear and first wy_doy
    result_row <- data.frame(
      waterYear = year,
      first_ice_wy_doy = first_ice_wy_doy$wy_doy
    )
    
    # Append the result_row to the result_df
    ice_on_observed <- bind_rows(ice_on_observed, result_row)
  }
}

# Adding "observed" to model column for observed df
ice_on_observed <- ice_on_observed %>% mutate(model="observed")
# Combining all three models hindcasts
all_three_hinds_df <- rbind(hindcasted_ice_on_dates_sink,hindcasted_ice_on_dates_met,hindcasted_ice_on_dates_hydro,ice_on_observed)
# changing column names and adding date
all_three_hinds_df <- all_three_hinds_df %>%
  mutate(
    ice_on_dowy = first_ice_wy_doy,
    ice_on_date = make_date(waterYear - 1, 10, 1) +
      days(first_ice_wy_doy - 1)
  ) %>%
  select(-first_ice_wy_doy)
# rearranging the columns to match Katie's hindcast tables
all_three_hinds_df <- all_three_hinds_df %>% select(c(model,waterYear,ice_on_date,ice_on_dowy))


ggplot(all_three_hinds_df,
       aes(x = waterYear, y = ice_on_dowy, color = model)) +
  geom_point(size = 3) +
  scale_color_manual(values = c(
    "met" = "orange",
    "hydro" = "steelblue",
    "sink" = "forestgreen",
    "observed" = "pink"
  )) +
  theme_minimal(base_size = 16) +
  labs(x = "Water Year", y = "Ice On Day of Water Year", title = "Hindcasted and Observed Ice On Dates")

# Write a CSV for all dates:
#write.csv(all_three_hinds_df, "derived_data/01_hindcast_ice_on_dates_lr")

