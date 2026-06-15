library(gratia)
library(mgcv)


# Summary -----------------------------------------------------------------


#Since the first derivative of a continuous function y = f(x) represents the slope
#of the tangent to the curve, and, since a slope defines a change in the dependent
#variable (y) corresponding to a change in the independent variable ( x),
#the first derivative is a rate of change. 

#This script helps us identify periods of change, inspired by this blog post:
#https://www.fromthebottomoftheheap.net/2014/05/15/identifying-periods-of-change-with-gams/

#For this script to run you need to load in the functions:
source("00_functions.R")
#And also load in cumulative_dat_ungroup from the LV_DischargePlots.Rmd file
#Feel free to rename the dataframe from "test" to something else



# Test example - one year, manually ---------------------------------------



test <- cumulative_dat_ungroup %>%
  filter(waterYear == 2018) 

### Model
mod <- gam(Flow ~ s(wy_doy, k=20),
                       data = test,
                       method = "REML")
draw(mod)
summary(mod)

#Generate a dataframe of predicted y values (e.g. Flow) based on the GAM model above
pred_df <-
  with(test, data.frame(wy_doy = seq(
    min(wy_doy, na.rm = TRUE),
    max(wy_doy, na.rm =
          TRUE),
    length.out = nrow(test)
  )))

pred_df <- bind_cols(pred_df, data.frame(
    predict(
      mod,
      pred_df,
      type = "response",
      se.fit = TRUE
    )
  )) %>%
  mutate(upper = fit + (2 * se.fit),
         lower = fit - (2 * se.fit),
         waterYear = "2014")


# Extract the first derivative from the model
Term <- "wy_doy"
m1.d <- Deriv(mod, n=nrow(test))
# Calculate confidence intervals around the 1st derivative 
m1.dci <- confint(m1.d, term = "wy_doy")

#Identify periods of change that are increasing or decreasing
m1.dsig <- signifD(pred_df$fit,
                   d = m1.d[[Term]]$deriv,
                   m1.dci[[Term]]$upper,
                   m1.dci[[Term]]$lower)

#Create a new dataframe which has a column for periods of time
#where the time series is accelerating or decelerating
accel <-
  data.frame(value_pred_incr = unlist(m1.dsig$incr),
             value_pred_decr = unlist(m1.dsig$decr),
             wy_doy = pred_df$wy_doy) %>%
  left_join(., pred_df) #Join with the original predicted dataframe for plotting the predicted "fit" 

#Visualize raw data, model fit, and lines showing where the trends are accelerating (red) or decelerating (blue)
test %>%
  ggplot(aes(x=wy_doy, y=Flow))+
  geom_point()+
  geom_line(data=accel, aes(x=wy_doy, y=fit)) +
  geom_line(data=accel, aes(x=wy_doy, y=upper), linetype="dashed") +
  geom_line(data=accel, aes(x=wy_doy, y=lower), linetype="dashed") +
  geom_line(data=accel, aes(x=wy_doy, y=value_pred_incr), color="red", linewidth=1.5)+
  geom_line(data=accel, aes(x=wy_doy, y=value_pred_decr), color="blue", linewidth=1.5)



# Same as above, but loop over all unique combinations of waterYear -----------------------------------------------------------------

library(dplyr)
library(mgcv)
library(ggplot2)
library(purrr) #This makes for loops, but tidy!

# Create a function to process each waterYear
process_water_year <- function(water_year) {
  # Filter the data for the specific water year
  test <- cumulative_dat_ungroup %>%
    filter(waterYear == water_year)
  
  # Fit the GAM model
  mod <- gam(Flow ~ s(wy_doy, k = 20),
             data = test,
             method = "REML")
  
  # Generate predictions with confidence intervals
  pred_df <- with(test, data.frame(
    wy_doy = seq(min(wy_doy, na.rm = TRUE),
                 max(wy_doy, na.rm = TRUE),
                 length.out = nrow(test))
  ))
  
  pred_df <- bind_cols(pred_df, data.frame(
    predict(mod, pred_df, type = "response", se.fit = TRUE)
  )) %>%
    mutate(upper = fit + (2 * se.fit),
           lower = fit - (2 * se.fit),
           waterYear = water_year)
  
  # Calculate the derivatives for periods of change
  Term <- "wy_doy"
  m1.d <- Deriv(mod, n = nrow(test))
  m1.dci <- confint(m1.d, term = "wy_doy")
  m1.dsig <- signifD(pred_df$fit,
                     d = m1.d[[Term]]$deriv,
                     m1.dci[[Term]]$upper,
                     m1.dci[[Term]]$lower)
  
  # Create the accel dataframe
  accel <- data.frame(value_pred_incr = unlist(m1.dsig$incr),
                      value_pred_decr = unlist(m1.dsig$decr),
                      wy_doy = pred_df$wy_doy) %>%
    left_join(pred_df, by = "wy_doy")
  
  return(accel)
}

# Apply the function to all unique waterYear values and store in a list
accel_list <- cumulative_dat_ungroup %>%
  pull(waterYear) %>%
  unique() %>%
  map(process_water_year)

# Optionally, you can bind the results together if you want a single dataframe
accel_combined <- bind_rows(accel_list, .id = "waterYear")

# Plot for one example water year (e.g., 2018)
example_year <- 2015
example_accel <- accel_list[[which(unique(cumulative_dat_ungroup$waterYear) == example_year)]]

test <- cumulative_dat_ungroup %>%
  filter(waterYear == example_year)

test %>%
  ggplot(aes(x = wy_doy, y = Flow)) +
  geom_point() +
  geom_line(data = example_accel, aes(x = wy_doy, y = fit)) +
  geom_line(data = example_accel, aes(x = wy_doy, y = upper), linetype = "dashed") +
  geom_line(data = example_accel, aes(x = wy_doy, y = lower), linetype = "dashed") +
  geom_line(data = example_accel, aes(x = wy_doy, y = value_pred_incr), color = "red", linewidth = 1.5)+
  geom_line(data = example_accel, aes(x = wy_doy, y = value_pred_decr), color = "blue", linewidth = 1.5)


