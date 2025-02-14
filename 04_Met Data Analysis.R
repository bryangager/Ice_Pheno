source("Input_Files/00_functions.R")
source("Input_Files/01_Data_Input.R")

## First, bring in the dfs for met data and outlet data as well as predicted days of ice-off:

# Bear Lake Snow Telemetry Data:
SWE_stats <- read.csv("Input_Files/Bear_SWE_stats.csv")

# Weather station data (temp, wind):
weatherData <- read.csv("Input_Files/subdaily_met_1991to2022.csv") %>% select(date_time, waterYear, T_air_2_m, T_air_6_m, WSpd_2_m, WSpd_6_m, SWin_2m6m_mean)

# hindcasted dates of ice-off
hindcasted_dates <- read.csv("Input_Files/hindcasted_ice_off_dates.csv") %>% select(-"X")

# hindcasted dates and SWE
hindcast_SWE <- full_join(SWE_stats,hindcasted_dates, by="waterYear") %>% rename(predicted_ice_off_wy_doy=first_no_ice_wy_doy)

## Weather needs to be daily - here I find the mean daily air temp and wind speed
weather_daily <- weatherData %>%
  mutate(Date = as.Date(date_time)) %>% # Extract the date part
  group_by(Date) %>% # Group by date
  summarise(
    T_air_2_m_mean = mean(T_air_2_m, na.rm = TRUE),
    T_air_6_m_mean = mean(T_air_6_m, na.rm = TRUE),
    WSpd_2_m_mean = mean(WSpd_2_m, na.rm = TRUE),
    WSpd_6_m_mean = mean(WSpd_6_m, na.rm = TRUE),
    SWin_2m6m_daily_mean = mean(SWin_2m6m_mean, na.rm = TRUE)
  ) %>% addWaterYear()
## Now I'll calculate the cumulative temperature of different time windows
# CHANGE THESE DATES TO MAKE EACH DATAFRAME
start_month_day <- "03-01"
end_month_day <- "05-01"

# Add year-based grouping and calculate cumulative sums
march_april_cumul_temp <- weather_daily %>%
  mutate(
    year = format(Date, "%Y"),                     # Extract the year
    month_day = format(Date, "%m-%d")              # Extract month and day
  ) %>%
  filter(month_day >= start_month_day & month_day < end_month_day) %>% # Filter by date window
  group_by(waterYear) %>%                               # Group by year
  # mutate(
  #   T_air_2_m_AprilMaycumul = cumsum(T_air_2_m_mean),     # Cumulative sum for T_air_2_m_mean
  #   T_air_6_m_AprilMaycumul = cumsum(T_air_6_m_mean)      # Cumulative sum for T_air_6_m_mean
  # ) %>%
  summarize(
    T_MarAprcumul = sum(T_air_2_m_mean)     # Cumulative sum for T_air_2_m_mean
    # T_air_6_m_AprilMaycumul = sum(T_air_6_m_mean)      # Cumulative sum for T_air_6_m_mean
  ) 
  # ungroup()  %>%
  # distinct(., waterYear, T_air_2_m_AprilMaycumul, .keep_all = FALSE)

monthly_cumul_temp <- weather_daily %>%
  mutate(month = month(Date, label=TRUE)) %>%
  filter(month %in% c("Feb","Mar","Apr","May")) %>%
  group_by(waterYear, month) %>%
  summarize(T_ = sum(T_air_2_m_mean)) %>%
  pivot_wider(names_from = "month", values_from = "T_",
              names_glue = "{.value}_{month}")

april_may_cumul_temp %>% 
  ggplot(aes(x=waterYear, y=T_AprMaycumul))+
  geom_point()

# hooray! now we have mean air temp and cumulative air temp for many different windows. Let's merge it all together:

met_full <- april_may_cumul_temp %>%
  left_join(., monthly_cumul_temp, by = "waterYear") %>%
  left_join(., hindcast_SWE %>% select(contains("wydoy"),max_swe,waterYear,predicted_ice_off_wy_doy), by ="waterYear")

met_full <- left_join(met_full,march_may_cumul_temp,by="waterYear")

met_full <- left_join(met_full,march_april_cumul_temp,by="waterYear")

met_full <- met_full %>% select(c("waterYear","predicted_ice_off_wy_doy","T__Feb","T__Mar","T__Apr","T__May","T_MarAprcumul.x","T_AprMaycumul","T_MarMaycumul","first_snow_acc_wydoy","cont_snow_acc_wydoy","first_snow_melt_wydoy","last_snow_melt_wydoy","max_swe_wydoy","max_swe"))
# This is the final df with the temperatures, snow, and hindcasted dates.
met_full <- met_full %>% rename(c(T_MarAprcumul=T_MarAprcumul.x,hindcast_ice_off_wy_doy=predicted_ice_off_wy_doy))

##### Correlations


ggpairs(met_full)+theme_bw()

ggpairs(met_full %>% select(T_AprMaycumul,T_MarAprcumul,T_MarMaycumul,T__Apr,T__May,
                            first_snow_melt_wydoy, last_snow_melt_wydoy,
                            max_swe_wydoy, max_swe,hindcast_ice_off_wy_doy))+theme_bw()


##### 20th Percentile Dates:

# Function to find wy_doy for specific percentiles
find_percentiles <- function(data, percentiles) {
  total_dis <- max(data$cumulative_dis, na.rm = TRUE) # Total discharge for the waterYear
  targets <- percentiles * total_dis                  # Target values for the percentiles
  
  # Find wy_doy for each target
  data.frame(
    percentile = percentiles,
    wy_doy = sapply(targets, function(target) {
      data$wy_doy[which.min(abs(data$cumulative_dis - target))]
    })
  )
}

# Apply the function for each waterYear
cumul_dis_percentiles <- cumulative_dat_ungroup %>%
  group_by(waterYear) %>%
  group_modify(~ find_percentiles(.x, c(0.2, 0.5, 0.8)))

twent_perc_dates <- twent_perc_dates %>% addWaterYear() %>% select(c(waterYear,wy_doy_20Percent))

#### Join 20th percentile of cumulative discharge dates with met_full data
met_full <- left_join(met_full,twent_perc_dates,by="waterYear") 


# Plot of 20th percentile wy_doy over time
ggplot(twent_perc_dates, aes(x=waterYear,y=wy_doy_20Percent))+
  geom_point()

### Correlation with 20 percetn dates
ggpairs(met_full %>% select(T_AprMaycumul,T_MarAprcumul,T_MarMaycumul,T__Apr,T__May,
                            first_snow_melt_wydoy, last_snow_melt_wydoy,
                            max_swe_wydoy, max_swe,wy_doy_20Percent,hindcast_ice_off_wy_doy))+theme_bw()
## 20th percent dates woohoo


##### Pierson Dates:
pierson_dates <- read_xlsx("Input_Files/Peirson_IcePhenoDates_AVG_20250120.xlsx")
pierson_dates <- pierson_dates %>% mutate(Ice_Off_Peirson = as.Date(Ice_Off_Peirson)) %>%  mutate(wy_doy_pierson_off = hydro.day(Ice_Off_Peirson))
pierson_dates <- pierson_dates %>% mutate(Ice_On_Peirson = as.Date(Ice_On_Peirson)) %>%  mutate(wy_doy_pierson_on = hydro.day(Ice_On_Peirson))

pierson_hindcast_dates <- left_join(pierson_dates,hindcasted_dates,by="waterYear")

# pierson vs. my model dates
labels_pier=c(232,242,252,262,272,282)

ggplot(pierson_hindcast_dates, aes(x=first_no_ice_wy_doy,y=wy_doy_pierson_off))+
  geom_point()+
  scale_color_gradient(low="blue",high="red")+
  geom_abline(intercept=0, slope=1)+
  labs(y="Modified Pierson Ice-Off Date",x="Outlet Modelled Ice-Off Date")+
  scale_y_continuous(breaks=labels_pier,labels=c(
    "20-May","30-May","09-Jun","19-Jun", "29-Jun","09-Jul"),limits=c(230,290))+
  scale_x_continuous(breaks=labels_pier,labels=c(
    "20-May","30-May","09-Jun","19-Jun", "29-Jun","09-Jul"),limits=c(230,290))
ggsave("Figures/pierson_mine_1:1.png", dpi=600, width=6, height=4, units="in")
