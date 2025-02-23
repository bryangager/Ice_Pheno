library(pacman)
p_load(dplyr,dataRetrieval,lubridate,tidyr,ggplot2,viridis,readxl,imputeTS,tsibble,sjPlot,pROC,gridExtra,broom,gtsummary,patchwork,GGally)

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
march_start <- "03-01"
april_start <- "04-01"
april_end <- "05-01"
may_end <- "06-01"

### March - April
# Add year-based grouping and calculate cumulative sums
march_april_cumul_temp <- weather_daily %>%
  mutate(
    year = format(Date, "%Y"),                     # Extract the year
    month_day = format(Date, "%m-%d")              # Extract month and day
  ) %>%
  filter(month_day >= march_start & month_day < april_end) %>% # Filter by date window
  group_by(waterYear) %>%                               # Group by year
  summarize(
    T_MarAprcumul = sum(T_air_2_m_mean))     # Cumulative sum for T_air_2_m_mean

### March - May
# Add year-based grouping and calculate cumulative sums
march_may_cumul_temp <- weather_daily %>%
  mutate(
    year = format(Date, "%Y"),                     # Extract the year
    month_day = format(Date, "%m-%d")              # Extract month and day
  ) %>%
  filter(month_day >= march_start & month_day < may_end) %>% # Filter by date window
  group_by(waterYear) %>%                               # Group by year
  summarize(
    T_MarMaycumul = sum(T_air_2_m_mean))     # Cumulative sum for T_air_2_m_mean

### April-May
# Add year-based grouping and calculate cumulative sums
april_may_cumul_temp <- weather_daily %>%
  mutate(
    year = format(Date, "%Y"),                     # Extract the year
    month_day = format(Date, "%m-%d")              # Extract month and day
  ) %>%
  filter(month_day >= april_start & month_day < may_end) %>% # Filter by date window
  group_by(waterYear) %>%                               # Group by year
  summarize(
    T_AprMaycumul = sum(T_air_2_m_mean))     # Cumulative sum for T_air_2_m_mean


# Now calculate the monthly sum air temp for Feb-May individually:
monthly_cumul_temp <- weather_daily %>%
  mutate(month = month(Date, label=TRUE)) %>%
  filter(month %in% c("Feb","Mar","Apr","May")) %>%
  group_by(waterYear, month) %>%
  summarize(T_ = sum(T_air_2_m_mean)) %>%
  pivot_wider(names_from = "month", values_from = "T_",
              names_glue = "{.value}_{month}")

# hooray! now we have mean air temp and cumulative air temp for many different windows. Let's merge it all together:

met_full <- april_may_cumul_temp %>%
  left_join(., monthly_cumul_temp, by = "waterYear") %>%
  left_join(., hindcast_SWE %>% select(contains("wydoy"),max_swe,waterYear,predicted_ice_off_wy_doy), by ="waterYear")

met_full <- left_join(met_full,march_may_cumul_temp,by="waterYear")

met_full <- left_join(met_full,march_april_cumul_temp,by="waterYear")

met_full <- met_full %>% select(c("waterYear","predicted_ice_off_wy_doy","T__Feb","T__Mar","T__Apr","T__May","T_MarAprcumul","T_AprMaycumul","T_MarMaycumul","first_snow_acc_wydoy","cont_snow_acc_wydoy","first_snow_melt_wydoy","last_snow_melt_wydoy","max_swe_wydoy","max_swe"))
# This is the final df with the temperatures, snow, and hindcasted dates.
met_full <- met_full %>% rename(c(hindcast_ice_off_wy_doy=predicted_ice_off_wy_doy))

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



twent_perc_dates <- cumul_dis_percentiles %>% filter(percentile==0.2 & waterYear<2024) %>% rename(wy_doy_20Percent=wy_doy) %>% select(-percentile)

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
  theme_bw()+
  scale_color_gradient(low="blue",high="red")+
  geom_abline(intercept=0, slope=1)+
  labs(y="Modified Pierson Ice-Off Date",x="Outlet Modelled Ice-Off Date")+
  scale_y_continuous(breaks=labels_pier,labels=c(
    "20-May","30-May","09-Jun","19-Jun", "29-Jun","09-Jul"),limits=c(230,290))+
  scale_x_continuous(breaks=labels_pier,labels=c(
    "20-May","30-May","09-Jun","19-Jun", "29-Jun","09-Jul"),limits=c(230,290))
ggsave("Figures/pierson_mine_1:1.png", dpi=600, width=6, height=4, units="in")




############# Correlation scatter plots:

labels_x<-c(220,240,260,280)
labels_y<-c(230,240,250,260,270)

aprTempPlot <-
  ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=T__Apr))+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y="April Cumul. Temperature (C)", x="Hindcasted Date of Ice-Off")+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(210,281))
aprTempPlot

mayTempPlot <-
  ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=T__May))+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y="May Cumul. Temperature (C)", x="Hindcasted Date of Ice-Off")+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(210,281))
mayTempPlot

marAprTempPlot <- 
ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=T_MarAprcumul))+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y="March-April Cumul. Temperature (C)", x="Hindcasted Date of Ice-Off")+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(210,281))
marAprTempPlot

marMayTempPlot <-
  ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=T_MarMaycumul))+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y="March-May Cumul. Temperature (C)", x="Hindcasted Date of Ice-Off")+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(220,281))
marMayTempPlot

aprMayTempPlot <-
  ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=T_AprMaycumul))+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y="April-May Cumul. Temperature (C)", x="Hindcasted Date of Ice-Off")+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(210,281))
aprMayTempPlot

firstSnowPlot <-
  ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=first_snow_melt_wydoy))+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y="Date of First Snowmelt", x="Hindcasted Date of Ice-Off")+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(210,280))+
  scale_y_continuous(breaks=labels_y,labels=c("May-17","May-27","Jun-06","Jun-16","Jun-26"),limits=c(229,270))
firstSnowPlot

lastSnowPlot <-
  ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=last_snow_melt_wydoy))+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y="Date of Last Snowmelt", x="Hindcasted Date of Ice-Off")+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(210,280))+
  scale_y_continuous(breaks=labels_y,labels=c("May-17","May-27","Jun-06","Jun-16","Jun-26"),limits=c(229,270))
lastSnowPlot

new_y <- c(170,190,210,230)

maxSWEDatePlot <-
  ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=max_swe_wydoy))+
  geom_point(size=3)+
  theme_bw()+
  theme(axis.title.x = element_blank())+
  labs(y="Date of Max Snow Water Equivalent", x="Hindcasted Date of Ice-Off")+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(211,280))+
  scale_y_continuous(breaks=new_y,labels=c("Mar-18","Apr-07","Apr-27","May-17"),limits=c(169,244))
maxSWEDatePlot

maxSWEPlot <-
  ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=max_swe))+
  geom_point(size=3)+
  theme_bw()+
  labs(y="Max Snow Water Equivalent", x="Hindcasted Date of Ice-Off")+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(210,281))
maxSWEPlot

new_y_again <- c(230,240,250,260)

twenPercPlot <-
  ggplot(met_full, aes(x=hindcast_ice_off_wy_doy,y=wy_doy_20Percent))+
  geom_point(size=3)+
  theme_bw()+
  labs(y="Date 20th Percentile Cumul Discharge", x="Hindcasted Date of Ice-Off")+
  scale_y_continuous(breaks=new_y_again,labels=c("May-17","May-27","Jun-06","Jun-16"),limits=c(229,260))+
  scale_x_continuous(breaks=labels_x,labels=c("May-07","May-27","Jun-16","Jul-06"),limits=c(212,281))
twenPercPlot

metScatterPlots <- (aprTempPlot | mayTempPlot | marAprTempPlot) /
  (aprMayTempPlot | marMayTempPlot | maxSWEDatePlot) / 
  (maxSWEPlot | twenPercPlot | plot_spacer())
metScatterPlots


### Analysis of weird years:
labels_x_newer<-c(222,232,242,252,262)

ggplot(
  flow_temp_cond_daily_ice %>%
    filter(waterYear == 2022),
  aes(x = wy_doy, y = Temperature_C, group = waterYear)) +
  theme_bw() +
  geom_line(size=1) +
  geom_point(aes(x = wy_doy, y = Temperature_C, group = waterYear),size=1.5)+
  geom_vline(xintercept=243, color="forestgreen")+ # observed
  geom_vline(xintercept=252, color="brown")+ # hindcasted
  lims(x=c(220,265))+
  scale_x_continuous(breaks=labels_x_newer,labels=c("May-10","May-20", "May-30","Jun-09","Jun-19"),limits=c(220,265))+
  labs(x="Date", y ="Temperature (C)" )
#ggsave("Figures/2022_temp.png", dpi=600, width=6, height=8, units="in")

ggplot(
  flow_temp_cond_daily_ice %>%
    filter(waterYear == 2023),
  aes(x = wy_doy, y = Temperature_C, group = waterYear)) +
  theme_bw() +
  geom_line(size=1) +
  geom_point(aes(x = wy_doy, y = Temperature_C, group = waterYear),size=1.5)+
  geom_vline(xintercept=242, color="forestgreen")+ # observed
  geom_vline(xintercept=253, color="brown")+ # hindcasted
  lims(x=c(220,265))+
  scale_x_continuous(breaks=labels_x_newer,labels=c("May-10","May-20", "May-30","Jun-09","Jun-19"),limits=c(220,265))+
  labs(x="Date", y ="Temperature (C)" )
#ggsave("Figures/2023_temp.png", dpi=600, width=6, height=8, units="in")


