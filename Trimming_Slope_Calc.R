## TRIMMING AND SLOPE CALC #####

# Copied from DEC Ponds LGR data KAG needs to update for Discharge data 

##### 0) Set Up R Environment  #### 

    # Set working directory 
    setwd("/Users/Gagers/Documents/Loch Vale & Oleksy/Ice Phenology Project/Ice_Pheno")    

    # Load Packages 
    suppressMessages(library(dplyr))
    suppressMessages(library(readxl))
    suppressMessages(library(tidyverse))
    suppressMessages(library(devtools))
    suppressMessages(library(lme4))
    suppressMessages(library(car))
    suppressMessages(library(mosaic))
    suppressMessages(library(writexl))
    suppressMessages(library(lubridate))
    suppressMessages(library(readxl))

  # Load Data 
    
    # Trimming Windows 
      trim_windows <- read_xlsx("Input_Files/20240819_ice_windows.xlsx")
      trim_windows <- as.data.frame(trim_windows)
      trim_windows$Full_Ice_Cover <- as.POSIXct(trim_windows$Full_Ice_Cover, tz = "MST", format = "%Y-%m-%d")
      trim_windows$Functional_Ice_Off <- as.POSIXct(trim_windows$Functional_Ice_Off, tz = "MST", format = "%Y-%m-%d")
      trim_windows$No_Ice_Cover <- as.POSIXct(trim_windows$No_Ice_Cover, tz = "MST", format = "%Y-%m-%d")
      trim_windows$Lake <- "LOC"
      trim_windows$Lake_WaterYear <- paste(trim_windows$Lake, trim_windows$Water_Year, sep = "_")
      trim_windows <- trim_windows %>% drop_na()
      head(trim_windows)


    # Discharge Data
        discharge_data <- read.csv("Input_Files/cumulativeQ_data.csv")
        head(discharge_data)
        
        #Format Discharge Data 
        discharge_data$Lake <- "LOC"
        discharge_data$Lake_WaterYear <- paste(discharge_data$Lake, discharge_data$waterYear, sep= "_")
        discharge_data$Date <- as.POSIXct(discharge_data$Date, tz = "MST", format = "%Y-%m-%d")
        head(discharge_data)
        
        # Subset discharge data to only years when we have ice on and ice off data - not using 2018 because it contains NAs
        water_years_of_interest <- c("2014", "2015", "2016", "2017","2018","2019", "2020", "2021", "2022", "2023")
        discharge_data_sub <- discharge_data[discharge_data$waterYear %in% water_years_of_interest , ]
        
        #Break up Discharge data into list by lake_WaterYear 
        discharge_data_lst <- split(discharge_data_sub, f = discharge_data_sub$Lake_WaterYear)
        
        # Plot to Check Discharge Data 
            # dummy_discharge <- discharge_data_lst[[3]]
            # discharge_plot <- ggplot(dummy_discharge, aes(x=Date, y=cumulative_dis)) +
              # geom_point()  +  geom_line() + 
              # theme_bw() +
              # labs(x = "Date", y = "Cumulative Discharge", title = "Cumulative Discharge over time") 
            # discharge_plot 
        
##### 1) Trim Discharge Data   ####    
# Trim the windows of discharge data based on the windows of ice off
        

# 1.1 Write Functions to Trim Discharge files based on off windows    

        # Dummy data to write function 
         discharge_df <- discharge_data_lst[[3]]
         windows_df <- as.data.frame(trim_windows)
         head(discharge_df)
         head(windows_df)
        
    # Trim From Full Ice Cover to No Ice Cover _________________________
    Trim_FullNo_FUNC <- function(discharge_df, windows_df){
      
      # Formatting Lake Water Years 
      discharge_df$Lake_WaterYear <- as.character(discharge_df$Lake_WaterYear)  # format ID to be a character in concentration df
      Lake_WaterYear_OfInterest <- discharge_df[1, "Lake_WaterYear"]  # save Unique ID as a variable 
      windows_df$Lake_WaterYear <- as.character(windows_df$Lake_WaterYear)  #format ID to be a character in windows_df
      windows_df_specific <- windows_df[windows_df$Lake_WaterYear == Lake_WaterYear_OfInterest , ]  # make a new dataframe from the windows_df with only the rows that correspond to the unique ID 
      
      # Extract Start and End 
      window_start <- windows_df_specific[1, "Full_Ice_Cover"]  #Using that new data frame that you made save the CO2 start time 
      window_start <- as.POSIXct(window_start, tz = "MST", format = "%Y-%m-%d")
      window_end <- windows_df_specific[1, "No_Ice_Cover"]  # Using the new data frame save the CO2 end time 
      window_end <- as.POSIXct(window_end, tz = "MST", format = "%Y-%m-%d")
      
      # Select only discharge in between start and end 
      discharge_df$Date <- as.POSIXct(discharge_df$Date, tz = "MST", format = "%Y-%m-%d")
      df <- discharge_df[discharge_df$Date >= window_start & discharge_df$Date <= window_end, ]  # make a subsetted data frame that only contains the rows that fall within the designated window 
      trimmed_df <- as.data.frame(df) # Save output as a data frame 
    }
     
    # Trim From Full Ice Cover to Functional Ice Off _________________________       
    Trim_FullFunctional_FUNC <- function(discharge_df, windows_df){
      
      # Formatting Lake Water Years 
      discharge_df$Lake_WaterYear <- as.character(discharge_df$Lake_WaterYear)  # format ID to be a character in concentration df
      Lake_WaterYear_OfInterest <- discharge_df[1, "Lake_WaterYear"]  # save Unique ID as a variable 
      windows_df$Lake_WaterYear <- as.character(windows_df$Lake_WaterYear)  #format ID to be a character in windows_df
      windows_df_specific <- windows_df[windows_df$Lake_WaterYear == Lake_WaterYear_OfInterest , ]  # make a new dataframe from the windows_df with only the rows that correspond to the unique ID 
      
      # Extract Start and End 
      window_start <- windows_df_specific[1, "Full_Ice_Cover"]  #Using that new data frame that you made save the CO2 start time 
      window_start <- as.POSIXct(window_start, tz = "MST", format = "%Y-%m-%d")
      window_end <- windows_df_specific[1, "Functional_Ice_Off"]  # Using the new data frame save the CO2 end time 
      window_end <- as.POSIXct(window_end, tz = "MST", format = "%Y-%m-%d")
      
      # Select only discharge in between start and end 
      discharge_df$Date <- as.POSIXct(discharge_df$Date, tz = "MST", format = "%Y-%m-%d")
      df <- discharge_df[discharge_df$Date >= window_start & discharge_df$Date <= window_end, ]  # make a subsetted data frame that only contains the rows that fall within the designated window 
      trimmed_df <- as.data.frame(df) # Save output as a data frame 
    }
    
    
    # Trim From Functional Ice Off to No Ice Cover _________________________       
    Trim_FunctionalNo_FUNC <- function(discharge_df, windows_df){
      
      # Formatting Lake Water Years 
      discharge_df$Lake_WaterYear <- as.character(discharge_df$Lake_WaterYear)  # format ID to be a character in concentration df
      Lake_WaterYear_OfInterest <- discharge_df[1, "Lake_WaterYear"]  # save Unique ID as a variable 
      windows_df$Lake_WaterYear <- as.character(windows_df$Lake_WaterYear)  #format ID to be a character in windows_df
      windows_df_specific <- windows_df[windows_df$Lake_WaterYear == Lake_WaterYear_OfInterest , ]  # make a new dataframe from the windows_df with only the rows that correspond to the unique ID 
      
      # Extract Start and End 
      window_start <- windows_df_specific[1, "Functional_Ice_Off"]  #Using that new data frame that you made save the CO2 start time 
      window_start <- as.POSIXct(window_start, tz = "MST", format = "%Y-%m-%d")
      window_end <- windows_df_specific[1, "No_Ice_Cover"]  # Using the new data frame save the CO2 end time 
      window_end <- as.POSIXct(window_end, tz = "MST", format = "%Y-%m-%d")
      
      # Select only discharge in between start and end 
      discharge_df$Date <- as.POSIXct(discharge_df$Date, tz = "MST", format = "%Y-%m-%d")
      df <- discharge_df[discharge_df$Date >= window_start & discharge_df$Date <= window_end, ]  # make a subsetted data frame that only contains the rows that fall within the designated window 
      trimmed_df <- as.data.frame(df) # Save output as a data frame 
    }

        # # Check function 
            check <- Trim_FullNo_FUNC (discharge_data_lst[[2]], trim_windows)
            head(check)

  # 1.2 Apply each function across the list of discharge data 
    discharge_trimmed_FullNo_lst <- lapply(discharge_data_lst, windows_df = trim_windows, FUN = Trim_FullNo_FUNC)
    discharge_trimmed_FullFunctional_lst <- lapply(discharge_data_lst, windows_df = trim_windows, FUN = Trim_FullFunctional_FUNC)
    discharge_trimmed_FunctionalNo_lst <- lapply(discharge_data_lst, windows_df = trim_windows, FUN = Trim_FunctionalNo_FUNC)
    
    # Plot Trimmed to Check 
        # discharge_plot <- ggplot(discharge_trimmed_FullNo_lst[[5]], aes(x=Date, y=cumulative_dis)) +
        # geom_point()  +  geom_line() +
        # theme_bw() +
        # labs(x = "Date", y = "Cumulative Discharge", title = "Cumulative Discharge over time")
        # discharge_plot

# ________________________________________________________________________________________________
##### 2) Slope Calc #### 

# 2.1 Write Function to calculate slope
    
    # Dummy data to write function 
       trimmed_discharge <- discharge_trimmed_FullNo_lst[[5]]

    SlopeCalc_FUNC <- function(trimmed_discharge){
      
      # Start time as Index 
      Lake_WaterYear_OfInterest <- as.character(trimmed_discharge[1, "Lake_WaterYear"])
      
      # Start and end cumulative flow 
      start_cumulative_dis <- trimmed_discharge[1, "cumulative_dis"] # cumulative discharge at the start of the trimmed window
      last_row_number <- nrow(trimmed_discharge) %>% as.numeric()
      end_cumulative_dis <- trimmed_discharge[last_row_number, "cumulative_dis"]
      
      # Start and end date 
      window_start_date <- trimmed_discharge[1, "Date"] %>% as.character()
      window_end_date <- trimmed_discharge[last_row_number, "Date"] %>% as.character()
      
      #Linear models and extracting regression coefficients 
      lm_discharge <- lm(cumulative_dis ~ Date, data = trimmed_discharge) #Linear Regression of CO2 concentration over time 
      R2 <- summary(lm_discharge)$r.squared   #pull the R^2 of the regression line and save to a variable 
      pvalue <- summary(lm_discharge)$coefficients[2,4]   #pull the R^2 of the regression line and save to a variable 
      slope <- summary(lm_discharge)$coefficients[2,1]     #pull the slope of the regression line and save to a variable
      intercept <- summary(lm_discharge)$coefficients[1,1]
      
      # Save Results 
      output <- c(Lake_WaterYear_OfInterest, window_start_date, window_end_date, 
                       start_cumulative_dis, end_cumulative_dis, 
                       R2, pvalue, slope, intercept)
    }

        # # Check that the function works on one df 
              # check <-  SlopeCalc_FUNC(discharge_trimmed_FullNo_lst[[5]])
              # check

# 2.2 Apply the function across data 
  SlopeCalc_Output_FullNo <- lapply(discharge_trimmed_FullNo_lst, SlopeCalc_FUNC)
  SlopeCalc_Output_FullFunctional <- lapply(discharge_trimmed_FullFunctional_lst, SlopeCalc_FUNC)
  SlopeCalc_Output_FunctionalNo <- lapply(discharge_trimmed_FunctionalNo_lst, SlopeCalc_FUNC)

# 2.3 Clean up Output 
  new_row_names <- c("Lake_WaterYear", "start_date", "end_date", 
                        "start_cumulative_dis_cfs", "end_cumulative_dis_cfs", 
                        "R2", "pvalue", "slope_cfs_per_day", "intercept")
 
   Slope_FullNo <- do.call(rbind, SlopeCalc_Output_FullNo) %>% as.data.frame()
        names(Slope_FullNo) <- new_row_names
        row.names(Slope_FullNo) <- seq(1:nrow(Slope_FullNo))
        Slope_FullNo$Window <- "full_ice_to_no_ice"
        head(Slope_FullNo)
        
  Slope_FullFunctional <- do.call(rbind, SlopeCalc_Output_FullFunctional) %>% as.data.frame()
        names(Slope_FullFunctional) <- new_row_names
        row.names(Slope_FullFunctional) <- seq(1:nrow(Slope_FullFunctional))
        Slope_FullFunctional$Window <- "full_ice_to_functional_ice_off"
        head(Slope_FullFunctional)
        
  Slope_FunctionalNo <- do.call(rbind, SlopeCalc_Output_FunctionalNo) %>% as.data.frame()
        names(Slope_FunctionalNo) <- new_row_names
        row.names(Slope_FunctionalNo) <- seq(1:nrow(Slope_FunctionalNo))
        Slope_FunctionalNo$Window <- "functional_ice_off_to_no_ice"
        head(Slope_FunctionalNo)
  
# 2.4 Save Output   
    write_xlsx(Slope_FullNo ,"Output_Files/20240826_DischargeSlope_FullNo.xlsx")
    write_xlsx(Slope_FullFunctional ,"Output_Files/20240826_DischargeSlope_FullFunctional.xlsx")
    write_xlsx(Slope_FunctionalNo  ,"Output_Files/20240826_DischargeSlope_FunctionalNo.xlsx")



