##############################################
# Libraries 
##############################################

if (!require('pacman')) install.packages('pacman'); library('pacman')

##Load all the libraries your heart desires
pacman::p_load(dplyr,
              dataRetrieval,
              lubridate,
              tidyr,
              ggplot2,
              viridis,
              readxl,
              imputeTS,
              tsibble,
              sjPlot,
              pROC,
              gridExtra,
              broom,
              gtsummary, 
              snotelr)

