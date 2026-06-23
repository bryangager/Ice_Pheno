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
              snotelr, # for pull snotel data  
              tree, # for tree based models 
              randomForest, # for random forest specifically 
              gbm, # I think this is for xgboost but not 100% sure on that 
              precrec # for AUC (area under the curve) modeling tuning for xgb
            )

