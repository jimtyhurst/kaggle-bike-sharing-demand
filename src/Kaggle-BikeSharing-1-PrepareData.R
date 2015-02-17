# Kaggle competition: Bike Sharing Demand
# http://www.kaggle.com/c/bike-sharing-demand/
# Jim Tyhurst
# https://github.com/jimtyhurst/bike-sharing-demand

# This script loads the training data for the Bike Sharing Demand problem into 
# two data frames:
#   training.raw : direct representation of the original data
#     in the CSV file, without using factors. There are no NA values.
#   training.factors : interprets the data as factors and enhances the data 
#     with a few additional factors derived from the original factors:
#       'POSIXct',   # datetime
#       'factor',    # month: [JAN,FEB,...,DEC] derived from 'datetime'.
#       'factor',    # season: [WINTER, SPRING, SUMMER, FALL] derived from 'month'.
#       'factor',    # quarter: [Q1,Q2,Q3,Q4] renamed and relabeled from original 'season' predictor.
#       'factor',    # holiday: [FALSE, TRUE]
#       'factor',    # workingday: [FALSE, TRUE]
#       'factor',    # day.type: [WORKINGDAY, WEEKEND, HOLIDAY] derived from 'holiday' and 'workingday'.
#       'factor',    # weather:
#                        1: Clear, Few clouds, Partly cloudy, Partly cloudy
#                        2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#                        3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#                        4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
#       'numeric',   # temp: temperature in Celsius
#       'numeric',   # atemp: "feels like" temperature in Celsius
#       'integer',   # humidity: relative humidity
#       'numeric',   # windspeed: (units?)
#       'integer',   # casual: number of non-registered user rentals initiated
#       'integer',   # registered: number of registered user rentals initiated
#       'integer'    # count = total rentals = casual + registered

# Dependencies
library(plyr)
 
# Assumes a home.dir with a 'data' subdirectory:
# home.dir
#  |-- data
#       |-- train.csv
#       |-- test.csv
home.dir <- "~/projects/Kaggle/BikeSharingDemand"
setwd(home.dir)

#####
# Load raw data with types as specified in file.
# Interpret training data as numeric.
train.column.types.raw <- c(
  'POSIXct',   # datetime
  'integer',   # season: {1,2,3,4} ~ {JAN-MAR, APR-JUN, JUL-SEP, OCT-DEC}
  'integer',   # holiday: {0,1} ~ {FALSE, TRUE}
  'integer',   # workingday: {0,1} ~ {FALSE, TRUE}
  'integer',   # weather: {1: Clear, 2: Mist, 3: Light Snow, 4: Heavy Rain}
  'numeric',   # temp: temperature in Celsius
  'numeric',   # atemp: "feels like" temperature in Celsius
  'integer',   # humidity: relative humidity
  'numeric',   # windspeed: (units?)
  'integer',   # casual: number of non-registered user rentals initiated
  'integer',   # registered: number of registered user rentals initiated
  'integer'    # count = total rentals = casual + registered
)
training.raw <- read.csv("data/train.csv", header=TRUE, sep=",", colClasses=train.column.types.raw)
# Rename 'season' as 'quarter', because we will derive a 'season' predictor later.
training.raw <- rename(training.raw, replace=c('season' = 'quarter'))
str(training.raw)

#####
# Interpret training data with factors.
train.column.types.as.factors <- c(
  'POSIXct',   # datetime
  'factor',    # season: {1,2,3,4} ~ {JAN-MAR, APR-JUN, JUL-SEP, OCT-DEC}
  'integer',   # holiday: {0,1} ~ {FALSE, TRUE}
  'integer',   # workingday: {0,1} ~ {FALSE, TRUE}
  'factor',    # weather: {1: Clear, 2: Mist, 3: Light Snow, 4: Heavy Rain}
  'numeric',   # temp: temperature in Celsius
  'numeric',   # atemp: "feels like" temperature in Celsius
  'integer',   # humidity: relative humidity
  'numeric',   # windspeed: (units?)
  'integer',   # casual: number of non-registered user rentals initiated
  'integer',   # registered: number of registered user rentals initiated
  'integer'    # count = total rentals = casual + registered
)
training.factors <- read.csv("data/train.csv", header=TRUE, sep=",", colClasses=train.column.types.as.factors)
# Rename 'season' as 'quarter', because we will derive a 'season' predictor later.
training.factors <- rename(training.factors, replace=c('season' = 'quarter'))
str(training.factors)

#####
# Rename 'quarter' factor levels to: {Q1, Q2, Q3, Q4}
# in order to make their meaning more explicit.
training.factors$quarter <- mapvalues(
  training.factors$quarter, 
  from=c("1","2","3","4"), 
  to=c("Q1", "Q2", "Q3", "Q4")
)

#####
# Derive 'day.type' factor: {HOLIDAY, WORKDAY, WEEKEND} from 'holiday' and 'workingday' predictors.
# (holiday, workingday)
# (1,0) = holiday (non-weekend)
# (0,1) = work day
# (0,0) = weekend day
# (1,1) = not a logical possibility and never occurs in data
as.day.type <- function(is.holiday, is.workingday) {
  if (is.holiday != 0) {
    return("HOLIDAY")
  } else if (is.workingday != 0) {
    return("WORKDAY")
  } else {
    return("WEEKEND")
  }
}
training.factors$day.type <- factor(
  mapply(as.day.type, training.factors$holiday, training.factors$workingday),
  levels=c("WORKDAY", "WEEKEND", "HOLIDAY") # sorted by frequency
)

#####
# Convert 'holiday' and 'workingday' predictors into Boolean factors.
# [There must be an easier way to do this.]
int.to.boolean <- function(x) {
  if (x == 0) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
training.factors$holiday <- factor(sapply(training.factors$holiday, int.to.boolean))
training.factors$workingday <- factor(sapply(training.factors$workingday, int.to.boolean))

#####
# Derive 'season' factor: 
#   {WINTER, SPRING, SUMMER, FALL} 
# from datetime, based on whole months and
# assuming northern hemisphere, since data is for Washington, DC: 
#   WINTER: Dec - Feb
#   SPRING: Mar - May
#   SUMMER: Jun - Aug
#   FALL: Sep - Nov

# Extract 'month' factor from 'datetime'.
date.to.month <- function(x) {
  # Extract uppercase month abbreviation as a string from a date.
  return(toupper(format(x, "%b")))
}
training.factors$month <- factor(
  sapply(training.factors$datetime, date.to.month),
  levels=c("JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC")
)

# Derive 'season' from 'month'.
month.to.season <- function(x) {
  switch(as.character(x),
    "JAN" = "WINTER",
    "FEB" = "WINTER",
    "MAR" = "SPRING",
    "APR" = "SPRING",
    "MAY" = "SPRING",
    "JUN" = "SUMMER",
    "JUL" = "SUMMER",
    "AUG" = "SUMMER",
    "SEP" = "FALL",
    "OCT" = "FALL",
    "NOV" = "FALL",
    "DEC" = "WINTER"    
  )
}
training.factors$season <- factor(
  sapply(training.factors$month, month.to.season),
  levels=c("WINTER","SPRING","SUMMER","FALL")
)

#####
# Re-order the columns, so that similar factors are next to each other.
training.factors <- training.factors[
  c("datetime",
    "month",
    "season",
    "quarter",
    "holiday",
    "workingday",
    "day.type",
    "weather",
    "temp",
    "atemp",
    "humidity",
    "windspeed",
    "casual",
    "registered",
    "count"
  )
]

#####
# Save the enhanced data back to the file system.
write.csv(training.factors, file="data/train-factors.csv", row.names=FALSE)
