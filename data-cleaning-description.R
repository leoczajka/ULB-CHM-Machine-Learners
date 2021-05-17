
rm(list = ls())
if (Sys.info()[8] == "leoczajka") {
  setwd("/Users/leoczajka/Dropbox (Leo_and_Co)/Research/learning/Doctoral Training/machine-learning/ULB-CHM-Machine-Learners")
}
if (Sys.info()[8] == "??") {
  setwd("~/BioInformatique/MA1/Q2/machine learning/projet")
}

if (Sys.info()[8] == "??") {
  setwd("~/RProjects/Pumpit")
}


# load packages
library(Hmisc)
library(tidyverse)
library(dplyr) 
library(caret)
library(xgboost)
library(mlbench)# For data
library(FSelector)

# Importing data sets 
train_values <- read.csv("trainingsetvalues.csv")
train_labels <- read.csv("trainingsetlabels.csv")
df <- merge(train_values, train_labels)
test_values <- read.csv("testsetvalues.csv")

# Elements of strategy
# start with the simplest model - without all hard to deal with data
# make a data set with missing values
# make a data set with replaced missing values
  # for cat-var = set missing to some cat
  # for num var - either remove the var or set to missing (remove data), or set to the mode/mean or P50
# remove var with too many weird values
# always remove unprecise small cat 
# add very precise cat-var progressively

# general cleaning : set categorical var into factor

# classify the variables based on type and number of different values
# get a glimpse at the data
describe(df)

# irrelevant or not varying variables
# num_private, recorded_by
describe(df$num_private)
describe(df$recorded_by)

# numerical variables
  # amount_tsh - but few values - 98, many 0
  # gps_height - half are 0
  # longitude 
  # latitude 
  # population - many 0
hist(df$latitude)
sum(df$latitude==0)
hist(df$longitude) # set 0 to Mode ? set NA 
sum(df$longitude==0)
hist(df$gps_height)
sum(df$gps_height==0)
df<- df[, !(colnames(df) %in% c("num_private", "recorded_by"))]

table(df$population)
hist(df$population[df$population>1 & df$population <500]) # 
 # put at test
  # unlikely counts for 0 and 1 - set to NA - set to mode or P50
hist(df$amount_tsh[df$amount_tsh>0 & df$amount_tsh<1000])
table(df$amount_tsh) # pretty weird values

# categorical var 
  # FEW
  # basin - 9
table(df$basin)

  # region - 21
table(df$region)

  # region_code  27
table(df$region_code)
table(df$region_code, df$region) # to be tested 

  # district_code - 20
table(df$district_code)
  # lga - 125
table(df$lga)
ggplot(df) + geom_bar(aes(x = lga))

  # public_meeting - 2
table(df$public_meeting)
  # scheme_management 12
table(df$scheme_management)
  # permit  2 - 3056 
table(df$permit)

  # construction_year  - 55
  hist(df$construction_year[df$construction_year!=0])
    # to be tested

  # extraction_type - 18 extraction_type_group extraction_type_class 
  table(df$extraction_type, df$extraction_type_group)
  table(df$extraction_type_group, df$extraction_type_class)
  
  # management management_group
  table(df$management, df$management_group )
  
  # payment payment_type
  table(df$payment,df$payment_type )
  
  # water_quality quality_group 
  table(df$water_quality,df$quality_group )
  
  # quantity quantity_group
  table(df$quantity, df$quantity_group)
  
  # source source_type source_class
  table(df$source, df$source_type )
  table(df$source_type,df$source_class )
  
  # waterpoint_type waterpoint_type_group
  table(df$waterpoint_type,df$waterpoint_type_group )
  
  # NUMEROUS
  # date_recorded - 356 different values - can be transformed into 
  ggplot(df) + geom_bar(aes(x = date_recorded))
  df$m_date <- as.Date(df$date_recorded)
  df$daily_time_trend <- as.numeric(df$m_date) # remove the too early ones
  hist(df$daily_time_trend[df$daily_time_trend>14942], breaks = seq(from=14942, to=16042, by=10))
  
  df$m_year <- as.factor(as.numeric(format(df$m_date,'%Y')))
  df$m_months <- as.factor(as.numeric(format(df$m_date,'%m')))
  df$m_day <- as.factor(weekdays(df$m_date))
  df$age <- df$m_year - df$construction_year 
  df$age[df$construction_year==0] <- NA # to b tested 
  
  # View(df %>% filter(df$time==53))

  ggplot(df) + geom_bar(aes(x = m_day))
  ggplot(df) + geom_bar(aes(x = m_months))
  # ward - 2092 
  # ggplot(df) + geom_bar(aes(x = ward))
  # funder -  1897  distinct - 3635 missing
  # installer - 2145 
  # wpt_name 37400 
  # subvillage 19287
  # scheme_name  - 2696
 df$installer <- tolower(df$installer)
 df$funder <- tolower(df$funder)
 df$wpt_name <- tolower(df$wpt_name)
 df$subvillage <- tolower(df$subvillage)
 df$scheme_name <- tolower(df$scheme_name)
 table(df$funder)
  df$funder_is_installer <- df$funder == df$installer & df$installer!=""
  df$funder_is_installer[df$installer=="" | df$funder==""] <- "MISSING"
  table(df$funder_is_installer)
  
# create variables :
  # distance to measuring dates
  # year and month of the measured

 # df<- df[, !(colnames(df) %in% c("region", "extraction_type_group", "extraction_type_class", "management_group", "payment_type", "quality_group", "quantity_group", "source_class","source_type", "waterpoint_type_group", "num_private", "recorded_by"))]
# remove also the date - better use it as numerical
str(df)
 # so what do we get for the most basic model?
 

 
  