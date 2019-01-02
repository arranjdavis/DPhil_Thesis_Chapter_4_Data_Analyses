################################################################################################################################################

#clean environment
rm(list = ls())

#working directory
setwd("/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1/")

library(lubridate)
library(lme4)
library(optimx)
library(lmerTest)
library(RColorBrewer)
library(nlme)

#load the different data sets, where the FPRS variable is calculated by having finished within 15, 60, 120, or 180 s of partners
mydata.15sec <- read.csv("full_data_set_ordered_by_date_friends_runs_partners.csv")
mydata.1min <- read.csv("full_data_set_ordered_by_date_friends_1min_runs_partners.csv")
mydata.2min <- read.csv("full_data_set_ordered_by_date_friends_2min_runs_partners.csv")
mydata.3min <- read.csv("full_data_set_ordered_by_date_friends_3min_runs_partners.csv")

#create a list of the data sets
data_list = c("mydata.15sec", "mydata.1min", "mydata.2min", "mydata.3min")

################################################################################################################################################

### RUN THE MAIN MODEL ON ALL OF THE DATA SETS ###

#iterate through all data sets
for (i in data_list){
  
  #make the current object the data set
  mydata = get(i)
  
  ### DESCRIPTIVE STATISTICS ###
  
  #total number of parkrunners and runs in the original data set
  length(unique(mydata$athnumber)) #there are 1,160,283 parkrunners in original data set
  nrow(mydata)
  
  ### VARIABLE TRANSFORMATIONS ###
  
  #converts times to seconds
  mydata$time <- period_to_seconds(hms(mydata$time))
  
  #converts time to minutes
  mydata$time <- (mydata$time/60)
  
  #apply natural log transformation to the time variable
  mydata$time.lg = log(mydata$time)
  
  #apply log2 transformation to the previous runs and FPRS variables
  mydata$previous_runs.lg2 = log2(1 + mydata$previous_runs)
  mydata$partner_relation.lg2 = log2(1 + mydata$partner_relation)
  
  #make running partner variable a factor
  mydata$partners = as.factor(mydata$partners)
  
  ### DATA REDUCTION ###
  
  #select only people over age 18 and under 95, and not in a wheelchair
  
  #see age distribution
  table(mydata$age)
  
  #remove those under 18 and over 95, as well as those in a wheelchair
  mydata = subset(mydata, age != "C" & age != "10" & age != "11-14" & age != "15-17" & age != "---" & age != "100-104" & age != "110-114")
  mydata$age = droplevels(mydata$age)
  mydata$age = as.integer(mydata$age)
  
  #see age distribution again
  table(mydata$age)
  
  #get the total number of parkrunners, runs, parkrun events, and locations after this transformation
  length(unique(mydata$athnumber))
  nrow(mydata)
  length(unique(mydata$parkrunid))
  
  #creates a 'location' variable
  mydata$location = gsub("\\d+", "", mydata$parkrunid)
  mydata$location = as.factor(mydata$location)
  
  #gets the total number of locations
  length(unique(mydata$location))
  
  ### ### ### ### ### ### ### ### ###
  
  #this removes all parkrunners with fewer than 10 observations
  
  #drops any athletes with less than 10 runs
  mydata$athnumber = as.factor(mydata$athnumber)
  tbl = table(mydata$athnumber) 
  mydata.more_than_10 = droplevels(mydata[mydata$athnumber %in% names(tbl)[tbl >= 10],,drop=TRUE])
  
  #drop unused parkrunners and locations
  mydata.more_than_10$athnumber = droplevels(mydata.more_than_10$athnumber) 
  mydata.more_than_10$location = droplevels(mydata.more_than_10$location) 
  
  #get the total number of parkrunners, runs, and parkrun events after this transformation
  length(unique(mydata.more_than_10$athnumber))
  nrow(mydata.more_than_10)
  length(unique(mydata.more_than_10$parkrunid))
  length(unique(mydata.more_than_10$location))
  
  ### ### ### ### ### ### ### ### ###
  
  #reduce data set to UK runs only
  mydata.UK.more_than_10 = subset(mydata.more_than_10, Country == "UK")
  
  #get the total number of parkrunners, runs, and parkrun events after this transformation (this is the data set used for the model reported in the main text)
  #this will include some parkrunners with fewer than 10 runs (those that ran fewer than 10 times in the UK but more than 10 times at all global locations;
  #excluding these parkrunners leads the model estimated below to fail to converge)
  
  ### DESCRIPTIVE STATISTICS FOR FINAL MODEL ###
  
  print('DESCRIPTIVE STATISTICS FOR FINAL MODEL:')
  
  #get the total number of parkrunners, runs, and parkrun events after this transformation
  length(unique(mydata.UK.more_than_10$athnumber))
  nrow(mydata.UK.more_than_10)
  length(unique(mydata.UK.more_than_10$parkrunid))
  length(unique(mydata.UK.more_than_10$location))
  
  ### MULTILEVEL MODEL ###
  
  #excluding runnners with less than 10 runs and runs not at UK parkruns (this model converges)
  results.UK.more_than_10 <- lmer(time.lg ~ 1 + gender + age + partners + previous_runs.lg2 + partner_relation.lg2 + (previous_runs.lg2 + partner_relation.lg2 | athnumber), data = mydata.UK.more_than_10)
  
  #print the data set and the model summary
  print(i)
  summary(results.UK.more_than_10)
  
}

