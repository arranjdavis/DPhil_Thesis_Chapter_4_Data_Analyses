#clean environment
rm(list = ls())

library(lubridate)
library(dplyr)

#create timer for entire script
print(Sys.time())
beginning = Sys.time()

#data set
mydata <- read.csv("full_data_set_ordered_by_date_friends_runs_partners.csv")

################################################################################################################################################

### VARIABLE TRANSFORMATIONS

#converts times to seconds
mydata$time <- period_to_seconds(hms(mydata$time))

#converts time to minutes
mydata$time <- (mydata$time/60)

#apply natural log transformation to the time variable
mydata$time.lg = log(mydata$time)

#apply natural log and scale transformation to the time variable
mydata$time.lg.sc = scale(log(mydata$time))

#apply log2 transformation to the previous runs and FPRS variables
mydata$previous_runs.lg2 = log2(1 + mydata$previous_runs)
mydata$partner_relation.lg2 = log2(1 + mydata$partner_relation)

#apply log transformation to the previous runs and FPRS variables
mydata$previous_runs.lg = log(1 + mydata$previous_runs)
mydata$partner_relation.lg = log(1 + mydata$partner_relation)

#apply log and scale transformations to the previous runs and FPRS variables
mydata$previous_runs.lg.sc = log(1 + mydata$previous_runs)
mydata$partner_relation.lg.sc = log(1 + mydata$partner_relation)

#make running partner variable a factor
mydata$partners = as.factor(mydata$partners)

################################################################################################################################################

### DATA REDUCTION ###

#select only people over age 18 and under 95, and not in a wheelchair

#see age distribution
table(mydata$age)

#make age variable an integer
mydata$age.int = as.integer(mydata$age)
table(mydata$age.int)

#subset by age
mydata <- subset(mydata, age.int > 6)
mydata <- subset(mydata, age.int <= 22)

#rename age variable
mydata$age = mydata$age.int

#get the total number of parkrunners, runs, parkrun events, and locations after this transformation
length(unique(mydata$athnumber))
nrow(mydata)
length(unique(mydata$parkrunid))

#creates a 'location' variable
mydata$location = gsub("\\d+", "", mydata$parkrunid)
mydata$location = as.factor(mydata$location)

#gets the total number of locations
length(unique(mydata$location))

### ### ###

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

### ### ###

#reduces data set to UK runs only
mydata.UK.more_than_10 = subset(mydata.more_than_10, Country == "UK")
mydata.UK.more_than_10$athnumber = droplevels(mydata.UK.more_than_10$athnumber)

#make sure that there are no runners with fewer than 10 runs
tbl = table(mydata.UK.more_than_10$athnumber) 
mydata.UK.more_than_10 = droplevels(mydata.UK.more_than_10[mydata.UK.more_than_10$athnumber %in% names(tbl)[tbl >= 10],,drop=TRUE])

#get the total number of parkrunners, runs, and parkrun events after this transformation
length(unique(mydata.UK.more_than_10$athnumber))
nrow(mydata.UK.more_than_10)
length(unique(mydata.UK.more_than_10$parkrunid))
length(unique(mydata.UK.more_than_10$location))

################################################################################################################################################

### CREATION OF EMPTY DATA FRAMES ###

#this will create empty data sets to be filled with the results of the models run on data from different locations
dat_int.part_cont = data.frame()
dat_gen.part_cont = data.frame()
dat_age.part_cont = data.frame()
dat_run.part_cont = data.frame()
dat_bud.part_cont = data.frame()
dat_part.part_cont = data.frame()
dat_loc = data.frame()

################################################################################################################################################

### RUN FPRS MODEL ON EACH UK PARKRUN LOCATION ###

#timer for the for loop
beginning = Sys.time()

#this will create a list of unique parkruns
uniq.locations = unique(mydata$location)

#this will track the progress of the script
count = 1

#iterate through all the locations
for (i in uniq.locations) {
  
  #this will skip locations where the model fails to converge
  if (i == "keswick") next 
  if (i == "barryisland") next
  if (i == "bramley") next
  if (i == "burnhamandhighbridge") next
  if (i == "gedling") next
  if (i == "goldenharvest") next
  if (i == "heirissonisland") next
  if (i == "hortonpark") next
  if (i == "hovepromenade") next
  if (i == "penrose") next
  if (i == "stamfordpark") next
  if (i == "wetherby") next
  if (i == "croxtethhall") next
  if (i == "bracknell") next
  if (i == "portobello") next
  if (i == "longeaton") next
  if (i == "cassiobury") next
  if (i == "maidenhead") next
  if (i == "rushmere") next
  if (i == "hatfieldforest") next  
  
  #subset data by a particular location
  loc.dat = subset(mydata, location == i)
  
  #this will skip all locations that are in the bottom 20% in total runs
  if (nrow(loc.dat) < as.numeric(sort(table(mydata$location))[(length(unique(mydata$location))/5)])) next
  
  #this will get the country of the locaiton (all should be the "UK")
  loc.dat$Country = droplevels(loc.dat$Country)
  country = as.character(unique(loc.dat$Country))
  
  #this will get the number of parkrun events at the current location
  location.runs = length(unique(loc.dat$parkrunid))
  
  #this will get the average number of parkrunners per parkrun event at the current location
  ave.parkrunners.per.event = (nrow(loc.dat) / location.runs)
  
  #this will get the average number of friends for the parkrunners at this location
  ave_friends_per_event = (sum(loc.dat$partner_relation) / nrow(loc.dat))
  
  #this will get the average time of the parkrunners at this location
  ave_time = (sum(loc.dat$time) / nrow(loc.dat))
  
  #performs the necessary transformations
  loc.dat$previous_runs.lg2 = log2(1 + loc.dat$previous_runs)
  loc.dat$partner_relation.lg2 = log2(1 + loc.dat$partner_relation)
  loc.dat$time.lg = log(loc.dat$time)
  
  #runs the model on the subsetted data
  results.part_cont <- lmer(time.lg ~ 1 + gender + age + partners + previous_runs.lg2 + partner_relation.lg2 + (previous_runs.lg2 + partner_relation.lg2 | athnumber), data = loc.dat)
  
  #create an object of the model results
  res.sum.part_cont = summary(results.part_cont)
  
  #this creates a matrix (1 row, 5 columns) that is the estimate, SE, and t-value for each coefficient in every model for each locaton
  intercept.part_cont = res.sum.part_cont$coefficients["(Intercept)", ]
  gender.part_cont = res.sum.part_cont$coefficients["genderW", ]
  age.part_cont = res.sum.part_cont$coefficients["age", ]
  runs.part_cont = res.sum.part_cont$coefficients["previous_runs.lg2", ]
  friends.part_cont = res.sum.part_cont$coefficients["partner_relation.lg2", ]
  partners.part_cont = res.sum.part_cont$coefficients["partners1", ]
  
  #this creates a matrix for the location data
  location = data.frame(i, location.runs, ave.parkrunners.per.event, ave_friends_per_event, ave_time, country)
  
  #this adds the variable matrixes to an empty data set
  dat_int.part_cont = rbind(dat_int.part_cont, intercept.part_cont)
  dat_gen.part_cont = rbind(dat_gen.part_cont, gender.part_cont)
  dat_age.part_cont = rbind(dat_age.part_cont, age.part_cont)
  dat_run.part_cont = rbind(dat_run.part_cont, runs.part_cont)
  dat_bud.part_cont = rbind(dat_bud.part_cont, friends.part_cont)
  dat_part.part_cont = rbind(dat_part.part_cont, partners.part_cont)
  dat_loc = rbind(dat_loc, location)
  
  #this prints the location to track progress
  print(i)
  print(count)
  count = count + 1
  
}

#gives the appropriate column names for the data sets
colnames(dat_loc) = c("location", "total_events", "runners_per_event", "ave_friends_per_event", "ave_time", "country")
colnames(dat_int.part_cont) = c("est","se","df","t-val","p")
colnames(dat_gen.part_cont) = c("est","se","df","t-val","p")
colnames(dat_age.part_cont) = c("est","se","df","t-val","p")
colnames(dat_run.part_cont) = c("est","se","df","t-val","p")
colnames(dat_bud.part_cont) = c("est","se","df","t-val","p")
colnames(dat_part.part_cont) = c("est","se","df","t-val","p") 

#prints total time
end = Sys.time()
end - beginning

################################################################################################################################################

### MERGE ALL DATA SETS INTO ONE ###

#name the variables
dat_int.part_cont$variable = "intercept"
dat_age.part_cont$variable = "age"
dat_gen.part_cont$variable = "gender"
dat_bud.part_cont$variable = "partner_relation"
dat_run.part_cont$variable = "previous_runs"
dat_part.part_cont$variable = "partners"

#add the location data to all dataframes
dat_int.part_cont$location = dat_loc$location
dat_int.part_cont$total_events = dat_loc$total_events
dat_int.part_cont$runners_per_event = dat_loc$runners_per_event
dat_int.part_cont$ave_friends_per_event = dat_loc$ave_friends_per_event
dat_int.part_cont$ave_time = dat_loc$ave_time
dat_int.part_cont$country = dat_loc$country

dat_age.part_cont$location = dat_loc$location
dat_age.part_cont$total_events = dat_loc$total_events
dat_age.part_cont$runners_per_event = dat_loc$runners_per_event
dat_age.part_cont$ave_friends_per_event = dat_loc$ave_friends_per_event
dat_age.part_cont$ave_time = dat_loc$ave_time
dat_age.part_cont$country = dat_loc$country

dat_gen.part_cont$location = dat_loc$location
dat_gen.part_cont$total_events = dat_loc$total_events
dat_gen.part_cont$runners_per_event = dat_loc$runners_per_event
dat_gen.part_cont$ave_friends_per_event = dat_loc$ave_friends_per_event
dat_gen.part_cont$ave_time = dat_loc$ave_time
dat_gen.part_cont$country = dat_loc$country

dat_bud.part_cont$location = dat_loc$location
dat_bud.part_cont$total_events = dat_loc$total_events
dat_bud.part_cont$runners_per_event = dat_loc$runners_per_event
dat_bud.part_cont$ave_friends_per_event = dat_loc$ave_friends_per_event
dat_bud.part_cont$ave_time = dat_loc$ave_time
dat_bud.part_cont$country = dat_loc$country

dat_run.part_cont$location = dat_loc$location
dat_run.part_cont$total_events = dat_loc$total_events
dat_run.part_cont$runners_per_event = dat_loc$runners_per_event
dat_run.part_cont$ave_friends_per_event = dat_loc$ave_friends_per_event
dat_run.part_cont$ave_time = dat_loc$ave_time
dat_run.part_cont$country = dat_loc$country

dat_part.part_cont$location = dat_loc$location
dat_part.part_cont$total_events = dat_loc$total_events
dat_part.part_cont$runners_per_event = dat_loc$runners_per_event
dat_part.part_cont$ave_friends_per_event = dat_loc$ave_friends_per_event
dat_part.part_cont$ave_time = dat_loc$ave_time
dat_part.part_cont$country = dat_loc$country

#merge the data sets into one
model_estimates_by_location = rbind(dat_int.part_cont, dat_age.part_cont, dat_gen.part_cont, dat_part.part_cont, dat_bud.part_cont, dat_run.part_cont)

### ### ### 

#save the data sets
write.csv(model_estimates_by_location, file = "FPRS_model_estimates_by_location.csv", quote = FALSE, row.names = FALSE)
write.csv(dat_bud.part_cont, file = "FPRS_model_FPRS_estimates_by_location.csv", quote = FALSE, row.names = FALSE)

################################################################################################################################################

### GRAPH THE RESULTS ###

library(RColorBrewer)

#create a variable indicating the significance of the effect of the log2 FPRS variable on run times
dat_bud.part_cont$sig <- ifelse(dat_bud.part_cont$p < 5e-2, "p < .05", "p > .05")

#create a variable indicating the total number of runs at a given parkrun
dat_bud.part_cont$total_alltime_runs = dat_bud.part_cont$runners_per_event*dat_bud.part_cont$total_events

qplot(total_alltime_runs, est, data = dat_bud.part_cont, color = sig) + 
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
  theme(legend.key = element_rect(fill = "white")) +
  scale_fill_discrete(name = "New Legend Title") + 
  xlab("Total runs at location") + 
  ylab(expression('Coefficient estimate for log'[2]*' FPRS variable')) + 
  labs(color='Statistical significance\nof coefficient estimate') + 
  theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1))) +
  theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1.1))) +
  theme(legend.title = element_text(family="Arial", face="bold", colour="black", size=rel(0.9))) + 
  theme(legend.text = element_text(family="Arial", colour="black", size=rel(0.8))) +
  theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(0.95))) + 
  theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(0.95))) +
  theme_classic()

### ### ###

### THIS WILL GIVE THE AMOUNT OF LOCATIONS WITH SIGNIFICANT POSITIVE AND NEGATIVE EFFECTS FOR THE FPRS VARIABLE ###

pos_locs.part_cont = subset(dat_bud.part_cont, est > 0)
nrow(pos_locs.part_cont)
neg_locs.part_cont = subset(dat_bud.part_cont, est < 0)
nrow(neg_locs.part_cont)

pos_locs.part_cont = subset(pos_locs.part_cont, p < 5e-2)
nrow(pos_locs.part_cont)
neg_locs.part_cont = subset(neg_locs.part_cont, p < 5e-2)
nrow(neg_locs.part_cont)

#give the percentile rank in terms of parkrunners per event and total events for the locations with signigicant positive effects for the FPRS variable
perc.rank <- function(x) trunc(rank(x))/length(x)
dat_bud.part_cont = within(dat_bud.part_cont, percentile_rank_atr <- perc.rank(total_alltime_runs))
sig_pos_locs = subset(dat_bud.part_cont, est > 0 & p < 5e-2)

mean(sig_pos_locs$percentile_rank_atr)
range(sig_pos_locs$percentile_rank_atr)

mean(sig_pos_locs$runners_per_event)
mean(sig_pos_locs$total_events)
