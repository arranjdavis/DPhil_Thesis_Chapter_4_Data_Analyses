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

#this creates the data frames to fill with the model results from 100 resampled models
modeldat_int = data.frame()
modeldat_gen = data.frame()
modeldat_age = data.frame()
modeldat_par = data.frame()
modeldat_run = data.frame()
modeldat_bud = data.frame()

################################################################################################################################################

### LINEAR MODELS ON RANDOM SAMPLES OF ONE RUN PER PARKRUNNER ###

library(dplyr)

#reorder data by subject using ID (not strictly necessary)
mydata.UK.more_than_10$time.lg.sc = as.numeric(mydata.UK.more_than_10$time.lg.sc) 
ordered_dat <- arrange(mydata.UK.more_than_10, athnumber)
unique_athnumber <- unique(ordered_dat$athnumber)

#make a data frame of the right size so that the for loop is faster; all this data will be overwritten
num_ath <- length(unique_athnumber)
new_dat <- ordered_dat[1:num_ath,]

#this will be used to track the progress of the for loop
count.model = 0

#for loop to pick one line of data (run) per parkrunner
for (model_number in 1:1000){
  
  #this will randomly sample one run (row) from each parkrunner in the dataset
  new.dat = mydata.UK.more_than_10 %>% group_by(athnumber) %>% sample_n(size = 1)
  
  #this model will be run on one (randomly sampled) run from each parkrunner
  results.lm <- lm(time.lg ~ gender + age + partners + previous_runs.lg2 + partner_relation.lg2, data = new.dat)
  
  #print progress
  count.model = count.model + 1
  print(count.model)
  
  #this creates a matrix (1 row, 3 columns) that is the estimate, SE, and t-value for each coefficient in every 'results' model on the sampled data
  intercept = summary(results.lm)$coefficients["(Intercept)", ]
  gender = summary(results.lm)$coefficients["genderW", ]
  partners = summary(results.lm)$coefficients["partners1", ]
  age = summary(results.lm)$coefficients["age", ]
  runs = summary(results.lm)$coefficients["previous_runs.lg2", ]
  friends = summary(results.lm)$coefficients["partner_relation.lg2", ]
  
  #add the matrices to the empty data sets
  modeldat_int = rbind(modeldat_int, intercept)
  modeldat_gen = rbind(modeldat_gen, gender)
  modeldat_age = rbind(modeldat_age, age)
  modeldat_par = rbind(modeldat_par, partners)
  modeldat_run = rbind(modeldat_run, runs)
  modeldat_bud = rbind(modeldat_bud, friends)
  
}

################################################################################################################################################

### COMBINE THE DATA ###

#name columns
colnames(modeldat_int) <- c("est","se","t-val","p")
colnames(modeldat_gen) <- c("est","se","t-val","p")
colnames(modeldat_par) <- c("est","se","t-val","p")
colnames(modeldat_age) <- c("est","se","t-val","p")
colnames(modeldat_run) <- c("est","se","t-val","p")
colnames(modeldat_bud) <- c("est","se","t-val","p")

#create a column in each data frame that names the variable that the estimates describe
modeldat_int[, 'variable'] = 'intercept'
modeldat_gen[, 'variable'] = 'gender'
modeldat_age[, 'variable'] = 'age'
modeldat_par[, 'variable'] = 'partners'
modeldat_run[, 'variable'] = 'previous runs'
modeldat_bud[, 'variable'] = 'FPRS'

#now combine the datasets
lm_one_run_per_parkrunner_data = rbind(modeldat_int, modeldat_gen, modeldat_age, modeldat_par, modeldat_run, modeldat_bud)

### ### ###

### SAVE THE DATA ###

write.csv(lm_one_run_per_parkrunner_data,'lm_one_run_per_parkrunner_data.csv')

################################################################################################################################################

### GRAPH THE RESULTS OF THE LINEAR MODELS ###

#remove the intercept variable from the data set
lm_one_run_per_parkrunner_data_no.int = subset(lm_one_run_per_parkrunner_data, variable != "intercept")

#create a new variable that contains information on the model variable and on the data type
lm_one_run_per_parkrunner_data_no.int$variable = ifelse(lm_one_run_per_parkrunner_data_no.int$variable == "age", "Age", 
                                                        ifelse(lm_one_run_per_parkrunner_data_no.int$variable == "FPRS", "FPRS",
                                                               ifelse(lm_one_run_per_parkrunner_data_no.int$variable == "gender", "Gender",
                                                                      ifelse(lm_one_run_per_parkrunner_data_no.int$variable == "partners", "Partners",
                                                                             ifelse(lm_one_run_per_parkrunner_data_no.int$variable == "previous runs", "Previous runs", NA)))))

#give colors for the graph (this will just generate colors from the sequential pallates of nine colors)
green = rgb(0,(153/255),0,.66)
green.t = rgb(0,(153/255),0,.33)

red = rgb((255/255), 0, 0,.66)
red.t = rgb((255/255), 0, 0,.33)

purple = rgb((102/255), 0, (204/255),.66)
purple.t = rgb((102/255), 0, (204/255),.33)

blue = rgb(0, 0, (255/255), .8)
blue.t = rgb(0, 0, (255/255), .1)

grey = rgb((96/255), (96/255), (96/255),.66)
grey.t = rgb( (96/255), (96/255), (96/255),.33)

#make a graph with the distributions of the coefficients from the 1000 linear models
ggplot(lm_one_run_per_parkrunner_data_no.int, aes(x = t.val, fill = variable)) +
  geom_histogram(position = "identity", binwidth = .01) +
    scale_fill_manual(values = c(green, red, purple, grey, blue)) +
      theme(panel.grid.minor = element_blank()) + 
        theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + 
          labs(x = "Coefficient estimate t-value", y = "Count", fill = "Variable") +
            theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1))) +
              theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1.1))) +
                theme(legend.title = element_text(family="Arial", face="bold", colour="black", size=rel(0.9))) + 
                  theme(legend.text = element_text(family="Arial", colour="black", size=rel(0.8))) +
                    theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(0.95))) + 
                      theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(0.95))) + 
                        #set this to 'size = 0.080' for thin lines
                          theme(panel.grid.major = element_line(colour = 'grey', size = 0.000)) +
                            theme(panel.grid.minor = element_line(colour = 'grey', size = 0.00)) +
                              theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
                                scale_y_continuous(limits = c(0, 15), expand = c(0,0)) +
                                  theme_classic()
