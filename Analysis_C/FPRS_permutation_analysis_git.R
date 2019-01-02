#clean environment
rm(list = ls())

#working directory
setwd("/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1/")

library(lubridate)
library(lme4)

print(Sys.time())

#create timer for entire script
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

#this creates the data frames to fill with the model results from the permuted data
permdat_int = data.frame()
permdat_gen = data.frame()
permdat_age = data.frame()
permdat_par = data.frame()
permdat_run = data.frame()
permdat_bud = data.frame()
permdat_con = data.frame()

#this creates the data frames to fill with the model results from the real data
trdat_int = data.frame()
trdat_gen = data.frame()
trdat_age = data.frame()
trdat_par = data.frame()
trdat_run = data.frame()
trdat_bud = data.frame()
trdat_con = data.frame()

#creates an empty column in mydata to be filled by the permuted times data
mydata.UK.more_than_10[, "time.lg.sc.perm"] = NA

################################################################################################################################################

### PERMUTATION ANALYSIS ###

#get 10% of all parkrunners
athun = unique(mydata.UK.more_than_10$athnumber)
ten_percent = as.integer(length(athun) / 10)

#1000 permutations
for (perm_number in 1:1000) {

  #empites and removes old data set used for last parkrunner
  mydata_temp = NA
  rm(mydata_temp)
  
  #takes a random sample of 10% of all parkrunners
  athun_temp = droplevels(sample(athun, size = ten_percent, replace = FALSE))

  #subsets the data set to only those parkrunners in the sample
  mydata_temp = droplevels(mydata.UK.more_than_10[mydata.UK.more_than_10$athnumber %in% athun_temp,,drop=FALSE])
  
  #prints progress
  print(perm_number)
  print(Sys.time())
  ath_counter = 1
  
  #interates through all parkrunners in sample
  for(athc in athun_temp){  
    
    #prints progress
    ath_pc = (ath_counter/length(athun_temp))*100
    cat('\r',ath_pc)
    flush.console() 
    ath_counter = ath_counter + 1
     
    #creates an object with all the times for athc
    times.curr.ath = mydata_temp[mydata_temp$athnumber == athc, "time.lg.sc"]
    
    #permutes current parkrunner's run times; sample() just reorders times.curr.ath
    times.curr.ath.perm = sample(times.curr.ath)
    
    #fills the NA ("times.lg.perm") column with the athc's randomized times; the athc rows in mydata_temp are populated with the randomized times;
    #after the first loop it will just be the first athc's rows filled with randomized times, the rest NA
    #this will continue until the "times.lg.perm" column is filled with every parkrunner's permuted times
    mydata_temp[mydata_temp$athnumber == athc, "time.lg.sc.perm"] = times.curr.ath.perm
    
  }
  
  #once the permutation is complete, and the data set is full, estimate the model on the permuted data
  
  #runs a model after every new permutation (this model converges and will thus be used since I am only interested in the t-values from the models)
  results = lmer(time.lg.sc.perm ~ 1 + gender + age + partners + previous_runs.lg.sc + partner_relation.lg.sc + (previous_runs.lg.sc + partner_relation.lg.sc | athnumber), data = mydata_temp)
  
  #this creates a matrix (1 row, 3 columns) that is the estimate, SE, and t-value for each coefficient in every 'results' model on the permuted data
  intercept = summary(results)$coefficients["(Intercept)", ]
  gender = summary(results)$coefficients["genderW", ]
  age = summary(results)$coefficients["age", ]
  partners = summary(results)$coefficients["partners1", ]
  runs = summary(results)$coefficients["previous_runs.lg.sc", ]
  friends = summary(results)$coefficients["partner_relation.lg.sc", ]
  
  #record whether or not the model failed to converge
  warnings = results@optinfo$conv$lme4
  
  failure_to_converge = ''
  
  if (length(warnings) > 0){
    
    for (i in warnings$messages){
      
      if (grepl('failed to converge', i) == TRUE){
        
        failure_to_converge = 'yes'
        
        #if this happens, break the for loop
        break
        
      }
    }
  }
  
  #add whether or not there was a failure to converge to the data
  
  convergence = ''
  
  if (failure_to_converge == 'yes'){
    
    convergence = 0
    
    } else{
      
      convergence = 1
      
    }
  
  
  #this adds the variable matrixes to an empty data set; by the end of the permutation loop the dataset should have n = permutations (1000) rows
  permdat_int = rbind(permdat_int, intercept)
  permdat_gen = rbind(permdat_gen, gender)
  permdat_age = rbind(permdat_age, age)
  permdat_par = rbind(permdat_par, partners)
  permdat_run = rbind(permdat_run, runs)
  permdat_bud = rbind(permdat_bud, friends)
  permdat_con = rbind(permdat_con, convergence)
  
}

#name columns
colnames(permdat_int) = c("est","se","t-val")
colnames(permdat_gen) = c("est","se","t-val")
colnames(permdat_age) = c("est","se","t-val")
colnames(permdat_par) = c("est","se","t-val")
colnames(permdat_run) = c("est","se","t-val")
colnames(permdat_bud) = c("est","se","t-val")
colnames(permdat_con) = c("convergence")

#rename variables in 'permdat_con' to 'success' and 'failure'
permdat_con$convergence = ifelse(permdat_con$convergence == "1", "success", ifelse(permdat_con$convergence == "0", "failure", NA))

################################################################################################################################################

### BUILD DISTRIBUTION OF RESULTS FROM 1000 MODELS ON RANDOM SAMPLES OF UNPERMUTED DATA ###

#1000 samples
for (sample_number in 1:1000) {

  #empites and removes old data set used for last sample
  mydata_true_temp = NA
  rm(mydata_true_temp)
  
  #takes a random sample of 10% of all parkrunners
  athun_true_temp = sample(athun, size = ten_percent, replace = FALSE)

  #subsets the data set to only those parkrunners in the 1000 parkrunner sample of from in athun_temp
  mydata_true_temp = droplevels(mydata.UK.more_than_10[mydata.UK.more_than_10$athnumber %in% athun_true_temp,,drop=FALSE])
  
  #prints progress
  print(sample_number)
  print(Sys.time())
  ath_counter = 1
  
  #runs a model after every new sample of 1000 parkrunners
  results.true = lmer(time.lg.sc ~ 1 + gender + age + partners + previous_runs.lg.sc + partner_relation.lg.sc + (previous_runs.lg.sc + partner_relation.lg.sc | athnumber), data = mydata_true_temp)
  
  #this creates a matrix (1 row, 3 columns) that is the estimate, SE, and T-Value for each coefficient in every model on the true, sampled data
  intercept.true = summary(results.true)$coefficients["(Intercept)", ]
  gender.true = summary(results.true)$coefficients["genderW", ]
  partners.true = summary(results.true)$coefficients["partners1", ]
  age.true = summary(results.true)$coefficients["age", ]
  runs.true = summary(results.true)$coefficients["previous_runs.lg.sc", ]
  friends.true = summary(results.true)$coefficients["partner_relation.lg.sc", ]
  
  #record whether or not the model failed to converge
  warnings = results.true@optinfo$conv$lme4
  
  failure_to_converge = ''
  
  if (length(warnings) > 0){
    
    for (i in warnings$messages){
      
      if (grepl('failed to converge', i) == TRUE){
        
        failure_to_converge = 'yes'
      }
    }
  }
  
  #add whether or not there was a failure to converge to the data
  
  convergence = ''
  
  if (failure_to_converge == 'yes'){
    
    convergence = 0
    
  } else{
    
    convergence = 1
    
  }
  
  #this adds the variables matrixes to an empty data set; by the end of the permutation loop the dataset should have n = samples (1000) rows
  trdat_int = rbind(trdat_int, intercept.true)
  trdat_gen = rbind(trdat_gen, gender.true)
  trdat_age = rbind(trdat_age, age.true)
  trdat_par = rbind(trdat_par, partners.true)
  trdat_run = rbind(trdat_run, runs.true)
  trdat_bud = rbind(trdat_bud, friends.true)
  trdat_con = rbind(trdat_con, convergence)
}

#rename columns
colnames(trdat_int) = c("est","se","t-val")
colnames(trdat_gen) = c("est","se","t-val")
colnames(trdat_age) = c("est","se","t-val")
colnames(trdat_par) = c("est","se","t-val")
colnames(trdat_run) = c("est","se","t-val")
colnames(trdat_bud) = c("est","se","t-val")
colnames(trdat_con) = c("convergence")

#rename variables in 'trdat_con' to 'success' and 'failure'
trdat_con$convergence = ifelse(trdat_con$convergence == "1", "success", ifelse(trdat_con$convergence == "0", "failure", NA))

################################################################################################################################################

#prints total time
end = Sys.time()
end - beginning

################################################################################################################################################

### COMBINE THE DATA ###

#combine the permuted data

#first create a column in each data frame that names the variable that the estimates describe
permdat_int[, 'variable'] = 'intercept'
permdat_gen[, 'variable'] = 'gender'
permdat_age[, 'variable'] = 'age'
permdat_par[, 'variable'] = 'partners'
permdat_run[, 'variable'] = 'previous runs'
permdat_bud[, 'variable'] = 'FPRS'

#second, create a column with the permutation number for the model from which the estimates were derived
permdat_int[, 'model_number'] = 1:nrow(permdat_int)
permdat_gen[, 'model_number'] = 1:nrow(permdat_gen)
permdat_age[, 'model_number'] = 1:nrow(permdat_age)
permdat_par[, 'model_number'] = 1:nrow(permdat_par)
permdat_run[, 'model_number'] = 1:nrow(permdat_run)
permdat_bud[, 'model_number'] = 1:nrow(permdat_bud)

#third, add the convergence information to each dataset
permdat_int[, 'convergence'] = permdat_con$convergence
permdat_gen[, 'convergence'] = permdat_con$convergence
permdat_age[, 'convergence'] = permdat_con$convergence
permdat_par[, 'convergence'] = permdat_con$convergence
permdat_run[, 'convergence'] = permdat_con$convergence
permdat_bud[, 'convergence'] = permdat_con$convergence

#now combine the datasets
permuted_data = rbind(permdat_int, permdat_gen, permdat_age, permdat_par, permdat_run, permdat_bud)

#add a column to indicatge that this is the permuted data
permuted_data[, 'data_type'] = "permuted"

### ### ### 

#combine the true data

#first create a column in each data frame that names the variable that the estimates describe
trdat_int[, 'variable'] = 'intercept'
trdat_gen[, 'variable'] = 'gender'
trdat_age[, 'variable'] = 'age'
trdat_par[, 'variable'] = 'partners'
trdat_run[, 'variable'] = 'previous runs'
trdat_bud[, 'variable'] = 'FPRS'

#second, create a column with the interation number for the model from which the estimates were derived
trdat_int[, 'model_number'] = 1:nrow(trdat_int)
trdat_gen[, 'model_number'] = 1:nrow(trdat_gen)
trdat_age[, 'model_number'] = 1:nrow(trdat_age)
trdat_par[, 'model_number'] = 1:nrow(trdat_par)
trdat_run[, 'model_number'] = 1:nrow(trdat_run)
trdat_bud[, 'model_number'] = 1:nrow(trdat_bud)

#third, add the convergence information to each dataset
trdat_int[, 'convergence'] = trdat_con$convergence
trdat_gen[, 'convergence'] = trdat_con$convergence
trdat_age[, 'convergence'] = trdat_con$convergence
trdat_par[, 'convergence'] = trdat_con$convergence
trdat_run[, 'convergence'] = trdat_con$convergence
trdat_bud[, 'convergence'] = trdat_con$convergence

#now combine the datasets
true_data = rbind(trdat_int, trdat_gen, trdat_age, trdat_par, trdat_run, trdat_bud)

#add a column to indicatge that this is the true data
true_data[, 'data_type'] = "true"

### ### ###

#now combine both data sets
full_permutation_analysis_data = rbind(permuted_data, true_data)

################################################################################################################################################

### SAVE THE PERMUTATION DATA ###
write.csv(full_permutation_analysis_data,'full_permutation_analysis_data.csv')

################################################################################################################################################

### ANALYSIS OF PERMUTATION DATA ###

#give the percentage of models that converged for the permuted data
permuted_data = subset(full_permutation_analysis_data, data_type == "permuted")
table(permuted_data$convergence)[2] / (sum(table(permuted_data$convergence)))

#compare the distributions of model estimates from the models that converged to those that did not

#get a dataset of just the FPRS variable
FPRS_perm_dat = subset(permuted_data, variable == "FPRS")

#make the graph

library(ggplot2)
library(viridis)
library(RColorBrewer)

#compare the distributions of estimates from the models that coverged and failed to converge
ggplot(FPRS_perm_dat, aes(x = t.val, fill = convergence)) + 
  geom_histogram(binwidth = .1, alpha=0.2, position="identity") + 
    xlab("FPRS coefficient estimate t-value") +
      ylab("Frequency") + 
        labs(fill = "Model convergence") + 
          scale_fill_discrete(labels=c("Failure", "Success")) +
            theme(axis.title.x = element_text(family="Arial", face="bold", colour="black", size=rel(1))) +
              theme(axis.title.y = element_text(family="Arial", face="bold", colour="black", size=rel(1.1))) +
                theme(legend.title = element_text(family="Arial", face="bold", colour="black", size=rel(0.9))) + 
                  theme(legend.text = element_text(family="Arial", colour="black", size=rel(0.8))) +
                    theme(axis.text.x = element_text(family="Arial", colour="black", size=rel(0.95))) + 
                      theme(axis.text.y = element_text(family="Arial", colour="black", size=rel(0.95))) + 
                        theme(panel.grid.major = element_line(colour = 'grey', size = 0.08)) +
                          theme(panel.grid.minor = element_line(colour = 'grey', size = 0.02)) +
                            theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
                              theme(axis.text.y = element_blank())

#t-test on the differences between these distributions
FPRS_perm_dat_success = subset(FPRS_perm_dat, convergence == "success")
FPRS_perm_dat_failure = subset(FPRS_perm_dat, convergence == "failure")
t.test(FPRS_perm_dat_failure$t.val, FPRS_perm_dat_success$t.val)

### #### ###

### GRAPHS AND T-TESTS TO COMPARE DISTRIBUTIONS FROM PERMUTED V. TRUE SAMPLES ###

#graph the distributions of the estimates for all the predictor variables for both the permuted and true data

#remove the intercept data
permutation_data_minus_intercept = subset(full_permutation_analysis_data, variable != "intercept")

#create a new variable that contains information on the model variable and on the data type
permutation_data_minus_intercept$var_and_dat = ifelse(permutation_data_minus_intercept$variable == "age" & permutation_data_minus_intercept$data_type == "true", "Age (true)", 
                                                      ifelse(permutation_data_minus_intercept$variable == "age" & permutation_data_minus_intercept$data_type == "permuted", "Age (permuted)",
                                                             ifelse(permutation_data_minus_intercept$variable == "FPRS" & permutation_data_minus_intercept$data_type == "true", "FPRS (true)",
                                                                    ifelse(permutation_data_minus_intercept$variable == "FPRS" & permutation_data_minus_intercept$data_type == "permuted", "FPRS (permuted)",
                                                                           ifelse(permutation_data_minus_intercept$variable == "gender" & permutation_data_minus_intercept$data_type == "true", "Gender (true)",
                                                                                  ifelse(permutation_data_minus_intercept$variable == "gender" & permutation_data_minus_intercept$data_type == "permuted", "Gender (permuted)",
                                                                                         ifelse(permutation_data_minus_intercept$variable == "partners" & permutation_data_minus_intercept$data_type == "true", "Partner presence (true)",
                                                                                                ifelse(permutation_data_minus_intercept$variable == "partners" & permutation_data_minus_intercept$data_type == "permuted", "Partner presence (permuted)",
                                                                                                       ifelse(permutation_data_minus_intercept$variable == "previous runs" & permutation_data_minus_intercept$data_type == "true", "Previous runs (true)",
                                                                                                              ifelse(permutation_data_minus_intercept$variable == "previous runs" & permutation_data_minus_intercept$data_type == "permuted", "Previous runs (permuted)", NA))))))))))

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

#make a graph with the distributions of the permuted and true data
ggplot(permutation_data_minus_intercept, aes(x = t.val, fill = var_and_dat)) +
  geom_histogram(position = "identity", binwidth = .25) +
    scale_fill_manual(values = c(green.t, green, red.t, red, purple.t, purple, grey.t, grey, blue.t, blue)) +
      theme(panel.grid.minor = element_blank()) + 
        theme(panel.background = element_rect(fill = 'white', colour = 'grey')) + 
          labs(x = "Coefficient estimate t-value", y = "Count", fill = "Variable (data type)") +
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
                                  scale_y_continuous(limits = c(0, 125), expand = c(0,0)) +
                                    theme_classic()

### ### ###

### T-TESTS ###

age_perm_dat = subset(permutation_data_minus_intercept, var_and_dat == "Age (permuted)")
age_true_dat = subset(permutation_data_minus_intercept, var_and_dat == "Age (true)")

FPRS_perm_dat = subset(permutation_data_minus_intercept, var_and_dat == "FPRS (permuted)")
FPRS_true_dat = subset(permutation_data_minus_intercept, var_and_dat == "FPRS (true)")

gender_perm_dat = subset(permutation_data_minus_intercept, var_and_dat == "Gender (permuted)")
gender_true_dat = subset(permutation_data_minus_intercept, var_and_dat == "Gender (true)")

partner_perm_dat = subset(permutation_data_minus_intercept, var_and_dat == "Partner presence (permuted)")
partner_true_dat = subset(permutation_data_minus_intercept, var_and_dat == "Partner presence (true)")

prev.runs_perm_dat = subset(permutation_data_minus_intercept, var_and_dat == "Previous runs (permuted)")
prev.runs_true_dat = subset(permutation_data_minus_intercept, var_and_dat == "Previous runs (true)")

t.test(age_perm_dat$t.val, age_true_dat$t.val)
t.test(FPRS_perm_dat$t.val, FPRS_true_dat$t.val) 
t.test(gender_perm_dat$t.val, gender_true_dat$t.val)
t.test(partner_perm_dat$t.val, partner_true_dat$t.val)
t.test(prev.runs_perm_dat$t.val, prev.runs_true_dat$t.val)
