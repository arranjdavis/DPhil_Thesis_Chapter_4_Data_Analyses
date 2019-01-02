#set current working directory
setwd("/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1")

#clean environment
rm(list = ls())

mydata <- read.csv("partners_output.csv")

library(lubridate)
library(lme4)
library(lmerTest)

################################################################################################################################################

### DESCRIPTIVE STATISTICS ###

#get total number of athletes
total.uniq <- unique(mydata$athnumber)
length(total.uniq)

#get the average number of runs per parkrunner
mean(table(mydata$athnumber))
range(table(mydata$athnumber))

#get total number of athletes who are the faster partner
fast.runners <- mydata$athnumber[mydata$faster_or_slower == 'f']
fast.uniq <- unique(fast.runners)
length(unique(fast.runners))

#get total number of athletes who are the slower partner
slow.runners <- mydata$athnumber[mydata$faster_or_slower == 's']
slow.uniq <- unique(slow.runners)
length(unique(slow.runners))

################################################################################################################################################

### VARIABLE TRANSFORMATION AND EXCLUSION OF CASES ###

#make the age variable an integer
#mydata$age.int = as.integer(mydata$age)

#convert time to integer
mydata$time = period_to_seconds(hms(mydata$time))
mydata$time = mydata$time / 60
mydata$time.lg = log(mydata$time)

#exclude the parkrunners aged less than 18 or in a wheelchair
mydata = subset(mydata, age != "C" & age != "10" & age != "11-14" & age != "15-17" & age != "---")
mydata$age = droplevels(mydata$age)

#get a list of unique parkrunners
unique.runners = unique(mydata$athnumber)

### SUBSET DATA SET TO INCLUDE PARKRUNNERS WHO WERE SLOWER RUNNING PARTNERS AND NEVER FASTER RUNNING PARTNERS ###

slow_but_never_fast = list()

for (i in unique.runners) {

  runs = subset(mydata, athnumber == i)

  faster_or_slower = table(runs$faster_or_slower)

  if (faster_or_slower[2] == 0 && faster_or_slower[3] > 0) {

    slow_but_never_fast = append(i, slow_but_never_fast)

  }
}

length(slow_but_never_fast)

#get a subset of the data that is just parkrunners who are slower partners (and who have never been a faster partner)
slow_only = subset(mydata, athnumber %in% slow_but_never_fast)

#drops any athletes with less than 10 runs
slow_only$athnumber = as.factor(slow_only$athnumber)
tbl = table(slow_only$athnumber) 
slow_only.more_than_10 = droplevels(slow_only[slow_only$athnumber %in% names(tbl)[tbl >= 10],,drop=TRUE])

#drop unused parkrunners and locations
slow_only.more_than_10$athnumber = droplevels(slow_only.more_than_10$athnumber) 
slow_only.more_than_10$location = droplevels(slow_only.more_than_10$location) 

#descriptive statistics of data set to be used in analyses
mean(table(slow_only.more_than_10$athnumber))
sd(table(slow_only.more_than_10$athnumber))
range(table(slow_only.more_than_10$athnumber))

#percentage partner runs
table(slow_only.more_than_10$partner)[2] / sum(table(slow_only.more_than_10$partner))

################################################################################################################################################

### T-TEST ON OVERALL PARTNER EFFECT ###

#create time variable that is in minutes 
mydata$time = period_to_seconds(hms(mydata$time))
mydata$time = mydata$time / 60

#subset the data sets
partner_runs = subset(mydata, partner == 1)
mean(partner_runs$time)

solo_runs = subset(mydata, partner == 0)
mean(solo_runs$time)

#this holds for just the slower runners as well

#partner runs
slow.runners.subset.partner.runs = subset(slow.runners.subset, partner == 1)
slow.runners.subset.partner.runs$time = period_to_seconds(hms(slow.runners.subset.partner.runs$time))
slow.runners.subset.partner.runs$time = slow.runners.subset.partner.runs$time / 60
mean(slow.runners.subset.partner.runs$time)

#solo runs
slow.runners.subset.solo.runs = subset(slow.runners.subset, partner == 0)
slow.runners.subset.solo.runs$time = period_to_seconds(hms(slow.runners.subset.solo.runs$time))
slow.runners.subset.solo.runs$time = slow.runners.subset.solo.runs$time / 60
mean(slow.runners.subset.solo.runs$time)

#t-test
t.test(slow.runners.subset.partner.runs$time, slow.runners.subset.solo.runs$time)

#mean differences
mean(slow.runners.subset.partner.runs$time) - mean(slow.runners.subset.solo.runs$time)

################################################################################################################################################

### MULTILEVEL MODEL ###

#get the overall average run time 
mean_time = mean(slow_only.more_than_10$time)
mean_time_seconds = mean_time * 60

#this is the full model, controlling for previous runs, logged to facilitate model conversion
slow_only.more_than_10$current_prev_runs.lg = log(slow_only.more_than_10$current_prev_runs)

mlm = lmer(time.lg ~ 1 + partner + current_prev_runs.lg + (1 + partner | athnumber), data = slow_only.more_than_10)
summary(mlm)

#this translates the effect into time instead of logged time
partner_effect = summary(mlm)$coefficients["partner", ]

#this give the percentage decrease in run times as a result of having a finishing partner
#https://www.statalist.org/forums/forum/general-stata-discussion/general/1362222-interpretation-of-coefficients-for-log-transformed-dependent-variable-panel-data
100 - exp(partner_effect[1]) * 100

#this would be the change (in seconds) of running with a partner v. running alone
mean_time_seconds - (mean_time_seconds * exp(partner_effect[1]))

################################################################################################################################################

### MULTILEVEL REGRESSION - ASSUMPTION CHECKS ###

### ASSUMPTION CHECKS (FROM SNIJDERS & BOSKER, 2012) ###

###################################################################
###  ch10.r                                                     ###
###                                                             ###
###  This is an R script for producing examples in              ###
###  Chapter 10 of                                              ###
###  Snijders, Tom A.B., and Bosker, Roel J.                    ###
###  Multilevel Analysis:                                       ###
###  An Introduction to Basic and Advanced Multilevel Modeling, ###
###  second edition.                                            ###
###  London etc.: Sage Publishers, 2012                         ###
###                                                             ###
###  version April 4, 2014                                      ###
###                                                             ###
###################################################################

setwd("/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1")

library(lubridate)
library(lme4)
library(lmerTest)
library(RColorBrewer)

mydata = slow.runners.subset.exclusions

### ### ###

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

#This calculates OLS regressions per parkrunner. To be able to use the compareFits function, this keeps the parkrunner-level variables

#this will estimate seperate linear models for every level of the grouping variable

#to create the above model, first need to subset the data - take %25 of all parkrunners 
sample_size = length(unique(mydata$athnumber)) / 4

parkrunners.sample = sample(unique(mydata$athnumber), sample_size)

mydata.sub = subset(mydata, athnumber %in% parkrunners.sample)

mydata.sub$athnumber = droplevels(mydata.sub$athnumber)

#exclude any parkrunners with less than 15 runs (Snijders & Bosker, 2012; p. 159)
tbl = table(mydata.sub$athnumber) 
mydata.sub = droplevels(mydata.sub[mydata.sub$athnumber %in% names(tbl)[tbl >= 15],,drop=TRUE])

#drop unused parkrunners and locations
mydata.sub$athnumber = droplevels(mydata.sub$athnumber) 

#number of unique parkrunners in 'mydata.sub'
length(unique(mydata.sub$athnumber))

#this will run a liner model at every level of the grouping factor that is present in the data set (so, a linear model will be run on ever parkrunners' data in that is in the data set)

#this renames the variables (for plot title purposes)
mydata.sub$Partner = mydata.sub$partner
mydata.sub$Runs = mydata.sub$current_prev_runs.lg

#this keeps the parkrunner-level variables (even though they make no sense and will generate NA's since they don't vary, so that the compareFits function can be used)
olselist <- lmList(time.lg ~ Partner + Runs| athnumber, data = mydata.sub) #DO NOT LOAD NLME WHEN ESTIMATING THIS

#this runs the main model on the same subset of parkrunners
#summary(mod1 <- lme(time.lg ~ gender + age + partners + previous_runs.lg + partner_relation.lg, random = ~ previous_runs.lg + partner_relation.lg | athnumber, data = mydata.sub))
library(nlme)
summary(mod1 <- lme(time.lg ~ Partner + Runs, random = ~ Partner | athnumber, data = mydata.sub))

#The compareFits function compares the OLS estimates with the posterior means of the multilevel model. This is done only for the level-1 effects.
#The graph produced below will compare the variance of the OLS regression estimates from each parkrunner to the posterior means calculated in the multilevel model

comp.models <- compareFits(coef(olselist), coef(mod1))
plot(comp.models, ylab = "")

# The plots indicate that variabiliry for the OLS estimates is indeed larger for the 'Partner' variable

#this will get the within-group residual d.f.
#first see how olselist is structured:
str(olselist, 1)

#it is a list of results for each of the parkrunners.
str(olselist[[1]], 1)

#the within-group residual d.f. can be obtained as follows (this is number of runs minus the number of parameters plus 1):

df_res <- sapply(olselist, function(x){x$df.residual})
table(df_res)

#to study the outliers for the effect of the 'Partner' variable the plot of comp.models shows that the value .2 could be used as to seperate the outliers from the rest
strong.partner_relation.effect <- which(abs(comp.models[,1,"Partner"]) > .2)

#Are these particularly small groups? The average number of runs per parkrunner is:
mean_runs = mean(table(mydata.sub$athnumber))

strong.Partner.table = df_res[strong.partner_relation.effect] #smaller than average, but larger groups are also included
mean(strong.Partner.table)

#this will give the percentage of these runners who have a total number of runs larger than the average
counter = 0

for (i in strong.Partner.table) {
  
  if (mean_runs < i)
    
    counter = counter + 1
  
}

#percentage of these parkrunners who have fewer than the average amount of runs
1 - (counter / length(strong.Partner.table))

#the relatively small size of these groups may well account for their deviating observed effects of the FPRS variable; 
#these outliers are thus not very bothering (Snijders & Bosker, 2012)

### ### ###

### NULL IS HOMOSCEDASTICITY, AND IT HAS A CHI-SQUARE DISTRIBUTION UNDER THE NULL ###

# Example 10.1
#make a selection of the parkrunners with d.f. at least 20
use <- df_res >= 20

# The number of such parkrunners is
sum(use)

#now get the within-group residual standard deviations
summary(olselist[[1]])$sigma

#make a plot of residual variance against residual d.f.:
sigma2_res <- sapply(olselist, function(x){(summary(x)$sigma)^2})
plot(df_res, sigma2_res) 

# There are some outliers for the very low residual d.f.

# Formula (10.3); "Used to detect heteroscedasticity in the form of between-group differences in level-one residual variance" (Snijders & Bosker, 2012; p. 159).

#test heteroscedasticity
ls_tot <- sum((df_res*log(sigma2_res))[use])/sum(df_res[use])
d <- (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
(H <- sum((d^2)[use]))

# The associated p-value is
1-pchisq(H, sum(use)-1)

#this will plot d, which is Gausian if there is level-one homoscedasticity
qqnorm(d[use])
qqline(d[use])

#graph the level-one residuals, if there is a heavy tail then this can bias the test for heteroskedasticity
res = resid(mlm)
qqnorm(resid(mlm))
qqline(resid(mlm))

### ### ###

# Example 10.2
#the goal here is to inspect level-one residuals, step one is to plot the unstandardised OLS residuals against level-one explanatory variables to check for non-linearity
#step two is to make a normal probability plot of the standardised OLS residuals to check the assumption of a normal distribution

### PLOT UNSTANDARDISED RESIDUALS AGAINST LEVEL-ONE EXPLANATORY VARIABLES TO CHECK FOR NON-LINEARITY ###

library(RColorBrewer)

# The within-group OLS residuals are obtained by
resi <- residuals(olselist)

#for the logged previous runs variable
cols = brewer.pal(n=4,name="Set1")

with(mydata.sub,plot(current_prev_runs, resi, xlab = expression('Previous runs'), ylab = "Residual"))
with(mydata.sub,lines(lowess(current_prev_runs, resi), col=cols[2])) #the lowess line indicates linearity

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(time.lg ~ Partner + Runs,
              data=x))
}

#compute within-parkrunner studentized OLS residuals
resi_st <- by(mydata.sub, mydata.sub$athnumber, res_st5)
rs <- unlist(resi_st)

#some of the residuals are NA or exactly 0; this is because of the presence of small groups (Snijders & Bosker, 2012)
sum(is.na(rs))
sum(rs[!is.na(rs)]==0)

#make a QQ plot with use the <use> vector defined above (to only include the parkrunners that have enough runs)
qqnorm(rs[use], main = "")
qqline(rs[use])

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

### LEVEL-TWO RESIDUAL NORMALITY AND HETEROSKEDASTICITY ###

# Example 10.3
#the goal here is to inspect level-two residuals, step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables

#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution

#step three the squared standardised level-two residuals may be plotted as a function of level-two variables to check homoscedasticity

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model run above)

### LEVEL-TWO RESIDUAL NORMALITY AND HETEROSKEDASTICITY ###

# Example 10.3
#the goal here is to inspect level-two residuals, step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step one will be skipped, since there are no relevant level-two predictor variables

#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution

#step three the squared standardised level-two residuals may be plotted as a function of level-two variables to check homoscedasticity
#step three will be skipped, since there are no relevant level-two predictor variables

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model run above)

#get the random effects for this model
re.mod103 <- ranef(mlm, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each parkrunner):
postmean  <- re.mod103$athnumber[,1] #first column is the intercept
postmean  <- re.mod103$athnumber$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.partner <- re.mod103$athnumber$partner

#this will get the posterior variances:
#for example, attr(re.mod103$athnumber,'postVar')[,,1103] will give the posterior variabnces for the 1103rd parkrunner
#the first row/column is the intercept, the second row/column is for 'previous_runs.lg,' and the third is for 'partner_relation.lg'
#so, attr(re.mod103$athnumber,'postVar')[2,2,1103] gives the posterior variance for the 'previous_runs.lg'for the 1103rd parkrunner
postmeanvar <-  attr(re.mod103$athnumber,'postVar')[1,1,]
postslopevar.partner <-  attr(re.mod103$athnumber,'postVar')[2,2,] 

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is inlfuenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(mlm)

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(mlm)$athnumber[1,1] - postmeanvar
diagslopevar.partner <- VarCorr(mlm)$athnumber[2,2] - postslopevar.partner

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#makee a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the 'partner' variables:

#for the intercepts
postmean.stand <- postmean/diagmeanvar
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'partner' variable
postmean.stand <- postslope.partner/diagslopevar.partner
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#get mean of slopes and intercepts
mean(diagmeanvar)
mean(diagslopevar.partner)

t.test(diagslopevar.partner)

### HOMOSCEDASTICITY ###

#we need to get the average for the predictor variable for each participant
participant.which <- match(unique(slow.runners.subset.exclusions$athnumber), slow.runners.subset.exclusions$athnumber)

#get the average for 'partner' for each participant
library(plyr)

j <- ddply(slow.runners.subset.exclusions, .(athnumber), summarise, mean_y = mean(partner))
j$partner_ave <- j$mean_y

#plots from Snijders & Bosker (2012; pp. 166-167)
#figure 10.4

#for posterior intercept
plot(j$partner_ave, postmean, xlab = "Parkrunner average for finish presence (not present = 0, present = 1)", ylab = "Posterior intercepts")
lines(lowess(j$partner_ave, postmean))

#for posterior slope
plot(j$partner_ave, postslope.partner, xlab = "Parkrunner average for finish partner presence (not present = 0, present = 1)", ylab = "Posterior slopes")
lines(lowess(j$partner_ave, postslope.partner))

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

#INFLUENTIAL DATA POINTS (from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)
library(influence.ME)

#this will estimate the model over and over again, excluding each parkrunner and seeing how the model changes, this would take ~20,000 * 2 hours on the full model, so it was 
#done for a model run on subset of 25% of parkrunners

#subset the data - take 25% of all parkrunners 
sample_size = length(unique(mydata$athnumber)) / 4

parkrunners.sample = sample(unique(mydata$athnumber), sample_size)

mydata.sub = subset(mydata, athnumber %in% parkrunners.sample)

mydata.sub$athnumber = droplevels(mydata.sub$athnumber)

#this model is run on the subset of data, which is made up of data from  
(mod_sub <- lmer(time.lg ~ partner + current_prev_runs.lg + (partner | athnumber), data = mydata.sub))
alt.est <- influence(mod_sub, group= "athnumber")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

#percent change in parameter estimates when including and not including each level-two unit (i.e., participant)
pchange(alt.est)

influencers = cooks.distance.estex(alt.est)
p = length(influencers)

#this will give the percentage of participants with potentially problematic Cook's distances
count4 = 0
for (i in influencers) {
  #threshold for potentially problematic Cook's distances; either 4 / n where n in the number of level-two units (Nieuwenhuis et al., 2012) or 1 (Field et al., 2014)
  if (i > 1){
    print(i)
    count4 = count4 + 1
  }
}

#percentage of participants above threshold for potentially problematic Cook's distances; either 4 / n where n in the number of level-two units (Nieuwenhuis et al., 2012) or 1 (Field et al., 2014)
print(count4 / p)

#the sigtest function tests whether excluding a particular level-two unit changes the statistical significance (at 1.96) of a model’s predictor variables (Nieuwenhuis et al. 2012)
sig.test.dataframe = as.data.frame(sigtest(alt.est))

#partner variable

#this give the range of t-values produced by interatively excluding the sampled parkrunners one at a time
range(sig.test.dataframe$partner.Altered.Teststat)

#does excluding this level-two unit change the significance of the FPRS variable?
table(sig.test.dataframe$partner.Changed.Sig)

#graph variation in partner variable t-statistic created by excluding level-two units one at a time
hist(sig.test.dataframe$partner.Altered.Teststat, breaks = 100, main = "", xlab = "t-statistic")

################################################################################################################################################

### RUN THE MODEL ON DATA USING USING DIFFERENT CUT-OFFS FOR WHAT CONSTITUTES A TRAVEL PARTNER ###

#load the data sets (these data sets need to be made with the Python script 'finish_partner_data_creation_git.py')
mydata.25 = read.csv("finish_partner_data_25.csv")
mydata.24 = read.csv("finish_partner_data_24.csv")
mydata.23 = read.csv("finish_partner_data_23.csv")
mydata.22 = read.csv("finish_partner_data_22.csv")
mydata.21 = read.csv("finish_partner_data_21.csv")
mydata.20 = read.csv("finish_partner_data_20.csv")
mydata.19 = read.csv("finish_partner_data_19.csv")
mydata.18 = read.csv("finish_partner_data_18.csv")
mydata.17 = read.csv("finish_partner_data_17.csv")
mydata.16 = read.csv("finish_partner_data_16.csv")
mydata.15 = read.csv("finish_partner_data_15.csv")
mydata.14 = read.csv("finish_partner_data_14.csv")
mydata.13 = read.csv("finish_partner_data_13.csv")
mydata.12 = read.csv("finish_partner_data_12.csv")
mydata.11 = read.csv("finish_partner_data_11.csv")
mydata.10 = read.csv("finish_partner_data_10.csv")
mydata.9 = read.csv("finish_partner_data_9.csv")
mydata.8 = read.csv("finish_partner_data_8.csv")
mydata.7 = read.csv("finish_partner_data_7.csv")
mydata.6 = read.csv("finish_partner_data_6.csv")
mydata.5 = read.csv("finish_partner_data_5.csv")
mydata.4 = read.csv("finish_partner_data_4.csv")
mydata.3 = read.csv("finish_partner_data.csv")

### RUN THE MODELS ON EACH OF THE DATA SETS ###

data_list = c("mydata.1", "mydata.2", "mydata.3", "mydata.4", "mydata.5", "mydata.6", "mydata.7", "mydata.8", "mydata.9", "mydata.10", "mydata.11", "mydata.12",
              "mydata.13", "mydata.14", "mydata.15", "mydata.16", "mydata.17", "mydata.18", "mydata.19", "mydata.20", "mydata21", "mydata22", "mydata23", "mydata24", "mydata25")

#this creates the data frames to fill with the model results
mlm.it_int = data.frame()
mlm.it_partner = data.frame()
mlm.it_runs = data.frame()

#this creates an empty list to be filled with the size of each of the data sets
uniq_parkrunners = c()
total_runs = c()
percentage_partner_runs = c()

for (i in data_list){
  
  #this converst the character string in 'data_list' to one of the previously loaded date sets
  mydata = get(i) 
  
  ### GET INFORMATION ON FULL DATA SET ###
  
  #all unique parkrunenrs
  total.uniq <- unique(mydata$athnumber)
  
  #get total number of parkrunenrs who are the faster partner
  fast.runners <- mydata$athnumber[mydata$faster_or_slower == 'f']
  fast.uniq <- unique(fast.runners)
  # length(unique(fast.runners))
  
  #get total number of parkrunenrs who are the slower partner
  slow.runners <- mydata$athnumber[mydata$faster_or_slower == 's']
  slow.uniq <- unique(slow.runners)

  ### TRANSFORM THE VARIABLES ###
  
  #convert time to integer
  mydata$time = period_to_seconds(hms(mydata$time))
  mydata$time = mydata$time / 60
  mydata$time.lg = log(mydata$time)
  
  #exclude the parkrunners aged less than 18 or in a wheelchair
  mydata = subset(mydata, age != "C" & age != "10" & age != "11-14" & age != "15-17" & age != "---")
  mydata$age = droplevels(mydata$age)
  
  #get a subset of the data that is just parkrunners who are slower partners (and who have never been a faster partner)
  #slow_but_never_fast is defined above
  slow_only = subset(mydata, athnumber %in% slow_but_never_fast)
  
  #drops any athletes with less than 10 runs
  slow_only$athnumber = as.factor(slow_only$athnumber)
  tbl = table(slow_only$athnumber) 
  slow_only.more_than_10 = droplevels(slow_only[slow_only$athnumber %in% names(tbl)[tbl >= 10],,drop=TRUE])
  
  #drop unused parkrunners and locations
  slow_only.more_than_10$athnumber = droplevels(slow_only.more_than_10$athnumber) 
  
  #total number of slow partners and their runs (after exclusions)
  print('DATA SET INFORMATION AFTER EXCLUSIONS')
  print(length(unique(slow_only.more_than_10$athnumber)))
  print(nrow(slow_only.more_than_10))
  
  #get information on the size of the sample
  parkrunners = length(unique(slow_only.more_than_10$athnumber))
  tot_runs = nrow(slow_only.more_than_10)
  perc_partner_runs = table(slow_only.more_than_10$partner)[2] / sum(table(slow_only.more_than_10$partner))
  
  #add the size of the sample
  uniq_parkrunners = append(uniq_parkrunners, parkrunners)
  total_runs = append(total_runs, tot_runs)
  percentage_partner_runs = append(percentage_partner_runs, perc_partner_runs)
  
  print(table(slow_only.more_than_10$age))
  
  #log the previous runs variable
  slow_only.more_than_10$current_prev_runs.lg = log(slow_only.more_than_10$current_prev_runs)
  
  #make partner variable a factor
  slow_only.more_than_10$partner = as.factor(slow_only.more_than_10$partner)
  
  #run the multilevel model on the current data set
  mlm.it = lmer(time.lg ~ 1 + partner + current_prev_runs.lg + (1 + partner | athnumber), data = slow_only.more_than_10)
  summary(mlm.it)
  
  #this creates a matrix (1 row, 5 columns) that is the estimate, SE, df, t-value, and p-value for each coefficient in every 'results' model on the permuted data
  intercept = summary(mlm.it)$coefficients["(Intercept)", ]
  partner = summary(mlm.it)$coefficients["partner1", ]
  previous_runs = summary(mlm.it)$coefficients["current_prev_runs.lg", ]
  
  #add the matrices to the empty data sets
  mlm.it_int = rbind(mlm.it_int, intercept)
  mlm.it_partner = rbind(mlm.it_partner, partner)
  mlm.it_runs = rbind(mlm.it_runs, previous_runs)
  
}

#combine the data

#name columns
colnames(mlm.it_int) <- c("est", "se", "df", "t-val", "p")
colnames(mlm.it_partner) <- c("est", "se", "df", "t-val", "p")
colnames(mlm.it_runs) <- c("est", "se", "df", "t-val", "p")

#create a column that represents the number of times finishing partners have finished together within two seconds

#strength = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
strength = c(3,4,5,6,7,8,9,10,11,12)

mlm.it_int$partner_finishes = strength
mlm.it_partner$partner_finishes = strength
mlm.it_runs$partner_finishes = strength

#create a column that represents the sample size (unique parkrunners) for each 'times finished together' cut-off level
mlm.it_int$uniq_parkrunners = uniq_parkrunners
mlm.it_partner$uniq_parkrunners = uniq_parkrunners
mlm.it_runs$uniq_parkrunners = uniq_parkrunners

#create a column that represents the sample size (total runs) for each 'times finished together' cut-off level
mlm.it_int$total_runs = total_runs
mlm.it_partner$total_runs = total_runs
mlm.it_runs$total_runs = total_runs

#create a column in each data frame that names the variable that the estimates describe
mlm.it_int[, 'variable'] = 'intercept'
mlm.it_partner[, 'variable'] = 'partner'
mlm.it_runs[, 'variable'] = 'previous_runs'

#now combine the datasets

#for the multilevel models on run times
finishing_partner_effect_by_relationship_strength_mlm = rbind(mlm.it_int, mlm.it_partner, mlm.it_runs)

# #save data frame
# write.csv(finishing_partner_effect_by_relationship_strength_mlm, "finishing_partner_effect_by_relationship_strength_mlm.csv")
# 
# #reload the data frames
# finishing_partner_effect_by_relationship_strength_mlm = read.csv("finishing_partner_effect_by_relationship_strength_mlm.csv")

################################################################################################################################################

### MAKE A GRAPH SHOWING THE ESTIMATES OF THE FINISHING PARTNER EFFECT ON RUN TIMES BY EACH RELATIONSHIP STRENGTH ###

#subset the data to only have the estimates for the finishing partner variable and from the model on run times
finish_partner_estimates_run_time = subset(finishing_partner_effect_by_relationship_strength_mlm, variable == "partner")

#creaate upper and lower bounds of 95% confidence interval (2 se on either side)
finish_partner_estimates_run_time$lower.bound = (finish_partner_estimates_run_time$est - 2*finish_partner_estimates_run_time$se)
finish_partner_estimates_run_time$upper.bound = (finish_partner_estimates_run_time$est + 2*finish_partner_estimates_run_time$se)

#convert the 'est' variable (effect of travel partner on logged 5 km run times) to percentage change
finish_partner_estimates_run_time$est.percentage = finish_partner_estimates_run_time$est * 100

#creaate upper and lower bounds of confidence interval (1 se on either side)
finish_partner_estimates_run_time$lower.bound.percentage = (finish_partner_estimates_run_time$est.percentage - 1*(finish_partner_estimates_run_time$se * 100))
finish_partner_estimates_run_time$upper.bound.percentage = (finish_partner_estimates_run_time$est.percentage + 1*(finish_partner_estimates_run_time$se * 100))

# #run a linear model on the estimates (to be used as a line in the graph, but not appropriate here due to non-linearity)
# mod = lm(est.percentage ~ partner_finishes, data = finish_partner_estimates_run_time)
# summary(mod)
# 
# #get the estimated slope and intercept from the model
# par = summary(mod)$coefficients["partner_finishes", ][1]
# int = summary(mod)$coefficients["(Intercept)", ][1]

#remove data set for cut-off of one
finish_partner_estimates_run_time = subset(finish_partner_estimates_run_time, partner_finishes != 1)

#renumber rows to match 'partner_finishes' variable
row.names(finish_partner_estimates_run_time) = finish_partner_estimates_run_time$partner_finishes

#this is for the significane asterisks (index is one less than row number)
label.df.001 = data.frame(partner_finishes = c(2,3,24,25), est.percentage = c(finish_partner_estimates_run_time$upper.bound.percentage[1] + .03,
                                                                              finish_partner_estimates_run_time$upper.bound.percentage[2] + .03,
                                                                              finish_partner_estimates_run_time$upper.bound.percentage[23] + .03,
                                                                              finish_partner_estimates_run_time$upper.bound.percentage[24] + .03))

label.df.01 = data.frame(partner_finishes = c(23), est.percentage = c(finish_partner_estimates_run_time$upper.bound.percentage[22] + .03))

label.df.05 = data.frame(partner_finishes = c(4,18,19,20,21,22), est.percentage = c(finish_partner_estimates_run_time$upper.bound.percentage[3] + .03,
                                                                                    finish_partner_estimates_run_time$upper.bound.percentage[17] + .03,
                                                                                    finish_partner_estimates_run_time$upper.bound.percentage[18] + .03,
                                                                                    finish_partner_estimates_run_time$upper.bound.percentage[19] + .03,
                                                                                    finish_partner_estimates_run_time$upper.bound.percentage[20] + .03,
                                                                                    finish_partner_estimates_run_time$upper.bound.percentage[21] + .03))

label.df.10 = data.frame(partner_finishes = c(16,17), est.percentage = c(finish_partner_estimates_run_time$upper.bound.percentage[15] + .05,
                                                                         finish_partner_estimates_run_time$upper.bound.percentage[16] + .05))

#round the percentage of partner runs
finish_partner_estimates_run_time$percentage_partner_runs.rounded = round((100* finish_partner_estimates_run_time$percentage_partner_runs))

#make the actual graph

library(ggplot2)

graph.1 = ggplot(data = finish_partner_estimates_run_time, aes(x = partner_finishes, y = est.percentage)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower.bound.percentage, ymax = upper.bound.percentage), width = 0.5) + 
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
  theme(axis.title.y = element_text(family = "Arial", colour = "black", face = "bold", size = 11, margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(family = "Arial", colour = "black", face = "bold", size = 11, margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  theme(axis.text = element_text(family = "Arial", colour = "black", size = 10)) + 
  xlab("Minimum number of finishes together for finishing partners") + 
  ylab("Estimated finishing partner effect (as percentage change in 5 km run times)") +
  scale_x_continuous(breaks = pretty(finish_partner_estimates_run_time$partner_finishes,  n = 24)) +
  geom_hline(yintercept=c(0),linetype="dotted") +  
  #this draws a line segment based on the linear regression (mod) and the y-axis
  #geom_segment(aes(x = 0.5, y = int + par*0.5, xend = 12.5, yend = int + par*15.5), linetype = "dashed", colour = "blue") + 
  #geom_segment(aes(x = 1, y = - 1.725, xend = 15.5, yend = -1.725)) +
  geom_text(aes(label= percentage_partner_runs.rounded, y = -2.25), size = 3.75) +
  geom_text(aes(label= uniq_parkrunners, y = -2.3), size = 3.75) +
  geom_text(aes(label= total_runs, y = -2.35), size = 3.75) + 
  annotate("text", x = 0.2, y = -2.25, label = 'bold("partner runs (%)")', size = 3.75, parse = TRUE) +
  annotate("text", x = 0.2, y = -2.3, label = 'bold("parkrunners")', size = 3.75, parse = TRUE) + 
  annotate("text", x = 0.2, y = -2.35, label = 'bold("total runs")', size = 3.75, parse = TRUE) +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_text(data = label.df.001, label = "***") + 
  geom_text(data = label.df.01, label = "**") + 
  geom_text(data = label.df.05, label = "*") +
  geom_text(data = label.df.10, label = ".")

ggsave(filename = "mlm_finish_partner_effect_by_finishes_together.jpeg", graph.1)
