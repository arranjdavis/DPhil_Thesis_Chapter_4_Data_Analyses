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

#working directory
setwd("/Users/arrandavis/Desktop/University of Oxford/DPHIL/Study 2/Parkrun.1/Data.Merge1")

#clean environment
rm(list = ls())

library(lubridate)
library(lme4)
library(lmerTest)
library(RColorBrewer)

#data for model to be checked
mydata = read.csv("full_data_set_ordered_by_date_friends_runs_partners.csv")

### ### ###

### DATA REDUCTION, VARIABLE CREATION, AND DESCRIPTIVE STATISTICS ###

#total number of parkrunners and runs in the original data set
orig_parkrunners = length(unique(mydata$athnumber))
orig_runs = nrow(mydata)

#takes only parkrunners who are over age 18 and not in a wheelchair
mydata$age = as.integer(mydata$age)
mydata <- subset(mydata, age > 3)
mydata <- subset(mydata, age < 20)

#drops any parkrunners with less than 10 runs
mydata$athnumber <- as.factor(mydata$athnumber)
tbl <- table(mydata$athnumber)
mydata <- droplevels(mydata[mydata$athnumber %in% names(tbl)[tbl >= 10],,drop=FALSE])

#takes only athletes from the UK
mydata.UK = subset(mydata, Country == "UK")

#total number of removed parkrunners and removed runs
used_parkrunners = length(unique(mydata.UK$athnumber))
used_runs = nrow(mydata.UK)

#return the number of excluded parkrunners and runs
orig_parkrunners - used_parkrunners
orig_runs - used_runs

#use the UK data set (see FPRS_model_by_cutoff.R)
mydata = mydata.UK

#drop unused parkrunners
mydata$athnumber = droplevels(mydata$athnumber)

### ### ###

#creates the parkrun location variable
mydata.UK$location = gsub("\\d+", "", mydata.UK$parkrunid)

#converts times to seconds
mydata.UK$time = period_to_seconds(hms(mydata.UK$time))
#converts time to minutes
mydata.UK$time = (mydata.UK$time/60)

#performs the necessary transformations on the variables in the model
mydata.UK$previous_runs.lg2 = log2(1 + mydata.UK$previous_runs)
mydata.UK$previous_runs.lg = log(1 + mydata.UK$previous_runs)

mydata.UK$partner_relation.lg2 = log2(1 + mydata.UK$partner_relation)
mydata.UK$partner_relation.lg = log(1 + mydata.UK$partner_relation)

mydata.UK$time.lg = log(mydata.UK$time)

### ### ###

#use the UK data set (see FPRS_model_by_cutoff.R)
mydata = mydata.UK

#multilevel models estimated below need to be calculated using this package
library(nlme)

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

###   EXCLUDE ANY RUNNERS WITH LESS THAN 15 RUNS (SNIJDERS & BOSKER, 2012; P. 159) ###

#extract model variance and correlation components
VarCorr(mod1)

### ### ###

#This calculates OLS regressions per parkrunner. To be able to use the compareFits function, this keeps the parkrunner-level variables (age and gender), although they have no sense for the OLS regressions.

#this will estimate seperate linear models for every level of the grouping variable

#to create the above model, first need to subset the data - take %1 of all parkrunners (these estimates would take weeks using the entire data set)
sample_size = length(unique(mydata$athnumber)) / 100

parkrunners.sample = sample(unique(mydata$athnumber), sample_size)

mydata.sub = subset(mydata, athnumber %in% parkrunners.sample)

mydata.sub$athnumber = droplevels(mydata.sub$athnumber)

#this will run a liner model at every level of the grouping factor that is present in the data set (so, a linear model will be run on ever parkrunners' data that is in the data set)

#for some reason gender cannot be included as a factor
mydata.sub$gender = as.integer(mydata.sub$gender)

#this renames the variables (for plot title purposes)
mydata.sub$Gender = mydata.sub$gender
mydata.sub$Age = mydata.sub$age
mydata.sub$Partners = mydata.sub$partners
mydata.sub$Runs = mydata.sub$previous_runs.lg
mydata.sub$FPRS = mydata.sub$partner_relation.lg

#this keeps the parkrunner-level variables (even though they make no sense and will generate NA's since they don't vary, so that the compareFits function can be used)
#olselist <- lmList(time.lg ~ gender + age + partners + previous_runs.lg + partner_relation.lg | athnumber, data = mydata.sub)
olselist <- lmList(time.lg ~ Gender + Age + Partners + Runs + FPRS | athnumber, data = mydata.sub) #renamed variables

#this runs the main model on the same subset of parkrunners
#summary(mod1 <- lme(time.lg ~ gender + age + partners + previous_runs.lg + partner_relation.lg, random = ~ previous_runs.lg + partner_relation.lg | athnumber, data = mydata.sub))
summary(mod1 <- lme(time.lg ~  Gender + Age + Partners + Runs + FPRS, random = ~ Runs + FPRS | athnumber, data = mydata.sub)) #renamed variables

#The compareFits function compares the OLS estimates with the posterior means of the multilevel model. This is done only for the level-1 effects.
#The graph produced below will compare the variance of the OLS regression estimates from each parkrunner to the posterior means calculated in the multilevel model

comp.models <- compareFits(coef(olselist), coef(mod1))
plot(comp.models, ylab = "")

# The plots indicate that variabiliry for the OLS estimates is indeed larger, but less so for the FPRS variable

#this will get the within-group residual d.f.
#first see how olselist is structured:
str(olselist, 1)

#it is a list of results for each of the parkrunners.
str(olselist[[1]], 1)

#the within-group residual d.f. can be obtained as follows (this is number of runs minus the number of parameters plus 1):

df_res <- sapply(olselist, function(x){x$df.residual})
table(df_res)

#to study the outliers for the effect of the FPRS variable the plot of comp.models shows that the value .5 could be used to seperate the outliers from the rest
strong.partner_relation.effect <- which(abs(comp.models[,1,"FPRS"]) > .5) 

#What are the numbers in the table when this is printed? They are the number of the parkrunner and the number of the model.

#Are these particularly small groups? The average number of runs per parkrunner is:
mean_runs = mean(table(mydata.sub$athnumber))

strong.FPRS.table = df_res[strong.partner_relation.effect] #smaller than average, but larger groups are also included

#this will give the percentage of these runners who have a total number of runs larger than the average
counter = 0

for (i in strong.FPRS.table) {
  
  if (mean_runs < i)
    
    counter = counter + 1
  
}

#percentage of these parkrunners who have fewer than the average amount of runs
1 - (counter / length(strong.FPRS.table))

#the relatively small size of these groups may well account for their deviating observed effects of the FPRS variable; 
#these outliers are thus not very bothering (Snijders & Bosker, 2012)

### ### ###

### NULL IS HOMOSCEDASTICITY, AND IT HAS A CHI-SQUARE DISTRIBUTION UNDER THE NULL ###

#Example 10.1 from Snijders & Bosker (2012)

#make a selection of the parkrunners with d.f. at least 20
use <- df_res >= 20

#The number of such parkrunners is
sum(use)

#now get the within-group residual standard deviations
summary(olselist[[1]])$sigma

#make a plot of residual variance against residual d.f.:
sigma2_res <- sapply(olselist, function(x){(summary(x)$sigma)^2})
plot(df_res, sigma2_res) 

#There are some outliers for the very low residual d.f.

#Formula (10.3); "Used to detect heteroscedasticity in the form of between-group differences in level-one residual variance" (Snijders & Bosker, 2012; p. 159).

#test heteroscedasticity
ls_tot <- sum((df_res*log(sigma2_res))[use])/sum(df_res[use])
d <- (sqrt(0.5*df_res))*(log(sigma2_res) - ls_tot)
(H <- sum((d^2)[use]))

#The associated p-value is:
1-pchisq(H, sum(use)-1)

#this will plot d, which is Gausian if there is level-one homoscedasticity
qqnorm(d[use])
qqline(d[use])

### ### ###

# Example 10.2

#the goal here is to inspect level-one residuals, step one is to plot the unstandardised OLS residuals against level-one explanatory variables to check for non-linearity
#step two is to make a normal probability plot of the standardised OLS residuals to check the assumption of a normal distribution

### PLOT UNSTANDARDISED RESIDUALS AGAINST LEVEL-ONE EXPLANATORY VARIABLES TO CHECK FOR NON-LINEARITY ###

#The within-group OLS residuals are obtained by
resi <- residuals(olselist)

#for the logged previous runs variable
with(mydata.sub,plot(previous_runs.lg,resi, xlab = "Logged previous runs", ylab = "Residual"))
with(mydata.sub,lines(lowess(previous_runs.lg,resi)))

#for the logged FPRS variable
with(mydata.sub,plot(partner_relation.lg,resi, xlab = "Logged FPRS", ylab = "Residual"))
with(mydata.sub,lines(lowess(partner_relation.lg,resi)))

#the lowess line indicates linearity

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(time.lg ~ gender + age + partners + previous_runs.lg + partner_relation.lg,
              data=x))
}

#compute within-parkrunner studentized OLS residuals
resi_st <- by(mydata.sub, mydata.sub$athnumber, res_st5)
rs <- unlist(resi_st)

#some of the residuals are NA or exactly 0.
sum(is.na(rs))
sum(rs[!is.na(rs)]==0)

#this is because of the presence of small groups (Snijders & Bosker, 2012)

#make a QQ plot with use the <use> vector defined above (to only include the parkrunners that have enough runs)
qqnorm(rs[use], main = "")
qqline(rs[use])

###################################################################
###                                                             ###
### Residuals at level two                                      ###
###                                                             ###
###################################################################

#Example 10.3

#the goal here is to inspect level-two residuals, step one is to plot the unstandardised level-two residuals against "relevant level-two variables" to check for non-linearity
#step two is to make normal probability plots of standardised level-two residuals to check the assumption of a normal distribution, and in
#step three the squared standardised level-two residuals may be plotted as a function of level-two variables to check homoscedasticity

#lme4 is used for these checks
detach("package:nlme")
library(lme4)

#this is the model for which the level-two residuals will be inspected (this should be the full model)
(mod103 <- lmer(time.lg ~ gender + age + partners + previous_runs.lg + partner_relation.lg + (previous_runs.lg + partner_relation.lg | athnumber), data = mydata.UK))

#get the random effects for this model
re.mod103 <- ranef(mod103, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each parkrunner):
postmean  <- re.mod103$athnumber[,1] #first column is the intercept
postmean  <- re.mod103$athnumber$`(Intercept)` #shows that the first column is indeed the intercept
postslope.prev_runs <- re.mod103$athnumber$previous_runs.lg
postslope.FPRS <- re.mod103$athnumber$partner_relation.lg

#this will get the posterior variances:
#for example, attr(re.mod103$athnumber,'postVar')[,,1103] will give the posterior variabnces for the 1103rd parkrunner
#the first row/column is the intercept, the second row/column is for 'previous_runs.lg,' and the third is for 'partner_relation.lg'
#so, attr(re.mod103$athnumber,'postVar')[2,2,1103] gives the posterior variance for the 'previous_runs.lg' for the 1103rd parkrunner
postmeanvar <-  attr(re.mod103$athnumber,'postVar')[1,1,]
postslopevar.prev_runs <-  attr(re.mod103$athnumber,'postVar')[2,2,] 
postslopevar.FPRS <-  attr(re.mod103$athnumber,'postVar')[3,3,]

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is influenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(mod103)   

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(mod103)$athnumber[1,1] - postmeanvar
diagslopevar.prev_runs <- VarCorr(mod103)$athnumber[2,2] - postslopevar.prev_runs
diagslopevar.FPRS <- VarCorr(mod103)$athnumber[3,3] - postslopevar.FPRS

### PLOT UNSTANDARDISED LEVEL-TWO RESIDUALS AGAINST RELEVSNT LEVEL TWO VARIABELS (PARKRUNNER'S AGE IN THIS CASE), TO CHECK FOR LINEARITY (GENDER NOT APPLICABLE, BUT WILL BE USED LATER) ###

#the parkrunner-level variables in the data frame (use the full data frame here, since the model that is being assessed is based of this data)
#can be taken from the level of individual runs to the parkrunner level as follows:
parkrunner.which <- match(unique(mydata.UK$athnumber),mydata.UK$athnumber)

#this is the row number in 'mydata.UK' of the first run from each parkrunner:
parkrunner.which

#using this, the vector of parkrunners' gender:
gender.parkrunner = mydata.UK$gender[parkrunner.which]

#the vector for parrkunners average age
t = aggregate(mydata.UK$age, by = list(mydata.UK$athnumber), FUN = mean)
age.parkrunner = t$x

#these should all have the same length
length(age.parkrunner)
length(gender.parkrunner)
length(unique(mydata.UK$athnumber))
length(parkrunner.which)

#now make a plot like figure 10.4 (Snijders & Bosker, 2012; p. 166) for both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#posterior intercepts by age
plot(age.parkrunner, postmean, ylab = "Posterior intercept", xlab = "Age category")
cols = brewer.pal(n=4,name="Set1")
lines(lowess(age.parkrunner, postmean), col=cols[2])

#now make a plot like figure 10.5 (Snijders & Bosker, 2012; p. 166) for both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#posterior previous runs slopes by age
plot(age.parkrunner, postslope.prev_runs, ylab = "Posterior previous runs slope", xlab = "Age category")
lines(lowess(age.parkrunner, postslope.prev_runs), col=cols[2])

#posterior FPRS slopes by age
plot(age.parkrunner, postslope.FPRS, ylab = "Posterior FPRS slope", xlab = "Age category")
lines(lowess(age.parkrunner, postslope.FPRS), col=cols[2])

### NORMAL PROBABILITY PLOTS OF STANDARDISED LEVEL-TWO RESIDUALS TO CHECK FOR LEVEL-TWO RESIDUAL NORMALITY ###

#makee a plot like figure 10.6 (Snijders & Bosker, 2012; p. 166) for the intercepts and both the 'partner_relation.lg' and 'previous_runs.lg' variables:

#for the intercepts
postmean.stand <- postmean/sqrt(diagmeanvar)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'previous_runs.lg' variable
postmean.stand <- postslope.prev_runs/sqrt(diagslopevar.prev_runs)
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

#for the 'partner_relation.lg' variable
postmean.stand <- postslope.FPRS/sqrt(diagslopevar.FPRS) 
qqnorm(postmean.stand, main = "")
qqline(postmean.stand)

### PLOT SQUARED, STANDARDIZED LEVEL-TWO RESIDUALS AS A FUNCTION OF RELEVANT LEVEL-TWO VARIABLES (AGE AND GENDER, IN THIS CASE) TO CHECK FOR HOMOSCEDASTICITY ###

#for the intercepts

#squared, standardized posterior intercepts
sq.postmean.stand <- (postmean/sqrt(diagmeanvar))^2

#by gender
boxplot(gender.parkrunner, sq.postmean.stand, outline = FALSE, names = c("Male", "Female"), xlab = "Gender", ylab = "Squared standardised level-two intercept residuals")

#by age
plot(age.parkrunner, sq.postmean.stand, xlab = "Age category", ylab = "Squared standardised level-two intercept residuals")

#for the 'previous_runs.lg' variable

#squared, standardized posterior previous runs slopes
sq.postmean.stand <- (postslope.prev_runs/sqrt(diagslopevar.prev_runs))^2

#by gender
boxplot(gender.parkrunner, sq.postmean.stand, outline = FALSE, names = c("Male", "Female"), xlab = "Gender", ylab = "Squared standardised level-two previous runs slope residuals")

#by age
plot(age.parkrunner, sq.postmean.stand, xlab = "Age category", ylab = "Squared standardised level-two previous runs slope residuals")

#for the 'partner_relation.lg' variable

#squared, standardized posterior FPRS slopes
sq.postmean.stand <- (postslope.FPRS/sqrt(diagslopevar.FPRS))^2

#by gender
boxplot(gender.parkrunner, sq.postmean.stand, outline = FALSE, names = c("Male", "Female"), xlab = "Gender", ylab = "Squared standardised level-two FPRS slope residuals")

#by age
plot(age.parkrunner, sq.postmean.stand, xlab = "Age category", ylab = "Squared standardised level-two FPRS slope residuals")

###################################################################
###                                                             ###
### Influence of level-two units                                ###
###                                                             ###
###################################################################

### INFLUENTIAL DATA POINTS ###

#(from this package: https://cran.r-project.org/web/packages/influence.ME/influence.ME.pdf)

library(influence.ME)

#this will estimate the model over and over again, excluding each parkrunner and seeing how the model changes, this would take ~150,000 * 2 hours on the full model, so it was 
#done for a model run on subset of 1% of parkrunners

#this model is run on a subset of data, which is made up of data from 1% of parkrunners 
(mod_sub <- lmer(time.lg ~ gender + age + partners + previous_runs.lg + partner_relation.lg + (previous_runs.lg + partner_relation.lg | athnumber), data = mydata.sub))
alt.est <- influence(mod_sub, group= "athnumber")

#Cook's distances
cooks.distance.estex(alt.est)
range(cooks.distance.estex(alt.est))
mean(cooks.distance(alt.est))
sd(cooks.distance(alt.est))
hist(cooks.distance(alt.est), breaks = 100, main = "", xlab = "Cook's distance")

#percent change in parameter estimates when including and not including each level-two unit (i.e., parkrunner)
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

#the sigtest function tests whether excluding a particular level-two unit (i.e., parkrunner) changes the statistical significance (at 1.96) of a model’s predictor variables (Nieuwenhuis et al. 2012)
sig.test.dataframe = as.data.frame(sigtest(alt.est))

#FPRS variable

#this gives the range of t-values produced by interatively excluding the sampled parkrunners one at a time
range(sig.test.dataframe$partner_relation.lg.Altered.Teststat)

#does excluding this level-two unit change the significance of the FPRS variable?
table(sig.test.dataframe$partner_relation.lg.Changed.Sig)

#graph variation in FPRS t-statistic created by excluding level-two units one at a time
hist(sig.test.dataframe$partner_relation.lg.Altered.Teststat, breaks = 100, main = "", xlab = "t-statistic")
