#clean environment
rm(list = ls())

library(lubridate)
library(lme4)
library(lmerTest)
library(optimx)
library(ggplot2)

#set current working directory
setwd("/Users/arrandavis/Desktop/University of Oxford/DPhil/Study 2/Parkrun.1/Data.Merge1")

#data for analyses
mydata = read.csv("travel_partners_full_8locations.csv", sep = ",")

### EXCLUSION OF CASES ###

#make the travel partner and athlete ID variables a factor
mydata$athnumber = as.factor(mydata$athnumber)

#exclude the parkrunners aged less than 18 or in a wheelchair
mydata = subset(mydata, age != "C" & age != "10" & age != "11-14" & age != "15-17" & age != "---")
mydata$age = droplevels(mydata$age)
table(mydata$age)

#exclude all parkrunners with less than 10 runs
mydata = mydata[mydata$athnumber %in% names(which(table(mydata$athnumber)>=10)), ]
range(table(mydata$athnumber))

################################################################################################################################################

### AVERAGE NUMBER OF DIFFERENT LOCATIONS TRAVELLED TO - DESCRIPTIVES ###

library(stringr)
library(tidyverse)
library(e1071)

locations_traveled_to <- mydata %>%
  #create location variable
  mutate(location = str_replace_all(parkrunid, "[0-9]", "")) %>%
  #sumarise unique locations attended by each parkrunner
  group_by(athnumber) %>% 
  summarize(n = n_distinct(location))

#these should be the same
length(unique(mydata$athnumber))
nrow(locations_traveled_to)

#descriptives for number of locations traveled to
mean(locations_traveled_to$n)
sd(locations_traveled_to$n)
range(locations_traveled_to$n)
skewness(locations_traveled_to$n)
mode(locations_traveled_to$n)

#mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(locations_traveled_to$n)

#assign a percentile for the distribution of locations travelled to
hist(locations_traveled_to$n, breaks = 100)
quantile(locations_traveled_to$n, c(.95))

#partners are people parkrunners have been to 'x' or more parkrun locations with that they do NOT run with (i.e., whom they've never finished within 1 minute of)

#numebr of solo v. number runs with parnter
table(mydata$travel_partner)

#percentage of partner runs
(table(mydata$travel_partner)[2] / sum(table(mydata$travel_partner))) * 100

#how many unique parkrunners have run with a partner at 8 or more seperate locations?
partner_runs = subset(mydata, travel_partner == '1')
length(unique(partner_runs$athnumber))

#this should be the same as above
length(unique(mydata$athnumber))

#average number of runs per parkrunner
sum(table(mydata$time)) / length(unique(mydata$athnumber))
mean(table(mydata$athnumber))

#standard deviation for number of runs per parkrunner
sd(table(mydata$athnumber))

#range for number of runs per parkrunner
range(table(mydata$athnumber))

#how many unique parkrun sites?
length(unique(mydata$location))

#this will get the average number of runs per participant (roughly 46 for the Australia subset; 45 for the entire data set)
nrow(mydata) / length(unique(mydata$athnumber))

################################################################################################################################################

### TIME TRANSFORMATION ###

#converts times to seconds and then minutes
mydata$time = period_to_seconds(hms(mydata$time))
mydata$time = mydata$time / 60
mydata$log_time = log(mydata$time)

mean_times <- aggregate(time ~ travel_partner, mydata, mean)

#converts average times to minutes
mydata$ave_time = mydata$ave_time / 60
hist(mydata$ave_time, breaks = 100)

################################################################################################################################################

### EXCLUSION OF CASES ###

#make the travel partner and athlete ID variables a factor
mydata$travel_partner = as.factor(mydata$travel_partner)  
mydata$athnumber = as.factor(mydata$athnumber)

#make the age variable an integer
mydata$age.int = as.integer(mydata$age)

#exclude the parkrunners aged less than 18
mydata = subset(mydata, age.int > 3)

#this gets the new number of parkrunners and runs in the dataset
length(unique(mydata$athnumber))
nrow(mydata)

#average number of runs per parkrunner
sum(table(mydata$time)) / length(unique(mydata$athnumber))
mean(table(mydata$athnumber))

#standard deviation for number of runs per parkrunner
sd(table(mydata$athnumber))

#range for number of runs per parkrunner
range(table(mydata$athnumber))

#percentage of partner runs
(table(mydata$travel_partner)[2] / sum(table(mydata$travel_partner))) * 100

################################################################################################################################################

### MAIN MULTILEVEL MODEL ON RUN TIMES ###

#this is the maximal model that can be fit to the data (adding additional random effects leads failure to converge)
logged.partner.model = lmer(log_time ~ 1 + travel_partner + gender + age.int + (1 + travel_partner | athnumber), data = mydata, REML = FALSE) #REML performs better in terms of parameter estimation (Maas & Hox, 2004)
summary(logged.partner.model)

#this gives the varainces and covarainces for the random effects
as.data.frame(VarCorr(partner.model))

#this will get the standard errors for the random effects
n = 28339
se.int = (sqrt(22.773) / sqrt(n))
se.part = (sqrt(0.749) / sqrt(n))
se.gen = (sqrt(10.054) / sqrt(n))
se.resid = (sqrt(6.394) / sqrt(n))

#this will give psuedo R-squared values
library(MuMIn)
r.squaredGLMM(logged.partner.model)

#this gets the mean time for solo runs
solo.subset = subset(mydata, travel_partner == 0)
mean(solo.subset$time)
sd(solo.subset$time)
range(solo.subset$time)

#this gets the mean time for partner runs
partner.subset = subset(mydata, travel_partner == 1)
mean(partner.subset$time)
sd(partner.subset$time)
range(partner.subset$time)

### T-TEST ON RUN TIMES (IGNORING LEVEL-TWO STRUCTURE) ###

#t-test on time between solo and partner runs
t.test(partner.subset$time, solo.subset$time)

### CHI-SQUARE TEST ON WHETHER SLOW RUNNERS RUN MORE OFTEN WITH PARTNERS ###

#test whether slower running partners are significantly more likely to run with a partner

#add a variable to the dataset that indicates whether runs come from faster or slower runners
slow.runners = mydata$athnumber[mydata$rank != 1 & mydata$rank !=0]
slow.uniq = unique(slow.runners)
length(slow.uniq)

fast.runners = subset(mydata, athnumber %!in% slow.uniq)
fast.uniq = unique(fast.runners$athnumber)
length(fast.uniq)

mydata$faster.or.slower = ifelse(mydata$athnumber %in% slow.uniq, 'Slow', ifelse(mydata$athnumber %in% fast.uniq, 'Fast', NA))

slow.subset = subset(mydata, faster.or.slower == "Slow")
slow.ath.solo.runs = table(slow.subset$travel_partner)[1]
slow.ath.partner.runs = table(slow.subset$travel_partner)[2]
#this will give the percentage of runs where slow partners ran with their faster partner
slow.ath.partner.runs / (slow.ath.partner.runs + slow.ath.solo.runs)

fast.subset = subset(mydata, faster.or.slower == "Fast")
fast.ath.solo.runs = table(fast.subset$travel_partner)[1]
fast.ath.partner.runs = table(fast.subset$travel_partner)[2]
#this will give the percentage of runs where fast partners ran with their slower partner
fast.ath.partner.runs / (fast.ath.partner.runs + fast.ath.solo.runs)

#test whether this difference is significant
chi.square.table = matrix(c(slow.ath.partner.runs, slow.ath.solo.runs, fast.ath.partner.runs, fast.ath.solo.runs), nrow = 2, ncol = 2)
chisq.test(chi.square.table)

### MULTILEVEL MODEL ON INTERACTION BETWEEN TRAVEL PARTNER EFFECT AND HOW FAST PARKRUNNERS ARE ###

#does the effect of travel partner interact with how fast people are? 
partner.model.speed = lmer(time ~ 1 + travel_partner*tertile.cat + gender + totalruns + age.int + (1 + travel_partner + gender | athnumber), data = mydata)
summary(partner.model.speed)

################################################################################################################################################

### MAKE GRAPHS FOR THIS MODEL (GRAPH OF RANDOM EFFECT) ###

#graph the random effects (https://stackoverflow.com/questions/13847936/in-r-plotting-random-effects-from-lmer-lme4-package-using-qqmath-or-dotplot/13921774; see also Snijders & Bosker, 2012; pp. 65-66)

partner.model = logged.partner.model

require(lme4)

library(ggplot2)
randoms = ranef(partner.model, postVar = TRUE)
qq = attr(ranef(partner.model, postVar = TRUE)[[1]], "postVar")

#this gives the random effects for the intercept and all slopes allowed to vary
random.effects = as.data.frame(randoms$athnumber)

#this gets the variance for the intercept for every athelete
int.var = list()
partner.var = list()

g = as.array(c(1:496)) #414 is the number of athletes

#this will get the variances for the empirical Bayes estimate, which are estimated for every level-two unit (Snijders & Bosker, 2012; p. 63)
for (i in g) {
  int.var = c(int.var, qq[,,i][1,1]) #the intercept variance is in the first row and column of the variance matrix
  partner.var = c(partner.var, qq[,,i][2,2]) #the partner slope variance is in the second row and column of the variance matrix
}

#appends the variances to the data frame
random.effects$var.int = as.numeric(sapply(int.var, paste0, collapse = ","))
random.effects$var.partner = as.numeric(sapply(partner.var, paste0, collapse = ","))

#create variable the is the estimated slope for the parkrunner (overall slope plus random slope estimate)
#IS THIS CORRECT?
random.effects$partner_slope_estimate = fixef(partner.model)["travel_partner1"] + ranef(partner.model)$athnumber$travel_partner1

#create confidence intervals for each variable; "the comparative standard error is the square root of the variance of the empirical Bayes Estimate" (Snijders & Bosker, 2012; p. 65)
#IS THIS CORRECT?
random.effects$CI.int = 1.96*sqrt(random.effects$var.int)
random.effects$CI.partner = 1.96*sqrt(random.effects$var.partner)

random.effects$CI.90.int = 1.64*sqrt(random.effects$var.int)
random.effects$CI.90.partner = 1.64*sqrt(random.effects$var.partner)

#create a variable based on row names (athlete ID's)
random.effects$athnumber = as.factor(rownames(random.effects))

#order dataset by the size of the random slope for the partner effect for each athlete
random.effects.ordered = random.effects[order(random.effects$travel_partner1),]

#create variable based on row number
random.effects.ordered$row.number = c(1:496)

#create variable to see if random effect confidence intervale crosses 0
#IS THIS CORRECT?

#this is for a 95% condidence interval
random.effects.ordered$partner.pos.or.neg = ifelse(random.effects.ordered$partner_slope_estimate > 0, "pos", ifelse(random.effects.ordered$partner_slope_estimate < 0, 'neg', NA))
random.effects.ordered$CI.contains.zero = ifelse(random.effects.ordered$partner.pos.or.neg == "pos", random.effects.ordered$partner_slope_estimate - random.effects.ordered$CI.partner, ifelse(random.effects.ordered$partner.pos.or.neg == "neg", random.effects.ordered$partner_slope_estimate + random.effects.ordered$CI.partner, NA))
random.effects.ordered$CI.significance = ifelse(random.effects.ordered$partner.pos.or.neg == "pos" & random.effects.ordered$CI.contains.zero > 0, 'yes',
                                                ifelse(random.effects.ordered$partner.pos.or.neg == "pos" & random.effects.ordered$CI.contains.zero < 0, 'no',
                                                       ifelse(random.effects.ordered$partner.pos.or.neg == "neg" & random.effects.ordered$CI.contains.zero < 0, 'yes',
                                                              ifelse(random.effects.ordered$partner.pos.or.neg == "neg" & random.effects.ordered$CI.contains.zero > 0, 'no', NA))))

#this is for a 90% confidence interval
random.effects.ordered$partner.pos.or.neg = ifelse(random.effects.ordered$partner_slope_estimate > 0, "pos", ifelse(random.effects.ordered$partner_slope_estimate < 0, 'neg', NA))
random.effects.ordered$CI.90.contains.zero = ifelse(random.effects.ordered$partner.pos.or.neg == "pos", random.effects.ordered$partner_slope_estimate - random.effects.ordered$CI.90.partner, ifelse(random.effects.ordered$partner.pos.or.neg == "neg", random.effects.ordered$partner_slope_estimate + random.effects.ordered$CI.90.partner, NA))
random.effects.ordered$CI.90.significance = ifelse(random.effects.ordered$partner.pos.or.neg == "pos" & random.effects.ordered$CI.90.contains.zero > 0, 'yes',
                                                ifelse(random.effects.ordered$partner.pos.or.neg == "pos" & random.effects.ordered$CI.90.contains.zero < 0, 'no',
                                                       ifelse(random.effects.ordered$partner.pos.or.neg == "neg" & random.effects.ordered$CI.90.contains.zero < 0, 'yes',
                                                              ifelse(random.effects.ordered$partner.pos.or.neg == "neg" & random.effects.ordered$CI.90.contains.zero > 0, 'no', NA))))

#make the graph
ggplot() + 
  geom_bar(data = random.effects.ordered, aes(x = row.number, y = partner_slope_estimate, fill = CI.90.significance),stat = "identity", width = 1) +
    theme(plot.background = element_blank(), panel.grid.major.y = element_line(colour="grey",size = rel(0.5)), panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(), panel.background = element_blank(), axis.line.x = element_line(color="black", size = 0.2), axis.line.y = element_line(color="black", size = 0.2), legend.key = element_blank(), axis.title.x = element_text(margin = unit(c(3, 0, 0, 0), "mm")), axis.title.y = element_text(margin = unit(c(0, 3, 0, 0), "mm")), text = element_text(size = 11, family = "sans")) + 
      scale_y_continuous(name="Random effect of travel partner presence on logged run times (minutes)", limits=c(-.25, .1)) +
        #n needs to equal the number of parkrunners in the data set
        xlab("Parkrunner (ordered by posterior slope size, n = 496)") + 
          guides(fill = FALSE)

#dev.off()

#do a chi-square to test whether there are more parkrunners with significant positive random effects than significant negative random effects
random.effects.ordered.neg = subset(random.effects.ordered, partner.pos.or.neg == "neg")
random.effects.ordered.pos = subset(random.effects.ordered, partner.pos.or.neg == "pos")

pos.no = table(random.effects.ordered.pos$CI.90.significance)[1]
pos.yes = table(random.effects.ordered.pos$CI.90.significance)[2]

neg.no = table(random.effects.ordered.neg$CI.90.significance)[1]
neg.yes= table(random.effects.ordered.neg$CI.90.significance)[2]

chi.square.table = matrix(c(pos.no, pos.yes, neg.no, neg.yes), nrow = 2, ncol = 2)

chisq.test(chi.square.table)

################################################################################################################################################

### MULTILEVEL REGRESSION - ASSUMPTION CHECKS ###

### ASSUMPTION CHECKS (FROM SNIJDERS & BOSKER, 2012) ###

results = lmer(log_time ~ 1 + travel_partner + previous_runs.lg + age.int + gender +  (1 + travel_partner | athnumber), data = mydata, REML = FALSE) #REML performs better in terms of parameter estimation (Maas & Hox, 2004)
summary(results)

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

library(lubridate)
library(lme4)
library(lmerTest)
library(RColorBrewer)

### ### ###

###################################################################
###                                                             ###
### Within - group OLS residuals                                ###
###                                                             ###
###################################################################

#this calculates OLS regressions per parkrunner; to be able to use the compareFits function, this keeps the parkrunner-level variables

#(nlme cannot be loaded for this to run)
detach("package:lmerTest", unload=TRUE)
detach("package:lme4", unload=TRUE)
detach("package:nlme", unload=TRUE)
library(lme4)

#this will estimate seperate linear models for every level of the grouping variable (so, a linear model will be run on ever parkrunners' data in that is in the data set)

#this renames the variables (for plot title purposes)
mydata$Partner = as.integer(mydata$travel_partner)
mydata$Runs = mydata$previous_runs.lg
mydata$Age = mydata$age.int
mydata$Gender = as.integer(mydata$gender)

#this keeps the parkrunner-level variables (even though they make no sense and will generate NA's since they don't vary, so that the compareFits function can be used)
olselist <- lmList(log_time ~ 1 + Partner + Runs + Age + Gender | athnumber, data = mydata)

#this runs the main model on the same subset of parkrunners
#summary(mod1 <- lme(time.lg ~ gender + age + partners + previous_runs.lg + partner_relation.lg, random = ~ previous_runs.lg + partner_relation.lg | athnumber, data = mydata.sub))
library(nlme)
summary(mod1 <- lme(log_time ~ Partner + Runs + Age + Gender, random = ~ Partner | athnumber, data = mydata))

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

#to study the outliers for the effect of the 'Partner' variable the plot of comp.models shows that the value .1 could be used as to seperate the outliers from the rest
strong.partner_relation.effect <- which(abs(comp.models[,1,"Partner"]) > .1)

#Are these particularly small groups? The average number of runs per parkrunner is:
mean_runs = mean(table(mydata$athnumber))

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
res = resid(results)
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

with(mydata,plot(previous_runs.lg, resi, xlab = expression('Previous runs'), ylab = "Residual"))
with(mydata,lines(lowess(previous_runs.lg, resi), col=cols[2])) #the lowess line indicates linearity

### MAKE A NORMAL PROBABILITY PLOT OF THE STANDARDISED OLS RESIDUALS TO CHECK THE ASSUMPTIONS OF A NORMAL DISTRIBUTION ###

#first define a function that gives the studentized OLS residuals for this model in a given data frame:
res_st5 <- function(x){
  rstudent(lm(log_time ~ Partner + Runs + Age + Gender,
              data=x))
}

#compute within-parkrunner studentized OLS residuals
resi_st <- by(mydata, mydata$athnumber, res_st5)
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
re.mod103 <- ranef(results, condVar=TRUE, standard=TRUE)

#this will get the posterior means (taking out the columns of random slopes and intercepts for each parkrunner):
postmean  <- re.mod103$athnumber[,1] #first column is the intercept
postmean  <- re.mod103$athnumber$`(Intercept)` #first column is the intercept (this is the same as above)
postslope.partner <- re.mod103$athnumber$travel_partner1

#this will get the posterior variances:
postmeanvar <-  attr(re.mod103$athnumber,'postVar')[1,1,]
postslopevar.partner <-  attr(re.mod103$athnumber,'postVar')[2,2,] 

#these are the comparative variances, cf. Section 4.8. in Snijders & Bosker (2012; p. 65)

#posterior means are computed by taking information from the overall mean and the group mean, which is inlfuenced by the size and variability of the observations within the group
#posterior means, in other words, are the empirical Bayes' estiamtes of the random slopes and intercepts (Snijders & Bosker, 2012; p. 64)

#the parameters of the random part
VarCorr(results)

#diagnostic variance is calculated using 4.18 from Snijders & Bosker (2012; p. 65):
diagmeanvar  <- VarCorr(results)$athnumber[1,1] - postmeanvar
diagslopevar.partner <- VarCorr(results)$athnumber[2,2] - postslopevar.partner

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

### HOMOSCEDASTICITY ###

#travel partner variable needs to be in integer (and 0 or 1)
mydata$travel_partner_int = as.integer(mydata$travel_partner)
mydata$travel_partner_int = mydata$travel_partner_int - 1

#we need to get the average for the predictor variable for each participant
participant.which <- match(unique(mydata$athnumber), mydata$athnumber)

#get the average for 'partner' for each participant
library(plyr)

j <- ddply(mydata, .(athnumber), summarise, mean_y = mean(travel_partner_int))
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

#this will estimate the model over and over again, excluding each parkrunner and seeing how the model changes

#this model is run on the subset of data, which is made up of data from (the model needs to be run with lmerTest loaded)
alt.est <- influence(results, group= "athnumber")

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

#load the data sets (these data sets need to be made with the Python script 'networkx_different_location_runs_git.py')
mydata.15 = read.csv("travel_partners_15locations.csv", sep = ",")
mydata.14 = read.csv("travel_partners_14locations.csv", sep = ",")
mydata.13 = read.csv("travel_partners_13locations.csv", sep = ",")
mydata.12 = read.csv("travel_partners_12locations.csv", sep = ",")
mydata.11 = read.csv("travel_partners_11locations.csv", sep = ",")
mydata.10 = read.csv("travel_partners_10locations.csv", sep = ",")
mydata.9 = read.csv("travel_partners_9locations.csv", sep = ",")
mydata.8 = read.csv("travel_partners_8locations.csv", sep = ",")
mydata.7 = read.csv("travel_partners_7locations.csv", sep = ",")
mydata.6 = read.csv("travel_partners_6locations.csv", sep = ",")
mydata.5 = read.csv("travel_partners_5locations.csv", sep = ",")
mydata.4 = read.csv("travel_partners_4locations.csv", sep = ",")
mydata.3 = read.csv("travel_partners_3locations.csv", sep = ",")

### RUN THE MODEL ON EACH OF THE DATA SETS ###

#this creates the data frames to fill with the model results
modeldat_int = data.frame()
modeldat_gen = data.frame()
modeldat_age = data.frame()
modeldat_par = data.frame()
modeldat_runs = data.frame()

#this creates an empty list to be filled with the size of the rows
percentage_partner_runs = c()
uniq_parkrunners = c()
total_runs = c()

data_list = c("mydata.3", "mydata.4", "mydata.5", "mydata.6", "mydata.7", "mydata.8",
              "mydata.9", "mydata.10", "mydata.11", "mydata.12", "mydata.13", "mydata.14")

#run seperate models
for (i in data_list){
  
  #this converst the character string in 'data_list' to one of the previously loaded date sets
  mydata = get(i)
  
  #transform the variables
  
  #converts times to seconds and then minutes
  mydata$time = period_to_seconds(hms(mydata$time))
  mydata$time = mydata$time / 60
  mydata$log_time = log(mydata$time)
  
  #make the travel partner and athlete ID variables a factor
  mydata$travel_partner = as.factor(mydata$travel_partner)  
  mydata$athnumber = as.factor(mydata$athnumber)
  
  #make the age variable an integer
  mydata$age.int = as.integer(mydata$age)
  
  #log transform the 'previous_runs' variable
  mydata$previous_runs.lg = log(mydata$previous_runs + 1)
  
  #exclude parkrunners
  
  #exclude the parkrunners aged less than 18 or in a wheelchair
  mydata = subset(mydata, age != "C" & age != "10" & age != "11-14" & age != "15-17" & age != "---")
  mydata$age = droplevels(mydata$age)
  
  #exclude all parkrunners with less than 10 runs
  mydata = mydata[mydata$athnumber %in% names(which(table(mydata$athnumber)>=10)), ]
  mydata$athnumber = droplevels(mydata$athnumber)
  
  #get information on the subsetted data set
  perc_partner_runs = sum(table(mydata$travel_partner)[2]) / sum(table(mydata$travel_partner))
  parkrunners = length(unique(mydata$athnumber))
  tot_runs = nrow(mydata)
  
  print(table(mydata$age))
  print(mean(table(mydata$athnumber)))
  print(sd(table(mydata$athnumber)))
  print(range(table(mydata$athnumber)))
  
  #add the size of the sample
  percentage_partner_runs = append(percentage_partner_runs, perc_partner_runs)
  uniq_parkrunners = append(uniq_parkrunners, parkrunners)
  total_runs = append(total_runs, tot_runs)
  
  #run the multilevel model on the current data set
  results = lmer(log_time ~ 1 + travel_partner + previous_runs.lg + age.int + gender +  (1 + travel_partner | athnumber), data = mydata, REML = FALSE) #REML performs better in terms of parameter estimation (Maas & Hox, 2004)
  summary(results)
  
  #this creates a matrix (1 row, 5 columns) that is the estimate, SE, df, t-value, and p-value for each coefficient in every 'results' model on the permuted data
  intercept = summary(results)$coefficients["(Intercept)", ]
  travel_partner = summary(results)$coefficients["travel_partner1", ]
  previous_runs = summary(results)$coefficients["previous_runs.lg", ]
  gender = summary(results)$coefficients["genderW", ]
  age = summary(results)$coefficients["age.int", ]
  
  #add the matrices to the empty data sets
  modeldat_int = rbind(modeldat_int, intercept)
  modeldat_gen = rbind(modeldat_gen, gender)
  modeldat_age = rbind(modeldat_age, age)
  modeldat_par = rbind(modeldat_par, travel_partner)
  modeldat_runs = rbind(modeldat_runs, previous_runs)
  
}

#combine the data

#name columns
colnames(modeldat_int) <- c("est", "se", "df", "t-val", "p")
colnames(modeldat_gen) <- c("est", "se", "df", "t-val", "p")
colnames(modeldat_par) <- c("est", "se", "df", "t-val", "p")
colnames(modeldat_age) <- c("est", "se", "df", "t-val", "p")
colnames(modeldat_runs) <- c("est", "se", "df", "t-val", "p")

#create a column that represents the strength of the travel partner relationship (i.e., locations visited)

strength = c(3,4,5,6,7,8,9,10,11,12,13,14,15)

modeldat_int$locations_visited = strength
modeldat_gen$locations_visited = strength
modeldat_age$locations_visited = strength
modeldat_par$locations_visited = strength
modeldat_runs$locations_visited = strength

#create a column that represents the percentage of partner runs in the data set
modeldat_int$partner_runs = percentage_partner_runs
modeldat_gen$partner_runs = percentage_partner_runs
modeldat_age$partner_runs = percentage_partner_runs
modeldat_par$partner_runs = percentage_partner_runs
modeldat_runs$partner_runs = percentage_partner_runs

#create a column that represents the sample size (unique parkrunners) for each level of the strength of the partner relationship
modeldat_int$uniq_parkrunners = uniq_parkrunners
modeldat_gen$uniq_parkrunners = uniq_parkrunners
modeldat_age$uniq_parkrunners = uniq_parkrunners
modeldat_par$uniq_parkrunners = uniq_parkrunners
modeldat_runs$uniq_parkrunners = uniq_parkrunners

#create a column that represents the sample size (total runs) for each level of the strength of the partner relationship
modeldat_int$total_runs = total_runs
modeldat_gen$total_runs = total_runs
modeldat_age$total_runs = total_runs
modeldat_par$total_runs = total_runs
modeldat_runs$total_runs = total_runs

#create a column in each data frame that names the variable that the estimates describe
modeldat_int[, 'variable'] = 'intercept'
modeldat_gen[, 'variable'] = 'gender'
modeldat_age[, 'variable'] = 'age'
modeldat_par[, 'variable'] = 'travel_partner'
modeldat_runs[, 'variable'] = 'previous_runs'

#now combine the datasets
travel_partner_effect_by_relationship_strength = rbind(modeldat_int, modeldat_gen, modeldat_age, modeldat_par, modeldat_runs)

#save the data set
write.csv(travel_partner_effect_by_relationship_strength, "travel_partner_effect_by_relationship_strength_1min_11SEP2018.csv")

################################################################################################################################################

### MAKE A GRAPH SHOWING THE ESTIMATES OF THE TRAVEL PARTNER EFFECT BY EACH RELATIONSHIP STRENGTH ###

#subset the data to only have the estimates for the travel partner variable
travel_partner_estimates = subset(travel_partner_effect_by_relationship_strength, variable == "travel_partner")

#subset the to 15 locations or less
travel_partner_estimates = subset(travel_partner_estimates, locations_visited <=15)

#make the percentage of partner runs variable a percentage
travel_partner_estimates$partner_runs = travel_partner_estimates$partner_runs * 100
travel_partner_estimates$partner_runs = round(travel_partner_estimates$partner_runs, digits = 0)

#convert the 'est' variable (effect of travel partner on logged 5 km run times) to percentage change
travel_partner_estimates$est.percentage = travel_partner_estimates$est * 100

#creaate upper and lower bounds of confidence interval (1 se on either side)
travel_partner_estimates$lower.bound.percentage = (travel_partner_estimates$est.percentage - 1*(travel_partner_estimates$se * 100))
travel_partner_estimates$upper.bound.percentage = (travel_partner_estimates$est.percentage + 1*(travel_partner_estimates$se * 100))

#this is for the significane asterisks (index is one less than row number)
label.df.05 = data.frame(locations_visited = c(8), est.percentage = c(travel_partner_estimates$upper.bound.percentage[6] + .03))

label.df.10 = data.frame(locations_visited = c(9,11), est.percentage = c(travel_partner_estimates$upper.bound.percentage[7] + .05,
                                                                         travel_partner_estimates$upper.bound.percentage[9] + .05))
library(ggplot2)

ggplot(data = travel_partner_estimates, aes(x = locations_visited, y = est.percentage)) + 
  geom_point() + 
  geom_errorbar(aes(ymin = lower.bound.percentage, ymax = upper.bound.percentage), width = 0.5) + 
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(panel.background = element_rect(fill = 'white', colour = 'grey')) +
  theme(axis.title.y = element_text(family = "Arial", colour = "black", face = "bold", size = 11, margin = margin(t = 0, r = 15, b = 0, l = 0))) +
  theme(axis.title.x = element_text(family = "Arial", colour = "black", face = "bold", size = 11, margin = margin(t = 15, r = 0, b = 0, l = 0))) +
  theme(axis.text = element_text(family = "Arial", colour = "black", size = 10)) + 
  xlab("Number of locations visited by travel partners") + 
  ylab("Estimated travel partner effect (as percentage change in 5 km run times)") +
  scale_x_continuous(breaks = pretty(travel_partner_estimates$locations_visited,  n = 12)) +
  geom_hline(yintercept=c(0), linetype="dotted") + 
  #geom_segment(aes(x = 2.5, y = int + par*2.5, xend = 15.5, yend = int + par*15.5), linetype = "dashed", colour = "blue") + 
  geom_text(aes(label= partner_runs, y = -1.8), size=4) +
  geom_text(aes(label= uniq_parkrunners, y = -1.9), size=4) +
  geom_text(aes(label= total_runs, y = -2), size=4) + 
  annotate("text", x = 1.8, y = -1.8, label = 'bold("partner runs (%)")', size = 3.75, parse = TRUE) +
  annotate("text", x = 1.8, y = -1.9, label = 'bold("parkrunners")', size = 4, parse = TRUE) + 
  annotate("text", x = 1.8, y = -2, label = 'bold("total runs")', size = 4, parse = TRUE) +
  geom_segment(aes(x = 1, y = - 1.725, xend = 15.5, yend = -1.725)) + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  geom_text(data = label.df.05, label = "*") +
  geom_text(data = label.df.10, label = ".")
