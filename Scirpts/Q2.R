# Step 1: Load libraries and read data
library(dplyr)
library(survey)
library(ggplot2)
library(tidyr)
library(ezids)
library(car)
library(pROC)
library(pscl)
library(corrplot)

setwd("C:/Users/dhowell07/Documents/DATS6101/R/Research Project/Project2")

brfss = read.csv("brfss_2018_2023.csv", header=T)


##2. Physical Activity and BMI: 
##To what extent can physical activity level predict an individual’s BMI, 
##controlling for demographic covariates and survey year?

brfss_1 = brfss[,c(1:8,12:16,19:22,24)]

##Check class of variables
str(brfss_1)

##Change variable class to factor and order variables that need to be ordered
brfss_1$psu = as.factor(brfss_1$psu)
brfss_1$psu <- factor(brfss_1$psu)
brfss_1$interview_year = as.factor(brfss_1$interview_year)
brfss_1$interview_year <- factor(brfss_1$interview_year, order=T,levels = c("2018","2019","2020","2021","2022","2023"))
brfss_1$bmi_category = as.factor(brfss_1$bmi_category)
brfss_1$bmi_category <- factor(brfss_1$bmi_category, order=T,levels = c("Underweight","Normal weight","Overweight","Obese"))
brfss_1$overweight_or_obese = as.factor(brfss_1$overweight_or_obese)
brfss_1$overweight_or_obese <- factor(brfss_1$overweight_or_obese, order=T,levels = c("Yes","No"))
brfss_1$any_exercise_last_month = as.factor(brfss_1$any_exercise_last_month)
brfss_1$any_exercise_last_month <- factor(brfss_1$any_exercise_last_month, order=T,levels = c("No","Yes"))
brfss_1$sex = as.factor(brfss_1$sex)
brfss_1$sex <- factor(brfss_1$sex, order=F)
brfss_1$age_group = as.factor(brfss_1$age_group)
brfss_1$age_group <- factor(brfss_1$age_group, order=T,levels = c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60-64","65-69","70-74","75-79","80 or older"))
brfss_1$race_ethnicity = as.factor(brfss_1$race_ethnicity)
brfss_1$race_ethnicity <- factor(brfss_1$race_ethnicity, order=F)
brfss_1$education_level = as.factor(brfss_1$education_level)
brfss_1$education_level <- factor(brfss_1$education_level, order=T,levels = c("No school/K","Grades 1-8","Grades 9-11","Grade 12/GED","College 1-3 years","College 4+ years"))
brfss_1$diabetes_status = as.factor(brfss_1$diabetes_status)
brfss_1$diabetes_status <- factor(brfss_1$diabetes_status, order=T,levels = c("No","Yes","Pre-diabetes","Yes, only pregnancy"))
brfss_1$coronary_hd_history = as.factor(brfss_1$coronary_hd_history)
brfss_1$coronary_hd_history <- factor(brfss_1$coronary_hd_history, order=T,levels = c("No","Yes"))
brfss_1$stroke_history = as.factor(brfss_1$stroke_history)
brfss_1$stroke_history <- factor(brfss_1$stroke_history, order=T,levels = c("No","Yes"))
brfss_1$self_reported_health = as.factor(brfss_1$self_reported_health)
brfss_1$self_reported_health <- factor(brfss_1$self_reported_health, order=T,levels = c("Good or Better","Fair or Poor"))

str(brfss_1)

brfss_1 <- brfss_1 %>%
  mutate(obese = ifelse(bmi >= 30, 1, 0))
brfss_1 <- brfss_1 %>%
  mutate(exercise = ifelse(any_exercise_last_month == "No", 0, 1))
brfss_1 <- brfss_1 %>%
  mutate(diabetes = ifelse(diabetes_status == "No", 0, 1))
brfss_1 <- brfss_1 %>%
  mutate(heart_attack = ifelse(heart_attack_history == "No", 0, 1))
brfss_1 <- brfss_1 %>%
  mutate(coronary = ifelse(coronary_hd_history == "No", 0, 1))
brfss_1 <- brfss_1 %>%
  mutate(stroke = ifelse(stroke_history == "No", 0, 1))
brfss_1 <- brfss_1 %>%
  mutate(self_health = ifelse(self_reported_health == "Fair or Poor", 0, 1))

str(brfss_1)

# Spearman Correlation Matrix
brfss_nums = brfss_1[,c(5,10,19:25)]
str(brfss_nums)
spearman_corr <- cor(brfss_nums, use = "complete.obs", method = "spearman")
print(spearman_corr)
corrplot(spearman_corr, method = "color", type = "upper", tl.cex = 0.8)

brfss_1$obese = as.factor(brfss_1$obese)
brfss_1$obese <- factor(brfss_1$obese, order=T,levels = c("0","1"))
brfss_1$exercise = as.factor(brfss_1$exercise)
brfss_1$exercise <- factor(brfss_1$exercise, order=T,levels = c("0","1"))
brfss_1$diabetes = as.factor(brfss_1$diabetes)
brfss_1$diabetes <- factor(brfss_1$diabetes, order=T,levels = c("0","1"))
brfss_1$heart_attack = as.factor(brfss_1$heart_attack)
brfss_1$heart_attack <- factor(brfss_1$heart_attack, order=T,levels = c("0","1"))
brfss_1$coronary = as.factor(brfss_1$coronary)
brfss_1$coronary <- factor(brfss_1$coronary, order=T,levels = c("0","1"))
brfss_1$stroke = as.factor(brfss_1$stroke)
brfss_1$stroke <- factor(brfss_1$stroke, order=T,levels = c("0","1"))
brfss_1$self_health = as.factor(brfss_1$self_health)
brfss_1$self_health <- factor(brfss_1$self_health, order=T,levels = c("0","1"))

str(brfss_1)

##Fitting a line to between BMI and Exercise
fit1 <- lm(bmi ~ exercise, data = brfss_1)
summary(fit1)
xkabledply(fit1, title = paste("Model (factor):", format(formula(fit1)) ) )
xkablevif(fit1)
plot(fit1)

##Fitting a line to between BMI, Exercise, and Age
fit2 <- lm(bmi ~ exercise+age+self_health, data = brfss_1)
summary(fit2)
xkabledply(fit2, title = paste("Model (num):", format(formula(fit2)) ) )
xkablevif(fit2)
plot(fit2)

##Fitting a line to between BMI, Exercise, and Age
fit3 <- lm(bmi ~ exercise+age+self_reported_health+diabetes+stroke, data = brfss_1)
summary(fit3)
#xkabledply(fit3, title = paste("Model (num):", format(formula(fit3)) ) )
xkablevif(fit3)
plot(fit3)

##Interaction Fit
mixfit1 = lm(bmi~exercise+age + exercise:self_health, data = brfss_1)
summary(mixfit1)

mixfit2 <- lm(bmi ~ exercise+age+self_reported_health+diabetes*stroke, data = brfss_1)
summary(mixfit2)

## Effects of Physical Activity on BMI Category

### Chi squared test

bmi_activitytable = xtabs(~ any_exercise_last_month + bmi_category, data = brfss_1)
bmi_activitytable

chisqactivity = chisq.test(bmi_activitytable)
chisqactivity

##From the small p-value of the chi-squared test, we conclude that the BMI categories have different frequency distribution among the physical activity levels. 
##This is just another way of saying that the physical activity level and BMI category are not independent and physical activity does have an effect on BMI category. 

## Effects of Self-reported Health on BMI Category

### Chi squared test

bmi_selfhealthtable = xtabs(~ self_reported_health + bmi_category, data = brfss_1)
bmi_selfhealthtable

chisqselfhealth = chisq.test(bmi_selfhealthtable)
chisqselfhealth

##From the small p-value of the chi-squared test, we conclude that the BMI categories have different frequency distribution among the physical activity levels. 
##This is just another way of saying that the physical activity level and BMI category are not independent and physical activity does have an effect on BMI category. 


## Effects of Self-reported Health on BMI Category (Overweight/Obese)

### Chi squared test

bmi_selfhealthtable2 = xtabs(~ self_reported_health + overweight_or_obese, data = brfss_1)
bmi_selfhealthtable2

chisqselfhealth2 = chisq.test(bmi_selfhealthtable2)
chisqselfhealth2

## Effects of Physical Activity on BMI

### Two-way T-test
t.test(brfss_1$bmi ~ brfss_1$any_exercise_last_month)

##From the small p-value, we can reject the null hypothesis that there is no significant difference in whether a person's bmi and whether they have been physically active. 
##The t-test shows that there is a significant difference between average bmi and if a person's physical activity.

##From looking at the below mean body mass index of participants who reported being physically active in the last 30 days vs participants who did not report being physically active, we can observe that the average body mass indexes are different.

mean_bmi=aggregate(bmi~any_exercise_last_month,data=brfss_1,FUN = mean)
mean_bmi



## Logistic Regression Model for physical activity in the last month (bmi)
activityLogit <- glm(any_exercise_last_month ~ bmi , data = brfss_1, family = "binomial")
summary(activityLogit)

#### Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC)

## Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC) measures the true positive rate (or sensitivity) against the false positive rate (or specificity). The area-under-curve is always between 0.5 and 1. Values higher than 0.8 is considered good model fit.  

prob=predict(activityLogit, type = "response" )
brfss_1$prob=prob
h <- roc(any_exercise_last_month~prob, data=brfss_1)
auc(h) # area-under-curve prefer 0.8 or higher.
plot(h)


## Logistic Regression Model for physical activity in the last month (bmi & age)
activityLogit2 <- glm(any_exercise_last_month ~ bmi + age + self_reported_health, data = brfss_1, family = "binomial")
summary(activityLogit2)

#### Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC)

## Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC) measures the true positive rate (or sensitivity) against the false positive rate (or specificity). The area-under-curve is always between 0.5 and 1. Values higher than 0.8 is considered good model fit.  

prob2=predict(activityLogit2, type = "response" )
brfss_1$prob2=prob2
i <- roc(any_exercise_last_month~prob2, data=brfss_1)
auc(i) # area-under-curve prefer 0.8 or higher.
plot(i)

## Logistic Regression Model for bmi overweight/obese in the last month (bmi)
activityLogit3 <- glm(obese ~ exercise + age +self_health, data = brfss_1, family = "binomial")
summary(activityLogit3)

## Confusion Matrix
library(ModelMetrics)
#confusionMatrix(activityLogit3)
#default is threshold 0.5
xkabledply( confusionMatrix(actual=activityLogit3$y,predicted=activityLogit3$fitted.values), title = "Confusion matrix from Logit Model" )


brfss_3 = brfss_1[,c(10,19:25)]
str(brfss_3)

loadPkg("bestglm")
res.bestglm1 <- bestglm(Xy = brfss_3, family = binomial,
                       IC = "AIC",                 # Information criteria for
                       method = "exhaustive")
summary(res.bestglm1)

res.bestglm1$BestModels

str(brfss_1)
brfss_2 = brfss_1[,c(5,10,19:25)]
str(brfss_2)

loadPkg("bestglm")
res.bestglm <- bestglm(Xy = brfss_2, family = binomial,
                       IC = "AIC",                 # Information criteria for
                       method = "exhaustive")
summary(res.bestglm)

res.bestglm$BestModels

## Logistic Regression Model for bmi overweight/obese in the last month (bmi)
activityLogit3 <- glm(obese ~ exercise + age +self_health, data = brfss_1, family = "binomial")
summary(activityLogit3)


#### Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC)

## Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC) measures the true positive rate (or sensitivity) against the false positive rate (or specificity). The area-under-curve is always between 0.5 and 1. Values higher than 0.8 is considered good model fit.  

prob3=predict(activityLogit3, type = "response" )
brfss_1$prob3=prob3
j <- roc(obese~prob3, data=brfss_1)
auc(j) # area-under-curve prefer 0.8 or higher.
plot(j)


##McFadden for model 2 (bmi & age)
library(pscl) # use pR2( ) function to calculate McFadden statistics for model eval
activityLogit3pr2 = pR2(activityLogit3)
activityLogit3pr2
unloadPkg("pscl")

###

## Logistic Regression Model for bmi overweight/obese in the last month (bmi)
activityLogit4 <- glm(obese ~ exercise +age+self_health+diabetes+stroke+heart_attack+coronary, data = brfss_3, family = "binomial")
summary(activityLogit4)

#### Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC)

## Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC) measures the true positive rate (or sensitivity) against the false positive rate (or specificity). The area-under-curve is always between 0.5 and 1. Values higher than 0.8 is considered good model fit.  

prob4=predict(activityLogit4, type = "response" )
brfss_3$prob4=prob4
k <- roc(obese~prob4, data=brfss_3)
auc(k) # area-under-curve prefer 0.8 or higher.
plot(k)

##McFadden for model 4
library(pscl) # use pR2( ) function to calculate McFadden statistics for model eval
activityLogit4pr2 = pR2(activityLogit4)
activityLogit4pr2
unloadPkg("pscl")

## Logistic Regression Model for bmi overweight/obese in the last month (bmi)
activityLogit5 <- glm(obese ~ exercise +age+self_health+diabetes+stroke+coronary, data = brfss_3, family = "binomial")
summary(activityLogit5)

#### Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC)

## Receiver-Operator-Characteristic (ROC) curve and Area-Under-Curve (AUC) measures the true positive rate (or sensitivity) against the false positive rate (or specificity). The area-under-curve is always between 0.5 and 1. Values higher than 0.8 is considered good model fit.  

prob5=predict(activityLogit5, type = "response" )
brfss_3$prob5=prob5
k <- roc(obese~prob5, data=brfss_3)
auc(k) # area-under-curve prefer 0.8 or higher.
plot(k)

##McFadden for model 5
library(pscl) # use pR2( ) function to calculate McFadden statistics for model eval
activityLogit5pr2 = pR2(activityLogit5)
activityLogit5pr2
unloadPkg("pscl")

## Confusion Matrix
library(ModelMetrics)
print(confusionMatrix(activityLogit5))
#default is threshold 0.5
xkabledply( confusionMatrix(actual=activityLogit5$y,predicted=activityLogit5$fitted.values), title = "Confusion matrix from Logit Model" )
print

conf_mat_5 <- confusionMatrix(activityLogit5, brfss_3$obese, positive = "1")

