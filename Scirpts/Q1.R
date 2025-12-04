library(tidyverse)
library(lattice)
library(corrplot)
library(car)
library(caret)
library(leaps)
library(bestglm)
library(pROC)
library(pscl)
library(rpart)
library(rpart.plot)
library(randomForest)
library(MASS)
library(glmnet)  

brfss = readRDS("../Data/Processed/brfss_2018_2023.rds")

brfss_postcovid <- subset(brfss, interview_year >= 2020)

brfss_postcovid$obese <- ifelse(brfss_postcovid$bmi >= 30, 1, 0)
brfss_postcovid$obese <- factor(brfss_postcovid$obese, labels = c("No","Yes"))

model1 <- glm(
  obese ~ age + sex + race_ethnicity + income_category +
    any_exercise_last_month + binge_drinking +
    max_drinks_30day + interview_year,
  data = brfss_postcovid,
  family = binomial
)

summary(model1)
