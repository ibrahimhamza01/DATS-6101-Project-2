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

###############################################
# DATA PREP
###############################################

brfss = readRDS("../Data/Processed/brfss_2018_2023.rds")

brfss_postcovid <- subset(brfss, interview_year >= 2020)

brfss_postcovid$obese <- ifelse(brfss_postcovid$bmi >= 30, 1, 0)
brfss_postcovid$obese <- factor(brfss_postcovid$obese, labels = c("No","Yes"))

###############################################
# SELECT VARIABLES FOR ANALYSIS
###############################################

vars <- brfss_postcovid %>%
  dplyr::select(obese, bmi, age, sex, race_ethnicity, income_category,
         any_exercise_last_month, binge_drinking,
         max_drinks_30day, interview_year)

###############################################
# 1. CORRELATION (PEARSON & SPEARMAN)
###############################################

numeric_vars <- vars %>% dplyr::select(age, bmi, max_drinks_30day, interview_year)

# Pearson
pearson_corr <- cor(numeric_vars, use = "complete.obs", method = "pearson")
print(pearson_corr)

# Spearman
spearman_corr <- cor(numeric_vars, use = "complete.obs", method = "spearman")
print(spearman_corr)

# Heatmap
corrplot(pearson_corr, method = "color", type = "upper")

###############################################
# 2. LATTICE PLOTS
###############################################

# Density plot by obesity status
densityplot(~ bmi | obese, data = vars)

# Boxplot income vs BMI
bwplot(bmi ~ income_category, data = vars)

# Scatterplot with regression line
xyplot(bmi ~ age,
       groups = obese,
       data = vars,
       auto.key = TRUE,
       type = c("p", "r"))



###
model1 <- glm(
  obese ~ age + sex + race_ethnicity + income_category +
    any_exercise_last_month + binge_drinking +
    max_drinks_30day + interview_year,
  data = brfss_postcovid,
  family = binomial
)

summary(model1)
