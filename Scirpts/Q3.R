# Step 1: Load libraries
library(tidyverse)

# Step 2: Load BRFSS dataset
brfss <- readRDS("Data/Processed/brfss_2018_2023.rds")

# Step 3: Convert overweight_or_obese from factor to character
brfss$overweight_or_obese <- as.character(brfss$overweight_or_obese)

# Step 4: Recode obesity correctly to 0/1
brfss <- brfss %>%
  mutate(
    obese = if_else(overweight_or_obese == "Yes", 1L, 0L)
  )

# Step 5: Verify obesity recode
table(brfss$obese)

# Step 6: Create pre/post COVID indicator
brfss <- brfss %>%
  mutate(
    covid_period = if_else(interview_year <= 2019, 0L, 1L)
  )

# Step 7: Create depression numeric indicator
brfss <- brfss %>%
  mutate(
    depress = if_else(depression_history == "Yes", 1L, 0L)
  )

# Step 8: Prepare clean modeling dataset
model_data <- brfss %>%
  select(depress, obese, covid_period, age_group, sex) %>%
  drop_na()

# Step 9: Fit logistic regression with interaction 
model_int_fixed <- glm(
  depress ~ obese * covid_period + age_group + sex,
  data   = model_data,
  family = binomial()
)

# Step 10: View model summary
summary(model_int_fixed)

# Step 11: Compute OR for obesity pre-COVID
OR_pre <- exp(coef(model_int_fixed)["obese"])

# Step 12: Compute OR for obesity post-COVID
OR_post <- exp(
  coef(model_int_fixed)["obese"] +
    coef(model_int_fixed)["obese:covid_period"]
)

# Step 13: Print OR results
OR_pre
OR_post

# Step 14: Create data frame for plotting
or_df <- data.frame(
  Period = c("Pre-COVID", "Post-COVID"),
  Odds_Ratio = c(as.numeric(OR_pre), as.numeric(OR_post))
)

# Step 15: Plot odds ratios
ggplot(or_df, aes(x = Period, y = Odds_Ratio)) +
  geom_bar(stat = "identity", fill = "orange") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  labs(
    title = "Change in Obesity–Depression Association Pre vs Post COVID",
    x = "Period",
    y = "Odds Ratio (Obese vs Non-obese)"
  ) +
  theme_minimal()
