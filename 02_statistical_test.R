#=====================================================================================
# Name: Olamide Borishade Daniel
# NUID: 003153846
# Project Name: ALY 6010 Final Milestone 
# Date: 03/28/2026
# Dataset: U.S. Treasury Reporting Rates of Exchange (2001–2025)
# Source: https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/
#=====================================================================================

#=============================================
# loading libraries and dataset
library(tidyverse)
rate_clean <- read.csv("treasury_exchange_rates_cleaned.csv")
#=========================================================================

#=========================================================================

#=============================================
# QUESTION 1: CORRELATION ANALYSIS
# Is there a significant correlation between
# year and log exchange rate?
#=============================================
# creating scatter plots
#-------------------------------------------------------------------------
ggplot(rate_clean, aes(x = year, y = log_exchange_rate)) +
  geom_point(alpha = 0.2, size = 0.8, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2) +
  labs(
    title   = "Relationship Between Year and Log Exchange Rate (2001–2025)",
    x       = "Year",
    y       = "Log Exchange Rate (log units per USD)",
    caption = "Source: U.S. Treasury Reporting Rates of Exchange (2001–2025)"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

#---------------------------------------------------------------------------
#cortest
#---------------------------------------------------------------------------
# NOTE: Observations are not fully independent — each country contributes
# approximately 100 quarterly observations over 25 years. This inflates
# the effective sample size. Results should be interpreted with caution.

cor.test(rate_clean$year, rate_clean$log_exchange_rate)
# There was pressence of missing values after running the cortest

# Check for NAs and infinite values
sum(is.na(rate_clean$year))
sum(is.na(rate_clean$log_exchange_rate))
sum(is.infinite(rate_clean$log_exchange_rate))
sum(is.infinite(rate_clean$year))

# Cleaning the log_exchange_rate infinite value
rate_clean <- rate_clean %>% filter(is.finite(log_exchange_rate))
cat(sprintf("Rows after cleaning: %d\n", nrow(rate_clean)))

# Re-reunning the cortest
cor.test(rate_clean$year, rate_clean$log_exchange_rate)

cat("=== CORRELATION ANALYSIS RESULT ===\n")
cat("H0: rho = 0 — no significant correlation between year and log exchange rate\n")
cat("H1: rho != 0 — significant correlation exists\n")
cat(sprintf("Correlation coefficient (r): %.4f\n", 
            cor(rate_clean$year, rate_clean$log_exchange_rate)))
cat("p-value: 1.329e-05\n")
cat("Decision: Reject H0 — p-value < 0.05\n")
cat("Conclusion: Very weak positive correlation exists but is practically negligible\n\n")
#============================================================================

#========================================================
# Regression Analysis
#--------------------------------------------------------
model1 <- lm(log_exchange_rate ~ year, data = rate_clean)
summary(model1)

cat("=== SIMPLE REGRESSION RESULT ===\n")
cat("H0: beta = 0 — year does not significantly predict log exchange rate\n")
cat("H1: beta != 0 — year significantly predicts log exchange rate\n")
cat(sprintf("Equation: log_exchange_rate = -23.530 + 0.013 x year\n"))
cat(sprintf("R-squared: %.4f (%.1f%% of variation explained)\n",
            summary(model1)$r.squared,
            summary(model1)$r.squared * 100))
cat("Decision: Reject H0 — p-value < 0.05\n")
cat("Conclusion: Year is statistically significant but explains only 0.1% of variation\n\n")

# Model Diagnostic
#--------------------------------------------------------
par(mfrow = c(2, 2))
plot(model1)
#========================================================

#====================================================================================
# Multiple Regression Analysis- A bonus analysis
#------------------------------------------------------------------------------------
# Seting Europe as reference group, since it is the most stable region in the dataset
rate_clean$region <- relevel(factor(rate_clean$region), ref = "Europe")

model2<- lm(log_exchange_rate~ year+region, data=rate_clean)
summary(model2)

cat("=== MULTIPLE REGRESSION RESULT ===\n")
cat("H0: No predictors significantly predict log exchange rate\n")
cat("H1: At least one predictor significantly predicts log exchange rate\n")
cat(sprintf("R-squared: %.4f (%.1f%% of variation explained)\n",
            summary(model2)$r.squared,
            summary(model2)$r.squared * 100))
cat("Decision: Reject H0 — p-value < 2.2e-16\n")
cat("Conclusion: Year + region together explain 6.1% of variation\n")
cat("Africa coefficient: 1.832 — African currencies 1.832 log units above Europe\n")
cat("Pacific coefficient: -1.172 — Pacific currencies 1.172 log units below Europe\n\n")

# Model Diagnostic
#-----------------
par(mfrow = c(2, 2))
plot(model2)

cat("=== MODEL COMPARISON ===\n")
cat(sprintf("Simple Regression R2    : %.4f (%.1f%%)\n", 
            summary(model1)$r.squared, 
            summary(model1)$r.squared * 100))
cat(sprintf("Multiple Regression R2  : %.4f (%.1f%%)\n", 
            summary(model2)$r.squared, 
            summary(model2)$r.squared * 100))
cat(sprintf("R2 Improvement          : %.4f\n", 
            summary(model2)$r.squared - summary(model1)$r.squared))
cat("Conclusion: Adding region significantly improves model fit\n\n")
