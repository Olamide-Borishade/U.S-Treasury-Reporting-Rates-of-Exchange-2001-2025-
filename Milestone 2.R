#=====================================================================================
# Name: Olamide Borishade Daniel
# NUID: 003153846
# Project Name: ALY 6010 Milestone 2
# Date: 03/14/2026
# Dataset: U.S. Treasury Reporting Rates of Exchange (2001–2025)
# Source: https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/
#=====================================================================================

#=============================================
# loading librariesand dataset
library(tidyverse)
library(car)        # for Levene's test later
library(e1071)      # for skewness
rate_clean <- read.csv("treasury_exchange_rates_cleaned.csv")
#=============================================

#=======================================================================
# Making corrections to my milestone 1
other_countries <- rate_clean %>%
  filter(region == "Other") %>%
  distinct(country) %>%
  arrange(country)

print(other_countries)
nrow(other_countries)

# ── EXPANDED REGION MAPPING ────────────────────────────────────

africa <- c("Algeria", "Angola", "Botswana", "Cameroon", "Congo",
            "Egypt", "Ethiopia", "Ghana", "Kenya", "Libya", "Morocco",
            "Mozambique", "Nigeria", "Rwanda", "Senegal", "South Africa",
            "Tanzania", "Tunisia", "Uganda", "Zambia", "Zimbabwe",
            # NEW ADDITIONS
            "Benin", "Burkina Faso", "Burundi", "Cape Verde",
            "Central African Republic", "Chad", "Comoros", "Cote D'Ivoire",
            "Democratic Republic Of Congo", "Djibouti", "Equatorial Guinea",
            "Eritrea", "Eswatini", "Gabon", "Gambia", "Guinea",
            "Guinea Bissau", "Lesotho", "Liberia", "Madagascar", "Malawi",
            "Mali", "Mauritania", "Mauritius", "Namibia", "Niger",
            "Sao Tome & Principe", "Seychelles", "Sierra Leone", "Somali",
            "South Sudan", "Sudan", "Swaziland", "Togo")

americas <- c("Argentina", "Bahamas", "Barbados", "Belize", "Bolivia",
              "Brazil", "Canada", "Chile", "Colombia", "Costa Rica",
              "Cuba", "Dominican Republic", "Ecuador", "El Salvador",
              "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico",
              "Nicaragua", "Panama", "Paraguay", "Peru", "Trinidad",
              "Uruguay", "Venezuela",
              # NEW ADDITIONS
              "Antigua & Barbuda", "Bermuda", "Cayman Islands", "Curacao",
              "Grenada", "Guyana", "Martinique", "Netherlands Antilles",
              "St. Lucia", "Suriname", "Trinidad & Tobago")

asia <- c("Afghanistan", "Bahrain", "Bangladesh", "Burma", "Cambodia",
          "China", "Hong Kong", "India", "Indonesia", "Iran", "Iraq",
          "Israel", "Japan", "Jordan", "Kazakhstan", "Korea", "Kuwait",
          "Laos", "Lebanon", "Malaysia", "Mongolia", "Nepal", "Oman",
          "Pakistan", "Philippines", "Qatar", "Saudi Arabia", "Singapore",
          "Sri Lanka", "Syria", "Taiwan", "Thailand", "Turkey",
          "United Arab Emirates", "Vietnam", "Yemen",
          # NEW ADDITIONS
          "Brunei", "Burma Myanmar", "Cambodia (Khmer)", "Cyprus",
          "Kyrgyzstan", "Macao", "Maldives", "Myanmar", "Tajikistan",
          "Timor", "Turkmenistan", "Uzbekistan")

europe <- c("Albania", "Armenia", "Azerbaijan", "Belarus", "Bosnia",
            "Bulgaria", "Croatia", "Czech Republic", "Denmark",
            "Euro Zone", "Georgia", "Hungary", "Iceland", "Kosovo",
            "Macedonia", "Moldova", "Norway", "Poland", "Romania",
            "Russia", "Serbia", "Sweden", "Switzerland", "Ukraine",
            "United Kingdom",
            # NEW ADDITIONS
            "Austria", "Belgium", "Bosnia Hercegovina", "Estonia",
            "Finland", "France", "Germany", "Germany Frg", "Greece",
            "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg",
            "Macedonia Fyrom", "Malta", "Maltese", "Montenegro",
            "Netherlands", "Portugal", "Republic Of North Macedonia",
            "Slovak", "Slovak Republic", "Slovakia", "Slovenia", "Spain")

pacific <- c("Australia", "Fiji", "New Zealand", "Papua New Guinea",
             "Samoa", "Solomon Islands", "Tonga", "Vanuatu",
             # NEW ADDITIONS
             "Marshall Islands", "Micronesia", "Palau",
             "Republic Of Palau", "Western Samoa")

# Apply updated region labels
rate_clean <- rate_clean %>%
  mutate(region = case_when(
    country %in% africa   ~ "Africa",
    country %in% americas ~ "Americas",
    country %in% asia     ~ "Asia",
    country %in% europe   ~ "Europe",
    country %in% pacific  ~ "Pacific",
    TRUE                  ~ "Other"
  ))

# Exclude Cross Border and Jerusalem entirely
rate_clean <- rate_clean %>%
  filter(!country %in% c("Cross Border", "Jerusalem"))

# Check new distribution
table(rate_clean$region)
#========================================================================

#================================================================
# QUESTION 1: ONE-SAMPLE T-TEST
# Is the mean log exchange rate of African
# currencies significantly different from
# the global mean?
#================================================================
rate_clean <- rate_clean %>% filter(exchange_rate > 0, 
                                    is.finite(log_exchange_rate))
african_rate <- rate_clean %>% filter(region == "Africa")
nrow(african_rate)
global_mean <- mean(rate_clean$log_exchange_rate, na.rm = TRUE)
#================================================================

#=============================================
# Testing normality with QQplot
qqnorm(african_rate$log_exchange_rate,
       main = "Q-Q Plot — African Currencies")
qqline(african_rate$log_exchange_rate)
#=============================================

#========================================================================
# one sample t-test
#------------------------------------------------------------------------
# NOTE: Observations are not fully independent — each country contributes
# approximately 100 quarterly observations over 25 years. This inflates
# the effective sample size and may increase statistical significance.
# Results should be interpreted with caution.
# -----------------------------------------------------------------------
t.test(african_rate$log_exchange_rate, 
       mu = global_mean, alternative = "two.sided")

# Cohen's d — effect size measure
cohens_d_test1 <- (mean(african_rate$log_exchange_rate) - global_mean) / 
  sd(african_rate$log_exchange_rate)
#------------------------------------------------------------------------

cat("=== ONE SAMPLE T-TEST RESULT ===\n")
cat("H0: Mean log exchange rate of African currencies = global mean\n")
cat("H1: Mean log exchange rate of African currencies != global mean\n")
cat(sprintf("Global mean (mu): %.4f\n", global_mean))
cat(sprintf("Effect size (Cohen's d): %.4f\n", cohens_d_test1))
cat("Decision: Reject H0 — p-value < 0.05\n")
cat("Conclusion: African currencies are significantly different from global mean\n\n")
#========================================================================

#=============================================
# QUESTION 2: TWO-SAMPLE T-TEST
# Do developing regions have significantly
# higher log exchange rates than developed?
#=============================================
developing <- rate_clean %>% filter(
  region %in% c("Africa", "Americas", "Asia"))
developed  <- rate_clean %>% filter(
  region %in% c("Europe", "Pacific"))
nrow(developing)
nrow(developed)

# normality test
#---------------------------------------------

# Developing group
qqnorm(developing$log_exchange_rate, 
       main = "Q-Q Plot — Developing Regions")
qqline(developing$log_exchange_rate)

# Developed group
qqnorm(developed$log_exchange_rate, 
       main = "Q-Q Plot — Developed Regions")
qqline(developed$log_exchange_rate)

# Levene's test 
#-----------------------------------------------------------------------------
# Combine both groups
combined <- rate_clean %>% 
  filter(region %in% c("Africa", "Americas", "Asia", "Europe", "Pacific")) %>%
  mutate(dev_group = ifelse(region %in% c("Africa", "Americas", "Asia"), 
                            "Developing", "Developed"))

leveneTest(log_exchange_rate ~ dev_group, data = combined)
#-----------------------------------------------------------------------------
cat("=== LEVENE'S TEST RESULT ===\n")
cat("H0: Variances are equal across developing and developed groups\n")
cat("H1: Variances are NOT equal across developing and developed groups\n")
cat("Decision: Reject H0 — p-value < 0.05\n")
cat("Conclusion: Variances are unequal — Welch's t-test will be used\n\n")
#-----------------------------------------------------------------------------
# two sample t-test
#-----------------------------------------------------------------------------
# NOTE: Observations are not fully independent — each country contributes
# approximately 100 quarterly observations over 25 years. This inflates
# the effective sample size and may increase statistical significance.
# Results should be interpreted with caution.
#-----------------------------------------------------------------------------
t.test(developing$log_exchange_rate, 
       developed$log_exchange_rate, 
       alternative = "greater", 
       var.equal = FALSE)

# Cohen's d — effect size measure
pooled_sd <- sqrt((var(developing$log_exchange_rate) * (nrow(developing)-1) + 
                     var(developed$log_exchange_rate)  * (nrow(developed)-1)) / 
                    (nrow(developing) + nrow(developed) - 2))
cohens_d_test2 <- (mean(developing$log_exchange_rate) - 
                     mean(developed$log_exchange_rate)) / pooled_sd

#-----------------------------------------------------------------------------

cat("=== TWO SAMPLE T-TEST RESULT ===\n")
cat("H0: Mean log exchange rate of developing = developed regions\n")
cat("H1: Mean log exchange rate of developing > developed regions\n")
cat(sprintf("Effect size (Cohen's d): %.4f\n", cohens_d_test2))
cat("Decision: Reject H0 — p-value < 0.05\n")
cat("Conclusion: Developing regions have significantly higher exchange rates than developed regions\n\n")
#=============================================================================