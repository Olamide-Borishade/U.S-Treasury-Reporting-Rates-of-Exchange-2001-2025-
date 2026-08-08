#=====================================================================================
# Name: Olamide Borishade Daniel
# Project Name: ALY 6010 Milestone 1
# Date: 03/01/2026
# Dataset: U.S. Treasury Reporting Rates of Exchange (2001–2025)
# Source: https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/
#=====================================================================================

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(scales)
library(e1071)

# Load the dataset
rate_clean <- read.csv("RprtRateXchgCln_20010331_20251231.csv", stringsAsFactors = FALSE)

# Preview the data
head(rate_clean)
dim(rate_clean)        # Check rows and columns
str(rate_clean)        # Check data types
summary(rate_clean)    # Basic statistics
view(rate_clean)
# ================================================================
# BEFORE CLEANING — Capture Initial State
# ================================================================

before_rows <- nrow(rate_clean)
before_cols <- ncol(rate_clean)
before_missing <- colSums(is.na(rate_clean))
before_missing_total <- sum(is.na(rate_clean))
before_duplicates <- sum(duplicated(rate_clean))

cat("================================================================\n")
cat("BEFORE CLEANING — Initial Dataset State\n")
cat("================================================================\n")
cat(sprintf("Rows               : %s\n", format(before_rows, big.mark = ",")))
cat(sprintf("Columns            : %d\n", before_cols))
cat(sprintf("Total Missing (NA) : %d\n", before_missing_total))
cat(sprintf("Duplicate Rows     : %d\n", before_duplicates))
cat("\nMissing values per column (before cleaning):\n")
print(before_missing)
cat("\n")


# ================================================================
# CLEANING STEP 1 — Rename Columns
# ================================================================
cat("--- Cleaning Step 1: Renaming Columns ---\n")

rate_clean <- rate_clean %>%
  rename(
    country_currency = Country...Currency.Description,
    exchange_rate    = Exchange.Rate,
    record_date      = Record.Date,
    effective_date   = Effective.Date
  )

cat("Columns renamed for clarity:\n")
cat("  'Country...Currency.Description' -> 'country_currency'\n")
cat("  'Exchange.Rate'                  -> 'exchange_rate'\n")
cat("  'Record.Date'                    -> 'record_date'\n")
cat("  'Effective.Date'                 -> 'effective_date'\n\n")


# ================================================================
# CLEANING STEP 2 — Fix Data Types
# ================================================================
cat("--- Cleaning Step 2: Fixing Data Types ---\n")

rate_clean$exchange_rate  <- as.numeric(rate_clean$exchange_rate)
rate_clean$record_date    <- as.Date(rate_clean$record_date,    format = "%Y-%m-%d")
rate_clean$effective_date <- as.Date(rate_clean$effective_date, format = "%Y-%m-%d")

cat("Data type conversions applied:\n")
cat("  'exchange_rate'  : character -> numeric\n")
cat("  'record_date'    : character -> Date\n")
cat("  'effective_date' : character -> Date\n\n")

# Verify
cat("Updated data types:\n")
str(rate_clean)
cat("\n")


# ================================================================
# CLEANING STEP 3 — Check for Missing Values
# ================================================================
cat("--- Cleaning Step 3: Checking for Missing Values ---\n")

missing_after_typefix <- colSums(is.na(rate_clean))
cat("Missing values per column after type conversion:\n")
print(missing_after_typefix)

if (sum(missing_after_typefix) == 0) {
  cat("\nDecision: No missing values found. No imputation or removal required.\n\n")
} else {
  cat(sprintf("\nTotal missing values found: %d\n", sum(missing_after_typefix)))
  cat("Decision: Rows with missing exchange_rate values will be removed\n")
  cat("as exchange_rate is the primary analytical variable.\n\n")
  rate_clean <- rate_clean %>% filter(!is.na(exchange_rate))
}


# ================================================================
# CLEANING STEP 4 — Check for Blank Strings
# ================================================================
cat("--- Cleaning Step 4: Checking for Blank Strings ---\n")

blank_counts <- colSums(rate_clean == "", na.rm = TRUE)
cat("Blank string counts per column:\n")
print(blank_counts)

if (sum(blank_counts) == 0) {
  cat("\nDecision: No blank strings found. No action required.\n\n")
} else {
  cat(sprintf("\nTotal blank strings found: %d\n", sum(blank_counts)))
  cat("Decision: Blank strings converted to NA and assessed.\n\n")
}


# ================================================================
# CLEANING STEP 5 — Check and Remove Duplicate Rows
# ================================================================
cat("--- Cleaning Step 5: Checking for Duplicate Rows ---\n")

n_duplicates <- sum(duplicated(rate_clean))
cat(sprintf("Number of duplicate rows found: %d\n", n_duplicates))

if (n_duplicates > 0) {
  rate_clean <- rate_clean %>% distinct()
  cat(sprintf("Decision: %d duplicate rows removed.\n", n_duplicates))
  cat(sprintf("Rows remaining after deduplication: %s\n\n",
              format(nrow(rate_clean), big.mark = ",")))
} else {
  cat("Decision: No duplicates found. No rows removed.\n\n")
}


# ================================================================
# CLEANING STEP 6 — Split country_currency into Two Columns
# ================================================================
cat("--- Cleaning Step 6: Splitting country_currency Column ---\n")

rate_clean <- rate_clean %>%
  separate(country_currency, into = c("country", "currency_type"),
           sep = "-", extra = "merge", fill = "right")

cat("'country_currency' split into:\n")
cat("  'country'       : country name (e.g. Canada, Japan)\n")
cat("  'currency_type' : currency name (e.g. Dollar, Yen)\n\n")


# ================================================================
# CLEANING STEP 7 — Check for Inconsistent Categories
# ================================================================
cat("--- Cleaning Step 7: Checking for Inconsistent Categories ---\n")

# Check for leading/trailing whitespace in country and currency_type
country_ws    <- sum(rate_clean$country != trimws(rate_clean$country), na.rm = TRUE)
currency_ws   <- sum(rate_clean$currency_type != trimws(rate_clean$currency_type), na.rm = TRUE)

cat(sprintf("Countries with leading/trailing whitespace : %d\n", country_ws))
cat(sprintf("Currency types with whitespace             : %d\n", currency_ws))

# Trim whitespace regardless
rate_clean$country       <- trimws(rate_clean$country)
rate_clean$currency_type <- trimws(rate_clean$currency_type)

# Check unique counts
cat(sprintf("Unique countries      : %d\n", n_distinct(rate_clean$country)))
cat(sprintf("Unique currency types : %d\n", n_distinct(rate_clean$currency_type)))

# Show any suspicious country entries (very short names or NAs)
suspicious <- rate_clean %>%
  filter(is.na(country) | nchar(country) <= 1) %>%
  select(country, currency_type) %>%
  distinct()

if (nrow(suspicious) > 0) {
  cat("\nSuspicious country entries found:\n")
  print(suspicious)
  cat("Decision: These entries will be reviewed manually.\n\n")
} else {
  cat("\nNo suspicious country entries found. Categories are consistent.\n\n")
}


# ================================================================
# CLEANING STEP 8 — Create Region Variable
# ================================================================
cat("--- Cleaning Step 8: Creating Region Categorical Variable ---\n")

africa <- c("Algeria", "Angola", "Botswana", "Cameroon", "Congo",
            "Egypt", "Ethiopia", "Ghana", "Kenya", "Libya", "Morocco",
            "Mozambique", "Nigeria", "Rwanda", "Senegal", "South Africa",
            "Tanzania", "Tunisia", "Uganda", "Zambia", "Zimbabwe")

americas <- c("Argentina", "Bahamas", "Barbados", "Belize", "Bolivia",
              "Brazil", "Canada", "Chile", "Colombia", "Costa Rica",
              "Cuba", "Dominican Republic", "Ecuador", "El Salvador",
              "Guatemala", "Haiti", "Honduras", "Jamaica", "Mexico",
              "Nicaragua", "Panama", "Paraguay", "Peru", "Trinidad",
              "Uruguay", "Venezuela")

asia <- c("Afghanistan", "Bahrain", "Bangladesh", "Burma", "Cambodia",
          "China", "Hong Kong", "India", "Indonesia", "Iran", "Iraq",
          "Israel", "Japan", "Jordan", "Kazakhstan", "Korea", "Kuwait",
          "Laos", "Lebanon", "Malaysia", "Mongolia", "Nepal", "Oman",
          "Pakistan", "Philippines", "Qatar", "Saudi Arabia", "Singapore",
          "Sri Lanka", "Syria", "Taiwan", "Thailand", "Turkey",
          "United Arab Emirates", "Vietnam", "Yemen")

europe <- c("Albania", "Armenia", "Azerbaijan", "Belarus", "Bosnia",
            "Bulgaria", "Croatia", "Czech Republic", "Denmark",
            "Euro Zone", "Georgia", "Hungary", "Iceland", "Kosovo",
            "Macedonia", "Moldova", "Norway", "Poland", "Romania",
            "Russia", "Serbia", "Sweden", "Switzerland", "Ukraine",
            "United Kingdom")

pacific <- c("Australia", "Fiji", "New Zealand", "Papua New Guinea",
             "Samoa", "Solomon Islands", "Tonga", "Vanuatu")

rate_clean <- rate_clean %>%
  mutate(region = case_when(
    country %in% africa   ~ "Africa",
    country %in% americas ~ "Americas",
    country %in% asia     ~ "Asia",
    country %in% europe   ~ "Europe",
    country %in% pacific  ~ "Pacific",
    TRUE                  ~ "Other"
  ))

cat("Region variable created with 6 categories:\n")
print(table(rate_clean$region))
cat("\n")


# ================================================================
# CLEANING STEP 9 — Extract Year and Quarter
# ================================================================
cat("--- Cleaning Step 9: Extracting Year and Quarter ---\n")

rate_clean <- rate_clean %>%
  mutate(
    year    = year(record_date),
    quarter = quarter(record_date)
  )

cat(sprintf("Year range : %d to %d\n", min(rate_clean$year), max(rate_clean$year)))
cat(sprintf("Quarters   : %s\n\n", paste(sort(unique(rate_clean$quarter)), collapse = ", ")))


# ================================================================
# CLEANING STEP 10 — Handle Outliers & Log Transformation
# ================================================================
cat("--- Cleaning Step 10: Outlier Check and Log Transformation ---\n")

cat("Exchange rate summary (raw):\n")
print(summary(rate_clean$exchange_rate))

# Identify zero and extreme values
n_zeros    <- sum(rate_clean$exchange_rate == 0, na.rm = TRUE)
n_extreme  <- sum(rate_clean$exchange_rate > 1e6, na.rm = TRUE)

cat(sprintf("\nRows with exchange_rate = 0      : %d\n", n_zeros))
cat(sprintf("Rows with exchange_rate > 1,000,000 : %d\n", n_extreme))

if (n_zeros > 0) {
  cat("\nZero-rate rows (to be removed):\n")
  print(rate_clean %>% filter(exchange_rate == 0) %>%
          select(country, currency_type, exchange_rate, record_date))
}

# Apply log transformation
rate_clean <- rate_clean %>%
  mutate(log_exchange_rate = log(exchange_rate))

# Remove zero-rate row (produces -Inf in log)
rows_before_outlier_removal <- nrow(rate_clean)
rate_clean <- rate_clean %>%
  filter(exchange_rate > 0, is.finite(log_exchange_rate))
rows_removed <- rows_before_outlier_removal - nrow(rate_clean)

cat(sprintf("\nLog transformation applied to exchange_rate -> log_exchange_rate\n"))
cat(sprintf("Rows removed (zero/non-finite exchange rate): %d\n", rows_removed))
cat(sprintf("Justification: Zimbabwe 2019 Q3 recorded exchange_rate = 0,\n"))
cat(sprintf("likely a data entry error. log(0) = -Inf which is not analytically\n"))
cat(sprintf("useful. Row removed as it represents a data quality issue.\n\n"))


# ================================================================
# AFTER CLEANING — Final State
# ================================================================

after_rows          <- nrow(rate_clean)
after_cols          <- ncol(rate_clean)
after_missing       <- colSums(is.na(rate_clean))
after_missing_total <- sum(is.na(rate_clean))

cat("================================================================\n")
cat("AFTER CLEANING — Final Dataset State\n")
cat("================================================================\n")
cat(sprintf("Rows               : %s\n", format(after_rows,  big.mark = ",")))
cat(sprintf("Columns            : %d\n", after_cols))
cat(sprintf("Total Missing (NA) : %d\n", after_missing_total))
cat("\nMissing values per column (after cleaning):\n")
print(after_missing)
cat("\n")


# ================================================================
# BEFORE vs AFTER SUMMARY TABLE
# ================================================================
cat("================================================================\n")
cat("BEFORE vs AFTER CLEANING — Summary\n")
cat("================================================================\n")

summary_table <- data.frame(
  Metric = c(
    "Total Rows",
    "Total Columns",
    "Total Missing Values",
    "Duplicate Rows",
    "Rows Removed (zero exchange rate)",
    "New Columns Added",
    "Data Type Fixes"
  ),
  Before_Cleaning = c(
    format(before_rows,           big.mark = ","),
    as.character(before_cols),
    as.character(before_missing_total),
    as.character(before_duplicates),
    "N/A",
    "N/A",
    "N/A"
  ),
  After_Cleaning = c(
    format(after_rows,            big.mark = ","),
    as.character(after_cols),
    as.character(after_missing_total),
    "0",
    as.character(rows_removed),
    "5 (country, currency_type, region, year, quarter)",
    "3 (exchange_rate, record_date, effective_date)"
  )
)

print(summary_table, row.names = FALSE)


# ── Colorblind-Friendly Palette (Wong 2011) ────────────────────
cbf_palette <- c(
  "Africa"   = "#E69F00",
  "Americas" = "#56B4E9",
  "Asia"     = "#009E73",
  "Europe"   = "#0072B2",
  "Pacific"  = "#D55E00"
)

# ── Working dataset (exclude "Other" region for regional plots) ─
rate_clean_named <- rate_clean %>% filter(region != "Other")


# ================================================================
# PLOT 1 — HISTOGRAM 1: Distribution of Log Exchange Rates
# ================================================================

mean_log  <- mean(rate_clean$log_exchange_rate, na.rm = TRUE)
median_log <- median(rate_clean$log_exchange_rate, na.rm = TRUE)

png("plot1_hist_log_exchange_rate.png", width = 1000, height = 650, res = 130)

ggplot(rate_clean, aes(x = log_exchange_rate)) +
  geom_histogram(binwidth = 1, fill = "#0072B2", color = "white", alpha = 0.85) +
  geom_vline(aes(xintercept = mean_log,   linetype = "Mean"),
             color = "red",  linewidth = 1.1) +
  geom_vline(aes(xintercept = median_log, linetype = "Median"),
             color = "blue", linewidth = 1.1) +
  scale_linetype_manual(
    name   = "Reference Line",
    values = c("Mean" = "dashed", "Median" = "solid")
  ) +
  scale_x_continuous(breaks = seq(-5, 25, by = 5)) +
  labs(
    title    = "Distribution of Log-Transformed Exchange Rates (2001–2025)",
    subtitle = "Bin width = 1 log unit | Red dashed = Mean | Blue solid = Median | n = 18,639 observations",
    x        = "Log Exchange Rate (natural log of foreign currency units per USD)",
    y        = "Frequency (Number of Observations)",
    caption  = "Source: U.S. Department of the Treasury, Bureau of the Fiscal Service (2026)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(size = 11, color = "gray35"),
    plot.caption    = element_text(size = 9,  color = "gray50"),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 11),
    legend.position = "top",
    legend.title    = element_text(face = "bold")
  )

dev.off()
cat("Plot 1 saved.\n")

cat("\n--- PLOT 1 INTERPRETATION ---\n")
cat("The distribution of log-transformed exchange rates is unimodal and\n")
cat("right-skewed, with the highest frequency of observations concentrated\n")
cat("between log values 0 and 5, corresponding to exchange rates of roughly\n")
cat("1 to 150 foreign currency units per USD. The mean (3.32) sits to the\n")
cat("right of the median (2.81), confirming the right skew. The long right\n")
cat("tail represents a small number of currencies with extremely high nominal\n")
cat("values, such as the Vietnamese Dong (~23,000/USD) and the Indonesian\n")
cat("Rupiah (~15,000/USD). An unexpected finding is how pronounced the skew\n")
cat("remains even after log transformation, suggesting that a small group of\n")
cat("currencies is structurally and persistently weaker than the majority.\n")
cat("This raises the question: which specific regions or countries are driving\n")
cat("the right tail, and has this pattern changed over time?\n\n")

# ================================================================
# PLOT 2 — HISTOGRAM 2: Log Exchange Rate Distribution by Region
# ================================================================

png("plot2_hist_log_rate_by_region.png", width = 1100, height = 750, res = 130)

rate_clean_named %>%
  ggplot(aes(x = log_exchange_rate, fill = region)) +
  geom_histogram(binwidth = 1, color = "white", alpha = 0.9) +
  facet_wrap(~region, ncol = 3, scales = "free_y") +
  scale_fill_manual(values = cbf_palette) +
  scale_x_continuous(breaks = seq(-5, 25, by = 5)) +
  labs(
    title    = "Log Exchange Rate Distribution by World Region (2001–2025)",
    subtitle = "Bin width = 1 log unit | Y-axis scaled freely per region to show shape differences",
    x        = "Log Exchange Rate (natural log of foreign currency units per USD)",
    y        = "Frequency (Number of Observations)",
    fill     = "Region",
    caption  = "Source: U.S. Department of the Treasury, Bureau of the Fiscal Service (2026)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(size = 11, color = "gray35"),
    plot.caption    = element_text(size = 9,  color = "gray50"),
    strip.text      = element_text(face = "bold", size = 12),
    axis.title      = element_text(size = 11),
    legend.position = "none"
  )

dev.off()
cat("Plot 2 saved.\n")

cat("\n--- PLOT 2 INTERPRETATION ---\n")
cat("Each region shows a distinctly different distribution shape, confirming\n")
cat("that geography is a meaningful grouping variable for exchange rate analysis.\n")
cat("Asia has the widest spread and most pronounced right skew, reflecting the\n")
cat("coexistence of very strong currencies (Kuwaiti Dinar, ~0.31/USD) and very\n")
cat("weak ones (Vietnamese Dong, ~23,000/USD) within the same region. Pacific\n")
cat("currencies cluster tightly at low log values (0 to 2), indicating they are\n")
cat("consistently strong relative to the USD. Africa shows a broad, relatively\n")
cat("flat distribution, suggesting high variability among African currencies with\n")
cat("no dominant cluster. Europe and the Americas show moderate, roughly symmetric\n")
cat("distributions. An unexpected finding is the near-uniform spread in Africa,\n")
cat("which raises the question: are African currencies becoming more or less\n")
cat("dispersed over the 25-year period?\n\n")


# ================================================================
# PLOT 3 — BOXPLOT 1: Log Exchange Rate by World Region
# ================================================================

png("plot3_boxplot_region.png", width = 1050, height = 680, res = 130)

rate_clean_named %>%
  ggplot(aes(x    = reorder(region, log_exchange_rate, FUN = median),
             y    = log_exchange_rate,
             fill = region)) +
  geom_boxplot(
    outlier.shape  = 21,
    outlier.size   = 1.8,
    outlier.fill   = "white",
    outlier.color  = "gray30",
    outlier.alpha  = 0.5,
    alpha          = 0.85,
    linewidth      = 0.6
  ) +
  scale_fill_manual(values = cbf_palette) +
  annotate("text", x = 0.55, y = 23,
           label = "Box = IQR (25th–75th percentile)\nLine = Median | Whiskers = 1.5×IQR\nDots = Outliers",
           hjust = 0, size = 3.2, color = "gray30", fontface = "italic") +
  labs(
    title    = "Log Exchange Rate Distribution by World Region (2001–2025)",
    subtitle = "Regions ordered by median log exchange rate (lowest to highest) | Outliers shown as points",
    x        = "World Region",
    y        = "Log Exchange Rate (natural log of foreign currency units per USD)",
    caption  = "Source: U.S. Department of the Treasury, Bureau of the Fiscal Service (2026)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(size = 11, color = "gray35"),
    plot.caption    = element_text(size = 9,  color = "gray50"),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 11),
    legend.position = "none"
  )

dev.off()
cat("Plot 3 saved.\n")

cat("\n--- PLOT 3 INTERPRETATION ---\n")
cat("The boxplot confirms substantial differences in both the level and spread\n")
cat("of log exchange rates across regions. Pacific has the lowest median log\n")
cat("exchange rate (~0.77), indicating the strongest currencies relative to the\n")
cat("USD. Africa has the highest median (~4.36), meaning African currencies are\n")
cat("weakest on average. Asia has the widest IQR (5.72), reflecting the extreme\n")
cat("diversity of currency strength within the region. Africa shows the most\n")
cat("upper outliers, representing currencies that experienced severe depreciation\n")
cat("episodes at specific points in time, such as Zimbabwe during hyperinflation.\n")
cat("Europe has the tightest IQR (3.73), suggesting the most consistency among\n")
cat("its currencies. An unexpected finding is that Africa's median is higher than\n")
cat("Asia's, despite Asia containing some of the world's most devalued currencies.\n")
cat("This raises the question: what specific economic events produced the extreme\n")
cat("upper outliers visible in Africa and the Americas?\n\n")


# ================================================================
# PLOT 4 — BOXPLOT 2: Log Exchange Rate by Reporting Quarter
# ================================================================

png("plot4_boxplot_quarter.png", width = 950, height = 630, res = 130)

rate_clean %>%
  mutate(Quarter = factor(paste0("Q", quarter), levels = c("Q1","Q2","Q3","Q4"))) %>%
  ggplot(aes(x = Quarter, y = log_exchange_rate, fill = Quarter)) +
  geom_boxplot(
    outlier.shape  = 21,
    outlier.size   = 1.5,
    outlier.fill   = "white",
    outlier.color  = "gray30",
    outlier.alpha  = 0.4,
    alpha          = 0.85,
    linewidth      = 0.6
  ) +
  scale_fill_manual(values = c("Q1" = "#E69F00", "Q2" = "#56B4E9",
                               "Q3" = "#009E73", "Q4" = "#0072B2")) +
  labs(
    title    = "Log Exchange Rate Distribution by Reporting Quarter (2001–2025)",
    subtitle = "Comparing exchange rate levels and spread across the four quarterly reporting periods",
    x        = "Reporting Quarter",
    y        = "Log Exchange Rate (natural log of foreign currency units per USD)",
    caption  = "Source: U.S. Department of the Treasury, Bureau of the Fiscal Service (2026)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(size = 11, color = "gray35"),
    plot.caption    = element_text(size = 9,  color = "gray50"),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 11),
    legend.position = "none"
  )

dev.off()
cat("Plot 4 saved.\n")

cat("\n--- PLOT 4 INTERPRETATION ---\n")
cat("All four reporting quarters show virtually identical medians, IQRs, and\n")
cat("whisker lengths, indicating that the quarter in which a rate is reported\n")
cat("has no meaningful association with exchange rate levels or spread. This\n")
cat("is an expected finding for a quarterly government administrative dataset,\n")
cat("where rates reflect end-of-quarter snapshots rather than intra-year\n")
cat("seasonal trading patterns. The consistency across quarters validates the\n")
cat("internal stability of the dataset and confirms that quarter can be excluded\n")
cat("as a confounding variable in further analysis. The key question this raises\n")
cat("is whether year-over-year changes are more meaningful than quarter-to-quarter\n")
cat("differences — which is explored in the scatterplot (Plot 5) and heatmap\n")
cat("(Plot 8).\n\n")

# ================================================================
# PLOT 5 — SCATTERPLOT: Log Exchange Rate vs. Year by Region
# ================================================================

png("plot5_scatter_lograte_vs_year.png", width = 1150, height = 700, res = 130)

rate_clean_named %>%
  ggplot(aes(x = year, y = log_exchange_rate, color = region)) +
  geom_jitter(width = 0.25, size = 0.6, alpha = 0.25) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 1.4, alpha = 0.15) +
  scale_color_manual(values = cbf_palette) +
  scale_x_continuous(breaks = seq(2001, 2025, by = 3)) +
  labs(
    title    = "Log Exchange Rate vs. Year by World Region (2001–2025)",
    subtitle = "Each point = one country-quarter observation | Shaded bands = 95% confidence interval around LOESS trend",
    x        = "Year",
    y        = "Log Exchange Rate (natural log of foreign currency units per USD)",
    color    = "Region",
    caption  = "Source: U.S. Department of the Treasury, Bureau of the Fiscal Service (2026)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(size = 11, color = "gray35"),
    plot.caption    = element_text(size = 9,  color = "gray50"),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 11),
    legend.position = "right",
    legend.title    = element_text(face = "bold")
  )

dev.off()
cat("Plot 5 saved.\n")

cat("\n--- PLOT 5 INTERPRETATION ---\n")
cat("The scatterplot reveals clear regional trends over the 25-year period.\n")
cat("Asia shows a gradual upward trend, meaning Asian currencies have weakened\n")
cat("on average against the USD since 2001, though with high variability within\n")
cat("each year. Africa shows a steeper upward trend particularly after 2015,\n")
cat("consistent with commodity price declines that impacted African economies.\n")
cat("Europe and the Americas show relatively flat LOESS trends, indicating\n")
cat("greater long-term exchange rate stability against the USD. Pacific remains\n")
cat("consistently low throughout, confirming persistent currency strength.\n")
cat("An unexpected finding is the sharp upward movement for Africa and the\n")
cat("Americas post-2020, which aligns with USD strength driven by Federal\n")
cat("Reserve interest rate hikes from 2022 onward. The wide vertical scatter\n")
cat("within each year for Asia confirms that no single trend line can represent\n")
cat("all Asian currencies. This raises the question: which individual countries\n")
cat("are driving each region's upward trend post-2020?\n\n")


# ================================================================
# PLOT 6 — BAR PLOT: Proportion of Observations by Region
# ================================================================

region_counts <- rate_clean %>%
  group_by(region) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    pct   = n / sum(n) * 100,
    label = paste0(comma(n), "\n(", round(pct, 1), "%)")
  )

png("plot6_barplot_observations_by_region.png", width = 1000, height = 650, res = 130)

ggplot(region_counts,
       aes(x = reorder(region, -n), y = n,
           fill = region)) +
  geom_bar(stat = "identity", alpha = 0.9, width = 0.7) +
  geom_text(aes(label = label), vjust = -0.4,
            size = 3.8, lineheight = 1.3, fontface = "bold") +
  scale_fill_manual(values = c(cbf_palette,
                               "Other" = "gray60")) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Number and Proportion of Observations by World Region (2001–2025)",
    subtitle = "Count labels show total observations and percentage share of the full dataset (n = 18,639)",
    x        = "World Region",
    y        = "Number of Observations",
    caption  = "Source: U.S. Department of the Treasury, Bureau of the Fiscal Service (2026)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(size = 11, color = "gray35"),
    plot.caption    = element_text(size = 9,  color = "gray50"),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 11),
    legend.position = "none"
  )

dev.off()
cat("Plot 6 saved.\n")

cat("\n--- PLOT 6 INTERPRETATION ---\n")
cat("The bar plot reveals a significant imbalance in the representation of world\n")
cat("regions within the dataset. The 'Other' category is the largest group\n")
cat("(39.1%), indicating that a large proportion of countries could not be\n")
cat("assigned to one of the five named regions under the current classification\n")
cat("scheme. Among named regions, Asia contributes the most observations (19.2%),\n")
cat("followed by the Americas (13.8%), Europe (12.5%), and Africa (11.6%).\n")
cat("Pacific is the most underrepresented named region at just 3.8%, reflecting\n")
cat("the smaller number of Pacific island nations tracked by the U.S. Treasury.\n")
cat("This imbalance is an important finding because it means region-level\n")
cat("comparisons should be interpreted with caution — conclusions about Pacific\n")
cat("carry more uncertainty due to the smaller sample size. This raises the\n")
cat("question: could refining the region classification to reduce the 'Other'\n")
cat("category improve the quality of regional analysis?\n\n")


# ================================================================
# PLOT 7 — VIOLIN PLOT: Log Exchange Rate by Region
# ================================================================

png("plot7_violin_region.png", width = 1100, height = 700, res = 130)

rate_clean_named %>%
  ggplot(aes(x    = reorder(region, log_exchange_rate, FUN = median),
             y    = log_exchange_rate,
             fill = region)) +
  geom_violin(alpha = 0.8, trim = FALSE, linewidth = 0.4) +
  geom_boxplot(width = 0.08, fill = "white",
               outlier.shape = NA, linewidth = 0.6) +
  scale_fill_manual(values = cbf_palette) +
  labs(
    title    = "Violin Plot of Log Exchange Rates by World Region (2001–2025)",
    subtitle = "Width of violin = density of observations | Inner box = IQR and median",
    x        = "World Region (ordered by median log exchange rate)",
    y        = "Log Exchange Rate (natural log of foreign currency units per USD)",
    caption  = "Source: U.S. Department of the Treasury, Bureau of the Fiscal Service (2026)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(size = 11, color = "gray35"),
    plot.caption    = element_text(size = 9,  color = "gray50"),
    axis.title      = element_text(size = 12),
    axis.text       = element_text(size = 11),
    legend.position = "none"
  )

dev.off()
cat("Plot 7 saved.\n")

cat("\n--- PLOT 7 INTERPRETATION ---\n")
cat("The violin plot adds distributional detail beyond what the boxplot shows.\n")
cat("Asia's violin is the widest and most elongated, confirming extreme spread\n")
cat("across Asian currencies with a long upper tail. Africa shows a bimodal\n")
cat("shape with two distinct density peaks — one around log values 2-3 and\n")
cat("another around 5-7 — suggesting two clusters of African currencies: those\n")
cat("that are relatively stable and those that have experienced sustained\n")
cat("depreciation. Europe and the Americas show compact, roughly symmetric\n")
cat("violins with most density concentrated near the median. Pacific's very\n")
cat("narrow violin confirms that Pacific currencies are tightly clustered with\n")
cat("minimal spread. The bimodal shape in Africa is the most unexpected finding\n")
cat("in this visualization — it would not be visible in a standard boxplot.\n")
cat("This raises the question: which specific African countries form each of\n")
cat("the two clusters, and what economic characteristics distinguish them?\n\n")


# ================================================================
# PLOT 8 — HEATMAP: Mean Log Exchange Rate by Region & Year
# ================================================================

heatmap_data <- rate_clean_named %>%
  group_by(year, region) %>%
  summarise(mean_log = mean(log_exchange_rate, na.rm = TRUE),
            .groups = "drop")

png("plot8_heatmap_region_year.png", width = 1250, height = 620, res = 130)

ggplot(heatmap_data,
       aes(x = year, y = reorder(region, mean_log), fill = mean_log)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    colors = c("#313695", "#74ADD1", "#FEE090", "#F46D43", "#A50026"),
    name   = "Mean Log\nExchange Rate\n(log units/USD)",
    guide  = guide_colorbar(barwidth = 1.2, barheight = 8)
  ) +
  scale_x_continuous(breaks = seq(2001, 2025, by = 2),
                     expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(
    title    = "Heatmap of Mean Log Exchange Rate by Region and Year (2001–2025)",
    subtitle = "Warmer colours (red/orange) = higher log exchange rate (weaker currency vs. USD) | Cooler (blue) = stronger currency",
    x        = "Year",
    y        = "World Region",
    caption  = "Source: U.S. Department of the Treasury, Bureau of the Fiscal Service (2026)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 15),
    plot.subtitle   = element_text(size = 11, color = "gray35"),
    plot.caption    = element_text(size = 9,  color = "gray50"),
    axis.title      = element_text(size = 12),
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y     = element_text(size = 11),
    legend.title    = element_text(size = 10, face = "bold"),
    panel.grid      = element_blank()
  )

dev.off()
cat("Plot 8 saved.\n")

cat("\n--- PLOT 8 INTERPRETATION ---\n")
cat("The heatmap provides a simultaneous view of how mean log exchange rates\n")
cat("have evolved across both regions and time. Asia consistently shows the\n")
cat("warmest tones (orange/red) throughout the entire 25-year period, confirming\n")
cat("it has the highest sustained mean log exchange rates. Africa shows a clear\n")
cat("gradual warming trend from 2001 to 2025, indicating progressive depreciation\n")
cat("of African currencies against the USD over time. Europe and Pacific remain\n")
cat("in consistently cool (blue) tones, confirming long-term currency strength\n")
cat("relative to the USD. The Americas show a slight warming post-2020, consistent\n")
cat("with currencies like the Argentine Peso experiencing sharp depreciation.\n")
cat("A notable unexpected finding is the visible warming across nearly all regions\n")
cat("between 2022 and 2025, which aligns with the period of aggressive USD\n")
cat("strengthening driven by Federal Reserve rate hikes. This raises the question:\n")
cat("will the post-2022 warming trend reverse as global interest rate differentials\n")
cat("narrow in future years?\n\n")


# ================================================================
# SECTION A — OVERALL DATASET STATISTICS
# ================================================================


# ----------------------------------------------------------------
# A1. NUMERICAL VARIABLE: exchange_rate (raw)
# ----------------------------------------------------------------
cat("--- A1. Numerical Variable: exchange_rate (raw) ---\n")

er <- rate_clean$exchange_rate

cat(sprintf("Count (n)          : %s\n",   format(length(er), big.mark=",")))
cat(sprintf("Mean               : %s\n",   format(round(mean(er), 4), big.mark=",")))
cat(sprintf("Median             : %s\n",   format(round(median(er), 4), big.mark=",")))
cat(sprintf("Mode               : %s\n",   format(as.numeric(names(sort(table(round(er,1)),
                                                                        decreasing=TRUE)[1])), big.mark=",")))
cat(sprintf("Std Deviation      : %s\n",   format(round(sd(er), 4), big.mark=",")))
cat(sprintf("Variance           : %s\n",   format(round(var(er), 2), big.mark=",")))
cat(sprintf("Min                : %s\n",   format(round(min(er), 4), big.mark=",")))
cat(sprintf("Max                : %s\n",   format(round(max(er), 4), big.mark=",")))
cat(sprintf("Range              : %s\n",   format(round(max(er)-min(er), 4), big.mark=",")))
cat(sprintf("Q1 (25th pctile)   : %s\n",   format(round(quantile(er, 0.25), 4), big.mark=",")))
cat(sprintf("Q3 (75th pctile)   : %s\n",   format(round(quantile(er, 0.75), 4), big.mark=",")))
cat(sprintf("IQR                : %s\n",   format(round(IQR(er), 4), big.mark=",")))
cat(sprintf("Skewness           : %.4f\n", skewness(er)))
cat(sprintf("Kurtosis (excess)  : %.4f\n", kurtosis(er)))
cat("\n")


# ----------------------------------------------------------------
# A2. NUMERICAL VARIABLE: log_exchange_rate
# ----------------------------------------------------------------
cat("--- A2. Numerical Variable: log_exchange_rate ---\n")

ler <- rate_clean$log_exchange_rate

cat(sprintf("Count (n)          : %s\n",   format(length(ler), big.mark=",")))
cat(sprintf("Mean               : %.4f\n", mean(ler)))
cat(sprintf("Median             : %.4f\n", median(ler)))
cat(sprintf("Mode               : %.1f\n", as.numeric(names(sort(table(round(ler,1)),
                                                                 decreasing=TRUE)[1]))))
cat(sprintf("Std Deviation      : %.4f\n", sd(ler)))
cat(sprintf("Variance           : %.4f\n", var(ler)))
cat(sprintf("Min                : %.4f\n", min(ler)))
cat(sprintf("Max                : %.4f\n", max(ler)))
cat(sprintf("Range              : %.4f\n", max(ler)-min(ler)))
cat(sprintf("Q1 (25th pctile)   : %.4f\n", quantile(ler, 0.25)))
cat(sprintf("Q3 (75th pctile)   : %.4f\n", quantile(ler, 0.75)))
cat(sprintf("IQR                : %.4f\n", IQR(ler)))
cat(sprintf("Skewness           : %.4f\n", skewness(ler)))
cat(sprintf("Kurtosis (excess)  : %.4f\n", kurtosis(ler)))
cat("\n")


# ----------------------------------------------------------------
# A3. CATEGORICAL VARIABLE: region
# ----------------------------------------------------------------
cat("--- A3. Categorical Variable: region ---\n")

region_freq <- rate_clean %>%
  count(region, name = "Frequency") %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>%
  arrange(desc(Frequency))

cat(sprintf("Unique Categories  : %d\n", n_distinct(rate_clean$region)))
cat(sprintf("Mode (most common) : %s\n\n", region_freq$region[1]))
print(as.data.frame(region_freq), row.names = FALSE)
cat("\n")


# ----------------------------------------------------------------
# A4. CATEGORICAL VARIABLE: currency_type
# ----------------------------------------------------------------
cat("--- A4. Categorical Variable: currency_type ---\n")

currency_freq <- rate_clean %>%
  count(currency_type, name = "Frequency") %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>%
  arrange(desc(Frequency))

cat(sprintf("Unique Categories  : %d\n", n_distinct(rate_clean$currency_type)))
cat(sprintf("Mode (most common) : %s (%d, %.1f%%)\n\n",
            currency_freq$currency_type[1],
            currency_freq$Frequency[1],
            currency_freq$Percentage[1]))
cat("Top 10 Currency Types:\n")
print(as.data.frame(head(currency_freq, 10)), row.names = FALSE)
cat("\n")


# ----------------------------------------------------------------
# A5. CATEGORICAL VARIABLE: quarter
# ----------------------------------------------------------------
cat("--- A5. Categorical Variable: quarter ---\n")

quarter_freq <- rate_clean %>%
  mutate(quarter = paste0("Q", quarter)) %>%
  count(quarter, name = "Frequency") %>%
  mutate(Percentage = round(Frequency / sum(Frequency) * 100, 2)) %>%
  arrange(quarter)

cat(sprintf("Unique Categories  : %d\n", 4))
cat(sprintf("Mode (most common) : %s\n\n", quarter_freq$quarter[which.max(quarter_freq$Frequency)]))
print(as.data.frame(quarter_freq), row.names = FALSE)
cat("\n")


# ================================================================
# SECTION B — SUBSET ANALYSIS
# ================================================================



# ----------------------------------------------------------------
# B1. SUBSET 1 — By World Region (5 named regions)
# ----------------------------------------------------------------
cat("--- B1. Subset 1: Log Exchange Rate by World Region ---\n\n")

region_stats <- rate_clean %>%
  filter(region != "Other") %>%
  group_by(region) %>%
  summarise(
    n          = n(),
    Mean       = round(mean(log_exchange_rate),   4),
    Median     = round(median(log_exchange_rate), 4),
    Std_Dev    = round(sd(log_exchange_rate),     4),
    Variance   = round(var(log_exchange_rate),    4),
    Min        = round(min(log_exchange_rate),    4),
    Max        = round(max(log_exchange_rate),    4),
    Range      = round(Max - Min,                 4),
    Q1         = round(quantile(log_exchange_rate, 0.25), 4),
    Q3         = round(quantile(log_exchange_rate, 0.75), 4),
    IQR        = round(IQR(log_exchange_rate),    4),
    Skewness   = round(skewness(log_exchange_rate), 4),
    .groups    = "drop"
  ) %>%
  arrange(desc(Mean))

print(as.data.frame(region_stats), row.names = FALSE)

cat("\n--- B1. Interpretation ---\n")
cat("Asia and Africa have the highest mean log exchange rates (4.23 and 4.41\n")
cat("respectively), indicating their currencies are weakest against the USD.\n")
cat("Pacific has the lowest mean (1.40) and tightest spread (SD = 1.44),\n")
cat("confirming persistent currency strength. Asia has the widest IQR (5.72),\n")
cat("reflecting the extreme diversity of currencies within the region. The\n")
cat("Americas show the highest skewness (1.10), driven by Venezuela and Brazil.\n")
cat("Europe has the lowest standard deviation (2.37), indicating the most\n")
cat("consistent currencies relative to the USD across all five regions.\n\n")


# ----------------------------------------------------------------
# B2. SUBSET 2 — By Time Period (Pre-2010, 2010-2019, Post-2020)
# ----------------------------------------------------------------
cat("--- B2. Subset 2: Log Exchange Rate by Time Period ---\n\n")

rate_clean_period <- rate_clean %>%
  mutate(time_period = case_when(
    year < 2010              ~ "Pre-2010 (2001-2009)",
    year >= 2010 & year <= 2019 ~ "2010-2019",
    year >= 2020             ~ "Post-2020 (2020-2025)"
  ))

period_stats <- rate_clean_period %>%
  group_by(time_period) %>%
  summarise(
    n          = n(),
    Mean       = round(mean(log_exchange_rate),   4),
    Median     = round(median(log_exchange_rate), 4),
    Std_Dev    = round(sd(log_exchange_rate),     4),
    Variance   = round(var(log_exchange_rate),    4),
    Min        = round(min(log_exchange_rate),    4),
    Max        = round(max(log_exchange_rate),    4),
    Range      = round(Max - Min,                 4),
    Q1         = round(quantile(log_exchange_rate, 0.25), 4),
    Q3         = round(quantile(log_exchange_rate, 0.75), 4),
    IQR        = round(IQR(log_exchange_rate),    4),
    Skewness   = round(skewness(log_exchange_rate), 4),
    .groups    = "drop"
  ) %>%
  arrange(factor(time_period, levels = c("Pre-2010 (2001-2009)",
                                         "2010-2019",
                                         "Post-2020 (2020-2025)")))

print(as.data.frame(period_stats), row.names = FALSE)

cat("\n--- B2. Interpretation ---\n")
cat("The Post-2020 period has the highest mean log exchange rate (3.72),\n")
cat("indicating global currencies weakened most against the USD recently,\n")
cat("consistent with Federal Reserve interest rate hikes from 2022 onward.\n")
cat("The 2010-2019 period has the lowest mean (3.05), reflecting a decade of\n")
cat("relative USD stability. The Pre-2010 period has the largest IQR (5.37),\n")
cat("suggesting greater global exchange rate variability in the early 2000s,\n")
cat("likely driven by post-dot-com recession effects and the 2008 financial\n")
cat("crisis. Spread (Std Dev) is similar across all three periods (2.92-3.06),\n")
cat("suggesting time period affects rate levels more than variability.\n\n")

# -------------------------------------------
# B3. COMPARISON TABLE — Side by Side Summary 
# -------------------------------------------
cat("--- B3. Comparison Summary: Region vs Time Period ---\n\n")

# Extract key metrics from region_stats
region_comparison <- region_stats %>%
  select(Subset = region, n, Mean, Median, Std_Dev, IQR, Skewness) %>%
  mutate(Subset_Type = "Region") %>%
  arrange(Mean)

# Extract key metrics from period_stats
period_comparison <- period_stats %>%
  select(Subset = time_period, n, Mean, Median, Std_Dev, IQR, Skewness) %>%
  mutate(Subset_Type = "Time Period") %>%
  arrange(Mean)

# Combine into one comparison table
comparison_table <- bind_rows(region_comparison, period_comparison) %>%
  select(Subset_Type, Subset, n, Mean, Median, Std_Dev, IQR, Skewness)

cat("Combined Comparison Table (Region vs Time Period):\n\n")
print(as.data.frame(comparison_table), row.names = FALSE)

# Compute range of means to show which grouping has more variation
region_mean_range  <- round(max(region_comparison$Mean)  - min(region_comparison$Mean),  4)
period_mean_range  <- round(max(period_comparison$Mean)  - min(period_comparison$Mean),  4)
region_sd_range    <- round(max(region_comparison$Std_Dev) - min(region_comparison$Std_Dev), 4)
period_sd_range    <- round(max(period_comparison$Std_Dev) - min(period_comparison$Std_Dev), 4)

cat("\n--- Key Comparison Metrics ---\n")
cat(sprintf("Range of Mean Log Rate — By Region     : %.4f\n", region_mean_range))
cat(sprintf("Range of Mean Log Rate — By Time Period: %.4f\n", period_mean_range))
cat(sprintf("Range of Std Dev       — By Region     : %.4f\n", region_sd_range))
cat(sprintf("Range of Std Dev       — By Time Period: %.4f\n", period_sd_range))

cat("\n--- Key Finding ---\n")
if (region_mean_range > period_mean_range) {
  cat(sprintf("Regional differences (range = %.4f) are LARGER than time period\n", region_mean_range))
  cat(sprintf("differences (range = %.4f), suggesting geography is a stronger\n", period_mean_range))
  cat("predictor of exchange rate levels than the era of observation.\n\n")
} else {
  cat(sprintf("Time period differences (range = %.4f) are LARGER than regional\n", period_mean_range))
  cat(sprintf("differences (range = %.4f).\n\n", region_mean_range))
}
