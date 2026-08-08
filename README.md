# U.S. Treasury Reporting Rates of Exchange (2001–2025)

Cleaning, hypothesis testing, and regression analysis of 25 years of official
U.S. Treasury foreign exchange rate data — 18,639 quarterly observations across
five world regions.

**Tools:** R (tidyverse, ggplot2, car, e1071, lubridate, scales)

---

## Summary

**Data preparation.** Standardized column names, converted three character
columns to numeric and Date types, split a combined country/currency field into
separate variables, and derived five new columns (`country`, `currency_type`,
`region`, `year`, `quarter`). Applied a natural-log transformation to correct
severe right skew in the raw exchange rate. One record with an exchange rate of
zero (Zimbabwe, 2019 Q3) was removed as a data-quality error — `log(0)` is
undefined. Final dataset: 18,639 records, zero missing values.

**Exploratory findings.** The log-transformed distribution remains right-skewed
(mean 3.32 vs median 2.81), driven by a small group of structurally weak
currencies such as the Vietnamese Dong and Indonesian Rupiah. Regional
differences are substantial: Africa has the highest mean log exchange rate
(4.41) and Pacific the lowest (1.40, SD 1.44), while Asia shows the widest
spread (IQR 5.72) — reflecting the Kuwaiti Dinar and Vietnamese Dong sitting in
the same region. Europe is the most internally consistent (SD 2.37). Across
time, the post-2020 period has the highest mean (3.72) versus 3.05 for
2010–2019, consistent with Federal Reserve rate hikes from 2022 onward.

**Hypothesis testing.** A one-sample t-test found the mean log exchange rate of
African currencies significantly different from the global mean. Levene's test
rejected equal variances between developing (Africa, Americas, Asia) and
developed (Europe, Pacific) groups, so a one-sided Welch's t-test was used; it
confirmed significantly higher log exchange rates in developing regions.
Normality was assessed via Q-Q plots and effect sizes reported as Cohen's d.

**Regression.** Year alone is a statistically significant but practically
negligible predictor: `log_exchange_rate = -23.530 + 0.013 × year`, R² ≈ 0.1%
(p = 1.33e-05). Adding region raises explained variance to 6.1% — with Europe as
the reference group, African currencies sit 1.832 log units higher and Pacific
currencies 1.172 log units lower. The practical conclusion is that *where* a
currency is matters far more than *when* it was measured.

**Methodological caveat.** Observations are not fully independent — each country
contributes roughly 100 quarterly records over 25 years, inflating the effective
sample size and making p-values optimistic. This is flagged throughout the
scripts, and effect sizes are reported alongside significance for that reason.

---

## Data

[U.S. Treasury Reporting Rates of Exchange](https://fiscaldata.treasury.gov/datasets/treasury-reporting-rates-exchange/)
— Bureau of the Fiscal Service. Quarterly rates, 2001–2025, public domain.

## Files

| File | Purpose |
|---|---|
| `01_data_cleaning.R` | 10-step cleaning pipeline, region derivation, log transformation, 8 visualizations, and descriptive statistics by region and time period |
| `02_statistical_test.R` | Expanded region mapping, normality checks, Levene's test, one-sample and two-sample t-tests with Cohen's d |
| `03_regressional_analysis.R` | Correlation analysis, simple and multiple linear regression, model diagnostics and comparison |

Run in order — `01` writes the cleaned CSV that `02` and `03` read.

## Visualizations

All eight plots use the Wong (2011) colorblind-safe palette. Includes
histograms (overall and faceted by region), boxplots by region and quarter, a
scatterplot with fitted trend, a proportion bar plot, a violin plot, and a
region-by-year heatmap of mean log exchange rate.
