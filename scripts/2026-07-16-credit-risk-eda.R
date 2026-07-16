library(tidyverse)

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------

credit <- read.csv(
  "C:/Users/X1_carbon/Desktop/R Practice Data/GiveMeSomeCredit/cs-training.csv"
)

# Remove the CSV row-index column if present
if ("X" %in% names(credit)) {
  credit <- credit |>
    select(-X)
}

dim(credit)
names(credit)
head(credit)
str(credit)

# Target:
# SeriousDlqin2yrs = serious delinquency during the following two years (0/1)

table(credit$SeriousDlqin2yrs)
prop.table(table(credit$SeriousDlqin2yrs))
mean(credit$SeriousDlqin2yrs)

overall_summary <- credit |>
  summarise(
    customer_count = n(),
    serious_delinquency_count = sum(SeriousDlqin2yrs),
    serious_delinquency_rate = mean(SeriousDlqin2yrs)
  )

overall_summary

# ------------------------------------------------------------
# 2. Missing values
# ------------------------------------------------------------

colSums(is.na(credit))
colMeans(is.na(credit))

missing_rates_wide <- credit |>
  summarise(
    across(
      everything(),
      ~ mean(is.na(.x))
    )
  )

missing_summary <- data.frame(
  variable = names(credit),
  missing_count = colSums(is.na(credit)),
  missing_rate = colMeans(is.na(credit))
) |>
  arrange(desc(missing_rate))

missing_summary

# ------------------------------------------------------------
# 3. Age-group analysis
# ------------------------------------------------------------

credit_age <- credit |>
  mutate(
    age_group = case_when(
      age < 30 ~ "Under 30",
      age < 40 ~ "30s",
      age < 50 ~ "40s",
      age < 60 ~ "50s",
      TRUE ~ "60+"
    ),
    age_group = factor(
      age_group,
      levels = c("Under 30", "30s", "40s", "50s", "60+")
    )
  )

age_summary <- credit_age |>
  group_by(age_group) |>
  summarise(
    customer_count = n(),
    serious_delinquency_count = sum(SeriousDlqin2yrs),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  ) |>
  arrange(desc(serious_delinquency_rate))

age_summary

# ------------------------------------------------------------
# 4. MonthlyIncome distribution
# ------------------------------------------------------------

summary(credit$MonthlyIncome)

ggplot(credit, aes(x = MonthlyIncome)) +
  geom_histogram(bins = 50)

income_quantiles <- quantile(
  credit$MonthlyIncome,
  probs = c(0.90, 0.95, 0.99, 0.999, 1),
  na.rm = TRUE
)

income_quantiles

top_income_values <- credit |>
  filter(!is.na(MonthlyIncome)) |>
  arrange(desc(MonthlyIncome)) |>
  select(MonthlyIncome) |>
  head(20)

top_income_values

income_p99 <- quantile(
  credit$MonthlyIncome,
  probs = 0.99,
  na.rm = TRUE
)

income_99 <- credit |>
  filter(
    !is.na(MonthlyIncome),
    MonthlyIncome <= income_p99
  )

summary(income_99$MonthlyIncome)

income_99 |>
  ggplot(aes(x = MonthlyIncome)) +
  geom_histogram(bins = 50)

# ------------------------------------------------------------
# 5. Income groups
# ------------------------------------------------------------

credit_income <- credit |>
  mutate(
    income_group = case_when(
      is.na(MonthlyIncome) ~ "Missing",
      MonthlyIncome == 0 ~ "Zero",
      MonthlyIncome <= 3400 ~ "Low",
      MonthlyIncome <= 5400 ~ "Lower-middle",
      MonthlyIncome <= 8249 ~ "Upper-middle",
      MonthlyIncome <= 25000 ~ "High",
      TRUE ~ "Top 1%"
    ),
    income_group = factor(
      income_group,
      levels = c(
        "Missing",
        "Zero",
        "Low",
        "Lower-middle",
        "Upper-middle",
        "High",
        "Top 1%"
      )
    ),
    income_missing = if_else(is.na(MonthlyIncome), 1L, 0L)
  )

income_summary <- credit_income |>
  group_by(income_group) |>
  summarise(
    customer_count = n(),
    serious_delinquency_count = sum(SeriousDlqin2yrs),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )

income_summary

income_missing_summary <- credit_income |>
  group_by(income_missing) |>
  summarise(
    customer_count = n(),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )

income_missing_summary

income_profile_summary <- credit_income |>
  group_by(income_group) |>
  summarise(
    customer_count = n(),
    median_age = median(age, na.rm = TRUE),
    median_debt_ratio = median(DebtRatio, na.rm = TRUE),
    median_open_credit = median(
      NumberOfOpenCreditLinesAndLoans,
      na.rm = TRUE
    ),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )

income_profile_summary

# ------------------------------------------------------------
# 6. Past 90-day delinquency
# ------------------------------------------------------------

table(credit$NumberOfTimes90DaysLate)

income_late90_summary <- credit_income |>
  group_by(income_group) |>
  summarise(
    customer_count = n(),
    any_90days_late_rate = mean(
      NumberOfTimes90DaysLate > 0,
      na.rm = TRUE
    ),
    mean_90days_late_valid = mean(
      if_else(
        NumberOfTimes90DaysLate < 90,
        NumberOfTimes90DaysLate,
        NA_real_
      ),
      na.rm = TRUE
    ),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )

income_late90_summary

had_late90_summary <- credit_income |>
  mutate(
    had_90days_late = case_when(
      NumberOfTimes90DaysLate %in% c(96, 98) ~ "Special code",
      NumberOfTimes90DaysLate == 0 ~ "No",
      NumberOfTimes90DaysLate > 0 ~ "Yes"
    )
  ) |>
  group_by(had_90days_late) |>
  summarise(
    customer_count = n(),
    serious_delinquency_count = sum(SeriousDlqin2yrs),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )

had_late90_summary

credit_late90 <- credit_income |>
  mutate(
    late90_group = case_when(
      NumberOfTimes90DaysLate %in% c(96, 98) ~ "Special code",
      NumberOfTimes90DaysLate == 0 ~ "0",
      NumberOfTimes90DaysLate == 1 ~ "1",
      NumberOfTimes90DaysLate == 2 ~ "2",
      NumberOfTimes90DaysLate >= 3 ~ "3+"
    ),
    late90_group = factor(
      late90_group,
      levels = c("0", "1", "2", "3+", "Special code")
    )
  )

late90_summary <- credit_late90 |>
  group_by(late90_group) |>
  summarise(
    customer_count = n(),
    serious_delinquency_count = sum(SeriousDlqin2yrs),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )

late90_summary

income_late_summary <- credit_late90 |>
  filter(late90_group != "Special code") |>
  group_by(income_group, late90_group) |>
  summarise(
    customer_count = n(),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )

income_late_summary |>
  print(n = Inf)

income_late_summary |>
  arrange(customer_count) |>
  print(n = Inf)

# ------------------------------------------------------------
# 7. Visualization
# ------------------------------------------------------------

ggplot(
  late90_summary,
  aes(
    x = late90_group,
    y = serious_delinquency_rate
  )
) +
  geom_col() +
  labs(
    title = "Future Serious Delinquency Rate by Past 90-Day Delinquency",
    x = "Past 90-day delinquency count",
    y = "Future serious-delinquency rate"
  ) +
  theme_minimal()
