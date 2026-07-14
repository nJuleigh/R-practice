# 2026-07-14
# R Practice: dplyr basics and SalePrice EDA
#
# Expected project structure:
# R-practice/
# ├── data/
# │   └── train.csv
# └── scripts/
#     └── 2026-07-14-dplyr-saleprice-eda.R

library(tidyverse)

# 1. Load data ------------------------------------------------------------

train <- read_csv(
  "data/train.csv",
  show_col_types = FALSE
)

# 2. Inspect the tibble ----------------------------------------------------

class(train)
dim(train)
head(train)
names(train)
glimpse(train)

# Check whether a column exists
"Neighborhood" %in% names(train)

# 3. select(): choose columns ---------------------------------------------

train |>
  select(SalePrice)

train |>
  select(SalePrice, PoolArea, GarageCond)

train |>
  select(-Id)

train |>
  select(starts_with("Sale"))

train |>
  select(
    OverallQual,
    GrLivArea,
    YearBuilt,
    Neighborhood,
    SalePrice
  )

# 4. filter(): choose rows -------------------------------------------------

train |>
  filter(SalePrice >= 300000)

train |>
  filter(SalePrice >= 300000) |>
  select(Id, OverallQual, GrLivArea, SalePrice)

train |>
  filter(OverallQual >= 8)

train |>
  filter(
    OverallQual >= 8,
    SalePrice >= 300000
  )

train |>
  filter(
    Neighborhood %in% c("NridgHt", "StoneBr", "NoRidge")
  )

# 5. mutate(): create variables -------------------------------------------

# Keep all engineered variables in one object so that later assignments
# do not accidentally remove variables created earlier.
train2 <- train |>
  mutate(
    SalePrice_10k = SalePrice / 10000,
    HouseAge = YrSold - YearBuilt,
    TotalBathrooms =
      FullBath +
      0.5 * HalfBath +
      BsmtFullBath +
      0.5 * BsmtHalfBath,
    PriceLevel = if_else(
      SalePrice >= 200000,
      "High",
      "Low"
    ),
    PriceGroup = case_when(
      SalePrice < 150000 ~ "Low",
      SalePrice < 250000 ~ "Middle",
      TRUE ~ "High"
    ),
    LogSalePrice = log1p(SalePrice)
  )

train2 |>
  select(
    SalePrice,
    SalePrice_10k,
    HouseAge,
    TotalBathrooms,
    PriceLevel,
    PriceGroup,
    LogSalePrice
  )

# 6. Combined pipeline example --------------------------------------------

expensive_houses <- train |>
  filter(SalePrice >= 300000) |>
  mutate(
    HouseAge = YrSold - YearBuilt,
    PricePerArea = SalePrice / GrLivArea
  ) |>
  select(
    Id,
    Neighborhood,
    OverallQual,
    GrLivArea,
    HouseAge,
    PricePerArea,
    SalePrice
  )

expensive_houses

# 7. SalePrice summary statistics -----------------------------------------

summary(train$SalePrice)

train |>
  summarise(
    count = n(),
    mean_price = mean(SalePrice),
    median_price = median(SalePrice),
    min_price = min(SalePrice),
    max_price = max(SalePrice),
    sd_price = sd(SalePrice)
  )

# 8. SalePrice visualizations ---------------------------------------------

# Histogram
ggplot(train, aes(x = SalePrice)) +
  geom_histogram(
    bins = 30,
    color = "black",
    fill = "skyblue"
  ) +
  labs(
    title = "Distribution of SalePrice",
    x = "Sale Price",
    y = "Count"
  )

# Density plot
ggplot(train, aes(x = SalePrice)) +
  geom_density() +
  labs(
    title = "Density of SalePrice",
    x = "Sale Price",
    y = "Density"
  )

# Histogram and density curve on the same density scale
ggplot(train, aes(x = SalePrice)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30
  ) +
  geom_density() +
  labs(
    title = "Distribution of SalePrice",
    x = "Sale Price",
    y = "Density"
  )

# Boxplot
ggplot(train, aes(y = SalePrice)) +
  geom_boxplot() +
  labs(
    title = "Boxplot of SalePrice",
    y = "Sale Price"
  )

# Log-transformed SalePrice histogram
train |>
  mutate(LogSalePrice = log1p(SalePrice)) |>
  ggplot(aes(x = LogSalePrice)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Distribution of log1p(SalePrice)",
    x = "log1p(SalePrice)",
    y = "Count"
  )