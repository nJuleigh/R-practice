# R Day 3-4: Credit risk EDA and baseline logistic regression
# Date: 2026-07-17 ~ 2026-07-20
# Dataset: Kaggle - Give Me Some Credit (cs-training.csv)
#
# 학습 목표
# - RevolvingUtilizationOfUnsecuredLines 분포와 극단값 확인
# - 30~59일 및 60~89일 연체 변수 비교
# - 목표변수 비율을 유지하는 계층화 분할
# - MonthlyIncome 결측 대체 방법 비교
# - 로지스틱 회귀 기본 모델 생성 및 계수 해석
# - ROC-AUC 및 임계값별 성능 비교

library(tidyverse)
library(rsample)
library(pROC)

# 패키지가 설치되지 않았다면 콘솔에서 한 번만 실행한다.
# install.packages("rsample")
# install.packages("pROC")

# 사용자 환경에 맞게 경로를 수정한다.
data_path <- "C:/Users/X1_carbon/Desktop/R Practice Data/GiveMeSomeCredit/cs-training.csv"

credit <- read.csv(data_path)

names(credit)
colSums(is.na(credit))


# -----------------------------------------------------------------------------
# 1. RevolvingUtilizationOfUnsecuredLines
# -----------------------------------------------------------------------------
# 무담보 회전신용 한도 중 현재 사용 중인 비율.
# 값이 0.3이면 한도의 약 30%, 1이면 약 100%를 사용한 것으로 해석한다.

summary(credit$RevolvingUtilizationOfUnsecuredLines)

ggplot(
  credit,
  aes(x = RevolvingUtilizationOfUnsecuredLines)
) +
  geom_histogram(bins = 30) +
  theme_minimal()

quantile(
  credit$RevolvingUtilizationOfUnsecuredLines,
  probs = c(0.9, 0.95, 0.99, 0.999, 1),
  na.rm = TRUE
)

# 메모: 99% 분위수는 약 1.093인데 99.9% 분위수는 약 1,571,
# 최댓값은 50,708로 갑자기 튄다. 일부 값은 일반적인 비율로 해석하기 어렵다.

utilization_99 <- quantile(
  credit$RevolvingUtilizationOfUnsecuredLines,
  0.99,
  na.rm = TRUE
)

# 전체 히스토그램에서는 극단값 때문에 대부분의 관측치가 왼쪽에 뭉쳐 보인다.
# 분포를 자세히 보기 위해 시각화에 한해서 99% 분위수 이하만 사용한다.
credit |>
  filter(
    RevolvingUtilizationOfUnsecuredLines <= utilization_99
  ) |>
  ggplot(
    aes(x = RevolvingUtilizationOfUnsecuredLines)
  ) +
  geom_histogram(
    bins = 50,
    color = "white",
    fill = "steelblue"
  ) +
  labs(
    title = "Revolving utilization at or below the 99th percentile",
    x = "Revolving utilization",
    y = "Customer count"
  ) +
  theme_minimal()

# EDA용 구간화.
# 일반 하이픈(-)으로 통일한다. case_when()의 문자열과 factor levels가 다르면
# 해당 관측치가 NA로 변환될 수 있다.
credit <- credit |>
  mutate(
    utilization_group = case_when(
      is.na(RevolvingUtilizationOfUnsecuredLines) ~ "Missing",
      RevolvingUtilizationOfUnsecuredLines == 0 ~ "Zero",
      RevolvingUtilizationOfUnsecuredLines <= 0.25 ~ "0-25%",
      RevolvingUtilizationOfUnsecuredLines <= 0.50 ~ "25-50%",
      RevolvingUtilizationOfUnsecuredLines <= 0.75 ~ "50-75%",
      RevolvingUtilizationOfUnsecuredLines <= 1.00 ~ "75-100%",
      RevolvingUtilizationOfUnsecuredLines <= 2.00 ~ "100-200%",
      RevolvingUtilizationOfUnsecuredLines <= 10.00 ~ "200-1000%",
      TRUE ~ "Extreme (>1000%)"
    ),
    utilization_group = factor(
      utilization_group,
      levels = c(
        "Missing",
        "Zero",
        "0-25%",
        "25-50%",
        "50-75%",
        "75-100%",
        "100-200%",
        "200-1000%",
        "Extreme (>1000%)"
      )
    )
  )

table(credit$utilization_group, useNA = "always")

utilization_summary <- credit |>
  group_by(utilization_group) |>
  summarise(
    customer_count = n(),
    delinquency_count = sum(SeriousDlqin2yrs),
    delinquency_rate = mean(SeriousDlqin2yrs),
    median_utilization = median(
      RevolvingUtilizationOfUnsecuredLines,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

utilization_summary

# 메모:
# - 정상적으로 해석 가능한 0~2 범위에서는 사용률과 연체율이 함께 급증했다.
# - 100~200% 그룹의 연체율은 약 40.1%였다.
# - Extreme 그룹의 중앙값은 2,012로, 일반적인 사용률의 연장선으로 보기 어렵다.
# - 따라서 중요한 변수 후보이지만 극단값은 별도 처리 방법을 비교해야 한다.


# -----------------------------------------------------------------------------
# 2. 30~59일 연체와 60~89일 연체 비교
# -----------------------------------------------------------------------------

table(credit$NumberOfTime30.59DaysPastDueNotWorse)
table(credit$NumberOfTime60.89DaysPastDueNotWorse)

# 큰 횟수는 표본이 매우 적으므로 두 변수를 동일하게 0 / 1 / 2 / 3+로 묶는다.
# 96과 98은 실제 횟수로 보기 어려워 Special로 먼저 분리한다.
credit <- credit |>
  mutate(
    late_30_59_group = case_when(
      NumberOfTime30.59DaysPastDueNotWorse >= 90 ~ "Special",
      NumberOfTime30.59DaysPastDueNotWorse == 0 ~ "0",
      NumberOfTime30.59DaysPastDueNotWorse == 1 ~ "1",
      NumberOfTime30.59DaysPastDueNotWorse == 2 ~ "2",
      NumberOfTime30.59DaysPastDueNotWorse >= 3 ~ "3+"
    ),
    late_60_89_group = case_when(
      NumberOfTime60.89DaysPastDueNotWorse >= 90 ~ "Special",
      NumberOfTime60.89DaysPastDueNotWorse == 0 ~ "0",
      NumberOfTime60.89DaysPastDueNotWorse == 1 ~ "1",
      NumberOfTime60.89DaysPastDueNotWorse == 2 ~ "2",
      NumberOfTime60.89DaysPastDueNotWorse >= 3 ~ "3+"
    ),
    late_30_59_group = factor(
      late_30_59_group,
      levels = c("0", "1", "2", "3+", "Special")
    ),
    late_60_89_group = factor(
      late_60_89_group,
      levels = c("0", "1", "2", "3+", "Special")
    )
  )

late_30_59_summary <- credit |>
  group_by(late_30_59_group) |>
  summarise(
    customer_count = n(),
    delinquency_count = sum(SeriousDlqin2yrs),
    delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )

late_60_89_summary <- credit |>
  group_by(late_60_89_group) |>
  summarise(
    customer_count = n(),
    delinquency_count = sum(SeriousDlqin2yrs),
    delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )

late_30_59_summary
late_60_89_summary

# 메모:
# - 두 변수 모두 연체 횟수가 많아질수록 향후 2년 내 심각한 연체율이 증가했다.
# - 같은 횟수라면 60~89일 연체가 30~59일 연체보다 더 강한 위험 신호였다.
# - 60~89일 연체가 0회여도 30~59일 또는 90일 이상 연체는 있을 수 있다.

# Special이 두 변수에서 모두 269명이어서 동일 고객인지 확인한다.
special_value_check <- credit |>
  filter(
    NumberOfTime30.59DaysPastDueNotWorse >= 90 |
      NumberOfTime60.89DaysPastDueNotWorse >= 90 |
      NumberOfTimes90DaysLate >= 90
  ) |>
  count(
    NumberOfTime30.59DaysPastDueNotWorse,
    NumberOfTime60.89DaysPastDueNotWorse,
    NumberOfTimes90DaysLate
  )

special_value_check

# 메모: 우연이 아니었다. 세 연체 변수에서 동일한 고객 5명은 96,
# 동일한 고객 264명은 98이었다. 실제 횟수보다 공통 특수코드로 판단한다.


# -----------------------------------------------------------------------------
# 3. 학습·검증 데이터 계층화 분할
# -----------------------------------------------------------------------------

table(credit$SeriousDlqin2yrs)

# 0과 1의 비율 차이가 크기 때문에 목표변수의 비율을 유지하도록 계층화한다.
set.seed(2026)

credit_split <- initial_split(
  credit,
  prop = 4 / 5,
  strata = SeriousDlqin2yrs
)

train_data <- training(credit_split)

# 객체 이름은 test_data지만, 모델과 임계값 선택에 사용하므로 실제 역할은 validation이다.
test_data <- testing(credit_split)

dim(credit)
dim(train_data)
dim(test_data)

split_summary <- bind_rows(
  credit |>
    summarise(
      dataset = "Full",
      customer_count = n(),
      delinquency_count = sum(SeriousDlqin2yrs),
      delinquency_rate = mean(SeriousDlqin2yrs)
    ),
  train_data |>
    summarise(
      dataset = "Train",
      customer_count = n(),
      delinquency_count = sum(SeriousDlqin2yrs),
      delinquency_rate = mean(SeriousDlqin2yrs)
    ),
  test_data |>
    summarise(
      dataset = "Validation",
      customer_count = n(),
      delinquency_count = sum(SeriousDlqin2yrs),
      delinquency_rate = mean(SeriousDlqin2yrs)
    )
)

split_summary


# -----------------------------------------------------------------------------
# 4. MonthlyIncome 결측 대체
# -----------------------------------------------------------------------------

income_missing_summary <- bind_rows(
  train_data |>
    summarise(
      dataset = "Train",
      missing_count = sum(is.na(MonthlyIncome)),
      missing_rate = mean(is.na(MonthlyIncome)),
      income_median = median(MonthlyIncome, na.rm = TRUE)
    ),
  test_data |>
    summarise(
      dataset = "Validation",
      missing_count = sum(is.na(MonthlyIncome)),
      missing_rate = mean(is.na(MonthlyIncome)),
      income_median = median(MonthlyIncome, na.rm = TRUE)
    )
)

income_missing_summary

# 전처리 규칙은 train에서 학습하고 train과 validation에 동일하게 적용한다.
# 두 중앙값이 우연히 모두 5,400이어도 validation 중앙값을 대체에 사용하지 않는다.
income_median_train <- median(
  train_data$MonthlyIncome,
  na.rm = TRUE
)

train_data <- train_data |>
  mutate(
    MonthlyIncome_missing = if_else(is.na(MonthlyIncome), 1, 0),
    MonthlyIncome_imputed = replace_na(
      MonthlyIncome,
      income_median_train
    ),
    # 소득 1,000 증가에 따른 계수를 해석하기 위한 단위 변경이다.
    MonthlyIncome_imputed_k = MonthlyIncome_imputed / 1000,
    # 극단값의 영향을 줄이는 후보 변환이다.
    MonthlyIncome_imputed_log = log1p(MonthlyIncome_imputed)
  )

test_data <- test_data |>
  mutate(
    MonthlyIncome_missing = if_else(is.na(MonthlyIncome), 1, 0),
    MonthlyIncome_imputed = replace_na(
      MonthlyIncome,
      income_median_train
    ),
    MonthlyIncome_imputed_k = MonthlyIncome_imputed / 1000,
    MonthlyIncome_imputed_log = log1p(MonthlyIncome_imputed)
  )

imputation_check <- bind_rows(
  train_data |>
    summarise(
      dataset = "Train",
      original_missing = sum(is.na(MonthlyIncome)),
      imputed_missing = sum(is.na(MonthlyIncome_imputed)),
      missing_indicator_count = sum(MonthlyIncome_missing)
    ),
  test_data |>
    summarise(
      dataset = "Validation",
      original_missing = sum(is.na(MonthlyIncome)),
      imputed_missing = sum(is.na(MonthlyIncome_imputed)),
      missing_indicator_count = sum(MonthlyIncome_missing)
    )
)

imputation_check


# -----------------------------------------------------------------------------
# 5. 로지스틱 회귀 기본 모델
# -----------------------------------------------------------------------------
# glm(): 일반화 선형모델을 적합한다.
# 목표변수가 0/1이므로 family = binomial을 사용한다.
# binomial의 기본 link는 logit이므로 다음을 선형식으로 모델링한다.
# log(p / (1 - p)) = beta_0 + beta_1*x_1 + ... + beta_k*x_k

# 모델 1: 중앙값 대체 소득만 사용
model_median <- glm(
  SeriousDlqin2yrs ~
    age +
    MonthlyIncome_imputed_k +
    utilization_group +
    late_30_59_group +
    late_60_89_group,
  data = train_data,
  family = binomial
)

summary(model_median)

# Estimate는 확률의 변화가 아니라 로그 오즈의 변화다.
# exp(Estimate)는 오즈비다.
# 범주형 계수는 출력되지 않은 기준 그룹과 비교한다.
# - utilization_group 기준: Zero
# - late_30_59_group 기준: 0회
# - late_60_89_group 기준: 0회

odds_ratio_table <- tibble(
  term = names(coef(model_median)),
  coefficient = coef(model_median),
  odds_ratio = exp(coef(model_median))
) |>
  mutate(
    coefficient = round(coefficient, 3),
    odds_ratio = round(odds_ratio, 3)
  )

odds_ratio_table

# late_60_89_groupSpecial이 NA인 이유:
# 세 연체 변수의 Special 고객이 완전히 같아 선형 종속이 발생했다.
# R은 중복된 계수 하나를 추정에서 제외한다. 향후 공통 special indicator로 정리한다.

# p-value는 H0: beta_j = 0이 참일 때 현재 z값 이상으로 극단적인 결과가
# 나올 확률이다. p-value가 작다고 효과가 반드시 큰 것은 아니다.
# Null deviance는 절편만 사용한 모델, residual deviance는 현재 모델의 부적합도다.
# AIC는 -2*log-likelihood + 2*k이며, 같은 데이터에서 적합한 모델끼리 비교한다.


# -----------------------------------------------------------------------------
# 6. 극단적인 적합확률 경고 확인
# -----------------------------------------------------------------------------
# 경고: glm.fit: 적합된 확률값들이 0 또는 1 입니다
# 모델은 수렴했지만 소득 극단값 세 개 때문에 확률이 수치적으로 0에 가까워졌다.

model_median$converged
range(fitted(model_median))
sum(fitted(model_median) < 1e-8)
sum(fitted(model_median) > 1 - 1e-8)

extreme_probability_rows <- train_data |>
  mutate(train_probability = fitted(model_median)) |>
  filter(train_probability < 1e-8) |>
  select(
    SeriousDlqin2yrs,
    train_probability,
    age,
    MonthlyIncome,
    MonthlyIncome_imputed_k,
    utilization_group,
    late_30_59_group,
    late_60_89_group
  ) |>
  arrange(train_probability)

extreme_probability_rows

# 확인된 세 소득: 3,008,750 / 1,794,060 / 1,560,100.
# 모델이 수렴했고 해당 관측치가 세 개뿐이므로 기준 모델은 유지하되,
# 로그 변환 모델과 실제 검증 성능을 비교한다.


# -----------------------------------------------------------------------------
# 7. 소득 처리 방법 비교
# -----------------------------------------------------------------------------

# 모델 2: 중앙값 대체 + 원래 결측이었다는 indicator
model_median_indicator <- glm(
  SeriousDlqin2yrs ~
    age +
    MonthlyIncome_imputed_k +
    MonthlyIncome_missing +
    utilization_group +
    late_30_59_group +
    late_60_89_group,
  data = train_data,
  family = binomial
)

# 모델 3: 로그 소득 + 결측 indicator
model_log_indicator <- glm(
  SeriousDlqin2yrs ~
    age +
    MonthlyIncome_imputed_log +
    MonthlyIncome_missing +
    utilization_group +
    late_30_59_group +
    late_60_89_group,
  data = train_data,
  family = binomial
)

summary(model_median_indicator)$coefficients[
  "MonthlyIncome_missing",
]

AIC(
  model_median,
  model_median_indicator,
  model_log_indicator
)

# 관찰 결과
# - MonthlyIncome_missing p-value = 0.355: 다른 변수를 통제하면 추가 효과 근거가 약함.
# - AIC: Median 46081.58 / Median+indicator 46082.72 / Log+indicator 46101.22.
# - 학습 AIC 기준으로 가장 단순한 model_median을 우선 선택한다.


# -----------------------------------------------------------------------------
# 8. 검증 데이터 예측 및 ROC-AUC
# -----------------------------------------------------------------------------

test_data <- test_data |>
  mutate(
    pred_median = predict(
      model_median,
      newdata = test_data,
      type = "response"
    ),
    pred_median_indicator = predict(
      model_median_indicator,
      newdata = test_data,
      type = "response"
    ),
    pred_log_indicator = predict(
      model_log_indicator,
      newdata = test_data,
      type = "response"
    )
  )

# type = "response": 기본 출력인 로그 오즈가 아니라 0~1의 예측확률을 반환한다.
test_data |>
  summarise(
    median_na = sum(is.na(pred_median)),
    median_indicator_na = sum(is.na(pred_median_indicator)),
    log_indicator_na = sum(is.na(pred_log_indicator))
  )

roc_median <- roc(
  response = test_data$SeriousDlqin2yrs,
  predictor = test_data$pred_median,
  levels = c(0, 1),
  direction = "<"
)

roc_median_indicator <- roc(
  response = test_data$SeriousDlqin2yrs,
  predictor = test_data$pred_median_indicator,
  levels = c(0, 1),
  direction = "<"
)

roc_log_indicator <- roc(
  response = test_data$SeriousDlqin2yrs,
  predictor = test_data$pred_log_indicator,
  levels = c(0, 1),
  direction = "<"
)

# ROC의 각 점은 특정 임계값에서의 (FPR, TPR)이다.
# 이상적인 점은 (1, 1)이 아니라 (FPR, TPR) = (0, 1)이다.
# AUC는 연체 고객과 정상 고객을 하나씩 뽑았을 때 연체 고객에게 더 높은
# 점수를 줄 확률로 해석할 수 있다.
auc_summary <- tibble(
  model = c(
    "Median",
    "Median + missing indicator",
    "Log income + missing indicator"
  ),
  auc = c(
    as.numeric(auc(roc_median)),
    as.numeric(auc(roc_median_indicator)),
    as.numeric(auc(roc_log_indicator))
  )
)

auc_summary

# 검증 AUC: 0.838 / 0.838 / 0.837. 개선이 없으므로 model_median 선택.
selected_model <- model_median
selected_roc <- roc_median

plot(
  selected_roc,
  col = "steelblue",
  lwd = 2,
  main = "ROC curve - selected median-imputation model"
)


# -----------------------------------------------------------------------------
# 9. 임계값별 성능 비교
# -----------------------------------------------------------------------------

best_closest <- coords(
  selected_roc,
  x = "best",
  best.method = "closest.topleft",
  ret = c("threshold", "sensitivity", "specificity")
)

best_youden <- coords(
  selected_roc,
  x = "best",
  best.method = "youden",
  ret = c("threshold", "sensitivity", "specificity")
)

best_closest
best_youden

# closest top-left: 약 0.0589, recall 76.8%, specificity 74.9%
# Youden: 약 0.0490, recall 81.0%, specificity 70.8%
# 자동 기준은 오류의 실제 비용과 추가 심사 가능 인원을 고려하지 않는다.

threshold_summary <- tibble(
  threshold = c(0.5, 0.4, 0.3, 0.2, 0.1)
) |>
  rowwise() |>
  mutate(
    TP = sum(
      test_data$SeriousDlqin2yrs == 1 &
        test_data$pred_median >= threshold
    ),
    FN = sum(
      test_data$SeriousDlqin2yrs == 1 &
        test_data$pred_median < threshold
    ),
    FP = sum(
      test_data$SeriousDlqin2yrs == 0 &
        test_data$pred_median >= threshold
    ),
    TN = sum(
      test_data$SeriousDlqin2yrs == 0 &
        test_data$pred_median < threshold
    ),
    recall = TP / (TP + FN),
    precision = TP / (TP + FP),
    specificity = TN / (TN + FP),
    accuracy = (TP + TN) / (TP + TN + FP + FN),
    predicted_positive_rate =
      (TP + FP) / (TP + TN + FP + FN)
  ) |>
  ungroup()

threshold_summary

# 메모:
# - threshold 0.5: recall 14.9%, precision 58.0%. 연체 고객 대부분을 놓친다.
# - threshold 0.1: recall 63.3%, precision 24.3%. 더 많이 발견하지만 오경보 증가.
# - 불균형 데이터이므로 accuracy만으로 임계값을 선택하면 안 된다.
# - 최종 임계값은 연체 미탐지 비용, 정상 고객 오분류 비용,
#   추가 심사 가능한 고객 수를 고려해 정해야 한다.


# -----------------------------------------------------------------------------
# 10. 다음 분석 TODO
# -----------------------------------------------------------------------------
# [ ] 세 연체 변수의 96/98을 공통 special indicator로 정리해 singularity 제거
# [ ] NumberOfTimes90DaysLate를 정상 횟수와 Special로 분리하여 모델에 추가
# [ ] DebtRatio, NumberOfOpenCreditLinesAndLoans,
#     NumberRealEstateLoansOrLines, NumberOfDependents 추가 EDA
# [ ] NumberOfDependents 결측값을 train 기준으로 대체
# [ ] 교차검증으로 전처리·변수 선택을 더 안정적으로 비교
# [ ] ROC-AUC뿐 아니라 PR-AUC, calibration plot, log loss 확인
# [ ] 비용 기준 또는 심사 가능 인원 기준으로 임계값 결정
# [ ] Random Forest / XGBoost 등 비선형 모델과 로지스틱 회귀 비교
