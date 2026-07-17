**Dataset:** Kaggle — Give Me Some Credit  
**File:** `cs-training.csv`

## 오늘의 학습 목표

실제 신용위험 데이터를 처음 받았을 때 어떤 순서로 확인하고 해석해야 하는지 연습.

- 목표변수 `SeriousDlqin2yrs`
- 전체 고객 수, 심각한 연체 고객 수, 연체율 계산
- 변수별 결측값 개수와 비율 확인
- `across()` 함수 
- 연령대별 심각한 연체율 비교
- `MonthlyIncome`의 분포와 극단값 조사
- 소득 구간을 설계하고 그룹별 연체율 비교
- `Missing`, `Zero`, `Top 1%` 그룹을 별도로 해석
- `NumberOfTimes90DaysLate`의 이상값 96, 98 확인
- 과거 90일 이상 연체 이력이 미래 심각한 연체와 어떤 관계가 있는지 분석
- `ggplot2`로 그룹별 연체율 시각화

---

# 1. 데이터 불러오기와 구조 확인

```r
library(tidyverse)

credit <- read.csv(
  "C:/Users/X1_carbon/Desktop/R Practice Data/GiveMeSomeCredit/cs-training.csv"
)

dim(credit)
names(credit)
head(credit)
str(credit)
```

데이터는 150,000개의 행과 12개의 열로 구성되어 있었다.

CSV를 불러오면서 생성된 `X` 열은 단순 행 번호이므로 실제 분석 변수로 사용하지 않는다.

```r
credit <- credit |>
  select(-X)
```

---

# 2. 목표변수 이해

목표변수는 다음과 같다.

```text
SeriousDlqin2yrs
```

의미는 **기준 시점 이후 향후 2년 안에 심각한 연체가 발생했는지 여부**이다.

- `0`: 향후 2년 내 심각한 연체 없음
- `1`: 향후 2년 내 심각한 연체 발생

이 데이터는 어느 기준 시점의 고객 정보와 과거 신용이력을 모은 뒤, 그 고객이 이후 2년 동안 실제로 심각한 연체를 했는지 추적하여 목표변수를 붙인 학습용 데이터라고 이해했다.

```r
table(credit$SeriousDlqin2yrs)

prop.table(
  table(credit$SeriousDlqin2yrs)
)

mean(credit$SeriousDlqin2yrs)
```

`SeriousDlqin2yrs`가 0과 1로 이루어져 있으므로 평균은 1의 비율과 같다.

```text
mean(SeriousDlqin2yrs)
= 심각한 연체 고객 수 / 전체 고객 수
```

이를 한 번에 요약하면 다음과 같다.

```r
credit |>
  summarise(
    customer_count = n(),
    serious_delinquency_count = sum(SeriousDlqin2yrs),
    serious_delinquency_rate = mean(SeriousDlqin2yrs)
  )
```

처음에는 `default_count`, `default_rate`라는 이름을 사용했지만, 이 데이터의 목표가 법적 채무불이행 전체가 아니라 **향후 2년 내 심각한 연체**이므로 `serious_delinquency_count`, `serious_delinquency_rate`가 더 정확한 이름이다.

---

# 3. 결측값 확인

## 3.1 열별 결측값 개수

```r
colSums(is.na(credit))
```

- `is.na(credit)`는 각 값이 결측값인지 확인하여 `TRUE` 또는 `FALSE`로 바꾼다.
- R에서 계산할 때 `TRUE = 1`, `FALSE = 0`으로 취급된다.
- `colSums()`는 열별 합계를 계산한다.
- 따라서 `colSums(is.na(credit))`는 각 열의 결측값 개수를 계산한다.

## 3.2 열별 결측 비율

```r
colMeans(is.na(credit))
```

`TRUE/FALSE`의 평균은 `TRUE`의 비율이므로 각 열의 결측률을 얻을 수 있다.

## 3.3 `across()`를 이용한 결측률 계산

```r
credit |>
  summarise(
    across(
      everything(),
      ~ mean(is.na(.x))
    )
  )
```

`across()`는 **여러 열에 동일한 작업을 반복 적용하는 `dplyr` 함수**이다.

기본 구조는 다음과 같다.

```text
across(열 선택, 적용할 함수)
```

- `everything()`은 모든 열을 선택한다.
- `.x`는 현재 처리 중인 열을 뜻한다.
- `~ mean(is.na(.x))`는 아래 익명 함수의 축약형이다.

```r
function(x) mean(is.na(x))
```

결과를 보기 좋게 세로형 표로 정리하였다.

```r
missing_summary <- data.frame(
  variable = names(credit),
  missing_count = colSums(is.na(credit)),
  missing_rate = colMeans(is.na(credit))
) |>
  arrange(desc(missing_rate))
```

이 결과를 통해 `MonthlyIncome`과 `NumberOfDependents`에 결측값이 존재함을 확인하였다.

---

# 4. 연령대별 분석

연령을 다음과 같이 구간화하였다.

```r
credit_age <- credit |>
  mutate(
    age_group = case_when(
      age < 30 ~ "Under 30",
      age < 40 ~ "30s",
      age < 50 ~ "40s",
      age < 60 ~ "50s",
      TRUE ~ "60+"
    )
  )
```

`case_when()`은 위에서부터 조건을 확인한다.

예를 들어 `age < 40` 조건은 이미 `age < 30`에 해당하는 사람을 제외한 뒤 적용되므로 실제로는 30세 이상 40세 미만을 뜻한다.

```r
age_summary <- credit_age |>
  group_by(age_group) |>
  summarise(
    customer_count = n(),
    serious_delinquency_count = sum(SeriousDlqin2yrs),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  ) |>
  arrange(desc(serious_delinquency_rate))
```

## 결과

| age_group | customer_count | serious_delinquency_rate |
|---|---:|---:|
| Under 30 | 8,821 | 11.7% |
| 30s | 23,183 | 10.1% |
| 40s | 34,377 | 8.37% |
| 50s | 35,301 | 6.45% |
| 60+ | 48,318 | 3.10% |

## 해석

연령이 높아질수록 향후 2년 내 심각한 연체율이 일관되게 감소하였다.

다만 그룹별 고객 수에는 차이가 있고, 이 결과는 다른 변수를 통제하지 않은 단변량 분석이다. 따라서 연령 자체가 독립적으로 연체율을 낮춘다고 단정할 수는 없다.

---

# 5. `MonthlyIncome` 분포 확인

```r
summary(credit$MonthlyIncome)
```

결과는 다음과 같았다.

```text
Min.        0
1st Qu. 3,400
Median   5,400
Mean     6,670
3rd Qu. 8,249
Max. 3,008,750
NAs     29,731
```

평균이 중앙값보다 크다는 사실만으로 강한 오른쪽 꼬리라고 확정하지 않고, 실제 히스토그램을 그려 분포를 확인하였다.

```r
ggplot(
  credit,
  aes(x = MonthlyIncome)
) +
  geom_histogram(bins = 50)
```

원본 히스토그램에서는 대부분의 값이 왼쪽에 몰려 보였다.

처음에는 실제로 저소득 구간에 값이 몰려 있는 것인지, 아니면 극단적으로 큰 소득값 때문에 x축 범위가 과도하게 커져서 대부분의 관측값이 눌려 보이는 것인지 의문을 가졌다.

그래서 상위 분위수를 확인하였다.

```r
quantile(
  credit$MonthlyIncome,
  probs = c(0.90, 0.95, 0.99, 0.999, 1),
  na.rm = TRUE
)
```

## 결과

| 분위수 | MonthlyIncome |
|---:|---:|
| 90% | 11,666.00 |
| 95% | 14,587.60 |
| 99% | 25,000.00 |
| 99.9% | 78,395.75 |
| 100% | 3,008,750.00 |

99%의 관측값은 25,000 이하였지만 최댓값은 3,008,750이었다.

따라서 원본 히스토그램이 왼쪽에 압축되어 보인 주요 원인은 극소수의 매우 큰 관측값이 x축을 크게 늘렸기 때문이라고 판단하였다.

## 상위 소득값 조사

```r
credit |>
  filter(!is.na(MonthlyIncome)) |>
  arrange(desc(MonthlyIncome)) |>
  select(MonthlyIncome) |>
  head(20)
```

## 상위 1%를 제외한 분포 확인

```r
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
```

```r
summary(income_99$MonthlyIncome)
```

결과:

```text
Min.        0
1st Qu. 3,381
Median   5,336
Mean     6,166
3rd Qu. 8,100
Max.    25,000
```

상위 1%를 제외한 후에도 평균이 중앙값보다 높아 오른쪽 비대칭은 남아 있었다. 다만 소득 데이터에서는 자연스럽게 나타날 수 있는 형태이며, 이것만으로 문제라고 볼 수는 없다.

```r
income_99 |>
  ggplot(aes(x = MonthlyIncome)) +
  geom_histogram(bins = 50)
```

상위 1%를 제외한 것은 모델링 단계에서 삭제하겠다는 의미가 아니라, 분포의 중심부를 확대해서 보기 위한 EDA였다.

---

# 6. 소득 구간 설정

소득을 구간화하는 것이 항상 최선은 아니다.

연속형 변수를 구간화하면 해석은 쉬워지지만, 구간 내부의 세부 정보가 사라진다. 따라서 EDA에서는 그룹별 위험률 패턴을 확인하기 위해 구간화하되, 모델링 단계에서는 원래 연속형 변수도 함께 고려할 필요가 있다.

이번 분석에서는 다음 사항을 고려하였다.

1. `NA`는 단순 삭제하지 않고 `Missing` 그룹으로 분리
2. 0은 실제 무소득인지 미입력값인지 확실하지 않으므로 `Zero` 그룹으로 분리
3. 일반 양수 소득은 사분위수를 기준으로 구간화
4. 99백분위수를 넘는 값은 `Top 1%` 그룹으로 분리

```r
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
    )
  )
```

## `factor()` 함수

`factor()`는 문자형 또는 숫자형 값을 범주형 변수로 바꾸는 함수이다.

```r
factor(
  범주형 변수로 바꿀 대상,
  levels = c("허용할 범주와 그 순서")
)
```

여기서는 표와 그래프가 알파벳순이 아니라 분석에서 의도한 순서대로 출력되도록 범주 순서를 지정하였다.

---

# 7. 소득 그룹별 심각한 연체율

```r
income_summary <- credit_income |>
  group_by(income_group) |>
  summarise(
    customer_count = n(),
    serious_delinquency_count = sum(SeriousDlqin2yrs),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )
```

## 결과

| income_group | customer_count | serious_delinquency_rate |
|---|---:|---:|
| Missing | 29,731 | 5.61% |
| Zero | 1,634 | 4.04% |
| Low | 28,655 | 9.50% |
| Lower-middle | 30,026 | 7.87% |
| Upper-middle | 29,895 | 6.07% |
| High | 28,891 | 4.60% |
| Top 1% | 1,168 | 5.31% |

## 해석

일반 소득 구간에서는 소득이 높아질수록 심각한 연체율이 거의 일관되게 감소하였다.

```text
Low 9.50%
→ Lower-middle 7.87%
→ Upper-middle 6.07%
→ High 4.60%
```

일반 소득 그룹의 고객 수는 약 2.8만~3만 명으로 대체로 비슷하므로 그룹 간 비교가 비교적 안정적이었다.

`Top 1%`는 고객 수가 1,168명으로 다른 그룹보다 작고 연체율이 `High`보다 약간 높았다. 따라서 고소득일수록 무조건 연체율이 낮아진다고 단정하기보다는, 최상위 소득 그룹은 별도로 확인할 필요가 있다.

`MonthlyIncome`은 유용한 후보 변수로 보이지만, `Missing`, `Zero`, 극단값을 어떻게 처리할지 추가 검토가 필요하다.

---

# 8. `Missing`과 `Zero` 그룹 추가 조사

결측 여부를 별도의 변수로 만들었다.

```r
credit_income <- credit_income |>
  mutate(
    income_missing = if_else(
      is.na(MonthlyIncome),
      1,
      0
    )
  )
```

```r
credit_income |>
  group_by(income_missing) |>
  summarise(
    customer_count = n(),
    serious_delinquency_rate = mean(SeriousDlqin2yrs)
  )
```

소득 그룹별로 나이, 부채비율, 신용계좌 및 대출 수를 함께 비교하였다.

```r
credit_income |>
  group_by(income_group) |>
  summarise(
    customer_count = n(),
    median_age = median(age, na.rm = TRUE),
    median_debt_ratio = median(DebtRatio, na.rm = TRUE),
    median_open_credit = median(
      NumberOfOpenCreditLinesAndLoans,
      na.rm = TRUE
    ),
    serious_delinquency_rate = mean(SeriousDlqin2yrs)
  )
```

## 결과

| income_group | median_age | median_debt_ratio | median_open_credit | serious_delinquency_rate |
|---|---:|---:|---:|---:|
| Missing | 57 | 1159 | 6 | 5.61% |
| Zero | 47 | 930 | 6 | 4.04% |
| Low | 47 | 0.316 | 6 | 9.50% |
| Lower-middle | 50 | 0.309 | 7 | 7.87% |
| Upper-middle | 51 | 0.304 | 9 | 6.07% |
| High | 53 | 0.268 | 10 | 4.60% |
| Top 1% | 53 | 0.116 | 10 | 5.31% |

## 해석

일반 양수 소득 구간에서는:

- 소득이 높아질수록 심각한 연체율이 감소
- 소득이 높아질수록 `median_open_credit`가 `6 → 7 → 9 → 10`으로 증가
- 소득이 높아질수록 `median_debt_ratio`는 감소

즉 고소득층은 단순히 소득만 높은 것이 아니라 보유 신용계좌나 대출 수도 더 많은 경향이 있었다.

반면 `Missing`과 `Zero` 그룹의 `median_debt_ratio`는 1159와 930으로 일반 그룹의 0.1~0.3 수준과 전혀 달랐다.

따라서 이 두 그룹은 단순한 저소득층으로 해석하기 어렵고, 소득 정보가 정상적으로 집계되지 않았거나 `DebtRatio`의 의미가 다른 방식으로 기록되었을 가능성을 고려해야 한다.

---

# 9. `NumberOfTimes90DaysLate` 분석

`NumberOfTimes90DaysLate`는 기준 시점 이전에 90일 이상 연체한 횟수를 의미한다.

```r
table(credit$NumberOfTimes90DaysLate)
```

대부분의 고객은 0이었지만 다음과 같은 특이값이 있었다.

```text
96
98
```

이 값들은 일반적인 연체 횟수로 보기 어려우므로 데이터 오류 또는 특수코드 후보로 판단하였다.

## 중앙값 대신 다른 요약을 사용한 이유

이 변수는 대부분이 0인 횟수형 변수이므로 중앙값을 계산하면 거의 모든 그룹에서 0이 나올 가능성이 높다.

따라서 다음 두 값을 확인하였다.

1. 90일 이상 연체 경험이 한 번이라도 있는 고객 비율
2. 96과 98을 제외한 평균 연체 횟수

```r
credit_income |>
  group_by(income_group) |>
  summarise(
    customer_count = n(),

    any_90days_late_rate =
      mean(NumberOfTimes90DaysLate > 0, na.rm = TRUE),

    mean_90days_late_valid =
      mean(
        if_else(
          NumberOfTimes90DaysLate < 90,
          NumberOfTimes90DaysLate,
          NA_real_
        ),
        na.rm = TRUE
      ),

    serious_delinquency_rate =
      mean(SeriousDlqin2yrs),

    .groups = "drop"
  )
```

`mean(NumberOfTimes90DaysLate > 0)`은 평균 연체 횟수가 아니다.

`NumberOfTimes90DaysLate > 0`이 `TRUE/FALSE`를 만들고, 그 평균은 `TRUE`의 비율이므로 **과거에 90일 이상 연체한 경험이 있는 고객 비율**이다.

## 결과

| income_group | any_90days_late_rate | mean_90days_late_valid | serious_delinquency_rate |
|---|---:|---:|---:|
| Missing | 5.43% | 0.0862 | 5.61% |
| Zero | 4.10% | 0.0664 | 4.04% |
| Low | 8.57% | 0.1370 | 9.50% |
| Lower-middle | 6.51% | 0.1120 | 7.87% |
| Upper-middle | 4.58% | 0.0755 | 6.07% |
| High | 2.89% | 0.0451 | 4.60% |
| Top 1% | 3.68% | 0.0523 | 5.31% |

## 해석

소득이 높아질수록 과거 90일 이상 연체 경험 비율과 평균 연체 횟수가 대체로 감소하였다.

또한 이 패턴은 향후 심각한 연체율과도 유사하게 움직였다.

따라서 `NumberOfTimes90DaysLate`는 향후 심각한 연체를 예측하는 데 꽤 유효한 변수 후보라고 판단하였다.

---

# 10. 과거 90일 이상 연체 횟수 범주화

연체 횟수를 그대로 사용하면 5회 이상부터 표본 수가 매우 작아지므로 다음처럼 범주화하였다.

```text
0
1
2
3+
Special code
```

```r
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
      levels = c(
        "0",
        "1",
        "2",
        "3+",
        "Special code"
      )
    )
  )
```

```r
late90_summary <- credit_late90 |>
  group_by(late90_group) |>
  summarise(
    customer_count = n(),
    serious_delinquency_count = sum(SeriousDlqin2yrs),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )
```

연체 횟수가 증가할수록 향후 심각한 연체율도 단조롭게 증가하였다.

따라서 이 변수는 강력한 후보 변수로 보인다.

---

# 11. 소득과 과거 연체 이력을 함께 분석

소득과 과거 90일 이상 연체를 동시에 그룹화하였다.

```r
income_late_summary <- credit_late90 |>
  filter(late90_group != "Special code") |>
  group_by(
    income_group,
    late90_group
  ) |>
  summarise(
    customer_count = n(),
    serious_delinquency_rate = mean(SeriousDlqin2yrs),
    .groups = "drop"
  )
```

```r
income_late_summary |>
  print(n = Inf)
```

이를 통해 같은 소득 그룹 안에서도 과거 연체 횟수가 많을수록 향후 심각한 연체율이 높아지는지 확인할 수 있었다.

따라서 과거 연체 이력은 단순히 소득과 같은 방향으로 움직이는 변수에 그치지 않고, 소득과 별개로도 설명력이 있을 가능성이 있다.

단, 조합별 고객 수가 너무 작은 경우에는 연체율이 불안정할 수 있으므로 다음처럼 고객 수가 작은 그룹도 확인하였다.

```r
income_late_summary |>
  arrange(customer_count) |>
  print(n = Inf)
```

---

# 12. 그래프 작성

```r
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
```

- `geom_col()`은 이미 계산된 `serious_delinquency_rate`를 막대 높이로 사용한다.
- `labs()`는 제목과 축 이름을 지정한다.
- `theme_minimal()`은 데이터는 바꾸지 않고 그래프의 디자인만 깔끔한 스타일로 변경한다.

---

# 13. 오늘 배운 함수와 개념

## `group_by()`

데이터를 특정 변수의 그룹별로 나눈 뒤 계산하도록 설정한다.

## `summarise()`

그룹 또는 전체 데이터에서 한 줄짜리 요약값을 만든다.

## `n()`

현재 그룹의 행 개수를 센다.

## `across()`

여러 열에 같은 함수를 한 번에 적용한다.

## `quantile()`

데이터를 크기순으로 정렬했을 때 특정 백분위수에 해당하는 값을 계산한다.

## `factor()`

변수를 범주형으로 바꾸고 범주의 허용값과 순서를 지정한다.

## `.groups = "drop"`

`summarise()` 이후 그룹 상태를 모두 해제한다.

이 옵션을 생략해도 출력값 자체는 같을 수 있지만, 이후 작업에서 그룹이 남아 의도하지 않은 그룹별 계산이 수행될 수 있으므로 최종 요약표에는 붙이는 것이 안전하다.

## `print(n = Inf)`

tibble에서 생략된 모든 행을 출력한다.

---

# 14. 오늘의 핵심 해석

1. 연령이 높아질수록 향후 2년 내 심각한 연체율이 감소하는 패턴이 있었다.
2. 일반 양수 소득 구간에서는 소득이 증가할수록 심각한 연체율이 감소하였다.
3. `Missing`, `Zero`, `Top 1%` 그룹은 일반 소득 그룹과 별도로 해석해야 한다.
4. `Missing`과 `Zero` 그룹의 `DebtRatio`는 일반 그룹과 전혀 다른 규모를 보였다.
5. 과거 90일 이상 연체 이력은 미래 심각한 연체율과 강한 관계를 보였다.
6. `96`, `98`은 일반 연체 횟수로 보기 어려워 특수값으로 분리하였다.
7. 그룹별 연체율은 각 그룹 내부 비율이므로 서로 더해서 1이 될 필요가 없다.
8. 현재 결과는 인과관계가 아니라 변수별 또는 그룹별 연관성을 확인한 EDA 결과이다.

---

# 15. 다음 단계

다음 학습에서는 아래 내용을 진행할 수 있다.

- `RevolvingUtilizationOfUnsecuredLines` 분포와 이상값 분석
- 30~59일 연체와 60~89일 연체 변수 비교
- `MonthlyIncome` 결측 대체 방법 비교
- 학습용/검증용 데이터 분리
- 로지스틱 회귀 기본 모델 생성
- 계수와 오즈비 해석
- ROC-AUC와 임계값에 따른 성능 비교
