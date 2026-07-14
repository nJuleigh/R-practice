# 2026-07-14 R Study Note

## 학습 주제

- tibble 구조 확인
- `select()`, `filter()`, `mutate()`
- 파이프 `|>`
- `%in%`
- `if_else()`와 `case_when()`
- `SalePrice` 요약 통계
- 히스토그램, 밀도곡선, 박스플롯
- `log1p()`를 이용한 로그변환

---

## 1. 데이터 경로 작성

### 오답 1: 경로를 따옴표 없이 입력

```r
train <- read_csv(C:\Users\X1_carbon\Desktop\R Practice Data\Hause Price)
```

### 원인

파일 경로는 문자열이므로 따옴표로 감싸야 한다. 또한 폴더 경로가 아니라 실제 CSV 파일까지 지정해야 한다.

### 오답 2: Windows 역슬래시를 한 번만 사용

```r
train <- read_csv(
  "C:\Users\X1_carbon\Desktop\R Practice Data\Hause Price/train.csv"
)
```

### 원인

R 문자열 안에서 `\`는 이스케이프 문자다. `\U`가 유니코드 표현으로 해석되어 오류가 발생했다.

### 수정

```r
train <- read_csv(
  "C:/Users/X1_carbon/Desktop/R Practice Data/Hause Price/train.csv"
)
```

또는:

```r
train <- read_csv(
  "C:\\Users\\X1_carbon\\Desktop\\R Practice Data\\Hause Price\\train.csv"
)
```


---

## 2. `select()`와 파이프

다음 두 코드는 같은 의미다.

```r
select(train, SalePrice, PoolArea, GarageCond)
```

```r
train |>
  select(SalePrice, PoolArea, GarageCond)
```

파이프 `|>`는 왼쪽의 데이터를 다음 함수의 첫 번째 인수로 넘긴다. 작업을 여러 개 연결할 때 위에서 아래로 읽을 수 있다는 장점이 있다.

```r
train |>
  filter(SalePrice >= 300000) |>
  select(Id, OverallQual, GrLivArea, SalePrice)
```

---

## 3. `%in%` 철자

### 오답

```r
train |>
  filter(Neighborhood %in c("NridgHt", "StoneBr"))
```

### 원인

연산자 이름은 `%in%`이며 양쪽에 `%`가 모두 필요하다.

### 수정

```r
train |>
  filter(
    Neighborhood %in% c("NridgHt", "StoneBr")
  )
```

열의 존재 여부도 같은 연산자로 확인할 수 있다.

```r
"Neighborhood" %in% names(train)
```

---


## 4. 열 이름이 기억나지 않을 때:

```r
names(train)
glimpse(train)
```

특정 열의 존재 여부를 확인할 때:

```r
"YrSold" %in% names(train)
```

---

## 5. `train2`를 다시 만들면서 이전 열이 사라진 문제

### 진행 과정

처음에는 다음처럼 `SalePrice_10k`를 만들었다.

```r
train2 <- train |>
  mutate(SalePrice_10k = SalePrice / 10000)
```

그 후 다음 코드로 `train2`를 다시 만들었다.

```r
train2 <- train |>
  mutate(HauseAge = YrSold - YearBuilt)
```

이렇게 하면 새 `train2`는 다시 원본 `train`에서 만들어지므로 기존의 `SalePrice_10k` 열이 사라진다.

### 수정 방법 1: 기존 `train2`에서 이어서 만들기

```r
train2 <- train2 |>
  mutate(HouseAge = YrSold - YearBuilt)
```

### 수정 방법 2: 한 번의 `mutate()`에서 함께 만들기

```r
train2 <- train |>
  mutate(
    SalePrice_10k = SalePrice / 10000,
    HouseAge = YrSold - YearBuilt,
    LogSalePrice = log1p(SalePrice)
  )
```

이번 정리 스크립트에서는 두 번째 방식을 사용했다.

---

## 6. `if_else()`, `case_when()`과 가격 그룹 만들기

### 오답 0

```r
PriceLevel = if else(
  SalePrice >= 200000,
  "High",
  "Low"
)
```

### 원인

tidyverse 함수 이름은 띄어쓰기가 없는 `if_else()`다.

### 수정

```r
train2 <- train2 |>
  mutate(
    PriceLevel = if_else(
      SalePrice >= 200000,
      "High",
      "Low"
    )
  )
```

### 오답 1: 함수 이름에 공백 사용

```r
case when(...)
```

### 수정

```r
case_when(...)
```

### 오답 2: 가격 기준에서 0 하나 누락

```r
SalePrice < 15000 ~ "Low",
SalePrice < 25000 ~ "Middle",
TRUE ~ "High"
```

### 결과

House Prices의 가격은 대부분 25,000보다 크므로 거의 모든 행이 `"High"`가 되었다.

### 수정

```r
SalePrice < 150000 ~ "Low",
SalePrice < 250000 ~ "Middle",
TRUE ~ "High"
```

### 오답 3: `"TURE"`를 문자열로 작성

```r
"TURE" ~ "High"
```

### 원인

`case_when()`의 `~` 왼쪽에는 `TRUE` 또는 `FALSE`를 반환하는 논리 조건이 와야 한다. `"TURE"`는 문자형 값이며, `TRUE`의 철자도 잘못되었다.

### 수정

```r
TRUE ~ "High"
```

### 최종 코드

```r
train |>
  mutate(
    PriceGroup = case_when(
      SalePrice < 150000 ~ "Low",
      SalePrice < 250000 ~ "Middle",
      TRUE ~ "High"
    )
  )
```

`case_when()`은 위에서부터 조건을 검사한다. 두 번째 조건까지 도달한 값은 이미 첫 번째 조건을 통과했으므로, 실제 의미는 `150000 이상 250000 미만`이다.

---

## 7. `summary()` 사용법

### 오답

```r
Summary(SalePrice)
summary(SalePrice)
summary(train, SalePrice)
```

### 원인

- 함수 이름은 소문자 `summary()`다.
- `SalePrice`는 독립된 객체가 아니라 `train` 안의 열이다.
- `summary(train, SalePrice)`는 특정 열만 선택하는 문법이 아니다.

### 수정

```r
summary(train$SalePrice)
```

결과:

```text
Min.      34900
1st Qu.  129975
Median   163000
Mean     180921
3rd Qu.  214000
Max.     755000
```

평균이 중앙값보다 큰 것은 고가 주택들이 오른쪽 꼬리를 형성한다는 신호다.

tidyverse 방식으로 여러 통계를 계산할 수도 있다.

```r
train |>
  summarise(
    mean_price = mean(SalePrice),
    median_price = median(SalePrice),
    sd_price = sd(SalePrice)
  )
```

---

## 8. `aes()`와 `after_stat()`

### 오답

```r
ggplot(train, ase(x = SalePrice))
```

### 수정

```r
ggplot(train, aes(x = SalePrice))
```

`aes()`는 변수와 그래프의 시각적 요소를 연결한다. (aesthetic mapping의 약자)

### 오답

```r
after_start(density)
```

### 수정

```r
after_stat(density)
```

`geom_histogram()`의 기본 y축은 개수지만, `geom_density()`의 y축은 밀도다. 두 그래프를 같은 축에 겹치려면 히스토그램도 밀도 스케일로 바꾼다.

```r
ggplot(train, aes(x = SalePrice)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30
  ) +
  geom_density()
```

---

## 9. 박스플롯

사용한 코드:

```r
ggplot(train, aes(y = SalePrice)) +
  geom_boxplot()
```

박스플롯은 다음을 요약해 보여준다.

- 중앙값
- 제1사분위수 `Q1`
- 제3사분위수 `Q3`
- 사분위범위 `IQR = Q3 - Q1`
- 수염 밖의 잠재적 이상치

점으로 표시되는 값은 반드시 잘못된 데이터가 아니라, 다른 값들과 멀리 떨어진 관측값이다.

---

## 10. 로그 히스토그램 오답과 수정

### 오답 1: `geom_histogram()`에 변환값을 `y` 인수로 전달

```r
ggplot(train, aes(x = SalePrice)) +
  geom_histogram(
    y = log1p(SalePrice),
    bins = 30
  )
```

### 원인

히스토그램에서 분석할 변수는 `aes(x = ...)`에 지정해야 한다. `SalePrice`는 `train` 안의 열이므로 `geom_histogram()` 바깥에서 바로 찾을 수도 없다.

### 수정 방법

```r
ggplot(train, aes(x = log1p(SalePrice))) +
  geom_histogram(bins = 30)
```

또는 새 열을 먼저 만든다.

```r
train |>
  mutate(LogSalePrice = log1p(SalePrice)) |>
  ggplot(aes(x = LogSalePrice)) +
  geom_histogram(bins = 30)
```

### 오답 2: 파이프 뒤에서 `train`을 다시 전달

```r
train |>
  mutate(LogSalePrice = log1p(SalePrice)) |>
  ggplot(train, aes(x = LogSalePrice)) +
  geom_histogram(bins = 30)
```

### 원인

파이프를 통해 변환된 데이터가 이미 `ggplot()`의 첫 번째 인수로 전달된다. 여기에 `train`을 다시 쓰면 R은 `train`을 `mapping` 인수로 잘못 해석한다.

### 최종 수정

```r
train |>
  mutate(LogSalePrice = log1p(SalePrice)) |>
  ggplot(aes(x = LogSalePrice)) +
  geom_histogram(bins = 30)
```

---

## 11. `log1p()`의 의미와 해석

```r
log1p(x)
```

는 수학적으로 다음과 같다.

```r
log(1 + x)
```

장점:

- 큰 값을 압축한다.
- 오른쪽 꼬리가 긴 분포를 더 대칭적으로 만들 수 있다.
- 고가 주택 몇 개가 제곱오차에 미치는 영향을 줄일 수 있다.
- `x = 0`일 때도 결과가 0이라 안전하다.

주의할 점:

로그변환은 단순히 그래프를 예쁘게 만드는 것이 아니라 분석 기준을 바꾼다.

- 원래 가격 스케일: 절대 금액 차이를 더 직접적으로 다룸
- 로그 가격 스케일: 비율 또는 퍼센트 차이를 더 중요하게 다룸

로그 스케일 예측을 원래 가격으로 되돌릴 때는:

```r
pred_price <- expm1(pred_log)
```

를 사용한다.

---

## 정리

1. 문자열 경로는 따옴표로 감싸고 `/` 또는 `\\`를 사용한다.
2. `%in%`, `if_else()`, `case_when()`, `aes()`, `after_stat()`의 정확한 철자를 기억한다.
3. `train2 <- train |> ...`로 다시 할당하면 기존에 만든 파생변수가 사라질 수 있다.
4. `case_when()`은 위에서부터 조건을 검사하며 마지막 기본 조건은 `TRUE`다.
5. 특정 열의 요약은 `summary(train$SalePrice)`처럼 작성한다.
6. 파이프 뒤의 `ggplot()`에는 데이터를 다시 적지 않는다.
