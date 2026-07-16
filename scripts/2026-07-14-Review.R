library(dplyr)

train<-read.csv("C:/Users/X1_carbon/Desktop/R Practice Data/Hause Price/train.csv")


#1. 데이터 구조 확인

dim(train)
head(train,6)
str(train)
names(train)
class(train$SalePrice)

#2. 변수 선택하기 

house_basic<- train|>
  select(SalePrice, PoolArea, GarageCond)
head(house_basic)

#3. 두 코드가 같은 결과인지 확인하기 

result_a <- select(train, SalePrice, PoolArea, GarageCond)
result_b <- train|> 
  select(SalePrice, PoolArea, GarageCond)

identical(result_a, result_b)

#4. 조건에 맞은 집만 필터링

expensive_house <- train|>
  filter(SalePrice >= 200000)
head(expensive_house)


#5. 여러 조건으로 필터링

train|> 
  filter(
    SalePrice >= 200000, 
    PoolArea>0
  )


#6. 특정 값들만 선택하기

high_quality_a <- train|>
  filter(OverallQual %in% c(8,9,10))

high_quality_b <- train|>
  filter(
    OverallQual== 8 |
    OverallQual == 9 |
    OverallQual ==10
  )

identical(high_quality_a, high_quality_b)


#7. 새 변수 만들기

train|>
  mutate(SalePrice_10k= SalePrice / 10000)

SalePrice_10k %in% names(train)

names(train)

train_price<- train|>
  mutate(
    SalePrice_10k = SalePrice /10000
  )

train_price|>
  select(SalePrice, SalePrice_10k)|>
  head()


#8. If else 사용

train_binary <- train|>
  mutate(PriceLevel = if_else( 
    SalePrice>=200000, 
    "High", 
    "Low"
  ))

table(train_binary$PriceLevel)

#9. case when

train_grouped <- train|>
  mutate(PriceGroup= case_when(
    SalePrice < 150000 ~ 'Low', 
    SalePrice <250000 ~ 'Middle',
    TRUE ~ 'High'
  ))

#10. 가격 그룹 결과 확인 
table(train_grouped$PriceGroup)
train_grouped|>
  select(SalePrice, PriceGroup)|>
  head(10)

train_grouped|>
  class(PriceGroup)

class(train_grouped$PriceGroup)

#11. 요약 통계
summary(train$SalePrice)

#12. 평균, 중앙값
mean_price <- mean(train$SalePrice)
median_price<- median(train$SalePrice)

mean_price, median_price

mean_price 
median_price

#13. 결측값을 고려한 평균
mean(train$LotFrontage)
mean(train$LotFrontage, NA=True)
sum(is.na(train$SalePrice))


#14. 선택 - 필터 - 변형

train|>
  select(SalePrice, OverallQual, PoolArea)|>
  filter( OverallQual >= 8)|>
  mutate(SalePrice_10 = SalePrice / 10000)|>
  head(10)

#15. 오류 찾기 
train |>
  select(SalePrice, PoolArea) |>
  filter(OverallQual >= 8) # OverallQual 열이 선택되지 않은 상태라서? 

train |>
  select(SalePrice, PoolArea, OverallQual) |>
  filter(OverallQual >= 8)

# OR

train |>
  select(SalePrice, PoolArea) |>
  filter(SalePrice >= 200000)


#16. 조건별 새 변수 

min(train$OverallQual)
max(train$OverallQual)
train_quality <- train|>
  mutate(QualityGroup= case_when(
    OverallQual<=4 ~ 'Low',
    OverallQual <= 7 ~ 'Middle', 
    TRUE ~'High'
  ))

table(train_quality$QualityGroup)

#17. log1p

train_log<- train|>
  mutate(LogSalePrice = log1p(SalePrice))
head(train_log)

train_log|>
  select(SalePrice, LogSalePrice)|>
  head()

#18. log vs log1p
log(100)
log1p(100)
log(101)

log(0)
log1p(0)
# log1p(x) = log(1+x)

#19. 

#20. 콘솔 기호 오류 
train |>
  select(SalePrice, PoolArea)

#21. 

review <- train|>
  select(SalePrice, OverallQual, GarageCars, PoolArea)|>
  filter(OverallQual %in% c(7,8,9,10))|>
  mutate(PriceGroup = case_when(
    SalePrice <150000 ~ 'Low',
    SalePrice <250000 ~'Middle',
    TRUE ~ 'High'
  ), LogSalePrice = log1p(SalePrice))

head(review,10)

#22. plot

ggplot(train, aes(x=SalePrice))+
  geom_histogram()


ggplot(train, aes(x=SalePrice))+
  geom_density()

ggplot(train, aes(x = SalePrice)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 30
  ) +
  geom_density()



