##### 기술 통계 #####
data(iris)
mean(iris$Sepal.Length)


##### 회귀 분석 #####

# 1. 두 벡터 x와 y를 생성하고, x와 y 사이에 선형관계가 있다고 가정하고 lm()함수를 이용해 단순선형회귀분석을 실시하라.

set.seed(2)
x = runif(10, 0, 11)  # 평균 0, 표준편차 11의 연속균등분포로 난수 10개 추출
y = 2 + 3*x + rnorm(10,0,0.2)   # rnorm(10,0,0.2) 평균 0, 표준편차 2의 정규분포로 난수 10개 추출

dfrm <- data.frame(x, y)

# lm(선형회귀)(종속변수~독립변수, 데이터)
lm( y~x, data = dfrm )   # 회귀방정식 : y = 2.213 + 2.979x



# 2. 여러 개의 독립변수(u,v,w)와 하나의 반응변수(y)를 생성하고, 이들 간에 선행관계가 있다고 생각하며, 데이터의 다중선형회귀를 실시하라.

set.seed(2)
u <- runif(10, 0, 11)
v <- runif(10, 11, 20)
w <- runif(10, 1, 30)

y = 3 + 0.1*u + 2*v - 3*w + rnorm(10, 0, 0.1)

dfrm1 <- data.frame(y, u, v, w )

m <- lm(y~u+v+w, dfrm1)  # 회귀방정식 : y = 0.1232u + 1.9890v - 2.9978w

# 적합성 검증
summary(m)    # 결정계수, F통계량, 잔차의 표준오차 등 주요 통계량의 정보
# F통계량 : 1.038e+05,  p-value: 1.564e-14, 유의확률은 0.001보다 작으므로 추정된 회귀모형이 통계적으로 유의함을 알 수 있다. Multiple R-squared : 1(100%),	Adjusted R-squared: 1(100%) 로 회귀식이 데이터를 잘 표현하고 있음을 알 수 있다.




# 3. 다음은 식이요법 방법을 적용한 닭에 대한 데이터이다.
#install.packages("MASS")
library(MASS)

# 1번 닭에게 식이요법 방법 1을 적용한 데이터 chick
chick <- ChickWeight[ChickWeight$Diet==1,]
chick <- ChickWeight[ChickWeight$Chick==1,]
chick

# 시간 경과에 따른 닭들의 무게를 단순회귀분석
m1 = lm(weight~Time, chick)   # 회귀식 : weight = 24.465 + 7.988Time
summary(m1)
# F통계량 : 232.7,  p-value: 2.974e-08, 유의확률은  0.001보다 작다. 결정계수, 수정된 결정계수 95%로 매우 높게 보이므로 회귀모형이 통계적으로 유의미함을 볼 수 있다. 
# Time에 대한 회귀계수가 7.9879로, Time이 1 증가할 때 weight가 7.9879 증가한다고 해석할 수 있다.



# 4. 모델 진단 그래프
data(cars)
m <- lm(dist~speed, cars)
summary(m)
par(mfrow = c(2,2))
plot(m)
par(mfrow = c(1,1))


# 5. 다음과 같은 데이터가 있다. Y를 반응변수로 하고, X1, X2, X3, X4를 설명변수로 하는 선형회귀모형을 고려하고, 후진 제거법을 이해하기 위해 수동으로 변수제거를 한다.

x1 = c(7,1,11,11,7,11,3,1,2,21,1,11,10)
x2 = c(26,29,56,31,52,55,71,31,54,47,40,66,68)
x3 = c(6,15,8,8,6,9,17,22,18,4,23,9,8)
x4 = c(60,52,20,47,33,22,6,44,22,26,34,12,12)

y = c(78.5, 74.3, 104.3, 87.6, 95.9, 109.2, 102.7, 72.5, 93.1, 115.9, 83.8, 113.3, 109.4 )

df <- data.frame(x1, x2, x3, x4, y)
head(df)

#회귀분석
a <- lm(y~x1+x2+x3+x4, df)
summary(a)

#유의확률이 가장 높은 x3를 제거하고 다시 회귀분석
a <- lm(y~x1+x2+x4, df)
summary(a)   # x3를 제거한 후 결정계수 값이 높아졌다. ( 0.9736 -> 0.9764 )

#유의확률이 높은 x4를 다시 제거하고 회귀분석
a <- lm(y~x1+x2, df)
summary(a)  
# F통계량 :  229.5,  p-value: 4.407e-09 이 유의확률 0.05 아래에서 유의하고, 설명변수 x1, x2 유의확률값이 유의하므로 변수제거를 멈춘다.
# 최종 회귀식 : y = 52.57735 +  1.46831x1 + 0.66225x2 로 추정된다.


# 6. 예에서 주어진 자료와 선형회귀모형에 대해 전진 선택법을 적용하여 모형을 선택하시오. 

# step( lm(종속변수~설명변수, 데이터셋), scope = list(lower=~1, upper=~설명변수), direction = 변수선택방법)
step(lm(y~1,df), scope = list(lower =~ 1, upper =~ x1 + x2 + x3 + x4), 
     direction = "forward")

# scope 분석할 때 고려할 변수의 범위를 지정. '1'은 상수항.
# direction : 변수선택 방법. forward, backward, both.
# 최종 회귀식 : y = 71.6483 - 0.2365x4 + 1.4519x1 + 0.4161x2  



#### 다변량 분석 ####

#상관계수 검정
cor.test( c(1,2,3,4,5), c(1,0,5,7,9), method = "pearson" )
# p-value : 0.01523, p값이 0.05보다 작으므로 상관관계가 유의하다.

cor.test( c(1,2,3,4,5), c(1,0,5,7,9), method = "spear" )

#다차원척도법
loc <- cmdscale(eurodist)
x <- loc[ ,1]
y <- loc[ ,2]

plot(x, y, type = 'n', main = 'eurodist')
text(x, y, rownames(loc), cex = .8, col = 'red')
abline(v = 0, h = 0)

#주성분 분석
library(datasets)
data("USArrests")

fit <- prcomp(USArrests, scale = TRUE)   # 주성분 분석 함수, sclae = T 표준화
summary(fit)
# 첫번째 주성분  PC1이 전체의 62%로 첫번째만 수용했을 때 정보손실은 38%.

fit$rotation   # 주성분들의 로딩 벡터

plot(fit, type = "lines")
# 스크리도표 : 고유값이 1보다 크며 하나의 요인이 변수 1개 이상의 분신을 설명한다는 의미이다. 1보다 작으면 요인으로서 의미가 없다는 뜻. 

biplot(fit)



#### 시계열 예측 #####

#분해 시계열 

# 1. R에 기본적으로 내장되어 있는 Nile 데이터이다. 1871년부터 1970년까지 아스완 댐에서 측정한 나일강의 연간 유입량에 대한 시계열 데이터이다. 
Nile
plot(Nile)

Nile.diff1 <- diff(Nile, differences = 1)
plot(Nile.diff1)

Nile.diff2 <- diff(Nile, differences = 2)
plot(Nile.diff2)

# 2. ldeaths 1974년부터 1979년까지 영국 내의 월별 폐질환 사망자에 관한 시열 자료이다.
ldeaths
plot(ldeaths)

#decompose() 함수로 시계열 자료를 4가지 요인으로 분해
ldeaths.decompose <- decompose(ldeaths)
ldeaths.decompose$seasonal
plot(ldeaths.decompose)

ldeaths.decompose.adj <- ldeaths - ldeaths.decompose$seasonal
plot(ldeaths.decompose.adj)


#### ARIMA 모델 적합 및 결정 #####

acf(Nile.diff2, lag.max = 20)
acf(Nile.diff2, lag.max = 20, plot = FALSE)

#부분자기상환함수 pacf()
pacf(Nile.diff2, lag.max = 20)
pacf(Nile.diff2, lag.max = 20, plot = FALSE)

#install.packages("forecast")
library(forecast)

auto.arima(Nile)
Nile.arima <-  arima(Nile, order = c(1,1,1))
#데이터에 모형을 적합한 후, forecast 함수를 이용하여 미래의 수치값을 예측

Nile.forecast <- forecast(Nile.arima, h = 50)   # h : 연도, 50년의 나일강 데이터
plot(Nile.forecast)
