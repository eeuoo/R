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


