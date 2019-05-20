##### 로지스틱 회귀모형 ######

# 온도 변화에 따른 거북이 알의 수멋과 암컷의 결과
x <- c(27.2, 27.7, 28.3, 28.4, 29.9)
male <- c(2, 17, 26, 19, 27)
female <- c(25, 7, 4, 8, 1)
total = male + female
pmale <- male/total

# 1. 온도를 독립변수로 수컷 비율을 종속변수로 하여 단순선형회귀식을 추정하시오.
z <- lm(pmale~x)
summary(z)   # 추정 회귀식 : 수컷 비율 = -6.9020 + 0.2673*온도

p <- coefficients(z)[1] + coefficients(z)[2]*x    # 결괏값이 1보다 큰 게 존재.


# 2. 로짓변환된 값 log(pmale/(1-pmale))를 종속변수로 한 단순선형회귀식을 추정하시오. 
logit <- log(pmale/(1-pmale))
z1 <- lm(logit~x)
summary(z1)

logit2 <- coefficients(z1)[1] + coefficients(z1)[2]*x
rmalehat <- exp(logit2) / (1 + exp(logit2))
#로짓변환하여 온도별 예측 확률값은 0~1 사이에 값을 갖는다.


# 3. 최대우도추정법을 사용하여 회귀식 추정
logit = glm(pmale~x, family = "binomial", weights = total )
# 추정 회귀식 : 수컷 비율 =  -61.318 + 2.211*온도
# 즉, -61.318 + 2.211 = 0 이 되는 27.3도에서 암컷과 수컷을 구분짓게 된다. 

exp(-61.318)*exp(2.211*27)   # 27도에서 오즈 예측값
exp(-61.318)*exp(2.211*28)   # 28도에서 오즈 예측값
# 즉, 28도에서 오즈 예측값은 27도에서의 오즈 예측값보다 exp(2.211) = 9.125배


