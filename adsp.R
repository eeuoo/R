# install.packages("reshape")
library(reshape)

# install.packages("reshape2")

data("airquality")     # airquality 데이터 가져오기
airquality

colnames(airquality)
tolower( colnames(airquality) )

colnames(airquality) <- tolower( colnames(airquality) )

head(airquality)
head(airquality, 3)


??melt   # melt는 reshape의 함수
T = reshape::melt(airquality, id = c("month", "day"), na.rm = TRUE)


??cast   # cast는 reshape의 함수
reshape::cast(T, day~month~variable) # 행을 day, 열을 month로 변수 새롭게 배열


# acast는 reshape의 함수
# 각 변수들의 month 값 평균
b = reshape2::acast(T, month~variable, mean)    

# margin 관련 옵션, 행과 열에 대한 합계를 산출
d = reshape::cast(T, month~variable, mean, margins = c("grand_row", "grand_col"))

# subset 기능으로 특정 변수(ozone)만 처리
e = reshape::cast(T, day~month, mean, subset = variable == "ozone")

# range는 min은 _X1, max는 _X2를 변수 뒤에 붙여줌
f = reshape::cast(T, month~variable, range)



#### sqldf를 이용한 데이터 분석 #####

# install.packages("sqldf")
library(sqldf)


data(iris)

sqldf("select * from iris")
sqldf("select * from iris limit 10")
sqldf("select count(*) from iris where species like '%se'")


##### plyr #####

set.seed(1)
d <- data.frame( year = rep(2012:2014,each=6), count = round(runif(9,0,20)) )

library(plyr)

ddply( d, "year", function(x){
  mean.count = mean(x$count)
  sd.count = sd(x$count)
  cv = sd.count / mean.count
  data.frame(cv.count = cv)
})

# ddplay의 summarise : 데이터의 요약 정보를 새로운 변수에 만든다.
ddply( d, .(year), summarise, mean.count = mean(count) )

# ddplay의 transform : 연산 결과를 데이터 프레임의 새로운 칼럼에 저장.
ddply(d, .(year), transform, total.count = sum(count) )



##### 데이터 테이블 #####
#install.packages("data.table")
library(data.table)

DT = data.table( x = c("b", "b", "b", "a", "a"), v = rnorm(5))

str(cars)

CARS = as.data.table(cars)
head(CARS)

data.table::tables()

sapply(CARS, class)

# setkey( 데이터 테이블, 정렬할 컬럼), 컬럼에 key를 지정한다.
data.table::setkey(DT, x)
DT

# 'b'가 들어간 데이터 표시
DT['b']
DT['b', mult = 'first']
DT['b', mult = 'last']



#### 데이터 기초 통계 ####

data(iris)
head(iris)
str(iris)
summary(iris)
cov(iris[,1:4])   # covariance(공분산)
cor(iris[,1:4])   # correlation(상관계수)

# 결측값 처리
y = c(1,2,3,NA)
is.na(y)

# iris 데이터 특정값 결측처리
iris[iris$Petal.Width == 0.2, "Petal.Width"] <- NA
is.na(iris$Sepal.Width)

x <- c(1,2,NA,3)
mean(x)
mean( x, na.rm = TRUE )

library(reshape)
data("french_fries")

# coplete.cases()를 통해 결측열만 추출
french_fries[!complete.cases(french_fries), ]

#install.packages("Amelia")
library(Amelia)

data("freetrade")
head(freetrade)

# m : imputation 데이터셋 수
a.out <- amelia(freetrade, m=5, ts='year', cs='country')

# 결측값 제거 전
hist(a.out$imputations[[3]]$tariff, col = 'grey', border='white')
missmap(a.out)

# 결측값 제거 후
freetrade$tariff <- a.out$imputations[[5]]$tariff
missmap(freetrade)
