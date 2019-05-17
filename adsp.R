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

tables()
