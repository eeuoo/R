# rep, seq ####
rep(1, times=3)
rep(LETTERS[1:3], times=3)
rep(LETTERS[1:3], each=3)
rep(c("I","am"), times=2, length.out=7)

data.frame(cumtomer=rep(c('김일수','김이수'), each=2),
           menu=rep(c('짜장','짬뽕'), times=2))

-3:5
5.5:-4.5
seq(5.5:-4.5)
seq(from=1, to=10, by=0.5)
seq(5.4, -4.5, length.out = 10)

# runif, sample ####
runif(20)
t=runif(n=30, min=10, max=20)
t
plot(t[order(t)])

sample(1:5, size=3)
sample(1:5, size = 30, replace = T)
set.seed(100); sample(1:5, size = 3)

data[sample(1:nrow(data), size = 5),]
data[sample(1:nrow(data), size = 5, replace = F),]

data$c1 = sample(c('AA','BB'), size=nrow(data), replace = T)
cat(nrow(data), nrow(data[data$c1 == "AA",]))
data$c1 = sample(c('AA','BB'), size = nrow(data), replace = T, prob = c(0.5, 0.5))
cat(nrow(data), nrow(data[data$c1 == "AA",]))

# set.seed ####
set.seed(255)
sample(1:100,10)

smdt = data.frame(stuno = 1:5,
                  Korean = sample(60:100, 5),
                  English = sample((5:10)*10,5),
                  Math = sample(50:100, 5))
smdt

# 문자열 함수 ####
s = "abc,efg,abc"
nchar(s)
toupper(s)

substr(s, 1, 5)
strsplit(s, ',')
sub('abc', 'ttt', s)
gsub('abc', 'ttt', s)

s = "first\tsecond\nthird"
print(s)
cat(s)

paste('aaa-bbb','ccc-ddd', sep='**')
paste(data[1:3,'반'], collapse = '**')
paste(data[1:3,'반'])

outer(month.abb, 2011:2020, paste, sep='_')
outer(LETTERS, 2010:2020, paste0)

grep(pattern='3(.*)1', x=data$학번, value = T)

# 시간 함수 #####
install.packages('lubridate')
library(lubridate)

as.Date('2019-03-04 09:12')
dt1 = as.POSIXct('2019-03-04 09:12')

seq(dt1, as.POSIXct('2019-04-01'), by='day')
seq(dt1, as.POSIXct('2019-04-01'), by='2 hour')
seq(dt1, as.POSIXct('2019-04-01 23:59'), by='min')

ymd('2019/02/28')
mdy('03152019')
dmy('12052019')

year(dt1)
quarter(dt1)
days_in_month(1:12)
ddays(10)
dhours(50)
duration(1000)
round(as.POSIXct('2019-03-05 18:39:45'),'min')

# for ####
for (i in 1:3){print(i)}
for (r in i:nrow(data)) {print(data[r,'scout'])}

while(i < 10){ print(i); i = i + 1}

i = 0
while(TRUE){
  i = i + 1
  if (i %% 2 == 0) next
  if ( i > 10) break
  print(i)
}

# 할당 연산자 ####
x = 10
fn_for = function(n) {
  x = 0
  for (i in 1:n) {
    # x = x + i
    x <<- x + i
  }
  print(paste("x =", x))
}

fn_for(x <- 20)
x


# apply ####
apply(smdt[ ,2:4], MARGIN = 1, FUN = mean)
apply(smdt[ ,2:4], MARGIN = 2, FUN = mean)
apply(smdt[ ,2:4], MARGIN = 2, FUN = quantile)

lapply(smdt[ ,2:4], FUN = mean)
unlist(lapply(smdt[ ,2:4], FUN = mean))

sapply(smdt[ ,2:4], FUN = mean, simplify = T)
vapply(smdt[ ,2:4], FUN = mean, FUN.VALUE = '')


# reshape2 - melt, dcast ####
library('reshape2')

dfsum = cbind( data.frame(no=1:4, year=2016:2019), 
               matrix(round(runif(16), 3) * 1000, ncol=4, dimnames = list(NULL, paste0('Q', 1:4))) )
dfsum
melt(data = dfsum[, 2:6], id.vars = "year")
meltsum = melt(dfsum[ ,2:6], id.vars = "year", variable.name = "Sales")               

dcast(meltsum, Sales~year, value.var = "value")


# try this ####

#1 data$group 컬럼에 A조~C조 랜덤으로 160명씩 고르게 분포시키시오.
data$c1 = sample(rep(LETTERS[1:3], times=3, length.out=nrow(data)), size=nrow(data), replace=F)
cat(nrow(data), nrow(data[data$c1 == "C",]))
cat(nrow(data), nrow(data[data$c1 == "B",]))
cat(nrow(data), nrow(data[data$c1 == "A",]))
table(data$c1)

#2 fibonacci.R 파일을 작성하고 console에서 실행하시오.
#fibonacci.R

#3 apply를 이용하여 smdt에 과목별 (총)평균점수 행을 추가하고, 총점과 평균 변수(컬럼)을 추가하시오.

smdt
smdtmean = apply(smdt[, 2:4], MARGIN = 2, FUN = mean)
smdtmean
smdt1 = rbind(smdt, c(6, smdtmean))
smdt1$total = apply(smdt1[, 2:4], MARGIN = 1, FUN = sum)
smdt1$avg = apply(smdt1[, 2:4], MARGIN = 1, FUN = mean)
smdt1[smdt1$stuno == 6 ,]$stuno = "계"
smdt1


#4 2016~2019년 연도별 1월(Jan) ~ 12월(Dec) 매출액 데이터 `no year Jan Feb … Dec` 형태로 만든 다음, 아래와 같이 출력하시오.
dfsum = cbind( data.frame(no=1:4, year=2016:2019), 
               matrix(round(runif(48), 3) * 10000, ncol=12, dimnames = list(NULL, month.abb)) )

meltsum = melt(dfsum[ ,2:14], id.vars = "year", variable.name = "month", value.name = "saleamt") 
meltsum
