# try this ####

#1 data$group 컬럼에 A조~C조 랜덤으로 160명씩 고르게 분포시키시오.
data$c1 = sample(rep(LETTERS[1:3], times=3, length.out=nrow(data)), size=nrow(data), replace=F)
cat(nrow(data), nrow(data[data$c1 == "C",]))
cat(nrow(data), nrow(data[data$c1 == "B",]))
cat(nrow(data), nrow(data[data$c1 == "A",]))
table(data$c1)

#2 fibonacci.R 파일을 작성하고 console에서 실행하시오.
source('fibonacci.R')

#3 apply를 이용하여 smdt에 과목별 (총)평균점수 행을 추가하고, 총점과 평균 변수(컬럼)을 추가하시오.

smdt
smdtmean = round(apply(smdt[, 2:4], MARGIN = 2, FUN = mean))
smdtmean
smdt = rbind(smdt, c(6, smdtmean))
smdt$total = round(apply(smdt[, 2:4], MARGIN = 1, FUN = sum))
smdt$avg = round(apply(smdt[, 2:4], MARGIN = 1, FUN = mean))
smdt[smdt$stuno == 6 ,]$stuno = "계"
smdt


#4 2016~2019년 연도별 1월(Jan) ~ 12월(Dec) 매출액 데이터 `no year Jan Feb … Dec` 형태로 만든 다음, 아래와 같이 출력하시오.
dfsum = cbind( data.frame(no=1:4, year=2016:2019), 
               matrix(round(runif(48), 3) * 10000, ncol=12, dimnames = list(NULL, month.abb)) )

meltsum = melt(dfsum[ ,2:14], id.vars = "year", variable.name = "month", value.name = "saleamt") 
meltsum
