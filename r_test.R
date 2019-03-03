library(ggplot2)
mpg = as.data.frame(ggplot2::mpg)
str(mpg)
summary(mpg)

# manufacturer: 제조사
# displ: 배기량(displacement)
# trans: 변속기 종류
# cyl: 실린더 개수
# drv: 구동방식(drive wheel)
# cty: 도시 연비
# hwy: 고속도로 연비
# fl: 연료종류(fuel)
  
# mpg 데이터에서 통합 연비(도시와 고속도로)가 높은 순으로 출력하시오.
mpg[order((mpg[,8]+mpg[,9])*-1),]

# mpg 데이터에서 생산연도별 연료 종류에 따른 통합연비를 연도순으로 출력하시오.
fl_year = mpg[,c(4,8,9,10)]
fl_year
aggregate(data=fl_year, cbind(cty, hwy)~fl, sum)

# midwest 데이터를 data.frame으로 불러온 후, 데이터의 특징을 설명하시오.

# poptotal 변수(컬럼)를 total로, popasian 변수를 asian으로 변수명을 변경하는 코드를 작성하시오.

# total, asian 변수를 이용해 `전체 인구 대비 아시아계 인구 백분율` 파생변수(asianpct)를 추가하고, 히스토그램을 그려, 도시들이 어떻게 분포하는지 설명하시오.

# 아시아계 인구 백분율(asianpct)의 전체 평균을 구하고, 평균을 초과하면 "lg", 그 외는 "sm"을 부여하는 파생변수(asianrate)를 추가하는 코드를 작성하시오.

# "lg"와 "sm"에 해당하는 지역이 얼마나 되는지 빈도 막대그래프(qplot)을 그려보시오.
