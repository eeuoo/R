# 군집 분석

#### SOM (자기조직화지도) #####

# 1. SOM Clustering을 수행하기 위해 R 패키지 kohonen을 설치한 후에 5*4 SOM 군집분석을 수행한다.
#install.packages('kohonen')
library(kohonen)

data("wines")
head(wines)

set.seed(7)

# scale() 표준화 함수
wine.som = som(scale(wines), grid = somgrid(5, 4, 'hexagonal'))
summary(wine.som)

plot(wine.som, main = 'wine data')


# 2. plot.kohonen 패키지를 활용한 시각화
par(mfrow = c(1,3))

plot(wine.som, type = 'counts')
plot(wine.som, type = 'quality')
plot(wine.som, type = 'mapping')

par(mfrow = c(1,1)) # 초기화
