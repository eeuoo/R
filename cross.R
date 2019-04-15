##### 교차분석 ######

# 1. 데이터 준비 (가상)
kdata = data.frame(group=rep(c('YogaO', 'YogaO', 'YogaX', 'YogaX'), each=25),
                   golda=rep(c('GoldaO', 'GoldaX', 'GoldaO','GoldaX'), times=c(19,31,32,18)) )


# 2. 데이터 확인 (분할표, 그래프)
nkdata = table(kdata)
addmargins(nkdata)
prop.table(nkdata)

barplot(nkdata, beside = T, legend = T)
mosaicplot(nkdata, shade = F, color = c('red', 'green'))
mosaicplot(nkdata, shade = T)

# 3. 카이제곱 분석
chisq.test(nkdata)  # p-value 체크

# 4. 교차표(CrossTable) 작성  (install.packages('gmodels'))
library(gmodels)
CrossTable(nkdata, expected = T, chisq = T)

# 5. 상대 비율 계산
0.373 / 0.627

