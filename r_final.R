load("~/workspace/R/data/data.rda")

# `죽반과 매반의 수학성적은 차이가 없다` 라는 가설을 검증하시오.

data

mjmath = data %>% filter(cls %in% c('죽', '매')) %>% select(cls, math)

mjmath$cls = factor(mjmath$cls, levels=c('죽','매'), labels=c('죽', '매'))

describeBy(mjmath$math, mjmath$cls, mat = T)

orgpar = par(no.readonly = T)     
layout(matrix(c(1,1,2,3), 2, 2, byrow = T))
boxplot(mjmath$math ~ mjmath$cls)
hist(mjmath$math[mjmath$cls == '죽'])
hist(mjmath$math[mjmath$cls == '매'])
par(orgpar)

var.test(mjmath$math ~ mjmath$cls, data = mjmath) 

t.test(mjmath$math ~ mjmath$cls, data = mjmath,
       alternative = c("two.sided"),
       var.equal = T,                
       conf.level = 0.95)

mu1 = 63.46667
se1 = 2.144661
rn1 = sort(rnorm(1000, mu1, se1))

plot(rn1, dnorm(rn1, mu1, se1), col='green', type = 'l', main = '평균점수',
     xlim = c(50, 80), ylim = c(0, 0.25)) 
abline(v=mu1, col="green", lty=5)

mu2 = 63.84167
se2 = 2.114145
rn2 = sort(rnorm(1000, mu2, se2))

par(new = T) 

plot(rn2, dnorm(rn2, mu2, se2), col='red', type = 'l', main = '평균점수',
     xlim = c(50, 80), ylim = c(0, 0.25))
abline(v=mu2, col="red", lty=5)




# 4개반 수학성적의 유사도(동질의 정도)를 분석하시오.

# 기술 통계 확인
describeBy(data$math, data$cls, mat = T)

# 그래프로 데이터 확인하기 (ggplot2)
ggplot(data, aes(x=cls, y=kor)) +
  geom_boxplot(outlier.colour = 'blue') +
  ggtitle("각반 국어 성적")  # 반별 box

ggplot(data, aes(x=kor)) +
  geom_histogram(binwidth = 10, col="white") +
  facet_grid(. ~ data$cls)   # 반별 hist

# 등분산 검정 
bartlett.test(data$math ~ data$cls, data=data)

# ANOVA 분석
aaa = aov(data$math ~ data$cls, data = data)
summary(aaa)

# Welch's ANOVA One-way test 
oneway.test(data$math ~ data$cls, data = data, var.equal = F)

# 사후 분석
TukeyHSD(aaa)

# 6. 동질성 결과 그래프
plot(TukeyHSD(aaa))

# plot 함수 작성 (반복 제거)
draw = function(rn, mu, se, col){
  plot(rn, dnorm(rn, mu, se), col=col, type='l',
       xlim = c(50,80), ylim = c(0, 0.25))
  abline(v=mu, col=col, lty=5)
}
  
# 4개 한번에 그리기
mu = 63.59167; se = 2.020535; rn = sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'red')     #국
par(new = T)
mu = 63.08333; se = 2.028632; rn = sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'blue')    #난
par(new = T)
mu = 63.8416; se = 2.11414; rn = sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'green')   #매

par(new = T)
mu = 63.46667; se = 2.144661; rn = sort(rnorm(1000, mu, se))
draw(rn, mu, se, 'black')   #죽
 
legend('topright',
       legend = c('국', '난', '매', '죽'),
       pch = 8,
       col =c('red', 'blue', 'green', 'black'),
       bg='white')

par(orgpar)



# 전교생의 국어성적과 영어성적에 대한 상관분석(Correlation)을 수행하시오.
data %>% select(kor, eng) -> cdata

describe(cdata)
pairs.panels(cdata)  

cor(cdata, use = "complete.obs",  
    method = c("pearson"))   

plot(kor ~ eng, data=cdata)
abline(lm(kor ~ eng, data=cdata), col='red')  


# 단순 회귀 분석
mpg %>% select(displ, cty) -> cdata
lm(cty ~ displ, data=cdata) -> lmdata
summary(lmdata) 
pairs.panels(cdata)
par(mfrow=c(2,2))
plot(lmdata)  
par(orgpar)
shapiro.test(lmdata$residuals)    

# 다중 회귀 분석
mpg %>% select(displ, year, cyl, cty) -> cdata2
describe(cdata2)
pairs.panels(cdata2)
lmdata2 = lm(cty ~ displ+cyl+year, data=cdata2)    
lmdata2 = lm(cty ~ displ+cyl, data=cdata2)       # year는 무관하므로 제거
summary(lmdata2)

install.packages('car')
library(car)
vif(lmdata2)
par(mfrow=c(2,2))
plot(lmdata2)  
par(orgpar)

# 로지스틱 회귀 분석
unique(mpg$trans) 
unique(mpg$year)

cdata3 = mpg %>%
  mutate(trs = ifelse(substr(trans, 1, 4) == 'auto', 1, 0), 
         y = ifelse(year == 1999, 0, 1)) %>%
  select(y, displ, cyl, trs, cty, hwy)

describe(cdata3)
pairs.panels(cdata3)

glmdata = glm(y ~ displ+cyl+cty+hwy+trs, family = binomial, data=cdata3)
summary(glmdata)
par(mfrow=c(2,2))
plot(glmdata)
par(orgpar)

round(exp(cbind(LOR = coef(glmdata), confint(glmdata))), 2)
