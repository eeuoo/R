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



