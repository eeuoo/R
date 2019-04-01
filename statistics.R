
table(data$cls)

n.gen = table(data$gen)  # 도수
p.gen = prop.table(n.gen)  # 상대도수
   
 
#### cf. 분할표 ######

rt.gen = rbind(n.gen, p.gen)
ct.gen = cbind(n.gen, p.gen)

colnames(ct.gen) = c('도수', '상대도수')

ct.gen = addmargins(ct.gen, margin = 1)

t.gen_cls = table(data$gen, data$cls)
p.gen_cls = prop.table(t.gen_cls)

round(p.gen_cls, 2)

barplot(p.gen, main='성별', xlab="gender", ylab='%', 
        col=c("red", "blue"), ylim=c(0, 0.7), legend = rownames(p.gen))

barplot(p.gen_cls, main='성별', xlab="gender", ylab='%', col=c("green", "blue"), 
        ylim=c(0, 0.2), legend = rownames(p.gen_cls), beside = T)



#### 평균, 중앙값, 최빈값 ######

# 산술 평균
mean(data$kor)   #모집단
mean(data[data$cls == '매', ]$kor)  #추출

# 중앙값(Median)
median(data$kor)

data %>% group_by(cls) %>% summarise(m = mean(kor), n = n())
data %>% filter(cls == '매') %>% select(kor) %>% summarise(m = mean(kor), n = n())



###  가중평균 ######

# 학급별 학생수 가중치 적용
data2 = data %>% filter(stuno < 10300 | stuno > 30000) %>%
  group_by(cls) %>% summarise(m = mean(kor), n = n())

w = data2$n / sum(data2$n)

# 가중평균 계산
weighted.mean(data2$m, w)
mean(data2$m)


#### 기하평균 #####

gmdata = c(1100/1000, 1320/1100, 1122/1320)

geometric.mean(gmdata) - 1



#### 조화평균 #####

harmonic.mean(c(100, 50))



#### 절사(Trimmed) 평균,  boxplot, stem, 분산, 편차  #######

# 최저, 최고 10% 제외 평균
t = c(20, 70, 60, 78, 69, 72, 79, 75, 65, 99)
t2 = c(70, 60, 78, 69, 72, 79, 75, 65)
mean(t)
mean(t2)
mean(t, trim=0.10)
mean(t[t < 90 & t > 20])

# 극단치를 시각적으로 보기
boxplot(t)                  # cf.  boxplot(kor~cls, data=data)
stem(t, scale = 2)
stem(t, scale = 1)

# 분산(variance), 표준편차(standard deviation)
var(t)
var(t2)
sd(t)
sd(t2)

# cf. IQR(Inter-Quantitle Range)
summary(t)
describe(t)
IQR(t)    # Q3 - Q1



#### 범위, 왜도, 첨도  #####

# 범위: 데이터의 범위(최소 ~ 최대)
range(t)
min(t)
max(t)

# 왜도(Skewness) : 대칭성 측정
hist(t, breaks = 10)
hist(t)
skew(t)   # psych

# 첨도(Kurtosis): 평균의 몰림 측정
kurtosi(t)  # psych

# 종합 보기(summary, describe)
summary(t)
describe(t)         # psych





