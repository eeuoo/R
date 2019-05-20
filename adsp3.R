##### 로지스틱 회귀모형 ######

# 온도 변화에 따른 거북이 알의 수멋과 암컷의 결과
x <- c(27.2, 27.7, 28.3, 28.4, 29.9)
male <- c(2, 17, 26, 19, 27)
female <- c(25, 7, 4, 8, 1)
total = male + female
pmale <- male/total

# 1. 온도를 독립변수로 수컷 비율을 종속변수로 하여 단순선형회귀식을 추정하시오.
z <- lm(pmale~x)
summary(z)   # 추정 회귀식 : 수컷 비율 = -6.9020 + 0.2673*온도

p <- coefficients(z)[1] + coefficients(z)[2]*x    # 결괏값이 1보다 큰 게 존재.


# 2. 로짓변환된 값 log(pmale/(1-pmale))를 종속변수로 한 단순선형회귀식을 추정하시오. 
logit <- log(pmale/(1-pmale))
z1 <- lm(logit~x)
summary(z1)

logit2 <- coefficients(z1)[1] + coefficients(z1)[2]*x
rmalehat <- exp(logit2) / (1 + exp(logit2))
#로짓변환하여 온도별 예측 확률값은 0~1 사이에 값을 갖는다.


# 3. 최대우도추정법을 사용하여 회귀식 추정
logit = glm(pmale~x, family = "binomial", weights = total )
# 추정 회귀식 : 수컷 비율 =  -61.318 + 2.211*온도
# 즉, -61.318 + 2.211 = 0 이 되는 27.3도에서 암컷과 수컷을 구분짓게 된다. 

exp(-61.318)*exp(2.211*27)   # 27도에서 오즈 예측값
exp(-61.318)*exp(2.211*28)   # 28도에서 오즈 예측값
# 즉, 28도에서 오즈 예측값은 27도에서의 오즈 예측값보다 exp(2.211) = 9.125배


# 4. iris 데이터 로지스틱 회귀분석
colnames(iris) = tolower(colnames(iris))
a = subset(iris, species == 'setosa' | species == 'versicolor')
# 로지스틱 회귀를 하기 위해 범주가 2개인 setosa = 1 과 versicolor = 2 만 추출.

a$species = factor(a$species)  # 2개 레벨을 가지 새로운 factor행(범주형)

b = glm(species ~ sepal.length, data = a, family = binomial)
summary(b)

coef(b)      

fitted(b)[c(1:3, 98:100)]
# 로지스틱 회귀모델은 0 또는 1로 값을 예측하는 모델이다. 적합된 값을 통해 0.5 이하면 'setosa', 0.5 이상이면 'versicolor' 예측값을 의미한다.

predict(b, newdata = a[c(1, 50, 51, 100),], type = 'response')
# type을 response로 지정하고 예측 수행하면 0~1 사이 확률을 구해준다.

cdplot(species~sepal.length, data = a)
# cdplot() 함수 : 연속형 변수의 변화에 따른 범주형 변수의 조건부 분포를 보여준다.
# sepal.length가 커짐에 따라 versicolor의 확률이 증가.


# 5. 이항변수 vs(0:flat engine, 1:straight engine)를 반응변수로, mpg와 am(Transmission : automatic = 0, manual = 1)을 예측변수로 하는 로지스틱 회귀 모형을 추정하시오.
str(mtcars)    # attach() 데이터 프레임에 바로 접근

glm.vs = glm(vs~mpg + am, data = mtcars, family = 'binomial')
summary(glm.vs)
# am이 주어질 때 mpg 값이 한 단위 증가할 때 vs=1의 오즈가 exp(0.6809) = 1.98(98%) 증가한다. 
# mpg가 주어질 때 am의 효과는 exp(-3.0073) = 0.05배, 즉 변속기가 수동인 경우 자동에 비해 vs=1의 오즈가 95% 감소한다.


# 6. ANOVA : 모형의 적합 단계별로 이탈도의 감소량과 유의성 검정 결과를 제시
anova(glm.vs, test = 'Chisq')
# am가 포함되었을 때 resid.Dev의 값이 가장 많이 하락한다.

#install.packages('pscl')
library(pscl)

pR2(glm.vs)
# r2CU 의 값이 0.6914854으로 보아 모델이 데이터셋의 분산의 약 69.1%정도 설명하고 있다. 


##### 신경망 모형 ####
colnames(iris) = tolower(colnames(iris))
#install.packages("nnet")
library(nnet)
data <- iris

Scale = data.frame(lapply(data[,1:4], function(x) scale(x)))
Scale$species = data$species

#신경망 모형 적합 전 scaling(정규화)이 필요
index = sample(1:nrow(Scale), round(0.75*nrow(Scale)), replace = FALSE)

#데이터 분할 (train / test)
train = Scale[index,]
test = Scale[-index,]

#출력, 은닉, 입력 노드의 수와 가중치의 수를 보여준 후 입력 노드 → 은닉 노드, 은닉 노드 → 입력 노드로 가는 가중치의 값을 보여준다. 
model = nnet(species~., size = 2, decay = 5e-04, data = train)
summary(model)

#신경망 모델의 검증
#predict : 결과는 테스트 집합에 대한 예측 결과를 벡터 형태로 제공
predict.model = predict(model, test[,1:4], type = 'class')

#오분류표를 만들기 위해 table 함수를 이용해 실제 결과와 예측 결과에 대한 교차표 작성
actual = test$species
confusion.matrix = table(actual, predict.model)
sum(predict.model == actual) / NROW(predict.model)    # 97.3의 정확도


#### 의사결정나무 #####
colnames(iris) = tolower(colnames(iris))
#install.packages('rpart')
library(rpart)

k = rpart(species~., data = iris)

plot(k, compress = TRUE, margin = .3)
text(k, cex = 1.0)

#install.packages('rpart.plot')
library(rpart.plot)

prp(k, type = 4, extra = 2, digits = 3)

#정확성 평가
head(predict(k, newdata = iris, type = 'class'))
printcp(k)  # error가 제일 낮은 부분 찾기
plotcp(k)

#install.packages('caret')
library(caret)

#install.packages('e1071')
library(e1071)

rpartpred = predict(k, iris, type = 'class')    # 예측
confusionMatrix(rpartpred, iris$species)    # 정확성 평가


##### 앙상블 모형 #####
#install.packages('adabag')
library(adabag)

data(iris)
set.seed(1)

train = c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))

#species에서 각각 25개 sampling
iris.bagging = bagging(Species~., data = iris[train,], mfinal = 10, control = rpart.control(maxdepth = 1))

iris.bagging$importance
#가장 큰 기여를 하는 변수는 Petal.Length(70.32%), Petal.Width(29.67%)

barplot(iris.bagging$importance[order(iris.bagging$importance, decreasing = TRUE)], ylim = c(0, 100), main = 'Variables Relative Importance', col = 'red')

table(iris.bagging$class, iris$Species[train], dnn = c('Predicted Class', 'Observed Class'))
# virginica 1개가 versicolor로 잘못 분류됨.

1 - sum(iris.bagging$class == iris$Species[train]) / length(iris$Species[train])
# 따라서 error는 1.333333 %

iris.predbagging = predict.bagging(iris.bagging, newdata = iris[train, ])
# 따라서 error는 1.333333 %

