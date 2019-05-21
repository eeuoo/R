# 분류 분석
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
# 1. bagging(배깅)
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


# 2. boosting(부스팅) 
set.seed(1)
train = c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))

iris.adaboost = boosting(Species~., data=iris[train,], mfinal = 10, control = rpart.control(maxdepth = 1))   # importance, weight이 배깅과 다르다.

table(iris.adaboost$class, iris$Species[train], dnn = c('predicted class', 'Observed Class'))  # 배깅과 결과 같음.


# 3. random forest (랜덤 포레스트)
#install.packages('randomForest')
library(randomForest)
library(rpart)
data(stagec)

# 결측값 없는 행만 추출. complete.cases : NA 있는지 확인
stagec3 = stagec[complete.cases(stagec), ]

# 샘플링(train, test)
set.seed(1234)
ind = sample(2, nrow(stagec3), replace = TRUE, prob = c(0.7, 0.3))

train = stagec3[ind == 1,]
test = stagec3[ind == 2, ]

# proximity 분석 대상 자료 간의 유사도를 산정한 값
rf = randomForest(ploidy~., data = train, ntree = 100, proximity =TRUE, importance = TRUE)
table(predict(rf), train$ploidy)
print(rf)
# Confusion matrix : 정오분류표, OOB : Out Of Bag, 모델 훈련에 사용되지 않은 데이터를 사용한 에러 추정치

varImpPlot(rf)
# 정확성, 불순도 개선 기여도 모두 g2가 가장 높다.



#### SVM (서포트 벡터 머신) #####
library(e1071)

s = sample(150, 100)  # 150 중 100 샘플링
col = c('Petal.Length', 'Petal.Width', 'Species')   # 컬럼명 지정

iris_train = iris[s, col]
iris_test = iris[-s, col]

# linear kernel 방식으로 modeling
# cost : 커널 파라미터 cost 과적합 조절, kernel : 커널 함수
iris_svm = svm(Species~., data = iris_train, cost = 1, kernel = 'linear')

plot(iris_svm, iris_train[, col])

# svm train vs test 결과 예측
p = predict(iris_svm, iris_test[,col], type = 'class')
plot(p)
table(p, iris_test[,3])



#### naive bayes (나이브 베이즈) #####
colnames(iris) = tolower(colnames(iris))

m = naiveBayes(species~., data = iris)
# [,1]setosa 평균, [,2]setosa 표준편차

table(predict(m, iris[,-5]), iris[,5], dnn = list('predicted', 'actual'))


##### 모형 평가 #####
# bootstrap
# ROC 그래프
# 1. infert data를 신경망 모형과 싀사결정 모형을 통해 모평 평가를 한다. 
set.seed(1234)
str(infert)

infert = infert[sample(nrow(infert)),]
infert = infert[ , c('age', 'parity', 'induced', 'spontaneous', 'case')]

traindata = infert[1:(nrow(infert)*0.7 + 1), ]
testdata = infert[(nrow(infert)*0.7 + 1) : nrow(infert), ]

#install.packages("neuralnet")
library(neuralnet)

m = model.matrix(~ case + age + parity + induced + spontaneous, data = traindata)

net.infert = neuralnet(case~age + parity + induced + spontaneous, data = m, hidden = 3, err.fct = 'ce', linear.output = FALSE, likelihood = TRUE)

n_test = subset(testdata, select = -case)
head(n_test)

# model estimate value
nn_pred = compute(net.infert, n_test)
testdata$net_pred = nn_pred$net.result
head(testdata)

# decision tree
#install.packages("C50")
library(C50)

traindata$case = factor(traindata$case)

dt.infert = C5.0(case~age + parity + induced + spontaneous, data = traindata)

# predict value extract
testdata$dt_pred = predict(dt.infert, testdata, type = 'prob')[ ,2]
head(testdata)

#install.packages('Epi')
library(Epi)

neural_ROC = ROC(form = case~net_pred, data = testdata, plot = 'ROC')
dtree_ROC = ROC(form = case~dt_pred, data = testdata, plot = 'ROC')
# 분석 결과 신경망 모형의 AUC(0.723)가 의사결정나무 AUC(0.661)보다 높아 신경망 모형의 분류분석 모형이 더 높은 성과를 보여주고 있다. 


# 이익도표와 향상도
#install.packages('ROCR')
library(ROCR)

n_r = prediction(testdata$net_pred, testdata$case)
d_r = prediction(testdata$dt_pred, testdata$case)

n_p = performance(n_r, 'tpr', 'fpr')
d_p = performance(d_r, 'tpr', 'fpr')

plot(n_p, col = 'red')
par(new = TRUE)
plot(d_p, col = 'blue')
abline(a = 0, b = 1)
par(new = FALSE)

# 신경망 모형의 향상도 곡선
n_lift = performance(n_r, 'lift', 'rpp')
plot(n_lift, col = 'red')
abline(v = 0.2)
# 신경망 모형의 경우 상위 20%의 집단에 대하여 랜덤 모델과 비교할 때 2배의 성과 향상을 보인다. 
