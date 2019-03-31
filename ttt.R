#factor ####
factor(2)
factor(2, levels=1:5)
factor(1, levels = 1:5, labels = LETTERS[1:5], ordered = T)
t =factor(2, levels=1:3, labels=c('A','B','C'))

row.names(d) = NULL 
d

is.factor(t)
levels(t)
nlevels(t)

as.numeric(t)
c(t)

as.vector(t)
as.character(t)

# vector ####
v1 = c(1,2,3)
v1
length(v1)
v1[1]+v1[3]
summary(v1)
v1[4] = 4
v1[6] = 6
class(v1)
v1 [ c(1,3,5)] = 1
v1[-(1:3)] = 0
v1[c(1:2, 4)]
names(v1)
LETTERS
length(v1)
names(v1) = LETTERS[1:length(v1)]
v1['D']
names(v1)=NULL
v1

# matrix ####
m2=matrix(c(LETTERS,1,2,3,4), ncol=5)
m2
colnames(m2)=c('1class','2class','3class','4class','5class')
rownames(m2)=1:nrow(m2)
m2[,c(1,3)]

data1 = 1:30
rowcnt = 6
m3 = matrix(data1, ncol=length(data1)/rowcnt, byrow = T)
m3
cbind(m2,m3)
rbind(m2,m3)
colnames(m3) = LETTERS[1:ncol(m3)]
rownames(m3)= letters[1:nrow(m3)]

# try this ####

#1
getbt = function(num){
  bt = factor(num, levels=1:4, labels=c('A','B','O','AB'))
  return(as.vector(bt))
}
getbt(1)

#2
vec1 = c(1,2,3) 

vecappend = function(vec, value) {
  vec[length(vec)+1] = value
  return(vec)
}

vecappend(vec1, 4) 
vec1 

#3
nums = 1:200
numsrow = 10
mat1 = matrix(nums, ncol = length(nums)/numsrow, byrow = T)
mat1
colnames(mat1) = LETTERS[1:ncol(mat1)]
rownames(mat1) = letters[1:nrow(mat1)]
colnames(mat1)[c(10,20)] = c(10,20)
paste('A', 10, sep='')
paste0('A', 10)

# data frame ####
df1 = data.frame(column1= 11:15, column2 = LETTERS[1:5])
df1
str(df1)

df2 = data[1:10, 1:6]
df3 = data[1:10, c(1:3, 7,8)]
cbind(df2, df3)
df100 = data[101:110, 1:6]
df100
rbind(df2, df100)

lst1 = list(a=1:3, b=4:8)
str(lst1)
names(lst1)

lst2 = list(a=1:3, b=c(4,'5',6))
names(lst2)

lst3 = list(1:3, LETTERS[1:3], c(2.5, 4.5, 3.7))
lst3[1]
unlist(lst3)
unlist(lst1)

# try this 2 ####
df2
df3_1 = df3[c('과학','예체')]
df4 = cbind(df2, df3_1)

cn = colnames(df4)
df4[,c(1:4, 7, 6, 8, 5 )]
df4[,c(cn[1:4], '과학','수학','예체','영어')]


# package #####
install.packages('data.table')

library('data.table')

start = Sys.time()
#read.csv('data/성적.csv')
fread('data/score.csv')
Sys.time() - start


# read.delim #####
sepdata = read.delim('data/sep.txt', sep="#")
str(sepdata)
summary(sepdata)
View(sepdata)
sepdata$name = as.character(sepdata$name)
class(sepdata$name)

# read tsv #####
tsvdata = read.table("data/tcv.tsv", sep = '\t', header = T, stringsAsFactors = F)
str(tsvdata)
View(tsvdata)

# read fwf ####
fwfdata = read.fwf('data/fwf.txt', header = F, widths = c(8, 6, 5, 3, 4))
fwfdata
str(fwfdata)
dim(fwfdata)
head(fwfdata)
View(fwfdata)

# read excel #####
# install.packages('readxl')
library('readxl')
mtx = read_excel('data/melon_top100.xlsx', sheet = 1, col_names = T)
head(mtx)
str(mtx)
dim(mtx)
View(mtx)
colnames(mtx) = NULL


# try this 3 ####
#1
mtx = mtx[-c(1,nrow(mtx)),]
save(mtx, file = "data/meltop100.rda")

#2
write.csv(mtx, "data/meltop100.csv")
mtxcsv = read.csv("data/meltop100.csv", header=F)
str(mtx)

#3
temperdata = read.fwf('data/temper.txt', header = F, widths = c(15, 4, 68, 5, 1))
temperdata
temperdata = temperdata[,-c(1,3)]
str(temperdata)

# row operator & aggregate ####
data[data$학번 == 10102,]
km = data[data$국어 > 90 & data$수학 > 90, ]
km[order(km$수학),]
mean(data$수학)
sum(data$수학)
median(data$수학)
data$국어 + data$영어 + data$수학
data$총점 = data[,4] + data[,5] + data[,6]

aggregate(data=data, 수학~반, mean)
aggregate(data=data, cbind(국어,영어,수학)~반, mean)
 
# install.packages('psych')
library('psych')
describe(data)
summary(data)

# try this 4 ####
#1
data$반 == '난'
data$성별 == '남'
nannam = data[data$반 == '난' & data$성별 == '남' & data$국어 > 90 & data$수학 > 90, ]
nannam[order(nannam$국어 * -1), ]

#2
data
data$총점 = data[,4] + data[,5] + data[,6] + data[,7] + data[,8]
data$평균 = data[,9]/5
korean80 = data[data$국어 >= 80,]
aggregate(data=korean80, 국어~반, mean)
aggregate(data=korean80, 평균~반, mean)
aggregate(data=data[data$국어 >= 80,], cbind(국어,평균)~반, mean)


# install.packages("ggplot2")
library("ggplot2")

# ifelse ####
data$pass = ifelse(data$subMean >= 60, TRUE, FALSE)
head(data)
data[data$pass,]
data$scout = ifelse(data$평균 >= 60, ifelse(data$성별 == '남', 'BoyScout', 'GirlScout'), '')
qplot(data$pass)
qplot(data[data$scout != '',]$scout)
data$학점 = ifelse(data$평균 >= 90, 'A', ifelse(data$평균 >= 80, 'B', ifelse(data$평균 >= 70, 'C', ifelse(data$평균 >= 60, 'D', 'F'))))

table(data$학점)

data()
dr = data()$result
colnames(dr)       # dim(dr),   head(dr)
dr$item

data("AirPassengers")
AirPassengers
str(AirPassengers)
head(AirPassengers)

data('trees')
trees
class(trees)
head(trees)

letters
LETTERS
month.name
month.abb
pi

# 초기화
letters = c('AA', 'BB', 'CC')
letters
rm(letters)
letters
ls(pattern = 'm')

ggplot(data$pass)
