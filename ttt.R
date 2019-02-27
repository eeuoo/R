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
