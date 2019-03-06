# order, sort ####
t = c(5, 7, 2, 8, 20, 11, 19)
t[order(t)]
t[order(t, decreasing = T)]

smdt[order(smdt$avg, -smdt$Korean)]

rev(t[order(t)])
sort(t)
sort(smdt$avg, decreasing = T)

t = c(1:5, NA, 7, NA, 9, 10)
m1 = m2 = m3 = matrix(c(1:3, NA, 9:3, NA, 1:3), nrow = 3)
is.na(t)
is.infinite(t)
table(is.na(t))
t[is.na(t)]

mean(t)
mean(t, na.rm = T)
mean(t[!is.na(t)])

t = ifelse(is.na(t), 0 , t)
m1
m1[is.na(m1)] = 0
m2
m2[is.na(m2[,2]), 2] = 55

# array ####
dataArray = array(1:24, dim=c(3,4,2))
dim(dataArray)
nrow(dataArray)
ncol(dataArray)
length(dataArray)

dataArray
dimnames(dataArray)
dimnames(dataArray) = list(1:3, c('c1','c2','c3','c4'), c('x','y'))

ad1 = dataArray[,,1]
ad1

attr(dataArray, "dim") = c(3,8)
attr(dataArray, "dim") = NULL
dataArray
