# 비정돈 형식 간에 변환하기
# - 정돈 텍스트 형식을 다른 중요한 패키지 및 데이터 구조와 연결하는 접착제(glue)


### 문서-용어 행렬 정돈
# - 각 행은 하나의 문서를 나타낸다
# - 각 열은 하나의 용어를 나타낸다
# - 각 값에는 해당 문서에서 해당 용어가 출연하는 횟수가 들어간다

# cast() : 1행당 1용어
# cast_sparse() : Matrix 패키지에서 희소 행렬로 변환
# cast_dtm() : tm에서 DocumentTermMatrix 객체로 변환
# cas_dfm() : quanteda에서 dfm 객체로 변환

library(tm)
# install.packages("topicmodels")
library(topicmodels)

data("AssociatedPress", package = "topicmodels")
AssociatedPress

