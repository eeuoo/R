# 토픽 모델링(= 주제 모형화)
# - 잠재 디리클레 할당(Latent Dirichlet allocation, LDA)은 모델 적합화(fit)에 많이 사용
# - 모든 문서는 토픽드르이 혼합체이다
# - 모든 토픽은 단어들의 혼합체이다 

library(topicmodels)

data("AssociatedPress") # 1988년경에 주로 작성한 2,246개의 뉴스 기사 모음집

AssociatedPress
