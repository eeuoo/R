# 토픽 모델링(= 주제 모형화)
# - 잠재 디리클레 할당(Latent Dirichlet allocation, LDA)은 모델 적합화(fit)에 많이 사용
# - 모든 문서는 토픽드르이 혼합체이다
# - 모든 토픽은 단어들의 혼합체이다 

library(topicmodels)

data("AssociatedPress") # 1988년경에 주로 작성한 2,246개의 뉴스 기사 모음집

AssociatedPress

# seed 값을 정해 모델의 결과를 에측할 수 있게 하자
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))

ap_lda


### 단어-토픽 확률
# - 단어당 토픽당 확률 추출(per-topic-per-word)
# - 1행당 1용어, 1용어당 1토픽

library(tidytext)
# install.packages("reshape2")

ap_topics <- tidy(ap_lda, matrix = "beta")

ap_topics

library(ggplot2)
library(dplyr)

ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = 'free') +
  coord_flip()

