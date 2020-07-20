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
# install.packages("topicmodels")  # brew install gsl
library(topicmodels)

data("AssociatedPress", package = "topicmodels")
AssociatedPress

terms <- Terms(AssociatedPress)
head(terms)

library(dplyr)
library(tidytext)

ap_td <- tidy(AssociatedPress)
ap_td

ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

library(ggplot2)

ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()


### dfm 객체 정돈하기
# dfm : 문서-특징 행렬 (document-feature matrix)

library(methods)
install.packages("quanteda")

data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_dfm

inaug_td <- tidy(inaug_dfm)
inaug_td

inaug_tf_idf <- inaug_td %>% 
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

speeches <- c("1933-Roosevelt", "1861-Lincoln",
              "1961-Kennedy", "2009-Obama")

inaug_tf_idf %>%
  filter(document %in% speeches) %>%
  group_by(document) %>% 
  top_n(10, tf_idf) %>%
  ungroup %>% 
  mutate(term = reorder_within(term, tf_idf, document)) %>%
  ggplot(aes(term, tf_idf, fill = document)) + 
  geom_col(show.legend = FALSE) +
  facet_wrap(~ document, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = "",
       y = "tf-idf")


### 정돈 텍스트 데이터를 행렬에 캐싕하기
ap_td %>%
  cast_dtm(document, term, count)

ap_td %>%
  cast_dfm(document, term, count)

library(Matrix)

# Matrix 객체로 캐스팅해 넣기
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)

dim(m)

library(janeaustenr)

austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm


### Corpus 객체를 메타데이터로 정돈하기
data("acq")

acq

acq[[1]]

acq_td <- tidy(acq)

acq_td

acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# 가장 흔한 단어들
acq_tokens %>%
  count(word, sort = TRUE)

# tf-idf
acq_tokens %>% 
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))


### 사례 연구 : 금융 관련 기사 마이닝


