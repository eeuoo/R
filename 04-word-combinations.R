# 단어 간 관계 : 엔그램과 상관

library(dplyr)
library(tidytext)
library(janeaustenr)

### 엔그램에 의한 토큰화

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams


### 엔그램 개수 세기와 선별하기

austen_bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)

bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_united


### 트라이그램(trigram) : 서로 이어져 있는 세 단어

austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


### 바이그램 분석 
# - 1행 1바이그램

bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

bigram_tf_idf <- bigram_united %>%
  count(book, bigram) %>%
  bind_tf_idf(bigram, book, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf


### 정서분석에 바이그램으로 문맥 파악

bigrams_separated %>% 
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

load("~/workspace/R/afinn.rda")
AFINN <- afinn

AFINN

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

not_words

library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(word2, n * value, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  xlab("Words preceded by \"not\"") +
  ylab("Sentiment value * number of occurrences") +
  coord_flip()


  