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

negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup()

negated_words

negated_words %>%
  mutate(contribution = n * value,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  top_n(12, abs(contribution)) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment value * # of occurrences") +
  coord_flip()


### 바이그램 연결망 시각화
# - from : 연결선이 나가는 정점
# - to : 연결선이 향하는 정점
# - weight : 각 연결선과 연관된 숫자 값

# install.packages("igraph") https://igraph.org/r/
library(igraph)

bigram_counts # 원래 카운트

# 상대적으로 흔한 조합만을 선별하는 필터
bigram_graph <- bigram_counts %>%
  # filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

# install.packages("ggraph") https://www.rdocumentation.org/packages/ggraph/versions/0.1.1
# devtools::install_github('thomasp85/ggforce')
# devtools::install_github('thomasp85/ggraph')
# install.packages("ggplot2")
library(tidygraph)
library(ggplot2)
library(ggraph)
set.seed(2020)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

