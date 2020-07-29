# 사례 연구 : NASA 메타데이터 마이닝
# 단어 동시 발생 (co-occurrences) 및 상관(correlations)
# tf-idf 및 토픽 모델링을 사용한 데이터셋 간의 연결

library(jsonlite)
metadata <- fromJSON("https://data.nasa.gov/data.json")
names(metadata$dataset)

class(metadata$dataset$title)
class(metadata$dataset$description)
class(metadata$dataset$keyword)


## 데이터 랭글링과 정돈
library(dplyr)

nasa_title <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                     title = metadata$dataset$title)
nasa_title

nasa_desc <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                    desc = metadata$dataset$description)

nasa_desc %>% 
  select(desc) %>% 
  sample_n(5)

library(tidyr)

nasa_keyword <- tibble(id = metadata$dataset$`_id`$`$oid`, 
                       keyword = metadata$dataset$keyword) %>%
  unnest(keyword)

nasa_keyword

library(tidytext)

nasa_title <- nasa_title %>% 
  unnest_tokens(word, title) %>%
  anti_join(stop_words)

nasa_desc <- nasa_desc %>%
  unnest_tokens(word, desc) %>%
  anti_join(stop_words)

nasa_title

nasa_desc


### 일부 초기 단순 탐사 
# 가장 흔한 단어는 무엇인가?
nasa_title %>%
  count(word, sort = TRUE)

nasa_desc %>% 
  count(word, sort = TRUE)

my_stopwords <- tibble(word = c(as.character(1:10), 
                                "v1", "v03", "l2", "l3", "l4", "v5.2.0", 
                                "v003", "v004", "v005", "v006", "v7"))

nasa_title <- nasa_title %>% 
  anti_join(my_stopwords)

nasa_desc <- nasa_desc %>% 
  anti_join(my_stopwords)

nasa_keyword %>% 
  group_by(keyword) %>% 
  count(sort = TRUE)

nasa_keyword <- nasa_keyword %>%
  mutate(keyword = toupper(keyword))


### 단어 동시 발생과 상관
# install.packages("widyr")
library(widyr)

title_word_pairs <- nasa_title %>%
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

title_word_pairs

desc_word_pairs <- nasa_desc %>% 
  pairwise_count(word, id, sort = TRUE, upper = FALSE)

desc_word_pairs

library(ggplot2)
# install.packages("igraph")
library(igraph)
installed.packages("ggraph")
library(ggraph)

set.seed(1234)

title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  theme_void()

set.seed(1234)

desc_word_pairs %>%
  filter(n >= 5000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


### 중요어 연결망 
keyword_pairs <- nasa_keyword %>% 
  pairwise_count(keyword, id, sort = TRUE, upper = FALSE)

keyword_pairs

set.seed(1234)

keyword_pairs %>%
  filter(n >= 700) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# 상관(correlation)
keyword_cors <- nasa_keyword %>% 
  group_by(keyword) %>%
  filter(n() >= 50) %>%
  pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)

keyword_cors

set.seed(1234)

keyword_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


### 설명 필드에 대한 tf-idf 계산
# 여기서 설명 필드 = 문서 = 말뭉치

desc_tf_idf <- nasa_desc %>%
  count(id, word, sort = TRUE) %>%
  ungroup() %>%
  bind_tf_idf(word, id, n)

desc_tf_idf %>%
  arrange(-tf_idf) %>%
  select(-id)

desc_tf_idf <- full_join(desc_tf_idf, nasa_keyword, by = "id")

desc_tf_idf %>%
  filter(!near(tf, 1)) %>%
  filter(keyword %in% c("SOLAR ACTIVITY", "CLOUDS", 
                        "SEISMOLOGY", "ASTROPHYSICS",
                        "HUMAN HEALTH", "BUDGET")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = TRUE) %>%
  top_n(15, tf_idf) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = keyword)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  coord_flip() + 
  labs(title = "Highest tf-idf words in NASA metadata description fields",
       caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = NULL, y = "tf-idf")


### 토픽 모델링
my_stop_words <- bind_rows(stop_words, 
                           tibble(word = c("nbsp", "amp", "gt", "lt",
                                           "timesnewromanpsmt", "font",
                                           "td", "li", "br", "tr", "quot",
                                           "st", "img", "src", "strong",
                                           "http", "file", "files",
                                           as.character(1:12)), 
                                  lexicon = rep("custom", 30)))

word_counts <- nasa_desc %>%
  anti_join(my_stop_words) %>%
  count(id, word, sort = TRUE) %>%
  ungroup()

