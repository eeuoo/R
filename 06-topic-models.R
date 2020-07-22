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

# install.packages("tidyr")
library(tidyr)

beta_spread <- ap_topics %>% 
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2/ topic1))

beta_spread


### 문서-토픽 확률
ap_documents <- tidy(ap_lda, matrix = "gamma")

ap_documents

tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))


### 예제 : 대도서관 강도
# 토픽 모델링을 사용해 각 장이 어떻게 개별 토픽으로 군집화되는지를 알아낸다

titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
           "Pride and Prejudice", "Great Expectations")

# install.packages("gutenbergr")
library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

library(stringr)

# 각기 1개 장을 대표하는 문서들로 나눈다
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# 단어들로 분리한다
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# 문서-단어 카운트를 알아낸다
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))

chapters_lda

chapters_topics <- tidy(chapters_lda, matrix = "beta")

chapters_topics

top_terms <- chapters_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()


### 문서당 분류
# r(감마) : 토픽별 문서당 확률
# 어떤 토픽이 각 문서과 관련되어 있는가?

chapters_gamma <- tidy(chapters_lda, matrix = "gamma")

chapters_gamma 

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

# 그림을 그리기 전에 토픽 1, 토픽 2 등의 순서에 따라 제목(title)의 순서를 바꾼다
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ title)

chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)


### 단어별 할당 : augment
# 각 문서의 각 단어를 토픽에 할당(assigning)

assignments <- augment(chapters_lda, data = chapters_dtm)

assignments

assignments <- assignments %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE) %>%
  inner_join(book_topics, by = c(".topic" = "topic"))

assignments

library(scales)

assignments %>%
  count(title, consensus, wt = count) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "red", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

# 어떤 단어가 잘못 할당되었나?
wrong_words <- assignments %>%
  filter(title != consensus)

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

wrong_words

wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))

word_counts %>%
  filter(word == "flopson")


### 대체 LDA 구현
# install.packages("mallet")
library(mallet)

# 1개 장당 1개 문자열을 사용해 벡터를 생성
collapsed <- by_chapter_word %>%
  anti_join(stop_words, by = "word") %>%
  mutate(word = str_replace(word, "'", "")) %>%
  group_by(document) %>%
  summarize(text = paste(word, collapse = " "))

# 불용어를 넣을 빈 파일 생성
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)

# 단어-토필 쌍들
tidy(mallet_model)

# 문서-토픽 쌍들
tidy(mallet_model, matrix = "gamma")

# 'augment'에 대한 열의 이름이 'term'으로 되어야 한다.
term_counts <- rename(word_counts, term = word)
augment(mallet_model, term_counts)
