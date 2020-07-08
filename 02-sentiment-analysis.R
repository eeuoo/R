# 정서분석

library(knitr)
opts_chunk$set(message = FALSE, warning = FALSE, cache = TRUE)
options(width = 100, dplyr.width = 100)
library(ggplot2)
theme_set(theme_light()

library(tidytext)

sentiments

load("~/workspace/R/afinn.rda")
afinn

get_sentiments("bing")

load("~/workspace/R/nrc.rda")
nrc

library(janeaustenr)
library(dplyr)
library(stringr)

# 텍스트 한 행에 한 단어씩 정돈
# 정서 용어집과 불용어 데이터셋에 word 열이 있기 때문에 join 하기 쉽도록 unnest_tokens()에 word라는 이름을 붙임
tidy <- austen_books() %>% 
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text) 

# 정서분석 (기쁨)
nrc_joy <- nrc %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


# 80줄을 단위로 index 정의
library(tidyr)

jane_austen_sentiment <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>% 
  count(book, index = linenumber %/% 80, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

# 각 플롯의 정서 서사 시각화
library(ggplot2)

ggplot(jane_austen_sentiment, aes(index, sentiment, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free_x")


