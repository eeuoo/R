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