library(ggplot2)
mpg = as.data.frame(ggplot2::mpg)


# mpg데이터에서 차종(class)가 suv, compact인 자동차의 모델과 연비관련 변수만 출력하세요.
mpg %>% filter(class == c('suv' ,'compact') ) %>%  select(class, model, cty, hwy)


# mpg데이터에서 고속도로연비(hwy) 1 ~ 5위에 해당하는 자동차의 데이터를 출력하세요.
mpg %>% arrange( desc(hwy) ) %>% head(5)


# 회사별로 suv 차들의 통합연비(평균) 구해 1 ~ 5위를 출력하세요.
mpg %>% group_by(manufacturer) %>%
        filter(class=='suv') %>%
        summarise(c_h_mean = mean((cty + hwy)/2)) %>%
        arrange(desc(c_h_mean)) %>% head(5)


# 다음과 같이 연료별 가격이 정해져 있을 때, mpg에 fl_price라는 컬럼을 추가하세요.

# fl   type    price
# c   CNG     1.33
# d   diesel  1.02
# e   E85     0.92
# p   Premi   1.99
# r   Reqular 1.22

fuel = data.frame(fl=c('c','d','e','p','r'),
                  type=c('CNG','diesel','E85','Premi','Reaular'),
                  price=c(1.33, 1.02, 0.92, 1.99, 1.22), stringsAsFactors = F)

mpg = inner_join(mpg, fuel, by=('fl'='fl')) %>% 
      select(-type) %>%
      dplyr::rename( fl_price= price )

mpg
