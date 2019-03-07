# install.packages('dplyr')
library('dplyr')

data = read.csv('data/score.csv')
data = dplyr::rename(data, math=수학)
head(data)

data$group = sample(rep(LETTERS[1:3], times=160), replace = F, size = nrow(data))

# filter, select #####
data %>% filter(group == 'C')
data %>% filter(group == 'C' & math > 90)
data %>% filter(group %in% c('A','C'))

data %>% select(-반, -영어)
data %>% filter(math>95) %>% select(학번, 국어, 영어, math)

# arrange, mutate #####
data = dplyr::rename(data, kor=국어, sci=과학, eng=영어, art=예체)
data = dplyr::rename(data, stuno = 학번, cls = 반, gen=성별)

data %>% arrange(math) %>% head
data %>% arrange(math, kor, eng) %>% head

data = data %>% mutate(subTotal = kor + eng + math,
                       subMean = round((kor + eng + math)/3))
data %>%
  mutate(kor_eng = kor + eng) %>%
  arrange(desc(kor_eng)) %>%
  head

# summarize, group_by ####
data %>% dplyr::summarise(t = mean(math))
data %>% group_by(cls, gen) %>%
         summarise(m = mean(math))

data %>% group_by(cls) %>% 
         summarise(mean_math = mean(math),
                   sum_math = sum(math),
                   medi_math = median(math),
                   var_math = var(math),
                   n_math = n()) %>%
         arrange(desc(mean_math))

data %>% group_by(cls, gen) %>% 
  summarise(mean_math = mean(math),
            sum_math = sum(math),
            medi_math = median(math),
            var_math = var(math),
            n_math = n()) %>%
  group_by(gen) %>%
  summarise(mean = mean(mean_math)) %>%
  arrange(desc(mean))

# join #####
dfsum = cbind( data.frame(yno=1:4, year=2016:2019), 
               matrix(round(runif(16), 3) * 1000, ncol=4, dimnames = list(NULL, paste0('Q', 1:4))))

sales = cbind( data.frame(no=1:12, year=2016:2019), 
               matrix(round(runif(144), 3) * 100000, ncol=12, dimnames = list(NULL, month.abb)) )

left_join(sales, dfsum, by=c('year'='year'))
right_join(sales, dfsum, by=c('year'='year'))

inner_join(sales, dfsum, by=c('year'='year', 'no'='yno'))
semi_join(sales, dfsum, by=c('year'='year', 'no'='yno'))

full_join(sales, dfsum, by=c('year'='year', 'no'='yno'))
anti_join(sales, dfsum, by=c('no'='yno'))

# bind_rows, bind_cols ####
topsale4 = sales[1:4, ] %>% select(year, Jan, Apr, Jul, Oct)
top4 = sales[5:8, ] %>%
          select(1:4, year, Jan, Apr, Jul, Oct) %>%
          dplyr::rename(yno=no, Q1=Jan, Q2=Apr, Q3=Jul, Q4=Oct)
topsale4
top4

bind_rows(dfsum, topsale4)
bind_rows(dfsum, top4)
bind_rows(dfsum, top4, .id = 'group')

bind_cols(dfsum,top4)
cbind(dfsum, top4)
bind_cols(dfsum, top4) %>% select(-year1, -yno1, -Feb)
