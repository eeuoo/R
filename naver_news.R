# install.packages(c('rvest', 'httr', 'stringr'))


# 1. http://www.ize.co.kr/features.html에 있는 기사를 수집하시오.


URL = 'http://www.ize.co.kr/features.html'

html = read_html(URL)

links = html_attr(html_nodes(html, '.lst_type7  ul  li a'), 'href')

links = links[!is.na(links)]

news = list()

for (i in 1:length(links)) {
  try({
    htxt = read_html(paste0('http://www.ize.co.kr', links[i]))
    comments = html_nodes(htxt, '.article_body')
    # repair_encoding(html_text(comments), from='utf-8')
    get_news = repair_encoding(html_text(comments))
    news[i] = str_trim(get_news)
  }, silent = F)
}

for ( i in 1:length(news)){
  news[[i]][1] = removeStopword(news[[i]][1] )
}
  
removeStopword = function(t) {
  t = gsub("[[:cntrl:]]", "", t) 
  t = gsub("http[s]?://[[:alnum:].\\/]+", "", t) 
  t = gsub("&[[:alnum:]]+;", "", t)
  t = gsub("@[[:alnum:]]+", "", t)
  t = gsub("@[[:alnum:]]+[:]?", "", t)
  t = gsub("[ㄱ-ㅎㅏ-ㅣ]","",t) 
  t = gsub("\\s{2,}", " ", t) 
  t = gsub("[[:punct:]]", "", t)  
  t = gsub("https", "", t)
  # t = gsub("RT", "", t)
  t = gsub("\\s{2,}", " ", t) 
  # mac: emo 제거
  gsub('\\p{So}|\\p{Cn}', '', t, perl = TRUE)
}

# 2. 수집 된 뉴스로 WordCloud를 작도하시오.

wc = sapply(news, extractNoun, USE.NAMES = F)
wc1 = unlist(wc)
wc1 = wc1[ nchar(wc1) > 1 ]
wc1 = table(wc1)
wc2 = head(sort(wc1, decreasing = T), 100)

pal = brewer.pal(9, "Set1")

wordcloud(names(wc2), freq=wc2, scale=c(5,0.5),  min.freq = 1, 
          random.order = F, random.color = T, colors = pal)



# 3. 수집 된 뉴스로 연관성분석을 하시오.

nouns1 = sapply(wc, function(x) {
  Filter(function(y='') { nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y) }, x)
})

wtrans = as(nouns1, "transactions")

rules = apriori(wtrans, parameter = list(supp=0.5, conf=0.5))

# 작도
subrules2 <- head(sort(rules, by="confidence"), 30)
ig <- plot( subrules2, method="graph", control=list(type="items") )

# interactive
ig_df <- get.data.frame( ig, what = "both" )
visNetwork(
  nodes = data.frame(id = ig_df$vertices$name,
                     value = ig_df$vertices$support, ig_df$vertices),
  edges = ig_df$edges
) %>% visEdges(ig_df$edges) %>%visOptions( highlightNearest = T )
