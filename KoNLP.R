##### java setting on mac ########

# 0. dylib 위치
paste0(system2('/usr/libexec/java_home', stdout = TRUE), '/jre/lib/server/libjvm.dylib')

# 1. load dylib
dyn.load(paste0(system2('/usr/libexec/java_home', stdout = TRUE), 
                '/jre/lib/server/libjvm.dylib'))



tweets = searchTwitter(enc2utf8('승리'), n=100, lan='ko', 
                       since='2019-03-11', until='2019-03-31')

tdf = twListToDF(tweets)


# tdf = tdf %>% filter(!isRetweet) %>% filter(favoriteCount > 2)
tdf = tdf %>% filter(regexpr('광고',text) == -1)
tw = unique(tdf$text)

tw
tw = gsub("[[:cntrl:]]", "", tw)                      # 제어문자(\n, \t등) 제거
tw = gsub("http[s]?://[[:alnum:].\\/]+", "", tw)     # link 제거
tw = gsub("&[[:alnum:]]+;", "", tw)            # escape(&amp; &lt;등) 제거
tw = gsub("@[[:alnum:]]+[:]?", "", tw)             # 트위터 계정 부분 제거
tw = gsub("[ㄱ-ㅎㅏ-ㅣ]","",tw)                   # 한글 불용어(ㅋㅋㅎㅎ ㅠㅜ등) 제거
tw = gsub("<.*>", "", enc2native(tw))          # EMO(/U00000f등) 제거 (windows)
tw = gsub('\\p{So}|\\p{Cn}', '', tw, perl = TRUE)    # EMO(/U00000f등) 제거 (mac)
tw = gsub("\\s{2,}", " ", tw)                  # 2개이상 공백을 한개의 공백으로 처리
tw = gsub("[[:punct:]]", "", tw)
tw = gsub("RT",'',tw)


#### 명사 추출 ######

?extractNoun

wc = sapply(tw, extractNoun, USE.NAMES = F)
wc1 = table(unlist(wc))

# 한 글자 단어 제외
ul = unlist(wc)
ul = ul[ nchar(ul) > 1 ]
wc1 = table(ul)

names(wc1)
length(wc1)

wc2 = head(sort(wc1, decreasing = T), 100)


#### wordcloud #######
pal = brewer.pal(9, "Set1") 

wordcloud(names(wc2), freq=wc2, scale=c(5,0.5), rot.per=0.25, 
            min.freq = 1, random.order = F, random.color = T, colors = pal)
