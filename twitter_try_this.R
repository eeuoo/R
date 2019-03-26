mytweet = searchTwitter(enc2utf8('파이썬'), n=100, lan='ko', since='2019-03-11', until='2019-03-31')
class(mytweet)
tdf = twListToDF(mytweet)
colnames(tdf)
class(tdf)
tw = unique(tdf$text)


tw
tw = gsub("[[:cntrl:]]", "", tw)                      
tw = gsub("http[s]?://[[:alnum:].\\/]+", "", tw)    
tw = gsub("&[[:alnum:]]+;", "", tw)            
tw = gsub("@[[:alnum:]]+[:]?", "", tw)           
tw = gsub("[ㄱ-ㅎㅏ-ㅣ]","",tw)                  
tw = gsub('\\p{So}|\\p{Cn}', '', tw, perl = TRUE)   
tw = gsub("\\s{2,}", " ", tw)                  
tw = gsub("[[:punct:]]", "", tw)
tw = gsub("RT",'',tw)

?extractNoun

py = sapply(tw, extractNoun, USE.NAMES = F)
py1 = table(unlist(py))

names(py1)
length(py1)

py2 = head(sort(py1, decreasing = T), 100)


#### wordcloud #######
pal = brewer.pal(9, "Set1") 

wordcloud(names(py2), freq=py2, scale=c(5,0.5), rot.per=0.25, 
          min.freq = 1, random.order = F, random.color = T, colors = pal)
