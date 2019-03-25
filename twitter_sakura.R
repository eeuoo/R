mytweet = searchTwitter(enc2utf8('벚꽃'), n=100, lan='ko')

tdf = twListToDF(mytweet)
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


py = sapply(tw, extractNoun, USE.NAMES = F)
py1 = table(unlist(py))

names(py1)
length(py1)

py2 = head(sort(py1, decreasing = T), 100)


#### wordcloud #######
pal = brewer.pal(9, "Set1") 

wordcloud(names(py2), freq=py2, scale=c(5,0.5), 
          min.freq = 2, random.order = F, random.color = T, colors = pal)
