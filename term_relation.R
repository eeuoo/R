tweets = searchTwitter(enc2utf8('마블'), n=100, lan='ko')
tweets

tdf = twListToDF(tweets)  

tw = unique(tdf$text)

tw = gsub("[[:cntrl:]]", "", tw)                    
tw = gsub("http[s]?://[[:alnum:].\\/]+", "", tw)    
tw = gsub("&[[:alnum:]]+;", "", tw)           
tw = gsub("@[[:alnum:]]+[:]?", "", tw)         
tw = gsub("[ㄱ-ㅎㅏ-ㅣ]","",tw)                  
tw = gsub('\\p{So}|\\p{Cn}', '', tw, perl = TRUE)  
tw = gsub("\\s{2,}", " ", tw)                 
tw = gsub("[[:punct:]]", "", tw)     
tw = gsub("RT","",tw)


wc = sapply(tw, extractNoun, USE.NAMES = F)

wc1 = table(unlist(wc))
names(wc1)
length(wc1)


# install.packages(c("arules", "igraph", "combinat", "arulesViz", "visNetwork"))

nouns = sapply(wc, unique)

nouns1 = sapply(nouns, function(x) {
  Filter(function(y='') { nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y) }, x)
})

wtrans = as(nouns1, "transactions")

rules = apriori(wtrans, parameter = list(supp=0.05, conf=0.5))

inspect(sort(rules))


# install.packages(c("arulesViz", "visNetwork"))
subrules2 <- head(sort(rules, by="lift"), 20)
ig <- plot( subrules2, method="graph", control=list(type="items") )
  

subrules2 <- head(sort(rules, by="confidence"), 30)
ig <- plot( subrules2, method="graph", control=list(type="items") )
            
            
# interactive
ig_df <- get.data.frame( ig, what = "both" )
visNetwork(
  nodes = data.frame(id = ig_df$vertices$name,
                     value = ig_df$vertices$support, ig_df$vertices),
  edges = ig_df$edges
) %>% visEdges(ig_df$edges) %>%visOptions( highlightNearest = T )
