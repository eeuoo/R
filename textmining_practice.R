# install.packages('tm')
library(tm)

# 조회 및 저장 ####
getSources()
getReaders()

folder = system.file("texts", "txt", package = "tm")
txtSource = DirSource(folder)
doc = VCorpus(txtSource, readerControl = list(language = 'lat'))
class(doc)
summary(doc)

meta(doc)
meta(doc, type = 'local')
inspect(doc)
inspect(doc[1])
doc[[1]]
doc[[1]][1]

names(doc)
writeCorpus(doc, path='data', filenames = names(doc))


# 전처리 #####
getTransformations()
doc = tm_map(doc, stripWhitespace)
doc[[1]][1]

data('crude')
crude
crude[[1]]
crude[[1]][1]

crude = tm_map(crude, stripWhitespace)
crude = tm_map(crude, content_transformer(tolower))
crude = tm_map(crude, removePunctuation)
crude = tm_map(crude, removeWords, stopwords("english"))
crude = tm_map(crude, stripWhitespace)
# install.packages('SnowballC')
crude = tm_map(crude, stemDocument, language = 'english')


# Term & Document Matrix ####
tdm = TermDocumentMatrix(crude)  
tdm
rownames(tdm)
tail(as.matrix(tdm))
tdm['year',] 
View(as.matrix(tdm)) 
dimnames(tdm)    

tdm$i
tdm$j
tdm$v

tdm = removeSparseTerms(tdm, 0.8)
tdm
tail(as.matrix(tdm))
rownames(tdm)

dtm = t(tdm)
head(as.matrix(dtm))
inspect(tdm[1:5, 1:10])


# 빈도 분석 ####
findFreqTerms(tdm, 20)
findFreqTerms(tdm, 20, 30)
findFreqTerms(tdm, 0, 10)

findAssocs(tdm, "last", 0.5)
findAssocs(tdm, "oil", .7)

rowSums(as.matrix(tdm))
wFreq = sort(rowSums(as.matrix(tdm)), decreasing = T)
names(wFreq)
wFreq > 10

wFreq = subset(wFreq, wFreq > 10)

# word cloud ####
# install.packages('RColorBrewer')  
library(RColorBrewer)

display.brewer.all()
brewer.pal.info
pa = brewer.pal(8, 'Blues')
darks = brewer.pal(8, 'Dark2')

# install.packages("wordcloud")
library(wordcloud)
wordcloud(words = names(wFreq), freq = wFreq, min.freq = 10, 
          random.order = F, colors = brewer.pal(9,'Greens'))


# try this ####
doc = VCorpus(txtSource, readerControl = list(language = 'lat'))

# Corpus  전처리
doc[[1]][1]
doc = tm_map(doc, stripWhitespace)
doc = tm_map(doc, content_transformer(tolower))
doc = tm_map(doc, removePunctuation)
doc = tm_map(doc, removeWords, stopwords("english"))
doc = tm_map(doc, stripWhitespace)
doc = tm_map(doc, stemDocument, language = 'english')

# Term & Document Matrix
doc = TermDocumentMatrix(doc)  
class(doc)
head(as.matrix(doc))
rownames(doc)

# 빈도 분석
doc = removeSparseTerms(doc, 0.8)
findFreqTerms(doc, 0, 10)
docfreq = sort(rowSums(as.matrix(doc)), decreasing = T)
names(docfreq)
rowSums(as.matrix(docfreq))

# word cloud
wordcloud(words = names(docfreq), freq = docfreq, min.freq = 3, scale = c(5, 2),
          random.order = F, rot.per = 0.2,
          colors = brewer.pal(9,'Greens'))

