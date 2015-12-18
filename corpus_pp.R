rm(list = ls())
wd <- "C:/Users/KLN/Documents/projects/avilla"
dd <- "C:/Users/KLN/Documents/projects/avilla/documents"
# packages
library(tm)
avilaimport  <- Corpus(DirSource(dd, encoding = "UTF-8"), readerControl = list(language="PlainTextDocument"))
names(avilaimport) <- gsub("\\..*","",names(avilaimport))
### class labels
filenum <- gsub("\\D*","",names(avilaimport))
fileclass <- gsub("\\d+_","",names(avilaimport))
fileclass[fileclass == "lette"] = "letter" # correct for spelling error

# visualize corpus classes
cl <- unique(fileclass)
cloc <- matrix(0,nrow = length(cl), ncol = 1)
for(i in 1:length(cl)){
  cloc[i,] <- sum(fileclass == cl[i])
}
cloc <- sort(cloc, decreasing = TRUE)
cl <- cl[order(cloc, decreasing = TRUE)]
dev.new()
barplot(cloc,  xlab = 'Document Class', ylab = 'Corpus Frequency', main = 'Avila Corpus', names.arg = cl)
rm(cloc, cl, i)

### preprocessing
avila1 <- tm_map(avilaimport, removePunctuation)
avila1 <- tm_map(avila1, removeNumbers) 
avila1 <- tm_map(avila1, content_transformer(tolower))
avila1 <- tm_map(avila1, removeWords, stopwords("english"))
avila1 <- tm_map(avila1, stripWhitespace)
avila1 <- tm_map(avila1, stemDocument, language = "english")# Porter's algorithm
avila1 <- tm_map(avila1, PlainTextDocument)
names(avila1) <- names(avilaimport)
### build document term matrix
a1dtm <- DocumentTermMatrix(avila1)
rownames(a1dtm) <- names(avilaimport)

## reduce sparsity
docsparse <- function(mindocs,dtm){
  n = length(row.names(dtm))
  sparse <- 1 - mindocs/n;
  dtmreduce <- removeSparseTerms(dtm, sparse)
  return(dtmreduce)
}
a1dtm <- docsparse(10,a1dtm)

# transform to matrix
library(reshape2)
a1mat <- as.matrix(a1dtm)
a1matdense <- melt(a1mat, value.name = "count")

# plot number of words and document length
dev.new()
doclen <- sort(rowSums(a1mat), decreasing = TRUE)
wordfreq <- sort(colSums(a1mat), decreasing = TRUE)
head(wordfreq)
par(mfrow=c(1,2))
plot(log(1:length(wordfreq)),log(wordfreq), main = "Vocabulary", xlab = "Index", ylab = "Word frequency",bty="n")
plot(doclen, main = "Document length", ylab = "Number of words", bty="n")

##### visualize most frequent words
v.v <- sort(colSums(a1mat),decreasing=TRUE)
wordlist <- data.frame(word = names(v.v), freq = v.v)
library(wordcloud)
require(RColorBrewer)
pal2 <- brewer.pal(8,"Dark2")
dev.new()
wordcloud(wordlist$word,wordlist$freq, scale=c(8,.2),min.freq=50,
          max.words=100, random.order=FALSE, rot.per=.15, colors=pal2)

### save corpus and dtm
rm(dd,pal2,v.v,wd)
save.image(file = "avilaCorpusPp.RData")
