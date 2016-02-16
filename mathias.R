######### Preamble start ##########
#clear
rm(list = ls())
#workingdirect
wd = '~/Dropbox/tavilia'
setwd(wd)
# install af pakker
#install.packages("NLP", dependencies = TRUE)
#install.packages("tm", dependencies = TRUE)
#install.packages("SnowballC", dependencies = TRUE)
#install.packages("Slam", dependencies = TRUE)
#install.packages("wordcloud", dependencies = TRUE)
#install.packages("reshape2", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("ggmap", dependencies = TRUE)

#library usepackage
library(NLP)
library(tm)
#library(SnowballC)
#library(slam)
#library(wordcloud)
#library(reshape2)
#library(ggplot2) 
#library(ggmap)
######### Preamble end ##########
### Load Data
dd <- "~/Dropbox/tavilia/Data/documents"
#Corpus
avilia  <- Corpus(DirSource(dd, encoding = "latin1"), readerControl = list(language="PlainTextDocument"))
names(avilia) <- gsub("\\..*","",names(avilia))
### class labels
filenum <- gsub("\\D*","",names(avilia))
fileclass <- gsub("\\d+_","",names(avilia))
fileclass[fileclass == "lette"] = "letter" # correct for spelling error
#Data tabel: 
metadata <- read.csv(("~/Dropbox/tavilia/Meta Data/metadata.csv"), colClasses=c('character')) #fra num til char grundet sortering

for (j in 2:14) {
  tags <- names(metadata)
  for (i in 1:690) {
    meta(avilia[[i]], tag=tags[j]) <- metadata[i,j]
  }
} #Loop der indlæser meta data Anne
library(ggplot2)