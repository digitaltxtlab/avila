##### Preamble
rm(list = ls())
wd = '~/Dropbox/tavilia'
setwd(wd)
#Pakker
#install.packages('igraph')
library(igraph)
####### Loader Data
dd <- "~/Dropbox/tavilia/Data/documents"
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