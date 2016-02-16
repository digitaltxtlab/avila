library(magrittr)
library(tm)


# Load data
src <- "~/Google Drev/tavila/documents"
corpus <- Corpus(DirSource(src, encoding="latin1"), 
                 readerControl=list(Language="PlainTextDocument"))

corpus[[3]] <- NULL # note: letter 100 = rapidminer-trash!


# Metadata
meta_src <- "~/Google Drev/tavila/metadata.csv"
metadata <- read.csv(meta_src)
#install.packages("plyr")
library(plyr)
metadata$dokument.nr <- as.character(metadata$dokument.nr) # tm's rækkefølge er efter chars
metadata <- arrange(metadata, dokument.nr)

for (j in 2:14) {
    tags <- names(metadata)
    for (i in 1:690) {
        meta(corpus[[i]], tag=tags[j]) <- metadata[i,j]
    }
}