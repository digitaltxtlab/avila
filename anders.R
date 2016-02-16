library(magrittr)
library(tm)
library(plyr)
library(ggplot2)
library(quanteda)

# Load data
src <- "~/Google Drev/tavila/documents"
corpus <- Corpus(DirSource(src, encoding="latin1"), 
                 readerControl=list(Language="PlainTextDocument"))

corpus[[3]] <- NULL # note: letter 100 = rapidminer-trash!


# Metadata
meta_src <- "~/Google Drev/tavila/metadata.csv"
metadata <- read.csv(meta_src)

metadata$dokument.nr <- as.character(metadata$dokument.nr) # tm arranges by chars
metadata <- arrange(metadata, dokument.nr)

for (j in 2:14) {
    tags <- names(metadata)
    for (i in 1:690) {
        meta(corpus[[i]], tag=tags[j]) <- metadata[i,j]
    }
}


### Overview

# Histogram
ggplot(metadata, aes(årstal)) +
    geom_histogram(binwidth=1) +
    xlab("") +
    ylab("Number of texts")

ggplot(metadata, aes(as.factor(modtager))) +  # clean up modtager-list?
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    coord_flip()

ggplot(metadata, aes(as.factor(modtagerby))) +
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    coord_flip()

ggplot(metadata, aes(as.factor(afsenderby))) +
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    coord_flip()

ggplot(metadata, aes(as.factor(metadata[,3]))) +
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    coord_flip()

