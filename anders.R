library(magrittr)
library(tm)
<<<<<<< HEAD
library(plyr)
library(ggplot2)
library(quanteda)
=======

>>>>>>> 9ac40994ca013ecacf44fbe01b853cb39526ea3e

# Load data
src <- "~/Google Drev/tavila/documents"
corpus <- Corpus(DirSource(src, encoding="latin1"), 
                 readerControl=list(Language="PlainTextDocument"))

corpus[[3]] <- NULL # note: letter 100 = rapidminer-trash!


# Metadata
meta_src <- "~/Google Drev/tavila/metadata.csv"
metadata <- read.csv(meta_src)
<<<<<<< HEAD

metadata$dokument.nr <- as.character(metadata$dokument.nr) # tm arranges by chars
=======
#install.packages("plyr")
library(plyr)
metadata$dokument.nr <- as.character(metadata$dokument.nr) # tm's rækkefølge er efter chars
>>>>>>> 9ac40994ca013ecacf44fbe01b853cb39526ea3e
metadata <- arrange(metadata, dokument.nr)

for (j in 2:14) {
    tags <- names(metadata)
    for (i in 1:690) {
        meta(corpus[[i]], tag=tags[j]) <- metadata[i,j]
    }
<<<<<<< HEAD
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

=======
}
>>>>>>> 9ac40994ca013ecacf44fbe01b853cb39526ea3e
