library(magrittr)
library(tm)
library(plyr)
library(ggplot2)
library(scales)

# Load data
src <- "~/Google Drev/tavila/documents"
corpus <- Corpus(DirSource(src, encoding="latin1"), 
                 readerControl=list(Language="PlainTextDocument"))

# corpus[[3]] <- NULL # note: letter 100 = rapidminer-trash!


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


# Overview plots
gg_year <- ggplot(metadata, aes(årstal)) +
    geom_histogram(binwidth=1) +
    xlab("") +
    ylab("Number of texts")

gg_receiver <- ggplot(metadata, aes(as.factor(modtager))) +  # clean up modtager-list?
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    ggtitle("Receiver") +
    coord_flip()

gg_receivercity <- ggplot(metadata, aes(as.factor(modtagerby))) +
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    ggtitle("Receiver city") +
    coord_flip()

gg_sendercity <- ggplot(metadata, aes(as.factor(afsenderby))) +
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    ggtitle("Sender city") +
    coord_flip()

gg_type <- ggplot(metadata, aes(as.factor(metadata[,3]))) +
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    ggtitle("Type of text") +
    coord_flip()

# Word-production per year
words <- as.vector(NULL)
for (i in 1:690) {
    words[i] <- as.character(corpus[[i]][1]) %>% 
        strsplit(., " ") %>% 
        unlist() %>% 
        length()
}

yearword <- cbind(year = metadata$årstal, words) %>% 
    as.data.frame()
yearword <- aggregate(yearword$words, list(yearword$year), sum)
names(yearword) <- c("year", "words")
yearword$year <- paste0(yearword$year, "-01-01") %>% 
    as.Date(., "%Y-%m-%d")

gg_yearword <- ggplot(yearword, aes(x=year, y=words)) +
    geom_bar(stat="identity") +
    xlab("") +
    ylab("Words") +
    scale_x_date(labels = date_format("%Y"),
                 breaks = date_breaks("3 year"))


