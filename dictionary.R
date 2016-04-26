# dictionary pipeline
library(magrittr)
library(plyr)
library(reshape2)
library(quanteda)   # devtools::install_github("kbenoit/quantedaData")
library(stringr)
library(ggplot2)

# load data
src <- "~/Google Drev/tavila/documents/*.txt"
corpus_src <- textfile(src, encodingFrom="latin1")
corpus <- corpus(corpus_src)

# metadata
meta_src <- "~/Google Drev/tavila/metadata.csv"
metadata <- read.csv(meta_src)

metadata$dokument.nr <- as.character(metadata$dokument.nr) # tm arranges by chars
metadata <- arrange(metadata, dokument.nr)
names(metadata) <- c("Number", "Name", "Format", "Year", "Month", "Day", "CitySend", 
                     "Receiver", "CityReceive", "Incomplete", "NChapters", "Book", "Notes", 
                     "Translator", "X", "Y", "Legend")

# cleaning
metadata$Format <- revalue(metadata$Format, c("sol"="soliloquy", "tes"="testimony"))

# assigning metadata to corpus
for (i in 3:14) {
    docvars(corpus, names(metadata)[i]) <- metadata[,i]
}
summary(corpus, n=5, showmeta=TRUE)

# inspect data
summary(corpus)
head(kwic(corpus, "god")) # cool

# clean corpus
dfm_raw <- dfm(corpus)
dfm_clean <- dfm(corpus, 
                 ignoredFeatures=stopwords("english"),
                 stem=TRUE,
                 removePunct=TRUE,
                 removeSeparators=TRUE,
                 toLower=TRUE)

# normalize
norm_length <- median(rowSums(dfm_raw))
docs <- texts(corpus) %>% 
    cbind("year" = metadata$Year, "doc" = .) %>% 
    aggregate(data=., doc~year, FUN=paste, collapse=' ')

Sys.setlocale(locale="C")
norm_doc <- NULL
for (i in 1:dim(docs)[1]) {
    # split docs into median-length batches
    temp_doc <- strsplit(docs[i,2], " ") %>% 
        unlist(., use.names=FALSE) %>% 
        split(., ceiling(seq_along(.)/norm_length)) %>% 
        ldply(., data.frame) %>% 
        aggregate(data=., .[,2]~.[,1], FUN=paste, collapse=" ")

    # add year
    temp_doc <- rep(docs[i,1], dim(temp_doc)[1]) %>% 
        cbind(., temp_doc)
    
    names(temp_doc) <- c("year", "batch", "doc")

    norm_doc <- rbind(norm_doc, temp_doc, make.row.names=FALSE)
}

rm(temp_doc)
norm_doc$batch <- as.numeric(norm_doc$batch)
norm_doc <- arrange(norm_doc, year, batch)

# cut out snippets of length < norm_length
short <- NULL
for (i in 1:dim(norm_doc)[1]) {
    short[i] <- (strsplit(norm_doc[i,3], " ") %>% 
         unlist(., use.names=FALSE) %>% 
         length()) < norm_length
}
sum(short) # 23 snippets < norm_length
norm_doc <- norm_doc[!short,] # remove short snippets

# convert to corpus, dfm
norm_corpus <- corpus(norm_doc[,3], docvars = data.frame())

dfm_raw_norm <- dfm(norm_corpus)
dfm_clean_norm <- dfm(norm_corpus, 
                      ignoredFeatures=stopwords("english"),
                      stem=TRUE,
                      removePunct=TRUE,
                      removeSeparators=TRUE,
                      toLower=TRUE)


# inspect dfm
topfeatures(dfm_clean, 20) # will = stopword?
wordcloud <- topfeatures(dfm_clean, 100) %>%
    plot() # plot frequency of top 100 features
plot(dfm_clean, max.words=100) # wordcloud

### dictionaries
# load dictionary
dict_src <- "~/Google Drev/tavila/dictionaries/BodyType/bodtyp.cat" # http://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/body-type-dictionary/
dict <- dictionary(file=dict_src, format="wordstat")

dict_liwc2001 <- "~/Google Drev/tavila/dictionaries/LIWC/LIWC2001_English.dic"  # 2001
dict_liwc2007 <- "~/Google Drev/tavila/dictionaries/LIWC/LIWC2007_English100131.dic"  # 2007
dict_liwc <- dictionary(file=dict_liwc2007, format="LIWC")

# provalis
provalis <- dfm(corpus, dictionary=dict) %>% 
    as.data.frame()
provalis_norm <- dfm(norm_corpus, dictionary=dict) %>% 
    as.data.frame()
provalis_norm_all <- cbind("year"=as.integer(as.character(norm_doc$year)), provalis_norm)

provalis <- cbind("barrier"=rowSums(provalis[,1:7]), 
                  "penetration"=rowSums(provalis[,8:14]))
provalis_norm <- cbind("barrier"=rowSums(provalis_norm[,1:7]), 
                       "penetration"=rowSums(provalis_norm[,8:14]))
metadata <- cbind(metadata, provalis)
norm_doc <- cbind(norm_doc, provalis_norm)

# liwc
liwc <- dfm(corpus, dictionary=dict_liwc) %>% 
    as.data.frame()
liwc_norm <- dfm(norm_corpus, dictionary=dict_liwc) %>% 
    as.data.frame()

personal <- c("work", "leisure", "home", "money", "relig", "death")
affect <- c("negemo", "posemo", "anx", "anger", "sad")
cogproc <- c("insight", "cause", "discrep", "tentat", "certain")
perproc <- c("see", "hear", "feel")
bioproc <- c("body", "health", "sexual", "ingest")

metadata <- cbind(metadata, liwc[, c(personal, affect, cogproc, perproc, bioproc)])
norm_doc <- cbind(norm_doc, liwc_norm[, c(personal, affect, cogproc, perproc, bioproc)])
norm_doc$year <- as.integer(as.character(norm_doc$year))

# visualize normalized dict scores (per year)
norm_doc_vis <- norm_doc[, -c(2, 3)] %>% 
    aggregate(data=., .~year, mean)

# provalis
provalis_vis <- melt(norm_doc_vis[, c(1, 2:3)], "year")
ggplot(provalis_vis, aes(year, value, color=variable)) +
    geom_line() +
    labs(x="Year", y="Score") +
    theme(legend.title=element_blank())

provalis_vis_all <- melt(provalis_norm_all[, c(1, 2:6, 9:15)], "year") %>% 
    aggregate(data=., value~variable+year, mean)
provalis_vis_all <- cbind(colsplit(tolower(provalis_vis_all$variable), " ", c("type", "category")), provalis_vis_all)
provalis_vis_all$category <- gsub(x=provalis_vis_all$category, "total.", "")
provalis_vis_all$variable <- NULL

ggplot(provalis_vis_all, aes(year, value, color=category)) +
    geom_line() +
    facet_wrap(~type) +
    labs(x="Year", y="Score") +
    scale_color_brewer(palette="Paired") +
    theme(legend.title=element_blank())

# LIWC
personal_vis <- melt(norm_doc_vis[, c(1, 4:9)], "year")
ggplot(personal_vis, aes(year, value, color=variable)) +
    geom_line() +
    labs(x="Year", y="Score") +
    theme(legend.title=element_blank())

affect_vis <- melt(norm_doc_vis[, c(1, 10:14)], "year")
ggplot(affect_vis, aes(year, value, color=variable)) +
    geom_line() +
    labs(x="Year", y="Score") +
    theme(legend.title=element_blank())

cogproc_vis <- melt(norm_doc_vis[, c(1, 15:19)], "year")
ggplot(cogproc_vis, aes(year, value, color=variable)) +
    geom_line() +
    labs(x="Year", y="Score") +
    theme(legend.title=element_blank())

perproc_vis <- melt(norm_doc_vis[, c(1, 20:22)], "year")
ggplot(perproc_vis, aes(year, value, color=variable)) +
    geom_line() +
    labs(x="Year", y="Score") +
    theme(legend.title=element_blank())

bioproc_vis <- melt(norm_doc_vis[, c(1, 23:26)], "year")
ggplot(bioproc_vis, aes(year, value, color=variable)) +
    geom_line() +
    labs(x="Year", y="Score") +
    theme(legend.title=element_blank())


# visualize unnormalized dict scores (per year)



# normalisér ved at klippe tekster op med median-tekstens længde (drop residual-tekst per værk)
# kør LIWC og barrier/penetration; 
# mean score per år
# evt. standardiser scores (0-1)
# plot scores per år for alle kategorier







