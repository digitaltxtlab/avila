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
corp <- corpus(corpus_src)

# Metadata
meta_src <- "~/Google Drev/tavila/metadata.csv"
metadata <- read.csv(meta_src)

metadata$dokument.nr <- as.character(metadata$dokument.nr) # tm arranges by chars
metadata <- arrange(metadata, dokument.nr)
names(metadata) <- c("Number", "Name", "Format", "Year", "Month", "Day", "CitySend", 
                     "Receiver", "CityReceive", "Incomplete", "NChapters", "Book", "Notes", 
                     "Translator", "X", "Y", "Legend")
for (i in 3:14) {
    docvars(corp, names(metadata)[i]) <- metadata[,i]
}
summary(corp, n=5, showmeta=TRUE)

# inspect data
summary(corp)
head(kwic(corp, "god")) # cool

# clean corpus
dfm_raw <- dfm(corp)
dfm_clean <- dfm(corp, 
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

# load dictionary
dict_src <- "~/Google Drev/tavila/dictionaries/BodyType/bodtyp.cat" # http://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/body-type-dictionary/
dict <- dictionary(file=dict_src, format="wordstat")

# dictionary
dfm_dict <- dfm(corp, dictionary=dict)

index_bt <- dfm_dict[1:690, 1:14] %>% 
    as.data.frame()
Barrier <- rowSums(index_bt[1:7])
Penetration <- rowSums(index_bt[8:14])
metadata <- cbind(metadata, Barrier, Penetration)
metadata$Date <- paste(metadata$Day, metadata$Month, metadata$Year, sep="-") %>% 
    as.Date(., format="%d-%m-%Y") # NA's if either d, m or y missing
metadata$Number <- as.integer(metadata$Number)


### visualize
# by text number
textdict <- metadata[,c("Number", "Barrier", "Penetration")] %>% 
    melt(., "Number")

gg_textdict_color <- ggplot(textdict, aes(x=Number, y=value, color=variable)) + # color
    geom_line() +
    labs(x="Text number", y="Score") +
    theme(legend.title=element_blank())

gg_textdict_facet <- ggplot(textdict, aes(x=Number, y=value)) + # facet
    geom_line() +
    facet_wrap(~ variable) +
    labs(x="Text number", y="Score")


# by year (aggregated)
yeardict <- metadata[,c("Year", "Barrier", "Penetration")] %>% 
    aggregate(. ~ Year, ., sum) %>% 
    melt(., "Year")

gg_yeardict_color <- ggplot(yeardict, aes(x=Year, y=value, color=variable)) + # color
    geom_line() +
    labs(x="Year", y="Score") +
    theme(legend.title=element_blank())

gg_yeardict_facet <- ggplot(yeardict, aes(x=Year, y=value)) + # facet
    geom_line() +
    facet_wrap(~ variable) +
    labs(x="Year", y="Score")

