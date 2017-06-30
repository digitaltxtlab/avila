library(magrittr)
library(tm)
library(plyr)
library(ggplot2)
library(scales)


# Load data
src <- "~/Google Drev/tavila/documents"
corpus <- Corpus(DirSource(src, encoding="latin1"), 
                 readerControl=list(Language="PlainTextDocument"))


# Metadata
meta_src <- "~/Google Drev/tavila/metadata.csv"
metadata <- read.csv(meta_src, stringsAsFactors = FALSE)


metadata$dokument.nr <- as.character(metadata$dokument.nr) # tm arranges by chars
metadata <- arrange(metadata, dokument.nr)
metadata[619,7] <- NA # reads as "" for unknown reasons
names(metadata) <- c("Number", "Name", "Type", "Year", "Month", "Day", "CitySend", 
                     "Receiver", "CityReceive", "Complete", "NChapters", "Book", "Notes", 
                     "Translator", "X", "Y", "Legend")

# cleaning
metadata$Type <- revalue(metadata$Type, c("sol"="soliloquy", "tes"="testimony"))

metadata$CityReceive <- gsub("^\\s+", "", metadata$CityReceive) %>%  # remove leading whitespace
    gsub("\\s+$", "", .)  # remove trailing whitespace
metadata$CityReceive <- revalue(metadata$CityReceive, c("Madrid?" = NA,
                                                        "Pastrana/Alcalá (?)" = NA,
                                                        "Tozos (Salamanca)" = "Salamanca",
                                                        "Quito" = "Quito (Ecuador)",
                                                        "Alba" = "Alba de Tormes",
                                                        "La Serna (Avila)" = "Avila",
                                                        "Alcalá" = "Alcalá de Henares"))

metadata$Receiver <- gsub("^\\s+", "", metadata$Receiver) %>% 
    gsub("\\s+$", "", .) %>% 
    gsub("\\(\\?\\)", "", .) # for mange modtagere til visualisering (og oprensning)

metadata$CitySend <- revalue(metadata$CitySend, c("Avila?" = NA,
                                                  "Toledo/Avila" = NA,
                                                  "Salamanca or Alba" = NA,
                                                  "Valladolid-Medina" = NA))

metadata$Complete <- mapvalues(metadata$Complete, c(0, 1), c("No", "Yes"))


# assigning metadata to corpus
for (j in 2:14) {
    tags <- names(metadata)
    for (i in 1:690) {
        meta(corpus[[i]], tag=tags[j]) <- metadata[i,j]
    }
}


# Overview plots
gg_year <- ggplot(metadata, aes(Year, fill=Type)) +
    geom_histogram(binwidth=1) +
    xlab("") +
    ylab("Number of texts")

gg_type <- ggplot(metadata, aes(Type, fill=Complete)) +
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    ggtitle("Type of text") +
    coord_flip()

gg_receiver <- ggplot(metadata, aes(Receiver, fill=Type)) +  # not helpful
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    ggtitle("Receiver") +
    coord_flip()

gg_receivercity <- ggplot(metadata, aes(CityReceive, fill=Type)) +
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    ggtitle("Receiver city") +
    coord_flip()

gg_sendercity <- ggplot(metadata, aes(CitySend, fill=Type)) +
    geom_bar() +
    xlab("") +
    ylab("Number of texts") +
    ggtitle("Sender city") +
    coord_flip()


# Word-production per year
words <- as.vector(NULL)
for (i in 1:690) {
    words[i] <- as.character(corpus[[i]][1]) %>% 
        strsplit(., " ") %>% 
        unlist() %>% 
        length()
}

yearword <- cbind(year = metadata$Year, format = metadata$Type, words = words) %>% 
    as.data.frame()
yearword <- aggregate(as.integer(yearword$words), list(yearword$year, yearword$format), sum)
names(yearword) <- c("Year", "Type", "Words")
yearword$Year <- paste0(yearword$Year, "-01-01") %>% 
    as.Date(., "%Y-%m-%d")

gg_yearword <- ggplot(yearword, aes(x=Year, y=Words, fill=Type)) +
    geom_bar(stat="identity") +
    xlab("") +
    ylab("Words") +
    ggtitle("Word production") +
    scale_x_date(labels = date_format("%Y"),
                 breaks = date_breaks("10 year"),
                 limits = c(as.Date("1543-01-01"), as.Date("1595-01-01")))

    


