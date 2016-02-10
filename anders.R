library(tm)

# Load data
src <- "~/Google Drev/tavila/documents"
corpus <- Corpus(DirSource(src, encoding="latin1"), 
                 readerControl=list(Language="PlainTextDocument"))
