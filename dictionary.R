# dictionary pipeline
library(magrittr)
library(quanteda)   # devtools::install_github("kbenoit/quantedaData")
library(stringr)

# load data
src <- "~/Google Drev/tavila/documents/*.txt"
corpus_src <- textfile(src, encodingFrom="latin1")
corp <- corpus(corpus_src)

# add docvars
meta <- summary(corp, n=1000)$Text %>% 
    as.character() %>% 
    strsplit(., c("_")) %>% 
    unlist() %>% 
    gsub(".txt", "", .)

docvars(corp, "Number") <- meta[seq(1, length(meta), 2)]
docvars(corp, "Type") <- meta[seq(2, length(meta), 2)]
docvars(corp)
summary(corp, n=5, showmeta=TRUE)
########## Map .csv variables as docvars (or metadoc()?)

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
topfeatures(dfm_clean, 20)
wordcloud <- topfeatures(dfm_clean, 100) %>%
    plot()
plot(dfm_clean, max.words=100)

# load dictionary
dict_src <- "~/Google Drev/tavila/dictionaries/BodyType/bodtyp.cat" # http://provalisresearch.com/products/content-analysis-software/wordstat-dictionary/body-type-dictionary/
dict <- dictionary(file=dict_src, format="wordstat")

# dictionary
dfm_dict <- dfm(corp, dictionary=dict)
dfm_dict 
########## is the dictionary loaded/interpreted correctly by quanteda?
