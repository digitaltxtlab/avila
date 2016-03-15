######### Preamble start ##########
#clear
rm(list = ls())
#workingdirect
wd = '~/Dropbox/tavilia'
setwd(wd)
# install af pakker
#install.packages("NLP", dependencies = TRUE)
#install.packages("tm", dependencies = TRUE)
#install.packages("SnowballC", dependencies = TRUE)
#install.packages("Slam", dependencies = TRUE)
#install.packages("wordcloud", dependencies = TRUE)
#install.packages("reshape2", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("ggmap", dependencies = TRUE)
#install.packages('igraph')
#install.packages('geosphere', dependencies = TRUE)
#install.packages("devtools", dependencies = TRUE)
#install.packages("plotly", dependencies = TRUE)
#devtools::install_github("ropensci/plotly")
#install.packages('plyr', dependencies = TRUE)
#library usepackage
library(NLP)
library(tm)
library(ggmap)
library(plotly)
library(geosphere)
library(plyr)
library(maps)
#library(SnowballC)
#library(slam)
#library(wordcloud)
#library(reshape2)
#library(ggplot2) 
#library(ggmap)
######### Preamble end ##########
### Load Data
dd <- "~/Dropbox/tavilia/Data/documents"
#Corpus
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

#p <- plot_ly(midwest, x = percollege, color = state, type = "box")

#metadata2 <- read.csv("~/Desktop/metadata2.csv") # fix det her til at virke med metadata 
#translaterere stednavne overtil koordinater
#laver en variabel der knytter land til by (det antages at alle steder er i spanien)
metadata$land <- NA
metadata[1:551, 18] <- "Spain"
#usikkerhed om følgende lokationer: lisbon(portugal), cremona(italien?), Quito (Ecuador)
metadata$landmod <- NA
metadata$landmod[1:468] <- "Spain"
metadata$landmod[2] <- ""
#Lisbon er i portugal
metadata$landmod[82] <- "Portugal"
metadata$landmod[436] <- "Portugal"
#Rom er i italien
metadata$landmod[440] <- "Italy"
#Peru er i Peru
metadata$landmod[379] <- ""
#Almodôvar er i Portugal
metadata$landmod[117] <- "Portugal"
metadata$landmod[118] <- "Portugal"
metadata$landmod[119] <- "Portugal"
metadata$landmod[121] <- "Portugal"
#Evora er i Portugal
metadata$landmod[226] <- "Portugal"
metadata$landmod[305] <- "Portugal"
#Quito er i Ecuador (måske)
metadata$landmod[2] <- ""
metadata$landmod[24] <- ""
metadata$landmod[363] <- "Equador"
metadata$landmod[427] <- "Equador"
metadata$afsland <- NA
metadata$afsland[1:551] <- cbind(paste0(metadata$afsenderby[1:551], " ", metadata$land[1:551]))
metadata$afsland[552:690] <- NA
metadata$modland <- NA
#Alcala de henares (antages det udfra obs. 268)
#metadata[c(377:371, 328, 324, 320, 317:315, 311, 307, 303, 302, 297, 291, 290, 289, 288, 268, 247:244, 242, 239, 238, 233, 216),c(9)] <- "Madrid" 
metadata$modtagerby[metadata$modtagerby == "Alcalá"] <- "Madrid" 
metadata[c(372, 315, 268, 216),c(9)] <- "Madrid"
#Alba de Tormes (antages det udfra obs. 22 og 220)
metadata$modtagerby[metadata$modtagerby == "Alba"] <- "Alba de Tormes"
#Avila = Avila?
metadata$modtagerby[metadata$afsenderby == "Avila?"] <- "Avila"
#Duchess of Alba omkodes til Alba de Tormes
metadata$modtagerby[c(278)] <- "Alba de Tormes"
metadata$modland[1:468] <- cbind(paste0(metadata$modtagerby[1:468], " ", metadata$landmod[1:468]))

#geocode sender afsender og modtager for at slå lon og lat koordinater op 
afskoor <- geocode(unique(metadata$afsland[1:551]))
modkoor <- geocode(unique(metadata$modland[1:468]))
byland <- cbind("afsland"=unique(metadata$afsland[1:551]), afskoor)
landby <- cbind("modland"=unique(metadata$modland[1:468]), modkoor)
#laver en NA variable så jeg kan merge tilbage ind i metadata
byland[c(19), c(1,2,3)] <- NA
landby[c(59), c(1,2,3)] <- NA
#navngiver lon og lat ifht afs og mod så jeg kan merge
byland <- rename(byland, c("lon"="lonafs", "lat"="latafs"))
landby <- rename(landby, c("lon"="lonmod", "lat"="latmod"))
#sætter usikkre observationer = NA for afsender
byland[c(12,13,18),c(2,3)] <- NA
#sætter usikkre observationer = NA for modtager (NA, Medina, Granada)
landby[c(22,25,39),c(2,3)] <- NA
#Folder koordinater tilbage ind i datasættet (måske unødvendigt...)
metadata <- merge(metadata, byland, by = 'afsland')
metadata <- merge(metadata, landby, by = 'modland')

# map projection
geo <- list(
  resolution = 50,
  showland = TRUE,
  showlakes = TRUE,
  landcolor = toRGB("grey80"),
  countrycolor = toRGB("grey80"),
  lakecolor = toRGB("white"),
  projection = list(type = "equirectangular"),
  coastlinewidth = 2,
  lataxis = list(
    range = c(35, 45),
    showgrid = TRUE,
    tickmode = "linear",
    dtick = 10
  ),
  lonaxis = list(
    range = c(-10, 2),
    showgrid = TRUE,
    tickmode = "linear",
    dtick = 20
  )
)

early1 <- subset(metadata, årstal < 1571)
later <-  subset(metadata, årstal > 1571)

#laver en variable der tæller ens observationer
metadata$test <- NA
metadata$test <- cbind(paste0(metadata$modtagerby, " ", metadata$afsenderby))
test <- count(metadata$test)
test[c(164),c(1,2)] <- NA
test <- rename(test, c("x" = "test"))
test <- merge(test, metadata[,c(22:26)], by = "test")
test <- unique(test)
#laver cirkler
#tæller observationer af afsenderby
testmod <- count(metadata$afsenderby)
metadata$test1 <- NA
metadata$test1 <- cbind(paste0(metadata$afsenderby))
testmod[c(23),c(1,2)] <- NA
testmod <- rename(testmod, c("x" = "test1"))
testmod <- merge(testmod, metadata[,c(22:23, 27)], by = "test1")
testmod <- unique(testmod)

#Laver kort med plotly

p <- plotly(username = "bojje", key= "yvlnbgl4uh")

res <- plot_ly(na.omit(testmod), lon = lonafs, lat = latafs,
               marker = list(size = sqrt(testmod$freq)+7), color = 'red', type = 'scattergeo', locationmode = 'ESP',
               inherit = FALSE, text = test1
               ) %>%
  add_trace(na.omit(test), lon = list(lonafs, lonmod), lat = list(latafs, latmod),
            group = test,
            mode = 'lines', line = list(width = 1, color = 'blue'),
            type = 'scattergeo', locationmode = 'ESP',
            text = metadata$dokument.nr, data = na.omit(metadata)
  ) %>%
#  add_trace(na.omit(testmod), lon = lonafs, lat = latafs,
 #           marker = list(size = testmod$freq/15), color = 'red', type = 'scattergeo', locationmode = 'ESP',
  #          inherit = FALSE
  #) %>% 
  layout(title = 'Avilia',
         geo = geo,
         autosize = T,
       #  width = 2400,
        # height = 1800,
         hovermode = T,
       showlegend = FALSE
         )

plotly_POST(res, filename = "r-docs/avilia", world_readable=TRUE)

#Dropper variable og data frames som ikke længere er nødvendige.
#drops <- c("afsland", "modland", "landmod", "land")
#drops <- c("test")
#metadata <- metadata[,!(names(metadata) %in% drops)]
#rm(byland, modkoor, afskoor, landby)