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
metadata[1:468, 18] <- "Spain"
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
metadata$afsland <- NA
metadata$afsland[1:468] <- cbind(paste0(metadata$afsenderby[1:468], " ", metadata$land[1:468]))
metadata$afsland[469:690] <- NA
metadata$modland <- NA
metadata$modland[1:468] <- cbind(paste0(metadata$modtagerby[1:468], " ", metadata$landmod[1:468]))
metadata$modland[469:690] <- NA

#geocode sender afsender og modtager for at slå lon og lat koordinater op 
afskoor <- geocode(unique(metadata$afsland[1:468]))
modkoor <- geocode(unique(metadata$modland[1:468]))
byland <- cbind("afsland"=unique(metadata$afsland[1:468]), afskoor)
landby <- cbind("modland"=unique(metadata$modland[1:468]), modkoor)
#laver en NA variable så jeg kan merge tilbage ind i metadata
byland[c(19), c(1,2,3)] <- NA
landby[c(65), c(1,2,3)] <- NA
#navngiver lon og lat ifht afs og mod så jeg kan merge
byland <- rename(byland, c("lon"="lonafs", "lat"="latafs"))
landby <- rename(landby, c("lon"="lonmod", "lat"="latmod"))
#sætter usikkre observationer = NA for afsender
byland[c(12,13,18),c(2,3)] <- NA
#sætter usikkre observationer = NA for modtager (NA, Medina, Gotarrendura(der er nok flere, f.eks. alba spain))
landby[c(4, 23,26),c(2,3)] <- NA
#Folder koordinater tilbage ind i datasættet (måske unødvendigt...)
metadata <- merge(metadata, byland, by = 'afsland')
metadata <- merge(metadata, landby, by = 'modland')

#Forsøger at lave et plot
map("world")
set.seed(10)

points(x = metadata$lonafs, y = metadata$latafs, col = "red")
lines(x = metadata$lonmod, y = metadata$latmod, col = "blue")
#det var grimt, igen: 
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

plot_ly(na.omit(metadata), lon = lonmod, lat = latmod, type = 'scattergeo',
             locationmode = 'ESP', marker = list(size = 10, color = 'red'),
             inherit = FALSE) %>%
  #add_trace(lon = list(lonafs, lonmod), lat = list(latafs, latmod),
   #         group = dokument.nr,
    #        mode = 'lines', line = list(width = 1, color = 'red'),
     #       type = 'scattergeo', locationmode = 'ESP',
      #      text = årstal, data = na.omit(metadata)
       #     ) %>%
  layout(title = 'Avilia',
         geo = geo,
         autosize = F,
         width = 1100,
         height = 900,
         hovermode = F
         )
#skal opdele data i subplots... skal finde en anden baggrund. 
#Dropper variable og data frames som ikke længere er nødvendige.
#drops <- c("afsland", "modland", "landmod", "land")
#metadata <- metadata[,!(names(metadata) %in% drops)]
#rm(byland, modkoor, afskoor, landby)