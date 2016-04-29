######### Preamble start ##########
#clear
rm(list = ls())
#workingdirect
wd = '~/Google Drev/tavila'
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
#install.packages('igraph', dependencies = TRUE)
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
library(igraph)
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
dd <- "~/Google Drev/tavila/documents"
#Corpus
avilia  <- Corpus(DirSource(dd, encoding = "latin1"), readerControl = list(language="PlainTextDocument"))
names(avilia) <- gsub("\\..*","",names(avilia))
### class labels
filenum <- gsub("\\D*","",names(avilia))
fileclass <- gsub("\\d+_","",names(avilia))
fileclass[fileclass == "lette"] = "letter" # correct for spelling error
#Data tabel: 
metadata <- read.csv(("~/Google Drev/tavila/metadata.csv"), colClasses=c('character')) #fra num til char grundet sortering

for (j in 2:14) {
    tags <- names(metadata)
    for (i in 1:690) {
        meta(avilia[[i]], tag=tags[j]) <- metadata[i,j]
    }
} #Loop der indlæser meta data

metadata[619,7] <- NA # "" <- NA
metadata$modtagerby <- revalue(metadata$modtagerby, c("Madrid?" = "Madrid",
                                                      "Quito" = "Quito (Ecuador)",
                                                      "La Serna (Avila)" = "Avila",
                                                      "Medina" = "Medina del Campo"))

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
#metadata$modtagerby[metadata$modtagerby == "Alcalá"] <- "Madrid" 
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
metadata$modtagerby <- gsub("^\\s+", "", metadata$modtagerby) %>%  # remove leading whitespace
    gsub("\\s+$", "", .)  # remove trailing whitespace
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
#sætter usikkre observationer = NA for modtager (NA, Medina, cremona)
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



#laver en variable der tæller ens observationer (gammel kode)
metadata$test <- NA
metadata$test <- cbind(paste0(metadata$afsenderby, " ", metadata$modtagerby))
test <- count(metadata$test)
test[c(164),c(1,2)] <- NA
test <- rename(test, c("x" = "test"))
test <- merge(test, metadata[,c(22:26)], by = "test")
test <- unique(test)
#laver cirkler
#tæller observationer af afsenderby
testafs <- count(metadata$afsenderby)
metadata$test1 <- NA
metadata$test1 <- cbind(paste0(metadata$afsenderby))
testafs[c(23),c(1,2)] <- NA
testafs <- rename(testafs, c("x" = "test1"))
testafs <- merge(testafs, metadata[,c(22:23, 27)], by = "test1")
testafs <- unique(testafs)
#tæller observationer af modtagerby
testmod <- count(na.omit(metadata$modtagerby))
#laver variabel til at merge med
metadata$test2 <- NA
metadata$test2 <- cbind(paste0(metadata$modtagerby))
testmod <- rename(testmod, c("x" = "test2"))
testmod <- merge(testmod, metadata[,c(24:25, 28)], by = "test2")
testmod <- unique(testmod)

#Laver kort med plotly

p <- plotly(username = "bojje", key= "yvlnbgl4uh")

res <- plot_ly(na.omit(testafs), lon = lonafs, lat = latafs,
               marker = list(size = sqrt(testafs$freq)+7), fillcolor = 'purple', type = 'scattergeo', locationmode = 'ESP',
               inherit = FALSE, text = test1
) %>%
    add_trace(na.omit(test), lon = list(lonafs, lonmod), lat = list(latafs, latmod),
              group = test,
              mode = 'lines', line = list(width = 1, color = 'red'),
              type = 'scattergeo', locationmode = 'ESP',
              text = metadata$dokument.nr, data = na.omit(metadata)
    ) %>%
    add_trace(na.omit(testmod), lon = lonmod, lat = latmod,
              marker = list(size = sqrt(testmod$freq)+5), fillcolor = 'blue', type = 'scattergeo', locationmode = 'ESP',
              inherit = TRUE, text = test2
    )
#  add_trace(na.omit(testafs), lon = lonafs, lat = latafs,
#          marker = list(size = testafs$freq/15), fillcolor = 'red', type = 'scattergeo', locationmode = 'ESP',
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


#plotly_POST(res, filename = "r-docs/avilia", world_readable=TRUE)

#Laver netværks graf: 
library(igraph)
metadata$test1[metadata$test1 == "NA"] <- NA
metadata$test2[metadata$test2 == "NA"] <- NA

#g <- graph.data.frame(as.matrix(na.omit(metadata[,c(27,28)])),directed=TRUE)
#### 1567
dda <- subset(na.omit(metadata), årstal <= 1567)#  & årstal < 1580 )
netvaerk <- na.omit(dda[,c(27,28)])
netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
#netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
g <- graph.data.frame(as.matrix(na.omit(netvaerk[,c(1,2)])),directed=TRUE)
E(g)$arrow.width <- 1
set.seed(2)
layout <- layout.fruchterman.reingold(g, niter=100,area=vcount(g)^1,repulserad=vcount(g)^1)
plot(g, edge.width=log(netvaerk$freq+1)*2, edge.label=netvaerk$freq, vertex.size=1, vertex.shape = "circle", layout = layout)

#### 1570
dda <- subset(na.omit(metadata), årstal %in% 1568:1570) 
netvaerk <- na.omit(dda[,c(27,28)])
netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
#netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
g <- graph.data.frame(as.matrix(na.omit(netvaerk[,c(1,2)])),directed=TRUE)
E(g)$arrow.width <- 1
set.seed(50)
layout <- layout_with_lgl(g, area = vcount(g)^6, repulserad=vcount(g)^6)#layout.fruchterman.reingold(g, niter=5000,area=vcount(g)^30,repulserad=vcount(g)^20)
plot(g, edge.width=log(netvaerk$freq+1)*2, edge.label=netvaerk$freq, vertex.size=1, vertex.shape = "circle", layout = layout)

#### 1570-1575
dda <- subset(na.omit(metadata), årstal %in% 1571:1575) 
netvaerk <- na.omit(dda[,c(27,28)])
netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
#netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
g <- graph.data.frame(as.matrix(na.omit(netvaerk[,c(1,2)])),directed=TRUE)
E(g)$arrow.width <- 1
set.seed(433)
layout <- layout_with_lgl(g, area = vcount(g)^6, repulserad=vcount(g)^6)#layout.fruchterman.reingold(g, niter=5000,area=vcount(g)^30,repulserad=vcount(g)^20)
plot(g, edge.width=log(netvaerk$freq+1)*2, edge.label=netvaerk$freq, vertex.size=1, vertex.shape = "circle", layout = layout)

#### 1575-1577
dda <- subset(na.omit(metadata), årstal %in% 1576:1577) 
netvaerk <- na.omit(dda[,c(27,28)])
netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
#netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
g <- graph.data.frame(as.matrix(na.omit(netvaerk[,c(1,2)])),directed=TRUE)
E(g)$arrow.width <- 1
set.seed(10)
layout <- layout_with_lgl(g, area = vcount(g)^6, repulserad=vcount(g)^6)#layout.fruchterman.reingold(g, niter=5000,area=vcount(g)^30,repulserad=vcount(g)^20)
plot(g, edge.width=log(netvaerk$freq+1)*2, edge.label=netvaerk$freq, vertex.size=.1, vertex.shape = "circle", layout = layout)

#### 1577-1594
dda <- subset(na.omit(metadata), årstal %in% 1578:1595) 
netvaerk <- na.omit(dda[,c(27,28)])
netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
#netvaerk <- unique(count(na.omit(dda[,c(27,28)])))
g <- graph.data.frame(as.matrix(na.omit(netvaerk[,c(1,2)])),directed=TRUE)
E(g)$arrow.width <- 1
set.seed(14)
layout <- layout_with_lgl(g, area = vcount(g)^6, repulserad=vcount(g)^6)#layout.fruchterman.reingold(g, niter=5000,area=vcount(g)^30,repulserad=vcount(g)^20)
plot(g, edge.width=log(netvaerk$freq+1)*2, edge.label=netvaerk$freq, vertex.size=0.1, vertex.shape = "circle", layout = layout)


#install.packages("qgraph", dependencies = TRUE)
#library("qgraph")
#qgraph(na.omit(netvaerk[,c(1,2)])), esize = 5, gray = TRUE)

#Arcdiagram
#install.packages("devtools", dependencies = TRUE)
library(devtools)
#install_github('arcdiagram', username = 'gastonstat')
library(arcdiagram)
arcplot(as.matrix(netvaerk[,c(1,2)]), sorted = TRUE, decreasing = FALSE, horizontal=FALSE)
#To do: find noget retning eller afsender/modtager på det; måske evt med frekvens nedenunder. 
#laver måske en tidslig dimension

#Dropper variable og data frames som ikke længere er nødvendige.
#drops <- c("afsland", "modland", "landmod", "land")
#drops <- c("test")
#metadata <- metadata[,!(names(metadata) %in% drops)]
#rm(byland, modkoor, afskoor, landby)