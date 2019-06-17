rm(list = ls())
#libraries
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)
library(data.table)
library(rpart)
library(rpart.plot)
dir.create("data")

#download the data and save
download.file(url="https://archive.ics.uci.edu/ml/machine-learning-databases/flags/flag.data" , method="libcurl", destfile="data/flags",  quiet = FALSE, mode = "w",
              cacheOK = TRUE,
              extra = getOption("download.file.extra"),
              headers = NULL)
dfFlags <-read.csv("data/flags", header = FALSE)

#adding headers to  the data 
namesCountry <- "name"
namesGeograpyhy <- c("continent","zone", "area")
namesDemgraphy <- c("population","language","religion")
namesAttributes <- c(namesCountry,namesGeograpyhy,namesDemgraphy)
namesNumbers <- c("bars","stripes","colors")
namesColors <- c("red","green","blue","gold","white","black","orange")
namesMainColor <- "mainhue"
namesDrawings <- c("circles","crosses","saltires","quarters","sunstars","cresent","triangle","icon","animate","text")
namesAngles <- c("topleft","botright")
namesFlag <- c(namesNumbers,namesColors,namesMainColor,namesDrawings,namesAngles)
names(dfFlags) <- c(namesAttributes,namesFlag) 
nameCol <- "mainhue"
#converting to data table
dtFlags <- data.table(dfFlags)

#making the table more readable, the data documentation from the download side is used
vectorContinents <- c("N.America","S.America","Europe","Africa","Asia","Oceania")
vectorZone <- c("NE","SE","SW","NW")
vectorLanguages <- c("English","Spanish","French","German","Slavic","Other Eropean","Chinese","Arabic","Japanese/Turkish/Finish/Maygar","Others")
vectorReligion <- c("Catholic","Other Cristian","Muslim","Buddhist","Hindu","Ethnic","Marxist","Other")
dtFlags[,continent :=factor(continent,labels = vectorContinents)]
dtFlags[,zone := factor(zone, labels = vectorZone)]
dtFlags[,language := factor(language, labels = vectorLanguages)]
dtFlags[,religion := factor(religion, labels = vectorReligion)]
dtFeatures <- dtFlags[,c("language",namesFlag),with = FALSE]
namesProcessed <- c()
nameFeat <- "red"


for(nameFeat  in namesFlag) {
  if (length(unique(dtFeatures[,get(nameFeat)])) ==2){
    vectorFactor<- dtFeatures[
      ,factor(get(nameFeat),labels = c("no","yes"))]
    dtFeatures[, eval(nameFeat):= vectorFactor]
    namesProcessed <- c(namesProcessed,nameFeat)
  }
}

dtFeatures [,nBars0 := bars == 0]
dtFeatures [,nBars1_2 := bars == c(1,2)]
dtFeatures [,nBars3 := bars == 3]
dtFeatures [,bars := NULL]
namesProcessed <- c(namesProcessed,"bars")

dtFeatures [,nStrip0 := stripes == 0]
dtFeatures [,nStrip2 := stripes == 2]
dtFeatures [,nStrip3 := stripes == 3]
dtFeatures [,nStrip5 := stripes == 5]
dtFeatures [,stripes := NULL]
namesProcessed <- c(namesProcessed,"stripes")


dtFeatures [,nCol12 := colors %in% c(1,2) ]
dtFeatures [,nCol3 := colors == 3]
dtFeatures [,nCol45 := colors %in% c(4,5) ]

dtFeatures [,colors := NULL]
namesProcessed <- c(namesProcessed,"colors")

for (nameCol in setdiff(namesDrawings,namesProcessed)){
  dtFeatures[,eval(nameCol) := ifelse(get(nameCol)>0,"yes","no")]
  namesProcessed <- c(namesProcessed,nameCol)
}

namesToDummy <- c("topleft","botright","mainhue")
for (nameCol in namesToDummy){
  frequencyColors <- dtFeatures[,list(.N),by = nameCol]
  for (color in frequencyColors[N>20,get(nameCol)]){
    namefFeatNew <- paste (nameCol, color,sep = "")
    dtFeatures[,eval(namefFeatNew):=get(nameCol)==color]
  }
  dtFeatures[,eval(nameCol):=NULL]
  namesProcessed <- c(namesProcessed,nameCol)
}
for (nameCol in names(dtFeatures)){
  if (dtFeatures[,class(get(nameCol))]== "logical"){
    print(nameCol)
    dtFeatures[,eval(nameCol):= ifelse(get(nameCol),"yes","no")]
  }
}

save(dtFlags,file="data/dtFlags.rda")
save(dtFeatures,file="data/dtFeatures.rda")
save(dtFlags,file="data/dtFlags.rda")
save(nameCol,file = "data/nameCol.rda")