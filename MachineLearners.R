rm(list=ls())

library(dplyr)
library(tidyverse)
library(caret)
library(data.table)
library(rpart)
library(rpart.plot)
set.seed(888)

if(!exists("data/dtFlags.Rda")){
source("Data Prep.R")
}
if(!exists("dtFlags")){load("data/dtFlags.Rda")}
if(!exists("dtFeatures")){load("data/dtFeatures.Rda")}
if(!exists("nameCol")){load("data/dtFeatures.Rda")}
if(!exists("dtFlags")){load("data/dtFlags.rda")}

if(!exists("partition")){partition <- createDataPartition(y=dtFeatures$language, times = 1, p=0.20, list = FALSE)}
if(!exists("test_set")){test_set <- dtFeatures[partition,]}
if(!exists("train_set")){train_set <- dtFeatures[-partition,]}



arrayFeatures <- names(dtFlags)[-1]
dtFeatures[,country := rownames(dtFlags)]
dtFeatures[,table(language)]

#reducing the amount of groups
listgroups <- list(
  english = "English",
  spanish = "Spanish",
  frgr = c("French","German"),
  indoEu = c("Slavic","Other European"),
  arabic = "Arabic",
  other = c("Japanese/Turkish/Finish/Maygar","Chinese","Others")
)
for(NameGroup in names(listgroups)){
  dtFeatures[language %in% listgroups[[NameGroup]],
             language:=NameGroup
             ]
}

#pure guessing
languages <- c("english","spanish","frgr","indoEu","arabic","other")
guesses <- sample(languages, nrow(test_set), replace = TRUE)
test_set$languageguessed = guesses
correct_hits <- test_set[,sum(language==languageguessed)/.N]
Results <- data_frame(Method = "Pure guess",Score = correct_hits)
Results %>% knitr::kable()M

#base Knn
train_knn <- train(language ~ .,method = "knn",data=train_set)
Results <- bind_rows(Results, 
                     data_frame(Method = "KNN", 
                                Score = confusionMatrix(predict(train_knn,test_set), test_set$language)$overall["Accuracy"]))
Results
#Tuned KNN
train_knn <- train(language ~ .,method = "knn",data=train_set,tuneGrid = data.frame(k= seq(1,50,1)))
Results <- bind_rows(Results, 
                     data_frame(Method = "Tuned Knn", 
                                Score = confusionMatrix(predict(train_knn,test_set), test_set$language)$overall["Accuracy"]))
#Lda
train_lda<- train(language ~ .,method = "lda",data=train_set)
confusionMatrix(predict(train_lda,test_set), test_set$language)$overall["Accuracy"]
Results <- bind_rows(Results, 
                     data_frame(Method = "Lda ", 
                                Score = confusionMatrix(predict(train_lda,test_set), test_set$language)$overall["Accuracy"]))

#Desision tree
train_rpart <- train(language ~ .,method = "rpart",
                    tuneGrid = data.frame(cp = seq(0.0,0.1,len=25)),data=train_set)
Results <- bind_rows(Results, 
                     data_frame(Method = "Desicion Tree ", 
                                Score = confusionMatrix(predict(train_rpart,test_set), test_set$language)$overall["Accuracy"]))


Results %>% knitr::kable()

