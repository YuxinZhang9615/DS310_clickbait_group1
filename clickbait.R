###Project 1: Clickbait
###Team: Group 3
###Team Member: Yuxin Zhang, Rafael Vinluan, Nicholas Cianci, Cameron Zenier
###Date: 9/18/2017
###Due: 10/8/2017
###############################################################################

install.packages("rjson")
library(rjson)
library("rjson")
json_file <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
raw_data <- fromJSON(paste(readLines("instances_train.jsonl"), collapse=""))
raw_data
label <- fromJSON(paste(readLines("truth_train.jsonl"), collapse=""))
label

###############################Diana######################################################
###Youtube
library("tm")
docs <- Corpus(DirSource("/Users/cameronzenier/R files/DS310-Project1"))
dtm <- DocumentTermMatrix(docs)
freq <- colSums(as.matrix(dtm))
freq
ord <- order(freq,decreasing=TRUE)
freq[head(ord)]

writeLines(as.character(docs))
removeWords(docs, "melanoma")
#############################Nick########################################################
