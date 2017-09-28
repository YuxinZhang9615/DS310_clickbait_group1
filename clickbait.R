###Project 1: Clickbait
###Team: Group 3
###Team Member: Yuxin Zhang, Rafael Vinluan, Nicholas Cianci, Cameron Zenier
###Date: 9/18/2017
###Due: 10/8/2017
###############################################################################

install.packages("rjson")
library(rjson)
json_file <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
raw_data <- readLines("instances_train.jsonl")
str(raw_data)
label <- readLines("truth_train.jsonl")
label

###############################Diana######################################################
###sample data length of 10
sample_data <- raw_data[10]
sample_label <- label[10]

sample_data
sample_data_posttext <- sample_data

###Youtube
install.packages("rJava")
install.packages("NLP")
install.packages("openNLP")
require(rJava)
require(NLP)
require(openNLP)
##Need Sentence and word token annotations.
postText <- raw_data[["postText"]]
postText
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(postText, list(sent_token_annotator, word_token_annotator))
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(postText, pos_tag_annotator, a2)
a3

#############################Nick########################################################