###Project 1: Clickbait
###Team: Group 3
###Team Member: Yuxin Zhang, Rafael Vinluan, Nicholas Cianci, Cameron Zenier
###Date: 9/18/2017
###Due: 10/8/2017
###############################################################################

# install.packages("rjson")
# library(rjson)
# json_file <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"
# raw_data <- readLines("instances_train.jsonl")
# str(raw_data)
# label <- readLines("training/truth_train.jsonl")
# label
# data <- fromJSON(file = "instances_train.jsonl")
# 
# library("tm")
# docs <- Corpus(DirSource("training"))
# dtm <- DocumentTermMatrix(docs)
# freq <- colSums(as.matrix(dtm))
# ord <- order(freq,decreasing=TRUE)
# freq[head(ord)]
# ###############################Diana######################################################
# ###list
# library(rjson)
# raw_data <- readLines("training/instances_train.jsonl")
# data1 <- fromJSON(raw_data[1])
# data2 <- fromJSON(raw_data[2])
# data6 <- fromJSON(raw_data[6])
# data11 <- fromJSON(raw_data[17581])
data335 <- fromJSON(raw_data[335])
rm(data335)
# data1
# data2
# dataAll <- Map(list, data1,data2)
# raw_data
# 
# 
# data <- list()
# for (i in 1:17581){
#   data[i] <- fromJSON(raw_data[i])
# }
# data[[17581]]
# data[[17582]]
# 
# head(data,10)
# 
# str(data)
# 
# ?ls
# 
# for (i in 1:17581){
#   all <- Map(list, data[i])
# }
# 
# data
# head(raw_data)
# 
# 
# 
# 
# 
# ###sample data length of 10
# sample_data <- raw_data[10]
# sample_label <- label[10]
# 
# sample_data
# sample_data_posttext <- sample_data["postText"]
# sample_data_posttext
# 
# content<-readLines("instances_train.jsonl")
# 
# ###Youtube
# 
# install.packages("rJava")
# install.packages("NLP")
# install.packages("openNLP")
# require(rJava)
# require(NLP)
# require(openNLP)
# 
# ##Need Sentence and word token annotations.
# postText <- raw_data[["postText"]]
# postText
# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# a2 <- annotate(postText, list(sent_token_annotator, word_token_annotator))
# pos_tag_annotator <- Maxent_POS_Tag_Annotator()
# a3 <- annotate(postText, pos_tag_annotator, a2)
# a3
# 
# 
# library("tm")
# docs <- Corpus(DirSource("/Users/cameronzenier/R files/DS310-Project1"))
# dtm <- DocumentTermMatrix(docs)
# freq <- colSums(as.matrix(dtm))
# freq
# ord <- order(freq,decreasing=TRUE)
# freq[head(ord)]
# 
# writeLines(as.character(docs))
# removeWords(docs, "melanoma")
# ?gsub        
# data12 <- gsub("\\{\\\"targetCaptions\\\": \\[.*\\], \\\"targetDescription\\\"", "\\{\\\"targetDescription\\\"", raw_data[1], perl = TRUE)
# data12 <- gsub( "\\{\\\"targetDescription\\\"", "\\{\\\"targetCaptions\\\":\\[.*\\], \\\"targetDescription\\\"",raw_data[1])
# data12  
# 
# a = "12345"
# b = gsub("123", "789",a)


######Start with data set
library(rjson)
raw_data <- readLines("training/instances_train.jsonl")
label <- readLines("training/truth_train.jsonl")
###sample as reference
data1 <- fromJSON(raw_data[1])
data1
data2 <- fromJSON(raw_data[2])
data2
data6 <- fromJSON(raw_data[6])
data6
data17581 <- fromJSON(raw_data[17581])
###caption
caption <- list()
for (i in 1:17581){
  caption[i] <- fromJSON(raw_data[i])
}
###description
data_startDescrip <- c()
for (i in 1:17581){
  data_startDescrip[i] <- gsub("\\{\\\"targetCaptions\\\": \\[.*\\], \\\"targetDescription\\\"", "\\{\\\"targetDescription\\\"", raw_data[i], perl = TRUE)
}
description <- list()
for (i in 1:17581){
  description[i] <- fromJSON(data_startDescrip[i])
}
head(description)
###postText
data_startPosttext <- c()
for (i in 1:17581){
  data_startPosttext[i] <- gsub("\\{\\\"targetDescription\\\": \\\"\\\", \\\"postText\\\"", "\\{\\\"postText\\\"", data_startDescrip[i], perl = TRUE)
}
postText <- list()
for (i in 1:17581){
  postText[i] <- fromJSON(data_startPosttext[i])
}
####
###label
data_label <- c()
for (i in 1:17581){
  data_label[i] <- gsub("\\{\\\"truthMedian\\\": .* \\\"truthMean\\\": .*, \\\"truthMode\\\": .*, \\\"truthClass\\\"", "\\{\\\"truthClass\\\"", label[i], perl = TRUE)
}
label <- list()
for (i in 1:17581){
  label[i] <- fromJSON(data_label[i])
}
label_num <- c()
for (i in 1:17581){
  if (label[[i]] == "no-clickbait"){
    label_num[i] = 0
  }else if (label[[i]] == "clickbait"){
    label_num[i] = 1
  }
}
####Youtube

install.packages("rJava")
install.packages("NLP")
install.packages("openNLP")
library(rJava)
library(NLP)
library(openNLP)

##Need Sentence and word token annotations.
# postText <- raw_data[["postText"]]
postText[2]
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
p2 <- list()
for (i in 1:10000){
  p2[[i]] <- annotate(postText[[i]], list(sent_token_annotator, word_token_annotator))
}
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
p3 <- list()
for (i in 1:10000){
  p3[[i]] <- annotate(postText[[i]], pos_tag_annotator, p2[[i]])
}
p3 <- annotate(postText, pos_tag_annotator, p2)
p3[1:3]
###analysis of first 10000 postText
##number of sentences
numSentence_postText <- c()
for (i in 1:10000){
  numSentence_postText[i] <- sum(p3[[i]]$type == "sentence")
}
label_num <- label_num[1:10000]
f_numSentence_postText <- cbind(numSentence_postText, label_num)
f_numSentence_postText
##number of words
numWord_postText <- c()
for (i in 1:10000){
  numWord_postText[i] <- sum(p3[[i]]$type == "word")
}
f_numWord_postText <- cbind(numWord_postText,numSentence_postText, label_num[1:10000])
f_numWord_postText
##average length of sentence
lenSentence_postText <- c()
for (i in 1:10000){
  lenSentence_postText[i] <- mean(p3[[i]]$end[p3[[i]]$type == "sentence"] - p3[[i]]$start[p3[[i]]$type == "sentence"] + 1) 
}
feature <- cbind(lenSentence_postText,f_numWord_postText)
head(feature)
##average length of words
lenWord_postText <- c()
for (i in 1:10000){
  lenWord_postText[i] <- mean(p3[[i]]$end[p3[[i]]$type == "word"] - p3[[i]]$start[p3[[i]]$type == "word"] + 1) 
}
feature <- cbind(lenWord_postText,feature)
#Rename features
colnames(feature) <- c("L.W_pText","L.S_pText,","n_W_pText","n_S_pText","class")
head(feature)

# class(p3[[1]]$feature)
# p3[[1]]$feature[[3]] == "JJ"
###proportion of adj. in postText
adj_pText <- c()
for (i in 1:10000){
  adj_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    adj_pText[i] = sum(adj_pText[i], sum(p3[[i]]$feature[[j]] == "JJ", p3[[i]]$feature[[j]] == "JJR", p3[[i]]$feature[[j]] == "JJS"))
  }
}
ave_adj_pText <- c()
for (i in 1:10000){
  ave_adj_pText[i] = adj_pText[i] / numSentence_postText[i]
}
feature <- cbind(ave_adj_pText,feature)
feature

prop_adj_pText <- c()
for (i in 1:10000){
  prop_adj_pText[i] = adj_pText[i] / numWord_postText[i]
}
prop_adj_pText
feature <- cbind(prop_adj_pText, feature)
feature

###proportion of JJS in postText
jjs_pText <- c()
for (i in 1:10000){
  jjs_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    jjs_pText[i] = sum(jjs_pText[i], p3[[i]]$feature[[j]] == "JJS")
  }
}
prop_jjs_pText <- c()
for (i in 1:10000){
  prop_jjs_pText[i] = jjs_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_jjs_pText, feature)
head(feature)

###ave propernoun in postText
nnp_pText <- c()
for (i in 1:10000){
  nnp_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    nnp_pText[i] = sum(nnp_pText[i], p3[[i]]$feature[[j]] == "NNP", p3[[i]]$feature[[j]] == "NNPS")
  }
}
prop_nnp_pText <- c()
for (i in 1:10000){
  prop_nnp_pText[i] = nnp_pText[i] / numSentence_postText[i]
}
feature <- cbind(prop_nnp_pText, feature)
head(feature)
###
###ave DT in postText
dt_pText <- c()
for (i in 1:10000){
  dt_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    dt_pText[i] = sum(dt_pText[i], p3[[i]]$feature[[j]] == "DT")
  }
}
prop_dt_pText <- c()
for (i in 1:10000){
  prop_dt_pText[i] = dt_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_dt_pText, feature)
head(feature)
###
###ave adv in postText
adv_pText <- c()
for (i in 1:10000){
  adv_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    adv_pText[i] = sum(adv_pText[i], p3[[i]]$feature[[j]] == "RB", p3[[i]]$feature[[j]] == "RBR", p3[[i]]$feature[[j]] == "RBS")
  }
}
prop_adv_pText <- c()
for (i in 1:10000){
  prop_adv_pText[i] = adv_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_adv_pText, feature)
head(feature)
###
###ave prp in postText
prp_pText <- c()
for (i in 1:10000){
  prp_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    prp_pText[i] = sum(prp_pText[i], p3[[i]]$feature[[j]] == "PRP", p3[[i]]$feature[[j]] == "PRP$")
  }
}
prop_prp_pText <- c()
for (i in 1:10000){
  prop_prp_pText[i] = prp_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_prp_pText, feature)
head(feature)
###
feature <- feature[, c(1,3,4,6,7,8,10,11,12,13,14)]

###proportion of nn in postText
nn_pText <- c()
for (i in 1:10000){
  nn_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    nn_pText[i] = sum(nn_pText[i], p3[[i]]$feature[[j]] == "NN", p3[[i]]$feature[[j]] == "NNS", p3[[i]]$feature[[j]] == "NNP", p3[[i]]$feature[[j]] == "NNPS")
  }
}
prop_nn_pText <- c()
for (i in 1:10000){
  prop_nn_pText[i] = nn_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_nn_pText, feature)
head(feature)
###

###proportion of v in postText
v_pText <- c()
for (i in 1:10000){
  v_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    v_pText[i] = sum(v_pText[i], p3[[i]]$feature[[j]] == "VB", p3[[i]]$feature[[j]] == "VBD", p3[[i]]$feature[[j]] == "VBG", p3[[i]]$feature[[j]] == "VBN", p3[[i]]$feature[[j]] == "VBP", p3[[i]]$feature[[j]] == "VBZ")
  }
}
prop_v_pText <- c()
for (i in 1:10000){
  prop_v_pText[i] = v_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_v_pText, feature)
head(feature)
###
###proportion of vbn in postText
vbn_pText <- c()
for (i in 1:10000){
  vbn_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    vbn_pText[i] = sum(vbn_pText[i], p3[[i]]$feature[[j]] == "VBN")
  }
}
prop_vbn_pText <- c()
for (i in 1:10000){
  prop_vbn_pText[i] = vbn_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_vbn_pText, feature)
head(feature)
###
###proportion of vbz in postText
vbz_pText <- c()
for (i in 1:10000){
  vbz_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    vbz_pText[i] = sum(vbz_pText[i], p3[[i]]$feature[[j]] == "VBZ")
  }
}
prop_vbz_pText <- c()
for (i in 1:10000){
  prop_vbz_pText[i] = vbz_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_vbz_pText, feature)
head(feature)
feature <- feature[,2:16]
###
###proportion of vbd in postText
vbd_pText <- c()
for (i in 1:10000){
  vbd_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    vbd_pText[i] = sum(vbd_pText[i], p3[[i]]$feature[[j]] == "VBD")
  }
}
prop_vbd_pText <- c()
for (i in 1:10000){
  prop_vbd_pText[i] = vbd_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_vbd_pText, feature)
head(feature)
###
###proportion of vbp in postText
vbp_pText <- c()
for (i in 1:10000){
  vbp_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    vbp_pText[i] = sum(vbp_pText[i], p3[[i]]$feature[[j]] == "VBP")
  }
}
prop_vbp_pText <- c()
for (i in 1:10000){
  prop_vbp_pText[i] = vbp_pText[i] / numWord_postText[i]
}
feature <- cbind(prop_vbp_pText, feature)
head(feature)
###
write.csv(feature, "feature.csv")


####targetTitle
data_targetTitle <- c()
for (i in 1:17581){
  data_targetTitle[i] <- gsub("\\{\\\"targetDescription\\\":.* \\\"targetTitle\\\":", "\\{\\\"targetTitle\\\":", data_startDescrip[i], perl = TRUE)
}
targetTitle <- list()
for (i in 1:17581){
  targetTitle[i] <- fromJSON(data_targetTitle[i])
}
head(targetTitle)
targetTitle[[1]]



sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
t2 <- list()
for (i in 1:10000){
  t2[[i]] <- annotate(targetTitle[[i]], list(sent_token_annotator, word_token_annotator))
}
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
t3 <- list()
for (i in 1:10000){
  t3[[i]] <- annotate(targetTitle[[i]], pos_tag_annotator, t2[[i]])
}
t3 <- annotate(targetTitle, pos_tag_annotator, t2)
t3

###
##number of sentences
numSentence_targetTitle <- c()
for (i in 1:10000){
  numSentence_targetTitle[i] <- sum(t3[[i]]$type == "sentence")
}
numSentence_targetTitle
label_num <- label_num[1:10000]
f_numSentence_targetTitle <- cbind(numSentence_targetTitle, label_num)
f_numSentence_targetTitle
##number of words
numWord_targetTitle <- c()
for (i in 1:10000){
  numWord_targetTitle[i] <- sum(t3[[i]]$type == "word")
}
numWord_targetTitle
f_numWord_targetTitle <- cbind(numWord_targetTitle,numSentence_targetTitle, label_num[1:10000])
f_numWord_targetTitle
##average length of sentence
lenSentence_targetTitle <- c()
for (i in 1:10000){
  lenSentence_targetTitle[i] <- mean(t3[[i]]$end[t3[[i]]$type == "sentence"] - t3[[i]]$start[t3[[i]]$type == "sentence"] + 1) 
}
feature <- cbind(lenSentence_targetTitle,f_numWord_targetTitle)
head(feature)
##average length of words
lenWord_targetTitle <- c()
for (i in 1:10000){
  lenWord_targetTitle[i] <- mean(t3[[i]]$end[t3[[i]]$type == "word"] - t3[[i]]$start[t3[[i]]$type == "word"] + 1) 
}
feature <- cbind(lenWord_targetTitle,feature)
#Rename features
colnames(feature) <- c("L.W_tT","L.S_tT,","n_W_tT","n_S_tT","class")
head(feature)

# class(t3[[1]]$feature)
# t3[[1]]$feature[[3]] == "JJ"
###proportion of adj. in targetTitle
adj_tTitle <- c()
for (i in 1:10000){
  adj_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    adj_tTitle[i] = sum(adj_tTitle[i], sum(t3[[i]]$feature[[j]] == "JJ", t3[[i]]$feature[[j]] == "JJR", t3[[i]]$feature[[j]] == "JJS"))
  }
}
ave_adj_tTitle <- c()
for (i in 1:10000){
  ave_adj_tTitle[i] = adj_tTitle[i] / numSentence_targetTitle[i]
}

feature

prop_adj_tTitle <- c()
for (i in 1:10000){
  prop_adj_tTitle[i] = adj_tTitle[i] / numWord_targetTitle[i]
}
prop_adj_tTitle
feature <- cbind(prop_adj_tTitle, feature)
feature

###proportion of JJS in targetTitle
jjs_tTitle <- c()
for (i in 1:10000){
  jjs_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    jjs_tTitle[i] = sum(jjs_tTitle[i], t3[[i]]$feature[[j]] == "JJS")
  }
}
prop_jjs_tTitle <- c()
for (i in 1:10000){
  prop_jjs_tTitle[i] = jjs_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_jjs_tTitle, feature)
head(feature)

###ave propernoun in targetTitle
nnp_tTitle <- c()
for (i in 1:10000){
  nnp_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    nnp_tTitle[i] = sum(nnp_tTitle[i], t3[[i]]$feature[[j]] == "NNP", t3[[i]]$feature[[j]] == "NNPS")
  }
}

prop_nnp_tTitle <- c()
for (i in 1:10000){
  prop_nnp_tTitle[i] = nnp_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_nnp_tTitle, feature)
head(feature)
###
###ave DT in targetTitle
dt_tTitle <- c()
for (i in 1:10000){
  dt_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    dt_tTitle[i] = sum(dt_tTitle[i], t3[[i]]$feature[[j]] == "DT")
  }
}
prop_dt_tTitle <- c()
for (i in 1:10000){
  prop_dt_tTitle[i] = dt_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_dt_tTitle, feature)
head(feature)
###
###ave adv in targetTitle
adv_tTitle <- c()
for (i in 1:10000){
  adv_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    adv_tTitle[i] = sum(adv_tTitle[i], t3[[i]]$feature[[j]] == "RB", t3[[i]]$feature[[j]] == "RBR", t3[[i]]$feature[[j]] == "RBS")
  }
}
prop_adv_tTitle <- c()
for (i in 1:10000){
  prop_adv_tTitle[i] = adv_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_adv_tTitle, feature)
head(feature)
###
###ave prp in targetTitle
prp_tTitle <- c()
for (i in 1:10000){
  prp_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    prp_tTitle[i] = sum(prp_tTitle[i], t3[[i]]$feature[[j]] == "PRP", t3[[i]]$feature[[j]] == "PRP$")
  }
}
prop_prp_tTitle <- c()
for (i in 1:10000){
  prop_prp_tTitle[i] = prp_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_prp_tTitle, feature)
head(feature)


###proportion of nn in targetTitle
nn_tTitle <- c()
for (i in 1:10000){
  nn_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    nn_tTitle[i] = sum(nn_tTitle[i], t3[[i]]$feature[[j]] == "NN", t3[[i]]$feature[[j]] == "NNS", t3[[i]]$feature[[j]] == "NNP", t3[[i]]$feature[[j]] == "NNPS")
  }
}
prop_nn_tTitle <- c()
for (i in 1:10000){
  prop_nn_tTitle[i] = nn_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_nn_tTitle, feature)
head(feature)
###

###proportion of v in targetTitle
v_tTitle <- c()
for (i in 1:10000){
  v_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    v_tTitle[i] = sum(v_tTitle[i], t3[[i]]$feature[[j]] == "VB", t3[[i]]$feature[[j]] == "VBD", t3[[i]]$feature[[j]] == "VBG", t3[[i]]$feature[[j]] == "VBN", t3[[i]]$feature[[j]] == "VBP", t3[[i]]$feature[[j]] == "VBZ")
  }
}
prop_v_tTitle <- c()
for (i in 1:10000){
  prop_v_tTitle[i] = v_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_v_tTitle, feature)
head(feature)
###
###proportion of vbn in targetTitle
vbn_tTitle <- c()
for (i in 1:10000){
  vbn_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    vbn_tTitle[i] = sum(vbn_tTitle[i], t3[[i]]$feature[[j]] == "VBN")
  }
}
prop_vbn_tTitle <- c()
for (i in 1:10000){
  prop_vbn_tTitle[i] = vbn_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_vbn_tTitle, feature)
head(feature)
###
###proportion of vbz in targetTitle
vbz_tTitle <- c()
for (i in 1:10000){
  vbz_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    vbz_tTitle[i] = sum(vbz_tTitle[i], t3[[i]]$feature[[j]] == "VBZ")
  }
}
prop_vbz_tTitle <- c()
for (i in 1:10000){
  prop_vbz_tTitle[i] = vbz_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_vbz_tTitle, feature)
head(feature)
###
###proportion of vbd in targetTitle
vbd_tTitle <- c()
for (i in 1:10000){
  vbd_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    vbd_tTitle[i] = sum(vbd_tTitle[i], t3[[i]]$feature[[j]] == "VBD")
  }
}
prop_vbd_tTitle <- c()
for (i in 1:10000){
  prop_vbd_tTitle[i] = vbd_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_vbd_tTitle, feature)
head(feature)
###
###proportion of vbp in targetTitle
vbp_tTitle <- c()
for (i in 1:10000){
  vbp_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    vbp_tTitle[i] = sum(vbp_tTitle[i], t3[[i]]$feature[[j]] == "VBP")
  }
}
prop_vbp_tTitle <- c()
for (i in 1:10000){
  prop_vbp_tTitle[i] = vbp_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(prop_vbp_tTitle, feature)
head(feature)
feature <- cbind(lenSentence_postText,feature)
write.csv(feature, "feature_tT2")
