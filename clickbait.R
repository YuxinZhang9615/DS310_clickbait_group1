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
raw_data <- readLines("https://raw.githubusercontent.com/YuxinZhang9615/DS310_clickbait_group1/master/instances_train.jsonl")
label <- readLines("https://raw.githubusercontent.com/YuxinZhang9615/DS310_clickbait_group1/master/instances_train.jsonl")
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

clickbait <- which(label_num[1:16558] == 1)
unclickbait <- which(label_num[1:16558] == 0)
length(clickbait)
#4194; 11806 ; 16000
set.seed(12345)
index <- sample(unclickbait, 11806, replace = FALSE)
index <- c(clickbait, index)
length(index)
#16000



####Youtube

#install.packages("rJava")
#install.packages("NLP")
#install.packages("openNLP")
library(rJava)
library(NLP)
library(openNLP)

##Need Sentence and word token annotations.
# postText <- raw_data[["postText"]]
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
p2 <- list()
index = sort(index)
i = 1
for (j in index){
  p2[[i]] <- annotate(postText[[j]], list(sent_token_annotator, word_token_annotator))
  i = i + 1
} 
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
p3 <- list()
i = 1
for (j in index){
  p3[[i]] <- annotate(postText[[j]], pos_tag_annotator, p2[[i]])
  i = i + 1
}
p3 <- annotate(postText, pos_tag_annotator, p2)
head(p3)
###analysis of first 16000 postText
##number of sentences
numSentence_postText <- c()
for (i in 1:16000){
  numSentence_postText[i] <- sum(p3[[i]]$type == "sentence")
}
#f_numSentence_postText <- cbind(numSentence_postText, label_num)
#f_numSentence_postText
##number of words
numWord_postText <- c()
for (i in 1:16000){
  numWord_postText[i] <- sum(p3[[i]]$type == "word")
}
#f_numWord_postText <- cbind(numWord_postText,numSentence_postText, label_num[1:16000])
#f_numWord_postText
##average length of sentence
lenSentence_postText <- c()
for (i in 1:16000){
  lenSentence_postText[i] <- mean(p3[[i]]$end[p3[[i]]$type == "sentence"] - p3[[i]]$start[p3[[i]]$type == "sentence"] + 1) 
}

len_postText <- c()
for (i in 1:16000){
  len_postText[i] <- sum(p3[[i]]$end[p3[[i]]$type == "sentence"] - p3[[i]]$start[p3[[i]]$type == "sentence"] + 1) 
}
#feature <- cbind(lenSentence_postText,f_numWord_postText)
#head(feature)
##average length of words
lenWord_postText <- c()
for (i in 1:16000){
  lenWord_postText[i] <- mean(p3[[i]]$end[p3[[i]]$type == "word"] - p3[[i]]$start[p3[[i]]$type == "word"] + 1) 
}

lenLongWord_postText <- c()
for (i in 1:16000){
  lenLongWord_postText[i] <- max(p3[[i]]$end[p3[[i]]$type == "word"] - p3[[i]]$start[p3[[i]]$type == "word"] + 1) 
}
#feature <- cbind(lenWord_postText,feature)
#Rename features
#colnames(feature) <- c("L.W_pText","L.S_pText,","n_W_pText","n_S_pText","class")
#head(feature)

# class(p3[[1]]$feature)
# p3[[1]]$feature[[3]] == "JJ"
###proportion of adj. in postText
adj_pText <- c()
for (i in 1:16000){
  adj_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    adj_pText[i] = sum(adj_pText[i], sum(p3[[i]]$feature[[j]] == "JJ", p3[[i]]$feature[[j]] == "JJR", p3[[i]]$feature[[j]] == "JJS"))
  }
}
# ave_adj_pText <- c()
# for (i in 1:16000){
#   ave_adj_pText[i] = adj_pText[i] / numSentence_postText[i]
# }
#rm(ave_adj_pText)
#feature <- cbind(ave_adj_pText,feature)
#feature

prop_adj_pText <- c()
for (i in 1:16000){
  prop_adj_pText[i] = adj_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_adj_pText, feature)
#feature

###proportion of JJS in postText
jjs_pText <- c()
for (i in 1:16000){
  jjs_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    jjs_pText[i] = sum(jjs_pText[i], p3[[i]]$feature[[j]] == "JJS")
  }
}
prop_jjs_pText <- c()
for (i in 1:16000){
  prop_jjs_pText[i] = jjs_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_jjs_pText, feature)
#head(feature)

###ave propernoun in postText
nnp_pText <- c()
for (i in 1:16000){
  nnp_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    nnp_pText[i] = sum(nnp_pText[i], p3[[i]]$feature[[j]] == "NNP", p3[[i]]$feature[[j]] == "NNPS")
  }
}
prop_nnp_pText <- c()
for (i in 1:16000){
  prop_nnp_pText[i] = nnp_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_nnp_pText, feature)
#head(feature)
###
###ave DT in postText
dt_pText <- c()
for (i in 1:16000){
  dt_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    dt_pText[i] = sum(dt_pText[i], p3[[i]]$feature[[j]] == "DT")
  }
}
prop_dt_pText <- c()
for (i in 1:16000){
  prop_dt_pText[i] = dt_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_dt_pText, feature)
#head(feature)
###
###ave adv in postText
adv_pText <- c()
for (i in 1:16000){
  adv_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    adv_pText[i] = sum(adv_pText[i], p3[[i]]$feature[[j]] == "RB", p3[[i]]$feature[[j]] == "RBR", p3[[i]]$feature[[j]] == "RBS")
  }
}
prop_adv_pText <- c()
for (i in 1:16000){
  prop_adv_pText[i] = adv_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_adv_pText, feature)
#head(feature)
###
###ave prp in postText
prp_pText <- c()
for (i in 1:16000){
  prp_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    prp_pText[i] = sum(prp_pText[i], p3[[i]]$feature[[j]] == "PRP", p3[[i]]$feature[[j]] == "PRP$")
  }
}
prop_prp_pText <- c()
for (i in 1:16000){
  prop_prp_pText[i] = prp_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_prp_pText, feature)
#head(feature)
###
#feature <- feature[, c(1,3,4,6,7,8,10,11,12,13,14)]

###proportion of nn in postText
nn_pText <- c()
for (i in 1:16000){
  nn_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    nn_pText[i] = sum(nn_pText[i], p3[[i]]$feature[[j]] == "NN", p3[[i]]$feature[[j]] == "NNS", p3[[i]]$feature[[j]] == "NNP", p3[[i]]$feature[[j]] == "NNPS")
  }
}
prop_nn_pText <- c()
for (i in 1:16000){
  prop_nn_pText[i] = nn_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_nn_pText, feature)
#head(feature)
###

###proportion of v in postText
v_pText <- c()
for (i in 1:16000){
  v_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    v_pText[i] = sum(v_pText[i], p3[[i]]$feature[[j]] == "VB", p3[[i]]$feature[[j]] == "VBD", p3[[i]]$feature[[j]] == "VBG", p3[[i]]$feature[[j]] == "VBN", p3[[i]]$feature[[j]] == "VBP", p3[[i]]$feature[[j]] == "VBZ")
  }
}
prop_v_pText <- c()
for (i in 1:16000){
  prop_v_pText[i] = v_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_v_pText, feature)
#head(feature)
###
###proportion of vbn in postText
vbn_pText <- c()
for (i in 1:16000){
  vbn_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    vbn_pText[i] = sum(vbn_pText[i], p3[[i]]$feature[[j]] == "VBN")
  }
}
prop_vbn_pText <- c()
for (i in 1:16000){
  prop_vbn_pText[i] = vbn_pText[i] / numWord_postText[i]
}

wrb_pText <- c()
for (i in 1:16000){
  wrb_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    wrb_pText[i] = sum(wrb_pText[i], p3[[i]]$feature[[j]] == "WRB")
  }
}
prop_wrb_pText <- c()
for (i in 1:16000){
  prop_wrb_pText[i] = wrb_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_vbn_pText, feature)
#head(feature)
###
###proportion of vbz in postText
vbz_pText <- c()
for (i in 1:16000){
  vbz_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    vbz_pText[i] = sum(vbz_pText[i], p3[[i]]$feature[[j]] == "VBZ")
  }
}
prop_vbz_pText <- c()
for (i in 1:16000){
  prop_vbz_pText[i] = vbz_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_vbz_pText, feature)
#head(feature)
#feature <- feature[,2:16]
###
###proportion of vbd in postText
vbd_pText <- c()
for (i in 1:16000){
  vbd_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    vbd_pText[i] = sum(vbd_pText[i], p3[[i]]$feature[[j]] == "VBD")
  }
}
prop_vbd_pText <- c()
for (i in 1:16000){
  prop_vbd_pText[i] = vbd_pText[i] / numWord_postText[i]
}
#feature <- cbind(prop_vbd_pText, feature)
#head(feature)
###
###proportion of vbp in postText
vbp_pText <- c()
for (i in 1:16000){
  vbp_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    vbp_pText[i] = sum(vbp_pText[i], p3[[i]]$feature[[j]] == "VBP")
  }
}
prop_vbp_pText <- c()
for (i in 1:16000){
  prop_vbp_pText[i] = vbp_pText[i] / numWord_postText[i]
}


cd_pText <- c()
for (i in 1:16000){
  cd_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    cd_pText[i] = sum(cd_pText[i], p3[[i]]$feature[[j]] == "CD")
  }
}
prop_cd_pText <- c()
for (i in 1:16000){
  prop_cd_pText[i] = cd_pText[i] / numWord_postText[i]
}

rb_pText <- c()
for (i in 1:16000){
  rb_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    rb_pText[i] = sum(rb_pText[i], p3[[i]]$feature[[j]] == "RB")
  }
}
prop_rb_pText <- c()
for (i in 1:16000){
  prop_rb_pText[i] = rb_pText[i] / numWord_postText[i]
}

md_pText <- c()
for (i in 1:16000){
  md_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    md_pText[i] = sum(md_pText[i], p3[[i]]$feature[[j]] == "MD")
  }
}
prop_md_pText <- c()
for (i in 1:16000){
  prop_md_pText[i] = md_pText[i] / numWord_postText[i]
}

wh_pText <- c()
for (i in 1:16000){
  wh_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    wh_pText[i] = sum(wh_pText[i], p3[[i]]$feature[[j]] == "WDT", p3[[i]]$feature[[j]] == "WP", p3[[i]]$feature[[j]] == "WP$", p3[[i]]$feature[[j]] == "WRB")
  }
}
prop_wh_pText <- c()
for (i in 1:16000){
  prop_wh_pText[i] = wh_pText[i] / numWord_postText[i]
}


#feature <- cbind(prop_vbp_pText, feature)
#head(feature)
###
#write.csv(feature, "feature.csv")


####targetTitle
data_targetTitle <- c()
for (i in 1:17581){
  data_targetTitle[i] <- gsub("\\{\\\"targetDescription\\\":.* \\\"targetTitle\\\":", "\\{\\\"targetTitle\\\":", data_startDescrip[i], perl = TRUE)
}
targetTitle <- list()
for (i in 1:17581){
  targetTitle[i] <- fromJSON(data_targetTitle[i])
}
#head(targetTitle)
#targetTitle[[1]]



sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
t2 <- list()
i = 1
for (j in index){
  t2[[i]] <- annotate(targetTitle[[j]], list(sent_token_annotator, word_token_annotator))
  i = i + 1
}
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
t3 <- list()
i = 1
for (j in index){
  t3[[i]] <- annotate(targetTitle[[j]], pos_tag_annotator, t2[[i]])
  i = i + 1
}
t3 <- annotate(targetTitle, pos_tag_annotator, t2)
t3

###
##number of sentences
numSentence_targetTitle <- c()
for (i in 1:16000){
  numSentence_targetTitle[i] <- sum(t3[[i]]$type == "sentence")
}
#numSentence_targetTitle
#label_num <- label_num[1:16000]
#f_numSentence_targetTitle <- cbind(numSentence_targetTitle, label_num)
#f_numSentence_targetTitle
##number of words
numWord_targetTitle <- c()
for (i in 1:16000){
  numWord_targetTitle[i] <- sum(t3[[i]]$type == "word")
}
#numWord_targetTitle
#f_numWord_targetTitle <- cbind(numWord_targetTitle,numSentence_targetTitle, label_num[1:16000])
#f_numWord_targetTitle
##average length of sentence
lenSentence_targetTitle <- c()
for (i in 1:16000){
  lenSentence_targetTitle[i] <- mean(t3[[i]]$end[t3[[i]]$type == "sentence"] - t3[[i]]$start[t3[[i]]$type == "sentence"] + 1) 
}

len_targetTitle <- c()
for (i in 1:16000){
  len_targetTitle[i] <- sum(t3[[i]]$end[t3[[i]]$type == "sentence"] - t3[[i]]$start[t3[[i]]$type == "sentence"] + 1) 
}
#feature <- cbind(lenSentence_targetTitle,f_numWord_targetTitle)
#head(feature)
##average length of words
lenWord_targetTitle <- c()
for (i in 1:16000){
  lenWord_targetTitle[i] <- mean(t3[[i]]$end[t3[[i]]$type == "word"] - t3[[i]]$start[t3[[i]]$type == "word"] + 1) 
}

lenLongWord_targetTitle <- c()
for (i in 1:16000){
  lenLongWord_targetTitle[i] <- max(t3[[i]]$end[t3[[i]]$type == "word"] - t3[[i]]$start[t3[[i]]$type == "word"] + 1) 
}
#feature <- cbind(lenWord_targetTitle,feature)
#Rename features
#colnames(feature) <- c("L.W_tT","L.S_tT,","n_W_tT","n_S_tT","class")
#head(feature)

# class(t3[[1]]$feature)
# t3[[1]]$feature[[3]] == "JJ"
###proportion of adj. in targetTitle
adj_tTitle <- c()
for (i in 1:16000){
  adj_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    adj_tTitle[i] = sum(adj_tTitle[i], sum(t3[[i]]$feature[[j]] == "JJ", t3[[i]]$feature[[j]] == "JJR", t3[[i]]$feature[[j]] == "JJS"))
  }
}
#ave_adj_tTitle <- c()
# for (i in 1:16000){
#   ave_adj_tTitle[i] = adj_tTitle[i] / numSentence_targetTitle[i]
# }
# 
# feature

prop_adj_tTitle <- c()
for (i in 1:16000){
  prop_adj_tTitle[i] = adj_tTitle[i] / numWord_targetTitle[i]
}
#prop_adj_tTitle
#feature <- cbind(prop_adj_tTitle, feature)
#feature

###proportion of JJS in targetTitle
jjs_tTitle <- c()
for (i in 1:16000){
  jjs_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    jjs_tTitle[i] = sum(jjs_tTitle[i], t3[[i]]$feature[[j]] == "JJS")
  }
}
prop_jjs_tTitle <- c()
for (i in 1:16000){
  prop_jjs_tTitle[i] = jjs_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_jjs_tTitle, feature)
#head(feature)

###ave propernoun in targetTitle
nnp_tTitle <- c()
for (i in 1:16000){
  nnp_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    nnp_tTitle[i] = sum(nnp_tTitle[i], t3[[i]]$feature[[j]] == "NNP", t3[[i]]$feature[[j]] == "NNPS")
  }
}

prop_nnp_tTitle <- c()
for (i in 1:16000){
  prop_nnp_tTitle[i] = nnp_tTitle[i] / numWord_targetTitle[i]
}


rb_tTitle <- c()
for (i in 1:16000){
  rb_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    rb_tTitle[i] = sum(rb_tTitle[i], t3[[i]]$feature[[j]] == "RB")
  }
}

prop_rb_tTitle <- c()
for (i in 1:16000){
  prop_rb_tTitle[i] = rb_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_nnp_tTitle, feature)
#head(feature)
###
###ave DT in targetTitle
dt_tTitle <- c()
for (i in 1:16000){
  dt_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    dt_tTitle[i] = sum(dt_tTitle[i], t3[[i]]$feature[[j]] == "DT")
  }
}
prop_dt_tTitle <- c()
for (i in 1:16000){
  prop_dt_tTitle[i] = dt_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_dt_tTitle, feature)
#head(feature)
###
###ave adv in targetTitle
adv_tTitle <- c()
for (i in 1:16000){
  adv_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    adv_tTitle[i] = sum(adv_tTitle[i], t3[[i]]$feature[[j]] == "RB", t3[[i]]$feature[[j]] == "RBR", t3[[i]]$feature[[j]] == "RBS")
  }
}
prop_adv_tTitle <- c()
for (i in 1:16000){
  prop_adv_tTitle[i] = adv_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_adv_tTitle, feature)
#head(feature)
###
###ave prp in targetTitle
prp_tTitle <- c()
for (i in 1:16000){
  prp_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    prp_tTitle[i] = sum(prp_tTitle[i], t3[[i]]$feature[[j]] == "PRP", t3[[i]]$feature[[j]] == "PRP$")
  }
}
prop_prp_tTitle <- c()
for (i in 1:16000){
  prop_prp_tTitle[i] = prp_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_prp_tTitle, feature)
#head(feature)


###proportion of nn in targetTitle
nn_tTitle <- c()
for (i in 1:16000){
  nn_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    nn_tTitle[i] = sum(nn_tTitle[i], t3[[i]]$feature[[j]] == "NN", t3[[i]]$feature[[j]] == "NNS", t3[[i]]$feature[[j]] == "NNP", t3[[i]]$feature[[j]] == "NNPS")
  }
}
prop_nn_tTitle <- c()
for (i in 1:16000){
  prop_nn_tTitle[i] = nn_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_nn_tTitle, feature)
#head(feature)
###

###proportion of v in targetTitle
v_tTitle <- c()
for (i in 1:16000){
  v_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    v_tTitle[i] = sum(v_tTitle[i], t3[[i]]$feature[[j]] == "VB", t3[[i]]$feature[[j]] == "VBD", t3[[i]]$feature[[j]] == "VBG", t3[[i]]$feature[[j]] == "VBN", t3[[i]]$feature[[j]] == "VBP", t3[[i]]$feature[[j]] == "VBZ")
  }
}
prop_v_tTitle <- c()
for (i in 1:16000){
  prop_v_tTitle[i] = v_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_v_tTitle, feature)
#head(feature)
###
###proportion of vbn in targetTitle
vbn_tTitle <- c()
for (i in 1:16000){
  vbn_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    vbn_tTitle[i] = sum(vbn_tTitle[i], t3[[i]]$feature[[j]] == "VBN")
  }
}
prop_vbn_tTitle <- c()
for (i in 1:16000){
  prop_vbn_tTitle[i] = vbn_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_vbn_tTitle, feature)
#head(feature)
###
###proportion of vbz in targetTitle
vbz_tTitle <- c()
for (i in 1:16000){
  vbz_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    vbz_tTitle[i] = sum(vbz_tTitle[i], t3[[i]]$feature[[j]] == "VBZ")
  }
}
prop_vbz_tTitle <- c()
for (i in 1:16000){
  prop_vbz_tTitle[i] = vbz_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_vbz_tTitle, feature)
#head(feature)
###
###proportion of vbd in targetTitle
vbd_tTitle <- c()
for (i in 1:16000){
  vbd_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    vbd_tTitle[i] = sum(vbd_tTitle[i], t3[[i]]$feature[[j]] == "VBD")
  }
}
prop_vbd_tTitle <- c()
for (i in 1:16000){
  prop_vbd_tTitle[i] = vbd_tTitle[i] / numWord_targetTitle[i]
}
#feature <- cbind(prop_vbd_tTitle, feature)
#head(feature)
###
###proportion of vbp in targetTitle
vbp_tTitle <- c()
for (i in 1:16000){
  vbp_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    vbp_tTitle[i] = sum(vbp_tTitle[i], t3[[i]]$feature[[j]] == "VBP")
  }
}
prop_vbp_tTitle <- c()
for (i in 1:16000){
  prop_vbp_tTitle[i] = vbp_tTitle[i] / numWord_targetTitle[i]
}
cd_tTitle <- c()
for (i in 1:16000){
  cd_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    cd_tTitle[i] = sum(cd_tTitle[i], t3[[i]]$feature[[j]] == "CD")
  }
}
prop_cd_tTitle <- c()
for (i in 1:16000){
  prop_cd_tTitle[i] = cd_tTitle[i] / numWord_targetTitle[i]
}

md_tTitle <- c()
for (i in 1:16000){
  md_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    md_tTitle[i] = sum(md_tTitle[i], t3[[i]]$feature[[j]] == "MD")
  }
}
prop_md_tTitle <- c()
for (i in 1:16000){
  prop_md_tTitle[i] = md_tTitle[i] / numWord_targetTitle[i]
}

wh_tTitle <- c()
for (i in 1:16000){
  wh_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    wh_tTitle[i] = sum(wh_tTitle[i], t3[[i]]$feature[[j]] == "WDT", t3[[i]]$feature[[j]] == "WP", t3[[i]]$feature[[j]] == "WP$", t3[[i]]$feature[[j]] == "WRB")
  }
}
prop_wh_tTitle <- c()
for (i in 1:16000){
  prop_wh_tTitle[i] = wh_tTitle[i] / numWord_targetTitle[i]
}

###Excel File
feature <- cbind(len_postText,len_targetTitle,lenLongWord_postText,lenLongWord_targetTitle,lenSentence_postText,lenSentence_targetTitle,
                 lenWord_postText,lenWord_targetTitle,prop_adj_pText,prop_adj_tTitle,prop_adv_pText,prop_adv_tTitle,
                 prop_cd_pText,prop_cd_tTitle,prop_dt_pText,prop_dt_tTitle,prop_jjs_pText,prop_jjs_tTitle,prop_md_pText,prop_md_tTitle,
                 prop_nn_pText,prop_nn_tTitle,prop_nnp_pText,prop_nnp_tTitle,prop_prp_pText,prop_prp_tTitle,
                 prop_v_pText,prop_v_tTitle,prop_vbd_pText,prop_vbd_tTitle,prop_vbn_pText,prop_vbn_tTitle,prop_vbp_pText,prop_vbp_tTitle,
                 prop_vbz_pText,prop_vbz_tTitle,prop_wh_pText,prop_wh_tTitle)

dim(feature)

# ####Add more features
# startNum_pt <- c()
# for (i in 1:16000){
#   if (p3[[i]]$features[[which(p3[[i]]$type == "word")[1]]] == "CD"){
#     startNum_pt[i] = 1
#   }else{
#     startNum_pt[i] = 0
#   }
# }
# 
# startNum_tt <- c()
# for (i in 1:16000){
#   if (t3[[i]]$features[[which(t3[[i]]$type == "word")[1]]] == "CD"){
#     startNum_tt[i] = 1
#   }else{
#     startNum_tt[i] = 0
#   }
# }
library(Matrix)
library(quanteda)
postText <- unlist(postText)
postText.tokens <- tokens(postText[index], what = "word")
start <- c()
for (i in 1:16000){
  start[i] <- postText.tokens[[i]][1]
}
start <- as.numeric(start)
startNum_pt <- rep(0, 16000)
startNum_pt[!is.na(start)] = 1


targetTitle <- unlist(targetTitle)
targetTitle.tokens <- tokens(targetTitle[index], what = "word")
start <- c()
for (i in 1:16000){
  start[i] <- targetTitle.tokens[[i]][1]
}
start <- as.numeric(start)
startNum_tt <- rep(0, 16000)
startNum_tt[!is.na(start)] = 1
feature <- cbind(feature, startNum_pt,startNum_tt)


in_pText <- c()
for (i in 1:16000){
  in_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    in_pText[i] = sum(in_pText[i], p3[[i]]$feature[[j]] == "IN")
  }
}
prop_in_pText <- c()
for (i in 1:16000){
  prop_in_pText[i] = in_pText[i] / numWord_postText[i]
}

in_tTitle <- c()
for (i in 1:16000){
  in_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    in_tTitle[i] = sum(in_tTitle[i], t3[[i]]$feature[[j]] == "IN")
  }
}
prop_in_tTitle <- c()
for (i in 1:16000){
  prop_in_tTitle[i] = in_tTitle[i] / numWord_targetTitle[i]
}
feature <- cbind(feature, prop_in_pText, prop_in_tTitle)


nns_pText <- c()
for (i in 1:16000){
  nns_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    nns_pText[i] = sum(nns_pText[i], p3[[i]]$feature[[j]] == "NN")
  }
}
prop_nns_pText <- c()
for (i in 1:16000){
  prop_nns_pText[i] = nns_pText[i] / numWord_postText[i]
}

nns_tTitle <- c()
for (i in 1:16000){
  nns_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    nns_tTitle[i] = sum(nns_tTitle[i], t3[[i]]$feature[[j]] == "NN")
  }
}
prop_nns_tTitle <- c()
for (i in 1:16000){
  prop_nns_tTitle[i] = nns_tTitle[i] / numWord_targetTitle[i]
}




rbs_pText <- c()
for (i in 1:16000){
  rbs_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    rbs_pText[i] = sum(rbs_pText[i], p3[[i]]$feature[[j]] == "RBS")
  }
}
prop_rbs_pText <- c()
for (i in 1:16000){
  prop_rbs_pText[i] = rbs_pText[i] / numWord_postText[i]
}

rbs_tTitle <- c()
for (i in 1:16000){
  rbs_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    rbs_tTitle[i] = sum(rbs_tTitle[i], t3[[i]]$feature[[j]] == "RBS")
  }
}
prop_rbs_tTitle <- c()
for (i in 1:16000){
  prop_rbs_tTitle[i] = rbs_tTitle[i] / numWord_targetTitle[i]
}





wdt_pText <- c()
for (i in 1:16000){
  wdt_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    wdt_pText[i] = sum(wdt_pText[i], p3[[i]]$feature[[j]] == "WDT")
  }
}
prop_wdt_pText <- c()
for (i in 1:16000){
  prop_wdt_pText[i] = wdt_pText[i] / numWord_postText[i]
}

wdt_tTitle <- c()
for (i in 1:16000){
  wdt_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    wdt_tTitle[i] = sum(wdt_tTitle[i], t3[[i]]$feature[[j]] == "WDT")
  }
}
prop_wdt_tTitle <- c()
for (i in 1:16000){
  prop_wdt_tTitle[i] = wdt_tTitle[i] / numWord_targetTitle[i]
}





wp_pText <- c()
for (i in 1:16000){
  wp_pText[i] = 0
  for (j in 1:length(p3[[i]])){
    wp_pText[i] = sum(wp_pText[i], p3[[i]]$feature[[j]] == "WP")
  }
}
prop_wp_pText <- c()
for (i in 1:16000){
  prop_wp_pText[i] = wp_pText[i] / numWord_postText[i]
}

wp_tTitle <- c()
for (i in 1:16000){
  wp_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    wp_tTitle[i] = sum(wp_tTitle[i], t3[[i]]$feature[[j]] == "WP")
  }
}
prop_wp_tTitle <- c()
for (i in 1:16000){
  prop_wp_tTitle[i] = wp_tTitle[i] / numWord_targetTitle[i]
}


wrb_tTitle <- c()
for (i in 1:16000){
  wrb_tTitle[i] = 0
  for (j in 1:length(t3[[i]])){
    wrb_tTitle[i] = sum(wrb_tTitle[i], t3[[i]]$feature[[j]] == "WRB")
  }
}
prop_wrb_tTitle <- c()
for (i in 1:16000){
  prop_wrb_tTitle[i] = wrb_tTitle[i] / numWord_targetTitle[i]
}

####
exclaimation_pt <- c()
for (i in 1:16000){
  if (unlist(gregexpr("!",postText[[i]])) == -1){
    exclaimation_pt[i] = 0
  }else{
    exclaimation_pt[i] = length(unlist(gregexpr("!",postText[[i]])))
  }
}

exclaimation_tt <- c()
for (i in 1:16000){
  if (unlist(gregexpr("!",targetTitle[[i]])) == -1){
    exclaimation_tt[i] = 0
  }else{
    exclaimation_tt[i] = length(unlist(gregexpr("!",targetTitle[[i]])))
  }
}


####
comma_pt <- c()
for (i in 1:16000){
  if (unlist(gregexpr(",",postText[[i]])) == -1){
    comma_pt[i] = 0
  }else{
    comma_pt[i] = length(unlist(gregexpr(",",postText[[i]])))
  }
}


comma_tt <- c()
for (i in 1:16000){
  if (unlist(gregexpr(",",targetTitle[[i]])) == -1){
    comma_tt[i] = 0
  }else{
    comma_tt[i] = length(unlist(gregexpr(",",targetTitle[[i]])))
  }
}

feature <- cbind(feature, exclaimation_pt,exclaimation_tt,comma_pt,comma_tt)
feature <- cbind(feature, prop_rb_pText, prop_rb_tTitle)
dim(feature)


#########################################################################
install.packages("Matrix")
install.packages("quanteda")
library(quanteda)
postTextPart <- unlist(postText)[index]
postText.tokens <- tokens(postTextPart, what = "word", remove_numbers = TRUE, remove_symbols = TRUE, remove_hyphens = TRUE, remove_punct = TRUE)
postText.tokens <- tokens_tolower(postText.tokens)
postText.tokens <- tokens_select(postText.tokens, stopwords(), selection = "remove")
postText.tokens <- tokens_wordstem(postText.tokens, language = "english")

##Reduce the tokens
postText.tokens <- tokens_tolower(postText.tokens)
postText.tokens <- tokens_select(postText.tokens, stopwords(), selection = "remove")
postText.tokens <- tokens_wordstem(postText.tokens, language = "english")
##Add feature:Num_tokensReduced
num_tokensR <- c()
for (i in 1:16000){
  num_tokensR[i] <- length(postText.tokens[[i]]) 
}

feature <- cbind(feature, num_tokens, num_tokensR)
dim(feature)
# #######################################################################################
# 
# 
# postText.tokens.dfm <- dfm(postText.tokens)
# postText.tokens.matrix <- as.matrix(postText.tokens.dfm)
# #dim(postText.tokens.matrix)
# postText.tokens.df <- cbind(as.data.frame(postText.tokens.dfm),label_num)
# names(postText.tokens.df) <- make.names(names(postText.tokens.df),unique = TRUE)
# ##TF & IDF
# term.frequency <- function(row){
#   row/sum(row)
# }
# 
# inverse.doc.freq <- function(col){
#   corpus.size <- length(col)
#   doc.count <- length(which(col > 0))
#   
#   log10(corpus.size / doc.count)
# }
# 
# tf.idf <- function(tf, idf){
#   tf * idf
# }
# 
# postText.tokens.dataframe <- apply(postText.tokens.matrix, 1, term.frequency)
# dim(postText.tokens.dataframe)
# 
# postText.tokens.idf <- apply(postText.tokens.matrix, 2, inverse.doc.freq)
# str(postText.tokens.idf)
# 
# postText.tokens.tfidf <- apply(postText.tokens.dataframe, 2, tf.idf, idf = postText.tokens.idf)
# postText.tokens.tfidf <- t(postText.tokens.tfidf)
# 
# incomplete.cases <- which(!complete.cases(postText.tokens.tfidf))
# postText.tokens.tfidf[incomplete.cases,] <- rep(0.0, ncol(postText.tokens.tfidf))
# dim(postText.tokens.tfidf)
# sum(which(!complete.cases(postText.tokens.tfidf)))
# 
# postText.tokens <- tokens_ngrams(postText.tokens, n = 2)
# ###########################################################################################

##POS n_gram
p3_features <- list()
for (i in 1:16000){
  p3_features[[i]] = as.character(unlist(p3[[i]]$features[p3[[i]]$type == "word"]))
}

POS_2gram_pt <- list()
for (i in 1:16000){
  POS_2gram_pt[[i]] = c("null","null")
}
for (i in 1:16000){
  for (j in 1:(length(p3_features[[i]])-1)){
    POS_2gram_pt[[i]][j] = paste(unlist(p3_features[[i]][j]), unlist(p3_features[[i]][j + 1]), sep = "_")
  }
}

##prop_POS2_gram
prop_NNP2NNP_pt <- c()
for (i in 1:16000){
  prop_NNP2NNP_pt[i] = sum(POS_2gram_pt[[i]] == "NNP_NNP")/length(POS_2gram_pt[[i]])
}

prop_NN2NN_pt <- c()
for (i in 1:16000){
  prop_NN2NN_pt[i] = sum(POS_2gram_pt[[i]] == "NN_NN")/length(POS_2gram_pt[[i]])
}

prop_NN2IN_pt <- c()
for (i in 1:16000){
  prop_NN2IN_pt[i] = sum(POS_2gram_pt[[i]] == "NN_IN")/length(POS_2gram_pt[[i]])
}

prop_IN2NN_pt <- c()
for (i in 1:16000){
  prop_IN2NN_pt[i] = sum(POS_2gram_pt[[i]] == "IN_NN")/length(POS_2gram_pt[[i]])
}
prop_JJ2NN_pt <- c()
for (i in 1:16000){
  prop_JJ2NN_pt[i] = sum(POS_2gram_pt[[i]] == "JJ_NN")/length(POS_2gram_pt[[i]])
}
prop_IN2JJ_pt <- c()
for (i in 1:16000){
  prop_IN2JJ_pt[i] = sum(POS_2gram_pt[[i]] == "IN_JJ")/length(POS_2gram_pt[[i]])
}
prop_NN2NNP_pt <- c()
for (i in 1:16000){
  prop_NN2NNP_pt[i] = sum(POS_2gram_pt[[i]] == "NN_NNP")/length(POS_2gram_pt[[i]])
}
prop_NNP2IN_pt <- c()
for (i in 1:16000){
  prop_NNP2IN_pt[i] = sum(POS_2gram_pt[[i]] == "NNP_IN")/length(POS_2gram_pt[[i]])
}

prop_VBN2NN_pt <- c()
for (i in 1:16000){
  prop_VBN2NN_pt[i] = sum(POS_2gram_pt[[i]] == "VBN_NN")/length(POS_2gram_pt[[i]])
}
prop_VBN2IN_pt <- c()
for (i in 1:16000){
  prop_VBN2IN_pt[i] = sum(POS_2gram_pt[[i]] == "VBN_IN")/length(POS_2gram_pt[[i]])
}
prop_JJ2NNP_pt <- c()
for (i in 1:16000){
  prop_JJ2NNP_pt[i] = sum(POS_2gram_pt[[i]] == "JJ_NNP")/length(POS_2gram_pt[[i]])
}
prop_DT2NN_pt <- c()
for (i in 1:16000){
  prop_DT2NN_pt[i] = sum(POS_2gram_pt[[i]] == "DT_NN")/length(POS_2gram_pt[[i]])
}
prop_NNP2VBD_pt <- c()
for (i in 1:16000){
  prop_NNP2VBD_pt[i] = sum(POS_2gram_pt[[i]] == "NNP_VBD")/length(POS_2gram_pt[[i]])
}
prop_PRP2VBP_pt <- c()
for (i in 1:16000){
  prop_PRP2VBP_pt[i] = sum(POS_2gram_pt[[i]] == "PRP_VBP")/length(POS_2gram_pt[[i]])
}
prop_NNP2VBZ_pt <- c()
for (i in 1:16000){
  prop_NNP2VBZ_pt[i] = sum(POS_2gram_pt[[i]] == "NNP_VBZ")/length(POS_2gram_pt[[i]])
}
prop_IN2NNP_pt <- c()
for (i in 1:16000){
  prop_IN2NNP_pt[i] = sum(POS_2gram_pt[[i]] == "IN_NNP")/length(POS_2gram_pt[[i]])
}
prop_NNP2NNS_pt <- c()
for (i in 1:16000){
  prop_NNP2NNS_pt[i] = sum(POS_2gram_pt[[i]] == "NNP_NNS")/length(POS_2gram_pt[[i]])
}


##POS 3_gram
p3_features <- list()
for (i in 1:16000){
  p3_features[[i]] = as.character(unlist(p3[[i]]$features[p3[[i]]$type == "word"]))
}

POS_3gram_pt <- list()
for (i in 1:16000){
  POS_3gram_pt[[i]] = c("null","null")
}
for (i in 1:16000){
  for (j in 1:(length(p3_features[[i]])-2)){
    POS_3gram_pt[[i]][j] = paste(unlist(p3_features[[i]][j]), unlist(p3_features[[i]][j + 1]), unlist(p3_features[[i]][j + 2]), sep = "_")
  }
}

##prop_POS3_gram
prop_NNP3NNP3VBZ_pt <- c()
for (i in 1:16000){
  prop_NNP3NNP3VBZ_pt[i] = sum(POS_3gram_pt[[i]] == "NNP_NNP_VBZ")/length(POS_3gram_pt[[i]])
}
prop_NN3IN3NNP_pt <- c()
for (i in 1:16000){
  prop_NN3IN3NNP_pt[i] = sum(POS_3gram_pt[[i]] == "NN_IN_NNP")/length(POS_3gram_pt[[i]])
}
prop_NNP3NNP3VBZ_pt <- c()
for (i in 1:16000){
  prop_NNP3NNP3VBZ_pt[i] = sum(POS_3gram_pt[[i]] == "NNP_NNP_VBZ")/length(POS_3gram_pt[[i]])
}
prop_NNP3NN3NN_pt <- c()
for (i in 1:16000){
  prop_NNP3NN3NN_pt[i] = sum(POS_3gram_pt[[i]] == "NNP_NN_NN")/length(POS_3gram_pt[[i]])
}
prop_NNP3NNP3NNP_pt <- c()
for (i in 1:16000){
  prop_NNP3NNP3NNP_pt[i] = sum(POS_3gram_pt[[i]] == "NNP_NNP_NNP")/length(POS_3gram_pt[[i]])
}
prop_NNP3NNP3NN_pt <- c()
for (i in 1:16000){
  prop_NNP3NNP3NN_pt[i] = sum(POS_3gram_pt[[i]] == "NNP_NNP_NN")/length(POS_3gram_pt[[i]])
}
prop_IN3NNP3NNP_pt <- c()
for (i in 1:16000){
  prop_IN3NNP3NNP_pt[i] = sum(POS_3gram_pt[[i]] == "IN_NNP_NNP")/length(POS_3gram_pt[[i]])
}




label_num <- as.character(label_num[1:16000])
feature <- data.frame()
feature <- data.frame(len_postText, len_targetTitle, lenLongWord_postText, lenLongWord_targetTitle,
                 lenSentence_postText, lenSentence_targetTitle, lenWord_postText, lenWord_targetTitle,
                 prop_adj_pText, prop_adj_tTitle, prop_adv_pText, prop_adv_tTitle,
                 prop_cd_pText, prop_cd_tTitle, prop_dt_pText, prop_dt_tTitle, prop_jjs_pText, prop_jjs_tTitle,
                 prop_md_pText, prop_md_tTitle, prop_nn_pText, prop_nn_tTitle, prop_nnp_pText, prop_nnp_tTitle,
                 prop_prp_pText, prop_prp_tTitle, prop_v_pText, prop_v_tTitle, prop_vbd_pText, prop_vbd_tTitle,
                 prop_vbn_pText, prop_vbn_tTitle, prop_vbp_pText, prop_vbp_tTitle, prop_vbz_pText, prop_vbz_tTitle,
                 prop_wrb_pText, prop_wrb_tTitle, prop_nns_pText, prop_nns_tTitle,
                 prop_wp_pText, prop_wp_tTitle, prop_wdt_pText, prop_wdt_tTitle, prop_rbs_pText, prop_rbs_tTitle,
                 prop_wh_pText, prop_wh_tTitle, prop_in_pText, prop_in_tTitle, prop_rb_pText, prop_rb_tTitle,
                 startNum_pt, startNum_tt, exclaimation_pt, exclaimation_tt, comma_pt, comma_tt, num_tokens, num_tokensR,
                 prop_NNP2NNP_pt, prop_NN2NN_pt, prop_NN2IN_pt, prop_IN2NN_pt, prop_JJ2NN_pt, prop_IN2JJ_pt,
                 prop_NN2NNP_pt, prop_NNP2IN_pt, prop_VBN2NN_pt, prop_VBN2IN_pt, prop_JJ2NNP_pt, prop_DT2NN_pt,
                 prop_NNP2VBD_pt, prop_PRP2VBP_pt, prop_NNP2VBZ_pt, prop_IN2NNP_pt, prop_NNP3NNP3VBZ_pt, prop_NN3IN3NNP_pt,
                 prop_NNP3NN3NN_pt, prop_NNP3NNP3VBZ_pt, prop_NNP3NNP3NN_pt, prop_NNP3NNP3NNP_pt, prop_IN3NNP3NNP_pt, label_num)
dim(feature)

library(farff)

names(trained) <- make.names(names(trained),unique = TRUE)


feature <- cbind(lenSentence_postText,feature)
write.csv(feature, "feature_tT2")

###Punctuation Overuse Point Occurence
data_punct <- 0
for (i in 1:length(targetTitle)){
  if (grepl("\\!+|\\?+\\*+", targetTitle[i]) == TRUE){
    data_punct <- data_punct + 1
  }
}
data_punct.percentage <- (data_punct / length(targetTitle))*100

###Begins with 'you' variation in target Title
for (i in 1:length(targetTitle)){
  if (tolower((stringr::str_extract(targetTitle[i], '^.{0,3}'))) == c("you")){
    if (tolower((stringr::str_extract(targetTitle[i], '^.{0,4}'))) != c("youn")){
      print(targetTitle[i])
    }
  }
}

###Begins with 'you' variation in postText
for (i in 1:length(postText)){
  if ((stringr::str_extract(postText[i], '^.{0,3}')) == c("You")){
    if (tolower((stringr::str_extract(postText[i], '^.{0,4}'))) != c("youn")){
      print(postText[i])
    }
  }
}

