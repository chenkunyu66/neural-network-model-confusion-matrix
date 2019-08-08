# devtools::install_github("rstudio/keras")
# library(keras)
# install_keras()
library(keras)
library(foreign)
library(corpcor)
library(tseries)
library(quantmod)
library(plyr)
library(dplyr)
library(DMwR)
library(Hmisc)
library(ROCR)
library(CausalImpact)
library(devtools)
library(pcalg)
# devtools::install_github("rstudio/keras", force = TRUE)
#library(reticulate)

library(caret)
library(e1071)
#library(tensorflow)

data<-read.xport("F:/wifeintern/DEMO_I.XPT")
data_food2<-read.xport("F:/wifeintern/DR2IFF_I.XPT")
data_food3<-read.xport("F:/wifeintern/DR1TOT_I.XPT")
data_food4<-read.xport("F:/wifeintern/DR2TOT_I.XPT")
data_food1<-read.xport("F:/wifeintern/DR2IFF_I.XPT")


# data_question1 <-read.xport("F:/wifeintern/ACQ_I.XPT")
# 
# #questionnaire_1 = left_join(food_1, data_question1, by = "SEQN")
# #rm(data_question1)
# #rm(food_1)
# #gc()
# 
# data_question2 <-read.xport("F:/wifeintern/ALQ_I.XPT")
# 
# #questionnaire_1 = left_join(questionnaire_1, data_question2, by = "SEQN")
# #rm(data_question2)
# #gc()
# 
# questionnaire_1 = left_join(data_question1, data_question2, by = "SEQN")
# rm(data_question1)
# rm(data_question2)
# gc()
# 
# questionnaire_1 = left_join(data_question1, data_food1, by = "SEQN")
# questionnaire_1 = left_join(data_question1, data_food2, by = "SEQN")
# questionnaire_1 = left_join(data_question1, data_food3, by = "SEQN")
# questionnaire_1 = left_join(data_question1, data_food4, by = "SEQN")
# questionnaire_1 = left_join(data_question1, data, by = "SEQN")
# 
# 
# 
# 
# # food_1 = left_join(data_food2, data_food3, by="SEQN")
# # food_1 =left_join(food_1, data_food4, by = "SEQN")
# # food_1 = left_join(food_1, data, by = "SEQN")
# # food_1 = left_join(food_1, data_food1, by = "SEQN")
# 
# rm(data_food2)
# rm(data_food3)
# rm(data_food4)
# rm(data)
# rm(data_food1)
# gc()
# 
# #questionnaire_1 = left_join(questionnaire_1, food_1, by = "SEQN")
# #rm(food_1)
# gc()
# 
# data_question3 <-read.xport("F:/wifeintern/AUQ_I.XPT")
# questionnaire_1 = left_join(questionnaire_1, data_question3, by = "SEQN")
# rm(data_question3)
# gc()
# data_question4 <-read.xport("F:/wifeintern/BPQ_I.XPT")
# questionnaire_1 = left_join(questionnaire_1, data_question4, by = "SEQN")
# rm(data_question4)
# gc()
# data_question5 <-read.xport("F:/wifeintern/CBQ_I.XPT")
# questionnaire_1 = left_join(questionnaire_1, data_question5, by = "SEQN")
# rm(data_question5)
# gc()
# 
# dpq = na.omit(dpq)
# temp = vector(mode="numeric", length = nrow(dpq))
# dpq = cbind(dpq,temp)
# for (i in 1:nrow(dpq)){
#   if ((dpq[i,2]>=2)|(dpq[i,3]>=2)|(dpq[i,4]>=2)|(dpq[i,5]>=2)|(dpq[i,6]>=2)|(dpq[i,7]>=2)|(dpq[i,8]>=2)|(dpq[i,9]>=2)|(dpq[i,10]>=2))
#     dpq[i,12]<-1
#   else dpq[i,12]<-0
# }
# 
# dpq = dpq[,c(1,12)]
# all = questionnaire_1
# all[is.na(all)] <- 0
# newAll <- all
# gc()
# newAll = centralImputation(newAll)
# gc()
# cov = cov(newAll, method = "spearman")
# gc()
# cor = cor(newAll, method = "spearman")
# gc()
# p = cor2pcor(cov)
# gc()
# # 
# # allwithmeasure = left_join(dpq, newAll, by="SEQN")
# # mental_cov = cov(allwithmeasure, method = "spearman")
# # mental_cor = cor(allwithmeasure, method = "spearman")
# # mental_pcor = cor2pcor(mental_cov)
# # 
# # colnames(mental_pcor) = colnames(mental_cor)
# # rownames(mental_pcor) = colnames(mental_cor)
# # pcormeasure = mental_pcor[,2]
# # pcormeasure = pcormeasure[c(-1,-2)]
# # pcorresult = pcormeasure[-which(abs(pcormeasure)>1)]
# # cov = cov[rowSums(cov != 0) != 0,]
# # gc()
# #
data_all = left_join(data, data_food2, by = "SEQN")
rm(data_food2)
gc()
data_all = left_join(data, data_food1, by = "SEQN")
rm(data_food1)
gc()
data_all = left_join(data, data_food3, by = "SEQN")
rm(data_food3)
gc()
data_all = left_join(data, data_food4, by = "SEQN")
rm(data_food4)
gc()

data_all <- apply(data_all,2, function(data_all){
  if(sum(is.na(data_all))>0){
    data_all[is.na(data_all)]<- quantile(data_all,na.rm = T, probs = 0.5)
  }
  data_all
})

mental <- read.xport("F:/wifeintern/DPQ_I.XPT")

mental <- apply(mental,2, function(mental){
  if(sum(is.na(mental))>0){
    mental[is.na(mental)]<- quantile(mental,na.rm = T, probs = 0.5)
  }
  mental
})


temp = vector(mode="numeric", length = nrow(mental))
mental = cbind(mental,temp)
for (i in 1:nrow(mental)){
  if ((mental[i,2]>1)|(mental[i,3]>1)|(mental[i,4]>1)|(mental[i,5]>1)|(mental[i,6]>1)|(mental[i,7]>1)|(mental[i,8]>1)|(mental[i,9]>1)|(mental[i,10]>1))
    mental[i,12]<-1
  else mental[i,12]<-0
}

mental = mental[,c(1,12)]

final <- merge(mental,data_all)
final2=final[-2]

set.seed(243523)
hm <- nrow(final)
data_pick <- sample(hm,0.7*hm)

training <- final[data_pick,]
testing <- final[-data_pick,]

mymodel=glm(temp~.,training, family = binomial(link = logit) )


l <- predict(mymodel,testing,type="response")

p <- prediction(l,testing$temp)

k <- performance(p,'auc')
k@y.values
# 
# library(zoo)
# 
# tempdir = cbind(final['INDFMPIR'],final['RIAGENDR'],final['DMDEDUC2'],final['DMDHHSZB'],final['DRDINT'])
# tempdir = data.matrix(tempdir)
# 
# matplot(tempdir, type = "l")
# 
# pre.period <- c(1,3500)
# post.period <- c(3501,5500)
# 
# impact <- CausalImpact(tempdir, pre.period, post.period)
# 
# plot(impact)

#confusion matrix
temp_p = unlist(p@predictions)
temp_l = unlist(p@labels)
prediction = as.integer(temp_p > 0.5)
temp_matrix = table(prediction,temp_l)
confusionMatrix(temp_matrix, positive = "1")

model <- keras_model_sequential()

model %>% 
  layer_dense(unit = 132,input_shape = c(132)) %>%
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 132) %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 132) %>%
  layer_dropout(rate = 0.2)
  layer_dense(units = 132) %>%
  layer_dropout(rate = 0.15)


summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

#temp = testing[,"SEQN"]
#temp = data.frame(SEQN = temp)
training_dataframe = training[0:1721,]
index <- as.numeric(row.names(training_dataframe))
training_dataframe = training_dataframe[order(index), ]

training_new = data.matrix(training_dataframe)
testing_new = data.matrix(testing)

#training_num = nrow(training_new)
#testing_num = nrow(testing_new)

history <- model %>% fit(
  training_new, testing_new,
  epochs = 30, batch_size = 200, 
  validation_split = 0.2
)

model %>% evaluate(training_new, testing_new)

model %>% predict_classes(testing_new)
