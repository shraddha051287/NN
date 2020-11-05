startups<- read.csv(file.choose())
library(plyr)
View(startups)
str(startups)
summary(startups)
dim(is.na(startups))
#attach(startups)
Administration
#normal_concrete<-scale(foresfires_1)
## or 

startups$State=as.factor(startups$State)
str(startups)
startups$State

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))}
  

startups_norm<-as.data.frame(lapply(startups[,-4],FUN=normalize))
startups_norm<- startups_norm[,-4]
startups_norm
#summary(startups_norm$profit)
summary(startups_norm)
summary(startups$area)

startups_norm <- cbind(startups_norm,startups$Profit)

#How to combine column into dataset
#colnames(startups_norm)[5] <- "profit"
startups_train<-startups_norm[1:35,]
startups_test<-startups_norm[36:50,]

allVars <- colnames(startups_norm)
predictorVars <- allVars[!allVars%in%'target']
predictorVars <- paste(predictorVars, collapse = "+")
f <- as.formula(paste("startups$Profit~", predictorVars, collapse = "+"))

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
formula_nn <- paste("startups$Profit",paste(colnames(startups[,-4]),collapse ="+"),sep="~")
startups_model <- neuralnet(startups$Profit~R.D.Spend+Administration+Marketing.Spend,data = startups_train)
startups_model <- neuralnet(formula = f,linear.output = F,data = startups_train)
str(startups_model)
plot(startups_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(startups_model,startups_test)
str(model_results)
predicted_profit<- model_results$net.result
# predicted_profit
# model_results$neurons
cor(predicted_profit,startups_test$profit)
plot(predicted_profit,startups_test$profit)
model_5<-neuralnet(startups$Profit~R.D.Spend+Administration+Marketing.Spend,data= startups_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,startups_test[1:3])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,stratups_test$profit)
plot(pred_strn_5,startups_test$profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased

