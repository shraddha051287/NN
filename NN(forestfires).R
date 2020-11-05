forestfires <- read.csv(file.choose())
View(forestfires)
colnames(forestfires)
forestfires_1<-forestfires[c("FFMC","DMC","DC","ISI","temp","RH","wind","rain","area","size_category")]
View(forestfires_1)
forestfires_1$size_category=as.factor(forestfires_1$size_category)
str(forestfires_1)
summary(forestfires_1$size_category)
colnames(forestfires_1)[9] <- "area"

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 


forestfires_1_train<-forestfires_1[1:400,]
forestfires_1_test<-forestfires_1[401:517,]

# Building model
formula_nn <- paste("area",paste(colnames(forestfires_1[9,]),collapse ="+"),sep="~")
forestfires_1_model <- neuralnet(size_category ~.,data = forestfires_1_train)
forestfires_1_model <- neuralnet(formula = formula_nn,data = forestfires_1_train)
str(forestfires_1_model)
plot(forestfires_1_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(forestfires_1_model,forestfires_1_test[1:9])
str(model_results)
predicted_area <- model_results$net.result
# predicted_area
# model_results$neurons
cor(predicted_area,forestfires_1_test$area)
cor(predicted_area,forestfires_1_test$area)
plot(predicted_area,forestfires_1_test$area)
model_5<-neuralnet(area~.,data= forestfires_1,hidden = 5)
plot(model_5)
model_5_res<-compute(model_forestfires_1_test[1:11])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,forestfires_1_test$area)
plot(pred_strn_5,forestfires_1_test$area)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased

