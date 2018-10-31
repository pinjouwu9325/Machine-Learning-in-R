#Backpropagation Neural Network
setwd("D:/Chen Lab/Programme file/R")

#Packages
install.packages("neuralnet")
install.packages("nnet")
install.packages("caret")
library(neuralnet)
library(nnet)
library(caret)

#import data
data <- iris
#input node: Sepal.Length...
#output node: Species
#Because Species is categorical variable, it has to be transfered to dummy variable
#use class.ind() to transfer Species to three output nodes
head(class.ind(data$Species))

#Combine the transfered data to the original data
data <- cbind(data, class.ind(data$Species))
head(data)

#create a formula
formula.bpn <- setosa + versicolor + virginica ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width

#train bpn model:neuralnet()
bpn <- neuralnet(formula = formula.bpn,
                 data = data,
                 hidden = c(2), #one hidden layer: 2 nodes
                 learningrate = 0.01, 
                 threshold = 0.01, #partial derivatives of the error function, a stopping criteria
                 stepmax = 5e5) # max iteration = 5*10^5
plot(bpn)


##Tuning Parameters
#MSE(RMSE) => mini => optimal parameters
#use caret to tune

#traing data:80%, testing data:20%
#nrow() grep the obs. 
grep_size <- floor(0.8*nrow(data))

#set seed
set.seed(130)

#grep training data
train_data <- sample(seq_len(nrow(data)), grep_size)
train <- data[train_data, ]
test <- data[-train_data, ]

#tune parameter
model <- train(form=formula.bpn,
               data=train,
               method="neuralnet",
               #!! 觀察不同排列組合(第一層1-4個nodes; 第二層0-4個nodes)
               #to see which combination represent the minimun RMSE
               tuneGrid = expand.grid(.layer1=c(1:4), .layer2=c(0:4), .layer3=c(0)),
               learningrate = 0.01,
               threshold = 0.01,
               stepmax = 5e5
               )
model
plot(model)

#use 2 hidden layers(1 node, 3 nodes) to train NN again
bpn <- neuralnet(formula = formula.bpn,
                 data = train,
                 hidden = c(1,3),
                 learningrate = 0.01,
                 threshold = 0.01,
                 stepmax = 5e5)
plot(bpn)


#Prediciton
#use bpn model and input test set to predict
#!! input testing data contains input node only
#select the first to the fourth into the model
pred <- compute(bpn, test[, 1:4])
pred$net.result

#round the data
pred_result <- round(pred$net.result)
pred_result

#transfer the data to data frame
pred_result <- as.data.frame(pred_result)

#create a new column: Species
pred_result$Species <- ""

#put the predict result back as Species catagories
for( i in 1:nrow(pred_result)){
  if(pred_result[i, 1]==1){ pred_result[i, "Species"] <- "setosa"}
  if(pred_result[i, 2]==1){ pred_result[i, "Species"] <- "versicolor"}
  if(pred_result[i, 3]==1){ pred_result[i, "Species"] <- "virginica"}
}

pred_result  

#accuracy
table(real = test$Species,
      predict = pred_result$Species)
