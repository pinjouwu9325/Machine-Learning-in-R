#SVM
install.packages("e1071")
install.packages("mlbench")
library(e1071)
library(mlbench)
data(Glass, package = "mlbench")
data <- Glass

#training 80%, testing 20%
smp_size <- floor(0.8*nrow(data))
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), smp_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]

#training
model <- svm(formula = Type ~., #y=Type and data format has to be "Factor"
             data = train)
summary(model)

#prediction
train_pre <- predict(model, train)
test_pre <- predict(model, test)

#confusion matrix: training data
table(real=train$Type, predict=train_pre)
confus_matrix <- table(real=train$Type, predict=train_pre)
sum(diag(confus_matrix))/sum(confus_matrix)

#confision matrix: testing data
confus_matrix <- table(real=test$Type, predict=test_pre)
sum(diag(confus_matrix))/sum(confus_matrix)

##!!SVM is a binary classifier!!
#But we do have 6 classes!? how?

##SVM and multi-class
#One-against-rest
#One-against-one
#here, e1071 svm() is one-against-one

##Support Vector Regression(SVR)
#in this package e1071, svr() function is in svm()
#if y is factor, svm() -> svm
#if y is numeric, svm() -> svr

#Given example
data <- data.frame(x=1:20,
                   y=c(3,4,8,2,6,10,12,13,15,14,17,18,20,17,21,22,25,30,29,31))
plot(data$x, data$y, pch=19, xlab = "x", ylab = "y" )

#linear regression
model <- lm(y~x, data)
summary(model)
lm_pre <- predict(model, data)
points(lm_pre, col="red")
abline(model, col="red")

#SVM
model_s <- svm(y ~ x, data) #here, y is numeric
svr_pre <- predict(model_s, data)
plot(data$x, data$y, pch=16, xlab = "x", ylab = "y" )
points(svr_pre, col="dark green")

#Compare lm and svr
#origional data
plot(data$x, data$y, pch=16, xlab = "x", ylab = "y" )
#lm prediction
points(lm_pre, pch=2, col="orange")
#svr prediction
points(svr_pre, pch=4, col="blue")

#(lm, svr) in RMSE (root mean squared error)
c(sqrt(mean((data$y - lm_pre)^2)),
  sqrt(mean((data$y - svr_pre)^2))
)
#svr is better than lm (1.79 < 1.91)


##Parameters discussion
#SVM(...
#    type = decide to use classification(cata) or regression(numeric),
#    scale = regularization the data to (mean, std)=(0, 1),
#    kernal = function to reflect the data to the feature space, use to deal with "非線性可分"的問題,
#    cost = C in Lagrange formulation, 決定給被誤差/分錯的資料"多少"懲罰值,
#    epsilon = margin of tolerance, if bigger, 表示在容忍範圍內被分錯的資料，不會被懲罰,
#    反之越靠近0, 更多的分錯資料被懲罰,
#    gamma = parameter inside kernal function,
#    ...
#    )

#C(cost)
#藉由C，可以給予被分錯的資料懲罰值，控制support vectors的影響
#support vectors: 用來決定超平面的資料點
#bigger c => 懲罰值越大容錯越小 => less support vectors => close to hard-margin SVM, easy to overfitting
#smaller C => 懲罰值越小容錯越大 => more support vectors => bigger mirgin

#when c get higher, count the number of support vectors
num_SV <- sapply(X=1:1000,
                 FUN=function(C) svm(Type~., data, cost=C, epsilon =.1)$tot.nSV)
plot(x=1:1000, y=num_SV, xlab="C value", ylab = "# of support vectors", pch=16, cex=.5, main="# of SVs in soft-margin SVM")

#Epsilon(0~1)
#influence SVR because n loss function in SVR, it used epsilon intensive hinge loss
#epsilon create a width zone that the data in this zone will be ignored
#bigger epsilon => bigger zone => more data ignored => lower accuracy => less support vectors
#smaller epsilon => smaller zone => more data(error) considered => easy to overfit

df <- data.frame(x=1:20,
                 y=c(3,4,8,2,6,10,12,13,15,14,17,18,20,17,21,22,25,30,29,31))

#when epsilon get bigger, count the number of support vectors(SVR)
num_SV_E <- sapply(X=seq(0, 1, 0.01),
                   FUN=function(e) svm(y~x, df, cost=1, epsilon = e)$tot.nSV)
plot(x=seq(0, 1, 0.01), y=num_SV_E, xlab = "epsilon value", ylab = "# of support vectors", pch=15, cex=.5, main="# of SVs in SVR")

#when epsilon get biggerm, RMSE?
RMSE <- sapply(X=seq(0,1,0.01),
              FUN=function(e) sqrt(mean((svm(y~x, df, cost=1, epsilon = e)$residuals)^2)))
plot(x=seq(0, 1, 0.01), y=RMSE, xlab = "epsilon value", ylab = "RMSE", pch=16, cex=.5, main = "RMSE in SVR")


#Gamma: How far the influence of a single training example reaches
#bigger gamma => 資料點的影響力範圍比較近，對超平面來說，近點的影響力權重較大，容易勾勒出擬合近點的超平面，也容易造成overfitting
#smaller gamma => 資料點的影響力範圍比較遠，對超平面來說，較遠的資料點也有影響力，因此能勾勒出平滑、近似直線的超平面

#when gamma get bigger, the trainging data accuracy
train_a <- sapply(X=seq(0.1, 10, 0.1),
                  FUN=function(g){
                    model = svm(Type~., train, gamma = g, epsilon = .1)
                    pred = predict(model, train)
                    confus_matrix = table(real=train$Type, predict=pred)
                    sum(diag(confus_matrix))/sum(confus_matrix)
                  }
)

#when gamma get bigger, the testing data accuracy
test_a <- sapply(X=seq(0.1, 10, 0.1),
                  FUN=function(g){
                    model = svm(Type~., train, gamma = g, epsilon = .1)
                    pred = predict(model, test)
                    confus_matrix = table(real=test$Type, predict=pred)
                    sum(diag(confus_matrix))/sum(confus_matrix)
                  }
)                  


#train accuracy = red 
plot(x = seq(0.1, 10, 0.1), y = train_a, pch=16, cex=.5, col="orange", ylim = c(0,1), 
     xlab = "gamma value", ylab="Class Accuracy", main="Accuracy in soft-margin SVM")

points(x = seq(0.1, 10, 0.1), y = test_a, pch=16, cex=.5, col="blue")

legend("bottomright", pch = 16, col = c("orange","blue"),legend=c("Train-Accuracy", "Test-Accuracy"))
#when gamma get bigger, accuracy of trainng data is higher, but accuracy of testing data is lower => it's overfitting

##Tune Parameters
#tune() in e1071 package
#Tune parameters in SVM(soft-magrin)
#in SVM, we are used to tune (cost, gamma)
tune_model <- tune(svm,
                   Type~.,
                   data = data,
                   kernel="radial", #RBF kernel function
                   range = list(cost=10^(-1:2), gamma=c(.5,1,2))
                   )
#cost = 10^(-1, 0, 1, 2); gamma = 0.5, 1, 2
#4*3 = 12 models

summary(tune_model)
#error = classification error
plot(tune_model)
#the darker, the lowere classification error
#here, cost=10, gamma=0.5 is the best
#Best model in set of tuning models
tune_model$best.model

#Tune parameters in SVR
#we used to tune (cost, epsilon)
tune_model <- tune(svm,
                   y~x,
                   data = df,
                   range = list(cost=2^(2:9), epsilon = seq(0,1,0.1))
)
#cost = 2^2, 2#3, ..., 2^9; epsilon = 0, 0.1, 0.2, ..., 1
#8*11=88 models
#the value is mean square error
tune_model
plot(tune_model)
#Best model in set of tuning models
tune_model$best.model
