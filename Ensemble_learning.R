##Ensemble Learning
#Bagging(Bootstrap aggregating)
#Boosting
#Stacking

data(Prostate, package="lasso2")
str(Prostate)
set.seed(22)
train_ind <- sample(x=1:nrow(Prostate), size = floor(0.8*nrow(Prostate)))
train <- Prostate[train_ind, ]
test <- Prostate[-train_ind, ]

##Bagging: Bootstrap aggregation
#used when model is complex and easy overfit ex. decision tree
#help decrease variance
#Fisrt, data重複抽樣
#here, we create 3 subsets(n=33)

#Subset-1
set.seed(1)
ind_1 <- sample(x=1:nrow(train), 33)
subset_1 <- train[ind_1, ]

#Subset-2
set.seed(2)
ind_2 <- sample(x=1:nrow(train), 33)
subset_2 <- train[ind_2, ]

#Subset-3
set.seed(3)
ind_3 <- sample(x=1:nrow(train), 33)
subset_3 <- train[ind_3, ]

#Model-1:linear regression
model_1 <- lm(lpsa~., subset_1)
y1 <- predict(model_1, test)

#Model-2:linear regression
model_2 <- lm(lpsa~., subset_2)
y2 <- predict(model_2, test)

#model-3:linear regression
model_3 <- lm(lpsa~., subset_3)
y3 <- predict(model_3, test)

#Average Prediction Results (bagging)
ave_y <- (y1 + y2 + y3)/3

#Because lpsa is continuous value, we use MSE to determined the preformance of model 1,2,3 and model(after bagging)
#MSE comparison between three models and the bagging model
c(mean((y1 - test$lpsa)^2), # lm of subset1
  mean((y2 - test$lpsa)^2), # lm of subset2
  mean((y3 - test$lpsa)^2), # lm of subset3
  mean((ave_y - test$lpsa)^2)) #bagging model

#Random Forest (bagging + CART)
library(randomForest)
rf_model <- randomForest(lpsa~.,
                         data = train,
                         ntree = 150 #how many decision trees?
                         )
#Predicion
rf_y <- predict(rf_model, test)
mean((rf_y - test$lpsa)^2)

#Observe the best number of decision trees
plot(rf_model)
# 100 looks good

#tune mtry(每次抽樣需要抽多少個變數): use tuneRF()
tuneRF(train[, -9], train[, 9])
# mtry = 4 is the best

#rewrite the random forest model
rf_model <- randomForest(lpsa~.,
                         data = train,
                         ntree = 100, 
                         mtry = 4
                         )
rf_y <- predict(rf_model, test)
mean((rf_y - test$lpsa)^2)


##Boosting
#weak model
#decrease bias
#due to the practise data here is not classificatio problem
#here, we introduce Gradient Boosting Machine(XGboost)

#Gradient Boosting Machine(XGboost) = Gradient descending + Booosting
install.packages("xgboost")
library(xgboost)

#transfer data format from data.frame to xgboost(稀疏矩陣): use xgb.DMatrix()
dtrain <- xgb.DMatrix(data = as.matrix(train[, 1:8]),
                      label = train$lpsa)
dtest <- xgb.DMatrix(data = as.matrix(test[, 1:8]),
                     label = test$lpsa) 

#set parameters: xgb.params
xgb.params <- list(
  #col的抽樣比例, the bigger, the more col used and will increase complexity of each tree
  colsample_bytree = 0.5,
  #row的抽樣比例, the biger, the more row used and will increase complexity of each tree
  subsample = 0.5,
  booster = "gbtree",
  #depth of the tree, the bigger, the model will get deeper and more complex
  max_depth = 2,
  #boosting會增加被分錯的資料權重，此參數是讓權重不增加得那麼快，因此越大會讓模型越保守
  eta = 0.03,
  eval_metric = "rmse",
  objective = "reg:linear",
  #gamma get bigger, the model will be more conserved
  gamma = 0
)

#find the best number of decision tree "nrounds": use xgb.cv()
cv_model <- xgb.cv(
  params = xgb.params,
  data = dtrain,
  nfold = 5, #5-fold cv
  nrounds = 200,
  early_stopping_rounds = 30, #when nrounds <30, overfitting occur, then we can stop
  print_every_n = 20 #evert 20 units then show the result
)

tmp <- cv_model$evaluation_log
plot(x=1:nrow(tmp), y = tmp$train_rmse_mean, col = "orange", xlab = "nround", ylab = "rmse", main = "Avg.Performance in CV")
points(x=1:nrow(tmp), y = tmp$test_rmse_mean, col = "blue")
legend("topright", pch = 1, col=c("orange", "blue"),
       legend = c("Train", "Validation"))
#一般來說，Train的表現會比Validaiton還要好，這時候需要思考兩種情況：
# 1. if Train and Validation are close => this model can be trained even better
#    we can tune these parameters:
#    max_depth: up for 1 unit (reconmmend tune this)
#    colsample_bytree, subsample: up the ratio (this is good to tune too)
#    eta: go down (if you have no choise)
#    gamma: go sown (if you have no choise)
# 2. if Train is far away better than Validation => overfitting!!
#    the parameters above have to go opposite to decrease the complexity of model

#Best nround
best_n <- cv_model$best_iteration
best_n

#use xgb.train() to create a model
x_model <- xgb.train(paras = xgb.params,
                     data = dtrain,
                     nrounds = best_n) 
#plot (a lot!)
#install.packages("DiagrammeR")
#xgb.plot.tree(model = x_model, col="blue")

xgb_y <- predict(x_model, dtest)
mean((xgb_y - test$lpsa)^2)


##Stacking
# 1. stacking: train many models(lm, SVR, CART...etc), and the predict results are called Meta-Data. The Mets-Data will be the inputs of final model(Meta-Model; Blender) 
# 2. blending: final model(Meta-Model) will get the Meta-Data for trainning and then do the final Predicted Results

#1. stacking
#3-folds
n <- 3
n_fold <- rep(1:n, each=nrow(train)/n)
train_folds <- split(train, n_fold)

#linear regression:model 1
meta_x <- vector()
meta_y <- list()

#1st fold for validation
stack_train <- rbind(train_folds[[2]], train_folds[[3]]) #[[]] expand the dataframe
stack_valid <- train_folds[[1]]
stack_test <- test

model_1 <- lm(lpsa~., stack_train)

tmp_metax <- predict(model_1, stack_valid)
tmp_metay <- predict(model_1, stack_test)

meta_x <- c(meta_x, tmp_metax)
meta_y[[1]] <- tmp_metay

#2nd fold for validation
stack_train <- rbind(train_folds[[1]], train_folds[[3]]) #[[]] expand the dataframe
stack_valid <- train_folds[[2]]
stack_test <- test

model_1 <- lm(lpsa~., stack_train)

tmp_metax <- predict(model_1, stack_valid)
tmp_metay <- predict(model_1, stack_test)

meta_x <- c(meta_x, tmp_metax)
meta_y[[2]] <- tmp_metay

#3rd fold for validation
stack_train <- rbind(train_folds[[1]], train_folds[[2]]) #[[]] expand the dataframe
stack_valid <- train_folds[[3]]
stack_test <- test

model_1 <- lm(lpsa~., stack_train)

tmp_metax <- predict(model_1, stack_valid)
tmp_metay <- predict(model_1, stack_test)

meta_x <- c(meta_x, tmp_metax)
meta_y[[3]] <- tmp_metay

#thought there is only model_1 but each fold trained a new model of lm. It just replaced the previous one.

#Average Meta.X of Test
mean_meta_y <- (meta_y[[1]] + meta_y[[2]] + meta_y[[3]])/3
meta_train_1 <- data.frame(meta_x = meta_x,
                           y = train$lpsa)
meta_test_1 <- data.frame(meta_y = mean_meta_y, 
                          y = test$lpsa)
#SVM: model 2
library(e1071)
meta_x <- vector()
meta_y <- list()

#1st fold for validation
stack_train <- rbind(train_folds[[2]], train_folds[[3]])
stack_valid <- train_folds[[1]]
stack_test <- test

model_2 <- svm(lpsa~., stack_train)

tmp_metax <- predict(model_2, stack_valid)
tmp_metay <- predict(model_2, stack_test)

meta_x <- c(meta_x, tmp_metax)
meta_y[[1]] <- tmp_metay

#2nd fold for validation
stack_train <- rbind(train_folds[[1]], train_folds[[3]])
stack_valid <- train_folds[[2]]
stack_test <- test

model_2 <- svm(lpsa~., stack_train)

tmp_metax <- predict(model_2, stack_valid)
tmp_metay <- predict(model_2, stack_test)

meta_x <- c(meta_x, tmp_metax)
meta_y[[2]] <- tmp_metay

#3rd fold for validation
stack_train <- rbind(train_folds[[1]], train_folds[[2]])
stack_valid <- train_folds[[3]]
stack_test <- test

model_2 <- svm(lpsa~., stack_train)

tmp_metax <- predict(model_2, stack_valid)
tmp_metay <- predict(model_2, stack_test)

meta_x <- c(meta_x, tmp_metax)
meta_y[[3]] <- tmp_metay

#Average of Meta.X of Test
mean_meta_y <- (meta_y[[1]] + meta_y[[2]] + meta_y[[3]]) / 3
meta_train_2 <- data.frame(meta_x = meta_x, y = train$lpsa)
meta_test_2 <- data.frame(meta_y = mean_meta_y, y = test$lpsa)


#CART : model 3
library(rpart)
meta_x <- vector()
meta_y <- list()

#1st fold for validation
stack_train <- rbind(train_folds[[2]], train_folds[[3]])
stack_valid <- train_folds[[1]]
stack_test <- test

model_3 <- rpart(lpsa~., stack_train)

tmp_metax <- predict(model_3, stack_valid)
tmp_metay <- predict(model_3, stack_test)

meta_x <- c(meta_x, tmp_metax)
meta_y[[1]] <- tmp_metay 

#2nd fold for validation
stack_train <- rbind(train_folds[[1]], train_folds[[3]])
stack_valid <- train_folds[[2]]
stack_test <- test

model_3 <- rpart(lpsa~., stack_train)

tmp_metax <- predict(model_3, stack_valid)
tmp_metay <- predict(model_3, stack_test)

meta_x <- c(meta_x, tmp_metax)
meta_y[[2]] <- tmp_metay 

#3rd fold for validation
stack_train <- rbind(train_folds[[1]], train_folds[[2]])
stack_valid <- train_folds[[3]]
stack_test <- test

model_3 <- rpart(lpsa~., stack_train)

tmp_metax <- predict(model_3, stack_valid)
tmp_metay <- predict(model_3, stack_test)

meta_x <- c(meta_x, tmp_metax)
meta_y[[3]] <- tmp_metay 

#Average of Meta.X of Test
mean_meta_y <- (meta_y[[1]] + meta_y[[2]] + meta_y[[3]])/3
meta_train_3 <- data.frame(meta_x = meta_x, y = train$lpsa)
meta_test_3 <- data.frame(meta_y = mean_meta_y, y = test$lpsa)


##Blending
#Now we have three Meta-Train and three Meta-Test:
c(dim(meta_train_1), dim(meta_test_1))

#Meta-Model construction
#here we use xgboost
#combind three Meta-Train
library(xgboost)
C_meta_train <- rbind(meta_train_1, meta_train_2, meta_train_3)

#transfer to xgboost format
dtrain <- xgb.DMatrix(data = as.matrix(C_meta_train[, 1]), label = C_meta_train[, 2])

#train XGboost model
#xgb.paras are from previous session 2
#simply set nrounds = 100
xgb_model <- xgb.train(paras = xgb.params, data = dtrain, nrounds = 100)

#predict by three Meta-Test
dtest1 <- xgb.DMatrix(data = as.matrix(meta_test_1[, 1], label = meta_test_1[, 2]))
final_1 <- predict(xgb_model, dtest1)

dtest2 <- xgb.DMatrix(data = as.matrix(meta_test_2[, 1], label = meta_test_2[, 2]))
final_2 <- predict(xgb_model, dtest2)

dtest3 <- xgb.DMatrix(data = as.matrix(meta_test_3[, 1], label = meta_test_3[, 2]))
final_3 <- predict(xgb_model, dtest3)

#Mean the three result and calculate MSE
final_y <- (final_1 + final_2 + final_3)/3
mean((final_y - test$lpsa)^2)
