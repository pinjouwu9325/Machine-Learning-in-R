##Subsets & Shrinkage Regression (Stepwise & Lasso)

#Subsets regression
install.packages("lasso2")
library(lasso2)
data(Prostate, package = "lasso2")
str(Prostate)

#y: lpsa(Log Prostate Specific Antigen)
#training = 0.8, testing =0.2
set.seed(22)
train_ind <- sample(x=1: nrow(Prostate), size = ceiling(0.8*nrow(Prostate)))
train <- Prostate[train_ind, ]
test <- Prostate[-train_ind, ]

#Forward Stepwise Regression
#create null regression(only intercept)
null <- lm(lpsa ~ 1, data = train)
full <- lm(lpsa ~ ., data = train) #upper, the full regression

#Put each variable into the model: step()
#choose the one which AIC decreases the most and keep going
for_lm <- step(null, 
               #upper required!
               scope = list(lower=null, upper=full),
               direction = "forward")

#AIC smaller the better
summary(for_lm)

#Backward Stepwise Regression
#create a full regression
full <- lm(lpsa ~ ., data = train)

#Remove each variable one by one :step()
#remove the one which AIC decreases the most
back_lm <- step(full,
                #lower optional
                scope = list(upper=full),
                direction = "backward")
summary(back_lm)

#Both stepwise regression
both_1 <- step(null, scope = list(upper=full), direction = "both") 
both_2 <- step(full, scope = list(upper=full), direction = "both")

#Prediction
#self-defined R-squared function
R_sq <- function(actual, predict){
  mean_of_obs <-rep(mean(actual), length(actual))
  ss_tot <- sum((actual - mean_of_obs)^2)
  ss_reg <- sum((predict - mean_of_obs)^2)
  #ss_res <- sum((actual - predict)^2)
  R_sq <- ss_reg/ss_tot  # 1-(ss_res/ss_tot)
  R_sq
}

#predict()
lm_test <- predict(full, test)
for_test <- predict(for_lm, test)
back_test <- predict(back_lm, test)

c(R_sq(test$lpsa, lm_test),
  R_sq(test$lpsa, for_test),
  R_sq(test$lpsa, back_test))


##Shrinkage Method
install.packages("glmnet")
library(glmnet)


#glmnet(...
       #family = y is continuous>"gaussian", y is binomial>"binomial",
                #y is multinomial>"multinomial"
       #alpha = 0(Ridge), 1(Lasso)
       #lambda = penalty
ridge <- glmnet(x = as.matrix(train[, -9]),
                y = train[, 9],
                alpha = 0,
                family = "gaussian")

lasso <- glmnet(x = as.matrix(train[, -9]),
                y = train[, 9],
                alpha = 1,
                family = "gaussian")

par(mfcol = c(1, 2))
plot(ridge, xvar = "lambda", main = "Ridge")
plot(lasso, xvar = "lambda", main = "Lasso")
print(train[, -9])

#Find the best lambda: cv.glmnet()
#cv: cross validation
#using cv, estimate cvm(mean cross-validation error under different lambda)
cv_lasso <- cv.glmnet(x = as.matrix(train[, -9]),
                      y = train[, 9],
                      alpha = 1,
                      family = "gaussian")
best_lam <- cv_lasso$lambda.min
best_lam

#blue line indicate the best lambda value
plot(lasso, xvar = "lambda", main = "Lasso")
abline(v=log(best_lam), col="blue", lty=3)


#select the variables whose coefficients are not 0
coef(cv_lasso, s= "lambda.min")

select_ind <- which(coef(cv_lasso, s = "lambda.min") != 0)
select_ind <- select_ind[-1]-1 #remove intercept and move the rest index
select_ind

select_var <- colnames(train)[select_ind]
select_var

#lm again with with selected variables
#compare the coef of lasso and lm_again 
lm(lpsa ~ ., train[, c(select_var, "lpsa")])

#prediction
#glmnet() create Ridgr/Lasso model 
ridge <- glmnet(x = as.matrix(train[, -9]),
               y = trian[, 9],
               alpha = 0,
               family = "gaussian")

#cv.glmnet() to fin the best lambda
cv_ridge <- cv.glmnet(x = as.matrix(train[, -9]),
                      y = train[, 9],
                      alpha = 0,
                      family = "gaussian") 
best_ridge_lambda <- cv_ridge$lambda.min

#prediction
ridge_test <- predict(ridge, 
                      s = best_ridge_lambda,
                      newx = as.matrix(test[, -9]))

#validate model
R_sq(test$lpsa, ridge_test)
