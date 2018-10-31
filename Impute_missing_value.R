#Is there any missing value
tmp <- c(1, 5, 8, NA, 5, NA, 6)
is.na(tmp)

#Count the missing values
sum(is.na(tmp))
summary(is.na(tmp))

#Generate NA randomly in iris data
install.packages('missForest')
require(missForest)
data <- prodNA(iris, noNA = 0.1)
head(data)

##Method 1: Remove all the NA
#If an observation without NA return TRUE; if it contains any NA return FALSE
complete.cases(data)

#Remove all the obs. contains NA
rm_data <- data[complete.cases(data), ]
print(head(rm_data))

##Method 2: Impute mean value to the NA
mean_data <- data

#Mean of 1st column, na.rm=remove NA then mean using the remains
mean.1 <- mean(mean_data[,1], na.rm = T)

#Extract obs with 1st column which contains NA
na.rows <- is.na(mean_data[,1])
print(na.rows)

#Fill the NA with mean
mean_data[na.rows, 1] <- mean.1

##Method 3: K-Nearest Neighbours
install.packages("DMwR")
library(DMwR)

imputeData <- knnImputation(data)
head(imputeData)

##Method 4: MICE
install.packages("mice")
library(mice)

#m=3: generate 3 filled data table
#maxit: max iteration
#method="cart": use CART decision tree to predict NA
#seed=188: set.seed() make the same random data
mice_data <- mice(data, m = 3, maxit = 50,
                  method = "cart", seed = 188)
#3 filled data
complete(mice_data, 1)
complete(mice_data, 2)
complete(mice_data, 3)

#Take the 2nd to further analysis
df <- complete(mice_data, 2)
head(df)
