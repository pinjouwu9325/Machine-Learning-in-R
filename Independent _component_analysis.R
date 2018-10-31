##Independent Component Analysis(ICA)
#Dimension Reduction
#Extract features from data

#simulation
#two original sounds(source, feature)
#two collected data(obseration)

#create two sources
S <- cbind(sin((1:1000)/20),
           rep((((1:200)-100)/100), 5)
)
par(mfcol = c(1, 2))
plot(1:1000, S[, 1], type = "l", xlab = "S1", ylab = "") # sin
plot(1:1000, S[, 2], type = "l", xlab = "S2", ylab = "")

#Mixing Matrix
A <- matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)
A

#New signal(Observation)
X <- S %*% A
par(mfcol = c(1, 2))
plot(1:1000, X[, 1], type = "l", xlab = "X1", ylab = "") # sin
plot(1:1000, X[, 2], type = "l", xlab = "X2", ylab = "")

#ICA
#FastICA-Algorithm
#need to follow: 
# 1. sourse and observation has to be independent
# 2. source and observation cannot be normal distribution
install.packages("fastICA")
library(fastICA)

#ICA for extrating independent sources from mixed signals
ICA_result <- fastICA(X, n.comp = 2)
S1 <- ICA_result$S[, 1]
S2 <- ICA_result$S[, 2]

par(mfcol = c(1, 2))
plot(1:1000, S1, type = "l", xlab = "S'1", ylab = "") 
plot(1:1000, S2, type = "l", xlab = "S'2", ylab = "")

#similar to PCA, we have to decide how many original sources(n.comp)
