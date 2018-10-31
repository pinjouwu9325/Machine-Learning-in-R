#Simple regression
library(datasets)
str(iris)
head(iris, n=6)
summary(iris)

##Plotting:
#Base Plotting System, pch= shape of dots
plot(x=iris$Sepal.Length, y=iris$Sepal.Width, pch=1)

#Lattice
library(lattice)
xyplot(Sepal.Width~Sepal.Length, data=iris)

#ggplot2
install.packages("ggplot2")
library(ggplot2)
ggplot(data=iris)+ #prepare the data
  geom_point(aes(x=Sepal.Length, y=Sepal.Width))+ #scatterplot
  theme_bw()  #make the background white

ggplot(data = iris)+ 
  geom_point(aes(x=Petal.Length, y=Petal.Width))+
  theme_bw()

##There are two clusters. It may be because of the species
##Spereate the species with different colors to check
##Label the obs with different colors to indicate the species
#Base Plotting system
plot(x=iris$Petal.Length, y=iris$Petal.Width, pch=16)
d1 <- iris[iris$Species=="versicolor", ]
points(x=d1$Petal.Length, y=d1$Petal.Width, pch=16, col="green")
d2 <- iris[iris$Species=="setosa", ]
points(x=d2$Petal.Length, y=d2$Petal.Width, pch=16, col="red")
d3 <- iris[iris$Species=="virginica", ]
points(x=d3$Petal.Length, y=d3$Petal.Width, pch=16, col="blue")

legend("topleft", pch=16,
       legend=c("setosa", "versicolor","virginica"), col=c("red", "green", "blue"))


#Lattice
xyplot(Petal.Width~Petal.Length,
       data=iris,
       pch=16,
       group=Species,
       auto.key = list(space="top", columns=3, cex.title=1, title="Species Labels",
                       pch=16))

#ggplot2
ggplot(data = iris)+
  geom_point(aes(x=Petal.Length, y=Petal.Width, color=Species))+
  theme_bw()


##Boxplot:
#Base Plotting system
boxplot(Petal.Length~Species, data=iris, xlab="Species", ylab="Petal.Length")
boxplot(Petal.Width~Species, data=iris, xlab="Species", ylab="Petal.Width")

#Lattice
bwplot(x=Petal.Length~Petal.Width | Species, data = iris)

#ggplot2
qplot(x=Petal.Length, y=Petal.Width, data = iris, geom = "boxplot",  #graph type is boxplot
      color=Species)+theme_bw()


##Check if NA in data
table(is.na(iris))

##Regression
##lm() (Linear Model): model =lm(Y~X1+X2+...Xk, data=...)
model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
            data = iris)
summary(model)
#Sepal.Length = 1.85600 + 0.65084xSepal.Width + 0.70913xPetal.Length - 0.55648xPetal.Width
#p-values show that all (X) are significant to Y
#R-squared:  0.8586; Adjusted R-squared:  0.8557 => not bad!
#Residual standard error: 0.3145

#When we create a model, we have to check if the residual follow the hypothesis of:
#1. Normality
#2. Independent
#3. Homogeneity of Variance
names(model)
model$residual

#check residual:Normality
shapiro.test(model$residual)
#H0: residual follow normality distribution
#Because p-vlaue > 0.05 =>do not reject H0

#check residual:Idependent
install.packages("car")
library(car)
#Function durbinWatsonTest will extract residual automatically, so just put model as input
durbinWatsonTest(model)
#H0:residuals are independent
#p-value > 0.05 =>do not reject H0

#check residual: Homogeneity of Variance
ncvTest(model)
#H0:residuals are homogeneity of Variance
#p-value <0.05 => reject H0 => this model cant be used


##Prediction
new_iris <- data.frame(Sepal.Width=3.456, Petal.Length=1.535, Petal.Width=0.341)
new_iris

predict(model, new_iris)


##Analysis of Variance(ANOVA)
#Due to the data visualization, 
#we found that the mean of Petal.Width(Petal.Length) is different between species 
#to check the difference is real, use ANOVA!
#H0:mean(Setosa) = mean(Versicolor) = mean(Virginica)
#H1:at least one mean is different with others

#creat the linear model first
w_lm <- lm(Petal.Width~Species, data = iris)
anova(w_lm)
summary(w_lm)
plot(w_lm)

l_lm <- lm(Petal.Length~Species, data = iris)
anova(l_lm)
#Both p-value < 0.05 =>there are significance between species

