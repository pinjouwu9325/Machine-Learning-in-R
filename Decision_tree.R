##Association Rules
setwd("D:\\Chen Lab\\Programme file\\R")

#Load data
load("titanic.raw.rdata")
table(is.na(titanic.raw))
str(titanic.raw)

#Creat association rules
#min support: "規則rules"在資料內具有普遍性，A和B同時出現的機率有多少
#min confidence: "規則"要有一定的信心水準，購買A的狀況下，也會購買B條件機率

install.packages("arules")
library(arules)

#Here, we suggest: under A circumstance, if the passenger survive
#apriori rules with right hand side(rhs) containing "Survived" only
rule <- apriori(titanic.raw,
                #min support and confidence, 最小規則長度(lhs+rhs)
                parameter = list(minlen=3, supp=0.1, conf=0.7),
                appearance = list(default="lhs", 
                                  #features showed at right hand side
                                  rhs=c("Survived=No", "Survived=Yes")))
inspect(rule)
?apriori

#Sorting by lift
sort_rule <- sort(rule, by="lift")
inspect(sort_rule)
#lift=2.3 > 1 =>positive related
#but the support value was quite low may be due to less female on board

#redundant rule remove
# no.5 lift = no.6 lift => the no.6 rule is redundant
#check if that rule is a subset of the others rules
#sort by support
sort_rule_s <- sort(rule, by="support")
inspect(sort_rule_s)
subset_matirx <- as.matrix(is.subset(x=sort_rule_s, y=sort_rule_s))

#leave the upper right info
subset_matirx[lower.tri(subset_matirx, diag=T)] <- NA

#count the TRUE, if there is one or more TRUE, which means that that column is redundent
redundent <- colSums(subset_matirx, na.rm=T) >= 1

#remove the redundent
sort_rule_s <- sort_rule_s[!redundent]
inspect(sort_rule_s)

#visualization
install.packages("arulesViz")
library(arulesViz)
plot(sort_rule_s)

plot(sort_rule_s, method = "graph")
plot(sort_rule_s, method = "grouped")


##Decision Tree
#CART
library(rpart)

#training data = 0.8, testing = 0.2
set.seed(22)
train_index <- sample(x=1:nrow(titanic.raw), size = ceiling(0.8*nrow(titanic.raw)))
train <- titanic.raw[train_index, ]
test <- titanic.raw[train_index, ]
?ceiling
?sample
?set.seed

#CART model: Y = Survived, X = other variables
cart_model <- rpart(Survived~.,
                    data = train)

#visualization
install.packages("rpart.plot")
library(rpart.plot)
prp(cart_model,
    faclen = 0, #呈現變數不要縮寫
    fallen.leaves = TRUE, #讓樹枝呈垂直方式呈現
    shadow.col = "gray", #最下面的node塗上陰影
    extra = 2)  #no. of correct classifiation/ no. of obs.  inthat node

#prediction
pred <- predict(cart_model, newdata=test, type="class")
table(real=test$Survived, predict=pred)

#calculate accuracy = diag num/ total num
confus_matrix <- table(real=test$Survived, predict=pred)
sum(diag(confus_matrix))/sum(confus_matrix)
