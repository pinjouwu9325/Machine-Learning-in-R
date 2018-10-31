#Laod data
data <- read.csv("2012MLB.csv", header = T, sep=',')
head(data)

#PCA
pca_data <- prcomp(formula = ~H1B+H2B+H3B+HR+RBI+SB+BB, #select vairable, now is 7
                   data = data, #your data
                   scale = T) #Regularized data正規劃資料
pca_data
#Standard deviations:特徵值開根號
#Rotation: 特徵向量，各個主成分所對應的線性組合(linear combination)係數

#How many PCAs do we need?
#Ask Scree plot and Pareto plot for help

##Scree plot
plot(pca_data,  #PCA data here
     type="line",  #create line plot
     main = "Scree Plot for 2012MLB")  #main title

#red line indicate the vairance特徵值=1 
#Kaiser eigenvalue-greater-than-one rule,
#depends on this rule, we could pick the PCA which is greater than one 
abline(h=1, col="red")
#here, we select the first to the third PCA

##Pareto plot
#計算特徵值variance(=srd^2)
vars <- (pca_data$sdev)^2
vars

#計算每個主成分的解釋比例(各個主成分的特徵值variance/總特徵值total variance) 
props <- vars/sum(vars)
props

#aggregated effects累加每個主成分的解釋比例 
#cumcum:Cumulative Sums
cumlative_props <- cumsum(props)
cumlative_props

#plot the aggregated effects
plot(cumlative_props)
cumlative_props[3] #when we select the first to the third PC, it could explain 70.64% variance

#After PCA, the original data will be transfer to the new dataset replaced by PC
#select the first to the third PC and replace them in to the original data, making a new dataset
#pca$rotation
top3_pca_data <- pca_data$x[, 1:3] #PC1-PC3
top3_pca_data


##主成分負荷PC loading:主成分PC和原變數的關係
#Each PC都是原變數經過線性組合後產生的值
#要解釋PC：觀察PC和原變數間的關係，也就是觀察原變數在線性組合中的係數(特徵向量)
#看原變數對PC究竟是正面還是負面、有多大的影響

#特徵向量(原變數的線性組合)
pca_data$rotation

#Select the first to the third
top3_pca_eigenvector <- pca_data$rotation[, 1:3]
top3_pca_eigenvector

#plot loading plot for PCs 觀察原變數與PC之間的關係
first_pca <- top3_pca_eigenvector[, 1]
second_pca <- top3_pca_eigenvector[, 2]
third_pca <- top3_pca_eigenvector[, 3]

#PC1
#SB(盜壘)、BB(四壞)與PC1呈正相關，看起來看和"上壘"有關
first_pca[order(first_pca, decreasing = F)]
dotchart(first_pca[order(first_pca, decreasing = F)], 
         main = "Loading Plot for PC1",
         xlab = "Variable Laodings",
         color = "red")

#PC2
#HR(homerun)、BB、RBI(打點)與PC2呈正相關，看起來和"打擊者"相關
second_pca[order(second_pca, decreasing = F)]
dotchart(second_pca[order(second_pca, decreasing = F)],
         main = "Loading Plot for PC2",
         xlab = "Vairable Loadings",
         color = "blue")

#PC3
#H1B(一壘安打)、H2B(二壘安打)與PC3呈正相關，看起來和"安打"有關
third_pca[order(third_pca, decreasing = F)]
dotchart(third_pca[order(third_pca, decreasing = F)],
         main = "Loading Plot for PC3",
         xlab = "Variable Loadings",
         color = "dark green")

#另一種PC分析圖，觀察每個球隊(obs.)的擅長特性是什麼
#Select PC1, PC2 to plot a loading plot
biplot(pca_data, choices=1:2)

#右邊的球隊適合上壘，多以盜壘(SB)和四壞球(BB)保送見長，全壘打表現中等(e.g.編號19)
#左上方的球隊以力量取勝，在全壘打(HR)和打點(RBI)上有顯著的優勢(e.g.編號11)
#下方的球隊不擅長全壘打，但在安打上的表現遠勝於其他球隊，盜壘也有一定水準(e.g.編號5)