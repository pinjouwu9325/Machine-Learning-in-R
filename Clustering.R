#Clustering

##Hierarchical Clustering
head(iris)

#Because it is unsupervised learning, remove the "Species" column
data <- iris[, -5]
head(data)

#cluster by the distance between each data
#use dist() to create distance matrix
E_dist <- dist(data, method = "euclidean")
M_dist <- dist(data, method = "manhattan")

#set rhe grach 1*2
par(mfrow=c(1, 2)) 

#clustering: hclust()
#HC:euclidean
h_E_dist <- hclust(E_dist)
plot(h_E_dist, xlab = "Euclidean dist")

#HC:mahattan 
h_M_dist <- hclust(M_dist)
plot(h_M_dist)

#Single Linkage: "single"
#Complete Linkage: "complete"
#Average Linkage: "average"
#Centroid Method: "centroid"
#Wards Method: "ward.D2"

#elcidean + ward
w_E_cluster <- hclust(E_dist, method = "ward.D2")
plot(w_cluster)
abline(h=9, col="blue")
#it seems to be three clusters

#cut the tree:cutree()
cut_cluster <- cutree(w_E_cluster, k=3) #cut to three
cut_cluster

#summary
table(cut_cluster, iris$Species)
ggplot(data=iris)+
         geom_point(aes(x=Petal.Length, y=Petal.Width, color=Species))+
  theme_bw()

##Partitional Clustering 
#K-means
#make it three clusters
kmeans_cluster <- kmeans(data, centers = 3)

#variance within a cluster
kmeans_cluster$withinss

#summary
table(kmeans_cluster$cluster, iris$Species)

#visualisation
install.packages("factoextra")
library(factoextra)
fviz_cluster(kmeans_cluster, #clustered result
             data = data,    #original data
             geom = c("point", "text"),  #point and label
             frame.type = "norm")  #frame type

#K-Medoid
install.packages("cluster")
library(cluster)             

#pam = Partitioning Around Medoids
kmedoid_cluster <- pam(data, k=3)

#variance in a cluster
kmedoid_cluster$objective

#summary
table(kmedoid_cluster$clustering, iris$Species)

#visualisation
fviz_cluster(kmedoid_cluster,
             data = data,
             geom = c("point"),
             frame.type = "norm")


##Optimal number of clusters
#Aim: mini variance within a cluster(SEE), max variance between clusters
#Elbow Method: given n. When data is clustered into n clusters,SSE is the minimun.
#now we can say that n is optinal number of clusters

#Elbow Method + Hierarchical clustering
fviz_nbclust(data,
             FUNcluster = hcut, #hierarchical clustering
             method = "wss", #total within sum of square
             k.max = 10 #max number of clusters to consider
             )+
  labs(title="Elbow Method for HC")+
  geom_vline(xintercept = 3, # x=3
             linetype = 2)  #draw a dot line

#Elbow Method + K-means
fviz_nbclust(data,
             FUNcluster = kmeans,
             method = "wss",
             k.max = 10)+
  labs(title="Elbow Method for K-Means")+
  geom_vline(xintercept = 3,
             linetype = 2)

#Elbow + K-Medoid
fviz_nbclust(data,
             FUNcluster = pam,
             method = "wss",
             k.max = 10)+
  labs(title="Elbow Method for K-Medoid")+
  geom_vline(xintercept = 3,
             linetype = 2)

#Another method: Average Silhouette Method
#Silhouette coefficient側影係數:根據每個資料點(i)的內聚力和分散力，衡量分群的效果(quality)
fviz_nbclust(data,
             FUNcluster = kmeans,
             method = "silhouette",
             k.max = 10)+
  labs(title="Silhouette method for K-means")
