food <- read.csv("./protein.csv")
View(food)
attach(food)

# Attributes / Column names
names(food)

# Column datatypes
sapply(food, class)

# Dimensions
dim(food)

# Check if there are cells with unknown values
any(is.na(food))


###################################
# Explore the data
###################################

[1] "Country"   "RedMeat"   "WhiteMeat" "Eggs"      "Milk"      "Fish"
 [7] "Cereals"   "Starch"    "Nuts"      "Fr.Veg"

par(mfrow =c(3,3))
densityPlot( ~ RedMeat, data=food, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ WhiteMeat, data=food, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ Eggs, data=food, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ Milk, data=food, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ Fish, data=food, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ Cereals, data=food, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ Starch, data=food, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ Nuts, data=food, bw="SJ", adjust=1, kernel="gaussian")
densityPlot( ~ Fr.Veg, data=food, bw="SJ", adjust=1, kernel="gaussian")

scatterplotMatrix(~Cereals+Eggs+Fish+Fr.Veg+Milk+Nuts+RedMeat+Starch+WhiteMeat,
reg.line=FALSE, smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE,
levels=c(.5, .9), id.n=0, diagonal = 'density', data=food)

####################################
# Cluster analysis
####################################

library(stats)
library(cluster)

# Determine number of clusters
cluster_number_plot <- function(data, maxNumberOfClusters=20) {

  wss <- (nrow(data)-1)*sum(apply(data,2,var))

  for (i in 2:maxNumberOfClusters) {
      tmp_cluster <- kmeans(data,centers=i, nstart = 20)
      wss[i] <- sum(tmp_cluster$withinss)
  }

  plot(1:maxNumberOfClusters, wss, type="b", xlab="Number of Clusters",
      main="Assessing the Optimal Number of Clusters with the Elbow Method",
       ylab="Within groups sum of squares")
}


foodstuff  <- food[, c(2:10)]
cluster_number_plot(foodstuff)
cluster <- kmeans(foodstuff, centers = 5, nstart = 20)

# Cluster means
aggregate(foodstuff,by=list(cluster$cluster),FUN=mean)

## list of cluster assignments
o=order(cluster$cluster)
data.frame(food$Country[o],cluster$cluster[o])

# Visualization
library(fpc)

plotcluster(foodstuff, cluster$cluster)

clusplot(foodstuff, cluster$cluster, color=TRUE, shade=TRUE, lines=0, labels = 2)

plot(foodstuff[, c("Eggs","Milk")], col =(cluster$cluster +1) , main="K-Means result with 5 clusters", pch=20, cex=2)
