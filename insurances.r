library(readr)
insurances <- read_delim("seguros2.csv", ";", escape_double = FALSE, trim_ws = TRUE)
View(insurances)
attach(insurances)

# Attributes / Column names
names(insurances)

# Column datatypes
sapply(insurances, class)

# Dimensions
dim(insurances)

# It seems that the first column has different values for each row, therefore is irrelevant
dim(insurances[1])
dim(unique(insurances[1]))

# Remove column "Numero incidente"
insurances <- insurances[2:6]

# Summary
summary(insurances)
table(insurances[1])

# Explore the data

library(Rcmdr)

# Histograms
par(mfrow =c(2,3))
with(insurances, Hist(`Sexo (1 1 0 D)`, scale="frequency", breaks="Sturges", col="darkgray"))
with(insurances, Hist(Edad, scale="frequency", breaks="Sturges", col="darkgray"))
with(insurances, Hist(`Anos Coche`, scale="frequency", breaks="Sturges", col="darkgray"))
with(insurances, Hist(Caballos, scale="frequency", breaks="Sturges", col="darkgray"))
with(insurances, Hist(Costos, scale="frequency", breaks="Sturges", col="darkgray"))
par(mfrow=c(1,1))

# Boxplots
par(mfrow =c(1,3))
Boxplot( ~ Edad, data=insurances, id.method="y")
Boxplot( ~ Anos.Coche, data=insurances, id.method="y")
Boxplot( ~ Caballos, data=insurances, id.method="y")


# Scatter matrix
scatterplotMatrix(~Anos.Coche+Caballos+Costos+Edad+Sexo..1.1.0.D., reg.line=FALSE, smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), id.n=0, diagonal = 'density', data=insurances)

# Scatter plots
scatterplot(Caballos~Anos.Coche, reg.line=FALSE, smooth=FALSE, spread=FALSE,boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=insurances)
scatterplot(Costos~Anos.Coche, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=insurances)
scatterplot(Edad~Anos.Coche, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=insurances)
scatterplot(Sexo..1.1.0.D.~Anos.Coche, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=insurances)

scatterplot(Costos~Caballos, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=insurances)
scatterplot(Caballos~Edad, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=insurances)
scatterplot(Sexo..1.1.0.D.~Caballos, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=insurances)

scatterplot(Costos~Edad, reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=insurances)

scatterplot(Costos~Sexo..1.1.0.D., reg.line=FALSE, smooth=FALSE, spread=FALSE, boxplots=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), data=insurances)

# Scatter plots by Sex

# Minor data transformation (Just for the scatter plots by Sex)
insurances$`Sexo (1 1 0 D)` <- sapply(insurances$`Sexo (1 1 0 D)`, function(x) {
    ifelse(x == 0, "Mujer", "Hombre")
})

scatterplotMatrix(~Anos.Coche+Caballos+Costos+Edad | Sexo..1.1.0.D.,
reg.line=FALSE, smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE,
levels=c(.5, .9), id.n=0, diagonal= 'density', by.groups=TRUE,
data=insurances)

# Undo the transformation
insurances$`Sexo (1 1 0 D)` <- sapply(insurances$`Sexo (1 1 0 D)`, function(x) {
    ifelse(x == "Mujer", 0, 1)
})


# Cluster analysis
library(stats)

# Fix the seed so that the experiment is reproducible
set.seed(42);

# Determine number of clusters

cluster_number_plot <- function(data) {

  wss <- (nrow(data)-1)*sum(apply(data,2,var))

  for (i in 2:20) {
      tmp_cluster <- kmeans(data,centers=i, nstart = 20)
      wss[i] <- sum(tmp_cluster$withinss)
  }

  plot(1:20, wss, type="b", xlab="Number of Clusters",
      main="Assessing the Optimal Number of Clusters with the Elbow Method",
       ylab="Within groups sum of squares")
}

cluster_number_plot(insurances)

cluster <- kmeans(insurances,centers=6, nstart = 20)

# Cluster means
aggregate(insurances,by=list(cluster$cluster),FUN=mean)

# New dataset with the cluster assignment
insurancesAndClass <- data.frame(insurances, cluster$cluster)

# Visualization
library(fpc)

plotcluster(insurances, clus$cluster)

clusplot(insurances, clus$cluster, color=TRUE, shade=TRUE,
         labels=6, lines=0)

with(insurancesAndClass, pairs(insurances, col=c(1:5)[cluster$cluster]))

##############################################################
# Let's find the clusters without taking into account the Sex
##############################################################

insurancesWithoutSex <- insurances[-1]

clusterWithoutSex <- kmeans(insurancesWithoutSex,centers=6, nstart = 20)

# Cluster means
aggregate(insurancesWithoutSex,by=list(clusterWithoutSex$cluster),FUN=mean)

# New dataset with the cluster assignment
insurancesWithoutSexAndClass <- data.frame(insurancesWithoutSex, clusterWithoutSex$cluster)

plotcluster(insurancesWithoutSex, clusterWithoutSex$cluster)

clusplot(insurancesWithoutSex, clusterWithoutSex$cluster, color=TRUE, shade=TRUE,
         labels=6, lines=0)

with(insurancesWithoutSexAndClass, pairs(insurancesWithoutSex, col=c(1:5)[cluster$cluster]))
