#Importing Libraries
  library(cluster)
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(factoextra)
#Load CSV data set and making a copy
  protein_1<-read.csv("protein.csv",fileEncoding = "latin1")
  protein_2<-protein_1

#PROGRAM FOR CLUSTERING REDMEAT and WHITEMEAT USING Kmean ALGORITHM AND PLOT USING DIFFERENT GRAPHS

#Select an object with the required fields for Kmeans
  red_white_meat<-protein_2 %>% select(RedMeat,WhiteMeat)
#For labeling get the country details in other variable
  Country<-protein_2$Country
#Calculate Kmean for the red_white_meat object(Redmeat and whitemeat) with 3 clusters and 25 iterations
  kmean.result <- kmeans(red_white_meat,3,25)
#Adding labels to cluster after making into factor datatype
  protein_2$Cluster_Meat <- as.factor(kmean.result$cluster)

#Plot using ggplot
  ggplot(protein_2, aes(x = RedMeat, y = WhiteMeat, color = Cluster_Meat, label = Country)) +
    geom_point(size = 3) +
    geom_text(vjust = -0.5, size = 5) +
    labs(title = "K-Means Clustering (k = 3) on Red and White Meat",
       x = "Red Meat Consumption", y = "White Meat Consumption") +
    theme_minimal()
#scaling the whitemeat and red meat protein values for getting normalized values
  cluster_colors<-c("Red","green","blue")
  red_white_meat_scaled<-red_white_meat %>%scale()
#Generating Kmeans with normalized values
  kmean.result <- kmeans(red_white_meat_scaled,3,25)
#plot a table to find similarity between countries and their redmeat and whitemeat consuption
  table(protein_2$Country,kmean.result$cluster)
#Plot the graph based on clusters, centroids and counties with RedMeat in x-axis and WhiteMeat in y-axis.
  plot(red_white_meat_scaled[,"RedMeat"],red_white_meat_scaled[,"WhiteMeat"],col=cluster_colors[kmean.result$cluster],xlim=c(-2,3),ylim = c(-2,2),xlab ="RedMeat",ylab = "WhiteMeat" )
  points(kmean.result$centers[,c("RedMeat", "WhiteMeat")], pch = 8, cex=4,col=cluster_colors[1:3])
  text(kmean.result$centers, labels = paste("C", 1:3, sep=""), pch=8, pos = 3, cex = 1,col = cluster_colors[1:3])
  text(red_white_meat_scaled[,"RedMeat"], red_white_meat_scaled[,"WhiteMeat"], 
     labels = Country,col = cluster_colors[kmean.result$cluster],pos = 4,cex = 1)

#install.packages("factoextra")
#Generate the same plot with fviz_cluster, which shows boundaries for each cluster and centriods
  fviz_cluster(kmeans(red_white_meat,centers = 3,100),data=red_white_meat)

# PROGRAM FOR CLUSTERING 9 PROTEIN INTO 7 CLUSTERS USING KMeans

#Removing the Country names from data for analystical purpose
  protein_1$Country<-NULL
#Scaling or Normalizing the data
  protein_1_scaled<-protein_1 %>% scale()
#For the pupose of ploting generate an object with different colors
  cluster_colors<-c("Red","blue","green","brown","violet","black","purple")
#Apply kmeans algorithm on above scaled data
  kmean.result1<-kmeans(protein_1_scaled,7,100) 

#Plot a table to find relationship between European Countries and protein consumption
  table(protein_2$Country,kmean.result1$cluster)
#piloting the graph with Eggs on X-axis and Milk on Y-axis and also influenced by all 9 protein consumption
#Created 7 clusters with countries as datapoints and each cluster having  centroids 
  plot(protein_1_scaled[,"Eggs"],protein_1_scaled[,"Milk"],col=cluster_colors[kmean.result1$cluster],xlim=c(-2.5,2),ylim =c(-2,2.8),ylab="Milk",xlab="Eggs")
  points(kmean.result1$centers[,c("Eggs", "Milk")], col = cluster_colors[1:7], pch = 8, cex=2,pos=3)
  text(kmean.result1$centers[,"Eggs"],kmean.result1$centers[,"Milk"]+.2,labels = paste("C", 1:7, sep=""),col=cluster_colors[1:7] ,cex = 1,pch=8)
  text(protein_1_scaled[,"Eggs"], protein_1_scaled[,"Milk"], 
     labels = Country, 
     pos = 3, cex = 1, col = cluster_colors[kmean.result1$cluster])

#piloting the graph with Fish on X-axis and Cereals on Y-axis and also influenced by all 9 protein consumption
#Created 7 clusters with countries as data points and each cluster having  centroids 

  plot(protein_1_scaled[,"Fish"],protein_1_scaled[,"Cereals"],col=cluster_colors[kmean.result1$cluster],xlim=c(-1,3.5),ylim = c(-1.3,2.5),xlab = "Fish",ylab = "Cereals")
  points(kmean.result1$centers[,c("Fish", "Cereals")], col = cluster_colors[1:7], pch = 8, cex=2)
  text(protein_1_scaled[,"Fish"], protein_1_scaled[,"Cereals"], 
     labels = Country, 
     pos = 4, cex = 1, col = cluster_colors[kmean.result1$cluster])
  text(kmean.result1$centers[,"Fish"],kmean.result1$centers[,"Cereals"], labels = paste("C", 1:7, sep=""),col = cluster_colors[1:7], pos = 3, cex = 0.8)


#piloting the graph with Starch on X-axis and Nuts on Y-axis and also influenced by all 9 protein consumption
#Created 7 clusters with countries as data points and each cluster having  centroids 
  plot(protein_1_scaled[,"Starch"],protein_1_scaled[,"Nuts"],col=cluster_colors[kmean.result1$cluster], xlim=c(-2,2.5), ylim=c(-1.5,2),xlab = "Starch",ylab = "Nuts")
  points(kmean.result1$centers[,c("Starch", "Nuts")], col = cluster_colors[1:7], pch = 8, cex=2)
  text(protein_1_scaled[,"Starch"],protein_1_scaled[,"Nuts"], 
     labels = protein_2$Country, 
     pos = 4, cex = 1, col = cluster_colors[kmean.result1$cluster])
  text(kmean.result1$centers[,"Starch"],kmean.result1$centers[,"Nuts"], labels = paste("C", 1:7, sep=""),col=cluster_colors[1:7], pos = 3, cex = 0.8)

#Plot a graph with 9 protein consumption, x axis and y axis plot is based on Dimensions
  fviz_cluster(kmean.result1,data = protein_1_scaled,geom="point")


