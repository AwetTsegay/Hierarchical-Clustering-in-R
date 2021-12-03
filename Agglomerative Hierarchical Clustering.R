#======================================================#
#====================# Task 2 #========================#
#======================================================#
#### **Task 2: Implement agglomerative hierarchical clustering (AHC) in R from first principles**
# - You should apply your AHC program to the `NCI microarray` dataset.

# **You need to complete the following parts:**
  
#  a. Implement AHC with the following linkage functions: 
#  - single linkage, 
#  - complete linkage, 
#  - average linkage and 
#  - centroid linkage. 
# Your output should be a data structure that represents a dendrogram. 


# b. Implement a function getClusters that takes 
# - a dendrogram and a positive integer K as arguments, 
# - and its output is the K clusters obtained by cutting the dendrogram at an appropriate height.

#c. In your report, use the getClusters function to discuss the performance of AHC 
# with the four different linkage functions when applied to the NCI microarray dataset.
#======================================================#

### **2 (a). Implementing agglomerative hierarchical clustering (AHC)**
# First, importing the NCI microarray dataset
# Then applying agglomerative hierarchical clustering with different linkage functions.

# Step 1. Importing the dataset.
ncidata <- read.table("ncidata.txt")
ncidata <- t(ncidata)
any(is.na(ncidata))
dim(ncidata)

#Step 2. Preparing the data.
# Standardize the data
ncidata <- scale(ncidata)

#Step 3. Calculate Euclidean distance.
# Finding distance matrix
ahc.dist <- dist(ncidata, method = "euclidean")
as.matrix(ahc.single.dist)[1:5, 1:5]

#Step 4. Applying AHC using linkage functions and Create dendrograms.
#Implement AHC with the linkage functions:
#Fitting Agglomerative Hierarchical Clustering Model to ncidata dataset

# methods to assess
m <- c("single", "complete", "average", "centroid")

# function to compute linkage functions.  
AHC <- function(x) {
  for (i in x) {
    ahc.di <- dist(ncidata, method = "euclidean")
    ahc.med <- hclust(ahc.di, method = i)
    print(ahc.med)
    plot(ahc.med, hang = -1, cex = 0.6)
  }
}
AHC(m)

# Agglomerative Hierarchical Clustering for single linkage
start.time <- Sys.time()
ahc.single <- hclust(ahc.dist, method="single")
ahc.single
end.time <- Sys.time()
print(end.time - start.time)
# Plotting a dendrogram for single linkage
plot(ahc.single, hang = -1, cex = 0.6, main="Single Linkage: Cluster Dendrogram")


# Agglomerative Hierarchical Clustering for complete linkage
start.time <- Sys.time()
ahc.complete <- hclust(ahc.dist , method="complete")
ahc.complete
end.time <- Sys.time()
print(end.time - start.time)
# Plotting a dendrogram for complete linkage
plot(ahc.complete, hang = -1, cex = 0.6, main="Complete Linkage: Cluster Dendrogram")


# Agglomerative Hierarchical Clustering for average linkage
start.time <- Sys.time()
ahc.average <- hclust(ahc.dist, method="average")
ahc.average
end.time <- Sys.time()
print(end.time - start.time)
# Plotting a dendrogram for average linkage
plot(ahc.average, hang = -1, cex = 0.6, main="Average Linkage: Cluster Dendrogram")


# Agglomerative Hierarchical Clustering for centroid linkage
start.time <- Sys.time()
ahc.centroid <- hclust(ahc.dist, method="centroid")
ahc.centroid
end.time <- Sys.time()
print(end.time - start.time)
# Plotting a dendrogram for centroid linkage
plot(ahc.centroid, hang = -1, cex = 0.6, main="Centroid Linkage: Cluster Dendrogram")

#====================================================

### 2 (b). Implementing a function getClusters 

## Choosing no. of clusters
# Cutting tree by height

# Cutting tree by no. of clusters
fit <- cutree(Hierar_cl, k = 3 )

table(fit)
rect.hclust(Hierar_cl, k = 3, border = "green")


