##########################################
###    Methods for clustering in R    ####
##########################################

##  Data Preparation ###
## ################# ###

#Using internal data set
mydata = USArrests
# Prepare Data #
mydata <- na.omit(mydata) # listwise deletion of missing
mydata.orig = mydata #save orig data copy
mydata <- scale(mydata) # standardize variables


### Method 1 -  agglomerative Hierarchical clustering  ###
##########################################################

# Ward Hierarchical Clustering
distMat <- dist(mydata, method = "euclidean") # distance matrix
fit <- hclust(distMat, method="ward")
plot(fit) # display dendogram

#Eyeball the dendogram. Imagine horizontally slicing through the dendogram's longest vertical lines, each of which represents a cluster. Should you cut it at 2 clusters or at 4? How to know? Sometimes eyeballing is enough to give a clear idea, sometimes not. Suppose you decide 2 is better. Then set the optimal no. of clusters 'k1' to 2.

k1 = 4 # eyeball the no. of clusters
#Note: If for another dataset, the optimal no. of clusters changes to, say, 5 then use 'k1=5' in the line above instead. Don't blindly copy-paste that part. However, once you have set 'k1', the rest of the code can be peacefully copy-pasted as-is.
# cut tree into k1 clusters
groups <- cutree(fit, k=k1)
# draw dendogram with red borders around the k1 clusters
rect.hclust(fit, k=k1, border="red")


###             Method 2 - K-Mean Clustering           ###
##########################################################
# Again, the Q arises, how to know the optimal no. of clusters? Eyeballing the dendogram might sometimes help. But at other times, what should you do? MEXL (and most commercial software too) requires you to magically come up with the correct number as input to K-means. R does one better and shows you a scree plot of sorts that shows how the within-segment variance (a proxy for clustering solution quality) varies with the no. of clusters. So with R, you can actually take an informed call.

# Determine number of clusters #
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) # preparing a vector
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss) #calculating the variance per each K
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")
# Look for an "elbow" in the scree plot #
#The interior node at which the angle formed by the 'arms' is the smallest. This scree-plot is not unlike the one we saw in factor-analysis. Again, as with the dendogram, we get either 2 or 4 as the options available. Suppose we go with 2.
# Use optimal no. of clusters in k-means #
k1=4
# K-Means Cluster Analysis
fit <- kmeans(mydata, k1) # k1 cluster solution

# Get the meaning of the clusters
# =================================
#To understand a clustering solution, we need to go beyond merely IDing which individual unit goes to which cluster.
#We have to characterize the cluster, interpret what is it that's common among a cluster's membership, give each cluster
#a name, an identity, if possible. Ideally, after this we should be able to think in terms of clusters (or segments) 
#rather than individuals for downstream analysis.

# get cluster means
aggregate(mydata.orig,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata1 <- data.frame(mydata.orig, fit$cluster)

#But can I actually, visually, *see* what the clustering solution looks like? Sure. 
#In 2-dimensions, the easiest way is to plot the clusters on the 2 biggest principal components that arise.
library(cluster)
clusplot(mydata, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0) #plot the clustres base on the 2 biggest principals 


##    Method 3 - Model Based Clustering         ##
##################################################
# It is the most general approach (it nests the others as special cases), is the most robust to distributional and 
# linkage assumptions and because it penalizes for surplus complexity (resolves the fit-complexity tradeoff in an 
# objective way). My thumb-rule is: When in doubt, use model based clustering. And yes, mclust is available *only* on R 
# Model Based Clustering
library(mclust)
fit <- Mclust(mydata)
fit # view solution summary
#The mclust solution has 3 components! Something neither the dendogram nor the k-means scree-plot predicted. 
# Perhaps the assumptions underlying the other approaches don't hold for this dataset.
# I'll go with mclust simply because it is more general than the other approaches.
# Remember, when in doubt, go with mclust.

fit$BIC # lookup all the options attempted
classif = fit$classification # classifn vector
mydata1 = cbind(mydata.orig, classif) # append to dataset
mydata1[1:10,] #view top 10 rows
#The classification vector is appended to the original dataset as its last column. Can now easily assign individual units to segments.

# Use only if you want to save the output
write.table(mydata1,file.choose())#save output

#Visualize the solution. See how exactly it differs from that for the other approaches.
fit1=cbind(classif) #Convert to matrix
rownames(fit1)=rownames(mydata)
library(cluster)
clusplot(mydata, fit1, color=TRUE, shade=TRUE,labels=2, lines=0)
