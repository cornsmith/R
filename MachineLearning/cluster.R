df <- as.matrix(read.csv("data.csv", row.names = 1))
df[is.na(df)] <- 0

# Hierachical Cluster -----------------------------------------------------
hCluster <- hclust(dist(t(df)), "ward")
hCluster <- hclust(dist(t(df)), "complete")
plot(hCluster)



# K-Means Cluster ---------------------------------------------------------
# df <- scale(df)

# Scree plot k = 1 to 30
wss <- sapply(1:30, function(i){
    sum(kmeans(df, centers = i)$withinss)
    print(i)
})
plot(1:30, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# clustering
km <- kmeans(df, centers = 6, iter.max = 50, nstart = 10)

df$Cluster <- km$cluster