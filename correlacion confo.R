

distancia <- dist(pceable, method = "euclidean")
hc1 <- hclust(distancia, method = "complete" )
plot(hc1)
complete <- agnes(pceable, method = "complete")
complete$ac
ward <-agnes(pceable, method = "ward")
ward$ac
single <-agnes(pceable, method = "single")
single$ac
average <- agnes(pceable, method = "average")
average$ac
pltree(ward, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
clust <- cutree(ward, k = 5)
fviz_cluster(list(data = pceable, cluster = clust)) 
pltree(ward, hang=-1, cex = 0.6)
rect.hclust(ward, k = 5, border = 2:10)

