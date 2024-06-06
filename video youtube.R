#CLUSTERING VIDEO YOUTUBE COMPARATIVO
d<-dist(pceable, method = "euclidean")
fit<-hclust(d, method = "ward")
plot(fit, labels = 1:nrow(pceable))
pceable<-scale(pceable)
grupos<-cutree(fit, k=4)
table(grupos)
datos$grupo_cluster<-grupos_pca
mypca<-resultado_pca$svd$U
dpca<-dist(mypca, method = "euclidean")
fit_pca<-hclust(dpca, method = "ward.D")
plot(fit_pca)
dist_matrix <- dist(pceable, method = "euclidean")
cof_dist_matrix <- cophenetic(fit_pca)
correlation <- cor(dist_matrix, cof_dist_matrix)
print(correlation)
