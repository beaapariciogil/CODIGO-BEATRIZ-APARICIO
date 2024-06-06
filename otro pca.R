# Instalar y cargar las librerías necesarias
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("ggplot2")

library(FactoMineR)
library(factoextra)
library(ggplot2)

# Supongamos que tu dataframe de datos se llama 'pcable'
# Cargar los datos (esto es solo un ejemplo, usa tus datos reales)
# pcable <- read.csv("ruta/a/tus/datos.csv")

# Realiza el PCA con el número máximo de componentes
resultado_pca <- PCA(pceable, ncp = min(nrow(pceable), ncol(pceable)), graph = FALSE)

# Obtener la varianza explicada por cada componente
varianza_explicada <- resultado_pca$eig[, 2]

# Obtener la varianza acumulada
varianza_acumulada <- resultado_pca$eig[, 3]

# Crear un dataframe para visualizar los resultados
varianza_df <- data.frame(
  Componente = 1:length(varianza_explicada),
  Varianza_Explicada = varianza_explicada,
  Varianza_Acumulada = varianza_acumulada
)

# Mostrar el dataframe
print(varianza_df)


#Obtener las cargas de las variables en las componentes principales
cargas <- resultado_pca$var$coord

# Mostrar las cargas de las primeras 5 componentes
print(cargas[, 1:5])


# Crear un biplot
fviz_pca_biplot(resultado_pca, axes = c(1, 2), 
                geom = c("point", "text"), 
                col.var = "red", col.ind = "blue")


fviz_contrib(resultado_pca, choice = "var", axes =2)

#nuevo pca
resultado_pca <- PCA(pceable, ncp = 5, graph = T)
#CLUSTERING A PARTIR DE PCA
#Para hacer clustering, debemos sacar la distancia
mypca<-resultado_pca$svd$U
dpca<-dist(mypca, method = "euclidean")
fit_pca<-hclust(dpca, method = "ward.D2")
plot(fit_pca, hang = -1)
grupos_pca<-cutree(fit_pca, k=4)
grupos_pca
table(grupos_pca)

plot(fit_pca, main = "Dendrograma - Método de Ward", labels = FALSE, hang = -1)


