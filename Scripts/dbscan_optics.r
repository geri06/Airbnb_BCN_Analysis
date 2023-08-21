library(cluster)
library(fpc)
library(pracma)
library(factoextra)
library(dbscan)
library(tidyverse)

# =============================================================================
### Generamos una semilla para poder ejecutar los datos
set.seed(04102022)

# ==============================================================================
### Creamos la base de datos que vamos a utilizar para detectar los grupos
data <- read_csv("C:/Users/alexc/OneDrive/Escriptori/PMAAD/Dataset_clean_clean.csv")
data_dummies <- fastDummies::dummy_cols(data, select_columns = c("source","h_loc","h_res_t","s_host","h_ver","h_id_v","n_gr_cl","r_type","inst_bk","gender"),
                                        remove_first_dummy = TRUE,remove_selected_columns = TRUE)
data_num <- data_dummies[sapply(data_dummies,is.numeric)]

# ==============================================================================
# KMEANS: 
### Gráficamos los datos a través de un k-means para visualizar como quedarian los 
### grupos cuando utilizamos unos algoritmos de agrupación a partir de la inercia
km_clusters <- kmeans(x = data_num, centers = 5, nstart = 50)
fviz_cluster(object = km_clusters, data = data_num, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw() +
  theme(legend.position = "none")

### Como podemos ver, Kmeans ha hecho una muy mala clusterización, puesto que:
###  - No ha conseguido clusterizar según las formas complejas del modelo.
###  - No ha tenido en cuenta que existen outliers, incluyéndolos en los distintos clusters

# ==============================================================================
# DBSCAN: 

### DBSCAN parte de dos parámetros que son: 
### - eps: distancia máxima a la que debe haber otra observación para ser considerar 
###        que cumple con el criterio de "estar cerca"
### - minPts: parámetro que controla la densidad mínima requerida para que un punto 
###           sea considerado un núcleo y se incluya en un grupo/clúster.

### Para un punto p, si existen al menos minPts puntos dentro del radio eps alrededor de p, 
### entonces p se considera un núcleo (core point) y se incluye en el mismo grupo/clúster 
### que los demás puntos dentro del radio eps. 
### Si no hay suficientes puntos dentro del radio eps, p se considera un punto frontera (border point) 
### y se incluye en el mismo grupo/clúster que su punto núcleo más cercano. 
### Si no hay ningún punto dentro del radio eps, p se considera un punto de ruido (noise point) 
### y no se incluye en ningún grupo/clúster.

# Cálculo de min_pts

### El parámetro min_pts establece el número de puntos mínimo que, dado un radio eps, tiene
### que haber para que se considere que dichos puntos forman un clúster.
### Un valor bajo de min_pts asegurará que más puntos son agrupados, pero se corre el riesgo de 
### agrupar outliers. Por el contrario, un valor muy alto de min_pts puede descartar valores que 
### no son anómalos.

### En la literatura hablan de usar un valor entre 3 y 5 ya que funcionan bastante bien en la mayoría de los casos. min Pts igual 2 cuando tenemos una distribución normal y otra nube de outliers

### Para calcularlo de manera empírica, diremos que el mínimo de puntos sea igual al 0.2% - 0.25% del total de los datos teniendo en cuenta que: 

### - El minimo será de 2 para datos que sean muy pequeños
### - El máximo será de 10 para datos con mucha información

#### Cálculo de min_pts

porcentaje <- 0.0025 

# Cálculo de min_pts.

min_pts <- round(nrow(data_num) * porcentaje) 

# Realitzem els talls de 2 i 15. 15 ja que tenim molts samples

min_pts <- ifelse(min_pts <= 1, 2, min_pts)
min_pts <- ifelse(min_pts >= 10, 15, min_pts)

#Normalitzem dades

data_num_norm <- data.frame(lapply(data_num, scales::rescale))

#Càlcul de epsilon

#The method proposed here consists of computing the k-nearest 
#neighbor distances in a matrix of points.
#The idea is to calculate, the average of the distances of every point 
#to its k nearest neighbors. The value of k will be specified by the user 
#and corresponds to MinPts.
#Next, these k-distances are plotted in an ascending order. 
#The aim is to determine the “knee”, which corresponds to the 
#optimal eps parameter. 

eps_plot = kNNdistplot(data_num_norm, k=5,minPts = min_pts)
eps_plot %>% abline(h = 1.4, lty = 2)  
#El més acurat és 1.4 per a tallar on l'elbow

epsilon <- 1.4

d <- dbscan::dbscan(data_num_norm, eps = epsilon, MinPts =  min_pts)
d$cluster  #Els del cluster 0 no pertanyen a cap cluster. Són soroll/outliers.

### Añado la columna clúster a mis datos.
data_num$cluster <- d$cluster

### Guardo datos limpios.
data_num_clean <- dplyr::filter(data_num, cluster != 0)

### Guardo outliers.
outliers <- dplyr::filter(data_num, cluster == 0) 

### Graficamos el dbscan obtenido 
fviz_cluster(object = d, data = data_num[,-45], geom = "point", ellipse = TRUE,
             show.clust.cent = FALSE, pallete = "jco") +
  theme_bw()


### Otra manera de visualizar los clusters obtenidos
hullplot(data_num, d$cluster, main = paste0("Convex cluster Hulls, eps = ", epsilon))

print(d)

data_num_dbscan <- data_num  #Guardem dataset amb columna del cluster
data_num <- data_num[,-45]

#--------------------------OPTICS---------------------------------#
#Fem una primera prova amb valors arbitraris de epsilon i min_pts

library(dbscan)
optics <- dbscan::optics(data_num_norm, eps = 1.5,  minPts = 10)

### get order
optics$order

### plot produces a reachability plot
plot(optics)

# -----------------------------------------------------------------------------
### Optimizamos la búsqueda de parámetros para epsilon y minPts en Optics
library(doParallel)
library(foreach)

### Definimos los valores que se van a probar para eps y minPts
eps_values <- seq(0.1, 1.5, by = 0.1)
minPts_values <- seq(5, 20, by = 5)

### Crear una cuadrícula de búsqueda de los valores de eps y minPts
grid <- expand.grid(eps = eps_values, minPts = minPts_values)

### Establecemos el número de núcleos que se van a usar para realizar la optimización en paralelo
cores <- detectCores()
registerDoParallel(cores = cores)

### Creamos una función para ejecutar OPTICS con una combinación de parámetros y calcular el coeficiente de silueta. 
run_optics <- function(data, eps, minPts) {
  optics <- dbscan::optics(data, eps = eps, minPts = minPts)
  res <- dbscan::extractDBSCAN(optics, eps_cl = eps)
  sil <- cluster::silhouette(res$cluster, dist(data))
  return(ifelse(is.na(sil), sil, mean(sil[, 3])))
}
### Con esta función nos permitirá luego paralelizar le proceso

### Ejecutar la cuadrícula de búsqueda en paralelo para la función dada
results <- foreach(i = 1:nrow(grid), .combine = rbind) %dopar% {
  eps <- grid$eps[i]
  minPts <- grid$minPts[i]
  score <- run_optics(data_num_norm, eps, minPts)
  c(eps, minPts, score)
}

results <- results[, c(1:3)]  

### Seleccionamos la combinación de parámetros que produjo el mejor resultado
best_params <- grid[which.max(results[, 3]), ] #eps = 1.5 i min_pts = 5

### Creamos el modelo con los mejores parámetros
optics <- dbscan::optics(data_num_norm, eps = best_params$eps, minPts = best_params$minPts)
plot(optics)  #Reachability plot



#---------Mètode de la Silueta per trobar valor òptim de epsilon amb minPts determinat---------#


#### Ejecutar OPTICS para diferentes valores de eps
eps_values <- seq(0.1, 1.5, by = 0.1)
optics_results <- lapply(eps_values, function(e) optics(data_num_norm, eps = e, minPts = 5))

#### Obtener los agrupamientos para cada valor de eps
clusters <- lapply(optics_results, function(x) extractDBSCAN(x, eps = x$eps))

#### Calcular la medida de silhouette promedio para cada valor de eps
silhouette_avg <- sapply(clusters, function(x) mean(cluster::silhouette(x$cluster, dist(data_num_norm))))

# Graficar la medida de silhouette promedio en función de eps
plot(eps_values, silhouette_avg, type = "b", pch = 20, main = "Silhouette Plot")

# Agregar una línea vertical en el valor óptimo de eps
opt_eps <- eps_values[which.max(silhouette_avg)]
abline(v = opt_eps, lty = 2, col = "red")


#----------------------------------------#

optics_1 <- optics(data_num_norm,eps=1.5,minPts=5)  #Alternativa del primer mètode d'optimització
optics_2 <- optics(data_num_norm,eps=1,minPts=5)

#Comparem reachability plots

plot(optics_1)  
plot(optics_2)

#Comparem clusters generats extraient el dbscan de cadascun

cl_1 <- dbscan::extractDBSCAN(optics_1, eps_cl = 1.5)
cl_2 <- dbscan::extractDBSCAN(optics_2, eps_cl = 1)

#Comparem els clusters al reachability plot

plot(cl_1)
plot(cl_2)

#Veiem quants clusters han quedat per a cada alternativa

print(cl_1)
print(cl_2)

#I ara fent un PCA

data_num_optics_1 <- data_num
data_num_optics_2 <- data_num

data_num_optics_1$cluster <- as.factor(cl_1$cluster)
data_num_optics_2$cluster <- as.factor(cl_2$cluster)

#install.packages("ggfortify")
library(ggfortify)
pca_1 <- prcomp(data_num_optics_1[,-45],scale. = TRUE)
autoplot(pca_1,data=data_num_optics_1,colour="cluster",frame=TRUE)

pca_2 <- prcomp(data_num_optics_2[,-45],scale. = TRUE)
autoplot(pca_2,data=data_num_optics_2,colour="cluster")  #llegenda
autoplot(pca_2,data=data_num_optics_2,colour="cluster",frame=TRUE)+theme(legend.position="NULL")

#Saving best cluster


data_clustered <- read_csv("C:/Users/alexc/OneDrive/Escriptori/PMAAD/Clustered_DB.csv")
data_clustered <- data_clustered[,-1]
data_clustered$dbscan_4 <- data_num_dbscan$cluster

setwd("C:/Users/alexc/OneDrive/Escriptori/PMAAD/")
write.csv(data_clustered, file = "Clustered_DB.csv", row.names = FALSE)