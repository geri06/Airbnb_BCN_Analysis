# ==============================================================================
# [PMAAD] - CURE Algoirthm
# 
# Author(s):    Dante Conti and Sergi Ramírez, IDEAI  (c)
# Date:         6th March 2023
# Description: 


# ==============================================================================
library(dplyr)
library(readr)
library(fastDummies)
library(cluster)
library(factoextra)
library(tidyr)

set.seed(42277)
#Cargamos los datos 

setwd("D:/Descargas/Universitat/2.2/PMAAD")
datos<-read_csv("D:/Descargas/Universitat/2.2/PMAAD/BasedeDades/CLustered_DB.csv",locale = locale(encoding = "UTF-8"))
#Preparamos los datos para el clustering (hacemos las dummies)

cluster_data <- datos[, setdiff(names(datos), c("description","n_ovw","h_abt","h_since","fst_r","lst_r","dbscan_4","cluster_CURE_2","cluster_CURE_3","cluster_HC_2","cluster_HC_3"))]
cluster_data <- cluster_data %>% mutate_if(is.numeric, scale)
cluster_data$id_orig <- as.integer(rownames(cluster_data))
df_cat <- names(cluster_data[sapply(cluster_data, is.character)])
dummies <- dummy_cols(cluster_data, select_columns = df_cat, remove_selected_columns = TRUE)

#Hacemos un hierarchical clustering para ver los resultados i definir una k.

hc <- hclust(dist(dummies[, -which(names(dummies) == "id_orig")], method ="euclidean"), method = "ward.D2") # asumimos que data es nuestra matriz de datos
barplot(hc$height)
plot(hc)
abline(h = 95, col = "red")
#The optim may be k=2, but we will try with a higher k also, because it may give a better result for the business requirements.
abline(h = 80, col = "orange")
#We will try k=2 and k=3 too
cluster <- cutree(hc,k=3)
datos$cluster_HC_2 <- cluster
fviz_cluster(list(data = dummies[, -which(names(dummies) == "id_orig")], cluster = cluster), geom = "point")
datos$cluster_HC_3 <- cluster
# Paso 1: Definir el número de clústeres k y el factor de reducción r
set.seed(42277)
k <- 3
r <- 0.2 #Como el resultado es bastante parecido al clustering jerarquico dejaremos la r tal como esta ya que es bastante baja y los resultados siguen siendo similares
metodo <- "euclidean"
data <- dummies
n <- ceiling(0.3*nrow(data))

# Sample de la muestra
ids <- sample(1:nrow(data), replace = FALSE, size = n)
dataNoMuestra <- data[-ids, ]
data <- data[ids, ]
dataMuestra <- data

# El factor de reducción en el algoritmo CURE de clustering indica la proporción de elementos que se 
# seleccionan como representantes en cada subconjunto de datos. Este factor se suele representar por 
# la letra "r" y se define como un número entre 0 y 1.

# Por ejemplo, si el factor de reducción es 0.2, significa que se seleccionará el 20% de los elementos 
# de cada subconjunto como representantes. En general, un factor de reducción menor implica una mayor 
# cantidad de elementos representativos, lo que puede aumentar la precisión del clustering, pero también 
# aumenta el costo computacional.

# Es importante destacar que el factor de reducción debe ser elegido cuidadosamente para cada conjunto 
# datos, ya que no existe un valor óptimo para todos los casos. Por lo tanto, es recomendable probar 
# diferentes valores y evaluar la calidad del clustering resultante.


# Paso 2: Dividir los datos en s subconjuntos utilizando un algoritmo de partición jerárquica
# s <- ceiling(1/r)
hclust <- hclust(dist(data[, -which(names(data) == "id_orig")], method = metodo), method = "ward.D2") # asumimos que data es nuestra matriz de datos
plot(hclust)
subsets <- cutree(hclust, k)
dataMuestra$cluster <- subsets

### Aqui se podria realizar el plot del arbol cortado

# Paso 3: Seleccionar r elementos representativos de cada subconjunto utilizando k-means
centroides <- matrix(0, k, ncol(data[, -which(names(subset) == "id_orig")]))
representativos <- list()
noRepresentativos <- list()
indicesRepresentativos <- list()
for (i in 1:k) {
  subset <- data[subsets == i,]
  subset$id <- rownames(subset)
  colmeans_result <- colMeans(subset[, -which(names(subset) %in% c("id", "id_orig"))]) #Sustituimos el kmeans por una agregacion de las muestras de ese subset
  centroides[i,] <- colmeans_result

  matriz <- rbind(centroides[i, ], subset[, -which(names(subset) %in% c("id", "id_orig"))]) # Creamos una matriz con los centroides y todas las muestras
  distancias <- dist(x = matriz, method = metodo) #calculamos las distancias entre todos los elementos de la matriz
  distanciaFinal <- as.matrix(distancias)[, -1] #nos quedamos con la distancia de las muestras al centroide para escoger las representativas
  names(distanciaFinal) <- subset$id #cambiamos los nombres de las filas por su id, para trackear los id despues de ordenar las distancias
  pesosOrdenados <- sort(distanciaFinal, decreasing = FALSE) #ordenamos las distancias para encontrar las m�s representativas
  indices <- as.numeric(names(pesosOrdenados)[1:(r*nrow(subset))]) #cojemos las r m�s representativas
  indicesRepresentativos[[i]] <- indices
  representativos[[i]] <- subset[which(rownames(subset) %in% indices), ]
  noRepresentativos[[i]] <- subset[which(!rownames(subset) %in% indices), ]
}

noRepresentativos <- dplyr::bind_rows(noRepresentativos)

# Paso 4: Unir los elementos representativos seleccionados con los centroides
merged_representatives <- list()
for (i in 1:k) {
  bbdd <- rbind(centroides[i, ], representativos[[i]][, -ncol(representativos[[i]])])
  bbdd$cluster <- i
  merged_representatives[[i]] <- bbdd
}

centroidesRepresentativos <- dplyr::bind_rows(merged_representatives)

# Paso 5: Calculamos los nuevos centroides por cada una de las listas

NuevosCentroides <- centroidesRepresentativos %>% group_by(cluster) %>% summarise_all(mean) %>% data.frame()

# Paso 6: Asignar cada observación al clúster cuyo centroide esté más cercano
## Calculamos la distancia para cada cluster i detectamos cual es el más proximo a ellos 
clusterPertenece <- c()
for (i in 1:nrow(dataNoMuestra)) {
  ## 1. Agregamos el valor no representivo a los centroides nuevos
  bbdd <- dataNoMuestra[i, ]
  bbdd$cluster <- 0
  agregado <- rbind(bbdd, NuevosCentroides)
  
  ## 2. Calcula la distancia correspondiente
  distanciaCorr <- as.matrix(dist(agregado[, -which(names(agregado) %in% c("cluster", "id_orig"))], method = metodo))[1, -1]
  quienEsMenor <- as.numeric(which.min(distanciaCorr))
  clusterPertenece <- c(clusterPertenece, agregado[quienEsMenor+1, "cluster"])   
}

dataNoMuestra$cluster <- clusterPertenece
dataNoMuestra <- unnest(dataNoMuestra,cluster)

data <- rbind(dataMuestra,dataNoMuestra)
data_ordenado <- data[order(data$id_orig), ]

datos$cluster_CURE_2 <- data_ordenado$cluster
#volvemos a ejecutar con k=3
datos$cluster_CURE_3 <- data_ordenado$cluster

fviz_cluster(list(data= data_ordenado[, -which(names(agregado) %in% c("cluster", "id_orig"))], cluster = data_ordenado$cluster), geom="point")

write.csv(datos,file="Clustered_DB.csv")

