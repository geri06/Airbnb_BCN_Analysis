# ==============================================================================
# Time Series
# ==============================================================================
# Load Packages
install.packages("dtw")
library(dtw)
library(tidyverse)
library(forecast)
library(ggplot2)
library(readxl)
library(httr)
library(dplyr)

setwd("D:/Descargas/Universitat/2.2/PMAAD/BasedeDades/TimeSeries")

archivos_csv <- list.files()
lista_datos <- lapply(archivos_csv, read.csv)

# ------------------------------------------------------------------------------
#We create the dataset using the first country of the list.
data <- lista_datos[[1]]
data$date <- substr(data$date, 1, 7)
datos <- data %>% group_by(date) %>% summarize(count = n())
complete_dataset <- datos
l_datos <- tail(lista_datos,-1)

for (dd in l_datos){
  dd$date <- substr(dd$date, 1, 7)
  datos <- dd %>% group_by(date) %>% summarize(count = n())
  complete_dataset <- merge(complete_dataset, datos, by = "date", all = TRUE)
}


data_clean <- complete_dataset[1:167, ]
data_clean[is.na(data_clean)] <- 0
colnames(data_clean) <- c("Date",gsub("\\.csv$", "", archivos_csv))
data_clean1 <- data_clean[ ,2:ncol(data_clean)]
data_clean1 %>% 
  dplyr::mutate(t = 1:n()) %>% 
  tidyr::gather(key, value, -t) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=t, y=value, color=key), show.legend = F)+
  ggplot2::facet_wrap(.~key)


data_precovid <- data_clean1[1:130, ]
data_postcovid <- data_clean1[143:167, ]

data_precovid %>% 
  dplyr::mutate(t = 1:n()) %>% 
  tidyr::gather(key, value, -t) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=t, y=value, color=key), show.legend = F)+
  ggplot2::facet_wrap(.~key)

data_postcovid %>% 
  dplyr::mutate(t = 1:n()) %>% 
  tidyr::gather(key, value, -t) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=t, y=value, color=key), show.legend = F)+
  ggplot2::facet_wrap(.~key)

#PreCovid
preTrasp <- t(data_precovid)
distMatrixPRE <- dist(preTrasp, method="DTW")
hcpre <- hclust(distMatrixPRE, method="complete")
plot(hcpre)
#k=3
clusterPRE <- cutree(hcpre,k=3)

# Crea un gráfico de líneas con todas las series temporales

v_colores <- c("orange", "red", "green")

# Crear el gráfico utilizando ggplot2 y asignar colores desde el vector v_colores
p <- ggplot(data = data_precovid, aes(x = 1:nrow(data_precovid)))

# Bucle for para agregar las capas geom_line con los colores correspondientes
for (i in 1:ncol(data_precovid)) {
  p <- p + geom_line(aes_string(y = colnames(data_precovid)[i]), color = v_colores[clusterPRE[i]]) + ggtitle("Number of reviews over time") + 
    labs(y= "Number of reviews", x="Time")
}

# Imprimir el gráfico
print(p)



###############################
data_precovid%>% 
  dplyr::mutate(t = 1:n()) %>% 
  tidyr::gather(key, value, -t) %>% 
  ggplot2::ggplot()+
  ggplot2::geom_line(ggplot2::aes(x=t, y=value, color=key), show.legend = TRUE)


#PostCovid
postTrasp <- t(data_postcovid)
distMatrixPOST <- dist(postTrasp, method="DTW")
hcpost <- hclust(distMatrixPOST, method="complete")
plot(hcpost)
#k=3
clusterPOST <- cutree(hcpost,k=3)

# Crear el gráfico utilizando ggplot2 y asignar colores desde el vector v_colores
p <- ggplot(data = data_postcovid, aes(x = 1:nrow(data_postcovid)))

# Bucle for para agregar las capas geom_line con los colores correspondientes
for (i in 1:ncol(data_postcovid)) {
  p <- p + geom_line(aes_string(y = colnames(data_postcovid)[i]), color = v_colores[clusterPOST[i]]) + ggtitle("Number of reviews over time") + 
    labs(y= "Number of reviews", x="Time")
}

# Imprimir el gráfico
print(p)

