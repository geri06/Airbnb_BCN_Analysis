##import de libraries
library(FactoMineR)
library(Matrix)
library(factoextra)
library("corrplot")
library(ggplot2)

##import the dataset
data<-read.csv("C:/Users/dancv/OneDrive/Escritorio/cuatri4/PMAAD/Práctica/Dataset_clean_clean.csv")
##summary of the dataset
summary(data)
##name of the variables
names(data)

##we select the categorical variables: source, h_loc, h_res_t, s_host, h_ver, h_id_v, n_gr_cl, r_type, inst_bk and gender.
qualitatives<-data[c(1,5,7,10,11,12,13,16,26,29)]
##plot them
n=10
for (i in 1:n) {barplot(table(qualitatives[i]), main=colnames(qualitatives)[i], col="steelblue", las = 2)}

#MCA
res.mca<-MCA(data[c(1,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,25,26,27,28,29,30)],quanti.sup=c(4,5,10,11,13,14,15,16,17,18,19,21,22,24),graph=TRUE)
##We have 40 modalities (k) in 10 active categorical variables (p), so the max number of MC dimensions is (k-p), 40-10=30 dimensions.
##We have to select the number of dimensions by (1/p) 1/10. So we select 13 dimensions as from 14 on the variance value is less than 1/10.

##summary from the MCA where we can see the results
summary(res.mca)

#Eigenvalues
##summary of the eigenvalues
res.mca$eig 
###graphic from eigenvalues
barplot(res.mca$eig[,1],main="EIGENVALUES",names.arg=1:nrow(res.mca$eig),col="steelblue") 
abline(h=0.1, col="red")
###graphic from percentage of explained variances
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 20))
###Equal to (k-p)/p = (40-10)/10. Is the total inertia from the data, 3.
totalInertia<- sum(res.mca$eig[,1]) 
totalInertia
###inertia from 13 first dimensions
totalInertia_13dim <- sum(res.mca$eig[1:13, 1])
totalInertia_13dim
###inertia from 3 first dimensions
totalInertia_3dim <- sum(res.mca$eig[1:3, 1])
totalInertia_3dim 

###percentage of explained variances
pinerEix<- 100*res.mca$eig[,1]/totalInertia 
barplot(cumsum(pinerEix),col="steelblue")
cumsum(pinerEix)

#Contributions

##we get more information about the individuals and the variables
ind <- get_mca_ind(res.mca)
ind
var <- get_mca_var(res.mca)
var

#CATEGORICAL VARIABLES - MODALITIES
##More important features for each dimension
res.mca$var$contrib
##More important features for dimensions 1,2 and 3
contrib_sorted <- list()
for (i in 1:3) {
  contrib_dim <- res.mca$var$contrib[,i]
  sorted_contrib_dim <- sort(contrib_dim, decreasing = TRUE)
  contrib_sorted[[paste0("Dimension", i)]] <- head(sorted_contrib_dim, 5)
}
print(contrib_sorted)

#plots from contributions for each combination of dimensions
fviz_contrib(res.mca, choice = "var", axes = c(1,2), top = 10)
fviz_contrib(res.mca, choice = "var", axes = c(1,3), top = 10)
fviz_contrib(res.mca, choice = "var", axes = c(2,3), top = 10)


#Individuals
res.mca$ind$contrib
##factorial maps of the modalities and with the individuals
fviz_mca_var(res.mca,  axes = c(1,2),col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, ggtheme = theme_minimal())#este cunde mucho más
fviz_mca_biplot(res.mca,axes = c(1,2),repel = TRUE, ggtheme = theme_minimal())
fviz_mca_var(res.mca,  axes = c(1,3),col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, ggtheme = theme_minimal())#este cunde mucho más
fviz_mca_biplot(res.mca,axes = c(1,3),repel = TRUE, ggtheme = theme_minimal())
fviz_mca_var(res.mca,  axes = c(2,3),col.var = "contrib",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE, ggtheme = theme_minimal())#este cunde mucho más
fviz_mca_biplot(res.mca,axes = c(2,3),repel = TRUE, ggtheme = theme_minimal())
###coordinates from de individuals
res.mca$ind$coord

fviz_mca_var(res.mca, axes = c(1,2),choice = "mca.cor", repel = TRUE,ggtheme = theme_minimal(),geom = "text")
fviz_mca_var(res.mca, axes = c(1,3),choice = "mca.cor", repel = TRUE,ggtheme = theme_minimal(),geom = "text")
fviz_mca_var(res.mca, axes = c(2,3),choice = "mca.cor", repel = TRUE,ggtheme = theme_minimal(),geom = "text")

#for dimension 1¬2 plot of the individuals
var_list <- colnames(qualitatives) # Reemplaza df con el nombre de tu dataframe
for (var in var_list) {
  print(fviz_mca_ind(res.mca, 
                     label = "none", 
                     habillage = var, 
                     palette = c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
                                 "#D55E00", "#CC79A7", "#000000", "#999999", "#ECECEC"),
                     addEllipses = TRUE, ellipse.type = "confidence",
                     ggtheme = theme_minimal()))
}



