#IMPUTATION TOOLS

### Cargamos las librerias necesarias
library(tidyverse)  # Preparación de datos y gráficos
library(missForest)
library(VIM)
library(pool)

#install.packages("mice")
#install.packages("devtools")
#devtools::install_github(repo = "amices/mice")
library(mice, warn.conflicts = FALSE)

#install.packages("Amelia")
library(Amelia)

#install.packages("Hmisc")
library(Hmisc)

#install.packages("mi")
library(mi)

data <- read_csv("C:/Users/alexc/OneDrive/Escriptori/PMAAD/Reduced_Database.csv")

# En comptes de l'string posem NA per a què els detecti
library(naniar)
na_string <- c("N/A","")
data <- data %>% replace_with_na_all(condition = ~.x %in% na_string)

#Treiem la primera columna d'índex

data <- data[,-1]


#1 -- Profiling your current 
#database detecting missing data in rows and columns

#----------------#

#Columnes i els seus missings. Iterar des de 1 a 30

summary(data)
aggr(data[21:29])
summary(aggr(data[,21:29]))
gg_miss_upset(data) #Visualització de la intersecció de missings

#Files amb més missings

#Mirem totes les files que continguin algun NA per veure si a simple vista podem
#analitzar quin tipus de Missing Value és.

data_na <- data[!complete.cases(data), ]

#Afegim una columna al dataframe original que compti els NA per fila,
#analitzem així si hi ha algun individu amb un nombre massa elevat de missings
#per a eliminar-lo directament. Només comptem aquells NA que provenen
#de columnes que no són ni de dates ni de text (serà tractat apart).


data$count_na <- rowSums(is.na(data[,-c(2,3,4,6,23,24)]))

#Comptem quin és el nombre de files que tenen un nombre "i" de missings
#iterant des de 0 missings fins a el nombre màxim (7).

for (i in 0:max(data$count_na)){
  cat(i,nrow(data[data$count_na == i, ]),"\n")
  
}

#Decidim que 7 és un nombre massa elevat de missings i esborrem les 4
#files amb 7 missings. La resta les deixem.

data <- data[data$count_na != 7, ]

#Esborrem la columna de sumatori de missings per fila que ja no necessitem

data <- data[,-c(31)]

#Amb aquest test de Little verifiquem si els missings restants 
#en variables numèriques del nostre dataset són
#MCAR o no ho són.

#Veiem que la variable max_navg té outliers que ens impedeixen que
#el mcar_test funcioni bé per la diferència de rang entre aquestes.
#Els valors anòmals els convertirem a NA.

summary(data$max_navg) 

#Veiem màxim i mitjana desvariats pels valors erronis. La mediana i 
#els quartils al ser indicadors millors de la tendència central i 
#més robustos es mantenen en els valors "normals".

hist(data$max_navg)

#Veiem que hi ha dos valors estranys que no concorden amb la informació
#que aporta la variable i tenen valors exactes. Outliers.

nrow(data[data$max_navg >= 9999, ]) #2

#Els convertim en NA

data[data$max_navg >= 9999, ] <- NA

#Utilitzem només les numèriques per a fer el test de Little i saber
#si els missings segueixen un patró MCAR.

data_num <- data[sapply(data,is.numeric)]
#Apliquem MCAR_TEST

mcar_test(data_num)

#Donat que el p-valor < 0.05, rebutjem Ho i podem dir que els missings
#en les nostres dades no són classificables com a MCAR.

#Fixant-nos una mica en la "sub-base" de dades creada a partir de filtrar
#tots aquells individus que contenen NAs en algun dels atributs juntament
#amb el coneixement que tenim sobre la nostra base de dades, podem establir
#un patró en els NAs d'alguns individus (llocs). Veiem que aquells llocs
#que no tenen first_review (sigui per què acaben de començar o alguna altra raó)
#tampoc tenen last_review, ni reviews_score rating, ni reviews_per_month, com és lògic.
#Al ser un patró no aleatori causat per la pròpia construcció de les dades
#podem afirmar que aquests individus contenen NAs de la forma MNAR de manera que
#eliminem aquests individus.

data_mnar <- data[!complete.cases(data[,c(23,24,25,28)]),]  #Aquesta és la base de dades en qüestió.
#data_not_mnar és la base de dades resultant d'eliminar les MNAR.
data_not_mnar <- data[complete.cases(data[,c(23,24,25,28)]),]

#D'aquesta base de dades primer tractem els NAs en categòriques. Si ens
#fixem en la BD veiem que les columnes de text (descripcions) contenen
#força NAs però no ens molesta ja que no les usarem per fer models. Les
#que són classes o categories reemplaçarem els NA per una nova classe anomenada
#"unknown", per tal que ens permeti realitzar models.

library(tidyr)
data_not_mnar <- data_not_mnar %>% replace_na(list(h_loc = "unknown",h_res_t="unknown"))

#IMPUTACIÓ DE LES VARIABLES NUMÈRIQUES MAR

data_not_mnar_num <- data_not_mnar[sapply(data_not_mnar,is.numeric)]

#Després d'eliminar els individus que tenien MNAR i com que només estem
#mirant variables numèriques, veient els gràfics abans mostrats, les úniques
#variables numèriques amb missings per a imputar són: h_res_r,h_acc_r,bth_txt,price.

#IMPUTACIÓ AMB MIMMI
library(ggplot2)
library(plyr)
source("C:/Users/alexc/OneDrive/Escriptori/PMAAD/1MIMMIK.R")

data_num_imputed <- MiMMi(data_not_mnar_num)

#Veient el dendograma i sabent que una k alta ens aporta més precisió
#escollim un total de k = 6 clusters.

#Taula dels valors usats per a imputar. Si executem es veuen els valors
#usats per a cada columna i per a cada cluster. Els individus amb NAs 
#classificats en cadascun d'aquests clusters adoptaran els valors corresponents
#seguint aquesta mesura de semblança.

data_num_imputed$imputation

#Imputed Dataset

data_num_imp_MiMMi <- data_num_imputed$imputedData 

#IMPUTACIÓ AMB MICE

library(mice)
library(VIM)

#Veiem quin patró segueixen els missings que tenim. Surt el mateix
#que hem vist abans.

md.pattern(data_not_mnar_num)
mice_plot <- aggr(data_not_mnar_num, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(data_not_mnar_num), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Imputació com a tal. Agafem els valors per defecte.
data_num_imp_mice <- mice(data_not_mnar_num, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(data_num_imp_mice)

# Aquest plot serveix per visualitzar a on han quedat els valors imputats
#dins del total. Per a major descripció fer-ho per totes les vars que tenen NAs
#que imputarem.

stripplot(data_num_imp_mice, price, pch = 19, xlab = "Imputation number")
data_num_imp_mice$imp$price  #evolució dels diferents NA a imputar

data_num_imp_mice <- mice::complete(data_num_imp_mice, 2)


#COMPARACIÓ DE DISTRIBUCIONS ABANS I DESPRÉS DEL TRACTAMENT DE MISSINGS PER ALS DOS MÈTODES
#Per diferents variables: en verd el dataset original, en vermell sense
#les files amb MNAR i en blau sense files amb MNAR i els MAR imputats amb MIMMI.

#price

#Dataset amb els valors de price per als diferents Df

a = data.frame(group = "data", price = data$price)
b = data.frame(group = "data_not_mnar", price = data_not_mnar$price)
c = data.frame(group = "data_num_imp_MiMMi", price = data_num_imp_MiMMi$price)
d = data.frame(group = "data_num_imp_mice", price = data_num_imp_mice$price)
plot.data = rbind(a, b, c, d)

#Mitjana de la variable en cadascun dels df.

mu <- ddply(plot.data, "group", summarise, grp.mean=mean(price,na.rm=TRUE))

#Fem tres histogrames en un mateix plot per a veure les diferències en les
#tres fases d'imputació. Les línies discontinues mostren la de cadascuna
#de les distribucions que, en aquest cas, estan gairebé superposades de manera
#que podem dir que la imputació no ha alterat la distribució de les variables.


ggplot(plot.data, aes(x=price, color=group)) +
  geom_histogram(fill="white", position="dodge")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")+
  theme(legend.position="top")

#Boxplot de totes per veure diferències

ggplot(plot.data, aes(x=group, y=price, fill=group)) +  
  geom_boxplot(outlier.shape = 1, outlier.color = "grey")


#h_res_r

a = data.frame(group = "data", h_res_r = data$h_res_r)
b = data.frame(group = "data_not_mnar", h_res_r = data_not_mnar$h_res_r)
c = data.frame(group = "data_num_imp_MiMMi", h_res_r = data_num_imp_MiMMi$h_res_r)
d = data.frame(group = "data_num_imp_mice", h_res_r = data_num_imp_mice$h_res_r)
plot.data = rbind(a, b, c, d)

mu <- ddply(plot.data, "group", summarise, grp.mean=mean(h_res_r,na.rm=TRUE))


ggplot(plot.data, aes(x=h_res_r, color=group)) +
  geom_histogram(fill="white", position="dodge")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")+
  theme(legend.position="top")

#Boxplot de totes per veure diferències

ggplot(plot.data, aes(x=group, y=h_res_r, fill=group)) +  
  geom_boxplot(outlier.shape = 1, outlier.color = "grey")

#h_acc_r

a = data.frame(group = "data", h_acc_r = data$h_acc_r)
b = data.frame(group = "data_not_mnar", h_acc_r = data_not_mnar$h_acc_r)
c = data.frame(group = "data_num_imp_MiMMi", h_acc_r = data_num_imp_MiMMi$h_acc_r)
d = data.frame(group = "data_num_imp_mice", h_acc_r = data_num_imp_mice$h_acc_r)
plot.data = rbind(a, b, c, d)

mu <- ddply(plot.data, "group", summarise, grp.mean=mean(h_acc_r,na.rm=TRUE))


ggplot(plot.data, aes(x=h_acc_r, color=group)) +
  geom_histogram(fill="white", position="dodge")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")+
  theme(legend.position="top")

#Boxplot de totes per veure diferències

ggplot(plot.data, aes(x=group, y=h_acc_r, fill=group)) +  
  geom_boxplot(outlier.shape = 1, outlier.color = "grey")

#bth_txt

a = data.frame(group = "data", bth_txt = data$bth_txt)
b = data.frame(group = "data_not_mnar", bth_txt = data_not_mnar$bth_txt)
c = data.frame(group = "data_num_imp_MiMMi", bth_txt = data_num_imp_MiMMi$bth_txt)
d = data.frame(group = "data_num_imp_mice", bth_txt = data_num_imp_mice$bth_txt)
plot.data = rbind(a, b, c, d)

mu <- ddply(plot.data, "group", summarise, grp.mean=mean(bth_txt,na.rm=TRUE))


ggplot(plot.data, aes(x=bth_txt, color=group)) +
  geom_histogram(fill="white", position="dodge")+
  geom_vline(data=mu, aes(xintercept=grp.mean, color=group),
             linetype="dashed")+
  theme(legend.position="top")

#Boxplot de totes per veure diferències

ggplot(plot.data, aes(x=group, y=bth_txt, fill=group)) +  
  geom_boxplot(outlier.shape = 1, outlier.color = "grey")




#Tots els gràfics vistos anteriorment manifesten que la diferència entre
#les distribucions obtingudes havent imputat respecte de les dades originals és més
#gran fent imputació amb MiMMi que amb Mice. Així doncs, en l'apartat visual podem
#concloure que les dades es veuen menys distorsionades realitzant imputació amb Mice.

#TEST DE KOLMOGOROV SMIRNOV

#Anem a corroborar aquest anàlisi amb el test estadístic de Kolmogorov-Smirnov
#que ens permet dir si dues distribucions poden ser considerades iguals,
#sense diferències significants o no.

ks.test(data$price,data_num_imp_mice$price)  
#p = 0.4355, D = 0.0185. On D és la major diferència absoluta trobada entre les dues
#distribucions
ks.test(data$price,data_num_imp_MiMMi$price)
#D = 0.016297, p-value = 0.6051. En aquest cas la diferència és menor en el
#cas de MiMMi

ks.test(data$h_res_r,data_num_imp_mice$h_res_r)
ks.test(data$h_res_r,data_num_imp_MiMMi$h_res_r) 
#En aquest cas veiem que el test fet per a la imputació amb MiMMi
#dona un p-valor < 0.05 de manera que podem dir que les dades originals
#i les imputades amb Mimmi no segueixen la mateixa distribució.


ks.test(data$h_acc_r,data_num_imp_mice$h_acc_r)
ks.test(data$h_acc_r,data_num_imp_MiMMi$h_acc_r) #El mateix que abans


ks.test(data$bth_txt,data_num_imp_mice$bth_txt)
ks.test(data$bth_txt,data_num_imp_MiMMi$bth_txt) 
#No hi ha diferències però Mice obté una diferència major absoluta menor.

#Podem afirmar que el mètode d'imputació Mice per a les nostres dades obté
#uns resultats més satisfactoris en quant a semblança amb les dades originals que no
#pas amb MiMMi de manera que aquestes dades imputades són amb les que treballarem.


data_not_mnar[,c(8,9,18,19)] <- data_num_imp_mice[,c(1,2,6,7)]
data_clean <- data_not_mnar
setwd("C:/Users/alexc/OneDrive/Escriptori/PMAAD/")
write.csv(data_clean, file = "Dataset_clean.csv", row.names = FALSE)

data_clean_num <- data_clean[sapply(data_clean,is.numeric)]
sum(is.na(data_clean_num)) #0