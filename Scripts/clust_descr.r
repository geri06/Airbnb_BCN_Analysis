# Carreguem els paquets necessaris
list.of.packages = c("pdftools", "tm", "SnowballC", "wordcloud", "ggplot2", "dplyr", 
                     "readr", "cluster", "ColorBrewer") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

library(stringr)
library(textcat)
library(tm)


## Llegim el dataset del que agafarem les descripcions
setwd("C:/Users/rions/UPC/Q4/PMAAD")
dd <- read.csv("C:/Users/rions/UPC/Q4/PMAAD/Dataset_clean_clean.csv", sep = ",")

dscr <- dd$description

## Visualitzem una part del texto
print(dscr[1:3], width = 30)

# Treballarem únicament amb descripcions que es trobin en un sol idioma, concretament, en anglès
dscr <- dscr[textcat::textcat(dscr) == "english"]
print(dscr[1:3], width = 30)

# Eliminem missing values 
dscr <- na.omit(dscr)


# ==============================================================================
# Neteja del text
## Eliminem símbols estranys com els <br/> o similars que ens trobem i el nombre de llicencia 
# ------------------------------------------------------------------------------
dscr <- str_remove_all(dscr, "License number")
print(dscr[1:3], width = 30)

dscr <- str_remove_all(dscr, "Exempt")
print(dscr[1:3], width = 30)

dscr <- str_replace_all(dscr, "\\b[A-Za-z]{2}-\\d+\\b", "")
print(dscr[1:3], width = 30)

dscr <- str_remove_all(dscr, "<.*?>")
print(dscr[1:3], width = 30)

dscr <- str_replace_all(dscr, "\\s+", " ")
print(dscr[1:3], width = 30)
# ------------------------------------------------------------------------------
# Passem tot el text a minúscules 
dscr <- tolower(dscr)
print(dscr[1:3], width = 30)

# ------------------------------------------------------------------------------
## Eliminem les stopwords: eliminar paraules buides, és a dir, aquelles amb 
## poc valor per al anàlisi, tals como algunes preposiciones.
dscr <- tm::removeWords(dscr, words = tm::stopwords("english"))
print(dscr[1:3], width = 30)

# ------------------------------------------------------------------------------
## Eliminem la puntuació
dscr <- tm::removePunctuation(dscr)
dscr <- gsub("â€”|Â¡|Â¿|", "", dscr)
print(dscr[1:3], width = 30)

# ------------------------------------------------------------------------------
## Eliminem els nombres
dscr <- tm::removeNumbers(dscr)
print(dscr[1:3], width = 30)

# ------------------------------------------------------------------------------
##  Eliminem espais buits excessius
dscr <- tm::stripWhitespace(dscr)
print(dscr[1:3], width = 30)

# Generem més depuració eliminant aquelles paraules que es repeteixen molt en totes les descripcions i, per tant, no aporten distinció entre elles
dscr <- tm::removeWords(dscr, words = c("apartment", "barcelona", "will","room"))
print(dscr[1:3], width = 30)

# La llargada del nostre vector de descripcions haurà de ser igual al nombre de documents que contingui el nostre corpus
length(dscr) 

# ==============================================================================
# Creem el corpus de les dades
nov_corpus = tm::Corpus(tm::VectorSource(dscr))
nov_corpus

# Creem Document Term Matrix, matriu mxn on m es el nombre de descripcions i n el nombre de termes existents 
dtm <- DocumentTermMatrix(nov_corpus)

# Eliminem termes esparsos, és a dir termes poc freqüents que no aporten característiques rellevants en la classificació de descripcions
nov_dtm <- tm::removeSparseTerms(dtm, sparse = .95)
dtm
nov_dtm

# Passem el Document Term Matrix amb les paraules rellevants a matriu per tal de poder fer operacions
matrix_nov_dtm <- as.matrix(nov_dtm)

# Calculem la matriu de distàncies
dist_matrix <- dist(matrix_nov_dtm, method = "cosine")

# Veiem com ha quedat el cluster jeràrquic
hclust_result <- hclust(dist_matrix, method = "ward.D2")
plot(hclust_result)

# Veiem que 3 és un nombre adequat de grups 
k <- 3 

# Obtenim les etiquetes dels clusters assignats a cada document
cluster_labels <- cutree(hclust_result, k)

#-------------------------------------------------------------------------------#
# Assignem les etiquetes al DTM i mostrem també un dataset de dues columnes per veure com aquestes han quedat classificades
dtm_with_clusters <- cbind(nov_dtm, cluster = cluster_labels)
dscr_clusterized <- cbind(dscr, cluster = cluster_labels)

# Dividim el DTM en subconjunts per cluster 
cluster_subsets <- split(nov_dtm,cluster_labels)

# Calculem la freqüencia de paraules en cada subconjunt 
word_freq <- lapply(cluster_subsets, function(subset) {
  colSums(as.matrix(subset))
})

# Generem un núvol de paraules per a cada cluster
library(wordcloud)

for (i in 1:length(word_freq)) {
  wordcloud(words = names(word_freq[[i]]),
            freq = word_freq[[i]],
            max.words = 50,
            random.order = FALSE,
            main = paste("Wordcloud Cluster", i))
}

