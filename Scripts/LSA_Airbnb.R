library(ggplot2)
library(lsa)
library(LSAfun)

td.mat <- read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/term_doc_matrix.csv", stringsAsFactors = FALSE, encoding =' UTF-8')
data <- read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/data_reviews_merge.csv", stringsAsFactors = FALSE, encoding =' UTF-8')

td.mat <- td.mat[1:3717,]
td.mat <- t(td.mat) # transpose matrix
td.mat.lsa <- lw_bintf(td.mat) * gw_idf(td.mat) # weighting
lsaSpace <- lsa(td.mat.lsa) # create LSA space
as.textmatrix(lsaSpace)
#EXPLORING RESULTS
results<- as.textmatrix(lsaSpace)
head(results)

# compare two terms with the cosine measure ##ALSO AVAILABLE FOR ALL TERMS
cosine(results["sagrada",], results["familia",])
cosine(results["reserv",], results["cancel",])
cosine(results["hostel",], results["dog",])
cosine(results["hotel",], results["beauti",])
cosine(results["hostel",], results["beauti",])

# compare two documents with pearson ### ALSO AVAILABLR FOR ALL DOCUMENTS
cor(results[,1], results[,14], method="pearson")
cor(results[,1], results[,21], method="pearson")


data$comments[1] 
data$comments[14]# text 14 0 correlation
data$comments[21]# text 21 17 correlation

# calc associations for words
associate(results, "gracia")
associate(results, "familia")
associate(results, "cancel")
associate(results, "clean")


###Library(LSAfun)
plot_neighbors("gracia", #single word
               n = 10, #number of neighbors
               tvectors = results, #matrix space
               method = "MDS", #PCA or MDS
               dims = 2) #number of dimensions

###Library(LSAfun)
plot_neighbors("sagrada", #single word
               n = 10, #number of neighbors
               tvectors = results, #matrix space
               method = "MDS", #PCA or MDS
               dims = 2) #number of dimensions

###Library(LSAfun)
plot_neighbors("host", #single word
               n = 10, #number of neighbors
               tvectors = results, #matrix space
               method = "MDS", #PCA or MDS
               dims = 2) #number of dimensions

list1 = c("nice", "clean", "dirti", "street",'room')

plot_wordlist(list1, #put in the list above 
              method = "MDS", 
              dims = 2, #pick the number of dimensions
              tvectors = results)

# No té sentit posar view1,2 i 3 random si no sabem de que van els texts
# Fem gràfic en 2d amb 3 clusters
text <- c(data$comments)
dist.mat.lsa <- dist(t(as.textmatrix(lsaSpace))) # compute distance matrix
dist.mat.lsa # check distance matrix for DOCUMENTS
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])


view <- factor(rep(c("view 1", "view 2", 'view 3'), each=1239))
df <- data.frame(text[1:3717], view, stringsAsFactors=FALSE)

ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y, color=df$view))

library(scatterplot3d)
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=3)
colors <- rep(c("blue", "green", "red"), each=1239)
scatterplot3d(fit$points[, 1], fit$points[, 2], fit$points[, 3], color=colors, pch=16, 
              main="Semantic Space Scaled to 3D", xlab="x", ylab="y", zlab="z", type="h")

dist.mat.lsa <- dist((as.textmatrix(lsaSpace))) # compute distance matrix
dist.mat.lsa # check distance matrix for TERMS
fit <- cmdscale(dist.mat.lsa, eig=TRUE, k=2)
points <- data.frame(x=fit$points[, 1], y=fit$points[, 2])
ggplot(points,aes(x=x, y=y)) + 
  geom_point(data=points,aes(x=x, y=y)) + 
  geom_text(data=points,aes(x=x, y=y-0.2, label=row.names(points)))

