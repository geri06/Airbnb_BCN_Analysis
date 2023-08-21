
library("factoextra")
library(FactoMineR)

data <- read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/data_reviews_merge.csv", stringsAsFactors = FALSE, encoding =' UTF-8')
tdm <- read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/term_doc_matrix.csv", stringsAsFactors = FALSE, encoding =' UTF-8')

# Add variables of interest in Fact Analiysis
tdm$n_gr_cl <- data$n_gr_cl
tdm$s_host <- data$s_host
tdm$h_loc <- data$h_loc
tdm$h_res_t <- data$h_res_t
tdm$r_type <- data$r_type

?CA
res.ca<-CA(tdm[,1:1197], graph=FALSE)


summary(res.ca)


# Choose dimensions 
fviz_screeplot(res.ca, addlabels = TRUE, ylim = c(0, 5))

########## HIGHER CONTRIBUTIONS OF THE words in the dimensions. 
res.ca$col$contr[order(apply(res.ca$col$contr[,1:2],1,sum),decreasing=TRUE)[1:10],1:2]
res.ca$col$contr[order(apply(res.ca$col$contr[,3:4],1,sum),decreasing=TRUE)[1:10],3:4]


######### Plots
?plot.CA

# Dims 1 and 2
plot.CA(res.ca,invisible="row")
plot.CA(res.ca,invisible="col")

# Dims 3 and 4
plot.CA(res.ca,invisible="row",axes=c(3,4))
plot.CA(res.ca,invisible="col",axes=c(3,4))


########## PERFORM CORRESPONDENCE ANALYSIS ON GENERALISED AGGREGATED LEXICAL TABLE (CA-GALT)
?CaGalt
res.cagalt<-CaGalt(Y=tdm[,1:1197],X=tdm[,1198:1202],type="n")

########## EIGENVALUES
res.cagalt$eig

########## QUALITATIVE VARIABLES
names(res.cagalt$quali.var)


coordinates <- res.cagalt$quali.var$coord[, (1:6)]
cos2 <- res.cagalt$quali.var$cos2[,(1:6)]
table_data_coord <- data.frame(Variable = rownames(coordinates), coordinates)
table_data_cos2 <- data.frame(Variable = rownames(cos2), cos2)

specific_location <- "C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets"
output_file <- file.path(specific_location, "table_data_coord.csv")
write.csv(table_data_coord, file = output_file, row.names = FALSE)

specific_location <- "C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets"
output_file <- file.path(specific_location, "table_data_cos2.csv")
write.csv(table_data_cos2, file = output_file, row.names = FALSE)


########## SUMMARY
summary(res.cagalt)

########## WORDS
names(res.cagalt$freq)
res.cagalt$freq$coord
res.cagalt$freq$cos2
res.cagalt$freq$contr
table <- res.cagalt$freq$coord[order(apply(res.cagalt$freq$coord[,1:4],1,sum),decreasing=TRUE)[1:10],1:4]
table <- data.frame(table)

specific_location <- "C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets"
output_file <- file.path(specific_location, "table_galt_freq_words.csv")
write.csv(table, file = output_file, row.names = TRUE)

########## CA-GALT PLOTS
?plot.CaGalt

# Dimms 1 and 2
plot.CaGalt(res.cagalt,choix="freq",axes=c(1,2),select = "coord 40", cex = 0.7)
plot.CaGalt(res.cagalt,choix="quali.var",axes=c(1,2), select = "coord 20", autoLab = 'yes')


# Dimms 3 and 4
plot.CaGalt(res.cagalt,choix="freq",axes=c(3,4),select = "contrib 50", autoLab="yes", cex = 0.8)
plot.CaGalt(res.cagalt,choix="quali.var",axes=c(3,4), autoLab="auto")


# Dimms 5 and 6
plot.CaGalt(res.cagalt,choix="freq",axes=c(5,6),select = "contrib 50", autoLab="yes", cex = 0.8)
plot.CaGalt(res.cagalt,choix="quali.var",axes=c(5,6))
