library(tm)
library(textstem)


data<- read.csv("data_reviews_merge.csv", stringsAsFactors = FALSE, encoding =' UTF-8')

data$comments[1]# Text without preprocessing

# PREPROCESSING TEXT 
corpus <- Corpus(VectorSource(data$comments))
# Minuscules
corpus <- tm_map(corpus, tolower)
# Puntuació
corpus <- tm_map(corpus, removePunctuation)
# Remove non-alphanumeric signs
corpus <- tm_map(corpus, removeNumbers)
# Remove stopwords
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
# Stemm the words
corpus <- tm_map(corpus, stemDocument, language = "english")
# lemmatization of the words
#corpus <- tm_map(corpus, content_transformer(lemmatize_strings))

corpus # check corpus

corpus[["1"]][["content"]]# Preprocessed example

## we preper the column of coments with the preprocessed ones
preprocessed_coments <- list()

for (i in 1:length(corpus)) {
  preprocessed_comment <- as.character(corpus[[i]])
  preprocessed_coments[[i]] <- preprocessed_comment
}

## we upload the new dataset with the comments preprocessed for the sentiment analysis
data$comments <- unlist(preprocessed_coments)
output_file <- file.path("reviews_preprocessed.csv")
write.csv(data, file = output_file, row.names = FALSE)


## we create the term matrix for the topic modeling
tdm <- TermDocumentMatrix(corpus)

# Remove less frequent terms from the term-document matrix
tdm <- removeSparseTerms(tdm, sparse = 0.95)  # Keep terms that occur in at least 1% of the documents
td.mat <- as.matrix(tdm)
td.mat[,1]
term_doc_matrix <- as.data.frame(t(td.mat))

output_file <- file.path("term_doc_matrix.csv")
write.csv(term_doc_matrix, file = output_file, row.names = FALSE)
