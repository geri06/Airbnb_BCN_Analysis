library(knitr) 
library(kableExtra) 
library(DT)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(wordcloud)
library(pals)
library(SnowballC)
library(lda)
library(ldatuning)
library(flextable)

DTM <- read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/term_doc_matrix.csv", stringsAsFactors = FALSE, encoding =' UTF-8')

# Troebm nombre òptim de topics
result <- ldatuning::FindTopicsNumber(
  DTM,
  topics = seq(from = 5, to = 30, by = 2),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
FindTopicsNumber_plot(result) # 11

K <- 17

# set seed
set.seed(9161)
# compute the LDA model, Gibbs sampling 500 iter
topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25))

# have a look a some of the results (posterior distributions)
tmResult <- posterior(topicModel)

# format of the resulting object
attributes(tmResult)

# topics are probability distributions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)                # K distributions over nTerms(DTM) terms

# for every document we have a probability distribution of its contained topics
theta <- tmResult$topics 
dim(theta)               # nDocs(DTM) distributions over K topics

terms(topicModel, 10)

exampleTermData <- terms(topicModel, 10)
exampleTermData[, 1:K]

top5termsPerTopic <- terms(topicModel, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
topicNames # Names to Topics 

# --------------------------- Visualization-------------------------
# See terms of topics 
ap_topics <- tidy(topicModel, matrix = "beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

exampleIds <- c(1, 14, 21)
N <- length(exampleIds)
# get topic proportions form example documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

# We have to make changes
attr(topicModel, "alpha") 
topicModel2 <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25, alpha = 1.5))
tmResult <- posterior(topicModel2)
theta <- tmResult$topics
beta <- tmResult$terms
topicNames <- apply(terms(topicModel2, 5), 2, paste, collapse = " ")

# View most influent words in each topicmodel2
ap_topics <- tidy(topicModel2, matrix = "beta")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# get topic proportions form example documents for topic2
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

top5termsPerTopic2 <- terms(topicModel2, 5)
topicNames <- apply(top5termsPerTopic2, 2, paste, collapse=" ")
topicNames # 
# ---------------------- Postprocessing step on topicmodel2 ----------------------

doc_topic <- posterior(topicModel2)$topics
# Calculate the correlation matrix between topics and documents
correlation <- cor(doc_topic)
# Print the correlation matrix
print(correlation)

# ------- Wordclouds------------
library(RColorBrewer)
n_terms <- 20
# visualize topics as word cloud
topicToViz <- 5 # change for your own topic of interest
# select to 20 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:n_terms]
words <- names(top40terms)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:n_terms]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

# --------- Topic Ranking ---------- 

topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
nDocs <- nrow(DTM)
countsOfPrimaryTopics <- rep(0, K)
names(countsOfPrimaryTopics) <- topicNames
for (i in 1:nDocs) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  countsOfPrimaryTopics[primaryTopic] <- countsOfPrimaryTopics[primaryTopic] + 1
}
sort(countsOfPrimaryTopics, decreasing = TRUE)
so <- sort(countsOfPrimaryTopics, decreasing = TRUE)
paste(so, ":", names(so))
#----------------- Add topics to data ----------------------

# Create list with all topics document by document

topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
nDocs <- nrow(DTM)
topics <- c()
for (i in 1:nDocs) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1] 
  topics <- c(topics, primaryTopic) # add index
  #topics <- c(topics, topicNames[primaryTopic]) add name
}

print(topics)
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")

data <- read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/data_reviews_merge.csv", stringsAsFactors = FALSE, encoding =' UTF-8')

# Replace reviews column with topics column 
data$comments <- topics
colnames(data)[colnames(data) == "comments"] <- "topics"

data$topics <- factor(data$topics)

# -------- Visualize interesting insights ---------

# Topics and scores
library(ggplot2)
library(dplyr)
library(hrbrthemes)

data %>%
  mutate(type = ifelse(topics == "5", "Highlighted", ifelse(topics == "16", "Topic16", ifelse(topics == "1", "Topic1", ifelse(topics %in% c("2", "7", "13"), "Topic2,7,13", "Normal"))))) %>%
  ggplot(aes(x = topics, y = r_sco_rt, fill = type, alpha = type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#FFB3B3", "grey", "#B3FFB3", "#B3FFB3", "#B3FFB3")) +
  scale_alpha_manual(values = c(1, 0.1, 1, 1, 1)) +
  theme(legend.position = "none") +
  labs(x = "Topic", y = "Score Rating", title = "Score Rating vs. Topic")



topicNames[5]
topicNames[1]
topicNames[16]
lda::top.topic.words(beta, 30, by.score = T)

mean_topic5 <- data %>%
  filter(topics == "5") %>%
  summarise(mean_score_rating = mean(r_sco_rt))

mean_topic16 <- data %>%
  filter(topics == "16") %>%
  summarise(mean_score_rating = mean(r_sco_rt))

mean_topic1 <- data %>%
  filter(topics == "1") %>%
  summarise(mean_score_rating = mean(r_sco_rt))

print(mean_topic5$mean_score_rating)
print(mean_topic16$mean_score_rating)
print(mean_topic1$mean_score_rating)

# Topics and price
data %>%
  mutate(type = ifelse(topics == "5", "Highlighted", ifelse(topics == "16", "Topic16", ifelse(topics == "1", "Topic1", ifelse(topics %in% c("2", "7", "13"), "Topic2,7,13", "Normal"))))) %>%
  ggplot(aes(x = topics, y = price, fill = type, alpha = type)) +
  geom_boxplot() +
  scale_fill_manual(values = c("grey", "grey", "grey", "grey", "grey")) +
  scale_alpha_manual(values = c(1, 0.1, 1, 1, 1)) +
  theme(legend.position = "none") +
  labs(x = "Topic", y = "Price", title = "Price vs. Topic")

# ----------------- Final word clouds ------------

# Extract the top 20 words for each topic, sorted by score
top_words <- lda::top.topic.words(beta, 30, by.score = TRUE)
# Define the topics of interest
# Filter the top words for the selected topics
filtered_words_5 <- top_words[,5]

# Wordclouds of reranked terms topic 5 -------------

library(RColorBrewer)
n_terms <- 30
# visualize topics as word cloud
topicToViz <- 5 # change for your own topic of interest
# select to 20 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:n_terms]
words <- unlist(filtered_words_5)
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:n_terms]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)

# Wordclouds of reranked terms topic positius -------------
# Extract the top 20 words for each topic, sorted by score
top_words <- lda::top.topic.words(beta, 30, by.score = TRUE)
# Define the topics of interest
# Filter the top words for the selected topics
filtered_words_1 <- top_words[,1]
filtered_words_2 <- top_words[,2]
filtered_words_7 <- top_words[,7]
filtered_words_13 <- top_words[,13]
filtered_words_16 <- top_words[,16]


n_terms <- 20
# visualize topics as word cloud
topicToViz <- 16 # change for your own topic of interest
# select to 20 most probable terms from the topic by sorting the term-topic-probability vector in decreasing order
top40terms <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:n_terms]
words <- unlist(filtered_words_16)# change for your own topic of interest
# extract the probabilites of each of the 40 terms
probabilities <- sort(tmResult$terms[topicToViz,], decreasing=TRUE)[1:n_terms]
# visualize the terms as wordcloud
mycolors <- brewer.pal(8, "Dark2")
wordcloud(words, probabilities, random.order = FALSE, color = mycolors)