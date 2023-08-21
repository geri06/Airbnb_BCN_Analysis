library(dplyr)
library(tm)
library(tidytext)
library(tidyverse)
library(wordcloud)
library(reshape2)
library(ROAuth)



## we load the data and select the following variables: id, disctrict, review score rating and comments
data_airbnb <- read.csv("reviews_preprocessed.csv", stringsAsFactors = FALSE, encoding =' UTF-8')
data_airbnb <- select(data_airbnb, 1,14,26,32)


## We load in order to use the afinn_lexicon which provides words punctuated between (-4,4), following its negativity/positivity
afinn_lexicon <- read.csv("lexico_afinn.en.es.csv", stringsAsFactors = FALSE, fileEncoding = "latin1") %>% 
  tibble::as_tibble()
afinn_lexicon <- afinn_lexicon[, c("Word", "Puntuacion")]

## For each word of the reviews we label if its positive or negative
airbnb_afinn <- 
  data_airbnb %>%
  unnest_tokens(input = "comments", output = "Word") %>%
  inner_join(afinn_lexicon, ., by = "Word") %>%
  mutate(Tipo = ifelse(Puntuacion > 0, "Positive", "Negative")) %>%
  rename("District" = n_gr_cl)

## Number of words from the ones that appear the most
count(airbnb_afinn,
      Word,
      sort = TRUE) 


## Plot of the words that appear the most
airbnb_afinn %>%
  count(Word, sort = TRUE) %>%
  filter(n > 3000) %>%
  mutate(Word = reorder(Word, n)) %>%
  ggplot(aes(Word, n)) +
  geom_text(aes(label=n), hjust= -0.2) +
  geom_col() +
  xlab(NULL) +
  coord_flip()+
  theme_minimal()



## Cloud with the most appearance words
airbnb_afinn%>%
  count(Word) %>%
  with(wordcloud(words=Word,
                 freq=n,
                 max.words = 250,
                 scale = c(3,1),
                 rot.per = 0.3,
                 random.order = FALSE,
                 colors=brewer.pal(6,"Dark2")))

##nube con las palabras más positivas y negativas
airbnb_afinn %>%
  count(Word,Tipo,sort=TRUE) %>%
  acast(Word~Tipo,value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("red","green"), 
                   max.words = 200,
                   title.size = 1)



## Number of words by district. Eixample with the most.
airbnb_afinn %>%
  count(District)



## Number of unique words by district. Eixample with the most
airbnb_afinn %>% 
  group_by(District) %>% 
  distinct(Word) %>% 
  count()


## for every district, most negative and positive words
tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

map(c("Positive", "Negative"), function(sentimiento) {
  airbnb_afinn %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(District) %>%
    count(Word, sort = T) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Word, n, fill = District) +
    geom_col() +
    facet_wrap("District", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = sentimiento) + 
    tema_graf
})


##Proportion positives/negatives
airbnb_afinn %>%
  count(District, Tipo) %>%
  group_by(District) %>%
  mutate(Proportion = n / sum(n)) %>%
  ggplot() +
  aes(District, Proportion, fill = Tipo) +
  geom_col() 

# We calculate the proportion values in order to extract more conclusions.
proportion <- airbnb_afinn %>%
  count(District, Tipo) %>%
  group_by(District) %>%
  mutate(Proportion = n / sum(n))
print(proportion)

 
## We calculate the mean punctuation for the reviews of the apartments and see the values in a plot
data_airbnb <-
  airbnb_afinn %>%
  group_by(id) %>%
  summarise(Puntuaction_review = mean(Puntuacion)) %>%
  left_join(data_airbnb, ., by = "id") %>% 
  mutate(Puntuaction_review = ifelse(is.na(Puntuaction_review), 0, Puntuaction_review))  %>%
rename("District" = n_gr_cl)

value_sentiment_analysis <- data_airbnb$Puntuaction_review

df <- data.frame(
  Group = rep(c("value_sentiment_analysis"), each = length(value_sentiment_analysis)),
  Valor = c(value_sentiment_analysis)
)

ggplot(df, aes(x = Valor, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(x = "Value", y = "Density") +
  ggtitle("Reviews rates we have calculated") +
  scale_fill_manual(values = c("red"))

## we escale the values between (0,5)
minimum <- min(data_airbnb$Puntuaction_review)
print(minimum)
maximum <- max(data_airbnb$Puntuaction_review)
print(maximum)

data_airbnb$normalized <- 5 * (data_airbnb$Puntuaction_review - minimum) / (maximum - minimum)


## review from the punctuations
data_airbnb %>%
  ggplot() +
  aes(District, normalized, fill = District) +
  geom_boxplot() +
  tema_graf

data_airbnb %>%
  ggplot() +
  aes(District, r_sco_rt, fill = District) +
  geom_boxplot() +
  tema_graf


## t-test in order to compare the distributions 
value_airbnb <- data_airbnb$r_sco_rt
value_sentiment_analysis <- data_airbnb$normalized
test_result <- t.test(value_airbnb, value_sentiment_analysis)

print(test_result)




## graph to compare distributions
df <- data.frame(
  Group = rep(c("value_airbnb", "value_sentiment_analysis"), each = length(value_airbnb)),
  Valor = c(value_airbnb, value_sentiment_analysis)
)
ggplot(df, aes(x = Valor, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(x = "Value", y = "Density") +
  ggtitle("Distributions comparison") +
  scale_fill_manual(values = c("blue", "red"))


## we see the correlation between both distributions
correlation <- cor(value_airbnb, value_sentiment_analysis)
print(correlation)

