library(tm)
library(ggplot2)
library(lsa)
library(LSAfun)
library(cld2)


data<- read.csv("reviews.csv", stringsAsFactors = FALSE, encoding =' UTF-8')
data <-  data[,c('listing_id','comments')]

# We have reviews in different idioms, we delete all reviews which are not in english
detect_language <- function(text) {
  result <- cld2::detect_language(text)
  return(result)
}

data$language <- sapply(data$comments, detect_language)

sort(table(data$language), decreasing = TRUE)

data <- data[data$language == "en", ]

# We drop missing values 
data <- na.omit(data)

# Drop Lenguage column 
data$language <- NULL

# Join comments by listing ID
aggregated_data <- aggregate(comments ~ listing_id, data, paste, collapse = " ")

# Rename the 'comments' column
colnames(aggregated_data) <- c("listing_id", "comments")

# Save dataset aggregated_data
output_file <- file.path("english_reviews.csv")
write.csv(aggregated_data, file = output_file, row.names = FALSE)


data<- read.csv("Dataset_clean_clean_id.csv", stringsAsFactors = FALSE, encoding =' UTF-8')

data_reviews <- merge(data, aggregated_data, by.x = "id", by.y = "listing_id")


output_file <- file.path("data_reviews_merge.csv")
write.csv(data_reviews, file = output_file, row.names = FALSE)





