setwd("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD")


library(dplyr)
library(gridExtra)
library(ggplot2)


data_clust <- read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/Clustered_DB.csv", stringsAsFactors = FALSE)

data_clust[, c('X', 'description', 'h_abt', 'lst_r', 'fst_r', "n_ovw", 'h_since', 'lt','ln', 'cluster_HC_2', 'cluster_CURE_2','cluster_CURE_3',"dbscan_4","source","h_loc", "n_gr_cl","gender" )] <- NULL

names(data_clust)[19] <- "cluster"

data_clust$cluster <- as.factor(data_clust$cluster)

# Ordenem primer numèriques després categriques

# Extract columns of each data type into separate data frames
num_cols <- sapply(data_clust, is.numeric)
cat_cols <- sapply(data_clust, is.character) | sapply(data_clust, is.logical)
fac_cols <- sapply(data_clust, is.factor)
num_data <- data_clust[, num_cols]
cat_data <- data_clust[, cat_cols]
fac_data <- data_clust[, fac_cols]

# Rename the data frames to match the original variable names
names(num_data) <- names(data_clust)[num_cols]
names(cat_data) <- names(data_clust)[cat_cols]
names(fac_data) <- names(data_clust)[fac_cols]

# Concatenate the data frames in the desired order
data_clust <- cbind(num_data, cat_data, fac_data)
names(data_clust)[19] <- "cluster"


# Generem la llista de plots
generate_histogram_list <- function(data, clusters, variables) {
  result_list <- list()
  for (clust in clusters) {
    for (variable in variables) {
      data_subset <- filter(data, cluster == clust)
      variable_type <- class(data_subset[[variable]])
      if (variable_type %in% c("numeric", "integer")) {
        result_list[[length(result_list) + 1]] <- ggplot(data_subset, aes(x = !!as.name(variable))) + 
          geom_histogram() + 
          labs(x = NULL, y = NULL) +
          theme(axis.text.y = element_blank()) +
          scale_x_continuous(breaks = c(min(data_subset[[variable]]), max(data_subset[[variable]])))
        
      } else if (variable_type == "character" || variable_type == "logical") {
        result_list[[length(result_list) + 1]] <- ggplot(data_subset, aes(x = !!as.name(variable))) + geom_bar() + labs(x = NULL, y = NULL) + theme(axis.text.y = element_blank())
      }
    }
  }
  return(result_list)
}

histogram_list <- generate_histogram_list(data_clust, levels(data_clust$cluster), names(data_clust)[-ncol(data_clust)])




# Create a matrix with plots
nrows <- length(levels(data_clust$cluster))
ncols <- ncol(data_clust) - 1
histo_matrix <- matrix(nrow = nrows, ncol = ncols)
histo_matrix[] <- histogram_list

# Create a plot with the matrix
grid.arrange(grobs = histo_matrix, ncol = ncols, nrow = nrows)



# Access the data from the first plot
# Calculate the means of the histograms
mean_list <- list()
for (i in seq_along(histogram_list)) {
  plot_data <- ggplot_build(histogram_list[[i]])
  x_data <- plot_data$data[[1]]$x
  y_data <- plot_data$data[[1]]$count
  mean_list[[i]] <- mean(x_data * y_data) / mean(y_data)
}

# Print the means
print(mean_list)
histogram_list[[12]]
