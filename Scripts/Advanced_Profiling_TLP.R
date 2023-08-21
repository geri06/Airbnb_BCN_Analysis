setwd("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD")


library(dplyr)
library(gridExtra)
library(ggplot2)


data_clust <- read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/Clustered_DB.csv", stringsAsFactors = FALSE)

data_clust[, c('X', 'description', 'h_abt', 'lst_r', 'fst_r', "n_ovw", 'h_since', 'lt','ln', 'cluster_HC_2', 'cluster_CURE_2','cluster_CURE_3',"dbscan_4","source","h_loc", "n_gr_cl","gender" )] <- NULL

names(data_clust)[19] <- "cluster"

data_clust$cluster <- as.factor(data_clust$cluster)

# Ordenem per ordre grups TLP
# Define groups of variables
G1 <- c("h_res_t", "h_res_r", "h_acc_r", "s_host", "h_ver", "h_id_v","h_lst_c") # About Host
G2 <- c("r_type", "accs", "bth_txt", "amts_c") # About Apartment
G3 <- c("r_sco_rt", "rws_m") # About clients opinions
G4 <- c("price", "min_navg", "max_navg", "av_365","inst_bk", "cluster") # Sobre condicions de lloguer (l'última no conta, es pq estigui al final)

# Reorder columns by group
data_clust <- data_clust[, c(G1, G2, G3, G4)]


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
histogram_list[[7]]
