setwd("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD")


# MULTIVARIATE OUTLIER DETECTION WITH MAHALANOBIS 

# Load required packages
library(stats)
library(ggplot2)
library(naniar)
library(patchwork)


data<-read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/Dataset_clean.csv", stringsAsFactors = FALSE)
data$X <- NULL
data$ln <- NULL
data$lt <- NULL

# Extract only the numeric columns
num_data <- data[, sapply(data, is.numeric)]

# Create a list of boxplot plots for each numeric variable
plots <- lapply(1:ncol(num_data), function(i) {
  p <- ggplot(num_data, aes(y = num_data[, i])) +
    geom_boxplot(color = "black", fill = "gray", outlier.color = "red", outlier.shape = 1) +
    labs(y = names(num_data)[i]) +
    theme_classic()
  return(p)
})

# Arrange the boxplot plots in a grid
grid <- wrap_plots(plots, ncol = 3)

# Display the grid with outliers
grid
# -------------------------- Mahalanobis distance study -----------------------------
# Compute Mahalanobis distance of each row
mahalanobis_dist <- mahalanobis(num_data, colMeans(num_data), cov(num_data), tol=1e-20)

# Find the threshold for outlier detection using chi-square distribution
threshold <- qchisq(0.99, df = ncol(num_data))
threshold <- 125

# Identify outliers
outliers <- which(mahalanobis_dist > threshold)

# Print number of outliers detected
cat("Number of outliers detected: ", length(outliers), "\n")

# Plot the Mahalanobis distances
plot(mahalanobis_dist, pch=16, main="Mahalanobis Distances Plot")

# Add a line to indicate the outlier threshold
abline(h = threshold, col = "red", lwd = 1.2)

outliers

rows_with_outliers <- num_data[outliers, ]

# --------------------- Replace extreme values for NA in data ----------------------
data[409,c('rws_m')] <- NA
data[948,c('min_navg')] <- NA 
data[1206,c('min_navg')]<- NA
data[1592,c('max_navg')]<- NA
data[1608,c('rws_m')]<- NA
data[3165,c('bth_txt', 'h_lst_c')]<- NA
data[3510,c('h_res_r', 'r_sco_rt')]<- NA
data[3628,c('max_navg')]<- NA
data[3653,c('rws_m')]<- NA
data[3762,c('h_acc_r')]<- NA

# --------------------- Impute NAs with MICE in data extreme values for NA ----------------------

library(mice)
library(VIM)
num_data <- data[, sapply(data, is.numeric)]
rows_with_NA <- num_data[outliers, ]


num_data_not_out <- mice(num_data, m=5, maxit = 50, method = 'pmm', seed = 500)
num_data_not_out <- complete(num_data_not_out, 2)
rows_without_outliers <- num_data_not_out[outliers, ]


data[, sapply(data, is.numeric)] <- num_data_not_out
data_clean  <- data
setwd("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets")
write.csv(data_clean, file = "Dataset_clean_clean.csv", row.names = FALSE)

# Check Statistics of distributions
summary(num_data_not_out)

# --------------------- Plot Boxplots after imputation of outliers ----------------------
library(patchwork)

# Agafem dades de les variables numèriques ja tractades a data
numeric_data <- data_clean[, sapply(data, is.numeric)]

# Create a list of boxplot plots for each numeric variable
plots <- lapply(1:ncol(numeric_data), function(i) {
  p <- ggplot(numeric_data, aes(y = numeric_data[, i])) +
    geom_boxplot(color = "black", fill = "gray", outlier.color = "red", outlier.shape = 1) +
    labs(y = names(numeric_data)[i]) +
    theme_classic()
  return(p)
})

# Arrange the boxplot plots in a grid
grid <- wrap_plots(plots, ncol = 3)

# Display the grid with outliers
grid

# --------------------- Plot Final mahalanobis distance again and see the big samples are closer than 90 ----------------------

# Extract only the numeric columns
num_data <- data_clean[, sapply(data, is.numeric)]

# Compute Mahalanobis distance of each row
mahalanobis_dist <- mahalanobis(num_data, colMeans(num_data), cov(num_data), tol=1e-20)

# Find the threshold for outlier detection using chi-square distribution
threshold <- 90

# Identify outliers
outliers <- which(mahalanobis_dist > threshold)

# Print number of outliers detected
cat("Number of outliers detected: ", length(outliers), "\n")

# Plot the Mahalanobis distances
plot(mahalanobis_dist, pch=16, main="Mahalanobis Distances Plot")


