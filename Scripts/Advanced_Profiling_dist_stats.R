setwd("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD")


# Load required packages
library(gridExtra)

#install.packages("gridExtra")


data<-read.csv("C:/Users/gerar/Documents/UNIVERSITAT/q2_2022_2023/PMAAD/Datasets/Dataset_clean_clean.csv", stringsAsFactors = FALSE)

# Define function to create histograms and basic statistics for numerical variables
hist_and_stats <- function(data) {
  # Get names of numerical variables
  num_vars <- names(data)[sapply(data, is.numeric)]
  
  # Loop over numerical variables
  for (var in num_vars) {
    # Create histogram
    hist(data[[var]], main = paste0("Histogram of ", var), xlab = var)
    
    # Create summary statistics
    stats <- summary(data[[var]])
    cat(paste0("Summary statistics for ", var, ":\n"))
    cat(paste0("  Mean: ", stats["Mean"], "\n"))
    cat(paste0("  Median: ", stats["Median"], "\n"))
    cat(paste0("  Minimum: ", stats["Min."], "\n"))
    cat(paste0("  Maximum: ", stats["Max."], "\n"))
    cat(paste0("  1st Quartile: ", stats["1st Qu."], "\n"))
    cat(paste0("  3rd Quartile: ", stats["3rd Qu."], "\n"))
    cat("\n")
  }
}
# Call function on your dataset
hist_and_stats(data)
