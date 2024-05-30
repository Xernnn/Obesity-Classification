library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(countrycode)

# Change the base directory here:
base_path <- "C:/Users/stdso/Documents/USTH/Spatial/Final Spatial"

file <- "obesity_by_country.xlsx"
file_path <- file.path(base_path, file)
data <- read_excel(file_path, .name_repair = "minimal")

# Remove the first two rows if they are NA and not needed
data <- data[-c(1,2), ]

# Function to clean and make unique column names
clean_column_names <- function(names) {
  # Remove the .0 in year names
  clean_names <- gsub("\\.0", "", names)
  return(clean_names)
}

names(data) <- clean_column_names(names(data))
print(data)



