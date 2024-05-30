library(countrycode)
library(readr)

# Change the base directory here:
base_path <- "C:/Users/stdso/Documents/USTH/Spatial/Final Spatial"

file <- "obesity_by_country.xlsx"
file_path <- file.path(base_path, file)
data <- read_excel(file_path, .name_repair = "minimal")
data <- data[-c(1,2),]
View(data)

which(is.na(data))
sum(is.na(data))
data = data[data[2] != "No data", ] 

gender_row <- data[1, ]

# Extract function
extract_numeric_part <- function(string) {
  as.numeric(regmatches(string, regexpr("\\d+\\.\\d+", string)))
}

# Filter columns
year <- rev(c(1975:2016))
year

# Define the continents
continents <- unique(countrycode::codelist$continent)

for (continent in continents) {
  countries <- countrycode::codelist$country.name.en[countrycode::codelist$continent == continent]
  continent_data <- data[data[[1]] %in% countries, ]
  
  continent_data <- rbind(gender_row, continent_data)
  
  for (gender in c("Male", "Female")) {
    gender_cols <- (continent_data[1,] == gender)
    gender_cols <- c(gender_cols)
    gender_data <- as.data.frame(continent_data[2:192,gender_cols])
    
    for (x in 1:length(year))
      gender_data[,x] <- sapply(gender_data[,x], extract_numeric_part)
    
    colnames(gender_data) <- year
    
    # Calculate means
    if (gender == "Male") {
      male_mean <- colMeans(gender_data, na.rm = TRUE)
    } else if (gender == "Female") {
      female_mean <- colMeans(gender_data, na.rm = TRUE)
    }
  }
  
  # Plotting both genders together
  plot(year, male_mean, main = paste("% Obesity from 1975 to 2016 in", continent), 
       xaxp = c(1976, 2016, 10), xlab = "Year", ylab = "% Obesity", 
       ylim = c(0, 35), col = "blue")
  lines(year, male_mean, type = "l", col = "blue")
  
  points(year, female_mean, col = "red")
  lines(year, female_mean, type = "l", col = "red")
  
  axis(side=1, at=year, labels = FALSE)
  legend("topleft", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1)
}
