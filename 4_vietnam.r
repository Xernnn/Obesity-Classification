library(countrycode)
library(readr)

# Change the base directory here:
base_path <- "C:/Users/stdso/Documents/USTH/Spatial/Final Spatial"

file <- "obesity_by_country.xlsx"
file_path <- file.path(base_path, file)
data <- read_excel(file_path, .name_repair = "minimal")
data <- data[-c(1,2),]

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

# Filter the dataset for Vietnam
vietnam_data <- data[data[[1]] == "Viet Nam", ]

# Combine the first data row with the filtered dataset for Vietnam
vietnam_data <- rbind(gender_row, vietnam_data)

# Extract and process data for males and females in Vietnam
for (gender in c("Male", "Female")) {
  gender_cols <- (vietnam_data[1,] == gender)
  gender_cols <- c(gender_cols)
  gender_data <- as.data.frame(vietnam_data[2:192,gender_cols])
  
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

# Plotting both genders together for Vietnam
plot(year, male_mean, main = "% Obesity from 1975 to 2016 in Vietnam", 
     xaxp = c(1976, 2016, 10), xlab = "Year", ylab = "% Obesity", 
     ylim = range(c(male_mean, female_mean)), col = "blue")
lines(year, male_mean, type = "l", col = "blue")

points(year, female_mean, col = "red")
lines(year, female_mean, type = "l", col = "red")

axis(side=1, at=year, labels = FALSE)
legend("topleft", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1)
