# Load necessary libraries
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

df <- df[-c(1,2),]
View(df)
df[2:192,2]
colnames(df)
rownames(df)[2]
summary(df)

# Check for Null values and No data
which(is.na(df))
sum(is.na(df))
df = df[df[2] != "No data", ] 
View(df)

# Extract function
extract_numeric_part <- function(string) {
  as.numeric(regmatches(string, regexpr("\\d+\\.\\d+", string)))
}

# Filter columns
year <- rev(c(1975:2016))
year

# Male
male_cols <- (df[1,] == "Male")
male_cols <- c(male_cols)
male_cols
male <- as.data.frame(df[2:192,male_cols])
for (x in 1:length(year))
  male[,x] <- sapply(male[,x], extract_numeric_part)
colnames(male) <- year
male <- rev(male)
View(male)

# Female
female_cols <- (df[1,] == "Female")
female_cols <- c(female_cols)
female_cols
female <- as.data.frame(df[2:192,female_cols])
for (x in 1:length(year))
  female[,x] <- sapply(female[,x], extract_numeric_part)
colnames(female) <- year
female <- rev(female)
View(female)

# Both genders
both_cols <- (df[1,] == "Both sexes")
both_cols <- c(both_cols)
both_cols
both <- as.data.frame(df[2:192,both_cols])
for (x in 1:length(year))
  both[,x] <- sapply(both[,x], extract_numeric_part)
colnames(both) <- year
both <- rev(both)
View(both)

# Draw graphs for genders
year <- rev(year)
xticks <- seq(1976, 2016, by=5)

# Male
male_mean <- colMeans(male[, 1:ncol(male)])
male_mean
plot(year, male_mean, main="% Obesity of Male from 1975 to 2016"
     ,xaxp= c(1976, 2016, 10) , xlab="Year", ylab="% Obesity", pch = 11)
lines(year, male_mean, type="l", col = "black")
axis(side=1, at=year, labels = FALSE)

# Female
female_mean <- colMeans(female[, 1:ncol(female)])
female_mean
plot(year, female_mean, main="% Obesity of Female from 1975 to 2016"
     ,xaxp= c(1976, 2016, 10) , xlab="Year", ylab="% Obesity", pch = 11)
lines(year, female_mean, type="l", col = "black")
axis(side=1, at=year, labels = FALSE)

# Both genders
both_mean <- colMeans(both[, 1:ncol(both)])
both_mean
plot(year, both_mean, main="% Obesity of Both Genders from 1975 to 2016"
     ,xaxp= c(1976, 2016, 10) , xlab="Year", ylab="% Obesity", pch = 11)
lines(year, both_mean, type="l", col = "black")
axis(side=1, at=year, labels = FALSE)

# All together
male_mean <- colMeans(male, na.rm = TRUE)
female_mean <- colMeans(female, na.rm = TRUE)

# Plotting
plot(year, male_mean, main = "% Obesity from 1975 to 2016", 
     xaxp = c(1976, 2016, 10), xlab = "Year", ylab = "% Obesity", 
     ylim = range(c(male_mean, female_mean)), col = "blue")
lines(year, male_mean, type = "l", col = "black")

points(year, female_mean, col = "red")
lines(year, female_mean, type = "l", col = "black")

axis(side=1, at=year, labels = FALSE)
legend("topleft", legend = c("female", "male"), col = c("red", "blue"), lty = 1)