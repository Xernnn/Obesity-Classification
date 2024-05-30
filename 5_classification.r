library(ggplot2)
library(dplyr)

# Change the base directory here:
base_path <- "C:/Users/stdso/Documents/USTH/Spatial/Final Spatial"

file <- "obesity_classification.csv"
file_path <- file.path(base_path, file)
data <- read.csv(file_path)

# Check for missing values
if(sum(is.na(data)) > 0) {
  data <- na.omit(data) 
} else {
  print("No missing values in the dataset.")
}

# Improved Histogram for Age with Density Curve
age_plot <- ggplot(data, aes(x=Age)) +
  geom_histogram(aes(y=..density..), binwidth=5, fill="skyblue", color="black") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Distribution of Age with Density Curve") +
  xlab("Age") +
  ylab("Density") +
  theme_minimal()

# Categorize age into ranges
data$AgeRange <- cut(data$Age, breaks=c(0, 18, 30, 40, 50, 60, 70, 100),
                     labels=c("0-18", "19-30", "31-40", "41-50", "51-60", "61-70", "71+"))

# BMI Comparison in Age Ranges for Both Genders
bmi_age_plot <- ggplot(data, aes(x=AgeRange, y=BMI, fill=Gender)) +
  geom_boxplot() +
  ggtitle("BMI Comparison by Age Range and Gender") +
  xlab("Age Range") +
  ylab("BMI") +
  scale_fill_manual(values=c("blue", "pink")) +
  theme(axis.text.x = element_text(angle=45, hjust=1))

# BMI and Physical Activity Level
bmi_pa_plot <- ggplot(data, aes(x=as.factor(PhysicalActivityLevel), y=BMI, fill=as.factor(PhysicalActivityLevel))) +
  geom_boxplot() +
  ggtitle("BMI by Physical Activity Level") +
  xlab("Physical Activity Level") +
  ylab("BMI") +
  scale_fill_brewer(palette="Pastel1") +
  theme_minimal()

print(age_plot)
print(bmi_age_plot)
print(bmi_pa_plot)
