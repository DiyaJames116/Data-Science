# Load necessary packages
library(dplyr)   # For data manipulation
library(tidyr)   # For data tidying
library(ggplot2) # For data visualization

# Load data from CSV
data <- read.csv("/Users/diyajames/Desktop/ias-release-main/ias-profile.csv", header = TRUE, stringsAsFactors = FALSE)

# Correct column names by removing leading spaces
colnames(data) <- gsub("^\\s+|\\s+$", "", colnames(data))

# Examine the structure of the data
str(data)

# Data Cleaning
# Convert date columns to proper date format
data$Date_of_Birth <- as.Date(data$Date_of_Birth)
data$Date_of_Joining <- as.Date(data$Date_of_Joining)
data$Last_Start_Date <- as.Date(data$Last_Start_Date)
data$Last_End_Date <- as.Date(data$Last_End_Date)

# Data Summaries
# Summary statistics for numeric columns
summary_numeric <- summary(data %>% select(Allotment_Year))

# Frequency distribution of Cadre
cadre_freq <- table(data$Cadre)

# Visualization
# Bar plot for Cadre frequency distribution
cadre_freq_plot <- ggplot(data, aes(x = Cadre, fill = Cadre)) +
  geom_bar() +
  labs(title = "Frequency Distribution of Cadre", x = "Cadre", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")
data$time_difference_years <- as.numeric(difftime(data$Last_Start_Date, data$Allotment_Year, units = "days") / 365)

# Calculate correlation matrix for numeric columns
correlation_matrix <- cor(data %>% select(Allotment_Year, time_difference_years), use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)
# Correlation Analysis
correlation_matrix <- cor(data %>% select(Allotment_Year, Last_Start_Date))

# Grouping and Aggregation
cadre_stats <- data %>%
  group_by(Cadre) %>%
  summarise(
    Mean_Allotment_Year = mean(Allotment_Year, na.rm = TRUE),
    Median_Allotment_Year = median(Allotment_Year, na.rm = TRUE),
    Max_Allotment_Year = max(Allotment_Year, na.rm = TRUE),
    Min_Allotment_Year = min(Allotment_Year, na.rm = TRUE)
  )

# Print results and visualizations
print(summary_numeric)
print(cadre_freq)
print(correlation_matrix)
print(cadre_stats)

print(cadre_freq_plot)



subset_data <- data$Allotment_Year[1:5000]
# Perform Shapiro-Wilk test for normality
shapiro_test_result <- shapiro.test(subset_data)
print(shapiro_test_result)

ggplot(data, aes(x = "", y = Allotment_Year)) +
 geom_boxplot() +
labs(title = "Box Plot of Allotment Year", y = "Allotment Year") +
theme_minimal()

# Load required library for Kruskal-Wallis test
library(stats)

# Extract the relevant variable for the groups (assuming you have a factor variable)
groups <- data$Gender[1:5000]  # Replace "GroupColumn" with the actual column name

# Perform Kruskal-Wallis test
kruskal_test_result <- kruskal.test(subset_data, g = groups)

# Print the test result
print(kruskal_test_result)


