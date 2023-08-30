# Load necessary packages
library(dplyr)   # For data manipulation
library(tidyr)   # For data tidying
library(ggplot2) # For data visualization

 # For working with dates


# Load data from CSV
data <- read.csv("/Users/diyajames/Desktop/ias-release-main/ias-profile.csv", header = TRUE, stringsAsFactors = FALSE)
#data <- read.csv("C:/Users/anish/Downloads/ias-profile.csv", header = TRUE, stringsAsFactors = FALSE)

colnames(data) <- gsub("^\\s+|\\s+$", "", colnames(data))

# Examine the structure of the data
str(data)

# Data Cleaning
columns_to_remove <- c("Source", "Gender_Source","Date_of_Birth","Date_of_Joining","Source","Gender_Source","Last_Start_Date","Last_End_Date","Last_Education_Subject","Last_Office","Last_Field_of_Experience","Last_Category_of_Experience")
# Remove specified columns
data <- data[, !(names(data) %in% columns_to_remove)]
column_titles <- names(data)
print(column_titles)

# Convert date columns to proper date format
#data$Date_of_Birth <- as.Date(data$Date_of_Birth)
#data$Date_of_Joining <- as.Date(data$Date_of_Joining)
#data$Last_Start_Date <- as.Date(data$Last_Start_Date)
#data$Last_End_Date <- as.Date(data$Last_End_Date)
data$Retired <- as.integer(data$Retired)


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

#--------

# Create a subset for males and females
males <- data[data$Gender == "Male", ]
females <- data[data$Gender == "Female", ]

# Create tables for each gender
table_males <- table(males$Allotment_Year)
table_females <- table(females$Allotment_Year)

# Get unique years from both tables
unique_years <- sort(unique(c(names(table_males), names(table_females))))

# Create a data frame for the result
result_table <- data.frame(
  Allotment_Year = unique_years,
  Male = table_males[match(unique_years, names(table_males))],
  Female = table_females[match(unique_years, names(table_females))]
)

print(result_table)
result_table <- result_table[, !names(result_table) %in% c("Female.Var1", "Male.Var1")]
print(result_table)
result_table$Female.Freq[is.na(result_table$Female.Freq)] <- 0
print(result_table)

#result_table <- result_table[-(1:15), , drop = FALSE]
result_table <- result_table[-(1:27), , drop = FALSE]
print(result_table)
#----------


#shapiro test for frequency of male
subset_data <- result_table$Male.Freq
# Perform Shapiro-Wilk test for normality
shapiro_test_result <- shapiro.test(subset_data)
print(shapiro_test_result)

ggplot(result_table, aes(x = "", y = Male.Freq)) +
 geom_boxplot() +
labs(title = "Box Plot of Male frequency", y = "Male Frequency") +
theme_minimal()

#shapiro test for frequency of female
subset_data <- result_table$Female.Freq
# Perform Shapiro-Wilk test for normality
shapiro_test_result <- shapiro.test(subset_data)
print(shapiro_test_result)

ggplot(result_table, aes(x = "", y = Female.Freq)) +
  geom_boxplot() +
  labs(title = "Box Plot of Female Frequency", y = "Female Frequency") +
  theme_minimal()


#kruskal_test_result <- kruskal.test(result_table$Male.Freq, result_table$Female.Freq)
#print(kruskal_test_result)

#Cov test
covariance <- cov(result_table$Male.Freq, result_table$Female.Freq)
print(covariance)

#Correlation test
correlation <- cor(result_table$Male.Freq, result_table$Female.Freq)
print(correlation)

#Linear Model test
model <- lm(Female.Freq ~ Male.Freq, data = result_table)
# Print the model summary
summary(model)

#ANOVA test
one.way <- aov(Female.Freq ~ Male.Freq, data=result_table)
summary(one.way)
two.way <- aov(Allotment_Year ~ Female.Freq + Male.Freq, data= result_table)
summary(two.way)


#3-fold cross verification
# Split the data into three parts
set.seed(123)  # For reproducibility
n <- nrow(result_table)
indices <- sample(1:n, size = n)
subset_size <- n %/% 3

subset1 <- result_table[indices[1:subset_size], ]
subset2 <- result_table[indices[(subset_size + 1):(2 * subset_size)], ]
subset3 <- result_table[indices[(2 * subset_size + 1):n], ]

# Calculate covariance and correlation for each subset
covariance1 <- cov(subset1$Male.Freq, subset1$Female.Freq)
correlation1 <- cor(subset1$Male.Freq, subset1$Female.Freq)

covariance2 <- cov(subset2$Male.Freq, subset2$Female.Freq)
correlation2 <- cor(subset2$Male.Freq, subset2$Female.Freq)

covariance3 <- cov(subset3$Male.Freq, subset3$Female.Freq)
correlation3 <- cor(subset3$Male.Freq, subset3$Female.Freq)

# Print the results
cat("Subset 1:\n")
cat("Covariance:", covariance1, "\n")
cat("Correlation:", correlation1, "\n\n")

cat("Subset 2:\n")
cat("Covariance:", covariance2, "\n")
cat("Correlation:", correlation2, "\n\n")

cat("Subset 3:\n")
cat("Covariance:", covariance3, "\n")
cat("Correlation:", correlation3, "\n")
# Create a data frame for the results
results <- data.frame(
  Subset = c("Subset 1", "Subset 2", "Subset 3"),
  Covariance = c(covariance1, covariance2, covariance3),
  Correlation = c(correlation1, correlation2, correlation3)
)

# Print the results data frame
print(results)




#creating a new table with home and away postings
filtered_data <- subset(data, Place_of_Domicile != "-" & Place_of_Domicile != "Not Found")
print(filtered_data[, c("Allotment_Year","Place_of_Domicile", "Cadre")])

#library(dplyr)
# Create a new data frame with counts for home and away postings
posting_counts <- filtered_data %>%
  filter(Place_of_Domicile != "-" & Place_of_Domicile != "Not Found") %>%
  group_by(Allotment_Year) %>%
  summarize(
    Home_Postings = sum(Place_of_Domicile == Cadre),
    Away_Postings = sum(Place_of_Domicile != Cadre)
  )

posting_table <- posting_counts[-(1:27), , drop = FALSE]
# Print the posting counts
print(posting_table)


#Question: Is there a significant difference in the ages of individuals based on their place of domicile or mother tongue?
print(data)
group_a <- data %>%
  filter(Place_of_Domicile == "Karnataka") %>%
  mutate(Age = floor((as.numeric(Sys.Date() - as.Date(Date_of_Birth, format = "%Y-%m-%d")))/365.25))

group_b <- data %>%
  filter(Place_of_Domicile == "Bihar") %>%
  mutate(Age = floor((as.numeric(Sys.Date() - as.Date(Date_of_Birth, format = "%Y-%m-%d")))/365.25))

# Correct column names by removing leading spaces
View(group_a)
t_test_result <- t.test(group_a$Age, group_b$Age)

# Step 4: Calculate T-Statistic and P-Value
t_statistic <- t_test_result$statistic
p_value <- t_test_result$p.value
print(p_value)
# Step 5: Interpret Results
if (p_value < 0.05) {
  conclusion <- "Reject the null hypothesis. There is a significant difference in ages."
} else {
  conclusion <- "Fail to reject the null hypothesis. There is no significant difference in ages."
}

# Print results
cat("T-Statistic:", t_statistic, "\n")
cat("P-Value:", p_value, "\n")
cat("Conclusion:", conclusion, "\n")




