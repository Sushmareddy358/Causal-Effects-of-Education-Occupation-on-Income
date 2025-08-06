data <- read.csv("C:/Users/sushm/Downloads/sgdata.csv")
# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming 'data' is your dataset (if not, replace 'data' with the name of your dataset)

# Numerical summary statistics
summary_stats <- data %>%
  summarise(
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE),
    min_age = min(Age, na.rm = TRUE),
    max_age = max(Age, na.rm = TRUE),
    mean_income = mean(Income, na.rm = TRUE),
    median_income = median(Income, na.rm = TRUE),
    sd_income = sd(Income, na.rm = TRUE),
    min_income = min(Income, na.rm = TRUE),
    max_income = max(Income, na.rm = TRUE)
  )

# Display numerical summary stats
print("Numerical Summary Statistics:")
print(summary_stats)

# Categorical variable summaries (counts)
sex_count <- data %>% count(Sex)
marital_status_count <- data %>% count(Marital.status)
education_count <- data %>% count(Education)
occupation_count <- data %>% count(Occupation)
settlement_size_count <- data %>% count(Settlement.size)

# Display counts for categorical variables
print("Sex Distribution:")
print(sex_count)

print("Marital Status Distribution:")
print(marital_status_count)

print("Education Level Distribution:")
print(education_count)

print("Occupation Distribution:")
print(occupation_count)

print("Settlement Size Distribution:")
print(settlement_size_count)

# Histograms for Age and Income distribution
ggplot(data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black") +
  labs(title = "Age Distribution", x = "Age", y = "Frequency")

ggplot(data, aes(x = Income)) +
  geom_histogram(binwidth = 50000, fill = "lightgreen", color = "black") +
  labs(title = "Income Distribution", x = "Income", y = "Frequency")

# Bar plots for categorical variables

# Sex Distribution
ggplot(data, aes(x = factor(Sex))) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Sex Distribution", x = "Sex", y = "Count")

# Marital Status Distribution
ggplot(data, aes(x = factor(Marital.status))) +
  geom_bar(fill = "lightgreen", color = "black") +
  labs(title = "Marital Status Distribution", x = "Marital Status", y = "Count")

# Education Level Distribution
ggplot(data, aes(x = factor(Education))) +
  geom_bar(fill = "lightcoral", color = "black") +
  labs(title = "Education Level Distribution", x = "Education Level", y = "Count")

# Occupation Distribution
ggplot(data, aes(x = factor(Occupation))) +
  geom_bar(fill = "lightgoldenrodyellow", color = "black") +
  labs(title = "Occupation Distribution", x = "Occupation", y = "Count")

# Settlement Size Distribution
ggplot(data, aes(x = factor(Settlement.size))) +
  geom_bar(fill = "lightpink", color = "black") +
  labs(title = "Settlement Size Distribution", x = "Settlement Size", y = "Count")

# --- Relationships Between Variables ---
# 1. Boxplot of Income by Education Level
boxplot(Income ~ Education, data = data, main = "Income by Education Level", xlab = "Education", ylab = "Income", col = "lightblue")

# 2. Boxplot of Income by Occupation Type
boxplot(Income ~ Occupation, data = data, main = "Income by Occupation Type", xlab = "Occupation", ylab = "Income", col = "lightgreen")

# 3. Scatter Plot of Age vs. Income
plot(data$Age, data$Income, main="Age vs Income", xlab="Age", ylab="Income", pch=19, col="blue")


