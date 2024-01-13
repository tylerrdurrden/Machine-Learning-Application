# Load necessary libraries
library(dplyr)
library(randomForest)
library(naivebayes)
library(caret)
library(ggplot2)
library(tidyverse)
library(class)

# Read the dataset
data <- read.csv("smoking.csv")

# Replace 'No' with 0 and 'Yes' with 1 in the 'smoke' variable
data$smoke <- ifelse(data$smoke == "No", 0, 1)

# Handle missing values
data[data == "NA"] <- NA

# Convert categorical variables to factors
data[c("gender", "marital_status", "highest_qualification", "nationality", "ethnicity", "region", "type")] <- lapply(data[c("gender", "marital_status", "highest_qualification", "nationality", "ethnicity", "region", "type")], as.factor)

# Replace missing values with mean for numeric columns
data$amt_weekends[is.na(data$amt_weekends)] <- mean(data$amt_weekends, na.rm = TRUE)
data$amt_weekdays[is.na(data$amt_weekdays)] <- mean(data$amt_weekdays, na.rm = TRUE)

# Fit logistic regression model
model <- glm(smoke ~ ., data = data, family = "binomial")
# Summarize the model
summary(model)

# Split the data into predictors and the outcome variable
predictors <- data[, -which(names(data) %in% "smoke")]
outcome <- data$smoke
# Fit Random Forest model
set.seed(123)  # For reproducibility
rf_model <- randomForest(predictors, outcome, ntree = 500)
# View model summary
print(rf_model)

# Fit Naive Bayes model
nb_model <- naive_bayes(as.factor(smoke) ~ ., data = data)
# View model summary
print(nb_model)

# Visualization - Variable Importance Plot for Random Forest
varImpPlot(rf_model, main = "Variable Importance Plot")

# Select variables for scatterplots
vars_to_plot <- c("age", "amt_weekends", "amt_weekdays")

# Filter data to remove rows with NA values in selected variables
data_filtered <- na.omit(data[, vars_to_plot])

# Plot scatterplots
scatter_plots <- ggplot(data_filtered, aes(x = age, y = amt_weekends)) +
  geom_point() +
  labs(title = "Scatterplot: Age vs Amount on Weekends")

scatter_plots + theme_minimal()  # Display the scatterplot for Age vs Amount on Weekends

# Create scatterplot for Age vs Amount on Weekdays
scatter_plots_weekdays <- ggplot(data_filtered, aes(x = age, y = amt_weekdays)) +
  geom_point() +
  labs(title = "Scatterplot: Age vs Amount on Weekdays")

scatter_plots_weekdays + theme_minimal()  # Display the scatterplot for Age vs Amount on Weekdays

# Plot boxplot for smoking vs age
boxplot_age <- ggplot(data, aes(x = as.factor(smoke), y = age)) +
  geom_boxplot() +
  labs(title = "Boxplot: Smoking vs Age")

# Plot boxplot for smoking vs gross_income
boxplot_income <- ggplot(data, aes(x = as.factor(smoke), y = as.numeric(gsub("[^0-9]", "", gross_income)))) +
  geom_boxplot() +
  labs(title = "Boxplot: Smoking vs Gross Income")

# Plot line plot for smoking vs amt_weekends
lineplot_weekends <- ggplot(data, aes(x = age, y = amt_weekends, color = as.factor(smoke))) +
  geom_line() +
  labs(title = "Line Plot: Smoking vs Amount on Weekends")

# Create a bar plot for count of smokers by gender
barplot_gender <- data %>%
  group_by(gender, smoke) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = gender, y = count, fill = as.factor(smoke))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Bar Plot: Count of Smokers by Gender")

# Display plots
boxplot_age + theme_minimal()  # Display boxplot for Smoking vs Age
boxplot_income + theme_minimal()  # Display boxplot for Smoking vs Gross Income
lineplot_weekends + theme_minimal()  # Display line plot for Smoking vs Amount on Weekends
barplot_gender + theme_minimal()  # Display bar plot for Count of Smokers by Gender

# Density Plot of Age by Smoking Status
ggplot(data, aes(x = age, fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age by Smoking Status", x = "Age", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Density Plot of Age by Gender and Smoking Status
ggplot(data, aes(x = age, fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~gender, scales = "free_y") +
  labs(title = "Density Plot of Age by Gender and Smoking Status", x = "Age", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Density Plot of Gross Income by Smoking Status
ggplot(data, aes(x = gross_income, fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Gross Income by Smoking Status", x = "Gross Income", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Density Plot for 'Smoking Patterns' by 'Nationality'
ggplot(data, aes(x = factor(nationality), fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Smoking Patterns by Nationality", x = "Nationality", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Density Plot for 'Smoking Patterns' by 'Region'
ggplot(data, aes(x = factor(region), fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Smoking Patterns by Region", x = "Region", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Density Plot for 'Smoking Patterns' by 'Ethnicity'
ggplot(data, aes(x = factor(ethnicity), fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Smoking Patterns by Ethnicity", x = "Ethnicity", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Density Plot for 'Smoking Patterns' by 'Marital Status'
ggplot(data, aes(x = factor(marital_status), fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Smoking Patterns by Marital Status", x = "Marital Status", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Plot density for 'amt_weekends' by 'smoke'
ggplot(data, aes(x = as.numeric(amt_weekends), fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Smoking Patterns by Amount on Weekends", x = "Amount on Weekends", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Plot density for 'amt_weekdays' by 'smoke'
ggplot(data, aes(x = as.numeric(amt_weekdays), fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Smoking Patterns by Amount on Weekdays", x = "Amount on Weekdays", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Density Plot for 'Smoking Patterns' by 'Highest Qualification'
ggplot(data, aes(x = factor(highest_qualification), fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Smoking Patterns by Highest Qualification", x = "Highest Qualification", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Density Plot for 'Smoking Patterns' by 'Type'
ggplot(data, aes(x = factor(type), fill = factor(smoke))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Smoking Patterns by Type", x = "Type", y = "Density") +
  scale_fill_manual(values = c("0" = "skyblue", "1" = "orange"), name = "Smoking Status") +
  guides(fill = guide_legend(title = "Smoking Status"))

# Remove 'gross_income' and 'type' columns
data_without_cols <- subset(data, select = -c(gross_income, type))

# Define features and target variable
your_features <- c("gender", "age", "marital_status", "highest_qualification", "nationality", "ethnicity", "region", "amt_weekends", "amt_weekdays")
X <- data_without_cols[, your_features]
y <- data_without_cols[["smoke"]]  # Assuming "smoke" is the target variable column name

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(data_without_cols), 0.7 * nrow(data_without_cols))  # 70% for training, adjust as needed
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]
# Select 'k' (number of neighbors) for classification
k <- 5  # Setting 'k' as 5 for KNN classification

# Convert all columns to numeric
X_train_numeric <- as.data.frame(sapply(X_train, as.numeric))
X_test_numeric <- as.data.frame(sapply(X_test, as.numeric))

# Train the KNN model for classification
model_knn <- knn(train = as.matrix(X_train_numeric), test = as.matrix(X_test_numeric), cl = y_train, k = k)
# Predictions for classification
predictions_classification <- as.factor(model_knn)
kmeans_result <- kmeans(X_train_numeric, centers = 3)
print(kmeans_result)

# Generate confusion matrix
confusion_matrix <- confusionMatrix(predictions_classification, as.factor(y_test))
print(confusion_matrix)