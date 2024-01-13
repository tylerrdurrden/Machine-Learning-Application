# Machine-Learning-Application
## Overview
This R script performs exploratory data analysis and builds various machine learning models on a dataset related to smoking behavior. The analysis includes data preprocessing, logistic regression, random forest, naive Bayes, and k-nearest neighbors (KNN) classification. Additionally, various visualizations, such as scatterplots, boxplots, and density plots, are generated to gain insights into the data.
## Requirements
Make sure you have the following R libraries installed:
* dplyr
* randomForest
* naivebayes
* caret
* ggplot2
* tidyverse
* class

Install any missing libraries using:

```install.packages(c("dplyr", "randomForest", "naivebayes", "caret", "ggplot2", "tidyverse", "class"))```

## Dataset
The analysis is performed on the "smoking.csv" dataset. The dataset contains information about individuals, including demographics, smoking habits, and other relevant features.

## Usage
1. Ensure that the required R libraries are installed.
2. Download the dataset ("smoking.csv") and place it in the same directory as the R script.
3. Run the R script (your_script_name.R) in an R environment or RStudio.

## Analysis Steps
1. Data Preprocessing:
* Replace 'No' with 0 and 'Yes' with 1 in the 'smoke' variable.
* Handle missing values by replacing them with the mean for numeric columns.
2. Model Building:
* Fit logistic regression, random forest, naive Bayes, and KNN models.
* Visualize variable importance for the random forest model.
3. Visualizations:
* Generate scatterplots, boxplots, line plots, and density plots to explore relationships in the data.
4. Model Evaluation:
* Evaluate the KNN model using a confusion matrix.
5. Plots
* Various plots are generated, including scatterplots, boxplots, line plots, and density plots, to visualize relationships between different variables and smoking status.

## Additional Notes
* The script includes steps to split the dataset into training and testing sets for the KNN model.
* Adjust the percentage of data used for training as needed.
