## Copyright (C) 2024 Naveen Pentela @ Flinders University
## All Rights Reserved

##----------------------------------------------------------------------------##
## Required dependencies.
##----------------------------------------------------------------------------##


#Project R code file for the topic Data Engineering

# Install the packages if not installed yet
install.packages("mongolite")
install.packages("tidyverse")
install.packages("lmtest")
install.packages("ggplot2")
install.packages("ggfortify")

# Load relevant libraries
library(mongolite)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lmtest)
library(ggplot2)
library(ggfortify)

# Setup MongoDB connection
connection = 'mongodb+srv://pent0020:pent0020@comp2031-8031.hztdz6h.mongodb.net/?retryWrites=true&w=majority&appName=COMP2031-8031'

# 1) Data Wrangling
# 1a) Loading the data
grades_data = mongo(collection="grades", db="sample_training", url=connection)
#load data particulary from class id 7
pipeline <- '[{"$match":{"class_id":7}},{"$unwind":{"path": "$scores"}},{"$project":{"scores.score":1,"_id":0,"scores.type":1,"class_id":1}}]'
scores_of_all_students <- grades_data$aggregate(pipeline)
View(scores_of_all_students)

# Extract score values for detailed analysis
score_values <- scores_of_all_students$scores$score

# 1b) Handling missing data
scores_of_all_students <- scores_of_all_students[!is.na(scores_of_all_students$scores$score), ]

# 1c) Tidying the data
scores_df <- data.frame(
  class_id = scores_of_all_students$class_id,
  score_type = scores_of_all_students$scores$type,
  score = scores_of_all_students$scores$score
)
# 2) Data Transformation
# 2a) Data Transformation techniques
# Winsorization and Standardization of score values have been applied in the below code
# The winsorize function has been defined and used to limit extreme values to reduce the effect of possible outliers.
# Winsorize function to handle outliers
winsorize <- function(x, trim = 0.05) {
  q <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  x[x < q[1]] <- q[1]
  x[x > q[2]] <- q[2]
  x
}

scores_df$score <- as.numeric(scores_df$score)
scores_df$score <- winsorize(scores_df$score, trim = 0.05)
scores_df$standardized_score <- scale(scores_df$score)


# 2b) Creating new variables
# Create dummy variables for score_type
dummy_vars <- model.matrix(~ score_type - 1, data = scores_df)
dummy_vars <- dummy_vars[, !colnames(dummy_vars) %in% colnames(scores_df)]  # Remove duplicates
scores_df <- cbind(scores_df, dummy_vars)

if ("class_id" %in% names(scores_df)) {
  class_mean_scores <- aggregate(score ~ class_id, data = scores_df, FUN = mean, na.rm = TRUE)  # Add na.rm = TRUE to handle missing values
  names(class_mean_scores) <- c("class_id", "mean_score")
  
  if ("class_id" %in% names(class_mean_scores)) {
    scores_df <- merge(scores_df, class_mean_scores, by = "class_id", all.x = TRUE)  # Use all.x = TRUE to keep all rows from 'scores_df'
  }
}

# Creating standardized_score and dummy variables for score_type
scores_df$standardized_score <- scale(scores_df$score)

# 3) Data Analysis
# 3a) Statistical analysis or exploratory data analysis
# Calculate median and mean of scores
median_score <- median(score_values)
mean_score <- mean(score_values)
# Display median and mean
print(paste("Median Score:", median_score))
print(paste("Mean Score:", mean_score))

# View statistics from the boxplot
boxplot_stats <- boxplot(score_values, plot = FALSE)$stats
print(boxplot_stats)
# Summary statistics
summary(scores_df)

# 3b) Data visualisation
# Create a boxplot of the scores
boxplot(score_values, col="orange", main = "Overall scores of all students class with id '7'")
# Add data points to the boxplot
stripchart(score_values, method = "jitter", pch = 19, add = TRUE, col = "black", vertical=TRUE)

# Create a histogram of the scores
hist(score_values, col="skyblue", border="black", xlab="Scores of all students of class id 7", main="Histogram of scores_of_all_students")

# Visualization: Histogram of scores
ggplot(scores_df, aes(x = score)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Scores", x = "Score", y = "Frequency")

# Visualization: Boxplot of scores by score type
ggplot(scores_df, aes(x = score_type, y = score)) +
  geom_boxplot(fill = "red", color = "black") +
  labs(title = "Boxplot of Scores by Score Type", x = "Score Type", y = "Score")

# edge case handling optional
# check whether data does not have extreme outliers by visualizing again and see further transformations if needed.
ggplot(scores_df, aes(x = standardized_score)) +
  geom_histogram(binwidth = 0.1, fill = "green", color = "black") +
  labs(title = "Distribution of Standardized Scores", x = "Standardized Score", y = "Frequency")

# 4) Data Modelling
# 4a) Simple linear model
simple_lm <- lm(score ~ standardized_score, data = scores_df)
summary(simple_lm)

# 4b) General linear model
# 4bi) Predictors are categorical
categorical_lm <- lm(score ~ score_type, data = scores_df)
summary(categorical_lm)

# 4bii) Predictors are categorical and continuous
categorical_continuous_lm <- lm(score ~ score_type + standardized_score, data = scores_df)
summary(categorical_continuous_lm)

# 4biii) Predictors are continuous
continuous_lm <- lm(score ~ standardized_score, data = scores_df)
summary(continuous_lm)

# 4c) Model evaluation
# Calculate R-squared and RMSE for each model
evaluate_model <- function(model, data) {
  pred <- predict(model, newdata = data)
  actual <- data$score
  rss <- sum((pred - actual) ^ 2)
  tss <- sum((actual - mean(actual)) ^ 2)
  r_squared <- 1 - (rss / tss)
  rmse <- sqrt(mean((pred - actual) ^ 2))
  list(R_squared = r_squared, RMSE = rmse)
}

simple_lm_eval <- evaluate_model(simple_lm, scores_df)
categorical_lm_eval <- evaluate_model(categorical_lm, scores_df)
categorical_continuous_lm_eval <- evaluate_model(categorical_continuous_lm, scores_df)
continuous_lm_eval <- evaluate_model(continuous_lm, scores_df)

# Print model evaluation metrics
simple_lm_eval
categorical_lm_eval
categorical_continuous_lm_eval
continuous_lm_eval

# 4d) Model Interpretation
# Interpret the significance of predictors and overall fit of the models
interpret_model <- function(model, model_name) {
  cat("\nModel:", model_name, "\n")
  print(summary(model))
  cat("\nR-squared:", evaluate_model(model, scores_df)$R_squared, "\n")
  cat("RMSE:", evaluate_model(model, scores_df)$RMSE, "\n")
}

interpret_model(simple_lm, "Simple Linear Model")
interpret_model(categorical_lm, "Categorical Linear Model")
interpret_model(categorical_continuous_lm, "Categorical and Continuous Linear Model")
interpret_model(continuous_lm, "Continuous Linear Model")

