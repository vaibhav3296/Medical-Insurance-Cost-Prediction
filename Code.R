# README: Dataset Setup
# ---------------------
# This script uses the "Medical Insurance Cost Prediction" dataset from Kaggle.
# Steps to download and load the dataset:
# 1. Visit https://www.kaggle.com/datasets/rahulvyasm/medical-insurance-cost-prediction
# 2. Download and place `medical_insurance.csv` in the script directory.

# ============================
# Library Installation & Setup
# ============================
required_libraries <- c("tidyverse", "ggplot2", "corrplot", "ISLR2", "leaps", "caret", "MASS", "class", "glmnet")


for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib, dependencies = TRUE)
  }
  library(lib, character.only = TRUE)
}

set.seed(123)

# ==================================
# ==================================
# Part II - Data Preparation and EDA
# ==================================
# ==================================

# Load the dataset
getwd()
# download the data and change the "working directory" from "Session" in order for "R" to read it.
data <- read.csv("medical_insurance.csv")

# Preview dataset structure
str(data)
summary(data)
dim(data)

# Count observations
num_obs <- nrow(data)
cat("Number of observations:", num_obs, "\n")

# Count variables
num_variables <- length(data)

# Display variable descriptions and types
cat("Variable descriptions:\n")
print(data.frame(Variable = names(data), Type = sapply(data, class)))

# General Information: Missing Values
missing_values <- colSums(is.na(data))
cat("Missing values per variable:\n")
print(missing_values)

# ==================================
# Univariate Analysis
# ==================================

# 1. Age Analysis
range(data$age)
table(data$age)
mean(data$age)
sd(data$age)

cat("Analyzing 'age' variable...\n")
hist(data$age, main = "Histogram of Age", xlab = "Age", col = "blue", border = "black")
boxplot(data$age, main = "Boxplot of Age", ylab = "Age", col = "blue")
cat("Summary of Age:\n")
print(summary(data$age))

# Create age bins/groups for better description of age distribution
data$age_group <- cut(data$age, 
                      breaks = c(18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 64), 
                      right = TRUE, 
                      include.lowest = TRUE,
                      labels = c("18-20", "21-25", "26-30", "31-35", "36-40", 
                                 "41-45", "46-50", "51-55", "56-60", "61-64"))

# Calculate the number of individuals in each age group
age_group_counts <- table(data$age_group)

# Print the counts for each age group
cat("Age Group Distribution:\n")
print(age_group_counts)

# 2. Sex Analysis
table(data$sex)

cat("Analyzing 'sex' variable...\n")
sex_counts <- table(data$sex)
sex_counts
sex_percentage <- round(prop.table(sex_counts) * 100, 1)
sex_percentage

# Create pie chart for sex distribution
pie(sex_counts, 
    labels = paste0(names(sex_counts), " (", sex_counts, ")"), 
    main = "Sex Distribution", 
    col = c("pink", "lightblue"), 
    cex = 1.2)

# Calculate mid-points for each slice to position percentage labels
angles <- seq(0, 2 * pi, length.out = length(sex_counts) + 1)[-1] - pi / length(sex_counts)
x_pos <- 0.4 * cos(angles)
y_pos <- 0.4 * sin(angles)

# Add percentage labels inside the slices
text(x_pos, y_pos, 
     labels = paste0(sex_percentage, "%"), 
     cex = 1.5,
     col = "black")

# 3. BMI Analysis
range(data$bmi)
table(data$bmi)
mean(data$bmi)
sd(data$bmi)

cat("Analyzing 'bmi' variable...\n")
hist(data$bmi, main = "Histogram of BMI", xlab = "BMI", col = "blue", border = "black")
boxplot(data$bmi, main = "Boxplot of BMI", ylab = "BMI", col = "blue")
cat("Summary of BMI:\n")
print(summary(data$bmi))

# 4. Children Analysis
range(data$children)
table(data$children)
mean(data$children)
sd(data$children)

cat("Analyzing 'children' variable...\n")
children_counts <- table(data$children)
children_counts
children_percentage <- round(prop.table(children_counts) * 100, 1)
children_percentage

# Define a custom color palette
children_colors <- c("#FF9999", "#FFCC99", "#FFFF99", "#CCFF99", "#99CCFF", "#CC99FF")

# Create pie chart for children distribution
pie(children_counts, 
    labels = ifelse(names(children_counts) %in% c("4", "5"), paste0(names(children_counts), " (", children_counts, ")"),  
                    paste0(names(children_counts), " (", children_counts, ")")),
    main = "Number of Children Distribution", 
    col = children_colors, 
    cex = 1.2)

# Correctly calculate mid-points for percentage labels
angles <- cumsum(prop.table(children_counts) * 2 * pi) - (prop.table(children_counts) * pi)
x_pos <- 0.5 * cos(angles)
y_pos <- 0.5 * sin(angles)

# Add percentage labels for each slice except 4 and 5
text(x_pos, y_pos, 
     labels = ifelse(names(children_counts) %in% c("4", "5"), "", 
                     paste0(children_percentage, "%")), 
     cex = 1.2,
     col = "black")

# Special treatment for 4 and 5 children categories
small_labels <- which(names(children_counts) %in% c("4", "5"))
text(x_pos[small_labels] + 0.1, y_pos[small_labels] - 0.015, 
     labels = paste0(children_percentage[small_labels], "%"),
     cex = 0.9,
     col = "black")

cat("Children Distribution:\n")
print(children_counts)

# 5. Smoker Analysis
table(data$smoker)

smoker_counts <- table(data$smoker)
smoker_counts
smoker_percentage <- round(prop.table(smoker_counts) * 100, 1)
smoker_percentage

# Define smoker colors
smoker_colors <- c("#66C2A5", "#FFA500")

# Create pie chart for smoker status
pie(smoker_counts, 
    labels = paste0(names(smoker_counts), " (", smoker_counts, ")"), 
    main = "Smoker Status Distribution", 
    col = smoker_colors, 
    cex = 1.2)

# Calculate mid-points for each slice
angles <- cumsum(prop.table(smoker_counts) * 2 * pi) - (prop.table(smoker_counts) * pi)
x_pos <- 0.5 * cos(angles)
y_pos <- 0.5 * sin(angles)

# Fine-tune slice positions
x_pos[1] <- x_pos[1]
y_pos[1] <- y_pos[1] - 0.15
x_pos[2] <- x_pos[2] + 0.03
y_pos[2] <- y_pos[2] - 0.03

# Add percentage labels
text(x_pos, y_pos, 
     labels = paste0(smoker_percentage, "%"), 
     cex = 1.5,
     col = "black")

cat("Smoker Status Distribution:\n")
print(smoker_counts)

# 6. Region Analysis
table(data$region)

cat("Analyzing 'region' variable...\n")
region_counts <- table(data$region)
region_counts
region_percentage <- round(prop.table(region_counts) * 100, 1)
region_percentage

# Define region colors
region_colors <- c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF")

# Create pie chart for region distribution without percentages in external labels
pie(region_counts, 
    labels = paste0(names(region_counts), " (", region_counts, ")"),  # Remove percentages from external labels
    main = "Region Distribution", 
    col = region_colors)

# Calculate mid-points for placing percentage labels
angles <- cumsum(prop.table(region_counts) * 2 * pi) - (prop.table(region_counts) * pi)
x_pos <- 0.5 * cos(angles)  # Adjust radius for percentage positions
y_pos <- 0.5 * sin(angles)

# Add percentage labels inside slices
text(x_pos, y_pos, 
     labels = paste0(region_percentage, "%"), 
     cex = 1.5,  # Adjust font size
     col = "black")

cat("Region Distribution:\n")
print(region_counts)


# 7. Charges Analysis
range(data$charges)
table(data$charges)
mean(data$charges)
sd(data$charges)

cat("Analyzing 'charges' variable...\n")
hist(data$charges, main = "Histogram of Charges", xlab = "Charges", col = "blue", border = "black")
boxplot(data$charges, main = "Boxplot of Charges", ylab = "Charges", col = "blue")
cat("Summary of Charges:\n")
print(summary(data$charges))


# ==================================
# Bivariate Analysis
# ==================================

# 1. Correlation Matrix for Numeric Variables
cat("Generating Correlation Matrix for numeric variables...\n")
numeric_vars <- sapply(data, is.numeric)  # Identify numeric variables
cor_matrix <- cor(data[, numeric_vars], use = "complete.obs")
cat("Correlation Matrix:\n")
print(cor_matrix)

# Visualizing the Correlation Matrix

corrplot(cor_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.cex = 0.8, 
         cl.cex = 0.8, number.cex = 0.7, 
         addCoef.col = "darkgrey",
         tl.srt = 45,
         title = "Correlation Matrix of Numeric Variables", mar = c(0, 0, 2, 0))


#2. Smoking

# Boxplot: Smoker vs Charges
cat("Generating boxplot for Smoker vs Charges...\n")
ggplot(data, aes(x = smoker, y = charges, fill = smoker)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1.5) +
  labs(title = "Boxplot of Charges by Smoker Status", 
       x = "Smoker Status", 
       y = "Charges") +
  scale_fill_manual(values = c("blue", "orange")) +
  theme_minimal()

# Scatter Plot: BMI vs Charges with Smoking included
cat("Generating scatter plot for BMI vs Charges...\n")
ggplot(data, aes(x = bmi, y = charges, color = smoker)) +
  geom_point(alpha = 0.6) +
  labs(title = "Scatter Plot of BMI vs Charges by Smoker Status", 
       x = "BMI", 
       y = "Charges") +
  theme_minimal()

# 3. Region vs Charges
cat("Generating boxplot for Region vs Charges...\n")
ggplot(data, aes(x = region, y = charges, fill = region)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 1.5) +
  scale_fill_manual(values = region_colors) +
  labs(title = "Boxplot of Charges by Region", 
       x = "Region", 
       y = "Charges") +
  theme_minimal()

# 4. Scatter Plot: Age vs Charges
cat("Generating scatter plot for Age vs Charges...\n")
ggplot(data, aes(x = age, y = charges)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Scatter Plot of Age vs Charges with Trend Line", 
       x = "Age", 
       y = "Charges") +
  theme_minimal()

# ==================================
# Data Cleaning/Preparation
# ==================================

# 1. Transform charges for skewness reduction
# The variable "charges" is positively skewed. A log transformation normalizes its distribution, improving model assumptions.
data$log_charges <- log(data$charges)

# Visualize and summarize the log-transformed charges
hist(data$log_charges, main = "Histogram of Log-Transformed Charges", 
     xlab = "Log(Charges)", col = "blue", border = "black")
cat("Log-Transformed Charges Distribution Summary:\n")
print(summary(data$log_charges))

# 2. Handle interaction terms
# Create interaction term between smoker status and BMI to capture the compounding effect of these predictors on charges.
data$smoker_bmi_interaction <- ifelse(data$smoker == "yes", data$bmi, 0)

# Summarize the interaction term
cat("Smoker-BMI Interaction Summary:\n")
summary(data$smoker_bmi_interaction)

# 3. Create a flag for high charges
# Define the 75th percentile of charges as the cutoff for the "high_charge" flag
high_charge_cutoff <- quantile(data$charges, 0.75)

# Add the high_charge binary variable
data$high_charge <- ifelse(data$charges > high_charge_cutoff, 1, 0)

# Visualize the charges distribution with the cutoff
hist(data$charges, breaks = 50, col = "blue", border = "black",
     main = "Charges Distribution with High Cost Cutoff", 
     xlab = "Charges", ylab = "Frequency")
abline(v = high_charge_cutoff, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("75th Percentile Cutoff"), 
       col = c("red"), lty = c(2), lwd = c(2))

# Calculate the distribution of the high charge flag
high_charge_distribution <- table(data$high_charge)

# Visualize the high charge flag distribution
barplot_counts <- barplot(table(data$high_charge), 
                          main = "High Charge Flag Distribution", 
                          xlab = "Charge Category", 
                          ylab = "Count", 
                          col = c("blue", "orange"), 
                          names.arg = c("Non-High Charge", "High Charge"),
                          ylim = c(0, 2500))

# Add exact numbers directly above the bars
text(barplot_counts, 
     table(data$high_charge) + 100, # Adjusting vertical position
     labels = as.character(table(data$high_charge)), 
     cex = 1.2, 
     col = "black")

# 4. Standardize numeric variables
# Standardize continuous predictors to have a mean of 0 and a standard deviation of 1 for uniform model training.
numeric_vars_to_scale <- c("age", "bmi", "log_charges")
data[numeric_vars_to_scale] <- scale(data[numeric_vars_to_scale])

# Summarize the standardized variables
cat("Scaled Variables Summary:\n")
summary(data[numeric_vars_to_scale])

# 5. Add polynomial features
# Introduce polynomial features to capture non-linear relationships.
data$age_squared <- data$age^2
data$bmi_squared <- data$bmi^2

# Summarize the polynomial features
cat("Polynomial Features Summary:\n")
summary(data[, c("age_squared", "bmi_squared")])

# 6. Correlation analysis after transformations
# Compute and visualize the correlation matrix for all numeric variables.
numeric_vars <- sapply(data, is.numeric)
cor_matrix <- cor(data[, numeric_vars], use = "complete.obs")
cat("Updated Correlation Matrix with Transformed Variables and Polynomial Features:\n")
print(cor_matrix)

# Visualize the updated correlation matrix
corrplot(cor_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.cex = 0.8, 
         cl.cex = 0.8, number.cex = 0.7, 
         addCoef.col = "darkgrey",
         tl.srt = 45,
         title = "Correlation Matrix with Standardized and Transformed Variables", 
         mar = c(0, 0, 2, 0))

# Libraries for modeling and evaluation
library(caret)
library(MASS)   # For LDA
library(class)  # For KNN

# Split the dataset into training and test sets
set.seed(123)
train_index <- createDataPartition(data$high_charge, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Separate predictors and response
predictors <- c("age", "bmi", "smoker_bmi_interaction", "age_squared", "bmi_squared")
response <- "high_charge"

train_x <- train_data[, predictors]
train_y <- train_data[[response]]
test_x <- test_data[, predictors]
test_y <- test_data[[response]]

# Logistic Regression Model
logit_model <- glm(high_charge ~ ., family = binomial, data = train_data[, c(predictors, response)])

# Predictions
logit_prob <- predict(logit_model, newdata = test_data, type = "response")
logit_pred <- ifelse(logit_prob > 0.5, 1, 0)

# Evaluate the Logistic Regression Model
confusion_logit <- confusionMatrix(as.factor(logit_pred), as.factor(test_y))

# Output results
cat("Logistic Regression Results:\n")
summary(logit_model)
print(confusion_logit)

lda_model <- lda(high_charge ~ ., data = train_data[, c(predictors, response)])

# Predictions
lda_pred <- predict(lda_model, newdata = test_data)
lda_class <- lda_pred$class

# Evaluate the LDA Model
confusion_lda <- confusionMatrix(as.factor(lda_class), as.factor(test_y))

# Output results
cat("LDA Results:\n")
print(lda_model)
print(confusion_lda)

# Determine optimal K using cross-validation
set.seed(123)
knn_tune <- train(train_x, as.factor(train_y), method = "knn",
                  tuneGrid = expand.grid(k = 1:20),
                  trControl = trainControl(method = "cv", number = 10))

# Best value of K
best_k <- knn_tune$bestTune$k

# Train KNN with optimal K
knn_pred <- knn(train = train_x, test = test_x, cl = train_y, k = best_k)

# Evaluate the KNN Model
confusion_knn <- confusionMatrix(as.factor(knn_pred), as.factor(test_y))

# Output results
cat("KNN Results:\n")
print(paste("Optimal K:", best_k))
print(confusion_knn)

# Compare Model Performance
cat("\nComparison of Model Results:\n")

# Logistic Regression
cat("\nLogistic Regression:\n")
print(confusion_logit)

# LDA
cat("\nLDA:\n")
print(confusion_lda)

# KNN
cat("\nKNN:\n")
print(confusion_knn)