# Load required libraries
library(ggplot2)
library(dplyr)
# Load the data
data <- read.csv("bank-full.csv", sep = ";")
# Convert categorical variables to factors
data$loan <- as.factor(data$loan)
data$marital <- as.factor(data$marital)
data$education <- as.factor(data$education)
# Fit a Logistic Regression Model
model <- glm(loan ~ age + marital + education, data = data, family = "binomial")
# Display the summary
summary(model)
# Set a seed for reproducibility
set.seed(123)
# Build the random forest model
rf_model <- randomForest(loan ~ age + marital + education, data = data, ntree = 500, mtry = 2, importance = TRUE)
17
# Print the model summary
print(rf_model)
# Predict on the training data
pred <- predict(rf_model, data)
# Generate confusion matrix
table(pred, data$loan)
# Plot variable importance
importance(rf_model)
varImpPlot(rf_model, main = "Variable Importance Plot")
# Boxplot: Age Distribution by Loan Status
ggplot(data, aes(x = loan, y = age, fill = loan)) +
geom_boxplot() +
labs(title = "Age Distribution by Loan Status",
x = "Loan Status (No/Yes)",
y = "Age") +
theme_minimal()
# Bar Plot: Loan Status by Marital Status and Education
loan_summary <- data %>%
group_by(marital, education) %>%
summarise(loan_rate = mean(ifelse(loan == "yes", 1, 0)))
pastel_colors <- c("#FFB3BA", "#FFDFBA", "#FFFFBA", "#BAFFC9", "#BAE1FF")
# Create the bar plot
ggplot(loan_summary, aes(x = marital, y = loan_rate, fill = education)) +
geom_bar(stat = "identity", position = "dodge") +
scale_fill_manual(values = pastel_colors) +
labs(title = "Loan Status by Marital Status and Education",
x = "Marital Status",
y = "Proportion Taking Loans",
fill = "Education Level") +
theme_minimal()
