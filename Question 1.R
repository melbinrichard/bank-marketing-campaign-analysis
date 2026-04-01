# Load necessary library
library(ggplot2)
# Load the dataset
bank_data <- read.csv("bank-full.csv", sep = ";")
# Convert the target variable to a binary factor
bank_data$y <- as.factor(bank_data$y)
# Build a logistic regression model
model <- glm(y ~ duration, data = bank_data, family = binomial)
# Summarize the model to check the significance of 'duration'
summary(model)
# Predict the probability of subscription based on contact duration
bank_data$predicted_prob <- predict(model, type = "response")
# Visualization 1: Probability of Subscription vs. Contact Duration
ggplot(bank_data, aes(x = duration, y = predicted_prob)) +
geom_line(color = "red") +
labs(title = "Predicted Subscription Probability by Contact Duration",
x = "Contact Duration (seconds)",
y = "Predicted Subscription Probability") +
theme_minimal()
# Visualization 2: Violin Plot of Duration by Subscription Outcome
ggplot(bank_data, aes(x = y, y = duration)) +
geom_violin(trim = FALSE, fill = "lightblue") +
labs(title = "Contact Duration vs. Subscription Outcome",
x = "Subscription Outcome",
y = "Contact Duration (seconds)") +
theme_minimal()
