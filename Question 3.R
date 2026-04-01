# Load required libraries
library(dplyr)
library(ggplot2)
# Load the dataset
data <- read.csv("bank-full.csv", sep = ";")
data$y_binary <- ifelse(data$y == "yes", 1, 0)
#Then convert contact and month into factor variables.
data$contact <- as.factor(data$contact)
data$month <- as.factor(data$month)
#Fits a logistic regression model
model <- glm(y_binary ~ contact + day + month + duration + campaign + pdays + previous, data = data, family = binomial)
summary(model)
# Ensure the 'month' and 'y' columns are correctly formatted
data$month <- factor(data$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))
data$y <- as.factor(data$y)
# Summarize counts of "yes" and "no" by month
subscription_summary <- data %>%
18
group_by(month, y) %>%
summarize(count = n(), .groups = "drop")
# Create stacked bar graph
ggplot(subscription_summary, aes(x = month, y = count, fill = y)) +
geom_bar(stat = "identity", position = "stack") +
labs(
title = "Campaign Outcomes by Month",
x = "Month",
y = "Count",
fill = "Subscription Outcome"
) +
theme_minimal() +
theme(axis.text.x = element_text(angle = 45, hjust = 1))
