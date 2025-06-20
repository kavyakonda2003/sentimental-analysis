# Install and load required packages
install.packages("sentimentr")
install.packages("dplyr")
install.packages("stringi")
install.packages("ggplot2")
install.packages("scales")
install.packages("plotly")

library(sentimentr)
library(dplyr)
library(stringi)
library(ggplot2)
library(scales)
library(plotly)

# Reading CSV file
df <- read.csv("sentiment.csv", header = TRUE)

# Perform sentiment analysis
sentiment_results <- sentiment_by(df$text)

# Combine sentiment results with the original DataFrame
df <- df %>%
  mutate(sentiment_score = sentiment_results$ave_sentiment,
         sd = sentiment_results$sd,
         ave_sentiment = sentiment_results$ave_sentiment)

# Display first few rows of the DataFrame
print("First few rows of the combined DataFrame:")
print(head(df))

# Display summary statistics of the sentiment scores
print("Summary statistics of sentiment scores:")
print(summary(df$sentiment_score))

# Display some sample sentiment scores
print("Sample sentiment scores:")
print(df %>% select(id, text, sentiment_score) %>% head(10))

# Visualization of sentiment scores
print("Creating visualizations...")

# Histogram of sentiment scores
hist_plot <- ggplot(df, aes(x = sentiment_score)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Distribution of Sentiment Scores", x = "Sentiment Score", y = "Frequency")
print(hist_plot)

# Boxplot of sentiment scores
box_plot <- ggplot(df, aes(x = "", y = sentiment_score)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Boxplot of Sentiment Scores", x = "", y = "Sentiment Score")
print(box_plot)

# Density plot of sentiment scores
density_plot <- ggplot(df, aes(x = sentiment_score)) +
  geom_density(fill = "blue", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Density Plot of Sentiment Scores", x = "Sentiment Score", y = "Density")
print(density_plot)

# Bar plot of sentiment distribution
bar_plot <- ggplot(df, aes(x = ave_sentiment)) +
  geom_bar(fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(title = "Sentiment Distribution", x = "Sentiment", y = "Count")
print(bar_plot)

# Pie chart of sentiment distribution
pie_data <- df %>%
  count(ave_sentiment) %>%
  mutate(perc = n / sum(n) * 100, labels = paste(ave_sentiment, round(perc, 1), "%"))

pie_plot <- ggplot(pie_data, aes(x = "", y = perc, fill = ave_sentiment)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_minimal() +
  labs(title = "Pie Chart of Sentiment Distribution", x = "", y = "") +
  theme(axis.text.x = element_blank()) +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5))
print(pie_plot)

# Scatter plot of sentiment scores vs. text length
df$length <- stri_length(df$text)
scatter_plot <- ggplot(df, aes(x = length, y = sentiment_score, color = ave_sentiment)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Scatter Plot of Sentiment Scores vs. Text Length", x = "Text Length", y = "Sentiment Score")
print(scatter_plot)

