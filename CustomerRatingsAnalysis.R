################################################
# Fast-Food Customer Ratings Analysis
# Author: Meanna Solomon
################################################

# Load Wendy's dataset
data <- read.csv("data_wendys.csv")
str(data)
head(data)

################################################
# Step 1: Data Cleaning and Preparation
################################################
# Check column names
colnames(data)

# Create a clean dataset excluding rows with missing ratings for Wendy's, McDonald's, Burger King, and Subway
data_clean <- subset(data, !is.na(rating_wendys) & !is.na(rating_mcd) & !is.na(rating_burgerking) & !is.na(rating_subway))

################################################
# Step 2: Boxplot Comparison of Wendy’s and McDonald's Ratings
################################################
# Plot side-by-side boxplots for customer ratings of Wendy’s and McDonald's
boxplot(data_clean$rating_wendys, data_clean$rating_mcd,
        names = c("Wendy's", "McDonald's"),
        main = "Customer Ratings Comparison",
        ylab = "Ratings",
        col = c("red", "blue"))

################################################
# Step 3: Summary Table for Average Ratings and IQR for Burger King and Subway
################################################
# Convert ratings to numeric type for Burger King and Subway
data_clean$rating_burgerking <- as.numeric(data_clean$rating_burgerking)
data_clean$rating_subway <- as.numeric(data_clean$rating_subway)

# Calculate mean and IQR for Burger King and Subway ratings
ratings_table <- data.frame(
  Mean = c(mean(data_clean$rating_burgerking, na.rm = TRUE), mean(data_clean$rating_subway, na.rm = TRUE)),
  IQR = c(IQR(data_clean$rating_burgerking, na.rm = TRUE), IQR(data_clean$rating_subway, na.rm = TRUE))
)
rownames(ratings_table) <- c("Burger King", "Subway")
print(ratings_table)

# Observation:
# Burger King has an average rating of approximately 6.85, and Subway has an average rating of 7.84.
# The IQR for both brands is 2, indicating similar variability in ratings.

################################################
# Step 4: Difference in Ratings Between Wendy's and McDonald's
################################################
# Calculate the difference in ratings between Wendy's and McDonald's
data_clean$diff <- data_clean$rating_wendys - data_clean$rating_mcd

################################################
# Step 5: Quantile Analysis of Rating Differences
################################################
# Calculate quantiles for the difference in ratings between Wendy's and McDonald's
quantiles <- quantile(data_clean$diff, probs = c(0.25, 0.75), na.rm = TRUE)
print(quantiles)

# Interpretation:
# At least 25% of the rating differences are 0 or higher, suggesting that Wendy’s is rated the same or higher than McDonald's for at least 75% of customers.
# The 75th percentile difference of 2 indicates that Wendy’s is rated up to 2 points higher than McDonald's.

################################################
# Step 6: Paired T-Test - Wendy's vs. McDonald's Ratings
################################################
# Perform a paired t-test to compare ratings between Wendy's and McDonald's
t_test_result <- t.test(data_clean$rating_wendys, data_clean$rating_mcd, alternative = "greater", paired = TRUE)
print(t_test_result)

# Interpretation:
# The small p-value suggests that Wendy’s ratings are significantly higher than McDonald's in this dataset.
# On average, Wendy's rating is approximately 0.86 points higher than McDonald's.

################################################
# Step 7: Correlation Analysis Between Ratings of Different Fast-Food Chains
################################################
# Load required library for correlation plot
library(corrplot)

# Select relevant columns for correlation analysis
rating_matrix <- data_clean[, c("rating_wendys", "rating_mcd", "rating_burgerking", "rating_subway")]

# Calculate correlation matrix
correlation_matrix <- cor(rating_matrix, use = "complete.obs")

# Plot the correlation matrix
corrplot(correlation_matrix, method = "circle")
