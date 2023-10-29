library(readxl) # read data 
library(ggplot2) # data visualization
library(tidyverse) # utility functions
library(forcats) # handling factors
library(scales) # axis scale formatting 

data <- read_excel('040522 Data Mid-term test Final.xlsx')
data <- subset(data, exchangename == "HANOI STOCK EXCHANGE")
view(data) 

data$depreciation_a <- ifelse(is.na(data$depreciation) | data$depreciation == 0, 0, 
                                 ifelse(data$depreciation > 100000000000, 3, 
                                        ifelse(data$depreciation >= 10000000000 & data$depreciation <= 100000000000, 2, 
                                               ifelse(data$depreciation < 10000000000, 1, 0))))


set.seed(907)
data <- data[sample(nrow(data), 100), ]
data

na_sum <- sum(is.na(data))

# Apply median and ifelse to all numeric columns
data[, sapply(data, is.numeric)] <- lapply(data[, sapply(data, is.numeric)], function(x) {
  median_val <- median(x, na.rm = TRUE)
  ifelse(is.na(x), median_val, x)
})

na_sum <- sum(is.na(data))
na_sum

data_top_receivable <- data %>%
  select(firmname, receivable) %>%
  top_n(5, receivable)

data_top_receivable

data_bottom_receivable <- data %>%
  select(firmname, receivable) %>%
  top_n(5, desc(receivable))

data_bottom_receivable

unique(data$industry)

depreciation_levels <- unique(data$'depreciation_a')
depreciation_levels

results <- c()
for (level in depreciation_levels) {
  # Filter rows with 'depreciation' equal to 'level'."
  sub_data <- data[data$'depreciation_a' == level, ]
  
  # Descriptive Statistics 
  n <- nrow(sub_data)
  min_receivable <- Inf
  max_receivable <- -Inf
  mean_receivable <- sum(sub_data$receivable)/n 
  std_receivable <- sqrt(sum((sub_data$receivable - mean_receivable)^2)/(n - 1)) #Standard Deviation
 
  for (i in 1:n) {
    if (sub_data$receivable[i] > max_receivable) max_receivable <- sub_data$receivable[i] # Tìm giá trị tối đa
    if (sub_data$receivable[i] < min_receivable) min_receivable <- sub_data$receivable[i] # Tìm giá trị tối thiểu
  }
  
  # Create a string of results for each level
  result_str <- paste("Depreciation_levels:", level, "\n",
                      "Minimum receivable:", min_receivable, "\n",
                      "Maximum receivable:", max_receivable, "\n",
                      "Mean receivable:", mean_receivable, "\n",
                      "Standard deviation of receivable:", std_receivable, "\n",
                      "\n")
  results <- c(results, result_str)
}

# Sort the results in the order of depreciation_levels
order_index <- order(depreciation_levels)
results <- results[order_index]

# Print the results
cat(results)


# Calculate the median of revenue
med_revenue <- median(data$revenue)

# Create two groups of data based on the median of 'revenue'
below <- data[data$revenue < med_revenue, ]
above <- data[data$revenue >= med_revenue, ]

# Define a function to calculate summary statistics
stats <- function(x) {
  return(c(min(x), max(x), mean(x), sd(x)))
}

# Calculate summary statistics for 'receivable' in the below-median group
below_stats <- stats(below$receivable)

# Calculate summary statistics for 'receivable' in the above-median group
above_stats <- stats(above$receivable)

# Print the results
cat("Below median:\n")
cat("Minimum receivable:", below_stats[1], "\n")
cat("Maximum receivable:", below_stats[2], "\n")
cat("Mean receivable:", below_stats[3], "\n")
cat("Standard deviation of receivable:", below_stats[4], "\n\n")

cat("Above median:\n")
cat("Minimum receivable:", above_stats[1], "\n")
cat("Maximum receivable:", above_stats[2], "\n")
cat("Mean receivable:", above_stats[3], "\n")
cat("Standard deviation of receivable:", above_stats[4], "\n")


library(ggplot2)

ggplot(data, aes(x = receivable)) +
  geom_histogram(fill = "dodgerblue", color = "black") +
  labs(title = "Histogram of Receivable", x = "Receivable", y = "Frequency")


ggplot(data, aes(x = revenue, y = receivable, color = receivable)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red")

# Create a boxplot of payable by depreciation category
boxplot(receivable ~ depreciation_a, 
        data = data, 
        col = c("#FFCC66", "#FF6666", "#66CC66", "#6666FF"), # custom color scheme
        xlab = "Depreciation Category", 
        ylab = "Receivable", 
        main = "Receivable by Depreciation Category Boxplot")

# Add a legend to the boxplot
legend("topright", 
       legend = c("0", "1", "2", "3"), # category labels
       fill = c("#FFCC66", "#FF6666", "#66CC66", "#6666FF")) # color scheme matching the categories


ggplot(data, aes(x = revenue, y = receivable, color = depreciation_a)) +
  geom_point() +
  labs(x = "Revenue", y = "Receivable", color = "Depreciation Category") +
  ggtitle("Revenue and Depreciation Category's impact on Receivable")

# Create a dataframe containing the following variables
df <- data.frame(
                 receivable = data$receivable,
                 depreciation_a = data$depreciation_a,
                 revenue = data$revenue,
                 totalasset = data$totalasset,
                 totalcurrentasset = data$totalcurrentasset
                 )
head(df, 5)

model <- lm(receivable ~ depreciation_a + revenue + totalasset + totalcurrentasset, data = data)
model

# Check regression model summary
summary(model)

# Check for multicollinearity
anova(model)

industry_counts <- as.data.frame(table(data$industry))
names(industry_counts) <- c("Industry", "Count")
cat("Number of firms in an industry:", "\n")
print(industry_counts)


industry_name <- "Industrials"
threshold_value <- mean(data$receivable)

industry_count <- sum(data$industry == industry_name & data$receivable > threshold_value)

cat(paste("Number of firms in", industry_name, "with trade credit above the mean value of Receivable in dataset (", threshold_value, ") is", industry_count))



