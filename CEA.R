# this data is downloaded from Kaggle.com
# I am going to do a CEA analysis on this data
# At first we need to load this data and inspect it

data <- read.csv("Drug.csv")
data()
colnames(data)

#ensuring numbers are as numeric values and text are characters
str(data)

#checking for missing values in the data shows out data is clean and easy to use
missing_values <- sum(is.na(data))
print(missing_values)

# now we need to select a condition to limit our data and continue analysis
# Counting each condition and make a dataframe
condition_counts <- table(data$Condition)
condition_counts_df <- as.data.frame(condition_counts)
colnames(condition_counts_df) <- c("Condition", "Count")
#Sorting the dataframe to find the most prevalent condition
condition_counts_df <- condition_counts_df[order(-condition_counts_df$Count), ]
print(condition_counts_df)

# Now we found that the most prevalent condition is "hypertension"
# Let's filter our dataframe to this condition
hypertention <- subset(data, Condition == "hypertension")
hypertention
---------------------------------------------------------------------
# Now let's continue the analysis on this filtered subset
# Let's check the distribution of our numeric values by histograms
  
# Load the necessary libraries
library(ggplot2)
library(viridis)

# Specify the numeric variables for which you want to create histograms
numeric_vars <- c("EaseOfUse", "Effective", "Price", "Reviews", "Satisfaction")

# Create an empty list to store the plots
plot_list <- list()

# Create a separate histogram plot for each numeric variable
for (i in 1:length(numeric_vars)) {
  # Create a histogram plot using ggplot2
  p <- ggplot(data = hypertention, aes(x = .data[[numeric_vars[i]]])) +
    geom_histogram(fill = viridis_pal()(1), color = "white", bins = 20) +
    labs(x = numeric_vars[i], y = "Frequency") +
    theme_minimal() +
    theme(legend.position = "none") +
    theme_minimal() +
    theme(
      strip.text = element_text(size = 14),
      axis.title = element_text(size = 14, face = "bold"),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      strip.background = element_rect(fill = "lightgray", color = "white"),
    )
  
# Append the plot to the list
  plot_list[[i]] <- p
}

# Arrange the plots in a 2x2 grid
library(gridExtra)
arranged_plots <- grid.arrange(grobs = plot_list, ncol = 2)

# Let's save this figure for now
ggsave("histograms.pdf", arranged_plots, width = 10, height = 10, units = "in")

-------------------------------------------------------------------------------
# We have 5 numeric values.
# Now I want to generate the pairwise correlation plot between them


# Specify the numeric variables for which you want to create pairwise correlations
numeric_vars <- c("EaseOfUse", "Effective", "Price", "Reviews", "Satisfaction")

# Create an empty list to store the scatterplots
scatterplot_list <- list()

# Create pairwise scatterplots for each pair of numeric variables
for (i in 1:length(numeric_vars)) {
  for (j in 1:length(numeric_vars)) {
    if (i != j) {
      p <- ggplot(data = hypertention, aes(x = .data[[numeric_vars[i]]], y = .data[[numeric_vars[j]]])) +
        geom_point(aes(color = .data[[numeric_vars[i]]]), alpha = 0.7) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
        labs(x = numeric_vars[i], y = numeric_vars[j]) +
        theme_minimal()
      
      # Calculate R-squared
      fit <- lm(hypertention[[numeric_vars[j]]] ~ hypertention[[numeric_vars[i]]])
      r_squared <- summary(fit)$r.squared
      r_squared_label <- sprintf("R^2 = %.2f", r_squared)
      
      # Add R-squared text
      p <- p + annotate("text", x = max(hypertention[[numeric_vars[i]]]), y = max(hypertention[[numeric_vars[j]]]), 
                        label = r_squared_label, hjust = 1, vjust = 1)
      
      scatterplot_list[[length(scatterplot_list) + 1]] <- p
    }
  }
}

# Arrange the scatterplots in an 8x4 grid
arranged_scatterplots <- grid.arrange(grobs = scatterplot_list, ncol = 4)

# Save the arranged scatterplots as a high-quality PDF
ggsave("pairwise_correlations_with_regression.pdf", arranged_scatterplots, width = 14, height = 10, units = "in")


# Save the arranged scatterplots as a high-quality PDF
ggsave("pairwise_correlations_with_regression.pdf", arranged_scatterplots, width = 14, height = 10, units = "in")
------------------------------------------------------------------------------------------------
# Now let's calculate ICER for one of the anti-hypertension drugs
# Let's use the satisfaction col to sort rows and see which one is the most liked
# Then we compare all drugs on our list with that to see which one give us the highest ICER

# Sorting anti-hypertension drugs
hypertention <- hypertention[order(-hypertention$Satisfaction), ]
hypertention

# It looks like Verapamil has the most Satisfaction among patients
--------------------------------------------------------------------------------------------------
# Now Let's compare it to all other drugs by calculation of ICER between them

# Load the necessary libraries
library(openxlsx)

# Find the drug with the highest satisfaction and effectiveness
max_satisfaction_effectiveness <- max(hypertention$Satisfaction)
verapamil_data <- hypertention %>%
  filter(Drug == "Verapamil", Satisfaction == max_satisfaction_effectiveness, Effective == 5)

# Initialize an empty vector to store ICER values
icer_values <- numeric()

# Iterate over rows
for (i in 1:nrow(hypertention)) {
  current_row <- hypertention[i, ]
  
  # Calculate incremental cost and effectiveness between "Verapamil" and the current drug
  verapamil_effectiveness <- verapamil_data$Effective
  incremental_cost <- sum(current_row$Price) - sum(verapamil_data$Price)
  incremental_effectiveness <- sum(current_row$Effective) - sum(verapamil_data$Effective)
  
  # Calculate ICER
  icer <- incremental_cost / incremental_effectiveness
  
  # Append the ICER value to the vector
  icer_values <- c(icer_values, icer)
}

# Add a new column "ICER to Verapamil" to the dataset and sort it
hypertention$`ICER to Verapamil` <- icer_values

# Save the updated dataset as an Excel file
write.xlsx(hypertention, "hypertention_with_ICER.xlsx")

