library(dplyr)


setwd("C:\\Users\\rolfe\\Desktop\\Uni\\4th sem\\Results\\Report data\\SGI")

SGItotals <- read.csv("SGItotals.csv", header = TRUE, sep = ",")

SGItotals <- SGItotals %>%
  filter(!is.na(Phylum) & Phylum != "", !is.na(state) & state != "")

category_counts <- table(SGItotals$state)

# Calculate the total number of rows in the dataset
total_count <- nrow(SGItotals)

# Calculate the percentage for each category
category_percentages <- (category_counts / total_count) * 100

# Print the percentages
category_percentages


# Filter out cases where counts are zero
filtered_SGItotals <- SGItotals %>%
  group_by(Phylum, state) %>%
  filter(n() > 0) %>%
  ungroup()

# Perform Fisher's Exact Test again for valid data
fisher_results_filtered <- list()

for (phylum in unique(filtered_SGItotals$Phylum)) {
  for (state in unique(filtered_SGItotals$state)) {
    # Create a 2x2 contingency table
    contingency_table <- matrix(c(
      sum(filtered_SGItotals$Phylum == phylum & filtered_SGItotals$state == state),      # Count of specific phylum in the state
      sum(filtered_SGItotals$Phylum == phylum & filtered_SGItotals$state != state),      # Count of specific phylum in other states
      sum(filtered_SGItotals$Phylum != phylum & filtered_SGItotals$state == state),      # Count of other phyla in the state
      sum(filtered_SGItotals$Phylum != phylum & filtered_SGItotals$state != state)       # Count of other phyla in other states
    ), nrow = 2, byrow = TRUE)
    
    # Check if table is valid for Fisher's test (i.e., no zero counts in all cells)
    if (all(contingency_table > 0)) {
      fisher_result <- fisher.test(contingency_table)
      result_name <- paste(phylum, state, sep = "_")
      fisher_results_filtered[[result_name]] <- fisher_result
    }
  }
}

# Extract p-values and create a dataframe
p_values_df_filtered <- data.frame(
  Phylum = character(),
  State = character(),
  PValue = numeric(),
  stringsAsFactors = FALSE
)

for (result_name in names(fisher_results_filtered)) {
  p_values_df_filtered <- rbind(p_values_df_filtered, data.frame(
    Phylum = strsplit(result_name, "_")[[1]][1],
    State = strsplit(result_name, "_")[[1]][2],
    PValue = fisher_results_filtered[[result_name]]$p.value
  ))
}




## code for chi square with monte carlo simulation
# Filter out any rows with missing values if needed
SGItotals <- SGItotals %>%
  filter(!is.na(Family) & Family != "", !is.na(state) & state != "")

SGItotals <- SGItotals[SGItotals$Phylum != "Firmicutes", ]

# Create the contingency table
contingency_table <- table(SGItotals$Phylum, SGItotals$state)
print(contingency_table)

# Perform the Chi-Square Test with Monte Carlo simulation
chi_square_test_montecarlo <- chisq.test(contingency_table, simulate.p.value = TRUE)

# Output the results
print(chi_square_test_montecarlo)

# Extract standardized residuals if needed
standardized_residuals <- chi_square_test_montecarlo$stdres
print(standardized_residuals)


##code for sap presence

# Filter out rows where Phylum is missing or empty
SGItotals <- read.csv("SGItotals.csv", header = TRUE, sep = ",")

# Filter out rows where Phylum is missing or empty
SGItotals <- SGItotals %>%
  filter(!is.na(Phylum) & Phylum != "")


min_samples_per_phylum <- 5
SGItotals <- SGItotals %>%
  group_by(Phylum) %>%
  filter(n() >= min_samples_per_phylum)

# Perform Fisher's Exact Test for all data without additional filtering
contingency_table <- table(SGItotals$Phylum, SGItotals$sap_positive)
fisher_test <- fisher.test(contingency_table)
print(fisher_test)
print(contingency_table)

phylum_list <- unique(SGItotals$Phylum)
for (phylum in phylum_list) {
  subset_table <- table(SGItotals$Phylum == phylum, SGItotals$sap_positive)
  fisher_test_subset <- fisher.test(subset_table)
  print(paste("Phylum:", phylum))
  print(fisher_test_subset$p.value)
}
