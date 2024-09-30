library(ggplot2)
library(cowplot)
library(dplyr)
library(car)
library(pgirmess)
library(PMCMRplus)
library(dunn.test)
library(multcompView)
library(tidyr)
library(viridis)
library(RColorBrewer)
library(agricolae)
library(multcompView)
library(patchwork)

setwd("C:\\Users\\rolfe\\Desktop\\Uni\\4th sem\\Results\\Report data\\SGI")

jul4 <- read.csv("SGIJul4.csv", header = TRUE, sep = ",")
jul5 <- read.csv("SGIJul5.csv", header = TRUE, sep = ",")
aug1 <- read.csv("SGIAug1.csv", header = TRUE, sep = ",")
jun3 <- read.csv("SGIJun3 10nM 1%suc.csv", header = TRUE, sep = ",")

jul4 <- jul4 %>% mutate(replicate = 1)
jul5 <- jul5 %>% mutate(replicate = 2)
aug1 <- aug1 %>% mutate(replicate = 3)

sgicomb <- bind_rows(jul4, jul5, aug1)

sgicomb <- sgicomb %>%
  group_by(replicate) %>%
  mutate(relative_weight = `weight.mg.` - mean(`weight.mg.`[treatment == "mock"])) %>%
  ungroup()

##replacing rep 1 100nmelf with jun3, retain relative weight from respective mock
sgicomb <- sgicomb %>%
  filter(!(replicate == 1 & treatment == "100nmelf"))
jun3_100nmelf <- jun3 %>%
  filter(treatment == "100nmelf") %>%
  mutate(replicate = 1) 
sgicomb <- bind_rows(sgicomb, jun3_100nmelf)

sample_ids <- unique(sgicomb$treatment[!grepl("dead|mock|100nmelf", sgicomb$treatment)])

sgicomb_long <- sgicomb %>%
  filter(treatment %in% c(sample_ids, paste0(sample_ids, "dead"), "mock", "100nmelf")) %>%
  mutate(
    sample_type = case_when(
      grepl("dead", treatment) ~ "Dead",
      treatment %in% c("mock", "100nmelf") ~ treatment,
      TRUE ~ "Live"
    ),
    sample_id = sub("dead", "", treatment)
  ) %>%
  # Set the levels of sample_type to ensure the correct order
  mutate(sample_type = factor(sample_type, levels = c("mock", "100nmelf", "Live", "Dead")))

sample_counts <- sgicomb_long %>%
  group_by(treatment) %>%
  summarise(sample_count = n())

# View the counts
print(sample_counts)

anova_results <- list()

# Loop through each sample ID and perform ANOVA using `relative_weight`
for (id in sample_ids) {
  # Filter data for the current sample
  sample_data <- sgicomb_long %>%
    filter(sample_id %in% c(id, paste0(id, "dead"), "mock", "100nmelf"))
  
  # Perform ANOVA
  weights_model <- lm(relative_weight ~ sample_type, data = sample_data)
  waov <- aov(relative_weight ~ sample_type, data = sample_data)
  
  # Perform post-hoc Tukey test
  waovtukey <- TukeyHSD(waov)
  
  # Get compact letter display from Tukey test
  weights_cld <- multcompLetters4(waov, waovtukey)
  cld <- as.data.frame.list(weights_cld$sample_type)
  cld$sample_type <- rownames(cld)
  
  # Merge the letters with sample_data
  sample_data <- merge(sample_data, cld, by = "sample_type")
  
  # Store the data and results
  anova_results[[id]] <- list(data = sample_data, cld = cld)
}

# Initialize an empty list to store plots
plot_list <- list()

# Loop through each sample ID and create a plot
for (id in sample_ids) {
  # Get data and CLD for the current sample
  sample_data <- anova_results[[id]]$data
  cld <- anova_results[[id]]$cld
  
  # Create the plot using `relative_weight`
  p <- ggplot(sample_data, aes(x = sample_type, y = relative_weight, fill = sample_type)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color = as.factor(replicate), shape = as.factor(replicate)), width = 0.2) +  # Adding jittered points with color and shape
    xlab(NULL) + 
    ggtitle(paste("Isolate", id)) + 
    ylab("Relative Weight (mg)") +
    theme_minimal() +
    scale_fill_manual(values = c("mock" = "#E1E5F2", "100nmelf" = "#BFDBF7", "Live" = "#1F7A8C", "Dead" = "#022B3A")) +
    scale_color_manual(values = c("1" = "#FFC15E", "2" = "#F7934C", "3" = "#F25C54")) +  # Customize colors for replicates
    scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 18)) +
    scale_x_discrete(
      labels = c("mock" = "mock", "100nmelf" = "100nm elf18", "Live" = "Live", "Dead" = "Dead")
    ) +  # Customize shapes for replicates
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 14),
      plot.title = element_text(size = 18),
      legend.position = "right"  # Move the legend to the bottom for clarity
    ) +
    guides(
      fill = "none",  # Remove the legend for sample_type
      color = guide_legend(title = "Replicate"),  # Keep the legend for replicate with a title
      shape = guide_legend(title = "Replicate")  # Keep the legend for replicate with a title
    ) +
    geom_text(data = cld, aes(label = Letters, y = max(sample_data$relative_weight) + 0.1), 
              position = position_dodge(width = 0.8), vjust = -0.5) +
    ylim(-30, 61)
  
  # Store the plot
  plot_list[[id]] <- p
}

# Combine all plots into a single layout
combined_plots <- wrap_plots(plot_list, ncol = 4)  # Adjust ncol for desired number of columns

# Save the combined plot to a file
ggsave("anova_plots_with_relative_weight.png", plot = combined_plots, width = 20, height = 20)

##code for individual plots
# Initialize an empty list to store plots
plot_list_dynamic_size <- list()

# Loop through each sample ID and create a plot
for (id in sample_ids) {
  # Get data and CLD for the current sample
  sample_data <- anova_results[[id]]$data
  cld <- anova_results[[id]]$cld
  
  # Create the plot using `relative_weight`
  p <- ggplot(sample_data, aes(x = sample_type, y = relative_weight, fill = sample_type)) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color = as.factor(replicate), shape = as.factor(replicate)), width = 0.2) +  # Adding jittered points with color and shape
    xlab(NULL) + 
    ggtitle(paste("Isolate", id)) + 
    ylab("Relative Weight (mg)") +
    theme_bw() +
    scale_fill_manual(values = c("mock" = "#E1E5F2", "100nmelf" = "#BFDBF7", "Live" = "#1F7A8C", "Dead" = "#022B3A")) +
    scale_color_manual(values = c("1" = "#FFC15E", "2" = "#F7934C", "3" = "#F25C54")) +  # Customize colors for replicates
    scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 18)) +
    scale_x_discrete(
      labels = c("mock" = "Mock", "100nmelf" = "100 nM elf18", "Live" = "Live", "Dead" = "Dead")
    ) +  # Customize shapes for replicates
    theme(
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 14),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 14),
      plot.title = element_text(size = 14),
      legend.position = "right"  # Move the legend to the bottom for clarity
    ) +
    guides(
      fill = "none",  # Remove the legend for sample_type
      color = guide_legend(title = "Replicate"),  # Keep the legend for replicate with a title
      shape = guide_legend(title = "Replicate")  # Keep the legend for replicate with a title
    )
  
  # Store the plot in the list
  plot_list_dynamic_size[[id]] <- p
}

# Specify the isolate ID
isolate_id <- "216"

# Extract the plot for the specific isolate
individual_plot <- plot_list_dynamic_size[[isolate_id]]

# Get the data and CLD for the specified isolate
sample_data <- anova_results[[isolate_id]]$data
cld <- anova_results[[isolate_id]]$cld

# Calculate the maximum y value and add a buffer for the ylim
max_y <- max(sample_data$relative_weight, na.rm = TRUE)
buffer <- 0.1 * (max_y - min(sample_data$relative_weight, na.rm = TRUE))  # 10% of the range
ylim_max <- max_y + buffer  # New upper limit with buffer

# Update the individual plot with the correct max_y value and adjusted ylim
individual_plot <- individual_plot +
  geom_text(data = cld,
            aes(label = Letters, y = max_y),
            size = 5,
            position = position_dodge(width = 0.8), vjust = -0.5) +
  ylim(c(NA, ylim_max))  # Set dynamic ylim

# Display the updated plot
print(individual_plot)

ggsave("216_with_relative_weight.png", plot = individual_plot, width = 5, height = 5)




##calculating percent changes in weight
# Calculate average mock weight for each replicate
average_weight <- sgicomb_long %>%
  group_by(sample_id, sample_type) %>%
  summarize(
    avg__weight = mean(weight.mg., na.rm = TRUE),
    sd__weight = sd(weight.mg., na.rm = TRUE),
    n = n()
  ) %>%
  arrange(sample_id, sample_type)

print(average_weight)

write.csv(average_weight, "average_weight_summary.csv", row.names = FALSE)
