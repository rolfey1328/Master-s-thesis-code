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
library(tibble)
library(pracma)
library(FSA)
library(rcompanion)
library(trapz)

setwd("C:\\Users\\rolfe\\Desktop\\Uni\\4th sem\\Results\\Ros bursts\\Supernatant test")
ros <- read.csv("super5dpi.csv", header = TRUE, sep = ",")

ros <- ros %>%
  mutate(across(-Frame, as.numeric))

ros_long <- ros %>%
  pivot_longer(-c(Frame), names_to = "sample", values_to = "luminescence") %>%
  mutate(treatment = gsub("\\d+$", "", sample))


rename_treatments <- function(treatment) {
  case_when(
    treatment == "A" ~ "10 nM elf18 + Mock",
    treatment == "B" ~ "10 nM elf18 + 194",
    treatment == "C" ~ "10 nM elf18 + 201",
    treatment == "D" ~ "10 nM elf18 + 359",
    treatment == "E" ~ "10 nM elf18 + 394",
    treatment == "F" ~ "10 nM elf18 + 404",
    treatment == "G" ~ "100 nM elf18",
    treatment == "H" ~ "Mock",
    treatment == "I" ~ "194",
    treatment == "J" ~ "201",
    treatment == "K" ~ "359",
    treatment == "L" ~ "394",
    treatment == "M" ~ "404",
    treatment == "N" ~ "fresh 10 nM elf18",
    TRUE ~ as.character(treatment)  # Handle any other cases (though should not be necessary)
  )
}

ros_long <- ros_long %>%
  mutate(treatment = rename_treatments(treatment))


sum_luminescence <- ros_long %>%
  group_by(sample) %>%
  summarize(total_luminescence = sum(luminescence))

sum_luminescence <- sum_luminescence %>%
  left_join(ros_long %>% distinct(sample, treatment), by = "sample")

sem_luminescence <- ros_long %>%
  group_by(Frame, treatment) %>%
  summarize(avg_luminescence = mean(luminescence, na.rm = TRUE),
            sem = sd(luminescence, na.rm = TRUE) / sqrt(n()))

# Define controls
controls <- c("100 nM elf18", "10 nM elf18 + Mock", "Mock")

# Define the treatments of interest (including controls)
treatments_of_interest <- unique(c(controls, unique(sem_luminescence$treatment)))

# Filter the sem_luminescence data to only include these treatments and controls
filtered_sem_luminescence <- sem_luminescence %>%
  filter(treatment %in% treatments_of_interest)

# Split data by treatment
split_data_list <- filtered_sem_luminescence %>%
  split(.$treatment)

# Define a function to add controls to each treatment's data
add_controls_to_treatment <- function(treatment_data, controls, sem_luminescence) {
  # Extract the treatment name from the treatment_data
  treatment_name <- unique(treatment_data$treatment)
  
  # Create a filter for the treatment and controls
  relevant_data <- sem_luminescence %>%
    filter(treatment %in% c(treatment_name, controls))
  
  return(relevant_data)
}

# Apply the function to each treatment's data in split_data_list
split_data_with_controls <- lapply(split_data_list, function(df) {
  add_controls_to_treatment(df, controls, sem_luminescence)
})

# Set names for the list based on treatments
names(split_data_with_controls) <- names(split_data_list)

linetypes <- c("solid", "dashed", "dotted", "twodash", "longdash", 
               "dotdash", "1F")

# Define colors for specific treatments and a default color for others
color_map <- c(
  "Mock" = "#073b4c",
  "10 nM elf18 + Mock" = "#118ab2",
  "100 nM elf18" = "#06d6a0",
  "10 nM elf18 + 194" = "#ffdd00",
  "10 nM elf18 + 201" = "#ffc300",
  "10 nM elf18 + 359" = "#ffaa00",
  "10 nM elf18 + 394" = "#ff9500",
  "10 nM elf18 + 404" = "#ff7b00",
  "fresh 10 nM elf18" = "#ffd166",
  "194" = "#ffd166",
  "201" = "#ffd166",
  "359" = "#ffd166",
  "394" = "#ffd166",
  "404" = "#ffd166",
  "default" = "#ffd166"
)

# Define the new names for the treatments
rename_treatments_for_legend <- function(treatment) {
  case_when(
    treatment == "10 nM elf18 + Mock" ~ "10 nM elf18 + Mock",
    treatment == "100 nM elf18" ~ "100 nM elf18",
    grepl("^10 nM elf18 \\+ ", treatment) ~ gsub("10 nM elf18 \\+ ", "10 nM elf18 + ", treatment),
    TRUE ~ as.character(treatment)  # Handle any other cases (though should not be necessary)
  )
}

# Create the 'plots' directory if it doesn't exist
if (!dir.exists("plots")) {
  dir.create("plots")
}

selected_treatments <- c("Mock", "10 nM elf18 + Mock", 
                         "10 nM elf18 + 194", "10 nM elf18 + 201", 
                         "10 nM elf18 + 359", "10 nM elf18 + 394", 
                         "10 nM elf18 + 404")

# Filter the ros_long dataframe to only include the selected treatments
filtered_treatments_df <- filtered_sem_luminescence %>%
  filter(treatment %in% selected_treatments)

filtered_treatments_df <- filtered_treatments_df %>%
  mutate(treatment = factor(treatment, levels = c(
    "Mock", "10 nM elf18 + Mock", "100 nM elf18",
    "10 nM elf18 + 194", "10 nM elf18 + 201", "10 nM elf18 + 359",
    "10 nM elf18 + 394", "10 nM elf18 + 404", "fresh 10 nM elf18",
    "194", "201", "359", "394", "404"
  )))

p <- ggplot(filtered_treatments_df, aes(x =10*(Frame), y = avg_luminescence, color = treatment, linetype = treatment)) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_errorbar(aes(ymin = avg_luminescence - sem, ymax = avg_luminescence + sem), width = 0.2, alpha = 0.5) +
  labs(title = paste("Luminescence Over Frame for Treatment:"),
       x = "Time (Seconds)",
       y = "Luminescence (RLU)") +
  scale_color_manual(values = color_map) +  # Use the color map for colors
  scale_linetype_manual(values = linetypes) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 14),
    plot.title = element_text(size = 18),
    legend.position = "right"  # Move the legend to the bottom for clarity
  )

ggsave(filename = "A5dpi10nMelfcombined_lowres.png", plot = p, width = 10, height = 5)

p

# Function to generate a ggplot for each treatment
plot_luminescence <- function(data, treatment_name) {
  # Rename treatments for the legend
  data <- data %>%
    mutate(treatment = rename_treatments_for_legend(treatment)) %>%
    mutate(treatment = factor(treatment, levels = c(
      "Mock", "10 nM elf18 + Mock", "100 nM elf18",
      "10 nM elf18 + 194", "10 nM elf18 + 201", "10 nM elf18 + 359",
      "10 nM elf18 + 394", "10 nM elf18 + 404", "fresh 10 nM elf18",
      "194", "201", "359", "394", "404"
    )))
  
  ggplot(data, aes(x = Frame, y = avg_luminescence, color = treatment, linetype = treatment)) +
    geom_line(size = 1, alpha = 0.7) +
    geom_errorbar(aes(ymin = avg_luminescence - sem, ymax = avg_luminescence + sem), width = 0.2, alpha = 0.5) +
    labs(title = paste("Luminescence Over Frame for Treatment:", treatment_name),
         x = "Frame",
         y = "Average Luminescence") +
    scale_color_manual(values = color_map) +  # Use the color map for colors
    scale_linetype_manual(values = linetypes) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right"
    )
}

# Generate and save plots for each treatment and its controls
for (treatment_name in names(split_data_with_controls)) {
  data <- split_data_with_controls[[treatment_name]]
  plot <- plot_luminescence(data, treatment_name)
  
  # Save the plot to the 'plots' directory
  ggsave(filename = paste0("plots/5dpiluminescence_plot_", treatment_name, ".png"),
         plot = plot,
         width = 10, height = 6)
}

