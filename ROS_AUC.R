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

setwd("C:\\Users\\rolfe\\Desktop\\Uni\\4th sem\\Results\\Report data\\ROS")

rosaug3_3_auc <- read.csv("rosaug3_3_auc.csv", header = TRUE, sep = ",")
rosaug3_2_auc <- read.csv("rosaug3_2_auc.csv", header = TRUE, sep = ",")
rosaug3_1_auc <- read.csv("rosaug3_1_auc.csv", header = TRUE, sep = ",")
rosaug2_auc <- read.csv("rosaug2_auc.csv", header = TRUE, sep = ",")
rosaug1_2_auc <- read.csv("rosaug1_2_auc.csv", header = TRUE, sep = ",")
rosaug1_1_auc <- read.csv("rosaug1_1_auc.csv", header = TRUE, sep = ",")
rosjul5_1_auc <- read.csv("rosjul5_1_auc.csv", header = TRUE, sep = ",")
rosjul5_2_auc <- read.csv("rosjul5_2_auc.csv", header = TRUE, sep = ",")
rosjul4_auc <- read.csv("rosjul4_auc.csv", header = TRUE, sep = ",")
rosjul2_auc <- read.csv("rosjul2_auc.csv", header = TRUE, sep = ",")
rosjul1_auc <- read.csv("rosjul1_auc.csv", header = TRUE, sep = ",")
rosjun5_auc <- read.csv("rosjun5_auc.csv", header = TRUE, sep = ",")
rosjun2_1_auc <- read.csv("rosjun2_1_auc.csv", header = TRUE, sep = ",")
rosjun2_2_auc <- read.csv("rosjun2_2_auc.csv", header = TRUE, sep = ",")
rosmay1_auc <- read.csv("rosmay1_auc.csv", header = TRUE, sep = ",")

rosaug3_3_auc$filename <- "rosaug3_3_auc.csv"
rosaug3_2_auc$filename <- "rosaug3_2_auc.csv"
rosaug3_1_auc$filename <- "rosaug3_1_auc.csv"
rosaug2_auc$filename <- "rosaug2_auc.csv"
rosaug1_1_auc$filename <- "rosaug1_1_auc.csv"
rosjul5_1_auc$filename <- "rosjul5_1_auc.csv"
rosjul5_2_auc$filename <- "rosjul5_2_auc.csv"
rosjul4_auc$filename <- "rosjul4_auc.csv"
rosjul2_auc$filename <- "rosjul2_auc.csv"
rosjul1_auc$filename <- "rosjul1_auc.csv"
rosjun5_auc$filename <- "rosjun5_auc.csv"
rosjun2_1_auc$filename <- "rosjun2_1_auc.csv"
rosjun2_2_auc$filename <- "rosjun2_2_auc.csv"
rosmay1_auc$filename <- "rosmay1_auc.csv"



combined_data <- bind_rows(rosaug3_3_auc, rosaug3_2_auc, rosaug3_1_auc,
                           rosaug2_auc,
                           #rosaug1_2_auc,
                           rosaug1_1_auc,
                           rosjul5_1_auc,
                           rosjul5_2_auc,
                           rosjul4_auc,
                           rosjul2_auc, 
                           rosjul1_auc,
                           rosjun5_auc,
                           rosjun2_1_auc, rosjun2_2_auc, 
                           #rosmay1_auc
                           )


sample_ids <- unique(combined_data$treatment[!grepl("dead|Mock", combined_data$treatment, ignore.case = TRUE)])

# Process the data by filtering, modifying treatment names, and adding sample types
combined_data_long <- combined_data %>%
  
  # Filter for rows where treatment matches sample_ids, sample_ids + "dead", or "Mock"
  filter(treatment %in% c(sample_ids, paste0(sample_ids, "dead"), "Mock")) %>%
  
  # Add sample_type and sample_id columns based on treatment
  mutate(
    sample_type = case_when(
      grepl("dead", treatment, ignore.case = TRUE) ~ "Dead",
      grepl("Mock", treatment, ignore.case = TRUE) ~ "Mock",
      TRUE ~ "Live"
    ),
    # Clean the treatment names by removing "dead" if present
    sample_id = sub("dead", "", treatment, ignore.case = TRUE)
  ) %>%
  
# Set the levels of sample_type to ensure the correct order for factors
  mutate(sample_type = factor(sample_type, levels = c("Mock", "Live", "Dead")))

combined_data_long$treatment <- as.factor(combined_data_long$treatment)

combined_data_long <- combined_data_long %>%
  mutate(treatment = factor(treatment, levels = c(
    "Mock", 
    "elf100nM",
    
    # Numerical treatments and their "dead" versions
    "9", "9dead",
    "11", "11dead",
    "82", "82dead",
    "126", "126dead",
    "127", "127dead",
    "129", "129dead",
    "130", "130dead",
    "131", "131dead",
    "137", "137dead",
    "151", "151dead",
    "159", "159dead",
    "160", "160dead",
    "171", "171dead",
    "182", "182dead",
    "194", "194dead",
    "201", "201dead",
    "216", "216dead",
    "225", "225dead",
    "280", "280dead",
    "296", "296dead",
    "306", "306dead",
    "307", "307dead",
    "311", "311dead",
    "337", "337dead",
    "344", "344dead",
    "347", "347dead",
    "359", "359dead",
    "394", "394dead",
    "404", "404dead",
    "405", "405dead",
    "434", "434dead",
    "436", "436dead"
 )))

colour_map <- c(
  "Mock" = "#073b4c",
  "Live" = "#118ab2",      # Any treatment without "dead"
  "Dead" = "#BFDBF7",     # Any treatment with "dead"
  
  # Assign colors to specific treatments
  "359" = "#118ab2",
  "359dead" = "#BFDBF7",
  "394" = "#118ab2",
  "394dead" = "#BFDBF7",
  "404" = "#118ab2",
  "404dead" = "#BFDBF7",
  "405" = "#118ab2",
  "405dead" = "#BFDBF7",
  "434" = "#118ab2",
  "434dead" = "#BFDBF7",
  "436" = "#118ab2",
  "436dead" = "#BFDBF7",
  "296" = "#118ab2",
  "296dead" = "#BFDBF7",
  "306" = "#118ab2",
  "306dead" = "#BFDBF7",
  "307" = "#118ab2",
  "307dead" = "#BFDBF7",
  "311" = "#118ab2",
  "311dead" = "#BFDBF7",
  "337" = "#118ab2",
  "337dead" = "#BFDBF7",
  "344" = "#118ab2",
  "344dead" = "#BFDBF7",
  "347" = "#118ab2",
  "347dead" = "#BFDBF7",
  "137" = "#118ab2",
  "137dead" = "#BFDBF7",
  "159" = "#118ab2",
  "159dead" = "#BFDBF7",
  "160" = "#118ab2",
  "160dead" = "#BFDBF7",
  "171" = "#118ab2",
  "171dead" = "#BFDBF7",
  "182" = "#118ab2",
  "182dead" = "#BFDBF7",
  "216" = "#118ab2",
  "216dead" = "#BFDBF7",
  "225" = "#118ab2",
  "225dead" = "#BFDBF7",
  "11" = "#118ab2",
  "11dead" = "#BFDBF7",
  "129" = "#118ab2",
  "129dead" = "#BFDBF7",
  "130" = "#118ab2",
  "130dead" = "#BFDBF7",
  "131" = "#118ab2",
  "131dead" = "#BFDBF7",
  "151" = "#118ab2",
  "151dead" = "#BFDBF7",
  "82" = "#118ab2",
  "82dead" = "#BFDBF7",
  "9" = "#118ab2",
  "9dead" = "#BFDBF7",
  "126" = "#118ab2",
  "126dead" = "#BFDBF7",
  "127" = "#118ab2",
  "127dead" = "#BFDBF7",
  "280" = "#118ab2",
  "280dead" = "#BFDBF7",
  "194" = "#118ab2",
  "194dead" = "#BFDBF7",
  "201" = "#118ab2",
  "201dead" = "#BFDBF7"
)

new_labels <- c(
  "359dead" = "Boiled 359",
  "394dead" = "Boiled 394",
  "404dead" = "Boiled 404",
  "405dead" = "Boiled 405",
  "434dead" = "Boiled 434",
  "436dead" = "Boiled 436",
  "296dead" = "Boiled 296",
  "306dead" = "Boiled 306",
  "307dead" = "Boiled 307",
  "311dead" = "Boiled 311",
  "337dead" = "Boiled 337",
  "344dead" = "Boiled 344",
  "347dead" = "Boiled 347",
  "137dead" = "Boiled 137",
  "159dead" = "Boiled 159",
  "160dead" = "Boiled 160",
  "171dead" = "Boiled 171",
  "182dead" = "Boiled 182",
  "216dead" = "Boiled 216",
  "225dead" = "Boiled 225",
  "11dead" = "Boiled 11",
  "129dead" = "Boiled 129",
  "130dead" = "Boiled 130",
  "131dead" = "Boiled 131",
  "151dead" = "Boiled 151",
  "82dead" = "Boiled 82",
  "9dead" = "Boiled 9",
  "126dead" = "Boiled 126",
  "127dead" = "Boiled 127",
  "280dead" = "Boiled 280",
  "194dead" = "Boiled 194",
  "201dead" = "Boiled 201")


# Step 1: Split data by sample_id
split_data_by_id <- combined_data_long %>%
  group_split(sample_id)

# Create names for the split data list based on sample_id
names(split_data_by_id) <- sapply(split_data_by_id, function(x) unique(x$sample_id))

split_data_list <- lapply(names(split_data_by_id), function(name) {
  
  # Get the current dataset
  df <- split_data_by_id[[name]]
  filenames <- unique(df$filename)  # Get the filenames present in this sample_id group
  
  # Debugging: Print filenames for the current group
  print(paste("Processing sample_id:", name))
  print("Filenames in this group:")
  print(filenames)
  
  # Filter for mock treatments that match the filenames in this sample_id group
  mock_data <- combined_data_long %>%
    filter(tolower(sample_id) == "mock" & filename %in% filenames)  # Using tolower to handle case-sensitivity
  
  # Debugging: Print mock data being added
  if (nrow(mock_data) == 0) {
    print("No Mock data found for these filenames.")
  } else {
    print("Mock data found:")
    print(mock_data)
    
    # Adjust the sample_id for Mock to match the current element's name
    mock_data <- mock_data %>%
      mutate(sample_id = name)  # Set sample_id to match the name of the current element
  }
  
  # Combine live, dead, and mock treatments
  combined_df <- bind_rows(df, mock_data)
  
  # Add replicate number based on filename
  combined_df <- combined_df %>%
    mutate(
      replicate = as.numeric(factor(filename))  # Convert filenames to numeric factors
    )
  
  return(combined_df)
})

anova_results <- list()

split_data_list <- lapply(split_data_list, function(data) {
  data[, !(names(data) %in% "AUC_diff_final")]
})

split_data_list <- split_data_list[-34]
split_data_list <- split_data_list[-33]

# Loop through each dataset in the split_data_list
for (i in seq_along(split_data_list)) {
  
  # Get the current dataset from the split data list
  current_data <- split_data_list[[i]]
  
  # Perform ANOVA
  weights_model <- lm(AUC_diff ~ treatment, data = current_data)
  waov <- aov(AUC_diff ~ treatment, data = current_data)
  
  # Perform post-hoc Tukey test
  waovtukey <- TukeyHSD(waov)
  
  # Get compact letter display from Tukey test
  weights_cld <- multcompLetters4(waov, waovtukey)
  cld <- as.data.frame.list(weights_cld$treatment)
  cld$treatment <- rownames(cld)
  
  # Merge the letters with sample_data
  current_data <- merge(current_data, cld, by = "treatment")
  
  # Store the data and results
  anova_results[[i]] <- list(data = current_data, cld = cld)
}


for (i in seq_along(split_data_list)) {
  split_data_list[[i]]$replicate <- as.factor(split_data_list[[i]]$replicate)
}

replicate_color_map <- c("1" = "#FFC15E", "2" = "#F7934C", "3" = "#F25C54")
replicate_shapes <- c("1" = 16, "2" = 17, "3" = 18)

# Initialize an empty list to store all the plots
plot_list <- list()

# Loop through each dataset in the split_data_list
for (i in seq_along(split_data_list)) {
  
  # Get the current dataset for this sample_id
  current_data <- split_data_list[[i]]
  
  # Get the sample_id for the current dataset (this will be used for the plot title)
  sample_id <- unique(current_data$sample_id)
  
  # Retrieve the CLD data for the current dataset
  cld <- anova_results[[i]]$cld
  
  # Create the ggplot comparing treatments (Mock, Live, Dead) for the current treatment
  p <- ggplot(current_data, aes(x = treatment, y = AUC_diff, fill = treatment)) +
    geom_boxplot(outlier.shape = NA) +  # Remove outliers
    geom_jitter(aes(color = replicate, shape = replicate), width = 0.2, size = 1.5) +  # Set jitter points to color and shape by replicate
    ggtitle(paste("Isolate", sample_id)) +
    ylab("Relative AUC") +
    xlab("Treatment") +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right"
    ) +
    scale_x_discrete(labels = new_labels) +  # Apply new labels to the x-axis
    guides(
      fill = "none",
      color = guide_legend(title = "Replicate"),  # Keep the legend for replicate with a title
      shape = guide_legend(title = "Replicate")  # Keep the legend for replicate with a title
    )  +
    scale_fill_manual(values = colour_map) +  # Use scale_fill_manual for fill aesthetics
    scale_color_manual(values = replicate_color_map) +  # Use scale_color_manual for color aesthetics
    scale_shape_manual(values = replicate_shapes) +  # Use scale_shape_manual for shape aesthetics
    # Add CLD labels
    geom_text(data = cld, aes(x = treatment, y = Inf, label = Letters), 
              vjust = 1.5, hjust = 0.5, size = 5, color = "black")
  
  # Store the plot in the list with the name of the sample_id
  plot_list[[sample_id]] <- p
}

# Loop to display each plot and pause for inspection
for (i in seq_along(plot_list)) {
  sample_id <- names(plot_list)[i]  # Get the sample_id for the current plot
  print(paste("Displaying plot for sample_id:", sample_id))
  print(plot_list[[sample_id]])  # Print the plot
  
  # Pause between plots for inspection (Press ENTER to continue)
  readline(prompt="Press [Enter] to view the next plot...")
}

combined_plots <- wrap_plots(plot_list, ncol = 4)
print(combined_plots)

ggsave("anova_plots_with_relative_weight.png", plot = combined_plots, width = 20, height = 20)

# Specify the isolate ID
isolate_id <- "130"

# Extract the plot for the specific isolate
individual_plot <- plot_list[[isolate_id]]

individual_plot
ggsave("130.png", plot = individual_plot, width = 5, height = 5)


# Save each plot as an image file
for (sample_id in names(plot_list)) {
  ggsave(filename = paste0(sample_id, "_treatment_comparison.png"), plot = plot_list[[sample_id]])
}

# Initialize an empty list to store average AUC_diff values
avg_auc_list <- list()

# Loop through each dataset in the split_data_list
for (i in seq_along(split_data_list)) {
  
  # Get the current dataset
  current_data <- split_data_list[[i]]
  
  # Get the sample_id for the current dataset
  sample_id <- unique(current_data$sample_id)
  
  # Calculate the average AUC_diff for each treatment
  avg_auc <- current_data %>%
    group_by(treatment) %>%
    summarise(
      avg_AUC_diff = mean(AUC_diff, na.rm = TRUE),  # Calculate mean, ignoring NA values
      n = n()  # Get the sample size
    ) %>%
    mutate(sample_id = sample_id)  # Add the sample_id as a column
  
  # Add the summary data to the list
  avg_auc_list[[i]] <- avg_auc
}

# Combine all the data into a single data frame
avg_auc_df <- bind_rows(avg_auc_list)


