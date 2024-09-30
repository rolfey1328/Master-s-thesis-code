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

rosaug3_3_kinetics <- read.csv("rosaug3_3_kinetics.csv", header = TRUE, sep = ",")
rosaug3_2_kinetics <- read.csv("rosaug3_2_kinetics.csv", header = TRUE, sep = ",")
rosaug3_1_kinetics <- read.csv("rosaug3_1_kinetics.csv", header = TRUE, sep = ",")
rosaug2_kinetics <- read.csv("rosaug2_kinetics.csv", header = TRUE, sep = ",")
rosaug1_2_kinetics <- read.csv("rosaug1_2_kinetics.csv", header = TRUE, sep = ",")
rosaug1_1_kinetics <- read.csv("rosaug1_1_kinetics.csv", header = TRUE, sep = ",")
rosjul5_1_kinetics <- read.csv("rosjul5_1_kinetics.csv", header = TRUE, sep = ",")
rosjul5_2_kinetics <- read.csv("rosjul5_2_kinetics.csv", header = TRUE, sep = ",")
rosjul4_kinetics <- read.csv("rosjul4_kinetics.csv", header = TRUE, sep = ",")
rosjul2_kinetics <- read.csv("rosjul2_kinetics.csv", header = TRUE, sep = ",")
rosjul1_kinetics <- read.csv("rosjul1_kinetics.csv", header = TRUE, sep = ",")
rosjun5_kinetics <- read.csv("rosjun5_kinetics.csv", header = TRUE, sep = ",")
rosjun2_1_kinetics <- read.csv("rosjun2_1_kinetics.csv", header = TRUE, sep = ",")
rosjun2_2_kinetics <- read.csv("rosjun2_2_kinetics.csv", header = TRUE, sep = ",")
rosmay1_kinetics <- read.csv("rosmay1_kinetics.csv", header = TRUE, sep = ",")


combined_data <- bind_rows(rosaug3_3_kinetics, rosaug3_2_kinetics, rosaug3_1_kinetics,
                           rosaug2_kinetics,rosaug1_2_kinetics,rosaug1_1_kinetics,
                           rosjul5_1_kinetics,rosjul5_2_kinetics,rosjul4_kinetics,
                           rosjul2_kinetics, rosjul1_kinetics,rosjun5_kinetics,
                           rosjun2_1_kinetics, rosjun2_2_kinetics, rosmay1_kinetics)

sample_ids <- unique(combined_data$treatment[!grepl("dead|mock", combined_data$treatment)])

exclude_files <- c("rosaug1_2_kinetics.csv", "rosmay1_kinetics.csv")

# Filter out rows where the filename is in the exclude_files list
combined_data_long <- combined_data %>%
  # Optionally, add a column with filenames if not already present
  # mutate(filename = extract_filename_from_some_column_or_logic()) %>%
  filter(!filename %in% exclude_files) %>%
  filter(treatment %in% c(sample_ids, paste0(sample_ids, "dead"), "mock")) %>%
  mutate(
    sample_type = case_when(
      grepl("dead", treatment) ~ "Dead",
      treatment == "mock" ~ "Mock",
      TRUE ~ "Live"
    ),
    sample_id = sub("dead", "", treatment)
  ) %>%
  # Set the levels of sample_type to ensure the correct order
  mutate(sample_type = factor(sample_type, levels = c("Mock", "Live", "Dead")))

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

# Step 1: Split data by sample_id
split_data_by_id <- combined_data_long %>%
  group_split(sample_id)

# Create names for the split data list based on sample_id
names(split_data_by_id) <- unique(combined_data_long$sample_id)

# Step 2: Add mock treatments to each sample_id group
split_data_list <- lapply(split_data_by_id, function(df) {
  filenames <- unique(df$filename)  # Get the filenames present in this sample_id group
  
  # Filter for mock treatments that match the filenames in this sample_id group
  mock_data <- combined_data_long %>%
    filter(sample_id == "Mock" & filename %in% filenames)
  
  # Combine live, dead, and mock treatments
  combined_df <- bind_rows(df, mock_data)
  
  # Add replicate number based on filename
  combined_df <- combined_df %>%
    mutate(
      replicate = as.numeric(factor(filename))  # Convert filenames to numeric factors
    )
  
  return(combined_df)
})

# Extract and display the first vector to check
first_vector <- split_data_list[[11]]

# Ensure the plots directory exists
if (!dir.exists("plots")) {
  dir.create("plots")
}

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

colour_map <- c(
  "Mock" = "#073b4c",
  "Live" = "#118ab2",      # Any treatment without "dead"
  "Dead" = "#BFDBF7",     # Any treatment with "dead"
  
  # Assign colors to specific treatments
  "359" = "#118ab2",
  "359dead" = "#06d6a0",
  "394" = "#118ab2",
  "394dead" = "#06d6a0",
  "404" = "#118ab2",
  "404dead" = "#06d6a0",
  "405" = "#118ab2",
  "405dead" = "#06d6a0",
  "434" = "#118ab2",
  "434dead" = "#06d6a0",
  "436" = "#118ab2",
  "436dead" = "#06d6a0",
  "296" = "#118ab2",
  "296dead" = "#06d6a0",
  "306" = "#118ab2",
  "306dead" = "#06d6a0",
  "307" = "#118ab2",
  "307dead" = "#06d6a0",
  "311" = "#118ab2",
  "311dead" = "#06d6a0",
  "337" = "#118ab2",
  "337dead" = "#06d6a0",
  "344" = "#118ab2",
  "344dead" = "#06d6a0",
  "347" = "#118ab2",
  "347dead" = "#06d6a0",
  "137" = "#118ab2",
  "137dead" = "#06d6a0",
  "159" = "#118ab2",
  "159dead" = "#06d6a0",
  "160" = "#118ab2",
  "160dead" = "#06d6a0",
  "171" = "#118ab2",
  "171dead" = "#06d6a0",
  "182" = "#118ab2",
  "182dead" = "#06d6a0",
  "216" = "#118ab2",
  "216dead" = "#06d6a0",
  "225" = "#118ab2",
  "225dead" = "#06d6a0",
  "11" = "#118ab2",
  "11dead" = "#06d6a0",
  "129" = "#118ab2",
  "129dead" = "#06d6a0",
  "130" = "#118ab2",
  "130dead" = "#06d6a0",
  "131" = "#118ab2",
  "131dead" = "#06d6a0",
  "151" = "#118ab2",
  "151dead" = "#06d6a0",
  "82" = "#118ab2",
  "82dead" = "#06d6a0",
  "9" = "#118ab2",
  "9dead" = "#06d6a0",
  "126" = "#118ab2",
  "126dead" = "#06d6a0",
  "127" = "#118ab2",
  "127dead" = "#06d6a0",
  "280" = "#118ab2",
  "280dead" = "#06d6a0",
  "194" = "#118ab2",
  "194dead" = "#06d6a0",
  "201" = "#118ab2",
  "201dead" = "#06d6a0"
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

plot_list <- list() 

# Function to create and save plots
save_plots <- function(data_list) {
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    
    # Create ggplot with faceting by replicate
    p <- ggplot(df, aes(x = 10 * Frame, y = avg_luminescence, color = treatment, linetype = treatment)) +
      geom_line() +  # Add lines for average luminescence difference
      geom_errorbar(aes(ymin = avg_luminescence - sem_luminescence, ymax = avg_luminescence + sem_luminescence), width = 0.01, alpha = 0.4) +  # Add error bars for SEM difference
      labs(title = paste("Kinetics of ROS Burst for", df$sample_id[1]),  # Dynamically set the title
           x = "Time (seconds)",
           y = "Luminescence (RLU)",
           color = "Treatment",
           linetype = "Treatment") +  # Add legend label for treatment
      scale_color_manual(values = colour_map, labels = new_labels) +  # Apply custom color map and new labels
      scale_linetype_manual(values = rep(c("solid", "dashed"), length.out = length(colour_map)), labels = new_labels) +  # Apply linetype and new labels
      theme_bw() +  # Use theme_bw for a white background
      facet_wrap(~ replicate, scales = "free_y") + # Facet by replicate with free y scales
      theme(strip.text = element_text(size = 10), # Adjust facet label size if necessary
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10)) +
      xlim(0, 2000)
    
    # Save plot to the "plots" directory
    file_name <- paste0("plots/plot_", i, "_sample_id_", df$sample_id[1], ".png")
    ggsave(filename = file_name, plot = p, width = 8, height = 6)
  }
}

# Call the function to save all plots
save_plots(split_data_list)
