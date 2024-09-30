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

setwd("C:\\Users\\rolfe\\Desktop\\Uni\\4th sem\\Results\\Report data\\SAP")

sap <- read.csv("results.csv", header = TRUE, sep = ",")

sap$treatment <- factor(sap$treatment, levels = c("mock",
                                                  "xcc",
                                                  "9",
                                                  "11", 
                                                  "82",
                                                  "126", 
                                                  "127", 
                                                  "129",
                                                  "130", 
                                                  "131", 
                                                  "137",
                                                  "151", 
                                                  "159", 
                                                  "160",
                                                  "171", 
                                                  "182", 
                                                  "194",
                                                  "201", 
                                                  "216", 
                                                  "225",
                                                  "280", 
                                                  "296", 
                                                  "306",
                                                  "307", 
                                                  "311", 
                                                  "337",
                                                  "344", 
                                                  "347", 
                                                  "359",
                                                  "394", 
                                                  "404", 
                                                  "405",
                                                  "434", 
                                                  "436"))
##total drops collected
p <- ggplot(sap, aes(x = treatment, y = no..drops)) +
  geom_boxplot() +
  geom_jitter(aes(color = as.factor(replicate), shape = as.factor(replicate)),size = 2.5,  width = 0.2) +
  labs(title = "Number of drops collected by Treatment",
       x = "Treatment",
       y = "No. Drops") +
  scale_color_manual(values = c("1" = "#FFC15E", "2" = "#F7934C", "3" = "#F25C54")) +  # Customize colors for replicates
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 18)) +
  theme_minimal() +
  guides(
    color = guide_legend(title = "Replicate"),  # Keep the legend for replicate with a title
    shape = guide_legend(title = "Replicate")  # Keep the legend for replicate with a title
  ) +
  ylim (-1, 23)

ggsave(filename = "sapplot.png", plot = p, width = 10, height = 5)

p

filtered_sap <- sap %>%
  filter(drops.positive > 0) %>%  # Keep only positive values
  group_by(treatment) %>%
  filter(sum(drops.positive) > 0)

##total drops positive
ggplot(filtered_sap, aes(x = treatment, y = drops.positive)) +
  geom_boxplot() +
  geom_jitter(aes(color = as.factor(replicate), shape = as.factor(replicate)),  size = 2.5,  width = 0.1) +
  labs(title = "Number of drops collected by Treatment",
       x = "Treatment",
       y = "No. Drops") +
  scale_color_manual(values = c("1" = "#FFC15E", "2" = "#F7934C", "3" = "#F25C54")) +  # Customize colors for replicates
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 18)) +
  theme_minimal() +
  guides(
    color = guide_legend(title = "Replicate"),  # Keep the legend for replicate with a title
    shape = guide_legend(title = "Replicate")  # Keep the legend for replicate with a title
  )



filtered_sap <- sap %>%
  filter(drops.positive > 0) %>%  # Keep only positive values
  group_by(treatment) %>%
  filter(sum(drops.positive) > 0)

##total drops positive
ggplot(filtered_sap, aes(x = treatment, y = proportion.of.drops.positive)) +
  geom_boxplot() +
  geom_jitter(aes(color = as.factor(replicate), shape = as.factor(replicate)),  size = 2.5,  width = 0.1) +
  labs(title = "Number of drops collected by Treatment",
       x = "Treatment",
       y = "Percentage of drops positive") +
  scale_color_manual(values = c("1" = "#FFC15E", "2" = "#F7934C", "3" = "#F25C54")) +  # Customize colors for replicates
  scale_shape_manual(values = c("1" = 16, "2" = 17, "3" = 18)) +
  theme_minimal() +
  guides(
    color = guide_legend(title = "Replicate"),  # Keep the legend for replicate with a title
    shape = guide_legend(title = "Replicate")  # Keep the legend for replicate with a title
  ) + 
  ylim (0, NA)
