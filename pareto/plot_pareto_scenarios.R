# Plot Pareto optimal scenarios in R 

library(ggplot2)
library(gridExtra)

# Load the data
set1 <- read.csv("data/optimization_results/set1.csv", sep = " ")
set2 <- read.csv("data/optimization_results/set2.csv", sep = " ")
set3 <- read.csv("data/optimization_results/set3.csv", sep = " ")
set4 <- read.csv("data/optimization_results/set4.csv", sep = " ")

# Add scenario labels
set1$Scenario <- "Private No STEM"
set2$Scenario <- "Private STEM"
set3$Scenario <- "Public No STEM"
set4$Scenario <- "Public STEM"

# Combine all datasets
data <- rbind(set1, set2, set3, set4)
data$Scenario <- factor(data$Scenario, levels = names(colors))

# Rename columns for clarity
colnames(data)[1:3] <- c("Economic", "Biodiversity", "Health")

# Define color palette and markers
colors <- c("Private No STEM" = "blue", 
            "Private STEM" = "orange", 
            "Public No STEM" = "green", 
            "Public STEM" = "red")

# Create pairwise scatterplots for Pareto fronts
plot_pareto <- function(x, y, data, colors) {
  ggplot(data, aes_string(x = x, y = y, color = "Scenario")) +
    geom_point(alpha = 0.7, size = 3) +
    scale_color_manual(values = colors) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none") +
    labs(x = x, y = y)
}

# Generate individual plots for each pair of objectives
plot1 <- plot_pareto("Economic", "Biodiversity", data, colors)
plot2 <- plot_pareto("Economic", "Health", data, colors)
plot3 <- plot_pareto("Biodiversity", "Health", data, colors)

# Extract legend from one plot 
# Create a base plot to extract the legend
base_plot <- ggplot(data, aes(x = Economic, y = Biodiversity, color = Scenario)) +
  geom_point(alpha = 0.7, size = 3) +
  scale_color_manual(values = colors) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "right")

# Extract the legend
legend <- cowplot::get_legend(base_plot)

# Arrange plots and legend
final_plot <- cowplot::plot_grid(
  plot1, 
  plot2, 
  plot3, 
  legend,
  ncol = 2, 
  nrow = 2, 
  rel_widths = c(1, 1), 
  rel_heights = c(1, 1), 
  labels = c("A", "B", "C", "")
)

# Save the plot
ggsave("figures/pareto_fronts_improved_r.png", final_plot, width = 10, height = 8, bg = "white")
