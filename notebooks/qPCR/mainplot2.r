# Load necessary libraries
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(here)

# Load the data
plot_data <- readRDS(here("notebooks/qPCR/data_final_for_plotting/plot_data.rds"))


# Filter for CYP enzymes only, include all treatments and CYPs
cyp_data <- plot_data[target %like% "CYP"]

# Split data based on condition, ensuring CYP3A4 is included
cyp_data_IL6 <- cyp_data[condition == "IL-6"]
cyp_data_IL1B <- cyp_data[condition == "IL-1B"]

# Create a custom flat horizontal rectangular shape function
geom_flat_rect <- function(...) {
  geom_tile(width = 0.5, height = 0.08, ...)
}

# Updated function to create the plot based on the new requirements
create_custom_plot <- function(cyp_data, condition_name) {
  # Calculate mean normalized expression for each combination of CYP enzyme, treatment, and time
  cyp_means <- cyp_data[, .(mean_normalized_expression = mean(normalized_expression)),
                        by = .(time, treatment, target)]
  
  # Calculate the overall mean across time points
  cyp_overall_means <- cyp_means[, .(overall_mean_normalized_expression = mean(mean_normalized_expression)),
                                  by = .(treatment, target)]
  
  # Define colors and shapes
  shapes <- c(16, 17, 15) # Circle, triangle, square
  colors <- brewer.pal(3, "Set2") # Use Set2 color palette
  
  # Create the plot
  ggplot() +
    geom_point(data = cyp_means, 
               aes(x = treatment, y = mean_normalized_expression, color = factor(time), shape = factor(time)),
               size = 4, alpha = 0.8) +
    geom_flat_rect(data = cyp_overall_means, 
                   aes(x = treatment, y = overall_mean_normalized_expression),
                   color = "black", fill = "black", alpha = 0.7) +
    geom_hline(yintercept = 1, linetype = "dotted", color = "grey50") +  # Adjusted y-intercept
    scale_color_manual(values = colors) +
    scale_shape_manual(values = shapes) +
    scale_y_continuous(limits = c(0, NA)) +
    facet_wrap(~ target, scales = "free_y") +
    labs(x = "Treatment", y = "Mean Normalized Expression", 
         title = paste("Normalized Expression after", condition_name)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          axis.title.y = element_text(vjust = 3, face = "bold"),
          axis.title.x = element_text(vjust = -1.5, face = "bold"),
          plot.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"))
}

# Create the separate plots for IL-6 and IL-1B
plot_IL6_custom <- create_custom_plot(cyp_data_IL6, "IL-6")
plot_IL1B_custom <- create_custom_plot(cyp_data_IL1B, "IL-1B")

# View the plots
plot_IL6_custom
plot_IL1B_custom

ggsave(plot_IL6_custom,filename = here("images/mRNA/main_new_IL6.png"), device = "png", dpi = "retina")
ggsave(plot_IL1B_custom,filename = here("images/mRNA/main_new_IL1B.png"), device = "png", dpi = "retina")
