library(data.table)
library(ggplot2)
library(gridExtra)

plot_data <- readRDS(here("notebooks/qPCR/data_final_for_plotting/plot_data.rds"))

# Assuming plot_data is your original dataset
# Filter for CYP enzymes only, remove lowest concentration
cyp_data <- plot_data[target %like% "CYP" & !treatment %like% "0.001"]

# Split data based on condition
cyp_data_IL6 <- cyp_data[condition == "IL-6"]
cyp_data_IL1B <- cyp_data[condition == "IL-1B"]

create_plot_with_time_grouping <- function(cyp_data, condition) {
  # Calculate mean normalized expression for each combination of CYP enzyme, treatment, and time
  cyp_means <- cyp_data[, .(mean_normalized_expression = mean(normalized_expression)),
                        by = .(time, treatment, target)]

  # Calculate the overall mean across time points
  cyp_overall_means <- cyp_means[, .(overall_mean_normalized_expression = mean(mean_normalized_expression)),
                                 by = .(treatment, target)]

  # Create and return the plot
  ggplot() +
    geom_point(data = cyp_means, aes(x = treatment, y = mean_normalized_expression, color = factor(time)), alpha = 0.7, size = 3) +
    geom_point(data = cyp_overall_means, aes(x = treatment, y = overall_mean_normalized_expression),
               color = "black", size = 5, shape = 18, alpha = 0.7) +
    scale_y_continuous(limits = c(0,NA)) +
    facet_wrap(~ target, scales = "free_y", ncol = 2) +
    labs(x = "Treatment", y = "Mean Normalized Expression", 
         title = paste("Normalized Expression after", condition), 
         color = "Time (hours)") + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_text(vjust = 3, face = "bold"),
          axis.title.x = element_text(vjust = -1, face = "bold"),
          plot.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          strip.background = element_rect(fill = "lightgrey"),
          strip.text = element_text(face = "bold")
    )
}

# Create the plots
plot_IL6 <- create_plot_with_time_grouping(cyp_data_IL6, "IL-6")
plot_IL1B <- create_plot_with_time_grouping(cyp_data_IL1B, "IL-1β")

# Save the plots
ggsave("Normalized_Expression_IL6.png", plot_IL6, width = 12, height = 10, dpi = 300)
ggsave("Normalized_Expression_IL1B.png", plot_IL1B, width = 12, height = 10, dpi = 300)

# Display the plots
print(plot_IL6)
print(plot_IL1B)


#################################
library(data.table)
library(ggplot2)
library(gridExtra)

plot_data <- readRDS(here("notebooks/qPCR/data_final_for_plotting/plot_data.rds"))

# Assuming plot_data is your original dataset
# Filter for CYP enzymes only, remove lowest concentration
cyp_data <- plot_data[target %like% "CYP" & !treatment %like% "0.001"]

# Split data based on condition
cyp_data_IL6 <- cyp_data[condition == "IL-6"]
cyp_data_IL1B <- cyp_data[condition == "IL-1B"]


# ... (previous data preparation steps remain the same)
library(patchwork)
# Function to create the plot
create_plot <- function(data, data_means, title, add_rectangles = FALSE) {
  p <- ggplot(data, aes(x = conc, y = time_means)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_point(aes(shape = factor(time), fill = factor(time)), size = 7, stroke = 1, alpha = .7) +
    scale_shape_manual(values = c(21, 22, 24)) +
    scale_fill_brewer(palette = "Set2") +
    geom_point(data = data_means, aes(x = conc, y = overall_mean), 
               size = 12, shape = 22, fill = "black", alpha = 0.8) +
    labs(x = paste("Concentration", title, "(ng/ml)"),
         y = "Expression relative to vehicle control",
         fill = "Duration of treatment, hours:") +
    scale_y_continuous(limits = c(0, 1.25), n.breaks = 6) +
    scale_x_log10() +
    annotation_logticks(sides = "b") +
    theme_bw() +
    theme(
      axis.text = element_text(size = 12, face = "bold"),
      axis.title = element_text(face = "bold", size = 15),
      plot.title = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank()
    ) +
    ggtitle(title)
  
  if (add_rectangles) {
    p <- p +
      geom_rect(xmin = log10(0.00000000001), xmax = log10(0.02), ymin = -Inf, ymax = Inf, fill = "lightgreen", alpha = 0.006) +
      geom_rect(xmin = log10(0.015), xmax = log10(0.15), ymin = -Inf, ymax = Inf, fill = "yellow", alpha = 0.006) +
      geom_rect(xmin = log10(0.09), xmax = log10(Inf), ymin = -Inf, ymax = Inf, fill = "coral", alpha = 0.006)
  }
  
  return(p)
}

# Generate the plots
plot_IL1B <- create_plot(data_IL1B, data_IL1B_means, "IL-1β")
plot_IL6 <- create_plot(data_IL6, data_IL6_means, "IL-6", add_rectangles = TRUE)

# Combine the plots
combined_plot <- plot_IL1B + plot_IL6 +
  plot_layout(guides = 'collect') & 
  plot_annotation(
    title = 'Relative expression of CYP3A4',
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 18)
    )
  ) & 
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.title = element_text(size = 16)
  )

# Print the combined plot
combined_plot