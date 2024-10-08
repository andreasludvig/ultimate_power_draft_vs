library(data.table)
library(ggplot2)
library(gridExtra)
library(patchwork)
library(here)

plot_data <- readRDS(
  here("notebooks/qPCR/data_final_for_plotting/plot_data.rds")
)

# Assuming plot_data is your original dataset
# Filter for CYP enzymes only, remove lowest concentration
cyp_data <- plot_data[target %like% "CYP" & !treatment %like% "0.001"]

# Split data based on condition
cyp_data_IL6 <- cyp_data[condition == "IL-6"]
cyp_data_IL1B <- cyp_data[condition == "IL-1B"]

create_plot_with_time_grouping <- function(cyp_data, condition) {
  # Calculate mean normalized expression for each combination of:
  # CYP enzyme, treatment and time
  cyp_means <-
  cyp_data[, .(mean_normalized_expression = geometric_mean(normalized_expression)),
    by = .(time, treatment, target)
  ]

  # Calculate the overall mean across time points
  cyp_overall_means <- cyp_means[, .(overall_mean_normalized_expression = geometric_mean(mean_normalized_expression)),
    by = .(treatment, target)
  ]

  # Create and return the plot
  ggplot() +
    geom_point(data = cyp_means, aes(x = treatment, y = mean_normalized_expression, color = factor(time)), alpha = 0.7, size = 3) +
    geom_point(
      data = cyp_overall_means, aes(x = treatment, y = overall_mean_normalized_expression),
      color = "black", size = 5, shape = 18, alpha = 0.7
    ) +
    scale_y_continuous(limits = c(0, NA)) +
    facet_wrap(~target, scales = "free_y", ncol = 2) +
    labs(
      x = "Treatment", y = "Mean Normalized Expression",
      title = paste("Normalized Expression after", condition),
      color = "Time (hours)"
    ) +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
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
plot_IL1B
plot_IL6
# Save the plots
ggsave("Normalized_Expression_IL6.png", plot_IL6, width = 12, height = 10, dpi = 300)
ggsave("Normalized_Expression_IL1B.png", plot_IL1B, width = 12, height = 10, dpi = 300)

# Display the plots
print(plot_IL6)
print(plot_IL1B)
