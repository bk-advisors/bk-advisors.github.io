# Enhanced Uganda Exports Visualization with Tufte Principles
# Load libraries
library(tidyverse)
library(grid)
library(png)
library(cowplot)

# Load your logo (if absolutely necessary)
logo_path <- "./assets/BKA_Logo_Apr2025.png"
logo_img <- readPNG(logo_path)
logo_grob <- rasterGrob(logo_img, interpolate = TRUE)

# Load and prepare data - sort by value for better visual perception
df <- data.frame(
  Country = c("Burundi", "Tanzania", "Rwanda", "DRC", "South Sudan", "Kenya"),
  Exports = c(70.21, 137.08, 245.04, 446.94, 449.97, 549.96)
)
# Sort data from highest to lowest for better visual perception
df <- df[order(df$Exports), ]
df$Country <- factor(df$Country, levels = df$Country)

# Calculate subtle reference lines at meaningful intervals
value_max <- max(df$Exports)
tick_interval <- 100  # Meaningful interval for this data
ticks <- seq(0, ceiling(value_max/tick_interval)*tick_interval, by = tick_interval)

# Define a subtle color palette - use muted colors that don't overwhelm the data
main_color <- "#4575b4"  # A muted blue that's professional but not too bright

# Main plot - applying Tufte principles
main_plot <- ggplot(df, aes(x = Exports, y = Country)) +
  # Simple bars with reduced emphasis
  geom_col(fill = main_color, alpha = 0.8, width = 0.7) +
  
  # Direct data labeling instead of requiring a legend or visual inference
  geom_text(aes(label = paste0("$", round(Exports), "M")), 
            hjust = -0.2, size = 3, family = "sans", color = "gray20") +
  
  # Add subtle, meaningful reference lines
  geom_vline(xintercept = ticks, color = "gray90", size = 0.3) +
  
  # Expand axis just enough to show labels
  scale_x_continuous(
    expand = expansion(mult = c(0, 0.12)), 
    position = "top",  # Move scale to top as per Tufte's recommendation
    breaks = ticks,
    labels = paste0("$", ticks, "M")
  ) +
  
  # Minimalist theme inspired by Tufte's principles
  theme_minimal(base_family = "sans") +
  theme(
    # Remove unnecessary grid lines
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),  # Remove grid lines
    
    # Remove borders
    panel.border = element_blank(),
    axis.line = element_blank(),
    
    # Subtle axis text
    axis.text = element_text(color = "gray20", size = 9),
    axis.title = element_blank(),  # Remove axis titles as they're self-evident
    
    # Clean margins
    plot.margin = margin(5, 15, 5, 5),
    
    # Align y-axis labels right to the data
    axis.text.y = element_text(hjust = 1, margin = margin(r = 5))
  )

# Create a minimalist container - just enough context, no decorative elements
# Remove unnecessary elements like the heavy border line

# Title with clear hierarchy
title_text <- "Uganda's Exports to East African Community (2024)"
subtitle_text <- "Values in millions USD"
caption_text <- "Source: Central Bank of Uganda | Formal Trade | 2024"

# Complete visualization with minimal framing
final_plot <- ggdraw() +
  # Logo in bottom right, with increased size for better visibility
  draw_grob(logo_grob, x = 0.95, y = 0.01, width = 0.12, hjust = 1, vjust = 0.4) +
  
  # Clear but unobtrusive title with hierarchy
  draw_label(title_text, fontface = 'bold', size = 14, x = 0.03, y = 0.95, hjust = 0) +
  draw_label(subtitle_text, size = 10, color = "gray40", x = 0.03, y = 0.92, hjust = 0) +
  
  # Main data - the focus of the visualization
  draw_plot(main_plot, x = 0, y = 0.05, width = 1, height = 0.85) +
  
  # Minimal source attribution
  draw_label(caption_text, size = 8, color = "gray40", x = 0.03, y = 0.02, hjust = 0)

# Display the plot
print(final_plot)

# Optional save with appropriate dimensions
ggsave("uganda_exports_tufte.png", final_plot, width = 9, height = 6, dpi = 300)
