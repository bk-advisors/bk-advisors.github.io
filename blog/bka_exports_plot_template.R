
# Load libraries
library(tidyverse)
library(grid)
library(png)
library(cowplot)

# Load your logo
logo_path <- "./assets/BKA_Logo_Apr2025.png"
logo_img <- readPNG(logo_path)
logo_grob <- rasterGrob(logo_img, interpolate = TRUE)

# Load data
df <- data.frame(
  Country = c("Burundi", "Tanzania", "Rwanda", "DRC", "South Sudan", "Kenya"),
  Exports = c(70.21, 137.08, 245.04, 446.94, 449.97, 549.96)
)
df$Country <- factor(df$Country, levels = df$Country)

# Main plot
main_plot <- ggplot(df, aes(x = Exports, y = Country)) +
  geom_col(fill = "#005587") +
  geom_text(aes(label = paste0("$", round(Exports), "M")), 
            hjust = -0.05, size = 3.5, family = "sans") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "grey80"),
    axis.title = element_blank(),
    axis.text = element_text(color = "black"),
    plot.margin = margin(10, 10, 10, 10)
  )

# Top row: logo, title, bold line
top_row <- ggdraw() +
  draw_line(x = c(0, 1), y = c(1, 1), size = 2, color = "#000000") +  # Top border line
  draw_grob(logo_grob, x = 0, y = 0.92, width = 0.15, hjust = 0, vjust = 1) +  # Logo
  draw_label("Uganda’s Exports to East African Community (2024)", 
             fontface = 'bold', size = 14, x = 0.5, y = 0.55, hjust = 0.5)

# Subtitle row
subtitle_row <- ggdraw() +
  draw_label("Exports reported in USD millions | Calendar year 2024", 
             x = 0.5, y = 0.5, hjust = 0.5, size = 10, color = "gray30")

# Source row
bottom_row <- ggdraw() +
  draw_label("Source: Uganda Bureau of Statistics (UBOS)", 
             x = 0, hjust = 0, vjust = 0, size = 9, color = "grey40")

# Combine layout
final_plot <- plot_grid(
  top_row,
  subtitle_row,
  main_plot,
  bottom_row,
  ncol = 1,
  rel_heights = c(0.13, 0.07, 1, 0.08)
)

# Display the plot
print(final_plot)

# Optional save
# ggsave("uganda_exports_with_subtitle.png", final_plot, width = 10, height = 7)
