# Install and load necessary packages
# If you don't have them installed, uncomment and run the following lines:
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyr") # For data manipulation if needed

library(tidyverse)

# BK Advisors color palette
bk_colors <- c(
  deepBlue = "#0A4C7F",
  mediumBlue = "#1976D2",
  lightBlue = "#64B5F6",
  teal = "#00ACC1",
  navy = "#0D47A1",
  darkGray = "#455A64",
  lightGray = "#ECEFF1"
)

# Load your data (assuming your CSV file is in the same directory as your R script)
df <- read.csv("ug_health_budget_summary.csv")

# Ensure Fiscal_Year is treated as a factor for correct plotting order
df$Fiscal_Year <- factor(df$Fiscal_Year, levels = c("2024/25", "2025/26"))

# Chart 1: Health Budget Allocation in Billions USD
ggplot(df, aes(x = Fiscal_Year, y = Health_USD_bn, fill = Fiscal_Year)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0("$", sprintf("%.2f", Health_USD_bn), "B")),
            vjust = -0.5, color = bk_colors["darkGray"], size = 5) +
  scale_fill_manual(values = c("2024/25" = bk_colors["mediumBlue"], "2025/26" = bk_colors["deepBlue"])) +
  labs(
    title = "Uganda's Health Budget Allocation (USD Billions)",
    subtitle = "FY2024/25 vs FY2025/26",
    x = "Fiscal Year",
    y = "Health Budget (USD Billions)",
    caption = "Data Source: Uganda's Ministry of Finance | Analysis: BK Advisors"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = bk_colors["navy"]),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = bk_colors["darkGray"]),
    axis.title.x = element_text(face = "bold", size = 12, color = bk_colors["darkGray"]),
    axis.title.y = element_text(face = "bold", size = 12, color = bk_colors["darkGray"]),
    axis.text.x = element_text(size = 12, color = bk_colors["darkGray"]),
    axis.text.y = element_text(size = 12, color = bk_colors["darkGray"]),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(hjust = 1, size = 10, color = bk_colors["darkGray"], face = "italic")
  )

# Save the chart (uncomment to save)
# ggsave("health_budget_usd_billions.png", width = 8, height = 6, dpi = 300)

# Chart 2: Health Budget as Percentage of Total National Budget
ggplot(df, aes(x = Fiscal_Year, y = Health_Perc, fill = Fiscal_Year)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = paste0(sprintf("%.1f", Health_Perc), "%")),
            vjust = -0.5, color = bk_colors["darkGray"], size = 5) +
  scale_fill_manual(values = c("2024/25" = bk_colors["mediumBlue"], "2025/26" = bk_colors["deepBlue"])) +
  labs(
    title = "Health Budget as Percentage of Total National Budget",
    subtitle = "FY2024/25 vs FY2025/26",
    x = "Fiscal Year",
    y = "Health Budget Percentage (%)",
    caption = "Data Source: Uganda's Ministry of Finance | Analysis: BK Advisors"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, color = bk_colors["navy"]),
    plot.subtitle = element_text(hjust = 0.5, size = 14, color = bk_colors["darkGray"]),
    axis.title.x = element_text(face = "bold", size = 12, color = bk_colors["darkGray"]),
    axis.title.y = element_text(face = "bold", size = 12, color = bk_colors["darkGray"]),
    axis.text.x = element_text(size = 12, color = bk_colors["darkGray"]),
    axis.text.y = element_text(size = 12, color = bk_colors["darkGray"]),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.caption = element_text(hjust = 1, size = 10, color = bk_colors["darkGray"], face = "italic")
  )

# Save the chart (uncomment to save)
# ggsave("health_budget_percentage.png", width = 8, height = 6, dpi = 300)