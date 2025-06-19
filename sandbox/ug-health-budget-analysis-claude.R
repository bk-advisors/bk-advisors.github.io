# Uganda Health Budget Analysis - Publication Ready Charts
# Data source: Uganda Ministry of Finance
# Analysis by: BK Advisors

# Load required libraries
library(tidyverse)
library(scales)
library(gridExtra)
library(readr)

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

# Read the data
data <- read_csv("ug_health_budget_summary.csv")

# Data preparation
data <- data %>%
  mutate(
    Health_USD_bn = round(Health_USD_bn, 3),
    Health_Perc = round(Health_Perc, 2),
    Year_Label = case_when(
      Fiscal_Year == "2024/25" ~ "FY 2024/25",
      Fiscal_Year == "2025/26" ~ "FY 2025/26"
    )
  )

# Calculate the percentage increase
health_increase <- ((data$Health_USD_bn[2] - data$Health_USD_bn[1]) / data$Health_USD_bn[1]) * 100

# Set up theme for publication-ready charts
theme_bk <- theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", color = bk_colors["deepBlue"]),
    plot.subtitle = element_text(size = 12, color = bk_colors["darkGray"]),
    plot.caption = element_text(size = 9, color = bk_colors["darkGray"], hjust = 0),
    axis.title = element_text(size = 11, color = bk_colors["darkGray"]),
    axis.text = element_text(size = 10, color = bk_colors["darkGray"]),
    legend.title = element_text(size = 11, color = bk_colors["darkGray"]),
    legend.text = element_text(size = 10, color = bk_colors["darkGray"]),
    panel.grid.major = element_line(color = bk_colors["lightGray"], size = 0.3),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Chart 1: Health Budget Allocation in USD Billions
chart1 <- ggplot(data, aes(x = Year_Label, y = Health_USD_bn)) +
  geom_col(fill = bk_colors["mediumBlue"], width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0("$", Health_USD_bn, "B")), 
            vjust = -0.5, size = 4.5, fontface = "bold", color = bk_colors["deepBlue"]) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "B"), 
                     limits = c(0, max(data$Health_USD_bn) * 1.15)) +
  labs(
    title = "Uganda's Health Budget Nearly Doubled",
    subtitle = paste0("Health allocation increased by ", round(health_increase, 0), "% from FY 2024/25 to FY 2025/26"),
    x = "Fiscal Year",
    y = "Health Budget (USD Billions)",
    caption = "Source: Uganda Ministry of Finance | Analysis: BK Advisors"
  ) +
  theme_bk

# Chart 2: Health Budget as Percentage of Total Budget
chart2 <- ggplot(data, aes(x = Year_Label, y = Health_Perc)) +
  geom_col(fill = bk_colors["teal"], width = 0.6, alpha = 0.9) +
  geom_text(aes(label = paste0(Health_Perc, "%")), 
            vjust = -0.5, size = 4.5, fontface = "bold", color = bk_colors["deepBlue"]) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, max(data$Health_Perc) * 1.15)) +
  labs(
    title = "Health Share of National Budget Doubled",
    subtitle = "From 4.1% to 8.1% of total government budget",
    x = "Fiscal Year",
    y = "Health Budget (% of Total Budget)",
    caption = "Source: Uganda Ministry of Finance | Analysis: BK Advisors"
  ) +
  theme_bk

# Chart 3: Combined Dual-Axis Chart
chart3 <- ggplot(data, aes(x = Year_Label)) +
  geom_col(aes(y = Health_USD_bn), fill = bk_colors["mediumBlue"], 
           width = 0.6, alpha = 0.9) +
  geom_line(aes(y = Health_Perc/2, group = 1), 
            color = bk_colors["teal"], size = 2) +
  geom_point(aes(y = Health_Perc/2), 
             color = bk_colors["teal"], size = 4) +
  geom_text(aes(y = Health_USD_bn, label = paste0("$", Health_USD_bn, "B")), 
            vjust = -0.5, size = 4, fontface = "bold", color = bk_colors["deepBlue"]) +
  geom_text(aes(y = Health_Perc/2, label = paste0(Health_Perc, "%")), 
            vjust = -1.5, size = 4, fontface = "bold", color = bk_colors["teal"]) +
  scale_y_continuous(
    name = "Health Budget (USD Billions)",
    labels = dollar_format(prefix = "$", suffix = "B"),
    sec.axis = sec_axis(~.*2, name = "Health Budget (% of Total)", 
                        labels = percent_format(scale = 1))
  ) +
  labs(
    title = "Uganda's Historic Health Budget Increase",
    subtitle = "Both absolute amounts and budget share show dramatic improvement",
    x = "Fiscal Year",
    caption = "Source: Uganda Ministry of Finance | Analysis: BK Advisors"
  ) +
  theme_bk +
  theme(
    axis.title.y.left = element_text(color = bk_colors["mediumBlue"]),
    axis.title.y.right = element_text(color = bk_colors["teal"]),
    axis.text.y.left = element_text(color = bk_colors["mediumBlue"]),
    axis.text.y.right = element_text(color = bk_colors["teal"])
  )

# Chart 4: Progress Toward Abuja Declaration (15% target)
abuja_data <- data %>%
  mutate(
    Abuja_Target = 15,
    Gap_to_Abuja = Abuja_Target - Health_Perc
  )

chart4 <- ggplot(abuja_data, aes(x = Year_Label)) +
  geom_col(aes(y = Health_Perc), fill = bk_colors["mediumBlue"], 
           width = 0.6, alpha = 0.9) +
  geom_col(aes(y = Gap_to_Abuja), fill = bk_colors["lightGray"], 
           width = 0.6, alpha = 0.7, position = position_stack()) +
  geom_hline(yintercept = 15, color = bk_colors["teal"], 
             linetype = "dashed", size = 1) +
  geom_text(aes(y = Health_Perc/2, label = paste0(Health_Perc, "%")), 
            size = 4, fontface = "bold", color = "white") +
  annotate("text", x = 1.5, y = 15.5, label = "Abuja Declaration Target (15%)", 
           color = bk_colors["teal"], fontface = "bold", size = 3.5) +
  scale_y_continuous(labels = percent_format(scale = 1), 
                     limits = c(0, 16)) +
  labs(
    title = "Progress Toward Abuja Declaration Target",
    subtitle = "Uganda moving closer to 15% health budget commitment",
    x = "Fiscal Year",
    y = "Health Budget (% of Total Budget)",
    caption = "Source: Uganda Ministry of Finance | Analysis: BK Advisors\nNote: Gray area shows remaining gap to Abuja Declaration 15% target"
  ) +
  theme_bk

# Chart 5: Comparison Table Visual
comparison_data <- data.frame(
  Metric = c("Health Budget (USD)", "% of Total Budget", "Increase"),
  FY2024_25 = c("$797M", "4.1%", "-"),
  FY2025_26 = c("$1.59B", "8.1%", "+99%"),
  stringsAsFactors = FALSE
)

# Create a summary stats dataframe for display
summary_stats <- data.frame(
  Metric = c(
    "FY 2024/25 Health Budget",
    "FY 2025/26 Health Budget", 
    "Absolute Increase",
    "Percentage Increase",
    "Budget Share 2024/25",
    "Budget Share 2025/26"
  ),
  Value = c(
    "$797 Million",
    "$1.59 Billion",
    "$793 Million",
    "99%",
    "4.1%",
    "8.1%"
  )
)

# Display all charts
print("Chart 1: Health Budget in USD Billions")
print(chart1)

print("Chart 2: Health Budget as Percentage of Total Budget")
print(chart2)

print("Chart 3: Combined Dual-Axis Chart")
print(chart3)

print("Chart 4: Progress Toward Abuja Declaration Target")
print(chart4)

# Save charts as high-resolution PNG files for publication
ggsave("uganda_health_budget_usd.png", chart1, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("uganda_health_budget_percent.png", chart2, width = 10, height = 6, dpi = 300, bg = "white")
ggsave("uganda_health_budget_combined.png", chart3, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("uganda_health_budget_abuja.png", chart4, width = 10, height = 6, dpi = 300, bg = "white")

# Print key statistics for LinkedIn post
cat("\n=== KEY STATISTICS FOR LINKEDIN POST ===\n")
cat("Health Budget Increase:", round(health_increase, 0), "%\n")
cat("FY 2024/25: $", data$Health_USD_bn[1], "B (", data$Health_Perc[1], "% of budget)\n")
cat("FY 2025/26: $", data$Health_USD_bn[2], "B (", data$Health_Perc[2], "% of budget)\n")
cat("Absolute increase: $", round(data$Health_USD_bn[2] - data$Health_USD_bn[1], 3), "B\n")
cat("Progress to Abuja target: ", round((data$Health_Perc[2]/15)*100, 1), "% of 15% target achieved\n")

# Create a grid of charts for comprehensive view
combined_charts <- grid.arrange(chart1, chart2, chart3, chart4, ncol = 2, nrow = 2)

# Save the combined chart
ggsave("uganda_health_budget_all_charts.png", combined_charts, 
       width = 16, height = 12, dpi = 300, bg = "white")