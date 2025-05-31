# Load required libraries
library(tidyverse)

# Read the dataset
health_exp <- read_csv("./data/eac-govt-health-exp.csv")

# Filter for Uganda and most recent year
uganda_data <- health_exp %>%
  filter(location == "Uganda" | code == "UGA") %>%
  arrange(desc(year)) %>%
  slice(1)  # Select most recent year

# Extract expenditure components
govt_exp <- uganda_data$gghed_pc_usd
private_exp <- uganda_data$pvtd_pc_usd
oop_exp <- uganda_data$oop_pc_usd
external_exp <- uganda_data$ext_pc_usd
total_exp <- govt_exp + private_exp + oop_exp + external_exp

# Create summary data frame
expenditure_breakdown <- data.frame(
  Source = c("Government", "Private", "Out-of-Pocket", "External", "Total"),
  Amount_Per_Capita = c(govt_exp, private_exp, oop_exp, external_exp, total_exp),
  Year = uganda_data$year
)

# Calculate percentage distribution
expenditure_breakdown <- expenditure_breakdown %>%
  mutate(Percentage = round(Amount_Per_Capita / total_exp * 100, 1))

# Print results
print(expenditure_breakdown)
