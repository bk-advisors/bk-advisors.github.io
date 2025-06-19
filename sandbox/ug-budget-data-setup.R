# Load required library
library(tidyverse)

# Define the dataset
budget_data <- tibble::tribble(
  ~Fiscal_Year,         ~Category,                                  ~Item, ~Amount_UGX_trn, ~Amount_USD_bn, ~Perc_Budget,
  # 2024/25: Total National Budget
  "2024/25",           "Total",               "Total National Budget",         72.136,          19.496,        100.0,
  
  # 2024/25: Sectors
  "2024/25",          "Sector",                                 "Health",          2.950,           0.797,          4.09,
  "2024/25",          "Sector",                              "Education",          4.800,           1.297,          6.66,
  "2024/25",          "Sector",                           "Agriculture",          1.100,           0.297,          1.53,
  "2024/25",          "Sector", "Climate Change/Water, Sanitation & Environment",          0.745,           0.201,          1.03,
  "2024/25",          "Sector",                    "Social Protection",          0.405,           0.109,          0.56,
  "2024/25",          "Sector",    "Infrastructure (Works and Transport)",          5.500,           1.486,          7.63,
  "2024/25",          "Sector",                                "Defence",          3.500,           0.946,          4.85,
  "2024/25",          "Sector",                                 "Energy",          1.500,           0.405,          2.08,
  "2024/25",          "Sector",        "Parish Development Model (PDM)",          1.059,           0.286,          1.47,
  "2024/25",          "Sector",                               "Tourism",          0.000,           0.000,          0.00,
  "2024/25",          "Sector", "Housing, Urban Development, and Public Works",          0.000,           0.000,          0.00,
  "2024/25",          "Sector",                                   "ICT",          0.000,           0.000,          0.00,
  "2024/25",          "Sector",                "Governance and Justice",          0.000,           0.000,          0.00,
  "2024/25",          "Sector",                       "Other Sectors",          4.000,           1.081,          5.55,
  
  # 2024/25: Functional Expenditures
  "2024/25",       "Functional",                     "Debt Servicing",         27.000,           7.297,         37.44,
  "2024/25",       "Functional",                      "Interest payments",         10.030,           2.711,         13.91,
  "2024/25",       "Functional",                  "Salaries (Public Sector)",          8.570,           2.316,         11.88,
  "2024/25",       "Functional",                   "Development Projects",         34.700,           9.378,         48.11,
  "2024/25",       "Functional",                "Recurrent Expenditure",         18.900,           5.108,         26.20,
  
  # 2025/26: Total National Budget
  "2025/26",           "Total",               "Total National Budget",         72.380,          19.562,        100.0,
  
  # 2025/26: Sectors
  "2025/26",          "Sector",                                 "Health",          5.870,           1.586,          8.11,
  "2025/26",          "Sector",                              "Education",          5.040,           1.362,          6.96,
  "2025/26",          "Sector",                           "Agriculture",          0.816,           0.220,          1.13,
  "2025/26",          "Sector", "Climate Change/Water, Sanitation & Environment",          0.365,           0.099,          0.50,
  "2025/26",          "Sector",                    "Social Protection",          0.405,           0.109,          0.56,
  "2025/26",          "Sector",    "Infrastructure (Works and Transport)",          5.700,           1.541,          7.87,
  "2025/26",          "Sector",                                "Defence",          3.700,           1.000,          5.11,
  "2025/26",          "Sector",                                 "Energy",          1.600,           0.432,          2.21,
  "2025/26",          "Sector",        "Parish Development Model (PDM)",          1.059,           0.286,          1.46,
  "2025/26",          "Sector",                               "Tourism",          0.430,           0.116,          0.59,
  "2025/26",          "Sector", "Housing, Urban Development, and Public Works",          0.120,           0.032,          0.17,
  "2025/26",          "Sector",                                   "ICT",          0.013,           0.003,          0.02,
  "2025/26",          "Sector",                "Governance and Justice",          0.048,           0.013,          0.07,
  "2025/26",          "Sector",                       "Other Sectors",          2.400,           0.649,          3.32,
  
  # 2025/26: Functional Expenditures
  "2025/26",       "Functional",                     "Debt Servicing",         27.000,           7.297,         37.30,
  "2025/26",       "Functional",                      "Interest payments",         11.300,           3.054,         15.61,
  "2025/26",       "Functional",                  "Salaries (Public Sector)",          8.570,           2.316,         11.84,
  "2025/26",       "Functional",                   "Development Projects",         18.240,           4.930,         25.20,
  "2025/26",       "Functional",                "Recurrent Expenditure",         28.330,           7.657,         39.14
)

# Verify totals
budget_data %>%
  group_by(Fiscal_Year, Category) %>%
  summarise(Total_UGX = sum(Amount_UGX_trn), .groups = "drop")

# Save as CSV (optional)
write_csv(budget_data, "uganda_budget_allocations.csv")


# Preliminary Analysis

# 1. Compare health allocation across years
health_alloc <- budget_data %>%
  filter(Item == "Health") %>%
  select(Fiscal_Year, Amount_UGX_trn, Amount_USD_bn, Perc_Budget)

# 2. Sectoral changes (2024/25 vs 2025/26)
sector_change <- budget_data %>%
  filter(Category == "Sector") %>%
  pivot_wider(names_from = Fiscal_Year, values_from = c(Amount_UGX_trn, Perc_Budget))

# 3. Health vs. Key Sectors (2025/26)
health_vs_others <- budget_data %>%
  filter(Fiscal_Year == "2025/26",
         Item %in% c("Health", "Education", "Defence", "Agriculture"))

# 4. Recurrent vs. Development shift
functional_shift <- budget_data %>%
  filter(Item %in% c("Recurrent Expenditure", "Development Projects")) %>%
  select(Fiscal_Year, Item, Amount_UGX_trn, Perc_Budget)