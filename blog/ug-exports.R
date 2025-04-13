# Load required packages
library(readxl)
library(tidyverse)
library(ggthemes)
library(janitor)
library(stringr)

# Define EAC member countries (verify exact names in your data)
eac_countries <- c("Kenya", "Tanzania", "Rwanda", "Burundi", 
                   "South Sudan", "DRC")

# Read annual data --------------------------------------------------------
df_annual <- read_excel("./data/Direction-of-Trade_Exports.xlsx",
                        sheet = "Annual_CY",
                        skip = 5) %>%  # Adjust skip based on actual header rows
  rename(Country = 1) %>% 
  filter(!is.na(Country)) %>%
  # Convert to tidy format
  pivot_longer(cols = -Country,
               names_to = "Year",
               values_to = "Exports") %>%
  # Clean year column and convert exports to numeric
  mutate(Year = as.numeric(gsub("\\D", "", Year)),  # Extract numeric year
         Exports = as.numeric(Exports)) %>%
  filter(!is.na(Exports))

df_annual_clean <- df_annual %>% 
  mutate(Country = str_trim(Country)) # Removing white space before and after country name


# Filter for EAC countries only
eac_annual <- df_annual_clean %>%
  filter(Country %in% eac_countries)

# Analysis ----------------------------------------------------------------

# 1. Total EAC exports trend
total_trend <- eac_annual %>%
  group_by(Year) %>%
  summarise(Total = sum(Exports, na.rm = TRUE))

ggplot(total_trend, aes(x = Year, y = Total)) +
  geom_line(color = "#2c7bb6", linewidth = 1) +
  geom_point(color = "#2c7bb6", size = 2.5) +
  labs(title = "Uganda's Total Exports to EAC Countries",
       subtitle = "Annual Trend (Calendar Years)",
       y = "Exports (US$ Millions)",
       x = "") +
  scale_x_continuous(breaks = seq(min(total_trend$Year), max(total_trend$Year), 2)) +
  theme_fivethirtyeight()

# 2. Country breakdown (latest year)
latest_year <- max(eac_annual$Year)

country_breakdown <- eac_annual %>%
  filter(Year == latest_year) %>%
  mutate(Share = Exports/sum(Exports))

ggplot(country_breakdown, aes(x = reorder(Country, Exports), y = Exports)) +
  geom_col(fill = "#2c7bb6", width = 0.8) +
  geom_text(aes(label = scales::percent(Share, accuracy = 1)), 
            hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(title = paste("EAC Export Distribution", latest_year),
       subtitle = "Percentage share of total EAC exports",
       y = "Exports (US$ Millions)",
       x = "") +
  theme_economist()

#write.csv(country_breakdown, file="./data/ug-exports-2024.csv", row.names = FALSE)

# 3. Country-specific trends
ggplot(eac_annual, aes(x = Year, y = Exports, color = Country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Export Trends to Individual EAC Countries",
       y = "Exports (US$ Millions)",
       x = "") +
  scale_color_brewer(palette = "Set1") +
  theme_minimal() +
  theme(legend.position = "bottom")

# 4. Export composition table (latest 3 years)
eac_annual %>%
  filter(Year >= max(Year) - 2) %>%
  pivot_wider(names_from = Year, values_from = Exports) %>%
  arrange(desc(.[[3]])) %>%  # Sort by latest year
  mutate(across(where(is.numeric), ~ round(., 1)))
