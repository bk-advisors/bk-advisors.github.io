# Setup ----------------------------------------------------------------

# Load required packages
library(readxl)
library(tidyverse)
library(ggthemes)
library(janitor)
library(stringr)
library(showtext)
library(shadowtext)
library(scales)
library(png)
library(cowplot)
library(grid)
library(ggimage)
library(ggtext)
library(patchwork)
library(ggnewscale)

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


# Filter for EAC countries only, after 2014
eac_annual <- df_annual_clean %>%
  filter(Country %in% eac_countries) |> 
  filter(Year >= 2014)

# write.csv(eac_annual, file = "./data/ug-eac-exports.csv", row.names = FALSE)

# Exploratory Data Analysis with Deepseek ----------------------------------

# Line Chart

# Calculate annual shares
export_shares <- eac_annual %>%
  group_by(Year) %>%
  mutate(Total_Exports = sum(Exports),
         Share = Exports / Total_Exports * 100) %>%
  ungroup() %>%
  arrange(Year, desc(Share))

# View the share results
print(export_shares %>% select(Country, Year, Share), n = 50)

# Visualize the trends
ggplot(export_shares, aes(x = Year, y = Share, color = Country)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Uganda's Export Share to EAC Countries (2014-2024)",
       y = "Percentage Share",
       x = "Year",
       color = "Country") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Optional: Create a table of shares by year (wide format)
share_table <- export_shares %>%
  select(Country, Year, Share) %>%
  pivot_wider(names_from = Year, values_from = Share)

print(share_table)

# Stacked Area Chart

# Create stacked area chart
ggplot(export_shares, aes(x = Year, y = Exports, fill = Country)) +
  geom_area(color = "white", linewidth = 0.3, alpha = 0.8) +
  scale_x_continuous(breaks = seq(2014, 2024, 1)) +
  scale_y_continuous(labels = dollar_format(prefix = "$", suffix = "M"),
                     expand = c(0, 0)) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Uganda's Exports to EAC Countries (2014-2024)",
       subtitle = "Absolute values in US$ millions",
       y = "Export Value",
       x = "Year",
       caption = "Source: ug-eac-exports.csv") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major.x = element_blank(),
        legend.position = "bottom")



# Analysis ----------------------------------------------------------------

# First, define colors.
BROWN <- "#AD8C97"
BROWN_DARKER <- "#7d3a46"
GREEN <- "#2FC1D3"
BLUE <- "#076FA1"
GREY <- "#C7C9CB"
GREY_DARKER <- "#5C5B5D"
RED <- "#E3120B"


# Chart 1 - The Line Chart

# Aesthetics defined in the `ggplot()` call are reused in the 
# `geom_line()` and `geom_point()` function calls.

p1 <- ggplot(export_shares, aes(Year, Share)) +
  geom_line(aes(color = Country), size = 2.4) +
  geom_point(
    aes(fill = Country), 
    size = 5, 
    pch = 21, # Type of point that allows us to have both color (border) and fill.
    color = "white", 
    stroke = 1 # The width of the border, i.e. stroke.
  ) +
  # Set values for the color and the fill
  scale_color_manual(values = c(GREY_DARKER, GREY, BLUE, GREEN, BROWN, BROWN_DARKER)) +
  scale_fill_manual(values = c(GREY_DARKER, GREY, BLUE, GREEN, BROWN, BROWN_DARKER)) + 
  # Do not include any legend
  theme(legend.position = "none")

p1

# Chart 2


