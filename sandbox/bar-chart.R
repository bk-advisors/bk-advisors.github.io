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


# Filter for EAC countries only, from 2014 to 2024
eac_annual <- df_annual_clean %>%
  filter(Country %in% eac_countries) |> 
  filter(Year >= 2014)

# Country Breakdown
latest_year <- max(eac_annual$Year)

country_breakdown <- eac_annual %>%
  filter(Year == latest_year) %>%
  mutate(Share = Exports/sum(Exports)) |> 
  mutate(y = seq(length(Country)) * 0.9)

# Convert the class

country_breakdown %>% 
  arrange(desc(Exports)) %>%
  mutate(Country = factor(Country, levels = Country)) -> country_breakdown

# Recheck the data

str(country_breakdown)

# Preparing colors and fonts

# Colors

bar_color <- c('#066f9f')
red_icon <- c('#ee1c25')
grid_line_color <- c('#d9dfe2')

# Font

my_font <- "Roboto Slab"

font_add_google(name = my_font, family = my_font)

showtext_auto()

# Reference lines
# Calculate subtle reference lines at meaningful intervals
value_max <- max(country_breakdown$Exports)
tick_interval <- 100  # Meaningful interval for this data
ticks <- seq(0, ceiling(value_max/tick_interval)*tick_interval, by = tick_interval)


# Analysis and inputs ----------------------------------------------------------------

# Draw the first basic chart

ggplot(country_breakdown, aes(x = reorder(Country, Exports), y = Exports)) +
  geom_col(fill=bar_color, width = 0.6) +
  coord_flip() +
  theme_minimal() -> p1

# Show the first chart

p1



# Chart 2 ----------------------------------------------------

# The second chart

p2 <- p1 +
  labs(title = paste("Uganda's Exports to the East African Community", latest_year),
       subtitle = "Value in US$ Millions",
       caption = "Sources: Central Bank of Uganda | Formal Trade | 2024") +
  geom_col(fill = bar_color, width = 0.6) +
  # Add subtle, meaningful reference lines
  geom_vline(xintercept = ticks, color = "gray90", linewidth = 0.3) +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.title = element_blank()) + 
  theme(axis.ticks = element_blank()) + 
  theme(panel.grid.major.x = element_line(color = grid_line_color)) +   
  theme(plot.title = element_text(size = 20, face = "bold", color = "black", family = my_font)) + 
  theme(plot.subtitle = element_text(size = 15, color = "black", family = my_font)) + 
  theme(plot.caption = element_text(size = 10, color = "grey30", family = my_font, hjust = 0, lineheight = 2))+
  theme(axis.text.x = element_text(size = 10, family = my_font)) + 
  theme(axis.text.y = element_text(size = 15, face = "bold", family = my_font))

# Show the chart

p2

# Add labels

# Version 1

p2 + 
  geom_text(aes(label = paste0("$", round(Exports), "M")), 
            hjust = -0.2, 
            size = 5, 
            family = my_font, 
            color = "gray20")

# Version 2
p3 <- p2 +
  geom_shadowtext(aes(label = paste0("$", round(Exports), "M")),
                  hjust = 1, 
                  nudge_x = 0, 
                  colour = bar_color, 
                  bg.colour = "white", 
                  bg.r = 0.2,
                  family = my_font, 
                  size = 5)

p3
# Add company logo (if needed)


p3 +
  geom_image(data = tibble(Exports = 500, Country = "Burundi"),
             aes(image = "./assets/BKA_Enhanced_Logo_Apr2025.png"),
             size = 0.2)


