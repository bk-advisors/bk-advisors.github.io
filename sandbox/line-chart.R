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
eac_annual <- read.csv("./data/ug-eac-exports.csv")

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

# Line Chart with Labels at the end

# Create line chart with labels
ggplot(export_shares, aes(x = Year, y = Share, color = Country)) +
  geom_line(linewidth = 1) +
  # Add shadow text labels at the end of lines (2024)
  shadowtext::geom_shadowtext(
    data = export_shares %>% filter(Year == 2024),
    aes(label = Country, x = Year + 0.2),  # Small offset for label position
    hjust = 0,  # Left-align text
    bg.color = "white",  # Background halo color
    color = "black",     # Text color
    size = 3.5,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(2014, 2024, 1),
    limits = c(2014, 2026.5)  # Extend x-axis for labels
  ) +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Uganda's Export Share to EAC Countries (2014-2024)",
    subtitle = "Line labels show country positions in 2024",
    y = "Percentage Share",
    x = "Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none",  # Remove legend since we have direct labels
    panel.grid.major = element_line(linewidth = 0.25)
  )

# Stacked Area Chart

# Create stacked area chart - without labels
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

# Stacked area chart - with labels and viridis color scale


# Calculate optimal label positions
labeled_data <- export_shares %>%
  mutate(Country = fct_reorder(Country, Exports, sum, .desc = TRUE)) %>%
  group_by(Country) %>%
  # Find year with maximum export value for each country
  mutate(peak_year = Year[which.max(Exports)]) %>%
  group_by(Year) %>%
  arrange(desc(Country)) %>% # Match stacking order
  mutate(
    ymin = lag(cumsum(Exports), default = 0),
    ymax = cumsum(Exports),
    mid_y = (ymin + ymax)/2
  ) %>%
  ungroup()

# Create area chart with internal labels
ggplot(labeled_data, aes(x = Year, y = Exports, fill = Country)) +
  geom_area(color = "white", linewidth = 0.3, position = "stack") +
  # Add labels at peak years
  geom_shadowtext(
    data = . %>% filter(Year == peak_year),
    aes(x = Year, y = mid_y, label = Country),
    color = "white",
    bg.color = "black",
    size = 3.5,
    fontface = "bold",
    vjust = 0.5,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(2014, 2024, 1),
    expand = c(0, 0),
    limits = c(2014, 2026.5)  # Extend x-axis for labels
  ) +
  scale_y_continuous(
    labels = dollar_format(prefix = "$", suffix = "M"),
    expand = c(0, 0)
  ) +
  scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8) +
  labs(
    title = "Uganda's EAC Exports with In-Stream Labels",
    subtitle = "Country names placed at their respective peak export years",
    y = "Export Value (US$ Millions)",
    x = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, margin = margin(t = 5)),
    panel.grid.major.x = element_blank(),
    legend.position = "none"
  )



# Analysis ----------------------------------------------------------------

# First, define colors.
BROWN <- "#AD8C97"
BROWN_DARKER <- "#7d3a46"
GREEN <- "#2FC1D3"
BLUE <- "#076FA1"
GREY <- "#C7C9CB"
GREY_DARKER <- "#5C5B5D"
RED <- "#E3120B"

# Define Font

my_font <- "Roboto Slab"

font_add_google(name = my_font, family = my_font)

showtext_auto()



# Chart 1 - The Line Chart


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

# Customize Chart 1 layout
p1 <- p1 + 
  scale_x_continuous(
    limits = c(2013.5, 2024.5),
    expand = c(0, 0), # The horizontal axis does not extend to either side
    breaks = c(2014,2016,2018,2020,2022,2024),  # Set custom break locations
    labels = c("2014", "16", "18", "20","22","24") # And custom labels on those breaks!
  ) +
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0, 30, by = 5), 
    expand = c(0, 0)
  ) + 
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Remove all grid lines
    panel.grid = element_blank(),
    # But add grid lines for the vertical axis, customizing color and size 
    panel.grid.major.y = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks on the vertical axis by setting their length to 0
    axis.ticks.length.y = unit(0, "mm"), 
    # But keep tick marks on horizontal axis
    axis.ticks.length.x = unit(2, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only the bottom line of the vertical axis is painted in black
    axis.line.x.bottom = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = my_font, size = 16)
  )

p1

# Add annotations and title

# Add labels for the lines
p1 <- p1 +
  geom_shadowtext(
    data = export_shares %>% filter(Year == 2014),
    aes(label = Country, x = Year + 0.2),  # Small offset for label position
    hjust = 0,  # Left-align text
    bg.color = "white",  # Background halo color
    color = "black",     # Text color
    family = my_font,
    size = 6,
    show.legend = FALSE
  )

# Add labels for the horizontal lines
p1 <- p1 + 
  geom_text(
    data = data.frame(x = 2024.5, y = seq(0, 45, by = 5)),
    aes(x, y, label = y),
    hjust = 1, # Align to the right
    vjust = 0, # Align to the bottom
    nudge_y = 45 * 0.01, # The pad is equal to 1% of the vertical range (32 - 0)
    family = my_font,
    size = 6
  )

# Add title
p1 <- p1 +
  labs(
    title = "**EAC,** % Share of Uganda's Exports to the Region, 2014 - 2024",
  ) + 
  theme(
    # theme_markdown() is provided by ggtext and means the title contains 
    # Markdown that should be parsed as such (the '**' symbols)
    plot.title = element_markdown(
      family = my_font, 
      size = 18
    )
  )

p1

# Chart 2 - The Stacked Area Chart

# Calculate optimal label positions
exports_labeled <- export_shares %>%
  mutate(Country = fct_reorder(Country, Exports, sum, .desc = TRUE)) %>%
  group_by(Country) %>%
  # Find year with maximum export value for each country
  mutate(peak_year = Year[which.max(Exports)]) %>%
  group_by(Year) %>%
  arrange(desc(Country)) %>% # Match stacking order
  mutate(
    ymin = lag(cumsum(Exports), default = 0),
    ymax = cumsum(Exports),
    mid_y = (ymin + ymax)/2
  ) %>%
  ungroup()


p2 <- ggplot(exports_labeled) +
  # color = "white" indicates the color of the lines between the areas
  geom_area(aes(Year, Exports, fill = Country), color = "white") +
  scale_fill_manual(values = c(GREY_DARKER, GREY, BLUE, GREEN, BROWN, BROWN_DARKER)) +
  theme(legend.position = "None") # no legend

p2

# Customize the layout

p2 <- p2 + 
  scale_x_continuous(
    # Note: Data goes from 2014 to 2024. Extra space is added on the right
    # so there's room for the grid line labels ;)
    limits = c(2013.5, 2024.5),
    expand = c(0, 0), # The horizontal axis does not extend to either side
    breaks = c(2014,2016,2018,2020,2022,2024),  # Set custom break locations
    labels = c("2014", "16", "18", "20","22","24") # And custom labels on those breaks!
  ) +
  scale_y_continuous(
    limits = c(0, 2250),
    labels = dollar_format(prefix = "$", suffix = "M"),
    breaks = seq(0, 2250, by = 500), 
    expand = c(0, 0)
  ) + 
  theme(
    # Set background color to white
    panel.background = element_rect(fill = "white"),
    # Remove all grid lines
    panel.grid = element_blank(),
    # But add grid lines for the vertical axis, customizing color and size 
    panel.grid.major.y = element_line(color = "#A8BAC4", linewidth = 0.3),
    # Remove tick marks by setting their length to 0
    axis.ticks.length.y = unit(0, "mm"), 
    axis.ticks.length.x = unit(2, "mm"),
    # Remove the title for both axes
    axis.title = element_blank(),
    # Only bottom line of the vertical axis is painted in black
    axis.line.x.bottom = element_line(color = "black"),
    # Remove labels from the vertical axis
    axis.text.y = element_blank(),
    # But customize labels for the horizontal axis
    axis.text.x = element_text(family = my_font, size = 16)
  )

p2

# Adding annotations and labels

# Add labels to the area streams
p2 <- p2 +
  geom_shadowtext(
    data = exports_labeled %>% filter(Year == peak_year),
    aes(x = Year, y = mid_y, label = Country),
    color = "white",
    bg.color = "black",
    size = 6,
    family = my_font,
    fontface = "bold",
    vjust = 0.5,
    show.legend = FALSE
  )

p2

# Add labels for the horizontal lines

p2 <- p2 +
  geom_text(
    data = data.frame(x = 2024.5, y = seq(0, 2250, by = 500)),
    aes(x, y, label = y),
    hjust = 1,
    vjust = 0,
    nudge_y = 2250 * 0.01, # Again, the pad is equal to 1% of the vertical range.
    family = my_font,
    size = 6,
    inherit.aes = FALSE
  )

p2

# Add title

# Note again we use `element_markdown()` to render Markdown content
p2 <- p2 + 
  labs(
    title = "**Exports Value,** US$ Millions",
  ) + 
  theme(
    plot.title = element_markdown(
      family = my_font, 
      size = 18
    )
  )

p2

# Combining the Charts

p1 <- p1 + theme(plot.margin = margin(0, 0.05, 0, 0, "npc"))
p2 <- p2 + theme(plot.margin = margin(0, 0, 0.05, 0, "npc"))
p <- p1 | p2

p
