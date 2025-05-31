# Load required libraries
library(tidyverse)
library(highcharter)
library(scales)
library(readxl)

# Set BK Advisors color palette
deepBlue <- "#0A4C7F"
mediumBlue <- "#1976D2"
lightBlue <- "#64B5F6"
teal <- "#00ACC1"
navy <- "#0D47A1"
darkGray <- "#455A64"
lightGray <- "#ECEFF1"

# Define East African countries (verify names match WHO data)
east_africa <- c("Burundi", "Kenya", "Rwanda", "Uganda", "United Republic of Tanzania","South Sudan","Democratic Republic of the Congo")


# Read the dataset
eac_total_health_exp <- read_excel("./data/GHED_data.xlsx", 
                                  sheet = "Data") %>%
  filter(location %in% east_africa) %>%
  mutate(Year = as.numeric(year),
         Govt_Health_Exp = round(as.numeric(gghed_pc_usd), 2))

write.csv(eac_total_health_exp, file = "./data/eac-total-health-exp.csv", row.names = FALSE)


health_exp <- read_csv("./data/eac-total-health-exp.csv")

# Process Uganda data (2000-2022)
uganda_data <- health_exp %>%
  filter(location == "Uganda" | code == "UGA") %>%
  mutate(
    Total = che_pc_usd,
    Government = gghed_pc_usd,
    Private = pmax(0, pvtd_pc_usd - oop_pc_usd),  # Non-OOP private
    `Out-of-Pocket` = oop_pc_usd,
    External = ext_pc_usd,
    External_Pct = ext_pc_usd / Total * 100
  ) %>%
  select(year, Total, Government, Private, `Out-of-Pocket`, External, External_Pct)

# Create stacked column chart with total trend line
stacked_chart <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Uganda's Health Expenditure by Source (2000-2022)") %>%
  hc_subtitle(text = "Per Capita Expenditure in USD with Total Expenditure Trend") %>%
  hc_xAxis(categories = uganda_data$year, title = list(text = "Year")) %>%
  hc_yAxis(
    title = list(text = "Per Capita Expenditure (USD)"),
    labels = list(format = "${value}")
  ) %>%
  hc_plotOptions(
    column = list(
      stacking = "normal",
      dataLabels = list(
        enabled = FALSE
      )
    ),
    series = list(
      marker = list(enabled = FALSE)
    )
  ) %>%
  hc_add_series(
    name = "External Funding",
    data = uganda_data$External,
    color = deepBlue
  ) %>%
  hc_add_series(
    name = "Out-of-Pocket",
    data = uganda_data$`Out-of-Pocket`,
    color = teal
  ) %>%
  hc_add_series(
    name = "Private (non-OOP)",
    data = uganda_data$Private,
    color = mediumBlue
  ) %>%
  hc_add_series(
    name = "Government",
    data = uganda_data$Government,
    color = lightBlue
  ) %>%
  hc_add_series(
    name = "Total Expenditure",
    data = uganda_data$Total,
    type = "line",
    color = darkGray,
    lineWidth = 3,
    marker = list(radius = 5),
    yAxis = 0
  ) %>%
  hc_tooltip(
    shared = TRUE,
    pointFormat = "<span style='color:{point.color}'>\u25CF</span> {series.name}: <b>${point.y:.1f}</b><br/>"
  ) %>%
  hc_legend(
    align = "center",
    verticalAlign = "bottom",
    layout = "horizontal"
  )

# Create external funding percentage chart
pct_chart <- highchart() %>%
  hc_chart(type = "area") %>%
  hc_title(text = "External Funding as Percentage of Total Health Expenditure") %>%
  hc_xAxis(categories = uganda_data$year, title = list(text = "Year")) %>%
  hc_yAxis(
    title = list(text = "Percentage of Total"),
    labels = list(format = "{value}%"),
    max = 100
  ) %>%
  hc_add_series(
    name = "External Funding %",
    data = uganda_data$External_Pct,
    color = deepBlue,
    fillOpacity = 0.3
  ) %>%
  hc_tooltip(
    pointFormat = "<span style='color:{point.color}'>\u25CF</span> {series.name}: <b>{point.y:.1f}%</b>"
  ) %>%
  hc_legend(enabled = FALSE)

stacked_chart %>%
  hc_credits(
    enabled = TRUE,
    text = "Source: WHO Global Health Expenditure Database",
    href = "https://apps.who.int/nha/database"
  )%>%
  hc_exporting(enabled = TRUE,
               buttons = list(contextButton = list(theme = list(fill = lightGray))))


pct_chart

# Combine charts vertically

hw_grid(
  stacked_chart,
  pct_chart,
  ncol = 1,
  rowheight = c(2, 1)  # Make first chart taller
) %>%
  hc_credits(
    enabled = TRUE,
    text = "Source: WHO Global Health Expenditure Database",
    href = "https://apps.who.int/nha/database"
  )
