
library(tidyverse)
library(highcharter)
library(readxl)


# Define East African countries (verify names match WHO data)
east_africa <- c("Burundi", "Kenya", "Rwanda", "Uganda", "Tanzania","South Sudan","Democratic Republic of the Congo")

# Read and clean data
eac_health_exp <- read_excel("./data/GHED_data.xlsx", 
                         sheet = "Data") %>%
  filter(location %in% east_africa) %>%
  filter(year >= 2012) %>%
  mutate(Year = as.numeric(year),
         Expenditure = round(as.numeric(che_pc_usd), 2))

#write.csv(eac_health_exp, file = "./data/eac-health-exp.csv", row.names = FALSE)
#eac_health_exp <- read.csv("./data/eac-health-exp.csv")

# Create interactive visualization
hchart(eac_health_exp, 
       type = "line",
       hcaes(x = Year, 
             y = Expenditure, 
             group = location)) %>%
  hc_title(text = "Health Expenditure per Capita Trend in East Africa (2012-2022)",
           style = list(fontWeight = "bold", fontSize = "16px")) %>%
  hc_subtitle(text = "Current international $US") %>%
  hc_yAxis(title = list(text = "Health Expenditure per Capita ($US)"),
           labels = list(format = "${value}"),
           min = 0) %>%
  hc_xAxis(title = list(text = "Year"),
           allowDecimals = FALSE,
           tickInterval = 2) %>%
  hc_tooltip(valueDecimals = 2,
             valuePrefix = "$",
             headerFormat = "<b>{series.name}</b><br>",
             pointFormat = "Year: {point.x}<br>Expenditure: {point.y}") %>%
  hc_legend(element_blank) %>%
  hc_caption(
    text = "Source: WHO Global Health Expenditure Database",
    style = list(fontSize = "10px")) %>% 
  hc_exporting(enabled = TRUE) %>%
  hc_add_theme(hc_theme_smpl()) %>%
  hc_plotOptions(series = list(marker = list(enabled = TRUE),
                               connectNulls = TRUE))
