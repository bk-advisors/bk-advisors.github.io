
library(tidyverse)
library(highcharter)
library(readxl)

# BK Advisors color palette
bk_colors <- list(  # Define as list instead of vector
  deepBlue = "#0A4C7F",
  mediumBlue = "#1976D2",
  lightBlue = "#64B5F6",
  teal = "#00ACC1",
  navy = "#0D47A1",
  darkGray = "#455A64",
  lightGray = "#ECEFF1"
)

# Define East African countries (verify names match WHO data)
east_africa <- c("Burundi", "Kenya", "Rwanda", "Uganda", "United Republic of Tanzania","South Sudan","Democratic Republic of the Congo")

# Read csv file

eac_govt_health_exp <- read.csv("./data/eac-govt-health-exp.csv")

# Visualization thats compliant with color palette
plot_govt_exp <- hchart(eac_govt_health_exp, 
       type = "line",
       hcaes(x = Year, 
             y = Govt_Health_Exp, 
             group = location)) %>%
  hc_colors(as.list(unname(bk_colors))[1:5]) %>% # Changed to as.list(unname())
  hc_title(text = "Government Health Expenditure per Capita Trend in East Africa (2012-2022)",
           style = list(fontWeight = "bold", 
                        fontSize = "20px",
                        color = bk_colors["darkGray"])) %>%
  hc_subtitle(text = "Current international $US",
              style = list(color = bk_colors["darkGray"])) %>%
  hc_yAxis(title = list(text = "Govt. Health Expenditure per Capita ($US)",
                        style = list(color = bk_colors["darkGray"])),
           labels = list(format = "${value}",
                         style = list(color = bk_colors["darkGray"])),
           gridLineColor = bk_colors["lightGray"],
           min = 0) %>%
  hc_xAxis(title = list(text = "Year",
                        style = list(color = bk_colors["darkGray"])),
           labels = list(style = list(color = bk_colors["darkGray"])),
           allowDecimals = FALSE,
           tickInterval = 2,
           gridLineColor = bk_colors["lightGray"]) %>%
  hc_tooltip(valueDecimals = 2,
             valuePrefix = "$",
             headerFormat = "<span style='color:{point.color}'>\u25CF</span> <b>{series.name}</b><br>",
             pointFormat = "Year: {point.x}<br>Govt. Health Expenditure: {point.y}",
             backgroundColor = bk_colors["lightGray"]) %>%
  hc_legend(element_blank) %>%
  hc_caption(
    text = "Source: WHO Global Health Expenditure Database",
    style = list(fontSize = "10px")) %>%
  hc_exporting(enabled = TRUE,
               buttons = list(contextButton = list(theme = list(fill = bk_colors["lightGray"])))) %>%
  hc_add_theme(hc_theme_merge(
    hc_theme_smpl(),
    hc_theme(
      chart = list(backgroundColor = "white"),
      title = list(align = "left"),
      legend = list(itemHoverStyle = list(color = bk_colors["mediumBlue"]))
    )
  )) %>%
  hc_plotOptions(series = list(
    marker = list(enabled = TRUE,
                  fillColor = "white",
                  lineWidth = 2,
                  lineColor = NULL),
    dataLabels = list(
      enabled = TRUE,
      allowOverlap = FALSE,
      formatter = JS("function() {
      if (this.point.x === 2022) {
        return '<span style=\"color: ' + this.series.color + '\">' + 
               this.series.name + '</span>';
      }
      return '';
    }"),
      style = list(color = bk_colors["darkGray"], 
                   fontSize = "12px",
                   textOutline = "none"),
      align = "left",
      x = 5
    ),
    states = list(hover = list(halo = list(size = 10)))
  ))

plot_govt_exp
