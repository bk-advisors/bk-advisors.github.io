
library(tidyverse)
library(lubridate)

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

### 1. Vaccine Introduction Timeline (Strategic Context)


vaccine_data <- data.frame(
  Vaccine = c("Pentavalent", "PCV", "Rotavirus", "HPV", "Covax", "HepB Birth", "MR2", "IPV2", "Malaria", "Mpox", "NVI"),
  Year = c(2010, 2014, 2013, 2016, 2021, 2022, 2022, 2022, 2025, 2025, 2026),
  Type = c("Routine", "Routine", "Routine", "Targeted", "Pandemic", "Routine", "Booster", "Booster", "Routine", "Outbreak", "Routine")
)

ggplot(vaccine_data, aes(x = Year, y = reorder(Vaccine, Year), color = Type)) +
  geom_point(size = 4) +
  geom_vline(xintercept = 2026, linetype = "dashed", color = bk_colors["navy"], size = 1) +
  annotate("text", x = 2026.5, y = 3, label = "Gavi Transition", angle = 90, color = bk_colors["navy"]) +
  labs(title = "Uganda's Vaccine Introduction Timeline (2010-2025)",
       subtitle = "Accelerated adoption precedes Gavi graduation",
       x = "Year", y = "New Vaccines",
       caption = "Source: WHO Uganda ToR Document") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

### 2. Co-financing

financing <- data.frame(
  Year = c(2023, 2025),
  Amount = c(7.4, 11.4), # Millions USD
  Event = c("Current", "Post-Malaria Vaccine")
)

ggplot(financing, aes(x = Event, y = Amount, fill = Event)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0("$", Amount, "M")), vjust = -0.5, size = 5) +
  labs(title = "Uganda's Rising Vaccine Co-financing Obligations",
       subtitle = "Projected 54% increase by 2025",
       x = "", y = "Millions USD",
       caption = "Source: ToR Section 1 (Current: $7.4M → $11.4M with Malaria vaccine)") +
  ylim(0, 13) +
  theme_minimal() +
  scale_fill_manual(values = c("#00ACC1", "#0D47A1")) +
  theme(legend.position = "none")

### 3. Demonstrates project management understanding

deliverables <- data.frame(
  Task = c("Inception Report", "Vaccine Impact Assessment", "Investment Case", "Advocacy Products"),
  Start = as.Date(c("2025-01-01", "2025-02-10", "2025-03-20", "2025-05-20")),
  End = as.Date(c("2025-01-30", "2025-03-10", "2025-05-10", "2025-06-20"))
)

ggplot(deliverables) +
  geom_segment(aes(x = Start, xend = End, y = reorder(Task, Start), yend = Task), 
               size = 6, color = "#2ca25f") +
  labs(title = "Proposed Project Timeline (6 Months)",
       subtitle = "100 person-days effort per ToR Section 5",
       x = "Timeline", y = "",
       caption = "Adapted from ToR Deliverables Table") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y")
