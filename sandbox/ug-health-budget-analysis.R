library(tidyverse)
library(scales)
library(highcharter)



# Create the dataset
budget_data <- tibble::tribble(
  ~Fiscal_Year, ~Total_Budget_UGX_trn, ~Health_UGX_trn, ~Health_Perc,
  "2024/25", 72.136, 2.950, 4.09,
  "2025/26", 72.380, 5.870, 8.11
) %>%
  mutate(
    Non_Health_UGX_trn = Total_Budget_UGX_trn - Health_UGX_trn,
    Fiscal_Year = factor(Fiscal_Year, levels = c("2024/25", "2025/26"))
  )

# Create stacked column charts
ggplot(budget_data, aes(x = Fiscal_Year)) +
  geom_col(
    aes(y = Total_Budget_UGX_trn, fill = "Non-Health"),
    width = 0.7
  ) +
  geom_col(
    aes(y = Health_UGX_trn, fill = "Health"),
    width = 0.7
  ) +
  geom_text(
    aes(y = Health_UGX_trn, 
        label = paste0(Health_Perc, "%")),
    vjust = -0.5, 
    size = 5,
    fontface = "bold",
    color = "#2F4F4F"
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "T", scale = 1),
    expand = expansion(mult = c(0, 0.15))
  ) +
  scale_fill_manual(
    values = c("Health" = "#1E90FF", "Non-Health" = "#D3D3D3"),
    name = "Budget Segment"
  ) +
  labs(
    title = "Uganda Health Budget as Proportion of National Budget",
    subtitle = "FY2024/25 vs FY2025/26 Allocation Comparison",
    y = "Budget (UGX Trillions)",
    x = "Fiscal Year",
    caption = "Source: Uganda Ministry of Finance, Parliament Reports | BK Advisors Analysis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    plot.caption = element_text(hjust = 0, margin = margin(t = 10))
  ) +
  guides(fill = guide_legend(reverse = TRUE))

# Save the plot (optional)
ggsave("uganda_health_budget_proportion.png", width = 10, height = 8, dpi = 300)


# In USD


# Create dataset with USD values
budget_data <- tibble::tribble(
  ~Fiscal_Year, ~Total_Budget_USD_bn, ~Health_USD_bn, ~Health_Perc,
  "2024/25", 19.496, 0.797, 4.09,
  "2025/26", 19.562, 1.586, 8.11
) %>%
  mutate(
    Non_Health_USD_bn = Total_Budget_USD_bn - Health_USD_bn,
    Fiscal_Year = factor(Fiscal_Year, levels = c("2024/25", "2025/26"))
  )

# Create stacked column charts
ggplot(budget_data, aes(x = Fiscal_Year)) +
  geom_col(
    aes(y = Total_Budget_USD_bn, fill = "Non-Health"),
    width = 0.7
  ) +
  geom_col(
    aes(y = Health_USD_bn, fill = "Health"),
    width = 0.7
  ) +
  # Health percentage labels
  geom_text(
    aes(y = Health_USD_bn, 
        label = paste0(Health_Perc, "%")),
    vjust = -0.5, 
    size = 5,
    fontface = "bold",
    color = "#2F4F4F"
  ) +
  # USD amount labels
  geom_text(
    aes(y = Health_USD_bn/2, 
        label = paste0("USD ", round(Health_USD_bn, 3), "B")),
    color = "white",
    size = 4.5,
    fontface = "bold"
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "B", scale = 1),  # B for billions
    expand = expansion(mult = c(0, 0.15)),
    limits = c(0, 22)
  ) +
  scale_fill_manual(
    values = c("Health" = "#1E90FF", "Non-Health" = "#D3D3D3"),
    name = "Budget Segment"
  ) +
  labs(
    title = "Uganda Health Budget Allocation in National Budget",
    subtitle = "FY2024/25 vs FY2025/26 (USD Values)",
    y = "Budget (USD Billions)",
    x = "Fiscal Year",
    caption = "Source: Uganda Ministry of Finance, Parliament Reports | Exchange Rate: 1 USD = 3,700 UGX\nBK Advisors Analysis"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    plot.subtitle = element_text(hjust = 0.5, size = 14, margin = margin(b = 15)),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    plot.caption = element_text(hjust = 0, margin = margin(t = 10))
  ) +
  guides(fill = guide_legend(reverse = TRUE))

# Save the plot
ggsave("uganda_health_budget_usd.png", width = 10, height = 8, dpi = 300)

# Publish ready
library(tidyverse)
library(scales)
library(ggthemes)

# Create dataset with USD values

budget_data <- tibble::tribble(
  ~Fiscal_Year, ~Total_Budget_USD_bn, ~Health_USD_bn, ~Health_Perc,
  "2024/25", 19.496, 0.797, 4.09,
  "2025/26", 19.562, 1.586, 8.11
) 

budget_data %>%
  mutate(
    Non_Health_USD_bn = Total_Budget_USD_bn - Health_USD_bn,
    Fiscal_Year = factor(Fiscal_Year, levels = c("2024/25", "2025/26")))
    )

# Save as CSV (optional)
write_csv(budget_data, "ug_health_budget_summary.csv")
    
    # Create publication-ready visualization
    ggplot(budget_data, aes(x = Fiscal_Year)) +
      # Background bars (full budget)
      geom_col(
        aes(y = Total_Budget_USD_bn, fill = "Non-Health"),
        width = 0.65,
        alpha = 0.9
      ) +
      # Health allocation bars
      geom_col(
        aes(y = Health_USD_bn, fill = "Health"),
        width = 0.65,
        alpha = 1
      ) +
      # Health percentage labels
      geom_text(
        aes(y = Health_USD_bn, 
            label = paste0(Health_Perc, "%")),
        vjust = -0.8, 
        size = 6.5,
        fontface = "bold",
        color = "#005A9C",
        family = "Segoe UI"
      ) +
      # USD amount labels
      geom_text(
        aes(y = Health_USD_bn/2, 
            label = paste0("$", round(Health_USD_bn, 2), "B")),
        color = "white",
        size = 6,
        fontface = "bold",
        family = "Segoe UI"
      ) +
      # Growth arrows and labels
      geom_segment(
        aes(x = 1.15, xend = 1.85, 
            y = Health_USD_bn[1] + 0.5, yend = Health_USD_bn[2] + 0.5),
        arrow = arrow(length = unit(0.25, "cm"), 
                      color = "#E63946",
                      linewidth = 1.2
        ) +
          geom_label(
            aes(x = 1.5, y = 2.5, 
                label = "99% INCREASE\nin Health Allocation"),
            size = 5,
            fontface = "bold",
            fill = "#F8F9FA",
            color = "#E63946",
            family = "Segoe UI",
            label.size = NA
          ) +
          # Styling
          scale_y_continuous(
            labels = label_dollar(suffix = "B", scale = 1),
            expand = expansion(mult = c(0, 0.15)),
            limits = c(0, 23),
            breaks = seq(0, 20, 5)
          ) +
          scale_fill_manual(
            values = c("Health" = "#1A85FF", "Non-Health" = "#E2E2E2"),
            name = NULL,
            guide = guide_legend(reverse = TRUE)
          ) +
          labs(
            title = "UGANDA'S HEALTH BUDGET NEARLY DOUBLED IN 2025/26",
            subtitle = "Health Allocation as Percentage of National Budget (USD Values)",
            y = "Budget (USD Billions)",
            x = NULL,
            caption = "Sources: Uganda Ministry of Finance, Parliament Reports | Analysis: BK Advisors\nExchange Rate: 1 USD = 3,700 UGX | Graphic: Health Economics Awareness Campaign"
          ) +
          theme_fivethirtyeight() +
          theme(
            text = element_text(family = "Segoe UI"),
            plot.title = element_text(
              face = "bold", 
              size = 24,
              hjust = 0.5,
              margin = margin(t = 10, b = 5),
              color = "#003366"
            ),
            plot.subtitle = element_text(
              size = 18,
              hjust = 0.5,
              margin = margin(b = 20),
              color = "#333333"
            ),
            legend.position = "top",
            legend.text = element_text(size = 14),
            legend.margin = margin(b = 15),
            axis.title.y = element_text(
              size = 14,
              face = "bold",
              margin = margin(r = 10),
              color = "#333333"
            ),
            axis.text = element_text(
              size = 14,
              face = "bold",
              color = "#333333"
            ),
            panel.grid.major.x = element_blank(),
            plot.caption = element_text(
              hjust = 0.5,
              size = 12,
              color = "#666666",
              margin = margin(t = 15)
            ),
            plot.background = element_rect(fill = "#F8F9FA", color = NA),
            panel.background = element_rect(fill = "#F8F9FA", color = NA)
          ))
        
        # Save high-resolution version
        ggsave("uganda_health_budget_linkedin.png", 
               width = 12, 
               height = 10, 
               dpi = 400,
               bg = "#F8F9FA")
    



# Save the plot
ggsave("uganda_health_budget_usd_abuja.png", width = 12, height = 9, dpi = 300)

