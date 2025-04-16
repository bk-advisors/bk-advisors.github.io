# Setup ----------------------------------------------------------------

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

