# --- Packages ---
# install.packages(c("readxl","dplyr","tidyr","ggplot2","fixest","broom","purrr","scales","countrycode"))
library(readxl)
library(fixest)
library(broom)
library(purrr)
library(scales)
library(countrycode)

# --- 0) Settings ---
path_xlsx <- "./data/GHED_data.xlsx"  # put your file path if different
eac_iso3  <- c("BDI","KEN","RWA","SSD","TZA","UGA","COD","SOM")   # EAC (expanded)
eac_core6 <- c("BDI","KEN","RWA","TZA","UGA","SSD")               # toggle if you prefer core set

use_set   <- eac_core6      # or switch to eac_core6
year_min  <- 2012
year_max  <- 2022          # set to last year in your file

# --- 1) Load just the columns we need (fast) ---
# Columns expected in GHED export:
#   code (ISO3), location (country), year
#   gghed_pc_usd = General Government Health Expenditure per capita (US$)
#   ext_pc_usd   = External Health Expenditure per capita (US$)
ghed <- read_excel(
  path_xlsx, sheet = "Data",
  col_types = "text"  # read as text then coerce needed columns (faster for very wide files)
)

# Coerce only needed columns and drop the rest
ghed_small <- ghed %>%
  transmute(
    code      = .data$code,
    location  = .data$location,
    year      = suppressWarnings(as.integer(.data$year)),
    gghed_pc  = suppressWarnings(as.numeric(.data$gghed_pc_usd)),
    ext_pc    = suppressWarnings(as.numeric(.data$ext_pc_usd))
  ) %>%
  filter(!is.na(code), !is.na(year)) %>%
  filter(code %in% use_set, year >= year_min, year <= year_max)

# Optional: if you want to exclude countries with < N years of data
min_years <- 10
keep_codes <- ghed_small %>%
  group_by(code) %>% summarise(ny = n_distinct(year), .groups="drop") %>%
  filter(ny >= min_years) %>% pull(code)

panel <- ghed_small %>%
  filter(code %in% keep_codes) %>%
  arrange(code, year) %>%
  group_by(code) %>%
  mutate(
    d_gov_pc   = gghed_pc - lag(gghed_pc),   # first difference of gov't spend per capita
    ext_pc_lag = lag(ext_pc)                 # lag external spend per capita
  ) %>%
  ungroup()

# --- 2) Quick diagnostics: how many usable rows? ---
usable <- panel %>% filter(!is.na(d_gov_pc), !is.na(ext_pc_lag))
message("Panel rows used in FE model: ", nrow(usable))

# --- 3) Panel fixed-effects regression (country & year FE) ---
# Interpretation: beta < 0 suggests that higher external funding (t-1) is followed by
# a decrease in gov’t health spend per capita (t) — a displacement pattern.
est <- feols(
  d_gov_pc ~ ext_pc_lag | code + year,
  data = usable,
  vcov = ~code  # cluster by country
)

print(summary(est))

# Tidy coefficient + CI for quick reporting
coef_tbl <- broom::tidy(est, conf.int = TRUE)
print(coef_tbl)

# --- 4) Visualization A: Panel scatter with FE slope annotation ---
p_scatter <- usable %>%
  ggplot(aes(x = ext_pc_lag, y = d_gov_pc)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x, linewidth = 0.6) +
  labs(
    title = "Displacement test (EAC): ΔGov’t health spend per capita vs lagged external spend",
    subtitle = paste0("Country & year FE in model; clustered SEs by country (N rows = ", nrow(usable), ")"),
    x = "External health expenditure per capita (US$), lagged 1 year",
    y = "Change in gov’t health spend per capita (US$)"
  ) +
  scale_x_continuous(labels = label_dollar(accuracy = 1)) +
  scale_y_continuous(labels = label_dollar(accuracy = 1)) +
  theme_minimal(base_size = 12)

print(p_scatter)

# --- 5) Visualization B: Country-level slopes (within-country) ---
# For each country, regress ΔGov on lagged external; show coefficient + CI
by_ctry <- usable %>%
  group_split(code)

fit_ctry <- map_dfr(by_ctry, function(d) {
  m <- lm(d_gov_pc ~ ext_pc_lag, data = d)
  broom::tidy(m, conf.int = TRUE) %>%
    filter(term == "ext_pc_lag") %>%
    mutate(code = d$code[1])
})

# Order countries by coefficient
fit_ctry <- fit_ctry %>% arrange(estimate) %>% mutate(code = factor(code, code))

p_coef <- ggplot(fit_ctry, aes(estimate, code)) +
  geom_vline(xintercept = 0, linewidth = 0.3) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.15) +
  labs(
    title = "Country-specific displacement slopes (ΔGov ~ lag(External))",
    x = "Coefficient (US$ change in gov’t pc per +US$1 external pc, lagged)",
    y = NULL
  ) +
  theme_minimal(base_size = 12)

print(p_coef)

# --- 6) Visualization C: Mini-profiles (indexed time-series) ---
# Index both series to 100 in the first non-missing year per country to compare co-movement
index_it <- function(x) { x / first(na.omit(x)) * 100 }

index_df <- ghed_small %>%
  filter(code %in% keep_codes) %>%
  group_by(code) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(
    gghed_idx = index_it(gghed_pc),
    ext_idx   = index_it(ext_pc)
  ) %>%
  ungroup() %>%
  pivot_longer(c(gghed_idx, ext_idx), names_to = "series", values_to = "index")

series_labels <- c(gghed_idx = "Gov’t health spend (pc), index = 100 at first year",
                   ext_idx   = "External health spend (pc), index = 100 at first year")

p_facets <- ggplot(index_df, aes(year, index, color = series)) +
  geom_hline(yintercept = 100, linewidth = 0.2) +
  geom_line() +
  facet_wrap(~ code, scales = "free_y") +
  labs(
    title = "Government vs External Health Spending (per capita, indexed) in 5 EAC countries, 2012-2022",
    subtitle = "As external aid ramps up, gov’t flattens or dips a year later.",
    x = NULL, y = "Index (first year = 100)", color = NULL,
    caption = "Source: WHO Global Health Expenditure Database | Analysis: BK Advisors"
  ) +
  scale_color_manual(values = c("#00ACC1", "#d95f02"), labels = series_labels) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

print(p_facets)

# --- 7) Compact result line for your post ---
beta <- coef_tbl$estimate[coef_tbl$term=="ext_pc_lag"]
lo   <- coef_tbl$conf.low[coef_tbl$term=="ext_pc_lag"]
hi   <- coef_tbl$conf.high[coef_tbl$term=="ext_pc_lag"]
n_ct <- length(unique(usable$code))

cat("\n--- Summary line for LinkedIn ---\n")
cat(
  sprintf(
    "Across %d EAC countries, a one-year-lagged +$1 in external health funding per capita is associated with a %.2f change (95%% CI: %.2f to %.2f) in government health spending per capita the following year, controlling for country and year.",
    n_ct, beta, lo, hi
  ),
  "\n"
)


