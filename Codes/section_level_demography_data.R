
library(tidyverse)
library(readxl)
library(stringr)
library(readr)




#   1) Build a SIC division → section lookup from the ONS "Industry Definition" sheet
#   2) Filter to the five selected sections for the coursework
#   3) Merge lookup with the division-level master panel and aggregate to section–year totals
# Output:
#   - sic_division_to_section_lookup.csv
#   - section_year_business_demography.csv (input for Figures 1–5)



# 1) Create SIC section lookup
# The ONS "Industry Definition" sheet provides section names and division ranges
sic_raw <- read_excel(
  "./Dataset/ONS_Business_Demography/ons_original.xlsx",
  sheet = "Industry Definition",
  skip = 2
)

names(sic_raw)


# Keep only columns needed for mapping divisions to sections
sic_clean <- sic_raw %>%
  select(
    section_name = Description,
    sic_section = `SIC07 section letter`,
    division_range = Division
  ) %>%
  filter(!is.na(division_range))


# Expand division ranges (e.g., "10-33") into individual division codes ("10", "11", ..., "33")
sic_section_lookup <- sic_clean %>%
  mutate(
    division_range = str_replace_all(division_range, " ", ""),
    start_div = as.integer(str_extract(division_range, "^\\d+")),
    end_div   = as.integer(str_extract(division_range, "\\d+$"))
  ) %>%
  rowwise() %>%
  mutate(
    division = list(
      sprintf("%02d", seq(start_div, end_div))
    )
  ) %>%
  unnest(division) %>%
  select(division, sic_section, section_name) %>%
  distinct()


# Quick checks for division coverage and duplicates
sic_section_lookup %>%
  count(section_name, sort = TRUE)


# Check no missing divisions
anyNA(sic_section_lookup$division)


# 2) Restrict to the five industries

key_sections <- c(
  "Manufacturing",
  "Construction",
  "Wholesale and retail; repair of motor vehicles",
  "Accommodation & food services",
  "Professional, scientific & technical"
)

sic_section_lookup <- sic_section_lookup %>%
  filter(section_name %in% key_sections)


# Save lookup for reproducibility and downstream use
write_csv(
  sic_section_lookup,
  "./Dataset/Final_Master_Datasets/sic_division_to_section_lookup.csv"
)

# 3) Merge lookup with master panel (division–year)

master_df <- read_csv(
  "./Dataset/Final_Master_Datasets/master_panel_enterprise_demography_2019_2023.csv"
)

sic_lookup <- read_csv(
  "./Dataset/Final_Master_Datasets/sic_division_to_section_lookup.csv"
)

glimpse(master_df)
glimpse(sic_lookup)



# Ensure SIC codes match formatting (two-digit strings for divisions)
master_df <- master_df %>% mutate(industry_code = str_pad(industry_code, 2, pad = "0"))

sic_lookup <- sic_lookup %>% mutate(division = str_pad(division, 2, pad = "0"))

# Keep only division rows that belong to the selected sections
master_df <- master_df %>%
  right_join(sic_lookup, by = c("industry_code" = "division"))


# 4) Aggregate division-level indicators to section–year totals

section_level_demography_data <- master_df %>%
  group_by(year, section_name) %>%
  summarise(
    
    births_of_new_enterprises = sum(births_of_new_enterprises, na.rm = TRUE),
    active_enterprises        = sum(active_enterprises, na.rm = TRUE),
    deaths_of_new_enterprises = sum(deaths_of_new_enterprises, na.rm = TRUE),
    high_growth_enterprises   = sum(high_growth_enterprises, na.rm = TRUE),
    
    survivors_1yr = sum(survival_of_1_year, na.rm = TRUE),
    survival_rate = if_else(
      births_of_new_enterprises > 0,
      survivors_1yr / births_of_new_enterprises,
      NA_real_
    ),
    .groups = "drop"
  )

View(section_level_demography_data)


# Save the section–year dataset used for the composite visualisation
write_csv(
  section_level_demography_data,
  "./Dataset/Final_Master_Datasets/section_year_business_demography.csv"
)
