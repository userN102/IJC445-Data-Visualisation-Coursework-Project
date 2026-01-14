
library(dplyr)
library(readr)


# Merge ONS business demography indicators into a single industry–year panel
# Period: 2019–2023 (consistent across all indicators and used in the report visuals)
# Level: SIC division (later aggregated to SIC section for the final composite charts)
# Output: master_panel dataset used to compute rates, net change, survival, and growth indicators



# 1) Read and filter inputs
# Each input file was produced by a separate extraction script from ons_original.xlsx.
# We restrict to:
# - division level only (consistent industry unit)
# - years 2019–2023 (common period across indicators used in the report)

births <- read_csv("./Dataset/ONS_Business_Demography/Births_Of_New_Enterprises_2019_2024_by_Industry.csv") %>%
  filter(industry_level == "division", year >= 2019,
         year <= 2023) %>%
  select(industry_raw, industry_code, year, births_of_new_enterprises)


active_all <- read_csv("./Dataset/ONS_Business_Demography/Active_Enterprises_2019_2024_by_Industry.csv") %>%
  filter(industry_level == "division", year >= 2019,
         year <= 2023) %>%
  select(industry_code, year, active_enterprises)


deaths <- read_csv("./Dataset/ONS_Business_Demography/Deaths_Of_New_Enterprises_2019_2024_by_Industry.csv") %>%
  filter(industry_level == "division", year >= 2019,
         year <= 2023) %>%
  select(industry_code, year, deaths_of_new_enterprises)


high_growth <- read_csv("./Dataset/ONS_Business_Demography/High_Growth_Enterprises_2019_2024_by_Industry.csv") %>%
  filter(industry_level == "division", year >= 2019,
         year <= 2023) %>%
  select(industry_code, year, high_growth_enterprises)


survival <- read_csv("./Dataset/ONS_Business_Demography/Births_and_Survival_of_Enterprises_2019_2023_by_Industry.csv") %>%
  filter(industry_level == "division") %>%
  
  select(
    industry_code,
    year,
    percent_of_1_year,
    percent_of_2_year,
    percent_of_3_year,
    percent_of_4_year,
    percent_of_5_year,
    survival_of_1_year,
    survival_of_2_year,
    survival_of_3_year,
    survival_of_4_year,
    survival_of_5_year
  )


# 2) Merge into a single industry–year panel
# Births is used as the base table, then additional indicators are joined by industry_code × year.
# Left joins preserve all industry–year rows in the births dataset and attach other indicators when available.


master_panel <- births %>%
  left_join(active_all,  by = c("industry_code", "year")) %>%
  left_join(deaths,      by = c("industry_code", "year")) %>%
  left_join(survival,    by = c("industry_code", "year")) %>%
  left_join(high_growth, by = c("industry_code", "year"))



# 3) Save final output
# This dataset is the input for downstream calculations (rates, net change, aggregation to section level)

# Ensure output directory exists
output_dir <- "./Dataset/Final_Master_Datasets"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

write_csv(
  master_panel,
  "./Dataset/Final_Master_Datasets/master_panel_enterprise_demography_2019_2023.csv"
)

