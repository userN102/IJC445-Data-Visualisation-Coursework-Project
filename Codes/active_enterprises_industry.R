library(readxl)
library(dplyr)
library(tidyr)
library(stringr)


# Extract active enterprise counts by SIC industry and year from ONS Business Demography
# Source: ons_original.xlsx (ONS), Table 3.2
# Output: Long-format dataset (industry_code × year) for downstream merging and rate calculations


# Data source is provided ONS Excel file
file_path <- "./Dataset/ONS_Business_Demography/ons_original.xlsx"


# Read table with ONS header rows skipped as Excel formatting places metadata above the table
df <- read_excel(file_path, sheet = "Table 3.2", skip = 3)


# Remove fully empty rows or columns introduced by the Excel layout
df <- df %>%
  select(where(~ !all(is.na(.)))) %>%
  filter(!if_all(everything(), is.na))


# Clean column names to avoid issues when detecting year columns
names(df) <- str_trim(names(df))


# Standardise the industry label column as first column contains SIC code + label
df <- df %>%
  rename(industry_raw = 1) %>%
  mutate(industry_raw = str_trim(industry_raw))


# Extract SIC code from the start of the label
# - 2 digits = division level (used for consistent analysis)
# - 3 digits = group level (excluded to avoid mixing levels)
df <- df %>%
  mutate(
    industry_code = str_extract(industry_raw, "^[0-9]{2,3}"),
    industry_level = ifelse(nchar(industry_code) == 2, "division", "group")
  )


# Identify year columns (ONS tables store years as column headers)
year_cols <- names(df)[str_detect(names(df), "^[0-9]{4}")]


# Convert year columns to numeric (remove commas from Excel-formatted numbers)
df <- df %>%
  mutate(across(all_of(year_cols), ~ {
    .x %>%
      as.character() %>%
      str_replace_all(",", "") %>%
      as.numeric()
  }))


# Reshape to long format to create an industry-year panel structure
df_long <- df %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "active_enterprises"
  )


# Save output for merging with births/deaths/survival/high-growth tables
write.csv(
  df_long,
  "./Dataset/ONS_Business_Demography/Active_Enterprises_2019_2024_by_Industry.csv",
  row.names = FALSE
)

head(df_long)
