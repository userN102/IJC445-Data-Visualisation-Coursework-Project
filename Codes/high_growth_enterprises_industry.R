library(readxl)
library(dplyr)
library(tidyr)
library(stringr)



# Extract high-growth enterprise counts by SIC industry and year
# Source: ONS Business Demography Excel file (ons_original.xlsx), Table 7.2
# Output: Long-format dataset (industry_code × year) used for Figure 5 comparisons

# Data source (provided ONS Excel file)
file_path <- "./Dataset/ONS_Business_Demography/ons_original.xlsx"

# Read Table 7.2, skipping metadata rows added by ONS Excel formatting
df <- read_excel(file_path, sheet = "Table 7.2", skip = 3)


# Remove fully empty rows and columns inherited from the Excel layout
df <- df %>%
  select(where(~ !all(is.na(.)))) %>%
  filter(!if_all(everything(), is.na))

# Clean column names to ensure reliable year-column detection
names(df) <- str_trim(names(df))


# Standardise the industry label column (first column contains SIC code + description)
df <- df %>%
  rename(industry_raw = 1) %>%
  mutate(industry_raw = str_trim(industry_raw))


# Extract SIC code from the start of the industry label
# - 2 digits correspond to division level (used for consistent analysis)
# - 3 digits correspond to group level
df <- df %>%
  mutate(
    industry_code = str_extract(industry_raw, "^[0-9]{2,3}"),
    industry_level = ifelse(nchar(industry_code) == 2, "division", "group")
  )


# Identify year columns (ONS stores years as column headers)
year_cols <- names(df)[str_detect(names(df), "^[0-9]{4}")]


# Convert year columns to numeric (remove commas from Excel-formatted numbers)
df <- df %>%
  mutate(across(all_of(year_cols), ~ {
    .x %>%
      as.character() %>%
      str_replace_all(",", "") %>%
      as.numeric()
  }))


# Reshape to long format to create an industry–year pane
df_long <- df %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year",
    values_to = "high_growth_enterprises"
  )


# Save output for merging with entry/survival indicators in the composite visualisation
write.csv(
  df_long,
  "./Dataset/ONS_Business_Demography/High_Growth_Enterprises_2019_2024_by_Industry.csv",
  row.names = FALSE
)

head(df_long)
