library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)


# Extract enterprise births and survival outcomes by SIC industry and year
# Source: ONS Business Demography Excel file (ons_original.xlsx), Tables 5.2a–5.2e
# Output: Industry-level panel containing births and 1–5 year survival counts and rates


# Data source (provided ONS Excel file)
file_path <- "./Dataset/ONS_Business_Demography/ons_original.xlsx"

# ONS survival tables are split across sheets by birth cohort year (2019–2023)
sheets_to_read <- c("Table 5.2a", "Table 5.2b", "Table 5.2c", "Table 5.2d", "Table 5.2e")


# Map each table to its corresponding birth year
extract_year <- function(sheet_name) {
  case_when(
    sheet_name == "Table 5.2a" ~ 2019,
    sheet_name == "Table 5.2b" ~ 2020,
    sheet_name == "Table 5.2c" ~ 2021,
    sheet_name == "Table 5.2d" ~ 2022,
    sheet_name == "Table 5.2e" ~ 2023,
    TRUE ~ NA_real_
  )
}


# Function to clean and standardise a single survival table
clean_sheet_52 <- function(sheet_name) {
  
  # Identify the cohort year associated with the sheet
  year <- extract_year(sheet_name)
  
  
  # Read table while skipping ONS metadata rows and suppressing column names
  df <- read_excel(
    file_path,
    sheet = sheet_name,
    skip = 4,
    col_names = FALSE
  ) %>% 
    # Remove fully empty rows introduced by Excel formatting
    filter(!if_all(everything(), is.na))
  
  # Assign explicit column names (ONS survival tables do not include clean headers)
  names(df) <- c(
    "industry_raw",
    "births",
    "survival_of_1_year", "percent_of_1_year",
    "survival_of_2_year", "percent_of_2_year",
    "survival_of_3_year", "percent_of_3_year",
    "survival_of_4_year", "percent_of_4_year",
    "survival_of_5_year", "percent_of_5_year"
  )[1:ncol(df)]
  
  
  # Standardise industry labels and extract SIC codes
  # Survival tables include multiple SIC levels and section headers
  df <- df %>%
    mutate(
      industry_raw = str_trim(as.character(industry_raw)),
      industry_code = str_extract(industry_raw, "^[0-9]{2,4}"),
      industry_level = case_when(
        nchar(industry_code) == 2 ~ "division",
        nchar(industry_code) == 3 ~ "group",
        nchar(industry_code) == 4 ~ "class",
        TRUE ~ "header"
      )
    ) %>%
    # Remove section header rows to retain only industry records
    filter(industry_level != "header")   
  
  # Convert numeric columns from Excel-formatted strings to numeric values
  df <- df %>%
    mutate(across(
      c(births,
        survival_of_1_year, survival_of_2_year, survival_of_3_year,
        survival_of_4_year, survival_of_5_year,
        percent_of_1_year, percent_of_2_year, percent_of_3_year,
        percent_of_4_year, percent_of_5_year),
      ~ as.numeric(str_replace_all(.x, ",", ""))
    ))
  
  # Add birth cohort year to enable industry–year panel structure
  df$year <- year
  
  # Return cleaned data in wide format
  df %>%
    select(
      year,
      industry_raw, industry_code, industry_level,
      births,
      survival_of_1_year, percent_of_1_year,
      survival_of_2_year, percent_of_2_year,
      survival_of_3_year, percent_of_3_year,
      survival_of_4_year, percent_of_4_year,
      survival_of_5_year, percent_of_5_year
    )
}


# Apply cleaning function to all cohort-year tables and combine results
industry_survival_all <- bind_rows(
  lapply(sheets_to_read, clean_sheet_52)
)

# Save cleaned survival data for use in composite visualisations
write_csv(
  industry_survival_all,
  "./Dataset/ONS_Business_Demography/Births_and_Survival_of_Enterprises_2019_2023_by_Industry.csv"
)

View(industry_survival_all)
