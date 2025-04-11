library(readr)      # For reading CSV files
library(dplyr)      # For data manipulation
library(tidyr)      # For reshaping data
library(stringr)    # For string manipulation

# Read demographic data files
read_demo_file = function(file_name) {
    file_path = here::here("data", "STATS22084", file_name)
    read_csv(file_path, progress = FALSE)
}

# Read all demographic files
age_data = read_demo_file("age-groups-table.csv")
sex_data = read_demo_file("sex-groups-table.csv")
eth_data = read_demo_file("eth-groups-table.csv")
region_data = read_demo_file("gor_name-groups-table.csv")
hhsize_data = read_demo_file("hhsize-groups-table.csv")

# Read ONS population estimates
ons = read_csv(here::here("data", "pop-estimates-2020.csv"), progress = FALSE)

# Create region mapping between SRS codes and ONS names
region_mapping = tibble(
    srs_code = c("1_NE", "2_NW", "3_YH", "4_EM", "5_WM", "6_EE", "7_LD", "8_SE", "9_SW", "10_NI", "11_SCO", "12_WAL"),
    ons_name = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", 
                 "East", "London", "South East", "South West", "Northern Ireland", "Scotland", "Wales")
)

# Map ONS age to SRS age groups
map_age = function(age) {
    age_num = case_match(
        age,
        "Aged 90+" ~ 90,
        .default =  as.numeric(str_replace(age, "Age ", ""))
    )
    
    case_when(
        age_num <= 4 ~ "2-4",
        age_num >= 5 & age_num <= 9 ~ "5-9",
        age_num >= 10 & age_num <= 14 ~ "10-14",
        age_num >= 15 & age_num <= 19 ~ "15-19",
        age_num >= 20 & age_num <= 24 ~ "20-24",
        age_num >= 25 & age_num <= 29 ~ "25-29",
        age_num >= 30 & age_num <= 34 ~ "30-34",
        age_num >= 35 & age_num <= 39 ~ "35-39",
        age_num >= 40 & age_num <= 44 ~ "40-44",
        age_num >= 45 & age_num <= 49 ~ "45-49",
        age_num >= 50 & age_num <= 54 ~ "50-54",
        age_num >= 55 & age_num <= 59 ~ "55-59",
        age_num >= 60 & age_num <= 64 ~ "60-64",
        age_num >= 65 & age_num <= 69 ~ "65-69",
        age_num >= 70 & age_num <= 74 ~ "70-74",
        age_num >= 75 & age_num <= 79 ~ "75-79",
        age_num >= 80 & age_num <= 84 ~ "80-84",
        age_num >= 85 & age_num <= 89 ~ "85-89",
        age_num >= 90 ~ "90+",
        TRUE ~ NA_character_
    )
}

# Process ONS data for age comparison
ons_by_age = ons |>
    filter(Age != "All Ages") |>
    # Add age group column using mapping function
    mutate(age_group = map_age(Age)) |>
    # Reshape data to have one row per region-age combination
    pivot_longer(
        cols = -c(Age, age_group),
        names_to = "region",
        values_to = "population"
    ) |>
    # Sum population by age group across all regions
    group_by(age_group) |>
    summarize(ons_count = sum(population), .groups = "drop") |>
    # Calculate proportion
    mutate(ons_proportion = ons_count / sum(ons_count))

# Process ONS data for region comparison
ons_by_region = ons |>
    # Remove the total row
    filter(Age != "All Ages") |>
    # Reshape data to have one row per region-age combination
    pivot_longer(
        cols = -Age,
        names_to = "region",
        values_to = "population"
    ) |>
    # Sum population by region across all age groups
    group_by(region) |>
    summarize(ons_count = sum(population), .groups = "drop") |>
    # Calculate proportion
    mutate(ons_proportion = ons_count / sum(ons_count))

# Create Table 1

# Age section
age_section = age_data |>
    select(name, frequency, proportion) |>
    rename(age_group = name, srs_count = frequency, srs_proportion = proportion) |>
    left_join(ons_by_age, by = "age_group") |>
    mutate(
        diff_proportion = srs_proportion - ons_proportion,
        category = "Age Group"
    ) |>
    rename(group = age_group)

# Region section
region_section = region_data |>
    select(name, frequency, proportion) |>
    rename(region_code = name, srs_count = frequency, srs_proportion = proportion) |>
    left_join(region_mapping, by = c("region_code" = "srs_code")) |>
    left_join(ons_by_region, by = c("ons_name" = "region")) |>
    mutate(
        diff_proportion = srs_proportion - ons_proportion,
        category = "Region"
    ) |>
    rename(group = ons_name) |>
    select(-region_code)

# Other demographic sections (no ONS comparison)
create_section = function(data, category_name) {
    data |>
        select(name, frequency, proportion) |>
        rename(group = name, srs_count = frequency, srs_proportion = proportion) |>
        mutate(
            ons_count = NA_real_,
            ons_proportion = NA_real_,
            diff_proportion = NA_real_,
            category = category_name
        )
}

sex_section = create_section(sex_data, "Sex")
eth_section = create_section(eth_data, "Ethnicity")
hhsize_section = create_section(hhsize_data, "Household Size")

# Combine all sections
table1 = bind_rows(
    age_section,
    region_section,
    sex_section,
    eth_section,
    hhsize_section
) |>
    select(category, group, srs_count, srs_proportion, ons_count, ons_proportion, diff_proportion)

# Format the table for presentation
table1_formatted = table1 |>
    mutate(
        # Format percentages as strings with % sign
        srs_percent = paste0(format(round(srs_proportion * 100, 1), nsmall = 1), "%"),
        ons_percent = ifelse(is.na(ons_proportion), NA_character_,
                             paste0(format(round(ons_proportion * 100, 1), nsmall = 1), "%")),
        diff_percent = ifelse(is.na(diff_proportion), NA_character_,
                              paste0(format(round(diff_proportion * 100, 1), nsmall = 1), "%"))
    ) |>
    select(
        Category = category,
        Group = group,
        `SRS Count` = srs_count,
        `SRS %` = srs_percent,
        `ONS Count` = ons_count,
        `ONS %` = ons_percent,
        `Difference` = diff_percent
    )

# Print the table
print(table1_formatted, n = Inf)