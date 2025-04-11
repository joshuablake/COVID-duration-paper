library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(here)

# Function to read demographic files
read_demo_file = function(file_name) {
    file_path = here::here("data", "STATS22084", file_name)
    read_csv(file_path, progress = FALSE, show_col_types = FALSE)
}

# Reading demographic files
age_groups = read_demo_file("age-groups-table.csv")
sex_groups = read_demo_file("sex-groups-table.csv")
eth_groups = read_demo_file("eth-groups-table.csv")
region_groups = read_demo_file("gor_name-groups-table.csv")
hhsize_groups = read_demo_file("hhsize-groups-table.csv")

# Reading ONS data
ons = read_csv(here::here("data", "pop-estimates-2020.csv"), 
    progress = FALSE, show_col_types = FALSE)

# Reading census ethnicity data
census_ethnicity = read_csv(here::here("data", "by-ethnicity-5-groups-table.csv"), 
    progress = FALSE, show_col_types = FALSE)

# Function to extract the lower bound of an age range for sorting
extract_lower_age = function(age_str) {
    if (age_str == "2-4") return(0)  # Place "Under 4" first
    if (age_str == "90+") return(100)  # Place "Over 90" last
    
    # Extract the first number from strings like "45-49"
    as.numeric(str_extract(age_str, "^\\d+"))
}

# Map region codes to full names and determine if they're in England
region_info = tibble(
    name = c("1_NE", "2_NW", "3_YH", "4_EM", "5_WM", "6_EE", "7_LD", "8_SE", "9_SW", "10_NI", "11_SCO", "12_WAL"),
    full_name = c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East", "London", "South East", "South West", "Northern Ireland", "Scotland", "Wales"),
    is_england = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE)
)

# Process ONS data for age groups percentages
process_ons_age_data = function(ons_data) {
    # Sum all regions to get UK total by age
    ons_by_age = ons_data |>
        filter(Age != "All Ages") |>
        mutate(across(-Age, as.numeric)) |>
        mutate(UK_Total = rowSums(across(-Age))) |>
        select(Age, UK_Total)
    
    # Calculate total UK population (excluding "All Ages" row)
    uk_total_pop = sum(ons_by_age$UK_Total)
    
    # Calculate percentages
    ons_age_percent = ons_by_age |>
        mutate(Proportion = UK_Total / uk_total_pop * 100)
    
    # Prepare age groupings matching our data
    # First extract numeric values from age strings
    ons_age_percent = ons_age_percent |>
        mutate(
            Age_Num = as.numeric(str_extract(Age, "\\d+")),
            Age_Group = case_when(
                Age_Num < 5 ~ "Under 4",
                Age_Num >= 5 & Age_Num < 10 ~ "5-9",
                Age_Num >= 10 & Age_Num < 15 ~ "10-14",
                Age_Num >= 15 & Age_Num < 20 ~ "15-19",
                Age_Num >= 20 & Age_Num < 25 ~ "20-24",
                Age_Num >= 25 & Age_Num < 30 ~ "25-29", 
                Age_Num >= 30 & Age_Num < 35 ~ "30-34",
                Age_Num >= 35 & Age_Num < 40 ~ "35-39",
                Age_Num >= 40 & Age_Num < 45 ~ "40-44",
                Age_Num >= 45 & Age_Num < 50 ~ "45-49",
                Age_Num >= 50 & Age_Num < 55 ~ "50-54",
                Age_Num >= 55 & Age_Num < 60 ~ "55-59",
                Age_Num >= 60 & Age_Num < 65 ~ "60-64",
                Age_Num >= 65 & Age_Num < 70 ~ "65-69",
                Age_Num >= 70 & Age_Num < 75 ~ "70-74",
                Age_Num >= 75 & Age_Num < 80 ~ "75-79",
                Age_Num >= 80 & Age_Num < 85 ~ "80-84",
                Age_Num >= 85 & Age_Num < 90 ~ "85-89",
                Age_Num >= 90 ~ "Over 90",
                TRUE ~ "Unknown"
            )
        )
    
    # Group by the defined age groups and sum
    ons_age_grouped = ons_age_percent |>
        group_by(Age_Group) |>
        summarize(Total = sum(UK_Total)) |>
        mutate(Percent = Total / uk_total_pop * 100) |>
        select(Age_Group, Percent)
    
    return(ons_age_grouped)
}

# Process ONS data for regions
process_ons_region_data = function(ons_data) {
    # Get total population by region (using "All Ages" row)
    ons_regions = ons_data |>
        filter(Age == "All Ages") |>
        pivot_longer(cols = -Age, 
                     names_to = "Region", 
                     values_to = "Population")
    
    # Calculate total UK population
    uk_total = sum(ons_regions$Population)
    
    # Calculate percentages
    ons_region_percent = ons_regions |>
        mutate(Percent = Population / uk_total * 100) |>
        select(Region, Percent)
    
    return(ons_region_percent)
}

# Process regions and countries
process_regions_and_countries = function(region_groups, region_info, ons_region_percentages) {
    # Process individual regions
    region_table = region_groups |>
        left_join(region_info, by = "name") |>
        mutate(
            category = "Region",
            Variable = full_name,
            n = frequency,
            Percent = proportion * 100
        ) |>
        left_join(ons_region_percentages, by = c("Variable" = "Region")) |>
        rename(`ONS Percent` = Percent.y, Percent = Percent.x) |>
        select(category, Variable, n, Percent, `ONS Percent`, is_england)
    
    # Create countries table (England, Scotland, Wales, Northern Ireland)
    # First, get the aggregate for England
    england_total = region_table |>
        filter(is_england == TRUE) |>
        summarize(
            category = "Country",
            Variable = "England",
            n = sum(n),
            Percent = sum(n) / sum(region_groups$frequency) * 100,
            # Calculate England ONS percent by summing respective regions
            `ONS Percent` = sum(`ONS Percent`, na.rm = TRUE)
        )
    
    # Get the other countries
    other_countries = region_table |>
        filter(is_england == FALSE) |>
        mutate(category = "Country") |>
        select(category, Variable, n, Percent, `ONS Percent`)
    
    # Combine England with other countries
    countries_table = bind_rows(england_total, other_countries)
    
    return(list(regions = region_table, countries = countries_table))
}

# Get ONS percentages
ons_age_percentages = process_ons_age_data(ons)
ons_region_percentages = process_ons_region_data(ons)

# Process age groups
age_table = age_groups |>
    mutate(
        category = "Age group",
        sort_order = sapply(name, extract_lower_age),
        Variable = ifelse(name == "2-4", "Under 4", 
                  ifelse(name == "90+", "Over 90", name)),
        n = frequency,
        Percent = proportion * 100
    ) |>
    left_join(ons_age_percentages, by = c("Variable" = "Age_Group")) |>
    rename(`ONS Percent` = Percent.y, Percent = Percent.x) |>
    arrange(sort_order) |>
    select(category, Variable, n, Percent, `ONS Percent`)

# Process sex groups
sex_table = sex_groups |>
    mutate(
        category = "Sex",
        Variable = name,
        n = frequency,
        Percent = proportion * 100,
        `ONS Percent` = ifelse(name == "Male", 49, 51)
    ) |>
    select(category, Variable, n, Percent, `ONS Percent`)

# Process ethnicity groups
eth_table = eth_groups |>
    mutate(
        category = "Ethnicity",
        Variable = name,
        n = frequency,
        Percent = proportion * 100
    ) |>
    # Join with census ethnicity data
    left_join(
        census_ethnicity |> 
        rename(`ONS Percent` = `%`),
        by = c("Variable" = "Ethnicity")
    ) |>
    select(category, Variable, n, Percent, `ONS Percent`)

# Process region groups and countries
region_data = process_regions_and_countries(region_groups, region_info, ons_region_percentages)
region_table = region_data$regions
countries_table = region_data$countries

# Process household size groups
hhsize_table = hhsize_groups |>
    mutate(
        category = "Household size",
        Variable = name,
        n = frequency,
        Percent = proportion * 100,
        `ONS Percent` = NA
    ) |>
    select(category, Variable, n, Percent, `ONS Percent`)

# Combine demographic tables (except regions and countries, which are handled specially)
demographic_tables = bind_rows(
    age_table,
    sex_table,
    eth_table,
    hhsize_table
)

# Output basic LaTeX table
write_table1_latex = function(demographic_data, region_data, countries_data, file_name) {
    # Start the LaTeX table
    latex_text = "% Table generated by R\n"
    latex_text = paste0(latex_text, "\\begin{tabular}{lrrr}\n")
    latex_text = paste0(latex_text, "\\hline\n")
    latex_text = paste0(latex_text, "& Cohort count & Cohort \\% & Population \\% \\\\\n")
    latex_text = paste0(latex_text, "\\hline\n")
    
    # Process standard demographic categories
    categories = unique(demographic_data$category)
    for (cat in categories) {
        # Add category header
        latex_text = paste0(latex_text, "\\multicolumn{4}{l}{\\textbf{", cat, "}} \\\\\n")
        
        # Standard category handling
        cat_rows = demographic_data |> filter(category == cat)
        for (i in 1:nrow(cat_rows)) {
            row = cat_rows[i, ]
            
            # Add the row data
            latex_text = paste0(latex_text, row$Variable, " & ", 
                               row$n, " & ", 
                               sprintf("%.1f\\%%", row$Percent), " & ", 
                               ifelse(is.na(row$`ONS Percent`), "", 
                                      sprintf("%.1f\\%%", row$`ONS Percent`)), " \\\\\n")
        }
        
        # Add a small gap after each category
        latex_text = paste0(latex_text, "\\hline\n")
    }
    
    # Add Countries section
    latex_text = paste0(latex_text, "\\multicolumn{4}{l}{\\textbf{Country}} \\\\\n")
    
    # Process countries
    for (i in 1:nrow(countries_data)) {
        row = countries_data[i, ]
        
        # Add the row data
        latex_text = paste0(latex_text, row$Variable, " & ", 
                           row$n, " & ", 
                           sprintf("%.1f\\%%", row$Percent), " & ", 
                           ifelse(is.na(row$`ONS Percent`), "", 
                                  sprintf("%.1f\\%%", row$`ONS Percent`)), " \\\\\n")
    }
    
    # Add a small gap after countries
    latex_text = paste0(latex_text, "\\hline\n")
    
    # Process regions
    latex_text = paste0(latex_text, "\\multicolumn{4}{l}{\\textbf{Region}} \\\\\n")
    
    # English regions only
    eng_regions = region_data |> filter(is_england == TRUE)
    for (i in 1:nrow(eng_regions)) {
        row = eng_regions[i, ]
        
        # Add the row data
        latex_text = paste0(latex_text, row$Variable, " & ", 
                           row$n, " & ", 
                           sprintf("%.1f\\%%", row$Percent), " & ", 
                           ifelse(is.na(row$`ONS Percent`), "", 
                                  sprintf("%.1f\\%%", row$`ONS Percent`)), " \\\\\n")
    }
    
    # End the LaTeX table
    latex_text = paste0(latex_text, "\\hline\n")
    latex_text = paste0(latex_text, "\\end{tabular}\n")
    
    # Write to file
    writeLines(latex_text, file_name)
}

# Use the function to create the LaTeX table
write_table1_latex(demographic_tables, region_table, countries_table,
    here::here("figures/output/table1.tex"))