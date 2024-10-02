# Reproducible Research Fundamentals 
# 02. Data construction
# RRF - 2024 - Construction

# Preliminary - Load Data ----
# Load household-level data (HH)
hh_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH.dta"))

# Load HH-member data
mem_data <- read_dta(file.path(data_path, "Intermediate/TZA_CCT_HH_mem.dta"))

# Load secondary data
secondary_data <- read_dta(file.path(data_path, "Intermediate/TZA_amenity_tidy.dta"))

# Exercise 1: Plan construction outputs ----
# Plan the following outputs:
# 1. Area in acres.
# 2. Household consumption (food and nonfood) in USD.
# 3. Any HH member sick.
# 4. Any HH member can read or write.
# 5. Average sick days.
# 6. Total treatment cost in USD.
# 7. Total medical facilities.



# Exercise 2: Standardize conversion values ----
# Define standardized conversion values:
# 1. Conversion factor for acres.
# 2. USD conversion factor.

# Data construction: Household (HH) ----
# Instructions:
# 1. Convert farming area to acres where necessary.
# 2. Convert household consumption for food and nonfood into USD.


hec_to_acr <- 2.47
tzs_to_usd <- 0.00037

hh_data <- hh_data %>% 
    mutate(area_acre = case_when(ar_unit == 2 ~ ar_farm, ar_unit == 3 ~ ar_farm * hec_to_acr )) %>%
    mutate(area_acre = replace_na(area_acre, 0)) %>%
    mutate(across(c(food_cons, nonfood_cons), ~ .x*tzs_to_usd, .names = "{.col}_usd"))


# Exercise 3: Handle outliers ----
# you can use custom Winsorization function to handle outliers.
winsor_function <- function(dataset, var, min = 0.00, max = 0.95) {
    var_sym <- sym(var)
    
    percentiles <- quantile(
        dataset %>% pull(!!var_sym), probs = c(min, max), na.rm = TRUE
    )
    
    min_percentile <- percentiles[1]
    max_percentile <- percentiles[2]
    
    dataset %>%
        mutate(
            !!paste0(var, "_w") := case_when(
                is.na(!!var_sym) ~ NA_real_,
                !!var_sym <= min_percentile ~ percentiles[1],
                !!var_sym >= max_percentile ~ percentiles[2],
                TRUE ~ !!var_sym
            )
        )
}

# Tips: Apply the Winsorization function to the relevant variables.
# Create a list of variables that require Winsorization and apply the function to each.

win_vars <- c("area_acre", "food_cons_usd", "nonfood_cons_usd")

for (var in win_vars) { hh_data <- winsor_function(hh_data, var)}

hh_data <- hh_data %>% mutate(across(ends_with("_w"), ~ labelled(.x, label = paste0(attr(.x, "label"), "(winsorized 0.05)"))))

# Exercise 4.1: Create indicators at household level ----
# Instructions:
# Collapse HH-member level data to HH level.
# Plan to create the following indicators:
# 1. Any member was sick.
# 2. Any member can read/write.
# 3. Average sick days.
# 4. Total treatment cost in USD.

hh_mem_collapsed <- mem_data  %>%
    group_by(hhid) %>%
    summarize(any_sick = any(sick == 1, na.rm = TRUE), 
              any_read = any(read == 1, na.rm = TRUE),
              avg_sick_days = mean(days_sick, na.rm = TRUE),
              total_treatment_cost_usd = sum(treat_cost, na.rm = TRUE)) %>%
    mutate_all(~ifelse(is.nan(.), NA, .)) %>%
    set_variable_labels(any_sick = "Any member was sick", 
                        any_read = "Any member can read/write",
                        avg_sick_days = "Average sick days",
                        total_treatment_cost_usd = "Total treatment cost in USD")




# Exercise 4.2: Data construction: Secondary data ----
# Instructions:
# Calculate the total number of medical facilities by summing relevant columns.
# Apply appropriate labels to the new variables created.

secondary_data <- secondary_data %>%
    mutate(n_medical = rowSums(across(c(n_school, n_clinic)), na.rm = TRUE))

# Exercise 5: Merge HH and HH-member data ----
# Instructions:
# Merge the household-level data with the HH-member level indicators.
# After merging, ensure the treatment status is included in the final dataset.

hh_data_merged <- hh_data %>%
    left_join(hh_mem_collapsed, by = "hhid")

treat_status <- read_dta(file.path(data_path, "Raw/treat_status.dta"))

hh_data_merged <- hh_data_merged %>%
    left_join(treat_status, by = "vid")

# Exercise 6: Save final dataset ----
# Instructions:
# Only keep the variables you will use for analysis.
# Save the final dataset for further analysis.
# Save both the HH dataset and the secondary data.

write_dta(hh_data_merged, file.path(data_path, "Final/TZA_CCT_analysis.dta"))
write_dta(secondary_data, file.path(data_path, "Final/TZA_amenity_analysis.dta"))

# Tip: Ensure all variables are correctly labeled 

