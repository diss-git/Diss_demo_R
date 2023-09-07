# Load the required libraries and dependencies
source("dependencies.R")

# Fetch data from CANSIM API
data_in <-
    
    get_cansim_vector(
        c(
            "v1",
            "v2",
            "v8",
            "v9",
            "v10",
            "v11",
            "v12",
            "v13",
            "v14",
            "v15",
            "v3",
            "v4",
            "v6",
            "v7"
        ), 
        start_time =  "2011-01-01",
        refresh = TRUE
    )

# Columns to remove from the data_in dataset
columns_to_remove_data_in <- c("DECIMALS", "val_norm", "releaseTime", "COORDINATE", "SCALAR_ID", "frequencyCode", "SYMBOL", "Date")
data_in <- select(data_in, -one_of(columns_to_remove_data_in))

# Fetch labels from CANSIM API (The above data call does not include labels)
labels <- 
    
    get_cansim_vector_info(
        c(
            "v1",
            "v2",
            "v8",
            "v9",
            "v10",
            "v11",
            "v12",
            "v13",
            "v14",
            "v15",
            "v3",
            "v4",
            "v6",
            "v7"
        ))

# Columns to remove from the labels dataset using dplyr library(select)
columns_to_remove_labels <- c("DECIMALS", "table", "COORDINATE", "title_fr", "frequencyCode","UOM", "SCALAR_ID", "title")
labels <- select(labels, -one_of(columns_to_remove_labels))

# Merge data_in and labels datasets based on the common "VECTOR" column
data <- merge(data_in, labels, by = "VECTOR", all = TRUE)

# Cleaning and removing columns or keep the ones required
cleaned_data <- data %>%
    select(REF_DATE, title_en, VALUE) %>%
    filter(!is.na(title_en))

# Find the minimum date for each group (GEO) for calculating growth rate
min_dates <- cleaned_data %>%
    group_by(title_en) %>%
    summarize(min_date = min(REF_DATE))

# Merge the minimum dates back into the original dataset for calculating growth rate
cleaned_data <- cleaned_data %>%
    left_join(min_dates, by = "title_en")

# Filter the dataset to get the rows matching the minimum date within each group for calculating growth rate
min_date_values <- cleaned_data %>%
    filter(REF_DATE == min_date) %>%
    select(title_en, min_date, min_value = VALUE) %>%
    distinct()

# Merge the minimum date values back into the original dataset for calculating growth rate
cleaned_data <- cleaned_data %>%
    left_join(min_date_values, by = c("title_en", "min_date"))

# Calculate the growth percentage for each value relative to the minimum value within the same minimum date and group
cleaned_data <- cleaned_data %>%
    mutate(GrowthRate = (VALUE / min_value) * 100)

# Define a mapping of month numbers to quarter labels
quarter_mapping <- c("Q1", "Q1", "Q1", "Q2", "Q2", "Q2", "Q3", "Q3", "Q3", "Q4", "Q4", "Q4")

# Create a new column 'Quarter' with year and quarter label
cleaned_data <- cleaned_data %>%
    mutate(Year = substr(REF_DATE, 1, 4),  # Extract the year
           Quarter = paste(Year, quarter_mapping[as.integer(substr(REF_DATE, 6, 7))], sep = "-"))

# Cleaning and removing columns or keep the ones required
cleaned_data <- cleaned_data %>%
    select(Quarter, title_en, VALUE, GrowthRate)

# Rename the columns for readability
cleaned_data <- cleaned_data %>%
    rename("Reference period" = Quarter, Geography = title_en, Population = VALUE, "Growth rate" = GrowthRate)

# Define the custom order from east to west for province
custom_order <- c(
    "Canada",
    "Newfoundland and Labrador",
    "Prince Edward Island",
    "Nova Scotia",
    "New Brunswick",
    "Quebec",
    "Ontario",
    "Manitoba",
    "Saskatchewan",
    "Alberta",
    "British Columbia",
    "Yukon",
    "Northwest Territories",
    "Nunavut"
)

# Sort Geography by matching values in custom_order
cleaned_data <- cleaned_data %>%
    arrange(match(Geography, custom_order))

