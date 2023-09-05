# Load the required libraries and dependencies
source("dependencies.R")

# Load your data
url <- str_glue("https://www150.statcan.gc.ca/t1/tbl1/en/",
                "dtl!downloadDbLoadingData.action?pid=1710000901&",
                "latestN=&startDate=2011-01&endDate=&csvLocale=en&",
                "selectedMembers=",
                URLencode("[[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]]",
                          reserved = TRUE))

# Read the CSV data from the specified URL
data <- read_csv(url)  

# Cleaning and removing columns or keep the ones required
cleaned_data <- data %>%
    select(REF_DATE, GEO, VALUE) %>%
    filter(!is.na(GEO))

# Find the minimum date for each group (GEO) for calculating growth rate
min_dates <- cleaned_data %>%
    group_by(GEO) %>%
    summarize(min_date = min(REF_DATE))

# Merge the minimum dates back into the original dataset for calculating growth rate
cleaned_data <- cleaned_data %>%
    left_join(min_dates, by = "GEO")

# Filter the dataset to get the rows matching the minimum date within each group for calculating growth rate
min_date_values <- cleaned_data %>%
    filter(REF_DATE == min_date) %>%
    select(GEO, min_date, min_value = VALUE) %>%
    distinct()

# Merge the minimum date values back into the original dataset for calculating growth rate
cleaned_data <- cleaned_data %>%
    left_join(min_date_values, by = c("GEO", "min_date"))

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
    select(Quarter, GEO, VALUE, GrowthRate)

# Rename the columns for readability
cleaned_data <- cleaned_data %>%
    rename("Reference period" = Quarter, Geography = GEO, Population = VALUE, "Growth rate" = GrowthRate)

