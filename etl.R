# Load the required libraries and dependencies
source("dependencies.R")

# Load your data
url <- str_glue("https://www150.statcan.gc.ca/t1/tbl1/en/",
                "dtl!downloadDbLoadingData.action?pid=1710000901&",
                "latestN=50&startDate=&endDate=&csvLocale=en&",
                "selectedMembers=",
                URLencode("[[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14]]",
                          reserved = TRUE))

# Read the CSV data from the specified URL
data <- read_csv(url)  

# Cleaning and removing columns
cleaned_data <- data %>%
    select(REF_DATE, GEO, VALUE) %>%  # Select specific columns
    filter(!is.na(GEO))  # Remove rows with missing GEO values

# Calculate GrowthRate per 1000
cleaned_data <- cleaned_data %>%
    group_by(GEO) %>%
    mutate(GrowthRate = (VALUE - lag(VALUE))/lag(VALUE) * 1000) %>% 
    ungroup()

# Rename the columns for readability
cleaned_data <- cleaned_data %>%
    rename(Year = REF_DATE, Geography = GEO, Population = VALUE, "Growth Rate" = GrowthRate)

