# Load necessary libraries
library(dplyr)
library(tidyr)
library(readxl)    # For reading Excel files
library(writexl)   # For writing Excel files

# Load the data from Excel
data <- read_excel("dataB.xlsx")  # Replace "dataB.xlsx" with your Excel file name

# Specify the country and years for calculating the change
country_to_rank <- "Afghanistan"  # Replace with desired country
start_year <- 2010  # Replace with the starting year
end_year <- 2021    # Replace with the ending year

# Filter data for the chosen country and years
data_filtered <- data %>%
  filter(Country == country_to_rank, Year %in% c(start_year, end_year)) %>%
  select(-Country, -CountryCode) %>%  # Exclude CountryCode column
  pivot_longer(cols = -Year, names_to = "Indicator", values_to = "Value")

# Separate the values for start and end years and calculate the change
ranked_changes <- data_filtered %>%
  pivot_wider(names_from = Year, values_from = Value, names_prefix = "Year_") %>%
  mutate(Change = abs(Year_2021 - Year_2010)) %>%
  arrange(desc(Change)) %>%
  mutate(Rank = row_number())

# Save the ranked changes as a table in memory
ranked_table <- ranked_changes

# Write the output to an Excel file
write_xlsx(ranked_table, "ranked_changes.xlsx")  # Replace with your desired output file name

# Display the table
print(ranked_table)
