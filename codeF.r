# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Function to create a stacked area chart for population growth and refugee population
plot_stacked_area_chart <- function(dataB, country, start_year, end_year) {
  
  # Filter data for the given country and time range
  filtered_data <- dataB %>%
    filter(Country == country, Year >= start_year, Year <= end_year)
  
  # Check if there's data for the specified filter
  if (nrow(filtered_data) == 0) {
    stop("No data available for the specified country and time range.")
  }
  
  # Reshape the data to long format for ggplot
  data_long <- filtered_data %>%
    select(Year, `Population growth (annual %)`, `Refugee population by country or territory of origin`) %>%
    pivot_longer(cols = c(`Population growth (annual %)`, `Refugee population by country or territory of origin`),
                 names_to = "Indicator",
                 values_to = "Value")
  
  # Create the stacked area chart
  ggplot(data_long, aes(x = Year, y = Value, fill = Indicator)) +
    geom_area() +
    labs(title = paste("Population Growth and Refugee Population Over Time for", country),
         x = "Year",
         y = "Percentage / Population",
         fill = "Indicator") +
    theme_minimal() +
    scale_fill_manual(values = c("skyblue", "salmon"))
}
# Example usage: replace with your desired country and year range
# Assuming 'dataB' is preloaded in your R environment with necessary columns
plot_stacked_area_chart(dataB, country = "Ukraine", start_year = 1990, end_year = 2023)