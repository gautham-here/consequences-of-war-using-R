# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to create the dual line graph for GDP growth and inflation over time
plot_dual_line_graph <- function(dataB, country, start_year, end_year) {
  
  # Filter data for the given country and time range
  filtered_data <- dataB %>%
    filter(Country == country, Year >= start_year, Year <= end_year)
  
  # Check if there's data for the specified filter
  if (nrow(filtered_data) == 0) {
    stop("No data available for the specified country and time range.")
  }
  
  # Reshape the data to long format for ggplot
  data_long <- filtered_data %>%
    select(Year, `GDP growth (annual %)`, `Inflation, consumer prices (annual %)`) %>%
    pivot_longer(cols = c(`GDP growth (annual %)`, `Inflation, consumer prices (annual %)`),
                 names_to = "Indicator",
                 values_to = "Value")
  
  # Create the dual line graph
  ggplot(data_long, aes(x = Year, y = Value, color = Indicator)) +
    geom_line(size = 1) +
    labs(title = paste("GDP Growth and Inflation Over Time for", country),
         x = "Year",
         y = "Percentage",
         color = "Indicator") +
    theme_minimal() +
    scale_color_manual(values = c("blue", "red"))
}

# Example usage: replace with your desired country and year range
# Assuming 'dataB' is preloaded in your R environment with necessary columns
plot_dual_line_graph(dataB, country = "Ukraine", start_year = 1990, end_year = 2023)
