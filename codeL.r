# Function to create the line graph for Trade as a percentage of GDP over time
plot_trade_percentage_of_gdp <- function(dataB, country, start_year, end_year) {
  
  # Filter data for the specified country and time range
  filtered_data <- dataB %>%
    filter(Country == country, Year >= start_year, Year <= end_year)
  
  # Check if there's data for the specified filter
  if(nrow(filtered_data) == 0) {
    stop("No data available for the specified country and time range.")
  }
  
  # Create the line graph for Trade (% of GDP)
  ggplot(filtered_data, aes(x = Year, y = `Trade (% of GDP)`)) +
    geom_line(color = "purple") +
    labs(title = paste("Trade as % of GDP for", country, "from", start_year, "to", end_year),
         x = "Year",
         y = "Trade (% of GDP)") +
    theme_minimal()
}
# Example usage: replace with your desired country and year range
# Example usage for Ukraine from 1990 to 2023
plot_trade_percentage_of_gdp(dataB, country = "Ukraine", start_year = 1990, end_year = 2023)
