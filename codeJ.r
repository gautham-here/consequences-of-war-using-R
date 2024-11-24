# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to create the line graph for FDI and government debt comparison
plot_fiscal_strain_line_graph <- function(dataB, country, start_year, end_year) {
  
  # Filter data for the specified country and time range
  filtered_data <- dataB %>%
    filter(Country == country, Year >= start_year, Year <= end_year)
  
  # Check if there's data for the specified filter
  if(nrow(filtered_data) == 0) {
    stop("No data available for the specified country and time range.")
  }
  
  # Create the line graph comparing FDI and government debt
  ggplot(filtered_data, aes(x = Year)) +
    geom_line(aes(y = `Foreign direct investment, net (BoP, current US$)`, color = "FDI")) +
    geom_line(aes(y = `Central government debt, total (% of GDP)`, color = "Government Debt")) +
    labs(title = paste("FDI and Government Debt Trends for", country, "from", start_year, "to", end_year),
         x = "Year",
         y = "Value",
         color = "Indicator") +
    scale_color_manual(values = c("FDI" = "blue", "Government Debt" = "red")) +
    theme_minimal() +
    theme(legend.position = "top")
}

# Example usage: replace with your desired country and year range
# Example usage for Ukraine, from 1990 to 2023
plot_fiscal_strain_line_graph(dataB, country = "Ukraine", start_year = 1990, end_year = 2023)
