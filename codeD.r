# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to create the line graph for any two indicators across time
plot_indicators_line_graph <- function(dataB, country, start_year, end_year, indicator1, indicator2) {
  
  # Filter data for the specified country and time range
  filtered_data <- dataB %>%
    filter(Country == country, Year >= start_year, Year <= end_year)
  
  # Check if there's data for the specified filter
  if(nrow(filtered_data) == 0) {
    stop("No data available for the specified country and time range.")
  }
  
  # Create the line graph
  ggplot(filtered_data, aes(x = Year)) +
    geom_line(aes(y = .data[[indicator1]], color = indicator1)) +
    geom_line(aes(y = .data[[indicator2]], color = indicator2)) +
    labs(title = paste(indicator1, "and", indicator2, "in", country, "from", start_year, "to", end_year),
         x = "Year",
         y = "Value",
         color = "Indicator") +
    theme_minimal()
}

# Function to create a bar chart for any single indicator (pre-war vs war years)
plot_single_indicator_bar_chart <- function(dataB, country, pre_war_years, war_years, indicator) {
  
  # Filter data for the specified country and pre-war and war periods
  filtered_data <- dataB %>%
    filter(Country == country, Year %in% c(pre_war_years, war_years)) %>%
    mutate(Period = ifelse(Year %in% pre_war_years, "Pre-War", "War"))
  
  # Check if there's data for the specified filter
  if(nrow(filtered_data) == 0) {
    stop("No data available for the specified country and periods.")
  }
  
  # Create the bar chart
  ggplot(filtered_data, aes(x = Period, y = .data[[indicator]], fill = Period)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste(indicator, "in Pre-War vs. War Years for", country),
         x = "Period",
         y = paste(indicator, "(%)")) +
    theme_minimal() +
    scale_fill_manual(values = c("Pre-War" = "skyblue", "War" = "salmon"))
}

# Example usage: replace with your desired country, year range, and indicators
# Assuming 'dataB' is preloaded in your R environment with necessary columns
plot_indicators_line_graph(dataB, "Ukraine", 1990, 2023, "GDP per capita (current US$)", "Unemployment, total (% of total labor force) (national estimate)")
#plot_single_indicator_bar_chart(dataB, "Ukraine", pre_war_years = 1990:2021, war_years = 2022:2023, "Age_dependency_ratio")
