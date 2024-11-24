# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # For pivot_longer

# Function to create a side-by-side bar chart for two indicators and two countries over two periods
plot_comparison_bar_chart <- function(dataB, country1, country2, timeline1, timeline2, indicator1, indicator2) {
  
  # Convert dataB to a data.frame if it is not already
  dataB <- as.data.frame(dataB)
  
  # Filter data for the specified countries and periods
  filtered_data <- dataB %>%
    filter(Country %in% c(country1, country2), Year %in% c(timeline1, timeline2)) %>%
    mutate(Period = ifelse(Year %in% timeline1, "Timeline1", "Timeline2"))
  
  # Check if there's data for the specified filter
  if (nrow(filtered_data) == 0) {
    stop("No data available for the specified countries and periods.")
  }
  
  # Reshape data to long format for easier plotting
  data_long <- filtered_data %>%
    select(Country, Period, !!sym(indicator1), !!sym(indicator2)) %>%
    pivot_longer(cols = c(!!sym(indicator1), !!sym(indicator2)), names_to = "Indicator", values_to = "Value")
  
  # Create the side-by-side bar chart
  ggplot(data_long, aes(x = Period, y = Value, fill = Indicator)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ Country) +
    labs(title = paste(indicator1, "and", indicator2, "Comparison for", country1, "and", country2),
         x = "Period",
         y = "Value",
         fill = "Indicator") +
    theme_minimal() +
    scale_fill_manual(values = c("skyblue", "salmon"))
}

# Example usage: replace with your desired countries, timelines, and indicators
# Assuming 'dataB' is preloaded in your R environment with necessary columns
plot_comparison_bar_chart(dataB, 
                          country1 = "Ukraine", 
                          country2 = "Turkiye", 
                          timeline1 = 1990:2013, 
                          timeline2 = 2014:2023, 
                          indicator1 = "Official exchange rate (LCU per US$, period average)", 
                          indicator2 = "Central government debt, total (% of GDP)")
