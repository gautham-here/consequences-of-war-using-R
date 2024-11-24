# Load necessary libraries
library(ggplot2)
library(dplyr)

# Function to create the clustered bar chart for Imports vs. Exports pre-war and during war
plot_trade_comparison_bar_chart <- function(dataB, country, pre_war_years, war_years) {
  
  # Filter data for the specified country and pre-war and war periods
  filtered_data <- dataB %>%
    filter(Country == country, Year %in% c(pre_war_years, war_years)) %>%
    mutate(Period = ifelse(Year %in% pre_war_years, "Pre-War", "War"))
  
  # Check if there's data for the specified filter
  if(nrow(filtered_data) == 0) {
    stop("No data available for the specified country and periods.")
  }
  
  # Reshape data to long format for easier plotting (Imports vs Exports)
  data_long <- filtered_data %>%
    select(Country, Period, `Imports of goods and services (current US$)`, `Exports of goods and services (current US$)`) %>%
    pivot_longer(cols = c(`Imports of goods and services (current US$)`, `Exports of goods and services (current US$)`), 
                 names_to = "Trade_Type", values_to = "Value")
  
  # Create the clustered bar chart
  ggplot(data_long, aes(x = Period, y = Value, fill = Trade_Type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Imports vs Exports for", country, "Pre-War vs. War Period"),
         x = "Period",
         y = "Trade Value (current US$)",
         fill = "Trade Type") +
    theme_minimal() +
    scale_fill_manual(values = c("Imports of goods and services (current US$)" = "blue", 
                                 "Exports of goods and services (current US$)" = "green"))
}

# Example usage: replace with your desired country, pre-war years, and war years
# Example usage for Ukraine
plot_trade_comparison_bar_chart(dataB, country = "Ukraine", pre_war_years = 1990:2021, war_years = 2022:2023)
