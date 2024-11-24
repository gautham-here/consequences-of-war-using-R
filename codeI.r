# Load necessary libraries
library(ggplot2)
library(dplyr)
# Function to create the box plot for unemployment before and during the war
plot_unemployment_box_plot <- function(dataB, country, pre_war_years, war_years) {
  
  # Check if dataB is a data frame
  if (!is.data.frame(dataB)) {
    stop("dataB is not a data frame!")
  }
  
  # Filter data for the specified country and time range
  filtered_data <- dataB %>%
    filter(Country == country, Year %in% c(pre_war_years, war_years)) %>%
    mutate(Period = ifelse(Year %in% pre_war_years, "Pre-War", "War"))
  
  # Check if there's data for the specified filter
  if (nrow(filtered_data) == 0) {
    stop("No data available for the specified country and periods.")
  }
  
  # Create the box plot for unemployment
  ggplot(filtered_data, aes(x = Period, y = 'Unemployment, total (% of total labor force) (national estimate)', fill = Period)) +
    geom_boxplot() +
    labs(title = paste("Unemployment Before vs During War for", country),
         x = "Period",
         y = "Unemployment (%)") +
    theme_minimal() +
    scale_fill_manual(values = c("Pre-War" = "skyblue", "War" = "salmon"))
}

# Example usage: replace with your desired country and year range
# Example usage for Ukraine
plot_unemployment_box_plot(dataB, country = "Ukraine", pre_war_years = 1990:2021, war_years = 2022:2023)
