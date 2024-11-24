# Function to create a pie chart for rural vs. urban population growth
plot_pie_chart <- function(dataB, country, pre_war_years, war_years) {
  
  # Filter data for the specified country and periods
  filtered_data <- dataB %>%
    filter(Country == country, Year %in% c(pre_war_years, war_years)) %>%
    mutate(Period = ifelse(Year %in% pre_war_years, "Pre-War", "War"))
  
  # Check if there's data for the specified filter
  if (nrow(filtered_data) == 0) {
    stop("No data available for the specified country and periods.")
  }
  
  # Summarize the data for the pie chart (total urban vs rural growth)
  pie_data <- filtered_data %>%
    group_by(Period) %>%
    summarise(Total_urban_growth = sum(`Urban population growth (annual %)`, na.rm = TRUE),
              Total_rural_growth = sum(`Population growth (annual %)`, na.rm = TRUE) - Total_urban_growth) %>%
    pivot_longer(cols = c(Total_urban_growth, Total_rural_growth),
                 names_to = "Indicator",
                 values_to = "Value")
  
  # Create the pie chart
  ggplot(pie_data, aes(x = "", y = Value, fill = Indicator)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    labs(title = paste("Urban vs Rural Population Growth for", country),
         fill = "Growth Type") +
    theme_void() +
    scale_fill_manual(values = c("skyblue", "salmon"))
}

# Example usage: replace with your desired country, year ranges
# Assuming 'dataB' is preloaded in your R environment with necessary columns
plot_pie_chart(dataB, country = "Ukraine", pre_war_years = 1990:2010, war_years = 2011:2023)
