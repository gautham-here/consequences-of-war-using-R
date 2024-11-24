# VARIANCE CALCULATION

country_codes <- c("UKR", "RUS") 
year_ranges <- list(
  "1993-2013" = 1993:2013,
  "2013-2023" = 2013:2023
)

variance_list <- list()


for (country in country_codes) {
  for (year_range_name in names(year_ranges)) {
    years <- year_ranges[[year_range_name]]
    country_data <- imputedA[imputedA$CountryCode == country, ]
    
    for (i in 1:nrow(country_data)) {
      param_name <- country_data$Indicator[i]
      data_values <- as.numeric(country_data[i, as.character(years)])
      variance_value <- var(data_values, na.rm = TRUE)
      column_name <- paste(country, "(", year_range_name, ")", sep = "")

      if (!param_name %in% names(variance_list)) {
        variance_list[[param_name]] <- data.frame(Indicator = param_name)
      }
      variance_list[[param_name]][[column_name]] <- variance_value
    }
  }
}

variance_table <- do.call(rbind, variance_list)
variance_table <- as.data.frame(variance_table, stringsAsFactors = FALSE)

if (!require("writexl")) install.packages("writexl")
library(writexl)

write_xlsx(variance_table, "Country_Variances_by_Year.xlsx")
