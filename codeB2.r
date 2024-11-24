# COEFFICIENT OF VARIANCE

country_codes <- c("UKR", "RUS")
year_ranges <- list(
  "1993-2013" = 1993:2013,
  "2013-2023" = 2013:2023
)

cv_list <- list()

for (country in country_codes) {
  for (year_range_name in names(year_ranges)) {
    years <- year_ranges[[year_range_name]]
    
    country_data <- imputedA[imputedA$CountryCode == country, ]
    
    for (i in 1:nrow(country_data)) {
      param_name <- country_data$Indicator[i]
      
      data_values <- as.numeric(country_data[i, as.character(years)])
      
      mean_value <- mean(data_values, na.rm = TRUE)
      variance_value <- var(data_values, na.rm = TRUE)
      
      cv_value <- (sqrt(variance_value) / abs(mean_value)) * 100
      column_name <- paste(country, "(", year_range_name, ")", sep = "")

      if (!param_name %in% names(cv_list)) {
        cv_list[[param_name]] <- data.frame(Indicator = param_name)
      }
      cv_list[[param_name]][[column_name]] <- cv_value
    }
  }
}

cv_table <- do.call(rbind, cv_list)
cv_table <- as.data.frame(cv_table, stringsAsFactors = FALSE)

if (!require("writexl")) install.packages("writexl")
library(writexl)
write_xlsx(cv_table, "Country_CV_by_Year.xlsx")
