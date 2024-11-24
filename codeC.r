# Load required library
if (!require(openxlsx)) install.packages("openxlsx", dependencies = TRUE)
library(openxlsx)

# Define country and indicators
country_code <- "UKR"
indicator1_name <- "GDP growth (annual %)"   # Name of the GDP growth indicator
indicator2_name <- "Inflation, consumer prices (annual %)"  # Name of the inflation indicator

# Define years for pre-war and war periods
pre_war_years <- as.character(2004:2013)
war_years <- as.character(2014:2023)

# Filter data for the specified country and indicators
country_data <- dataA[dataA$CountryCode == country_code, ]
indicator_data_1 <- country_data[country_data$Indicator == indicator1_name, pre_war_years]
indicator_data_2 <- country_data[country_data$Indicator == indicator2_name, pre_war_years]

# Convert the data to vectors for pre-war period
pre_war_data_1 <- as.numeric(unlist(indicator_data_1))
pre_war_data_2 <- as.numeric(unlist(indicator_data_2))

indicator_data_1_war <- country_data[country_data$Indicator == indicator1_name, war_years]
indicator_data_2_war <- country_data[country_data$Indicator == indicator2_name, war_years]
war_data_1 <- as.numeric(unlist(indicator_data_1_war))
war_data_2 <- as.numeric(unlist(indicator_data_2_war))

# Ensure vectors are aligned correctly by checking length equality
if (length(pre_war_data_1) == length(pre_war_data_2) && length(war_data_1) == length(war_data_2)) {
  
  # Calculate covariance and pairwise correlation for pre-war period
  pre_war_cov <- cov(pre_war_data_1, pre_war_data_2, use = "pairwise.complete.obs")
  pre_war_cor_pearson <- cor(pre_war_data_1, pre_war_data_2, method = "pearson", use = "pairwise.complete.obs")
  pre_war_cor_spearman <- cor(pre_war_data_1, pre_war_data_2, method = "spearman", use = "pairwise.complete.obs")
  pre_war_cor_kendall <- cor(pre_war_data_1, pre_war_data_2, method = "kendall", use = "pairwise.complete.obs")
  
  # Calculate covariance and pairwise correlation for war period
  war_cov <- cov(war_data_1, war_data_2, use = "pairwise.complete.obs")
  war_cor_pearson <- cor(war_data_1, war_data_2, method = "pearson", use = "pairwise.complete.obs")
  war_cor_spearman <- cor(war_data_1, war_data_2, method = "spearman", use = "pairwise.complete.obs")
  war_cor_kendall <- cor(war_data_1, war_data_2, method = "kendall", use = "pairwise.complete.obs")
  
  results <- data.frame(
    Period = c("Pre-War", "Pre-War", "Pre-War", "Pre-War", "War", "War", "War", "War"),
    Metric = c("Covariance", "Pearson Correlation", "Spearman Correlation", "Kendall Correlation",
               "Covariance", "Pearson Correlation", "Spearman Correlation", "Kendall Correlation"),
    Value = c(pre_war_cov, pre_war_cor_pearson, pre_war_cor_spearman, pre_war_cor_kendall,
              war_cov, war_cor_pearson, war_cor_spearman, war_cor_kendall)
  )
  write.xlsx(results, file = "correlation_results.xlsx", sheetName = "Pairwise Correlations", rowNames = FALSE)
  print("Results saved to 'correlation_results.xlsx'")
} else {
  print("Data vectors do not match in length for pre-war or war period. Please check your data.")
}
