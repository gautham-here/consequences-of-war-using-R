Install and load necessary packages
install.packages("readxl")
library(readxl)

# Load the data from the specified path
data <- read_excel("imputed_data.xlsx")

# Inspect the data
head(data)
str(data)

# Define the war years
war_years <- c("2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010", "2011")

# Filter data for specific indicators and war timeframes
gdp_growth <- data[data$Indicator == "GDP growth (annual %)", war_years]
inflation <- data[data$Indicator == "Inflation, consumer prices (annual %)", war_years]

# Perform a z-test (assuming large sample size)
z_test_result <- t.test(as.numeric(unlist(gdp_growth)), as.numeric(unlist(inflation)))
print(z_test_result)

# Perform an F-test to compare variances
f_test_result <- var.test(as.numeric(unlist(gdp_growth)), as.numeric(unlist(inflation)))
print(f_test_result)

# Perform a t-test to compare means
t_test_result <- t.test(as.numeric(unlist(gdp_growth)), as.numeric(unlist(inflation)))
print(t_test_result)

# Perform a paired t-test (if the data is paired)
paired_t_test_result <- t.test(as.numeric(unlist(gdp_growth)), as.numeric(unlist(inflation)), paired = TRUE)
print(paired_t_test_result)

# Perform ANOVA to compare means of multiple groups
anova_data <- data.frame(
  gdp_growth = as.numeric(unlist(gdp_growth)),
  inflation = as.numeric(unlist(inflation))
)
anova_result <- aov(gdp_growth ~ inflation, data = anova_data)
summary(anova_result)

# Perform a chi-square test to compare frequencies
chi_square_data <- table(data$Country, data$Indicator)
chi_square_result <- chisq.test(chi_square_data)
print(chi_square_result)