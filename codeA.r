# IMPUTATION OF ORIGINAL DATA

library(readxl)
dataA <- read_excel("dataA.xlsx")
View(init)

library(dplyr)

data_imputed <- dataA %>% mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))
summary(data_imputed)

install.packages("writexl")
library(writexl)


write_xlsx(data_imputed, "imputedA.xlsx")

data_imputed
