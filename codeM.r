data=imputedA
countr <- split(data,data$CountryCode)
rus=countr$"UKR"
# Assuming 'rus' contains the data for Ukraine
# Subset the data for the years 2004 to 2013
x<- rus[, c("CountryCode", "Indicator", as.character(1960:2023))]
x=subset(x,Indicator=="GDP (current US$)")
y<-rus[, c("CountryCode", "Indicator", as.character(1960:2023))]
y=subset(y,Indicator=="Inflation, consumer prices (annual %)")
# Assuming 'x' and 'y' contain only the rows for the specified IndicatorCode and years
# Load necessary library for interpolation if needed
# install.packages("zoo")
library(zoo)
# Extract the numeric values for each year range
x_values <- as.numeric(unlist(x[, as.character(1960:2023)]))
y_values <- as.numeric(unlist(y[, as.character(1960:2023)]))
# Check lengths and adjust if necessary
if (length(x_values) != length(y_values)) {
   min_length <- min(length(x_values), length(y_values))
   x_values <- x_values[1:min_length]
   y_values <- y_values[1:min_length]
} 
# Run the regression if lengths are equal
if (length(x_values) == length(y_values)) {
   reg <- lm(y_values ~ x_values)
   summary(reg)   
   # Plot with regression line
   plot(x_values, y_values, 
        main = "Relationship between Pre-War and War Period Indicators",
        xlab = "GDP (current US$) (1960-2023)", 
        ylab = "Inflation, consumer prices (annual %) (1960-2023)",
        pch = 16, col = "blue")
abline(reg, col = "red", lwd = 2)  
# Optionally, add text to show the equation on the plot
eq <- paste("y =", round(coef(reg)[2], 2), "* x +", round(coef(reg)[1]    ,2))
legend("topleft", legend = eq, col = "red", bty = "n")
} else {
   cat("Error: x_values and y_values still differ in length.")
}
lm(x_values~y_values,data=rus)
