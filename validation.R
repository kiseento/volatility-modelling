# Load required libraries
library(Metrics)     # For RMSE and MAPE calculations
library(Directional) # For Directional Accuracy (DA)
library(zoo)
# Actual exchange rates
actual_rates <- c(129.2927, 129.3060, 129.2894, 129.3480, 129.3805, 
                  129.4176, 129.4688, 129.4860, 129.4843, 129.5063, 129.4729)

# Compute actual log returns
actual_returns <- diff(log(actual_rates))

# Compute actual volatility using rolling standard deviation (window = 3)
actual_volatility <- rollapply(actual_returns, width = 3, FUN = sd, align = "right", fill = NA)

# Forecasted volatilities
models <- list(
  "sGARCH" = c(0.000123, 0.000128, 0.000127, 0.000128, 0.000128, 0.000128, 0.000129, 0.000129, 0.000129, 0.000130),
  "pGARCH" = c(0.000119, 0.000125, 0.000138, 0.000150, 0.000162, 0.000174, 0.000186, 0.000197, 0.000209, 0.000221),
  "eGARCH" = c(0.000090, 0.000080, 0.000080, 0.000080, 0.000081, 0.000081, 0.000081, 0.000082, 0.000082, 0.000082),
  "tGARCH" = c(0.000133, 0.000152, 0.000170, 0.000189, 0.000207, 0.000225, 0.000244, 0.000262, 0.000281, 0.000299),
  "DCC-MGARCH Volatility_1" = c(0.000142, 0.000144, 0.000145, 0.000147, 0.000148, 0.000150, 0.000151, 0.000152, 0.000154, 0.000155),
  "DCC-MGARCH Volatility_2" = c(0.000479, 0.000471, 0.000462, 0.000454, 0.000446, 0.000438, 0.000430, 0.000423, 0.000415, 0.000408)
)

# Store results
results <- data.frame(Model = character(), RMSE = numeric(), MAPE = numeric(), DA = numeric(), stringsAsFactors = FALSE)

for (model in names(models)) {
  forecasted_volatility <- models[[model]]
  
  # Compute RMSE and MAPE using Metrics package
  rmse_val <- rmse(actual_volatility, forecasted_volatility)
  mape_val <- mape(actual_volatility, forecasted_volatility)
  
  # Compute Directional Accuracy (DA) using Directional package
  actual_change <- diff(actual_volatility) > 0  # True if actual increased
  forecasted_change <- diff(forecasted_volatility) > 0  # True if forecast increased
  correct_predictions <- sum(actual_change == forecasted_change, na.rm = TRUE)
  total_predictions <- length(actual_change)
  da_val <- (correct_predictions / total_predictions) * 100
  
  # Store results
  results <- rbind(results, data.frame(Model = model, RMSE = rmse_val, MAPE = mape_val, DA = da_val))
}

# Sort results by RMSE and print
results <- results[order(results$RMSE), ]
print(results)
