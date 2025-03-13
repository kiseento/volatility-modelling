# Load necessary libraries
library(rugarch)
library(forecast)
library(tseries)
library(FinTS)  # Load the FinTS package for ArchTest

# Create a directory to save results and plots
if (!dir.exists("results")) {
  dir.create("results")
}

# Function to save text output to a file
save_text_output <- function(text, filename) {
  writeLines(text, file.path("results", filename))
}

# Function to save plots to a file
save_plot <- function(plot_func, filename, width = 800, height = 600) {
  png(file.path("results", filename), width = width, height = height)
  plot_func()
  dev.off()
}

# ✅ Data loading & preprocessing
cat("Loading data...\n")
data <- read.csv("TRADE WEIGHTED AVERAGE INDICATIVE RATES.csv")
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
data <- data[order(data$Date), ]

# Calculate log returns and handle missing values
cat("Calculating log returns...\n")
returns <- na.omit(diff(log(data$EXCHANGE.RATE)))

# Save log returns to a file with improved formatting
formatted_log_returns <- paste(
  "Log Returns:\n",
  "------------\n",
  paste(sprintf("%.6f", returns), collapse = "\n"),
  sep = ""
)
save_text_output(formatted_log_returns, "log_returns.txt")

# Plot log returns
save_plot(function() {
  plot(data$Date[-1], returns, type = "l", main = "Log Returns Over Time", xlab = "Date", ylab = "Log Returns")
}, "log_returns_plot.png")

# Check stationarity of returns
cat("Checking stationarity of returns...\n")
adf_test <- adf.test(returns)
adf_result <- paste("ADF Test p-value:", adf_test$p.value, "\n")
cat(adf_result)
if (adf_test$p.value > 0.05) {
  warning("Returns are not stationary. Consider differencing or transforming the data.")
} else {
  cat("Returns are stationary. Proceeding with ARMA-GARCH modeling.\n")
}

# Save ADF test results
save_text_output(adf_result, "adf_test_results.txt")

# ✅ ARMA order selection, GARCH order selection, and distribution selection
cat("Selecting best ARMA-GARCH model...\n")
auto_garch <- function(returns) {
  best_aic <- Inf
  best_bic <- Inf
  best_loglik <- -Inf
  best_order <- c(0, 0)
  best_garch_order <- c(1, 1)
  best_dist <- "norm"
  
  # Loop over possible ARMA orders
  for (p in 0:2) {
    for (q in 0:2) {
      # Loop over possible GARCH orders
      for (garch_p in 1:2) {
        for (garch_q in 1:2) {
          # Loop over possible distributions
          for (dist in c("norm", "std", "ged")) {
            spec <- ugarchspec(
              variance.model = list(model = "sGARCH", garchOrder = c(garch_p, garch_q)),
              mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
              distribution.model = dist
            )
            fit <- tryCatch({
              ugarchfit(spec, returns)
            }, error = function(e) NULL)
            
            # Check if the model converged
            if (!is.null(fit) && fit@fit$convergence == 0) {
              # Extract AIC, BIC, and log-likelihood
              current_aic <- infocriteria(fit)[1]  # AIC
              current_bic <- infocriteria(fit)[2]  # BIC
              current_loglik <- fit@fit$LLH        # Log-likelihood
              
              # Update best model based on AIC (prioritized)
              if (current_aic < best_aic) {
                best_aic <- current_aic
                best_bic <- current_bic
                best_loglik <- current_loglik
                best_order <- c(p, q)
                best_garch_order <- c(garch_p, garch_q)
                best_dist <- dist
              }
            }
          }
        }
      }
    }
  }
  
  return(list(
    arma_order = best_order,
    garch_order = best_garch_order,
    dist = best_dist,
    aic = best_aic,
    bic = best_bic,
    loglik = best_loglik
  ))
}

# Automatically select the best model
best_model <- auto_garch(returns)

# Save best model details
best_model_details <- paste(
  "Best ARMA Order: (", paste(best_model$arma_order, collapse = ", "), ")\n",
  "Best GARCH Order: (", paste(best_model$garch_order, collapse = ", "), ")\n",
  "Best Distribution: ", best_model$dist, "\n",
  "Best AIC: ", best_model$aic, "\n",
  "Best BIC: ", best_model$bic, "\n",
  "Best Log-Likelihood: ", best_model$loglik, "\n",
  sep = ""
)
cat(best_model_details)
save_text_output(best_model_details, "best_model_details.txt")

# ✅ Model fitting
cat("Fitting the best model...\n")
spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = best_model$garch_order),
  mean.model = list(armaOrder = best_model$arma_order, include.mean = TRUE),
  distribution.model = best_model$dist
)
fit <- ugarchfit(spec, returns)

# Save model summary
sink(file.path("results", "sgarch_fit_summary.txt"))
summary(fit)
sink()

# ✅ Model diagnostics
cat("Performing model diagnostics...\n")
residuals <- residuals(fit)

# Save residuals plot
save_plot(function() {
  par(mfrow = c(2, 2))
  plot(residuals, type = "l", main = "Residuals from sGARCH Model", xlab = "Time", ylab = "Residuals")
  hist(residuals, breaks = 30, main = "Histogram of Residuals", xlab = "Residuals")
  qqnorm(residuals, main = "Q-Q Plot of Residuals")
  qqline(residuals)
  plot(density(residuals), main = "Density Plot of Residuals", xlab = "Residuals")
}, "sgarch_residuals.png")

# Save ACF plot of residuals
save_plot(function() acf(residuals, main = "ACF of Residuals"), "acf_residuals.png")

# Perform ARCH test on residuals
arch_test <- ArchTest(residuals, lags = 10)
arch_test_result <- paste(
  "ARCH Test p-value:", arch_test$p.value, "\n"
)
cat(arch_test_result)
save_text_output(arch_test_result, "arch_test_result.txt")

# ✅ Forecasting
cat("Forecasting volatility...\n")
n_ahead <- 10  # Number of periods to forecast
forecast <- ugarchforecast(fit, n.ahead = n_ahead)

# Extract forecasted series values and volatility
forecasted_series <- as.data.frame(forecast@forecast$seriesFor)
forecasted_volatility <- as.data.frame(forecast@forecast$sigmaFor)

# Combine forecasted values and volatility into a single data frame
forecast_results <- data.frame(
  Time = paste0("t+", 1:n_ahead),
  Forecasted_Values = forecasted_series[, 1],
  Forecasted_Volatility = forecasted_volatility[, 1]
)

# Format the forecast results for better readability
formatted_forecast_results <- paste(
  "Forecasted Values and Volatility:\n",
  "---------------------------------\n",
  "Time   | Forecasted Values | Forecasted Volatility\n",
  "---------------------------------\n",
  paste(
    sprintf("%-5s | %-16.6f | %-16.6f", forecast_results$Time, forecast_results$Forecasted_Values, forecast_results$Forecasted_Volatility),
    collapse = "\n"
  ),
  sep = ""
)

# Save combined forecast results to a file
save_text_output(formatted_forecast_results, "sgarch_forecast_results.txt")

# Print combined forecast results to the console
cat("Forecasted Values and Volatility:\n")
cat(formatted_forecast_results)

# Save forecast plot
save_plot(function() plot(forecast, which = 3), "sgarch_forecast.png")

# ✅ Final message
cat("All results and plots saved in the 'results' folder.\n")



library(Metrics)
# Actual exchange rates
actual_rates <- c(129.2927, 129.3060, 129.2894, 129.3480, 129.3805, 
                  129.4176, 129.4688, 129.4860, 129.4843, 129.5063, 129.4729)

# Forecasted values (log returns)
forecasted_returns <- c(-0.000003, 0.000003, 0.000006, 0.000007, 0.000008, 
                        0.000008, 0.000008, 0.000008, 0.000008, 0.000008)

# Forecasted volatility
forecasted_volatility <- c(0.000123, 0.000128, 0.000127, 0.000128, 0.000128, 
                           0.000128, 0.000129, 0.000129, 0.000129, 0.000130)

# Compute actual log returns
actual_returns <- diff(log(actual_rates))

# Compute actual volatility as absolute deviation from mean actual return
mean_actual_return <- mean(actual_returns)
actual_volatility <- abs(actual_returns - mean_actual_return)

# Define RMSE function
rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2, na.rm = TRUE))
}

# Define MAPE function
mape <- function(actual, forecast) {
  mean(abs((actual - forecast) / actual), na.rm = TRUE) * 100
}

# Validate forecasted returns
rmse_returns <- rmse(actual_returns, forecasted_returns)
mape_returns <- mape(actual_returns, forecasted_returns)

# Validate forecasted volatility
rmse_volatility <- rmse(actual_volatility, forecasted_volatility)
mape_volatility <- mape(actual_volatility, forecasted_volatility)

# Print results
cat("Validation Results:\n")
cat("-------------------\n")
cat("RMSE (Log Returns):", round(rmse_returns, 6), "\n")
cat("MAPE (Log Returns):", round(mape_returns, 2), "%\n")
cat("RMSE (Volatility):", round(rmse_volatility, 6), "\n")
cat("MAPE (Volatility):", round(mape_volatility, 2), "%\n")
