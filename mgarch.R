# Load necessary libraries
library(rmgarch)
library(forecast)
library(tseries)
library(FinTS)
library(ggplot2)  # For plotting

# Create a directory to save results and plots
if (!dir.exists("results5")) {
  dir.create("results5")
}

# Function to save text output in tabular format
save_table_output <- function(data, filename) {
  write.table(data, file.path("results5", filename), sep = "\t", row.names = FALSE, quote = FALSE)
}

# Data loading & preprocessing
cat("Loading data...\n")
data <- read.csv("TRADE WEIGHTED AVERAGE INDICATIVE RATES.csv")
data$Date <- as.Date(data$Date, format="%d/%m/%Y")
data <- data[order(data$Date), ]

# Calculate log returns and handle missing values
cat("Calculating log returns...\n")
returns <- na.omit(diff(log(data$EXCHANGE.RATE)))  # First differencing
returns2 <- diff(returns, differences = 1)  # Second differencing

# Align date indices
dates1 <- data$Date[-1]
dates2 <- data$Date[-c(1,2)]

# Check for NA values
if (sum(is.na(returns)) > 0 || sum(is.na(returns2)) > 0) {
  stop("Error: Missing values detected in log returns. Consider data cleaning.")
}

# Save log returns in tabular format
log_returns_table <- data.frame(
  Date = dates1[-1],
  First_Difference = returns[-1],
  Second_Difference = returns2
)
save_table_output(log_returns_table, "log_returns.txt")

# Model Selection & Fitting
cat("Automatically selecting best DCC-GARCH model...\n")
auto_univariate_garch <- function(returns) {
  best_aic <- Inf
  best_order <- c(0, 0)
  best_garch_order <- c(1, 1)
  best_dist <- "norm"
  
  for (p in 0:1) {
    for (q in 0:1) {
      for (garch_p in 1:1) {
        for (garch_q in 1:1) {
          for (dist in c("norm", "std", "ged")) {
            spec <- ugarchspec(
              variance.model = list(model = "sGARCH", garchOrder = c(garch_p, garch_q)),
              mean.model = list(armaOrder = c(p, q), include.mean = TRUE),
              distribution.model = dist
            )
            fit <- tryCatch({
              ugarchfit(spec, returns, solver = "hybrid")
            }, error = function(e) NULL, warning = function(w) NULL)
            
            if (!is.null(fit) && fit@fit$convergence == 0) {
              current_aic <- infocriteria(fit)[1]  # AIC
              if (current_aic < best_aic) {
                best_aic <- current_aic
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
  return(list(arma_order = best_order, garch_order = best_garch_order, dist = best_dist, aic = best_aic))
}

best_model1 <- auto_univariate_garch(returns)
best_model2 <- auto_univariate_garch(returns2)

# DCC-GARCH Model Specification
cat("Specifying DCC-GARCH model...\n")
uspec1 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = best_model1$garch_order),
  mean.model = list(armaOrder = best_model1$arma_order, include.mean = TRUE),
  distribution.model = best_model1$dist
)
uspec2 <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = best_model2$garch_order),
  mean.model = list(armaOrder = best_model2$arma_order, include.mean = TRUE),
  distribution.model = best_model2$dist
)

spec <- dccspec(
  uspec = multispec(list(uspec1, uspec2)),  
  dccOrder = c(1, 1),  
  distribution = "mvt"
)

cat("Fitting DCC-GARCH model...\n")
fit <- dccfit(spec, data = cbind(returns, returns2))

# Check for convergence
if (fit@mfit$convergence != 0) {
  stop("DCC-GARCH model failed to converge. Check data or model specification.")
}
print(fit)

# Forecasting
cat("Forecasting volatility and correlations...\n")
n_ahead <- 10  
forecast <- dccforecast(fit, n.ahead = n_ahead)

# Extract forecasted values
forecasted_series <- as.data.frame(forecast@mforecast$mu)
forecasted_volatility <- as.data.frame(t(sapply(1:n_ahead, function(i) sqrt(diag(forecast@mforecast$H[[1]][,,i])))))
colnames(forecasted_volatility) <- c("Volatility_1", "Volatility_2")
forecasted_volatility[is.na(forecasted_volatility)] <- 0

# Extract forecasted correlations
cor_matrices <- rcor(forecast)
forecasted_correlations <- sapply(1:n_ahead, function(i) cor_matrices[[1]][1,2,i])
forecasted_correlations[is.na(forecasted_correlations)] <- 0

# Save results in tabular format
forecast_results <- data.frame(
  Time = 1:n_ahead,
  Correlation = forecasted_correlations,
  Volatility_1 = forecasted_volatility[,1],
  Volatility_2 = forecasted_volatility[,2]
)
save_table_output(forecast_results, "forecast_results.txt")

cat("Forecasting completed. Results saved in 'results5' directory.\n")

# Plotting Volatilities and Correlation
cat("Generating and saving plots...\n")

# Combine volatilities into a single data frame for plotting
volatility_data <- data.frame(
  Time = 1:n_ahead,
  Volatility_1 = forecasted_volatility[,1],
  Volatility_2 = forecasted_volatility[,2]
)

# Plot for Volatility_1 vs Volatility_2
volatility_comparison_plot <- ggplot(volatility_data, aes(x = Time)) +
  geom_line(aes(y = Volatility_1, color = "Volatility_1"), size = 1) +
  geom_line(aes(y = Volatility_2, color = "Volatility_2"), size = 1) +
  labs(title = "Comparison of Volatility_1 and Volatility_2",
       x = "Time",
       y = "Volatility",
       color = "Legend") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),  # Set background to white
        plot.background = element_rect(fill = "white"))   # Set plot background to white

# Save the plot
ggsave(file.path("results5", "volatility_comparison_plot.png"), volatility_comparison_plot, width = 8, height = 6)

# Plot for Correlation
correlation_plot <- ggplot() +
  geom_line(aes(x = 1:n_ahead, y = forecasted_correlations, color = "Forecasted Correlation"), size = 1) +
  labs(title = "Forecasted Correlation",
       x = "Time",
       y = "Correlation",
       color = "Legend") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "white"),  # Set background to white
        plot.background = element_rect(fill = "white"))   # Set plot background to white

# Save the plot
ggsave(file.path("results5", "correlation_plot.png"), correlation_plot, width = 8, height = 6)

cat("Plots saved in 'results5' directory.\n")