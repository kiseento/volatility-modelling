# KES/USD Exchange Rate Volatility Modeling

This project models and forecasts the volatility of the KES/USD exchange rate using various GARCH family models in R.

## Objective

To analyze and compare the performance of different GARCH models—`sGARCH`, `eGARCH`, `tGARCH`, `pGARCH`, and `mGARCH`—in forecasting the volatility of the KES/USD exchange rate. The goal is to help in understanding the risk dynamics of currency movements for investors, policymakers, and financial analysts.


## Tools & Technologies

- **R** (primary language)
- **Packages**: `rugarch`, `fGarch`, `forecast`, `tseries`, `PerformanceAnalytics`, `Metrics`, `Directional`, `ggplot2`
- **RStudio** (IDE)
- **Git/GitHub** for version control

## Dataset

- **Source**: CBK
- **Frequency**: Daily
- **Period Covered**: [Jan-Dec 2024]
- **Variables**: `Date`, `ExchangeRate` (KES/USD)


## Methodology

1. **Data Cleaning**  
   - Removed missing values
   - Filtered out weekends/holidays

2. **Stationarity Check**  
   - Used ADF Test  
   - Differenced non-stationary series

3. **ARMA Order Selection**  
   - AIC and BIC criteria used to determine optimal ARMA(p,q) terms for each GARCH model

4. **Model Fitting**  
   - Applied each GARCH variant using `rugarch`
   - Modeled conditional volatility for KES/USD

5. **Model Evaluation**  
   - Metrics: RMSE, MAPE, and Directional Accuracy (DA)
   - Visual inspection of forecast vs actual

6. **Comparison**  
   - Ranked models based on accuracy metrics


## Results

- The best-performing model: **[sGARCH]**
- Highest Directional Accuracy: **[sGARCH- 66.67%]**
- Most stable forecast: **[sGARCH- stability in horizon]**

 Visualizations include time series plots, residual diagnostics, and volatility forecasts._



## Team

- Philip Leteipa K. (Lead Analyst)
- Alice Atieno
- Dennis Mwangi Wambui
- Matilda Achieng  
_Supervised by Dr. Walter Onchere_

## Structure
volatility-modeling/ ┣  plots/ # Forecast and diagnostics plots ┣  models/ # GARCH model scripts ┣  report/ # PDF/Word final report ┣  data/ # Cleaned data ┣  README.md # This file ┗  final_script.R # Main R script
