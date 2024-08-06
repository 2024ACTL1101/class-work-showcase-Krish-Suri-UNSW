# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.

$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
AMD_previous_price <- 0
GPSC_previous_price <- 0

# Initialising the daily return values for AMD and GSPC with NA
df$AMD_return <- NA
df$GSPC_return <- NA

# Looping through the rows to calculate daily return
for (i in 1:nrow(df)) {
  if (i != 1) {
    df$AMD_return[i] <- (df$AMD[i] - AMD_previous_price)/AMD_previous_price
    df$GSPC_return[i] <- (df$GSPC[i] - GSPC_previous_price)/GSPC_previous_price
  } 
  
  AMD_previous_price <- df$AMD[i]
  GSPC_previous_price <- df$GSPC[i]
}

```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:

$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
# Initialising the daily risk-free rate to NA
df$daily_RF <- NA

# Looping through the rows to calculate daily risk-free rate
for (i in 1:nrow(df)) {
  df$daily_RF[i] <- (1 + (df$RF[i]) / 100) ^ (1/360) - 1
}
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r

# Initialising the daily excess returns for AMD and GSPC with NA
df$AMD_excess_returns <- NA
df$GSPC_excess_returns <- NA

# Looping through the rows to calculate excess returns
for (i in 1:nrow(df)) {
  if (i != 1) {
    df$AMD_excess_returns[i] = df$AMD_return[i] - df$daily_RF[i]
    df$GSPC_excess_returns[i] = df$GSPC_return[i] - df$daily_RF[i]
  }
}
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
# Performing linear regression using the excess returns of AMD and GSPC
df_lm <- lm(AMD_excess_returns ~ GSPC_excess_returns, data = df)

# Displaying the summary
summary(df_lm)

# Extracting beta from the summary
df_summary <- summary(df_lm)
beta <- coef(df_summary)[2]
cat("Beta:", beta, "\n")

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**
The above regression analysis has determined \(\beta\) to be approximately 1.57. This 
indicates the AMD share price increases by an average of 1.57% for every 1% increase in the S&P 
500. Similarly, for every 1% decrease in the S&P 500, the AMD share price decreases 
by an average of 1.57%. Overall, this suggests that AMD exhibits greater fluctuations, and 
therefore higher volatility, than the market. As investment into AMD carries greater risk than
investing in the S&P 500 index, there is also the expectation of greater returns. Investors who 
are risk averse should choose to invest in the S&P 500, while those willing to carry additional 
risk for the potential of greater returns should choose to invest in AMD shares.

This conclusion is supported by a p-value below 2e-16, which indicates that if there were
no relationship between the AMD and S&P 500, the probability of these results
randomly occurring is extremely low. Thus, we can reject the null hypothesis, and affirm that 
\(\beta\) can reliably be used as a measure of the volatility for the AMD share price. 

#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
# Plotting AMD vs S&P 500
plot <- ggplot(df, aes(x = GSPC_excess_returns, y = AMD_excess_returns)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)+
  labs(title = "Beta of AMD relative to S&P 500", x = "S&P 500 (GSPC) Excess Returns", y = "AMD Excess Returns")
print(plot)
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
# Determining the annual standard error
s_f <- df_summary$sigma
annual_stderr <- sqrt(252) * s_f

rf_rate <- 0.05
GSPC_annual_return <- 0.133

# Calculating expected annual returns
AMD_annual_returns <- rf_rate + beta*(GSPC_annual_return - rf_rate)

# Using the qnorm function to calculate the z-score 
# associated with a 90% prediction interval
z_score <- qnorm(1 - (1 - 0.90) / 2)

# Calculating and printing the lower and upper bound for annual expected return
lower_bound <- AMD_annual_returns - z_score * annual_stderr
upper_bound <- AMD_annual_returns + z_score * annual_stderr

cat("90% Prediction Interval for AMD's Annual Expected Return:\n")
cat("Lower Bound:", lower_bound, "\n")
cat("Upper Bound:", upper_bound, "\n")
```

Therefore, we have determined the 90% prediction interval for AMD's annual expected return
to be between -49.0% and 85.1%. This wide range for potential annual returns further reflects 
the high volatility and unpredictability associated with investing solely in AMD shares.
