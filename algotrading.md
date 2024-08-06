## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  # Checking for the last entry in amd_df
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(accumulated_shares)
    amd_df$accumulated_shares[i] = 0
  }
  
  # Checking if it's the first day, or the current close is less than the previous close
  else if (previous_price == 0 || amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(-share_size)
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] = accumulated_shares
  }

  previous_price <- amd_df$close[i]
}
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years
  
```r
# The trading period I have chosen is the 2022 financial year (01/07/2021 to 30/06/2022)

# Convert the date strings to Date type
start_date <- as.Date("2021-07-01")
end_date <- as.Date("2022-06-30")

amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

# Copying my code from step 2 such that the program will treat the final day of the 2022
# financial year as the final day for the trading algorithm

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  # Checking for the last entry in amd_df
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(accumulated_shares)
    amd_df$accumulated_shares[i] = 0
  }
  
  # Checking if it's the first day, or the current close is less than the previous close
  else if (previous_price == 0 || amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(-share_size)
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] = accumulated_shares
  }

  previous_price <- amd_df$close[i]
}
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Initialise variables for ROI calculation
expenditure <- 0
profit <- 0

# Calculating expenditure & profit
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$trade_type[i])) {
    if (amd_df$trade_type[i] == 'buy') {
      expenditure <- expenditure - amd_df$costs_proceeds[i]
    }
    profit <- profit + amd_df$costs_proceeds[i]
  }
}

# Printing results of ROI calculations
roi <- (profit/expenditure)*100
print(paste("The total expenditure is $", expenditure))
print(paste("The total profit is $", profit))
print(paste("Therefore, the ROI is", roi, "%"))
```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

I have chosen to implement option 1, selling half of the remaining holdings if the price has increased by 10% from the average purchase price. The logic for my solution is as follows:

- It will always prioritise selling over buying, even if there is only 1 remaining share to sell
- I have chosen to round down the total number of shares sold to the nearest integer to prevent fractional shares from being sold. Because it is rounding down, there will always be at least one remaining share, which also assists with ensuring average purchase price calculations (expenditure/accumulated_shares) never becomes an undefined value (as accumulated shares can never become 0)

This logic may cause my programs results to differ from others, however I believe this most accurately reflects how the provided profit-taking strategy would actually be implemented


```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
expenditure <- 0
avg_purchase_price <- amd_df$close[1]

for (i in 1:nrow(amd_df)) {
  # Checking for the last day of trading period
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(accumulated_shares)
    amd_df$accumulated_shares[i] = 0
  }
  else {
    # Here, we are checking if the current close is sufficiently greater than the average
    # for us to sell our stocks. If this does occur, I have utilised integer division 
    # (i.e. division that rounds down), as you cannot hold half a share 
    if (amd_df$close[i] > (1.1*avg_purchase_price)) {
      amd_df$trade_type[i] <- 'sell'
      amd_df$costs_proceeds[i] <- amd_df$close[i]*(accumulated_shares %/% 2)
      accumulated_shares <- accumulated_shares - accumulated_shares %/% 2
      # Expenditure must be decreased dependent upon the number of shares sold. Here, 
      # accumulated shares holds the amount of remaining shares (after integer divison),
      # and amd_df$accumulated_shares[i - 1] holds the shares held beforehand
      expenditure <- expenditure * (accumulated_shares/amd_df$accumulated_shares[i - 1])
      amd_df$accumulated_shares[i] <- accumulated_shares
    }
    # Checking if we must purchase shares
    else if (previous_price == 0 || amd_df$close[i] < previous_price) {
      amd_df$trade_type[i] <- 'buy'
      amd_df$costs_proceeds[i] <- amd_df$close[i]*(-share_size)
      expenditure <- expenditure - amd_df$costs_proceeds[i]
      accumulated_shares <- accumulated_shares + share_size
      amd_df$accumulated_shares[i] <- accumulated_shares
    }
    # If neither a purchase or sale is made, accumulated_shares must transfer over
    else {
      amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] 
    }
    avg_purchase_price <- expenditure/accumulated_shares
  }
  
  previous_price <- amd_df$close[i]
}

# Redoing the ROI Calculations for the second trading strategy

# Initialise variables for ROI calculation
expenditure <- 0
profit <- 0

# Calculating expenditure & profit
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$trade_type[i])) {
    if (amd_df$trade_type[i] == 'buy') {
      expenditure <- expenditure - amd_df$costs_proceeds[i]
    }
    profit <- profit + amd_df$costs_proceeds[i]
  }
}

# Printing results of ROI calculations
roi <- (profit/expenditure)*100
print(paste("The total expenditure is $", expenditure))
print(paste("The total profit is $", profit))
print(paste("Therefore, the ROI is", roi, "%"))
```

## Step 5b): Applying the Profit-Taking Strategy to Another Period
After running our algorithm over the 2021-22 financial year, a period of uncertainty with
substantial share price fluctuations and an overall trend of a decreasing share price, we 
will now look at a period with a general trend of stable growth. To do so, I have chosen
the year beginning in Q4 2022 (October 1 2022 - September 30 2023). Below is all the code
requried for calculating P/L and ROI using step 2 & 5a's trading strategies

```r
# The trading period I have chosen is the year beginning in Q4 of 
# 2022 (01/10/2022 to 30/09/2023)

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]

# Convert the date strings to Date type
start_date <- as.Date("2022-10-01")
end_date <- as.Date("2023-09-30")

amd_df <- amd_df[amd_df$date >= start_date & amd_df$date <= end_date, ]

# Copying my code from step 2 such that the program will treat the final day of the
# provided period as the final day for the trading algorithm

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

for (i in 1:nrow(amd_df)) {
  # Checking for the last entry in amd_df
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(accumulated_shares)
    amd_df$accumulated_shares[i] = 0
  }
  
  # Checking if it's the first day, or the current close is less than the previous close
  else if (previous_price == 0 || amd_df$close[i] < previous_price) {
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(-share_size)
    accumulated_shares <- accumulated_shares + share_size
    amd_df$accumulated_shares[i] = accumulated_shares
  }

  previous_price <- amd_df$close[i]
}

# Checking the results of the basic trading algorithm
expenditure <- 0
profit <- 0

# Calculating expenditure & profit
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$trade_type[i])) {
    if (amd_df$trade_type[i] == 'buy') {
      expenditure <- expenditure - amd_df$costs_proceeds[i]
    }
    profit <- profit + amd_df$costs_proceeds[i]
  }
}

# Printing results of ROI calculations

roi <- (profit/expenditure)*100
print(paste("The total expenditure is $", expenditure))
print(paste("The total profit is $", profit))
print(paste("Therefore, the ROI is", roi, "%"))

# Determining the results for the profit-taking strategy

# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
expenditure <- 0
avg_purchase_price <- amd_df$close[1]

for (i in 1:nrow(amd_df)) {
  # Checking for the last day of trading period
  if (i == nrow(amd_df)) {
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- amd_df$close[i]*(accumulated_shares)
    amd_df$accumulated_shares[i] = 0
  }
  else {
    # Here, we are checking if the current close is sufficiently greater than the average
    # for us to sell our stocks. If this does occur, I have utilised integer division 
    # (i.e. division that rounds down), as you cannot hold half a share 
    if (amd_df$close[i] > (1.1*avg_purchase_price)) {
      amd_df$trade_type[i] <- 'sell'
      amd_df$costs_proceeds[i] <- amd_df$close[i]*(accumulated_shares %/% 2)
      accumulated_shares <- accumulated_shares - accumulated_shares %/% 2
      # Expenditure must be decreased dependent upon the number of shares sold. 
      # Here, accumulated shares holds the amount of remaining shares (after integer 
      # divison), and amd_df$accumulated_shares[i - 1] holds the shares held beforehand
      expenditure <- expenditure * (accumulated_shares/amd_df$accumulated_shares[i - 1])
      amd_df$accumulated_shares[i] <- accumulated_shares
    }
    # Checking if we must purchase shares
    else if (previous_price == 0 || amd_df$close[i] < previous_price) {
      amd_df$trade_type[i] <- 'buy'
      amd_df$costs_proceeds[i] <- amd_df$close[i]*(-share_size)
      expenditure <- expenditure - amd_df$costs_proceeds[i]
      accumulated_shares <- accumulated_shares + share_size
      amd_df$accumulated_shares[i] <- accumulated_shares
    }
    # If neither a purchase or sale is made, accumulated_shares must transfer over
    else {
      amd_df$accumulated_shares[i] <- amd_df$accumulated_shares[i-1] 
    }
    avg_purchase_price <- expenditure/accumulated_shares
  }
  
  previous_price <- amd_df$close[i]
}

# Redoing the ROI Calculations for the second trading strategy

# Initialise variables for ROI calculation
expenditure <- 0
profit <- 0

# Calculating expenditure & profit 
for (i in 1:nrow(amd_df)) {
  if (!is.na(amd_df$trade_type[i])) {
    if (amd_df$trade_type[i] == 'buy') {
      expenditure <- expenditure - amd_df$costs_proceeds[i]
    }
    profit <- profit + amd_df$costs_proceeds[i]
  }
}

# Printing results of ROI calculations
roi <- (profit/expenditure)*100
print(paste("The total expenditure is $", expenditure))
print(paste("The total profit is $", profit))
print(paste("Therefore, the ROI is", roi, "%"))

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.

The overall downturn and share price instability during the 2021-22 financial year can be
attributed to a decline in PC sales. During this period, high interests and inflation, and 
the threat of global recession caused consumer PC demand to drop substantially. In fact, the 
worldwide shipments of PCs decreased by 16.2% from 2021 to 2022. This decrease in revenue, along 
with supply-chain disruptions impacting PC inventory levels, produced uncertainty amongst 
investors surrounding AMD's future growth. This ultimately resulted in significant share price 
fluctuations and an 18.0% decline over the 2021-22 financial year. Under these conditions, the 
profit-taking trading strategy outperformed the standard trading algorithm from step 2, with 
profit increasing from -$453088.98 to $26412.17, and ROI increasing from -30.82% to 7.78%. This
occurs as the profit-taking strategy has already sold the AMD shares before they substantially 
decrease.

In order to fully assess the effectiveness of the profit taking strategy, we must also examine
what occurs in a period of strong and stable share price growth. This occurred in the year
beginning Q4 2022 (01/10/2022 to 30/09/2023), with share price increasing by 54.36% over this
period. This increase in investor confidence occurred due to a general improvement in overall
market conditions, with inflationary pressures and interest rates lowering, and rising demand
within the semiconductor industry as a whole due to growth of AI and machine learning. In these
conditions, the profit-taking trading strategy produces a total profit of $42028.72, 75.7% less
than the profit of $172679.00 recorded by the basic strategy from step 2. This occurs as the
profit-taking strategy sells too early, and doesn't repurchase shares if they remain at a higher
price. As a result, this strategy had already sold all of AMD's shares before the substantial
increase in share price before the end of the period.

Overall, from both these examples we can observe that profit taking strategy should be utilised 
in periods of uncertainty, where downturns in share price are considered likely.
