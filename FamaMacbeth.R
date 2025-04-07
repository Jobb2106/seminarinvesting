# This script can be used to perform the FamaMacbeth regression. 

# NOTE: not finished yet. Read comments to understand what is going on, not every piece of code should be run

library(tidyfinance)

# The following function is all that is needed to run the regression. However, it requires that we create a dataframe that contains 
# all observations underneath each other for the entire period. 
estimate_fama_macbeth(
  data = data_fama_macbeth, 
  model = "",
  vcov = "newey-west"
)

# We need to collect some variables. I suggest we do: RSJ, market beta, log(market cap), B/M ratio, 1-week lagged return 
# Once we have these variables and put them in a data frame, we simply plug this in the above function and define models 





 