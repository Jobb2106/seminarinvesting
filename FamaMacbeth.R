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


# Single variable regressions are performed below: 
independent_vars <- c("RSJ", "beta", "MC", "BM", "REV")
results <- list()

# Loop through each independent variable
for (var in independent_vars) {
  # Construct the model formula as a string
  formula_str <- paste("return ~", var) # <------- We need to replace return with correct name
  
  # Run the Fama-MacBeth regression using tidyfinance
  results[[var]] <- estimate_fama_macbeth(
    data = data_fama_macbeth,
    model = formula_str,
    vcov = "newey-west"
  )
}

# Run the full model with all variables
full_model <- estimate_fama_macbeth(
  data = data_fama_macbeth,
  model = "ret ~ RSJ + beta + MC + BM + REV",
  vcov = "newey-west"
)

# Define the other variables (excluding RSJ)
other_vars <- c("beta", "MC", "BM", "REV")

# Initialize a list to store RSJ + one-variable models
rsj_plus_models <- list()

# Loop over each of the other variables to run RSJ + X regression
for (var in other_vars) {
  formula_str <- paste("ret ~ RSJ +", var)
  rsj_plus_models[[var]] <- estimate_fama_macbeth(
    data = data_fama_macbeth,
    model = formula_str,
    vcov = "newey-west"
  )
}







 