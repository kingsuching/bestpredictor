#' Generates the predictors
#'
#' @import tidyverse
#' @import dplyr
#' @param df Dataframe
#' @return best predictor (numeric) in the dataset
#' @export predictors

predictors <- function(df, response) {
  models <- data.frame(create_models(df, response))
  return(models)
}

predictors(sf_salaries, "TotalPay")
