#' Generates the models
#'
#' @import tidyverse
#' @import dplyr
#' @export get_numeric_columns
#'

linear_model <- function(df, y, x) {
  return(lm(y ~ x, df))
}

linear_model(sf_salaries, TotalPay, BasePay)
