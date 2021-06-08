#' Generates the predictors
#'
#' @import tidyverse
#' @import dplyr
#' @export predictors

predictors <- function(df, response) {
  models <- create_models(df, response)
}
