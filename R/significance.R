#' Generates the predictors
#'
#' @import tidyverse
#' @import dplyr
#' @import broom
#' @param df dataframe
#' @param response response variable
#' @param explanatory explanatory variable
#' @return Sum of squares for the variable that explains the most variation in the dataset
#' @export significance

significance <- function(df, response, explanatory) {
  formula <- paste(response, "~", explanatory)
  table <- lm(formula, data = df) %>%
    anova() %>%
    pluck("Pr(>F)")
  return(table[1])
}
