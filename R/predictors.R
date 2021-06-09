#' Generates the predictors
#'
#' @import tidyverse
#' @import dplyr
#' @import broom
#' @param df dataframe
#' @return best predictors (numeric) in the dataset
#' @export predictors

predictors <- function(df, response) {
  df <- df %>%
    select_if(is.numeric)
  final <- c()
  for(i in names(df)) {
    if(i != response) {
      f <- paste(response, "~", i)
      m <- lm(formula = f, data = sf_salaries) %>% anova() %>% pluck("Sum Sq")
      final <- append(final, m[1])
    }
  }
  return(max(final))
}

predictors(sf_salaries, "TotalPayBenefits")
