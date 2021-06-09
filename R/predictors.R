#' Generates the predictors
#'
#' @import tidyverse
#' @import dplyr
#' @import broom
#' @param df dataframe
#' @return Sum of squares for the variable that explains the most variation in the dataset
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
  #return(max(final))
  max_index <- index(final, max(final))
  value <- names(df)[max_index]
  return(value)
}

predictors(sf_salaries, "TotalPayBenefits")
