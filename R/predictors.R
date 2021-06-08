#' Generates the predictors
#'
#' @import tidyverse
#' @import dplyr
#' @param df Dataframe
#' @return best predictor (numeric) in the dataset
#' @export predictors

predictors <- function(df, response) {
  df <- df %>%
    select_if(is.numeric)
  for(i in names(df)) {
    f <- paste(response, "~", i)
    m <- lm(formula = f, data = sf_salaries) %>% anova() %>% pluck("Sum Sq")
    final <- append(final, m)
  }
  max_r2 <- max(final)
  index <- which(max_r2 %in% final)
  return(names(df)[index])
}
