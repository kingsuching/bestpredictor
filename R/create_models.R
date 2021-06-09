#' Generates the models
#'
#' @import tidyverse
#' @import dplyr
#' @param df dataframe
#' @return all possible linear regressions of the dataset
#' @export create_models

plot <- function(df, response, explanatory) {
  df %>%
    ggplot(aes(x=explanatory, y=response)) + geom_point() + geom_smooth(method = 'lm')
}

plot(sf_salaries, "TotalPay", "BasePay")
