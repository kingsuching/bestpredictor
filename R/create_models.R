#' Generates the models
#'
#' @import tidyverse
#' @import dplyr
#' @param df dataframe
#' @return all possible linear regressions of the dataset
#' @export create_models

plot <- function(df, yvar, xvar) {
  df %>%
    ggplot(aes(x=xvar, y=yvar)) + geom_point() + geom_smooth(method = 'lm')
}

plot(sf_salaries, "TotalPay")
