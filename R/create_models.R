#' Generates the models
#'
#' @import tidyverse
#' @import dplyr
#' @param df dataframe
#' @return all possible linear regressions of the dataset
#' @export create_models

create_models <- function(df, response) {
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

create_models(sf_salaries, "TotalPay")
