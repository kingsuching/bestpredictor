#' Returns the index of element in vector
#'
#' @import tidyverse
#' @import dplyr
#' @param finals, vector of sums of squares for each explanatory variable correlation
#' @param element, element to be searched for
#' @return index of elementi n vector
#' @export create_models

index <- function(finals, element) {
  idx <- 0
  for(i in finals) {
    if(i == element) {
      return(idx)
    }
    idx <- idx+1
  }
  return(-1)
}
