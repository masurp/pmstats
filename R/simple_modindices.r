#' Simple function compute modification indices
#' 
#' @param object An object of class lavaan.
#' @param arrange Should the modification indices be sorted from large to small?
#' @param brief If true, only modification indices are shown.
#' @export
simple_modindices <- function(object,
                              arrange = TRUE,
                              brief = TRUE) {
  # dependencies
  library(lavaan)
  library(tidyverse)
  
  temp <- modindices(object) %>%
    as.tibble
  
  if (isTRUE(arrange)) {
    temp <- temp %>%
      arrange(desc(mi))
  }
  
  if (isTRUE(brief)) {
    temp <- temp %>%
      select(lhs, op, rhs, mi)
  }
  
  return(temp)
}