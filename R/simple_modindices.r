#' Simple function compute modification indices
#' 
#' This is simply a convenient function that allows to extract modification indices from a fitted lavaan object. It uses the function \code{\link[lavaan]{modindices}} to compute modification indices, yet provides additional options to arrange them in descending order and exclude any additional information.
#' 
#' @param object An object of class lavaan.
#' @param arrange Should the modification indices be sorted from large to small?
#' @param brief If true, only modification indices are shown.
#' @param ... Further argument that can be passed to the function \code{modindices()}.
#' @export
simple_modindices <- function(object,
                              arrange = TRUE,
                              brief = TRUE,
                              ...) {
  # dependencies
  library(lavaan)
  library(tidyverse)
  
  temp <- modindices(object, ...) %>%
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