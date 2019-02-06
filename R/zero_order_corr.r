#' Creates a zero-order correlation table
#' 
#' Creates a classical zero-order correlation table. A vector with variable names can be added.
#' 
#' @param data A data frame containing all variables that should be investigated.
#' @param var_names A vector with fitting variable names.
#' @param upper_tri Should the upper triangle be omitted?
#' @examples 
#' d <- mtcars
#' 
#' zero_order_corr(d)
zero_order_corr <- function(data,
                            var_names = NULL,
                            upper_tri = T) {
  # dependencies
  library(tidyverse)
  library(psych)
  library(magrittr)
  
  # function
  temp <- corr.test(data)$r %>%
    as.data.frame %>%
    rownames_to_column("Variables") %>%
    as.tibble
  
  if (!is.null(var_names)) {
    temp$Variables <- var_names
  }
  
  cols <- c(1:(length(temp)-1)) %>% 
    as.character
  temp <- temp %>%
    set_colnames(c("Variables", cols)) %>%
    mutate(Variables = paste(cols, Variables))
  
  if (isTRUE(upper_tri)) {
    temp[upper.tri(temp)] <- NA
  }
  
  return(temp)
  
}