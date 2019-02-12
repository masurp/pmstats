#' Create results from invariance test
#' 
#' This function creates a tibble that display a classical Chisq-test of differently configured models.
#' 
#' @param ... Four lavaan fit objects with base, weak, strong and strict invariance constraints.
#' @param print A logical value indicating whether table shoud be formatted according to APA.
#' @export
cfa_invariance_test <- function(...,
                                print = FALSE){
  # dependencies
  library(lavaan)
  library(tidyverse)
  library(magrittr)
  library(papaja)
  
  # function
  temp <- anova(...) %>%
    as.tibble %>%
    set_colnames(c("df", "aic", "bic", "chisq", "diff_chisq", "diff_df", "p"))
  
  if (isTRUE(print)) {
    
    temp2 <- temp %>%
      mutate(p = as.character(p)) %>%
      set_colnames(c("df", "AIC", "BIC", "Chisq", "\\Delta Chisq", "\\Delta df", "\\textit{p}"))
    temp2[2:4,7] <- temp[2:4,7] %>% 
      mutate(p = printp(p))
    
    
  }
  return(temp2)
  
}
