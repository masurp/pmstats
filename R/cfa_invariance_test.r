#' Model comparison using ANOVA
#' 
#' This function creates a tibble that display a classical Chisq-test of differently configured models.
#' 
#' @param ... Four lavaan fit objects with base, weak, strong and strict invariance constraints.
#' @param model_names Do you want to rename the models? Provide a vector with the respective model names.
#' @param print A logical value indicating whether table shoud be formatted according to APA.
#' @export
cfa_invariance_test <- function(...,
                                model_names = NULL,
                                print = FALSE){
  # dependencies
  library(lavaan)
  library(tidyverse)
  library(magrittr)
  library(papaja)
  
  # subfunctions
  names_from_dots <- function(...) sapply(substitute(list(...))[-1], deparse)
  
  # Getting model names
  model <- names_from_dots(...)
  
  # function
  temp <- anova(...) %>%
    as.tibble %>%
    set_colnames(c("df", "aic", "bic", "chisq", "diff_chisq", "diff_df", "p"))
  
  temp <- temp %>%
    bind_cols(model = model) %>%
    select(model, everything())
  
  if (!is.null(model_names)) {
    temp <- temp %>%
      mutate(model = model_names)
  }
  
  if (isTRUE(print)) {
    temp2 <- temp %>%
      mutate(p = as.character(p)) %>%
      set_colnames(c("df", "AIC", "BIC", "Chisq", "\\Delta Chisq", "\\Delta df", "\\textit{p}"))
    temp2[2:4,7] <- temp[2:4,7] %>% 
      mutate(p = printp(p))
    return(temp2)
  } else {
  
  return(temp)
    
  }
  
}
