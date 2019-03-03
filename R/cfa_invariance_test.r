#' Model comparison using ANOVA
#' 
#' This function creates a tibble that display a classical Chisq-test of differently configured models.
#' 
#' @param ... Four lavaan fit objects with base, weak, strong and strict invariance constraints.
#' @param model_names Do you want to rename the models? Provide a vector with the respective model names.
#' @param print A logical value indicating whether table shoud be formatted according to APA.
#' @examples 
#' model_1 <- '
#' # latent variables
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + y2 + y3 + y4
#' dem65 =~ y5 + y6 + y7 + y8
#' '
#' fit_1 <- cfa(model_1, data = PoliticalDemocracy)
#' 
#' model_2 <- '
#' # latent variables
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + y2 + y3
#' dem65 =~ y5 + y6 + y7 + y8
#' '
#' fit_2 <- cfa(model_2, data = PoliticalDemocracy)
#' 
#' # Model comparison
#' cfa_invariance_test(fit_1, fit_2, print = TRUE)
#' 
#' # Renaming model names
#' cfa_invariance_test(fit_1, fit_2, model_names = c("Model 1: All variables", "Model 2: Some omissions"), print = TRUE)
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
  
  ## Length of dots
  length_dots <- length(list(...))
  
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
      set_colnames(c("model", "df", "AIC", "BIC", "Chisq", "\\Delta Chisq", "\\Delta df", "p"))
    temp2[2:length_dots,8] <- temp[2:length_dots,8] %>% 
      mutate(p = printp(p))
    return(temp2)
  } else {
  
  return(temp)
    
  }
  
}
