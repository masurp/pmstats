#' Compare fit of several CFA or SEM models
#' 
#' This function produces fit indices for several CFA or SEM models. This can be handy when investigating measurement invariance or when comparing models in general. It imports the function \code{fit_table()} from the same package (pmstats) to perform the calculations.
#' 
#' @param ... Lavaan fit objects (e.g., with base, weak, strong and strict invariance constraints).
#' @param print Logical value indicating whether the output should be formatted according to APA guidelines. 
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
#' cfa_invariance_fit(fit_1, fit_2, print = TRUE)
#' @export
cfa_invariance_fit <- function(...,
                               model_names = NULL,
                               print = FALSE){
  
  # dependencies
  library(tidyverse)
  
  # subfunctions
  names_from_dots <- function(...) sapply(substitute(list(...))[-1], deparse)
  
  # Getting model names
  model <- names_from_dots(...)
  
  # Creating fit table
  temp <- lapply(list(...), function(x) fit_table(x, reliability = FALSE)) %>%
    bind_rows
  
  temp <- temp %>%
    bind_cols(model = model) %>%
    select(model, everything())
  
  if (isTRUE(print)) {
    temp <- temp %>%
      mutate_at(vars(chisq), funs(printnum)) %>%
      mutate_at(vars(df), funs(printnum(., digits = 0))) %>%
      mutate_at(vars(pvalue), funs(printp(.))) %>%
      mutate_at(vars(cfi, tli, rmsea, srmr), funs(printnum(., gt1 = F)))
  }
  
  if (!is.null(model_names)) {
    temp <- temp %>%
      mutate(model = model_names)
  }
  
  return(temp)
}
