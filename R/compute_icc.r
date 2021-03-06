#' Intraclass Correlation Coefficient
#' 
#' This function computes the intraclass correlation coefficients for one or several models. 
#' 
#' @param ... One or more objects of class \code{lmer}.
#' @param var_names By default, the function extracts the name(s) of the dependent variable(s) from the objects. If you provide a vector with new names here, these new names will be used. 
#' @param digits_icc How many digits should be printed for the ICC (only works with \code{print = TRUE})?
#' @param digits_percent How many digits should be printed for the percentage (only works with \code{print = TRUE})?
#' @param print Logical value indicating whether the table should be formatted according to APA guidelines. 
#' @examples 
#' library(lme4)
#' # Estimate a multilevel model
#' mod1 <- lmer(Reaction ~ 1 + (1 | Subject), sleepstudy)
#' mod2 <- lmer(Reaction ~ 1 + Days + (1 | Subject), sleepstudy)
#'
#' # Computing the ICC for the first model
#' compute_icc(mod1, digits_icc = 4, digits_percent = 2, print = TRUE)
#' 
#' # Compute ICCs for both models
#' compute_icc(mod1, mod2)
#' @export
compute_icc <- function(...,
                        var_names = NULL,
                        digits_icc = 2,
                        digits_percent = 1,
                        percent = TRUE,
                        print = FALSE){
  
  # dependencies
  library(tidyverse)
  library(sjstats)
  library(papaja)
  
  # get objects
  object_list <- list(...)
  
  # compute icc
  if (length(object_list) == 1) {
    names_vars <- names(model_frame(...))
    
    temp <- icc(...) %>%
      as.tibble %>%
      bind_cols(., vars = names_vars) %>%
      dplyr::select(variable = vars, 
                    icc = value)
    
  } else {
  
  # get names of variables
  names_vars <- object_list %>%
    sapply(., function(x) names(model_frame(x))[[1]])
  temp <- object_list %>%
    sapply(., icc) %>% 
    as.tibble %>%
    bind_cols(., vars = names_vars) %>%
    mutate(model = c(1:length(object_list))) %>%
    dplyr::select(model,
                  variable = vars, 
                  icc = value)
  }
  
  if (!is.null(var_names)) {
    temp <- temp %>%
      mutate(variables = var_names)
  }
  
  if (isTRUE(percent)) {
    temp <- temp %>%
      mutate(percent = icc*100)
  }
  
  if (isTRUE(print)) {
    temp <- temp %>%
      mutate_at(vars(icc), 
                funs(printnum(., 
                              gt1 = F, 
                              zero = F, 
                              digits = digits_icc)))
    if(isTRUE(percent)) {
      temp <- temp %>%
        mutate(percent = printnum(percent, digits = digits_percent))
    }
  }
  
  return(temp)
}

