#' Table with items and factor loadings
#' 
#' This functions extracts the factor loadings from a lavaan object. 
#' 
#' @param object An object of class \code{lavaan} created by using \code{cfa} or \code{sem()}.
#' @param latent_var If more than one variable is specified in the CFA, which one should be shown? If left NULL, items of all latent variable will be evaluated. 
#' @param items A data frame containing item codes and formulations.
#' @param brief A logical value specifying whether only standardized factor loadings should be shown.
#' @param new_labels A vector that needs to have the length of the number of effects that can be used to included a separate column with Hypotheses labels. 
#' @param print A logical value indicating whether the values should formatted according to APA-guidelines. 
#' @param std Indicates which standarized coefficient should be used (defaults to "std.all").
#' @examples 
#' model <- '
#' # latent variables
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + y2 + y3 + y4
#' dem65 =~ y5 + y6 + y7 + y8
#' '
#' fit <- sem(model,
#' data = PoliticalDemocracy)
#' 
#' # Creating output table
#' cfa_table(fit, latent_var = "dem65", brief = FALSE)
#' @export
cfa_table <- function(object,
                      latent_var = NULL,
                      items = NULL,
                      brief = TRUE,
                      std = "std.all",
                      print = FALSE,
                      group = FALSE){
  # dependencies    
  library(lavaan)
  library(papaja)
  library(tidyverse)
  
  # function
  temp <- object %>%
    parameterEstimates(standardized = TRUE) %>%
    subset(op == "=~")
  
  if (!is.null(latent_var)) {
    temp <- temp %>%
      subset(lhs == latent_var) 
  }
  
  coeffs <- temp %>% 
    select(code = rhs,
           b = est,
           se,
           ll = ci.lower, 
           ul = ci.upper, 
           beta = std) %>%
    as.tibble
  
  if (isTRUE(print)) {
    coeffs <- coeffs %>% 
      mutate_at(vars(b, se, ll, ul), funs(printnum(.))) %>% 
      mutate(beta = printnum(beta, gt1 = F))
  }
  
  if (isTRUE(brief)) {
    coeffs <- coeffs %>%
      select(code, beta)
  }
  
  if (!is.null(items)) {
    coeffs <- coeffs %>%
      left_join(items) %>%
      select(code, item, everything())
  }
  
  if (isTRUE(group)) {
    coeffs <- coeffs %>%
      bind_cols(object %>%
                  parameterEstimates(standardized = TRUE) %>%
                  subset(op == "=~") %>% select(group)) %>%
      select(group, everything())
  }
  
  return(coeffs)
  
}
