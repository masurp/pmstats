#' Result table for SEM model
#' 
#' This function creates a printable results table based on an SEM model. Several arguments can be specified in order to customize the output. 
#' 
#' @param object An object of class \code{lavaan} created by using \code{sem()} from the package 'lavaan'.
#' @param effect Vector of specific effects in the model (need to be labelled)
#' @param labels A logical value specifying whether only effects with labels should be included
#' @param regression A logical value specifying whether only regressions should be included
#' @param new_labels A vector that needs to have the length of the number of effects that can be used to included a separate column with Hypotheses labels. 
#' @param one_sided A logical value indicating whether the hypotheses was tested one-sided (p-values are divided by two). 
#' @param beta Indicates which standarized coefficient should be used (defaults to "std.all").
#' @param print A logical value indicating whether the printable numbers (rounding) should be displayed. 
#' @examples 
#' model <- '
#' # latent variables
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + y2 + y3 + y4
#' dem65 =~ y5 + y6 + y7 + y8
#' 
#' # regressions
#' dem60 ~ a*ind60
#' dem65 ~ b*ind60 + c*dem60
#' 
#' # residual covariances
#' y1 ~~ y5
#' y2 ~~ y4 + y6
#' y3 ~~ y7
#' y4 ~~ y8
#' y6 ~~ y8
#' '
#' fit <- sem(model,
#' data = PoliticalDemocracy)
#' 
#' # Creating output table
#' sem_table(fit, regressions = T, new_labels = c("H1", "H2", "H3"))
#' @export
sem_table <- function(object,
                      effect = NULL,
                      labels = FALSE,
                      regressions = FALSE,
                      new_labels = NULL,
                      one_sided = FALSE,
                      std = "std.all",
                      print = FALSE){
  
  # dependencies
  library(lavaan)
  library(tidyverse)
  library(papaja)
  
  # function
  
  if(!is.null(effect)){
    coeffs <- object %>%
      parameterEstimates(standardized = TRUE) %>% 
      filter(label %in% effect)
  } else if (isTRUE(regressions)) {
    coeffs <- object %>%
      parameterEstimates(standardized = TRUE) %>%
      filter(op == "~")
  } else if (isTRUE(labels)) {
    coeffs <- object %>%
      parameterEstimates(standardized = TRUE) %>%
      subset(label != "")
  } else {
    coeffs <- object %>%
      parameterEstimates(standardized = TRUE)
  }
  
  if(isTRUE(one_sided)) {
    coeffs["pvalue"] <- coeffs["pvalue"] / 2
  }
  
  coeffs <- coeffs %>% 
    select(outcome = lhs, 
           predictor = rhs,
           b = est,
           se = se,
           ll = ci.lower, 
           ul = ci.upper, 
           p = pvalue,
           beta = std) %>%
    as.tibble
  
  if(isTRUE(print)) {
    coeffs <- coeffs %>% 
      mutate_at(vars(b, SE, ll, ul), funs(printnum(.))) %>% 
      mutate(beta = printnum(beta, gt1 = F)) %>% 
      mutate(p = printp(p))
  }
  
  if (isTRUE(labels)) {
    coeffs <- coeffs %>%
      cbind(., parameterEstimates(object, standardized = TRUE) %>%
              subset(label != "") %>% select(label)) %>%
      select(outcome, predictor, label, everything()) %>%
      as.tibble
  }
  
  
  if (!is.null(new_labels)) {
    coeffs <- coeffs %>%
      cbind(., new_labels) %>%
      mutate(label = as.character(new_labels)) %>%
      select(outcome, predictor, label, everything(), -new_labels) %>%
      as.tibble
  }
  return(coeffs)
}
