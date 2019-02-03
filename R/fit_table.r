#' Creates a table with relevant fit indices for an SEM of CFA
#' 
#' Coming soon
#' 
#' @param object An object of class \code{lavaan} created by using \code{cfa()} or \code{sem()} from the package 'lavaan'.
#' @param indices Vector of the indices that should be in the ouput.
#' @param reliability Logical value indicating whether Cronbach's alpha, McDonald's omega (composite reliability), and the average variance extracted should be included-
#' @param robust Logical value indicating whether the model was fitted with a robust estimator. If yes, scaled indices will be used. 
#' @examples 
#' model <- '
#' # latent variables
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + y2 + y3 + y4
#' dem65 =~ y5 + y6 + y7 + y8
#' 
#' # regressions
#' dem60 ~ ind60
#' dem65 ~ ind60 + dem60
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
#' fit_table(fit)
#' @export
fit_table <- function(object,
                      indices = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"),
                      rmsea_ci = FALSE,
                      reliability = TRUE,
                      robust = FALSE) {
  # dependencies
  library(lavaan)
  library(semTools)
  library(tidyverse)
  library(papaja)
  
  # function
  if(!isTRUE(robust)){
    fit_indices <- inspect(object, what = "fit") %>% 
      .[indices]
  } else {
    indices_robust <- paste0(indices, ".scaled")
    if(isTRUE("srmr.scaled" %in% indices_robust)) (
      indices_robust[indices_robust == "srmr.scaled"] <- "srmr_bentler"
    )
    
    fit_indices <- inspect(object, what = "fit") %>% 
      .[indices_robust] %>% 
      set_names(indices)
  }
  
  # creating table
  fit_indices <- fit_indices %>%
    t %>% as.tibble %>%
    mutate_at(vars(chisq), 
              funs(printnum(.))) %>% 
    mutate_at(vars(cfi, tli, rmsea, rmsea.ci.lower, rmsea.ci.upper, srmr), 
              funs(printnum(., gt1 = F, zero = F))) %>% 
    mutate_at(vars(pvalue), 
              funs(printp(.)))
  
  if(isTRUE(reliability)) {
  fit_indices <- fit_indices %>% 
      mutate(alpha = semTools::reliability(object)["alpha", "total"],
             omega = semTools::reliability(object)["omega3", "total"],
             ave = semTools::reliability(object)["avevar", "total"]) %>%
      mutate_at(vars(alpha, omega, ave), 
                funs(printnum(., gt1 = F, zero = F)))
  }
  
  if(isTRUE(rmsea_ci)){
    fit_indices <- fit_indices %>%
      select(-rmsea.ci.lower, -rmsea.ci.upper)
  }
  return(fit_indices)
}


