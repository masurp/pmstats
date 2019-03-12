#' Creates a table with relevant fit indices for an SEM or CFA
#' 
#' This function returns a data frame that contains the fit indices for a SEM or CFA model. The type of indices can be chosen (available are all indices that can be obtained using the function \code{\link[lavaan]{inspect}}). Reliability indices (alpha, omega, average variance extracted) can added if desired (will be computed by using the function \code{\link[semTools]{reliability}}).
#' 
#' @param object An object of class \code{lavaan} created by using \code{cfa()} or \code{sem()} from the package 'lavaan'.
#' @param indices Vector of the indices that should be in the ouput.
#' @param rmsea_ci Logical vector indicating whether the confidence intervals of the RMSEA should be included (important for reporting the fit in an RMarkdown document using \code{\link[pmstats]{print_fit}})
#' @param reliability Logical value indicating whether Cronbach's alpha, McDonald's omega (composite reliability), and the average variance extracted should be included. 
#' @param robust Logical value indicating whether the model was fitted with a robust estimator. If yes, scaled indices will be used. 
#' @param print Logical value indicating whether the table should be formatted according to APA guidelines. 
#' @return A data frame containing the fit indices of the model. 
#' @examples 
#' library(lavaan)
#' model <- '
#'   # latent variables
#'   ind60 =~ x1 + x2 + x3
#'   dem60 =~ y1 + y2 + y3 + y4
#'   dem65 =~ y5 + y6 + y7 + y8
#' 
#'   # regressions
#'   dem60 ~ ind60
#'   dem65 ~ ind60 + dem60
#' 
#'   # residual covariances
#'   y1 ~~ y5
#'   y2 ~~ y4 + y6
#'   y3 ~~ y7
#'   y4 ~~ y8
#'   y6 ~~ y8
#' '
#' fit <- sem(model,
#' data = PoliticalDemocracy)
#' 
#' # Create table of fit indices
#' fit_table(fit)
#' fit_table(fit, indices = c("cfi", "tli"))
#' fit_table(fit, rmsea_ci = TRUE, print = TRUE)
#' @export
fit_table <- function(object,
                      indices = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"),
                      rmsea_ci = FALSE,
                      reliability = FALSE,
                      robust = FALSE,
                      print = FALSE) {
  # dependencies
  library(lavaan)
  library(semTools)
  library(tidyverse)
  library(papaja)
  
  # Check if robust estimator was used
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
  
  # Create table 
  fit_indices <- fit_indices %>%
    t %>% as.tibble
  
  if (isTRUE(reliability)) {
      fit_indices <- fit_indices %>% 
        mutate(alpha = semTools::reliability(object)["alpha", "total"],
               omega = semTools::reliability(object)["omega3", "total"],
               ave = semTools::reliability(object)["avevar", "total"])
  }
  
  if (isTRUE(print)) {
  # creating table
   fit_indices <- fit_indices %>%
    mutate_at(vars(chisq), 
              funs(printnum(.))) %>% 
    mutate_at(vars(cfi, tli, rmsea, srmr), 
              funs(printnum(., gt1 = F, zero = F))) %>% 
    mutate_at(vars(pvalue), 
              funs(printp(.)))
   
     if (isTRUE(rmsea_ci)) {
       fit_indices <- fit_indices %>%
         mutate_at(vars(rmsea.ci.lower, rmsea.ci.upper), 
                   funs(printnum(., gt1 = FALSE, zero = F)))
     }
  }
  
  if (isTRUE(reliability) & isTRUE(print)) {
    fit_indices <- fit_indices %>%
      mutate_at(vars(alpha, omega, ave), 
                funs(printnum(., gt1 = F, zero = F)))
  }

  if("rmsea.ci.lower" %in% names(fit_indices) & !isTRUE(rmsea_ci)){
    fit_indices <- fit_indices %>%
      select(-rmsea.ci.lower, -rmsea.ci.upper)
  }
  return(fit_indices)
}


