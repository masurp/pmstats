#' Creates a table with the reliability estimates of a SEM or CFA
#' 
#' Creates a table in the tibble format that includes reliability values (Cronbach's alpha, omegas, and average variance extracted).
#' 
#' @param object An object of class \code{lavaan} created by using \code{cfa()} or \code{sem()} from the package 'lavaan'.
#' @param var Character value that indicates which variables should be printed.
#' @param total Logical value indicating if total estimates should be printed. 
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
#' 
#' rel_table(fit)
#' @export
rel_table <- function(object,
                      var = NULL,
                      total = TRUE){
  
  # dependencies
  library(semTools)
  library(tidyverse)
  library(papaja)
  
  # function
  temp <- object %>%
    reliability %>% t %>%
    as.data.frame %>%
    rownames_to_column("variable") %>%
    mutate_at(vars(alpha, omega, omega2, omega3, avevar),
              funs(printnum(., gt1 = F)))
  
  if (!isTRUE(total)) {
    temp <- temp %>%
      subset(., variable != "total")
  }
  
  if (!is.null(var)) {
    temp <- temp %>%
      subset(., variable == var)
  }
  return(temp %>% as.tibble)
}


