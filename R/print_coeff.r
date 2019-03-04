#' Prints individual coefficients for inline reporting
#' 
#' This function takes the output of \code{lm_table}, \code{sem_table()} or \code{lmer_table()} and transforms it into latex-code to be used in a rmarkdown file. Individual effects in the original sem_table()-file need to be labelled. Alternatively, the predictor variable can be named. Attention: Only one coefficient at a time can be printed.
#' 
#' @param object A dataframe resulting from \code{sem_table()} or \code{lmer_table()}. Important: The dataframe needs to be print-ready (i.e, the argument \code{print = TRUE} must be used!).
#' @param var_label A character value indicating which coefficient should be printed (draws from the label column in the dataframe).
#' @param var_predict Predictor variable that should be printed.
#' @param b Should the unstandardized effect be printed?
#' @param se Should the standard error be printed?
#' @param ci Should the confidence intervals be printed?
#' @param p Should the p-value be printed?
#' @param beta Should the standarized coefficient be printed?
#' @return A string including a latex code snippet, to be used in Rmarkdown documents.
#' @examples 
#' ## Example 1: Reporting effects from a structural equation modelling
#' 
#' # Estimate the structural equation model
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
#' 
#' # Fitting model
#' fit <- lavaan::sem(model,
#' data = PoliticalDemocracy)
#' 
#' # First step (print = TRUE is mandatory!)
#' results <- sem_table(fit, regressions = TRUE, new_labels = c("H1", "H2", "H3"), print = TRUE)
#' 
#' # Second step
#' print_coeff(results, effect = "H1")
#' print_coeff(results, "H2", se = FALSE, beta = TRUE)
#' 
#' 
#' 
#' ## Example 2: Reporting effects from a multilevel model
#' 
#' # Estimate the multilevel model
#' model <- lmerTest::lmer(Reaction ~ 1 + Days + (1 | Subject), sleepstudy)
#' 
#' # First step (print = TRUE is mandatory!)
#' results <- lmer_table(model, print = TRUE)
#' 
#' # Second step
#' print_coeff(results, variable = "Days", ci = FALSE, p = FALSE)
#' @export
print_coeff <- function(object,
                        var_label = NULL,
                        var_predict = NULL,
                        b = TRUE,
                        se = TRUE,
                        ci = TRUE,
                        p = TRUE,
                        beta = FALSE) {
  # dependencies
  library(tidyverse)
  
  if (!is.null(var_label)) {
    temp <- object %>%
      subset(., label == var_label)
  } else if (!is.null(var_predict)) {
    temp <- object %>%
      subset(., predictor == var_predict)
  } else {
    message("You need to provide the existing label of the relationship or the name of the predictor variable!")
  }
  
  print_coeff <- paste0(
    if (isTRUE(b)) {
      paste0("$b = ", temp$b, "$")
    }, 
    if (isTRUE(se)) {
      paste0(", $se = ", temp$se, "$")
    },
    if (isTRUE(ci)) {
      paste0(", 95\\% CI $[", temp$ll, ", ", temp$ul, "]$")
    },
    if(isTRUE(p)){ 
      paste0(", $\\textit{p} " , 
             if (temp$p != "< .001") {
               paste0("= ", temp$p, "$")
             } else {paste0(temp$p, "$")})
    },
    if (isTRUE(beta)) {
      paste0(", $\\beta = ", temp$beta, "$")
    }
  )
  
  return(print_coeff)
}
