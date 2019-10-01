#' Prints individual coefficients for RMarkdown inline reporting
#' 
#' This function takes the output of \code{\link[pmstats]{result_table}} and transforms it into latex-code to be used in a rmarkdown file. There are two ways to specify which relationship or effect should be printed: 1) If the relationships are labelled in the result table that is passed to the function, the argument \code{var_label} can be used to specify which effect should be printed; 2) If the result table does not contain an labels, the argument \code{var_predict} can be used to specify the predictor variable of the effect that should be printed. Alternatively, it also takes output of \code{\link[sjstats]{anova_stats}} and transforms it into latex-code.
#' 
#' @param object A dataframe resulting from \code{\link[pmstats]{result_table}}. Important: The dataframe needs to be print-ready (i.e, the argument \code{print = TRUE} must be used!).
#' @param var_label A character value indicating which coefficient should be printed (draws from the label column in the dataframe).
#' @param var_predict Predictor variable that should be printed.
#' @param b Should the unstandardized effect be printed?
#' @param se Should the standard error be printed?
#' @param ci Should the confidence intervals (if bayesian: high credibility intervals) be printed?
#' @param p Should the p-value be printed?
#' @param beta Should the standarized coefficient be printed?
#' @param anova_effect Which effect size should be reported (e.g., "cohens.f", "etasq", or "partial.etasq") 
#' @return A string representing a latex code that can be used in inline reporting in Rmarkdown documents.
#' @examples 
#' ## Example 1: Reporting effects from a structural equation modelling
#' 
#' library(lavaan)
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
#' # Fitting model
#' fit <- sem(model,
#'            data = PoliticalDemocracy)
#' 
#' # First step (print = TRUE is mandatory!)
#' results <- result_table(fit, 
#'                         regressions = TRUE, 
#'                         new_labels = c("H1", "H2", "H3"), 
#'                         print = TRUE)
#' 
#' # Second step
#' print_coeff(results, 
#'             var_label = "H1")
#' print_coeff(results, "H2", 
#'             se = FALSE, 
#'             beta = TRUE)
#' 
#' 
#' ## Example 2: Reporting effects from a multilevel model
#' 
#' # Estimate the multilevel model
#' model <- lmerTest::lmer(Reaction ~ 1 + Days + (1 | Subject), 
#'                         data = sleepstudy)
#' 
#' # First step (print = TRUE is mandatory!)
#' results <- result_table(model, print = TRUE)
#' 
#' # Second step
#' print_coeff(results, 
#'             var_predict = "Days", 
#'             ci = FALSE, 
#'             p = FALSE)
#' 
#' 
#' ## Example 3: Reporting effects from a Bayesian multilevel model
#' 
#' # Estimate the Bayesian multilevel model (using "brms")
#' model_bayes <- brms::brm(Reaction ~ 1 + Days + (1 | Subject), 
#'                          data = sleepstudy,
#'                          chains = 1)
#' 
#' # First step (print = TRUE is mandatory!)
#' results_bayes <- result_table(model_bayes, print = TRUE)
#' 
#' # Second step
#' print_coeff(results_bayes, 
#'             var_predict = "b_Days")
#'             
#' ## Example 4: Reporting results from ANOVAs
#' x <- rep(c(1,0), 50)
#' y <- 2*x + rnorm(100, 0, 1)
#' 
#' # First step
#' m <- aov(y ~ x)
#' 
#' # Second step (sjstats::anova_stats() needs to be used)
#' print_coeff(sjstats::anova_stats(m), 
#'             var_predict = "x", 
#'             anova_effect = "etasq")
#' @export
print_coeff <- function(object,
                        var_label = NULL,
                        var_predict = NULL,
                        b = TRUE,
                        median = TRUE,
                        se = TRUE,
                        ci = TRUE,
                        p = TRUE,
                        beta = FALSE,
                        anova_effect = NULL) {
  
  # dependencies
  library(tidyverse)
  library(papaja)
  
  if ("term" %in% names(object)) {
    object <- object %>%
      mutate(predictor = term)
    res.df <- object %>%
      subset(predictor == "Residuals") %>%
      dplyr::select(df)
  }
  
  # Get relevant subset of the results table
  if (!is.null(var_label)) {
    
    temp <- object %>%
      subset(., label == var_label)
    
  } else if (!is.null(var_predict)) {
    
    temp <- object %>%
      subset(., predictor == var_predict)
    
  } else {
    
    message("You need to provide either the label of the relationship or the name of the predictor variable!")
    
  }
  
  # Transform parameters into printable latex code
  
  ## Bayesian models
  if ("rhat" %in% names(object)) {
    temp <- paste0(
      
      if (isTRUE(median)) {
        
        paste0("$Mdn = ", temp$median, "$")
        
      }, 
      if (isTRUE(se)) {
        
        paste0(", $se = ", temp$se, "$")
        
      },
      if (isTRUE(ci)) {
        
        paste0(", 90\\% HDI $[", temp$ll, ", ", temp$ul, "]$")
        
      }
    )
    
  ## Analysis of Variance
  } else if ("sumsq" %in% names(object)) {
    temp <- paste0(
      
      "$F(", temp$df, ", ", res.df, ") = ", printnum(temp$statistic, 
                                                     gt1 = T, 
                                                     digits = 2), 
      "$",
      if (isTRUE(p)) {
        temp <- temp %>%
          mutate(p = printp(p.value))
        
        paste0(", $p " , 
               if (temp$p != "< .001") {
                 
                 paste0("= ", temp$p, "$")
                 
               } else {
                 paste0(temp$p, "$")})
        
      },
      if ("etasq" %in% anova_effect) {
        
        paste0(", $\\hat{\\eta}^2_G = ", printnum(temp$etasq, gt1 = T, digits = 3), "$")
        
      } else if ("cohens.f" %in% anova_effect) {
        
        paste0(", $f = ", printnum(temp$cohens.f, gt1 = T, digits = 3), "$")
        
      } else if ("partial.etasq" %in% anova_effect) {
        
        paste0(", partial $\\hat{\\eta}^2_G = ", printnum(temp$partial.etasq, gt1 = T, digits = 3), "$")
      }
    )
    
  } else {
    
    
    temp <- paste0(
      
      if (isTRUE(b)) {
        
        paste0("$b = ", temp$b, "$")
        
      }, 
      if (isTRUE(se)) {
        
        paste0(", $se = ", temp$se, "$")
        
      },
      if ("or" %in% names(object)) {
        
        paste0(", Odds Ratio = $", temp$or, "$")
        
      },
      if (isTRUE(ci)) {
        
        paste0(", 95\\% CI $[", temp$ll, ", ", temp$ul, "]$")
        
      },
      if (isTRUE(p)) { 
        
        paste0(", $p " , 
               if (temp$p != "< .001") {
                 
                 paste0("= ", temp$p, "$")
                 
               } else {
                 paste0(temp$p, "$")})
        
      },
      if (isTRUE(beta)) {
        
        paste0(", $\\beta = ", temp$beta, "$")
        
      }
    )
    
  }
  
  return(temp)
}
