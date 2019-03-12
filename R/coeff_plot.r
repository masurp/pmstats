#' Coefficient plot for a SEM
#' 
#' This function plots a classic coefficient plot for a structural equation model. More ggplot functions (e.g., theme, labs, etc.) can be passed to the resulting object.
#' 
#' @param object An object of class \code{lavaan} created by using \code{sem()} from the package 'lavaan'.
#' @param effect Vector of specific effects in the model (need to be labelled in the original model)
#' @param labels A logical value specifying whether only effects with labels should be plotted
#' @param regression A logical value specifying whether only regressions should be plotted
#' @param y_limits Change the limits of the x-axis.
#' @param y_intercept Defaults to a dashed vertical line at y = 0.
#' @return A ggplot object that can be further customized using standard ggplot elements.
#' @examples 
#' library(lavaan)
#' model <- '
#'   # latent variables
#'   ind60 =~ x1 + x2 + x3
#'   dem60 =~ y1 + y2 + y3 + y4
#'   dem65 =~ y5 + y6 + y7 + y8
#'   
#'   # regressions
#'   dem60 ~ a*ind60
#'   dem65 ~ b*ind60 + c*dem60
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
#' # Creating output table
#' coeff_plot(fit, regression = T, y_limits = c(-1,2.5)) + theme_minimal()
#' @export
coeff_plot <- function(object,
                       effect = NULL,
                       labels = FALSE,
                       regressions = FALSE,
                       y_limits = NULL,
                       y_intercept = 0){
  
  # dependencies
  library(lavaan)
  library(tidyverse)
  library(papaja)
  
  # function
  if (class(object) == "lavaan") {
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
    
    coeffs <- coeffs %>% 
      select(outcome = lhs, 
             predictor = rhs,
             b = est,
             SE = se,
             ll = ci.lower, 
             ul = ci.upper, 
             p = pvalue) 
    
  } else {
    
    coeffs <- object
    
  }
  
  coeffs <- coeffs %>%
    as.tibble %>%
    unite(path, c("outcome", "predictor"), 
          sep = " <- ", remove = F)
  
  plot <- coeffs %>%
    ggplot(aes(x = path,
               y = b,
               ymin = ll,
               ymax = ul)) +
    geom_hline(yintercept = y_intercept, 
               color = "grey", 
               linetype = "dashed") +
    geom_pointrange() +
    labs(x = "",
         y = "Unstandardized coefficients") +
    coord_flip()
  
  if(!is.null(y_limits)) {
    
    plot <- plot +
      ylim(y_limits)
    
  }
  
  return(plot)
}
