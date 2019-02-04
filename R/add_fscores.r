#'Adds factor scores computed from CFA to data frame
#'
#'Creates and adds factor scores to the data frame that was used to estimate the CFA. 
#'
#'@param data The data frame that passed to \code{cfa()} or \code{sem()}.
#'@param object An object of class \code{lavaan} created by using \code{cfa()}
#'  or \code{sem()}.
#'@param rename_vars A vector including new names for the factor scores. 
#' @examples
#' d <- PoliticalDemocracy
#' 
#' # Specifying model
#' model <- '
#' # latent variables
#' ind60 =~ x1 + x2 + x3
#' dem60 =~ y1 + y2 + y3 + y4
#' dem65 =~ y5 + y6 + y7 + y8
#' '
#' fit <- sem(model, data = d)
#'
#' add_fscores(d, fit)
add_fscores <- function(data,
                        object,
                        rename_vars = NULL){
  
  fscores <- predict(object) %>%
    as.tibble
  if (!is.null(rename_vars)) {
    colnames(fscores) <- rename_vars
  }
  
  temp <- data %>% 
    cbind(., fscores) %>%
    as.tibble
  
  return(temp)
}

