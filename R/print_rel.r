#' Print reliabilities of a latent factor 
#' 
#' This function takes the output of \code{rel_table()} and transforms it into latex-code to be used in a rmarkdown file. 
#' 
#' @param rel_table A dataframe resulting from \code{rel_table()}.
#' @param indices Vector of the indices that should be in the ouput.
#' @param list Logical value indicating whether each estimate should be printed separately and put into a list.
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
#' # First step
#' table <- rel_table(fit)
#' 
#' # Second step
#' print_rel(table)
#' @export
print_rel <- function(object,
                      var = NULL,
                      rel_values = c("alpha", "omega3", "avevar"),
                      list = F){
  # dependencies
  library(tidyverse)
  library(papaja)
  
  if (is.null(var)) {
    message("You need to provide a valid variable name")
  } else {
  
  temp <- object %>%
      subset(., variable == var)
  
  if (!isTRUE(list)) {
    
  final <- paste0(
    if(isTRUE("alpha" %in% rel_values)) {
      paste0("$\\alpha$ = ", temp$alpha)
    },
    if(isTRUE("omega3" %in% rel_values)) {
      paste0("; $\\omega$ = ", temp$omega3)
    }, if(isTRUE("avevar" %in% rel_values)) {
      paste0("; AVE = ", temp$avevar)
    }
  )
  return(final)
  } else {
    list(
      alpha = paste0("$\\alpha$ = ", temp$alpha),
      omega = paste0("$\\omega$ = ", temp$omega3),
      AVE = paste0("AVE = ", temp$avevar)
    )
  }
  }
}
