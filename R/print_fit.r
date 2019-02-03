#' Print fit indices 
#' 
#' This function takes the output of \code{fit_table()} and transforms it into latex-code to be used in a rmarkdown file. 
#' 
#' @param fit_table A dataframe resulting from \code{fit_table()}.
#' @param indices Vector of the indices that should be in the ouput.
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
#' table <- fit_table(fit)
#' 
#' # Second step
#' print_fit(table)
#' @export
print_fit <- function(fit_table,
                      indices = c("chisq", "df", "pvalue", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")){
  
  temp <- paste0(
    if(isTRUE("chisq" %in% indices)) {
      paste0("$\\chi^2$(", fit_table$df, ") = ", fit_table$chisq, ", \\textit{p} = ", fit_table$pvalue)
      },
    if(isTRUE("cfi" %in% indices)) {
      paste0("; CFI = ", fit_table$cfi)
      },
    if(isTRUE("tli" %in% indices)) {
      paste0("; TLI = ", fit_table$tli)
      },
    if(isTRUE("rmsea" %in% indices)) {
      paste0("; RMSEA = ", fit_table$rmsea,
             ", 90% CI [", fit_table$rmsea.ci.lower, ",", fit_table$rmsea.ci.upper, "]")
      },
    if(isTRUE("srmr" %in% indices)) {
      paste0("; SRMR = ", fit_table$srmr)
    }
  )
  return(temp)
}
