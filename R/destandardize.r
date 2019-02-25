#' Destandardize effect sizes (r) 
#' 
#' This function allows to destandarize (or vice versa: standardize) an effect size (Pearson's r or a typical Beta-coefficient) using the standard deviations of the independent and dependent variable.
#' 
#' @param b The unstandarized effect size.
#' @param Beta The standardized effect size.
#' @param sd_x The standard deviation of the independent variable.
#' @param sd_y The standard deviation of the dependent variable.
#' @examples 
#' x <- rnorm(100, 2, 1)
#' y <- 2*x + rnorm(100, 0, 1)
#' 
#' destandardize(Beta = .1, sd_x = sd(x), sd_y = sd(y))
#' @export
destandardize <- function(b = NULL, 
                          Beta = NULL, 
                          sd_x, 
                          sd_y){
  if (is.null(b) & is.null(Beta)) {
    message("You need to provide either an unstandarized (b) or standarized (Beta) coefficient!")
  }
  
  if (is.null(sd_x) | is.null(sd_y)) {
    message("You need to provide the standard deviation of the independent and the dependent variable!")
  }
  
  if(is.null(b)){
    
    b <- Beta*sd_y/sd_x
    return(b)
    
  } else {
    
    Beta <- b*sd_x/sd_y
    return(Beta)
    
  }
}
