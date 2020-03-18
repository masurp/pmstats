#' Computes goodness-of-fit indices from "lm" objects
#' 
#' This function takes one of more "lm" objects and extracts goodness-of-fit indices such as AIC, BIC, logLikelihood and deviance.
#' 
#' @param ... objects of class \code{"lm"}.
#' @example 
#' x <- rnorm(100, 2, 1)
#' y <- 2*x + rnorm(100, 0, 1)  
#' m <- lm(y ~ x)
#' 
#' extract_gof(m, print = T)
#' @export
# Extracting goodness-of-fit indices from lm() objects
extract_gof <- function(...,
                        digits = 2,
                        print = TRUE) {
  
  library(tidyverse)
  library(magrittr)
  library(papaja)
  
  # Create list of objects
  l <- list(...)
  
  # Extract indices from each object and transform into dataframe
  suppressWarnings(
  temp <- map_dfr(l, 
                  function(x) bind_cols(aic = AIC(x), 
                                        bic = BIC(x), 
                                        logLik = logLik(x), 
                                        deviance = deviance(x))) %>%
    t %>%
    as.data.frame %>%
    rownames_to_column("gof") %>%
    as_tibble %>%
    rename(value = "V1")
  )
    
    if (print == T) {
      temp <- temp %>%
        mutate_if(is.numeric, 
                printnum, 
                digits = digits,
                gt1 = T)
    }
  return(temp)
}
