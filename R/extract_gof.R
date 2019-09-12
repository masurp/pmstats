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
#' extract_gof(m)
#' @export
# Extracting goodness-of-fit indices from lm() objects
extract_gof <- function(...,
                        digits = 2) {
  
  library(tidyverse)
  library(magrittr)
  
  # Create list of objects
  l <- list(...)
  
  # Extract indices from each object and transform into dataframe
  temp <- map_dfr(l, 
                  function(x) bind_cols(aic = AIC(x), 
                                        bic = BIC(x), 
                                        logLik = logLik(x), 
                                        deviance = deviance(x)) %>%
                    mutate_if(is.numeric, 
                              round, 
                              digits = digits)) %>% 
    t %>%
    as.data.frame %>%
    rownames_to_column("gof") %>%
    set_colnames(c("gof", "value")) %>%
    as_tibble
  return(temp)
}
