#' Results table for multilevel models
#' 
#' This functions extracts the fixed effects from a lmer-model.
#' 
#' @param object An object resulting from the function \code{lmer()} from the package \code{lmerTest}.
#' @param print A logical value indicating whether the values should formatted according to APA-guidelines. 
#' @return A tibble.
#' @export
lmer_table <- function(object,
                       print = FALSE){
  
  library(tidyverse)
  library(magrittr)
  library(papaja)
  
  coeffs <- summary(object)$coef %>%
    as.data.frame %>%
    rownames_to_column("predictor")
  cis <- confint(object) %>%
    as.tibble %>% slice(3:nrow(.))
  
  var <- VarCorr(object) %>% 
    as.data.frame %>%
    select(predictor = grp, vcov)
  
  temp <- cbind(coeffs, cis) %>%
    as.tibble %>%
    set_colnames(c("predictor", "b", "SE", "df", "t", "p", "ll", "ul")) %>%
    select(predictor, b, SE, ll, ul, p)
  
  if (isTRUE(print)) {
    temp <- temp %>% 
      mutate_at(vars(b, SE, ll, ul), funs(printnum(.))) %>% 
      mutate(p = printp(p))
  }
  
  return(temp)
}
