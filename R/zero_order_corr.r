#' Creates a zero-order correlation table
#' 
#' Creates a classical zero-order correlation table. A vector with variable names can be added.
#' 
#' @param data A data frame containing all variables that should be investigated.
#' @param var_names A vector with fitting variable names.
#' @param upper_tri Should the upper triangle be omitted?
#' @param sig Logical vector indicating whether stars should be printed when the effect is significant at alpha = .05.
#' @examples 
#' d <- mtcars
#' 
#' zero_order_corr(d)
#' zero_order_corr(d, print = T, sig = TRUE)
#' @export
zero_order_corr <- function(data,
                            var_names = NULL,
                            upper_tri = TRUE,
                            print = FALSE,
                            sig = TRUE) {
  # dependencies
  library(tidyverse)
  library(psych)
  library(magrittr)
  library(papaja)
  
  stars <- corr.test(data)$p %>%
    as.data.frame %>%
    rownames_to_column("Variables") %>%
    as.tibble
  stars[upper.tri(stars)] <- NA
  
  temp <- corr.test(data)$r %>%
    as.data.frame %>%
    rownames_to_column("Variables") %>%
    as.tibble
  
  if (!is.null(var_names)) {
    temp$Variables <- var_names
  }
  
  cols <- c(1:(length(temp)-1)) %>% 
    as.character
  temp <- temp %>%
    set_colnames(c("Variables", cols)) %>%
    mutate(Variables = paste(cols, Variables))
  
  if (isTRUE(upper_tri)) {
    temp[upper.tri(temp)] <- NA
  }
  
  if (isTRUE(print)) {
    temp <- temp %>%
      mutate_at(vars(2:length(.)), function(x) printnum(x, gt1 = F)) %>%
      mutate_at(vars(2:length(.)), 
                funs(ifelse(. == "NA", "", .)))
  }
  
  if (isTRUE(sig)) {
    stars <- as.data.frame(stars < .05)
  
    for(i in 1:length(temp)) {
      for(j in 1:nrow(temp)){
      if (isTRUE(stars[i,j])) {
      temp[i, j] <- paste0(temp[i, j], "*")
      } else {
      temp[i, j]
      }
    } 
   }
  }
  
  return(temp)
  
}

zero_order_corr(d, print = T, sig = .001)
