#' Zero-order correlation table
#' 
#' This function creates a zero-order correlation table. A vector with variables names can be included in order to replace the first column. Further arguments allow to transform the table into a APA-ready print format.
#' 
#' @param data A data frame containing all variables that should be investigated.
#' @param var_names A vector with fitting variable names.
#' @param upper_tri Should the upper triangle be omitted?
#' @param print A logical value indicating whether the table should be formatted according to APA guidelines.
#' @param digits A number specifying how many digit should be printed.
#' @param sig Logical value indicating whether stars should be printed when the effect is significant at alpha = .05. Defaults to true when print = TRUE.
#' @examples 
#' d <- mtcars
#' 
#' # Default
#' zero_order_corr(d)
#' 
#' # Customized for printing
#' zero_order_corr(d, print = T, digits = 3, sig = TRUE)
#' @export
zero_order_corr <- function(data,
                            var_names = NULL,
                            upper_tri = TRUE,
                            print = FALSE,
                            digits = 2,
                            sig = TRUE) {
  # dependencies
  library(tidyverse)
  library(psych)
  library(magrittr)
  library(papaja)
  
  # primary function
  stars <- corr.test(data)$p %>%
    as.data.frame %>%
    rownames_to_column("Variables") %>%
    as.tibble
  stars[upper.tri(stars)] <- NA
  
  temp <- corr.test(data)$r %>%
    as.data.frame %>%
    rownames_to_column("Variables") %>%
    as.tibble
  
  # customizations based on arguments
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
      mutate_at(vars(2:length(.)), 
                function(x) printnum(x, gt1 = F, digits = digits)) %>%
      mutate_at(vars(2:length(.)), 
                funs(ifelse(. == "NA", "", .)))
  }
  
  if (isTRUE(sig)) {
    stars <- as.data.frame(stars < .05)
  
    for(i in 1:length(temp)) {
      for(j in 1:nrow(temp)){
        if (isTRUE(stars[i,j])) {
          temp[i,j] <- paste0(temp[i,j], "*")
        } else {
          temp[i,j]
          }
      } 
    }
  }
  
  return(temp)
  
}
