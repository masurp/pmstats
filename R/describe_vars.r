#' Customized version of the function describe (psych)
#' 
#' This function is essentially similar to the function \code{describe()} in the package \code{psych} (it even uses that function), but provides some additional benefits: First, it transforms the the output into a tibble, second, it reduces number of psychometrics  to the most important ones, and it allows to include item formulation drawn from an additional data frame (needs to have a variable "code" that includes similar variable codes as the original data frame and a variable named "item" that includes the item formulations).
#' 
#' @param data A data frame that includes that variables that should be evaluated. Can also be a vector with numeric values. 
#' @param items A data frame containing the variable codes and item formulations.
#' @param var_name If you are evaluating a single vector, you specify the variable name here. 
#' @param brief A logical value indicating wether all psychometrics or only the mean and the standard deviations should be printed
#' @examples 
#' d <- mtcars
#' 
#' describe_vars(mtcars)
#' describe_vars(mtcars$cyl, var_name = "cylinder")
#' @export
describe_vars <- function(data, 
                          items = NULL, 
                          var_name = NULL, 
                          brief = FALSE, 
                          first_col = "code") {
  
  # dependencies
  library(psych)
  library(tidyverse)
  
  temp <- data %>%
    describe %>%
    as.data.frame %>%
    rownames_to_column(first_col) 
  
  if (!is.null(items)) {
    temp <- temp %>%
      left_join(items) %>%
      select(first_col, item, mean, sd, min, max, skew, kurtosis, n)
  } else {
    temp <- temp %>%
      select(first_col, mean, sd, min, max, skew, kurtosis, n)
  }
  
  if((class(data) == "integer" | class(data) == "numeric") & !is.null(var_name)){
    temp <- temp %>%
      mutate(variable = var_name) %>%
      select(variable, everything(), -first_col)
  }
  
  if (isTRUE(brief) & !is.null(items)) {
    temp %>%
      select(first_col, item, mean, sd) %>%
      as.tibble
  } else if (isTRUE(brief) & is.null(items)){
    temp %>%
      select(first_col, mean, sd) %>%
      as.tibble
  } else {
    temp %>% 
      as.tibble
  }
}


