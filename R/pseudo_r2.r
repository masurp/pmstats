#' Pseudo R-Square for Logistic Regression Models
#' 
#' This functions computes several types of Pseudo-R-Squares from one or several logistic regression models obtained from \code{glm}, \code{polr}, or \code{multinom}.
#' 
#' @param ... one or several objects of type \code{glm}, \code{polr}, or \code{multinom}.
#' @param type character, one or several out of "McFadden", "Nagelkerke", "CoxSnell".
#' @param digits how many digits should be printed?
#' @return A table with all Pseudo-R-Squares.
#' @examples
#' # Simulate data
#' set.seed(666)
#' x1 = rnorm(1000)           # some continuous variables 
#' x2 = rnorm(1000)
#' z = 1 + 2*x1 + 3*x2        # linear combination with a bias
#' pr = 1/(1+exp(-z))         # pass through an inv-logit function
#' y = rbinom(1000,1,pr)      # bernoulli response variable
#' d = data.frame(y=y,x1=x1,x2=x2)
#' 
#' # Estimate models
#' m1 <- glm(y~x1, data=d, family="binomial")
#' m2 <- glm(y~x1+x2, data=d, family="binomial")
#' 
#' # Run function
#' pseudo_r2(m1, m2)
pseudo_r2 <- function(...,
                      type = c("McFadden", 
                               "Nagelkerke",
                               "CoxSnell"),
                      digits = 2) {
  
  library(DescTools)
  library(tidyverse)
  library(magrittr)
  library(papaja)
  
  # transform models into list
  l_temp <- list(...)
  
  # Create Pseudo R2
  temp <- l_temp %>% 
    map_dfc(., function(x) PseudoR2(x, 
                                    which = type) %>%
            as.data.frame %>%
            rownames_to_column("pseudoR2") %>%
            set_colnames(c("pseudoR2", "value")) %>%
            mutate(value = printnum(value, 
                                    digits = digits))) %>%
    select(contains("value")) %>%
    bind_cols("pseudoR2" = type, .)
  
  # Rename columns
  column_names <- c(1:length(l_temp))
  column_names <- paste("M", column_names, sep = "")
  
  temp <- temp %>%
    set_colnames(c("pseudoRs", column_names))
  
  return(temp)
}
