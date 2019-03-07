#' Extracting the prior and posterior distributions from brmsfit-objects
#' 
#' This functions extract the posterior distribution values from a brmsfit-object and simulates the prior distribution values
#' 
#' @param object An object of class \code{brmsfit}.
#' @param var_names Optional a vector that includes new names for the effects (variables) that should be extracted.
#' @param post_effects Which effect should be extracted?
#' @param prior_means What are means of the prior distributions (needs to be in the same order as the post_effects)?
#' @param n_sim How many values should be simulated for the prior distributions (at best, this number is close to the effective sample size)
#' @return A tibble.
#' @export
prior_post_values <- function(object, 
                             var_names = NULL,
                             post_effects = NULL,
                             prior_means = NULL,
                             prior_sd = NULL,
                             n_sim = 10000) {
  
  library(tidyverse)
  
  n <- length(post_effects)
  
  temp <- object %>%
    as.tibble %>%
    select(post_effects) %>%
    mutate(type = "Posterior") %>%
    set_names(c(post_effects, "type"))
  
  output <- data.frame(V1 = 1:n_sim)
  
  for (i in 1:n){
    data <- rnorm(n_sim, prior_means[i], prior_sd[i]) 
    output[i] <- data
  }
  
  output <- output %>% 
    as.tibble %>%
    set_colnames(c(post_effects)) %>%
    mutate(type = "Prior")
  
  if (!is.null(var_names)) {
    temp <- bind_rows(output, temp) %>%
      set_colnames(c(var_names, "type"))
  }
  
  temp <- temp %>%
    gather(key, value, -type)
  
  return(temp)
}