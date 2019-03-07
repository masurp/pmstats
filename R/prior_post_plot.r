#' Plotting the prior and posterior distributions of a 
#' 
#' This functions takes the output of \code{prior_post_values.r} and creates overlaying density plots for each effect.
#' 
#' @param object A tibble resulting from the function \code{prior_post_values.r} or any similarly created tibble.
#' @param extra_key Logical value indicating whether there is more than one key to facet the plots (the second key needs to be named "key2").
#' @param scales Should the scales be the same of different?
#' @return A ggplot object.
#' @export
prior_post_plot <- function(object, 
                            extra_key = FALSE,
                            scales = "free"){
  
  # dependencies
  library(tidyverse)
  
  if (!isTRUE(extra_key)) {
    
    object %>% 
      ggplot(aes(x = value,
                 group = type,
                 fill = type)) + 
      geom_density(alpha = .50) +
      facet_wrap(~key, scales = "free")
    
  } else {
    
    object %>% 
      ggplot(aes(x = value,
                 group = type,
                 fill = type)) + 
      geom_density(alpha = .50) +
      facet_grid(key2~key, scales = "free")
  }
}