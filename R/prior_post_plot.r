#' Plotting the prior and posterior distributions of a 
#' 
#' This functions takes the output of \code{\link[pmstats]{prior_post_values}} and creates overlaying density plots for each effect.
#' 
#' @param object A data frame resulting from the function \code{\link[pmstats]{prior_post_values}} or any similarly created data frame.
#' @param extra_key Logical value indicating whether there is more than one key to facet the plots (note: if you used the function \code{\link[pmstats]{prior_post_values}} to get the values and did not modify it further, the first key is automatically called "key". A potential second key needs to be named "key2").
#' @param scales Should scales be fixed ("fixed"), free ("free", the default), or free in one dimension ("free_x", "free_y")?
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