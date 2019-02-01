#' Plotting distributions of several likert-type variables
#' 
#' This functions computes a facetted plot of variable distributions. Using the density function, the first layer is a histogram and the second a normal distributed density curve based on the mean and standard deviation of the respective variable. 
#'
#' 
#' @param data A data.frame containing all variables that should be plotted. 
#' @param levels Number of bins that should be plotted in the histogram. In most cases should be equal to number of answer options. 
#' @param basic A logical value indicating whether the stylized or basis version should be plotted (defaults to FALSE = stylized version). 
#' @export
desc_plot <- function(data, levels = 7, basic = FALSE){
  
  # Dependencies
  library(dplyr)
  library(ggplot2)
  
  # transform data
    d_p <- data %>% 
      gather(key, value)
  
    # plot data 
  if (basic == TRUE) {
  plot1 <- ggplot(d_p, aes(x = value)) + 
    geom_histogram(aes(y = stat(density)),
                   bins = levels) +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(d_p$value), sd = sd(d_p$value)), 
                  lwd = .5, 
                  col = 'black') +
    facet_wrap(~key)
  return(plot1)
  
  } else {
    
  plot2 <- ggplot(d_p, aes(x = value)) + 
    geom_histogram(aes(y = stat(density)), 
                   bins = levels,
                   fill = "#5e999b",
                   color ="white") +
    stat_function(fun = dnorm, 
                  args = list(mean = mean(d_p$value), sd = sd(d_p$value)), 
                  lwd = .5, 
                  col = 'black') +
    facet_wrap(~key) +
    labs(x = "Value",
         y = "Density")
  return(plot2)
  
  }
}
