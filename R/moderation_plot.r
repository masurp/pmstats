#' Interaction plot for continuous moderators
#' 
#' This functions creates a conditional effect plot that illustrates how an effect of x on y changes with changes in a continuous moderator variable. It also allows to plot a facetted version if the predictor variable x is a three-level factor.
#' 
#' @param object An object of class \code{"lm"} or \code{"lmer"} that includes an interaction.
#' @param x1 The name of the predictor variable. Can be numeric or the first level of a factor variable.
#' @param x2 If the factor variabel has more than two levels, this can be the second level.
#' @param m The name of the continuous moderator variable.
#' @param x_lab A character value specifying the x label in the final plot.
#' @param y_lab A character value specifying the y label in the final plot.
#' @param quantil Number of quantiles that should be calculated.
#' @param mod_hist A logical value indicating whether a histogram of the moderator variable should be added above the moderation plot. 
#' @return If mod_hist = TRUE, this function returns a "ggExtraPlot" (i.e. a "gtable"). If mod_hist = FALSE, it returns a ggplot object that can be further customized using standard ggplot elements.
#' @examples
#' # Simulate an interaction
#' set.seed(10)
#' x <- rnorm(500, 2, 1)
#' z <- rnorm(500, 2, 1)
#' y <- 0.5*x + 1.5*(z*x) + rnorm(500, 0, 3.5)
#'
#' # Estimate linear model
#' model <- lm(y ~ x + z + x:z)
#' summary(model)
#'
#' # Plot model
#' moderation_plot(model, x1 = "x", m = "z")
#' @export
moderation_plot <- function(object,
                            x1 = NULL,
                            x2 = NULL,
                            m = NULL,
                            x_lab = "Moderator",
                            y_lab = "Conditional effect of x on y",
                            quantil = 100,
                            mod_hist = TRUE){
  # dependencies
  library(tidyverse)
  library(ggExtra)
  
  if (is.null(x1) | is.null(m)) {
    message("You need to specify the predictor and moderator to plot the interaction.")
  }
  
  # subfunction
  conditional.effects <-  function(model, x, m, quantiles = quantil){
    interact = paste0(x,':',m)
    beta.hat = coef(model) 
    covs = vcov(model)
    z0  = quantile(model$model[,m], seq(0 , 1, 1/quantiles))
    dy.dx = beta.hat[x] + beta.hat[interact]*z0
    se.dy.dx = sqrt(covs[x, x] + z0^2*covs[interact, interact] + 2*z0*covs[x, interact])
    upr = dy.dx+1.96*se.dy.dx
    lwr = dy.dx-1.96*se.dy.dx
    data.frame(m = z0, b = dy.dx, lwr, upr)
  }
  
  # main function
  temp <- conditional.effects(object, 
                              x = x1, 
                              m = m) %>%
    as.tibble %>%
    mutate(grp = x1) 
  
  # Check if a second level factor variable is specified
  if (!is.null(x2)) {
    temp2 <- conditional.effects(object, 
                                 x = x2, 
                                 m = m) %>%
      as.tibble %>%
      mutate(grp = x2)
    
    temp <- bind_rows(temp, temp2)
    
  }
  
  # If not, print a classic interaction plot
  if (is.null(x2)) {
    plot <- ggplot(temp, 
                   aes(x = m,
                       y = b, 
                       ymin = lwr,
                       ymax = upr)) +
      geom_smooth(stat = "identity",
                  color = "black") +
      geom_hline(yintercept = 0, 
                 linetype = "dashed", 
                 color = "darkgrey") +
      theme_bw() +
      labs(x = x_lab,
           y = y_lab)
    
    # If mod_hist = TRUE, print marginal histogram
    if (isTRUE(mod_hist)) {
      plot <- ggMarginal(plot,            
                         margins = c("x"),
                         type = "histogram",
                         color = "white",
                         fill = "darkgrey",
                         size = 2)
    }
    
  # If a second factor level is specified, print facetted plot
  } else {
    
    plot <- ggplot(temp, 
                   aes(x = m,
                       y = b, 
                       ymin = lwr,
                       ymax = upr)) +
      geom_smooth(stat = "identity",
                  color = "black") +
      geom_hline(yintercept = 0, 
                 linetype = "dashed", 
                 color = "darkgrey") +
      facet_wrap(~grp) + 
      theme_bw() + 
      labs(x = x_lab,
           y = y_lab)
  }
  
  return(plot)
}

moderation_plot(model, "x", m = "z")
