#' Interaction plot for continuous moderators
#' 
#' This functions creates a conditional effect plot that illustrates how an effect of x on y changes with changes in a continuous moderator variable. 
#' 
#' @param object An object of class \code{\link[base]{lm}} or \code{\link[lme4]{lmer}} that includes an interaction.
#' @param x The name of the predictor variable.
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
#' mod.lm <- lm(y ~ x + z + x:z)
#' summary(mod.lm)
#'
#' # Plot model
#' moderation_plot(mod.lm, x = "x", m = "z")
#' @export
moderation_plot <- function(object,
                            x = NULL,
                            m = NULL,
                            x_lab = "Moderator",
                            y_lab = "Conditional effect of x on y",
                            quantil = 100,
                            mod_hist = TRUE){
  # dependencies
  library(tidyverse)
  library(ggExtra)
  
  if (is.null(x) | is.null(m)) {
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
  temp <- conditional.effects(model = object, 
                              x = x, 
                              m = m) %>%
    as.tibble %>%
    mutate(grp = x) 
  
  # Print a classic interaction plot
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
  
  return(plot)
}
