#' Explained variance for two-level random intercept models
#' 
#' Function to compute the amount explained variance on each level by a random-intercept multilevel model. 
#' The amount of explained variance is computed by comparing the relevant models to the baseline (null-) model (i.e., the bare random-intercept model without any predictors). 
#'
#' 
#' @param object An object of class merMod (more specifically, an object of subclass lmerMod). This first object needs to be the null model with no predictors. All other models will be compared to this baseline model. 
#' @param ... Up to four objects of class merMod.
#' @param red A logical value indicating wether only the explained variance should be reported (defaults to TRUE).
#' @return A data fram containing the explained variance on both levels.
#' @examples 
#' m0 <- lme4::lmer(Reaction ~ 1 + (1 | Subject), sleepstudy)
#' m1 <- lme4::lmer(Reaction ~ 1 + Days + (1 | Subject), sleepstudy)
#' 
#' variance_reduction(m0, m1, print = T, digits = 3)
#' @export
variance_reduction <- function(object, 
                               ...,
                               digits = 2, 
                               print = FALSE){
  
  # dependencies
  library(tidyverse)
  library(magrittr)
  library(papaja)
  
  # subfunction
  get_var <- function(model, no = 0){
    VarCorr(model) %>% 
      as.data.frame %>% 
      dplyr::select(grp, var = vcov) %>%
      magrittr::set_colnames(c("level", paste0("m", no, "_var")))
    }
    
  # create list of additinal models
  object_list <- list(...)
  
  # Check length of list
  if (length(object_list) == 0) {
    message("You need to compare at least two models to obtain the explained variance on each level!")
    
  } else if (length(object_list) == 1) {
  
    temp <- object %>% 
      get_var %>%
      left_join(
        object_list[[1]] %>%
          get_var(no = 1)
      ) %>% 
      mutate(m1 = 1 - m1_var / m0_var) %>%
      select(level, m1)
   
  } else if (length(object_list) == 2) {
    
    tab_2 <- object %>% 
      get_var %>%
      left_join(
        object_list[[1]] %>%
          get_var(no = 1)
      ) %>% 
      mutate(m1 = 1 - m1_var / m0_var) 
    
    temp <- tab_2 %>%
      left_join(
        object_list[[2]] %>%
          get_var(no = 2)
        ) %>%
      mutate(m2 = 1 - m2_var / m0_var) %>%
      select(level, m1, m2) 
    
  } else if (length(object_list) == 3) {
    
    tab_2 <- object %>% 
      get_var %>%
      left_join(
        object_list[[1]] %>%
          get_var(no = 1)
      ) %>% 
      mutate(m1 = 1 - m1_var / m0_var) 
    
    tab_3 <- tab_2 %>%
      left_join(
        object_list[[2]] %>%
          get_var(no = 2)
      ) %>%
      mutate(m2 = 1 - m2_var / m0_var)
    
    temp <- tab_3 %>%
      left_join(
        object_list[[3]] %>%
          get_var(no = 3)
      ) %>%
      mutate(m3 = 1 - m3_var / m0_var) %>%
      select(level, m1, m2, m3) 
    
  } else {
    
    tab_2 <- object %>% 
      get_var %>%
      left_join(
        object_list[[1]] %>%
          get_var(no = 1)
      ) %>% 
      mutate(m1 = 1 - m1_var / m0_var) 
    
    tab_3 <- tab_2 %>%
      left_join(
        object_list[[2]] %>%
          get_var(no = 2)
      ) %>%
      mutate(m2 = 1 - m2_var / m0_var)
    
    tab_4 <- tab_3 %>%
      left_join(
        object_list[[3]] %>%
          get_var(no = 3)
      ) %>%
      mutate(m3 = 1 - m3_var / m0_var)
    
    temp <- tab_4 %>%
      left_join(
        object_list[[4]] %>%
          get_var(no = 4)
      ) %>%
      mutate(m4 = 1 - m4_var / m0_var) %>%
      select(level, m1, m2, m3, m4) 
  }
  
  temp <- temp %>% 
    mutate_if(is.numeric, 
              round, 
              digits = digits) %>%
    as.tibble
  
  if (print == TRUE) {
    temp <- temp %>%
      mutate_if(is.numeric, 
                printnum, 
                gt1 = TRUE, 
                digits = digits)
  }
  return(temp)
}
