#' Variance reduction for two-level random intercept models
#' 
#' Function to compute the amount explained variance on each level by a random-intercept multilevel model. 
#' The amount of explained variance is computed by comparing the relevant models to the baseline (null-) model (i.e., the bare random-intercept model without any predictors). 
#'
#' 
#' @param base_model baseline model with no predictors
#' @param ... up to four comparison models with predictors
#' @param red a logical value indicating wether only the explained variance should be reported (defaults to TRUE).
#' @export
variance_reduction <- function(base_model, ..., red = TRUE){
  
  # Subfunction
  get_var <- function(model, no = 0){
    VarCorr(model) %>% 
      as.data.frame %>% 
      dplyr::select(grp, var = vcov) %>%
      magrittr::set_colnames(c("level", paste0("m", no, "_var")))
    }
    
  # create list of additinal models
  x <- list(...)
  
  # Check length of list
  if (length(x) == 0) {
    print("You need to compare at least two models to obtain the variance reduction")
    
  } else if (length(x) == 1) {
  
    tab_2 <- base_model %>% 
      get_var %>%
      left_join(
        x[[1]] %>%
          get_var(no = 1)
      ) %>% 
      mutate(m1_red = 1 - m1_var / m0_var) 
    if (red == TRUE) {
      tab_2 %>%
        dplyr::select(level, 
                      contains("red"))
    } else {
      tab_2 %>%
        dplyr::select(level,
                      m0_var, m1_var,
                      m1_red)
    }
   
  } else if (length(x) == 2) {
    
    tab_2 <- m0 %>% 
      get_var %>%
      left_join(
        x[[1]] %>%
          get_var(no = 1)
      ) %>% 
      mutate(m1_red = 1 - m1_var / m0_var) 
    
    tab_3 <- tab_2 %>%
      left_join(
        x[[2]] %>%
          get_var(no = 2)
        ) %>%
      mutate(m2_red = 1 - m2_var / m0_var) 
    
    if (red == TRUE) {
      tab_3 %>%
        dplyr::select(level, 
               contains("red"))
    } else {
      tab_3 %>%
        dplyr::select(level,
                      m0_var, m1_var, m2_var,
                      m1_red, m2_red)
    }
  } else if (length(x) == 3) {
    tab_2 <- m0 %>% 
      get_var %>%
      left_join(
        x[[1]] %>%
          get_var(no = 1)
      ) %>% 
      mutate(m1_red = 1 - m1_var / m0_var) 
    
    tab_3 <- tab_2 %>%
      left_join(
        x[[2]] %>%
          get_var(no = 2)
      ) %>%
      mutate(m2_red = 1 - m2_var / m0_var)
    
    tab_4 <- tab_3 %>%
      left_join(
        x[[3]] %>%
          get_var(no = 3)
      ) %>%
      mutate(m3_red = 1 - m3_var / m0_var)
    
    if (red == TRUE) {
      tab_4 %>%
        dplyr::select(level, 
                      contains("red"))
    } else {
      tab_4 %>%
        dplyr::select(level,
                      m0_var, m1_var, m2_var, m3_var,
                      m1_red, m2_red, m3_red)
    }
  } else {
    tab_2 <- m0 %>% 
      get_var %>%
      left_join(
        x[[1]] %>%
          get_var(no = 1)
      ) %>% 
      mutate(m1_red = 1 - m1_var / m0_var) 
    
    tab_3 <- tab_2 %>%
      left_join(
        x[[2]] %>%
          get_var(no = 2)
      ) %>%
      mutate(m2_red = 1 - m2_var / m0_var)
    
    tab_4 <- tab_3 %>%
      left_join(
        x[[3]] %>%
          get_var(no = 3)
      ) %>%
      mutate(m3_red = 1 - m3_var / m0_var)
    
    tab_5 <- tab_4 %>%
      left_join(
        x[[4]] %>%
          get_var(no = 4)
      ) %>%
      mutate(m4_red = 1 - m4_var / m0_var)
    
    if (red == TRUE) {
      tab_5 %>%
        dplyr::select(level, 
                      contains("red"))
    } else {
      tab_5 %>%
        dplyr::select(level,
                      m0_var, m1_var, m2_var, m3_var, m4_var,
                      m1_red, m2_red, m3_red, m4_red)
    }
  }
}
