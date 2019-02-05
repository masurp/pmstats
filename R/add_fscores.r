#'Adds factor scores computed from CFA to data frame
#'
#'Creates and adds factor scores to the data frame that was used to estimate the CFA. 
#'
#'@param data The data frame that was originally passed to \code{cfa()} or \code{sem()}.
#'@param id Variable that identifies each individual case in the data frame. Only necessary if the data frame is in the long format. 
#'@param object An object of class \code{lavaan} created by using \code{cfa()}
#'  or \code{sem()}.
#'@param to_wide A logical value indicating whether the multigroup factor scores should be passed to long or to a wide format data frame.
#'@param group A vector representing the grouping variable in the long format that was passed to data.
#'@param rename_vars A vector that represents new names for the factor scores.
#' @export
add_fscores <- function(data,
                        id = NULL,
                        object,
                        to_wide = FALSE,
                        group = NULL,
                        rename_vars = NULL){
  # dependencies
  library(lavaan)
  library(tidyverse)
  
  fscores_raw <- predict(object)
  
  if (class(fscores_raw) == "list") {
    fscores <- fscores_raw %>%
      lapply(., as.data.frame) %>%
      lapply(., function(x) rownames_to_column(x, "pers_id")) %>%
      bind_rows(., .id = "grp") %>%
      as.tibble %>% 
      mutate(pers_id = as.numeric(pers_id)) %>%
      mutate(grp2 = rep(group, each = nrow(.)/length(group))) %>%
      select(-grp) %>%
      arrange(pers_id, grp2)
  } else {
    fscores <- predict(object)
  }
  
  if (isTRUE(to_wide)) {
    fscores <- fscores %>%
      gather(key, value, -grp2, -pers_id) %>%
      as.tibble %>%
      unite(vars, c("grp2", "key")) %>%
      spread(vars, value) %>%
      arrange(pers_id)
  }
  
  if (!is.null(rename_vars)) {
    colnames(fscores) <- rename_vars
  }
  
  if (class(fscores_raw) == "list" & isTRUE(to_wide)) {
    temp <- data %>% arrange(id) %>%
      cbind(., fscores) %>%
      select(-pers_id) %>%
      as.tibble
  } else if (class(fscores_raw) == "list" & !isTRUE(to_wide)){
    temp <- data %>% arrange(id) %>%
      cbind(., fscores) %>%
      select(-pers_id, -grp2) %>%
      as.tibble
  } else {
    temp <- data %>%
      cbind(., fscores) %>%
      as.tibble
  }
  return(temp)
}