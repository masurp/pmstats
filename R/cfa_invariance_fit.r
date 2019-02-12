#' Compare fit of different CFA models
#' 
#' @param ... Four lavaan fit objects with base, weak, strong and strict invariance constraints.
#' @export


cfa_invariance_fit <- function(...){
  
  lapply(list(...), function(x) fit_table(x, reliability = FALSE)) %>%
    bind_rows
}
