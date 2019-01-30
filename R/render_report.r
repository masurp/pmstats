#' Creates an html report based on a .rmd file
#' 
#' This function simply allows to render rmd. files into html reports. 
#' To be used in a "run_all.r" syntax.
#' 
#' @param file path to the .rmd file that should be rendered
#' @param name name for the final report
#' @export
render_report = function(file, name) {
  rmarkdown::render(
    file, params = list(
      name = name,
      numner = number
    ),
    output_file = paste0(name, ".html")
  )
}
