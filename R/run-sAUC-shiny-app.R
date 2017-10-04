#' @name run_sAUC_shiny_app
#'
#' @export
#'
#' @title Run Shiny application built for sAUC package
#'
#' @description This function allows users to run Shiny application built for sAUC package
#' where users can perform sAUC data analysis and run simulation for one predictor
#'
#' @author Som Bohora

run_sAUC_shiny_app <- function() {
  app_dir <- system.file("shiny-examples", "shiny-app", package = "sAUC")
  if (app_dir == "") {
    stop("Could not find example directory. Try re-installing `sAUC`.", call. = FALSE)
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
