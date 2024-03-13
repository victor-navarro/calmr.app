#' Run the calm shiny application
#' @param browser If TRUE, the app is launched in a browser.
#' @export
launch_app <- function(
    browser = getOption("shiny.launch.browser", interactive())) {
  app_dir <- system.file("calmr_app", package = "calmr.app")
  shiny::runApp(app_dir,
    display.mode = "normal", launch.browser = browser
  )
}
