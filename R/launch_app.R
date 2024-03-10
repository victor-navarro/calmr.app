#' Run the calmr shiny application
#' @param browser If TRUE, the app is launched in a browser.
#' @export

launch_app <- function(
    browser = getOption("shiny.launch.browser", interactive())) {
  app_dir <- system.file("calmr_app", package = "calmr.app")
  if (app_dir == "") {
    stop("Could not find app directory.
    Try re-installing `calmr.app`.
    If all else fails, let the maintainer know and use the online app.")
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
