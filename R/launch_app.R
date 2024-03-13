#' Run the calm shiny application
#' @param browser If TRUE, the app is launched in a browser.
#' @export
launch_app <- function(
    browser = getOption("shiny.launch.browser", interactive())) {
  shiny::addResourcePath(
    "resources",
    system.file("app_resources", package = "calmr.app")
  )
  ui <- calmr_ui()
  server <- calmr_server()
  shiny::runApp(list(ui = ui, server = server),
    display.mode = "normal", launch.browser = browser
  )
}
