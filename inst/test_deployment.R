devtools::install_github("victor-navarro/calmr.app", force = TRUE)
devtools::install_github("victor-navarro/calmr", force = TRUE)

shiny::addResourcePath(
  "resources",
  system.file("app_resources", package = "calmr.app")
)
ui <- calmr.app::calmr_ui(
  analytics_file =
    "inst/app_resources/google_analytics.html"
)
server <- calmr.app::calmr_server()

rsconnect::deployApp(list(ui = ui, server = server),
  forceUpdate = TRUE
)
