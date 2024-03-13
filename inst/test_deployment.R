devtools::install_github("victor-navarro/calmr.app", force = TRUE)
devtools::install_github("victor-navarro/calmr", force = TRUE)


# move files to temporary folder
tmp <- tempdir()
file.copy("R/server.R", file.path(tmp, "server.R"))
file.copy("R/ui.R", file.path(tmp, "ui.R"))
file.copy("inst/resources", tmp, recursive = TRUE)

shiny::addResourcePath(
  "resources",
  system.file("resources", package = "calmr.app")
)

rsconnect::deployApp(tmp,
  appName = "calmr_app_test",
  forceUpdate = TRUE
)
