devtools::install_github("victor-navarro/calmr.app", force = TRUE)
devtools::install_github("victor-navarro/calmr", force = TRUE)

rsconnect::deployApp(tmp,
  appName = "calmr_app_test",
  forceUpdate = TRUE
)
