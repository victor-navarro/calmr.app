# restart your RStudio/R
# please remember to install the latest version of calmr from your github page
devtools::install_github("victor-navarro/calmr.app", force = TRUE)
rsconnect::deployApp("C:/Users/Victor/calmr.app/inst/calm_app",
  forceUpdate = TRUE
)
