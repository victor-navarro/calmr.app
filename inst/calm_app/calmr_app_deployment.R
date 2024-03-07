# restart your RStudio/R
# please remember to install the latest version of calm from your github page
devtools::install_github("victor-navarro/calm.app", force = TRUE)
rsconnect::deployApp("C:/Users/sapvn2/calm.app/inst/calm_app",
  forceUpdate = TRUE
)
