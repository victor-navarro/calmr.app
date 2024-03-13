# restart your R session
# please remember to install the latest version of calmr and calmr.app
devtools::install_github("victor-navarro/calmr.app", force = TRUE)
devtools::install_github("victor-navarro/calmr", force = TRUE)

rsconnect::deployApp("C:/Users/sapvn2/calmr.app/inst/calmr_app",
  forceUpdate = TRUE
)
