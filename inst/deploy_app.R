# # authenticate
rsconnect::setAccountInfo(
  name = Sys.getenv("SHINY_ACC_NAME"),
  token = Sys.getenv("TOKEN"),
  secret = Sys.getenv("SECRET")
)
# deploy
rsconnect::deployApp("calmr_app",
  appName = "calmr_app_test",
  forceUpdate = TRUE
)
