FROM rocker/shiny
# RUN install2.r rsconnect htmltools openxlsx rhandsontable shiny shinyalert shinydashboard
WORKDIR /home/calmr.app
COPY inst/deploy_app.R deploy_app.R
CMD Rscript deploy_app.R