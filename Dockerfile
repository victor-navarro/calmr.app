FROM rocker/r-ver:4.2.0

Label MAINTAINER Victor Navarro

RUN install2.r rsconnect htmltools openxlsx rhandsontable shiny shinyalert shinyjs bslib plotly remotes
RUN installGithub.r victor-navarro/calmr victor-navarro/calmr.app
WORKDIR /home/calmr.app
COPY inst/deploy_app.R deploy_app.R
COPY inst/calmr_app calmr_app
CMD Rscript deploy_app.R