calmr_ui <- function() {
  supported_models <- calmr::supported_models()
  shinydashboard::dashboardPage(
    skin = "red",
    shinydashboard::dashboardHeader(
      title = "Calmr Simulator",
      htmltools::tags$li(
        htmltools::a("Help",
          href = "https://victornavarro.org/calmr/articles/calmr_app.html",
          target = "_blank",
          title = "Help"
        ),
        class = "dropdown"
      )
    ),
    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Home", tabName = "home"),
        shinydashboard::menuItem("Options", tabName = "options"),
        shinydashboard::menuItem("About", tabName = "about")
      )
    ),
    shinydashboard::dashboardBody(
      shinydashboard::tabItems(
        # First tab content
        shinydashboard::tabItem(
          tabName = "home",
          shiny::fluidRow(
            # only tracks shinyapps website
            htmltools::tags$head(
              htmltools::includeHTML("google_analytics.html")
            ),
            shinydashboard::box(
              width = 12,
              title = "Design",
              htmltools::div(
                style = "float:left",
                shiny::actionButton(
                  inputId = "grouprm",
                  label = "Group-", class = "btn-s"
                ),
                shiny::actionButton(
                  inputId = "groupadd",
                  label = "Group+", class = "btn-s"
                ),
                shiny::actionButton(
                  inputId = "phaserm",
                  label = "Phase-", class = "btn-s"
                ),
                shiny::actionButton(
                  inputId = "phaseadd",
                  label = "Phase+", class = "btn-s"
                ),
                shiny::actionButton(
                  inputId = "parse_design",
                  label = "Parse Design", class = "btn-s"
                ),
                htmltools::div(
                  style = "display:inline-block;",
                  shiny::conditionalPanel(
                    "output.parsed",
                    shiny::actionButton(
                      inputId = "run_experiment",
                      label = "Run Experiment", class = "btn-s"
                    )
                  )
                ),
                htmltools::div(
                  style = "display:inline-block;",
                  shiny::conditionalPanel(
                    "output.ran",
                    shiny::downloadButton("exportresults", "Save Data",
                      icon = shiny::icon("file-download"), class = "btn-s"
                    )
                  )
                )
              ),
              htmltools::br(), htmltools::br(),
              rhandsontable::rHandsontableOutput("design_tbl", width = "100%"),
              htmltools::br()
            ),
            shinydashboard::box(
              collapsible = TRUE,
              width = 12,
              title = "Parameters",
              shiny::selectInput(
                inputId = "model_selection",
                label = "Model", choices = supported_models,
                selected = "ANCCR", multiple = FALSE
              ),
              shiny::conditionalPanel(
                "output.parsed",
                htmltools::h5("Stimulus-specific parameters")
              ),
              shiny::conditionalPanel(
                "output.parsed",
                rhandsontable::rHandsontableOutput(
                  "stim_par_tbl",
                  width = "100%"
                )
              ),
              shiny::conditionalPanel(
                "output.parsed && output.needs_globalpars",
                htmltools::h5("Global parameters")
              ),
              shiny::conditionalPanel(
                "output.parsed && output.needs_globalpars",
                rhandsontable::rHandsontableOutput(
                  "glob_par_tbl",
                  width = "100%"
                )
              ),
              shiny::conditionalPanel(
                "output.parsed && output.needs_trialpars",
                htmltools::h5("Trial-specific parameters")
              ),
              shiny::conditionalPanel(
                "output.parsed && output.needs_trialpars",
                rhandsontable::rHandsontableOutput(
                  "trial_par_tbl",
                  width = "100%"
                )
              ),
              shiny::conditionalPanel(
                "output.parsed && output.needs_transpars",
                htmltools::h5("Transition-specific parameters")
              ),
              shiny::conditionalPanel(
                "output.parsed && output.needs_transpars",
                rhandsontable::rHandsontableOutput(
                  "trans_par_tbl",
                  width = "100%"
                )
              )
            ),
            shinydashboard::box(
              collapsible = TRUE,
              width = 12,
              title = "Results",
              shiny::conditionalPanel(
                "output.ran",
                align = "center",
                shiny::selectInput(
                  inputId = "plot_selection",
                  label = NULL, choices = NA, multiple = TRUE
                ),
                shiny::plotOutput("plot")
              )
            ),
            shinydashboard::box(
              collapsible = TRUE,
              width = 12,
              title = "Association Graphs",
              shiny::conditionalPanel(
                "output.ran",
                shiny::sliderInput(
                  inputId = "graph_trial", label = "Trial",
                  min = 1, max = 1, value = 1, step = 1,
                  ticks = FALSE, width = "30%"
                ),
                shiny::plotOutput("graph")
              )
            )
          )
        ),
        # Options tab content
        shinydashboard::tabItem(
          tabName = "options",
          shiny::fluidPage(
            shinydashboard::box(
              collapsible = TRUE,
              width = NULL,
              title = "Simulation Options",
              shiny::sliderInput(
                inputId = "iterations",
                label = "Iterations", min = 1,
                max = 200, value = 1, ticks = FALSE
              ),
              shiny::checkboxInput(
                inputId = "miniblocks",
                label = "Create trial blocks",
                value = TRUE
              )
            ),
            shinydashboard::box(
              collapsible = TRUE,
              width = NULL,
              title = "Plotting Options",
              shiny::checkboxInput(
                inputId = "common_scale",
                label = "Plot in common scale", value = TRUE
              )
            )
          )
        ),
        shinydashboard::tabItem(
          tabName = "about",
          shiny::fluidPage(
            htmltools::HTML(
              '<center><img src="logo.png" width="20%"></center>'
            ),
            htmltools::br(), htmltools::br(),
            htmltools::HTML('Canonical Associative Learning Models
            and their Representations (calmr)
          is developed by <a href="https://victornavarro.org" target="_blank">
          Victor Navarro</a>.'),
            htmltools::br(), htmltools::br(),
            htmltools::HTML('To get access to the source code behind the package
          (and this app), head over to the
          <a href="https://github.com/victor-navarro/calmr"
          target="_blank">GitHub repository</a>.'),
            htmltools::br(), htmltools::br(),
            htmltools::HTML(
              'To consult the package documentation and other articles of
          interest, head over to the
          <a href="https://victornavarro.org/calmr/"
          target="_blank">package site</a>.'
            ),
            htmltools::br(), htmltools::br(),
            htmltools::p("Thanks for using the simulator."),
            htmltools::p("Victor")
          )
        )
      )
    )
  )
}
