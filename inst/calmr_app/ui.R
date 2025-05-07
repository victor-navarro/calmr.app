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
              "output.parsed && output.needs_global_timings",
              htmltools::h5("Global timing parameters")
            ),
            shiny::conditionalPanel(
              "output.parsed && output.needs_global_timings",
              rhandsontable::rHandsontableOutput(
                "trial_par_tbl",
                width = "100%"
              )
            ),
            shiny::conditionalPanel(
              "output.parsed && output.needs_trial_timings",
              htmltools::h5("Trial-specific parameters")
            ),
            shiny::conditionalPanel(
              "output.parsed && output.needs_trial_timings",
              rhandsontable::rHandsontableOutput(
                "trial_par_tbl",
                width = "100%"
              )
            ),
            shiny::conditionalPanel(
              "output.parsed && output.needs_period_timings",
              htmltools::h5("Period-specific parameters")
            ),
            shiny::conditionalPanel(
              "output.parsed && output.needs_period_timings",
              rhandsontable::rHandsontableOutput(
                "period_par_tbl",
                width = "100%"
              )
            ),
            shiny::conditionalPanel(
              "output.parsed && output.needs_transition_timings",
              htmltools::h5("Transition-specific parameters")
            ),
            shiny::conditionalPanel(
              "output.parsed && output.needs_transition_timings",
              rhandsontable::rHandsontableOutput(
                "trans_par_tbl",
                width = "100%"
              )
            )
          ),
          bslib::accordion_panel(
            "Filters",
            shiny::uiOutput("tut_filters"),
            shiny::uiOutput("filters")
          ),
          bslib::accordion_panel(
            "Options",
            shiny::uiOutput("tut_options"),
            shiny::sliderInput(
              inputId = "iterations",
              label = "Iterations", min = 1,
              max = 200, value = 1, ticks = FALSE
            ),
            shiny::checkboxInput(
              inputId = "miniblocks",
              label = "Create trial blocks",
              value = TRUE
            ),
            shiny::selectizeInput(
              inputId = "plotting_palette",
              label = "Plots palette",
              choices = c("Viridis", "Hue")
            )
          ),
          bslib::card(
            class = "accordion-item",
            shiny::checkboxInput(
              inputId = "tutorial_mode",
              label = "Show help",
              value = FALSE
            )
          ),
          shiny::imageOutput("logo",
            width = "10vw", height = "auto",
            inline = TRUE, fill = FALSE
          )
        ),
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("Design"),
          bslib::card_body(
            fillable = FALSE,
            shiny::uiOutput("tut_design1"),
            shiny::actionButton(
              inputId = "grouprm",
              label = "Group-", class = "xs"
            ),
            shiny::actionButton(
              inputId = "groupadd",
              label = "Group+"
            ),
            shiny::actionButton(
              inputId = "phaserm",
              label = "Phase-"
            ),
            shiny::actionButton(
              inputId = "phaseadd",
              label = "Phase+"
            ),
            shiny::actionButton(
              inputId = "parse_design",
              label = "Parse Design"
            ),
            shiny::actionButton(
              inputId = "run_experiment",
              label = "Run Experiment",
              disabled = TRUE
            ),
            shiny::downloadButton("export_results", "Save Results",
              icon = shiny::icon("file-download")
            ),
            htmltools::br(),
            htmltools::br(),
            rhandsontable::rHandsontableOutput("design_tbl"),
            shiny::uiOutput("tut_design2")
          )
        ),
        shiny::conditionalPanel(
          "output.parsed",
          bslib::card(
            bslib::card_header("Parameters"),
            full_screen = TRUE,
            shiny::uiOutput("tut_parameters"),
            shiny::uiOutput("parameter_ui")
          )
        ),
        shiny::conditionalPanel(
          "output.ran",
          bslib::card(
            bslib::card_header("Results"),
            full_screen = TRUE,
            shiny::uiOutput("tut_results"),
            shiny::uiOutput("results_panel")
          )
        ),
        shiny::conditionalPanel(
          "output.ran",
          bslib::card(
            bslib::card_header("Association Graphs"),
            full_screen = TRUE,
            shiny::uiOutput("tut_graphs"),
            shiny::uiOutput("graphs_panel")
          )
        ), fillable = FALSE
      )
    )
  ),
  bslib::nav_panel(
    title = "About",
    bslib::layout_column_wrap(
      width = 1 / 3,
      NULL,
      bslib::card(
        width = "20%",
        htmltools::HTML(
          '<center><img src="logo.png" width="25%"></center>'
        ),
        shiny::markdown(
          "Canonical Associative Learning Models
        and their Representations (calmr)
        is developed by [Victor Navarro](https://victornavarro.org).

        To get access to the source code behind the package
          (and this app), head over to the
          [github repository](https://github.com/victor-navarro/calmr).

        To consult the package documentation and other articles of
          interest, head over to the
          [package site](https://victornavarro.org/calmr/).


        Thanks for using the simulator."
        )
      )
    )
  ),
  bslib::nav_spacer(),
  bslib::nav_menu(
    title = "Links",
    align = "right",
    bslib::nav_item(htmltools::tags$a("calmr on GitHub",
      href = "https://github.com/victor-navarro/calmr"
    )),
    bslib::nav_item(htmltools::tags$a("calmr.app on Github",
      href = "https://github.com/victor-navarro/calmr.app"
    )),
    bslib::nav_item(htmltools::tags$a("calmr on CRAN",
      href = "https://cran.r-project.org/web/packages/calmr/"
    ))
  )
)
