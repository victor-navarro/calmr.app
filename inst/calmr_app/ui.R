supported_models <- calmr::supported_models()

rhandsontable_css <- ".handsontable {
    overflow: hidden;
}"

bslib::page_navbar(
  title = "Calmr Simulator",
  bg = "#d3374a",
  inverse = TRUE,
  bslib::nav_panel(
    title = "Home",
    shinyjs::useShinyjs(),
    htmltools::tags$head(
      htmltools::includeHTML("google_analytics.html")
    ),
    htmltools::tags$style(
      htmltools::HTML(rhandsontable_css)
    ),
    bslib::card(
      bslib::layout_sidebar(
        sidebar = bslib::accordion(
          id = "sidemenu",
          multiple = FALSE,
          bslib::accordion_panel(
            "Model",
            shiny::selectInput(
              inputId = "model_selection",
              label = NULL, choices = supported_models,
              selected = "RW1972", multiple = FALSE
            )
          ),
          bslib::accordion_panel(
            "Filters",
            shiny::uiOutput("filters")
          ),
          bslib::accordion_panel(
            "Options",
            shiny::sliderInput(
              inputId = "iterations",
              label = "Iterations", min = 1,
              max = 200, value = 1, ticks = FALSE
            ),
            # shiny::checkboxInput(
            #   inputId = "miniblocks",
            #   label = "Create trial blocks",
            #   value = TRUE
            # ),
            shiny::selectizeInput(
              inputId = "plotting_palette",
              label = "Plots palette",
              choices = c("Viridis", "Hue")
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
            rhandsontable::rHandsontableOutput("design_tbl")
          )
        ),
        shiny::conditionalPanel(
          "output.parsed",
          bslib::card(
            bslib::card_header("Parameters"),
            full_screen = TRUE,
            shiny::uiOutput("parameter_ui")
          )
        ),
        shiny::conditionalPanel(
          "output.ran",
          bslib::card(
            bslib::card_header("Results"),
            full_screen = TRUE,
            shiny::uiOutput("results_panel")
          )
        ),
        shiny::conditionalPanel(
          "output.ran",
          bslib::card(
            bslib::card_header("Association Graphs"),
            full_screen = TRUE,
            shiny::uiOutput("graphs_panel")
          )
        ), fillable = FALSE
      )
    )
  ),
  # bslib::nav_panel(
  #   title = "Help",
  #   "Hello"
  # ),
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
  bslib::nav_item(htmltools::tags$a("Help",
    href = "https://victornavarro.org/calmr/articles/calmr_app.html"
  )),
  bslib::nav_spacer(),
  bslib::nav_menu(
    title = "Links",
    align = "right",
    bslib::nav_item(htmltools::tags$a("calmr on GitHub",
      href = "https://github.com/victor-navarro/calmr"
    )),
    bslib::nav_item(htmltools::tags$a("calmr.app on Github",
      href = "https://github.com/victor-navarro/calmr"
    )),
    bslib::nav_item(htmltools::tags$a("calmr on CRAN",
      href = "https://cran.r-project.org/web/packages/calmr/"
    ))
  )
)
