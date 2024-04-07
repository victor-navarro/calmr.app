library(calmr.app)
# get welcome design
base_df <- calmr::get_design("controlled_blocking")
# whether to print debugging messages
debug <- FALSE
# some options
base_sim_options <- list(iterations = 1, miniblocks = TRUE)


shiny::shinyServer(function(input, output) { # nolint: cyclocomp_linter.
  #### Reactive values ####
  design_df <- shiny::reactiveVal(base_df)
  design <- shiny::reactiveVal()
  current_parameters <- shiny::reactiveVal()
  current_timings <- shiny::reactiveVal()
  par_tables <- shiny::reactiveVal()
  plots <- shiny::reactiveVal()
  current_plot1 <- shiny::reactiveVal()
  current_plot2 <- shiny::reactiveVal()
  graphs <- shiny::reactiveVal()
  current_graph1 <- shiny::reactiveVal()
  current_graph2 <- shiny::reactiveVal()
  sim_options <- shiny::reactiveVal(base_sim_options)
  parsed <- shiny::reactiveVal(FALSE)
  needs_globalpars <- shiny::reactiveVal(FALSE)
  needs_timings <- shiny::reactiveVal(FALSE)
  ran <- shiny::reactiveVal(FALSE)
  experiment <- shiny::reactiveVal()
  # a copy of the experiment for plotting
  plot_experiment <- shiny::reactiveVal()
  # available options for filtering
  avail_phases <- shiny::reactiveVal()
  avail_trials <- shiny::reactiveVal()
  avail_stimuli <- shiny::reactiveVal()

  #### Input Logic ####
  shiny::observeEvent(input$groupadd, {
    if (debug) print("adding group")
    df <- rhandsontable::hot_to_r(input$design_tbl)
    df[nrow(df) + 1, ] <- df[nrow(df), ]
    df[nrow(df), 1] <- paste("Group", nrow(df))
    design_df(df)
    parsed(FALSE)
    ran(FALSE)
  })
  shiny::observeEvent(input$grouprm, {
    if (debug) print("removing group")
    df <- rhandsontable::hot_to_r(input$design_tbl)
    if (nrow(df) > 1) {
      df <- df[1:(nrow(df) - 1), ]
      design_df(df)
      parsed(FALSE)
      ran(FALSE)
    }
  })
  shiny::observeEvent(input$phaseadd, {
    if (debug) print("adding phase")
    df <- rhandsontable::hot_to_r(input$design_tbl)
    cols <- ncol(df) - 1
    df[, paste0("P", cols / 2 + 1)] <- ""
    df[, paste0("R", cols / 2 + 1)] <- TRUE
    design_df(df)
    parsed(FALSE)
    ran(FALSE)
  })
  shiny::observeEvent(input$phaserm, {
    if (debug) print("removing phase")
    df <- rhandsontable::hot_to_r(input$design_tbl)
    if (ncol(df) > 3) {
      df <- df[, 1:(ncol(df) - 2)]
      design_df(df)
      parsed(FALSE)
      ran(FALSE)
    }
  })
  shiny::observeEvent(input$model_selection, {
    if (debug) print("reset due to model selection")
    if (!(input$model_selection %in% calmr::supported_timed_models())) {
      current_timings(NULL)
    }
    parsed(FALSE)
    ran(FALSE)
  })
  shiny::observeEvent(input$parse_design, {
    # get old stimuli (for parameter retention)
    if (debug) print("parsing")
    # parse design_df
    design_df(rhandsontable::hot_to_r(input$design_tbl))
    design(calmr::parse_design(design_df()))
    # get parameters
    # TODO: keep parameters if there are compatible parameters already
    if (debug) print("getting parameters")
    new_params <- calmr::get_parameters(
      design(),
      model = input$model_selection
    )
    current_parameters(new_params)

    # flips needs_timings if necessary
    needs_timings(
      input$model_selection %in%
        calmr::supported_timed_models()
    )
    if (needs_timings()) {
      if (debug) print("getting timings")
      current_timings(calmr::get_timings(design()))
    }
    # make parameter tables
    par_tables(calmr.app:::.make_par_tables(
      model = input$model_selection,
      parameters = current_parameters(),
      timings = current_timings()
    ))

    # flip needs_globalpars if necessary
    needs_globalpars(calmr.app:::.check_globalpars(
      input$model_selection,
      current_parameters()
    ))

    if (debug) print("done with parameters")
    # flip parsed
    parsed(TRUE)
  })

  shiny::observeEvent(input$run_experiment, {
    if (debug) print("running experiment")
    tryCatch(
      {
        # expected experiment size
        n <- length(unique(design()@design$group)) * input$iterations
        # create a callback function for sampling progress
        args_call <- function() {
          shiny::incProgress(
            1 / n
          )
        }

        if (debug) {
          print("running with parameters...")
          print(current_parameters())
          print("running with timings...")
          print(current_timings())
        }

        shiny::withProgress(message = "Sampling trials...", value = 0, {
          experiment <- calmr::make_experiment(
            design(),
            model = input$model_selection,
            parameters = current_parameters(),
            timings = current_timings(),
            iterations = sim_options()$iterations,
            .callback_fn = args_call
          )
        })
        # create a callback function for running progress
        run_call <- function() {
          shiny::incProgress(
            1 / n
          )
        }
        # run the experiment
        shiny::withProgress(message = "Simulating...", value = 0, {
          experiment <- calmr::run_experiment(experiment,
            aggregate = FALSE,
            .callback_fn = run_call
          )
        })
        # create a callback function for aggregation progress
        n_outputs <- length(calmr::model_outputs(experiment@model))
        agg_call <- function() {
          shiny::incProgress(
            1 / n_outputs
          )
        }
        if (debug) print("experiment ran")
        shiny::withProgress(message = "Aggregating results...", value = 0, {
          experiment <- calmr::aggregate(experiment)
        })
        if (debug) print("experiment aggregated")
        experiment(experiment)
        plot_experiment(experiment) # make a copy for plotting
        ran(TRUE)
        # toggle filters
        bslib::accordion_panel_open(id = "sidemenu", "Filters")
      },
      error = function(x) {
        print(x)
        shinyalert::shinyalert(
          title = "Error!",
          text = "Something went wrong. Please check your design/parameters.",
          size = "s", closeOnEsc = TRUE,
          closeOnClickOutside = TRUE, html = FALSE,
          type = "error", showConfirmButton = TRUE, showCancelButton = FALSE,
          confirmButtonText = "OK", confirmButtonCol = "#AEDEF4"
        )
      }
    )
  })

  #### Options Logic ####
  shiny::observeEvent(input$iterations, {
    if (debug) print("changing iterations option")
    sopts <- sim_options()
    sopts$iterations <- input$iterations
    sim_options(sopts)
  })

  # TODO: Breaking somehow
  shiny::observeEvent(input$miniblocks, {
    if (debug) print("changing miniblocks option")
    sopts <- sim_options()
    sopts$miniblocks <- input$miniblocks
    sim_options(sopts)
  })

  #### Observers ####
  # enabling the run experiment button
  shiny::observeEvent(parsed(), {
    shiny::updateActionButton(
      inputId = "run_experiment",
      disabled = !parsed()
    )
  })

  # populating the phase options upon design change
  shiny::observeEvent(design(), {
    if (debug) print("populating phase options due to design change")
    if (!is.null(design())) {
      avail_phases(unique(sapply(design()@design, "[[", "phase")))
      avail_trials(design()@mapping$trial_names)
      avail_stimuli(design()@mapping$unique_functional_stimuli)
    }
  })

  # populating the plot selections upon plots change
  shiny::observeEvent(plots(), {
    if (!is.null(plots())) {
      if (debug) print("populating plot selections")
      pnames <- names(plots())
      # we must check whether something is already selected
      if (!(input$plot1_sel %in% pnames)) {
        pn1 <- pnames[1]
      } else {
        pn1 <- input$plot1_sel
      }
      shiny::updateSelectizeInput(
        inputId = "plot1_sel",
        selected = pn1,
        choices = pnames
      )
      if (!(input$plot2_sel %in% pnames)) {
        pn2 <- pnames[2]
      } else {
        pn2 <- input$plot2_sel
      }
      shiny::updateSelectizeInput(
        inputId = "plot2_sel",
        selected = pn2,
        choices = pnames
      )
    }
  })

  # make plots on plot_experiment change
  shiny::observeEvent(plot_experiment(), {
    if (!is.null(plot_experiment())) {
      if (debug) print("making plots")
      shiny::withProgress(message = "Making plots...", {
        plots(calmr::plot(plot_experiment()))
        shiny::setProgress(1)
      })
    }
  })

  # putting current plots on plots() change
  shiny::observeEvent(plots(), {
    if (debug) print("putting plots based on selections")
    # make selection
    if (input$plot1_sel != "") {
      current_plot1(plots()[[input$plot1_sel]])
    } else {
      current_plot1(plots()[[1]])
    }
    if (input$plot2_sel != "") {
      current_plot2(plots()[[input$plot2_sel]])
    } else {
      current_plot2(plots()[[2]])
    }
  })

  # remaking plots on colour/fill scale change
  shiny::observeEvent(input$plotting_palette, {
    if (input$plotting_palette != "") {
      if (debug) print("changing plotting palette")
      calmr::set_calmr_palette(tolower(input$plotting_palette))
      # replot if ready
      if (!is.null(plot_experiment())) {
        if (debug) print("making plots")
        shiny::withProgress(message = "Making plots...", {
          plots(calmr::plot(plot_experiment()))
          shiny::setProgress(1)
        })
      }
    }
  })

  # remaking plot1 on selection change
  shiny::observeEvent(input$plot1_sel, {
    if (debug) print("remaking plot1 due to selection change")
    if (input$plot1_sel != "") {
      new_plots <- calmr::plot(experiment())
      current_plot1(new_plots[[input$plot1_sel]])
    }
  })

  # remaking plot2 on selection change
  shiny::observeEvent(input$plot2_sel, {
    if (debug) print("remaking plot2 due to selection change")
    if (input$plot2_sel != "") {
      new_plots <- calmr::plot(experiment())
      current_plot2(new_plots[[input$plot2_sel]])
    }
  })

  # filtering plot data on filter change
  shiny::observeEvent(list(
    input$phase_selection,
    input$trial_selection,
    input$stim_selection
  ), {
    filters <- list(
      phase_f = input$phase_selection,
      trial_f = input$trial_selection,
      stim_f = input$stim_selection
    )
    if (all(!sapply(filters, is.null))) {
      if (debug) print("filtering data upon filter change")
      plot_experiment(calmr.app:::.filter_experiment(
        experiment(),
        filters
      ))
    }
  })

  # make graph on experiment run
  shiny::observeEvent(experiment(), {
    if (!is.null(experiment())) {
      if (debug) print("making graphs")
      shiny::withProgress(message = "Making graphs...", {
        graphs(unlist(unname(calmr::graph(experiment())),
          recursive = FALSE
        ))
        current_graph1(graphs()[1])
        current_graph2(graphs()[1])
        shiny::setProgress(1)
      })
    }
  })

  # remaking graph1 on trial change
  shiny::observeEvent(input$graph1_trial, {
    if (!is.null(current_graph1())) {
      if (debug) print("remaking graph1 due to slider change")
      if (input$graph1_sel != "") {
        new_graphs <- unlist(
          unname(calmr::graph(experiment(),
            t = input$graph1_trial
          )),
          recursive = FALSE
        )
        current_graph1(new_graphs[input$graph1_sel])
      }
    }
  })

  # remaking graph2 on trial change
  shiny::observeEvent(input$graph2_trial, {
    if (!is.null(graphs())) {
      if (debug) print("remaking graph2 due to slider change")
      if (input$graph2_sel != "") {
        new_graphs <- unlist(
          unname(calmr::graph(experiment(),
            t = input$graph2_trial
          )),
          recursive = FALSE
        )
        current_graph2(new_graphs[input$graph2_sel])
      }
    }
  })

  # remaking graph1 on selection change
  shiny::observeEvent(input$graph1_sel, {
    if (!is.null(graphs())) {
      if (debug) print("remaking graph1 due to selection change")
      if (input$graph1_sel != "") {
        new_graphs <- unlist(
          unname(calmr::graph(experiment(),
            t = input$graph1_trial
          )),
          recursive = FALSE
        )
        current_graph1(new_graphs[input$graph1_sel])
      }
    }
  })

  # remaking graph2 on selection change
  shiny::observeEvent(input$graph2_sel, {
    if (!is.null(graphs())) {
      if (debug) print("remaking graph2 due to selection change")
      if (input$graph2_sel != "") {
        new_graphs <- unlist(
          unname(calmr::graph(experiment(),
            t = input$graph2_trial
          )),
          recursive = FALSE
        )
        current_graph2(new_graphs[input$graph2_sel])
      }
    }
  })

  # Changes to the stimulus parameter table
  shiny::observeEvent(input$stim_par_tbl$changes$changes, {
    if (debug) print("changing stimulus parameters due to changes in table")
    df <- rhandsontable::hot_to_r(input$stim_par_tbl)
    pars <- current_parameters()
    newpars <- calmr.app:::.df_to_parlist(df, type = "stimulus")
    pars[names(newpars)] <- newpars
    current_parameters(pars)
    # Remake the parameter tables
    par_tables(calmr.app:::.make_par_tables(
      model = input$model_selection,
      parameters = current_parameters(),
      timings = current_timings()
    ))
    ran(FALSE)
  })

  # Changes to the global parameter table
  shiny::observeEvent(input$glob_par_tbl$changes$changes, {
    if (debug) print("changing global parameters due to changes in table")
    df <- rhandsontable::hot_to_r(input$glob_par_tbl)
    pars <- current_parameters()
    newpars <- calmr.app:::.df_to_parlist(df, type = "global")
    pars[names(newpars)] <- newpars
    current_parameters(pars)
    # Remake the parameter tables
    par_tables(calmr.app:::.make_par_tables(
      model = input$model_selection,
      parameters = current_parameters(),
      timings = current_timings()
    ))
    ran(FALSE)
  })

  # Changes to the timings:trial parameter table
  shiny::observeEvent(input$trial_par_tbl$changes$changes, {
    if (debug) print("changing trial parameters due to changes in table")
    tims <- current_timings()
    tims$trial_ts[] <- rhandsontable::hot_to_r(input$trial_par_tbl)
    current_timings(tims)
    # Remake the parameter tables
    par_tables(calmr.app:::.make_par_tables(
      model = input$model_selection,
      parameters = current_parameters(),
      timings = current_timings()
    ))
    ran(FALSE)
  })

  # Changes to the timings:period parameter table
  shiny::observeEvent(input$period_par_tbl$changes$changes, {
    if (debug) print("changing period parameters due to changes in table")
    tims <- current_timings()
    tims$period_ts[] <- rhandsontable::hot_to_r(input$period_par_tbl)
    current_timings(tims)
    # Remake the parameter tables
    par_tables(calmr.app:::.make_par_tables(
      model = input$model_selection,
      parameters = current_parameters(),
      timings = current_timings()
    ))
    ran(FALSE)
  })

  # Changes to the timings:transitions parameter table
  shiny::observeEvent(input$trans_par_tbl$changes$changes, {
    if (debug) print("changing transition parameters due to changes in table")
    tims <- current_timings()
    tims$transition_ts[] <- rhandsontable::hot_to_r(input$trans_par_tbl)
    current_timings(tims)
    # Remake the parameter tables
    par_tables(calmr.app:::.make_par_tables(
      model = input$model_selection,
      parameters = current_parameters(),
      timings = current_timings()
    ))
    ran(FALSE)
  })

  # Changes to the timings:global parameter table
  shiny::observeEvent(input$time_glob_par_tbl$changes$changes, {
    if (debug) print("changing time glob pars due to changes in table")
    tims <- current_timings()
    df <- calmr.app:::.df_to_parlist(rhandsontable::hot_to_r(input$time_glob_par_tbl),
      type = "global"
    )
    tims[names(df)] <- df
    current_timings(tims)
    # Remake the parameter tables
    par_tables(calmr.app:::.make_par_tables(
      model = input$model_selection,
      parameters = current_parameters(),
      timings = current_timings()
    ))
    ran(FALSE)
  })

  # Changes to the design_df table
  shiny::observeEvent(input$design_tbl$changes$changes, {
    if (debug) print("changing design due to changes in table")
    design_df(rhandsontable::hot_to_r(input$design_tbl))
    parsed(FALSE)
    ran(FALSE)
  })

  #### Outputs ####
  # Design table
  output$design_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering design table")
    if (!is.null(design_df())) {
      rhandsontable::rhandsontable(design_df(), rowHeaders = FALSE) |>
        rhandsontable::hot_col(
          col = seq(3, ncol(design_df()), 2), renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.CheckboxRenderer.apply(this, arguments);
              td.style.textAlign = 'center';
           }"
        )
    }
  })

  # Stimulus parameters table
  output$stim_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering stimulus parameters table")
    if (!is.null(par_tables()$stimulus)) {
      rhandsontable::rhandsontable(par_tables()$stimulus,
        rowHeaders = FALSE
      ) |>
        rhandsontable::hot_col("Stimulus", readOnly = TRUE)
    }
  })

  # Global parameters table
  output$glob_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering global parameters table")
    if (!is.null(par_tables()$global)) {
      rhandsontable::rhandsontable(par_tables()$global,
        rowHeaders = FALSE
      ) |>
        rhandsontable::hot_col("Parameter", readOnly = TRUE)
    }
  })

  # Trial parameters table
  output$trial_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering trial parameters table")
    if (!is.null(par_tables()$trial)) {
      rhandsontable::rhandsontable(par_tables()$trial,
        rowHeaders = FALSE, wordWrap = FALSE
      ) |>
        rhandsontable::hot_col(c("Trial"), readOnly = TRUE)
    }
  })

  # Transition parameters table
  output$trans_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering trans parameters table")
    if (!is.null(par_tables()$transition)) {
      rhandsontable::rhandsontable(par_tables()$transition,
        rowHeaders = FALSE, wordWrap = FALSE
      ) |>
        rhandsontable::hot_col(c("Trial", "Transition"),
          readOnly = TRUE
        )
    }
  })

  # Period parameters table
  output$period_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering period parameters table")
    if (!is.null(par_tables()$period)) {
      rhandsontable::rhandsontable(par_tables()$period,
        rowHeaders = FALSE, wordWrap = FALSE
      ) |>
        rhandsontable::hot_col(c("Trial", "Period", "Stimulus"),
          readOnly = TRUE
        )
    }
  })

  # Global timing parameters table
  output$time_glob_par_tbl <- rhandsontable::renderRHandsontable({
    if (debug) print("rendering global timing parameters table")
    if (!is.null(par_tables()$time_global)) {
      rhandsontable::rhandsontable(par_tables()$time_global,
        rowHeaders = FALSE, wordWrap = FALSE
      ) |>
        rhandsontable::hot_col(c("Parameter"),
          readOnly = TRUE
        )
    }
  })

  output$parameter_ui <- shiny::renderUI({
    stim_nav <- global_nav <- trial_nav <-
      period_nav <- trans_nav <- time_glob_nav <- NULL
    if (parsed()) {
      stim_nav <- bslib::nav_panel(
        "Stimulus",
        rhandsontable::rHandsontableOutput("stim_par_tbl")
      )
      if (needs_globalpars()) {
        global_nav <- bslib::nav_panel(
          "Global",
          rhandsontable::rHandsontableOutput("glob_par_tbl")
        )
      }
      if (needs_timings()) {
        trial_nav <- bslib::nav_panel(
          "Timings: Trial",
          rhandsontable::rHandsontableOutput("trial_par_tbl")
        )
        period_nav <- bslib::nav_panel(
          "Timings: Period",
          rhandsontable::rHandsontableOutput("period_par_tbl")
        )
        trans_nav <- bslib::nav_panel(
          "Timings: Transitions",
          rhandsontable::rHandsontableOutput("trans_par_tbl")
        )
        time_glob_nav <- bslib::nav_panel(
          "Timings: Global",
          rhandsontable::rHandsontableOutput("time_glob_par_tbl")
        )
      }
    }
    bslib::page_navbar(
      stim_nav, global_nav, trial_nav,
      period_nav, trans_nav, time_glob_nav
    )
  })

  # general show
  output$parsed <- shiny::reactive({
    parsed()
  })


  # Whether the experiment has been ran
  output$ran <- shiny::reactive({
    return(ran())
  })

  # Enable/disable download button
  shiny::observeEvent(ran(), {
    if (ran()) {
      shinyjs::enable("export_results")
    } else {
      shinyjs::disable("export_results")
    }
  })

  output$export_results <- shiny::downloadHandler(
    filename = "calmr_results.xlsx",
    content = function(filename) {
      data <- list(
        design = design_df(),
        model = input$model_selection,
        stimulus_parameters = par_tables()$stimulus
      )
      if (needs_globalpars()) {
        data <- c(
          data,
          list(global_parameters = par_tables()$global)
        )
      }
      if (needs_timings()) {
        data <- c(
          data,
          list(
            trial_parameters = par_tables()$trial,
            period_parameters = par_tables()$period,
            transition_parameters = par_tables()$transition,
            timing_global_parameters = par_tables()$time_global
          )
        )
      }
      data <- c(
        data,
        calmr::results(experiment())
      )
      openxlsx::write.xlsx(data, file = filename, overwrite = TRUE)
    }
  )

  # Sidebar selection menu
  output$filters <- shiny::renderUI({
    if (ran()) {
      list(
        shiny::checkboxGroupInput("phase_selection",
          label = "Phase",
          choices = avail_phases(),
          selected = avail_phases()
        ),
        shiny::checkboxGroupInput("trial_selection",
          label = "Trial type",
          choices = avail_trials(),
          selected = avail_trials()
        ),
        shiny::checkboxGroupInput("stim_selection",
          label = "Stimulus",
          choices = avail_stimuli(),
          selected = avail_stimuli()
        )
      )
    } else {
      "You must first run an experiment."
    }
  })

  output$plot_1 <- plotly::renderPlotly({
    if (!is.null(current_plot1())) {
      if (debug) print("rendering plot1")
      shiny::withProgress(message = "Rendering plot 1 ...", {
        p <- plotly::ggplotly(current_plot1())
        shiny::incProgress(1)
        p
      })
    }
  })

  output$plot_2 <- plotly::renderPlotly({
    if (!is.null(current_plot2())) {
      if (debug) print("rendering plot2")
      shiny::withProgress(message = "Rendering plot 2 ...", {
        p <- plotly::ggplotly(current_plot2())
        shiny::incProgress(1)
        p
      })
    }
  })

  output$graph1 <- shiny::renderPlot({
    if (!is.null(current_graph1())) {
      if (debug) print("rendering graph1")
      shiny::withProgress(message = "Rendering graph 1 ...", {
        g <- current_graph1()
        shiny::incProgress(1)
        g
      })
    }
  })

  output$graph2 <- shiny::renderPlot({
    if (debug) print("rendering graph2")
    if (!is.null(current_graph2())) {
      shiny::withProgress(message = "Rendering graph 2 ...", {
        g <- current_graph2()
        shiny::incProgress(1)
        g
      })
    }
  })

  # UI for results
  output$results_panel <- shiny::renderUI({
    if (debug) print("creating results panel")
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        full_screen = TRUE,
        shiny::selectizeInput("plot1_sel",
          label = NULL, choices = NULL, width = "100%"
        ),
        plotly::plotlyOutput("plot_1")
      ),
      bslib::card(
        full_screen = TRUE,
        shiny::selectInput("plot2_sel",
          label = NULL, choices = NULL, width = "100%"
        ),
        plotly::plotlyOutput("plot_2")
      )
    )
  })

  # UI for graphs panel
  output$graphs_panel <- shiny::renderUI({
    card2 <- NULL
    if (nrow(design_df()) > 1) {
      if (debug) print("creating second graph panel")
      card2 <- bslib::card(
        class = "align-items-center",
        shiny::selectizeInput("graph2_sel",
          label = NULL, choices = NULL, width = "100%"
        ),
        shiny::sliderInput(
          inputId = "graph2_trial",
          label = "Trial", ticks = FALSE, width = "100%",
          min = 1, max = 1, value = 1, step = 1
        ),
        bslib::card(
          class = "align-items-center",
          shiny::plotOutput("graph2",
            height = "100%",
            width = "100%"
          )
        )
      )
    }
    if (debug) print("assembling panels")
    bslib::layout_column_wrap(
      width = 1 / 2,
      bslib::card(
        class = "align-items-center",
        shiny::selectizeInput("graph1_sel",
          label = NULL, choices = NULL, width = "100%"
        ),
        shiny::sliderInput(
          inputId = "graph1_trial",
          label = "Trial", ticks = FALSE, width = "100%",
          min = 1, max = 1, value = 1, step = 1
        ),
        bslib::card(
          class = "align-items-center",
          shiny::plotOutput("graph1",
            height = "100%",
            width = "100%"
          )
        )
      ),
      card2
    )
  })

  # populating the graph selections and trials upon experiment change
  shiny::observeEvent(experiment(), {
    if (!is.null(graphs())) {
      if (debug) print("populating graph selectors and sliders 1")
      gnames <- names(graphs())
      shiny::updateSelectizeInput(
        inputId = "graph1_sel",
        selected = gnames[1],
        choices = gnames
      )
      # populating the slider for graph trial selection
      calmr.app:::.update_trial_slider(
        slider_id = "graph1_trial",
        group_number = 1,
        experiment = experiment()
      )
      if (length(graphs()) > 1) {
        if (debug) print("populating graph selectors and sliders 2")
        shiny::updateSelectizeInput(
          inputId = "graph2_sel",
          selected = gnames[2],
          choices = gnames
        )
        calmr.app:::.update_trial_slider(
          slider_id = "graph2_trial",
          group_number = 2,
          experiment = experiment()
        )
      }
    }
  })

  output$logo <- shiny::renderImage(
    {
      list(
        src = "www/logo.png",
        contentType = "image/png",
        alt = "calmr_logo",
        width = "100%",
        height = "100%"
      )
    },
    deleteFile = FALSE
  )

  shiny::outputOptions(output, "parsed", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "ran", suspendWhenHidden = FALSE)
  shiny::outputOptions(output, "filters", suspendWhenHidden = FALSE)
  shinyjs::disable("export_results")
})
