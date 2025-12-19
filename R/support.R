#' Parse parameter list into stimulus-specific and global parameters
#' @param model A modelname string
#' @param parameters A list with parameters
#' @return A list with stimulus and global parameters (both data.frames)
#' @noRd
.make_par_tables <- function(model, parameters, timings) {
  parnames <- names(parameters)
  # mark global parameters
  gpars <- calmr::model_parameters(model)$is_global
  spars <- !gpars

  stimpars <- glob_pars <- NULL
  if (any(spars)) {
    stimnames <- names(parameters[[which(spars)[1]]])
    stimpars <- data.frame(
      stimulus = stimnames,
      as.data.frame(parameters[parnames[spars]])
    )
    names(stimpars) <- tools::toTitleCase(names(stimpars))
  }

  if (any(gpars)) {
    glob_pars <- data.frame(
      parameter = parnames[gpars],
      value = as.numeric(unlist(parameters[parnames[gpars]]))
    )
    names(glob_pars) <- tools::toTitleCase(names(glob_pars))
  }

  list(
    stimulus = stimpars,
    global = glob_pars
  )
}
#' Parse timing parameter list into data.frames
#' @param timings A list with timings, as returned by `calmr::get_timings()`
#' @return A list with data.frames
#' @noRd
.make_timing_tables <- function(timings) {
  # As of calmr 0.7.0, the timing parameters
  # are either:
  # "trial_ts", "period_ts", or "transition_ts"
  # everything else is a global parameter.
  globals <- names(timings)[!names(timings) %in% c(
    "trial_ts", "period_ts", "transition_ts"
  )]

  glob_pars <- trial_pars <- period_pars <- transition_pars <- NULL
  # make global parameters table
  if (length(globals)) {
    glob_pars <- data.frame(
      parameter = globals,
      value = as.numeric(unlist(timings[globals]))
    )
    names(glob_pars) <- stringr::str_to_title(names(glob_pars))
  }
  if ("trial_ts" %in% names(timings)) {
    trial_pars <- timings$trial_ts
    names(trial_pars) <- stringr::str_to_title(names(trial_pars))
  }
  if ("period_ts" %in% names(timings)) {
    period_pars <- timings$period_ts
    names(period_pars) <- stringr::str_to_title(names(period_pars))
  }
  if ("transition_ts" %in% names(timings)) {
    transition_pars <- timings$transition_ts
    names(transition_pars) <- stringr::str_to_title(names(transition_pars))
  }
  list(
    global_timings = glob_pars,
    trial_timings = trial_pars,
    period_timings = period_pars,
    transition_timings = transition_pars
  )
}

#' Convert parameter data.frame to list
#' @param df A `data.frame`
#' @param type A character specifying which type of
#' list we're dealing with.
#' @return A list.
#' @note This is a support function for the app.
#' @noRd
.df_to_parlist <- function(df, type) {
  parnames <- names(df)
  pars <- NULL
  if (type == "stimulus") {
    stimnames <- df$Stimulus
    pars <- list()
    for (p in parnames[-1]) {
      pars[[p]] <- stats::setNames(df[[p]], stimnames)
    }
    names(pars) <- stringr::str_to_lower(names(pars))
  }
  if (type == "global") {
    pars <- c(sapply(df$Parameter, function(p) {
      df$Value[df$Parameter == p]
    }, simplify = FALSE))
  }
  pars
}

.check_globalpars <- function(model, parameters) {
  any(calmr::model_parameters(model)$is_global)
}


.update_trial_slider <- function(slider_id, group_number, experiment) {
  # updates a trial slider with the maximum trial in the experiment
  max_t <- calmr.app:::.max_graph_trial(group_number, experiment)
  shiny::updateSliderInput(inputId = slider_id, max = max_t, value = max_t)
}

.max_graph_trial <- function(group_number, experiment) {
  max(calmr::experiences(experiment)[[group_number]]$trial)
}

# filters experimental data
.filter_experiment <- function(experiment, filters) {
  res <- calmr::results(experiment)
  res <- lapply(
    res,
    function(r) r[r$phase %in% filters$phase_f, ]
  )
  res <- lapply(
    res,
    function(r) r[r$trial_type %in% filters$trial_f, ]
  )
  res <- lapply(
    res,
    function(r) r[r$s1 %in% filters$stim_f, ]
  )
  res <- lapply(
    res,
    function(r) {
      if ("s2" %in% names(r)) {
        r[r$s2 %in% filters$stim_f, ]
      } else {
        r
      }
    }
  )
  experiment@results <- res
  experiment
}


#' Get version mismatch warning message
#' Returns a character string with the warning message
#' @noRd
.get_version_warning <- function() {
  calmr_version <- as.character(utils::packageVersion("calmr"))
  # Get calmr version from DESCRIPTION
  app_desc <- utils::packageDescription("calmr.app")
  imps <- strsplit(app_desc$Imports, ",")[[1]]
  imps <- imps[grepl("calmr", imps)]
  # extract version requirement
  required_calmr_version <- gsub(".*>= *([0-9\\.]+).*", "\\1", imps)
  required_calmr_version <- "0.9.0"
  if (utils::compareVersion(calmr_version, required_calmr_version) >= 0) {
    msg <- ""
  } else {
    msg <- paste0(
      "calmr.app requires calmr version ",
      required_calmr_version,
      " or higher. Please match versions to avoid issues."
    )
  }
  msg
}
