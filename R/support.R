#' Parse parameter list into stimulus-specific and global parameters
#' @param model A modelname string
#' @param parameters A list with parameters
#' @return A list with stimulus and global parameters (both data.frames)
#' @noRd
.make_par_tables <- function(model, parameters, timings) {
  parnames <- names(parameters)
  gpars <- sapply(parnames, calmr:::.is_global_parameter, model = model)
  spars <- !gpars

  stimpars <- globpars <- trialpars <- transpars <-
    periodpars <- timglobalpars <- NULL
  if (any(spars)) {
    stimnames <- names(parameters[[which(spars)[1]]])
    stimpars <- data.frame(
      stimulus = stimnames,
      as.data.frame(parameters[parnames[spars]])
    )
    names(stimpars) <- stringr::str_to_title(names(stimpars))
  }

  if (any(gpars)) {
    globpars <- data.frame(
      parameter = parnames[gpars],
      value = as.numeric(unlist(parameters[parnames[gpars]]))
    )
    names(globpars) <- stringr::str_to_title(names(globpars))
  }

  if (!is.null(timings)) {
    trialpars <- timings$trial_ts
    names(trialpars) <- stringr::str_to_title(names(trialpars))

    transpars <- timings$transition_ts
    names(transpars) <- stringr::str_to_title(names(transpars))

    periodpars <- timings$period_ts
    names(periodpars) <- stringr::str_to_title(names(periodpars))

    timglobalpars <- timings[names(timings)[!(names(timings) %in%
      c("trial_ts", "transition_ts", "period_ts"))]]
    timglobalpars <- data.frame(
      Parameter = names(timglobalpars),
      Value = unname(unlist(c(timglobalpars)))
    )
  }

  return(list(
    stimulus = stimpars,
    global = globpars,
    trial = trialpars,
    transition = transpars,
    period = periodpars,
    time_global = timglobalpars
  ))
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
  any(sapply(names(parameters),
    calmr:::.is_global_parameter,
    model = model
  ))
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
  experiment@results@aggregated_results <- res
  experiment
}
