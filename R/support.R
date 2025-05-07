#' Parse parameter list into stimulus-specific and global parameters
#' @param model A modelname string
#' @param parameters A list with parameters
#' @return A list with stimulus and global parameters (both data.frames)
#' @export
make_par_tables <- function(model, parameters) {
  parnames <- names(parameters)
  gpars <- sapply(parnames, calmr:::.is_global_parameter, model = model)
  spars <- !gpars

  stimpars <- globpars <- NULL
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

  return(list(
    stimulus = stimpars,
    global = globpars
  ))
}
#' Parse timing parameter list into data.frames
#' @param parameters A list with timings, as returned by `calmr::get_timings()`
#' @return A list with data.frames
#' @export
make_timing_tables <- function(timings) {
  browser()
  # As of calmr 0.7.0, the timing parameters
  # are either:
  # "trial_ts", "period_ts", or "transition_ts"
  # everything else is a global parameter.
  needs_periodpars <- "period_ts" %in% names(timings)
  needs_transitionpars <- "transition_ts" %in% names(timings)
  glob_pars <- names(timings)[!names(timings) %in% c(
    "trial_ts", "period_ts", "transition_ts"
  )]

  glob_pars <- trial_pars <- period_pars <- transition_pars <- NULL
  # make global parameters table
  if (length(glob_pars)) {
    globpars <- data.frame(
      parameter = glob_pars,
      value = as.numeric(unlist(timings[glob_pars]))
    )
    names(globpars) <- stringr::str_to_title(names(globpars))
  }
  if ("trial_ts" %in% names(timings)) {
    trial_pars <- data.frame(
      trial = names(timings$trial_ts),
      value = as.numeric(unlist(timings$trial_ts))
    )
    names(trial_pars) <- stringr::str_to_title(names(trial_pars))
  }
  if ("period_ts" %in% names(timings)) {
    period_pars <- data.frame(
      period = names(timings$period_ts),
      value = as.numeric(unlist(timings$period_ts))
    )
    names(period_pars) <- stringr::str_to_title(names(period_pars))
  }
  if ("transition_ts" %in% names(timings)) {
    transition_pars <- data.frame(
      transition = names(timings$transition_ts),
      value = as.numeric(unlist(timings$transition_ts))
    )
    names(transition_pars) <- stringr::str_to_title(names(transition_pars))
  }
  list(
    global_timings = globpars,
    trial_timings = trial_pars,
    period_timings = period_pars,
    transition_timings = transition_pars
  )
}

#' Convert parameter data.frame to list
#' @param df A `data.frame`
#' @param type A character specifying which type of
#' list we're dealing with.
#' @export
df_to_parlist <- function(df, type) {
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
  if (type == "trial") {
    pars <- sapply(unique(df$Parameter), function(p) {
      stats::setNames(
        df$Value[df$Parameter == p],
        df$Trial[df$Parameter == p]
      )
    }, simplify = FALSE)
  }
  if (type == "transition") {
    pars <- list()
    parnames <- unique(df$Parameter)
    for (p in parnames) {
      pdat <- df[df$Parameter == p, ]
      pars[[p]] <- sapply(unique(pdat$Trial), function(t) {
        stats::setNames(
          pdat$Value[df$Trial == t],
          df$Transition[df$Trial == t]
        )
      }, simplify = FALSE)
    }
  }
  pars
}
#' @export
check_globalpars <- function(model, parameters) {
  any(sapply(names(parameters),
    calmr:::.is_global_parameter,
    model = model
  ))
}
#' @export
check_trialpars <- function(model, parameters) {
  any(sapply(names(parameters),
    calmr:::.is_trial_parameter,
    model = model
  ))
}
#' @export
check_transpars <- function(model, parameters) {
  any(sapply(names(parameters),
    calmr:::.is_trans_parameter,
    model = model
  ))
}
#' @export
join_parameters <- function(old, new) {
  # changing between models
  if (length(setdiff(names(old), names(new)))) {
    return(new)
  }
  mapply(.replace, new, old, simplify = FALSE)
}

# recursively replaces the intersection of parameters
.replace <- function(a, b, ...) {
  if (is.list(a)) {
    mapply(.replace, a = a, b = b, simplify = FALSE)
  } else {
    to_replace <- intersect(names(a), names(b))
    a[to_replace] <- b[to_replace]
    list(a)
  }
}
