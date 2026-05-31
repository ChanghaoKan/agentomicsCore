# Helpers for recording decisions, scripts, and figures onto AgentOmics.
# These form the public extension API used by downstream domain packages.

#' Record a pipeline step onto an AgentOmics object
#'
#' Appends a decision record and a reproducible script snippet to the
#' object, merges step parameters into `obj@params`, and optionally
#' advances `obj@stage`. Every tool function in a downstream package
#' calls this at the end so the returned object carries a complete audit
#' trail.
#'
#' @param obj AgentOmics object to update.
#' @param step_name Short machine-readable name (matches the `@@stage`
#'   convention).
#' @param function_name Name of the calling function (for the log).
#' @param params Named list of parameters passed to the function. Merged
#'   into `obj@params` (keys present here take precedence; other keys are
#'   preserved).
#' @param rationale Human-readable explanation of why this step was taken
#'   (ideally LLM-generated or defaulted to a template).
#' @param script_snippet Character string with the R code that reproduces
#'   this step; appended to `obj@scripts`.
#' @param new_stage Optional character; if provided, updates `obj@stage`.
#' @param success Logical, whether the step completed successfully.
#' @return The updated AgentOmics object.
#' @export
record_step <- function(obj,
                         step_name,
                         function_name,
                         params,
                         rationale,
                         script_snippet,
                         new_stage = NULL,
                         success = TRUE) {

  decision <- list(
    step          = step_name,
    function_name = function_name,
    timestamp     = Sys.time(),
    params        = params,
    rationale     = rationale,
    success       = success
  )

  obj@decisions <- c(obj@decisions, list(decision))
  obj@scripts   <- c(obj@scripts, script_snippet)
  # Merge instead of overwrite: @params accumulates rich intermediate
  # products (ndim, batch_candidates, markers_filtered, llm_annotations,
  # reference_matches, resolution_recommendation, ...) that downstream
  # tools depend on. Overwriting wiped them out. Keys present in the new
  # `params` arg take precedence.
  if (length(params) > 0) {
    obj@params[names(params)] <- params
  }
  obj@updated_at <- Sys.time()
  if (!is.null(new_stage)) {
    obj@stage <- new_stage
  }
  obj
}

#' Register a figure file against an AgentOmics object
#'
#' Appends a row to `obj@@figures` so the figure is picked up by
#' [render_report()]'s gallery. Downstream plotting functions call this
#' after writing a PNG/SVG to disk.
#'
#' @param obj An AgentOmics object.
#' @param step Character, the step name the figure belongs to.
#' @param path Character, path to the image file on disk.
#' @param description Character, caption for the figure.
#' @return The updated AgentOmics object.
#' @export
record_figure <- function(obj, step, path, description) {
  new_row <- data.frame(
    step        = step,
    path        = path,
    description = description,
    stringsAsFactors = FALSE
  )
  obj@figures <- rbind(obj@figures, new_row)
  obj
}

# Format a named list of parameters into an R-call-style snippet fragment,
# e.g. list(min_nCount = 1000, min_nFeature = 500) becomes
# "min_nCount = 1000, min_nFeature = 500".
.format_params <- function(params) {
  if (length(params) == 0) return("")
  parts <- vapply(names(params), function(nm) {
    val <- params[[nm]]
    val_fmt <- if (is.character(val)) {
      paste0('"', val, '"')
    } else if (is.null(val)) {
      "NULL"
    } else if (is.logical(val)) {
      as.character(val)
    } else if (length(val) > 1) {
      paste0("c(", paste(vapply(val, .format_scalar, character(1)),
                         collapse = ", "), ")")
    } else {
      format(val)
    }
    paste0(nm, " = ", val_fmt)
  }, character(1))
  paste(parts, collapse = ", ")
}

# Format a scalar value for inclusion in a snippet.
.format_scalar <- function(val) {
  if (is.character(val)) paste0('"', val, '"') else format(val)
}

# Search the decision log for a parameter value recorded at a previous step.
#
# Used by downstream functions to auto-pull upstream choices (e.g. ndim
# selected during sc_pca is reused by sc_harmony, sc_umap, sc_cluster
# unless overridden).
#
# @param obj AgentOmics object.
# @param param_name Name of the parameter to look up.
# @param from_step Optional, restrict search to a specific step name.
# @return The most recently recorded value, or NULL if not found.
find_in_decisions <- function(obj, param_name, from_step = NULL) {
  for (d in rev(obj@decisions)) {
    if (!is.null(from_step) && d$step != from_step) next
    if (param_name %in% names(d$params)) {
      return(d$params[[param_name]])
    }
  }
  NULL
}

#' Apply a function across the data payload
#'
#' Hides the single-object vs list-of-objects branching from callers.
#' If `obj@data_type == "seurat"` (or any non-list payload), `fn` is
#' applied directly; otherwise `fn` is mapped over the list.
#'
#' @param obj An AgentOmics object.
#' @param fn A function `(payload, ...) -> payload`.
#' @param ... Extra arguments passed to `fn`.
#' @return The updated AgentOmics object with `fn` applied to `@@data`.
#' @export
apply_to_data <- function(obj, fn, ...) {
  if (obj@data_type == "seurat" || !is.list(obj@data)) {
    obj@data <- fn(obj@data, ...)
  } else {
    obj@data <- lapply(obj@data, fn, ...)
  }
  obj
}
