#' AgentOmics: S4 container for provenance-tracked omics analysis
#'
#' An S4 class that wraps an arbitrary omics data payload (a Seurat object, SingleCellExperiment, GRanges, RangedSummarizedExperiment, or a list of any of these)
#' together with decision logs, generated script
#' snippets, figure metadata, and stage tracking. Tool functions in
#' downstream packages (scAgentKit, atacAgentKit, etc.) take and return
#' an AgentOmics, accumulating state as the pipeline progresses.
#'
#' @slot data The core data payload. Either a single primary object
#'   (Seurat / SingleCellExperiment / GRanges / &c.) or a named list of
#'   such objects (during per-sample / per-condition processing phases).
#' @slot data_type Character, free-form tag describing the structure of
#'   @@data (e.g. "seurat", "seurat_list", "sce", "granges_list").
#' @slot stage Character tag for the current pipeline stage
#'   (e.g. "initialized", "qc_metrics_added", "doublet_removed",
#'   "normalized", "pca_done", "harmony_integrated", "clustered",
#'   "annotated").
#' @slot decisions List of decision records. Each element is a list with
#'   fields: step, function_name, timestamp, params, rationale,
#'   success.
#' @slot scripts Character vector of R script snippets intended as a review
#'   and reconstruction aid. Downstream packages determine whether the
#'   concatenated snippets are independently runnable.
#' @slot figures Data frame with columns step, path, description,
#'   recording figures generated during the pipeline.
#' @slot params Flat list of the most recently applied parameters (useful
#'   for quick inspection; full history lives in @@decisions).
#' @slot token_usage List of per-step LLM token consumption summaries
#'   (originating in scAgentKit v0.2.0). One entry per pipeline step that involved an LLM
#'   call. Each entry is a list with `input_tokens`, `output_tokens`,
#'   `cached_tokens`, `n_calls`, and a `by_model` data frame.
#' @slot version Character, the version (and ideally package name) of the framework that created this
#'   object. Used by [upgrade_checkpoint()] to handle backward
#'   compatibility when loading saved objects.
#' @slot created_at POSIXct timestamp of object creation.
#' @slot updated_at POSIXct timestamp of the last modification.
#'
#' @name AgentOmics-class
#' @rdname AgentOmics-class
#' @importFrom methods setClass setMethod new is validObject slotNames as callNextMethod setValidity show
#' @exportClass AgentOmics
methods::setClass(
  "AgentOmics",
  slots = c(
    data        = "ANY",
    data_type   = "character",
    stage       = "character",
    decisions   = "list",
    scripts     = "character",
    figures     = "data.frame",
    params      = "list",
    token_usage = "list",
    version     = "character",
    created_at  = "POSIXct",
    updated_at  = "POSIXct"
  ),
  prototype = list(
    data        = NULL,
    data_type   = "seurat",
    stage       = "initialized",
    decisions   = list(),
    scripts     = character(0),
    figures     = data.frame(
      step        = character(0),
      path        = character(0),
      description = character(0),
      stringsAsFactors = FALSE
    ),
    params      = list(),
    token_usage = list(),
    version     = NA_character_,
    created_at  = Sys.time(),
    updated_at  = Sys.time()
  )
)

#' Construct an AgentOmics container
#'
#' Entry point of any agentomicsCore-based pipeline. Wraps a user-supplied
#' omics object (a Seurat, SingleCellExperiment, GRanges,
#' RangedSummarizedExperiment, or any other BioC-style payload) into a
#' decision-logged container.
#'
#' Downstream packages typically provide their own thinner constructors
#' (e.g. `scAgentKit::AgentSeurat()`, `atacAgentKit::AgentATAC()`) that
#' delegate here and stamp the appropriate `data_type` tag.
#'
#' @param data An arbitrary omics object, or a named list of such objects.
#' @param data_type Optional character tag describing the structure of
#'   `data` (e.g. "seurat", "sce", "granges_list"). If `NULL`, a sensible
#'   default is inferred: `"seurat_list"` if `data` is an unnamed list of
#'   non-Seurat-class members, `"seurat"` otherwise.
#' @param initial_script Optional character string recording how the input
#'   was loaded (e.g. the readRDS call); will be prepended to the
#'   generated script trace.
#' @param package_name Optional name of the constructing framework
#'   package (e.g. `"scAgentKit"`). Used to stamp `@@version` from
#'   `packageVersion(package_name)`. If `NULL`, agentomicsCore's own
#'   version is used.
#'
#' @return An AgentOmics object at stage "initialized".
#' @export
#'
#' @examples
#' \dontrun{
#'   seu <- readRDS("my_data.rds")
#'   obj <- AgentOmics(seu, data_type = "seurat",
#'                     initial_script = 'seu <- readRDS("my_data.rds")')
#' }
AgentOmics <- function(data,
                       data_type      = NULL,
                       initial_script = NULL,
                       package_name   = NULL) {
  if (is.null(data_type)) {
    data_type <- if (is.list(data) && !inherits(data, "Seurat")) {
      "seurat_list"
    } else {
      "seurat"
    }
  }

  pkg_for_version <- package_name %||% "agentomicsCore"
  pkg_version <- tryCatch(
    as.character(utils::packageVersion(pkg_for_version)),
    error = function(e) NA_character_
  )

  obj <- methods::new(
    "AgentOmics",
    data       = data,
    data_type  = data_type,
    stage      = "initialized",
    version    = pkg_version %||% NA_character_,
    created_at = Sys.time(),
    updated_at = Sys.time()
  )

  if (!is.null(initial_script)) {
    obj@scripts <- c(obj@scripts, initial_script)
  }

  obj
}

#' @rdname AgentOmics-class
#' @param object An AgentOmics object.
#' @export
methods::setMethod("show", "AgentOmics", function(object) {
  cat("<AgentOmics>\n")
  ver <- if (length(object@version) == 0 || is.na(object@version)) {
    "<unknown>"
  } else object@version
  cat("  Version:    ", ver, "\n", sep = "")
  cat("  Stage:      ", object@stage, "\n", sep = "")
  cat("  Data type:  ", object@data_type, "\n", sep = "")

  if (object@data_type == "seurat") {
    n_cells <- tryCatch(ncol(object@data), error = function(e) NA)
    n_genes <- tryCatch(nrow(object@data), error = function(e) NA)
    cat("  Cells:      ", n_cells, "\n", sep = "")
    cat("  Genes:      ", n_genes, "\n", sep = "")
  } else {
    n_samples <- length(object@data)
    cell_counts <- tryCatch(
      vapply(object@data, function(s) as.integer(ncol(s)), integer(1)),
      error = function(e) NA
    )
    cat("  Samples:    ", n_samples, "\n", sep = "")
    if (!any(is.na(cell_counts))) {
      cat("  Cells:      ", sum(cell_counts),
          " (total across ", n_samples, " samples)\n", sep = "")
    }
  }

  cat("  Decisions:  ", length(object@decisions), " steps recorded\n", sep = "")
  cat("  Figures:    ", nrow(object@figures), "\n", sep = "")

  # Token usage one-liner if any LLM calls happened
  if (length(object@token_usage) > 0) {
    tot_in  <- sum(vapply(object@token_usage,
                          function(x) x$input_tokens  %||% 0L, integer(1)))
    tot_out <- sum(vapply(object@token_usage,
                          function(x) x$output_tokens %||% 0L, integer(1)))
    n_calls <- sum(vapply(object@token_usage,
                          function(x) x$n_calls %||% 0L, integer(1)))
    cat(sprintf("  LLM tokens: %d in + %d out across %d call(s)\n",
                tot_in, tot_out, n_calls))
  }

  cat("  Updated:    ", format(object@updated_at, "%Y-%m-%d %H:%M:%S"),
      "\n", sep = "")
  invisible(NULL)
})

# ---- Getters ----------------------------------------------------------------

#' Extract the underlying data payload
#' @param obj An AgentOmics object.
#' @return The wrapped data object (or list of objects).
#' @export
get_data <- function(obj) {
  stopifnot(methods::is(obj, "AgentOmics"))
  obj@data
}

#' Return the accumulated decision log
#' @param obj An AgentOmics object.
#' @return A list of decision records.
#' @export
get_decisions <- function(obj) {
  stopifnot(methods::is(obj, "AgentOmics"))
  obj@decisions
}

#' Return the accumulated generated R script
#'
#' Concatenates all recorded script snippets, separated by blank lines,
#' yielding a script trace. Its standalone executability depends on the
#' snippets supplied by the downstream package and should be reviewed.
#'
#' @param obj An AgentOmics object.
#' @return Character string containing the accumulated script snippets.
#' @export
get_script <- function(obj) {
  stopifnot(methods::is(obj, "AgentOmics"))
  paste(obj@scripts, collapse = "\n\n")
}

#' Return the figure registry
#' @param obj An AgentOmics object.
#' @return Data frame with columns step, path, description.
#' @export
get_figures <- function(obj) {
  stopifnot(methods::is(obj, "AgentOmics"))
  obj@figures
}


#' Return the per-step LLM token usage log
#'
#' Each pipeline step that issued LLM calls records its consumption here.
#' Use this for cost reporting and as a more granular alternative to the
#' global [token_usage_summary()].
#'
#' @param obj An AgentOmics object.
#' @return A named list keyed by step name. Each entry is a list with
#'   `input_tokens`, `output_tokens`, `cached_tokens`, `n_calls`, and a
#'   `by_model` data frame.
#' @export
get_token_usage <- function(obj) {
  stopifnot(methods::is(obj, "AgentOmics"))
  obj@token_usage
}


#' Upgrade a saved AgentOmics from an older scAgentKit version
#'
#' Saved checkpoints (qs/rds) from older scAgentKit releases may be missing
#' slots that newer versions expect. This function fills in defaults so
#' `obj` round-trips through the current S4 class without errors.
#'
#' Currently handles:
#' \itemize{
#'   \item pre-v0.2.0 objects without `@@token_usage` slot
#'   \item pre-v0.2.0 objects without `@@version` slot
#' }
#'
#' @param obj An AgentOmics object loaded from a checkpoint.
#' @return The same object with any missing slots populated.
#' @export
upgrade_checkpoint <- function(obj) {
  stopifnot(methods::is(obj, "AgentOmics"))
  # For objects deserialised under the current class definition, missing
  # slots already read back as their prototype defaults. We defensively
  # re-initialise token_usage if it isn't a list, and stamp a version if
  # one isn't present.
  changed <- character(0)

  tu <- tryCatch(obj@token_usage, error = function(e) NULL)
  if (!is.list(tu)) {
    obj@token_usage <- list()
    changed <- c(changed, "token_usage")
  }

  current_ver <- tryCatch(obj@version, error = function(e) NA_character_)
  if (length(current_ver) == 0 || all(is.na(current_ver)) ||
      !any(nzchar(current_ver))) {
    obj@version <- tryCatch(
      as.character(utils::packageVersion("agentomicsCore")),
      error = function(e) "unknown"
    )
    changed <- c(changed, "version")
  }

  if (length(changed) > 0) {
    message(sprintf(
      "[upgrade_checkpoint] populated missing slot(s): %s",
      paste(changed, collapse = ", ")
    ))
  }
  obj
}
