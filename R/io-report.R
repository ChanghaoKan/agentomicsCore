#' Generate a self-contained HTML analysis report
#'
#' Produces a single HTML file summarising the entire pipeline: stage
#' progression, decision log, embedded figures, LLM token usage, and the
#' full reproducible R script. Figures are base64-embedded so the HTML is
#' portable (no accompanying image folder needed).
#'
#' Downstream packages (scAgentKit, atacAgentKit, ...) typically wrap this
#' function and pass domain-specific section renderers via the `extras`
#' argument. See the `extras` parameter.
#'
#' @param obj An AgentOmics object (any stage).
#' @param path Output HTML path. Default "analysis_report.html".
#' @param title Character, report title shown in the HTML. If NULL,
#'   a default based on the object's stage is used.
#' @param include_script Logical, whether to embed the full reproducible
#'   script inline. Default TRUE.
#' @param include_params Logical, whether to include full parameter
#'   details (can be verbose). Default TRUE.
#' @param extras A named list of section-renderer functions, each of
#'   signature `function(obj) -> character` returning a complete
#'   `<section>...</section>` HTML block. Renderers returning `""` are
#'   skipped. Names are not displayed (renderers should produce their
#'   own `<h2>` headers); they exist only to disambiguate keys.
#'   Sections are rendered after the figure gallery, in list order.
#' @param header_title_prefix Character used as the title prefix when
#'   `title = NULL`. Default `"agentomicsCore analysis report"`. Wrappers
#'   typically override (e.g. `"scAgentKit analysis report"`).
#'
#' @return Invisibly returns `path`.
#' @export
#'
#' @examples
#' \dontrun{
#'   render_report(obj, path = "report.html",
#'                 title = "PBMC 3k pipeline")
#'
#'   # Downstream packages plug in their own section renderers:
#'   render_report(obj,
#'                 extras = list(annotations  = scAgentKit:::render_annotations,
#'                               comparison   = scAgentKit:::render_comparison))
#' }
render_report <- function(obj,
                          path                = "analysis_report.html",
                          title               = NULL,
                          include_script      = TRUE,
                          include_params      = TRUE,
                          extras              = list(),
                          header_title_prefix = "agentomicsCore analysis report") {

  stopifnot(methods::is(obj, "AgentOmics"))

  if (is.null(title)) {
    title <- sprintf("%s (stage: %s)", header_title_prefix, obj@stage)
  }

  head <- .report_css(title)

  n_decisions <- length(obj@decisions)
  n_figures   <- nrow(obj@figures)

  overview_rows <- list(
    c("Stage",          esc_html(obj@stage)),
    c("Data type",      esc_html(obj@data_type)),
    c("Cells (total)",  as.character(.safe_total_cells(obj))),
    c("Genes",          as.character(.safe_genes(obj))),
    c("Decisions",      as.character(n_decisions)),
    c("Figures",        as.character(n_figures)),
    c("Created",        format(obj@created_at, "%Y-%m-%d %H:%M:%S")),
    c("Last updated",   format(obj@updated_at, "%Y-%m-%d %H:%M:%S"))
  )
  overview_html <- paste0(
    '<table class="overview"><tbody>',
    paste(vapply(overview_rows, function(r)
      sprintf('<tr><th>%s</th><td>%s</td></tr>', r[1], r[2]),
      character(1)), collapse = "\n"),
    '</tbody></table>'
  )

  decisions_html <- .render_decisions_html(obj@decisions, include_params)
  gallery_html   <- .render_figure_gallery(obj@figures)
  tokens_html    <- .render_token_usage(obj)

  # Domain-specific extras (e.g. LLM annotations, author comparison).
  extras_html <- if (length(extras) > 0) {
    pieces <- lapply(extras, function(fn) {
      if (!is.function(fn)) return("")
      out <- tryCatch(fn(obj), error = function(e) {
        sprintf('<section><h2>(renderer error)</h2><p>%s</p></section>',
                esc_html(conditionMessage(e)))
      })
      if (is.null(out)) "" else as.character(out)
    })
    paste(unlist(pieces), collapse = "\n")
  } else ""

  script_html <- if (isTRUE(include_script)) {
    full <- paste(obj@scripts, collapse = "\n\n")
    sprintf(
      '<section><h2>Reproducible script</h2><pre class="script"><code>%s</code></pre></section>',
      esc_html(full)
    )
  } else ""

  body <- paste0(
    '<main>',
    sprintf('<h1>%s</h1>', esc_html(title)),
    '<section><h2>Overview</h2>', overview_html, '</section>',
    '<section><h2>Decision log</h2>', decisions_html, '</section>',
    '<section><h2>Figures</h2>', gallery_html, '</section>',
    tokens_html,
    extras_html,
    script_html,
    .report_footer(),
    '</main>'
  )

  html <- paste0('<!DOCTYPE html>\n<html lang="en">\n',
                 head, '\n<body>\n', body, '\n</body>\n</html>\n')

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, con = path, useBytes = TRUE)
  message(sprintf("[render_report] wrote %s (%s KB)",
                  path, format(round(file.info(path)$size / 1024, 1))))
  invisible(path)
}


# v0.1.0: lightweight LLM token usage section. Produces a small table
# summarising per-step consumption from obj@token_usage. Empty if no LLM
# calls were made.
.render_token_usage <- function(obj) {
  tu <- obj@token_usage
  if (length(tu) == 0) return("")
  rows <- vapply(names(tu), function(step) {
    x <- tu[[step]]
    sprintf(
      '<tr><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
      esc_html(step),
      as.character(x$n_calls       %||% 0L),
      as.character(x$input_tokens  %||% 0L),
      as.character(x$output_tokens %||% 0L),
      as.character(x$cached_tokens %||% 0L)
    )
  }, character(1))
  paste0(
    '<section><h2>LLM token usage</h2>',
    '<table><thead><tr><th>Step</th><th>Calls</th>',
    '<th>Input</th><th>Output</th><th>Cached</th></tr></thead>',
    '<tbody>',
    paste(rows, collapse = "\n"),
    '</tbody></table></section>'
  )
}

# ---- Internal report helpers -----------------------------------------------

.report_css <- function(title) {
  paste0(
    '<head>\n<meta charset="utf-8">\n',
    sprintf('<title>%s</title>\n', esc_html(title)),
    '<style>
      :root {
        --fg: #1e1e1e; --bg: #fafafa; --border: #d9d9d9;
        --accent: #2a6f97; --muted: #666; --code-bg: #f3f3f3;
        --table-head: #eef2f5;
      }
      body { font-family: -apple-system, BlinkMacSystemFont, "Segoe UI",
             Helvetica, Arial, sans-serif; color: var(--fg);
             background: var(--bg); margin: 0; padding: 0; }
      main { max-width: 980px; margin: 0 auto; padding: 2rem 1.5rem 4rem; }
      h1 { color: var(--accent); border-bottom: 2px solid var(--accent);
           padding-bottom: .3em; margin-top: 0; }
      h2 { color: var(--accent); margin-top: 2.2em; border-bottom:
           1px solid var(--border); padding-bottom: .25em; }
      h3 { margin-bottom: .25em; }
      section { margin-bottom: 1.2em; }
      table { border-collapse: collapse; width: 100%; margin: .6em 0; }
      th, td { border: 1px solid var(--border); padding: .45em .7em;
               text-align: left; vertical-align: top; font-size: .92rem; }
      th { background: var(--table-head); font-weight: 600; }
      table.overview th { width: 160px; }
      table.decisions td.params { font-family: ui-monospace, SFMono-Regular,
        Menlo, Consolas, monospace; font-size: .82rem;
        white-space: pre-wrap; word-break: break-word; max-width: 540px; }
      pre.script { background: var(--code-bg); border: 1px solid var(--border);
                   padding: 1em; border-radius: 4px; overflow-x: auto;
                   font-size: .82rem; line-height: 1.4;
                   max-height: 620px; overflow-y: auto; }
      pre.script code { font-family: ui-monospace, SFMono-Regular, Menlo,
                        Consolas, monospace; }
      .fig { margin: 1.5em 0; }
      .fig img { max-width: 100%; border: 1px solid var(--border);
                 border-radius: 3px; }
      .fig .caption { color: var(--muted); font-size: .88rem;
                      margin-top: .3em; }
      .rationale { color: var(--muted); font-size: .9rem; }
      .badge { display: inline-block; padding: 1px 8px; border-radius: 9px;
               font-size: .75rem; font-weight: 600;
               background: #e7f0f7; color: var(--accent); }
      .badge.warn { background: #fff3cd; color: #8a6d0f; }
      .badge.err  { background: #fbe3e4; color: #a12f2f; }
      tr.row-warn td { background: #fffbeb; }
      tr.row-err  td { background: #fdecea; }
      table small { color: var(--muted); font-size: .78rem; }
      footer { color: var(--muted); font-size: .85rem; margin-top: 3em;
               border-top: 1px solid var(--border); padding-top: 1em; }
      details { margin-top: .5em; }
      summary { cursor: pointer; font-size: .85rem; color: var(--accent); }
    </style>\n</head>'
  )
}

.report_footer <- function() {
  sprintf(
    '<footer>Generated by <code>scAgentKit::report_html()</code> on %s.</footer>',
    format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  )
}

# HTML-escape a string. Internal -- keeps us free of xml2 / htmltools deps.
#' Escape a string for safe inclusion in HTML
#'
#' Exported so that downstream packages' report-section extras can use
#' the same escaping convention as [render_report()] does internally.
#'
#' @param x A character vector (or coercible to one).
#' @return A character vector of the same length, with `&`, `<`, `>`,
#'   `"`, and `'` replaced by their HTML entities.
#' @export
esc_html <- function(x) {
  if (is.null(x)) return("")
  x <- as.character(x)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

.safe_total_cells <- function(obj) {
  tryCatch({
    if (obj@data_type == "seurat") ncol(obj@data)
    else sum(vapply(obj@data, function(s) as.integer(ncol(s)), integer(1)))
  }, error = function(e) NA)
}

.safe_genes <- function(obj) {
  tryCatch({
    if (obj@data_type == "seurat") nrow(obj@data)
    else nrow(obj@data[[1]])
  }, error = function(e) NA)
}

.format_params_html <- function(params) {
  if (length(params) == 0) return("")
  # Pretty-print using jsonlite if available; otherwise fall back to
  # a simple name = value listing.
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    txt <- tryCatch(
      jsonlite::toJSON(params, auto_unbox = TRUE, pretty = TRUE, null = "null"),
      error = function(e) NULL
    )
    if (!is.null(txt)) return(esc_html(txt))
  }
  lines <- vapply(names(params), function(nm) {
    val <- params[[nm]]
    repr <- if (length(val) == 0) "<empty>" else paste(val, collapse = ", ")
    sprintf("%s = %s", nm, repr)
  }, character(1))
  esc_html(paste(lines, collapse = "\n"))
}

.render_decisions_html <- function(decisions, include_params) {
  if (length(decisions) == 0) {
    return('<p class="rationale">No decisions recorded yet.</p>')
  }
  rows <- vapply(seq_along(decisions), function(i) {
    d <- decisions[[i]]
    badge <- if (isTRUE(d$success)) '<span class="badge">ok</span>'
             else '<span class="badge err">failed</span>'
    params_cell <- if (isTRUE(include_params)) {
      sprintf('<td class="params">%s</td>', .format_params_html(d$params))
    } else ''
    sprintf(
      '<tr><td>%d</td><td>%s</td><td><code>%s</code></td><td>%s</td><td>%s</td>%s</tr>',
      i,
      format(d$timestamp, "%Y-%m-%d %H:%M:%S"),
      esc_html(d$function_name),
      badge,
      esc_html(d$rationale %||% ""),
      params_cell
    )
  }, character(1))
  params_header <- if (isTRUE(include_params)) '<th>Params</th>' else ''
  paste0(
    '<table class="decisions"><thead><tr>',
    '<th>#</th><th>Time</th><th>Function</th>',
    '<th>Status</th><th>Rationale</th>', params_header,
    '</tr></thead><tbody>',
    paste(rows, collapse = "\n"),
    '</tbody></table>'
  )
}

.render_figure_gallery <- function(figures) {
  if (nrow(figures) == 0) {
    return('<p class="rationale">No figures registered.</p>')
  }
  items <- vapply(seq_len(nrow(figures)), function(i) {
    row <- figures[i, ]
    embedded <- .embed_image(row$path)
    if (is.null(embedded)) {
      sprintf(
        '<div class="fig"><div class="caption"><strong>%s</strong><br>%s<br><em>(image not found: %s)</em></div></div>',
        esc_html(row$step), esc_html(row$description), esc_html(row$path)
      )
    } else {
      sprintf(
        '<div class="fig"><img src="%s" alt="%s" /><div class="caption"><strong>%s</strong> &mdash; %s</div></div>',
        embedded, esc_html(row$description),
        esc_html(row$step), esc_html(row$description)
      )
    }
  }, character(1))
  paste(items, collapse = "\n")
}

.embed_image <- function(path) {
  if (!file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  mime <- switch(ext,
                 png = "image/png",
                 jpg = "image/jpeg", jpeg = "image/jpeg",
                 webp = "image/webp", gif = "image/gif",
                 svg = "image/svg+xml",
                 NULL)
  if (is.null(mime)) return(NULL)
  b64 <- tryCatch({
    if (requireNamespace("base64enc", quietly = TRUE)) {
      base64enc::base64encode(path)
    } else if (requireNamespace("openssl", quietly = TRUE)) {
      openssl::base64_encode(readBin(path, "raw",
                                     n = file.info(path)$size))
    } else {
      return(NULL)
    }
  }, error = function(e) NULL)
  if (is.null(b64)) return(NULL)
  sprintf("data:%s;base64,%s", mime, b64)
}

