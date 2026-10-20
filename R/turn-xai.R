#' One native xAI Responses turn
#'
#' Sends one text/function-call request without retries, redirects, server tools,
#' stored-response references or automatic conversation compaction. The caller
#' supplies the complete native input array and owns tool execution, privacy,
#' authorization and append-only history. Returned output items, including
#' encrypted reasoning, must be preserved unchanged for the next input.
#'
#' This is a protocol adapter, not a claim of live model qualification.
#' `max_tokens` limits visible output, not all reasoning or billed tokens.
#' @param model Exact model ID; default `"grok-4.7"`.
#' @param api_key API key, or NULL to read `api_key_env` on dispatch only.
#' @param api_key_env Credential environment variable.
#' @param max_tokens Positive visible-output limit.
#' @param timeout_secs Positive per-request timeout.
#' @param temperature Sampling temperature between zero and two.
#' @param effort Reasoning effort: low, medium, high or xhigh.
#' @return A function accepting `input`, OpenAI-shaped `tools = NULL`,
#'   `max_tokens`, `timeout_secs` and `prompt_cache_key = NULL`. Preparation uses
#'   [prepare_turn_request()] without reading a key. Results use
#'   `agentomics.turn.v2`: a convenience assistant message plus the complete
#'   `protocol$output` native array. The message alone is not replay history.
#'   Usage is reported, never estimated; unknown counts remain missing.
#' @export
turn_grok <- function(model = "grok-4.7", api_key = NULL,
                      api_key_env = "XAI_API_KEY", max_tokens = 2000L,
                      timeout_secs = 180, temperature = .7, effort = "high") {
  .turn_require(requireNamespace("httr2", quietly = TRUE) &&
    requireNamespace("jsonlite", quietly = TRUE), "httr2 and jsonlite are required.")
  .turn_require(.turn_string(model) && .turn_string(api_key_env) &&
    (is.null(api_key) || .turn_string(api_key)), "Invalid model or credential configuration.")
  .turn_require(.turn_string(effort) && effort %in% c("low", "medium", "high", "xhigh"),
    "Unsupported reasoning effort.")
  .turn_generation(max_tokens, temperature, timeout_secs)
  default_max <- max_tokens; default_timeout <- timeout_secs
  prepare <- function(input, tools = NULL, max_tokens = default_max,
                      timeout_secs = default_timeout, prompt_cache_key = NULL) {
    .turn_generation(max_tokens, temperature, timeout_secs)
    .xai_items(input, history = TRUE)
    .turn_validate_tools(tools, if (is.null(tools)) NULL else "auto",
      if (is.null(tools)) NULL else FALSE)
    .turn_require(is.null(prompt_cache_key) ||
      (.turn_string(prompt_cache_key) && nchar(prompt_cache_key, type = "bytes") <= 128L),
      "Invalid prompt cache key.")
    body <- list(model = model, input = input, store = FALSE, stream = FALSE,
      include = list("reasoning.encrypted_content"), max_output_tokens = max_tokens,
      temperature = temperature, reasoning = list(effort = effort))
    if (!is.null(tools)) {
      body$tools <- lapply(tools, function(tool) {
        fn <- tool[["function"]]
        # xAI does not enforce the compatibility strict flag. Local validators
        # remain responsible; do not imply a server guarantee.
        out <- list(type = "function", name = fn$name,
          parameters = .turn_schema_json(fn$parameters))
        if (!is.null(fn$description)) out$description <- fn$description
        out
      })
      body$tool_choice <- "auto"; body$parallel_tool_calls <- FALSE
    }
    if (!is.null(prompt_cache_key)) body$prompt_cache_key <- prompt_cache_key
    json <- as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null",
      digits = NA, force = TRUE))
    .turn_require(nchar(json, type = "bytes") <= 4000000L, "Native request exceeds its size limit.")
    list(body = body, json = json)
  }
  turn <- function(input, tools = NULL, max_tokens = default_max,
                   timeout_secs = default_timeout, prompt_cache_key = NULL) {
    request <- prepare(input, tools, max_tokens, timeout_secs, prompt_cache_key)
    key <- if (is.null(api_key)) Sys.getenv(api_key_env) else api_key
    .turn_require(.turn_string(key), "A non-empty API key is required before dispatch.")
    req <- httr2::request("https://api.x.ai/v1/responses") |>
      httr2::req_headers(Authorization = paste("Bearer", key)) |>
      httr2::req_body_raw(charToRaw(enc2utf8(request$json)), type = "application/json") |>
      httr2::req_timeout(timeout_secs) |>
      httr2::req_options(followlocation = FALSE) |>
      httr2::req_error(is_error = function(response) FALSE)
    response <- tryCatch(httr2::req_perform(req), error = function(e)
      .turn_abort("transport_error", "Responses transport failed; delivery is unknown.", "unknown"))
    .xai_response(response, model, key)
  }
  attr(turn, "agentomics_prepare") <- prepare
  turn
}

# Shape validation never rewrites an item. Complete item bytes/ordering are
# owned by the coordinator; opaque reasoning is protocol state, not evidence.
.xai_items <- function(items, history = FALSE) {
  .turn_require(.turn_array(items) && length(items) > 0L &&
    length(items) <= 1000L && .turn_json_value(items), "Invalid native item array.")
  pending <- character(); seen <- character()
  for (item in items) {
    .turn_require(.turn_object(item), "Native items must be JSON objects.")
    type <- item$type %||% if (history && !is.null(item$role)) "message" else ""
    .turn_require(.turn_string(type), "Missing native item type.")
    if (type == "function_call") {
      .turn_require(.turn_string(item$call_id) && !item$call_id %in% seen &&
        .turn_string(item$name) && grepl("^[A-Za-z0-9_-]{1,64}$", item$name) &&
        .turn_string(item$arguments, empty = TRUE), "Invalid or duplicate function call.")
      seen <- c(seen, item$call_id); pending <- c(pending, item$call_id)
    } else if (type == "function_call_output" && history) {
      .turn_require(.turn_string(item$call_id) && item$call_id %in% pending &&
        .turn_string(item$output, empty = TRUE), "Tool result has no unique pending call.")
      pending <- setdiff(pending, item$call_id)
    } else if (type == "reasoning") {
      .turn_require(.turn_array(item$summary) &&
        (is.null(item$encrypted_content) || .turn_string(item$encrypted_content, empty = TRUE)),
        "Invalid reasoning item.")
    } else if (type == "message") {
      .turn_require(.turn_string(item$role) &&
        item$role %in% if (history) c("system", "user", "assistant") else "assistant",
        "Invalid native message role.")
      if (history && item$role != "assistant")
        .turn_require(!length(pending), "Complete pending function results before another user message.")
      if (is.character(item$content)) {
        .turn_require(history && item$role != "assistant" && .turn_string(item$content, empty = TRUE),
          "Assistant output must retain its content blocks.")
      } else {
        .turn_require(.turn_array(item$content), "Native message content must be text blocks.")
        for (block in item$content) {
          .turn_require(.turn_object(block) && .turn_string(block$type) &&
            block$type %in% if (item$role == "assistant") c("output_text", "refusal") else "input_text",
            "Only text/refusal blocks are supported.")
          .turn_require(.turn_string(if (block$type == "refusal") block$refusal else block$text,
            empty = TRUE), "Invalid native text.")
        }
      }
    } else .turn_abort("unsupported_item", "Unsupported native protocol item; nothing was silently dropped.")
  }
  if (history) .turn_require(!length(pending), "Every function call needs one result before sending.")
  invisible(TRUE)
}

.xai_usage <- function(raw) {
  if (is.null(raw)) return(NULL)
  if (!.turn_object(raw)) return(FALSE)
  values <- list(input_tokens = raw$input_tokens, output_tokens = raw$output_tokens,
    total_tokens = raw$total_tokens, cached_tokens = raw$input_tokens_details$cached_tokens,
    reasoning_tokens = raw$output_tokens_details$reasoning_tokens)
  if (!all(vapply(values, function(x) is.null(x) ||
    (.turn_number(x) && x >= 0 && x == floor(x) && x <= .Machine$integer.max), logical(1))))
    return(FALSE)
  # Native totals may include reasoning outside output_tokens. Do not impose
  # the Chat Completions arithmetic or double-count a provider-specific total.
  lapply(values, function(x) if (is.null(x)) NA_integer_ else as.integer(x))
}

.xai_response <- function(response, model, secret) {
  status <- httr2::resp_status(response)
  parsed <- tryCatch(httr2::resp_body_json(response, simplifyVector = FALSE), error = function(e) NULL)
  if (!.turn_object(parsed)) parsed <- NULL
  safe <- function(x) if (.turn_string(x) && nchar(x, type = "bytes") <= 200L &&
    grepl("^[A-Za-z0-9_.:-]+$", x) && !grepl(secret, x, fixed = TRUE)) x else NULL
  metadata <- list(id = safe(parsed$id), model = safe(parsed$model),
    request_id = safe(httr2::resp_header(response, "x-request-id")), http_status = status,
    protocol = "xai.responses.v1")
  usage <- tryCatch(.xai_usage(parsed$usage), error = function(e) FALSE)
  raw <- .turn_raw_usage(parsed$usage)
  ticks <- if (.turn_object(parsed$usage)) parsed$usage$cost_in_usd_ticks else NULL
  if (.turn_number(ticks) && ticks >= 0 && ticks == floor(ticks) && ticks <= 2^53 - 1)
    raw$cost_in_usd_ticks <- ticks
  if (is.list(usage)) token_record(provider = "xai:responses", model = metadata$model %||% model,
    input_tokens = usage$input_tokens, output_tokens = usage$output_tokens,
    cached_tokens = usage$cached_tokens, call_type = "structured_turn")
  fail <- function(code, message) {
    problem <- tryCatch(.turn_abort(code, message, "received", status,
      if (is.list(usage)) usage else NULL, metadata, raw), agentomics_turn_error = identity)
    # Unknown output is not usable history, but a bounded original can still
    # be inspected by the caller's privacy validator and saved as audit data.
    if (.turn_array(parsed$output) && .turn_json_value(parsed$output)) {
      encoded <- jsonlite::toJSON(parsed$output, auto_unbox = TRUE, null = "null", digits = NA)
      if (nchar(encoded, type = "bytes") <= 4000000L)
        problem$protocol <- list(schema = "xai.responses.v1", output = parsed$output)
    }
    stop(problem)
  }
  if (status < 200 || status >= 300) fail("http_error", "Responses endpoint returned a non-success status.")
  if (is.null(parsed)) fail("invalid_response", "Responses endpoint returned invalid JSON.")
  if (identical(usage, FALSE)) fail("invalid_usage", "Responses endpoint returned malformed usage.")
  if (!.turn_string(parsed$status) || !parsed$status %in% c("completed", "incomplete", "failed"))
    fail("invalid_response", "Unexpected synchronous response status.")
  valid <- tryCatch({ .xai_items(parsed$output); TRUE }, error = function(e) FALSE)
  if (!valid) fail("invalid_response", "Unsupported or malformed output; native history cannot continue.")
  encoded <- jsonlite::toJSON(parsed$output, auto_unbox = TRUE, null = "null", digits = NA)
  if (nchar(encoded, type = "bytes") > 4000000L)
    fail("invalid_response", "Native response exceeds its size limit.")
  text <- character(); calls <- list(); refusal <- FALSE
  for (item in parsed$output) {
    if (item$type == "function_call") calls <- append(calls, list(list(id = item$call_id,
      type = "function", `function` = list(name = item$name, arguments = item$arguments))))
    if (item$type == "message") for (block in item$content) {
      if (block$type == "refusal") refusal <- TRUE
      if (block$type == "output_text") text <- c(text, block$text)
    }
  }
  finish <- if (refusal) "refusal" else if (parsed$status != "completed") parsed$status else
    if (length(calls)) "tool_calls" else "stop"
  if (finish == "stop" && !any(nzchar(text))) fail("invalid_response", "No final text or function call.")
  message <- list(role = "assistant", content = if (length(text)) paste(text, collapse = "\n") else NULL)
  if (length(calls)) message$tool_calls <- calls
  structure(list(schema_version = "agentomics.turn.v2", message = message,
    finish_reason = finish, usage = usage, raw_usage = raw, metadata = metadata,
    protocol = list(schema = "xai.responses.v1", output = parsed$output,
      status = parsed$status, incomplete_details = parsed$incomplete_details)),
    class = c("agentomics_turn", "list"))
}
