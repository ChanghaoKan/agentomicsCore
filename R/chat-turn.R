#' Build a single structured OpenAI-compatible chat turn
#'
#' Unlike the text-only chat factories, this interface accepts the complete
#' text conversation, including assistant function calls and tool results.
#' It performs one non-streaming HTTP request, does not retry or follow
#' redirects, and never executes tools. Callers own tool argument validation,
#' authorization, budgets, execution, conversation state, and retry decisions.
#'
#' @param base_url Endpoint base URL, without credentials, query or fragment.
#' @param model Model identifier supported by the endpoint.
#' @param api_key API key; NULL reads `api_key_env`.
#' @param api_key_env API key environment variable. Use `""` for a local
#'   endpoint requiring no key.
#' @param max_tokens Positive integer output limit, overridable per turn.
#' @param temperature Number between 0 and 2. Zero does not guarantee identical
#'   responses across repeated provider requests.
#' @param timeout_secs Positive request timeout, overridable per turn.
#' @param extra_headers Optional named character vector of HTTP headers.
#'   Authorization and Content-Type cannot be overridden.
#' @param extra_body Optional named list of provider-specific JSON fields.
#'   Protocol fields, including messages, tools and generation parameters,
#'   cannot be overridden.
#'
#' @return A function with arguments `messages`, `tools = NULL`,
#'   `tool_choice = NULL`, `parallel_tool_calls = NULL`, `max_tokens`, and
#'   `timeout_secs`. It returns a list of class `agentomics_turn` with
#'   `schema_version`, `message` (assistant role, nullable content, and optional
#'   tool_calls), `finish_reason`, `usage`, and `metadata`.
#'
#'   Messages are an unnamed list of named text-message lists. Supported roles
#'   are system, user, assistant, and tool. Every assistant tool call must have
#'   exactly one following tool result, identified by `tool_call_id`, before
#'   another non-tool message or the next request. Tool call arguments remain
#'   JSON strings: the caller must parse and validate them against its schema.
#'   Unknown tool names and invalid argument JSON in responses are deliberately
#'   returned so the caller can provide a structured error to the model.
#'
#'   `usage` is NULL when absent, otherwise a whitelist of provider-returned
#'   token counts; missing counts are NA, never estimated. `metadata` contains
#'   only response id, model, created, system_fingerprint, request_id,
#'   http_status, and a recognized finish_reason. No raw response, request,
#'   headers, key or endpoint is returned.
#'   Identifiers are character-limited and exclude configured key values.
#'   Treat response metadata as untrusted provider text. `raw_usage` preserves
#'   only known numeric usage fields for accounting audits.
#'
#'   Errors have class `agentomics_turn_error` and fields `code`, `delivery`,
#'   `http_status`, `usage`, numeric `raw_usage`, and `metadata`. Delivery is `"not_sent"` for local
#'   validation errors, `"unknown"` for transport failures, and `"received"`
#'   after an HTTP response. An unknown delivery must not be automatically
#'   retried. Returned usage is recorded even for a tool-only response or an
#'   invalid completion when its usage block is valid.
#' @seealso [prepare_turn_request()], [turn_deepseek()],
#'   [make_chat_fn_openai_compatible()]
#' @md
#' @export
make_turn_fn_openai_compatible <- function(
    base_url, model, api_key = NULL, api_key_env = "OPENAI_API_KEY",
    max_tokens = 2000L, temperature = 0, timeout_secs = 180,
    extra_headers = NULL, extra_body = NULL) {
  .turn_require(requireNamespace("httr2", quietly = TRUE) &&
                  requireNamespace("jsonlite", quietly = TRUE),
                "httr2 and jsonlite are required for structured turns.")
  .turn_require(.turn_string(base_url) &&
                  grepl("^https?://[^/?#@[:space:]]+(/[^?#[:space:]]*)?$", base_url),
                "base_url must be an HTTP(S) URL without credentials, query or fragment.")
  .turn_require(.turn_string(model), "model must be a non-empty string.")
  .turn_require(.turn_string(api_key_env, empty = TRUE),
                "api_key_env must be a string.")
  .turn_require(is.null(api_key) || .turn_string(api_key, empty = TRUE),
                "api_key must be a string or NULL.")
  .turn_generation(max_tokens, temperature, timeout_secs)
  if (!is.null(extra_headers)) {
    .turn_require(is.character(extra_headers) && !anyNA(extra_headers) &&
                    .turn_names(extra_headers) && !anyDuplicated(tolower(names(extra_headers))) &&
                    !any(tolower(names(extra_headers)) %in%
                           c("authorization", "content-type")) &&
                    !any(grepl("[\r\n]", c(names(extra_headers), extra_headers))),
                  "extra_headers must be uniquely named and cannot override authentication.")
  }
  if (!is.null(extra_body)) {
    .turn_require(.turn_object(extra_body), "extra_body must be a named JSON object.")
    reserved <- c("model", "messages", "tools", "tool_choice", "parallel_tool_calls",
                  "max_tokens", "max_completion_tokens", "temperature", "stream", "n")
    .turn_require(!any(names(extra_body) %in% reserved),
                  "extra_body cannot override structured-turn protocol fields.")
    .turn_require(.turn_json_value(extra_body), "extra_body contains a non-JSON value.")
  }
  base_url <- sub("/+$", "", base_url)
  default_max_tokens <- max_tokens
  default_timeout <- timeout_secs
  prepare <- function(messages, tools = NULL, tool_choice = NULL, parallel_tool_calls = NULL,
                      max_tokens = default_max_tokens, timeout_secs = default_timeout) {
    .turn_generation(max_tokens, temperature, timeout_secs)
    .turn_validate_messages(messages)
    .turn_validate_tools(tools, tool_choice, parallel_tool_calls)
    body <- c(list(model = model, messages = messages, max_tokens = max_tokens,
                   temperature = temperature, stream = FALSE, n = 1L), extra_body)
    if (!is.null(tools)) {
      body$tools <- lapply(tools, function(tool) {
        tool[["function"]]$parameters <- .turn_schema_json(tool[["function"]]$parameters)
        tool
      })
    }
    if (!is.null(tool_choice)) body$tool_choice <- tool_choice
    if (!is.null(parallel_tool_calls)) body$parallel_tool_calls <- parallel_tool_calls
    list(body = body,
         json = as.character(jsonlite::toJSON(body, auto_unbox = TRUE, null = "null",
                                              digits = NA, force = TRUE)))
  }
  turn <- function(messages, tools = NULL, tool_choice = NULL, parallel_tool_calls = NULL,
                   max_tokens = default_max_tokens, timeout_secs = default_timeout) {
    prepared <- prepare(messages, tools, tool_choice, parallel_tool_calls, max_tokens, timeout_secs)
    key <- if (is.null(api_key)) {
      if (nzchar(api_key_env)) Sys.getenv(api_key_env) else ""
    } else api_key
    .turn_require(.turn_string(key, empty = !nzchar(api_key_env)),
                  "A non-empty API key is required before dispatch.")
    headers <- c(`Content-Type` = "application/json",
                 if (nzchar(key)) c(Authorization = paste("Bearer", key)),
                 extra_headers)
    req <- httr2::request(base_url) |>
      httr2::req_url_path_append("chat", "completions") |>
      httr2::req_headers(!!!as.list(headers)) |>
      httr2::req_body_raw(charToRaw(enc2utf8(prepared$json)), type = "application/json") |>
      httr2::req_timeout(timeout_secs) |>
      httr2::req_options(followlocation = FALSE) |>
      httr2::req_error(is_error = function(r) FALSE)
    resp <- tryCatch(httr2::req_perform(req), error = function(e) {
      .turn_abort("transport_error", "Structured-turn transport failed; delivery is unknown.",
                  delivery = "unknown")
    })
    .turn_parse_response(resp, model, secrets = key)
  }
  attr(turn, "agentomics_prepare") <- prepare
  turn
}

#' Prepare the exact JSON body of a structured turn without dispatch
#'
#' Uses the same validation and serialization as the turn function. No key
#' lookup or network operation occurs. The returned JSON is the exact UTF-8
#' request body used when dispatching with the same arguments; callers can
#' hash it for authorization before dispatch.
#'
#' @param turn_fn A function from [make_turn_fn_openai_compatible()] or
#'   [turn_deepseek()] or [turn_grok()].
#' @param ... The same arguments passed to the structured turn function.
#' @return A list with `body` (the JSON-ready request body) and `json` (the
#'   exact serialized request body). It has no key, headers, or endpoint.
#' @md
#' @export
prepare_turn_request <- function(turn_fn, ...) {
  prepare <- attr(turn_fn, "agentomics_prepare", exact = TRUE)
  .turn_require(is.function(turn_fn) && is.function(prepare),
                "turn_fn must be an agentomicsCore structured-turn function.")
  prepare(...)
}

#' DeepSeek structured text turns with thinking disabled
#'
#' A preset of [make_turn_fn_openai_compatible()] that fixes temperature to
#' zero and sends `thinking = list(type = "disabled")`. This is the supported
#' provider preset for structured turns in version 0.2.0; no Grok structured
#' preset is provided. Availability and tool support of a selected model still
#' depend on the provider. Tests use mocked responses, not paid API requests.
#'
#' @param model Model identifier; default `"deepseek-flash"`.
#' @param api_key API key, or NULL to read `api_key_env`.
#' @param api_key_env Default `"DEEPSEEK_API_KEY"`.
#' @param max_tokens Positive integer output limit.
#' @param timeout_secs Positive per-request timeout in seconds.
#' @return A structured turn function; see [make_turn_fn_openai_compatible()].
#' @md
#' @export
turn_deepseek <- function(model = "deepseek-flash", api_key = NULL,
                          api_key_env = "DEEPSEEK_API_KEY", max_tokens = 2000L,
                          timeout_secs = 180) {
  make_turn_fn_openai_compatible(
    base_url = "https://api.deepseek.com/v1", model = model,
    api_key = api_key, api_key_env = api_key_env,
    max_tokens = max_tokens, timeout_secs = timeout_secs, temperature = 0,
    extra_body = list(thinking = list(type = "disabled"))
  )
}

.turn_abort <- function(code, message, delivery = "not_sent", http_status = NULL,
                        usage = NULL, metadata = NULL, raw_usage = NULL) {
  stop(structure(list(message = message, call = NULL, code = code,
                      delivery = delivery, http_status = http_status,
                      usage = usage, metadata = metadata, raw_usage = raw_usage,
                      response_metadata = metadata),
                 class = c("agentomics_turn_error", "error", "condition")))
}

.turn_require <- function(ok, message) {
  if (!isTRUE(ok)) .turn_abort("invalid_request", message)
  invisible(NULL)
}

.turn_string <- function(x, empty = FALSE) {
  is.character(x) && length(x) == 1L && !is.na(x) &&
    (empty || nzchar(trimws(x)))
}

.turn_names <- function(x) {
  length(x) == 0L || (!is.null(names(x)) && !anyNA(names(x)) &&
                       all(nzchar(names(x))) && !anyDuplicated(names(x)))
}

.turn_object <- function(x) is.list(x) && !is.data.frame(x) && .turn_names(x)
.turn_array <- function(x) is.list(x) && !is.data.frame(x) && is.null(names(x))
.turn_number <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
.turn_bool <- function(x) is.logical(x) && length(x) == 1L && !is.na(x)
.turn_fields <- function(x, allowed, required = character()) {
  .turn_object(x) && all(names(x) %in% allowed) && all(required %in% names(x))
}

.turn_json_value <- function(x) {
  if (is.null(x)) return(TRUE)
  if (is.list(x) && !is.object(x)) {
    return((is.null(names(x)) || .turn_names(x)) &&
             all(vapply(x, .turn_json_value, logical(1))))
  }
  is.atomic(x) && !is.object(x) && !anyNA(x) &&
    (is.character(x) || is.logical(x) || (is.numeric(x) && all(is.finite(x))))
}

.turn_schema_json <- function(schema) {
  if (!is.list(schema)) return(schema)
  if (!length(schema)) return(stats::setNames(list(), character()))
  out <- schema
  for (field in intersect(names(out), c("required", "enum"))) {
    out[[field]] <- unname(as.list(out[[field]]))
  }
  for (field in intersect(names(out), c("allOf", "anyOf", "oneOf", "prefixItems"))) {
    out[[field]] <- unname(lapply(out[[field]], .turn_schema_json))
  }
  for (field in intersect(names(out), c("properties", "patternProperties",
                                        "$defs", "definitions", "dependentSchemas"))) {
    out[[field]] <- lapply(out[[field]], .turn_schema_json)
    if (!length(out[[field]])) out[[field]] <- stats::setNames(list(), character())
  }
  for (field in intersect(names(out), c("items", "additionalProperties",
                                        "unevaluatedProperties", "contains", "not",
                                        "if", "then", "else", "propertyNames"))) {
    if (.turn_object(out[[field]])) out[[field]] <- .turn_schema_json(out[[field]])
  }
  out
}

.turn_generation <- function(max_tokens, temperature, timeout_secs) {
  .turn_require(.turn_number(max_tokens) && max_tokens > 0 &&
                  max_tokens == floor(max_tokens) && max_tokens <= .Machine$integer.max,
                "max_tokens must be a positive integer.")
  .turn_require(.turn_number(temperature) && temperature >= 0 && temperature <= 2,
                "temperature must be a number in [0, 2].")
  .turn_require(.turn_number(timeout_secs) && timeout_secs > 0,
                "timeout_secs must be positive.")
}

.turn_valid_calls <- function(calls) {
  if (!.turn_array(calls) || !length(calls)) return(FALSE)
  valid <- vapply(calls, function(x) {
    .turn_fields(x, c("id", "type", "function"), c("id", "type", "function")) &&
      .turn_string(x$id) && identical(x$type, "function") &&
      .turn_fields(x[["function"]], c("name", "arguments"), c("name", "arguments")) &&
      .turn_string(x[["function"]]$name) && .turn_string(x[["function"]]$arguments, empty = TRUE)
  }, logical(1))
  all(valid) && !anyDuplicated(vapply(calls, function(x) x$id, character(1)))
}

.turn_validate_messages <- function(messages) {
  .turn_require(.turn_array(messages) && length(messages) > 0L,
                "messages must be a non-empty unnamed list.")
  pending <- seen <- character()
  for (msg in messages) {
    .turn_require(.turn_object(msg) && .turn_string(msg[["role"]]) &&
                    msg[["role"]] %in% c("system", "user", "assistant", "tool"),
                  "Each message must have a supported role.")
    allowed <- switch(msg[["role"]],
                      assistant = c("role", "content", "tool_calls"),
                      tool = c("role", "content", "tool_call_id"),
                      c("role", "content"))
    .turn_require(.turn_fields(msg, allowed, c("role", "content")),
                  "Message contains missing or unsupported fields.")
    if (identical(msg[["role"]], "tool")) {
      .turn_require(.turn_string(msg[["content"]], empty = TRUE) &&
                      .turn_string(msg[["tool_call_id"]]) && msg[["tool_call_id"]] %in% pending,
                    "Tool result must match an outstanding tool_call_id.")
      pending <- setdiff(pending, msg[["tool_call_id"]])
    } else {
      .turn_require(!length(pending), "All pending tool calls need results before continuing.")
      calls <- msg[["tool_calls"]]
      if (!is.null(calls)) {
        .turn_require(.turn_valid_calls(calls), "Invalid assistant tool_calls.")
        ids <- vapply(calls, function(x) x$id, character(1))
        .turn_require(!any(ids %in% seen), "Tool call identifiers must be unique in the conversation.")
        seen <- c(seen, ids)
        pending <- ids
      }
      .turn_require(.turn_string(msg[["content"]], empty = TRUE) ||
                      (identical(msg[["role"]], "assistant") && is.null(msg[["content"]]) &&
                         length(pending) > 0L),
                    "Message content must be text, or null for an assistant tool call.")
    }
  }
  .turn_require(!length(pending), "All pending tool calls need results before requesting a turn.")
}

.turn_validate_tools <- function(tools, tool_choice, parallel_tool_calls) {
  if (is.null(tools)) {
    .turn_require(is.null(tool_choice) && is.null(parallel_tool_calls),
                  "Tool options require tools.")
    return(invisible(NULL))
  }
  .turn_require(.turn_array(tools) && length(tools) > 0L,
                "tools must be a non-empty unnamed list.")
  tool_names <- vapply(tools, function(tool) {
    .turn_require(.turn_fields(tool, c("type", "function"), c("type", "function")) &&
                    identical(tool$type, "function"),
                  "Each tool must be a function definition.")
    fn <- tool[["function"]]
    .turn_require(.turn_fields(fn, c("name", "description", "parameters", "strict"),
                               c("name", "parameters")) &&
                    .turn_string(fn$name) && grepl("^[A-Za-z0-9_-]{1,64}$", fn$name),
                  "Invalid function definition or name.")
    .turn_require(is.null(fn$description) || .turn_string(fn$description, empty = TRUE),
                  "Function description must be text.")
    .turn_require(is.null(fn$strict) || .turn_bool(fn$strict),
                  "Function strict must be a boolean.")
    .turn_require(.turn_object(fn$parameters) &&
                    identical(fn$parameters$type, "object") &&
                    .turn_object(fn$parameters$properties) &&
                    .turn_json_value(fn$parameters),
                  "Function parameters must be a JSON object schema with properties.")
    fn$name
  }, character(1))
  .turn_require(!anyDuplicated(tool_names), "Function names must be unique.")
  if (!is.null(tool_choice)) {
    choice_ok <- .turn_string(tool_choice) && tool_choice %in% c("none", "auto", "required")
    if (.turn_fields(tool_choice, c("type", "function"), c("type", "function"))) {
      choice_ok <- identical(tool_choice$type, "function") &&
        .turn_fields(tool_choice[["function"]], "name", "name") &&
        .turn_string(tool_choice[["function"]]$name) && tool_choice[["function"]]$name %in% tool_names
    }
    .turn_require(choice_ok, "tool_choice must select an available function or a supported mode.")
  }
  .turn_require(is.null(parallel_tool_calls) || .turn_bool(parallel_tool_calls),
                "parallel_tool_calls must be a boolean.")
}

.turn_usage <- function(raw) {
  if (is.null(raw)) return(NULL)
  if (!.turn_object(raw)) return(FALSE)
  details <- raw[["prompt_tokens_details"]]
  completion <- raw[["completion_tokens_details"]]
  if ((!is.null(details) && !.turn_object(details)) ||
      (!is.null(completion) && !.turn_object(completion))) return(FALSE)
  values <- list(input_tokens = raw[["prompt_tokens"]] %||% raw[["input_tokens"]],
                 output_tokens = raw[["completion_tokens"]] %||% raw[["output_tokens"]],
                 total_tokens = raw[["total_tokens"]],
                 cached_tokens = raw[["prompt_cache_hit_tokens"]] %||%
                   details[["cached_tokens"]] %||% raw[["cache_read_input_tokens"]],
                 cache_miss_tokens = raw[["prompt_cache_miss_tokens"]],
                 reasoning_tokens = completion[["reasoning_tokens"]])
  if (!all(vapply(values, function(x) is.null(x) ||
                  (.turn_number(x) && x >= 0 && x == floor(x) &&
                     x <= .Machine$integer.max), logical(1)))) return(FALSE)
  lapply(values, function(x) if (is.null(x)) NA_integer_ else as.integer(x))
}

.turn_raw_usage <- function(raw) {
  allowed <- c("prompt_tokens", "completion_tokens", "total_tokens", "input_tokens",
               "output_tokens", "prompt_cache_hit_tokens", "prompt_cache_miss_tokens",
               "cache_read_input_tokens", "cache_creation_input_tokens", "cached_tokens",
               "prompt_tokens_details", "completion_tokens_details", "input_tokens_details",
               "output_tokens_details", "reasoning_tokens", "audio_tokens", "text_tokens",
               "image_tokens", "video_tokens", "accepted_prediction_tokens",
               "rejected_prediction_tokens")
  clean <- function(x, depth = 0L) {
    if (.turn_number(x) && x >= 0 && x == floor(x)) return(x)
    if (!.turn_object(x) || depth > 3L) return(NULL)
    Filter(Negate(is.null), lapply(x[intersect(names(x), allowed)], clean, depth = depth + 1L))
  }
  if (.turn_object(raw)) clean(raw) else NULL
}

.turn_parse_response <- function(resp, model, secrets = character()) {
  status <- httr2::resp_status(resp)
  parsed <- tryCatch(httr2::resp_body_json(resp, simplifyVector = FALSE),
                     error = function(e) NULL)
  if (!.turn_object(parsed)) parsed <- NULL
  safe_text <- function(x) {
    if (.turn_string(x) && nchar(x, type = "bytes") <= 200L &&
        grepl("^[A-Za-z0-9_.:-]+$", x) &&
        !any(vapply(secrets[nzchar(secrets)], function(s) grepl(s, x, fixed = TRUE),
                    logical(1)))) x else NULL
  }
  metadata <- list(id = safe_text(parsed[["id"]]), model = safe_text(parsed[["model"]]),
                   created = if (.turn_number(parsed[["created"]])) parsed[["created"]] else NULL,
                   system_fingerprint = safe_text(parsed[["system_fingerprint"]]),
                   request_id = safe_text(httr2::resp_header(resp, "x-request-id")),
                   http_status = status)
  allowed_finish <- c("stop", "length", "tool_calls", "content_filter",
                      "function_call", "max_tokens", "max_output_tokens",
                      "eos_token", "end_turn", "stop_sequence")
  if (.turn_array(parsed[["choices"]]) && length(parsed[["choices"]]) == 1L &&
      .turn_object(parsed[["choices"]][[1L]])) {
    finish <- parsed[["choices"]][[1L]]$finish_reason
    if (.turn_string(finish) && finish %in% allowed_finish) metadata$finish_reason <- finish
  }
  usage <- .turn_usage(parsed[["usage"]])
  if (is.list(usage)) {
    token_record(provider = "openai_compat:structured", model = metadata$model %||% model,
                 input_tokens = usage$input_tokens, output_tokens = usage$output_tokens,
                 cached_tokens = usage$cached_tokens, call_type = "structured_turn")
  }
  fail <- function(code, message) {
    .turn_abort(code, message, "received", status,
                if (is.list(usage)) usage else NULL, metadata, .turn_raw_usage(parsed[["usage"]]))
  }
  if (status < 200L || status >= 300L) fail("http_error", "Structured-turn endpoint returned a non-success status.")
  if (is.null(parsed)) fail("invalid_response", "Structured-turn endpoint returned invalid JSON.")
  if (identical(usage, FALSE)) fail("invalid_usage", "Structured-turn response has invalid usage counts.")
  if (!.turn_array(parsed[["choices"]]) || length(parsed[["choices"]]) != 1L) {
    fail("invalid_response", "Structured-turn response must contain exactly one choice.")
  }
  choice <- parsed[["choices"]][[1L]]
  if (!.turn_object(choice) || !.turn_object(choice[["message"]])) {
    fail("invalid_response", "Structured-turn response has no assistant message.")
  }
  msg <- choice[["message"]]
  if (!identical(msg[["role"]], "assistant") ||
      !.turn_string(choice[["finish_reason"]]) || !choice[["finish_reason"]] %in% allowed_finish ||
      !(is.null(msg[["content"]]) || .turn_string(msg[["content"]], empty = TRUE))) {
    fail("invalid_response", "Structured-turn assistant message or finish reason is invalid.")
  }
  calls <- msg[["tool_calls"]]
  if (!is.null(calls) && !.turn_valid_calls(calls)) {
    fail("invalid_response", "Structured-turn response contains invalid tool calls.")
  }
  if (!length(calls) && !.turn_string(msg[["content"]])) {
    fail("invalid_response", "Structured-turn response contains neither text nor tool calls.")
  }
  message <- list(role = "assistant", content = msg[["content"]])
  if (length(calls)) message$tool_calls <- calls
  structure(list(schema_version = "agentomics.turn.v1", message = message,
                 finish_reason = choice[["finish_reason"]], usage = usage,
                 raw_usage = .turn_raw_usage(parsed[["usage"]]), metadata = metadata),
            class = c("agentomics_turn", "list"))
}
