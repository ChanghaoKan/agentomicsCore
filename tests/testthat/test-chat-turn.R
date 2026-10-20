skip_if_not_installed("httr2")
skip_if_not_installed("jsonlite")

turn_messages <- function() list(list(role = "system", content = "Use evidence."),
                                 list(role = "user", content = "Annotate cluster 0."))
turn_tool <- function() list(type = "function", "function" = list(
  name = "cluster_markers", description = "Read markers.",
  parameters = list(type = "object", properties = list(cluster_id = list(type = "string")),
                    required = "cluster_id", additionalProperties = FALSE)))
turn_call <- function(id = "call_1", name = "cluster_markers", arguments = '{"cluster_id":"0"}') {
  list(id = id, type = "function", "function" = list(name = name, arguments = arguments))
}
turn_payload <- function(content = "done", calls = NULL, finish = "stop",
                         usage = list(prompt_tokens = 21L, completion_tokens = 8L,
                                      prompt_cache_hit_tokens = 5L)) {
  msg <- list(role = "assistant", content = content)
  if (!is.null(calls)) msg$tool_calls <- calls
  list(id = "response-1", model = "returned-model", created = 1700000000,
       choices = list(list(index = 0L, message = msg, finish_reason = finish)), usage = usage)
}
turn_response <- function(payload = turn_payload(), status = 200L,
                          headers = list(`x-request-id` = "request-1")) {
  httr2::response(
    status_code = status, headers = c(list(`content-type` = "application/json"), headers),
    body = charToRaw(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")))
  )
}
turn_factory <- function(...) make_turn_fn_openai_compatible(
  base_url = "https://example.test/v1", model = "configured-model", api_key = "mock-secret", ...)
turn_error <- function(expr) tryCatch(force(expr), agentomics_turn_error = identity)

test_that("prepare is key-free and exactly matches serialized HTTP bytes", {
  withr::local_envvar(AGENTOMICS_TURN_TEST_KEY = NA_character_)
  fn <- make_turn_fn_openai_compatible("https://example.test/v1", "model",
                                       api_key_env = "AGENTOMICS_TURN_TEST_KEY")
  prepared <- prepare_turn_request(fn, turn_messages(), list(turn_tool()),
                                   tool_choice = "auto", parallel_tool_calls = FALSE)
  expect_named(prepared, c("body", "json"))
  expect_false(grepl("api_key|Authorization|example.test", prepared$json))
  expect_match(prepared$json, '"required":\\["cluster_id"\\]')
  expect_identical(turn_error(fn(turn_messages()))$delivery, "not_sent")

  fn <- turn_factory()
  prepared <- prepare_turn_request(fn, turn_messages(), list(turn_tool()),
                                   tool_choice = "auto", parallel_tool_calls = FALSE,
                                   max_tokens = 41L, timeout_secs = 2)
  calls <- 0L
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1L
    expect_identical(rawToChar(req$body$data), prepared$json)
    expect_identical(req$url, "https://example.test/v1/chat/completions")
    expect_identical(req$options$followlocation, FALSE)
    expect_equal(req$options$timeout_ms, 2000)
    turn_response()
  })
  out <- fn(turn_messages(), list(turn_tool()), tool_choice = "auto",
            parallel_tool_calls = FALSE, max_tokens = 41L, timeout_secs = 2)
  expect_s3_class(out, "agentomics_turn")
  expect_identical(out$message$content, "done")
  expect_equal(calls, 1L)
})

test_that("schema canonicalization preserves keyword-named properties and literal enum values", {
  tool <- turn_tool()
  tool[["function"]]$parameters <- list(
    type = "object", properties = list(
      required = list(type = "string"),
      enum = list(type = "object", properties = list(), enum = list(list(required = "literal"))),
      empty = list(type = "array", items = list())),
    required = c("required", "enum"))
  prepared <- prepare_turn_request(turn_factory(), turn_messages(), list(tool))
  decoded <- jsonlite::fromJSON(prepared$json, simplifyVector = FALSE)
  schema <- decoded$tools[[1]][["function"]]$parameters
  expect_identical(schema$properties$required$type, "string")
  expect_identical(schema$properties$enum$enum[[1]]$required, "literal")
  expect_identical(names(schema$properties$enum$properties), character())
  expect_identical(names(schema$properties$empty$items), character())
  expect_identical(schema$required, list("required", "enum"))
})

test_that("DeepSeek preset disables thinking and fixes temperature zero", {
  fn <- turn_deepseek(api_key = "mock-secret")
  prepared <- prepare_turn_request(fn, turn_messages(), list(turn_tool()))
  expect_identical(prepared$body$thinking, list(type = "disabled"))
  expect_identical(prepared$body$temperature, 0)
  expect_identical(prepared$body$model, "deepseek-flash")
  expect_false("temperature" %in% names(formals(fn)))
  httr2::local_mocked_responses(function(req) {
    expect_identical(req$url, "https://api.deepseek.com/v1/chat/completions")
    turn_response()
  })
  expect_identical(fn(turn_messages())$message$content, "done")
})

test_that("tool-only responses retain usage and can continue a complete conversation", {
  token_usage_reset()
  on.exit(token_usage_reset(), add = TRUE)
  seen <- list()
  httr2::local_mocked_responses(function(req) {
    seen[[length(seen) + 1L]] <<- jsonlite::fromJSON(rawToChar(req$body$data), simplifyVector = FALSE)
    if (length(seen) == 1L) turn_response(turn_payload(NULL, list(turn_call()), "tool_calls"))
    else turn_response()
  })
  fn <- turn_factory()
  first <- fn(turn_messages(), list(turn_tool()))
  expect_null(first$message$content)
  expect_identical(first$message$tool_calls[[1]][["function"]]$arguments, '{"cluster_id":"0"}')
  expect_equal(first$usage$input_tokens, 21L)
  expect_equal(first$usage$cached_tokens, 5L)
  expect_equal(first$raw_usage$prompt_cache_hit_tokens, 5L)
  history <- c(turn_messages(), list(first$message,
    list(role = "tool", tool_call_id = "call_1", content = '{"markers":[]}')))
  second <- fn(history, list(turn_tool()))
  expect_identical(seen[[2]]$messages, history)
  expect_identical(second$message$content, "done")
  expect_equal(token_usage_summary()$total$n_calls, 2L)
})

test_that("multiple tool calls require one distinct result each", {
  fn <- turn_factory()
  assistant <- list(role = "assistant", content = NULL,
                    tool_calls = list(turn_call("a"), turn_call("b")))
  history <- c(turn_messages(), list(assistant,
    list(role = "tool", tool_call_id = "b", content = "{}"),
    list(role = "tool", tool_call_id = "a", content = "{}")))
  expect_type(prepare_turn_request(fn, history)$json, "character")
  incomplete <- history[-length(history)]
  expect_identical(turn_error(prepare_turn_request(fn, incomplete))$code, "invalid_request")
  duplicate <- history
  duplicate[[5]]$tool_call_id <- "b"
  expect_error(prepare_turn_request(fn, duplicate), class = "agentomics_turn_error")
  orphan <- c(turn_messages(), list(list(role = "tool", tool_call_id = "a", content = "{}")))
  expect_error(prepare_turn_request(fn, orphan), class = "agentomics_turn_error")
  repeated <- c(history, list(assistant,
    list(role = "tool", tool_call_id = "a", content = "{}"),
    list(role = "tool", tool_call_id = "b", content = "{}")))
  expect_error(prepare_turn_request(fn, repeated), class = "agentomics_turn_error")
})

test_that("unknown tools and malformed argument JSON remain executor-owned", {
  httr2::local_mocked_responses(list(turn_response(turn_payload(
    NULL, list(turn_call(name = "not_registered", arguments = "{bad")), "tool_calls"))))
  result <- turn_factory()(turn_messages(), list(turn_tool()))
  expect_identical(result$message$tool_calls[[1]][["function"]]$name, "not_registered")
  expect_identical(result$message$tool_calls[[1]][["function"]]$arguments, "{bad")
  history <- c(turn_messages(), list(result$message,
    list(role = "tool", tool_call_id = "call_1", content = '{"error":"unknown_tool"}')))
  expect_type(prepare_turn_request(turn_factory(), history)$json, "character")
})

test_that("invalid local protocol is rejected before any HTTP request", {
  requests <- 0L
  httr2::local_mocked_responses(function(req) {
    requests <<- requests + 1L
    stop("HTTP must not be called")
  })
  fn <- turn_factory()
  bad_messages <- list(
    list(), list(list(role = "developer", content = "x")),
    list(list(role = "user", content = "x", path = "/private/file")),
    list(list(role = "user", content = NA_character_)),
    list(list(role = "assistant", content = NULL)),
    list(list(role = "user", content = list(type = "image_url", image_url = "private"))),
    list(list(role = "assistant", content = NULL,
              tool_calls = list(turn_call(), turn_call())))
  )
  for (messages in bad_messages) {
    err <- turn_error(fn(messages))
    expect_s3_class(err, "agentomics_turn_error")
    expect_identical(err$delivery, "not_sent")
  }
  expect_error(fn(turn_messages(), list(turn_tool(), turn_tool())), class = "agentomics_turn_error")
  expect_error(fn(turn_messages(), tool_choice = "auto"), class = "agentomics_turn_error")
  expect_error(fn(turn_messages(), list(turn_tool()), parallel_tool_calls = 1), class = "agentomics_turn_error")
  expect_error(fn(turn_messages(), list(turn_tool()), tool_choice = "invalid"), class = "agentomics_turn_error")
  expect_error(fn(turn_messages(), max_tokens = 0), class = "agentomics_turn_error")
  expect_error(fn(turn_messages(), max_tokens = 0.5), class = "agentomics_turn_error")
  expect_error(fn(turn_messages(), timeout_secs = Inf), class = "agentomics_turn_error")
  expect_equal(requests, 0L)
})

test_that("forced function selection is validated against registered tools", {
  selected <- list(type = "function", "function" = list(name = "cluster_markers"))
  expect_identical(prepare_turn_request(turn_factory(), turn_messages(), list(turn_tool()),
                                        tool_choice = selected)$body$tool_choice, selected)
  selected[["function"]]$name <- "missing"
  expect_error(prepare_turn_request(turn_factory(), turn_messages(), list(turn_tool()),
                                    tool_choice = selected), class = "agentomics_turn_error")
})

test_that("protocol fields and authentication cannot be overridden", {
  for (field in c("messages", "tools", "temperature", "max_tokens", "n", "stream")) {
    expect_error(turn_factory(extra_body = stats::setNames(list("bad"), field)),
                 class = "agentomics_turn_error")
  }
  expect_error(turn_factory(extra_headers = c(Authorization = "secret")),
               class = "agentomics_turn_error")
  expect_error(turn_factory(extra_headers = c(Foo = "a", foo = "b")),
               class = "agentomics_turn_error")
  expect_error(make_turn_fn_openai_compatible("https://user:secret@example.test", "model"),
               class = "agentomics_turn_error")
  expect_error(make_turn_fn_openai_compatible("https://example.test?secret=key", "model"),
               class = "agentomics_turn_error")
})

test_that("missing usage remains missing and raw usage is numeric-only", {
  httr2::local_mocked_responses(list(
    turn_response(turn_payload(usage = NULL)),
    turn_response(turn_payload(usage = list(
      input_tokens = 3L, output_tokens = 2L,
      prompt_tokens_details = list(cached_tokens = 1L, secret = "private"),
      request_body = "private")))))
  fn <- turn_factory()
  expect_null(fn(turn_messages())$usage)
  out <- fn(turn_messages())
  expect_equal(out$usage$input_tokens, 3L)
  expect_equal(out$usage$output_tokens, 2L)
  expect_true(is.na(out$usage$total_tokens))
  expect_equal(out$raw_usage$prompt_tokens_details$cached_tokens, 1L)
  expect_false(grepl("private|request_body|secret", jsonlite::toJSON(out)))
})

test_that("HTTP and transport errors never retry or expose provider diagnostics", {
  count <- 0L
  httr2::local_mocked_responses(function(req) {
    count <<- count + 1L
    turn_response(list(error = list(message = "mock-secret /private/path")), status = 429L)
  })
  err <- turn_error(turn_factory()(turn_messages()))
  expect_identical(err$code, "http_error")
  expect_identical(err$delivery, "received")
  expect_equal(err$http_status, 429L)
  expect_false(grepl("mock-secret|private", paste(capture.output(str(err)), collapse = "")))
  expect_equal(count, 1L)
})

test_that("transport failure remains delivery-unknown with one attempt", {
  count <- 0L
  httr2::local_mocked_responses(function(req) {
    count <<- count + 1L
    stop("Timeout at /private/path; Authorization mock-secret")
  })
  err <- turn_error(turn_factory()(turn_messages()))
  expect_identical(err$code, "transport_error")
  expect_identical(err$delivery, "unknown")
  expect_null(err$usage)
  expect_false(grepl("mock-secret|private", conditionMessage(err)))
  expect_equal(count, 1L)
})

test_that("redirects fail without following a second endpoint", {
  count <- 0L
  httr2::local_mocked_responses(function(req) {
    count <<- count + 1L
    expect_false(req$options$followlocation)
    turn_response(status = 307L, headers = list(location = "https://other.test"))
  })
  err <- turn_error(turn_factory()(turn_messages()))
  expect_identical(err$code, "http_error")
  expect_equal(count, 1L)
})

test_that("invalid output still records valid returned usage", {
  token_usage_reset()
  on.exit(token_usage_reset(), add = TRUE)
  payload <- turn_payload(content = NULL, finish = "length")
  httr2::local_mocked_responses(list(turn_response(payload)))
  err <- turn_error(turn_factory()(turn_messages()))
  expect_identical(err$code, "invalid_response")
  expect_equal(err$usage$output_tokens, 8L)
  expect_equal(err$raw_usage$completion_tokens, 8L)
  expect_identical(err$response_metadata$finish_reason, "length")
  expect_equal(token_usage_summary()$total$n_calls, 1L)
})

test_that("malformed response envelope, duplicate calls and invalid usage are rejected", {
  cases <- list(
    list(choices = list()),
    list(choices = list(turn_payload()$choices[[1]], turn_payload()$choices[[1]])),
    turn_payload(NULL, list(turn_call(), turn_call()), "tool_calls"),
    turn_payload(usage = list(prompt_tokens = -1)),
    turn_payload(content = list(type = "image_url", url = "private"))
  )
  httr2::local_mocked_responses(lapply(cases, turn_response))
  fn <- turn_factory()
  for (i in seq_along(cases)) expect_s3_class(turn_error(fn(turn_messages())), "agentomics_turn_error")
})

test_that("non-JSON HTTP responses are discarded with received delivery status", {
  httr2::local_mocked_responses(list(httr2::response(
    headers = list(`content-type` = "application/json"),
    body = charToRaw("mock-secret /private/path <html>"))))
  err <- turn_error(turn_factory()(turn_messages()))
  expect_identical(err$code, "invalid_response")
  expect_identical(err$delivery, "received")
  expect_false(grepl("mock-secret|private", paste(capture.output(str(err)), collapse = "")))
})

test_that("metadata excludes arbitrary body fields, headers, paths and the configured key", {
  payload <- turn_payload()
  payload$id <- "mock-secret"
  payload$model <- "/private/provider/file"
  payload$system_fingerprint <- "mock-secret"
  payload$secret <- "private"
  httr2::local_mocked_responses(list(turn_response(payload, headers = list(
    `x-request-id` = "mock-secret", authorization = "private", `set-cookie` = "private"))))
  out <- turn_factory()(turn_messages())
  expect_null(out$metadata$id)
  expect_null(out$metadata$model)
  expect_null(out$metadata$system_fingerprint)
  expect_null(out$metadata$request_id)
  expect_false(grepl("private|mock-secret|authorization|set-cookie", jsonlite::toJSON(out)))
})

test_that("legacy factories keep text returns and offer opt-in structured metadata", {
  token_usage_reset()
  on.exit(token_usage_reset(), add = TRUE)
  httr2::local_mocked_responses(rep(list(turn_response()), 3L))
  text <- make_chat_fn_openai_compatible("https://example.test", "test", api_key = "mock-secret")
  expect_identical(text("system", "user"), "done")
  structured <- chat_deepseek(api_key_env = "DEEPSEEK_API_KEY", api_key = "mock-secret", structured_response = TRUE)
  out <- structured("system", "user")
  expect_identical(out$content, "done")
  expect_identical(out$model, "returned-model")
  expect_equal(out$usage$input_tokens, 21L)
  expect_equal(out$raw_usage$prompt_cache_hit_tokens, 5L)
  expect_identical(out$response_metadata$id, "response-1")
  expect_identical(out$response_metadata$finish_reason, "stop")
  expect_equal(out$response_metadata$http_status, 200L)
  old <- make_chat_fn_openai_compat("test", "mock-secret", "https://example.test")
  expect_identical(old("system", "user"), "done")
  expect_equal(token_usage_summary()$total$n_calls, 3L)
})

test_that("legacy structured errors expose safe metadata for downstream ledgers", {
  httr2::local_mocked_responses(list(turn_response(
    list(error = list(message = "mock-secret /private/path")), status = 500L)))
  fn <- chat_deepseek(api_key_env = "DEEPSEEK_API_KEY", api_key = "mock-secret", structured_response = TRUE)
  err <- turn_error(fn("system", "user"))
  expect_identical(err$delivery, "received")
  expect_equal(err$response_metadata$http_status, 500L)
  expect_identical(err$response_metadata$request_id, "request-1")
  expect_false(grepl("mock-secret|private", conditionMessage(err)))
})
