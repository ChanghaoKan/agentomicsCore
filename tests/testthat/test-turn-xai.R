skip_if_not_installed("httr2")
skip_if_not_installed("jsonlite")
xai_input <- function() list(list(role = "system", content = "Use local evidence."),
  list(role = "user", content = "Inspect cluster 0."))
xai_tool <- function() list(type = "function", `function` = list(name = "cluster_markers",
  parameters = list(type = "object", properties = list(cluster_id = list(type = "string")),
    required = "cluster_id", additionalProperties = FALSE), strict = TRUE))
xai_output <- function(tool = FALSE) list(
  list(type = "reasoning", id = "r1", summary = list(), encrypted_content = "opaque-original"),
  if (tool) list(type = "function_call", id = "fc1", call_id = "c1", name = "cluster_markers",
    arguments = '{"cluster_id":"0"}', status = "completed") else
    list(type = "message", id = "m1", role = "assistant", status = "completed",
      content = list(list(type = "output_text", text = '{"draft":true}', annotations = list()))))
xai_payload <- function(tool = FALSE) list(id = "resp1", model = "grok-4.7",
  status = "completed", output = xai_output(tool),
  usage = list(input_tokens = 32L, output_tokens = 9L, total_tokens = 151L,
    input_tokens_details = list(cached_tokens = 12L),
    output_tokens_details = list(reasoning_tokens = 110L), cost_in_usd_ticks = 123456789))
xai_http <- function(payload = xai_payload(), status = 200L)
  httr2::response(status_code = status, headers = list(`content-type` = "application/json",
    `x-request-id` = "request1"),
    body = charToRaw(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null"))))
xai_error <- function(expr) tryCatch(force(expr), agentomics_turn_error = identity)

test_that("xAI preparation is key-free with no hidden history or server tools", {
  withr::local_envvar(XAI_OFFLINE_MISSING = NA_character_)
  fn <- turn_grok(api_key_env = "XAI_OFFLINE_MISSING")
  p <- prepare_turn_request(fn, xai_input(), list(xai_tool()))
  expect_identical(p$body$model, "grok-4.7")
  expect_false(p$body$store); expect_false(p$body$stream)
  expect_identical(p$body$reasoning$effort, "high")
  expect_equal(p$body$temperature, .7)
  expect_identical(p$body$include, list("reasoning.encrypted_content"))
  expect_false(p$body$parallel_tool_calls)
  expect_identical(p$body$tool_choice, "auto")
  expect_null(p$body$tools[[1]]$strict)
  expect_null(p$body$previous_response_id); expect_null(p$body$context_management)
  expect_null(p$body$fallbacks)
  expect_identical(xai_error(fn(xai_input()))$delivery, "not_sent")
  expect_error(prepare_turn_request(fn, xai_input(), list(list(type = "web_search"))))
})

test_that("HTTP bytes match preview and native output survives tool continuation", {
  fn <- turn_grok(api_key = "fake-key", timeout_secs = 2)
  initial <- prepare_turn_request(fn, xai_input(), list(xai_tool()))
  calls <- 0L
  httr2::local_mocked_responses(function(req) {
    calls <<- calls + 1L
    expect_identical(rawToChar(req$body$data), if (calls == 1L) initial$json else p$json)
    expect_identical(req$url, "https://api.x.ai/v1/responses")
    expect_false(req$options$followlocation)
    expect_equal(req$options$timeout_ms, 2000)
    body <- jsonlite::fromJSON(rawToChar(req$body$data), simplifyVector = FALSE)
    expect_identical(body$model, "grok-4.7")
    if (calls == 2L) expect_identical(body$input[3:4], xai_output(TRUE))
    xai_http(xai_payload(tool = calls == 1L))
  })
  first <- fn(xai_input(), list(xai_tool()))
  expect_identical(first$schema_version, "agentomics.turn.v2")
  expect_identical(first$finish_reason, "tool_calls")
  expect_identical(first$protocol$output, xai_output(TRUE))
  expect_equal(first$usage$reasoning_tokens, 110)
  expect_equal(first$usage$total_tokens, 151)
  expect_equal(first$usage$cached_tokens, 12)
  expect_equal(first$raw_usage$cost_in_usd_ticks, 123456789)
  history <- c(xai_input(), first$protocol$output,
    list(list(type = "function_call_output", call_id = "c1", output = '{"markers":[]}')))
  p <- prepare_turn_request(fn, history, list(xai_tool()))
  expect_identical(p$body$input, history)
  second <- fn(history, list(xai_tool()))
  expect_identical(second$finish_reason, "stop")
  expect_identical(second$message$content, '{"draft":true}')
  expect_equal(calls, 2L)
})

test_that("native history rejects missing duplicate and foreign tool results", {
  fn <- turn_grok(api_key = "fake")
  history <- c(xai_input(), xai_output(TRUE))
  expect_error(prepare_turn_request(fn, history), "Every function call")
  result <- list(type = "function_call_output", call_id = "c1", output = "{}")
  expect_error(prepare_turn_request(fn, c(history, list(result, result))), "unique pending")
  result$call_id <- "foreign"
  expect_error(prepare_turn_request(fn, c(history, list(result))), "unique pending")
  expect_error(prepare_turn_request(fn, c(xai_input(), list(list(type = "web_search_call")))),
    "Unsupported")
  expect_error(turn_grok(effort = "none"))
  bad <- xai_output(); bad[[2]]$content[[1]]$type <- "input_text"
  expect_error(prepare_turn_request(fn, c(xai_input(), bad)), "Only text")
})

test_that("unrecognized tool names and invalid argument JSON reach the local executor", {
  payload <- xai_payload(TRUE)
  payload$output[[2]]$name <- "not_registered"
  payload$output[[2]]$arguments <- "{"
  local_mocked_bindings(req_perform = function(...) xai_http(payload), .package = "httr2")
  out <- turn_grok(api_key = "fake")(xai_input(), list(xai_tool()))
  expect_identical(out$finish_reason, "tool_calls")
  expect_identical(out$message$tool_calls[[1]]$`function`$arguments, "{")
  expect_identical(out$protocol$output, payload$output)
})

test_that("refusal incomplete and unknown output keep distinct protocol outcomes", {
  for (mode in c("refusal", "incomplete", "unknown")) {
    payload <- xai_payload()
    if (mode == "refusal") payload$output[[2]]$content <-
      list(list(type = "refusal", refusal = "Cannot respond."))
    if (mode == "incomplete") {
      payload$status <- "incomplete"
      payload$incomplete_details <- list(reason = "max_output_tokens")
    }
    if (mode == "unknown") payload$output <- list(list(type = "new_item", value = "original"))
    local_mocked_bindings(req_perform = function(...) xai_http(payload), .package = "httr2")
    out <- xai_error(turn_grok(api_key = "fake")(xai_input()))
    if (mode == "unknown") {
      expect_identical(out$code, "invalid_response")
      expect_identical(out$delivery, "received")
    } else expect_identical(out$finish_reason, mode)
    expect_identical(out$protocol$output, payload$output)
    expect_equal(out$raw_usage$cost_in_usd_ticks, 123456789)
  }
})

test_that("HTTP failures and malformed usage preserve delivery without retry or key leakage", {
  calls <- 0L
  local_mocked_bindings(req_perform = function(...) {
    calls <<- calls + 1L
    stop("fake-secret in transport body")
  }, .package = "httr2")
  out <- xai_error(turn_grok(api_key = "fake-secret")(xai_input()))
  expect_identical(out$delivery, "unknown"); expect_equal(calls, 1L)
  expect_false(grepl("fake-secret", conditionMessage(out), fixed = TRUE))
  local_mocked_bindings(req_perform = function(...) xai_http(status = 429L), .package = "httr2")
  out <- xai_error(turn_grok(api_key = "fake")(xai_input()))
  expect_identical(out$delivery, "received"); expect_identical(out$code, "http_error")
  expect_equal(out$usage$input_tokens, 32L)
  payload <- xai_payload(); payload$usage <- "bad"
  local_mocked_bindings(req_perform = function(...) xai_http(payload), .package = "httr2")
  expect_identical(xai_error(turn_grok(api_key = "fake")(xai_input()))$code, "invalid_usage")
  payload$usage <- NULL
  out <- turn_grok(api_key = "fake")(xai_input())
  expect_null(out$usage); expect_null(out$raw_usage)
})
