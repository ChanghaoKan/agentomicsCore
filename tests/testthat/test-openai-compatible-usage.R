test_that("preferred OpenAI-compatible path records returned usage", {
  token_usage_reset()
  on.exit(token_usage_reset(), add = TRUE)

  parsed <- list(
    model = "provider-model-id",
    usage = list(
      prompt_tokens = 21L,
      completion_tokens = 8L,
      prompt_tokens_details = list(cached_tokens = 5L)
    )
  )

  agentomicsCore:::.record_openai_compatible_usage(
    parsed = parsed,
    base_url = "https://api.openai.com/v1",
    model = "configured-model"
  )

  usage <- token_usage_summary()
  expect_equal(usage$total$n_calls, 1L)
  expect_equal(usage$total$input_tokens, 21L)
  expect_equal(usage$total$output_tokens, 8L)
  expect_equal(usage$total$cached_tokens, 5L)
  expect_equal(usage$by_call$provider, "openai_compat:openai")
  expect_equal(usage$by_call$model, "provider-model-id")
  expect_equal(usage$by_call$call_type, "text")
})

test_that("missing usage metadata does not create a token record", {
  token_usage_reset()
  on.exit(token_usage_reset(), add = TRUE)

  agentomicsCore:::.record_openai_compatible_usage(
    parsed = list(model = "provider-model-id"),
    base_url = "http://localhost:11434/v1",
    model = "configured-model"
  )

  expect_equal(token_usage_summary()$total$n_calls, 0L)
})

test_that("alternative input/output token field names are normalised", {
  token_usage_reset()
  on.exit(token_usage_reset(), add = TRUE)

  agentomicsCore:::.record_openai_compatible_usage(
    parsed = list(
      usage = list(input_tokens = 13L, output_tokens = 4L)
    ),
    base_url = "https://example.test/v1",
    model = "configured-model",
    image_path = "plot.png"
  )

  usage <- token_usage_summary()$by_call
  expect_equal(usage$input_tokens, 13L)
  expect_equal(usage$output_tokens, 4L)
  expect_equal(usage$call_type, "vision")
})

test_that("an image path ignored by a text-only model is counted as text", {
  token_usage_reset()
  on.exit(token_usage_reset(), add = TRUE)

  agentomicsCore:::.record_openai_compatible_usage(
    parsed = list(
      usage = list(prompt_tokens = 7L, completion_tokens = 2L)
    ),
    base_url = "https://example.test/v1",
    model = "text-only-model",
    image_path = "ignored-plot.png",
    image_sent = FALSE
  )

  usage <- token_usage_summary()$by_call
  expect_equal(usage$call_type, "text")
})
