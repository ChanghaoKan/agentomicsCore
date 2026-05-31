test_that("AgentOmics constructor works on a list payload", {
  obj <- AgentOmics(list(s1 = "stub", s2 = "stub"),
                    data_type = "seurat_list",
                    initial_script = "# initial load")
  expect_s4_class(obj, "AgentOmics")
  expect_equal(obj@data_type, "seurat_list")
  expect_equal(obj@stage, "initialized")
  expect_equal(length(obj@decisions), 0)
  expect_equal(obj@scripts, "# initial load")
  expect_true(inherits(obj@created_at, "POSIXct"))
})

test_that("record_step accumulates decisions, scripts, and params", {
  obj <- AgentOmics(list(s1 = "stub"), data_type = "seurat_list")
  obj <- record_step(
    obj,
    step_name      = "qc_threshold",
    function_name  = "qc_threshold",
    params         = list(min_nCount = 1000, min_nFeature = 500),
    rationale      = "Initial hard filter.",
    script_snippet = "# qc_threshold(min_nCount = 1000, ...)",
    new_stage      = "qc_filtered"
  )
  expect_equal(length(obj@decisions), 1)
  expect_equal(obj@decisions[[1]]$step, "qc_threshold")
  expect_equal(obj@stage, "qc_filtered")
  expect_equal(obj@params$min_nCount, 1000)
  expect_true(any(grepl("qc_threshold", obj@scripts)))
})

test_that("record_step merges params instead of overwriting", {
  obj <- AgentOmics(list(s1 = "stub"), data_type = "seurat_list")
  obj <- record_step(obj, "step1", "f1", list(ndim = 30, foo = "a"),
                     "", "", new_stage = NULL)
  obj <- record_step(obj, "step2", "f2", list(bar = 99),
                     "", "", new_stage = NULL)
  expect_equal(obj@params$ndim, 30)
  expect_equal(obj@params$foo, "a")
  expect_equal(obj@params$bar, 99)
})

test_that("find_in_decisions retrieves most recent value", {
  obj <- AgentOmics(list(s1 = "stub"), data_type = "seurat_list")
  obj <- record_step(obj, "sc_pca", "sc_pca", list(ndim = 25), "", "")
  obj <- record_step(obj, "sc_select_pcs", "sc_select_pcs",
                     list(ndim = 30), "", "")
  expect_equal(find_in_decisions(obj, "ndim"), 30)
  expect_equal(find_in_decisions(obj, "ndim", from_step = "sc_pca"), 25)
  expect_null(find_in_decisions(obj, "no_such_param"))
})

test_that("accessors get_decisions / get_script / get_figures / get_data work", {
  obj <- AgentOmics(list(s1 = "stub"), data_type = "seurat_list",
                    initial_script = "# load")
  obj <- record_step(obj, "step1", "f1", list(x = 1),
                     "Step 1 rationale", "x <- 1", new_stage = NULL)
  expect_length(get_decisions(obj), 1)
  expect_match(get_script(obj), "x <- 1")
  expect_match(get_script(obj), "# load")
  expect_s3_class(get_figures(obj), "data.frame")
  expect_equal(get_data(obj), list(s1 = "stub"))
})

test_that("record_figure registers a figure row", {
  obj <- AgentOmics(list(s1 = "stub"), data_type = "seurat_list")
  obj <- record_figure(obj, "qc", "/tmp/qc.png", "QC violin")
  expect_equal(nrow(get_figures(obj)), 1)
  expect_equal(get_figures(obj)$path, "/tmp/qc.png")
})

test_that("token tracking accumulates and resets", {
  token_usage_reset()
  token_record("anthropic", "claude-x", 100, 50)
  token_record("anthropic", "claude-x", 200, 80)
  s <- token_usage_summary()
  expect_equal(s$total$n_calls, 2)
  expect_equal(s$total$input_tokens, 300)
  token_usage_reset()
  expect_equal(token_usage_summary()$total$n_calls, 0)
})

test_that("attach_step_tokens stores a per-step summary", {
  token_usage_reset()
  obj <- AgentOmics(list(s1 = "stub"), data_type = "seurat_list")
  t0 <- length(token_state$records)
  token_record("anthropic", "claude-x", 10, 5)
  obj <- attach_step_tokens(obj, "annotate", t0)
  expect_false(is.null(obj@token_usage$annotate))
  expect_equal(obj@token_usage$annotate$n_calls, 1)
})

test_that("esc_html escapes HTML metacharacters", {
  expect_equal(esc_html('<a>&"'), "&lt;a&gt;&amp;&quot;")
})

test_that("upgrade_checkpoint stamps version on a bare object", {
  obj <- methods::new("AgentOmics", data = list(),
                      data_type = "seurat_list", stage = "initialized",
                      created_at = Sys.time(), updated_at = Sys.time())
  obj <- upgrade_checkpoint(obj)
  expect_true(is.character(obj@version))
})
