# agentomicsCore

[![R-CMD-check](https://github.com/ChanghaoKan/agentomicsCore/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ChanghaoKan/agentomicsCore/actions/workflows/R-CMD-check.yaml)

> **Experimental infrastructure for human-in-the-loop, provenance-tracked bioinformatics workflows.**

`agentomicsCore` is the shared infrastructure used by domain-specific toolkits. It provides:

- **`AgentOmics`** — an S4 container that can wrap an arbitrary data payload and store decision records, script snippets, figures, parameters, and checkpoints alongside it.
- **`record_step()` / `find_in_decisions()`** — an extension API for downstream packages to record parameters, rationales, and timestamps.
- **Multi-provider chat factory** — `chat_claude()`, `chat_openai()`, `chat_deepseek()`, `chat_grok()`, `chat_qwen()`, `chat_kimi()`, and `make_chat_fn_openai_compatible()` (`make_chat_fn_openai_compat()` is retained for compatibility). Anthropic and OpenAI-compatible schemas are supported; image input depends on the selected provider/model.
- **Best-effort token tracking** — built-in chat factories record `{input, output, cached}` token fields when the provider returns usage metadata. Custom chat functions must call `token_record()` themselves.
- **`render_report()`** — single-file portable HTML report with a `extras` hook for domain-specific section renderers.
- **`save_checkpoint()` / `load_checkpoint()`** — qs2-backed persistence with a limited `upgrade_checkpoint()` migration helper.

This package does **not** ship any single-cell, ATAC, ChIP, or other domain-specific logic. Those live in dedicated packages built on top of it.

## Existing instantiations

| Package | Domain | Status |
|---|---|---|
| [scAgentKit](https://github.com/ChanghaoKan/scAgentKit) | scRNA-seq | v0.4.0 |
| atacAgentKit | ATAC-seq | planned |
| chipAgentKit | ChIP-seq | planned |
| spatialAgentKit | Spatial transcriptomics | planned |

## Installation

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ChanghaoKan/agentomicsCore@v0.1.1")
```

## Quick start

```r
library(agentomicsCore)

# Build a container around a concrete payload
counts <- matrix(c(1, 0, 3, 2), nrow = 2,
                 dimnames = list(c("G1", "G2"), c("C1", "C2")))
obj <- AgentOmics(counts, data_type = "matrix",
                  initial_script = "counts <- readRDS('counts.rds')")

# Use the LLM chat factory
chat <- chat_claude()    # uses ANTHROPIC_API_KEY env var
response <- chat(
  system_prompt = "You are a careful bioinformatician.",
  user_prompt   = "Summarise the strengths and weaknesses of MAD-based QC."
)

# Usage appears only when the provider returns a usage block
token_usage_summary()$total
```

## How downstream packages use it

A downstream package follows this pattern (see scAgentKit for a complete example):

```r
# In atacAgentKit/R/whatever.R
my_qc_function <- function(obj, threshold = 1000, rationale = NULL) {
  stopifnot(methods::is(obj, "AgentOmics"))
  # Perform domain work here, then record what was done.
  if (is.null(rationale)) rationale <- "Threshold selected by the analyst."
  agentomicsCore::record_step(
    obj            = obj,
    step_name      = "atac_qc",
    function_name  = "my_qc_function",
    params         = list(threshold = threshold),
    rationale      = rationale,
    script_snippet = '# Script skeleton for this step',
    new_stage      = "qc_done"
  )
}
```

## Why a separate package

The package is separate so that:

1. provenance, reporting, checkpoint, and provider code can be reused;
2. downstream packages can keep domain logic separate from orchestration; and
3. experimental domain packages can evolve without forking scAgentKit.

## Prototype status

This is an early research-software prototype. A recorded decision log is not proof that a decision is correct, and generated script snippets are not guaranteed to form a standalone replay for every downstream package. Inspect exported code, pin dependencies and model identifiers, retain the original inputs, and validate all biological conclusions independently.

## Citation

If you use this package in research, please cite both `agentomicsCore` and whichever downstream package you used:

```
Kan, Changhao. agentomicsCore: Infrastructure for human-in-the-loop, provenance-tracked bioinformatics workflows. Version 0.1.1 (2026). https://github.com/ChanghaoKan/agentomicsCore
```

## License

MIT.
