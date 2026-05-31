# agentomicsCore

> **A generic R framework for LLM-orchestrated, fully audited bioinformatics pipelines.**

`agentomicsCore` is the substrate that domain-specific agent toolkits build on. It provides:

- **`AgentOmics`** — an S4 container that wraps an arbitrary BioC payload (Seurat / SingleCellExperiment / GRanges / RangedSummarizedExperiment / any list-of-these) and accumulates a decision log + reproducible R script as the pipeline runs.
- **`record_step()` / `find_in_decisions()`** — the extension API for downstream packages to log every decision with parameters, rationale, and timestamp.
- **Multi-provider chat factory** — `chat_claude()`, `chat_openai()`, `chat_deepseek()`, `chat_grok()`, `chat_qwen()`, `chat_kimi()`, `make_chat_fn_openai_compat()`. Anthropic and OpenAI-compatible schemas both supported. Vision-capable where the provider allows.
- **Per-call token tracking** — every chat_fn call records `{input, output, cached}` tokens to a package-level accumulator. `token_usage_summary()` for global totals, `attach_step_tokens()` to attribute per-pipeline-step.
- **`render_report()`** — single-file portable HTML report with a `extras` hook for domain-specific section renderers.
- **`save_checkpoint()` / `load_checkpoint()`** — qs2-backed persistence with `upgrade_checkpoint()` for forward compatibility.

This package does **not** ship any single-cell, ATAC, ChIP, or other domain-specific logic. Those live in dedicated packages built on top of it.

## Existing instantiations

| Package | Domain | Status |
|---|---|---|
| [scAgentKit](https://github.com/ChanghaoKan/scAgentKit) | scRNA-seq | v0.3.0 |
| atacAgentKit | ATAC-seq | planned |
| chipAgentKit | ChIP-seq | planned |
| spatialAgentKit | Spatial transcriptomics | planned |

## Installation

```r
remotes::install_github("ChanghaoKan/agentomicsCore")
```

## Quick start

```r
library(agentomicsCore)

# Build a container around any BioC object
obj <- AgentOmics(my_seurat_or_sce_or_granges, data_type = "seurat",
                  initial_script = '# loaded my data')

# Use the LLM chat factory
chat <- chat_claude()    # uses ANTHROPIC_API_KEY env var
response <- chat(
  system = "You are a careful bioinformatician.",
  user   = "Summarise the strengths and weaknesses of MAD-based QC."
)

# Every chat_fn call is metered
token_usage_summary()$total
# $ n_calls : 1
# $ input_tokens : 47
# $ output_tokens : 312
# ...
```

## How downstream packages use it

A downstream package follows this pattern (see scAgentKit for a complete example):

```r
# In atacAgentKit/R/whatever.R
my_qc_function <- function(obj, ..., rationale = NULL) {
  stopifnot(methods::is(obj, "AgentOmics"))
  ...do work...
  agentomicsCore::record_step(
    obj            = obj,
    step_name      = "atac_qc",
    function_name  = "my_qc_function",
    params         = list(threshold = threshold),
    rationale      = rationale %||% "default rationale",
    script_snippet = '# the R code to reproduce this step',
    new_stage      = "qc_done"
  )
}
```

## Why a separate package

Three reasons:

1. **Code reuse.** scAgentKit's S4 container + decision log + chat factory + report rendering + token tracking are not single-cell-specific. Future ATAC / ChIP / spatial packages would otherwise duplicate ~2000 lines of identical infrastructure.
2. **A stable target for the methods paper.** "An LLM-orchestrated bioinformatics framework with single-cell instantiation" is a stronger contribution than "yet another LLM annotation tool".
3. **Allowing external contributions in unowned domains.** Anyone who wants to add proteomics, microbiome, or any other omic instance can do so without forking scAgentKit.

## Citation

If you use this package in research, please cite both `agentomicsCore` and whichever downstream package you used:

```
Kan, Changhao. agentomicsCore: A framework for LLM-orchestrated audited bioinformatics pipelines. (2026). https://github.com/ChanghaoKan/agentomicsCore
```

## License

MIT.
