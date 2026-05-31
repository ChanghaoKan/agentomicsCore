# agentomicsCore 0.1.0 (initial release, 2026)

Extracted as a standalone package from `scAgentKit` v0.2.0. Contains the
omic-agnostic infrastructure that was previously bundled inside
scAgentKit, intended as the foundation for multiple domain packages
(scAgentKit, atacAgentKit, chipAgentKit, spatialAgentKit, ...).

## Features

* S4 container `AgentOmics` (formerly `scAgentKit::AgentSeurat`) with a
  generalised `data` slot accepting any BioC payload.
* Decision-log + script-export infrastructure via `record_step()` and
  `find_in_decisions()` (formerly the internal `.record_step` / etc.).
* Per-call LLM token tracking via `token_record()`,
  `token_usage_summary()`, `attach_step_tokens()`, `with_token_scope()`.
* Multi-provider chat factory: Anthropic, OpenAI, DeepSeek, Grok, Qwen,
  Kimi, generic OpenAI-compatible.
* Checkpoint I/O (`save_checkpoint`, `load_checkpoint`) and
  `upgrade_checkpoint()` for cross-version compatibility.
* HTML report renderer `render_report()` with an `extras` hook so
  downstream packages can register domain-specific section renderers.
* Exported `esc_html()` helper for downstream renderers.

## Migrating from scAgentKit pre-v0.3.0

Existing scAgentKit users do not need to do anything. scAgentKit v0.3.0
depends on agentomicsCore but provides full backward compatibility:
`AgentSeurat` remains a class (now an S4 subclass of `AgentOmics`), and
all previously-internal helpers (`.record_step`, etc.) remain available
via an internal alias layer.

For new downstream packages, use the `agentomicsCore::` prefix
explicitly:

```r
agentomicsCore::AgentOmics(my_data, data_type = "granges_list")
agentomicsCore::record_step(obj, step_name = "...", ...)
agentomicsCore::render_report(obj, file = "report.html", extras = list(...))
```
