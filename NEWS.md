# agentomicsCore 0.1.1 (2026-07-20)

* Record provider-returned usage metadata for the preferred
  `make_chat_fn_openai_compatible()` path, including the DeepSeek, Grok,
  Qwen, Kimi, and OpenAI presets.
  Calls that receive but do not transmit an image path are recorded as text.
* Add regression tests for OpenAI-compatible usage parsing and restore the
  package-level test runner.
* Add a minimal `R CMD check` workflow and clarify which exported scripts
  are executable traces versus reviewable skeletons.
* Tighten dependency, local-file, and API-key documentation for a cleaner
  source release.

---

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
* Best-effort LLM token tracking via `token_record()`,
  `token_usage_summary()`, `attach_step_tokens()`, and
  `with_token_scope()`. Built-in factories record usage only when the
  provider includes usage metadata in its response.
* Multi-provider chat factory: Anthropic, OpenAI, DeepSeek, Grok, Qwen,
  Kimi, generic OpenAI-compatible.
* Checkpoint I/O (`save_checkpoint`, `load_checkpoint`) and
  `upgrade_checkpoint()` for cross-version compatibility.
* HTML report renderer `render_report()` with an `extras` hook so
  downstream packages can register domain-specific section renderers.
* Exported `esc_html()` helper for downstream renderers.

## Migrating from scAgentKit pre-v0.3.0

scAgentKit v0.3.0 depends on agentomicsCore and retains compatibility
shims for the main public interfaces:
`AgentSeurat` remains a class (now an S4 subclass of `AgentOmics`), and
the previously-internal helpers used by scAgentKit (`.record_step`, etc.)
remain available via an internal alias layer. Older checkpoints should
still be backed up and tested before replacing the originals.

For new downstream packages, use the `agentomicsCore::` prefix
explicitly:

```r
agentomicsCore::AgentOmics(granges_list, data_type = "granges_list")
agentomicsCore::record_step(obj, step_name = "...", ...)
agentomicsCore::render_report(obj, path = "report.html", extras = list(...))
```
