\name{turn_grok}
\alias{turn_grok}
\title{One native xAI Responses turn}
\usage{
turn_grok(model = "grok-4.7", api_key = NULL, api_key_env = "XAI_API_KEY",
  max_tokens = 2000L, timeout_secs = 180, temperature = 0.7, effort = "high")
}
\arguments{
\item{model}{Exact model ID.}
\item{api_key}{API key, or NULL to read the environment on dispatch only.}
\item{api_key_env}{Credential environment variable.}
\item{max_tokens}{Positive visible-output limit, not a total reasoning or monetary limit.}
\item{timeout_secs}{Positive per-request timeout.}
\item{temperature}{Sampling temperature between zero and two.}
\item{effort}{Reasoning effort: low, medium, high or xhigh.}
}
\description{Sends one native text/function-call request without retries,
redirects, server tools, stored-response references or automatic compaction.
This protocol adapter is not a live model qualification claim.}
\details{The returned function accepts \code{input}, OpenAI-shaped
\code{tools = NULL}, \code{max_tokens}, \code{timeout_secs} and
\code{prompt_cache_key = NULL}. Use \code{prepare_turn_request()} to inspect
the exact serialized body without reading a key. The caller owns local tool
execution, privacy, authorization, durable history and deadlines.

Responses use \code{agentomics.turn.v2}. Preserve the complete ordered
\code{protocol$output} array, including encrypted reasoning, unchanged and
append matching function-call outputs to the next native input. The assistant
message is a convenience projection and is insufficient for replay.
Opaque reasoning is protocol state, not scientific evidence.

Local validation fails with delivery \code{not_sent}; transport failures have
delivery \code{unknown}; HTTP responses have delivery \code{received}.
Unknown delivery is never retried automatically. Bounded native output on
invalid responses may be attached to the error's \code{protocol} field for
caller-controlled privacy validation and audit; it is not usable continuation.
Refusal and incomplete results retain their native output and a distinct
\code{finish_reason}. Missing usage remains NULL; missing token components
remain NA. Returned USD ticks are preserved only when exactly representable
as R numbers, and are not a pre-send estimate.}
\value{A structured turn function. Existing text and v1 turn interfaces are unchanged.}
\seealso{\code{\link{prepare_turn_request}}, \code{\link{turn_deepseek}}}
