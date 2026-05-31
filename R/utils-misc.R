#' Null-coalescing (default value) operator
#'
#' Returns `a` if it is non-`NULL` and non-empty (`length(a) > 0`),
#' otherwise `b`. Exported so dependent packages can use it without
#' re-declaring.
#'
#' @param a Value to test.
#' @param b Fallback value if `a` is `NULL` or empty.
#' @return `a` or `b`.
#' @name op-null-default
#' @rdname op-null-default
#' @export
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}
