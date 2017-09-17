add_cols <- function(x, y, where = 1) {
  where <- check_where(where, ncol(x))

  if (where == 1) {
    cbind(y, x)
  } else if (where > ncol(x)) {
    cbind(x, y)
  } else {
    lhs <- 1:(where - 1)
    cbind(x[lhs], y, x[-lhs])
  }
}

check_where <- function(x, ncol) {
  if (length(x) != 1 || !is.numeric(x)) {
    stop("`where` must be a length one numeric vector.", call. = FALSE)
  }
  x <- as.integer(x)

  if (x == 0 || is.na(x)) {
    stop("`where` must not be zero or missing", call. = FALSE)
  } else if (x < 0) {
    (ncol + 1) + (x + 1)
  } else {
    x
  }
}
