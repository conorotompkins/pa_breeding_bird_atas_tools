mode <- function(x, na.rm = FALSE) {
  # remember original class
  cls <- class(x)

  if (na.rm) {
    x <- x[!is.na(x)]
  }

  # empty or all NA → NA of same class if possible
  if (length(x) == 0L || all(is.na(x))) {
    out <- NA
    class(out) <- cls
    return(out)
  }

  # work on a copy without NA
  x_no_na <- x[!is.na(x)]
  tab <- table(x_no_na)

  # index of first max frequency
  idx <- which.max(tab)

  # position(s) of that value in x_no_na
  # (tab is in the order of unique(x_no_na))
  val <- unique(x_no_na)[idx]

  # `val` comes directly from x_no_na, so it keeps its original type/class
  val
}
