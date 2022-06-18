# Create a calculator environment to safely evaluate arithmetic expressions
# Modified from <https://stackoverflow.com/a/18391779/4568207>

safe_f <- c(getGroupMembers("Math"), getGroupMembers("Arith"), "(")
safe_env <- new.env(parent = emptyenv())
for (f in safe_f) safe_env[[f]] <- get(f, "package:base")

#' Evaluate evaluate arithmetic expressions (in character string format) safely
#'
#' @param x Must be a character string
#'
#' @examples
#' # Can't access variables outside of that environment
#' a <- 1
#' safe_eval_str("a")
#'
#' # Can't create variable in that environment
#' safe_eval_str("a <- 2")
#'
#' # Can't access dangerous functions
#' safe_eval_str('cat("Hi!")')
#'
#' # Can't return non-numeric values
#' safe_eval_str('"a"')
safe_eval_str <- function(x) {
  if (!is.character(x)) stop("`x` must be a character.")
  res <- eval(parse(text = x), env = safe_env)
  if (!is.numeric(res)) stop("The result must be a numeric.")
  res
}

df2graph <- function(namesH, df) {
  # Check to make sure the column names are present
  if (!any(names(df) %in% c("From", "To", "Weight"))) {
    print("Must have three columns in the input data frame: 'From', 'To', and 'Weight'")
  }
  dim <- length(namesH)
  m <- matrix(rep(0, dim^2), nrow = dim)

  for (i in 1:nrow(df)) {
    row.index <- which(namesH %in% df$From[i])
    col.index <- which(namesH %in% df$To[i])
    w_numeric <- try(safe_eval_str(as.character(df$Weight[i])), silent = TRUE)
    m[row.index, col.index] <- if (inherits(w_numeric, "try-error")) NA else w_numeric
  }
  m
}
