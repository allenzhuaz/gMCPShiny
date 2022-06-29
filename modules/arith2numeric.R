# Create a calculator environment to safely evaluate arithmetic expressions
# Modified from <https://stackoverflow.com/a/18391779/4568207>

safe_f <- c(getGroupMembers("Math"), getGroupMembers("Arith"), "(", "pi")
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


#' Convert arithmetic (character) format to numeric
#'
#' @param x character string containing arithmetic operators
#'
#' @examples
#' # return to the numeric values
#' arithmetic_to_numeric("1/3")
#' arithmetic_to_numeric("sqrt(2)")
#' arithmetic_to_numeric("log(2)")
#' arithmetic_to_numeric("exp((-3^2)/2)/sqrt(2*pi)")
#'
#' # retrun to NA if containing non-arithmetic string
#' arithmetic_to_numeric("1/3a")
arithmetic_to_numeric <- function(x){
  w_numeric <- try(safe_eval_str(as.character(x)), silent = TRUE)
  if (inherits(w_numeric, "try-error")) NA
  else w_numeric
}
