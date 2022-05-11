df2graph <- function(df) {
  # Check to make sure the column names are present
  if (!any(names(df) %in% c("From", "To", "Weight"))) {
    print("Must have three columns in the input data frame: 'From', 'To', and 'Weight'")
  }

  maxUnNodes <- max(unique(c(df$From, df$To)))
  m <- matrix(rep(0, maxUnNodes^2), nrow = maxUnNodes)
  m[df$From + maxUnNodes * (df$To - 1)] <- df$Weight # From = rows, and To = columns

  m
}
