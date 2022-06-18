df2graph <- function(namesH,df) {
  # Check to make sure the column names are present
  if (!any(names(df) %in% c("From", "To", "Weight"))) {
    print("Must have three columns in the input data frame: 'From', 'To', and 'Weight'")
  }
  dim <- length(namesH)
  m <- matrix(rep(0, dim^2), nrow = dim)

  for (i in 1:nrow(df)){
    row.index <- which(namesH%in%df$From[i])
    col.index <- which(namesH%in%df$To[i])
    w_numeric <- try(eval(parse(text=df$Weight[i])), silent = TRUE)
    m[row.index,col.index] <- ifelse(class(w_numeric) == "try-error", NA, w_numeric)
  }
  m
}
