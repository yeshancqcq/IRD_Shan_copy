regex.combine <- function(df,regex_df){
  regex_row <- nrow(regex_df)
  vector.app <- vector()
  for(i in 1:regex_row){
    vector <- vector()
    vector <- match.regex(regex_df$regex[i],df)
    for(j in 1:length(vector)){
      if (is.null(vector.app[j])){
        vector.app[j] <- vector[j]
      } else if (vector.app[j] & vector[j]){
        vector.app[j] <- TRUE
      } else {
        vector.app[j] <- FALSE
      }
    }
  }
  return(vector.app)
}