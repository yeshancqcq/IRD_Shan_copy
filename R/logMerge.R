logMerge <- function(input1, input2){
  output <- vector()
  length <- length(input1)
  for(i in 1:length){
    if(isTRUE(input[1]&input2[2])){
      output <- c(output, TRUE)
    } else {
      output <- c(output, FALSE)
    }
  }
  return(output)
}