convert_date <- function(x, i) {
  
  # We use the `i` index so that we can keep the coordinate outputs from the 
  #  regex in a smaller list.
  dms_test <- data.frame(deg = as.numeric(x[,2]), 
                         min = as.numeric(x[,3]) / 60,
                         sec = as.numeric(x[,4]) / 60 ^ 2, 
                         stringsAsFactors = FALSE)
  
  dms_test <- rowSums(dms_test, na.rm = TRUE)
  
  domain <- (str_detect(x[,5], 'N') * 1 +
               str_detect(x[,5], 'E') * 1 +
               str_detect(x[,5], 'W') * -1 +
               str_detect(x[,5], 'S') * -1) *
    dms_test
  
  publ <- match(nlp_test$`_gddid`[i], publications_test$`_gddid`)
  
  point_pairs_test <- data.frame(sentence = nlp_test$word[i],
                                 lat = domain[x[,5] %in% c('N', 'S')],
                                 lng = domain[x[,5] %in% c('E', 'W')],
                                 publications_test[publ,],
                                 date_test <- str_match(nlp_test$word, 
                                                              "(\\d+(?:[.]\\d+)*),((?:-{1,2})|(?:to)),(\\d+(?:[.]\\d+)*),([a-zA-Z]+,BP),") %>% na.omit()
                                 stringsAsFactors = FALSE)
  
  return(point_pairs_test)  
}

#if(isTRUE(date_range_test[coord_idx])){
# coordinates_test[[coord_idx]] <- str_match(nlp_test$word[coord_idx], 
# "(\\d+(?:[.]\\d+)*),((?:-{1,2})|(?:to)),(\\d+(?:[.]\\d+)*),([a-zA-Z]+,BP)," %>% na.omit())
#    } else if(isTRUE(number_test[coord_idx])){
#                            ",(\\d+(?:[\\.\\s]\\d+){0,1}),.*?,yr,"},