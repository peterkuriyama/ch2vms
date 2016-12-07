#' Convert missing values to zeroes

#' Function to convert NAs to 0

#' @param vector_in Vector to manipulate

#' @export
#' @examples
#' na_to_zero(c(NA, NA, 2))


na_to_zero <- function(vector_in){
  
  vector_in[which(is.na(vector_in))] <- 0
  vector_out <- vector_in
  return(vector_out)  
}

