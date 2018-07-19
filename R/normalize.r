#' This function normalize a values in a numeric variable
#' @param 
#' @keywords normalize, standardize, zero, one, numeric
#' @export
#' @examples
#' normalize(df$numeric_variable)
#' normalize(string_of_numbers)
#' normalize(c(1,3,4))

normalize <- function(j){
    (j-min(j,na.rm=TRUE))/(max(j,na.rm=TRUE)-min(j,na.rm=TRUE))
 }