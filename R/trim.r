#' This is a function remove space characters from the head and tail of a string
#' @param 
#' @keywords string
#' @export
#' @examples
#' trim(' cats haunt me ')

trim <- function (j) {
    gsub("^\\s+|\\s+$", "", j)
}