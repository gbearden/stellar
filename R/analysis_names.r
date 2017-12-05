#' This is a function to format variable names to make analysis easier
#' @param 
#' @keywords column, variable, names, data frame
#' @export
#' @examples
#' analysis_names(your_df)

analysis_names <- function(df){

    colnames(df) <- gsub("\\#", "", colnames(df))
    colnames(df) <- gsub("^\\s+|\\s+$", "", colnames(df))
    colnames(df) <- gsub("\\ |\\.|\\,|\\-|\\/|\\__", "_", colnames(df))
    colnames(df) <- gsub("\\__", "_", colnames(df))
    colnames(df) <- tolower(colnames(df))

    return(df)
}