#' This is a function remove space characters from the head and tail of a string
#' @param
#' @keywords dummy, dummies, dummy variable
#' @export
#' @examples
#' dummify(df, id_var = 'id')

dummify <- function(data, id_var) {
    require(tidyverse)

    df <- data

    cols <- df %>%
        mutate_if(is.factor, as.character) %>%
        select_if(is.character)

    name_cols <- cols %>% colnames()

    output <- df %>% select(- one_of(name_cols))

    id <- df %>% select(id_var)

    for(i in 2:(length(cols)+1)) {
        output <- output %>%
            left_join(
                bind_cols(id, cols) %>%
                filter(! name_cols[i] %in% c(NA, '')) %>%
                transmute(id, col = .[,i], val = 1) %>%
                group_by(id) %>%
                spread(col, val) %>%
                replace(is.na(.), 0)
                , by = 'id'
                )
    }

    output <- output %>% select(-contains("V1"))

    return(output)
}
