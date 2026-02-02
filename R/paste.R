#' Combine multiple columns into one by pasting strings together
#'
#' @param .SD A dataset/data.table
#' @param collapse A character string to separate the results
#'
#' @return A character vector of the concatenated values after removing NA and ''  
#' @export
#'
#' @examples
#' # Used in the data.table
#' # dbin[, .(NEW = rw.paste(.SD[, c('VAR1', 'VAR2', 'VAR3')], collapse = ''))]
#' 

# TEST DB ----
# dbin = 
#   data.table(
#     VAR1 = sample(c('A', 'B', 'C', ''), 10, replace = T),
#     VAR2 = sample(c(1:3, NA), 10, replace = T),
#     VAR3 = sample(c('#', '$', '*', '', NA), 10, replace = T)
#   )

# Func
rw.paste = function(.SD, collapse = ' ') {
  apply(.SD, MARGIN = 1, FUN = function(x) {
    y = stringr::str_trim(x, side = 'left')
    paste(y[! y %in% c("", NA)], collapse = collapse)
  })
} 


