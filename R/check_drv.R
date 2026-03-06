# SETTING ----
# list.packages = 
#   c('dplyr', 'stringr', 'data.table')
# 
# invisible(lapply(list.packages, library, character.only = TRUE))


# TEST DB ----
# dbin =
#   data.table(
#     USUBJID = paste0('P',
#                     str_pad(1:100, width = 3, side = 'left', pad = '0')),
#     VAR = sample(c('LVL1', 'LVL2', 'LVL3'), size = 100, replace = T)
#   )
# 
# dbin[, `:=`(
#   x0 = fcase(
#     VAR %in% c('LVL1', 'LVL2'), 'LOW',
#     default = 'HIGH'
#   ), 
#   x1 =
#     fcase(
#       VAR %in% c('LVL1', 'LVL3'), 'LOW',
#       default = 'HIGH'
#     ),
#   x0_RW = fcase(
#     VAR %in% c('LVL1', 'LVL2'), 'LOW',
#     default = 'HIGH'
#   )
# )]

# FUNC ----
#' Check equality between two variables
#'
#' @param x0 A variable to be checked
#' @param x1 The variable used to check x0. If x1 is not provided, <x0>_RW will be used. 
#' For example, if x0 = VAR, then VAR_RW will be the default for comparison.
#' @param db A dataset
#' @param var.info The variables to be displayed along with x0 and x1 for information
#'
#' @return A dataset showing records with non-identical values 

rw.check.drv = 
  function(x0, x1, db, var.info){
    db = data.table::as.data.table(db)
    
    if (missing(x1)) {
      x1 = paste0(x0, '_RW')
    }
    
    if (missing(var.info)) {
      var.info = names(db)
    } else {
      var.info = c(var.info, x0, x1)
    }
    
    out = 
      db[x0 != x1 |
           (x0 %in% c('', NA) & ! x1 %in% c('', NA)) |
           (x1 %in% c('', NA) & ! x0 %in% c('', NA)),
         var.info, with = F,
         env = list(x0 = x0, x1 = x1)]
    
    return(out)
  }


