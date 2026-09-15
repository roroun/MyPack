# TEST DB ----
# dbin =
#   data.table::data.table(
#     USUBJID = paste0('P',
#                      stringr::str_pad(1:100, width = 3, side = 'left', pad = '0')),
#     TRT = sample(c('TEST', 'CONTROL'), size = 100, replace = T),
#     VAR1N = runif(100, min = 0, max = 100) |> round(digits = 1),
#     VAR2N = runif(100, min = 0, max = 100) |> round(digits = 1),
#     VARC = sample(c('VAL1', 'VAL2'), size = 100, replace = T),
#     WT = runif(100, min = 0, max = 1)
#   )

# FUNC  ----
#' Calculate Standardized Mean Difference (SMD)
#'
#' @param db A dataset, e.g., ADSL
#' @param trt A treatment variable, e.g., 'TRTA'. Two groups are supported.
#' @param var variables used for SMD calculation. For categorical variables, only binomial ones are supported.
#' @param wt A weighting variable
#' @param trt.ref A character string, specify the trt group to be the reference for comparison
#'
#' @return A vector of SDMs with or without weighting 
#' @export
#'


rw.smd =
  function(db, trt, var, wt, trt.ref){
    . = trtn = WT = NULL   # No visible binding for global variable
    
    # Set up trt class and order
    db[, trt := as.character(trt),
       env = list(trt = trt)]
    
    lvl.trt = db[order(trt), .(trt), env = list(trt = trt)] %>% unique()
    
    if (missing(trt.ref)) {
      trt.ref = lvl.trt[1] %>% as.character()
    }
    
    lvl.trt[, trtn := recode(trt, trt.ref = 1,  .default = 0),
            env = list(trt = trt, trt.ref = trt.ref)]
    
    order.trt = lvl.trt[order(trtn), trt, env = list(trt = trt)]
    
    # Identify char/num variables
    var.c = var[sapply(db[, var, with = F], is.character)]
    var.n = var[! var %in% var.c]
    
    # Convert categorical variables to numeric values, if present
    db[, (var.c) := lapply(.SD, FUN = function(x) {
      as.factor(x) |> as.numeric() - 1
    }),
    .SDcols = var.c]
    
    # Weighting variable
    if (missing(wt)) {
      db[, WT := 1]
      wt = 'WT'
    }
    
    # Calculate the required parameters 
    par.mean =
      db[, lapply(.SD, FUN = function(x) {
        sum(x * wt, na.rm = T) / sum(wt, na.rm = T)
      }),
      .SDcols = var,
      by = .(trt),
      env = list(trt = trt, wt = wt)]
    
    par.mean =
      par.mean[order(factor(trt, levels = order.trt)),
               env = list(trt = trt)]
    
    par.n.sd =
      db[, lapply(.SD, FUN = function(x, wt) {
        Hmisc::wtd.var(x, weights = wt) |> sqrt()
      }),
      .SDcols = var.n,
      by = .(trt),
      env = list(trt = trt, wt = wt)]
    
    par.c.sd = 
      par.mean[, lapply(.SD, FUN = function(x) {
        sqrt(x * (1-x))
      }),
      .SDcols = var.c,
      by = .(trt),
      env = list(trt = trt)]
    
    par.sd.pooled =
      merge(
        par.n.sd, par.c.sd, 
        by = trt,
        all = T
      ) |> 
      dplyr::select(var) |> 
      apply(MARGIN = 2, 
            FUN = function(x) {
              sqrt(sum(x^2) / 2)
            }) |> 
      as.list() |> 
      as.data.table()
    
    par.sd.pooled[, (trt) := 'SD.POOLED']
    
    dbin = 
      rbind(
        par.mean,
        par.sd.pooled
      )
    
    smd =
      apply(dbin[, -1], MARGIN = 2, 
            FUN = function(x) {
              (x[1] - x[2]) / x[3]
            })
    
    return(smd)
  } 

