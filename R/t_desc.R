# TEST DB ----
dbin =
  data.table(
    paste0('P',
           str_pad(1:100, width = 3, side = 'left', pad = '0')),
    TRT = sample(c('TEST', 'CONTROL'), size = 100, replace = T),
    AVAL = runif(100, min = 0, max = 100) %>% round(digits = 1),
    L1 = sample(c('V1', 'V2'), size = 100, replace = T),
    L2 = sample(c('PARAM1', 'PARAM2'), size = 100, replace = T)
  )

db = dbin[TRT %in% 'TEST']
trt = 'TRT'
aval = 'AVAL'
l2 = 'L2'
l1 = 'L1'

#' Title: Descriptive Summary for Numeric Variables
#'
#' @param db A dataset, e.g., ADSL
#' @param trt A treatment variable, e.g., 'TRTA'
#' @param aval The numeric variable to be summarized, e.g., AVAL
#' @param l1 Level 1, e.g., 'AVISIT'
#' @param l2 Level 2, e.g., 'PARAMCD'
#' @param dp Decimal place to be displayed
#' @param direction Display direction: long or wide
#'
#' @return A dataset with common stats, including n, mean, sd, min, max
#' @export
#'
#' @examples
t.desc = function(db, trt, aval, l1, l2, dp = 0, direction = 'long') {
  
  if(missing(l2)){
    db[, L2_VST := 'ALL']
    
    l2 = 'L2_VST'
  }
  
  if(missing(l1)){
    db[, L1_VST := 'ALL']
    
    l1 = 'L1_VST'
  }
  
  lvl.trt = levels(db[, trt, env = list(trt = trt)])
  if(! length(lvl.trt)){
    lvl.trt = db[order(trt), unique(trt), env = list(trt = trt)]
  }
  
  lvl.l2 = levels(db[, l2, env = list(l2 = l2)])
  if(! length(lvl.l2)){
    lvl.l2 = db[order(l2), unique(l2), env = list(l2 = l2)]
  }
  
  lvl.l1 = levels(db[, l1, env = list(l1 = l1)])
  if(! length(lvl.l1)){
    lvl.l1 = db[order(l1), unique(l1), env = list(l1 = l1)]
  }
  
  out1 =
    db[! is.na(aval),
       .(N.Valid = .N,
         Mean = mean(aval),
         Std.Dev = sd(aval),
         Min = min(aval),
         Median = median(aval),
         Max = max(aval)
       ),
       by = .(l2, l1, trt),
       env =
         list(aval = aval, l2 = l2, l1 = l1, trt = trt)]
  
  out2.wide =
    out1[, .(
      l2, l1, trt, N.Valid = Format(N.Valid, digits = 0),
      Mean = Format(Mean, digits = dp + 1),
      Std.Dev = Format(Std.Dev, digits = dp + 2),
      Min = Format(Min, digits = dp),
      Median = Format(Median, digits = dp + 1),
      Max = Format(Max, digits = dp)
    ),
    env =
      list(l2 = l2, l1 = l1, trt = trt)]
  
  out2.wide[, `:=`(
    trt = factor(trt, levels = lvl.trt),
    l2 = factor(l2, levels = lvl.l2),
    l1 = factor(l1, levels = lvl.l1)
  ),
  env = list(trt = trt, l2 = l2, l1 = l1)]
  
  out2.wide =
    out2.wide[order(l2, l1, trt),
              env = list(l2 = l2, l1 = l1, trt = trt)]
  
  # Get trt levels observed
  lvl.trt1 = 
    lvl.trt[lvl.trt %in% out1[, unique(trt), env = list(trt = trt)]]
  
  out2.long =
    out2.wide[order(trt),
              env = list(trt = trt)]  %>%
    reshape(direction = 'long', idvar = c(l2, l1, trt), timevar = 'SEQ',
            v.names = 'RESULT',
            varying = c('N.Valid', 'Mean', 'Std.Dev', 'Median', 'Min', 'Max')) %>%
    reshape(direction = 'wide', idvar = c(l2, l1, 'SEQ'), timevar = trt,
            v.names = 'RESULT',
            varying = list(lvl.trt1))
  
  out2.long =
    out2.long[order(l2, l1, SEQ),
              env = list(l2 = l2, l1 = l1)]
  
  out2.long[, `:=`(
    SEQ = recode(SEQ, `1` = '_n', `2` ='Mean', `3` = 'SD', `4` = 'Median', `5` = 'Min', `6` = 'Max')
  )]
  
  out2.long[, `:=`(
    SEQ = factor(SEQ, levels = c('_n', 'Mean', 'SD', 'Median', 'Min', 'Max'))
  )]
  
  if (direction == 'long') {
    return(out2.long)
  } else if (direction == 'wide') {
    return(out2.wide)
  } else stop('Warning: direction value must be "long" or "wide"')
  
}





