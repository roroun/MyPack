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
l1 = 'L1'
l2 = 'L2'


t.desc = function(db, trt, aval, l1, l2, stats = 'common', dp = 0, layout = 'long1') {
  
  # Check
  stats = toupper(stats)
  if (! stats %in% c('COMMON', 'N', 'MEAN', 'SD', 'MED', 'MIN', 'MAX', 'Q1', 'Q3', 'CV')) {
    stop("'stats' must be 'common', 'n', 'mean', 'sd', 'med', 'min', 'max', 'q1', 'q3', 'cv'")
  } 
  if (stats %in% 'COMMON') {
    stats = c('N', 'MEAN', 'SD', 'MED', 'MIN', 'MAX')
  } 
  
  if (! layout %in% c('long1', 'long2', 'wide')) {
    stop("'layout' must be 'long1', 'long2', 'wide'")
  } 
  
  # create l1/l2 if any is missing
  arg.lvl = c('l1', 'l2')
  arg = names(as.list(match.call())[-1])
  arg.mis = arg.lvl[! arg.lvl %in% arg]
  
  for (i in arg.mis) {
    db[, paste0(toupper(i), '_RW') := 'ALL']
    
    assign(i, paste0(toupper(i), '_RW'))
  }
  
  # define variable levels
  for (i in c('trt', 'l1', 'l2')) {
    lvl.var = paste0('lvl.', i)
    
    assign(lvl.var, 
           db[, levels(i), env = list(i = get(i))])
    
    if(is.null(get(lvl.var))){
      assign(lvl.var, 
             db[order(i), unique(i), env = list(i = get(i))])
    }
  }
  
  # Calculate stats
  out =
    db[! is.na(aval),
       .(N = .N %>% as.character(),
         MEAN = mean(aval) %>% 
           DescTools::Format(digits = dp + 1) %>% as.character(),
         SD = sd(aval) %>% 
           DescTools::Format(digits = dp + 2) %>% as.character(),
         MIN = min(aval) %>% 
           DescTools::Format(digits = dp) %>% as.character(),
         MAX = max(aval) %>% 
           DescTools::Format(digits = dp) %>% as.character(),  
         
         # type = 2 (SAS default)
         MED = quantile(aval, probs = 0.5, type = 2) %>% 
           DescTools::Format(digits = dp + 1) %>% as.character(),
         Q1 = quantile(aval, probs = 0.25, type = 2) %>% 
           DescTools::Format(digits = dp + 1) %>% as.character(),   
         Q3 = quantile(aval, probs = 0.75, type = 2) %>% 
           DescTools::Format(digits = dp + 1) %>% as.character(),
         
         CV = (sd(aval) / mean(aval) * 100) %>% 
           DescTools::Format(digits = 1) %>% as.character()
       ),
       by = .(trt, l1, l2),
       env =
         list(aval = aval, l2 = l2, l1 = l1, trt = trt)]
  
  out[, `:=`(
    trt = factor(trt, levels = lvl.trt),
    l1 = factor(l1, levels = lvl.l1),
    l2 = factor(l2, levels = lvl.l2)
  ),
  env = list(trt = trt, l1 = l1, l2 = l2)]
  
  # Add missing trt groups
  trt.mis =
    lvl.trt[! lvl.trt %in% out[, unique(trt), env = list(trt = trt)]]
  
  if (length(trt.mis) > 0) {
    out =
      rbindlist(
        list(
          out,
          out[, .(trt = trt.mis, N = '0'), by = .(l1, l2), 
              env = list(trt = trt, l1 = l1, l2 = l2)]
        ),
        fill = T, use.names = T
      )
  }
  
  # Display: Wide format
  arg.lvl1 = arg.lvl[! arg.lvl %in% arg.mis] %>% sort(decreasing = T)
  
  if (length(arg.lvl1) > 0) {
    out.var = c(unlist(mget(arg.lvl1), use.names = F), trt, stats)
  } else out.var = c(trt, stats)
  
  out.wide = 
    out[order(l2, l1, trt),
        ..out.var,
        env = list(l2 = l2, l1 = l1, trt = trt)]
  
  
  ### HERE
  
  
  
  
  
  # Display: long1 format
  idvar = c(get(arg.lvl[! arg.lvl %in% arg.mis]), trt)
  idvar = get(arg.lvl[! arg.lvl %in% arg.mis])
  
  out.long1 =
    out.wide[order(trt),
             env = list(trt = trt)]  %>%
    reshape(direction = 'long', idvar = idvar, timevar = 'SEQ',
            v.names = 'RESULT',
            varying = stats) %>%
    reshape(direction = 'wide', idvar = c(idvar[-length(idvar)], 'SEQ'), timevar = trt,
            v.names = 'RESULT',
            varying = list(lvl.trt))
  
  idvar1 = c(idvar[-length(idvar)], 'SEQ')

  out.long1 =
    out.long1[order(mget(idvar1))]
  
  
  
  
  out2.long[, `:=`(
    SEQ = recode(SEQ, `1` = '_n', `2` ='MEAN', `3` = 'SD', `4` = 'MED', `5` = 'MIN', `6` = 'MAX')
  )]

  out2.long[, `:=`(
    SEQ = factor(SEQ, levels = c('_n', 'MEAN', 'SD', 'MED', 'MIN', 'MAX'))
  )]

  
  if (layout %in% 'long1') return(out2.long) 
  if (layout %in% 'long2') return(out2.long)
  if (layout %in% 'wide') return(out.wide)

}

t.desc(db = db, aval = 'AVAL', l1 = 'L1', l2 = 'l2') %>% View()



