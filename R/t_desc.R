#' Descriptive Summary for Numeric Variables
#'
#' @param db A dataset, e.g., ADSL
#' @param trt A treatment variable, e.g., 'TRTA'
#' @param aval The numeric variable to be summarized, e.g., AVAL
#' @param l1 Level 1, e.g., 'AVISIT'
#' @param l2 Level 2, e.g., 'PARAMCD'
#' @param stats Character. Which stats to produce. Either “common” (default), or a selection of : 
#' 'n', 'mean', 'sd', 'mean_sd', 'med', 'q1', 'q3', 'iqr', 'med_iqr', 'min', 'max', 'range', 'cv', 'ci'. 
#' CI is calculated based on t distribution.
#' @param dp Decimal place to be displayed
#' @param alpha Alpha value for CI calculation
#' @param direction Display direction: long or wide
#'
#' @return A dataset with common stats, including n, mean, sd, min, max
#' @export

# TEST DB ----
# dbin =
#   data.table(
#     USUBJID = paste0('P',
#                      str_pad(1:100, width = 3, side = 'left', pad = '0')),
#     TRT = sample(c('TEST', 'CONTROL'), size = 100, replace = T),
#     AVAL = runif(100, min = 0, max = 100) %>% round(digits = 1),
#     L1 = sample(c('V1', 'V2'), size = 100, replace = T),
#     L2 = sample(c('PARAM1', 'PARAM2'), size = 100, replace = T)
#   )
# 
# db = dbin[TRT %in% 'TEST']
# trt = 'TRT'
# aval = 'AVAL'
# l1 = 'L1'
# l2 = 'L2'

# FUNC ----
rw.desc = function(db, trt, aval, l1, l2, stats = 'common', dp = 0, alpha = 0.05, direction = 'long') {
  . = lvl.trt = lvl.l1 = lvl.l2 = STATS = MEAN = MEAN_RAW = SD = MED = IQR = MIN = MAX = CI_MARGIN = NULL   # No visible binding for global variable
  
  db = data.table::data.table(db)
  
  # Check arguments #
  stats = toupper(stats)
  if (any(! stats %in% c('COMMON', 'N', 'MEAN', 'SD', 'MEAN_SD', 'MED', 'Q1', 'Q3', 'IQR', 'MED_IQR', 'MIN', 'MAX', 'RANGE', 'CV', 'CI'))) {
    stop("'stats' must be 'common', 'n', 'mean', 'sd', 'mean_sd', 'med', 'q1', 'q3', 'iqr', 'med_iqr', 'min', 'max', 'range', 'cv', 'ci'.")
  } 
  if (any(stats %in% 'COMMON')) {
    stats = append(stats, c('N', 'MEAN', 'SD', 'MED', 'MIN', 'MAX'), after = which(stats %in% 'COMMON'))
    stats = stats[-which(stats %in% 'COMMON')]
  } 
  if (any(stats %in% 'CI')) {
    stats[which(stats %in% 'CI')] = paste0('CI_', (1-alpha)*100)
  } 
  
  if (! direction %in% c('long', 'wide')) {
    stop("'direction' must be 'long', 'wide'")
  } 
  
  # create l1/l2 if any of them is missing #
  arg.lvl = c('l1', 'l2')
  arg = names(as.list(match.call())[-1])
  arg.mis = arg.lvl[! arg.lvl %in% arg]
  arg.lvl.in = arg.lvl[! arg.lvl %in% arg.mis] %>% sort(decreasing = T)
  
  for (i in arg.mis) {
    db[, paste0(toupper(i), '_RW') := '_ALL']
    
    assign(i, paste0(toupper(i), '_RW'))
  }
  
  # define variable levels #
  for (i in c('trt', 'l1', 'l2')) {
    lvl.var = paste0('lvl.', i)
    
    assign(lvl.var, 
           db[, levels(i), env = list(i = get(i))])
    
    if(is.null(get(lvl.var))){
      assign(lvl.var, 
             db[order(i), unique(i), env = list(i = get(i))])
    }
  }
  
  # Calculate stats #
  out =
    db[! is.na(aval),
       .(N = .N %>% as.character(),
         MEAN = mean(aval) %>% 
           DescTools::Format(digits = dp + 1) %>% as.character(),
         MEAN_RAW = mean(aval),
         SD = sd(aval) %>% 
           DescTools::Format(digits = dp + 2) %>% as.character(),
         SE = (sd(aval) / sqrt(.N)) %>% DescTools::Format(digits = 2) %>% as.character(),
         MIN = min(aval) %>% 
           DescTools::Format(digits = dp) %>% as.character(),
         MAX = max(aval) %>% 
           DescTools::Format(digits = dp) %>% as.character(),  
         CI_MARGIN = stats::qt(1-(alpha/2),df = .N-1) * sd(aval) / sqrt(.N),
         
         # type = 2 (SAS default)
         MED = quantile(aval, probs = 0.5, type = 2) %>% 
           DescTools::Format(digits = dp + 1) %>% as.character(),
         Q1 = quantile(aval, probs = 0.25, type = 2) %>% 
           DescTools::Format(digits = dp + 1) %>% as.character(),   
         Q3 = quantile(aval, probs = 0.75, type = 2) %>% 
           DescTools::Format(digits = dp + 1) %>% as.character(),
         IQR = (quantile(aval, probs = 0.75, type = 2) - quantile(aval, probs = 0.25, type = 2)) %>% 
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
    l2 = factor(l2, levels = lvl.l2),
    MEAN_SD = paste0(MEAN, ' (', SD, ')'),
    MED_IQR = paste0(MED, ' (', IQR, ')'),
    RANGE = paste(MIN, MAX, sep = ', ')
  ),
  env = list(trt = trt, l1 = l1, l2 = l2)]
  
  out[, paste0('CI_', (1-alpha)*100) :=
        paste0((MEAN_RAW - CI_MARGIN) %>% DescTools::Format(digits = dp + 2)
               , ', ',
               (MEAN_RAW + CI_MARGIN) %>% DescTools::Format(digits = dp + 2))]
  
  # Add missing trt groups #
  trt.mis =
    lvl.trt[! lvl.trt %in% out[, unique(trt), env = list(trt = trt)]]
  
  if (length(trt.mis) > 0) {
    out =
      data.table::rbindlist(
        list(
          out,
          out[, .(trt = trt.mis, N = '0'), by = .(l1, l2), 
              env = list(trt = trt, l1 = l1, l2 = l2)]
        ),
        fill = T, use.names = T
      )
  }
  
  out =
    out[, lapply(.SD, FUN = function(x) {
      if (is.character(x)) {
        dplyr::recode(x, .missing = '')
      } else x 
    })]
  
  # Display: Wide format #
  if (length(arg.lvl.in) > 0) {
    out.var = c(unlist(mget(arg.lvl.in), use.names = F), trt, stats)
  } else out.var = c(trt, stats)
  
  out.wide = 
    out[order(l2, l1, trt),
        out.var, with = F,
        env = list(l2 = l2, l1 = l1, trt = trt)]
  
  # Display: long format #
  var.lvl = mget(arg.lvl.in) %>% unlist(use.names = F)
  
  if (length(arg.lvl.in) > 0) {
    out.long =
      out.wide[order(trt),
               env = list(trt = trt)]  %>%
      reshape(direction = 'long', idvar = c(var.lvl, trt), timevar = 'STATS', times = stats,
              v.names = 'RESULT',
              varying = stats) %>%
      reshape(direction = 'wide', idvar = c(var.lvl, 'STATS'), timevar = trt,
              v.names = 'RESULT',
              varying = list(lvl.trt))
  } else {
    out.long =
      data.table::transpose(out.wide, make.names = trt, keep.names = 'STATS')
  }
  
  out.long[, `:=`(
    STATS = factor(STATS, levels = stats)
  )]
  
  expr = paste0('order(',  paste(var.lvl, collapse = ', '), ', STATS)') %>% parse(text = .)
  
  out.long = 
    out.long[eval(expr)]
  
  # Output #
  if (direction %in% 'long') return(out.long) 
  if (direction %in% 'wide') return(out.wide)
}

