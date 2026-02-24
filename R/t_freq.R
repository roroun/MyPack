# SETTING ----
list.packages = 
  c('data.table', 'DescTools', 'dplyr', 'stringr')

invisible(lapply(list.packages, library, character.only = TRUE))


# TEST DB ----
dbin =
  data.table(
    TRT = sample(c('TEST1', 'TEST2', 'CONTROL'), size = 100, replace = T),
    VAR1 = sample(c('L1_1', 'L1_2', 'L1_3'), size = 100, replace = T),
    VAR2 = sample(c('L2_1', 'L2_2'), size = 100, replace = T),
    VAR3 = sample(c('L3_1', 'L3_2'), size = 100, replace = T)
  )

db = copy(dbin) %>% filter(TRT %in% c('TEST1', 'CONTROL'))
trt = 'TRT'
l1 = 'VAR1'
l2 = 'VAR2'
N = 'n'
N = c('CONTROL' = 100, 'TEST1' = 100, 'TEST2' = 100)
stats = c('n')
ci = F
ci.method = 'clopper-pearson'
conf.level = 0.95
diffci.method = 'scorecc'
diff.reff = 'CONTROL'

# FUNC  ----
#' Frequency Summary for Categorical Variables
#'
#' @param db A dataset, e.g., ADSL
#' @param trt A treatment variable, e.g., 'TRTA'
#' @param l1 Level 1, e.g., 'AVALC'
#' @param l2 Level 2, e.g., 'AVISIT'
#' @param l3 Level 3, e.g., 'PARAMCD'
#' @param N The sample size serves as the denominator in the percentage calculation. 'n' for observed records (default)); 
#' a table or named vector to specify the n for each group, e.g., c('Group A' = 100, 'Group B' = 1000)
#' @param stats Character. Which stats to produce. Either “n_pct” (default), or a selection of : 
#' 'n', 'n_n', 'n_pct', 'pct', 'ci', 'diff', 'diffci' 
#' @param ci.method A character string, specifying the method for CI from BinomCI; e.g., "clopper-pearson", "wald", "wilson", "logit"
#' @param conf.level The confidence level, defaults to 0.95
#' @param diffci.method # A character string, specifying the method for CI of diff from BinomDiffCI;
#' e.g., 'wald' for traditional Wald; 'score' for Newcombe; 'scorecc' for Newcombe with continuity correction 
#' @param diff.reff A character string, specify the trt group to be the reference for comparison
#'
#' @return A dataset with frequency counts, CI, diff, and/or CI of diff
#' @export

rw.freq = function(db, trt, l1, l2, l3, N = 'n', stats = 'n_pct',
                   ci.method = 'clopper-pearson', conf.level = 0.95,
                   diffci.method, diff.reff) {
  
  # Check arguments #
  if ( 
    ! any(N == 'n', is.table(N), (is.vector(N) & ! is.null(names(N)))) 
  ) stop("N must be 'n', a named vector, or a table.")
  
  stats = toupper(stats)
  if (any(! stats %in% c('N', 'N_N', 'N_PCT', 'PCT', 'CI', 'DIFF', 'DIFFCI'))) {
    stop("'stats' must be 'n', 'n_n', 'n_pct', 'pct', 'ci', 'diff', 'diffci'.")
  } 
  
  # create l2/l3 if any of them is missing #
  arg.lvl = c('l1', 'l2', 'l3')
  arg = names(as.list(match.call())[-1])
  arg.mis = arg.lvl[! arg.lvl %in% arg]
  arg.lvl.in = arg.lvl[! arg.lvl %in% arg.mis] %>% sort(decreasing = T)
  
  for (i in arg.mis) {
    db[, paste0(toupper(i), '_RW') := '_ALL']
    
    assign(i, paste0(toupper(i), '_RW'))
  }
  
  # define variable levels #
  for (i in c('trt', 'l1', 'l2', 'l3')) {
    lvl.var = paste0('lvl.', i)
    
    assign(lvl.var, 
           db[, levels(i), env = list(i = get(i))])
    
    if(is.null(get(lvl.var))){
      assign(lvl.var, 
             db[order(i), unique(i), env = list(i = get(i))])
    }
  }
  
  # Calculate stats #
  out1 =
    db[, table(l3, l2, l1, trt),
       env = list(l3 = l3, l2 = l2, l1 = l1, trt = trt)] %>%
    addmargins(margin = c(3, 4), FUN = list(`_n` = sum, OVERALL = sum)) %>%
    data.table::as.data.table()
  
  # Add PCT and CL
  if(any(is.table(N), (is.vector(N) & ! is.null(names(N))))){
    n.trt = data.table::as.data.table(N, keep.rownames = 'TRT')
    
    db.n =
      rbind(
        n.trt,
        n.trt[, .(TRT = 'OVERALL', N = sum(N))]
      )
    
    for (i in db.n$TRT) {
      n = db.n[TRT %in% i, N]
      
      out1[l1 %in% '_n' & trt %in% i,
           N := n,
           env = list(l1 = l1, trt = trt)]
    }
  }
  
  out1[,
       `:=`(
         N_N = paste0(N, '/', N[l1 %in% '_n']),
         PCT =
           (DescTools::BinomCI(N, N[l1 %in% '_n'], 
                               method = ci.method, conf.level = conf.level)[, 'est'] * 100) %>%
           DescTools::Format(digits = 1) %>% as.character(),
         CI.LL =
           (DescTools::BinomCI(N, N[l1 %in% '_n'],
                               method = ci.method, conf.level = conf.level)[, 'lwr.ci'] * 100) %>%
           DescTools::Format(digits = 1) %>% as.character(),
         CI.UL =
           (DescTools::BinomCI(N, N[l1 %in% '_n'], 
                               method = ci.method, conf.level = conf.level)[, 'upr.ci'] * 100) %>%
           DescTools::Format(digits = 1) %>% as.character()
       ),
       by = .(l3, l2, trt),
       env = list(l3 = l3, l2 = l2, l1 = l1, trt = trt)]
  
  out1[, `:=`(
    N_N = fcase(l1 %in% '_n', '', default = N_N),
    PCT = fcase(l1 %in% '_n', '', default = PCT),
    N_PCT = fcase(
      l1 %in% '_n', as.character(N),
      N > 0, paste0(N, ' (', PCT, ')'),
      default = '0'
    ),
    CI =
      fcase(
        l1 %in% '_n', '',
        default = paste0(CI.LL, ', ', CI.UL)
      )
  ),
  by = .I,
  env = list(l1 = l1)]
  
  # Display: long format 
  stats.all = names(out1)[! names(out1) %in% c(l3, l2, l1, trt)]
  
  out2 =
    reshape(out1[order(factor(trt, levels = c(lvl.trt, 'OVERALL'))),
                 env = list(trt = trt)],
            direction = 'long', idvar = c(l3, l2, l1, trt), timevar = 'STATS', times = stats.all,
            varying = stats.all,
            v.names = 'RESULT') %>% 
    reshape(direction = 'wide', idvar = c(l3, l2, l1, 'STATS'), timevar = trt,
            v.names = 'RESULT',
            varying = list(c(lvl.trt, 'OVERALL'))) %>% 
    suppressWarnings()
  
  # Add DIFF and corresponding CI
  if (! missing(diffci.method)) {
    db.diff = 
      out2[STATS %in% 'N'
      ][,
        (lvl.trt) := lapply(.SD, as.numeric),
        .SDcols = lvl.trt]
    
    lvl.trt1 = lvl.trt[! lvl.trt %in% diff.reff]
    for (i in lvl.trt1) {
      db.diff[, paste0(i1, '_DIFF') := 
                (DescTools::BinomDiffCI(x1 = i, n1 = i[l1 %in% '_n'], 
                                        x2 = diff.reff, n2 = diff.reff[l1 %in% '_n'],
                                        conf.level = conf.level)[, 'est'] * 100) %>% 
                DescTools::Format(digits = 1) %>% as.character(), 
              by = .(l3, l2),
              env = list(l3 = l3, l2 = l2, l1 = l1, i = i, i1 = I(i), diff.reff = diff.reff)]
      
      db.diff[, paste0(i1, '_DIFFCI') := 
                paste0(
                  (DescTools::BinomDiffCI(x1 = i, n1 = i[l1 %in% '_n'], 
                                          x2 = diff.reff, n2 = diff.reff[l1 %in% '_n'],
                                          conf.level = conf.level)[, 'lwr.ci'] * 100) %>% 
                    DescTools::Format(digits = 2) %>% as.character(),
                  ', ',
                  (DescTools::BinomDiffCI(x1 = i, n1 = i[l1 %in% '_n'], 
                                          x2 = diff.reff, n2 = diff.reff[l1 %in% '_n'],
                                          conf.level = conf.level)[, 'upr.ci'] * 100) %>% 
                    DescTools::Format(digits = 2) %>% as.character()
                ), 
              by = .(l3, l2),
              env = list(l3 = l3, l2 = l2, l1 = l1, i = i, i1 = I(i), diff.reff = diff.reff)]
    }
    
    db.diff1 = data.table(NULL)
    for (i in c('DIFF', 'DIFFCI')) {
      var.temp = c(l3, l2, l1, paste(lvl.trt1, i, sep = '_'))
      
      temp1 =
        db.diff[! l1 %in% '_n', 
                ..var.temp,
                env = list(l1 = l1)] 
      
      temp1[, `:=`(
        STATS = i
      )]
      
      names(temp1)[4:5] = lvl.trt1
      
      db.diff1 = rbind(db.diff1, temp1)
    }
    
    out2 =
      rbindlist(
        list(
          out2,
          db.diff1
        ),
        fill = T, use.names = T
      )
  }
  
  # Organize final output #
  out2[, `:=`(
    l1 = factor(l1, levels = c('_n', lvl.l1)),
    l2 = factor(l2, levels = lvl.l2),
    l3 = factor(l3, levels = lvl.l3)
  ),
  env = list(l1 = l1, l2 = l2, l3 = l3)]
  
  var.lvl = mget(arg.lvl.in) %>% unlist(use.names = F)
  out.var = c(var.lvl, 'STATS', lvl.trt, 'OVERALL')
  
  out3 =
    out2[l1 %in% '_n' & STATS %in% 'N' | (! l1 %in% '_n' & STATS %in% stats), 
         ..out.var,
         env = list(l1 = l1)]
  
  out3[, `:=`(
    STATS = factor(STATS, levels = c('N', stats))
  )]
  
  expr = paste0('order(',  paste(var.lvl, collapse = ', '), ', STATS)') %>% parse(text = .)
  
  out.final = 
    out3[eval(expr)]
  
  return(out.final)
  
  # 
  # out =
  #   out2[order(l2, l1, SEQ),
  #        env = list(l2 = l2, l1 = l1)
  #   ][! (l1 %in% '_n' & SEQ %in% 3),
  #     env = list(l1 = l1)]
  # 
  # if (is.null(N) & isFALSE(ci) & missing(diffci.method) ) {
  #   return(
  #     out[SEQ %in% 1,
  #         ..out.var]
  #   ) } else if (! is.null(N) & isFALSE(ci) & missing(diffci.method)) {
  #     return(
  #       out[SEQ %in% 2,
  #           ..out.var]
  #     )
  #   } else if (isTRUE(ci) & missing(diffci.method)) {
  #     return(
  #       out[SEQ %in% c(2, 3), 
  #           ..out.var]
  #     )
  #   } else if (isFALSE(ci) & ! missing(diffci.method)) {
  #     return(
  #       out[SEQ %in% c(2, 4, 5), 
  #           ..out.var]
  #     )  
  #   } else if (isTRUE(ci) & ! missing(diffci.method)) {
  #     return(
  #       out[SEQ %in% c(2, 3, 4, 5), 
  #           ..out.var]
  #     )
  #   }
  # 
}

# HERE: UNDER TESTING #
rw.freq(db = dbin, trt = 'TRT', l1 = 'VAR1', stats = c('N_N', 'PCT', 'DIFF'), diffci.method = 'score', diff.reff = 'CONTROL') %>% 
  View()




