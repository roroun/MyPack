# SETTING ----
list.packages = 
  c('data.table', 'DescTools', 'dplyr', 'rio', 'stringr')



# TEST DB ----
dbin =
  data.table(
    TRT = sample(c('TEST1', 'TEST2', 'CONTROL'), size = 100, replace = T),
    VAR1 = sample(c('L1_1', 'L1_2', 'L1_3'), size = 100, replace = T),
    VAR2 = sample(c('L2_1', 'L2_2'), size = 100, replace = T)
  )

db = dbin[TRT %in% c('TEST1', 'CONTROL')]
trt = 'TRT'
l1 = 'VAR1'
l2 = 'VAR2'
N = NULL
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
#' @param N The sample size serves as the denominator in the percentage calculation. 
#' NULL to only display the counts (default); 'n' for observed records; 
#' a table or vector to specify the n for each group, e.g., c('Group A' = 100, 'Group B' = 1000)
#' @param ci Logical. If FALSE, not display CI (default).
#' @param ci.method A character string, specifying the method for CI from BinomCI; e.g., "clopper-pearson", "wald", "wilson", "logit"
#' @param conf.level The confidence level, defaults to 0.95
#' @param diffci.method # A character string, specifying the method for CI of diff from BinomDiffCI;
#' e.g., 'wald' for traditional Wald; 'score' for Newcombe; 'scorecc' for Newcombe with continuity correction 
#' @param diff.reff A character string, specify the trt group to be the reference for comparison
#'
#' @return A dataset with frequency counts, CI, diff, and/or CI of diff
#' @export

rw.freq = function(db, trt, l1, l2, l3, N = NULL, 
                   ci = FALSE, ci.method = 'clopper-pearson', conf.level = 0.95,
                   diffci.method, diff.reff) {
  
  # Check arguments #
  
  # HERE 
  if ( 
    ! any(is.table(N) | N == 'n' | (is.vector(N) & ! is.character(N)) ) 
  ) 
    stop("N must be ...")
  
   
  
  N = 'n'
  N = 'D'
  N = c(1, 3)
  is.vector(N)
  

  
  
  if(missing(l2)){
    db[, L2_VST := '_ALL']
    
    l2 = 'L2_VST'
  }
  
  lvl.trt = db[, levels(trt), env = list(trt = trt)]
  if(! length(lvl.trt)) {
    lvl.trt = db[order(trt), unique(trt), env = list(trt = trt)]
  }
  
  lvl.l1 = db[, levels(l1), env = list(l1 = l1)]
  if(! length(lvl.l1)) {
    lvl.l1 = db[order(l1), unique(l1), env = list(l1 = l1)]
  }
  
  lvl.l2 = db[, levels(l2), env = list(l2 = l2)]
  if(! length(lvl.l2)) {
    lvl.l2 = db[order(l2), unique(l2), env = list(l2 = l2)]
  }
  
  db[, `:=`(
    trt = factor(trt, levels = lvl.trt),
    l1 = factor(l1, levels = lvl.l1),
    l2 = factor(l2, levels = lvl.l2)
  ),
  env = list(trt = trt, l1 = l1, l2 = l2)]
  
  # Counting #
  out1 =
    db[, table(l2, l1, trt),
       env = list(l2 = l2, l1 = l1, trt = trt)] %>% 
    addmargins(margin = c(2, 3), FUN = list(`_n` = sum, OVERALL = sum)) %>% 
    as.data.table()
  
  if (is.null(N)) {
    out1[, `:=`(
      VAL = N,
      CI = ''
    )]
  } else if (is.table(N) | (is.vector(N) & ! is.character(N)) | all(N == 'n')) {
    if( is.table(N) | (is.vector(N) & ! is.character(N)) ){
      temp = as.data.table(N, keep.rownames = T)
      names(temp) = c('TRT', 'N')
      
      db.n =
        rbind(
          temp,
          temp[, .(TRT = 'OVERALL', N = sum(N))]
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
           PCT = 
             (BinomCI(N, N[l1 %in% '_n'], method = ci.method, conf.level = conf.level)[, 'est'] * 100) %>% 
             Format(digits = 1) %>% as.character(),
           CI.LL = 
             (BinomCI(N, N[l1 %in% '_n'], method = ci.method, conf.level = conf.level)[, 'lwr.ci'] * 100) %>% 
             Format(digits = 1) %>% as.character(),
           CI.UL = 
             (BinomCI(N, N[l1 %in% '_n'], method = ci.method, conf.level = conf.level)[, 'upr.ci'] * 100) %>% 
             Format(digits = 1) %>% as.character()
         ),
         by = .(l2, trt),
         env = list(l2 = l2, l1 = l1, trt = trt)]
    
    out1[, `:=`(
      VAL = fcase(
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
    
  }
  
  out2 =
    reshape(out1[order(factor(trt, levels = c(lvl.trt, 'OVERALL'))),
                 ! c('PCT', 'CI.LL', 'CI.UL'),
                 env = list(trt = trt)],
            direction = 'long', idvar = c(l2, l1, trt), timevar = 'SEQ',
            varying = list(c('N', 'VAL', 'CI')),
            v.names = 'VAL') %>% 
    reshape(direction = 'wide', idvar = c(l2, l1, 'SEQ'), timevar = trt,
            v.names = 'VAL',
            varying = list(c(lvl.trt, 'OVERALL'))) %>% 
    suppressWarnings()
  
  out2[, `:=`(
    STAT = fcase(
      l1 %in% '_n', 'n',
      SEQ %in% 1, 'n',
      SEQ %in% 2, 'n (%)',
      SEQ %in% 3, 'CI',
      default = ''
    ) 
  ),
  env = list(l1 = l1)]
  
  # Add DIFF and corresponding CI
  if (! missing(diffci.method)) {
    
    temp = 
      out2[SEQ %in% 1
      ][,
        (lvl.trt) := lapply(.SD, as.numeric),
        .SDcols = lvl.trt]
    
    
    for (i in lvl.trt[! lvl.trt %in% diff.reff]) {
      temp[, paste0(i1, '.DIFF') := 
             (BinomDiffCI(x1 = i, n1 = i[l1 %in% '_n'], x2 = diff.reff, n2 = diff.reff[l1 %in% '_n'],
                          conf.level = conf.level)[, 'est'] * 100) %>% 
             Format(digits = 1) %>% as.character(), 
           by = .(l2),
           env = list(l2 = l2, l1 = l1, i = i, i1 = I(i), diff.reff = diff.reff)]
      
      temp[, paste0(i1, '.DIFFCI') := 
             paste0(
               (BinomDiffCI(x1 = i, n1 = i[l1 %in% '_n'], x2 = diff.reff, n2 = diff.reff[l1 %in% '_n'],
                            conf.level = conf.level)[, 'lwr.ci'] * 100) %>% 
                 Format(digits = 2) %>% as.character(),
               ', ',
               (BinomDiffCI(x1 = i, n1 = i[l1 %in% '_n'], x2 = diff.reff, n2 = diff.reff[l1 %in% '_n'],
                            conf.level = conf.level)[, 'upr.ci'] * 100) %>% 
                 Format(digits = 2) %>% as.character()
             ), 
           by = .(l2),
           env = list(l2 = l2, l1 = l1, i = i, i1 = I(i), diff.reff = diff.reff)]
    }
    
    lvl.trt1 = lvl.trt[! lvl.trt %in% diff.reff]
    
    var.temp = c(l2, l1, paste0(lvl.trt1, '.DIFF'))
    
    temp1 =
      temp[! l1 %in% '_n', 
           ..var.temp,
           env = list(l1 = l1)]
    
    temp1[, `:=`(
      SEQ = 4,
      STAT = 'DIFF'
    )]
    
    names(temp1)[3:4] = lvl.trt1
    
    var.temp = c(l2, l1, paste0(lvl.trt1, '.DIFFCI'))
    
    temp2 =
      temp[! l1 %in% '_n', 
           ..var.temp,
           env = list(l1 = l1)]
    
    temp2[, `:=`(
      SEQ = 5,
      STAT = 'DIFFCI'
    )]
    
    names(temp2)[3:4] = lvl.trt1
    
    out2 =
      rbindlist(
        list(
          out2,
          temp1, 
          temp2
        ),
        fill = T, use.names = T
      )
  }
  
  # organize final output #
  out.var = c(l2, l1, 'STAT', lvl.trt, 'OVERALL')
  
  out2[, `:=`(
    l1 = factor(l1, levels = c('_n', lvl.l1)),
    l2 = factor(l2, levels = lvl.l2)
  ),
  env = list(l1 = l1, l2 = l2)]
  
  out =
    out2[order(l2, l1, SEQ),
         env = list(l2 = l2, l1 = l1)
    ][! (l1 %in% '_n' & SEQ %in% 3),
      env = list(l1 = l1)]
  
  if (is.null(N) & isFALSE(ci) & missing(diffci.method) ) {
    return(
      out[SEQ %in% 1,
          ..out.var]
    ) } else if (! is.null(N) & isFALSE(ci) & missing(diffci.method)) {
      return(
        out[SEQ %in% 2,
            ..out.var]
      )
    } else if (isTRUE(ci) & missing(diffci.method)) {
      return(
        out[SEQ %in% c(2, 3), 
            ..out.var]
      )
    } else if (isFALSE(ci) & ! missing(diffci.method)) {
      return(
        out[SEQ %in% c(2, 4, 5), 
            ..out.var]
      )  
    } else if (isTRUE(ci) & ! missing(diffci.method)) {
      return(
        out[SEQ %in% c(2, 3, 4, 5), 
            ..out.var]
      )
    }
  
}

