# TEST DB ----
# dbin =
#   data.table(
#     USUBJID = paste0('P',
#                      str_pad(1:20, width = 2, side = 'left', pad = '0')) %>% 
#       rep(each = 5),
#     TRT = sample(c('TEST', 'CONTROL'), size = 20, replace = T) %>% 
#       rep(each = 5) %>% 
#       factor(levels = c('TEST', 'CONTROL')),
#     SOC = sample(c('SOC-1', 'SOC-1', 'SOC-1', 'SOC-2', 'SOC-3'), size = 100, replace = T)
#   )
# 
# dbin[, PT := sample(paste0('PT-', substr(SOC, 5, 5), rep(1:9, time = 9:1)), 
#                     size = .N, replace = T),
#      by = .(SOC)]
# 
# db = copy(dbin) %>% filter(TRT %in% 'TEST')
# subj = 'USUBJID'
# trt = 'TRT'
# l1 = 'PT'
# l2 = 'SOC'
# sort.l1 = 'freq'
# sort.l2 = 'freq'
# l1.desc = TRUE
# l2.desc = TRUE
# sort.ref = 'OVERALL'
# N = c('TEST' = 100, 'CONTROL' = 100)
# stats = 'n'
# diffci.method = 'scorecc'
# diff.ref = 'CONTROL'
# conf.level = 0.95

# FUNC  ----
#' Frequency Summary for Categorical Variables, taking unique records
#'
#' @param db A dataset, e.g., ADAE
#' @param subj A subject ID variable, e.g., USUBJID
#' @param trt A treatment variable, e.g., 'TRTA'
#' @param l1 Level 1, e.g., 'PT'
#' @param l2 Level 2, e.g., 'SOC'
#' @param N The sample size serves as the denominator in the percentage calculation; 
#' a table or named vector to specify the n for each group, e.g., c('Group A' = 100, 'Group B' = 1000)#'
#' @param stats Character. Which stats to produce. Either “n_pct” (default), or a selection of : 
#' 'n', 'n_pct', 'diff', 'diffci' 
#' @param sort.l1 The sorting method to be applied to l1: 'freq' for frequency (default); 'alphabet' for alphabetical
#' @param sort.l2 The sorting method to be applied to l2: 'freq' for frequency (default); 'alphabet' for alphabetical 
#' @param l1.desc A logical specifying whether the sorting of l1 is performed by descending (default = TRUE) or not
#' @param l2.desc A logical specifying whether the sorting of l2 is performed by descending (default = TRUE) or not
#' @param sort.ref A character string, specifying the trt group to be the reference for sorting
#' @param diffci.method A character string, specifying the method for CI of diff from BinomDiffCI;
#' e.g., 'wald' for traditional Wald; 'score' for Newcombe; 'scorecc' for Newcombe with continuity correction 
#' @param diff.ref A character string, specify the trt group to be the reference for comparison
#' @param conf.level The confidence level, defaults to 0.95
#' @return A dataset with unique frequency counts diff, and/or CI of diff
#' @export

rw.freq1 =
  function(db, subj, trt, l1, l2, N, stats = 'n',
           sort.l1 = 'freq', sort.l2 = 'freq', l1.desc = TRUE, l2.desc = TRUE, sort.ref = 'OVERALL', 
           diffci.method, diff.ref, conf.level = 0.95) {
    . = TRT = lvl.trt = lvl.l1 = lvl.l2 = i1 = ORDERL1 = ORDERL2 = STATS = OVERALL = NULL   # No visible binding for global variable
    
    db = data.table::copy(db)
    
    # Check arguments #
    if (! missing(N)) {
      if (! any(is.table(N), (is.vector(N) & ! is.null(names(N)))))
        stop("N must be a named vector or a table.")
    }
    
    stats = toupper(stats)
    if (any(! stats %in% c('N', 'N_PCT', 'DIFF', 'DIFFCI'))) 
      stop("'stats' must be 'n', 'n_pct', 'diff', 'diffci'.")
    
    if (any(stats %in% c('DIFF', 'DIFFCI')) & any(missing(diffci.method), missing(diff.ref)))
      stop("Both 'diffci.method' and 'diff.ref' must be specified as 'stats' include 'diff' or 'diffci'.")
    
    if (any(! sort.l1 %in% c('freq', 'alphabet'))) 
      stop("'sort.l1' must be 'freq', 'alphabet'.")
    
    if (any(! sort.l2 %in% c('freq', 'alphabet'))) 
      stop("'sort.l2' must be 'freq', 'alphabet'.")
    
    if (! is.logical(l1.desc) ) 
      stop("'l1.desc' must be a logical.")
    
    if (! is.logical(l2.desc) ) 
      stop("'l2.desc' must be a logical.")
    
    # create l2 if missing #
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
    
    db[, trt := factor(trt, levels = lvl.trt),
       env = list(trt = trt)]
    
    # Freq tabulation $
    freq.lvl = function(x) {
      x[, table(l2, l1, trt),
        env = list(l2 = l2, l1 = l1, trt = trt)] %>% 
        data.table::as.data.table() %>% 
        reshape(direction = 'wide', idvar = c(l2, l1), timevar = trt,
                v.names = 'N',
                varying = lvl.trt)
    }
    
    # L0 #
    t.l0.1 = 
      db[, .(subj, l2 = '_ALL', l1 = '_ALL', trt),
         env = list(subj = subj, l2 = l2, l1 = l1, trt = trt)] %>% 
      unique()
    
    t.l0.2 = freq.lvl(t.l0.1)
    
    # L1 #
    t.l1.1 = 
      db[, .(subj, l2, l1, trt),
         env = list(subj = subj, l2 = l2, l1 = l1, trt = trt)] %>% 
      unique()
    
    t.l1.2 = freq.lvl(t.l1.1)
    
    # L2 #
    t.l2.1 = 
      db[, .(subj, l2, l1 = '_ALL', trt),
         env = list(subj = subj, l2 = l2, l1 = l1, trt = trt)] %>% 
      unique()
    
    t.l2.2 = freq.lvl(t.l2.1)
    
    # Combine all levels #
    t.all =
      rbind(t.l0.2,
            t.l1.2,
            t.l2.2) %>% 
      unique()
    
    t.all[, OVERALL := sum(.SD),
          .SDcols = lvl.trt,
          by = .I]
    
    t.all = 
      t.all[, STATS := 'N'
      ][OVERALL > 0]
    
    # Sorting #
    t.all[l2 %in% '_ALL', 
          ORDERL2 := 1,
          env = list(l2 = l2)]
    
    t.all[l1 %in% '_ALL', 
          ORDERL1 := 1,
          env = list(l1 = l1)]
    
    # L2 order
    if (sort.l2 %in% 'freq') {
      if (l2.desc) {
        t.all = 
          t.all[order(ORDERL2, -sort.ref, l2),
                env = list(sort.ref = sort.ref, l2 = l2)]
      } else (
        t.all = 
          t.all[order(ORDERL2, sort.ref, l2),
                env = list(sort.ref = sort.ref, l2 = l2)]
      )
    }
    
    if (sort.l2 %in% 'alphabet') {
      if (l2.desc) {
        t.all = 
          t.all[order(ORDERL2, -l2),
                env = list(l2 = l2)]
      } else (
        t.all = 
          t.all[order(ORDERL2, l2),
                env = list(l2 = l2)]
      )
    }
    
    t.all[l1 %in% '_ALL',
          ORDERL2 := 1:.N,
          env = list(l1 = l1)]
    
    t.all[, ORDERL2 := data.table::nafill(ORDERL2, type = 'locf')  ,
          by = l2]
    
    # L1 order
    if (sort.l1 %in% 'freq') {
      if (l1.desc) {
        t.all = 
          t.all[order(ORDERL1, -sort.ref, l1),
                env = list(sort.ref = sort.ref, l1 = l1)]
      } else (
        t.all = 
          t.all[order(ORDERL1, sort.ref, l1),
                env = list(sort.ref = sort.ref, l1 = l1)]
      )
    }
    
    if (sort.l1 %in% 'alphabet') {
      if (l1.desc) {
        t.all = 
          t.all[order(ORDERL1, -l1),
                env = list(l1 = l1)]
      } else (
        t.all = 
          t.all[order(ORDERL1, l1),
                env = list(l1 = l1)]
      )
    }
    
    t.all[, ORDERL1 := 1:.N,
          by = l2,
          env = list(l2 = l2)]
    
    # Add PCT #
    if(! missing(N)) {
      db.pct = copy(t.all)
      db.pct[, STATS := 'N_PCT']
      
      n.trt = data.table::as.data.table(N, keep.rownames = 'TRT')
      
      db.n =
        rbind(
          n.trt,
          n.trt[, .(TRT = 'OVERALL', N = sum(N))]
        )
      
      for (i in db.n$TRT) {
        n = db.n[TRT %in% i, N]
        
        db.pct[, `:=`(
          i = fcase(
            i > 0,  paste0(
              i,
              ' (',
              (i / n * 100) %>% DescTools::Format(digits = 1) %>% as.character(),
              ')'
            ),
            default = '0'
          ) 
        ),
        env = list(i = i)]
      }
      
      t.all = 
        rbind(t.all, db.pct)
    }
    
    # Add DIFF and corresponding CI
    if (! missing(diffci.method)) {
      db.diff = 
        t.all[STATS %in% 'N'
        ][, (lvl.trt) := lapply(.SD, as.numeric),
          .SDcols = lvl.trt]
      
      lvl.trt1 = lvl.trt[! lvl.trt %in% diff.ref]
      for (i in lvl.trt1) {
        n1 = db.n[TRT %in% i, N]
        n2 = db.n[TRT %in% diff.ref, N]
        
        db.diff[, paste0(i1, '_DIFF') := 
                  (DescTools::BinomDiffCI(x1 = i, n1 = n1, 
                                          x2 = diff.ref, n2 = n2,
                                          conf.level = conf.level)[, 'est'] * 100) %>% 
                  DescTools::Format(digits = 1) %>% as.character(), 
                by = .(.I),
                env = list(i = i, i1 = I(i), diff.ref = diff.ref)]
        
        db.diff[, paste0(i1, '_DIFFCI') := 
                  paste0(
                    (DescTools::BinomDiffCI(x1 = i, n1 = n1, 
                                            x2 = diff.ref, n2 = n2,
                                            conf.level = conf.level)[, 'lwr.ci'] * 100) %>% 
                      DescTools::Format(digits = 2) %>% as.character(),
                    ', ',
                    (DescTools::BinomDiffCI(x1 = i, n1 = n1, 
                                            x2 = diff.ref, n2 = n2,
                                            conf.level = conf.level)[, 'upr.ci'] * 100) %>% 
                      DescTools::Format(digits = 2) %>% as.character()
                  ), 
                by = .(.I),
                env = list(i = i, i1 = I(i), diff.ref = diff.ref)]
      }
      
      db.diff1 = data.table(NULL)
      for (i in c('DIFF', 'DIFFCI')) {
        var.temp = c(l2, l1, paste(lvl.trt1, i, sep = '_'), 'ORDERL2', 'ORDERL1')
        
        temp1 =
          db.diff[, var.temp, with = F] 
        
        temp1[, `:=`(
          STATS = i
        )]
        
        names(temp1)[3:(3+length(lvl.trt1)-1)] = lvl.trt1
        
        db.diff1 = rbind(db.diff1, temp1)
      }
      
      t.all =
        data.table::rbindlist(
          list(
            t.all,
            db.diff1
          ),
          fill = T, use.names = T
        )
    }
    
    # Organize final output #
    out = 
      t.all[STATS %in% stats]
    
    out[, `:=`(
      l1 = factor(l1, levels = c('_ALL', lvl.l1)),
      l2 = factor(l2, levels = c('_ALL', lvl.l2) %>% unique()),
      STATS = factor(STATS, levels = stats)
    ),
    env = list(l1 = l1, l2 = l2)]
    
    var.lvl = mget(arg.lvl.in) %>% unlist(use.names = F)
    out.var = c(var.lvl, 'STATS', lvl.trt, 'OVERALL')
    
    out.final =
      out[order(ORDERL2, ORDERL1, STATS), 
          out.var, with = F]
    
    return(out.final)
  }
