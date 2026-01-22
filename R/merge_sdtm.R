# Function for SDTM merging ----
#' Merge SDTM Main and SUPP Domains
#'
#' @param sdtm A SDTM main domain
#' @param supp A SDTM SUPP domain
#'
#' @return A merged data.table with all variables from the main domain as well as the variables from all QNAM values 
#' @export
#'

rw.merge.sdtm =
  function(sdtm, supp){
    
    sdtm = data.table::as.data.table(sdtm)
    supp = data.table::as.data.table(supp)
    
    supp.wide =
      supp[, .(USUBJID, IDVARVAL = as.numeric(IDVARVAL), QNAM, QVAL)]  %>% 
      dplyr::arrange(QNAM) %>% 
      reshape(direction = 'wide', idvar = c('USUBJID', 'IDVARVAL'), timevar = 'QNAM',
              v.names = 'QVAL',
              varying = list(supp[order(QNAM), unique(QNAM)]))
    
    names(supp.wide)[2] = unique(supp$IDVAR) 
    
    out =
      merge(
        sdtm,
        supp.wide,
        by = names(supp.wide)[1:2],
        all.x = T
      )
    
    return(out)
  }

