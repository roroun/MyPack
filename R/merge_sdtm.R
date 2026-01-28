# Function for SDTM merging ----
#' Merge SDTM Main and SUPP Domains
#'
#' @param sdtm A SDTM main domain
#' @param supp A SDTM SUPP domain
#'
#' @return A merged data.table with all variables from the main domain as well as the variables from all QNAM values 
#'  
#' @export

rw.merge.sdtm =
  function(sdtm, supp){
    # Address check note: undefined global functions or variables
    USUBJID = IDVARVAL = QNAM = QVAL = NULL   # No visible binding for global variable
    `.` = list()

    sdtm = data.table::as.data.table(sdtm)
    supp = data.table::as.data.table(supp)
    idvar = unique(supp$IDVAR)
    
    if (! idvar %in% '') {
      reshape.idvar = c('USUBJID', 'IDVARVAL')
    } else reshape.idvar = c('USUBJID')
    
    supp.wide =
      supp[order(QNAM), 
           .(USUBJID, IDVARVAL = as.numeric(IDVARVAL), QNAM, QVAL)]  %>% 
      stats::reshape(direction = 'wide', idvar = reshape.idvar, timevar = 'QNAM',
                     v.names = 'QVAL',
                     varying = list(supp[order(QNAM), unique(QNAM)]))
    
    if (! idvar %in% '') {
      names(supp.wide)[2] = unique(supp$IDVAR) 
    } 
    
    out =
      merge(
        sdtm,
        supp.wide,
        by = names(supp.wide)[1:length(reshape.idvar)],
        all.x = T
      )
    
    return(out)
  }

