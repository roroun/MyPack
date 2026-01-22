# list.packages = 
#   c('data.table', 'dplyr', 'rio')
# 
# # Check and install the missing packages
# new.packages =
#   list.packages[! list.packages %in% installed.packages()[, "Package"]]
# 
# if(length(new.packages)) {
#   install.packages(new.packages)
# }
# # Check and load the missing packages
# load.packages =
#   list.packages[! list.packages %in% .packages()]
# 
# if(length(load.packages)) {
#   invisible(lapply(load.packages, library, character.only = TRUE))
# }


# Function for SDTM merging ----
#' Merge SDTM Main and SUPP Domains
#'
#' @param sdtm A SDTM main domain
#' @param supp A SDTM SUPP domain
#'
#' @return A merged data.table with all variables from the main domain as well as the variables from all QNAM values 
#' @export
#'

merge.sdtm =
  function(sdtm, supp){
    
    sdtm = as.data.table(sdtm)
    supp = as.data.table(supp)
    
    supp.wide =
      supp[, .(USUBJID, IDVARVAL = as.numeric(IDVARVAL), QNAM, QVAL)]  %>% 
      arrange(QNAM) %>% 
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

