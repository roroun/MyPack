# FUNC ----
#' Impute a partial date
#'
#' @param x The character string of date to be imputed 
#' @param format A character string, specifying the date formats, including 'ymd' (default), 'dmy', 'mdy'
#' @param m A numeric value used to impute the month
#' @param d A numeric value used to impute the day
#' @param date.ref The reference date, in the same date format, for comparison with the imputed date. 
#' The reference date is displayed if the available components of the partial date match 
#' the corresponding components of the reference date
#'
#' @return An imputed date
#' @importFrom lubridate ymd
#' @importFrom lubridate ceiling_date
#' @importFrom lubridate rollback
#' 
#' @export

rw.date = function(x, format = 'ymd', m, d, date.ref) {
  # Check arguments #
  if (any(! format %in% c('ymd', 'dmy', 'mdy'))) 
    stop("'format' must be 'ymd', 'dmy', 'mdy'.")
  
  # Initial date conversion #
  if (format %in% 'ymd') {
    x.date0 = lubridate::ymd(x, quiet = T)
    x.date1 = lubridate::ymd(x, truncated = 1, quiet = T)
    x.date2 = lubridate::ymd(x, truncated = 2, quiet = T)
    
    if (! missing(date.ref)) date.ref = lubridate::ymd(date.ref)
  }
  
  if (format %in% 'dmy') {
    x.date0 = lubridate::dmy(x, quiet = T)
    x.date1 = lubridate::my(x, quiet = T)
    x.date2 = lubridate::ymd(x, truncated = 2, quiet = T)
    
    if (! missing(date.ref)) date.ref = lubridate::dmy(date.ref)
  }
  
  if (format %in% 'mdy') {
    if (nchar(x) >= 8) {
      x.date0 = lubridate::mdy(x, quiet = T)
    } else x.date0 = NA
    
    x.date1 = lubridate::my(x, quiet = T)
    x.date2 = lubridate::ymd(x, truncated = 2, quiet = T)
    
    if (! missing(date.ref)) date.ref = lubridate::mdy(date.ref)
  }
  
  if (missing(date.ref)) date.ref = lubridate::ymd('1970-01-01')
  
  # Impute the partial dates #
  if (! is.na(x.date0)) {
    # Case: complete date #
    
    out.date = x.date0
    
  } else if (! is.na(x.date1)) {
    # Case: day is missing #
    
    last.date = 
      lubridate::ceiling_date(x.date1, unit = 'month') %>% 
      lubridate::rollback()
    
    lubridate::day(x.date1) = d
    
    # Use the ref date if known parts are the same as those of the ref date 
    if (lubridate::year(last.date) == lubridate::year(date.ref) & 
        lubridate::month(last.date) == lubridate::month(date.ref)) {
      out.date = date.ref
    } else 
      out.date = min(x.date1, last.date)  # use the last day in case the imputed date > last date
    
  } else if (! is.na(x.date2)) {
    # Case: month and day are missing #
    
    lubridate::day(x.date2) = d
    lubridate::month(x.date2) = m
    
    # Use the ref date if known parts are the same as those of the ref date 
    if (lubridate::year(x.date2) == lubridate::year(date.ref)) {
      out.date = date.ref
    } else 
      out.date = x.date2
    
  } else out.date = NA
  
  return(out.date)
}

