# TEST DB ----
# path.f1 = 'C:/Users/ralf.wu/OneDrive - Veristat, LLC/Documents/myrepo/TestOutput/rt-dm-saf.rtf'
# path.f2 = 'C:/Users/ralf.wu/OneDrive - Veristat, LLC/Documents/myrepo/TestOutput/Previous/rt-dm-saf.rtf'

# FUNC ----
#' Display difference between two files side by side
#'
#' @param path.f1 The full directory path of File 1
#' @param path.f2 The full directory path of File 2
#' @param before Text to display on File 1
#' @param after Text to display on File 2
#'
#' @return Side-by-side display of differences between two files 
#' @export
#'

rw.tlf.diff = 
  function(path.f1, path.f2, before = path.f1, after = path.f2) {
    
    
    # Check1: if the type is pdf or rtf.  
    Check1 =
      lapply(list(path.f1, path.f2), 
             FUN = function(x) stringr::str_ends(x, pattern = 'rtf|pdf')) %>% 
      unlist() 
    
    Check1.pos =
      which(Check1 %in% FALSE)
    
    if ( length(Check1.pos) > 0 ) {
      msg = 
        paste0(
          paste0('path.f', Check1.pos, collapse = ' and '),
          ' must be PDF or RTF.'
        )
      stop(msg)
    }
    
    # File input
    input = function(x) {
      if (stringr::str_ends(x, pattern = 'pdf')) {
        out =
          pdftools::pdf_text(x) %>% 
          paste(collapse = '\n')
      } else {
        doconv::docx2pdf(x)
        
        x.pdf = 
          stringr::str_replace(x, pattern = '.rtf', replacement = '.pdf')
        
        out = 
          pdftools::pdf_text(x.pdf) %>% 
          paste(collapse = '\n')
        
        file.remove(x.pdf)
      }
      return(out)
    }
    
    tlf.in =
      lapply(list(path.f1, path.f2), FUN = input)
    
    # Show the difference
    x = list(message = message, contextSize = 0, minJumpSize = 10, wordWrap = TRUE, 
             file1 = before, file2 = after, 
             f1 = tlf.in[[1]], f2 = tlf.in[[2]])
    
    htmlwidgets::createWidget(name = "diffr", x, width = NULL, 
                              height = NULL, package = "diffr")
  }

# TEST RUN ----
# rw.tlf.diff(path.f1, path.f2)
