# SETTING ----
list.packages = 
  c('dplyr', 'stringr', 'pdftools', 'diffr', 'doconv')

invisible(lapply(list.packages, library, character.only = TRUE))

# TEST Output ----
path.f1 = 'C:/Users/ralf.wu/OneDrive - Veristat, LLC/Documents/myrepo/TestOutput/rt-ae-teae.pdf'
path.f2 = 'C:/Users/ralf.wu/OneDrive - Veristat, LLC/Documents/myrepo/TestOutput/Previous/rt-ae-teae.pdf'


# FUNC ----
rw.tlf.diff = 
  function(path.f1, path.f2, before = path.f1, after = path.f2) {
    
    
    # Check1: if the type is pdf or rtf.  
    Check1 =
      lapply(list(path.f1, path.f2), 
             FUN = function(x) str_ends(x, pattern = 'rtf|pdf')) %>% 
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
      if (str_ends(x, pattern = 'pdf')) {
        pdf_text(x) %>% paste(collapse = '\n')
      } else {
        docx2pdf(x)
        
        str_replace(x, pattern = '.rtf', replacement = '.pdf') %>%
          pdf_text %>% paste(collapse = '\n')
      }
    }
    
    tlf.in =
      lapply(list(path.f1, path.f2), FUN = input)
    
    # Show the difference
    x = list(message = message, contextSize = 0, minJumpSize = 10, wordWrap = TRUE, 
             file1 = before, file2 = after, 
             f1 = tlf.in[[1]], f2 = tlf.in[[2]])
    
    htmlwidgets::createWidget(name = "diffr", x, width = NULL, 
                              height = NULL, package = "diffr")
    
    # Here
    to remove the converted PDF file
    
    
    
    
    
  }

rw.tlf.diff(path.f1, path.f2)


# TEST RUN ----

