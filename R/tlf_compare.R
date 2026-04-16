# TEST Output ----
# env = 'VST'
# path.new = 'C:/Users/ralf.wu/OneDrive - Veristat, LLC/Documents/myrepo/TestOutput'
# path.old = paste(path.new, 'Previous', sep = '/')
# path.out = path.new

# FUNC ----
#' TLF Output Comparison
#'
#' @param env The environment where the outputs are generated. Default: VST
#' @param path.new The full directory path where the new outputs are located.
#' @param path.old The full directory path where the old outputs are located.
#' @param path.out The full directory path for saving the list of changed outputs.
#'
#' @return A PDF showing outputs that differ between versions
#' @export 


rw.tlf.compare = 
  function(env = 'VST', path.new, path.old, path.out) {
    
    . = VAL = TLF = NULL   # No visible binding for global variable
    
    # Check arguments #
    env = toupper(env)
    if (any(! env %in% c('VST')))
      stop("'env' must be 'VST'.")
    
    # Identify the common TLFs for comparison
    name.tlf.new =
      list.files(path.new, 
                 pattern = '.rtf$|.pdf$')
    
    name.tlf.old =
      list.files(path.old, 
                 pattern = '.rtf$|.pdf$')
    
    name.tlf.common = 
      intersect(name.tlf.new, name.tlf.old)
    
    # Import common TLFs
    path.tlf.new =
      paste(
        path.new,
        name.tlf.common,
        sep = '/'
      )
    
    path.tlf.old =
      paste(
        path.old,
        name.tlf.common,
        sep = '/'
      )  
    
    tlf.in = function(path) {
      
      temp.tlf =
        lapply(path, function(path) {
          
          if (stringr::str_detect(path, '.rtf$')) {
            tlf.raw = 
              readLines(path)
          } else {
            tlf.raw = 
              pdftools::pdf_text(path)
          }
          
          if (env %in% 'VST') {
            # Step 1
            temp1 =
              stringr::str_subset(tlf.raw,
                                  paste('f\\d\\{',    # keep the cell elements 
                                        '^\\w',    # keep the elements staring with a word character (for figure output)
                                        '^\\[',    # keep the elements starting with [ due to the line break in the text
                                        '^\\(',    # keep the elements starting with ( due to the line break in the text
                                        sep = '|'))
            
            temp2 = 
              stringr::str_subset(temp1,
                                  paste('f\\d\\{\\\\cell\\}',    # remove empty cell elements
                                        'f\\d\\{\\\\~\\\\cell\\}',    # remove empty cell elements
                                        sep = '|'),    
                                  negate = T)
            
            # Step 2
            if (stringr::str_detect(path, '.rtf$')) {
              temp3 =
                stringr::str_extract(temp2, 
                                     paste('(?<=f\\d\\{).*(?=\\\\cell\\})',    # extract the text within cells,
                                           '(?<=f\\d\\{).*(?=\\\\line\\})',    # extract the text within cells, considering the line break 
                                           '^\\w+',     # keep the elements staring with a word character (for figure output)
                                           '^\\[.*',    # keep the elements starting with [ due to the line break in the text
                                           '^\\(.*',    # keep the elements starting with ( due to the line break in the text
                                           sep = '|'))  
            } else temp3 = temp2    # skip extraction above for the PDF output
            
            # Step 3
            temp4 =
              stringr::str_replace_all(temp3, 
                                       paste('\\{\\\\field.+\\}\\}\\}',    # remove field elements
                                             '(\\\\\\w+)',     # remove word controls 
                                             '\\n',     # remove new line symbol
                                             '\\{',     # remove special symbol 
                                             '\\}',     # remove special symbol 
                                             '\\_+',    # remove special symbol 
                                             sep = '|'),
                                       '') 
            
            temp5 =
              stringr::str_replace_all(temp4, 
                                       paste('Program Path:.*',    # remove the program path cell
                                             'PROGRAM PATH:.*',
                                             'Program Name:.*',    # remove the program name cell
                                             'PROGRAM NAME:.*',
                                             '\\d{2}[:alpha:]{3}\\d{4}:\\d{2}:\\d{2}:\\d{2}',    # remove the program running time cell
                                             sep = '|'),
                                       '') 
            
            # Step 4
            temp6 =
              stringr::str_replace_all(temp5,
                                       '\\s',    # remove space 
                                       '')
            
            temp7 =
              stringr::str_subset(temp6, ".+")    # Keep non-missing elements 
          }
          # output
          return(temp7)
        }
        )
      
      names(temp.tlf) = stringr::str_replace(name.tlf.common, '.rtf$|.pdf$', '')
      
      return(temp.tlf)
    }
    
    tlf.new =
      tlf.in(path.tlf.new)
    
    tlf.old =
      tlf.in(path.tlf.old)
    
    # compare
    tlf.compare =
      mapply(
        function(old, new) {
          all.equal(old, new)
        },
        tlf.old,
        tlf.new
      ) 
    
    if(is.list(tlf.compare)) {
      tlf.compare1 =
        tlf.compare %>% 
        data.table::transpose(keep.names = 'TLF') %>% 
        data.table::as.data.table()
    } else if(is.matrix(tlf.compare)) {
      tlf.compare1 =
        tlf.compare %>% 
        t() %>% 
        data.table::as.data.table(keep.rownames = T)
    } else {
      tlf.compare1 = 
        tlf.compare %>% as.data.table(keep.rownames = T)
    }
    
    names(tlf.compare1)[1:2] = c('TLF', 'VAL')
    
    tlf.out =
      tlf.compare1[! VAL %in% 'TRUE',
                   .(TLF)]
    
    # Create PDF outputs  
    temp = 
      c(tlf.out$TLF, 
        rep('', times = (3 - nrow(tlf.out) %% 3))
      )
    
    output = 
      data.table::data.table(matrix(temp, ncol = 3, byrow = F))
    
    row.max = 35
    
    npages = ceiling(nrow(output) / row.max)
    
    pdf.out = paste0(path.out, '/RTFCompare.pdf')
    
    grDevices::pdf(pdf.out, height = 11, width = 8.5)
    
    for (i in 1:npages) {
      
      grid::grid.newpage()
      
      grid::grid.text('Difference', x = 0.5, y = 0.97)
      
      if(npages == 1){
        idx = seq(1, nrow(output))
      } else if (i * row.max <= nrow(output)){
        idx = seq(1+(i-1)*row.max, i*row.max)
      } else {
        idx = seq(1+(i-1)*row.max, nrow(output))
      }
      
      gridExtra::grid.table(output[idx,], cols = NULL)
      
    }
    
    grDevices::dev.off()
    
    return(
      output
    )
  }


# TEST Run ----
# rw.tlf.compare(
#   env = 'VST',
#   path.new = path.new,
#   path.old = path.old,
#   path.out = path.out
# )






