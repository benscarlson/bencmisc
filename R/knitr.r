#' Escapes latex special characters.
#' Add additional to this function as needed.
#'
#' @export
#'
escapeForLatex <- function(str) {
  #Note, don't need to escape dash symbol '-'
  result <- str
  result <- gsub("&", "\\&", result, fixed = TRUE)
  result <- gsub("_", "\\_", result, fixed = TRUE)

  return(result)
}

#this is a general function to run a report and copy the pdf to a specified directory
#' @export
reportRunCopy <- function(reportPF,reportOutPF,params=NULL) {
  require(glue)
  require(knitr)
  require(tools)

  reportP <- dirname(reportPF)
  reportName <- sub('\\.rnw$','', basename(reportPF), ignore.case=TRUE)

  #parameters used internally in the report
  .p <- params

  #note: this will set the wd to the directory of the report
  message('Starting report generation...')
  wd <- getwd()

  #TODO: do this all within trycatch so that I can set wd back if process fails
  setwd(reportP) #have to setwd for knit to work correctly
  knitr::knit2pdf(
    input=file.path(reportP,glue('{reportName}.rnw')),
    output=file.path(reportP,glue('{reportName}.tex')),
    quiet=TRUE
  )

  dir.create(dirname(reportOutPF),recursive=TRUE,showWarnings=FALSE)

  copied <- file.copy(file.path(reportP,glue('{reportName}.pdf')), reportOutPF, overwrite=TRUE)

  if(copied) {
    message(glue('Report copied successfully to {reportOutPF}'))
  } else {
    message('Report failed to copy')
  }

  setwd(wd)
  return(copied)
}
