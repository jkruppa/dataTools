##' Small wrapper for dir.create
##'
##' Small wrapper for dir.create
##' @title Small wrapper for dir.create 
##' @param dir_path 
##' @return NULL
##' @author Jochen Kruppa
##' @export
mk_dir <- function(dir_path){
  if(!file.exists(dir_path)) dir.create(dir_path, recursive = TRUE)
}


##' Small function to return list to DNAStringSet
##'
##' Small function to return list to DNAStringSet
##' @title Small function to return list to DNAStringSet 
##' @param list 
##' @return DNAStringSet
##' @author Jochen Kruppa
##' @export
list2Biostring <- function(list){
  return(DNAStringSet(Reduce(c, list)))
}


##' Alias for glimpse
##'
##' Alias for glimpse
##' @title Alias for glimpse
##' @param ... 
##' @return NULL
##' @author Jochen Kruppa
##' @export
gli <- function(...) {
  require(dplyr)
  glimpse(...) 
}

##' Load named fasta file
##'
##' You get the same result with readDNAStringSet, but this function
##' is used in some older projects and can not be dismissed
##' @title Load named fasta file
##' @param file file to be loaded
##' @return DNAStringSet
##' @author Jochen Kruppa
##' @export
loadNamedFasta <- function(file) {
  require(ShortRead)
  tmp <- readFasta(file)
  tmpDNAStringSet <- sread(tmp)
  names(tmpDNAStringSet) <- id(tmp)
  return(tmpDNAStringSet)
}

##' Time function
##'
##' Time function
##' @title Time function 
##' @param ... 
##' @return Time in specified format
##' @author Jochen Kruppa
TIME <- function(...) format(Sys.time(), "%b %d %X")

##' Message function with time
##'
##' Message function with time
##' @title Message function with time 
##' @param ... 
##' @return Message with time
##' @author Jochen Kruppa
##' @export
talk <- function(...) {
  require(stringr)
  message(str_c(TIME(), " ..... "), ...)
}

##' Lazy alias for dir() function
##'
##' Lazy alias for dir() function
##' @title Lazy alias for dir() function
##' @param ... 
##' @return file list
##' @author Jochen Kruppa
##' @export
dir0 <- function(...) dir(..., full.names = TRUE, recursive = TRUE)

##' Check for named vector
##'
##' Check for named vector
##' @title Check for named vector 
##' @param x  
##' @return Check for named vector
##' @author Jochen Kruppa
##' @export
is.named <- function(file){
    if(is.null(names(file))) {stop("Named 'file' is needed")}
}

##' Lazy wrapper for system
##'
##' Lazy wrapper for system
##' @title Lazy wrapper for system
##' @param cmd pasted command to run
##' @return NULL
##' @author Jochen Kruppa
##' @export
runCMD <- function(cmd, intern = FALSE) {
    try(system(cmd, wait = TRUE, intern = intern))
}
