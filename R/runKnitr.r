##' Compiler for RNW and TEX files in subdirs
##'
##' Compiler for RNW and TEX files in subdirs
##' @title Compiler for RNW and TEX files in subdirs
##' @param files RNW or TEX files
##' @param type determines the type of files. Default NULL self extracting
##' @param copy.dir where should the final pdfs copy to?
##' @return NULL
##' @author Jochen Kruppa
##' @export
runKnitr <- function(files, type = NULL, copy.dir = NULL, force = FALSE){
    require(plyr)
    require(knitr)
    ## some options...
    runwd <- getwd()
    ## automatically determine the type... could be buggy...
    if(is.null(type))
        type <- unique(gsub(".*.(\\w{3})$", "\\1", files))
    switch(type,
           "Rnw" = {
               l_ply(files, function(file) {
                   message("+------------------------------------------------------+")
                   message(paste0("| Running on file: ... ", basename(file)))
                   message("+------------------------------------------------------+")
                   ## knitr the file
                   setwd(dirname(file))
                   knit(basename(file), quiet = TRUE)
                   ## tex the file
                   texFileName <- gsub("Rnw", "tex", basename(file))
                   if(force){
                       system2("latexmk", args = c("-pdf", "-g", "-quiet", texFileName),
                               wait = TRUE, stdout = NULL)
                   } else { 
                       system2("latexmk", args = c("-pdf", "-quiet", texFileName),
                               wait = TRUE, stdout = NULL)
                   }
                   ## return to upper dir
                   setwd(runwd)
                   message("+------------------------------------------------------+")    
                   message("Finished")    
                   message("+------------------------------------------------------+\n")
               })
           },
           "tex" = {
               l_ply(files, function(file) {
                   message("+------------------------------------------------------+")
                   message(paste0("| Running on file: ... ", basename(file)))
                   message("+------------------------------------------------------+")
                   setwd(dirname(file))
                   ## tex the file
                   system2("latexmk", args = c("-pdf", basename(file)), wait = TRUE, stdout = NULL)
                   ## return to upper dir
                   setwd(runwd)
                   message("+------------------------------------------------------+")    
                   message("Finished")    
                   message("+------------------------------------------------------+\n")    
               })
           })
    if(!is.null(copy.dir)){
        pdfFiles <- gsub("\\w{3}$", "pdf", files)
        l_ply(pdfFiles, function(file) file.copy(file, copy.dir, overwrite = TRUE))
    }
}


