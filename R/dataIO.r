##' Small wrapper for write.table
##'
##' Small wrapper for write.table with the options col.names = FALSE, row.names = FALSE, quote = FALSE
##' @title Small wrapper for write.table
##' @param ... 
##' @return NULL
##' @author Jochen Kruppa
##' @export
write.table0 <- function(...){
    write.table(..., col.names = FALSE, row.names = FALSE, quote = FALSE)
}


##' A writeRDS wrapper for more convienence
##'
##' Caution this is a wrapper...
##' @title SaveRDS wrapper
##' @param object which should be saved
##' @param dir where should the object be saved
##' @param verbose should the function talk?
##' @return NULL
##' @author Jochen Kruppa
##' @export
writeRDS <- function(object, dir, verbose = FALSE){
    objectName <- deparse(substitute(object))
    dirName <- deparse(substitute(dir))
    file <- file.path(dir, paste0(objectName, ".RDS"))
    filePath <- paste0(paste0(objectName, "File"), " <- file.path(", dirName, ", '", objectName, ".RDS')")
    if(verbose){
        cat("Write", objectName, "to", dirName, "\n")
        cat("'", filePath, "'... will be added to the init.R\n")
    }    
    write_flag <- suppressWarnings(as.numeric(try(system(paste0("grep -c '", paste0(objectName, "File"), "' init.R"), intern  = TRUE))))
    if(write_flag)
        warning("File already exists in init.R. Overwrite.\n")
    else
        write(filePath, "init.R", append = TRUE)
    saveRDS(object, file = file)
}

##' Get a object back
##'
##' Get a object back, which was saved as a RDS 
##' @title ReadRDS wrapper
##' @param object a desired object name
##' @return the object
##' @author Jochen Kruppa
##' @export
getObject <- function(object){
    source("init.R")
    objectName <- deparse(substitute(object))
    return(readRDS(eval(parse(text=paste0(objectName, "File")))))
}


##' A wrapper how saves a list to a xls file
##'
##' A wrapper how saves a list to a xls file
##' @title A wrapper how saves a list to a xls file
##' @param list The data frames in a list which should be written
##' @param file Save location
##' @param oneFile One file with sheets or many files?
##' @return NULL
##' @author Jochen Kruppa
##' @export
saveList2Xls <- function(list, file, oneFile = TRUE){
    require(openxlsx)
    wb <- createWorkbook()
    if (is.null(names(list))) 
        stop("The list must be named by: names(list) <- c('nameList1',...)")
    if (oneFile) {
        for (i in seq_along(names(list))) {
            addWorksheet(wb, names(list)[i])
            writeData(wb, sheet = i, x = list[[i]], colNames = TRUE)
        }
        saveWorkbook(wb, file, overwrite = TRUE)
    } else {
        if(length(file) != length(list)) stop ("If you do not want 'oneFile' provide number of files equal list length")
        for (i in seq_along(names(list))) {
            wb <- createWorkbook()
            addWorksheet(wb, "Results")
            writeData(wb, sheet = 1, x = list[[i]], colNames = TRUE)
            saveWorkbook(wb, file[i], overwrite = TRUE)
        }
    }
}

#' This is a link to a older function
#'
#' @param link to saveList2Xls
#' @export
writeXLS <- saveList2Xls

