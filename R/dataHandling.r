#' serviceTools.
#'
#' @name serviceTools
#' @docType package
NULL

##' A saveRDS wrapper for more convienence
##'
##' Caution this is a wrapper...
##' @title SaveRDS wrapper
##' @param object which should be saved
##' @param dir where should the object be saved
##' @param verbose should the function talk?
##' @return NULL
##' @author Jochen Kruppa
##' @export
writeRDS <- function(object, dir, verbose = TRUE){
    objectName <- deparse(substitute(object))
    dirName <- deparse(substitute(dir))
    file <- file.path(dir, paste0(objectName, ".RDS"))
    filePath <- paste0(paste0(objectName, "File"), " <- file.path(", dirName, ", '", objectName, ".RDS')")
    if(verbose){
        cat("Write", objectName, "to", dirName, "\n")
        cat("'", filePath, "'... will be added to the init.R\n")
    }    
    write_flag <- as.numeric(try(system(paste0("grep -c '", paste0(objectName, "File"), "' init.R"), intern  = TRUE)))
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
    
##' A p value rounder
##'
##' A p value rounder
##' @title A p value rounder
##' @param x a vector
##' @return a vector
##' @author Jochen Kruppa
##' @export
round0 <- function(x){
    xRound <- round(x,3)
    ifelse(xRound < 0.001, "<0.001", xRound)
}

#' This is a link to a older function
#'
#' @param link to saveList2Xls
#' @export
writeXLS <- saveList2Xls


##' Paste factores
##'
##' Paste factores
##' @title Paste factores
##' @param x Factor x
##' @param y Factor y
##' @return Factor
##' @author Jochen Kruppa
##' @export
cFactor <- function (x, y) {
    newlevels <- union(levels(x), levels(y))
    m <- match(levels(y), newlevels)
    ans <- c(unclass(x), m[unclass(y)])
    levels(ans) <- newlevels
    class(ans) <- "factor"
    return(ans)
}

##' Split data into chunks
##'
##' Split data into chunks
##' @title Split data into chunks
##' @param x Vector
##' @param size Size of chunks
##' @return List
##' @author Jochen Kruppa
##' @examples
##' d <- rpois(73, 5)
##' chunks(d, size = 10)
##' @export
chunks <- function(x, size){
    return(split(x, ceiling(seq_along(x)/size)))
}

##' Get the modal value of vector
##'
##' Get the modal value of vector
##' @title Get the modal value of vector
##' @param vector 
##' @return modal value
##' @author Jochen Kruppa
##' @export
modal <- function(vector) as.numeric(names(which.max(table(vector))))

