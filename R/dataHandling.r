#' dataTools.
#'
#' @name dataTools
#' @docType package
NULL

##' Small wrapper for glimpse()
##'
##' Small wrapper for glimpse()
##' @title Small wrapper for glimpse()
##' @param ... 
##' @return glimpse()
##' @author Jochen Kruppa
gli <- function(...) glimpse(...) 
    
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

