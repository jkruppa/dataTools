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
