##' init project function
##'
##' init project function
##' @title init project function
##' @param ... 
##' @return NULL
##' @author Jochen Kruppa
##' @export
init <- function(...) {
  require(pacman)
  par$packages <- switch(Sys.info()['sysname'],
                         "Windows" = {
                           grep("doMC|foreach", par$packages,
                                invert = TRUE, value = TRUE)
                         })
  talk("Load demanded packages: ", paste0(par$packages, collapse = " "))
  p_load(char = par$packages, character.only = TRUE)
  talk("Set max.print to ", par$max.print)
  options(max.print = par$max.print)
  talk("Register ", par$nCores, " CPU cores. Change with 'registerDoMC()'")  
  if(Sys.info()['sysname'] != "Windows")
    registerDoMC(par$nCores)
  if(file.exists("init.R"))
    source("init.R")
}
