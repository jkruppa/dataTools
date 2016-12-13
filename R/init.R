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
  talk("Load demanded packages")

  par$packages <- switch(Sys.info()['sysname'],
                         "Windows" = {
                           grep("doMC|foreach", par$packages,
                                invert = TRUE, value = TRUE)
                         })
  p_load(char = par$packages, character.only = TRUE)
  talk("Set max.print to ", par$max.print)
  options(max.print = par$max.print)
  talk("Register ", par$nCores, " CPU cores. Change with 'registerDoMC()'")  
  if(Sys.info()['sysname'] != "Windows")
    registerDoMC(par$nCores)
}
