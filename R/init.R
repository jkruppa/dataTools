##' init project function
##'
##' init project function
##' @title init project function
##' @param ... 
##' @param init_path give the path to the init file 
##' @return NULL
##' @author Jochen Kruppa
##' @export
init <- function(..., init_path = NULL) {
  require(pacman)
  par$packages <- switch(Sys.info()['sysname'],
                         "Windows" = {
                           grep("doMC|foreach", par$packages,
                                invert = TRUE, value = TRUE)
                         },
                         "Linux" = {
                           par$packages
                         })
  talk("Load demanded packages: ", paste0(par$packages, collapse = " "))
  p_load(char = par$packages, character.only = TRUE)
  talk("Set max.print to ", par$max.print)
  options(max.print = par$max.print)
  talk("Register ", par$nCores, " CPU cores. Change with 'registerDoMC()'")  
  if(Sys.info()['sysname'] != "Windows")
    registerDoMC(par$nCores)
  if(is.null(init_path)){
    if(file.exists("init.R")){
      talk("Source file paths from 'init.R'")
      source("init.R")
    }
  } else {
    if(file.exists(init_path)){
      talk("Source file paths from ", init_path)
      source(init_path)
    }
  }
}
