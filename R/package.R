##' Small wrapper function
##'
##' Small wrapper function
##' @title R internal function to build and compile packages
##' @param packageName Name of the package
##' @param method 'build' or 'compile' [default]
##' @param version 'minor' [default] or 'major', the version number
##'   will be incremented at the last position (minor) or second last
##'   (major).
##' @return NULL
##' @author Jochen Kruppa
##' @export
##' @examples
##' library(devtools)
##' 
##' ## Step 1: Building the package
##' 
##' package("kmerPyramid", method = "build")
##'
##' ## Step 2: Compile the package
##' 
##' package("kmerPyramid", method = "compile", version = "minor")
##'
##' ## [optional] add data to packages
##'
##' devtools::use_data(par, pkg = "dataTools", overwrite = TRUE)
package <- function(packageName, method = "compile", version = "minor"){
  require(devtools)
  require(methods)
  require(roxygen2)
  require(utils)
  pkgDir <- "C:/Users/157216/source/r-pkg"
  pkg_storage_dir <- file.path(pkgDir, paste0("_archive"))
  if(!file.exists(pkg_storage_dir)) dir.create(pkg_storage_dir)
  switch(method,
         build = {
           author <- "Jochen Kruppa <jochen.kruppa@gmail.com> [aut,cre]"
           license <- "GPL-3"
           packageDir <- paste(pkgDir, packageName, sep ='/')
           options(devtools.desc.author = '$author',
                   devtools.desc.license = '$license')
           create(packageDir)
         },
         compile = {
           packageDir <- paste(pkgDir, packageName, sep ='/')           
           ## update version line in DESCRIPTION file
           descr_file <- file.path(packageDir, "DESCRIPTION")
           descr_lines <- readLines(descr_file)
           version_pos <- grep("^Version:", descr_lines)
           version_old <- gsub("^Version\\:\\s*", "", descr_lines[version_pos])
           version_old_num <- unlist(strsplit(version_old, "\\."))
           version_new_num <- switch(version,
                                     minor = {
                                       minor_pos <- length(version_old_num)
                                       last_old <- version_old_num[minor_pos]
                                       last_new <- as.numeric(last_old) + 1
                                       version_old_num[minor_pos] <- last_new
                                       version_old_num
                                     },
                                     major = {                                       
                                       major_pos <- length(version_old_num) - 1
                                       last_old <- version_old_num[major_pos]
                                       last_new <-as.numeric(last) + 1
                                       version_old_num[minor_pos] <- last_new
                                       version_old_num
                                     })
           descr_new_version <- paste("Version:", paste(version_new_num, collapse = "."))
           descr_lines[version_pos] <- descr_new_version
           writeLines(descr_lines, descr_file)
           ## go on with package building
           document(packageDir)
           build(packageDir)
           install(packageDir, local = FALSE)
           ## copy package into storage
           message("Move gz files to folder '_archive'")
           pkg_gz_files <- list.files(pkgDir, pattern = paste0(packageName, "_"),
                                      full.names = TRUE)
           file.rename(pkg_gz_files,
                       file.path(dirname(pkg_gz_files), "_archive",
                                 basename(pkg_gz_files)))
           message("Package version was ",
                   version_old,
                   " and is now ",
                   paste(version_new_num, collapse = "."))
         })
}
