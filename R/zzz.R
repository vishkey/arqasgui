#--------------------------------------------------------------------
.onAttach <- function(libname, pkgname){
  #--------------------------------------------------------------------
  #   pkg.info <- utils::packageDescription(pkgname, libname, fields = c("Title", "Version", "Date"))
  #   Read all the routes in the ini file
  userfile <- file(system.file("userRoutes.ini", package="arqasgui"), "r")
  routes <- readLines(userfile)
  for (i in routes) {
    source(i)
  }
  close(userfile)
  
  pkg.info <- drop( read.dcf( file = system.file("DESCRIPTION", package = "arqas"),
                              fields = c("Title", "Version", "Date") ))
  packageStartupMessage( 
    paste("\n Loading package arqasgui:", pkg.info["Title"], "\n"),
    paste(" version ", pkg.info["Version"], " (built on ", pkg.info["Date"], ").\n", sep=""),
    " Copyright Borja Varela 2013.\n")
}
