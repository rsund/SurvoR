.onLoad <- function(libname,pkgname)  {

if(unlist(Sys.info()["sysname"])[[1]]=="Darwin")
  {
# if(!capabilities("X11") || !capabilities("tcltk"))
#  if(Sys.which("wish")[[1]]=="")
  if (getRversion() < "3.0.0")
    {
    if(!file.exists("/usr/local/include/tk.h"))
        stop("Please install Tcl/Tk for X11! (Available from http://cran.r-project.org/bin/macosx/tools/)")
    }
  }
  .muste$libname <- libname
  .muste$pkgname <- pkgname
#  requireNamespace("tcltk",quietly=TRUE)
#  attachNamespace("tcltk")
#  require(tcltk)
#  require(utils)
#  libdir<-file.path(libname, pkgname, "tklibs")
#  tcl("source", file.path(libname,pkgname,"tklibs","choosefont.tcl"))
#  addTclPath(libdir)
#  tclRequire("choosefont")
#  cat("\nent1",objects(all=TRUE, envir=.GlobalEnv))
#  rm(list=ls(pattern=".muste",all=TRUE), envir=.GlobalEnv)
#  cat("\nent2",objects(all=TRUE, envir=.GlobalEnv))  
  library.dynam("muste",pkgname,libname)

if(file.access(system.file(package=pkgname),mode=2)==-1)
  warning("Survo has no write access to its own directories!")
#else muste()  
#  cat("\nent3",objects(all=TRUE, envir=.GlobalEnv))
}

.onAttach <- function(libname,pkgname) {
packageStartupMessage("Welcome to Survo R! Launch editor using command: survo()\n")
}


