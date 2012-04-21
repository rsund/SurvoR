.onLoad <- function(libname,pkgname) { .First.lib(libname,pkgname) }

.First.lib <- function(libname,pkgname) {

if(unlist(Sys.info()["sysname"])[[1]]=="Darwin")
  {
#  if(Sys.which("wish")[[1]]=="")
  if(!file.exists("/usr/local/include/tk.h"))
    stop("Please install Tcl/Tk for X11! (Available from http://cran.r-project.org/bin/macosx/tools/)")
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
  library.dynam("muste",package="muste",lib.loc=NULL)

if(file.access(system.file(package="muste"),mode=2)==-1)
  warning("Muste has no write access to its own directories!")
#else muste()  
packageStartupMessage("Welcome to Muste! Launch editor using command: muste()\n")
#  cat("\nent3",objects(all=TRUE, envir=.GlobalEnv))
}
