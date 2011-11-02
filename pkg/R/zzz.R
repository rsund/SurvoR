.onLoad <- function(libname,pkgname) { .First.lib(libname,pkgname) }

.First.lib <- function(libname,pkgname) {

if(file.access(system.file(package="muste"),mode=2)==-1)
  stop("Muste requires write access to its own directories!")

if(unlist(Sys.info()["sysname"])[[1]]=="Darwin")
  {
#  if(Sys.which("wish")[[1]]=="")
  if(!file.exists("/usr/local/include/tk.h"))
    stop("Please install Tcl/Tk for X11! (Available from http://cran.r-project.org/bin/macosx/tools/)")
  }

  require(tcltk)
  require(utils)
#  libdir<-file.path(libname, pkgname, "tklibs")
  tcl("source", file.path(libname,pkgname,"tklibs","choosefont.tcl"))
#  addTclPath(libdir)
#  tclRequire("choosefont")
  library.dynam("muste",package="muste",lib.loc=NULL)
  muste()
}
