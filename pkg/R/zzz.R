.First.lib <- function(libname,pkgname) {
  require(tcltk)
  require(utils)
#  libdir<-file.path(libname, pkgname, "tklibs")
  tcl("source", file.path(libname,pkgname,"tklibs","choosefont.tcl"))
#  addTclPath(libdir)
#  tclRequire("choosefont")
  library.dynam("muste")
  muste()
}
