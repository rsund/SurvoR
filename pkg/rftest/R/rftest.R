.rftest.init <- function()
  {
  .rftest$environment <- environment()
  
  .rftest$writeaccess<-as.integer(1)
  if(file.access(system.file(package="muste"),mode=2)==-1) .rftest$writeaccess<-as.integer(0) 

  tcl("source", file.path(.rftest$libname,.rftest$pkgname,"tklibs","choosefont.tcl"))
    
  .rftest$ikkuna <- tktoplevel()
  tcl("wm", "protocol", .rftest$ikkuna, "WM_DELETE_WINDOW", quote(.rftest.command("Exit")))
  tkwm.resizable(.rftest$ikkuna, FALSE, FALSE)

  tkwm.title(.rftest$ikkuna, "Muste")

  .rftest$sysname<-unlist(Sys.info()["sysname"])[[1]]
  .rftest$Rhome<-R.home()
  .rftest$Rtempdir <- tempdir()
  .rftest$OS.type<-.Platform$OS.type
  if (.rftest$sysname=="Darwin") { .rftest$font <- tkfont.create(family="Menlo",size=14) }
  else if (.rftest$sysname=="Windows")
  	{ 
  	.rftest$font <- tkfont.create(family="Lucida Console",size=12)
  	.rftest$menu<-tkmenu(.rftest$ikkuna)
	tkconfigure(.rftest$ikkuna,menu=.rftest$menu)
	tkadd(.rftest$menu, "cascade", label="Muste")
  	}
  else { .rftest$font <- tkfont.create(family="Courier",size=12) }
   
  .rftest$txt <- tktext(.rftest$ikkuna,width=80,height=25,foreground="#000000",background="snow",
                            wrap="none",font=.rftest$font,undo=FALSE)
  tkgrid(.rftest$txt)  
  
  .rftest$window<-.Tk.ID(.rftest$txt)

  Sys.sleep(1);
  sidokset <- gsub("Text ","",tclvalue(tkbindtags(.rftest$txt)))
  tkbindtags(.rftest$txt,sidokset)
  
  tcl("bind","all","<Key-F10>","")
  tcl("bind","all","<Alt-Key>","")
  tcl("bind","Menubutton","<Key-F10>","")
  tcl("bind","Menubutton","<Alt-Key>","")

if (.rftest$sysname!="Windows") { tcl("clipboard","clear") }

  .rftest$key.status<-as.integer(0)
  .rftest$scale.lock<-FALSE

tkfocus("-force",.rftest$txt)
tkfocus(.rftest$txt)
tcl("update","idletasks")
tcl("update")
}

rftest <- function() 
{
requireNamespace("tcltk",quietly=TRUE)
attachNamespace("tcltk")

.rftest.init()

Sys.sleep(10);
tkdestroy(.rftest$txt)
tkdestroy(.rftest$ikkuna)
}
