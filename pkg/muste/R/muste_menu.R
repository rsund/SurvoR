
# the following lines, adding support for ttk widgets, borrowed from Rcmdr
if (!(as.character(tcl("info", "tclversion")) >= "8.5" && getRversion() >= "2.7.0"))
	{
	ttkbutton <- tkbutton
	ttklabel <- tklabel
	ttkentry <- function(parent, ...) tkentry(parent, ...)
	ttkframe <- tkframe
	ttkradiobutton <- tkradiobutton
	ttkscrollbar <- function(...) tkscrollbar(..., repeatinterval=5)
	} 

.muste.statusbar <- function(visible=TRUE,init=FALSE)
	{
	if (!visible)
		{
		tkgrid.remove(.muste$statbar)
		return()
		}
	.muste$statbar <- ttkframe(.muste$ikkuna, relief="sunken")
	.muste$statbarl0 <- ttklabel(.muste$statbar)	
	.muste$statbarl1 <- ttklabel(.muste$statbar)
	.muste$statbarl2 <- ttklabel(.muste$statbar)
	.muste$statbarl3 <- ttklabel(.muste$statbar)
	if (init) return()
	tkpack(.muste$statbarl1, side="left", pady=2, padx=5, expand=0, fill="x")
	tkpack(.muste$statbarl2, side="left", pady=2, padx=5, expand=0, fill="x")
	tkpack(.muste$statbarl3, side="left", pady=2, padx=5, expand=0, fill="x")
	tkpack(.muste$statbarl0, side="right", pady=2, padx=5, expand=0, fill="x")
	tkgrid(.muste$statbar,column=0,row=2,sticky="sew",pady=0)
	
	}
	
.muste.scrollbar <- function(visible=TRUE)
	{
	.muste.yscrollbar(visible)
	.muste.xscrollbar(visible)
	}

.muste.yscrollbar <- function(visible=TRUE)
	{
	if (visible)
		{
		.muste.getfontdim()
		tkgrid(.muste$scry,column=1,row=0,pady=c(0,2*.muste$font.height),sticky="ns")
		tkconfigure(.muste$txt,yscrollcommand=function(...).muste.yset(.muste$scry,...))
#		tkgrid.configure(.muste$scry,sticky="ns",columnspan=4)
		}
	else tkgrid.remove(.muste$scry)
	}

.muste.xscrollbar <- function(visible=TRUE)
	{
	if (visible)
		{
		.muste.getfontdim()
		tkgrid(.muste$scrx,column=0,row=1,padx=c(0,0),sticky="nsew")
		tkconfigure(.muste$txt,xscrollcommand=function(...).muste.xset(.muste$scrx,...))
#		tkgrid.configure(.muste$scry,sticky="ns",columnspan=4)
		}
	else tkgrid.remove(.muste$scrx)
	}


.muste.yview <- function(txt,com1=NULL,com2=NULL,com3=NULL)
	{
	if (.muste$yviewrunning) return()
	.muste$yviewrunning<-TRUE
#	cat("\nyview:",com1,com2,com3)
    .Call("Muste_Edtdim","Edtdim",PACKAGE="muste")
    compar<-as.numeric(com2)
	if (identical(com1,"moveto"))
	  {
#	  cat("\nmoveto",com2)	  
	  if (compar<0) compar=0.0
	  .muste$edty.newfirst<-as.integer(compar*.muste$edty.end+1)
#	  .muste$edt.goto<-toString(c("GOTO",newfirst+1,newcur+1))
#	  cat("\n",lahteva)
	  }
	else if (identical(com1,"scroll"))
	  {
	  if (identical(com3,"pages"))
	    {
#	    cat("\npages",com2)	    
	    if (compar>0)
	    	{	    
	    	.muste$edty.newfirst<-as.integer(.muste$edty.last)
			}
		else
			{
	    	.muste$edty.newfirst<-as.integer(.muste$edty.first-(.muste$edty.last-.muste$edty.first-2))
			}
	    }
	  else if (identical(com3,"units")) 
	    {
#	    cat("\nunits",com2)	    
	    if (compar>0)
	    	{	    
	    	.muste$edty.newfirst<-as.integer(.muste$edty.first+2)
			}
		else
			{
	    	.muste$edty.newfirst<-as.integer(.muste$edty.first)
			}
		}
	  }
	if (.muste$edty.newfirst<1) .muste$edty.newfirst<-as.integer(1)
	if (.muste$edty.newfirst>.muste$edty.max) .muste$edty.newfirst<-as.integer(.muste$edty.max-(.muste$edty.last-.muste$edty.first-2))
	.muste$edty.newcur<-as.integer(.muste$edty.newfirst+(.muste$edty.cur-.muste$edty.first)-1)
	.muste$edtx.newcur<-as.integer(.muste$edtx.cur)
	.muste$edtx.newfirst<-as.integer(.muste$edtx.first+1)
    .Call("Muste_Edtgoto","",PACKAGE="muste")	  
	.muste$yviewrunning<-FALSE  
	}

.muste.xview <- function(txt,com1=NULL,com2=NULL,com3=NULL)
	{
	if (.muste$xviewrunning) return()
	.muste$xviewrunning<-TRUE	
#	cat("\nxview:",com1,com2,com3)	
    .Call("Muste_Edtdim","Edtdim",PACKAGE="muste")
    compar<-as.numeric(com2)
	if (identical(com1,"moveto"))
	  {
	  if (compar<0) compar=0.0
	  .muste$edtx.newfirst<-as.integer(compar*.muste$edtx.end+1)
	  }
	else if (identical(com1,"scroll"))
	  {
	  if (identical(com3,"pages"))
	    {
	    if (compar>0)
	    	{	    
	    	.muste$edtx.newfirst<-as.integer(.muste$edtx.last)
			}
		else
			{
	    	.muste$edtx.newfirst<-as.integer(.muste$edtx.first-(.muste$edtx.last-.muste$edtx.first-2))
			}
	    }
	  else if (identical(com3,"units")) 
	    {
	    if (compar>0)
	    	{	    
	    	.muste$edtx.newfirst<-as.integer(.muste$edtx.first+2)
			}
		else
			{
	    	.muste$edtx.newfirst<-as.integer(.muste$edtx.first)
			}
		}
	  }
	  else return()	
	if (.muste$edtx.newfirst<1) .muste$edtx.newfirst<-as.integer(1)
	
	apumax<-as.integer(.muste$edtx.max-(.muste$edtx.last-.muste$edtx.first-2))
	if (.muste$edtx.newfirst>apumax) .muste$edtx.newfirst<-apumax
	.muste$edtx.newcur<-as.integer(.muste$edtx.newfirst+(.muste$edtx.cur-.muste$edtx.first)-1)
	.muste$edty.newcur<-as.integer(.muste$edty.cur)
	.muste$edty.newfirst<-as.integer(.muste$edty.first+1)
	.muste$xviewrunning<-FALSE
    .Call("Muste_Edtgoto","",PACKAGE="muste")	  
	}


.muste.yset <- function(scry,com1=NULL,com2=NULL,com3=NULL)
    {
#    cat("\nyset:",com1,com2,com3)
    .Call("Muste_Edtdim","Edtdim",PACKAGE="muste")
#    cat("\n",.muste$edt.first,.muste$edt.last,.muste$edt.end,.muste$edt.max)
    tkset(scry,.muste$edty.first/.muste$edty.end,.muste$edty.last/.muste$edty.end)
    }

.muste.xset <- function(scrx,com1=NULL,com2=NULL,com3=NULL)
    {
#    cat("\nyset:",com1,com2,com3)
    .Call("Muste_Edtdim","Edtdim",PACKAGE="muste")
#    cat("\n",.muste$edt.first,.muste$edt.last,.muste$edt.end,.muste$edt.max)
    tkset(scrx,.muste$edtx.first/.muste$edtx.end,.muste$edtx.last/.muste$edtx.end)
    }


#if(as.character(tcl("info", "tclversion")) >= "8.5") {
  # make use of themed widgets
  # list themes
#  as.character(tcl("ttk::style", "theme", "names"))
  # select a theme -- here pre-XP windows
#  tcl("ttk::style", "theme", "use", "winnative")
#} else {
  # use Tk 8.0 widgets
#}

.muste.choosedir <- function()
	{
	dir_name <- as.character(tkchooseDirectory(initialdir=getwd(), parent=.muste$ikkuna))
    if(length(dir_name)!=0) setwd(dir_name)
	}

.muste.loadedt <- function()
	{
	file_name <- tclvalue(tkgetOpenFile(filetypes="{{Survo edit fields} {.EDT}} {{All files} *}",
					defaultextension="EDT", parent=.muste$ikkuna))
#    if (length(file_name)!=0) 
	 if (file_name == "") return()
	 file_name <- as.character(file_name)       
     if(file.exists(file_name))
        {
        setwd(dirname(file_name))
    	.muste.command(c("LoadEdt",file_name))
        }
	}

.muste.savedt <- function()
	{
	.muste$saverror <- as.integer(0);
	.muste.command("SaveEdt")
	if (.muste$saverror==1) .muste.savedtname()
	}

.muste.savedtname <- function()
	{
	 .muste$savename <- ""
	 .muste.command("GetSaveName")
	 saveFile <- tclvalue(tkgetSaveFile(filetypes="{{Survo edit fields} {.EDT}} {{All files} *}",
					defaultextension=".EDT",
					initialfile=.muste$savename,
					parent=.muste$ikkuna))
	if (saveFile == "") return()
	setwd(dirname(saveFile))
	.muste.command(c("SaveEdtName",basename(saveFile)))
	}

.muste.removeplotwindows <- function()
	{
	.muste.command("RemovePlotWindows")
	}

.muste.close <- function() 
	{
	response <- tclvalue(tkmessageBox(message="Exit from Muste?",
						icon="question", type="yesno", default="no",title=""))
	if (response == "no") return(invisible(response))
	.muste$termination<-TRUE
	.muste.end()
	}
	
#.muste.edit <- function(task="Copy") 
#	{
#	}

.muste.closer <- function()
	{
	.muste.close()
	.muste$Rtermination<-as.integer(1)
	if (.muste$Rtermination==1) quit(save="no",status=1)
	}

.muste.theme <- function(theme="CLASSIC")
	{
    .muste.command(c("Theme",theme))
    }

.muste.menu <- function(paction="ON") # ON/OFF/ONF
	{
	action<-paction
	if (action=="ONF")
		{
		.muste$menuon <- as.integer(1-.muste$menuon)
		if (.muste$menuon==0) action<-"OFF"
		else if (.muste$menuon==1) action<-"ON"
		}
	tcl("option","add","*tearOff", 0) # disable tearoff menus
   	.muste$menu<-tkmenu(.muste$ikkuna)
 	tkconfigure(.muste$ikkuna,menu=.muste$menu)
 	
 	if (action=="ON")
 	{
 	.muste$menuon <- as.integer(1)
 	.muste$file_menu<-tkmenu(.muste$menu, tearoff=FALSE)
 	tkadd(.muste$menu, "cascade", label="File",menu=.muste$file_menu)
	tkadd(.muste$file_menu, "command", label="Load edit field...",command=.muste.loadedt)
	tkadd(.muste$file_menu, "command", label="Save edit field",command=.muste.savedt)
	tkadd(.muste$file_menu, "command", label="Save edit field as...",command=.muste.savedtname)
	       	
	tkadd(.muste$file_menu, "command", label="Change directory...",command=.muste.choosedir) 
    tkadd(.muste$file_menu, "separator")

	.muste$exit_menu<-tkmenu(.muste$file_menu, tearoff=FALSE)   
    tkadd(.muste$file_menu, "cascade", label="Exit",menu=.muste$exit_menu)   	
	tkadd(.muste$exit_menu, "command", label="Exit from Muste",command=.muste.close) 
    tkadd(.muste$exit_menu, "command", label="Exit from Muste and R",command=.muste.closer)   	

    .muste$edit_menu<-tkmenu(.muste$menu, tearoff=FALSE)   
    tkadd(.muste$menu, "cascade", label="Edit",menu=.muste$edit_menu)
	tkadd(.muste$edit_menu, "command", label="Cut",command=function() .muste.command(c("Cut","1")))     
 	tkadd(.muste$edit_menu, "command", label="Cut rows",command=function() .muste.command(c("Cut","2")))    
    tkadd(.muste$edit_menu, "command", label="Cut columns",command=function() .muste.command(c("Cut","3")))
    tkadd(.muste$edit_menu, "separator")
    tkadd(.muste$edit_menu, "command", label="Copy",command=function() .muste.command(c("Cut","0")))
#    tkadd(.muste$edit_menu, "command", label="Copy rows",command=function() .muste.edit("Cut"))
    tkadd(.muste$edit_menu, "separator")
    tkadd(.muste$edit_menu, "command", label="Paste",command=function() .muste.command(c("Cut","11")))
    tkadd(.muste$edit_menu, "command", label="Paste rows",command=function() .muste.command(c("Cut","12")))
    tkadd(.muste$edit_menu, "command", label="Paste columns",command=function() .muste.command(c("Cut","13")))
    tkadd(.muste$edit_menu, "separator")
	tkadd(.muste$edit_menu, "command", label="Clear",command=function() .muste.command(c("Cut","4")))     
#    tkadd(.muste$edit_menu, "separator")
#    tkadd(.muste$edit_menu, "command", label="Select all",command=function() .muste.edit("Cut"))     
    
      
    .muste$view_menu<-tkmenu(.muste$menu, tearoff=FALSE)   
    tkadd(.muste$menu, "cascade", label="View",menu=.muste$view_menu)
    
 	.muste$theme_menu<-tkmenu(.muste$view_menu, tearoff=FALSE)   
    tkadd(.muste$view_menu, "cascade", label="Theme",menu=.muste$theme_menu)   	
	tkadd(.muste$theme_menu, "command", label="Classic",command=function() .muste.theme("CLASSIC")) 
    tkadd(.muste$theme_menu, "command", label="White",command=function() .muste.theme("WHITE")) 
    tkadd(.muste$view_menu, "command", label="Remove plot windows",command=.muste.removeplotwindows)
 
 
 
     .muste$help_menu<-tkmenu(.muste$menu, tearoff=FALSE)   
     tkadd(.muste$menu, "cascade", label="Help",menu=.muste$help_menu,state="disabled")
 	 }
 	 else .muste$menuon <- as.integer(0)
	}
	
	


#tkadd(file_menu,"command", label = "Source file...",
#      command =  function() {
#        file_name <- tkgetOpenFile(filetypes=
#                        "{{R files} {.R}} {{All files} *}")
#        if(file.exists(file_name <- as.character(file_name)))
#           source(tclvalue(file_name))
#      })
#tkadd(file_menu, "command", label = "Save workspace as...",
#      command = function() {
#        file_name <- tkgetSaveFile(defaultextension = "Rsave")
#        if(nchar(fname <- as.character(file_name)))
#          save.image(file = file_name)
#      })

