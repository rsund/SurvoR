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