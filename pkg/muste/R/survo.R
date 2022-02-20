.muste <- new.env(hash=TRUE, parent=emptyenv())

read.svo <- function(file)
{
    if(length(grep("^(http|ftp|https)://", file))) {
        tmp <- tempfile()
        download.file(file, tmp, quiet = TRUE, mode = "wb")
        file <- tmp
        on.exit(unlink(file))
    }
    rval <- .Call("do_readSurvo", file, PACKAGE="muste")    
    attributes(rval) <- .muste.svoattributes(rval)
    rval
}

write.svo <- function(dataf,svofile)
{
    rval <- .Call("do_writeSurvo", as.data.frame(dataf), as.character(svofile),
                  as.character(deparse(substitute(dataf))),PACKAGE="muste")    
}

muste_ExpandFileName <- function(path="")
	{
	.Call("Muste_ExpandPath",as.character(path))
	}

.muste.restore.eventloop <- function()  
  {
#  .muste$eventloop.after<-0
   if (.muste$eventloop.after==1) 
   tcl("after", "cancel", .muste$eventloopid)
  .muste$eventloop.after<-0
  .muste$jatkuu<-as.integer(1)
  
  .muste$eventlooprun<-TRUE
   invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
  if (.muste$eventloop)
  	{
  	.muste$eventloopid <- tcl("after",.muste$eventlooptime,.muste.eventloop)  
  	}
  }

  
.muste.eventloop <- function()  
  {
  .muste$eventloop.after<-0

  if (!.muste$eventlooprun || .muste$termination) 
     { 
#     cat("Muste terminated!!!\n")
     .muste.end()
#     if (.muste$Rtermination==1) quit(save="no",status=1)
     return()
     }  
  
  invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
  if (.muste$eventlooprun && .muste$eventloop)
    { 
    .muste$eventloop.after<-1
    .muste$eventloopid <- tcl("after",.muste$eventlooptime,.muste.eventloop)
    }

  }

.muste.sleep <- function(time)
  {
tryCatch(
  {
   Sys.sleep(time)
  }, 
  interrupt = function(inter) { 
  if (.muste$interrupt==0)
  	{
  	cat("Please return to normal editorial mode in Survo!\n")
  	.muste$interrupt<-1
  	}
  else if (.muste$interrupt==1)	
  	{
  	cat("Trying to force Survo back to normal state!\n")
  	.muste$interrupt<-2
#    .Call("Muste_Command","Restore",PACKAGE="muste")	    	
    .muste.command("Restore", force=TRUE)
  	}
  else if (.muste$interrupt==2)	
  	{
  	cat("Beware! Next break will shut down Survo and R!\n")
  	.muste$interrupt<-3
  	}  	
  else if (.muste$interrupt>2)
  	{
#  	.muste$interrupt<-0
  	cat("Dumping the edit field!\n")
#    .Call("Muste_Command","DumpEdt",PACKAGE="muste")
    .muste.command("DumpEdt",force=TRUE)	    	
  	.muste.end()
  	cat("Emergency shut down for Survo and R!\n")
  	quit(save="no",status=1, runLast=FALSE)
  	}	
  }
#  , finally = { cat("Finalizing\n") }
  )  
  }


.muste.runsource <- function(file,dest=NULL,echo=TRUE,print.eval=TRUE,encoding=getOption("encoding"))
  {

   if (.muste$eventloop.after) 
   tcl("after", "cancel", .muste$eventloopid)
   
   if (!is.null(dest))
   		{
   		output <- file(dest,open="w+")
   		sink(output,type="output")
    	sink(output,type="message")  		
   		}
   		
tryCatch(
  {
   source(file,echo=echo,print.eval=print.eval,encoding=encoding)     
  },
  error = function(error) { 
  cat("Error in R code!\n")
  print(error)
  .muste.restore.eventloop()
  }, 
  interrupt = function(inter) { 
  cat("Running of R script interrupted!\n")
  .muste.restore.eventloop()
  }
#  , finally = { cat("Finalizing\n") }
  )

   if (!is.null(dest))
   		{
   		sink(type="message")
   		sink(type="output")   		
   		close(output)
   		}   
  
   .muste.restore.eventloop()
  }
	

.muste$charin <-  as.character("\u20AC\u058A\u05BE\u2010\u2011\u2012\u2013\u2014\u2015\u207B\u208B\u2212\uFE58\uFE63\uFF0D\u2018\u2019\u201A\u201B\u201C\u201D\u201E\u201F\u301D\u301E\u301F\uFF02")
.muste$charout <- as.character("\u0131\u2010\u2010\u2010\u2010\u2010\u2010\u2010\u2010\u2010\u2010\u2010\u2010\u2010\u2010\u0027\u0027\u0027\u0027\u0022\u0022\u0022\u0022\u0022\u0022\u0022\u0022")

.muste.getclipboard <- function()
  {
tryCatch(
  {
  if (.muste$sysname=="Windows") { clipb<-try(paste(readClipboard(),collapse="\n")) }
  else
    { 
    clipb<-try(tclvalue(tcl("clipboard","get")))
    if (nchar(clipb)==0) clipb<-try(tclvalue(tcl("clipboard","get","-type","UTF8_STRING"))) 
    }
  }, 
  interrupt = function(inter) { 
  cat("Clipboard empty!\n")
  }
#  , finally = { cat("Finalizing\n") }
  )  
   
#  .muste$clipboard<-tcl("clipboard","get")
#  clipb<-try(tclvalue(tcl("clipboard","get")))
  clipapu<-ifelse(class(clipb)=="try-error",as.character(""),enc2utf8(as.character(clipb)))
  .muste$clipboard<-chartr("\u2010","-",chartr(.muste$charin,.muste$charout,clipapu))
#.muste$clipboard<-ifelse(class(clipb)=="try-error",as.character(""),iconv(as.character(clipb),from="",to="UTF-8",sub="?"))
  .muste$clipboardlen<-as.integer(nchar(.muste$clipboard, type = "bytes", allowNA = TRUE))
  }
 
.muste.putclipboard <- function(leike="")
	{
tryCatch(
  {	 
	  if (.muste$sysname=="Windows")
	  	{
	  	leike <- gsub("\n","\r\n",as.character(leike),fixed=TRUE) 
	  	utils::writeClipboard(as.character(leike))
	  	}
	  else 
	  	{ 
	  	tcl("clipboard","clear")
	  	tcl("clipboard","append",leike)
	  	}
  }, 
  interrupt = function(inter) { 
  cat("Clipboard error!\n")
  }
  )  
	}
  
  

.muste.keypress <- function(A,K,N,k,t,T,s)
  {

if (.muste$termination) return()

# A = UNICODE character
# K = The keysym corresponding to the event, substituted as a textual string.
# N = The keysym corresponding to the event, substituted as a decimal number.
# k = The keycode field from the event.
# t = The time field from the event.
# T = The type field from the event.

  .muste$key.status<-as.integer(s)
  if (as.integer(N)==65406) .muste$key.alt<-TRUE; # Mac only???

 latinchar <- charToRaw(iconv(A, "UTF-8","LATIN1","?"))

#cat("\nstatus:",N,A,K,k,t,s,T,latinchar)

  .muste$inchar<-iconv(A, "UTF-8","CP850","?") 

  if (identical(K,"KP_Enter")) { .muste$inchar<-"?" }

  if (is.na(.muste$inchar))
  {
  .muste$inchar<-"?"
  }

  nonascii <- (.muste$inchar=="" || .muste$inchar=="?" || charToRaw(.muste$inchar)>127)

  if (identical(K,"EuroSign")) # Special handling for euro (as in CP858)
  	{
  	  nonascii <- FALSE
  	  N <- as.integer(213)
  	}
  else
  if (length(latinchar)==1)
    {
    if (latinchar==163) # Special handling for pound in Mac
  	  { 
  	    nonascii <- FALSE
  	    N <- as.integer(156)
  	  } 
  	}
  else
    if (length(latinchar)==3)
    	{
    	if (latinchar[1]==63 && latinchar[2]==63 && latinchar[3]==63 && as.integer(N)==52)
    		{
    		nonascii <- FALSE
  	  		N <- as.integer(213)
    		}
    	}

  if ((as.integer(s)==8192 || as.integer(s)==8194) && (nonascii || identical(K,"Delete")) )
    {
    
    .muste$event.time<-as.integer(t)
    .muste$event.type<-as.integer(3)  # SPECIAL_KEY_EVENT
    .muste$key.keysym<-as.integer(as.integer(N)+100000)
    .muste$key.status<-as.integer(s)

	if (as.integer(N)==101) .muste$key.keysym<-as.integer(as.integer(213))

invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
    
#    cat("Erikois.muste$inchar ALT:",A,.muste$key.keysym,k,t,s,"\n")

    }
  else if (as.integer(s)==4 || as.integer(s)==6 && nonascii )
    {
    .muste$event.time<-as.integer(t)
    .muste$event.type<-as.integer(3)  # SPECIAL_KEY_EVENT
    .muste$key.keysym<-as.integer(as.integer(N)+200000)
    .muste$key.status<-as.integer(s)

invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
    
#    cat("Erikois.muste$inchar CTRL:",A,.muste$key.keysym,k,t,s,"\n")
    }
  else {

  if (identical(K,"BackSpace")) 
    { 
    .muste$inchar<-"?"
    N <- as.integer(65288)
    }
  
  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(1)  # KEY_EVENT
  .muste$key.char<-.muste$inchar
  .muste$key.keysym<-as.integer(N)
  .muste$key.status<-as.integer(s)

invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
#cat("\nMerkki:",A,.muste$key.keysym,k,t,s,"\n")
  }
  }

.muste.specialkeypress <- function(A,K,N,k,t,T,s)
  {

if (.muste$termination) return()

  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(3)  # SPECIAL_KEY_EVENT
  .muste$key.keysym<-as.integer(as.integer(N)+100000)
  .muste$key.status<-as.integer(s)

#cat("Erikois.muste$inchar:",A,.muste$key.keysym,k,t,s,"\n")
invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
  } 

.muste.specialkeypress_ctrl <- function(A,K,N,k,t,T,s)
  {

if (.muste$termination) return()

  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(3)  # SPECIAL_KEY_EVENT
  .muste$key.keysym<-as.integer(as.integer(N)+200000)
  .muste$key.status<-as.integer(s)

#cat("shift_Erikois.muste$inchar:",A,.muste$key.keysym,k,t,s,"\n")
invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
  } 

.muste.specialkeypress_shift <- function(A,K,N,k,t,T,s)
  {

if (.muste$termination) return()

  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(3)  # SPECIAL_KEY_EVENT
  .muste$key.keysym<-as.integer(as.integer(N)+300000)
  .muste$key.status<-as.integer(s)

#cat("shift_Erikois.muste$inchar:",A,.muste$key.keysym,k,t,s,"\n")
invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
  } 
  
.muste.specialkeypress_euro <- function(A,K,N,k,t,T,s)
  {

if (.muste$termination) return()

  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(3)  # SPECIAL_KEY_EVENT
  .muste$key.keysym<-as.integer(as.integer(213))
  .muste$key.status<-as.integer(s)

#cat("shift_Erikois.muste$inchar:",A,.muste$key.keysym,k,t,s,"\n")
invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
  }   


.muste.keyrelease <- function(A,K,N,k,t,T,s)
  {
if (.muste$termination) return()

  .muste$key.status<-as.integer(s)
  if (as.integer(N)==65406) .muste$key.alt<-FALSE;  
#  cat("Keyrelease:",A,.muste$key.keysym,k,t,s,.muste$key.status,"\n")
invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
  }


.muste.mouseevent <- function(x,y,t,T,b)
  {
# t = The time field from the event.
# T = The type field from the event.
	if (.muste$termination) return()

  .muste$mouseevent.x<-x
  .muste$mouseevent.y<-y
  .muste$mouseevent.t<-t
  .muste$mouseevent.T<-T
  .muste$mouseevent.b<-b

#cat("\ns:",.muste$key.status)
#cat("\nT,b",T,b)

  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(2)  # MOUSE_EVENT
  .muste.getmouse()

  if (as.integer(T)!=4) b<-0
  if (as.integer(b)==2) b<-3 # Mac Aqua remap
  .muste$mouse.button<-as.integer(b)
  .muste$mouse.double<-as.integer(0)
  
  if (.muste$selection==1 || .muste$selection==2) 
  	{
  	.muste$selection<-as.integer(2)
  	.muste.getmouse2(x,y)
#  	cat("\nselcoord:",.muste$mouse.row,.muste$mouse.col)
  	.muste$selection.r2<-.muste$mouse.row
  	.muste$selection.c2<-.muste$mouse.col
  	.muste$selection.show<-as.integer(1)
  	.Call("Muste_Selection","selcoord",PACKAGE="muste")
  	}
  if (b==1)
  	{
  	
	if (.muste$key.alt)
    {
	.muste$selection.alt<-as.integer(1-.muste$selection.alt)
    }
  	
#  	cat("\nstartcoord:",.muste$mouse.row,.muste$mouse.col);
  	.muste$selection.r1<-.muste$mouse.row
#  	.muste$selection.r2<-as.integer(-2)
  	.muste$selection.c1<-.muste$mouse.col  
  	.muste$selection.r2<-.muste$mouse.row
  	.muste$selection.c2<-.muste$mouse.col 
  	.muste$selection<-as.integer(1)
  	.muste$selection.show<-as.integer(0)
  	.Call("Muste_Selection","selcoord",PACKAGE="muste")
  	}

invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
#cat("Mouse:",.muste$mouse.col,.muste$mouse.row,x,y,t,T,b,.muste$mouse.double,"\n")
}

.muste.selcoord <- function()
	{
#	if (.muste$selcoordrunning) return()
#	.muste$selcoordrunning<-TRUE
	
	.muste.getmouse2(.muste$mouseevent.x,.muste$mouseevent.y)
  	.muste$selection.r2<-.muste$mouse.row
  	.muste$selection.c2<-.muste$mouse.col
#	cat("\nselcoord:",.muste$selection.c1)
	if (.muste$selection==2) .Call("Muste_Selection","selcoord",PACKAGE="muste")
#	.muste$selcoordrunning<-FALSE
	}

.muste.mousealtbuttonevent <- function(x,y,t,T,b)
  {  
# Windows only, not working in mac
if (.muste$termination) return()
  
#cat("\naltbuttonevent:",.muste$mouse.row,.muste$mouse.col)
    
  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(2)  # MOUSE_EVENT
  .muste.getmouse()

  .muste$mouse.button<-as.integer(1) # Motion only with left button
  .muste$mouse.double<-as.integer(0)

  .muste$selection.alt<-as.integer(1-.muste$selection.alt)

#  	cat("\nstartcoord:",.muste$mouse.row,.muste$mouse.col);
  	.muste$selection.r1<-.muste$mouse.row
#  	.muste$selection.r2<-as.integer(-2)
  	.muste$selection.c1<-.muste$mouse.col  
  	.muste$selection.r2<-.muste$mouse.row
  	.muste$selection.c2<-.muste$mouse.col 
  	.muste$selection<-as.integer(1)
  	.muste$selection.show<-as.integer(0)
  	.Call("Muste_Selection","selcoord",PACKAGE="muste")
  


invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
#cat("Mouse:",.muste$mouse.col,.muste$mouse.row,x,y,t,T,b,.muste$mouse.double,"\n")  
  }

.muste.mousebuttonreleaseevent <- function(x,y,t,T,b)
  {
if (.muste$termination) return()  
#  .muste$event.time<-as.integer(t)
#  .muste$event.type<-as.integer(2)  # MOUSE_EVENT
  .muste.getmouse()

#  .muste$mouse.button<-as.integer(b) 
#  .muste$mouse.double<-as.integer(0)
  
  if (b==1)
  	{ 
#  	cat("\nendcoord:",.muste$mouse.row,.muste$mouse.col)
#  	if (.muste$selection!=5) 
  	.muste$selection<-as.integer(3)
  	if (.muste$selection.show==0) .muste$selection<-as.integer(4)
  	.muste$selection.r2<-.muste$mouse.row
  	.muste$selection.c2<-.muste$mouse.col
  	.Call("Muste_Selection","endcoord",PACKAGE="muste")  	  	
  	}
  
#invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
#cat("Mouse:",.muste$mouse.col,.muste$mouse.row,x,y,t,T,b,.muste$mouse.double,"\n")  
  }

.muste.doublemouseevent <- function(x,y,t,T,b)
  {
if (.muste$termination) return()  
# t = The time field from the event.
# T = The type field from the event.

  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(2)  # MOUSE_EVENT
  .muste.getmouse()
  .muste$mouse.button<-as.integer(b)
  .muste$mouse.double<-as.integer(1)

invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
#cat("Mouse:",.muste$mouse.col,.muste$mouse.row,x,y,t,T,b,.muste$mouse.double,"\n")
}

.muste.command <- function(command="Exit",force=FALSE)
	{
	if (.muste$exitok || force)
	    {
        .Call("Muste_Command",command,PACKAGE="muste")
        if (command[[1]]=="Exit") invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
	    }
	else
	    {
	    if (command[[1]]=="Exit") .muste$exitpressed<-as.integer(1)
	    else .muste$exitpressed<-as.integer(0)
	    }
	}



.muste.init <- function()
  {
  .muste$environment <- environment()
  
  .muste$writeaccess<-as.integer(1)
  if(file.access(system.file(package="muste"),mode=2)==-1) .muste$writeaccess<-as.integer(0) 

# R.version$platform
#.Platform$OS.type  "unix" or "windows"
  .muste$sysname<-unlist(Sys.info()["sysname"])[[1]]
  .muste$startdir <- getwd()
  setwd(R.home())
  .muste$Rhome<-getwd()
  .muste$homedir<-normalizePath("~/")
  setwd(.muste$startdir)
  .muste$Rtempdir <- tempdir()
  .muste$mustepath <- system.file(package="muste")
  .muste$OS.type <- .Platform$OS.type
  .muste$r_arch <- .Platform$r_arch
  if (.muste$sysname=="Windows")
    {
    .muste$Rbin <-  paste(file.path(R.home("bin"),"Rgui --sdi"))  # paste(shQuote(file.path(R.home("bin"),"Rgui --sdi"))) 
    }
  else .muste$Rbin <- paste(file.path(R.home("bin"),"R")) # paste(shQuote(file.path(R.home("bin"),"R")))   
  

if (.muste$sysname!="Windows") { tcl("clipboard","clear") }
#tcl("clipboard","append","")

  .muste$key.status<-as.integer(0)
  .muste$scale.lock<-FALSE

}

.muste.stop <- function()
{
.muste$eventlooprun <- FALSE
#if (.muste$eventloop.after) 
#   tcl("after", "cancel", .muste$eventloopid)
#.muste$eventloop.after <- TRUE  
}


.muste.end <- function()
{
if (.muste$endfunction) return()
.muste$endfunction <- TRUE
.muste$termination<-as.integer(1)
.muste$eventlooprun <- FALSE
.muste$eventlooptime<-as.integer(1)
if (.muste$eventloop.after) 
tcl("after", "cancel", .muste$eventloopid)
 .muste.command("Exit")

#tcl("update","idletasks")
#tcl("update")

#invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))

#endwait<-0
#while (.muste$eventloop.after==1 && endwait<20)
#while (.muste$jatkuu==1 && endwait<50)
#  {
#  Sys.sleep(0.1)
#  endwait<-endwait+1
#  }

if (exists("editor",envir=.muste)) rm("editor",envir=.muste) # ,inherits=TRUE)
tcl("after",100,.muste.destroywindow) 
.muste.remove.bindings() 
#q()
}

survo <- function(sucro="<empty>",config="<empty>") muste(sucro,config)

muste <- function(sucro="<empty>",config="<empty>") 
{

if (!interactive() && sucro=="<empty>")
    {
    warning("Survo requires interactive session!")
    invisible(return(FALSE))
    }

if (exists("editor",where=.muste))
	{
	stop("Survo editor is already running! Please use sucro /Z to launch a new editor.")
	}

.muste$mustepath <- system.file(package="muste")

if (sucro!="<empty>") # 27.2.2013
    {
    if  (file.exists(sucro) || file.exists(paste(sucro,".TUT",sep=""))) 
        { 
        cat(paste("Using start sucro",sucro,"!\n"))
        }
    else if (file.exists(paste(.muste$mustepath,"/S/",sucro,sep="")) ||
             file.exists(paste(.muste$mustepath,"/S/",sucro,".TUT",sep="")))
        { 
#        cat(paste("Using start sucro",sucro,"\n"))
        sucro <- (paste(.muste$mustepath,"/S/",sucro,".TUT",sep=""))
        }
    else
        {
        stop(paste("Start sucro",sucro,"not found!"))
        }    
    }

.muste$exitok<-0

if (getRversion() >= "2.14.0")
	{
	requireNamespace("tcltk",quietly=TRUE)
	try(attachNamespace("tcltk"),silent=TRUE)
	#  require(tcltk)
	}
else .muste.command(c("Require","tcltk"),force=TRUE)

 .muste$editor=TRUE
 .muste$termination<-FALSE
 .muste$Rtermination<-FALSE
 .muste$jatkuu<-as.integer(1)
 
# .muste.command("HookLast",force=TRUE)


.muste$eventloopargs<-"Tosi"
.muste.init()
.muste.inittkwindow()

# Initialize global variables
.muste$webedit<-as.integer(0)
.muste$statbar<-FALSE
.muste$menuon <- as.integer(0)
.muste$help.ikkuna.existing<-FALSE
.muste$selcoordrunning<-FALSE
.muste$oldeventtime<-as.numeric(0.0)
.muste$mousewheeltime<-as.integer(9999)
.muste$yviewrunning<-FALSE
.muste$xviewrunning<-FALSE
.muste$selection<-as.integer(0)
.muste$selection.show<-as.integer(0)
.muste$selection.r1<-as.integer(0)
.muste$selection.c1<-as.integer(0)
.muste$selection.r2<-as.integer(0)
.muste$selection.c2<-as.integer(0)
.muste$selection.alt<-as.integer(0)
.muste$mouseevent.x<-NULL
.muste$mouseevent.y<-NULL
.muste$mouseevent.t<-NULL
.muste$mouseevent.T<-NULL
.muste$mouseevent.b<-NULL
.muste$key.alt<-FALSE;
.muste$edty.newfirst<-as.integer(0)
.muste$edty.newfcur<-as.integer(0)
.muste$edtx.newfirst<-as.integer(0)
.muste$edtx.newfcur<-as.integer(0)
.muste$edty.first<-as.integer(0)
.muste$edty.last<-as.integer(0)
.muste$edty.max<-as.integer(0)
.muste$edty.end<-as.integer(0)
.muste$edty.cur<-as.integer(0)
.muste$edtx.first<-as.integer(0)
.muste$edtx.last<-as.integer(0)
.muste$edtx.max<-as.integer(0)
.muste$edtx.end<-as.integer(0)
.muste$edtx.cur<-as.integer(0)


.muste$winsize <- NULL
.muste$inchar <- NULL
.muste$plotid <- NULL
.muste$event.type <- NULL
.muste$mouse.button <- NULL
.muste$mouse.double <- NULL
.muste$clipboard <- NULL
.muste$cursor.row <- NULL
.muste$cursor.col <- NULL
.muste$font.width <- NULL
.muste$font.height <- NULL
.muste$mouse.row <- NULL
.muste$mouse.col <- NULL
.muste$screen.width <- NULL
.muste$screen.height <- NULL
.muste$event.type <- NULL
.muste$key.keysym <- NULL
.muste$key.char <- NULL
.muste$event.type <- NULL
.muste$mouse.button <- NULL
.muste$mouse.double <- NULL
.muste$plotid <- NULL
.muste$key.keysym <- NULL

.muste$tmp.filespec <- NULL
.muste$tmp.length <- NULL
.muste$tmp.filespec <- NULL
.muste$tmp.length <- NULL
.muste$tmp.dirname <- NULL
.muste$tmp.fileinfo <- NULL
.muste$tmp.dirname <- NULL
.muste$tmp.nfiles <- NULL
.muste$tmp.nthese <- NULL
.muste$tmp.selected <- NULL
.muste$tmp.fileinfo1 <- NULL
.muste$tmp.filename <- NULL
.muste$tmp.fileinfo0 <- NULL
.muste$tmp.filecount <- NULL
.muste$tmp.filisdir <- NULL
.muste$tmp.filesize <- NULL
.muste$tmp.filetime <- NULL
.muste$tmp.basename <- NULL
.muste$endfunction <- FALSE
.muste$interrupt<-0
.muste$exitok<-1
.muste$exitpressed<-as.integer(0)
.muste$insertcursorcolor <- "#90F"
.muste$startsucro <- as.character(sucro)
.muste$redraw <- as.integer(0)

.muste$event.time<-as.integer(0)
.muste$eventlooptime<-as.integer(1000)
.muste$eventlooprun<-TRUE
.muste$eventloop.after<-0
.muste$eventloop<-FALSE

if (config=="<empty>") # 29.5.2013
    {
    .muste$apufile <- Sys.getenv("MUSTEAPU")
    if (nchar(.muste$apufile)==0)
        {
        if (.muste$sysname=="Windows") .muste$apufile <- paste(.muste$homedir,'\\.muste\\muste.apu',sep="")
        else .muste$apufile <- paste(.muste$homedir,'/.muste/muste.apu',sep="")
        }
	}
	else .muste$apufile <- config
#    args<-"A"
i<-as.integer(.Call("Muste_Editor",.muste,PACKAGE="muste"))
if (i>0)
	{
	if(!file.exists(.muste$apufile)) .muste.setup(init=TRUE)
	.muste.init.bindings()
	invisible(.muste.eventloop())
	tcl("wm", "attributes", .muste$ikkuna, topmost=TRUE)
	.muste.focus.editor()
	tcl("wm", "attributes", .muste$ikkuna, topmost=FALSE)
	}
if (i<0) 
	{
	.muste$eventlooprun <- FALSE
	.muste.end()
	warning("Failed to initialize Survo!")
	invisible(return(FALSE))
	}
invisible(TRUE)		
}
