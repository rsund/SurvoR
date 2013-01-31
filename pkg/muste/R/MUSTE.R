#require(tcltk)
.muste <- new.env(hash=TRUE, parent=emptyenv())

.muste.del <- function(tiedosto)
  {
  unlink(tiedosto)
  if (file.exists(tiedosto)) { file.remove(tiedosto) }
  }

.muste.dir <- function(komento,odotus=FALSE)
  {
  if (.muste$sysname=="Windows") 
  	{
#  	dircmd <- paste("DIR",gsub("/","\\",komento,fixed=TRUE),"/-c")
    dircmd <- paste("DIR",komento)
  	.muste.system(dircmd,odotus=odotus)
  	}
  else
  	{ 
  	dircmd <- paste("ls -al",komento)
  	.muste.system(dircmd,odotus=odotus)
  	}
  }

.muste.getfile <- function(file="") # ,keep=0,dest=NULL
	{		
	if(length(grep("^(http|ftp|https)://", file)))
		{
		tmp <- tempfile()
		download.file(file, tmp, quiet = TRUE, mode = "wb")
#		if (keep)
#			{
#			if (!is.null(dest)) outfile=dest
#			else outfile=substring(file,regexpr("\\/[^\\/]*$", file)+1)
#			file.copy(tmp,outfile,overwrite=TRUE)
#			file<-outfile
#			}
#		else 
		file <- tmp
		}
	
	.muste$retrievedfile <- file	
	}

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

.muste.txtconv <- function(infile,outfile,instring=c(""),outstring=c(""),lines=1,MAXSIZE=10000000,perl=FALSE)
	{
    if(length(grep("^(http|ftp|https)://", infile)))
    	{
        tmp <- tempfile()
        download.file(infile, tmp, quiet = TRUE, mode = "wb")
        infile <- tmp
        on.exit(unlink(infile))
        }	
	
	if (!file.exists(infile)) return
	fs <- file.info(infile)$size
	if (fs>MAXSIZE) 
		{ 
		stop("File too large, increase MAXSIZE!\n") 
		return 
		}
	if (lines==1) s<-readLines(infile)
	else s<-readChar(infile,fs)
	for (i in 1:length(instring)) s <- gsub(instring[i],outstring[i],s,perl=perl)		
	sink(outfile)
	cat(s,sep="\n")
	sink()
	}

.muste.svoattributes <- function(rval)
	{
	att <- attributes(rval)
    rval <- as.data.frame(rval, stringsAsFactors=FALSE)
##    class(rval) <- "data.frame"
    newatt <- attributes(rval)
    newatt <- c(newatt, att[!(names(att) %in% names(newatt))])
    return(newatt)
	}

.muste.mousewheel <- function(t,X,Y,D,s)
	{
#	.muste$mousewheeltime<-as.integer(abs(as.numeric(t)-.muste$oldeventtime))
	if (as.numeric(t)-.muste$oldeventtime<3) 
		{
		if (.muste$oldeventtime==0) .muste$oldeventtime<-as.numeric(t)
		return()
		}
#	cat("\n",t,X,Y,D,s,as.numeric(t)-.muste$oldeventtime)
	D<-as.numeric(D)
	if (abs(D)<120)	D<-D/abs(D)*120
	delta <- -1*D/120
	if (.muste$sysname=="Windows") delta <- (abs(delta)^2)*sign(delta)
#############  mac=1   ############	 windows=9 ##########
	if (as.integer(s)==1 || as.integer(s)==9) .muste.xview(.muste$scrx,"scroll",delta,"units")
	else .muste.yview(.muste$scry,"scroll",delta,"units")
	.muste$oldeventtime<-as.numeric(t)
#	.muste$mousewheeltime<-as.integer(9999)
	}

.muste.mousewheelpos <- function(t,X,Y,s)
	{
	.muste.mousewheel(t,X,Y,120,s)
	}

.muste.mousewheelneg <- function(t,X,Y,s)
	{
	.muste.mousewheel(t,X,Y,-120,s)
	}

.muste.system <- function(komento,odotus=FALSE)
  {
  if (.muste$sysname=="Windows")
  	{
#  	komento <- gsub("\t","\\",komento,fixed=TRUE) 
  	shell(komento,wait=odotus)
  	}
  else
  	{
  	komento <- gsub("\\","/",komento,fixed=TRUE) 
  	system(komento,wait=odotus)
  	}
  }

.muste.systemopen <- function(komento=".",odotus=FALSE,paran=1)
  {
  if (substr(komento,1,4)=="www.") cat(paste("http://",komento,sep=""))
    
  if (.muste$sysname=="Windows")
  	{
  	if (paran==1) 
  		{
  		komento2 <- gsub("\"","",komento,fixed=TRUE)
  		if (substr(komento2,1,7)=="http://" ||
  		    substr(komento2,1,8)=="https://" ||
  		    substr(komento2,1,6)=="ftp://")
  		    {
  		    komento <- paste("start",komento2,sep=" ")
  		    }
  		else
  			{
 			ismis<-is.na(file.info(komento2)$isdir) 
 			if (ismis) if (substr(komento2,nchar(komento2)-1,nchar(komento2))==":\\") ismis<-as.integer(2)
			if (ismis==0 || ismis==2) if (ismis==2 || file.info(komento2)$isdir)
				{
				komento <- paste("explorer",komento2,sep=" ")
				}
  			}
  		}
#  	cat(komento)		
  	shell(komento,wait=odotus)
  	}
  else
  	{
  	komento <- gsub("\\","/",komento,fixed=TRUE) 
  	
  	if (.muste$sysname=="Darwin") komento <- paste("open",komento,sep=" ")
  	else komento <- paste("xdg-open",komento,sep=" ")

  	system(komento,wait=odotus)
  	}
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


#z <- function () { cat("Hello you!\n"); .id <<- tcl("after", 1000, z)}
#.id <<- tcl("after", 1000, z)
#tcl("after", "info", .id)   # To get info about this scheduled task
#tcl("after", "cancel", .id) # To cancel the currently scheduled task
  
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
  	cat("Please return to normal editorial mode in Muste!\n")
  	.muste$interrupt<-1
  	}
  else if (.muste$interrupt==1)	
  	{
  	cat("Trying to force Muste back to normal state!\n")
  	.muste$interrupt<-2
    .Call("Muste_Command","Restore",PACKAGE="muste")	    	
  	}
  else if (.muste$interrupt==2)	
  	{
  	cat("Beware! Next break will shut down Muste and R!\n")
  	.muste$interrupt<-3
  	}  	
  else if (.muste$interrupt>2)
  	{
#  	.muste$interrupt<-0
  	cat("Saving the edit field!\n")
    .Call("Muste_Command","SaveEdt",PACKAGE="muste")	    	
  	.muste.end()
  	cat("Emergency shut down for Muste and R!\n")
  	quit(save="no",status=1)
  	}	
  }
#  , finally = { cat("Finalizing\n") }
  )  
  }

#.muste.runsourcefile <- function()
#  {
#  source(.muste$runsourcefile,echo=TRUE,print.eval=TRUE)
#  }

.muste.runsource <- function(file,dest=NULL,echo=TRUE,print.eval=TRUE)
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
   source(file,echo=echo,print.eval=print.eval)     
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
#  .muste$runsourcefile<-file  
#  tcl("after",100,.muste.runsourcefile)
  }
	

.muste.focus.editor <- function()
	{
	tkfocus("-force",.muste$txt)
	tkfocus(.muste$txt)
#    if (.muste$sysname!="Windows")
#	  {
#      .muste.getwindowdim()
#      tkwm.withdraw(.muste$ikkuna)
#      tkwm.deiconify(.muste$ikkuna)
#      resi<-sprintf("+%d+%d",.muste$window.topx,.muste$window.topy)
#      tcl("wm","geometry",.muste$ikkuna,resi)
#      }
	}

.muste.scale <- function()
	{
	plot_id<-.muste$plotid
    .muste$winsize2<-as.numeric(unlist(strsplit(as.character(tkwm.geometry(.muste$plotwin[[plot_id]])),"x|\\+")))	
#	if (.muste$winsize[1]!=.muste$winsize2[1] || .muste$winsize[2]!=.muste$winsize2[2])
#		{
#		tcl("after",500,.muste.scale)
#		return()
#		}
	
    cansize<-as.numeric(unlist(strsplit(as.character(tkwinfo("geometry",.muste$canvas[[plot_id]])),"x|\\+")))
	tkconfigure(.muste$canvas[[plot_id]],"-width",.muste$winsize[1]-2,"-height",.muste$winsize[2]-2)
    xscalefactor<-.muste$winsize2[1]/cansize[1]
	yscalefactor<-.muste$winsize2[2]/cansize[2]
	tcl(.muste$canvas[[plot_id]],"scale","all","0","0",xscalefactor,yscalefactor)
	.muste$scale.lock<-FALSE
	}

.muste.canvas.windif <- function(plot_id)
	{
#	tkconfigure(.muste$canvas[[plot_id]],"-state","disabled")
    tcl("update")
    tcl("update","idletasks")	
#    cansize<-as.numeric(unlist(strsplit(as.character(tkwinfo("geometry",.muste$canvas[[plot_id]])),"x|\\+")))
#cat("\ncansize:",cansize)
	winsize<-as.numeric(unlist(strsplit(as.character(tkwm.geometry(.muste$plotwin[[plot_id]])),"x|\\+")))
    .muste$plotwinsize[[plot_id]][[1]]<-winsize[1]
    .muste$plotwinsize[[plot_id]][[2]]<-winsize[2]
#cat("\nwinsize:",winsize)
#    .muste$canwindif <- winsize-cansize
#    tkconfigure(.muste$canvas[[plot_id]],"-state","normal")	       
    }


.muste.canvas.scale <- function(W)
	{
	if (.muste$scale.lock) return()
	.muste$scale.lock<-TRUE
	plot_id<-1
    pit<-length(unlist(strsplit(.Tk.ID(.muste$plotwin[[1]]),"\\.")))
	Wpit<-length(unlist(strsplit(W,"\\.")))
	if (pit!=Wpit) { .muste$scale.lock<-FALSE; return() }	
#cat("\n",plot_id,pit,Wpit,W,.Tk.ID(.muste$plotwin[[1]]))	
	
	while (W != .Tk.ID(.muste$plotwin[[plot_id]])) 
		{ 
		plot_id<-plot_id+1
		}
#	.muste$scale.lock<-TRUE
	tkconfigure(.muste$canvas[[plot_id]],"-state","disabled")
    tcl("update")
    tcl("update","idletasks")	  		
    .muste$plotid<-plot_id
    cansize<-as.numeric(unlist(strsplit(as.character(tkwinfo("geometry",.muste$canvas[[plot_id]])),"x|\\+")))
    .muste$winsize<-as.numeric(unlist(strsplit(as.character(tkwm.geometry(.muste$plotwin[[plot_id]])),"x|\\+")))
	tkconfigure(.muste$canvas[[plot_id]],"-width",.muste$winsize[1]-2,"-height",.muste$winsize[2]-2)	
    xscalefactor<-.muste$winsize[1]/.muste$plotwinsize[[plot_id]][[1]] #cansize[1]
	yscalefactor<-.muste$winsize[2]/.muste$plotwinsize[[plot_id]][[2]] #cansize[2]
	.muste$plotwinsize[[plot_id]][[1]] <- .muste$winsize[1]
	.muste$plotwinsize[[plot_id]][[2]] <- .muste$winsize[2]
	tcl(.muste$canvas[[plot_id]],"scale","all","0","0",xscalefactor,yscalefactor)
    tcl("update")
    tcl("update","idletasks")	
	.muste$scale.lock<-FALSE
	tkconfigure(.muste$canvas[[plot_id]],"-state","normal")	       
	}
	
#.muste.mpchangebase <- function(instr,inbase,outbase)
#  {
#     mpfrvalue<-mpfr(instr,base=inbase)
#     mpfrstrvalue<-formatMpfr(mpfrvalue,drop0trailing=TRUE)
#     mpbigzvalue<-as.bigz(mpfrstrvalue)
#     return(as.character(mpbigzvalue,b=outbase))
#   }       

.muste.getclipboard <- function()
  {
tryCatch(
  {
  if (.muste$sysname=="Windows") { clipb<-try(paste(readClipboard(),collapse="\n")) }
  else { clipb<-try(tclvalue(tcl("clipboard","get"))) }
  }, 
  interrupt = function(inter) { 
  cat("Clipboard empty!\n")
  }
#  , finally = { cat("Finalizing\n") }
  )  
   
#  .muste$clipboard<-tcl("clipboard","get")
#  clipb<-try(tclvalue(tcl("clipboard","get")))
  .muste$clipboard<-ifelse(class(clipb)=="try-error",as.character(""),enc2utf8(as.character(clipb)))
  .muste$clipboardlen<-as.integer(nchar(.muste$clipboard, type = "bytes", allowNA = TRUE))
  }
 
.muste.putclipboard <- function(leike="")
	{
tryCatch(
  {	 
	  if (.muste$sysname=="Windows")
	  	{
	  	leike <- gsub("\n","\r\n",as.character(leike),fixed=TRUE) 
	  	writeClipboard(as.character(leike))
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
  
  
.muste.getcursor <- function()
  {
  apu<-as.numeric(unlist(strsplit(as.character(tkindex(.muste$txt,"insert")),"\\.")))
  .muste$cursor.row<-as.integer(apu[1])
  .muste$cursor.col<-as.integer(apu[2])
  }

.muste.getwindowdim <- function()
  
   {
  
   apu<-unlist(strsplit(as.character(tkwm.geometry(.muste$ikkuna)),"x|\\+"))
  .muste$window.vwidth<-as.integer(apu[1])
  .muste$window.vheight<-as.integer(apu[2])
  .muste$window.topx<-as.integer(apu[3])
  .muste$window.topy<-as.integer(apu[4])
  .muste$window.vtopx <- as.integer(tkwinfo("rootx",.muste$txt))
  .muste$window.vtopy <- as.integer(tkwinfo("rooty",.muste$txt))
  .muste$window.xframe <- as.integer(.muste$window.vtopx - .muste$window.topx)
  .muste$window.yframe <- as.integer(.muste$window.xframe) # same as xframe
  .muste$window.caption <- as.integer(.muste$window.vtopy-.muste$window.topy)
  .muste$window.bottomx<-as.integer(.muste$window.vwidth+.muste$window.topx+2*.muste$window.xframe)
  .muste$window.bottomy<-as.integer(.muste$window.vheight+.muste$window.topy+.muste$window.caption+.muste$window.yframe)
  }

.muste.getscreendim <- function()
  {
  .muste$screen.width<-as.integer(tkwinfo("screenwidth",.muste$ikkuna))
  .muste$screen.height<-as.integer(tkwinfo("screenheight",.muste$ikkuna))
  .muste$screen.widthmm<-as.integer(tkwinfo("screenmmwidth",.muste$ikkuna))
  .muste$screen.heightmm<-as.integer(tkwinfo("screenmmheight",.muste$ikkuna))
  }

.muste.pixelstoinches <- function(x,y)
  {
  .muste.getscreendim()
  .muste$xinches<-x*.muste$screen.widthmm/.muste$screen.width/25.4
  .muste$yinches<-y*.muste$screen.heightmm/.muste$screen.height/25.4
#  x/as.numeric(tkwinfo("fpixels",.muste$ikkuna,"1i"))
#  y/as.numeric(tkwinfo("fpixels",.muste$ikkuna,"1i"))
  }

.muste.findfontsize <- function(orgfont=.muste$font,x=12,y=20)
  {
  f<-paste(tkfont.actual(orgfont))
  .muste$sizefont <- tkfont.create(f[1],f[2],f[3],"1",f[5],f[6],f[7],f[8],f[9],f[10],f[11],f[12])
  .muste.getfontdim(font=.muste$sizefont)

  i<-1
  while (.muste$font.width < x || .muste$font.height < y)
    {
    tkfont.configure(.muste$sizefont,"-size",paste(i))
    .muste.getfontdim(font=.muste$sizefont)
    i<-i+1
    }
  .muste$font.size<-as.integer(tkfont.actual(.muste$sizefont,"-size"))  
  
  tkfont.delete(.muste$sizefont)	
  }

.muste.getfontdim <- function(text="R",font=.muste$font)
  {
  .muste$font.width<-as.integer(tkfont.measure(font,text))
  .muste$font.height<-as.integer(tkfont.metrics(font,"-linespace"))
   }

.muste.choosefont <- function()
  
   {

   valittu<-as.character(tcl("choosefont::choosefont", fonttype="fixed"))
   valittu[2]<-paste("{",valittu[2],"}",sep="")
argumentit<-paste(as.character(valittu),collapse=" ")
   komento <- paste("font configure",as.character(.muste$font),argumentit,sep=" ")
   .Tcl(komento)
  }



.muste.getmouse <- function()
  {
  apu<-as.numeric(unlist(strsplit(as.character(tkindex(.muste$txt,"current")),"\\.")))
  .muste$mouse.row<-as.integer(apu[1])
  .muste$mouse.col<-as.integer(apu[2])
  }
  
.muste.getmouse2 <- function(x=0,y=0) # Get index from pixel-coordinates
  {
  apu<-as.numeric(unlist(strsplit(as.character(tkindex(.muste$txt,paste("@",x,",",y,sep=""))),"\\.")))
  .muste$mouse.row<-as.integer(apu[1])
  .muste$mouse.col<-as.integer(apu[2])
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
    	if (latinchar[1]==63 && latinchar[2]==63 && latinchar[3]==63)
    		{
    		nonascii <- FALSE
  	  		N <- as.integer(213)
    		}
    	}

  if ((as.integer(s)==8192 || as.integer(s)==8194) && (nonascii || identical(K,"Delete") ) )
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
  
  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(1)  # KEY_EVENT
  .muste$key.char<-.muste$inchar
  .muste$key.keysym<-as.integer(N)
  .muste$key.status<-as.integer(s)

invisible(.Call("Muste_Eventloop",.muste$eventloopargs,PACKAGE="muste"))
#cat("Merkki:",A,.muste$key.keysym,k,t,s,"\n")
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

  .muste$event.time<-as.integer(t)
  .muste$event.type<-as.integer(2)  # MOUSE_EVENT
  .muste.getmouse()

  if (as.integer(T)!=4) b<-0
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

.muste.command <- function(command="Exit")
	{
	.Call("Muste_Command",command,PACKAGE="muste")
	}

.muste.resize <- function(cols,rows)
  {
  tkconfigure(.muste$txt,width=cols,height=rows)
  tkdelete(.muste$txt,"1.0","end")
  tyhjarivi <- paste(c(rep(" ",cols),"\n"),sep="",collapse="")
  tyhjaruutu <- paste(rep(tyhjarivi,rows),sep="",collapse="")
  tkinsert(.muste$txt,"end",tyhjaruutu) 
  }

.muste.init.bindings <- function()
{
  tkbind(.muste$txt,"<KeyPress>",.muste.keypress)
  tkbind(.muste$txt,"<KeyRelease>",.muste.keyrelease)

  tkbind(.muste$txt,"<Control-End>",.muste.specialkeypress)

tkbind(.muste$txt,"<Alt-Right>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-Left>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-Down>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-Up>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-Right>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-Left>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-Down>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-Up>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F1>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F2>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F3>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F4>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F5>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F6>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F7>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F8>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F9>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F10>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F11>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-F12>",.muste.specialkeypress)

tkbind(.muste$txt,"<Control-F7>",.muste.specialkeypress_ctrl)
tkbind(.muste$txt,"<Control-F11>",.muste.specialkeypress_ctrl)
tkbind(.muste$txt,"<Control-F12>",.muste.specialkeypress_ctrl)
tkbind(.muste$txt,"<Shift-F11>",.muste.specialkeypress_shift)
tkbind(.muste$txt,"<Shift-F12>",.muste.specialkeypress_shift)

tkbind(.muste$txt,"<Alt-KeyPress-e>",.muste.specialkeypress_euro)
tkbind(.muste$txt,"<Alt-KeyPress-E>",.muste.specialkeypress_euro)
tkbind(.muste$txt,"<Control-KeyPress-R>",.muste.specialkeypress) # Activate R
tkbind(.muste$txt,"<Control-KeyPress-r>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-KeyPress-V>",.muste.specialkeypress) # Paste
tkbind(.muste$txt,"<Control-KeyPress-v>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-Shift-KeyPress-V>",.muste.specialkeypress_ctrl) # Paste
tkbind(.muste$txt,"<Control-Shift-KeyPress-v>",.muste.specialkeypress_ctrl)
tkbind(.muste$txt,"<Control-KeyPress-C>",.muste.specialkeypress) # Copy selected
tkbind(.muste$txt,"<Control-KeyPress-c>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-KeyPress-X>",.muste.specialkeypress) # Cut selected
tkbind(.muste$txt,"<Control-KeyPress-x>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-KeyPress-Z>",.muste.specialkeypress) # Undo
tkbind(.muste$txt,"<Control-KeyPress-z>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-Shift-KeyPress-Z>",.muste.specialkeypress_ctrl) # Redo
tkbind(.muste$txt,"<Control-Shift-KeyPress-z>",.muste.specialkeypress_ctrl)
tkbind(.muste$txt,"<Control-Shift-KeyPress-X>",.muste.specialkeypress_ctrl) # Cut selected
tkbind(.muste$txt,"<Control-Shift-KeyPress-x>",.muste.specialkeypress_ctrl)
tkbind(.muste$txt,"<Control-KeyPress-A>",.muste.specialkeypress) # Beginning of line
tkbind(.muste$txt,"<Control-KeyPress-a>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-KeyPress-D>",.muste.specialkeypress) # Delete
tkbind(.muste$txt,"<Control-KeyPress-d>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-KeyPress-E>",.muste.specialkeypress) # End of line
tkbind(.muste$txt,"<Control-KeyPress-e>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-KeyPress-K>",.muste.specialkeypress) # Kill-line (ctrl+end or alt+f10)
tkbind(.muste$txt,"<Control-KeyPress-k>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-KeyPress-O>",.muste.specialkeypress) # Open-line (F6 or alt+f9)
tkbind(.muste$txt,"<Control-KeyPress-o>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-KeyPress-M>",.muste.specialkeypress) # Open-line (F6 or alt+f9)
tkbind(.muste$txt,"<Control-KeyPress-m>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-KeyPress-S>",.muste.specialkeypress) # Save edit field
tkbind(.muste$txt,"<Control-KeyPress-s>",.muste.specialkeypress)

tkbind(.muste$txt,"<Meta-KeyPress-R>",.muste.specialkeypress)
tkbind(.muste$txt,"<Meta-KeyPress-r>",.muste.specialkeypress)
tkbind(.muste$txt,"<Meta-KeyPress-V>",.muste.specialkeypress)
tkbind(.muste$txt,"<Meta-KeyPress-v>",.muste.specialkeypress)
tkbind(.muste$txt,"<Meta-KeyPress-C>",.muste.specialkeypress)
tkbind(.muste$txt,"<Meta-KeyPress-c>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-Delete>",.muste.specialkeypress)
tkbind(.muste$txt,"<Alt-Insert>",.muste.specialkeypress)
tkbind(.muste$txt,"<Control-Insert>",.muste.specialkeypress_ctrl)
tkbind(.muste$txt,"<Shift-Insert>",.muste.specialkeypress_shift)
tkbind(.muste$txt,"<Shift-Return>",.muste.specialkeypress_shift)
tkbind(.muste$txt,"<Shift-BackSpace>",.muste.specialkeypress_shift)
tkbind(.muste$txt,"<Alt-1>",.muste.mousealtbuttonevent) # Does not work for Mac
tkbind(.muste$txt,"<ButtonPress>",.muste.mouseevent)
tkbind(.muste$txt,"<ButtonRelease-1>",.muste.mousebuttonreleaseevent)
tkbind(.muste$txt,"<Double-ButtonPress>",.muste.doublemouseevent)
tkbind(.muste$txt,"<Motion>",.muste.mouseevent)
#tkbind(.muste$txt,"<B1-Motion>",.muste.mousebuttonmotionevent)
tkbind(.muste$txt,"<MouseWheel>",.muste.mousewheel) # Windows only
#tkbind(.muste$txt,"<Shift-MouseWheel>",.muste.mousewheel)
tkbind(.muste$txt,"<Button-4>",.muste.mousewheelpos)  # Mousewheel for mac
tkbind(.muste$txt,"<Button-5>",.muste.mousewheelneg)  # Mousewheel for mac

#tkbind(.muste$txt,"<Option-F1>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Option-F2>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Option-F3>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Option-F4>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Option-F5>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Option-F6>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Option-F7>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Option-F8>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Option-F9>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Option-F10>",.muste.specialkeypress)

#tkbind(.muste$txt,"<Command-F1>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Command-F2>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Command-F3>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Command-F4>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Command-F5>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Command-F6>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Command-F7>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Command-F8>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Command-F9>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Command-F10>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F1>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F2>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F3>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F4>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F5>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F6>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F7>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F8>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F9>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-F10>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-1>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-2>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-3>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-4>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-5>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-6>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-7>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-8>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-9>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Alt-KeyPress-0>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-1>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-2>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-3>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-4>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-5>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-6>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-7>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-8>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-9>",.muste.specialkeypress)
#tkbind(.muste$txt,"<Control-KeyPress-0>",.muste.specialkeypress)

#tkbind(.muste$plotwin[[1]],"<Configure>",.muste.canvas.scale)

#tkbind(txt, "<Button-3>",RightClick)
}

.muste.shadows <- function(color="snow",bgcolor="B0B0B0")
	{
#	tktag.configure(.muste$txt,"shadow10",background="red",foreground="black",font=.muste$fakefont)
	
	tktag.configure(.muste$txt,"shadow0",background=bgcolor,foreground="black")
	tktag.configure(.muste$txt,"shadow1",background=bgcolor,foreground="red")
	tktag.configure(.muste$txt,"shadow2",background=bgcolor,foreground="darkgrey") # line numbers
	tktag.configure(.muste$txt,"shadow3",background=bgcolor,foreground="blue")
	tktag.configure(.muste$txt,"shadow4",background="darkblue",foreground="grey")
	tktag.configure(.muste$txt,"shadow5",background="yellow",foreground="black")
	tktag.configure(.muste$txt,"shadow6",background=bgcolor,foreground="forest green") # changed to dark
	tktag.configure(.muste$txt,"shadow7",background="blue",foreground="white")
	tktag.configure(.muste$txt,"shadow8",background="darkblue",foreground="yellow")
	tktag.configure(.muste$txt,"shadow9",background=bgcolor,foreground="darkgrey")
	
	tktag.configure(.muste$txt,"shadow32",background=color,foreground="black")
	tktag.configure(.muste$txt,"shadow49",background=color,foreground="red")
	tktag.configure(.muste$txt,"shadow50",background=color,foreground="darkgrey") 
	tktag.configure(.muste$txt,"shadow51",background=color,foreground="blue")
	tktag.configure(.muste$txt,"shadow52",background="darkblue",foreground="grey")
	tktag.configure(.muste$txt,"shadow53",background="yellow",foreground="black")
	tktag.configure(.muste$txt,"shadow54",background=color,foreground="forest green") # changed to dark
	tktag.configure(.muste$txt,"shadow55",background="blue",foreground="white")
	tktag.configure(.muste$txt,"shadow56",background="darkblue",foreground="yellow")
	tktag.configure(.muste$txt,"shadow57",background=color,foreground="darkgrey")
	}

.muste.init <- function()
  {
  .muste$environment <- environment()
  
  .muste$writeaccess<-as.integer(1)
  if(file.access(system.file(package="muste"),mode=2)==-1) .muste$writeaccess<-as.integer(0) 

  tcl("source", file.path(.muste$libname,.muste$pkgname,"tklibs","choosefont.tcl"))
    
  .muste$ikkuna <- tktoplevel()
  tcl("wm", "protocol", .muste$ikkuna, "WM_DELETE_WINDOW", quote(.muste.command("Exit")))
  #quote(cat("Use F8 to exit!\n"))) 

#  tcl("wm", "resizable", .muste$ikkuna, "FALSE", "FALSE")
  tkwm.resizable(.muste$ikkuna, FALSE, FALSE)

  tkwm.title(.muste$ikkuna, "Muste")

# R.version$platform
#.Platform$OS.type  "unix" or "windows"
  .muste$sysname<-unlist(Sys.info()["sysname"])[[1]]
  .muste$startdir <- getwd()
  setwd(R.home())
  .muste$Rhome<-getwd()
  .muste$homedir<-normalizePath("~")
  setwd(.muste$startdir)
  .muste$Rtempdir <- tempdir()
  .muste$mustepath <- system.file(package="muste")
  .muste$OS.type<-.Platform$OS.type
  if (.muste$sysname=="Darwin") { .muste$font <- tkfont.create(family="Menlo",size=14) }
  else if (.muste$sysname=="Windows")
  	{ 
  	.muste$font <- tkfont.create(family="Lucida Console",size=12)
#  	.muste$menu<-tkmenu(.muste$ikkuna)
#	tkconfigure(.muste$ikkuna,menu=.muste$menu)
#	tkadd(.muste$menu, "cascade", label="Muste")
  	}
  else { .muste$font <- tkfont.create(family="Courier",size=12) }

#  .muste$fakefont <- tkfont.create(family="Courier",size=4)

#  .muste.menu()   
  .muste$txt <- tktext(.muste$ikkuna,width=80,height=25,foreground="#000000",background="snow",
                            wrap="none",font=.muste$font,undo=FALSE)
  tkgrid(.muste$txt,sticky="nw")  
#  tkinsert(.muste$txt,"1.1","Initializing Tcl/Tk")
  
  .muste$window<-.Tk.ID(.muste$txt)

  Sys.sleep(1);
  # Poistetaan text-widgetin perussidokset
  sidokset <- gsub("Text ","",tclvalue(tkbindtags(.muste$txt)))
  tkbindtags(.muste$txt,sidokset)
  
  tcl("bind","all","<Key-F10>","")
  tcl("bind","all","<Alt-Key>","")
  tcl("bind","Menubutton","<Key-F10>","")
  tcl("bind","Menubutton","<Alt-Key>","")

if (.muste$sysname!="Windows") { tcl("clipboard","clear") }
#tcl("clipboard","append","")

  .muste$key.status<-as.integer(0)
  .muste$scale.lock<-FALSE

  .muste.resize(80,25)
  .muste.getwindowdim()

.muste.statusbar(init=TRUE)

tktag.configure(.muste$txt,"shadow33",background="darkblue",foreground="darkblue")
tktag.configure(.muste$txt,"shadow34",background="darkblue",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow35",background="darkblue",foreground="cyan4")
tktag.configure(.muste$txt,"shadow36",background="darkblue",foreground="darkred")
tktag.configure(.muste$txt,"shadow37",background="darkblue",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow38",background="darkblue",foreground="yellow4")
tktag.configure(.muste$txt,"shadow39",background="darkblue",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow40",background="darkblue",foreground="grey")
tktag.configure(.muste$txt,"shadow41",background="darkblue",foreground="blue")
tktag.configure(.muste$txt,"shadow42",background="darkblue",foreground="green")
tktag.configure(.muste$txt,"shadow43",background="darkblue",foreground="cyan")
tktag.configure(.muste$txt,"shadow44",background="darkblue",foreground="red")
tktag.configure(.muste$txt,"shadow45",background="darkblue",foreground="magenta")
tktag.configure(.muste$txt,"shadow46",background="darkblue",foreground="yellow")
tktag.configure(.muste$txt,"shadow47",background="darkblue",foreground="white")
tktag.configure(.muste$txt,"shadow48",background="darkgreen",foreground="black")

.muste.shadows("#FFFEFE","#FFFEFE")

tktag.configure(.muste$txt,"shadow58",background="darkgreen",foreground="green")
tktag.configure(.muste$txt,"shadow59",background="darkgreen",foreground="cyan")
tktag.configure(.muste$txt,"shadow60",background="darkgreen",foreground="red")
tktag.configure(.muste$txt,"shadow61",background="darkgreen",foreground="magenta")
tktag.configure(.muste$txt,"shadow62",background="darkgreen",foreground="yellow")
tktag.configure(.muste$txt,"shadow63",background="darkgreen",foreground="white")

tktag.configure(.muste$txt,"shadow64",background="cyan4",foreground="black")
tktag.configure(.muste$txt,"shadow65",background="cyan4",foreground="darkblue")
tktag.configure(.muste$txt,"shadow66",background="cyan4",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow67",background="cyan4",foreground="cyan4")
tktag.configure(.muste$txt,"shadow68",background="cyan4",foreground="darkred")
tktag.configure(.muste$txt,"shadow69",background="cyan4",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow70",background="cyan4",foreground="yellow4")
tktag.configure(.muste$txt,"shadow71",background="cyan4",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow72",background="cyan4",foreground="grey")
tktag.configure(.muste$txt,"shadow73",background="cyan4",foreground="blue")
tktag.configure(.muste$txt,"shadow74",background="cyan4",foreground="green")
tktag.configure(.muste$txt,"shadow75",background="cyan4",foreground="cyan")
tktag.configure(.muste$txt,"shadow76",background="cyan4",foreground="red")
tktag.configure(.muste$txt,"shadow77",background="cyan4",foreground="magenta")
tktag.configure(.muste$txt,"shadow78",background="cyan4",foreground="yellow")
tktag.configure(.muste$txt,"shadow79",background="cyan4",foreground="white")

tktag.configure(.muste$txt,"shadow80",background="darkred",foreground="black")
tktag.configure(.muste$txt,"shadow81",background="darkred",foreground="darkblue")
tktag.configure(.muste$txt,"shadow82",background="darkred",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow83",background="darkred",foreground="cyan4")
tktag.configure(.muste$txt,"shadow84",background="darkred",foreground="darkred")
tktag.configure(.muste$txt,"shadow85",background="darkred",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow86",background="darkred",foreground="yellow4")
tktag.configure(.muste$txt,"shadow87",background="darkred",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow88",background="darkred",foreground="grey")
tktag.configure(.muste$txt,"shadow89",background="darkred",foreground="blue")
tktag.configure(.muste$txt,"shadow90",background="darkred",foreground="green")
tktag.configure(.muste$txt,"shadow91",background="darkred",foreground="cyan")
tktag.configure(.muste$txt,"shadow92",background="darkred",foreground="red")
tktag.configure(.muste$txt,"shadow93",background="darkred",foreground="magenta")
tktag.configure(.muste$txt,"shadow94",background="darkred",foreground="yellow")
tktag.configure(.muste$txt,"shadow95",background="darkred",foreground="white")

tktag.configure(.muste$txt,"shadow96",background="darkmagenta",foreground="black")
tktag.configure(.muste$txt,"shadow97",background="darkmagenta",foreground="darkblue")
tktag.configure(.muste$txt,"shadow98",background="darkmagenta",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow99",background="darkmagenta",foreground="cyan4")
tktag.configure(.muste$txt,"shadow100",background="darkmagenta",foreground="darkred")
tktag.configure(.muste$txt,"shadow101",background="darkmagenta",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow102",background="darkmagenta",foreground="yellow4")
tktag.configure(.muste$txt,"shadow103",background="darkmagenta",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow104",background="darkmagenta",foreground="grey")
tktag.configure(.muste$txt,"shadow105",background="darkmagenta",foreground="blue")
tktag.configure(.muste$txt,"shadow106",background="darkmagenta",foreground="green")
tktag.configure(.muste$txt,"shadow107",background="darkmagenta",foreground="cyan")
tktag.configure(.muste$txt,"shadow108",background="darkmagenta",foreground="red")
tktag.configure(.muste$txt,"shadow109",background="darkmagenta",foreground="magenta")
tktag.configure(.muste$txt,"shadow110",background="darkmagenta",foreground="yellow")
tktag.configure(.muste$txt,"shadow111",background="darkmagenta",foreground="white")

tktag.configure(.muste$txt,"shadow112",background="yellow4",foreground="black")
tktag.configure(.muste$txt,"shadow113",background="yellow4",foreground="darkblue")
tktag.configure(.muste$txt,"shadow114",background="yellow4",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow115",background="yellow4",foreground="cyan4")
tktag.configure(.muste$txt,"shadow116",background="yellow4",foreground="darkred")
tktag.configure(.muste$txt,"shadow117",background="yellow4",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow118",background="yellow4",foreground="yellow4")
tktag.configure(.muste$txt,"shadow119",background="yellow4",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow120",background="yellow4",foreground="grey")
tktag.configure(.muste$txt,"shadow121",background="yellow4",foreground="blue")
tktag.configure(.muste$txt,"shadow122",background="yellow4",foreground="green")
tktag.configure(.muste$txt,"shadow123",background="yellow4",foreground="cyan")
tktag.configure(.muste$txt,"shadow124",background="yellow4",foreground="red")
tktag.configure(.muste$txt,"shadow125",background="yellow4",foreground="magenta")
tktag.configure(.muste$txt,"shadow126",background="yellow4",foreground="yellow")
tktag.configure(.muste$txt,"shadow127",background="yellow4",foreground="white")

tktag.configure(.muste$txt,"shadow128",background="darkgrey",foreground="black")
tktag.configure(.muste$txt,"shadow129",background="darkgrey",foreground="darkblue")
tktag.configure(.muste$txt,"shadow130",background="darkgrey",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow131",background="darkgrey",foreground="cyan4")
tktag.configure(.muste$txt,"shadow132",background="darkgrey",foreground="darkred")
tktag.configure(.muste$txt,"shadow133",background="darkgrey",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow134",background="darkgrey",foreground="yellow4")
tktag.configure(.muste$txt,"shadow135",background="darkgrey",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow136",background="darkgrey",foreground="grey")
tktag.configure(.muste$txt,"shadow137",background="darkgrey",foreground="blue")
tktag.configure(.muste$txt,"shadow138",background="darkgrey",foreground="green")
tktag.configure(.muste$txt,"shadow139",background="darkgrey",foreground="cyan")
tktag.configure(.muste$txt,"shadow140",background="darkgrey",foreground="red")
tktag.configure(.muste$txt,"shadow141",background="darkgrey",foreground="magenta")
tktag.configure(.muste$txt,"shadow142",background="darkgrey",foreground="yellow")
tktag.configure(.muste$txt,"shadow143",background="darkgrey",foreground="white")

tktag.configure(.muste$txt,"shadow144",background="grey",foreground="black")
tktag.configure(.muste$txt,"shadow145",background="grey",foreground="darkblue")
tktag.configure(.muste$txt,"shadow146",background="grey",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow147",background="grey",foreground="cyan4")
tktag.configure(.muste$txt,"shadow148",background="grey",foreground="darkred")
tktag.configure(.muste$txt,"shadow149",background="grey",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow150",background="grey",foreground="yellow4")
tktag.configure(.muste$txt,"shadow151",background="grey",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow152",background="grey",foreground="grey")
tktag.configure(.muste$txt,"shadow153",background="grey",foreground="blue")
tktag.configure(.muste$txt,"shadow154",background="grey",foreground="green")
tktag.configure(.muste$txt,"shadow155",background="grey",foreground="cyan")
tktag.configure(.muste$txt,"shadow156",background="grey",foreground="red")
tktag.configure(.muste$txt,"shadow157",background="grey",foreground="magenta")
tktag.configure(.muste$txt,"shadow158",background="grey",foreground="yellow")
tktag.configure(.muste$txt,"shadow159",background="grey",foreground="white")

tktag.configure(.muste$txt,"shadow160",background="blue",foreground="black")
tktag.configure(.muste$txt,"shadow161",background="blue",foreground="darkblue")
tktag.configure(.muste$txt,"shadow162",background="blue",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow163",background="blue",foreground="cyan4")
tktag.configure(.muste$txt,"shadow164",background="blue",foreground="darkred")
tktag.configure(.muste$txt,"shadow165",background="blue",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow166",background="blue",foreground="yellow4")
tktag.configure(.muste$txt,"shadow167",background="blue",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow168",background="blue",foreground="grey")
tktag.configure(.muste$txt,"shadow169",background="blue",foreground="blue")
tktag.configure(.muste$txt,"shadow170",background="blue",foreground="green")
tktag.configure(.muste$txt,"shadow171",background="blue",foreground="cyan")
tktag.configure(.muste$txt,"shadow172",background="blue",foreground="red")
tktag.configure(.muste$txt,"shadow173",background="blue",foreground="magenta")
tktag.configure(.muste$txt,"shadow174",background="blue",foreground="yellow")
tktag.configure(.muste$txt,"shadow175",background="blue",foreground="white")

tktag.configure(.muste$txt,"shadow176",background="green",foreground="black")
tktag.configure(.muste$txt,"shadow177",background="green",foreground="darkblue")
tktag.configure(.muste$txt,"shadow178",background="green",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow179",background="green",foreground="cyan4")
tktag.configure(.muste$txt,"shadow180",background="green",foreground="darkred")
tktag.configure(.muste$txt,"shadow181",background="green",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow182",background="green",foreground="yellow4")
tktag.configure(.muste$txt,"shadow183",background="green",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow184",background="green",foreground="grey")
tktag.configure(.muste$txt,"shadow185",background="green",foreground="blue")
tktag.configure(.muste$txt,"shadow186",background="green",foreground="green")
tktag.configure(.muste$txt,"shadow187",background="green",foreground="cyan")
tktag.configure(.muste$txt,"shadow188",background="green",foreground="red")
tktag.configure(.muste$txt,"shadow189",background="green",foreground="magenta")
tktag.configure(.muste$txt,"shadow190",background="green",foreground="yellow")
tktag.configure(.muste$txt,"shadow191",background="green",foreground="white")

tktag.configure(.muste$txt,"shadow192",background="cyan",foreground="black")
tktag.configure(.muste$txt,"shadow193",background="cyan",foreground="darkblue")
tktag.configure(.muste$txt,"shadow194",background="cyan",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow195",background="cyan",foreground="cyan4")
tktag.configure(.muste$txt,"shadow196",background="cyan",foreground="darkred")
tktag.configure(.muste$txt,"shadow197",background="cyan",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow198",background="cyan",foreground="yellow4")
tktag.configure(.muste$txt,"shadow199",background="cyan",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow200",background="cyan",foreground="grey")
tktag.configure(.muste$txt,"shadow201",background="cyan",foreground="blue")
tktag.configure(.muste$txt,"shadow202",background="cyan",foreground="green")
tktag.configure(.muste$txt,"shadow203",background="cyan",foreground="cyan")
tktag.configure(.muste$txt,"shadow204",background="cyan",foreground="red")
tktag.configure(.muste$txt,"shadow205",background="cyan",foreground="magenta")
tktag.configure(.muste$txt,"shadow206",background="cyan",foreground="yellow")
tktag.configure(.muste$txt,"shadow207",background="cyan",foreground="white")

tktag.configure(.muste$txt,"shadow208",background="red",foreground="black")
tktag.configure(.muste$txt,"shadow209",background="red",foreground="darkblue")
tktag.configure(.muste$txt,"shadow210",background="red",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow211",background="red",foreground="cyan4")
tktag.configure(.muste$txt,"shadow212",background="red",foreground="darkred")
tktag.configure(.muste$txt,"shadow213",background="red",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow214",background="red",foreground="yellow4")
tktag.configure(.muste$txt,"shadow215",background="red",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow216",background="red",foreground="grey")
tktag.configure(.muste$txt,"shadow217",background="red",foreground="blue")
tktag.configure(.muste$txt,"shadow218",background="red",foreground="green")
tktag.configure(.muste$txt,"shadow219",background="red",foreground="cyan")
tktag.configure(.muste$txt,"shadow220",background="red",foreground="red")
tktag.configure(.muste$txt,"shadow221",background="red",foreground="magenta")
tktag.configure(.muste$txt,"shadow222",background="red",foreground="yellow")
tktag.configure(.muste$txt,"shadow223",background="red",foreground="white")

tktag.configure(.muste$txt,"shadow224",background="black",foreground="black")
tktag.configure(.muste$txt,"shadow225",background="black",foreground="darkblue")
tktag.configure(.muste$txt,"shadow226",background="black",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow227",background="black",foreground="cyan4")
tktag.configure(.muste$txt,"shadow228",background="black",foreground="darkred")
tktag.configure(.muste$txt,"shadow229",background="black",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow230",background="black",foreground="yellow4")
tktag.configure(.muste$txt,"shadow231",background="black",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow232",background="black",foreground="grey")
tktag.configure(.muste$txt,"shadow233",background="black",foreground="blue")
tktag.configure(.muste$txt,"shadow234",background="black",foreground="green")
tktag.configure(.muste$txt,"shadow235",background="black",foreground="cyan")
tktag.configure(.muste$txt,"shadow236",background="black",foreground="red")
tktag.configure(.muste$txt,"shadow237",background="black",foreground="magenta")
tktag.configure(.muste$txt,"shadow238",background="black",foreground="yellow")
tktag.configure(.muste$txt,"shadow239",background="black",foreground="white")

tktag.configure(.muste$txt,"shadow240",background="yellow",foreground="black")
tktag.configure(.muste$txt,"shadow241",background="yellow",foreground="darkblue")
tktag.configure(.muste$txt,"shadow242",background="yellow",foreground="darkgreen")
tktag.configure(.muste$txt,"shadow243",background="yellow",foreground="cyan4")
tktag.configure(.muste$txt,"shadow244",background="yellow",foreground="darkred")
tktag.configure(.muste$txt,"shadow245",background="yellow",foreground="darkmagenta")
tktag.configure(.muste$txt,"shadow246",background="yellow",foreground="yellow4")
tktag.configure(.muste$txt,"shadow247",background="yellow",foreground="darkgrey")
tktag.configure(.muste$txt,"shadow248",background="yellow",foreground="grey")
tktag.configure(.muste$txt,"shadow249",background="yellow",foreground="blue")
tktag.configure(.muste$txt,"shadow250",background="yellow",foreground="green")
tktag.configure(.muste$txt,"shadow251",background="yellow",foreground="cyan")
tktag.configure(.muste$txt,"shadow252",background="yellow",foreground="red")
tktag.configure(.muste$txt,"shadow253",background="yellow",foreground="magenta")
tktag.configure(.muste$txt,"shadow254",background="yellow",foreground="yellow")
tktag.configure(.muste$txt,"shadow255",background="yellow",foreground="white")


#tktag.configure(.muste$txt,"shadow237",background="grey",foreground="grey")

#.muste$scry <- tkscrollbar(.muste$ikkuna,repeatinterval=5, command=function(...).muste.yview(.muste$txt,...))  
#.muste$scrx <- tkscrollbar(.muste$ikkuna,orient="horizontal",repeatinterval=5, command=function(...).muste.xview(.muste$txt,...))

.muste$scry <- ttkscrollbar(.muste$ikkuna, command=function(...).muste.yview(.muste$txt,...))  
.muste$scrx <- ttkscrollbar(.muste$ikkuna,orient="horizontal", command=function(...).muste.xview(.muste$txt,...))


tkfocus("-force",.muste$txt)
tkfocus(.muste$txt)
tcl("update","idletasks")
tcl("update")
}

.muste.stop <- function()
{
.muste$eventlooprun <- FALSE
#if (.muste$eventloop.after) 
#   tcl("after", "cancel", .muste$eventloopid)
#.muste$eventloop.after <- TRUE  
}

.muste.remove.bindings <- function()
	{
if (.muste$eventloop.after) 
tcl("after", "cancel", .muste$eventloopid)	
	bindvec<-unlist(strsplit(tclvalue(tkbind(.muste$txt))," "))
for(i in 1:length(bindvec)) { tkbind(.muste$txt,bindvec[i],"") }
	}

.muste.destroywindow <- function()
{
tkdestroy(.muste$txt)
tkdestroy(.muste$ikkuna)
}

.muste.end <- function()
{
.muste$termination<-as.integer(1)
.muste$eventlooprun <- FALSE
.muste$eventlooptime<-as.integer(1)
if (.muste$eventloop.after) 
tcl("after", "cancel", .muste$eventloopid)

 .muste.command("Exit")
.muste.remove.bindings()

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
  
rm(editor,envir=.muste,inherits=TRUE)
tcl("after",100,.muste.destroywindow)  
#q()
}

muste <- function(sucro="<empty>") 
{

if (exists("editor",where=.muste))
	{
	stop("Muste editor is already running! Please use sucro /Z to launch a new editor.")
	}

if (getRversion() >= "2.14.0")
	{
	requireNamespace("tcltk",quietly=TRUE)
	try(attachNamespace("tcltk"),silent=TRUE)
	#  require(tcltk)
	}
else .muste.command("Require")

 .muste$editor=TRUE
 .muste$termination<-FALSE
 .muste$Rtermination<-FALSE
 .muste$jatkuu<-as.integer(1)

.muste$eventloopargs<-"Tosi"
.muste.init()

# Initialize global variables
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
.muste$interrupt<-0
.muste$insertcursorcolor <- "#90F"
.muste$startsucro <- as.character(sucro)

.muste$event.time<-as.integer(0)
.muste$eventlooptime<-as.integer(1000)
.muste$eventlooprun<-TRUE
.muste$eventloop.after<-0
.muste$eventloop<-FALSE
.muste$apufile <- Sys.getenv("MUSTEAPU")
if (nchar(.muste$apufile)==0)
	{
	if (.muste$sysname=="Windows") .muste$apufile <- paste(.muste$homedir,'\\.muste\\muste.apu',sep="")
	else .muste$apufile <- paste(.muste$homedir,'/.muste/muste.apu',sep="")
	}
#    args<-"A"
i<-as.integer(.Call("Muste_Editor",.muste,PACKAGE="muste"))
if (i>0)
	{
	if(!file.exists(.muste$apufile)) .muste.setup(init=TRUE)
	.muste.init.bindings()
	invisible(.muste.eventloop())
	}
if (i<0) 
	{
	.muste$eventlooprun <- FALSE
	.muste.end()
	warning("Failed to initialize Muste!")
	}	
}
