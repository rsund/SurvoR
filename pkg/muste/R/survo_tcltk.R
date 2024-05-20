.muste.mousewheel <- function(t,X,Y,D,s)
	{
    tt<-100*proc.time()[[3]]	
#    cat("\n",tt,X,Y,D,s,as.numeric(tt)-.muste$oldeventtime)    
#	.muste$mousewheeltime<-as.integer(abs(as.numeric(t)-.muste$oldeventtime))
	if (as.numeric(tt)-.muste$oldeventtime<3) 
		{
		if (.muste$oldeventtime==0) .muste$oldeventtime<-as.numeric(tt)
		return()
		}
	D<-as.numeric(D)
	if (abs(D)<120)	D<-D/abs(D)*120
	delta <- -1*D/120
	if (.muste$sysname=="Windows") delta <- (abs(delta)^2)*sign(delta)
#############  mac=1   ############	 windows=9 ##########
	if (as.integer(s)==1 || as.integer(s)==9) .muste.xview(.muste$scrx,"scroll",delta,"units")
	else .muste.yview(.muste$scry,"scroll",delta,"units")
	.muste$oldeventtime<-as.numeric(tt)
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


tkbind(.muste$txt,"<Option-Right>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-Left>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-Down>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-Up>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F1>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F2>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F3>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F4>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F5>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F6>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F7>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F8>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F9>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F10>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F11>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-F12>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-KeyPress-e>",.muste.specialkeypress_euro)
tkbind(.muste$txt,"<Option-KeyPress-E>",.muste.specialkeypress_euro)
tkbind(.muste$txt,"<Option-Delete>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-Insert>",.muste.specialkeypress)
tkbind(.muste$txt,"<Option-1>",.muste.mousealtbuttonevent)

if (.muste$sysname=="Darwin") # Mac CMD keybinds RS 14.1.2014
    {
    tkbind(.muste$txt,"<Meta-KeyPress-R>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-r>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-V>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-v>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-C>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-c>",.muste.specialkeypress)    
    tkbind(.muste$txt,"<Meta-End>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-Right>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-Left>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-Down>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-Up>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-F7>",.muste.specialkeypress_ctrl)
    tkbind(.muste$txt,"<Meta-F11>",.muste.specialkeypress_ctrl)
    tkbind(.muste$txt,"<Meta-F12>",.muste.specialkeypress_ctrl)
    tkbind(.muste$txt,"<Meta-Shift-KeyPress-V>",.muste.specialkeypress_ctrl) # Paste
    tkbind(.muste$txt,"<Meta-Shift-KeyPress-v>",.muste.specialkeypress_ctrl)
    tkbind(.muste$txt,"<Meta-KeyPress-X>",.muste.specialkeypress) # Cut selected
    tkbind(.muste$txt,"<Meta-KeyPress-x>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-Z>",.muste.specialkeypress) # Undo
    tkbind(.muste$txt,"<Meta-KeyPress-z>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-Shift-KeyPress-Z>",.muste.specialkeypress_ctrl) # Redo
    tkbind(.muste$txt,"<Meta-Shift-KeyPress-z>",.muste.specialkeypress_ctrl)
    tkbind(.muste$txt,"<Meta-Shift-KeyPress-X>",.muste.specialkeypress_ctrl) # Cut selected
    tkbind(.muste$txt,"<Meta-Shift-KeyPress-x>",.muste.specialkeypress_ctrl)
    tkbind(.muste$txt,"<Meta-KeyPress-A>",.muste.specialkeypress) # Beginning of line
    tkbind(.muste$txt,"<Meta-KeyPress-a>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-D>",.muste.specialkeypress) # Delete
    tkbind(.muste$txt,"<Meta-KeyPress-d>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-E>",.muste.specialkeypress) # End of line
    tkbind(.muste$txt,"<Meta-KeyPress-e>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-K>",.muste.specialkeypress) # Kill-line (ctrl+end or alt+f10)
    tkbind(.muste$txt,"<Meta-KeyPress-k>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-O>",.muste.specialkeypress) # Open-line (F6 or alt+f9)
    tkbind(.muste$txt,"<Meta-KeyPress-o>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-M>",.muste.specialkeypress) # Open-line (F6 or alt+f9)
    tkbind(.muste$txt,"<Meta-KeyPress-m>",.muste.specialkeypress)
    tkbind(.muste$txt,"<Meta-KeyPress-S>",.muste.specialkeypress) # Save edit field
    tkbind(.muste$txt,"<Meta-KeyPress-s>",.muste.specialkeypress)   
    }


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


.muste.defaulthighlightstyle()

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

.muste.shadows("white","#F8F8F8")

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
#tktag.configure(.muste$txt,"shadow248",background="yellow",foreground="grey")
tktag.configure(.muste$txt,"shadow249",background="yellow",foreground="blue")
tktag.configure(.muste$txt,"shadow250",background="yellow",foreground="green")
tktag.configure(.muste$txt,"shadow251",background="yellow",foreground="cyan")
tktag.configure(.muste$txt,"shadow252",background="yellow",foreground="red")
tktag.configure(.muste$txt,"shadow253",background="yellow",foreground="magenta")
tktag.configure(.muste$txt,"shadow254",background="yellow",foreground="yellow")
tktag.configure(.muste$txt,"shadow255",background="yellow",foreground="white")


#tktag.configure(.muste$txt,"shadow237",background="grey",foreground="grey")


}

.muste.shadows <- function(color="snow",bgcolor="B0B0B0")
	{
#	tktag.configure(.muste$txt,"shadow10",background="red",foreground="black",font=.muste$fakefont)
	# .muste.shadows("white","#F8F8F8")
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
	
	tktag.configure(.muste$txt,"shadow9999",background=color,foreground=color)
	tktag.configure(.muste$txt,"shadow10",background=bgcolor,foreground="black",font=.muste$minifont)	
	tktag.configure(.muste$txt,"shadow248",background=color,foreground=color)
	}
	
.muste.inittkwindow <- function()
    {
    # .muste$dotTcl <- tcltk:::.C_dotTcl$address
   .muste$dotTcl <- getNativeSymbolInfo("dotTcl","tcltk")$address
   .Call("Survo_FindFunc",.muste$dotTcl)
    
  tcl("source", file.path(.muste$libname,.muste$pkgname,"tklibs","choosefont.tcl"))
    
  .muste$ikkuna <- tktoplevel()
  tcl("wm", "protocol", .muste$ikkuna, "WM_DELETE_WINDOW", quote(.muste.close()))
  #quote(cat("Use F8 to exit!\n"))) 

#  tcl("wm", "resizable", .muste$ikkuna, "FALSE", "FALSE")
  tkwm.resizable(.muste$ikkuna, FALSE, FALSE)

  tkwm.title(.muste$ikkuna, "Survo")

  .muste$minifont <- tkfont.create(family="Courier",size=1)
  if (.muste$sysname=="Darwin") { .muste$font <- tkfont.create(family="Menlo",size=14) }
  else if (.muste$sysname=="Windows")
  	{ 
  	.muste$font <- tkfont.create(family="Lucida Console",size=12)
#  	.muste$menu<-tkmenu(.muste$ikkuna)
#	tkconfigure(.muste$ikkuna,menu=.muste$menu)
#	tkadd(.muste$menu, "cascade", label="Survo")
  	}
  else { .muste$font <- tkfont.create(family="Courier",size=12) }

#  .muste$fakefont <- tkfont.create(family="Courier",size=4)

#  .muste.menu()   
  .muste$txt <- tktext(.muste$ikkuna,width=80,height=25,foreground="#000000",background="snow",
                            wrap="none",font=.muste$font,undo=FALSE)
  tkgrid(.muste$txt,row=1,sticky="nw")  
#  tkinsert(.muste$txt,"1.1","Initializing Tcl/Tk")
  
  .muste$window<-.Tk.ID(.muste$txt)
  
  Sys.sleep(1);
  # Poistetaan text-widgetin perussidokset
  sidokset <- gsub("Text ","",tclvalue(tkbindtags(.muste$txt)))
  tkbindtags(.muste$txt,sidokset)
  
  tcl("bind","all","<Key-F10>","")
  tcl("bind","all","<KeyRelease-F10>","")
  tcl("bind","all","<Alt-Key>","")
  tcl("bind","all","<Alt-KeyRelease>","")
  tcl("bind","all","<Key-Alt_R>","")
  tcl("bind","all","<KeyRelease-Alt_R>","")
  tcl("bind","all","<Key-Alt_L>","")
  tcl("bind","all","<KeyRelease-Alt_L>","")
  
  tcl("bind","Menubutton","<Key-F10>","")
  tcl("bind","Menubutton","<Alt-Key>","")

  .muste.resize(80,25)
  .muste.getwindowdim()

.muste.statusbar(init=TRUE)

#.muste$scry <- tkscrollbar(.muste$ikkuna,repeatinterval=5, command=function(...).muste.yview(.muste$txt,...))  
#.muste$scrx <- tkscrollbar(.muste$ikkuna,orient="horizontal",repeatinterval=5, command=function(...).muste.xview(.muste$txt,...))

.muste$scry <- ttkscrollbar(.muste$ikkuna, command=function(...).muste.yview(.muste$txt,...))  
.muste$scrx <- ttkscrollbar(.muste$ikkuna,orient="horizontal", command=function(...).muste.xview(.muste$txt,...))


tkfocus("-force",.muste$txt)
tkfocus(.muste$txt)
tcl("update","idletasks")
tcl("update")
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
    