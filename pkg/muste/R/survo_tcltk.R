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
	tcltk::tkfocus("-force",.muste$txt)
	tcltk::tkfocus(.muste$txt)
#    if (.muste$sysname!="Windows")
#	  {
#      .muste.getwindowdim()
#      tkwm.withdraw(.muste$ikkuna)
#      tkwm.deiconify(.muste$ikkuna)
#      resi<-sprintf("+%d+%d",.muste$window.topx,.muste$window.topy)
#      tcltk::tcl("wm","geometry",.muste$ikkuna,resi)
#      }
	}

.muste.scale <- function()
	{
	plot_id<-.muste$plotid
    .muste$winsize2<-as.numeric(unlist(strsplit(as.character(tcltk::tkwm.geometry(.muste$plotwin[[plot_id]])),"x|\\+")))	
#	if (.muste$winsize[1]!=.muste$winsize2[1] || .muste$winsize[2]!=.muste$winsize2[2])
#		{
#		tcl("after",500,.muste.scale)
#		return()
#		}
	
    cansize<-as.numeric(unlist(strsplit(as.character(tcltk::tkwinfo("geometry",.muste$canvas[[plot_id]])),"x|\\+")))
	tcltk::tkconfigure(.muste$canvas[[plot_id]],"-width",.muste$winsize[1]-2,"-height",.muste$winsize[2]-2)
    xscalefactor<-.muste$winsize2[1]/cansize[1]
	yscalefactor<-.muste$winsize2[2]/cansize[2]
	tcltk::tcl(.muste$canvas[[plot_id]],"scale","all","0","0",xscalefactor,yscalefactor)
	.muste$scale.lock<-FALSE
	}

.muste.canvas.windif <- function(plot_id)
	{
#	tkconfigure(.muste$canvas[[plot_id]],"-state","disabled")
    tcltk::tcl("update")
    tcltk::tcl("update","idletasks")	
#    cansize<-as.numeric(unlist(strsplit(as.character(tkwinfo("geometry",.muste$canvas[[plot_id]])),"x|\\+")))
#cat("\ncansize:",cansize)
	winsize<-as.numeric(unlist(strsplit(as.character(tcltk::tkwm.geometry(.muste$plotwin[[plot_id]])),"x|\\+")))
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
    pit<-length(unlist(strsplit(tcltk::.Tk.ID(.muste$plotwin[[1]]),"\\.")))
	Wpit<-length(unlist(strsplit(W,"\\.")))
	if (pit!=Wpit) { .muste$scale.lock<-FALSE; return() }	
#cat("\n",plot_id,pit,Wpit,W,.Tk.ID(.muste$plotwin[[1]]))	
	
	while (W != tcltk::.Tk.ID(.muste$plotwin[[plot_id]])) 
		{ 
		plot_id<-plot_id+1
		}
#	.muste$scale.lock<-TRUE
	tcltk::tkconfigure(.muste$canvas[[plot_id]],"-state","disabled")
    tcltk::tcl("update")
    tcltk::tcl("update","idletasks")	  		
    .muste$plotid<-plot_id
    cansize<-as.numeric(unlist(strsplit(as.character(tcltk::tkwinfo("geometry",.muste$canvas[[plot_id]])),"x|\\+")))
    .muste$winsize<-as.numeric(unlist(strsplit(as.character(tcltk::tkwm.geometry(.muste$plotwin[[plot_id]])),"x|\\+")))
	tcltk::tkconfigure(.muste$canvas[[plot_id]],"-width",.muste$winsize[1]-2,"-height",.muste$winsize[2]-2)	
    xscalefactor<-.muste$winsize[1]/.muste$plotwinsize[[plot_id]][[1]] #cansize[1]
	yscalefactor<-.muste$winsize[2]/.muste$plotwinsize[[plot_id]][[2]] #cansize[2]
	.muste$plotwinsize[[plot_id]][[1]] <- .muste$winsize[1]
	.muste$plotwinsize[[plot_id]][[2]] <- .muste$winsize[2]
	tcltk::tcl(.muste$canvas[[plot_id]],"scale","all","0","0",xscalefactor,yscalefactor)
    tcltk::tcl("update")
    tcltk::tcl("update","idletasks")	
	.muste$scale.lock<-FALSE
	tcltk::tkconfigure(.muste$canvas[[plot_id]],"-state","normal")	       
	}

.muste.getcursor <- function()
  {
  apu<-as.numeric(unlist(strsplit(as.character(tcltk::tkindex(.muste$txt,"insert")),"\\.")))
  .muste$cursor.row<-as.integer(apu[1])
  .muste$cursor.col<-as.integer(apu[2])
  }

.muste.getwindowdim <- function()
  
   {
  
   apu<-unlist(strsplit(as.character(tcltk::tkwm.geometry(.muste$ikkuna)),"x|\\+"))
  .muste$window.vwidth<-as.integer(apu[1])
  .muste$window.vheight<-as.integer(apu[2])
  .muste$window.topx<-as.integer(apu[3])
  .muste$window.topy<-as.integer(apu[4])
  .muste$window.vtopx <- as.integer(tcltk::tkwinfo("rootx",.muste$txt))
  .muste$window.vtopy <- as.integer(tcltk::tkwinfo("rooty",.muste$txt))
  .muste$window.xframe <- as.integer(.muste$window.vtopx - .muste$window.topx)
  .muste$window.yframe <- as.integer(.muste$window.xframe) # same as xframe
  .muste$window.caption <- as.integer(.muste$window.vtopy-.muste$window.topy)
  .muste$window.bottomx<-as.integer(.muste$window.vwidth+.muste$window.topx+2*.muste$window.xframe)
  .muste$window.bottomy<-as.integer(.muste$window.vheight+.muste$window.topy+.muste$window.caption+.muste$window.yframe)
  }

.muste.getscreendim <- function()
  {
  .muste$screen.width<-as.integer(tcltk::tkwinfo("screenwidth",.muste$ikkuna))
  .muste$screen.height<-as.integer(tcltk::tkwinfo("screenheight",.muste$ikkuna))
  .muste$screen.widthmm<-as.integer(tcltk::tkwinfo("screenmmwidth",.muste$ikkuna))
  .muste$screen.heightmm<-as.integer(tcltk::tkwinfo("screenmmheight",.muste$ikkuna))
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
  f<-paste(tcltk::tkfont.actual(orgfont))
  .muste$sizefont <- tcltk::tkfont.create(f[1],f[2],f[3],"1",f[5],f[6],f[7],f[8],f[9],f[10],f[11],f[12])
  .muste.getfontdim(font=.muste$sizefont)

  i<-1
  while (.muste$font.width < x || .muste$font.height < y)
    {
    tcltk::tkfont.configure(.muste$sizefont,"-size",paste(i))
    .muste.getfontdim(font=.muste$sizefont)
    i<-i+1
    }
  .muste$font.size<-as.integer(tcltk::tkfont.actual(.muste$sizefont,"-size"))  
  
  tcltk::tkfont.delete(.muste$sizefont)	
  }

.muste.getfontdim <- function(text="R",font=.muste$font)
  {
  .muste$font.width<-as.integer(tcltk::tkfont.measure(font,text))
  .muste$font.height<-as.integer(tcltk::tkfont.metrics(font,"-linespace"))
   }

.muste.choosefont <- function()
  
   {

   valittu<-as.character(tcltk::tcl("choosefont::choosefont", fonttype="fixed"))
   valittu[2]<-paste("{",valittu[2],"}",sep="")
argumentit<-paste(as.character(valittu),collapse=" ")
   komento <- paste("font configure",as.character(.muste$font),argumentit,sep=" ")
   tcltk::.Tcl(komento)
  }



.muste.getmouse <- function()
  {
  apu<-as.numeric(unlist(strsplit(as.character(tcltk::tkindex(.muste$txt,"current")),"\\.")))
  .muste$mouse.row<-as.integer(apu[1])
  .muste$mouse.col<-as.integer(apu[2])
  }
  
.muste.getmouse2 <- function(x=0,y=0) # Get index from pixel-coordinates
  {
  apu<-as.numeric(unlist(strsplit(as.character(tcltk::tkindex(.muste$txt,paste("@",x,",",y,sep=""))),"\\.")))
  .muste$mouse.row<-as.integer(apu[1])
  .muste$mouse.col<-as.integer(apu[2])
  }  

.muste.resize <- function(cols,rows)
  {
  tcltk::tkconfigure(.muste$txt,width=cols,height=rows)
  tcltk::tkdelete(.muste$txt,"1.0","end")
  tyhjarivi <- paste(c(rep(" ",cols),"\n"),sep="",collapse="")
  tyhjaruutu <- paste(rep(tyhjarivi,rows),sep="",collapse="")
  tcltk::tkinsert(.muste$txt,"end",tyhjaruutu) 
  }

.muste.init.bindings <- function()
{
  tcltk::tkbind(.muste$txt,"<KeyPress>",.muste.keypress)
  tcltk::tkbind(.muste$txt,"<KeyRelease>",.muste.keyrelease)

  tcltk::tkbind(.muste$txt,"<Control-End>",.muste.specialkeypress)

tcltk::tkbind(.muste$txt,"<Alt-Right>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-Left>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-Down>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-Up>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-Right>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-Left>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-Down>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-Up>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F1>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F2>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F3>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F4>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F5>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F6>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F7>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F8>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F9>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F10>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F11>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-F12>",.muste.specialkeypress)

tcltk::tkbind(.muste$txt,"<Control-F7>",.muste.specialkeypress_ctrl)
tcltk::tkbind(.muste$txt,"<Control-F11>",.muste.specialkeypress_ctrl)
tcltk::tkbind(.muste$txt,"<Control-F12>",.muste.specialkeypress_ctrl)
tcltk::tkbind(.muste$txt,"<Shift-F11>",.muste.specialkeypress_shift)
tcltk::tkbind(.muste$txt,"<Shift-F12>",.muste.specialkeypress_shift)

tcltk::tkbind(.muste$txt,"<Alt-KeyPress-e>",.muste.specialkeypress_euro)
tcltk::tkbind(.muste$txt,"<Alt-KeyPress-E>",.muste.specialkeypress_euro)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-R>",.muste.specialkeypress) # Activate R
tcltk::tkbind(.muste$txt,"<Control-KeyPress-r>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-V>",.muste.specialkeypress) # Paste
tcltk::tkbind(.muste$txt,"<Control-KeyPress-v>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-Shift-KeyPress-V>",.muste.specialkeypress_ctrl) # Paste
tcltk::tkbind(.muste$txt,"<Control-Shift-KeyPress-v>",.muste.specialkeypress_ctrl)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-C>",.muste.specialkeypress) # Copy selected
tcltk::tkbind(.muste$txt,"<Control-KeyPress-c>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-X>",.muste.specialkeypress) # Cut selected
tcltk::tkbind(.muste$txt,"<Control-KeyPress-x>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-Z>",.muste.specialkeypress) # Undo
tcltk::tkbind(.muste$txt,"<Control-KeyPress-z>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-Shift-KeyPress-Z>",.muste.specialkeypress_ctrl) # Redo
tcltk::tkbind(.muste$txt,"<Control-Shift-KeyPress-z>",.muste.specialkeypress_ctrl)
tcltk::tkbind(.muste$txt,"<Control-Shift-KeyPress-X>",.muste.specialkeypress_ctrl) # Cut selected
tcltk::tkbind(.muste$txt,"<Control-Shift-KeyPress-x>",.muste.specialkeypress_ctrl)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-A>",.muste.specialkeypress) # Beginning of line
tcltk::tkbind(.muste$txt,"<Control-KeyPress-a>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-D>",.muste.specialkeypress) # Delete
tcltk::tkbind(.muste$txt,"<Control-KeyPress-d>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-E>",.muste.specialkeypress) # End of line
tcltk::tkbind(.muste$txt,"<Control-KeyPress-e>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-K>",.muste.specialkeypress) # Kill-line (ctrl+end or alt+f10)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-k>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-O>",.muste.specialkeypress) # Open-line (F6 or alt+f9)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-o>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-M>",.muste.specialkeypress) # Open-line (F6 or alt+f9)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-m>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-KeyPress-S>",.muste.specialkeypress) # Save edit field
tcltk::tkbind(.muste$txt,"<Control-KeyPress-s>",.muste.specialkeypress)

tcltk::tkbind(.muste$txt,"<Alt-Delete>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Alt-Insert>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Control-Insert>",.muste.specialkeypress_ctrl)
tcltk::tkbind(.muste$txt,"<Shift-Insert>",.muste.specialkeypress_shift)
tcltk::tkbind(.muste$txt,"<Shift-Return>",.muste.specialkeypress_shift)
tcltk::tkbind(.muste$txt,"<Shift-BackSpace>",.muste.specialkeypress_shift)
tcltk::tkbind(.muste$txt,"<Alt-1>",.muste.mousealtbuttonevent) # Does not work for Mac
tcltk::tkbind(.muste$txt,"<ButtonPress>",.muste.mouseevent)
tcltk::tkbind(.muste$txt,"<ButtonRelease-1>",.muste.mousebuttonreleaseevent)
tcltk::tkbind(.muste$txt,"<Double-ButtonPress>",.muste.doublemouseevent)
tcltk::tkbind(.muste$txt,"<Motion>",.muste.mouseevent)
#tcltk::tkbind(.muste$txt,"<B1-Motion>",.muste.mousebuttonmotionevent)
tcltk::tkbind(.muste$txt,"<MouseWheel>",.muste.mousewheel) # Windows only
#tcltk::tkbind(.muste$txt,"<Shift-MouseWheel>",.muste.mousewheel)
tcltk::tkbind(.muste$txt,"<Button-4>",.muste.mousewheelpos)  # Mousewheel for mac
tcltk::tkbind(.muste$txt,"<Button-5>",.muste.mousewheelneg)  # Mousewheel for mac


tcltk::tkbind(.muste$txt,"<Option-Right>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-Left>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-Down>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-Up>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F1>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F2>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F3>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F4>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F5>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F6>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F7>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F8>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F9>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F10>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F11>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-F12>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-KeyPress-e>",.muste.specialkeypress_euro)
tcltk::tkbind(.muste$txt,"<Option-KeyPress-E>",.muste.specialkeypress_euro)
tcltk::tkbind(.muste$txt,"<Option-Delete>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-Insert>",.muste.specialkeypress)
tcltk::tkbind(.muste$txt,"<Option-1>",.muste.mousealtbuttonevent)

if (.muste$sysname=="Darwin") # Mac CMD keybinds RS 14.1.2014
    {
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-R>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-r>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-V>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-v>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-C>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-c>",.muste.specialkeypress)    
    tcltk::tkbind(.muste$txt,"<Meta-End>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-Right>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-Left>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-Down>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-Up>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-F7>",.muste.specialkeypress_ctrl)
    tcltk::tkbind(.muste$txt,"<Meta-F11>",.muste.specialkeypress_ctrl)
    tcltk::tkbind(.muste$txt,"<Meta-F12>",.muste.specialkeypress_ctrl)
    tcltk::tkbind(.muste$txt,"<Meta-Shift-KeyPress-V>",.muste.specialkeypress_ctrl) # Paste
    tcltk::tkbind(.muste$txt,"<Meta-Shift-KeyPress-v>",.muste.specialkeypress_ctrl)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-X>",.muste.specialkeypress) # Cut selected
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-x>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-Z>",.muste.specialkeypress) # Undo
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-z>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-Shift-KeyPress-Z>",.muste.specialkeypress_ctrl) # Redo
    tcltk::tkbind(.muste$txt,"<Meta-Shift-KeyPress-z>",.muste.specialkeypress_ctrl)
    tcltk::tkbind(.muste$txt,"<Meta-Shift-KeyPress-X>",.muste.specialkeypress_ctrl) # Cut selected
    tcltk::tkbind(.muste$txt,"<Meta-Shift-KeyPress-x>",.muste.specialkeypress_ctrl)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-A>",.muste.specialkeypress) # Beginning of line
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-a>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-D>",.muste.specialkeypress) # Delete
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-d>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-E>",.muste.specialkeypress) # End of line
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-e>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-K>",.muste.specialkeypress) # Kill-line (ctrl+end or alt+f10)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-k>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-O>",.muste.specialkeypress) # Open-line (F6 or alt+f9)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-o>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-M>",.muste.specialkeypress) # Open-line (F6 or alt+f9)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-m>",.muste.specialkeypress)
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-S>",.muste.specialkeypress) # Save edit field
    tcltk::tkbind(.muste$txt,"<Meta-KeyPress-s>",.muste.specialkeypress)   
    }


#tcltk::tkbind(.muste$txt,"<Option-F1>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Option-F2>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Option-F3>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Option-F4>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Option-F5>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Option-F6>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Option-F7>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Option-F8>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Option-F9>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Option-F10>",.muste.specialkeypress)

#tcltk::tkbind(.muste$txt,"<Command-F1>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Command-F2>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Command-F3>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Command-F4>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Command-F5>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Command-F6>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Command-F7>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Command-F8>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Command-F9>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Command-F10>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F1>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F2>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F3>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F4>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F5>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F6>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F7>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F8>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F9>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-F10>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-1>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-2>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-3>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-4>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-5>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-6>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-7>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-8>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-9>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Alt-KeyPress-0>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-1>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-2>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-3>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-4>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-5>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-6>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-7>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-8>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-9>",.muste.specialkeypress)
#tcltk::tkbind(.muste$txt,"<Control-KeyPress-0>",.muste.specialkeypress)

#tcltk::tkbind(.muste$plotwin[[1]],"<Configure>",.muste.canvas.scale)

#tcltk::tkbind(txt, "<Button-3>",RightClick)


.muste.defaulthighlightstyle()

tcltk::tktag.configure(.muste$txt,"shadow33",background="darkblue",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow34",background="darkblue",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow35",background="darkblue",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow36",background="darkblue",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow37",background="darkblue",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow38",background="darkblue",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow39",background="darkblue",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow40",background="darkblue",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow41",background="darkblue",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow42",background="darkblue",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow43",background="darkblue",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow44",background="darkblue",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow45",background="darkblue",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow46",background="darkblue",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow47",background="darkblue",foreground="white")
tcltk::tktag.configure(.muste$txt,"shadow48",background="darkgreen",foreground="black")

.muste.shadows("white","#F8F8F8")

tcltk::tktag.configure(.muste$txt,"shadow58",background="darkgreen",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow59",background="darkgreen",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow60",background="darkgreen",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow61",background="darkgreen",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow62",background="darkgreen",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow63",background="darkgreen",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow64",background="cyan4",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow65",background="cyan4",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow66",background="cyan4",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow67",background="cyan4",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow68",background="cyan4",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow69",background="cyan4",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow70",background="cyan4",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow71",background="cyan4",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow72",background="cyan4",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow73",background="cyan4",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow74",background="cyan4",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow75",background="cyan4",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow76",background="cyan4",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow77",background="cyan4",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow78",background="cyan4",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow79",background="cyan4",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow80",background="darkred",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow81",background="darkred",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow82",background="darkred",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow83",background="darkred",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow84",background="darkred",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow85",background="darkred",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow86",background="darkred",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow87",background="darkred",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow88",background="darkred",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow89",background="darkred",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow90",background="darkred",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow91",background="darkred",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow92",background="darkred",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow93",background="darkred",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow94",background="darkred",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow95",background="darkred",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow96",background="darkmagenta",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow97",background="darkmagenta",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow98",background="darkmagenta",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow99",background="darkmagenta",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow100",background="darkmagenta",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow101",background="darkmagenta",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow102",background="darkmagenta",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow103",background="darkmagenta",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow104",background="darkmagenta",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow105",background="darkmagenta",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow106",background="darkmagenta",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow107",background="darkmagenta",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow108",background="darkmagenta",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow109",background="darkmagenta",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow110",background="darkmagenta",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow111",background="darkmagenta",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow112",background="yellow4",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow113",background="yellow4",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow114",background="yellow4",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow115",background="yellow4",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow116",background="yellow4",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow117",background="yellow4",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow118",background="yellow4",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow119",background="yellow4",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow120",background="yellow4",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow121",background="yellow4",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow122",background="yellow4",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow123",background="yellow4",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow124",background="yellow4",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow125",background="yellow4",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow126",background="yellow4",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow127",background="yellow4",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow128",background="darkgrey",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow129",background="darkgrey",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow130",background="darkgrey",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow131",background="darkgrey",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow132",background="darkgrey",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow133",background="darkgrey",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow134",background="darkgrey",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow135",background="darkgrey",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow136",background="darkgrey",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow137",background="darkgrey",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow138",background="darkgrey",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow139",background="darkgrey",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow140",background="darkgrey",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow141",background="darkgrey",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow142",background="darkgrey",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow143",background="darkgrey",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow144",background="grey",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow145",background="grey",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow146",background="grey",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow147",background="grey",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow148",background="grey",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow149",background="grey",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow150",background="grey",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow151",background="grey",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow152",background="grey",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow153",background="grey",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow154",background="grey",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow155",background="grey",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow156",background="grey",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow157",background="grey",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow158",background="grey",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow159",background="grey",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow160",background="blue",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow161",background="blue",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow162",background="blue",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow163",background="blue",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow164",background="blue",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow165",background="blue",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow166",background="blue",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow167",background="blue",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow168",background="blue",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow169",background="blue",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow170",background="blue",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow171",background="blue",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow172",background="blue",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow173",background="blue",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow174",background="blue",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow175",background="blue",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow176",background="green",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow177",background="green",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow178",background="green",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow179",background="green",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow180",background="green",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow181",background="green",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow182",background="green",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow183",background="green",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow184",background="green",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow185",background="green",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow186",background="green",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow187",background="green",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow188",background="green",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow189",background="green",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow190",background="green",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow191",background="green",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow192",background="cyan",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow193",background="cyan",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow194",background="cyan",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow195",background="cyan",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow196",background="cyan",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow197",background="cyan",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow198",background="cyan",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow199",background="cyan",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow200",background="cyan",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow201",background="cyan",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow202",background="cyan",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow203",background="cyan",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow204",background="cyan",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow205",background="cyan",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow206",background="cyan",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow207",background="cyan",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow208",background="red",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow209",background="red",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow210",background="red",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow211",background="red",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow212",background="red",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow213",background="red",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow214",background="red",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow215",background="red",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow216",background="red",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow217",background="red",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow218",background="red",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow219",background="red",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow220",background="red",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow221",background="red",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow222",background="red",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow223",background="red",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow224",background="black",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow225",background="black",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow226",background="black",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow227",background="black",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow228",background="black",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow229",background="black",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow230",background="black",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow231",background="black",foreground="darkgrey")
tcltk::tktag.configure(.muste$txt,"shadow232",background="black",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow233",background="black",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow234",background="black",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow235",background="black",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow236",background="black",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow237",background="black",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow238",background="black",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow239",background="black",foreground="white")

tcltk::tktag.configure(.muste$txt,"shadow240",background="yellow",foreground="black")
tcltk::tktag.configure(.muste$txt,"shadow241",background="yellow",foreground="darkblue")
tcltk::tktag.configure(.muste$txt,"shadow242",background="yellow",foreground="darkgreen")
tcltk::tktag.configure(.muste$txt,"shadow243",background="yellow",foreground="cyan4")
tcltk::tktag.configure(.muste$txt,"shadow244",background="yellow",foreground="darkred")
tcltk::tktag.configure(.muste$txt,"shadow245",background="yellow",foreground="darkmagenta")
tcltk::tktag.configure(.muste$txt,"shadow246",background="yellow",foreground="yellow4")
tcltk::tktag.configure(.muste$txt,"shadow247",background="yellow",foreground="darkgrey")
#tcltk::tktag.configure(.muste$txt,"shadow248",background="yellow",foreground="grey")
tcltk::tktag.configure(.muste$txt,"shadow249",background="yellow",foreground="blue")
tcltk::tktag.configure(.muste$txt,"shadow250",background="yellow",foreground="green")
tcltk::tktag.configure(.muste$txt,"shadow251",background="yellow",foreground="cyan")
tcltk::tktag.configure(.muste$txt,"shadow252",background="yellow",foreground="red")
tcltk::tktag.configure(.muste$txt,"shadow253",background="yellow",foreground="magenta")
tcltk::tktag.configure(.muste$txt,"shadow254",background="yellow",foreground="yellow")
tcltk::tktag.configure(.muste$txt,"shadow255",background="yellow",foreground="white")


#tcltk::tktag.configure(.muste$txt,"shadow237",background="grey",foreground="grey")


}

.muste.shadows <- function(color="snow",bgcolor="B0B0B0")
	{
#	tcltk::tktag.configure(.muste$txt,"shadow10",background="red",foreground="black",font=.muste$fakefont)
	# .muste.shadows("white","#F8F8F8")
	tcltk::tktag.configure(.muste$txt,"shadow0",background=bgcolor,foreground="black")
	tcltk::tktag.configure(.muste$txt,"shadow1",background=bgcolor,foreground="red")
	tcltk::tktag.configure(.muste$txt,"shadow2",background=bgcolor,foreground="darkgrey") # line numbers
	tcltk::tktag.configure(.muste$txt,"shadow3",background=bgcolor,foreground="blue")
	tcltk::tktag.configure(.muste$txt,"shadow4",background="darkblue",foreground="grey")
	tcltk::tktag.configure(.muste$txt,"shadow5",background="yellow",foreground="black")
	tcltk::tktag.configure(.muste$txt,"shadow6",background=bgcolor,foreground="forest green") # changed to dark
	tcltk::tktag.configure(.muste$txt,"shadow7",background="blue",foreground="white")
	tcltk::tktag.configure(.muste$txt,"shadow8",background="darkblue",foreground="yellow")
	tcltk::tktag.configure(.muste$txt,"shadow9",background=bgcolor,foreground="darkgrey")
	
	tcltk::tktag.configure(.muste$txt,"shadow32",background=color,foreground="black")
	tcltk::tktag.configure(.muste$txt,"shadow49",background=color,foreground="red")
	tcltk::tktag.configure(.muste$txt,"shadow50",background=color,foreground="darkgrey") 
	tcltk::tktag.configure(.muste$txt,"shadow51",background=color,foreground="blue")
	tcltk::tktag.configure(.muste$txt,"shadow52",background="darkblue",foreground="grey")
	tcltk::tktag.configure(.muste$txt,"shadow53",background="yellow",foreground="black")
	tcltk::tktag.configure(.muste$txt,"shadow54",background=color,foreground="forest green") # changed to dark
	tcltk::tktag.configure(.muste$txt,"shadow55",background="blue",foreground="white")
	tcltk::tktag.configure(.muste$txt,"shadow56",background="darkblue",foreground="yellow")
	tcltk::tktag.configure(.muste$txt,"shadow57",background=color,foreground="darkgrey")
	
	tcltk::tktag.configure(.muste$txt,"shadow9999",background=color,foreground=color)
	tcltk::tktag.configure(.muste$txt,"shadow10",background=bgcolor,foreground="black",font=.muste$minifont)	
	tcltk::tktag.configure(.muste$txt,"shadow248",background=color,foreground=color)
	}
	
.muste.inittkwindow <- function()
    { 
   #.muste$dotTcl <- getNativeSymbolInfo("dotTcl","tcltk")$address
   #.Call("Survo_FindFunc",.muste$dotTcl)
   .muste$dotTcl <- getFromNamespace(".C_dotTcl", "tcltk")$address # tcltk:::.C_dotTcl$address
   .Call("Survo_FindRegFunc",.muste$dotTcl)
   
    
  tcltk::tcl("source", file.path(.muste$libname,.muste$pkgname,"tklibs","choosefont.tcl"))
    
  .muste$ikkuna <- tcltk::tktoplevel()
  tcltk::tcl("wm", "protocol", .muste$ikkuna, "WM_DELETE_WINDOW", quote(.muste.close()))
  #quote(cat("Use F8 to exit!\n"))) 

#  tcltk::tcl("wm", "resizable", .muste$ikkuna, "FALSE", "FALSE")
  tcltk::tkwm.resizable(.muste$ikkuna, FALSE, FALSE)

  tcltk::tkwm.title(.muste$ikkuna, "Survo")

  .muste$minifont <- tcltk::tkfont.create(family="Courier",size=1)
  if (.muste$sysname=="Darwin") { .muste$font <- tcltk::tkfont.create(family="Courier",size=14) }
#  RS Menlo font is no longer working 2026-07-01 # if (.muste$sysname=="Darwin") { .muste$font <- tkfont.create(family="Menlo",size=14) }
  else if (.muste$sysname=="Windows")
  	{ 
  	.muste$font <- tcltk::tkfont.create(family="Lucida Console",size=12)
#  	.muste$menu<-tkmenu(.muste$ikkuna)
#	tkconfigure(.muste$ikkuna,menu=.muste$menu)
#	tkadd(.muste$menu, "cascade", label="Survo")
  	}
  else { .muste$font <- tcltk::tkfont.create(family="Courier",size=12) }

#  .muste$fakefont <- tcltk::tkfont.create(family="Courier",size=4)

#  .muste.menu()   
  .muste$txt <- tcltk::tktext(.muste$ikkuna,width=80,height=25,foreground="#000000",background="snow",
                            wrap="none",font=.muste$font,undo=FALSE)
  tcltk::tkgrid(.muste$txt,row=1,sticky="nw")  
#  tcltk::tkinsert(.muste$txt,"1.1","Initializing Tcl/Tk")
  
  .muste$window <- tcltk::.Tk.ID(.muste$txt)
  
  Sys.sleep(1);
  # Poistetaan text-widgetin perussidokset
  sidokset <- gsub("Text ","",tcltk::tclvalue(tcltk::tkbindtags(.muste$txt)))
  tcltk::tkbindtags(.muste$txt,sidokset)
  
  tcltk::tcl("bind","all","<Key-F10>","")
  tcltk::tcl("bind","all","<KeyRelease-F10>","")
  tcltk::tcl("bind","all","<Alt-Key>","")
  tcltk::tcl("bind","all","<Alt-KeyRelease>","")
  tcltk::tcl("bind","all","<Key-Alt_R>","")
  tcltk::tcl("bind","all","<KeyRelease-Alt_R>","")
  tcltk::tcl("bind","all","<Key-Alt_L>","")
  tcltk::tcl("bind","all","<KeyRelease-Alt_L>","")
  
  tcltk::tcl("bind","Menubutton","<Key-F10>","")
  tcltk::tcl("bind","Menubutton","<Alt-Key>","")

  .muste.resize(80,25)
  .muste.getwindowdim()

.muste.statusbar(init=TRUE)

#.muste$scry <- tkscrollbar(.muste$ikkuna,repeatinterval=5, command=function(...).muste.yview(.muste$txt,...))  
#.muste$scrx <- tkscrollbar(.muste$ikkuna,orient="horizontal",repeatinterval=5, command=function(...).muste.xview(.muste$txt,...))

.muste$scry <- tcltk::ttkscrollbar(.muste$ikkuna, command=function(...).muste.yview(.muste$txt,...))  
.muste$scrx <- tcltk::ttkscrollbar(.muste$ikkuna,orient="horizontal", command=function(...).muste.xview(.muste$txt,...))


tcltk::tkfocus("-force",.muste$txt)
tcltk::tkfocus(.muste$txt)
tcltk::tcl("update","idletasks")
tcltk::tcl("update")
    }
    
.muste.remove.bindings <- function()
	{
if (.muste$eventloop.after) 
tcltk::tcl("after", "cancel", .muste$eventloopid)	
	bindvec<-unlist(strsplit(tcltk::tclvalue(tcltk::tkbind(.muste$txt))," "))
for(i in 1:length(bindvec)) { tcltk::tkbind(.muste$txt,bindvec[i],"") }
	}

.muste.destroywindow <- function()
{
tcltk::tkdestroy(.muste$txt)
tcltk::tkdestroy(.muste$ikkuna)
}
