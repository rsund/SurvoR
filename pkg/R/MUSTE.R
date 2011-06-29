#require(tcltk)

.muste.getcursor <- function()
  {
  apu<-as.numeric(unlist(strsplit(as.character(tkindex(.muste.txt,"insert")),"\\.")))
  .muste.cursor.row<<-as.integer(apu[1])
  .muste.cursor.col<<-as.integer(apu[2])
  }

.muste.getmouse <- function()
  {
  apu<-as.numeric(unlist(strsplit(as.character(tkindex(.muste.txt,"current")),"\\.")))
  .muste.mouse.row<<-as.integer(apu[1])
  .muste.mouse.col<<-as.integer(apu[2])
  }

.muste.keypress <- function(A,K,N,k,t,T)
  {

# A = UNICODE character
# K = The keysym corresponding to the event, substituted as a textual string.
# N = The keysym corresponding to the event, substituted as a decimal number.
# k = The keycode field from the event.
# t = The time field from the event.
# T = The type field from the event.

  merkki<-iconv(A, "UTF8","CP850") 

  if (is.na(merkki))
    {
    merkki<-"?"
    }

  .muste.event.time<<-as.integer(t)
  .muste.event.type<<-as.integer(1)  # KEY_EVENT
  .muste.key.char<<-merkki
  .muste.key.keysym<<-as.integer(N)


#cat("Merkki:",A,.muste.key.keysym,k,t,"\n")
  }

.muste.specialkeypress <- function(A,K,N,k,t,T)
  {

  .muste.event.time<<-as.integer(t)
  .muste.event.type<<-as.integer(3)  # SPECIAL_KEY_EVENT
  .muste.key.keysym<<-as.integer(as.integer(N)+100000)

#cat("Erikoismerkki:",A,.muste.key.keysym,k,t,"\n")

  } 


.muste.mouseevent <- function(x,y,t,T,b)
  {
# t = The time field from the event.
# T = The type field from the event.


  .muste.event.time<<-as.integer(t)
  .muste.event.type<<-as.integer(2)  # MOUSE_EVENT
  .muste.getmouse()

  if (as.integer(T)!=4) b<-0
  .muste.mouse.button<<-as.integer(b)
  .muste.mouse.double<<-as.integer(0)

#cat("Mouse:",.muste.mouse.col,.muste.mouse.row,x,y,t,T,b,.muste.mouse.double,"\n")
}

.muste.doublemouseevent <- function(x,y,t,T,b)
  {
# t = The time field from the event.
# T = The type field from the event.

  .muste.event.time<<-as.integer(t)
  .muste.event.type<<-as.integer(2)  # MOUSE_EVENT
  .muste.getmouse()
  .muste.mouse.button<<-as.integer(b)
  .muste.mouse.double<<-as.integer(1)

#cat("Mouse:",.muste.mouse.col,.muste.mouse.row,x,y,t,T,b,.muste.mouse.double,"\n")
}


.muste.resize <- function(cols,rows)
  {
  tkconfigure(.muste.txt,width=cols,height=rows)
  tkdelete(.muste.txt,"1.0","end")
  tyhjarivi <- paste(c(rep(" ",cols),"\n"),sep="",collapse="")
  tyhjaruutu <- paste(rep(tyhjarivi,rows),sep="",collapse="")
  tkinsert(.muste.txt,"end",tyhjaruutu) 
  }

muste <- function()
  {
  .muste.environment <<- environment()
  .muste.ikkuna <<- tktoplevel()

  tcl("wm", "protocol", .muste.ikkuna, "WM_DELETE_WINDOW", quote(cat("Use F8 to exit!\n"))) 
  tcl("wm", "resizable", .muste.ikkuna, "FALSE", "FALSE")

  tkwm.title(.muste.ikkuna, "Muste")

  .muste.font <<- tkfont.create(family="Courier",size=12)

  .muste.txt <<- tktext(.muste.ikkuna,width=80,height=25,foreground="#000000",background="#FEFEFE",
                            wrap="none",font=.muste.font,undo=FALSE)
  tkgrid(.muste.txt)
  .muste.window<<-.Tk.ID(.muste.txt)

  # Poistetaan text-widgetin perussidokset käytöstä
  sidokset <- gsub("Text ","",tclvalue(tkbindtags(.muste.txt)))
  tkbindtags(.muste.txt,sidokset)

  .muste.resize(80,25)

  tkbind(.muste.txt,"<KeyPress>",.muste.keypress)

  tkbind(.muste.txt,"<Control-End>",.muste.specialkeypress)

tkbind(.muste.txt,"<Alt-Right>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-Left>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-Down>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-Up>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-Right>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-Left>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-Down>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-Up>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F1>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F2>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F3>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F4>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F5>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F6>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F7>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F8>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F9>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-F10>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Option-F1>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Option-F2>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Option-F3>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Option-F4>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Option-F5>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Option-F6>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Option-F7>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Option-F8>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Option-F9>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F1>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F2>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F3>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F4>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F5>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F6>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F7>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F8>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F9>",.muste.specialkeypress)
#tkbind(.muste.txt,"<Command-F10>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F1>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F2>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F3>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F4>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F5>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F6>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F7>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F8>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F9>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-F10>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-1>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-2>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-3>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-4>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-5>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-6>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-7>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-8>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-9>",.muste.specialkeypress)
tkbind(.muste.txt,"<Alt-KeyPress-0>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-1>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-2>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-3>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-4>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-5>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-6>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-7>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-8>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-9>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-0>",.muste.specialkeypress)
tkbind(.muste.txt,"<ButtonPress>",.muste.mouseevent)
tkbind(.muste.txt,"<Double-ButtonPress>",.muste.doublemouseevent)
tkbind(.muste.txt,"<Motion>",.muste.mouseevent)


#tkbind(txt, "<Button-3>",RightClick)

tktag.configure(.muste.txt,"shadow0",background="#FEFEFE",foreground="black")
tktag.configure(.muste.txt,"shadow32",background="#FEFEFE",foreground="black")
tktag.configure(.muste.txt,"shadow48",background="#FEFEFE",foreground="black")
tktag.configure(.muste.txt,"shadow49",background="#FEFEFE",foreground="red")
tktag.configure(.muste.txt,"shadow50",background="#FEFEFE",foreground="grey")
tktag.configure(.muste.txt,"shadow51",background="#FEFEFE",foreground="blue")
tktag.configure(.muste.txt,"shadow52",background="darkblue",foreground="grey")
tktag.configure(.muste.txt,"shadow53",background="yellow",foreground="black")
tktag.configure(.muste.txt,"shadow54",background="#FEFEFE",foreground="green")
tktag.configure(.muste.txt,"shadow55",background="blue",foreground="white")
tktag.configure(.muste.txt,"shadow56",background="darkblue",foreground="yellow")
tktag.configure(.muste.txt,"shadow237",background="grey",foreground="grey")


tkfocus(.muste.txt)
.muste.event.time<<-as.integer(0)
    args<-"A"
    .Call("Muste_Editor",args)

#tkdestroy(.muste.txt)
#tkdestroy(.muste.ikkuna)
}

