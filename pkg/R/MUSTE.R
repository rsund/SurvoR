#require(tcltk)

.muste.eventloop <- function()
  
  {
  
  args<-"Tosi"
  
  invisible(.Call("Muste_Eventloop",args))
  if (.muste.eventlooprun) { tcl("after",10,.muste.eventloop) }
  if (.muste.eventlooprun==0) 
     { 
#     cat("Muste terminated!!!\n")
     .muste.end()
     }
  }
  

.muste.getclipboard <- function()
  {
#  .muste.clipboard<<-tcl("clipboard","get")
  clip<-tcl("clipboard","get")
  .muste.clipboard<<-tclvalue(clip)
  }
  
  
.muste.getcursor <- function()
  {
  apu<-as.numeric(unlist(strsplit(as.character(tkindex(.muste.txt,"insert")),"\\.")))
  .muste.cursor.row<<-as.integer(apu[1])
  .muste.cursor.col<<-as.integer(apu[2])
  }

.muste.getwindowdim <- function()
  
   {
  
    apu<-unlist(strsplit(as.character(tkwm.geometry(.muste.ikkuna)),"x|\\+"))
   .muste.window.width<<-as.integer(apu[1])
  .muste.window.height<<-as.integer(apu[2])
  .muste.window.topx<<-as.integer(apu[3])
  .muste.window.topy<<-as.integer(apu[4])
  .muste.window.bottomx<<-.muste.window.width+.muste.window.topx
  .muste.window.bottomy<<-.muste.window.height+.muste.window.topy
  }


.muste.getscreendim <- function()
  {
  .muste.screen.width<<-as.integer(tkwinfo("screenwidth",.muste.ikkuna))
  .muste.screen.height<<-as.integer(tkwinfo("screenheight",.muste.ikkuna))
  }


.muste.getfontdim <- function()
  {
  .muste.font.width<<-as.integer(tkfont.measure(.muste.font,"R"))
  .muste.font.height<<-as.integer(tkfont.metrics(.muste.font,"-linespace"))
   }

.muste.choosefont <- function()
  
   {

   valittu<-as.character(tcl("choosefont::choosefont", fonttype="fixed"))
   valittu[2]<-paste("{",valittu[2],"}",sep="")
argumentit<-paste(as.character(valittu),collapse=" ")
   komento <- paste("font configure",as.character(.muste.font),argumentit,sep=" ")
   .Tcl(komento)
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

.muste.init <- function()
  {
  .muste.environment <<- environment()
  .muste.ikkuna <<- tktoplevel()

  tcl("wm", "protocol", .muste.ikkuna, "WM_DELETE_WINDOW", quote(cat("Use F8 to exit!\n"))) 

#  tcl("wm", "resizable", .muste.ikkuna, "FALSE", "FALSE")
  tkwm.resizable(.muste.ikkuna, FALSE, FALSE)

  tkwm.title(.muste.ikkuna, "Muste")

#  .muste.font <<- tkfont.create(family="Courier",size=12)
   .muste.font <<- tkfont.create(family="Menlo",size=12)

  .muste.txt <<- tktext(.muste.ikkuna,width=80,height=25,foreground="#000000",background="snow",
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
tkbind(.muste.txt,"<Control-KeyPress-R>",.muste.specialkeypress)
tkbind(.muste.txt,"<Control-KeyPress-r>",.muste.specialkeypress)
tkbind(.muste.txt,"<ButtonPress>",.muste.mouseevent)
tkbind(.muste.txt,"<Double-ButtonPress>",.muste.doublemouseevent)
tkbind(.muste.txt,"<Motion>",.muste.mouseevent)


#tkbind(txt, "<Button-3>",RightClick)

tktag.configure(.muste.txt,"shadow0",background="snow",foreground="black")
tktag.configure(.muste.txt,"shadow32",background="snow",foreground="black")
tktag.configure(.muste.txt,"shadow33",background="darkblue",foreground="darkblue")
tktag.configure(.muste.txt,"shadow34",background="darkblue",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow35",background="darkblue",foreground="cyan4")
tktag.configure(.muste.txt,"shadow36",background="darkblue",foreground="darkred")
tktag.configure(.muste.txt,"shadow37",background="darkblue",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow38",background="darkblue",foreground="yellow4")
tktag.configure(.muste.txt,"shadow39",background="darkblue",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow40",background="darkblue",foreground="grey")
tktag.configure(.muste.txt,"shadow41",background="darkblue",foreground="blue")
tktag.configure(.muste.txt,"shadow42",background="darkblue",foreground="green")
tktag.configure(.muste.txt,"shadow43",background="darkblue",foreground="cyan")
tktag.configure(.muste.txt,"shadow44",background="darkblue",foreground="red")
tktag.configure(.muste.txt,"shadow45",background="darkblue",foreground="magenta")
tktag.configure(.muste.txt,"shadow46",background="darkblue",foreground="yellow")
tktag.configure(.muste.txt,"shadow47",background="darkblue",foreground="white")
tktag.configure(.muste.txt,"shadow48",background="darkgreen",foreground="black")
tktag.configure(.muste.txt,"shadow49",background="snow",foreground="red")
tktag.configure(.muste.txt,"shadow50",background="snow",foreground="grey")
tktag.configure(.muste.txt,"shadow51",background="snow",foreground="blue")
tktag.configure(.muste.txt,"shadow52",background="darkblue",foreground="grey")
tktag.configure(.muste.txt,"shadow53",background="yellow",foreground="black")
tktag.configure(.muste.txt,"shadow54",background="snow",foreground="green")
tktag.configure(.muste.txt,"shadow55",background="blue",foreground="white")
tktag.configure(.muste.txt,"shadow56",background="darkblue",foreground="yellow")
tktag.configure(.muste.txt,"shadow57",background="snow",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow58",background="darkgreen",foreground="green")
tktag.configure(.muste.txt,"shadow59",background="darkgreen",foreground="cyan")
tktag.configure(.muste.txt,"shadow60",background="darkgreen",foreground="red")
tktag.configure(.muste.txt,"shadow61",background="darkgreen",foreground="magenta")
tktag.configure(.muste.txt,"shadow62",background="darkgreen",foreground="yellow")
tktag.configure(.muste.txt,"shadow63",background="darkgreen",foreground="white")

tktag.configure(.muste.txt,"shadow64",background="cyan4",foreground="black")
tktag.configure(.muste.txt,"shadow65",background="cyan4",foreground="darkblue")
tktag.configure(.muste.txt,"shadow66",background="cyan4",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow67",background="cyan4",foreground="cyan4")
tktag.configure(.muste.txt,"shadow68",background="cyan4",foreground="darkred")
tktag.configure(.muste.txt,"shadow69",background="cyan4",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow70",background="cyan4",foreground="yellow4")
tktag.configure(.muste.txt,"shadow71",background="cyan4",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow72",background="cyan4",foreground="grey")
tktag.configure(.muste.txt,"shadow73",background="cyan4",foreground="blue")
tktag.configure(.muste.txt,"shadow74",background="cyan4",foreground="green")
tktag.configure(.muste.txt,"shadow75",background="cyan4",foreground="cyan")
tktag.configure(.muste.txt,"shadow76",background="cyan4",foreground="red")
tktag.configure(.muste.txt,"shadow77",background="cyan4",foreground="magenta")
tktag.configure(.muste.txt,"shadow78",background="cyan4",foreground="yellow")
tktag.configure(.muste.txt,"shadow79",background="cyan4",foreground="white")

tktag.configure(.muste.txt,"shadow80",background="darkred",foreground="black")
tktag.configure(.muste.txt,"shadow81",background="darkred",foreground="darkblue")
tktag.configure(.muste.txt,"shadow82",background="darkred",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow83",background="darkred",foreground="cyan4")
tktag.configure(.muste.txt,"shadow84",background="darkred",foreground="darkred")
tktag.configure(.muste.txt,"shadow85",background="darkred",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow86",background="darkred",foreground="yellow4")
tktag.configure(.muste.txt,"shadow87",background="darkred",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow88",background="darkred",foreground="grey")
tktag.configure(.muste.txt,"shadow89",background="darkred",foreground="blue")
tktag.configure(.muste.txt,"shadow90",background="darkred",foreground="green")
tktag.configure(.muste.txt,"shadow91",background="darkred",foreground="cyan")
tktag.configure(.muste.txt,"shadow92",background="darkred",foreground="red")
tktag.configure(.muste.txt,"shadow93",background="darkred",foreground="magenta")
tktag.configure(.muste.txt,"shadow94",background="darkred",foreground="yellow")
tktag.configure(.muste.txt,"shadow95",background="darkred",foreground="white")

tktag.configure(.muste.txt,"shadow96",background="darkmagenta",foreground="black")
tktag.configure(.muste.txt,"shadow97",background="darkmagenta",foreground="darkblue")
tktag.configure(.muste.txt,"shadow98",background="darkmagenta",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow99",background="darkmagenta",foreground="cyan4")
tktag.configure(.muste.txt,"shadow100",background="darkmagenta",foreground="darkred")
tktag.configure(.muste.txt,"shadow101",background="darkmagenta",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow102",background="darkmagenta",foreground="yellow4")
tktag.configure(.muste.txt,"shadow103",background="darkmagenta",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow104",background="darkmagenta",foreground="grey")
tktag.configure(.muste.txt,"shadow105",background="darkmagenta",foreground="blue")
tktag.configure(.muste.txt,"shadow106",background="darkmagenta",foreground="green")
tktag.configure(.muste.txt,"shadow107",background="darkmagenta",foreground="cyan")
tktag.configure(.muste.txt,"shadow108",background="darkmagenta",foreground="red")
tktag.configure(.muste.txt,"shadow109",background="darkmagenta",foreground="magenta")
tktag.configure(.muste.txt,"shadow110",background="darkmagenta",foreground="yellow")
tktag.configure(.muste.txt,"shadow111",background="darkmagenta",foreground="white")

tktag.configure(.muste.txt,"shadow112",background="yellow4",foreground="black")
tktag.configure(.muste.txt,"shadow113",background="yellow4",foreground="darkblue")
tktag.configure(.muste.txt,"shadow114",background="yellow4",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow115",background="yellow4",foreground="cyan4")
tktag.configure(.muste.txt,"shadow116",background="yellow4",foreground="darkred")
tktag.configure(.muste.txt,"shadow117",background="yellow4",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow118",background="yellow4",foreground="yellow4")
tktag.configure(.muste.txt,"shadow119",background="yellow4",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow120",background="yellow4",foreground="grey")
tktag.configure(.muste.txt,"shadow121",background="yellow4",foreground="blue")
tktag.configure(.muste.txt,"shadow122",background="yellow4",foreground="green")
tktag.configure(.muste.txt,"shadow123",background="yellow4",foreground="cyan")
tktag.configure(.muste.txt,"shadow124",background="yellow4",foreground="red")
tktag.configure(.muste.txt,"shadow125",background="yellow4",foreground="magenta")
tktag.configure(.muste.txt,"shadow126",background="yellow4",foreground="yellow")
tktag.configure(.muste.txt,"shadow127",background="yellow4",foreground="white")

tktag.configure(.muste.txt,"shadow128",background="darkgrey",foreground="black")
tktag.configure(.muste.txt,"shadow129",background="darkgrey",foreground="darkblue")
tktag.configure(.muste.txt,"shadow130",background="darkgrey",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow131",background="darkgrey",foreground="cyan4")
tktag.configure(.muste.txt,"shadow132",background="darkgrey",foreground="darkred")
tktag.configure(.muste.txt,"shadow133",background="darkgrey",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow134",background="darkgrey",foreground="yellow4")
tktag.configure(.muste.txt,"shadow135",background="darkgrey",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow136",background="darkgrey",foreground="grey")
tktag.configure(.muste.txt,"shadow137",background="darkgrey",foreground="blue")
tktag.configure(.muste.txt,"shadow138",background="darkgrey",foreground="green")
tktag.configure(.muste.txt,"shadow139",background="darkgrey",foreground="cyan")
tktag.configure(.muste.txt,"shadow140",background="darkgrey",foreground="red")
tktag.configure(.muste.txt,"shadow141",background="darkgrey",foreground="magenta")
tktag.configure(.muste.txt,"shadow142",background="darkgrey",foreground="yellow")
tktag.configure(.muste.txt,"shadow143",background="darkgrey",foreground="white")

tktag.configure(.muste.txt,"shadow144",background="grey",foreground="black")
tktag.configure(.muste.txt,"shadow145",background="grey",foreground="darkblue")
tktag.configure(.muste.txt,"shadow146",background="grey",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow147",background="grey",foreground="cyan4")
tktag.configure(.muste.txt,"shadow148",background="grey",foreground="darkred")
tktag.configure(.muste.txt,"shadow149",background="grey",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow150",background="grey",foreground="yellow4")
tktag.configure(.muste.txt,"shadow151",background="grey",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow152",background="grey",foreground="grey")
tktag.configure(.muste.txt,"shadow153",background="grey",foreground="blue")
tktag.configure(.muste.txt,"shadow154",background="grey",foreground="green")
tktag.configure(.muste.txt,"shadow155",background="grey",foreground="cyan")
tktag.configure(.muste.txt,"shadow156",background="grey",foreground="red")
tktag.configure(.muste.txt,"shadow157",background="grey",foreground="magenta")
tktag.configure(.muste.txt,"shadow158",background="grey",foreground="yellow")
tktag.configure(.muste.txt,"shadow159",background="grey",foreground="white")

tktag.configure(.muste.txt,"shadow160",background="blue",foreground="black")
tktag.configure(.muste.txt,"shadow161",background="blue",foreground="darkblue")
tktag.configure(.muste.txt,"shadow162",background="blue",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow163",background="blue",foreground="cyan4")
tktag.configure(.muste.txt,"shadow164",background="blue",foreground="darkred")
tktag.configure(.muste.txt,"shadow165",background="blue",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow166",background="blue",foreground="yellow4")
tktag.configure(.muste.txt,"shadow167",background="blue",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow168",background="blue",foreground="grey")
tktag.configure(.muste.txt,"shadow169",background="blue",foreground="blue")
tktag.configure(.muste.txt,"shadow170",background="blue",foreground="green")
tktag.configure(.muste.txt,"shadow171",background="blue",foreground="cyan")
tktag.configure(.muste.txt,"shadow172",background="blue",foreground="red")
tktag.configure(.muste.txt,"shadow173",background="blue",foreground="magenta")
tktag.configure(.muste.txt,"shadow174",background="blue",foreground="yellow")
tktag.configure(.muste.txt,"shadow175",background="blue",foreground="white")

tktag.configure(.muste.txt,"shadow176",background="green",foreground="black")
tktag.configure(.muste.txt,"shadow177",background="green",foreground="darkblue")
tktag.configure(.muste.txt,"shadow178",background="green",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow179",background="green",foreground="cyan4")
tktag.configure(.muste.txt,"shadow180",background="green",foreground="darkred")
tktag.configure(.muste.txt,"shadow181",background="green",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow182",background="green",foreground="yellow4")
tktag.configure(.muste.txt,"shadow183",background="green",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow184",background="green",foreground="grey")
tktag.configure(.muste.txt,"shadow185",background="green",foreground="blue")
tktag.configure(.muste.txt,"shadow186",background="green",foreground="green")
tktag.configure(.muste.txt,"shadow187",background="green",foreground="cyan")
tktag.configure(.muste.txt,"shadow188",background="green",foreground="red")
tktag.configure(.muste.txt,"shadow189",background="green",foreground="magenta")
tktag.configure(.muste.txt,"shadow190",background="green",foreground="yellow")
tktag.configure(.muste.txt,"shadow191",background="green",foreground="white")

tktag.configure(.muste.txt,"shadow192",background="cyan",foreground="black")
tktag.configure(.muste.txt,"shadow193",background="cyan",foreground="darkblue")
tktag.configure(.muste.txt,"shadow194",background="cyan",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow195",background="cyan",foreground="cyan4")
tktag.configure(.muste.txt,"shadow196",background="cyan",foreground="darkred")
tktag.configure(.muste.txt,"shadow197",background="cyan",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow198",background="cyan",foreground="yellow4")
tktag.configure(.muste.txt,"shadow199",background="cyan",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow200",background="cyan",foreground="grey")
tktag.configure(.muste.txt,"shadow201",background="cyan",foreground="blue")
tktag.configure(.muste.txt,"shadow202",background="cyan",foreground="green")
tktag.configure(.muste.txt,"shadow203",background="cyan",foreground="cyan")
tktag.configure(.muste.txt,"shadow204",background="cyan",foreground="red")
tktag.configure(.muste.txt,"shadow205",background="cyan",foreground="magenta")
tktag.configure(.muste.txt,"shadow206",background="cyan",foreground="yellow")
tktag.configure(.muste.txt,"shadow207",background="cyan",foreground="white")

tktag.configure(.muste.txt,"shadow208",background="red",foreground="black")
tktag.configure(.muste.txt,"shadow209",background="red",foreground="darkblue")
tktag.configure(.muste.txt,"shadow210",background="red",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow211",background="red",foreground="cyan4")
tktag.configure(.muste.txt,"shadow212",background="red",foreground="darkred")
tktag.configure(.muste.txt,"shadow213",background="red",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow214",background="red",foreground="yellow4")
tktag.configure(.muste.txt,"shadow215",background="red",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow216",background="red",foreground="grey")
tktag.configure(.muste.txt,"shadow217",background="red",foreground="blue")
tktag.configure(.muste.txt,"shadow218",background="red",foreground="green")
tktag.configure(.muste.txt,"shadow219",background="red",foreground="cyan")
tktag.configure(.muste.txt,"shadow220",background="red",foreground="red")
tktag.configure(.muste.txt,"shadow221",background="red",foreground="magenta")
tktag.configure(.muste.txt,"shadow222",background="red",foreground="yellow")
tktag.configure(.muste.txt,"shadow223",background="red",foreground="white")

tktag.configure(.muste.txt,"shadow224",background="black",foreground="black")
tktag.configure(.muste.txt,"shadow225",background="black",foreground="darkblue")
tktag.configure(.muste.txt,"shadow226",background="black",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow227",background="black",foreground="cyan4")
tktag.configure(.muste.txt,"shadow228",background="black",foreground="darkred")
tktag.configure(.muste.txt,"shadow229",background="black",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow230",background="black",foreground="yellow4")
tktag.configure(.muste.txt,"shadow231",background="black",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow232",background="black",foreground="grey")
tktag.configure(.muste.txt,"shadow233",background="black",foreground="blue")
tktag.configure(.muste.txt,"shadow234",background="black",foreground="green")
tktag.configure(.muste.txt,"shadow235",background="black",foreground="cyan")
tktag.configure(.muste.txt,"shadow236",background="black",foreground="red")
tktag.configure(.muste.txt,"shadow237",background="black",foreground="magenta")
tktag.configure(.muste.txt,"shadow238",background="black",foreground="yellow")
tktag.configure(.muste.txt,"shadow239",background="black",foreground="white")

tktag.configure(.muste.txt,"shadow240",background="yellow",foreground="black")
tktag.configure(.muste.txt,"shadow241",background="yellow",foreground="darkblue")
tktag.configure(.muste.txt,"shadow242",background="yellow",foreground="darkgreen")
tktag.configure(.muste.txt,"shadow243",background="yellow",foreground="cyan4")
tktag.configure(.muste.txt,"shadow244",background="yellow",foreground="darkred")
tktag.configure(.muste.txt,"shadow245",background="yellow",foreground="darkmagenta")
tktag.configure(.muste.txt,"shadow246",background="yellow",foreground="yellow4")
tktag.configure(.muste.txt,"shadow247",background="yellow",foreground="darkgrey")
tktag.configure(.muste.txt,"shadow248",background="yellow",foreground="grey")
tktag.configure(.muste.txt,"shadow249",background="yellow",foreground="blue")
tktag.configure(.muste.txt,"shadow250",background="yellow",foreground="green")
tktag.configure(.muste.txt,"shadow251",background="yellow",foreground="cyan")
tktag.configure(.muste.txt,"shadow252",background="yellow",foreground="red")
tktag.configure(.muste.txt,"shadow253",background="yellow",foreground="magenta")
tktag.configure(.muste.txt,"shadow254",background="yellow",foreground="yellow")
tktag.configure(.muste.txt,"shadow255",background="yellow",foreground="white")


#tktag.configure(.muste.txt,"shadow237",background="grey",foreground="grey")

tkfocus("-force",.muste.txt)
tkfocus(.muste.txt)
}

.muste.stop <- function()
{
.muste.eventlooprun<<-0
}

.muste.end <- function()
{
tkdestroy(.muste.txt)
tkdestroy(.muste.ikkuna)
}

muste <- function() 
{

.muste.init()

.muste.event.time<<-as.integer(0)
.muste.eventlooprun<<-1
    args<-"A"
    .Call("Muste_Editor",args)
#  tcl("after",1000,.muste.eventloop)
invisible(.muste.eventloop())

#.muste.end()

}
