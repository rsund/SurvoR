.muste.help.keypress <- function(A, K, N, k, t, T, s) {
  # A = UNICODE character
  # K = The keysym corresponding to the event, substituted as a textual string.
  # N = The keysym corresponding to the event, substituted as a decimal number.
  # k = The keycode field from the event.
  # t = The time field from the event.
  # T = The type field from the event.
  #  tcltk::tkinsert(.muste$help.txt,"3.1",A)
  return(tcltk::tclvalue(tcltk::tcl("break")))
}

.muste.help.end <- function() {
  .muste$help.ikkuna.existing <- FALSE
  tcltk::tkdestroy(.muste$help.ikkuna)
  # 	cat("Help ikkuna poistettu")
}

.muste.help.init <- function(title = "Survo R - Help Window") {
  if (.muste$help.ikkuna.existing) {
    tcltk::tkfocus(.muste$help.ikkuna)
    return()
  }

  .muste$help.ikkuna <- tcltk::tktoplevel()
  .muste$help.ikkuna.existing <- TRUE

  #  tcltk::tcl("wm", "protocol", .muste$ikkuna, "WM_DELETE_WINDOW", quote(.muste.command("Exit")))
  tcltk::tkwm.resizable(.muste$help.ikkuna, FALSE, FALSE)

  tcltk::tkwm.title(.muste$help.ikkuna, title)
  .muste$help.font <- tcltk::tkfont.create(family = "Courier", size = 10)

  .muste$help.txt <- tcltk::tktext(.muste$help.ikkuna,
    width = 80, height = 25,
    foreground = "white", background = "blue",
    wrap = "none", font = .muste$help.font, undo = FALSE,
    insertwidth = 0
  )

  tcltk::tkgrid(.muste$help.txt)
  .muste$help.window <- tcltk::.Tk.ID(.muste$help.txt)

  #    sidokset <- gsub("Text ","",tcltk::tclvalue(tcltk::tkbindtags(.muste$help.txt)))
  #    tkbindtags(.muste$help.txt,sidokset)

  tcltk::tkinsert(.muste$help.txt, "1.1", "Help window for Survo R")
  #  	tkbind(.muste$help.txt,"<KeyPress>",.muste.help.keypress)

  a <- sprintf(".Tcl('bind %s <KeyPress> break')", as.character(.muste$help.window))
  eval(parse(text = a))

  a <- sprintf(".Tcl('bind %s <<PasteSelection>> break')", as.character(.muste$help.window))
  eval(parse(text = a))

  a <- sprintf(".Tcl('bind %s <Control-c> {event generate %%W <<Copy>>}')", as.character(.muste$help.window))
  eval(parse(text = a))

  a <- sprintf(".Tcl('bind %s <Control-C> {event generate %%W <<Copy>>}')", as.character(.muste$help.window))
  eval(parse(text = a))

  tcltk::tkbind(.muste$help.ikkuna, "<<Destroy>>", .muste.help.end)
  tcltk::tcl("wm", "protocol", .muste$help.ikkuna, "WM_DELETE_WINDOW", quote(.muste.help.end()))

  # tcltk::.Tcl("bind .2.1 <KeyPress> break")
  # tcltk::.Tcl("bind .2.1 <<PasteSelection>> break")
  # tcltk::.Tcl("bind .2.1 <Control-c> {event generate %W <<Copy>>}")
  # tcltk::.Tcl("bind .2.1 <Control-C> {event generate %W <<Copy>>}")
}
