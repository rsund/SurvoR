rftest <- function() 
{
#requireNamespace("tcltk",quietly=TRUE)
#attachNamespace("tcltk")

window <- tktoplevel()
tkwm.title(window, "Rftest")
font <- tkfont.create(family="Courier",size=12)
txtwidget <- tktext(window,width=80,height=25,foreground="#000000",background="snow",wrap="none",font=font,undo=FALSE)
tkgrid(txtwidget)  

Sys.sleep(10)
tkdestroy(txtwidget)
tkdestroy(window)
}

