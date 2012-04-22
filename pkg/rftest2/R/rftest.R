rftest <- function() 
{
#requireNamespace("tcltk",quietly=TRUE)
#attachNamespace("tcltk")

.rftest$ikkuna <- tktoplevel()
tkwm.title(.rftest$ikkuna, "Rftest")
.rftest$font <- tkfont.create(family="Courier",size=12) }
.rftest$txt <- tktext(.rftest$ikkuna,width=80,height=25,foreground="#000000",background="snow",wrap="none",font=.rftest$font,undo=FALSE)
 tkgrid(.rftest$txt)  

Sys.sleep(10);
tkdestroy(.rftest$txt)
tkdestroy(.rftest$ikkuna)
}
