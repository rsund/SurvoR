require(tcltk)

execute <- function(code) {
  e <- try(parse(text=code))
  if (inherits(e, "try-error")) {
    cat("-----","Syntax error while executing from editor:",code,sep="\n")
    return()
  }
  cat("-----","Executing from editor:",code,"-----","result:", sep="\n")
  print(eval.parent(e))
}

print.editfield <- function(showx,showy) {
  tkdelete(txt,"1.0","end")
  
  tkinsert(txt,"1.0","    1  1 MUSTE      Sun Feb 08 19:26:00 2009 C:\\MUSTE      2000  100 0\n","titlebar")
  tktag.configure(txt,"titlebar",background="darkblue",foreground="#AAAAAA")
  tktag.add(txt,"muste", "1.8", "1.15")
  tktag.configure(txt,"muste",background="blue",foreground="white")

  for (i in (0:(editarea.height-1))) {
    tkinsert(txt,"end",sprintf(fmt="%6d ",i+editfield.showy))
    tkinsert(txt,"end",paste(editfield[showy+i,1],sep=""))
    tkinsert(txt,"end",paste(paste(editfield[showy+i,][(showx+1):(showx+1+editarea.width)],sep="",collapse=""),"\n"))
  }
  tkinsert(txt,"end","\n")
  tkinsert(txt,"end","START MENU DEMO HELP SM UDLR pUpD SC JOBS MAIN SYSTEM NEWS e cp F  OWN OFF  EXIT\n")
  tkinsert(txt,"end","Kokeilua TCL/TK:lla","note")
  tktag.add(txt,"pehmo", "26.0", "26.end")
  tktag.configure(txt,"pehmo",background="blue",foreground="white")
  tktag.configure(txt,"note",background="yellow",foreground="black")
}

create.editfield <- function(width,rows,shadowrows) {
  editfield.width<<-as.numeric(width)
  editfield.rows<<-as.numeric(rows)
  editfield.shadowrows<<-as.numeric(shadowrows)
  cat("Creating edit field:\nWidth:",width,"\nRows:",rows,"\nShadowrows:",shadowrows,"\n")
  editfield<<-matrix(nrow=rows,ncol=width)
  for (i in 1:width) for (j in 1:rows) editfield[j,i]<<-c(" ")
  for (j in 1:rows) editfield[j,1]<<-c("*")
for (i in 1:width) editfield[1,i+1]<<-substr(as.character(i),1,1)
  editfield.showy<<-1
  editfield.showx<<-1
  editfield.cursory<<-1
  editfield.cursorx<<-2

}

load.editfield <- function(tiedosto) {
  filecon<-file(tiedosto, open="r")
  pos<-seek(filecon, rw="r")
  tt<-readLines(filecon, n=1)
  tt2<-unlist(strsplit(unlist(strsplit(gsub(': ',':',gsub('  +*',' ',tt)),':'))[2],' '))
  create.editfield(as.numeric(tt2[1]),as.numeric(tt2[2]),as.numeric(tt2[3]))
  pos<-seek(filecon, rw="r")
  tt<-readLines(filecon, n=1)

  while (!identical(tt,character(0))) {
    tt2<-unlist(strsplit(tt,'\\|'))
    if (!identical(tt2[1],"S   ")) {
      rivi<-as.numeric(tt2[1])
      pituus<-nchar(tt2[2])
      tt3<-unlist(strsplit(tt2[2],'|'))
      for (i in 1:pituus) editfield[rivi,i]<<-tt3[i]
    }
    pos<-seek(filecon, rw="r")
    tt<-readLines(filecon, n=1)
  }
  close.connection(filecon)
  print.editfield(1,1)
}

getCursor <- function() {
  as.numeric(unlist(strsplit(as.character(tkindex(txt,"insert")),"\\.")))
}

setCursor <- function(cursor) {
  tkmark.set(txt,"insert",paste(as.character(cursor[1]),".",as.character(cursor[2]),sep=""))
}

aktivointi <- function() {
  cursor<-getCursor()
  rivi <- cursor[1] # strsplit(as.character(tkindex(txt,"insert")),"\\.")[[1]][1]
  rivi.alku <- paste(rivi,".8",sep='')
  rivi.loppu <- paste(rivi,".end",sep='')
  input <- gsub('\n','',gsub('  +*',' ',tclvalue(tkget(txt,rivi.alku,rivi.loppu))))
  komentosanat <- unlist(strsplit(input,' '))
#  cat("Komentosanat",komentosanat[1],komentosanat[2],sep='\n')

  if (identical(substr(input,1,2),'R>')) {                        # R-komento
    code=substr(input,3,nchar(input))
    execute(code)
  } else
  if (identical(komentosanat[1],'MAT')) {                    # Matriisikomennot
    komento <- unlist(strsplit(komentosanat[2],'='))
    if (!is.na(komento[2])) {
      dest <- komento[1]
      oikea <- unlist(strsplit(komento[2],'\\('))
      if (!is.na(oikea[2])) {
        param <- unlist(strsplit(gsub('\\)','',oikea[2]),','))
        if (identical(oikea[1],'CON')) {
          if(is.na(param[3])) { param[3] <- 1 }
          code <- paste('mat',dest,'<-matrix(',param[3],',',param[1],',',param[2],')',sep='')
          execute(code)
        }
      }
    }
  } else
  if (identical(komentosanat[1],'LOAD')) {                    # LOAD
    tiedosto<-paste(komentosanat[2],".EDT",sep="")
    load.editfield(tiedosto)
  }
}

checkeditboundaries <- function() {
  if(editfield.cursorx>editfield.width) editfield.cursorx<<-editfield.width
  if(editfield.cursory>editfield.rows) editfield.cursory<<-editfield.rows
  if(editfield.cursorx<1) editfield.cursorx<<-1
  if(editfield.cursory<1) editfield.cursory<<-1

  if(editfield.showx>(editfield.width-editarea.width)) editfield.showx<<-(editfield.width-editarea.width)
  if(editfield.showy>(editfield.rows-editarea.height+1)) editfield.showy<<-(editfield.rows-editarea.height+1)
  if(editfield.showx<1) editfield.showx<<-1
  if(editfield.showy<1) editfield.showy<<-1
}

OnKey <- function()
{
cursor <- getCursor()
# print.editfield(editfield.showx,editfield.showy)
}

OnEnter <- function() {
  cursor<-getCursor()

#  cat("Entering enter",cursor[1],cursor[2],sep="\n")

  editfield.cursorx<<-editfield.showx
  editfield.cursory<<-editfield.cursory+1

  if(cursor[1]>editarea.height) {
    editfield.showy<<-(editfield.showy+1)
    cursor[1]<-(cursor[1]-1)
  }
  checkeditboundaries()
  print.editfield(editfield.showx,editfield.showy)
  setCursor(c((cursor[1]+1),8))
}


OnDown <- function() {
  cursor<-getCursor()

  editfield.cursorx<<-editfield.cursorx
  editfield.cursory<<-editfield.cursory+1

  if(cursor[1]>editarea.height) {
    editfield.showy<<-(editfield.showy+1)
    cursor[1]<-(cursor[1]-1)
  }
  checkeditboundaries()
  print.editfield(editfield.showx,editfield.showy)
  setCursor(c((cursor[1]+1),cursor[2]))
}

OnUp <- function() {
  cursor<-getCursor()

  editfield.cursorx<<-editfield.cursorx
  editfield.cursory<<-editfield.cursory-1

  if(cursor[1]<3) {
    editfield.showy<<-(editfield.showy-1)
    cursor[1]<-(cursor[1]+1)
  }
  checkeditboundaries()
  print.editfield(editfield.showx,editfield.showy)
  setCursor(c((cursor[1]-1),cursor[2]))
}

OnRight <- function() {
  cursor<-getCursor()

  editfield.cursorx<<-editfield.cursorx+1
  editfield.cursory<<-editfield.cursory

  if(cursor[2]==(editarea.width+8-1)) {
    editfield.showx<<-(editfield.showx+1)
    cursor[2]<-(cursor[2]-1)
  }
  checkeditboundaries()
  print.editfield(editfield.showx,editfield.showy)
  setCursor(c(cursor[1],(cursor[2]+1)))
}

OnLeft <- function() {
  cursor<-getCursor()
  
  if(editfield.cursorx<3) {
    if(cursor[2]<8) {
      cursor[2]<-(cursor[2]+1)
    }
  } else
  if(cursor[2]<9) {
    editfield.showx<<-(editfield.showx-1)
    cursor[2]<-(cursor[2]+1)
  }

  editfield.cursorx<<-editfield.cursorx-1
  editfield.cursory<<-editfield.cursory

  checkeditboundaries()
  print.editfield(editfield.showx,editfield.showy)
  setCursor(c(cursor[1],(cursor[2]-1)))
}



OnPehmoEnter <- function()
{
	tkdelete(txt,"27.0","27.end")
        tkinsert(txt,"27.0","Pehmonapisto","muste")
}

OnPehmoLeave <- function()
{
	tkdelete(txt,"27.0","27.end")
        tkinsert(txt,"27.0","Kokeilua TCL/TK:lla","note")
}


ikkuna <<- tktoplevel()
tkwm.title(ikkuna, "     MUSTE       Käyttöoikeus: Vapaa    - 10.2.2009, ver.0.1")
txt <<- tktext(ikkuna,width=80,height=27,foreground="#CCCCCC",background="#FEFEFE",wrap="none",font="courier")
tkgrid(txt)
editarea.height<<-23
editarea.width<<-72

# Poistetaan text-widgetin perussidokset käytöstä
sidokset <- gsub("Text ","",tclvalue(tkbindtags(txt)))
tkbindtags(txt,sidokset)

create.editfield(101,300,300)
print.editfield(1,1)
setCursor(c(2,8))

#tkconfigure(txt, state="disabled")


tkbind(txt,"<Key>",OnKey)
tkbind(txt,"<Down>",OnDown)
tkbind(txt,"<Up>",OnUp)
tkbind(txt,"<Left>",OnLeft)
tkbind(txt,"<Right>",OnRight)
tkbind(txt,"<Return>",OnEnter)
tkbind(txt,"<Escape>",aktivointi)
tktag.bind(txt,"pehmo","<Enter>",OnPehmoEnter)
tktag.bind(txt,"pehmo","<Leave>",OnPehmoLeave)

tkfocus(txt)


