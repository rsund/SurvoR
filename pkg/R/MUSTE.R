#require(tcltk)

execute <- function(code) {
  e <- try(parse(text=code))
  if (inherits(e, "try-error")) {
    cat("-----","Syntax error while executing from MUSTE:",code,sep="\n")
    return()
  }
  cat("-----","Executing from MUSTE:",code,"-----","result:", sep="\n")
  print(eval(e, envir=muste.environment))
}

getCursor <- function() {
  as.numeric(unlist(strsplit(as.character(tkindex(txt,"insert")),"\\.")))
}

setCursor <- function(cursor) {
  tkmark.set(txt,"insert",paste(as.character(cursor[1]),".",as.character(cursor[2]),sep=""))
}

MusteSetCursor <- function(row,col) {
  tkmark.set(txt,"insert",paste(as.character(col),".",as.character(row),sep=""))
}

koe <- function() {
.Call("Muste_SetCursorKoe",as.integer(10),as.integer(10))
}

wri <- function() {
.Call("Muste_Write",as.integer(10),as.integer(10),as.integer(1))
}


print.header <- function() {
  cursor<-getCursor()
  tkdelete(txt,"1.0","1.end")
  paiva<-date()
  tkinsert(txt,"1.0",paste("    1  1 Muste      ",paiva," C:\\MUSTE                2000  100 0",sep=""),"titlebar")
  setCursor(c(cursor[1],cursor[2]))
  tktag.configure(txt,"titlebar",background="darkblue",foreground="#AAAAAA")
  tktag.add(txt,"muste", "1.8", "1.15")
  tktag.configure(txt,"muste",background="blue",foreground="white")
}

print.editfield <- function(showx,showy) {

  tkdelete(txt,"1.0","end")
  print.header()
  tkinsert(txt,"end","\n")
  for (i in (0:(editarea.height-1))) {
    tkinsert(txt,"end",sprintf(fmt="%6d ",i+editfield.showy),"predit")
    tkinsert(txt,"end",paste(editfield[showy+i,1],sep=""), "predit")
    tkinsert(txt,"end",paste(paste(editfield[showy+i,][(showx+1):(showx+1+editarea.width)],sep="",collapse=""),"\n"))
  }
  tktag.configure(txt,"predit",background="#FEFEFE",foreground="#CCCCCC")
  tkinsert(txt,"end","\n")
  tkinsert(txt,"end","START MENU DEMO HELP SM UDLR pUpD SC JOBS MAIN SYSTEM NEWS e cp F  OWN OFF  EXIT\n")
  tkinsert(txt,"end","Kokeilua TCL/TK:lla","note")
  tktag.add(txt,"pehmo", "26.0", "26.end")
  tktag.configure(txt,"pehmo",background="blue",foreground="white")
  tktag.configure(txt,"note",background="yellow",foreground="black")

}

print.editline <- function(showx,showy) {
  cursor<-getCursor()

# cat("Tyhjennys:",paste(cursor[1],".8\n",sep=""))


tkdelete(txt,paste(cursor[1],".7",sep=""),paste(cursor[1],".end",sep=""))
tkinsert(txt,paste(cursor[1],".7",sep=""),paste(editfield[editfield.cursory,1],sep=""),"predit")
tkinsert(txt,paste(cursor[1],".8",sep=""),paste(editfield[editfield.cursory,][(showx+1):(showx+1+editarea.width-1)],sep="",collapse=""))

}


create.editfield <- function(width,rows,shadowrows) {
  editfield.width<<-as.numeric(width)
  editfield.rows<<-as.numeric(rows)
  editfield.shadowrows<<-as.numeric(shadowrows)
#  cat("Creating edit field:\nWidth:",width,"\nRows:",rows,"\nShadowrows:",shadowrows,"\n")
  editfield<<-matrix(nrow=rows,ncol=width)
  for (i in 1:width) for (j in 1:rows) editfield[j,i]<<-c(" ")
  for (j in 1:rows) editfield[j,1]<<-c("*")
}

load.editfield <- function(tiedosto) {
  filecon<-file(tiedosto, open="r", encoding="CP850")
  pos<-seek(filecon, rw="r")
  tt<-readLines(filecon, n=1)
  tt2<-unlist(strsplit(unlist(strsplit(gsub(': ',':',gsub('  +*',' ',tt)),':'))[2],' '))

  create.editfield(as.numeric(tt2[1]),as.numeric(tt2[2]),as.numeric(tt2[3]))
  pos<-seek(filecon, rw="r")
  tt<-readLines(filecon, n=1)
  tt <- iconv(tt, "CP850","ISO8859-1")

  while (!identical(tt,character(0))) {
    tt2<-unlist(strsplit(tt,'\\|'))
    if (!identical(tt2[1],"S   ")) {
      rivi<-as.numeric(tt2[1])
      apu <- grep("\\|",unlist(strsplit(tt,NULL)))
      actualine<-substr(tt,apu[1]+1,nchar(tt))
      pituus<-nchar(actualine)
      tt3<-unlist(strsplit(actualine,NULL))
      for (i in 1:pituus) editfield[rivi,i]<<-tt3[i]
    }
    pos<-seek(filecon, rw="r")
    tt<-readLines(filecon, n=1)
    tt <- iconv(tt, "CP850","ISO8859-1")
  }
  close.connection(filecon)
  cursor<-getCursor()
  print.editfield(editfield.showx,editfield.showy)
  setCursor(c(cursor[1],cursor[2]))
}

save.editfield <- function(tiedosto) {
  filecon<-file(tiedosto, open="w", encoding="CP850")
  otsikko <- paste("SURVO 98 edit field:",editfield.width,editfield.rows,editfield.shadowrows,"(32 bit version)")
#  writeLines(otsikko,filecon,sep="\r\n")
  writeLines(otsikko,filecon,sep="\n")
  for (i in 1:editfield.rows) {
    editline<-paste(editfield[i,][1:(editfield.width)],sep="",collapse="")
    editline<-sub(' +$','', editline)
    if (nchar(editline)>1) {
      editline<-paste(i,"|",editline,sep="")
#      writeLines(editline,filecon,sep="\r\n")
      writeLines(editline,filecon,sep="\n")
    }
  }
  close.connection(filecon)
}

load.dump <- function() {
#load.dump <- function(tiedosto) {
#   load(system.file("data","muste.dump.Rdata",package="muste"),envir=muste.environment)
#  filecon<-file(tiedosto, open="r", encoding="CP850")
#  muste.dump<<-readLines(filecon)
#  close.connection(filecon)
data(muste.dump)
}

save.dump <- function(tiedosto) {
  filecon<-file(tiedosto, open="w", encoding="CP850")
  muste.dump[3]<<-as.character(editfield.cursory)
  muste.dump[8]<<-as.character(editfield.cursorx-1)
#  writeLines(muste.dump,filecon,sep="\r\n")
  writeLines(muste.dump,filecon,sep="\n")


#  alkurivi<-as.character(c(0,0,editfield.cursory,1,0,0,0,(editfield.cursorx-1)))
#  writeLines(alkurivi,filecon,sep="\r\n")
#  tokarivi<-as.character(c(0,0,0,"C:\\SURVO\\U\\D\\","C:","C:\\Survo\\U\\TMP\\RESULTS",0,"C:\\Survo\\S\\SURVO.TUT"))
#  writeLines(tokarivi,filecon,sep="\r\n")
#  for (k in 1:7) writeLines("0",filecon,sep="\r\n")
#  writeLines("",filecon,sep="\r\n")
#  kolmasrivi<-as.character(c(0,0,0,2,7,70))
#  writeLines(kolmasrivi,filecon,sep="\r\n")
#  for (k in 1:277) writeLines("0",filecon,sep="\r\n")
#  writeLines(c("C:\\muste\\survo\\ASURVOMM.EDT","C:\\Survo\\U\\"),filecon,sep="\r\n")
#  for (k in 1:14) writeLines("0",filecon,sep="\r\n")
#  writeLines(c("","0","",""),filecon,sep="\r\n")
  close.connection(filecon)
}

aktivointi <- function() {
  cursor<-getCursor()
  rivi <- cursor[1] # strsplit(as.character(tkindex(txt,"insert")),"\\.")[[1]][1]
  rivi.alku <- paste(rivi,".8",sep='')
  rivi.loppu <- paste(rivi,".end",sep='')
  rivi.org<-tclvalue(tkget(txt,rivi.alku,rivi.loppu))
  input <- gsub('\n','',gsub('  +*',' ',rivi.org))
  komentosanat <- unlist(strsplit(input,' ')) #  cat("Komentosanat",komentosanat[1],komentosanat[2],sep='\n')

#print(substr(rivi.org,cursor[2]-8,cursor[2]-8))

  if (identical(substr(rivi.org,cursor[2]-8,cursor[2]-8),'=')) {   # Editoriaalista laskentaa
    tiedosto<-"ASURVOMM.EDT"
    save.editfield(tiedosto)
    dump<-"ASURVOMM.DMP"
    save.dump(dump)
    args<-"A"
    .Call("Muste_EditorialArithmetics",args)
    load.editfield("ASURVOMM.EDT")
  } else

  if (identical(substr(input,1,2),'R>')) {                        # R-komento
    code=substr(input,3,nchar(input))
    execute(code)
  } else

  if (identical(substr(input,1,4),'CORR')) {   # CORR-moduli
    tiedosto<-"ASURVOMM.EDT"
    save.editfield(tiedosto)
    dump<-"ASURVOMM.DMP"
    save.dump(dump)
    args<-"A"
    .Call("Muste_CorrModule",args)
    load.editfield("ASURVOMM.EDT")
  } else

  if (identical(substr(input,1,3),'VAR')) {   # VAR-operaatiot
    tiedosto<-"ASURVOMM.EDT"
    save.editfield(tiedosto)
    dump<-"ASURVOMM.DMP"
    save.dump(dump)
    args<-"A"
    .Call("Muste_VarOperation",args)
    load.editfield("ASURVOMM.EDT")
  } else

  if (identical(substr(input,1,4),'FILE')) {   # FILE-operaatiot
    tiedosto<-"ASURVOMM.EDT"
    save.editfield(tiedosto)
    dump<-"ASURVOMM.DMP"
    save.dump(dump)
    args<-"A"
    .Call("Muste_FileShow",args)
    load.editfield("ASURVOMM.EDT")
  } else


  if (identical(substr(input,1,4),'WAIT')) {   
    .Call("Muste_WaitKoe",quote(MusteGetKey()),muste.environment)
  } else

  if (identical(substr(input,1,3),'WRI')) {   
     wri()
  } else



  if (identical(toupper(komentosanat[1]),'MAT')) {                         # Matriisikomennot
    if (identical(komentosanat[2],'LOAD')) {
       matcode<-paste("apumat<-mat",komentosanat[3],sep="")
       apumat<-eval(parse(text=matcode),envir=muste.environment)
       matnimi<-paste("MATRIX",komentosanat[3],"///")
       matheader<-unlist(strsplit(matnimi,NULL))
       editfield[(editfield.cursory+1),2:(nchar(matnimi)+1)]<<-matheader
       for (i in 1:nrow(apumat)) {
         rivi<-paste(apumat[i,],sep="",collapse=" ")
         eririvi<-unlist(strsplit(rivi,NULL))
         editfield[(editfield.cursory+1+i),2:(nchar(rivi)+1)]<<-eririvi
       }
       getCursor()
       print.editfield(editfield.showx,editfield.showy)
       setCursor(c(cursor[1],cursor[2]))
    }
    komento <- unlist(strsplit(komentosanat[2],'='))
    if (!is.na(komento[2])) {
      dest <- komento[1]
      oikea <- unlist(strsplit(komento[2],'\\('))
      if (!is.na(oikea[2])) {
        param <- unlist(strsplit(gsub('\\)','',oikea[2]),','))
        if (identical(oikea[1],'CON')) {                          # MAT A=CON(5,5,0.3)
          if(is.na(param[3])) { param[3] <- 1 }
          code <- paste('mat',dest,'<-matrix(',param[3],',',param[1],',',param[2],')',sep='')
          execute(code)
        }
      }
      oikea <- unlist(strsplit(komento[2],'\\*'))                # MAT B=A*A
      if (!is.na(oikea[2])) {
        code <- paste('mat',dest,'<-mat',oikea[1],'%*%mat',oikea[2],sep='')
        execute(code)
      }
    }
  } else
  if (identical(toupper(komentosanat[1]),'LOAD')) {                    # LOAD
    tiedosto<-paste(komentosanat[2],".EDT",sep="")
    setCursor(c(2,8))
    editfield.showx<<-1
    editfield.showy<<-1
    editfield.cursorx<<-2
    editfield.cursory<<-1
    load.editfield(tiedosto)
  } else
  if (identical(toupper(komentosanat[1]),'SAVE')) {                    # SAVE
    tiedosto<-paste(komentosanat[2],".EDT",sep="")
    save.editfield(tiedosto)
  } else {
#  if (identical(komentosanat[1],'NTERM')) {                    # NTERM
    tiedosto<-"ASURVOMM.EDT"
    save.editfield(tiedosto)
    dump<-"ASURVOMM.DMP"
    save.dump(dump)

    moduli<-paste("wine _",komentosanat[1],".exe C:/muste/survo/A A",sep="")  # Linux
#    moduli<-paste("_",komentosanat[1],".exe D:/survo/muste/A A",sep="")   # Windows

    komento<-paste("system('",moduli,"', wait=FALSE)",sep="")
    eval(parse(text=komento),envir=muste.environment)
#    system("wine _nterm.exe C:/muste/survo/A A", wait=FALSE)
    Sys.sleep(1)
#    load.dump("ASURVOMM.DMP")
    load.editfield("ASURVOMM.EDT")
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

OnKey <- function(A,K,N,k) {
  cursor <- getCursor()

cat("Merkki:",A,K,N,k,"\n")
# A = UNICODE character
# K = The keysym corresponding to the event, substituted as a textual string.
# N = The keysym corresponding to the event, substituted as a decimal number.
# k = The keycode field from the event.

  tclvalue(mustekey)<-N   # Merkki MusteGetKeylle

  if (identical(K,"BackSpace")) return()
  if (identical(K,"Delete")) return()
  if (identical(K,"Tab")) return()
  if (identical(A,"")) return()

  merkki <- iconv(A, "UTF-8","")
  if (is.na(merkki)) {
    merkki<-"?"
#    koodi<-"merkki<-'\u20ac'" #return()
#    eval(parse(text=koodi))
  }


  editfield[editfield.cursory,editfield.cursorx]<<-substr(merkki,1,1)
  print.editline(editfield.showx,editfield.showy)
  setCursor(c(cursor[1],cursor[2]))
  if (cursor[2]<editarea.width+8-1) {
    if (editfield.cursorx==1) OnDown()
    else OnRight()
  }
}

MusteGetKey <- function() {
  tclvalue(mustekey)<-0
  tkwait.variable(mustekey)
  return (as.integer(tclvalue(mustekey)))
}

OnEnter <- function() {
  cursor<-getCursor()

  editfield.cursorx<<-editfield.showx+1
  editfield.cursory<<-editfield.cursory+1

  if(cursor[1]>editarea.height) {
    editfield.showy<<-(editfield.showy+1)
    cursor[1]<-(cursor[1]-1)
    checkeditboundaries()
    print.editfield(editfield.showx,editfield.showy)
  }
  checkeditboundaries()
  setCursor(c((cursor[1]+1),8))
}


OnDown <- function() {
  cursor<-getCursor()

  editfield.cursory<<-editfield.cursory+1

  if(cursor[1]>editarea.height) {
    editfield.showy<<-(editfield.showy+1)
    cursor[1]<-(cursor[1]-1)
    checkeditboundaries()
    print.editfield(editfield.showx,editfield.showy)
  }
  checkeditboundaries()
  setCursor(c((cursor[1]+1),cursor[2]))
}

OnUp <- function() {
  cursor<-getCursor()

  editfield.cursory<<-editfield.cursory-1

  if(cursor[1]<3) {
    editfield.showy<<-(editfield.showy-1)
    cursor[1]<-(cursor[1]+1)
    checkeditboundaries()
    print.editfield(editfield.showx,editfield.showy)
  }
  checkeditboundaries()
  setCursor(c((cursor[1]-1),cursor[2]))
}

OnRight <- function() {
  cursor<-getCursor()

  editfield.cursorx<<-editfield.cursorx+1

  if(cursor[2]==(editarea.width+8-1)) {
    editfield.showx<<-(editfield.showx+1)
    cursor[2]<-(cursor[2]-1)
    checkeditboundaries()
    print.editfield(editfield.showx,editfield.showy)
  }
  checkeditboundaries()
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
    checkeditboundaries()
    print.editfield(editfield.showx,editfield.showy)
  }

  editfield.cursorx<<-editfield.cursorx-1
  checkeditboundaries()
  setCursor(c(cursor[1],(cursor[2]-1)))
}

OnPageDown <- function() {
  cursor<-getCursor()
  cursordifference<-editfield.cursory-editfield.showy
  newposition<-editfield.showy+editarea.height
  if (newposition>(editfield.rows+1-editarea.height)) newposition=editfield.rows+1-editarea.height
  editfield.showy<<-newposition
  editfield.cursory<<-newposition+cursordifference
  checkeditboundaries()
  print.editfield(editfield.showx,editfield.showy)
  setCursor(c(cursor[1],cursor[2]))
}

OnPageUp <- function() {
  cursor<-getCursor()
  cursordifference<-editfield.cursory-editfield.showy
  newposition<-editfield.showy-editarea.height
  if (newposition<1) newposition=1
  editfield.showy<<-newposition
  editfield.cursory<<-newposition+cursordifference
  checkeditboundaries()
  print.editfield(editfield.showx,editfield.showy)
  setCursor(c(cursor[1],cursor[2]))
}


OnHome <- function() {
  cursor<-getCursor()
  if (editfield.cursorx<3) {
    if(cursor[1]<3) {
      editfield.cursory<<-1
      editfield.showy<<-1
      checkeditboundaries()
      print.editfield(editfield.showx,editfield.showy)
      setCursor(c(cursor[1],(cursor[2])))
      return()
    }
    setCursor(c(2,cursor[2]))
    editfield.cursory<<-editfield.showy
    checkeditboundaries()
    return()
  }
  if (editfield.showx>1) {
    if (identical(editfield.showx+1,editfield.cursorx)) {
      editfield.showx<<-1
      editfield.cursorx<<-2
      checkeditboundaries()
      print.editfield(editfield.showx,editfield.showy)
      setCursor(c(cursor[1],cursor[2]))
      return()
    }
    setCursor(c(cursor[1],8))
    editfield.cursorx<<-editfield.showx+1
    checkeditboundaries()
    return()
  }
  setCursor(c(cursor[1],8))
  editfield.cursorx<<-2
  checkeditboundaries()
}

OnEnd <- function() {
  cursor<-getCursor()
  alkupaikka<-editfield.showx
  editline<-paste(editfield[editfield.cursory,][1:(editfield.width)],sep="",collapse="")
  editline<-sub(' +$','', editline)
  pituus<-nchar(editline, type="chars")
  sivu=floor(pituus/editarea.width)
  editfield.cursorx<<-(pituus+1)
  paikka<-(sivu*editarea.width)+1
  if (pituus>=editfield.width) paikka<-paikka-1
  editfield.showx<<-paikka
  checkeditboundaries()
  if (sivu>0 | alkupaikka>1) print.editfield(editfield.showx,editfield.showy)
  kursorinpaikka<-(pituus-editfield.showx+8)
  if (pituus>=editfield.width) kursorinpaikka<-(kursorinpaikka-1)
  setCursor(c(cursor[1],kursorinpaikka))
}

OnControlEnd <- function() {
  cursor<-getCursor()
  pituus<-(editfield.width-editfield.cursorx)
  for (i in 1:pituus) editfield[editfield.cursory,(editfield.cursorx-1+i)]<<-" "
  print.editline(editfield.showx,editfield.showy)
  setCursor(c(cursor[1],cursor[2]))
}




OnBackSpace <- function() {
  if (editfield.cursorx>2) {
    if (editfield.cursorx>=editfield.width) {
      if (!identical(editfield[editfield.cursory,(editfield.cursorx)]," ")) {
        OnKey(" "," ")
        return()
      }
    }
    OnLeft()
    OnKey(" "," ")
    OnLeft()
  }
}

OnDel <- function() {
 cursor<-getCursor()
 if(editfield.cursorx>=editfield.width) return() # editfield[editfield.cursory,editfield.cursorx]<<-" "
 else {
   editline<-paste(editfield[editfield.cursory,][(editfield.cursorx+1):(editfield.width)],sep="",collapse="")
   editline<-paste(editline," ",sep="")
# print(editline)
   pituus<-nchar(editline, type="chars")
   chars<-unlist(strsplit(editline,NULL))
   for (i in 1:pituus) editfield[editfield.cursory,(editfield.cursorx-1+i)]<<-chars[i]
 }
 print.editline(editfield.showx,editfield.showy)
 setCursor(c(cursor[1],cursor[2]))
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

RightClick <- function(x,y) # x and y are the mouse coordinates
{
  cat("Right click:",as.integer(x),as.integer(y))
  rootx <- as.integer(tkwinfo("rootx",txt))
  rooty <- as.integer(tkwinfo("rooty",txt))
  xTxt <- as.integer(x)+rootx
  yTxt <- as.integer(y)+rooty
}

update.header <- function() {
cursor<-getCursor()
print.header()
setCursor(c(cursor[1],cursor[2]))
tcl("after",1000,update.header)
}

muste <- function() {
  muste.environment <<- environment()
  ikkuna <<- tktoplevel()
  tkwm.title(ikkuna, "MUSTE")
  fixedfont <<- tkfont.create(family="Courier",size=9)
  txt <<- tktext(ikkuna,width=80,height=27,foreground="#000000",background="#FEFEFE",wrap="none",font=fixedfont)
  tkgrid(txt)
  editarea.height<<-23
  editarea.width<<-72

# Napinpainallusten seuranta
  mustekey <<- tclVar(0)

#tkfont.configure(fixedfont,family="Courier",size=9)

# Poistetaan text-widgetin perussidokset käytöstä
sidokset <- gsub("Text ","",tclvalue(tkbindtags(txt)))
tkbindtags(txt,sidokset)

#load.dump("MUSTE.DMP")
load.dump()
create.editfield(101,300,300)
editfield.showy<<-1
editfield.showx<<-1
editfield.cursory<<-1
editfield.cursorx<<-2
print.editfield(1,1)
setCursor(c(2,8))

tkbind(txt,"<Key>",OnKey)
tkbind(txt,"<Next>",OnPageDown)
tkbind(txt,"<Prior>",OnPageUp)
tkbind(txt,"<Home>",OnHome)
tkbind(txt,"<End>",OnEnd)
tkbind(txt,"<Control-End>",OnControlEnd)
tkbind(txt,"<Down>",OnDown)
tkbind(txt,"<Up>",OnUp)
tkbind(txt,"<Left>",OnLeft)
tkbind(txt,"<Right>",OnRight)
tkbind(txt,"<Return>",OnEnter)
tkbind(txt,"<Escape>",aktivointi)
tkbind(txt,"<BackSpace>",OnBackSpace)
tkbind(txt,"<Delete>",OnDel)
tktag.bind(txt,"pehmo","<Enter>",OnPehmoEnter)
tktag.bind(txt,"pehmo","<Leave>",OnPehmoLeave)
tkbind(txt, "<Button-3>",RightClick)

tktag.configure(txt,"shadow0",background="#FEFEFE",foreground="black")
tktag.configure(txt,"shadow1",background="blue",foreground="white")
tktag.configure(txt,"shadow2",background="red",foreground="yellow")


tkfocus(txt)
update.header()

}

# package.skeleton(name="muste",path="D:\",code_files=D:\SURVO\MUSTE\muste190209.txt")
