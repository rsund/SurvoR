.muste.highlightstyles <- function(x)
    {
    desc   <- x$token
    styles <- character( length( desc ) )

    styles[ desc %in% c("COMMENT","LINE_DIRECTIVE") ] <- "comment"

    styles[ grepl( "^'.*?'$", desc ) ] <- "keyword"
    styles[ desc %in% c( "FUNCTION", "FOR", "IN", "IF", "ELSE",
             "WHILE", "NEXT", "BREAK", "REPEAT", "AND", "AND2",
             "OR", "OR2", "GT", "LT", "GE", "LE", "LBB", "NE", "EQ",
             "SPECIAL", "NS_GET_INT", "NS_GET", "LOW", "NOT", "UNOT",
             "TILDE", "UPLUS", "UMINUS" ) ] <- "keyword"

    styles[ desc %in% c("STR_CONST","INCOMPLETE_STRING") ] <- "string"
    styles[ desc %in% c("NUM_CONST","NULL_CONST") ] <- "number"

    styles[ desc == "SYMBOL_FUNCTION_CALL" ] <- "functioncall"
    styles[ desc %in% c("SYMBOL_SUB", "EQ_SUB" )  ] <- "argument"
    styles[ desc == "SYMBOL_PACKAGE" ] <- "package"

    styles[ desc %in% c("SYMBOL_FORMALS") ] <- "formalargs"
    styles[ desc %in% "EQ_FORMALS" ] <- "eqformalargs"

    styles[ desc %in% c("EQ_ASSIGN", "LEFT_ASSIGN",
              "RIGHT_ASSIGN", "COLON_ASSIGN" )] <- "assignement"

    styles[ desc == "SYMBOL" ] <- "symbol"
    styles[ desc == "SLOT" ] <- "slot"

    styles[ desc %in% c("ERROR","END_OF_INPUT") ] <- "error"

    styles
    }
    
.muste.rbuffonts <- function()
    {
    if(exists("fontb",where=.muste))
      {
      tkfont.delete(.muste$fontb)
      rm("fontb",envir=.muste)
      }
    f<-paste(tkfont.actual(.muste$font))
    .muste$fontb <- tkfont.create(f[1],f[2],f[3],as.character(as.numeric(f[4])-1),
                                  f[5],"bold",f[7],f[8],f[9],f[10],f[11],f[12])

    if(exists("fonti",where=.muste))
      {
      tkfont.delete(.muste$fonti)
      rm("fonti",envir=.muste)
      }
    f<-paste(tkfont.actual(.muste$font))
    .muste$fonti <- tkfont.create(f[1],f[2],f[3],as.character(as.numeric(f[4])-1),
                                  f[5],f[6],f[7],"italic",f[9],f[10],f[11],f[12])
    }

.muste.defaulthighlightstyle <- function(bkgr="#F0FFF0")
    {
    .muste.rbuffonts()
    tktag.configure(.muste$txt,"rbuf",foreground="#1514B5",background=bkgr,font=.muste$fonti)    
    tktag.configure(.muste$txt,"rback",foreground=bkgr,background=bkgr)
    tktag.configure(.muste$txt,"number",foreground="#1514B5",background=bkgr)
    tktag.configure(.muste$txt,"functioncall",foreground="red",background=bkgr)
    tktag.configure(.muste$txt,"string",foreground="#9999FF",background=bkgr)
    tktag.configure(.muste$txt,"keyword",foreground="#145214",background=bkgr,font=.muste$fontb)
    tktag.configure(.muste$txt,"argument",foreground="#B13F05",background=bkgr,font=.muste$fonti)
    tktag.configure(.muste$txt,"comment",foreground="#CCCCCC",background=bkgr)
    tktag.configure(.muste$txt,"formalargs",foreground="#12B612",background=bkgr)
    tktag.configure(.muste$txt,"eqformalargs",foreground="#12B612",background=bkgr)
    tktag.configure(.muste$txt,"assignement",foreground="#4D3762",background=bkgr,font=.muste$fontb)
    tktag.configure(.muste$txt,"package",foreground="#96B625",background=bkgr)
    tktag.configure(.muste$txt,"slot",foreground="black",background=bkgr,font=.muste$fonti)
    tktag.configure(.muste$txt,"symbol",foreground="black",background=bkgr)
    tktag.configure(.muste$txt,"prompt",foreground="black",background=bkgr)
    tktag.configure(.muste$txt,"error",foreground="white",background="red")
    }

# if (getRversion() < "3.0.0") GetParseData <- function(...) {}

.muste.highlight <- function(file, firstvisible=1, lastvisible=NA, firstxvisible=1,
                             screenmin=1, screenmax=NA, screenwidth=72)
    {
    if (getRversion() < "3.0.0")
        {
        .muste.rbuffonts()
        tktag.add(.muste$txt,"rbuf",paste(screenmin+1,".0",sep=""),paste(screenmax+1,".end",sep=""))    
        tktag.raise(.muste$txt,"rbuf")
        tktag.raise(.muste$txt,"shadow9999") 
        }
    else
        {
        a <- try(parse(file),silent=TRUE)
    # if parse fails something should be done here    
    #    a <- subset(getParseData(a,includeText=NA),terminal==TRUE & line1>=firstvisible)
        a <- getParseData(a,includeText=NA)
        a <- a[which(a$terminal==TRUE & a$line1>=firstvisible),]
        style <- .muste.highlightstyles(a)

        bx <- 7  # Coordinates to correct place
        by <- screenmin
        sta <- paste(a$line1-firstvisible+1+by, ".", a$col1+bx, sep="")
        end <- paste(a$line2-firstvisible+1+by, ".", a$col2+bx+1, sep="")
    # too wide or long should be checked and dropped    

        tab<-rbind(style,sta,end)
        apu <- unique(style)
        tul <- lapply(1:length(apu),function(i) c(apu[i],as.vector(tab[2:3,style==apu[i]])))

        .muste.rbuffonts()
        tktag.add(.muste$txt,"rback",paste(screenmin+1,".0",sep=""),paste(screenmax+1,".end",sep=""))    
        eval(parse(text=sub("c(","tktag.add(.muste$txt,",tul,fixed=TRUE)))

        tktag.lower(.muste$txt,"rback")
        tktag.lower(.muste$txt,"shadow32")
        tktag.lower(.muste$txt,"shadow9999") 
        }   
    }
