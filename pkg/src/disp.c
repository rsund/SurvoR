#include <R.h>
#include <Rinternals.h>

#include <string.h>
#include "survo.h"

extern SEXP Muste_EvalRExpr();

extern unsigned char *shadow_code;
extern int display_off;

static char komento[256];

int sur_locate(int row,int col)
{
   sprintf(komento,"MusteSetCursor(%d,%d)",col-1,row);
   Muste_EvalRExpr(komento);
   return(1);
}

/*
        COORD coords;

        coords.X=col-1; coords.Y=row-1;
        SetConsoleCursorPosition(hStdOut,coords);
        return(1);
        }
*/

void muste_flushscreen() {
    sprintf(komento,".Tcl(\"update idletasks\")");
//    sprintf(komento,"tcl('update')");
//    sprintf(komento,"Sys.sleep(0.1)");
    Muste_EvalRExpr(komento);
}

int write_string(char *x, int len, char shadow, int row, int col)
    {
    if (display_off) return(1);

/* extern Tcl_Interp *RTcl_interp; 
Tcl_Eval(RTcl_interp, "wm withdraw .");
*/

    const char kom[] = ".1.1 delete 1.0 end"; // delete 1.0 end";
    SEXP alist;
    alist = R_NilValue;
    PROTECT(alist);
    alist = LCONS(mkString(kom), alist);
//    PrintValue(CAR(alist));
    PrintValue(CADR(alist));
    PrintValue(CADDR(alist));
    if(!isValidString(CADR(alist))) error("String ei kelpaa\n");
    UNPROTECT(1);
//    dotTcl(alist);


/*
    SEXP teksti;
    PROTECT(teksti = allocVector(STRSXP, 1));
    SET_STRING_ELT(teksti, 0, mkChar(kom));
    dotTcl(teksti);
    UNPROTECT(1);
*/
    char y[2*LLENGTH];
    *y=EOS;
    strncat(y,x,len); 

/*
    SEXP teksti;
    PROTECT(teksti = allocVector(STRSXP, 1));
    SET_STRING_ELT(teksti, 0, mkCharCE(y,CE_NATIVE));
*/

/* RS turhana pois
    unsigned char sha;
    int i;

    i=(int)shadow; if (i<0) i+=256;
    sha=shadow_code[i]; */

/* Pitäisi ottaa nykyinen kursorin paikka talteen */
    sur_locate(row,col);

    sprintf(komento,"tkdelete(txt,\"%d.%d\",\"%d.%d\")",row,col-1,row,col-1+len);
/* Rprintf("delkom: %s\n",komento); */
    Muste_EvalRExpr(komento);

    sprintf(komento,"tkinsert(txt,\"%d.%d\",\"%s\",\"shadow%d\")",
            row,col-1,y,(unsigned char) shadow);
/* Rprintf("inskom: %s\n",komento); */
    Muste_EvalRExpr(komento);


/* Ja tässä palauttaa kursori oikealle paikalleen */

/*    UNPROTECT(1);  */
    return(len);
    }

/*
    bufSize.X=len;
    bufSize.Y=1;
    sr0.Left=col-1; sr0.Top=row-1;
    sr0.Right=col-1+len-1;
    sr0.Bottom=row-1;
    dwBufferCoord.X=0;
    dwBufferCoord.Y=0;
    for (i=0; i<len; ++i)
        {
        ci[i].Char.AsciiChar=x[i];
        ci[i].Attributes=sha;
        }
    i=WriteConsoleOutput(hStdOut,ci,bufSize,dwBufferCoord,&sr0);
*/



int nextch()
{
   sprintf(komento,"MusteGetKey()");
   return(INTEGER(Muste_EvalRExpr(komento))[0]);

}


