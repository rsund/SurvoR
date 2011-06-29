#include <R.h>
#include <Rinternals.h>

#include <string.h>
#include "survo.h"

extern SEXP Muste_EvalRExpr();

extern unsigned char *shadow_code;
extern int display_off;

static char komento[3*LLENGTH]; /* 256 */

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

/* RS turhana pois
    unsigned char sha;
    int i;

    i=(int)shadow; if (i<0) i+=256;
    sha=shadow_code[i]; */


    SEXP alist,aptr;
    char y[2*LLENGTH];
    *y=EOS;
    strncat(y,x,len); 

//    const char kom[] = ".1.1 delete 1.0 end"; // delete 1.0 end";

    PROTECT(alist = allocList(2));

    sur_locate(row,col);
    sprintf(komento,".1.1 delete %d.%d %d.%d",row,col-1,row,col-1+len);



    aptr=alist;
//    SETCAR(aptr, install("koe1"));
    aptr=CDR(aptr); 
    SETCAR(aptr, mkString(komento));
//    PrintValue(CADR(alist));
//    if(!isValidString(CADR(alist))) error("String ei kelpaa\n");
    dotTcl(alist);

    sprintf(komento,".1.1 insert %d.%d \"%s\" shadow%d",row,col-1,y,(unsigned char) shadow);
    aptr=alist;
    aptr=CDR(aptr); 
    SETCAR(aptr, mkString(komento));
    dotTcl(alist);

    UNPROTECT(1);



/* Pitäisi ottaa nykyinen kursorin paikka talteen */
//    sur_locate(row,col);

//    sprintf(komento,"tkdelete(txt,\"%d.%d\",\"%d.%d\")",row,col-1,row,col-1+len);
/* Rprintf("delkom: %s\n",komento); */
//    Muste_EvalRExpr(komento);

//    sprintf(komento,"tkinsert(txt,\"%d.%d\",\"%s\",\"shadow%d\")",
//            row,col-1,y,(unsigned char) shadow);
//    Muste_EvalRExpr(komento);


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


