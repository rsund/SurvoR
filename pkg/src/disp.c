#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <string.h>
#include "survo.h"

extern SEXP Muste_EvalRExpr();
int muste_iconv();

extern unsigned char *shadow_code;
extern int display_off;

extern int c3,c;
extern int r3,r;
extern int sdisp;
extern int scroll_line;
extern int space_break;

static char komento[3*LLENGTH]; /* 256 */
static char tclkomento[3*LLENGTH]; /* 256 */

char muste_window[64] = "";
/* int muste_cursor_row=1;
int muste_cursor_col=1;
*/


DL_FUNC RdotTcl = NULL;

int Muste_EvalTcl(char *komento, int ikkuna) 
{
//    extern SEXP dotTcl();

    SEXP alist,aptr;

    if (RdotTcl == NULL) // RdotTcl = R_GetCCallable("tcltk", "dotTcl");
    RdotTcl = R_FindSymbol("dotTcl","tcltk",NULL);


    if (strlen(muste_window)<2)
    {
    SEXP avar=R_NilValue;
    avar = findVar(install("muste.window"),R_GlobalEnv);
    strcpy(muste_window,CHAR(STRING_ELT(avar,0)));
    strcat(muste_window," ");
/* Rprintf("Löytyi ikkuna: %s\n",muste_window); */
    }

    if(!ikkuna) strcpy(tclkomento,komento);
    else 
    {
      strcpy(tclkomento,muste_window);
      strcat(tclkomento,komento);
    }
/* Rprintf("Komento: %s\n",tclkomento);
*/

    PROTECT(alist = allocList(2));
    aptr=alist;
    aptr=CDR(aptr); 
    SETCAR(aptr, mkString(tclkomento));
    RdotTcl(alist);
    UNPROTECT(1);
    return(1);
}



int sur_locate(int row,int col)
{
    sprintf(komento,"mark set insert %d.%d",row,col-1);
    Muste_EvalTcl(komento,TRUE);
    return(1);
}

void cursor(unsigned int r,unsigned int c)
        {
        if (c>c3) c=c3;
        sur_locate(r+1,c+8);
        }


int sur_cursor_position(int *prow,int *pcol)
        {
    SEXP avar=R_NilValue;

    sprintf(komento,"MusteGetCursor()");
    Muste_EvalRExpr(komento);

    avar = findVar(install("muste.cursor.col"),R_GlobalEnv);
    *prow=INTEGER(avar)[0];

    avar = findVar(install("muste.cursor.row"),R_GlobalEnv);
    *pcol=1+INTEGER(avar)[0];

        return(1);
        }

/*
        GetConsoleScreenBufferInfo(hStdOut,&buffer_info);
        *prow=buffer_info.dwCursorPosition.Y+1;
        *pcol=buffer_info.dwCursorPosition.X+1;
*/

int sur_cursor_move(int drow,int dcol)
        {
        int row,col;
        sur_cursor_position(&row,&col);
        sur_locate(row+drow,col+dcol);
        return(1);
        }

void muste_flushscreen() {
    sprintf(komento,"update idletasks");
    Muste_EvalTcl(komento,FALSE);
}

int write_string(char *x, int len, char shadow, int row, int col)
    {
    if (display_off) return(1);

/* RS turhana pois
    unsigned char sha;
    int i;

    i=(int)shadow; if (i<0) i+=256;
    sha=shadow_code[i]; */

    char y[2*LLENGTH];
//    *y=EOS;
//    strncat(y,x,len);
    int i,j;

    for (i=0, j=0; i<len; i++) {
       if (x[i]==34 || x[i]==91 || x[i]==92 ) y[j++]=92;
       y[j++]=x[i];
    }
    y[j]=EOS;

    muste_iconv(y,"","CP850");

//    if (col<1) col=1;

// RS Tämä näyttäisi olevan turha:    sur_locate(row,col);

    sprintf(komento,"delete %d.%d %d.%d",row,col-1,row,col-1+len);
    Muste_EvalTcl(komento,TRUE);

    sprintf(komento,"insert %d.%d \"%s\" shadow%d",row,col-1,y,(unsigned char) shadow);
    Muste_EvalTcl(komento,TRUE);


/* Suora kutsu Tcl:n parseriin
    SEXP alist,aptr;
    PROTECT(alist = allocList(2));

    sprintf(komento,".1.1 delete %d.%d %d.%d",row,col-1,row,col-1+len);
    aptr=alist;
    aptr=CDR(aptr); 
    SETCAR(aptr, mkString(komento));
    dotTcl(alist);

    sprintf(komento,".1.1 insert %d.%d \"%s\" shadow%d",row,col-1,y,(unsigned char) shadow);
    aptr=alist;
    aptr=CDR(aptr); 
    SETCAR(aptr, mkString(komento));
    dotTcl(alist);

    UNPROTECT(1);
*/

/* HIDAS R:n kautta kierrättävä tulostus 
    sprintf(komento,"tkdelete(txt,\"%d.%d\",\"%d.%d\")",row,col-1,row,col-1+len);
    Muste_EvalRExpr(komento);

    sprintf(komento,"tkinsert(txt,\"%d.%d\",\"%s\",\"shadow%d\")",
            row,col-1,y,(unsigned char) shadow);
    Muste_EvalRExpr(komento);
*/



/* RS Palautetaan kursori oikealle paikalleen */
//    sur_locate(muste_cursor_row,muste_cursor_col);

    return(len);
    }


int sur_erase(unsigned char color)
        {
        int i,row,col;
        char x[256];

        for (i=0; i<256; ++i) x[i]=' ';
        sur_cursor_position(&row,&col);
        write_string(x,c3+8+1-col,color,row,col);
        return(1);
        }


int sur_scroll_up(int lines,int row1,int col1,int row2,int col2,int attr)
    {
//    char tyhja[2*LLENGTH]="----------------------------------------------\n";
    int i;

    sprintf(komento,"delete %d.0 %d.0",row1-1,row1);
    Muste_EvalTcl(komento,TRUE);

/*    for (i=0; i<col2-col1; i++) tyhja[i]=' ';
    tyhja[i+1]='\n';
    tyhja[i+2]=EOS;
*/
    sprintf(komento,"insert %d.0 \" \n\"",row2);
    Muste_EvalTcl(komento,TRUE);

    return(1);
    }

int sur_scroll_down(int lines,int row1,int col1,int row2,int col2,int attr)
    {
    char tyhja[2*LLENGTH];
    int i;

    sprintf(komento,"delete %d.0 %d.0",row2+1,row2+2);
    Muste_EvalTcl(komento,TRUE);

/*
    for (i=0; i<col2-col1; i++) tyhja[i]=' ';
    tyhja[i+1]='\n';
    tyhja[i+2]=EOS;
*/
//    sprintf(komento,"insert %d.0 %s",row1,tyhja);
    sprintf(komento,"insert %d.0 \" \n\"",row1);
    Muste_EvalTcl(komento,TRUE);

    return(1);
    }


/*
    sr0.Left=col1-1; sr0.Right=col2-1;
    sr0.Top=row1-1; sr0.Bottom=row2-1;
    bufSize.X=col1-1;
    bufSize.Y=row1-1-lines;
    ci2.Char.AsciiChar=' ';
    ci2.Attributes=attr;

    i=ScrollConsoleScreenBuffer(hStdOut,&sr0,NULL,bufSize,&ci2);
    Sleep(2);
*/

/* suunta: 6=up 7=down */
int sur_scroll(int r1,int r2,int n,int suunta)
        {
        if (display_off) return(1);

        if (suunta==7) { n=-n; sur_scroll_down(n,r1+1,0,r2,c3+8,119); }
        else sur_scroll_up(n,r1+2,0,r2+1,c3+8,119);
        return(1);
        }

int sur_cls(unsigned char color)
        {
        int i;
        char x[256];
        extern int r_soft, r3, c3; /* RS Mistä nämä löytyvät? Tuntuvat kuitenkin toimivan */

        if (!display_off)
            {
            for (i=0; i<256; ++i) x[i]=' ';
            for (i=1; i<=r3+2+r_soft; ++i)
                write_string(x,c3+8,color,i,1);
            }
        sur_locate(1,1);
        return(1);
        }

static int sur_print2(char *text,int lf)
        {
        int len,row,col,tila;
        char *p;

        p=text;
        while (*p)
            {
            len=strlen(p);
            sur_cursor_position(&row,&col);
            tila=c3+8+1-col;

            if (tila<=len)
                {
                write_string(p,tila,(unsigned char)sdisp,row,col);
                ++row; p+=tila;
                if (row>r3+1)
                    {
                                                /*   2  */
                    sur_scroll_up(1,scroll_line+1,1,r3+1,c3+8,(int)shadow_code[sdisp]);

                    sur_locate(r3+1,1);
                    }
                else
                    {
                    sur_locate(row,1);
                    }
                }
            else
                {
                write_string(p,len,(unsigned char)sdisp,row,col);
                sur_locate(row,col+len);
                p+=len;
                }
            }
        if (lf)
            {
            sur_cursor_position(&row,&col);
            ++row;
            if (row>r3+1)
                {
                sur_scroll_up(1,scroll_line+1,1,r3+1,c3+8,(int)shadow_code[sdisp]);
                sur_locate(r3+1,1);
                }
            else
                {
                sur_locate(row,1);
                }
            }
        return(1);
        }

int sur_print(char *text)
        {
        int i,m;
        char *p,*q;
        char x[LLENGTH];

        strncpy(x,text,LLENGTH); x[LLENGTH-1]='\0';
        p=x;
        while (*p)
            {
            q=strchr(p,'\n');
            if (q==NULL) { sur_print2(p,0); break; }
            *q='\0'; sur_print2(p,1);
            p=q+1;
            }
/* RS näppäimistön hallinta toistaiseksi pois tästä
        if (space_break && kbhit())
            {
            i=getch();
            if (i==(int)' ')
                {
                sur_print2(" ",1); sur_print2("Continue operation (Y/N)? ",0);
                m=getch(); if (m==(int)'N' || m==(int)'n') exit(1);
                sur_print2("Y",1);
                }
            else ungetch(i);
            }
*/
        return(1);
        }


