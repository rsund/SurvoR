#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <string.h>
#include "survo.h"

#define MAXPLOTWINDOWS 300
#define MAXFONTS 300

extern int muste_lopetus;
extern int muste_evalr();
extern int muste_iconv();
extern int muste_get_R_int();
extern double muste_get_R_real();
extern int muste_get_R_string();
extern int muste_get_R_int_vec();
extern double muste_get_R_real_vec();
extern int muste_get_R_string_vec();
extern void muste_set_R_int();
extern void muste_set_R_string();
extern double muste_R_function();
extern void muste_Survo2R();
extern void muste_R2Survo();

static char komento[3*LLENGTH];
static char tclkomento[3*LLENGTH]; 
// static char plotkomento[3*LLENGTH]; 

int muste_window_existing=FALSE;
int muste_window_minimized=FALSE;
char muste_window[64] = "";
char muste_plotwindow[64] = "";
char muste_plotcanvas[64] = "";
int muste_old_plotid=0;

char muste_window_name[]=".muste$ikkuna"; 
int muste_canvasfonts[MAXPLOTWINDOWS];

static SEXP(*RdotTcl)(SEXP) = NULL;
static SEXP(*SurvodotTcl)(SEXP) = NULL;

SEXP Survo_FindFunc(SEXP symbol) {
    SurvodotTcl=(SEXP(*)(SEXP)) R_ExternalPtrAddrFn(symbol);
    return R_NilValue;
}

static int Muste_EvalTcl_core(char *komento, int ikkuna) 
{
    SEXP alist,aptr;

    if (RdotTcl == NULL) { // RdotTcl = R_GetCCallable("tcltk", "dotTcl");
      RdotTcl = (SEXP(*)(SEXP)) R_FindSymbol("dotTcl","tcltk",NULL);
    if (RdotTcl == NULL)   {
      RdotTcl=SurvodotTcl;
    }
    if (RdotTcl == NULL)   {
      Rprintf("ERROR: Can't find function dotTCL!\n");
      return(0);
    }    
//      Rprintf("RdotTcl: %p\n",RdotTcl);
//      Rprintf("Survo_RdotTcl: %p\n",SurvodotTcl);      
      }
      

//    if (strlen(muste_window)<2)
    if (muste_window_existing==FALSE) 
    {
    if (muste_lopetus) return(0); // RS 21.12.2012
    
//    SEXP avar=R_NilValue;
//    avar = findVar(install("muste:::.muste$window"),R_GlobalEnv);
//    strcpy(muste_window,CHAR(STRING_ELT(avar,0)));
    muste_get_R_string(muste_window,".muste$window",63);
    strcat(muste_window," ");
    muste_window_existing=TRUE;
//Rprintf("\nLoytyi ikkuna: %s\n",muste_window);
    }

    if(!ikkuna) strcpy(tclkomento,komento);
    else 
    {
      strcpy(tclkomento,muste_window);
      strcat(tclkomento,komento);
    }
// Rprintf("Komento: %s\n",tclkomento);


    PROTECT(alist = allocList(2));
    aptr=alist;
    aptr=CDR(aptr); 
    SETCAR(aptr, mkString(tclkomento));
    if (!muste_lopetus) RdotTcl(alist); // RS 21.12.2012 if
    UNPROTECT(1);
    return(1);
}

int Muste_EvalTcl(char *komento, int ikkuna) // RS 1.7.2015
    {
    Muste_EvalTcl_core(komento,ikkuna);
//    Muste_EvalTcl_core("update",FALSE);
    return(1);
    }


int sur_locate_tcltk(int row,int col)
{
    sprintf(komento,"mark set insert %d.%d",row,col-1);
    Muste_EvalTcl(komento,TRUE);
    return(1);
}

