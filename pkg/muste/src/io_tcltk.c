#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

typedef struct {
    char       *name;
    DL_FUNC     fun;
    int         numArgs;

    R_NativePrimitiveArgType *types;   
} Rf_DotCSymbol;

typedef Rf_DotCSymbol Rf_DotFortranSymbol;

typedef struct {
    char       *name;
    DL_FUNC     fun;
    int         numArgs;
} Rf_DotCallSymbol;

typedef Rf_DotCallSymbol Rf_DotExternalSymbol;

struct Rf_RegisteredNativeSymbol {
    NativeSymbolType type;
    union {
	Rf_DotCSymbol        *c;
	Rf_DotCallSymbol     *call;
	Rf_DotFortranSymbol  *fortran;
	Rf_DotExternalSymbol *external;
    } symbol;
};

#include <string.h>
#include "survo.h"

#define MAXPLOTWINDOWS 300
#define MAXFONTS 300

extern int muste_lopetus;
extern int muste_evalr(char *cmd);
extern int muste_iconv(char *teksti,char *to,char *from);
extern int muste_requirepackage(char *package);
extern int muste_get_R_int(char *sour);
extern double muste_get_R_real(char *sour);
extern int muste_get_R_string(char *dest,char *sour,int length);
extern int muste_get_R_int_vec(char *sour,int element);
extern double muste_get_R_real_vec(char *sour,int element);
extern int muste_get_R_string_vec(char *dest,char *sour,int length,int element);
extern void muste_set_R_int(char *dest,int luku);
extern void muste_set_R_string(char *dest,char *sour);
extern double muste_R_function(char *s,double *x,int n);
extern void muste_Survo2R(char *dest,char *sour);
extern void muste_R2Survo(char *dest,char *sour);
extern void muste_init_plotwindows(void);
extern int muste_beep(void);
extern int muste_debug_print(char *teksti);

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

SEXP Survo_FindRegFunc(SEXP symbol) {
    R_RegisteredNativeSymbol *tmp = NULL;
    tmp = (R_RegisteredNativeSymbol *) R_ExternalPtrAddr(symbol);
    if (tmp==NULL) return R_NilValue;
	SurvodotTcl = (SEXP(*)(SEXP)) tmp->symbol.external->fun;
//	Rprintf("Survo_RdotTcl: %p\n",SurvodotTcl); 	
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

