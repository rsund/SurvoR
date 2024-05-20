#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>


  /*
     A name-routine pair.
   */
typedef struct {
    char *name;
    DL_FUNC func;
} CFunTabEntry;

  /*
     These three structures are the processed, internal information about
     native routines that can be called by R. They are intended to be 
     instantiated by packages that explicitly register the routines in the
     library.
   */

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

typedef void *HINSTANCE;

  /*
      This structure holds the information about a library that is 
      loaded into R and whose symbols are directly accessible to
      .C, .Call, .Fortran, .External, ...
      This stores the short name of the library (with the path and extension 
      removed), and its fully  qualified name including the path and extension.
      Additionally, it can potentially be populated with information about
      the native routines in that library that are callable by R.
   */
struct _DllInfo {
    char  *path;
    char  *name;
    HINSTANCE handle;
    Rboolean useDynamicLookup; /* Flag indicating whether we use both
				  registered and dynamic lookup (TRUE)
				  or just registered values if there
				  are any. */
    int numCSymbols;
    Rf_DotCSymbol *CSymbols;

    int numCallSymbols;
    Rf_DotCallSymbol *CallSymbols;

    int numFortranSymbols;
    Rf_DotFortranSymbol *FortranSymbols;

    int numExternalSymbols;
    Rf_DotExternalSymbol *ExternalSymbols;

    Rboolean forceSymbols;
};


struct Rf_RegisteredNativeSymbol {
    NativeSymbolType type;
    union {
	Rf_DotCSymbol        *c;
	Rf_DotCallSymbol     *call;
	Rf_DotFortranSymbol  *fortran;
	Rf_DotExternalSymbol *external;
    } symbol;
    DllInfo *dll;
};

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

