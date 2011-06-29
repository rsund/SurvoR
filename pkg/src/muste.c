#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <stdio.h>
#include "survolib.h"

extern int s_init();
extern int s_end();
extern int op_arit();
extern int muste_corr();
extern int muste_var();
extern int muste_file_show();
extern int muste_editor();
extern int headline();

extern int etu;
extern int muste_eventpeek;

static char komento[10*256];

static int muste_eventlooprunning;


SEXP Muste_Editor(SEXP session)
{
   extern int muste_window_existing;
   extern int muste_eventtime;
   extern int muste_eventtype;
extern long tutalku;
extern int tut_special_code;
extern int tut_not_break;
extern int sukro_esto;
extern int tut_not_break2;
extern int help_on;
extern int rajoitettu_vastausaika;
extern int sucro_menu;
extern int etu;
extern int tut_index;
extern int ntut;

   char argument[256];

    muste_eventpeek=TRUE;
    muste_eventlooprunning=FALSE;
    
    muste_window_existing=FALSE;
    muste_eventtime=0;
    muste_eventtype=0;
tutalku=0L; /* 10.2.90 */
tut_special_code=0;
tut_not_break=0;
sukro_esto=0;
tut_not_break2=0;
help_on=0;
rajoitettu_vastausaika=0;
sucro_menu=0;
etu=0;
tut_index=0;
ntut=0;

    strcpy(argument,CHAR(STRING_ELT(session,0)));
    muste_editor(argument);
    return(session);
}


/*
int muste_checkstack(void)
{
   int dummy;
   intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);
   if(usage > 0.95 * R_CStackLimit) return(-1);
   return(1);
} 
*/

SEXP Muste_EvalRExpr(char *cmd)
{
   ParseStatus status;
   SEXP cmdsexp, cmdexpr, ans = R_NilValue;
   int i;

//   sprintf(komento,"if (inherits(try(.muste.ans<-%s,silent=TRUE), \"try-error\")) FALSE else TRUE",cmd);
   sprintf(komento,"if (inherits(try(.muste.ans<-%s,silent=FALSE), \"try-error\")) FALSE else TRUE",cmd);

// Rprintf("EvalR: %s\n",komento); // RS DEBUG

   PROTECT(cmdsexp = allocVector(STRSXP, 1));
   SET_STRING_ELT(cmdsexp, 0, mkChar(komento));
   cmdexpr = PROTECT(R_ParseVector(cmdsexp, -1, &status, R_NilValue));
   if (status != PARSE_OK) {
       UNPROTECT(2);
// RS REM      error("Invalid call %s",cmd);
Rprintf("\nSyntax error!");
       return (R_NilValue);
   }
   for(i=0; i<length(cmdexpr); i++) ans = eval(VECTOR_ELT(cmdexpr,i),R_GlobalEnv);
   UNPROTECT(2); 
   if (INTEGER(ans)[0]==FALSE) return (R_NilValue);
   ans = findVar(install(".muste.ans"),R_GlobalEnv);
   return ans;
}

int muste_evalr(char *cmd)
   {
   int retstat;
   SEXP status;
   retstat=1;
   status=Muste_EvalRExpr(cmd);
   if (status==R_NilValue) retstat=-1;
   return retstat;
   }
   
 int muste_systemcall(char *cmd)
	{
	sprintf(komento,"system(\"%s\")",cmd);
	muste_copy_to_clipboard(komento);        
    muste_evalclipboard();
  	return(1);
	}  

int muste_stopeventloop()
   {

//    sprintf(komento,".muste.stop()");

   muste_evalr(".muste.stop()");
   R_ProcessEvents();
   muste_sleep(100);   
   return(1);
   }
   
SEXP Muste_Command(SEXP session)
{
extern int muste_lopetus;
  muste_lopetus=TRUE;
return(session);
}   

SEXP Muste_Eventloop(SEXP session)
{
    int jatkuu;

    if (muste_eventlooprunning) return(session);
    muste_eventlooprunning=TRUE;

/*    
    R_FlushConsole();
    R_ProcessEvents();
    Muste_EvalTcl("update idletasks",FALSE);
    Muste_EvalTcl("update",FALSE);
*/

//    muste_eventpeek=FALSE;

    jatkuu=1;

    if (etu==2)
        {
        muste_eventpeek=FALSE;
        while (etu==2) { jatkuu=muste_editor_eventhandler(); }
        muste_eventpeek=TRUE;
        }

//    muste_eventpeek=TRUE;

//    if (muste_peekinputevent(FALSE))
//        {
        jatkuu=muste_editor_eventhandler();
//        }

     if (jatkuu==FALSE) muste_stopeventloop();
//    muste_eventpeek=FALSE;
    muste_eventlooprunning=FALSE;
    return(session);
}



/*

SEXP Muste_EditorialArithmetics(SEXP session)
{
    s_init("A");
    op_arit();
    s_end("A");
    return(session);
}

SEXP Muste_CorrModule(SEXP session)
{
    muste_corr("A");
    return(session);
}

SEXP Muste_HeaderLine(SEXP session)
{
    headline();
    return(session);
}


SEXP Muste_VarOperation(SEXP session)
{
    muste_var("A");
    return(session);
}


SEXP Muste_FileShow(SEXP session)
{
    muste_file_show("A");
    return(session);
}


*/

//int Muste_GetKey(SEXP funktio, SEXP ymparisto)
//{
/* 
  const char funktio[] = "MusteGetKey\0";
   const char ymparisto[] = "muste.environment\0";

   SEXP a = allocVector(STRSXP, 2);
   PROTECT(a);
   SET_STRING_ELT(a, 0, mkChar(funktio));
   SET_STRING_ELT(a, 1, mkChar(ymparisto));
   UNPROTECT(1);
   return a;
*/   
//    return(INTEGER(eval(funktio,ymparisto))[0]);
//}

/*
SEXP Muste_WaitKoe(SEXP funktio, SEXP ymparisto)
{
    int i;
    i=Muste_GetKey(funktio,ymparisto);
    Rprintf("\n%d\n",i);
    return(funktio);
}
*/

/* .Call("Muste_SetCursorKoe",as.integer(10),as.integer(10)) 
SEXP Muste_SetCursorKoe(SEXP row, SEXP col)
{
    int i,j,k;
    i=INTEGER(row)[0];
    j=INTEGER(col)[0];
    k=sur_locate(i,j);
    return(row);
}

SEXP Muste_Write(SEXP row, SEXP col,SEXP sha)
{
    const char testi[] = "Kokeilu";
    int i,j,k;
    i=INTEGER(row)[0];
    j=INTEGER(col)[0];
    k=INTEGER(sha)[0];
    write_string(testi,7,k,i,j);
    return(row);
}

*/
