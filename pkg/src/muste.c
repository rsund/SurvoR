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

SEXP Muste_EvalRExpr(char *cmd)
{
   ParseStatus status;
   SEXP cmdsexp, cmdexpr, ans = R_NilValue;
   int i;
/*
   i=0;
   while (cmd[i]!='\0') { 
       if ((unsigned char)cmd[i]>127) cmd[i]='.'; 
       i++;
   }
*/
   PROTECT(cmdsexp = allocVector(STRSXP, 1));
   SET_STRING_ELT(cmdsexp, 0, mkChar(cmd));
   cmdexpr = PROTECT(R_ParseVector(cmdsexp, -1, &status, R_NilValue));
   if (status != PARSE_OK) {
       UNPROTECT(2);
       error("Invalid call %s",cmd);
   }
   for(i=0; i<length(cmdexpr); i++) ans = eval(VECTOR_ELT(cmdexpr,i),R_GlobalEnv);
   UNPROTECT(2); 
   return ans;
}






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

SEXP Muste_Editor(SEXP session)
{
    muste_editor();
    return(session);
}



int Muste_GetKey(SEXP funktio, SEXP ymparisto)
{
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
    return(INTEGER(eval(funktio,ymparisto))[0]);
}

SEXP Muste_WaitKoe(SEXP funktio, SEXP ymparisto)
{
    int i;
    i=Muste_GetKey(funktio,ymparisto);
    Rprintf("\n%d\n",i);
    return(funktio);
}

/* .Call("Muste_SetCursorKoe",as.integer(10),as.integer(10)) */
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


