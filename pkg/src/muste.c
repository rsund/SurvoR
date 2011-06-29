#include <R.h>
#include <Rinternals.h>
#include <stdio.h>

extern int s_init();
extern int s_end();
extern int op_arit();
extern int muste_corr();
extern int muste_var();

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

SEXP Muste_VarOperation(SEXP session)
{
    muste_var("A");
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

