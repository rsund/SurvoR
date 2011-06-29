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

