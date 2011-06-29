#include <R.h>
#include <Rinternals.h>
#include <stdio.h>

extern int s_init();
extern int s_end();
extern int op_arit();

SEXP edarit(SEXP session)
{
/*
s_init("A");
op_arit(); 
s_end("A");
*/
Rprintf("\nEntering editorial arithmetics\n");
s_init("A");
op_arit(); 
s_end("A");
return(session);
}


