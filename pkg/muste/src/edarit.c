#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <stdio.h>

extern int s_init(char *siirtop);
extern int s_end(char *siirtop);
extern int op_arit(void);

SEXP edarit(SEXP session)
{
double koe;

koe=dnorm(0.5,0.0,1.0,0);
Rprintf("\nEntering editorial arithmetics, %f\n",koe);
s_init("A");
op_arit(); 
s_end("A");
return(session);
}


