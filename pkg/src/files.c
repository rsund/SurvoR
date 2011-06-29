#include <R.h>
#include <Rinternals.h>
#include "survo.h"

extern SEXP Muste_EvalRExpr();

static char komento[3*LLENGTH]; /* 256 */


/* RS Näiden toimintaa ei ole vielä testattu; mitä käy virhetilanteissa??? */
int sur_delete1(char *s)
    {
    sprintf(komento,"file.remove(\"%s\")",s);
    return(INTEGER(Muste_EvalRExpr(komento))[0]);

/*    return(DeleteFile(s)); */
    }

int sur_rename(char *s,char *t)
    {
    sprintf(komento,"file.rename(\"%s\",\"%s\")",s,t);
    return(INTEGER(Muste_EvalRExpr(komento))[0]);

/*    return(MoveFile(s,t)); */
    }
