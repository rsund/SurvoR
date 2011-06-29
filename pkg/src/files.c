#include <R.h>
#include <Rinternals.h>
#include "survo.h"

#ifdef Win32
    char filesep[] = "\\";
#else
    char filesep[] = "/";
#endif

extern SEXP Muste_EvalRExpr();

static char komento[3*LLENGTH]; /* 256 */

char *muste_getwd()
    {
    static char *polku;
    SEXP ans;
    sprintf(komento,"getwd()");
    ans=Muste_EvalRExpr(komento);
    polku=(char *)CHAR(VECTOR_ELT(ans,0));
    strcat(polku,filesep);
    return(polku);
// Rprintf("WD: %s", polku); // CHAR(VECTOR_ELT(ans,0)));
    }

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
