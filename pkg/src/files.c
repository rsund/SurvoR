#include <R.h>
#include <Rinternals.h>
#include "survo.h"

#ifdef Win32
    char filesep[] = "\\";
#else
    char filesep[] = "/";
#endif

extern SEXP Muste_EvalRExpr();
extern int hae_apu();

static char komento[3*LLENGTH]; /* 256 */

char *muste_getwd()
    {
    static char *polku;
    int pituus;
    SEXP ans;
    sprintf(komento,"try(getwd())");
    ans=Muste_EvalRExpr(komento);
    polku=(char *)CHAR(VECTOR_ELT(ans,0));
    strcpy(komento,polku);
    strcat(komento,filesep);
    return(komento);
// Rprintf("WD: %s", polku); // CHAR(VECTOR_ELT(ans,0)));
    }

int muste_setwd()
{
    extern int g;
    extern char *parm[MAXPARM];
    extern char *edisk;
    extern char *survo_path;

    int i;
    char path[LNAME];
    char *polku;
    SEXP ans;

    if (g<2 || strcmp(parm[1],"-")==0)
        { // plain CD merely changes to default datapath
            {
            i=hae_apu("edisk",path);
            if (i==0) sprintf(path,"%s",survo_path);
            }
        }
       else
        {
          if (strcmp(parm[1],"*")==0) 
            { 
            sprintf(komento,"try(setwd(tclvalue(tkchooseDirectory())))");
            }
          else
            { 
            strcpy(path, parm[1]);
            sprintf(komento,"try(setwd(\"%s\"))",path);
            }
        }

    ans=Muste_EvalRExpr(komento);

//    subst_survo_path_in_editor(path); // 27.2.2001
//    i=SetCurrentDirectory(path);

/* RS KORJAA: Virheentarkistus pitää lisätä!
    if (i==0) {
        sprintf(sbuf, "\nCan not change to %s!", path);
        sur_print(sbuf); WAIT;
        return -1;
    }
*/

/* RS NYI
    GetCurrentDirectory(LNAME-1, path);
    strupr(path); i=strlen(path);
    if (path[i-1]!='\\') { path[i]='\\'; path[i+1]='\0'; }
*/
    polku=muste_getwd();
    strcpy(edisk, polku);
    return 1;
}



/* RS Näiden toimintaa ei ole vielä testattu; mitä käy virhetilanteissa??? */
int sur_delete1(char *s)
    {
    sprintf(komento,"try(file.remove(\"%s\"))",s);
    return(INTEGER(Muste_EvalRExpr(komento))[0]);

/*    return(DeleteFile(s)); */
    }

int sur_rename(char *s,char *t)
    {
    sprintf(komento,"try(file.rename(\"%s\",\"%s\"))",s,t);
    return(INTEGER(Muste_EvalRExpr(komento))[0]);

/*    return(MoveFile(s,t)); */
    }
