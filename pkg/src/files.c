#include <R.h>
#include <Rinternals.h>
#include "survo.h"
#include "survolib.h"


extern SEXP Muste_EvalRExpr();

static char komento[256]; /* 256 */

char filesep[] = "/";

char *muste_getmustepath()
    {
    static char *polku;
    SEXP ans;
    char ch;
    sprintf(komento, "system.file(package=\"muste\")");

    ans=Muste_EvalRExpr(komento);
    if (ans==R_NilValue) 
        {
        sprintf(komento, "\nCan not get muste directory!");
        sur_print(komento); WAIT;
        return NULL;
        }

    polku=(char *)CHAR(STRING_ELT(ans,0));
    strcpy(komento,polku);
    ch=komento[strlen(komento)-1];
    if (ch!='/' && ch!='\\') strcat(komento,filesep);
    return(komento);
    }

char *muste_getwd()
    {
    static char *polku;
    char ch;
    SEXP ans;
    sprintf(komento, "getwd()");

    ans=Muste_EvalRExpr(komento);
    if (ans==R_NilValue) 
        {
        sprintf(komento, "\nCan not get working directory!");
        sur_print(komento); WAIT;
        return NULL;
        }

    polku=(char *)CHAR(STRING_ELT(ans,0));
    strcpy(komento,polku);
    ch=komento[strlen(komento)-1];
    if (ch!='/' && ch!='\\') strcat(komento,filesep);
    return(komento);
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
        i=hae_apu("edisk",path);
        if (i==0) sprintf(path,"%s",survo_path);
        }
    else
        { 
        strcpy(path, parm[1]);
        }

    subst_survo_path_in_editor(path); // 27.2.2001

    sprintf(komento,"setwd(\"%s\")",path);
        
    if (g>=2 && strcmp(parm[1],"*")==0) 
        { 
        sprintf(komento,"setwd(tclvalue(tkchooseDirectory()))");
        }

// RS Changes backward slashes to slashes 
    for (i=0; komento[i]!='\0'; i++)
        {
        if (komento[i]=='\\') komento[i]='/';      
        }

//Rprintf("\nkomento: %s\n",komento); // RS DEBUG

    ans=Muste_EvalRExpr(komento);
    if (ans==R_NilValue)
        {
        sprintf(komento, "\nCan not change to %s!", path);
        sur_print(komento); WAIT;
        return -1;
        }



/* RS NYI KORJAA: Virheentarkistus pitää lisätä!
    if (i==0) {
        sprintf(sbuf, "\nCan not change to %s!", path);
        sur_print(sbuf); WAIT;
        return -1;
    }
*/
    polku=muste_getwd();
    if (polku!=NULL) strcpy(edisk, polku);
    return(1);
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
