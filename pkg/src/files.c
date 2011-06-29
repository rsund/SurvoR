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
        if (g>=2 && strcmp(parm[1],"*")==0) { disp(); return(-1); }
        sprintf(komento, "\nCan not change to %s!", path);
        sur_print(komento); WAIT;
        disp(); // RS lisäys varmuuden vuoksi
        return -1;
        }

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

int sur_delete(char *s)
    {
    int i;
    char x[LNAME];

    if (strchr(s,'*')==NULL && strchr(s,'?')==NULL) // 22.10.2000
        return(sur_delete1(s));

Rprintf("FIXME: Wild cards not allowed in sur_delete()\n");
/* RS NYI
    while (1)
        {
        file_to_be_found=FindFirstFile(s,&find_data);
        if (file_to_be_found==INVALID_HANDLE_VALUE) break;
        FindClose(file_to_be_found);

        strcpy(x,find_data.cFileName); // ei polkutunnusta edessä !!?
        if (strchr(x,'\\')==NULL)
            {
            for (i=strlen(s)-1; i>0; --i) if (s[i]=='\\') break;
            if (i>0)
                {
                *x=EOS; strncat(x,s,i+1);
                strcat(x,find_data.cFileName);
                }
            }
        DeleteFile(x);
        }
*/
    return(1);
    }

int sur_copy_file(char *s,char *d)
    {
Rprintf("\nFIXME: sur_copy_file() not yet implemented\n");
// RS NYI    return(CopyFile(s,d,FALSE));
    return(1);
    }

int sur_make_dir(char *s)
    {
    int i;
    
    sprintf(komento,"dir.create(\"%s\")",s);         
    i=muste_evalr(komento);
    if (i) return(1);

    return(-1);

/* RS OLD
    Rprintf("\nFIXME: sur_make_dir() not yet implemented\n");
    i=CreateDirectory(s,NULL);
*/
    }

int sur_remove_dir(char *s)
    {
    int i;
    
    i=sur_delete1(s);
    if (i) return(1);
    
//Rprintf("\nFIXME: sur_remove_dir() not yet implemented\n");
/* RS NYI
    int i;
    i=RemoveDirectory(s);
    if (i) return(1);
*/
    return(-1);
    }

int sur_file_exists(char *s)
    {
Rprintf("\nFIXME: sur_file_exists() not yet implemented\n");
/* RS NYI
    DWORD i;
    i=GetFileAttributes(s);
    if (i==0xFFFFFFFF) return(-1);
*/
    return(1);
    }

int sur_is_directory(char *s)
    {

Rprintf("\nFIXME: sur_is_directory() not yet implemented\n");
/* RS NYI
    DWORD i;

    i=GetFileAttributes(s);
    if (i==0xFFFFFFFF) return(0);
    if (i & FILE_ATTRIBUTE_DIRECTORY) return(1);
*/
    return(0);
    }




int sur_rename(char *s,char *t)
    {
    sprintf(komento,"file.rename(\"%s\",\"%s\")",s,t);
    return(INTEGER(Muste_EvalRExpr(komento))[0]);

/*    return(MoveFile(s,t)); */
    }
