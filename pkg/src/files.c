#include <R.h>
#include <Rinternals.h>
#include "survo.h"
#include "survolib.h"


extern SEXP Muste_EvalRExpr();

static char komento[LLENGTH]; /* 256 */

char filesep[] = "/";

int muste_standardize_path(char *path)
	{
	int i;
// RS Changes backward slashes to slashes 
    for (i=0; path[i]!='\0'; i++)
        {
        if (path[i]=='\\') path[i]='/';      
        }
     return(1);   
     }   

int muste_simplify_path(char *path)
	{
	muste_standardize_path(path);
	unsubst_survo_path_in_editor(path);
	return(1);
	}

int muste_expand_path(char *path)
	{
	subst_survo_path_in_editor(path);
    muste_standardize_path(path);
	return(1);
	}

char *muste_getmustepath()
    {
//    extern char *edisk;
    static char *polku;
    SEXP ans;
//    char ch;
    sprintf(komento, "system.file(package=\"muste\")");

    ans=Muste_EvalRExpr(komento);
    if (ans==R_NilValue) 
        {
        sprintf(komento, "\nCannot get Muste directory!");
        sur_print(komento); WAIT;
        return NULL;
        }

    polku=(char *)CHAR(STRING_ELT(ans,0));
    
/* RS Get standardized path by changing to the directory */ 
    sprintf(komento,"setwd(\"%s\")",polku);
    
    
    ans=Muste_EvalRExpr(komento);
    if (ans==R_NilValue)
        {
        sprintf(komento, "\nCannot get Muste directory!");
        sur_print(komento); WAIT;
        return NULL;
        }

    polku=muste_getwd();

/*
	if (edisk!=NULL) 
		{
    	sprintf(komento,"setwd(\"%s\")",edisk); // RS Return back to workpath
    	ans=Muste_EvalRExpr(komento);
    	}
 */
 
 /*   
    strcpy(komento,polku);
    ch=komento[strlen(komento)-1];
    if (ch!='/' && ch!='\\') strcat(komento,filesep);
*/  
    return(polku);
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
        sprintf(komento, "\nCannot get working directory!");
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
//    extern char *survo_path;
	extern char *muste_startpath;

//    int i;
    char path[LNAME];
    char *polku;
    SEXP ans;
        
    if (g<2 || strcmp(parm[1],"-")==0)
        { // plain CD merely changes to default datapath

		sprintf(path,"%s",muste_startpath); // RS CHA
/* RS CHA        
        i=hae_apu("edisk",path);
        if (i==0) sprintf(path,"%s",survo_path);
*/        
        }
    else
        { 
        strcpy(path, parm[1]);
        }

		muste_expand_path(path);
// RS CHA    subst_survo_path_in_editor(path); // 27.2.2001

    sprintf(komento,"setwd(\"%s\")",path);
        
    if (g>=2 && strcmp(parm[1],"*")==0) 
        { 
        sprintf(komento,"setwd(tclvalue(tkchooseDirectory()))");
        }

   muste_expand_path(komento);

    ans=Muste_EvalRExpr(komento);
    if (ans==R_NilValue)
        {
        if (g>=2 && strcmp(parm[1],"*")==0) { disp(); return(-1); }
        sprintf(komento, "\nCannot change to %s!", path);
        sur_print(komento); WAIT;
//        disp(); // RS lisäys varmuuden vuoksi
        return -1;
        }

    polku=muste_getwd();
    if (polku!=NULL) strcpy(edisk, polku);
    return(1);
}  

/* RS Näiden toimintaa ei ole vielä testattu; mitä käy virhetilanteissa??? */
int sur_delete1(char *s)
    {
    muste_expand_path(s); // RS ADD
      sprintf(komento,"unlink(\"%s\")",s);
//    sprintf(komento,"if (file.exists(\"%s\")) { file.remove(\"%s\") } else { FALSE }",s,s);
    return(INTEGER(Muste_EvalRExpr(komento))[0]);

/*    return(DeleteFile(s)); */
    }

int sur_delete(char *s)
    {
// RS REM    int i;
// RS REM    char x[LNAME];
    
//    muste_expand_path(s); // RS ADD

    return(sur_delete1(s));


// RS REM    if (strchr(s,'*')==NULL && strchr(s,'?')==NULL) // 22.10.2000
//        return(sur_delete1(s));
//muste_fixme("\nFIXME: Wild cards not allowed in sur_delete()");
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
    

int sur_delete_files(char *s)
    { 
        return(sur_delete1(s));
        
// RS REM muste_expand_path(s);   
// RS REM muste_fixme("\nFIXME: sur_delete_files() not yet implemented");   
/*    
HANDLE file_to_be_found;
WIN32_FIND_DATA find_data;
    int i,k;
    char x[LNAME];
// printf("\nfiles: %s|",s); getck();
    k=0;
    while (1)
        {
        if (k==0)
            {
            file_to_be_found=FindFirstFile(s,&find_data);
            if (file_to_be_found==INVALID_HANDLE_VALUE) break;
            k=1; i=1;
            }
        else
            i=FindNextFile(file_to_be_found,&find_data);
        if (i==0) break;
        strcpy(x,find_data.cFileName); // ei polkutunnusta edessÑ !!?
// printf("\nx=%s|",x); getck();
        if (strchr(x,'\\')==NULL)
            {
            for (i=strlen(s)-1; i>0; --i) if (s[i]=='\\') break;
            if (i>0)
                {
                *x=EOS; strncat(x,s,i+1);
                strcat(x,find_data.cFileName);
                }
            }
// printf("\ndel=%s|",x); getck();
        DeleteFile(x);
        }
    FindClose(file_to_be_found);    
    return(1);
*/    
    }
    

int sur_copy_file(char *s,char *d)
    {
muste_expand_path(s);
muste_expand_path(d);
    sprintf(komento,"file.copy(\"%s\",\"%s\",overwrite=TRUE)",s,d);         
    muste_evalr(komento);

// RS REM muste_fixme("\nFIXME: sur_copy_file() not yet implemented");
// RS NYI    return(CopyFile(s,d,FALSE));
    return(1);
    }

int sur_make_dir(char *s)
    {
    int i;
muste_expand_path(s);    
    
    sprintf(komento,"dir.create(\"%s\")",s);         
    i=muste_evalr(komento);
    if (i) return(1);

    return(-1);
    }

int sur_remove_dir(char *s)
    {
    int i;
    
    i=sur_delete1(s);
    if (i) return(1);
  
    return(-1);
    }

int sur_file_exists(char *s)
    {
muste_expand_path(s);    
muste_fixme("\nFIXME: sur_file_exists() not yet implemented");
/* RS NYI
    DWORD i;
    i=GetFileAttributes(s);
    if (i==0xFFFFFFFF) return(-1);
*/
    return(1);
    }

int sur_is_directory(char *s)
    {
muste_expand_path(s);
muste_fixme("\nFIXME: sur_is_directory() not yet implemented");
/* RS NYI
    DWORD i;

    i=GetFileAttributes(s);
    if (i==0xFFFFFFFF) return(0);
    if (i & FILE_ATTRIBUTE_DIRECTORY) return(1);
*/
    return(0);
    }

int sur_get_file_time(char *tiedosto,char *date,char *time)
    {
muste_expand_path(tiedosto);
muste_fixme("\nFIXME: sur_get_file_time() not yet implemented");
/* RS NYI
    {
    HANDLE hFile;
    FILETIME ftime,ftime2;
    SYSTEMTIME sysTime;
    hFile=CreateFile(tiedosto,GENERIC_READ,0,NULL,
                 OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
                 1111111111111
    if (hFile==INVALID_HANDLE_VALUE)
        {
        return(-1);
        }
    GetFileTime(hFile,NULL,NULL,&ftimsur_get_filee);
    FileTimeToLocalFileTime(&ftime,&ftime2);
    FileTimeToSystemTime(&ftime2,&sysTime);
    sprintf(date,"%d-%.2d-%.2d",
                 sysTime.wYear,sysTime.wMonth,sysTime.wDay);
    sprintf(time,"%d:%.2d:%.2d",
                 sysTime.wHour,sysTime.wMinute,sysTime.wSecond);
    CloseHandle(hFile);
    return(1);
    }


*/
    sprintf(date,"%d-%.2d-%.2d",
                 2010,11,11);
    sprintf(time,"%d:%.2d:%.2d",
                 10,10,10);
    return(0);
    }


int sur_find_files(char *s,char *t)
    {
 muste_fixme("\nFIXME: sur_find_files() not yet implemented");
/* f_files.c 9.6.2005

#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <stdio.h>
#include <survo.h>
HANDLE file_to_be_found;
WIN32_FIND_DATA find_data;


    int i,k,h;
    char x[LNAME];
    char attr[10];
    DWORD a;
// printf("\nfiles: %s|",s); getck();
    *t=EOS;
    k=0; h=0;
    while (1)
        {
        if (k==0)
            {
            file_to_be_found=FindFirstFile(s,&find_data);
            if (file_to_be_found==INVALID_HANDLE_VALUE) break;
            k=1; i=1;
            }
        else
            i=FindNextFile(file_to_be_found,&find_data);
        if (i==0) break;
        strcpy(x,find_data.cFileName); // ei polkutunnusta edessÑ
// printf("\nx=%s|",x); getck();
        *attr=EOS;
        a=find_data.dwFileAttributes;
        if ( a & FILE_ATTRIBUTE_DIRECTORY) strcat(attr,"D");
        if ( a & FILE_ATTRIBUTE_HIDDEN) strcat(attr,"H");
        if ( a & FILE_ATTRIBUTE_ARCHIVE) strcat(attr,"A");
        if ( a & FILE_ATTRIBUTE_READONLY) strcat(attr,"R");
        if ( a & FILE_ATTRIBUTE_SYSTEM) strcat(attr,"S");
        if ( a & FILE_ATTRIBUTE_TEMPORARY) strcat(attr,"T");
        if (*attr==EOS) strcat(attr,"N");
        h+=sprintf(t+h,"%s %s\n",x,attr);
        }
    FindClose(file_to_be_found);
*/
    return(1);
    }


int sur_find_file(char *s)
    {
muste_expand_path(s);
sprintf(komento,"file.exists(\"%s\")",s);
    return(INTEGER(Muste_EvalRExpr(komento))[0]);

// muste_fixme("\nFIXME: sur_find_file() not yet implemented");
/*
HANDLE FindFile;
WIN32_FIND_DATA FindData;
sur_find_file(pathname)
char *pathname;
    {
    FindFile=FindFirstFile(pathname,&FindData);
    if (FindFile==INVALID_HANDLE_VALUE) return(0);
    FindClose(FindFile);
*/

//    return(1);
    }


int sur_rename(char *s,char *t)
    {
muste_expand_path(s);
muste_expand_path(t);
    
    sprintf(komento,"file.rename(\"%s\",\"%s\")",s,t);
    return(INTEGER(Muste_EvalRExpr(komento))[0]);

/*    return(MoveFile(s,t)); */
    }
	
int muste_fseek(FILE *stream_pointer, long offset, int origin)
	{
	int os;
	os=(int)offset;
//Rprintf("\nseek offset: %d",os);	
	return(fseek(stream_pointer,(int)os,origin));
	}
	
int muste_copytofile(char *sis,char *tied)
        {
        char x[LLENGTH], out[LNAME];
//        unsigned int j;
//        int k;
        FILE *ofile;
        extern char *etmpd;

		strcpy(x,tied);
		strcpy(out,etmpd); strcat(out,x);
		
        ofile=muste_fopen(out,"wt");
        fputs(sis,ofile);
        muste_fclose(ofile);

        return(1);
        }

