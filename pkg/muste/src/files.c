#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include "survo.h"
#include "survolib.h"


extern SEXP Muste_EvalRExpr();

static char komento[LLENGTH]; /* 256 */

char filesep[] = "/";

int muste_is_path(char *path)
	{
	char *apu;
	
	if (*path=='"') apu=path+1; else apu=path;
	
    if (strchr(apu,':')!=NULL) return(1);     
    if (*apu=='/' || *apu=='\\' || *apu=='~' || *apu=='<') return(1);
    if (*apu=='.' && (*(apu+1)=='.' || *(apu+1)=='/' || *(apu+1)=='\\')) return(1);
	
	return(0);
	}

int muste_removequotes(char *path)
	{
	int i;
	if (*path=='"' && *(path+strlen(path)-1)=='"')
		{
		for (i=1; i<strlen(path); i++) path[i-1]=path[i];
		*(path+strlen(path)-2)=EOS;
		}
	return(1);	
	}

int muste_insertquotes(char *path)
	{
	int i;
	
	if (strchr(path,' ')==NULL) return(0);
    for (i=strlen(path); i>0; i--) path[i]=path[i-1];
    *(path+strlen(path)+1)=EOS;
    *(path+strlen(path))='"';
    *path='"';
	return(1);	
	}


int muste_standardize_path(char *path)
	{
	int i;
	
// RS Changes backward slashes to slashes 
//        and substitute \032 to space
    for (i=0; path[i]!='\0'; i++)
        {
        if (path[i]=='\\') path[i]='/';
        if (path[i]=='\032') path[i]=' ';       
        }
     muste_removequotes(path);  
     return(1);   
     }   

int muste_standardize_path2(char *path) // RS 15.11.2012
	{
	int i;
	
    for (i=0; path[i]!='\0'; i++)
        {
//        if (path[i]=='\\') path[i]='/';
        if (path[i]=='\032') path[i]=' ';       
        }
//     muste_removequotes(path);  
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

int muste_expand_path2(char *path)
	{
	extern int subst_survo_path_in_editor2();
	subst_survo_path_in_editor2(path);
    muste_standardize_path2(path);
	return(1);
	}

#define EOS '\0'

int muste_removedoublequotes(char *cmd)
	{
	int i,j,k;

//Rprintf("\nrdqin: %s",cmd);	
	i=1; k=0;
   	while (cmd[i]!=EOS)
   		{
   		if (cmd[i]=='"' && cmd[i-1]=='"')
   			{
   			j=0; k=1; 			
   			while (cmd[i+j]!=EOS)
   				{
   				cmd[i-1+j]=cmd[i+j];
   				j++;
   				}
   			cmd[i+j-1]=EOS;	
   			}
   		i++;	
   		}   	
//Rprintf("\nrdqout: %s",cmd);	   		
   	return(k);
 	}

void muste_append_path(char *nimi,char *liite)
	{
	if (*(nimi+strlen(nimi)-1)=='"')
        {
        if (strchr(nimi+strlen(nimi)-5,'.')==NULL)
        	{
        	*(nimi+strlen(nimi)-1)=EOS;
        	strcat(nimi,liite);        	
        	strcat(nimi,"\"");
			}
        }
    else if (strchr(nimi+strlen(nimi)-4,'.')==NULL) strcat(nimi,liite);

    }

char *muste_getapufilepath()
	{
	char path[LNAME];
	static char *polku;
	
	muste_get_R_string(path,".muste$apufile",LNAME);
	polku=path;
	return(polku);
	}

char *muste_getmustepath()
    {
//    extern char *edisk;
	char path[LNAME];
    static char *polku;
//    SEXP ans;
    int i;
//    char ch;
/*
    sprintf(komento, ".muste$mustepath <- system.file(package=\"muste\")");

	i=muste_evalr(komento); 

//    ans=Muste_EvalRExpr(komento);
//    if (ans==R_NilValue) 
	 if (i<0)
        {
        sprintf(komento, "\nCannot get Muste directory!");
        sur_print(komento); WAIT;
        return NULL;
        }
*/
//    polku=(char *)CHAR(STRING_ELT(ans,0));
    
    muste_get_R_string(path,".muste$mustepath",LNAME);
    
/* RS Get standardized path by changing to the directory */ 
    sprintf(komento,"setwd(\"%s\")",path);
    
	i=muste_evalr(komento); 
	
//    ans=Muste_EvalRExpr(komento);
//    if (ans==R_NilValue)
	 if (i<0)
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
//    static char *polku;
    char ch;
//    SEXP ans;
    
    sprintf(komento, ".muste$workdir <- getwd()");
	muste_evalr(komento);
/*
    ans=Muste_EvalRExpr(komento);
    if (ans==R_NilValue) 
        {
        sprintf(komento, "\nCannot get working directory!");
        sur_print(komento); WAIT;
        return NULL;
        }

    polku=(char *)CHAR(STRING_ELT(ans,0));
    strcpy(komento,polku);
*/    
    muste_get_R_string(komento,".muste$workdir",LLENGTH);
   
    ch=komento[strlen(komento)-1];
    if (ch!='/' && ch!='\\') strcat(komento,filesep);
    return(komento);
    }

void muste_updatewd()
	{
    extern char *edisk;	
	char *polku;
    polku=muste_getwd();
    if (polku!=NULL) strcpy(edisk, polku);	
	}

int muste_setwd()
    {
    extern int g;
    extern char *parm[MAXPARM];
//    extern char *edisk;
//    extern char *survo_path;
	extern char *muste_startpath;

    int i;
    char path[LNAME];
//    char *polku;
//    SEXP ans;
        
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
        
   muste_expand_path(komento);        
        
    if (g>=2 && strcmp(parm[1],"*")==0) 
        { 
//        sprintf(komento,"setwd(tclvalue(tkchooseDirectory()))");
        sprintf(komento,".muste.choosedir()");
        }

	i=muste_evalr(komento);
	
//    ans=Muste_EvalRExpr(komento);
//    if (ans==R_NilValue)
 	if (i<0)
        {
        if (g>=2 && strcmp(parm[1],"*")==0) { disp(); return(-1); }
        sprintf(komento, "\nCannot change to %s!", path);
        sur_print(komento); WAIT;
//        disp(); // RS lisäys varmuuden vuoksi
        return -1;
        }

	muste_updatewd();
    return(1);
}  

/* RS Näiden toimintaa ei ole vielä testattu; mitä käy virhetilanteissa??? */
int sur_delete1(char *s)
    {
    int i;
    muste_expand_path(s); // RS ADD
      sprintf(komento,".muste.del(\"%s\")",s);
      i=muste_evalr(komento)-1;
	return(i);

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
// Rprintf("\nfiles: %s|",s); getck();
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
// Rprintf("\nx=%s|",x); getck();
        if (strchr(x,'\\')==NULL)
            {
            for (i=strlen(s)-1; i>0; --i) if (s[i]=='\\') break;
            if (i>0)
                {
                *x=EOS; strncat(x,s,i+1);
                strcat(x,find_data.cFileName);
                }
            }
// Rprintf("\ndel=%s|",x); getck();
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
    
    sprintf(komento,"if (!file.exists(\"%s\")) dir.create(\"%s\")",s,s);         
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

int muste_is_directory(char *s)
{
    int i;

    muste_expand_path(s);

    sprintf(komento, ".muste$isdir <- as.integer(file_test(\"-d\", \"%s\"))", s);
    muste_evalr(komento);
    i=muste_get_R_int(".muste$isdir");

    return(i);
}

int muste_is_write_access(char *s)
{
    int i;

    muste_expand_path(s);

    sprintf(komento, ".muste$iswa <- as.integer((file.access(\"%s\",mode=2)==0))", s);
    muste_evalr(komento);
    i=muste_get_R_int(".muste$iswa");

    return(i);
}

int sur_is_directory(char *s)
    {
	
//muste_expand_path(s);
//muste_fixme("\nFIXME: sur_is_directory() not yet implemented");
/* RS NYI
    DWORD i;

    i=GetFileAttributes(s);
    if (i==0xFFFFFFFF) return(0);
    if (i & FILE_ATTRIBUTE_DIRECTORY) return(1);
*/
    return(muste_is_directory(s));
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
// Rprintf("\nfiles: %s|",s); getck();
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
// Rprintf("\nx=%s|",x); getck();
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
//    int i;
muste_expand_path(s);
sprintf(komento,".muste$filestatus <- as.integer(file.exists(\"%s\"))",s);
muste_evalr(komento);
muste_sleep(10); // RS 12.1.2013
return(muste_get_R_int(".muste$filestatus"));
//    return(INTEGER(Muste_EvalRExpr(komento))[0]);

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

int sur_file_exists(char *s)
    {
    return(sur_find_file(s));
// muste_fixme("\nFIXME: sur_file_exists() not yet implemented");
/* RS NYI
    DWORD i;
    i=GetFileAttributes(s);
    if (i==0xFFFFFFFF) return(-1);
*/
    }

int sur_find_svo_file(char *nimi,char *pathname)
        {
        extern char *edisk;
        strcpy(pathname,nimi);
/* RS FIXME: Levytunnus ei ehkä porttautuvaa koodia */
        if (!muste_is_path(nimi)) // RS CHA if ((strchr(nimi,':')==NULL) && (nimi[0]!='/') && (nimi[0]!='~') && (nimi[0]!='\\') && (nimi[0]!='<') && (nimi[0]!='.')) // RS ADD unix path FIXME
            { strcpy(pathname,edisk); strcat(pathname,nimi); }
        muste_append_path(pathname,".SVO"); // RS CHA if (strchr(pathname+strlen(pathname)-4,'.')==NULL) strcat(pathname,".SVO");
            
        if (pathname[strlen(pathname)-1]=='.') pathname[strlen(pathname)-1]=EOS; // RS ADD
              
		return(sur_find_file(pathname));
		}




int sur_rename(char *s,char *t) // from, to
    {
muste_expand_path(s);
muste_expand_path(t);
    
    sprintf(komento,"file.rename(\"%s\",\"%s\")",s,t);
//    return(INTEGER(Muste_EvalRExpr(komento))[0]);
	return(muste_evalr(komento)+1);

/*    return(MoveFile(s,t)); */
    }

extern int muste_iconv();	
int muste_copytofile(char *sis,char *tied)
        {
        char x[LLENGTH*2], out[LNAME];
//        unsigned int j;
//        int k;
        FILE *ofile;
        extern char *etmpd;
//		strcpy(x,tied);
		
		strcpy(out,etmpd); strcat(out,tied);
		strncpy(x,sis,LLENGTH);
		
		muste_iconv(x,"","CP850");				
        ofile=muste_fopen(out,"wt");
        if (ofile==NULL)
        	{
        	sprintf(x,"\nError! Could not open file %s",out);
        	sur_print(x); WAIT;
        	return(-1);
        	}
        fputs(x,ofile);
        fputc('\r',ofile);
        fputc('\n',ofile);
        muste_fclose(ofile);
        return(1);
        }

int muste_fseek(FILE *stream_pointer, muste_int64 offset, int origin)
	{
	int os;
	os=(int)offset;
//Rprintf("\nseek offset: %d",os);	
	return(fseek(stream_pointer,(int)os,origin));
//	return(fseeko(stream_pointer,offset,origin));
	}

muste_int64 muste_ftell(FILE *stream_pointer)
	{

	int os;
	long pal;
	os=(int)ftell(stream_pointer);
	pal=(long)os;
	return(pal);

//	return((muste_int64)ftello(stream_pointer));	
	}
