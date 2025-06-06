#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>
#include <R_ext/Parse.h>
#include <stdio.h>
#include "survolib.h"
//#include "survo.h"

#define MAXPLOTWINDOWS 300
#define MAX_HDL MAXPLOTWINDOWS
#define LLENGTH 10002
#define EOS '\0'
#define UTF8_MASK (1<<3)
#define IS_UTF8(x) (LEVELS(x) & UTF8_MASK)

//extern int s_init();
//extern int s_end();
//extern int op_arit();
extern int muste_corr(char *argv);
extern int muste_var(char *argv);
//extern int muste_file_show();
extern int muste_editor(char *argv);

extern void survo_open_ajaxbuffer(int);
extern void survo_close_ajaxbuffer(void);

int muste_save_stack_count(int debug);
int muste_restore_stack_count(void);



extern int etu;
extern int muste_eventpeek;
extern int muste_iconv(char *teksti,char *to,char *from);



static char komento[2*LLENGTH];
static char cmd[2*LLENGTH];
static char str1[2*LLENGTH];

int muste_eventlooprunning;
int muste_emergency_stop;
int muste_debug;

SEXP muste_environment;

SEXP Muste_Editor(SEXP session)
{
   extern int muste_window_existing;
   extern int muste_eventtime;
   extern int muste_eventtype;
extern long tutalku;
extern int tut_special_code;
extern int tut_not_break;
extern int sukro_esto;
extern int tut_not_break2;
extern int help_on;
extern int rajoitettu_vastausaika;
extern int sucro_menu;
extern int etu;
extern int tut_index;
extern int ntut;
extern int fixed_plot;
extern int first_plot_number;
extern int gplot_count;
extern int max_hdl;
extern int muste_expand;

extern int muste_GetTickCount_start(int start);

//   char argument[256];
   int i;
   SEXP ans=R_NilValue;
   int *x;

    muste_eventpeek=TRUE;
    muste_eventlooprunning=FALSE;
    
    muste_window_existing=FALSE;
    muste_eventtime=0;
    muste_eventtype=0;
tutalku=0L; /* 10.2.90 */
tut_special_code=0;
tut_not_break=0;
sukro_esto=0;
tut_not_break2=0;
help_on=0;
rajoitettu_vastausaika=0;
sucro_menu=0;
etu=0;
tut_index=0;
ntut=0;
muste_expand=0;
muste_emergency_stop=0;
muste_debug=0;

fixed_plot=0;
first_plot_number=1;
gplot_count=0;
max_hdl=MAX_HDL;

muste_init_plotwindows();
muste_GetTickCount_start(1);

//    strcpy(argument,CHAR(STRING_ELT(session,0)));
	muste_environment=session;

    muste_eventpeek=FALSE;
    muste_eventlooprunning=TRUE;  
    i=muste_editor("Muste"); // RS CHA argument);

    survo_open_ajaxbuffer(1);
    
PROTECT(ans = NEW_INTEGER(1));
x=INTEGER_POINTER(ans);
x[0]=i;
UNPROTECT(1);

    
    muste_eventlooprunning=FALSE;
    muste_eventpeek=TRUE;
    return(ans);
}


/*
int muste_checkstack(void)
{
   int dummy;
   intptr_t usage = R_CStackDir * (R_CStackStart - (uintptr_t)&dummy);
   if(usage > 0.95 * R_CStackLimit) return(-1);
   return(1);
} 
*/

SEXP Muste_EvalRExpr(char *cmd)
{
   ParseStatus status;
   SEXP cmdsexp, cmdexpr, ans = R_NilValue;
   int i;
   char *apu,*apu2,*apu3;
   muste_removedoublequotes(cmd);
//   sprintf(komento,"if (inherits(try(.muste$ans<-%s,silent=TRUE), \"try-error\")) FALSE else TRUE",cmd);   
   apu=apu2=apu3=NULL;
   apu=strchr(cmd,'('); apu2=strchr(cmd,' '); apu3=strchr(cmd,'<');   
   if ((apu2!=NULL && apu3!=NULL && (apu3-cmd)<(apu2-cmd)) || (apu2==NULL)) apu2=apu3;
   if (strncmp(cmd,".muste.",7)==0 && 
      (apu!=NULL && 
      (apu2==NULL || 
      (apu2!=NULL && (apu-cmd)<(apu2-cmd))))
      )
		{
		sprintf(komento,"if (inherits(try(.muste$ans<-muste:::%s,silent=FALSE), \"try-error\")) FALSE else TRUE",cmd);
		}
   else
   		{
   		sprintf(komento,"if (inherits(try(.muste$ans<-%s,silent=FALSE), \"try-error\")) FALSE else TRUE",cmd);
   		}

//Rprintf("EvalR: %s\n",komento); // RS DEBUG
   PROTECT(cmdsexp = allocVector(STRSXP, 1));
   SET_STRING_ELT(cmdsexp, 0, mkChar(komento));
   cmdexpr = PROTECT(R_ParseVector(cmdsexp, -1, &status, R_NilValue));
   if (status != PARSE_OK) {
       UNPROTECT(2);
// RS REM      error("Invalid call %s",cmd);
Rprintf("\nSyntax error!\n%s",cmd);
       return (R_NilValue);
   } 
   for(i=0; i<length(cmdexpr); i++) ans = eval(VECTOR_ELT(cmdexpr,i),R_GlobalEnv);
   UNPROTECT(2); 
   if (INTEGER(ans)[0]==FALSE) return (R_NilValue);
   ans = findVar(install(".muste$ans"),R_GlobalEnv);    
   return ans;
}

int muste_evalr(char *cmd)
   {
   char apu[256];
   int retstat;
   SEXP status;   
   retstat=1;
   
   sprintf(apu,".muste <- muste:::.muste");
   Muste_EvalRExpr(apu);
   
   status=Muste_EvalRExpr(cmd);
   if (status==R_NilValue) retstat=-1; 
 
//   sprintf(apu,"remove(.muste)");
//   i=Muste_EvalRExpr(apu); 
   R_CheckUserInterrupt();  // RS ADD 6.10.2012
   return retstat;
   }

 int muste_evalclipboard()
	{
//	char *mp;
	
	muste_save_stack_count(10);
	muste_sleep(100);
    muste_get_clipboard(); // mp=
    muste_sleep(100);    
    sprintf(cmd,"source(\"clipboard\",echo=TRUE,print.eval=TRUE)");         
    muste_evalr(cmd);
	muste_restore_stack_count();
    return(1);
    }
    
    
int muste_evalsource_output(char *sfile,char *rout)
	{
//	char x[LLENGTH], 
	char out[LLENGTH];
// RS REM	FILE *ifile;
	extern char *etmpd;
// RS REM	extern char *muste_rout;
     
//	strcpy(x,sfile);
    if (!muste_is_path(sfile)) { strcpy(out,etmpd); strcat(out,sfile); } // RS CHA 14.11.2012
    else strcpy(out,sfile);

	if (*rout!=EOS) sprintf(cmd,".muste.runsource(\"%s\",dest=\"%s\")",out,rout);
	else sprintf(cmd,".muste.runsource(\"%s\")",out);
//    "source(\"%s\",echo=TRUE,print.eval=TRUE)",out);         
    muste_evalr(cmd);
    return(1);
    }

int muste_evalsource(char *sfile)
	{
	muste_evalsource_output(sfile,"");
/*	
	char x[LLENGTH], out[LNAME];
// RS REM	FILE *ifile;
	extern char *etmpd;
	
	muste_sleep(100);
	     
	strcpy(x,sfile);
	strcpy(out,etmpd); strcat(out,x);
	
    sprintf(cmd,"source(\"%s\",echo=TRUE,print.eval=TRUE)",out);         
    muste_evalr(cmd);
*/    
    return(1);
    }

#define WAIT sur_print("\nPress any key!"); getcm()
   
 int muste_system(char *incmd,int odotus)
	{
	extern char *muste_command;
	int i,j;

    int len;
    char *clip;
    char tyhja[]="";
    char y[LLENGTH*2];
	char x[LLENGTH*2];  // RS 15.11.2012
	char *osat[5];
/*
    len=strlen(cmd)+1;
//    y=Calloc(3*len,char); // RS ei muste_malloc, koska putsataan heti pois
    y=malloc(3*len);
    if (y==NULL)
    	{
    	sur_print("\nMemory allocation error!"); WAIT;
    	return(-1);
    	}
*/

	strcpy(x,incmd);	
	muste_iconv(x,"","CP850");
	
	for (i=0,j=0; i<strlen(x); i++) 
		{
//		if (y[i]=='"') y[i]='\''; 
		y[j]=x[i];
		if (x[i]=='\'') y[j]='"'; 		
//		if (y[i]=='\\') y[i]='/';
		if (x[i]=='\\') { j++; y[j]='\\'; } // RS CHA 15.11.2012
		j++;
		}
	y[j]=EOS;
		
	if (strncmp(y,"DIR",3)==0 || strncmp(y,"dir",3)==0 ||
		strncmp(y,"LS",2)==0 || strncmp(y,"ls",2)==0)
		{
		if (strchr(y,' ')==NULL)  { clip=tyhja; }
		else { clip=strchr(y,' ')+1; }
		if (odotus) snprintf(cmd,LLENGTH,"muste:::.muste.dir('%s',TRUE)",clip);
		else snprintf(cmd,LLENGTH,"muste:::.muste.dir('%s',FALSE)",clip);			
//		if (wait) sprintf(cmd,"muste:::.muste.dir(\"%s\",TRUE)",clip);
//		else sprintf(cmd,"muste:::.muste.dir(\"%s\",FALSE)",clip);		
		}
	else
		{
		if (odotus==1) snprintf(cmd,LLENGTH,"muste:::.muste.system('%s',TRUE)",y);
		else if (odotus==2)
			{
			strcpy(x,y);
			len=splitq(x,osat,5);		
			snprintf(cmd,LLENGTH,"muste:::.muste.systemopen('%s',FALSE,%d)",y,len); // RS 25.11.2012
			}
		else snprintf(cmd,LLENGTH,"muste:::.muste.system('%s',FALSE)",y);		
//		if (wait) sprintf(cmd,"muste:::.muste.system(\"%s\",TRUE)",y);
//		else sprintf(cmd,"muste:::.muste.system(\"%s\",FALSE)",y);	
    	}

/*
char muste_command_local[]="AMUSTE.CMD";
char sbuf[10000];
	clip=muste_command_local;
sur_print("\nPetri debug OS file start"); WAIT;
snprintf(sbuf,1000,"\nPetri debug OS file - cmd:|%s|",cmd); sur_print(sbuf); WAIT;  	
snprintf(sbuf,1000,"\nPetri debug OS file - localfile:|%s|",clip); sur_print(sbuf); WAIT; 
snprintf(sbuf,1000,"\nPetri debug OS file - globalfile:|%s|",muste_command); sur_print(sbuf); WAIT; 	  	
 */
    muste_copytofile(cmd,muste_command); // "MUSTE.CMD");
// snprintf(sbuf,1000,"\nPetri debug OS eval - file:|%s|",clip); sur_print(sbuf); WAIT;    
    muste_evalsource(muste_command); // "MUSTE.CMD");
// snprintf(sbuf,1000,"\nPetri debug OS done"); sur_print(sbuf); WAIT;
   
// Free(y);   
//    free(y); 

   
  	return(1);
	}  

int muste_requirepackage(char *package)
  {
//  SEXP avar=R_NilValue;
  int i,vast;

  sprintf(cmd,".muste$req<-FALSE");
  muste_evalr(cmd);
  
  snprintf(cmd,LLENGTH,".muste$req<-as.integer(length(find.package(\"%s\")))",package);  
  muste_evalr(cmd);

//  avar = findVar(install(".muste$req"),R_GlobalEnv);
//  vast=INTEGER(avar)[0];

  vast=muste_get_R_int(".muste$req");

  if (vast==FALSE)
    {
    sprintf(cmd,"\nRequired R-package %s not found!",package);
    sur_print(cmd);
    sur_print("\nInstall now (Y/N)?"); // RS 29.8.2013
    i=sur_getch();
    if (i!='Y' && i!='y') return(vast);    
    snprintf(cmd,LLENGTH,"install.packages(\"%s\",dep=TRUE)",package);  
    muste_evalr(cmd);
    snprintf(cmd,LLENGTH,"if (!require(%s)) { install.packages(\"%s\",contriburl=\"http://www.survo.fi/muste\") }",package,package);
    }
  
  snprintf(cmd,LLENGTH,".muste$req<-as.integer(require(%s))",package);  
  muste_evalr(cmd);
  
//  avar = findVar(install(".muste$req"),R_GlobalEnv);
//  vast=INTEGER(avar)[0];

  vast=muste_get_R_int(".muste$req");

  if (vast==FALSE)
    {
    sprintf(cmd,"\nRequired R-package %s could not be loaded!",package);
    sur_print(cmd);
    }
  
  return(vast);  
  }

void muste_set_R_string(char *dest,char *sour) // RS 25.11.2012
	{
	SEXP tmp;
	char *hakuapu,*teksti;
	char tyhja[]="";

	if (dest==NULL) return;
	if (sour==NULL) teksti=tyhja;
	else teksti=sour;

 	hakuapu=strchr(dest,'$')+1;
  	if (hakuapu==NULL) hakuapu=dest;
	  
	PROTECT(tmp = allocVector(STRSXP, 1));
	SET_STRING_ELT(tmp, 0, mkChar(teksti));
	defineVar(install(hakuapu),tmp,muste_environment);
	UNPROTECT(1); // tmp
  
//  snprintf(cmd,LLENGTH,"%s<-\"%s\"",dest,sour);
//  muste_evalr(cmd);
  }

int muste_get_R_char_noencode(char *dest,char *sour,int length)
  {
  SEXP avar=R_NilValue;
  char *hakuapu;

  hakuapu=strchr(sour,'$')+1;
  if (hakuapu==NULL) hakuapu=sour;
  avar = findVar(install(hakuapu),muste_environment); // RS CHA R_GlobalEnv);
  snprintf(dest,length,"%s",CHAR(STRING_ELT(avar,0)));
  
  return(1);
  }

int muste_get_R_string_vec(char *dest,char *sour,int length,int element)
  {
  SEXP enc;
  SEXP avar=R_NilValue;
  char *hakuapu,*hakubuf;
  int len;

  hakuapu=strchr(sour,'$')+1;
  if (hakuapu==NULL) hakuapu=sour;
  avar = findVar(install(hakuapu),muste_environment); // RS CHA R_GlobalEnv);
  if (!isString(avar)) // RS 29.8.2013
    {
    *dest=EOS;
    return(0);
    }
  enc=STRING_ELT(avar,element); // RS 20.12.2012 Convert automatically to CP850  
  hakuapu=(char *)CHAR(enc);
  len=strlen(hakuapu);
  hakubuf=(char *)malloc(len+2);
  if (hakubuf==NULL) return(0);
  strcpy(hakubuf,hakuapu); 
  if (IS_UTF8(enc)) { muste_iconv(hakubuf,"CP850","UTF-8"); }
  else muste_iconv(hakubuf,"CP850","");
  snprintf(dest,length,"%s",hakubuf);
  free(hakubuf);
  return(1);
  }

int muste_get_R_string(char *dest,char *sour,int length)
  {
  return(muste_get_R_string_vec(dest,sour,length,0));
  }

void muste_set_R_int(char *dest,int luku)
  {
  snprintf(cmd,LLENGTH,"%s<-as.integer(%d)",dest,luku);
  muste_evalr(cmd);
  }

int muste_get_R_int_vec(char *sour,int element)
  {
  SEXP avar=R_NilValue;
  int vast;
  char *hakuapu;

  hakuapu=strchr(sour,'$')+1;
  if (hakuapu==NULL) hakuapu=sour;
  avar = findVar(install(hakuapu),muste_environment);
  if (!isInteger(avar) && !isLogical(avar))  // RS 29.8.2013
    {
    sprintf(cmd,"\nFIXME: %s not of type INTEGER or LOGICAL",sour);
    muste_fixme(cmd);
    }
  vast=INTEGER(avar)[element];
  return(vast);  
  }

int muste_get_R_int(char *sour)
  {
  return(muste_get_R_int_vec(sour,0));
  }

double muste_get_R_real_vec(char *sour,int element)
  {
  SEXP avar=R_NilValue;
  double vast;
  char *hakuapu;

  hakuapu=strchr(sour,'$')+1;
  if (hakuapu==NULL) hakuapu=sour;
  avar = findVar(install(hakuapu),muste_environment);
  if (!isReal(avar))  // RS 29.8.2013
    {
    sprintf(cmd,"\nFIXME: %s not of type REAL",sour);
    muste_fixme(cmd);
    }  
  vast=REAL(avar)[element];
  return(vast);  
  }

double muste_get_R_real(char *sour)
  {
  return(muste_get_R_real_vec(sour,0));
  }

void muste_copy_to_clipboard(char *x)
    {
    int len;
    char *y; // ,*leike;
//    int i,j;
    
    len=strlen(x);    
    
    y=malloc(3*(len+3)); // RS ei muste_malloc, koska putsataan heti pois
	if (y==NULL) return; // RS ADD 3.10.2012
//    leike=malloc(3*(len+1));
//    if (leike==NULL) return; // RS ADD 3.10.2012

//	y[0]=EOS;
/* RS Handle Tcl-special characters: 34="  36=$  91=[  92=\       */
/*
    for (i=0, j=0; i<len; i++) {
//       		if (x[i]==34 || x[i]==92 ) y[j++]=92; // RS REM 19.11.2012 || x[i]==36 || x[i]==91 
       		if ((unsigned char)x[i]==213) 
       			{
       			y[j]=EOS;
       			strcat(y,"\\u20AC"); j=strlen(y);
       			}
      		else y[j++]=x[i];
    }
    y[j]=EOS;
*/
	strcpy(y,x);
    muste_iconv(y,"","CP850");

	muste_set_R_string(".muste$cliptext",y); // RS 26.11.2012
//	snprintf(leike,3*len,".muste.putclipboard(\"%s\")",y); // RS CHA 19.11.2012
	sprintf(cmd,".muste.putclipboard(.muste$cliptext)");
	muste_evalr(cmd);

/*
    sprintf(str1,"clipboard clear");
    Muste_EvalTcl(str1,FALSE);

    sprintf(leike,"clipboard append \"%s\"",y);
    Muste_EvalTcl(leike,FALSE);
*/
//	free(leike);
	free(y);

/* RS CHA
    p=clip;
    hGlob=GlobalAlloc(GHND,len);
    pGlob=GlobalLock(hGlob);
    for (i=0; i<len; ++i)
        *pGlob++=*p++;
    GlobalUnlock(hGlob);
    OpenClipboard(NULL);
    EmptyClipboard();
    SetClipboardData(CF_TEXT,hGlob);
    CloseClipboard();
*/  
    return;
    }
    

char *muste_get_clipboard()
    {

/* RS NYI
    if (!IsClipboardFormatAvailable(CF_TEXT))
        {
        if (etu==0)
            {
            sur_print("\nNo data in the clipboard!");
            WAIT;
            }
        return(1);
        }
*/


//    SEXP avar;
    char *clip; 
    int len;

    sprintf(cmd,".muste.getclipboard()");
    muste_evalr(cmd);

//    avar=R_NilValue;
//    avar=findVar(install(".muste$clipboard"),muste_environment);
//    clip=(char *)CHAR(STRING_ELT(avar,0));
    
    len=muste_get_R_int(".muste$clipboardlen")+1; // RS 20.12.2012
//Rprintf("\ncliplen: %d",len);    
    clip=muste_malloc(len+2);
    if (clip==NULL) return(NULL);     
    muste_get_R_string(clip,".muste$clipboard",len+1);    

//    muste_iconv(clip,"CP850","");   
    strcat(clip,"\n");
//sprintf(cmd,"\nclip:\n%s",clip); 
//sur_print(cmd); WAIT; 
       
    return(clip);

/* RS REM
    OpenClipboard(NULL);
    hClip=GetClipboardData(CF_TEXT);
    len=GlobalSize(hClip);
    clip=muste_malloc(len);
    pClip=GlobalLock(hClip);
    strcpy(clip,pClip);
    GlobalUnlock(hClip);
    CloseClipboard();
*/
    }

int muste_geturlfile(char *path, char *retfilename) // RS 29.8.2013	
    {
    char clip[LLENGTH];
    int i,len;
    char open_copy[LLENGTH];
    
    strcpy(clip,path);
	muste_iconv(clip,"LATIN1","CP850");    
// RS 27.9.2014 from get_www (editor.c)   
    *open_copy=EOS;
    len=strlen(clip);
    for (i=0; i<len; ++i) 
        {
        if ((unsigned char)clip[i]<32 || (unsigned char)clip[i]>127) // || clip[i]==91 || clip[i]==92 || clip[i]==93 || clip[i]==94 || clip[i]==95 || clip[i]==96)
            {
            sprintf(cmd,"%s%%%2x",open_copy,(unsigned char)clip[i]);
            }
        else
            {
            sprintf(cmd,"%s%c",open_copy,(unsigned char)clip[i]);
            }
        strcpy(open_copy,cmd);
        }
    strcpy(clip,open_copy);
        
    sprintf(cmd,".muste.getfile(\"%s\")",clip);
    muste_evalr(cmd);
    muste_get_R_string(retfilename,".muste$retrievedfile",LLENGTH);
    if (*retfilename==EOS) return(-1);
    return(1);
	}
		                
                
// y=muste_R_function(s+2,x,n,str_opnd);
double muste_R_function(char *s,double *x,int n)
	{
	int i;
	char luku[32];
	double y;
	
//	Rprintf("\ns: %s; x[0]: %f, n: %d",s,x[0],n);
	
	sprintf(cmd,".muste$rfu<-as.numeric(%s(%.16g",s,x[0]); // RS 9.10.2013 as.real -> as.numeric
	if (n>1) for (i=1; i<n; i++)
		{
		sprintf(luku,",%.16g",x[i]);
		strcat(cmd,luku);
		}
	strcat(cmd,"))");

//	Rprintf("\ncmd: %s",cmd);	
	muste_evalr(cmd);
	
	y=muste_get_R_real(".muste$rfu");

//	Rprintf("\nvast: %g",y);	
	return(y);
	}

int muste_beep()
	{
	muste_evalr("tkbell()"); // tai alarm()
	return(1);
	}

int sur_play_sound(char *nimi)
  {
  if (muste_requirepackage("survo.audio"))
    {
    sprintf(cmd,"survo.play(\"%s\")",nimi);
    muste_evalr(cmd);
    }
  else
    {
    muste_beep();
    }
// muste_fixme("\nFIXME: sur_play_sound not yet implemented!");
 return(1);
 }

int sur_play_tone(double freq,double time) // RS 17.12.2013
  {
  if (muste_requirepackage("survo.audio"))
    {
    sprintf(cmd,"survo.tone(%f,%f)",freq,time/1000);
//    Rprintf("\n%s,time: %f",cmd,time);
    muste_evalr(cmd);
    }
  else
    {
    muste_beep();
    }
 return(1);
 }

int muste_theme(int classic)
	{
	if (classic) muste_evalr(".muste.shadows(\"#FFFEFF\",\"#FFFEFF\")");
	else muste_evalr(".muste.shadows(\"white\",\"#F8F8F8\")");
	return(1);
	}

int muste_statusbar(int basic,int shadow)
	{
	extern int c,c1,c2,r,r1,r2,dispm;
	extern char *edisk;
	extern int insert_type,insert_mode;
	
/*	
	if (other!=NULL)
		{
		sprintf(str1,"tclvalue(.muste$status0 <- %s",other);
		muste_evalr(str1);
		}
	else
		{
		sprintf(str1,"tclvalue(.muste$status0) <- \" \"");
		muste_evalr(str1);
		}
	if (basic)
		{
		sprintf(str1,"tclvalue(.muste$status1) <- \"Row: %5d / %d \"",r1+r-1,r2);
		muste_evalr(str1);		
		sprintf(str1,"tclvalue(.muste$status2) <- \"Column: %4d / %d \"",c1+c-1,c2);
		muste_evalr(str1);
		strcpy(cmd,edisk); unsubst_survo_path_in_editor(cmd);
		snprintf(str1,256,"tclvalue(.muste$status3) <- \"Path: %s\"",cmd);
		muste_evalr(str1);		
		}

	if (other!=NULL)
		{
			      
		sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"%s\")",other);
		muste_evalr(str1);
		}
	else
		{
		sprintf(str1,"tkconfigure(.muste$statbarl0,text=\" \")");
		muste_evalr(str1);
		}
*/		
	if (basic)
		{
		sprintf(str1,"tkconfigure(.muste$statbarl1,text=\"Row: %5d / %d \")",r1+r-1,r2);
		muste_evalr(str1);		
		sprintf(str1,"tkconfigure(.muste$statbarl2,text=\"Column: %4d / %d \")",c1+c-1,c2);
		muste_evalr(str1);
		strcpy(cmd,edisk); unsubst_survo_path_in_editor(cmd);
		snprintf(str1,256,"tkconfigure(.muste$statbarl3,text=\"Path: %s\")",cmd);		
		muste_evalr(str1);
		
		if (insert_type && insert_mode)
			 sprintf(str1,"tkconfigure(.muste$statbarl4,text=\"Ins\",background=.muste$insertcursorcolor,foreground=\"white\")");
		else sprintf(str1,"tkconfigure(.muste$statbarl4,text=\"Ins\",background=\"white\",foreground=\"white\")");
//		else sprintf(str1,"tkconfigure(.muste$statbarl4,text='',background='')");
	
			
		muste_evalr(str1);
		switch (dispm)
			{
 case 1: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"1\",background=\"white\",foreground=\"red\")"); break;
 case 2: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"2\",background=\"white\",foreground=\"darkgrey\")"); break;
 case 3: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"3\",background=\"white\",foreground=\"blue\")"); break;
 case 4: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"4\",background=\"darkblue\",foreground=\"grey\")"); break;
 case 5: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"5\",background=\"yellow\",foreground=\"black\")"); break;
 case 6: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"6\",background=\"white\",foreground=\"forest green\")"); break;
 case 7: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"7\",background=\"blue\",foreground=\"white\")"); break;
 case 8: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"8\",background=\"darkblue\",foreground=\"yellow\")"); break;
 case 9: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"9\",background=\"white\",foreground=\"darkgrey\")"); break;
default: sprintf(str1,"tkconfigure(.muste$statbarl0,text=\"0\",background=\"white\",foreground=\"black\")"); break;			
			}
		muste_evalr(str1);		
		
		
		}		
	return(1);	
	}

int muste_stopeventloop()
   {

//    sprintf(komento,".muste.stop()");

   muste_evalr(".muste.end()");
//   R_CheckUserInterrupt(); // R_ProcessEvents();
//   muste_sleep(100);   
   return(1);
   }

/* 
SEXP Muste_Check(SEXP session)
   {
        sessions=muste_fopen(apufile,"wt"); // RS ADD
        if (sessions==NULL)
          {
          muste_eventlooprunning=FALSE;
          jatkuu=FALSE;
          muste_stopeventloop();
          error("\nMuste requires write access to its own directories!"); // RS ADD
          }
        muste_fclose(sessions);
    }    
*/
extern int op_gplot(char *op);
SEXP Muste_Command(SEXP para) // RS EDT 19.9.2012
{
extern int muste_lopetus;
extern int muste_window_existing;
extern int op_load(void);
extern void op_theme(void);
extern int g;
extern char *parm[];
extern char *word[];
extern char orig_setup[], current_setup[];
extern int sur_dump(void);
extern char sur_session[];
char *kojo, *txtparm;
//Rprintf("\nMuste_Command: %s",CHAR(STRING_ELT(para,0)));

kojo=(char *)CHAR(STRING_ELT(para,0));
if (strcmp(kojo,"LoadEdt")==0)
	{
	sur_dump();	
	g=2;
	parm[1]=(char *)CHAR(STRING_ELT(para,1));
	op_load();
	disp();
	return(para);
	}

if (strcmp(kojo,"Restore")==0)
	{
	muste_emergency_stop=1;
	return(para);
	}

if (strcmp(kojo,"DumpEdt")==0)
	{
	extern int sur_dump(void);
	sur_dump();
	return(para);
	}

if (strcmp(kojo,"SaveEdt")==0)
	{
	extern int muste_save_firstline(void);
	int i;
	i=muste_save_firstline();
	if (i<0) muste_set_R_int(".muste$saverror",1);
	return(para);
	}

if (strcmp(kojo,"SaveEdtName")==0)
	{
	extern int muste_save_firstline_name(char *name);
	word[1]=(char *)CHAR(STRING_ELT(para,1));
	muste_save_firstline_name(word[1]);
	return(para);
	}

if (strcmp(kojo,"RemovePlotWindows")==0)
	{
	g=3; parm[1]="/DEL"; parm[2]="ALL";
	op_gplot("GPLOT");
	return(para);
	}	
	
	
if (strcmp(kojo,"GetSaveName")==0)
	{	
	edread(str1,1);
    g=splitq(str1+1,parm,2);
    if (g>1) 
    	{
    	if (strcmp(parm[0],"SAVE")==0) muste_set_R_string(".muste$savename",parm[1]);
    	}
    return(para);
    }	

if (strcmp(kojo,"Theme")==0)
	{
	g=2;
	word[1]=(char *)CHAR(STRING_ELT(para,1));
	op_theme();
	disp();
	return(para);
	}	

if (strcmp(kojo,"Cut")==0)
	{
	extern int muste_cutselection(int type);
	extern int muste_selection;
	int i;

	sur_dump();	
	i=atoi((char *)CHAR(STRING_ELT(para,1)));
	if (i<10 && muste_selection)
		{	
		muste_cutselection(i);
		}
	else if (i>10) muste_cutselection(i);
	return(para);
	}	

if (strcmp(kojo,"Undo")==0)
	{
	extern int op_undo(void);
	op_undo();
	return(para);
	}	

if (strcmp(kojo,"Redo")==0)
	{
	extern int op_redo(void);
	op_redo();
	return(para);
	}

if (strcmp(kojo,"Require")==0)
	{
	txtparm=(char *)CHAR(STRING_ELT(para,1));
	if (strcmp(txtparm,"tcltk")==0)
	    {
        muste_evalr("require(tcltk)");
	    }
	else // RS 29.8.2013
	    {
	    muste_requirepackage(txtparm);
	    }
	return(para);
	}
	
if (strcmp(kojo,"Apufile")==0)
	{
	word[1]=(char *)CHAR(STRING_ELT(para,1));
	strcpy(orig_setup,word[1]);
	strcpy(current_setup,word[1]);
	return(para);
	}	

/*
if (strcmp(kojo,"HookLast")==0)
    {
//    sprintf(cmd,".Last.sys <- function() { cat('collection is invoked...') }");
sprintf(cmd,"qq <- q"); 
    muste_evalr(cmd);
sprintf(cmd,"quit <- q <- function(save = \"default\", status = 0, runLast = TRUE) { if (exists(\"editor\",where=.muste)) muste:::.muste.end(); else qq(save,status,runLast); }");
    muste_evalr(cmd);
    return(para);
    }
*/
if (strcmp(kojo,"Exit")==0)
	{
	muste_lopetus=TRUE;
	muste_window_existing=FALSE;
	}
	
return(para);
}   

/*
              case CODE_DOWN:
                if (r<r3) { r=r3; break; }
                i=lastline2();
                if (i<r3) { r1=1; r=i+1; if (i<1) r=1; disp(); break; }
                r1=i-6; r=8; if (r1>r2-r3+1) { r1=r2-r3+1; r=i-r1+2; }
                if (r>r3) r=r3;
                disp();

move_r1 mc1
move_r2 mc2

move_r1=r1+r-1; mc1=c1+c-1;


move_ind=3;

            m_move_ind=0; // 21.3.2004

            op_block(r1+r-1,c1+c-1);
            move_r1=mr1; move_r2=mr2;
            
int op_block(int rr,int cc)
        {
// RS REM        char block[LLENGTH];
        int i;

        m_move_ind=0; // 21.3.2004
        mr=rr; mc=cc;
        mr1=move_r1; mr2=move_r2;
        strcpy(survoblo,etmpd); strcat(survoblo,"SURVO.BLO");
        strcpy(survowrd,etmpd); strcat(survowrd,"SURVO.WRD");

        if (mr<0) block_erase();
        else
            {
            i=sur_move();
            if (i==2) return(1);
            }
        disp(); soft_disp(1);
        return(1);
        }            

          case 2:
            move_r2=r1+r-1; mc2=c1+c-1;
            if (move_r2<move_r1) { i=move_r1; move_r1=move_r2; move_r2=i; }
            if (!move_words && mc2<mc1) { i=mc1; mc1=mc2; mc2=i; }

            script_save(move_r1,move_r2,mc1,mc2); // 11.12.2005



*/

SEXP Muste_ExpandPath(SEXP infile)
	{
	SEXP res;
	int i,pit;
	
	pit=length(infile);
	PROTECT(res = allocVector(STRSXP, pit));
	
	for (i=0; i<pit; i++)
		{
		strncpy(cmd,(char *)CHAR(STRING_ELT(infile,i)),2*LLENGTH);
		muste_expand_path(cmd);
		SET_STRING_ELT(res, i, mkChar(cmd));
		}
	UNPROTECT(1);
	return(res);	
	}

int muste_selection=0;
int muste_no_selection=FALSE;
int muste_selection_running=FALSE;

SEXP Muste_Selection(SEXP session)
	{
	extern void move_clear(void);
//	extern int sur_locate();
	extern int r1,r3,c1,c2,c3,move_r1,move_r2,mc1,mc2,move_ind,m_move_ind;


	int i,j,k,seltype;

	if (muste_selection_running) // RS 22.11.2012 muste_no_selection
		{
//		Rprintf("\nret");
		return(session);
		}
	muste_selection_running=TRUE;

	muste_sleep(10);
	
	seltype=muste_get_R_int(".muste$selection");
	
//Rprintf("\nseltype: %d, move_ind: %d",seltype,move_ind);	
	if (seltype==1)
		{
		
		if ((move_ind && !muste_selection) || m_move_ind || muste_no_selection)
			{
			muste_selection_running=FALSE;
			return(session);
			}


		muste_selection=1; move_clear();
	    move_r1=r1+muste_get_R_int(".muste$selection.r1")-2;
	    mc1=c1+muste_get_R_int(".muste$selection.c1")-8;

		muste_set_R_int(".muste$selection.r1",move_r1);
		muste_set_R_int(".muste$selection.c1",mc1);
		
		muste_selection_running=FALSE;
		return(session);
		}
	if (seltype>1)
		{
		if (!muste_selection || muste_no_selection)
		{
		muste_selection_running=FALSE;
		return(session);
		}

		move_ind=3;	
//		endsel=muste_get_R_int(".muste$selection.r2");	
	
	    move_r1=muste_get_R_int(".muste$selection.r1");
	    mc1=muste_get_R_int(".muste$selection.c1");
		
	    move_r2=r1+muste_get_R_int(".muste$selection.r2")-2;
	    mc2=c1+muste_get_R_int(".muste$selection.c2")-8;

		j=0; k=0;
		if (seltype!=4)
			{
			if (move_r2-r1+2>r3+1) j=1;
			if (move_r2-r1+2<=1) j=-1;
			
//Rprintf("\nraja: mc2: %d, c1: %d, c3: %d",mc2,c1,c3);			
			if (mc2-c1+2>c3) k=1;
			if (mc2-c1+2<=1) k=-1;
			}

		muste_set_R_int(".muste$selection.r2",move_r2);
		muste_set_R_int(".muste$selection.c2",mc2);

		if (seltype==4)
			{
			move_r1=0; move_r2=0; mc1=0; mc2=0; // RS ADD
			
			move_ind=0;
			muste_selection=0; 
			}
		else if (j!=0 || k!=0)
				{
  				sprintf(cmd,".muste.yview(.muste$scry,\"scroll\",as.integer(%d),\"units\")",j);
  				if (j!=0) muste_evalr(cmd);
  				
  				sprintf(cmd,".muste.xview(.muste$scrx,\"scroll\",as.integer(%d),\"units\")",k);
  				if (k!=0) muste_evalr(cmd);
  			
//  				sprintf(cmd,"tcl(\"after\",100,.muste.yview(.muste$scry,\"scroll\",%d,\"units\"))",i);
  				sprintf(cmd,"tcl(\"after\",10,muste:::.muste.selcoord)");
  				muste_evalr(cmd);
  				
//tcl("after",100,.muste.yview(.muste$scry,\"scroll\",1,\"units\"))
//tkevent.generate(.muste$txt,"<Motion>")  				
				
  				}
		
		}

	if (move_r2<move_r1) { i=move_r1; move_r1=move_r2; move_r2=i; }
	if (mc2<mc1) { i=mc1; mc1=mc2; mc2=i; }
	
	if (mc2>c2) mc2=c2;

	if (muste_get_R_int(".muste$selection.alt")) { mc2=c2; if (mc1<1) mc1=0; else mc1=1; }
		
//Rprintf("\nr1: %d, c1: %d, r2: %d, c2: %d",move_r1,mc1,move_r2,mc2);	
	disp();
	
	muste_selection_running=FALSE;	
	return(session);
	}
	


static void muste_edt_dim()
	{
	extern int r,r1,r2,r3;
	extern int c,c1,c2,c3;
//	extern int lastline2();
    int first,last,max,end,cur;
    
    first=r1-1;
    last=r1+r3;
    max=r2;
    cur=r1+r-1;
    end=lastline2();
    if (first>end) end=last;
    if (end<last) end=last;
	
    muste_set_R_int(".muste$edty.first",first);
    muste_set_R_int(".muste$edty.last",last);
    muste_set_R_int(".muste$edty.max",max);
    muste_set_R_int(".muste$edty.end",end);
    muste_set_R_int(".muste$edty.cur",cur);

    first=c1-1;
    last=c1+c3;
    max=c2;
    cur=c1+c-1;
    end=max; // RS FIXME
    if (first>end) end=last;
    if (end<last) end=last;
    
    muste_set_R_int(".muste$edtx.first",first);
    muste_set_R_int(".muste$edtx.last",last);
    muste_set_R_int(".muste$edtx.max",max);
    muste_set_R_int(".muste$edtx.end",end);
    muste_set_R_int(".muste$edtx.cur",cur);
    
	}

SEXP Muste_Edtdim(SEXP session)
	{
	muste_edt_dim();
	return(session);
	}

int muste_mousewheel=TRUE;

SEXP Muste_Edtgoto(SEXP gotoparm)
	{
	int newfirst,newcur; // ,mousewheel;
	char *gprm[5];
//	extern int op_goto2();
//	extern int disp();
	char eka[256];
	char toka[256];
	char kolmas[256];
	char neljas[256];
	
	

/*
    mousewheel=muste_get_R_int(".muste$mousewheeltime");
    Rprintf("\neventpeek: %d, mousewheel: %d",muste_eventpeek,mousewheel);
    if (muste_eventpeek==FALSE && mousewheel!=9999) return(gotoparm);
*/	
    if (muste_mousewheel==FALSE || muste_no_selection) return(gotoparm);

	newfirst=muste_get_R_int(".muste$edty.newfirst");
	newcur=muste_get_R_int(".muste$edty.newcur");

    sprintf(eka,"%d",newfirst); gprm[1]=eka;
    sprintf(toka,"%d",newcur); gprm[2]=toka;

	newfirst=muste_get_R_int(".muste$edtx.newfirst");
	newcur=muste_get_R_int(".muste$edtx.newcur");    
    
    sprintf(kolmas,"%d",newfirst); gprm[3]=kolmas;
    sprintf(neljas,"%d",newcur); gprm[4]=neljas;    

    op_goto2(5,gprm);

//    muste_edt_dim();


    disp();
	return(gotoparm);
	}


SEXP Muste_RestoreEventloop(SEXP session)
	{
	muste_eventlooprunning = FALSE;
	muste_eventpeek = TRUE;
	return(session);
	}

SEXP Muste_Eventloop(SEXP session)
{
    int jatkuu,i,dispcall;
    extern int edrun;
    extern void remove_muste_related(void);

// Rprintf("\nIn eventloop!");

    if (muste_eventlooprunning)
        {
//        Rprintf("\nBusy Eventloop exit!");
        return(session);
        }
    muste_eventlooprunning=TRUE;
    
    
	muste_set_R_int(".muste$interrupt",0);

//    dispcall=muste_get_R_int(".muste$redraw");

// Rprintf("\nOpening Ajaxbuffer!");
	
	survo_open_ajaxbuffer(dispcall); // RS 10.12.2015

// Rprintf("\nAjaxbuffer opened!");

    if (dispcall!=2)
        {

// Rprintf("\nDispcall: %d!",dispcall);
	
/*    
    R_FlushConsole();
    R_ProcessEvents();
    Muste_EvalTcl("update idletasks",FALSE);
    Muste_EvalTcl("update",FALSE);
*/

//    muste_eventpeek=FALSE;

    jatkuu=1;

    if (etu==2)
        {
        muste_eventpeek=FALSE;
//        muste_save_stack_count();
        while (etu==2) { jatkuu=muste_editor_eventhandler(); }
//        muste_restore_stack_count();
        muste_eventpeek=TRUE;
        }

//    muste_eventpeek=TRUE;

//    if (muste_peekinputevent(FALSE))
//        {
//        muste_save_stack_count();
        jatkuu=muste_editor_eventhandler();
//        muste_restore_stack_count();        
//        }


	 i=muste_get_R_int(".muste$termination");
     if (jatkuu==FALSE || i) 
     	{
     	muste_stopeventloop();
		muste_set_R_int(".muste$jatkuu",0);
		edrun=0; // RS ADD 3.10.2012   	
		remove_muste_related();
		sprintf(cmd,".muste.remove.bindings()");
		muste_evalr(cmd);		
		muste_eventlooprunning=TRUE; // RS 25.11.2012 No more new events accepted
		return(session);
     	}
//    muste_eventpeek=FALSE;

    }
    else disp();
    
//    survo_ajax_screenbuffer(); // RS 1.12.2015
    survo_close_ajaxbuffer(); // RS 10.12.2015

    muste_eventlooprunning=FALSE;    
    return(session);
}

int muste_eventloop_enable()
	{
	sprintf(cmd,".muste$eventloop<-TRUE");
	muste_evalr(cmd);
	sprintf(cmd,".muste.eventloop()");
	muste_evalr(cmd);
	return(1);
	}

int muste_eventloop_disable()
	{
	sprintf(cmd,".muste$eventloop<-FALSE");
	muste_evalr(cmd);
	return(1);
	}

/*

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


SEXP Muste_FileShow(SEXP session)
{
    muste_file_show("A");
    return(session);
}


*/

//int Muste_GetKey(SEXP funktio, SEXP ymparisto)
//{
/* 
  const char funktio[] = "MusteGetKey\0";
   const char ymparisto[] = "muste.environment\0";

   SEXP a = allocVector(STRSXP, 2);
   PROTECT(a);
   SET_STRING_ELT(a, 0, mkChar(funktio));
   SET_STRING_ELT(a, 1, mkChar(ymparisto));
   UNPROTECT(1);
   return a;
*/   
//    return(INTEGER(eval(funktio,ymparisto))[0]);
//}

/*
SEXP Muste_WaitKoe(SEXP funktio, SEXP ymparisto)
{
    int i;
    i=Muste_GetKey(funktio,ymparisto);
    Rprintf("\n%d\n",i);
    return(funktio);
}
*/

/* .Call("Muste_SetCursorKoe",as.integer(10),as.integer(10)) 
SEXP Muste_SetCursorKoe(SEXP row, SEXP col)
{
    int i,j,k;
    i=INTEGER(row)[0];
    j=INTEGER(col)[0];
    k=sur_locate(i,j);
    return(row);
}

SEXP Muste_Write(SEXP row, SEXP col,SEXP sha)
{
    const char testi[] = "Kokeilu";
    int i,j,k;
    i=INTEGER(row)[0];
    j=INTEGER(col)[0];
    k=INTEGER(sha)[0];
    write_string(testi,7,k,i,j);
    return(row);
}


- - -

Actually, it just came to me that there is a hack you could use. 
The problem with it is that it will eat all errors, even if they 
were not yours (e.g. those resulting from events triggered the event loop), 
so I would not recommend it for general use. But here we go: 

static void chkIntFn(void *dummy) { 
  R_CheckUserInterrupt(); 
} 

// this will call the above in a top-level context so it won't longjmp-out of your context 
bool checkInterrupt() { 
  return (R_ToplevelExec(chkIntFn, NULL) == FALSE); 
} 

// your code somewhere ... 
if (checkInterrupt()) { // user interrupted ... } 

You must call it on the main thread and you should be prepared that it may take some time and may interact with the OS... 


*/


/* 
   Ideas for the following code are borrowed from IGraph R package made by Gabor Csardi 
*/

#define MUSTESTACKSIZE 3000

/*
#define muste_Calloc(n,t)    (t*) calloc( (size_t)(n), sizeof(t) )
#define muste_Realloc(p,n,t) (t*) realloc((void*)(p), (size_t)((n)*sizeof(t)))
#define muste_Malloc(n,t)    (t*) malloc( (size_t)(n), sizeof(t) )
#define muste_Free(p)        (free( (void *)(p) ), (p) = NULL)
*/

#define muste_resource_allocation(func,ptr) \
  MUSTE_REAL((muste_func_t*)(func), (ptr))


struct muste_protectedPtr
	{
  	int all;
  	void *ptr;
  	void (*func)(void*);
	};

typedef void muste_func_t (void*);

struct muste_protectedPtr muste_stack[MUSTESTACKSIZE*2];

int muste_stack_count=0;
int muste_stackdepth[MUSTESTACKSIZE];

// RS Globals needing stacking 13.2.2013
extern int spn;
extern char **spb2;
extern double *arvo;
extern int *spp;
extern unsigned int *spplace;

int muste_stack_spn[MUSTESTACKSIZE];
int muste_stack_splist[MUSTESTACKSIZE];
char **muste_stack_spa[MUSTESTACKSIZE];
char **muste_stack_spb[MUSTESTACKSIZE];
char **muste_stack_spb2[MUSTESTACKSIZE];
char **muste_stack_spshad[MUSTESTACKSIZE];
double *muste_stack_arvo[MUSTESTACKSIZE];
int *muste_stack_spp[MUSTESTACKSIZE];
unsigned int *muste_stack_spplace[MUSTESTACKSIZE];
int muste_stack_own_spec_line1[MUSTESTACKSIZE];
int muste_stack_own_spec_line2[MUSTESTACKSIZE];


int muste_show_resource_usage()
	{
    sprintf(cmd,"\nResource usage: %d",muste_stack[0].all); 
    sur_print(cmd);	
	return(1);
	}

void muste_initstack()
	{
	int i;
	muste_stack[0].all=0;
	muste_stack_count=0;
	for (i=0; i<MUSTESTACKSIZE; i++) muste_stackdepth[i]=0;
	}

int muste_save_stack_count(int debug)
	{
	int pal;
	pal=muste_stack_count;
//Rprintf("\nstack: %d, save_stack: %d",muste_stack_count,muste_stack[0].all);

/*
    if (debug) // RS 10.3.2014
        {
        sprintf(cmd,"\nsave_stack_count: %d, count: %d, res: %d",debug,muste_stack_count,muste_stack[0].all);
//        sur_print(cmd);
Rprintf(cmd);        
        }	
*/
	muste_stack_spn[muste_stack_count]=spn;
    muste_stack_spa[muste_stack_count]=spa;
    muste_stack_spb[muste_stack_count]=spb;
    muste_stack_spb2[muste_stack_count]=spb2;
    muste_stack_spb2[muste_stack_count]=spshad;
    muste_stack_arvo[muste_stack_count]=arvo;
    muste_stack_spp[muste_stack_count]=spp;
    muste_stack_spplace[muste_stack_count]=spplace;
	
	muste_stackdepth[muste_stack_count++]=muste_stack[0].all;
	

        
        	
	return(pal);
	}

void muste_clean(int lastremaining)
{
  int p;
//Rprintf("\nmuste_clean ");
  for (p=muste_stack[0].all-1; p>lastremaining; p--)
  {
//Rprintf("%d:%ld ",p,muste_stack[p].ptr);  
    muste_stack[p].func(muste_stack[p].ptr);
    muste_stack[p].ptr=NULL;
    muste_stack[p].func=NULL;
  }
  muste_stack[0].all=lastremaining;
}

int muste_restore_stack_count_manual(int override)
	{
	if (override>0)
	    {
	    if (override>muste_stack_count)
	        {
	        sprintf(cmd,"\nInvalid stack count: %d (%d)",muste_stack_count,muste_stack[0].all); 
            sur_print(cmd);
            muste_fixme(cmd);
            WAIT;
            return(-1);	
	        }
	    else
	        {
	        muste_stack_count=override;
	        muste_stackdepth[muste_stack_count]=0;
	        }
	    }
    else
        {	    
	    muste_stackdepth[muste_stack_count--]=0;
	    }

// Rprintf("\nstack: %d, restore_stack: %d",muste_stack_count,muste_stackdepth[muste_stack_count]);
	
	muste_clean(muste_stackdepth[muste_stack_count]);
	
	spn=muste_stack_spn[muste_stack_count];
    spa=muste_stack_spa[muste_stack_count];
    spb=muste_stack_spb[muste_stack_count];
    spb2=muste_stack_spb2[muste_stack_count];		
    spshad=muste_stack_spb2[muste_stack_count];
    arvo=muste_stack_arvo[muste_stack_count];
    spp=muste_stack_spp[muste_stack_count];
    spplace=muste_stack_spplace[muste_stack_count];
    
// Rprintf("\nrestored to %d, res: %d",muste_stack_count,muste_stack[0].all);    

	return(muste_stack_count);
	}

int muste_restore_stack_count()
    {
    return(muste_restore_stack_count_manual(0));
    }

int muste_free(void *p)
	{
  	int no;
  	for (no=muste_stack[0].all-1; no>=0; no--)
  		{
    	if (muste_stack[no].ptr==p) muste_stack[no].ptr=NULL;
  		}	
  	if (p!=NULL)  
  		{ 
//  		Free(p);  // RS free="normal", Free="R"
		free(p);
  		p=NULL; 
  		}
  	return 0;
	}

int muste_free2(void *p)
	{
//Rprintf("free:");	
  	if (p!=NULL) 
  	  {
//  	  Free(p); // RS free="normal", Free="R"
	  free(p);
  	  p=NULL;
  	  }
 	return 0;
	}

int muste_fclose(FILE *p)
	{
  	int no;
//Rprintf("\nfclose"); 	
  	for (no=muste_stack[0].all-1; no>=0; no--)
  		{
    	if (muste_stack[no].ptr==p) muste_stack[no].ptr=NULL;
  		}	
  	if (p!=NULL) fclose(p);
  	p=NULL;
  	return 0;
	}

int muste_fclose2(void *p)
	{
//Rprintf("\nfclose2");	
  	if (p!=NULL) fclose((FILE *)p);
  	p=NULL;
  	return 0;
	}



/*
 * Adds another element to the free list
 */

void MUSTE_REAL(void (*func)(void*), void* ptr)
{
  int no;  
  no=muste_stack[0].all;
//Rprintf(", no: %d",no);  
  if (no<0 || no>=MUSTESTACKSIZE) 
  	{
  	sprintf(cmd,"\nResource allocation error! no=%d",no); 
  	sur_print(cmd); sur_getch();
  	return;
  	}
  muste_stack[no].ptr=ptr;
  muste_stack[no].func=func;
  muste_stack[0].all++;
//Rprintf(":%ld..%d",ptr,muste_stack[0].all);  
}

void *muste_memset(void *s, int c, size_t n)
{
    unsigned char* p=s;
    while(n--)
        *p++ = (unsigned char)c;
    return s;
}

void *muste_realloc(void *p,size_t n)
	{
    int no,loytyi;

    if (p!=NULL)
    	{
    	no=muste_stack[0].all-1;
    	loytyi=0;
  		while (no>=0)
  			{
    		if (muste_stack[no].ptr==p) { loytyi=1; break; }
    		no--;
  			}	
  		if (no<0) no=0;
  		if (loytyi==0) 
  			{ 
  			p=muste_malloc(n); 
  			return(p);
  			}	
//		Free(p); 
		free(p);
		p=NULL;  // RS free="normal", Free="R"
//		p=(void *)Calloc(n,char); // RS malloc="normal", Calloc="R"
		p=(void *)malloc(n); // RS malloc="normal", Calloc="R"
        if (p!=NULL) muste_memset(p,0,n);		
	    muste_stack[no].ptr=p;
	    return(p);
		}
    p=muste_malloc(n);
	return(p);
	}



void *muste_malloc(size_t n)
	{
	void *ptr;
//Rprintf("\nmalloc");	
//	ptr=(void *)Calloc(n+1,char); // RS malloc="normal", Calloc="R"
	ptr=(void *)malloc(n+1); // RS malloc="normal", Calloc="R"
	if (ptr!=NULL) muste_memset(ptr,0,n);
	muste_resource_allocation(muste_free2,ptr);
	return(ptr);
	}


FILE *muste_fopen(char *path, char *mode)
	{
	char* mem;
	FILE *ptr;	
	mem=malloc(5*strlen(path)); // RS 26.11.2012
	if (mem==NULL) return(NULL);
	if (strncmp(path,"http://",7)==0 || strncmp(path,"https://",8)==0 || strncmp(path,"ftp://",6)==0) // RS 15.1.2013
		{
		muste_geturlfile(path,mem); // RS 29.8.2013	
		}
	else strcpy(mem,path);
	muste_iconv(mem,"UTF-8","CP850");
	muste_expand_path(mem);
	ptr=fopen(mem,mode);
	free(mem);
	muste_resource_allocation(muste_fclose2,ptr);	
	return(ptr);
	}

FILE *muste_fopen2(char *path, char *mode)
	{
	char* mem;
	FILE *ptr;	
	mem=malloc(5*strlen(path)); // RS 26.11.2012
	if (mem==NULL) return(NULL);
	if (strncmp(path,"http://",7)==0 || strncmp(path,"https://",8)==0 || strncmp(path,"ftp://",6)==0) // RS 15.1.2013
		{
		muste_geturlfile(path,mem); // RS 29.8.2013	
		}
	else strcpy(mem,path);
	muste_iconv(mem,"UTF-8","CP850");
	muste_expand_path(mem);
	ptr=fopen(mem,mode);
	free(mem);
	return(ptr);
	}

int muste_debug_print(char *teksti)
	{
	Rprintf("\n%s",teksti);
	return(1);
	}

