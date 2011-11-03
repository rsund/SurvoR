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

extern int s_init();
extern int s_end();
extern int op_arit();
extern int muste_corr();
extern int muste_var();
extern int muste_file_show();
extern int muste_editor();


extern int etu;
extern int muste_eventpeek;

static char komento[2*LLENGTH];
static char cmd[2*LLENGTH];


static int muste_eventlooprunning;


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


   char argument[256];
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

fixed_plot=0;
first_plot_number=1;
gplot_count=0;
max_hdl=MAX_HDL;

muste_init_plotwindows();

    strcpy(argument,CHAR(STRING_ELT(session,0)));
    
    muste_eventpeek=FALSE;
    muste_eventlooprunning=TRUE;
    i=muste_editor(argument);
    

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

//   sprintf(komento,"if (inherits(try(.muste.ans<-%s,silent=TRUE), \"try-error\")) FALSE else TRUE",cmd);   
   apu=apu2=apu3=NULL;
   apu=strchr(cmd,'('); apu2=strchr(cmd,' '); apu3=strchr(cmd,'<');   
   if ((apu2!=NULL && apu3!=NULL && (apu3-cmd)<(apu2-cmd)) || (apu2==NULL)) apu2=apu3;
   if (strncmp(cmd,".muste.",7)==0 && 
      (apu!=NULL && 
      (apu2==NULL || 
      (apu2!=NULL && (apu-cmd)<(apu2-cmd))))
      )
		{
		sprintf(komento,"if (inherits(try(.muste.ans<-muste:::%s,silent=FALSE), \"try-error\")) FALSE else TRUE",cmd);
		}
   else
   		{
   		sprintf(komento,"if (inherits(try(.muste.ans<-%s,silent=FALSE), \"try-error\")) FALSE else TRUE",cmd);
   		}

// Rprintf("EvalR: %s\n",komento); // RS DEBUG

   PROTECT(cmdsexp = allocVector(STRSXP, 1));
   SET_STRING_ELT(cmdsexp, 0, mkChar(komento));
   cmdexpr = PROTECT(R_ParseVector(cmdsexp, -1, &status, R_NilValue));
   if (status != PARSE_OK) {
       UNPROTECT(2);
// RS REM      error("Invalid call %s",cmd);
Rprintf("\nSyntax error!");
       return (R_NilValue);
   }
   for(i=0; i<length(cmdexpr); i++) ans = eval(VECTOR_ELT(cmdexpr,i),R_GlobalEnv);
   UNPROTECT(2); 
   if (INTEGER(ans)[0]==FALSE) return (R_NilValue);
   ans = findVar(install(".muste.ans"),R_GlobalEnv);
   return ans;
}

int muste_evalr(char *cmd)
   {
   int retstat;
   SEXP status;
   retstat=1;
   status=Muste_EvalRExpr(cmd);
   if (status==R_NilValue) retstat=-1;
   return retstat;
   }
   
 int muste_system(char *cmd,int wait)
	{
	if (wait) sprintf(komento,".muste.system(\"%s\",TRUE)",cmd);
	else sprintf(komento,".muste.system(\"%s\",FALSE)",cmd);	
    muste_copytofile(komento,"MUSTE.CMD");
    muste_evalsource("MUSTE.CMD");
/*    	
	muste_copy_to_clipboard(komento);        
    muste_evalclipboard();
*/    
  	return(1);
	}  

int muste_requirepackage(char *package)
  {
  SEXP avar=R_NilValue;
  int vast;

  sprintf(cmd,".muste.req<-FALSE");
  muste_evalr(cmd);
  
  snprintf(cmd,LLENGTH,".muste.req<-as.integer(length(find.package(\"%s\")))",package);  
  muste_evalr(cmd);

  avar = findVar(install(".muste.req"),R_GlobalEnv);
  vast=INTEGER(avar)[0];

  if (vast==FALSE)
    {
    sprintf(cmd,"\nRequired R-package %s not found!",package);
    sur_print(cmd);
    return(vast);
    }
  
  snprintf(cmd,LLENGTH,".muste.req<-as.integer(require(%s))",package);  
  muste_evalr(cmd);
  
  avar = findVar(install(".muste.req"),R_GlobalEnv);
  vast=INTEGER(avar)[0];

  if (vast==FALSE)
    {
    sprintf(cmd,"\nRequired R-package %s could not be loaded!",package);
    sur_print(cmd);
    }
  
  return(vast);  
  }

void muste_set_R_string(char *dest,char *sour)
  {
  snprintf(cmd,LLENGTH,"%s<<-\"%s\"",dest,sour);
  muste_evalr(cmd);
  }

int muste_get_R_string(char *dest,char *sour)
  {
  SEXP avar=R_NilValue;
 
  avar = findVar(install(sour),R_GlobalEnv);
  
  snprintf(dest,LLENGTH,"%s",CHAR(STRING_ELT(avar,0)));
  return(1);
  }

int muste_get_R_int(char *sour)
  {
  SEXP avar=R_NilValue;
  int vast;
 
  avar = findVar(install(sour),R_GlobalEnv);
  vast=INTEGER(avar)[0];
  return(vast);  
  }

int muste_stopeventloop()
   {

//    sprintf(komento,".muste.stop()");

   muste_evalr(".muste.stop()");
   R_CheckUserInterrupt(); // R_ProcessEvents();
   muste_sleep(100);   
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
 
SEXP Muste_Command(SEXP session)
{
extern int muste_lopetus;
  muste_lopetus=TRUE;
return(session);
}   

SEXP Muste_Eventloop(SEXP session)
{
    int jatkuu;

    if (muste_eventlooprunning) return(session);
    muste_eventlooprunning=TRUE;

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
        while (etu==2) { jatkuu=muste_editor_eventhandler(); }
        muste_eventpeek=TRUE;
        }

//    muste_eventpeek=TRUE;

//    if (muste_peekinputevent(FALSE))
//        {
        jatkuu=muste_editor_eventhandler();
//        }

     if (jatkuu==FALSE) muste_stopeventloop();
//    muste_eventpeek=FALSE;
    muste_eventlooprunning=FALSE;
    return(session);
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

#define MUSTESTACKSIZE 1000

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

struct muste_protectedPtr muste_stack[MUSTESTACKSIZE];

int muste_stack_count=0;
int muste_stackdepth[MUSTESTACKSIZE];

void muste_initstack()
	{
	int i;
	muste_stack[0].all=0;
	muste_stack_count=0;
	for (i=0; i<MUSTESTACKSIZE; i++) muste_stackdepth[i]=0;
	}

void muste_save_stack_count()
	{
//Rprintf("\nstack: %d, save_stack: %d",muste_stack_count,muste_stack[0].all);
	
	muste_stackdepth[muste_stack_count++]=muste_stack[0].all;
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

void muste_restore_stack_count()
	{
	muste_stackdepth[muste_stack_count--]=0;

//Rprintf("\nstack: %d, restore_stack: %d",muste_stack_count,muste_stackdepth[muste_stack_count]);
	
	muste_clean(muste_stackdepth[muste_stack_count]);
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
//  	p=NULL;
  	return 0;
	}

int muste_fclose2(void *p)
	{
//Rprintf("\nfclose2");	
  	if (p!=NULL) fclose((FILE *)p);
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
  	sprintf(komento,"\nResource allocation error! no=%d",no); 
  	sur_print(komento); sur_getch();
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
		free(p); p=NULL;
		p=(void *)malloc(n);
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
	ptr=(void *)malloc(n+1);
	if (ptr!=NULL) muste_memset(ptr,0,n);
	muste_resource_allocation(muste_free2,ptr);
	return(ptr);
	}

FILE *muste_fopen(char *path, char *mode)
	{
	FILE *ptr;	
	muste_expand_path(path);
	ptr=fopen(path,mode);
	muste_resource_allocation(muste_fclose2,ptr);	
	return(ptr);
	}

FILE *muste_fopen2(char *path, char *mode)
	{
	FILE *ptr;
	muste_expand_path(path);
	ptr=fopen(path,mode);
	return(ptr);
	}

