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

extern int s_init();
extern int s_end();
extern int op_arit();
extern int muste_corr();
extern int muste_var();
extern int muste_file_show();
extern int muste_editor();


extern int etu;
extern int muste_eventpeek;
extern int muste_iconv();

static char komento[2*LLENGTH];
static char cmd[2*LLENGTH];


static int muste_eventlooprunning;

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

//    strcpy(argument,CHAR(STRING_ELT(session,0)));
	muste_environment=session;
    
    muste_eventpeek=FALSE;
    muste_eventlooprunning=TRUE;  
    i=muste_editor("Muste"); // RS CHA argument);

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
Rprintf("\nSyntax error!");
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
   int retstat,i;
   SEXP status;   
   retstat=1;
   
   sprintf(apu,".muste <- muste:::.muste");
   Muste_EvalRExpr(apu);
   
   status=Muste_EvalRExpr(cmd);
   if (status==R_NilValue) retstat=-1; 
 
//   sprintf(apu,"remove(.muste)");
//   i=Muste_EvalRExpr(apu); 
 
   return retstat;
   }
   
 int muste_system(char *cmd,int wait)
	{
	extern char muste_command[];
	int i,j;

    int len;
    char *y,*clip;
    char tyhja[]="";
  
    len=strlen(cmd)+1;
//    y=Calloc(3*len,char); // RS ei muste_malloc, koska putsataan heti pois
    y=malloc(3*len);

	strcpy(y,cmd);
	muste_iconv(y,"","CP850");

	for (i=0; i<strlen(y); i++) 
		{
		if (y[i]=='"') y[i]='\''; 
		if (y[i]=='\\') y[i]='/';
		}
	
	if (strncmp(y,"DIR",3)==0 || strncmp(y,"dir",3)==0 ||
		strncmp(y,"LS",2)==0 || strncmp(y,"ls",2)==0)
		{
		if (strchr(y,' ')==NULL) clip=tyhja;
		else clip=strchr(y,' ')+1;
		if (wait) sprintf(komento,"muste:::.muste.dir(\"%s\",TRUE)",clip);
		else sprintf(komento,"muste:::.muste.dir(\"%s\",FALSE)",clip);		
		}
		else
		{
		if (wait) sprintf(komento,"muste:::.muste.system(\"%s\",TRUE)",y);
		else sprintf(komento,"muste:::.muste.system(\"%s\",FALSE)",y);	
    	}
    muste_copytofile(komento,muste_command); // "MUSTE.CMD");
    muste_evalsource(muste_command); // "MUSTE.CMD");
   
// Free(y);   
    free(y); 

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
    return(vast);
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

void muste_set_R_string(char *dest,char *sour)
  {
  snprintf(cmd,LLENGTH,"%s<-\"%s\"",dest,sour);
  muste_evalr(cmd);
  }

int muste_get_R_string_vec(char *dest,char *sour,int length,int element)
  {
  SEXP avar=R_NilValue;
  char *hakuapu;

  hakuapu=strchr(sour,'$')+1;
  if (hakuapu==NULL) hakuapu=sour;
  avar = findVar(install(hakuapu),muste_environment); // RS CHA R_GlobalEnv);  
  snprintf(dest,length,"%s",CHAR(STRING_ELT(avar,element)));
  return(1);
  }

int muste_get_R_string(char *dest,char *sour,int length)
  {
  muste_get_R_string_vec(dest,sour,length,0);
  return(1);
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
//  avar = findVar(install(sour),R_GlobalEnv);
  vast=INTEGER(avar)[element];
  return(vast);  
  }

int muste_get_R_int(char *sour)
  {
  return(muste_get_R_int_vec(sour,0));
  }

void muste_copy_to_clipboard(char *x)
    {
    int len;
    char *y,*clip;
    
    len=strlen(x)+1;
/* RS REM korvattu    
    w_codes_load(1);

    for (i=0; i<len-1; ++i)
        (unsigned char)clip[i]=code[(unsigned char)clip[i]];

*/
    int i,j;

//    y=Calloc(3*len,char); // RS ei muste_malloc, koska putsataan heti pois
//    clip=Calloc(3*len,char);

    y=malloc(3*len); // RS ei muste_malloc, koska putsataan heti pois
    clip=malloc(3*len);

	y[0]=EOS;
/* RS Handle Tcl-special characters: 34="  36=$  91=[  92=\       */
    for (i=0, j=0; i<len; i++) {
       		if (x[i]==34 || x[i]==36 || x[i]==91 || x[i]==92 ) y[j++]=92;
       		if ((unsigned char)x[i]==213) 
       			{
       			y[j]=EOS;
       			strcat(y,"\\u20AC"); j=strlen(y);
       			}
      		else y[j++]=x[i];
    }
    y[j]=EOS;

    muste_iconv(y,"","CP850");

    sprintf(komento,"clipboard clear");
    Muste_EvalTcl(komento,FALSE);

    sprintf(clip,"clipboard append \"%s\"",y);
    Muste_EvalTcl(clip,FALSE);

//   Free(clip);
//   Free(y); // RS ADD
	free(clip);
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


    SEXP avar;
    char *clip; 

    sprintf(cmd,".muste.getclipboard()");
    muste_evalr(cmd);

//    avar=R_NilValue;
//    avar=findVar(install(".muste$clipboard"),muste_environment);
//    clip=(char *)CHAR(STRING_ELT(avar,0));
    
    clip=cmd;    
    muste_get_R_string(clip,".muste$clipboard",LLENGTH*3);

    muste_iconv(clip,"CP850","");
    
    strcat(clip,"\n");
       
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

int muste_selection=0;
int muste_no_selection=FALSE;
int muste_selection_running=FALSE;

SEXP Muste_Selection(SEXP session)
	{
	extern void move_clear();
	extern int sur_locate();
	extern int r1,r3,c1,c2,c3,move_r1,move_r2,mc1,mc2,move_ind,m_move_ind;


	int i,j,k,seltype,endsel;

	if (muste_selection_running)
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
		if (!muste_selection)
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
			move_r1=0; move_r2, mc1=0; mc2=0; // RS ADD
			
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
	extern int lastline2();
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
	int newfirst,newcur,mousewheel;
	char *gprm[5];
	extern int op_goto2();
	extern int disp();
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
	muste_eventlooprunning <- FALSE;
	muste_eventpeek <- TRUE;
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

