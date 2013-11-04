#include "muste.h"
 /* _htmtab.c      */
#include  <string.h>
#include  <math.h>
#include  <stdlib.h>
// #include  <process.h>
//#include  <muste_malloc.h>
#include  <stdio.h>
//#include  <conio.h>
//#include  <dos.h>
#include <ctype.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"
//#include "mpkbase.h"
// #define MAXWAYS 9
#define MAXWAYS 100 // RS CHA 10 -> 100
static SURVO_DATA d;
/*                                                  */
/*  HTMTAB produces HTML-code for WEB-Tables        */
/*         otherwise like MTAB                      */
/*         written 23.6.1998 by mpk                 */
 static FILE *html_stream;
 static char *labls;
 static int nextln,pway,nd[MAXWAYS],former_message;
 static double *values,s,ss,*total_sums,*total_ss;
 static long *lkm,*total_lkm;
 static double *sz,*szy;
 static double max_gvals[MAXWAYS];
 static double *min_yvals,*max_yvals,*total_miny,*total_maxy;
 static int *iy;
 static int igv[MAXWAYS],igv_used[MAXWAYS],cutp[MAXWAYS],y_used;
 static int igv_vartype[MAXWAYS];
 static int ilis[MAXWAYS],ik[MAXWAYS];
 static int mh,ntulo,npot,ipot,isze,ny;
 static int dump,separ,nbas,isbs;
 static int maxn, max_des;
 static int pctdec;
 static int nbackgrvbles,inbas,ntlo,n_specs;
 static int col_given,row_given;
 static int rowtotals,coltotals,nototals,totalclass;
 static int statistics[25],prntopt[23];
 static int label_length,label_length1;
 static char mess_char;
 //static char fltr[MAXWAYS];
 static int maxp,ngv;
 static char *buffer[501],*gnest_fmt; // RS CHA 101 -> 501
 static int next_buffer;
 static int *col_address,*row_address;
 static int col_buffseq,row_buffseq;
 static int *y_width,*y_des;
 static int grp_width[MAXWAYS],grp_des[MAXWAYS];
 static int line_length;
 static char *grp_fmt,*y_fmt;
 static char g_formatarea[289];
 static char *nambuff[1000], xx[LLENGTH],yy[LLENGTH]; // RS CHA 100,512,128 -> LLENGTH
 static int ivariables,*ivbles_type;
 static double *ivbles_imin,*ivbles_imax;
 static char *ivbles_ilabs;
 static char htmltitle[256],htmlname[80],tabltitle[256],html_file[128];
 static char gtitle[1024],htmlremarks[256],hrefbase[20];
 static char tablbgcolor[12],tablhdcolor[12],tablvcolor[12];
 static char bodybgcolor[12],bodytextcolor[12],bodylinkcolor[12];
 static char bodyvlinkcolor[12],bodyalinkcolor[12];
 static char *y_title;
 static char ltotal[32];
 static int html_start, html_end, htmlfile_open, html_on, hrefvble;
/*                                                        */
 static char *p,*pspacelkm,*pspacesz,*pspaceszy,*pspacecomp,*result;
 static char *pspacemin,*pspacemax,*pspacetotmin,*pspacetotmax;
 static char *pspacetotlkm,*pspacetotals,*pspacetotalss;
 static char *pspace_nested;
 static char *p_statspacel, *p_statspaced;
 static long *lstarea;
 static double *dstarea;
 static int nested[9],igv_nested[48],pway_nested,igv_vartype_nested[9];
 static int cutp_nested[9],nested_maxn;
 static double *values_nested;
 static char *labels_nested,frmt_nested[289];
 static double max_gvals_nest[9];
 static int address_nested[248],n_of_nests,nest_width[9],nest_des[9]; // RS CHA 48 -> 248
 static int nested_exist,nd_nested[9],*change_points;
 static long n1;
 static int ijk,ijk2,ijk3;
 static int minc,statspacesize;

/* meanscmp.c */
static char *p_comp2,*pspace_cols;
static char *pspace_values,*pspace_labls;
static char *pspace2,*pspace_yfmt,*pspace_ydes,*pspace_changep;
static char *pspace_itype,*pspace_imin,*pspace_imax; 

/* mnsdefs.c */
static int real_vals;  

/* meansums.c */
static char nested_area[264];
static double nested_xval[8],xvert[8],gxvert[9];
static double *xvrt;
static char *pspace_xvrt;

/* optintpr.c  */
static char x2[LLENGTH]; // RS CHA 32 -> LLENGTH
// RS REM extern char *p;

/* listcomp.c  */                                                        
static char *p_wrk;
static int vble_seqs[2*200]; // RS ADD 2*
static int oper_list[2*200]; // RS ADD 2*

/* evaloprs.c    */                                         
static char *p_test;
static char *p_bf1,*p_bf2,*p_bf3;

/* meanspr.c    */
static int isbas,nwnd[10];
// static char cc[2*40]; // RS ADD 2*
static char *pspace_pr,*pspace_nl,*pspace_pr2;
static long *lkrt,*lkct;


/* mnsprmdv.c   */
static char *p_mnspr;
static int col_ways,col_lngth,col_vbltype,*col_objvbles,*col_gvbles;
static int *col_symbols;
static int row_ways,row_lngth,row_vbltype,*row_objvbles,*row_gvbles;
static int *row_symbols;
static char former_outlab[64];
static int cumulative_results,percents_results;
static long cum_rnobs,cum_cnobs;
static double cum_rsumy,cum_csumy,cum_rymin,cum_rymax,cum_cymin,cum_cymax;
// extern char former_outlab[];

 static void foutput(int);
 static int meanscmp(char *[]);
 static int meansums(void);
 static int meanspr(void);
// static int ipow2(int);
 static void linspace(char *,int);
// static void print_backgrvbles(int *);
// static int s_int(char *);
 static int vbl_frmt(int,int,int *,int *);
// static int mtb_message(int,int,int);
 static int spacealloc3(void);

 static int meanscmp(char *[]);
 static int meansums(void);
 static int meanspr(void);
// static void print_backgrvbles(int *);
 static int vbl_frmt(int,int,int *,int *);
// static int mtb_message(int,int,int);
 
 static int replace_underscore(char *source);
 static int replace_aakkos(char *source);
 static int rc_pros(int iyseq,int gnam[],int i_colseqs,int i_rowseqs,int required[],long *nobsdiv,
             double *divsum,int indcol);
static int ijth_cellstat(long *nobs,double *sumy,double *ssumy,double *ymin,double *ymax,
     long *n_grandtotal,double *s_grandtotal,long *n_coltotal,long *n_rowtotal,double *s_coltotal,
     double *s_rowtotal,long *n_tabletotal,double *s_tabletotal,
     int ithrow,int jthcol,int ind_oldrow,int *ynam);             
static int cellstat(long *nobs,double *sumy,double *ssumy,
		int ynam,int gnam[],int gseqs[],double *ymin,double *ymax,
     	long *n_grandtotal,double *s_grandtotal,long *n_coltotal,long *n_rowtotal,double *s_coltotal,
     	double *s_rowtotal,long *n_tabletotal,double *s_tabletotal,
     	int i_colseqs,int i_rowseqs);
static int eval_opers(int *op_list,int i_from,int i_to,int *vbl_seqs);  

/* meanscmp.c */
static int ano_spfind(int *,char *);
static int list_comp(int *, char *, int *);
static void linspace(char *,int);
static void linspace_long(char *,long);


 

static int optintpr(void);  
static int mnsdefs(void);
static int ivbldefs(void);  
static int a_spfind(char *);
static int comp_format(char *,int,int *,int *,int *);
static int mns_findvblelist(int *,int,int);
// static int mtb_message(int,int,int);
static int anovbl_spfind_fmt(int *,char *,int *, char *,int);
static int anovbl_spfind_yfmt(int *,char *,int *,char *,int); 

/* mnsdefs.c */
static void valsort2(double *,char *,int);
static void charsort(char *, double *, int *, int,int,int,int *,int *);
static void chr_sort(char *,int);
static int grpvals(int,int,char *,int);
static int grouperror(char *);
static int read_obs(long,int,int,double *, char *);

/* meansums.c */
static void linspace(char *,int);

static int read_obs(long,int,int,double *, char *);
static int comp_xval(int,double,int,int);
static int comp_xval_nested(int,double *,char *);
static void comp_xval_xvert(double *);
static void comp_nest_xvert(double *);
static int read_obs(long,int,int,double *,char *);

/* ivbldefs.c    */
static void valsort2(double *,char *,int);
static void charsort(char *, double *, int *, int,int,int,int *,int *);
static int grpvals(int,int,char *,int);

/* optintpr.c  */
static int a_spfind(char *); 

 /* listcomp.c  */
static int get_next_word(char *, char *, int, int *, int);
static int perform_operlist(int *,int *,int *,int);

/* evaloprs.c    */
static int new_buffer(int,int,int);
static int buffcheck(int *,int *,int *,int);
static int put_into_buffer(int *,int,int);
static int combine_buffers(int,int,int,int *);

/* meanspr.c    */
static void foutput(int);
static int mnsprmdv(void);
static int mns_prbackv(int,int,int);

/* mnsprmdv.c   */
static void linspace(char *,int);
static int stat_space(int);
static int blnkaway(char *,char *);
static int dep_frmt(int,int,int,int,double,long);
static int mns_labcpy(int,int,int,int);
static int mns_nest_labcpy(char [],int,int,int,int,int);
static int move_str(char *, char *,int,int);
static int get_colwidth(int);
static int get_column_width(int,int,int,int,int);
static int move_str(char *,char *,int,int);

/* getcolw.c  */
static void foutput(int);
static int blnkaway(char *,char *);
static int get_column_width(int,int,int,int,int);


/*                                                        */
void muste_mtab(int argc,char *argv[])
 {
  int i,l; // j
/*                                                               */
  if(argc==1)return;


// RS ADD Variable init

for (i=0; i<MAXWAYS; i++)
    {
    nd[i]=0;
    max_gvals[i]=0;
    igv[i]=0;
    igv_used[i]=0;
    cutp[i]=0;
    igv_vartype[i]=0;
    ilis[i]=0;
    ik[i]=0;
//    fltr[i]=0;
    grp_width[i]=0;
    grp_des[i]=0;
    }

 html_stream=NULL;
 labls=NULL;
 nextln=pway=0;
 former_message=0;
 values=NULL;
 s=ss=0;
 total_sums=NULL;
 total_ss=NULL;
 lkm=NULL;
 total_lkm=NULL;
 sz=NULL;
 szy=NULL;
 min_yvals=NULL;
 max_yvals=NULL;
 total_miny=NULL;
 total_maxy=NULL;
 iy=NULL;
 y_used=0;
 mh=ntulo=npot=ipot=isze=ny=0;
 dump=separ=nbas=isbs=0;
 maxn=max_des=0;
 pctdec=0;
 nbackgrvbles=inbas=ntlo=n_specs=0;
 col_given=row_given=0;
 rowtotals=coltotals=nototals=totalclass=0;
// statistics[25],prntopt[23];
 label_length=label_length1=0;
 mess_char=0;
 maxp=ngv=0;
for (i=0; i<501; i++) buffer[i]=NULL;
 gnest_fmt=NULL;
 next_buffer=0;
 col_address=NULL;
 row_address=NULL;
 col_buffseq=row_buffseq=-1;
 y_width=NULL;
 y_des=NULL;
 line_length=0;
 grp_fmt=NULL;
 y_fmt=NULL;
// g_formatarea[289];
for (i=0; i<1000; i++) nambuff[i]=NULL; 
// xx[512],yy[128];
 ivariables=0;
 ivbles_type=NULL;
 ivbles_imin=NULL;
 ivbles_imax=NULL;
 ivbles_ilabs=NULL;
// htmltitle[256],htmlname[80],tabltitle[256],html_file[128];
// gtitle[1024],htmlremarks[256],hrefbase[20];
// tablbgcolor[12],tablhdcolor[12],tablvcolor[12];
// bodybgcolor[12],bodytextcolor[12],bodylinkcolor[12];
// bodyvlinkcolor[12],bodyalinkcolor[12];
 y_title=NULL;
// ltotal[32];
 html_start=html_end=htmlfile_open=html_on=hrefvble=0;
 p=NULL;
 pspacelkm=NULL;
 pspacesz=NULL;
 pspaceszy=NULL;
 pspacecomp=NULL;
 result=NULL;
 pspacemin=NULL;
 pspacemax=NULL;
 pspacetotmin=NULL;
 pspacetotmax=NULL;
 pspacetotlkm=NULL;
 pspacetotals=NULL;
 pspacetotalss=NULL;
 pspace_nested=NULL;
 p_statspacel=NULL;
 p_statspaced=NULL;
 lstarea=NULL;
 dstarea=NULL;
// nested[9],igv_nested[48]
 pway_nested=0;
// igv_vartype_nested[9];
// cutp_nested[9]
 nested_maxn=0;
 values_nested=NULL;
 labels_nested=NULL;
for (i=0; i<289; i++) frmt_nested[i]=0;
// max_gvals_nest[9];
for (i=0; i<248; i++) address_nested[i]=-1;
 n_of_nests=0;
// nest_width[9],nest_des[9];
 nested_exist=0;
// nd_nested[9]
 change_points=NULL;
 n1=0;
 ijk=ijk2=ijk3=0;
 minc=statspacesize=0;
 p_comp2=NULL;
 pspace_cols=NULL;
 pspace_values=NULL;
 pspace_labls=NULL;
 pspace2=NULL;
 pspace_yfmt=NULL;
 pspace_ydes=NULL;
 pspace_changep=NULL;
 pspace_itype=NULL; 
 pspace_imin=NULL;
 pspace_imax=NULL; 
 real_vals=0;  
// nested_area[264];
// nested_xval[8],xvert[8],gxvert[9];
 xvrt=NULL;
 pspace_xvrt=NULL;
// char x2[32];                                                      
 p_wrk=NULL;
// vble_seqs[200];
for (i=0;i<200;i++) vble_seqs[i]=0;
for (i=0;i<200;i++) oper_list[i]=0;
// oper_list[200];                                        
 p_test=NULL;
 p_bf1=NULL;
 p_bf2=NULL;
 p_bf3=NULL;
 isbas=0;
// nwnd[10];
// cc[40];
 pspace_pr=NULL;
 pspace_nl=NULL;
 pspace_pr2=NULL;
 lkrt=NULL;
 lkct=NULL;
 p_mnspr=NULL;
 col_ways=col_lngth=col_vbltype=0;
 col_objvbles=NULL;
 col_gvbles=NULL;
 col_symbols=NULL;
 row_ways=row_lngth=row_vbltype=0;
 row_objvbles=NULL;
 row_gvbles=NULL;
 row_symbols=NULL;
// former_outlab[64];
 cumulative_results=percents_results=0;
 cum_rnobs=cum_cnobs=0;
 cum_rsumy=cum_csumy=cum_rymin=cum_rymax=cum_cymin=cum_cymax=0;
  
  
/*                                                              */
/* interpret the instructions                                   */
/*                                                              */
   htmlfile_open=0; html_on=0;
   max_des=3;   mess_char='1';
   i=meanscmp(argv);
   if(i<0)goto dclose;
/*                                                              */
/* phase for collecting sums of numbers, sums and crossproducts */
/*                                                              */
    i=spacealloc3();
    if(i<0)goto notenough;
    
    i=meansums();
/*  set the default formats if they are not given          */
    for(i=0;i<ngv;i++)
     {
      vbl_frmt(1,i,&grp_width[i],&grp_des[i]); // j=
     }
    if(n_of_nests>0)
     {
      for(i=0;i<=n_of_nests;i++)
       {
        vbl_frmt(2,i,&nest_width[i],&nest_des[i]); // j=
       }
     }
    for(i=0;i<ny;i++)
     {
      vbl_frmt(0,i,&y_width[i],&y_des[i]); // j=
     }
/*                                                         */
    i=output_open(eout);
    if(i<0)
     {
      sprintf(sbuf,"\nfailed to open output file");
      goto errclose;
     }
    if(html_start>0)
     {
      strcpy(xx,"<HTML><HEAD><TITLE>");
      strcat(xx,htmltitle);
      strcat(xx,"</TITLE>");
      foutput(0);
      strcpy(xx,"</HEAD><BODY");
      l=strlen(bodybgcolor);
      if(l>1){strcat(xx,"BGCOLOR="); strcat(xx,bodybgcolor);}
      l=strlen(bodytextcolor);
      if(l>1){strcat(xx,"TEXT="); strcat(xx,bodytextcolor);}
      l=strlen(bodylinkcolor);
      if(l>1){strcat(xx,"LINK="); strcat(xx,bodylinkcolor);}
      l=strlen(bodyvlinkcolor);
      if(l>1){strcat(xx,"VLINK="); strcat(xx,bodyvlinkcolor);}
      l=strlen(bodyalinkcolor);
      if(l>1){strcat(xx,"ALINK="); strcat(xx,bodyalinkcolor);}
      strcat(xx,"><A NAME=");
      strcat(xx,htmlname); strcat(xx,"></A>");
      foutput(0);

      strcpy(xx,"<P><CENTER><H1><B>");
      strcat(xx,htmltitle);
      strcat(xx,"</B></H1></CENTER>");
      foutput(0);
     }
    if(html_on>0)
     {
      i=strlen(htmlremarks);
      if(i>1)
       {
        strcpy(xx,"<FONT SIZE=1>");
        strcat(xx,htmlremarks);
        strcat(xx,"</FONT>");
        foutput(0);
       }
     }
/*                                                              */
    inbas=0;
    ntlo=1;
    for(i=0;i<pway;i++)
     {
      ntlo=ntlo*(nd[i]+1);
     }
/*                                                              */
      mh=ny;
/*       calling the printout routine originally a child   */
        i=(int)meanspr();
        if(i<0)goto lab3301;
/*                                                         */

      ;
lab3301: if(html_on>0)
          {
           if(html_end>0)
            {
             strcpy(xx,"</BODY></HTML>");
             foutput(0);
            }
           if(htmlfile_open>0)muste_fclose(html_stream);
          }
         output_close(eout);
         s_end(argv[1]);
dclose:  data_close(&d);
         return;
notenough: ; /* i=mtb_message(1,10,1);  */
     write_string("* Err001: Not enough memory!",28,mess_char,11,1);
     write_string("  Press any key!  ",18,mess_char,12,1); sur_getch();
errclose: goto dclose;
   }
/*                                           */

 static int spacealloc3(void)
 {
  int ldm1;
  int ijm;
  ntulo=ny;
  if(ny>0 && ivariables==1)ntulo=2*ny;
  if(ny==0)ntulo=1;
  for(ijm=0;ijm<pway;ijm++)
   {
    if(nd[ijm]>0)
     {
      ilis[ijm]=ntulo;
      ntulo=ntulo*(nd[ijm]+1);
     }
   }
  ldm1=ntulo;
  if(ny>0 && ivariables==0)
   {
    ntulo=ntulo/ny;
   }
  pspacelkm=(char *)muste_malloc(ldm1*sizeof(long));
  if(pspacelkm==NULL)return(-1);
  lkm=(long *)pspacelkm;
  ijm=ny; if(ijm<1)ijm=1;
  pspacetotlkm=(char *)muste_malloc(ijm*sizeof(long));
  if(pspacetotlkm==NULL)return(-1);
  total_lkm=(long *)pspacetotlkm;
  if(ny>0) // RS 26.11.2012 REM && ivariables==0)
   {
    pspacesz=(char *)muste_malloc(ldm1*sizeof(double));
    if(pspacesz==NULL)return(-1);
    sz=(double *)pspacesz;
    pspaceszy=(char *)muste_malloc(ldm1*sizeof(double));
    if(pspaceszy==NULL)return(-1);
    szy=(double *)pspaceszy;
    pspacemin=(char *)muste_malloc(ldm1*sizeof(double));
    if(pspacemin==NULL)return(-1);
    min_yvals=(double *)pspacemin;
    pspacemax=(char *)muste_malloc(ldm1*sizeof(double));
    if(pspacemax==NULL)return(-1);
    max_yvals=(double *)pspacemax;
    pspacetotmin=(char *)muste_malloc(ny*sizeof(double));
    if(pspacetotmin==NULL)return(-1);
    total_miny=(double *)pspacetotmin;
    pspacetotmax=(char *)muste_malloc(ny*sizeof(double));
    if(pspacetotmax==NULL)return(-1);
    total_maxy=(double *)pspacetotmax;
    pspacetotals=(char *)muste_malloc(ny*sizeof(double));
    if(pspacetotals==NULL)return(-1);
    total_sums=(double *)pspacetotals;
    pspacetotalss=(char *)muste_malloc(ny*sizeof(double));
    if(pspacetotalss==NULL)return(-1);
    total_ss=(double *)pspacetotalss;
    for(ijm=0;ijm<ldm1;ijm++)
     {
      sz[ijm]=0.0; szy[ijm]=0.0;
      min_yvals[ijm]=-9876.5;
      max_yvals[ijm]=-9876.6;
     }
    for(ijm=0;ijm<ny;ijm++)
     {
      total_miny[ijm]=-9876.5;
      total_maxy[ijm]=-9876.6;
      total_lkm[ijm]=0; total_sums[ijm]=0.0; total_ss[ijm]=0.0;
     }
    }
   for(ijm=0;ijm<ldm1;ijm++)
   {
    lkm[ijm]=0L;
   }
  if(ny<1)total_lkm[0]=0;
  return(1);
 }

static void linspace(char *xx,int k)
 {
  int i;
  for(i=0;i<k;i++)
   { xx[i]=' '; }
  xx[k]='\0';
  return;
 }

static int rmvblnks(char *zz,char *yy)
 {
  int i,l;
  l=strlen(zz)-1;if(l<0)return(l);
  for(i=l;i>=0;i--)
   { if(zz[i] != ' ')break; }
  i++;
  strcpy(yy,zz);
  yy[i]='\0';
  return(i);
 }
/*
static int labcpy(char *cc,int k,int j)
 {
  int i,l; // ,ii;
  long los,los9;
  los=k;
  if(j>0)los=los+j*maxn;
  los9=los*(label_length+1);
  strncpy(cc,labls+los9,label_length);cc[label_length]='\0';
  l=strlen(cc);
  i=replace_underscore(cc);
  i=replace_aakkos(cc);
  for(i=0;i<l;i++)
   { if(cc[i] != ' ')goto nonblank;
   }
  fnconv(values[los],8,cc); l=strlen(cc);
nonblank: return(l);
 } */
/* tÑmÑ ei liene kÑytîssÑ ?   */
// RS REM extern int max_des;
/* static int d_labcpy(char *cc,int k,int j)
 {
  int i,l; // ,ii;
  long los,los9;
  los=k; los=j*los;los9=los*(label_length+1);
  strncpy(cc,labls+los9,label_length);cc[label_length]='\0';
  l=strlen(cc);
  for(i=0;i<l;i++)
   { if(cc[i] != ' ')goto nonblank;
   }
  if(max_des != 0)
    fnconv(values[los],8,cc);
  else
    fconv(values[los],"1234567",cc);
  i=replace_underscore(cc);
  i=replace_aakkos(cc);
  l=strlen(cc);
nonblank: return(l);
 } */

static void foutput(int i)
 {
  int l; // j
  if(i==-1)strcpy(xx," ");
 strt:
  if(htmlfile_open>0)
   {
   l=strlen(xx);
    if(l>0)
     {
      xx[l]='\n';
      xx[l+1]='\0';
      fputs(xx,html_stream); // j=
      return;
     }
   }
  else
   {
    output_line(xx,eout,nextln);if(nextln)++nextln;
   }
  if(i<=0)return;
  strcpy(xx," ");i--; goto strt;
 }

/*  loadp anospfnd.c  */
/*                                                                 */
/*     ano_spfind to find values of single parameters 20.12.1987   */
/*                                                                 */
static int a_spfind(char *);
static int ano_spfind(int *par,char *parname)
 {
  int i;  
  i=a_spfind(parname);
  if(i>=0)
   {
/*    strcpy(xx,spb[i]);   */
    *par=atoi(spb[i]);
   }
  return(i);
 }
/*                                                                 */
/*     a_spfind as spfind but allows both small and capital letters  */
/*                                                                 */
// RS REM extern int n_specs;
static int a_spfind(char *parname)
 {
  int i,j;     
  i=spfind(parname); 
  if(i>=0)return(i);
  for(i=0;i<n_specs;i++)
   {
    j=muste_strcmpi(spa[i],parname);
    if(j==0){return(i);}
   }
   
  return(-1);
 }
static void linspace_long(char *xx,long k)
 {
  long i;
  for(i=0;i<k;i++)
   { xx[i]=' '; }
  xx[k-1]='\0';
  return;
 }

/* meanscmp.c   */
/*                                                                 */
/*     Interpreter for MEANS instructions  /   Markku Korhonen     */
/*                                             15.8.1989           */
/*                                                              */
static int meanscmp(char *argv[])
 {
  int i,j,k,l,ij,ijk,ngv1,format_given;
  long nestwrds,ij_long;
  char yyapu[LLENGTH]; // RS CHA 80 -> LLENGTH
/*  i=mask(&d);   */
/*  scales(&d);   */
  former_message=-99;
  col_address=&col_buffseq;   row_address=&row_buffseq;
  for(i=0;i<501;i++){buffer[i]=NULL;} // RS CHA 100 -> 501
  pspacelkm=NULL; pspacesz=NULL; pspaceszy=NULL;
  pspacemin=NULL; pspacemax=NULL;
  pspacetotmin=NULL; pspacetotmax=NULL;
  pspacetotlkm=NULL; pspacetotals=NULL; pspacetotalss=NULL;
  pspace_nested=NULL;
  p_statspacel=NULL; p_statspaced=NULL;
  statspacesize=0;
  grp_fmt=&g_formatarea[0];
  gnest_fmt=&frmt_nested[0];
  label_length=32;  /* default max of the length of labels/strings  */
  s_init(argv[1]);
  write_string(" +-----------------------------------------------------+ ",57,'4',6,10);
  write_string(" | MTAB - Version  0.33 for SURVO MM / M. Korhonen HUCC| ",57,'4',7,10);
  write_string(" +-----------------------------------------------------+ ",57,'4',8,10);  
  muste_sleep(1); // RS ADD 26.11.2012
  if(g<2)
   {
   write_string(" * Err009: Correct form: MTAB <data>,L ",39,mess_char,
       11,1);
   write_string("   Press any key! ",18,mess_char,12,1); sur_getch();
    return(-1);
   }
  nextln=0;
  if(g>2)
   {
    nextln=edline2(word[2],1,1);
    if(nextln==0)return(-1);
   }
  i=data_read_open(word[1],&d);
  if(i<0)return(-1);
  n_specs=sp_init(r1+r-1);
  if(n_specs<0)
    {
     write_string("* Err002: Not enough space for specifications! ",47,
     mess_char,11,1); sur_getch();
     return(-1);
    }
/*                                                              */
  col_buffseq=-1; row_buffseq=-1; next_buffer=0;
  col_address=&col_buffseq;
  pspace_yfmt=NULL; pspace_ydes=NULL; pspace_changep=NULL;
  pspace_values=NULL; pspace_labls=NULL;
  i=conditions(&d); if(i<0)return(-1);
  maxn=512; maxp=32; label_length=32;
  nested_maxn=512; n_of_nests=0;
  pway_nested=6; nested_exist=0;
/*     length of string variable values and labels    */
  label_length1=label_length+1;
  if(d.m_act<32)maxp=d.m_act;
/*     HTMLTITLE  title for the whole HTML-file         */
  html_start=0; html_end=0;
  i=ano_spfind(&html_start,"HTML_BEGIN");
  i=ano_spfind(&html_end,"HTML_END");
  if(html_start>0 || html_end>0)html_on=1;
  strcpy(ltotal,"Total");
  i=a_spfind("LTOTAL");
  if(i>=0)strcpy(ltotal,spb[i]);
  strcpy(htmltitle," ");
  i=a_spfind("HTMLTITLE");
  if(i>=0)
   { strcpy(htmltitle,spb[i]);
     j=replace_underscore(htmltitle);
     j=replace_aakkos(htmltitle);
   }
  strcpy(htmlname," ");
  i=a_spfind("HTMLNAME");
  if(i>=0){strcpy(htmlname,spb[i]);
           j=replace_underscore(htmlname);
           j=replace_aakkos(htmlname);
          }
  strcpy(tabltitle," ");
  i=a_spfind("TABLTITLE");
  if(i>=0){strcpy(tabltitle,spb[i]);html_on=1;
           j=replace_underscore(tabltitle);
           j=replace_aakkos(tabltitle);
          }
  strcpy(bodybgcolor," ");
  i=a_spfind("BODYBGCOLOR");
  if(i>=0){strcpy(bodybgcolor,spb[i]);html_on=1;
          }
  strcpy(bodytextcolor," ");
  i=a_spfind("BODYTEXTCOLOR");
  if(i>=0){strcpy(bodytextcolor,spb[i]);html_on=1;
          }

  strcpy(bodylinkcolor," ");
  i=a_spfind("BODYLINKCOLOR");
  if(i>=0){strcpy(bodylinkcolor,spb[i]);html_on=1;
          }
  strcpy(bodyvlinkcolor," ");
  i=a_spfind("BODYVLINKCOLOR");
  if(i>=0){strcpy(bodyvlinkcolor,spb[i]);html_on=1;
          }

  strcpy(bodyalinkcolor," ");
  i=a_spfind("BODYALINKCOLOR");
  if(i>=0){strcpy(bodyalinkcolor,spb[i]);html_on=1;
          }

  strcpy(tablbgcolor," ");
  i=a_spfind("TBGCOLOR");
  if(i>=0){strcpy(tablbgcolor,spb[i]);html_on=1;
          }
  strcpy(tablhdcolor," ");
  i=a_spfind("THDCOLOR");
  if(i>=0){strcpy(tablhdcolor,spb[i]);html_on=1;
          }
  strcpy(tablvcolor," ");
  i=a_spfind("TBVCOLOR");
  if(i>=0){strcpy(tablvcolor,spb[i]);html_on=1;
          }
  strcpy(htmlremarks," ");
  i=a_spfind("HTMLREMARK");
  if(i>=0){strcpy(htmlremarks,spb[i]); html_on=1;
           j=replace_underscore(htmlremarks);
           j=replace_aakkos(htmlremarks);
          }
  i=a_spfind("HTMLFILE");
  if(i>=0)
   {
    strcpy(xx,spb[i]);
    l=strlen(xx);
    for(ijk=0;ijk<l;ijk++)
     {
      if(xx[ijk]=='\\' || xx[ijk]==':')
       { strcpy(html_file,xx);
         goto avaa;
       }
     }
    strcpy(html_file,edisk);
    strcat(html_file,xx);
avaa: html_stream=muste_fopen(html_file,"w");
    if(html_stream==NULL)
     {
      ijk=sprintf(sbuf,"* Failed to open the file %.32s ",html_file);
      write_string(sbuf,ijk,mess_char,22,1); sur_getch();
      htmlfile_open=0;
     }
    else
     {  htmlfile_open=1; }
    html_on=1;
   }
  hrefvble=-1; strcpy(hrefbase," ");
  i=a_spfind("HREFVBLE");
  if(i>=0)hrefvble=varfind2(&d,spb[i],1);
  i=a_spfind("HREFBASE");
  if(i>=0)strcpy(hrefbase,spb[i]);
  j=strlen(hrefbase);
  for(i=j-1;i>=0;i--)
   { if(hrefbase[i] != ' ')break;
     hrefbase[i]='\0';
   }
/*                                                               */
/*    first check GROUPING/CLASS statement                       */
/*                (it has to be given!)                          */
  pway=0; ngv=0;
  linspace(grp_fmt,288);    /*  8*36 = 288    */
/*      nested  information                                      */
  linspace(gnest_fmt,288);
     pway_nested=6;
     for(j=0;j<pway_nested;j++)
      {
       igv_vartype_nested[j]=-1;
      }
     for(i=0;i<8;i++)
      {
       nested[i]=0;
       for(j=0;j<pway_nested;j++)
        {
         k=j+i*pway_nested;
         igv_nested[k]=-1;
         address_nested[k]=-1;
        }
      }
  i=ano_spfind(&nested_maxn,"MAXNEST");
  i=anovbl_spfind_fmt(&ngv,"GROUPING",igv,grp_fmt,1);
  if(i==-2)return(-1);
  if(i==-3)return(-1);
  if(i<0)
   {
    j=anovbl_spfind_fmt(&ngv,"CLASS",igv,grp_fmt,1);
    if(j>=0)i=j;
    if(j==-2)return(-1);
    if(j==-3)return(-1);
   }
 /*      form from grp_fmt -list the following lists            */
 /*          grp_width  contains the format lengths               */
 /*          grp_des    contains the n of decimals, -1: not given */
   if(ngv==0)goto nogrping;   
   for(i=0;i<ngv;i++)
    {
     k=comp_format(grp_fmt,i,&grp_width[i],&grp_des[i],
                    &format_given);
     if(k<0)return(-1);
    }    /* end of loop for grouping vbles      */
   if(n_of_nests>0)
    {
     for(i=0;i<=n_of_nests;i++)
      {
       k=comp_format(gnest_fmt,i,&nest_width[i],&nest_des[i],
         &format_given);
      }
    }
/*      check the object variables                                    */
/*      VARIABLES  specification defines variables at interval scale  */
/*      IVARIABLES specification defines indicator variables          */
/*      VARIABLES and IVARIABLES may not be used at the same time     */
nogrping: i=ano_spfind(&maxp,"MAXVBLS");
  ivariables=0;
  p_comp2=(char *)muste_malloc(maxp*sizeof(int));
  if(p_comp2==NULL)goto notenough;
  iy=(int *)p_comp2;
  y_fmt=(char *)muste_malloc((36*maxp+1)*sizeof(char));
  if(y_fmt==NULL)goto notenough;
  linspace(y_fmt,36*maxp+1);
  y_title=(char *)muste_malloc((41*maxp+1)*sizeof(char));
  if(y_title==NULL)goto notenough;
  ny=0;
/*     y_fmt will contain possible format definitions                */
/*     VARIABLES/VARIABLE                                            */
  i=anovbl_spfind_yfmt(&ny,"VARIABLES",iy,y_fmt,1);
  if(i==-2)return(-1);
  if(i==-3)return(-1);
  if(i<0)
   {
    j=anovbl_spfind_yfmt(&ny,"VARS",iy,y_fmt,1);
    if(j<0)j=anovbl_spfind_yfmt(&ny,"VARIABLE",iy,y_fmt,1);
    if(j>=0)i=j;
    if(j==-2)return(-1);
    if(j==-3)return(-1);
   }
/*                                                                   */
/*     IVARIABLES                                                    */
/*                                                                   */
  if(i<0)
   {
    j=anovbl_spfind_yfmt(&ny,"IVARIABLES",iy,y_fmt,1);
    if(j==-3)return(-1);
    if(j==-2)return(-1);
    if(j>=0)
     {
      pspace_itype=NULL; pspace_imin=NULL; pspace_imax=NULL;
      ivbles_ilabs=NULL;
      ivariables=1;
      pspace_itype=(char *)muste_malloc(maxp*sizeof(int));
      if(pspace_itype==NULL)goto notenough;
      ivbles_type=(int *)pspace_itype;
      pspace_imin=(char *)muste_malloc(maxp*sizeof(double));
      if(pspace_itype==NULL)goto notenough;
      ivbles_imin=(double *)pspace_imin;
      pspace_imax=(char *)muste_malloc(maxp*sizeof(double));
      if(pspace_imax==NULL)goto notenough;
      ivbles_imax=(double *)pspace_imax;
      ivbles_ilabs=(char *)muste_malloc(maxp*label_length1*sizeof(char));
      if(ivbles_ilabs==NULL)goto notenough;
      k=ivbldefs();
      if(k<0)return(-1);
     }
   }
  mh=ny;
  if(ny==0)goto nodeps;
 /*                                                           */
 /*      form from y_fmt -list the following lists            */
 /*          y_width  contains the format lengths               */
 /*          y_des    contains the n of decimals, -1: not given */
   pspace_yfmt=(char *)muste_malloc(ny*sizeof(int));
   if(pspace_yfmt==NULL)goto notenough;
   y_width=(int *)pspace_yfmt;
   pspace_ydes=(char *)muste_malloc(ny*sizeof(int));
   if(pspace_ydes==NULL)goto notenough;
   y_des=(int *)pspace_ydes;
   for(i=0;i<ny;i++)
    {
     k=comp_format(y_fmt,i,&y_width[i],&y_des[i],
                   &format_given);
     if(k<0)return(-1);
     strcpy(y_title+i*41," ");
     strcpy(yyapu,d.varname[iy[i]]);
     strcat(yyapu,"TITLE");
     k=(int)spfind(yyapu);
     if(k>=0)
      {
       strcpy(y_title+i*41,spb[k]);
       ijk=replace_underscore(y_title+i*41);
       if(html_on>0)ijk=replace_aakkos(y_title+i*41);
      }
     else
      {
       strcpy(y_title+i*41,d.varname[iy[i]]);
       if(html_on>0)ijk=replace_aakkos(y_title+i*41);
      }
     }
nodeps:if(ngv==0)goto lab708;
/*     definitions for grouping variables           */
   i=ano_spfind(&maxn,"MAXNCL");
   pway=ngv;
   ngv1=ngv;if(ngv<1)ngv1=1;
    k=ngv1*maxn;
    ij_long=k*label_length1+1;
    pspace_values=(char *)muste_malloc(k*sizeof(double));
    if(pspace_values==NULL)goto notenough;
    values=(double *)pspace_values;
    pspace_labls=(char *)muste_malloc(ij_long*sizeof(char));
    if(pspace_labls==NULL)goto notenough;
    labls=(char *)pspace_labls;
    linspace_long(labls,ij_long);
    if(n_of_nests>0)
     {
      ij=(n_of_nests+1)*nested_maxn;
      pspace_nested=(char *)muste_malloc(ij*sizeof(double));
      if(pspace_nested==NULL)goto notenough;
      values_nested=(double *)pspace_nested;
      pspace_nested=NULL;
      nestwrds=label_length1;
      nestwrds=nestwrds*ij+1;
      pspace_nested=(char *)muste_malloc(nestwrds*sizeof(char));
      if(pspace_nested==NULL)goto notenough;
      labels_nested=(char *)pspace_nested;
      linspace_long(labels_nested,nestwrds);
      pspace_changep=NULL;
      pspace_changep=(char *)muste_malloc(ij*sizeof(int));
      if(pspace_changep==NULL)goto notenough;
      change_points=(int *)pspace_changep;
     }
lab708: 	i=mnsdefs();
    if(i<0)return(-1);
    j=optintpr();   /* options for statistics, etc.   */
    line_length=ed1-2;
    i=ano_spfind(&line_length,"MAXCOLS");
    if(line_length>ed1)line_length=ed1;
    pctdec=2;
    i=ano_spfind(&pctdec,"PCTDEC");
    if(pctdec>5)pctdec=5;
/*                                                       */
/*   check if row or col definitions are given           */
/*                                                       */
  col_given=0; row_given=0;
  next_buffer=0;
  y_used=0;
  i=a_spfind("COL");
  if(i>=0)
   {
    k=0; col_given=1; totalclass=coltotals;
    j=(int)list_comp(col_address,spb[i],&k);
    if(k>1)y_used=1;
    if(j<0)return(-1);
   }
  i=a_spfind("ROW");
  if(i>=0)
   {
    k=0;
    row_given=1; totalclass=rowtotals;
    j=(int)list_comp(row_address,spb[i],&k);
    if(j<0)return(-1);
    if(k==1 && ny>0)
     {
      if(y_used==0 && col_given==1)
       {
 write_string(" *Err005: Object variables not mentioned in COL or ROW specifications! ",71,
         mess_char,11,1); sur_getch();
         return(-1);
       }
     }
    if(k>1)
     {
      if(y_used==0)
       {
        y_used=2;
       }
      else
       {
 write_string(" *Err006: Object variables may not be used both in COL and ROW specifications! ",79,
        mess_char,11,1); sur_getch();
         return(-1);
       }
     }
   }
  return(1);
notenough: ;
 write_string(" *Err001: Not enough memory!  Press any key! ",45,
   mess_char,11,1); sur_getch();
  return(-1);
 }
/*                                          */
static int replace_underscore(char *source)
  {
   int i,j,ijk;
   ijk=strlen(source); i=0;
   for(j=0;j<ijk;j++)
    {
     if(source[j]=='_'){ source[j]=' ';i++;}
     if(source[j]==';'){ source[j]=',';i++;}
    }
   return(i);
  }

static int replace_aakkos(char *source)
  {
   int j,ijk; // i
   ijk=strlen(source);
   for(j=0;j<ijk;j++)
    {
     if(source[j]=='\204')source[j]='\344'; 
     if(source[j]=='\224')source[j]='\366';
     if(source[j]=='\206')source[j]='\345';
     if(source[j]=='\216')source[j]='\304';
     if(source[j]=='\217')source[j]='\326';
     if(source[j]=='\231')source[j]='\305';
    }
   return(1);
  }
static int anovbl_spfind_fmt(int *npar,char *parname,int *parlist,char *flist,int indik)
 {
  int i,i24,j,k,l,k1,k2,n_f,n_f2,k_frst,k_strted;
  int n_exist,ijk,no_format, n_ofnested, ivble; // no_given,
  char *p,x2[256],vble[12];

/*  return value 1: normal return  -1: not found              */
/*              -2: error (message not yet printed            */
/*              -3: error, message already printed            */
  *npar=0;
  i=a_spfind(parname);
  if(i<0)return(-1);
  if(i>=0)
   {
/*    8 is the maximum number of grouping variables   */
    j=split(spb[i],nambuff,maxp);
    *npar=j;
    if(j>8)
     {
      write_string(" *Err: At most 8 grouping variables",35,mess_char,
        11,1);sur_getch();
      return(-2);
     }
    for(i=0;i<*npar;i++)
     {
        i24=i*36;
        p=(char *)strcpy(x2,nambuff[i]); if (p==NULL) return(-1); // RS 4.2.2013 ADD if    
        l=strlen(x2);
        n_exist=0;
        for(k=0;k<l;k++)
         {
          if(x2[k]=='['){n_exist=1;nested[i]=1;break;}
         }
 /*      check formats and nested information      */
        k1=-1; k2=-1; k_frst=0; k_strted=l;
        no_format=1; n_ofnested=0;
        if(n_exist==1)
         {
          for(k=0;k<l;k++)
           {
            if(x2[k]=='(' || x2[k]=='[')break;
           }
          k_strted=k;
          strncpy(vble,x2,k);
          vble[k]='\0';
          ivble=varfind2(&d,vble,1);
          if(ivble<0)return(-3);
          ijk=n_ofnested+i*pway_nested;
          address_nested[ijk]=n_of_nests;
          igv_nested[ijk]=ivble;
          k_frst=k;
          nested_exist=1;
         }
nextpart:   if(k_frst>l)goto nextdef;
        for (k=k_frst;k<l;k++)
         {
          if(x2[k]=='(')goto labformat;
          if(x2[k]=='[')goto nestlab;
          if(x2[k]==']')goto nextdef;
         }
        if(no_format==1)
           {
            if(n_ofnested==0)
             { strncpy(flist+i24,"def",3);
               flist[i24+3]='\0';
             }
            if(n_exist>0)
             {
              n_f=36*n_of_nests;
              strncpy(gnest_fmt+n_f,"def",3);
              gnest_fmt[n_f+3]='\0';
             }
           }
          goto nextdef;
/*                                                                   */
/*    formats given in parenthesis                                   */
/*    ============================                                   */
/*    strings in parenthesis into flist                              */
/*                                                                   */
labformat: if(k<k_strted)k_strted=k;
        k1=k+1; k2=-1;
        for(k=k1;k<l;k++)
         {
          if(x2[k]==')'){k2=k;goto lab2;}
         }
errlab: ;
     write_string("*Err008: Left or right parenthesis is missing! ",47,
         mess_char,11,1); sur_getch();
        return(-2);
lab2:   n_f=k2-k1;
        if(n_ofnested==0)
          {
           strncpy(flist+i24,x2+k1,n_f);
           flist[i24+n_f]='\0';
          }
         if(n_exist>0)
          {
           n_f2=36*n_of_nests;
           strncpy(gnest_fmt+n_f2,x2+k1,n_f);
           gnest_fmt[n_f2+n_f]='\0';
          }
        k_frst=k2+1; /* no_given=0; */ no_format=0;
        goto nextpart;
/*   next check for nested def                           */
/*   =========================                           */
nestlab:  if(no_format==1)
           {
            if(n_ofnested==0)
             { strncpy(flist+i24,"def",3);
               flist[i24+3]='\0';
             }
            n_f=36*n_of_nests;
            strncpy(gnest_fmt+n_f,"def",3);
            gnest_fmt[n_f+3]='\0';
           }
        k1=k+1; k2=-1;
        for(k=k1;k<l;k++)
         {
          if(x2[k]==']' || x2[k]=='(' || x2[k]=='['){k2=k;goto lab3;}
         }
        goto errlab;
lab3:   strncpy(vble,x2+k1,k2-k1);
        vble[k2-k1]='\0';
        ivble=varfind2(&d,vble,1);
        if(ivble<0)return(-3);
        n_ofnested++; n_of_nests++;
        ijk=n_of_nests+i*pway_nested;
        address_nested[ijk]=n_of_nests;
        igv_nested[ijk]=ivble;
        k_frst=k2;
        if(x2[k]=='(')k_frst=k2;
        no_format=1;
        goto nextpart;
/*                                                       */
/* delete parenthesis part from nambuff                  */
nextdef: x2[k_strted]='\0';
        strcpy(nambuff[i],x2);
        if(no_format==1)
         {
          n_f=36*n_of_nests;
          strncpy(gnest_fmt+n_f,"def",3);
          gnest_fmt[n_f+3]='\0';
         }
    }    /*   end of loop for variables in VBLES        */
/*             next get the indices for variables        */
    j=mns_findvblelist(parlist,*npar,indik);
   if(j<0)return(-3);
   }
  return(1);
 }
static int anovbl_spfind_yfmt(int *npar,char *parname,int *parlist,char *flist,int indik)
 {
  int i,i24,j,k,l,k1,k2;
  char *p,x2[32];
  *npar=0;
  i=a_spfind(parname);
  if(i<0)return(-1);
  if(i>=0)
   {  
    j=split(spb[i],nambuff,maxp);
    *npar=j;
    for(i=0;i<*npar;i++)
     {
        i24=i*36;
        p=(char *)strcpy(x2,nambuff[i]); if (p==NULL) return(-1); // RS 4.2.2013 ADD if
        l=strlen(x2);
        k1=-1; k2=-1;
        for (k=0;k<l;k++)
         {
          if(x2[k]=='(')k1=k;
          if(x2[k]==')')k2=k;
         }
        if(k1==-1 && k2==-1)
         {
          strncpy(flist+i24,"def",3);
          flist[i24+3]='\0'; continue;
         }
        if(k1>=0 && k2>=0)
         {
          x2[k2]='\0';
          strncpy(flist+i24,x2+k1+1,k2-k1-1);
          flist[i24+k2-k1-1]='\0';
          x2[k1]='\0';
          strcpy(nambuff[i],x2); continue;
         }
write_string("*Err008: Left or right parenthesis is missing! ",47,
        mess_char,11,1); sur_getch();
        return(-2);
      }    /*   end of loop for variables in VBLES        */
/*             next get the indices for variables        */
    j=mns_findvblelist(parlist,*npar,indik);
    if(j<0)return(-3);
   }
  return(1);
 }
/*                                   */
static int mns_findvblelist(int *ivbles,int nvbls,int indik)
 {
  int j,k; // i
  for(j=0; j<nvbls; j++)
   {
/*    if(indik<0)
     {
      i=-indik-1;
     } */
    if(indik<2)
     {
      k=varfind2(&d,nambuff[j],1);
      if(k<0)return(-1);
     }
    ivbles[j]=k;
  }
  return(0);
 }
static int comp_format(char *source,int index,int *width,int *des,int *format_given)
 {
  char x2[37],x_intpart[37],x_despart[37];
  int i=0,ind24,l,ifend,ipros,ipoint;
  double cs;
/*                                              */
     ind24=index*36;
     strcpy(x2,source+ind24);
     l=strlen(x2);
     if(l>0)i=(int)strcmp(x2,"def");
/*     width/des = -1 means unknown or default         */
     if(i==0 || l==0)
      {
       *format_given=0; *width=-1; *des=-1;
       return(1);
      }
     ipoint=-1; ifend=-1;
     for(i=0;i<l;i++)
      {
       if(x2[i]=='f')ifend=i;
       if(x2[i]=='.')ipoint=i;
      }
     ipros=0;
     if(x2[0]=='%')ipros=1;
     strcpy(x_intpart," ");
     strcpy(x_despart," ");
     if(ipros==1)
      {
/*        format given in the form   %5.2f      */
       if(ipoint<0 || ifend<=ipoint)
        {
write_string("*Err010: Erreneous printing format ",35,mess_char,11,1);
         sur_getch(); return(-1);
        }
       strcpy(x_intpart,x2+1);
       x_intpart[ipoint+1]='\0';
       strcpy(x_despart,x2+ipoint+1);
       x_despart[ifend-ipoint-1]='\0';
       *format_given=1;
       cs=(double)atof(x_intpart);
       *width=cs;
       cs=(double)atof(x_despart);
       *des=cs;
      }
/*        format given in the form ###.### or 123.123  */
     if(ipros==0)
      {
       l=strlen(x2); *width=l; *des=0;
       if(ipoint>0)*des=l-ipoint-1;
       if(ipoint==0)*des=l-1;
      }
    return(1);
   }
 FILE *messag;
/*                                                                 */
/*                                                                 */
// RS REM extern int former_message;
// RS REM extern char xx[];
// RS REM void foutput(int);
// RS REM int write_string(char *,int,char,int,int);
/*   message is copied to sbuf and written on the screen if line_no>0 */
/*              to xx if line_no==-8 and written to output file       */
/*              to xx if line_no==-9 and not written to output file   */
/*static int mtb_message(int mess_no,int line_no,int press_ind)
 {
  int i,l,ijk;
  char *reslt,mess_char;

  ijk=0;
  if(former_message+1==mess_no){ijk=mess_no; goto notopen;}
  if(former_message != -99)muste_fclose(messag);
  messag=muste_fopen("messages.mtb","r");
  if(messag==NULL)
   {
    write_string("Message file missing in the Survo directory!",45,
    '1',8,12); return(-1);
   }
notopen:  for(i=ijk;i<=mess_no;i++)
   {
    reslt=(char *)fgets(sbuf,164,messag);
    if(reslt==NULL)
     { write_string("Failed to give an error message!",32,
       '1',8,12); return(-1);
     }
   }
  former_message=mess_no;
  l=strlen(sbuf);
  for(i=l-1; i>=0;i--)
   {
    if(sbuf[i] == '\n')break;
   }
  sbuf[i]=' '; sbuf[i+1]='\0'; l=i+2;
  mess_char='1';
  if(line_no<=-8)strcpy(xx,sbuf);
  if(line_no==-8)foutput(0);
  if(line_no>0)
   {
    write_string(sbuf,l,mess_char,line_no,1);
    if(press_ind>0)
     write_string(" Press any key!",15,mess_char,line_no+1,1);
    if(press_ind>=0)sur_getch();
   }
  return(l);
 } */

 /*   mnsdefs.c   */
static int mnsdefs(void)
 {
  int i,i2,ij2,jfnd,j,iij,jjj,k,l2,j2,ij,ky,kk,isort; // l
  int ivrt,jmaxn,lenofvar=0,j_textvbl;
  int str_maxlen,str_maxlen_nest[8],i_str,textvbl[8];
  int m,kk_nest=0,nestaddr,iaddr;
  long iii,kk9,i2long;
  double xval,csval,nested_xval[8];
  char xyz[24],nested_area[264];
  char yypar[64];
/*                                                                        */

  for(j=0;j<pway_nested;j++){textvbl[j]=0;}
  for(j=0;j<ngv;j++)
   {
    jmaxn=j*maxn;
    j_textvbl=0;
    ij=igv[j];
    rmvblnks(d.varname[ij],yy); // l=(int)
    strcpy(xyz,d.vartype[ij]);
    igv_vartype[j]=0; str_maxlen=0;
    if(xyz[0]=='S')
     {
      lenofvar=d.varlen[ij];
      igv_vartype[j]=lenofvar;
     }
    strcpy(gtitle+j*128," ");
    strcpy(yypar,yy); strcat(yypar,"TITLE");
    i=(int)spfind(yypar);
    if(i>=0)
     {
      strcpy(gtitle+j*128,spb[i]);
      kk=replace_underscore(y_title+i*41);
      if(html_on>0)kk=replace_aakkos(y_title+i*41);
     }
    else
     {
      strcpy(gtitle+j*128,d.varname[ij]);
      if(html_on>0)kk=replace_aakkos(y_title+i*41);
     }
    i=(int)spfind(yy);
    if(i<0)
     {    
      kk=sprintf(sbuf,
    "Checking the grouping structure of the variable %.8s ...",yy);
      write_string(sbuf,kk,'8',25,1);
       kk=0;  kk_nest=0;
/* starting values for string lengths and vartype    */
      for(i2=0;i2<pway_nested;i2++)
       {
        ij2=igv_nested[j*pway_nested+i2];
        if(ij2<0)break;
        nestaddr=address_nested[j*pway_nested+i2];
        strcpy(xyz,d.vartype[ij2]);
        igv_vartype_nested[nestaddr]=0;
        str_maxlen_nest[i2]=0;
        if(xyz[0]=='S')
         {
          igv_vartype_nested[nestaddr]=d.varlen[ij2];
         }
       }         
      for(iii=d.l1;iii<=d.l2;iii++)
       {
      i2=sprintf(sbuf,"obs. %ld ",iii);
      write_string(sbuf,i2,'8',25,60);
        if(unsuitable(&d,iii))continue;
        if(nested[j]>0)
         {
/*           for a nested class variable A[B[C...]]                    */
/*           the values of "pairs" (A,B,C,...) must be examined        */
          for(i2=0;i2<pway_nested;i2++)
           {
            ij2=igv_nested[j*pway_nested+i2];
            if(ij2<0)continue;
            nestaddr=address_nested[j*pway_nested+i2];
            i2long=i2; i2long=i2long*33;
            jfnd=read_obs(iii,ij2,igv_vartype_nested[nestaddr],
                &nested_xval[i2],&nested_area[i2long]);
            if(jfnd==0)goto nexti;
           }
          for(j2=0;j2<kk_nest;j2++)
           {
            for(i2=0;i2<pway_nested;i2++)
             {
              nestaddr=address_nested[i2+j*pway_nested];
              if(nestaddr<0)break;
              if(igv_vartype_nested[nestaddr]==0)
               {
/*                    real value                                        */
                if(values_nested[nestaddr*nested_maxn+j2] == nested_xval[i2])
                  continue;
                else
                 goto nextlab;
               }
              else
/*                    character value                                   */
               {
                kk9=label_length1;
                kk9=kk9*(j2+nestaddr*nested_maxn);
          ivrt=(int)strncmp(labels_nested+kk9,nested_area+i2*33,label_length);
          if(ivrt==0)continue;
          else
           goto nextlab;
               }
             }            /* end of loop for nesting ways  i2  */
            goto nexti;   /* same nest vble values         */
 nextlab:   ;
           }              /* loop nest values  j2         */
/*      add a new value list for nested vbles             */
          kk_nest++;
          if(kk_nest>nested_maxn)goto errmaxn;
          for(i2=0;i2<pway_nested;i2++)
           {
            nestaddr=address_nested[i2+j*pway_nested];
            if(igv_vartype_nested[nestaddr]==0)
             { kk9=nestaddr; kk9=kk9*nested_maxn+kk_nest-1;
             values_nested[kk9]=nested_xval[i2];
             }
            else
             {
              kk9=label_length1*(kk_nest-1+nested_maxn*nestaddr);
              i2long=i2; i2long=i2long*33;
              strncpy(labels_nested+kk9,nested_area+i2long,label_length);
/*                  check if the string contains a real value           */
              csval=(double)atof(nested_area+i2long);
              if(csval==0.)
               {
                iij=(int)strcmp(nested_area+i2long,"0");
                if(iij != 0)
                 {textvbl[i2]=1;continue;}
               }
              kk9=kk_nest-1+nested_maxn*nestaddr;
              values_nested[kk9]=csval;
/*               check the numbers of nonblank chars    */
              for(i_str=0;i_str<label_length;i_str++)
               {
                i2long=i2; i2long=i2long*33;
                if(nested_area[i2long+label_length-i_str-1] != ' ')goto lstbl;
               }
lstbl:        if(label_length-i_str>str_maxlen_nest[i2])
                 str_maxlen_nest[i2]=label_length-i_str;
              igv_vartype_nested[nestaddr]=str_maxlen_nest[i2];
             }
           } /*  end of loop for i2    */
         }   /* end of nesting part    */
        if(nested[j]==0)
         {
          if(igv_vartype[j]==0)
           {
            ky=data_load(&d,iii,ij,&xval); if (ky<0) return(-1); // RS 4.2.2013 ADD if
            if(xval==MISSING8)continue;
            for(j2=0;j2<kk;j2++)
             {
              if(values[j2+jmaxn]==xval)goto nexti;
             }
            kk++;
            if(kk>maxn)goto errmaxn;
            values[kk+jmaxn-1]=xval;
           }
          else
           {
            jjj=lenofvar;
            if(jjj>label_length)jjj=label_length;
            linspace(xx,label_length1);
            ky=(int)data_alpha_load(&d,iii,ij,&xx[0]); if (ky<0) return(-1); // RS 4.2.2013 ADD if
            l2=strlen(xx);
            xx[l2]=' '; xx[label_length1]='\0';
            for(j2=0;j2<jjj;j2++)
             {
              if(xx[j2] != ' ')goto notblank;
             }
            continue;  /*  blanks are treated as missing values   */
/*         check the number of nonblank chars    */
notblank:   for(i_str=0;i_str<label_length;i_str++)
             {
              if(xx[label_length-i_str-1] != ' ')goto lstblnk;
             }
lstblnk:    if(label_length-i_str>str_maxlen)str_maxlen=label_length-i_str;
            igv_vartype[j]=str_maxlen;
            for(j2=0;j2<kk;j2++)
             {
              kk9=label_length1*(j2+jmaxn);
              ivrt=(int)strncmp(labls+kk9,xx,label_length);
              if(ivrt==0)goto nexti;
             }
            kk++;
            if(kk>maxn)goto errmaxn;
            kk9=label_length1*(kk-1+jmaxn);
            strncpy(labls+kk9,xx,label_length);
/*  check if the string contains a real value   */
            csval=(double)atof(xx);
            if(csval==0.)
             {
              iij=(int)strcmp(xx,"0");
              if(iij != 0)
               {
                j_textvbl=1; goto nexti;
               }
             }
            values[kk-1+jmaxn]=csval;
           }   /*  vartype     */
         }     /*  non nesting */
   nexti: ;
       }       /*  end of loop for obs       */
     if(nested[j]==0)
       {
        if(igv_vartype[j]==0 || j_textvbl==0)
         {
          valsort2(&values[jmaxn],&labls[label_length1*jmaxn],kk);
         }
        else
         {
          chr_sort(&labls[label_length1*jmaxn],kk);
         }
        if(igv_vartype[j]>0)
         {
          if(j_textvbl==0)igv_vartype[j]=-igv_vartype[j];
         }
       }
      else
       {
        m=0;
        for(i2=0;i2<pway_nested;i2++)
         {
          iaddr=address_nested[i2+j*pway_nested];
          if(iaddr<0)break;
          m++;
          if(igv_vartype_nested[iaddr]>0 && textvbl[i2]==0)
             igv_vartype_nested[iaddr]=-igv_vartype_nested[iaddr];
         }
        j2=address_nested[j*pway_nested];
        iij=nested_maxn*j2; jjj=iij*label_length1;
        charsort(&labels_nested[jjj],&values_nested[iij],
                 &igv_vartype_nested[j2],m,kk_nest,nested_maxn,
                 &nd_nested[j2],&change_points[iij]);
       }
     }
    if(i>=0)
     {      
      k=(int)split(spb[i],nambuff,maxn);       
      kk=(int)grpvals(jmaxn,k,d.varname[ij],igv_vartype[j]);      
      if(kk<0)goto errwait;
      if(igv_vartype[j]>0)
       {
        if(real_vals==1)igv_vartype[j]=-igv_vartype[j];
       }
     }
    if(nested[j]==0)
     nd[j]=kk;
    else
     nd[j]=kk_nest;
    if(nested[j]==0)
     {
      if(igv_vartype[j]<=0)
       {
        isort=1;
        for(j2=1;j2<kk;j2++)
         {
          if(values[j2-1+jmaxn]>=values[j2+jmaxn])
           { isort=0; break; }
         }
        cutp[j]=isort;
        max_gvals[j]=values[jmaxn];
        for(j2=1;j2<kk;j2++)
         {
          if(values[j2+jmaxn]>max_gvals[j])max_gvals[j]=values[j2+jmaxn];
         }
       }
     }    /* if nested=0    */
    else
     {
      for(i=0;i<pway_nested;i++)
       {
        nestaddr=i+pway_nested*j;
        ij=address_nested[j*pway_nested+i];
        if(ij<0)break;
        if(igv_vartype_nested[ij]<=0)
         {
          isort=1; iij=ij*nested_maxn;
          for(j2=1;j2<kk_nest;j2++)
           {
   if(values_nested[j2-1+iij]>=values_nested[j2+iij])
             { isort=0; break; }
           }
          cutp_nested[ij]=isort;
          max_gvals_nest[ij]=values_nested[ij*nested_maxn];
          for(j2=1;j2<kk_nest;j2++)
           {
   if(values_nested[j2+iij]>max_gvals_nest[ij])
          max_gvals_nest[ij]=values_nested[j2+iij];
           }
         } /*  igv_vartype_nested  */
       }   /*  end of loop for i   */
     }     /*  nested              */
   }       /*  end of loop for j   */
/*                                                    */
    return(1);
errmaxn: i=sprintf(sbuf,"Variable %.8s has too many groups",d.varname[ij]);
         write_string(sbuf,i,'4',10,1);
errwait: sur_getch(); return(-1);
   }
/*                                               */
// RS REM extern char *labls;
// RS REM extern int real_vals;
static int grpvals(int jmaxn,int nvl,char *vname,int vtype)
  {
/*       vtype=0 for numeric and 1 for alphameric values     */
   char *cc1[3],*k1,*k2;
   double cs,cstep,cmax;
   int kk,k,l,iij,kkj,kkj9,nwrds; // i,nm,
/*                                           */
      kk=-1; real_vals=1;
      for(k=0;k<nvl;k++)
       {        
//        nm=(int)strlen(nambuff[k]);
        k1=(char *)strchr(nambuff[k],'(');
        k2=(char *)strchr(nambuff[k],')');
        if(k1 != NULL)*k1=',';
        if(k2 != NULL)*k2=',';
        if(k1 != NULL || k2 != NULL)
         {
          if(k1==NULL || k2==NULL)
           { grouperror(nambuff[k]); return(-1);}
         }
        nwrds=(int)split(nambuff[k],cc1,3);
        cs=(double)atof(cc1[0]);
        if(cs==0.)
         { iij=(int)strcmp(cc1[0],"0");
           if(iij != 0)real_vals=0;
           if(vtype==0 && iij != 0)
            { grouperror(cc1[0]);return(-1);}
         }
        if(nwrds>1 && real_vals==0)
           { grouperror(nambuff[k]);return(-1);}
        if(nwrds<3)
         {
          kk++; kkj=kk+jmaxn; kkj9=kkj*label_length1;
          if(vtype==0 || real_vals==1)
           {
            values[kkj]=cs;
            if(nwrds==2)
             {
              l=(int)strlen(cc1[1]); if(l>label_length)l=label_length;
              strncpy(labls+kkj9,cc1[1],l);
             }
           }
          if(vtype != 0)
           {
            if(real_vals==0)
             {
              l=(int)strlen(cc1[0]); if(l>label_length)l=label_length;
              strncpy(labls+kkj9,cc1[0],l);
             }
           }
         }
        if(nwrds==3)
         {
          cstep=(double)atof(cc1[1]);
          if(cstep==0.){grouperror(cc1[1]); return(-1); }
          cmax=(double)atof(cc1[2]);
          if(cmax==0.)
           {
            if((int)strcmp(cc1[2],"0") != 0)
             { grouperror(cc1[2]); return(-1); }
           }
          while (cs<=cmax)
           {
            kk++;
            if(kk>maxn)
             {
              sprintf(sbuf,"\n Variable %.8s has too many groups",vname);
              return(-1);
             }
            kkj=kk+jmaxn; values[kkj]=cs;
            cs+=cstep;
           }
         }
       }     /*  end of loop for k */
      return(kk+1);
     }
static int grouperror(char *cc)
     {
      sprintf(sbuf,"\nError in group specifications: %.8s",cc);
      return(-1);
     }

/* meansums.c  */
/*                                                                 */
/*  collecting cell numbers, sums and crossproducts                */
/*                                                                 */
static int meansums(void)
 {
  double xval,yval;
  long iii;
  int i,j,ij,i1,i2,ij2,k=0,m,nestaddr;
  int ndj,iosot,iyj,jmaxn,jfnd,indpos,len_alphaval,l2;
/*                                                              */
  nbas=ntulo;
  if(ivariables>0 && ny>0)
   {
    pspace_xvrt=(char *)muste_malloc(ny*sizeof(double));
    if(pspace_xvrt==NULL)return(-1);
    xvrt=(double *)pspace_xvrt;
    for(i=0;i<ny;i++)
     {
      if(ivbles_type[i]==0)
       {
        xvrt[i]=0.0000005;
        if(ivbles_imin[i] != 0.0)
         {
          xvrt[i]=ivbles_imin[i]*0.0000005;
          xvrt[i]=fabs(xvrt[i]);
         }
       }
     }
   }
  if(n_of_nests>0)
   {
    comp_nest_xvert(&xvert[0]);
   }
  if(pway>0)
   {
    comp_xval_xvert(&gxvert[0]);
   }
/*                loop over observations    */
    for(iii=d.l1;iii<=d.l2;iii++)
     {
      i=unsuitable(&d,iii);
      if(i==1)continue;
      j=sprintf(sbuf," %ld observations processed ",iii);
      write_string(sbuf,j,'8',25,1);
      iosot=0;
      if(pway>0)
       {
        for(j=0;j<pway;j++)
         {         
          jmaxn=j*maxn;
          ij=igv[j]; ndj=nd[j];
          if(nested[j]>0)
           {
/*                   loop over nested class variables                 */
            for(i2=0;i2<pway_nested;i2++)
             {
              nestaddr=address_nested[j*pway_nested+i2];
              if(nestaddr<0)break;
              ij2=igv_nested[j*pway_nested+i2];
              if(ij2<0)break;
                jfnd=(int)read_obs(iii,ij2,igv_vartype_nested[nestaddr],
                &nested_xval[i2],&nested_area[i2*33]);
              if(jfnd==0){ goto rjctd; }
             }
            k=(int)comp_xval_nested(j,nested_xval,&nested_area[0]);
           }
/*                    normal crossed class variables                  */
          if(nested[j]==0)
           {
            strcpy(xx," ");
            jfnd=(int)read_obs(iii,ij,igv_vartype[j],&xval,&xx[0]);
            if(jfnd==0){ goto rjctd;}
            k=(int)comp_xval(j,xval,ndj,jmaxn);
           }
          iosot+=k*ilis[j];
         }
       }
      if(ny>0)
       {
        for(m=0;m<ny;m++)
         {
          iyj=iy[m];
          if(ivariables==0)
           {
            jfnd=data_load(&d,iii,iyj,&yval);
            if(yval==MISSING8)continue;
            i1=m+iosot;
           }
          else
           {
            indpos=0;
            if(ivbles_type[m]==0)
             {
              jfnd=data_load(&d,iii,iyj,&yval);
              if(yval==MISSING8)continue;
        if(yval>=ivbles_imin[m]-xvrt[m] && yval<=ivbles_imax[m]+xvrt[m])
               indpos=1;
             }
            else
             {
              linspace(xx,label_length1);
              jfnd=data_alpha_load(&d,iii,iyj,&xx[0]);
              len_alphaval=d.varlen[iyj];
              if(len_alphaval>label_length)len_alphaval=label_length;
              l2=strlen(xx); xx[l2]=' '; xx[label_length]='\0';
              i=(int)muste_strcmpi(xx," ");
              if(i==0)continue;
              ij2=m*label_length1;
              i=(int)strncmp(ivbles_ilabs+ij2,xx,label_length);
              if(i==0)indpos=1;
             }
            i1=2*m + iosot;
            if(indpos>0)lkm[i1+1]++;
           }
          lkm[i1]++;
          total_lkm[m]++;
          if(ivariables==1)continue;
          sz[i1]+=yval; total_sums[m]+=yval;
          szy[i1]+=yval*yval; total_ss[m]+=yval;
          if(min_yvals[i1]>max_yvals[i1])
            {min_yvals[i1]=yval;
             max_yvals[i1]=yval;
            }
          if(min_yvals[i1]>yval)min_yvals[i1]=yval;
          if(max_yvals[i1]<yval)max_yvals[i1]=yval;
          if(total_maxy[m]<total_miny[m])
           {total_maxy[m]=yval;
            total_miny[m]=yval;
           }
          if(total_maxy[m]<yval)total_maxy[m]=yval;
          if(total_miny[m]>yval)total_miny[m]=yval;
         }   /*  end of loop for object variables */
       }
      if(ny==0)
        {lkm[iosot]++;total_lkm[0]++;}
      rjctd: ;
     }     /*  end of loop for observations        */
    return(0);
   }
/*                                                                 */
/*  read one observation                                           */
/*                                                                 */
/*   function returns 0 if the xval/chrval is MISSING              */
/*   else 1 is returned                                            */
/*   parameters:                                                   */
/*   iii   # of observation   vartype  type of variable            */
/*   ij    # of variable      xval     output value if real        */
/*                            chrval   output value if string      */
static int read_obs(long iii,int ij,int vartype,double *xval,char *chrval)
 {
  int i,l2,jfnd,len_alphaval;
/*                                                              */
  if(vartype<=0)
    {
     if(vartype<0)
       {
        linspace(chrval,label_length1);
        jfnd=data_alpha_load(&d,iii,ij,&chrval[0]); if (jfnd<0) return(-1); // RS 4.2.2013 if
        *xval=(double)atof(chrval);
       }
      else
       {
        jfnd=data_load(&d,iii,ij,xval); if (jfnd<0) return(-1); // RS 4.2.2013 if
       }
      if(*xval==MISSING8)return(0);
    }
  else
    {
     linspace(chrval,label_length1);
     jfnd=data_alpha_load(&d,iii,ij,&chrval[0]); if (jfnd<0) return(-1); // RS 4.2.2013 if
     len_alphaval=d.varlen[ij];
     if(len_alphaval>label_length)len_alphaval=label_length;
     l2=strlen(chrval);
     chrval[l2]=' '; chrval[label_length1]='\0';
     i=(int)muste_strcmpi(chrval," ");
     if(i==0)return(0);
    }
   return(1);
  }

 /*  chr_sort */
static void chr_sort(char *chrarr,int n)
 {
/* sort character array chrarr                          */
  int i,j,icomp;
  int ik,jk;
  char ccc[36];
  for(i=0;i<n-1;i++)
   {
    ik=label_length1*i;
    for(j=i+1;j<n;j++)
     {
      jk=label_length1*j;
      icomp=strncmp(chrarr+jk,chrarr+ik,label_length);
      if(icomp>=0)goto notchng;
      if(icomp<0)
       {
        strncpy(ccc,chrarr+jk,label_length);
        strncpy(chrarr+jk,chrarr+ik,label_length);
        strncpy(chrarr+ik,ccc,label_length);
       }
notchng: ;
     }
   }
  return;
 }
static void charsort(char *chrarr,double *xarr,int *types,int m,int n,int lngth_array,int *ndtop,int *change_point)
 {
/* sort arrays chrarr/xarr according                          */
  int i,j,k,kln,icomp,ichange;
  int ik,jk,kk,i1,i2;
  char ccc[36];
  double xapu;
  kk=label_length1*lngth_array;
  for(i=0;i<n-1;i++)
   {
    ik=label_length1*i;
    for(j=i+1;j<n;j++)
     {
      jk=label_length1*j;
      ichange=0;
      for(k=m-1;k>=0;k--)
       {
        i1=kk*k+jk; i2=kk*k+ik;
        if(types[k]>0)
         {
          icomp=strncmp(chrarr+i1,chrarr+i2,label_length);
          if(icomp>0)goto notchng;
          if(icomp<0)
           { ichange=1; goto chnglab; }
         }
        else
         {
          kln=k*lngth_array;
          if(xarr[j+kln]<xarr[i+kln])
           { ichange=1; goto chnglab; }
          if(xarr[j+kln]>xarr[i+kln])
           { goto notchng; }
         }
       }
chnglab: if(ichange==1)
       {
        for(k=m-1;k>=0;k--)
         {
          kln=k*lngth_array;
          if(types[k]<=0)
           { xapu=xarr[j+kln];
             xarr[j+kln]=xarr[i+kln];
             xarr[i+kln]=xapu;
           }
          i1=kk*k+jk; i2=kk*k+ik;
          strncpy(ccc,chrarr+i1,label_length);
          strncpy(chrarr+i1,chrarr+i2,label_length);
          strncpy(chrarr+i2,ccc,label_length);
         }
       }
notchng: ;
     }
   }
/*  compute number of distinct values in the topmost classif variable  */
  if(m<2)return;
  j=0; k=m-1; kln=k*lngth_array; xapu=-99999.99;
  strcpy(ccc," ");
  for(i=0;i<n;i++)
   {
    if(types[k]<=0)
      {
       if(xapu != xarr[i+kln])
        {
         xapu=xarr[i+kln]; j++;
         if(j>1)change_point[j-2]=i;
       }
      }
     else
      {
       i2=label_length1*(kln+i);
       icomp=strncmp(ccc,chrarr+i2,label_length);
       if(icomp !=0 )
        {
         strncpy(ccc,chrarr+i2,label_length); j++;
         if(j>1)change_point[j-2]=i;
        }
      }
   }
  if(j>1)change_point[j-1]=n;
  *ndtop=j;
  return;
 }

/* ivbldefs.c    */
static int ivbldefs(void)
 {
  int i,j,l,ij; // ,j_textvbl,i_strlen; // k
  int nvl,lenofvar,real_val,iij;
  double cs,cs2;
  char xyz[24],ccapu[36];
/*                                           */
  for(j=0;j<ny;j++)
   {
    ij=iy[j];
//    j_textvbl=0;
    l=(int)rmvblnks(d.varname[ij],yy);
    strcpy(xyz,d.vartype[ij]);
    ivbles_type[j]=0; // i_strlen=0;
    if(xyz[0]=='S')
     {
      lenofvar=d.varlen[ij];
      ivbles_type[j]=-lenofvar;
     }
/* check if the conditions of true value is given   */
/* in the form <vble>=<min>,<max> or <strvalue>     */
    i=(int)spfind(yy);
    if(i<0)
     {
      if(ivbles_type[j]==0)
       {
        ivbles_imin[j]=1.0; ivbles_imax[j]=1.0;
       }
      else
       {
write_string("*Err011: String indicator variable definition missing ",
            54,mess_char,11,1); sur_getch();
        return(-1);
       }
     }
    if(i>=0)
     {
      nvl=(int)split(spb[i],nambuff,maxp);
      real_val=1;
      cs=(double)atof(nambuff[0]);
      if(cs==0.)
       {
        iij=(int)strcmp(nambuff[0],"0");
        if(iij != 0)real_val=0;
        if(ivbles_type[j]==0 && iij != 0)goto error;
       }
      if(real_val==1)
       {
        ivbles_imin[j]=cs;
        ivbles_imax[j]=cs;
        if(nvl>1)
         {
          cs2=(double)atof(nambuff[1]);
          if(cs2==0.)
           {
            iij=(int)strcmp(nambuff[1],"0");
            if(iij != 0)goto error;
           }
          ivbles_imax[j]=cs2;
         }
       }
      if(real_val==0)
       {
        linspace(ccapu,33);
        l=strlen(nambuff[0]);
        if(l>label_length)l=label_length;
        strncpy(ccapu,nambuff[0],l); ccapu[l]=' ';
        ccapu[label_length]='\0';
        strncpy(ivbles_ilabs+j*label_length1,ccapu,label_length);
       }
     }
   }
  return(1);
error: sprintf(sbuf,"\nError in specification for the variable %.8s",yy);
  return(-1);
 }

/* optintpr.c  */
static int optintpr(void)
  {
   int nr,iopt;
   int i,j,jj,minc,nogiven; // l,
/*                                       */
   for(i=0;i<23;i++){prntopt[i]=-1;}
   jj=-1; nototals=-1; rowtotals=-1; coltotals=-1;
    for(i=0;i<24;i++){statistics[i]=0;}
    iopt=a_spfind("OPTIONS");
    if(iopt>=0)
     {
      nr=(int)split(spb[iopt],nambuff,25);
      for(j=0; j<nr; j++)
       {
        /* l=(int)strlen(nambuff[j]); */ minc=4;
        p=(char *)strncpy(x2,nambuff[j],minc);
        x2[minc]='\0';
        i=(int)muste_strcmpi(x2,"MEAN");
        if(i==0){ prntopt[0]=1;jj++;statistics[jj]=1;continue;}
        i=(int)muste_strcmpi(x2,"SUMS");
        if(i==0){ prntopt[1]=1;jj++;statistics[jj]=2;continue;}
        i=(int)muste_strcmpi(x2,"DEVS");
        if(i==0){ prntopt[2]=1;jj++;statistics[jj]=3;continue;}
        i=(int)muste_strcmpi(x2,"SEM");
        if(i==0){ prntopt[6]=1;jj++;statistics[jj]=7;continue;}
        i=(int)muste_strcmpi(x2,"MIN");
        if(i==0){ prntopt[4]=1;jj++; statistics[jj]=5; continue;}
        i=(int)muste_strcmpi(x2,"MAX");
        if(i==0){ prntopt[5]=1;jj++; statistics[jj]=6; continue;}
        i=(int)muste_strcmpi(x2,"NOTO");
        if(i==0){ nototals=1;}
        i=(int)muste_strcmpi(x2,"F");
        if(i != 0)i=(int)muste_strcmpi(x2,"N");
        if(i==0){ prntopt[3]=1;jj++;statistics[jj]=4;continue;}
        i=(int)muste_strcmpi(x2,"ROWT");
        if(i==0){ coltotals=1;}
        i=(int)muste_strcmpi(x2,"COLT");
        if(i==0){ rowtotals=1;}
        i=(int)muste_strcmpi(x2,"%F");
        if(i==0) { prntopt[7]=1; jj++; statistics[jj]=8; continue;}
        i=(int)muste_strcmpi(x2,"%S");
        if(i==0) { prntopt[8]=1; jj++; statistics[jj]=9; continue;}
        i=(int)muste_strcmpi(x2,"C%F");
        if(i != 0)i=(int)muste_strcmpi(x2,"C%");
        if(i==0) { prntopt[9]=1; jj++; statistics[jj]=10; continue;}
        i=(int)muste_strcmpi(x2,"C%S");
        if(i==0) { prntopt[10]=1; jj++; statistics[jj]=11; continue;}
        i=(int)muste_strcmpi(x2,"R%F");
        if(i != 0)i=(int)muste_strcmpi(x2,"R%");
        if(i==0) { prntopt[11]=1; jj++; statistics[jj]=12; continue;}
        i=(int)muste_strcmpi(x2,"R%S");
        if(i==0) { prntopt[12]=1; jj++; statistics[jj]=13; continue;}
        i=(int)muste_strcmpi(x2,"T%F");
        if(i==0) { prntopt[13]=1; jj++; statistics[jj]=14; continue;}
        i=(int)muste_strcmpi(x2,"T%S");
        if(i==0) { prntopt[14]=1; jj++; statistics[jj]=15; continue;}
        i=(int)muste_strcmpi(x2,"CCF");
        if(i==0) { prntopt[15]=1; jj++; statistics[jj]=16; continue;}
        i=(int)muste_strcmpi(x2,"CRF");
        if(i==0) { prntopt[16]=1; jj++; statistics[jj]=17; continue;}
        i=(int)muste_strcmpi(x2,"CCS");
        if(i==0) { prntopt[17]=1; jj++; statistics[jj]=18; continue;}
        i=(int)muste_strcmpi(x2,"CRS");
        if(i==0) { prntopt[18]=1; jj++; statistics[jj]=19; continue;}
        i=(int)muste_strcmpi(x2,"CR%F");
        if(i==0) { prntopt[19]=1; jj++; statistics[jj]=20; continue;}
        i=(int)muste_strcmpi(x2,"CC%F");
        if(i==0) { prntopt[20]=1; jj++; statistics[jj]=21; continue;}
        i=(int)muste_strcmpi(x2,"CR%S");
        if(i==0) { prntopt[21]=1; jj++; statistics[jj]=22; continue;}
        i=(int)muste_strcmpi(x2,"CC%S");
        if(i==0) { prntopt[22]=1; jj++; statistics[jj]=23; continue;}
       }          /* end of loop for j    */
     }
    nogiven=1;
    for(i=0;i<23;i++)
     {
      if(prntopt[i]>=0)nogiven=0;
     }
    if(nogiven==1)
     {
      for(i=0;i<23;i++)
       {
        prntopt[i]=0;
       }
      if(ny>0 && ivariables==0)
       {
        prntopt[0]=1;
        prntopt[2]=1;
        statistics[0]=1; statistics[1]=3; statistics[2]=4;
       }
      else
       {
        prntopt[3]=1;
        statistics[0]=4;
       }
     }
    else
     {
      for(i=0;i<23;i++)
       {
        if(prntopt[i]<0)prntopt[i]=0;
       }
     }
    if(rowtotals<0 && coltotals<0 && nototals<0)
     {
      rowtotals=1; coltotals=1; nototals=0;
     }
    else
     {
      if(rowtotals<0)rowtotals=0;
      if(coltotals<0)coltotals=0;
      if(nototals<0)nototals=0;
     }
    if(nototals==1){rowtotals=0; coltotals=0;}
    return(1);
   }

 /* listcomp.c  */
/*                                                                 */
/*     Interpreter for ROW and COL definitions of MEANS operation  */
/*     Markku Korhonen                         23.8.1989           */
/*                                                              */
static int list_comp(int *result_address,char *source,int *vble_type)
 {
  int ij,i2,l,m,l_source,l_oper,fchar,lchar,i,j,i1,k;
  int index1,index2;
  int ioper;
  char wrd[12];
/*                                                    */
  index1=index2=0; // RS 7.2.2013
  l_source=strlen(source);
  for(i=l_source-1;i>=0;i--)
   {
    if(source[i] != ' ')break;
   }
  l_source=i+1;
  l_oper=-1;
  fchar=0;
/*                                                    */
nextw:  i=get_next_word(wrd,source,fchar,&lchar,l_source);
  if(i<=0)goto nomore;
  ioper=0;
  i1=(int)muste_strcmpi(wrd,"*");
  if(i1==0){ioper=1;goto next;}
  i1=(int)muste_strcmpi(wrd,",");
  if(i1==0){ioper=2;goto next;}
  i1=(int)muste_strcmpi(wrd,"(");
  if(i1==0){ioper=3;goto next;}
  i1=(int)muste_strcmpi(wrd,")");
  if(i1==0)
   {ioper=4;}
/*   if the word was not an operation then it is       */
/*   either a grouping or object variable              */
next:  if(ioper==0)
   {
    *vble_type=1;
    m=strlen(wrd);
    ij=ny+ngv; i1=0;
    for(j=0;j<ij;j++)
     {
      if(j<ny)
       k=iy[j];
      else
       k=igv[j-ny];
      l=(int)rmvblnks(d.varname[k],xx);
      if(l<m){continue;}
      i2=(int)strncmp(wrd,xx,m);
      if(i2==0){i1++;index1=j; index2=k;}
     }
    if(i1==1)
     {
      k=index2;
      if(index1>=ny){igv_used[index1-ny]=1; goto labg;}
      goto laby;
     }
    if(i1 > 1)
     {
      sprintf(sbuf,"\nAbbreviation %.8s is not unique",wrd);
     }
    if(i1 == 0)
     {
    sprintf(sbuf,
"\nVariable %.8s should be defined either in VARIABLES or GROUPING/CLASS",
    wrd);
     }
    goto errwait;
laby: *vble_type=2;
labg: ;
   }
  l_oper++;
  oper_list[l_oper]=ioper;
  if(ioper==0)
   {
    vble_seqs[l_oper]=k;
    oper_list[l_oper]=*vble_type*10;
   }
  fchar=lchar+1;
  goto nextw;
/*      source string handled             */
nomore: l_oper++;
        i=perform_operlist(result_address,&oper_list[0],
          &vble_seqs[0],l_oper);
  if(i>=0)return(1);
errwait: return(-1);
 }
/*  perfopl.c  */
// #define MAXWAYS 10
static int perform_operlist(int *address,int *op_list,int *vbl_seqs,int l_oper)
 {
/*     buffer[address]  contains the address of results    */
  int  i,j,k;
  int last_left,right_par; // oper_left,
/*                                                         */
/*                                                           */
/*   (A,B)*(C,D):  (A,B) to Buffer[0]  (C,D) to Buffer[1]    */
/*                 Buffer[0]*Buffer[1] to Buffer[2]          */
//  oper_left=l_oper;
nxtphase: last_left=-1;
  for(i=0;i<l_oper;i++)
   {
    if(op_list[i]==3)last_left=i;
    if(op_list[i]==4)goto labright;
   }
/*   no parenthesis                        */
     j=l_oper-1;
     i=eval_opers(op_list,0,j,vbl_seqs);
     if(i<0)return(-1);
     goto alldone;
/*   parenthesis from last_left to i        */
labright:   right_par=i;
/*   evaluate operations in that interval   */
     i=eval_opers(op_list,last_left+1,i-1,vbl_seqs);
/*   mark operations in that interval done  */
/*   and delete parenthesis                 */
     k=last_left;
     op_list[k]=op_list[k+1];
     vbl_seqs[k]=vbl_seqs[k+1];
     op_list[k+1]=-1; vbl_seqs[k+1]=-1;
     for(j=right_par+1;j<l_oper;j++)
      {
       k++;
       op_list[k]=op_list[j];
       vbl_seqs[k]=vbl_seqs[j];
      }
     l_oper=k+1;
     if(l_oper==1)goto alldone;
     goto nxtphase;
/*                                         */
  alldone: *address=op_list[0]-50;
        return(1);
 }

/* evaloprs.c    */
/*                                                           */
/*   perform operations in interval (i-from,i_to)            */
/*   (no parenthesis exists in that interval)                */
/*                                                           */
static int eval_opers(int *op_list,int i_from,int i_to,int *vbl_seqs)
 {
  int i,j,ij,ii;
  int lasti,buff_addr,buff_addr2,buff_addr3,j2;
  int operseq,i2,jj2,ifrom,ito,operation;
/*                                                          */
/*   make operations in interval (i-from,i_to) in op_list   */
/*                                                          */
  ifrom=i_from; ito=i_to;
  i=ifrom;
/*   first evaluate all cross operations in that interval   */
/*   ====================================================   */
subint:
  for(ij=i;ij<=ito;ij++)
    {
     if(op_list[ij]==2)goto labcomma;   /* comma found   */
    }
  lasti=ito;
  goto secondp;
/*                                                                */
labcomma: lasti=ij-1;
/*   subinterval is (i,lasti), it contains only cross operations  */
/*                                                                */
secondp: if(i>ito)goto subready;
/*   convert first operand into buffer                            */
    j=op_list[i];
    if(j==-1){i++;goto secondp;}  /* -1 marks a deleted element   */
    ii=(int)buffcheck(&buff_addr,op_list,vbl_seqs,i);
    if(ii<0)return(-1);
    op_list[i]=50+buff_addr;
    if(i==lasti)goto subready;
/*                              */
/*    find operation mark       */
/*                              */
    i2=i;
nexti: i2++; if(i2>lasti)goto subready;
    j2=op_list[i2];
    if(j2==-1)goto nexti;
    operation=j2; operseq=i2;
/*                                                                  */
/*  second operand in operation                                     */
/*                                                                  */
nextii:i2++; if(i2>lasti)goto subready;
    j2=op_list[i2]; if(j2==-1)goto nextii;
    ii=(int)buffcheck(&buff_addr2,op_list,vbl_seqs,i2);
    if(ii<0)return(-1);
    op_list[i2]=50+buff_addr2;
/*                                                   */
    combine_buffers(operation,buff_addr,buff_addr2,&buff_addr3);
    op_list[i]=50+buff_addr3;
    op_list[operseq]=-1;
    op_list[i2]=-1;
    if(i2>=lasti)goto subready;
    buff_addr=buff_addr3;
    goto nexti;
/*   subinterval ready                                           */
subready: i=lasti;
subready2: i++;
    if(i>ito)goto scndphase;
    if(op_list[i]==2)goto subready2;
    if(i<=ito)goto subint;  /* next subinterval */
/*                                                               */
/*   next all comma operations are performed for the buffers     */
/*   =======================================================     */
scndphase: ito=i_to;
      i=i_from;
scnd: if(i>ito)goto readylab;
/*   first operand                                              */
    j=op_list[i];
    if(j==-1){i++;goto scnd;}   /* -1 marks a deleted element   */
    buff_addr=j-50;
    i2=i;
nextj: i++; if(i>ito)goto readylab;
    j2=op_list[i];if(j2==-1)goto nextj;
    operseq=i;
    if(j2 != 2){sprintf(sbuf,"\nLogical error in program");return(-1);}
nextjj: i++;
    jj2=op_list[i];if(jj2==-1)goto nextjj;
    buff_addr2=jj2-50;
    combine_buffers(j2,buff_addr,buff_addr2,&buff_addr3);
    op_list[i2]=50+buff_addr3;
    op_list[operseq]=-1;
    op_list[i]=-1;
    i=i2;
    buff_addr=buff_addr3;
    goto nextj;
/*                                                               */
/*  now the interval should contain one reference to one buffer  */
/*  other words are filled with -1                               */
readylab: return(1);
  }
/*   get a new buffer                                         */
     int new_buffer(lngth,nofways,vbltype)
     int lngth,nofways,vbltype;
      {
       int buftot,*dims;
       char *p_wrk;
/*   buffer contents:                                         */
/*       1. word   n of ways                                  */
/*       2. word   lngth                                      */
/*       3. word   vbltype                                    */
/*       object vbles, array length=lngth                     */
/*       grouping vble names, array length= nofways*lngth     */
/*       group symbols, array length=nofways*lngth            */
       buftot=3+(2*nofways+1)*lngth;
       buffer[next_buffer]=(char *)muste_malloc(buftot*sizeof(int));
       if(buffer[next_buffer]==NULL)goto notenough;
       p_wrk=(char *)buffer[next_buffer];
       dims=(int *)p_wrk;
       dims[0]=nofways;
       dims[1]=lngth;
       dims[2]=vbltype;
       return(1);
notenough: sprintf(sbuf,"\nNot enough memory!");return(-1);
      }
    int buffcheck(buff_addr,op_list,vbl_seqno,i)
    int *buff_addr,*vbl_seqno,i,*op_list;
     {
      int j,vbltype,ii;
      j=op_list[i];
      if(j==10 || j==20)
       {
        vbltype=j/10;
        ii=put_into_buffer(buff_addr,vbltype,vbl_seqno[i]);
        if(ii<0)return(-1);
        op_list[i]=50+*buff_addr;
       }
      if(j>=50)*buff_addr=j-50;
      return(1);
     }
/*     put into buffer                                       */
       char * p_buff;
       int *objvbls,*gnames,*gsymbols;
       int put_into_buffer(address,vbltype,vblseqno)
       int vbltype,vblseqno,*address;
        {
         int i,length,nofways,igvno=0,kpoint,nestaddr=0;
         length=1;
         nofways=0;
         if(vbltype==1)
          {
           for(i=0;i<ngv;i++)
            {
             if(igv[i]==vblseqno)break;
            }
           igvno=i;
           length=nd[i];
           if(totalclass==1)
            {
             length++;
             if(nested[i]>0)length+=nd_nested[i];
            }
           nofways=1;
          }
         i=new_buffer(length,nofways,vbltype);
        *address=next_buffer;
         p_buff=(char *)buffer[next_buffer];
         p_buff=p_buff+3*sizeof(int);
         if(vbltype==2)
          {
           objvbls=(int *)p_buff;
           objvbls[0]=vblseqno;
          }
         p_buff=p_buff+length*sizeof(int);
         if(vbltype==1)
          {
           gnames=(int *)p_buff;
           p_buff=p_buff+length*nofways*sizeof(int);
           gsymbols=(int *)p_buff;
           kpoint=0;
           if(nested[igvno]>0)nestaddr=address_nested[igvno*pway_nested];
           for(i=0;i<length;i++)
            {
             gnames[i]=vblseqno;
             gsymbols[i]=i;
             if(totalclass==1)
              {
               if(nested[igvno]>0)
                {
                 if(i==change_points[nestaddr*nested_maxn+kpoint]+kpoint)
                  {
                   gsymbols[i]=10000+kpoint; kpoint++;
                  }
                 else
                  {
                   gsymbols[i]=i-kpoint;
                  }
                 }
               if(i==length-1)gsymbols[i]=9999;
              }
            }
          }
         next_buffer++;
         return(1);
        }
/*         combine two buffers into a third one            */
/*         ====================================            */
static int combine_buffers(int operation,int buff1,int buff2,int *resultbuff)
   {
/*         operation = 1    cross  buff1*buff2             */
/*                     2    put in sequence buff1,buff2    */
    int i,j,i1,ii,jj,jj2;
    int nway1,nway2,lngth1,lngth2,vbltyp1,vbltyp2,*objvbl1,*objvbl2;
    int *gnames1,*gnames2,*gsymbs1,*gsymbs2;
    int nway3,lngth3,vbltyp3,*gnames3,*gsymbs3,*objvbl3;
/*   get dimensions and vbltypes of the two buffers                */
    p_bf1=buffer[buff1]; gsymbs2=NULL; gnames2=NULL; objvbl2=NULL; gsymbs1=NULL; gnames1=NULL; objvbl1=NULL;
    nway1=*(int *)p_bf1;p_bf1+=sizeof(int);
    lngth1=*(int *)p_bf1;p_bf1+=sizeof(int);
    vbltyp1=*(int *)p_bf1;p_bf1+=sizeof(int);
    if(vbltyp1>=2)
     {
      objvbl1=(int *)p_bf1;
     }
    p_bf1+=lngth1*sizeof(int);
    if(vbltyp1 == 1 || vbltyp1==3)
     {
      gnames1=(int *)p_bf1; p_bf1+=lngth1*nway1*sizeof(int);
      gsymbs1=(int *)p_bf1;
     }
    p_bf2=buffer[buff2];
    nway2=*(int *)p_bf2;p_bf2+=sizeof(int);
    lngth2=*(int *)p_bf2;p_bf2+=sizeof(int);
    vbltyp2=*(int *)p_bf2;p_bf2+=sizeof(int);
    if(operation==1 && vbltyp1>1 && vbltyp2>1)
     {  sprintf(sbuf,"\nTwo object variables cannot be crossed");
        return(-1);
     }
    if(vbltyp2>=2)
     {
      objvbl2=(int *)p_bf2;
     }
    p_bf2+=lngth2*sizeof(int);
    if(vbltyp2==1 || vbltyp2==3)
     {
      gnames2=(int *)p_bf2; p_bf2+=lngth2*nway2*sizeof(int);
      gsymbs2=(int *)p_bf2;
     }
/*    compute dimensions for the resulting buffer         */
    if(operation==1)
     {
      nway3=nway1+nway2;
      lngth3=lngth1*lngth2;
     }
    else
     {
      nway3=nway1; if(nway2>nway3)nway3=nway2;
      lngth3=lngth1+lngth2;
     }
    if(vbltyp1 == vbltyp2)vbltyp3=vbltyp1;
    if(vbltyp1 != vbltyp2)vbltyp3=3;
    i=(int)new_buffer(lngth3,nway3,vbltyp3);
    if(i<0)return(-1);
    *resultbuff=next_buffer;
    p_bf3=buffer[next_buffer];
    p_bf3+=3*sizeof(int);
    next_buffer++;
    objvbl3=(int *)p_bf3;
    p_bf3+=lngth3*sizeof(int);
    gnames3=(int *)p_bf3;
    p_bf3+=lngth3*nway3*sizeof(int);
    gsymbs3=(int *)p_bf3;
/*     fill gnames3 and gsymbs3                      */
/*          operation   buff1*buff2                  */
    if(operation==2)goto commalab;
    ii=-1;
    for(i=0;i<lngth1;i++)
     {
      for(j=0;j<lngth2;j++)
       {
        ii++;
        for(i1=0;i1<nway3;i1++)
         {
          if(i1<nway1)
           {
            jj=gnames1[i1*lngth1+i];
            jj2=gsymbs1[i1*lngth1+i];
           }
          else
           {
            jj=gnames2[(i1-nway1)*lngth2+j];
            jj2=gsymbs2[(i1-nway1)*lngth2+j];
           }
          gnames3[i1*lngth3+ii]=jj;
          gsymbs3[i1*lngth3+ii]=jj2;
         }
       }
     }
/*    fill object vble area             */
    if(vbltyp3==1)goto endlab;
    ii=-1;
    for(i=0;i<lngth1;i++)
     {
      for(j=0;j<lngth2;j++)
       {
        ii++;
        if(vbltyp2>=2)
          objvbl3[ii]=objvbl2[j];
        if(vbltyp1>=2)
          objvbl3[ii]=objvbl1[i];
       }
     }
    goto endlab;
/* operation comma:  buff1,buff2            */
commalab:  ii=-1;
   for(i=0;i<nway3;i++)
    {
    for(j=0;j<lngth3;j++)
     {
      ii++;jj=-1;jj2=-1;
      if(j<lngth1)
       {
        if(i<nway1)
         {
          jj=gnames1[i*lngth1+j];
          jj2=gsymbs1[i*lngth1+j];
         }
       }
      else
       {
        if(i<nway2)
         {
          jj=gnames2[i*lngth2+j-lngth1];
          jj2=gsymbs2[i*lngth2+j-lngth1];
         }
       }
      gnames3[ii]=jj;
      gsymbs3[ii]=jj2;
     }
   }
  if(vbltyp3==1)goto endlab;
/*   fill objvbl3                   */
  ii=-1;
  for(i=0;i<lngth3;i++)
   {
    ii++;objvbl3[ii]=-1;
    if(i<lngth1 && vbltyp1>=2)
        objvbl3[ii]=objvbl1[i];
    if(i>=lngth1 && vbltyp2>=2)
        objvbl3[ii]=objvbl2[i-lngth1];
   }
/*                                  */
/*    free buff1 and buff2          */
endlab: 
/* RS REM
		muste_free(buffer[buff1]);
        muste_free(buffer[buff2]);       
        buffer[buff1]=NULL;
        buffer[buff2]=NULL;
*/         
  return(1);
 }

 /* meanspr.c    */
/*                                                             */
/*  meanspr printout of means, sums and deviations             */
/*                                       mpk       16.8.1989   */
static int meanspr(void)
 {
  int nbackvbles,first_back,found_unused;
  int firstb,lasti=0,lt,ltext=0,ik2,ik1,ik7,ik8,ik6,ik5,ik4,ik3,i,j,k;
/*                                                             */
  pspace_pr=NULL; pspace_nl=NULL; pspace_pr2=NULL;
/*  i=prspace(); if(i<0)goto errclosop;    */
/*  strcpy(xx," ");foutput(0);             */
  j=-1; lt=0;
  for(i=0;i<24;i++)
   {if(statistics[i]>0)lasti=i;}
  for(i=0;i<24;i++)
   {
    k=statistics[i];
    if(k<1)continue;
    j++;
    if(k==1){strcpy(yy,"Means");ltext=5;}
    if(k==2){strcpy(yy,"Sums");ltext=4;}
    if(k==3){strcpy(yy,"Deviations");ltext=10;}
    if(k==4){strcpy(yy,"Frequencies");ltext=11;}
    if(k==5){strcpy(yy,"Minimums");ltext=8;}
    if(k==6){strcpy(yy,"Maximums");ltext=8;}
    if(k==7){strcpy(yy,"Std. err. of means");ltext=18;}
    if(k==8){strcpy(yy,"Percentages of freq.");ltext=20;}
    if(k==9){strcpy(yy,"Percentages of sums");ltext=19;}
    if(k==10){strcpy(yy,"Column percentages of freq.");ltext=27;}
    if(k==11){strcpy(yy,"Column percentages of sums");ltext=26;}
    if(k==12){strcpy(yy,"Row percentages of freq.");ltext=24;}
    if(k==13){strcpy(yy,"Row percentages of sums");ltext=23;}
    if(k==14){strcpy(yy,"Table percentages of freq.");ltext=26;}
    if(k==15){strcpy(yy,"Table percentages of sums");ltext=25;}
    if(k==16){strcpy(yy,"Columnwise cumulative frequencies");ltext=32;}
    if(k==17){strcpy(yy,"Rowwise cumulative frequencies");ltext=30;}
    if(k==18){strcpy(yy,"Columnwise cumulative sums");ltext=25;}
    if(k==19){strcpy(yy,"Rowwise cumulative sums");ltext=23;}
    if(k==20){strcpy(yy,"Cumulative column percentages of frequencies");
              ltext=45;}
    if(k==21){strcpy(yy,"Cumulative row percentages of frequencies");
              ltext=42;}
    if(k==22){strcpy(yy,"Cumulative column perecntages of sums");ltext=38;}
    if(k==23){strcpy(yy,"Cumulative row percentages of sums");ltext=35;}
    if(j==0)
     { strcpy(xx,yy);lt+=ltext; }
    else
     {
      if(i==lasti)
       {strcat(xx," and ");lt+=5;}
      else
       {strcat(xx,", ");lt+=2;}
      if(lt+ltext>72)
       { if(html_on==0 || strlen(tabltitle)<2)foutput(0);
         lt=ltext; strcpy(xx,yy);}
      else
       {
        strcat(xx,yy); lt+=ltext;
       }
     }
   }
  if(html_on==0 || strlen(tabltitle)<2)foutput(1);
/*                                                                       */
/* determine which grouping variables should be used as background vbles */
/*                                                                       */
  nbackvbles=0;
  first_back=0;
  if(col_given==0)
   {
    if(ny>0 && y_used==2)first_back=1;
    if(ny<2 && y_used==0)first_back=1;
   }
  if(row_given==0)
   {
    if(ny<2 || y_used>0)first_back++;
    if(ny>1 && col_given==0 && y_used==0)first_back++;
   }
  found_unused=0;
  for(i=0;i<8;i++)
   {
    if(i<pway)
     {
      if(igv_used[i]<1)
       {
        found_unused++;
        if(found_unused>first_back)
         {nwnd[i]=nd[i]; nbackvbles++;}
        else
         {nwnd[i]=1;igv_used[i]=-1;}
       }
      else
       nwnd[i]=1;
     }
    else
      nwnd[i]=1;
   }
  firstb=1;
  for(ik8=0;ik8<nwnd[7];ik8++)
   {
    ik[7]=ik8;
  for(ik7=0;ik7<nwnd[6];ik7++)
   {
    ik[6]=ik7;
  for(ik6=0;ik6<nwnd[5];ik6++)
   {
    ik[5]=ik6;
    for(ik5=0;ik5<nwnd[4];ik5++)
     {
      ik[4]=ik5;
      for(ik4=0;ik4<nwnd[3]; ik4++)
       {
        ik[3]=ik4;
        for(ik3=0;ik3<nwnd[2]; ik3++)
         {
         ik[2]=ik3;
         for(ik2=0;ik2<nwnd[1]; ik2++)
          {
          ik[1]=ik2;
          for(ik1=0;ik1<nwnd[0]; ik1++)
            {
             ik[0]=ik1;
          if(nbackvbles>0 && firstb==1)
             {
              strcpy(xx," "); foutput(0);
             }
         if(nbackvbles>0)i=(int)mns_prbackv(nbackvbles,first_back,pway);
         i=(int)mnsprmdv();
         if(i<0)return(-1);
          }
         }
       }
     }
   }
   }
   }
   }
/* RS REM   
  if(pspace_pr != NULL)muste_free(pspace_pr);
  if(pspace_nl != NULL) muste_free(pspace_nl);
  if(pspace_pr2 != NULL)muste_free(pspace_pr2);
*/  
  return(1);
//errclosop: return(-1);
//errwait:   return(-1);
 }
/* vbl_frmt.c  */
/*                                                                  */
/*   Replace format symbol "def" according to the minima and maxima */
/*                                     mpk     6.9.1989             */
static int vbl_frmt(int i_grp,int index,int *width,int *des)
 {
  char x2[36],*pfrmt;
  int i,ind24,j,l,valos,ndj,jmaxn,indsums;
  double cs=0,maxcslkm,xintpart,valx,xvert,xv1;
/*                                              */
  ind24=index*36; pfrmt=(char *)y_fmt;
  indsums=0;
  if(ny>0)
   {
    if(prntopt[1]>0 || prntopt[17]>0 || prntopt[18]>0)indsums=1;
   }
  if(i_grp==2)pfrmt=(char *)gnest_fmt;
  if(i_grp==1)pfrmt=(char *)grp_fmt;
  if(i_grp==0)pfrmt=(char *)y_fmt;
  pfrmt+=ind24*sizeof(char);
  strcpy(x2,pfrmt);
  l=strlen(x2);
  if(l>0)i=(int)strcmp(x2,"def");
/*     width/des = -1 means unknown or default         */
  if(i != 0 && l>0)return(0);  /* format given         */
  if(i_grp==2)
   {
    if(igv_vartype_nested[index]>0)
     {
      *width=igv_vartype_nested[index]; *des=0;
      if(*width<8)*width=8;
      return(0);
     }
   }
  if(i_grp==1)
   {
    if(igv_vartype[index]>0)
     {
       *width=igv_vartype[index];
       *des=0;
       if(*width<8)*width=8;
       return(0);
     }
   }
  if(i_grp==0)
   {
    cs=(double)fabs(total_maxy[index]);
    *des=6; strcpy(x2,"1.123456");
    if(indsums==0)
     {
      if(cs>0.1)
       {
        strcpy(x2,"12.12345");*des=5;
       }
      if(cs>10.0){strcpy(x2,"1234.123");*des=3;}
      if(cs>1000.0){strcpy(x2,"12345.12");*des=2;}
      if(cs>100000.0){strcpy(x2,"123456.1");*des=1;}
     }
    else
     {
      maxcslkm=total_lkm[index]*cs;
      strcpy(x2,"12345678"); *des=0;
      if(maxcslkm<1000000.0){strcpy(x2,"123456.1"); *des=1;}
      if(maxcslkm<10000.0){strcpy(x2,"1234.123"); *des=3;}
      if(maxcslkm<100.0){strcpy(x2,"12.12345"); *des=5;}
     }
   }
  if(i_grp>0)
   {
    if(i_grp==1)
     {
      cs=(double)fabs(max_gvals[index]);
     }
    if(i_grp==2)
     {
      cs=(double)fabs(max_gvals_nest[index]);
     }
    strcpy(x2,"1.123456"); *des=6;
    if(cs>0.01){strcpy(x2,"12.12345");*des=5;}
    if(cs>1.0){strcpy(x2,"1234.123");*des=3;}
    if(cs>1000.0){strcpy(x2,"123456.1");*des=1;}
    if(cs>=100000.0){strcpy(x2,"12345678");*des=0;}
/*   check if decimals are unnecessary                   */
    if(cs<100000.0)
     {
      if(i_grp==1)
       {
           jmaxn=maxn*index; ndj=nd[index]; xv1=values[jmaxn];
       }
      else
       {
           jmaxn=nested_maxn*index; ndj=nd[index];
           xv1=values_nested[jmaxn];
       }
      xvert=0.0000005;
      if(xv1 != 0.0)
          {
           xvert=xv1*xvert;
           xvert=fabs(xvert);
          }
       j=0;
       for(i=0;i<ndj;i++)
          {
           valos=i+jmaxn;
           if(i_grp==1)
             xv1=values[valos];
           else
             xv1=values_nested[valos];
           xintpart=floor(xv1);
           valx=xv1-xintpart;
           valx=fabs(valx);
           if(valx>xvert){ j=1; break;}
          }
       if(j==0)strcpy(x2,"12345678"); *des=0;
    }
  }
 strcpy(pfrmt,x2); *width=8;
 return(1);
}
/*  mnsprbck.c      */
/*                                                              */
/*  print values of background variables                        */
/*                                       mpk       5.9.1989     */
static int mns_prbackv(int nbackvbls,int firstback,int i2)
 {
  int i,j,iprinted,ibackf,lowerlim,l,k,ix1;
  int maxlines,iyla,leveys,outermost=0,nestaddr=0;
  maxlines=line_length-1;
  linspace(xx,maxlines); ix1=0; ibackf=0; iprinted=0;
  if(html_on>0)strcpy(xx," ");
  for(k=0;k<i2;k++)
   {
    if(igv_used[k]<1)
     {
      ibackf++;
      if(ibackf>firstback)goto found;
      igv_used[k]=-1;
     }
   }
  return(1);
found: lowerlim=k;
  for(k=i2-1;k>=lowerlim;k--)
   {
    if(igv_used[k]>0)continue;
    iyla=0;
    if(nested[k]>0)iyla=pway_nested-1;
    for(i=iyla;i>=0;i--)
     {
      if(iyla>0)
       {
        outermost=0;
        if(iyla==i)outermost=1;
        nestaddr=address_nested[i+k*pway_nested];
        if(nestaddr<0)continue;
        if(html_on>0)
         {
          strcpy(yy,"?");
          for(j=0;j<ngv;j++)
           { if(igv[j]==igv_nested[i+k*pway_nested])
              { strcpy(yy,gtitle+128*j); break;
              }
           }
         }
        else
         strcpy(yy,d.varname[igv_nested[i+k*pway_nested]]);
       }
      else
       {
        if(html_on>0)
         strcpy(yy,gtitle+128*k);
        else
         strcpy(yy,d.varname[igv[k]]);
       }
      l=strlen(yy);
      if(html_on>0)
       {
        strcat(xx,yy); strcat(xx,": ");
       }
      else
       {
        strcpy(xx+ix1,"Variable ");
        strcpy(xx+ix1+9,yy); xx[ix1+l+9]=' ';
        strcpy(xx+ix1+17," Group ");
       }
      if(iyla>0)
       {
        leveys=nest_width[nestaddr];
        l=(int)mns_nest_labcpy(yy,ik[k],nestaddr,leveys,0,outermost);
       }
      else
       {
        leveys=grp_width[k];
        if(leveys<1)leveys=label_length1;
        l=(int)mns_labcpy(ik[k],k,leveys,0);
       }
      if(html_on>0)
       {
        strcat(xx,yy); foutput(0); strcpy(xx," ");
       }
      else
       {
        strcpy(xx+ix1+24,yy);
        if(ix1>39 || maxlines<80)
         {
          foutput(0); ix1=0; linspace(xx,maxlines);
          iprinted++;
         }
        else
         {
          xx[ix1+l+24]=' '; ix1=ix1+l+28;
         }
       }
     }
   }
  if(ix1>0){foutput(0);iprinted++;}
  if(iprinted>0)foutput(-1);
  return(1);
 }

 /* mnsprmdv.c   */
/*                                                              */
/*  print means and deviations                                  */
/*                                       mpk       28.8.1989    */
/*  percentages and nested grouping vbles added   24.12.1990    */
static int mnsprmdv(void)
 {
  int i,i1,j,ij,ij2,ijk,ikx,mijk=0,k,l=0,ix1,icols,rowmax,max_length;
  int rowstat,nof_stat,row_stat,col_stat,lasti,cols_used,same_row;
  int ynam,same_subrow,firsti,subtable_no,icode;
  int iprst,nik,nestaddr=0,iix1,nest_ways,j1,j2,fill_blank;
  int vbleos=0,kpoint,outermost,first_nestway,indij;
  int ixhtml,icolshtml; // merkki,
  char zz[LLENGTH],zzname[93]; // RS CHA 128 -> LLENGTH
 long ndv,nobs,total_row,n_grandtotal,n_coltotal,n_rowtotal,n_tabletotal; // ncod,
  double s_coltotal,s_rowtotal,s_tabletotal;
  double znobs,a,b,ymin,ymax,xmean=0,xdev=0,s_grandtotal,prcnt=0;
/*                                              */
/* get the column defintions                    */
  strcpy(zzname,
"M   S   D   F   MIN MAX SEM %F  %S  C%F C%S R%F R%S T%F T%S CCF CRF CCS CRS");
  strcat(zzname," CR%FCC%FCR%SCC%S");
  i1=0;
  if(*col_address >= 0)
   {
    p_mnspr=(char *)buffer[*col_address];
    col_ways=*(int *)p_mnspr; p_mnspr+=sizeof(int);
    col_lngth=*(int *)p_mnspr; p_mnspr+=sizeof(int);
    col_vbltype=*(int *)p_mnspr; p_mnspr+=sizeof(int);
    col_objvbles=(int *)p_mnspr; p_mnspr+=col_lngth*sizeof(int);
    if(col_vbltype != 2)
     {
      col_gvbles=(int *)p_mnspr; p_mnspr+=col_ways*col_lngth*sizeof(int);
      col_symbols=(int *)p_mnspr;
     }
   }
  else
   {
    i1=0; col_ways=0; col_lngth=1;
    col_vbltype=1;
    if(y_used==0 && ny>1)
     {
      col_lngth=ny;
      col_objvbles=iy;col_vbltype=2;
     }
    if(y_used==2 || (y_used==0 && ny<=1))
     {
     for(i=0;i<ngv;i++)
      {
       if(igv_used[i]<1)goto gfound;
      }
     col_ways=0; col_lngth=1;
     goto colcont;
gfound: col_ways=1;
     col_lngth=nd[i];
     if(coltotals>0)col_lngth++;
     if(nested[i]>0)
      {
       nestaddr=address_nested[i*pway_nested];
       col_lngth+=nd_nested[nestaddr];
      }
colcont:  // p_mnspr=(char *)muste_malloc(3*col_lngth*sizeof(int)); // RS CHA
     p_mnspr=(char *)muste_realloc(p_mnspr,10*col_lngth*sizeof(int));
     col_symbols=(int *)p_mnspr; p_mnspr+=col_lngth*sizeof(int);
     col_gvbles=(int *)p_mnspr; p_mnspr+=col_lngth*sizeof(int);
     col_objvbles=(int *)p_mnspr; kpoint=0;
     for(j=0;j<col_lngth;j++)
      {
       col_symbols[j]=j;
       if(col_ways>0)
        {
         if(nested[i]>0)
          {
           if(j==change_points[nestaddr*nested_maxn+kpoint]+kpoint)
            {
             col_symbols[j]=10000+kpoint;
             kpoint++;
            }
           else
            {
             col_symbols[j]=j-kpoint;
            }
          }
        }
       if(coltotals>0 && j==col_lngth-1)col_symbols[j]=9999;
       col_objvbles[j]=0;
       if(y_used==0 && ny==1)col_objvbles[j]=iy[0];
       col_gvbles[j]=igv[i];
      }
     if(ny>0 && y_used==0)col_vbltype=3;
     i1=i+1;
    }
   }
/*  get the row definitions                               */
  if(*row_address >= 0)
   {
    p_mnspr=(char *)buffer[*row_address];
    row_ways=*(int *)p_mnspr; p_mnspr+=sizeof(int);
    row_lngth=*(int *)p_mnspr; p_mnspr+=sizeof(int);
    row_vbltype=*(int *)p_mnspr; p_mnspr+=sizeof(int);
    row_objvbles=(int *)p_mnspr; p_mnspr+=row_lngth*sizeof(int);
    if(row_vbltype != 2)
     {
      row_gvbles=(int *)p_mnspr;p_mnspr+=row_lngth*row_ways*sizeof(int);
      row_symbols=(int *)p_mnspr;
     }
   }
  else
   {
    row_lngth=1; row_ways=0;
    if(ny>0 && y_used==0 && col_vbltype<2)
     {
      row_lngth=ny;
      row_objvbles=iy; row_vbltype=2;
     }
    if(ny<2 || y_used>0 || col_vbltype>1)
     {
      for(i=i1;i<ngv;i++)
       {
        if(igv_used[i]<1)goto rowfound;;
       }
      goto rowcont;
rowfound: row_ways=1;
      row_lngth=nd[i]; if(rowtotals>0)row_lngth++;
      if(nested[i]>0)
       {
        nestaddr=address_nested[i*pway_nested];
        row_lngth+=nd_nested[nestaddr];
       }
      row_gvbles=&igv[i];
rowcont:  // p_mnspr=(char *)muste_malloc(3*row_lngth*sizeof(int)); // RS CHA
      p_mnspr=(char *)muste_realloc(p_mnspr,10*row_lngth*sizeof(int));
      row_symbols=(int *)p_mnspr;p_mnspr+=row_lngth*sizeof(int);
      row_gvbles=(int *)p_mnspr; p_mnspr+=row_lngth*sizeof(int);
      row_objvbles=(int *)p_mnspr;
      kpoint=0;
      for(j=0;j<row_lngth;j++)
       {
        row_symbols[j]=j;
        if(row_ways>0)
        {
         if(nested[i]>0)
          {
           if(j==change_points[nestaddr*nested_maxn+kpoint]+kpoint)
            {
             row_symbols[j]=10000+kpoint;
             kpoint++;
            }
           else
            {
             row_symbols[j]=j-kpoint;
            }
          }
        }
        if(rowtotals>0 && j==row_lngth-1)
         {
          row_symbols[j]=9999;
         }
        row_gvbles[j]=igv[i]; row_objvbles[j]=0;
        if(ny>0 && y_used==0 && col_vbltype==1)row_objvbles[j]=iy[0];
       }
      row_vbltype=1;
      if(y_used==0 && col_vbltype==1 && ny>0)row_vbltype=3;
     }
   }
/*     compute col and row lengths and divide the table in parts    */
/*     first the width of row definitions in columns                */
/*           at this moment 8+1 char / variable is assumed            */
  icols=5; icolshtml=0;
  if(row_vbltype>1)icols=9;
  for(i=row_ways-1;i>=0;i--)
   {
    icols++;   /*  first a blank column as a separator  */
    rowmax=get_column_width(0,row_lngth-1,i,0,0);
    icols+=rowmax;   icolshtml++;
   }
/*  number of statistics to be printed                    */
  nof_stat=0; cumulative_results=0; percents_results=0;
  for(i=0;i<25;i++)
   {
    j=statistics[i];
    if(j>0)
     {
      nof_stat++;
      if(j>15)cumulative_results=1;
      if(j>7 && j<16)percents_results=1;
      if(j>19 && j<24)percents_results=1;
     }
   }
/*  find the seq number of last column in this subtable   */
  subtable_no=0;
  firsti=0;
  max_length=line_length;
  row_stat=1; col_stat=1;
  if(row_vbltype>1)
    row_stat=nof_stat;
  else
   col_stat=nof_stat;
/*  next subtable    */
nextsubtabl:  cols_used=icols+1;
  for(i=firsti;i<col_lngth;i++)
   {
    j=get_colwidth(i);
    cols_used=cols_used+(j+1)*row_stat;
    if(cols_used>max_length)goto lab1;
   }
  lasti=col_lngth;
  goto lab2;
lab1: lasti=i;
  if(lasti<=firsti)
   { sprintf(sbuf,"\nSpecified table requires longer line length!");
     return(-1);
   }
lab2: subtable_no++;
  if(html_on>0)
   {
    strcpy(xx,"<P><CENTER><TABLE BORDER=2");
    ijk=strlen(tablbgcolor);
    if(ijk>1)
     {
      strcat(xx," BGCOLOR="); strcat(xx,tablbgcolor);
     }
    strcat(xx," CELLPADDING=5>");
    strcat(xx,"<TH COLSPAN=");
    ijk=lasti-firsti+1;
    muste_itoa(ijk,yy,10); strcat(xx,yy);
    strcat(xx," ROWSPAN=1>");
    foutput(0);
    l=strlen(tablhdcolor);
    if(l>1)
     {
      strcpy(xx,"<font color=>");
      strcat(xx,tablhdcolor); strcat(xx,">");
      foutput(0);
     }
    strcpy(xx,tabltitle);
    if(lasti<col_lngth || subtable_no>1)
     { strcat(xx," Subtable ");
       muste_itoa(subtable_no,yy,10);
       strcat(xx,yy);
     }
    if(l>1)strcat(xx,"</font>");
    strcat(xx,"<TR>"); foutput(0); goto jatkatabl;
   }
  if(lasti<col_lngth || subtable_no >1)
   {
    strcpy(xx,"Subtable ");
    muste_itoa(subtable_no,yy,10);
    strcat(xx,yy);
    foutput(1);
   }
jatkatabl: ;
  if(html_on>0)
   {
    for(i=0;i<row_ways;i++)
     {

      strcpy(xx,"<TH ROWSPAN=2 VALIGN=BOTTOM ALIGN=LEFT COLSPAN=1>");
      if(col_ways<1)xx[12]='1'; /* jos ei col-mtjaa niin rowspan=1 */
      for(k=0;k<ngv;k++)
       {
        if(row_gvbles[i]==igv[k])
         {
          strcat(xx,gtitle+128*k);
          if(col_ways<1)strcat(xx,"<TR ALIGN=RIGHT>");
          foutput(0); goto jatkagt;
         }
       }
 jatkagt: ;
     }
   }

/*                                                        */
/*   print the subtable from column firsti to lasti       */
/*   first col_vble names and symbols                     */
/*   ===================================================  */
for(i=0; i<col_ways;i++)
 {
  nest_ways=0;
  for(j=firsti;j<lasti;j++)
   {
    if(col_gvbles[j+i*col_lngth]<0)continue;
    for(i1=0;i1<pway;i1++)
     {
      if(igv[i1]==col_gvbles[j+i*col_lngth])break;
     }
    k=0;
    if(nested[i1]==0)continue;
    for(j1=0;j1<pway_nested;j1++)
     {
      nestaddr=address_nested[j1+i1*pway_nested];
      if(nestaddr<0)break;
      k++;
     }
    if(k>nest_ways)nest_ways=k;
   }
/*       loop over nested ways                                  */
  if(nest_ways==0)nest_ways=1;
  strcpy(former_outlab," ");
  for(j1=nest_ways-1;j1>=0;j1--)
/* -----------------------------          */
   {
    outermost=0;
    if(nest_ways>1 && j1==nest_ways-1)outermost=1;
    linspace(xx,max_length);
    ix1=icols; ixhtml=icolshtml;
    if(html_on>0)strcpy(xx," ");
    for(j=firsti;j<lasti;j++)
     {
      strcpy(yy," ");
      ij=col_gvbles[j+i*col_lngth];
      fill_blank=0;
      if(ij>=0)
       {
        for(j2=0;j2<ngv;j2++)
         {
          if(igv[j2]==ij)break;
         }
        if(nested[j2]==0 && j1>0)fill_blank=1;
        if(nested[j2]>0)
         {
          nestaddr=address_nested[j1+j2*pway_nested];
          if(nestaddr<0)fill_blank=1;
         }
        if(nested[j2]==0 || fill_blank==1)
          vbleos=ij;
        else
         {
          vbleos=igv_nested[j1+j2*pway_nested];
         }
        l=blnkaway(d.varname[vbleos],yy);
       }
      ijk=get_colwidth(j);
      for(k=0;k<row_stat;k++)
       {
        ix1++;
        ix1+=ijk; if(l>ijk)l=ijk;
        if(html_on>0)
         {
          if(i==0)
           {
            if(j==0 && k==0)
             {
              strcat(xx,"<TH ALIGN=CENTER ROWSPAN=1 COLSPAN=");
              muste_itoa(row_stat*(lasti-firsti),yy,10);
              strcat(xx,yy); strcat(xx,">");
              strcat(xx,gtitle+128*i1);
             }
           }
          else
           {
            strcat(xx,"<TD align=center>");
            strcat(xx,yy); strcat(xx,"</td>");
           }
         }
        else
         {
          if(fill_blank==0)strncpy(xx+ix1-l,yy,l);
          if(j<lasti-1 || k<row_stat-1)xx[ix1]=' ';
         }
       }
     }
    if(html_on>0)
     {
      strcat(xx,"<TR ALIGN=RIGHT>");
     }
    else
     {
      xx[ix1]='\0';
     }
    foutput(0);


    linspace(xx,max_length);
    if(html_on>0)strcpy(xx," ");
    ix1=icols;  ixhtml=icolshtml;
    for(j=firsti;j<lasti;j++)
     {
      ij=j+i*col_lngth;
      for(j2=0;j2<ngv;j2++)
       {
        if(igv[j2]==col_gvbles[ij])break;
       }
      fill_blank=0;
      if(nested[j2]==0 && j1>0)fill_blank=1;
      if(nested[j2]>0)
       {
        nestaddr=address_nested[j1+j2*pway_nested];
        if(nestaddr<0)fill_blank=1;
       }
      if(nested[j2]==0 || fill_blank==0)vbleos=col_gvbles[ij];
      if(nested[j2]>0 && fill_blank==1)vbleos=igv_nested[j1+j2*pway_nested];
      ijk=get_colwidth(j);
      strcpy(yy," ");
      if(vbleos>=0)
       {
        ij2=col_symbols[ij];
        for(k=0;k<ngv;k++)
         {
          if(igv[k]==vbleos)break;
         }
        if(nested[j2]==0)
          {
           l=mns_labcpy(ij2,k,ijk,1);
          }
        if(nested[j2]>0 && fill_blank==0)
          {
           l=mns_nest_labcpy(yy,ij2,nestaddr,ijk,1,outermost);
           if(outermost==1)strcpy(former_outlab,yy);
         }
       }
      l=strlen(yy);
      for(k=0;k<row_stat;k++)
       {
        if(html_on>0)
         {
          strcat(xx,"<TD align=center>");
          strcat(xx,yy); strcat(xx,"</TD>");
         }
        else
         {
          ix1+=ijk; ix1++;
          if(fill_blank==0)strcpy(xx+ix1-l,yy);
          if(j<lasti-1 || k<row_stat-1)xx[ix1]=' ';
         }
       }
     }
    if(html_on>0)
     strcat(xx,"<TR ALIGN=RIGHT>");
    else
     xx[ix1]='\0';
    foutput(0);
   }    /*  end of loop for column variables          */
 }      /*  end of loop for nested variables          */
/*      print the row for col_objvbles              */
  linspace(xx,max_length);
  if(html_on>0)strcpy(xx," ");


  ix1=icols;   ixhtml=icolshtml;
  if(col_vbltype>1)
   {
    if(ixhtml>0)
     {
      for(j=0;j<ixhtml;j++)
       {
         strcat(xx,"<TD>"); strcat(xx," ");
       }
     }
    for(j=firsti;j<lasti;j++)
     {
      strcpy(yy," ");
      if(col_objvbles[j]>=0)
       {
        if(html_on>0)
         {
          mijk=-1;
          for(ijk=0;ijk<ny;ijk++)
           {
            if(iy[ijk]==col_objvbles[i]){mijk=ijk; break;}
           }
          if(mijk<0 || y_title[mijk*41]==' ')
           {
            l=blnkaway(d.varname[col_objvbles[j]],yy);
           }
          else
           strcat(yy,&y_title[mijk*41]);
         }
        else
          l=blnkaway(d.varname[col_objvbles[j]],yy);
       }
      l=strlen(yy);
      ijk=get_column_width(j,j,0,1,1);
      if(ijk<1)ijk=8;

      if(html_on>0)
       { strcat(xx,"<TD align=center>");
         strcat(xx,yy);  strcat(xx,"</TD>");
       }
      if(html_on==0)
       {
        ix1+=ijk; ix1++;
        strcpy(xx+ix1-l,yy);
        if(j<lasti-1)xx[ix1]=' ';
       }
     }
    if(html_on>0)
     strcat(xx,"<TR ALIGN=RIGHT>");
    if(html_on==0)
     xx[ix1]='\0';
    foutput(0);
   }

  if(row_vbltype>1)
   {
 /*   print the row for labels for statistics (M, D, N, S)   */
    linspace(xx,max_length);
    if(html_on>0)strcpy(xx," ");
    ix1=icols;
    for(j=firsti;j<lasti;j++)
     {
      strcpy(yy," ");
      ijk=get_column_width(0,row_lngth-1,0,0,1);
      for(k=0;k<row_stat;k++)
       {
        ix1++;
        mijk=statistics[k];
        if(mijk==1)strcpy(yy,"Means");
        if(mijk==2)strcpy(yy,"Sums");
        if(mijk==3)strcpy(yy,"St.d.");
        if(mijk==4)strcpy(yy,"Freq.");
        if(mijk==5)strcpy(yy,"Min");
        if(mijk==6)strcpy(yy,"Max");
        if(mijk==7)strcpy(yy,"Sem");
        if(mijk==8)strcpy(yy,"%F");
        if(mijk==9)strcpy(yy,"%Sums");
        if(mijk>9)
         {
          l=4*(mijk-1);
          strncpy(yy,zzname+l,4);
          yy[4]='\0';
         }
        l=move_str(zz,yy,ijk,1);
        l=strlen(zz);
        if(html_on>0 && mijk<=9)
         {
          strcat(xx,"<TD align=right>");
          strcat(xx,zz); strcat(xx,"</TD>");
         }
        else
         {
          strcpy(xx+ix1,zz);xx[ix1+l]=' ';
          ix1+=ijk;
          if(j<lasti-1 || k<row_stat-1)xx[ix1]=' ';
         }
       }
     }
    if(html_on>0 && mijk<=9)
     {
      strcat(xx,"<TR ALIGN=RIGHT>");
     }
    else
     {
      xx[ix1]='\0';
     }
    foutput(0);
   }
/*                                                      */
/*   loop over rows in the table                        */
/*   ===========================                        */
/*                                                      */
/*   reserve area for results in one row                */
  i=lasti-firsti;
  if(i>statspacesize)
   {
    j=stat_space(i*100); // RS ADD *100
    if(j<0)return(j);
    statspacesize=i;
   }
/*        */
  for(i=0;i<row_lngth;i++)
   {
/*      print names of row variables        */
    same_row=1;
    if(i>0)
     {
      for(j=0;j<row_ways;j++)
       {
        ij=i+j*row_lngth;
        if(row_gvbles[ij] != row_gvbles[ij-1]){same_row=0;break;}
       }
      if(same_row==0)
       {
        strcpy(xx," "); foutput(0);
       }
     }
    if(i==0 || same_row==0)
     {
      ix1=0;
      linspace(xx,max_length);
      if(html_on>0)strcpy(xx," ");
      for(j=0;j<row_ways;j++)
       {
        mijk=get_column_width(0,row_lngth-1,j,0,0);
        ix1++;
        strcpy(yy," ");
        ijk=i+j*row_lngth;
        for(k=0;k<ngv;k++)
         {
          if(igv[k]==row_gvbles[ijk])break;
         }
        if(nested[k]>0 && row_gvbles[ijk]>=0)
         {
          for(i1=pway_nested-1;i1>=0;i1--)
           {
            nik=i1+k*pway_nested;
            nestaddr=address_nested[nik];
            if(nestaddr<0)continue;
            l=blnkaway(d.varname[igv_nested[nik]],yy);
            if(l>nest_width[nestaddr])
             {
              l=nest_width[nestaddr]; yy[l]='\0';
             }
            if(html_on>0)
             {
              strcat(xx,"<TD ALIGN=LEFT>");
              strcat(xx,gtitle+k*128); strcat(xx,"</TD>");
             }
            else
             {
              strncpy(xx+ix1,yy,l);
              if(i1>0 || j<row_ways-1)xx[ix1+l]=' ';
              ix1+=nest_width[nestaddr];
              if(i1>0)ix1++;
             }
           }
         }
        else
         {
          if(row_gvbles[ijk]>=0)
           {
            l=blnkaway(d.varname[row_gvbles[ijk]],yy);
           }
          if(l>mijk)
           { yy[mijk]='\0'; l=mijk; }
          if(html_on>0)
           {
      /*    strcat(xx,"<TD>");
            strcat(xx,yy)   */  ;
           }
          else
           {
            strncpy(xx+ix1,yy,l);
            if(j<row_ways-1){xx[ix1+l]=' ';}
            ix1+=mijk;
           }

         }
       }
/*    if(html_on>0)
       {
        strcat(xx,"<TR ALIGN=RIGHT>");
       }  */
      if(html_on==0)
       {
        xx[ix1]='\0';
        foutput(0);
       }
     }
    total_row=0;
/*                                                        */
/*    loop over statistics to be printed on diff rows     */
    iprst=0; indij=-1;
    for(rowstat=0;rowstat<col_stat;rowstat++)
     {
      k=statistics[rowstat];
      j=1;
      if(k==4 || k==8 || k==10 || k==12 || k==14 || k==16 || k==17)j=0;
      if(k==20 || k==21)j=0;
      if(j==1)
       {
   if(col_vbltype<2 && row_vbltype<2)continue; /* only frequencies possible */
   if(ivariables==1 && row_vbltype<2)continue;
       }
      indij++; if(indij>1)indij=1;
      ix1=0; iprst++;
      linspace(xx,max_length);
      if(html_on>0)
       strcpy(xx," ");
      if(iprst==1)
       {
        for(j=0;j<row_ways;j++)
         {
          rowmax=get_column_width(0,row_lngth-1,j,0,0);
          ix1++;
          strcpy(yy," ");
          ijk=i+j*row_lngth;
          same_subrow=1;
          if(same_row==0 || i==0)same_subrow=0;
          if(row_symbols[ijk-1] != row_symbols[ijk])same_subrow=0;
          if(j==row_ways-1 && row_symbols[ijk]>=9999)total_row=1;
          if(same_subrow==0)
           {
            if(row_gvbles[ijk]>=0)
             {
              ij=row_symbols[ijk];
              for(k=0;k<ngv;k++)
               {
                if(igv[k]==row_gvbles[ijk])break;
               }
              if(nested[k]>0)
               {
                iix1=0; strcpy(yy," "); first_nestway=-1;
                for(i1=pway_nested-1;i1>=0;i1--)
                 {
                  outermost=0;
                  nik=i1+k*pway_nested;
                  nestaddr=address_nested[nik];
                  if(nestaddr<0)continue;
                  if(first_nestway<0)first_nestway=i1;
                  if(i1==first_nestway)outermost=1;
           l=mns_nest_labcpy(zz,ij,nestaddr,nest_width[nestaddr],0,outermost);
                  l=strlen(zz);
                  if(l>nest_width[nestaddr])
                   {
                    l=nest_width[nestaddr]; zz[l]='\0';
                   }
                  if(outermost==1)strcpy(former_outlab,zz);
                  strcpy(yy+iix1,zz);
                  yy[iix1+l]=' ';
                  iix1+=nest_width[nestaddr]; if(i1>0)iix1++;
                 }
               }
              else
               {
                l=mns_labcpy(ij,k,rowmax,0);
               }
             }
           }
          l=strlen(yy);
          if(l>rowmax)
           { yy[rowmax]='\0'; l=rowmax; }
          if(html_on>0)
           {
            strcat(xx,"<TD ALIGN=LEFT>");
            if(hrefvble==igv[k])
             {
              strcat(xx,"<A HREF=");
              strcat(xx,hrefbase);
              ijk=l; if(ijk>3)ijk=3;
              for(ikx=l-1;ikx>=0;ikx--)
               {
                if(yy[ikx] != ' ')break;
                ijk=ikx;
               }
              strncat(xx,yy,ijk);
              strcat(xx,".htm");
              strcat(xx,"#"); strcat(xx,hrefbase);
              strncat(xx,yy,ijk); strcat(xx,">");
              strcat(xx,yy);
              strcat(xx,"</A>");
             }
            else
             { strcat(xx,yy);
             }
            strcat(xx,"</TD>");
           }
          else
           {
            strcpy(xx+ix1,yy);
            xx[ix1+l]=' ';
            ix1+=rowmax;
           }
         }
        if(row_vbltype>1)
         {
          ix1++;
          strcpy(yy," ");
          if(row_objvbles[i]>=0)
           {
            l=blnkaway(d.varname[row_objvbles[i]],yy);
           }
          if(html_on>0)
           {
            strcat(xx,"<TD ALIGN=LEFT>");
            mijk=-1;
            for(ijk=0;ijk<ny;ijk++)
             {
              if(iy[ijk]==row_objvbles[i]){mijk=ijk; break;}
             }
            if(mijk<0 || y_title[mijk*41]==' ')
              strcat(xx,yy);
            else
              strcat(xx,&y_title[mijk*41]);
            strcat(xx,"</TD>");
           }
          else
           {
            l=strlen(yy);
            strcpy(xx+ix1,yy);
            xx[ix1+l]=' '; ix1+=8;
            xx[ix1]=' ';
           }
         }
       }
      else
       {
        ix1=icols-5;
        if(html_on>0)
         {
          for(j=0;j<row_ways;j++)
           {
            strcat(xx,"<TD ALIGN=LEFT> </TD>");
           }
         }
       }
      if(row_vbltype<2)
       {
        k=statistics[rowstat];
        ix1++;
        l=4*(k-1);
        strncpy(yy,zzname+l,4);
        yy[4]='\0';
          if(html_on>0)
         {
/*          strcat(xx,"<TD ALIGN=LEFT>");
          strcat(xx,yy)  */   ;
         }
        else
         {
          l=strlen(yy);
          strcpy(xx+ix1,yy);
          xx[ix1+l]=' ';ix1+=4;
         }
       }
/*  loop over colums in the row                                */
      for(j=firsti;j<lasti;j++)
       {
        mijk=8;
        if(col_vbltype>1)
         {
          mijk=get_column_width(j,j,0,1,1);
         }
        if(row_vbltype>1)
         {
          mijk=get_column_width(0,row_lngth-1,0,0,1);
         }
        if(col_vbltype==1 && row_vbltype==1)
         {
          mijk=get_column_width(j,j,0,1,0);
         }
        ijth_cellstat(&nobs,&a,&b,&ymin,&ymax,&n_grandtotal,
                      &s_grandtotal,&n_coltotal,&n_rowtotal,
                      &s_coltotal,&s_rowtotal,
                      &n_tabletotal,&s_tabletotal,i,j,indij,&ynam);
/*  loop over statistics in diff cols                          */
        for(k=0;k<row_stat;k++)
         {
          if(row_vbltype>1)
           ij=statistics[k];
          else
           ij=statistics[rowstat];
          icode=1;
          if(nobs<1)icode=0;
          if(ynam<0 || ivariables==1)
           {
            if(ij<4 || ij==5 || ij==6 || ij==7)icode=0;
           }
          if(ij==1)
           {
            if(nobs>0 && icode>0)xmean=a/nobs;
            l=dep_frmt(i,j,mijk,icode,xmean,nobs);
           }
          if(ij==2)
           {
            l=dep_frmt(i,j,mijk,icode,a,nobs);
           }
          if(ij==3 || ij==7)
           {
            if(nobs<2 || ynam<0)
             { icode=0;}
            else
             {
              icode=1;
              xdev=b-a*a/nobs;
              if(xdev<0.0)xdev=0.0;
              xdev=sqrt(xdev/(nobs-1));
              if(ij==7)
               { znobs=nobs;xdev=xdev/sqrt(znobs);
               }
             }
            l=dep_frmt(i,j,mijk,icode,xdev,nobs);
           }
          if(ij==4)
           {
            icode=2;
            l=dep_frmt(i,j,mijk,icode,xdev,nobs);
           }
/*         cumulative frequencies                     */
          if(ij==16 || ij==17)
           {
            icode=2;
            if(ij==17)l=dep_frmt(i,j,mijk,icode,xdev,cum_cnobs);
            if(ij==16)l=dep_frmt(i,j,mijk,icode,xdev,cum_rnobs);
           }
/*         cumulative sums                            */
          if(ij==18 || ij==19)
           {
            if(ynam>=0)icode=1;
            if(ij==19)l=dep_frmt(i,j,mijk,icode,cum_csumy,cum_cnobs);
            if(ij==18)l=dep_frmt(i,j,mijk,icode,cum_rsumy,cum_rnobs);
           }
          if(ij==5)
           {
            l=dep_frmt(i,j,mijk,icode,ymin,nobs);
           }
          if(ij==6)
           {
            l=dep_frmt(i,j,mijk,icode,ymax,nobs);
           }
          if(ij==8 || ij==9)
           {
            icode=3;
            if(ij==8)prcnt=100.0*nobs/n_grandtotal;
            if(ij==9)
             {
              if(ynam<0)icode=0;
              prcnt=100.0*a/s_grandtotal;
             }
            l=dep_frmt(i,j,mijk,icode,prcnt,nobs);
           }
          if(ij>=10 && ij<16)
           {
            icode=3; ndv=nobs;
            if(ynam<0)
             {
               if(ij==11 || ij==13 || ij==15)icode=0;
             }
            if(ij==10 && n_coltotal>0L)prcnt=100.0*nobs/n_coltotal;
            if(ij==11 && s_coltotal != 0.0)prcnt=100.0*a/s_coltotal;
            if(ij==12 && n_rowtotal>0L)prcnt=100.0*nobs/n_rowtotal;
            if(ij==13 && s_rowtotal != 0.0)prcnt=100.0*a/s_rowtotal;
            if(ij==14 && n_tabletotal>0L)prcnt=100.0*nobs/n_tabletotal;
            if(ij==15 && s_tabletotal != 0.0)prcnt=100.0*a/s_tabletotal;
            l=dep_frmt(i,j,mijk,icode,prcnt,nobs);
           }
/*          percentages of cumulative frequencies and  sums     */
          if(ij>=20 && ij<24)
           {
            icode=3;ndv=cum_rnobs;
            if(ij==20 || ij==22)ndv=cum_cnobs;
            if(ij==21 && n_coltotal>0L)
               {prcnt=100.0*cum_rnobs/n_coltotal;}
            if(ij==20 && n_rowtotal>0L)
               {prcnt=100.0*cum_cnobs/n_rowtotal;}
            if(ij==23 && s_coltotal != 0.0)
               {prcnt=100.0*cum_rsumy/s_coltotal;}
            if(ij==22 && s_rowtotal != 0.0)
               {prcnt=100.0*cum_csumy/s_rowtotal;}
            if(ynam<0)
             {
              if(ij==22 || ij==23)icode=0;
             }
            l=dep_frmt(i,j,mijk,icode,prcnt,ndv);
           }
          if(html_on>0)
           {
            strcat(xx,"<TD align=center>");
            strcat(xx,yy);   strcat(xx,"</TD>");
           }
          else
           {
            ix1+=mijk; ix1++;
            l=strlen(yy);
            strcpy(xx+ix1-l,yy);
            if(j<lasti-1 || k<row_stat-1)
             { xx[ix1]=' '; }
           }
         }   /*   end of loop for statistics in cols   */
       }     /*   end of loop for columns              */
      if(html_on>0)strcat(xx,"<TR ALIGN=RIGHT>");
      foutput(0);
     }      /*   end of loop for statistics in rows   */
    if(row_ways<=1)total_row=0;
    if(row_vbltype>1)total_row=0;
    if(col_stat>1 || total_row==1)
     {
      strcpy(xx," ");foutput(0);
     }
    }       /*   end of loop for rows                 */
   if(html_on>0)
    { strcpy(xx,"</TABLE></CENTER></P>"); foutput(0);}
   if(lasti>=col_lngth)return(1);
   firsti=lasti;
   strcpy(xx," ");foutput(0);
   goto nextsubtabl;
  }

static int mns_labcpy(int k,int j,int width,int i_col)
 {
  int j24,l,los,los9,ii,wuse; // ival,i1,i,ij,
  char ccapu[36],frmtapu[36];
//  char ccapu2[36];
/*                                          */

  los=k; // i1=0;
  linspace(ccapu,35);
  if(los==9999)
   {
    los9=strlen(ltotal);
    if(los9>label_length)los9=label_length;
    strncpy(ccapu,ltotal,los9); ccapu[los9]='\0';
   }
  else
   {
    if(j>0)los+=j*maxn;
    los9=label_length1*los;
    strncpy(ccapu,labls+los9,label_length);ccapu[label_length]='\0';
   }
  l=strlen(ccapu);
  ii=strncmp(ccapu,"                                      ",l);
  j24=j*36;
  strcpy(frmtapu,grp_fmt+j24);
  if(ii==0 || l==0)
   {
    fconv(values[los],frmtapu,ccapu); // ival=(int)
   }
  wuse=move_str(yy,ccapu,width,i_col);
  if(html_on>0)j=replace_aakkos(&yy[0]);
  return(wuse);
 }

static int mns_nest_labcpy(char zz[],int k,int j,int width,int i_col,int outermost)
 {
  int j24,l,los,los9,ii,wuse; // ,ival,i1;
  char ccapu[36],frmtapu[36];
/*                                          */
  los=k; // i1=0;
  linspace(ccapu,35);
  if(strlen(ltotal)>label_length)ltotal[label_length]='\0';
  if(los==9999)
   {
    strcpy(ccapu,ltotal);
   }
  if(los>9999 && outermost==0)
   {
    strcpy(ccapu,ltotal);
   }
  if(los>9999 && outermost==1)
   {
    strcpy(ccapu,former_outlab);
   }
  if(los<9999)
   {
    if(j>0)los+=j*nested_maxn;
    los9=label_length1*los;
    strncpy(ccapu,labels_nested+los9,label_length);
    ccapu[label_length]='\0';
   }
  l=strlen(ccapu);
  ii=strncmp(ccapu,"                                      ",l);
  j24=j*36;
  strcpy(frmtapu,gnest_fmt+j24);
  if(ii==0 || l==0)
   {
    fconv(values_nested[los],frmtapu,ccapu); // ival=(int)
   }
  ii=replace_underscore(ccapu);
  if(html_on>0)ii=replace_aakkos(ccapu);
  wuse=move_str(zz,ccapu,width,i_col);
  return(wuse);
 }

/* ijcellst.c  */
/*  computing basic statistics for the given cell MPK 27.8.1989 */
static int ijth_cellstat(long *nobs,double *sumy,double *ssumy,double *ymin,double *ymax,
     long *n_grandtotal,double *s_grandtotal,long *n_coltotal,long *n_rowtotal,double *s_coltotal,
     double *s_rowtotal,long *n_tabletotal,double *s_tabletotal,
     int ithrow,int jthcol,int ind_oldrow,int *ynam)
   {
     int gnam[8],gseqs[8],i_colseqs,i_rowseqs;
     int i,j;
/*                                                             */
    *ynam=-1;
    if(col_vbltype>1)
     *ynam=col_objvbles[jthcol];
    if(row_vbltype>1)
     *ynam=row_objvbles[ithrow];
    j=-1;
    for(i=0;i<8;i++){gnam[i]=-1; gseqs[i]=-1;}
    for(i=0;i<col_ways;i++)
     {
      if(col_gvbles[jthcol+i*col_lngth]>=0)
       {
        j++;
        gnam[j]=col_gvbles[jthcol+i*col_lngth];
        gseqs[j]=col_symbols[jthcol+i*col_lngth];
       }
     }
    i_colseqs=j;
    for(i=0;i<row_ways;i++)
     {
      if(row_gvbles[ithrow+i*row_lngth]>=0)
       {
        j++;
        gnam[j]=row_gvbles[ithrow+i*row_lngth];
        gseqs[j]=row_symbols[ithrow+i*row_lngth];
       }
     }
    i_rowseqs=j;
    if(ind_oldrow==1)
     {
      *nobs=lstarea[jthcol];
      *n_grandtotal=lstarea[jthcol+statspacesize];
      *n_coltotal=lstarea[jthcol+statspacesize*2];
      *n_rowtotal=lstarea[jthcol+statspacesize*3];
      *n_tabletotal=lstarea[jthcol+statspacesize*4];
      cum_rnobs=lstarea[jthcol+statspacesize*5];
      cum_cnobs=lstarea[jthcol+statspacesize*6];
      *sumy=dstarea[jthcol];
      *ssumy=dstarea[jthcol+statspacesize];
      *ymin=dstarea[jthcol+statspacesize*2];
      *ymax=dstarea[jthcol+statspacesize*3];
      *s_grandtotal=dstarea[jthcol+statspacesize*4];
      *s_coltotal=dstarea[jthcol+statspacesize*5];
      *s_rowtotal=dstarea[jthcol+statspacesize*6];
      *s_tabletotal=dstarea[jthcol+statspacesize*7];
      if(cumulative_results>0)
       {
        cum_rsumy=dstarea[jthcol+statspacesize*8];
        cum_csumy=dstarea[jthcol+statspacesize*9];
        cum_rymin=dstarea[jthcol+statspacesize*10];
        cum_rymax=dstarea[jthcol+statspacesize*11];
        cum_cymin=dstarea[jthcol+statspacesize*12];
        cum_cymax=dstarea[jthcol+statspacesize*13];
       }
     }
    else
     {
      i=cellstat(nobs,sumy,ssumy,*ynam,gnam,gseqs,ymin,ymax,
               n_grandtotal,s_grandtotal,n_coltotal,n_rowtotal,
               s_coltotal,s_rowtotal,n_tabletotal,
               s_tabletotal,i_colseqs,i_rowseqs);
      lstarea[jthcol]=*nobs;
      lstarea[jthcol+statspacesize]=*n_grandtotal;
      lstarea[jthcol+statspacesize*2]=*n_coltotal;
      lstarea[jthcol+statspacesize*3]=*n_rowtotal;
      lstarea[jthcol+statspacesize*4]=*n_tabletotal;
      lstarea[jthcol+statspacesize*5]=cum_rnobs;
      lstarea[jthcol+statspacesize*6]=cum_cnobs;
      dstarea[jthcol]=*sumy;
      dstarea[jthcol+statspacesize]=*ssumy;
      dstarea[jthcol+statspacesize*2]=*ymin;
      dstarea[jthcol+statspacesize*3]=*ymax;
      dstarea[jthcol+statspacesize*4]=*s_grandtotal;
      dstarea[jthcol+statspacesize*5]=*s_coltotal;
      dstarea[jthcol+statspacesize*6]=*s_rowtotal;
      dstarea[jthcol+statspacesize*7]=*s_tabletotal;
      if(cumulative_results>0)
       {
        dstarea[jthcol+statspacesize*8]=cum_rsumy;
        dstarea[jthcol+statspacesize*9]=cum_csumy;
        dstarea[jthcol+statspacesize*10]=cum_rymin;
        dstarea[jthcol+statspacesize*11]=cum_rymax;
        dstarea[jthcol+statspacesize*12]=cum_cymin;
        dstarea[jthcol+statspacesize*13]=cum_cymax;
       }
     }
    return(1);
   }
/*  computing basic statistics for the given cell MPK 27.8.1989 */
static int cellstat(long *nobs,double *sumy,double *ssumy,
		int ynam,int gnam[],int gseqs[],double *ymin,double *ymax,
     	long *n_grandtotal,double *s_grandtotal,long *n_coltotal,long *n_rowtotal,double *s_coltotal,
     	double *s_rowtotal,long *n_tabletotal,double *s_tabletotal,
     	int i_colseqs,int i_rowseqs)
/*
 int cellstat(nobs,sumy,ssumy,ynam,gnam,gseqs,ymin,ymax,
              n_grandtotal,s_grandtotal,n_coltotal,n_rowtotal,
              s_coltotal,s_rowtotal,n_tabletotal,s_tabletotal,
              i_colseqs,i_rowseqs)
 long *nobs,*n_grandtotal,*n_coltotal,*n_rowtotal,*n_tabletotal;
 double *sumy,*ssumy,*ymin,*ymax,*s_grandtotal,*s_coltotal,*s_rowtotal;
 double *s_tabletotal;
 int ynam,gnam[],gseqs[],i_colseqs,i_rowseqs;
*/ 
   {
     int i,j,jj,k,ndk,ind[8],required[8],iyseq,ijk,i_gtot;
     int rc_cols[9];
     int i_nest,nestaddr,notsame,i_col,i_row;
/*                                                             */
     *sumy=0.0; *ssumy=0.0; *nobs=0;
     *ymin=-9876.5; *ymax=-9876.6;
     *n_grandtotal=0; *s_grandtotal=0.0;
     cum_cnobs=0; cum_rnobs=0; cum_csumy=0.0; cum_rsumy=0.0;
     cum_cymin=-9876.5; cum_cymax=-9876.6;
     cum_rymin=-9876.5; cum_rymax=-9876.6;
     iyseq=0;
     if(ny>0 && ynam>=0)
      {
       for(i=0;i<ny;i++)
        {
         if(ynam==iy[i])break;
        }
       iyseq=i;
      }
     if(ngv==0)
      {
       if(ivariables==0)
         *nobs=lkm[iyseq];
       else
         {
          *nobs=lkm[iyseq*2+1];
          *n_coltotal=lkm[iyseq*2];
          *n_rowtotal=*nobs;
          *n_tabletotal=*n_coltotal;
         }
       if(ny>0 && ivariables==0)
        {
         *sumy=sz[iyseq];
         *ssumy=szy[iyseq];
         *ymin=min_yvals[iyseq];
         *ymax=max_yvals[iyseq];
        }
       return(1);
      }
     if(cumulative_results==1)
      {
       for(i=0;i<9;i++){rc_cols[i]=0;}
       for(k=0;k<ngv;k++)
        {
         if(gnam[i_colseqs]==igv[k])break;
        }
       rc_cols[k]=1;
       for(k=0;k<ngv;k++)
        {
         if(gnam[i_rowseqs]==igv[k])break;
        }
       rc_cols[k]+=2;
      }
     for(i=0;i<ngv;i++){required[i]=-1;}
     for(i=0;i<ngv;i++)
      {
       if(gnam[i]<0)continue;
       for(j=0;j<ngv;j++)
        {
         if(gnam[i]==igv[j])break;
        }
       required[j]=gseqs[i];
      }
   if(percents_results==0)goto overprcnts;
/*    percents                                                       */
   if(prntopt[9]>0 || prntopt[10]>0 || prntopt[20]>0 || prntopt[22]>0)
    {
     i=rc_pros(iyseq,gnam,i_colseqs,i_rowseqs,required,n_coltotal,
       s_coltotal,1);
    }
   if(prntopt[11]>0 || prntopt[12]>0 || prntopt[19]>0 || prntopt[21]>0)
    {
     i=rc_pros(iyseq,gnam,i_colseqs,i_rowseqs,required,n_rowtotal,
       s_rowtotal,0);
    }
   if(prntopt[13]>0 || prntopt[14]>0)
    {
     i=rc_pros(iyseq,gnam,i_colseqs,i_rowseqs,required,n_tabletotal,
       s_tabletotal,2);
    }
overprcnts: for(j=0;j<ntlo;j++)
      {
       jj=j; notsame=0; i_row=0; i_col=0;
       for (k=0;k<pway;k++)
        {
         ndk=nd[k]+1;
         ind[k]=jj % ndk;
         jj=jj/ndk;
        }
/*          check first for the right subtable       */
       for(k=0;k<pway;k++)
        {
         if(igv_used[k]==0)
          {
           if(ind[k] != ik[k])goto rejected;
          }
        }
       for(k=0;k<pway;k++)
        {
         if(ind[k]<nd[k] && required[k]==9999)continue;
         if(required[k]>9999)
          {
           i_nest=required[k]-10000;
           nestaddr=address_nested[k*pway_nested]*nested_maxn;
           if(i_nest==0)
            {
             if(ind[k]<change_points[nestaddr+i_nest])
               continue;
             else
               goto rejected;
            }
           if(i_nest>0)
            {
             if(ind[k]>=change_points[nestaddr+i_nest-1] &&
                ind[k]<change_points[nestaddr+i_nest])
              continue;
             else
              goto rejected;
            }
          }
         if(required[k]==-1)continue;
         if(required[k] != ind[k])
          {
           notsame++;
           if(cumulative_results==0)goto rejected;
           if(notsame>1)goto rejected;
           if(rc_cols[k]<1)goto rejected;
           if(rc_cols[k]==1 || rc_cols[k]==3)
            {
             if(required[k]>=ind[k])i_col=1;
            }
           if(rc_cols[k]==2 || rc_cols[k]==3)
            {
             if(required[k]>=ind[k])i_row=1;
            }
           if(i_col==0 && i_row==0)goto rejected;
          }
        }
       if(ny>0)
        {
         i_gtot=0;
         if(ynam>=0)i_gtot=iyseq;
         if(ivariables==0)
          {
           if(ynam>=0)
             ijk=iyseq+j*ny;
           else
             ijk=j*ny;
          }
         else
          {
           ijk=2*(iyseq+j*ny)+1;
          }
        }
       else
        { ijk=j; i_gtot=0; }
       if(notsame==0)
        {
         *nobs+=lkm[ijk];
         *n_grandtotal=total_lkm[i_gtot];
         if(ny>0 && ivariables==0 && ynam>=0)
          {
           *sumy+=sz[ijk]; *s_grandtotal=total_sums[i_gtot];
           *ssumy+=szy[ijk];
           if(min_yvals[ijk]<=max_yvals[ijk])
            {
             if(*ymin>*ymax)
              {
               *ymin=min_yvals[ijk];
               *ymax=max_yvals[ijk];
              }
             if(*ymin>min_yvals[ijk])*ymin=min_yvals[ijk];
             if(*ymax<max_yvals[ijk])*ymax=max_yvals[ijk];
            }
          }
        }
       if(cumulative_results>0)
        {
         if(notsame==0){i_col=1;i_row=1;}
         if(i_col==1)cum_cnobs+=lkm[ijk];
         if(i_row==1)cum_rnobs+=lkm[ijk];
         if(ny>0 && ivariables==0 && ynam>=0)
          {
           if(i_col==1)cum_csumy+=sz[ijk];
           if(i_row==1)cum_rsumy+=sz[ijk];
           if(i_col==1 && min_yvals[ijk]<=max_yvals[ijk])
            {
             if(cum_cymin>cum_cymax)
              {
               cum_cymin=min_yvals[ijk];
               cum_cymax=max_yvals[ijk];
              }
             if(cum_cymin>min_yvals[ijk])cum_cymin=min_yvals[ijk];
             if(cum_cymax<max_yvals[ijk])cum_cymax=max_yvals[ijk];
            }
           if(i_row==1 && min_yvals[ijk]<=max_yvals[ijk])
            {
             if(cum_rymin>cum_rymax)
              {
               cum_rymin=min_yvals[ijk];
               cum_rymax=max_yvals[ijk];
              }
             if(cum_rymin>min_yvals[ijk])cum_rymin=min_yvals[ijk];
             if(cum_rymax<max_yvals[ijk])cum_rymax=max_yvals[ijk];
            }
          }
        }
    rejected: ;
       }
    return(1);
   }
/*  computing basic statistics for the given cell MPK 27.8.1989 */
static int stat_space(int sze)
  {
/* RS CHA
   if(p_statspacel != NULL)muste_free(p_statspacel);
   if(p_statspaced != NULL)muste_free(p_statspaced);
   p_statspacel=(char *)muste_malloc(7*sze*sizeof(long));
   if(p_statspacel==NULL)goto notenough;
   lstarea=(long *)p_statspacel;
   p_statspaced=(char *)muste_malloc(14*sze*sizeof(double));
   if(p_statspaced==NULL)goto notenough;
   dstarea=(double *)p_statspaced;
   return(1);
*/   
   p_statspacel=(char *)muste_realloc(p_statspacel,7*sze*sizeof(long));
   if(p_statspacel==NULL)goto notenough;
   lstarea=(long *)p_statspacel;
   p_statspaced=(char *)muste_realloc(p_statspaced,14*sze*sizeof(double));
   if(p_statspaced==NULL)goto notenough;
   dstarea=(double *)p_statspaced;
   return(1);
notenough: sprintf(sbuf,"\nNot enough memory"); sur_print(sbuf);
   WAIT; return(-1);
 }
/* rc_pros.c  */
/*  computing divisors for row, col and table percentages              */
static int rc_pros(int iyseq,int gnam[],int i_colseqs,int i_rowseqs,int required[],long *nobsdiv,
             double *divsum,int indcol)
   {
    int i,j,jj,k,ndk,ind[8],req[8],ijk,indprs;
/*  indcol= 0 row, 1 for col and 2 for table percentages              */
  *nobsdiv=0L; *divsum=0.0;
  for(j=0;j<8;j++)
   {
    req[j]=required[j];
   }
  indprs=1;
  if(ivariables==1)
   {
    indprs=0;
    if(row_vbltype>1 && indcol != 1)indprs=1;
    if(col_vbltype>1 && indcol != 0)indprs=1;
   }
  if(indprs==1)
   {
    if(indcol==0 || indcol==2)
     {
      for(i=0;i<=i_colseqs;i++)
       {
        for(k=0;k<ngv;k++)
         {
          if(gnam[i]==igv[k])break;
         }
        req[k]=9999;
       }
     }
    if(indcol==1 || indcol==2)
     {
      for(i=i_colseqs+1;i<=i_rowseqs;i++)
       {
        for(k=0;k<ngv;k++)
         {
          if(gnam[i]==igv[k])break;
         }
        req[k]=9999;
       }
     }
   }
  for(j=0;j<ntlo;j++)
      {
       jj=j;
       for (k=0;k<pway;k++)
        {
         ndk=nd[k]+1;
         ind[k]=jj % ndk;
         jj=jj/ndk;
        }
/*          check first for the right subtable       */
       for(k=0;k<pway;k++)
        {
         if(igv_used[k]==0)
          {
           if(ind[k] != ik[k])goto rejected;
          }
        }
       for(k=0;k<pway;k++)
        {
         if(ind[k]<nd[k] && req[k]==9999)continue;
         if(req[k]==-1)continue;
         if(req[k] != ind[k])goto rejected;
        }
       if(ny>0)
        {
         if(ivariables==0)
          ijk=iyseq+j*ny;
         else
          {
           ijk=2*(iyseq+j*ny);
           if(row_vbltype>1 && indcol != 1)ijk++;
           if(col_vbltype>1 && indcol != 0)ijk++;
          }
        }
       else
        { ijk=j; }
       *nobsdiv+=lkm[ijk];
       if(ny>0 && ivariables==0)
        {
         *divsum+=sz[ijk];
        }
    rejected: ;
       }
    return(1);
   }

/* getnxtw.c   */
/*   finds next word from source(fchar:lchar)      */
static int get_next_word(char *result,char *source,int fchar,int *lchar,int maxlen)
 {
  int i,j;
  i=fchar-1;
  nextchar: i++;
  if(i>=maxlen)goto toend;
  if(source[i]=='*')goto found;
  if(source[i]==',')goto found;
  if(source[i]=='(')goto found;
  if(source[i]==')')goto found;
  if(source[i]==' ')goto toend;
  goto nextchar;
found: if(i==fchar)
   {
    strncpy(result,source+fchar,1);
    result[1]='\0'; *lchar=i;
   }
  else
   {
    j=i-fchar;
    strncpy(result,source+fchar,j);
    result[j]='\0'; *lchar=i-1;
   }
  return(1);
toend: if(i<=fchar)
   {
    *lchar=0;
    return(0);
   }
  goto found;
 }
/* move_str.c   */
static int move_str(char *reslt,char *source,int width,int i_col)
 {
  int i,j,l,wuse,i1,i2; // ij,
  char ccapu[33],ccapu2[33];
/*                                          */
  i1=0;
  linspace(ccapu,32);
  strcpy(ccapu,source);
  l=strlen(ccapu);
  wuse=width; if(width<1)wuse=8;
  for(i=0;i<l;i++)
   {
    if(ccapu[i] != ' '){i1=i;break;}
   }
  i2=l;
  for(i=i1;i<l;i++)
   {
    if(ccapu[i] == ' '){i2=i; break;}
   }
  if(i_col==0)
   {
    ccapu[l]=' ';
    ccapu[wuse+i1]='\0';
    strcpy(reslt,ccapu+i1);
   }
  else
   {
    linspace(ccapu2,32);
    i=i2-i1;
    j=wuse-i;
    if(j>=0)
     {
      strncpy(ccapu2+wuse+i1-i2,ccapu+i1,i);
     }
    else
     {
      strncpy(ccapu2,ccapu+i1,i);
     }
    l=strlen(ccapu2);
    ccapu2[l]=' ';
    ccapu2[wuse]='\0';
    strcpy(reslt,ccapu2);
   }
  return(wuse);
 }
/* getcolw.c  */
static int get_colwidth(int i)
 {
  int j=0; // ,k,mijk;
  if(col_vbltype>1)
   {
    j=get_column_width(i,i,0,1,1);
   }
  if(row_vbltype>1)
   {
    j=get_column_width(0,row_lngth-1,0,0,1);
   }
  if(col_vbltype==1 && row_vbltype==1)
   {
    j=get_column_width(i,i,0,1,0);
   }
  if(j<1)j=8;
  return(j);
 }
/* getwdth.c  */
/*                                                              */
/*  get column width                                            */
/*                                                              */
static int get_column_width(int findx,int lindx,int index2,int i_col,int i_y)
 {
  int i,index,ijk,i2y,k=0,m,wdth=0,nestaddr;
  if(ny==0 && ngv==0)return(8);
  ijk=0;
  for(index=findx;index<=lindx;index++)
   {
    if(i_y==1)
     {
      if(i_col==1)k=col_objvbles[index];
      if(i_col==0)k=row_objvbles[index];
      if(k<0){wdth=-1;goto nextj;}
      for(i2y=0;i2y<ny;i2y++)
       {
        if(iy[i2y]==k)break;
       }
  /*
      for(i=0;i<ngv;i++)
       {
        if(igv[i]==k)break;
       }
      if(nested[i]>0)
       {
        wdth=0;
        for(m=0;m<pway_nested;m++)
         {
          nestaddr=address_nested[m+i*pway_nested];
          if(nestaddr<0)break;
          if(i_col==0)
           {
            wdth+=nest_width[nestaddr];
            if(m>0)wdth++;
           }
          else
           {
            if(nest_width[nestaddr]>wdth)
              wdth=nest_width[nestaddr];
           }
         }
       }
      else
       {
        wdth=grp_width[i];
       }
   //  if(wdth>y_width[i2y])return(wdth);        */
      if(y_width[i2y]<1)
       {wdth=-1;goto nextj;}
      wdth=y_width[i2y];
     }
    if(i_y==0)
     {
      if(i_col==1)k=col_gvbles[index+index2*col_lngth];
      if(i_col==0)k=row_gvbles[index+index2*row_lngth];
      if(k<0){wdth=-1;goto nextj;}
      for(i=0;i<ngv;i++)
       {
        if(igv[i]==k)break;
       }
      if(nested[i]>0)
       {
/*   if a nested vble is a column vble (i_col==1) then labels are printed  */
/*   on separate rows --> the width of a column is the maximum of lengths  */
/*   if a nested vble is as a row vble then the width is the sum of lengths */
        wdth=0;
        for(m=0;m<pway_nested;m++)
         {
          nestaddr=address_nested[m+i*pway_nested];
          if(nestaddr<0)break;
          if(i_col==0)
           {
            wdth+=nest_width[nestaddr];
            if(m>0)wdth++;
           }
          else
           {
            if(nest_width[nestaddr]>wdth)
              wdth=nest_width[nestaddr];
           }
         }
       }
      else
       {
        wdth=grp_width[i];
       }
     }
nextj: if(wdth>ijk)ijk=wdth;
   }
  if(ijk<1)ijk=8;
  return(ijk);
 }

/*   compnest.c  */
/*                                                                 */
/*  find seq. number for a nested classification                   */
/*  xreal  value vector to be searched                             */
/*  xchrval character vector to be searched                        */
static int comp_xval_nested(int j,double *xreal,char *xchrval)
 {
  double valx;
  int ifound,mm,i,ink;
  int ndj,valos,impn,iaddr;
/*                                                              */
  ndj=nd[j];
  ifound=0;
  for(ink=0;ink<ndj;ink++)
   {
    for(mm=0;mm<pway_nested;mm++)
     {
      impn=pway_nested*j+mm;
      iaddr=address_nested[impn];
      if(iaddr<0)break;
      if(igv_vartype_nested[iaddr]<=0)
       {
        valos=ink+iaddr*nested_maxn;
        if(cutp_nested[iaddr]==1)
         {
          if(xreal[mm]<=values_nested[valos]+xvert[iaddr])goto labfnd;
         }
        else
         {
          valx=values_nested[valos]-xreal[mm]; valx=fabs(valx);
          if(valx<xvert[iaddr])goto labfnd;
         }
       }
      else
       {
        valos=label_length1*(ink+iaddr*nested_maxn);
        i=(int)strncmp(labels_nested+valos,xchrval+mm*33,label_length);
        if(i==0)goto labfnd;
       }
      ifound=0; break;   /* next class  */
labfnd: ifound=1;
     }    /* end of loop for mm   (ways)      */
    if(ifound==1)break;
   }      /* end of loop for ink  (classes)   */
  return(ink);
}
/*                                                                 */
/*  collecting cell numbers, sums and crossproducts                */
/*                                                                 */
static void comp_xval_xvert(double *xvert)
 {
  double valx;
  int j,jmaxn,indalpha;
/*                                                              */
  for(j=0;j<pway;j++)
   {
    if(nested[j]>0)continue;
    indalpha=0; jmaxn=j*maxn;
    if(igv_vartype[j]>0)indalpha=1;
    if(indalpha==0)
     {
      valx=0.0000005;
      if(values[jmaxn] != 0.0)
       {
        valx=values[jmaxn]*valx;
        valx=fabs(valx);
       }
      xvert[j]=valx;
     }
   }
  return;
 }
/*                                                                 */
/*  collecting cell numbers, sums and crossproducts                */
/*                                                                 */
static int comp_xval(int j,double xval,int ndj,int jmaxn)
 {
  double valx;
  int i,imk;
  int valos;
/*                                                              */
  if(igv_vartype[j]<=0)
   {
    for(imk=0;imk<ndj;imk++)
     {
      valos=imk+jmaxn;
      if(cutp[j]==1)
       {
        if(xval<=values[valos]+gxvert[j])goto labfnd;
       }
      else
       {
        valx=values[valos]-xval; valx=fabs(valx);
        if(valx<gxvert[j])goto labfnd;
       }
     }
   }
  else
   {
     for(imk=0;imk<ndj;imk++)
      {
       valos=label_length1*(imk+j*maxn);
       i=(int)strncmp(labls+valos,xx,label_length);
       if(i==0)goto labfnd;
      }
    }
   imk=ndj;
labfnd: return(imk);
}
/*                                                                 */
  void comp_nest_xvert(xvert)
  double *xvert;
   {
    double valx;
    int j,mm;
    int impn,iaddr;
/*                                                              */
    for(j=0;j<pway;j++)
     {
      if(nested[j]==0)continue;
      for(mm=0;mm<pway_nested;mm++)
       {
        impn=pway_nested*j+mm;
        iaddr=address_nested[impn];
        if(iaddr<0)break;
        if(igv_vartype_nested[iaddr]<=0)
         {
          valx=0.0000005;
          if(values_nested[iaddr*nested_maxn] != 0.0)
           {
            valx=values_nested[iaddr*nested_maxn]*valx;
            valx=fabs(valx);
           }
          xvert[iaddr]=valx;
         }
       }
     }
    return;
   }

/*  valsort2.c   */
static void valsort2(double *xarr,char *arr,int n)
 {
  int i,j; // ,l;
  double x1; char cccc[36];
  for(i=0;i<n-1;i++)
   {
    for(j=i+1;j<n;j++)
     {
      if(xarr[j]<xarr[i])
       {
        strncpy(cccc,arr+i*label_length1,label_length);
        strncpy(arr+i*label_length1,arr+j*label_length1,label_length);
        strncpy(arr+j*label_length1,cccc,label_length);
        x1=xarr[i]; xarr[i]=xarr[j]; xarr[j]=x1;
       }
     }
   }
  return;
 }
static int blnkaway(char *zz,char *yy)
 {
  int i,j,l,pit;
  l=strlen(zz);if(l<=0)return(l);
/* first nonblank char from the beginning  */
  for(j=0;j<l;j++)
   {
    if(zz[j] != ' ')goto nonblank;
   }
  return(0);
/* first nonblank char from the beginning is j */
nonblank: for(i=l-1;i>=j;i--)
      { if(zz[i] != ' ')break; }
      i++;
/* last nonblank char  is i-1  */
  linspace(yy,24); pit=i-j;
  strcpy(yy,zz+j); yy[pit]='\0';
  return(pit);
 }

/*   dep_frmt.c    */
int dep_frmt(int ithrow,int jthcol,int width,int i_code,double xval,long longval)
 {
/*        i_code = 0   write - for no observations     */
/*                 1   input xval                      */
/*                 2   input longval                   */
/*                 3   input precentages write with    */
  int i,i24,l,ifmt,ynam,l1; // ival,j,
  char ccapu[36],ccapu2[36],fmtapu[36];
/*                                          */
  ynam=-1; ifmt=0; l=0;
  if(i_code != 3)
   {
    if(ny<1 || i_code==2)goto lkmonly;
    if(col_vbltype>1)
      ynam=col_objvbles[jthcol];
    if(row_vbltype>1)
      ynam=row_objvbles[ithrow];
    for(i=0;i<ny;i++)
     {
      if(iy[i]==ynam)break;
     }
    if(ynam>=0)
     {
      i24=i*36;
      strcpy(fmtapu,y_fmt+i24);
      l=strlen(fmtapu);
      ifmt=(int)muste_strcmpi(fmtapu,"def");
     }
   }
  if(i_code==3 && longval==0L)i_code=0;
  if(i_code==0)
   {
    linspace(ccapu,35);
    if(ifmt==0)
     {
      l1=width; if(l1<1)l1=8;
      ccapu[l1-1]='-'; ccapu[l1]='\0';
     }
    else
     {
      ccapu[l-1]='-'; ccapu[l]='\0';
     }
    strcpy(yy,ccapu);
    l=strlen(yy);
    return(l);
   }
  if(ifmt==0 && (i_code==1 || i_code==3))
   {
/*          no format given                 */
    l1=width; if(l1<8)l1=8;
    linspace(ccapu,35);
    if(i_code==1)
     fconv(xval,"1234.123",ccapu);
    else
     {
      l1=width;
      if(l1<5)fconv(xval,"1234",ccapu);
      if(l1==5)
       {
        if(pctdec>0)
          fconv(xval,"123.1",ccapu);
        else
          fconv(xval,"12345",ccapu);
       }
      if(l1>=6)
       {
        if(pctdec==0)fconv(xval,"123456",ccapu);
        if(pctdec==1)fconv(xval,"1234.1",ccapu);
        if(pctdec==2 || (pctdec>2 && l1<7))fconv(xval,"123.12",ccapu);
        if(pctdec==3 || (pctdec>3 && l1==7))fconv(xval,"123.123",ccapu);
        if(pctdec==4 || (pctdec>4 && l1==8))fconv(xval,"123.1234",ccapu);
        if(pctdec==5 || (pctdec>5 && l1==9))fconv(xval,"123.12345",ccapu);
       }
     }
    if(l1>8)
     {
      linspace(ccapu2,l1);
      strcpy(ccapu2+l1-8,ccapu);
      ccapu2[l1]='\0';
      strcpy(ccapu,ccapu2);
     }
    strcpy(yy,ccapu);
    l=strlen(yy); return(l);
   }
  if(ifmt != 0)
   {
    if(i_code==1 || i_code==3)
     {
      fconv(xval,fmtapu,yy); // ival=(int)
     }
   }
lkmonly:  if(i_code==2)
   {
    muste_ltoa(longval,yy,10);
   }
  l=strlen(yy); return(l);
 }

