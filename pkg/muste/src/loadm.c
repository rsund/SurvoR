#include "muste.h"
/* _loadm.c 4.11.1986/SM (24.11.1987) (26.12.1998)
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define C_WIDTH 32

static double *A;
static int m,n;
static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];

// SHADOW_MATRIX 24.6.2007
static double *B;
static int mb,nb;
static char *rlabb,*clabb;
static int lrb,lcb;
static int typeb;
static char exprb[129];

static int resline;
static int prind=1;
static char *form_tila;
static char **c_form;
static char form[LLENGTH];
static int colwidth;
static int *first_var;
static int data_names;
static SURVO_DATA dat;
static double *sum2;
static int one_block;
static int *o,*mv,*nv;

static double *s_sum2;
static int *s_max;
static double *vv;
static double *A2;
static char *rlab2;
// RS REM static char sort_param[LNAME];
static char *s_word[2];
static int *o2;

/* RS REM
static char *specs0[]={ "SORT", "ROWS", "COLUMNS", "LIMITS", "SHADOWS",
                 "PRIND", "WIDE", "SUMS", "POSDIR", "!" };
static char **specs=specs0;
*/

static int row_sort();
static int write_sums(int k,int *o,int *oc);
static int write_long_names();
static int indicate_first_vars();
static int line_error();
static int posdir(double *A,int n,int m);
static int not_enough_memory();
static void op_posdir();
static void posdir_sqrsum(double *A,int n,int m);
static void posdir_max(double *A,int n,int m);
static void posdir_maxcases(double *A,int n,int m);

void muste_loadm(int argc, char *argv[])
        {
        int i,j,k;
        int m2,n2;
        int *pmv,*pnv;
        char x[LLENGTH], *osa[EP4];
        char *p;
        int nlimit;
        double limit[32];
        char shadow[32];
        double a;
// RS REM        int width;

        double *lim_or_sha; // 24.6.2007

// RS Variables init 19.11.2012
A=NULL;
m=n=0;
rlab=NULL;
clab=NULL;
lr=lc=0;
type=0;
//static char expr[129];
B=NULL;
mb=nb=0;
rlabb=NULL;
clabb=NULL;
lrb=lcb=0;
typeb=0;
//static char exprb[129];
resline=0;
prind=1;
form_tila=NULL;
c_form=NULL;
//static char form[LLENGTH];
colwidth=0;
first_var=NULL;
data_names=0;
//static SURVO_DATA dat;
sum2=NULL;
one_block=0;
o=NULL;
mv=NULL;
nv=NULL;
s_sum2=NULL;
s_max=NULL;
vv=NULL;
A2=NULL;
rlab2=NULL;
//char *s_word[2];
o2=NULL;

        if (argc==1) return;
        s_init(argv[1]);
        if (muste_strcmpi(word[0],"POSDIR")==0) { op_posdir(); s_end(argv[1]); return; }

        sp_init(r1+r-1);

        if (g<3)
            {
            init_remarks();
            rem_pr("Usage: LOADM A,format,L");
            wait_remarks(2);  s_end(argv[1]);       return;
            }
        resline=0;
        if (g==4) { resline=edline2(word[3],1,1); if (resline==0) return; }

        A=NULL; rlab=NULL; clab=NULL;
        i=matrix_load(word[1],&A,&m,&n,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return;

        posdir(A,m,n);
        first_var=NULL;

        o=(int *)muste_malloc(m*sizeof(int)); /* 9.12.1999 */
        if (o==NULL) { not_enough_memory(); return; } // RS CHA exit -> return
        mv=(int *)muste_malloc(m*sizeof(int)); /* 9.12.1999 */
        if (mv==NULL) { not_enough_memory(); return; } // RS CHA exit -> return
        nv=(int *)muste_malloc(n*sizeof(int)); /* 9.12.1999 */
        if (nv==NULL) { not_enough_memory(); return; } // RS CHA exit -> return

        for (i=0; i<m; ++i) o[i]=i;
        for (i=0; i<n; ++i) nv[i]=i;
        i=spfind("SORT");
        if (i>=0)
            {
            i=row_sort(spb[i],A,m,n,rlab,clab,lr,lc);
            if (i<0) return; // RS CHA exit -> return
            }

        for (i=0; i<m; ++i) mv[i]=o[i];
        i=spfind("ROWS");
        if (i<0) { m2=m; pmv=NULL; }
        else
            { strcpy(x,spb[i]); m2=split(x,osa,EP4);
              if (m2>m)
                  {
                  sur_print("\nError in ROWS specification!");
                  WAIT; return; // RS CHA exit -> return
                  }
              for (i=0; i<m2; ++i) mv[i]=atoi(osa[i])-1;
              pmv=mv;
            }

// RS REM        i=optdim_d(); if (i && i<m && i<n) err(0);

        i=spfind("COLUMNS");
        if (i<0) { n2=n; pnv=NULL; }
        else
            {
            strcpy(x,spb[i]); n2=split(x,osa,EP4);
            if (muste_strcmpi(osa[0],"SORT")==0)
                {
                sum2=(double *)muste_malloc(n*sizeof(double));
                if (sum2==NULL) { not_enough_memory(); return; } // RS CHA exit -> return
                for (j=0; j<n; ++j) sum2[j]=0.0;
                for (i=0; i<m; ++i)
                    for (j=0; j<n; ++j) sum2[j]+=A[i+m*j]*A[i+m*j];
                for (j=0; j<n; ++j)
                    {
                    a=-1.0; k=-1;
                    for (i=0; i<n; ++i)
                        if (sum2[i]>a) { k=i; a=sum2[i]; }
                    nv[j]=k;
                    sum2[k]=-1.0;
                    }
                n2=n;
                }
            else
                {
                if (n2>n)
                    {
                    sur_print("\nError in COLUMNS specification!");
                    WAIT; return; // RS CHA exit -> return
                    }
                for (i=0; i<n2; ++i) nv[i]=atoi(osa[i])-1;
                }
            pnv=nv;
            }

        nlimit=0;
        i=spfind("LIMITS");
        if (i>=0)
            {
            strcpy(x,spb[i]); nlimit=split(x,osa,32);
            for (i=0; i<nlimit; ++i) limit[i]=atof(osa[i]);
            i=spfind("SHADOWS");
            if (i<0) { sur_print("\nSHADOWS missing!"); WAIT; return; }
            strcpy(x,spb[i]); i=split(x,osa,32);
            if (i!=nlimit)
                {
                sprintf(sbuf,"\n%d items in LIMITS, but %d items in SHADOWS!",
                                nlimit,i); sur_print(sbuf); WAIT; return;
                }
            for (i=0; i<nlimit; ++i)
                {
                if (*osa[i]=='0') *osa[i]=' ';
                shadow[i]=*osa[i];
                }
            }

        i=spfind("SHADOW_MATRIX"); // 24.6.2007
        if (i>=0)
            {
            nlimit=-1;
            B=NULL; rlabb=NULL; clabb=NULL;
            i=matrix_load(spb[i],&B,&mb,&nb,&rlabb,&clabb,&lrb,&lcb,&typeb,exprb);
            if (i<0) return;
            }

        k=output_level;
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);
        if (prind==0) output_level=0;

        strcpy(form,word[2]);
        if (*form=='(')         /* (accuracy) */
            {
            p=strchr(form,')');
            if (p==NULL)
                {
                sprintf(sbuf,"\n) missing in format %s (accuracy)",form);
                sur_print(sbuf); WAIT; return;
                }
            *p=EOS;
            if (form[1]=='C')
                {
                colwidth=atoi(form+2);
                form_tila=muste_malloc((C_WIDTH+1)*n);
                if (form_tila==NULL) { not_enough_memory(); return; } // RS CHA exit -> return
                c_form=(char **)muste_malloc(n*sizeof(char **));
                if (c_form==NULL) { not_enough_memory(); return; } // RS CHA exit -> return
                p=form_tila;
                for (i=0; i<n; ++i)
                    {
                    c_form[i]=p;
                    matrix_format(form,colwidth,A+i*m,m,1);
                    strcpy(p,form); p+=strlen(form)+1;
                    }
                }
            else matrix_format(form,atoi(form+1),A,m,n);
            }

/*      if (!nlimit)      poistettu 26.12.1998
        matrix_print(A,m,n,rlab,clab,lr,lc,m2,n2,pmv,pnv,form,c3,resline,eout,expr);
        else
*/
        k=0;
        i=spfind("WIDE"); /* wide output */
        if (i>=0) k=atoi(spb[i]);
        if (k>0) k=c2; else k=c3;

        if (nlimit==-1) lim_or_sha=B; else lim_or_sha=limit; // 24.6.2007

        matrix_print2(A,m,n,rlab,clab,lr,lc,m2,n2,pmv,pnv,form,k,resline,eout,expr,
                        nlimit,lim_or_sha,shadow,c_form,&one_block);
        output_level=k;

        i=spfind("SUMS");
        if (i>=0 && one_block)
            {
            k=atoi(spb[i]);
            if (k<0 || k>2)
                {
                sur_print("\nPermitted alternatives:");
                sur_print("\nSUMS=0  no marginals (default)");
                sur_print("\nSUMS=1  marginal sums");
                sur_print("\nSUMS=2  marginal sums of squares");
                WAIT; return; // RS CHA exit -> return
                }
            if (k)  { i=write_sums(k,o,nv); if (i<0) return; } // RS ADD exit
            }
        if (data_names) { i=write_long_names(); if (i<0) return; } // RS ADD exit
        if (first_var!=NULL) { i=indicate_first_vars(); if (i<0) return; } // RS ADD exit
        s_end(argv[1]);
        }

static int row_sort(
char *spbi,
double *A,
int m,
int n,
char *rlab,
char *clab,
int lr,
int lc
)
        {
        int i,j,k,h;
        double a,b;
        int i0,ii,i1=0;
        int additional_negsort;
        char *pname;
        double min_loading;
        int m1;

        s_sum2=(double *)muste_malloc(n*sizeof(double));
        if (s_sum2==NULL) { not_enough_memory(); return(-1); } // RS CHA exit -> return
        s_max=(int *)muste_malloc(m*sizeof(int));
        if (s_max==NULL) { not_enough_memory(); return(-1); }
        vv=(double *)muste_malloc(m*sizeof(double));
        if (vv==NULL) { not_enough_memory(); return(-1); }
        A2=(double *)muste_malloc(m*n*sizeof(double));
        if (A2==NULL) { not_enough_memory(); return(-1); }
        rlab2=muste_malloc(m*lr);
        if (rlab2==NULL) { not_enough_memory(); return(-1); }
        o2=(int *)muste_malloc(m*sizeof(int));
        if (o2==NULL) { not_enough_memory(); return(-1); }
        first_var=(int *)muste_malloc((n+1)*sizeof(int));
        if (first_var==NULL) { not_enough_memory(); return(-1); }


        data_names=0;
        i=split(spbi,s_word,2);
        pname=s_word[0];
        min_loading=0.0; if (i>1) min_loading=atof(s_word[1]);
        additional_negsort=0;
        if (*pname=='-') { additional_negsort=1; ++pname; }
        if (strcmp(pname,"1")!=0)
            {
            i=data_open2(pname,&dat,0,1,0);
            if (i<0) return(-1);
            data_names=1;
            }

        for (j=0; j<n; ++j) s_sum2[j]=0.0;
        for (i=0; i<m; ++i)
            {
            o[i]=i; a=-1.0; k=0;
            for (j=0; j<n; ++j)
                {
                b=fabs(A[i+m*j]);
                s_sum2[j]+=b*b;
                if (b>a) { a=b; k=j; }
                }
            s_max[i]=k;
            }

         m1=m;
         if (min_loading>0.0)
             {
             k=0;
             for (i=0; i<m; ++i)
                 if (fabs(A[i+m*s_max[i]])<min_loading)
                     { o2[i]=0; ++k; } else o2[i]=1;
             if (k>0)
                 {
                 j=0; h=0;
                 for (i=0; i<m; ++i)
                     if (o2[i]) o[j++]=i;
                     else { o[m-k+h]=i; ++h; }
                 }
             m1=m-k;
             }


        i0=0; first_var[0]=0;
        for (j=0; j<n; ++j)
            {
            k=0; a=-1.0;
            for (i=0; i<n; ++i)
                if (s_sum2[i]>a) { k=i; a=s_sum2[i]; }
            s_sum2[k]=0.0;

            for (i=i0; i<m1; ++i) vv[o[i]]=fabs(A[o[i]+m*k]);
            for (i=i0; i<m1; ++i)
                {
                a=-1.0; h=-1;
                for (ii=i; ii<m1; ++ii)
                    {
                    if (vv[o[ii]]>=a) { a=vv[o[ii]]; h=ii; }
                    }
                vv[o[h]]=-1000;
                ii=o[i]; o[i]=o[h]; o[h]=ii;
                }
            for (i=i0; i<m1; ++i)
                {
                if (s_max[o[i]]==k) continue;
                for (h=i+1; h<m1; ++h)
                    if (s_max[o[h]]==k) break;
                if (h<m1)
                    {
                    ii=o[i]; o[i]=o[h]; o[h]=ii;
                    continue;
                    }
                i1=i;
                break;
                }
            if (i==m1) i1=m1;
            if (additional_negsort)
                {
                for (i=i0; i<i1; ++i) vv[o[i]]=A[o[i]+m*k];
                for (i=i0; i<i1; ++i)
                    {
                    a=-1000.0; h=-1;
                    for (ii=i; ii<i1; ++ii)
                        {
                        b=vv[o[ii]];
                        if (b>a) { a=b; h=ii; }
                        }
                    vv[o[h]]=-1000.0;
                    ii=o[i]; o[i]=o[h]; o[h]=ii;
                    }
                }
            i0=i1; first_var[k+1]=i0;
            } /* j */
/*
printf("\n");
for (i=0; i<m; ++i) Rprintf("%d ",o[i]); Rprintf("\n"); getch();
*/
        for (i=0; i<m*n; ++i) A2[i]=A[i];
        for (i=0; i<lr*m; ++i) rlab2[i]=rlab[i];

        for (i=0; i<m; ++i)
            {
            for (j=0; j<n; ++j) A[i+m*j]=A2[o[i]+m*j];
            for (j=0; j<lr; ++j) rlab[i*lr+j]=rlab2[o[i]*lr+j];
            }

        muste_free(rlab2); muste_free(A2);
        muste_free(vv); muste_free(s_max); muste_free(s_sum2);

        return(1);
        }

static int write_sums(int k,int *o,int *oc)
        {
        int i,j,h,pos;
        double a,b;
        char x[LLENGTH];

        if (resline==0) return(1);
        if (c_form!=NULL) return(1);
        j=resline+2;
        edread(x,j);
        pos=c2; while (x[pos]==' ') --pos; pos+=2;
        if (pos>=c2) { line_error(); return(-1); }

        if (k==1) strcpy(x,"Sum"); else strcpy(x,"Sumsqr");
        edwrite(x,j-1,pos+1);

        for (i=0; i<m; ++i)
            {
            a=0.0;
            for (h=0; h<n; ++h)
                {
                b=A[i+m*oc[h]];
                if (k==1) a+=b; else a+=b*b;
                }
            fconv(a,form,x);
            edwrite(x,j+i,pos);
            }

        if (k==1) strcpy(x,"Sum"); else strcpy(x,"Sumsqr");
        edwrite(space,j+m,1);
        edwrite(x,j+m,1);
        pos=lr+2;
        for (h=0; h<n; ++h)
            {
            a=0.0;
            for (i=0; i<m; ++i)
                {
                b=A[i+m*oc[h]];
                if (k==1) a+=b; else a+=b*b;
                }
            fconv(a,form,x);
            edwrite(x,j+m,pos);
            pos+=strlen(form)+1;
            }
        edwrite(space,j+m+1,1);
        return(1);
        }

static int write_long_names()
        {
        int i,j,k,var,pos;
        char x[LLENGTH];
        char nimi[10];


        if (resline==0) return(1);
        if (dat.type!=2) return(1);
        if (dat.d2.l<10) return(1);
        j=resline+2;
        edread(x,j);
        pos=c2; while (x[pos]==' ') --pos; pos+=2;
        if (pos>=c2) { line_error(); return(-1); }

        k=0;
        for (i=0; i<m; ++i)
            {
            edread(x,j+i);
            for (k=0; k<8; ++k) nimi[k]=x[k+1]; nimi[9]=EOS;
            k=7; while (nimi[k]==' ') nimi[k--]=EOS;
            var=varfind(&dat,nimi);
            if (var<0) return(-1);
            edwrite(dat.varname[var]+9,j+i,pos);
            }
        return(1);
        }

static int indicate_first_vars()
        {
        int i,j,k,h,k1;
        char x[LLENGTH];
// RS REM        char nimi[9];

        if (resline==0) return(1);
        j=resline+2;

        if (first_var[n]<m) k1=n+1; else k1=n;
        for (k=0; k<k1; ++k)
            {
            h=j+first_var[k];
            if (zs[h]==0)
                {
                i=shadow_create(h);
                if (i<0) return(-1);
                }
            edread(x,zs[h]);
            edwrite("1",zs[h],1);
            }
        return(1);
        }

static int line_error()
        {
        sur_print("\nEdit field too narrow!");
        sur_print("\nIncrease # of columns by the REDIM command!");
        WAIT; return(-1);
        }

static int posdir(double *A,int n,int m)
        {
        int i,criterion;

        i=spfind("POSDIR");
        if (i<0) return(-1);
        criterion=atoi(spb[i]);

        switch (criterion)
            {
          case 1: posdir_sqrsum(A,n,m); break;
          case 2: posdir_max(A,n,m); break;
          case 3: posdir_maxcases(A,n,m); break;
         default: sur_print("\nPermitted POSDIR values are 1,2,3.");
                  WAIT; return(1);
            }
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(1);
        }

/* !posdir.c 2.12.1988/SM (3.12.1988)
*/

static void op_posdir()
        {
        int i;
// RS REM        char x[LLENGTH];
        int criterion;

        if (g<2)
            {
            init_remarks();
            rem_pr("POSDIR <matrix_file>,<criterion>                 / S.Mustonen 3 Dec 1988");
            rem_pr("alters the orientation of the columns of <matrix_file>");
            rem_pr("to positive direction by using <criterion>.");
            rem_pr("Possible values of <criterion> are 1,2,3:");
            rem_pr("1: (default) If sum of squares of negative elements exceeds");
            rem_pr("   that of positive ones in current column, change signs.");
            rem_pr("2: If the maximum element in current column is negative,");
            rem_pr("   change signs.");
            rem_pr("3: If most of the elements in current column are negative,");
            rem_pr("   change signs.");
            rem_pr(" ");
            rem_pr("No results are printed in the edit field.");
            wait_remarks(2);
            return;
            }
        i=matrix_load(word[1],&A,&n,&m,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0)
            {
            sprintf(sbuf,"\nMatrix file %s not found!",word[1]);
            sur_print(sbuf); WAIT; return;
            }
        criterion=1;
        if (g>2) criterion=atoi(word[2]);

        switch (criterion)
            {
          case 1: posdir_sqrsum(A,n,m); break;
          case 2: posdir_max(A,n,m); break;
          case 3: posdir_maxcases(A,n,m); break;
         default: sur_print("\nPermitted criterion values are 1,2,3.");
                  WAIT; return;
            }

        i=matrix_save(word[1],A,n,m,rlab,clab,lr,lc,-1,expr,0,0);
        }

static void posdir_sqrsum(double *A,int n,int m)
        {
        int i,j;
        double *col;
        double splus,sminus;

        for (j=0; j<m; ++j)
            {
            col=&A[j*n];
            splus=sminus=0.0;
            for (i=0; i<n; ++i)
                {
                if (col[i]>0) splus+=col[i]*col[i];
                else          sminus+=col[i]*col[i];
                }
            if (sminus>splus)
                {
                for (i=0; i<n; ++i)
                    col[i]=-col[i];
                }
            }
        }

static void posdir_max(double *A,int n,int m)
        {
        int i,j;
        double *col;
        double max;

        for (j=0; j<m; ++j)
            {
            col=&A[j*n];
            max=0.0;
            for (i=0; i<n; ++i)
                {
                if (fabs(col[i])>fabs(max)) max=col[i];
                }
            if (max<0.0)
                {
                for (i=0; i<n; ++i)
                    col[i]=-col[i];
                }
            }
        }

static void posdir_maxcases(double *A,int n,int m)
        {
        int i,j;
        double *col;
        int nplus;

        for (j=0; j<m; ++j)
            {
            col=&A[j*n];
            nplus=0;
            for (i=0; i<n; ++i)
                {
                if (col[i]>=0.0) ++nplus;
                }
            if (nplus<n-nplus)
                {
                for (i=0; i<n; ++i)
                    col[i]=-col[i];
                }
            }
        }


/* mp2.c 4.11.1986/SM (3.9.1993)
   matrix_print2()    ei MAT-operaatioihin
   varjotulostus
*/

int matrix_print2(
double *A,
int m,
int n,
char *rlab,
char *clab,
int lr,
int lc,
int m2,
int n2,        /* # of selected rows/cols */
int *mv,
int *nv,      /* lists of selected rows/cols */
char *form,       /* format for single element 123.12 or %5.5g  */
int width,        /* entire printing width */
int editline,
char *outfile,
char *header,
int nlimit,
double *limit,
char *shadow,
char **c_form,
int *p_one
)
        {
        int i,j,k,h,ii,jj;
        int len;
        int nblock;
        int i1,i2,j1,j2;
        char x[LLENGTH];
        char label[LLENGTH];
        char *matrix_label2();
        double da;
        char sh;
        char x2[LLENGTH];

        double *bb=NULL;
        
        for (i=0; i<LLENGTH; i++) { x[i]=EOS; x2[i]=EOS; } // RS 19.11.2012

        if (nlimit==-1) bb=limit; // 24.6.2007 SHADOW_MATRIX

        output_open(outfile);
        output_line(header,outfile,editline); if (editline) ++editline;

        if (*form=='%') len=sprintf(x,form,fabs(A[0]))+1;
        else len=strlen(form);

        nblock=(width-lr-1+1)/(len+1);
        if (nblock>n2) { nblock=n2; *p_one=1; } else *p_one=0;

        i1=0; i2=m2-1;
        j1=0; j2=nblock-1;

        while (1)
            {
            k=sprintf(x,"%.*s",lr+1,space);
            for (j=j1; j<=j2; ++j)
                {
                if (nv==NULL) jj=j; else jj=nv[j];
                k+=sprintf(x+k,"%*.*s ",len,len,matrix_label2(clab,lc,len,jj,label));
                }
            output_line(x,outfile,editline); if (editline) ++editline;
            for (i=i1; i<=i2; ++i)
                {
                if (mv==NULL) ii=i; else ii=mv[i];
                k=0; while (k<lr) { label[k]=rlab[ii*lr+k]; ++k; } label[lr]=EOS;
                k=sprintf(x,"%*.*s ",lr,lr,label);
                strncpy(x2,space,c2);
                for (j=j1; j<=j2; ++j)
                    {
                    if (nv==NULL) jj=j; else jj=nv[j];
                    da=A[jj*m+ii];
                    if (c_form==NULL)
                        h=fconv(da,form,label);
                    else
                        h=fconv(da,c_form[jj],label);

                    if (h<0) { strncpy(label,space,len); label[len-1]='-'; }
                    if (nlimit)
                        {
                        if (nlimit==-1)
                            {
                            h=(int)bb[jj*m+ii];
                            if (h==0) sh=' ';
                            else sh=(char)((int)'0'+h);
                            }
                        else
                            {
                            for (h=0; h<nlimit; ++h)
                                if (da<=limit[h]) break;
                            if (h==nlimit) --h; sh=shadow[h];
                            }
                        for (h=0; h<len; ++h) x2[k+h]=sh;
                        }
                    k+=sprintf(x+k,"%*.*s ",len,len,label);
                    }
                if (nlimit && editline && editline<=r2)  /* <=r2 3.9.1993 */
                    {
                    if (zs[editline]==0)
                        {
                        j=shadow_create(editline);
                        if (j<0) return(-1);
                        }
                    edwrite(x2,zs[editline],1);
                    }
                output_line(x,outfile,editline); if (editline) ++editline;
                }
            strcpy(x," ");
            output_line(x,outfile,editline); if (editline) ++editline;
            if (j2==n2-1) break;
            j1=j2+1;
            j2+=nblock; if (j2>=n2) j2=n2-1;
            }

        output_close(outfile);
        return(editline);
        }

char *matrix_label2(char *lablist,int l,int len,int i,char *label)
        {
        int k=0;
        int h=0;

        while (lablist[i*l+h]==' ' && h<l) ++h;

        while (h<l && k<len) { label[k]=lablist[i*l+h]; ++k; ++h; }
        label[k]=EOS;
        while (label[k-1]==' ' && k>0) label[--k]=EOS;
        return(label);
        }

/* mf.c 8.11.1986/SM (8.11.1986)
   kuten etsi_muoto() MAT
*/

int matrix_format(char *muoto,int minlev,double *A,int m,int n)
        {
        int kok=1;
        int neg=0;
        double a, max;
        unsigned int i;
        int lev,lev2;

        max=fabs(A[0]);
        for (i=0; i<m*n; ++i)
            {
            a=A[i]; if (a<0) { neg=1; a=-a; }
            if (a!=floor(a)) kok=0;
            if (a>max) max=a;
            }

        if (max>1e10)
            {
            strcpy(muoto,"%8e");
            return(1);
            }

        if (max==0) max=1;
        if (max) lev=lev2=(int)(log(max)/log((double)10))+1;
        if (lev<minlev) lev=minlev;
        for (i=0; i<lev; ++i) muoto[i]='#'; muoto[lev]=EOS;
        if (kok) return(1);
        if (lev2<1) lev2=1;
        if (lev2<lev-neg) muoto[lev2+neg]='.';
        return(1);
        }

