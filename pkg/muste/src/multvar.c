#include "muste.h"
/* !multvar.c 27.5.1995/SM (23.7.1995) (1.1.1997)
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
// #include <conio.h>
#include <math.h>
// #include <malloc.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static int method; /* 1=STEPWISE (default) 2=EXHAUSTIVE */
            /* 3=sum of std.devs (exhaustive) */
static int results_line;
static double ceps;
static FILE *f;

static double *rr;
static int m;   /* total # of variables */
static double *rr11;
static double tr,tr_max;

static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];

static int *p,*p_max;
static int *var1;
static double *tr2;

static double *rr22;
static double tr_min;

static int l;

static int *perm1,*perm2,*perm_max;

static int next_perm(int n,int *p,int *q);
static char *sppois(char *s);
static int not_mem();
static int print_line(char *x);
static int maximum_var();
static int max_var();
static int sum2_Cholesky(double *rr,int m);
static int save_var(double *rr,int m,char *text);
static int partial_Cholesky(double *C,double *S,int mm,double eps,int *p,int h);
static int mat_symmetric(double *aa,int n);
static int maximum_var_perm();
static int max_varp(double *rr,int m);
static int comp_varp(double *rr,int m);
static int save_varp(double *rr11,int m,char *text);
static int cholesky5(double *T,double *X,int m,double eps,int *p);

/*************************************
char *specs0[]={ "EPS", "METHOD",
                  "!" };
char **specs=specs0;
***************************************/
extern char **spb;

/************************
main(argc,argv)
int argc; char *argv[];
***********************/

void muste_multvar(char *argv)
        {
        int i;
        char *p;
        extern double entr();
    //  char x[LLENGTH];

//      if (argc==1) return;
        s_init(argv);


    rr11=NULL;
    p=NULL;
    p_max=NULL;
    var1=NULL;
    tr2=NULL;
    rr11=NULL;
    rr22=NULL;
    perm1=NULL;
    perm2=NULL;
    perm_max=NULL;

        if (g<3)
            {
            init_remarks();
            rem_pr("MULTVAR <covariance_matrix_S>,L");
            rem_pr("computes a variability measure Mvar(S) of S.Mustonen (1995).");
            rem_pr("By default, the stepwise method is used.");
            rem_pr("The exhaustive method is selected by METHOD=EXHAUSTIVE.");
            rem_pr("The accuracy parameter in Cholesky decompositions is");
            rem_pr("set by EPS=eps (Default EPS=0.000001).");
            rem_pr("The optimally permuted covariance matrix is saved");
            rem_pr("as a matrix file COVVAR.M .");
            rem_pr(" ");
            rem_pr("Reference: ");
            rem_pr("S. Mustonen: A measure for total variability");
            rem_pr("             in multivariate normal distribution");
            rem_pr("Computational Statistics & Data Analysis, 23, 321-334 (1997)");

            wait_remarks(2);
            s_end(argv);
            return;
            }
        i=spec_init(r1+r-1); if (i<0) return;
        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1);
            if (results_line==0) return;
            }
/*      i=sp_init(r1+r-1); if (i<0) return;      poistettu 1.1.1997 */

        ceps=0.000001;
        i=spfind("EPS");
        if (i>=0) ceps=atof(spb[i]);

        method=1;
        i=spfind("METHOD");
        if (i>=0)
            {
            if (strncmp(spb[i],"EXHAUSTIVE",3)==0) method=2;
            else if (strncmp(spb[i],"STEPWISE",3)==0) method=1;
            else method=atoi(spb[i]);
            }
        if (method==1) { i=maximum_var(); s_end(argv); return; }
        else if (method==2 || method==3)
            { i=maximum_var_perm(); muste_fclose(f); s_end(argv);  return; }
        else
            {
            sprintf(sbuf,"\nMETHOD=%s unknown!",spb[i]);
            sur_print(sbuf); WAIT; s_end(argv); return;
            }
        }

static int next_perm(int n,int *p,int *q)
// int n;
// int *p;  /* permutation of 0,1,2,...,n --> next permutation */
// int *q;  /* auxiliary n vector */
        {
        int i,k,h;

        for (i=n-1; i>0; --i)
            {
            if (p[i-1]<p[i]) break;
            }
        if (i==0) return(-1);

        for (k=n-1; k>=0; --k)
            if (p[k]>p[i-1]) break;
        p[i-1]=p[k];

        for (k=0; k<n; ++k) q[k]=0;
        for (k=0; k<i; ++k) q[p[k]]=1;
        h=0;

        for (k=i; k<n; ++k)
            {
            while (q[h]) ++h;
            p[k]=h++;
            }
/*
printf("\nperm: ");
for (i=0; i<n; ++i) Rprintf("%d ",p[i]); getch();
*/
        return(1);
        }

static char *sppois(char *s)
        {
        while (*s!=EOS && *s==' ') ++s;
        return(s);
        }

static int not_mem()
        {
        sur_print("\nNot enough memory!"); WAIT; return(1);
        }

static int print_line(char *x)
        {
        output_line(x,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

/* mvar.c 13.7.1995/SM (23.7.1995) (30.10.1995) (17.2.1996)
*/

extern double sis_tulo();
extern double l_luku; /* mat */

extern char **spb;

static int maximum_var()
        {
        int i,k;
   //   char x[LLENGTH];
   //   double eps,a;

        i=matrix_load(word[1],&rr,&m,&k,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return(-1);

        if (k!=m)
            {
            sprintf(sbuf,"\n%s not a square matrix!",word[1]); sur_print(sbuf); WAIT; return(-1);
            }
        if(!mat_symmetric(rr,m))
            {
            sprintf(sbuf,"\nMatrix %s is not symmetric!",word[1]);
            sur_print(sbuf); WAIT; return(-1);
            }
        rr11=(double *)muste_malloc(m*m*sizeof(double));
        if (rr11==NULL) { not_mem(); return(-1); }
        p=(int *)muste_malloc(m*sizeof(int));
        if (p==NULL) { not_mem(); return(-1); }
        p_max=(int *)muste_malloc(m*sizeof(int));
        if (p_max==NULL) { not_mem(); return(-1); }
        var1=(int *)muste_malloc(m*sizeof(int));
        if (var1==NULL) { not_mem(); return(-1); }
        tr2=(double *)muste_malloc(m*sizeof(double));
        if (tr2==NULL) { not_mem(); return(-1); }

        max_var();

        return(1);
        }


static int max_var()    /* Stepwise_computation_of_Mvar() in pseudocode */
        {
        int i,j,k;
        char text[LLENGTH];
    //  double a;
        int ii;

        /* m is p in pseudocode */
        /* ii is i in pseudocode */
        /* indexing from 1 to p instead of from 0 to m-1 in pseudocode */

        tr_max=-1.0; /* Mvar in pseudocode */
        for (ii=0; ii<m; ++ii)
            {
            if (ii==0) for (i=0; i<m; ++i) p[i]=i;   /* p is q in pseudocode */
            else
                {
                k=0; p[0]=ii; j=0;
                for (i=1; i<m; ++i)
                    {
                    p[i]=j+k;
                    if (i==ii) k=1;
                    ++j;
                    }
                }

            k=sum2_Cholesky(rr,m); /* rr is S in pseudocode */
            if (k<0)
                {
                sprintf(sbuf,"\n %d gives the same value as %d",ii+1,-k);
                sur_print(sbuf);
                }
            else
                {
                sprintf(sbuf,"\n %d %g %g",ii+1,tr,tr_max); sur_print(sbuf);
                if (tr>tr_max)
                    {
                    tr_max=tr;
                    for (i=0; i<m; ++i) p_max[i]=p[i];
                    }
                }
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') break; }

            }

        for (i=0; i<m; ++i) p[i]=p_max[i];
        for (i=0; i<m; ++i) for (j=0; j<m; ++j)
            rr11[i+m*j]=rr[p[i]+m*p[j]];
        save_var(rr11,m,text);

        output_open(eout);
        fnconv(tr_max,accuracy+2,text);
        sprintf(sbuf,"Mvar[%s]=%s (Total variability in a %d*%d matrix)",
                          word[1],sppois(text),m,m);
        print_line(sbuf);
        print_line("MAT LOAD COVVAR.M,END+2 / Optimally permuted covariance matrix");
        return(1);
        }

static int sum2_Cholesky(double *rr,int m) /* stepwise maximizing sum of residual variances */
        {
        int i,k,kk;
        int k_max=0;
        double a,b,var_max;

        tr=rr[p[0]*(m+1)];
        for (k=1; k<m; ++k)
            {
            partial_Cholesky(rr11,rr,m,ceps,p,k-1);
            a=rr11[p[k-1]*(m+1)];
            for (i=k; i<m; ++i)
                {
                if (0.99999*fabs(rr11[p[i]+m*p[k-1]])>a)
                    { tr=-1.0; return(1); }
                }

            var_max=-1.0;
            for (kk=k; kk<m; ++kk)
                {
                a=0.0;
                for (i=k; i<=kk; ++i) { b=rr11[p[kk]+m*p[i]]; a+=b*b; }
                if (a-var_max>1e-10) { var_max=a; k_max=kk; }
                } /* kk */
            i=p[k_max]; p[k_max]=p[k]; p[k]=i;
            tr+=var_max;
            if (k==1)
                {
                var1[p[0]]=p[1]; tr2[p[0]]=tr;
                if (p[1]<p[0] && var1[p[1]]==p[0]
                       && fabs(tr-tr2[p[1]])<1e-7) return(-p[1]-1);
                }

            } /* k */
        return(1);
        }

static int save_var(double *rr,int m,char *text)  /* saving the optimally permuted covariance matrix */
        {
        int i,j;
        char nimi[LNAME];

        *text=EOS;
        for (i=0; i<m; ++i)
            {
            for (j=0; j<lr; ++j) rlab[i*lr+j]=clab[p[i]*lr+j];
            }
        for (i=0; i<m*lr; ++i) clab[i]=rlab[i];
        strcpy(nimi,edisk); strcat(nimi,"COVVAR.M");
        sprintf(sbuf,"Permuted_covariance_matrix %s",text);
        matrix_save(nimi,rr11,m,m,rlab,clab,lr,lr,0,sbuf,0,0);

        return(1);
        }

/* updating Cholesky decomposition */
static int partial_Cholesky(double *C,double *S,int mm,double eps,int *p,int h)
// double *C;
// double *S;
// int m;
// double eps;
// int *p;       /* permutation vector */
// int h;        /* start column */
        {
        int i,j,k;
        double a,b;

        for (i=h; i<m; ++i)
            for (j=0; j<=i; ++j)
                {
                a=S[p[i]+m*p[j]];
                for (k=0; k<=j-1; ++k)
                    a-=C[p[i]+m*p[k]]*C[p[j]+m*p[k]];

                if (i==j)
                    {
                    b=sqrt(fabs(a));
                    if (b<eps) b=eps;
                    C[p[i]*(m+1)]=b;
                    }
                else
                    {
                    b=C[p[j]*(m+1)];
                    if (b>0.0) C[p[i]+m*p[j]]=a/b; else C[p[i]+m*p[j]]=0.0;
                    C[p[j]+m*p[i]]=0.0;
                    }
                }
        return(1);
        }

static int mat_symmetric(double *aa,int n) /* testing symmetry of input matrix */
        {
        int i,j;
        double a,b;

        for (i=0; i<n; ++i)
            for (j=i+1; j<n; ++j)
                {
                a=aa[i+n*j]; b=aa[j+n*i];
                if (fabs(a-b)/(fabs(a)+1.0)>1e-7) return(0);
                }
        return(1);
        }

/* mvarp.c 7.7.1995/SM (23.7.1995)
*/
static int maximum_var_perm()  /* exhaustive solution */
        {
        int i,k;
   //   char *p;
        char x[LLENGTH];
   //   double eps,a;

        i=matrix_load(word[1],&rr,&m,&k,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return(-1);

        if (k!=m)
            {
            sprintf(sbuf,"\n%s not a square matrix!",word[1]); sur_print(sbuf); WAIT; return(-1);
            }
        if(!mat_symmetric(rr,m))
            {
            sprintf(sbuf,"\nMatrix %s is not symmetric!",word[1]);
            sur_print(sbuf); WAIT; return(-1);
            }

        rr11=(double *)muste_malloc(m*m*sizeof(double));
        if (rr11==NULL) { not_mem(); return(-1); }
        rr22=(double *)muste_malloc(m*m*sizeof(double));
        if (rr22==NULL) { not_mem(); return(-1); }
        perm1=(int *)muste_malloc(m*sizeof(int));
        if (perm1==NULL) { not_mem(); return(-1); }
        perm2=(int *)muste_malloc(m*sizeof(int));
        if (perm2==NULL) { not_mem(); return(-1); }
        perm_max=(int *)muste_malloc(m*sizeof(int));
        if (perm_max==NULL) { not_mem(); return(-1); }

        strcpy(x,etmpd); strcat(x,"SURVO.TMP");
        f=muste_fopen(x,"wt");

        max_varp(rr,m);

        return(1);
        }

static int max_varp(double *rr,int m)
        {
        int i,j;
        char text[LLENGTH];
        char text2[LLENGTH];
    //  double a;

        for (i=0; i<m; ++i) perm1[i]=i;

        l=0L;
        tr_max=-1.0; tr_min=1e10;
        while (1)
            {
            ++l;
            comp_varp(rr,m);
            sprintf(sbuf,"\n %d %g %g",l,tr,tr_max); sur_print(sbuf);
            fprintf(f,"%g\n",tr);

            if (tr>tr_max)
                {
                tr_max=tr;
                for (i=0; i<m; ++i) perm_max[i]=perm1[i];
                }
            if (tr>=0.0 && tr<tr_min)
                {
                tr_min=tr;
                }
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') break; }

            i=next_perm(m,perm1,perm2);

            if (i<0) break;
            }

        for (i=0; i<m; ++i) perm1[i]=perm_max[i];
        comp_varp(rr,m);
        for (i=0; i<m; ++i) for (j=0; j<m; ++j)
            rr11[i+m*j]=rr[perm1[i]+m*perm1[j]];
        save_varp(rr11,m,text);
        output_open(eout);
        fnconv(tr,accuracy+2,text);
        fnconv(tr_min,accuracy+2,text2);
        sprintf(sbuf,"Mvar[%s]=%s minM[%s]=%s dim=%d",
            word[1],sppois(text),word[1],sppois(text2),m);
        print_line(sbuf);
        print_line("MAT LOAD COVVAR.M,END+2 / Optimally permuted covariance matrix");
        return(1);
        }

static int comp_varp(double *rr,int m)
        {
        int i;
        double a;

        i=cholesky5(rr11,rr,m,ceps,perm1);
        if (i<0) { tr=-1.0; return(1); }
        if (method==3)
            {
            tr=0.0; for (i=0; i<m; ++i) { a=rr11[i*(m+1)]; tr+=fabs(a); }
            }
        else
            {
            tr=0.0; for (i=0; i<m; ++i) { a=rr11[i*(m+1)]; tr+=a*a; }
            }
        return(1);
        }


static int save_varp(double *rr11,int m,char *text)
        {
        int i,j;
    //  char *p;
        char nimi[LNAME];

        *text=EOS;
        for (i=0; i<m; ++i)
            {
            for (j=0; j<lr; ++j) rlab[i*lr+j]=clab[perm1[i]*lr+j];
            }
        for (i=0; i<m*lr; ++i) clab[i]=rlab[i];
        strcpy(nimi,edisk); strcat(nimi,"COVVAR.M");
        sprintf(sbuf,"Permuted_covariance_matrix %s",text);
        matrix_save(nimi,rr11,m,m,rlab,clab,lr,lr,0,sbuf,0,0);

        return(1);
        }

static int cholesky5(double *T,double *X,int m,double eps,int *p)
        {
        int i,j,k;
        double a,b;

        for (i=0; i<m; ++i)
            {
            for (j=0; j<=i; ++j)
                {
                a=X[p[i]+m*p[j]];
                for (k=0; k<=j-1; ++k)
                    a-=T[p[i]+m*p[k]]*T[p[j]+m*p[k]];

                if (i==j)
                    {
                    b=sqrt(fabs(a));
                    if (b<eps) b=eps;
                    T[p[i]*(m+1)]=b;
                    }
                else
                    {
                    b=T[p[j]*(m+1)];
                    if (b>0.0) T[p[i]+m*p[j]]=a/b; else T[p[i]+m*p[j]]=0.0;
                    T[p[j]+m*p[i]]=0.0;
                    }
                }
            for (j=0; j<i; ++j)
                if (0.999999*fabs(T[p[i]+m*p[j]])>T[p[j]*(m+1)]) return(-1);
            }
        return(1);
        }

