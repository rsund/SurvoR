#include "muste.h"
/* statmsf.c 16.11.1988/SM (24.10.1991) (2.7.1996)
*/
#include <stdio.h>
#include <stdlib.h>
//#include <conio.h>
//#include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXCLASS 101

static SURVO_DATA d;

static int prind=1;
static double *sum;       /* sums of active variables */
static double *sum2;
static long   *f;         /* frequencies */
static double *w;         /* sums of weigths */
static long   *f2;
static double limit[MAXCLASS];
static char x[LLENGTH],*osa[MAXCLASS];

static int n;
static int weight_variable;
static int results_line;
static unsigned int n_class;
static unsigned int m;
/*************
char *spec_msf[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                   "PRIND", "TRESHOLDS", "LIMITS", "!" };
extern char **specs;
***************/
static double *X;
static char *rlabX, *clabX, exprX[LLENGTH];
static int  mX, nX, typeX, lrX, lcX;

/* RS REM
static double *Y;
static char *rlabY, *clabY, exprY[LLENGTH];
static int  mY, nY, typeY, lrY, lcY;
*/

static double *T;
static char *rlabT, *clabT, exprT[LLENGTH];
static int  mT, nT, typeT, lrT, lcT;

// static char expr[3*LLENGTH];

static int *v;

static int m_test_scaletypes();
static int m_space_allocation();
static int compute_sums();
static int m_printout();
static int pvalues(int ii);
static int nrot();
static int load_X(char *nimi);
// RS REM static int load_Y(char *nimi);
static int save_T(char *nimi);
// RS REM static int mat_alloc(double **A,int m,int n);
static int mat_alloc_lab(double **A,int m,int n,char **rlab,char **clab);
static int varaa_tila(double **A,int m,int n,char **rlab,char **clab,int mcr,int mcl);
static int ei_tilaa();

void muste_statmsf(char *argv)
        {
        int i;

// RS Variable init
		prind=1;
		sum=NULL;
		sum2=NULL;
		f=NULL;
		w=NULL;
		f2=NULL;
		X=NULL;
		rlabX=NULL;
		clabX=NULL;
		T=NULL;
		rlabT=NULL;
		clabT=NULL;
		v=NULL;

        s_init(argv);

        typeT=lrT=lcT=0; // to avoid warnings from compiler

  //    specs=spec_msf;
        if (g<2)
            {
            init_remarks();
            rem_pr("STATMSF <Survo_data>,<output_line>                                      ");
            rem_pr("        LIMITS=<low1>,<up1>,<up2>,...");
            rem_pr("computes means, standard deviations, and frequency distributions");
            rem_pr("of active variables. Cases can be limited by IND and CASES specifications.");
            rem_pr("The frequencies are computed according to a classification given by the");
            rem_pr("LIMITS specification where <low1> is the lower limit of the first class 1");
            rem_pr("and <up1>,<up2>,... are the upper limits of the classes 1,2,...");
            rem_pr("The default setting is LIMITS=0,1,2,3,4,5 .");
            wait_remarks(1);
            rem_pr("STATMSF <Survo_data> / TRESHOLDS=<matrix_file>");
            rem_pr("where <matrix_file> is of the form");
            rem_pr("row label   1st column");
            rem_pr("variable_1  treshold_value_1");
            rem_pr("variable_2  treshold_value_2");
            rem_pr("...");
            rem_pr("computes relative frequencies of values exceeding treshold values");
            rem_pr("given as the first column of <matrix_file> for variables given");
            rem_pr("as row labels in <matrix_file> for active observations");
            rem_pr("in  <Survo_data>.");
            rem_pr("The results are saved in a matrix file TAILFREQ.M .");
            wait_remarks(2);
            return;
            }
        results_line=0;
        i=spec_init(r1+r-1); if (i<0) return;
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);
        i=spfind("TRESHOLDS");
        if (i>=0) { pvalues(i); return; }

        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }
        i=data_read_open(word[1],&d); if (i<0) return;
        i=spfind("LIMITS");
        if (i<0)
            {
            n_class=5; limit[0]=0;
            for (i=1; i<=5; ++i) limit[i]=i;
            }
        else
            {
            strcpy(x,spb[i]);
            n_class=split(x,osa,MAXCLASS);
            for (i=0; i<n_class; ++i) limit[i]=atof(osa[i]);
            --n_class;
            }
        i=mask(&d); if (i<0) return;
        weight_variable=activated(&d,'W');
        i=m_test_scaletypes(); if (i<0) return;
        i=conditions(&d); if (i<0) return;  /* permitted only once */
        m=d.m_act;
        if (m==0)
            {
            sur_print("\nNo active (acceptable) variables!");
            WAIT; return;
            }
        i=m_space_allocation(); if (i<0) return;

//      i=optdim_d(); if (i && i<d.m) err(0);
//      i=optdim_o(); if (i && (long)i<d.n) err(0);

        compute_sums();
        m_printout();
        data_close(&d);
        s_end(argv);
        }

static int m_test_scaletypes()
        {
        int i,scale_error;

        scales(&d);
        if (weight_variable>=0)
            {
            if (!scale_ok(&d,weight_variable,RATIO_SCALE))
                {
                sprintf(sbuf,"\nWeight variable %.8s must have ratio scale!",
                          d.varname[weight_variable]); sur_print(sbuf);
                WAIT; if (scale_check==SCALE_INTERRUPT) return(-1);
                }
            }
        scale_error=0;
        for (i=0; i<d.m_act; ++i)
            {
            if (!scale_ok(&d,d.v[i],SCORE_SCALE))
                {
                if (!scale_error)
                    sur_print("\nInvalid scale in variables: ");
                scale_error=1;
                sprintf(sbuf,"%.8s ",d.varname[d.v[i]]); sur_print(sbuf);
                }
            }
        if (scale_error)
            {
            sur_print("\nIn MEAN score scale at least is expected!");
            WAIT; if (scale_check==SCALE_INTERRUPT) return(-1);
            }
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (STAT)");
        WAIT;
        return(1);
        }

static int m_space_allocation()
        {
        sum=(double *)muste_malloc(m*sizeof(double));
        if (sum==NULL) { not_enough_memory(); return(-1); }
        sum2=(double *)muste_malloc(m*sizeof(double));
        if (sum2==NULL) { not_enough_memory(); return(-1); }
        f=(long *)muste_malloc(m*sizeof(long));
        if (f==NULL) { not_enough_memory(); return(-1); }
        w=(double *)muste_malloc(m*sizeof(double));
        if (w==NULL) { not_enough_memory(); return(-1); }
        f2=(long *)muste_malloc(m*n_class*sizeof(long));
        if (w==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int compute_sums()
        {
        unsigned int i,k;
        unsigned int ui;
        long l;

        n=0L;
        for (i=0; i<d.m_act; ++i)
            {
            f[i]=0L; w[i]=0.0; sum[i]=0.0; sum2[i]=0.0;
            }
        for (ui=0; ui<m*n_class; ++ui) f2[ui]=0L;

        sur_print("\n");
        for (l=d.l1; l<=d.l2; ++l)
            {
            double weight;

            if (unsuitable(&d,l)) continue;
            if (weight_variable==-1) weight=1.0;
            else
                {
                data_load(&d,l,weight_variable,&weight);
                if (weight==MISSING8) continue;
                }
            ++n;
      /*********************
            if (muste_kbhit()) { muste_getch(); if (muste_kbhit()) muste_getch(); prind=1-prind; }
            if (prind)
                { sprintf(sbuf,"%ld ",l); sur_print(sbuf); }
      ***********************/
            for (i=0; i<d.m_act; ++i)
                {
                double x;

                if (d.v[i]==weight_variable) continue;
                data_load(&d,l,d.v[i],&x);
                if (x==MISSING8) continue;
                if (x<=limit[0] || x>limit[n_class]) continue;
                ++f[i]; w[i]+=weight; sum[i]+=weight*x; sum2[i]+=weight*x*x;
                for (k=0; k<n_class-1; ++k)
                    {
                    if (x<=limit[k+1]) break;
                    }
                ++f2[k+i*n_class];
                }
            }
        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

static int m_printout()
        {
        int i,k,h;
        char line[LLENGTH];
        char mean[32];
        char stddev[32];

        output_open(eout);
        print_line(" Means, std.devs and frequency distributions of variables");
        sprintf(line," in %s N=%d",
                          word[1],n);
        if (weight_variable>=0)
            {
            strcat(line," Weight="); strncat(line,d.varname[weight_variable],8);
            }
        print_line(line);
        strcpy(line,"                                           Frequencies");
        print_line(line);
        h=sprintf(line," Variable     Mean     Std.dev. N(missing)");
        for (k=0; k<n_class; ++k) h+=sprintf(line+h,"%4d ",k+1);
        print_line(line);
        for (i=0; i<d.m_act; ++i)
            {
            if (d.v[i]==weight_variable) continue;
            if (w[i]==0.0)
                sprintf(line," %-8.8s         -          -  %6d",d.varname[d.v[i]],
                         (int)(n-f[i]));
            else
                {
                fnconv(sum[i]/w[i],accuracy+2,mean);
                if (w[i]>1.0)
                   fnconv(sqrt((sum2[i]-sum[i]*sum[i]/w[i])/(w[i]-1)),accuracy+2,stddev);
                else
                   { strncpy(stddev,space,accuracy+2); stddev[accuracy+2]=EOS;
                     stddev[accuracy+1]='-';
                   }

                h=sprintf(line," %-8.8s %s  %s  %6d    ",d.varname[d.v[i]],
                             mean,stddev,(int)(n-f[i]));
                for (k=0; k<n_class; ++k)
                    h+=sprintf(line+h,"%4ld ",f2[k+i*n_class]);
                }
            print_line(line);
            }
        output_close(eout);
        return(1);
        }

/* statm2.c 29.4.1996/SM (30.4.1996)
*/

static int pvalues(int ii)
        {
        int i;
        char x[LLENGTH];
        double *freq;
        long l;
        double a;

        strcpy(x,spb[ii]);
        i=load_X(x); if (i<0) return(-1);
/* Rprintf("\ndim=%d,%d",mX,nX); getch(); */
        i=data_open(word[1],&d); if (i<0) return(-1);
        v=(int *)muste_malloc(mX*sizeof(int));
        if (v==NULL) { ei_tilaa(); return(-1); }
        i=nrot();
        mT=mX; nT=2; rlabT=rlabX ;
        i=mat_alloc_lab(&T,mT,nT,NULL,&clabT);
        freq=T+mT;
        for (i=0; i<mX; ++i) freq[i]=0.0;

        i=conditions(&d); if (i<0) return(-1);

        n=0L; sur_print("\n");
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d)) continue;
            sprintf(sbuf,"%ld ",l); sur_print(sbuf);
            ++n;
            for (i=0; i<mX; ++i)
                {
                data_load(&d,l,v[i],&a);
                if (a==MISSING8) continue;
                if (a>X[i]) ++freq[i];
                }
            }
        if (n==0L)
            {
            sur_print("\nNo active observations!"); WAIT; return(-1);
            }

        a=1.0/(double)n;
        for (i=0; i<mX; ++i)
            {
            T[i]=X[i];
            freq[i]*=a;
            }
        strncpy(clabT,"Value   P       ",16);
        sprintf(exprT,"Tail_frequencies_in_data_%s_N=%d",word[1],n);
        save_T("TAILFREQ.M");

        return(1);
        }

static int nrot()
        {
        int i,h;
        char nimi[9];

        for (i=0; i<mX; ++i)
            {
            for (h=0; h<8; ++h) nimi[h]=rlabX[i*8+h];
            h=7; while (h>0 && nimi[h]==' ') nimi[h--]=EOS;
            h=varfind(&d,nimi);
            if (h<0) return(-1);
            v[i]=h;
            }
        return(1);
        }

static int load_X(char *nimi)
        {
        int i;

        i=matrix_load(nimi,&X,&mX,&nX,&rlabX,&clabX,&lrX,&lcX,&typeX,exprX);
        return(i);
        }
/*********************
static int load_Y(char *nimi)
        {
        int i;

        i=matrix_load(nimi,&Y,&mY,&nY,&rlabY,&clabY,&lrY,&lcY,&typeY,exprY);
        return(i);
        }
************************/
static int save_T(char *nimi)
        {
        int i;

        i=matrix_save(nimi,T,mT,nT,rlabT,clabT,8,8,-1,exprT,0,0);
        return(i);
        }
/*********************
static int mat_alloc(double **A,int m,int n)
        {
        return(varaa_tila(A,m,n,NULL,NULL,0,0));
        }
*********************/
static int mat_alloc_lab(double **A,int m,int n,char **rlab,char **clab)
        {
        return(varaa_tila(A,m,n,rlab,clab,8,8));
        }

static int varaa_tila(double **A,int m,int n,char **rlab,char **clab,int mcr,int mcl)
        {
        *A=(double *)muste_realloc(*A,m*n*sizeof(double));
        if (*A==NULL) { ei_tilaa(); return(-1); }
        *rlab=(char *)muste_realloc(*rlab,m*mcr);
        if (*rlab==NULL) { ei_tilaa(); return(-1); }
        *clab=(char *)muste_realloc(*clab,n*mcl);
        if (*clab==NULL) { ei_tilaa(); return(-1); }
/* RS CHA       
        if (*A!=NULL) muste_free(*A);
        *A=(double *)muste_malloc(m*n*sizeof(double));
        if (*A==NULL) { ei_tilaa(); return(-1); }
        if (rlab!=NULL)
            {
            if (*rlab!=NULL) muste_free(*rlab);
            *rlab=(char *)muste_malloc(m*mcr);
            if (*rlab==NULL) { ei_tilaa(); return(-1); }
            }
        if (clab!=NULL)
            {
            if (*clab!=NULL) muste_free(*clab);
            *clab=(char *)muste_malloc(n*mcl);
            if (*clab==NULL) { ei_tilaa(); return(-1); }
            }
*/            
        return(1);
        }

static int ei_tilaa()
        {
        PR_EBLD;
        sur_print("\nNot enough space for matrices");
        WAIT; PR_ENRM; erun=0;
        return(1);
        }

