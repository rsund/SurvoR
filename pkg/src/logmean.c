/* !logmean.c 27.8.2002/SM (27.8.2002)
   Logarithmic mean (Original solution presented in 1974 by SM)
*/

#include <stdio.h>
// #include <string.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define NMAX 200000

static SURVO_DATA dat;
static int xvar;
static int results_line;
static int prind;
static double x[NMAX],logx[NMAX];
static int n;
static double lmean;
static int method; // 1=direct 2=series expansion 3=series expansion (recursive)
                   // 4=(n-1)th diff.ratio
static int term_comp;
static double mean,geom_mean,hmean;
static double scale_factor;
static double *d1,*d2;

static int load_data();
static int comp1();
static int comp_x(int k);
static int comp4();
static int comp5();
static int comp2();
static double polm(int n,int m);
static int comp3();
static int next_m_distr(int n,int m,int *elem1);
static int other_means();
static int print_line(char *x);

/**********************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "METHOD", "TERM_COMP", "OTHERS", "POWMAX",
                 "RESULTS", "PRIND",  "!" };
char **specs=specs0;
**************************************/
/****************************
void main(argc,argv)
int argc; char *argv[];
****************************/

void muste_logmean(char *argv)
        {
        int i;

//      if (argc==1) return;
        s_init(argv);

        if (g<2)
            {
            sur_print("\nUsage: LOGMEAN <data>,<var>,L");
            WAIT; return;
            }
        results_line=0;
        if (g>3)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return;
            }

    d1=NULL;
    d2=NULL;

        i=spec_init(r1+r-1); if (i<0) return;
        i=data_read_open(word[1],&dat);
        if (i<0) return;

        i=conditions(&dat); if (i<0) return;

        prind=0;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        xvar=varfind(&dat,word[2]); if (xvar<0) return;

        method=3;
        i=spfind("METHOD");
        if (i>=0) method=atoi(spb[i]);

        term_comp=0;
        i=spfind("TERM_COMP"); if (i>=0) term_comp=atoi(spb[i]);

        i=load_data(); if (i<0) return;
        if (n<2L) { sur_print("\nAt least 2 observations needed!");
                    WAIT; return;
                  }

        if (method>3)
            {
            if (method==5)
//              for (i=0; i<n; ++i)
//              logx[i]=sqrt(x[i]);
            comp5();

            else comp4();
            }
        else if (method==3) comp3();
        else if (method==2) comp2();
        else if (method==1) comp1();
        else comp_x(-method);

        i=output_open(eout); if (i<0) return;
        sprintf(sbuf,"Data: %s Variable: %s  N=%d",word[1],word[2],n);
        print_line(sbuf);
        sprintf(sbuf,"Logarithmic mean: %16.16g",lmean/scale_factor);
        print_line(sbuf);
        i=spfind("OTHERS");
        if (i>=0 && atoi(spb[i])>0)
            {
            other_means();
            sprintf(sbuf,"Arithmetic mean:  %16.16g",mean/scale_factor);
            print_line(sbuf);
            sprintf(sbuf,"Geometric mean:   %16.16g",geom_mean/scale_factor);
            print_line(sbuf);
            sprintf(sbuf,"Harmonic mean:    %16.16g",hmean/scale_factor);
            print_line(sbuf);
            }
        output_close(eout);
        s_end(argv);
        }

static int load_data()
    {
    int l;
    double a;
    double min,max;

    min=1e308; max=1e-308;
    n=0L;

    for (l=dat.l1; l<=dat.l2; ++l)
        {
        if (unsuitable(&dat,l)) continue;
        data_load(&dat,l,xvar,&a);
        if (a==MISSING8) continue;
        if (a<=0.0) { sur_print("\nOnly positive values allowed!");
                      WAIT; return(-1);
                    }
        x[n]=a;
        if (a<min) min=a;
        if (a>max) max=a;
        logx[n]=log(a);
        ++n;
        }

    if (min!=max) scale_factor=2/(max-min);
    else scale_factor=max;
    if (method<=0 || term_comp) scale_factor=1.0;

    for (l=0L; l<n; ++l)
        { x[l]*=scale_factor; logx[l]=log(x[l]); }
    return(1);
    }

static int comp1()
    {
    int i,j;
    double fact;
    double term;
    double lx;

    fact=1.0; lmean=0;
    for (i=0; i<n; ++i)
        {
        if (i) fact*=(double)i;
        term=x[i]; lx=logx[i];
        for (j=0; j<n; ++j)
            {
            if (i!=j) term/=lx-logx[j];
            }
        lmean+=term;
        }
    lmean*=fact;
    return(1);
    }

static int comp_x(int k)
    {
    int i,j;
    double term;
    double lx;

    lmean=0;
    for (i=0; i<n; ++i)
        {
        term=pow(x[i],(double)k);
        lx=x[i];
        for (j=0; j<n; ++j)
            {
            if (i!=j) term/=lx-x[j];
            }
        lmean+=term;
        }
    return(1);
    }

static int comp4()
    {
    int i,j;
    double fact;



    d1=(double *)muste_malloc(n*sizeof(double));
    d2=(double *)muste_malloc(n*sizeof(double));

    fact=1.0; lmean=0;
    for (i=1; i<n; ++i)
        fact*=(double)i;

    for (i=0; i<n; ++i) d1[i]=x[i];

// for (i=0; i<n; ++i) Rprintf("\n%d %g %g",i,d1[i],logx[i]);

    for (j=0; j<n-1; ++j)
        {
        for (i=0; i<n-j-1; ++i)
            {
// Rprintf("\nj=%d i=%d %g %g %g %g",j,i,d1[i+1],d1[i],logx[i+j+1],logx[i]);
// getch();
            d2[i]=(d1[i+1]-d1[i])/(logx[i+j+1]-logx[i]);
            }
        for (i=0; i<n-j-1; ++i) d1[i]=d2[i];
        }
    lmean=fact*d1[0];
    return(1);
    }

static int comp5()
    {
    int i,j;
    double fact;



    d1=(double *)muste_malloc(n*sizeof(double));
    d2=(double *)muste_malloc(n*sizeof(double));

    fact=1.0; lmean=0;
//  for (i=1; i<n; ++i)
//      fact*=(double)i;

    for (i=0; i<n; ++i) d1[i]=x[i];

// for (i=0; i<n; ++i) Rprintf("\n%d %g %g",i,d1[i],logx[i]);

    for (j=0; j<n-1; ++j)
        {
        for (i=0; i<n-j-1; ++i)
            {
// Rprintf("\nj=%d i=%d %g %g %g %g",j,i,d1[i+1],d1[i],logx[i+j+1],logx[i]);
// getch();

            d2[i]=fact*(d1[i+1]-d1[i])/(logx[i+j+1]-logx[i]);
            }
        for (i=0; i<n-j-1; ++i) d1[i]=d2[i];
        ++fact;
        }
    lmean=d1[0];
    return(1);
    }

#define MMAX 200000
#define POWMAX 60

static int pot[MMAX];
static double powlog[MMAX][POWMAX];
static int powmax=POWMAX;
static double pm[MMAX][POWMAX];

static int comp2()
    {
    int i;
    int m;
    int ncomb;
    double term1,term2;
    double fact;
    double lmean1=0.0;

    i=spfind("POWMAX");
    if (i>=0) powmax=atoi(spb[i]);
    if (powmax>POWMAX) powmax=POWMAX;

    for (i=0; i<n; ++i) powlog[i][0]=logx[i];
    lmean=1.0;
    m=1; fact=1.0;
    while (1)
        {
// Rprintf("\nm=%d",m); getch();
        for (i=0; i<n; ++i) pot[i]=0;
        pot[n-1]=m; ncomb=1L;
        term2=0.0;
        while (1)
            {
//          sur_print("\n");
//          for (i=0; i<n; ++i) { sprintf(sbuf,"%d ",pot[i]); sur_print(sbuf); }
            term1=1.0;
            for (i=0; i<n; ++i)
                {
                if (pot[i]!=0) term1*=powlog[i][pot[i]-1];
                }
            term2+=term1;
// sprintf(sbuf,"\nterm1=%g term2=%g",term1,term2); sur_print(sbuf); getch();
            i=next_m_distr(m,n,pot);
            if (i<0) break;
            ++ncomb;
            }
        fact*=(double)m;
//      Rprintf("\nncomb=%ld fact=%g",ncomb,fact); getch();
        lmean+=term2/(double)ncomb/fact;
sprintf(sbuf,"\n%d: lmean=%16.16g",m,lmean); sur_print(sbuf);
        if (fabs(lmean-lmean1)==0.0 || m>=powmax) break;
        lmean1=lmean;
        ++m;

        for (i=0; i<n; ++i) powlog[i][m-1]=logx[i]*powlog[i][m-2];
        }
    return(1);
    }

static double polm(int n,int m)
    {
    int i;
    double s;

    s=pm[n-1][m-1];
    if (s!=1e308) return(s);

    if (m==1)
        {
        s=0.0;
        for (i=0; i<n; ++i) s+=logx[i];
        pm[n-1][m-1]=s;
        return(s);
        }
    if (n==1)
        {
        s=pm[n-1][m-1]=powlog[0][m-1];
        return(s);
        }
    s=powlog[n-1][m-1];
    for (i=1; i<m; ++i) s+=powlog[n-1][m-i-1]*polm(n-1,i);
    s+=polm(n-1,m);
    pm[n-1][m-1]=s;
    return(s);
    }

static int comp3()
    {
    int i;
    int m;
    double ncomb;
    double term2;
    double fact;
    double lmean1=0.0;
    double term3,aterm,gterm;
    double sum=0;


    i=spfind("POWMAX");
    if (i>=0) powmax=atoi(spb[i]);
    if (powmax>POWMAX) powmax=POWMAX;

    if (term_comp)
        {
        sum=0.0;
        for (i=0; i<n; ++i) sum+=logx[i];
        sum/=(double)n;
        }

    for (i=0; i<n; ++i) for(m=0; m<powmax; ++m) pm[i][m]=1e308;

    for (i=0; i<n; ++i)
        {
        powlog[i][0]=logx[i];
        for (m=2; m<=powmax; ++m) powlog[i][m-1]=powlog[i][m-2]*logx[i];
        }

    lmean=1.0;
    m=1; fact=1.0; ncomb=n;
    while (1)
        {
// Rprintf("\nm=%d",m); getch();
        term2=polm(n,m);
// Rprintf("\nterm2=%g",term2); getch();
        fact*=(double)m;
//      Rprintf("\nncomb=%g fact=%g",ncomb,fact); getch();
        if (!term_comp)
            lmean+=term2/(double)ncomb/fact;
        else
            {
            term3=term2/(double)ncomb;
            lmean+=term2/(double)ncomb/fact;
            aterm=0.0;
            for (i=0; i<n; ++i) aterm+=pow(logx[i],(double)m);
            aterm/=(double)n;
            gterm=pow(sum,(double)m);
            if (term3<gterm || aterm<term3)
   {
sprintf(sbuf,"\nm=%d gterm=%g term3=%g aterm=%g",m,gterm,term3,aterm);
sur_print(sbuf); sur_getch();
   }

            }
// sprintf(sbuf,"\n%d: lmean=%16.16g",m,lmean); sur_print(sbuf);
        if (fabs(lmean-lmean1)==0.0 || m>=powmax) break;
        lmean1=lmean;

        ++m;
        ncomb*=(double)(n+m-1)/(double)m;
        }
    return(1);
    }

static int next_m_distr(int n,int m,int *elem1) // n ja m vaihtaneet paikkojaan!
        {
        int i,k;

        i=m-1;
        while (elem1[i]==0) --i;
        if (i==0) return(-1);
        ++elem1[i-1];
        elem1[m-1]=elem1[i]-1;
        for (k=i; k<m-1; ++k) elem1[k]=0;
        return(1);
        }

static int other_means()
    {
    long l;

    mean=geom_mean=hmean=0.0;
    for (l=0L; l<n; ++l)
        {
        mean+=x[l]; geom_mean+=logx[l];
        hmean+=1.0/x[l];
        }
    mean/=(double)n; geom_mean=exp(geom_mean/(double)n);
    hmean=(double)n/hmean;
    return(1);
    }


static int print_line(char *x)
        {
        output_line(x,eout,results_line);
        if (results_line) ++results_line;
        if (results_line>r2) results_line=0;
        return(1);
        }

