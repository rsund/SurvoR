#include "muste.h"
/* covtest.c 16.2.2000/SM (16.2.2000)
*/

#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define S_MAX 10

static SURVO_DATA d;
static FILE *tempfile;

static int n[S_MAX+1];
static int nt; /* n total */
static int tulosrivi;
static int m;
static char aineisto[LNAME];
static char tempname[LNAME];
static int prind;
static double *x;
static int *v;
static short *ind;
static double *s[S_MAX+1];
static double *s2[S_MAX+1];
static double *xx;
static int ns; /* otoksia */
static char y[LLENGTH],*otos[S_MAX];

static double t0,t1;
static double p_sim;
static double x2,os1,nim2,a,df,pr_x2;

// static int rand_type;
// static int seed;
static int nn,nn1;
static int simumax=10000;

static int talleta(int k);
static int data_error(int k);
static int laske_summat();
static double testi();
static int print_t0(double x2,double df,double p);
static int not_enough_memory();
static int eoutput(char *rivi);

/*************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "SIMUMAX", "SAMPLES", "RAND", "RND", "SEED",
                 "RESULTS", "PRIND", "!" };
char **specs=specs0;
****************************/
extern double testi();
extern double muste_cdf_f();
extern double muste_cdf_chi2();

/**********************
main(argc,argv)
int argc; char *argv[];
**********************/

void muste_covtest(char *argv)
        {
        int i,k,h,kk;
        int l,l2,li;

     // if (argc==1) return(1);
        s_init(argv[1]);
        if (g<2)
            {
            sur_print("\nUsage: COVTEST <output_line>");
            sur_print("\n       SAMPLES=<data(1),...,<data(m)>");
            WAIT; return;
            }
        tulosrivi=0;
        if (g>1)
            {
            tulosrivi=edline2(word[1],1,1);
            if (tulosrivi==0) return;
            }
        i=spec_init(r1+r-1); if (i<0) return;
        if ((i=spfind("RESULTS"))>=0) results=atoi(spb[i]);
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        simumax=10000;
        if ((i=spfind("SIMUMAX"))>=0) simumax=atol(spb[i]);

        spec_rnd();

        strcpy(tempname,etmpd); strcat(tempname,"SURVOCOV.TMP");
        tempfile=fopen(tempname,"wb");
        if (tempfile==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s!",tempname);
            sur_print(sbuf); WAIT; return;
            }

        i=spfind("SAMPLES");
        if (i<0)
            {
            sur_print("SAMPLES=<data(1),...,<data(m)> missing!");
            WAIT; return;
            }

        strcpy(y,spb[i]);
        ns=split(y,otos,S_MAX);
        if (ns<2)
            {
            sur_print("At least 2 samples must be given by SAMPLES!");
            WAIT; return;
            }

    x=NULL;
    v=NULL;
    xx=NULL;
    ind=NULL;

        nt=0L;
        for (k=0; k<ns; ++k)
            {
            strcpy(aineisto,otos[k]);
            i=data_read_open(aineisto,&d); if (i<0) return;
            i=mask(&d); if (i<0) return;
            if (d.m_act==0)
                {
                sur_print("\nNo active variables!");
                WAIT; return;
                }

            if (k==0)
                {
                m=d.m_act;
                x=(double *)muste_malloc(m*sizeof(double));
                if (x==NULL) { not_enough_memory(); return; }
                v=(int *)muste_malloc(m*sizeof(int));
                if (v==NULL) { not_enough_memory(); return; }
                for (i=0; i<m; ++i) v[i]=d.v[i];
                }

            if (k!=0)
                {
                if (d.m_act!=m) { data_error(k); return; }
                for (i=0; i<m; ++i)
                    {
                    if (v[i]!=d.v[i]) { data_error(k); return; }
                    }
                }
            sur_print("\n");
            talleta(k);   /* data k */
            data_close(&d);
            }

        muste_fclose(tempfile);


        tempfile=fopen(tempname,"rb");

        xx=(double *)muste_malloc(nt*m*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return; }

        fread(xx,sizeof(double),nt*(int)m,tempfile);
        muste_fclose(tempfile);

        ind=muste_malloc(sizeof(short)*nt);
        if (ind==NULL) { not_enough_memory(); return; }

        for (k=0; k<ns+1; ++k)
            {
            s[k]=NULL;
            s[k]=muste_malloc(m*sizeof(double));
            if (s[k]==NULL) { not_enough_memory(); return; }
            s2[k]=NULL;
            s2[k]=muste_malloc(m*m*sizeof(double));
            if (s2[k]==NULL) { not_enough_memory(); return; }
            }

        l=0;
        for (k=0; k<ns; ++k)
            for (li=0L; li<n[k]; ++li) ind[l++]=k;

        laske_summat();
        l=0;
        for (k=0; k<ns; ++k)
            for (li=0; li<n[k]; ++li)
                {
                for (h=0; h<m; ++h)
                    xx[l+h]-=s[k][h]/n[k];
                l+=m;
                }

        t0=testi();
        os1=(double)(nt-ns)*m/2*log((double)(nt-ns));
        nim2=0.0; a=0.0;
        for (k=0; k<ns; ++k)
            {
            nim2+=(n[k]-1)*muste_log((double)(n[k]-1));
            a+=1.0/(n[k]-1);
            }
        x2=t0/2+os1-(double)m/2*nim2;
        a=1-(a-1.0/(nt-ns))*(2*m*m+3*m-1)/6.0/(m+1)/(k-1);
        x2*=-2*a;
// Rprintf("x2=%g\n",x2); getch();
        df=(double)m/2*(m+1)*(ns-1);
        pr_x2=1.0-muste_cdf_chi2(x2,df,1e-7);

/*****************************************************
os1=n*p/2*log(n)
os1=10326.054776478
os2=0.5*(n1*log(det1)+n2*log(det2)+n3*log(det3))
os2=8299.3933358899
nim1=n/2*log(det)
nim1=9932.2525957076
nim2=p/2*(n1*log(n1)+n2*log(n2)+n3*log(n3))
nim2=8697.69129334

logL=os1+os2-nim1-nim2
logL=-4.4957766798343

a=1-(1/n1+1/n2+1/n3-1/n)*(2*p*p+3*p-1)/6/(p+1)/(k-1)

X2=-2*a*logL
X2=8.9516537694752
df=p/2*(p+1)*(k-1)
********************************************************/

        print_t0(x2,df,pr_x2);
        ++scroll_line;
        nn1=0L; kk=0;
        for (nn=1; nn<=simumax; ++nn)
            {
            for (l=0; l<nt; ++l) ind[l]=0;
            for (k=1; k<ns; ++k)
                {
                for (l=0L; l<n[k]; ++l)
                    {
                    while (1)
                        {
                        l2=nt*uniform_dev();
                        if (ind[l2]!=(short)0) continue;
                        ind[l2]=k; break;
                        }
                    }
                }

            t1=testi();
            if (t1<t0) ++nn1;

            ++kk;
/*************************
            if (kbhit())
                {
                i=getch(); if (i=='.') break;
                prind=1-prind;
                }
******************************/
            if (kk==1000)
                {
                if (prind)
                     {
                     sprintf(sbuf,"\n%d %g  ",nn,(double)nn1/(double)nn);
                     sur_print(sbuf);
                     }
                kk=0;
                }
            }

        output_open(eout);
        --nn;
        eoutput("Comparing covariance matrices:");
        sprintf(sbuf,"Asymptotic X^2 test: X2=%g df=%g P=%g",x2,df,pr_x2);
        eoutput(sbuf);
        p_sim=(double)nn1/(double)nn;
        sprintf(sbuf,"Randomization test: N=%d P=%g (s.e. %g)",
            nn,p_sim,sqrt(p_sim*(1-p_sim)/nn));
        eoutput(sbuf);
        output_close(eout);

        s_end(argv);

        return;
        }

static int talleta(int k)
        {
        int i,h;
        int l;
        char *p;

        n[k]=0L;
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            for (i=0; i<m; ++i)
                {
                data_load(&d,l,v[i],&x[i]);
                if (x[i]==MISSING8) break;

                }
            if (i<m) continue;
            for (i=0; i<m; ++i)
                {
                p=(char *)&x[i];
                for (h=0; h<sizeof(double); ++h)
                    fputc((int)p[h],tempfile);
                }
            ++n[k];
            }
        nt+=n[k];
        return(1);
        }

static int data_error(int k)
        {
        sprintf(sbuf,"\nData sets %s and %s must have same structures!",
                    otos[0],otos[k]);
        sur_print(sbuf); WAIT; return(1);
        }

static int laske_summat()
        {
        int i,j,k;
        int l,r;


        rewind(tempfile);
        for (k=0; k<ns+1; ++k)
            {
            for (i=0; i<m; ++i) s[k][i]=0.0;
            for (i=0; i<m*m; ++i) s2[k][i]=0.0;
            }

        for (l=0; l<nt; ++l)
            {
            k=ind[l];
            r=l*m;
            for (i=0; i<m; ++i)
                {
                s[k][i]+=xx[i+r];
                for (j=0; j<=i; ++j) s2[k][i+m*j]+=xx[i+r]*xx[j+r];
                }
            }
        return(1);
        }

static double testi()
        {
        int i,j,k;
        double t,ldet;

        laske_summat();
        for (i=0; i<m*m; ++i) s2[ns][i]=0.0;

        t=0.0;
        for (k=0; k<ns; ++k)
            {
            for (i=0; i<m; ++i)
                for (j=0; j<=i; ++j)
                    {
                    s2[k][i+m*j]-=s[k][i]*s[k][j]/n[k];
                    s2[k][j+m*i]=s2[k][i+m*j];
                    s2[ns][i+m*j]+=s2[k][i+m*j];
                    }
            mat_logdet(s2[k],m,&ldet);
// Rprintf("det=%g\n",exp(ldet)); getch();
            t+=(n[k]-1)*ldet;
            }

        for (i=0; i<m; ++i)
            for (j=0; j<=i; ++j)
                {
                s2[ns][j+m*i]=s2[ns][i+m*j];
                }
        mat_logdet(s2[ns],m,&ldet);
// Rprintf("det=%g\n",exp(ldet)); getch();
        t-=(nt-ns)*ldet;

        return (t);
        }


static int print_t0(double x2,double df,double p)
        {
        sprintf(sbuf,"Asymptotic X^2=%g df=%g P=%g\n",x2,df,p);
        sur_print(sbuf);
        return(1);
        }
/******************************
mprint(A,m,n)
double *A;
int m,n;
        {
        int i,j;
        for (i=0; i<m; ++i)
            {
            Rprintf("\n");
            for (j=0; j<n; ++j) Rprintf("%g ",A[i+m*j]);
            }
        Rprintf("\n");
        getch();
        return(1);
        }
*********************************/
static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (T2TEST)");
        WAIT; return(1);
        }

static int eoutput(rivi)
char *rivi;
        {
        output_line(rivi,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

