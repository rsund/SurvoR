/* !mntest.c 28.1.1996/SM (5.2.1996) (2.4.1996) (14.4.1996)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
// #include <memory.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXSPACE 8100L
#define MAXIFLOAT 16200L

static SURVO_DATA d;
static int results_line;
static int m_act;
static double *xx,*mean,*S,*T,*d2,*yy;
static int n;
static FILE *temp;
static int dim;
static double vin,huip;
static double eps_dim;
static double *zz;
static int ind_zz;
static int prind;
static int results1;
static int test;

static FILE *temp2;
static int ks_stat=0;

#define MARDIA 1
#define MAHAL  2
#define CUBE   3

/* CUBE */
static int fr1[100],fr2[100];
static int nf;

/* MAHAL */
static int fm[100];
static int nm;

static int varaa_tilat();
static int ei_tilaa();
static int talleta_data();
static int laske_momentit();
static int principal_comp();
static int siirto_zz();
static int tunnusluvut();
static int lue(double *xx,int j);
static int tulostus();
static int print_line(char *line);
static char *spois(char *s);
static int cube_test();
static int load_fp(int k);
static int load_fp1();
static int mahal_test();
static int laske_x2(int n,int *f,double *px2,double *pp);
static int mntest_heapsort(int n,float *ra);
static int ks_test(float *data,int n,double (*func)(),double *d,double *prob);
static double probks(double alam);

/***********************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "RESULTS", "PRIND", "TEST", "EPS", "!" };
char **specs=specs0;
*****************************/

/********************
void main(argc,argv)
int argc; char *argv[];
*******************/

static void muste_mntest2(char *argv)
        {
        int i;
        char x[LLENGTH],*osa[3];
        char nimi[LNAME];

        s_init(argv);
        if (g<2)
            {
            init_remarks();
rem_pr("MNTEST <data>,<output_line>        / S.Mustonen 5.2.1996  (14.4.1996)");
rem_pr("tests whether the active part of <data> is a random sample from");
rem_pr("a multivariate normal distribution.");
rem_pr("");
rem_pr("By default the multivariate measures of skewness and kurtosis");
rem_pr("presented by Mardia (1970) are computed and asymptotic test statistics");
rem_pr("related to them as well as their P values are presented.");
rem_pr("The test statistics are computed through principal components of the");
rem_pr("data. The actual dimension m of the distribution is determined");
rem_pr("by the sizes of eigenvalues. The proportion of the last");
rem_pr("accepted eigenvalue to the largest should exceed the value given by");
rem_pr("a specification EPS=<value> (default is EPS=1e-10).");
rem_pr("");
rem_pr("Since P values of Mardia's tests can be far from truth on small sample");
rem_pr("sizes, a sucro /MSKEW determines them by simulation.");

            wait_remarks(1);

rem_pr("By specification TEST=MAHAL,<k>");
rem_pr("Mahanobis' distances of each observation from the mean are computed");
rem_pr("after determining the true dimesionality (say m) of data (by EPS).");
rem_pr("If data is a (large) sample from a multivariate normal distribution,");
rem_pr("these distances have an approximate chi^2 distribution with m degrees");
rem_pr("of freedom. This is tested by transforming the distances to uniform");
rem_pr("distribution on (0,1) by the distribution function of chi^2 and");
rem_pr("counting the # of observations in each of the <k> (default 10) subintervals.");
rem_pr("The uniformity of this frequency distribution is tested by the X^2 test");
rem_pr("and by the Kolmogorov-Smirnov test.");

            wait_remarks(1);

rem_pr("By specification TEST=CUBE,<k>");
rem_pr("the data is mapped into a m-dimensional hypercube by computing principal");
rem_pr("components and transforming them into uniformly distributed values on");
rem_pr("(0,1). The dimension m is determined in the same way as in Mardia's");
rem_pr("tests. Thus in large multivariate normal samples the transformed data values are");
rem_pr("independently and uniformly distributed in the hypercube.");
rem_pr("");
rem_pr("For each observation, the maximum and minimum values (xmax and xmin) of");
rem_pr("m standardized (variance=1) principal component values are computed and");
rem_pr("and the observation is classified in two ways. In the first");
rem_pr("classification, it belongs to class # 1+int(k*F(xmax)^m) and in the second");
rem_pr("classification to class # 1+int(k*F(-xmin)^m) where F is the distribution");
rem_pr("function of the normal distribution. This means that in both");
rem_pr("classifications the frequencies should be distributed unformly in <k>");
rem_pr("classes (default is 10). Appropriate X^2 test is performed on this basis.");
rem_pr("Also the Kolmogorov-Smirnov test is made on the max and min values");
rem_pr("of the transformed data.");

            wait_remarks(2);
            s_end(argv);
            return;
            }

        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }

        i=data_read_open(word[1],&d); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;
        m_act=d.m_act;
        if (m_act<1)
            {
            sur_print("\nAt least 1 variable must be active!"); WAIT; return;
            }

        test=MARDIA;
        i=spfind("TEST");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,osa,3);
            if (muste_strcmpi(osa[0],"MARDIA")==0) test=MARDIA;
            else if (muste_strcmpi(osa[0],"CUBE")==0)
                {
                test=CUBE; nf=10; if (i>1) nf=atoi(osa[1]);
                if (nf>100) nf=100; if (nf<=0) nf=10;
                for (i=0; i<nf; ++i) fr1[i]=fr2[i]=0L;
                }
            else if (muste_strcmpi(osa[0],"MAHAL")==0)  /* TEST=MAHAL,nm */
                {
                test=MAHAL; nm=10; if (i>1) nm=atoi(osa[1]);
                if (nm>100) nf=100; if (nm<=0) nm=10;
                for (i=0; i<nm; ++i) fm[i]=0L;
                }
            else
                {
                sprintf(sbuf,"\nUnknown TEST=%s",osa[0]);
                sur_print(sbuf); WAIT; return;
                }
            }
        i=varaa_tilat(); if (i<0) return;
        i=talleta_data(); if (i<0) return;
        data_close(&d);
        i=laske_momentit(); if (i<0) return;

        eps_dim=1e-10;
        i=spfind("EPS"); if (i>=0) eps_dim=atof(spb[i]);

        prind=1;
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);
        results1=1;
        i=spfind("RESULTS"); if (i>=0) results1=atoi(spb[i]);

        if (n<MAXIFLOAT) ks_stat=1;

        if ((test==CUBE || test==MAHAL) && ks_stat)
            {
            strcpy(nimi,etmpd); strcat(nimi,"SURVO2.TMP");
            temp2=muste_fopen(nimi,"w+b");
            if (temp2==NULL)
                {
                sprintf(sbuf,"Cannot open %s for temporary data files!",nimi);
                sur_print(sbuf); WAIT; return;
                }
            }

        i=principal_comp(); if (i<0) return;

        if (test==CUBE) { cube_test(); s_end(argv); return; }

        ind_zz=0;
        if (n*(int)dim<=MAXSPACE)
            {
            zz=(double *)muste_malloc((int)n*dim*sizeof(double));
            if (zz!=NULL) ind_zz=1;
            }

        if (ind_zz) siirto_zz();

        i=tunnusluvut();

        if (test==MAHAL) { mahal_test(); s_end(argv); return; }
        tulostus();
        s_end(argv);
        }

void muste_mntest(char *argv) // RS ADD Confirm that files are closed
	{
    xx=NULL;
    mean=NULL;
    S=NULL;
    T=NULL;
    d2=NULL;
    yy=NULL;
    zz=NULL;
    ks_stat=0;
    temp=NULL;
    temp2=NULL;
    
    muste_mntest2(argv);
    
    if (temp!=NULL) muste_fclose(temp);
    if (temp2!=NULL) muste_fclose(temp2);
    
	}

static int varaa_tilat()
        {
        xx=(double *)muste_malloc(m_act*sizeof(double));
        if (xx==NULL) { ei_tilaa(); return(-1); }
        mean=(double *)muste_malloc(m_act*sizeof(double));
        if (mean==NULL) { ei_tilaa(); return(-1); }
        S=(double *)muste_malloc(m_act*m_act*sizeof(double));
        if (S==NULL) { ei_tilaa(); return(-1); }
        T=(double *)muste_malloc(m_act*m_act*sizeof(double));
        if (T==NULL) { ei_tilaa(); return(-1); }
        d2=(double *)muste_malloc(m_act*sizeof(double));
        if (d2==NULL) { ei_tilaa(); return(-1); }
        yy=(double *)muste_malloc(m_act*sizeof(double));
        if (yy==NULL) { ei_tilaa(); return(-1); }
        return(1);
        }

static int ei_tilaa()
        {
        sur_print("\nNot enough memory!"); WAIT; return(1);
        }

static int talleta_data()
        {
        char nimi[LLENGTH];
        int i;
        int j;

        strcpy(nimi,etmpd); strcat(nimi,"SURVO.TMP");
        temp=muste_fopen(nimi,"wb");
        if (temp==NULL)
            {
            sprintf(sbuf,"Cannot open %s for temporary data files!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        n=0L;
        if (prind) sur_print("\nSaving active data in a temporary file... ");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            if (prind) { sprintf(sbuf,"%d ",j); sur_print(sbuf); }
            for (i=0; i<m_act; ++i)
                {
                data_load(&d,j,d.v[i],&xx[i]);
                if (xx[i]==MISSING8) break;
                }
            if (i<m_act)
                {
                sprintf(sbuf,"Value of variable %s missing in observation #%d!",
                              d.varname[i],j);
                sur_print(sbuf); WAIT; return(-1);
                }
            ++n;
            fwrite(xx,sizeof(double),m_act,temp);
            }
        muste_fclose(temp);
        if (n<2)
            {
            sur_print("\nLess than 2 valid observations!");
            WAIT; return(-1);
            }
        temp=muste_fopen(nimi,"r+b");

        return(1);
        }

static int laske_momentit()
        {
        int i,h;
        int j;

        if (prind) sur_print("\nComputing moments...");
        for (i=0; i<m_act; ++i)
            {
            mean[i]=0.0;
            for (h=0; h<=i; ++h)
                S[i+h*m_act]=0.0;
            }
        for (j=0L; j<n; ++j)
            {
            fread(xx,sizeof(double),m_act,temp);
            if (prind) { sprintf(sbuf," %d",j+1); sur_print(sbuf); }
            for (i=0; i<m_act; ++i)
                {
                mean[i]+=xx[i];
                for (h=0; h<=i; ++h)
                    S[i+h*m_act]+=xx[i]*xx[h];
                }
            }  /* j */

        for (i=0; i<m_act; ++i) mean[i]/=n;
        for (i=0; i<m_act; ++i)
            for (h=0; h<=i; ++h)
                {
                S[i+h*m_act]-=n*mean[i]*mean[h];
    /*          S[h+i*m_act]=S[i+h*m_act]/=(n-1);  */
                S[h+i*m_act]=S[i+h*m_act]/=n;
                }
        return (1);
        }

static int principal_comp()
        {
        int i,k;
        int j;
        double eps,tol;
        int posx,posy;
        double b;
/* CUBE */
        extern double muste_st_norm();
        double max,min;
        float fb;

        if (prind)
            {
            sprintf(sbuf,"\nPrincipal components for %d variables ... ",m_act);
            sur_print(sbuf);
            }
        eps=1e-16; tol=(1e-300)/eps;
        mat_tred2(d2,xx,S,m_act,tol);
        i=mat_tql2(d2,xx,S,m_act,eps,30);
        if (i<0)
            {
            sur_print("\nNo convergence when computing eigenvalues!");
            WAIT; return(-1);
            }

        for (i=m_act-1; i>=0; --i)
            if (d2[i]/d2[0]>eps_dim) break;
        dim=i+1;
        if (dim==0) { sur_print("\nNo variation in data!!!"); WAIT; return(-1); }
        for (k=0; k<dim; ++k)
            {
            d2[k]=1.0/sqrt(d2[k]);
            for (i=0; i<m_act; ++i) S[i+m_act*k]*=d2[k];
            }
        if (prind)
            {
            sprintf(sbuf,"\nComputing values of %d principal components ... ",dim);
            sur_print(sbuf);
            }
        rewind(temp); posx=posy=0L;
        for (j=0L; j<n; ++j)
            {
            if (prind) { sprintf(sbuf,"%d ",j+1); sur_print(sbuf); }
            muste_fseek(temp,posx,SEEK_SET); fread(xx,sizeof(double),m_act,temp);
            posx=ftell(temp);
            max=-1e10; min=-max; /* CUBE */
            for (i=0; i<m_act; ++i) xx[i]-=mean[i];

            for (k=0; k<dim; ++k)
                {
                b=0.0;
                for (i=0; i<m_act; ++i) b+=xx[i]*S[i+m_act*k];
                yy[k]=b;
                if (test==CUBE)
                    {
                    if (b>max) max=b;
                    if (b<min) min=b;
                    }
                }
            if (test==CUBE)
                {
                b=muste_st_norm(max,(double)0.0);
                b=pow(b,(double)dim);
                if (ks_stat) { fb=b; fwrite(&fb,sizeof(float),1,temp2); }
                k=nf*b;
                ++fr1[k];
                b=muste_st_norm(-min,(double)0.0);
                b=pow(b,(double)dim);
                if (ks_stat) { fb=b; fwrite(&fb,sizeof(float),1,temp2); }
                k=nf*b;
                ++fr2[k];
                }

            if (test!=CUBE)
                {
                muste_fseek(temp,posy,SEEK_SET); fwrite(yy,sizeof(double),dim,temp);
                posy=ftell(temp);
                }
            }

if (test==CUBE)
{
i=spfind("CHECK");
if (i>=0)
    {
    for (i=0; i<nf; ++i) Rprintf("\n%d %d %d",i,fr1[i],fr2[i]); sur_getch();
    }
}

        return(1);
        }

static int siirto_zz()
        {
        rewind(temp);
        fread(zz,sizeof(double),dim*(int)n,temp);
        return(1);
        }

static int tunnusluvut()
        {
        int i;
        int j,jj;
        double a,b;
        extern double sis_tulo();
        extern double muste_cdf_chi2();
        float fb;

        vin=huip=0.0;
        if (prind)
            {
            if (test==MAHAL)
                   sur_print("\nComputing Mahalanobis' distances ... ");
            else
                   sur_print("\nComputing skewness and kurtosis ... ");
            }
        for (j=0L; j<n; ++j)
            {
            if (!ind_zz) muste_fseek(temp,j*(int)dim*(int)sizeof(double),SEEK_SET);
            lue(xx,j);
            if (prind) { sprintf(sbuf,"%d ",j+1); sur_print(sbuf); }
            if (test==MAHAL)
                {
                a=sis_tulo(xx,xx,1,1,dim);
                a=muste_cdf_chi2(a,(double)dim,1e-15);
                if (ks_stat) { fb=a; fwrite(&fb,sizeof(float),1,temp2); }
                i=nm*a;
                ++fm[i];
                continue;
                }
            if (!ind_zz) rewind(temp);
            for (jj=0L; jj<n; ++jj)
                {
                lue(yy,jj);
                a=sis_tulo(xx,yy,1,1,dim);
                b=a*a;
                if (j==jj)
                    {
                    huip+=b;
                    }
                vin+=a*b;
                }
            }
        huip/=n; vin/=n*n;

i=spfind("CHECK");
if (i>=0 && test==MAHAL)
    {
    for (i=0; i<=nm; ++i) Rprintf("\n%d %d",i,fm[i]); sur_getch();
    }

        return(1);
        }

static int lue(double *xx,int j)
        {
        if (ind_zz)
            {
            memcpy(xx,zz+(unsigned int)j*dim,dim*sizeof(double));

/*      j=(unsigned int)j*dim;
        for (i=0; i<dim; ++i) xx[i]=zz[j++];
*/
            return(1);
            }
        fread(xx,sizeof(double),dim,temp);
        return(1);
        }

static int tulostus()
        {
        int i;
        char x[LLENGTH];
        char y[LLENGTH];
        char u[LLENGTH];
        extern char *spois();
        int df;
        double a;
        extern double muste_cdf_chi2();
        extern double muste_st_norm();

        i=output_open(eout);  if (i<0) return(-1);

        if (results1==0)
            {
            fnconv(vin,accuracy,y);
            fnconv(huip,accuracy,u);
            i=sprintf(x,"Multidimensional skewness=%s kurtosis=%s (Mardia 1970)",spois(y),spois(u));
            print_line(x);
            output_close(eout);
            return(1);
            }
        i=sprintf(x,"Data %s: %d active variables N=%d",word[1],m_act,n);
        print_line(x);
        i=sprintf(x,"Dimensionality of the distribution %d",dim);
        print_line(x);
        fnconv(vin,accuracy,y);
        fnconv(huip,accuracy,u);
        i=sprintf(x,"Multidimensional skewness=%s kurtosis=%s (Mardia 1970)",spois(y),spois(u));
        print_line(x);
        a=n*vin/6;
        fnconv(a,accuracy,y);
        df=dim*(dim+1)*(dim+2)/6;
        fnconv(1.0-muste_cdf_chi2(a,(double)df,1e-15),accuracy,u);
        i=sprintf(x,"Chi^2 approximation of skewness %s df=%d P=%s",spois(y),df,spois(u));
        print_line(x);
        a=(huip-dim*(dim+2))/sqrt(8.0*(double)dim*(dim+2)/n);
        fnconv(a,accuracy,y);
        fnconv(muste_st_norm(a,(double)0.0),accuracy,u);
        i=sprintf(x,"Normal approximation of kurtosis %s P=%s",spois(y),spois(u));
        print_line(x);
        output_close(eout);
        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

static char *spois(char *s)
        {
        char *p;

        while (*s==' ') ++s;
        p=s+strlen(s)-1; while (*p==' ') { *p=EOS; --p; }
        return(s);
        }
/*********************
mprint(A,m,n)
double *A;
int m,n;
        {
        int i,j;
        for (i=0; i<m; ++i)
            { printf("\n"); for (j=0; j<n; ++j) printf("%g ",A[i+m*j]); }
        getch();
        return(1);
        }
*************************/

/* mntest2.c 28.1.1996/SM (5.2.1996) (31.3.1996) (14.4.1996)
*/

static float *fp;

/* CUBE */
// extern int fr1[],fr2[];
// extern int nf;

/* MAHAL */
// extern int fm[];
// extern int nm;

static int cube_test()
        {
        int i;
        double x2,p;
        double d,prob;
        char s1[LLENGTH],s2[LLENGTH];

    fp=NULL;

        i=output_open(eout); if (i<0) return(-1);
        sprintf(sbuf,"Data %s: %d active variables N=%d",word[1],m_act,n);
        print_line(sbuf);
        sprintf(sbuf,"Dimensionality of the distribution %d",dim);
        print_line(sbuf);
        print_line("Cube test for multivariate normality:");
        laske_x2(nf,fr1,&x2,&p);
        fnconv(x2,accuracy,s1);
        fnconv(p,accuracy,s2);
        sprintf(sbuf,"chi2[max]=%s df=%d P=%s",spois(s1),nf,spois(s2));
        print_line(sbuf);
        laske_x2(nf,fr2,&x2,&p);
        fnconv(x2,accuracy,s1);
        fnconv(p,accuracy,s2);
        sprintf(sbuf,"chi2[min]=%s df=%d P=%s",spois(s1),nf,spois(s2));
        print_line(sbuf);

        if (!ks_stat) return(1);

        if (zz==NULL)
            {
            zz=(double *)muste_malloc((int)n*sizeof(float));
            if (zz==NULL) return(-1);
            }
        fp=(float *)zz;
        load_fp(0);
        mntest_heapsort((int)n,fp-1);
/* printf("\n");
   for (i=0; i<(int)n; ++i) printf("%g ",(double)fp[i]);
*/
        ks_test(fp,(int)n,NULL,&d,&prob);
/* printf("\nd=%g prob=%g",d,prob); getch(); */
        fnconv(d,accuracy,s1);
        fnconv(prob,accuracy,s2);
        sprintf(sbuf,"Kolmogorov-Smirnov D[max]=%s P=%s",spois(s1),spois(s2));
        print_line(sbuf);

        load_fp(1);
        mntest_heapsort((int)n,fp-1);
        ks_test(fp,(int)n,NULL,&d,&prob);
        fnconv(d,accuracy,s1);
        fnconv(prob,accuracy,s2);
        sprintf(sbuf,"Kolmogorov-Smirnov D[min]=%s P=%s",spois(s1),spois(s2));
        print_line(sbuf);
        output_close(eout);
        return(1);
        }

static int load_fp(int k)
        {
        int i;
        float a[2];

        rewind(temp2);
        for (i=0; i<(int)n; ++i)
            {
            fread(a,sizeof(float),2,temp2);
            fp[i]=a[k];
            }
        return(1);
        }

static int load_fp1()
        {
        int i;
        float a;

        rewind(temp2);
        for (i=0; i<(int)n; ++i)
            {
            fread(&a,sizeof(float),1,temp2);
            fp[i]=a;
            }
        return(1);
        }

static int mahal_test()
        {
        int i;
        double x2,p;
        double d,prob;
        char s1[LLENGTH],s2[LLENGTH];

        i=output_open(eout); if (i<0) return(-1);
        sprintf(sbuf,"Data %s: %d active variables N=%d",word[1],m_act,n);
        print_line(sbuf);
        sprintf(sbuf,"Dimensionality of the distribution %d",dim);
        print_line(sbuf);
        print_line("Mahalanobis distance test for multivariate normality:");
        laske_x2(nm,fm,&x2,&p);
        fnconv(x2,accuracy,s1);
        fnconv(p,accuracy,s2);
        sprintf(sbuf,"chi2=%s df=%d P=%s",spois(s1),nm,spois(s2));
        print_line(sbuf);

        if (!ks_stat) return(1);

        if (zz==NULL)
            {
            zz=(double *)muste_malloc((int)n*sizeof(float));
            if (zz==NULL) return(-1);
            }
        fp=(float *)zz;
        load_fp1();
        mntest_heapsort((int)n,fp-1);
        ks_test(fp,(int)n,NULL,&d,&prob);
        fnconv(d,accuracy,s1);
        fnconv(prob,accuracy,s2);
        sprintf(sbuf,"Kolmogorov-Smirnov D=%s P=%s",spois(s1),spois(s2));
        print_line(sbuf);
        output_close(eout);
        return(1);
        }

static int laske_x2(int n,int *f,double *px2,double *pp)
        {
        int i;
        int nt;
        double a,b,d;
        extern double muste_cdf_chi2();

        nt=0L; for (i=0; i<n; ++i) nt+=f[i];
        a=0.0; b=(double)nt/(double)n;
        for (i=0; i<n; ++i) { d=f[i]-b; a+=d*d/b; }
        *px2=a;
        *pp=1-muste_cdf_chi2(a,(double)n,1e-15);
        return(1);
        }

static int mntest_heapsort(int n,float *ra)
        {
        unsigned int i,ir,j,l;
        float rra;

        if (n<2) return(1);
        l=(n>>1)+1;
        ir=n;

        for (;;)
            {
            if (l>1) rra=ra[--l];
            else
                {
                rra=ra[ir];
                ra[ir]=ra[1];
                if (--ir==1)
                    {
                    ra[1]=rra; break;
                    }
                }
            i=l;
            j=l+l;
            while (j<=ir)
                {
                if (j<ir && ra[j]<ra[j+1]) ++j;
                if (rra<ra[j])
                    {
                    ra[i]=ra[j]; i=j;
                    j<<=1;
                    }
                else j=ir+1;
                }
            ra[i]=rra;
            }
        return(1);
        }

/* ks_test.c 13.4.1996/SM (13.4.1996)
*/


static int ks_test(float *data,int n,double (*func)(),double *d,double *prob)
        {
        int j;
        double fo=0.0,fn,ff,en,dt;
        double probks();
        double b;

        en=n;
        *d=0.0;

        for (j=0; j<n; ++j)
            {
            fn=(double)(j+1)/en;
            if (func==NULL) ff=data[j];
            else ff=(*func)(data[j]);
            dt=fabs(fo-ff); b=fabs(fn-ff);
            if (b>dt) dt=b;
            if (dt>*d) *d=dt;
            fo=fn;
            }
        *prob=probks(sqrt(en)*(*d));
        return(1);
        }

#define EPS1 0.001
#define EPS2 1.0e-8

static double probks(double alam)
        {
        int j;
        double a2,fac=2.0,sum=0.0,term,termbf=0.0;

        a2=-2.0*alam*alam;
        for (j=1; j<=100; ++j)
            {
            term=fac*exp((double)(a2*j*j));
            sum+=term;
            if (fabs(term)<=EPS1*termbf || fabs(term)<EPS2*sum)
                return(sum);
            fac=-fac;
            termbf=fabs(term);
            }
        return(0.0);
        }

