/* !linreg.c 26.2.1986/SM (13.12.1991)
      muunnelma !corr.c
*/


#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
// #include <process.h>
#include "survo.h"
#include "survoext.h"
//#include "survodat.h"
#include "survolib.h"

#define YMAX 100

static SURVO_DATA d;

static char *ptila;       /* dynaamisten tilojen osoitin */
static double *x;         /* havaintovektori */
static double *x0;        /* keskistysvektori (1.havainto) */
static double *sum;       /* summat */
static double *sum2;      /* neliösummat */
static char **varname;    /* muuttujien nimet */
static char *lab;         /* matriisin rivi/sar.otsikot yhtenä jonona */
static double *A;         /* momentit */

static long n,n1;
static double weightsum;
static int painomuuttuja;
static int tulosrivi;
static int m;
static char aineisto[LNAME];

static int nyvar, nxvar;
static int yvariable[YMAX];
static int xvariable[EP4];
static int keyind=1;
static char corrfile[LNAME];
static double nsuhde;
/* static char *argv1; */
static char *argv1;  // kokeilu 15.6.2011
/* SM
static char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "RESULTS", "PRIND", "!" };
static char **specs=specs0;
*/

static double *invR;
static double invc;  /* 26.7.90 */
static double *reg_stddev;
static double dw_stat;
static double stddev0;

#define N_RG 15
static double rg[N_RG];

#define _n        0
#define _k        1
#define _df       2
#define _Wsum     3
#define _Yvar     4
#define _SST      5
#define _SSE      6
#define _SSR      7
#define _MSE      8
#define _Resvar   9
#define _R       10
#define _R2      11
#define _DW      12
#define _F       13
#define _PF      14


static double *S,*b,*bs;
static double constant;
static double vy,vres;

extern int Rprintf();
extern int matrix_save0();

static void not_enough_memory()
        {
        sur_print("\nNot enough memory! (LINREG)");
        WAIT;
        }

static int n_strcat(char *x,int len,char *s)
    {
    strcat(x,s); strncat(x,space,len-strlen(s));
    return(1);
    }

static char *spois(char *s)
        {
        while (*s && *s==' ') ++s;
        return(s);
        }


/* cholinv.c 22.7.85/SM (5.4.1986)
*/
/*****************************
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <math.h>
**************************/
/*
  B=INV(A), A positive definite    double A[(n+1)*n]

   n=5
        0   1   2     n-1
0     A00 A01 A02 A03 A04
1     B00 A11 A12 A13 A14
2     B10 B11 A22 A23 A24
      B20 B21 B22 A33 A34
n-1   B30 B31 B32 B33 A44
n     B40 B41 B42 B43 B44

Normally 1 is returned.
If A is not positive definite, -j (j first var dependent on previous ones)
is returned.

*/
static int cholinv(double *a,int n)
        {
        int i,j,k,i1,j1;
        double z,x,y=0;

        for (i=0; i<n; ++i)
            {
            i1=i+1;
            for (j=i; j<n; ++j)
                {
                j1=j+1;
                x=a[n*i+j];
                for (k=i-1; k>=0; --k)
                    x-=a[n*j1+k]*a[n*i1+k];
                if (j==i)
                    {
                    if (x<=1e-15) return(-j-1);
                    a[n*i1+i]=y=1/sqrt(x);
                    }
                else a[n*j1+i]=x*y;
                }
            }

        for (i=0; i<n; ++i)
        for (j=i+1; j<n; ++j)
            {
            z=0;
            j1=j+1;
            for (k=j-1; k>=i; --k)
                z-=a[n*j1+k]*a[n*(k+1)+i];
            a[n*j1+i]=z*a[n*j1+j];
            }
        for (i=0; i<n; ++i)
        for (j=i; j<n; ++j)
            {
            z=0;
            j1=n;
            for (k=j+1; k<=j1; ++k)
                z+=a[n*k+j]*a[n*k+i];
            a[n*(j+1)+i]=z;
            }
        return(1);
        }

static int varaa_tilat()
        {
        int tila;
        int x_tila, x0_tila, sum_tila, sum2_tila, nimet, matots;
        unsigned int A_tila;
        char *p;

        x_tila=x0_tila=sum_tila=sum2_tila=m*sizeof(double);
        nimet=m*sizeof(char **);
        matots=8*m;
        tila=x_tila+x0_tila+sum_tila+sum2_tila+nimet+matots;
        ptila=malloc(tila);
        if (ptila==NULL) { not_enough_memory(); return(-1); }
        p=ptila;
        x=(double *)p;    p+=x_tila;
        x0=(double *)p;   p+=x0_tila;
        sum=(double *)p;  p+=sum_tila;
        sum2=(double *)p; p+=sum2_tila;
        varname=(char **)p; p+=nimet;
        lab=p;            p+=matots;

        A_tila=m*m*sizeof(double);
        A=(double *)malloc(A_tila);
        if (A==NULL)
            {
            sur_print("\nNot enough memory! (LINREG)");
            WAIT; return(-1);
            }
        return(1);
        }


static int eoutput(char *rivi)
        {
        output_line(rivi,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

static int momentit()
        {
        int i,j,k;
        long l;

        n=n1=0L; weightsum=0.0;
        for (i=0; i<m; ++i)
            {
            sum[i]=0.0;
            for (j=0; j<=i; ++j)
                A[i+m*j]=0.0;
            }

        for (i=0; i<m; ++i)
            {
            k=data_load(&d,d.l1,d.v[i],&x0[i]);
            if (k<0) return(-1);
            if (x0[i]==MISSING8) x0[i]=0.0;
            }

        i=hae_apu("prind",sbuf); if (i) keyind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) keyind=atoi(spb[i]);

        sur_print("\n");
        for (l=d.l1; l<=d.l2; ++l)
            {
            double paino;   // ,ind; 16.6.2011 SM

            if (unsuitable(&d,l)) continue;
            if (painomuuttuja==-1) paino=1.0;
            else
                {
                k=data_load(&d,l,painomuuttuja,&paino);
                if (k<0) return(-1);
                if (paino==MISSING8) continue;
                }
            if (keyind) { sprintf(sbuf,"%ld ",l); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); keyind=1-keyind; }
//          if (kbhit()) { getch(); keyind=1-keyind; }
            ++n;
            for (i=0; i<m; ++i)
                {
                k=data_load(&d,l,d.v[i],&x[i]);
                if (k<0) return(-1);
                if (x[i]==MISSING8) break;
                x[i]-=x0[i];  /* keskistys */
                }
            if (i<m) { sur_print("- "); continue; }

            ++n1; weightsum+=paino;
            for (i=0; i<m; ++i)
                {
                double z;

                if (d.v[i]==painomuuttuja) continue;
                z=paino*x[i];
                sum[i]+=z;
                for (j=0; j<=i; ++j)
                    A[i+m*j]+=z*x[j];
                }
            } /* l */

        if (n1<2 || weightsum<=0.0)
            {
            if (etu==2)
                {
                strcpy(tut_info,"___@24@LINREG@Too few observations!@");
                s_end(argv1);
                return(-1);
                }
            sur_print("\nLess than 2 observations or sum of weights<=0");
            WAIT; return(-1);
            }
        nsuhde=(double)(n1-1L)/(double)n1;
        for (i=0; i<m; ++i)
            for (j=0; j<=i; ++j)
                A[i+m*j]-=sum[i]*sum[j]/weightsum;
        for (i=0; i<m; ++i)
            {
            sum[i]=sum[i]/weightsum+x0[i];

            if (A[i+m*i]==0.0)
                {
                sprintf(sbuf,"Variable %s is a constant %g!",
                            d.varname[d.v[i]],sum[i]);
                if (etu==2)
                    {
                    sprintf(tut_info,"___@25@LINREG@%s@",sbuf);
                    s_end(argv1);
                    return(-1);
                    }
                sur_print("\n"); sur_print(sbuf); WAIT; return(-1);
                }
            sum2[i]=sqrt(A[i+m*i]);
            for (j=0; j<=i; ++j)
                {
                double xxx;

                A[i+m*j]/=sum2[i]*sum2[j];
                xxx=A[i+m*j];
                A[j+m*i]=xxx;
                }
            }

        for (i=0; i<m; ++i) sum2[i]/=sqrt(nsuhde*weightsum);

        return(1);
        }


static int corrp_linreg(
double S[],
int m,     /* dimensio */
char *xname[],
int   lev, /* kentÃ¤n leveys */
int   sar, /* sarakkeen leveys */
int   des, /* desimaaleja */
char otsikko[]
)
        {
        int i,j,k;
        int j1,j2,j3;
        char rivi[LLENGTH];
        char xsar[10], xriv[10], rriv[10];
        char ind[10];
        char nimi[LLENGTH];
        extern char *eout;


        output_open(eout);
        strcpy(xsar," %"); strcat(xsar,muste_itoa(sar-1,ind,10)); strcat(xsar,"s");
        strcpy(xriv," %-"); strcat(xriv,muste_itoa(sar+3,ind,10)); strcat(xriv,"s");
        strcpy(rriv,"%"); strcat(rriv,muste_itoa(sar,ind,10)); strcat(rriv,".");
        strcat(rriv,muste_itoa(des,ind,10)); strcat(rriv,"f");
        sprintf(rivi,"%s",otsikko);
        eoutput(rivi);

        j1=0; j2=m-1;
        j3=(int)floor((double)((lev-sar-3)/sar));
        if (j3<m) j2=j3-1;
        while (j2<=m-1)
            {
            k=0;
            for (i=0; i<sar+4; ++i) k+=sprintf(rivi+k," ");
            for (j=j1; j<=j2; ++j)
                {
                strcpy(nimi,xname[j]); nimi[sar-1]=EOS;
                k+=sprintf(rivi+k,xsar,nimi);
                }
            eoutput(rivi);
            for (i=0; i<m; ++i)
                {
                strcpy(nimi,xname[i]); nimi[sar+2]=EOS;
                k=sprintf(rivi,xriv,nimi);
                for (j=j1; j<=j2; ++j)
                    k+=sprintf(rivi+k,rriv,S[i+j*m]);

                eoutput(rivi);
                }
            j1=j2+1;
            if (j2==m-1) ++j2;
            else         { j2+=j3; if (j2>m-1) j2=m-1; }
            }
        output_close(eout);

        return(1);
        }

static int tulostus()
        {
        int i;
        char rivi[LLENGTH];

        i=output_open(eout); if (i<0) return(-1);
        sprintf(rivi,"Means, std.devs and correlations of %s  N=%ld",
                          word[1],n);
        if (painomuuttuja>=0)
            {
            strcat(rivi," Weight="); strncat(rivi,d.varname[painomuuttuja],8);
            }
        eoutput(rivi);
        if (n1<n)
            {
            sprintf(rivi,"# of missing observations=%ld",n-n1);
            eoutput(rivi);
            }
        sprintf(rivi,"Variable  Mean %.*s Std.dev.",accuracy-1,space);
        eoutput(rivi);
        for (i=0; i<m; ++i)
            {
            char mean[32],stddev[32];
            fnconv(sum[i],accuracy+2,mean);
            fnconv(sum2[i],accuracy+2,stddev);
            sprintf(rivi,"%-8.8s %*s   %*s",d.varname[d.v[i]],
                         accuracy+2,mean,accuracy+2,stddev);
            eoutput(rivi);
            }
        output_close(eout);

        for (i=0; i<m; ++i) varname[i]=d.varname[d.v[i]];
// Rprintf("\ncorrp 16.6.2011"); sur_getch();
        corrp_linreg(A,m,varname,c3,accuracy+1,accuracy-3,"Correlations:");
        output_open(eout);
        *rivi=EOS; eoutput(rivi);
        output_close(eout);

        return(1);
        }








char *lab_corr;
static int save_corr(double *A,int m)
    {
    int i,j;
    int mm;
    double s;
    extern double *invR;
    extern double *reg_stddev;
//  extern double invc;        16.6.2011 SM
    extern double stddev0;

    if (nyvar>1) return(1);

    lab_corr=(char *)malloc(m*8);
    if (lab_corr==NULL) { not_enough_memory(); return(-1); }
/******************************************
    for (i=0; i<m*8; ++i) lab_corr[i]=' ';
    for (i=0; i<m; ++i)
        {
        strncpy(lab_corr+i*8,d.varname[d.v[i]],strlen(d.varname[d.v[i]]));
        }
    strcpy(sbuf,"reg_corr("); strcat(sbuf,word[1]); strcat(sbuf,")");
    matrix_save("REG_CORR.M",A,m,m,lab_corr,lab_corr,8,8,-1,sbuf,0,0);
***************************/
    mm=nxvar;

// mprint(invR,mm,mm);
    for (i=0; i<mm; ++i) A[i]=1.0/sqrt(invR[i*(mm+1)]);

    for (i=0; i<mm; ++i)
    for (j=0; j<mm; ++j)
        invR[i+mm*j]*=A[i]*A[j];
    for (i=0; i<mm; ++i)
    for (j=0; j<mm; ++j)
         A[i+1+m*(j+1)]=invR[i+mm*j];

    for (i=0; i<mm; ++i)
        {
        s=0.0;
        for (j=0; j<mm; ++j)
            {
// printf("\n%g %g %g|",sum[j+1],reg_stddev[j+1],A[i+1+m*(j+1)]); getch();
            s+=sum[j+1]*reg_stddev[j+1]*A[i+1+m*(j+1)];
            }
// printf("\ns=%g dev0=%g|",s,stddev0); getch();
        A[i+1]=A[(i+1)*m]=-s/stddev0;
        }
    A[0]=1.0;
    for (i=0; i<m*8; ++i) lab_corr[i]=' ';
    for (i=0; i<mm; ++i)
        {
        strncpy(lab_corr+(i+1)*8,d.varname[d.v[xvariable[i]]],strlen(d.varname[d.v[xvariable[i]]]));
        }
    strncpy(lab_corr,"Constant",8);
    strcpy(sbuf,"reg_corr("); strcat(sbuf,word[1]); strcat(sbuf,")");
    matrix_save("REG_CORR.M",A,m,m,lab_corr,lab_corr,8,8,-1,sbuf,0,0);


// d.varname[d.v[xvariable[i]]]

    free(lab_corr);
    return(1);
    }


/* lin2.c 5.4.1986/SM (4.4.1992) (5.8.1996)
      LINREG
*/

static int varaa_matriisit()
        {
/*
        if (nxvar>90)
           {
           sur_print("\nToo many regressors!");
           WAIT; return(-1);
           }
*/
        S=(double *)malloc(nxvar*(nxvar+1)*sizeof(double));
        if (S==NULL) { not_enough_memory(); return(-1); }
        b=(double *)malloc(nxvar*sizeof(double));
        if (b==NULL) { not_enough_memory(); return(-1); }
        bs=(double *)malloc(nxvar*sizeof(double));
        if (bs==NULL) { not_enough_memory(); return(-1); }
        invR=S+nxvar;
        return(1);
        }

static int kokoa_momentit()
        {
        int i,j;

        for (i=0; i<nxvar; ++i)
            for (j=i; j<nxvar; ++j)
                S[i*nxvar+j]=A[xvariable[i]*m+xvariable[j]];
        return(1);
        }

static int symmetrisoi()
        {
        int i,j;
        double xxx;

        for (i=0; i<nxvar; ++i)
            for (j=i+1; j<nxvar; ++j)
                {
                xxx=invR[j*nxvar+i];
                invR[i*nxvar+j]=xxx;
                }

//      mprint(invR,nxvar,nxvar);
        return(1);
        }
/**************************************
static int mprint(double *a,int m,int n)
    {
    int i,j;

    for (i=0; i<m; ++i)
        {
        Rprintf("\n");
        for (j=0; j<n; ++j) Rprintf("%g ",a[i+m*j]);
        }
    sur_getch();
    return(1);
    }
**************************************/
static int regrkert(int k)
        {
        int i,j,yvar;
        double z;

        yvar=yvariable[k]; constant=sum[yvar];
        for (j=0; j<nxvar; ++j)
            {
            z=0.0;
            for (i=0; i<nxvar; ++i)
                z+=invR[i+nxvar*j]*A[xvariable[i]+m*yvar];
            b[j]=z*sum2[yvar]/sum2[xvariable[j]];
            bs[j]=z;
            constant-=b[j]*sum[xvariable[j]];
            }
        return(1);
        }

static int resvariance(int k)
        {
        int i,j;
        double s,a;

        s=0.0;
        for (i=0; i<nxvar; ++i)
            for (j=0; j<nxvar; ++j)
                    s+=bs[i]*bs[j]*A[xvariable[i]+m*xvariable[j]];
        a=sum2[yvariable[k]];
        vy=a*a;
        vres=(1.0-s)*vy*(double)(n1-1L)/(double)(n1-(long)nxvar-1L);
        return(1);
        }

static int laske_invc()
        {
        int i,j;
        int ii,jj;

        invc=0.0;
        for (i=0; i<nxvar; ++i)
            {
            ii=xvariable[i];
            for (j=0; j<nxvar; ++j)
                {
                jj=xvariable[j];
                invc+=sum[ii]*sum[jj]
                     *invR[i+nxvar*j]/sum2[ii]/sum2[jj];
                }
            }
        invc*=weightsum/(double)(n1-1L)*(double)n1;

/*      invc=(1+invc/weightsum)/weightsum;   muutettu 4.4.92  */
        invc=(1+invc/weightsum)/n;
        return(1);
        }

static int regtulostus(int k)
        {
        int i,h;
        char rivi[LLENGTH];
        char sana[32];
        double a,stddev;
        char *spois();

        reg_stddev=(double *)malloc(m*sizeof(double)); // 12.4.2005
        if (reg_stddev==NULL) { not_enough_memory(); return(-1); }

        if (k==0) { i=output_open(eout); if (i<0) return(-1); }
        i=sprintf(rivi,"Linear regression analysis: Data %s",word[1]);
        i+=sprintf(rivi+i,", Regressand %.8s",d.varname[d.v[yvariable[k]]]);
        i+=sprintf(rivi+i,"  N=%ld",n1);
        rg[_n]=(double)n1;
        rg[_k]=(double)m;
        rg[_df]=(double)(n1-m);
        if (painomuuttuja) rg[_Wsum]=weightsum; else rg[_Wsum]=rg[_n];


        eoutput(rivi);
        i=0;
        if (n>n1)
            i=sprintf(rivi,"N(missing)=%ld  ",n-n1);
        if (painomuuttuja>=0)
            {
            i+=sprintf(rivi+i,"Weight variable: %.8s",d.varname[painomuuttuja]);
            fnconv(weightsum,accuracy+2,sana);
            i+=sprintf(rivi+i,", sum of weights=%s",sana);
            }
        if (i) eoutput(rivi);

        h=sprintf(rivi,"Variable Regr.coeff. %.*sStd.dev.",accuracy-4,space);
        h+=sprintf(rivi+h," %.*st %.*s beta",accuracy-4,space,accuracy-4,space);
        eoutput(rivi);
        if (vres<0.0) vres=0.0;  // 12.10.2000
        for (i=0; i<nxvar; ++i)
            {
            a=sum2[xvariable[i]];
            stddev=sqrt(invR[i*(nxvar+1)]*vres/a/a/(double)(n1-1L));
            reg_stddev[i+1]=stddev; // 12.4.2005
            h=sprintf(rivi,"%.8s",d.varname[d.v[xvariable[i]]]);
            fnconv(b[i],accuracy+2,sana);
            h+=sprintf(rivi+h," %s     ",sana);
            fnconv(stddev,accuracy+2,sana);
            h+=sprintf(rivi+h," %s",sana);
            if (stddev>0.0 && b[i]/stddev<1e5)
                {
                fnconv(b[i]/stddev,accuracy-1,sana);
                h+=sprintf(rivi+h," %s",sana);
                fnconv(bs[i],accuracy-1,sana);
                h+=sprintf(rivi+h," %s",sana);
                }
            eoutput(rivi);
            }
        fnconv(constant,accuracy+2,sana);
        h=sprintf(rivi,"constant %s     ",sana);
        stddev0=stddev=sqrt(invc*vres);
        reg_stddev[0]=stddev; // 12.4.2005
        if (results!=3)  /* M.Karpojan STEPREG-sukron vuoksi */
            {
            fnconv(stddev,accuracy+2,sana);
            h+=sprintf(rivi+h," %s",sana);
            if (stddev>0.0 && constant/stddev<1e5)
                {
                fnconv(constant/stddev,accuracy-1,sana);
                h+=sprintf(rivi+h," %s",sana);
                }
            }
        eoutput(rivi);

        strncpy(sana,d.varname[d.v[yvariable[k]]],8);
        h=8; sana[h]=EOS; while(sana[h-1]==' ') sana[--h]=EOS;
        h=sprintf(rivi,"Variance of regressand %s",sana);
        fnconv(vy,accuracy+5,sana);
        rg[_Yvar]=vy; rg[_SST]=(double)(n1-1L)*vy;
        h+=sprintf(rivi+h,"=%s df=%ld",spois(sana),n1-1L);
        eoutput(rivi);

        rg[_SSE]=(double)(n1-(long)nxvar-1L)*vres;
        rg[_SSR]=rg[_SST]-rg[_SSE];
        rg[_MSE]=rg[_Resvar]=vres;
        fnconv(vres,accuracy+5,sana);
        h=sprintf(rivi,"Residual variance=%s df=%ld",spois(sana),n1-(long)nxvar-1L);
        eoutput(rivi);

        a=1.0-vres/(vy*(double)(n1-1L)/(double)(n1-(long)nxvar-1L));
   /*   a=1.0-vres/vy;          */
        rg[_R2]=a; rg[_R]=sqrt(a);
        fnconv(sqrt(a),accuracy,sana);
        h=sprintf(rivi,"R=%s",spois(sana));
        fnconv(a,accuracy,sana);
        h=sprintf(rivi+h," R^2=%s",spois(sana));
        eoutput(rivi);
        if (k==nyvar-1) output_close(eout);
             /* ennen 8.5.90 k==nyvar */
        return(1);
        }

#define ERC 128
#define N_LINES 16
static int save_lg_matrix()
    {
//  extern double cdf_f(); // 14.6.2011 ei löydy: siirretty alkuun
    char label[8*N_RG+1];
    char text[N_LINES*ERC+1];
    char text1[50];

    strcpy(label,"n       k       df      Wsum    Yvar    SST     ");
    strcat(label,"SSE     SSR     MSE     Resvar  R       ");
    strcat(label,"R2      DW      F       Pr_F    ");

    rg[_DW]=dw_stat;
    rg[_F]=rg[_SSR]/(m-1)/rg[_MSE];
    rg[_PF]=1.0-muste_cdf_f(rg[_F],rg[_k]-1.0,rg[_df],1e-15);
//  rg[_PF]=0.0; // 15.6.2011 tilap. SM

    *text=EOS;
    sprintf(text1,"LINREG statistics from data %s",word[1]);
    n_strcat(text,ERC,text1);
    n_strcat(text,ERC,"/n: # of cases");
    n_strcat(text,ERC,"/k: # of regression coefficients");
    n_strcat(text,ERC,"/df: n-k");
    n_strcat(text,ERC,"/Wsum: Sum of weights");
    n_strcat(text,ERC,"/Yvar: variance of regressand");
    n_strcat(text,ERC,"/SST: total sum of squares");
    n_strcat(text,ERC,"/SSE: residual sum of squares");
    n_strcat(text,ERC,"/SSR: SST-SSE");
    n_strcat(text,ERC,"/MSE: mean square error");
    n_strcat(text,ERC,"/Resvar: MSE");
    n_strcat(text,ERC,"/R: multiple correlation coefficient");
    n_strcat(text,ERC,"/R2: R^2");
    n_strcat(text,ERC,"/DW: Durbin-Watson statistics");
    n_strcat(text,ERC,"/F: Overall F-test");
    n_strcat(text,ERC,"/Pr_F: P-value of F-test");

    matrix_save0("LG.M",rg,N_RG,1,label,"LINREG  ",8,8,-1,"LG.M",
                  N_LINES,0,text);
    return(1);
    }

static int residuals(int k)
        {
        int i;
        int resvar,predvar;
        long j;
        int miss;
        double d1,d2,xd;
        int dw;
        long ndw;
        char sana[LLENGTH];

        if (nyvar>1) return(1);

        resvar=activated(&d,'R');
        predvar=activated(&d,'P');
        if (resvar<0 && predvar<0) return(1);

        sur_print("\nSaving ");
        if (resvar>=0) { sprintf(sbuf,"\nresiduals as variable %.8s...",
                                        d.varname[resvar]);
                         sur_print(sbuf);
                       }
        if (predvar>=0) { sprintf(sbuf,"\npredicted values of model as variable %.8s...",
                                        d.varname[predvar]);
                          sur_print(sbuf);
                        }

        i=data_to_write(aineisto,&d); if (i<0) return(-1);

        dw=1; ndw=0L; d1=d2=0.0;  /* d1,d2 5.8.1996 */
        xd=0.0; // 16.6.2011 SM (ei välttämätön)
        for (j=d.l1; j<=d.l2; ++j)
            {
            double x,pred;

            pred=constant; miss=0;
            for (i=0; i<nxvar; ++i)
                {
                data_load(&d,j,d.v[xvariable[i]],&x);
                if (x==MISSING8) { miss=1; break; }
                pred+=b[i]*x;
                }
            if (miss) pred=MISSING8;
            if (resvar>=0)
                {
                data_load(&d,j,d.v[yvariable[k]],&x);
                if (x!=MISSING8 && pred!=MISSING8) x-=pred;
                else x=MISSING8;
                i=data_save(&d,j,resvar,x);
                if (i<0) return(-1);
                if (dw)
                    {
                    if (x==MISSING8) dw=0;
                    else
                        {
                        if (ndw>0L)
                            d1+=(x-xd)*(x-xd);
                        d2+=x*x;
                        xd=x; ++ndw;
                        }
                    }
                }
            if (predvar>=0)
                {
                i=data_save(&d,j,predvar,pred);
                if (i<0) return(-1);
                }

            if (keyind) { sprintf(sbuf," %ld",j); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); keyind=1-keyind; }
//          if (kbhit()) { getch(); keyind=1-keyind; }
            }

        /* 11.10.1996 */
        if (resvar>=0) update_varname(&d,resvar,"Residual from LINREG");
        if (predvar>=0) update_varname(&d,predvar,"Predicted value from LINREG");

        if (dw && ndw>0L && d2>0)
            {
            output_open(eout);
            dw_stat=d1/d2;
            fnconv(d1/d2,accuracy,sana);
            sprintf(sbuf,"DW=%s",spois(sana));
            eoutput(sbuf);
            output_close(eout);
            }
        return(1);
        }


static int regressiolaskut()
        {
        int i,k;

        i=varaa_matriisit(); if (i<0) return(-1);
        kokoa_momentit();
        i=cholinv(S,nxvar);
        if (i<0)
            {
            sprintf(sbuf,"Regressor %.8s is linearly dependent on the previous regressors!",
                       d.varname[d.v[xvariable[-i-1]]]);
            if (etu==2)
                {
                sprintf(tut_info,"___@26@LINREG@%s@",sbuf);
                s_end(argv1);
                return(-1);
                }
            sur_print("\n"); sur_print(sbuf);
            WAIT; return(-1);
            }
        symmetrisoi();
        laske_invc(); /* vakiotermin hajontaa varten  26.7.90 */
        for (k=0; k<nyvar; ++k)
            {
            regrkert(k);
            resvariance(k);
            i=regtulostus(k); if (i<0) return(-1);
            i=residuals(k); if (i<0) return(-1);

            }
        return(1);
        }


static int regr_talletus()
        {

        int i,h;
        char expr[LLENGTH];
//      char *p;       16.6.2011 SM
        int m2;
        double *reg;
        char *label;
        char name[9];

        if (nyvar>1) return(1);
        m2=m;
        reg=(double *)malloc(2*m2*sizeof(double));
        if (reg==NULL) { not_enough_memory(); return(-1); }
        label=(char *)malloc(m2*8);
        if (label==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<8*m2; ++i) label[i]=' ';
        strncpy(label,"Constant",8);
        for (i=1; i<m2; ++i)
            {
            strncpy(name,d.varname[d.v[xvariable[i-1]]],8); name[8]=EOS;
            for (h=0; h<8; ++h)
                {
                if (name[h]==EOS) break;
                label[8*i+h]=name[h];
                }
            }

        reg[0]=constant; reg[m2]=reg_stddev[0];
        for (i=1; i<m2; ++i) { reg[i]=b[i-1]; reg[i+m2]=reg_stddev[i]; }

        sur_print("\nSaving regression coefficients in REG.M");
        strcpy(expr,"regr("); strcat(expr,aineisto); strcat(expr,")");
        matrix_save("REG.M",reg,m2,1,label,"%1      ",8,8,-1,expr,0,0);
        strcpy(expr,"regs("); strcat(expr,aineisto); strcat(expr,")");
        matrix_save("REGS.M",reg,m2,2,label,"Coeff   Std.dev.",8,8,-1,expr,0,0);
        save_lg_matrix(); // 12.4.2005

        if (m==2)  // 31.3.2005
            {
            double mx,my,sx,sy,r;
            double a,b;

// printf("y=%d x=%d",yvariable[0],xvariable[0]); getch();

            if (yvariable[0]>xvariable[0])
                {
                mx=sum[0]; my=sum[1];
                sx=sum2[0]; sy=sum2[1];
                }
            else
                {
                mx=sum[1]; my=sum[0];
                sx=sum2[1]; sy=sum2[0];
                }
            r=A[1];
//          printf("\nm2=%g m1=%g",sum[0],sum[1]);
//          printf("\ns2=%g s1=%g r=%g",sum2[0],sum2[1],A[1]);
//          getch();


            if (fabs(sx-sy)<1e-10)
                {
                b=1.0;
                if (r<0.0) b=-1.0;
                }
            else
                {
                if (sy>=sx)
                    {
                    b=2*sx*sy*r/(sx*sx-sy*sy);
                    b=-(sqrt(b*b+1)+1.0)/b;
                    }
                else
                    {
                    b=2*sx*sy*r/(sx*sx-sy*sy);
                    b=(sqrt(b*b+1)-1.0)/b;
                    }
                }
            a=my-b*mx;
            reg[0]=a; reg[1]=b;

            strcpy(expr,"O_REGR("); strcat(expr,aineisto); strcat(expr,")");
            matrix_save("OREG.M",reg,m2,1,label,"%1      ",8,8,-1,expr,0,0);
            }

        free(reg); free(label);
        return(1);
        }


/* linr.c 9.11.1986/SM (7.9.1987)
      LINREG  corr_momentit()
*/

static int corr_ind(int i,char *lab,int len,int dim,char *filename)
        {
        char nimi[9];
        char nimi2[32];
        int k,h;

        strncpy(nimi,d.varname[d.v[i]],8); nimi[8]=EOS;
        k=8; while (nimi[k-1]==' ') nimi[--k]=EOS;
        for (h=0; h<dim; ++h)
            {
            strncpy(nimi2,lab+h*len,len); nimi2[8]=EOS;
            k=8; while (nimi2[k-1]==' ') nimi2[--k]=EOS;
            if (strcmp(nimi,nimi2)==0) return(h);
            }
        sprintf(sbuf,"\nVariable %s not in file %s",nimi,filename);
        sur_print(sbuf); WAIT;
        return(-1);
        }

static int corr_momentit()
        {
        int i,j;
        char *corrf;
        char filename[LNAME];
        double *R;
        int rdim,cdim;
        char *rlab,*clab;
        int lr,lc,type;
        char expr[129];
        int *vc;   /* corr-valintavektori */
//      char *p;     16.6.2011 SM
        double *V;
        int vrdim,vcdim;
        char *vrlab,*vclab;
        int vlr,vlc,vtype;
        char vexpr[129];

        vc=NULL; R=NULL; V=NULL; rlab=NULL; clab=NULL; vrlab=NULL; vclab=NULL;
        corrf=corrfile;
        if (*corrfile=='.') corrf=corrfile+1;
        strcpy(filename,"CORR."); strcat(filename,corrf);

        i=matrix_load(filename,&R,&rdim,&cdim,&rlab,&clab,&lr,&lc,&type,expr);

        if (i<0) return(-1);

        if (rdim!=cdim)
            {
            sprintf(sbuf,"\n%s not a correlation file!",filename);
            sur_print(sbuf); WAIT; return(-1);
            }

        vc=(int *)malloc(m*sizeof(int));
        if (vc==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<m; ++i)
            {
            vc[i]=corr_ind(i,rlab,lr,rdim,filename);
            if (vc[i]<0) return(-1);
            }

        for (i=0; i<m; ++i)
            for (j=0; j<m; ++j)
                A[i*m+j]=R[vc[i]*rdim+vc[j]];

        strcpy(filename,"MSN."); strcat(filename,corrf);
        i=matrix_load(filename,&V,&vrdim,&vcdim,&vrlab,&vclab,&vlr,&vlc,&vtype,vexpr);
        if (i<0) return(-1);
        if (vrdim!=rdim || strncmp(vrlab,rlab,rdim*lr)!=0)
            {
            sprintf(sbuf,"\nFile %s is not compatible with correlation file!",filename);
            sur_print(sbuf); WAIT; return(-1);
            }
        for (i=0; i<m; ++i) sum[i]=V[vc[i]];
        for (i=0; i<m; ++i) sum2[i]=V[rdim+vc[i]];
        n=n1=(long)V[2*rdim];
        weightsum=(double)n;


        free(V); free(vrlab); free(vclab);
        free(vc);
        free(R); free(rlab); free(clab);

        return(1);
        }


/* SM cha
main(argc,argv)
int argc; char *argv[]; */

void muste_linreg(char *argv)
        {
        int i,k;
        char *p;
//      int max_dim;  16.6.2011 SM

/*      if (argc==1) return;  */
        s_init(argv);
/*      argv1=argv[1];     */
        argv1=argv;
/*      s_opt(argv[2]);    */
        if (g<2)
            {
            sur_print("\nUsage: LINREG <SURVO_data>,<output_line>");
            WAIT; return;
            }
        tulosrivi=0;
        if (g>2)
            {
            tulosrivi=edline2(word[2],1,1);
            if (tulosrivi<0) return;
            }

        strcpy(aineisto,word[1]);
        *corrfile=EOS;
        p=strchr(aineisto,'>');
        if (p!=NULL)
            {
            *p=EOS;
            strcpy(corrfile,p+1);
            }
        i=data_read_open(aineisto,&d); if (i<0) { s_end(argv[1]); return; }
        i=spec_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) { s_end(argv[1]); return; }
        scales(&d);
        i=conditions(&d); if (i<0) { s_end(argv[1]); return; }
        i=spfind("RESULTS"); if (i>=0) results=atoi(spb[i]);
/******************
        i=optdim_d(); if (i && i<d.m) err(0);
        i=optdim_o(); if (i && (long)i<d.n) err(0);
*****************/

        painomuuttuja=activated(&d,'W');
        if (painomuuttuja>=0 && !scale_ok(&d,painomuuttuja,RATIO_SCALE))
            {
            sprintf(sbuf,"\nWeight variable %.8s has not ratio scale!",
                        d.varname[painomuuttuja]); sur_print(sbuf);
            WAIT; if (scale_check==SCALE_INTERRUPT) return;
            }
        k=0;
        nyvar=nxvar=0;
        for (i=0; i<d.m; ++i)
            {
            char ch=d.vartype[i][1];
            if (ch=='-' || ch=='W' || (ch!='X' && ch!='Y')) continue;
            d.v[k++]=i;
            if (ch=='X') xvariable[nxvar++]=k-1;
            if (ch=='Y')
                {
                if (nyvar==YMAX)
                    {
                    sprintf(sbuf,"\nToo many (more than %d) regressands (Y)",
                                                YMAX); sur_print(sbuf);
                    WAIT; return;
                    }
                yvariable[nyvar++]=k-1;
                }
            }
        if (!nxvar)
                {
                sprintf(sbuf,"No regressors (X)!");
                if (etu==2)
                    {
                    sprintf(tut_info,"___@23@LINREG@%s@",sbuf);
                    s_end(argv[1]);
                    return;
                    }
                sur_print("\n"); sur_print(sbuf);
                WAIT; return;
                }
        d.m_act=k;
        m=d.m_act; k=0;
        for (i=0; i<m; ++i)
            if (!scale_ok(&d,d.v[i],SCORE_SCALE))
                {
                if (k==0)
                    sur_print("\nInsufficient scale in variables: ");
                k=1;
                sprintf(sbuf,"%.8s ",d.varname[d.v[i]]); sur_print(sbuf);
                }
        if (k)
            {
            if (etu==2)
                {
                if (scale_check==SCALE_INTERRUPT)
                    {
        strcpy(tut_info,"___@21@LINREG@Insufficient scales in variables!@");
                    s_end(argv[1]);
                    return;
                    }
                }
            sur_print("\nInterval or score scale required, at least!");
            WAIT; if (scale_check==SCALE_INTERRUPT) return;
            }
        i=varaa_tilat(); if (i<0) return;
        if (*corrfile)
            { i=corr_momentit(); if (i<0) return; }
        else
            { i=momentit(); if (i<0) return; }
        if ((long)(nxvar+1)>=n1)
            {
            if (etu==2)
                {
                strcpy(tut_info,"___@24@LINREG@Too few observations!@");
                s_end(argv[1]);
                return;
                }
            sprintf(sbuf,"\nToo few observations (%ld). ",n1);
            sur_print(sbuf);
            sprintf(sbuf,"%d required at least for the current model."
                           ,nxvar+2);
            sur_print(sbuf); WAIT; return;
            }
        if (results>=70) { i=tulostus(); if (i<0) return; }

        i=regressiolaskut(); if (i<0) return;
        i=regr_talletus();
        save_corr(A,m); // 15.4.2005
        free(A); free(ptila);
        data_close(&d);
        s_end(argv); // SM
        }

