#include "muste.h"
/* _corr.c 26.2.1986/SM (24.10.1991) (1.4.1994) (2.2.1996) (2.6.1997)
*/

#define NSTEP 25

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

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
static int prind=1;
static int fast=1;

// RS REM static char argv1[100];

/* RS REM
static char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "RESULTS", "PRIND", "FAST", "!"
               };
static char **specs=specs0;
*/

static double *datab;



static int not_enough_memory()
{
    sur_print("\nNot enough memory! (CORR)");
    WAIT;
    return(1);
}

static int varaa_tilat()
{
//    int tila;
    int x_tila, x0_tila, sum_tila, sum2_tila, nimet, matots;
    unsigned int A_tila;
    /* RS REM       char *p;   */
    int mm;

    x_tila=x0_tila=sum_tila=sum2_tila=m*sizeof(double);
    nimet=m*sizeof(char **);
    matots=8*(m+2);  /* kun m=1,2, MSN.M:ssä 3 sar. */
//    tila=x_tila+x0_tila+sum_tila+sum2_tila+nimet+matots;

    x=(double *)muste_malloc(m*sizeof(double));
    if (x==NULL)
    {
        not_enough_memory();
        return(-1);
    }
    x0=(double *)muste_malloc(m*sizeof(double));
    if (x0==NULL)
    {
        not_enough_memory();
        return(-1);
    }
    sum=(double *)muste_malloc(m*sizeof(double));
    if (sum==NULL)
    {
        not_enough_memory();
        return(-1);
    }
    sum2=(double *)muste_malloc(m*sizeof(double));
    if (sum2==NULL)
    {
        not_enough_memory();
        return(-1);
    }
    varname=(char **)muste_malloc(m*sizeof(char **));
    if (varname==NULL)
    {
        not_enough_memory();
        return(-1);
    }
    lab=muste_malloc((unsigned int)8*(m+2));
    if (lab==NULL)
    {
        not_enough_memory();
        return(-1);
    }

    mm=m*m;
    if (m<3) mm=6;   /* 31.1.90 */
    A_tila=mm*sizeof(double);
    if (results==-1) A_tila=(3*m+3)*sizeof(double); /* myös MSN-talletus! */
    A=(double *)muste_malloc(A_tila);
    if (A==NULL)
    {
        not_enough_memory();
        return(-1);
    }
    return(1);
}

static int momentit2()
        {
        int i,j,k;
        long l;
        double nsuhde;
        int prind_gap,prind_count;
        int stop;

        int nb,ib;
/* RS REM       double b; */
        int loppu;

/*
        if (painomuuttuja>=0)
            {
            sur_print("\nIn FAST=1 mode weight variable cannot be used!");
            WAIT; return(-1);
            }
*/
        datab=(double *)muste_malloc(NSTEP*m*sizeof(double));
        if (datab==NULL) { not_enough_memory(); return(-1); }

/*      nb=MAXSPACE/m;  */
        nb=NSTEP;

        loppu=0; // RS loppu init
        n=n1=0L; stop=0;
        for (i=0; i<m; ++i)
            {
            sum[i]=0.0;

            if (results==-1) /* 1.4.1994 */
                {
                A[i]=0.0;
                continue;
                }

            for (j=0; j<=i; ++j)
                A[i+m*j]=0.0;
            }

        for (i=0; i<m; ++i)
            {
            k=data_load(&d,d.l1,d.v[i],&x0[i]);
            if (k<0) return(-1);
            if (x0[i]==MISSING8) x0[i]=0.0;
            }

        sur_print("\n");
        ib=0; prind_gap=100; prind_count=0;
        for (l=d.l1; l<=d.l2; ++l)
            {
/*  RS REM          double ind;  */

            ++prind_count;
            if (prind_count==prind_gap)
                {
                prind_count=0;
                if (sur_kbhit()) { i=sur_getch(); prind=1-prind; } /* RS CHA kbhit->sur_kbhit*/
                }

            if (unsuitable(&d,l))
                {
                if (l==d.l2) stop=loppu=1; else continue;
                /* 2.4.1996 */
                }
            else if (prind) { sprintf(sbuf,"% ld",l); sur_print(sbuf); }
            if (!stop)
                {
                ++n;
                for (i=0; i<m; ++i)
                    {
                    k=data_load(&d,l,d.v[i],&x[i]);
                    if (k<0) return(-1);
                    if (x[i]==MISSING8) break;
                    }
                if (i<m) sur_print("-");
                else
                    {
                    for (i=0; i<m; ++i)
                        {
                        x[i]-=x0[i];  /* keskistys */
                        sum[i]+=x[i];
                        }
                    memcpy(datab+ib*m,x,m*sizeof(double));
                    ++ib;
                    ++n1;
                    if (ib<nb && l<d.l2) continue;
                    }
                loppu=0; if (l==d.l2 || ib==nb ) loppu=1;
                }
            if (!loppu) continue;
/* Rprintf("\nib=%d nb=%d\n",ib,nb); getch(); */
            if (ib<nb) nb=ib; /* for the last case */
            ib=0;
            for (i=0; i<m; ++i)
                {
                if (results==-1)   /* 1.4.1994 */
                    {
                    A[i]+=sis_tulo(datab+i,datab+i,m,m,nb);
                    continue;
                    }

                for (j=0; j<=i; ++j)
                    A[i+m*j]+=sis_tulo(datab+i,datab+j,m,m,nb);
                }
            } /* l */

        weightsum=n1;

        if (n1<2 || weightsum<=0.0)
            {
            strcpy(sbuf,"Too few observations or sum of weights<=0!");
            if (etu==2)
                {
                sprintf(tut_info,"___@22@CORR@%s@",sbuf);
                return(-1);
                }
            sur_print("\n"); sur_print(sbuf);
            WAIT; return(-1);
            }
        nsuhde=(double)(n1-1L)/(double)n1;
        for (i=0; i<m; ++i)
            {
            if (results==-1)
                {
                A[i]-=sum[i]*sum[i]/weightsum;
                continue;
                }
            for (j=0; j<=i; ++j)
                A[i+m*j]-=sum[i]*sum[j]/weightsum;
            }
        for (i=0; i<m; ++i)
            {
            sum[i]=sum[i]/weightsum+x0[i];

            if (results==-1) /* 1.4.1994 */
                {
                sum2[i]=sqrt(A[i]);
                continue;
                }

            sum2[i]=sqrt(A[i+m*i]);
            for (j=0; j<=i; ++j)
                {
                double tulo;
                double xxx;

                tulo=sum2[i]*sum2[j];
                if (tulo==0.0) A[i+m*j]=0.0;
                else A[i+m*j]/=sum2[i]*sum2[j];
                xxx=A[i+m*j];
                A[j+m*i]=xxx;
                }
            }
        for (i=0; i<m; ++i) sum2[i]/=sqrt(nsuhde*weightsum);
        return(1);
        }

static int momentit()
{
    int i,j,k;
    long l;
    double nsuhde;
    /* RS REM      long max_n;  rajoitetut versiot */

    n=n1=0L;
    weightsum=0.0;
    for (i=0; i<m; ++i)
    {
        sum[i]=0.0;

        if (results==-1) /* 1.4.1994 */
        {
            A[i]=0.0;
            continue;
        }

        for (j=0; j<=i; ++j)
            A[i+m*j]=0.0;
    }
    for (i=0; i<m; ++i)
    {
        k=data_load(&d,d.l1,d.v[i],&x0[i]);
        if (k<0) return(-1);
        if (x0[i]==MISSING8) x0[i]=0.0;
    }
    sur_print("\n");
    for (l=d.l1; l<=d.l2; ++l)
    {
        double paino; /* RS REM ,ind; */

        if (unsuitable(&d,l)) continue;
        if (painomuuttuja==-1) paino=1.0;
        else
        {
            k=data_load(&d,l,painomuuttuja,&paino);
            if (k<0) return(-1);
            if (paino==MISSING8) continue;
        }
        if (prind)
        {
            sprintf(sbuf,"% ld",l);
            sur_print(sbuf);
        }
        if (sur_kbhit()) /* RS CHA "tajunnanvirta" kbhit->sur_kbhit  */
        {
             sur_getch();
             if (sur_kbhit()) sur_getch();
             prind=1-prind;
        }
        ++n;
        for (i=0; i<m; ++i)
        {
            k=data_load(&d,l,d.v[i],&x[i]);
            if (k<0) return(-1);
            if (x[i]==MISSING8) break;
            x[i]-=x0[i];  /* keskistys */
        }
        if (i<m)
        {
            sur_print("-");
            continue;
        }

        ++n1;
        weightsum+=paino;

        for (i=0; i<m; ++i)
        {
            double z;

            if (d.v[i]==painomuuttuja) continue;
            z=paino*x[i];
            sum[i]+=z;

            if (results==-1)   /* 1.4.1994 */
            {
                A[i]+=z*z;
                continue;
            }

            for (j=0; j<=i; ++j)
                A[i+m*j]+=z*x[j];
        }
    } /* l */

    if (n1<2 || weightsum<=0.0)
    {
        strcpy(sbuf,"Too few observations or sum of weights<=0!");
        if (etu==2)
        {
            sprintf(tut_info,"___@22@CORR@%s@",sbuf);
            return(-1);
        }
        sur_print("\n");
        sur_print(sbuf);
        WAIT;
        return(-1);
    }
    nsuhde=(double)(n1-1L)/(double)n1;
    for (i=0; i<m; ++i)
    {
        if (results==-1)
        {
            A[i]-=sum[i]*sum[i]/weightsum;
            continue;
        }
        for (j=0; j<=i; ++j)
            A[i+m*j]-=sum[i]*sum[j]/weightsum;
    }
    for (i=0; i<m; ++i)
    {
        sum[i]=sum[i]/weightsum+x0[i];

        if (results==-1) /* 1.4.1994 */
        {
            sum2[i]=sqrt(A[i]);
            continue;
        }

        sum2[i]=sqrt(A[i+m*i]);
        for (j=0; j<=i; ++j)
        {
            double tulo;
            double xxx;

            tulo=sum2[i]*sum2[j];
            if (tulo==0.0) A[i+m*j]=0.0;
            else A[i+m*j]/=sum2[i]*sum2[j];
            xxx=A[i+m*j];
            A[j+m*i]=xxx;
        }
    }
    for (i=0; i<m; ++i) sum2[i]/=sqrt(nsuhde*weightsum);
    return(1);
}

static void eoutput(char *rivi)
{
    output_line(rivi,eout,tulosrivi);
    if (tulosrivi) ++tulosrivi;
}

int corrp(
double S[],
int m,     /* dimensio */
char *xname[],
int   lev, /* kentän leveys */
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

    i=output_open(eout);
    if (i<0) return(1);

    if (m>1 && results>0)
        sprintf(rivi,"Means, std.devs and correlations of %s  N=%ld",
                word[1],n);
    else
        sprintf(rivi,"%s  N=%ld",word[1],n);

    if (painomuuttuja>=0)
    {
        strcat(rivi," Weight=");
        strncat(rivi,d.varname[painomuuttuja],8);
    }
    eoutput(rivi);
    if (n1<n)
    {
        sprintf(rivi,"# of missing observations =%ld",n-n1);
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
    if (m>1 && results!=-1)
    {
        corrp(A,m,varname,c3,accuracy+1,accuracy-3,"Correlations:");
    }
    return(1);
}

static void mat_talletus()
{
    int i,h;
    char expr[LLENGTH];
    /*  RS REM      char *p;   */

    for (i=0; i<8*m; ++i) lab[i]=' ';
    for (i=0; i<m; ++i)
    {
        for (h=0; h<8; ++h)
        {
            if (varname[i][h]==EOS) break;
            lab[8*i+h]=varname[i][h];
        }
    }
    if (results!=-1)
    {
        sur_print("\nSaving correlations in CORR.M");
        sprintf(expr,"R(%s) / N=%ld",aineisto,n1);
        matrix_save("CORR.M",A,m,m,lab,lab,8,8,-1,expr,0,0);
    }
    sur_print("\nSaving means, stddevs and N in MSN.M");
    sprintf(expr,"MSN(%s) / N=%ld",aineisto,n1);
    for (i=0; i<m; ++i)
    {
        A[i]=sum[i];
        A[i+m]=sum2[i];
        A[i+2*m]=(double)n1;
    }
    matrix_save("MSN.M",A,m,3,lab,"mean    stddev  N       ",8,8,-1,expr,0,0);

}

/* RS CHA int muste_corr(int argc,char *argv[]) */
int muste_corr(char *argv)
{
   int i,k;

// RS local globals init
    i=0;
    k=0;
    n=0;
    n1=0;
    weightsum=0;
    painomuuttuja=0;
    tulosrivi=0;
    m=0;
    prind=1;
    fast=1;


/* RS REM    if (argc==1) return(1);  */
    s_init(argv);
/* RS CHA   s_init(argv[1]); */
    /*  RS REM      s_opt(argv[2]);   Rajoitustietoja info_s:ään */
    if (g<2)
    {
        sur_print("\nUsage: CORR <SURVO_data>,<output_line>");
        WAIT;
        return(1);
    }
    tulosrivi=0;
    if (g>2)
    {
        tulosrivi=edline2(word[2],1,1);
        if (tulosrivi==0) return(1);
    }
    strcpy(aineisto,word[1]);
    i=data_read_open(aineisto,&d);
    if (i<0)
    {
        s_end(argv);
/* RS CHA       s_end(argv[1]); */
        return(1);
    }

    /* RS REM Rajoitustarkistuksia
            i=optdim_d(); if (i && i<d.m) err(0);
            i=optdim_o(); if (i && (long)i<d.n) err(0);
    */

    i=spec_init(r1+r-1);
    if (i<0) return(1);
    i=mask(&d);
    if (i<0)
    {
        s_end(argv);
/* RS CHA       s_end(argv[1]); */
        return(1);
    }
    i=mask_sort(&d);
    if (i<0)
    {
        s_end(argv);
/*  RS CHA      s_end(argv[1]); */;
        return(1);
    }
    scales(&d);
    if (d.m_act==0)
    {
        if (etu==2)
        {
            strcpy(tut_info,"___@29@CORR@No active variables!@");
        s_end(argv);
/*  RS CHA      s_end(argv[1]); */
            return(1);
        }
        sur_print("\nNo active variables!");
        WAIT;
        return(1);
    }
    i=conditions(&d);
    if (i<0)
    {
        s_end(argv);
/* RS CHA       s_end(argv[1]); */
        return(1);
    }
    if ((i=spfind("RESULTS"))>=0) results=atoi(spb[i]);
    i=hae_apu("prind",sbuf);
    if (i) prind=atoi(sbuf);
    if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);
    if ((i=spfind("FAST"))>=0) fast=atoi(spb[i]);
    painomuuttuja=activated(&d,'W');
    if (painomuuttuja>=0 && !scale_ok(&d,painomuuttuja,RATIO_SCALE))
    {
        sprintf(sbuf,"\nWeight variable %.8s has not ratio scale!",
                d.varname[painomuuttuja]);
        sur_print(sbuf);
        WAIT;
        if (scale_check==SCALE_INTERRUPT)
        {
        s_end(argv);
/*  RS CHA      s_end(argv[1]); */
            return(1);
        }
    }
    if (painomuuttuja>=0)
    {
        fast=0;
        k=0;
        for (i=0; i<d.m; ++i)
            if (d.vartype[i][1]!='-' && d.vartype[i][1]!='W')
                d.v[k++]=i;
        d.m_act=k;
    }
    m=d.m_act;
    k=0;

    for (i=0; i<m; ++i)
        if (!scale_ok(&d,d.v[i],SCORE_SCALE))
        {
            if (k==0)
                sur_print("\nInsufficient scale in variables: ");
            k=1;
            sprintf(sbuf,"%.8s ",d.varname[d.v[i]]);
            sur_print(sbuf);
        }
    if (k)
    {
        if (etu==2)
        {
            if (scale_check==SCALE_INTERRUPT)
            {
                strcpy(tut_info,"___@21@CORR@Insufficient scales in variables!@");
        s_end(argv);
/*  RS CHA      s_end(argv[1]); */
                return(1);
            }
        }
        sur_print("\nInterval or score scale required, at least!");
        WAIT;
        if (scale_check==SCALE_INTERRUPT) return(1);
    }

    i=varaa_tilat();
    if (i<0) return(1);

    if (fast && painomuuttuja<0)
        i=momentit2();
    else
        i=momentit();
    if (i<0)
    {
        s_end(argv);
/* RS CHA       s_end(argv[1]); */
        return(1);
    }
    for (i=0; i<m; ++i) varname[i]=d.varname[d.v[i]];
    if (results>0 || results==-1) tulostus();
    mat_talletus();
    muste_free(A);
    muste_free(ptila);
    data_close(&d);
    s_end(argv);
/* RS CHA       s_end(argv[1]); */
    return(1);
}

