/* _corrmv.c 11.5.1996/SM (22.6.1996)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;
static FILE *ftemp;
static FILE *ftemp2;

static double *x,*sum;
static int *miss;
static int *nx;
static double *sum2;      /* neliösummat */
static char **varname;    /* muuttujien nimet */
static char *lab;         /* matriisin rivi/sar.otsikot yhtenä jonona */
static double *aa;         /* momentit */
static double *A;
static double *mm1, *mm2;
static double ero,ero0;
static int method;
static int miss_total;
static double *res_dev;

static int n;
static int tulosrivi;
static int m,m1;
static char aineisto[LNAME];
static int prind=0;
static int max_iter,iter;
static int replace;

static int *nn;
static int *n1;

static double *xy,*xy2;

static int tell_miss(char *s);
static int varaa_tilat();
static int not_enough_memory();
static int summat();
static int sijoita_keskiarvot();
static int momentit();
static int tulostus();
static int eoutput(char *rivi);
static int mat_talletus();
static int tee_lab(char *lab,char **varname);
static int iteroi(int iter);
static int korjaa_puuttuvat();
static int pairwise_corr();
static int pair_varaus();
static int talleta_frekvenssit();
static int pair_tulostus();
static int pair_talletus();
static int replacement();
static int rajat(int i,double *pmin,double *pmax);

extern int rnd_def(char *x);
/***************************
static char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "IMPUTE", "REPLACE", "METHOD", "ITER", "RAND", "RND",
                 "SEED", "RESULTS", "PRIND", "!" };
static char **specs=specs0;
****************************/
/*********************
void main(argc,argv)
int argc; char *argv[];
**********************/

void muste_corrmv(char *argv)
        {
        int i,k;
        char nimi[LNAME];
        char x2[LLENGTH];   // x -> x2 22.9.2011/SM

 //     if (argc==1) return;
        s_init(argv);

        if (g<2)
            {
            init_remarks();
rem_pr("CORRMV <Survo_data>,L");
rem_pr("computes means, standard deviations, and correlations from active");
rem_pr("variables and observations by accepting also cases containing");
rem_pr("missing values. The standard CORR module leaves out all incomplete");
rem_pr("cases.");
rem_pr("The default method (METHOD=1) is a simplified EM algorithm by S.Mustonen.");
rem_pr("In this method the data set is first standardized (means=0 stddevs=1)");
rem_pr("and the missing values are replaced by 0's.");
rem_pr("Thereafter estimates for missing values are improved iteratively");
rem_pr("by linear regressions where each variable is explained by all other");
rem_pr("variables. In each iteration, old estimates of missing values are");
rem_pr("replaced by the regression estimates.");
rem_pr("In one iteration, all regression parameters are obtained simply by");
rem_pr("updating the moment matrix of variables and by inverting it by the");
rem_pr("Cholesky method.");
rem_pr("Convergence of the process can be monitored by the mean squared");
rem_pr("difference of consecutive estimates of missing values.");
rem_pr("After ITER iterations (default ITER=20) the procedure is interrupted.");
rem_pr("To obtain unbiased estimates for variances, in sums of squares each term");
rem_pr("of a missing value is extended by the residual variance of the corres-");
rem_pr("ponding regression model.");
            wait_remarks(1);
rem_pr("If the line for results (L) is given, the means, standard deviations,");
rem_pr("and correlations are printed in the edit field from line L onwards.");
rem_pr("If RESULTS=0 is given, only a summary of results is printed.");
rem_pr("In any case the results are saved in matrix files MSN.M and CORR.M");
rem_pr("as in CORR.");
            wait_remarks(1);
rem_pr("By default, missing values are not replaced by any estimates. However,");
rem_pr("if a specification IMPUTE (or REPLACE) is given, missing values are");
rem_pr("filled in.");
rem_pr("");
rem_pr("By IMPUTE=REG they are replaced by their regression estimates.");
rem_pr("Please note that regression estimates of missing values are too");
rem_pr("well-adapted and the variability in the data is reduced. Thus, if means,");
rem_pr("standard deviations, and correlations were recomputed from the patched");
rem_pr("data, the variances would become smaller than those given by CORRMV");
rem_pr("from incomplete data. Also correlations would be more biased.");
rem_pr("");
rem_pr("By IMPUTE=REG+rand(123456789) missing values are replaced by");
rem_pr("reg.est+u*s");
rem_pr("where s is the square root of the residual variance of the regression");
rem_pr("model in question and u is a standard normal variate obtained by");
rem_pr("using the pseudo-random number generator rand with seed 123456789.");
rem_pr("In this case means, std.devs and correlations recomputed from the");
rem_pr("patched data are less biased.");
            wait_remarks(1);
rem_pr("When METHOD=PAIRWISE is used, correlations are computed for non-");
rem_pr("missing pairs of observations. This may lead to more biased results");
rem_pr("than METHOD=1. Also the correlation matrix (CORR.M) may have negative");
rem_pr("eigenvalues (i.e. it is not positive definite or semidefinite).");
rem_pr("In METHOD=PAIRWISE the frequencies of observations for each pair");
rem_pr("of observations is saved as PAIRFREQ.M .");
            wait_remarks(2);
            s_end(argv);
            return;
            }

    x=NULL;
    nx=NULL;
    sum=NULL;
    miss=NULL;
    sum2=NULL;
    varname=NULL;
    lab=NULL;
    A=NULL;
    aa=NULL;
    mm1=NULL;
    mm2=NULL;
    res_dev=NULL;
    nn=NULL;
    n1=NULL;
    xy=NULL;
    xy2=NULL;


        miss_total=0;

        tulosrivi=0;
        if (g>2)
            {
            tulosrivi=edline2(word[2],1,1);
            if (tulosrivi==0) return;
            }

        i=spec_init(r1+r-1); if (i<0) return;
        strcpy(aineisto,word[1]);

        replace=0;
        i=spfind("IMPUTE");
        if (i<0) i=spfind("REPLACE");
        if (i>=0)
            {
            strcpy(x2,spb[i]);
            if (muste_strnicmp(x2,"REG+",4)==0) { replace=1; rnd_def(x2+4); }
            else replace=2;
            }
        if (!replace)
            { i=data_read_open(aineisto,&d); if (i<0) return; }
        else
            { i=data_open2(aineisto,&d,0,1,0); if (i<0) return; }

        i=mask(&d); if (i<0) return;
        i=mask_sort(&d); if (i<0) return;
        scales(&d);
        if (d.m_act<2)
            {
            sur_print("\nAt least 2 active variables required!");
            WAIT; return;
            }
        i=conditions(&d); if (i<0) return;

        if ((i=spfind("RESULTS"))>=0) results=atoi(spb[i]);
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        method=1;
        i=spfind("METHOD");
        if (i>=0)
            {
            strcpy(x2,spb[i]);
            if (strcmp(x2,"PAIRWISE2")==0 || atoi(x2)==3)  method=3;
            else if (muste_strnicmp(x2,"PAIR",4)==0 || atoi(x2)==2)  method=2;
            }
        max_iter=20;
        if ((i=spfind("ITER"))>=0) max_iter=atoi(spb[i]);


        m=d.m_act; m1=m+1; k=0;
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
                    strcpy(tut_info,"___@21@CORR@Insufficient scales in variables!@");
                    return;
                    }
                }
            sur_print("\nInterval or score scale required, at least!");
            WAIT; if (scale_check==SCALE_INTERRUPT) return;
            }

        if (method>1) { pairwise_corr(); s_end(argv);  return; }
                                      // 3.3.2005
        i=varaa_tilat(); if (i<0) return;

        strcpy(nimi,etmpd); strcat(nimi,"SURVO.TMP");
        ftemp=muste_fopen(nimi,"w+b");
        if (ftemp==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s !",nimi);
            sur_print(sbuf); WAIT; return;
            }
        i=summat(); if (i<0) return;
        i=sijoita_keskiarvot(); if (i<0) return;
        iter=0; ero0=1e30;
        if (miss_total==0) { max_iter=0; replace=0; }
        while (iter<max_iter)
            {
            i=iteroi(iter);
            if (i<0) break;
            ++iter;
            sprintf(sbuf,"\nIteration %d: mean squared difference %g",
                                      iter,ero/(double)miss_total);
            if (ero>ero0)
                {
                sur_print("\nDifficulties in convergence!");
                tell_miss(sbuf); sur_print("\n"); sur_print(sbuf);
                sur_print("\nInterrupting...");
                WAIT; break;
                }
            ero0=ero;
            sur_print(sbuf);
            if (sur_kbhit()) break;
            }

        if (!iter && max_iter>0)
            {
            sur_print("\nCannot continue!"); return;
            }
        i=momentit();
        for (i=0; i<m; ++i) varname[i]=d.varname[d.v[i]];
        i=tulostus();
        mat_talletus();
        if (replace) { replacement(); }
        muste_fclose(ftemp);    // 22.9.2011/SM
        muste_fclose(ftemp2);   // 22.9.2011/SM
        s_end(argv);
        }

static int tell_miss(char *s)
        {
        sprintf(s,"%d missing values of %d (%.2g per cent)",
                     miss_total, n*m, 100.0*(double)miss_total/(n*m));
        return(1);
        }

static int varaa_tilat()
        {


        x=(double *)muste_malloc(m1*sizeof(double));
        if (x==NULL) { not_enough_memory(); return(-1); }
        nx=(int *)muste_malloc(m*sizeof(int));
        if (nx==NULL) { not_enough_memory(); return(-1); }
        sum=(double *)muste_malloc(m1*sizeof(double));
        if (sum==NULL) { not_enough_memory(); return(-1); }
        miss=(int *)muste_malloc(m1*sizeof(int));
        if (miss==NULL) { not_enough_memory(); return(-1); }
        sum2=(double *)muste_malloc(m1*sizeof(double));
        if (sum2==NULL) { not_enough_memory(); return(-1); }
        varname=(char **)muste_malloc(m*sizeof(char **));
        if (varname==NULL) { not_enough_memory(); return(-1); }
        lab=muste_malloc((m+2)*8);
        if (lab==NULL) { not_enough_memory(); return(-1); }
        A=(double *)muste_malloc(m1*(m1+1)*sizeof(double));
        if (A==NULL) { not_enough_memory(); return(-1); }
        aa=(double *)muste_malloc(m1*(m1+1)*sizeof(double));
        if (aa==NULL) { not_enough_memory(); return(-1); }
        mm1=(double *)muste_malloc(m1*sizeof(double));
        if (mm1==NULL) { not_enough_memory(); return(-1); }
        mm2=(double *)muste_malloc(m1*sizeof(double));
        if (mm2==NULL) { not_enough_memory(); return(-1); }
        res_dev=(double *)muste_malloc(m1*sizeof(double));
        if (res_dev==NULL) { not_enough_memory(); return(-1); }

        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (CORRMV)");
        WAIT; return(1);
        }

static int summat()
        {
        int i,k;
        int l;


        n=0L;
        for (i=0; i<m; ++i)
            {
            sum[i]=0.0; nx[i]=0L;
            mm2[i]=0.0;
            }

        sur_print("\n");
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            if (prind) { sprintf(sbuf," %d",l); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
            ++n;
            for (i=0; i<m; ++i)
                {
                k=data_load(&d,l,d.v[i],&x[i]);
                if (k<0) return(-1);
                miss[i]=0;
                if (x[i]==MISSING8) { miss[i]=1; ++miss_total; }
                               else { ++nx[i]; sum[i]+=x[i]; mm2[i]+=x[i]*x[i]; }
                }
            fwrite(x,sizeof(double),m,ftemp);
            fwrite(miss,sizeof(int),m,ftemp);
            } /* l */

/*      if (miss_total==0L)
            {
            sur_print("\nNo missing values! Interrupting..."); WAIT; return(-1);
            }
*/
        l=n; for (i=0; i<m; ++i) if (nx[i]<l) l=nx[i];

        if (l<2L)  /* suurempi alaraja ? */
            {
            strcpy(sbuf,"Too few observations!");
            if (etu==2)
                {
                sprintf(tut_info,"___@22@CORR@%s@",sbuf);
                return(-1);
                }
            sur_print("\n"); sur_print(sbuf);
            WAIT; return(-1);
            }

        return(1);
        }

static int sijoita_keskiarvot()
        {
        int i;
        int l,tila;
        double b;

        tila=m*(sizeof(double)+sizeof(int));
        for (i=0; i<m; ++i) { sum[i]/=nx[i]; mm1[i]=sum[i]; }
        for (i=0; i<m; ++i)
            {
            b=fabs(mm2[i]-nx[i]*mm1[i]*mm1[i])/(double)(nx[i]-1);
            if (b<1e-30)
                {
                sprintf(sbuf,"\nVariable %.8s is a constant %g",
                                      d.varname[d.v[i]],mm1[i]);
                sur_print(sbuf); WAIT; return(-1);
                }
            mm2[i]=sqrt(b);
            }
        rewind(ftemp);
        for (l=0L; l<n; ++l)
            {
            muste_fseek(ftemp,l*tila,SEEK_SET);
            fread(x,sizeof(double),m,ftemp);
            fread(miss,sizeof(int),m,ftemp);
            for (i=0; i<m; ++i)
                {
                if (miss[i]) x[i]=0.0;
                else x[i]=(x[i]-mm1[i])/mm2[i];
                }
            muste_fseek(ftemp,l*tila,SEEK_SET);
            fwrite(x,sizeof(double),m,ftemp);
            fwrite(miss,sizeof(int),m,ftemp);
            }
/*
        for (l=0L; l<n; ++l)
            {
            muste_fseek(ftemp,l*tila,SEEK_SET);
            fread(x,sizeof(double),m,ftemp);
            fread(miss,sizeof(int),m,ftemp);
            Rprintf("\n%d:",l);
            Rprintf("\n"); for (i=0; i<m; ++i) Rprintf("%g ",x[i]);
            Rprintf("\n"); for (i=0; i<m; ++i) Rprintf("%d ",miss[i]); getch();
            }
*/
        return(1);
        }

static int momentit()
        {
        int i,j;
        int l;
        double zz;

        for (i=0; i<m; ++i)
            {
            sum[i]=0.0;
            for (j=0; j<=i; ++j)
                aa[i+m*j]=0.0;
            }

        for (i=0; i<m; ++i) sum2[i]=1.0/(double)(n-m1)/sum2[i];

        rewind(ftemp);
        sur_print("\n");
        for (l=0L; l<n; ++l)
            {
            if (prind) { sprintf(sbuf," %d",l); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }

            fread(x,sizeof(double),m,ftemp);
            fread(miss,sizeof(int),m,ftemp);
            for (i=0; i<m; ++i)
                {
                zz=x[i];
                sum[i]+=zz;

                for (j=0; j<i; ++j)
                    aa[i+m*j]+=zz*x[j];

                if (miss[i])
                    aa[i*(m+1)]+=zz*zz+sum2[i];
                else
                    aa[i*(m+1)]+=zz*zz;
                }
            } /* l */

        for (i=0; i<m; ++i)
            {
            for (j=0; j<=i; ++j)
                aa[i+m*j]-=sum[i]*sum[j]/n;
            }
        for (i=0; i<m; ++i)
            {
            sum[i]=sum[i]/n;

            sum2[i]=sqrt(aa[i+m*i]);
            for (j=0; j<=i; ++j)
                {
                double tulo;
                double xxx;

                tulo=sum2[i]*sum2[j];
                if (tulo==0.0) aa[i+m*j]=0.0;
                else aa[i+m*j]/=sum2[i]*sum2[j];
                xxx=aa[i+m*j];
                aa[j+m*i]=xxx;
                }
            }
        for (i=0; i<m; ++i)
            {
            sum[i]=mm1[i]+mm2[i]*sum[i];
            sum2[i]*=mm2[i]/sqrt((double)(n-1));
            }
        return(1);
        }

// kopio corr.c-modulin vastaavasta corrp():
static int mv_corrp(
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
                if (sar-1>8) nimi[8]=EOS; // 23.9.2011
                k+=sprintf(rivi+k,xsar,nimi);
                }
            eoutput(rivi);
            for (i=0; i<m; ++i)
                {
                strcpy(nimi,xname[i]); nimi[sar+2]=EOS;
                if (sar+2>8) nimi[8]=EOS; // 23.9.2011
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

        i=output_open(eout);  if (i<0) return(-1);

        sprintf(rivi,"Means, std.devs and correlations of %s  N=%d",
                              word[1],n);
        eoutput(rivi);
      if (miss_total)
        {
        sprintf(rivi,"Computed by simplified EM algorithm of S.Mustonen");
        eoutput(rivi);
        tell_miss(rivi);
        eoutput(rivi);
        sprintf(rivi,"Results after %d iterations, mean squared difference %g",
                            iter,ero/(double)miss_total);
        eoutput(rivi);
        }
      else
        {
        eoutput("No missing values!");
        }
        eoutput("Means, std.devs, and correlations saved in MSN.M and CORR.M");

        if (results==0) { output_close(eout); return(1); }
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
        if (m>1)
            {
            mv_corrp(aa,m,varname,c3,accuracy+1,accuracy-3,"Correlations:");
            }

        return(1);
        }

static int eoutput(char *rivi)
        {
        output_line(rivi,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

static int mat_talletus()
        {
        int i;
        char expr[LLENGTH];
     // char *p;

        tee_lab(lab,varname);
        sur_print("\nSaving correlations in CORR.M");
        sprintf(expr,"R(%s) / N=%d",aineisto,n);
        matrix_save("CORR.M",aa,m,m,lab,lab,8,8,-1,expr,0,0);

        sur_print("\nSaving means, stddevs and N in MSN.M");
        sprintf(expr,"MSN(%s) / N=%d",aineisto,n);
        for (i=0; i<m; ++i)
            {
            aa[i]=sum[i]; aa[i+m]=sum2[i]; aa[i+2*m]=(double)n;
            }
        matrix_save("MSN.M",aa,m,3,lab,"mean    stddev  N       ",8,8,-1,expr,0,0);

        return(1);
        }

static int tee_lab(char *lab,char **varname)
        {
        int i,h;

        for (i=0; i<8*m; ++i) lab[i]=' ';
        for (i=0; i<m; ++i)
            {
            for (h=0; h<8; ++h)
                {
                if (varname[i][h]==EOS) break;
                lab[8*i+h]=varname[i][h];
                }
            }
        return(1);
        }

/* corrmv2.c 12.5.1996/SM (27.5.1996)
*/
static FILE *ftemp2;

extern double sis_tulo();

/**********************************************************************


    X'X  X'y    inv(X'X) + inv(X'X)X'yy'Xinv(X'X)/z  -inv(X'X)X'y/z
inv           =
    y'X  y'y    -y'Xinv(X'X)/z                       1/z


missä z = y'y - y'Xinv(X'X)X'y

Siis jakamalla käänteismatriisin sarakkeet -lävistäjäalkiolla
saadaan sarakkeiksi regressiokertoimet inv(X'X)X'y
ko. muuttujan mallille, missä kaikki muut muuttujat ovat selittäjinä.

************************************************************************/

static int iteroi(int iter)
        {
        int i,j;
        int l;
        double zz;
        char nimi[LNAME];


        rewind(ftemp);

        x[m]=1.0; miss[m]=0; /* vakiotermi */

        if (iter==0)
            {
            for (i=0; i<m1; ++i)
                for (j=0; j<m1; ++j) A[i+m1*j]=0.0;
            for (l=0L; l<n; ++l)
                {
                fread(x,sizeof(double),m,ftemp);
                fread(miss,sizeof(int),m,ftemp);
                for (i=0; i<m1; ++i)
                    {
                    zz=x[i];   /* missing values all 0 */
                    for (j=0; j<=i; ++j)
                        A[i+m1*j]+=zz*x[j];
                    }
                }

            strcpy(nimi,etmpd); strcat(nimi,"SURVO2.TMP");
            ftemp2=muste_fopen(nimi,"w+b");
            if (ftemp2==NULL)
                {
                sprintf(sbuf,"\nCannot open temporary file %s !",nimi);
                sur_print(sbuf); WAIT; return(-1);
                }
            fwrite(A,sizeof(double),m1*m1,ftemp2);
            }
        else /* iter>0 */
            {
            rewind(ftemp2);
            fread(A,sizeof(double),m1*m1,ftemp2);
            for (l=0L; l<n; ++l)
                {
                fread(x,sizeof(double),m,ftemp);
                fread(miss,sizeof(int),m,ftemp);
                for (i=0; i<m1; ++i)
                    {
                    zz=x[i];  /* only pairs with missing values */
                    for (j=0; j<=i; ++j)
                        if (miss[i] || miss[j]) A[i+m1*j]+=zz*x[j];
                    }
                }
             }

        for (i=0; i<m1; ++i) for(j=0; j<=i; ++j) A[j+m1*i]=A[i+m1*j];

        i=mat_cholinv(A,m1);
        if (i<1)
            {
            if (-i<m)
                sprintf(sbuf,"\nVariable %.8s is linearly dependent on the previous ones!",
                              d.varname[d.v[-i]]);
            else sprintf(sbuf,"\nLinear combination of variables is a constant!");
            sur_print(sbuf); WAIT; return(-1);
            }
        mat_cholmove(A,m1);

        for (j=0; j<m; ++j)
            {
            res_dev[j]=A[j*(m1+1)]; /* "arvontaa" varten */
            sum2[j]=A[j*(m1+1)];
            zz=-1.0/A[j*(m1+1)];
            for (i=0; i<m1; ++i) A[i+m1*j]*=zz;
            A[j*(m1+1)]=0.0; /* jotta korjatut arvot suoraan skalaarituloina */
            }

        korjaa_puuttuvat();

        return(1);
        }

static int korjaa_puuttuvat()
        {
        int i;
        int muutos;
        int l,tila;
        double zz,u;

        rewind(ftemp);
        tila=m*(sizeof(double)+sizeof(int));
        x[m]=1.0; /* vakiotermi */
        ero=0.0;
        for (l=0L; l<n; ++l)
            {
            muutos=0;
            fseek(ftemp,l*tila,SEEK_SET);
            fread(x,sizeof(double),m,ftemp);
            fread(miss,sizeof(int),m,ftemp);
            for (i=0; i<m; ++i)
                {
                if (miss[i])
                    {
                    zz=sis_tulo(A+i*m1,x,1,1,m1);
/*      Rprintf("%d %d %g %g\n",l+1,i+1,x[i],zz); getch(); */
                    u=zz-x[i]; ero+=u*u;
                    x[i]=zz;
                    ++muutos;
                    }
                }
            if (muutos)
                {
                fseek(ftemp,l*tila,SEEK_SET);
                fwrite(x,sizeof(double),m,ftemp);
                }
            }
        return(1);
        }

/* corrmv3.c 19.5.1996/SM (19.5.1996) (7.12.1996)
*/

static int pairwise_corr()
        {
        int i,j,k,h1,h2;
        int l,nmin;
        double a,b;

        if (method==2) method=3; else method=2;

        i=pair_varaus(); if (i<0) return(-1);
        for (i=0; i<m; ++i) varname[i]=d.varname[d.v[i]];

        n=0L;
        for (i=0; i<m; ++i)
            {
            sum[i]=sum2[i]=0.0;
            n1[i]=0L;
            for (j=0; j<=i; ++j)
                {
                A[i+m*j]=0.0;
                nn[i+m*j]=0L;
                if (method==3) xy[i+m*j]=xy[j+m*i]=xy2[i+m*j]=xy2[j+m*i]=0.0;
                }
            }

        sur_print("\n");
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            if (prind) { sprintf(sbuf," %d",l); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
            ++n;
            for (i=0; i<m; ++i)
                {
                k=data_load(&d,l,d.v[i],&a);
                if (k<0) return(-1);
                x[i]=a;
                if (a==MISSING8) continue;
                ++n1[i]; sum[i]+=a; sum2[i]+=a*a;
                for (j=0; j<=i; ++j)
                    {
                    b=x[j]; if (b==MISSING8) continue;
                    ++nn[i+m*j];
                    A[i+m*j]+=a*b;
                    if (method==3 && j<i)
                        {
                        xy[i+m*j]+=a; xy2[i+m*j]+=a*a;
                        xy[j+m*i]+=b; xy2[j+m*i]+=b*b;
                        }
                    }
                }
            } /* l */

        nmin=n;
        for (i=0; i<m; ++i)
            for (j=0; j<=i; ++j)
                if (nn[i+m*j]<nmin) nmin=nn[i+m*j];
        if (nmin<2L)
            {
            i=output_open(eout); if (i<0) return(-1);
            sprintf(sbuf,"Smallest # pairwise observations is less than 2!");
            sur_print("\n"); sur_print(sbuf);
            eoutput(sbuf);
            sprintf(sbuf,"MAT LOAD PAIRFREQ.M,END+2 / Pairwise frequencies");
            sur_print("\n"); sur_print(sbuf);
            eoutput(sbuf);
            talleta_frekvenssit();
            return(-1);
            }

        for (i=0; i<m; ++i) sum[i]/=n1[i];
        for (i=0; i<m; ++i) sum2[i]=sqrt((sum2[i]-n1[i]*sum[i]*sum[i])/(n1[i]-1L));

        if (method==3)
            {
            for (i=0; i<m; ++i)
                for (j=0; j<i; ++j)
                    {
                    h1=i+m*j; h2=j+m*i;
                    xy[h1]/=nn[h1];
                    xy[h2]/=nn[h1];
                    xy2[h1]=sqrt((xy2[h1]-nn[h1]*xy[h1]*xy[h1])/(nn[h1]-1L));
                    xy2[h2]=sqrt((xy2[h2]-nn[h1]*xy[h2]*xy[h2])/(nn[h1]-1L));
                    }
            }

        if (method==2)
            {
            for (i=0; i<m; ++i)
                for (j=0; j<i; ++j)
                    A[i+m*j]=(A[i+m*j]-nn[i+m*j]*sum[i]*sum[j])/
                                     ((nn[i+m*j]-1L)*sum2[i]*sum2[j]);
            }
        else
            {
            for (i=0; i<m; ++i)
                for (j=0; j<i; ++j)
                    {
                    h1=i+m*j; h2=j+m*i;
                    A[h1]=(A[h1]-nn[h1]*xy[h1]*xy[h2])/
                                     ((nn[h1]-1L)*xy2[h1]*xy2[h2]);
                    }
            }

        for (i=0; i<m; ++i)
            {
            for (j=0; j<i; ++j) A[j+m*i]=A[i+m*j];
            A[i*(m+1)]=1.0;
            }
        pair_tulostus();
        pair_talletus();
        talleta_frekvenssit();
        return(1);
        }

static int pair_varaus()
        {
        int mm;

        mm=m*m; if (mm<=6) mm=6;

        x=(double *)muste_malloc(m*sizeof(double));
        if (x==NULL) { not_enough_memory(); return(-1); }
        sum=(double *)muste_malloc(m*sizeof(double));
        if (sum==NULL) { not_enough_memory(); return(-1); }
        sum2=(double *)muste_malloc(m*sizeof(double));
        if (sum2==NULL) { not_enough_memory(); return(-1); }
        varname=(char **)muste_malloc(m*sizeof(char **));
        if (varname==NULL) { not_enough_memory(); return(-1); }
        lab=muste_malloc((m+2)*8);
        if (lab==NULL) { not_enough_memory(); return(-1); }
        A=(double *)muste_malloc(mm*sizeof(double));
        if (A==NULL) { not_enough_memory(); return(-1); }
        nn=(int *)muste_malloc(m*m*sizeof(int));
        if (nn==NULL) { not_enough_memory(); return(-1); }
        n1=(int *)muste_malloc(m*sizeof(int));
        if (n1==NULL) { not_enough_memory(); return(-1); }

        if (method==3)
            {
            xy=(double *)muste_malloc(mm*sizeof(double));
            if (xy==NULL) { not_enough_memory(); return(-1); }
            xy2=(double *)muste_malloc(mm*sizeof(double));
            if (xy2==NULL) { not_enough_memory(); return(-1); }
            }

        return(1);
        }

static int talleta_frekvenssit()
        {
        int i,j;
        char expr[LLENGTH];

        tee_lab(lab,varname);

        for (i=0; i<m; ++i)
            for (j=0; j<=i; ++j)
                A[i+m*j]=A[j+i*m]=nn[i+m*j];

        sprintf(expr,"N(%s)",aineisto);
        matrix_save("PAIRFREQ.M",A,m,m,lab,lab,8,8,-1,expr,0,0);
        return(1);
        }

static int pair_tulostus()
        {
        int i;
        char rivi[LLENGTH];
        char x[LLENGTH];

        i=output_open(eout);  if (i<0) return(-1);

        if (m>1 && results>0)
            sprintf(rivi,"Means, std.devs and pairwise correlations of %s  N=%d",
                              word[1],n);
        else
            sprintf(rivi,"%s  N=%d",word[1],n);
        eoutput(rivi);

        sprintf(x,"%d",n); for (i=0; i<strlen(x); ++i) x[i]='#';
        sprintf(rivi,"MAT LOAD PAIRFREQ.M,%s,END+2 / Pairwise frequencies",x);
        eoutput(rivi);

        sprintf(rivi,"Variable  Mean %.*s Std.dev.       N",accuracy-1,space);
        eoutput(rivi);
        for (i=0; i<m; ++i)
            {
            char mean[32],stddev[32];

            fnconv(sum[i],accuracy+2,mean);
            fnconv(sum2[i],accuracy+2,stddev);
            sprintf(rivi,"%-8.8s %*s   %*s %7d",d.varname[d.v[i]],
                         accuracy+2,mean,accuracy+2,stddev,n1[i]);
            eoutput(rivi);
            }
        output_close(eout);
        if (m>1)
            {
            mv_corrp(A,m,varname,c3,accuracy+1,accuracy-3,"Pairwise_correlations:");
            }

        return(1);
        }

static int pair_talletus()
        {
        int i;
        char expr[LLENGTH];

        tee_lab(lab,varname);
        sur_print("\nSaving pairwise correlations in CORR.M");
        sprintf(expr,"R(%s) / N=%d",aineisto,n);
        matrix_save("CORR.M",A,m,m,lab,lab,8,8,-1,expr,0,0);

        sur_print("\nSaving means, stddevs and N in MSN.M");
        sprintf(expr,"MSN(%s) / N=%d",aineisto,n);
        for (i=0; i<m; ++i)
            {
            A[i]=sum[i]; A[i+m]=sum2[i]; A[i+2*m]=n1[i];
            }
        matrix_save("MSN.M",A,m,3,lab,"mean    stddev  N",8,8,-1,expr,0,0);

        return(1);
        }

/* corrmv4.c 25.5.1996/SM (27.5.1996)
*/

#define MAXM 1000

static double *min,*max;
// static char type[MAXM];

static int replacement()
        {
        int i,k;
        int l;
        double b;
        char type[MAXM];

        min=A;
        max=A+m;

        for (i=0; i<m; ++i) { min[i]=-1e30; max[i]=1e30; type[i]=' '; }
        if (d.type==2)
            {
            for (i=0; i<m; ++i)
                {
                type[i]=d.d2.vartype[d.v[i]][0];
                if(type[i]=='1')
                    { min[i]=0.0; max[i]=254.0; }
                rajat(i,&min[i],&max[i]);
                }
            }

        for (i=0; i<m; ++i) res_dev[i]=sqrt(1.0/(double)(n-m1)/res_dev[i]);
/* Rprintf("\n"); for (i=0; i<m; ++i) Rprintf("%g ",res_dev[i]); getch(); */

        rewind(ftemp);
        sur_print("\nReplacement of missing observations... ");
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            if (prind) { sprintf(sbuf," %d",l); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
            ++n;
            for (i=0; i<m; ++i)
                {
                k=data_load(&d,l,d.v[i],&x[i]);
                if (k<0) return(-1);
                }
            fread(sum,sizeof(double),m,ftemp);
            fread(miss,sizeof(int),m,ftemp);
            for (i=0; i<m; ++i)
                {
                if (miss[i])
                    {
                    if (replace==1) sum[i]+=res_dev[i]*normal_dev(); /* 20.6.1996 */

                    b=mm2[i]*sum[i]+mm1[i];
                    if (b<min[i]) b=min[i];
                    if (b>max[i]) b=max[i];
                    if (type[i]=='1' || type[i]=='2')
                        {
                        if (b>=0.0) b+=0.5;
                        else b-=0.5;
                        }
                    k=data_save(&d,l,d.v[i],b);
                    if (k<0) return(-1);
              /*    Rprintf("\ni=%d %g",i+1,sum[i]); getch(); */
                    }
                }
            } /* l */
        data_close(&d);
        return(1);
        }

static int rajat(int i,double *pmin,double *pmax)
        {
        char *p,*q;
        char teksti[LLENGTH];
        char *sana[2];
        int k;

        strcpy(teksti,d.varname[i]);
        p=strchr(teksti,'{'); if (p==NULL) return(1);
        q=strchr(p,'}'); if (q==NULL) return(1);
        *q=EOS;
        k=split(p+1,sana,2);
        if (k<2) return(1);
        *pmin=atof(sana[0]); *pmax=atof(sana[1]);
        return(1);
        }

