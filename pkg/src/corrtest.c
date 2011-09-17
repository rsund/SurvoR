/* !corrtest.c 27.5.1996/SM (7.6.1996) (20.6.1996)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

/* #define MAXN 16000 */

static SURVO_DATA d;
static FILE *ftemp;
static int nn,n[2];
static char nimi[2][LNAME];
static char v1[2][LNAME],v2[2][LNAME];
static float *xx,*yy;
static unsigned int *perm;
static int varx[2],vary[2];
static double ff[2];
static double sumx[2],sumy[2],sumx2[2],sumy2[2],sumxy[2];
static double sx1,sx2,sy1,sy2,sxy;
static double sx11,sx21,sy11,sy21,sxy1;
static double sx12,sx22,sy12,sy22,sxy2;
static double rr[2];
static int maxcount;
static double test_val;
static int u,u1;
static double conf_level,conf_coeff;
static int results_line;
/********************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "SIMUMAX", "CONF", "RAND", "RND", "SEED", "!" };
char **specs=specs0;
***************************************/
static int talleta(char *s1,char *s2,int k);
static int virhe(char *s);
static int laske_r(int n,int k);
static int simulation();
static int varaa_tilat();
static double fisher_z(double *r,int *n);
static int not_enough_memory();
static int disp0();
static int corrtest_disp();
static int printout();
static int print_line(char *line);
static int vertailu_annetuilla_arvoilla();
static int r_error(double r);
static int n_error(int n);
static int one_sample_test();
static int simulation1();
static int varaa_tilat1();
static int disp01();
static int disp1();
static int printout1();

// extern double fisher_z();
// extern int print_line();
extern double muste_cdf_std();
extern double uniform_dev();
extern double muste_inv_std();
extern double muste_cdf_t();

int (*g_print)();
/***************************
void main(argc,argv)
int argc; char *argv[];
*****************************/

void muste_corrtest(char *argv)
        {
        int i;
        char nimi[LNAME];
//      char x[LLENGTH];

//      if (argc==1) return;
        s_init(argv);

        if (g<3)
            {
            init_remarks();

rem_pr("CORRTEST DATA1(VAR1,VAR2),DATA2(VAR1,VAR2),L / or");
rem_pr("CORRTEST TWO-SAMPLE r1,n1,r2,n2,L / for given correlations and sample sizes");
rem_pr("tests the equality of correlation coefficients in two samples.");
rem_pr(" ");
rem_pr("CORRTEST DATA(VAR1,VAR2),L");
rem_pr("tests whether the correlation coefficient is 0.");
rem_pr("CORRTEST ONE-SAMPLE r,n,r0,L / for a given correlation r and sample size n");
rem_pr("tests whether the correlation coefficient is r0.");
rem_pr(" ");
rem_pr("More information by activating");
rem_pr("CORRTEST? ");

            wait_remarks(2); s_end(argv);
            return;
            }

    xx=NULL;
    yy=NULL;
    perm=NULL;

        i=spec_init(r1+r-1); if (i<0) return;
        results_line=0;

        if (muste_strnicmp(word[1],"TWO",3)==0 || muste_strnicmp(word[1],"ONE",3)==0)
            {
            vertailu_annetuilla_arvoilla();
            s_end(argv); return;
            }

        if (g>5)
            {
            results_line=edline2(word[5],1,1);
            if (results_line==0) return;
            }

        i=spec_init(r1+r-1); if (i<0) return;

        strcpy(nimi,etmpd); strcat(nimi,"SURVO.TMP");
        ftemp=muste_fopen(nimi,"w+b");
        if (ftemp==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s !",nimi);
            sur_print(sbuf); WAIT; return;
            }

        if (g<5)
            {
            one_sample_test();
            muste_fclose(ftemp); // 17.9.2011/SM
            s_end(argv); return;
            }

        n[0]=talleta(word[1],word[2],0); if (n[0]<0) return;
        n[1]=talleta(word[3],word[4],1); if (n[1]<0) return;
        nn=n[0]+n[1];
        i=varaa_tilat(); if (i<0) return;
        rewind(ftemp);
        for (i=0; i<n[0]; ++i)
            {
            fread(ff,sizeof(double),2,ftemp);
            xx[i]=(ff[0]-sumx[0])/sumx2[0]; yy[i]=(ff[1]-sumy[0])/sumy2[0];
            }
        for (i=n[0]; i<nn; ++i)
            {
            fread(ff,sizeof(double),2,ftemp);
            xx[i]=(ff[0]-sumx[1])/sumx2[1]; yy[i]=(ff[1]-sumy[1])/sumy2[1];
            }
        fclose(ftemp);
/*
        for (i=0; i<n; ++i)
            {
            printf("\n%g %g",xx[i],yy[i]);
            }
getch();
*/
        g_print=&sur_print;

        maxcount=10000000L;
        i=spfind("SIMUMAX");
        if (i>=0) maxcount=atol(spb[i]);
        conf_level=0.95;
        i=spfind("CONF");
        if (i>=0) conf_level=atof(spb[i]);
        if (conf_level<0.8 || conf_level>=1.0)
            {
            sur_print("\nError in CONF=p! Confidence level p must be 0.8<p<1");
            WAIT; return;
            }
        conf_coeff=muste_inv_std(1.0-(1.0-conf_level)/2);
        disp0();
        if (maxcount)
            {
            spec_rnd();
            i=simulation(); if (i<0) return;
            }
        g_print=&print_line;
        printout();
        muste_fclose(ftemp); // 17.9.2011/SM
        s_end(argv);
        }

static int talleta(char *s1,char *s2,int k)
        {
        int i,j,n;
        int l;
        char *p;
   //   char name[LNAME];
        char *var1,*var2;
        double a,b;


        p=strchr(s1,'(');
        if (p==NULL)
            {
            virhe("( missing!"); return(-1);
            }
        *p=EOS;
        var1=p+1;
        p=strchr(s2,')');
        if (p==NULL)
            {
            virhe(") missing!"); return(-1);
            }
        *p=EOS;
        var2=s2;
        strcpy(nimi[k],s1);
        strcpy(v1[k],var1);
        strcpy(v2[k],var2);
/*
printf("\n%s %s %s",s1,var1,var2);
*/
        i=data_read_open(s1,&d); if (i<0) return(-1);
        i=varfind(&d,var1); if (i<0) return(-1);
        varx[k]=i;
        j=varfind(&d,var2); if (j<0) return(-1);
        vary[k]=j;
        n=0;
        sumx[k]=sumy[k]=sumx2[k]=sumy2[k]=sumxy[k]=0.0;
        for (l=1; l<=d.n; ++l)
            {
            data_load(&d,l,varx[k],&a);
            if (a==MISSING8) continue;
            ff[0]=a;
            data_load(&d,l,vary[k],&b);
            if (b==MISSING8) continue;
            ff[1]=b;
            ++n;
            sumx[k]+=a; sumx2[k]+=a*a;
            sumy[k]+=b; sumy2[k]+=b*b; sumxy[k]+=a*b;
/*
            if (n>MAXN)
                {
                sprintf(sbuf,"More than %d cases in sample %s",MAXN,s1);
                virhe(sbuf); return(-1);
                }
*/
            fwrite(ff,sizeof(double),2,ftemp);
            }
        data_close(&d);
        laske_r(n,k);
        return(n);
        }

static int virhe(char *s)
        {
        sprintf(sbuf,"\nError in CORRTEST command: %s",s);
        sur_print(sbuf); WAIT;
        return(1);
        }

static int laske_r(int n,int k)
        {
        double a,b;
/*
printf("\nn=%d %g %g %g %g %g",n,sumx[k],sumy[k],sumx2[k],sumy2[k],sumxy[k]); getch();
*/
        a=fabs(sumx2[k]-sumx[k]*sumx[k]/n);
        b=fabs(sumy2[k]-sumy[k]*sumy[k]/n);
        if (a<1e-15)
            {
            sprintf(sbuf,"\n1st variable is a constant in sample %d!",k);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (b<1e-15)
            {
            sprintf(sbuf,"\n2nd variable is a constant in sample %d!",k);
            sur_print(sbuf); WAIT; return(-1);
            }
/*
printf("\na=%g b=%g",a,b); getch();
*/
        rr[k]=(sumxy[k]-sumx[k]*sumy[k]/n)/sqrt(a*b);
        sumx[k]/=n; sumy[k]/=n;
        sumx2[k]=sqrt(a/n); sumy2[k]=sqrt(b/n);
        return(1);
        }

static int simulation()
        {
        int i,k,j,h,ii;
        double x,y;
        int d1,d;
        double a,b;
        double rs[2];
        double t;

        sx1=sx2=sy1=sy2=sxy=0.0;
        for (i=0; i<nn; ++i)
            {
            x=xx[i]; y=yy[i];
            sx1+=x; sx2+=x*x; sy1+=y; sy2+=y*y; sxy+=x*y;
            }


        d1=10000; d=0; u=0L; u1=0L;  // 17.9.2011

        while (1)
            {
            for (k=0; k<nn; ++k) perm[k]=k;
            sx11=sx21=sy11=sy21=sxy1=0.0;
            for (j=0; j<n[0]; ++j)
                {
                h=j+(unsigned int)((nn-j)*uniform_dev());
                ii=perm[h];
                x=xx[ii]; y=yy[ii];
                sx11+=x; sx21+=x*x; sy11+=y; sy21+=y*y; sxy1+=x*y;
                perm[h]=perm[j];
                }
            a=fabs(sx21-sx11*sx11/n[0]);
            b=fabs(sy21-sy11*sy11/n[0]);
            rs[0]=(sxy1-sx11*sy11/n[0])/sqrt(a*b);
            sx12=sx1-sx11; sx22=sx2-sx21;
            sy12=sy1-sy11; sy22=sy2-sy21; sxy2=sxy-sxy1;
            a=fabs(sx22-sx12*sx12/n[1]);
            b=fabs(sy22-sy12*sy12/n[1]);
            rs[1]=(sxy2-sx12*sy12/n[1])/sqrt(a*b);
            t=fisher_z(rs,n);

/* printf("\nr1=%g r2=%g t=%g",rs[0],rs[1],t); getch(); continue; */

            ++u; ++d;
            if (t>=test_val) ++u1;
/* printf("\n%g",(double)u1/(double)u); getch(); */
            if (d==d1)
                {
                d=0;
                corrtest_disp();
                headline("");
                if (u>=maxcount) break;
                if (sur_kbhit())
                    {
                    i=sur_getch(); break;
                    }
                }


            }  /* while */




        return(1);
        }

static int varaa_tilat()
        {

        xx=(float *)muste_malloc(nn*sizeof(float));
        if (xx==NULL) { not_enough_memory(); return(-1); }
        yy=(float *)muste_malloc(nn*sizeof(float));
        if (yy==NULL) { not_enough_memory(); return(-1); }
        perm=(unsigned int *)muste_malloc(nn*sizeof(unsigned int));
        if (perm==NULL) { not_enough_memory(); return(-1); }

        return(1);
        }

static double fisher_z(double *r,int *n)
        {
        double r1,r2;

        r1=r[0]; r2=r[1];
        if (r1>0.9999999) r1=0.99999999;
        if (r1<-0.9999999) r1=-0.99999999;
        if (r2>0.9999999) r2=0.99999999;
        if (r2<-0.9999999) r2=-0.99999999;
//      r1=sqrt((double)(n[0]-3)/8.0)*log((1.0+r1)/(1.0-r1));
//      r2=sqrt((double)(n[1]-3)/8.0)*log((1.0+r2)/(1.0-r2));
//      return(r1-r2);
        r1=0.5*log((1.0+r1)/(1.0-r1));
        r2=0.5*log((1.0+r2)/(1.0-r2));
        return((r1-r2)/sqrt(1.0/(double)(n[0]-3)+1.0/(double)(n[1]-3)));
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (CORRTEST)");
        WAIT; return(1);
        }

static int disp0()
        {
        double prob;

        if (r>r3-6) r=1;
        LOCATE(r+2,9);
        sprintf(sbuf,"Comparing correlation coefficients in 2 samples:");
        (*g_print)(sbuf);
        LOCATE(r+3,9);
        sprintf(sbuf,"Sample 1: Data %s, Variables %s,%s  N1=%d R1=%g",
                        nimi[0],v1[0],v2[0],n[0],rr[0]);
        (*g_print)(sbuf);
        LOCATE(r+4,9);
        sprintf(sbuf,"Sample 2: Data %s, Variables %s,%s  N2=%d R2=%g",
                        nimi[1],v1[1],v2[1],n[1],rr[1]);
        (*g_print)(sbuf);
        LOCATE(r+5,9);
        test_val=fisher_z(rr,n);
        prob=1.0-muste_cdf_std(test_val);
        sprintf(sbuf,"Test based on Fisher's z %g Normal approximation P=%g",
                                 test_val,prob);
        (*g_print)(sbuf);

        if (!maxcount) return(1);
        LOCATE(r+6,9);
        sprintf(sbuf,"         N    P       Confidence interval (%g)",conf_level);
        (*g_print)(sbuf);

        LOCATE(r3+2,9); PR_EBLD;
        sprintf(sbuf,"To interrupt, press any key! (max N is %d)",maxcount);
        sur_print(sbuf);

        return(1);
        }

static int corrtest_disp()
        {
        double p1,se,lower,upper;

        LOCATE(r+7,9); PR_EUDL;
        p1=(double)u1/(double)u;
        se=sqrt(p1*(1.0-p1)/(double)u);
        lower=p1-conf_coeff*se; if (lower<0.0) lower=0.0;
        upper=p1+conf_coeff*se; if (upper>1.0) upper=1.0;
        sprintf(sbuf,"%10d %.8f %.8f lower limit",
                       u,p1,lower);
        (*g_print)(sbuf);
        LOCATE(r+8,9);
        sprintf(sbuf,"      s.e. %.8f %.8f upper limit",se,upper);
        (*g_print)(sbuf);
        return(1);
        }

static int printout()
        {
        g_print=print_line;
        output_open(eout);
        disp0();
        if (maxcount) corrtest_disp();
        output_close(eout);
        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

static int vertailu_annetuilla_arvoilla()
    {
    int i,k;
    double r1,r2,u,prob;
    double lr1,lr2;
    int n1,n2;
    int one;

    one=0;
    if (muste_strnicmp(word[1],"ONE",3)==0) one=1;

    if (one && g<6)
        {
        sur_print("\nUsage: CORRTEST ONE-SAMPLE r,n,r0,L");
        sur_print("\nwhen correlation r and sample size n are given");
        sur_print("\nand the null hypothesis is r=r0");
        WAIT;
        return(-1);
        }
    else if (!one && g<7)
        {
        sur_print("\nUsage: CORRTEST TWO-SAMPLE r1,n1,r2,n2,L");
        sur_print("\nwhen correlations r1,r2 and sample sizes n1,n2 are given");
        WAIT;
        return(-1);
        }
    i=spfind(word[2]);
    if (i<0) r1=atof(word[2]);
    else r1=atof(spb[i]);
    if (r1<=-1.0 || r1>=1.0) { r_error(r1); return(1); }
    i=spfind(word[3]);
    if (i<0) n1=atol(word[3]);
    else n1=atol(spb[i]);
    if (n1<3L) { n_error(n1); return(1); }
    i=spfind(word[4]);
    if (i<0) r2=atof(word[4]);
    else r2=atof(spb[i]);
    if (r2<=-1.0 || r2>=1.0) { r_error(r2); return(1); }
    if (one)
        {
        double test_val,prob,prob2;
        int neg;

        results_line=edline2(word[5],1,1);
        if (results_line==0) return(-1);
        if (r2==0.0)
            {
            output_open(eout);
            sprintf(sbuf,"Testing hypothesis: correlation coefficient = 0:");
            print_line(sbuf);
            sprintf(sbuf,"Sample: R=%g N=%d",r1,n1);
            print_line(sbuf);
            test_val=sqrt((double)(n1-2)/(1.0-r1*r1))*r1;
            neg=0; if (test_val<0) neg=1;
            prob=1.0-muste_cdf_t(test_val,(double)(n1-2));
            if (neg) prob2=2*(1-prob); else prob2=2*prob;
            k=sprintf(sbuf,"Standard t test value %g  P=%g (2-tailed P=%g)",
                                     test_val,prob,prob2);
            print_line(sbuf);
            output_close(eout);
            return(1);
            }
        n2=1000000000L;
        }
    else
        {
        i=spfind(word[5]);
        if (i<0) n2=atol(word[5]);
        else n2=atol(spb[i]);
        if (n2<3L) { n_error(n2); return(1); }
        results_line=edline2(word[6],1,1);
        if (results_line==0) return(-1);
        }
    lr1=0.5*log((1.0+r1)/(1.0-r1));
    lr2=0.5*log((1.0+r2)/(1.0-r2));
    u=(lr1-lr2)/sqrt(1.0/(double)(n1-3)+1.0/(double)(n2-3));

    output_open(eout);

    if (one)
        {
        sprintf(sbuf,"Testing hypothesis: correlation coefficient = %g:",r2);
        print_line(sbuf);
        sprintf(sbuf,"Sample: R=%g N=%d",r1,n1);
        print_line(sbuf);
        }
    else
        {
        sprintf(sbuf,"Comparing correlation coefficients in 2 samples:");
        print_line(sbuf);
        sprintf(sbuf,"Samples: R1=%g N1=%d, R2=%g N2=%d",r1,n1,r2,n2);
        print_line(sbuf);
        }
    prob=1.0-muste_cdf_std(u);
    sprintf(sbuf,"Test based on Fisher's z %g Normal approximation P=%g",
                             u,prob);
    print_line(sbuf);
    output_close(eout);

    return(1);
    }

static int r_error(double r)
    {
    sprintf(sbuf,"\nInvalid correlation coefficient %g ",r);
    sur_print(sbuf); WAIT;
    return(1);
    }

static int n_error(int n)
    {
    sprintf(sbuf,"\nInvalid sample size %d ",n);
    sur_print(sbuf); WAIT;
    return(1);
    }


/* corrt1.c 4.6.1996/SM (7.6.1996)
*/
static int u2;
static int neg;

static int one_sample_test()
        {
        int i;
    //  char nimi[LNAME];
    //  char x[LLENGTH];

        results_line=0;
        if (g>3)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return(1);
            }

        n[0]=talleta(word[1],word[2],0); if (n[0]<0) return(1);
        nn=n[0];
        i=varaa_tilat1(); if (i<0) return(1);
        rewind(ftemp);
        for (i=0; i<nn; ++i)
            {
            fread(ff,sizeof(double),2,ftemp);
      /*    xx[i]=(ff[0]-sumx[0])/sumx2[0]; yy[i]=(ff[1]-sumy[0])/sumy2[0]; */
            xx[i]=ff[0]; yy[i]=ff[1];
            }
        fclose(ftemp);
/*
        for (i=0; i<nn; ++i)
            {
            printf("\n%g %g",xx[i],yy[i]);
            }
        getch();
*/
        g_print=&sur_print;

        maxcount=10000000L;
        i=spfind("SIMUMAX");
        if (i>=0) maxcount=atol(spb[i]);
        conf_level=0.95;
        i=spfind("CONF");
        if (i>=0) conf_level=atof(spb[i]);
        if (conf_level<0.8 || conf_level>=1.0)
            {
            sur_print("\nError in CONF=p! Confidence level p must be 0.8<p<1");
            WAIT; return(1);
            }
        conf_coeff=muste_inv_std(1.0-(1.0-conf_level)/2);
        disp01();
        if (maxcount)
            {
            spec_rnd();
            i=simulation1(); if (i<0) return(1);
            }
        g_print=&print_line;
        printout1();
        return(1);
        }

static int simulation1()
        {
        int i,k,j,h,ii;
        double x,y;
        int d1,d;
     // double a,b;
     // double rs[2];
     // double t;
        double asxy,adev;

        sxy=0.0; sx1=sy1=0.0;
        for (i=0; i<nn; ++i)
            {
            x=xx[i]; y=yy[i];
            sxy+=x*y;
            sx1+=x; sy1+=y;
            }
        asxy=sx1*sy1/nn; adev=fabs(sxy-asxy);

        d1=10000; d=0; u=0L; u1=u2=0L;  // 17.9.2011

        while (1)
            {
            for (k=0; k<nn; ++k) perm[k]=k;
            sxy1=0.0;

            for (j=0; j<nn; ++j)
                {
                h=j+(unsigned int)((nn-j)*uniform_dev());
                ii=perm[h];
                x=xx[j]; y=yy[ii];
                sxy1+=x*y;               /* sis_tulo() ? */
                perm[h]=perm[j];
                }

            ++u; ++d;
            if (sxy1>=sxy) ++u1;
            if(fabs(sxy1-asxy)>=adev) ++u2;

            if (d==d1)
                {
                d=0;
                disp1();
                headline("");
                if (u>=maxcount) break;
                if (sur_kbhit())
                    {
                    i=sur_getch(); break;
                    }
                }


            }  /* while */

        return(1);
        }

static int varaa_tilat1()
        {

        xx=(float *)muste_malloc(nn*sizeof(float));
        if (xx==NULL) { not_enough_memory(); return(-1); }
        yy=(float *)muste_malloc(nn*sizeof(float));
        if (yy==NULL) { not_enough_memory(); return(-1); }
        perm=(unsigned int *)muste_malloc(nn*sizeof(unsigned int));
        if (perm==NULL) { not_enough_memory(); return(-1); }

        return(1);
        }


static int disp01()
        {
        int k;
        double prob,prob2;

        if (r>r3-6) r=1;
        LOCATE(r+2,9);
        sprintf(sbuf,"Testing hypothesis correlation coefficient = 0:");
        (*g_print)(sbuf);
        LOCATE(r+3,9);
        sprintf(sbuf,"Sample: Data %s, Variables %s,%s  N=%d R=%g",
                        nimi[0],v1[0],v2[0],n[0],rr[0]);
        (*g_print)(sbuf);
        LOCATE(r+4,9);
        test_val=sqrt((double)(nn-2)/(1.0-rr[0]*rr[0]))*rr[0];
        neg=0; if (test_val<0) neg=1;
        prob=1.0-muste_cdf_t(test_val,(double)(nn-2));
        if (neg) prob2=2*(1-prob); else prob2=2*prob;
        k=sprintf(sbuf,"Standard t test value %g  P=%g (2-tailed P=%g)",
                                 test_val,prob,prob2);
        (*g_print)(sbuf);

        if (!maxcount) return(1);
        LOCATE(r+5,9);
        sprintf(sbuf,"           1-tailed test         2-tailed test");
        (*g_print)(sbuf);

        LOCATE(r+6,9);
        sprintf(sbuf,"       N   P          Conf.int.  P          Conf.int.   (%g)",conf_level);
        (*g_print)(sbuf);

        LOCATE(r3+2,9); PR_EBLD;
        sprintf(sbuf,"To interrupt, press any key! (max N is %d)",maxcount);
        sur_print(sbuf);

        return(1);
        }

static int disp1()
        {
        double p1,se,lower,upper;
        double p2,se2,lower2,upper2;

        LOCATE(r+7,9); PR_EUDL;
        p1=(double)u1/(double)u;
        se=sqrt(p1*(1.0-p1)/(double)u);
        lower=p1-conf_coeff*se; if (lower<0.0) lower=0.0;
        upper=p1+conf_coeff*se; if (upper>1.0) upper=1.0;
        p2=(double)u2/(double)u;
        se2=sqrt(p2*(1.0-p2)/(double)u);
        lower2=p2-conf_coeff*se2; if (lower2<0.0) lower2=0.0;
        upper2=p2+conf_coeff*se2; if (upper2>1.0) upper2=1.0;
        sprintf(sbuf,"%10d %.8f %.8f %.8f %.8f lower limit",
                       u,p1,lower,p2,lower2);
        (*g_print)(sbuf);
        LOCATE(r+8,9);
        sprintf(sbuf,"      s.e. %.8f %.8f %.8f %.8f upper limit",se,upper,se2,upper2);
        (*g_print)(sbuf);
        return(1);
        }

static int printout1()
        {
        g_print=print_line;
        output_open(eout);
        disp01();
        if (maxcount) disp1();
        output_close(eout);
        return(1);
        }

