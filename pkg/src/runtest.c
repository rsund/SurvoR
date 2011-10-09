/* _runtest.c 9.5.2001/SM (9.5.2001)
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
static int tvar;
static int n; // otoskoko
static int *seq;
static int *perm;
static int maxcount;
static int u,u1;
static int e,e1;
static double conf_level,conf_coeff;

static int n0,n1,run0,run1;
static int run01,run11;
static int *r0_len,*r1_len;
static int r0_n,r1_n;
static double x2,x21,df,dp;
static int rcond;
static int varsum; // test statistics = sum of variances
static double dp_run1; // F(run1)
static double a0,a1; // varianssien kertoimet O'Brien,Dyck
static int permu;
static int *freq;
static double chi2_geom,dp_geom;
static int df_geom;
static double chi00,chi01;
static int df00,df01;
static double dp_geom0,dp_geom1;
static char chi2_check[LNAME];
static double minf;
       // int geom0; // 1=nollaluokat mukaan
static int geom00,geom01;
static double f2[8];
static double fm[4],fn[2];
static double x2_pair,px2_pair;
static int runs=0; // 24.12.2009
static int *runs0,*runs1;
static int results_line;

static int lue_data();
static int varaa_tilat();
static int basic_stat();
static int run_stat();
static int comp_freq0s();
static int pair_stat();
static int chi2_comp(double *f,double *fm,double *fn,int m,int n,double *pc2);
static int ww_test();
static int run1_test();
static double bin_coeff(int n,int m);
static int run_test2();
static int count_run_lengths();
static int o_brien(int k);
static int o_brien_arvonta();
static int fit_geom();
static int comp_freq(int n,int *len);
static int comp_chi2(int nr,int n,int n1,int k,int freq0,double *pchi2,int *pdf);
static int simulation();
static int not_enough_memory();
static int disp0();
static int permutoi();
static int runtest_disp();
static int printout();
static int print_line(char *line);

extern double muste_cdf_chi2();
extern double uniform_dev();

int (*g_print)();

// char **specs;
/***********************
void main(argc,argv)
int argc; char *argv[];
************************/

void muste_runtest(char *argv)
        {
        int i;
   //   char nimi[LNAME];
        char x[LLENGTH];
        extern double muste_inv_std();

   //   if (argc==1) return;
        s_init(argv);

        if (g<3)
            {
            init_remarks();

rem_pr("RUNTEST <data>,<var>,L");
rem_pr("tests randomness of a sequence of 0's and 1's.");
            wait_remarks(2); s_end(argv);
            return;
            }

        runs=0;

        results_line=0;
        if (g>3)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return;
            }

        i=spec_init(r1+r-1); if (i<0) return;

    seq=NULL;
    perm=NULL;
    r1_len=NULL;
    r0_len=NULL;
    freq=NULL;
    runs0=NULL;
    runs1=NULL;

        spec_rnd();

        *chi2_check=EOS;
        i=spfind("CHI2_CHECK");
        if (i>=0) strcpy(chi2_check,spb[i]);

        minf=5.0;
        i=spfind("MINF");
        if (i>=0) minf=atof(spb[i]);

        i=data_read_open(word[1],&d); if (i<0) return;
        tvar=varfind(&d,word[2]); if (tvar<0) return;

        i=spfind("RUNS"); // 24.12.2009
        if (i>=0 && atoi(spb[i]))
            { count_run_lengths(); s_end(argv[1]); return; }

        i=lue_data(); if (i<0) return;
        if (n<10)
            {
            sprintf(sbuf,"Too few (%d) observations!",n);
            sur_print(sbuf); WAIT; return;
            }
        for (i=0; i<n; ++i) perm[i]=i;

        basic_stat();
        if (n0==0 || n1==0)
            {
            sur_print("\nThe sequence consists of plain 0's or 1's.");
            sur_print("\nNothing to be done!");
            WAIT; return;
            }

        run_stat();
        comp_freq0s();

        strcpy(x,etmpd); strcat(x,"RUNTEST.TMP");
        ftemp=muste_fopen(x,"wt");
        for (i=0; i<r0_n; ++i) fprintf(ftemp,"%d\n",r0_len[i]);
        fprintf(ftemp,"***\n");
        for (i=0; i<r1_n; ++i) fprintf(ftemp,"%d\n",r1_len[i]);

        i=spfind("RUN1");
        if (i>=0 && atoi(spb[i])>0)
            run1_test();
        else
            ww_test();
        o_brien(0);
        pair_stat();
        fit_geom();
//      pair_stat3();

        g_print=&sur_print;

        rcond=0;
        i=spfind("RCOND");
        if (i>=0) rcond=atoi(spb[i]);

        permu=0;
        i=spfind("PERM");
        if (i>=0) permu=atoi(spb[i]);

        varsum=0;
        i=spfind("VARSUM");
        if (i>=0) varsum=atoi(spb[i]);

        maxcount=1000000L;
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
            i=simulation(); if (i<0) return;
            }

        g_print=&print_line;
        printout();

        s_end(argv);
        }

static int lue_data()
    {
    int i;
    char nimi[LNAME];
    int j;
    double a;

    n=0;
    i=conditions(&d); if (i<0) return(-1);
    strcpy(nimi,etmpd); strcat(nimi,"RUNTEST.TMP");
    ftemp=muste_fopen(nimi,"wt");
    for (j=d.l1; j<=d.l2; ++j)
        {
        if (unsuitable(&d,j)) continue;
        data_load(&d,j,tvar,&a);
        fprintf(ftemp,"%d\n",(int)a);
        ++n;
        }
    muste_fclose(ftemp);

    if (n<2)
        {
        sur_print("\nNot enough observations (less than 2)");
        WAIT; return(-1);
        }

    i=varaa_tilat(); if (i<0) return(-1);

    ftemp=muste_fopen(nimi,"rt");
    for (i=0; i<n; ++i)
        {
        fscanf(ftemp,"%d\n",&seq[i]);
        }
    muste_fclose(ftemp);

// printf("\n");
// for (i=0; i<n; ++i) printf("%d",seq[i]); getch();
    return(1);
    }

static int varaa_tilat()
    {
    seq=(int *)muste_malloc(n*sizeof(int));
    if (seq==NULL) { not_enough_memory(); return(-1); }
    perm=(int *)muste_malloc(n*sizeof(int));
    if (perm==NULL) { not_enough_memory(); return(-1); }
    r1_len=(int *)muste_malloc(n*sizeof(int));
    if (r1_len==NULL) { not_enough_memory(); return(-1); }
    r0_len=(int *)muste_malloc(n*sizeof(int));
    if (r0_len==NULL) { not_enough_memory(); return(-1); }
    freq=(int *)muste_malloc(n*sizeof(int));
    if (freq==NULL) { not_enough_memory(); return(-1); }


    return(1);
    }

static int basic_stat()
    {
    int i;
//  int type;

    n0=0;

    for (i=0; i<n; ++i)
        {
        if (seq[i]==0) ++n0;
        else seq[i]=1; // ei-nollat -> 1
        }

    n1=n-n0;
/***************************************
    if (n1<n0) // ykköset enemmistöksi!  Ei tarvita!
        {
        for (i=0; i<n; ++i) seq[i]=1-seq[i];
        i=n1; n1=n0; n0=i;
        }
**************************************/
    return(1);
    }

static int run_stat()
    {
    int i,k;
    int run;
    int runlen;

    run0=run1=r1_n=r0_n=0;
    run=seq[perm[0]];
    if (run==0) run0=1;
    else run1=1;
    runlen=1;

    for (i=1; i<n; ++i)
        {
        k=seq[perm[i]];
        if (k==0)
            {
            if (run==0) { ++runlen; continue; }  // 00
            r1_len[r1_n++]=runlen; run=0; runlen=1; ++run0; // 10
            }
        else
            {
            if (run==1) { ++runlen; continue; } // 11
            r0_len[r0_n++]=runlen; run=1; runlen=1; ++run1; // 01
            }
        }
    k=seq[perm[n-1]];
    if (k==0) r0_len[r0_n++]=runlen;
    else      r1_len[r1_n++]=runlen;

/***************************************
printf("\nrun0=%d run1=%d|",run0,run1);
printf("\npituudet: %d %d",r0_n,r1_n);
printf("\n");
for (i=0; i<r0_n; ++i) printf(" %d",r0_len[i]);
printf("\n");
for (i=0; i<r1_n; ++i) printf(" %d",r1_len[i]);
i=getch(); if (i=='.') exit(0);
****************************************/
    return(1);
    }

static int comp_freq0s()
    {
    int i;

    geom00=0;
    for (i=0; i<r1_n; ++i) geom00+=r1_len[i]-1;
    geom01=0;
    for (i=0; i<r0_n; ++i) geom01+=r0_len[i]-1;
    return(1);
    }

static int pair_stat()
    {
    int i;
//  double e;

    for (i=0; i<8; ++i) f2[i]=0;
    for (i=2; i<n; ++i)
        ++f2[4*seq[i]+seq[i-1]+2*seq[i-2]];

//  printf("\nfreq:");
//  for (i=0; i<8; ++i) printf(" %g",f2[i]); getch();

    chi2_comp(f2,fm,fn,4,2,&x2_pair);


    px2_pair=1-muste_cdf_chi2(x2_pair,3.0,1e-15);
// printf("\nx2_pair=%g p=%g ",x2_pair,px2_pair); getch();

    return(1);
    }

static int chi2_comp(double *f,double *fm,double *fn,int m,int n,double *pc2)
    {
    int i,j;
    double nn;
    double a,b;

    nn=0.0;
    for (i=0; i<m*n; ++i) nn+=f[i];
    for (i=0; i<m; ++i)
        {
        a=0.0;
        for (j=0; j<n; ++j) a+=f[i+m*j];
        fm[i]=a;
        }
    for (j=0; j<n; ++j)
        {
        a=0.0;
        for (i=0; i<m; ++i) a+=f[i+m*j];
        fn[j]=a;
        }

    *pc2=0.0;
    for (i=0; i<m; ++i) for (j=0; j<n; ++j)
        {
        a=f[i+m*j];
        b=fm[i]*fn[j]/nn;
        *pc2+=(a-b)*(a-b)/b;
        }
    return(1);
    }

/*****************************************

p(M,N,x)|=if(2*int(x/2)=x)then(p2(M,N,x/2))else(p1(M,N,(x-1)/2))
p1(M,N,k)|=((M+N)/k-2)*A(M,N,k)
p2(M,N,k)|=2*A(M,N,k)
A(M,N,k)|=if(k=1)then(1/C(M+N,M))else(A2(M,N,k))
A2(M,N,k)|=(M/(k-1)-1)*(N/(k-1)-1)*A(M,N,k-1)

*****************************************/

static int ww_test()
    {
    int i,k;
    int nr;
    int pariton;
    double a,pr;

    nr=run0+run1;
    if (n>1000) { run_test2(); return(1); }  // normal approx.
    a=1.0/bin_coeff(n,n1);
    dp_run1=2*a; pariton=0;
    for (i=3; i<=nr; ++i)
        {
        k=i/2; pariton=1-pariton;
        if (!pariton)
          a*=((double)n0/(double)(k-1)-1.0)*((double)n1/(double)(k-1)-1.0);
        if (pariton) pr=((double)(n0+n1)/(double)k-2.0)*a;
        else pr=2*a;
        dp_run1+=pr;
// printf("\ni=%d pr=%g sum=%g",i,pr,dp_run1); getch();
        }
    return(1);
    }

// P(N,X,S)|=if(X=1)then((N-S+1)/C(N,S))else(P2(N,X,S))
// P2(N,X,S)|=(S-X+1)*(N-S-X+2)/X/(X-1)*P(N,X-1,S)
// n=N X=run1 S=n1

static int run1_test()
    {
    int i;
    double a;

    if (n>1000) { run_test2(); return(1); }  // normal approx.
    a=(double)(n-n1+1)/bin_coeff(n,n1);
    dp_run1=a;
    for (i=2; i<=run1; ++i)
        {
        a*=(n1-i+1)*(n-n1-i+2)/(double)(i*(i-1));
        dp_run1+=a;
        }
    return(1);
    }

static double bin_coeff(int n,int m)
    {
    double u,v,y;
    int iu,iv;

    iv=v=n; iu=u=m;
    if ((double)iu!=u) return(0.0);
    if ((double)iv!=v) return(0.0);
    if (u>v/2) u=v-u;
    if (u<0 || v<0) return(0.0);
    if (u==0.0) return(1.0);
    y=1.0;
    for (; u>0; --u, --v) y*=(v/u);
    return(y);
    }

static int run_test2() // normal approximation
    {
    double mean,s,r;
    extern double muste_st_norm();
    double u,u0,u1;

    r=run0+run1;
    u=n; u0=n0; u1=n1;
    mean=2.0*u0*u1/u+1.0;
    s=sqrt(2.0*u0*u1*(2.0*u0*u1-u)/(u*u*(u-1.0)));
//  printf("\nr=%g mean=%g s=%g",r,mean,s); getch();
    dp_run1=muste_st_norm((r-mean)/s,0.0);

    return(1);
    }

#define MAX_RUN_LENGTH 1000

static int count_run_lengths()
    {
    int i,k,len;
    int run_type;
    int j;
    double a;


    runs0=(int *)muste_malloc((MAX_RUN_LENGTH+1)*sizeof(int));
    runs1=(int *)muste_malloc((MAX_RUN_LENGTH+1)*sizeof(int));
    for (i=1; i<=MAX_RUN_LENGTH; ++i) { runs0[i]=runs1[i]=0L; }

    data_load(&d,1L,tvar,&a);
    if ((int)a==0) run_type=0; else run_type=1;
    len=1;
    for (j=d.l1+1; j<=d.l2; ++j)
        {
        data_load(&d,j,tvar,&a);
        k=(int)a;
        if (k==run_type)
            {
            ++len;
            if (len>MAX_RUN_LENGTH)
                {
                sprintf(sbuf,"\nRun_length>%d !",MAX_RUN_LENGTH);
                sur_print(sbuf); WAIT; return(0);
                }
            }
        else
            {
            if (run_type==0) ++runs0[len]; else ++runs1[len];
            run_type=1-run_type; len=1;
            }
        }

    k=MAX_RUN_LENGTH;
    while (k>0)
        {
        if (runs0[k]!=0L || runs1[k]!=0L) break;
        --k;
        }

    edwrite(space,results_line,1);
    edwrite("Run_length   0-runs      1-runs",results_line,1);
    ++results_line;
    for (i=1; i<=k; ++i)
        {
        sprintf(sbuf,"%4d %10d  %10d",i,runs0[i],runs1[i]);
        edwrite(space,results_line,1);
        edwrite(sbuf,results_line,1);
        ++results_line;
        }
    return(1);
    }

static int o_brien(int k)
    {
    int i;
    double a,s,s2,dn;
    double var0,var1;
    double b0,b1;

    s=s2=0.0;
    for (i=0; i<r0_n; ++i)
        {
        a=r0_len[i];
        s+=a; s2+=a*a;
        }
    dn=r0_n;
    var0=(s2-s*s/dn)/(dn-1.0);

    s=s2=0.0;
    for (i=0; i<r1_n; ++i)
        {
        a=r1_len[i];
        s+=a; s2+=a*a;
        }
    dn=r1_n;
    var1=(s2-s*s/dn)/(dn-1.0);


    if (k==1) { x2=a0*var0+a1*var1; return(1); }

    if (n0-run0-1>0)
        a0=(double)(run0-1)*(run0+1)*(run0+2)*(run0+3)/
           (double)(2*(double)run0*(n0+1)*(n0-run0-1));
    else a0=1.0;

    if (n1-run1-1>0)
        a1=(double)(run1-1)*(run1+1)*(run1+2)*(run1+3)/
           (double)(2*(double)run1*(n1+1)*(n1-run1-1));
    else a1=1.0;

    if (varsum) { a0=a1=1.0; }

    x2=a0*var0+a1*var1;
// printf("\nx2=%g|",x2); getch();
//  if (k==1) return(1);

    b0=a0*n0*(n0-run0)/(double)(run0*(run0+1));
    b1=a1*n1*(n1-run1)/(double)(run1*(run1+1));
    df=b0+b1;
    if (df<=0.0) df=1.0;
// printf("\ndf=%g|",df); getch();

    dp=1-muste_cdf_chi2(x2,df,1e-15);
// printf("\ndp=%g|",dp); getch();

    x21=x2; run01=run0; run11=run1;

    return(1);
    }

static int o_brien_arvonta()  // r0,r1 kiinnitetty
    {
    int i,k,h;
    double s,s2,a;
    double dn,var0,var1;

    for (i=0; i<n0; ++i) perm[i]=0;
    for (i=0; i<run0-1; ++i)
        {
        while (1)
            {
            k=(n0-1)*uniform_dev();
            if (perm[k]) continue;
            perm[k]=1; break;
            }
        }
    k=0; h=1; perm[n0-1]=1;
    for (i=0; i<n0; ++i)
        {
        if (perm[i]) { r0_len[k++]=h; h=1; continue; }
        ++h;
        }

    for (i=0; i<n1; ++i) perm[i]=0;
    for (i=0; i<run1-1; ++i)
        {
        while (1)
            {
            k=(n1-1)*uniform_dev();
            if (perm[k]) continue;
            perm[k]=1; break;
            }
        }
    k=0; h=1; perm[n1-1]=1;
    for (i=0; i<n1; ++i)
        {
        if (perm[i]) { r1_len[k++]=h; h=1; continue; }
        ++h;
        }
/*******************************
printf("\n");
for (i=0; i<r0_n; ++i) printf(" %d",r0_len[i]);
printf("\n");
for (i=0; i<r1_n; ++i) printf(" %d",r1_len[i]);
i=getch(); if (i=='.') exit(0);
***********************************/
    s=s2=0.0;
    for (i=0; i<r0_n; ++i)
        {
        a=r0_len[i];
        s+=a; s2+=a*a;
        }
    dn=r0_n;
    var0=(s2-s*s/dn)/(dn-1.0);

    s=s2=0.0;
    for (i=0; i<r1_n; ++i)
        {
        a=r1_len[i];
        s+=a; s2+=a*a;
        }
    dn=r1_n;
    var1=(s2-s*s/dn)/(dn-1.0);

    x2=a0*var0+a1*var1;

// printf("\nx2=%g",x2); i=getch(); if (i=='.') exit(0);

    return(1);
    }

static int fit_geom()
    {
//  double e,o;
    int k;
    double chi20,chi21;
    int df0,df1;

    chi2_geom=0.0; df_geom=0;

    k=comp_freq(r0_n,r0_len);
    comp_chi2(r0_n,n,n0,k,000000,&chi20,&df0);
    chi2_geom=chi20; df_geom=df0;
    comp_chi2(r0_n,n,n0,k,geom00,&chi00,&df00);



    k=comp_freq(r1_n,r1_len);
    comp_chi2(r1_n,n,n1,k,000000,&chi21,&df1);
    chi2_geom+=chi21; df_geom+=df1;
    comp_chi2(r1_n,n,n1,k,geom01,&chi01,&df01);

    dp_geom=-1.0;

    df_geom-=1.0;
    if (df_geom>0) dp_geom=1.0-muste_cdf_chi2(chi2_geom,(double)df_geom,1e-15);

    dp_geom0=-1.0;
    --df00;
    if (df00>0) dp_geom0=1.0-muste_cdf_chi2(chi00,(double)df00,1e-15);
    dp_geom1=-1.0;
    --df01;
    if (df01>0) dp_geom1=1.0-muste_cdf_chi2(chi01,(double)df01,1e-15);


    if (*chi2_check)
        {
        ftemp=muste_fopen(chi2_check,"a+t");
        fprintf(ftemp,"%g %d ",chi20,df0);
        fprintf(ftemp,"%g %d ",chi21,df1);
        fprintf(ftemp,"%g %g %g ",dp_geom,dp_geom0,dp_geom1);
        fprintf(ftemp,"%g %g ",dp_run1,dp); // WW ja O'Brien P-arvot
        fprintf(ftemp,"%g ",px2_pair); // triple
        fprintf(ftemp,"%d\n",n1);
        muste_fclose(ftemp);
        }

    return(1);
    }

static int comp_freq(int n,int *len)
    {
    int i,j,k;

    for (i=0; i<n; ++i) freq[i]=0;
    k=0;
    for (i=0; i<n; ++i)
        {
        j=len[i];
        ++freq[j];
        if (j>k) k=j;
        }
// printf("\nfreq:");
// for (i=0; i<=k; ++i) printf(" %d",freq[i]); getch();

    return(k);
    }

static int comp_chi2(int nr,int n,int n1,int k,int freq0,double *pchi2,int *pdf)
    {
    int i;
    double p;
    double e,o,a;
    double esum;
    double freqsum;
    int nr0;

    p=(double)n1/(double)n;

    esum=0.0; freqsum=0;
    *pdf=0; *pchi2=0.0;
    nr0=nr;

    if (freq0)
        {
        nr0=nr+freq0;
        e=nr0*(1.0-p);
        a=freq0-e;
// printf("\n 0: e=%g o=%d",e,freq0); getch();
        *pchi2+=a*a/e;
        ++*pdf;
        esum+=e; freqsum+=freq0;
        e*=p;
        }
    else e=nr0*(1-p);
    for (i=0; i<=k; ++i)
        {
        ++*pdf;
// printf("\n i+1=%d e=%g o=%d",i+1,e,freq[i+1]); getch();
        a=freq[i+1]-e;
        *pchi2+=a*a/e;
        esum+=e; freqsum+=freq[i+1];
        e*=p;
        if (e<minf)
            {
            e=nr0-esum;
            o=nr0-freqsum;
// printf("\nloppu: e=%g o=%g",e,o); getch();
            a=o-e;
            *pchi2+=a*a/e;
            break;
            }
        }

//  *pdf-=2;
// printf("\nchi2=%g df=%d|",*pchi2,*pdf); getch();
    return(1);
    }

static int simulation()
        {
        int i;

        e1=100000; e=0; u=0L; u1=0L;

        while (1)
            {
/**********************************
printf("\n");
for (i=0; i<r0_n; ++i) printf(" %d",r0_len[i]);
printf("\n");
for (i=0; i<r1_n; ++i) printf(" %d",r1_len[i]);
i=getch(); if (i=='.') exit(0);
**********************************/

            if (permu)
                {
                while (1)
                    {
                    permutoi();
                    run_stat();
                    if (!rcond) break;
                    if (run0==run01 && run1==run11) break;
                    }
                o_brien(1);
                }
            else
                o_brien_arvonta();

// printf("\n%g %g",x2,x21); getch();

            ++u; ++e;
            if (x2>x21) ++u1;
            if (e==e1)
                {
                e=0;
                runtest_disp();
                headline("");
                if (u>=maxcount) break;
                if (sur_kbhit())
                    {
                    i=sur_getch(); break;
                    }
                }

            }

        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (RUNTEST)");
        WAIT; return(1);
        }

static int disp0()
        {


        if (r>r3-10) r=1;
        LOCATE(r+2,9);
        sprintf(sbuf,"Run tests for %s in data %s:",word[2],word[1]);
        (*g_print)(sbuf);

        LOCATE(r+3,9);
        sprintf(sbuf,"N=%d N0=%d N1=%d p=%g run0=%d run1=%d",
                        n,n0,n1,(double)n1/(double)n,run01,run11);
        (*g_print)(sbuf);


        LOCATE(r+4,9);
        if (n<=1000)
            sprintf(sbuf,"Wald-Wolfowitz test: P=%g",dp_run1);
        else
            sprintf(sbuf,"Wald-Wolfowitz test: P=%g (normal approx.)",dp_run1);
        (*g_print)(sbuf);

        LOCATE(r+5,9);
        sprintf(sbuf,"Geometric distribution test: X2=%g df=%d P=%g",
                        chi2_geom,df_geom,dp_geom);
        (*g_print)(sbuf);

        LOCATE(r+6,9);
        if (dp_geom0<0.0)
        sprintf(sbuf,"     for 0-runs separately: -");
        else
        sprintf(sbuf,"     for 0-runs separately: X20=%g df=%d P=%g",
                      chi00,df00,dp_geom0);
        (*g_print)(sbuf);

        LOCATE(r+7,9);
        if (dp_geom1<0.0)
        sprintf(sbuf,"     for 1-runs separately: -");
        else
        sprintf(sbuf,"     for 1-runs separately: X21=%g df=%d P=%g",
                      chi01,df01,dp_geom1);
        (*g_print)(sbuf);

        LOCATE(r+8,9);
        sprintf(sbuf,"Chi_square test for triples: X2=%g df=3 P=%g",
                        x2_pair,px2_pair);
        (*g_print)(sbuf);

        LOCATE(r+9,9);
        sprintf(sbuf,"O'Brien-Dyck test: X2=%g df=%g P=%g",
                        x21,df,dp);
        (*g_print)(sbuf);

        return(1);
        }

static int permutoi()
        {
        int i,k,h;

//      for (i=0; i<n; ++i) perm[i]=i;

        for (i=n-1; i>=1; --i)
            {
            k=n*uniform_dev();
            h=perm[i]; perm[i]=perm[k]; perm[k]=h;
            }

/************************
        for (i=0; i<n; ++i)
            {
            k=n*uniform_dev();
            h=perm[i]; perm[i]=perm[k]; perm[k]=h;
            }
************************/
        return(1);
        }


static int runtest_disp()
        {
        double p1,se,lower,upper;

        LOCATE(r+10,9); PR_EUDL;
        p1=(double)u1/(double)u;
        se=sqrt(p1*(1.0-p1)/(double)u);
        lower=p1-conf_coeff*se; if (lower<0.0) lower=0.0;
        upper=p1+conf_coeff*se; if (upper>1.0) upper=1.0;
        sprintf(sbuf,"%10d %.8f %.8f lower limit (O'Brien-Dyck test)",
                       u,p1,lower);
        (*g_print)(sbuf);
        LOCATE(r+11,9);
        sprintf(sbuf,"      s.e. %.8f %.8f upper limit (conf.level=%g)",se,upper,conf_level);
        (*g_print)(sbuf);
        return(1);
        }

static int printout()
        {
        g_print=print_line;
        output_open(eout);
        disp0();
        if (maxcount) runtest_disp();
        output_close(eout);
        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

