/* !compare.c 4.6.1986/SM (8.11.1991)
*/
#include <stdio.h>
#include <stdlib.h>
// RS REM #include <conio.h>
// RS REM #include <process.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"
/************************
char *specs0[]={ "SIMUMAX", "SEED", "TEST", "NORM",
                                              "!" };
char **specs=specs0;
***********************/
/*
main(argc,argv)
int argc; char *argv[];
*/

#define RAND_MAX1 32768
#define RND (double)(rand()%RAND_MAX1)/(double)RAND_MAX1   // SM CHA 32768.0 -> RAND_MAX1

static void op_compare();
static int load_samples();
static int not_enough_memory();
static int sort_joint_sample();
static int Mann_Whitney(int test);
static int only_two_samples_permitted();
static int clear_screen();
static int basic_statistics();
static int init_rivi(char *x);
static int init_rivi(char *x);
static int set(char *x,char *s,int pos);
static int eoutput(char *rivi);
static int Wald_Wolfowitz();
static int count_runs();
static int ww_test(int n,int n1,int nr);
static double bin_coeff(int n,int m);
static int run_test2(int n,int n1,int nr);
static int make_qq_data();
static int qq_create_data(char *name,double *x,double *y,int n);
static int tee_muuttujanimet(char *var1,char *var2);
static int qq_sort(double *x,int n);
static int Smirnov();
static double probks(double alam);
static int Kruskal_Wallis(int test);
static int anova();
static int Wilcoxon();
static int rank_sort(int n,double *y,int *order,float *rank);
static int paired_statistics();
static double sgn(double x);
static int rank_corr();
static int corr_statistics(double *par,double *psr1);
static char *spois(char *s);
static int Kendall_tau(int n,float *xrank,float *yrank,int *pnc,int *pnd,double *ptau,double *pptau);
static int op_compd();
static int d_load_samples();
static int sort_sample();
static int test_normality();
static int d_basic_statistics();
static int Shapiro_Wilk();
static int Dagostino();
// static int tblread(FILE *taulu,int n,float *p);
static int Anderson_Darling();

// tilapäisesti:
// static double muste_st_norm(double x,double upper);
// static double muste_cdf_std(double x);

void muste_compare(char *argv)
        {

//      if (argc==1) return;
        s_init(argv);

        if (g<3)
            {
            sur_print("\nUsage: COMPARE data1(variable),data2(variable),...,L");
            sur_print("\nFor more information, see COMPARE?                  ");
            WAIT; return;
            }
        if (*word[2]=='#') { op_compd(); s_end(argv); return; }
        op_compare(); s_end(argv);
        }

/* comp1.c 4.6.1986/SM (17.7.1990) (18.7.2011)
*/

#define MAX_N 10

static SURVO_DATA d;

static int n_sample;
static double *x_space;
static int *samp;
static float *rank;
// static int x_size;
static int sample[MAX_N], ns[MAX_N];
static int n_total;

static int results_line;
static int simumax;
static char survoxxx[LNAME];
static FILE *data;

static int p1=30;
static int p2=50;
static int disp_gap=10000; // new parameter 3.7.2011/SM

static double sb1,b2; // 18.7.2011

static void op_compare()
        {
        int i;
        char test[LLENGTH];
        if (g<2)
            {
            sur_print("\nUsage: COMPARE data1(variable),data2(variable),...,L");
            WAIT; return;
            }
        strcpy(survoxxx,etmpd); strcat(survoxxx,"SURVO.XXX");
        i=edline2(word[g-1],1,0);
        if (i==0) { results_line=0; n_sample=g-1; }
        else { results_line=i; n_sample=g-2; }
        if (n_sample>=MAX_N+1)
            {
            sprintf(sbuf,"\nToo many samples (max=%d)",MAX_N+1);
            sur_print(sbuf); WAIT; return;
            }

        i=spec_init(r1+r-1); if (i<0) return;
        i=load_samples(); if (i<0) return;
        i=spfind("SIMUMAX");
        if (i<0) simumax=100000; else simumax=atol(spb[i]);
        i=spfind("SEED");
        if (i>=0) { i=atoi(spb[i]); srand(i); }

        i=spfind("TEST");
        if (i<0) *test=EOS; else { strcpy(test,spb[i]); muste_strupr(test); }

        i=spfind("DISP_GAP");           // 3.7.2011/SM
        if (i>=0) disp_gap=atoi(spb[i]);

        scroll_line=2; LOCATE(2,1);
        space_break=0;
        if (n_sample==1)
            {
            sur_print("\nAt least 2 samples required!"); WAIT; return;
            }
        if (strncmp(test,"MANN",4)==0 || strncmp(test,"PITM",4)==0)
            { Mann_Whitney(1); return; }
        if (strcmp(test,"T")==0)
            { Mann_Whitney(0); return; }
        if (strncmp(test,"SMIR",4)==0 || strncmp(test,"KOLM",4)==0)
            { Smirnov(); return; }
        if (strncmp(test,"KRUS",4)==0)
            { Kruskal_Wallis(1); return; }
        if (strcmp(test,"F")==0)
            { Kruskal_Wallis(0); return; }
        if (strncmp(test,"PAIR",4)==0 || strncmp(test,"WILC",4)==0)
            { Wilcoxon(); return; }
        if (strncmp(test,"CORR",4)==0 || strncmp(test,"KEND",4)==0)
            { rank_corr(); return; }
        if (strncmp(test,"RUNS",4)==0 || strncmp(test,"WALD",4)==0)
            { Wald_Wolfowitz(); return; } // 27.5.2001

        if (strcmp(test,"Q-Q")==0) // 26.5.2005
            { make_qq_data(); return; }

        if (*test==EOS && n_sample==2)
            { Mann_Whitney(1); return; }
        if (*test==EOS && n_sample>2)
            { Kruskal_Wallis(1); return; }   // () -> (1) 1.7.2011

        sprintf(sbuf,"\nUnknown specification TEST=%s",test); sur_print(sbuf);
        WAIT;
        }

static int load_samples()
        {
        int i,k;
        int h;
        char x[LLENGTH];
        char *p,*q;
        char *var;
        int nro;
        int j;
        double y;

        n_total=0;
        sample[0]=0;
        data=muste_fopen(survoxxx,"wb");
        if (data==NULL)
            {
            sur_print("\nCannot open temporary file SURVO.XXX");
            WAIT; return(-1);
            }
        for (i=0; i<n_sample; ++i)
            {
            strcpy(x,word[i+1]);
            p=strchr(x,'(');
            if (p!=NULL)
                {
                *p=EOS; ++p;
                q=strchr(p,')');
                if (q==NULL)
                    {
                    sprintf(sbuf,"\n) missing in %s",word[i+1]); sur_print(sbuf);
                    WAIT; return(-1);
                    }
                *q=EOS;
                var=p;
                }
            else var=x;
            h=data_open(x,&d); if (h<0) return(-1);
            nro=varfind(&d,var); if (nro<0) return(-1);
            ns[i]=0;
            for (j=d.l1; j<=d.l2; ++j)
                {
                data_load(&d,j,nro,&y);
                if (y==MISSING8) continue;
                p=(char *)&y;
                for (h=0; h<sizeof(double); ++h,++p) putc((int)*p,data);
                ++n_total;
                ++ns[i];
                }
            data_close(&d);
            }
        muste_fclose(data);

        x_space=(double *)muste_malloc(n_total*sizeof(double));
        if (x_space==NULL) { not_enough_memory(); return(-1); }
        samp=(int *)muste_malloc(n_total*sizeof(int));
        if (samp==NULL) { not_enough_memory(); return(-1); }
        rank=(float *)muste_malloc(n_total*sizeof(float));
        if (rank==NULL) { not_enough_memory(); return(-1); }

        data=muste_fopen(survoxxx,"rb");
        k=0;
        for (i=0; i<n_sample; ++i)
            {
            sprintf(sbuf,"\nSample %d",i+1); sur_print(sbuf);
            sample[i]=k;
            for (j=0; j<ns[i]; ++j)
                {
                p=(char *)&x_space[k];
                for (h=0; h<sizeof(double); ++h,++p) *p=(char)getc(data);
                samp[k]=i;
                ++k;
                }
            }
        muste_fclose(data);
        remove(survoxxx);
/*      sprintf(sbuf,"DEL %s",survoxxx);
        system(sbuf);
*/
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(1);
        }

/***************
static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }
*******************/
/* comp2.c 28.6.1986/SM (16.7.1990)
*/


static int *perm;

static int sort_joint_sample()
        {
        int i;
        int h,k,g;
        char ind;
        double y;
        float a;

/*      Rprintf("\nSorting the joint sample: ");         */
        h=n_total;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind='1';
                for (k=0; k<n_total-h; ++k)
                    {
                    if (x_space[k]>x_space[k+h])
                        {
                        y=x_space[k]; x_space[k]=x_space[k+h]; x_space[k+h]=y;
                        i=samp[k]; samp[k]=samp[k+h]; samp[k+h]=i;
                        ind='0';
                        }
                    }
                if (ind=='1') break;
                }
            }

        k=0;
        while (k<n_total)
            {
            y=x_space[k]; h=1;
            while (k+h<n_total && y==x_space[k+h]) ++h;
            --h;
            a=(float)(k+1+h/2.0);
            for (g=k; g<=k+h; ++g) rank[g]=a;
            k+=h+1;
            }
/*
  Rprintf("\nRanks:");
  for (k=0; k<n_total; ++k) Rprintf(" %g",rank[k]);
  getch();
*/
        return(1);
        }

static int Mann_Whitney(int test)
// int test;   /* 0=t 1=all */
        {
        extern char *spois();
        int i;
        int j,k;
        float rsum1,rsum2;
        double sum1,sum2;
        double u0,v0;
        int d,d1,r,ii;
        int u,u1,r1;
        double s1;
        float s2;
        char rivi[LLENGTH], x[LLENGTH];
        char strp[32];
        double dn1,dn2;

        if (n_sample!=2) { only_two_samples_permitted(); return(-1); }
        i=sort_joint_sample(); if (i<0) return(-1);
        output_open(eout);
        clear_screen();
        basic_statistics();
        if (!test) return(-1);
        rsum1=rsum2=0.0;
        sum1=sum2=0.0;
        for (k=0; k<n_total; ++k)
            {
            if (samp[k]) { rsum2+=rank[k]; sum2+=x_space[k]; }
            else         { rsum1+=rank[k]; sum1+=x_space[k]; }
            }
        init_rivi(rivi);
        set(rivi,"Sum of ranks (R)",0);
        fconv((double)rsum1,"",x); set(rivi,x,p1);
        fconv((double)rsum2,"",x); set(rivi,x,p2);
        eoutput(rivi);

        u0=(double)rsum1-(double)ns[0]*((double)ns[0]+1)/2.0;
        v0=(double)ns[0]*(double)ns[1]-u0;
        init_rivi(rivi);
        set(rivi,"Mann-Whitney (U)",0);
        fconv((double)u0,"",x); set(rivi,x,p1);
        fconv((double)v0,"",x); set(rivi,x,p2);
        eoutput(rivi);

        dn1=ns[0]; dn2=ns[1];
        s1=(u0-dn1*dn2/2.0)/muste_sqrt((double)(dn1*dn2*(dn1+dn2+1))/12.0);
        fnconv(muste_st_norm(s1,(double)0.0),accuracy,strp);
        sprintf(rivi,"(P=%s one-sided Mann-Whitney, normal approximation)",
                        spois(strp));
        eoutput(rivi);

        if (!simumax)
            {
            output_close(eout);
            return(1);
            }

        sprintf(rivi,"Critical levels by simulation:");
        eoutput(rivi);
        init_rivi(rivi);
        set(rivi,"Mean    R or U",15);
        eoutput(rivi);
        sur_print("\n\n\n\nTo interrupt simulation press any key!\n");
        PR_UP; PR_UP; PR_UP; PR_UP;

        perm=(int *)muste_malloc(n_total*sizeof(int));
        if (perm==NULL) { not_enough_memory(); return(-1); }

        d1=disp_gap; d=0; u=0L; u1=0L; r1=0L;  // 3.7.2011/SM

        while (1)
            {
            for (k=0; k<n_total; ++k) perm[k]=k;
            s1=0.0; s2=0.0;
            for (j=0; j<ns[0]; ++j)
                {
                r=j+(int)((n_total-j)*RND);
                ii=perm[r];
                s1+=x_space[ii]; s2+=rank[ii]; perm[r]=perm[j];
                }
            ++u; ++d;
            if (s1<=sum1) ++u1;
            if (s2<=rsum1) ++r1;
            if (d==d1)
                {
                d=0;
                s1=(double)u1/(double)u;
                s2=(float)r1/(float)u;
                sprintf(sbuf,"Critical level %7.5f %.5f  N=%d",(double)s1,s2,u); sur_print(sbuf);
                sprintf(sbuf,"\nStandard error %7.5f %.5f",muste_sqrt((double)(s1*(1.0-s1)/(double)u))
                                    ,muste_sqrt((double)(s2*(1.0-s2)/(double)u))); sur_print(sbuf);
                sur_print("\n"); PR_UP; PR_UP;
                CURSOR_OFF; headline(""); CURSOR_ON;
                if (u>=simumax) break;
                if (sur_kbhit())
                    {
                    i=sur_getch(); break;
                    }
                }
            }

        PR_UP;
        sprintf(rivi,"Critical level %7.5f %.5f  N=%d",(double)s1,s2,u);
        eoutput(rivi);
        sprintf(rivi,"Standard error %7.5f %.5f",muste_sqrt((double)(s1*(1.0-s1)/(double)u))
                            ,muste_sqrt((double)(s2*(1.0-s2)/(double)u)));
        eoutput(rivi);

        output_close(eout);
        return(1);
        }

static int only_two_samples_permitted()
        {
        sur_print("\nOnly two samples permitted!");
        WAIT; return(1);
        }

/* comp3.c 28.6.1986/SM (29.6.1986)
*/



static int clear_screen()
        {
        CLS; LOCATE(2,1); return(1);
        }

static int basic_statistics()
        {
        extern double muste_cdf_t();
        extern char *spois();   /* comprc.c */
        char rivi[LLENGTH];
        char x[LLENGTH];
        int k;
        double y,sum1,sum2,ss1,ss2;
        double t,ns1,ns2;
        char strp[32];

        init_rivi(rivi);
        set(rivi,"Independent samples",0);
        set(rivi,word[1],p1);
        set(rivi,word[2],p2);
        eoutput(rivi);

        init_rivi(rivi);
        set(rivi,"Sample size",0);
        sprintf(x,"%d",ns[0]); set(rivi,x,p1);
        sprintf(x,"%d",ns[1]); set(rivi,x,p2);
        eoutput(rivi);

        sum1=sum2=ss1=ss2=0.0;
        for (k=0; k<n_total; ++k)
            {
            y=x_space[k];
            if (samp[k]) { sum2+=y; ss2+=y*y; }
            else         { sum1+=y; ss1+=y*y; }
            }
        init_rivi(rivi);
        set(rivi,"Mean",0);
        fnconv(sum1/ns[0],accuracy+2,x); set(rivi,x,p1);
        fnconv(sum2/ns[1],accuracy+2,x); set(rivi,x,p2);
        eoutput(rivi);
        init_rivi(rivi);
        set(rivi,"Standard deviation",0);
        fnconv(muste_sqrt((ss1-sum1*sum1/ns[0])/(ns[0]-1)),accuracy+2,x);
        set(rivi,x,p1);
        fnconv(muste_sqrt((ss2-sum2*sum2/ns[1])/(ns[1]-1)),accuracy+2,x);
        set(rivi,x,p2);
        eoutput(rivi);

        ns1=(double)ns[0]; ns2=(double)ns[1];
        t=(sum1/ns1-sum2/ns2)*muste_sqrt(ns1*ns2*(ns1+ns2-2.0)/(ns1+ns2))/
          muste_sqrt((ss1-sum1*sum1/ns1)+(ss2-sum2*sum2/ns2));
        init_rivi(rivi);
        fnconv(t,accuracy-1,x);
// pitäisi olla
        fnconv(muste_cdf_t(t,(double)(ns[0]+ns[1]-2)),accuracy,strp);
// nyt oli muutettava järjestystä
//      fnconv(muste_cdf_t((double)(ns[0]+ns[1]-2),t),accuracy,strp);
        sprintf(rivi,"Student's t=%s df=%d (P=%s one-sided test)",
                           spois(x),ns[0]+ns[1]-2,spois(strp));
        eoutput(rivi);
        return(1);
        }

static int init_rivi(char *x)
        {
        strncpy(x,space,c2); x[c2]=EOS; return(1);
        }

static int set(char *x,char *s,int pos)
        {
        char *p;

        p=x+pos;
        while (*s && *p) *p++=*s++;
        return(1);
        }

static int eoutput(char *rivi)
        {
        output_line(rivi,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

/* comp4.c 27.5.2001/SM (27.5.2001)
*/

static double dp_run;

static int Wald_Wolfowitz()
    {
    int i,nrun;


    if (n_sample!=2) { only_two_samples_permitted(); return(-1); }

//  Rprintf("\nsamp:");
//  for (i=0; i<n_total; ++i) Rprintf("%d ",samp[i]); getch();

    i=sort_joint_sample(); if (i<0) return(-1);

//  Rprintf("\nsamp:");
//  for (i=0; i<n_total; ++i) Rprintf("%d ",samp[i]); getch();

    nrun=count_runs();
// Rprintf("\nnrun=%d",nrun); getch();
    ww_test(n_total,ns[1],nrun);

    output_open(eout);
    basic_statistics();

    sprintf(sbuf,"Wald-Wolfowitz test: #runs=%d P=%g",nrun,dp_run);
    eoutput(sbuf);

    return(1);
    }

static int count_runs()
    {
    int i,nr,r1,r2;

    nr=1; r1=samp[0];
    for (i=1; i<n_total; ++i)
        {
        r2=samp[i];
        if (r2==r1) continue;
        r1=r2; ++nr;
        }
    return(nr);
    }

static int ww_test(int n,int n1,int nr)
    {
    int i,k;
    int pariton;
    double a,pr;
    int n0;

//  nr=run0+run1;
    if (n>1000) { run_test2(n,n1,nr); return(1); }  // normal approx.
    n0=n-n1;
    a=1.0/bin_coeff(n,n1);
    dp_run=2*a; pariton=0;
    for (i=3; i<=nr; ++i)
        {
        k=i/2; pariton=1-pariton;
        if (!pariton)
          a*=((double)n0/(double)(k-1)-1.0)*((double)n1/(double)(k-1)-1.0);
        if (pariton) pr=((double)(n0+n1)/(double)k-2.0)*a;
        else pr=2*a;
        dp_run+=pr;
// Rprintf("\ni=%d pr=%g sum=%g",i,pr,dp_run); getch();
        }
    return(1);
    }

// P(N,X,S)|=if(X=1)then((N-S+1)/C(N,S))else(P2(N,X,S))
// P2(N,X,S)|=(S-X+1)*(N-S-X+2)/X/(X-1)*P(N,X-1,S)
// n=N X=run1 S=n1

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

static int run_test2(int n,int n1,int nr) // normal approximation
    {
    double mean,s,r;
    double u,u0,u1;

    r=nr;
    u=n; u0=n-n1; u1=n1;
    mean=2.0*u0*u1/u+1.0;
    s=muste_sqrt(2.0*u0*u1*(2.0*u0*u1-u)/(u*u*(u-1.0)));
//  Rprintf("\nr=%g mean=%g s=%g",r,mean,s); getch();
    dp_run=muste_st_norm((r-mean)/s,0.0);

    return(1);
    }


/* qq.c 26.3.2005 (27.3.2005)

*/

// static double *sd;
static double *qp1,*qp2;
static double *ss2;
static double *s1,*s2;
static char var1[9],var2[9];

static int make_qq_data()
    {
    int i,j;
//  int msn;
    int n1,n2;
    char rivi[LLENGTH];

    if (n_sample!=2)
        {
        sur_print("\n2 samples required!"); WAIT; return(-1);
        }

    n1=ns[0]; n2=ns[1]; s1=x_space; s2=x_space+n1;
    if (ns[1]<ns[0])
        { n1=ns[1]; n2=ns[0]; s2=x_space; s1=x_space+n2; }

    qq_sort(s1,n1);
    qq_sort(s2,n2);
/*********************************
printf("\n");
for (i=0; i<n1; ++i) Rprintf("%g ",s1[i]);
printf("\n");
for (i=0; i<n2; ++i) Rprintf("%g ",s2[i]);
getch();
printf("\nn1=%d n2=%d",n1,n2); getch();
***********************************/
    if (n1<n2)
        {
        qp1=(double *)muste_malloc(n1*sizeof(double));
        if (qp1==NULL) { not_enough_memory(); return(-1); }
        qp2=(double *)muste_malloc(n2*sizeof(double));
        if (qp2==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<n1; ++i) qp1[i]=((double)i+0.625)/((double)n1+0.25);
        for (i=0; i<n2; ++i) qp2[i]=((double)i+0.625)/((double)n2+0.25);
        ss2=(double *)muste_malloc(n1*sizeof(double));
        if (ss2==NULL) { not_enough_memory(); return(-1); }
        j=0;
        for (i=0; i<n1; ++i)
            {
            while (qp2[j]<qp1[i]) ++j;
   ss2[i]=s2[j-1]+(s2[j]-s2[j-1])*(qp1[i]-qp2[j-1])/(qp2[j]-qp2[j-1]);
// Rprintf("\nss2=%g",ss2[i]); getch();


            }
        }
    else ss2=s2;

    qq_create_data("_QQ",s1,ss2,n1);

    output_open(eout);
    eoutput("..........");
    sprintf(rivi,"Data for Q-Q plot %s vs. %s saved in _QQ.SVO",
                       word[1],word[2]);
    eoutput(rivi);
    eoutput("HEADER=Quantile-Quantile_plot MODE=PS");
    sprintf(rivi,"GPLOT _QQ,%s,%s / TREND=[line_width(0.5)][RED],O POINT=11",var1,var2);
    eoutput(rivi);
    eoutput("..........");

    output_close(eout);

    return(1);
    }

// extern SURVO_DATA d;
static int qq_create_data(char *name,double *x,double *y,int n)
    {
    int j;
    char nimi[LNAME];
    char *varname[2];
    int varlen[2];
    char type[]="8A-    ";
    char *vartype[2];
    char **fitext;
    char *privi[1];
    int fitextn,fitextlen;
    char tt[LLENGTH];

    sprintf(nimi,"%s%s",edisk,name);
    tee_muuttujanimet(var1,var2);
    varname[0]=var1; varname[1]=var2;
    varlen[0]=varlen[1]=8;
    vartype[0]=vartype[1]=type;

    fitextn=1;
    fitextlen=c2;
    sprintf(tt,"Data for Q-Q plot: %s vs. %s",
                       word[1],word[2]);
    privi[0]=tt;
    fitext=privi;

    fi_create(nimi,16,2,2,(int)n,64,8,
               fitextn,fitextlen,fitext,varname,varlen,vartype);
    data_open(nimi,&d);
    for (j=1; j<=n; ++j)
        {
        data_save(&d,(int)(j),0,x[j-1]);
        data_save(&d,(int)(j),1,y[j-1]);
        }
    data_close(&d);
    return(1);
    }

static int tee_muuttujanimet(char *var1,char *var2)
    {
    int i;
    char *p;
    char x[LNAME];

    p=strchr(word[1],'(');
    if (p==NULL) strcpy(var1,word[1]);
    else
        {
        strcpy(x,p+1);
        i=strlen(x)-1; x[i]=EOS;
        strcpy(var1,x);
        }
    p=strchr(word[2],'(');
    if (p==NULL) strcpy(var2,word[2]);
    else
        {
        strcpy(x,p+1);
        i=strlen(x)-1; x[i]=EOS;
        strcpy(var2,x);
        }
// Rprintf("\nvar: %s %s|",var1,var2); getch();

    if (strcmp(var1,var2)==0)  // tällöin datanimet eroavat
        {
        strcpy(x,word[1]);
        p=strchr(x,'(');
        if (p!=NULL) *p=EOS;
        strcpy(var1,x);

        strcpy(x,word[2]);
        p=strchr(x,'(');
        if (p!=NULL) *p=EOS;
        strcpy(var2,x);
        }
    return(1);
    }

static int qq_sort(double *x,int n)
    {
    int h,k,ind;
    double y;

    h=n;
    while (h>1)
        {
        h/=2;
        while (1)
            {
            ind=1;
            for (k=0; k<n-h; ++k)
                {
                if (x[k]>x[k+h])
                    {
                    y=x[k]; x[k]=x[k+h]; x[k+h]=y;
                    ind=0;
                    }
                }
            if (ind==1) break;
            }
        }
    return(1);
    }

/* compsmi.c 29.6.1986/SM (9.5.1990) (17.4.1996)
*/
static int *uu;
static double *x2; // 1.7.2011/SM

static int Smirnov()
        {
        int i;
        int k,h,j;
        int j1,j2,k1,k2;
        double a,b,diff,ad0,ad1,ad2;
        int r,d,d1;
        int t,u,u1,u2;
        double ae1,ae2,as0,as1,as2,f1,f2,s,s1,s2;
        char rivi[LLENGTH];  // x[LLENGTH];
        int ind[2];
        extern double probks();

        i=sort_joint_sample(); if (i<0) return(-1);
        output_open(eout);
        clear_screen();
        basic_statistics();

        x2=(double *)muste_malloc(n_total*sizeof(double));
        if (x2==NULL) { not_enough_memory(); return(-1); }
        perm=(int *)muste_malloc(n_total*sizeof(int));
        if (perm==NULL) { not_enough_memory(); return(-1); }
        uu=(int *)muste_malloc(n_total*sizeof(int));
        if (uu==NULL) { not_enough_memory(); return(-1); }

        ind[0]=ind[1]=0;
        for (k=0; k<n_total; ++k)
            {
            h=samp[k];
            x2[sample[h]+ind[h]]=x_space[k];
            ++ind[h];
            }
/*
   for (i=0; i<2; ++i)
        {
        Rprintf("\notos %d: ",i+1);
        for (k=0; k<ns[i]; ++k) Rprintf("%g ",x2[sample[i]+k]);
        }
   getch();
*/

        j1=j2=0; ad0=ad1=ad2=0.0;
        while (j1<ns[0] && j2<ns[1])
            {
            a=x2[sample[0]+j1]; if (a>=x2[sample[1]+j2]) a=x2[sample[1]+j2];
            k1=0;
            while (j1+k1<ns[0] && x2[sample[0]+j1+k1]<=a) ++k1;  /* järj. vaihd. 17.4.1996 */
            k2=0;
            while (j2+k2<ns[1] && x2[sample[1]+j2+k2]<=a) ++k2;
            j1+=k1; j2+=k2;
            a=(double)j1/(double)ns[0];
            b=(double)j2/(double)ns[1];
            diff=a-b;
            if (muste_fabs(diff)>ad0) ad0=muste_fabs(diff);
            if (diff>ad1) ad1=diff;
            if (-diff>ad2) ad2=-diff;
            }

        a=muste_sqrt((double)(ns[0]*ns[1])/(double)(ns[0]+ns[1]));
        a=probks((a+0.12+0.11/a)*ad0);
        sprintf(rivi,"Kolmogorov-Smirnov test D=%7.5f (P=%.5f) D(+)=%7.5f D(-)=%7.5f",
                        ad0,a,ad1,ad2);
        eoutput(rivi);

        if (!simumax)
            {
            output_close(eout);
            return(1);
            }

        sprintf(rivi,"Critical levels by simulation:");
        eoutput(rivi);

        sur_print("\n\n\n\nTo interrupt simulation, press any key!\n");
        PR_UP; PR_UP; PR_UP; PR_UP;

        d1=disp_gap; d=0; t=u=u1=u2=0L;

        while (1)
            {
            for (k=0; k<n_total; ++k) { perm[k]=k; uu[k]=1; }
            for (j=0; j<ns[0]; ++j)
                {
                r=j+(int)((n_total-j)*RND);
                uu[perm[r]]=0; perm[r]=perm[j];
                }
            f1=f2=as0=as1=as2=0.0; ae1=1.0/(double)ns[0]; ae2=1.0/(double)ns[1];
            for (k=0; k<n_total; ++k)
                {
                if (uu[k]) f2+=ae2; else f1+=ae1;
                diff=f1-f2; if (muste_fabs(diff)>as0) as0=muste_fabs(diff);
                if (diff>as1) as1=diff;
                if (-diff>as2) as2=-diff;
                }
            ++t;
            if (as0>=ad0) ++u;
            if (as1>=ad1) ++u1;
            if (as2>=ad2) ++u2;
            ++d;

            if (d==d1)
                {
                d=0;
                s=(double)u/(double)t;
                s1=(double)u1/(double)t;
                s2=(double)u2/(double)t;
                sprintf(sbuf,"Critical level            %7.5f %12.5f %12.5f  N=%d",s,s1,s2,t);
                sur_print(sbuf);
                sprintf(sbuf,"\nStandard error            %7.5f %12.5f %12.5f",
                                muste_sqrt(s*(1.0-s)/(double)t),
                                muste_sqrt(s1*(1.0-s1)/(double)t),
                                muste_sqrt(s2*(1.0-s2)/(double)t)); sur_print(sbuf);

                sur_print("\n"); PR_UP; PR_UP;
                CURSOR_OFF; headline(""); CURSOR_ON;
                if (t>simumax) break;
                if (sur_kbhit())
                    {
                    i=sur_getch(); break;
                    }
                }
            }

        PR_UP;
        sprintf(rivi,"Critical level            %7.5f %12.5f %12.5f  N=%d",s,s1,s2,t);
        eoutput(rivi);
        sprintf(rivi,"Standard error            %7.5f %12.5f %12.5f",
                        muste_sqrt(s*(1.0-s)/(double)u),
                        muste_sqrt(s1*(1.0-s1)/(double)u),
                        muste_sqrt(s2*(1.0-s2)/(double)u));
        eoutput(rivi);
        output_close(eout);
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
            term=fac*muste_exp((double)(a2*j*j));
            sum+=term;
            if (muste_fabs(term)<=EPS1*termbf || muste_fabs(term)<EPS2*sum)
                return(sum);
            fac=-fac;
            termbf=muste_fabs(term);
            }
        return(0.0);
        }

/* compkw.c 1.7.1986/SM (16.7.1990)
*/

#define MAX_N 10

static int Kruskal_Wallis(int test)
// int test;  /* 0=F 1=all */
        {
        extern double muste_cdf_chi2();
        extern char *spois();
        int i;
        int h,j,k,jy;
        int d,d1; // r,ii;
        int u,u1;
        char rivi[LLENGTH], x[LLENGTH];
        double ri[MAX_N];
        double a,b,s1,s2,t,t2;
        double dn,kw;
        char strp[32];

        output_open(eout);
        clear_screen();
        anova();
        if (!test) return(1);
        i=sort_joint_sample(); if (i<0) return(-1);

        s2=0.0;
        for (i=0; i<n_sample; ++i) ri[i]=0.0;
        for (k=0; k<n_total; ++k)
            {
            a=rank[k]; s2+=a*a;
            j=samp[k]; ri[j]+=a;
            }
        dn=(double)n_total;
        a=dn*(dn+1.0)*(dn+1.0)/4.0; s2=(s2-a)/(dn-1.0);
        t=0.0; for (i=0; i<n_sample; ++i) t+=ri[i]*ri[i]/(double)ns[i];
        kw=(t-a)/s2;
        fnconv(kw,accuracy-1,x);
        sprintf(rivi,"Kruskal-Wallis test for equality of means=%s",spois(x));
        eoutput(rivi);
        fnconv(1.0-muste_cdf_chi2(kw,(double)(n_sample-1),1e-7),accuracy,strp);
        sprintf(rivi,"   P=%s df=%d (Chi^2-approximation)",
                        spois(strp),n_sample-1);
        eoutput(rivi);

        if (!simumax)
            {
            output_close(eout);
            return(1);
            }

        sprintf(rivi,"Critical level of Kruskal-Wallis test by simulation:");
        eoutput(rivi);
        sur_print("\n\n\n\nTo interrupt simulation press any key!\n");
        PR_UP; PR_UP; PR_UP; PR_UP;

        perm=(int *)muste_malloc(n_total*sizeof(int));
        if (perm==NULL) { not_enough_memory(); return(-1); }

        d1=disp_gap; d=0; u=0L; u1=0L;

        while (1)
            {
            for (k=0; k<n_total; ++k) perm[k]=k;
            t2=0.0; jy=0;
            for (h=0; h<n_sample; ++h)
                {
                b=0.0;
                for (j=jy; j<jy+ns[h]; ++j)
                    {
                    r=j+(int)((n_total-j)*RND);
                    b+=rank[perm[r]]; perm[r]=perm[j];
                    }
                jy+=ns[h]; t2+=b*b/(double)ns[h];
                }
            ++u; ++d;
            if (t2>=t) ++u1;
            if (d==d1)
                {
                d=0;
                s1=(double)u1/(double)u;
                sprintf(sbuf,"Critical level %7.5f  N=%d",s1,u); sur_print(sbuf);
                sprintf(sbuf,"\nStandard error %7.5f",muste_sqrt((s1*(1.0-s1)/(double)u)));
                sur_print(sbuf);
                sur_print("\n"); PR_UP; PR_UP;
                CURSOR_OFF; headline(""); CURSOR_ON;
                if (u>=simumax) break;
                if (sur_kbhit())
                    {
                    i=sur_getch(); break;
                    }
                }
            }

        PR_UP;
        sprintf(rivi,"Critical level %7.5f  N=%d",s1,u);
        eoutput(rivi);
        sprintf(rivi,"Standard error %7.5f",muste_sqrt((s1*(1.0-s1)/(double)u)));
        eoutput(rivi);

        output_close(eout);
        return(1);
        }

static int anova()
        {
        extern double muste_cdf_chi2();
        extern double muste_cdf_f();
        extern char *spois();
        double p[MAX_N],q[MAX_N];
        double a,b0,c,y;
        double t,u;
        int h,i;
        char rivi[LLENGTH],x[LLENGTH];
        char strp[32];

        a=b0=0.0;
        for (h=0; h<n_sample; ++h)
            {
            p[h]=q[h]=0.0;
            for (i=0; i<ns[h]; ++i)
                {
                y=x_space[sample[h]+i];
                p[h]+=y; q[h]+=y*y;
                }
            a+=p[h]; p[h]/=(double)ns[h];
            q[h]=(q[h]-ns[h]*p[h]*p[h]);
            b0+=q[h];
            q[h]/=(double)(ns[h]-1);
            }
        a/=(double)n_total;
        b0/=(double)(n_total-n_sample);
        c=0.0; for (h=0; h<n_sample; ++h) c+=ns[h]*(p[h]-a)*(p[h]-a);
        c/=(double)(n_sample-1);

        eoutput("Comparing independent samples:");
        eoutput("Sample               N    Mean           Std.dev.");
        for (h=0; h<n_sample; ++h)
            {
            init_rivi(rivi);
            set(rivi,word[h+1],0);
            sprintf(x,"%4d",ns[h]); set(rivi,x,18);
            fnconv(p[h],accuracy+2,x); set(rivi,x,25);
            fnconv(muste_sqrt(q[h]),accuracy+2,x); set(rivi,x,40);
            eoutput(rivi);
            }

        t=u=0.0; for (h=0; h<n_sample; ++h)
                     { t+=(ns[h]-1)*muste_log(q[h]/b0); u+=1.0/(double)(ns[h]-1); }
        a=-1.0/(1.0+1.0/(3.0*(n_sample-1))*(u+1.0/(n_total-n_sample)))*t;
        fnconv(1.0-muste_cdf_chi2(a,(double)(n_sample-1),1e-7),accuracy,strp);
        fnconv(a,accuracy-1,x);
        sprintf(rivi,"Bartlett's test for equality of standard deviations=%s",spois(x));
        eoutput(rivi);
        sprintf(rivi,"   P=%s df=%d (Chi^2-approximation)",spois(strp),n_sample-1);
        eoutput(rivi);
        a=c/b0;
        fnconv(a,accuracy-1,x);
        fnconv(1.0-muste_cdf_f(a,(double)(n_sample-1),(double)(n_total-n_sample),1e-7),accuracy,strp);
        sprintf(rivi,"F test for equality of means=%s",spois(x));
        eoutput(rivi);
        sprintf(rivi,"   P=%s df1=%d df2=%d",spois(strp),n_sample-1,n_total-n_sample);
        eoutput(rivi);
        return(1);
        }

/* compa.c 1.7.1986/SM (3.7.1986) (26.10.90) (20.2.1996)
*/

static double *diff,*y;    /* siirretty globaaleiksi 9.5.90 */
static int *order;
static float *srank;

static int Wilcoxon()
        {
        extern char *spois();
        extern double sgn();
        int i;
        int j,k,n;
        double a,ar,ar2;
        int d,d1;
        int u,u1,r1;
        double s1;
        float s2;
        char rivi[LLENGTH], x[LLENGTH];
        int n_ties;
        double td,ud;
        float ts,us;
        char strp[32];

        if (n_sample!=2) { only_two_samples_permitted(); return(-1); }
        if (ns[0]!=ns[1])
            {
            sur_print("\nSample sizes unequal! Paired comparison impossible!");
            WAIT; return(-1);
            }
        n=ns[0];

        diff=(double *)muste_malloc(n*sizeof(double));
        if (diff==NULL) { not_enough_memory(); return(-1); }
        y=(double *)muste_malloc(n*sizeof(double));
        if (y==NULL) { not_enough_memory(); return(-1); }
        order=(int *)muste_malloc(n*sizeof(int));
        if (order==NULL) { not_enough_memory(); return(-1); }
 /*     rank=(float *)muste_malloc(n*sizeof(float));
        if (rank==NULL) { not_enough_memory(); return(-1); }
   varattu n_total paikkaa comp1.c:ssä
 */
        srank=(float *)muste_malloc(n*sizeof(float));
        if (srank==NULL) { not_enough_memory(); return(-1); }
        for (k=0; k<n; ++k)
            {
            diff[k]=x_space[sample[1]+k]-x_space[sample[0]+k];
            y[k]=muste_fabs(diff[k]);
            order[k]=k;
            }

        rank_sort(n,y,order,rank);

        n_ties=0;
        for (k=0; k<n; ++k)
            {
            j=order[k]; srank[j]=sgn(diff[j])*rank[k];
            if (!srank[j]) ++n_ties;
            }

        for (k=0; k<n; ++k)
            {
            float a;
            a=srank[k]; if (a) srank[k]=a-sgn(a)*n_ties;
            }
        output_open(eout);
        clear_screen();
        p1=24; p2=40;
        i=paired_statistics(); if (i<0) return(-1);

        ar=ar2=0.0;
        for (k=0; k<n; ++k)
            {
            a=srank[k];
            ar+=a; ar2+=a*a;
            }
        a=ar/muste_sqrt(ar2);
        fnconv(a,accuracy-1,x);
        fnconv(muste_st_norm(a,(double)0.0),accuracy,strp);
        sprintf(rivi,"Wilcoxon signed ranks test=%s (P=%s normal approximation)"
                                        ,spois(x),spois(strp));
        eoutput(rivi);

        if (!simumax)
            {
            output_close(eout);
            return(1);
            }

        sprintf(rivi,"Critical levels by simulation:");
        eoutput(rivi);
        init_rivi(rivi);
        set(rivi,"Differences Signed rank",15);
        eoutput(rivi);
        sur_print("\n\n\n\nTo interrupt simulation press any key!\n");
        PR_UP; PR_UP; PR_UP; PR_UP;

        td=0.0; ts=0.0;
        for (k=0; k<n; ++k)
            {
            if (diff[k]>0.0) { td+=diff[k]; ts+=srank[k]; }
            diff[k]=muste_fabs(diff[k]); srank[k]=muste_fabs(srank[k]);
            }

        d1=disp_gap; d=0; u=0L; u1=0L; r1=0L;

        while (1)
            {
            ud=0.0; us=0.0;
            for (k=0; k<n; ++k)
                {
                if (RND<=0.5)
                    {
                    ud+=diff[k]; us+=srank[k];
                    }
                }

            ++u; ++d;
            if (ud<=td) ++u1;
            if (us<=ts) ++r1;
            if (d==d1)
                {
                d=0;
                s1=(double)u1/(double)u;
                s2=(float)r1/(float)u;
                sprintf(sbuf,"Critical level %7.5f %11.5f  N=%d",(double)s1,s2,u); sur_print(sbuf);
                sprintf(sbuf,"\nStandard error %7.5f %11.5f",muste_sqrt((double)(s1*(1.0-s1)/(double)u))
                                    ,muste_sqrt((double)(s2*(1.0-s2)/(double)u))); sur_print(sbuf);
                sur_print("\n"); PR_UP; PR_UP;
                CURSOR_OFF; headline(""); CURSOR_ON;
                if (u>=simumax) break;
                if (sur_kbhit())
                    {
                    i=sur_getch(); break;
                    }
                }
            }

        PR_UP;
        sprintf(rivi,"Critical level %7.5f %11.5f  N=%d",(double)s1,s2,u);
        eoutput(rivi);
        sprintf(rivi,"Standard error %7.5f %11.5f",muste_sqrt((double)(s1*(1.0-s1)/(double)u))
                            ,muste_sqrt((double)(s2*(1.0-s2)/(double)u)));
        eoutput(rivi);

        output_close(eout);
        return(1);
        }

static int rank_sort(int n,double *y,int *order,float *rank)
// unsigned int n;
// double *y;
// unsigned int *order;
// float *rank;
        {
        int h,k,g;
        char ind;
        double a;

        h=n;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind='1';
                for (k=0; k<n-h; ++k)
                    {
                    if (y[k]>y[k+h])
                        {
                        a=y[k]; y[k]=y[k+h]; y[k+h]=a;
                        g=order[k]; order[k]=order[k+h]; order[k+h]=g;
                        ind='0';
                        }
                    }
                if (ind=='1') break;
                }
            }

        k=0;
        while (k<n)
            {
            a=y[k]; h=1;
            while (k+h<n && a==y[k+h]) ++h;  /* 20.2.1996 cond's exchanged */
            --h;
            a=k+1+(double)h/2.0;
            for (g=k; g<=k+h; ++g) rank[g]=(float)a;
            k+=h+1;
            }
        return(1);
        }

static int paired_statistics()
        {
        extern double muste_cdf_t();
        extern char *spois();
        int k,n;
        char rivi[LLENGTH], xx[LLENGTH];
        double x,y,d,mx,my,md,sx,sy,sd;
        double t;
        char strp[32];

        n=ns[0];
        eoutput("Paired comparisons:");
        sprintf(rivi,"Samples: N=%d            %-15.14s %-15.14s Difference",
                               n,word[1],word[2]);
        eoutput(rivi);

        mx=my=md=sx=sy=sd=0.0;
        for (k=0; k<n; ++k)
            {
            x=x_space[sample[0]+k];
            y=x_space[sample[1]+k];
            d=y-x;
            mx+=x; my+=y; md+=d;
            sx+=x*x; sy+=y*y; sd+=d*d;
            }
        mx/=(double)n; my/=(double)n; md/=(double)n;
        sx=muste_sqrt((sx-n*mx*mx)/(double)(n-1));
        sy=muste_sqrt((sy-n*my*my)/(double)(n-1));
        sd=muste_sqrt((sd-n*md*md)/(double)(n-1));
        init_rivi(rivi);
        set(rivi,"Mean",0);
        fnconv(mx,accuracy+2,xx); set(rivi,xx,p1);
        fnconv(my,accuracy+2,xx); set(rivi,xx,p2);
        fnconv(md,accuracy+2,xx); set(rivi,xx,2*p2-p1);
        eoutput(rivi);
        set(rivi,"Standard deviation",0);
        fnconv(sx,accuracy+2,xx); set(rivi,xx,p1);
        fnconv(sy,accuracy+2,xx); set(rivi,xx,p2);
        fnconv(sd,accuracy+2,xx); set(rivi,xx,2*p2-p1);
        eoutput(rivi);

        if (mx==my && sd==0.0)
            {
            sur_print("\nThe samples are identical!");
            WAIT; return(-1);
            }
        t=md*muste_sqrt((double)n)/sd;
        fnconv(t,accuracy-1,xx);
        fnconv(muste_cdf_t(t,(double)(n-1)),accuracy,strp);
        sprintf(rivi,"Paired t=%s (P=%s one-sided t test df=%d)"
                       ,spois(xx),spois(strp),n-1);
        eoutput(rivi);
        return(1);
        }

static double sgn(double x)
        {
        if (x>0.0) return((double)1.0);
        if (x==0.0) return((double)0.0);
        return((double)-1.0);
        }

/* comprc.c 2.7.1986/SM (26.10.1990)
*/
static float *xrank,*yrank;

static int rank_corr()
        {
        extern char *spois();
        int i;
        int j,k,n,ii;
        int d,d1;
        int u,rr,rs;
        char rivi[LLENGTH], x[LLENGTH];
        double ar,sr1,sr2,rho;
        double mx,my,sx,sy,sxy;
        int nc,nd; double tau,ptau;  /* Kendall */
        double a;
        char strp[32];
        double ncd;
        double tr1,tr2,s1,s2;
        double xx,yy;

        if (n_sample!=2) { only_two_samples_permitted(); return(-1); }
        if (ns[0]!=ns[1])
            {
            sur_print("\nSample sizes unequal! Paired comparison impossible!");
            WAIT; return(-1);
            }
        n=ns[0];

        y=(double *)muste_malloc(n*sizeof(double));
        if (y==NULL) { not_enough_memory(); return(-1); }
/*      rank=(float *)muste_malloc(n*sizeof(float));
        if (rank==NULL) { not_enough_memory(); return(-1); }
      varattu n_total paikkaa comp1.c:ssä
*/
        xrank=(float *)muste_malloc(n*sizeof(float));
        if (xrank==NULL) { not_enough_memory(); return(-1); }
        yrank=(float *)muste_malloc(n*sizeof(float));
        if (yrank==NULL) { not_enough_memory(); return(-1); }
        order=(int *)muste_malloc(n*sizeof(int));
        if (order==NULL) { not_enough_memory(); return(-1); }

        for (k=0; k<n; ++k)
            {
            y[k]=x_space[sample[0]+k];
            order[k]=k;
            }
        rank_sort(n,y,order,rank);
        for (k=0; k<n; ++k) xrank[order[k]]=rank[k];
/* Rprintf("\nxranks:");
   for (k=0; k<n; ++k) Rprintf(" %g",xrank[k]);
*/
        for (k=0; k<n; ++k)
            {
            y[k]=x_space[sample[1]+k];
            order[k]=k;
            }
        rank_sort(n,y,order,rank);
        for (k=0; k<n; ++k) yrank[order[k]]=rank[k];
/* Rprintf("\nyranks:");
   for (k=0; k<n; ++k) Rprintf(" %g",yrank[k]); getch();
*/
        output_open(eout);
        clear_screen();
        p1=24; p2=40;
        i=corr_statistics(&ar,&sr1);
        if (i<0) return(-1);

        mx=my=sx=sy=sxy=0.0;
        for (k=0; k<n; ++k)
            {
            xx=xrank[k]; yy=yrank[k];
            mx+=xx; my+=yy; sx+=xx*xx; sy+=yy*yy; sxy+=xx*yy;
            }
        rho=(sxy-mx*my/n)/muste_sqrt((sx-mx*mx/n)*(sy-my*my/n));
        sr2=sxy;
        fnconv(rho,accuracy,x);
        sprintf(rivi,"Spearman's Rho=%s",spois(x));
        eoutput(rivi);
        i=Kendall_tau(n,xrank,yrank,&nc,&nd,&tau,&ptau);
        if (i<0) return(-1);
        ncd=(double)nc-(double)nd;
        a=muste_st_norm((double)(ncd/muste_sqrt(n*(n-1)*(2.0*n+5.0)/18.0)),(double)0.0);
        fnconv(tau,accuracy,x);
        fnconv(a,accuracy,strp);
        sprintf(rivi,"Kendall's  Tau=%s (Nc=%d Nd=%d P=%s normal approximation)"
                                ,spois(x),nc,nd,spois(strp));
        eoutput(rivi);
        if (ptau>=0.0)
            {
            fnconv(ptau,accuracy,strp);
            sprintf(rivi,"       Exact P=%s",spois(strp));
            eoutput(rivi);
            }

        if (!simumax)
            {
            output_close(eout);
            return(1);
            }
        sprintf(rivi,"Critical levels by simulation:");
        eoutput(rivi);
        init_rivi(rivi);
        set(rivi,"R           Rho        ",15);
        eoutput(rivi);
        sur_print("\n\n\n\nTo interrupt simulation press any key!\n");
        PR_UP; PR_UP; PR_UP; PR_UP;
        perm=(int *)muste_malloc(n*sizeof(int));
        if (perm==NULL) { not_enough_memory(); return(-1); }

        d1=disp_gap; d=0; u=0L; rr=0L; rs=0L;

        while (1)
            {
            for (j=0; j<n; ++j) perm[j]=j;
            tr1=tr2=0.0;
            for (j=0; j<n; ++j)
                {
                r=j+(int)((n-j)*RND);
                ii=perm[r]; perm[r]=perm[j];
                tr1+=x_space[sample[0]+j]*x_space[sample[1]+ii];
                tr2+=xrank[j]*yrank[ii];
                }
            ++u; ++d;
            if (tr1<=sr1) ++rr;
            if (tr2<=sr2) ++rs;
            if (d==d1)
                {
                d=0;
                s1=(double)rr/(double)u;
                s2=(double)rs/(double)u;
                sprintf(sbuf,"Critical level %7.5f %.5f  N=%d",(double)s1,s2,u); sur_print(sbuf);
                sprintf(sbuf,"\nStandard error %7.5f %.5f",muste_sqrt((double)(s1*(1.0-s1)/(double)u))
                                    ,muste_sqrt((double)(s2*(1.0-s2)/(double)u))); sur_print(sbuf);
                sur_print("\n"); PR_UP; PR_UP;
                CURSOR_OFF; headline(""); CURSOR_ON;
                if (u>=simumax) break;
                if (sur_kbhit())
                    {
                    i=sur_getch(); break;
                    }
                }
            }

        PR_UP;
        sprintf(rivi,"Critical level %7.5f %.5f  N=%d",(double)s1,s2,u);
        eoutput(rivi);
        sprintf(rivi,"Standard error %7.5f %.5f",muste_sqrt((double)(s1*(1.0-s1)/(double)u))
                            ,muste_sqrt((double)(s2*(1.0-s2)/(double)u)));
        eoutput(rivi);

        output_close(eout);
        return(1);
        }

static int corr_statistics(double *par,double *psr1)
        {
        extern double muste_cdf_t();
        extern char *spois();
        int k,n;
        char rivi[LLENGTH], xx[LLENGTH];
        double x,y,mx,my,sx,sy,sxy;
        char strp[32];

        n=ns[0];
        eoutput("Rank correlations:");
        sprintf(rivi,"Samples: N=%d            %-15.14s %-15.14s",
                               n,word[1],word[2]);
        eoutput(rivi);

        mx=my=sx=sy=sxy=0.0;
        for (k=0; k<n; ++k)
            {
            x=x_space[sample[0]+k];
            y=x_space[sample[1]+k];
            mx+=x; my+=y;
            sx+=x*x; sy+=y*y; sxy+=x*y;
            }
        mx/=(double)n; my/=(double)n;
        sx=muste_sqrt((sx-n*mx*mx)/(double)(n-1));
        sy=muste_sqrt((sy-n*my*my)/(double)(n-1));
        if (sx==0.0 || sy==0.0)
            {
            sur_print("\nVariable is a constant!");
            WAIT; return(-1);
            }
        *par=((sxy-n*mx*my)/(double)(n-1))/sx/sy;
        *psr1=sxy;

        init_rivi(rivi);
        set(rivi,"Mean",0);
        fnconv(mx,accuracy+2,xx); set(rivi,xx,p1);
        fnconv(my,accuracy+2,xx); set(rivi,xx,p2);
        eoutput(rivi);
        set(rivi,"Standard deviation",0);
        fnconv(sx,accuracy+2,xx); set(rivi,xx,p1);
        fnconv(sy,accuracy+2,xx); set(rivi,xx,p2);
        eoutput(rivi);

        fnconv(*par,accuracy,xx);
        x=*par*muste_sqrt((n-2.0)/(1.0-*par**par));
        fnconv(muste_cdf_t(x,(double)(n-2)),accuracy,strp);
        sprintf(rivi,"Product moment correlation R=%s (P=%s)",
                                        spois(xx),spois(strp));
        eoutput(rivi);

        return(1);
        }

static char *spois(char *s)
        {
        while (*s==' ') ++s;
        return(s);
        }

/* kendall.c 2.7.1986/SM (26.10.1990)
*/

static double *u,*v;
static int *k_order;    /* 26.10.1990 */

static int Kendall_tau(int n,float *xrank,float *yrank,int *pnc,int *pnd,double *ptau,double *pptau)
        {
        int i,j,h,k;
        char ind;
        double a,t;
        int m,mk=0;

        h=n;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind='1';
                for (k=0; k<n-h; ++k)
                    {
                    if (xrank[k]>xrank[k+h])
                        {
                        a=xrank[k]; xrank[k]=xrank[k+h]; xrank[k+h]=a;
                        a=yrank[k]; yrank[k]=yrank[k+h]; yrank[k+h]=a;
                        ind='0';
                        }
                    }
                if (ind=='1') break;
                }
            }
        k_order=(int *)muste_malloc(n*sizeof(int));
        if (k_order==NULL) { not_enough_memory(); return(-1); }
        for (k=0; k<n; ++k) k_order[k]=k;
        *pnc=*pnd=0;
        for (i=0; i<n; ++i)
            for (j=i; j<n; ++j)
                {
                if (xrank[i]==xrank[j]) continue;
                h=k_order[j]; k=k_order[i];
                if (yrank[h]>yrank[k]) ++*pnc;
                else if (yrank[h]<yrank[k]) ++*pnd;
                }
        *ptau=((double)*pnc-(double)*pnd)/(double)(n*(n-1)/2);
        if (n>30) { *pptau=-1.0; return(1); }

        sur_print("\nComputing null distribution of Kendall's tau...\n");

        m=n*(n-1)/2+2;  /* indeksointi 1,2,3,... */

        u=(double *)muste_malloc(m*sizeof(double));
        if (u==NULL) { not_enough_memory(); return(-1); }
        v=(double *)muste_malloc(m*sizeof(double));
        if (v==NULL) { not_enough_memory(); return(-1); }

        for (i=1; i<m; ++i) u[i]=v[i]=0.0;
        u[1]=u[2]=1.0; k=2;
        while (k<n)
            {
            ++k;
            mk=k*(k-1)/2+1;
            v[1]=1.0; i=1;
            while (i<mk)
                {
                int a;
                ++i;
                a=i-k+1; if (a<1) a=1;
                t=0.0;
                for (h=a; h<=i; ++h) t+=u[h]; v[i]=t;
                }
            sprintf(sbuf,"%d/%d\n",k,n); sur_print(sbuf); PR_UP;
            for (i=1; i<=mk; ++i) u[i]=v[i];
            }
        t=0.0; for (i=1; i<=mk; ++i) t+=u[i];
        a=0.0; for (i=1; i<=mk; ++i) { a+=u[i]; u[i]=a/t; }

        *pptau=u[(*pnc-*pnd+n*(n-1)/2+1)/2+1];
                               /*    +1 koska pyör.alaspäin */
                               /* tasatilanteissa parittomia arvoja */
        PR_UP; ERASE; PR_UP;
        muste_free(u); muste_free(v); muste_free(k_order);
        return(1);
        }

/* compd.c 4.7.1986/SM (10.6.1989)
    COMPARE <sample>,#<distribution>
                     #Normal
*/
// #define MAX_N 1

static int op_compd()
        {
        int i;
        char distr[LLENGTH];
        char test[LLENGTH];

        strcpy(survoxxx,etmpd); strcat(survoxxx,"SURVO.XXX");
        n_sample=1;
        results_line=0;
        if (g==4)
            {
            i=edline2(word[3],1,1);
            if (i==0) return(1);
            results_line=i;
            }
        i=sp_init(r1+r-1); if (i<0) return(1);
        i=d_load_samples(); if (i<0) return(1);
        strcpy(distr,word[2]+1);
        muste_strupr(distr);
        i=spfind("TEST");
        if (i<0) *test=EOS; else { strcpy(test,spb[i]); muste_strupr(test); }

        if (strncmp(distr,"NORM",4)==0)
            { test_normality(); return(1); }


        sprintf(sbuf,"\nUnknown distribution %s",distr); sur_print(sbuf);
        WAIT;
        return(1);
        }

extern FILE *data;

static int d_load_samples()
        {
        int i,k;
        int h;
        char x[LLENGTH];
        char *p,*q;
        char *var;
        int nro;
        int j;
        double y;

        n_total=0;
        sample[0]=0;

        data=muste_fopen(survoxxx,"wb");
        if (data==NULL)
            {
            sur_print("\nCannot open temporary file SURVO.XXX");
            WAIT; return(-1);
            }
        for (i=0; i<n_sample; ++i)
            {
            strcpy(x,word[i+1]);
            p=strchr(x,'(');
            if (p!=NULL)
                {
                *p=EOS; ++p;
                q=strchr(p,')');
                if (q==NULL)
                    {
                    sprintf(sbuf,"\n) missing in %s",word[i+1]);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                *q=EOS;
                var=p;
                }
            else var=x;

            h=data_open(x,&d); if (h<0) return(-1);
            nro=varfind(&d,var); if (nro<0) return(-1);
            ns[i]=0;
            for (j=d.l1; j<=d.l2; ++j)
                {
                data_load(&d,j,nro,&y);
                if (y==MISSING8) continue;
                p=(char *)&y;
                for (h=0; h<sizeof(double); ++h,++p) putc((int)*p,data);
                ++n_total;
                ++ns[i];
                }
            data_close(&d);
            }
        muste_fclose(data);

        x_space=(double *)muste_malloc(n_total*sizeof(double));
        if (x_space==NULL) { not_enough_memory(); return(-1); }
        samp=(int *)muste_malloc(n_total*sizeof(int));
        if (samp==NULL) { not_enough_memory(); return(-1); }
        rank=(float *)muste_malloc(n_total*sizeof(float));
        if (rank==NULL) { not_enough_memory(); return(-1); }

        data=muste_fopen(survoxxx,"rb");
        k=0;
        for (i=0; i<n_sample; ++i)
            {
            sprintf(sbuf,"\nSample %d",i+1); sur_print(sbuf);
            sample[i]=k;
            for (j=0; j<ns[i]; ++j)
                {
                p=(char *)&x_space[k];
                for (h=0; h<sizeof(double); ++h,++p) *p=(char)getc(data);
                samp[k]=i;
                ++k;
                }
            }
        muste_fclose(data);
        remove(survoxxx);
/*      sprintf(sbuf,"DEL %s",survoxxx);
        system(sbuf);
*/
        return(1);
        }

/* compd2.c 4.7.1986/SM (8.7.1986)
*/

static int sort_sample()
        {
        int i;
        int h,k,g;
        char ind;
        double y;
        float a;

        h=n_total;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind='1';
                for (k=0; k<n_total-h; ++k)
                    {
                    if (x_space[k]>x_space[k+h])
                        {
                        y=x_space[k]; x_space[k]=x_space[k+h]; x_space[k+h]=y;
                        i=samp[k]; samp[k]=samp[k+h]; samp[k+h]=i;
                        ind='0';
                        }
                    }
                if (ind=='1') break;
                }
            }

        k=0;
        while (k<n_total)
            {
            y=x_space[k]; h=1;
            while (k+h<n_total && y==x_space[k+h]) ++h;
            --h;
            a=(float)(k+1+h/2.0);
            for (g=k; g<=k+h; ++g) rank[g]=a;
            k+=h+1;
            }
        return(1);
        }

static int test_normality()
        {
        int i;
        char rivi[LLENGTH];

        i=sort_sample(); if (i<0) return(-1);

        output_open(eout);

        sprintf(rivi,"Tests for normality: Sample %s N=%u",word[1],n_total);
        eoutput(rivi);

        d_basic_statistics();
        Shapiro_Wilk();
        Dagostino();
        Anderson_Darling();
        return(1);
        }

/* compd3.c 4.7.1986/SM (8.7.1986)
*/
static double sd_copy; // 1.7.2011
static double mean_copy; // 1.7.2011

static int d_basic_statistics()
        {
        extern char *spois();
   //   int i;
        int k;
        char rivi[LLENGTH];
        char x[LLENGTH];
        char y[LLENGTH];
        double dn,s1,s2,s3,s4,a,b,sd,mean; // sd lisätty 1.7.2011/SM
        double median;
   //   char strp[32];
        dn=n_total;
        s1=s2=s3=s4=0.0;
        for (k=0; k<n_total; ++k) s1+=x_space[k];
        s1/=dn;
        for (k=0; k<n_total; ++k)
            {
            a=x_space[k]-s1;
            b=a*a; s2+=b;
            b=a*b; s3+=b;
            b=a*b; s4+=b;
            }
        sd=s2; sd_copy=s2;
        mean=s1; mean_copy=s1;
        fnconv(s1,accuracy+2,x);
        fnconv(muste_sqrt(s2/(dn-1.0)),accuracy+2,y);
        sprintf(rivi,"Mean=%s  Std.dev.=%s",spois(x),spois(y));
        eoutput(rivi);
        fnconv(s3/(dn-1)/(sd/(dn-1)*muste_sqrt(sd/(dn-1))),accuracy+2,x);
        sb1=s3/(dn-1)/(sd/(dn-1)*muste_sqrt(sd/(dn-1))); // 18.7.2011
        b2=s4/(dn-1)/(sd/(dn-1)*sd/(dn-1));       // 18.7.2011
        fnconv(s4/(dn-1)/(sd/(dn-1)*sd/(dn-1))-3,accuracy+2,y);
        sprintf(rivi,"Skewness=%s  Kurtosis=%s  (normal values=0)",
                        spois(x),spois(y));
        eoutput(rivi);
        if ((n_total>>1)<<1==n_total)
            median=(x_space[n_total/2-1]+x_space[n_total/2])/2.0;
        else
            median=x_space[(n_total-1)/2];
        fnconv(median,accuracy+2,x);
        sprintf(rivi,"Median=%s",spois(x));
        eoutput(rivi);
        return(1);
        }

/* compdsw.c 4.7.1986/SM (9.11.1991)
   Shapiro-Wilk
   D'agostino
   Anderson-Darling
*/
static FILE *taulu;
static char tbl_name[LLENGTH];

static int Shapiro_Wilk()
        {
        extern char *spois();
        int i,h,n2,seekpos;
        float coeff[25];
        float p[9],y[9];
        double sw;
        char rivi[LLENGTH], x[LLENGTH], xp[16];
        double a,x1,x2,x3;

        if (n_total>50) return(1);
        strcpy(tbl_name,survo_path); strcat(tbl_name,"SYS/SWCOEFF.TBL"); // RS CHA TBL\\ -> SYS/
        taulu=muste_fopen(tbl_name,"rb");
        if (taulu==NULL)
            {
            sur_print("\nShapiro-Wilk coefficients not found!");
            WAIT; return(-1);
            }

        n2=(n_total+1)/2;
        if (2*n2!=n_total+1) seekpos=n_total*n_total/4;
        else                 seekpos=(n_total*n_total-1)/4;
        muste_fseek(taulu,(int)sizeof(float)*(int)seekpos,0);
        for (i=0; i<n2; ++i)
            {
            char *pc;
            pc=(char *)&coeff[i];
            for (h=0; h<sizeof(float); ++h)
                {
                *pc=(unsigned char)getc(taulu);
                ++pc;
                }
            }
        muste_fclose(taulu);

        sw=0.0;
        for (i=0; i<n2; ++i) sw+=coeff[i]*(x_space[n_total-i-1]-x_space[i]);
        sw=sw*sw/sd_copy; // sd_copy saatu edellisessä funktiossa

        strcpy(tbl_name,survo_path); strcat(tbl_name,"SYS/SWPERC.TBL"); // RS CHA TBL\\ -> SYS/
        taulu=muste_fopen(tbl_name,"rb");
        if (taulu==NULL)
            {
            sur_print("\nShapiro-Wilk percentiles not found!");
            WAIT; return(-1);
            }
        muste_fseek(taulu,(int)((n_total-1)*9*sizeof(float)),0);
        for (i=0; i<9; ++i)
            {
            char *pc;
            pc=(char *)&p[i];
            for (h=0; h<sizeof(float); ++h)
                {
                *pc=(unsigned char)getc(taulu);
                ++pc;
                }
            }
        muste_fclose(taulu);
        y[0]=0.01; y[1]=0.02; y[2]=0.05; y[3]=0.10; y[4]=0.50;
        y[5]=0.90; y[6]=0.95; y[7]=0.98; y[8]=0.99;

        if (sw<p[0]) strcpy(xp,"(P<0.01)");
        else if (sw>p[8]) strcpy(xp,"(P>0.99)");
        else
            {
            i=0;
            while (1)
                {
                ++i;
                if (sw>=p[i]) continue;
                if (sw-p[i-1]<p[i]-sw) i-=2; else i-=1;
                if (i<0) i=0;
                if (i>6) i=6;
                break;
                }

            x1=p[i]; x2=p[i+1]; x3=p[i+2];
            a=y[i]*(sw-x2)*(sw-x3)/(x1-x2)/(x1-x3)
              +y[i+1]*(sw-x1)*(sw-x3)/(x2-x1)/(x2-x3)
              +y[i+2]*(sw-x1)*(sw-x2)/(x3-x1)/(x3-x2);
            sprintf(xp,"(P=%.3f)",a);
            }

        fnconv(sw,accuracy-1,x);
        sprintf(rivi,"Shapiro-Wilk W=%s %s",spois(x),xp);
        eoutput(rivi);
        return(1);
        }

static int Dagostino() // 18.7.2011/SM
    {
    double dn,yy,beta2,w,d,a,zb1;
    double meanb2,varb2,c,moment,zb2,b;
    double dag;
    char rivi[LLENGTH];
    extern double cdf_chi2();

    char x[LNAME],p[LNAME];

    dn=(double)n_total;

    yy=sb1*muste_sqrt((dn+1.0)*(dn+3.0)/(6.0*(dn-2.0)));
    beta2=3.0*(dn*dn+27.0*dn-70.0)*(dn+1.0)*(dn+3.0)/
          ( (dn-2.0)*(dn+5.0)*(dn+7.0)*(dn+9.0) );
    w=muste_sqrt(-1.0+muste_sqrt(2.0*(beta2-1.0)));
    d=1.0/muste_sqrt(muste_log(w));
    a=muste_sqrt(2.0/(w*w-1.0));
    zb1=d*muste_log(yy/a+muste_sqrt(yy*yy/(a*a)+1.0));

    meanb2=3.0*(dn-1.0)/(dn+1.0);
    varb2=24.0*dn*(dn-2.0)*(dn-3.0)/
          ((dn+1.0)*(dn+1.0)*(dn+3.0)*(dn+5.0));
    c=(b2-meanb2)/muste_sqrt(varb2);
    moment=6.0*(dn*dn-5.0*dn+2.0)/
       ((dn+7.0)*(dn+9.0))*muste_sqrt(6.0*(dn+3.0)*(dn+5.0)/
       (dn*(dn-2.0)*(dn-3.0)));
    a=6.0+8.0/moment*(2/moment+muste_sqrt(1.0+4.0/
      (moment*moment)));
    b=((1.0-2.0/a)/(1.0+c*muste_sqrt(2.0/(a-4))));
    b=muste_exp(muste_log(b)/3.0);
    b=1.0-2.0/(9.0*a)-b;
    zb2=b/muste_sqrt(2.0/(9.0*a));

    dag=zb1*zb1+zb2*zb2;

    fnconv(dag,accuracy+2,x);
    fnconv(1-muste_cdf_chi2(dag,2.0,1e-15),accuracy,p);
    sprintf(rivi,"D'Agostino-Pearson K^2=%s P=%s (%.4g %.4g)",spois(x),spois(p),zb1,zb2);
    eoutput(rivi);
    return(1);
    }

static int Anderson_Darling()
        {
        extern char *spois();
        int i;
        int k;
        float y[22],b0[22],b1[22],aa[22],p[22];
        double ad,sx;
        char rivi[LLENGTH], x[LLENGTH], xp[16];
        double a,b,x1,x2,x3;

        y[ 0]=0.050; b0[ 0]=-0.512; b1[ 0]= 2.10; aa[ 0]=0.1674;
        y[ 1]=0.100; b0[ 1]=-0.552; b1[ 1]= 1.25; aa[ 1]=0.1938;
        y[ 2]=0.150; b0[ 2]=-0.608; b1[ 2]= 1.07; aa[ 2]=0.2147;
        y[ 3]=0.200; b0[ 3]=-0.643; b1[ 3]= 0.93; aa[ 3]=0.2333;
        y[ 4]=0.250; b0[ 4]=-0.707; b1[ 4]= 1.03; aa[ 4]=0.2509;
        y[ 5]=0.300; b0[ 5]=-0.735; b1[ 5]= 1.02; aa[ 5]=0.2681;
        y[ 6]=0.350; b0[ 6]=-0.772; b1[ 6]= 1.04; aa[ 6]=0.2853;
        y[ 7]=0.400; b0[ 7]=-0.770; b1[ 7]= 0.90; aa[ 7]=0.3030;
        y[ 8]=0.450; b0[ 8]=-0.778; b1[ 8]= 0.80; aa[ 8]=0.3213;
        y[ 9]=0.500; b0[ 9]=-0.779; b1[ 9]= 0.67; aa[ 9]=0.3405;
        y[10]=0.550; b0[10]=-0.803; b1[10]= 0.70; aa[10]=0.3612;
        y[11]=0.600; b0[11]=-0.818; b1[11]= 0.58; aa[11]=0.3836;
        y[12]=0.650; b0[12]=-0.818; b1[12]= 0.42; aa[12]=0.4085;
        y[13]=0.700; b0[13]=-0.801; b1[13]= 0.12; aa[13]=0.4367;
        y[14]=0.750; b0[14]=-0.800; b1[14]=-0.09; aa[14]=0.4695;
        y[15]=0.800; b0[15]=-0.756; b1[15]=-0.39; aa[15]=0.5091;
        y[16]=0.850; b0[16]=-0.749; b1[16]=-0.59; aa[16]=0.5597;
        y[17]=0.900; b0[17]=-0.750; b1[17]=-0.80; aa[17]=0.6305;
        y[18]=0.950; b0[18]=-0.795; b1[18]=-0.89; aa[18]=0.7514;
        y[19]=0.975; b0[19]=-0.881; b1[19]=-0.94; aa[19]=0.8728;
        y[20]=0.990; b0[20]=-1.013; b1[20]=-0.93; aa[20]=1.0348;
        y[21]=0.995; b0[21]=-1.063; b1[21]=-1.34; aa[21]=1.1578;

        for (i=0; i<22; ++i) p[i]=aa[i]*(1+(b0[i]+b1[i]/n_total)/n_total);


        sx=muste_sqrt(sd_copy/(n_total-1.0));
        a=0.0;
        for (k=0; k<n_total; ++k)
            {
            b=muste_st_norm((double)((x_space[k]-mean_copy)/sx),(double)0.0);
            if (b>0.0 && b<1.0)
                a+=(2*k+1)*muste_log(b)+(2*n_total-2*k-1)*muste_log(1.0-b);
            }
        ad=-a/n_total-n_total;
        if (ad<p[0]) strcpy(xp,"(P>0.95)");
        else if (ad>p[21]) strcpy(xp,"(P<0.005)");
        else
            {
            i=0;
            while (1)
                {
                ++i;
                if (ad>=p[i]) continue;
                if (ad-p[i-1]<p[i]-ad) i-=2; else i-=1;
                if (i<0) i=0;
                if (i>19) i=19;
                break;
                }

            x1=p[i]; x2=p[i+1]; x3=p[i+2];
            a=y[i]*(ad-x2)*(ad-x3)/(x1-x2)/(x1-x3)
              +y[i+1]*(ad-x1)*(ad-x3)/(x2-x1)/(x2-x3)
              +y[i+2]*(ad-x1)*(ad-x2)/(x3-x1)/(x3-x2);
            sprintf(xp,"(P=%.3f)",1.0-a);
            }

        fnconv(ad,accuracy-1,x);
        sprintf(rivi,"Anderson-Darling A=%s %s",spois(x),xp);
        eoutput(rivi);
        return(1);
        }

