/* ctabtest.c 6.11.1994/SM (11.12.1994)
   TABTEST <table>,L
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXDIM 20
#define MAXT 1000
#define TSPACE 8000
#define MAXCLASS 30
#define MAXTOTAL 5000

typedef unsigned int FREQ;
#define ZERO 0.0
#define STRUCTURAL_ZERO 2147483646
#define MISSING_VALUE 2147483647
// 2^31-2=2147483646
#define FISHER 0
#define FIX_RC 1
#define FIX_R  2
#define FIX_C  3
#define FIX_N  4
#define FIX_F  5

static char *descr[] = { "Fisher's exact test",
                  "Fixed margins",
                  "Fixed row margins (horizontal sums)",
                  "Fixed column margins (vertical sums)",
                  "No fixed margins",
                  " " };
static char descrf[70];

#define PROB  0
#define X2    1
#define G2    2
static int test=PROB;

static char *tdescr[] = { ", Probability statistics", ", X^2 statistics", ", G^2 statistics" };

#define RAND  0
#define URAND 1
static int generator;
extern int sur_rand_seed();
extern double sur_rand();
extern int sur_urand_seed();
extern double sur_urand();
static int (*rand_seed1)();
static double (*rand1)();

static int results_line;
static int (*g_print)();

static char text[TSPACE];
static char *ptext;
static FREQ *f;
static int dim;
static int nc[MAXDIM];
static char *varname[MAXDIM];
static char *cname[MAXT];
static char type[16];
static int ndec;
static int missing_values;

static int m,n;
static FREQ total;
static FREQ total0;
static FREQ f_r[MAXCLASS];
static FREQ f_c[MAXCLASS];
static double cum_r[MAXCLASS];
static double cum_c[MAXCLASS];

static int sample1[MAXTOTAL];
static int sample2[MAXTOTAL];
static double chi2,g2;
static double prob;
static int fix;
static int fi,fj;
static FREQ fij;
static double logn[MAXTOTAL+1];
static double lnn[MAXTOTAL+1];
static double conf_level,conf_coeff;
static double eps=1e-7; /* when comparing values (case =) */

static int count,pcount,dn,dcount,ecount,maxcount;
extern char **spb;
extern double muste_cdf_chi2();
extern double muste_inv_std();

static int r_original;

static int c_chi2(double *pchi2,FREQ *f);
static int c_g2(double *pg2,FREQ *f);
static int c_prob(double *pprob,FREQ *f);
static int c_cprob(double *pprob,FREQ *f);
static int c_rprob(double *pprob,FREQ *f);
static int c_nprob(double *pprob,FREQ *f);
static int c_fprob(double *pprob,FREQ *f);
static int c_margins();
static int c_logn();
static int cumul_freq();
static int find_fix();
static int fix_error();
static int simulation();
static int rand_init();
static int rand_error(char *x);
static int disp0();
static int tab_disp();
static int make_samples();
static int permutoi(int n,int *s);
static int simul(FREQ total,int n,int *s,double *cum);
static int f_simul();
static int taulukoi(int *s1,int *s2,FREQ *f);
static int printout();
static int read_ftable(char *name,FREQ **f,int *pdim,int *pncvar,int *nc,
           char **varname,char **cname,char *type,int *pndec);
static int store_label(char *s);
// static int ctypes(int *ctype,int dim,int *nc,char **cname);
// static int check_varname_initials(int dim,char **varname);
static int print_line(char *line);
static int not_enough_memory();
static int table_to_matrix(char *name,FREQ *f,int m,int n,char **varname,char **cname);
static int lab_copy(int m,char **cname,char *lab);
static double chi_square(int m);
static int goodness_of_fit_test(FREQ *f,int m,int n);
static int disp0fit(int m,int n,double chi2);

/***********************
char *specs0[]={ "MATRIX", "TEST", "CONF", "SIMUMAX", "FIX",
                 "X2FILE", "GAP", "RAND", "!" };
char **specs=specs0;
*************************/
/***************
main(argc,argv)
int argc; char *argv[];
*****************/

void muste_tabtest(char *argv)
        {
        int i;
    //  char ch;
        int ncvar;
    //  char nimi[LLENGTH];
        char s[LLENGTH];
        extern int print_line();
        int ltotal;

//      if (argc==1) return;
        s_init(argv);
        i=spec_init(r1+r-1); if (i<0) return;
        r_original=r;
        results_line=0;
        if (g<2)
            {
init_remarks();
rem_pr("Usage: TABTEST <table_of_frequencies>,L");
rem_pr("");
rem_pr("TABTEST performs various tests for independence etc. by simulation");
rem_pr("for a two-dimensional table of frequencies. The table is");
rem_pr("given in edit field in the form used by the TAB operations.");
rem_pr("The table can be also given in a simpler form without any labels:");
rem_pr("TABLE TEST / Example");
rem_pr("7 2 0");
rem_pr("3 1 5");
rem_pr("");
rem_pr("Different assumptions about the stochastic structure of the table are");
rem_pr("determined by a FIX specification with following alternatives:");
rem_pr("   FIX=Fisher  Both row and column margins fixed (exact test)");
rem_pr("   FIX=RC      Both row and column margins fixed");
rem_pr("   FIX=C       Column margins (vertical sums) fixed");
rem_pr("   FIX=R       Row margins (horizontal sums) fixed");
rem_pr("   FIX=N       No fixed margins, only the grand total fixed");
rem_pr("   FIX=F(i,j)  Element of row i and column j fixed");
wait_remarks(1);
rem_pr("The test statistics used in simulation is selected by a TEST specification:");
rem_pr("TEST=X^2 is the common Pearson's chi-square statistics: sum of (O-E)^2/E .");
rem_pr("TEST=G^2 is the likelihood statistics: sum of -2*O*log(O/E) .");
rem_pr("TEST=PROB is the probability of the simulated table. The unknown");
rem_pr("          margin probabilities are replaced by simulated relative");
rem_pr("          frequencies. We call this `Probability statistics'.");
rem_pr("In case FIX=Fisher, TEST=PROB is always selected.");
rem_pr("In other cases TEST=X^2 is default.");
wait_remarks(1);
rem_pr("Maximum number of replicates is given by SIMUMAX (default 10000000).");
rem_pr("The seed number of the random number generator (either 'rand' or 'urand')");
rem_pr("is given by RAND (default RAND=rand(12345). See RAND? .");
rem_pr("The process may be interrupted by pressing any key.");
rem_pr("");
rem_pr("The results are displayed after each 100 replicates as a table of the form");
rem_pr("      N           P               Confidence interval (level=0.95)   ");
rem_pr("# of replicates   Estimate of P                lower limit           ");
rem_pr("            s.e.  Standard error               upper limit           ");
rem_pr("");
rem_pr("The confidence level for P is set by CONF=p (0.8<p<1). Default is CONF=0.95");
wait_remarks(1);
rem_pr("The two-way table is also saved as a matrix by using the specification");
rem_pr("MATRIX=<name_of_a_matrix_file> , say, MATRIX=T .");
rem_pr("This matrix can be analyzed further, for example, by the sucro command");
rem_pr("/CHI2 T");
rem_pr("which computes various derived tables as matrices such as the expected");
rem_pr("frequencies and decomposition of the X^2 statistics in cells and     ");
rem_pr("margins.                                                             ");
wait_remarks(1);
rem_pr("........................................................................");
rem_pr("Example:");
rem_pr("TABLE T / This 2x2 table is tested with default settings.");
rem_pr("7 2");
rem_pr("1 4");
rem_pr("");
rem_pr("TABTEST T,CUR+1");
wait_remarks(2);
s_end(argv[1]);
return;
            }
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }
        ptext=text;
        i=read_ftable(word[1],&f,&dim,&ncvar,nc,varname,cname,type,&ndec);
        if (i<0) return;
        if (dim!=2)
            {
            sur_print("\nTABTEST can handle two-way tables only!");
            WAIT; return;
            }
        m=nc[1]; n=nc[0];

        i=spfind("FIX");
        if (i>=0 && muste_strcmpi(spb[i],"FIT")==0)
            {
            i=goodness_of_fit_test(f,m,n);
            if (i>0) s_end(argv[1]);
            return;
            }

        i=spfind("MATRIX");
        if (i>=0)
            {
            i=table_to_matrix(spb[i],f,m,n,varname,cname);
            if (i<0) return;
            }
        ltotal=0L;
        for (i=0; i<m*n; ++i)
            {
            ltotal+=f[i];
            if (f[i]<0L)
                {
                sur_print("\nNegative \"frequencies\" in the table!!!");
                WAIT; return;
                }
            }
        if (ltotal>MAXTOTAL)
            {
            sprintf(sbuf,"\nTotal frequency %d too great! (max=%d)",
                                          ltotal,MAXTOTAL);
            WAIT; return;
            }
        total=ltotal;
        total0=total;

        test=X2;
        i=spfind("TEST");
        if (i>=0)
            {
            strcpy(s,spb[i]); muste_strupr(s);
            if (strncmp(s,"PR",2)==0) test=PROB;
            else if (strncmp(s,"X2",2)==0 || strncmp(s,"X^2",3)==0) test=X2;
            else if (strncmp(s,"G2",2)==0 || strncmp(s,"G^2",3)==0) test=G2;
            else
                {
                sur_print("\nError in TEST specification!");
                sur_print("\nAlternatives TEST=PROB, TEST=X2, TEST=G2");
                WAIT; return;
                }
            }

        conf_level=0.95;
        i=spfind("CONF");
        if (i>=0) conf_level=atof(spb[i]);
        if (conf_level<0.8 || conf_level>=1.0)
            {
            sur_print("\nError in CONF=p! Confidence level p must be 0.8<p<1");
            WAIT; return;
            }
        conf_coeff=muste_inv_std(1.0-(1.0-conf_level)/2);

        c_margins();

        i=find_fix(); if (i<0) return;
        c_logn();
        c_chi2(&chi2,f); chi2-=eps;
        c_g2(&g2,f); g2-=eps;
        if (fix==FISHER) test=PROB;
        if (test==PROB)
            {
            switch (fix)
                {
              case FIX_RC:
              case FISHER: c_prob(&prob,f); break;
              case FIX_C:  c_cprob(&prob,f); break;
              case FIX_R:  c_rprob(&prob,f); break;
              case FIX_N:  c_nprob(&prob,f); break;
              case FIX_F:  c_fprob(&prob,f); break;
                }
            prob-=eps;
            }

        if (fix!=FIX_RC && fix!=FISHER) cumul_freq();
        g_print=&sur_print;

        maxcount=10000000L;
        i=spfind("SIMUMAX");
        if (i>=0) maxcount=atol(spb[i]);
        disp0();
        i=simulation(); if (i<0) return;
        g_print=&print_line;
        printout();
        r=r_original;
        s_end(argv);
        }

static int c_chi2(double *pchi2,FREQ *f)
        {
        int i,j,k;
        double a,b,e;

        if (fix!=FIX_RC)
            {
            c_margins();
            }
        a=0.0; k=0;
        for (j=0; j<n; ++j)
            for (i=0; i<m; ++i)
                {
                e=(double)f_r[i]*(double)f_c[j]/(double)total;
                b=f[k]-e;
                ++k;
                if (e>0.0) a+=b*b/e;
                }
        *pchi2=a;
        return(1);
        }

static int c_g2(double *pg2,FREQ *f)
        {
        int i,j,k;
        double a;

        if (fix!=FIX_RC)
            c_margins();
        a=0.0; k=0;
        for (j=0; j<n; ++j)
            for (i=0; i<m; ++i)
                {
                a+=f[k]*(lnn[f_r[i]]+lnn[f_c[j]]-lnn[total]-lnn[f[k]]);
                ++k;
                }
        *pg2=-2*a;
        return(1);
        }

static int c_prob(double *pprob,FREQ *f)
        {
        int i,k;
        double a;

        a=0.0; k=m*n;
        for (i=0; i<k; ++i) a+=logn[f[i]];
        *pprob=a;
        return(1);
        }

static int c_cprob(double *pprob,FREQ *f)
        {
        int i,j,k;
        double a;

        c_margins();
        a=0.0; k=0;
        for (j=0; j<n; ++j)
            for (i=0; i<m; ++i)
                {
                a+=logn[f[k]]-f[k]*lnn[f_r[i]];
                ++k;
                }
/*      for (j=0; j<n; ++j) a-=logn[f_c[j]];   vakio! */
        *pprob=a;
        return(1);
        }

static int c_rprob(double *pprob,FREQ *f)
        {
        int i,j,k;
        double a;

        c_margins();
        a=0.0; k=0;
        for (j=0; j<n; ++j)
            for (i=0; i<m; ++i)
                {
                a+=logn[f[k]]-f[k]*lnn[f_c[j]];
                ++k;
                }
/*      for (i=0; i<m; ++i) a-=logn[f_r[i]];  vakio! */
        *pprob=a;
        return(1);
        }

static int c_nprob(double *pprob,FREQ *f)
        {
        int i,j,k;
        double a;

        c_margins();
        a=0.0; k=0;
        for (j=0; j<n; ++j)
            for (i=0; i<m; ++i)
                {
                a+=logn[f[k]]-f[k]*(lnn[f_r[i]]+lnn[f_c[j]]);
                ++k;
                }
        *pprob=a;
        return(1);
        }

static int c_fprob(double *pprob,FREQ *f)
        {
        int i,j,k,h;
        double a;

        c_margins();

        if (total>MAXTOTAL)
            {
            sprintf(sbuf,"\nSample size in a simulated table exceeds %d",MAXTOTAL);
            sur_print(sbuf);
            WAIT; return(-1);
            }
        h=fi+m*fj;
        --f[h]; /* --f_r[fi]; --f_c[fj]; */

        a=0.0; k=0;
        for (j=0; j<n; ++j)
            for (i=0; i<m; ++i)
                {
                a+=logn[f[k]]-f[k]*(lnn[f_r[i]]+lnn[f_c[j]]);
                ++k;
                }
        a-=lnn[f_r[fi]]+lnn[f_c[fj]];
/*      a-=logn[total-1]-(total-1.0)*(lnn[2]+lnn[total-1]);  total=total0 ! */
        ++f[h]; /* ++f_r[fi]; ++f_c[fj]; */
        *pprob=a;
        return(1);
        }

static int c_margins()
        {
        int i,j,k;

        for (i=0; i<m; ++i) f_r[i]=0;
        for (j=0; j<n; ++j) f_c[j]=0;
        k=0;
        for (j=0; j<n; ++j)
            for (i=0; i<m; ++i)
                {
                f_c[j]+=f[k];
                f_r[i]+=f[k];
                ++k;
                }
        return(1);
        }

static int c_logn()
        {
        FREQ i;
        double a,b;

        logn[0]=0.0; lnn[0]=0.0; a=0.0;
        for (i=1; i<=MAXTOTAL; ++i) { b=log((double)i); a+=b; logn[i]=a; lnn[i]=b; }
        return(1);
        }

static int cumul_freq()
        {
        int i,j;
        FREQ s;

        s=0; for (j=0; j<n; ++j) { s+=f_c[j]; cum_c[j]=(double)s/(double)total; }
        s=0; for (i=0; i<m; ++i) { s+=f_r[i]; cum_r[i]=(double)s/(double)total; }
        return(1);
        }

static int find_fix()
        {
        int i;
        char x[LLENGTH];
        char *p,*q;

        fix=FISHER;
        i=spfind("FIX");
        if (i<0) return(1);
        strcpy(x,spb[i]);
        muste_strupr(x);
        if (strncmp(x,"FI",2)==0) fix=FISHER;
        else if (strcmp(x,"RC")==0 || strcmp(x,"CR")==0) fix=FIX_RC;
        else if (strcmp(x,"R")==0) fix=FIX_R;
        else if (strcmp(x,"C")==0) fix=FIX_C;
        else if (strcmp(x,"N")==0) fix=FIX_N;
        else if (*x=='F')
            {
            fix=FIX_F;
            p=strchr(x,'(');
            if (p==NULL) { fix_error(); return(-1); }
            q=strchr(p+1,',');
            if (q==NULL) { fix_error(); return(-1); }
            *q=EOS;
            fi=atoi(p+1)-1;
            fj=atoi(q+1)-1;
            if (fi<0 || fi>m-1) { fix_error(); return(-1); }
            if (fj<0 || fj>n-1) { fix_error(); return(-1); }
            fij=f[fi+m*fj];
            if (fij==0)
                {
                sur_print("\nTarget value is 0!");
                WAIT; return(-1);
                }
            sprintf(descrf,"Target value F(%d,%d)=%d",fi+1,fj+1,fij);
            descr[FIX_F]=descrf;
            }
        else
            {
            sprintf(sbuf,"\nUnknown FIX=%s",spb[i]);
            sur_print(sbuf);
            sur_print("\nPermitted alternatives are:");
            sur_print("\nFIX=Fisher, FIX=RC, FIX=R, FIX=C, FIX=N, FIX=F(i,j)");
            WAIT; return(-1);
            }
        return(1);
        }

static int fix_error()
        {
        sur_print("\nError in FIX=F(i,j) specification!");
        WAIT; return(-1);
        }


static FILE *tied;

static int simulation()
        {
        int i;
        double kchi2, kg2, kprob;
    //  double p1,p2;
        int x2_file;
        char nimi[LNAME];

        i=rand_init(); if (i<0) return(-1);
        make_samples();

        x2_file=0;
        i=spfind("X2FILE");
        if (i>=0)
            {
            strcpy(nimi,spb[i]);
            if (strchr(nimi,':')==NULL) { strcpy(nimi,edisk); strcat(nimi,spb[i]); }
            tied=muste_fopen(nimi,"wt");
            if (tied==NULL)
                {
                sprintf(sbuf,"\nCannot open %s for X^2 values!",nimi);
                sur_print(sbuf); WAIT;
                return(-1);
                }
            x2_file=1;
            }

        dn=10000L;
        i=spfind("GAP");
        if (i>=0) dn=atol(spb[i]);

        count=0L; pcount=0L; dcount=0L; ecount=0L;

        while (1)
            {
            ++count; ++dcount;
            switch (fix)
                {
              case FISHER:
              case FIX_RC:
                permutoi(total,sample2);
                taulukoi(sample1,sample2,f);
                if (test==PROB)
                    {
                    c_prob(&kprob,f);
                    if (kprob>=prob) ++pcount;
                    }
                else if (test==X2)
                    {
                    c_chi2(&kchi2,f);
                    if (kchi2>=chi2) ++pcount;
                    }
                else
                    {
                    c_g2(&kg2,f);
                    if (kg2>=g2) ++pcount;
                    }
                break;
              case FIX_C:
                simul(total,m,sample1,cum_r);
                taulukoi(sample1,sample2,f);
                if (test==PROB)
                    {
                    c_cprob(&kprob,f);
                    if (kprob>=prob)
                        {
                        ++pcount;
                        if (x2_file)
                            {
                            c_chi2(&kchi2,f);
                            fprintf(tied,"%g\n",kchi2);
                            }
                        }
                    }
                else if (test==X2)
                    {
                    c_chi2(&kchi2,f);
                    if (kchi2>=chi2) ++pcount;
                    }
                else
                    {
                    c_g2(&kg2,f);
                    if (kg2>=g2) ++pcount;
                    }
                break;
              case FIX_R:
                simul(total,n,sample2,cum_c);
                taulukoi(sample1,sample2,f);
                if (test==PROB)
                    {
                    c_rprob(&kprob,f);
                    if (kprob>=prob)
                        {
                        ++pcount;
                        if (x2_file)
                            {
                            c_chi2(&kchi2,f);
                            fprintf(tied,"%g\n",kchi2);
                            }
                        }
                    }
                else if (test==X2)
                    {
                    c_chi2(&kchi2,f);
                    if (kchi2>=chi2) ++pcount;
                    }
                else
                    {
                    c_g2(&kg2,f);
                    if (kg2>=g2) ++pcount;
                    }
                break;
              case FIX_N:
                simul(total,n,sample2,cum_c);
                simul(total,m,sample1,cum_r);
                taulukoi(sample1,sample2,f);
                if (test==PROB)
                    {
                    c_nprob(&kprob,f);
                    if (kprob>=prob)
                        {
                        ++pcount;
                        if (x2_file)
                            {
                            c_chi2(&kchi2,f);
                            fprintf(tied,"%g\n",kchi2);
                            }
                        }
                    }
                else if (test==X2)
                    {
                    c_chi2(&kchi2,f);
                    if (kchi2>=chi2) ++pcount;
                    }
                else
                    {
                    c_g2(&kg2,f);
                    if (kg2>=g2) ++pcount;
                    }
                break;
              case FIX_F:
                i=f_simul(); if (i<0) return(1);
                if (test==PROB)
                    {
                    i=c_fprob(&kprob,f); if (i<0) return(1);
                    if (kprob>=prob)
                        {
                        ++pcount;
                        if (x2_file)
                            {
                            c_chi2(&kchi2,f);
                            fprintf(tied,"%g\n",kchi2);
                            }
                        }
                    }
                else if (test==X2)
                    {
                    c_chi2(&kchi2,f);
                    if (kchi2>=chi2) ++pcount;
                    }
                else
                    {
                    c_g2(&kg2,f);
                    if (kg2>=g2) ++pcount;
                    }
                break;
                }

            if (dcount>=dn)
                {
                tab_disp();
                if (count>=maxcount) break;
                if (sur_kbhit()) { sur_getch(); break; }         
                dcount=0;  // 21.10.2010
                }
            }
        if (x2_file) fclose(tied);
        return(1);
        }

static int rand_init()
        {
        int i;
        int seed;
        char x[LLENGTH];
        char *p,*q;
        double a;

        seed=12345L;
        generator=RAND; rand_seed1=sur_rand_seed; rand1=sur_rand;
        i=spfind("RAND");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            p=strchr(x,'(');
            if (p==NULL) { rand_error(x); return(-1); }
            *p=EOS; ++p; q=strchr(p,')');
            if (q==NULL) { rand_error(x); return(-1); }
            *q=EOS;
            seed=atol(p);
            muste_strupr(x);
            if (strcmp(x,"RAND")==0) { generator=RAND; rand_seed1=sur_rand_seed; rand1=sur_rand; }
            else if (strcmp(x,"URAND")==0) { generator=URAND; rand_seed1=sur_urand_seed; rand1=sur_urand; }
            else { rand_error(spb[i]); return(-1); }
            }
        (*rand_seed1)(seed);
        for (i=0; i<10; ++i) a=(*rand1)();

        return(1);
        }

static int rand_error(char *x)
        {
        sprintf(sbuf,"\nError in RAND=%s",x);
        sur_print(sbuf); WAIT; return(1);
        }

static int disp0()
        {
        int k;

        if (r>r3-6) r=1;
        LOCATE(r+2,9);
        k=sprintf(sbuf,"Testing a %d*%d table by simulation:",m,n);
        sprintf(sbuf+k,"  X^2=%g P=%6f",
                      chi2+eps,1-muste_cdf_chi2(chi2+eps,(double)((m-1)*(n-1)),1e-15));
        (*g_print)(sbuf);
        LOCATE(r+3,9);
        sprintf(sbuf,"%.*s  G^2=%g P=%6f",k,space,
                      g2+eps,1-muste_cdf_chi2(g2+eps,(double)((m-1)*(n-1)),1e-15));
        (*g_print)(sbuf);
        LOCATE(r+4,9);
        sprintf(sbuf,"%s%s",descr[fix],tdescr[test]);
        (*g_print)(sbuf);
        LOCATE(r+5,9);
        sprintf(sbuf,"         N    P       Confidence interval (%g)",conf_level);
        (*g_print)(sbuf);
        LOCATE(r3+2,9); PR_EBLD;
        sprintf(sbuf,"To interrupt, press any key! (max N is %d)",maxcount);
        sur_print(sbuf);
        return(1);
        }

static int tab_disp()
        {
        double p1;
        double se;
        double lower,upper;

        LOCATE(r+6,9); PR_EUDL;
        p1=(double)pcount/(double)count;
        se=sqrt(p1*(1.0-p1)/(double)count);
        lower=p1-conf_coeff*se; if (lower<0.0) lower=0.0;
        upper=p1+conf_coeff*se; if (upper>1.0) upper=1.0;
        sprintf(sbuf,"%10d %.8f %.8f lower limit",
                       count,p1,lower);
        (*g_print)(sbuf);
        LOCATE(r+7,9);
        sprintf(sbuf,"      s.e. %.8f %.8f upper limit",se,upper);
        (*g_print)(sbuf);
        dcount=0L;
        headline("");              
        return(1);
        }

static int make_samples()
        {
        int i,j,k,h;

        k=0;
        for (j=0; j<n; ++j)
            for (i=0; i<m; ++i)
                {
                for (h=0; h<f[i+m*j]; ++h)
                    {
                    sample1[k]=i; sample2[k]=j;
                    ++k;
                    }
                }
        return(1);
        }

static int permutoi(int n,int *s)
        {
        int i,k;
        FREQ h;

        for (i=0; i<n; ++i)
            {
            k=n*(*rand1)();
            h=s[i]; s[i]=s[k]; s[k]=h;
            }
        return(1);
        }

static int simul(FREQ total,int n,int *s,double *cum)
        {
        int k,i;
        double a;

        for (k=0; k<total; ++k)
            {
            a=(*rand1)();
            for (i=0; i<n; ++i)
                {
                if (a<=cum[i]) break;
                }
            if (i==n) { sur_print("\n???"); sur_getch(); }
            s[k]=i;
            }
        return(1);
        }

static int f_simul()
        {
        int i,j,h;
        double a;

        h=fi+m*fj;

while (1)
{
        if (sur_kbhit()) { sur_getch(); return(-1); }
        for (i=0; i<m; ++i) for(j=0; j<n; ++j) f[i+m*j]=0;
        total=0;
        while (f[h]<fij)
            {
            ++total; if (total>total0) break;
            a=(*rand1)();
            for (i=0; i<m; ++i)
                {
                if (a<=cum_r[i]) break;
                }
            a=(*rand1)();
            for (j=0; j<n; ++j)
                {
                if (a<=cum_c[j]) break;
                }
            ++f[i+m*j];
            }
if (total==total0) break;
}
        return(1);
        }

static int taulukoi(int *s1,int *s2,FREQ *f)
        {
        int i,k;

        for (i=0; i<m*n; ++i) f[i]=0;
        for (k=0; k<total; ++k) ++f[s1[k]+m*s2[k]];
        return(1);
        }

static int printout()
        {
        output_open(eout);
        disp0();
        tab_disp();
        output_close(eout);
        return(1);
        }

static int read_ftable(char *name,FREQ **f,int *pdim,int *pncvar,int *nc,
           char **varname,char **cname,char *type,int *pndec)
        {
        int i,j,k,j1,j2,apos,j0,ivar,rep,h,h2,n,len;
        unsigned int ncell,cell,ncell2,step;
        char x[LLENGTH], *px[EP4];
        char *p;
        int ncol,nrow;
        int nlabel,nlab;
        int posr[MAXDIM];
        FREQ cvalue;

        j=wfind("TABLE",name,1);
        if (j<0)
            {
            sprintf(sbuf,"\nTABLE %s not found in the edit field!",name);
            sur_print(sbuf); WAIT; return(-1);
            }
        edread(x,j);
        i=split(x+1,px,5);

        if (i>2 && strcmp(px[2],"/")==0) i=2;

        if (i==2)
            {
            *pdim=2;
            i=j; k=0;
            while (1)
                {
                ++i;
                edread(x,i);
                if (i==j+1) n=split(x+1,px,EP4);
                if (strncmp(x+1,space,c2)==0) break;
                ++k;
                }
            ncell=k*n;
            if (*f==NULL)
                *f=(FREQ *)malloc(ncell*sizeof(FREQ));
            else
                *f=(FREQ *)realloc(*f,ncell*sizeof(FREQ));
            if (*f==NULL)
                { not_enough_memory(); return(-1); }

            for (i=0; i<k; ++i)
                {
                edread(x,j+1+i);
                h=split(x+1,px,EP4);
                if (i==0) n=h;
                else
                    {
                    if (h!=n)
                        {
                        sprintf(sbuf,"\nError in table on edit line %d !",i);
                        sur_print(sbuf); WAIT; return(-1);
                        }
                    }
                for (h=0; h<n; ++h) (*f)[i+k*h]=atoi(px[h]);
                }
            nc[1]=k; nc[0]=n;
            varname[0]="C"; varname[1]="R";
            p=text;
            for (i=0; i<n; ++i)
                {
                cname[i]=p;
                sprintf(sbuf,"C%d",i+1);
                strcpy(p,sbuf); p+=strlen(sbuf); *p=EOS; ++p;
                }
            for (i=0; i<k; ++i)
                {
                cname[i+n]=p;
                sprintf(sbuf,"R%d",i+1);
                strcpy(p,sbuf); p+=strlen(sbuf); *p=EOS; ++p;
                }

            return(1);
            }
        if (i<5)
            {
            edread(x,j); i=strlen(x); while (x[i-1]==' ') x[--i]=EOS;
            sprintf(sbuf,"\nInvalid definition: %s",x+1); sur_print(sbuf);
            sprintf(sbuf,"\non line %d",j); sur_print(sbuf);
            sur_print("\nCorrect form: TABLE <name>,L1,L2,<type_of_table>");
            WAIT; return(-1);
            }
        j1=edline2(px[2],1,1); if (j1==0) return(-1);
        j2=edline2(px[3],j1,1); if (j2==0) return(-1);
        strncpy(type,px[4],15); type[15]=EOS;
        ncell=0;
        for (j=j1; j<=j2; ++j)
            {
            edread(x,j);
            p=strchr(x+1,'*'); if (p!=NULL) break;
            }
        if (j>j2)
            {
            sprintf(sbuf,"\nLine of row classifiers ending with *'s missing in table %s!",
                                  name); sur_print(sbuf); WAIT; return(-1);
            }
        apos=p-x;
        j0=j; ncol=j0-j1; k=0;
        for (j=j0+1; j<=j2; ++j)
            {
            edread(x,j);
            i=split(x+apos,px,EP4);
            if (i==0) continue;
            if (k==0) k=i;
            if (i!=k)
                {
                sprintf(sbuf,"\nNumber of elements on line %d conflicts previous lines!",
                                j); sur_print(sbuf);
                WAIT; return(-1);
                }
            ncell+=k;
            }
        if (*f==NULL)
            *f=(FREQ *)malloc(ncell*sizeof(FREQ));
        else
            *f=(FREQ *)realloc(*f,ncell*sizeof(FREQ));
        if (*f==NULL)
            { not_enough_memory(); return(-1); }

        nlabel=0;
        ivar=0; rep=1;
        for (j=j1; j<j0; ++j)
            {
            edread(x,j);
            n=split(x+1,px,EP4);
            if (n<3)
                {
                sprintf(sbuf,"\nError in column classifier on line %d",j);
                sur_print(sbuf); WAIT; return(-1);
                }
            h=(n-1)/rep;
            if (h*rep!=n-1)
                {
                sprintf(sbuf,"\nError in labels on line %d",j); sur_print(sbuf); WAIT; return(-1);
                }
            nc[ivar]=h;
            varname[ivar]=ptext;
            i=store_label(px[0]); if (i<0) return(-1);
            for (h=0; h<nc[ivar]; ++h)
                {
                if (nlabel>=MAXT)
                    {
                    sur_print("\nToo many labels!"); WAIT; return(-1);
                    }
                cname[nlabel++]=ptext;
                i=store_label(px[h+1]);
                }
            rep*=nc[ivar]; ++ivar;
            }

        edread(x,j0);
        i=split(x+1,px,EP4);
        nrow=i-1; ivar=ncol;
        for (i=0; i<nrow; ++i)
            {
            posr[i]=px[i]-x;
            varname[ivar++]=ptext;
            h=store_label(px[i]);
            }
        posr[nrow]=apos;

        ivar=ncol;
        ncell2=k;
        for (i=0; i<nrow; ++i)
            {
            nc[ivar]=0; nlab=nlabel;
            for (j=j0+1; j<=j2; ++j)
                {
                edread(x,j);
                h=posr[i];
                while (x[h]==' ' && h<posr[i+1]) ++h;
                if (h==posr[i+1]) continue;
                h2=h; while (x[h2]!=' ' && h2<posr[i+1]) ++h2;
                x[h2]=EOS;
                for (h2=0; h2<nc[ivar]; ++h2)
                    {
                    if (strcmp(x+h,cname[nlab+h2])==0) break;
                    }
                if (h2<nc[ivar]) continue;
                ++nc[ivar];
                if (nlabel>=MAXT)
                    {
                    sur_print("\nToo many labels!"); WAIT; return(-1);
                    }
                cname[nlabel++]=ptext;
                h2=store_label(x+h); if (h2<0) return(-1);
                }
            ncell2*=nc[ivar];
            ++ivar;
            }
        if (ncell2!=ncell)
            {
            sur_print("\nError in (row) labels!");
            i=0;
            for (ivar=0; ivar<ncol+nrow; ++ivar)
                {
                sprintf(sbuf,"\n%s:",varname[ivar]); sur_print(sbuf);
                for (h=0; h<nc[ivar]; ++h) { sprintf(sbuf," %s",cname[i+h]); sur_print(sbuf); }
                i+=nc[ivar];
                }
            WAIT; return(-1);
            }
        *pdim=ncol+nrow;
        h=0;
        step=ncell/k;
        *pndec=0;
        for (j=j0+1; j<=j2; ++j)
            {
            edread(x,j);
            i=split(x+apos,px,k);
            if (i==0) continue;
            cell=h;
            for (i=0; i<k; ++i)
                {
                len=strlen(px[i])-1;
                if (px[i][len]=='-') { cvalue=MISSING_VALUE; ++missing_values; }
                else if (strcmp(px[i],"*0")==0) cvalue=STRUCTURAL_ZERO;
                else cvalue=atof(px[i]);    /* depends on FREQ */
                (*f)[cell]=cvalue;
                cell+=step;
                p=strchr(px[i],'.');
                if (p!=NULL)
                    {
                    h2=len-(p-px[i]);
                    if (h2>*pndec) *pndec=h2;
                    }
                }
            ++h;
            }
        *pncvar=ncol;
        return(1);
        }

static int store_label(char *s)
        {
        char *p;
        if (ptext-text>TSPACE-strlen(s)-2)
            {
            sur_print("\nNot enough space for labels!"); WAIT; return(-1);
            }
        p=s; while (*p) *ptext++=*p++; *ptext++=EOS;
        return(1);
        }

/***************************
static int ctypes(int *ctype,int dim,int *nc,char **cname)
        {
        int i,j,k,type;

        k=0;
        for (i=0; i<dim; ++i)
            {
            type=1;
            for (j=0; j<nc[i]; ++j)
                if (!muste_isnumber(cname[k++])) type=0;
            ctype[i]=type;
            }
        return(1);
        }
*******************************/
/******************************
static int check_varname_initials(int dim,char **varname)
        {
        int i,k;

        for (i=1; i<dim; ++i)
            {
            for (k=0; k<i; ++k)
                {
                if (*varname[k]!=*varname[i]) continue;
                sprintf(sbuf,"\nSame initials in classifier names %s (#%d) and  %s (#%d)",
                                            varname[k],k+1,varname[i],i+1);
                sur_print(sbuf); WAIT; return(-1);
                }
            }
        return(1);
        }
******************************/
static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(1);
        }

/* tabtomat.c 26.11.1994/SM (26.11.1994)
   converts a two-way table to a matrix file
*/

static double *tt;
static char *rlab,*clab;
static char expr[129];

static int table_to_matrix(char *name,FREQ *f,int m,int n,char **varname,char **cname)
        {
        int i;
        unsigned int k;
/*
        printf("\nm=%d n=%d",m,n); getch();
        printf("\n"); for (i=0; i<2; ++i) printf("%s ",varname[i]);
        printf("\n"); for (i=0; i<m+n; ++i) printf("%s ",cname[i]); getch();
*/
        tt=(double *)malloc(m*n*sizeof(double));
        if (tt==NULL) { not_enough_memory(); return(-1); }
        rlab=malloc(8*m);
        if (rlab==NULL) { not_enough_memory(); return(-1); }
        clab=malloc(8*n);
        if (clab==NULL) { not_enough_memory(); return(-1); }

        for (k=0; k<m*n; ++k)
            {
            tt[k]=f[k];
            }

        lab_copy(m,cname+n,rlab);
        lab_copy(n,cname,clab);
        sprintf(expr,"Table_%s/%s",varname[1],varname[0]);
        i=matrix_save(name,tt,m,n,rlab,clab,8,8,-1,expr,0,0);
        free(tt); free(rlab); free(clab);
        return(i);
        }

static int lab_copy(int m,char **cname,char *lab)
        {
        int i,j,k;
        char ch;

        k=0;
        for (i=0; i<m; ++i)
            {
            for (j=0; j<8; ++j)
                {
                ch=cname[i][j];
                if (ch==EOS) break;
                lab[k++]=ch;
                }
            for (; j<8; ++j) lab[k++]=' ';
            }
        return(1);
        }

/* goodness.c 19.10.2010/SM (19.10.2010)
   goodness of fit test
*/
static int *o;
static double *e,*ecum;
static int n1,n2;
static int count,pcount,dn,dcount,ecount,maxcount;
// double (*rand1)();
extern char **spb;
extern int (*g_print)();
extern double eps;
extern double muste_cdf_chi2();
extern int print_line();
extern double muste_inv_std();
extern double conf_level,conf_coeff;

static double chi_square(int m)
    {
    int i;
    double x2=0.0;
    double a;

    x2=0.0;
    for (i=0; i<m; ++i) { a=(double)o[i]-e[i]; x2+=a*a/e[i]; }
    return(x2);
    }

static int goodness_of_fit_test(FREQ *f,int m,int n)
    {
    int i,j;
    double a,x2;

    if (n!=2)
        {
        sprintf(sbuf,"\nTable must have 2 columns (expected and observed) frquencies!");
        sur_print(sbuf); WAIT; return(-1);
        }
    if (m<2)
        {
        sprintf(sbuf,"\nTable must have at least two rows!");
        sur_print(sbuf); WAIT; return(-1);
        }

    e=(double *)malloc(m*sizeof(double));
    ecum=(double *)malloc(m*sizeof(double));
    o=(int *)malloc(m*sizeof(int));
    n2=0; for (i=0; i<m; ++i) { o[i]=f[i]; n2+=o[i]; }
    n1=0; for (i=0; i<m; ++i) n1+=f[i+m];
    a=(double)n2/(double)n1;

    for (i=0; i<m; ++i) e[i]=a*f[i+m];
    a=0.0; for (i=0; i<m; ++i) { a+=e[i]; ecum[i]=a; }
    for (i=0; i<m; ++i) ecum[i]/=ecum[m-1];

    g_print=&sur_print;

    x2=chi_square(m);
// printf("\nx2=%g n2=%d",x2,n2); getch();

    maxcount=1000000L;
    i=spfind("SIMUMAX");
    if (i>=0) maxcount=atol(spb[i]);
    i=rand_init(); if (i<0) return(-1);

    conf_level=0.95;
    i=spfind("CONF");
    if (i>=0) conf_level=atof(spb[i]);
    if (conf_level<0.8 || conf_level>=1.0)
        {
        sur_print("\nError in CONF=p! Confidence level p must be 0.8<p<1");
        WAIT; return(-1);
        }
    conf_coeff=muste_inv_std(1.0-(1.0-conf_level)/2);

    disp0fit(m,n,x2);

    dn=10000L;
    i=spfind("GAP");
    if (i>=0) dn=atol(spb[i]);
    count=0L; pcount=0L; dcount=0L; ecount=0L;

    while (1)
        {
        ++count; ++dcount;
        for (i=0; i<m; ++i) o[i]=0;
        for (j=0; j<n2; ++j)
            {
            a=(*rand1)();
            i=0;
            while (a>ecum[i]) ++i;
            ++o[i];
            }
// printf("\no: "); for (i=0; i<m; ++i) printf("%d ",o[i]);
// a=chi_square(m); printf("\na=%g",a);
        if (chi_square(m)>=x2) ++pcount;

        if (dcount>=dn)
            {
            tab_disp();
            if (count>=maxcount) break;
            if (sur_kbhit()) { sur_getch(); break; }           
            WAIT;
            dcount=0;
            }

        }
// printf("\np=%g",(double)pcount/(double)count); getch();
    g_print=&print_line;

    output_open(eout);
    disp0fit(m,n,x2);
    tab_disp();
    output_close(eout);

    return(1);
    }

static int disp0fit(int m,int n,double chi2)
        {
        if (r>r3-6) r=1;
        LOCATE(r+2,9);
        sprintf(sbuf,"Goodness of fit test of %d*%d table %s",m,n,word[1]);
        (*g_print)(sbuf);
        LOCATE(r+3,9);
        sprintf(sbuf,"Common Chi-squared test:  X^2=%g P=%6f",
                      chi2+eps,1-muste_cdf_chi2(chi2+eps,(double)(m-1),1e-15));
        (*g_print)(sbuf);


        LOCATE(r+4,9);
        sprintf(sbuf,"Estimating the P value by simulation:");
        (*g_print)(sbuf);
        LOCATE(r+5,9);
        sprintf(sbuf,"         N    P       Confidence interval (%g)",conf_level);
        (*g_print)(sbuf);
        LOCATE(r3+2,9); PR_EBLD;
        sprintf(sbuf,"To interrupt, press any key! (max N is %d)",maxcount);
        sur_print(sbuf);

        return(1);
        }

