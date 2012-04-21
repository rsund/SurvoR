/* !rndtest.c 5.6.1993/SM (19.6.1993)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXLAG 1000
#define MAXRUN 1000

static SURVO_DATA d;

static int var;
static double x,xlag;
static int n;
static int runs_up[MAXRUN],runs_down[MAXRUN];
static int run_start;
static int up;
static double sum1,sum2;
static unsigned int maxlag,lagpos;
static double *lagv,*lagvv,*first_x;
static int maxgap;
static double a_gap,b_gap;
static unsigned int *gap;
static unsigned int gaplen,gapmax;

static int fr_n;
static unsigned int *fr_f;
static double fr_a,fr_d;

static int permlen;
static unsigned int *f_perm;
static unsigned int n_perm;
#define PERM_MAX 7
static double x_perm[PERM_MAX];
static int c_perm[PERM_MAX];
static int i_perm;

static int poklen;      /* POKER=poklen,n_pok,<min=pok_a>,<max=pok_a+(n_pok-1)*pok_d> */
static unsigned int *f_pok;  /* Default: POKER=5,5,0,1 */
            /* n_pok */
static double *s_pok; /* poklen */
static int *f1_pok; /* poklen */
static double pok_a,pok_d;
static int n_pok,i_pok;

static int couplen;    /* COUPON=couplen,coup_max,<min=coup_a>,<max=copu_a+(couplen-1)*copu_d */
static int coup_max;
static unsigned int *f_coup; /* coup_max */
static int coup_found;
static int *coup_ind; /* couplen */
static double *s_coup;
static double coup_a,coup_d;
static int n_coup;

static unsigned int n_sub,i_sub;
static int n_subclass;
static unsigned int *p_mean,*p_max,*p_min;
static double sub_sum1,sub_max,sub_min;

static int results_line;

static int space_allocation();
static int read_data();
static int freq();
static int runs_updown();
static int lags();
static int gaps();
static int permtest();
static int poker();
static int init_coup();
static int coupon();
static int rnd_printout();
static int print_line(char *line);
static int fnconv2(double a,int len,char *s);
static int sub_results();
static int chi2_comp(int n,unsigned int *f,double *pchi2,double *pval);
static int fr_results();
static int runs_hald();
static int runtest2();
static int gap_results();
static int perm_results();
static int poker_results();
static int stirling(int n,double *s);
static int coup_results();

extern double muste_cdf_std();
extern double muste_st_norm();

void muste_rndtest(char *argv)
        {
        int i;
        char xx[LLENGTH],*sana[4];

        s_init(argv);

        if (g<3)
            {
init_remarks();
rem_pr("Usage: RNDTEST <SURVO_data>,<variable>,<output_line>");
rem_pr("This Survo operation makes empirical statistical tests on a series");
rem_pr("numbers supposed to form a random sample from a uniform distribution");
rem_pr("in the interval (0,1).");
rem_pr("Most of these tests are explained in Volume 2 of \"The Art of Programming\"");
rem_pr("by Donald E. Knuth.");
rem_pr("The main application of RNDTEST is testing of various random number");
rem_pr("generators.");
rem_pr("");
rem_pr("A standard set of tests is performed without any extra specification.");
rem_pr("However, If RESULTS=0, no test is performed without explicit specifica-");
rem_pr("tions. To select tests in a more detailed form, following specifications");
rem_pr("can be given.");
rem_pr("");
rem_pr("SUBSAMPLES=<size>,<# of classes>");
rem_pr("The sample is divided systematically in subsamples of given <size>");
rem_pr("and their uniformity is tested by the standard Chi^2-test by divi-");
rem_pr("ding the interval (0,1) in <# of classes>.");
rem_pr("Also tests for mean=0.5 as well for the minimum ans maximum values");
rem_pr("in subsamples are made.");
rem_pr("Default is SUBSAMPLES=0 (i.e. this test is omitted).");
wait_remarks(1);
rem_pr("");
rem_pr("FREQUENCIES=<# of classes>,<lower limit>,<upper limit>");
rem_pr("The uniformity of the total sample is tested by the Chi^2-test.");
rem_pr("Default: FREQUENCIES=10,0,1");
rem_pr("");
rem_pr("MAXLAG=<largest_lag>");
rem_pr("The autocorrelations of the series are computed up to the given");
rem_pr("maximum lag. Default: MAXLAG=10");
rem_pr("");
rem_pr("GAPTEST=<lower_limit>,<upper_limit>,<max.gap>");
rem_pr("The lengths of gaps between occurrences of values in the given range");
rem_pr("are computed.");
rem_pr("Default: GAPTEST=0,0.5,10");
rem_pr("");
rem_pr("PERMTEST=<# of consecutive numbers (3,4,5,6 or 7)>");
rem_pr("Frequencies of different permutations of relative orderings are computed.");
rem_pr("Default: PERMTEST=4");
rem_pr("");
rem_pr("POKER=<# of obs.>,<# of classes>,<lower limit>,<upper_limit>");
rem_pr("Default: POKER=5,5,0,1");
wait_remarks(1);
rem_pr("");
rem_pr("COUPON=<# of classes>,<max_len>,<lower limit>,<upper limit>");
rem_pr("Coupon collector's test");
rem_pr("Default: COUPON=5,20,0,1");
rem_pr("");
rem_pr("Certain run tests are performed in any case.");
wait_remarks(2);
return;
            }

    p_mean=NULL;
    p_min=NULL;
    p_max=NULL;
    fr_f=NULL;
    lagv=NULL;
    lagvv=NULL;
    first_x=NULL;
    gap=NULL;
    f_perm=NULL;
    f_pok=NULL;
    s_pok=NULL;
    f1_pok=NULL;
    f_coup=NULL;
    coup_ind=NULL;
    s_coup=NULL;

        i=spec_init(r1+r-1); if (i<0) return;

        for(i=0; i<MAXRUN; ++i) runs_up[i]=runs_down[i]=0;





        n_sub=0;
        i=spfind("SUBSAMPLES");
        if (i>=0)
            {
            strcpy(xx,spb[i]); i=split(xx,sana,2);
            if (i<1) { sur_print("\nUsage: SUBSAMPLES=<size>,<# of classes>");
                        WAIT; return;
                     }
            n_sub=atoi(sana[0]); n_subclass=10;
            if (i>1) n_subclass=atoi(sana[1]);
            }

        if (results==0) fr_n=0; else { fr_n=10; fr_a=0.0; fr_d=0.1; }
        i=spfind("FREQUENCIES");
        if (i>=0)
            {
            strcpy(xx,spb[i]); i=split(xx,sana,3);
            if (i<3) { sur_print("\nUsage: FREQUENCIES=<# of classes>,<lower_limit>,<upper_limit>");
                        WAIT; return;
                     }
            fr_n=atoi(sana[0]); fr_a=atof(sana[1]);
            fr_d=(atof(sana[2])-fr_a)/(double)fr_n;
            }

        if (results==0) maxlag=0; else maxlag=10;
        i=spfind("MAXLAG"); if (i>=0) maxlag=atoi(spb[i]);

        if (results==0) maxgap=0; else { maxgap=10; a_gap=0; b_gap=0.5; }
        i=spfind("GAPTEST");
        if (i>=0)
            {
            strcpy(xx,spb[i]); i=split(xx,sana,3);
            if (i<2) { sur_print("\nUsage: GAPTEST=<lower_limit>,<upper_limit>,<max.gap>");
                        WAIT; return;
                     }
            a_gap=atof(sana[0]); b_gap=atof(sana[1]);
            maxgap=10; if (i>2) maxgap=atoi(sana[2]);
            }

        if (results==0) permlen=0; else permlen=4;
        i=spfind("PERMTEST");
        if (i>=0)
            {
            permlen=atoi(spb[i]);
            if (permlen && (permlen<3 || permlen>7) )
                {
                sur_print("\nOnly values 0 and 3,4,5,6,7 permitted in PERMTEST");
                WAIT; return;
                }
            }

        if (results==0) poklen=0; else { poklen=5; n_pok=5; pok_a=0.0; pok_d=0.2; }
        i=spfind("POKER");
        if (i>=0)
            {
            strcpy(xx,spb[i]); i=split(xx,sana,4);
            if (i<4) { sur_print("\nUsage: POKER=<# of obs.>,<# of classes>,<l.limit>,<u.limit>");
                        WAIT; return;
                     }
            poklen=atoi(sana[0]);
            n_pok=atoi(sana[1]);
            pok_a=atof(sana[2]);
            pok_d=(atof(sana[3])-pok_a)/(double)n_pok;
            }

        if (results==0) couplen=0; else { couplen=5; coup_max=20; coup_a=0.0; coup_d=0.2; }
        i=spfind("COUPON");
        if (i>=0)
            {
            strcpy(xx,spb[i]); i=split(xx,sana,4);
            if (i<4) { sur_print("\nUsage: COUPON=<# of classes>,<max_len>,<l.limit>,<u.limit>");
                        WAIT; return;
                     }
            couplen=atoi(sana[0]);
            coup_max=atoi(sana[1]);
            coup_a=atof(sana[2]);
            coup_d=(atof(sana[3])-coup_a)/(double)couplen;
            }

        results_line=0;
        if (g>3)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return;
            }
        i=data_read_open(word[1],&d); if (i<0) return;
        var=varfind(&d,word[2]);
        if (var<0) return;
        i=sp_init(r1+r-1);
        if (i<0) { sur_print("\nToo many specifications!"); WAIT; return; }
        i=conditions(&d); if (i<0) return;

        i=space_allocation(); if (i<0) { sur_print("\nNot enough memory!"); return; }

        i=read_data(); if (i<0) return;

        rnd_printout();
        data_close(&d);
        s_end(argv);
        }

static int space_allocation()
        {
        unsigned int u;

        if (n_sub)
            {
            p_mean=(unsigned int *)muste_malloc(n_subclass*sizeof(unsigned int));
            if (p_mean==NULL) return(-1);
            p_min=(unsigned int *)muste_malloc(n_subclass*sizeof(unsigned int));
            if (p_min==NULL) return(-1);
            p_max=(unsigned int *)muste_malloc(n_subclass*sizeof(unsigned int));
            if (p_max==NULL) return(-1);
            }

        if (fr_n)
            {
            fr_f=(unsigned int *)muste_malloc(fr_n*sizeof(unsigned int));
            if (fr_f==NULL) return(-1);
            }

        if (maxlag>8191)
            {
            sur_print("\nMAXLAG=8191 at most!");
            WAIT; return(-1);
            }
        if (maxgap>8191)
            {
            sur_print("\nMaximum gap in GAPTEST is 8191!");
            WAIT; return(-1);
            }
        if (maxlag)
            {
            lagv=(double *)muste_malloc(maxlag*sizeof(double));
            if (lagv==NULL) return(-1);
            lagvv=(double *)muste_malloc(maxlag*sizeof(double));
            if (lagvv==NULL) return(-1);
            first_x=(double *)muste_malloc(maxlag*sizeof(double));
            if (first_x==NULL) return(-1);
            }
        if (maxgap)
            {
            gap=(unsigned int *)muste_malloc(maxgap*sizeof(unsigned int));
            if (gap==NULL) return(-1);
            }

        if (permlen)
            {
            n_perm=1; for (u=2; u<=permlen; ++u) n_perm*=u;
            f_perm=(unsigned int *)muste_malloc((unsigned int)n_perm*sizeof(unsigned int));
            if (f_perm==NULL) return(-1);
            }

        if (poklen)
            {
            f_pok=(unsigned int *)muste_malloc(n_pok*sizeof(unsigned int));
            if (f_pok==NULL) return(-1);
            s_pok=(double *)muste_malloc(poklen*sizeof(double));
            if (s_pok==NULL) return(-1);
            f1_pok=(int *)muste_malloc(poklen*sizeof(int));
            if (f1_pok==NULL) return(-1);
            }

        if (couplen)
            {
            f_coup=(unsigned int *)muste_malloc(coup_max*sizeof(unsigned int));
            if (f_coup==NULL) return(-1);
            coup_ind=(int *)muste_malloc(couplen*sizeof(int));
            if (coup_ind==NULL) return(-1);
            s_coup=(double *)muste_malloc(coup_max*sizeof(double));
            if (s_coup==NULL) return(-1);
            }

        return(1);
        }

static int read_data()
        {
        int i;
        int j;
        int prind;
        int runlen;
        unsigned int u;
        double a;

        n=0L; run_start=0L; up=-1;
        sum1=sum2=0.0;

        if (n_sub)
            {
            i_sub=0; sub_sum1=0.0; sub_min=1e30; sub_max=-1e30;
            for (i=0; i<n_subclass; ++i)
                {
                p_mean[i]=p_min[i]=p_max[i]=0;
                }
            }

        if (fr_n)
            for (i=0; i<fr_n; ++i) fr_f[i]=0L;

        lagpos=0;
        for (i=0; i<maxlag; ++i) lagv[i]=lagvv[i]=0.0;
        for (i=0; i<maxgap; ++i) gap[i]=0L;
        gaplen=gapmax=0L;
        if (permlen)
            {
            i_perm=0;
            for (u=0; u<n_perm; ++u) f_perm[u]=0L;
            }
        if (poklen)
            {
            i_pok=0;
            for (u=0; u<n_pok; ++u) f_pok[u]=0L;
            for (u=0; u<poklen; ++u) f1_pok[u]=0;
            }
        if (couplen)
            {
            init_coup();
            for (u=0; u<coup_max; ++u) f_coup[u]=0L;
            }
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]); else prind=0;
        sur_print("\n");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
/***********************
            if (sur_kbhit()) { sur_getch(); prind=1-prind; }
************************/
            if (prind)
                {
                sprintf(sbuf,"%d ",j); sur_print(sbuf);
                }

            data_load(&d,j,var,&x);
            if (x==MISSING8) continue;
            ++n;
            sum1+=x; sum2+=x*x;
            if (n_sub)
                {
                sub_sum1+=x;
                if (x<sub_min) sub_min=x;
                if (x>sub_max) sub_max=x;
                ++i_sub;
                if (i_sub==n_sub)
                    {
                    sub_sum1/=(double)n_sub;
                    a=muste_cdf_std(sqrt(12.0*(double)n_sub)*(sub_sum1-0.5));
                    i=a*(double)n_subclass;
                    ++p_mean[i];

                    i=pow(1.0-sub_min,(double)n_sub)*(double)n_subclass;
                    ++p_min[i];
                    i=pow(sub_max,(double)n_sub)*(double)n_subclass;
                    ++p_max[i];

                    i_sub=0; sub_sum1=0.0; sub_min=1e30; sub_max=-1e30;
                    }
                }
            if (fr_n) freq();
            runs_updown();
            lags();
            if (maxgap) gaps();
            if (permlen) permtest();
            if (poklen) poker();
            if (couplen) coupon();
            xlag=x;
            }
        runlen=n-run_start;
        if (runlen>MAXRUN) runlen=MAXRUN-1;
        if (up) ++runs_up[(int)(runlen-1)];
        else    ++runs_down[(int)(runlen-1)];
        if (maxgap)
            {
            if (gaplen>gapmax) gapmax=gaplen;
            if (gaplen>(unsigned int)(maxgap-1)) gaplen=maxgap-1;
            ++gap[(int)gaplen];
            }
        return(1);
        }

static int freq()
        {
        int i;

        i=(x-fr_a)/fr_d;
        if (i<0 || i>fr_n-1)
            {
            sprintf(sbuf,"\nValue %g not in permitted range (%g,%g) in frequency test!",
                                   x, fr_a, fr_a+fr_n*fr_d);
            fr_n=0; WAIT; return(-1);
            }
        ++fr_f[i];
        return(1);
        }

static int runs_updown()
        {
        int runlen;

        if (run_start==0L)
            {
            run_start=n;
            return(1);
            }
        if (up==-1)
            {
            if (x>=xlag) up=1; else up=0;
            return(1);
            }
        if (up)
            {
            if (x>=xlag) return(1);
            runlen=n-run_start-1;
/*  Rprintf("\nUP:   %d",runlen); getch();        */
            if (runlen>MAXRUN) runlen=MAXRUN-1;
            ++runs_up[(int)(runlen-1)];
            run_start=n-1L; up=0;
            return(1);
            }
        else
            {
            if (x<=xlag) return(1);
            runlen=n-run_start-1;
/*  Rprintf("\nDown: %d",runlen); getch();         */
            if (runlen>MAXRUN) runlen=MAXRUN-1;
            ++runs_down[(int)(runlen-1)];
            run_start=n-1L; up=1;
            return(1);
            }
        }

static int lags()
        {
        unsigned int i;

        for (i=0; i<maxlag; ++i)
            {
            if (n<=(int)(i+1))
                {
                first_x[(int)(n-1L)]=x;
                break;
                }
            if (lagpos>=i) lagvv[i]+=x*lagv[lagpos-i];
            else lagvv[i]+=x*lagv[maxlag-i+lagpos];
            }
        ++lagpos; if (lagpos==maxlag) lagpos=0;
        lagv[lagpos]=x;
        return(1);
        }

static int gaps()
        {
        if (x<a_gap || x>b_gap) { ++gaplen; return(1); }
        if (gaplen>gapmax) gapmax=gaplen;
        if (gaplen>(unsigned int)(maxgap-1)) gaplen=maxgap-1;
        ++gap[(int)gaplen];
        gaplen=0L;
        return(1);
        }

static int permtest()
        {
        int i,r,imax;
        double max;
        unsigned int f,v;

        x_perm[i_perm]=x;
        ++i_perm;
        if (i_perm<permlen) return(1);
        i_perm=0;

        for (r=permlen-1; r>=0; --r)
            {
            max=x_perm[0]; imax=0;
            for (i=1; i<=r; ++i)
                if (x_perm[i]>max) { max=x_perm[i]; imax=i; }
            c_perm[r]=imax;
            x_perm[imax]=x_perm[r]; x_perm[r]=max;
            }
        v=1; f=0;
        for (r=permlen-1; r>=0; --r)
            {
            f+=v*c_perm[r];
            v*=r+1;
            }
        ++f_perm[f];
        return(1);
        }

static int poker()
        {
        int i,k;

        i=(x-pok_a)/pok_d;
        if (i<0 || i>n_pok-1)
            {
            sprintf(sbuf,"\nValue %g not in permitted range (%g,%g) in Poker test!",
                                   x, pok_a, pok_a+n_pok*pok_d);
            poklen=0; WAIT; return(-1);
            }
        ++f1_pok[i];
        ++i_pok;
        if (i_pok<poklen) return(1);
        i_pok=0;
        k=0; for (i=0; i<poklen; ++i) { if (f1_pok[i]>0) ++k; f1_pok[i]=0; }
        ++f_pok[k-1];
        return(1);
        }

static int init_coup()
        {
        int u;

        coup_found=0; n_coup=0;
        for (u=0; u<couplen; ++u) coup_ind[u]=0;
        return(1);
        }

static int coupon()
        {
        int i;

        ++n_coup;
        i=(x-coup_a)/coup_d;
        if (i<0 || i>couplen-1)
            {
            sprintf(sbuf,"\nValue %g not in permitted range (%g,%g) in Coupon collector's test!",
                                   x, coup_a, coup_a+couplen*coup_d);
            couplen=0; WAIT; return(-1);
            }
        if (coup_ind[i]==1)
            {
            if (n_coup>=coup_max)
                {
                ++f_coup[coup_max-1];
                init_coup();
                }
            return(1);
            }
        coup_ind[i]=1;
        ++coup_found;
        if (coup_found==couplen)
            {
            ++f_coup[n_coup-1]; init_coup();
            }
        return(1);
        }

/* rndtest2.c 11.6.1993/SM (26.6.1993)
*/

extern double muste_cdf_std();
extern double muste_cdf_chi2();

static int imax;
static double e_sub;

static int rnd_printout()
        {
        int i,k;
        double a,a1,a2,a3,sumx,sumx2,sumy,sumy2,df;
        double mean,stddev,amax;
        char s1[LLENGTH],s2[LLENGTH],s3[LLENGTH];


        if (n<3L) { sprintf(sbuf,"\nOnly %d active observations!",n);
                    sur_print(sbuf); WAIT; return(-1);
                  }
        output_open(eout);
        mean=sum1/(double)n;
        stddev=sqrt((sum2-sum1*sum1/(double)n)/(double)(n-1));
        sprintf(sbuf,"Testing randomness of %s in data %s",word[2],word[1]);
        print_line(sbuf);
        fnconv2(mean,accuracy+2,s1); fnconv2(stddev,accuracy+2,s2);
        fnconv2(stddev/sqrt((double)n),accuracy+2,s3);
        sprintf(sbuf,"N=%d mean=%s stddev=%s SE[mean]=%s",n,s1,s2,s3); print_line(sbuf);
        if (stddev<1e-15) { sprintf(sbuf,"\nVariable %s is constant=%g",
                            word[2],mean); sur_print(sbuf); WAIT; return(-1);
                           }
        if (n_sub) sub_results();
        if (fr_n) fr_results();
        for (imax=MAXRUN-1; imax>=0; --imax)
            {
            if (runs_up[imax]+runs_down[imax]>0) break;
            }
        runs_hald();
        runtest2();

        if (maxlag)
            {
            amax=0.0;
            a=sum1*sum1/(double)n;
            a1=sum2-a;
            print_line(" ");
            print_line("   Lag    Autocorrelation");
            sumx=sumy=sum1; sumx2=sumy2=sum2;
            for (i=0; i<maxlag; ++i)
                {
                a=first_x[i];
                sumx-=a; sumx2-=a*a;
                a=lagv[lagpos];
                sumy-=a; sumy2-=a*a;
                if (lagpos>0) --lagpos; else lagpos=maxlag-1;
                df=n-(int)(i+1);
                a=lagvv[i]-sumx*sumy/df;
                a1=sumx2-sumx*sumx/df; a2=sumy2-sumy*sumy/df;
                a2=a/sqrt(a1*a2);
                if (fabs(a2)>amax) amax=fabs(a2);
                a3=2*muste_cdf_std(-fabs(a2)*sqrt((double)n));
                fnconv(a2,accuracy,s1);
/*  Rprintf("\nlag=%d sumx=%g sumx2=%g sumy=%g sumy2=%g S=%g",
                i+1,sumx,sumx2,sumy,sumy2,lagvv[i]); getch();
*/
                k=sprintf(sbuf,"%6d    %s",i+1,s1);
                if (a3<0.1)
                    {
                    k+=sprintf(sbuf+k," P=%g",a3);
                    }
                if (i==maxlag-1) { fnconv2(amax,accuracy,s1); k+=sprintf(sbuf+k,"  max.autocorr.=%s",s1); }
                print_line(sbuf);
                }
            }
        if (maxgap) gap_results();
        if (permlen) perm_results();
        if (poklen) poker_results();
        if (couplen) coup_results();
        output_close(eout);
        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

static int fnconv2(double a,int len,char *s)
        {
        char t[LLENGTH];
        char *p,*q;

        fnconv(a,len,t);
        p=t; while (*p==' ') ++p;
        q=s; while (*p!=EOS) *q++=*p++;
        *q=EOS;
        return(1);
        }

static int sub_results()
        {
        int i;
        unsigned int n1;
        double chi2,pval;
        char s1[LLENGTH],s2[LLENGTH];
        unsigned int sum;

        n1=n/n_sub;
        sum=0L;
        for (i=0; i<n_subclass; ++i) sum+=p_mean[i];
        e_sub=(double)sum/(double)n_subclass;
        print_line(" ");
        sprintf(sbuf,"Subsample statistics: samplesize=%u Number of samples =%u",
                            n_sub, n1);
        print_line(sbuf);
        print_line("P values in repeated tests should be uniformly distributed over (0,1)");
        fnconv2(e_sub,accuracy,s1);
        sprintf(sbuf," P (up.limit)  Mean         Min           Max   (Expected=%s)",
                         s1); print_line(sbuf);
        for (i=0; i<n_subclass; ++i)
            {
            sprintf(sbuf,"%5.2f    %8u      %8u      %8u",
              (double)(i+1)/(double)n_subclass, p_mean[i], p_min[i],p_max[i]);
            print_line(sbuf);
            }
        chi2_comp(n_subclass,p_mean,&chi2,&pval);
        fnconv2(chi2,accuracy,s1);
        fnconv2(pval,accuracy,s2);
        sprintf(sbuf,"Tests for mean=0.5: Chi2=%s df=%d P[submean]=%s",
                                       s1,n_subclass-1,s2);
        print_line(sbuf);
        chi2_comp(n_subclass,p_min,&chi2,&pval);
        fnconv2(chi2,accuracy,s1);
        fnconv2(pval,accuracy,s2);
        sprintf(sbuf,"Tests for min.values: Chi2=%s df=%d P[submin]=%s",
                                       s1,n_subclass-1,s2);
        print_line(sbuf);
        chi2_comp(n_subclass,p_max,&chi2,&pval);
        fnconv2(chi2,accuracy,s1);
        fnconv2(pval,accuracy,s2);
        sprintf(sbuf,"Tests for max.values: Chi2=%s df=%d P[submax]=%s",
                                       s1,n_subclass-1,s2);
        print_line(sbuf);
        return(1);
        }

static int chi2_comp(int n,unsigned int *f,double *pchi2,double *pval)
        {
        int i;
        double a;

        *pchi2=0.0;
        for (i=0; i<n; ++i)
            {
            a=f[i]-e_sub; *pchi2+=a*a/e_sub;
            }
        *pval=1.0-muste_cdf_chi2(*pchi2,(double)(n-1),1e-15);
        return(1);
        }


static int fr_results()
        {
        int i;
        double e,a,chi2;
        unsigned int f,min,max;
        char s1[LLENGTH],s2[LLENGTH],s3[LLENGTH];

        print_line(" ");
        sprintf(sbuf,"Frequency test: N[class]=%d Interval (%g,%g)",
                                   fr_n,fr_a,fr_a+fr_n*fr_d);
        print_line(sbuf);
        chi2=0.0; min=max=fr_f[0]; e=(double)n/(double)fr_n;
        for (i=0; i<fr_n; ++i)
            {
            f=fr_f[i];
            if (f<min) min=f;
            if (f>max) max=f;
            a=f-e; chi2+=a*a/e;
            }
        fnconv2(e,accuracy,s1);
        fnconv2(chi2,accuracy,s2);
        fnconv2(1.0-muste_cdf_chi2(chi2,(double)(fr_n-1),1e-15),accuracy,s3);
        sprintf(sbuf,"Expected=%s min=%u max=%u chi2=%s df=%d P[freq]=%s",
                         s1,min,max,s2,fr_n-1,s3);
        print_line(sbuf);
        return(1);
        }

static int runs_hald()
        {
        int i,ii,k;
        double fact,a;
        unsigned int rtot;

        print_line(" ");
        sprintf(sbuf,"Runs up and down (Hald 1952):"); print_line(sbuf);
        print_line("Length    Runs up    Runs down   Total   Expected");
        fact=6.0;
        rtot=0L;
        for (i=0; i<=imax; ++i)
            {
            ii=i+1;
            fact*=(double)(ii+3);
            a=2*((double)n*(double)(ii*ii+3*ii+1)-(ii*ii*ii-3*ii*ii-ii-4))/fact;
            k=sprintf(sbuf,"%6d  %8d %8d    %8d     %g",i+1,runs_up[i],runs_down[i],
                           runs_up[i]+runs_down[i],a);
            if (i==imax)
                {
                a=((double)n*(double)(ii+1)-(double)(ii*ii+ii-1))/fact*(double)(2*ii+2);
                a=1-exp(-a);
                sprintf(sbuf+k,"  P(#R%d>=1)=%g",ii,a);
                }
            print_line(sbuf);
            rtot+=runs_up[i]+runs_down[i];
            }
        sprintf(sbuf,"Total number of runs R=%d E[R]=%g S[R]=%g",rtot,
                 (double)((2*n-1)/3),sqrt((double)((16*n-29)/90)) );
        print_line(sbuf);
        return(1);
        }

double aa[]= {
           4529.4,  9044.9, 13568.0, 18091.0, 22615.0, 27892.0,
           9044.9, 18097.0, 27139.0, 36187.0, 45234.0, 55789.0,
          13568.0, 27139.0, 40721.0, 54281.0, 67852.0, 83685.0,
          18091.0, 36187.0, 54281.0, 72414.0, 90470.0, 111580.0,
          22615.0, 45234.0, 67852.0, 90470.0, 113262.0, 139476.0,
          27892.0, 55789.0, 83685.0, 111580.0, 139476.0, 172860.0
             };

double bb[]= { 1.0/6.0, 5.0/24.0, 11.0/120.0, 19.0/720.0, 29.0/5040.0, 1.0/840.0 };

static int runtest2()
        {
        unsigned int up2[6],dn2[6];
        unsigned int li;
        int i,ii,j;
        double a,v_up,v_dn;
        double fact;


        for (i=1; i<5; ++i)
            up2[i]=runs_up[i-1];
        li=0L;
        for (i=4; i<=imax; ++i)
            li+=runs_up[i];
        up2[5]=li;
        li=0; for (i=1; i<=imax; ++i) li+=(int)i*runs_down[i];
        up2[0]=li;
        v_up=0.0;
        for (i=0; i<6; ++i)
            for (j=0; j<6; ++j)
               v_up+=(up2[i]-(double)n*bb[i])*(up2[j]-(double)n*bb[j])*aa[i+6*j];
        v_up/=(double)n;
        for (i=1; i<5; ++i)
            dn2[i]=runs_down[i-1];
        li=0L;
        for (i=4; i<=imax; ++i)
            li+=runs_down[i];
        dn2[5]=li;
        li=0; for (i=1; i<=imax; ++i) li+=(int)i*runs_up[i];
        dn2[0]=li;
        v_dn=0.0;
        for (i=0; i<6; ++i)
            for (j=0; j<6; ++j)
               v_dn+=(dn2[i]-(double)n*bb[i])*(dn2[j]-(double)n*bb[j])*aa[i+6*j];
        v_dn/=(double)n;

        sprintf(sbuf,"Runs up and down (Levene and Wolfowitz 1944):"); print_line(sbuf);
        print_line("Length    Runs up    Runs down   Expected");
        fact=1.0;
        for (i=0; i<=5; ++i)
            {
            ii=i+1;
            fact*=(double)ii;
            a=(double)(n+1)*(double)ii/((double)(ii+1)*fact)-(double)(ii-1)/fact
             -(double)(n+1)*(double)(ii+1)/((double)(ii+2)*(double)(ii+1)*fact)-(double)(ii)/fact/(double)(ii+1);
            sprintf(sbuf,"%6d  %8d %8d          %g",i+1,up2[i],dn2[i],a);
            print_line(sbuf);
            }
        sprintf(sbuf,"Tests: V[up]=%g P[up]=%g    V[down]=%g P[down]=%g",
               v_up, 1-muste_cdf_chi2(v_up,6.0,1e-15), v_dn, 1-muste_cdf_chi2(v_dn,6.0,2e-15));
        print_line(sbuf);
        if (n<4000L)
            {
            sprintf(sbuf,"Since N=%u is less than 4000, the tests above are not valid!",n);
            print_line(sbuf);
            }
        return(1);
        }

static int gap_results()
        {
        int i,max,k,stop;
        double dp,a;
        unsigned int n2;
        char s1[LLENGTH],s2[LLENGTH],s3[LLENGTH];
        double e,chi2,schi2;
        int df;


        for (i=maxgap-1; i>=0; --i) if (gap[i]>0L) break;
        max=i;
        n2=0L; for (i=0; i<=max; ++i) n2+=gap[i];
        dp=(double)(n-n2)/(double)n;
        fconv(a_gap,"",s1); fconv(b_gap,"",s2);
        fnconv2(1.0-dp,accuracy,s3);
        print_line(" ");
        sprintf(sbuf,"Gap test: P(%s,%s)=%s",s1,s2,s3); print_line(sbuf);
        print_line("Length    Frequency    Expected     Chi2");
        e=(1.0-dp)*(double)n2; schi2=0.0; df=-2;  /* 26.6.93 */
        stop=0;
        for (i=0; i<=max; ++i)
            {
            if (e<5.0 || i==max)
                {
                e/=(double)(1.0-dp);
                for (k=i+1; k<=max; ++k) gap[i]+=gap[k];
                stop=1;
                }
            a=gap[i]-e; chi2=a*a/e;
            schi2+=chi2; ++df;
            fnconv(e,accuracy,s1); fnconv(chi2,accuracy,s2);
            sprintf(sbuf,"%6d  %8u      %s      %s",
                          i, gap[i], s1, s2);
            if (stop) sbuf[7]='-';
            print_line(sbuf);
            if (stop) break;
            e*=dp;
            }
        fnconv2(schi2,accuracy,s1);
        fnconv2(1.0-muste_cdf_chi2(schi2,(double)df,1e-15),accuracy,s2);
        sprintf(sbuf,"Chi2=%s df=%d P[gap]=%s    Max.gap=%u",s1,df,s2,gapmax);
        print_line(sbuf);
        return(1);
        }

static int perm_results()
        {
        unsigned int f;
        double e,chi2,a;
        char s1[LLENGTH],s2[LLENGTH];

        e=(double)(unsigned int)(n/(unsigned int)permlen)/(double)n_perm;

        chi2=0.0;
        for (f=0; f<n_perm; ++f)
            { a=(double)f_perm[f]-e; chi2+=a*a/e; }
        print_line(" ");
        fnconv2(chi2,accuracy,s1);
        fnconv2(1.0-muste_cdf_chi2(chi2,(double)(n_perm-1),1e-15),accuracy,s2);
        sprintf(sbuf,"Permutation test: Length=%d Chi2=%s df=%u P[perm]=%s",
                         permlen, s1, n_perm-1, s2);
        print_line(sbuf);
        return(1);
        }

static int poker_results()
        {
        int r;
        double chi2,a,b,e;
        double f2,e2;
        int df;
        char s1[LLENGTH],s2[LLENGTH];
                                 /* d=n_pok k=poklen */
        stirling(poklen,s_pok);
        chi2=0.0;
        b=(double)n/(double)poklen/pow((double)n_pok,(double)poklen);
        print_line(" ");
        sprintf(sbuf,"Poker test: Different values r in sequences of %d.",poklen);
        print_line(sbuf);
        print_line("     r    Frequency   Expected");
        f2=e2=0.0; df=-1;
        for (r=1; r<=n_pok; ++r)
            {
            b*=(double)(n_pok-r+1);
            e=b*s_pok[r-1];
            fconv(e,"",s1);
            sprintf(sbuf,"%6d   %8u        %s",r,f_pok[r-1],s1);
            print_line(sbuf);
            e2+=e; f2+=f_pok[r-1];
            if (e2<5.0 && r<n_pok) continue;
            a=f2-e2;
            chi2+=a*a/e2; ++df;
            e2=f2=0.0;
            }
        fnconv2(chi2,accuracy,s1);
        fnconv2(1.0-muste_cdf_chi2(chi2,(double)df,1e-15),accuracy,s2);
        sprintf(sbuf,"Chi2=%s df=%d P[poker]=%s",s1,df,s2);
        print_line(sbuf);
        return(1);
        }

static int stirling(int n,double *s)
        {
        int k,r;
        double s0,s1;

        for (r=0; r<n; ++r) s[r]=0.0;
        s[0]=1.0;
        s[1]=1.0;
        for (k=3; k<=n; ++k)
            {
            s0=1.0;
            for (r=2; r<=k; ++r)
                {
                s1=s0+(double)r*s[r-1];
                s0=s[r-1]; s[r-1]=s1;
                }
            }
/*
printf("\nStirling:");
   for (r=0; r<n; ++r) Rprintf(" %u",s[r]);  getch();
*/
        return(1);
        }

static int coup_results()
        {
        int i,r;
        double chi2,a,b,e;
        double f2,e2;
        int df;
        unsigned int nc;
        char s1[LLENGTH],s2[LLENGTH];

                          /* d=couplen t=coupmax */
        b=1.0;
        for (r=2; r<couplen; ++r) b*=(double)r/(double)couplen;
        nc=0L; for (i=couplen-1; i<coup_max; ++i) nc+=f_coup[i];

        print_line(" ");
        sprintf(sbuf,"Coupon collector's test: Waiting time r for a complete series of %d alternatives.",couplen);
        print_line(sbuf);
        print_line("     r    Frequency   Expected");
        f2=e2=0.0; df=-1; chi2=0.0;
        for (r=couplen; r<coup_max; ++r)
            {
            b/=(double)couplen;
            stirling(r-1,s_coup);
            e=(double)nc*b*s_coup[couplen-2];
            fconv(e,"",s1);
            sprintf(sbuf,"%6d   %8u     %s",r,f_coup[r-1],s1);
            print_line(sbuf);
            e2+=e; f2+=f_coup[r-1];
            if (e2<5.0) continue;
            a=f2-e2; chi2+=a*a/e2; ++df;
            e2=f2=0.0;
            }

        stirling(coup_max-1,s_coup);
        e=(double)nc*(1.0-b*s_coup[couplen-1]);
        fconv(e,"",s1);
        sprintf(sbuf,"%6d-  %8u   %s",coup_max,f_coup[coup_max-1],s1);
        print_line(sbuf);
        e2+=e; f2+=f_coup[coup_max-1];
        a=f2-e2; chi2+=a*a/e2; ++df;
        fnconv2(chi2,accuracy,s1);
        fnconv2(1.0-muste_cdf_chi2(chi2,(double)df,1e-15),accuracy,s2);
        sprintf(sbuf,"Chi2=%s df=%d P[coupon]=%s",s1,df,s2);
        print_line(sbuf);
        return(1);
        }

