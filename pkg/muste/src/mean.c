#include "muste.h"
/* !mean.c 21.2.1986/SM (19.3.1989)
*/

#include <stdio.h>
#include <stdlib.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;
static double *sum;       /* sums of active variables */
static long   *f;         /* frequencies */
static double *w;         /* sums of weigths */

static long n;
static int weight_variable;
static int results_line;




static void not_enough_memory()
        {
        sur_print("\nNot enough memory! (MEAN)");
        WAIT;
        }

static int space_allocation()
        {
        sum=(double *)muste_malloc(d.m_act*sizeof(double));
        if (sum==NULL) { not_enough_memory(); return(-1); }
        f=(long *)muste_malloc(d.m_act*sizeof(long));
        if (f==NULL) { not_enough_memory(); return(-1); }
        w=(double *)muste_malloc(d.m_act*sizeof(double));
        if (w==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }


static void print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        }

static void printout()
        {
        int i;
        char line[LLENGTH];
        char mean[32];

        output_open(eout);
        sprintf(line," Means of variables in %s N=%ld%c",
                          word[1],n,EOS);
        if (weight_variable>=0)
            {
            strcat(line," Weight=");
            strncat(line,d.varname[weight_variable],8);
            }
        print_line(line);
        strcpy(line," Variable     Mean     N(missing)");
        print_line(line);
        for (i=0; i<d.m_act; ++i)
            {
            if (d.v[i]==weight_variable) continue;
            if (w[i]==0.0)
                sprintf(line," %-8.8s            -  %6ld",d.varname[d.v[i]],
                         n-f[i]);
            else
                {
                fnconv(sum[i]/w[i],accuracy+2,mean);
                sprintf(line," %-8.8s %s  %6ld",d.varname[d.v[i]],
                             mean,n-f[i]);
                }
            print_line(line);
            }
        output_close(eout);
        }

static void compute_sums()
        {
        int i;
        long l;

        n=0L;
        for (i=0; i<d.m_act; ++i)
            { f[i]=0L; w[i]=0.0; sum[i]=0.0; }

        sur_print("\n");
        for (l=d.l1; l<=d.l2; ++l)
            {
            double weight;

            if (unsuitable(&d,l)) continue;
            if (weight_variable==-1) weight=1.0;
            else
                {
                data_load(&d,l,weight_variable,&weight);
                if (weight==MISSING8) continue;
                }
            ++n;
            sprintf(sbuf,"%ld ",l); sur_print(sbuf);
            for (i=0; i<d.m_act; ++i)
                {
                double x;

                if (d.v[i]==weight_variable) continue;
                data_load(&d,l,d.v[i],&x);
                if (x==MISSING8) continue;
                ++f[i]; w[i]+=weight; sum[i]+=weight*x;
                }
            }
        }

static int test_scaletypes()
        {
        int i,scale_error;

        scales(&d);
        if (weight_variable>=0)
            {
            if (!scale_ok(&d,weight_variable,RATIO_SCALE))
                {
                sprintf(sbuf,"\nWeight variable %.8s must have ratio scale!",
                          d.varname[weight_variable]); sur_print(sbuf);
                WAIT; if (scale_check==SCALE_INTERRUPT) return(-1);
                }
            }
        scale_error=0;
        for (i=0; i<d.m_act; ++i)
            {
            if (!scale_ok(&d,d.v[i],SCORE_SCALE))
                {
                if (!scale_error)
                    sur_print("\nInvalid scale in variables: ");
                scale_error=1;
                sprintf(sbuf,"%.8s ",d.varname[d.v[i]]); sur_print(sbuf);
                }
            }
        if (scale_error)
            {
            sur_print("\nIn MEAN score scale at least is expected!");
            WAIT; if (scale_check==SCALE_INTERRUPT) return(-1);
            }
        return(1);
        }

/* RS CHA
main(argc,argv)
int argc; char *argv[]; */
void muste_mean(char *argv)
        {
        int i;

/* RS REM
        if (argc==1)
            {
            Rprintf("This program can be used as a SURVO 84C module only.");
            return;
            }
*/
        s_init(argv); // RS CHA argv[1]
        if (g<2)
            {
            init_remarks();
            rem_pr("MEAN <data>,<output_line>        / S.Mustonen 4.3.1989");
            rem_pr("computes means of active variables. Cases can be limited");
            rem_pr("by IND and CASES specifications. The observations can be");
            rem_pr("weighted by a variable activated by 'W'.");
            wait_remarks(2);
            s_end(argv); // RS
            return;
            }
        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) { s_end(argv); return; }
            }
        i=data_open(word[1],&d); if (i<0) { s_end(argv); return; }
        i=sp_init(r1+r-1); if (i<0) { s_end(argv); return; }
        i=mask(&d); if (i<0) { s_end(argv); return; }
        weight_variable=activated(&d,'W');
        i=test_scaletypes(); if (i<0) { s_end(argv); return; }
        i=conditions(&d); if (i<0) { s_end(argv); return; }  /* permitted only once */
        i=space_allocation(); if (i<0) { s_end(argv); return; }
        compute_sums();
        printout();
        muste_free(sum); muste_free(f); muste_free(w);
        data_close(&d);
        s_end(argv); // RS
        }

