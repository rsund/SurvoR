/* !classi 18.4.1994/SM (23.4.1994) (26.6.1997)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAX_G 256

static SURVO_DATA d;
static FILE *matfile;
static char corr_names[LLENGTH];
static char msn_names[LLENGTH];
static char *corr_name[MAX_G];
static char *msn_name[MAX_G];
static int gg; /* # of groups */
static int p;
static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];
// static char *rlab2,*clab2;
static double *msn;
static double *mean[MAX_G];
static double *w[MAX_G];
static double *inv;
static double *det;
static double *wtot;
static double dettot;
static int *nk;
static int ntot;
static double *coeff;
static double *prior;
static char **pg; /* pointers to priors */
static double *xg;
static int p1;   /* # of orig,vars */
static int *v;   /* indices of vars */
static int coeff_given;


static int var_mahal1;   /* D Mahalanobis, equal covariance matrices */
static int var_mahal2;   /* d Mahalanobis, unequal covariance matrices */
static int var_bayes1;   /* B Bayes */
static int var_bayes2;   /* b Bayes */
static int ok1,ok2;
static int *var_post;
static int post_type; /* 1=D 2=d 3=B 4=b */

static double *xx,*yy,*x2;
static double *d21,*d22;

static int select_output_vars();
static int matrix_names();
static int mat_dim(char *name,int *pm,int *pn);
static int cla_mat_name(char *name,char *pathname);
static int space_allocation();
static int select_var_post();
static int read_matrices();
static int select_active_vars();
static int priors();
static int classify();
static int select_group(double *x,int minmax);
static double mahal(double *x,double *s,int m);
static int save_post(double *x,int l,int norm,int miss);
static int invert(double *A,int m,double *pdet);
static int compute_wtot();
static int poimi(char *nimi,char *lab,int l,int i);
static int not_enough_memory();
static void op_mahal();
static int varaa_tilat();
static int ei_tilaa();
static int laske_momentit();
static int laske_mahal();
static int tulostus();
static int print_line(char *line);
static char *spois(char *s);


/***************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "CORR", "MSN", "COEFF", "PRIORS", "!" };
char **specs=specs0;
***************************/
static int prind=0;

/*********
main(argc,argv)
int argc; char *argv[];
*****************/

void muste_classi(char *argv)
        {
        int i,k;

// RS ADD variable init
matfile=NULL;
rlab=NULL;
clab=NULL;
msn=NULL;
inv=NULL;
det=NULL;
wtot=NULL;
nk=NULL;
coeff=NULL;
prior=NULL;
xg=NULL;
v=NULL;
var_post=NULL;
xx=NULL;
yy=NULL;
x2=NULL;
d21=NULL;
d22=NULL;
prind=0;
gg=0;

        s_init(argv);
/*Rprintf("info=%s\n",info); getch(); */
        if (muste_strcmpi(word[0],"MAHAL")==0) { op_mahal(); s_end(argv); return; }

        if (g<2)
            {
            init_remarks();
            rem_pr("CLASSI <data>                        / S.Mustonen 18.4.1994");
            rem_pr("classifies observations in Survo <data> to g groups        ");
            rem_pr("according to Mahalanobis distances and derived measures.   ");
            rem_pr("The groups are defined by CORR and NSN specifications      ");
            rem_pr("of the form                                                ");
            rem_pr("   CORR=CORR1,CORR2,...,CORRg                              ");
            rem_pr("   MSN=MSN1,NSN2,...,MSNg                                  ");
            rem_pr("giving the correlation matrices and matrices of means and  ");
            rem_pr("standard deviations. These matrices are usually computed   ");
            rem_pr("by the CORR operation for g different groups with same     ");
            rem_pr("variables and transformed into corresponding matrices of   ");
            rem_pr("canonical discriminant functions (discriminators) with     ");
            rem_pr("lower dimensions by the /DISCRI (sucro) operation.         ");
            rem_pr("When discriminators are used as the basis for classifi-    ");
            rem_pr("cation (this is strongly recommended), the specification   ");
            rem_pr("   COEFF=DISCRL.M                                          ");
            rem_pr("must be included since these coefficients transform the    ");
            rem_pr("original variables into discriminant scores.               ");

            wait_remarks(1);

            rem_pr("The classification is based either on Mahalanobis distan-  ");
            rem_pr("ces or Bayes probabilities (assuming that the samples are  ");
            rem_pr("multivariate normal). The classification rules are         ");
            rem_pr("selected by activating variables in <data> as follows:     ");
            rem_pr("  D = Mahalanobis distances, equal covariances             ");
            rem_pr("  d = Mahalanobis distances, unequal covariances           ");
            rem_pr("  B = Bayes probabilities, equal covariances               ");
            rem_pr("  b = Bayes probabilities, unequal covariances             ");
            rem_pr("                                                           ");
            rem_pr("In B and b alternatives, numbers proportional to prior     ");
            rem_pr("probabilities are give by a specification                  ");
            rem_pr("  PRIORS=P1,P2,...,Pg .                                    ");
            rem_pr("Default is PRIORS=N1,N2,...,Ng where Nk is # of observa-   ");
            rem_pr("tions in the k'th group (taken from the MSN file).         ");
            rem_pr("                                                           ");
            rem_pr("In alternative B, posterior measures used in               ");
            rem_pr("classification are computed as                             ");
            rem_pr("  Pk*exp(-0.5*Dk^2)                                        ");
            rem_pr("where Dk^2 is the (squared) Mahalanobis distance.          ");
            rem_pr("In alternative b, the corresponding measure is             ");
            rem_pr("  Pk*exp(-0.5*Dk^2)/sqrt(det(Sk))                          ");
            rem_pr("where Sk is the covariance matrix in group k.              ");

            wait_remarks(1);

            rem_pr("All above rules can be used simultaneously by indicating   ");
            rem_pr("unique D,d,B,b variables.                                  ");
            rem_pr("Also posterior probabilities (or distances in cases D,d)   ");
            rem_pr("can be saved in g variables activated by P's.              ");
            rem_pr("This, however, is possible only for one of the alternatives");
            rem_pr("at a time. The precedence order is b,B,d,D.                ");

            wait_remarks(2);
            s_end(argv[1]);
            return;
            }
        i=data_open(word[1],&d); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;
        i=select_output_vars(); if (i<0) return;
        i=matrix_names(); if (i<0) return;
        i=mat_dim(msn_name[0],&p,&k); if (i<0) return;
        i=space_allocation(); if (i<0) return;
        i=select_var_post(); if (i<0) return;
        i=read_matrices(); if (i<0) return;
        i=select_active_vars(); if (i<0) return;
        i=priors(); if (i<0) return;
        i=classify(); if (i<0) return;
        data_close(&d);
        s_end(argv);
        }

static int select_output_vars()
        {

        var_mahal1=activated(&d,'D');
        var_mahal2=activated(&d,'d');
        var_bayes1=activated(&d,'B');
        var_bayes2=activated(&d,'b');

        if (var_mahal1>=0) post_type=1;
        if (var_mahal2>=0) post_type=2;
        if (var_bayes1>=0) post_type=3;
        if (var_bayes2>=0) post_type=4;

        ok1=ok2=0;
        if (var_mahal2>=0 || var_bayes2>=0)
            ok2=1;
        if (var_mahal1>=0 || var_bayes1>=0)
            ok1=1;

        if (!ok1 && !ok2)
            {
            sur_print("\nNo group predicting variables (activated by D d B b) given!");
            WAIT; return(-1);
            }
        return(1);
        }

static int matrix_names()
        {
        int i;

        i=spfind("CORR");
        if (i<0)
            {
            sur_print("\nCORR specification (list of correlation matrices) missing!");
            WAIT; return(-1);
            }
        strcpy(corr_names,spb[i]);
        gg=split(corr_names,corr_name,MAX_G);

        if (gg<2)
            {
            sur_print("\nLess than 2 groups! (CORR specification)");
            WAIT; return(-1);
            }

        i=spfind("MSN");
        if (i<0)
            {
            sur_print("\nMSN specification (list of MSN matrices) missing!");
            WAIT; return(-1);
            }

        strcpy(msn_names,spb[i]);
        i=split(msn_names,msn_name,MAX_G);
        if (i!=gg)
            {
            sprintf(sbuf,"\n# of MSN matrices (%d) does not equal to # (%d) of CORR matrices!",
                                               i,gg);
            sur_print(sbuf);
            WAIT; return(-1);
            }
        return(1);
        }

static int mat_dim(char *name,int *pm,int *pn)
        {
        char x[LLENGTH];

        cla_mat_name(name,x);
        matfile=muste_fopen(x,"rb");
        if (matfile==NULL)
            {
            sprintf(sbuf,"\nCannot find matrix file %s",x);
            sur_print(sbuf);
            WAIT; return(-1);
            }
        fscanf(matfile,"%s %d %d",x,pm,pn);
        muste_fclose(matfile);
        return(1);
        }

static int cla_mat_name(char *name,char *pathname)
        {
        char *p;

        strcpy(pathname,name);
        p=strchr(pathname,':');
        if (p==NULL) { strcpy(pathname,edisk); strcat(pathname,name); }
        p=strchr(pathname,'.');
        if (p==NULL) strcat(pathname,".MAT");
        return(1);
        }

static int space_allocation()
        {
        int k;

        for (k=0; k<gg; ++k)
            {
            mean[k]=(double *)muste_malloc((p+1)*sizeof(double)); // RS p+1
            if (mean[k]==NULL) { not_enough_memory(); return(-1); }
            w[k]=(double *)muste_malloc((p+1)*(p+1)*sizeof(double)); // RS p+1
            if (w[k]==NULL) { not_enough_memory(); return(-1); }
            }
        nk=(int *)muste_malloc(gg*sizeof(int));
        if (nk==NULL) { not_enough_memory(); return(-1); }

        inv=(double *)muste_malloc((p+1)*(p+1)*sizeof(double)); // RS p+1
        if (inv==NULL) { not_enough_memory(); return(-1); }
        det=(double *)muste_malloc((p+1)*sizeof(double)); // RS p+1
        if (det==NULL) { not_enough_memory(); return(-1); }
        prior=(double *)muste_malloc(gg*sizeof(double));
        if (prior==NULL) { not_enough_memory(); return(-1); }
        pg=(char **)muste_malloc(gg*sizeof(char **));
        if (pg==NULL) { not_enough_memory(); return(-1); }
        xg=(double *)muste_malloc(gg*sizeof(double));
        if (xg==NULL) { not_enough_memory(); return(-1); }
        var_post=(int *)muste_malloc(gg*sizeof(double));
        if (var_post==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int select_var_post()
        {
        int i,np;

        np=0;
        for (i=0; i<d.m_act; ++i)
            {
            if (d.vartype[d.v[i]][1]=='P')
                {
                var_post[np]=d.v[i];
                ++np;
                }
            }
        if (np==0) { post_type=0; return(1); }
        if (np!=gg)
            {
            sprintf(sbuf,"\n# (%d) for posterior probability (distance) variables",
                               np);
            sur_print(sbuf);
            sprintf(sbuf,"\ndoes not equal to # (%d) of groups!",gg);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(1);
        }

static int read_matrices()
        {
        int i,j,k;
        int m,n;
    //  int rlen,clen;

        ntot=0L;
        for (k=0; k<gg; ++k)
            {

         i=matrix_load(msn_name[k],&msn,&m,&n,&rlab,&clab,&lr,&lc,&type,expr);
            if (i<0) return(-1);
            if (m!=p || n!=3)
                {
                sprintf(sbuf,"\nIncompatible dimensions (%d,%d) in matrix file %s",
                                     m,n,msn_name[k]);
                sur_print(sbuf); WAIT; return(-1);
                }
         i=matrix_load(corr_name[k],&w[k],&m,&n,&rlab,&clab,&lr,&lc,&type,expr);
            if (i<0) return(-1);
            if (m!=p || n!=p)
                {
                sprintf(sbuf,"\nIncompatible dimensions (%d,%d) in matrix file %s",
                                     m,n,corr_name[k]);
                sur_print(sbuf); WAIT; return(-1);
                }

            nk[k]=(int)msn[2*p];
            ntot+=nk[k];
/* Rprintf("\nk=%d n=%d",k,nk[k]); getch();     */
            for (i=0; i<p; ++i) mean[k][i]=msn[i];

            for (i=0; i<p; ++i)
            for (j=0; j<p; ++j)
                w[k][i+p*j]*=msn[p+i]*msn[p+j];

/* mprint(w[k],p,p);  */

            }

        return(1);
        }

static int select_active_vars()
        {
        int i,n,k;
        char x[LLENGTH];
        char nimi[LLENGTH];
        char given_in[LLENGTH];

        i=spfind("COEFF");
        if (i>=0)
            {
            coeff_given=1;
            strcpy(x,spb[i]);
            i=matrix_load(x,&coeff,&p1,&n,&rlab,&clab,&lr,&lc,&type,expr);
            if (i<0) return(-1);
            if (n!=p)
                {
                sprintf(sbuf,"\n# (%d) of columns in COEFF file not equal to %d!",
                                   n,p);
                sur_print(sbuf); WAIT;
                }
            sprintf(given_in," (given in COEFF file %s)",x);
            }
        else
            {
            coeff_given=0;
            p1=p;
            *given_in=EOS;
            }
        v=(int *)muste_malloc(p1*sizeof(int));
        if (v==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<p1; ++i)
            {
            poimi(nimi,rlab,lr,i);
            k=varfind2(&d,nimi,0);
            if (k<0)
                {
                sprintf(sbuf,"\nVariable %s%s not found in data!",
                                      nimi,given_in);
                sur_print(sbuf); WAIT; return(-1);
                }
            v[i]=k;
            }
        return(1);
        }

static int priors()
        {
        int i,k;
        char x[LLENGTH];

        if (var_bayes1<0 && var_bayes2<0) return(1);
        i=spfind("PRIORS");
        if (i<0)
            {
            for (k=0; k<gg; ++k) prior[k]=nk[k];
            return(1);
            }
        strcpy(x,spb[i]); i=split(x,pg,gg);
        if (i!=gg)
            {
            sprintf(sbuf,"\n# of PRIORS (%d) not equal to # of groups (%d)",
                                    i,gg);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(1);
        }


static int classify()
        {
        int i,j,k,nvar;
        int l;
        double y;
        int miss;
        double *px;
        extern double mahal();
        double arvo=MISSING8; // RS ADD =MISSING8

        if (ok1==1) { i=compute_wtot(); if (i<0) return(-1); }

        if (coeff_given) nvar=p1; else nvar=p;
        xx=(double *)muste_malloc(nvar*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return(-1); }

        px=xx;
        if (coeff_given)
            {
            yy=(double *)muste_malloc(p*sizeof(double));
            if (yy==NULL) { not_enough_memory(); return(-1); }
            px=yy;
            }


        x2=(double *)muste_malloc(p*sizeof(double));
        if (x2==NULL) { not_enough_memory(); return(-1); }

        for (k=0; k<gg; ++k)
            {
            i=invert(w[k],p,&det[k]);
            if (i<0)
                {
                sprintf(sbuf,"\nCovariance matrix of group %d is singular!",
                                          k);
                sur_print(sbuf); WAIT; return(-1);
                }
            }


        if (ok1)
            {
            i=invert(wtot,p,&dettot);
/*Rprintf("\ndettot=%g",dettot); getch();
   mprint(wtot,p,p);
*/
            if (i<0)
                {
                sur_print("\nCommon covariance matrix of the groups is singular!");
                WAIT; return(-1);
                }
            }

        if (ok1)
            {
            d21=(double *)muste_malloc(gg*sizeof(double));  /* Mahalanobis */
            if (d21==NULL) { not_enough_memory(); return(-1); }
            }
        if (ok2)
            {
            d22=(double *)muste_malloc(gg*sizeof(double));  /* Mahalanobis */
            if (d22==NULL) { not_enough_memory(); return(-1); }
            }


        prind=0; sur_print("\n");
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            miss=0;
            for (i=0; i<nvar; ++i)
                {
                data_load(&d,l,v[i],&y);
                if (y==MISSING8) { miss=1; break; }
                xx[i]=y;
                }

//          if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
            if (prind) { sprintf(sbuf,"%d ",l); sur_print(sbuf); }

            if (coeff_given && !miss)
                {
                for (i=0; i<p; ++i)
                    {
                    y=0.0;
                    for (j=0; j<p1; ++j)
                        {
                        y+=coeff[j+i*p1]*xx[j];
                        }
                    yy[i]=y;
                    }
                }

            if (!miss) for (k=0; k<gg; ++k)
                {
                for (i=0; i<p; ++i) x2[i]=px[i]-mean[k][i];
                if (ok1)
                     d21[k]=mahal(x2,wtot,p);
                if (ok2)
                     d22[k]=mahal(x2,w[k],p);
                }
            else arvo=MISSING8;

            if (var_mahal1>=0)
                {
                if (!miss) { i=select_group(d21,1); arvo=(double)(i+1); }
                data_save(&d,l,var_mahal1,arvo);
                if (post_type==1) save_post(d21,l,0,miss);
                }
            if (var_mahal2>=0)
                {
                if (!miss) { i=select_group(d22,1); arvo=(double)(i+1); }
                data_save(&d,l,var_mahal2,arvo);
                if (post_type==2) save_post(d22,l,0,miss);
                }
            if (var_bayes1>=0)
                {
                if (!miss)
                    {
                    for (k=0; k<gg; ++k) xg[k]=prior[k]*exp(-d21[k]/2);
                    i=select_group(xg,2); arvo=(double)(i+1);
                    }
                data_save(&d,l,var_bayes1,arvo);
                if (post_type==3) save_post(xg,l,1,miss);
                }
            if (var_bayes2>=0)
                {
                if (!miss)
                    {
                    for (k=0; k<gg; ++k) xg[k]=prior[k]/sqrt(det[k])*exp(-d22[k]/2);
                    i=select_group(xg,2); arvo=(double)(i+1);
                    }
                data_save(&d,l,var_bayes2,arvo);
                if (post_type==4) save_post(xg,l,1,miss);
                }

            } // l

        return(1);
        }

static int select_group(double *x,int minmax)
// double *x;
// int minmax; /* 1=min 2=max */
        {
        int k,i;
        double y;

        i=0; y=x[0];
        for (k=1; k<gg; ++k)
            {
            if (minmax==1)
                { if (x[k]<y) { y=x[k]; i=k; } continue; }
            if (x[k]>y) { y=x[k]; i=k; }
            }
        return(i);
        }

static double mahal(double *x,double *s,int m)
        {
        int i,j;
        double d2;

        d2=0.0;
        for (i=0; i<m; ++i)
            {
            d2+=x[i]*x[i]*s[(m+1)*i];
            for (j=0; j<i; ++j)
                d2+=2*x[i]*x[j]*s[i+m*j];
            }
        return(d2);
        }

static int save_post(double *x,int l,int norm,int miss)
        {
        int k;
        double sum;
        double arvo=MISSING8; // RS ADD =MISSING8

        if (!miss)
            {
            if (norm)
                {
                sum=0.0;
                for (k=0; k<gg; ++k) sum+=x[k];
                }
            else sum=1.0;
            }
        else arvo=MISSING8;

        for (k=0; k<gg; ++k)
            {
            if (!miss) arvo=x[k]/sum;
            data_save(&d,l,var_post[k],arvo);
            }
        return(1);
        }

static int invert(double *A,int m,double *pdet)
        {
        int i,k;

        k=mat_inv(inv,A,m,pdet);
        for (i=0; i<m*m; ++i) A[i]=inv[i];
        return(k);
        }

static int compute_wtot()
        {
        int i,k;

        wtot=(double *)muste_malloc(p*p*sizeof(double));
        if (wtot==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<p*p; ++i) wtot[i]=0.0;
        for (k=0; k<gg; ++k)
            for (i=0; i<p*p; ++i) wtot[i]+=(nk[k]-1L)*w[k][i];
        for (i=0; i<p*p; ++i) wtot[i]/=(double)(ntot-(int)gg);
        return(1);
        }

static int poimi(char *nimi,char *lab,int l,int i)
        {
        int k=0;
        while (lab[i*l+k]!=' ' && k<l && k<8) { nimi[k]=lab[i*l+k]; ++k; }
        nimi[k]=EOS;
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (CLASSI)");
        WAIT; return(1);
        }

/* mahal.c 19.4.1990/SM (24.10.1991) (14.2.1994)
*/

static int results_line;
static int var_mahal;
static int var_c2mahal;
static int m_act;
static double *xx,*mah_mean,*S,*T;
static int n;
static double mah_det;
static double maxmahal;
static int jmaxmahal;

static void op_mahal()
        {
        int i,h;

// RS Variable init
xx=NULL;
mah_mean=NULL;
S=NULL;
T=NULL;

        if (g<2)
            {
            init_remarks();
            rem_pr("MAHAL <data>,<output_line>        / S.Mustonen 19.4.1990      ");
            rem_pr("computes Mahalanobis distances as a variable activated by `D' ");
            rem_pr("and/or their cumulative probabilities according to            ");
            rem_pr("Chi^2 distribution activated by `P'                           ");
            rem_pr("from other active variables in <data>.                        ");
            rem_pr("                                                              ");
            rem_pr("If the data is sorted by the `P' or `D' variable,             ");
            rem_pr("the `P' variable plotted against (ORDER-0.5)/N should give    ");
            rem_pr("a straight line when the data is a sample from a multinormal  ");
            rem_pr("distribution.                                                 ");
            wait_remarks(2);
            return;
            }
        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }

        i=data_open(word[1],&d); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;
        var_mahal=activated(&d,'D');
        var_c2mahal=activated(&d,'P');
        if (var_mahal<0 && var_c2mahal<0)
            {
            sur_print("\nNo variable activated by 'D' (or 'P') for Mahalanobis distances!");
            WAIT; return;
            }
        h=0;
        for (i=0; i<d.m_act; ++i)
            {
            if (d.v[i]==var_mahal) continue;
            if (d.v[i]==var_c2mahal) continue;
            d.v[h]=d.v[i];
            ++h;
            }
        m_act=d.m_act-1;
        i=varaa_tilat(); if (i<0) return;
        i=laske_momentit(); if (i<0) return;

        i=mat_inv(T,S,m_act,&mah_det);
        if (i<=0)
            {
            sur_print("\nCovariance matrix is singular!");
            sur_print("\nCannot compute distances!");
            WAIT; return;
            }

        laske_mahal();
        tulostus();
        sprintf(sbuf,"Mahalanobis distance in %d variables",m_act);
        update_varname(&d,var_mahal,sbuf);
        data_close(&d);
        return;
        }

static int varaa_tilat()
        {

        xx=(double *)muste_malloc(m_act*sizeof(double));
        if (xx==NULL) { ei_tilaa(); return(-1); }
        mah_mean=(double *)muste_malloc(m_act*sizeof(double));
        if (mah_mean==NULL) { ei_tilaa(); return(-1); }
        S=(double *)muste_malloc(m_act*m_act*sizeof(double));
        if (S==NULL) { ei_tilaa(); return(-1); }
        T=(double *)muste_malloc(m_act*m_act*sizeof(double));
        if (T==NULL) { ei_tilaa(); return(-1); }

        return(1);
        }

static int ei_tilaa()
        {
        sur_print("\nNot enough memory!"); WAIT; return(1);
        }

static int laske_momentit()
        {
        int i,h;
        int j;

        sur_print("\nComputing moments...");

        n=0L;
        for (i=0; i<m_act; ++i)
            {
            mah_mean[i]=0.0;
            for (h=0; h<=i; ++h)
                S[i+h*m_act]=0.0;
            }
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            for (i=0; i<m_act; ++i)
                {
                data_load(&d,j,d.v[i],&xx[i]);
                if (xx[i]==MISSING8) break;
                }
//          if (i<m_act) continue;
//          if (prind) { sprintf(sbuf," %d",j); sur_print(sbuf); }
            ++n;
            for (i=0; i<m_act; ++i)
                {
                mah_mean[i]+=xx[i];
                for (h=0; h<=i; ++h)
                    S[i+h*m_act]+=xx[i]*xx[h];
                }
            }  /* j */

        if (n<2)
            {
            sur_print("\nLess than 2 valid observations!");
            sur_print("\nCannot compute distances!");
            WAIT; return(-1);
            }
        for (i=0; i<m_act; ++i) mah_mean[i]/=n;
        for (i=0; i<m_act; ++i)
            for (h=0; h<=i; ++h)
                {
                S[i+h*m_act]-=n*mah_mean[i]*mah_mean[h];
                S[h+i*m_act]=S[i+h*m_act]/=(n-1);
                }
        return (1);
        }

static int laske_mahal()
        {
        int i,h;
        int j;
        double dd;

        sur_print("\nComputing Mahalanobis distances...");
        maxmahal=0.0;
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            dd=MISSING8;
            for (i=0; i<m_act; ++i)
                {
                data_load(&d,j,d.v[i],&xx[i]);
                if (xx[i]==MISSING8) break;
                }
            if (i==m_act)
                {
//              sprintf(sbuf," %d",j); sur_print(sbuf);
                for (i=0; i<m_act; ++i) xx[i]-=mah_mean[i];
                dd=0.0;
                for (i=0; i<m_act; ++i)
                    {
                    for (h=0; h<i; ++h)
                        dd+=2*T[i+h*m_act]*xx[i]*xx[h];
                    dd+=T[i*(m_act+1)]*xx[i]*xx[i];
                    }
                if (dd>maxmahal) { maxmahal=dd; jmaxmahal=j; }
                }
            if (var_mahal>=0) data_save(&d,j,var_mahal,dd);
            if (var_c2mahal>=0)
                data_save(&d,j,var_c2mahal,muste_cdf_chi2(dd,(double)m_act,1e-15));
            }  /* j */
        return(1);
        }

static int tulostus()
        {
        int i;
        char x[LLENGTH];
        char y[LLENGTH];
        char u[LLENGTH];
        extern char *spois();

        i=output_open(eout);  if (i<0) return(1);
        i=sprintf(x,"SURVO 84C data: %s  ",word[1]);
        if (var_mahal>=0) i+=sprintf(x+i,"Mahalanobis D^2 in %s  ",
                                                       d.varname[var_mahal]);
        if (var_c2mahal>=0) i+=sprintf(x+i,"P values of D^2 in %s",
                                                       d.varname[var_c2mahal]);
        print_line(x);
        fnconv(maxmahal,accuracy,y);

        *u=EOS;
        if (d.vartype[0][0]=='S')
            {
            data_alpha_load(&d,jmaxmahal,0,x);
            sprintf(u," (%s)",spois(x));
            }
        sprintf(x,"Max.distance=%s in obs. # %d%s",spois(y),jmaxmahal,u);
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

