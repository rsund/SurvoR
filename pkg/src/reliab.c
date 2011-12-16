/* RELIAB.C  -  K.Vehkalahti 1993-2005
   Converted for Muste 15.12.2011/KV
*/
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

extern void muste_kv_s_err(char *, ...);
extern void muste_kv_s_disp(char *, ...);
extern void muste_kv_usage_info(void);
extern int muste_kv_edline(char *, int, int);

#define NORMAL12  (model==1 || model==2)
#define WEIGHTED  (model>=3)
#define NO_RFACT  (model==1 || model==3 || model==5)
#define WEIGHTED2 (model>4)

static int results_line,model,alpha,orthogonal;
static int errorneous;
static double *CORR,*FACT,*RFACT,*COEFF,*COEFF2,*MSN,*COV;
static double *FT,*TMP,*RCOV,*RCOVd,*FC,*COEF,*COVd;
static int mX,nF,nW,nW2;
static char *clabR,*clabF,*clabW,*rlabW,*clabW2, *rlabW2;
static char *clab,*rlab;
static int lr,lc,type;
static char expr[129], weight[LNAME];
static int wdim, w2dim;

static int check_parameters(void);
static int check_specifications(void);
static void no_memory(void);
static int allocate_memory(void);
static int compute_weights(void);
static void free_spaces(void);
static int reliabilities(void);
static void print_line(void);
static void write_line(void);
static char *trim1(char *);
static void trim2(char *);
static void save_resmats(void);
static void add_loadms(void);
static void do_cov_matrix(void);
static double compute_alpha(void);

// 4.6.2004:
/**************************************
char *spec_reliab[]={ "WEIGHT", "WEIGHT2", "RESULTS", "MSN", "OUTFILE", "!" };

char **specs=spec_reliab;
***************************************/

void muste_reliab(char *argv)
{
    int i;
    s_init(argv[1]);
    spec_init(r1+r-1);
    i=check_parameters(); if (i<0) return;
    i=check_specifications(); if (i<0) return;
    i=allocate_memory(); if (i<0) return;
    i=compute_weights(); if (i<0) return;
    reliabilities();
    free_spaces();
    s_end(argv);
    return;
}

static int check_parameters (void)
{
    int i,j,k;

    if (g<3) {
        muste_kv_usage_info();
        return -1;
    }
    results_line=0; model=1; alpha=0;
    i=matrix_load(word[1],&CORR,&mX,&j,&rlab,&clabR,&lr,&lc,&type,expr);
    if (i<0) return -1;
    if (j!=mX) {
        muste_kv_s_err("%s is not a proper correlation matrix!",word[1]);
        return -1;
    }
    i=matrix_load(word[2],&FACT,&j,&nF,&rlab,&clabF,&lr,&lc,&type,expr);
    if (i<0) return -1;
    if (j!=mX) {
        muste_kv_s_err("Correlation matrix and factor matrix are incompatible!");
        return -1;
    }
    if (g>3) {
        i=muste_kv_edline(word[3],1,0);
        if (i==0) { /* no such edit line found! => matrix? */
            i=matrix_load(word[3],&RFACT,&j,&k,&rlab,&clab,&lr,&lc,&type,expr);
            if (i<0) return -1;
            model=2;
            if (j!=k) {
                muste_kv_s_err("%s is not a proper factor correlation matrix!", word[3]);
                return -1;
            }
            if (j!=nF) {
                muste_kv_s_err("Factor correlation matrix and factor matrix are incompatible!");
                return -1;
            }
        } else {
            results_line=i;
        }
    }

    if (g>4) {
        if (results_line) {
            muste_kv_s_err("Wrong number of parameters!");
            muste_kv_usage_info();
            return -1;
        }
        i=muste_kv_edline(word[4],1,0);
        if (i==0) {
            muste_kv_s_err("Invalid edit line: %s!", word[4]);
            return -1;
        }
        results_line=i;
    }
    return 1;
}

static int check_specifications(void)
{
    int i,j,k,n;

    j=spfind("WEIGHT");
    if ((j>=0) && (strcmp(spb[j],"0"))) {
        strcpy(weight,spb[j]);
        i=matrix_load(weight,&COEFF,&k,&nW,&rlabW,&clabW,&lr,&lc,&type,expr);
        if (i<0) return -1;
        model+=2;
        if (k-1!=mX && k!=mX ) {
            muste_kv_s_err("Incompatible dimensions in coefficient matrix %s!",weight);
            return -1;
        }
        wdim=k;
        j=spfind("WEIGHT2");
        if (j>=0) {
            strcpy(sbuf,spb[j]);
            strcat(weight,"*"); strcat(weight,sbuf);
            i=matrix_load(sbuf,&COEFF2,&k,&nW2,&rlab,&clabW2,&lr,&lc,&type,expr);
            if (i<0) return -1;
            model+=2;
            if (k-1!=nW && k!=nW ) {
                muste_kv_s_err("Incompatible dimensions in coefficient matrix %s!",spb[j]);
                return -1;
            }
            w2dim=k;
        }
    }
    j=spfind("RESULTS");
    if (j>=0) results=atoi(spb[j]);

    j=spfind("MSN_for_Cronbach's_alpha");
    if (j<0) j=spfind("MSN"); /* 9.12.93 */
    if (j>=0) {
        alpha=1;
        if (*spb[j]=='*') {
            MSN=(double *)muste_malloc(mX*2*sizeof(double));
            if (MSN==NULL) { no_memory(); return -1; }
            for (i=0; i<mX; i++) for (j=0; j<2; j++)
                MSN[i+mX*j]=(double)j; /* j=0: mean, j=1: stddev */
        } else {
            i=matrix_load(spb[j],&MSN,&k,&n,&rlab,&clab,&lr,&lc,&type,expr);
            if (i<0) return -1;
            if (n<2 || n>3) {
                muste_kv_s_err("%s is not a proper MSN-matrix!",spb[j]);
                return -1;
            }
            if (k!=mX) {
                muste_kv_s_err("Incompatible dimensions in MSN-matrix %s!",spb[j]);
                return -1;
            }
        }
        i=matrix_load(word[1],&COV,&mX,&n,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return -1;
    }
    return 1;
}

static void no_memory(void) { muste_kv_s_err("Not enough memory!"); }

static int allocate_memory(void)
{
    int i,j;
    double chsum;

    orthogonal=1;
    if (NO_RFACT) { /* create identity matrix */
        RFACT=(double *)muste_malloc(nF*nF*sizeof(double));
        if (RFACT==NULL) { no_memory(); return -1; }
        for (i=0; i<nF; i++) for (j=0; j<nF; j++)
            if (i==j) RFACT[i+nF*j]=1.0;
            else RFACT[i+nF*j]=0.0;
    } else { /* check given factor correlation matrix */
        chsum=0.0;
        for (i=0; i<nF; i++) for (j=0; j<nF; j++)
            chsum+=RFACT[i+nF*j];
        if (chsum!=nF) orthogonal=0; /* not an identity matrix */
    }
    if (orthogonal) {
        for (i=0; i<mX; i++) for (j=0; j<nF; j++)
          if (FACT[i+mX*j] > 1.0) {
              muste_kv_s_err("Error: factor loading > 1.0 in element (%d,%d) of factor matrix!",i+1,j+1);
              return -1;
          }
    }
    FT=(double *)muste_malloc(mX*nF*sizeof(double));
    if (FT==NULL) { no_memory(); return -1; }
    TMP=(double *)muste_malloc(mX*nF*sizeof(double));
    if (TMP==NULL) { no_memory(); return -1; }
    RCOV=(double *)muste_malloc(mX*mX*sizeof(double));
    if (RCOV==NULL) { no_memory(); return -1; }
    RCOVd=(double *)muste_malloc(mX*1*sizeof(double));
    if (RCOVd==NULL) { no_memory(); return -1; }
    FC=(double *)muste_malloc(mX*1*sizeof(double));
    if (FC==NULL) { no_memory(); return -1; }
    if (alpha) {
        COVd=(double *)muste_malloc(mX*1*sizeof(double));
        if (COVd==NULL) { no_memory(); return -1; }
    }
    return 1;
}

static int compute_weights(void)
{
    int i,j,k,l;
    double a;

    if (!WEIGHTED) return 1;
    if (WEIGHTED2) { /* 2nd order scale */
        COEF=(double *)muste_malloc((mX+1)*nW2*sizeof(double));
        if (COEF==NULL) { no_memory(); return -1; }
        for (i=0; i<mX+1; i++) for (j=0; j<nW2; j++)
            COEF[i+(mX+1)*j]=0.0;
        for (i=1; i<mX+1; i++) {
            for (j=0; j<nW2; j++) {
                a=0.0;
                if (wdim==mX) {
                    if (w2dim==nW) {
                        for (k=0,l=0; k<nW && l<nW; k++,l++)
                            a+=COEFF[(i-1)+mX*k]*COEFF2[(l+0)+nW*j];
                    } else {
                        for (k=0,l=0; k<nW && l<nW; k++,l++)
                            a+=COEFF[(i-1)+mX*k]*COEFF2[(l+1)+(nW+1)*j];
                    }
                } else {
                    if (w2dim==nW) {
                        for (k=0,l=0; k<nW && l<nW; k++,l++)
                            a+=COEFF[i+(mX+1)*k]*COEFF2[(l+0)+nW*j];
                    } else {
                        for (k=0,l=0; k<nW && l<nW; k++,l++)
                            a+=COEFF[i+(mX+1)*k]*COEFF2[(l+1)+(nW+1)*j];
                    }
                }
                COEF[i+(mX+1)*j]=a;
            }
        }
        if (wdim==mX) {
            rlabW2=(char *)muste_malloc((mX+1)*8*sizeof(char)+1);
            if (rlabW2==NULL) { no_memory(); return -1; }
            strcpy(rlabW2,"Constant");
            strcat(rlabW2,rlabW);
            rlabW2[(mX+1)*8]='\0';
        } else {
            rlabW2=rlabW;
        }
        sprintf(sbuf,"Second_order_scale_coefficients_(%s)",weight);
        matrix_save("WEIGHT2.M",COEF,mX+1,nW2,
                     rlabW2,clabW2,lr,lc,-1,sbuf,0,0);
        muste_free(rlabW2);
    } else { /* 1st order scale */
        COEF=(double *)muste_malloc((mX+1)*nW*sizeof(double));
        if (COEF==NULL) { no_memory(); return -1; }
        for (i=0; i<mX+1; i++) {
            for (j=0; j<nW; j++) {
                if (wdim==mX) {
                    if (i==0) COEF[i+(mX+1)*j]=0.0;
                    else      COEF[i+(mX+1)*j]=COEFF[(i-1)+mX*j];
                } else {
                              COEF[i+(mX+1)*j]=COEFF[i+(mX+1)*j];
                }
            }
        }
    }
    return 1;
}

static void free_spaces(void)
{
    if (alpha) { muste_free(COV); muste_free(MSN); muste_free(COVd); }
    if (WEIGHTED) muste_free(COEF);
    muste_free(FC); muste_free(RCOVd); muste_free(TMP); muste_free(FT);
    if (NO_RFACT) muste_free(RFACT);
    if (WEIGHTED2) muste_free(COEFF2);
    if (WEIGHTED) muste_free(COEFF);
    if (!NO_RFACT) muste_free(RFACT);
    muste_free(FACT); muste_free(CORR);
}

static int reliabilities(void)
{
    int i,j,k,l,up_lim;
    double a,b,rxx;

    char mod[LNAME];
    char lab[LNAME],reli[LNAME],alph[LNAME];

    errorneous=0;
    a=0.0; b=0.0; /* 30.12.96 */

    mat_transp(FT,FACT,mX,nF);
    mat_mlt(TMP,FACT,RFACT,mX,nF,nF);
    mat_mlt(RCOV,TMP,FT,mX,nF,mX);
    for (i=0; i<mX; i++) for (j=0; j<mX; j++)
        RCOV[i+mX*j]=CORR[i+mX*j]-RCOV[i+mX*j];
    for (i=0,j=0; i<mX; i++,j++) {
        RCOVd[i]=RCOV[i+mX*j];
        if (RCOVd[i] < 0.0) errorneous=1;
    }
    /* row&col multiply to get residual corr.matrix */
    for (i=0; i<mX; i++) for (j=0; j<mX; j++)
        if (RCOVd[i]<=0.0) CORR[i+mX*j]=0.0;
        else CORR[i+mX*j]=RCOV[i+mX*j]/sqrt(RCOVd[i]);
    for (i=0; i<mX; i++) for (j=0; j<mX; j++)
        if (RCOVd[i]<=0.0) CORR[i*mX+j]=0.0;
        else CORR[i*mX+j]/=sqrt(RCOVd[i]);
    save_resmats();

    output_open(eout);
    strcpy(sbuf,"Reliabilities according to models E2 and E3:");
    if (WEIGHTED) {
        sprintf(mod," (weighted by %s)", weight); strcat(sbuf,mod);
    }
    print_line();
    strcpy(sbuf,"E2: errors do not correlate; E3: errors may correlate.");
    print_line();

    if (alpha) do_cov_matrix(); /* 29.2.2000 */

    if (WEIGHTED)  up_lim=nW;
    if (WEIGHTED2) up_lim=nW2;
    if (NORMAL12)  up_lim=nF;

    for (k=0; k<=up_lim; k++) {
        if (k<up_lim) {
            if (WEIGHTED) {
                for (i=0; i<mX; i++) FC[i]=COEF[(i+1)+(mX+1)*k]; /* skip constant! */
                if (WEIGHTED2) strncpy(lab,&clabW2[lc*k],lc);
                          else strncpy(lab,&clabW [lc*k],lc);
                lab[lc]='\0'; trim2(lab);
            }
            if (NORMAL12) {
                for (i=0; i<mX; i++) FC[i]=FACT[i+mX*k];
                strncpy(lab,&clabF[lc*k],lc);
                lab[lc]='\0'; trim2(lab);
            }
        } else { /* last round for the unweighted sums only */
            if (WEIGHTED) break;
            for (i=0; i<mX; i++) FC[i]=1.0;
            strcpy(lab,"Sum");
        }
        if (alpha) {
            rxx=compute_alpha();
            fnconv(rxx,accuracy,alph);
            trim1(alph); trim2(alph);
        }
        mat_mlt(TMP,FACT,RFACT,mX,nF,nF);
        mat_transp(FT,FACT,mX,nF);
        mat_mlt(CORR,TMP,FT,mX,nF,mX);
        mat_mlt(TMP,FC,CORR,1,mX,mX);
        mat_mlt(&a,TMP,FC,1,mX,1);

        for (l=0; l<2; l++) {
            if (l==0) {
                for (i=0; i<mX; i++)
                    TMP[i]=FC[i]*RCOVd[i];
                sprintf(mod,"%s\\E2", lab);
            } else {
                mat_mlt(TMP,FC,RCOV,1,mX,mX);
                sprintf(mod,"%s\\E3", lab);
            }
            mat_mlt(&b,TMP,FC,1,mX,1);
            rxx=1.0/(1.0+b/a);
            fnconv(rxx,accuracy,reli);
            if (l==0) { /* first items on line */
                sprintf(sbuf,"%s=%s ",mod,trim1(reli));
            } else { /* the rest of the items */
                strcat(sbuf,mod); strcat(sbuf,"=");
                strcat(sbuf,trim1(reli));
                if (alpha) {
                    strcat(sbuf,"   (Cronbach's alpha:");
                    strcat(sbuf,alph); strcat(sbuf,")");
                }
                print_line();
            }
        }
    }
    output_close(eout);
    add_loadms();
    return 1;
}

static void print_line(void)
{
    int res_line;
    if (results<31) res_line=0; else res_line=results_line;
    if (results>=0) output_line(sbuf,eout,res_line);
    if (res_line) ++results_line;
}

static char *trim1(char *s) /* remove spaces from the beginning */
{
    while(*s==' ') ++s;
    return(s);
}

static void trim2(char *s) /* remove spaces from the end */
{
    while(*s) s++; s--;
    while(*s==' ') s--; s++;
    *s='\0';
}

static void save_resmats(void)
{
    strcpy(sbuf,"Residual_covariances");
    matrix_save("RCOV.M",RCOV,mX,mX,clabR,clabR,lr,lc,10,sbuf,0,0);
    strcpy(sbuf,"Residual_correlations");
    matrix_save("RCORR.M",CORR,mX,mX,clabR,clabR,lr,lc,10,sbuf,0,0);
}

static void add_loadms(void)
{
    if (results_line==0) return; /* 29.2.2000 */
    if (results<31) return;
    if (errorneous) {
        strcpy(sbuf,"At least one communality was greater than 1, which implies");
        write_line();
        strcpy(sbuf,"that there might be an error in the data or in the model!");
        write_line();
    } else {
        strcpy(sbuf,"LOADM RCOV.M,##.###,END+2 / ");
        strcat(sbuf,"Residual covariance matrix");
        write_line();
        strcpy(sbuf,"LOADM RCORR.M,##.###,END+2 / ");
        strcat(sbuf,"Residual correlation matrix");
        write_line();
        strcpy(sbuf,"LIMITS=-0.9,-0.2,-0.1,0.1,0.2,0.9,1 ");
        strcat(sbuf,"SHADOWS=7,8,1,0,1,8,7 ");
        write_line();
    }
}

static void write_line(void)
{
    edwrite(space,results_line,1);
    edwrite(sbuf,results_line,1); results_line++;
}

static void do_cov_matrix(void)
{
    int i,j;
    double a;

    for (i=0; i<mX; i++) {
        a=MSN[i+mX]; COVd[i]=a*a;
        for (j=0; j<mX; j++) {
            COV[i+mX*j]*=a; COV[i*mX+j]*=a;
        }
    }
}

static double compute_alpha(void)
{
    int i;
    double a,b;

    a=0.0; b=0.0;
    for (i=0; i<mX; i++)
        a+=FC[i]*COVd[i]*FC[i];

    mat_mlt(TMP,FC,COV,1,mX,mX);
    mat_mlt(&b,TMP,FC,1,mX,1);
    return ((double)mX/((double)mX-1.0)*(1.0-a/b));
}
