#include "muste.h"
/* !movreg.c 21.12.1994/kv (6.2.95) (3.5.95:PSP) (8.5.95:B,S,T)
             21.8.1998/kv (for 32bit SURVO 98)
             12.8.2000/kv (for SURVO MM)
             15.12.2011/kv (for Muste)
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

extern void muste_kv_s_err(char *, ...);
extern void muste_kv_s_disp(char *, ...);
extern void muste_kv_usage_info(void);

typedef double e_type;
typedef struct queue_record {
    unsigned int max_size;
    unsigned int front;
    unsigned int rear;
    unsigned int size;
    e_type *A;
} QUEUE_RECORD;

typedef QUEUE_RECORD *QUEUE;

static void make_null(QUEUE Q);
static void enqueue(e_type x, QUEUE Q);
/* for Survo matrices: */
static void enqueue2(e_type x, QUEUE Q, int col);

static void no_memory(void);
static int alloc_mem(void);
static void mov_reg(void);
static void save_missvals(int);
static void update_varnames(void);
static void put_varname(int, char *);
static void free_mem(void);
static void trim2(char *);

static SURVO_DATA dat;
static SURVO_DATA_FILE d2;
static int Yvar,Pvar,Rvar,R2var,Vvar,x_vars,*Xvars,
           b_vars,*Bvars,s_vars,*Svars,t_vars,*Tvars;
static int prediction,residuals,squaredcorr,resvariance,
           coeffs,stddevs,tvalues;
static int span,constant;
static QUEUE Qy,QX;
static double *U,*D,*V,*iD,*Ut,*xx,*iX,*beta,*yhat,*res,*xtxi,*varb;
static int prind;

// 4.6.2004:
/**************************************
char *spec_movreg[]={ "VARS", "MASK",
                      "PRIND", "SPAN", "CONSTANT", "!" };

char **specs=spec_movreg;
***************************************/

void muste_movreg(char *argv)
{
    int i,k,p,k0,k1,k2,k3,outvars;
    char ch;

    s_init(argv);
    if (g<2) { muste_kv_usage_info(); return; }
    if (word[1][0]=='?') { muste_kv_usage_info(); return; }

    k=spec_init(r1+r-1); if (k<0) return;
    i=fi_open(word[1],&d2); if (i<0) return;
    i=data_open2(word[1],&dat,0,1,0); if (i<0) return;
    mask(&dat);

 /* PRIND & prind */
    prind=0; // SM's suggestion 29.10.2001
    i=spfind("PRIND");
    if (i>=0) prind=atoi(spb[i]);
    else if (hae_apu("prind",sbuf)) prind=atoi(sbuf);

    i=spfind("SPAN");
    if (i<0) {
        muste_kv_s_err("Span must be given by SPAN=<span length> !");
        return;
    }
    span=atoi(spb[i]);
    if (span<2) {
        muste_kv_s_err("SPAN must be at least 2 !");
        return;
    }
    Yvar=Pvar=Rvar=R2var=Vvar=-1; x_vars=b_vars=s_vars=t_vars=outvars=0;
    prediction=residuals=squaredcorr=resvariance=coeffs=stddevs=tvalues=0;
    for (i=0; i<dat.m_act; i++) {
        ch=dat.vartype[dat.v[i]][1];
        switch (ch) {
          case 'Y' : if (Yvar<0) {
                         Yvar=dat.v[i];
                     }
                     break;
          case 'X' : x_vars++; break;
          case 'P' : if (!prediction) {
                         Pvar=dat.v[i];
                         prediction=1;
                         outvars++;
                     }
                     break;
          case 'R' : if (!residuals) {
                         Rvar=dat.v[i];
                         residuals=1;
                         outvars++;
                     }
                     break;
          case 'r' : if (!squaredcorr) {
                         R2var=dat.v[i];
                         squaredcorr=1;
                         outvars++;
                     }
                     break;
          case 'V' : if (!resvariance) {
                         Vvar=dat.v[i];
                         resvariance=1;
                         outvars++;
                     }
                     break;
          case 'B' : b_vars++; outvars++; break;
          case 'S' : s_vars++; outvars++; break;
          case 'T' : t_vars++; outvars++; break;
           default : break;
        }
    }
    if (b_vars>0) coeffs=1;
    if (s_vars>0) stddevs=1;
    if (t_vars>0) tvalues=1;
    if (Yvar<0) { muste_kv_s_err("No regressand (Y)!"); return; }
    if (x_vars==0) { muste_kv_s_err("No regressors (X)!"); return; }

    if (!outvars) {
        muste_kv_s_err("No output variables selected!");
    } else {
        muste_kv_s_disp("\n%d output variables selected", outvars);
    }

    constant=1;
    i=spfind("CONSTANT");
    if (i>=0) constant=atoi(spb[i]);
    if (!constant && squaredcorr) {
        muste_kv_s_err("R^2 values are valid only when the model has a constant!");
    }
    if (span <= x_vars) {
        muste_kv_s_err("Too many regressors (X) or span too narrow!");
        return;
    }
    Xvars=(int *)muste_malloc((size_t)x_vars*sizeof(int));
    if (Xvars==NULL) { no_memory(); return; }

    p=x_vars;
    if (constant) p++;

    if (coeffs) {
        if (b_vars<p) {
            muste_kv_s_err("Number of 'B' variables is too small!");
            return;
        }
        Bvars=(int *)muste_malloc((size_t)b_vars*sizeof(int));
        if (Bvars==NULL) { no_memory(); return; }
    }
    if (stddevs) {
        if (s_vars<p) {
            muste_kv_s_err("Number of 'S' variables is too small!");
            return;
        }
        Svars=(int *)muste_malloc((size_t)s_vars*sizeof(int));
        if (Svars==NULL) { no_memory(); return; }
    }
    if (tvalues) {
        if (t_vars<p) {
            muste_kv_s_err("Number of 'T' variables is too small!");
            return;
        }
        Tvars=(int *)muste_malloc((size_t)t_vars*sizeof(int));
        if (Tvars==NULL) { no_memory(); return; }
    }
    for (i=0, k0=k1=k2=k3=0; i<dat.m_act; i++) {
        ch=dat.vartype[dat.v[i]][1];
        switch (ch) {
          case 'X' :              Xvars[k0++]=dat.v[i]; break;
          case 'B' : if (coeffs)  Bvars[k1++]=dat.v[i]; break;
          case 'S' : if (stddevs) Svars[k2++]=dat.v[i]; break;
          case 'T' : if (tvalues) Tvars[k3++]=dat.v[i]; break;
          default : break;
        }
    }

    i=alloc_mem(); if (i<0) return;
    mov_reg();
    update_varnames();
    free_mem();
    s_end(argv);
}

static void no_memory(void)
{
    muste_kv_s_err("Not enough memory!");
}

static int alloc_mem(void)
{
    int p=x_vars;

    if (constant) p++;

    Qy=(QUEUE)muste_malloc(sizeof(QUEUE_RECORD));
      if (Qy==NULL) { no_memory(); return -1; }
    Qy->A=(e_type *)muste_malloc(span*1*sizeof(e_type));
      if (Qy->A==NULL) { no_memory(); return -1; }
    make_null(Qy);
    Qy->max_size=span;

    QX=(QUEUE)muste_malloc(sizeof(QUEUE_RECORD));
      if (QX==NULL) { no_memory(); return -1; }
    QX->A=(e_type *)muste_malloc(span*p*sizeof(e_type));
      if (QX->A==NULL) { no_memory(); return -1; }
    make_null(QX);
    QX->size=-1;        /* ks. enqueue2 ! */
    QX->max_size=span;  /* ei siis span*p ! */

    U=(double *)muste_malloc(span*p*sizeof(double));
      if (U==NULL) { no_memory(); return -1; }
    D=(double *)muste_malloc(p*1*sizeof(double));
      if (D==NULL) { no_memory(); return -1; }
    V=(double *)muste_malloc(p*p*sizeof(double));
      if (V==NULL) { no_memory(); return -1; }
    iD=(double *)muste_malloc(p*p*sizeof(double));
      if (iD==NULL) { no_memory(); return -1; }
    xx=(double *)muste_malloc(p*p*sizeof(double));
      if (xx==NULL) { no_memory(); return -1; }
    Ut=(double *)muste_malloc(p*span*sizeof(double));
      if (Ut==NULL) { no_memory(); return -1; }
    iX=(double *)muste_malloc(p*span*sizeof(double));
      if (iX==NULL) { no_memory(); return -1; }
    beta=(double *)muste_malloc(p*1*sizeof(double));
      if (beta==NULL) { no_memory(); return -1; }
    yhat=(double *)muste_malloc(span*1*sizeof(double));
      if (yhat==NULL) { no_memory(); return -1; }
    res=(double *)muste_malloc(span*1*sizeof(double));
      if (res==NULL) { no_memory(); return -1; }
    xtxi=(double *)muste_malloc(p*p*sizeof(double));
      if (xtxi==NULL) { no_memory(); return -1; }
    varb=(double *)muste_malloc(p*1*sizeof(double));
      if (varb==NULL) { no_memory(); return -1; }

    return 1;
}

static void free_mem(void)
{
    muste_free(QX->A); muste_free(QX);
    muste_free(Qy->A); muste_free(Qy);
    muste_free(Xvars); muste_free(Bvars); muste_free(Svars); muste_free(Tvars);
    data_close(&dat); fi_close(&d2);
    muste_free(U); muste_free(D); muste_free(V); muste_free(iD); muste_free(xx); muste_free(Ut);
    muste_free(iX); muste_free(beta); muste_free(yhat); muste_free(res); muste_free(xtxi); muste_free(varb);
}

static void mov_reg(void)
{
    int i,k,p,split,obsmiss;
    int j,n,nn;
    double x,y,eps,tol,ysum,SSE,SST,Rsquare,sigma2;

    eps=1e-16; tol=(1e-300)/eps; /* for SVD */
    split=span-1; /* ehk‰ s‰‰dett‰viss‰: span/2 ?? */

    p=x_vars; if (constant) p++;
    muste_kv_s_disp("\n"); /* postipankki(p); */
    muste_kv_s_disp("Loading first %d observations...", span);
    for (j=dat.l1, n=nn=0L; j<=dat.l2; j++) {
        obsmiss=0;
        data_load(&dat,j,Yvar,&y);
        if (y==MISSING8) obsmiss=1;
        for(i=0; i<x_vars; i++) {
            data_load(&dat,j,Xvars[i],&x);
            if (x==MISSING8) { obsmiss=1; break; }
        }
        if (obsmiss) {
            muste_kv_s_disp("%d- ",++nn);
            save_missvals(j);
            continue;
        }
        /* observation ok, show out and put values to the queues: */
        n++;
        if (n==span) {
            muste_kv_s_disp("\nComputing moving linear regression analysis by ");
            muste_kv_s_disp("orthogonalization...\n");
        }
/*      if (n>=span) muste_kv_s_disp("\n%d ",++nn);
        else muste_kv_s_disp("%d ",++nn); */
        if (prind) muste_kv_s_disp("%d ",++nn);
        enqueue(y,Qy);
        enqueue2(1.0,QX,0);
        for(i=0; i<x_vars; i++) {
            data_load(&dat,j,Xvars[i],&x);
            enqueue2(x,QX,i+1);
        }
        if (n<span) {
            save_missvals(j);
            continue;
        }
        for (i=0; i<span; i++) for (k=0; k<p; k++)
            U[i+span*k]=QX->A[i+span*k]; /* copy X to U before svd */
        mat_svd(U,D,V,span,p,eps,tol);
        for (i=0; i<p; i++) for (k=0; k<p; k++) {
            iD[i+k*p]=0.0;
            if (i==k && D[i]!=0.0) iD[i+k*p]=1.0/D[i];
        }
        mat_mltd(xx,V,iD,p,p);
        mat_transp(Ut,U,span,p);
        mat_mlt(iX,xx,Ut,p,p,span);
        mat_mlt(beta,iX,Qy->A,p,span,1);
        mat_mlt(yhat,QX->A,beta,span,p,1);
        mat_sub(res,Qy->A,yhat,span,1);
        mat_mmt(xtxi,xx,p,p);
        for (i=0, SSE=0.0; i<span; i++) { x=res[i]; SSE+=x*x; }
        sigma2=SSE/(span-p);
        for (i=0; i<p; i++) varb[i]=sqrt(sigma2*xtxi[i+i*p]);
        mat_sum(&ysum,Qy->A,span,1);
        ysum/=span;
        for (i=0, SST=0.0; i<span; i++) { x=Qy->A[i]-ysum; SST+=x*x; }
        Rsquare = 1.0-SSE/SST;

        if (squaredcorr) {
            data_save(&dat,j,R2var,Rsquare);
        }
        if (resvariance) {
            data_save(&dat,j,Vvar,sigma2);
        }
        if (prediction) {
            data_save(&dat,j,Pvar,yhat[split]);
        }
        if (residuals) {
            data_save(&dat,j,Rvar,res[split]);
        }
        if (++split==span) split=0;
        if (coeffs) {
            for (i=0; i<b_vars; i++) {
                data_save(&dat,j,Bvars[i],beta[i]);
            }
        }
        if (stddevs) {
             for (i=0; i<s_vars; i++) {
                 data_save(&dat,j,Svars[i],varb[i]);
             }
        }
        if (tvalues) {
             for (i=0; i<t_vars; i++) {
                 data_save(&dat,j,Tvars[i],beta[i]/varb[i]);
             }
        }
    }
    if (n==0L) muste_kv_s_err("No acceptable observations!");
}

static void save_missvals(int obs)
{
    int i;

    if (prediction) data_save(&dat,obs,Pvar,MISSING8);
    if (residuals) data_save(&dat,obs,Rvar,MISSING8);
    if (squaredcorr) data_save(&dat,obs,R2var,MISSING8);
    if (resvariance) data_save(&dat,obs,Vvar,MISSING8);

    if (coeffs) {
        for (i=0; i<b_vars; i++) data_save(&dat,obs,Bvars[i],MISSING8);
    }
    if (stddevs) {
        for (i=0; i<s_vars; i++) data_save(&dat,obs,Svars[i],MISSING8);
    }
    if (tvalues) {
        for (i=0; i<t_vars; i++) data_save(&dat,obs,Tvars[i],MISSING8);
    }
}

static void make_null(QUEUE Q)
{
    Q->size=0;
    Q->front=1;
    Q->rear=0;
}

static void enqueue(e_type x, QUEUE Q)
{
    Q->size++;
    Q->A[Q->rear] = x;
    if (++Q->rear == Q->max_size) Q->rear=0;
}

static void enqueue2(e_type x, QUEUE Q, int col)
{
    if (col==0) {
        if (Q->size == (Q->max_size-1)) Q->size=-1;
        Q->size++;
    }
    Q->rear=(Q->size+Q->max_size*col);
    Q->A[Q->rear] = x;
}

static void update_varnames(void)
{
    int i;
    char comment[LNAME];

    if (prediction) put_varname(Pvar,"~Predicted values from MOVREG");
    if (residuals) put_varname(Rvar,"~Residuals from MOVREG");
    if (squaredcorr) put_varname(R2var,"~R^2 values from MOVREG");
    if (resvariance) put_varname(Vvar,"~Residual variances from MOVREG");

    if (coeffs) {
        put_varname(Bvars[0],"~Constant values from MOVREG");
        for (i=1; i<b_vars; i++) {
            strncpy(sbuf,dat.varname[Xvars[i-1]],9);
            sbuf[9]='\0'; trim2(sbuf);
            sprintf(comment,
                 "~Regression coefficients of %s from MOVREG", sbuf);
            put_varname(Bvars[i],comment);
        }
    }
    if (stddevs) {
        for (i=0; i<s_vars; i++) {
            strncpy(sbuf,dat.varname[Bvars[i]],9);
            sbuf[9]='\0'; trim2(sbuf);
            sprintf(comment, "~Std.devs of %s from MOVREG", sbuf);
            put_varname(Svars[i],comment);
        }
    }
    if (tvalues) {
        for (i=0; i<t_vars; i++) {
            strncpy(sbuf,dat.varname[Bvars[i]],9);
            sbuf[9]='\0'; trim2(sbuf);
            sprintf(comment,
                 "~t-values of %s from MOVREG", sbuf);
            put_varname(Tvars[i],comment);
        }
    }
}

static void put_varname(int m, char *str)
{
    int i,size;
    int offset;
    char *pd2;

    if (dat.varname[m][9]=='~') { /* permission */
        strncpy(sbuf,dat.varname[m],9);
        sbuf[9]='\0';
        strcat(sbuf,str);
        size=d2.l;
        for (i=strlen(sbuf); i<size; i++) sbuf[i]=' '; sbuf[i]='\0';
        d2.varname[m]=sbuf;
        pd2=&d2.varname[m][0];
        offset=d2.var+(int)d2.extra;
        offset+=(int)m*(size+d2.extra);
        fi_puts(&d2,pd2,size,offset);
    }
}
static void trim2(char *s) /* remove spaces from the end */
{
    while(*s) s++; s--;
    while(*s==' ') s--; s++;
    *s='\0';
}
