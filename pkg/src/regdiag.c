/* !regdiag.c 7.3.1987/SM (19.3.1992) (25.2.1995) (9.10.1996) (16.6.1997)
   Regression diagnostics
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define NSTEP 1000
#define MAX_X 500

// Autokorrelaatioita varten
#define MAXLAG 101

static SURVO_DATA dat;
static int yvar;
static int xvar[MAX_X];
static int mxvar,m;  /* if CONSTANT included, m=mxvar+1. Otherwise m=mxvar */
static int n;
static int konst;
static int results_line;
static int resvar,predvar,hatvar,studvar,cookvar;
static double eps;
static double rss;
static int singular;
static double vy;
static int prind;
static double r_square;
static double cond_number;
static double *X;
static double *Y;
static double *D;
static double *V;
static double *b;
static double *collength;
static double *hat;
static double *res;
static double *a;  /* auxiliary m*1 */
static double *se;
static double *invxtx;
static double *amm; /* auxiliary m*m */
static double ssy;

static double dw;  /* Durbin-Watson */
// static double dwp;
static int implicit_constant=0;

static FILE *dw_data;

static int lcn1[MAXLAG];
static double corr[MAXLAG],dwk0[MAXLAG],dwk[MAXLAG];
static double p_dw[MAXLAG];
static int maxlag;
static int nsimul;
static int dw_test; // 1=test based on DW, 0=test based on autocorr
static double corr0[MAXLAG];

static int find_regressors();
static int space_allocation1();
static int space_allocation();
static int not_enough_memory();
static int load_data();
static int scale_data();
static int save_variables();
static int save_rg_matrix();
static int n_strcat(char *x,int len,char *s);
static int linreg();
static int compute_hat();
static int compute_b();
static int compute_res();
static int compute_invxtx();
static int compute_reg_corr();
static int compute_vy();
static int print_coeff();
static int r2_without_c(double *pa);
static int check_1(double *X,int n,int m);
static int print_line(char *x);
static char *spois(char *s);
static double dw_probability(); //  24.6.2011  kuten 5 muuta alla
static int dw_stat(double *res,int n,double rss,double *pdw);
static double dw_probability2();
static int dw_lagged();
static int autocorr();
static int mtm1(double *T,double *X,double *Y,int  m,int n);

#define N_RG 17
static double rg[N_RG]; // 25.2.2005

#define _n        0
#define _k        1
#define _const    2
#define _const2   3
#define _df       4
#define _Yvar     5
#define _SST      6
#define _SSE      7
#define _SSR      8
#define _MSE      9
#define _Resvar  10
#define _R       11
#define _R2      12
#define _kappa   13
#define _DW      14
#define _F       15
#define _PF      16

static char *r_label;
/**************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "RESULTS", "PRIND", "EPS", "CONSTANT", "DWR", "DWDATA",
                 "DWN", "MAXLAG", "AUTOCORR", "!" };

char **specs=specs0;
***************************/
/*
void main(argc,argv)
int argc; char *argv[];
*/
void muste_regdiag(char *argv)
        {
        int i,h;
        char name[9];

//      if (argc==1) return;
        s_init(argv);
//      s_opt(argv[2]);

        if (g<2)
            {
            sur_print("\nUsage: REGDIAG <data>");
            WAIT; return;
            }
        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }
        i=sp_init(r1+r-1); if (i<0) return;
        i=data_read_open(word[1],&dat);
        if (i<0) return;
        i=mask(&dat); if (i<0) return;
        yvar=activated(&dat,'Y');
        if (yvar<0)
            {
            sur_print("\nNo regressand (Y)!"); WAIT; return;
            }
        i=find_regressors(); if (i<0) return;
        resvar=activated(&dat,'R');
        predvar=activated(&dat,'P');
        hatvar=activated(&dat,'H');
        studvar=activated(&dat,'S');
        cookvar=activated(&dat,'C');
/*
   printf("\nmxvar=%d",mxvar);
  for (i=0; i<mxvar; ++i) printf(" %d",xvar[i]); getch();
*/
        i=conditions(&dat); if (i<0) return;
        konst=m-mxvar;
        i=space_allocation1(); if (i<0) return;
/*******************
        i=optdim_d(); if (i && i<dat.m) err(0);
        i=optdim_o(); if (i && (int)i<dat.n) err(0);
**********************/
        prind=0;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        i=load_data(); if (i<0) return;
        i=space_allocation(); if (i<0) return;
/*
  printf("\nm=%d n=%d",m,n); getch();
  for (j=0; j<n; ++j) { printf("\n%g",Y[j]); for (i=0; i<m; ++i)
                                              printf(" %g",X[i*n+j]); getch(); }
*/
        eps=1e-15;
        i=spfind("EPS"); if (i>=0) eps=atof(spb[i]);

        i=scale_data(); if (i<0) return;
        i=linreg(); if (i<0) return;

        if (resvar>=0 || predvar>=0 || hatvar>=0 || studvar>=0 || cookvar>=0)
            {
            i=data_to_write(word[1],&dat);
            if (i<0)
                {
                sprintf(sbuf,"\nCannot write derived variables in data %s",
                                word[1]);
                WAIT; return;
                }
            i=save_variables();
            }

        print_coeff();

        for (i=0; i<m; ++i) b[i]/=collength[i];
        for (i=0; i<8*m; ++i) r_label[i]=' ';
        for (i=0; i<m; ++i)
            {
            if (i==0 && konst) strncpy(name,"Constant",8);
            else strncpy(name,dat.varname[xvar[i-konst]],8);
            for (h=0; h<8; ++h)
                {
                if (name[h]==EOS) break;
                r_label[8*i+h]=name[h];
                }
            }
        strcpy(sbuf,"regr("); strcat(sbuf,word[1]); strcat(sbuf,")");
        matrix_save("REG.M",b,m,1,r_label,"%1      ",8,8,-1,sbuf,0,0);
        for (i=0; i<m; ++i) b[m+i]=se[i]/collength[i];
        strcpy(sbuf,"regs("); strcat(sbuf,word[1]); strcat(sbuf,")");
        matrix_save("REGS.M",b,m,2,r_label,"Coeff   Std.dev.",8,8,-1,sbuf,0,0);
        strcpy(sbuf,"reg_corr("); strcat(sbuf,word[1]); strcat(sbuf,")");
        matrix_save("REG_CORR.M",invxtx,m,m,r_label,label,8,8,-1,sbuf,0,0);
        save_rg_matrix();

        s_end(argv[1]);
        }

static int find_regressors()
        {
        int i;

        mxvar=0;
        for (i=0; i<dat.m_act; ++i)
            if (dat.vartype[dat.v[i]][1]=='X')
                {
                if (mxvar>=MAX_X)
                    {
                    sprintf(sbuf,"\nMax.# of regressors is %d!",MAX_X);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                xvar[mxvar++]=dat.v[i];
                }
        if (mxvar==0)
            {
            sur_print("\nNo regressors (X)!"); WAIT; return(-1);
            }
        m=mxvar+1;
        i=spfind("CONSTANT");
        if (i<0) return(1);
        if (atoi(spb[i])==0) m=mxvar;
        return(1);
        }

static int space_allocation1()
        {
        b=(double *)muste_malloc(2*m*sizeof(double)); // 23.1.2005
        if (b==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int space_allocation()
        {
        int i;

        D=(double *)muste_malloc((unsigned int)(m*sizeof(double)));
        if (D==NULL) { not_enough_memory(); return(-1); }
        V=(double *)muste_malloc((unsigned int)(m*m*sizeof(double)));
        if (V==NULL) { not_enough_memory(); return(-1); }
        collength=(double *)muste_malloc((unsigned int)(m*sizeof(double)));
        if (collength==NULL) { not_enough_memory(); return(-1); }
        hat=(double *)muste_malloc((unsigned int)(n*sizeof(double)));
        if (hat==NULL) { not_enough_memory(); return(-1); }
        res=(double *)muste_malloc((unsigned int)((n+MAXLAG)*sizeof(double)));
        if (res==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<n+MAXLAG; ++i) res[i]=0.0; // autokorr. varten!
        a=(double *)muste_malloc((unsigned int)(m*sizeof(double)));
        if (a==NULL) { not_enough_memory(); return(-1); }
        se=(double *)muste_malloc((unsigned int)(m*sizeof(double)));
        if (se==NULL) { not_enough_memory(); return(-1); }
        invxtx=(double *)muste_malloc((unsigned int)(m*m*sizeof(double)));
        if (invxtx==NULL) { not_enough_memory(); return(-1); }
        amm=(double *)muste_malloc((unsigned int)(m*m*sizeof(double)));
        if (amm==NULL) { not_enough_memory(); return(-1); }
        r_label=muste_malloc(m*8);
        if (r_label==NULL) { not_enough_memory(); return(-1); }
i=0;
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (REGDIAG)"); WAIT;
        return(1);
        }

static int load_data()
        {
        int i;
        int j;
        double y;
        int tila;

        tila=NSTEP;
        X=(double *)muste_malloc((unsigned int)((m+1)*tila*sizeof(double)));
        if (X==NULL) { not_enough_memory(); return(-1); }
/* printf("tila=%d X=%d\n",tila,(int)X); getch(); */
        sur_print("\nLoading data... ");

        n=0L;
        for (j=dat.l1; j<=dat.l2; ++j)
            {
            if (n>=tila)
                {
                tila+=NSTEP;
                X=(double *)muste_realloc(X,(m+1)*tila*sizeof(double));
                if (X==NULL) { not_enough_memory(); return(-1); }
/* printf("tila=%d X=%d\n",tila,(int)X); getch(); */

                }

            if (unsuitable(&dat,j)) continue;
            data_load(&dat,j,yvar,&y); if (y==MISSING8) continue;
            for (i=0; i<mxvar; ++i)
                {
                data_load(&dat,j,xvar[i],&b[i]); if (b[i]==MISSING8) break;
                }
            if (i==mxvar)
                {
           //   if (kbhit()) { getch(); prind=1-prind; }
           //   if (prind) { sprintf(sbuf,"%ld ",j); sur_print(sbuf); }
                for (i=0; i<mxvar; ++i) X[(m+1)*n+i+konst]=b[i];
                if (konst) X[(m+1)*n]=1.0;
                X[(m+1)*n+m]=y;
                ++n;
                }
            }
        if (n<m) { sprintf(sbuf,"\nToo few observations (%d)!",n);
                   sur_print(sbuf); WAIT; return(-1);
                 }
        mat_transp_in_situ(X,m+1,n);
        Y=X+m*n;
/* printf("Y0=%g\n",Y[0]); getch(); */

        return(1);
        }

static int scale_data()
        {
        int i,j;
        double s;

        for (i=0; i<m; ++i)
            {
            s=0.0;
            for (j=0; j<n; ++j)
                {
                s+=X[i*n+j]*X[i*n+j];
                }
            if (s==0.0)
                {
                sprintf(sbuf,"\nVariable %.8s is 0!",dat.varname[xvar[i-konst]]);
                sur_print(sbuf); WAIT; return(-1);
                }

            collength[i]=sqrt(s);
            for (j=0; j<n; ++j) X[i*n+j]/=collength[i];
            }
        return(1);
        }

static int save_variables()
        {
        int i,jn;
        int j;
        double x;
        int miss;

        sur_print("\nSaving derived variables... ");
        jn=0;
        for (j=dat.l1; j<=dat.l2; ++j)
            {
//          if (kbhit()) { getch(); prind=1-prind; }
            if (unsuitable(&dat,j)) continue;
            miss=0;
            data_load(&dat,j,yvar,&x); if (x==MISSING8) { miss=1; }
            for (i=0; i<mxvar; ++i)
                {
                data_load(&dat,j,xvar[i],&x); if (x==MISSING8) { miss=1; break; }
                }
            if (i==mxvar && prind)
                {
                sprintf(sbuf,"%d ",j); sur_print(sbuf);
                }
            if (miss) x=MISSING8;
            if (resvar>=0)
                { if (!miss) x=res[jn]; data_save(&dat,j,resvar,x); }
            if (predvar>=0)
                { if (!miss) x=Y[jn]-res[jn]; data_save(&dat,j,predvar,x); }
            if (hatvar>=0)
                { if (!miss) x=hat[jn]; data_save(&dat,j,hatvar,x); }
            if (studvar>=0)
                {
                if (!miss)
                    {
                /*  x=res[jn]/sqrt(1.0-hat[jn])/
                    sqrt((rss-res[jn]*res[jn]/(1-hat[jn]))/(n-m-1));
                    printf("\nhat=%g %g",hat[jn],x); getch();
                */
                    x=((1-hat[jn])*rss-res[jn]*res[jn])/(n-m-1);
                    if (x<=0.0) x=MISSING8; else x=res[jn]/sqrt(x);
                    }
                data_save(&dat,j,studvar,x);
                }
            if (cookvar>=0)
                {
                if (!miss)
                    {
                    x=1-hat[jn];
                    if (x<=0.0) x=MISSING8;
                    else x=res[jn]*sqrt(hat[jn]/m/(rss/(n-m)))/(1-hat[jn]);
                    }
                data_save(&dat,j,cookvar,x);
                }

            if (!miss) ++jn;

            }

        if (resvar>=0) update_varname(&dat,resvar,"Residual from REGDIAG");
        if (predvar>=0) update_varname(&dat,predvar,"Predicted value from REGDIAG");
        if (hatvar>=0) update_varname(&dat,hatvar,"Diagonal of hat matrix from REGDIAG");
        if (studvar>=0) update_varname(&dat,studvar,"Studentized residual from REGDIAG");
if (cookvar>=0) update_varname(&dat,cookvar,"Cook's distance (signed sqrt) from REGDIAG");
        return(1);
        }
/*****************
_n
_k
_const
_df
_Yvar
_SST
_SSE
_SSR
_MSE
_Resvar
_R
_R2
_cond
_DW
*******************/

#define ERC 128
#define N_LINES 18
static int save_rg_matrix()
    {
    extern double dw;
    extern double muste_cdf_f();
    extern int implicit_constant;
    char label[8*N_RG+1];
    char text[N_LINES*ERC+1];
    char text1[50];
    int const0;

    strcpy(label,"n       k       const   const2  df      Yvar    ");
    strcat(label,"SST     SSE     SSR     MSE     Resvar  ");
    strcat(label,"R       R2      kappa   DW      F       ");
    strcat(label,"Pr_F    ");

    rg[_n]=(double)n;
    rg[_k]=(double)m;
    rg[_const]=(double)konst;
    rg[_const2]=(double)(implicit_constant);

    if (konst || implicit_constant) const0=1; else const0=0; // 12.2.2005

    rg[_df]=(double)(n-m);
    rg[_Yvar]=vy;
    rg[_SST]=(n-1)*vy;
    if (const0==0) rg[_SST]=ssy; // 10.2.2005
    rg[_SSE]=rss;
    rg[_SSR]=rg[_SST]-rg[_SSE];
    rg[_MSE]=rss/(n-m);
    rg[_Resvar]=rg[_MSE];
    rg[_R]=sqrt(r_square);
    rg[_R2]=r_square;
    rg[_kappa]=cond_number;
    rg[_DW]=dw;
    rg[_F]=rg[_SSR]/(m-const0)/rg[_MSE];
    rg[_PF]=1.0-muste_cdf_f(rg[_F],rg[_k]-(double)const0,rg[_df],1e-15);

    *text=EOS;
    sprintf(text1,"REGDIAG statistics from data %s",word[1]);
    n_strcat(text,ERC,text1);
    n_strcat(text,ERC,"/n: # of cases");
    n_strcat(text,ERC,"/k: # of regression coefficients");
    n_strcat(text,ERC,"/const: 1 if constant term included, 0 otherwise");
    n_strcat(text,ERC,"/const2: 1 if implicit constant included, 0 otherwise");
    n_strcat(text,ERC,"/df: n-k");
    n_strcat(text,ERC,"/Yvar: variance of regressand");
    n_strcat(text,ERC,"/SST: total sum of squares");
    n_strcat(text,ERC,"/SSE: residual sum of squares");
    n_strcat(text,ERC,"/SSR: SST-SSE");
    n_strcat(text,ERC,"/MSE: mean square error");
    n_strcat(text,ERC,"/Resvar: MSE");
    n_strcat(text,ERC,"/R: multiple correlation coefficient");
    n_strcat(text,ERC,"/R2: R^2");
    n_strcat(text,ERC,"/kappa: condition number");
    n_strcat(text,ERC,"/DW: Durbin-Watson statistics");
    n_strcat(text,ERC,"/F: Overall F-test");
    n_strcat(text,ERC,"/Pr_F: P-value of F-test");

    matrix_save0("RG.M",rg,N_RG,1,label,"REGDIAG ",8,8,-1,"RG.M",
                  N_LINES,0,text);
    return(1);
    }

static int n_strcat(char *x,int len,char *s)
    {
    strcat(x,s); strncat(x,space,len-strlen(s));
    return(1);
    }

/* regd2.c 7.3.1987/SM (8.3.1987) (24.2.1997) (15.6.1997)
   Regression diagnostics
*/
static int linreg()
        {
        int i;
        char x[LLENGTH];

        output_open(eout);
        sprintf(x,"Regression diagnostics on data %s: N=%d",word[1],n);
        print_line(x);
        i=sprintf(x,"Regressand %s  # of regressors=%d",dat.varname[yvar],m);
        if (konst) sprintf(x+i," (Constant term included)");
        print_line(x);
        sur_print("\nSingular value decomposition of X...");
        mat_svd(X,D,V,n,m,1e-16,1e-300);
        sur_print("\nSing.values: "); for (i=0; i<m; ++i)
                         { sprintf(sbuf,"%g ",D[i]); sur_print(sbuf); }
        implicit_constant=0;
        if(m==mxvar) implicit_constant=check_1(X,n,m);  /* 15.6.1997 */

        singular=0;
        cond_number=D[0]/D[m-1];
        if (D[m-1]<eps)
            {
            sprintf(x,"Matrix X of regressors is singular!");
            singular=1;
            }
        else
            sprintf(x,"Condition number of scaled X: k=%g",cond_number);
        if (implicit_constant)
            strcat(x,"  Constant term implicitly included.");
        print_line(x);
        compute_hat();
        compute_b();
        compute_res();
        compute_invxtx();
        for (i=0; i<m; ++i) se[i]=sqrt(invxtx[i+m*i]*rss/(n-m));

        compute_vy();
        if (vy==0.0)
            {
            sprintf(sbuf,"\nRegressand %s is a constant=%g",dat.varname[yvar],Y[0]);
            sur_print(sbuf); WAIT; return(-1);
            }

        compute_reg_corr(); // 11.1.2005

//      print_coeff();
        return(1);
        }

static int compute_hat()
        {
        int i,j;
        double s;

        for (j=0; j<n; ++j)
            {
            s=0; for (i=0; i<m; ++i) s+=X[i*n+j]*X[i*n+j];
            hat[j]=s;
            }
        return(1);
        }

static int compute_b()
        {
        int i,j;
        double s;

        for (i=0; i<m; ++i)
            {
            if (D[i]<eps) b[i]=0.0; else b[i]=1/D[i];
            }           /* b temporarily = D+ */
        for (i=0; i<m; ++i)
            {
            s=0.0; for (j=0; j<n; ++j) s+=X[i*n+j]*Y[j];
            res[i]=b[i]*s; /* temp. */
            }
        for (i=0; i<m; ++i)
            {
            s=0.0;
            for (j=0; j<m; ++j) s+=V[i+m*j]*res[j];
            b[i]=s;
            }
        return(1);
        }

static int compute_res()
        {
        int i,j;
        double s;

        for (i=0; i<m; ++i)
            {
            s=0.0;
            for (j=0; j<n; ++j) s+=X[i*n+j]*Y[j];
            a[i]=s;
            }
        rss=0.0;
        dw=0.0;
        for (j=0; j<n; ++j)
            {
            s=Y[j];
            for (i=0; i<m; ++i) s-=X[i*n+j]*a[i];
            res[j]=s;
            rss+=s*s;
            if (j>0) dw+=(s-res[j-1])*(s-res[j-1]);
            }
//      if (rss>0.0) dw/=rss;
        if (rss>1e-25) dw/=rss; else dw=0.0;
        return(1);
        }

static int compute_invxtx()
        {
        int i,j,k;
        double s;

        for (j=0; j<m; ++j)
            {
            if (D[j]<eps) s=0.0; else s=1/D[j];
            for (i=0; i<m; ++i) amm[i+m*j]=s*V[i+m*j];
            }
        for (i=0; i<m; ++i)
            {
            for (j=0; j<m; ++j)
                {
                s=0.0; for (k=0; k<m; ++k) s+=amm[i+m*k]*amm[j+m*k];
                invxtx[i+m*j]=s;
                }
            }
//      mprint(invxtx,m,m);

        return(1);
        }

static int compute_reg_corr()
    {
    int i,j;

    for (i=0; i<m; ++i) a[i]=1.0/sqrt(invxtx[(m+1)*i]);
    for (i=0; i<m; ++i)
        for (j=0; j<m; ++j)
            {
            invxtx[i+m*j]*=a[i]*a[j];
            }
//  mprint(invxtx,m,m);
    return(1);
    }

static int compute_vy()
        {
        int j;
        double s1,s2;

        s1=s2=0.0;
        for (j=0; j<n; ++j) { s1+=Y[j]; s2+=Y[j]*Y[j]; }
        ssy=s2; // 10.2.2005;
        vy=(s2-s1*s1/n)/(n-1);
        return(1);
        }

static int print_coeff()
        {
        extern char *spois();
        int i,k,h;
        char x[LLENGTH];
        char sana1[32],sana2[32],sana3[32],sana4[32],sana5[32];
        int ac=accuracy+3;
        double a;
        extern double dw_probability2();
//      extern double dw_probability3();

        print_line("Variable Regr.coeff. Std.dev.     t");

        for (i=0; i<m; ++i)
            {
            if (konst && i==0) k=sprintf(x,"Constant");
            else k=sprintf(x,"%8.8s",dat.varname[xvar[i-konst]]);
            fnconv(b[i]/collength[i],ac,sana1);
            *sana2=*sana3=EOS;
            if (!singular)
                {
                fnconv(se[i]/collength[i],ac,sana2);
                if (se[i]<1e-12) strcpy(sana3,"-"); // 16.7.2004
                else fnconv(b[i]/se[i],ac-3,sana3);
                }
            k+=sprintf(x+k," %*.*s %*.*s %*.*s",ac,ac,sana1,ac,ac,sana2,ac,ac,sana3);
            print_line(x);
            }

        strncpy(sana1,dat.varname[yvar],8); sana1[8]=EOS;
        h=8; while(sana1[h-1]==' ') sana1[--h]=EOS;
        h=sprintf(x,"Variance of regressand %s",sana1);
        fnconv(vy,accuracy+5,sana1);
        h+=sprintf(x+h,"=%s df=%d",spois(sana1),n-1);
        print_line(x);
        fnconv(rss/(n-m),accuracy+5,sana1);
        h=sprintf(x,"Residual variance=%s df=%d",spois(sana1),n-m);
        print_line(x);
        if (m==mxvar && !implicit_constant) r2_without_c(&a);  /* 27.2.1997 */
                        /* 15.6.1997 */
        else a=1.0-rss/((n-1)*vy);
        r_square=a; // 25.1.2005
        fnconv(sqrt(a),accuracy,sana1);
        h=sprintf(x,"R=%s",spois(sana1));
        fnconv(a,accuracy,sana1);
        h+=sprintf(x+h," R^2=%s",spois(sana1));

        if (dw>0.0)
             {
             fnconv(dw,accuracy-1,sana1);
             h+=sprintf(x+h," Durbin-Watson=%s",spois(sana1));


             a=dw_probability2();
             if (a==MISSING8)
                 a=dw_probability();
             if (a!=MISSING8)
                 {
                 *sbuf=EOS;
                 if (a<0.05)
                     strcpy(sbuf,"Autocorr>0");
                 else if (a>0.95)
                     strcpy(sbuf,"Autocorr<0");
                 fnconv(a,accuracy,sana1);
                 h+=sprintf(x+h," (P=%s) %s",spois(sana1),sbuf);
                 }


             }
        print_line(x);
        if (maxlag)
            {
  print_line("Lag   Autocorr. DW       P(positive)  P(negative)  s.e. of P");
            for (i=1; i<=maxlag; ++i)
                {
                sprintf(sana1,"%7.*f",accuracy-3,corr0[i]);
                sprintf(sana2,"%.*f",accuracy-3,dwk0[i]);
                sprintf(sana3,"%.*f",accuracy-3,1.0-p_dw[i]);
                sprintf(sana4,"%.*f",accuracy-3,p_dw[i]);

                a=sqrt(p_dw[i]*(1.0-p_dw[i])/(double)nsimul);
                sprintf(sana5,"%.*f",accuracy-3,a);
                if (a==0) strcpy(sana5,"-");

                sprintf(sbuf,"%3d   %s   %s   %s       %s       %s",
                         i,sana1,sana2,sana3,sana4,sana5);
                print_line(sbuf);
                }
            if (dw_test==1)
                print_line("Randomized tests are based on DW values.");
            else
                print_line("Randomized tests are based on autocorrelations.");
            }
        return(1);
        }

static int r2_without_c(double *pa) /* 27.2.1997 */
        {
        int j;
        double s1,s2,a;

        s1=s2=0.0;
        for (j=0; j<n; ++j)
            {
            s1+=Y[j]*Y[j];
            a=Y[j]-res[j]; s2+=a*a;
            }
        *pa=s2/s1;
        return(1);
        }

static int check_1(double *X,int n,int m) /* 15.6.1997 */
        {
        int i,j,k;
        double a,b;

        for (j=0; j<n; ++j)
            {
            a=0.0;
            for (i=0; i<n; ++i)
                {
                b=0.0;
                for (k=0; k<m; ++k) b+=X[i+n*k]*X[j+n*k];
                a+=b;
                }
            if (fabs(a-1.0)>1e-10) break;
            }
        if (j==n) return(1);
        return(0);
        }

static int print_line(char *x)
        {
        output_line(x,eout,results_line);
        if (results_line) ++results_line;
        if (results_line>r2) results_line=0;
        return(1);
        }

static char *spois(char *s)
        {
        while (*s && *s==' ') ++s;
        return(s);
        }

/* regdw.c 29.3.2004/SM (8.3.1987) (24.2.1997) (15.6.1997)
   Regression diagnostics: DW probability

   DWN neliömuotojen suhde
   RN  autokorrelaatio
*/

static double dw_probability()
    {
    int i,j,k;
    int ln,ln1;
    double dp;
    double dw0,dw;
    double rss;
    int dwdata=0;
    extern double uniform_dev();

    dp=MISSING8;
    i=spfind("DWR");
    if (i<0) return(MISSING8);
    nsimul=atol(spb[i]);

    i=spfind("DWDATA");
    if (i>=0) dwdata=atoi(spb[i]);

// printf("\nnsimul=%ld|",nsimul); getch();
// printf("\nn=%d res1=%g resn=%g|",n,res[0],res[n-1]); getch();
    rss=0.0;
    for (j=0; j<n; ++j)
        {
        rss+=res[j]*res[j];
        }
    dw_stat(res,n,rss,&dw0);

    if (dwdata) dw_data=muste_fopen("DWDATA.TXT","wt");
    spec_rnd();
    ln1=0L;
    for (ln=0L; ln<nsimul; ++ln)
        {
        for (j=n-1; j>0; --j)
            {
            double a;

            k=(int)((j+1)*uniform_dev());
            a=res[k]; res[k]=res[j]; res[j]=a;
            }
        dw_stat(res,n,rss,&dw);
         if (dwdata) fprintf(dw_data,"%g\n",dw);
        if (dw<dw0) ++ln1;
// printf("%g ",dw);
        }
    if (dwdata) muste_fclose(dw_data);
    dp=(double)ln1/(double)nsimul;

    return(dp);
    }

static int dw_stat(double *res,int n,double rss,double *pdw)
    {
    int j;
    double dw,a;

    dw=0.0;
    for (j=1; j<n; ++j)
        {
        a=res[j]-res[j-1];
        dw+=a*a;
        }
    *pdw=dw/rss;
    return(1);
    }


static double dw_probability2()
    {
    int i,j;
    int ln,ln1;
    double dp;
    double dw0;
    int dwdata=0;
    double s,s1,s2;
    double rss0;
    extern double normal_dev();
    extern double dw;


    spec_rnd(); // 26.7.2011/SM
    dp=MISSING8;
    i=spfind("DWN");
    if (i>=0) dw_test=1;
    else
        {
        i=spfind("RN");
        if (i<0) return(MISSING8);
        dw_test=0;
        }
    nsimul=atol(spb[i]);
    if (nsimul<10000L)
        {
        sprintf(sbuf,"\n\nSet # of replicates DWN=10000 at least!");
        sur_print(sbuf); WAIT; return(1);
        }

    maxlag=0;
    i=spfind("MAXLAG");
    if (i<0) i=spfind("AUTOCORR");
    if (i>=0) maxlag=atoi(spb[i]);
    if (maxlag>MAXLAG-1) maxlag=MAXLAG-1;

    if (maxlag)
        {
        autocorr();
        for (i=1; i<=maxlag; ++i) { corr0[i]=corr[i]; lcn1[i]=0L; }
        dw_lagged();
        for (i=1; i<=maxlag; ++i) { dwk0[i]=dwk[i]; lcn1[i]=0L; }
        }
    rss0=rss;

    i=spfind("DWDATA");
    if (i>=0) dwdata=atoi(spb[i]);

    sprintf(sbuf,"\n\nP value for DW by simulation: %d replicates...",
                                                 nsimul);
    sur_print(sbuf);

    dw0=dw;

    spec_rnd();

    if (dwdata) dw_data=muste_fopen("DWDATA.TXT","wt");

    ln1=0L;
    for (ln=0L; ln<nsimul; ++ln)
        {
        for (j=0; j<n; ++j)
            Y[j]=normal_dev();
        mtm1(b,X,Y,n,m);
        mat_mlt(res,X,b,n,m,1);
        for (j=0; j<n; ++j) res[j]=Y[j]-res[j];
        s1=s2=0.0;
        for (j=0; j<n; ++j)
            {
            s=res[j];
            s1+=s*s;
            if (j>0) s2+=(s-res[j-1])*(s-res[j-1]);
            }
        rss=s1;
        dw=s2/s1;
        if (dwdata) fprintf(dw_data,"%g\n",dw);
        if (dw<dw0) ++ln1;
        if (maxlag)
            {
            if (dw_test)
                {
                dw_lagged();
                for (i=1; i<=maxlag; ++i)
                    if (dwk[i]>=dwk0[i]) ++lcn1[i];
                }
            else
                {
                autocorr();
                for (i=1; i<=maxlag; ++i)
                    if (corr[i]<corr0[i]) ++lcn1[i];
                }
            }
        }
    if (dwdata) muste_fclose(dw_data);
    dw=dw0;
    dp=(double)ln1/(double)nsimul;
    rss=rss0;
    if (maxlag)
        {
        for (i=1; i<=maxlag; ++i)
            p_dw[i]=(double)lcn1[i]/(double)nsimul;
        }
    return(dp);
    }


static int dw_lagged()
    {
    int j;
    int k;
    double resj;
    double a;

    for (k=1; k<=maxlag; ++k) dwk[k]=0.0;

    for (j=0; j<n; ++j)
        {
        resj=res[j];
        for (k=1; k<=maxlag; ++k)
            {
            if (j+k<n) { a=resj-res[j+k]; dwk[k]+=a*a; }
            }
        }
    for (k=1; k<=maxlag; ++k) dwk[k]/=rss;
    return(1);
    }

static int autocorr()
    {
    int j;
    int k;
    double resj;

    for (k=1; k<=maxlag; ++k) corr[k]=0.0;

    for (j=0; j<n; ++j)
        {
        resj=res[j];
        for (k=1; k<=maxlag; ++k)
            corr[k]+=resj*res[j+k];  // res-lopussa ainakin k kpl nollia
        }
    for (k=1; k<=maxlag; ++k) corr[k]/=rss;
    return(1);
    }

static int mtm1(double *T,double *X,double *Y,int  m,int n)     //  T=X'*y
        {
        int i,k;
        double *px,*py,s;

        for (i=0; i<n; ++i)
            {
//              T[i+n*j]=sis_tulo(X+m*i,Y+m*j,1,1,m);
                s=0.0; px=X+m*i; py=Y;
                for (k=0; k<m; ++k) { s+=(*px)*(*py); ++px; ++py; }
//              for (k=0; k<m; ++k) s+=(*px++)*(*py++);
                T[i]=s;

            }
        return(1);
        }
