/* _genreg.c 7.3.1988/SM (23.12.1990) (5.7.1996) (24.6.1997)
*/

#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
// #include <process.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;
static char data[LLENGTH];
static int results_line;
static int nxvar,yvar,weightvar;
static int xvar[EP4];
static unsigned int n,m,m1;
static int nbinvar;
static int resvar,predvar;
static char tempd[LLENGTH];

static FILE *temp;
static double *Y;
static double *W;
static double *X;
static double *V;
static char *lab;
static double *b;
static double *sb;
static double *my;
static int *u;
static double deviance;
static int df;
static unsigned int ig; /* index for observations 0,1,2,...,n-1 */

static unsigned int n0;
static int pp[3];
static int templen;
static int prind=0;

/*
  W       1
  Y       1
  X       m1
*/

#define iW 0
#define iY 1
#define iX 2

static double *XX,*XY;
static double *a_fit,*lmy_fit,*z_fit;
static int *idel_fit;
static char *lab_fit;

static double (*flink)();
static double (*filink)();
static double (*dfilink)();
static double (*vf)();
static double (*devf)();
static double (*fscale)();

static int varaa_tilat();
static int lue_datat();
static int temp_save(double u);
static double temp_load();
static int lue_temp();
static int not_enough_memory();
static int save_variables(char *data);
static int fit();
static int regr_talletus();
static char *spois(char *s);
static int print_line(char *x);
static double idn(double x);
static double one(double x);
static double log0(double x);
static double inv0(double x);
static double square(double x);
static double twice(double y);
static double logit(double y);
static double ilogit(double y);
static double dilogit(double y);
static double probitt(double y);
static double iprobit(double y);
static double diprobit(double y);
static double complog(double y);
static double icomplog(double y);
static double dicomplog(double y);
static double sqroot(double y);
static double exponent(double y);
static double iexponent(double y);
static double diexponent(double y);
static double digamma(double y);
static double devlog(double y,double my);
static double devlin(double y,double my);
static double devbin(double y,double my);
static double devgamma(double y,double my);
static double scale1(double dev,double df);
static double scale_normal(double dev,double df);
static double vfbin(double my);
static int lue_error();
static int lue_link();
static int glm_fit(double (*flink)(),double (*filink)(),double (*dfilink)(),
    double (*vf)(),double (*devf)(),double (*fscale)(),
    double *X,int nx,int mx,double *Y,double *W,double *V,
    char *lab,double *b,double *sb,double *my,double *pdev,int *pdf);
static int glm_fit_space(int mx,int nx);
static int mspace(double **A,int m,int n);
static int std_errors(double *R,int mx,double *sb,char *lab,double ss);
static int reduce(double *X,char *lab,int nx,int mx,int i1);
static int restore(double *b,double *sb,int ndel,int mx);
static int mat_cholinv_genreg(double *a,int n,double eps);
// static int mprint(double *X,int m,int n);

/*********************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "RESULTS", "PRIND", "TEMP", "ERROR", "LINK",
char **specs=specs0;
***********************/

/**********************
main(argc,argv)
int argc; char *argv[];
***********************/

void muste_genreg(char *argv)
        {
        int i;

        s_init(argv);
        if (g<2)
            {
            sur_print("\nUsage: GENREG <SURVO_data>,<output_line>");
            WAIT; return;
            }
        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line<0) return;
            }

        strcpy(data,word[1]);
        i=data_read_open(data,&d); if (i<0) return;
        i=sp_init(r1+r-1);
        if (i<0)
            {
            sur_print("\nToo many specifications!");
            WAIT; return;
            }
        i=mask(&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;  /* permitted only once */
        i=spfind("RESULTS"); if (i>=0) results=atoi(spb[i]);
        prind=0;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        strcpy(tempd,etmpd);
        i=spfind("TEMP"); if (i>=0) strcpy(tempd,spb[i]);
        strcat(tempd,"SURVO.TMP");

        weightvar=activated(&d,'W');
        resvar=activated(&d,'R');
        predvar=activated(&d,'P');
        nxvar=0; yvar=-1;
        for (i=0; i<d.m; ++i)
            {
            char ch=d.vartype[i][1];

            if (nxvar>=EP4)
                {
                sur_print("\nToo many regressors!");
                WAIT; return;
                }
            if (ch=='X') xvar[nxvar++]=i;
            if (ch=='Y')
                {
                if (yvar>=0)
                    {
                    sur_print("\nOnly one regressand (Y) permitted!");
                    WAIT; return;
                    }
                yvar=i;
                }
            }
        if (!nxvar)
                {
                sur_print("\nNo regressors (X)!");
                WAIT; return;
                }
        if (yvar<0)
                {
                sur_print("\nNo regressand (Y)!");
                WAIT; return;
                }
/*
printf("\nweightvar=%d yvar=%d",weightvar,yvar);
printf("\nregressors:\n");
for (i=0; i<nxvar; ++i) printf("%d ",xvar[i]);
getch();
*/
        m=nxvar;
        nbinvar=activated(&d,'N');
        m1=m; if (nbinvar>=0) { ++m1; xvar[nxvar]=nbinvar; }

        i=lue_datat(); if (i<0) return;
        i=varaa_tilat(); if (i<0) return;
        i=lue_temp(); if (i<0) return;
        i=fit(); if (i<0) { s_end(argv); return; }
        i=save_variables(word[1]);
    //  fclose(temp);   9.8.2011
        remove(tempd);
/*      sprintf(sbuf,"DEL %s",tempd);
        system(sbuf);
*/
        data_close(&d);
        free(X); free(Y); free(W);
        free(V); free(b); free(sb); free(my); free(lab); free(u);
        s_end(argv);
        }


static int varaa_tilat()
        {
/* n0 same as n */

        if (n0>0)
            {
            X=(double *)malloc(n0*m1*sizeof(double));
            if (X==NULL) { not_enough_memory(); return(-1); }
            Y=(double *)malloc((unsigned int)(n0*sizeof(double)));
            if (Y==NULL) { not_enough_memory(); return(-1); }
            W=(double *)malloc((unsigned int)(n0*sizeof(double)));
            if (W==NULL) { not_enough_memory(); return(-1); }
            }
        V=(double *)malloc((unsigned int)(n*sizeof(double)));
        if (V==NULL) { not_enough_memory(); return(-1); }
        b=(double *)malloc((unsigned int)(m*sizeof(double)));
        if (b==NULL) { not_enough_memory(); return(-1); }
        sb=(double *)malloc((unsigned int)(m*sizeof(double)));
        if (sb==NULL) { not_enough_memory(); return(-1); }
        my=(double *)malloc((unsigned int)(n*sizeof(double)));
        if (my==NULL) { not_enough_memory(); return(-1); }
        lab=malloc(8*m+1);
        if (lab==NULL) { not_enough_memory(); return(-1); }
        u=(int *)malloc((unsigned int)(m1*sizeof(int)));
        if (u==NULL) { not_enough_memory(); return(-1); }

        return(1);
        }

static int lue_datat()
        {
        int j;
        double a,w,y;
        int miss,i;

        for (i=0; i<3; ++i) pp[i]=i*sizeof(double);
        templen=pp[2]+m1*sizeof(double);

        temp=muste_fopen(tempd,"wb");
        sur_print("\nLoading data... ");
        n=0;
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (ferror(temp))
                {
                sprintf(sbuf,"\nCannot save temporary data in %s!",tempd);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (unsuitable(&d,j)) continue;
            if (weightvar>=0)
                {
                data_load(&d,j,weightvar,&w);
                if (w==MISSING8) continue;
                }
            else w=1.0;
            data_load(&d,j,yvar,&y);
            if (y==MISSING8) continue;
            miss=0;
            for (i=0; i<m1; ++i)
                {
                data_load(&d,j,xvar[i],&a);
                if (a==MISSING8) { miss=1; break; }
                }
            if (miss) continue;
            temp_save(w);
            temp_save(y);
            for (i=0; i<m1; ++i)
                {
                data_load(&d,j,xvar[i],&a);
                temp_save(a);
                }
            ++n;
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf,"%d ",j); sur_print(sbuf); }
            }
        fclose(temp);
        if (n<nxvar)
            {
            sprintf(sbuf,"\nToo few observations (%d) selected!",n);
            sur_print(sbuf); WAIT;
            }
        n0=n;
        return(1);
        }

static int temp_save(double u)
        {
        char *p;
        int i;

        p=(char *)&u;
        for (i=0; i<sizeof(double); ++i) { putc((int)*p,temp); ++p; }
        return(1);
        }
static double temp_load()
        {
        double u;
        char *p;
        int i;

        p=(char *)&u;
        for (i=0; i<sizeof(double); ++i) { *p=(char)getc(temp); ++p; }
        return(u);
        }

static int lue_temp()
        {
        int i,j;

        for (i=0; i<m1; ++i) u[i]=i;
        temp=muste_fopen(tempd,"rb");
        for (j=0; j<n; ++j)    /* n0 aikaisemmin! */
            {
            W[j]=temp_load();
            Y[j]=temp_load();
            for (i=0; i<m1; ++i) X[(int)u[i]*(int)n0+(int)j]=temp_load();
            }
        fclose(temp); // 9.8.2011/SM
        return(1);
        }


static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (GENREG)");
        WAIT; return(1);
        }

static int save_variables(char *data)
        {
        int i,miss;
        int j;
        double x;

        if (resvar<0 && predvar<0) return(1);
        i=data_to_write(data,&d);
        if (i<0) { sprintf(sbuf,"\nCannot write residuals etc. in %s!",data);
                   sur_print(sbuf); WAIT; return(-1);
                 }

        sur_print("\nSaving predicted values and residuals... ");
        ig=0;
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            if (weightvar>=0)
                {
                data_load(&d,j,weightvar,&x);
                if (x==MISSING8) continue;
                }
            data_load(&d,j,yvar,&x);
            if (x==MISSING8) continue;
            miss=0;
            for (i=0; i<m1; ++i)
                {
                data_load(&d,j,xvar[i],&x);
                if (x==MISSING8) { miss=1; break; }
                }
            if (miss) continue;
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf,"%d ",j); sur_print(sbuf); }

            if (resvar>=0)
                data_save(&d,j,resvar,(Y[ig]-my[ig])/sqrt((*vf)(my[ig])));

            if (predvar>=0)
                data_save(&d,j,predvar,my[ig]);
            ++ig;
            }

if (resvar>=0)
    update_varname(&d,resvar,"(Scaled) Pearson residuals from GENREG");
if (predvar>=0)
    update_varname(&d,predvar,"Predicted value from GENREG");
        return(1);
        }

/* gen2.c 8.3.1988/SM (23.12.1990)
*/
/*********************************
extern SURVO_DATA d;
extern char data[LLENGTH];
extern int results_line;
extern int nxvar,yvar,weightvar;
extern int xvar[EP4];
extern unsigned int n,m;
extern int nbinvar;

extern FILE *temp;
extern double *Y;
extern double *W;
extern double *X;
extern double *V;
extern char *lab;
extern double *b;
extern double *sb;
extern double *my;
extern double deviance;
extern int df;
extern unsigned int n0;

extern double cdf_chi2();
************************************/


static char g_error[LLENGTH],g_link[LLENGTH];
static int ng_error,ng_link;








static int math_error=0;
static double alpha; /* LINK=E,alpha */

static int fit()
        {
        int i,j,k;
        char s[LLENGTH];
   //   double da;
        char s1[32],s2[32];
   //   int digits;
   //   char nimi[LLENGTH];
        char *spois();

        math_error=0;
        *lab=EOS;
        for (i=0; i<m; ++i) strncat(lab,d.varname[xvar[i]],8);
        for (j=0; j<n; ++j) V[j]=Y[j];

        i=lue_error(); if (i<0) return(-1);
        i=lue_link(); if (i<0) return(-1);

        if (math_error) return(-1);
        i=glm_fit(flink,filink,dfilink,vf,devf,fscale,
                      X,n,m,Y,W,V,lab,b,sb,my,&deviance,&df);
        k=output_open(eout); if (k<0) return(-1);
        if (i==-2)
            {
            print_line("Model is saturated!");
            return(-1);
            }
        if (i<0) return(-1);
/*      da=1-cdf_chi2(deviance,(double)df,1e-7);        */
        fnconv(deviance,accuracy+2,s1);
        sprintf(s,"Data %s: Deviance=%s df=%d",
                       word[1],spois(s1),df);
        print_line(s);
// i=8; strncpy(s1,d.varname[yvar],8); while (s1[i-1]==' '&i>0) --i;
// 9.8.2011/SM
 i=8; strncpy(s1,d.varname[yvar],8); while (i>0 && s1[i-1]==' ') --i;
        sprintf(s,"Yvariate=%.*s ERROR=%s LINK=%s",i,s1,g_error,g_link);
        print_line(s);
        if (results>0)
            {
            sprintf(s,"Parameter    Estimate%.*s  s.e.",accuracy-4,space);
            print_line(s);
            for (i=0; i<m; ++i)
                {
                if (sb[i]<0) { sprintf(s1," -      %.*s",accuracy-4,space);
                               sprintf(s2," aliased%.*s",accuracy-4,space);
                             }
                else { fnconv(b[i],accuracy+2,s1);
                       fnconv(sb[i],accuracy+2,s2);
                     }
        /*      sprintf(s,"%3d %s  %s  %.8s",i+1,s1,s2,lab+8*i);  */
    sprintf(s,"%.8s    %.*s    %.*s",lab+8*i,accuracy+2,s1,accuracy+2,s2);
                print_line(s);
                }
            print_line(" ");
            }
        output_close(eout);

        regr_talletus();
        return(1);
        }

static int regr_talletus()
        {
        char expr[LLENGTH];
        char name[9];

        strncpy(name,d.varname[yvar],8); name[8]=EOS;
        strcpy(expr,"genreg("); strcat(expr,data); strcat(expr,")");
        matrix_save("GENREG.M",b,m,1,lab,name,8,8,-1,expr,0,0);
        return(1);
        }

static char *spois(char *s)
        {
        while (*s==' ') ++s;
        return(s);
        }

static int print_line(char *x)
        {
        output_line(x,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

static double idn(double x)
        { return(x); }
static double one(double x)
        { return(1.0); }
static double log0(double x)
        {
        if (x<0.0)
            {
            math_error=1;
            sur_print("\nLINK=LOG: Negative values not allowed!"); WAIT;
            return(0.0);
            }
        if (x==0.0) return(0.5);
        return(log(x));
        }
static double inv0(double x)
        {
        if (x==0.0)
            {
            math_error=1;
            sur_print("\nZeros not allowed!"); WAIT;
            return(0.0);
            }
        return(1.0/x);    // 24.11.2003 ennen log(1.0/x)
        }
static double square(double x)
        { return(x*x); }
static double twice(double y)
        { return(2*y); }

static double logit(double y)
        {
        double a;

        a=X[ig+nxvar*n0];
        if (y>a || y<0.0)
            {
            math_error=1;
            sur_print("\nIllegal value in logit transformation!"); WAIT;
            return(0.0);
            }
        return ( log((y+0.5)/(a-y+0.5)) );
        }

static double ilogit(double y)
        {
        double a;

        a=X[ig+nxvar*n0];
        return ( a/(1.0+exp(-y)) );
        }

static double dilogit(double y)
        {
        double a,b,c;

        a=X[ig+nxvar*n0];
        if (a<=0.0) a=0.1;
        b=exp(-y); c=b+1.0;
        return ( a*b/(c*c) );
        }
static double probitt(double y)
        {
        double a;
        extern double muste_inv_std();

        a=X[ig+nxvar*n0];
        if (y>a || y<0.0)
            {
            math_error=1;
            sur_print("\nIllegal value in probit transformation!"); WAIT;
            return(0.0);
            }
        return ( muste_inv_std((y+0.5)/(a+0.6)) );
        }
static double iprobit(double y)
        {
        double a;
        extern double muste_st_norm();

        a=X[ig+nxvar*n0];
        return ( a*muste_st_norm(y,0.0) );
        }
static double diprobit(double y)
        {
        double a;

        a=X[ig+nxvar*n0];
        if (a<=0.0) a=0.1;
        return ( a/sqrt((double)(2.0*3.14159265))*exp(-y*y/2) );
        }

static double complog(double y)
        {
        double a;

        a=X[ig+nxvar*n0];
        if (y>a || y<0.0)
            {
            math_error=1;
   sur_print("\nIllegal value in complementary log-log transformation!"); WAIT;
            return(0.0);
            }
        return ( log(-log((a-y+0.5)/(a+0.6))) );
        }
static double icomplog(double y)
        {
        double a;

        a=X[ig+nxvar*n0];
        return ( a*(1.0-exp(-exp(y))) );
        }
static double dicomplog(double y)
        {
        double a,b;

        a=X[ig+nxvar*n0];
        if (a<=0.0) a=0.1;
        b=exp(y);
        return ( a*exp(-b)*b );
        }

static double sqroot(double y)
        {
        if (y<0.0)
            {
            math_error=1;
            sur_print("\nNegative argument in square root!"); WAIT;
            return(0.0);
            }
        return(sqrt(y));
        }

static double exponent(double y)
        {
        if (y<=0.0)
            {
            math_error=1;
            sur_print("\nOnly positive values allowed!"); WAIT;
            return(0.0);
            }
        return(pow(y,alpha));
        }
static double iexponent(double y)
        { return(pow(y,1.0/alpha)); }
static double diexponent(double y)
        { return(pow(y,1.0/alpha-1.0)/alpha); }

static double digamma(double y)
        { return( -1.0/(y*y) ); }

static double devlog(double y,double my)
        { return( 2*(y*log(y/my)-(y-my)) ); }
static double devlin(double y,double my)
        { return( (y-my)*(y-my) ); }
static double devbin(double y,double my)
        {
        double a,y1,y2;

        a=X[ig+nxvar*n0];
/*      if (y=0.0 || my==0.0 || a<=y || a<=my) return(0.0); -13.2.98 */
        if (a==0.0) return(0.0);
//      return( 2*(y*log(y/my)+(a-y)*log((a-y)/(a-my))) );

        if (y==0.0) y1=0.0; else y1=y*log(y/my);
        if (a<=y) y2=0.0; else y2=(a-y)*log((a-y)/(a-my));
        return( 2*(y1+y2) );
        }

static double devgamma(double y,double my)
        { return( 2*(-log(y/my)+(y-my)/my) ); }

static double scale1(double dev,double df)
        { return(1.0); }
static double scale_normal(double dev,double df)
        { return(dev/df); }

static double vfbin(double my)
        {
        double a;

        a=X[ig+nxvar*n0];
        if (a<=0.0) return(1.0);
        return ( my*(a-my)/a );
        }

static int lue_error()
        {
        int i;
        char s[LLENGTH];

        i=spfind("ERROR");
        if (i>=0) { strcpy(s,spb[i]); strcpy(g_error,s); muste_strupr(s); }
        if (i<0 || *s=='N')
            {
            ng_error=ng_link=1; strcpy(g_error,"Normal"); flink=idn;
            filink=idn; dfilink=one; vf=one;
            devf=devlin; fscale=scale_normal;
            strcpy(g_link,"Identity");
            return(1);
            }
        if (*s=='P')
            {
            ng_error=ng_link=2; flink=log0;
            filink=exp; dfilink=exp; vf=idn;
            devf=devlog; fscale=scale1;
            strcpy(g_link,"Log");
            return(1);
            }
        if (*s=='B')
            {
            if (nbinvar<0)
                {
                sur_print("\n'N' variable is not given for ERROR=Binomial.");
                WAIT; return(-1);
                }
            ng_error=ng_link=3; flink=logit;
            filink=ilogit; dfilink=dilogit; vf=vfbin;
            devf=devbin; fscale=scale1;
            strcpy(g_link,"Logit");
            return(1);
            }
        if (*s=='G')
            {
            ng_error=ng_link=4; flink=inv0;
            filink=inv0; dfilink=digamma; vf=square;
            devf=devgamma; fscale=scale_normal;
            strcpy(g_link,"Reciprocal");
            return(1);
            }
        sprintf(sbuf,"\nUnknown ERROR=%s",g_error); sur_print(sbuf);
        WAIT;
        return(-1);
        }

static int lue_link()
        {
        int i;
        char s[LLENGTH],*part[2];

        i=spfind("LINK");
        if (i<0) return(1);
        strcpy(s,spb[i]); strcpy(g_link,s); muste_strupr(s);
        if (*s=='I')  /* Identity */
            {
            ng_link=1; flink=idn;
            filink=idn; dfilink=one;
            return(1);
            }
        if (strcmp(s,"L")==0 || strcmp(s,"LOG")==0 || strcmp(s,"LN")==0)
            {
            ng_link=2; flink=log0;
            filink=exp; dfilink=exp;
            return(1);
            }
        if (strcmp(s,"G")==0 || strcmp(s,"LOGIT")==0)
            {
            ng_link=3; flink=logit;
            filink=ilogit; dfilink=dilogit;
            return(1);
            }
        if (*s=='R')  /* Reciprocal */
            {
            ng_link=4; flink=inv0;
            filink=inv0; dfilink=digamma;
            return(1);
            }
        if (*s=='P')  /* Probit */
            {
            ng_link=5; flink=probitt;
            filink=iprobit; dfilink=diprobit;
            return(1);
            }
        if (*s=='C')  /* Complementary log-log */
            {
            ng_link=6; flink=complog;
            filink=icomplog; dfilink=dicomplog;
            return(1);
            }
        if (*s=='S')  /* Square root */
            {
            ng_link=7; flink=sqroot;
            filink=square; dfilink=twice;
            return(1);
            }
        if (*s=='E')  /* Exponent */
            {
            i=split(s,part,2);
            if (i<2)
   { sur_print("\nalpha missing in LINK=E,alpha"); WAIT; return(-1); }
            alpha=atof(part[1]);
            if (alpha==0.0)
                { sur_print("\nUse LINK=Log"); WAIT; return(-1); }
            ng_link=8; flink=exponent;
            filink=iexponent; dfilink=diexponent;
            return(1);
            }
        sprintf(sbuf,"\nUnknown LINK=%s",g_link); sur_print(sbuf);
        WAIT;
        return(-1);
        }

/* gen3.c 8.3.1988/SM (23.12.1990)
*/

/****************************
double *XX,*XY;
double *a_fit,*lmy_fit,*z_fit;
int *idel_fit;
char *lab_fit;
unsigned int ig;  // global index for observations 0,1,...,n-1
*****************************************/

static int glm_fit(double (*flink)(),double (*filink)(),double (*dfilink)(),
    double (*vf)(),double (*devf)(),double (*fscale)(),
    double *X,int nx,int mx,double *Y,double *W,double *V,
    char *lab,double *b,double *sb,double *my,double *pdev,int *pdf)

// double (*flink)();   /* link function */
// double (*filink)();  /* inverse link function */
// double (*dfilink)(); /* derivative of inverse link function */
// double (*vf)();      /* variance function */
// double (*devf)();    /* deviance function */
// double (*fscale)();  /* scaling function (s.e.) */
// double *X;           /* design matrix nx*mx */
// int nx,mx;
// double *Y;           /* dependent variable */
// double *W;           /* weight variable */
// double *V;           /* initial weights */
// char *lab;           /* labels of variables (8 bytes each) */
// double *b;           /* regression coefficients */
// double *sb;          /* standard errors of b */
// double *my;          /* fitted values */
// double *pdev;        /* deviance */
// int *pdf;            /* degrees of freedom */
        {
        int i,j;
        double da;
        int iteration;
        double dev0=1e15;
        int ndel=0;
        double aa,bb;
        double db;
/*
printf("\nX:"); mprint(X,nx,mx); getch();
printf("\nY:"); mprint(Y,nx,1); getch();
printf("\nW:"); mprint(W,nx,1); getch();
printf("\nV:"); mprint(V,nx,1); getch();
*/

        *pdf=-mx; for (i=0; i<nx; ++i) *pdf+=(int)W[i];
        if (*pdf<=0) return(-2);

        i=glm_fit_space(mx,nx); if (i<0) return(-1);
        for (i=0; i<8*mx; ++i) lab_fit[i]=lab[i];
/*      for (ig=0; ig<nx; ++ig) V[ig]=sqrt(fabs(V[ig]));  23.12.90  */

        while (1)
            {
            for (i=0; i<mx; ++i)
                {
                XY[i]=0.0;
                for (j=0; j<=i; ++j) XX[i+mx*j]=0.0;
                }

            for (ig=0; ig<nx; ++ig)
                {
                da=V[ig]*W[ig];
                aa=(*flink)(Y[ig]);
                for (i=0; i<mx; ++i)
                    {
                    bb=X[ig+u[i]*n0];
                    XY[i]+=da*bb*aa;
// printf("\npaino=%g x=%g y=%g",da,bb,aa); getch();
                    for (j=0; j<=i; ++j) XX[i+mx*j]+=da*bb*X[ig+u[j]*n0];
                    }
                }


            for (i=0; i<mx; ++i) for(j=0; j<=i; ++j) XX[j+mx*i]=XX[i+mx*j];

// mprint(XX,mx,mx);
// mprint(XY,1,mx); getch();

            i=mat_cholinv_genreg(XX,mx,(double)1e-10);

            if (i>0) break;
            sprintf(sbuf,"\nCol. %.8s linearly dependent on previous ones!",
                                        lab_fit-8*i); sur_print(sbuf);
            reduce(X,lab_fit,nx,mx,-i);
            --mx;
            ++*pdf;
            idel_fit[ndel++]=-i;
            }
        mat_cholmove(XX,mx);
        mat_mlt(b,XX,XY,mx,mx,1);
/*
   printf("\nb0: ");
   for (i=0; i<mx; ++i) printf("%g ",b[i]); getch();
*/
        if (*pdf<=0) return(1);

        iteration=0;
        while (1)
            {
            for (ig=0; ig<nx; ++ig)
                {
                da=0.0;
                for (j=0; j<mx; ++j) da+=X[ig+u[j]*n0]*b[j];
                lmy_fit[ig]=da;
                }
            for (ig=0; ig<nx; ++ig) my[ig]=(*filink)(lmy_fit[ig]);
            for (ig=0; ig<nx; ++ig)
                z_fit[ig]=lmy_fit[ig]+(Y[ig]-my[ig])/(*dfilink)(lmy_fit[ig]); ;

            for (ig=0; ig<nx; ++ig)
                {
                db=(*vf)(my[ig]); /* 4.7.1996 */
                if (db<=1e-15) db=1e-15;
                V[ig]=(*dfilink)(lmy_fit[ig])/sqrt(db);
                }

            for (i=0; i<mx; ++i)
                {
                XY[i]=0.0;
                for (j=0; j<=i; ++j) XX[i+mx*j]=0.0;
                }
            for (ig=0; ig<nx; ++ig)
                {
             /* da=V[ig]*W[ig];  23.12.90 */
                da=V[ig]*V[ig]*W[ig];
                aa=z_fit[ig];
                for (i=0; i<mx; ++i)
                    {
                    bb=X[ig+u[i]*n0];
                    XY[i]+=da*bb*aa;
                    for (j=0; j<=i; ++j) XX[i+mx*j]+=da*bb*X[ig+u[j]*n0];
                    }
                }
            for (i=0; i<mx; ++i) for(j=0; j<=i; ++j) XX[j+mx*i]=XX[i+mx*j];
            i=mat_cholinv_genreg(XX,mx,(double)1e-10);
            if (i<0)
                {
          sprintf(sbuf,"\nCol. %d linearly dependent on previous ones!",-i+1);
                sur_print(sbuf); WAIT; return(-1);
                }
            mat_cholmove(XX,mx);
            mat_mlt(b,XX,XY,mx,mx,1);
/*
   printf("\nb: ");
   for (i=0; i<mx; ++i) printf("%g ",b[i]); getch();
printf("\n");
mprint(W,1,nx);
mprint(Y,1,nx);
mprint(my,1,nx);
printf("\n");
*/
            da=0.0;
            for (ig=0; ig<nx; ++ig)
                {
                da+=W[ig]*(*devf)(Y[ig],my[ig]);
/*
printf("\nig=%d nxvar=%d n0=%d|",ig,nxvar,n0); getch();
printf("\n%g %g %g %g %g|",da,X[ig+nxvar*n0],W[ig],Y[ig],my[ig]);
getch();
*/
                }
            *pdev=da;

            ++iteration;
        sprintf(sbuf,"\niteration %d: Deviance=%g df=%d",iteration,*pdev,*pdf);
                sur_print(sbuf);

            if (fabs(*pdev/dev0-1)<1e-4)
                {
                std_errors(XX,mx,sb,lab_fit,(*fscale)(*pdev,(double)*pdf));
                restore(b,sb,ndel,mx);
                return(1);
                }
            dev0=*pdev;
            if (iteration>3)
                {
                if (sur_kbhit())
                    {
                    i=sur_getch(); while (sur_kbhit()) sur_getch();
                    if (i=='.')
              { std_errors(XX,mx,sb,lab_fit,(*fscale)(*pdev,(double)*pdf));
                restore(b,sb,ndel,mx); return(2); }
                    }
                }
            }

        return(1);
        }

static int glm_fit_space(int mx,int nx)
        {
        int i;

        i=mspace(&XX,mx,mx+1); if (i<0) return(-1);
        i=mspace(&XY,mx,1); if (i<0) return(-1);
        i=mspace(&a_fit,mx,1); if (i<0) return(-1);
        i=mspace(&lmy_fit,nx,1); if (i<0) return(-1);
        i=mspace(&z_fit,nx,1); if (i<0) return(-1);

/*******************
        XX=(double *)malloc(mx*(mx+1)*sizeof(double));
        XY=(double *)malloc(mx*sizeof(double));
        a_fit=(double *)malloc(mx*sizeof(double));
        lmy_fit=(double *)malloc(nx*sizeof(double));
        z_fit=(double *)malloc(nx*sizeof(double));
**********************/
        if (idel_fit!=NULL) idel_fit=(int *)realloc(idel_fit,mx*sizeof(int));
        else          idel_fit=(int *)malloc(mx*sizeof(int));
        if (idel_fit==NULL) { not_enough_memory(); return(-1); }
        if (lab_fit!=NULL) lab_fit=(char *)realloc(lab_fit,8*mx+1);
        else          lab_fit=(char *)malloc(8*mx+1);
        if (lab_fit==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int mspace(double **A,int m,int n)
        {
        if (*A!=NULL) *A=(double *)realloc(*A,m*n*sizeof(double));
        else          *A=(double *)malloc(m*n*sizeof(double));
        if (*A==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int std_errors(double *R,int mx,double *sb,char *lab,double ss)
        {
        int i,j;
        char s[129];
        double da;

        for (i=0; i<mx; ++i)
            {
            for (j=0; j<=i; ++j)
                {
                da=ss*R[i+mx*j]; R[i+mx*j]=da; R[j+mx*i]=da;
                }
            sb[i]=sqrt(R[i*(mx+1)]);
            }
        sprintf(s,"Covariances_of_parameters_in_a_GLM_model");
        matrix_save("PCOV.M",R,mx,mx,lab,lab,8,8,-1,s,0,0);
        return(1);
        }

static int reduce(double *X,char *lab,int nx,int mx,int i1)
        {
        unsigned int i;

/*      for (li=(int)nx*(int)i1; li<(int)nx*(int)(mx-1); ++li)
            X[li]=X[li+(int)nx]; */
        for (i=i1; i<mx-1; ++i) u[i]=u[i+1];
        for (i=8*i1; i<8*(mx-1); ++i) lab[i]=lab[i+8];
        return(1);
        }

static int restore(double *b,double *sb,int ndel,int mx)
        {
        int i,j,i1;

        if (ndel==0) return(1);
        for (j=ndel-1; j>=0; --j)
            {
            i1=idel_fit[j];
            for (i=ndel+mx-1; i>i1; --i) { b[i]=b[i-1]; sb[i]=sb[i-1]; }
            b[i1]=0.0; sb[i1]=-1.0;
            }
        return(1);
        }

/* cholinv2.c 22.7.85/SM (18.12.1990)
*/


/*
  B=INV(A), A positive definite    double A[(n+1)*n]

   n=5
        0   1   2     n-1   n
0     A00 B00 B01 B02 B03 B04
1     A10 A11 B11 B12 B13 B14
2     A20 A21 A22 B22 B23 B24
      A30 A31 A32 A33 B33 B34
n-1   A40 A41 A42 A43 A44 B44

Normally 1 is returned.
If A is not positive definite, -j (j first var dependent on previous ones)
is returned.

*/

static int mat_cholinv_genreg(double *a,int n,double eps)
        {
        int i,j,k,i1,j1;
        double z,x,y=0;

        for (i=0; i<n; ++i)
            {
            i1=i+1;
            for (j=i; j<n; ++j)
                {
                j1=j+1;
                x=a[n*i+j];     /* ajattele: i=sarake, j=rivi */
                                /* alunperin talletus riveittäin */
                                /* nyt sarakkeittain */
                for (k=i-1; k>=0; --k)
                    x-=a[n*j1+k]*a[n*i1+k];
                if (j==i)
                    {
                    if (x<=eps) return(-j);
                    a[n*i1+i]=y=1/sqrt(x);
                    }
                else a[n*j1+i]=x*y;
                }
            }

        for (i=0; i<n; ++i)
        for (j=i+1; j<n; ++j)
            {
            z=0;
            j1=j+1;
            for (k=j-1; k>=i; --k)
                z-=a[n*j1+k]*a[n*(k+1)+i];
            a[n*j1+i]=z*a[n*j1+j];
            }
        for (i=0; i<n; ++i)
        for (j=i; j<n; ++j)
            {
            z=0;
            j1=n;
            for (k=j+1; k<=j1; ++k)
                z+=a[n*k+j]*a[n*k+i];
            a[n*(j+1)+i]=z;
            }
        return(1);
        }
/************
static int mprint(double *X,int m,int n)
        {
        int i,j;

        for (i=0; i<m; ++i)
            {
            Rprintf("\n");
            for (j=0; j<n; ++j) Rprintf("%g ",X[i+m*j]);
            }
        return(1);
        }
********************/

