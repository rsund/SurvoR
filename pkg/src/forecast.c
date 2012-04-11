/* !hw.c 12.12.1986/SM (15.6.1992) (28.11.1994)
   FORECAST: Holt-Winters forecasting
   with back forecasting
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define PERMAX 64
static SURVO_DATA d;

static int per;
static int n,nmax;
static double *x,*xpred,*m,*s_tila,*s,*rr,*res,*corr;
static int var, predvar;
static double par[3],step[3];
static double mse;
static int nf;
static char *parnimi[]={ "a(level)","a(seasonal)","a(slope)" };
static double cc;
static int type;
static int tulosrivi;
static int maxout, noutlier, tout[10];
static double outlimit;
static int miss;
static int autom_period;
static int more_outliers;
static int ahead;
static int par_fixed;  /* 1=optimize  0=fixed parameters given by PAR=a1,a2,a3 */
static double *ms;

static int varaa_tilat2();
static int ei_tilaa();
static int init_hw();
static double hw(double *a);
static int pred_hw(double *a,int ahead);
static int reverse(double *x,int n);
static double ff(double x,double c);
static double gg(double x,double c);
static double pow2(double x,double c);
static int printout();
static int save_pred();
// static int gradhw(double *grad,double *x);
static int nelder(double *x,double *py,int n,double (*f)(),double *step,
       double alpha,double beta,double gamma,char **varname,char *fname,double yeps);
static int sp_outlier();
static int outliers();
static int tasoita_puuttuvat();
static int autoc(double *x,int n,double *r,int maxlag);
static int find_period(double *x,int n,int permax);
static int print_line(char *line);

void muste_forecast(char *argv)
        {
        int i,j;
        int l;
        double hw();
        int gradhw();
        char rivi[LLENGTH], *sana[3];
        double bpar[3];
   //   int stop;
        double y;

        autom_period=more_outliers=0;

        s_init(argv);
        if (g<4)
            {
            sur_print("\nUsage: FORECAST <SURVO_data>,<variable>,<predictor>,L");
            WAIT; return;
            }
        i=spec_init(r1+r-1); if (i<0) return;

    x=NULL;
    xpred=NULL;
    m=NULL;
    s_tila=NULL;
    rr=NULL;
    res=NULL;
    corr=NULL;
    ms=NULL;

        if (g<5) tulosrivi=0;
        else
            {
            tulosrivi=edline2(word[4],1);
            if (tulosrivi==0) return;
            }
        i=data_open2(word[1],&d,1,0,0); if (i<0) return;
                                /* tilaa uusille muuttujille */
        i=mask(&d); if (i<0) return;

        i=spfind("IND");   /* 15.6.1992 */
        if (i>=0)
            {
            strcpy(rivi,spb[i]);
            i=split(rivi,sana,3);
            if (i<3 || muste_strcmpi(sana[0],"ORDER")!=0)
                {
                sur_print("\nIn FORECAST, only IND of the form IND=ORDER,j1,j2 is permitted!");
                WAIT; return;
                }
            d.l1=atol(sana[1]);
            d.l2=atol(sana[2]);
            }

        n=d.l2-d.l1+1;
        var=varfind(&d,word[2]); if (var<0) return;
        if (*word[3]=='-') predvar=-1;
        else
            {
            predvar=varfind2(&d,word[3],0);
            if (predvar<0)
                {
                predvar=create_newvar(&d,word[3],'4',4);
                if (predvar<0) return;
                }
            }
        if (predvar==var)
            {
            sur_print("\nDo not select same variable for the predicted values!");
            WAIT; return;
            }
        i=sp_init(r1+r-1); if (i<0) return;

        i=spfind("PERIOD");
        if (i<0) { per=-1; nmax=1+n+PERMAX; }
        else { per=atoi(spb[i]); nmax=1+n+per; }

        if (per<0 && n<6) { per=1; nmax=1+n+per; }

        if (n<3 || n<3*per)
            {
            sur_print("\nNot enough observations!");
            WAIT; return;
            }

        ahead=per;
        i=spfind("AHEAD");
        if (i>=0)
            {
            ahead=atoi(spb[i]);
            if (per>0 && ahead>per) nmax=1+n+ahead;
            if (per<0 && ahead>PERMAX) nmax=1+n+ahead;
            }
        i=varaa_tilat2(); if (i<0) return;

        j=1;
        sp_outlier();
        miss=0;
        for (l=d.l1; l<=d.l2; ++l)
            {
            data_load(&d,l,var,&y);
            if (y==MISSING8)
                {
                ++miss;
                if (miss>maxout)
                    {
                    sur_print("\nToo many missing observations!");
                    WAIT; return;
                    }
                }
            x[j++]=y;
            }

        tasoita_puuttuvat();

        i=spfind("PAR");
        if (i<0)
            { par[0]=0.5; par[1]=0.5; par[2]=0.5; par_fixed=0;}
        else
            {
            strcpy(rivi,spb[i]);
            split(rivi,sana,3);
            par[0]=atof(sana[0]); par[1]=atof(sana[1]); par[2]=atof(sana[2]);
            par_fixed=1;
            }

        cc=1.0; i=spfind("C");
        if (i>=0) { cc=atof(spb[i]); if (cc<0.0) cc=1.0; }
        if (cc==1.0) type=1; else if (cc==0.0) type=0; else type=2;

        for (i=0; i<3; ++i) bpar[i]=par[i];

        if (per<0)
            {
            per=find_period(x+1,n,PERMAX);
            if (per<0)
                {
                sur_print("\nGive period as a specification in the form");
                sur_print("\nPERIOD=<#_of_obs._in_one_period>");
                WAIT; return;
                }
            if (ahead<0) ahead=per;
            autom_period=per;
            }

        noutlier=0;
        while (1)
            {
            reverse(x+1,n);
            init_hw();

            if (par_fixed)
                {
                mse=hw(par);
                }
            else
                {
                step[0]=step[1]=step[2]=0.1;
                nf=nelder(bpar,&mse,3,hw,step,1.0,0.5,2.0,parnimi,"MSE",0.005);
                }
            pred_hw(bpar,per);
            j=-per+1; for (i=n+per; i>n; --i) s[j++]=s[i];
            m[0]=m[n+1]; rr[0]=-rr[n+1];

            reverse(x+1,n);
            if (noutlier==0) for (i=0; i<3; ++i) par[i]=bpar[i];

            if (par_fixed)
                {
                mse=hw(par);
                }
            else
                {
                step[0]=step[1]=step[2]=0.1;
                nf=nelder(par,&mse,3,hw,step,1.0,0.5,2.0,parnimi,"MSE",0.001);
                }
            if (!outliers()) break;
            }

        pred_hw(par,ahead);
        printout();
        i=save_pred();
        if (i<0)
            {
            if (d.l2+ahead>d.n)
                {
                sur_print("\nCannot save all predicted values etc.");
                if (d.type==2)
                    {
                    sprintf(sbuf,"\nUse FILE INIT %s,%d  for example!",
                                word[1],d.l2+ahead-d.n);
                    sur_print(sbuf);
                    }
                WAIT;
                }
            }
        data_close(&d);
        s_end(argv);
        }

static int varaa_tilat2()
        {
        x=(double *)muste_malloc(nmax*sizeof(double));
        if (x==NULL) { ei_tilaa(); return(-1); }
        xpred=(double *)muste_malloc(nmax*sizeof(double));
        if (xpred==NULL) { ei_tilaa(); return(-1); }
        m=(double *)muste_malloc(nmax*sizeof(double));
        if (m==NULL) { ei_tilaa(); return(-1); }
        s_tila=(double *)muste_malloc((nmax+PERMAX)*sizeof(double));
        if (s_tila==NULL) { ei_tilaa(); return(-1); }
        s=s_tila+PERMAX;
        rr=(double *)muste_malloc(nmax*sizeof(double));
        if (rr==NULL) { ei_tilaa(); return(-1); }
        res=(double *)muste_malloc(nmax*sizeof(double));
        if (res==NULL) { ei_tilaa(); return(-1); }
        corr=(double *)muste_malloc(nmax*sizeof(double));
        if (corr==NULL) { ei_tilaa(); return(-1); }
        return(1);
        }

static int ei_tilaa()
        {
        sur_print("\nNot enough space for series!");
        WAIT; return(1);
        }

static int init_hw()
        {
        int t;
        double a; // ,m1,m2,m3;
        double c;
        extern double ff(),gg();
        double mean[10];
        int k,h;

        k=3;   /* periodeja alkuarvojen etsimiseen */
        c=cc;
/*      for (t=1; t<=n; ++t) printf(" %g",x[t]);        */
        for (h=0; h<k; ++h)
            {
            a=0.0; for (t=h*per+1; t<=(h+1)*per; ++t) a+=x[t];
            mean[h]=a/per;
            }
        rr[0]=(mean[k-1]-mean[0])/((k-1)*per);
        m[0]=mean[0]-per*rr[0]/2;

        if (type==1)
            {
            for (t=1; t<=per; ++t)
                {
                a=0.0;
                for (h=0; h<k; ++h) a+=x[h*per+t]-mean[h];
                s[t-per]=a/k;
                }
            }
        else if (type==0)
            {
            for (t=1; t<=per; ++t)
                {
                a=0.0;
                for (h=0; h<k; ++h) a+=x[h*per+t]/(mean[h]-rr[0]*((per+1.0)/2.0-t));
                s[t-per]=a/k;
                }
            a=0.0; for (t=1; t<=per; ++t) a+=s[t-per]; a/=per;
            for (t=1; t<=per; ++t) s[t-per]/=a;
            }
        else
            {
            for (t=1; t<=per; ++t)
                {
                a=0.0;
                for (h=0; h<k; ++h) a+=ff(x[h*per+t],c)-ff(mean[h],c);
                s[t-per]=gg(a/k,c);
                }
            }
/*        s[t]=s[t+per]=(gg(ff(x[t],c)-ff(m[0],c),c)+gg(ff(x[t+per],c)-ff(a,c),c))/2;
                vanha
*/
/*      printf("\nS:");
        for (t=1; t<=per; ++t) printf("\n %d %g",t-per,s[t-per]);
        printf("\nm0=%g rr0=%g",m[0],rr[0]); getch();
*/      return(1);
        }

static double hw(double *a)
        {
        int t;
        double mse,b;
        double c;
        extern double ff(),gg();
        int nmse;
/*
        if (a[0]<0.0 || a[1]<0.1 || a[2]<0 || a[0]>1.0 || a[1]>1.0 || a[2]>1.0)
            return (1e10);
*/
        c=cc;
        mse=0.0; nmse=0;
        for (t=1; t<=n; ++t)
            {
            if (type==1)
                {
                xpred[t]=(m[t-1]+rr[t-1])+s[t-per];
                m[t]=a[0]*(x[t]-s[t-per])+(1.0-a[0])*(m[t-1]+rr[t-1]);
                s[t]=a[1]*(x[t]-m[t])+(1.0-a[1])*s[t-per];
                rr[t]=a[2]*(m[t]-m[t-1])+(1.0-a[2])*rr[t-1];
                }
            else if (type==0)
                {
                xpred[t]=(m[t-1]+rr[t-1])*s[t-per];
                m[t]=a[0]*(x[t]/s[t-per])+(1.0-a[0])*(m[t-1]+rr[t-1]);
                s[t]=a[1]*(x[t]/m[t])+(1.0-a[1])*s[t-per];
                rr[t]=a[2]*(m[t]-m[t-1])+(1.0-a[2])*rr[t-1];
                }
            else
                {
                xpred[t]=gg(ff(m[t-1]+rr[t-1],c)+ff(s[t-per],c),c);
                m[t]=a[0]*gg(ff(x[t],c)-ff(s[t-per],c),c)+(1.0-a[0])*(m[t-1]+rr[t-1]);
                s[t]=a[1]*gg(ff(x[t],c)-ff(m[t],c),c)+(1.0-a[1])*s[t-per];
                rr[t]=a[2]*(m[t]-m[t-1])+(1.0-a[2])*rr[t-1];
                }

            ++nmse;
            b=xpred[t]-x[t]; mse+=b*b;
            }

        if (a[0]<0.0) mse*=(1-10*a[0]);
        if (a[0]>1.0) mse*=(1+10*(a[0]-1));
        if (a[1]<0.0) mse*=(1-10*a[1]);
        if (a[1]>1.0) mse*=(1+10*(a[1]-1));
        if (a[2]<0.0) mse*=(1-10*a[2]);
        if (a[2]>1.0) mse*=(1+10*(a[2]-1));
        return(mse/nmse);
        }

static int pred_hw(double *a,int ahead)
        {
        int t;
   //   double mse,b;
        double c;
        extern double ff(),gg();

        c=cc;
        for (t=1; t<=n+ahead; ++t)
            {
            if (type==1)
                {
                xpred[t]=(m[t-1]+rr[t-1])+s[t-per];
                if (t>n) x[t]=xpred[t];
                m[t]=a[0]*(x[t]-s[t-per])+(1.0-a[0])*(m[t-1]+rr[t-1]);
                s[t]=a[1]*(x[t]-m[t])+(1.0-a[1])*s[t-per];
                rr[t]=a[2]*(m[t]-m[t-1])+(1.0-a[2])*rr[t-1];
                }
            if (type==0)
                {
                xpred[t]=(m[t-1]+rr[t-1])*s[t-per];
                if (t>n) x[t]=xpred[t];
                m[t]=a[0]*(x[t]/s[t-per])+(1.0-a[0])*(m[t-1]+rr[t-1]);
                s[t]=a[1]*(x[t]/m[t])+(1.0-a[1])*s[t-per];
                rr[t]=a[2]*(m[t]-m[t-1])+(1.0-a[2])*rr[t-1];
                }
            else
                {
                xpred[t]=gg(ff(m[t-1]+rr[t-1],c)+ff(s[t-per],c),c);
                if (t>n) x[t]=xpred[t];
                m[t]=a[0]*gg(ff(x[t],c)-ff(s[t-per],c),c)+(1.0-a[0])*(m[t-1]+rr[t-1]);
                s[t]=a[1]*gg(ff(x[t],c)-ff(m[t],c),c)+(1.0-a[1])*s[t-per];
                rr[t]=a[2]*(m[t]-m[t-1])+(1.0-a[2])*rr[t-1];
                }
            }
        return(1);
        }

static int reverse(double *x,int n)
        {
        double a;
        int i,k;

        k=n>>1;
        for (i=0; i<k; ++i) { a=x[i]; x[i]=x[n-i-1]; x[n-i-1]=a; }
        return(1);
        }

static double ff(double x,double c)
        {
        double pow2();

/*  printf("\nff: x=%g c=%g y=%g",x,c,(pow2(x,c)-1.0)/c+c); getch();
*/      return((pow2(x,c)-1.0)/c+c);
        }

static double gg(double x,double c)
        {
        double pow2();

/*  printf("\ngg: x=%g c=%g y=%g",x,c,pow2(c*(x-c)+1.0,1.0/c)); getch();
*/      return(pow2(c*(x-c)+1.0,1.0/c));
        }

static double pow2(double x,double c)
        {
        if (x==0.0) return(0.0);
        if (x>0.0) return(pow(x,c));
        return(-pow(-x,c));
        }

/* hw2.c 14.12.1986/SM (25.12.1986)
   Holt-Winters forecasting
*/

static char *laji[]={ "Multiplicative", "Additive", " " };

static int printout()
        {
        int i,k,t;
        char y[LLENGTH];
        char sana[LLENGTH];

        i=output_open(eout);
        if (i<0) return(-1);

        if (type==2)
            {
            i=spfind("C");
            if (i>=0) sprintf(sana,"(C=%s)",spb[i]);
            else *sana=EOS;
            laji[2]=sana;
            }

        sprintf(y,"Holt-Winters' %s Seasonal Forecast: Data %s, Variable %s",
                                laji[type],word[1],word[2]);
        print_line(y);
        *sana=EOS; if (autom_period) strcpy(sana,"(judged from data)  ");
        sprintf(y,"Period=%d obs. %sEstimation on observations %d-%d",
                        per,sana,d.l1,d.l2);
        print_line(y);

        if (noutlier)
            {
            k=sprintf(y,"Outliers:");
            for (i=0; i<noutlier; ++i)
                {
                if (d.vartype[0][0]=='S')
                    {
                    data_alpha_load(&d,d.l1+(int)(tout[i]-1),0,sana);
                    k+=sprintf(y+k,"%d(%.8s),",tout[i],sana);
                    }
                else
                    k+=sprintf(y+k,"%d,",tout[i]);

                }
            y[k-1]=EOS;  /* pilkku pois lopusta */
            if (more_outliers) strcat(y," (+more to be found)");
            print_line(y);
            }

        sprintf(y,"MSE=%g %s=%.3f %s=%.3f %s=%.3f",mse,
                        parnimi[0],par[0],
                        parnimi[1],par[1],
                        parnimi[2],par[2]);
        print_line(y);

        k=sprintf(y,"Autocorrelations of residuals:");
        for (t=1; t<=n; ++t) res[t]=x[t]-xpred[t];
        autoc(res+1,n,corr,per);
        for (i=0; i<per; ++i)
            {
            k+=sprintf(y+k," r%d=%+5.2f",i+1,corr[i]);
            if (k>c3-10) { print_line(y); k=0; }
            }
        if (k>0) print_line(y);


        print_line("Obs.#   Forecast");
        for (i=n+1; i<=n+ahead; ++i)
            {
            fnconv(xpred[i],accuracy,sana);
            sprintf(y,"%4d    %s",d.l1+(int)(i-1),sana);
            print_line(y);
            }

        if (predvar>=0)
            {
            sprintf(y,"Predicted values saved as variable %s (obs. %d-%d)",
                            word[3],d.l1,d.l2+(int)per);
            print_line(y);
            }
        return(1);
        }
/*
print_line(line)
char *line;
        {
        output_line(line,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        }
*/
static int save_pred()
        {
        int l;
        int i,j;
        int trend,beta,seas;


        trend=activated(&d,'T');
        beta=activated(&d,'B');
        seas=activated(&d,'S');

        sur_print("\nSaving predicted values...");
        j=0;
        for (l=d.l1; l<=d.l2+(int)ahead; ++l)
            {
            sprintf(sbuf," %d",l); sur_print(sbuf);
            ++j;
            if (predvar>=0)
                {
                i=data_save(&d,l,predvar,xpred[j]);
                if (i<0) return(-1);
                }
            if (trend>=0)
                {
                i=data_save(&d,l,trend,m[j]);
                if (i<0) return(-1);
                }
            if (beta>=0)
                {
                i=data_save(&d,l,beta,rr[j]);
                if (i<0) return(-1);
                }
            if (seas>=0)
                {
                i=data_save(&d,l,seas,s[j]);
                if (i<0) return(-1);
                }
            }
        return(1);
        }

/* hw3.c 20.12.1986/SM (20.12.1986)
*/
/*****************************************************
static double gradstep=0.0000001;

#define NVAR 3

static int gradhw(double *grad,double *x)
        {
        int i;
        double x1[NVAR];
        double y;

        y=hw(x);
        for (i=0; i<NVAR; ++i)  x1[i]=x[i];
        for (i=0; i<NVAR; ++i)
            {
            x1[i]=x[i]+gradstep;
            grad[i]=(hw(x1)-y)/gradstep;
            x1[i]=x[i];
            }
        return(1);
        }
************************************************************/


/*  hwest.c 15.8.1986/SM (21.12.1986)
    HW       estimate   (kuten hisf3.c)
*/

#define N 3

/*
        nf=nelder(dpar,&maxl,npar,logl,step,1.0,0.5,2.0,parnimi,"-logL");
*/

static int nelder(double *x,double *py,int n,double (*f)(),double *step,
       double alpha,double beta,double gamma,char **varname,char *fname,double yeps)
// double *x;
// double *py;
// int n;
// double (*f)();
// double *step;
// double alpha,beta,gamma;
// char **varname;
// char *fname;
// double yeps;  /* esim. 0.001 */
        {
        int i,j;
        int nf,nfi;
        int jh,js,jl;
        double xx[N+1][N];
        double y[N+1];
        double xc[N],x0[N],x00[N];
        double y0,y00;
        int disp=0;
        double ylast=1e100;
        int stopstep=20;
        int nstop=0;

        nf=nfi=0;
        for (i=0; i<n; ++i) xx[0][i]=x[i];
        for (j=0; j<n; ++j)
            for (i=0; i<n; ++i)
                {
                if (i==j) xx[j+1][i]=x[i]+step[j];
                else      xx[j+1][i]=x[i];
                }
        for (i=0; i<n+1; ++i) y[i]=(*f)(xx[i]); nf+=n+1;

        while (1)
            {
            jh=0; js=1; jl=1; if (y[0]<y[1]) { jh=1; js=0; jl=0; }
            for (j=2; j<n+1; ++j)
                {
                if (y[j]>y[jh]) { js=jh; jh=j; }
                else if (y[j]>y[js]) js=j;
                else if (y[j]<y[jl]) jl=j;
                }

            for (i=0; i<n; ++i)
                {
                xc[i]=0.0;
                for (j=0; j<n+1; ++j)
                    if (j!=jh) xc[i]+=xx[j][i];
                xc[i]/=n;
                }

            if (sur_kbhit())
                {
                i=sur_getch(); if (i=='.') break;
                disp=1-disp;
                }

            if (disp)
                {
                sprintf(sbuf,"\n%s=%g",fname,y[jl]); sur_print(sbuf);
                for (i=0; i<n; ++i) { sprintf(sbuf," %s=%g",varname[i],xx[jl][i]); sur_print(sbuf); }
                sprintf(sbuf," nf=%d",nf); sur_print(sbuf);
                }
            if (nf>=nfi+200)
                {
                nfi+=200;
                sprintf(sbuf,"\n nf=%d  To interrupt, press '.'",nf); sur_print(sbuf);
                }
            ++nstop;
            if (nstop==stopstep)
                {
                nstop=0;
                if ((ylast-y[jl])/(fabs(ylast)+1e-30)<yeps) break;
                ylast=y[jl];
                }
            for (i=0; i<n; ++i) x0[i]=(1+alpha)*xc[i]-alpha*xx[jh][i];
            y0=(*f)(x0); ++nf;

            if (y[jl]<=y0 && y0<=y[js])
                {
                for (i=0; i<n; ++i) xx[jh][i]=x0[i];
                y[jh]=y0;
                continue;
                }
            if (y0<y[jl])
                {
                for (i=0; i<n; ++i) x00[i]=gamma*x0[i]+(1-gamma)*xc[i];
                y00=(*f)(x00); ++nf;
                if (y00<y[jl])
                    {
                    for (i=0; i<n; ++i) xx[jh][i]=x00[i];
                    y[jh]=y00;
                    continue;
                    }
                else
                    {
                    for (i=0; i<n; ++i) xx[jh][i]=x0[i];
                    y[jh]=y0;
                    continue;
                    }
                }

            /* y0>ys */
            if (y0<y[jh])
                {
                for (i=0; i<n; ++i) x00[i]=beta*x0[i]+(1-beta)*xc[i];
                }
            else
                {
                for (i=0; i<n; ++i) x00[i]=beta*xx[jh][i]+(1-beta)*xc[i];
                }
            y00=(*f)(x00); ++nf;
            if (y00<y[jh] && y00<y0)
                {
                for (i=0; i<n; ++i) xx[jh][i]=x00[i];
                y[jh]=y00;
                continue;
                }

            for (j=0; j<n+1; ++j)
                {
                if (j==jl) continue;
                for (i=0; i<n; ++i) xx[j][i]=(xx[j][i]+xx[jl][i])/2;
                y[j]=(*f)(xx[j]); ++nf;
                }
            }
        for (i=0; i<n; ++i) x[i]=xx[jl][i];
        *py=y[jl];
        return(nf);
        }

/* hwo.c 20.12.1986/SM (10.1.1987)
   Holt-Winters forecasting
*/

static int sp_outlier()
        {
        int i;
        char x[LLENGTH], *sana[2];

        maxout=3; outlimit=2.5;
        i=spnfind("OUTLIER");        /* OUTLIER tai OUTLIERS */
        if (i<0) return(1);
        strcpy(x,spb[i]);
        i=split(x,sana,2);
        maxout=atoi(sana[0]);
        if (maxout>10) maxout=10;
        if (i<2) return(1);
        outlimit=atof(sana[1]); return(1);
        }

static int outliers()
        {
        int t;
        double m,s,mx,sx;
        double e,max;
        int tmax=0;

        m=s=mx=sx=0.0;
        max=0.0;
        for (t=1; t<=n; ++t)
            {
            e=x[t]-xpred[t];
            m+=e; s+=e*e;
            mx+=x[t]; sx+=x[t]*x[t];
            if (fabs(e)>max) { max=fabs(e); tmax=t; }
            }

        m/=n; s=sqrt((s-n*m*m)/(n-1));
        if (s<1e-30) return(0);
        mx/=n; sx=sqrt((sx-n*mx*mx)/(n-1)); if (sx<1e-30) return(0);
        if (s/sx<1e-5) return(0);
        if (max<outlimit*s) return(0);

        if (noutlier>maxout-1) { more_outliers=1; return(0); }
        tout[noutlier++]=tmax;
        sprintf(sbuf,"\noutlier #%d value=%g deviation=%g treshold=%g",tmax,x[tmax],max/s,outlimit);
        sur_print(sbuf);
        x[tmax]=xpred[tmax];
        return(1);
        }

static int tasoita_puuttuvat()
        {
        int t;
        double a,max;

        if (miss==0) return(1);
        max=0;
        for (t=1; t<=n; ++t)
            {
            if (x[t]==MISSING8) continue;
            a=fabs(x[t]);
            if (a>max) max=a;
            }

        for (t=1; t<=n; ++t)
            {
            if (x[t]==MISSING8) x[t]=2*max;
            }
        return(1);
        }
/* autoc.c 23.12.1986/SM (23.12.1986)
*/

static int autoc(double *x,int n,double *r,int maxlag)
// double *x;   /* x[0],x[1],...,x[n-1] series */
// int n;
// double *r;   /* r[0],...,r[maxlag-1]   r[i-1]=i. autocorrelation */
// int maxlag;
        {
        int t,lag;
        double m,s;
        double sum;

        m=s=0.0;
        for (t=0; t<n; ++t) { m+=x[t]; s+=x[t]*x[t]; }
        m/=n; s=s/n-m*m;
        if (s<=0.0) { for (t=0; t<maxlag; ++t) r[t]=0.0; return(1); }

        for (lag=1; lag<=maxlag; ++lag)
            {
            sum=0.0;
            for (t=0; t<n-lag; ++t) sum+=(x[t]-m)*(x[t+lag]-m);
            r[lag-1]=sum/(n-lag)/s;
            }
        return(1);
        }
/* period.c 23.12.1986/SM (10.4.1987)
*/
static int find_period(double *x,int n,int permax)
// double *x;  /* series x[0],x[1],...,x[n-1]  */
// int n;
// int permax;
        {
        int i;
        int maxlag;
        double koepar[3];
        double min,maxsuhde,a1,a2;
        int imin=0;

/*      if (n<6) return(1);
*/      maxlag=n/3;
        if (maxlag>permax) maxlag=permax;
        ms=(double *)muste_malloc(maxlag*sizeof(double));
        if (ms==NULL)
            {
            sur_print("\nNot enough memory! (find_period)");
            WAIT; return(-1);
            }

        koepar[0]=koepar[1]=koepar[2]=0.1;
        min=1e300;
        for (i=0; i<maxlag; ++i)
            {
            per=i+1;
            init_hw();
            ms[i]=hw(koepar);
            if (ms[i]<min) { min=ms[i]; imin=i; }
            }
        if (min==0.0) return(imin+1);
  if (sur_kbhit())
      {
  sur_getch();
  sur_print("\n"); for (i=0; i<maxlag; ++i) { sprintf(sbuf,"MSE%d=%.2g ",i+1,ms[i]/min); sur_print(sbuf); }
  WAIT;
      }

        for (i=0; i<maxlag; ++i)
            {
            ms[i]*=1+0.03*i;
            }

        maxsuhde=0.0;
        for (i=1; i<maxlag-1; ++i)
            {
            a1=ms[i-1]/ms[i]; a2=ms[i+1]/ms[i];
            if (a2<a1) a1=a2;
            if (a1>maxsuhde) { maxsuhde=a1; imin=i; }
            }

        if (maxsuhde<1.5) imin=0;
        else
            {
            for (i=1; i<imin; ++i)
                {
               if (ms[i]<1.1*ms[imin] && ms[i-1]/ms[i]>1.3 && ms[i+1]/ms[i]>1.3)
                    { imin=i; break; }
                }
            }

        per=imin+1;
        sprintf(sbuf,"\nperiod=%d (judged from data)",per); sur_print(sbuf);
        muste_free(ms);
        return(per);
        }

static int print_line(char *line)
        {
        output_line(line,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

