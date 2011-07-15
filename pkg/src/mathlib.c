#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
/* RS REM #include <conio.h>*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* RS REM #include <process.h> */
// RS REM #include <errno.h>
#include <time.h>
#include <ctype.h>
#include "survo.h"
#include "survolib.h"
#include "survoext.h"

// RS REM #define RND (double)rand()/RAND_MAX  /* RAND_MAX=32768.0 */

double uniform(double x)
{
    time_t ltime;
    time_t *pi;
    static int next=0;  // RS init?

    if (x==0.0 && next==0)
    {
        time(&ltime);
        pi=&ltime;
        srand((unsigned int)(*pi+*psur_seed));
        rand();
        *psur_seed=rand();

        *psur_seed+=17;
        next=1;
    }
    else
    {
        if (next) return((double)(RND+1e-6));
        if (x!=0.0)
        {
            srand((unsigned int)(x));
            rand();
        }
        next=1;
    }
    return((double)(RND+1e-6));
}

double muste_sqrt(double number)
{
    return(sqrt(number));
}

double muste_log(double number)
{
    return(log(number));
}

double muste_log10(double number)
{
    return(log10(number));
}

double muste_exp(double number)
{
    return(exp(number));
}

double muste_sin(double number)
{
    return(sin(number));
}

double muste_cos(double number)
{
    return(cos(number));
}

double muste_tan(double number)
{
    return(tan(number));
}

double muste_atan(double number)
{
    return(atan(number));
}

double muste_asin(double number)
{
    return(asin(number));
}

double muste_acos(double number)
{
    return(acos(number));
}

double muste_fabs(double number)
{
    return(fabs(number));
}

double muste_floor(double number)
{
    return(floor(number));
}

double muste_sign(double number)
{
    if (number>0.0) return(1.0);
    if (number<0.0) return(-1.0);
    return (0.0);
}

double muste_ind(double number)
{
    if (number>0.0) return(1.0);
    else return(0.0);
}

double probit(double z)
{
    double z1,z2,f;

    z1=z;
    if (z>0.5) z1=1-z;
    z2=sqrt(log(1.0/(z1*z1)));
    f=1.0+z2*(1.432788+z2*(0.189269+z2*0.001308));
    f=z2-(2.515517+z2*(0.802853+z2*0.010328))/f;
    if (z<=0.5) f=-f;
    return(f);
}


double sur_round(double x) /* 8.9.1998 */
{
    long l;
    double a;

    l=x;
    a=l;
    if (x>0.0 && x-a>0.5) ++a;
    else if (x<0.0 && a-x>0.5) --a;
    return(a);
}

double fact(double x)
{
    double a;
    int i,n;

    n=(int)x;
    a=1.0;
    for (i=2; i<=n; ++i) a*=i;
    return(a);
}


/* 21.10.1998 */
double lfact(double x) 
{
    double a,di;

    a=0.0;
    for (di=2.0; di<=x; ++di) a+=log(di);
    return(a);
}

int nfact(unsigned long *pluku,unsigned long factor)
{
    int n=0;

    while (*pluku%factor==0)
    {
        ++n;
        *pluku/=factor;
    }
    return(n);
}

double nfactors(double d)
{
    unsigned long luku,factor,maxfactor;
    int i,k;
    /*
    	int is;
            int jatko=0;
    */
    double freq;

    if (d>4294967295.0) return(0.0); // Max. permitted integer 4294967295=2^32-1"
    if (d<0.0) return(1.0);
    if (d<2.0) return(1.0);

    luku=(unsigned long)d;

    freq=1.0;
    k=0;
    i=nfact(&luku,2L);
    if (i)
    {
        /* printf("\ni2=%i",i); getck(); */
        freq*=(double)(i+1);
    }
    factor=3L;
    maxfactor=(unsigned long)sqrt((double)(luku));
    while (maxfactor>=factor)
    {
        i=nfact(&luku,factor);
        if (i)
        {
            /* printf("\ni=%i factor=%ld",i,factor); getck(); */
            freq*=(double)(i+1);
            maxfactor=(unsigned long)sqrt((double)(luku));
        }
        factor+=2L;
    }
    if (luku>1L) freq*=2L;
    return(freq);
}

double totient(double nn)  // 19.4.2009
     {
     int i,n,result;

     n=(int)nn;
     result = n;

       for(i=2;i*i <= n;i++)
       {
         if (n % i == 0) result -= result / i;
         while (n % i == 0) n /= i;
       }
       if (n > 1) result -= result / n;
       return ((double)result);
     }
     
unsigned long igcd(unsigned long u,unsigned long v)
    {
    unsigned long w;

    if (u==0L) return(v);
    if (v==0L) return(u);
    if (u<v) { w=u; u=v; v=w; }
    while ((w=u%v)!=0)
        {
        u=v; v=w;
        }
    return(v);
    }     

double mtotient(double mm, double nn)  // 26.4.2009
     {
     unsigned int i,m,n,result;

     m=(unsigned int)mm;
     n=(unsigned int)nn;
     result = 0;

     for (i=1; i<=m; ++i) if (igcd(i,n)==1) ++result;
/*****************************
printf("\nm=%u n=%u",m,n); getck();
       for(i=2;i <= m;i++)
       {
         if (n % i == 0) result -= result / i;
         while (n % i == 0) n /= i;
       }
       if (n > 1) result -= result / n;
*****************************/
       return ((double)result);
     }

double nondiv(double mm, double nn)  // 26.4.2009
     {
     unsigned int i,m,n,result;

     m=(unsigned int)mm;
     n=(unsigned int)nn;
     result=0;
     for (i=2; i<=m; ++i) if (n%i>0) ++result;
     return ((double)result);
     }


#define N_ZETA 19
double zeta(double x)
{
    int n,i;
    double s,t,u;
    double zc[N_ZETA+1];

    n=N_ZETA;
    s=t=u=1.0;
    for (i=0; i<=n; ++i)
    {
        zc[i]=s;
        /* printf("\ni=%d zc=%g|",i,s); getck(); */
        t*=(n-i)*(n+i)*4;
        u*=(i+i+1)*(i+i+2);
        s+=t/u;
    }
    s=0.0;
    for (i=n-1; i>=0; --i)
        s=(zc[n]-zc[i])/pow((double)(i+1),x)-s;
    return(s/zc[n]/(1.0-2.0/pow(2.0,x)));
}


/* RS REM 14.9.94
double sur_rand0(double x)
{
    return(uniform(x)); 
}
*/

double gcd(double a,double b) /*  Greatest Common Divisor */
{
    unsigned long u,v,w;

    u=fabs(a);
    v=fabs(b);
    if (u<v)
    {
        w=u;
        u=v;
        v=w;
    }
    while ((w=u%v)!=0)
    {
        u=v;
        v=w;
    }
    return((double)v);
}

double root(double dn,double x)
{
    int n;

    n=dn;
    if (x>0.0 || (double)n!=dn || n<0 || ((n>>1)<<1)==n) return(R_pow(x,1/dn));
    if (x==0.0) return(0.0);
    return(-R_pow(-x,1/dn));
}

int sur_julian(double d,double m,double y,double *pdate)
{
    double extra;

    extra=100.0*y+m-190002.5;
    *pdate=367.0*y-(int)(7.0*(y+(int)((m+9.0)/12.0))/4.0);
    *pdate+=(int)(275.0*m/9.0)+d+1721013.5;
    *pdate-=0.5*extra/fabs(extra);
    return(1);
}

double muste_fin_pv(double x0,double x1,double x2)
        {
        double a;

        a=x1/100.0;
        return(x0/a*(1.0-pow(1.0+a,-x2)));
        }

double muste_fin_fv(double x0,double x1,double x2)
        {
        double a;

        a=x1/100.0;
        return(x0/a*(pow(1.0+a,x2)-1.0));
        }

double muste_fin_pmt(double x0,double x1,double x2)
        {
        double a;

        a=x1/100.0;
        return(x0*a/(1.0-pow(1.0+a,-x2)));
        }

double muste_boxcox(double x0,double x1)
        {
        if (x1==0.0) return(log(x0));
        else return((pow(x0,x1)-1.0)/x1);
        }

double muste_inv_boxcox(double x0,double x1)        
        {
        if (x1==0.0) return(exp(x0));
        else return(pow(1.0+x1*x0,1/x1));
        }

double muste_diss(double x0,double x1,int ratio)
        {
        int m,k,mm=0,nn=0;
        double f,a,b,diss;

        f=1e10; m=k=0; a=pow(10.0,x0);  /* k vastaa parametria n */
        while (k<f)
            {
            ++m; k=m*x1; diss=m*x1-k;
            if (diss>=0.5) { diss=1.0-diss; ++k; }
            b=diss/k; b=k+a*b*b;
            if (b<f) { f=b; mm=m; nn=k; }
            }
        if (ratio) return(mm+nn/1000.0);
        else return(log(f));
        }
        
double muste_bestval(double x, double y)
        {
        double z;
        int merkki;
        char a[22],b[22];
        int i,j,k,h;
        char u,v;

        merkki=1;
        if (x>y) { z=x; x=y; y=z; }
        if (x<=0.0 && y>=0.0) return(0.0);
        if (x==y) return(x);
        if (x<0) { merkki=-1; z=x; x=-y; y=-z; }

        sprintf(a,"%21.10f",x); a[21]='\0';
        sprintf(b,"%21.10f",y); b[21]='\0';
        i=0; while (a[i]==' ') a[i++]='0';
        i=0; while (b[i]==' ') b[i++]='0';

        i=0; k=0; while (a[i]==b[i] && i<22) { ++i; if (a[i]!='0') ++k; }
        h=0;
        for (j=i+1; j<21; ++j) { if (b[j]!='.') b[j]='0';
                                 if (a[j]!='.' && a[j]!='0') ++h;
                               }
        u=a[i]; v=b[i];
        if (h>0) ++u;
        if (u==v) ;
        else if (u=='0' && k==0) b[i]='1';
        else if (u=='0') b[i]='0';
        else if (u<'6' && v>'4') b[i]='5';
        else if (u<'3' && v<='3') b[i]='2';
        else if (u<'5' && v<'5') b[i]='4';
        else if (u=='6' && v=='7') b[i]='6';
        else b[i]='8';

        return(merkki*atof(b));
        }

double muste_lgamma(double number)
{
    return(lgammafn(number)); // Rmath
}


double muste_gamma(double number)
{
    return(gammafn(number)); // Rmath
}

double muste_digamma(double number)
{
    return(digamma(number)); // Rmath
}

double muste_trigamma(double number)
{
    return(trigamma(number)); // Rmath
}

double muste_tetragamma(double number)
{
    return(tetragamma(number)); // Rmath
}

double muste_pentagamma(double number)
{
    return(pentagamma(number)); // Rmath
}

double muste_beta(double a,double b)
{
    return(beta(a,b)); // Rmath
}

double muste_lbeta(double a,double b)
{
    return(lbeta(a,b)); // Rmath
}

double muste_pow(double a,double b)
{
    return(R_pow(a,b));  // Rmath
}


    /* RS Statistical distribution functions from Rmath */

double muste_pdf_binom(double x,double n,double p)
    {
        return(dbinom(x,n,p,(int)0));
    }

double muste_cdf_binom(double x,double n,double p)
    {
        return(pbinom(x,n,p,(int)1,(int)0));
    }

double muste_inv_binom(double x,double n,double p)
    {
        return(qbinom(x,n,p,(int)1,(int)0));
    }

double muste_pdf_poisson(double x,double lambda)
    {
        return(dpois(x,lambda,(int)0));
    }

double muste_cdf_poisson(double x,double lambda)
    {
        return(ppois(x,lambda,(int)1,(int)0));
    }

double muste_inv_poisson(double x,double lambda)
    {
        return(qpois(x,lambda,(int)1,(int)0));
    }

double muste_pdf_normal(double x,double mean,double var)
    {
        return(dnorm(x,mean,sqrt(var),(int)0));
    }

double muste_density_normal(double x,double mean,double var,double x3)
    {
        return(dnorm(x,mean,sqrt(var),(int)x3));        
    }

double muste_cdf_normal(double x,double mean,double var)
    {
        return(pnorm(x,mean,sqrt(var),(int)1,(int)0));
    }

double muste_inv_normal(double x,double mean,double var)
    {
        return(qnorm(x,mean,sqrt(var),(int)1,(int)0));
    }

double muste_pdf_t(double x,double df)
    {
        return(dt(x,df,(int)0));
    }

double muste_cdf_t(double x,double df)
    {
        return(pt(x,df,(int)1,(int)0));
    }

double muste_inv_t(double x,double df)
    {
        return(qt(x,df,(int)1,(int)0));
    }

double muste_pdf_chi2(double x,double df)
    {
        return(dchisq(x,df,(int)0));
    }

double muste_cdf_chi2(double x,double df)
    {
        return(pchisq(x,df,(int)1,(int)0));
    }

double muste_inv_chi2(double x,double df)
    {
        return(qchisq(x,df,(int)1,(int)0));
    }

double muste_pdf_f(double x,double n1,double n2)
    {
        return(df(x,n1,n2,(int)0));
    }

double muste_cdf_f(double x,double n1,double n2)
    {
        return(pf(x,n1,n2,(int)1,(int)0));
    }

double muste_inv_f(double x,double n1,double n2)
    {
        return(qf(x,n1,n2,(int)1,(int)0));
    }

double muste_pdf_gamma(double x,double shape,double scale)
    {
        return(dgamma(x,shape,scale,(int)0));
    }

double muste_cdf_gamma(double x,double shape,double scale)
    {
        return(pgamma(x,shape,scale,(int)1,(int)0));
    }

double muste_inv_gamma(double x,double shape,double scale)
    {
        return(qgamma(x,shape,scale,(int)1,(int)0));
    }

double muste_pdf_beta(double x,double a,double b)
    {
        return(dbeta(x,a,b,(int)0));
    }

double muste_cdf_beta(double x,double a,double b)
    {
        return(pbeta(x,a,b,(int)1,(int)0));
    }

double muste_inv_beta(double x,double a,double b)
    {
        return(qbeta(x,a,b,(int)1,(int)0));
    }

double muste_pdf_weibull(double x,double rate,double shape)
    {
        return(dweibull(x,shape,1/rate,(int)0));
    }

double muste_cdf_weibull(double x,double rate,double shape)
    {
        return(pweibull(x,shape,1/rate,(int)1,(int)0));
    }

double muste_inv_weibull(double x,double rate,double shape)
    {
        return(qweibull(x,shape,1/rate,(int)1,(int)0));
    }

double muste_pdf_exp(double x,double rate)
    {
        return(dexp(x,1/rate,(int)0));
    }

double muste_cdf_exp(double x,double rate)
    {
        return(pexp(x,1/rate,(int)1,(int)0));
    }

double muste_inv_exp(double x,double rate)
    {
        return(qexp(x,1/rate,(int)1,(int)0));
    }

double muste_max(double *x,int n)
    {
        int i;
        double y;
        y=x[0];
        for (i=1; i<n; ++i) y=(x[i]>y)? (x[i]):(y);
        return(y);
    }
    
double muste_min(double *x,int n)    
    {
        int i;
        double y;
        y=x[0];
        for (i=1; i<n; ++i) y=(x[i]<y)? (x[i]):(y);
        return(y);
    }

double muste_maxn(double *x,int n) 
    {
        int i,k;
        double y;
        y=x[0];
        k=0;
        for (i=1; i<n; ++i) if (x[i]>y)
            {
                y=x[i];
                k=i;
            }
        return((double)(k+1));
    }

double muste_minn(double *x,int n)
    {
        int i,k;
        double y;
        y=x[0];
        k=0;
        for (i=1; i<n; ++i) if (x[i]<y)
            {
                y=x[i];
                k=i;
            }
        return((double)(k+1));
    }

double muste_C(double v,double u)
    {
        double y;
        int iu,iv;

        iv=v;
        iu=u;
        if ((double)iu!=u) return(0.0);
        if ((double)iv!=v) return(0.0);
        if (u>v/2) u=v-u;
        if (u<0 || v<0) return(0.0);
        if (u==0.0) return(1.0);
        y=1.0;
        for (; u>0; --u, --v) y*=(v/u);
        return(y);
    }

double muste_k_fact(double v,double u,int h)
    {
        int i;
        double y;

        if (u<1.0) return(0.0);
        if (h)
        {
            y=log(v);
            for (i=1; i<u; ++i) y+=log(v-i);
        }
        else
        {
            y=v;
            for (i=1; i<u; ++i) y*=v-i;
        }
        return(y);
    }

double muste_mod(double x0,double x1)
    {
        return((double)((unsigned long)x0%(unsigned long)x1));
    }

double muste_round(double x0,double x1)
    {
        double y;
        y=pow(10.0,x1);
        return(sur_round(x0*y)/y);
    }

double muste_st_norm(double x,double upper)
    {
    if (upper==0.0)
        return(pnorm(x,0.0,1.0,(int)1,(int)0));
    else
        return(1.0-pnorm(x,0.0,1.0,(int)1,(int)0));
    }
    
double muste_cdf_std(double x)
    {
    return(muste_st_norm(x,0.0));
    }
    
double muste_inv_std(double p)
    {
    return(qnorm(p,0.0,1.0,(int)1,(int)0));
    }
    
