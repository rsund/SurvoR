/* !quanta.c 10.11.2005/SM (10.11.2005)
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
// #include <conio.h>
#include <math.h>
// #include <malloc.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAX_Q 1000
#define MAX_N 1000000

// QUANTA <data>,<var>,<dim>,L
static SURVO_DATA d;
static char name[LNAME];
static int var;
static int dim;
static int var_quant,var_res,var_coeff;
static double q[MAX_Q];
static double qmin[MAX_Q];
static char *s[MAX_Q];
static double xi[MAX_Q*MAX_Q];
static double x_min,x_max,step;
static double dat[MAX_N];
static double weight[MAX_N];
static int var_weight;
static int elem[MAX_Q];
static int n_elem;
static int n;
static int totnf;
static double q_min;
static int results_line;

// char **specs;

extern char **spb;
extern char **spa;
extern int spn;

static double sum_fit(double *q);
static double log_sum_fit(double *q);
static int next_m_comb(int n,int m,int *p);
static int print_line(char *line);
static int not_mem();
static int kendall();
static char *spois(char *s);
static int powell(double *p, double *xi, int n, double ftol, double *fret,
            double (*func)(double *));
static void linmin(double *p,double *xi,int n,double *fret,double
           (*func)(double *));
static double f1dim(double x);
static double brent(double ax,double bx,double cx,double (*f)(double),
             double tol,double *xmin);
static void mnbrak(double *ax,double *bx,double *cx,
           double *fa,double *fb,double *fc,double (*func)(double));


/*********************
void main(argc,argv)
int argc; char *argv[];
***********************/

void muste_quanta(char *argv)
    {
    int i,k,imin;
//  int nf,maxnf;
    double a;
    double (*f)();
    int j,jn;
    double b;
    double y,y_min;
    char *p,*p2;
    char luku1[20]; // luku2[20];

//  if (argc==1) return;
    s_init(argv);
    i=spec_init(r1+r-1); if (i<0) return;
    if (g<4)
        {
        init_remarks();
        rem_pr("Usage: QUANTA <data>,<variable>,<dim>,L");
        rem_pr("More information by QUANTA?            ");
        wait_remarks(2);
        s_end(argv[1]);
        return;
        }
    strcpy(name,word[1]);
    i=data_open(word[1],&d);
    if (i<0) return;

    var=varfind(&d,word[2]);
    if (var<0) return;

    dim=atoi(word[3]);
    if (dim>MAX_Q)
        {
        sprintf(sbuf,"\nMax quanta is %d!",MAX_Q);
        sur_print(sbuf); WAIT; return;
        }

    results_line=0;
    if (g>4)
        {
        results_line=edline2(word[4],1,1);
        if (results_line==0) return;
        }

    q_min=2.0;
    i=spfind("Q_MIN");
    if (i>=0) q_min=atof(spb[i]);

    i=spfind("RANGE");
    if (i<0)
        {
        sur_print("\nSpecification RANGE=min(step)max missing!");
        WAIT; return;
        }
    strcpy(sbuf,spb[i]);

    p=strchr(sbuf,'(');
    if (p==NULL) return;
    *p=EOS;
    x_min=atof(sbuf);
    p2=strchr(p+1,')');
    if (p2==NULL) return;
    step=atof(p+1);
    x_max=atof(p2+1);

    n_elem=1+(int)((x_max-x_min)/step);
    for (i=0; i<dim; ++i) elem[i]=i;

    i=conditions(&d);
    if (i<0) return;


    var_quant=var_res=var_coeff=-1;
    i=spfind("RES");
    if (i>=0)
        {
        strcpy(sbuf,spb[i]);
        i=split(sbuf,s,3);
        if (i>0)
            {
            var_quant=varfind(&d,s[0]);
            if (var_quant<0) return;
            if (i>1)
                {
                var_res=varfind(&d,s[1]);
                if (var_res<0) return;
                }
            if (i>2)
                {
                var_coeff=varfind(&d,s[2]);
                if (var_coeff<0) return;
                }
            }
        }

    var_weight=-1;
    i=spfind("WEIGHT");
    if (i>=0)
        {
        var_weight=varfind(&d,spb[i]);
        if (var_weight<0) return;
        }

    n=0L;
    for (j=d.l1; j<=d.l2; ++j)
        {
        if (unsuitable(&d,j)) continue;
        data_load(&d,j,var,&a);
        if (a==MISSING8) continue;
        dat[n]=a;
        if (var_weight>=0)
            data_load(&d,j,var_weight,&weight[n]);
        ++n;
        }

    if (n<(int)(dim+1))
        {
        sur_print("\nNot enough acceptable observations!");
        sur_print(sbuf); WAIT; return;
        }

    i=spfind("ACCURACY");
    if (i>=0) accuracy=atoi(spb[i]);

    i=spfind("METHOD");
    if (i>=0 && *spb[i]=='K')
        {
        kendall();
        s_end(argv[1]);
        return;
        }

    if (i>=0 && *spb[i]=='L')
        f=log_sum_fit;
    else
        f=sum_fit;

    i=spfind("FIXED_QUANTA");
    if (i>=0)
        {
        strcpy(sbuf,spb[i]);
        split(sbuf,s,dim);
        for (i=0; i<dim; ++i) qmin[i]=atof(s[i]);
        y_min=f(qmin);
        }
    else
     {
    y_min=1e10;
    while (1)
        {
        for (i=0; i<dim; ++i)
            {
            q[i]=x_min+elem[i]*step;
//          printf("\nq[%d]=%g|",i,q[i]);
            }
        for (i=0; i<dim; ++i) for (k=0; k<dim; ++k)
            if (i==k) xi[i+dim*k]=1.0; else xi[i+dim*k]=0.0;

        totnf=powell(q,xi,dim,(double)1e-5,&y,f);
                                 // oli 1e-4
        if (y<y_min)
            {
            y_min=y;
            sprintf(sbuf,"\nss=%g ",y); sur_print(sbuf);
            for (i=0; i<dim; ++i)
                {
                qmin[i]=q[i];
                sprintf(sbuf,"q[%d]=%g ",i+1,q[i]); sur_print(sbuf);
                }
            }
        i=next_m_comb(n_elem,dim,elem);
        if (i<0) break;
        }
      }
    j=0L;
    for (i=0; i<dim; ++i) elem[i]=0;
    for (jn=d.l1; jn<=d.l2; ++jn)
        {
        double min,a1,a2,r;
        int kmin=0;

        if (unsuitable(&d,jn)) continue;

        min=1e100; imin=0;
        a=dat[j];

        for (i=0; i<dim; ++i)
            {
            r=qmin[i];
//          if (r<q_min) { return(1e2); }
            k=(int)(a/r);
            a1=k*r; a2=a1+r;
            b=a-a1; if (a2-a<b) { b=a-a2; ++k; }
// printf("\nq=%g a1=%g dat=%g a2=%g k=%d b=%g",r,a1,a,a2,k,b); getch();
            if (fabs(b)<fabs(min)) { min=b; imin=i; kmin=k; }
            }

        if (var_quant>=0) data_save(&d,jn,var_quant,(double)(imin+1));

        if (var_res>=0) data_save(&d,jn,var_res,min);

        if (var_coeff>=0) data_save(&d,jn,var_coeff,(double)kmin);

        ++elem[imin];
        ++j;
        }

    i=output_open(eout);
    if (i<0) return;
    sprintf(sbuf,"Data: %s Variable: %s  N=%d",word[1],word[2],n);
    print_line(sbuf);
    sprintf(sbuf,"ss=%g",y_min);
    print_line(sbuf);
    print_line("     quant       # matches");
    for (i=0; i<dim; ++i)
        {
        fnconv(qmin[i],accuracy+2,luku1);
        sprintf(sbuf,"%2d  %s       %3d",i+1,luku1,elem[i]);
        print_line(sbuf);
        }
    output_close(eout);
    data_close(&d);
    s_end(argv);
    }

static double sum_fit(double *q)
    {
    int i,k;
    double s,a,a1,a2,b,r,min;
    int j;

    s=0.0;
    for (j=0; j<n; ++j)
        {
        min=1e100;
        a=dat[j];

        for (i=0; i<dim; ++i)
            {
            r=q[i];
            if (r<q_min) { return(1e2); }
            k=(int)(a/q[i]);
            a1=k*r; a2=a1+r;
            b=a-a1; if (a2-a<b) b=a2-a;
// printf("\nk=%d q=%g a1=%g a=%g a2=%g b=%g|",k,r,a1,a,a2,b); getch();
            if (b<min) min=b;
            }
        if (var_weight>=0)
           s+=weight[j]*min*min;
        else
           s+=min*min;
        }
    return(s);
    }

static double log_sum_fit(double *q)
    {
    int i,k;
    double s,a,a1,a2,b,r,min,aa,al;
    int j;

    s=0.0;
    for (j=0; j<n; ++j)
        {
        min=1e100;
        a=dat[j];
        al=log(a);

        for (i=0; i<dim; ++i)
            {
            r=q[i];
            if (r<q_min) { return(1e2); }
            k=(int)(a/q[i]);
            aa=al-log(r);
            a1=fabs(aa-log((double)k));
            a2=fabs(aa-log((double)(k+1)));
            b=a1; if (a2<a1) b=a2;
            if (b<min) min=b;
            }
        s+=min*min;
        }
    return(s);
    }

static int next_m_comb(int n,int m,int *p)
        {
        int i,k;

        if (p[m-1]<n-1) { ++p[m-1]; return(1); }
        i=m-2;
        while (p[i]==n-m+i && i>=0 ) --i;
        if (i<0) return(-1);
        ++p[i]; k=p[i]; ++i;
        for ( ; i<m; ++i) p[i]=++k;
        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

static int not_mem()
    {
    sur_print("\nNot enough memory!");
    WAIT;
    return(1);
    }


static double *cc;
#define PI2 2*3.141592653589793
static SURVO_DATA d2;
#define N_PEAK 30

static int kendall()
    {
    int i,j,k;
    double q,s;
    char   *varname[]={"quant   ","score   "};
    int     varlen[2];
    char   *vartype[]={"8A-     ","8A-     "};
    double s_min;
    double cc1,cc0,cc2;
    char luku1[30],luku2[30];
    double peak_max;
    char *spois();
    double qmax[N_PEAK],peak[N_PEAK];
    int n_peak;

    s_min=1.5;
    i=spfind("SCORE_MIN");
    if (i>=0) s_min=atof(spb[i]);

    cc=(double *)muste_malloc(n_elem*sizeof(double));
    if (cc==NULL) { not_mem(); return(-1); }
    j=0;
    for (i=0; i<n_elem; ++i)
        {
        s=0.0;
        q=x_min+i*step;
        for (j=0; j<n; ++j)
            {
            k=(int)(dat[j]/q);
            s+=cos(PI2*(dat[j]-k*q)/q);
            }
       cc[i]=s;
       }
    q=sqrt(2.0/(double)n);
    for (i=0; i<n_elem; ++i) cc[i]*=q;

    varlen[0]=varlen[1]=8;
    fi_create("COSQUANT",16,2,2,(int)n_elem,64,7,0,0,NULL,varname,varlen,vartype);
    i=data_open("COSQUANT",&d2);
    if (i<0) return(-1);

    for (i=0; i<n_elem; ++i)
        {
        q=x_min+i*step;
        data_save(&d2,(int)(i+1),0,q);
        data_save(&d2,(int)(i+1),1,cc[i]);
        }
    data_close(&d2);

    i=output_open(eout);
    if (i<0) return(-1);
    sprintf(sbuf,"Data: %s Variable: %s  N=%d",word[1],word[2],n);
    print_line(sbuf);
    print_line(
      "GPLOT COSQUANT,quant,score / LINE=1 MODE=SVGA Plot the quantogram!");
    sprintf(sbuf,"Peaks of Kendall's Cosine Quantogram:");
    print_line(sbuf);
    print_line(" quantum      score");
    i=1; cc1=0.0; cc0=cc[0]; peak_max=0.0; n_peak=0;
    while (i<n_elem)
        {
        cc2=cc[i];
        if (cc0>cc1 && cc0>cc2 && cc0>s_min)
            {
            q=x_min+(i-1)*step;
            if (cc0>peak_max) { peak[n_peak]=cc0; qmax[n_peak++]=q; }
            }
        if (n_peak>N_PEAK) { sur_print("\nToo many peaks! Increase SCORE_MIN!");
                             WAIT; return(-1);
                           }
        cc1=cc0; cc0=cc2;
        ++i;
        }

    for (i=0; i<n_peak; ++i)
        {
        peak_max=0.0;
        for (k=0; k<n_peak; ++k)
            {
            if (peak[k]>peak_max) { peak_max=peak[k]; j=k; }
            }
        fnconv(qmax[j],accuracy+2,luku1);
        fnconv(peak_max,accuracy+2,luku2);
        sprintf(sbuf,"%s   %s",spois(luku1),spois(luku2));
        print_line(sbuf);
        peak[j]=0.0;
        }

    output_close(eout);

    return(1);
    }

static char *spois(char *s)
    {
    if (*s==' ') ++s;
    return(s);
    }

#define ITMAX 200

static int powell(double *p, double *xi, int n, double ftol, double *fret,
            double (*func)(double *))
    {
    void linmin(double *p,double *xi,int n,double *fret,
            double (*func)(double *));
    int i,ibig,j;
    double del,fp,fptt,t,*pt,*ptt,*xit;
    int iter,stop;

    pt=(double *)muste_malloc(n*sizeof(double));
    if (pt==NULL) { not_mem(); return(-1); }
    ptt=(double *)muste_malloc(n*sizeof(double));

    if (ptt==NULL) { not_mem(); return(-1); }
    xit=(double *)muste_malloc(n*sizeof(double));
    if (xit==NULL) { not_mem(); return(-1); }
    *fret=(*func)(p);
    for (j=0; j<n; ++j) pt[j]=p[j];
    iter=0; totnf=0;
    while (1)
      {
      ++iter;
      fp=(*fret);
      ibig=0;
      del=0.0;
      for (i=0; i<n; ++i)
        {
        for (j=0; j<n; ++j) xit[j]=xi[j+n*i];
        fptt=(*fret);
        linmin(p,xit,n,fret,func);
// printf("\n*fret=%g|",*fret);
        if (fabs(fptt-(*fret)) > del)
          {
          del=fabs(fptt-(*fret));
          ibig=i;
          }
        }

      stop=0;
      if (sur_kbhit())
          {
          i=sur_getch();
          if (i=='.') stop=1;
          }


      if (stop==1 || 2.0*fabs(fp-(*fret)) <= ftol*(fabs(fp)+fabs(*fret)))
        {
        muste_free(xit); muste_free(ptt); muste_free(pt);
        return(totnf);
        }
//    sprintf(sbuf,"\n%d %g",iter,*fret); sur_print(sbuf);
      if (iter > ITMAX)
        {
        sur_print("No convergence!"); return(1);
        }
      for (j=0; j<n; ++j)
        {
        ptt[j]=2*p[j]-pt[j];
        xit[j]=p[j]-pt[j];
        pt[j]=p[j];
        }
      fptt=(*func)(ptt); ++totnf;
      if (fptt<fp)
        {
        double aa,bb;

        aa=fp-(*fret)-del; bb=fp-fptt;
        t=2*(fp-2*(*fret)+fptt)*aa*aa-del*bb*bb;
        if (t<0)
          {
          linmin(p,xit,n,fret,func);
          for (j=0; j<n; ++j)
            {
            xi[j+n*ibig]=xi[j+n*(n-1)];
            xi[j+n*(n-1)]=xit[j];
            }
          }

        }


      } // while
    return(totnf);
    }

#define TOL 2.0e-4

static int ncom;
static double *pcom,*xicom,(*nrfunc)(double *);

static void linmin(double *p,double *xi,int n,double *fret,double
           (*func)(double *))
    {
    double brent(double ax,double bx, double cx,
                 double (*f)(double),double tol,double *xmin);
    double f1dim(double x);
    void mnbrak(double *ax,double *bx,double *cx,double *fa,double *fb,
               double *fc,double (*func)(double));
    int j;
    double xx,xmin,fx,fb,fa,bx,ax;

    ncom=n;
    pcom=(double *)muste_malloc(n*sizeof(double));
    if (pcom==NULL) { not_mem(); return; }
    xicom=(double *)muste_malloc(n*sizeof(double));
    if (xicom==NULL) { not_mem(); return; }
    nrfunc=func;
    for (j=0; j<n; ++j)
      {
      pcom[j]=p[j];
      xicom[j]=xi[j];
      }
    ax=0.0; xx=1.0;
    mnbrak(&ax,&xx,&bx,&fa,&fx,&fb,f1dim);
// printf("\nlinmin: ax=%g xx=%g bx=%g|",ax,xx,bx); getch();
    *fret=brent(ax,xx,bx,f1dim,TOL,&xmin);
    for (j=0; j<n; ++j)
      {
      xi[j]*=xmin;
      p[j]+=xi[j];
      }
    muste_free(xicom); muste_free(pcom);
    }

static double f1dim(double x)
    {
    int j;
    double f,*xt;

    xt=(double *)muste_malloc(ncom*sizeof(double));
    if (xt==NULL) { not_mem(); return(0.0); }
    for (j=0; j<ncom; ++j) xt[j]=pcom[j]+x*xicom[j];
    f=(*nrfunc)(xt); ++totnf;
    muste_free(xt);
    return(f);
    }


#define B_ITMAX 100
#define CGOLD 0.3819660
#define ZEPS 1.0e-10
#define SHFT(a,b,c,d) (a)=(b);(b)=(c);(c)=(d);
#define SIGN(a,b) ((b)>=0.0 ? fabs(a) : -fabs(a))

static double brent(double ax,double bx,double cx,double (*f)(double),
             double tol,double *xmin)
    {
    int iter;
    double a,b,d=0,etemp,fu,fv,fw,fx,p,q,r,tol1,tol2,u,v,w,x,xm;
    double e=0.0;
// printf("\nax=%g bx=%g cx=%g|",ax,bx,cx); getch();
    a=(ax<cx ? ax:cx);
    b=(ax>cx ? ax:cx);
// printf("\na=%g b=%g|",a,b); getch();
    x=w=v=bx;
    fw=fv=fx=(*f)(x); ++totnf;
    for (iter=1; iter<=B_ITMAX; ++iter)
      {
      xm=0.5*(a+b);
// printf("\niter=%d xm=%g|",iter,xm); getch();
      tol2=2*(tol1=tol*fabs(x)+ZEPS);
      if (fabs(x-xm)<=(tol2-0.5*(b-a)))
        {
        *xmin=x; return(fx);
        }
      if (fabs(e)>tol1)
        {
        r=(x-w)*(fx-fv);
        q=(x-v)*(fx-fw);
        p=(x-v)*q-(x-w)*r;
        q=2*(q-r);
        if (q>0.0) p=-p;
        q=fabs(q);

        etemp=e;
        e=d;
        if (fabs(p)>=fabs(0.5*q*etemp) || p<=q*(a-x) || p>=q*(b-x))
            d=CGOLD*(e=(x>=xm ? a-x:b-x));
        else
          {
          d=p/q;
          u=x+d;
          if (u-a<tol2 || b-u<tol2) d=SIGN(tol1,xm-x);
          }
        }
      else d=CGOLD*(e=(x>=xm ? a-x:b-x));

      u=(fabs(d)>=tol1 ? x+d:x+SIGN(tol1,d));
      fu=(*f)(u); ++totnf;
      if (fu<=fx)
        {
        if (u>=x) a=x; else b=x;
        SHFT(v,w,x,u)
        SHFT(fv,fw,fx,fu)
        }
      else
        {
        if (u<x) a=u; else b=u;
        if (fu<=fw || w == x)
            { v=w; w=u; fv=fw; fw=fu; }
        else if (fu<=fv|| v==x || v==w) { v=u; fv=fu; }
        }




      } // iter
    sur_print("\nbret: too many iterations!");
    *xmin=x;
    return(fx);
    }

#define GOLD 1.618034
#define GLIMIT 100.0
#define TINY 1.0e-20

static double dm1,dm2;
#define DMAX(a,b) (dm1=(a),dm2=(b),(dm1)>(dm2) ? (dm1):(dm2))


static void mnbrak(double *ax,double *bx,double *cx,
           double *fa,double *fb,double *fc,double (*func)(double))
    {
    double ulim,u,r,q,fu,dum;

//  u=0.0; // ???????????
    *fa=(*func)(*ax);
    *fb=(*func)(*bx); totnf+=2;
    if (*fb>*fa)
      {
      dum=*ax; *ax=*bx; *bx=dum;
      dum=*fa; *fa=*fb; *fb=dum;
//    SHFT(dum,*ax,*bx,dum)
//    SHFT(dum,*fb,*fa,dum)
      }
    *cx=(*bx)+GOLD*(*bx-*ax);
    *fc=(*func)(*cx); ++totnf;
// printf("\nmnbrak: *cx=%g *fb=%g *fc=%g|",*cx,*fb,*fc); getch();
    while (*fb>*fc)
      {
// printf("\nmnbrak: *cx=%g *fc=%g|",*cx,*fc); getch();
      r=(*bx-*ax)*(*fb-*fc);
      q=(*bx-*cx)*(*fb-*fa);
      u=(*bx)-((*bx-*cx)*q-(*bx-*ax)*r)/
               (2*SIGN(DMAX(fabs(q-r),TINY),q-r));
      ulim=(*bx)+GLIMIT*(*cx-*bx);
      if ((*bx-u)*(u-*cx)>0.0)
        {
        fu=(*func)(u); ++totnf;
        if (fu<*fc)
          {
          *ax=(*bx); *bx=u; *fa=(*fb); *fb=fu;
          return;
          }
        else if (fu>*fb)
          {
          *cx=u; *fc=fu;
          return;
          }
        u=(*cx)+GOLD*(*cx-*bx);
        fu=(*func)(u); ++totnf;
        }
      else if ((*cx-u)*(u-ulim)>0.0)
        {
        fu=(*func)(u); ++totnf;
        if (fu<*fc)
          {
          SHFT(*bx,*cx,u,*cx+GOLD*(*cx-*bx))
          SHFT(*fb,*fc,fu,(*func)(u))
          ++totnf;
          }
        }
      else if ((u-ulim)*(ulim-*cx)>=0.0)
        {
        u=ulim; fu=(*func)(u); ++totnf;
        }
      else
        {
        u=(*cx)+GOLD*(*cx-*bx);
        fu=(*func)(u); ++totnf;
        }
      SHFT(*ax,*bx,*cx,u)
      SHFT(*fa,*fb,*fc,fu)
      } // while
    }

