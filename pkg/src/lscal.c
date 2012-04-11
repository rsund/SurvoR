/* !lscal.c 21.5.1994/SM (12.6.1994)
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

static double *dd;
static int n;
static int m;  /* dimension of Euclidean space */
static int npar;
static int dtrans; /* 0: no distance transformation */
                   /* 1: F(D)=D+C     */
static int power_not_2=0;
static double dpower=2.0;
static double fitpower=2.0;

static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];
static double *xx;
static double y;
static double *step;
static double *xx2,*xx3,*d,*e;
static double *w;
static int weight=0;
static int is_symmetric=0; // 25.6.2005
static int results_line;
static int method=1; // Powell
static double *xi;

/*******************
char *specs0[]={ "WEIGHT", "WEIGHTS", "METRICS", "CRITERION", "MAXNF",
                 "STEP", "EPS", "CONSTANT", "!" };
char **specs=specs0;
*************************/
extern double sis_tulo();
// static double l_luku=0.0; /* mat */

extern char **spb;
static int totnf=0;
static double dist_sum;
static int lstsqr=1; // 1,2,3

static double *bb; // 4.7.2005

static  int space_allocated=0; // polytope

static int poista_sp(char *x);
static int varaa_tilat();
static int test_symmetry(double *dd,int n);
static double sqrsum1(double *xx);
static void grad_sqrsum1(double *xx,double *grad);
static double sqrsum2(double *xx);
static double powersum(double *xx);
static int estim_dist();
static int tutki_dtrans();
static int alusta_dtrans();
static double mitta(char *x);
static int not_mem();
static int print_line(char *x);
static int polytope(double *x,double *py,int nn,double (*f)(),
       double *step,double alpha,double beta,double gamma,
       int maxnf,double eps,int *pstop);
static int disp_notice();
static int poly_malloc(int n);
static int not_mem_for_polytope();
static int powell(double *p, double *xi, int n, double ftol, double *fret,
            double (*func)(double *));
static void linmin(double *p,double *xi,int n,double *fret,double
           (*func)(double *));
static double f1dim(double x);
static double brent(double ax,double bx,double cx,double (*f)(double),
             double tol,double *xmin);
static void mnbrak(double *ax,double *bx,double *cx,
           double *fa,double *fb,double *fc,double (*func)(double));
static int  frprmn(double *p, int n, double ftol, double *fret,
            double (*func)(double *), void (*dfunc)(double *,double *));

void muste_lscal(char *argv)
        {
        int i,n2,n3,j;
        int nf,maxnf;
        double a;
        double (*f)();
        double *pscal;
        extern double sqrsum1();
        extern double sqrsum2();
        extern double powersum();
        extern double mitta();
        extern int polytope();
        char x[LLENGTH];
        char metrics[16];
        char criterion[16];
        double eps,y0,dy;
        int stop;
        extern void grad_sqrsum1();

   //   if (argc==1) return;
        s_init(argv);
        i=spec_init(r1+r-1); if (i<0) return;
        if (g<3)
            {
            init_remarks();
            rem_pr("Multidimensional least-squares scaling etc.               ");
            rem_pr("                                                          ");
            rem_pr("LSCAL <distance_matrix>,<initial_coordinates>,L           ");
            rem_pr("computes on the basis of a given n*n dissimilarity or     ");
            rem_pr("<distance matrix> a configuration of n points in an m     ");
            rem_pr("dimensional space. The distances of points in that        ");
            rem_pr("configuration should be as close to the given distances as");
            rem_pr("possible. <initial coordinates> gives initial estimates   ");
            rem_pr("of the configuration as a matrix file. # of columns in    ");
            rem_pr("<initial coordinates> gives the dimension m.              ");
            rem_pr("The result CSCAL.M of classical multidimensional scaling  ");
            rem_pr("obtained by the sucro /CSCAL (or /CSCAL2 for large n and  ");
            rem_pr("small m) is often a good choice for <initial coordinates>.");
            wait_remarks(2);
            s_end(argv[1]);
            return;
            }

        power_not_2=0;
        dpower=2.0;
        fitpower=2.0;
        is_symmetric=0;
        method=1;
        totnf=0;
        lstsqr=1;
        space_allocated=0; // polytope

        results_line=0;
        if (g>3)
            {
            results_line=edline2(word[3],1);
            if (results_line==0) return;
            }
        i=sp_init(r1+r-1); if (i<0) return;

        method=1;
        i=spfind("METHOD");
        if (i>=0) method=atoi(spb[i]);

        i=spfind("WEIGHT");
        if (i<0) i=spfind("WEIGHTS");
        if (i>=0)
            {
            weight=1; method=2;
            strcpy(x,spb[i]);
            i=matrix_load(x,&w,&n,&n3,&rlab,&clab,&lr,&lc,&type,expr);
            if (i<0) return;
            if (n3!=n)
                {
                sprintf(sbuf,"\n%s not a square matrix!",x); sur_print(sbuf); WAIT; return;
                }
            }
        i=matrix_load(word[1],&dd,&n,&n2,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return;

        if (n2!=n)
            {
            sprintf(sbuf,"\n%s not a square matrix!",word[1]); sur_print(sbuf); WAIT; return;
            }
        i=matrix_load(word[2],&xx,&n2,&m,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return;

        if (n2!=n)
            {
            sprintf(sbuf,"\n# of rows in %s (%d) not equal to %d!",
                               word[2],n2,n);
            sur_print(sbuf); WAIT; return;
            }

        dist_sum=0.0;
        for (i=0; i<n; ++i) for (j=0; j<n; ++j)
            if (i!=j && dd[i+n*j]>0.0) dist_sum+=dd[i+n*j];

        is_symmetric=1;
        i=spfind("SYMMETRIC");
        if (i>=0)
            {
            i=atoi(spb[i]);
            if (i!=0)
                is_symmetric=test_symmetry(dd,n); // 25.6.2005
            else is_symmetric=0;
            }

        if (weight && n3!=n)
            {
         sprintf(sbuf,"Dimension %d of weight matrix not equal to that of distance matrix %d",
                                  n3,n); WAIT; return;
            }

        npar=n*m;
        i=tutki_dtrans(); if (i<0) return;
        if (dtrans) method=2;

        strcpy(metrics,"L2");
        i=spfind("METRICS");
        if (i>=0)
            {
            strcpy(metrics,spb[i]);
            dpower=mitta(metrics);
            if (dpower!=2.0) method=2;
            }
        strcpy(criterion,"L2");
        i=spfind("CRITERION");
        if (i>=0)
            {
            strcpy(criterion,spb[i]);
            fitpower=mitta(criterion);
            if (fitpower!=2.0) method=2;
            }
        if (dpower!=2.0 || fitpower!=2.0) power_not_2=1;

        i=varaa_tilat(); if (i<0) return;

        alusta_dtrans();

        output_open(eout);
        if (power_not_2==0)
            {
          sprintf(sbuf,"Least-squares scaling for %d*%d dissimilarity (distance) matrix %s:",
                                     n,n,word[1]);
            print_line(sbuf);
            }
        else
            {
       sprintf(sbuf,"Multidimensional scaling for %d*%d dissimilarity (distance) matrix %s:",
                                     n,n,word[1]);
            print_line(sbuf);
            sprintf(sbuf,"Metrics=%s Criterion=%s",metrics,criterion);
            print_line(sbuf);
            }
        maxnf=10000L;
        i=spfind("MAXNF");
        if (i>=0) maxnf=atol(spb[i]);

        if (dtrans) f=sqrsum2; else f=sqrsum1;
        if (power_not_2) f=powersum;
        y=f(xx);

        sprintf(sbuf,"Initial criterion value %g  Dimension=%d",y,m);
        print_line(sbuf);

        for (i=0; i<npar; ++i) step[i]=1.0;
        i=spfind("STEP");
        if (i>=0) { y=atof(spb[i]); for (i=0; i<npar; ++i) step[i]=y; }

        eps=1e-05;
        i=spfind("EPS");
        if (i>=0) eps=atof(spb[i]);
        totnf=0;

      if (method==1 || method==12 || method==13)
          {
          bb=(double *)muste_malloc(npar*npar*sizeof(double));
          if (bb==NULL) { not_mem(); return; }
          if (method==1) lstsqr=1;
          else if (method==12) lstsqr=2;
          else lstsqr=3;
          totnf=frprmn(xx,npar,(double)1e-3,&y,f,grad_sqrsum1);
          }
      else if (method==2)
          {
          xi=(double *)muste_malloc(npar*npar*sizeof(double));
          if (xi==NULL) { not_mem(); return; }

          for (i=0; i<npar; ++i) for (j=0; j<npar; ++j)
              if (i==j) xi[i+npar*j]=1.0; else xi[i+npar*j]=0.0;

          totnf=powell(xx,xi,npar,(double)1e-2,&y,f);
          if (totnf<0) return;
          }

     else
        while (1)
            {
            y0=y; stop=0;
            nf=polytope(xx,&y,npar,f,step,1.0,0.5,2.0,maxnf,eps,&stop);

            totnf+=nf;
            if (stop==2) break;
            if (stop!=1)
                {
                dy=(y0-y)/(fabs(y0)+1e-10);
                if (dy<eps || y<eps) break;
                }
            sprintf(sbuf,"\nNew start: f=%g",y); sur_print(sbuf);
            } // while
        mat_center(e,xx,n,m);
        pscal=xx;
        if (m>1 && power_not_2==0)
            {
            mat_mtm(xx2,xx,n,m);
            mat_tred2(d,e,xx2,m,1e-284);
            mat_tql2(d,e,xx2,m,1e-16,30);
            mat_mlt(xx3,xx,xx2,n,m,m);
            pscal=xx3;
            }
        matrix_save("LSCAL.M",pscal,n,m,rlab,clab,lr,lc,0,"LS_scales",0,0);
        sprintf(sbuf,"Final criterion value %g  nf=%d",y,totnf);
        print_line(sbuf);

        if (dtrans)
            {
            i=sprintf(sbuf,"Distance transformation ");
            switch (dtrans)
                {
              case 1:
                      a=xx[npar-1];
                      if (a>=0.0)
                          sprintf(sbuf+i,"D+%g",a);
                      else
                          sprintf(sbuf+i,"D-%g",-a);
                      print_line(sbuf); break;

                }
            }
        sprintf(sbuf,"MAT LOAD LSCAL.M,END+2  /  Solution in %d dimensions",m);
        print_line(sbuf);

        estim_dist();
        matrix_save("LSDIST.M",dd,n,n,rlab,rlab,lr,lr,0,"LS_distances",0,0);
        sprintf(sbuf,"MAT LOAD LSDIST.M,END+2  /  Estimated distances");
        print_line(sbuf);

        if (m>1)
            {
            strcpy(sbuf,"GPLOT LSCAL.M,");
            strncpy(x,clab,lc); x[lc]=EOS; poista_sp(x); strcat(sbuf,x); strcat(sbuf,",");
            strncpy(x,clab+lc,lc); x[lc]=EOS; poista_sp(x); strcat(sbuf,x);
            strcat(sbuf," / POINT=[SMALL],CASE");
            print_line(sbuf);
            }
        output_close(eout);
        s_end(argv);
        }

static int poista_sp(char *x)
        {
        int i;

        i=strlen(x); while (x[i-1]==' ' && i>0) x[--i]=EOS;
        return(1);
        }

static int varaa_tilat()
        {
        if (dtrans)
            {
            xx=(double *)muste_realloc(xx,npar*sizeof(double));
            if (xx==NULL) { not_mem(); return(-1); }
            }
        step=(double *)muste_malloc(npar*sizeof(double));
        if (step==NULL) { not_mem(); return(-1); }
        xx2=(double *)muste_malloc(m*m*sizeof(double));
        if (xx2==NULL) { not_mem(); return(-1); }
        d=(double *)muste_malloc(m*sizeof(double));
        if (d==NULL) { not_mem(); return(-1); }
        e=(double *)muste_malloc(m*sizeof(double));
        if (e==NULL) { not_mem(); return(-1); }
        xx3=(double *)muste_malloc(n*m*sizeof(double));
        if (xx3==NULL) { not_mem(); return(-1); }
        return(1);
        }

static int test_symmetry(double *dd,int n)
    {
    int i,j;

    for (i=0; i<n; ++i)
        for (j=1; j<i; ++j)
            if (fabs(dd[i+n*j]-dd[j+n*i])/(1.0+fabs(dd[i+n*j]))
                       >1e-5)
                return(0);
    return(1);
    }

static double sqrsum1(double *xx)
        {
        int i,j,k,imax;
        double s,a,b;

        s=0.0;
        for (j=0; j<n; ++j)
          {
        if (is_symmetric) imax=j; else imax=n;
        for (i=0; i<imax; ++i)
            {
            if (i==j) continue;
            if (dd[i+n*j]<=0.0) continue;
            a=0;
            for (k=0; k<m; ++k) { b=xx[i+n*k]-xx[j+n*k]; a+=b*b; }
            a=sqrt(a);
            b=dd[i+n*j]-a;

            if (!weight)
                switch(lstsqr)
                    {
                  case 1:
                    s+=b*b;
                    break;
                  case 2:
                    b/=dd[i+n*j];
                    s+=b*b;
                    break;
                  case 3:
                    s+=b*b/dd[i+n*j];
                    break;
                    }
            else
                s+=w[i+n*j]*b*b;
            }
          }
        if (is_symmetric) s*=2;
// printf("\n%g",s); getch();
        s/=dist_sum;
        return(s);
        }

static void grad_sqrsum1(double *xx,double *grad)
    {
    int r,s,k,smax;
    double a,b;

    for (r=0; r<m*n; ++r) grad[r]=0.0;
    for (r=0; r<n; ++r)
      {
      if (is_symmetric) smax=r; else smax=n;
      for (s=0; s<smax; ++s)  // nopeuta (symmetria!)
        {
        if (s==r) continue;
        if (dd[r+n*s]<=0.0) { bb[r+n*s]=0.0; continue; }
        a=0.0;
        for (k=0; k<m; ++k) { b=xx[s+n*k]-xx[r+n*k]; a+=b*b; }
        b=sqrt(a);
        switch(lstsqr)
            {
          case 1:
            bb[r+n*s]=(dd[r+n*s]-b)/b/dist_sum;
            break;
          case 2:
            a=dd[r+n*s];
            bb[r+n*s]=(a-b)/b/dist_sum/(a*a);
            break;
          case 3:
            a=dd[r+n*s];
            bb[r+n*s]=(a-b)/b/dist_sum/a;
            break;
            }
        if (is_symmetric) bb[s+n*r]=bb[r+n*s];
        }
      }

//  mprint(bb,n,n);


    for (r=0; r<n; ++r)
      {
      for (k=0; k<m; ++k)
        for (s=0; s<n; ++s)
          {
          if (s==r) continue;
          grad[r+n*k]+=bb[r+n*s]*(xx[s+n*k]-xx[r+n*k]);
          }
      }

//  mprint(grad,n,m);

    }


static double sqrsum2(double *xx)
        {
        int i,j,k,imax;
        double s,a,b,c;
        double c_const;
//      double c_exp;

        c_const=0.0;
        if (dtrans==1) c_const=xx[npar-1];

        s=0.0;
        for (j=0; j<n; ++j)
          {
        if (is_symmetric) imax=j; else imax=n;
        for (i=0; i<imax; ++i)
            {
            if (i==j) continue;
            if (dd[i+n*j]<0.0) continue;
            if (dtrans==1)
                {
                a=dd[i+n*j]+c_const;
                if (a<0.0) { s+=-1000*a; continue; }
                }
            a=0;
            for (k=0; k<m; ++k) { b=xx[i+n*k]-xx[j+n*k]; a+=b*b; }
            c=dd[i+n*j];
            b=c+c_const-sqrt(a);
            if (!weight)
                s+=b*b;
            else
                s+=w[i+n*j]*b*b;
            }
          }
        if (is_symmetric) s*=2;
        return(s);
        }

static double powersum(double *xx)
        {
        int i,j,k,imax;
        double s,a,b,c;
        double c_const;
  //    double c_exp;
        double ww;

        c_const=0.0;
        if (dtrans==1) c_const=xx[npar-1];

        s=0.0;
        for (j=0; j<n; ++j)
          {
        if (is_symmetric) imax=j; else imax=n;
        for (i=0; i<imax; ++i)
            {
            if (i==j) continue;
            if (dd[i+n*j]<0.0) continue;
            if (dtrans==1)
                {
                a=dd[i+n*j]+c_const;
                if (a<0.0) { s+=-1000*a; continue; }
                }
            a=0;
            if (dpower==2.0)
                {
                for (k=0; k<m; ++k)
                    {
                    b=xx[i+n*k]-xx[j+n*k]; a+=b*b;
                    }
                a=sqrt(a);
                }
            else if (dpower==1000.0)
                {
                a=0.0;
                for (k=0; k<m; ++k)
                    {
                    b=fabs(xx[i+n*k]-xx[j+n*k]);
                    if (b>a) a=b;
                    }
                }
            else
                {
                for (k=0; k<m; ++k)
                    {
                    b=fabs(xx[i+n*k]-xx[j+n*k]);
                    if (dpower!=1.0) b=pow(b,dpower);
                    a+=b;
                    }
                if (dpower!=1.0) a=pow(a,1.0/dpower);
                }
            c=dd[i+n*j];
            if (weight) ww=w[i+n*j]; else ww=1.0;
            b=fabs(c+c_const-a);
            if (fitpower==2.0) s+=ww*b*b;
            else if (fitpower==1.0) s+=ww*b;
            else if (fitpower==1000.0)
                {
                if (b>s) s=b;
                }
            else s+=ww*pow(b,fitpower);
            }
          }
        if (is_symmetric && fitpower!=1000.0) s*=2;
        return(s);
        }

static int estim_dist()
        {
        int i,j,k;
        double a,b;

        for (j=0; j<n; ++j)
        for (i=0; i<n; ++i)
            {
            if (i==j) continue;
            if (dd[i+n*j]<0.0) continue;
            a=0;
            if (dpower==2.0)
                {
                for (k=0; k<m; ++k) { b=xx[i+n*k]-xx[j+n*k]; a+=b*b; }
                dd[i+n*j]=sqrt(a);
                continue;
                }
            if (dpower==1000.0)
                {
                for (k=0; k<m; ++k)
                    {
                    b=fabs(xx[i+n*k]-xx[j+n*k]);
                    if (b>a) a=b;
                    }
                dd[i+n*j]=a;
                continue;
                }
            for (k=0; k<m; ++k)
                {
                b=fabs(xx[i+n*k]-xx[j+n*k]);
                a+=pow(b,dpower);
                }
            dd[i+n*j]=pow(a,1.0/dpower);
            }
        return(1);
        }

static int tutki_dtrans()
        {
        int i;
    //  char x[LLENGTH];

        i=spfind("CONSTANT");
        if (i>=0)
            {
            dtrans=1;
            ++npar;
            return(1);
            }
        return(1);
        }

static int alusta_dtrans()
        {
        int i;

        switch (dtrans)
            {
          case 0: break;
          case 1: xx[npar-1]=0.0;
                  i=spfind("CONSTANT");
                  if (i>=0) { xx[npar-1]=atof(spb[i]); break; }
                  i=spfind("C");
                  if (i>=0) xx[npar-1]=atof(spb[i]);
                  break;
            }
        return(1);
        }

static double mitta(char *x)
        {
        double power;

        if (muste_strcmpi(x,"MAD")==0 || muste_strcmpi(x,"ABS")==0) return(1.0);
        if (muste_strcmpi(x,"MAX")==0) return(1000.0);
        power=atof(x+1);
        if (*x!='L' || power<=0.0)
            {
            sprintf(sbuf,"\nUnknown or improper METRICS or CRITERION (%s)!",
                               x); sur_print(sbuf); WAIT; return(2.0);
            }
        return(power);
        }

static int not_mem()
        {
        sur_print("\nNot enough memory!"); WAIT;
        return(1);
        }

static int print_line(char *x)
        {
        output_line(x,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }
/****************************
mprint(A,m,n)
double *A;
int m,n;
    {
    int i,j;

    for (i=0; i<m; ++i)
        {
        printf("\n");
        for (j=0; j<n; ++j) printf("%g ",A[i+m*j]); sur_getch();
        }
    sur_getch(); return(1);
    }
*********************************/
/*  polytope.c 15.8.1986/SM (29.5.1994)
    Polytope algorithm by Nelder and Mead
*/


#define MAXPAR 90
#define N MAXPAR
static  double *xx;
static  double *y11;
static  double *xc,*x0,*x00;

static int polytope(double *x,double *py,int nn,double (*f)(),
       double *step,double alpha,double beta,double gamma,
       int maxnf,double eps,int *pstop)
// double *x;
// double *py;
// int n;
// double (*f)();
// double *step;
// double alpha,beta,gamma;
// int maxnf;
// double eps; /* stopping value */
// int *pstop;
        {
        int i,j;
        int nf;
        int jh,js,jl;
        double y0,y00;
        int disp=1;
        int ndisp=0;
        char ss[100];
        double yy,dy;
        int dispgap;
        int inote;

        if (!space_allocated) { i=poly_malloc(n); if (i<0) return(-1); }

        dispgap=100+n;
        nf=0;
        for (i=0; i<n; ++i) xx[i]=x[i];
        for (j=0; j<n; ++j)
            for (i=0; i<n; ++i)
                {
                if (i==j) xx[(j+1)*n+i]=x[i]+step[j];
                else      xx[(j+1)*n+i]=x[i];
                }
        for (i=0; i<n+1; ++i) y11[i]=(*f)(xx+i*n); nf+=n+1;
        yy=y11[0]; dy=1.0;
        disp_notice(); inote=0;

        while (1)
            {
            jh=0; js=1; jl=1; if (y11[0]<y11[1]) { jh=1; js=0; jl=0; }
            for (j=2; j<n+1; ++j)
                {
                if (y11[j]>y11[jh]) { js=jh; jh=j; }
                else if (y11[j]>y11[js]) js=j;
                else if (y11[j]<y11[jl]) jl=j;
                }

            for (i=0; i<n; ++i)
                {
                xc[i]=0.0;
                for (j=0; j<n+1; ++j)
                    if (j!=jh) xc[i]+=xx[j*n+i];
                xc[i]/=n;
                }

            if (sur_kbhit())
                {
                i=sur_getch();
                if (i=='.') { *pstop=2; break; }
                if (i=='N' || i=='n') { *pstop=1; break; }

                disp=1-disp;
                }

            ++ndisp;
            if (disp && ndisp>dispgap)
                {
                dy=(yy-y11[jl])/(fabs(yy)+1e-10);
                sprintf(ss,"\nnf=%d f=%g (f0-f)/f0=%g",
                             nf,y11[jl],dy);
                sur_print(ss);
                ++inote;
                if (inote>=10)
                   { disp_notice(); inote=0; }
                ndisp=0; yy=y11[jl];
                }

            if (nf>=maxnf || (dy<eps && nf>5*dispgap) )
                {
                for (i=0; i<n; ++i) x[i]=xx[jl*n+i];
                *py=y11[jl];
                return(nf);
                }

            for (i=0; i<n; ++i) x0[i]=(1+alpha)*xc[i]-alpha*xx[jh*n+i];
            y0=(*f)(x0); ++nf;

            if (y11[jl]<=y0 && y0<=y11[js])
                {
                for (i=0; i<n; ++i) xx[jh*n+i]=x0[i];
                y11[jh]=y0;
                continue;
                }
            if (y0<y11[jl])
                {
                for (i=0; i<n; ++i) x00[i]=gamma*x0[i]+(1-gamma)*xc[i];
                y00=(*f)(x00); ++nf;
                if (y00<y11[jl])
                    {
                    for (i=0; i<n; ++i) xx[jh*n+i]=x00[i];
                    y11[jh]=y00;
                    continue;
                    }
                else
                    {
                    for (i=0; i<n; ++i) xx[jh*n+i]=x0[i];
                    y11[jh]=y0;
                    continue;
                    }
                }

            /* y0>ys */
            if (y0<y11[jh])
                {
                for (i=0; i<n; ++i) x00[i]=beta*x0[i]+(1-beta)*xc[i];
                }
            else
                {
                for (i=0; i<n; ++i) x00[i]=beta*xx[jh*n+i]+(1-beta)*xc[i];
                }
            y00=(*f)(x00); ++nf;
            if (y00<y11[jh] && y00<y0)
                {
                for (i=0; i<n; ++i) xx[jh*n+i]=x00[i];
                y11[jh]=y00;
                continue;
                }

            for (j=0; j<n+1; ++j)
                {
                if (j==jl) continue;
                for (i=0; i<n; ++i) xx[j*n+i]=(xx[j*n+i]+xx[jl*n+i])/2;
                y11[j]=(*f)(xx+j*n); ++nf;
                }
            }
        for (i=0; i<n; ++i) x[i]=xx[jl*n+i];
        *py=y11[jl];
        return(nf);
        }

static int disp_notice()
        {
        sur_print("\nTo stop, press `.'   ");
        sur_print("To restart with a regular polytope, press `N'");
        return(1);
        }

static int poly_malloc(int n)
        {
        xx=(double *)muste_malloc(n*(n+1)*sizeof(double));
        if (xx==NULL) { not_mem_for_polytope(); return(-1); }
        y11=(double *)muste_malloc((n+1)*sizeof(double));
        if (y11==NULL) { not_mem_for_polytope(); return(-1); }
        xc=(double *)muste_malloc(n*sizeof(double));
        if (xc==NULL) { not_mem_for_polytope(); return(-1); }
        x0=(double *)muste_malloc(n*sizeof(double));
        if (x0==NULL) { not_mem_for_polytope(); return(-1); }
        x00=(double *)muste_malloc(n*sizeof(double));
        if (x00==NULL) { not_mem_for_polytope(); return(-1); }
        space_allocated=1;
        return(1);
        }

static int not_mem_for_polytope()
        {
        sur_print("\nNot enough memory (polytope algorithm)");
        WAIT; return(1);
        }

/*  powell.c 28.6.2005/SM

*/

#define ITMAX 200
// extern int totnf;
// extern char sbuf[];

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
      sprintf(sbuf,"\n%d %g",iter,*fret); sur_print(sbuf);
      if (iter > ITMAX)
        {
        sur_print("No convergence!"); WAIT; return(1);
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
//  if (pcom==NULL) { not_mem(); exit(0); }
    xicom=(double *)muste_malloc(n*sizeof(double));
//  if (xicom==NULL) { not_mem(); exit(0); }
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
//  if (xt==NULL) { not_mem(); exit(0); }
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

/*  steep.c 4.7.2005/SM

*/

// #define ITMAX 200
#define EPS 1e-10

static int  frprmn(double *p, int n, double ftol, double *fret,
            double (*func)(double *), void (*dfunc)(double *,double *))
    {
    void linmin(double *p,double *xi,int n,double *fret,
            double (*func)(double *));
    int j;
    double gg,gam,fp,dgg;
    double *g,*h,*xi;
    int iter,stop;

    g=(double *)muste_malloc(n*sizeof(double));
//  if (g==NULL) { not_mem(); exit(0); }
    h=(double *)muste_malloc(n*sizeof(double));
//  if (h==NULL) { not_mem(); exit(0); }
    xi=(double *)muste_malloc(n*sizeof(double));
//  if (xi==NULL) { not_mem(); exit(0); }

    fp=(*func)(p);
    (*dfunc)(p,xi);
    for (j=0; j<n; ++j)
      {
      g[j]=-xi[j];

      xi[j]=h[j]=g[j];
      }
// printf("B");
    iter=0; totnf=2;
    while (1)
      {
      ++iter;
      linmin(p,xi,n,fret,func);
      stop=0;
      if (sur_kbhit())
          {
          j=sur_getch();
          if (j=='.') stop=1;
          }

      if (stop==1 || 2.0*fabs(fp-(*fret)) <= ftol*(fabs(fp)+fabs(*fret)+EPS))
        {
        muste_free(xi); muste_free(h); muste_free(g);
        return(totnf);
        }
      sprintf(sbuf,"\n%d %g",iter,*fret); sur_print(sbuf);
      if (iter > ITMAX)
        {
        sur_print("No convergence!"); WAIT; return(1);
        }
      fp=(*func)(p);
      (*dfunc)(p,xi); totnf+=2;
      dgg=gg=0.0;
      for (j=0; j<n; ++j)
        {
        gg+=g[j]*g[j];
        // dgg+=xi[j]*xi[j];        Fletcher-Reeves
        dgg+=(xi[j]+g[j])*xi[j]; // Polak-Ribiere
        }
      if (gg==0.0) { muste_free(xi); muste_free(h); muste_free(g); return(totnf); }
      gam=dgg/gg;
      for (j=0; j<n; ++j)
        {
        g[j]=-xi[j];
        xi[j]=h[j]=g[j]+gam*h[j];
        }
      } // while
    return(totnf);
    }

