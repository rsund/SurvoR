#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "survo.h"
#include "survolib.h"
#include "survoext.h"

#define SIGN(a,b) ((b)>=0.0 ? fabs(a) : -fabs(a))
#define TINY 1e-20;
#define N 50
#define SWAP(a,b) { temp=(a); (a)=(b); (b)=temp; }

static int *indxc,*indxr,*ipiv;
static double *D,*V;
static double *e,*v,*w,*u,*p,*pp,*px,*q;
static int *indx;
static char *trp;
static unsigned long *d,*s;
static int row,col;
// RS REM double l_luku=0.0;  /* sis_tuloa varten */

/* lin_dep.c 25.10.1986/SM (18.7.2005)
*/
static int lin_dep(int i)
        {
        char s[80];

        sprintf(s,"\nColumn # %d linearly dependent on previous columns!",i+1);
        sur_print(s); WAIT;
        return(1);
        }


static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(-1);
        }


double sis_tulo(double *a, double *b, int sa, int sb, int n)
{
    int i;
    double c = 0.0;
    for (i = 0; i < n; i++) c += a[i*sa] * b[i*sb];
    return c;
}

/* ortholin.c 25.10.1986/SM (8.3.1992)
*/

int ortholin1(double *a,int n,int m,double *b,int k,double eps,double *x,int improvement)
// int improvement;  /* 1=yes 0=no */
        {
        int g,h,i,j,l,ll,mm;
        double s,t;

        if (!improvement) u=a;
        else
            {
            u=(double *)malloc((unsigned int)m*n*sizeof(double));
            if (u==NULL) { not_enough_memory(); return(-1); }
            for (i=0; i<n; ++i)
                for (j=0; j<m; ++j)
                    {
                    double xxx;
                    xxx=a[i+n*j];
                    u[i+n*j]=xxx;
                    }
            }
        p=(double *)malloc(n*sizeof(double));
        if (p==NULL) { not_enough_memory(); return(-1); }
        px=(double *)malloc(m*sizeof(double));
        if (px==NULL) { not_enough_memory(); return(-1); }
        q=(double *)malloc(m*(m+1)/2*sizeof(double));
        if (q==NULL) { not_enough_memory(); return(-1); }

        l=-1;
        for (i=0; i<m; ++i)
            {
            s=0.0;
            for (j=0; j<n; ++j)
                {
                p[j]=u[j+n*i]; t=p[j]; s+=t*t;
                }
            if (s<eps) { lin_dep(i); return(-1); }
            q[++l]=s;
            for (g=0; g<k; ++g)
                {
                t=0.0;
                for (j=0; j<n; ++j)
                    t+=p[j]*b[j+n*g];
                x[i+m*g]=t;
                }
            for (g=i+1; g<m; ++g)
                {
                t=0.0;
                for (j=0; j<n; ++j)
                    t+=p[j]*u[j+n*g];
                q[++l]=t; t/=s;
                for (j=0; j<n; ++j)
                    u[j+n*g]-=p[j]*t;
                }
            }
        ll=l; mm=m+2;
        for (i=m-1; i>=0; --i)
            {
            h=l-i; t=q[l];
            for (j=0; j<k; ++j)
                {
                s=x[i+m*j];
                for (g=i+1; g<m; ++g)
                    s-=q[g+h]*x[g+m*j];
                x[i+m*j]=s=s/t;
                if (j==0) px[i]=s;
                }
            l+=i+1-mm;
            }

        /* iterative improvement */
        if (improvement)
            {
            double eps2,s0,s1,s2;

            pp=(double *)malloc(m*sizeof(double));
            if (pp==NULL) { not_enough_memory(); return(-1); }

            eps2=eps*eps;
            for (i=0; i<k; ++i)
                {
                s0=0.0;
                while (1)    /* iteration */
                    {
                    l=ll; s1=s2=0.0;
                    for (j=0; j<n; ++j)
                        {
                        p[j]=b[j+n*i]-sis_tulo(a+j,px,n,1,m);

/*                      double sc;
                        sc=-b[j+n*i];
                        for (g=0; g<m; ++g)
                            sc+=a[j+n*g]*px[g];
                        p[j]=-sc;
*/
                        }

                    for (j=0; j<m; ++j)
                        {
                        s=0.0;
                        for (g=0; g<n; ++g)
                            s+=u[g+n*j]*p[g];
                        pp[j]=s;
                        }

                    for (j=m-1; j>=0; --j)
                        {
                        h=l-j; s=pp[j];
                        for (g=j+1; g<m; ++g)
                            s-=q[g+h]*pp[g];
                        pp[j]=s=s/q[l]; s1+=s*s;
                        t=px[j]; s2+=t*t;
                        px[j]=s+t; l+=j+1-mm;
                        }
                    if (s1>s2*0.25)
                        {
                        printf("\nIterative improvement is ineffective!");
                        WAIT; return(1);
                        }
                    if ( s1>=s2*eps2 && (s0==0.0 || s1<=s0*0.01) )
                        { s0=s1; continue; }

                    g=i+1;
                    for (j=0; j<m; ++j)
                        {
                        double xxx;
                        xxx=px[j];
                        x[j+m*i]=xxx;
                        if (i!=k-1) px[j]=x[j+m*g];
                        }
                    break;
                    } /* while */

                } /* i */

            free(pp); free(u);
            }
        free(p); free(px); free(q);
        return(1);
        }


/*  lanczos.c 14.7.2005/SM (14.7.2005)
*/
static int mat_mlt_Hilbert(double *y,double *x,int n)  // y=H*x
    {
    int i,j;
    double s;

    for (j=0; j<n; ++j)
        {
        s=0.0;
        for (i=0; i<n; ++i)
            s+=x[i]/(double)(i+j+1);
        y[j]=s;
        }
    return(1);
    }

/*********************
Input: aa n*n, n, jmax
space allocated for alfa jmax+1, beta jmax+1
*********************/
int mat_lanczos(double *aa,double *alfa,double *beta,int n,int j,int jmax,double *ww)
        {
        int i;
// RS REM        int b0;
        double t,s;

// printf("\naa:");
// mprint(aa,n,n);
        if (j==-1)
            {
            free(u); free(w); free(v); return(1);
            }
        if (j==0)
            {
            v=(double *)malloc((n+1)*sizeof(double));
            w=(double *)malloc((n+1)*sizeof(double));
            u=(double *)malloc((n+1)*sizeof(double));

            for (i=0; i<n+1; ++i) v[i]=w[i]=0.0;
            beta[0]=1.0; j=0;

            t=1.0/sqrt((double)n);
            for (i=1; i<=n; ++i) w[i]=ww[i-1]=t;

            }
        while (beta[j]!=0.0 && j<jmax)
//      while (beta[j]>1e-12 && j<jmax)
            {
// printf("\nj=%d beta=%g",j,beta[j]); getch();
//          if (beta[j]<1e-12) beta[j]=1e-5; // kokeilu 25.7.2005
            if (j!=0)
                {
                for (i=1; i<=n; ++i)
                    {
                    t=w[i]; w[i]=ww[i-1+n*j]=v[i]/beta[j]; v[i]=-t*beta[j];
                    }
                }
            if (aa==NULL)
                {
                sur_print("(");
                mat_mlt_Hilbert(u+1,w+1,n);
                sur_print(")");
                }
            else
                mat_mlt(u+1,aa,w+1,n,n,1);
            for (i=1; i<=n; ++i) v[i]+=u[i];
            ++j;
            t=0.0; for (i=1; i<=n; ++i) t+=w[i]*v[i]; alfa[j]=t;
// printf("\nj=%d alfa=%g",j,alfa[j]);
            s=0.0;
            for (i=1; i<=n; ++i)
                {
                v[i]-=t*w[i];
                s+=v[i]*v[i];
                }
            beta[j]=sqrt(s);
            }
        return(j);
        }

/* mat_tqlb 6.7.2005/SM
*/

static double pythag(double a,double b)
    {
    double absa,absb;

    absa=fabs(a); absb=fabs(b);
    if (absa>absb) return( absa*sqrt(1.0+absb/absa*absb/absa));
    else return(absb==0.0 ? 0.0 : absb*sqrt(1.0+absa/absb*absa/absb));
    }

int mat_tqlb(double *d, double *e, int n, double *z)
    {
// RS REM    double pythag(double a,double b);
    int m,l,iter,i,k;
    double s,r,p,g,f,dd,c,b;

    for (i=1; i<n; ++i) e[i-1]=e[i];
    e[n-1]=0.0;
    for (l=0; l<n; ++l)
      {
      iter=0;
      do
        {
        for (m=l; m<n-1; ++m)
          {
          dd=fabs(d[m])+fabs(d[m+1]);
          if (fabs(e[m])+dd==dd) break;
          }
        if (m!=l)
          {
          if (iter++ == 30)
              {
              sur_print("\nNo convergence!"); WAIT; // RS CHA getch() -> WAIT;
              return(-1);
              }
          g=(d[l+1]-d[l])/(2.0*e[l]);
          r=pythag(g,1.0);
          g=d[m]-d[l]+e[l]/(g+SIGN(r,g));
          s=c=1.0;
          p=0.0;
          for (i=m-1; i>=l; --i)
              {
              f=s*e[i];
              b=c*e[i];
              e[i+1]=(r=pythag(f,g));
              if (r==0.0)
                {
                d[i+1]-=p;
                e[m]=0.0;
                break;
                }
              s=f/r;
              c=g/r;
              g=d[i+1]-p;
              r=(d[i]-g)*s+2.0*c*b;
              d[i+1]=g+(p=s*r);
              g=c*r-b;
            if (z!=NULL) // 15.7.2005  ei ominaisvektoreita, jos z=NULL
              for (k=0; k<n; ++k)
                {
                f=z[k+n*(i+1)];
                z[k+n*(i+1)]=s*z[k+n*i]+c*f;
                z[k+n*i]=c*z[k+n*i]-s*f;
                }
              }
          if (r==0.0 && i>=1) continue;
          d[l]-=p;
          e[l]=g;
          e[m]=0.0;
          }


        } while(m!=l);


      } // l


    for (i=0; i<n; ++i)
        {
        int j;

        k=i; p=d[i];
        for (j=i+1; j<n; ++j)
            if (d[j]>p)
                {
                k=j; p=d[j];
                }
        if (k!=i)
            {
            d[k]=d[i]; d[i]=p;
          if (z!=NULL)
            for (j=0; j<n; ++j)
                {
                double xxx;

                p=z[j+n*i]; xxx=z[j+n*k]; z[j+n*i]=xxx; z[j+n*k]=p;
                }
            }
        }

    return(1);
    }

/*  solve_lu.c 13.7.2005/SM
*/

static int ludcmp(double *a,int n,int *indx,double *d)
    {
    int i,imax=0,j,k;
    double big,dum,sum,temp;
    double *vv;

    vv=malloc(n*sizeof(double));
    *d=1.0;
    for (i=0; i<n; ++i)
      {
      big=0.0;
      for (j=0; j<n; ++j)
        if ((temp=fabs(a[i+n*j]))>big) big=temp;
      if (big==0.0) return(-1);
      vv[i]=1.0/big;
      }
    for (j=0; j<n; ++j)
      {
      for (i=0; i<j; ++i)
        {
        sum=a[i+n*j];
        for (k=0; k<i; ++k) sum-=a[i+n*k]*a[k+n*j];
        a[i+n*j]=sum;
        }
      big=0.0;
      for (i=j; i<n; ++i)
        {
        sum=a[i+n*j];for (k=0; k<j; ++k) sum-=a[i+n*k]*a[k+n*j];
        a[i+n*j]=sum;
        if ((dum=vv[i]*fabs(sum)) >= big) { big=dum; imax=i; }
        }
      if (j!=imax)
        {
        for (k=0; k<n; ++k)
          {
          dum=a[imax+n*k];
          a[imax+n*k]=a[j+n*k];
          a[j+n*k]=dum;
          }
        indx[j]= imax;
        *d=-(*d);
        vv[imax]=vv[j];
        }
      indx[j]=imax;
      if (a[j+n*j]==0.0) a[j+n*j]=TINY;
      if (j!=n-1)
        {
        dum=1.0/a[j+n*j];
        for (i=j+1; i<n; ++i) a[i+n*j]*=dum;
        }
      } // j
    return(1);
    }

static int lubksb(double *a, int n, int *indx, double *b)
    {
    int i,ii=0,ip,j;
    double sum;

    for (i=0; i<n; ++i)
      {
      ip=indx[i];
      sum=b[ip];
      b[ip]=b[i];
      if (ii!=0)
        for (j=ii-1; j<i; ++j) sum-=a[i+n*j]*b[j];
      else if (sum!=0.0)
        ii=i+1;
      b[i]=sum;
      }
    for (i=n-1; i>=0; --i)
      {
      sum=b[i];
      for (j=i+1; j<n; ++j) sum-=a[i+n*j]*b[j];
      b[i]=sum/a[i+n*i];
      }
    return(1);
    }


// a*x=b: tulos x=b
void mat_solve_lu(double *a,double *b,int n)
    {
    double d;

    indx=malloc(n*sizeof(int));

    ludcmp(a,n,indx,&d);  // d vain mahd. determinantin laskentaan!
    lubksb(a,n,indx,b);
    }

/* mat_treb 6.7.2005/SM
*/

int mat_treb(double *a, int n, double *d, double *e)
    {
    int l,k,j,i;
    double scale,hh,h,g,f;

    for (i=n-1; i>0; --i)
      {
      l=i-1;
      h=scale=0.0;
      if (l>0)
        {
        for (k=0; k<l+1; ++k)
            scale+=fabs(a[i+n*k]);
        if (scale==0.0)
            e[i]=a[i+n*l];
        else
            {
            for (k=0; k<l+1; ++k)
              {
              a[i+n*k]/=scale;
              h+=a[i+n*k]*a[i+n*k];
              }
            f=a[i+n*l];
            g=(f>=0 ? -sqrt(h) : sqrt(h));
            e[i]=scale*g;
            h-=f*g;
            a[i+n*l]=f-g;
            f=0.0;
            for (j=0; j<l+1; ++j)
              {
              a[j+n*i]=a[i+n*j]/h;
              g=0.0;
              for (k=0; k<j+1; ++k)
                  g+=a[j+n*k]*a[i+n*k];
              for (k=j+1; k<l+1; ++k)
                  g+=a[k+n*j]*a[i+n*k];
              e[j]=g/h;
              f+=e[j]*a[i+n*j];
              }
            hh=f/(h+h);
            for (j=0; j<l+1; ++j)
              {
              f=a[i+n*j];
              e[j]=g=e[j]-hh*f;
              for (k=0; k<j+1; ++k)
                  a[j+n*k]-=(f*e[k]+g*a[i+n*k]);
              }
            }
        } // l
      else
          e[i]=a[i+n*l];
      d[i]=h;
      } // i
    d[0]=0.0;
    e[0]=0.0;
    for (i=0; i<n; ++i)
      {
      l=i;
      if (d[i]!=0.0)
        {
        for (j=0; j<l; ++j)
          {
          g=0.0;
          for (k=0; k<l; ++k)
              g+=a[i+n*k]*a[k+n*j];
          for (k=0; k<l; ++k)
              a[k+n*j]-=g*a[k+n*i];
          }
        }
      d[i]=a[i+n*i];
      a[i+n*i]=1.0;
      for (j=0; j<l; ++j)
          a[j+n*i]=a[i+n*j]=0.0;
      }
    return(1);
    }

/* mat_qrp 27.6.1999/SM (29.6.1999)
   Householder QR with column pivoting (Golub,van Loan 5.4.1)
*/


static int row_house(double *A,double *v,double *w,int m,int n,int r)
        {
        int i,j;
        double beta,s;

        beta=0.0;
        for (i=r; i<m; ++i) beta+=v[i]*v[i];

        beta=-2/beta;
        for (i=r; i<n; ++i)
            {
            s=0.0;
            for (j=r; j<m; ++j)
                {
                s+=A[j+m*i]*v[j];
                }
            w[i]=beta*s;
            }

        for (i=r; i<m; ++i)
            for (j=r; j<n; ++j)
                A[i+m*j]+=v[i]*w[j];
        return(1);
        }

static int house(double *x,double *v,int n)
        {
        int i; // ,sgn;
        double my,beta;
        double a;

        my=0.0; for (i=0; i<n; ++i) { a=x[i]; my+=a*a; v[i]=a; }
        my=sqrt(my);
        if (my!=0.0)
            {
            if (x[0]>0)
                beta=x[0]+my;
            else
                beta=x[0]-my;
            for (i=1; i<n; ++i) v[i]/=beta;
            }
        v[0]=1.0;
        return(1);
        }
/************************
mprint(A,m,n)
double *A;
int m,n;
        {
        int i,j;

        for (i=0; i<m; ++i)
            {
            printf("\n");
            for (j=0; j<n; ++j) printf("%g ",A[i+m*j]);
            }
        printf("\n"); getch();
        }
**************************/


int mat_qrp(double *A,double *Q,int *piv,int m,int n,double tol)  /* A overwritten by R */
        {
        int i,j,k,r;
        double s,tau;
        double *c; // RS CHA moved from local globals

        k=m; if (n>m) k=n;
        c=(double *)malloc(n*sizeof(double));
        if (c==NULL) { not_enough_memory(); return(-1); }
        v=(double *)malloc(k*sizeof(double));
        if (v==NULL) { not_enough_memory(); return(-1); }
        w=(double *)malloc(k*sizeof(double));
        if (w==NULL) { not_enough_memory(); return(-1); }

        for (j=0; j<n; ++j)
            {
            s=0.0;
            for (i=0; i<m; ++i)
                s+=A[i+m*j]*A[i+m*j];
            c[j]=s;
            }

        r=-1; tau=-1.0;
        for (i=0; i<n; ++i)
            {
/*
   printf("c[i]=%g tau=%g k=%d\n",c[i],tau,k); getch();
*/
            if (c[i]<=tau) continue;
            tau=c[i]; k=i;
            }
        while (tau>tol)
            {
            ++r;
            piv[r]=k;
            if (k!=r)
                {
                for (i=0; i<m; ++i)
                    {
                    s=A[i+m*r]; A[i+m*r]=A[i+m*k]; A[i+m*k]=s;
                    }
                s=c[r]; c[r]=c[k]; c[k]=s;
                }
            house(A+r*m+r,v+r,m-r);
            row_house(A,v,w,m,n,r);
            for (i=r+1; i<m; ++i) A[i+m*r]=v[i];
            for (i=r+1; i<n; ++i) { s=A[r+m*i]; c[i]-=s*s; }
            if (r<n-1)
                {
                tau=-1.0;
                for (i=r+1; i<n; ++i)
                    {
                    if (c[i]<=tau) continue;
                    tau=c[i]; k=i;
                    }
                }
            else tau=0;
            }
/*
printf("\nrank r+1=%d\npiv:",r+1);
for (i=0; i<r+1; ++i) printf(" %d",piv[i]); printf("\n"); getch();
mprint(A,m,n);
*/

        for (i=0; i<m; ++i)
            {
            for (j=0; j<m; ++j) Q[i+m*j]=0;
            Q[(m+1)*i]=1.0;
            }
        for (j=r; j>=0; --j)
            {
            v[j]=1.0;
            for (i=j+1; i<m; ++i) v[i]=A[i+m*j];
            row_house(Q,v,w,m,m,j);
            }

// printf("\nQ:");
// mprint(Q,m,m);

        for (j=0; j<n; ++j) for (i=j+1; i<m; ++i) A[i+m*j]=0.0;

        free(w); free(v); free(c);
        return(r+1);
        }

/* mat_rank.c 11.3.2003 (11.3.2003)
*/

int mat_svd_rank(double *X,int mX,int nX,double eps)
    {
    double tol,svd_eps;
    int i;

    // oltava mX>=nX!
    D=(double *)malloc(nX*sizeof(double));
    if (D==NULL) { not_enough_memory(); return(-1); }
    V=(double *)malloc(nX*nX*sizeof(double));
    if (V==NULL) { not_enough_memory(); return(-1); }
    tol=1e-16; svd_eps=(1e-300)/eps;
    mat_svd(X,D,V,mX,nX,svd_eps,tol);

    for (i=nX-1; i>=0; --i)
        if (D[i]>eps*D[0]) break; // 25.12.2003
    X[0]=(double)(i+1);

    free(V); free(D);
    return(1);
    }

/* mat_basis 10.3.2003/SM (10.3.2003)
*/

int mat_column_space(int *pn,double *X,int mX,int nX,double eps) // int *pn; // aste
    {
    double tol,svd_eps;
    int i;

    // oltava mX>=nX!
    D=(double *)malloc(nX*sizeof(double));
    if (D==NULL) { not_enough_memory(); return(-1); }
    V=(double *)malloc(nX*nX*sizeof(double));
    if (V==NULL) { not_enough_memory(); return(-1); }
    tol=1e-16; svd_eps=(1e-300)/eps;
    mat_svd(X,D,V,mX,nX,svd_eps,tol);

    for (i=nX-1; i>=0; --i)
        if (D[i]>eps*D[0]) break; // 25.12.2003
    *pn=i+1;

    free(V); free(D);
    return(i);
    }

/* mat_null 9.3.2003/SM (9.3.2003)
*/

int mat_null_space(int *pn,double *X,int mX,int nX,double eps) // int *pn; // ratkaisujen lkm.
    {
    double tol,svd_eps;
    int i,j;

    // oltava mX>=nX!
    D=(double *)malloc(nX*sizeof(double));
    if (D==NULL) { not_enough_memory(); return(-1); }
    V=(double *)malloc(nX*nX*sizeof(double));
    if (V==NULL) { not_enough_memory(); return(-1); }
    tol=1e-16; svd_eps=(1e-300)/eps;
    mat_svd(X,D,V,mX,nX,svd_eps,tol);

    for (i=nX-1; i>=0; --i)
        if (D[i]>eps*D[0]) break; // 25.12.2003
    *pn=nX-1-i;

    if (*pn==0)
        {
        *pn=1;
        for (i=0; i<mX; ++i) X[i]=0.0;
        return(1);
        }
    for (j=0; j<*pn; ++j)
        for (i=0; i<nX; ++i)
            X[i+j*nX]=V[i+(j+nX-*pn)*nX];
    free(V); free(D);
    return(1);
    }

/* mat_ginv.c 14.12.2003 (14.12.2003)
*/

int mat_mp_inv(double *Z,double *X,int m,int n,double eps)
    {
    double tol,svd_eps;
    int i,j;
    int transp=0;

    if (m<n)
        {
        transp=1;
        for (i=0; i<m; ++i) for (j=0; j<n; ++j)
            Z[j+n*i]=X[i+m*j];
        for (i=0; i<m*n; ++i) X[i]=Z[i];
        i=m; m=n; n=i;
        }

    D=(double *)malloc(m*n*sizeof(double)); // tÑhÑn myîs U'
    if (D==NULL) { not_enough_memory(); return(-1); }
    V=(double *)malloc(n*n*sizeof(double));
    if (V==NULL) { not_enough_memory(); return(-1); }

    svd_eps=1e-16; tol=1e-300/svd_eps;
    mat_svd(X,D,V,m,n,svd_eps,tol);

    if (eps<1e-15) eps=1e-15;
    eps*=D[0];
    for (i=0; i<n; ++i) if (D[i]>eps) D[i]=1/D[i]; else D[i]=0.0;
// MPINV(X)=V*dinv(D)*U'  eli tÑssÑ nyt V*dinv(D)*X'
    for (j=0; j<n; ++j)
        {
        tol=D[j];
        for (i=0; i<n; ++i) V[i+n*j]*=tol;
        }

    for (i=0; i<m; ++i) for (j=0; j<n; ++j)
        D[j+n*i]=X[i+m*j]; // U'

    mat_mlt(Z,V,D,n,n,m);

    if (transp)
        {
        i=m; m=n; n=i;
        for (i=0; i<m; ++i) for (j=0; j<n; ++j)
            D[j+n*i]=Z[i+m*j];
        for (i=0; i<m*n; ++i) Z[i]=D[i];
        }
    free(V); free(D);
    return(1);
    }

/* mat_intv 13.3.2003/SM (13.3.2003)
*/

int ds_ratio(double luku,unsigned long *pd,unsigned long *ps,int nkonv,unsigned long *an)
    {
    int i;
    int a[N];
    double b,eps;
    double d1,s1,d2,s2;

    i=0;  eps=1e-15;
    while(1)
        {
        a[i]=(int)luku;
        b=luku-(double)a[i];
        if (fabs(b)<eps) break;
        luku=1.0/b;
        if (i>=nkonv) break;
        ++i;
        eps*=5.0;
        }
    d1=a[i]; s1=1L;
    *an=d1;
    while (i>0)
        {
        d2=a[i-1]*d1+s1;
        s2=d1;
        d1=d2;
        s1=s2;
        --i;
        }
    *pd=d1; *ps=s1;
    return(1);
    }

static unsigned long intv_gcd(unsigned long u,unsigned long v)  /* Greatest Common Divisor */
        {
        unsigned long w;

        if (u<v) { w=u; u=v; v=w; }
        while ((w=u%v)!=0)
            {
            u=v; v=w;
            }
        return(v);
        }

static unsigned long lcm_n(unsigned long *s,int n)
    {
    int i;
    unsigned long lcm;

    lcm=s[0];
    if (n==1) return(lcm);
    for (i=1; i<n; ++i)
        {
        lcm=lcm*s[i]/intv_gcd(lcm,s[i]);
        }
    return(lcm);
    }

int mat_intval(double *aa,int m,double feps,int nkonv)
    {
    int i,k;
    double max,b;
    unsigned long lcm;
    unsigned long an;

    d=(unsigned long *)malloc(m*sizeof(long));
    s=(unsigned long *)malloc(m*sizeof(long));

    max=-1e100;
    for (i=0; i<m; ++i)
        {
        b=fabs(aa[i]);
        if (b<feps) aa[i]=b=0.0;
        if (b>max) max=b;
        }

    for (i=0; i<m; ++i)
        {
        aa[i]/=max;
        if (aa[i]==1.0) { d[i]=1L; s[i]=1L; }
        else if (aa[i]==0.0) { d[i]=0L; s[i]=1L; }
        else k=ds_ratio(fabs(aa[i]),&d[i],&s[i],nkonv,&an);
// printf("\n%g %d/%d",aa[i],d[i],s[i]); getch();

        }

    lcm=lcm_n(s,m);
    for (i=0; i<m; ++i)
        {
        k=1; if (aa[i]<0) k=-1;
        aa[i]=k*(double)lcm/(double)s[i]*(double)d[i];
        }
    free(s); free(d);
    return(1);
    }

/* mat_fr_d.c 18.3.2003/SM (18.3.2003)
   FRAC_TO_DEC
*/

int mat_frac_to_dec(double *a,int m,double *pb)
    {
    int i;
    double d1,s1,d2,s2;

    i=m-1;
    d1=a[i]; s1=1.0;
    while (i>0)
        {
        d2=a[i-1]*d1+s1;
        s2=d1;
        d1=d2;
        s1=s2;
        --i;
        }
    *pb=d1/s1;
    return(1);
    }

/* mat_ldet 4.12.1998/SM (16.2.2000)
   based on pivotal operation
*/

int mat_logdet(double *X,int m,double *pdet)
        {
        int i,j,k;
        double a,b;

        *pdet=0.0;
        for (k=0; k<m; ++k)
            {
            a=X[k*(m+1)];
            if (a<=0.0) return(-1);
            *pdet+=log(a);
            if (k==m-1) return(1);
            a=1/a;
            X[k*(m+1)]=a;
            for (j=0; j<m; ++j)
                {
                if (j==k) continue;
                X[k+m*j]*=a;
                }
            for (i=0; i<m; ++i)
                {
                if (i==k) continue;
                b=X[i+m*k];
                for (j=0; j<m; ++j)
                    {
                    if (j==k) continue;
                    X[i+m*j]-=b*X[k+m*j];
                    }
                X[i+m*k]*=-a;
                }
            }
        return(1);
        }

/* mat_hom 9.3.2003/SM (9.3.2003)
*/

int mat_solve_homogeneous(int *pn,double *X,int mX,int nX,double eps) // int *pn; // ratkaisujen lkm.
    {
    double tol,svd_eps;
    int i,j;

    // oltava mX>=nX!
    D=(double *)malloc(nX*sizeof(double));
    if (D==NULL) { not_enough_memory(); return(-1); }
    V=(double *)malloc(nX*nX*sizeof(double));
    if (V==NULL) { not_enough_memory(); return(-1); }
    tol=1e-16; svd_eps=(1e-300)/eps;
    mat_svd(X,D,V,mX,nX,svd_eps,tol);

    for (i=nX-1; i>=0; --i)
        if (D[i]>eps) break;
    *pn=nX-1-i;

    if (*pn==0)
        {
        *pn=1;
        for (i=0; i<mX; ++i) X[i]=0.0;
        return(1);
        }
    for (j=0; j<*pn; ++j)
        for (i=0; i<nX; ++i)
            X[i+j*nX]=V[i+(j+nX-*pn)*nX];
    free(V); free(D);
    return(1);
    }

/* mat_qr 29.6.1999/SM (29.6.1999)
   Householder QR (Golub,van Loan 5.2.1)
*/

int mat_qr(double *A,double *Q,int m,int n,double tol)  /* A overwritten by R */
// double tol; /* toistaiseksi ei kÑytîssÑ */
        {

        int i,j;
// RS REM        double s;

        v=(double *)malloc(m*sizeof(double));
        if (v==NULL) { not_enough_memory(); return(-1); }
        w=(double *)malloc(m*sizeof(double));
        if (w==NULL) { not_enough_memory(); return(-1); }

        for (j=0; j<n; ++j)
            {
            house(A+j*m+j,v+j,m-j);
            row_house(A,v,w,m,n,j);
            if (j<m)
                for (i=j+1; i<m; ++i) A[i+m*j]=v[i];
            }
        for (i=0; i<m; ++i)
            {
            for (j=0; j<m; ++j) Q[i+m*j]=0;
            Q[(m+1)*i]=1.0;
            }
        for (j=n-1; j>=0; --j)
            {
            v[j]=1.0;
            for (i=j+1; i<m; ++i) v[i]=A[i+m*j];
            row_house(Q,v,w,m,m,j);
            }
        for (j=0; j<n; ++j) for (i=j+1; i<m; ++i) A[i+m*j]=0.0;
        free(w); free(v);
        return(1);
        }

int mat_transp_in_situ(double *aa,int m,int n)
        {
        int i,j,k,h;
        double x,y;

        trp=(char *)malloc(m*n);
        if (trp==NULL)
       { sur_print("Not enough space (mat_transp_in_situ)!\n"); return(-1); }
        for (i=0; i<m*n; ++i) trp[i]='1';
        trp[m*n-1]='2';
        h=1;
        while (h<m*n)
            {
            if (trp[h]=='2') { ++h; continue; }
            k=h;
            y=aa[k];
            while (1)
                {
                trp[k]='2';
                x=y;
                i=k%m; j=(k-i)/m;
                k=j+n*i;
                if (k==h) { aa[h]=x; break; }
                y=aa[k]; aa[k]=x;
                }
            ++h;
            }
        free(trp);
        return(1);
        }

/* mat_mtm 25.10.1986/SM (25.10.1986)
*/

int mat_mtm(double *T,double *X,int m,int n)
        {
        int i,j;
//        extern double sis_tulo();
        double a;

        for (i=0; i<n; ++i) for (j=0; j<=i; ++j)
            {
            a=sis_tulo(X+m*i,X+m*j,1,1,m);
            T[i+n*j]=T[j+n*i]=a;

/*          ilman sis_tuloa
            a=0;
            for (k=0; k<m; ++k) a+=X[k+m*i]*X[k+m*j];
            T[i+n*j]=a; T[j+n*i]=a;
*/
            }
        return(1);
        }
/*
mat_mtm(T,X,m,n)
double *T;
double *X;
int m;
int n;
        {
        int i,j,k;

        for (i=0; i<n*n; ++i) T[i]=0.0;
        for (i=0; i<n; ++i)
            for (k=0; k<m; ++k)
                for (j=0; j<=i; ++j)
                    T[i+n*j]+=X[k+m*i]*X[k+m*j];
        for (i=0; i<n; ++i)
            for (j=0; j<i; ++j)
                T[j+n*i]=T[i+n*j];
        return(1);
        }
*/

/* mat_mmt 25.10.1986/SM (25.10.1986)
*/
/* vanha
mat_mmt(T,X,m,n)
double *T;
double *X;
int m;
int n;
        {
        int i,j,k;
        extern double sis_tulo();
        double a;

        for (i=0; i<m; ++i) for (j=0; j<=i; ++j)
            {
            a=sis_tulo(X+i,X+j,m,m,n);
            T[i+m*j]=T[j+m*i]=a;


            ilman sis_tuloa
            a=0;
            for (k=0; k<n; ++k) a+=X[i+m*k]*X[j+m*k];
            T[i+m*j]=a; T[j+m*i]=a;

            }
        return(1);
        }
*/
/* ei kÑytîssÑ 2.6.1999- matriisitulkissa */
int mat_mmt(double *T,double *X,int m,int n)
        {
        int i;

        i=mat_transp_in_situ(X,m,n); if (i<0) return(-1);
        mat_mtm(T,X,n,m);
        i=mat_transp_in_situ(X,n,m); if (i<0) return(-1);
        return(1);
        }

/* mat_tred 25.10.1986/SM (25.10.1986)
*/

int mat_tred2(double *d,double *e,double *a,int n,double tol)
        {
        double *z;
        int i,j,k,l;
        double f,g,h,hh;

        z=a;  /* transformation matrix written on original matrix a */

        for (i=n-1; i>=1; --i)
            {
            l=i-2; f=z[i+n*(i-1)]; g=0.0;
            for (k=0; k<=l; ++k)
                g+=z[i+n*k]*z[i+n*k];
            h=g+f*f;

            if (g<=tol)
                { e[i]=f; h=0.0; }
            else
                {
                double apu;

                ++l;
                if (f>=0) apu=-sqrt(h); else apu=sqrt(h);
                e[i]=g=apu;
                h-=f*g; z[i+n*(i-1)]=f-g; f=0.0;
                for (j=0; j<=l; ++j)
                    {
                    z[j+n*i]=z[i+n*j]/h; g=0.0;
                    for (k=0; k<=j; ++k)
                        g+=z[j+n*k]*z[i+n*k];
                    for (k=j+1; k<=l; ++k)
                        g+=z[k+n*j]*z[i+n*k];
                    e[j]=g/h; f+=g*z[j+n*i];
                    }
                hh=f/(h+h);
                for (j=0; j<=l; ++j)
                    {
                    f=z[i+n*j]; e[j]=g=e[j]-hh*f;
                    for (k=0; k<=j; ++k)
                        z[j+n*k]-=f*e[k]+g*z[i+n*k];
                    }
                }
/* skip */  d[i]=h;
            }  /* i */

        d[0]=0.0; e[0]=0.0;
        for (i=0; i<n; ++i)
            {
            l=i-1;
            if (d[i]!=0.0)
                for (j=0; j<=l; ++j)
                    {
                    g=0.0;
                    for (k=0; k<=l; ++k)
                        g+=z[i+n*k]*z[k+n*j];
                    for (k=0; k<=l; ++k)
                        z[k+n*j]-=g*z[k+n*i];
                    }
            d[i]=z[i+n*i]; z[i+n*i]=1.0;
            for (j=0; j<=l; ++j) { z[i+n*j]=0.0; z[j+n*i]=0.0; }
            }
        return(1);
        }

/* mat_tql2 25.10.1986/SM (25.10.1986)
*/

int mat_tql2(double *d,double *e,double *z,int n,double eps,int maxiter)
        {
        int i,j,k,l,m;
        double b,c,f,g,h,p,r,s;
        double apu;

        for (i=1; i<n; ++i) e[i-1]=e[i];
        e[n-1]=0.0; b=f=0.0;
        for (l=0; l<n; ++l)
            {
            j=0; /* iter.laskuri */
            h=eps*(fabs(d[l])+fabs(e[l]));
            if (b<h) b=h;
            for (m=l; m<n; ++m)
                if (fabs(e[m])<=b) break;
            if (m!=l)
                {
                while (1)
                    {
                    if (j==maxiter)
                        {
                        return(-1);
                        }
                    ++j;
                    g=d[l]; p=(d[l+1]-g)/(2*e[l]);
                    r=sqrt(p*p+1.0);
                    if (p<0.0) apu=p-r; else apu=p+r;
                    d[l]=e[l]/apu; h=g-d[l];
                    for (i=l+1; i<n; ++i) d[i]-=h;
                    f+=h;

                    p=d[m]; c=1.0; s=0.0;
                    for (i=m-1; i>=l; --i)
                        {
                        g=c*e[i]; h=c*p;
                        if (fabs(p)>=fabs(e[i]))
                            {
                            c=e[i]/p; r=sqrt(c*c+1.0);
                            e[i+1]=s*p*r; s=c/r; c=1.0/r;
                            }
                        else
                            {
                            c=p/e[i]; r=sqrt(c*c+1.0);
                            e[i+1]=s*e[i]*r; s=1.0/r; c=c/r;
                            }
                        p=c*d[i]-s*g;
                        d[i+1]=h+s*(c*g+s*d[i]);

                        for (k=0; k<n; ++k)
                            {
                            h=z[k+n*(i+1)];
                            z[k+n*(i+1)]=s*z[k+n*i]+c*h;
                            z[k+n*i]=c*z[k+n*i]-s*h;
                            }
                        } /* i */
                    e[l]=s*p; d[l]=c*p;
                    if (fabs(e[l])<=b) break;
                    } /* while */
                }
/* root */  d[l]=d[l]+f;
            } /* l */

        for (i=0; i<n; ++i)
            {
            k=i; p=d[i];
            for (j=i+1; j<n; ++j)
                if (d[j]>p)
                    {
                    k=j; p=d[j];
                    }
            if (k!=i)
                {
                d[k]=d[i]; d[i]=p;
                for (j=0; j<n; ++j)
                    {
                    double xxx;

                    p=z[j+n*i]; xxx=z[j+n*k]; z[j+n*i]=xxx; z[j+n*k]=p;
                    }
                }
            }
        return(1);
        }

/* mat_svd 25.10.1986/SM (25.10.1986)
*/

int mat_svd(double *u,double *q,double *v,int m,int n,double eps,double tol)
        {
        int i,j,k,l=0,l1;
        double c,f,g,h,s,x,y,z;
/*      double e[100];   */
        double apu;

        e=(double *)malloc(n*sizeof(double));
        if (e==NULL) { not_enough_memory(); exit(1); }

/* Householder's reduction to bidiagonal form */
        g=x=0;
        for (i=0; i<n; ++i)
            {
            e[i]=g; s=0; l=i+1;
            for (j=i; j<m; ++j) s+=u[j+m*i]*u[j+m*i];
            if (s<tol) g=0;
            else
                {
                f=u[i*(m+1)];
                if (f<0) g=sqrt(s); else g=-sqrt(s);
                h=f*g-s; u[i*(m+1)]=f-g;
                for (j=l; j<n; ++j)
                    {
                    s=0;
                    for (k=i; k<m; ++k) s+=u[k+m*i]*u[k+m*j];
                    f=s/h;
                    for (k=i; k<m; ++k) u[k+m*j]+=f*u[k+m*i];
                    }
                }
            q[i]=g; s=0;
            for (j=l; j<n; ++j) s+=u[i+m*j]*u[i+m*j];
            if (s<tol) g=0;
            else
                {
                f=u[i+m*(i+1)];
                if (f<0) g=sqrt(s); else g=-sqrt(s);
                h=f*g-s; u[i+m*(i+1)]=f-g;
                for (j=l; j<n; ++j) e[j]=u[i+m*j]/h;
                for (j=l; j<m; ++j)
                    {
                    s=0;
                    for (k=l; k<n; ++k) s+=u[j+m*k]*u[i+m*k];
                    for (k=l; k<n; ++k) u[j+m*k]+=s*e[k];
                    }
                }
            y=fabs(q[i])+fabs(e[i]); if (y>x) x=y;
            }

/* Accumulation of right-hand transformations */
        for (i=n-1; i>=0; --i)
           {
            if (g)
                {
                h=u[i+m*(i+1)]*g;
                for (j=l; j<n; ++j) v[j+n*i]=u[i+m*j]/h;
                for (j=l; j<n; ++j)
                    {
                    s=0;
                    for (k=l; k<n; ++k) s+=u[i+m*k]*v[k+n*j];
                    for (k=l; k<n; ++k) v[k+n*j]+=s*v[k+n*i];
                    }
                }
            for (j=l; j<n; ++j) v[i+n*j]=v[j+n*i]=0;
            v[i*(n+1)]=1; g=e[i]; l=i;
            }

/* Accumulation of left-hand transformations */
        for (i=n-1; i>=0; --i)
            {
            l=i+1; g=q[i];
            for (j=l; j<n; ++j) u[i+m*j]=0;
            if (g)
                {
                h=u[i*(m+1)]*g;
                for (j=l; j<n; ++j)
                    {
                    s=0;
                    for (k=l; k<m; ++k) s+=u[k+m*i]*u[k+m*j];
                    f=s/h;
                    for (k=i; k<m; ++k) u[k+m*j]+=f*u[k+m*i];
                    }
                for (j=i; j<m; ++j) u[j+m*i]/=g;
                }
            else for (j=i; j<m; ++j) u[j+m*i]=0;
            ++u[i*(m+1)];
            }

/* Diagonalization of the bidiagonal form */
        eps*=x;
        for (k=n-1; k>=0; --k)
            {
test_f_splitting:
            for (l=k; l>=0; --l)
                {
                if (fabs(e[l])<=eps) goto test_f_convergence;
                if (fabs(q[l-1])<=eps) goto cancellation;
                }

/* cancellation of e[l] if l>1 */
cancellation:
            c=0; s=1; l1=l-1;
            for (i=l; i<=k; ++i)
                {
                f=s*e[i]; e[i]*=c;
                if (fabs(f)<=eps) goto test_f_convergence;
                g=q[i]; q[i]=h=sqrt(f*f+g*g); c=g/h; s=-f/h;
                for (j=0; j<m; ++j)
                    {
                    y=u[j+m*l1]; z=u[j+m*i];
                    u[j+m*l1]=y*c+z*s; u[j+m*i]=-y*s+z*c;
                    }
                }
test_f_convergence:
            z=q[k]; if (l==k) goto convergence;

/* Shift from bottom 2*2 minor */
            x=q[l]; y=q[k-1]; g=e[k-1]; h=e[k];
            f=((y-z)*(y+z)+(g-h)*(g+h))/(2*h*y); g=sqrt(f*f+1);
            if (f<0) apu=f-g; else apu=f+g;
            f=((x-z)*(x+z)+h*(y/apu-h))/x;

/* Next QR transformation */
            c=s=1;
            for (i=l+1; i<=k; ++i)
                {
                g=e[i]; y=q[i]; h=s*g; g*=c;
                e[i-1]=z=sqrt(f*f+h*h); c=f/z; s=h/z;
                f=x*c+g*s; g=-x*s+g*c; h=y*s; y*=c;
                for (j=0; j<n; ++j)
                    {
                    x=v[j+n*(i-1)]; z=v[j+n*i];
                    v[j+n*(i-1)]=x*c+z*s; v[j+n*i]=-x*s+z*c;
                    }
                q[i-1]=z=sqrt(f*f+h*h);
                if (z) { c=f/z; s=h/z; } else { c=1.0; s=0.0; } /* 4.10.85/SM*/
                f=c*g+s*y; x=-s*g+c*y;
                for (j=0; j<m; ++j)
                    {
                    y=u[j+m*(i-1)]; z=u[j+m*i];
                    u[j+m*(i-1)]=y*c+z*s; u[j+m*i]=-y*s+z*c;
                    }
                }
            e[l]=0; e[k]=f; q[k]=x; goto test_f_splitting;

convergence:
            if (z<0)
                {
                q[k]=-z;
                for (j=0; j<n; ++j) v[j+n*k]=-v[j+n*k];
                }
            } /* k */

/* Sorting */
        for (i=0; i<n; ++i)
            {
            k=i; x=q[i];
            for (j=i+1; j<n; ++j)
                if (q[j]>x) { k=j; x=q[j]; }
            if (k>i)
             {
             q[k]=q[i]; q[i]=x;
             for (j=0; j<m; ++j)
                { x=u[j+m*i]; y=u[j+m*k]; u[j+m*k]=x; u[j+m*i]=y; }
             for (j=0; j<n; ++j)
                { x=v[j+n*i]; y=v[j+n*k]; v[j+n*k]=x; v[j+n*i]=y; }
             }
            }
        return(1);
        }

/* mat_chol 25.10.1986/SM (23.3.1993) (9.6.1995)
*/

static void c_saxpy(double a,double *x,double *y,int n)  /* y+=a*x */
        {
        int i;

        for (i=0; i<n; ++i) y[i]+=a*x[i];
        }

int mat_chol2(double *G,double *A,int n,double eps)
        {
        int i,j,k,i1;
        double s;

        v=(double *)malloc(n*sizeof(double));
        if (v==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<n*n; ++i) G[i]=0.0;
        for (j=0; j<n; ++j)
            {
            for (i=j; i<n; ++i) v[i]=A[i+n*j];
            for (k=0; k<=j-1; ++k)
                {
      /*        for (i=j; i<n; ++i) v[i]-=G[j+n*k]*G[i+n*k]; */
                c_saxpy(-G[j+n*k],G+j+n*k,v+j,n-j);
                }
            s=fabs(v[j]);
            if (s<eps) return(-j);
            if (s>0.0) s=1/sqrt(s); i1=j+n*j;
            for (i=j; i<n; ++i) G[i1++]=s*v[i];
            }
        free(v);
        return(1);
        }

int mat_chol(double *T,double *X,int m)   /* Cholesky decomposition of X=TT' T lower triangular */
        {
        return(mat_chol2(T,X,m,0.0));
        }

/* mat_mlt 25.10.1986/SM (25.10.1986) (5.2.1999)
*/
/********************************** vanha
mat_mlt(
double *T,
double *X,
double *Y,
int m,
int n,
int r)
        {
        register int i,j;
        extern double sis_tulo();

        for (i=0; i<m; ++i)
            {
            for (j=0; j<r; ++j)
                {
                T[i+m*j]=sis_tulo(X+i,Y+n*j,m,1,n);
                }
            }
        return(1);
        }
***********************************************************/

/* 5.2.1999/SM        X(m*n)*Y(n*r)->T(n*r)
*/
int mat_mlt(
double *T,
double *X,
double *Y,
int m,
int n,
int r)
        {
        int i,j,k;

        for (i=0; i<m*r; ++i) T[i]=0.0;
        for (j=0; j<r; ++j)
            for (k=0; k<n; ++k)
                c_saxpy(Y[k+n*j],X+m*k,T+m*j,m);
        return(1);
        }

/* mat_det 4.12.1998/SM (4.12.1998)
   based on pivotal operation
*/

int mat_det(double *X,int m,int type,double *pdet)
/* int type;  0=compute determinant *pdet
              1=return 1 if positive definite
                       0 if det=0
                      -1 if indefinite
          */
        {
        int i,j,k,h;
        double a,b;

        h=1;
        if (!type) *pdet=1.0;
        for (k=0; k<m; ++k)
            {
            a=X[k*(m+1)];
            if (!type) *pdet*=a;
            if (a==0.0) return(0);
            if (a<0.0)
                {
                h=-1;
                if (type) return(-1);
                }
            if (k==m-1) return(h);
            a=1/a;
            X[k*(m+1)]=a;
            for (j=0; j<m; ++j)
                {
                if (j==k) continue;
                X[k+m*j]*=a;
                }
            for (i=0; i<m; ++i)
                {
                if (i==k) continue;
                b=X[i+m*k];
                for (j=0; j<m; ++j)
                    {
                    if (j==k) continue;
                    X[i+m*j]-=b*X[k+m*j];
                    }
                X[i+m*k]*=-a;
                }
            }
        return(h);
        }

/* mat_eig.c 12.6.1998/SM (12.6.1998)
*/

int mat_nonsymm_eigen(double *a,double *t,double *u,int n,int iter,double ep,int n_small)
        {
        double eps,aii,aij,aji,h,g,hj,aik,aki,aim,ami,
               tep,tem,d,c,e,akm,amk,cx,sx,cot2x,sig,cotx,
               cos2x,sin2x,te,tee,yh,den,tanhy,chy,shy,
               c1,c2,s1,s2,tki,tmi,tik,tim;
        int    i,j,k,m,it,nless1;
        int    mark,left,right;
        int    jatka;
        char x[80];
        int count;
        int *v,v_ind,v_count=0;
        double ns0,ns,aa;

/* printf("iter=%d n=%d\n",iter,n); getch();
*/
        mark=0; left=right=0;

        if (t!=NULL)
            {
            right=1;
            for (i=0; i<n; ++i)
                {
                for (j=0; j<n; ++j) t[i+n*j]=0.0;
                t[i*(n+1)]=1.0;
                }
            }

        if (u!=NULL)
            {
            left=1;
            for (i=0; i<n; ++i)
                {
                for (j=0; j<n; ++j) u[i+n*j]=0.0;
                u[i*(n+1)]=1.0;
                }
            }

        v=(int *)malloc(n*n*sizeof(int));
        /* testi puuttuu */
        for (i=0; i<n*n; ++i) v[i]=0;
        v_ind=0;

        ns0=0.0; for (i=0; i<n*n; ++i) ns0+=a[i]*a[i];

/*      ep=1e-15;    */
        eps=sqrt(ep);
        nless1=n-1;
        count=n*(n-1)/2;
        for (it=0; it<iter; ++it)
            {
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') return(it); } // RS CHA sur_

            sprintf(x," %d %d %d\n",v_ind,it+1,count); sur_print(x);
            count=0;
            if (n_small)
                {
                if (v_ind)
                    {
                    ++v_count;
                    if (v_count>=n_small || mark) v_ind=0;
                    mark=0;
                    }
                else
                    {
                    if (n_small>=10)
                        {
                        ns=0.0;
                        for (i=0; i<n; ++i) for (j=0; j<n; ++j)
                            if (i!=j && i-1!=j && i+1!=j)
                                ns+=a[i+n*j]*a[i+n*j];
                        aa=ns/(ns0+1e-15);
                        sur_cursor_position(&row,&col);
                        sprintf(x,"ss/ss0=%g",aa);
                        write_string("                    ",20,'4',2,8);
                        write_string(x,strlen(x),'4',2,8);
                        sur_locate(row,col);
                        }
                    if (it>0) { v_ind=1; v_count=0; }
                    }
                }
            if (!v_ind && mark) return(it);
            jatka=0;
            for (i=0; i<nless1; ++i)
                {
                aii=a[i*(n+1)];
                for (j=i+1; j<n; ++j)
                    {
                    aij=a[i+n*j]; aji=a[j+n*i];
                    if (fabs(aij+aji)>eps ||
                        (fabs(aij-aji)>eps && fabs(aii-a[j*(n+1)])>eps))
                    { jatka=1; break; }
                    }
                if (jatka) break;
                }

/*
printf("jatka=%d\n",jatka); getch();
*/
            if (!jatka) return(it);
            mark=1;

            for (k=0; k<nless1; ++k)
                {
                for (m=k+1; m<n; ++m)
                    {
                    if (v_ind && v[k+n*m]==0) continue;

                    h=g=hj=yh=0.0;
                    for (i=0; i<n; ++i)
                        {
                        aik=a[i+n*k]; aim=a[i+n*m];
                        te=aik*aik; tee=aim*aim;
                        yh+=te-tee;
                        if (i!=k && i!=m)
                            {
                            aki=a[k+n*i]; ami=a[m+n*i];
                            h+=aki*ami-aik*aim;
                            tep=te+ami*ami;
                            tem=tee+aki*aki;
                            g+=tep+tem;
                            hj-=tep-tem;
                            }
                        } /* i */
                     h*=2; d=a[k*(n+1)]-a[m*(n+1)];
                     akm=a[k+n*m]; amk=a[m+n*k];
                     c=akm+amk; e=akm-amk;
                     if (fabs(c)<=ep) { cx=1.0; sx=0.0; }
                     else
                         {
                         cot2x=d/c;
                         if (cot2x<0) sig=-1.0; else sig=1.0;
                         cotx=cot2x+(sig*sqrt(1.0+cot2x*cot2x));
                         sx=sig/sqrt(1+cotx*cotx);
                         cx=sx*cotx;
                         }
/*
printf("sx=%g cx=%g\n",sx,cx); getch();
*/
                    if (yh<0.0) { tem=cx; cx=sx; sx=-tem; }
                    cos2x=cx*cx-sx*sx;
                    sin2x=2.0*sx*cx;
                    d=d*cos2x+c*sin2x;
                    h=h*cos2x-hj*sin2x;
                    den=g+2.0*(e*e+d*d);
                    tanhy=(e*d-h/2.0)/den;
                    if (fabs(tanhy)<=ep) { chy=1.0; shy=0.0; }
                    else
                        {
                        chy=1.0/sqrt(1.0-tanhy*tanhy);
                        shy=chy*tanhy;
                        }

                    c1=chy*cx-shy*sx;
                    c2=chy*cx+shy*sx;
                    s1=chy*sx+shy*cx;
                    s2=-chy*sx+shy*cx;

                    if (!v_ind) v[k+n*m]=0;
                    if (fabs(s1)>ep || fabs(s2)>ep)
                        {
                        if (!v_ind) v[k+n*m]=1;
                        ++count;
                        mark=0;
                        for (i=0; i<n; ++i)
                            {
                            aki=a[k+n*i]; ami=a[m+n*i];
                            a[k+n*i]=c1*aki+s1*ami;
                            a[m+n*i]=s2*aki+c2*ami;
                            if (left)
                                {
                                tki=u[k+n*i]; tmi=u[m+n*i];
                                u[k+n*i]=c1*tki+s1*tmi;
                                u[m+n*i]=s2*tki+c2*tmi;
                                }
                            }
                        for (i=0; i<n; ++i)
                            {
                            aik=a[i+n*k]; aim=a[i+n*m];
                            a[i+n*k]=c2*aik-s2*aim;
                            a[i+n*m]=-s1*aik+c1*aim;
                            if (right)
                               {
                               tik=t[i+n*k]; tim=t[i+n*m];
                               t[i+n*k]=c2*tik-s2*tim;
                               t[i+n*m]=-s1*tik+c1*tim;
                               }
                            }

                        } /* if */
                    } /* m */
                } /* k */
            } /* it */

        return(it);
        }

/* mat_kron.c 7.6.1998/SM (7.6.1998)
*/

int mat_kronecker(double *zz,double *xx,double *yy,int mX,int nX,int mY,int nY)
        {
        int i,j,i1,j1;
        double a;

        for (i=0; i<mX; ++i) for (j=0; j<nX; ++j)
            {
            a=xx[i+mX*j];
            for (i1=0; i1<mY; ++i1) for (j1=0; j1<nY; ++j1)
                {
/* rivi: i*mY+i1  sar: j*nY+j1 */
                zz[i*mY+i1+(j*nY+j1)*mX*mY]=a*yy[i1+mY*j1];
                }
            }

        return(1);
        }

/* mat_gram 5.6.1999/SM (5.6.1999)
   Modified (Golub,van Loan)
*/

int mat_gram_schmidt(double *T,double *Y,double *X,int m,int n,double tol)
        {
        int i,k,j;
        double a,a1,b;

        for (i=0; i<n*n; ++i) Y[i]=0.0;
        a1=1.0;
        for (k=0; k<n; ++k)
            {
            a=0.0;
            for (i=0; i<m; ++i)
                {
                b=X[i+m*k];
                a+=b*b;
                }
            if (a/a1<tol*tol) return(-k);
            a1=a;
            Y[k*(n+1)]=sqrt(a);
            b=1.0/Y[k*(n+1)];
            for (i=0; i<m; ++i) T[i+m*k]=b*X[i+m*k];
            for (j=k+1; j<n; ++j)
                {
                a=0.0;
                for (i=0; i<m; ++i)
                    a+=T[i+m*k]*X[i+m*j];
                Y[k+n*j]=a;
                a=0.0;
                for (i=0; i<m; ++i)
                    X[i+m*j]-=T[i+m*k]*Y[k+n*j];
                }
            }
        return(1);
        }

/* mat_sub 25.10.1986/SM (25.10.1986)
*/

int mat_sub(double *T,double *X,double *Y,int m,int n)
        {
        int k;

        for (k=0; k<m*n; ++k) T[k]=X[k]-Y[k];
        return(1);
        }

/* mat_add 25.10.1986/SM (25.10.1986)
*/
int mat_add(double *T,double *X,double *Y,int m,int n)
        {
        int k;

        for (k=0; k<m*n; ++k) T[k]=X[k]+Y[k];
        return(1);
        }

/* mat_gj 6.2.1999/SM (6.2.1999)
*/

static void sing_matrix()
        {
        sur_print("\nSingular matrix!");
        WAIT;
        }

int mat_gj(double *a,int n,double *b,int m,double *pdet)
        {
        int i,icol=0,irow=0,j,k,l,ll,dets;
        double big,dum,pivinv,temp,det;
        int i1,i2;

        indxc=(int *)malloc(n*sizeof(int));
        if (indxc==NULL) { not_enough_memory(); return(-1); }
        indxr=(int *)malloc(n*sizeof(int));
        if (indxr==NULL) { not_enough_memory(); return(-1); }
        ipiv=(int *)malloc(n*sizeof(int));
        if (ipiv==NULL) { not_enough_memory(); return(-1); }

        det=0.0; dets=1;
        for (j=0; j<n; ++j) ipiv[j]=0;
        for (i=0; i<n; ++i)
            {
            big=0.0;
            for (j=0; j<n; ++j)
                if (ipiv[j]!=1) for (k=0,i1=n*j; k<n; ++k,++i1)
                    {
                    if (ipiv[k]==0)
                      {
                      if (fabs(a[i1])>=big)
                          { big=fabs(a[i1]); irow=j; icol=k; }
                      }
                    else if (ipiv[k]>1)
                      {
                      sing_matrix(); return(-1);
                      }
                    }  /* k */
            ++(ipiv[icol]);

            if (irow!=icol)
                {
                i1=n*irow; i2=n*icol;
                for (l=0; l<n; ++l,++i1,++i2) SWAP(a[i1],a[i2]);
                i1=n*irow; i2=n*icol;
                for (l=0; l<m; ++l,++i1,++i2) SWAP(b[i1],b[i2]);
                dets=-dets;
                }

            indxr[i]=irow; indxc[i]=icol;
            i1=(n+1)*icol;
            if (a[i1]==0.0) { sing_matrix(); return(-1); }
            det+=log(fabs(a[i1])); if (a[i1]<0) dets=-dets;
            pivinv=1/a[i1];
            a[i1]=1.0;
            i1=n*icol;
            for (l=0; l<n; ++l) a[i1++]*=pivinv;
            i1=n*icol;
            for (l=0; l<m; ++l) b[i1++]*=pivinv;
            for (ll=0; ll<n; ++ll)
                if (ll!=icol)
                    {
                    dum=a[icol+n*ll]; a[icol+n*ll]=0.0;
                    i1=n*ll; i2=n*icol;
                    for (l=0; l<n; ++l,++i1,++i2) a[i1]-=dum*a[i2];
                    i1=n*ll; i2=n*icol;
                    for (l=0; l<m; ++l,++i1,++i2) b[i1]-=dum*b[i2];
                    }
            } /* i */

        for (l=n-1; l>=0; --l)
            {
            if (indxr[l]!=indxc[l])
                for (k=0; k<n; ++k)
                    SWAP(a[indxr[l]+n*k],a[indxc[l]+n*k]);
            }

        free(ipiv);
        free(indxc);
        free(indxr);

        if (det<700.0) *pdet=dets*exp(det);
        else           *pdet=1e308;
        return(1);
        }

/* mat_inv 6.2.1999/SM (6.2.1999)
*/

int mat_inv(double *z,double *x,int n,double *pdet)
        {
        int i;

        i=mat_gj(x,n,z,0,pdet);
        if (i<0) return(-1);
        for (i=0; i<n*n; ++i) z[i]=x[i];
        return(1);
        }

/*  csolve2.c 12.4.1986/SM (12.4.1986)

*/

static int diag_error(int i)
        {
        printf("\nDiagonal element # %d 'zero'",i+1);
        WAIT;
        return(1);
        }

static int solve_upper(double *x,double *a,double *b,int m,int k,double eps)
        {
        int i,j,g;
        double s,t;

        sur_print("\nsolve_upper");
        for (g=0; g<k; ++g)
            for (i=m-1; i>=0; --i)
                {
                s=b[i+m*g];
                t=a[i*(m+1)];
                if (fabs(t)<eps) { diag_error(i); return(-1); }
                for (j=i+1; j<m; ++j)
                    s-=a[i+m*j]*x[j+m*g];
                x[i+m*g]=s/t;
                }
        return(1);
        }

static int solve_lower(double *x,double *a,double *b,int m,int k,double eps)
        {
        int i,j,g;
        double s,t;

        sur_print("\nsolve_lower");
        for (g=0; g<k; ++g)
            for (i=0; i<m; ++i)
                {
                s=b[i+m*g];
                t=a[i*(m+1)];
                if (fabs(t)<eps) { diag_error(i); return(-1); }
                for (j=0; j<i; ++j)
                    s-=a[i+m*j]*x[j+m*g];
                x[i+m*g]=s/t;
                }
        return(1);
        }


static int solve_diag(double *x,double *a,double *b,int m,int k,double eps)
        {
        int i,g;
        double s;

        sur_print("\nsolve_diag");
        for (i=0; i<m; ++i)
            {
            s=a[i*(m+1)];
            if (fabs(s)<eps) { diag_error(i); return(-1); }
            for (g=0; g<k; ++g)
                x[i+m*g]=b[i+m*g]/s;
            }
        return(1);
        }


static int solve_symm(double *x,double *a,double *b,int m,int k,double eps)
        {
        int i,j,h;
        double s;

        p=(double *)malloc(m*sizeof(double));
        if (p==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<m; ++i)
            for (j=0; j<m; ++j)
                {
                s=a[i+m*j];
                for (h=i-1; h>=0; --h)
                    s-=a[j+m*h]*a[i+m*h];
                if (j==i)
                    {
                    if (s<eps)
                        {
                        if (fabs(s)<eps) { lin_dep(i); return(-1); }
                        return(ortholin1(a,m,m,b,k,eps,x,0));
                        }
                    p[i]=1/sqrt(s);
                    }
                else a[j+m*i]=s*p[i];
                } /* j,i */

        for (j=0; j<k; ++j)
            {
            for (i=0; i<m; ++i)
                {
                s=b[i+m*j];
                for (h=i-1; h>=0; --h)
                    s-=a[i+m*h]*b[h+m*j];
                b[i+m*j]=s*p[i];
                }
            for (i=m-1; i>=0; --i)
                {
                s=b[i+m*j];
                for (h=i+1; h<m; ++h) s-=a[h+m*i]*b[h+m*j];
                b[i+m*j]=s*p[i];
                }
            }
        for (i=0; i<m*k; ++i) x[i]=b[i];
        free(p);
        return(1);
        }


/*  csolve.c 11.4.1986/SM (12.4.1986)

*/

static int tutki_kertoimet(double *a,int m,int n)
                                /* 20=diag, 10=symm, 9=upper triang
                                                     8=lower triang
                                    0=general
                                */
        {
        int i,j;
        int upper=1;
        int lower=1;
        int symm=1;

        if (m!=n) return(0);
        for (i=0; i<m; ++i)
            for (j=0; j<i; ++j)
                {
                if (a[i+m*j]!=a[j+m*i]) symm=0;
                if (a[i+m*j]!=0) upper=0;
                if (a[j+m*i]!=0) lower=0;
                }
        if (upper && lower) return(20);
        if (symm) return(10);
        if (upper) return(9);
        if (lower) return(8);
        return(0);
        }

int mat_solve(double *x,double *a,double *b,int m,int n,int k,double eps)
        {
        int i;
        int tyyppi;

        tyyppi=tutki_kertoimet(a,m,n);

        if (tyyppi==20) return(solve_diag(x,a,b,m,k,eps));
        if (tyyppi==10) return(solve_symm(x,a,b,m,k,eps));
        if (tyyppi==9) return(solve_upper(x,a,b,m,k,eps));
        if (tyyppi==8) return(solve_lower(x,a,b,m,k,eps));

        i=ortholin1(a,m,n,b,k,eps,x,0);
                               /*   0=!improvement  */
        return(i);
        }

/* mat_2mm 2.5.1992/SM (2.5.1992)
*/

int mat_2mtm(double *T,double *X,double *Y,int m,int n,int r)	  /*  T=X'*Y */
	{
	register int i,j;
//	extern double sis_tulo();

	for (i=0; i<n; ++i)
	    {
	    for (j=0; j<r; ++j)
		T[i+n*j]=sis_tulo(X+m*i,Y+m*j,1,1,m);
	    }
	return(1);
	}

int mat_2mmt(double *T,double *X,double *Y,int m,int n,int r)
	{
	register int i,j;
//	extern double sis_tulo();

	for (i=0; i<m; ++i)
	    {
	    for (j=0; j<r; ++j)
		T[i+m*j]=sis_tulo(X+i,Y+j,m,r,n);
	    }
	return(1);
	}

/* dcholinv.c 22.7.85/SM (13.11.1987)
   cholinv+det
*/

/*
  B=INV(A), A positive definite    double A[(n+1)*n]

   n=5
	0   1	2     n-1   n
0     A00 B00 B01 B02 B03 B04
1     A10 A11 B11 B12 B13 B14
2     A20 A21 A22 B22 B23 B24
      A30 A31 A32 A33 B33 B34
n-1   A40 A41 A42 A43 A44 B44

Normally 1 is returned.
If A is not positive definite, -j (j first var dependent on previous ones)
is returned.

*/

int mat_dcholinv(double *a,int n,double *pdet)
	{
	int i,j,k,i1,j1;
	double z,x,y=0;

	*pdet=1.0;
	for (i=0; i<n; ++i)
	    {
	    i1=i+1;
	    for (j=i; j<n; ++j)
		{
		j1=j+1;
		x=a[n*i+j];	/* ajattele: i=sarake, j=rivi */
				/* alunperin talletus riveittÑin */
				/* nyt sarakkeittain */
		for (k=i-1; k>=0; --k)
		    x-=a[n*j1+k]*a[n*i1+k];
		if (j==i)
		    {
		    if (x<=0.0) return(-j);
		    a[n*i1+i]=y=1/sqrt(x);
		    *pdet*=x;
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

/* mat_mtm2 25.10.1986/SM (26.3.1987)
  T=X'Y  n*r
*/

int mat_mtm2(double *T,double *X,double *Y,int m,int n,int r)
	{
	register int i,j;
//	extern double sis_tulo();
	double a;

	for (i=0; i<n; ++i) for (j=0; j<r; ++j)
	    {
	    a=sis_tulo(X+m*i,Y+m*j,1,1,m);
	    T[i+n*j]=a;
	    }
	return(1);
	}

/* mat_copy 24.3.1987/SM (24.3.1987)
*/

int mat_copy(double *T,double *X,int m,int n)
	{
	register unsigned int i,k;

	k=m*n;
	for (i=0; i<k; ++i)
	    T[i]=X[i];
	return(1);
	}

/* cholmove.c 1.11.1986/SM (1.11.1986)
*/

int mat_cholmove(double *A,int n)
	{
	int i,j;

	for (i=0; i<n*n; ++i) A[i]=A[i+n];
	for (j=0; j<n; ++j)
	    for (i=j+1; i<n; ++i)
		A[i+j*n]=A[j+i*n];
	return(1);
	}

/* cholinv.c 22.7.85/SM (5.4.1986)
*/

/*
  B=INV(A), A positive definite    double A[(n+1)*n]

   n=5
	0   1	2     n-1   n
0     A00 B00 B01 B02 B03 B04
1     A10 A11 B11 B12 B13 B14
2     A20 A21 A22 B22 B23 B24
      A30 A31 A32 A33 B33 B34
n-1   A40 A41 A42 A43 A44 B44

Normally 1 is returned.
If A is not positive definite, -j (j first var dependent on previous ones)
is returned.

*/

int mat_cholinv(double *a,int n)
	{
	int i,j,k,i1,j1;
	double z,x,y=0;

	for (i=0; i<n; ++i)
	    {
	    i1=i+1;
	    for (j=i; j<n; ++j)
		{
		j1=j+1;
		x=a[n*i+j];	/* ajattele: i=sarake, j=rivi */
				/* alunperin talletus riveittÑin */
				/* nyt sarakkeittain */
		for (k=i-1; k>=0; --k)
		    x-=a[n*j1+k]*a[n*i1+k];
		if (j==i)
		    {
		    if (x<=0) return(-j);
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

/* mat_nrm 25.10.1986/SM (25.10.1986)
*/

int mat_nrm(double *T,double *X,int m,int n)
	{
	int i,j;
	int mj;
	double a,b;

	for (j=0; j<n; ++j)
	    {
	    a=0.0;
	    mj=m*j;
	    for (i=0; i<m; ++i)
		{
		b=X[i+mj];
		a+=b*b;
		}
	    T[j]=a=sqrt(a);
	    if (a==0.0) continue;
	    for (i=0; i<m; ++i)
		X[i+mj]/=a;
	    }
	return(1);
	}

/* mat_sum 25.10.1986/SM (25.10.1986)
*/

int mat_sum(double *T,double *X,int m,int n)
	{
	int i,j;
	int mj;
	double a;

	for (j=0; j<n; ++j)
	    {
	    a=0.0;
	    mj=m*j;
	    for (i=0; i<m; ++i)
		a+=X[i+mj];
	    T[j]=a;
	    }
	return(1);
	}

/* mat_p 25.10.1986/SM (25.10.1986)
   pivotal operation
*/

int mat_p(double *X,int m,int k)
	{
	int i,j;
	double a,b;

	a=1/X[k+m*k];
	X[k+m*k]=a;
	for (j=0; j<m; ++j)
	    {
	    if (j==k) continue;
	    X[k+m*j]*=a;
	    }
	for (i=0; i<m; ++i)
	    {
	    if (i==k) continue;
	    b=X[i+m*k];
	    for (j=0; j<m; ++j)
		{
		if (j==k) continue;
		X[i+m*j]-=b*X[k+m*j];
		}
	    X[i+m*k]*=-a;
	    }
	return(1);
	}

/* mat_cent 25.10.1986/SM (25.10.1986)
*/

int mat_center(double *T,double *X,int m,int n)
	{
	int i,j;
	int mj;
	double a;

	for (j=0; j<n; ++j)
	    {
	    a=0.0;
	    mj=m*j;
	    for (i=0; i<m; ++i)
		a+=X[i+mj];
	    a=T[j]=a/(double)m;
	    for (i=0; i<m; ++i)
		X[i+mj]-=a;
	    }
	return(1);
	}

/* mat_tran 25.10.1986/SM (25.10.1986)
*/

int mat_transp(double *T,double *X,int m,int n)
	{
	register int i,j;

	for (i=0; i<m; ++i) for (j=0; j<n; ++j)
	    T[j+n*i]=X[i+m*j];
	return(1);
	}

/* mat_mltd 25.10.1986/SM (25.10.1986)
*/

int mat_mltd(double *T,double *X,double *Y,int m,int n)
	{
	int i,j;
	double yjj;

	for (j=0; j<n; ++j)
	    {
	    yjj=Y[j+n*j];
	    for (i=0; i<m; ++i)
		T[i+m*j]=X[i+m*j]*yjj;
	    }
	return(1);
	}

/* mat_dmlt 25.10.1986/SM (25.10.1986)
*/

int mat_dmlt(double *T,double *X,double *Y,int m,int n)
	{
	int i,j;
	double xii;

	for (i=0; i<m; ++i)
	    {
	    xii=X[i+m*i];
	    for (j=0; j<n; ++j)
		T[i+m*j]=xii*Y[i+m*j];
	    }
	return(1);
	}
