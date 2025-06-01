#include "muste.h"
/*  _pol.c 21.6.1986/SM (26.6.1986)
*/
//#include <conio.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>        /* struct complex */
#include "survo.h"
#include "survoext.h"
#include "survolib.h"
// #include "pol.h"

#define MAXN 10000

struct polynom
    {
    int n;
    struct complex a[MAXN];
    } ;

extern int muste_pol_load(char *matr,struct polynom *pol);
extern int muste_pol_save(char *matr,struct polynom *pol);
extern int muste_pol_save2(char *matr,struct polynom *pol,char *ots,int base,char *expr);



#define C_ZERO 1e-100
#define EPS 1e-12
#define MAX_ITER 50
// #define EPS 2.0e-6
// #define EPS 2.0e-14
#define LEPS 1.0e-14
#define MAXM 100
#define FMAX(a,b) (maxarg1=(a),maxarg2=(b),(maxarg1)>(maxarg2)?\
(maxarg1) : (maxarg2)
// #define EPSS 1.0e-7
// #define EPSS 1.0e-14
   #define EPSS 2.22e-16
#define MR 8
#define MT 10
#define MAXIT (MT*MR)

struct complex
{
   double x;  /*real part*/
   double y;  /*imag part*/
};


typedef struct FCOMPLEX {double r,i;} fcomplex;


static char xx[LLENGTH];
static char *p,*q;
static int mtx=0;
static int n_row_comments=0; // 16.8.2006 lis„tty
// RS REM static char **specs;
static double roots_eps=LEPS;
static int roots_max_iter=MAX_ITER;
// static double maxarg1,maxarg2;
static double frac[MR+1]={0.0,0.5,0.25,0.75,0.13,0.38,0.62,0.88,1.0};
static fcomplex pol2[100],roots2[100]; // 8.3.2017

static fcomplex Cadd(fcomplex a,fcomplex b)
{
fcomplex c;
c.r=a.r+b.r;
c.i=a.i+b.i;
return c;
}

static fcomplex Csub(fcomplex a,fcomplex b)
{
fcomplex c;
c.r=a.r-b.r;
c.i=a.i-b.i;
return c;
}

static fcomplex Cmul(fcomplex a,fcomplex b)
{
fcomplex c;
c.r=a.r*b.r-a.i*b.i;
c.i=a.i*b.r+a.r*b.i;
return c;
}

static fcomplex Complex(double re, double im)
{
fcomplex c;
c.r=re;
c.i=im;
return c;
}

/*
static fcomplex Conjg(fcomplex z)
{
fcomplex c;
c.r=z.r;
c.i=-z.i;
return c;
}
*/

static fcomplex Cdiv(fcomplex a,fcomplex b)
{
fcomplex c;
double r,den;
if (fabs(b.r)>=fabs(b.i)) {
  r=b.i/b.r;
  den=b.r+r*b.i;
  c.r=(a.r+r*a.i)/den;
  c.i=(a.i-r*a.r)/den;
} else {
  r=b.r/b.i;
  den=b.i+r*b.r;
  c.r=(a.r*r+a.i)/den;
  c.i=(a.i*r-a.r)/den;
}
return c;
}

static double Cabs(fcomplex z)
{
double x,y,ans,temp;
x=fabs(z.r);
y=fabs(z.i);
if (x==0.0)
  ans=y;
else if (y==0.0)
  ans=x;
else if (x>y) {
temp=y/x;
ans=x*sqrt(1.0+temp*temp);
} else {
temp=x/y;
ans=y*sqrt(1.0+temp*temp);
}
return ans;
}

static fcomplex Csqrt(fcomplex z)
{
fcomplex c;
double x,y,w,r;
if ((z.r==0.0) && (z.i==0.0)) {
  c.r=0.0;
  c.i=0.0;
  return c;
} else {
x=fabs(z.r);
y=fabs(z.i);
if (x>=y) {
  r=y/x;
  w=sqrt(x)*sqrt(0.5*(1.0+sqrt(1.0+r*r)));
} else {
  r=x/y;
  w=sqrt(y)*sqrt(0.5*(r+sqrt(1.0+r*r)));
}
if (z.r>=0.0) {
  c.r=w;
  c.i=z.i/(2.0*w);
} else {
  c.i=(z.i >= 0.0) ? w : -w;
  c.r=z.i/(2.0*c.i);
}
return c;
}
}

static fcomplex RCmul(double x, fcomplex a)
{
fcomplex c;
c.r=x*a.r;
c.i=x*a.i;
return c;
}

static void laguer(fcomplex a[], int m, fcomplex *x, int *its)
{
int iter,j;
double abx,abp,abm,err;
double maxa;
fcomplex dx,x1,b,d,f,g,h,sq,gp,gm,g2;
// static double frac[MR+1]={0.0,0.5,0.25,0.75,0.13,0.38,0.62,0.88,1.0};

for (iter=1; iter<=MAXIT;  iter++) {
  *its=iter;
  b=a[m];
  err=Cabs(b);
  d=f=Complex(0.0,0.0);
  abx=Cabs(*x);
  for (j=m-1; j>=0; j--) {
    f=Cadd(Cmul(*x,f),d);
    d=Cadd(Cmul(*x,d),b);
    b=Cadd(Cmul(*x,b),a[j]);
    err=Cabs(b)+abx*err;
  }
  err *= EPSS;
  if (Cabs(b) <= err) return;
  g=Cdiv(d,b);
  g2=Cmul(g,g);
  h=Csub(g2,RCmul(2.0,Cdiv(f,b)));
  sq=Csqrt(RCmul((double) (m-1),Csub(RCmul((double) m,h),g2)));
  gp=Cadd(g,sq);
  gm=Csub(g,sq);
  abp=Cabs(gp);
  abm=Cabs(gm);
  if (abp < abm) gp=gm;

// dx=((FMAX(abp,abm) > 0.0 ? Cdiv(Complex((double) m,0.0),gp)
  maxa=abp; if (abm>abp) maxa=abm;
  dx=((maxa > 0.0 ? Cdiv(Complex((double) m,0.0),gp)
 : RCmul(exp(log(1.0+abx)),Complex(cos((double)iter),sin((double)iter)))));
  x1=Csub(*x,dx);
  if (x->r == x1.r && x->i == x1.i) return; // Converged!
  if (iter%MT) *x=x1;
  else *x=Csub(*x,RCmul(frac[iter/MT],dx));
  }
sur_print("\nToo many iterations"); WAIT;
return;
}

// from SURVO MM
static int diss(double x,double ear,long *pm,long *pn) // approximation of real number by a ratio
        {
        long m,k,mm,nn;
        double f,a,b,diss;
        int vaihto=0;

/* printf("\nx=%g ear=%g",x,ear); getch();   */
        if (x==0.0) { *pm=0; *pn=0; return(1); }
        if (x<1.0) { x=1/x; vaihto=1; }
        f=1e10; m=k=0; a=pow(10.0,ear);  /* k vastaa parametria n */
        while ((double)k<f)
            {
            ++m; k=(long)(m*x); diss=m*x-k;
            if (diss>=0.5) { diss=1.0-diss; ++k; }
            b=diss/k; b=k+a*b*b;
            if (b<f) { f=b; mm=m; nn=k; }
            }
        if (vaihto) { *pm=nn; *pn=mm; } else { *pm=mm; *pn=nn; }
        return(1);
        }

static double tark(double r)
    {
    int merkki,kok;
    double a,b;
    long m,n;

    if (r<0.0) merkki=-1; else merkki=1;
    a=fabs(r); if (a<0.00001) return(0.0);
    kok=(int)a;
    b=a-(double)kok;
    if (b<0.0000001) return((double)kok);
// printf("\nkok=%d b=%g",kok,b); getch();
    diss(b,10.0,&m,&n);
    if (m==0) return(r);
// printf("\nn=%ld m=%ld",m,n); getch();
    a=merkki*((double)kok+(double)n/(double)m);
    return(a);
    }

// 9.3.2017 by SM
static int tarkennus(fcomplex a[], int m, fcomplex roots[])
    {
    int i,j;
    fcomplex b,b1,r;

    for (i=1; i<=m; ++i)
      {
      b=a[m];
      for (j=m-1; j>=0; j--)
          b=Cadd(Cmul(roots[i],b),a[j]);
//    printf("\nre=%.16g im=%.16g b.r=%g b.i=%g",
//              roots[i].r,roots[i].i,  b.r,b.i);
      r.r=tark(roots[i].r);
      r.i=tark(roots[i].i);
      b1=a[m];
      for (j=m-1; j>=0; j--)
          b1=Cadd(Cmul(r,b1),a[j]);
//    printf("\ntark:");
//    printf("\nre=%.16g im=%.16g b1.r=%g b1.i=%g",
//              r.r,r.i,  b1.r,b1.i);

   // Root is replaced by rational approximation
   // when latter is more accurate as a root.
      if(b1.r*b1.r+b1.i*b1.i<=b.r*b.r+b.i*b.i)
        { roots[i].r=r.r; roots[i].i=r.i; }

//    getch();
      }

    return(1);
    }


static void zroots(fcomplex a[], int m, fcomplex roots[], int polish)
{
void laguer(fcomplex a[], int m, fcomplex *x, int *its);
int i,its,j,jj;
fcomplex x,b,c,ad[MAXM];

for (j=0; j<=m; j++) ad[j]=a[j];
for (j=m; j>=1; j--) {
  x=Complex(0.0,0.0);
   laguer(ad,j,&x,&its);
  if (fabs(x.i) <= 2.0*LEPS*fabs(x.r)) x.i=0.0;
  roots[j]=x;
  b=ad[j];
  for (jj=j-1; jj>=0; jj--) {
    c=ad[jj];
    ad[jj]=b;
    b=Cadd(Cmul(x,b),c);
  }
}

/******************************
// kokeilu 9.3.2017
for (i=1; i<=m; ++i)
  {
  printf("\n");
  b=a[m];
  for (j=m-1; j>=0; j--)
      b=Cadd(Cmul(roots[i],b),a[j]);
  printf("\nre=%.16g im=%.16g b.r=%g b.i=%g",
            roots[i].r,roots[i].i,  b.r,b.i);
  }
  getch();
// - kokeilu 9.3.2017
******************************/

if (polish)
  for (j=1;j<=m; j++)
    laguer(a,m,&roots[j],&its);

for (j=2; j<=m; j++) {
  x=roots[j];
  for (i=j-1; i>=1; i--) {
    if (roots[i].r <= x.r) break;
    roots[i+1]=roots[i];
    }
  roots[i+1]=x;
  }

tarkennus(a,m,roots);
}



static int zero(double x)
        {
        if (fabs(x)<C_ZERO) return(1);
        return(0);
        }
        
static int c_zero(struct complex *z)
        {
        if (zero(z->x) && zero(z->y)) return(1);
        return(0);
        }

static struct complex *c_add(struct complex *z,struct complex *z1,struct complex *z2)
        {
        z->x=z1->x+z2->x;
        z->y=z1->y+z2->y;
        return(z);
        }
        
static struct complex *c_sub(struct complex *z,struct complex *z1,struct complex *z2)
        {
        z->x=z1->x-z2->x;
        z->y=z1->y-z2->y;
        return(z);
        }
        
static struct complex *c_mult(struct complex *z,struct complex *z1,struct complex *z2)
        {
        struct complex u;
        u.x=z1->x*z2->x-z1->y*z2->y;
        u.y=z1->x*z2->y+z2->x*z1->y;
        z->x=u.x; z->y=u.y;  /* jotta my”s z=z1 mahdollinen */
        return(z);
        }
        
struct complex *c_div(struct complex *z,struct complex *z1,struct complex *z2)
        {
        double divisor;
        struct complex u;
        divisor=z2->x*z2->x+z2->y*z2->y;
        if (zero(divisor))
            {
            sur_print("\nDivision by 0 in complex arithmetics!");
            WAIT; 
            return(z); // RS CHA exit
            }
        u.x=(z1->x*z2->x+z1->y*z2->y)/divisor;
        u.y=(z2->x*z1->y-z1->x*z2->y)/divisor;
        z->x=u.x; z->y=u.y;  /* jotta my”s z=z1 mahdollinen */
        return(z);
        }



static void pol_dim_overflow()
        {
        sprintf(sbuf,"\nMax. degree of a polynomial is %d",MAXN-1);
        sur_print(sbuf); WAIT;
        }

static struct polynom *pol_mult(struct polynom *p,struct polynom *p1,struct polynom *p2)
        {
        int i,j;
        struct complex tulo;

        p->n=p1->n+p2->n;
        if (p->n>MAXN-1) { pol_dim_overflow(); return(p); }

        for (i=0; i<=p->n; ++i)
            p->a[i].x=p->a[i].y=0.0;
        for (i=0; i<=p1->n; ++i)
            for (j=0; j<=p2->n; ++j)
                c_add(&(p->a[i+j]),&(p->a[i+j]),
                          c_mult(&tulo,&(p1->a[i]),&(p2->a[j])));

        return(p);
        }

static struct polynom *pol_mult_real(struct polynom *p,struct polynom *p1,struct polynom *p2)
        {
        int i,j;
// RS REM        struct complex tulo;

        p->n=p1->n+p2->n;
        if (p->n>MAXN-1) { pol_dim_overflow(); return(p); }

        for (i=0; i<=p->n; ++i)
            p->a[i].x=p->a[i].y=0.0;
        for (i=0; i<=p1->n; ++i)
            for (j=0; j<=p2->n; ++j)
                 p->a[i+j].x+=p1->a[i].x*p2->a[j].x;

         /*     c_add(&(p->a[i+j]),&(p->a[i+j]),
                          c_mult(&tulo,&(p1->a[i]),&(p2->a[j])));
         */
        return(p);
        }




static struct polynom *pol_div(struct polynom *p,struct polynom *q,struct polynom *p1,struct polynom *p2)
        {
        int i,j;
        struct complex z,z1;

		z.x=0; z.y=0; // RS 7.2.2013
        p->n=p1->n-p2->n;
        if (p->n<0)
            {
            sur_print("\nDegree of dividend < Degree of divisor");
            WAIT; return(p);
            }
        for (i=0; i<=p->n; ++i)
            p->a[i].x=p->a[i].y=0.0;
        for (i=0; i<=p1->n; ++i)
            { q->a[i].x=p1->a[i].x; q->a[i].y=p1->a[i].y; }

        for (i=p1->n; i>=p2->n; --i)
            {
            if (c_zero(&(q->a[i]))) continue;
            c_div(&z,&(q->a[i]),&(p2->a[p2->n]));
            p->a[i-p2->n].x=z.x; p->a[i-p2->n].y=z.y;
            q->a[i].x=q->a[i].y=0.0;
            for (j=i-1; j>=i-p2->n; --j)
                c_sub(&(q->a[j]),&(q->a[j]),
                      c_mult(&z1,&z,&(p2->a[p2->n-i+j])));
            }
        i=p2->n-1;
        while (c_zero(&(q->a[i])) && i>0) --i;
        q->n=i;
        return(p);
        }

static struct polynom *pol_add(struct polynom *p,struct polynom *p1,struct polynom *p2)
        {
        int i;
// RS REM        struct complex tulo;

        p->n=(p1->n>p2->n)? (p1->n):(p2->n);

        for (i=0; i<=p->n; ++i)
            {
            if (i<=p1->n)
                {
                if (i<=p2->n)
                    c_add(&(p->a[i]),&(p1->a[i]),&(p2->a[i]));
                else
                    { p->a[i].x=p1->a[i].x; p->a[i].y=p1->a[i].y; }
                }
            else
                { p->a[i].x=p2->a[i].x; p->a[i].y=p2->a[i].y; }
            }
        i=p->n;
        while (c_zero(&(p->a[i])) && i>0) --i;
        p->n=i;
        return(p);
        }

static struct polynom *pol_sub(struct polynom *p,struct polynom *p1,struct polynom *p2)
        {
        int i;
// RS REM        struct complex tulo;

        p->n=(p1->n>p2->n)? (p1->n):(p2->n);

        for (i=0; i<=p->n; ++i)
            {
            if (i<=p1->n)
                {
                if (i<=p2->n)
                    c_sub(&(p->a[i]),&(p1->a[i]),&(p2->a[i]));
                else
                    { p->a[i].x=p1->a[i].x; p->a[i].y=p1->a[i].y; }
                }
            else
                { p->a[i].x=-(p2->a[i].x); p->a[i].y=-(p2->a[i].y); }
            }
        i=p->n;
        while (c_zero(&(p->a[i])) && i>0) --i;
        p->n=i;
        return(p);
        }




static struct complex *pol_value(
struct polynom *p,
struct complex *z,  /* pointer to argument */
struct complex *v   /* pointer to function value */
)
        {
        int i;

        v->x=p->a[p->n].x;
        v->y=p->a[p->n].y;
        for (i=p->n-1; i>=0; --i)
            c_add(v,&(p->a[i]),c_mult(v,z,v));
        return(v);
        }

static struct polynom *pol_der(struct polynom *d,struct polynom *p)
        {
        int i;

        if (p->n==0)
            {
            d->n=0; d->a[0].x=0.0; d->a[0].y=0-0; return(d);
            }

        d->n=p->n-1;
        for (i=0; i<=d->n; ++i)
            {
            d->a[i].x=(double)(i+1)*p->a[i+1].x;
            d->a[i].y=(double)(i+1)*p->a[i+1].y;
            }
        return(d);
        }

static struct polynom *pol_lag(struct polynom *d,struct polynom *p,int j)
        {
        int i,h;
        double a=0,b=0;

//      Rprintf("\n");
//      for (i=0; i<=p->n; ++i) Rprintf("%g ",p->a[i].x); getch();

        for (i=0; i<=p->n; ++i)
            {
            if (p->a[i].y != 0.0)
                {
                sur_print("\nThis command is for polynomials with real coefficients only!");
                WAIT; return(d); // RS CHA exit
                }
            }

        for (i=0; i<=p->n; ++i)
            {
            for (h=0; h<=p->n-i; ++h)
                {
                if (h==0)
                    { a=p->a[i].x; /* Rprintf("\nai=%g",a); getch(); */ b=1.0; }
                else
                    {
                    b*=-(i+h)*j; b/=h;
/*          Rprintf("\ni=%d h=%d j=%d b=%g",i,h,j,b); getch();  */
                    a+=b*p->a[i+h].x;
/*          Rprintf("\naih=%g",a); getch();    */
                    }
                }
/* Rprintf("\na=%g|",a); getch(); */
            d->a[i].x=a; d->a[i].y=0.0;
            }
        d->n=p->n;
        return(d);
        }


/*
static int polroot(
struct polynom *p,
struct complex *pz,  // pointer to root 
struct complex *pz0  // pointer to initial value
)
        {
        int i;
        int n_iter;
        struct polynom d;
        struct complex v,v0,vd,delta,zmin;
        double y,ymin;
//        extern struct polynom *pol_der();
//        extern struct complex *pol_value();
		delta.x=0; delta.y=0; // RS 7.2.2013
//printf("\np->n=%d",p->n); getch();
        if (p->n==1)
            {
            c_div(pz,&(p->a[0]),&(p->a[1]));
            pz->x=-(pz->x); pz->y=-(pz->y);
            return(1);
            }
//printf("\nr2");
        pol_value(p,pz0,&v0);
//printf("\nr3");
        if (c_zero(&v0))
            {
            pz->x=pz0->x;
            pz->y=pz0->y;
            return(1);
            }
//printf("\nr4");
        zmin.x=pz0->x; zmin.y=pz0->y;
        ymin=v0.x*v0.x+v0.y*v0.y;
        pol_der(&d,p);
//printf("\nr5");
        n_iter=0;
        while (1)
            {
            pol_value(&d,pz0,&vd);

            c_div(&delta,&v0,&vd);
            c_sub(pz,pz0,&delta);
            pol_value(p,pz,&v);
            v0.x=v.x; v0.y=v.y;
            pz0->x=pz->x; pz0->y=pz->y;

            ++n_iter;
            PR_UP;
            sprintf(sbuf,"\npolroot: N=%d  Re=%e Im=%e",n_iter,pz->x,pz->y); sur_print(sbuf); // RS CHA Rprintf

// Rprintf("zero: %d\n",c_zero(pz)); getch(); 
//       if (c_zero(&pz)) break;    
            if (c_zero(pz)) break;
            y=v.x*v.x+v.y*v.y;
            if (y<ymin)
                { ymin=y; zmin.x=pz->x; zmin.y=pz->y; }

            if (fabs(delta.x)<roots_eps && fabs(delta.y)<roots_eps) break;

            if (n_iter>roots_max_iter)
                {
                pz->x=zmin.x; pz->y=zmin.y;
                break;
                }
            if (sur_kbhit())
                {
                pz->x=zmin.x; pz->y=zmin.y;
                i=sur_getch(); if (i=='.') break;
                }
            }
        return (n_iter);
        }


static void pol_roots(struct polynom *rt,struct polynom *p)
        {
        int i,j;
        struct polynom p1,q,r,p2;
        struct complex z,z0;
        int real_roots=1;
        int n_iter;

        p1.n=p->n;
        for (i=0; i<=p1.n; ++i)
            { p1.a[i].x=p->a[i].x; p1.a[i].y=p->a[i].y; }

        for (i=0; i<p->n; ++i)
            {
            z0.x=1.0; z0.y=1.0;
            if (real_roots) z0.y=0.0;
            sprintf(sbuf,"\nRoot %d:   (To interrupt, press '.')\n",i+1);
            sur_print(sbuf);
//printf("\np1.n=%d|",p1.n);
            n_iter=polroot(&p1,&z,&z0);

            if (real_roots && n_iter>=roots_max_iter) { real_roots=0; --i; continue; }
            rt->n=i;
            rt->a[i].x=z.x; rt->a[i].y=z.y;
            p2.n=1;
            p2.a[0].x=-z.x; p2.a[0].y=-z.y;
            p2.a[1].x=1.0; p2.a[1].y=0.0;
            pol_div(&q,&r,&p1,&p2);
            for (j=0; j<=q.n; ++j)
                { p1.a[j].x=q.a[j].x; p1.a[j].y=q.a[j].y; }
            p1.n=q.n;
            }

        }
*/

static void op_roots()
        {
        int i;
        struct polynom pol,roots;
        char expr[LLENGTH];
        char x[LLENGTH], *osa[2];

        i=sp_init(r1+r-1); if (i<0) return;
        i=spfind("MAX_ITER");
        if (i>=0) { strcpy(x,spb[i]); i=split(x,osa,2);
                    roots_max_iter=atoi(osa[0]);
                    if (i>1) roots_eps=atof(osa[1]);
                  }

        i=muste_pol_load(q,&pol); if (i<0) return;
        
        
// new code 8.3.2017
  //    printf("\npol.n=%d",pol.n);
        for (i=0; i<=pol.n; ++i)
            {
  //        printf("\npol.a[i].x=%g",pol.a[i].x);
            pol2[i].r=pol.a[i].x;
            pol2[i].i=pol.a[i].y;
            }
        zroots(pol2,pol.n,roots2,1);  // in COMPLEX2.EDT
  //    printf("\nRoots:");
        for (i=1; i<=pol.n; ++i)
            {
  //        printf("\n%g %g",roots2[i].r,roots2[i].i);
            roots.a[i-1].x=roots2[i].r;
            roots.a[i-1].y=roots2[i].i;
            }
        roots.n=pol.n-1;
  //    getch();        
        
        
        
        
        
        
//printf("2");
//        pol_roots(&roots,&pol); // 8.3.2017

        sprintf(expr,"Roots_of_%s=0",q);
        muste_pol_save2(xx,&roots,"real    imag    ",1,expr);
        }



static void op_mult()
        {
        int i1,i2;
        struct polynom pol,pol1,pol2;
// RS REM        extern struct polynom *pol_mult_real();
// RS REM        extern struct polynom *pol_mult();

        *q++=EOS;
        i1=muste_pol_load(p,&pol1); if (i1<0) return;
        i2=muste_pol_load(q,&pol2); if (i2<0) return;
        if (i1==1 && i2==1) pol_mult_real(&pol,&pol1,&pol2);
        else pol_mult(&pol,&pol1,&pol2);
        muste_pol_save(xx,&pol);
        }

static void op_div()
        {
        int i;
        struct polynom pol,polres,pol1,pol2;
        char *pres,*pp;

//        extern struct polynom *pol_div();

        *q++=EOS;
        i=muste_pol_load(p,&pol1); if (i<0) return;
        i=muste_pol_load(q,&pol2); if (i<0) return;
        pol_div(&pol,&polres,&pol1,&pol2);
        pres=strchr(xx,'(');
        if (pres!=NULL) { *pres=EOS; ++pres; }
        muste_pol_save(xx,&pol);
        if (pres==NULL) return;

        pp=strchr(pres,')');
        if (pp==NULL)
            {
            sur_print("\n( missing!");
            WAIT; return;
            }
        *pp=EOS;
        muste_pol_save(pres,&polres);
        }

static void op_add()
        {
        int i;
        struct polynom pol,pol1,pol2;
//        extern struct polynom *pol_add();

        *q++=EOS;
        i=muste_pol_load(p,&pol1); if (i<0) return;
        i=muste_pol_load(q,&pol2); if (i<0) return;
        pol_add(&pol,&pol1,&pol2);
        muste_pol_save(xx,&pol);
        }

static void op_sub()
        {
        int i;
        struct polynom pol,pol1,pol2;
//        extern struct polynom *pol_sub();

        *q++=EOS;
        i=muste_pol_load(p,&pol1); if (i<0) return;
        i=muste_pol_load(q,&pol2); if (i<0) return;
        pol_sub(&pol,&pol1,&pol2);
        muste_pol_save(xx,&pol);
        }



static struct polynom *pol_product(struct polynom *p,struct polynom *q)
        {
        int i,j;
        struct polynom t,u;

        t.n=1;
        t.a[0].x=-(q->a[0].x); t.a[0].y=-(q->a[0].y);
        t.a[1].x=1.0; t.a[1].y=0.0;

        for (i=1; i<=q->n; ++i)
            {
            u.n=1;
            u.a[0].x=-(q->a[i].x); u.a[0].y=-(q->a[i].y);
            u.a[1].x=1.0; u.a[1].y=0.0;
            pol_mult(p,&u,&t);
            t.n=p->n;
            for (j=0; j<=t.n; ++j) { t.a[j].x=p->a[j].x; t.a[j].y=p->a[j].y; }
            }
        return(p);
        }

static void op_value()   /* POL V=P(X) */
        {
        int i;
        struct polynom pol,arg,val;
        char expr[LLENGTH];
//        extern struct polynom *pol_product();

        i=muste_pol_load(p,&pol); if (i<0) return;
        i=muste_pol_load(q,&arg); if (i<0) return;

        for (i=0; i<=arg.n; ++i)
            pol_value(&pol,&(arg.a[i]),&(val.a[i]));
        val.n=arg.n;
        sprintf(expr,"Values_of_polynomial_%s_on_%s",p,q);
        muste_pol_save2(xx,&val,"real    imag    ",1,expr);
        }

static void op_der()
        {
        int i;
        struct polynom pol,d;
        char expr[LLENGTH];
//        extern struct polynom *pol_der();

        i=muste_pol_load(q,&pol); if (i<0) return;
        pol_der(&d,&pol);
        sprintf(expr,"Derivative_of_%s",q);
        muste_pol_save2(xx,&d,"real    imag    ",0,expr);
        }

static int op_lag(char *s)
    {
    int i;
    struct polynom pol,d;
    char expr[LLENGTH];
//    extern struct polynom *pol_lag();
    int j;
    q=s+4;
    j=atoi(word[2]);
// Rprintf("\nxx=%s q=%s j=%d|",xx,q,j); getch();
    i=muste_pol_load(q,&pol); if (i<0) return(-1);

    pol_lag(&d,&pol,j);
    sprintf(expr,"Lag(%s,%d)",q,j);
    muste_pol_save2(xx,&d,"real    imag    ",0,expr);
    return(1);
    }

static void op_product()
        {
        int i;
        struct polynom pol,roots;
//        extern struct polynom *pol_product();

        i=muste_pol_load(q,&roots); if (i<0) return;
        pol_product(&pol,&roots);
        muste_pol_save(xx,&pol);
        }


void muste_pol(int argc,char *argv[])
        {
// RS REM        int i;
        char *t;

// RS ADD Variable init
mtx=0;
n_row_comments=0;
roots_eps=EPS;
roots_max_iter=MAX_ITER;

        s_init(argv[1]);
        if (g<2) { sur_print("\nIncomplete POL operation!"); WAIT; return; } // RS ADD
        strcpy(xx,word[1]);
        
        p=strchr(xx,'=');
        if (p==NULL)
            {
            sur_print("\n= missing in POL operation!");
            WAIT; return;
            }
        *p++=EOS;
        if (g>2)
            {
            if (strncmp(p,"LAG(",4)==0)
                { op_lag(p); return; } // 16.8.2006
            }

        q=strchr(p,'*');
        if (q!=NULL) { op_mult(); return; }
        q=strchr(p,'/');
        if (q!=NULL) { op_div(); return; }
        q=strchr(p,'+');
        if (q!=NULL) { op_add(); return; }
        q=strchr(p,'-');
        if (q!=NULL) { op_sub(); return; }

        q=strchr(p,'(');

        if (q!=NULL)
            {
            *q++=EOS;
            t=strchr(q,')');
            if (t==NULL)
                {
                sur_print("\n) is missing!");
                WAIT; return;
                }
            *t=EOS;
            muste_strupr(p);
            if (strncmp(p,"PROD",4)==0) { op_product(); return; }
            if (strncmp(p,"ROOT",4)==0) { op_roots(); return; }
            if (strncmp(p,"DER",3)==0) { op_der(); return; }

            op_value();  /* POL V=P(X) */
            }

        }

