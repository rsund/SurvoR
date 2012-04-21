/* !facta.c 13.11.1987/SM (6.5.1992) (3.5.1996)
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static double *E;
static int p,n;
static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];
static int k; /* number of factors */
static int ind;  /* 1=ULS, 2=GLS, 3=ML */
// static int is=0;
static double *L,*S,*v,*gg,*d,*psi,*facta_gamma,*om,*u,*delta;
static int maxtry=10;
static double eps=0.005;
static double epsu=0.01;
static double epse=0.1;
static int mor=0;
static double det,bnd,f0,f;
static int maxit=30;  /* ?? */
static char mess[80];
static int results_line;
/******************************
char *specs0[]={ "FEPS", "METHOD",
                  "!" };
char **specs=specs0;
******************************/
extern double sis_tulo();
double l_luku=0.0; /* mat */

extern char **spb;

static int varaa_tilat();
static int fnot_enough_memory();
static int print_line(char *x);
static int f_orientation(double *L,int p,int k);
static int text_labels(char *lab,int n,int len,char *text);
static int solve_symm2(double *x,double *a,double *b,int m,int k,double eps);
static int nwtrap();
static int iteration();
static int fctgr();
static int incpsi();
static int der2();
static int invalid_R();
static int appr_der2();

/****************
main(argc,argv)
int argc; char *argv[];
*******************/
void muste_facta(char *argv)
        {
        int i,j,h;
        unsigned int ui;
        double da,db;
        char x[LLENGTH];
        double sumlogsii;
//      extern double cdf_chi2();
        char acc[32];

//      if (argc==1) return;
        s_init(argv);

        if (g<3)
            {
            init_remarks();
            rem_pr("Usage: FACTA <corr.matrix>,k,L                ");
            rem_pr("       k=number of factors                    ");
            rem_pr("       L=first line for the results           ");
            rem_pr(" Factor matrix saved as FACT.M                ");
            rem_pr("                                              ");
            rem_pr(" METHOD=ULS Unweighted Least Squares          ");
            rem_pr(" METHOD=GLS Generalized Least Squares         ");
            rem_pr(" METHOD=ML  Maximum Likelihood (default)      ");
            rem_pr(" Test statistics by N=<number of observations>");
            rem_pr("More information by FACTA?                    ");

            wait_remarks(2);
            s_end(argv);
            return;
            }
        results_line=0;
        if (g>3)
            {
            results_line=edline2(word[3],1);
            if (results_line==0) return;
            }
        i=sp_init(r1+r-1); if (i<0) return;
        i=matrix_load(word[1],&E,&p,&n,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) { s_end(argv); return; } // RS CHA argv[1]

        if (p!=n)
            {
            sprintf(sbuf,"\n%s not a square matrix!",word[1]); sur_print(sbuf); WAIT; return;
            }
        k=atoi(word[2]);
        if (k<1 || k>p-1)
            {
            sprintf(sbuf,"Incorrect number (%d) of factors!",k);
            if (etu==2)
                {
                sprintf(tut_info,"___@6@FACTA@%s@",sbuf); s_end(argv); // RS CHA argv[1]
                return;
                }
            sur_print("\n"); sur_print(sbuf); WAIT; return;
            }

        *mess=EOS;
        i=spfind("FEPS");
        if (i>=0)
            {
            double eps;

            eps=1.0-atof(spb[i]);
            for (i=0; i<p; ++i)
            for (j=0; j<p; ++j) { if (i==j) continue; E[i+j*p]*=eps; }

            }
        ind=3; i=spfind("METHOD");
        if (i>=0)
            {
            if (muste_strcmpi(spb[i],"ULS")==0) ind=1;
            if (muste_strcmpi(spb[i],"GLS")==0) ind=2;
            }

        for (i=0; i<p; ++i)
            {
            if (E[i*(p+1)]!=0.0) continue;
            sprintf(sbuf,"Variable %.*s is a constant!",lr,rlab+i*lr);
            if (etu==2)
                {
                sprintf(tut_info,"___@1@FACTA@%s@",sbuf); s_end(argv); // RS CHA argv[1]
                return;
                }
            sur_print("\n"); sur_print(sbuf); WAIT; return;
            }

/*
        if (ind==1)
            {
            for (i=0; i<p; ++i)
                {
                if (fabs(E[i*(p+1)]-1.0)<0.0001) continue;
                Rprintf("\n%s is not a correlation matrix as supposed in ULS!",
                                word[1]); WAIT; return;
                }
            }
*/

        i=varaa_tilat(); if (i<0) return;
        for (ui=0; ui<p*p; ++ui) S[ui]=E[ui];
        sumlogsii=0.0; for (i=0; i<p; ++i) sumlogsii+=log(S[i*(p+1)]);

        i=nwtrap();
        if (i<0)
            {
            if (etu!=2)
                { sur_print("\nSolution not found!"); WAIT; return; }
            s_end(argv); // RS CHA argv[1]
            return;
            }
        h=output_open(eout); if (h<0) return;
        strcpy(x,"Factor analysis: ");
        switch (ind)
            {
          case 1: strcat(x,"Unweighted Least Squares (ULS) solution"); break;
          case 2: strcat(x,"Generalized Least Squares (GLS) solution"); break;
          case 3: strcat(x,"Maximum Likelihood (ML) solution"); break;
            }
        print_line(x);
        if (*mess)
            {
            strcpy(x,"                 "); strcat(x,mess);
            print_line(x);
            if (etu!=2) { WAIT; }
            }
        i=spfind("N");
        if (i>=0)
            {
            if (ind>1)
                {
                double n1,c0,chi20,d0,m0,ck,chi2k,dk,mk,rho,pr;
                char *q;

                n1=atof(spb[i]);
                if (n1<(double)k) { sur_print("\nIncorrect N!"); WAIT; return; }
                c0=n1-1.0-(2.0*p+5.0)/6.0;
                chi20=c0*(sumlogsii-log(det));
                d0=p*(p-1.0)/2.0;
                m0=chi20/d0;
                ck=c0-2.0*k/3.0;
                chi2k=ck*f0;
                dk=((p-k)*(p-k)-p-k)/2.0;
                mk=chi2k/dk;
                rho=(m0-mk)/(m0-1.0);
//              pr=cdf_chi2(chi2k,dk,1e-10);
                pr=0.0;

                sprintf(x,"factors=%d Chi^2=%g df=%d P=%5.3f reliability=%g",
                            k,chi2k,(int)dk,pr,rho);
                if (rho>1.0) { q=strstr(x,"rel"); *q=EOS; } /* 3.6.1995 */

                print_line(x);
                }
            else
                {
                double n1,uu,dk,pr;

                n1=atof(spb[i]);
                if (n1<(double)k) { sur_print("\nIncorrect N!"); WAIT; return; }
                for (i=0; i<p; ++i) for (j=0; j<i; ++j)
                    {
                    da=0.0;
                    for (h=0; h<k; ++h) da+=L[i+p*h]*L[j+p*h];
                    S[i+p*j]=da; S[j+p*i]=da;
                    }  /* Huom. S-diagonaali säilytetään */
                mat_dcholinv(S,p,&uu);
                uu=(n1-1.0)*log(uu/det);
                dk=((p-k)*(p-k)+p-k)/2.0;   /* ei sama kuin yllä! */
//              pr=cdf_chi2(uu,dk,1e-10);
          pr=0.0;
                sprintf(x,"factors=%d Chi^2=%g df=%d P=%5.3f",
                            k,uu,(int)dk,pr);
                print_line(x);
                }
            }
        output_close(eout);

        f_orientation(L,p,k);
        text_labels(clab,k,lc,"F");
        matrix_save("FACT.M",L,p,k,rlab,clab,lr,lc,0,"F",0,0);
        strncpy(clab+k*lc,"h^2     ",8);
        for (i=0; i<p; ++i)
            {
            da=0.0;
            for (j=0; j<k; ++j)
                {
                db=L[i+p*j];
                S[i+p*j]=db;
                da+=db*db;
                }
            S[i+p*k]=da;
            }
        strcpy(acc,"12.1234567890123456");
        acc[accuracy-1]=EOS;
        matrix_print(S,p,k+1,rlab,clab,lr,lc,p,k+1,NULL,NULL,acc,c3,
                        results_line,eout,"Factor matrix");
        s_end(argv);
        }


static int varaa_tilat()
        {
        int i;

        S=(double *)muste_malloc((p+1)*p*sizeof(double));  /* p+1: dcholinv varten */
        if (S==NULL) { fnot_enough_memory(); return(-1); }
        L=(double *)muste_malloc(p*k*sizeof(double));
        if (L==NULL) { fnot_enough_memory(); return(-1); }
        v=(double *)muste_malloc(p*sizeof(double));
        if (v==NULL) { fnot_enough_memory(); return(-1); }
        gg=(double *)muste_malloc(p*sizeof(double));
        if (gg==NULL) { fnot_enough_memory(); return(-1); }
        d=(double *)muste_malloc(p*sizeof(double));
        if (d==NULL) { fnot_enough_memory(); return(-1); }
        psi=(double *)muste_malloc(p*sizeof(double));
        if (psi==NULL) { fnot_enough_memory(); return(-1); }
        facta_gamma=(double *)muste_malloc(p*sizeof(double));
        if (facta_gamma==NULL) { fnot_enough_memory(); return(-1); }
        om=(double *)muste_malloc(p*p*sizeof(double));
        if (om==NULL) { fnot_enough_memory(); return(-1); }
        u=(double *)muste_malloc(p*sizeof(double));
        if (u==NULL) { fnot_enough_memory(); return(-1); }
        delta=(double *)muste_malloc(p*sizeof(double));
        if (delta==NULL) { fnot_enough_memory(); return(-1); }

        for (i=0; i<p; ++i) u[i]=0.0; /* 3.5.1996 */

        return(1);
        }

static int fnot_enough_memory()
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

static int f_orientation(double *L,int p,int k)
        {
        int i,j;
        double pos,neg,a;

        for (j=0; j<k; ++j)
            {
            pos=neg=0.0;
            for (i=0; i<p; ++i)
                {
                a=L[i+p*j];
                if (a<0) neg+=a*a; else pos+=a*a;
                }
            if (neg>pos) for (i=0; i<p; ++i) L[i+p*j]=-L[i+p*j];
            }
        return(1);
        }

static int text_labels(char *lab,int n,int len,char *text)
        {
        char *t,*p;
        int pit;
        char label[32];
        int i,j;

        if (*text=='"') t=text+1; else t=text;
        p=strchr(t,'"'); if (p!=NULL) *p=EOS;
        pit=strlen(t);


        for (i=0; i<n*len; ++i) lab[i]=' ';
        for (i=0; i<n; ++i)
            {
            snprintf(label,32,"%s%d",t,i+1);
            for (j=0; j<len; ++j)
                {
                if (label[j]==EOS) break;
                lab[i*len+j]=label[j];
                }
            }
        return(1);
        }


/* solves2 25.10.1986/SM (15.11.1987)
   solve_symm2 kuten solve_symm, mutta ei käytä ortholin1
   return 1, kun pos.def.
*/

static int solve_symm2(double *x,double *a,double *b,int m,int k,double eps)
        {
        int i,j,h;
        double *p;
        double s;

        p=(double *)muste_malloc(m*sizeof(double));
        if (p==NULL) { fnot_enough_memory(); return(-1); }

        for (i=0; i<m; ++i)             /* choldet1 */
            for (j=i; j<m; ++j)
                {
                s=a[i+m*j];
                for (h=i-1; h>=0; --h)
                    s-=a[j+m*h]*a[i+m*h];
                if (j==i)
                    {
                    if (s<eps)
                        {
                        muste_free(p);  return(-i);
                   /*   return(ortholin1(a,m,m,b,k,eps,x,0));   */
                        }
                    p[i]=1/sqrt(s);
                    }
                else a[j+m*i]=s*p[i];
                } /* j,i */

        for (j=0; j<k; ++j)              /* cholsol1 */
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
        muste_free(p);
        return(1);
        }


static int nwtrap()
        {
        int i;
        unsigned int ui;
        double apu;

        if (ind>1)
            {
            i=mat_dcholinv(S,p,&det);
/*  Rprintf("i=%d det=%g\n",i,det); getch(); */
            if (i!=1 /* || det<1e-40 */ )    /* -10 aikaisemmin */
                {
                sprintf(sbuf,"R not positive definite! Try FEPS=0.005 or greater.");
                if (etu==2)
                    {
                    sprintf(tut_info,"___@2@FACTA@%s@",sbuf); return(-1);
                    }
                sur_print("\n"); sur_print(sbuf); WAIT; return(-1);
                }
            mat_cholmove(S,p);

            bnd=log(eps);
            }
        f0=1.0E10;
        if (ind>1)
            {
            for (i=0; i<p; ++i) v[i]=log((1.0-(double)k/(2*p))/S[i*(p+1)] );
            }
        else
            {
            bnd=sqrt(eps);
            i=mat_dcholinv(S,p,&det);
            if (i!=1)
                for (i=0; i<p; ++i) v[i]=0.6*E[i*(p+1)];
            else
                {
                mat_cholmove(S,p);
                for (i=0; i<p; ++i) v[i]=sqrt((1.0-(double)k/(2*p))/S[i*(p+1)]);
                }
            for (ui=0; ui<p*p; ++ui) { apu=E[ui]; E[ui]=S[ui]; S[ui]=apu; }
            }
        sprintf(sbuf,"\nMax. %d iterations. Interrupt by '.'",maxit);
        sur_print(sbuf);

        i=iteration();
        return(i);
        }

static int iteration()
        {
        int i,j;
        int iter;

        mor=1; iter=0;

        while (1)
            {
            fctgr();
            ++iter;

            sprintf(sbuf,"\nIteration %d: f0=%g",iter,f0); sur_print(sbuf);
/*          Rprintf("\npsi^2:");
            for (i=0; i<p; ++i) Rprintf(" %5.3f",exp(v[i]));
    getch();
*/


            if (sur_kbhit())
                {
                i=sur_getch();
                if (i=='.')
                    {
                    if (etu==2)
                        {
                        strcpy(tut_info,"___@3@FACTA@Operation interrupted by the user!@");
                        return(-1);
                        }
                    }
                return(-1);
                }
            if (iter==maxit)
                {
                if (etu==2)
                    {
          strcpy(tut_info,"___@4@FACTA@No convergence! (Try with FEPS=0.005 or greater.)@");
                    return(-1);
                    }
                sur_print("\nNo convergence! (Try with FEPS=0.005 or greater.)"); WAIT; return(-1);
                }

            if (sur_kbhit())        /* 6.5.1992 */
                {
                i=sur_getch();
                if (i=='.')
                    {
                    sprintf(mess,"Interrupted by the user after %d iterations!",iter);
                    mor=0;
                    }
                }
            if (iter==maxit)
                {
                mor=0;
                sprintf(mess,"Interrupted after %d iterations!",iter);
                }

            if (mor==0)
                {
/*      Rprintf("\ngamma:");
        for (i=0; i<p; ++i) Rprintf(" %g",gamma[i]); getch();
*/
                if (ind>1)
                    {
                    for (i=0; i<p; ++i) psi[i]=sqrt(exp(v[i]));
                    for (i=0; i<k; ++i) d[i]=sqrt(1/facta_gamma[p-1-i]-1.0);
                    for (i=0; i<p; ++i) for (j=0; j<k; ++j)
                        L[i+p*j]=psi[i]*om[i+p*(p-1-j)]*d[j];
                    }
                else
                    {
                    for (i=0; i<p; ++i) for (j=0; j<k; ++j)
                        L[i+p*j]=om[i+p*j]*sqrt(facta_gamma[j]);
                    }
                return(1);
                }
            else { i=incpsi(); if(i<0) return(-1); }

            }
        return(1);
        }


static int fctgr()
        {
        int itry;
        int i,j;
        double da;

        itry=0;
        while (1)
            {
            if (ind>1)
                {
                for (i=0; i<p; ++i) psi[i]=sqrt(exp(v[i]));
                for (i=0; i<p; ++i)
                    for (j=0; j<p; ++j) E[i+p*j]=om[i+p*j]=psi[i]*psi[j]*S[i+p*j];

                mat_tred2(facta_gamma,psi,om,p,1e-300/1e-16);
                mat_tql2(facta_gamma,psi,om,p,1e-16,30);

                if (ind==2)
                    {
                    f=0.0;
                    for (i=0; i<p-k; ++i) f+=(facta_gamma[i]-1)*(facta_gamma[i]-1);
                    f/=2;
                    for (i=0; i<p; ++i) d[i]=facta_gamma[i]*(facta_gamma[i]-1.0);
                    }
                else /* ind=3 */
                    {
                    f=0.0;
                    for (i=0; i<p-k; ++i) f+=log(facta_gamma[i])+1/facta_gamma[i]-1;
                    for (i=0; i<p; ++i) d[i]=1-1/facta_gamma[i];
                    }

                /* 16 */
                for (i=0; i<p; ++i)
                    {
                    gg[i]=0.0; for (j=0; j<p-k; ++j) gg[i]+=d[j]*om[i+p*j]*om[i+p*j];
                    }
                }
            else
                {
                for (i=0; i<p; ++i) psi[i]=v[i];
                for (i=0; i<p; ++i)
                    for (j=0; j<p; ++j)
                        {
                        da=S[i+p*j];
                        if (i==j) da-=psi[i]*psi[i];
                        E[i+p*j]=om[i+p*j]=da;
                        }
                mat_tred2(facta_gamma,d,om,p,1e-300/1e-16);
                mat_tql2(facta_gamma,d,om,p,1e-16,30);
                f=0.0;
                for (i=k; i<p; ++i) f+=facta_gamma[i]*facta_gamma[i];
                f/=2;
                for (i=0; i<p; ++i)
                    {
                    gg[i]=0.0; for (j=k; j<p; ++j) gg[i]+=facta_gamma[j]*om[i+p*j]*om[i+p*j];
                    gg[i]*=-2*psi[i];
                    }
                }

            /* kohta 4 */
            if (f0<f)
                {
                ++itry; sprintf(sbuf," %d",itry); sur_print(sbuf);
                if (itry<maxtry)
                    {
/*     Rprintf("\nu&v: ");for (i=0; i<p; ++i) Rprintf("%g & %g ",u[i],v[i]); getch(); */
                    for (i=0; i<p; ++i) v[i]=0.5*(u[i]+v[i]);
                    continue;
                    }
                }

            f0=f;
            for (i=0; i<p; ++i) u[i]=v[i];
            break;
            } /* while */
/* Rprintf("\ngg:"); for (i=0; i<p; ++i) Rprintf(" %g",gg[i]); getch();
*/

        return(1);
        }

static int incpsi()
        {
        int i,j,h;
        int sn;

        if (mor==0) appr_der2(); else der2();
        sn=0;
        while (1)
            {
            for (i=0; i<p; ++i)
                {
                if (E[i*(p+1)]<epsu)
                    {
                    for (j=0; j<p; ++j)
                        if (j!=i) E[i+p*j]=E[j+p*i]=0.0;
                    }
                }
/*
printf("\nE:");
for (i=0; i<p*p; ++i) Rprintf(" %g",E[i]);
printf("\ngg:"); for (i=0; i<p; ++i) Rprintf(" %g",gg[i]); getch();

        matrix_save("F:H.MAT",E,p,p,rlab,clab,lr,lc,0,"H",0,0);
        matrix_save("F:G.MAT",gg,p,1,rlab,clab,lr,lc,0,"G",0,0);
getch();
*/

            i=solve_symm2(delta,E,gg,p,1,(double)1e-10);
     sprintf(sbuf," s=%d",i); sur_print(sbuf);
            if (i<0) ++sn; else sn=0;
            if (sn>10)
                {
                if (etu==2)
                    {
                    strcpy(tut_info,"___@4@FACTA@No convergence! Try FEPS=0.005 or greater.@");
                    }
                else
                    {
                    sur_print("\nNo convergence!"); WAIT;
                    }
                return(-1);
                }


            if (i!=1)
                {
                appr_der2();
                continue;
                }
/*          for (i=0; i<p; ++i) Rprintf(" %g",delta[i]); getch();
*/

            for (i=0; i<p; ++i) v[i]-=delta[i];


            /* kokeilu 18.1.89 */
            if (ind==3)
                {
                h=0;
                for (i=0; i<p; ++i) if (v[i]>0) { v[i]=0; h=1; }
                if (h)
                    {
                    sur_print(" Almost singular corr.matrix??? ");
                    }
                }


            i=0;
            for (i=0; i<p; ++i)
                {
                if (v[i]>bnd)
                    {
                    if (fabs(delta[i])<epse) continue;
                    mor=1;  /* mor=0 kulkukaaviossa */
                    return(1);
                    }
                else v[i]=bnd;
                }
            for (i=0; i<p; ++i)
                {
                if (v[i]<=bnd) continue;
                if (fabs(delta[i])>eps) { mor=k; return(1); }
                }
            mor=0;
            return(1);
            }
        return(1);
        }

static int der2()
        {
        int i,j,m,n;
        double da,db,da2;
        double ggg;

        switch (ind)
            {
          case 1:
            for (i=0; i<p; ++i) psi[i]=v[i];
            for (i=0; i<p; ++i) for (j=0; j<=i; ++j)
                {
                da=da2=0.0;
                for (m=k; m<p; ++m)
                    {
                    db=0.0;
                    for (n=0; n<k; ++n)
                        {
                        ggg=facta_gamma[m]-facta_gamma[n];
                        if (ggg==0.0) invalid_R();
                        db+=(facta_gamma[m]+facta_gamma[n])/ggg
                            *om[i+p*n]*om[j+p*n];
                        }
                    db*=om[i+p*m]*om[j+p*m];
                    da+=db;
                    if (i==j) da2+=(psi[i]*psi[i]-facta_gamma[m]/2.0)
                                   *om[i+p*m]*om[i+p*m];
                    }
                da*=psi[i]*psi[j];
                da=4*(da+da2);
                E[i+p*j]=da; E[j+p*i]=da;
                }
            break;
/*
          case 1:
            for (i=0; i<p; ++i) for (j=0; j<=i; ++j)
                {
                da=da2=0.0;
                for (m=k; m<p; ++m)
                    {
                    db=0.0;
                    for (n=0; n<k; ++n)
                        if (n!=m)
                        db+=1.0/(facta_gamma[m]-facta_gamma[n])
                            *om[i+p*n]*om[j+p*n];
                    db*=2*facta_gamma[m]*om[i+p*m]*om[j+p*m];
                    da+=db+om[i+p*m]*om[i+p*m]*om[j+p*m]*om[j+p*m];
                    if (i==j) da2+=facta_gamma[m]
                                   *om[i+p*m]*om[i+p*m];
                    }
                da*=4*psi[i]*psi[j];
                da=da-2*da2;
                E[i+p*j]=da; E[j+p*i]=da;
                }
            break;
*/
          case 2:
            for (i=0; i<p; ++i) psi[i]=sqrt(exp(v[i]));
            for (i=0; i<p; ++i) for (j=0; j<=i; ++j)
                {
                if (i==j) da=gg[i]; else da=0.0;
                for (m=0; m<p-k; ++m)
                    {
                    db=0.0;
                    for (n=p-k; n<p; ++n)
                        {
                        ggg=facta_gamma[m]-facta_gamma[n];
                        if (ggg==0.0) invalid_R();
                        db+=facta_gamma[n]*(facta_gamma[m]+facta_gamma[n]-2)/ggg
                            *om[i+p*n]*om[j+p*n];
                        }
                    db+=S[i+p*j]*psi[i]*psi[j];
                    da+=db*facta_gamma[m]*om[i+p*m]*om[j+p*m];
                    }
                E[i+p*j]=da; E[j+p*i]=da;
                }
            break;
          case 3:
            for (i=0; i<p; ++i) psi[i]=sqrt(exp(v[i]));
            for (i=0; i<p; ++i) for (j=0; j<=i; ++j)
                {
                if (i==j) da=-gg[i]; else da=0.0;
                for (m=0; m<p-k; ++m)
                    {
                    db=0.0;
                    for (n=p-k; n<p; ++n)
                        {
                        ggg=facta_gamma[m]-facta_gamma[n];
                        if (ggg==0.0) invalid_R();
                        db+=(facta_gamma[m]+facta_gamma[n]-2)/ggg
                            *om[i+p*n]*om[j+p*n];
                        }
                    if (i==j) db+=1.0;
                    da+=db*om[i+p*m]*om[j+p*m];
                    }
                E[i+p*j]=da; E[j+p*i]=da;
                }
            break;
            }
        return(1);
        }

static int invalid_R()
        {
        sprintf(sbuf,"Invalid correlation matrix for factor analysis!");
        if (etu==2)
            {
            sprintf(tut_info,"___@5@FACTA@%s@",sbuf); return(-1);
            }
        sur_print("\n"); sur_print(sbuf); WAIT; return(-1);
        }

static int appr_der2()
        {
        int i,j,m;
        double da;

        switch (ind)
            {
          case 1:
            for (i=0; i<p; ++i) psi[i]=v[i];
            for (i=0; i<p; ++i) for (j=0; j<=i; ++j)
                {
                da=0.0;
                for (m=k; m<p; ++m) da+=om[i+p*m]*om[j+p*m];
                da*=da; da*=4*psi[i]*psi[j]; E[i+p*j]=da; E[j+p*i]=da;
                }
            break;
          case 2: case 3:
            for (i=0; i<p; ++i) for (j=0; j<=i; ++j)
                {
                da=0.0;
                for (m=0; m<p-k; ++m) da+=om[i+p*m]*om[j+p*m];
                da*=da; E[i+p*j]=da; E[j+p*i]=da;
                }
            break;
            }
        return(1);
        }
