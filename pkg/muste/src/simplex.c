#include "muste.h"
/* !simplex.c 29.9.1987/SM (1.4.1992) (2.10.1994)
    SIMPLEX A,m1,m2,m3,L
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static double *B;
static int mb,nb;
static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];
static int m,n,m1,m2,m3,mp,np,icase;
static double *A;
static int *iposv,*izrov;
static double eps=1e-6;
static int results_line;

static double *xx,*yy;
static int *l1,*l2,*l3;
static char *rlab2,*clab2;

static int not_enough_memory();
static int eoutput(char *line);
static int simplx(double *A,int m,int n,int mp,int np,
         int m1,int m2,int m3,int *izrov,int *iposv);
static int simp1(double *A,int mp,int np,int mm,int *ll,int nll,
                 int iabf,int *pkp,double *pbmax);
static int simp2(double *A,int m,int n,int mp,int np,
                 int *l2,int nl2,int *pip,int *pkp,double *pq1);
static int simp3(double *A,int mp,int np,int i1,int k1,int ip,int kp);
static int printout();
static char *spois(char *s);
static int save_tableau();
static int cop(char *lab2,int i,char *lab1,int j);
static int mat_del_row(int k,double *A,int m,int n,
                       char *rlab,char *clab,int lr,int lc);
static int mat_del_col(int k,double *A,int m,int n,
                       char *rlab,char *clab,int lr,int lc);

void muste_simplex(char *argv)
        {
        int i,j;

        s_init(argv);
        if (g<5)
            {
            sur_print("\nUsage: SIMPLEX A,m1,m2,m3,L");
            WAIT; return;
            }

        eps=1e-6;

    A=NULL;
    iposv=NULL;
    izrov=NULL;
    l1=NULL;
    l2=NULL;
    l3=NULL;
    xx=NULL;
    yy=NULL;
    rlab2=NULL;
    clab2=NULL;

    B=NULL;
    rlab=NULL;
    clab=NULL;
    rlab2=NULL;
    clab2=NULL;

        i=matrix_load(word[1],&B,&mb,&nb,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return;

        results_line=0;
        if (g>5)
            {
            results_line=edline2(word[5],1,1); if (!results_line) return;
            }
        m=mb-1; n=nb-1;

        m1=atoi(word[2]); m2=atoi(word[3]); m3=atoi(word[4]);
        if (mb-1!=m1+m2+m3)
            {
            sprintf(sbuf,"\n# of constraints %d+%d+%d not equal to row%s-1=%d",m1,m2,m3,
                                                        word[1],mb-1);
            sur_print(sbuf); WAIT; return;
            }

        mp=m+2; np=n+1;
        A=(double *)muste_malloc(mp*np*sizeof(double));
        if (A==NULL) { not_enough_memory(); return; }
        iposv=(int *)muste_malloc(m*sizeof(int));
        if (iposv==NULL) { not_enough_memory(); return; }
        izrov=(int *)muste_malloc(n*sizeof(int));
        if (izrov==NULL) { not_enough_memory(); return; }

        for (i=0; i<mb; ++i)
            for (j=0; j<nb; ++j) A[i+mp*j]=B[i+mb*j];

        icase=simplx(A,m,n,mp,np,m1,m2,m3,izrov,iposv);
/*
  Rprintf("\nicase=%d\n",icase);
  mprint(A,mp,np);
  Rprintf("\n"); for (i=0; i<m; ++i) Rprintf("%d ",iposv[i]);
  Rprintf("\n"); for (i=0; i<n; ++i) Rprintf("%d ",izrov[i]);  getch();
*/
        i=output_open(eout); if (i<0) return;
        switch (icase)
            {
          case  0: printout(); break;
          case -1: eoutput("No feasible solution"); WAIT; break;
          case -2: eoutput("Unbounded objective function"); WAIT; break;
          case -3: not_enough_memory(); return;
          case -4: eoutput("Bad input constraints count!"); WAIT; return;
          case -5: eoutput("Bad input tableau: Negative values in the first column!"); WAIT; break;
            }
        output_close(eout);
        s_end(argv);
        }

static int not_enough_memory()
        { sur_print("\nNot enough memory (SIMPLEX)"); WAIT; return(1); }

static int eoutput(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line; return(1);
        }


static int simplx(double *A,int m,int n,int mp,int np,
         int m1,int m2,int m3,int *izrov,int *iposv)
        {
        int nl2,nl1,m12,kp,kh,k,is,ir,ip,i;
        double q1,bmax;

        l1=l2=l3=NULL;
        l1=(int *)muste_malloc(np*sizeof(int));
        if (l1==NULL) return(-3);
        l2=(int *)muste_malloc(mp*sizeof(int));
        if (l2==NULL) return(-3);
        l3=(int *)muste_malloc(mp*sizeof(int));
        if (l3==NULL) return(-3);

        if (m!=m1+m2+m3) return(-4);  /* bad input constraints counts */
        nl1=n;
        for (k=1; k<=n; ++k) { l1[k-1]=k; izrov[k-1]=k; }
        nl2=m;
        for (i=1; i<=m; ++i)
            {
            if (A[i]<0) return(-5);  /* bad input tableau */
            l2[i-1]=i; iposv[i-1]=n+i;
            }
        for (i=1; i<=m2; ++i) l3[i-1]=1;
        ir=0;
        if (m2+m3==0) goto L30;
        ir=1;
        for (k=1; k<=n+1; ++k)
            {
            q1=0.0;
            for (i=m1+1; i<=m; ++i) q1+=A[i+mp*(k-1)];
            A[m+1+mp*(k-1)]=-q1;
            }
L10:
        simp1(A,mp,np,m+1,l1,nl1,0,&kp,&bmax);
/*      Rprintf("\nbmax=%g A[m+1]=%g",bmax,A[m+1]); getch();
        mprint(A,mp,np); getch();
*/
        if (bmax<=eps && A[m+1]<-eps) return(-1); /* no feasible solution */
        if (bmax<=eps && A[m+1]<=eps)
            {
            m12=m1+m2+1;
            if (m12<m)
                {
                for (ip=m12; ip<=m; ++ip)
                    {
                    if (iposv[ip-1]==ip+n)
                        {
                        simp1(A,mp,np,ip,l1,nl1,1,&kp,&bmax);
                        if (bmax>0) goto L1;
                        }
                    }
                }

            ir=0;
            --m12;
            if (m1+1>m12) goto L30;
            for (i=m1+1; i<=m12; ++i)
                {
                if (l3[i-m1-1]==1)
                    {
                    for (k=1; k<=n+1; ++k) A[i+mp*(k-1)]=-A[i+mp*(k-1)];
                    }
                }
            goto L30;
            }

        simp2(A,m,n,mp,np,l2,nl2,&ip,&kp,&q1);
        if (ip==0) return(-2); /* unbounded objective function */
L1:
        simp3(A,mp,np,m+1,n,ip,kp);
        if (iposv[ip-1]>=n+m1+m2+1)
            {
            for (k=1; k<=nl1; ++k)
                if (l1[k-1]==kp) break;
            --nl1;
            for (is=k; is<=nl1; ++is) l1[is-1]=l1[is];
            }
        else
            {
            if (iposv[ip-1]<n+m1+1) goto L20;
            kh=iposv[ip-1]-m1-n;
            if (l3[kh-1]==0) goto L20;
            l3[kh-1]=0;
            }
        A[m+1+mp*kp]+=1.0;
        for (i=1; i<=m+2; ++i)
            A[i-1+mp*kp]=-A[i-1+mp*kp];
L20:
        is=izrov[kp-1];
        izrov[kp-1]=iposv[ip-1];
        iposv[ip-1]=is;
        if (ir!=0) goto L10;



L30:
        simp1(A,mp,np,0,l1,nl1,0,&kp,&bmax);
        if (bmax<=0.0) return(0);
        simp2(A,m,n,mp,np,l2,nl2,&ip,&kp,&q1);
        if (ip==0) return(-2);
        simp3(A,mp,np,m,n,ip,kp);
        goto L20;

 //     muste_free(l1); muste_free(l2); muste_free(l3); // 31.10.2011
        return(0);

        }

static int simp1(double *A,int mp,int np,int mm,int *ll,int nll,
                 int iabf,int *pkp,double *pbmax)
        {
        int k;
        double test;

        *pkp=ll[0];
        *pbmax=A[mm+mp*(*pkp)];
        if (nll<2) return(1);
        for (k=2; k<=nll; ++k)
            {
            if (iabf==0) test=A[mm+mp*ll[k-1]]-*pbmax;
            else test=fabs(A[mm+mp*ll[k-1]])-fabs(*pbmax);
            if (test>0.0) { *pbmax=A[mm+mp*ll[k-1]]; *pkp=ll[k-1]; }
            }
        return(1);
        }

static int simp2(double *A,int m,int n,int mp,int np,
                 int *l2,int nl2,int *pip,int *pkp,double *pq1)
        {
        int k,ii,i;
        double qp,q0,q;

        *pip=0; qp=q0=0;
        if (nl2<1) return(1);
        for (i=1; i<=nl2; ++i)
            if (A[l2[i-1]+mp*(*pkp)]<-eps) break;
        if (i>nl2) return(1);
        *pq1=-A[l2[i-1]]/A[l2[i-1]+mp*(*pkp)];
        *pip=l2[i-1];
        if (i+1>nl2) return(1);
        for (i=i+1; i<=nl2; ++i)
            {
            ii=l2[i-1];
            if (A[ii+mp*(*pkp)]<-eps)
                {
                q=-A[ii]/A[ii+mp*(*pkp)];
                if (q<*pq1)
                    {
                    *pip=ii; *pq1=q;
                    }
                else if (q==*pq1)
                    {
                    for (k=1; k<=n; ++k)
                        {
                        qp=-A[*pip+mp*k]/A[*pip+mp*(*pkp)];
                        q0=-A[ii+mp*k]/A[ii+mp*(*pkp)];
                        if (q0!=qp) break;
                        }
                    if (q0<qp) *pip=ii;
                    }
                }
            }
        return(1);
        }

static int simp3(double *A,int mp,int np,int i1,int k1,int ip,int kp)
        {
        int kk,ii;
        double piv;

        piv=1/A[ip+mp*kp];
        if (i1>=0)
            {
            for (ii=1; ii<=i1+1; ++ii)
                {
                if (ii-1!=ip)
                    {
                    A[ii-1+mp*kp]*=piv;
                    for (kk=1; kk<=k1+1; ++kk)
                        if (kk-1!=kp)
                            A[ii-1+mp*(kk-1)]-=A[ip+mp*(kk-1)]*A[ii-1+mp*kp];
                    }
                }
            }
        for (kk=1; kk<=k1+1; ++kk)
            if (kk-1!=kp)
                A[ip+mp*(kk-1)]*=-piv;
        A[ip+mp*kp]=piv;
        return(1);
        }

/* simp2.c 4.10.1987/SM (18.10.1987) (2.10.1994)
*/

static int printout()
        {
  //    extern char *spois();
        char x[LLENGTH];
        char s[32],t[32];
        int i,k;
        double y;
        char *p;

        xx=NULL; yy=NULL;
        xx=(double *)muste_malloc(n*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return(-1); }
        if (m1+m2)
            {
            yy=(double *)muste_malloc((m1+m2)*sizeof(double));
            if (yy==NULL) { not_enough_memory(); return(-1); }
            }


        sprintf(x,"Simplex solution for Linear Programming problem %s:",word[1]); eoutput(x);
        sprintf(x,"Number of variables %d",m); eoutput(x);
        sprintf(x,"Number of '<' constraints %d",m1); eoutput(x);
        sprintf(x,"Number of '>' constraints %d",m2); eoutput(x);
        sprintf(x,"Number of '=' constraints %d",m3); eoutput(x);
        fnconv(A[0],accuracy,s);
        strncpy(t,rlab,lr); p=t+lr-1; while (p>t && *p==' ') { *p=EOS; --p; }
        sprintf(x,"Max.value=%s of %s obtained for: (SIMPLEX.M)",spois(s),spois(t)); eoutput(x);
        for (i=0; i<n; ++i)
            {
            for (k=0; k<m; ++k) if (iposv[k]==i+1) break;
            if (k<m) y=A[k+1]; else y=0.0;
            fnconv(y,accuracy,s); xx[i]=y;
            sprintf(x," %.*s %s",lc,clab+(i+1)*lc,s); eoutput(x);
            }
        matrix_save("SIMPLEX.M",xx,n,1,clab+lc,"simplex ",lc,8,-1,
                         "Simplex_solution",0,0);

        strcpy(x,"Values of slack variables: (SLACK.M)"); eoutput(x);

        if (m1+m2)
            {
            for (k=m; k<m+m1+m2; ++k)
                {
                y=B[k-m+1]; for (i=0; i<n; ++i) y+=xx[i]*B[k-m+1+(i+1)*mb];
                yy[k-m]=y;
                if (y<0.0 && y>-1e-10) y=0.0;
                fnconv(y,accuracy,s);
                sprintf(x," %.*s %s",lr,rlab+(k+1-m)*lr,s); eoutput(x);
                }
            matrix_save("SLACK.M",yy,m1+m2,1,rlab+lr,"slack   ",lr,8,-1,
                                 "Slack_values",0,0);
  //        muste_free(yy);
            }
 //     muste_free(xx);
        save_tableau();
        eoutput("");
        eoutput("MAT LOAD TSIMPLEX.M,CUR+1 / Simplex Output Tableau");
        return(1);
        }

static char *spois(char *s)
        {
        char *p;
        p=s; while (*p && *p==' ') ++p; return(p);
        }

static char lab0[]="????????";

static int save_tableau()
        {
        int n0;
        int i,j,k;

        rlab2=muste_malloc(8*mp);
        if (rlab2==NULL) { not_enough_memory(); return(-1); }
        clab2=muste_malloc(8*np);
        if (clab2==NULL) { not_enough_memory(); return(-1); }

        cop(rlab2,0,rlab,0);
        for (i=0; i<m; ++i)
            {
            k=iposv[i];
            if (k>n) cop(rlab2,i+1,rlab,k-n);
            else cop(rlab2,i+1,clab,k);
            }

        cop(clab2,0,clab,0);
        for (j=0; j<n; ++j)
            {
            k=izrov[j];
            if (k<=n) cop(clab2,j+1,clab,k);
            else
                {
                if (k>n+m1+m2) cop(clab2,j+1,lab0,0);
                else cop(clab2,j+1,rlab,k-n);
                }
            }

        mat_del_row(mp-1,A,mp,np,rlab2,clab2,8,8);

        n0=np;
        while (1)
            {
            for (i=1; i<n0; ++i)
                {
                if (strncmp(clab2+8*i,lab0,8)==0)
                    {
                    mat_del_col(i,A,mp-1,n0,rlab2,clab2,8,8);
                    --n0; break;
                    }
                }
            if (i==n0) break;
            }

        matrix_save("TSIMPLEX.M",A,mp-1,n0,rlab2,clab2,8,8,-1,
                         "Simplex_Output_Tableau",0,0);
        return(1);
        }

static int cop(char *lab2,int i,char *lab1,int j)
        {
        int k,g,h;

        k=8*i; g=8*j;
        for (h=0; h<8; ++h) lab2[k+h]=lab1[g+h];
/* Rprintf("\nlab=%.8s",lab2[k]); getch();  */
        return(1);
        }


static int mat_del_row(int k,double *A,int m,int n,
                       char *rlab,char *clab,int lr,int lc)
        {
        int i,j,g,h;

        g=h=0;
        for (j=0; j<n; ++j)
            {
            for (i=0; i<m; ++i)
                {
                if (i==k) ++h;
                else A[g++]=A[h++];
                }
            }

        if (k==m-1) return(1);
        for (i=k*lr; i<(m-1)*lr; ++i)
            rlab[i]=rlab[i+lr];
        return(1);
        }

static int mat_del_col(int k,double *A,int m,int n,
                       char *rlab,char *clab,int lr,int lc)
        {
        int i,j,g,h;

        if (k==n-1) return(1);
        g=h=0;
        for (j=0; j<n; ++j)
            {
            if (j==k) { h+=m; continue; }
            for (i=0; i<m; ++i)
                {
                A[g++]=A[h++];
                }
            }

        for (i=k*lc; i<(n-1)*lc; ++i)
            clab[i]=clab[i+lc];
        return(1);
        }

