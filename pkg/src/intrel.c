/* !intrel.c 21.2.1997/SM (23.2.1997) (4.9.1998)
*/
#include <stdio.h>
#include <stdlib.h>
// RS REM #include <conio.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define N 501

static double x[N];
static double A[N][N];
static double B[N][N];
static double H[N][N-1];
static double s[N],y[N];
static double gamma_intrel,t,t0,t1,t2,t3,t4;
static int p[N];

static double luku;
static double eps;
static int n; /* asteluku */
static int results_line;
static int check;

static double *aa;

static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];


static char mat_name1[LNAME];
static int n_const;

static int intrel(int n);
static double nint(double x);
static int i_printout(int k);

// op_intrel()
void muste_intrel(char *argv)
        {
        int i,m;
        char *p;
        char luku1[LNAME];

        s_init(argv);
// RS REM Rprintf("1");
        i=spec_init(r1+r-1); if (i<0) return;
// RS REM Rprintf("2");
        if (g>1)
            {
            i=spfind(word[1]);
            if (i>=0) strcpy(luku1,spb[i]); else strcpy(luku1,word[1]);
            luku=atof(luku1); p=strchr(luku1,'.');
            }
        if (g<2 || p==NULL)
            {
            init_remarks();
rem_pr("INTREL <decimal_number>,L");
rem_pr("tries to find an exact numeric expression for which <decimal_number>");
rem_pr("is an (accurate) approximation.");
rem_pr("The PSLQ algorithm by Ferguson and Plouffe (1992) is used.");
rem_pr("The main approach is to see <decimal_number> as a root X of an");
rem_pr("algebraic equation of nth degree");
rem_pr("    C0+C1*X+C2*X^2+...+Cn*X^n=0");
rem_pr("with integer coefficients C0,C1,C2,...,Cn.");
rem_pr("The maximum degree n is set by a specification DEGREE=n (n=1,2,...,20).");
rem_pr("The accuracy of approximation is set by EPS, default EPS=1e-12 .");
rem_pr("L is the first line for the results (default is CUR+1).");
rem_pr(".......................................................................");
rem_pr("Example: ACCURACY=16");
rem_pr("sqrt(2)=1.4142135623731 DEGREE=2");
rem_pr("INTREL 3.4142135623731");
rem_pr("X=3.4142135623731 is a root of X^2-4*X+2=0");
rem_pr(".......................................................................");

            wait_remarks(1);


rem_pr("By giving a specification CONSTANTS=<matrix_file> values of the first");
rem_pr("column, say X1,X2,..., are used instead of powers of X.");
rem_pr("Example on the next page:");

            wait_remarks(1);

rem_pr(".......................................................................");
rem_pr("Example:");
rem_pr("MATRIX C");
rem_pr("///    C");
rem_pr("1      1");
rem_pr("e      2.718281828459045");
rem_pr("Pi     3.141592653589793");
rem_pr("");
rem_pr("MAT SAVE C");
rem_pr("");
rem_pr("x=5+2*3.141592653589793-3*exp(1)");
rem_pr("x=3.128339821802451");
rem_pr(".......................................................................");
rem_pr("CONSTANTS=C");
rem_pr("INTREL 3.128339821802451");
rem_pr("Integer relation for X=3.128339821802451:");
rem_pr("Constant  Coefficient");
rem_pr("X                  1");
rem_pr("1                 -5");
rem_pr("e                  3");
rem_pr("Pi                -2");
rem_pr(".......................................................................");

            wait_remarks(2);
            return;
            }

        results_line=r1+r;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }

        n=2;
        i=spfind("DEGREE");
        if (i>=0) n=atoi(spb[i]);
        if (n>N-1) { sprintf(sbuf,"\nmax DEGREE=%d",N-1);
                       sur_print(sbuf); WAIT; return;
                 }
        ++n;
        eps=1e-12;
        i=spfind("EPS");
        if (i>=0) eps=atof(spb[i]);

        check=0;
        i=spfind("CHECK");
        if (i>=0) check=atoi(spb[i]);
        n_const=0;
        i=spfind("CONSTANTS");
        if (i<0)
            {
            x[1]=1.0; for (i=2; i<=n; ++i) x[i]=luku*x[i-1];
            }
        else
            {
// printf("X");
// printf("\nspb=%s|",spb[i]);
            strcpy(mat_name1,spb[i]);
            i=matrix_load(mat_name1,&aa,&n,&m,&rlab,&clab,&lr,&lc,&type,expr);
            if (i<0) return;
            x[1]=luku;
            for (i=2; i<=n+1; ++i) x[i]=aa[i-2];
            ++n; n_const=n;
            }
/*
printf("n=%d\n",n);
for (i=1; i<=n; ++i) printf("%g\n",x[i]); getch();
*/
// RS REM Rprintf("A");
        i=intrel(n);
// RS REM Rprintf("B");
        i_printout(i);
        s_end(argv);
        }

static int intrel(int n)
        {
        int i,j,k,m=0,g;
        double da,db,dg;

        gamma_intrel=sqrt(4.0/3.0);
        for (i=1; i<=n; ++i)
            {
            for (j=1; j<=n; ++j) A[i][j]=B[i][j]=0.0;
            A[i][i]=B[i][i]=1.0;
            }
/******************
printf("\n");
for (i=1; i<=n; ++i) printf("%g ",x[i]); getch();
********************/
        da=0.0;
        for (k=n; k>=1; --k)
            {
            da+=x[k]*x[k];
            s[k]=sqrt(da);
            }

        da=s[1];
        for (k=1; k<=n; ++k) { y[k]=x[k]/da; s[k]/=da; }

        for (i=1; i<=n; ++i)
            {
            for (j=i+1; j<=n-1; ++j) H[i][j]=0.0;
            if (i<=n-1) H[i][i]=s[i+1]/s[i];
            for (j=1; j<=i-1; ++j) H[i][j]=-y[i]*y[j]/(s[j]*s[j+1]);
            }

        for (i=2; i<=n; ++i)
            {
            for (j=i-1; j>=1; --j)
                {
                t=nint(H[i][j]/H[j][j]);
                y[j]+=t*y[i];
                for (k=1; k<=j; ++k) H[i][k]-=t*H[j][k];
                for (k=1; k<=n; ++k) { A[i][k]-=t*A[j][k]; B[k][j]+=t*B[k][i]; }
                }
            }

        while (1)
            {
            da=-1.0; dg=1.0;
            for (i=1; i<=n-1; ++i)
                {
                dg*=gamma_intrel;
                db=dg*fabs(H[i][i]);
                if (db>da) { da=db; m=i; }
                }

            da=y[m]; y[m]=y[m+1]; y[m+1]=da;
            for (i=1; i<=n; ++i)
                {
                da=A[m][i]; A[m][i]=A[m+1][i]; A[m+1][i]=da;
                da=H[m][i]; H[m][i]=H[m+1][i]; H[m+1][i]=da;
                da=B[i][m]; B[i][m]=B[i][m+1]; B[i][m+1]=da;
                }

            if (m<=n-2)
                {
                t0=sqrt(H[m][m]*H[m][m]+H[m][m+1]*H[m][m+1]);
                t1=H[m][m]/t0; t2=H[m][m+1]/t0;
                for (i=m; i<=n; ++i)
                    {
                    t3=H[i][m]; t4=H[i][m+1];
                    H[i][m]=t1*t3+t2*t4;
                    H[i][m+1]=-t2*t3+t1*t4;
                    }
                }

            for (i=m+1; i<=n; ++i)
                {
                g=i-1; if (m+1<g) g=m+1;
                for (j=g; j>=1; --j)
                    {
                    t=nint(H[i][j]/H[j][j]);
                    y[j]+=t*y[i];
                    for (k=1; k<=j; ++k) H[i][k]-=t*H[j][k];
                    for (k=1; k<=n; ++k)
                        {
                        A[i][k]-=t*A[j][k]; B[k][j]+=t*B[k][i];
                        }
                    }
                }

            da=fabs(y[1]); m=1;
            for (i=2; i<=n; ++i)
                {
                if (fabs(y[i])<da) { da=fabs(y[i]); m=i; }
                }

            db=fabs(A[1][1]);
            for (i=1; i<=n; ++i) for (j=1; j<=n; ++j)
                { dg=fabs(A[i][j]); if (dg>db) db=dg; }

            if (check)
                Rprintf("\nm=%d ymin=%g maxA=%f",m,y[m],db); // RS CHA printf -> Rprintf

            if (db>9e+15) return(-1);

            if (da<eps || db>9e+15)
                {
                for (i=1; i<=n; ++i) p[i-1]=B[i][m];
                if (!check) return(1);
                printf("\n");
                for (i=1; i<=n; ++i)
                    printf("%g ",B[i][m]);
                i=sur_getch();
                if (i!='.') continue;
                return(1);
                }

            } /* while */
        return(1);
        }

static double nint(double x)
        {
        double b;

        if (x>=0.0)
            {
            b=floor(x); if (x-b<0.5) return(b); else return(b+1.0);
            }
        b=floor(-x); if (-x-b<0.5) return(-b); else return(-b-1.0);
        }

static int i_printout(int k)
        {
        int m,i,j;
        char s1[16],s2[16];
        double a;
        char *q;
// RS REM Rprintf("\nIntrel Print");
        if (k<0)
            {
            edwrite(space,results_line,1);
            edwrite("Solution not found!",results_line,1);
            return(1);
            }

        if (n_const)
            {
            edwrite(space,results_line,1);
            sprintf(sbuf,"Integer relation for X=%s:",word[1]);
            edwrite(sbuf,results_line++,1);
            edwrite(space,results_line,1);
            edwrite("Constant  Coefficient",results_line++,1);
            edwrite(space,results_line,1);
            sprintf(sbuf,"X           %8d",p[0]);
            edwrite(sbuf,results_line++,1);
            for (i=1; i<n; ++i)
                {
                edwrite(space,results_line,1);
                *s1=EOS; strncat(s1,rlab+lr*(i-1),lr);
                q=s1; while (*q==' ') ++q;
                for (j=strlen(q); j<8; ++j) q[j]=' '; q[j]=EOS;
                sprintf(sbuf,"%s    %8d",q,p[i]);
                edwrite(sbuf,results_line++,1);
                }
            a=p[0]*luku;
            for (i=1; i<n; ++i) a+=p[i]*aa[i-1];
            sprintf(sbuf,"scalar product %e",a);
            edwrite(space,results_line,1);
            edwrite(sbuf,results_line++,1);
            edwrite(space,results_line,1);
            return(1);
            }

        m=n-1;
        if (p[m]<0.0) for (i=0; i<=m; ++i) p[i]=-p[i];

        if (m==1)
            {
            p[0]=-p[0];
            if (fabs((double)p[0]/(double)p[1]-atof(word[1]))>1e-10)
                strcpy(sbuf,"Solution not found!");
            else
                sprintf(sbuf,"X=%s is %d/%d",word[1],p[0],p[1]);
            edwrite(space,results_line,1);
            edwrite(sbuf,results_line,1);
            return(1);
            }
        j=sprintf(sbuf,"X=%s is a root of ",word[1]);
        for (i=m; i>0; --i)
            if (p[i])
                {
                *s1=EOS; if (p[i]==1) { if (i<m) strcpy(s1,"+"); }
                else if (p[i]==-1) strcpy(s1,"-");
                else if (p[i]>1 && i==m) sprintf(s1,"%d*",p[i]);
                else sprintf(s1,"%+d*",p[i]);
                if (i==1) strcpy(s2,"X");
                else sprintf(s2,"X^%d",i);
                j+=sprintf(sbuf+j,"%s%s",s1,s2);
                }
        sprintf(sbuf+j,"%+d=0",p[0]);
        edwrite(space,results_line,1);
        edwrite(sbuf,results_line,1);
        return(1);
        }

