/* !corresp.c 4.4.1993/SM (9.11.1993)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;

static int tulosrivi;
static int prind;
static char aineisto[LNAME];
static int n;
static int m;
static double tot_sum;
static int *mv;
static double *x;
static double *A;
static double *sum;
static double chi2;
static double *sx,*ssum;

static int mm;
static int nn;
static int nc;  /* # of X coordinates */
static int *cv;
static int nu;  /* # of U columns (U from SVD) */
static int *uv;
static int na;  /* # of a columns (absolute contributions) */
static int *av;
static int ns;  /* # of s columns (squared correlations) */
static int *sv;
static int mvar;  /* Mass var. */
static int dvar;  /* Dist. var. */
static int nr;  /* # of residual columns */
static int *rv;
static int nss; /* # of supplementary variables */
static int *ssv;

static int show_usage();
static int varaa_tilat2();
static int valitse_muuttujat();
static int too_many_var(char ch,int n);
static int laskut1();
static int sum_is_0(int var);
static int eoutput(char *rivi);
static int not_enough_memory();
static int corresp();
static int varaa_tilat3();
static int tee_otsikot(char *lab,int *v,int nl);
static int y_talletus();
static int x_talletus();
static int x_tall(int l);
static int tulostus1();

/*****************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "RESULTS", "SUPPL_CASES", "!" };
char **specs=specs0;
*******************/
/**********
main(argc,argv)
int argc; char *argv[];
**************/

void muste_corresp(char *argv)
        {
        int i;

    //  if (argc==1) return;
        s_init(argv);

        if (g<2)
            {
            show_usage(); s_end(argv[1]);
            return;
            }

        tulosrivi=0;
        if (g>2)
            {
            tulosrivi=edline2(word[2],1,1);
            if (tulosrivi==0) return;
            }

        strcpy(aineisto,word[1]);
        i=data_open(aineisto,&d); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;
        i=valitse_muuttujat(); if (i<0) return;
        i=varaa_tilat2(); if (i<0) return;

        i=laskut1(); if (i<0) return;

        mm=n; nn=m;
        i=corresp(); if (i<0) return;
        i=tulostus1(); if (i<0) return;
        i=y_talletus(); if (i<0) return;
        i=x_talletus(); if (i<0) return;

        data_close(&d);
        s_end(argv);
        }

static int show_usage()
        {
        init_remarks();
        rem_pr("Usage: CORRESP <SURVO_data>,<output_line>     ");
        rem_pr("       N lines, M columns in data             ");
        rem_pr(" Mask   Task                                  ");
        rem_pr(" A      Variables (Columns) to be analyzed    ");
        rem_pr(" S      Supplementary variables (Columns)     ");
        rem_pr("     Output:                                  ");
        rem_pr(" C      Coordinates    max # is K=min(M,N)-1  ");
        rem_pr(" u      U from SVD                            ");
        rem_pr(" a      Absolute contributions (squares of U) ");
        rem_pr(" s      Squared correlations                  ");
        rem_pr(" r      Residuals of the original table       ");
        rem_pr(" m      Mass  (one variable)                  ");
        rem_pr(" d      Chi^2 distance from 0 (one variable)  ");
        rem_pr(" Supplementary cases (Rows) are indicated by  ");
        rem_pr(" SUPPL_CASES=<variable>,<l.limit>,<u.limit>   ");
        wait_remarks(2); return(1);
        }

static int varaa_tilat2()
        {
        int mm;                    /* 10.12.1999 */
        mm=m; if (nss>m) mm=nss;

/*
        if (m>90)
            {
            sprintf(sbuf,"\n# of `A' variables (columns) %d exceeds 90.",m);
            sur_print(sbuf);
            sur_print("\nCannot continue!"); WAIT; return(-1);
            }
*/
        A=(double *)malloc(m*m*sizeof(double));
        if (A==NULL) { not_enough_memory(); return(-1); }
        x=(double *)malloc(mm*sizeof(double));
        if (x==NULL) { not_enough_memory(); return(-1); }
        sum=(double *)malloc(mm*sizeof(double));
        if (sum==NULL) { not_enough_memory(); return(-1); }
        sx=(double *)malloc(mm*sizeof(double));
        if (sx==NULL) { not_enough_memory(); return(-1); }
        ssum=(double *)malloc(mm*sizeof(double));
        if (ssum==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int valitse_muuttujat()
        {
        int i;
        char ch;

        mv=(int *)malloc(d.m*sizeof(int));
        if (mv==NULL) { not_enough_memory(); return(-1); }
        cv=(int *)malloc(d.m*sizeof(int));
        if (cv==NULL) { not_enough_memory(); return(-1); }
        uv=(int *)malloc(d.m*sizeof(int));
        if (uv==NULL) { not_enough_memory(); return(-1); }
        av=(int *)malloc(d.m*sizeof(int));
        if (av==NULL) { not_enough_memory(); return(-1); }
        sv=(int *)malloc(d.m*sizeof(int));
        if (sv==NULL) { not_enough_memory(); return(-1); }
        rv=(int *)malloc(d.m*sizeof(int));
        if (rv==NULL) { not_enough_memory(); return(-1); }
        ssv=(int *)malloc(d.m*sizeof(int));
        if (ssv==NULL) { not_enough_memory(); return(-1); }

        m=0; nc=0; nu=0; na=0; ns=0; nr=0; nss=0;
        for (i=0; i<d.m; ++i)
            {
            ch=d.vartype[i][1];
            switch (ch)
                {
              case 'A': mv[m]=i; ++m; break;
              case 'C': cv[nc]=i; ++nc; break;
              case 'u': uv[nu]=i; ++nu; break;
              case 'a': av[na]=i; ++na; break;
              case 's': sv[ns]=i; ++ns; break;
              case 'r': rv[nr]=i; ++nr; break;
              case 'S': ssv[nss]=i; ++nss; break;
                }
            }
        mvar=activated(&d,'m');
        dvar=activated(&d,'d');
        if (m<2) { sur_print("\nAt least 2 variables (columns) must be selected by 'A'!");
                    WAIT; return(-1);
                  }
        if (nc>m-1) { too_many_var('C',nc); return(-1); }
        if (nu>m-1) { too_many_var('u',nu); return(-1); }
        if (na>m-1) { too_many_var('a',na); return(-1); }
        if (ns>m-1) { too_many_var('s',ns); return(-1); }

        return(1);
        }

static int too_many_var(char ch,int n)
        {
        sprintf(sbuf,"\n# of '%c' variables (now %d) must be less than # of 'A' variables (%d)!",
                                   ch, n, m);
        sur_print(sbuf); WAIT;
        return(1);
        }

static int laskut1()
        {
        int i,j,k;
        int l;
    //  double a1;

        n=0L; tot_sum=0.0;
        for (i=0; i<m; ++i)
            {
            sum[i]=0.0;
            for (j=0; j<=i; ++j)
                A[i+m*j]=0.0;
            }
        for (i=0; i<nss; ++i) ssum[i]=0.0;
        prind=1;
        sur_print("\n");
        for (l=d.l1; l<=d.l2; ++l)
            {
            double msum;

            if (unsuitable(&d,l)) continue;
            if (prind) { sprintf(sbuf,"%d",l); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
            ++n; msum=0.0;
            for (i=0; i<m; ++i)
                {
                k=data_load(&d,l,mv[i],&x[i]);
                if (k<0) return(-1);
                if (x[i]==MISSING8) break;
                msum+=x[i];
                }
            if (i<m) { sur_print("-"); continue; }
            for (i=0; i<nss; ++i)
                {
                k=data_load(&d,l,ssv[i],&sx[i]);
                if (k<0) return(-1);
                if (sx[i]==MISSING8)
                    {
         sur_print("\nMissing values in supplementary variables are not permitted!");
                    WAIT; return(-1);
                    }
                }
            for (i=0; i<nss; ++i) ssum[i]+=sx[i];

            if (msum==0.0)
                {
                sprintf(sbuf,"\nSum of 'A' variables is 0 in observation #%d.",l);
                sur_print(sbuf);
                sur_print("\nCannot continue!");
                WAIT; return(-1);
                }
            tot_sum+=msum;
            for (i=0; i<m; ++i)
                {
                sum[i]+=x[i];
                for (j=0; j<=i; ++j)
                    A[i+m*j]+=x[i]*x[j]/msum;
                }

            } /* l */

        for (i=0; i<m; ++i)
            if (sum[i]==0.0) { sum_is_0(mv[i]); return(-1); }
        for (i=0; i<nss; ++i)
            if (ssum[i]==0.0) { sum_is_0(ssv[i]); return(-1); }

        for (i=0; i<m; ++i)
            for (j=0; j<=i; ++j)
                {
                A[i+m*j]-=sum[i]*sum[j]/tot_sum;
                }

        for (i=0; i<m; ++i) x[i]=1/sqrt(sum[i]);
        for (i=0; i<m; ++i)
            for (j=0; j<=i; ++j)
                A[i+m*j]*=x[i]*x[j];

        chi2=0.0;
        for (i=0; i<m; ++i)
            {
            chi2+=A[i*(m+1)];
            for (j=0; j<=i; ++j)
                A[j+m*i]=A[i+m*j];
            }
        chi2*=tot_sum;

        return(1);
        }

static int sum_is_0(int var)
        {
        sprintf(sbuf,"\nSum of values of variable '%.8s' is 0.",
                                          d.varname[var]);
        sur_print(sbuf);
        sur_print("\nCannot continue!");
        WAIT; return(1);
        }
/****************************
matprint(A,m,n)
double *A;
int m,n;
        {
        int i,j;
        for (i=0; i<m; ++i)
            {
            printf("\n");
            for (j=0; j<n; ++j) printf("%g ",A[i+m*j]);
            }
        getch();
        }
***************************/
static int eoutput(char *rivi)
        {
        output_line(rivi,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(1);
        }

/* crp2.c 4.4.1993/SM (22.4.1993)
*/
static double *dd;
static double *ee;
static double *yy;
static double *cc;
static double tol=(1e-300)/(1e-16);
static char *lab1,*lab2,*lab;
static double *apu;
static double *ann;  /* nn*nn jos na tai nu tai ns > 0 */
static double *ss;   /* nss*nn jos nss > 0 */

static double sqrt_tot;
static char acc[21];

static int corresp()
        {
        int i,j;
        double a1;

        i=varaa_tilat3(); if (i<0) return(-1);
        i=mat_tred2(dd,ee,A,nn,tol); if (i<0) return(-1);
        i=mat_tql2(dd,ee,A,nn,1e-16,30); if (i<0) return(-1);
/*
matprint(A,nn,nn);
matprint(dd,1,nn);
*/
        a1=sqrt(tot_sum);
        for (i=0; i<nn; ++i) ee[i]=sqrt(fabs(dd[i]));
        for (i=0; i<nn; ++i)
            for (j=0; j<nn; ++j)
                yy[i+m*j]=a1*A[i+m*j]*x[i]*ee[j];
/* matprint(yy,nn,nn);  */
        tee_otsikot(lab1,mv,m);
        tee_otsikot(lab2,cv,nc);
        return(1);
        }

static int varaa_tilat3()
        {
        unsigned int k;

        dd=(double *)malloc(nn*sizeof(double));
        if (dd==NULL) { not_enough_memory(); return(-1); }
        ee=(double *)malloc(nn*sizeof(double));
        if (ee==NULL) { not_enough_memory(); return(-1); }
        yy=(double *)malloc(nn*nn*sizeof(double));
        if (yy==NULL) { not_enough_memory(); return(-1); }
        cc=(double *)malloc(nn*sizeof(double));
        if (cc==NULL) { not_enough_memory(); return(-1); }
        lab1=malloc(8*nn+1);
        if (lab1==NULL) { not_enough_memory(); return(-1); }
        lab2=malloc(8*nn+1);
        if (lab2==NULL) { not_enough_memory(); return(-1); }
        lab=malloc(8*nn+1);
        if (lab==NULL) { not_enough_memory(); return(-1); }
        apu=(double *)malloc(3*nn*sizeof(double));
        if (apu==NULL) { not_enough_memory(); return(-1); }

        if (nu || na || ns || mvar>=0 || dvar>=0)
            {
            ann=(double *)malloc(nn*nn*sizeof(double));
            if (ann==NULL) { not_enough_memory(); return(-1); }
            }
        if (nss)
            {
            ss=(double *)malloc(nss*nn*sizeof(double));
            if (ss==NULL) { not_enough_memory(); return(-1); }
            for (k=0; k<nss*nn; ++k) ss[k]=0.0;
            }

        return(1);
        }

static int tee_otsikot(char *lab,int *v,int nl)
        {
        int i,h;

        for (i=0; i<8*nl; ++i) lab[i]=' ';
        for (i=0; i<nl; ++i)
            {
            for (h=0; h<8; ++h)
                {
                if (d.varname[v[i]][h]==EOS) break;
                lab[8*i+h]=d.varname[v[i]][h];
                }
            }
        return(1);
        }

static int y_talletus()
        {
        int i,j;
        char expr[LLENGTH];
   //   char s[LLENGTH];
        double a1;

        strcpy(acc,"12.1234567890123456");
        acc[accuracy-1]=EOS;

        sur_print("\nSaving eigenvalues in CR_EIGEN.M:");
        strcpy(expr,"CR_EIGEN.M");
        matrix_save("CR_EIGEN.M",dd,nc,1,lab2,"eig.val.",8,8,-1,expr,0,0);

        sur_print("\nSaving column coordinates in CR_COORD.M:");
        strcpy(expr,"CR_COORD.M");
        matrix_save("CR_COORD.M",yy,m,nc,lab1,lab2,8,8,-1,expr,0,0);
        tulosrivi=matrix_print(yy,m,nc,lab1,lab2,8,8,m,nc,NULL,NULL,acc,c3,
                        tulosrivi,eout,"Column coordinates (CR_COORD.M)");
        if (nu>0)
            {
        tee_otsikot(lab,uv,nu);
        matrix_save("CR_V.M",A,m,nu,lab1,lab,8,8,-1,"CR_V.M",0,0);
        tulosrivi=matrix_print(A,m,nu,lab1,lab,8,8,m,nu,NULL,NULL,acc,c3,
                        tulosrivi,eout,"V in SVD (CR_V.M)");
            }
        if (na>0)
            {
        for (i=0; i<nn; ++i) for (j=0; j<nn; ++j)
            ann[i+nn*j]=100*A[i+nn*j]*A[i+nn*j];
        tee_otsikot(lab,av,na);
        matrix_save("CR_CONTR.M",ann,m,na,lab1,lab,8,8,-1,"CR_CONTR.M",0,0);
        tulosrivi=matrix_print(ann,m,na,lab1,lab,8,8,m,na,NULL,NULL,acc,c3,
                        tulosrivi,eout,"Absolute contributions % (CR_CONTR.M)");
            }
        if (ns>0)
            {
        for (i=0; i<nn; ++i)
            {
            a1=0.0; for (j=0; j<nn; ++j) a1+=yy[i+nn*j]*yy[i+nn*j];
            for (j=0; j<ns; ++j) ann[i+nn*j]=yy[i+nn*j]*yy[i+nn*j]/a1;
            }
        tee_otsikot(lab,sv,ns);
        matrix_save("CR_CORR2.M",ann,m,ns,lab1,lab,8,8,-1,"CR_CORR2.M",0,0);
        tulosrivi=matrix_print(ann,m,ns,lab1,lab,8,8,m,ns,NULL,NULL,acc,c3,
                        tulosrivi,eout,"Squared correlations (CR_CORR2.M)");
            }
        if (mvar>=0 || dvar>=0)
            {
            for (i=0; i<nn; ++i)
                {
                a1=0.0; for (j=0; j<nn; ++j) a1+=yy[i+nn*j]*yy[i+nn*j];
                ann[nn+i]=a1;
                ann[i]=sum[i]/tot_sum;
                }
            strcpy(lab,"Mass    Dist^2  ");
        matrix_save("CR_MDIST.M",ann,m,2,lab1,lab,8,8,-1,"CR_MDIST.M",0,0);
        tulosrivi=matrix_print(ann,m,2,lab1,lab,8,8,m,2,NULL,NULL,acc,c3,
                        tulosrivi,eout,"Masses and distances from 0 (CR_MDIST.M)");
            }

        return(1);
        }

static int x_talletus()
        {
        int i;
        int l;
        char s[LLENGTH],*osa[3];
        int cvar;
        double a1,a2,a3;


        for (i=0; i<nn; ++i) ee[i]=sqrt(sum[i]);
        sqrt_tot=sqrt(tot_sum);

        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            x_tall(l);
            }

        if (nss)
            {
        tee_otsikot(lab,ssv,nss);
        matrix_save("CR_SUPPC.M",ss,nss,nc,lab,lab2,8,8,-1,"CR_SUPPC.M",0,0);
        tulosrivi=matrix_print(ss,nss,nc,lab,lab2,8,8,nss,nc,NULL,NULL,acc,c3,
                        tulosrivi,eout,"Column coordinates of supplementary variables (CR_SUPPC.M)");

            }

        i=spfind("SUPPL_CASES");
        if (i<0) return(1);
        strcpy(s,spb[i]);
        i=split(s,osa,3);
        if (i==0)
            {
            sur_print("\nUsage: SUPPL_CASES=<indicator_variable>,<lower_limit>,<upper_limit>");
            WAIT; return(-1);
            }
        if (strcmp(osa[0],"ORDER")==0) cvar=-1;
        else
            {
            cvar=varfind(&d,osa[0]);
            if (i<0) return(-1);
            }
        a1=a2=1.0;
        if (i>1) a1=a2=atof(osa[1]);
        if (i>2) a2=atof(osa[2]);
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (cvar==-1)
                {
                if ((double)l<a1 || (double)l>a2) continue;
                }
            else
                {
                data_load(&d,l,cvar,&a3);
                if (a3==MISSING8 || a3<a1 || a3>a2) continue;
                }
            x_tall(l);
            }
        return(1);
        }

static int x_tall(int l)
        {
        int i,j,k;
        double a2;
        double msum,msum2;

        if (prind) { sprintf(sbuf,"%d",l); sur_print(sbuf); }
        if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
        msum=0.0;
        for (i=0; i<m; ++i)
            {
            k=data_load(&d,l,mv[i],&x[i]);
            if (k<0) return(-1);
            if (x[i]==MISSING8) break;
            msum+=x[i];
            }
        if (i<m) { sur_print("-"); return(1); }
        msum2=1/msum;

        for (i=0; i<m; ++i)
            x[i]-=sum[i]*msum/tot_sum;
        for (j=0; j<m; ++j)
            {
            a2=0.0;
            for (i=0; i<nn; ++i)
                {
                a2+=x[i]*A[i+m*j]/ee[i];
                }
            cc[j]=sqrt_tot*msum2*a2;
            }
/* matprint(cc,1,nn);  */
        for (i=0; i<nc; ++i)
            data_save(&d,l,cv[i],cc[i]);    /* Coordinates */

        if (nu>0)
            {
            for (i=0; i<nu; ++i)
                {
                a2=sqrt(msum/tot_sum/dd[i])*cc[i];
                data_save(&d,l,uv[i],a2);
                }
            }
        if (na>0)
            {
            for (i=0; i<na; ++i)
                {
                a2=cc[i];
                a2=100*msum/tot_sum*a2*a2/dd[i];
                data_save(&d,l,av[i],a2);
                }
            }
        if (ns>0)
            {
            a2=0.0; for (i=0; i<nn; ++i) a2+=cc[i]*cc[i];
            for (i=0; i<ns; ++i)
                {
                data_save(&d,l,sv[i],cc[i]*cc[i]/a2);
                }
            }

        if (mvar>=0)
            data_save(&d,l,mvar,msum/tot_sum);
        if (dvar>=0)
            {
            a2=0.0; for (i=0; i<nn; ++i) a2+=cc[i]*cc[i];
            data_save(&d,l,dvar,a2);
            }

        if (nr>0)
            {
            for (i=0; i<nr; ++i)
                {
                a2=0.0; for (j=nc; j<nn; ++j) a2+=cc[j]*A[i+m*j]*ee[i];
                a2*=msum/sqrt_tot;
                data_save(&d,l,rv[i],a2);
                }
            }

        if (nss)
            {
            for (i=0; i<nss; ++i)
                {
                data_load(&d,l,ssv[i],&sx[i]);
                for (j=0; j<nn; ++j)
                    {
                    ss[i+nss*j]+=sx[i]*cc[j]/ssum[i]/sqrt(dd[j]);
                    }
                }


            }
        return(1);
        } /* x_tall() */

static int tulostus1()
        {
        int i,k;
        char rivi[LLENGTH];
        double sum_eig,a1;
        char sana1[20],sana2[20];
        int df;
        extern double cdf_chi2();
        int ni;

        i=output_open(eout);  if (i<0) return(-1);

        sprintf(rivi,"Correspondence analysis on data %s: Rows=%d Columns=%d",
                                       word[1],n,m);
        eoutput(rivi);
        eoutput(" ");

        sprintf(rivi,"    Canonical   %.*sEigen-    %.*sChi^2 %.*sCumulative",
                      accuracy-4,space,accuracy-0,space,accuracy-4,space);
        eoutput(rivi);
        sprintf(rivi,"    correlation %.*svalue     %.*s      %.*spercentage",
                      accuracy-4,space,accuracy-0,space,accuracy-4,space);
        eoutput(rivi);
        sum_eig=0.0; for (i=0; i<nn; ++i) sum_eig+=dd[i];
        a1=0.0;

        ni=nc+3; if (ni>nn-1) ni=nn-1;
        for (i=0; i<ni; ++i)
            {
            if (dd[i]<0.00001) break;
            fnconv(fabs(sqrt(dd[i])),accuracy,sana1);
            fnconv(dd[i],accuracy,sana2);
            k=sprintf(rivi,"%2d %.*s        %.*s",
                             i+1, accuracy,sana1,accuracy,sana2);

            fnconv(chi2*dd[i]/sum_eig,accuracy+4,sana1);
            a1+=dd[i];
            fconv(100*a1/sum_eig,"123.12",sana2);
            k+=sprintf(rivi+k,"     %.*s      %s",accuracy+4,sana1,sana2);
            eoutput(rivi);
            }
        if (ni<nn-1) eoutput(" ...");
        fnconv(sum_eig,accuracy,sana1);
        fnconv(chi2,accuracy+3,sana2);
        sprintf(sana2,"%*g",accuracy,chi2);
        k=sprintf(rivi,"    %.*s       %.*s     %.*s",
                      accuracy,space,accuracy,sana1,accuracy+4,sana2);
        df=(int)(nn-1L)*(mm-1L);
        k=sprintf(rivi+k," (df=%d P=%g)",df,1.0-muste_cdf_chi2(chi2,(double)df,1e-15));
        eoutput(rivi);
        eoutput(" ");
        output_close(eout);
        return(1);
        }

