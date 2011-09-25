/* t2test.c 9.2.2000/SM (9.2.2000)
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;
static FILE *tempfile;

static int n[3];
static int tulosrivi;
static int m;
static char aineisto[2][LNAME];
static char tempname[LNAME];
static int prind;
static double *x;
static int *v;
static char *ind;
static double *s,*s2;
static double *ss[2],*ss2[2];
static double *xx;
static double *ss_inv,*ss_apu,*ss_apu2; // vain TYPE=2
static double *ero;

static double t2_0,t2_1,t2_BF0;
static double p_hot,p_sim;
static double p_yao;

// static int rand_type;
// static double seed;
static int nn,nn1;
static int simumax=10000;
static int method;
static int fixed,orig_samples;
static char *method_text[2]={"Assuming equal cov.matrices",
                      "Assuming unequal cov.matrices"};

static int talleta(int k);
static int data_error();
static int laske_summat(double *s,double *s2);
static double T2();
static int print_t2_hot();
static int print_t2_yao();
static double T2_BF();
static int yao_test();
static int not_enough_memory();
static int eoutput(char *rivi);

extern double uniform_dev();

/***************************
char *specs0[]={ "SIMUMAX", "METHOD", "RAND", "RND", "SEED", "FIXED",
                 "RESULTS", "PRIND", "FAST", "!" };
char **specs=specs0;
*******************************/
extern double T2();
extern double T2_BF();
extern double sur_rand0();
extern double muste_cdf_f();

/*****************************
main(argc,argv)
int argc; char *argv[];
****************************/

void muste_t2test(char *argv)
        {
        int i,k;
        int l,l2;

//      if (argc==1) return(1);
        s_init(argv);
        if (g<3)
            {
            sur_print("\nUsage: T2TEST <data1>,<data2>,<output_line>");
            WAIT; return;
            }
        tulosrivi=0;
        if (g>3)
            {
            tulosrivi=edline2(word[3],1,1);
            if (tulosrivi==0) return;
            }

        simumax=10000;
        i=spec_init(r1+r-1); if (i<0) return;
        if ((i=spfind("RESULTS"))>=0) results=atoi(spb[i]);

    x=NULL;
    v=NULL;
    xx=NULL;
    ind=NULL;
    s=NULL;
    s2=NULL;
    ss[0]=NULL;
    ss2[0]=NULL;
    ss[1]=NULL;
    ss2[1]=NULL;
    ss_inv=NULL;
    ss_apu=NULL;
    ss_apu2=NULL;
    ero=NULL;

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        if ((i=spfind("SIMUMAX"))>=0) simumax=atol(spb[i]);

        method=1;
        if ((i=spfind("METHOD"))>=0) method=atoi(spb[i]);

        fixed=0;
        if ((i=spfind("FIXED"))>=0) fixed=atoi(spb[i]);
        orig_samples=1;
        spec_rnd();
        strcpy(tempname,etmpd); strcat(tempname,"SURVOHOT.TMP");
        tempfile=muste_fopen(tempname,"wb");
        if (tempfile==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s!",tempname);
            sur_print(sbuf); WAIT; return;
            }

        strcpy(aineisto[0],word[1]);
        i=data_read_open(aineisto[0],&d); if (i<0) return;
        i=mask(&d); if (i<0) return;
        m=d.m_act;
        if (m==0)
            {
            sur_print("\nNo active variables!");
            WAIT; return;
            }

        x=(double *)muste_malloc(m*sizeof(double));
        if (x==NULL) { not_enough_memory(); return; }
        v=(int *)muste_malloc(m*sizeof(int));
        if (v==NULL) { not_enough_memory(); return; }

        for (i=0; i<m; ++i) v[i]=d.v[i];

        sur_print("\n");
        talleta(1);   /* data 1 */
        data_close(&d);

        strcpy(aineisto[1],word[2]);
        i=data_read_open(aineisto[1],&d); if (i<0) return;
        i=mask(&d); if (i<0) return;

        if (d.m_act!=m) { data_error(); return; }
        for (i=0; i<m; ++i)
            {
            if (v[i]!=d.v[i]) {data_error(); return; }
            }

        talleta(2);   /* data 2 */
        data_close(&d);
        muste_fclose(tempfile);
        n[0]=n[1]+n[2];

        tempfile=muste_fopen(tempname,"rb");

        xx=(double *)muste_malloc(n[0]*m*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return; }

        fread(xx,sizeof(double),n[0]*(int)m,tempfile);
        muste_fclose(tempfile);

        ind=muste_malloc(n[0]);
        if (ind==NULL) { not_enough_memory(); return; }
        s=muste_malloc(m*sizeof(double));
        if (s==NULL) { not_enough_memory(); return; }
        s2=muste_malloc(m*m*sizeof(double));
        if (s2==NULL) { not_enough_memory(); return; }


        for (l=0; l<n[0]; ++l) ind[l]='1';
        laske_summat(s,s2);
// printf("s: %g %g\n",s[0],s[1]); getch();

        ss[0]=muste_malloc(m*sizeof(double));
        if (ss[0]==NULL) { not_enough_memory(); return; }
        ss2[0]=muste_malloc(m*m*sizeof(double));
        if (ss2[0]==NULL) { not_enough_memory(); return; }

        ss[1]=muste_malloc(m*sizeof(double));
        if (ss[1]==NULL) { not_enough_memory(); return; }
        ss2[1]=muste_malloc(m*m*sizeof(double));
        if (ss2[1]==NULL) { not_enough_memory(); return; }

        ss_inv=muste_malloc(m*m*sizeof(double));
        if (ss_inv==NULL) { not_enough_memory(); return; }
        ss_apu=muste_malloc(m*m*sizeof(double));
        if (ss_apu==NULL) { not_enough_memory(); return; }
        ss_apu2=muste_malloc(m*m*sizeof(double));
        if (ss_apu2==NULL) { not_enough_memory(); return; }
        ero=muste_malloc(m*sizeof(double));
        if (ero==NULL) { not_enough_memory(); return; }



// Todelliset otokset
        for (l=0; l<n[0]; ++l) ind[l]='0';
        for (l=0; l<n[1]; ++l) ind[l]='1';

        t2_0=T2();
        p_hot=1.0-muste_cdf_f((double)(n[0]-m-1)/(n[0]-2)/(double)m*t2_0,
                        (double)m,(double)(n[1]+n[2]-m-1),1e-15);

        if (method==2)
            {
            t2_BF0=T2_BF();
            yao_test();
            print_t2_yao();
            }
// printf("\nt2: %g %g",t2_0,t2_BF0); getch();
        else
            print_t2_hot();

        if (fixed) orig_samples=0;

        ++scroll_line;
        nn1=0L; k=0;
        if (simumax) sur_print("\nInterrupt by '.'");

        for (nn=1; nn<=simumax; ++nn)
            {
// Rprintf("\nnn=%d",nn);
            for (l=0; l<n[0]; ++l) ind[l]='0';
            for (l=0; l<n[1]; ++l)
                {
                while (1)
                    {
                    l2=n[0]*uniform_dev();
                    if (ind[l2]=='1') continue;
                    ind[l2]='1'; break;
                    }
                }

            if (method==1)
                {
                t2_1=T2();
// printf("t2: %g %g\n",t2_1,t2_0); getch();
                if (t2_1>t2_0) ++nn1;
                }
            else
                {
                t2_1=T2_BF();
// printf("t2: %g %g\n",t2_1,t2_BF0); getch();
                if (t2_1>t2_BF0) ++nn1;
                }

            ++k;
// printf("t2=%g\n",t2_1); i=getch(); if (i=='.') break;
/**********************************
            if (sur_kbhit())
                {
                i=sur_getch(); if (i=='.') break;
                prind=1-prind;
                }
***********************************/
            if (k>=1000 && prind)
                {
                sprintf(sbuf,"\n%d %g  ",nn,(double)nn1/(double)nn);
                sur_print(sbuf);
                k=0;
                }
            }
// Rprintf("4"); sur_getch();
        output_open(eout);
        --nn;
    if (method==1)
        {
        eoutput("Hotelling's two-sample test for equality of mean vectors:");
        sprintf(sbuf,"T2=%g p=%d n1=%d n2=%d P1=%g",
                    t2_0,m,n[1],n[2],p_hot);
        }
    else
        {
        eoutput("Yao's two-sample test for equality of mean vectors:");
        sprintf(sbuf,"T2=%g P1=%g (assuming nonequal cov.matrices)",
                    t2_BF0,p_yao);
        }
        eoutput(sbuf);
        if (simumax>0L)
            {
            eoutput("Randomization test:");
            p_sim=(double)nn1/(double)nn;
            sprintf(sbuf,"N=%d P=%g (s.e. %g) %s",
                nn,p_sim,sqrt(p_sim*(1-p_sim)/nn),method_text[method-1]);
            eoutput(sbuf);
            }
        output_close(eout);
        s_end(argv);
        return;
        }

// Hotelling's two-sample test for equality of mean vectors:
// T2=9.82294 p=2 n1=50 n2=50
// 1-F.F(p,n1+n2-p-1,(n1+n2-p-1)/(n1+n2-2)/p*T2)=0.00972

static int talleta(int k)
        {
        int i,h;
        int l;
        char *p;

        n[k]=0L;
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            for (i=0; i<m; ++i)
                {
                data_load(&d,l,v[i],&x[i]);
                if (x[i]==MISSING8) break;

                }
            if (i<m) continue;
//          if (prind)
//              {
//              sprintf(sbuf,"%d ",l); sur_print(sbuf);
//              }
            for (i=0; i<m; ++i)
                {
                p=(char *)&x[i];
                for (h=0; h<sizeof(double); ++h)
                    fputc((int)p[h],tempfile);
                }
            ++n[k];
            }
        return(1);
        }

static int data_error()
        {
        sprintf(sbuf,"\nData sets %s and %s must have same structures!",
                    word[1],word[2]);
        sur_print(sbuf); WAIT; return(1);
        }

static int laske_summat(double *s,double *s2)
        {
        int i,j;
        int l,r;

        rewind(tempfile);
        for (i=0; i<m; ++i) s[i]=0.0;
        if (s2!=NULL) for (i=0; i<m*m; ++i) s2[i]=0.0;

        for (l=0; l<n[0]; ++l)
            {
            if (ind[l]!='1') continue;
            r=l*m;
            for (i=0; i<m; ++i)
                {
                s[i]+=xx[i+r];

                if (s2!=NULL)
                    for (j=0; j<=i; ++j) s2[i+m*j]+=xx[i+r]*xx[j+r];
                }
            }
        return(1);
        }


static double T2()
        {
        int i,j;
        double t2,det;

        if (orig_samples)
            {
            laske_summat(ss[0],ss2[0]);

            for (i=0; i<m; ++i)
                {
                ss[1][i]=s[i]-ss[0][i];
                for (j=0; j<=i; ++j) ss2[1][i+m*j]=s2[i+m*j]-ss2[0][i+m*j];
                }
            }

        else
            laske_summat(ss[0],NULL);

        for (i=0; i<m; ++i)
            {
            ss[0][i]/=n[1];
            ss[1][i]/=n[2];
            }
// printf("ka:\n");
// mprint(ss[0],1,m);
// mprint(ss[1],1,m);

        if (orig_samples)
          {
          for (i=0; i<m; ++i)
              for (j=0; j<=i; ++j)
                  {
                  ss2[0][i+m*j]+=-n[1]*ss[0][i]*ss[0][j]
                                 +ss2[1][i+m*j]-n[2]*ss[1][i]*ss[1][j];
                  ss2[0][j+m*i]=ss2[0][i+m*j];
                  }

          for (i=0; i<m; ++i)
              for (j=0; j<=i; ++j)
                  {
                  ss2[0][i+m*j]/=n[0]-2;
                  ss2[0][j+m*i]=ss2[0][i+m*j];
                  }
// mprint(ss2[0],m,m);
          mat_inv(ss2[1],ss2[0],m,&det);
// mat_inv() ->  mat_gj() joka varaa 3 vektoritilaa, mutta vapauttaa ne



// mprint(ss2[1],m,m);
          }

        for (i=0; i<m; ++i) ss[0][i]-=ss[1][i];

        t2=0.0;
        for (i=0; i<m; ++i)
            {
            t2+=ss[0][i]*ss[0][i]*ss2[1][i*(m+1)];
            for (j=0; j<i; ++j)
                t2+=2*ss[0][i]*ss[0][j]*ss2[1][i+m*j];
            }
        t2*=(double)n[1]*(double)n[2]/(double)n[0];
        return (t2);
        }


static int print_t2_hot()
        {
        sprintf(sbuf,"Hotelling's T2=%g P=%g\n",t2_0,p_hot);
        sur_print(sbuf);
        return(1);
        }
static int print_t2_yao()
        {
        sprintf(sbuf,"Yao's test T2=%g P=%g\n",t2_BF0,p_yao);
        sur_print(sbuf);
        return(1);
        }

static double T2_BF()
        {
        int i,j;
        double t2,det;

        if (orig_samples)
            {
            laske_summat(ss[0],ss2[0]);
            for (i=0; i<m; ++i)
                {
                ss[1][i]=s[i]-ss[0][i];
                for (j=0; j<=i; ++j) ss2[1][i+m*j]=s2[i+m*j]-ss2[0][i+m*j];
                }

            for (i=0; i<m; ++i)
                for (j=0; j<=i; ++j)
                    {
                    ss_apu[i+m*j]=
                     (ss2[0][i+m*j]-ss[0][i]*ss[0][j]/n[1])/(n[1]*(n[1]-1))
                    +(ss2[1][i+m*j]-ss[1][i]*ss[1][j]/n[2])/(n[2]*(n[2]-1));
                    ss_apu[j+m*i]=ss_apu[i+m*j];
                    }

            mat_inv(ss_inv,ss_apu,m,&det);
// mprint(ss_inv,m,m);
            }
        else
            laske_summat(ss[0],NULL);

        for (i=0; i<m; ++i)
            {
            ss[0][i]/=n[1];
            ss[1][i]/=n[2];
            }
        for (i=0; i<m; ++i) ss_apu[i]=ss[0][i]-ss[1][i];

        t2=0.0;
        for (i=0; i<m; ++i)
            {
            t2+=ss_apu[i]*ss_apu[i]*ss_inv[i*(m+1)];
            for (j=0; j<i; ++j)
                t2+=2*ss_apu[i]*ss_apu[j]*ss_inv[i+m*j];
            }
        return (t2);
        }   /* T2_BF */

static int yao_test() // Srivastava s.119
    {
    int i,j,k;
    double a,dn,f,dm;

// printf("\nt2=%g",t2_BF0); getch();
/*******************
printf("\nkeskiarvot:");
mprint(ss[0],1,m);
mprint(ss[1],1,m);
mprint(ss2[0],m,m);
********************/

    for (i=0; i<m; ++i) ero[i]=ss[0][i]-ss[1][i];

    f=0.0;
    for (k=0; k<2; ++k)
        {
        for (i=0; i<m; ++i)
            for (j=0; j<=i; ++j)
                 {
          ss_apu2[i+m*j]=(ss2[k][i+m*j]-n[k+1]*ss[k][i]*ss[k][j])/(double)(n[k+1]-1);
          ss_apu2[j+m*i]=ss_apu2[i+m*j];
                 }
        mat_mlt(ss_apu,ss_inv,ss_apu2,m,m,m);
        mat_mlt(ss_apu2,ss_apu,ss_inv,m,m,m);

        mat_mlt(ss_apu,ss_apu2,ero,m,m,1);

        mat_mlt(ss_apu2,ero,ss_apu,1,m,1);

        dn=(double)n[k+1];
        a=ss_apu2[0];
        a=a*a/(t2_BF0*t2_BF0*dn*dn*(dn-1.0));
        f+=a;

        }
    f=1.0/f;

    dm=(double)m;
    p_yao=1.0-muste_cdf_f((f-dm+1.0)/(f*m)*t2_BF0,
                        dm,f-dm+1.0,1e-15);
// printf("\nf=%g P=%g",f,p_yao); getch();
    return(1);
    }
/*********************************
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
        printf("\n");
        getch();
        return(1);
        }
********************************/

static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (T2TEST)");
        WAIT; return(1);
        }

static int eoutput(char *rivi)
        {
        output_line(rivi,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

