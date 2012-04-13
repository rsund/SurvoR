/* !distv.c 14.12.1994/SM (19.12.1994) (1.4.1995) (13.6.1997)
*/

#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define BINARY 0
#define EUCLID 1
#define CITY   2
#define MINKOWSKI 3
#define CANBERRA 4
#define BRAY_CURTIS 5
#define BHATTA 6
#define ANGULAR 7
#define CORREL 8

// char *measures[]={ "BINARY", "EUCLID", "CITY", "MINKOWSKI" "CANBERRA",
//                     "BRAY_CURTIS", "BHATTA", "ANGULAR", "CORREL" };

static int measure;
static char coeff[LLENGTH];
static SURVO_DATA d;
static char aineisto[LNAME];
static int painomuuttuja;
static int m;
static int n;
static double *binlimit;
static int *bindir;
static double *na,*nb,*nc,*nn;
static double *xx;
static int *xxx;
static char **varname;
static double *dd;
static char *lab;
static int cov_ind;
static int scaling;
static char measure_name[32];
static double power2;
static FILE *data;
static char tmpdata[LNAME];
static double *mean,*stddev,*cov;

static int varaa_tilat();
static int kokoa_tiedot();
static int laske_matriisi();
static int load_cases();
static int open_data(char *mode);
static int laske_matriisi2();
static int not_enough_memory();
static int mat_talletus();
static int remarks();
static int etsi_coeff();
static int binlimits();

static int l_virhe;
extern char **spb,**spa;
extern double *arvo;
extern int spn;

static int laske(char *lauseke,double *y);
static double luku(char *sana,int len);
static double oper(double x1,double x2,char laji);
static double power(double x,double y);
static int supista(int *t,double opnd[],char op[],int v[]);
static double funktio(char *s,double x);
static int f_tuntematon(char *s);
static int syntax_error(char *s);
static int laske2(char *muuttuja,double *y);


/************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "RESULTS", "PRIND", "MEASURE", "SCALING", "WEIGHTS",
                 "COEFF", "BINARY", "!" };

char **specs=specs0;
****************************/
// extern int spn;
// extern char **spb2;
/*********************
main(argc,argv)
int argc; char *argv[];
******************/

void muste_distv(char *argv)
        {
        int i,k;
        char x[LLENGTH];
        char *p;

  //    if (argc==1) return(-1);
        s_init(argv);

        if (g<2)
            {
            remarks(); s_end(argv); return;
            }

        l_virhe=0;
        strcpy(aineisto,word[1]);
        i=data_read_open(aineisto,&d); if (i<0) return;
        i=sp_init(r1+r-1,4); if (i<0) return;

     // if (spec_check) i=spec_word_dist(spec_check); // 11.7.2004

        i=mask(&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;

        painomuuttuja=activated(&d,'W');
        if (painomuuttuja>=0)
            {
            k=0;
            for (i=0; i<d.m; ++i)
                if (d.vartype[i][1]!='-' && d.vartype[i][1]!='W')
                    d.v[k++]=i;
            d.m_act=k;
            }
        m=d.m_act;

        measure=EUCLID;
        cov_ind=0;
        scaling=0;
        strcpy(measure_name,"Euclidean");
        i=spfind("MEASURE");
        if (i>=0)
            {
            *x=EOS; strncat(x,spb[i],3);
            muste_strupr(x); strcpy(measure_name,spb[i]);
            if (strcmp(x,"BIN")==0) { measure=BINARY; cov_ind=0; }
            else if (strcmp(x,"EUC")==0) { measure=EUCLID; cov_ind=0; }
            else if (strcmp(x,"CIT")==0) { measure=CITY; cov_ind=0; }
            else if (strcmp(x,"MIN")==0)
                {
                measure=MINKOWSKI; cov_ind=0;
                power2=1.0;
                p=strchr(measure_name,'(');
                if (p==NULL)
                    {
                    sprintf(sbuf,"\nThe index (k) missing in %s(k)",measure_name);
                    sur_print(sbuf); WAIT; return;
                    }
                else
                    power2=atof(p+1);
                if (power2==1.0)
                    {
                    measure=CITY;
                    }
                }
            else if (strcmp(x,"CAN")==0) { measure=CANBERRA; cov_ind=0; }
            else if (strcmp(x,"BRA")==0) { measure=BRAY_CURTIS; cov_ind=0; }
            else if (strcmp(x,"BHA")==0) { measure=BHATTA; cov_ind=0; }
            else if (strcmp(x,"ANG")==0) { measure=ANGULAR; cov_ind=0; }
            else if (strcmp(x,"COR")==0) { measure=CORREL; cov_ind=1; }
            else
                {
                sprintf(sbuf,"\nUnknown MEASURE=%s",measure_name);
                sur_print(sbuf); WAIT; return;
                }
            }

        i=spfind("SCALING");
        if (i>=0)
            {
            strcpy(x,spb[i]); muste_strupr(x);
            if (strncmp(x,"Y",1)==0) scaling=1;
            else if (strncmp(x,"N",1)==0) scaling=0;
            else scaling=atoi(x);
            }

        i=varaa_tilat(); if (i<0) return;

        if (measure==BINARY)
            {
            i=etsi_coeff();
            if (i<0) return;
            i=binlimits();
            if (i<0) return;
            i=kokoa_tiedot(); if (i<0) return;
            i=laske_matriisi(); if (i<0) return;
            }
        else

            {
            i=load_cases(); if (i<0) return;
            i=laske_matriisi2(); if (i<0) return;
            }

        for (i=0; i<m; ++i) varname[i]=d.varname[d.v[i]];
        mat_talletus();
        s_end(argv);
        }


static int varnimet()
        {

        spa[0]="a"; spa[1]="b"; spa[2]="c"; spa[3]="d";
        spb[0]=spb[1]=spb[2]=spb[3]=NULL;
        spn=4;
        return(spn);
        }


static int etsi_coeff()
        {
        int i;

        i=spfind("COEFF");
        if (i<0)
            strcpy(coeff,"1-(a+d)/(a+b+c+d)");
        else
            strcpy(coeff,spb[i]);
        return(1);
        }

static int binlimits()
        {
        int i,k;
        char nimi[9];
        char x[LLENGTH],*osa[2];
        double bin_c;
        int bin_dir;

        bin_c=0.0; bin_dir=0;
        k=spfind("BINARY");
        if (k>=0)
                {
                strcpy(x,spb[k]);
                k=split(x,osa,2);
                bin_c=atof(osa[0]); bin_dir=0;
                if (k==2) bin_dir=1;
                }
        for (i=0; i<m; ++i)
            {
            *nimi=EOS; strncat(nimi,d.varname[d.v[i]],8);
            k=strlen(nimi)-1; while (k>=0 && nimi[k]==' ') nimi[k--]=EOS;
            k=spfind(nimi);
            if (k>=0)
                {
                strcpy(x,spb[k]);
                k=split(x,osa,2);
                binlimit[i]=atof(osa[0]); bindir[i]=0;
                if (k==2) bindir[i]=1;
                }
            else
                {
                binlimit[i]=bin_c; bindir[i]=bin_dir;
                }
            }
        return(1);
        }

static int varaa_tilat()
        {
        int i,m2;
        xx=(double *)muste_malloc(m*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return(-1); }
        xxx=(int *)muste_malloc(m*sizeof(int));
        if (xxx==NULL) { not_enough_memory(); return(-1); }
        dd=(double *)muste_malloc(m*m*sizeof(double));
        if (dd==NULL) { not_enough_memory(); return(-1); }
        varname=(char **)muste_malloc(m*sizeof(char *));
        if (varname==NULL) { not_enough_memory(); return(-1); }
        lab=muste_malloc(8*m);
        if (lab==NULL) { not_enough_memory(); return(-1); }

        m2=m*(m+1)/2;

        if (measure==BINARY)
            {
            binlimit=(double *)muste_malloc(m*sizeof(double));
            if (binlimit==NULL) { not_enough_memory(); return(-1); }
            bindir=(int *)muste_malloc(m*sizeof(int));
            if (bindir==NULL) { not_enough_memory(); return(-1); }
            na=(double *)muste_malloc(m2*sizeof(double));
            if (na==NULL) { not_enough_memory(); return(-1); }
            nb=(double *)muste_malloc(m2*sizeof(double));
            if (nb==NULL) { not_enough_memory(); return(-1); }
            nc=(double *)muste_malloc(m2*sizeof(double));
            if (nc==NULL) { not_enough_memory(); return(-1); }
            nn=(double *)muste_malloc(m2*sizeof(double));
            if (nn==NULL) { not_enough_memory(); return(-1); }

            for (i=0; i<m2; ++i)
                {
                na[i]=nb[i]=nc[i]=nn[i]=0.0;
                }
            }
        else

            {
            mean=(double *)muste_malloc(m*sizeof(double));
            if (mean==NULL) { not_enough_memory(); return(-1); }
            stddev=(double *)muste_malloc(m*sizeof(double));
            if (stddev==NULL) { not_enough_memory(); return(-1); }
            if (cov_ind)
                {
                cov=(double *)muste_malloc(m*m*sizeof(double));
                if (cov==NULL) { not_enough_memory(); return(-1); }
                }
            }
        return(1);
        }

static int kokoa_tiedot()
        {
        int i,k,j,h;
        int l;
        int prind;


        prind=1;
        sur_print("\n");
        for (l=d.l1; l<=d.l2; ++l)
            {
            double paino;

            if (unsuitable(&d,l)) continue;
            if (painomuuttuja<0) paino=1.0;
            else
                {
                k=data_load(&d,l,painomuuttuja,&paino);
                if (k<0) return(-1);
                if (paino==MISSING8) continue;
                }
            if (prind) { sprintf(sbuf,"% d",l); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
            for (i=0; i<m; ++i)
                {
                k=data_load(&d,l,d.v[i],&xx[i]);
                if (k<0) return(-1);
                }

            for (i=0; i<m; ++i)
                {
                if (xx[i]==MISSING8) xxx[i]=-1;
                else
                    {
                    if (xx[i]<=binlimit[i]) xxx[i]=0; else xxx[i]=1;
                    if (bindir[i]) xxx[i]=1-xxx[i];
                    }
                }

            k=-1;
            for (i=0; i<m; ++i)
                {
                h=xxx[i]; if (h<0) continue;
                for (j=0; j<=i; ++j)
                    {
                    ++k;
                    if (xxx[j]<0) continue;
                    nn[k]+=paino;
                    if (h)
                        {
                        if (xxx[j]) na[k]+=paino;
                        else nb[k]+=paino;
                        }
                    else
                        if (xxx[j]) nc[k]+=paino;
                    }
                }
            }
        return(1);
        }

static int laske_matriisi()
        {
        int i,j,k;
        extern double *arvo;
        double y;

//      for (i=0; i<spn; ++i) spb2[i]=spb[i];

        varnimet(); // 18.8.2011/SM

        k=-1;
        for (i=0; i<m; ++i)
            {
            for (j=0; j<=i; ++j)
                {
//              for (h=0; h<spn; ++h) spb[h]=spb2[h];
                ++k;
                arvo[0]=na[k]; arvo[1]=nb[k]; arvo[2]=nc[k];
                arvo[3]=nn[k]-na[k]-nb[k]-nc[k];

                laske(coeff,&y);
                if (l_virhe)
                    {
                    sprintf(sbuf,"\nCannot compute expression %s for a=%g b=%g c=%g d=%g !",
                                    coeff,arvo[0],arvo[1],arvo[2],arvo[3]);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                dd[i+m*j]=dd[j+m*i]=y;

                }
            }
        return(1);
        }

static int load_cases()
        {
        int j;
        double paino;
        double wsum;
        double a;
        int i,k;
        int miss;

        strcpy(tmpdata,etmpd); strcat(tmpdata,"SURVO.TMP");
        i=open_data("wb"); if (i<0) return(-1);

        for (i=0; i<m; ++i) mean[i]=stddev[i]=0.0;
        wsum=0.0;
        if (cov_ind) { for (i=0; i<m*m; ++i) cov[i]=0.0; }
        n=0L; sur_print("\n");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;

            if (painomuuttuja>=0)
                {
                data_load(&d,j,painomuuttuja,&paino);
                if (paino==MISSING8) continue;
                }
            else paino=1.0;

            miss=0;
            for (i=0; i<m; ++i)
                {
                data_load(&d,j,d.v[i],&xx[i]);
                if (xx[i]==MISSING8) { miss=1; break; }
                }
            if (miss) continue;
            ++n; wsum+=paino;
            for (i=0; i<m; ++i)
                {
                a=paino*xx[i];
                mean[i]+=a; stddev[i]+=a*xx[i];
                if (cov_ind)
                    for (k=0; k<=i; ++k)
                        cov[i+m*k]+=a*xx[k];
                }
            sprintf(sbuf,"%d ",j); sur_print(sbuf);

            fwrite(&paino,sizeof(double),1,data);
            fwrite(xx,sizeof(double),m,data);
            }

        if (wsum==0.0 || n<2L)
            {
            sur_print("\nToo few observations or sum of weights =0!");
            WAIT; return(-1);
            }

        for (i=0; i<m; ++i)
            {
            mean[i]/=wsum; stddev[i]=sqrt((stddev[i]-wsum*mean[i]*mean[i])/(wsum-1));
            }

        if (cov_ind)
                {
                for (i=0; i<m; ++i)
                    for (k=0; k<=i; ++k)
                        {
                        cov[i+m*k]=(cov[i+m*k]-wsum*mean[i]*mean[k])/(wsum-1.0)/(stddev[i]*stddev[k]);
                        cov[k+m*i]=cov[i+m*k];
                        }
/*
                if (measure==MAHAL)
                    {
                    for (i=0; i<m; ++i) for (k=0; k<m; ++k) dd[i+m*k]=cov[i+m*k];
                    i=mat_inv(cov,dd,m,&a);
                    if (i!=1)
                        {
                        sprintf(sbuf,"Singular covariance matrix (variable %.8s)!",
                                    d.varname[d.v[-i]]);
                        sur_print(sbuf); WAIT;
                        return(-1);
                        }
                    }
*/
                }
        muste_fclose(data);
        return(1);
        }

static int open_data(char *mode)
        {
        data=muste_fopen(tmpdata,mode);
        if (data==NULL)
            {
            sprintf(sbuf,"\nCannot open %s !",tmpdata);
            WAIT; return(-1);
            }
        return(1);
        }

static int laske_matriisi2()
        {
        int i,k;
        int j;
        double paino;
        double a;

        if (measure==CORREL)
            {
            for (i=0; i<m*m; ++i) dd[i]=1.0-cov[i];
            return(1);
            }
        i=open_data("rb"); if (i<0) return(-1);

        for (i=0; i<m*m; ++i) dd[i]=0.0;

        for (j=0L; j<n; ++j)
            {
            fread(&paino,sizeof(double),1,data);
            fread(xx,sizeof(double),m,data);
            if (scaling)
                {
                for (i=0; i<m; ++i) xx[i]=(xx[i]-mean[i])/stddev[i];
                }
            switch (measure)
                {
              case EUCLID:
                for (i=0; i<m; ++i) for (k=0; k<=i; ++k)
                    {
                    a=xx[i]-xx[k];
                    dd[i+m*k]+=paino*a*a;
                    }
                break;
              case CITY:
                for (i=0; i<m; ++i) for (k=0; k<=i; ++k)
                    {
                    dd[i+m*k]+=paino*fabs(xx[i]-xx[k]);
                    }
                break;
              case MINKOWSKI:
                for (i=0; i<m; ++i) for (k=0; k<=i; ++k)
                    {
                    a=xx[i]-xx[k];
                    dd[i+m*k]+=paino*pow(a,power2);
                    }
                break;


                }
            }

        switch (measure)
            {
          case EUCLID:
            for (i=0; i<m; ++i) for (k=0; k<=i; ++k)
                dd[i+m*k]=sqrt(dd[i+m*k]);
            break;
          case CITY:
            for (i=0; i<m; ++i) for (k=0; k<=i; ++k)
                dd[i+m*k]=fabs(dd[i+m*k]);
            break;
          case MINKOWSKI:
            for (i=0; i<m; ++i) for (k=0; k<=i; ++k)
                dd[i+m*k]=pow(dd[i+m*k],1.0/power2);
            break;


            }

        for (i=0; i<m; ++i) for (k=0; k<=i; ++k)
            dd[k+m*i]=dd[i+m*k];

        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(1);
        }

static int mat_talletus()
        {
        int i,h;
        char expr[LLENGTH];
   //   char *p;

        for (i=0; i<8*m; ++i) lab[i]=' ';
        for (i=0; i<m; ++i)
            {
            for (h=0; h<8; ++h)
                {
                if (varname[i][h]==EOS) break;
                lab[8*i+h]=varname[i][h];
                }
            }
        strcpy(expr,measure_name);
        if (measure==BINARY) { strcat(expr,"_"); strcat(expr,coeff); }
        matrix_save(word[2],dd,m,m,lab,lab,8,8,-1,expr,0,0);
        return(1);
        }

// read_loopar() {}
// p_end() {}


/* distv2.c 18.12.1994/SM (19.12.1994)
*/

static int remarks()
        {
init_remarks();
rem_pr("DISTV <data>,<matrix_file>        / S.Mustonen 18.12.1994");
rem_pr("computes a distance or (dis)similarity matrix of active variables (!)");
rem_pr("for active observations.");
rem_pr("There is another Survo module DIST for a distance matrix of active");
rem_pr("observations.");
rem_pr("");
rem_pr("The results are saved in <matrix_file> with default extension .MAT .");
rem_pr("");
rem_pr("<matrix file> can be used as an input in /CSCAL and LSCAL operations,");
rem_pr("for example. In this case the matrix must consist of dissimilarities.");
wait_remarks(1);
rem_pr("The (dis)similarity measure used is selected by a MEASURE specification");
rem_pr("with following alternatives (see T.C.Cox & M.A.A.Cox: Multidimensional");
rem_pr("Scaling, Chapman & Hall p.10):");
rem_pr("");
rem_pr("EUCLIDEAN");
rem_pr("CITY_BLOCK");
rem_pr("MINKOWSKI(k)   (k>0)");
rem_pr("CORRELATION    (1 - correlation)");
// rem_pr("BINARY         (various measures for binary variables; see next page)");
rem_pr("");
rem_pr("Three first letters are sufficient like MEASURE=MIN(2) which is the");
rem_pr("same as MEASURE=EUC . Also MEASURE=MIN(1) is the same as MEASURE=CITY .");
rem_pr("");
rem_pr("The variables can be standardized by SCALING=YES .");
rem_pr("The observations are weighted by activating a weight variable by `W'.");
wait_remarks(1);
rem_pr("In case MEASURE=BINARY various user-defined (dis)similarity measures");
rem_pr("for binary variables are used. ");
rem_pr("By default each active variable is converted to a binary one by mapping");
rem_pr("values X<=0 to 0 and values X>0 to 1.");
rem_pr("This convention is overridden by giving a specification BINARY=C");
rem_pr("Then values X<=C are mapped to 0 and values X>C to 1.");
rem_pr("An optional parameter R in BINARY=C,R exchanges the values 0 and 1.");
rem_pr("Both of the above conventions can be overridden individually in any");
rem_pr("variable, say Z, by entering a specification Z=C or Z=C,R with");
rem_pr("the same interpretation as in the BINARY specification.");
rem_pr("");
rem_pr("The actual (dis)similarity coefficient for binary variables is entered");
rem_pr("as a specification COEFF=<function of a,b,c,d> where a,b,c,d are the");
rem_pr("frequencies in a 2x2 table");
rem_pr("   X/Y 1      0");
rem_pr("   1   a      b");
rem_pr("   0   c      d");
rem_pr("for each pair X,Y of variables.");
rem_pr("For example, COEFF=1-(a+d)/(a+b+c+d)  gives a dissimilarity measure");
rem_pr("which is the complement of a simple matching coefficient (default).");
wait_remarks(2);
return(1);
        }

#define MAXPITUUS 100
#define MAXARG 1
// #define MISSING8 1e306

static int laske(char *lauseke,double *y)
        {
   //   double luku();
   //   double oper();
   //   double funktio();
        char x[MAXPITUUS];
        char *p,*q;
        char sana[32];
        int len;
        double opnd[MAXARG+4]; char op[MAXARG+4]; int v[MAXARG+4];
        int t,n;
        int i;

        if (*lauseke=='-')  /* 11.2.91 */
            {
            *x='0'; strcpy(x+1,lauseke);
            }
        else strcpy(x,lauseke);
        len=0;
        p=x;
        t=0;
        while (*p)
            {
            if (l_virhe) return(-1);
            switch (*p)
                {
              case '+':
                if (len==0) { ++p; break; }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='+'; v[t++]=1;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '-':
                if (len==0) { sana[len++]=*p; ++p; break; }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='-'; v[t++]=1;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '*':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='*'; v[t++]=2;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '/':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='/'; v[t++]=2;
                supista(&t,opnd,op,v);
                ++p;
                break;
              case '^':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='^'; v[t++]=3;
                supista(&t,opnd,op,v);
                ++p;
                break;
              case '(':
                q=p+1;
                if (*q==')') { sprintf(sbuf,"\nArguments missing in %s",lauseke); sur_print(sbuf);
                               l_virhe=1; return(-1); }
                n=1;

                while (n)
                    {
                    ++p;
                    if (*p=='(') { ++n; continue; }
                    if (*p==')') { --n; continue; }
                    if (*p==EOS) { sprintf(sbuf,"\n) is missing in %s",lauseke); sur_print(sbuf);
                                   l_virhe=1; return(-1); }
                    }
                if(strchr("+-*/^)\0",*(p+1))==NULL) { syntax_error(lauseke);
                                                      return(-1); }
                *p=EOS; ++p;
//   Rprintf("\nq=%s",q); getch();
                i=laske(q,&opnd[t]);
                if (i<0 || l_virhe) return(-1);
                if (i==2) { sur_print("\nError! ret2"); WAIT; }

/*   Rprintf("\ntulos1=%f",opnd[t]); getch();  */
                if (len==0) { len=-1; break; }
                sana[len]=EOS;

                /* Yhden muuttujan funktiot */
                if (*sana=='-')
                    opnd[t]=-funktio(sana+1,opnd[t]);
                else
                    opnd[t]=funktio(sana,opnd[t]);
                if (l_virhe) return(-1);
                len=-1;
                break;

              case ')':
                sprintf(sbuf,"\n( missing in %s",lauseke); sur_print(sbuf); l_virhe=1; return(-1);

              case 'e': case 'E':
                if (strchr("+-.0123456789",sana[0])!=NULL)
                    {
                    sana[len++]=*p; ++p;
                    if (*p!='+' && *p!='-') break;
                    }
              default:
                /* tarkistukset puuttuvat */
                sana[len++]=*p;
                ++p;
                }
            }

        if (len<0) { v[t++]=0; }
        else
                   if (len>0) { opnd[t]=luku(sana,len); v[t++]=0; }

        supista(&t,opnd,op,v);
        *y=opnd[0];

        return(1);
        }

static double luku(char *sana,int len)
        {
        char *p;
        double tulos=1.0;
        int i;

        sana[len]=EOS;
        p=sana; if (*p=='-') ++p;
        if (strchr("1234567890.",*p)==NULL)
            {
            i=laske2(p,&tulos); if (i<0) return((double)1.0);
            if (*sana=='-') return(-tulos);
            return(tulos);
            }
        return(atof(sana));
        }

static double oper(double x1,double x2,char laji)
        {
        extern double power();

        if (x1==MISSING8 || x2==MISSING8) return(MISSING8);
        switch (laji)
            {
          case '+':
            return(x1+x2);
          case '-':
            return(x1-x2);
          case '*':
            return(x1*x2);
          case '/':
            if (x2==0.0) { l_virhe=1; return(0.0); }
            return(x1/x2);
          case '^':
            return(power(x1,x2));
            }
        return(0.0);
        }

static double power(double x,double y)
        {
        int i;
        double f;
        if (y>=0 && y==floor(y) && y<10)
                {
                f=1;
                for (i=0; i<(int)y; ++i) f*=x;
                return(f);
                }
        return (pow(x,y));
        }


static int supista(int *t,double opnd[],char op[],int v[])
        {

        while (*t>1)
            {
            if (v[*t-1]>v[*t-2]) return(1);
            opnd[*t-2]=oper(opnd[*t-2],opnd[*t-1],op[*t-2]);
            op[*t-2]=op[*t-1]; v[*t-2]=v[*t-1];
            --(*t);
            }
        return(1);
        }

static double funktio(char *s,double x)
        {
   //   int i;
   //   double y;
        char S[32];

        if (*s==EOS) return(x);
        if (x==MISSING8) return(x);
        strncpy(S,s,31); S[31]=EOS; muste_strupr(S);

        if (strncmp(S,"SQR",3)==0) return(muste_sqrt(x));
        if (strcmp(S,"LOG")==0) return(muste_log(x));
        if (strcmp(S,"EXP")==0) return(muste_exp(x));
        if (strcmp(S,"SIN")==0) return(muste_sin(x));
        if (strcmp(S,"COS")==0) return(muste_cos(x));
        if (strcmp(S,"TAN")==0) return(tan(x));
        if (strcmp(S,"ATN")==0 || strcmp(S,"ARCTAN")==0) return(muste_atan(x));
        if (strcmp(S,"ARCSIN")==0) return(muste_asin(x));
        if (strcmp(S,"ARCCOS")==0) return(muste_acos(x));
        if (strcmp(S,"ABS")==0) return(muste_fabs(x));
        if (strcmp(S,"INT")==0) return(muste_floor(x));

// RS ADD
            if (*s=='R' && strncmp(s,"R>",2)==0)
                {
                double fx[1]; fx[0]=x;
                return(muste_R_function(s+2,fx,1));
                }

        f_tuntematon(s);
        l_virhe=1;
        return(x);
        }

static int f_tuntematon(s)
char *s;
        {
        sprintf(sbuf,"\nUnknown function %s",s); sur_print(sbuf);
        l_virhe=1; return(1);
        }

static int syntax_error(s)
char *s;
        {
        sprintf(sbuf,"\nsyntax error in %s",s); sur_print(sbuf);
        l_virhe=1; return(1);
        }

static int laske2(char *muuttuja,double *y)
        {
        int i,k;

        i=spfind(muuttuja);
        if (i<0)
            {
            sprintf(sbuf,"\nParameter %s not found!",muuttuja); sur_print(sbuf);
//            WAIT;
            l_virhe=1;
            return(-1);

            }
        if (spb[i]==NULL) { *y=arvo[i]; return(1); }
        k=laske(spb[i],y);
        arvo[i]=*y;
        spb[i]=NULL;
        return(1);
        }

