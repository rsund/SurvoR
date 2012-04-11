/* !dist.c 26.9.1994/SM (19.12.1994) (8.5.1995) (13.6.1997)
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAX_N 5000
#define LABEL_LEN 8

#define EUCLID 0
#define MAHAL  1
#define CITY   2
#define MINKOWSKI 3
#define CANBERRA 4
#define BRAY_CURTIS 5
#define BHATTA 6
#define ANGULAR 7
#define CORREL 8
#define BINARY 9

// static char *measures[]={ "EUCLID", "MAHAL", "CITY", "MINKOWSKI" "CANBERRA",
//                   "BRAY_CURTIS", "BHATTA", "ANGULAR", "CORREL", "BINARY" };
static char coeff[LLENGTH];
static int measure;
static char measure_name[LNAME];
static int cov_ind;
static double power2;

// #define YES 1
// #define NO  0
// static char *scalings[2]={ "YES", "NO" };
static int scaling;
static SURVO_DATA d;
static double *distm;
static char *lab;
static int m;
static int label_var;
static FILE *data;
static char tmpdata[LNAME];
static int n;
static double *xx,*xx2;
static int tmpsize;
static int previous_j;
static double *mean,*stddev,*cov;
static int const_ind,const_var;
static double *binlimit;
static int *bindir;
static double *weight;
static int wm,wn;
static int wlr,wlc;
static int wtype;
static char wexpr[129];
static char wname[LNAME];
static int prind;

static int n_center;
static double *cent;
static char *lab2;
static int *obs_nr;
static int gvar,dvar;
static double total_sum;
static int *gfreq;
static int tulosrivi;
static int l_virhe=0;

static int read_spec();
static int space_allocation1();
static int space_allocation2();
static int read_weights();
static int load_cases();
static int poista(int var);
static int etsi_coeff();
static int binlimits();
static int open_data(char *mode);
static int moments();
static int scale();
static int compute();
static double meas(double *xx,double *xx2);
static int tmp_read(int j,char *label,double *xx);
static int tmp_save(int j,char *label,double *xx);
static int euclid(double *pd,double *x,double *y);
static int mahal(double *pd,double *x,double *y);
static int city(double *pd,double *x,double *y);
static int minkowski(double *pd,double *x,double *y);
static int canberra(double *pd,double *x,double *y);
static int bray_curtis(double *pd,double *x,double *y);
static int bhatta(double *pd,double *x,double *y);
static int angular(double *pd,double *x,double *y);
static int correl(double *pd,double *x,double *y);
static int binary(double *pd,double *x,double *y);
static int mat_talletus();
static int not_enough_memory();
static int remarks();
static int sum_of_distances_from_centers();
static int read_centers();
static int comp_sum();
static int print_results();
static int eoutput(char *rivi);
static int centers_error();


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


/*******************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "RESULTS", "PRIND", "MEASURE", "SCALING", "WEIGHTS",
                 "COEFF", "BINARY", "CENTERS", "!" };

char **specs=specs0;
*****************************/
// extern int spn;
// extern char **spb2;
// extern int n_center;
// extern int gvar,dvar;

/***************************
main(argc,argv)
int argc; char *argv[];
*********************/

void muste_dist(char *argv)
        {
        int i;

        s_init(argv);
        if (g<3)
            {
            remarks();
            s_end(argv); // 26.11.2009
            return;
            }

        l_virhe=0;

        i=data_read_open(word[1],&d); if (i<0) return;
//      i=sp_init(r1+r-1,4); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return; // 16.8.2011/SM

//      if (spec_check) i=spec_word_dist(spec_check); // 11.7.2004

        i=read_spec(); if (i<0) return;

        i=mask(&d); if (i<0) return;
        if (muste_strcmpi(word[2],"CENTERS")==0)
            {
            n_center=1; /* tilapäisesti */
            sum_of_distances_from_centers(); s_end(argv[1]); return;
            }
        i=conditions(&d); if (i<0) return;  /* permitted only once */
        i=space_allocation1(); if (i<0) return;
        i=load_cases(); if (i<0) return;
        i=read_weights(); if (i<0) return;
        i=space_allocation2(); if (i<0) return;
        i=moments(); if (i<0) return;
        i=scale(); if (i<0) return;
        i=compute(); if (i<0) return;
        mat_talletus();

        }


static int varnimet()
        {

        spa[0]="a"; spa[1]="b"; spa[2]="c"; spa[3]="d";
        spb[0]=spb[1]=spb[2]=spb[3]=NULL;
        spn=4;
        return(spn);
        }

static int read_spec()
        {
        int i;
        char x[LLENGTH];
        char *p;

        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);
        measure=EUCLID;
        cov_ind=0;
        scaling=0;
        strcpy(measure_name,"Euclidean");
        i=spfind("MEASURE");
        if (i>=0)
            {
            *x=EOS; strncat(x,spb[i],3);
            muste_strupr(x); strcpy(measure_name,spb[i]);
            if (strcmp(x,"EUC")==0) { measure=EUCLID; cov_ind=0; }
            else if (strcmp(x,"MAH")==0) { measure=MAHAL; cov_ind=1; scaling=1; }
            else if (strcmp(x,"CIT")==0) { measure=CITY; cov_ind=0; }
            else if (strcmp(x,"MIN")==0)
                {
                measure=MINKOWSKI; cov_ind=0;
                power2=1.0;
                p=strchr(measure_name,'(');
                if (p==NULL)
                    {
                    sprintf(sbuf,"\nThe index (k) missing in %s(k)",measure_name);
                    sur_print(sbuf); WAIT; return(-1);
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
            else if (strcmp(x,"COR")==0) { measure=CORREL; cov_ind=0; }
            else if (strcmp(x,"BIN")==0)
                {
                measure=BINARY; cov_ind=0;
                }
            else
                {
                sprintf(sbuf,"\nUnknown MEASURE=%s",measure_name);
                sur_print(sbuf); WAIT; return(-1);
                }
            }

        i=spfind("SCALING");
        if (i>=0)
            {
            strcpy(x,spb[i]); muste_strupr(x);
            if (strncmp(x,"Y",1)==0) scaling=1;
            else if (strncmp(x,"N",1)==0) scaling=0;
            else scaling=atoi(x);
            if (measure==MAHAL) scaling=1;
            }
        return(1);
        }

static int space_allocation1()
        {
        lab=muste_malloc(MAX_N*LABEL_LEN);
        if (lab==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int space_allocation2()
        {
    //  int maxmn;

        mean=(double *)muste_malloc(m*sizeof(double));
        if (mean==NULL) { not_enough_memory(); return(-1); }
        stddev=(double *)muste_malloc(m*sizeof(double));
        if (stddev==NULL) { not_enough_memory(); return(-1); }
/*
        if (cov_ind && m>MAX_M)
            {
            sprintf(sbuf,"\# of active variables (%d) is too high (>90).",m);
            sur_print(sbuf);
            sur_print("\nCannot compute the covariance matrix!");
            WAIT; return(-1);
            }
*/
        if (cov_ind)
            {
            cov=(double *)muste_malloc(m*m*sizeof(double));
            if (cov==NULL) { not_enough_memory(); return(-1); }
            }

        if (n_center) return(1);

        distm=(double *)muste_malloc(n*n*sizeof(double));
        if (distm==NULL) { not_enough_memory(); return(-1); }

        return(1);
        }


static int read_weights()
        {
        int i;

        i=spfind("WEIGHTS");
        if (i>=0)
            {
            strcpy(wname,spb[i]);
            i=matrix_load(wname,&weight,&wm,&wn,NULL,NULL,&wlr,&wlc,&wtype,wexpr);
            if (i<0) return(-1);
            if ((wm==1 && wn==m) || (wm==m && wn==1)) return(1);
            sprintf(sbuf,"\nThe weights (%s) should be given as a vector of %d elements!",
                                   wname,m);
            sur_print(sbuf); WAIT; return(-1);
            }
        else
            {
            weight=(double *)muste_malloc(m*sizeof(double));
            if (weight==NULL) { not_enough_memory(); return(-1); }
            for (i=0; i<m; ++i) weight[i]=1.0;
            }
        return(1);
        }

static int load_cases()
        {
        int i,k;
        int j;
        char label[LLENGTH];
        int miss;

        label_var=activated(&d,'L');
        if (label_var<0) label_var=0;
        if (d.vartype[label_var][0]!='S') label_var=-1;

        k=0;
        if (label_var>=0)
            {
            for (i=0; i<d.m_act; ++i)
                {
                if (d.v[i]==label_var) k=1;
                d.v[i]=d.v[i+k];
                }
            if (k) --d.m_act;     /* if (k) 8.5.1995 */
            }

        if (n_center)
            {
            gvar=activated(&d,'G');
            if (gvar<0) { sur_print("\nNo grouping variable activated by `G'!");
                          WAIT; return(-1);
                        }
            dvar=activated(&d,'D');
            poista(gvar);
            poista(dvar);
            }

        m=d.m_act;
        if (m<=0)
            {
            sur_print("\nNo active variables!"); WAIT; return(-1);
            }
/*
printf("\nlabel_var=%d m=%d\n",label_var,m);
for (i=0; i<m; ++i) printf("%d ",d.v[i]); getch();
*/
        if (measure==BINARY)
            {
            binlimit=(double *)muste_malloc(m*sizeof(double));
            if (binlimit==NULL) { not_enough_memory(); return(-1); }
            bindir=(int *)muste_malloc(m*sizeof(int));
            if (bindir==NULL) { not_enough_memory(); return(-1); }
            i=etsi_coeff();
            if (i<0) return(-1);
            i=binlimits();
            if (i<0) return(-1);
            }


        xx=(double *)muste_malloc(m*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return(-1); }
        xx2=(double *)muste_malloc(m*sizeof(double));
        if (xx2==NULL) { not_enough_memory(); return(-1); }
        tmpsize=8+m*sizeof(double);

        strcpy(tmpdata,etmpd); strcat(tmpdata,"SURVO.TMP");
        data=muste_fopen(tmpdata,"wb");
        if (data==NULL)
            {
            sprintf(sbuf,"\nCannot save data in %s !",tmpdata);
            WAIT; return(-1);
            }
        n=0; sur_print("\n");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (!n_center && unsuitable(&d,j)) continue;
            miss=0;
            for (i=0; i<m; ++i)
                {
                data_load(&d,j,d.v[i],&xx[i]);
                if (xx[i]==MISSING8) { miss=1; break; }
                }
            if (miss) continue;
            ++n;
            if (prind)
                {
                sprintf(sbuf,"%d ",n); sur_print(sbuf);
                }
            if (measure==BINARY)
                {
                for (i=0; i<m; ++i)
                    {
                    if (xx[i]<=binlimit[i]) xx[i]=0.0; else xx[i]=1.0;
                    if (bindir[i]) xx[i]=1.0-xx[i];
                    }
                }
            if (label_var>=0)
                {
                data_alpha_load(&d,j,label_var,label);
                sprintf(sbuf,"%8.8s",label);
                }
            else
                {
                sprintf(sbuf,"%d",j); /* ennen n */
                for (i=strlen(sbuf); i<8; ++i) sbuf[i]=' ';
                }
            for (i=0; i<8; ++i) putc((int)sbuf[i],data);


            fwrite(xx,sizeof(double),m,data);
            }
        muste_fclose(data);
        if (cov_ind && m>n)
            {
            sprintf(sbuf,"\nNumber of valid cases (%d) too small! At least %d required!",n,m);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (n<3)
            {
            sprintf(sbuf,"\nNumber of valid cases (%d) too small! At least 3 required!",n);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (!n_center && n>MAX_N)
            {
            sprintf(sbuf,"\nNumber of valid cases (more than %d) too high!",MAX_N);
            sur_print(sbuf); WAIT; return(-1);
            }

        i=open_data("rb"); if (i<0) return(-1);
        previous_j=-1;
        return(1);
        }

static int poista(int var)
        {
        int i,k;

        k=0;
        for (i=0; i<d.m_act; ++i)
            {
            if (d.v[i]==var) ++k;
            d.v[i]=d.v[i+k];
            }
        d.m_act-=k;
        return(1);
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
/* -1.4.1995    binlimit[i]=0.0; bindir[i]=0; */
                binlimit[i]=bin_c; bindir[i]=bin_dir;
                }
            }
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

static int moments()
        {
        int i,j,k;
        char label[9];
        double a;

        for (i=0; i<m; ++i)
            {
            mean[i]=stddev[i]=0.0;
            if (cov_ind) for (k=0; k<m; ++k) cov[i+m*k]=0.0;
            }
        for (j=0; j<n; ++j)
            {
            tmp_read(j,label,xx);
/*
printf("\n%8.8s",label);
for (i=0; i<m; ++i) printf(" %g",xx[i]); getch();
*/
            for (i=0; i<m; ++i)
                {
                a=xx[i];
                mean[i]+=a; stddev[i]+=a*a;
                if (!cov_ind) continue;
                for (k=0; k<=i; ++k)
                    cov[i+m*k]+=a*xx[k];
                }
            }
        const_ind=0;
        for (i=0; i<m; ++i)
            {
            a=mean[i];
            stddev[i]=sqrt((stddev[i]-a*a/(double)(n))/(double)(n-1));
            if (stddev[i]<1e-20) { const_ind=1; const_var=i; }
            mean[i]/=(double)n;
            if (!cov_ind) continue;
            for (k=0; k<=i; ++k)
                {
                cov[i+m*k]=(cov[i+m*k]-a*mean[k])/(double)(n-1);
                cov[k+m*i]=cov[i+m*k];
                }
            if (scaling)
                {
                if (const_ind)
                    {
                    sprintf(sbuf,"\nVariable %.8s is a constant %g",
                                            d.varname[d.v[i]],mean[i]);
                    sur_print(sbuf); WAIT;
                    return(-1);
                    }
                    for (k=0; k<=i; ++k)
                    {
                    cov[i+m*k]/=stddev[i]*stddev[k];
                    cov[k+m*i]=cov[i+m*k];
                    }
                }
            }
/*      mprint(mean,1,m);
        mprint(stddev,1,m);
        mprint(cov,m,m);
*/
        return(1);
        }

static int scale()
        {
        int i,j;
        char label[9];
/*
printf("\n");
for (i=0; i<m; ++i) printf("%g ",mean[i]);
printf("\n");
for (i=0; i<m; ++i) printf("%g ",stddev[i]); getch();
*/
        if (!scaling) return(1);
        muste_fclose(data);
        i=open_data("r+b"); if (i<0) return(-1);
        for (j=0; j<n; ++j)
            {
            previous_j=-2;
            tmp_read(j,label,xx);
            for (i=0; i<m; ++i)
                xx[i]=(xx[i]-mean[i])/stddev[i];
            tmp_save(j,label,xx);
            }
        return(1);
        }

static int compute()
        {
        int i,k;
        int j,h; // RS REM m (because seems to be used as global in this function)
        char label[9];
        double a;
        extern double meas();

        if (measure==MAHAL)
            {
            muste_fixme("\nFIXME: If problems with MAHAL check compute() in dist.c!"); // RS FIXME
            for (i=0; i<m; ++i) for (k=0; k<m; ++k) distm[i+m*k]=cov[i+m*k];
            i=mat_inv(cov,distm,m,&a);
            if (i!=1)
                {
                sprintf(sbuf,"Singular covariance matrix (variable %.8s)!",
                            d.varname[d.v[-i]]);
                sur_print(sbuf); WAIT;
                return(-1);
                }
             }

        for (i=0; i<n; ++i) for (k=0; k<n; ++k) distm[i+n*k]=0.0;

/* mprint(cov,m,m); */

//      if (measure==BINARY)   16.8.2011
//          for (i=0; i<spn; ++i) spb2[i]=spb[i];

        sur_print("\nComputing distances: ");
        for (j=0; j<n; ++j)
            {
            if (prind)
                {
                sprintf(sbuf,"%d ",j+1); sur_print(sbuf);
                }
            tmp_read(j,label,xx);

            for (h=0; h<=j; ++h)
                {
                if (h==j && measure!=BINARY) continue;
                         /* 1.4.1995     */
                tmp_read(h,label,xx2);
                a=meas(xx,xx2);
                if (l_virhe==1) return(-1);
                distm[j+n*h]=distm[h+n*j]=a;
                }
            }
/*      mprint(distm,n,n); */

        return(1);
        }

static double meas(double *xx,double *xx2)
        {
        double a;
        int i;
        a=0.0;
        switch (measure)
            {
          case EUCLID: euclid(&a,xx,xx2); break;
          case MAHAL: mahal(&a,xx,xx2); break;
          case CITY: city(&a,xx,xx2); break;
          case MINKOWSKI: minkowski(&a,xx,xx2); break;
          case CANBERRA: canberra(&a,xx,xx2); break;
          case BRAY_CURTIS: bray_curtis(&a,xx,xx2); break;
          case BHATTA: bhatta(&a,xx,xx2); break;
          case ANGULAR: angular(&a,xx,xx2); break;
          case CORREL: correl(&a,xx,xx2); break;

          case BINARY: i=binary(&a,xx,xx2);
                       if (i<0) { l_virhe=1; return(0.0); }
                       break;
             }
        return(a);
        }

static int tmp_read(int j,char *label,double *xx)
        {
        int i;

/*      if (previous_j!=j-1)   */
            muste_fseek(data,(int)j*tmpsize,SEEK_SET);
        for (i=0; i<8; ++i) label[i]=(char)getc(data); label[8]=EOS;
        fread(xx,sizeof(double),m,data);
        previous_j=j;
/* printf("\n%2d %8.8s",j,label);
   for (i=0; i<m; ++i) printf(" %g",xx[i]); getch(); */
        return(1);
        }

static int tmp_save(int j,char *label,double *xx)
        {
        int i;

        muste_fseek(data,(int)j*tmpsize,SEEK_SET);
        for (i=0; i<8; ++i) putc((int)label[i],data);
        fwrite(xx,sizeof(double),m,data);
        previous_j=-2;
        return(1);
        }

static int euclid(double *pd,double *x,double *y)
        {
        int i;
        double a;

/*
printf("\nx: ");
for (i=0; i<m; ++i) printf("%g ",x[i]); getch();
printf("\ny: ");
for (i=0; i<m; ++i) printf("%g ",y[i]); getch();
*/

        *pd=0.0;
        for (i=0; i<m; ++i)
            {
            a=x[i]-y[i];
            *pd+=weight[i]*a*a;
            }
        *pd=sqrt(*pd);
        return(1);
        }

static int mahal(double *pd,double *x,double *y)
        {
        int i,k;
        double a;

        *pd=0.0;
        for (i=0; i<m; ++i)
            {
            a=x[i]-y[i];
            for (k=0; k<=i; ++k)
                if (k==i) *pd+=a*a*cov[(m+1)*i];
                else *pd+=2*a*(x[k]-y[k])*cov[i+m*k];
            }
        *pd=sqrt(*pd);
/* printf("\nd=%g",*pd); getch(); */
        return(1);
        }

static int city(double *pd,double *x,double *y)
        {
        int i;

        *pd=0.0;
        for (i=0; i<m; ++i)
            {
            *pd+=weight[i]*fabs(x[i]-y[i]);
            }
        return(1);
        }

static int minkowski(double *pd,double *x,double *y)
        {
        int i;
        double a;

        *pd=0.0;
        for (i=0; i<m; ++i)
            {
            a=x[i]-y[i];
            *pd+=pow(weight[i]*fabs(x[i]-y[i]),power2);
            }
        *pd=pow(*pd,1.0/power2);
        return(1);
        }

static int canberra(double *pd,double *x,double *y)
        {
        int i;

        *pd=0.0;
        for (i=0; i<m; ++i)
            {
            *pd+=weight[i]*fabs(x[i]-y[i])/(x[i]+y[i]);
            }
        return(1);
        }

static int bray_curtis(double *pd,double *x,double *y)
        {
        int i;
        double a;

        *pd=0.0; a=0.0;
        for (i=0; i<m; ++i)
            {
            *pd+=weight[i]*fabs(x[i]-y[i]);
            a+=x[i]+y[i];
            }
        *pd=m*(*pd)/a;  /* kerroin m eikä 1/m kuten Cox-Cox */
        return(1);
        }

static int bhatta(double *pd,double *x,double *y)
        {
        int i;

        *pd=0.0;
        for (i=0; i<m; ++i)
            {
            *pd+=weight[i]*fabs(sqrt(x[i])-sqrt(y[i]));  /* fabs() lisätty */
            }
        *pd=sqrt(*pd);
        return(1);
        }

static int angular(double *pd,double *x,double *y)
        {
        int i;
        double a,b;

        *pd=0.0; a=b=0.0;
        for (i=0; i<m; ++i)
            {
            *pd+=weight[i]*x[i]*y[i]; a+=weight[i]*x[i]*x[i]; b+=weight[i]*y[i]*y[i];
            }
        *pd=*pd/sqrt(a*b);
        return(1);
        }

static int correl(double *pd,double *x,double *y)
        {
        int i;
        double mx,my;
        double ax,ay,sx,sy;
        double sw;

        mx=my=0.0; sw=0.0;
        for (i=0; i<m; ++i) { mx+=weight[i]*x[i]; my+=weight[i]*y[i];
                              sw+=weight[i];
                            }
        mx/=sw; my/=sw;

        *pd=0.0; sx=sy=0.0;
        for (i=0; i<m; ++i)
            {
            ax=x[i]-mx; ay=y[i]-my;
            *pd+=weight[i]*ax*ay; sx+=weight[i]*ax*ax; sy+=weight[i]*ay*ay;
            }
        if (fabs(sx)<1e-20 || fabs(sy)<1e-20) *pd=1.0;
        else *pd=1-(*pd)/sqrt(sx*sy);
        return(1);
        }


static int binary(double *pd,double *x,double *y)
        {
        int i,a[4];
        int ix,iy;
        extern double *arvo;
        extern int l_virhe;

  //    for (i=0; i<spn; ++i) spb[i]=spb2[i];

        varnimet(); // kokeilu 18.8.2011/SM


        a[0]=a[1]=a[2]=a[3]=0;
        for (i=0; i<m; ++i)
            {
            ix=x[i]; iy=y[i];
            ++a[3-2*ix-iy];
            }
        arvo[0]=a[0]; arvo[1]=a[1]; arvo[2]=a[2]; arvo[3]=a[3];
// printf("\arvot: %g %g %g %g",a[0],a[1],a[2],a[3]);

        laske(coeff,pd);
        if (l_virhe)
            {
            sprintf(sbuf,"\nCannot compute expression %s for a=%g b=%g c=%g d=%g !",
                            coeff,arvo[0],arvo[1],arvo[2],arvo[3]);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(1);
        }

static int mat_talletus()
        {
        int i,h;
        char expr[LLENGTH];
        char label[9];
    //  char name[LNAME];

        for (i=0; i<8*n; ++i) lab[i]=' ';
        for (i=0; i<n; ++i)
            {
            tmp_read(i,label,xx);
            for (h=0; h<8; ++h)
                {
                if (label[h]==EOS) break; /* ei tarpeen */
                lab[8*i+h]=label[h];
                }
            }
        strcpy(expr,measure_name);
        if (measure==BINARY) { strcat(expr,"_"); strcat(expr,coeff); }
        i=matrix_save(word[2],distm,n,n,lab,lab,8,8,-1,expr,0,0);
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(1);
        }

// read_loopar() {}
// p_end() {}
/**************************
mprint(A,m,n)
double *A;
int m,n;
        {
        int i,j;

        for (i=0; i<m; ++i)
            {
            printf("\n"); for (j=0; j<n; ++j) printf("%g ",A[i+m*j]);
            }
        getch();
        }
*****************************/

/* dist2.c 1.11.1994/SM (20.12.1994)
*/

static int remarks()
        {
init_remarks();
rem_pr("DIST <data>,<matrix_file>        / S.Mustonen 19.12.1994");
rem_pr("computes a distance or dissimilarity matrix of active");
rem_pr("observations from active variables.");
rem_pr("There is another Survo module DISTV for a distance matrix of active");
rem_pr("variables.");
rem_pr("");
rem_pr("The results are saved in <matrix_file> with default extension .MAT .");
rem_pr("");
rem_pr("If a (string) variable is activated by `L', the 8 first characters of it");
rem_pr("are used as row and column labels in <matrix_file>.");
rem_pr("Otherwise, if the first active variable is a string, it will serve");
rem_pr("as a label variable. Otherwise labels will be integers 1,2,...");
rem_pr("");
rem_pr("<matrix file> can be used as an input in /CSCAL and LSCAL operations,");
rem_pr("for example.");
wait_remarks(1);
rem_pr("The dissimilarity measure used is selected by a MEASURE specification");
rem_pr("with following alternatives (see T.C.Cox & M.A.A.Cox: Multidimensional");
rem_pr("Scaling, Chapman & Hall p.10):");
rem_pr("");
rem_pr("EUCLIDEAN, MAHALANOBIS, CITY_BLOCK,");
rem_pr("MINKOWSKI(k)   (k>0)");
rem_pr("CANBERRA, BRAY_CURTIS, BHATTACHARYYA,");
rem_pr("ANGULAR        (Angular separation)");
rem_pr("CORRELATION    (1 - correlation)");
// rem_pr("BINARY         (various measures for binary variables; see next page)");
rem_pr("");
rem_pr("Three first letters are sufficient like MEASURE=MIN(2) which is the");
rem_pr("same as MEASURE=EUC . Also MEASURE=MIN(1) is the same as MEASURE=CITY .");
rem_pr("");
rem_pr("The variables can be standardized by SCALING=YES .");
rem_pr("The variables are also weighted by WEIGHTS=<vector_of_weights_as_matrix_file>.");
rem_pr("The order of weights must be the same as the order of active");
rem_pr("variables in <data>.");
wait_remarks(1);
rem_pr("In case MEASURE=BINARY various user-defined (dis)similarity measures");
rem_pr("for binary data are used. ");
rem_pr("By default each active variable is converted to a binary one by mapping");
rem_pr("values X<=0 to 0 and values X>0 to 1.");
rem_pr("This convention is overridden by giving a specification BINARY=C");
rem_pr("Then values X<=C are mapped to 0 and values X>C to 1.");
rem_pr("An optional parameter R in BINARY=C,R exchanges the values 0 and 1.");
rem_pr("Both of the above conventions can be overridden individually in any");
rem_pr("variable, say Z, by entering a specification Z=C or Z=C,R with");
rem_pr("the same interpretation as in the BINARY specification.");
rem_pr("");
rem_pr("The actual (dis)similarity coefficient for binary data is entered");
rem_pr("as a specification COEFF=<function of a,b,c,d> where a,b,c,d are the");
rem_pr("frequencies in a 2x2 table");
rem_pr("       1      0");
rem_pr("   1   a      b");
rem_pr("   0   c      d");
rem_pr("for each pair of observations.");
rem_pr("For example, COEFF=1-(a+d)/(a+b+c+d)  gives a dissimilarity measure");
rem_pr("which is the complement of a simple matching coefficient (default).");
wait_remarks(2);
return(1);
        }

/* dist.m 8.2.1998/SM (8.2.1998)
   sum of distances to n_group given observations

   DIST <data>,CENTERS,L

*/

static int sum_of_distances_from_centers()
    {
    int i;

    i=load_cases(); if (i<0) return(-1);
    if (d.n!=(int)n)
        {
        sur_print("\nAll records must be active! (No IND,CASES,etc. accepted)");
        WAIT; return(-1);
        }
    i=read_weights(); if (i<0) return(-1);
    i=space_allocation2(); if (i<0) return(-1);
    i=moments(); if (i<0) return(-1);
    i=scale(); if (i<0) return(-1);
    i=read_centers(); if (i<0) return(-1);
    i=data_to_write(word[1],&d); if (i<0) return(-1);
    i=comp_sum(); if (i<0) return(-1);

    print_results();
    return(1);
    }

static int read_centers()
    {
    int i,h;
    char x[LLENGTH],*s[EP4];
    int j;
    char label[9];

    i=spfind("CENTERS");
    if (i<0) { centers_error(); return(-1); }
    strcpy(x,spb[i]);
    n_center=split(x,s,EP4);
    if (n_center==0) { centers_error(); return(-1); }

    cent=(double *)muste_malloc(n_center*m*sizeof(double));
    if (cent==NULL) { not_enough_memory(); return(-1); }
    lab2=muste_malloc(9*n_center);
    if (lab2==NULL) { not_enough_memory(); return(-1); }
    obs_nr=(int *)muste_malloc(n_center*sizeof(int));
    if (obs_nr==NULL) { not_enough_memory(); return(-1); }
    gfreq=(int *)muste_malloc(n_center*sizeof(int));
    if (gfreq==NULL) { not_enough_memory(); return(-1); }

    for (i=0; i<n_center; ++i)
        {
        obs_nr[i]=j=atoi(s[i])-1;
        tmp_read(j,label,xx);
        for (h=0; h<m; ++h) cent[i*m+h]=xx[h];
        strcpy(lab2+8*i,label);
        }
/*
for (i=0; i<n_center; ++i)
    {
    printf("\n"); for (h=0; h<m; ++h) printf("%g ",cent[i*m+h]);
    }
printf("\n"); getch();
*/
    return(1);
    }

static int comp_sum()
    {
    int i,j,i_min;
    double a,min;
    char label[9];
    extern double meas();

	i_min=0;
    for (i=0; i<n_center; ++i) gfreq[i]=0;
    total_sum=0.0;
    for (j=0; j<n; ++j)
        {
        tmp_read(j,label,xx);
        min=1e100;
        for (i=0; i<n_center; ++i)
            {
            a=meas(xx,cent+i*m);
            if (l_virhe==1) return(-1);
            if (a<min) { min=a; i_min=i; }
            }
        data_save(&d,(int)(j+1),gvar,(double)(i_min+1));
        if (dvar>=0)
            data_save(&d,(int)(j+1),dvar,min);
        total_sum+=min; ++gfreq[i_min];
        }
    return(1);
    }

static int print_results()
    {
    int i;

    if ((i=spfind("RESULTS"))>=0) results=atoi(spb[i]);
    tulosrivi=0;
    if (g>3) tulosrivi=edline2(word[3],1,1);
    i=output_open(eout); if (i<0) return(-1);
    sprintf(sbuf,"Sum of distances from %d centers is D_total=%g ( N=%d )",
                  n_center,total_sum,n);
    eoutput(sbuf);
    if (results>0)
        {
        eoutput("Group    Obs.#   Center     n");
        for (i=0; i<n_center; ++i)
            {
            sprintf(sbuf,"%2d    %8d %8.8s%6d",
                          i+1,obs_nr[i]+1,lab2+8*i,gfreq[i]);
            eoutput(sbuf);
            }
        }
    eoutput(" ");
    output_close(eout);
    return(1);
    }

static int eoutput(char *rivi)
        {
        output_line(rivi,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

static int centers_error()
    {
    sur_print("Give CENTERS=j1,j2,...");
    WAIT; return(1);
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
                if (*q==')') { printf("\nArguments missing in %s",lauseke);
                               l_virhe=1; return(-1); }
                n=1;

                while (n)
                    {
                    ++p;
                    if (*p=='(') { ++n; continue; }
                    if (*p==')') { --n; continue; }
                    if (*p==EOS) { printf("\n) is missing in %s",lauseke);
                                   l_virhe=1; return(-1); }
                    }
                if(strchr("+-*/^)\0",*(p+1))==NULL) { syntax_error(lauseke);
                                                      return(-1); }
                *p=EOS; ++p;
//   printf("\nq=%s",q); getch();
                i=laske(q,&opnd[t]);
                if (i<0 || l_virhe) return(-1);
// RS REM                if (i==2) { printf("\nret2"); getch(); }

/*   printf("\ntulos1=%f",opnd[t]); getch();  */
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
                printf("\n( missing in %s",lauseke); l_virhe=1; return(-1);

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
        printf("\nUnknown function %s",s);
        l_virhe=1; return(1);
        }

static int syntax_error(s)
char *s;
        {
        printf("\nsyntax error in %s",s);
        l_virhe=1; return(1);
        }

static int laske2(char *muuttuja,double *y)
        {
        int i,k;

        i=spfind(muuttuja);
        if (i<0)
            {
            printf("\nParameter %s not found!",muuttuja);
            WAIT;
            l_virhe=1;
            return(-1);

            }
        if (spb[i]==NULL) { *y=arvo[i]; return(1); }
        k=laske(spb[i],y);
        arvo[i]=*y;
        spb[i]=NULL;
        return(1);
        }

