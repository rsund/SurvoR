#include "muste.h"
/* _stat.c 17.11.1986/SM (7.7.1994) (30.3.1996) (21.6.1997)
*/

#include <stdio.h>
#include <stdlib.h>
//#include <conio.h>
//#include <malloc.h>
#include <math.h>
//#include <process.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define DEPS 1e-12

SURVO_DATA d;

static int maxc;
static int maxstring=8;  /* max length of string class names */

static long *freq;         /* frequencies */
static long *freq2;
static double *class;
static int *nclass;
static long *nobs;
static double *min,*max;
static double *cwidth;   /* class width  0.0=discrete */
static double *cstart;   /* lowest class limit */
static double *cbest;    /* best value */
static int m_str;   /* # of S variables */
static int *v_str;  /* 1 for S variables */
static char *str_space;
static int *n_str_class;
static char **str_class;
static long *str_freq;
static long *min_obs,*max_obs;
static double *x_lag,*sum_lag;
static double *sum1,*sum2,*sum3,*sum4; /* sums of active variables */
static double *w;         /* sums of weights */
static double *x_first;
static double *sums[5]; // sums[0] ei käytössä!
static int pr_sums;
static int m;
static long ntotal;
static int weight_variable;
static int results_line;

static char *p_str;

static char barchar;
static char bar[LLENGTH];

static double *aa;     // STAT.M varten 29.1.2009
static int mm,nn;
static char *rlab,*clab;
// static char expr[128];
// static int mat_col;
// static char mat_col_name[9];


#define MEAN_MAX 32
static int n_means;
static double *p_mean;
static double *mean_tila;


#define N_STAT 11

#define _MEAN 0
#define _STDDEV 1
#define _N 2
#define _MIN 3
#define _MAX 4
#define _N_MISS 5
#define _SKEWNESS 6
#define _KURTOSIS 7
#define _LOWER_Q 8
#define _MEDIAN 9
#define _UPPER_Q 10



/*********************************
char *spec_stat[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                    "CLASSMAX", "RESULTS", "SUMS", "MAXSTRING",
                    "#MASK", "PRIND", "CONFMEAN", "FRACTILES",
                    "MEANS", "!" };
char **specs=spec_stat;
****************************************/

static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (STAT)");
        WAIT;
        return(1);
        }

static char *spois(char *x)
        {
        char *p;
        p=x; while (*p && *p==' ') ++p;
        return(p);
        }

static char *spoisloppu(char *x)
        {
        char *p;
        p=x+strlen(x)-1; while (p>x && *p==' ') { *p=EOS; --p; }
        return(x);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

static char *res(
double y,
char *x)
        {
        char *p;

        fnconv(y,accuracy+2,x);
        if (strchr(x,'e')!=NULL || strchr(x,'.')==NULL) return(x);
        p=x+strlen(x)-1; while (p>x && *p=='0') { *p=' '; --p; }
        if (*p=='.') { *p=' '; --p; }
        return(x);
        }

static int test_scaletypes()
        {
        scales(&d);
        if (weight_variable>=0)
            {
            if (!scale_ok(&d,weight_variable,RATIO_SCALE))
                {
                sprintf(sbuf,"\nWeight variable %.8s must have ratio scale!",
                          d.varname[weight_variable]); sur_print(sbuf);
                WAIT; if (scale_check==SCALE_INTERRUPT) return(-1);
                }
            }
        return(1);
        }

static int space_allocation()
        {
        int i;

        if (m==0) return(1);
        freq=(long *)muste_malloc((m+3)*maxc*sizeof(long));
        if (freq==NULL) { not_enough_memory(); return(-1); }
        freq2=(long *)muste_malloc((m+3)*maxc*sizeof(long));
        if (freq2==NULL) { not_enough_memory(); return(-1); }
        class=(double *)muste_malloc((m+3)*maxc*sizeof(double));
        if (class==NULL) { not_enough_memory(); return(-1); }
        nclass=(int *)muste_malloc((m+3)*sizeof(int));
        if (nclass==NULL) { not_enough_memory(); return(-1); }
        cwidth=(double *)muste_malloc((m+3)*sizeof(double));
        if (cwidth==NULL) { not_enough_memory(); return(-1); }
        cstart=(double *)muste_malloc((m+3)*sizeof(double));
        if (cstart==NULL) { not_enough_memory(); return(-1); }
        cbest=(double *)muste_malloc((m+3)*sizeof(double));
        if (cbest==NULL) { not_enough_memory(); return(-1); }
        v_str=(int *)muste_malloc((m+3)*sizeof(int));
        if (v_str==NULL) { not_enough_memory(); return(-1); }
        nobs=(long *)muste_malloc((m+3)*sizeof(long));
        if (nobs==NULL) { not_enough_memory(); return(-1); }
        min=(double *)muste_malloc((m+3)*sizeof(double));
        if (min==NULL) { not_enough_memory(); return(-1); }
        max=(double *)muste_malloc((m+3)*sizeof(double));
        if (max==NULL) { not_enough_memory(); return(-1); }
        sum1=(double *)muste_malloc((m+3)*sizeof(double));
        if (sum1==NULL) { not_enough_memory(); return(-1); }
        sum2=(double *)muste_malloc((m+3)*sizeof(double));
        if (sum2==NULL) { not_enough_memory(); return(-1); }
        sum3=(double *)muste_malloc((m+3)*sizeof(double));
        if (sum3==NULL) { not_enough_memory(); return(-1); }
        sum4=(double *)muste_malloc((m+3)*sizeof(double));
        if (sum4==NULL) { not_enough_memory(); return(-1); }
        x_first=(double *)muste_malloc((m+3)*sizeof(double));
        if (x_first==NULL) { not_enough_memory(); return(-1); }
        w=(double *)muste_malloc((m+3)*sizeof(double));
        if (w==NULL) { not_enough_memory(); return(-1); }
        min_obs=(long *)muste_malloc((m+3)*sizeof(long));
        if (min_obs==NULL) { not_enough_memory(); return(-1); }
        max_obs=(long *)muste_malloc((m+3)*sizeof(long));
        if (max_obs==NULL) { not_enough_memory(); return(-1); }
        x_lag=(double *)muste_malloc((m+3)*sizeof(double));
        if (x_lag==NULL) { not_enough_memory(); return(-1); }
        sum_lag=(double *)muste_malloc((m+3)*sizeof(double));
        if (sum_lag==NULL) { not_enough_memory(); return(-1); }

        for (i=1; i<=pr_sums; ++i)
            {
            sums[i]=(double *)muste_malloc((m+3)*sizeof(double));
            if (sums[i]==NULL) { not_enough_memory(); return(-1); }
            }

        return(1);
        }

static int string_var()
        {
        int i;

        m_str=0;
        for (i=0; i<m; ++i)
            {
            if (d.vartype[d.v[i]][0]=='S') { v_str[i]=1; ++m_str; }
            else v_str[i]=0;
            }
        if (m_str==0) return(1);

		maxstring=8; // RS ADD 25.5.2012
        i=spfind("MAXSTRING");
        if (i>=0)
            {
            maxstring=atoi(spb[i]);
            }
        m_str++;
        str_space=muste_malloc(5*m_str*maxc*(maxstring+1)); // 1 -> 5 21.2.2011
        if (str_space==NULL) { not_enough_memory(); return(-1); }
        n_str_class=(int *)muste_malloc(m_str*sizeof(int));
        if (n_str_class==NULL) { not_enough_memory(); return(-1); }
        str_class=(char **)muste_malloc(m_str*maxc*sizeof(char *));
        if (str_class==NULL) { not_enough_memory(); return(-1); }
        str_freq=(long *)muste_malloc(m_str*maxc*sizeof(long));
        if (str_freq==NULL) { not_enough_memory(); return(-1); }
		m_str--;

        return(1);
        }

static int tell_structure()
        {
        int i,n,k;
        char line[LLENGTH];
        char x[LLENGTH];
        char *p;

        sprintf(line,"#var=%d #act=%d #obs=%d",d.m,d.m_act,d.n);
                                     // %ld -> %d 17.6.2011
        print_line(line);
        i=spfind("#MASK"); if (i<0) return(1);

        strcpy(x,spb[i]);

        *line=EOS;
        p=x; k=0;
        while (*p!=EOS)
            {
            n=0;
            for (i=0; i<d.m; ++i) if (d.vartype[i][1]==*p) ++n;
            k+=sprintf(line+k,"#%c=%d ",*p,n);
            ++p;
            }
        print_line(line);
        return(1);
        }

static int double_width(int i,double x); // 17.6.2011
static int interval_classify(
int i,
double x)
        {
        int class;

		if (fabs(x)>1e8) { nclass[i]=-1; return(1); }
        if (fabs(cwidth[i])<DEPS) class=ceil((x-cstart[i])-1.0); // RS CHA 2.10.2012 if else
        else class=ceil((x-cstart[i])/cwidth[i]-1.0);      
        if (class<0 || class>maxc-1) { double_width(i,x); return(1); }
        ++freq[i*maxc+class];
        return(1);
        }

static int double_width(
int i,
double x)
        {
        int k,k1,k2;
        double askel,start;


        for (k=0; k<maxc; ++k) freq2[k]=0L;

        askel=cwidth[i];
        if (fabs(askel)<DEPS) return(1); // RS ADD 2.10.2012
        while (1)
            {
      /*    askel+=cwidth[i]; -27.4.1992 */
            askel*=2;

            k1=(cbest[i]-min[i])/askel+1; k2=(max[i]-cbest[i])/askel+1;
            k=(maxc-k1-k2)/2;
            start=cbest[i]-(k1+k)*askel;
            if (start>cstart[i]) start=cstart[i];
/* Rprintf("\nstart=%g mod=%g",start,fmod(cbest[i]-start,askel)); getch();
*/
/*          k=0;
          while (fmod(cbest[i]-start,askel)>0.001 && k++<10) start-=cwidth[i];
*/                                              /* k<10 vain varmistus */
            k=ceil((x-start)/askel-1.0);
/*          if (k>=0 || k<=maxc-1) break;  -28.4.1992   */
            if (k<0 || k>maxc-1) continue;
            k=ceil((min[i]-start)/askel-1.0);
            if (k<0) continue;
            k=ceil((max[i]-start)/askel-1.0);
            if (k<=maxc-1) break;

            }

        for (k=0; k<maxc; ++k)
            {
        	if (fabs(askel)<DEPS) { ceil((cstart[i]+(k+0.5)*cwidth[i]-start)); }  // RS ADD 2.10.2012 if else
            else { k1=ceil((cstart[i]+(k+0.5)*cwidth[i]-start)/askel-1.0); }
            freq2[k1]+=freq[i*maxc+k];
            }
        for (k=0; k<maxc; ++k) freq[i*maxc+k]=freq2[k];
        cstart[i]=start;
        cwidth[i]=askel;
        interval_classify(i,x);   /* new x */
        return(1);
        }

static double paras_arvo(double x,double y)
        {
        double z;
        int merkki=1;
        char a[22],b[22];
        int i,j,k,h;
        char u,v;

        if (x>y) { z=x; x=y; y=z; }
        if (x<=0.0 && y>=0.0) return(0.0);
        if (x==y) return(x);
        if (x<0) { merkki=-1; z=x; x=-y; y=-z; }

        sprintf(a,"%21.10f",x); a[21]='\0';
        sprintf(b,"%21.10f",y); b[21]='\0';
        i=0; while (a[i]==' ') a[i++]='0';
        i=0; while (b[i]==' ') b[i++]='0';

/*      Rprintf("\n%s\n%s",a,b);         */
        i=0; k=0; while (a[i]==b[i] && i<22) { ++i; if (a[i]!='0') ++k; }
        h=0;
        for (j=i+1; j<21; ++j) { if (b[j]!='.') b[j]='0';
                                 if (a[j]!='.' && a[j]!='0') ++h;
                               }
/*      Rprintf("\n%s",b);       */
        u=a[i]; v=b[i];
        if (h>0) ++u;

/*  Rprintf("\n u=%c v=%c k=%d h=%d",u,v,k,h);    */
        if (u==v) ;
        else if (u=='0' && k==0) b[i]='1';
        else if (u=='0') b[i]='0';
        else if (u<'6' && v>'4') b[i]='5';
        else if (u<'3' && v<='3') b[i]='2';
        else if (u<'5' && v<'5') b[i]='4';
        else if (u=='6' && v=='7') b[i]='6';
        else b[i]='8';

        return(merkki*atof(b));
        }

static int create_intervals(
int i,
double x)
        {
        double paras,askel;
        double minstep;
        int k1,k2,k;

 		if (fabs(x)>1e8) { nclass[i]=-1; return(1); }	
        paras=paras_arvo(min[i],max[i]);
        minstep=(max[i]-min[i])/maxc*1.5;
        askel=paras_arvo(minstep,2*minstep);
		if (fabs(askel)<DEPS) { k1=(paras-(int)min[i])+1; k2=((int)max[i]-paras)+1;  }   // RS ADD 2.10.2012 if else 
        else { k1=(paras-(int)min[i])/askel+1; k2=((int)max[i]-paras)/askel+1; }
        k=(maxc-k1-k2)/2;
        cstart[i]=paras-(k1+k)*askel;
        cwidth[i]=askel;
        cbest[i]=paras;
        for (k=0; k<maxc; ++k) freq2[k]=0;
        for (k=0; k<nclass[i]; ++k)
            {			
			if (fabs(askel)<DEPS) k1=ceil(class[i*maxc+k]-cstart[i]); // RS ADD 2.10.2012 if else 
			else k1=ceil((class[i*maxc+k]-cstart[i])/askel-1.0);
            freq2[k1]+=freq[i*maxc+k];
            }
        for (k=0; k<maxc; ++k) freq[i*maxc+k]=freq2[k];
        interval_classify(i,x);    /* new x */
        return(1);
        }

static int classify(
int i,
double x)
        {
        int imc;
        int k,h;

        if (nclass[i]==-1) return(1);
        if (cwidth[i]>0.0) { interval_classify(i,x); return(1); }
        imc=i*maxc;
        if (nclass[i]==0) { nclass[i]=1; class[imc]=x; freq[imc]=1; return(1); }
        k=0;
        while (k<nclass[i] && x>class[imc+k]) ++k;
        if (k==nclass[i])
            {
            if (nclass[i]==maxc) { create_intervals(i,x); return(1); }
            ++nclass[i]; class[imc+k]=x; freq[imc+k]=1; return(1);
            }
        if (x==class[imc+k]) { ++freq[imc+k]; return(1); }
        if (nclass[i]==maxc) { create_intervals(i,x); return(1); }
        for (h=nclass[i]; h>k; --h)
            { class[imc+h]=class[imc+h-1]; freq[imc+h]=freq[imc+h-1]; }
        ++nclass[i]; class[imc+k]=x; freq[imc+k]=1;
        return(1);
        }

static int str_classify(
int i,
char *s)
        {
        int imc;
        int k,h;
        int vert=0;
        int len;

        s[maxstring]=EOS; len=strlen(s);

/* Rprintf("\ni=%d s=%s len=%d n_str_class=%d",i,s,len,n_str_class[i]); getch();
*/
        imc=i*maxc;
        if (n_str_class[i]==0)
            {
            n_str_class[i]=1; str_freq[imc]=1;
            str_class[imc]=p_str; strcpy(p_str,s); p_str+=len+1;
            return(1);
            }
        k=0;
        while (k<n_str_class[i] && (vert=strcmp(s,str_class[imc+k]))>0) ++k;

/*  Rprintf("\nvert=%d s=%s s2=%s k=%d",vert,s,str_class[imc+k],k); getch();
*/

        if (k==n_str_class[i])
            {
            if (n_str_class[i]==maxc) { n_str_class[i]=-1; return(1); }
            ++n_str_class[i]; str_freq[imc+k]=1;
            str_class[imc+k]=p_str; strcpy(p_str,s); p_str+=len+1;
            return(1);
            }
        if (vert==0) { ++str_freq[imc+k]; return(1); }
        if (n_str_class[i]==maxc) { n_str_class[i]=-1; return(1); }
        for (h=n_str_class[i]; h>k; --h)
            { str_class[imc+h]=str_class[imc+h-1];
              str_freq[imc+h]=str_freq[imc+h-1]; }
        ++n_str_class[i]; str_freq[imc+k]=1;
        str_class[imc+k]=p_str; strcpy(p_str,s); p_str+=len+1;
        return(1);
        }

/* stat3l.c 17.11.1986/SM (6.7.1990) (7.7.1994) (30.3.1996)
*/
/******************************
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <malloc.h>
#include <math.h>
#include <survo.h>
#include <survoext.h>
#include <survodat.h>
#include "sur_stat.h"
********************************/

static int space_for_matrix()  // 29.1.2009
    {
    mm=m;
    nn=N_STAT;
    aa=(double *)muste_malloc(mm*N_STAT*sizeof(double));
    if (aa==NULL) { not_enough_memory(); return(-1); }
    rlab=(char *)muste_malloc(mm*8);
    clab=(char *)muste_malloc(N_STAT*8);

    return(1);
    }

static int stat_m_rowname(
int i,
char *name)
    {
    int k;

    *sbuf=EOS; strncat(sbuf,name,8);

    for (k=0; k<strlen(sbuf); ++k) rlab[8*i+k]=sbuf[k];
    for (k=strlen(sbuf); k<8; ++k) rlab[8*i+k]=' ';
    return(1);
    }

static int stat_m_save(
int i,int col,
char *name,
double value)
    {
    int k;

    aa[mm*col+i]=value;

    for (k=0; k<strlen(name); ++k) clab[8*col+k]=name[k];
    for (k=strlen(name); k<8; ++k) clab[8*col+k]=' ';

    return(1);
    }

static int save_stat_m()
    {
    char name[LNAME];

    sprintf(name,"%sSTAT.M",edisk);
    sprintf(sbuf,"Basic_statistics_of_%s",word[1]);
    matrix_save(name,aa,mm,nn,rlab,clab,8,8,-1,sbuf,0,0);

    return(1);
    }

static int smooth(int i)
        {
        double d,d2;
        int im=i*maxc;
        int k;

        d=class[im+1]-class[im];
        for (k=2; k<nclass[i]; ++k)
            {
            d2=class[im+k]-class[im+k-1];
            if ((d-d2)/d>1e-8) return(0);
            }
        return(1);
        }

static int siev(
double x,
char *form,
char *sana)
        {
        char *p;

        res(x,sana);
        fconv(atof(sana),form,sana);
        if (strchr(sana,'.')!=NULL && strchr(sana,'e')==NULL)
            {
            p=sana+strlen(sana)-1;
            while (*p=='0') { *p=' '; --p; }
            if (*p=='.') *p=' ';
            }
        return(1);
        }

static int str_print(int i,int is)
/* int i;        0,...,m-1  */
/* int is;       0,...,m_str-1 */
        {
        int h,len;
        long maxf;
        char *p;
    //  char sana[LLENGTH];
        char line[LLENGTH];
        int lcname,k,step,maxbar;
        int lev;

        len=0; maxf=0;
        for (h=0; h<n_str_class[is]; ++h)
            {
            p=str_class[is*maxc+h];
            if (strlen(p)>len)
                {
// -9.11.2002   len=strlen(sana);
                len=strlen(p);
                }
            if (str_freq[is*maxc+h]>maxf) maxf=str_freq[is*maxc+h];
            }
        lcname=len; if (lcname<8) lcname=8;
        k=sprintf(line,"%.8s%.*s",d.varname[d.v[i]],lcname-8,space);
        k+=sprintf(line+k,"      f     %% ");
        maxbar=c3-k-1;
        if (maxbar<1) maxbar=1; // RS ADD 25.5.2012
        step=1L;
        while ((double)maxf/(double)step+1.0>(double)maxbar) step*=2;
        if (step>1L) k+=sprintf(line+k,"     %c=%d obs.  ",barchar,step);
        print_line(line);

        for (h=0; h<n_str_class[is]; ++h)
            {
            long fr=str_freq[is*maxc+h];
            lev=fr/step;
            sprintf(line,"%-*.*s %6ld %5.1f %.*s",
                    lcname,lcname,str_class[is*maxc+h],
                    fr,
                    (double)(100.0*fr/nobs[i]),
                    lev,bar);
            if (fr && !lev) strcat(line,":");
            print_line(line);
            }
        return(1);
        }

// Confidence interval for mean on level 0.95: low=18.4563 up=18.9300
static int print_confmean(double confmean,double mean,
                          double stddev,double n)
    {
    double low,up;
    extern double muste_inv_t();

    if (confmean==0.0) return(1);
// mean=2.45; stddev=1.32; n=112.0;
// gives (2.20,2.70) <- Afifi,Azen

    up=muste_inv_t((1+confmean)/2,n-1.0)*stddev/sqrt(n);
    low=mean-up;
    up=mean+up;
    sprintf(sbuf,"%g confidence interval for mean: low=%g up=%g",
                      confmean,low,up);
    print_line(sbuf);

    return(1);
    }

static double fractile(
double pr,
int i)
        {
        int k;
        double pr_freq,sum_freq;
        double ed_class,ed_freq;
        int im=i*maxc;

        pr_freq=pr*nobs[i]+0.5;
        if (pr_freq>nobs[i]) pr_freq=nobs[i];
        k=0; sum_freq=freq[im];
        while (pr_freq>sum_freq)
            { ++k; sum_freq+=freq[im+k]; }
        if (cwidth[i]==0.0) return (class[im+k]);

        if (k==0)
            {
            ed_class=class[im+k]-cwidth[i];
            ed_freq=0.0;
            }
        else
            {
            ed_class=class[im+k-1];
            ed_freq=sum_freq-freq[im+k];
            }
        return (ed_class+(class[im+k]-ed_class)*
                  (pr_freq-ed_freq)/(sum_freq-ed_freq));
        }

static int print_fractiles(int i)
        {
//      extern char *res();
//      extern char *spois();
//      extern double fractile();
        int h,k;
        char line[LLENGTH];
        char sana[LLENGTH];
        char fract[LLENGTH], *fra[EP4];
        int nfract;
        double pr;
        double lq,med,uq;

        lq=fractile(0.25,i);
        med=fractile(0.5,i);
        uq=fractile(0.75,i);
        h=sprintf(line,"lower_Q=%s",spois(res(lq,sana)));
        h+=sprintf(line+h," median=%s",spois(res(med,sana)));
        h+=sprintf(line+h," upper_Q=%s",spois(res(uq,sana)));
        print_line(line);

        stat_m_save(i,_LOWER_Q,"lower_Q",lq);
        stat_m_save(i,_MEDIAN,"median",med);
        stat_m_save(i,_UPPER_Q,"upper_Q",uq);

        k=spfind("FRACTILES");
        if (k<0) return(1);

        strcpy(fract,spb[k]);
        nfract=split(fract,fra,EP4);
        for (k=0; k<nfract; ++k)
            {
            pr=atof(fra[k]);
            if (pr<0.0 || pr>1.0) continue;
            h=sprintf(line,"fractile(%s)=%s",fra[k],
                            spois(res(fractile(pr,i),sana)));
            print_line(line);
            }
        return(1);
        }

static int print_means(int i)
        {
        extern char *res();
        extern char *spois();
        int k;
        double *sum;
        double a,b;
        char line[LLENGTH];
        char sana[LLENGTH];
        char nimi[32];

        if (n_means==0) return(1);
        sum=mean_tila+i*n_means;
        for (k=0; k<n_means; ++k)
            {
            a=p_mean[k];
            b=*sum/w[i];
            if (a==0.0) { b=exp(b); strcpy(nimi,"Geometric mean"); }
            else { b=pow(b,1/a); strcpy(nimi,"Power mean"); }
            if (a==1.0) strcpy(nimi,"Arithmetic mean");
            else if (a==2.0) strcpy(nimi,"Quadratic mean");
            else if (a==-1.0) strcpy(nimi,"Harmonic mean");

            sprintf(line,"%-18.18s M[%g]=%s",nimi,a,spois(res(b,sana)));
            print_line(line);
            ++sum;
            }
        return(1);
        }

static int sum_sums(
int i,
double x,
double weight)
    {
    int h;
    double y;

    y=weight*x;
    for (h=1; h<=pr_sums; ++h)
        {
        sums[h][i]+=y;
        if (h<pr_sums) y*=x;
        }

    return(1);
    }

static int print_sums(int k,int i)
    {
    extern char *res();
    extern char *spois();
//  double s;
    int h;
    char sana[LLENGTH];
    char line[LLENGTH];

    for (h=1; h<=k; ++h)
        {
        sprintf(line,"sum%d=%s",h,spois(res(sums[h][i],sana)));
        print_line(line);
        }

    return(1);
    }

static int print_auto_corr(int i)
        {
        extern char *res();
        extern char *spois();
        extern char *spoisloppu();
        extern double fractile();
        int k;
        char line[LLENGTH];
        char sana[LLENGTH];
        double x1,x2;
        double r,mean1,mean2,ss1,ss2;
        double x;

        if (nobs[i]<d.l2-d.l1+1) return(1);
        data_load(&d,d.l1,d.v[i],&x1); x1-=x_first[i];
        data_load(&d,d.l2,d.v[i],&x2); x2-=x_first[i];

        mean1=(sum1[i]-x2)/(ntotal-1);
        mean2=(sum1[i]-x1)/(ntotal-1);
        ss1=sum2[i]-x2*x2-(ntotal-1)*mean1*mean1;
        ss2=sum2[i]-x1*x1-(ntotal-1)*mean2*mean2;

        if (ss1==0.0 || ss2==0.0) r=0.0;
        else r=(sum_lag[i]-(ntotal-1)*mean1*mean2)/sqrt(ss1*ss2);
        if (r*r<4.0/ntotal) return(1);
        if (r>0.9999999)
            {
            data_load(&d,(long)(d.l1+1L),d.v[i],&x);
            if(fabs(x1+(d.l2-d.l1)*(x-x1)-x2)<1e-10)
                {
                k=sprintf(line,"changing linearly from %s",
                          spoisloppu(spois(res(x1,sana))));
                k+=sprintf(line+k," to %s",spoisloppu(spois(res(x2,sana))));
                k+=sprintf(line+k,", increment=%s",
                           spois(res((x2-x1)/(ntotal-1),sana)));
                print_line(line);
                return(-1); /* linear trend */
                }
            }
             /*  r*r>4.0/ntotal   */
        sprintf(line,"autocorrelation=%.*f",accuracy-3,r);
        print_line(line);
        return(1);
        }

static int scale_type(
char m,
char *s)
        {

        *s=EOS;
        switch (m)
            {
          case ' ': break;
          case '-': strcpy(s,"Not a statistical variable"); break;
          case 'D': strcpy(s,"Dichotomous variable"); break;
          case 'N': strcpy(s,"Nominal scale"); break;
          case 'o':
          case 'O': strcat(s,"Ordinal scale"); break;
          case 's':
          case 'S': strcat(s,"Score scale"); break;
          case 'I':
          case 'i': strcat(s,"Interval scale"); break;
          case 'R':
          case 'r': strcat(s,"Ratio scale"); break;
          case 'F': strcpy(s,"Values are frequencies"); break;
          default: sprintf(s,"Scale type %c",m); break;
            }
        return(1);
        }

static int printout()
        {
//      extern char *res();
//      extern char *spois();
//      extern char *spoisloppu();
//      extern char *scale_type();
        int i,k;
        int h,len,h1,h2;
        char line[LLENGTH];
        char sana[LLENGTH];
        double mean,stddev,skewness,kurtosis;
        int lcname;
        char form[LLENGTH];
        long maxf;
        int maxbar;
        long step;
        char *p;
        int is;
        int strvar;
        double confmean; // 13.10.2002

        i=space_for_matrix(); if (i<0) return(-1); // 29.1.2009

        confmean=0.0;
        i=spfind("CONFMEAN");
        if (i<0) i=spfind("CONFMEANS");
        if (i>=0)
            {
            confmean=atof(spb[i]);
            if (confmean<=0.0 || confmean>=1.0)
                {
                sur_print("\nValid confidence level must be >0 and <1");
                WAIT; return(-1);
                }
            }

        output_open(eout);

        barchar='*';
        for (i=0; i<c3; ++i) bar[i]=barchar; bar[c3]=EOS;

        sprintf(line,"Basic statistics: %s N=%ld",word[1],ntotal);
        if (weight_variable>=0)
            {
            strcat(line," Weight variable=");
            strncat(line,d.varname[weight_variable],8);
            }
        print_line(line);
        is=-1;
        for (i=0; i<m; ++i)
            {
            if (v_str[i]) ++is;
            if (is>=0 && ((d.vartype[d.v[i]][3]=='N' && n_str_class[is]>1) ||
                 (nclass[i]==1 && v_str[i] && n_str_class[is]>1) )) strvar=1;
            else strvar=0;

            if (i>0) { *line=EOS; print_line(line); }
            sprintf(line,"Variable: %.*s ",c3,d.varname[d.v[i]]);
            print_line(line);
            stat_m_rowname(i,d.varname[d.v[i]]); // 29.1.2009

            if (nobs[i]<ntotal)
                {
                sprintf(line,"N(missing)=%ld",ntotal-nobs[i]);
                print_line(line);
                }
            if (!strvar && nclass[i]==1)
                {
                if (d.vartype[d.v[i]][0]=='S')
                    strcpy(line,"Cannot be classified");
                else
                    sprintf(line,"Constant=%s",res(class[i*maxc],sana));
                print_line(line);
                continue;
                }
            scale_type(d.vartype[d.v[i]][3],line);
            if (*line) print_line(line);
            stat_m_save(i,_N,"N",(double)nobs[i]);
            stat_m_save(i,_N_MISS,"N_miss",(double)(ntotal-nobs[i]) );

            if (nobs[i]==0L) continue;
            if (strvar && v_str[i]!=0)
                         /* 8.3.1994 */
                {
                double entropy=0.0;
                double dx;

                for (h=0; h<n_str_class[is]; ++h)
                    {
                    long ff=str_freq[is*maxc+h];
                    if (ff==0L || ff==nobs[i]) continue;
                    dx=(double)ff/(double)nobs[i];
                    entropy-=dx*log(dx);
                    }
                k=sprintf(line,"entropy=%s (%.1f%%)",
                                spois(res(entropy/log(2.0),sana)),
                                100.0*entropy/log((double)n_str_class[is]));
                print_line(line);

                str_print(i,is);
                continue;
                }              
            if (scale_ok(&d,d.v[i],ORDINAL_SCALE));
                {
                char type=d.vartype[0][0];

                k=sprintf(line,"min=%s in obs.#%ld",
                          spois(res(min[i],sana)),min_obs[i]);
                if (type=='S')
                    {
                    data_alpha_load(&d,min_obs[i],0,sana);
                    k+=sprintf(line+k," (%.32s)",spoisloppu(sana));
                    }
                print_line(line);
                k=sprintf(line,"max=%s in obs.#%ld",
                          spois(res(max[i],sana)),max_obs[i]);
                if (type=='S')
                    {
                    data_alpha_load(&d,max_obs[i],0,sana);
                    k+=sprintf(line+k," (%.32s)",spoisloppu(sana));
                    }
                print_line(line);
                stat_m_save(i,_MIN,"min",min[i]);
                stat_m_save(i,_MAX,"max",max[i]);
                }
            if (w[i]!=0.0 && scale_ok(&d,d.v[i],SCORE_SCALE))
                {
                mean=sum1[i]/w[i];
                stat_m_save(i,_MEAN,"mean",mean+x_first[i]);
                k=sprintf(line,"mean=%s",spois(res(mean+x_first[i],sana)));
      stddev=sqrt((sum2[i]-sum1[i]*sum1[i]/w[i])*nobs[i]/w[i]/(nobs[i]-1.0));
                stat_m_save(i,_STDDEV,"stddev",stddev);
                k+=sprintf(line+k," stddev=%s",spois(res(stddev,sana)));
                skewness=((sum3[i]-3*sum2[i]*mean+3*sum1[i]*mean*mean)/w[i]-
                                mean*mean*mean)*nobs[i]/(nobs[i]-1)/
                                        (stddev*stddev*stddev);
                k+=sprintf(line+k," skewness=%s",spois(res(skewness,sana)));
                stat_m_save(i,_SKEWNESS,"skewness",skewness);
                kurtosis=((sum4[i]-4*sum3[i]*mean+6*sum2[i]*mean*mean
                                        -4*sum1[i]*mean*mean*mean)/w[i]+
                          mean*mean*mean*mean)*nobs[i]/(nobs[i]-1)/
                                (stddev*stddev*stddev*stddev)-3;
                k+=sprintf(line+k," kurtosis=%s",spois(res(kurtosis,sana)));
                stat_m_save(i,_KURTOSIS,"kurtosis",kurtosis);
                print_line(line);
                print_confmean(confmean,mean+x_first[i],stddev,w[i]); // 13.10.2002

                print_means(i);  /* 28.6.90 */
                if (pr_sums) print_sums(pr_sums,i); // 23.7.2001

                k=print_auto_corr(i);
                if (k<0) continue;  /* linear trend */
                }
            if (nclass[i]==-1) continue;
/*          if (results<=30) continue;       - 6.7.1990 */

            h1=0; h2=nclass[i];
            if (cwidth[i]>0.0)
                {
                nclass[i]=maxc;
                for (k=0; k<maxc; ++k)
                   class[i*maxc+k]=cstart[i]+(k+1)*cwidth[i];
                h1=0; while (freq[i*maxc+h1]==0L) ++h1;
                h2=maxc; while (freq[i*maxc+h2-1]==0L) --h2;
                }
            len=0; maxf=0;
            for (h=h1; h<h2; ++h)
                {
                p=res(class[i*maxc+h],sana);
                k=strlen(p); while (p[k-1]==' ') p[--k]=EOS;
                if (strlen(p)>len)
                    {
                    len=strlen(p);
                    strcpy(form,p);
                    }
                if (freq[i*maxc+h]>maxf) maxf=freq[i*maxc+h];
                }

            if ((double)maxf<0.7*nobs[i]) print_fractiles(i);

            if (results<=30) continue;     /* 6.7.1990 */
            lcname=len; if (lcname<8) lcname=8;
            if (cwidth[i]>0.0) strcpy(sana,"up.limit");
            else { strncpy(sana,d.varname[d.v[i]],8); sana[8]=EOS; }
            k=sprintf(line,"%.8s%.*s",sana,(int)(lcname-strlen(sana)),space);
            k+=sprintf(line+k,"      f     %% ");

            maxbar=c3-k-1;
        	if (maxbar<1) maxbar=1; // RS ADD 25.5.2012            
            step=1L;
            while ((double)maxf/(double)step+1.0>(double)maxbar) step*=2;

            if (step>1L) k+=sprintf(line+k,"     %c=%ld obs.  ",barchar,step);
            if (cwidth[i]>0.0) k+=sprintf(line+k,"class width=%s",
                                      spois(res(cwidth[i],sana)));
            else
                { if (!smooth(i))
                    k+=sprintf(line+k,"Values not equidistant!");
                }

            print_line(line);
            for (h=h1; h<h2; ++h)
                {
                long fr=freq[i*maxc+h];
                int lev;

                siev(class[i*maxc+h],form,sana);
                lev=fr/step;
                sprintf(line,"%*.*s %6ld %5.1f %.*s",
                        lcname,lcname,sana,
                        fr,
                        (double)(100.0*fr/nobs[i]),
                        lev,bar);
                if (fr && !lev) strcat(line,":");
                print_line(line);
                }
            }
        output_close(eout);        
        save_stat_m();
        return(1);
        }

/* stat5.c 23.11.1986/SM (19.2.1987)
*/
/********************
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <math.h>
#include <survo.h>
***************************/


























/* stat6.c 28.6.1990/SM (1.7.1990)
*/
/************************
#include <stdio.h>
#include <stdlib.h>
#include <conio.h>
#include <math.h>
#include <malloc.h>
#include <survo.h>
#include <survoext.h>
******************************/

static int init_means()
        {
        int i;
        char x[LLENGTH];
        char *sana[MEAN_MAX];
        char ch;
        double a;

        n_means=0;
        i=spfind("MEANS"); if (i<0) return(1);
        strcpy(x,spb[i]);
        n_means=split(x,sana,MEAN_MAX);
        if (n_means==0) return(1);

        p_mean=(double *)muste_malloc(n_means*sizeof(double));
        if (p_mean==NULL) { not_enough_memory(); return(-1); }
        mean_tila=(double *)muste_malloc(n_means*m*sizeof(double));
        if (mean_tila==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<n_means; ++i)
            {
            muste_strupr(sana[i]);
            ch=*sana[i];
            if (ch=='A') a=1;
            else if (ch=='G') a=0;
            else if (ch=='H') a=-1;
            else if (ch=='Q') a=2;
            else a=atof(sana[i]);
            p_mean[i]=a;
            }

        for (i=0; i<n_means*m; ++i) mean_tila[i]=0.0;

        return(1);
        }

static int sum_means(
int i,
double x,
double weight)
        {
        int k;
        double *sum;
        double a;

        if (n_means==0) return(1);
        if (x<=0.0)
            {
    sur_print("\nOnly positive data values permitted in moment means (MEANS)");
            WAIT;
            return(-1);
            }
        sum=mean_tila+i*n_means;
        for (k=0; k<n_means; ++k)
            {
            a=p_mean[k];
            if (a==0.0) a=log(x);
            else a=pow(x,a);
            *sum+=weight*a;
            ++sum;
            }
        return(1);
        }

static int statistics()
        {
        int i,j,h;
        long l;
        double x,wx,x2;
        double weight;
        char s[LLENGTH];
        int is;
        int prind;

        ntotal=0L;
        for (i=0; i<m; ++i)
            {
            sum1[i]=sum2[i]=sum3[i]=sum4[i]=w[i]=0.0;
            max[i]=-MISSING8; min[i]=MISSING8; x_first[i]=MISSING8;
            nclass[i]=0;
            nobs[i]=0L;
            cwidth[i]=0.0;
            for (j=0; j<maxc; ++j) freq[i*maxc+j]=0L;
            sum_lag[i]=x_lag[i]=0.0;
            for (h=1; h<=pr_sums; ++h) sums[h][i]=0.0;
            }

        p_str=str_space;
        for (i=0; i<m_str; ++i)
            {
            n_str_class[i]=0;
            }

        prind=1;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        sur_print("\nScanning observations... ");
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            if (weight_variable>=0)
                {
                data_load(&d,l,weight_variable,&weight);
                if (weight==MISSING8) continue;
                }
            else weight=1.0;

            ++ntotal;
            if (prind) { sprintf(sbuf,"%ld ",l); sur_print(sbuf); }
            is=0;
            for (i=0; i<m; ++i)
                {
                if (v_str[i]) ++is;
                data_load(&d,l,d.v[i],&x);
                if (fabs(x)<DEPS) x=0.0;
                if (x==MISSING8) continue;
                if (x<min[i]) { min[i]=x; min_obs[i]=l; }
                if (x>max[i]) { max[i]=x; max_obs[i]=l; }
                classify(i,x);
                ++nobs[i];
                j=sum_means(i,x,weight); if (j<0) return(-1); /* 28.6.90 */
                j=sum_sums(i,x,weight); if (j<0) return(-1);
                if (x_first[i]==MISSING8) x_first[i]=x;
                x-=x_first[i];
                wx=weight*x;
            sum1[i]+=wx; x2=wx*x; sum2[i]+=x2; sum3[i]+=x*x2; sum4[i]+=x*x*x2;
                w[i]+=weight;
                sum_lag[i]+=x_lag[i]*x; x_lag[i]=x;

                if (v_str[i])
                    {
                    if (n_str_class[is-1]==-1) continue;
                    data_alpha_load(&d,l,d.v[i],s);
                    str_classify(is-1,s);
                    }                    
                }
            }
        return(1);
        }


/*****************
void main(argc,argv)
int argc; char *argv[];
**********************/

void muste_stat(char *argv)
        {
        int i;
// RS ADD 25.5.2012 variable init
maxc=0;
freq=NULL;         
freq2=NULL;
class=NULL;
nclass=NULL;
nobs=NULL;
min=NULL;
max=NULL;
cwidth=NULL;
cstart=NULL;
cbest=NULL;
m_str=0;
v_str=NULL;
str_space=NULL;
n_str_class=NULL;
str_class=NULL;
str_freq=NULL;
min_obs=NULL;
max_obs=NULL;
x_lag=NULL;
sum_lag=NULL;
sum1=NULL;
sum2=NULL;
sum3=NULL;
sum4=NULL;
w=NULL;
x_first=NULL;
//static double *sums[5]; // sums[0] ei käytössä!
pr_sums=0;
m=0;
ntotal=0;
weight_variable=0;
results_line=0;
p_str=NULL;
barchar=0;
//static char bar[LLENGTH];
aa=NULL;
mm=0;
nn=0;
rlab=NULL;
clab=NULL;
n_means=0;
p_mean=NULL;
mean_tila=NULL;

/*      s_init(argv[1]);    */
        s_init(argv);
/*      s_opt(argv[2]);     */

/*********
        if (strcmpi(word[0],"STATMSF")==0)
            { op_statmsf(); s_end(argv[1]); return; }
*******************/
        if (g<2)
            {
            init_remarks();
            rem_pr("\nUsage: STAT <Survo_data>,<output_line>");
            rem_pr("\nMore information by STAT?             ");
            wait_remarks(2);
            s_end(argv);
            return;
            }
        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }
        i=data_open3(word[1],&d,0,1,0,0); if (i<0) { s_end(argv); return; }
        i=spec_init(r1+r-1); if (i<0) return;
//      if (!lite_dim_ok(d.m,d.n)) { lite_err(2); s_end(argv); return; }

        if (i<0) { sur_print("\nToo many specifications!"); WAIT; return; }
        i=mask(&d); if (i<0) { s_end(argv); return; }
        if (g>3 && muste_strcmpi(word[3],"STATUS")==0)
            {
            output_open(eout); tell_structure(); output_close(eout);
            s_end(argv); return;
            }
        weight_variable=activated(&d,'W');
        i=test_scaletypes(); if (i<0) return;
        m=d.m_act;
        i=conditions(&d); if (i<0) return;  /* permitted only once */

//      i=optdim_d(); if (i && i<d.m) err(0);
//      i=optdim_o(); if (i && (long)i<d.n) err(0);
//      if (!lite_dim_ok(d.m,d.n)) { lite_err(2); return; } // 2.8.2005

        maxc=30;
        if (d.l2-d.l1<54L)              /* 30.4.90 */
            maxc=3+(d.l2-d.l1)/2.0;
        i=spfind("CLASSMAX");
            if (i>=0) { i=atoi(spb[i]); if (i>1) maxc=i; }
        i=spfind("RESULTS");
            if (i>=0) results=atoi(spb[i]);

        pr_sums=0;
        i=spfind("SUMS"); if (i>=0) pr_sums=atoi(spb[i]);
        if (pr_sums>4) pr_sums=4;

        i=space_allocation(); if (i<0) return;
        i=init_means(); if (i<0) return; /* 28.6.90 */
        i=string_var(); if (i<0) return;
        i=statistics(); if (i<0) return;
        printout();
        data_close(&d);
        s_end(argv);
        }

