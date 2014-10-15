#include "muste.h"
/* aggr.c 25.7.1993/SM (12.10.1993)
   FILE AGGR
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXVAR 1000
#define MAXORD 50
#define XN 0
#define XSUM 1
#define XMEAN 2
#define XSTDDEV 3
#define XMIN 4
#define XMAX 5
#define XFIRST 6
#define XLAST 7
#define XNMISS 8
#define XMEDIAN 9
#define XFRACTILE 10
#define XSUMS 11
#define XORDER 12
#define XORDERN 13
#define XTMEAN 14
#define XTPMEAN 15
#define XCORR 16
#define XSLOPE 17
#define XINTERCEPT 18
#define XXVALUES 19
#define XMODE 20
#define XMISSING 21
#define C_END -1
#define C_PLUS -2
#define C_NEG -3



extern int spn;
extern int survo_ferror;

static char *taskname[]={ "N","SUM","MEAN","STDDEV","MIN","MAX","FIRST","LAST",
                  "NMISS", "MEDIAN", "FRACTILE", "SUMS", "ORDER", "ORDERN",
                  "TRIM", "TRIMP", "CORR", "SLOPE", "INTERCEPT",
                  "#VALUES", "MODE", "MISSING",
                  "" };

                  /* Muuta myos lista aggr.h */

//                                         2   <- #VALUES CHA 28.2.2013
static char vartypes[]="2=44====2=====44444==4";
                     /* N                  */
                     /* Default types for aggregated fields, = indicates same */
//                                                            0    <- #VALUES CHA 28.2.2013
//                                                              0  <- MODE CHA 17.1.2014    
static int keytypes[]={ 0,0,0,0,0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,1,1,0};
static int n_work[]={ 1,1,2,3,1,1,1,1,1,0,0,2,0,0,0,0,7,7,7,2,0,0};
static int order_statistics[]={ 0,0,0,0,0,0,0,0,0,1,1,0,1,1,1,1,0,0,0,0,1,0};
static int xy_statistics[]={ 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0};

static unsigned int worksize;
static SURVO_DATA d1;
static SURVO_DATA_FILE d2;
static int nvar;
static char *namestring;
static int maxvar;
static char **varname;
static int *varnr;
static char *vartype;
static int *varlen;
static char *typestring;
static char **vartype2;

static int *task;
static int *keyvar;
static int *keytype;
static char *condstring;
static char *pcond;
static int ncond;
static int *condnr;
static char **condname;
static char *condtype;
static int  *condvar;
static double *condlimit1;
static double *condlimit2;
static char **condcases;

static int *condspace;
static int condcount;

static int varline;

static int aggvar;
static char aggtype;
static int multiple_aggvars; // RS 5.9.2013
static int aggvars[128];

static double *workspace;
static unsigned int *w;
static int *ok;

static int prind=1;

static int *ordvar;
static double *ordpar;
static double *ordpar2;
static int *ordnr;
static int n_ordvar=0;

static int maxord=MAXORD;
static int *ordkey;
static int *ordcond;
static unsigned int *n_cases;
static int n_ordkey=0;
static FILE *ordfile;
static long n_rec;

static double *hav;
static long *jhav;
static int *x_var;


//static char **specs;


static int right_par_missing()
        {
        sur_print("\n')' missing in SELECT!"); WAIT;
        return(-1);
        }

/*
  INPUT: t=(A+B*(C+D)+E)+F  RETURN: pointer to )+F
                                 *ppk is pointer to +B*(C+D)+E)+F
                                      (to `weakest' point in the expression)
*/
static char *end_par(char *t,char **ppk,int u)
 /* u=1: search for )    u=0: search for the term end */
        {
        int n,i;
        char *s;

        n=u; s=t; i=0; *ppk=t;
        while (*(s+1))
            {
            ++s;
            if (*s=='(') { ++n; continue; }
            if (*s==')') { --n; if (u && n==0) break; if (!u && n<0) return(s-1); }
            if (!u)
                {
                if (n==0 && (*s=='*' || *s=='+')) return(s-1);
                }
            if (n==1)
                {
                if (*s=='+') { i=2; *ppk=s; }
                else if (*s=='*' && i==0) { i=1; *ppk=s; }
                }
            }
        if (u && n) { right_par_missing(); s=NULL; }
        return(s);
        }


/*
    Returns the end of the first multiplicative or additive term in s
*/
static char *end_term(char *s)
        {
        char *pk;

        if (*s=='(') return(end_par(s,&pk,1));
        return(end_par(s,&pk,0));
        }


/*
    Returns the end of product s
*/
static char *end_prod(char *s)
        {
        char *p;

        p=s;
        while (1)
            {
            p=end_term(p); if (p==NULL) return(p);
            if (*(p+1)!='*') break;
            p+=2;
            }
        return(p);
        }


/*
   Multiplication (A+B+C)*D*E=A*D*E+B*D*E+C*D*E
                 pt    qt p q
   Result connected to s
*/
static int bool_mult(char *s,char *pt,char *qt,char *p,char *q)
        {
        char *p2;
        char x[LLENGTH];
        char y[LLENGTH];
        int i;

/*  Rprintf("\nBool_mult");
    Rprintf("\ns=%s\npt=%s\nqt=%s\np=%s\nq=%s\n",s,pt,qt,p,q); getch(); */

        *x=EOS; strncat(x,p,(int)(q-p)+1);

        ++pt; i=0;
        while (pt<qt)
            {
            p2=end_prod(pt); if (p2==NULL) return(-1);
            if (p2>qt-1) p2=qt-1;
            *y=EOS; strncat(y,pt,(int)(p2-pt)+1);
            if (i) strcat(s,"+");
            strcat(s,y); strcat(s,"*"); strcat(s,x);
            pt=p2+2; ++i;
            }
        return(1);
        }


/*
    INPUT: s=...!(A+B) pt=!(A+B)  OUTPUT: s=...(!A*!B)
    INPUT: s=...!(A*B) pt=!(A+B)  OUTPUT: s=...(!A+!B)

*/
static char *bool_neg(char *s,char *pt)
        {
        char *q;
        char *pk;
        char m[2];

        m[1]=EOS;
        q=end_par(pt,&pk,1); if (q==NULL) return(q);
        switch (*pk)
            {
          case '(':
            if (*(pt+1)=='!')  /* poista !! */
                strncat(s,pt+2,(int)(q-pt)-2);
            else
                {
                strcat(s,"!"); strncat(s,pt+1,(int)(q-pt)-1);
                }
            break;


          case '+': *m='*'; break;
          case '*': *m='+'; break;
            }
        if (*pk!='(')
            {
            strcat(s,"!("); strncat(s,pt+1,(int)(pk-pt)-1);
            strcat(s,")"); strcat(s,m);
            strcat(s,"!("); strncat(s,pk+1,(int)(q-pk)-1);
            strcat(s,")");
            }
        return(q+1);
        }



/*
   Converts Boolean expression s to form A1*B1*... + A2*B2*... + ...
*/
static int bool_norm(char *s)
        {
        char t[3*LLENGTH];
        char *ps,*pt;
        char *p,*q,*p2;
        char *pk;
        int i,k;

        while (1)
            {
            ps=strchr(s,'(');
            if (ps==NULL) break;
            strcpy(t,s);
            pt=strchr(t,'(');
            if (pt==t || *(pt-1)=='+')
                {
                *ps=EOS;
                p=end_par(pt,&pk,1);
                if (p==NULL) return(-1);
                if (*(p+1)!='*' || *pk=='*')
                    {
                    strncat(s,pt+1,p-pt-1);
                    strcat(s,p+1);
                    }
                else
                    {
                    q=end_prod(p+2); if (q==NULL) return(-1);
                    k=bool_mult(s,pt,p,p+2,q); if (k<0) return(-1);
                    strcat(s,q+1);
                    }
                }
            else if (*(pt-1)=='!')
                {
                if ((int)(pt-t)>1) *(ps-1)=EOS; else *s=EOS;
                strcat(s,"(");
                q=bool_neg(s,pt); if (q==NULL) return(-1);
                strcat(s,")");
                strcat(s,q);
                }
            else  /* *(pt-1)='*' */
                {
                q=pt-2;
                while (q>t) { if (*q=='+') { ++q; break; } --q; }
                *(s+(q-t))=EOS;
                p2=end_term(pt); if (p2==NULL) return(-1);
                k=0; if (*(p2+1)=='*') { k=1; strcat(s,"("); }
                i=bool_mult(s,pt,p2,q,pt-2); if (i<0) return(-1);
                if (k) strcat(s,")");
                strcat(s,p2+1);
                }
            }
        return(1);
        }



static int tilanpuute()
        {
        PR_EBLD;
        sur_print("\nNot enough memory for FILE AGGR!");
        PR_ENRM; WAIT; return(1);
        }

static int ord_error()
        {
        sprintf(sbuf,"\nCannot save more information in temporary file %sSURVO.XXX",etmpd);
        sur_print(sbuf); WAIT; return(-1); // RS CHA exit(1) -> return(-1)
        }

static int xy_compute(int taski,double *d,double *py)
        {
        double x2,y2;


                       /* N=d[1] SX=d[2] SY=d[3] SXX=d[4] SYY=d[5] SXY=d[6] */
        *py=MISSING8;
        if (d[1]<2.0) return(1);
        switch (taski)
            {
          case XCORR:
            x2=d[1]*d[4]-d[2]*d[2];
            y2=d[1]*d[5]-d[3]*d[3];
            if (fabs(x2)<1e-10 || fabs(y2)<1e-10) return(1);
            *py=(d[1]*d[6]-d[2]*d[3])/sqrt(x2*y2);
            break;
          case XSLOPE:
            x2=d[1]*d[4]-d[2]*d[2];
            if (fabs(x2)<1e-10) return(1);
            *py=(d[1]*d[6]-d[2]*d[3])/x2;
            break;
          case XINTERCEPT:
            x2=d[1]*d[4]-d[2]*d[2];
            if (fabs(x2)<1e-10) return(1);
            *py=(d[4]*d[3]-d[6]*d[2])/x2;
            break;
            }
        return(1);
        }


static int xy_aggregate(long j,int i,double y,double *d)
        {
        double x;

        if ((double)j==d[0]) return(1);
        d[0]=(double)j;
        if (x_var[i]==-2) x=d[1]+1.0;
        else
            {
            data_load(&d1,j,x_var[i],&x);
            if (x==MISSING8) return(1);
            }
        ++d[1];
        d[2]+=x; d[3]+=y; d[4]+=x*x; d[5]+=y*y; d[6]+=x*y;
        return(1);
        }

static int xy_stat1(int k,char *ppar)
        {
        int i;

        if (x_var==NULL)
            {
            x_var=(int *)muste_malloc(nvar*sizeof(int));
            if (x_var==NULL) { tilanpuute(); return(-1); }
            }

        i=strlen(ppar)-1; if (ppar[i]==')') ppar[i]=EOS;
        if (muste_strcmpi(ppar,"ORDER")==0) i=-2;
        else
            {
            i=varfind(&d1,ppar); if (i<0) return(-1);
            }
        x_var[k]=i;

        for (i=0; i<k; ++i)
            {
            if (!xy_statistics[task[i]]) continue;
/*
printf("\ni=%d",i);
printf("\nkeyvar: %d %d",keyvar[k],keyvar[i]);
printf("\nx_var: %d %d",x_var[k],x_var[i]);
printf("\ncondnr: %d %d",condnr[k],condnr[i]); getch();
*/
            if (keyvar[k]==keyvar[i] && x_var[k]==x_var[i] &&
                condnr[k]==condnr[i]) break;
            }
        if (i<k)
            {
            w[k]=w[i];
            worksize-=n_work[task[k]]; /* tilanvarauksessa mukana joka tapauksessa */
            }
        return(1);
        }


static int trim_mean(double *hav,unsigned int m,double a,double *py)
        {
        unsigned int i,k;
        double sum;

        *py=MISSING8;
        if (m==0) return(1);
        k=a;
        if (2*k>=m) return(1);
        sum=0.0;
        for (i=k; i<m-k; ++i)
           sum+=hav[i];
        *py=sum/(double)(m-2*k);
        return(1);
        }


static int sort_data(double *x,long *j,unsigned int n)
        {
        unsigned int h,k;
        int ind;
        double y;
        long jj;

        h=n;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind=1;
                for (k=0; k<n-h; ++k)
                    {
                    if (x[k]>x[k+h])
                        {
                        y=x[k]; x[k]=x[k+h]; x[k+h]=y;
                        jj=j[k]; j[k]=j[k+h]; j[k+h]=jj;
                        ind=0;
                        }
                    }
                if (ind==1) break;
                }
            }
        return(1);
        }


static int del_ordfile()
        {
        if (n_ordvar==0) return(1);
/*      sprintf(sbuf,"DEL %sSURVO.XXX",etmpd); system(sbuf); */
        sprintf(sbuf,"%sSURVO.XXX",etmpd);
        remove(sbuf);
        return(1);
        }

static int open_ordfile(char *mode)
        {
        char x[LLENGTH];

        strcpy(x,etmpd); strcat(x,"SURVO.XXX");
        ordfile=muste_fopen2(x,mode);
        if (ordfile==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s!",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(1);
        }


static int order_stat3(long n)
        {
        int i,ii;
        long j,jj;
        int h,k;
        double y;
        unsigned int max,m;
        unsigned int u;
        char sy[LLENGTH];

        int m_f=1,m_max,m_i;  // for MODE 17.5.2006
        double m_hav;
        long s_hav; // RS 17.1.2014

        if (n_ordvar==0) return(1);

        muste_fclose(ordfile);

        max=0L; for (i=0; i<n_ordkey; ++i) if (n_cases[i]>max) max=n_cases[i];

// RS REM       if (hav==NULL) /* 16.12.1998 */
//            hav=(double *)muste_malloc(max*sizeof(double));
//        else
            hav=(double *)muste_realloc(hav,max*sizeof(double));
        if (hav==NULL) { tilanpuute(); return(-1); }
// RS REM        if (jhav==NULL)
//            jhav=(long *)muste_malloc(max*sizeof(double));
//        else
            jhav=(long *)muste_realloc(jhav,max*sizeof(long)); // RS 17.1.2014 sizeof(double) -> long
        if (jhav==NULL) { tilanpuute(); return(-1); }

        open_ordfile("rb");

        for (i=0; i<n_ordkey; ++i)
            {
            rewind(ordfile);
            m=0;
            for (j=0L; j<n_rec; ++j)
                {
                fread(&h,sizeof(int),1,ordfile);
                fread(&k,sizeof(int),1,ordfile);
                fread(&jj,sizeof(int),1,ordfile);  // RS CHA 64-bit sizeof(long)
                fread(&y,sizeof(double),1,ordfile);

                if (h==ordkey[i] && k==ordcond[i])
                    { hav[m]=y; jhav[m]=jj; ++m; }
// Rprintf("\nh=%d k=%d jj=%d y=%g",h,k,jj,y);
                }

            sort_data(hav,jhav,m);

/*
Rprintf("\nSorted data: ");
for (u=0; u<m; ++u) Rprintf(" %g",hav[u]);
WAIT;
*/

            for (k=0; k<n_ordvar; ++k)
                {
                if (ordnr[k]!=i) continue;
                if (m==0) y=MISSING8;
                else
                    {
                    switch (task[ordvar[k]])
                        {
                      case XFRACTILE:
                        u=(unsigned int)(ordpar[k]*m);
                        if (u>m-1) u=m-1;
                        y=hav[u];
                        break;
                      case XORDER:
                        h=ordpar[k];
                        if (h<0) h=m+h+1;
                        if (h<1 || h>m) y=MISSING8; else y=hav[h-1];
                        break;
                      case XORDERN:
                        h=ordpar[k];
                        if (h<0) h=m+h+1;
                        if (h<1 || h>m) y=MISSING8; else y=hav[h-1];

                        ii=ordvar[k];
                        if (keytype[ii]==0)
                            {
                            if (y!=MISSING8)
                                data_load(&d1,jhav[h-1],(int)ordpar2[k],&y);
                            }
                        else
                            {
                            if (y==MISSING8) *sy=EOS;
                            else
                                data_alpha_load(&d1,jhav[h-1],(int)ordpar2[k],sy);
                            }
                        break;

                      case XTMEAN:
                        trim_mean(hav,m,ordpar[k],&y);
                        break;
                      case XTPMEAN:
                        trim_mean(hav,m,(double)m*ordpar[k],&y);
                        break;
                      case XMODE:
                        m_f=1; m_max=1; m_hav=hav[0]; s_hav=jhav[0];
                        for (m_i=1; m_i<m; ++m_i)
                            {
                            if (hav[m_i]==hav[m_i-1])
                                {
                                ++m_f;
                                if (m_f>m_max) { m_max=m_f; m_hav=hav[m_i]; s_hav=jhav[m_i]; }
                                }
                            else m_f=1;
                            }
                        y=m_hav; *sy=EOS;
//                        if (keytype[ordvar[k]]==1) // RS 17.1.2014
                        if (vartype[ordvar[k]]=='S') // RS 2.2.2014
                        	{
                        	if (y==MISSING8) *sy=EOS;
                            else
                                data_alpha_load(&d1,s_hav,ordvar[k],sy);
                        	}
// Rprintf("\nmode(hash): %f, s_hav=%d, sy:|%s| ",y,s_hav,sy);                        
                        }
                    }
                if (keytype[ordvar[k]]==0)
                    {
                    if (y==MISSING8) fi_miss_save(&d2,n,ordvar[k]);
                    else
                        {
                        if (d2.vartype[ordvar[k]][0]=='S')
                            {
                            fi_value_to_string(&d2,ordvar[k],y,sy);
                            fi_save(&d2,n,ordvar[k],sy);
                            }
                        else
                            fi_save(&d2,n,ordvar[k],&y);
                        }
                    }
                else
                    if (*sy==EOS) fi_miss_save(&d2,n,ordvar[k]);
                    else fi_save(&d2,n,ordvar[k],sy);
                }
            }
        muste_fclose(ordfile);
        return(1);
        }

static int cond_ok(int n)
        {
        int ok2;
        int ok0;
        int neg;

        ok2=1; neg=0;
        while (1)
            {
            switch (condspace[n])
                {
              case C_END: return(ok2);
              case C_PLUS: if (ok2) return(1);
                           ok2=1; neg=0; break;
              case C_NEG: neg=1; break;

           default:
                    ok0=ok[condspace[n]]; if (neg) ok0=1-ok0; ok2*=ok0;
                    neg=0; break;
                }
            ++n;
            }
        }

static unsigned long hash(unsigned char *str) // RS 28.2.2013
    {
    unsigned long hash = 5381;
    int c;

    while ((c = (*str++))) { hash = ((hash << 5) + hash) + c; } // hash * 33 + c

    return(hash);
    }

static int order_stat2(long j)
        {
        int i;
        double y;
        char sy[LLENGTH];
//        int k;

        if (n_ordvar==0) return(1);
/*
        Rprintf("\nordvar:");
        for (i=0; i<n_ordvar; ++i)
            Rprintf(" %d",ordvar[i]);
        Rprintf("\nordnr:");
        for (i=0; i<n_ordvar; ++i)
            Rprintf(" %d",ordnr[i]);
        Rprintf("\nordkey:");
        for (i=0; i<n_ordkey; ++i)
            Rprintf(" %d",ordkey[i]);
        Rprintf("\nordcond:");
        for (i=0; i<n_ordkey; ++i)
            Rprintf(" %d",ordcond[i]); getch();
*/


        for (i=0; i<n_ordkey; ++i)
            {
            if (ferror(ordfile)) { ord_error(); return(-1); } // RS ADD return
            if (ordcond[i]!=-1 && cond_ok(ordcond[i])==0) continue;

// Rprintf("\nordkey[%d]: %d, ordvar[%d]: %d, vartype=%c",i,ordkey[i],i,ordvar[i],vartype[ordvar[i]]);

//            if (keytype[ordkey[i]]==0) data_load(&d1,j,ordkey[i],&y);  // RS 16.1.2014                 
            if (vartype[ordvar[i]]!='S') data_load(&d1,j,ordkey[i],&y);  // RS 2.2.2014  28.8.2014 first ordkey->ordvar               
            else
                { 
                data_alpha_load(&d1,j,ordkey[i],sy);
                y=(double)hash((unsigned char *)sy); 
                }                      
                        
//            data_load(&d1,j,ordkey[i],&y);
            
            if (y==MISSING8) continue;

// Rprintf("\ny[%d]=%f",i,y);            
            
            if (ferror(ordfile)) { ord_error(); return(-1); }
            fwrite(&ordkey[i],sizeof(int),1,ordfile);
            if (ferror(ordfile)) { ord_error(); return(-1); }
            fwrite(&ordcond[i],sizeof(int),1,ordfile);
            if (ferror(ordfile)) { ord_error(); return(-1); }
            fwrite(&j,sizeof(int),1,ordfile); // RS CHA 64-BIT sizeof(long)
            if (ferror(ordfile)) { ord_error(); return(-1); }
            fwrite(&y,sizeof(double),1,ordfile);
            if (ferror(ordfile)) { ord_error(); return(-1); }

            ++n_rec;
/***************************
            if (n_cases[i]==65535)
                {
                sur_print("\nFor order statistics, max.# of cases in one aggregate is 65535.");
                WAIT; return(-1);
                }
*****************************/
            ++n_cases[i];
            }
        return(1);
        }


static int init_order_stat()
        {
        int i;

        if (n_ordvar==0) return(1);
        i=open_ordfile("wb"); if (i<0) return(-1);
        n_rec=0L;
        for (i=0; i<n_ordkey; ++i) n_cases[i]=0L;
        return(1);
        }

static int split_loppusulku(char *x,char **osa,int max)
        {
        int i;

        i=strlen(x); if (x[i-1]==')') x[i-1]=EOS;
        return(split(x,osa,max));
        }


static int order_stat1(int k,int *ptasknro,char *ppar)
        {
        int i;
        double par=0.0;
        char x[LLENGTH],*osa[2];

        if (n_ordvar==0)
            {
            ordvar=(int *)muste_malloc(nvar*sizeof(int));
            if (ordvar==NULL) { tilanpuute(); return(-1); }
            ordpar=(double *)muste_malloc(nvar*sizeof(double));
            if (ordpar==NULL) { tilanpuute(); return(-1); }
            ordpar2=(double *)muste_malloc(nvar*sizeof(double));
            if (ordpar2==NULL) { tilanpuute(); return(-1); }
            ordnr=(int *)muste_malloc(nvar*sizeof(int));
            if (ordnr==NULL) { tilanpuute(); return(-1); }
            maxord=MAXORD;
            i=spfind("MAXORD");
            if (i>=0) maxord=atoi(spb[i]);
            ordkey=(int *)muste_malloc(maxord*sizeof(int));
            if (ordkey==NULL) { tilanpuute(); return(-1); }
            ordcond=(int *)muste_malloc(maxord*sizeof(int));
            if (ordcond==NULL) { tilanpuute(); return(-1); }
            n_cases=(unsigned int *)muste_malloc(maxord*sizeof(unsigned int));
            if (n_cases==NULL) { tilanpuute(); return(-1); }
            }

        if (*ptasknro==XMEDIAN) { *ptasknro=XFRACTILE; par=0.5; }
        else if (*ptasknro==XFRACTILE) par=atof(ppar);
        switch (*ptasknro)
            {
          case XFRACTILE:
            if (par<0.0 || par>1.0)
                {
                sprintf(sbuf,"\nParameter p=%g not permitted in FRACTILE. Allowed range 0<=p<=1.",par);
                sur_print(sbuf); WAIT; return(-1);
                }
            break;
          case XORDER:
            par=atof(ppar);
            break;
          case XORDERN:
            strcpy(x,ppar);
            i=split_loppusulku(x,osa,2);
            if (i<2)
                {
                sur_print("\nError in ORDERN: Correct form ORDERN(i,variable)");
                WAIT; return(-1);
                }
            par=atof(osa[0]);
            i=varfind(&d1,osa[1]); if (i<0) return(-1);
            ordpar2[n_ordvar]=i;
            break;
          case XTMEAN:
          case XTPMEAN:
            par=atof(ppar);
            break;

            }


        ordvar[n_ordvar]=varnr[k];
        ordpar[n_ordvar]=par;

        for (i=0; i<n_ordkey; ++i)
            {
            if (ordkey[i]==keyvar[k] &&  ordcond[i]==condnr[k]) break;
            }
        if (n_ordkey>0 && i<n_ordkey)
            ordnr[n_ordvar]=i;
        else
            {
            if (n_ordkey>=maxord)
                {
                sprintf(sbuf,"\nMore than MAXORD=%d different orderings required!",maxord);
                sur_print(sbuf);
                sur_print("\nIncrease the maximum number of them by a MAXORD specification!");
                WAIT; return(-1);
                }
            ordkey[n_ordkey]=keyvar[k]; ordcond[n_ordkey]=condnr[k];
            ordnr[n_ordvar]=n_ordkey; ++n_ordkey;
            }
/* Rprintf("\nordnr=%d",ordnr[n_ordvar]); getch(); */
        ++n_ordvar;
        return(1);
        }

static int save_agg(long n)
        {
        int i;
        double *d;
        double y;
        char sy[LLENGTH];

        for (i=0; i<nvar; ++i)
            {
            d=workspace+w[i];
            switch (task[i])
                {
              case XN: y=d[0]; break;
              case XSUM: y=d[0]; break;
              case XMEAN:
                    if (d[0]==0.0) y=MISSING8;
                    else y=d[1]/d[0];
                    break;
              case XSTDDEV:
                    if (d[0]<2.0) y=MISSING8;
                    else y=sqrt((d[2]-d[1]*d[1]/d[0])/(d[0]-1));
                    break;
              case XMIN: if (d[0]>1e29) y=MISSING8; else y=d[0]; break;
              case XMAX: if (d[0]<-1e29) y=MISSING8; else y=d[0]; break;
              case XFIRST:
              case XLAST:
                    if (keytype[i]==0)
                        {
                        if (d[0]==-1.0) y=MISSING8;
                        else
                            data_load(&d1,(long)d[0],keyvar[i],&y);
                        }
                    else
                        {
                        if (d[0]==-1.0) *sy=EOS;
                        else
                            data_alpha_load(&d1,(long)d[0],keyvar[i],sy);
                        }
                    break;
              case XNMISS: y=d[0]; break;
              case XSUMS: if (d[0]==0.0) y=d[1]; else y=MISSING8; break;

              case XCORR:
              case XSLOPE:
              case XINTERCEPT:
                    xy_compute(task[i],d,&y); break;
              case XXVALUES: y=d[0]; break; 
              case XMISSING: y=MISSING8; break;

                } /* switch */

            if (keytype[i]==0 || task[i]==XXVALUES) // RS 28.2.2013 ADD || task...
                {
                if (y==MISSING8) fi_miss_save(&d2,n,varnr[i]);
                else
                    {
                    if (d2.vartype[i][0]=='S')
                        {
                        fi_value_to_string(&d2,i,y,sy); /* 31.12.1998 */
                        fi_save(&d2,n,varnr[i],sy);
                        }
                    else
                        fi_save(&d2,n,varnr[i],&y);
                    }

                }
            else
                if (*sy==EOS) fi_miss_save(&d2,n,varnr[i]);
                else fi_save(&d2,n,varnr[i],sy);
            }

        i=order_stat3(n);
        return(1);
        }


static int load_aggvar(long j,char *s,double *px)
        {
        int i;
        
        if (aggtype=='S')
            {
            if (multiple_aggvars) // RS 5.9.2013
                {
                *s=EOS;
                for (i=0; i<multiple_aggvars; i++)
                    {
                    if (d1.d2.vartype[aggvars[i]][0]=='S')
                        {
                        data_alpha_load(&d1,j,aggvars[i],sbuf);
                        strcat(s,"|");
                        strcat(s,sbuf);
                        strcat(s,"|");
                        }
                    else
                        {
                        data_load(&d1,j,aggvars[i],px);
                        if (*px==MISSING8) sprintf(sbuf,"|MISSING|");
                        else sprintf(sbuf,"|%e|",*px);
                        strcat(s,sbuf);
                        }                    
                    } 
// Rprintf("\nmultipleaggvar[%d]: %s",(int)j,s);                           
                }
            else data_alpha_load(&d1,j,aggvar,s);
            return(1);
            }
        else
            {
            data_load(&d1,j,aggvar,px);
            if (*px==MISSING8) return(-1);
            return(1);
            }
        }


static int tutki_ehdot(long j)
        {
        int i,k;
        double x;
        char s[LLENGTH];
        char t[LLENGTH];

        for (i=0; i<ncond; ++i)
            {
            if (condtype[i]==0)
                {
                data_load(&d1,j,condvar[i],&x);
                if (x==MISSING8 || x<condlimit1[i] || x>condlimit2[i])
                    ok[i]=0;
                else ok[i]=1;
                }
            else
                {
                data_alpha_load(&d1,j,condvar[i],t);
                k=strlen(t)-1; while (k && t[k]==' ') t[k--]=EOS;
                *s=','; s[1]=EOS; strcat(s,t); strcat(s,",");
                if (strstr(condcases[i],s)==NULL) ok[i]=0;
                else ok[i]=1;
                }
            }
        return(1);
        }


static int init_workspace2()
        {
        int i,k;
        double *d;

        for (i=0; i<nvar; ++i)
            {
            d=workspace+w[i];
            switch (task[i])
                {
              case XN:
              case XSUM:
                       d[0]=0.0; break;
              case XMEAN:
                       d[0]=d[1]=0.0; break;
              case XSTDDEV:
                       d[0]=d[1]=d[2]=0.0; break;
              case XMIN:
                       d[0]=1e30; break;
              case XMAX:
                       d[0]=-1e30; break;
              case XFIRST:
              case XLAST:
                       d[0]=-1.0; break; /* talletetaan havainnon nro. */
              case XNMISS:
                       d[0]=0.0; break;
              case XSUMS:
                       d[0]=d[1]=0.0; break;
              case XCORR:
              case XSLOPE:
              case XINTERCEPT:
                       for (k=0; k<7; ++k) d[k]=0.0; break;
              case XXVALUES:
                       d[0]=0.0; d[1]=1e100; // previous value
                       break;
              case XMISSING: break;
                }
            }
/*
printf("\nworksize=%u\n",worksize);
for (i=0; i<worksize; ++i) Rprintf("%g ",workspace[i]); getch();
*/
        i=init_order_stat(); if (i<0) return(-1);
        return(1);
        }

static int aggregate()
        {
        int i;
        int j,n; // RS CHA long -> int
        int new;
        char agg_string[LLENGTH],agg_string0[LLENGTH];
        double agg_value,agg_value0;
        double y;
        char sy[LLENGTH];
        double *d;

		agg_value0=0;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        new=1; n=0L; sur_print("\n");
        for (j=d1.l1; j<=d1.l2; ++j)
            {
            if (survo_ferror)
                {
                sur_print("\nCannot save more data in aggregated file!");
                WAIT; return(-1);
                }
            if (unsuitable(&d1,j)) continue;
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
            if (prind)
                { sprintf(sbuf,"%d ",j); sur_print(sbuf); } // RS CHA ld -> d
            if (new)
                {
                i=init_workspace2();
                i=load_aggvar(j,agg_string,&agg_value); if (i<0) continue;
                if (aggtype=='S') strcpy(agg_string0,agg_string);
                else agg_value0=agg_value;
                }
            else
                {
                i=load_aggvar(j,agg_string,&agg_value); if (i<0) continue;
                if (aggtype=='S')
                    {
                    if (strcmp(agg_string,agg_string0)!=0)
                        { ++n; save_agg(n); new=1; --j; continue; }
                    }
                else
                    {
                    if (agg_value!=agg_value0)
                        { ++n; save_agg(n); new=1; --j; continue; }
                    }
                }
            i=tutki_ehdot(j); if (i<0) return(-1);
            for (i=0; i<nvar; ++i)
                {
                if (condnr[i]!=-1 && cond_ok(condnr[i])==0) continue;
                d=workspace+w[i];
                if (keyvar[i]>=0)
                    {
                    if (keytype[i]==0)
                        {
                        data_load(&d1,j,keyvar[i],&y);                       
                        if (y==MISSING8)
                            {
                            if (task[i]==XNMISS || task[i]==XSUMS) ++d[0];    /* (1) */
                            continue;
                            }
                        }
                    else
                        {
                        data_alpha_load(&d1,j,keyvar[i],sy);                      
                        }
                    }
                else { y=1.0; *sy=EOS; }

                switch (task[i])
                    {
                  case XN: ++d[0]; break;
                  case XSUM: d[0]+=y; break;
                  case XMEAN: ++d[0]; d[1]+=y; break;
                  case XSTDDEV: ++d[0]; d[1]+=y; d[2]+=y*y; break;
                  case XMIN: if (y<d[0]) d[0]=y; break;
                  case XMAX: if (y>d[0]) d[0]=y; break;
                  case XFIRST:
                        if (d[0]==-1.0) d[0]=(double)j;
                        break;
                  case XLAST:
                        d[0]=(double)j;
                        break;
              /*  case XNMISS:  see  (1) above */
                  case XSUMS: d[1]+=y; break; /* d[0]=# of missing obs. */

                  case XCORR:
                  case XSLOPE:
                  case XINTERCEPT:
                        xy_aggregate(j,i,y,d); break;
                  case XXVALUES:
                        if (keytype[i]==1) // RS 28.2.2013
                            {
                            y=(double)hash((unsigned char *)sy);                            
                            }                           
                        if (y!=d[1]) ++d[0];
                        d[1]=y; break;
                  case XMISSING: break;
                    }
                }
            i=order_stat2(j); if (i<0) return(-1);
            
            new=0;
/* Rprintf("\nw: %g %g",workspace[0],workspace[1]); getch();   */
            }
            
        if (!new) { ++n; save_agg(n); }   
        fi_rewind(&d2);
/* Rprintf("\n*n=%ld",n); getch();  */
        fi_puts(&d2,(char *)&n,sizeof(int),22); // RS ADD (char *) CHA sizeof(long) -> sizeof(int) ja 22L -> 22
        fi_close(&d2);

        return(1);
        }




static int space_split(char *rivi,char **sana,int max) // RS CHA short -> int
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)   VAIN BLANKOT EROTTIMINA!
*/
        {
        int g=0;
        int p;
        int edell=0; /* vÑli edellÑ */
        int len;
        
        len=strlen(rivi);

        for (p=0; p<len; ++p)
                {
                if (rivi[p]==' ')
                        {
                        if (edell==1)
                                {
                                rivi[p]=EOS;
                                ++g;
                                if (g>=max) return(max);
                                edell=0;
                                }
                        }
                else
                        {
                        if (edell==0)
                                {
                                sana[g]=rivi+p;
                                edell=1;
                                }
                        }
                }
        if (edell==1) ++g;
        return(g);
        }


static int init_workspace()
        {
        workspace=(double *)muste_malloc((worksize+1)*sizeof(double));
        if (workspace==NULL) { tilanpuute(); return(-1); }
        ok=(int *)muste_malloc(ncond*sizeof(int)+1);
        if (ok==NULL) { tilanpuute(); return(-1); }
        return(1);
        }

static int varaa_tilat()
        {
        unsigned int u;

        namestring=muste_malloc(256*nvar);
        if (namestring==NULL) { tilanpuute(); return(-1); }
        varname=(char **)muste_malloc(nvar*sizeof(char **));
        if (varname==NULL) { tilanpuute(); return(-1); }
        varnr=(int *)muste_malloc(nvar*sizeof(int));
        if (varnr==NULL) { tilanpuute(); return(-1); }
        vartype=muste_malloc(nvar);
        if (vartype==NULL) { tilanpuute(); return(-1); }
        varlen=(int *)muste_malloc(nvar*sizeof(int));
        if (varlen==NULL) { tilanpuute(); return(-1); }
        typestring=muste_malloc(13*nvar);
        if (typestring==NULL) { tilanpuute(); return(-1); }
        vartype2=(char **)muste_malloc(nvar*sizeof(char **));
        if (vartype2==NULL) { tilanpuute(); return(-1); }
        w=(unsigned int *)muste_malloc(nvar*sizeof(unsigned int));
        if (w==NULL) { tilanpuute(); return(-1); }
        task=(int *)muste_malloc(nvar*sizeof(int));
        if (task==NULL) { tilanpuute(); return(-1); }
        keyvar=(int *)muste_malloc(nvar*sizeof(int));
        if (keyvar==NULL) { tilanpuute(); return(-1); }
        keytype=(int *)muste_malloc(nvar*sizeof(int));
        if (keytype==NULL) { tilanpuute(); return(-1); }
        u=nvar; if (u<20) u=20;
        condstring=muste_malloc(256*u);
        if (condstring==NULL) { tilanpuute(); return(-1); }
        condnr=(int *)muste_malloc(nvar*sizeof(int));
        if (condnr==NULL) { tilanpuute(); return(-1); }
        condname=(char **)muste_malloc(nvar*sizeof(char **));
        if (condname==NULL) { tilanpuute(); return(-1); }
        condtype=muste_malloc(nvar);
        if (condtype==NULL) { tilanpuute(); return(-1); }
        condvar=(int *)muste_malloc(nvar*sizeof(int));
        if (condvar==NULL) { tilanpuute(); return(-1); }
        condlimit1=(double *)muste_malloc(nvar*sizeof(double));
        if (condlimit1==NULL) { tilanpuute(); return(-1); }
        condlimit2=(double *)muste_malloc(nvar*sizeof(double));
        if (condlimit2==NULL) { tilanpuute(); return(-1); }
        condcases=(char **)muste_malloc(nvar*sizeof(char **));
        if (condcases==NULL) { tilanpuute(); return(-1); }
        condspace=(int *)muste_malloc(32*u*sizeof(int));
        if (condspace==NULL) { tilanpuute(); return(-1); }
        return(1);
        }


static int create_aggfile()
        {
        int i;
        int len,m1,actsar;
        char *p;
        char text[LLENGTH]; // 73 -> LLENGTH 31.8.2006
        char *ptext[1];

        len=0;
        actsar=7;
        if (multiple_aggvars) sprintf(text,"Aggregated from data %s by %d variables",word[2],multiple_aggvars); // RS 5.9.2013
        else sprintf(text,"Aggregated from data %s by variable %s",word[2],word[4]);
        ptext[0]=text;

//Rprintf("\n%s",text);

        p=typestring;
        for (i=0; i<nvar; ++i)
            {
            len+=varlen[i];

            vartype2[i]=p;
            strncpy(vartype2[i],space,actsar+5); vartype2[i][actsar+5]=EOS;
            vartype2[i][1]='A';
            vartype2[i][0]=vartype[i];
            p+=13;
            }
        len=16+1.2*len;
        m1=4+1.2*nvar;
// RS OLD                       0L        
i=fi_create(word[g-1],len,m1,nvar,0,64,actsar+5,1,strlen(text),ptext,varname,varlen,vartype2); // RS 5.9.2013 6->g-1
        return(i);
        }

static int etsi_ehto(char *nimi,int n)
        {
        int i,k;
        char x[LLENGTH],*osa[3];
        char *p;

        i=spfind(nimi);
        if (i<0)
            {
            sprintf(sbuf,"\nCondition '%s' is not given!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        strcpy(x,spb[i]);
        k=split(x,osa,3);
        if (k==0)
            {
            sprintf(sbuf,"\nIncomplete condition '%s'!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        p=strchr(osa[0],':');
        if (p==NULL)
            {
            condtype[n]=0;  /* IND */
            condvar[n]=varfind(&d1,osa[0]);
            if (condvar[n]<0) return(-1);
            condlimit1[n]=condlimit2[n]=1.0;
            if (k>1) condlimit1[n]=condlimit2[n]=atof(osa[1]);
            if (k>2) condlimit2[n]=atof(osa[2]);
            }
        else
            {
            strcpy(x,spb[i]);
            p=strchr(x,':'); *p=EOS; // ++p;
            condtype[n]=1; /* CASES */
            condvar[n]=varfind(&d1,osa[0]);
            if (condvar[n]<0) return(-1);
            *p=',';
            strcat(p,",");
            condcases[n]=pcond;
            strcpy(pcond,p);
// Rprintf("\npcond=%s|\n",pcond); getch();
            pcond+=strlen(p)+1;
            }
        return(1);
        }

static int tutki_ehto2(char *s)
        {
        int h;
        int nro;


        for (h=0; h<ncond; ++h)
            {
            if (strcmp(condname[h],s)==0) break;
            }
        nro=h;
        if (h==ncond)
            {
            condname[h]=pcond;
            strcpy(pcond,s); pcond+=strlen(s)+1;
            h=etsi_ehto(s,ncond); if (h<0) return(-1);
            ++ncond;
            }
        return(nro);
        }

static int tutki_ehto(char *s,int i)
        {
        int k,n,h;
        char condnimi[LLENGTH];
        int ci,ck,ok;

        ci=condnr[i]=condcount;
        k=0; n=0;
        while (s[k])
            {
            switch (s[k])
                {
              case '*':
                condnimi[n]=EOS;
                h=tutki_ehto2(condnimi);
                if (h<0) return(-1);
                condspace[condcount++]=h;
                n=0;
                break;
              case '+':
                condnimi[n]=EOS;
                h=tutki_ehto2(condnimi);
                if (h<0) return(-1);
                condspace[condcount++]=h;
                condspace[condcount++]=C_PLUS;
                n=0;
                break;
              case '!': condspace[condcount++]=C_NEG; break;
              default:
                condnimi[n++]=s[k];
                break;
                }
            ++k;
            }
        condnimi[n]=EOS;
        h=tutki_ehto2(condnimi);
        if (h<0) return(-1);
        condspace[condcount++]=h;
        condspace[condcount++]=C_END;

        for (k=0; k<i; ++k)   /* 27.8.93 */
            {
            ck=condnr[k];
            if (ck==-1) continue;
            h=0; ok=0;
            while (1)
                {
                if (condspace[ci+h]!=condspace[ck+h]) break;
                if (condspace[ck+h]==C_END) { ok=1; break; }
                ++h;
                }
            if (ok)
                {
                condcount=ci;
                condnr[i]=condnr[k];
                return(1);
                }
            }
        return(1);
        }


static int find_task(char *s)
        {
        int i;

        i=0;
        while (*taskname[i]!=EOS)
            {
            if (muste_strcmpi(s,taskname[i])==0) break;
            ++i;
            }
        if (*taskname[i]==EOS) return(-1);
        return(i);
        }

static int varlist_start()
        {
        int i,k;
        char x[LLENGTH],*osa[1];

        i=r1+r;
        while (i<=r2)
            {
            edread(x,i);
            k=split(x+1,osa,1);
            if (k==0) { ++i; continue; }
            if (strcmp(osa[0],"VARIABLES:")==0) break;
            ++i;
            }
        if (i>r2) return(-1);
        return(i);
        }


static int read_varlist()
        {
        int i,k,h, ii;
        char type[LLENGTH];
        char x[LLENGTH],*osa[5];
        char *pname;
        char *p;
        char *ppar;
        char xxx[LLENGTH];

        ncond=0;
        worksize=0;
        condcount=0;
        varline=varlist_start();
        if (varline<0)
            { sur_print("\nVARIABLES list missing!"); WAIT; return(-1); }
        ++varline;

        i=0;
        while (1)
            {
            if (varline+i>r2)
                {
                sur_print("\nEND line missing at the end of the VARIABLES list!");
                WAIT; return(-1);
                }
            edread(x,varline+i);
            k=split(x+1,osa,1);
            if (strcmp(osa[0],"END")==0) break;
            ++i;
            }
        nvar=i;
        i=varaa_tilat(); if (i<0) return(-1);
        pname=namestring;
        pcond=condstring;

        i=0;
        while (1)
            {
            edread(x,varline+i);
            k=space_split(x+1,osa,5);
            for (h=k-1; h>=0; --h)
                {
                if (strcmp(osa[h],"/")==0) break;
                }
            if (h>=0) k=h;
            if (k==0)
                {
                sur_print("\nEmpty line in VARIABLES list or END missing!");
                WAIT; return(-1);
                }
            if (strcmp(osa[0],"END")==0) break;
            if (k<3)
                {
                sprintf(sbuf,"\nIncomplete line %d in VARIABLES list!",varline+i);
                sur_print(sbuf); WAIT; return(-1);
                }
            p=strchr(osa[0],':');
            if (p==NULL) strcpy(type,"-"); else { *p=EOS; strcpy(type,p+1); }
            if (strchr("-1248S",*type)==NULL)
                {
                sprintf(sbuf,"\nError on line %d. Permitted types 1,2,4,8 and S<length>.",
                                        varline+i);
                sur_print(sbuf); WAIT; return(-1);
                }
            p=strchr(osa[0],'('); // RS 17.1.2014
            if (p==NULL) strcpy(pname,osa[0]);
            else
            	{ 
            	*p=EOS; 
            	strcpy(pname,osa[0]);
            	ii=strlen(pname);
            	while (ii<9) { strcat(pname," "); ii++; }
            	strcat(pname,"(");
            	strcat(pname,p+1);
            	}
//Rprintf("\npname:|%s|",pname);            
            varname[i]=pname; pname+=strlen(pname)+1;
            varnr[i]=i;

// Rprintf("\nvarname: %s",varname[i]);

            ppar=strchr(osa[1],'('); if (ppar!=NULL) { *ppar=EOS; ++ppar; }
            /* ppar osoittaa param.listaa */

            task[i]=find_task(osa[1]);
            if (task[i]<0)
                {
                sprintf(sbuf,"\nUnknown aggregation operation %s (for %s)!",
                                              osa[1],varname[i]);
                sur_print(sbuf); WAIT; return(-1);
                }

            w[i]=worksize;
            worksize+=n_work[task[i]];

            if (strcmp(osa[2],"-")==0) keyvar[i]=-1;
            else
                {
                keyvar[i]=varfind(&d1,osa[2]);
                if (keyvar[i]<0) return(-1);
                }
                
// Rprintf("\ni: %d, type: %c",i,*type);                
            if (*type=='-')
                {
                if (vartypes[task[i]]=='=' && keyvar[i]>=0)
                    {
                    vartype[i]=d1.d2.vartype[keyvar[i]][0];
                    varlen[i]=d1.d2.varlen[keyvar[i]];
                    }
                else
                    {
                    vartype[i]=vartypes[task[i]];
                    varlen[i]=vartype[i]-'0';
                    }
                }
            else
                {
                vartype[i]=*type;
                if (*type=='S') varlen[i]=atoi(type+1);
                else varlen[i]=vartype[i]-'0';
                }

// Rprintf("\nvartype[%d]=%c",i,vartype[i]);                
                
            keytype[i]=0;
            if (keytypes[task[i]]==1)
                {               
                if (vartype[i]=='S') keytype[i]=1;
                if (task[i]==XXVALUES) // RS 28.2.2013
                    {
                    vartype[i]='2';
                    varlen[i]=vartype[i]-'0';
                    }
                }
            condnr[i]=-1;
            if (k>=4)
                {
                strcpy(xxx,osa[3]);
                h=bool_norm(xxx); if (h<0) return(-1);
                h=tutki_ehto(xxx,i); if (h<0) return(-1);
                }

            if (order_statistics[task[i]])
                { h=order_stat1(i,&task[i],ppar); if (h<0) return(-1); }
                         /* task[i]-muutos MEDIAN -> FRACTILE(0.5) */

            if (xy_statistics[task[i]])
                { h=xy_stat1(i,ppar); if (h<0) return(-1); }
            ++i;
            }
        return(1);
        }


static int remarks()
        {
        init_remarks();
        rem_pr("Usage:");
        rem_pr("FILE AGGR <data> BY <aggr_variable> TO <new_data_file>");
        rem_pr("with a VARIABLES list forms a new data file");
        rem_pr("by aggregating consecutive observations (with a same value");
        rem_pr("in <aggr_variable>) according to different rules (functions).");
        rem_pr(" ");
        rem_pr("The VARIABLES list is given below the FILE AGGR operation line in the form:");
        rem_pr("VARIABLES:");
        rem_pr("A1 Function1 X1 Condition1");
        rem_pr("A2 Function2 X2 Condition2");
        rem_pr(".. ......... .. .......... ");
        rem_pr("END");
        rem_pr(" ");
        rem_pr("<data> must be sorted by <aggr_variable> before using FILE AGGR.");
        rem_pr("In the VARIABLES list, A1,A2,... are names of aggregated variables.");
        rem_pr("Also the type of a variable can be given as Sum:8, Name:S16.");
        rem_pr("Possible functions are listed on the next page.");
        rem_pr("X1,X2,... are names of variables in <data> to be aggregated.");
        rem_pr("Conditions are given in the form A1*A2*...+B1*B2*... (as in SELECT)");
        rem_pr("Each of terms A1,A2,etc. is given as a condition of type IND or CASES.");
        wait_remarks(1);

        rem_pr("Functions in FILE AGGR:");
        rem_pr("N        Number of cases");
        rem_pr("         Example: Nbig   N  -  Big / Big=Popul,30000,500000");
        rem_pr("         `-' above means that N assumes no X variable.");
        rem_pr("SUM      Sum of observations");
        rem_pr("         Example: Popul:8  SUM  Popul");
        rem_pr("MEAN     Arithmetic mean of observations");
        rem_pr("STDDEV   Standard deviation");
        rem_pr("MIN      Minimum value");
        rem_pr("MAX      Maximum value");
        rem_pr("#VALUES  # of different values");
        rem_pr("FIRST    Value of the first observation within the aggregate");
        rem_pr("         Typically, the name of the aggregate is copied by FIRST.");
        rem_pr("         Example: Province FIRST Province");
        rem_pr("LAST     Value of the last observation within the aggregate");
        rem_pr("NMISS    Number of missing observations");
        rem_pr("SUMS     Sum of observations. If any are missing, the result is missing.");
        rem_pr("To be continued on the next page");
        wait_remarks(1);

        rem_pr("Functions in FILE AGGR (Continued)");
        rem_pr("MEDIAN        Median of the observations");
        rem_pr("MODE          Mode of the observations");
        rem_pr("FRACTILE(p)   p-fractile of the observations (0<=p<=1)");
        rem_pr("ORDER(k)      Observation Xk in the ordered sample X1<=X2<=...<=Xn");
        rem_pr("              If k<0, observation X(n+k-1)");
        rem_pr("              Example: ORDER(-1) is same as MAX.");
        rem_pr("ORDERN(k,V)   Value of variable V for the kth observation");
        rem_pr("              Example: Maxcomm ORDERN(-1,Commune) Popul");
        rem_pr("TRIM(k)       Trimmed mean when k largest and least cases are omitted");
        rem_pr("TRIMP(p)      Trimmed mean on rejection probability level p (0<p<0.5)");
        rem_pr("CORR(V)       Correlation of the X and the V variable");
        rem_pr("SLOPE(V)      Slope a in the regression model X=a*V+b+eps");
        rem_pr("INTERCEPT(V)  Intercept constant b in the above begression model.");
        rem_pr("              In the 3 last functions V can be replaced by ORDER i.e.");
        rem_pr("              order 1,2,...,n of observation within the aggregate.");
        wait_remarks(2);
        return(1); // RS CHA exit -> return
        }



void muste_file_aggr(int argc,char *argv[])
        {
        int i;
        int new_file; // RS 17.1.2014

// RS ADD variable init
worksize=0;
nvar=0;
maxvar=0;
ncond=0;
condcount=0;
varline=0;
aggvar=0;
aggtype=0;
prind=1;
n_ordvar=0;
maxord=MAXORD;
n_ordkey=0;
n_rec=0;
multiple_aggvars=0;


namestring=NULL;
varnr=NULL;
vartype=NULL;
varlen=NULL;
typestring=NULL;
task=NULL;
keyvar=NULL;
keytype=NULL;
condstring=NULL;
pcond=NULL;
condnr=NULL;
condtype=NULL;
condvar=NULL;
condlimit1=NULL;
condlimit2=NULL;
condspace=NULL;
workspace=NULL;
w=NULL;
ok=NULL;
ordvar=NULL;
ordpar=NULL;
ordpar2=NULL;
ordnr=NULL;
ordkey=NULL;
ordcond=NULL;
n_cases=NULL;
hav=NULL;
jhav=NULL;
x_var=NULL;
new_file=0;


        if (argc==1) return;
        s_init(argv[1]);
        if (g<7) { remarks(); return; }
        if (muste_strcmpi(word[3],"BY")!=0) { remarks(); return; }
        if (muste_strcmpi(word[g-2],"TO")!=0) // RS 5.9.2013 5->g-2
            { 
            if (muste_strcmpi(word[g-3],"TO")==0 && muste_strcmpi(word[g-2],"NEW")==0) // RS 6.9.2013
                {
                word[g-2]=word[g-1]; g--;
                new_file=1; // RS 17.1.2014
                }
            else
                { 
                remarks(); 
                return;
                } 
            } 

        maxvar=MAXVAR;
        i=data_open3(word[2],&d1,1,1,1,0); if (i<0) return;
        if (d1.type!=2)
            {
            sprintf(sbuf,"\n%s must be a Survo data file!",word[2]);
            sur_print(sbuf); WAIT; return;
            }
        i=spec_init(r1+r-1); if (i<0) return;
        i=conditions(&d1); if (i<0) return;

        if (muste_strcmpi(word[2],word[g-1])==0) // RS 5.9.2013 6->g-1
            {
            sprintf(sbuf,"\nThe original file %s cannot be overwritten",word[2]);
            sur_print(sbuf);
            sur_print("\nby the aggregated file!");
            WAIT; return;
            }

        aggvar=varfind(&d1,word[4]);
        if (aggvar<0)
            {
            sprintf(sbuf,"\nVariable %s not found!",word[4]);
            sur_print(sbuf); WAIT; 
            return;
            }
        aggtype=d1.d2.vartype[aggvar][0];
        
        if (g>7) // RS 5.9.2013
            {
            aggtype='S';
            i=4;
            while (i<g && strcmp(word[i],"TO")!=0)
                {
                multiple_aggvars++;
                aggvars[i-4]=varfind(&d1,word[i]);
                if (aggvars[i]<0)
                    {
                    sprintf(sbuf,"\nVariable %s not found!",word[i]);
                    sur_print(sbuf); WAIT; 
                    return;
                    }
                i++;
                }
            }

        i=read_varlist(); if (i<0) { sur_print("\nFILE AGGR ERROR! (read_varlist)"); WAIT; return; }
		if (new_file) // RS 17.1.2014
			{
		    strcpy(sbuf,word[g-1]);
        	if (!muste_is_path(sbuf)) 
            { strcpy(sbuf,edisk); strcat(sbuf,word[g-1]); }         
        	muste_append_path(sbuf,".SVO");
			sur_delete(sbuf); 
			}
        i=create_aggfile(); if (i<0) { sur_print("\nFILE AGGR ERROR! (aggfile)"); WAIT; return; }        
        i=init_workspace(); if (i<0) { sur_print("\nFILE AGGR ERROR! (init_workspace)"); WAIT; return; }
        i=fi_open(word[g-1],&d2); if (i<0) { sur_print("\nFILE AGGR ERROR! (fi_open)"); WAIT; return; } // RS 5.9.2013 6->g-1
        i=aggregate(); if (i<0) { sur_print("\nFILE AGGR ERROR! (aggregate)"); WAIT; return; }
        del_ordfile();
        data_close(&d1); // RS ADD
        }
