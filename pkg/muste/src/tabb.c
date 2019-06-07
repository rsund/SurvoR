#include "muste.h"
/* &tabb.c 29.3.1986/SM (21.10.1992) (22.7.1994) (19.6.1997)
   TAB <data>,L
*/

#include <stdio.h>
#include <stdlib.h>
// #include <malloc.h>
// #include <conio.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

// tab.h
#define MAXDIM 200
#define MAXT 5000
#define MAXSPACE 50000
#define MAXCNAMES 2000
#define NTAB 200

typedef unsigned int FREQ;
#define ZERO 0
#define CELL_ONE 2147483647

static SURVO_DATA d;

static int dim;                    /* dimension of the tables */
static int nclassf;                /* # of classifiers */
static int ntab;                   /* # of tables */
static int colvar;                 /* # of column variables */
static unsigned int nspace;        /* # of cells for column variables */
static int classifier[MAXDIM];     /* indices of classifiers */
static int ctype[MAXDIM];          /* types of classifiers: 1=num 0=string */
static int nc[MAXDIM];             /* # of classes */
static int cumnc[MAXDIM];          /* cumulative # of classes */
static unsigned int ncc[MAXDIM];   /* products of nc[i] */
static int class[MAXDIM];          /* classes of an observation */
static char *cname[MAXT];          /* class names cname[cumnc[i]+j] */
static double climit[MAXT];        /* upper class limits climit[cumnc[i]+j] ctype=1 */
static char *cvalue[MAXT];         /* class values cvalue[cumnc[i]+j] ctype=0 */
static char namelist[LLENGTH];     /* names of classifiers as a list */
static char *varname[MAXDIM+1];    /* names of variables (pointers to namelist) */

static char clist[MAXSPACE];       /* list of class names and values */
static char *fspace;               /* pointer to allocated f space */
static FREQ *f[NTAB];              /* tables of frequencies */
static int cellvar;                /* cell variable */
static char cellformat[32];        /* format for sums etc. */
static char *sumspace;             /* pointer to allocated sum,sum2 space */
static double *sum[NTAB];          /* tables of sums */
static double *sum2[NTAB];         /* tables of sums of squares */
static int celloption;             /* 0=MEAN+SD (default), 1=SUM, 2=FSUM */

static FREQ total;
static FREQ missing[NTAB];
static FREQ misscol;

static int results_line;
static int weight_var;

// static char classlist[MAXSPACE];  /* 22.7.1994 */
// static char *classname[MAXCNAMES];
static int tab_labels=1;
static int miss_classes=0;

static int variables();
static int space_error();
static int compute_ncc();
static int space_allocation();
static int printout();
static int table_name(char *name,char *s,int index);
static int cell_variable();
static int weight_variable();
static int compute_frequencies();
static int print_ftable(char *name,int line,char *eout,int dim,int *nc,int *cumnc,
             int *ctype, FREQ *f,FREQ total,FREQ missing,char **varname,
             char **cname,int colvar,int minwidth,int isum,int ipros,int tab_labels);
static char *spois(char *s);
static int tline_init(char *x);
static int tline_write(char *s,char *x,int col);
static int not_enough_memory();
static int comp_chi2(FREQ *f,int m,int n,double *chi2);
static int print_stable(char *name,int line,char *eout,int dim,int *nc,
       int *cumnc,int *ctype,FREQ *f,FREQ total,FREQ missing,
       char **varname,char **cname,int colvar,int minwidth,
       char *cellvar,char *cellformat,double *sum,double *sum2,
       int celloption,int tab_labels);
static int skaala_arvot(char *s,char **osa,int max);

/**********************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "VARIABLES", "LABELS", "CELL", "WEIGHT", "CHI2",
                 "RESULTS", "PRIND", "!" };
char **specs=specs0;
************************/
/***************
main(argc,argv)
int argc; char *argv[];
******************/

void muste_tabb(char *argv)
        {
        int i;

//      if (argc==1) return;
        s_init(argv);
        if (g<2)
            {
            sur_print("\nUsage: TAB <data>,L");
            WAIT; return;
            }
        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }

        total=ZERO;
        misscol=0;

        i=sp_init(r1+r-1);
        if (i<0)
            {
            sur_print("\nToo many specifications!"); WAIT;
            return;
            }
        i=data_read_open(word[1],&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;
        i=variables(); if (i<0) return;
        compute_ncc();
        i=cell_variable(); if (i<0) return;
        i=weight_variable(); if (i<0) return;
        i=space_allocation(); if (i<0) return;
        i=compute_frequencies(); if (i<0) return;

        i=printout(); if (i<0) return;


        data_close(&d);
        s_end(argv);
        }

static int variables()
        {
        int i,k,n;
        char classlist[LLENGTH];
        char *classname[MAXCNAMES];
        int cum=0;
        char *p,*q,*r;
        int type;
   //   char x[LLENGTH];

        p=clist;
        i=spfind("VARIABLES");
        if (i<0 || *spb[i]==EOS)
            {
            sur_print("\nVARIABLES=<list of classifiers> missing!");
            WAIT; return(-1);
            }

        strcpy(namelist,spb[i]);
        colvar=0;
        q=namelist;
        while ((q=strchr(q,':'))!=NULL) { ++colvar; *q=','; ++q; }
        nclassf=split(namelist,varname,MAXDIM+1);
        if (nclassf>MAXDIM)
            {
            sprintf(sbuf,"\nMax. # of classifiers = %d",MAXDIM);
            sur_print(sbuf); WAIT; return(-1);
            }
        dim=colvar+1;
        ntab=nclassf-colvar;

        for (i=0; i<nclassf; ++i)
            {
            classifier[i]=varfind(&d,varname[i]);
            if (classifier[i]<0) return(-1);
            k=spfind(varname[i]);
            if (k<0 || *spb[k]==EOS)
                {
                sprintf(sbuf,"\n%s=<list of class limits/values> missing!",varname[i]);
                sur_print(sbuf); WAIT; return(-1);
                }

            strcpy(classlist,spb[k]);
            n=split(classlist,classname,MAXCNAMES);
            if (*classlist=='/')
                {
                type=ctype[i]=0;
                nc[i]=n;
                if (d.vartype[classifier[i]][0]!='S')
                    {
                    sur_print("\n/classes only for string variables!");
                    sprintf(sbuf,"\n%.8s is not a string variable!",
                                d.varname[classifier[i]]); sur_print(sbuf);
                    WAIT; return(-1);
                    }
                }
            else
                {
                if (n==1)
                    {
                    n=skaala_arvot(classlist,classname,MAXCNAMES);
                    if (n<0) return(-1);
                    }
                if (n<2)
                    {
                    sprintf(sbuf,"\nInsufficient classification for %.8s",
                                    d.varname[classifier[i]]);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                type=ctype[i]=1;
                nc[i]=n-1;
                }
            cumnc[i]=cum;
            cum+=n;
            if (cum>MAXT) { space_error(); return(-1); }
            for (k=0; k<n; ++k)
                {
                cname[cumnc[i]+k]=p;
                q=strchr(classname[k],'(');
                if (q!=NULL)
                    {
                    r=strchr(q,')');
                    if (r==NULL)
                        {
                        sprintf(sbuf,"\n) missing in %s",classname[k]);
                        sur_print(sbuf); WAIT; return(-1);
                        }
                    *r=EOS;
                    *q=EOS;
                    ++q;
                    if (p-clist>MAXSPACE-strlen(q)-1) { space_error(); return(-1); }
                    while (*q) *p++=*q++; *p++=EOS;
                    }
                cvalue[cumnc[i]+k]=p;
                q=classname[k];
                if (type)
                    {
                    if (k==n-1 && muste_strcmpi(q,"MISSING")==0)
                        {
                        climit[cumnc[i]+k]=MISSING8;
                        miss_classes=1;
                        }
                    else
                        climit[cumnc[i]+k]=atof(q);
                    }
                if (p-clist>MAXSPACE-strlen(q)-1) { space_error(); return(-1); }
                while (*q) *p++=*q++; *p++=EOS;
                } /* k */
            } /* i */
/*
 Rprintf("\nnclassf=%d colvar=%d dim=%d ntab=%d",nclassf,colvar,dim,ntab); getch();
 for (i=0; i<nclassf; ++i)
     {
     Rprintf("\nclassifier %d  %d %.8s",i+1,classifier[i],
                        d.varname[classifier[i]]);
     Rprintf("\nnc=%d ctype=%d",nc[i],ctype[i]);
     for (k=0; k<nc[i]+ctype[i]; ++k)
        Rprintf("\n%s  %s  %g",cvalue[cumnc[i]+k],cname[cumnc[i]+k],climit[cumnc[i]+k]);
     getch();
     }
*/
        return(1);
        }

static int space_error()
        {
        sur_print("\nNot enough space for all limits!");
        WAIT; return(1);
        }

static int compute_ncc()
        {
        int i;

        nspace=1;
        ncc[dim-2]=1;
        for (i=dim-3; i>=0; --i)
            { nspace*=nc[i+1]; ncc[i]=nspace; }
        nspace*=nc[0];
        return(1);
        }

static int space_allocation()
        {
        int i;
        unsigned int rivit;
 //     unsigned int tila;
        char *p;

        rivit=0;
        for (i=colvar; i<nclassf; ++i) rivit+=nc[i];
        fspace=muste_malloc((unsigned int)(rivit*nspace*sizeof(FREQ)));
        if (fspace==NULL)
            {
            sur_print("\nNot enough memory for frequencies!");
            WAIT; return(-1);
            }
        p=fspace;
        for (i=colvar; i<nclassf; ++i)
            {
            f[i]=(FREQ *)p;
            p+=nc[i]*nspace*sizeof(FREQ);
            }
/*  Rprintf("\ntilat: %u %u",rivit*nspace*sizeof(FREQ),p-fspace); getch();
*/
        if (cellvar<0) return(1);
        sumspace=muste_malloc((unsigned int)(2*rivit*nspace*sizeof(double)));
        if (sumspace==NULL)
            {
            sur_print("\nNot enough memory for means etc.!");
            WAIT; return(-1);
            }
        p=sumspace;
        for (i=colvar; i<nclassf; ++i)
            {
            sum[i]=(double *)p;
            p+=nc[i]*nspace*sizeof(double);
            sum2[i]=(double *)p;
            p+=nc[i]*nspace*sizeof(double);
            }
/*  Rprintf("\ntilat: %u %u",2*rivit*nspace*sizeof(double),p-sumspace); getch();
*/
        return(1);
        }


static int printout()
        {
        int i;
     // unsigned int u;
        char name[LNAME];
     // char x[16];
        int colv;
     // FREQ max;
        int width;
        char result[LLENGTH];
        int isum;
        int index;
        char y[LLENGTH];
		for (i=0; i<LLENGTH; i++) result[i]=0; // RS ADD 16.10.2012
        colv=colvar;
        if (g>3) colv=atoi(word[3]);
        if (colv<0 || colv>dim) colv=1;

        width=log10((double)total)+1;

        isum=0;
        i=spfind("RESULTS");
        if (i>=0)
            {
            strcpy(result,spb[i]);
            if (strstr(result,"CSUM")!=NULL) ++isum;
            if (strstr(result,"RSUM")!=NULL) isum+=2;
            }
        i=spfind("LABELS");
        if (i>=0)
            tab_labels=atoi(spb[i]);
        index=0;
        for (i=colvar; i<nclassf; ++i)
            {
            int k;


            nc[dim-1]=nc[i];
            cumnc[dim-1]=cumnc[i];
            ctype[dim-1]=ctype[i];
            varname[dim-1]=varname[i];


            if (cellvar<0)
                {
                table_name(name,word[1],++index);
                k=print_ftable(name,results_line,eout,dim,nc,cumnc,ctype,
                               f[i],total,missing[i]+misscol,
                               varname,cname,colv,width,isum,0,tab_labels);
                if (results_line && k>1) results_line=k;
                if (strstr(result,"C%")!=NULL)
                    {
                    table_name(name,word[1],++index);
                    k=print_ftable(name,results_line,eout,dim,nc,cumnc,ctype,
                               f[i],total,missing[i]+misscol,
                               varname,cname,colv,width,isum,1,tab_labels);
                    if (results_line && k>1) results_line=k;
                    }
                if (strstr(result,"R%")!=NULL)
                    {
                    table_name(name,word[1],++index);
                    k=print_ftable(name,results_line,eout,dim,nc,cumnc,ctype,
                               f[i],total,missing[i]+misscol,
                               varname,cname,colv,width,isum,2,tab_labels);
                    if (results_line && k>1) results_line=k;
                    }
                if (strstr(result,"T%")!=NULL)
                    {
                    table_name(name,word[1],++index);
                    k=print_ftable(name,results_line,eout,dim,nc,cumnc,ctype,
                               f[i],total,missing[i]+misscol,
                               varname,cname,colv,width,isum,3,tab_labels);
                    if (results_line && k>1) results_line=k;
                    }
                }
            else
                {
                if (cellvar==CELL_ONE) strcpy(y,"1"); else strcpy(y,d.varname[cellvar]);
                table_name(name,word[1],++index);
                k=print_stable(name,results_line,eout,dim,nc,cumnc,ctype,
                               f[i],total,missing[i]+misscol,
                               varname,cname,colv,width,
                               y,cellformat,sum[i],sum2[i],celloption,tab_labels);
                }
            if (results_line && k>1) results_line=k;
            else results_line=0;
            }
        return(1);
        }

static int table_name(char *name,char *s,int index)
        {
        char x[10];

        strcpy(name,s);
        muste_itoa(index,x,10);
        strcat(name,x);
        return(1);
        }

static int cell_variable() /* CELL=<cellvar>,<cellformat>,<celloption>  */
                           /*                             SUM or FSUM   */
                           /*                             1      2      */
        {
        int i;
        char x[LLENGTH], *w[3];

        cellvar=-1;
        i=spfind("CELL");
        if (i<0) return(1);
        strcpy(x,spb[i]);
        i=split(x,w,3);
        if (i==0)
            {
            sur_print("\nError in CELL specification!");
            WAIT; return(-1);
            }
        if (strcmp(w[0],"1")==0) cellvar=CELL_ONE;
        else
            {
            cellvar=varfind(&d,w[0]);
            if (cellvar<0) return(-1);
            }
        *cellformat=EOS;
        if (i>1) strcpy(cellformat,w[1]);
        celloption=0;
        if (i>2)
            {
            if (muste_strcmpi(w[2],"SUM")==0) celloption=1;
            else if (muste_strcmpi(w[2],"FSUM")==0) celloption=2;
            }
        return(1);
        }

static int weight_variable() /* 16.6.90 */
        {
        int i;
        char x[LLENGTH];

        weight_var=-1;
        i=spfind("WEIGHT");
        if (i<0) return(1);
        strcpy(x,spb[i]);
        weight_var=varfind(&d,x);
        if (weight_var<0) return(-1);
        return(1);
        }

static int compute_frequencies()
        {
        int i,k;
        unsigned int u;
        long j;
        double x;
        int cum;
        int miss;
        char s[LLENGTH];
        int len;
        char *p;
        unsigned int v;
        int prind=0;
        double weight;

        for (i=colvar; i<nclassf; ++i)
            {
            for (u=0; u<nspace*nc[i]; ++u) f[i][u]=ZERO;
            if (cellvar>=0)
                for (u=0; u<nspace*nc[i]; ++u) sum[i][u]=sum2[i][u]=0.0;
            missing[i]=ZERO;
            }
        total=ZERO;

        sur_print("\nComputing frequencies etc.: ");

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;

            if (weight_var>=0)
                {
                data_load(&d,j,weight_var,&weight);
                if (weight==MISSING8) continue;
                total+=weight;
                }
            else ++total;
            miss=0;
            for (i=0; i<nclassf; ++i)
                {
                class[i]=-1;
                cum=cumnc[i];
                if(ctype[i])
                    {
                    data_load(&d,j,classifier[i],&x);
     if ((!miss_classes && x==MISSING8) || x<climit[cum] || x>climit[cum+nc[i]])
                        { if (i<colvar) miss=1; continue; }
                    for (k=1; k<=nc[i]; ++k)
                        {
                        if (x<=climit[cum+k]) { class[i]=k-1; break; }
                        }
                    }
                else
                    {
                    data_alpha_load(&d,j,classifier[i],s);
                    p=NULL;
                    len=strlen(s); while (s[len-1]==' ' && len>0) s[--len]=EOS;
                    if (len) for (k=0; k<nc[i]; ++k)
                        {
                        p=cvalue[cum+k];
                        if (*(p+1)=='-' && *(p+2)==EOS)
                            { class[i]=k; break; }
                        while ((p=strchr(p,*s))!=NULL)
                            {
                            if (*(p-1)!='/') { ++p; continue; }
                            if (strncmp(s,p,len)==0 &&
                                   (p[len]=='/' || p[len]==EOS) )
                                { class[i]=k; break; }
                            ++p;
                            }
                        if (p!=NULL) break;
                        }
                    if (p==NULL && i<colvar) miss=1;
                    }
                if (miss) break;
                } /* i */

            if (miss) { ++misscol; continue; }

            if (prind) { sprintf(sbuf," %ld",j); sur_print(sbuf); }
        //  if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }

            v=0; for (i=0; i<colvar; ++i) v+=class[i]*ncc[i];

            for (i=colvar; i<nclassf; ++i)
                {
                register unsigned int z;

                if (class[i]<0) { ++missing[i]; continue; }

                z=v*nc[i]+class[i];
                if (cellvar<0)
                    {
                    if (weight_var<0) ++f[i][z];
                    else f[i][z]+=weight;
                    }
                else
                    {
                    if (cellvar==CELL_ONE) x=1.0;
                    else
                        {
                        data_load(&d,j,cellvar,&x);
                        if (x==MISSING8) continue;
                        }
                    if (weight_var<0) { ++f[i][z]; sum[i][z]+=x; sum2[i][z]+=x*x; }
                    else
                        {
                        f[i][z]+=weight;
                        sum[i][z]+=weight*x;
                        sum2[i][z]+=weight*x*x;
                        }
                    }

                }

            } /* j */

/*
   Rprintf("\nFrekvenssit:");
    for (i=colvar; i<nclassf; ++i)
        {
        Rprintf("\nFrekv.%d",i);
        for (u=0; u<nspace*nc[i]; ++u) Rprintf(" %u",f[i][u]);
        }
   getch();
*/
        return(1);
        }


/* tabprint.c 30.3.1986/SM (13.12.1987) (22.7.1994)
*/


#define FREQ0 0
extern char **spb;

static int print_ftable(char *name,int line,char *eout,int dim,int *nc,int *cumnc,
             int *ctype, FREQ *f,FREQ total,FREQ missing,char **varname,
             char **cname,int colvar,int minwidth,int isum,int ipros,int tab_tab_labels)
                                                                    /* 22.4.94 */
// char *name;              /* name of table */
// int line;                /* first edit line for table */
// char *eout;              /* output file (NULL=no output file) */
// int dim;                 /* dimension of the table */
// int *nc;                 /* # of classes */
// int *cumnc;              /* cumulative # of classes */
// int *ctype;              /* types of classifiers: 1=num 0=string */
// FREQ *f;                 /* table of frequencies */
// FREQ total;              /* total # of obs. (0=not to be displayed) */
// FREQ missing;            /* # of missing ogservations */
// char **varname;          /* names of classifiers */
// char **cname;            /* names of classes cname[cumnc[i]+j] */
// int colvar;              /* # of column classifiers */
// int minwidth;            /* min.width of column */
// int isum;                /* 0=no 1=col.sums 2=row sums 3=both */
// int ipros;               /* 0=no 1=C% 2=R% 3=T% */
// int tab_tab_labels;              /* 0=no TABLE definition, no edit line labels */
        {
        extern char *spois();
        extern double muste_cdf_chi2();
        int i,k,h,m,z;
        unsigned int nlines;     /* # of freq lines */
        unsigned int lin;
        int varwidth[MAXDIM];
        int starwidth;
        int colwidth;
        int freqcol;
        int len;
        char x[LLENGTH];
        int col,coldiff;
        int cum;
        int class[MAXDIM];
        int change[MAXDIM];
        char value[32];
        char ch1,ch2;
        FREQ fsum;
        FREQ *sum;
        double pros=0.0;
        FREQ ftotal;
        int freesum; // 21.1.2001

        sum=NULL;
        nlines=1;
        for (i=colvar; i<dim; ++i) nlines*=nc[i];

        starwidth=1;
        for (i=0; i<dim; ++i)
            {
            h=0;
            for (k=0; k<nc[i]; ++k)
                {
                len=strlen(cname[cumnc[i]+k+ctype[i]]);
                if (len>h) h=len;
                }
            varwidth[i]=h;
            if (i<colvar)
                {
                len=strlen(varname[i]);
                if (len>starwidth) starwidth=len;
                }
            else
                {
                len=strlen(varname[i]);
                if (len>varwidth[i]) varwidth[i]=len;
                }
            }

        freqcol=0; for (i=colvar; i<dim; ++i) freqcol+=varwidth[i]+1;
        freqcol+=starwidth+1;

        output_open(eout);
        colwidth=minwidth;
        if (colvar && varwidth[colvar-1]>colwidth) colwidth=varwidth[colvar-1];
        if (ipros && colwidth<5) colwidth=5;
        if (isum && colwidth<3) colwidth=3;

        if (tab_labels)
            {
            strcpy(x,"TABLE "); strcat(x,name);
            if (line)
                {
                ch1=muste_next_label('A');
                *value=ch1; value[1]=EOS; edwrite(value,line+1,0);
                ch2=muste_next_label(ch1);
                *value=' '; value[1]=ch1; value[2]=','; value[3]=ch2;
                value[4]=','; value[5]='F'; value[6]=EOS;
                if (isum)
                    {
                    if (isum==1 || isum==3) value[6]='C'; else value[6]='R';
                    value[7]=EOS;
                    if (isum==3) { value[7]='R'; value[8]=EOS; }
                    }
                if (ipros) value[5]='%';

                strcat(x,value);
                k=line+nlines+colvar+1;
                if (k<=r2) { *value=ch2; value[1]=EOS; edwrite(value,k,0); }
                }
            if (total)
                {
                strcat(x,"   N="); sprintf(value,"%u",total); strcat(x,value);
                if (missing)
                    {
                    strcat(x," N(missing)=");
                    sprintf(value,"%u",missing); strcat(x,value);
                    }
                }
            output_line(x,eout,line); if (line) ++line;
            } /* tab_labels */

        for (i=0; i<colvar; ++i)
            {
            cum=cumnc[i]+ctype[i];
            tline_init(x);
            tline_write(varname[i],x,freqcol-starwidth-1);
            h=1; for (k=i+1; k<colvar; ++k) h*=nc[k];
            coldiff=(colwidth+1)*h;
            col=freqcol+colwidth;
            m=1; for (k=0; k<i; ++k) m*=nc[k];
            for (h=0; h<m; ++h)
                for (k=0; k<nc[i]; ++k)
                    {
                    if (i<colvar-1) len=colwidth; else len=strlen(cname[cum+k]);
                    tline_write(cname[cum+k],x,col-len);
                    col+=coldiff;
                    }
            if (i==colvar-1 && (isum==2 || isum==3) )
                tline_write("sum",x,col-3);
            output_line(x,eout,line); if (line) ++line;
            }

        tline_init(x);
        col=0;
        for (i=colvar; i<dim; ++i)
            {
            tline_write(varname[i],x,col);
            col+=varwidth[i]+1;
            }
        for (i=0; i<starwidth; ++i) x[col+i]='*';
        output_line(x,eout,line); if (line) ++line;

        for (i=0; i<dim; ++i) { class[i]=0; change[i]=1; }
        z=0; lin=0;
        m=1; for (i=0; i<colvar; ++i) m*=nc[i];

        ftotal=0; for (k=0; k<m*nlines; ++k) ftotal+=f[k];  /* varmistus */
        if (ipros)
            {
            if (ipros==1)
                {
                sum=(FREQ *)muste_malloc(m*sizeof(FREQ));
                if (sum==NULL) { not_enough_memory(); return(-1); }
                for (k=0; k<m; ++k)
                   { sum[k]=0; for (i=0; i<nlines; ++i) sum[k]+=f[k*nlines+i]; }
                freesum=0;
                }
            if (ipros==2)
                {
                sum=(FREQ *)muste_malloc(nlines*sizeof(FREQ));
                if (sum==NULL) { not_enough_memory(); return(-1); }
                for (i=0; i<nlines; ++i)
                   { sum[i]=0; for (k=0; k<m; ++k) sum[i]+=f[k*nlines+i]; }
                freesum=0;
                }
            }

        while (lin<nlines)
            {
            z=lin;
            tline_init(x);
            col=0;
            for (i=colvar; i<dim; ++i)
                {
                if (change[i])
                    {
                    k=cumnc[i]+ctype[i]+class[i];
                    h=0; if(muste_isnumber(cname[k])) h=varwidth[i]-strlen(cname[k]);
                    tline_write(cname[k],x,col+h);
                    }
                col+=varwidth[i]+1;
                }
            col=freqcol+colwidth;
            fsum=0;
            for (i=0; i<m; ++i)
                {
                fsum+=f[z];
                if (ipros)
                    {
                    switch (ipros)
                        {
                      case 1: if (sum[i]==0) pros=0.0;
                              else pros=(double)f[z]/sum[i];
                              break;
                      case 2: if (sum[lin]==0) pros=0.0;
                              else pros=(double)f[z]/sum[lin];
                              break;
                      case 3: if (ftotal==0) pros=0.0;
                              else pros=(double)f[z]/ftotal;
                        }
                    fconv(100.0*pros,"###.#",value);
                    }
                else
                    sprintf(value,"%u",f[z]);
                tline_write(value,x,col-strlen(value));
                z+=nlines;
                col+=colwidth+1;
                }
            if (isum==2 || isum==3)
                {
                if (ipros)
                    {
                    switch (ipros)
                        {
                      case 1:
                      case 3: if (ftotal==0) pros=0.0;
                              else pros=(double)fsum/ftotal;
                              break;
                      case 2: pros=1.0;
                        }
                    fconv(100.0*pros,"###.#",value);
                    }
                else
                    sprintf(value,"%u",fsum);

                tline_write(value,x,col-strlen(value));
                }
            output_line(x,eout,line); if (line) ++line;

            for (i=0; i<dim; ++i) change[i]=0;
            for (i=dim-1; i>=0; --i)
                {
                ++class[i];
                change[i]=1;
                if (class[i]!=nc[i]) break;
                class[i]=0;
                }
            ++lin;
            }

        if (isum==1 || isum==3)
            {
            tline_init(x);
            tline_write("sum",x,freqcol-4);
            col=freqcol+colwidth;
            for (i=0; i<m; ++i)
                {
                fsum=0;
                for (k=0; k<nlines; ++k) fsum+=f[i*nlines+k];
                if (ipros)
                    {
                    switch (ipros)
                        {
                      case 1: pros=1.0; break;
                      case 2:
                      case 3: if (ftotal==0) pros=0.0;
                              else pros=(double)fsum/ftotal;
                        }
                    fconv(100.0*pros,"###.#",value);
                    }
                else
                    sprintf(value,"%u",fsum);
                tline_write(value,x,col-strlen(value));
                z+=nlines;
                col+=colwidth+1;
                }
            if (isum==3)
                {
                if (ipros) strcpy(value,"100.0");
                else       sprintf(value,"%u",ftotal);
                tline_write(value,x,col-strlen(value));
                }
            output_line(x,eout,line); if (line) ++line;
            }

        if (ipros==1 || ipros==2) { muste_free(sum); freesum=1; }

        if (!ipros && dim==2 && colvar==1 && (nlines-1)*(m-1)>0)
            {
            char sana1[LLENGTH], sana2[LLENGTH];
            int df;

            i=spfind("CHI2"); // 1.5.2001
            if (i<0 || (i>=0 && strcmp(spb[i],"-")!=0))
                {
                comp_chi2(f,nlines,m,&pros);
                fnconv(pros,accuracy-1,sana1);
                df=(nlines-1)*(m-1);
                fconv(1-muste_cdf_chi2(pros,(double)df,1e-7),"1.1234",sana2);
                sprintf(x,"Chi_square=%s df=%d P=%s",spois(sana1),df,sana2);
                output_line(x,eout,line); if (line) ++line;
                }
            }
        tline_init(x);
        output_line(x,eout,line); if (line) ++line;
        output_close(eout);
        if (!freesum) muste_free(sum);
        if (line) return(line);
        return(1);
        }

static char *spois(char *s)
        {
        while (*s==' ') ++s;
        return(s);
        }

static int tline_init(char *x)
        {
        strncpy(x,space,c2); x[c2]=EOS; return(1);
        }

static int tline_write(char *s,char *x,int col)
        {
        char *p;

        if (col>strlen(x)) return(1);
        p=x+col;
        while (*p && *s) *p++=*s++;
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(1);
        }

static int comp_chi2(FREQ *f,int m,int n,double *chi2)
        {
        FREQ *row,*col,tot;
        int i,j;
        double sum,a,d;

        row=(FREQ *)muste_malloc(m*sizeof(FREQ));
        if (row==NULL) { not_enough_memory(); return(-1); }
        col=(FREQ *)muste_malloc(n*sizeof(FREQ));
        if (col==NULL) { not_enough_memory(); return(-1); }

        tot=0;
        for (i=0; i<m; ++i)
            { row[i]=0; for (j=0; j<n; ++j) { row[i]+=f[i+m*j]; tot+=f[i+m*j]; } }
        for (j=0; j<n; ++j) { col[j]=0; for (i=0; i<m; ++i) col[j]+=f[i+m*j]; }


        sum=0.0;
        for (i=0; i<m; ++i)
            for (j=0; j<n; ++j)
                {
                a=(double)row[i]*col[j]/tot;
                if (a==0.0) continue;
                d=f[i+m*j]-a;
                sum+=d*d/a;
                }
        *chi2=sum;
        muste_free(row); muste_free(col);
        return(1);
        }

static int print_stable(char *name,int line,char *eout,int dim,int *nc,
       int *cumnc,int *ctype,FREQ *f,FREQ total,FREQ missing,
       char **varname,char **cname,int colvar,int minwidth,
       char *cellvar,char *cellformat,double *sum,double *sum2,
       int celloption,int tab_labels)
// char *name;              /* name of table */
// int line;                /* first edit line for table */
// char *eout;              /* output file (NULL=no output file) */
// int dim;                 /* dimension of the table */
// int *nc;                 /* # of classes */
// int *cumnc;              /* cumulative # of classes */
// int *ctype;              /* types of classifiers: 1=num 0=string */
// FREQ *f;                 /* table of frequencies */
// FREQ total;              /* total # of obs. (0=not displayed) */
// FREQ missing;            /* # of missing obs. */
// char **varname;          /* names of classifiers */
// char **cname;            /* names of classes cname[cumnc[i]+j] */
// int colvar;              /* # of column classifiers */
// int minwidth;            /* min.width of column */
// char *cellvar;           /* name of cell variable */
// char *cellformat;        /* format for means etc. */
// double *sum;             /* table of sums */
// double *sum2;            /* table of sums of squares */
// int celloption;          /* 0=MEAN+SD, 1=SUM, 2=FSUM */
// int tab_labels;              /* 0=no TABLE definition, no edit line labels */
        {
        int i,k,h,m,z;
        unsigned int nlines;     /* # of freq lines */
        unsigned int lin;
        int varwidth[MAXDIM];
        int starwidth;
        int colwidth;
        int freqcol;
        int len;
        char x[LLENGTH];
        int col,coldiff;
        int cum;
        int class[MAXDIM];
        int change[MAXDIM];
        char value[32];
        char ch1,ch2;
        int meancol;
        double a;

        nlines=1;
        for (i=colvar; i<dim; ++i) nlines*=nc[i];

        starwidth=1;
        for (i=0; i<dim; ++i)
            {
            h=0;
            for (k=0; k<nc[i]; ++k)
                {
                len=strlen(cname[cumnc[i]+k+ctype[i]]);
                if (len>h) h=len;
                }
            varwidth[i]=h;
            if (i<colvar)
                {
                len=strlen(varname[i]);
                if (len>starwidth) starwidth=len;
                }
            else
                {
                len=strlen(varname[i]);
                if (len>varwidth[i]) varwidth[i]=len;
                }
            }

        if (starwidth<4) starwidth=4;  /* space for text 'Mean' */
        freqcol=0; for (i=colvar; i<dim; ++i) freqcol+=varwidth[i]+1;
        freqcol+=starwidth+1;

        output_open(eout);
        colwidth=minwidth;
        if (colvar && varwidth[colvar-1]>colwidth) colwidth=varwidth[colvar-1];
        i=strlen(cellformat); if (i==0) i=accuracy+2;
        if (i>colwidth) colwidth=i;

        if (tab_labels)
            {
            strcpy(x,"TABLE "); strcat(x,name);
            if (line)
                {
                ch1=muste_next_label('A');
                *value=ch1; value[1]=EOS; edwrite(value,line+1,0);
                ch2=muste_next_label(ch1);
                *value=' '; value[1]=ch1; value[2]=','; value[3]=ch2;
                value[4]=','; value[5]=EOS;
                switch (celloption)
                    {
                  case 1: strcat(value,"SUM"); i=1; break;
                  case 2: strcat(value,"FSUM"); i=2; break;
                  default: strcat(value,"FMS"); i=3;
                    }
                strcat(x,value);
                k=line+i*nlines+colvar+1;
                if (k<=r2) { *value=ch2; value[1]=EOS; edwrite(value,k,0); }
                }
            if (total)
                {
                strcat(x,"   N="); sprintf(value,"%u",total); strcat(x,value);
                if (missing)
                    {
                    strcat(x," N(missing)=");
                    sprintf(value,"%u",missing); strcat(x,value);
                    }
                }
            if (celloption==1 || celloption==2) strcat(x, " Sums of ");
            else strcat(x," Mean and SD of ");
            strncat(x,cellvar,8);
            output_line(x,eout,line); if (line) ++line;
            } /* tab_labels */

        for (i=0; i<colvar; ++i)
            {
            cum=cumnc[i]+ctype[i];
            tline_init(x);
            tline_write(varname[i],x,freqcol-starwidth-1);
            h=1; for (k=i+1; k<colvar; ++k) h*=nc[k];
            coldiff=(colwidth+1)*h;
            col=freqcol+colwidth;
            m=1; for (k=0; k<i; ++k) m*=nc[k];
            for (h=0; h<m; ++h)
                for (k=0; k<nc[i]; ++k)
                    {
                    if (i<colvar-1) len=colwidth; else len=strlen(cname[cum+k]);
                    tline_write(cname[cum+k],x,col-len);
                    col+=coldiff;
                    }
            output_line(x,eout,line); if (line) ++line;
            }
        tline_init(x);
        col=0;
        for (i=colvar; i<dim; ++i)
            {
            tline_write(varname[i],x,col);
            col+=varwidth[i]+1;
            }
        meancol=col;
        for (i=0; i<starwidth; ++i) x[col+i]='*';
        output_line(x,eout,line); if (line) ++line;

        for (i=0; i<dim; ++i) { class[i]=0; change[i]=1; }
        z=0; lin=0;

        m=1; for (i=0; i<colvar; ++i) m*=nc[i];
        while (lin<nlines)
            {
            z=lin;
            tline_init(x);
            col=0;
            for (i=colvar; i<dim; ++i)
                {
                if (change[i])
                    {
                    k=cumnc[i]+ctype[i]+class[i];
                    h=0; if(muste_isnumber(cname[k])) h=varwidth[i]-strlen(cname[k]);
                    tline_write(cname[k],x,col+h);
                    }
                col+=varwidth[i]+1;
                }

            if (celloption!=1)
                {
                col=freqcol+colwidth;
                for (i=0; i<m; ++i)
                    {
                    sprintf(value,"%u",f[z]);
                    tline_write(value,x,col-strlen(value));
                    z+=nlines;
                    col+=colwidth+1;
                    }
                output_line(x,eout,line); if (line) ++line;
                }

            z=lin;
            if (celloption!=1) tline_init(x);
            if (celloption==0) tline_write("Mean",x,meancol);
            else tline_write("Sum ",x,meancol);
            col=freqcol+colwidth;
            for (i=0; i<m; ++i)
                {
                if (f[z]==0 && celloption==0) strcpy(value,"-");
                else
                    {
                    if (celloption==0) a=sum[z]/(double)f[z];
                    else a=sum[z];
                    if (*cellformat)
                        fconv(a,cellformat,value);
                    else
                        fnconv(a,accuracy+2,value);
                    }
                tline_write(value,x,col-strlen(value));
                z+=nlines;
                col+=colwidth+1;
                }
            output_line(x,eout,line); if (line) ++line;

            if (celloption==0)
                {
                z=lin;
                tline_init(x);
                tline_write("SD  ",x,meancol);
                col=freqcol+colwidth;
                for (i=0; i<m; ++i)
                    {
                    if (f[z]<2) strcpy(value,"-");
                    else
                        {
                        a=sqrt((sum2[z]-sum[z]*sum[z]/(double)f[z])/
                                            (double)(f[z]-1));
                        if (*cellformat)
                            fconv(a,cellformat,value);
                        else
                            fnconv(a,accuracy+2,value);
                        }
                    tline_write(value,x,col-strlen(value));
                    z+=nlines;
                    col+=colwidth+1;
                    }
                output_line(x,eout,line); if (line) ++line;
                }

            for (i=0; i<dim; ++i) change[i]=0;
            for (i=dim-1; i>=0; --i)
                {
                ++class[i];
                change[i]=1;
                if (class[i]!=nc[i]) break;
                class[i]=0;
                }
            ++lin;
            }

        tline_init(x);
        output_line(x,eout,line); if (line) ++line;
        output_close(eout);
        if (line) return(line);
        return(1);
        }

/* scl.c 8.1.1987/SM (8.1.1987)

*/

extern char sbuf[];

static int skaala_arvot(char *s,char **osa,int max)
        {
        char *p,*q;
        double a,h,b;
        char t[LLENGTH];
        int n;
        char sana[LLENGTH];

        strcpy(t,s);
        p=strchr(t,'('); if (p==NULL) return(1);
        *p=EOS;
        ++p;
        a=atof(t);
        q=strchr(p,')');
        if (q==NULL)
            {
            sprintf(sbuf,"\n) missing in classification %s",s);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (*(q+1)==EOS) return(1);
        *q=EOS;
        h=atof(p); b=atof(q+1);
        strcpy(t,s);
        p=s;
        a-=h;  /* 1.luokan alaraja */
        n=0;
        while (a<=b*(1.0+1e-7))
            {
            if (n>=max)
                {
                sprintf(sbuf,"\nToo many classes in %s",t);
                sur_print(sbuf); WAIT; return(-1);
                }
            fconv(a,"",sana);
            if (p-s>=MAXSPACE-strlen(sana)-1)
                {
                sprintf(sbuf,"\nToo many classes in %s",t);
                sur_print(sbuf); WAIT; return(-1);
                }
            osa[n++]=p;
            q=sana;
            while (*q) *p++=*q++; *p++=EOS;
            a+=h;
            }
        return(n);
        }
