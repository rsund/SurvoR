/* Burt's table from data or table 25.8.1998/kv (16.9.1998)
    converted for Muste 4.8.2011/KV (4.8.2011) (1.9.2011)
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

extern void muste_kv_s_err(char *, ...);
extern void muste_kv_s_disp(char *, ...);
extern void muste_kv_usage_info(void);
extern int  muste_kv_space_split(char *, char **, int);

static int check_params(void);
static int proceed_with_table(void);
static int proceed_with_data(void);
static void no_memory(void);
static int seek_table(void);
static int read_table(void);
static int show_info(void);
static int read_elements(void);
static int binary_table(void);
static int table_to_Z_matrix(void);
static int read_data(void);
static int data_to_Z_matrix();
static int compare(const void *, const void *);
static int make_labels(void);
static void trim2(char *);
static int save_matrices(void);
static int burt_table(void);
static int classifiers(void);
static int Z_matrix(void);

       enum source_type { UNKNOWN, TABLE, DATA };      /* BURT <source_type> */
static int source;                                   /* one of source_type's */
static int L1,L2;                                   /* TABLE <table>,L1,L2,F */
static char cnames[LLENGTH], rnames[LLENGTH];    /* col & row names of TABLE */
static char cclass[LLENGTH], rclass[LLENGTH];        /* class names of TABLE */
static char csizes[LNAME], rsizes[LNAME];       /* classifier sizes of TABLE */
static int dim,cdim,rdim,cols,rows;           /* various dimensions of TABLE */
static int crsiz[LNAME], cscal[LNAME];        /* classifier sizes and scales */
static int suppl[LNAME];                        /* supplementary classifiers */
static int N,classes;        /* number of observations and number of classes */
static char *wrd[LLENGTH];                            /* ~1000 char pointers */
static char *B;                /* binary table representing the combinations */
static int *T;                               /* table elements (frequencies) */
static int L,L0;                   /* "pointers" for navigating in the table */
static double *ZM;                   /* matrix Z (binary form of data/table) */
static double *BURT;                                   /* Burt's table (Z'Z) */
static double *CLASS;             /* classifiers and their number of classes */
static char *lab;                                  /* labels of Burt's table */
static char *Nlab;                                 /* row labels of matrix Z */
static int lablen;              /* length of labels (determined dynamically) */
#define LABLEN 8                             /* default (minimum) for lablen */
static char longlabs[20*LLENGTH];    /* a little oversized for making sure.. */
static char nam[2*LLENGTH];                   /* variable names, labels etc. */
static void trim2(char *);      /* removes spaces from the end of the string */
static SURVO_DATA dat;                                           /* the data */
static double *values;                            /* upper limits of classes */
static int *nval;                    /* number of classes + 1 (per variable) */
static int vmax;                   /* maximum number of classes per variable */
#define CLASSMAX 30                    /* default for vmax (analogy to STAT) */
#define NA (-123456789.0)              /* represents empty value in values[] */
#define MV -NA                      /* placeholder for minimum/missing value */
static char b_name[LNAME];         /* name for Burt's matrix, default BURT.M */
static char z_name[LNAME];        /* name for Z matrix, not saved by default */
static char c_name[LNAME];    /* name for classifier matrix, default CLASS.M */

/**********************
char *spec_burt[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                    "BURT", "CLASS", "TABDIM", "CLASSMAX", "LABLEN", "!" };
char **specs=spec_burt;
**********************/

void muste_burt(char *argv)
{
    int i;
    s_init(argv);
    if (g<3) { muste_kv_usage_info(); return; }
    i=spec_init(r1+r-1); if (i<0) return;
    i=check_params(); if (i<0) return;
    switch (source) {
        case TABLE: i=proceed_with_table(); if (i<0) return; break;
         case DATA: i=proceed_with_data(); if (i<0) return; break;
           default: muste_kv_usage_info(); return;
    }
    i=make_labels(); if (i<0) return;
    i=save_matrices(); if (i<0) return;
    s_end(argv);
}

static int check_params(void)
{
    int i;

    if (!strcmp(word[1],"TABLE")) {
        source=TABLE;
    } else if (!strcmp(word[1],"DATA")) {
        source=DATA;
    } else {
        source=UNKNOWN;
    }

    i=spfind("BURT");
    if (i>=0) {
        strcpy(b_name, spb[i]);
    } else {
        strcpy(b_name, "BURT.M");
    }

    i=spfind("CLASS");
    if (i>=0) {
        strcpy(c_name, spb[i]);
    } else {
        strcpy(c_name, "CLASS.M");
    }
    return 1;
}

static int proceed_with_table(void)
{
    int i;

    i=seek_table(); if (i<0) return -1;
    i=read_table(); if (i<0) return -1;
    i=spfind("TABDIM");
    if (i>=0) {
        if (atoi(spb[i])) {
            show_info();
            return -1;
        }
    }
    if (N==0) {
        muste_kv_s_err("TABLE %s is empty!", word[2]);
        return -1;
    }
    muste_kv_s_disp("\nCreating Burt's table from TABLE %s...", word[2]);
    i=read_elements(); if (i<0) return -1;
    i=binary_table(); if (i<0) return -1;
    i=table_to_Z_matrix(); if (i<0) return -1;
    return 1;
}

static int proceed_with_data(void)
{
    int i;

    muste_kv_s_disp("\nCreating Burt's table from DATA %s...", word[2]);
    vmax=CLASSMAX;
    i=spfind("CLASSMAX");
    if (i>=0) {
        vmax=max(vmax,atoi(spb[i]));
    }
    i=read_data(); if (i<0) return -1;
    i=data_to_Z_matrix(); if (i<0) return -1;
    return 1;
}

static void no_memory(void)
{
    muste_kv_s_err("Not enough memory!");
}

static int seek_table(void)
{
    int i;

    i=wfind("TABLE",word[2],1);
    if (i<0) {
        muste_kv_s_err("TABLE %s not found!", word[2]);
        return -1;
    }
    edread(sbuf,i);
    i=split(sbuf+1,wrd,5);
    if (i<5) {
        muste_kv_s_err("Invalid definition of TABLE %s!", word[2]);
        return -1;
    }
    if (*wrd[4]!='F') {
        muste_kv_s_err("TABLE %s is not a frequency table!", word[2]);
        return -1;
    }
    L1=edline2(wrd[2],1,1); if (!L1) return -1;
    L2=edline2(wrd[3],1,1); if (!L2) return -1;
    return 1;
}

static int read_table(void)
{
    int i,k,k0,j,h,n,elem;
    char x[LNAME];

    *cnames='\0'; *cclass='\0'; *csizes='\0';
    *rnames='\0'; *rclass='\0'; *rsizes='\0';
    L=L1; cdim=0; rdim=0; k0=1; cols=1; rows=0; N=0; classes=0;
    /* Columns: */
    while (L<=L2) {
        edread(sbuf,L); L++;
        if (strchr(sbuf+1,'*')!=NULL) break; /* line with '****' */
        k=muste_kv_space_split(sbuf+1,wrd,LLENGTH);
        strcat(cnames, wrd[0]); strcat(cnames, " ");
        k--;
        k/=k0; classes+=k;
        sprintf(x,"%d ",k);
        strcat(csizes, x);
        for (i=1; i<=k; i++) {
            strcat(cclass, wrd[i]);
            strcat(cclass, " ");
        }
        cdim++;
        cols*=k;
        k0*=k;
    }
    dim=cdim;

    /* Rows: */
    k=muste_kv_space_split(sbuf+1,wrd,LLENGTH);
    dim+=k-1;
    for (i=0; i<k-1; i++) {
        strcat(rnames, wrd[i]);
        strcat(rnames, " ");
    }
    rdim=dim-cdim; k0=1; L0=L;
    for (i=0; i<rdim; i++) {
        k=0; L=L0; h=0;
        while (L<=L2) {
            edread(sbuf,L); L++;
            j=muste_kv_space_split(sbuf+1,wrd,LLENGTH);
            if (j<rdim) continue; /* skip empty lines */
            if (i==0) {
                rows++;
                for (n=j; n>j-cols; n--) {
                    elem=atoi(wrd[n-1]);
                    N+=elem;
                }
            }
            if (h==rows/k0) break;
            h++;
            if (j>=(rdim+cols-i)) {
                strcat(rclass, wrd[abs(rdim+cols-j-i)]);
                strcat(rclass, " ");
                k++;
            }
        }
        classes+=k;
        sprintf(x,"%d ",k);
        strcat(rsizes, x);
        k0*=k;
    }
    return 1;
}

static int show_info(void)
{
    CLS; headline(""); PR_EBLD;
    muste_kv_s_disp("\n ");
    muste_kv_s_disp("\n TABLE %s", word[2]);
    muste_kv_s_disp("\n dim=%d cdim=%d rdim=%d cols=%d rows=%d N=%d classes=%d",
              dim, cdim, rdim, cols, rows, N, classes);
    muste_kv_s_disp("\n ");
    muste_kv_s_disp("\n col names :%s", cnames);
    muste_kv_s_disp("\n col sizes :%s", csizes);
    muste_kv_s_disp("\n class nam :%s", cclass);
    muste_kv_s_disp("\n ");
    muste_kv_s_disp("\n row names :%s", rnames);
    muste_kv_s_disp("\n row sizes :%s", rsizes);
    muste_kv_s_disp("\n class nam :%s", rclass);
    muste_kv_s_disp("\n ");
    WAIT;
    return 1;
}

static int read_elements(void)
{
    int j,h,n,elem;

    T=(int *)muste_malloc(cols*rows*sizeof(int));
    if (T==NULL) { no_memory(); return -1; }

    L=L0; h=0;
    while (L<=L2) {
        edread(sbuf,L); L++;
        j=muste_kv_space_split(sbuf+1,wrd,LLENGTH);
        if (j<rdim) continue; /* skip empty lines */
        for (n=j; n>j-cols; n--) {
            elem=atoi(wrd[n-1]);
            T[h+rows*(n-1-(j-cols))]=elem;
        }
        h++;
    }
    return 1;
}

static int binary_table(void)
{
    int i,j,k,cl,s0,s1,h,h0,row,bit,m;

    B=(char *)muste_malloc(cols*rows*classes*sizeof(char));
    if (B==NULL) { no_memory(); return -1; }

    muste_kv_space_split(csizes,wrd,LLENGTH); j=0;
    for (i=0; i<cdim; i++,j++) crsiz[j]=atoi(wrd[i]);
    muste_kv_space_split(rsizes,wrd,LLENGTH);
    for (i=0; i<rdim; i++,j++) crsiz[j]=atoi(wrd[i]);

    k=1; s0=0; s1=0;
    for (cl=0; cl<dim; cl++) { /* classifier at a time */
        m=crsiz[cl];
        s0=s1; s1+=m; k*=m;
        h=cols*rows/k; h0=0; bit=0;
        for (row=0; row<cols*rows; row++) { /* all rows */
            for (i=0,j=s0; j<s1; i++,j++) { /* classes of classifier */
                B[row+cols*rows*j] = (i==bit) ? 1 : 0;
            }
            if (++h0==h) { h0=0;
               if (++bit==m) bit=0;
            }
        }
    }
    return 1;
}

static int table_to_Z_matrix(void)
{
    int i,j,k,h;

    muste_kv_s_disp("\nCreating binary matrix Z (%d x %d)...", N, classes);

    ZM=(double *)muste_malloc(N*classes*sizeof(double));
    if (ZM==NULL) { no_memory(); return -1; }

    for (i=0,h=0; i<cols*rows; i++) {
        for (j=0; j<T[i]; j++,h++) {
            for (k=0; k<classes; k++) {
                ZM[h+N*k]=(double)B[i+cols*rows*k];
            }
        }
    }
    muste_free(B); muste_free(T);
    return 1;
}

static int read_data(void)
{
    int i,j,k,m,miss;
    double n1,n2,n3;
    double x;
    char *p;

    i=data_read_open(word[2],&dat); if (i<0) return -1;
    i=mask(&dat); if (i<0) return -1;
    scales(&dat); // in place but not in effective use until 30.7.2005!
    i=conditions(&dat); if (i<0) return -1;

    if (dat.m_act==0) {
        muste_kv_s_err("No active variables!");
        return -1;
    }
    values=(double *)muste_malloc(dat.m_act*vmax*sizeof(double));
    if (values==NULL) { no_memory(); return -1; }
    nval=(int *)muste_malloc(dat.m_act*sizeof(int));
    if (nval==NULL) { no_memory(); return -1; }

    classes=0;
    for (i=0; i<dat.m_act; i++) {
        for (k=0; k<vmax; k++) values[i*vmax+k]=NA;
        nval[i]=0; miss=0;
        strcpy(nam,dat.varname[dat.v[i]]); trim2(nam);
        j=spfind(nam); if (j<0) continue; /* classification not given */
        strcpy(sbuf, spb[j]);
        if (strchr(sbuf,',')!=NULL) {           /* <nam>=1,1,3,5 */
            m=split(sbuf,wrd,vmax);
            if (m>vmax) {
                muste_kv_s_err("Too many classes for variable %s!", nam);
                muste_kv_s_err("Increase CLASSMAX (default %d) if needed.",CLASSMAX);
                return -1;
            }
            if (!strncmp(wrd[m-1],"MISSING",7)) { /* <nam>=1,3,MISSING */
                miss=1; m--; /* forget missing class for a moment */
            }
            for (k=0; k<m; k++) values[i*vmax+k]=atof(wrd[k]);
            if (miss) {
                values[i*vmax+k]=MV;
                m++; /* restore missing class */
            }
            nval[i]=m;
        } else {                                /* <nam>=10(5)35 */
            p=sbuf; n1=n2=-1.0;
            while (*p++) {
                if (*p=='(') { n1++; *p=' '; }
                if (*p==')') { n2++; *p=' '; }
            }
            n3=split(sbuf,wrd,3);
            if (n1||n2||n3!=3) {
                muste_kv_s_err("Invalid classification of %s!", nam);
                return -1;
            }
            n1=atof(wrd[0]); n2=atof(wrd[1]); n3=atof(wrd[2]);
            m=(int)n3/n2-n1+2;    /* n1(n2)n3 */     /* 6.12.2000 (!) */
            if (n1==1) --m;
            if (n1==n2) m++;
            n1-=n2;
            for (k=0; k<m; k++,n1+=n2) values[i*vmax+k]=n1;
            nval[i]=m;
        }
        classes+=nval[i]-1;
    }

    muste_kv_s_disp("\nReading observations...");
    N=0;
    for (j=dat.l1; j<=dat.l2; j++) {
        if (unsuitable(&dat,j)) continue;
        N++;
        for (i=0; i<dat.m_act; i++) {
            if (values[i*vmax]!=NA && values[i*vmax]!=MV) {
                continue; /* already classified above */
            }
            if (values[i*vmax]!=MV) { /* (only once per var) */
                values[i*vmax]=MV; /* placeholder for minimum */
                nval[i]++; /* (was initialized above) */
            }
            data_load(&dat,j,dat.v[i],&x);
            if (x==MISSING8) continue; /* no missings by default */
            for (k=1; k<vmax; k++) { /* always skip MV */
                if (values[i*vmax+k]==x) break; /* is already */
                if (values[i*vmax+k]==NA) {
                    values[i*vmax+k]=x;
                    nval[i]++; classes++;
                    break;
                }
                if (k==vmax-1) {
                    strcpy(nam,dat.varname[dat.v[i]]); trim2(nam);
                    muste_kv_s_err("Variable %s has too many distinct values!", nam);
                    muste_kv_s_err("Increase CLASSMAX (default %d) or give a classification!", CLASSMAX);
                    return -1;
                }
            }
        }
    }
    if (N==0) {
        muste_kv_s_err("No acceptable observations!");
        return -1;
    }
    return 1;
}

static int data_to_Z_matrix()
{
    int i,j,k,h,bit,m,n,p;
    double x;
    int compare(const void *, const void *); /* for quicksort */

    muste_kv_s_disp("\nCreating binary matrix Z (%d x %d)...", N, classes);

    ZM=(double *)muste_malloc(N*classes*sizeof(double));
    if (ZM==NULL) { no_memory(); return -1; }

    for (i=0; i<dat.m_act; i++) {
        n=nval[i];
        if (values[i*vmax]==MV) { /* seek minimum for non-classified */
            for (k=1,x=MV; k<n; k++) {
                if (values[i*vmax+k]<x) x=values[i*vmax+k];
            }
            values[i*vmax]=x; /* minimum value doubled */
        }
        qsort((void *)&values[i*vmax],
            (size_t)n, sizeof(double), compare);
    }

    for (j=dat.l1,h=0; j<=dat.l2; j++) {
        if (unsuitable(&dat,j)) continue;
        for (i=0,p=0; i<dat.m_act; i++) {
            data_load(&dat,j,dat.v[i],&x);
            n=nval[i]-1; bit=-1; m=n;
            if (values[i*vmax+n]==MV) m--;
            if (x==MISSING8) {
                if (m<n) bit=n-1; /* missing class */
            } else if (x>=values[i*vmax] && x<=values[i*vmax+m]) {
                for (k=1; k<=m; k++) {
                    if (x<=values[i*vmax+k]) { bit=k-1; break; }
                }
            }
            for (m=0; m<n; m++) ZM[h+N*(p+m)] = (m==bit) ? 1.0 : 0.0;
            p+=n;
        }
        h++;
    }
    return 1;
}

static int compare(const void *val1, const void *val2)
{
    const double *v1=(const double *)val1;
    const double *v2=(const double *)val2;
    if (*v1<*v2) return -1;
    if (*v1>*v2) return  1;
    return 0;
}

static int make_labels(void)
{
    int i,j,k,m,n,labmax;
    char *p;

    labmax=LABLEN; *longlabs='\0';

    if (source==TABLE) {
        strcpy(nam, cclass); strcat(nam, rclass);
        muste_kv_space_split(nam,wrd,classes);
        for (i=0; i<classes; i++) {
            strcat(longlabs, wrd[i]); strcat(longlabs, " ");
            j=strlen(wrd[i]);
            if (j>labmax) labmax=j;
        }
    }

    if (source==DATA) {
        for (i=0; i<dat.m_act; i++) {
            n=nval[i];
            strcpy(nam,dat.varname[dat.v[i]]); trim2(nam);
            j=spfind(nam);
            if (j>=0) strcpy(sbuf, spb[j]);
            else *sbuf='\0';
            if (strchr(sbuf,',')!=NULL) { /* <nam>=1,2(foo),3(bar) */
                m=split(sbuf,wrd,vmax);
                for (k=1; k<m; k++) {
                    p=wrd[k];
                    j=strlen(p)-1;
                    if (p[j]==')') p[j]='\0';
                    while (*p++) { if (*p=='(') break; }
                    if (*p=='(') ++p; else p=wrd[k];
                    strcat(longlabs, p); strcat(longlabs, " ");
                    j=strlen(p);
                    if (j>labmax) labmax=j;
                }
            } else { /* <nam>=1(1)5 (j>=0) or <nam> not given (j<0) */
                if (j>=0) *nam='\0';
                for (k=1; k<n; k++) {
                    sprintf(sbuf, "%s%.f", nam, values[i*vmax+k]);
                    strcat(longlabs, sbuf); strcat(longlabs, " ");
                    j=strlen(sbuf);
                    if (j>labmax) labmax=j;
                }
            }
        }
        muste_free(values);
    }

    lablen=LABLEN;
    i=spfind("LABLEN"); if (i>=0) lablen=atoi(spb[i]);
    lablen=max(lablen,labmax);
    lab=(char *)muste_malloc(classes*lablen*sizeof(char)+1);
    if (lab==NULL) { no_memory(); return -1; }
    *lab='\0';
    muste_kv_space_split(longlabs,wrd,classes);
    for (i=0; i<classes; i++) {
        strcpy(nam, wrd[i]); j=strlen(nam);
        while (j<lablen) nam[j++]=' '; nam[lablen]='\0';
        strcat(lab,nam);
    }
    return 1;
}

static void trim2(char *s) /* remove spaces from the end */
{
    while(*s) s++; s--;
    while(*s==' ') s--; s++;
    *s='\0';
}

static int save_matrices(void)
{
    int i;

    i=burt_table(); if (i<0) return -1;
    i=spfind("Z");
    if (i>=0) {
        strcpy(z_name, spb[i]);
        i=Z_matrix(); if (i<0) return -1;
    }
    i=classifiers(); if (i<0) return -1;
    muste_free(lab); muste_free(BURT); muste_free(ZM);
    return 1;
}

static int burt_table(void)
{
    int i;

    muste_kv_s_disp("\nSaving Burt's table in %s (%d x %d)...",
        b_name, classes, classes);

    BURT=(double *)muste_malloc(classes*classes*sizeof(double));
    if (BURT==NULL) { no_memory(); return -1; }
    mat_mtm(BURT,ZM,N,classes); /* so simple & easy.. */

    sprintf(nam, "Burt's_table_of_%s_%s", word[1], word[2]);
    i=matrix_save(b_name,BURT,classes,classes,
        lab,lab,lablen,lablen,10,nam,0,0);
    if (i<0) return -1;
    return 1;
}

static int classifiers(void)
{
    int i,j,ccol; // 30.7.2005
    char act_char;

    muste_kv_s_disp("\nSaving classifier information in %s...", c_name);

    *lab='\0';
    ccol=1; // default: only 1 column in CLASS.M (30.7.2005)
    if (source==TABLE) {
        strcpy(longlabs, cnames); strcat(longlabs, rnames);
        muste_kv_space_split(longlabs,wrd,dim);
        for (i=0; i<dim; i++) {
            strcpy(nam, wrd[i]); j=strlen(nam);
            while (j<lablen) nam[j++]=' '; nam[lablen]='\0';
            strcat(lab,nam);
        }
    }
    if (source==DATA) {
        lablen=8; // 2.8.2005 (names correspond to varnames in data!)
        for (i=0; i<dat.m_act; i++) {
            crsiz[i]=nval[i]-1;
            strcpy(nam,dat.varname[dat.v[i]]); trim2(nam);
            j=strlen(nam);
            while (j<lablen) nam[j++]=' '; nam[lablen]='\0';
            strcat(lab,nam);
            // scales of classifiers (30.7.2005):
            cscal[i] = (scale_ok(&dat,dat.v[i],ORDINAL_SCALE)) ? 1 : 0;

            // find activating character (S for supplementary) (30.7.2005):
            act_char=dat.vartype[dat.v[i]][1];
            suppl[i] = (act_char=='S') ? 1 : 0;
        }
        dim=dat.m_act;
        ccol=3;
    }
    CLASS=(double *)muste_malloc(dim*ccol*sizeof(double));
    if (CLASS==NULL) { no_memory(); return -1; }
    for (i=0; i<dim; i++) CLASS[i]=(double)crsiz[i];
    strcpy(cnames, "Classes ");
    if (ccol>1) {
        for (i=1*dim,j=0; i<2*dim; i++,j++) CLASS[i]=(double)cscal[j];
        strcat(cnames, "Scale   ");
        for (i=2*dim,j=0; i<3*dim; i++,j++) CLASS[i]=(double)suppl[j];
        strcat(cnames, "Suppl   ");
    }
    sprintf(nam, "Classifiers_of_%s_%s", word[1], word[2]);
    i=matrix_save(c_name,CLASS,dim,ccol,lab,cnames,lablen,8,0,nam,0,0);
    if (i<0) return -1;
    muste_free(CLASS);
    muste_free(nval);
    data_close(&dat);
    return 1;
}

static int Z_matrix(void)
{
    int i;

    muste_kv_s_disp("\nSaving Z matrix to %s (%d x %d)...",
        z_name, N, classes);

    Nlab=(char *)muste_malloc(N*8*sizeof(char)+1);
    if (Nlab==NULL) { no_memory(); return -1; }
    *Nlab='\0';
    for (i=0; i<N; i++) {
        sprintf(sbuf,"%-8d",i+1);
        strcat(Nlab,sbuf);
    }
    sprintf(nam, "Binary_form_of_%s_%s", word[1], word[2]);
    i=matrix_save(z_name,ZM,N,classes,Nlab,lab,8,lablen,0,nam,0,0);
    if (i<0) return -1;
    muste_free(Nlab);
    return 1;
}
