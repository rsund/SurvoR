/* tabs.c 12.12.1987/SM (27.4.1992)
   TABS <table>,<# of row classifiers>,L
*/

#include <stdio.h>
#include <stdlib.h>
// #include <malloc.h>
// #include <conio.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXDIM 200
#define MAXT 10000
#define TSPACE 80000

typedef double FREQ;
#define ZERO 0.0
#define STRUCTURAL_ZERO 1e307
#define MISSING_VALUE MISSING8

/* double l_luku=0.0;  SURVOMAT */
static int results_line;
static int missing_values=0;

static int tabs();
static int read_ftable(char *name,FREQ **f,int *pdim,int *pncvar,
       int *nc,char **varname,char **cname,char *type,int *pndec);
static int store_label(char *s);
static int ctypes(int *ctype,int dim,int *nc,char **cname);
static int digts(FREQ *f,int dim,int *nc);
static int check_varname_initials(int dim,char **varname);
static int tabdim();
static int tabm();
static int jindex(int dim,int *nc,int *j);
static int tabi();
static int tabd();
static int tabdx(int ncvar);
static int tabj();
static int tabplus();
static int tabpluserror();
static int tabfit();
static int fit(int ncvar);
static int print_line(char *x);
static int glm_fit(double *X,int nx,int mx,double *Y,double *W,double *V,
           char *lab,double *b,double *sb,double *my,double *pdev,int *pdf);
static int lin_fit(double *X,int nx,int mx,double *Y,double *W,double *V,
           char *lab,double *b,double *sb,double *my,double *pdev,int *pdf);
static int glm_fit_space(int mx,int nx);
static int mspace(double **A,int m,int n);
static int std_errors(double *R,int mx,double *sb,char *lab,double *RRT,double ss);
static int reduce(double *X,char *lab,int nx,int mx,int i1);
static int restore(double *b,double *sb,int ndel,int mx);
static int read_model();
static int var(char *x);
static int update_model(int *iterm,char sign);
static int generate_x();
static int in_model(int *iterm);
static int col_labels();
static int row_labels();
static int print_ftable(char *name,int line,char *eout,int dim,int *nc,
       int *cumnc,int *ctype,FREQ *f,FREQ total,FREQ missing,
       char **varname,char **cname,int colvar,int minwidth,
       int isum,int ipros,char *form,char *type);
static char *spois(char *s);
static int tline_init(char *x);
static int tline_write(char *s,char *x,int col);
static int not_enough_memory();

/*********************
main(argc,argv)
int argc; char *argv[];
************************/

void muste_tabs(char *argv)
        {
//      int i;
        char ch;
        char *p;

//      if (argc==1) return;
        s_init(argv);
        missing_values=0;

        p=word[0];
        if (muste_strcmpi(p,"TABS")==0) { tabs(); s_end(argv); return; }
        if (muste_strcmpi(p,"TABM")==0) { tabm(); s_end(argv); return; }
        if (muste_strcmpi(p,"TABI")==0) { tabi(); s_end(argv); return; }
        if (muste_strcmpi(p,"TABD")==0) { tabd(); s_end(argv); return; }
        if (muste_strcmpi(p,"TABJ")==0) { tabj(); s_end(argv); return; }
        if (muste_strcmpi(p,"TABFIT")==0) { tabfit(); s_end(argv); return; }
        ch=p[3];
        if (ch=='+' || ch=='-' || ch=='*' || ch=='/') { tabplus(); s_end(argv); return; }
        if (muste_strcmpi(p,"TABDIM")==0) { tabdim(); s_end(argv); return; }

        sprintf(sbuf,"\nUnknown operation %s!",word[0]); sur_print(sbuf); WAIT;
        }

static char text[TSPACE];
static char *ptext;
static FREQ *f;
static int dim;
static int nc[MAXDIM];
static char *varname[MAXDIM];
static char *cname[MAXT];
static char type[16];
static int ndec;
static int cumnc[MAXDIM];
static int ctype[MAXDIM];

static int mx,nx;
static double *x;
static char *rlab,*clab;
static char expr[129];

static int tabs()
        {
        int i;
        int ncvar,nrvar2;
        char nimi[LLENGTH];
        int digits;
        char s[LLENGTH];

   f=NULL;
   x=NULL;
   rlab=NULL;
   clab=NULL;

        results_line=0;
        if (g<3)
            {
            sur_print("\nUsage: TABS <table>,<# of row classifiers>,L");
            WAIT; return(-1);
            }
        nrvar2=atoi(word[2]);
        if (g>3)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return(-1);
            }
        ptext=text;
        i=read_ftable(word[1],&f,&dim,&ncvar,nc,varname,cname,type,&ndec);
        if (i<0) return(-1);

        if (nrvar2<0) nrvar2=0;
        if (nrvar2>dim) nrvar2=dim;
        strcpy(nimi,word[1]); strcat(nimi,"S");
        cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc[i-1];
        ctypes(ctype,dim,nc,cname);
        digits=digts(f,dim,nc);
        *s=EOS;
        if (ndec)
            {
            for (i=0; i<digits; ++i) strcat(s,"1"); strcat(s,".");
            for (i=0; i<ndec; ++i) strcat(s,"1");
            }
print_ftable(nimi,results_line,eout,dim,nc,cumnc,ctype,
             f,ZERO,ZERO,varname,cname,dim-nrvar2,digits,0,0,s,type);
        return(1);
        }

static int read_ftable(char *name,FREQ **f,int *pdim,int *pncvar,
       int *nc,char **varname,char **cname,char *type,int *pndec)
        {
        int i,j,k,j1,j2,apos,j0,ivar,rep,h,h2,n,len;
        int ncell,cell,ncell2,step;
        char x[LLENGTH], *px[EP4];
        char *p;
        int ncol,nrow;
        int nlabel,nlab;
        int posr[MAXDIM];
        FREQ cvalue;

        j=wfind("TABLE",name,1);
        if (j<0)
            {
            sprintf(sbuf,"\nTABLE %s not found in the edit field!",name);
            sur_print(sbuf); WAIT; return(-1);
            }
        edread(x,j);
        i=split(x+1,px,5);
        if (i<5)
            {
            edread(x,j); i=strlen(x); while (x[i-1]==' ') x[--i]=EOS;
            sprintf(sbuf,"\nInvalid definition: %s",x+1); sur_print(sbuf);
            sprintf(sbuf,"\non line %d",j); sur_print(sbuf);
            sur_print("\nCorrect form: TABLE <name>,L1,L2,<type_of_table>");
            WAIT; return(-1);
            }
        j1=edline2(px[2],1,1); if (j1==0) return(-1);
        j2=edline2(px[3],j1,1); if (j2==0) return(-1);
        strncpy(type,px[4],15); type[15]=EOS;
        ncell=0;
        for (j=j1; j<=j2; ++j)
            {
            edread(x,j);
            p=strchr(x+1,'*'); if (p!=NULL) break;
            }
        if (j>j2)
            {
sprintf(sbuf,"\nLine of row classifiers ending with *'s missing in table %s!",
                           name); sur_print(sbuf); WAIT; return(-1);
            }
        apos=p-x;
        j0=j; ncol=j0-j1; k=0;
        for (j=j0+1; j<=j2; ++j)
            {
            edread(x,j);
            i=split(x+apos,px,EP4);
            if (i==0) continue;
            if (k==0) k=i;
            if (i!=k)
                {
sprintf(sbuf,"\nNumber of elements on line %d conflicts previous lines!",
                                j); sur_print(sbuf);
                WAIT; return(-1);
                }
            ncell+=k;
            }
        if (*f==NULL)
            *f=(FREQ *)muste_malloc(ncell*sizeof(FREQ));
        else
            *f=(FREQ *)muste_realloc(*f,ncell*sizeof(FREQ));
        if (*f==NULL)
            { not_enough_memory(); return(-1); }

        nlabel=0;
        ivar=0; rep=1;
        for (j=j1; j<j0; ++j)
            {
            edread(x,j);
            n=split(x+1,px,EP4);
            if (n<3)
                {
                sprintf(sbuf,"\nError in column classifier on line %d",j);
                sur_print(sbuf); WAIT; return(-1);
                }
            h=(n-1)/rep;
            if (h*rep!=n-1)
                {
                sprintf(sbuf,"\nError in labels on line %d",j);
                sur_print(sbuf); WAIT; return(-1);
                }
            nc[ivar]=h;
            varname[ivar]=ptext;
            i=store_label(px[0]); if (i<0) return(-1);
            for (h=0; h<nc[ivar]; ++h)
                {
                if (nlabel>=MAXT)
                    {
                    sur_print("\nToo many labels!"); WAIT; return(-1);
                    }
                cname[nlabel++]=ptext;
                i=store_label(px[h+1]);
                }
            rep*=nc[ivar]; ++ivar;
            }

        edread(x,j0);
        i=split(x+1,px,EP4);
        nrow=i-1; ivar=ncol;
        for (i=0; i<nrow; ++i)
            {
            posr[i]=px[i]-x;
            varname[ivar++]=ptext;
            h=store_label(px[i]);
            }
        posr[nrow]=apos;

        ivar=ncol;
        ncell2=k;
        for (i=0; i<nrow; ++i)
            {
            nc[ivar]=0; nlab=nlabel;
            for (j=j0+1; j<=j2; ++j)
                {
                edread(x,j);
                h=posr[i];
                while (x[h]==' ' && h<posr[i+1]) ++h;
                if (h==posr[i+1]) continue;
                h2=h; while (x[h2]!=' ' && h2<posr[i+1]) ++h2;
                x[h2]=EOS;
                for (h2=0; h2<nc[ivar]; ++h2)
                    {
                    if (strcmp(x+h,cname[nlab+h2])==0) break;
                    }
                if (h2<nc[ivar]) continue;
                ++nc[ivar];
                if (nlabel>=MAXT)
                    {
                    sur_print("\nToo many labels!"); WAIT; return(-1);
                    }
                cname[nlabel++]=ptext;
                h2=store_label(x+h); if (h2<0) return(-1);
                }
            ncell2*=nc[ivar];
            ++ivar;
            }
/*
    printf("\nncol=%d nrow=%d",ncol,nrow);
    i=0;
    for (ivar=0; ivar<ncol+nrow; ++ivar)
             {
             printf("\n%s:",varname[ivar]);
             for (h=0; h<nc[ivar]; ++h) printf(" %s",cname[i+h]);
             i+=nc[ivar];
             }
    getch();
*/
        if (ncell2!=ncell)
            {
            sur_print("\nError in (row) labels!");
            i=0;
            for (ivar=0; ivar<ncol+nrow; ++ivar)
                {
                sprintf(sbuf,"\n%s:",varname[ivar]); sur_print(sbuf);
                for (h=0; h<nc[ivar]; ++h)
                    { sprintf(sbuf," %s",cname[i+h]); sur_print(sbuf); }
                i+=nc[ivar];
                }
            WAIT; return(-1);
            }
        *pdim=ncol+nrow;
        h=0;
        step=ncell/k;
        *pndec=0;
        for (j=j0+1; j<=j2; ++j)
            {
            edread(x,j);
            i=split(x+apos,px,k);
            if (i==0) continue;
            cell=h;
            for (i=0; i<k; ++i)
                {
                len=strlen(px[i])-1;
                if (px[i][len]=='-')
                    { cvalue=MISSING_VALUE; ++missing_values; }
                else if (strcmp(px[i],"*0")==0) cvalue=STRUCTURAL_ZERO;
                else cvalue=atof(px[i]);    /* depends on FREQ */
                (*f)[cell]=cvalue;
                cell+=step;
                p=strchr(px[i],'.');
                if (p!=NULL)
                    {
                    h2=len-(p-px[i]);
                    if (h2>*pndec) *pndec=h2;
                    }
                }
            ++h;
            }
/*
   printf("\nf:");
   for (cell=0; cell<ncell; ++cell) printf("%u ",(*f)[cell]);
   getch();
*/
        *pncvar=ncol;
        return(1);
        }

static int store_label(char *s)
        {
        char *p;
        if (ptext-text>TSPACE-strlen(s)-2)
            {
            sur_print("\nNot enough space for labels!"); WAIT; return(-1);
            }
        p=s; while (*p) *ptext++=*p++; *ptext++=EOS;
        return(1);
        }


static int ctypes(int *ctype,int dim,int *nc,char **cname)
        {
        int i,j,k,type;

        k=0;
        for (i=0; i<dim; ++i)
            {
            type=1;
            for (j=0; j<nc[i]; ++j)
                if (!muste_isnumber(cname[k++])) type=0;
            ctype[i]=type;
            }
        return(1);
        }

static int digts(FREQ *f,int dim,int *nc)
        {
        FREQ max;
        int l,k;
        int i,neg;
        double da;

        max=0.0; neg=0;
        l=1L; for (i=0; i<dim; ++i) l*=nc[i];
        for (k=0L; k<l; ++k)
            {
            if (f[k]>MISSING_VALUE/100.0)
                { if (max<10.0) max=10; }  /* aaltosulut pakolliset! */
            else if (fabs((double)f[k])>max) max=fabs((double)f[k]);
            if (f[k]<ZERO) neg=1;
            }
        if (max==(FREQ)0) max=1;
        da=log((double)max)/log(10.0); if (da<0.0) da=0.0;
        return ((int)(neg+1.0001+da));
        }

static int check_varname_initials(int dim,char **varname)
        {
        int i,k;

        for (i=1; i<dim; ++i)
            {
            for (k=0; k<i; ++k)
                {
                if (*varname[k]!=*varname[i]) continue;
sprintf(sbuf,"\nSame initials in classifier names %s (#%d) and  %s (#%d)",
                                        varname[k],k+1,varname[i],i+1);
                sur_print(sbuf); WAIT; return(-1);
                }
            }
        return(1);
        }

static int tabdim()
        {
        int i,j,k;
        int ncvar;
  //    char nimi[LLENGTH];

        results_line=0;
        if (g<2)
            {
sur_print("\nUsage: TABDIM <table> / results: dimension classifiers");
            WAIT; return(-1);
            }
        ptext=text;
        i=read_ftable(word[1],&f,&dim,&ncvar,nc,varname,cname,type,&ndec);
        if (i<0) { dim=0; ncvar=0; strcpy(type,"?"); }
        else
            {
            i=check_varname_initials(dim,varname);
            if (i<0) strcpy(type,"?");
            }
        if (*type!='?' && missing_values) strcpy(type,"-");
        j=r1+r-1;
        edwrite(space,j,1);
        k=sprintf(sbuf,"TABDIM %s / %d %d %s",word[1],dim,ncvar,type);
        for (i=0; i<dim; ++i)
            k+=sprintf(sbuf+k," %s",varname[i]);
        edwrite(sbuf,j,1);
        return(1);
        }

/* tabm.c 13.12.1987/SM (25.12.1987)
   TABM <table>,A,B,L
*/

static FREQ *f2;
static int *nc2;
static int *pj;
static char **cname2;

static int tabm()
        {
        int i,j,k;
        int ncvar;
        char nimi[LLENGTH];
        int digits;
        int i1,i2;
        int ncell,nclass;
        int ind1,ind2;
        char *p;
        char s[LLENGTH];

    f2=NULL;
    nc2=NULL;
    pj=NULL;
    cname2=NULL;



        results_line=0;
        if (g<4)
            {
            sur_print("\nUsage: TABM <table>,C1,C2,L");
            sur_print("\n       interchanges classifiers C1 and C2.");
            WAIT; return(1);
            }
        if (g>4)
            {
            results_line=edline2(word[4],1,1);
            if (results_line==0) return(1);
            }
        ptext=text;
        i=read_ftable(word[1],&f,&dim,&ncvar,nc,varname,cname,type,&ndec);
        if (i<0) return(1);

        k=strlen(word[2]);
        for (i=0; i<dim; ++i)
            if (strncmp(word[2],varname[i],k)==0) break;
        if (i==dim)
            {
            sprintf(sbuf,"\nClassifier %s not found!",word[2]);
            sur_print(sbuf); WAIT; return(1);
            }
        i1=i;
        k=strlen(word[3]);
        for (i=0; i<dim; ++i)
            if (strncmp(word[3],varname[i],k)==0) break;
        if (i==dim)
            {
            sprintf(sbuf,"\nClassifier %s not found!",word[3]);
            sur_print(sbuf); WAIT; return(1);
            }
        i2=i;

        ncell=1; nclass=0; for (i=0; i<dim; ++i)
            { ncell*=nc[i]; nclass+=nc[i]; }
        f2=(FREQ *)muste_malloc(ncell*sizeof(FREQ));
        if (f2==NULL)
            { not_enough_memory(); return(1); }
        nc2=(int *)muste_malloc(dim*sizeof(int));
        if (nc2==NULL)
            { not_enough_memory(); return(1); }
        pj=(int *)muste_malloc(dim*sizeof(int));
        if (pj==NULL)
            { not_enough_memory(); return(1); }
        cname2=(char **)muste_malloc(nclass*sizeof(char **));
        if (cname2==NULL)
            { not_enough_memory(); return(1); }
        for (i=0; i<dim; ++i) nc2[i]=nc[i];
        nc2[i1]=nc[i2]; nc2[i2]=nc[i1];

        for (i=0; i<dim; ++i) pj[i]=0;
        while (1)
            {
            ind1=jindex(dim,nc,pj);
            i=pj[i1]; pj[i1]=pj[i2]; pj[i2]=i;
            ind2=jindex(dim,nc2,pj);
            f2[ind2]=f[ind1];
            i=pj[i1]; pj[i1]=pj[i2]; pj[i2]=i;
            i=0;
            while (1)
                {
                if (pj[i]<nc[i]-1) { ++pj[i]; break; }
                pj[i]=0;
                ++i; if (i==dim) break;
                }
            if (i==dim) break;
            }
        for (i=0; i<dim; ++i) pj[i]=i;
        pj[i1]=i2; pj[i2]=i1;
        cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc2[i-1];
        k=0;
        for (i=0; i<dim; ++i)
            for (j=0; j<nc[i]; ++j)
                cname2[cumnc[pj[i]]+j]=cname[k++];
        p=varname[i1]; varname[i1]=varname[i2]; varname[i2]=p;
        strcpy(nimi,word[1]); strcat(nimi,"M");
        ctypes(ctype,dim,nc,cname);
        digits=digts(f,dim,nc);
        *s=EOS;
        if (ndec)
            {
            for (i=0; i<digits; ++i) strcat(s,"1"); strcat(s,".");
            for (i=0; i<ndec; ++i) strcat(s,"1");
            }
        print_ftable(nimi,results_line,eout,dim,nc2,cumnc,ctype,
             f2,ZERO,ZERO,varname,cname2,ncvar,digits,0,0,s,type);
        return(1);
        }

static int jindex(int dim,int *nc,int *j)
        {
        int i;
        int ind;

        ind=j[0];
        for (i=1; i<dim; ++i)
            ind=nc[i]*ind+j[i];
        return(ind);
        }

/* tabi.c 13.12.1987/SM (25.12.1987)
   TABI <table>,C,C1,C2,L
*/

static int tabi()
        {
        int i,k;
        int ncvar;
        char nimi[LLENGTH];
        int digits;
        int i1,ic1,ic2;
        int ncell,nclass;
        int ind1,ind2;
        char *p;
        char s[LLENGTH];

        results_line=0;
        if (g<5)
            {
         sur_print("\nUsage: TABI <table>,C,C1,C2,L");
         sur_print("\n       interchanges classes C1 and C2 of classifier C.");
         WAIT; return(1);
            }
        if (g>5)
            {
            results_line=edline2(word[5],1,1);
            if (results_line==0) return(1);
            }
        ptext=text;
        i=read_ftable(word[1],&f,&dim,&ncvar,nc,varname,cname,type,&ndec);
        if (i<0) return(1);

        k=strlen(word[2]);
        for (i=0; i<dim; ++i)
            if (strncmp(word[2],varname[i],k)==0) break;
        if (i==dim)
            {
            sprintf(sbuf,"\nClassifier %s not found!",word[2]);
            sur_print(sbuf); WAIT; return(1);
            }
        i1=i;
        cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc[i-1];
        k=strlen(word[3]);
        for (i=0; i<nc[i1]; ++i)
            if (strncmp(word[3],cname[cumnc[i1]+i],k)==0) break;
        if (i==nc[i1])
            {
            sprintf(sbuf,"\nClass %s not found!",word[3]);
            sur_print(sbuf); WAIT; return(1);
            }
        ic1=i;
        k=strlen(word[4]);
        for (i=0; i<nc[i1]; ++i)
            if (strncmp(word[4],cname[cumnc[i1]+i],k)==0) break;
        if (i==nc[i1])
            {
            sprintf(sbuf,"\nClass %s not found!",word[4]);
            sur_print(sbuf); WAIT; return(1);
            }
        ic2=i;

        ncell=1; nclass=0; for (i=0; i<dim; ++i)
            { ncell*=nc[i]; nclass+=nc[i]; }
        f2=(FREQ *)muste_malloc(ncell*sizeof(FREQ));
        if (f2==NULL)
            { not_enough_memory(); return(1); }
        pj=(int *)muste_malloc(dim*sizeof(int));
        if (pj==NULL)
            { not_enough_memory(); return(1); }

        for (i=0; i<dim; ++i) pj[i]=0;
        while (1)
            {
            ind1=jindex(dim,nc,pj);
            if (pj[i1]==ic1) pj[i1]=ic2; else if (pj[i1]==ic2) pj[i1]=ic1;
            ind2=jindex(dim,nc,pj);
            f2[ind2]=f[ind1];
            if (pj[i1]==ic1) pj[i1]=ic2; else if (pj[i1]==ic2) pj[i1]=ic1;
            i=0;
            while (1)
                {
                if (pj[i]<nc[i]-1) { ++pj[i]; break; }
                pj[i]=0;
                ++i; if (i==dim) break;
                }
            if (i==dim) break;
            }
        i=cumnc[i1];
        p=cname[i+ic1]; cname[i+ic1]=cname[i+ic2]; cname[i+ic2]=p;
        strcpy(nimi,word[1]); strcat(nimi,"I");
        ctypes(ctype,dim,nc,cname);
        digits=digts(f,dim,nc);
        *s=EOS;
        if (ndec)
            {
            for (i=0; i<digits; ++i) strcat(s,"1"); strcat(s,".");
            for (i=0; i<ndec; ++i) strcat(s,"1");
            }
        print_ftable(nimi,results_line,eout,dim,nc,cumnc,ctype,
             f2,ZERO,ZERO,varname,cname,ncvar,digits,0,0,s,type);
        return(1);
        }

/* tabd.c 14.12.1987/SM (25.12.1987)
   TABD <table>,C,L
*/

static int *qj;

static int tabd()
        {
        int i,k;
        int ncvar;
        char nimi[LLENGTH];
        int digits;
        int i1;
        int ncell,nclass;
        int ind1,ind2;
    //  char *p;
        char s[LLENGTH];

        results_line=0;
        if (g<3)
            {
            sur_print("\nUsage: TABD <table>,C,L");
            sur_print("\n       collapsing over classification C.");
            WAIT; return(1);
            }
        if (g>3)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return(1);
            }
        ptext=text;
        i=read_ftable(word[1],&f,&dim,&ncvar,nc,varname,cname,type,&ndec);
        if (i<0) return(1);

        if (strcmp(type,"X")==0)
            {
            tabdx(ncvar);
            return(1);
            }
        k=strlen(word[2]);
        for (i=0; i<dim; ++i)
            if (strncmp(word[2],varname[i],k)==0) break;
        if (i==dim)
            {
            sprintf(sbuf,"\nClassifier %s not found!",word[2]);
            sur_print(sbuf); WAIT; return(1);
            }
        i1=i;

        ncell=1; nclass=0; for (i=0; i<dim; ++i)
            { ncell*=nc[i]; nclass+=nc[i]; }
        ncell/=nc[i1]; nclass-=nc[i1];
        f2=(FREQ *)muste_malloc(ncell*sizeof(FREQ));
        if (f2==NULL)
            { not_enough_memory(); return(1); }
        pj=(int *)muste_malloc(dim*sizeof(int));
        if (pj==NULL)
            { not_enough_memory(); return(1); }
        qj=(int *)muste_malloc(dim*sizeof(int));
        if (qj==NULL)
            { not_enough_memory(); return(1); }
        nc2=(int *)muste_malloc(dim*sizeof(int));
        if (nc2==NULL)
            { not_enough_memory(); return(1); }

        for (i=0; i<i1; ++i) nc2[i]=nc[i];
        for (i=i1+1; i<dim; ++i) nc2[i-1]=nc[i];

        for (i=0; i<ncell; ++i) f2[i]=STRUCTURAL_ZERO;
        for (i=0; i<dim; ++i) pj[i]=0;
        while (1)
            {
            ind1=jindex(dim,nc,pj);
            for (i=0; i<i1; ++i) qj[i]=pj[i];
            for (i=i1+1; i<dim; ++i) qj[i-1]=pj[i];
            ind2=jindex(dim-1,nc2,qj);
            if (f2[ind2]==MISSING_VALUE || f[ind1]==MISSING_VALUE)
                f2[ind2]=MISSING_VALUE;
            else if (f2[ind2]==STRUCTURAL_ZERO)
                {
                if (f[ind1]!=STRUCTURAL_ZERO) f2[ind2]=f[ind1];
                }
            else if (f[ind1]!=STRUCTURAL_ZERO) f2[ind2]+=f[ind1];
            i=0;
            while (1)
                {
                if (pj[i]<nc[i]-1) { ++pj[i]; break; }
                pj[i]=0;
                ++i; if (i==dim) break;
                }
            if (i==dim) break;
            }
        for (i=i1+1; i<dim; ++i) varname[i-1]=varname[i];
        cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc[i-1];
        for (i=cumnc[i1]; i<nclass; ++i) cname[i]=cname[i+nc[i1]];
        cumnc[0]=0; for (i=1; i<dim-1; ++i) cumnc[i]=cumnc[i-1]+nc2[i-1];

        strcpy(nimi,word[1]); strcat(nimi,"D");
        ctypes(ctype,dim-1,nc2,cname);
        digits=digts(f2,dim-1,nc2);
        *s=EOS;
        if (ndec)
            {
            for (i=0; i<digits; ++i) strcat(s,"1"); strcat(s,".");
            for (i=0; i<ndec; ++i) strcat(s,"1");
            }
        if (i1<ncvar) --ncvar;
        print_ftable(nimi,results_line,eout,dim-1,nc2,cumnc,ctype,
             f2,ZERO,ZERO,varname,cname,ncvar,digits,0,0,s,type);
        return(1);
        }

static int tabdx(int ncvar)
        {
        int i,k;
        char nimi[LLENGTH];
        int digits;
        int i1,in;
        int ncell,nclass;
        int ind1,ind2;
    //  char *p;
        char s[LLENGTH];

        k=strlen(word[2]);
        for (i=0; i<dim; ++i)
            if (strncmp(word[2],varname[i],k)==0) break;
        if (i==dim)
            {
            sprintf(sbuf,"\nClassifier %s not found!",word[2]);
            sur_print(sbuf); WAIT; return(1);
            }
        i1=i;
        for (i=0; i<dim; ++i) if (strcmp(varname[i],"N")==0) break;
        if (i==dim)
            {
            cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc[i-1];
            varname[i1]="N";
            for (i=0; i<nc[i1]; ++i)
                {
                sprintf(s,"%d",i+1);
                cname[cumnc[i1]+i]=ptext;
                k=store_label(s); if (k<0) return(1);
                }
            strcpy(nimi,word[1]); strcat(nimi,"D");
            ctypes(ctype,dim,nc,cname);
            digits=digts(f,dim,nc);
            *s=EOS;
            if (ndec)
                {
                for (i=0; i<digits; ++i) strcat(s,"1"); strcat(s,".");
                for (i=0; i<ndec; ++i) strcat(s,"1");
                }
            print_ftable(nimi,results_line,eout,dim,nc,cumnc,ctype,
                 f,ZERO,ZERO,varname,cname,ncvar,digits,0,0,s,type);
            return(1);
            }

        in=i; if (in==i1) return(1);
        if (in>i1) { i=in; in=i1; i1=i; }

        ncell=1; nclass=0; for (i=0; i<dim; ++i)
            { ncell*=nc[i]; nclass+=nc[i]; }
        f2=(FREQ *)muste_malloc(ncell*sizeof(FREQ));
        if (f2==NULL)
            { not_enough_memory(); return(-1); }
        pj=(int *)muste_malloc(dim*sizeof(int));
        if (pj==NULL)
            { not_enough_memory(); return(-1); }
        qj=(int *)muste_malloc(dim*sizeof(int));
        if (qj==NULL)
            { not_enough_memory(); return(-1); }
        nc2=(int *)muste_malloc(dim*sizeof(int));
        if (nc2==NULL)
            { not_enough_memory(); return(-1); }

        for (i=0; i<dim; ++i) nc2[i]=nc[i];
        nc2[in]=nc[in]*nc[i1];
        for (i=i1; i<dim-1; ++i) nc2[i]=nc[i+1];
        for (i=0; i<dim; ++i) pj[i]=0;
        while (1)
            {
            ind1=jindex(dim,nc,pj);
            for (i=0; i<dim; ++i) qj[i]=pj[i];
            qj[in]=pj[in]*nc[i1]+pj[i1];
            for (i=i1; i<dim-1; ++i) qj[i]=pj[i+1];
            ind2=jindex(dim-1,nc2,qj);
            f2[ind2]=f[ind1];
            i=0;
            while (1)
                {
                if (pj[i]<nc[i]-1) { ++pj[i]; break; }
                pj[i]=0;
                ++i; if (i==dim) break;
                }
            if (i==dim) break;
            }
        varname[in]="N";
        for (i=i1+1; i<dim; ++i) varname[i-1]=varname[i];
        cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc[i-1];
        for (i=cumnc[i1]; i<nclass; ++i) cname[i]=cname[i+nc[i1]];
        cumnc[0]=0; for (i=1; i<=in; ++i) cumnc[i]=cumnc[i-1]+nc[i-1];
        if (nclass-1+nc2[in]-nc[in]>MAXT)
            {
            sur_print("\nNot enough space for N labels!");
            WAIT; return(-1);
            }
        for (i=nclass-1; i>=cumnc[in]+nc[in]; --i)
            cname[i+nc2[in]-nc[in]]=cname[i];
        for (i=0; i<nc2[in]; ++i)
            {
            sprintf(s,"%d",i+1);
            cname[cumnc[in]+i]=ptext;
            k=store_label(s); if (k<0) return(-1);
            }
        cumnc[0]=0; for (i=1; i<dim-1; ++i) cumnc[i]=cumnc[i-1]+nc2[i-1];
        strcpy(nimi,word[1]); strcat(nimi,"D");
        ctypes(ctype,dim-1,nc2,cname);
        digits=digts(f2,dim-1,nc2);
        *s=EOS;
        if (ndec)
            {
            for (i=0; i<digits; ++i) strcat(s,"1"); strcat(s,".");
            for (i=0; i<ndec; ++i) strcat(s,"1");
            }
        if (i1<ncvar) --ncvar;
        print_ftable(nimi,results_line,eout,dim-1,nc2,cumnc,ctype,
             f2,ZERO,ZERO,varname,cname,ncvar,digits,0,0,s,type);
        return(1);
        }

/* tabj.c 24.12.1987/SM (25.12.1987)
   TABJ <table>,C,C1,C2,Cn,L
*/

static int tabj()
        {
        int i,k;
        int ncvar;
        char nimi[LLENGTH];
        int digits;
        int i1,ic1,ic2;
        int ncell,nclass;
        int ind1,ind2,ui;
    //  char *p;
        char s[LLENGTH];

        results_line=0;
        if (g<6)
            {
         sur_print("\nUsage: TABJ <table>,C,C1,C2,Cn,L");
         sur_print("\n       joins classes C1 and C2 of classifier C as Cn.");
         WAIT; return(1);
            }
        if (g>6)
            {
            results_line=edline2(word[6],1,1);
            if (results_line==0) return(1);
            }
        ptext=text;
        i=read_ftable(word[1],&f,&dim,&ncvar,nc,varname,cname,type,&ndec);
        if (i<0) return(1);

        k=strlen(word[2]);
        for (i=0; i<dim; ++i)
            if (strncmp(word[2],varname[i],k)==0) break;
        if (i==dim)
            {
            sprintf(sbuf,"\nClassifier %s not found!",word[2]);
            sur_print(sbuf); WAIT; return(1);
            }
        i1=i;
        cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc[i-1];
        k=strlen(word[3]);
        for (i=0; i<nc[i1]; ++i)
            if (strncmp(word[3],cname[cumnc[i1]+i],k)==0) break;
        if (i==nc[i1])
            {
            sprintf(sbuf,"\nClass %s not found!",word[3]);
            sur_print(sbuf); WAIT; return(1);
            }
        ic1=i;
        k=strlen(word[4]);
        for (i=0; i<nc[i1]; ++i)
            if (strncmp(word[4],cname[cumnc[i1]+i],k)==0) break;
        if (i==nc[i1])
            {
            sprintf(sbuf,"\nClass %s not found!",word[4]);
            sur_print(sbuf); WAIT; return(1);
            }
        ic2=i;
                /* cn=word[5] */

        ncell=1; nclass=0; for (i=0; i<dim; ++i)
            { ncell*=nc[i]; nclass+=nc[i]; }
        f2=(FREQ *)muste_malloc(ncell*sizeof(FREQ));
        if (f2==NULL)
            { not_enough_memory(); return(1); }
        pj=(int *)muste_malloc(dim*sizeof(int));
        if (pj==NULL)
            { not_enough_memory(); return(1); }
        nc2=(int *)muste_malloc(dim*sizeof(int));
        if (nc2==NULL)
            { not_enough_memory(); return(1); }

        for (i=0; i<dim; ++i) nc2[i]=nc[i]; nc2[i1]=nc[i1]-1;
        for (ui=0; ui<ncell; ++ui) f2[ui]=STRUCTURAL_ZERO;
        for (i=0; i<dim; ++i) pj[i]=0;
        while (1)
            {
            int pji1;

            ind1=jindex(dim,nc,pj);
            pji1=pj[i1];
            if (pji1==ic2) pj[i1]=ic1;
            if (pj[i1]>ic2) --pj[i1];
            ind2=jindex(dim,nc2,pj);
            if (f2[ind2]==MISSING_VALUE || f[ind1]==MISSING_VALUE)
                f2[ind2]=MISSING_VALUE;
            else if (f2[ind2]==STRUCTURAL_ZERO)
                {
                if (f[ind1]!=STRUCTURAL_ZERO) f2[ind2]=f[ind1];
                }
            else if (f[ind1]!=STRUCTURAL_ZERO) f2[ind2]+=f[ind1];
            pj[i1]=pji1;
            i=0;
            while (1)
                {
                if (pj[i]<nc[i]-1) { ++pj[i]; break; }
                pj[i]=0;
                ++i; if (i==dim) break;
                }
            if (i==dim) break;
            }
        cname[cumnc[i1]+ic1]=word[5];
        for (i=cumnc[i1]+ic2; i<nclass-1; ++i) cname[i]=cname[i+1];
        cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc2[i-1];
        strcpy(nimi,word[1]); strcat(nimi,"J");
        ctypes(ctype,dim,nc2,cname);
        digits=digts(f2,dim,nc2);
        *s=EOS;
        if (ndec)
            {
            for (i=0; i<digits; ++i) strcat(s,"1"); strcat(s,".");
            for (i=0; i<ndec; ++i) strcat(s,"1");
            }
        print_ftable(nimi,results_line,eout,dim,nc2,cumnc,ctype,
             f2,ZERO,ZERO,varname,cname,ncvar,digits,0,0,s,type);
        return(1);
        }

/* tabplus.c 13.12.1987/SM (23.12.1987)
   TAB+ <table1>,<table2>,L
   TAB- TAB* TAB/

*/

static int tabplus()
        {
        int i;
        int ncvar;
        char nimi[LLENGTH];
        int digits;
        int ncell;
        char oper;
        char s[LLENGTH];
        int ui;
        int dim1,ndec1;
        double konst=0;
        char *p;
        int constind=0;

        oper=word[0][3];
        results_line=0;
        if (g<3)
            {
            sur_print("\nUsage: %s <table1>,<table2>,L",word[0]);
            WAIT; return(-1);
            }
        if (g>3)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return(-1);
            }
        ptext=text;
        i=read_ftable(word[1],&f,&dim,&ncvar,nc,varname,cname,type,&ndec);
        if (i<0) return(1);
        for (i=0; i<dim; ++i) cumnc[i]=nc[i];  /* vertailua varten */
        dim1=dim; ndec1=ndec;
        if (!muste_isnumber(word[2]))
            {
            ptext=text;
            i=read_ftable(word[2],&f2,&dim,&ncvar,nc,varname,cname,type,&ndec);
            if (i<0) return(-1);
            }
        else
            {
            konst=atof(word[2]);
            ndec=0; p=strchr(word[2],'.');
            if (p!=NULL)
                ndec=strlen(word[2])-1-(p-word[2]);
            constind=1;
            }
        if (dim!=dim1) { tabpluserror(); return(-1); }
        for (i=0; i<dim; ++i)
            {
            if (nc[i]!=cumnc[i]) { tabpluserror(); return(-1); }
            }
        ncell=1; for (i=0; i<dim; ++i) ncell*=nc[i];
        switch (oper)
            {
          case '+':
            if (constind)
                for (ui=0; ui<ncell; ++ui) f[ui]+=konst;
            else
                for (ui=0; ui<ncell; ++ui) f[ui]+=f2[ui];
            if (ndec1>ndec) ndec=ndec1;
            break;
          case '-':
            if (constind)
                for (ui=0; ui<ncell; ++ui) f[ui]-=konst;
            else
                for (ui=0; ui<ncell; ++ui) f[ui]-=f2[ui];
            if (ndec1>ndec) ndec=ndec1;
            break;
          case '*':
            if (constind)
                for (ui=0; ui<ncell; ++ui) f[ui]*=konst;
            else
                for (ui=0; ui<ncell; ++ui) f[ui]*=f2[ui];
            ndec+=ndec1;
            break;
          case '/':
            if (constind)
                { if (konst!=0.0) for (ui=0; ui<ncell; ++ui) f[ui]/=konst; }
            else
                for (ui=0; ui<ncell; ++ui)
                    {
                    if (f2[ui]!=ZERO) f[ui]/=f2[ui];
                    else f2[ui]=MISSING_VALUE;
                    }
            ndec+=ndec1;
            break;
            }

        cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc[i-1];
        strcpy(nimi,word[1]); *s=oper; s[1]=EOS; strcat(nimi,s);
        strcat(nimi,word[2]);
        ctypes(ctype,dim,nc,cname);
        digits=digts(f,dim,nc);
        *s=EOS;
        if (ndec)
            {
            for (i=0; i<digits; ++i) strcat(s,"1"); strcat(s,".");
            for (i=0; i<ndec; ++i) strcat(s,"1");
            }
        print_ftable(nimi,results_line,eout,dim,nc,cumnc,ctype,
             f,ZERO,ZERO,varname,cname,ncvar,digits,0,0,s,type);
        return(1);
        }

static int tabpluserror()
        { sur_print("\nIncompatible tables!"); WAIT; return(1); }


/* tabf.c 17.12.1987/SM (27.4.1992)

*/

static char model[LLENGTH];

static int tabfit()
        {
        int i;
        int ncvar;

        i=spec_init(r1+r-1); if (i<0) return(-1);
        results_line=0;
        if (g<3)
            {
            sur_print("\nUsage: TABFIT <table>,<model>,L");
            WAIT; return(-1);
            }
        if (g>3)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return(-1);
            }

        i=spfind("RESULTS"); if (i>=0) results=atoi(spb[i]);

        ptext=text;
        i=read_ftable(word[1],&f,&dim,&ncvar,nc,varname,cname,type,&ndec);
        if (i<0) return(-1);

        i=wfind("MODEL",word[2],1);
        if (i<0)
            {
            sprintf(sbuf,"\nMODEL %s not found!",word[2]);
            sur_print(sbuf); WAIT; return(-1);
            }
        edread(model,i+1);
        i=strlen(model); while (model[i-1]==' ') model[--i]=EOS;
        i=read_model(); if (i<0) return(-1);
        i=generate_x(); if (i<0) return(-1);
        i=fit(ncvar); if (i<0) return(-1);
        return(1);
        }

extern double muste_cdf_chi2();

static double *b,*sb,*v,*my;
static double deviance;
static int df;

static int fit(int ncvar)
        {
        int i,k;
        char s[LLENGTH];
        double da;
        char s1[32],s2[32];
        int digits;
        char nimi[LLENGTH];

        v=(double *)muste_malloc(nx*sizeof(double));
        if (v==NULL) { not_enough_memory(); return(-1); }
        b=(double *)muste_malloc(mx*sizeof(double));
        if (b==NULL) { not_enough_memory(); return(-1); }
        sb=(double *)muste_malloc(mx*sizeof(double));
        if (sb==NULL) { not_enough_memory(); return(-1); }
        my=(double *)muste_malloc(nx*sizeof(double));
        if (my==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<nx; ++i) v[i]=x[i+nx];
        if (*type=='F')
            {
            for (i=0; i<nx; ++i)
                {
                da=x[i+nx];
                if (da==0.0) x[i+nx]=log(0.5);
                else         x[i+nx]=log(da);
                }
            i=glm_fit(x+2*nx,nx,mx,x+nx,x,v,clab+2*8,b,sb,my,&deviance,&df);
            }
        else
            i=lin_fit(x+2*nx,nx,mx,x+nx,x,v,clab+2*8,b,sb,my,&deviance,&df);
        k=output_open(eout); if (k<0) return(-1);
        if (i==-2)
            {
            sprintf(s,"Model %s is saturated (XTAB.M saved!)",word[2]);
            print_line(s);
            return(-1);
            }
        if (i<0) return(-1);
        if (*type=='F')
            {
            da=1-muste_cdf_chi2(deviance,(double)df,1e-7);
            fnconv(deviance,accuracy+2,s1);  /* 27.4.92 */
            sprintf(s,"Table %s: Deviance=%s df=%d P=%6.4f",
                           word[1],spois(s1),df,da);
            }
        else
            {
            fnconv(deviance,accuracy+2,s1);  /* 27.4.92 */
            sprintf(s,"Table %s: Deviance=%s df=%d",
                           word[1],spois(s1),df);
            }
        print_line(s);
        sprintf(s,"Model: %s",model+1);
        print_line(s);
        if (results>0)
            {
/*          print_line("    estimate      s.e.      parameter");  */
             sprintf(s,"    estimate%.*ss.e.%.*sparameter",
                        accuracy,space,accuracy-2,space);
            print_line(s);

            for (i=0; i<mx; ++i)
                {
                if (sb[i]<0) { strcpy(s1,"   -      ");
                               strcpy(s2,"   aliased");
                             }
                else { sprintf(s1,"%*.*f",accuracy+3,accuracy-2,b[i]);
                       sprintf(s2,"%*.*f",accuracy+3,accuracy-2,sb[i]);
                     }
                sprintf(s,"%3d %s  %s  %.8s",i+1,s1,s2,clab+8*(i+2));
                print_line(s);
                }
            print_line(" ");
            }
        output_close(eout);
        if (results>70)
            {
            digits=digts(my,dim,nc);
            *s=EOS; for (i=0; i<digits; ++i) strcat(s,"1");
            if (accuracy>4) { strcat(s,".");
            for (i=0; i<accuracy-4; ++i) strcat(s,"1"); }
            strcpy(nimi,word[1]); strcat(nimi,"FIT");
            cumnc[0]=0; for (i=1; i<dim; ++i) cumnc[i]=cumnc[i-1]+nc[i-1];
            print_ftable(nimi,results_line,eout,dim,nc,cumnc,ctype,
                 my,ZERO,ZERO,varname,cname,ncvar,digits,0,0,s,type);
            }
        return(1);
        }

static int print_line(char *x)
        {
        output_line(x,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

/* tabf2.c 17.12.1987/SM (3.4.1988)
*/

static double *x2_fit;
static double *c_fit,*u_fit,*r_fit,*rinv_fit,*a_fit,*lmy_fit,*z_fit;
static double *y_fit;
static int *idel_fit;
static char *lab_fit;

static int glm_fit(double *X,int nx,int mx,double *Y,double *W,double *V,
           char *lab,double *b,double *sb,double *my,double *pdev,int *pdf)
// double *X;        /* design matrix nx*mx */
// int nx,mx;
// double *Y;        /* dependent variable */
// double *W;        /* weight variable */
// double *V;        /* initial weights */
// char *lab;        /* labels of variables (8 bytes each) */
// double *b;        /* regression coefficients */
// double *sb;       /* standard errors of b */
// double *my;       /* fitted values */
// double *pdev;     /* deviance */
// int *pdf;         /* degrees of freedom */
        {
        int i,j;
        double da;
        int iteration;
        double dev0=1e15;
        int ndel=0;

    x2_fit=NULL;
    c_fit=NULL;
    u_fit=NULL;
    r_fit=NULL;
    rinv_fit=NULL;
    a_fit=NULL;
    lmy_fit=NULL;
    z_fit=NULL;
    y_fit=NULL;
    idel_fit=NULL;
    lab_fit=NULL;

        *pdf=-mx; for (i=0; i<nx; ++i) *pdf+=(int)W[i];
        if (*pdf<=0) return(-2);
        i=glm_fit_space(mx,nx); if (i<0) return(-1);
        for (i=0; i<8*mx; ++i) lab_fit[i]=lab[i];
        for (i=0; i<nx; ++i) V[i]=sqrt(V[i]);
        for (i=0; i<nx; ++i) y_fit[i]=exp(Y[i]);
        while (1)
            {
            for (i=0; i<nx; ++i)
                {
                da=V[i]*W[i];
                c_fit[i]=da*Y[i];
                for (j=0; j<mx; ++j) x2_fit[i+nx*j]=da*X[i+nx*j];
                }
            i=mat_gram_schmidt(u_fit,r_fit,x2_fit,nx,mx,1e-10);
                                        /* x2_fit muuttuu! */
            if (i>0) break;

            sprintf(sbuf,"\nCol. %.8s linearly dependent on previous ones!",
                                        lab_fit+8*(-i)); sur_print(sbuf);
            reduce(X,lab_fit,nx,mx,-i);
            --mx;
            ++*pdf;
            idel_fit[ndel++]=-i;
            }
        i=mat_inv(rinv_fit,r_fit,mx,&da);
        for (j=0; j<mx; ++j)
            {
            da=0.0;
            for (i=0; i<nx; ++i) da+=u_fit[i+nx*j]*c_fit[i];
            a_fit[j]=da;
            }
        for (i=0; i<mx; ++i)
            {
            da=0.0;
            for (j=0; j<mx; ++j) da+=rinv_fit[i+mx*j]*a_fit[j];
            b[i]=da;
            }
/* printf("\nb0:"); mprint(b,mx,1); getch();    */

        if (*pdf<=0) return(1);

        iteration=0;
        while (1)
            {
            for (i=0; i<nx; ++i)
                {
                da=0.0;
                for (j=0; j<mx; ++j) da+=X[i+nx*j]*b[j];
                lmy_fit[i]=da;
                }
            for (i=0; i<nx; ++i) my[i]=exp(lmy_fit[i]);
            for (i=0; i<nx; ++i)
                z_fit[i]=lmy_fit[i]+(y_fit[i]-my[i])/my[i];
            for (i=0; i<nx; ++i) V[i]=sqrt(my[i]);

            for (i=0; i<nx; ++i)
                {
                da=V[i]*W[i];
                c_fit[i]=da*z_fit[i];
                for (j=0; j<mx; ++j) x2_fit[i+nx*j]=da*X[i+nx*j];
                }
            i=mat_gram_schmidt(u_fit,r_fit,x2_fit,nx,mx,1e-10);
            if (i<0)
                {
          sprintf(sbuf,"\nCol. %d linearly dependent on previous ones!",-i+1);
                sur_print(sbuf); WAIT; return(-1);
                }
            i=mat_inv(rinv_fit,r_fit,mx,&da);
            for (j=0; j<mx; ++j)
                {
                da=0.0;
                for (i=0; i<nx; ++i) da+=u_fit[i+nx*j]*c_fit[i];
                a_fit[j]=da;
                }
            for (i=0; i<mx; ++i)
                {
                da=0.0;
                for (j=0; j<mx; ++j) da+=rinv_fit[i+mx*j]*a_fit[j];
                b[i]=da;
                }

            da=0.0;
            for (i=0; i<nx; ++i)
                da+=W[i]*(y_fit[i]*(Y[i]-lmy_fit[i])-(y_fit[i]-my[i]));
            *pdev=2*da;

            ++iteration;
      sprintf(sbuf,"\niteration %d: Deviance=%g df=%d",iteration,*pdev,*pdf);
                sur_print(sbuf);
            if (fabs(*pdev/dev0-1)<1e-4)
                {
                std_errors(rinv_fit,mx,sb,lab_fit,r_fit,1.0);
                restore(b,sb,ndel,mx);
                return(1);
                }
            dev0=*pdev;
            if (iteration>3)
                {
                if (sur_kbhit())
                    {
                    i=sur_getch(); while (sur_kbhit()) sur_getch();
                    if (i=='.') { std_errors(rinv_fit,mx,sb,lab_fit,r_fit,1.0);
                    restore(b,sb,ndel,mx); return(1); }
                    }
                }
            }

        return(1);
        }

static int lin_fit(double *X,int nx,int mx,double *Y,double *W,double *V,
           char *lab,double *b,double *sb,double *my,double *pdev,int *pdf)
// double *X;        /* design matrix nx*mx */
// int nx,mx;
// double *Y;        /* dependent variable */
// double *W;        /* weight variable */
// double *V;        /* initial weights */
// char *lab;        /* labels of variables (8 bytes each) */
// double *b;        /* regression coefficients */
// double *sb;       /* standard errors of b */
// double *my;       /* fitted values */
// double *pdev;     /* deviance */
// int *pdf;         /* degrees of freedom */
        {
        int i,j;
        double da;
  //    int iteration;
  //    double dev0=1e15;
        int ndel=0;
/*
printf("\nX:"); mprint(X,nx,mx); getch();
printf("\nY:"); mprint(Y,nx,1); getch();
printf("\nW:"); mprint(W,nx,1); getch();
printf("\nV:"); mprint(V,nx,1); getch();
*/
        *pdf=-mx; for (i=0; i<nx; ++i) *pdf+=(int)W[i];
        if (*pdf<=0) return(-2);
        i=glm_fit_space(mx,nx); if (i<0) return(-1);
        for (i=0; i<8*mx; ++i) lab_fit[i]=lab[i];
        for (i=0; i<nx; ++i) V[i]=1.0; /* sqrt(V[i]); */
        for (i=0; i<nx; ++i) y_fit[i]=Y[i];
        while (1)
            {
            for (i=0; i<nx; ++i)
                {
                da=V[i]*W[i];
                c_fit[i]=da*Y[i];
                for (j=0; j<mx; ++j) x2_fit[i+nx*j]=da*X[i+nx*j];
                }
            i=mat_gram_schmidt(u_fit,r_fit,x2_fit,nx,mx,1e-10);
                                        /* x2_fit muuttuu! */
            if (i>0) break;

            sprintf(sbuf,"\nCol. %.8s linearly dependent on previous ones!",
                                        lab_fit+8*(-i+1)); sur_print(sbuf);
            reduce(X,lab_fit,nx,mx,-i);
            --mx;
            ++*pdf;
            idel_fit[ndel++]=-i;
            }
        i=mat_inv(rinv_fit,r_fit,mx,&da);
        for (j=0; j<mx; ++j)
            {
            da=0.0;
            for (i=0; i<nx; ++i) da+=u_fit[i+nx*j]*c_fit[i];
            a_fit[j]=da;
            }
        for (i=0; i<mx; ++i)
            {
            da=0.0;
            for (j=0; j<mx; ++j) da+=rinv_fit[i+mx*j]*a_fit[j];
            b[i]=da;
            }
/* printf("\nb0:"); mprint(b,mx,1); getch();      */

        for (i=0; i<nx; ++i)
            {
            da=0.0;
            for (j=0; j<mx; ++j) da+=X[i+nx*j]*b[j];
            lmy_fit[i]=da;
            }
        for (i=0; i<nx; ++i) my[i]=lmy_fit[i];
        da=0.0;
        for (i=0; i<nx; ++i)
            da+=W[i]*(y_fit[i]-my[i])*(y_fit[i]-my[i]);
        *pdev=da;
        std_errors(rinv_fit,mx,sb,lab_fit,r_fit,*pdev/(double)*pdf);
        return(1);
        }

static int glm_fit_space(int mx,int nx)
        {
        int i;

        i=mspace(&x2_fit,nx,mx); if (i<0) return(-1);
        i=mspace(&c_fit,nx,1); if (i<0) return(-1);
        i=mspace(&u_fit,nx,mx); if (i<0) return(-1);
        i=mspace(&r_fit,mx,mx); if (i<0) return(-1);
        i=mspace(&rinv_fit,mx,mx); if (i<0) return(-1);
        i=mspace(&a_fit,mx,1); if (i<0) return(-1);
        i=mspace(&lmy_fit,nx,1); if (i<0) return(-1);
        i=mspace(&z_fit,nx,1); if (i<0) return(-1);
        i=mspace(&y_fit,nx,1); if (i<0) return(-1);
        if (idel_fit!=NULL) idel_fit=(int *)muste_realloc(idel_fit,mx*sizeof(int));
        else          idel_fit=(int *)muste_malloc(mx*sizeof(int));
        if (idel_fit==NULL) { not_enough_memory(); return(-1); }
        if (lab_fit!=NULL) lab_fit=(char *)muste_realloc(lab_fit,8*mx);
        else          lab_fit=(char *)muste_malloc(8*mx);
        if (lab_fit==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int mspace(double **A,int m,int n)
        {
        if (*A!=NULL) *A=(double *)muste_realloc(*A,m*n*sizeof(double));
        else          *A=(double *)muste_malloc(m*n*sizeof(double));
        if (*A==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int std_errors(double *R,int mx,double *sb,char *lab,double *RRT,double ss)
        {
        int i,j,k;
        double da;
        char s[129];

        for (i=0; i<mx; ++i)
            {
            for (j=0; j<=i; ++j)
                {
                da=0.0;
                for (k=0; k<mx; ++k) da+=R[i+mx*k]*R[j+mx*k];
                da*=ss; RRT[i+mx*j]=da; RRT[j+mx*i]=da;
                }
            sb[i]=sqrt(RRT[i*(mx+1)]);
            }
        sprintf(s,"Covariances_of_parameters_in_model_%s/%s",word[1],word[2]);
        if (results>=0)
            matrix_save("PCOV.M",RRT,mx,mx,lab,lab,8,8,-1,s,0,0);
        return(1);
        }

static int reduce(double *X,char *lab,int nx,int mx,int i1)
        {
        int i;

        for (i=nx*i1; i<nx*(mx-1); ++i) X[i]=X[i+nx];
        for (i=8*i1; i<8*(mx-1); ++i) lab[i]=lab[i+8];
        return(1);
        }

static int restore(double *b,double *sb,int ndel,int mx)
        {
        int i,j,i1;

        if (ndel==0) return(1);
        for (j=ndel-1; j>=0; --j)
            {
            i1=idel_fit[j];
            for (i=ndel+mx-1; i>i1; --i) { b[i]=b[i-1]; sb[i]=sb[i-1]; }
            b[i1]=0.0; sb[i1]=-1.0;
            }
        return(1);
        }

/* tabfm.c 17.12.1987/SM (19.12.1987)
*/

static unsigned char *term;

static int read_model()
        {
        int i,n,k;
        char *p,*q,*h;
        int ui,dim2;
        char sign,usign;
        char *pterm[MAXDIM];
        int iterm[MAXDIM];
        int ik[MAXDIM];
        char model2[LLENGTH];

        strcpy(model2,model+1);
        p=strchr(model2,'=');
        if (p==NULL) p=model2; else ++p;

        dim2=1; for (i=0; i<dim; ++i) dim2*=2;
        term=(unsigned char *)muste_malloc(dim2);
        if (term==NULL) { not_enough_memory(); return(-1); }
        for (ui=0; ui<dim2; ++ui) term[ui]=0;
        term[0]=1;  /* GM included */

        usign='+';
        while (usign)
            {
            sign=usign;
            q=p;
            while (1)
                {
                if (*q==EOS) { usign=EOS; break; }
                if (*q=='+') { usign='+'; *q=EOS; break; }
                if (*q=='-') { usign='-'; *q=EOS; break; }
                ++q;
                }

            if (strcmp(p,"1")==0)
                { if (sign=='-') term[0]=0; else term[0]=1; p=q+1; continue; }
            h=strchr(p,'*');
            if (h!=NULL)
                {
                while ((h=strchr(p,'*'))!=NULL) *h=' ';
                n=split(p,pterm,MAXDIM);
                for (i=0; i<n; ++i)
                    {
                    ik[i]=var(pterm[i]);
                    if (ik[i]<0) return(-1);
                    }
                for (i=0; i<dim; ++i) iterm[i]=0;
                iterm[ik[0]]=1;
                while (1)
                    {
                    update_model(iterm,sign);
                    i=0;
                    while (1)
                        {
                        if (iterm[ik[i]]==0) { ++iterm[ik[i]]; break; }
                        iterm[ik[i]]=0;
                        ++i;
                        if (i==n) break;
                        }
                    if (i==n) break;
                    }
                p=q+1;
                continue;
                }
            h=strchr(p,'/');
            if (h!=NULL)
                {
                while ((h=strchr(p,'/'))!=NULL) *h=' ';
                n=split(p,pterm,MAXDIM);
                for (i=0; i<dim; ++i) iterm[i]=0;
                for (i=0; i<n; ++i)
                    {
                    k=var(pterm[i]);
                    if (k<0) return(-1);
                    iterm[k]=1;
                    update_model(iterm,sign);
                    }
                p=q+1;
                continue;
                }
            h=strchr(p,'.');
            if (h!=NULL)
                {
                while ((h=strchr(p,'.'))!=NULL) *h=' ';
                n=split(p,pterm,MAXDIM);
                for (i=0; i<dim; ++i) iterm[i]=0;
                for (i=0; i<n; ++i)
                    {
                    k=var(pterm[i]);
                    if (k<0) return(-1);
                    iterm[k]=1;
                    }
                update_model(iterm,sign);
                p=q+1; continue;
                }
            k=var(p); if (k<0) return(-1);
            for (i=0; i<dim; ++i) iterm[i]=0; iterm[k]=1;
            update_model(iterm,sign);
            p=q+1;
            }
/*
printf("\nmodel:");
for (ui=0; ui<dim2; ++ui) printf("%d",term[ui]); getch();
*/
        return(1);
        }

static int var(char *x)
        {
        int i,len;

        len=strlen(x);
        for (i=0; i<dim; ++i)
            if (strncmp(x,varname[i],len)==0) break;
        if (i<dim) return(i);
        sprintf(sbuf,"\nUnknown variable %s in model!",x); sur_print(sbuf);
        WAIT;
        return(-1);
        }

static int update_model(int *iterm,char sign)
        {
        int ui;
        int i;

        ui=iterm[dim-1];
        for (i=dim-2; i>=0; --i)
            ui=2*ui+iterm[i];
        if (sign=='+') term[ui]=1; else term[ui]=0;
        return(1);
        }

/* tabfx.c 18.12.1987/SM (26.4.1992)

*/

static int generate_x()
        {
        int i,k,k0,n;
        int iterm[MAXDIM];
        double dx;
        int lin,col;
        int level[MAXDIM];
        int ik[MAXDIM];

        mx=0;
        for (i=0; i<dim; ++i) iterm[i]=0;
        while (1)
            {
            if (in_model(iterm))
                {
                k=1;
                for (i=0; i<dim; ++i) if (iterm[i]==1) k*=nc[i]-1;
                mx+=k;
                }
            i=0;
            while (1)
                {
                if (iterm[i]==0) { iterm[i]=1; break; }
                iterm[i]=0;
                ++i;
                if (i==dim) break;
                }
            if (i==dim) break;
            }
        nx=1; for (i=0; i<dim; ++i) nx*=nc[i];

/* printf("\nmx=%d nx=%d",mx,nx); getch();      */
        dx=(double)(mx+2)*nx;
        x=(double *)muste_malloc((int)dx*sizeof(double));
        if (x==NULL) { not_enough_memory(); return(-1); }
        rlab=(char *)muste_malloc(8*nx+1);
        if (rlab==NULL) { not_enough_memory(); return(-1); }
        clab=(char *)muste_malloc(8*(mx+2)+1);
        if (clab==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<(mx+2)*nx; ++i) x[i]=0.0;
        lin=0;
        for (i=0; i<dim; ++i) level[i]=0;
        while (1)
            {
            col=0;
            for (i=0; i<dim; ++i) iterm[i]=0;
            while (1)
                {
                if (in_model(iterm))
                    {
                    n=0; k=1;
                    for (i=0; i<dim; ++i)
                        if (iterm[i]==1) { ik[n++]=i; if (!level[i]) k=0; }
                    if (n==0) { x[lin+nx*(col+2)]=1.0; ++col; }
                    else
                        {
                        k0=1; for (i=0; i<n; ++i) k0*=nc[ik[i]]-1;
                        if (k)
                            {
                            k=level[ik[0]]-1;
                            for (i=1; i<n; ++i)
                                {
                                k=(nc[ik[i]]-1)*k+level[ik[i]]-1;
                                }
                            x[lin+nx*(col+k+2)]=1.0;
                            }
                        col+=k0;
                        }
                    }
                i=dim-1;
                while (1)
                    {
                    if (iterm[i]==0) { iterm[i]=1; break; }
                    iterm[i]=0;
                    --i;
                    if (i<0) break;
                    }
                if (i<0) break;
                }
            i=dim-1;
            while (1)
                {
                if (level[i]<nc[i]-1) { ++level[i]; break; }
                level[i]=0;
                --i;
                if (i<0) break;
                }
            if (i<0) break;
            ++lin;
            }
        for (i=0; i<nx; ++i)
            {
            if (f[i]>MISSING_VALUE/100.0) { x[i]=0.0; x[i+nx]=0.0; }
            else { x[i]=1.0; x[i+nx]=f[i]; }
            }
/*      mprint(x,nx,mx+2); getch();     */
        col_labels();
        row_labels();
        sprintf(expr,"Data_matrix_%s/%s",word[1],word[2]);
        if (results>=0)
            matrix_save("XTAB.M",x,nx,mx+2,rlab,clab,8,8,-1,expr,0,0);

        return(1);
        }

static int in_model(int *iterm)
        {
        int ui;
        int i;

        ui=iterm[dim-1];
        for (i=dim-2; i>=0; --i)
            ui=2*ui+iterm[i];
        return((int)term[ui]);
        }

static int col_labels()
        {
        int i,n,k,h;
        int iterm[MAXDIM];
        int col;
        int ik[MAXDIM];
        int lev[MAXDIM];
        char y[LLENGTH];

        sprintf(clab,"Weight  %8.8s",word[1]);
/*      strcpy(clab,"Weight  Freq    ");       */
        col=0;
        for (i=0; i<dim; ++i) iterm[i]=0;
        while (1)
            {
            if (in_model(iterm))
                {
                n=0;
                for (i=0; i<dim; ++i) if (iterm[i]) ik[n++]=i;
                if (n==0)  strcat(clab,"1       ");
                else
                    {
                    for (i=0; i<n; ++i) lev[i]=1;
                    while (1)
                        {
                        h=0;
                        for (k=n-1; k>=0; --k)
                            h+=sprintf(y+h,"%c%d",*varname[ik[k]],lev[k]+1);
                        for (i=strlen(y); i<8; ++i) y[i]=' '; y[8]=EOS;
                        strcat(clab,y);
                        i=n-1;
                        while (1)
                            {
                            if (lev[i]<nc[ik[i]]-1) { ++lev[i]; break; }
                            lev[i]=1;
                            --i;
                            if (i<0) break;
                            }
                        if (i<0) break;
                        }
                    }
                }
            i=dim-1;
            while (1)
                {
                if (iterm[i]==0) { iterm[i]=1; break; }
                iterm[i]=0;
                --i;
                if (i<0) break;
                }
            if (i<0) break;
            }
        return(1);
        }

static int row_labels()
        {
        int i,h;
        int level[MAXDIM];
        char y[LLENGTH];

        *rlab=EOS;
        for (i=0; i<dim; ++i) level[i]=0;
        while (1)
            {
            h=0;
            for (i=dim-1; i>=0; --i)
                h+=sprintf(y+h,"%c%d",*varname[i],level[i]+1);
            for (i=strlen(y); i<8; ++i) y[i]=' '; y[8]=EOS;
            strcat(rlab,y);
            i=dim-1;
            while (1)
                {
                if (level[i]<nc[i]-1) { ++level[i]; break; }
                level[i]=0;
                --i;
                if (i<0) break;
                }
            if (i<0) break;
            }
        return(1);
        }

/* tprint2.c 30.3.1986/SM (25.12.1987)
*/

static int print_ftable(char *name,int line,char *eout,int dim,int *nc,
       int *cumnc,int *ctype,FREQ *f,FREQ total,FREQ missing,
       char **varname,char **cname,int colvar,int minwidth,
       int isum,int ipros,char *form,char *type)

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
// char *form;              /* ""=FREQ *f   "123.12"=double *f; */
// char *type;              /* F,X etc. */
        {
        int i,k,h,m,z;
        int nlines;     /* # of freq lines */
        int lin;
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
        double *pdf;

        pdf=(double *)f;  /* elements not frequncies, *form!=EOS */
        if (strlen(form)>minwidth) minwidth=strlen(form);
        sum=NULL;
        nlines=1;
        for (i=colvar; i<dim; ++i) nlines*=nc[i];

        starwidth=1;
        for (i=0; i<dim; ++i)
            {
            h=0;
            for (k=0; k<nc[i]; ++k)
                {
                len=strlen(cname[cumnc[i]+k]);
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
/*      if (colvar && varwidth[colvar-1]>colwidth) colwidth=varwidth[colvar-1];
*/
        for (i=0; i<colvar; ++i)
            if (colwidth<varwidth[i]) colwidth=varwidth[i];

        if (ipros && colwidth<5) colwidth=5;
        if (isum && colwidth<3) colwidth=3;

        strcpy(x,"TABLE "); strcat(x,name);
        if (line)
            {
            ch1=muste_next_label('A');
            *value=ch1; value[1]=EOS; edwrite(value,line+1,0);
            ch2=muste_next_label(ch1);
            *value=' '; value[1]=ch1; value[2]=','; value[3]=ch2;
            value[4]=','; value[5]=EOS; strcat(value,type);
            if (isum)
                {
                if (isum==1 || isum==3) strcat(value,"C");
                else strcat(value,"R");
                if (isum==3) strcat(value,"R");
                }
            if (ipros) strcat(value,"%");

            strcat(x,value);
            k=line+nlines+colvar+1;
            if (k<=r2) { *value=ch2; value[1]=EOS; edwrite(value,k,0); }
            }
        if (total)
            {
            strcat(x,"   N="); sprintf(value,"%d",(int)total); strcat(x,value);
            if (missing)
                {
                strcat(x," N(missing)=");
                sprintf(value,"%d",(int)missing); strcat(x,value);
                }
            }
        output_line(x,eout,line); if (line) ++line;

        for (i=0; i<colvar; ++i)
            {
            cum=cumnc[i];
            tline_init(x);
            tline_write(varname[i],x,freqcol-starwidth-1);
            h=1; for (k=i+1; k<colvar; ++k) h*=nc[k];
            coldiff=(colwidth+1)*h;
            col=freqcol+colwidth;
            m=1; for (k=0; k<i; ++k) m*=nc[k];
            for (h=0; h<m; ++h)
                for (k=0; k<nc[i]; ++k)
                    {
                    len=strlen(cname[cum+k]);
                /*  len=colwidth; */
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
                }
            if (ipros==2)
                {
                sum=(FREQ *)muste_malloc(nlines*sizeof(FREQ));
                if (sum==NULL) { not_enough_memory(); return(-1); }
                for (i=0; i<nlines; ++i)
                   { sum[i]=0; for (k=0; k<m; ++k) sum[i]+=f[k*nlines+i]; }
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
                    k=cumnc[i]+class[i];
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
                    {
                    if (f[z]==MISSING_VALUE) strcpy(value,"-");
                    else if (f[z]==STRUCTURAL_ZERO) strcpy(value,"*0");
                    else fconv(pdf[z],form,value);
                    }
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
                    sprintf(value,"%f",fsum);

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
                 /* itoa(fsum,value,10);  */
                    sprintf(value,"%g",fsum);

                tline_write(value,x,col-strlen(value));
                z+=nlines;
                col+=colwidth+1;
                }
            if (isum==3)
                {
                if (ipros) strcpy(value,"100.0");
                else       sprintf(value,"%f",ftotal);
                tline_write(value,x,col-strlen(value));
                }
            output_line(x,eout,line); if (line) ++line;
            }

        if (ipros==1 || ipros==2) muste_free(sum);

        tline_init(x);
        output_line(x,eout,line); if (line) ++line;
        output_close(eout);
        if (sum!=NULL) muste_free(sum);
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

