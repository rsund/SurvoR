/* _var.c 28.4.1986/SM (13.7.1994) (29.9.1996)
*/

#include <Rmath.h>
#include "survo.h"
#include "survoext.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survolib.h"
/* #include "survodat.h" */

#define NDATA 12
#define MAXARG 10
#define MAXEARG 1000 // RS CHA 255
#define EARG '\376'
#define EQ '\176'

/* specifications in the edit field */
extern char *splist;
extern char **spa, **spb, **spshad;
extern char **spb2;
extern int spn;
extern char *spl;
extern int global;
extern double *arvo; /* vain arit.c tarvitsee  */


// RS CHA extern -> static (the same variables as in arit.c)
static int earg_varattu=0;
static int n_earg=0;
static double *earg;
static int l_virhe;

static SURVO_DATA d;
static SURVO_DATA sd[NDATA];

static int nvar,var[EP4];
static char lauseke[LLENGTH];
static long jnro;
static int lag;
static int vm_act; /* ennen uusien muuttujien perustamista */
static int ndata;
/* int sdata;  */
static char sdat_list[LLENGTH], *sdat[NDATA];

static char *str_opnd[MAXARG+4]; /* 11.10.2003 */

static int muunnos;
static double  *xx;
static long  *oxx;
static long nxx;
static long nx2; /* # of non-missing values 30.12.2002 */
static double mean,stddev;
static double sum;            /* 11.10.1996 */

static int str_muunnos=0;
static int first_new_var=0; /* 18.3.92 */


static int spn_order;

static double level;  /* ainakin muunto2 käyttää */

/* varstr-funktiot */
static int str_var,str_lag,str_var_start,str_var_len;
static char str_vasen[LLENGTH];
static int code_ind=0;
static unsigned char code[256];

static int varhaku(char *sana)
        {
        int i;
        char *p;
        char tyyppi;
        char vartype[LLENGTH];
        int varlen=0;

        tyyppi='4';
        p=strchr(sana,':');
        if (p!=NULL)
            {
            *p=EOS;
            tyyppi=*(p+1);
            if (tyyppi==EOS || strchr("1248S",tyyppi)==NULL)
                {
                if (tyyppi==EOS) tyyppi=' ';
                sprintf(sbuf,"\nUnknown type %c for %s!",tyyppi,sana); sur_print(sbuf);
                sur_print("\nPermitted types are 1,2,4,8 and S.");
                WAIT; return(-1);
                }
            if (tyyppi=='S')
                {
                varlen=atoi(p+2);
                if (varlen<=0)
                    {
                    sur_print("\nEnter the type of a string variable as Sn ");
                    sur_print("\nwhere n is an integer >0.                 ");
                    WAIT; return(-1);
                    }
                }
            }
        i=varfind2(&d,sana,0);
        if (i<0)
            {
            if (d.type!=2)
                {
                sprintf(sbuf,"\nVariable %s not found!",sana);
                sur_print(sbuf); WAIT; return(-1);
                }
            else
                {
                strncpy(vartype,space,(unsigned int)(d.d2.extra-4));
                *vartype=tyyppi;
                vartype[1]='A'; vartype[2]='-';
                if (tyyppi!='S') varlen=(int)(tyyppi-'0');
                i=fi_var_save(&d.d2,d.d2.m,vartype,varlen,sana);
                if (i<0) return(-1);
                d.m=d.d2.m;
                i=d.m-1;
                if (first_new_var==0) first_new_var=i; /* 18.3.92 */
                }
            }
        return(i);
        }

static void poista_var()
        {
        int i,k,h;

        k=0;
        for (i=0; i<d.m_act; ++i)
            {
            if (!nvar) { if (d.v[i]==var[0]) { ++k; continue; } }
            else
                {
                for (h=0; h<nvar; ++h)
                    {
                    if (d.v[i]==var[h]) { ++k; break; }
                    }
                if (h<nvar) continue;
                }
            d.v[i-k]=d.v[i];
            }
        d.m_act-=k;
        }


static int spread3_var(char *x, int j)
        {
        int i,k,pos;
        char *p;
        char xs[LLENGTH];

        pos=1;
        while (pos<ed1)
            {
            p=strchr(x+pos,'=');
            if (p==NULL) break;
            if (*(p+1)==EOS) break;
            if (*(p+1)=='=') { pos+=2; continue; }

/* Aktivoidun kohdan ohittaminen VAR-listan osalta */
            if (j==r1+r-1 && pos==1)   /* 1.3.1992 */
                {
                p=strstr(x+1," / ");
                if (p==NULL) break;
                p=strchr(p+1,'=');
                if (p==NULL) break;
                }

            if (spn>=specmax) return(-spn);
            pos=p-x; i=pos-1;
            while (i>0 && x[i]!=' ') --i;
            if (spl-splist+pos-i+1>speclist) return(-spn);
            strncpy(spl,x+i+1,(unsigned int)(pos-i-1));
            spa[spn]=spl; spl+=pos-i; *(spl-1)=EOS;
            i=pos+1;
            while (i<ed1 && x[i]!=' ') ++i;
            if (spl-splist+i-pos+1>speclist) return(-spn);
            strncpy(spl,x+pos+1,(unsigned int)(i-pos-1));
            spb[spn++]=spl; spl+=i-pos; *(spl-1)=EOS;

            if (*(spl-2)=='&') { k=jatkorivit(j+1);
                                 if (k<0) return(-spn);
                               }
            spshad[spn-1]=NULL;
            if (zs[j]!=0)
                {
                edread(xs,zs[j]);
                if (spl-splist+i-pos+1>speclist) return(-spn);
                strncpy(spl,xs+pos+1,(unsigned int)(i-pos-1));
                spshad[spn-1]=spl; spl+=i-pos; *(spl-1)=EOS;
                }

            ++pos;
            }
        return(spn);
        }


static int spread2_var(int lin, int *raja1)
        {
        char raja[12];
        int j,i;
        char x[LLENGTH];

        strcpy(raja,"*..........");
        for (j=lin-1; j>0; --j)
            {
            edread(x,j);
            i=muste_instr(x,raja);
            if (i>=0) break;
            }
        *raja1=j;
        for (j=*raja1+1; j<=ed2; ++j)
            {
            edread(x,j);
            if (global==1)
                {
                i=muste_instr(x,"*GLOBAL*");
                if (i>0) global=0;
                }
            i=muste_instr(x,raja);
            if (i>=0) break;
            if (j==r1+r-1) continue; /* aktivoidun rivin ohittaminen */
            spn=spread3_var(x,j); if (spn<0) return(spn);
            }


/*  Rprintf("\n"); for (i=0; i<spn; ++i) Rprintf("\n%s=%s varjo=%s",
                                         spa[i],spb[i],spshad[i]); getch();
*/
        return (spn);
        }

static int varnimet()
        {
 // RS REM       extern SURVO_DATA d;
        int i,k;
        char nimi[LLENGTH];

        for (i=0; i<d.m_act; ++i)
            {
            strcpy(nimi,d.varname[d.v[i]]);
            k=strlen(nimi); while (nimi[k-1]==' ') nimi[--k]=EOS;

            if (spn>=specmax) return(-spn);
            if (spl-splist+k+1>speclist) return(-spn);

            strncpy(spl,nimi,(unsigned int)k);
            spa[spn]=spl; spb[spn]=NULL;
            spl+=k+1; *(spl-1)=EOS;
            ++spn;
            }
        return(spn);
        }

static void spec_error()
        {
        sprintf(sbuf,"\nToo many active variables + specifications (specmax=%d)",
                        specmax); sur_print(sbuf);
        sprintf(sbuf,"\nor too much text in specifications (speclist=%d)",speclist);
        sur_print(sbuf); WAIT;
        }

static int sp_init_var(int lin, int m) /* m = aktiivisten muuttujien lkm */
        {
/*
        int i,tila;
        char *p;
        long l;
*/
        int spn1;
        int raja1;
        char x[LLENGTH];


/*      l=(long)speclist+9L*(long)m;
        if (l>65535L) l=65535L;
        speclist=l;
        specmax=specmax+m;
        if (specmax>8192) specmax=8192;
*/
        sp_check();
        speclist+=200; /* varnimet() */ // RS ADD
        speclist+=9*(m+3)+4;   /* 3 sp2_init() */
        specmax+=m+3+2;


        splist=muste_malloc((unsigned int)speclist);
        if (splist==NULL) { not_enough_mem_for_spec(); return(-1); }
        spa=(char **)muste_malloc(specmax*sizeof(char *));
        if (spa==NULL) { not_enough_mem_for_spec(); return(-1); }
        spb=(char **)muste_malloc(specmax*sizeof(char *));
        if (spb==NULL) { not_enough_mem_for_spec(); return(-1); }
        spb2=(char **)muste_malloc(specmax*sizeof(char *));
        if (spb2==NULL) { not_enough_mem_for_spec(); return(-1); }
        spshad=(char **)muste_malloc(specmax*sizeof(char *));
        if (spshad==NULL) { not_enough_mem_for_spec(); return(-1); }
        arvo=(double *)muste_malloc(specmax*sizeof(double));
        if (arvo==NULL) { not_enough_mem_for_spec(); return(-1); }

        spn=0; spl=splist; global=0;

        spn=varnimet(); if (spn<0) { spxxxx(); return(spn); }

        edread(x,lin); spn=spread3_var(x,lin);
        if (spn<0) { spxxxx(); return(spn); }
        spn=spread2_var(lin,&raja1);
        if (spn<0) { spxxxx(); return(spn); }
        if (raja1==0) { spxxxx(); return(spn); }
        spn1=spn;
        global=1;
        spn=spread2_var(1,&raja1);
        if (spn<0) { spxxxx(); return(spn); }
        if (global==1) spn=spn1;
        spxxxx(); return(spn);
        }

static int sp_write(char *nimi, double y)
        {
        int k=strlen(nimi);

        strncpy(spl,nimi,(unsigned int)k);
        spa[spn]=spl; spb[spn]=NULL;
        spl+=k+1; *(spl-1)=EOS;
        arvo[spn]=y;
        ++spn;
        return(1);
        }

static int sp2_init()
        {
        int i;
// RS REM        extern SURVO_DATA d;

        i=sp_write("MISSING",MISSING8); if (i<0) return(-1);
        i=sp_write("ORDER",MISSING8); if (i<0) return(-1);
        spn_order=spn-1;
        i=sp_write("N",(double)d.n); if (i<0) return(-1);
        return(1);
        }

static int lue_arvot(long j)
        {
// RS REM        extern SURVO_DATA d;
        int i,k;

        for (i=0; i<d.m_act; ++i)
            {
            k=data_load(&d,j,d.v[i],&arvo[i]);
            if (k<0) return(-1);
            }
        arvo[spn_order]=(double)j;  /* ORDER */
        return(1);
        }

static void poista_uudet_muuttujat()  /* 18.3.92 */
        {
        if (d.type==2 && first_new_var)
            {
            fi_puts(&d.d2,&first_new_var,2,20L);
            }
        }

static void var_error(char *s)
        {
        sprintf(sbuf,"\nError in %s",s); sur_print(sbuf);
/*        WAIT; */
        poista_uudet_muuttujat();
        }

/* Declaration */
static int laske_var();

static void korvaa_var(char *s,char *x,char *y)
        {
        char *p,*q;
        char z[LLENGTH];
        int len=strlen(x);

        *z=EOS;
        p=s;
        while ((q=strstr(p,x))!=NULL)
            {
            strncat(z,p,(unsigned int)(q-p));
            strcat(z,y);
            p=q+len;
            }
        strcat(z,p);
        strcpy(s,z);
        }

static int aseta_earg(double luku,char *sana)
        {
        char sana2[5];

        sana[0]=EARG;
        if (n_earg>=MAXEARG)
            {
            sur_print("\nStack overflow in editorial functions!");
            sur_print("\nMAXEARG=255");
            WAIT; l_virhe=1;
            return(-1);
            }
        sana[1]=EOS;

        muste_itoa(n_earg,sana2,10); // // RS CHA strcat(sana,itoa(n_earg,sana2,10));
        strcat(sana,sana2);

        earg[n_earg++]=luku;
        return(n_earg-1);
        }

static int f_edit_var(char *s,double *x,int n,double *py)
        {
// RS REM        extern int n_earg;
        int i,k,len;
        char lauseke[LLENGTH];
        char xx[LLENGTH], *osa[MAXARG];
        char sana[7];     /*  EARG 1 2 3 4 EARG EOS */
        double y;



        len=strlen(s); s[len++]='(';
        i=0;
/**************************************************
for (k=0; k<spn; ++k)
    {
    Rprintf("%d | %s | %s |    \n",k,spa[k],spb[k]);
    }
getch();
***************************************************/

        while (i<spn &&
               ( spa[i]==NULL || *spa[i]==EOS ||
           spa[i][strlen(spa[i])-1]!=':' || strncmp(s,spa[i],(unsigned int)len)!=0) )
                 ++i;

        if (i==spn) { s[len-1]=EOS; return(-1); }

/*      if (!earg_varattu) { k=varaa_earg(); if (k<0) return(-1); } */
        strcpy(lauseke,spb[i]);
        strcpy(xx,spa[i]);
        i=split(xx+len,osa,MAXARG);
        if (i!=n)
           {
           sprintf(sbuf,"\nArgument error in function %s",s); sur_print(sbuf);
           l_virhe=1; WAIT; return(-1);
           }
        osa[n-1][strlen(osa[n-1])-2]=EOS;  /* ): poistetaan */
/*
    for (i=0; i<n; ++i) Rprintf("osa %d: %s\n",i+1,osa[i]); getch();
*/
        for (i=0; i<n; ++i)
            {
            k=aseta_earg(x[i],sana); if (k<0) return(-1);
            korvaa_var(lauseke,osa[i],sana);
            }
        laske_var(lauseke,&y);
        *py=y;
        n_earg-=n;
        return(1);
        }



#define NMAT 20
static double *mat_var[NMAT];
static char *rlab_var[NMAT],*clab_var[NMAT];
static int lr_var[NMAT],lc_var[NMAT];
static int m_var[NMAT],n_var[NMAT];
static int type_var[NMAT];
static char expr_var[NMAT][LNAME];
static int nmat_var=0;
static char mat_name_var[NMAT][9];


static int lab_find(char *x,char *lab,int m,int len)
        {
        char s[32];
        int i;

        strcpy(s,x);
        for (i=strlen(s); i<len; ++i) s[i]=' ';
        for (i=0; i<m; ++i)
            {
//          Rprintf("\n%s| %.8s| %d",s,lab+i*len,len); getch();
            if (strncmp(s,lab+i*len,len)==0) break;
            }

        if (i==m) return(-1);
        return(i+1);
        }


static void mat_function_var(char *f,char **s,int nn,double *yy)
        {
        int i,j=0,k;
        double xx[2];
/*        char *lab; */

/* Rprintf("f=%s nn=%d %s %s\n",f,nn,s[0],s[1]); getch(); */

        for (k=0; k<nmat_var; ++k)
            {
            if (strcmp(f,mat_name_var[k])==0) break;
            }
        if (nmat_var==0 || k==nmat_var)
            {

            if (nmat_var==NMAT)
                {
                sprintf(sbuf,"Too many matrices (more than %d)!",NMAT);
                sur_print(sbuf); WAIT; l_virhe=1; nmat_var=0; return;
                }

/* mat_load(f,&mat[k],&m[k],&n[k],&rlab[k],&clab[k],&lr[k],&lc[k]); */

    matrix_load(f,&mat_var[k],&m_var[k],&n_var[k],&rlab_var[k],&clab_var[k],&lr_var[k],&lc_var[k],
                        &type_var[k],expr_var[k]);

            strcpy(mat_name_var[k],f);
            ++nmat_var;

            }
        if (nn==1 && m_var[k]==1) { nn=2; s[1]=s[0]; s[0]="1"; }
        i=lab_find(s[0],rlab_var[k],m_var[k],lr_var[k]);
        if (i>0) xx[0]=i;
        else
            {
            laske_var(s[0],&xx[0]);
            sprintf(sbuf,"%g",xx[0]);    /* 9.9.1999 */
            i=lab_find(sbuf,rlab_var[k],m_var[k],lr_var[k]);
            if (i>0) xx[0]=i;
            }
        if (nn>1)
            {
            i=lab_find(s[1],clab_var[k],n_var[k],lc_var[k]);
            if (i>0) xx[1]=i;
            else
                {
                laske_var(s[1],&xx[1]);
                sprintf(sbuf,"%g",xx[1]);    /* 9.9.1999 */
                i=lab_find(sbuf,clab_var[k],n_var[k],lc_var[k]);
                if (i>0) xx[1]=i;
                }
            }

        i=xx[0]; if (nn>1) j=xx[1];
        if (i<1 || i>m_var[k] || (nn>1 && (j<1 || j>n_var[k])) )
            {
            sur_print("\nError in matrix index!"); WAIT;
            l_virhe=1;
            return;
            }
        if (nn==1)
            *yy=mat_var[k][i-1];
        else
            {
            *yy=mat_var[k][i-1+m_var[k]*(j-1)];
            }
        }

static int arg_virhe(char *s)
{
    sprintf(sbuf,"\n%s: Error in arguments",s);
    sur_print(sbuf);
    l_virhe=1;
    return(1);
}


static double mfunktio_var(char *s, double *x, int n)
        {
        int i;
        double y;
        char S[32];
/* ****************************
       Rprintf("mfunktio: %s\n",s);
     for (i=0; i<n; ++i) Rprintf("%g ",x[i]); Rprintf("\n"); getch();
***************************** */
        strncpy(S,s,31); S[31]=EOS;


// RS ADD START
    if (strcmp(S,"bin.f")==0 || strcmp(S,"BIN.f")==0 || strcmp(S,"Bin.f")==0 )
    {
        return(muste_pdf_binom(x[2],x[0],x[1]));
    }

    if (strcmp(S,"bin.F")==0 || strcmp(S,"BIN.F")==0 || strcmp(S,"Bin.F")==0 )
    {
        return(muste_cdf_binom(x[2],x[0],x[1]));
    }

    if (strcmp(S,"bin.G")==0 || strcmp(S,"BIN.G")==0 || strcmp(S,"Bin.G")==0 )
    {
        return(muste_inv_binom(x[2],x[0],x[1]));
    }

    if (strcmp(S,"poisson.f")==0 || strcmp(S,"POISSON.f")==0 || strcmp(S,"Poisson.f")==0 )
    {
        return(muste_pdf_poisson(x[1],x[0]));
    }

    if (strcmp(S,"poisson.F")==0 || strcmp(S,"POISSON.F")==0 || strcmp(S,"Poisson.F")==0 )
    {
        return(muste_cdf_poisson(x[1],x[0]));
    }

    if (strcmp(S,"poisson.G")==0 || strcmp(S,"POISSON.G")==0 || strcmp(S,"Poisson.G")==0 )
    {
        return(muste_inv_poisson(x[1],x[0]));
    }

    if (strcmp(S,"N.f")==0 || strcmp(S,"n.f")==0 )
    {
        return(muste_pdf_normal(x[2],x[0],x[1]));
    }

    if (strcmp(S,"N.F")==0 || strcmp(S,"n.F")==0 )
    {
        return(muste_cdf_normal(x[2],x[0],x[1]));
    }

    if (strcmp(S,"N.G")==0 || strcmp(S,"n.G")==0 )
    {
        return(muste_inv_normal(x[2],x[0],x[1]));
    }

    if (strcmp(S,"t.f")==0 || strcmp(S,"T.f")==0 )
    {
        return(muste_pdf_t(x[1],x[0]));
    }

    if (strcmp(S,"t.F")==0 || strcmp(S,"T.F")==0 )
    {
        return(muste_cdf_t(x[1],x[0]));
    }

    if (strcmp(S,"t.G")==0 || strcmp(S,"T.G")==0 )
    {
        return(muste_inv_t(x[1],x[0]));
    }

    if (strcmp(S,"chi2.f")==0 || strcmp(S,"CHI2.f")==0 || strcmp(S,"Chi2.f")==0 )
    {
        return(muste_pdf_chi2(x[1],x[0]));
    }

    if (strcmp(S,"chi2.F")==0 || strcmp(S,"CHI2.F")==0 || strcmp(S,"Chi2.F")==0 )
    {
        return(muste_cdf_chi2(x[1],x[0]));
    }

    if (strcmp(S,"chi2.G")==0 || strcmp(S,"CHI2.G")==0 || strcmp(S,"Chi2.G")==0 )
    {
        return(muste_inv_chi2(x[1],x[0]));
    }

    if (strcmp(S,"F.f")==0 || strcmp(S,"f.f")==0 )
    {
        return(muste_pdf_f(x[2],x[0],x[1]));
    }

    if (strcmp(S,"F.F")==0 || strcmp(S,"f.F")==0 )
    {
        return(muste_cdf_f(x[2],x[0],x[1]));
    }

    if (strcmp(S,"F.G")==0 || strcmp(S,"f.G")==0 )
    {
        return(muste_inv_f(x[2],x[0],x[1]));
    }

    if (strcmp(S,"gamma.f")==0 || strcmp(S,"GAMMA.f")==0 || strcmp(S,"Gamma.f")==0 )
    {
        return(muste_pdf_gamma(x[2],x[0],x[1]));
    }

    if (strcmp(S,"gamma.F")==0 || strcmp(S,"GAMMA.F")==0 || strcmp(S,"Gamma.F")==0 )
    {
        return(muste_cdf_gamma(x[2],x[0],x[1]));
    }

    if (strcmp(S,"gamma.G")==0 || strcmp(S,"GAMMA.G")==0 || strcmp(S,"Gamma.G")==0 )
    {
        return(muste_inv_gamma(x[2],x[0],x[1]));
    }

    if (strcmp(S,"beta.f")==0 || strcmp(S,"BETA.f")==0 || strcmp(S,"Beta.f")==0 )
    {
        return(muste_pdf_beta(x[2],x[0],x[1]));
    }

    if (strcmp(S,"beta.F")==0 || strcmp(S,"BETA.F")==0 || strcmp(S,"Beta.F")==0 )
    {
        return(muste_cdf_beta(x[2],x[0],x[1]));
    }

    if (strcmp(S,"beta.G")==0 || strcmp(S,"BETA.G")==0 || strcmp(S,"Beta.G")==0 )
    {
        return(muste_inv_beta(x[2],x[0],x[1]));
    }

    if (strcmp(S,"weibull.f")==0 || strcmp(S,"WEIBULL.f")==0 || strcmp(S,"Weibull.f")==0 )
    {
        return(muste_pdf_weibull(x[2],x[0],x[1]));
    }

    if (strcmp(S,"weibull.F")==0 || strcmp(S,"WEIBULL.F")==0 || strcmp(S,"Weibull.F")==0 )
    {
        return(muste_cdf_weibull(x[2],x[0],x[1]));
    }

    if (strcmp(S,"weibull.G")==0 || strcmp(S,"WEIBULL.G")==0 || strcmp(S,"Weibull.G")==0 )
    {
        return(muste_inv_weibull(x[2],x[0],x[1]));
    }

    if (strcmp(S,"exp.f")==0 || strcmp(S,"EXP.f")==0 || strcmp(S,"Exp.f")==0 )
    {
        return(muste_pdf_exp(x[1],x[0]));
    }

    if (strcmp(S,"exp.F")==0 || strcmp(S,"EXP.F")==0 || strcmp(S,"Exp.F")==0 )
    {
        return(muste_cdf_exp(x[1],x[0]));
    }

    if (strcmp(S,"exp.G")==0 || strcmp(S,"EXP.G")==0 || strcmp(S,"Exp.G")==0 )
    {
        return(muste_inv_exp(x[1],x[0]));
    }
// RS ADD END

        muste_strupr(S);

// RS ADD START
    /* R-style normal density */
    if (strcmp(S,"DNORM")==0)
    {
        if (n>3) return(muste_density_normal(x[0],x[1],x[2],(int)x[3]));
        return(muste_density_normal(x[0],x[1],x[2],(int)0));
    }

    if (strcmp(S,"MAX")==0) return(muste_max(x,n));
    if (strcmp(S,"MIN")==0) return(muste_min(x,n));
    if (strcmp(S,"MAXN")==0) return(muste_maxn(x,n));
    if (strcmp(S,"MINN")==0) return(muste_minn(x,n));
    if (strcmp(s,"C")==0)
    {
        if (n!=2)
        {
            arg_virhe(s);
        }
        return(muste_C(x[0],x[1]));
    }

    if (strcmp(S,"K_FACT")==0 || strcmp(S,"LK_FACT")==0)
    {
        int h;

        h=0;
        if (*S=='L') h=1;
        if (n!=2)
        {
            arg_virhe(s);
        }
        return(muste_k_fact(x[0],x[1],h));
    }

    if (strcmp(S,"GCD")==0)
    {
        return (gcd(x[0],x[1]));
    }
    if (strcmp(S,"MOD")==0)
    {
        return(muste_mod(x[0],x[1]));
    }
    if (strcmp(S,"ROOT")==0)
    {
        return (root(x[0],x[1]));
    }
    if (strcmp(S,"ROUND")==0)
    {
        return(muste_round(x[0],x[1]));
    }

    /* 14.8.2005 days from 1.1.2000 */
    if (strcmp(S,"DAYS")==0)
    {
        double date;
        sur_julian(x[0],x[1],x[2],&date);
        return(date-2451544.0);
    }

        if (strcmp(S,"NONDIV")==0) // 26.4.2009
            {
            return(nondiv(x[0],x[1]));
            }

        if (strcmp(S,"MTOTIENT")==0) // 30.4.2009
            {
            return(mtotient(x[0],x[1]));
            }

        if (strcmp(S,"BETA")==0) return(muste_beta(x[0],x[1])); // RS
        if (strcmp(S,"LBETA")==0) return(muste_lbeta(x[0],x[1])); // RS

        if (strcmp(S,"FIN.PV")==0) return(muste_fin_pv(x[0],x[1],x[2])); // RS
        if (strcmp(S,"FIN.FV")==0) return(muste_fin_fv(x[0],x[1],x[2])); // RS
        if (strcmp(S,"FIN.PMT")==0) return(muste_fin_pmt(x[0],x[1],x[2])); // RS

        if (strcmp(S,"BOXCOX")==0) return(muste_boxcox(x[0],x[1])); // RS
        if (strcmp(S,"BOXCOX.G")==0) return(muste_inv_boxcox(x[0],x[1])); // RS

        if (strcmp(S,"DISS")==0) return(muste_diss(x[0],x[1],(int)0)); // RS
        if (strcmp(S,"DISS.F")==0) return(muste_diss(x[0],x[1],(int) 1)); // RS

        if (strcmp(S,"BESTVAL")==0) return(muste_bestval(x[0],x[1])); // RS
 // RS ADD END

/* RS REM
        if (strcmp(S,"MAX")==0)
            {
            y=x[0];
            for (i=1; i<n; ++i) y=(x[i]>y)? (x[i]):(y);
            return(y);
            }
        if (strcmp(S,"MIN")==0)
            {
            y=x[0];
            for (i=1; i<n; ++i) y=(x[i]<y)? (x[i]):(y);
            return(y);
            }

       if (strcmp(S,"MAXN")==0)  // 6.4.2003
           {
           y=x[0]; k=0;
           for (i=1; i<n; ++i) if (x[i]>y) { y=x[i]; k=i; }
           return((double)(k+1));
           }
       if (strcmp(S,"MINN")==0)
           {
           y=x[0]; k=0;
           for (i=1; i<n; ++i) if (x[i]<y) { y=x[i]; k=i; }
           return((double)(k+1));
           }

        if (strcmp(s,"C")==0)
            {
            double u,v;
            int iu,iv;

            if (n!=2) { arg_virhe(s); }
            iv=v=x[0]; iu=u=x[1];
            if ((double)iu!=u) return(0.0);
            if ((double)iv!=v) return(0.0);
            if (u>v/2) u=v-u;
            if (u<0 || v<0) return(0.0);
            if (u==0) return(1.0);
            y=1.0;
            for (; u>0; --u, --v) y*=(v/u);
            return(y);
            }

        if (strcmp(S,"MOD")==0)
            return((double)((unsigned long)x[0]%(unsigned long)x[1]));

        if (strcmp(S,"ROUND")==0)
            {
            y=pow(10.0,x[1]);
            return(sur_round(x[0]*y)/y);
            }
*/

        if (*s=='M' && strncmp(s,"MAT_",4)==0)
            {
            mat_function_var(s+4,str_opnd,n,&y);
/* mat_element=0; // ++++  */
            return(y);
            }

            if (*s=='R' && strncmp(s,"R>",2)==0)
                {
                y=muste_R_function(s+2,x,n);
                return(y);
                }

/************************
        if (*s=='M' && strncmp(s,"MAT_",4)==0)
            {
            mat_function_var(s+4,x,n,&y);
            return(y);
            }
**********************/
        i=f_edit_var(s,x,n,&y); if (i>0) return(y);
/*      i=f_tiedosto(s,x,n,&y); if (i>0) return(y); */

/*  RS NYI Väliaikaisesti pois käytöstä
        i=family_f(s,x,n,&y); if (i>0) return(y);
*/
// Rprintf("FIXME: family_f in VAR unimplemented\n");

        l_virhe=1;
        return(MISSING8);
        }

static double lfact_var(double x) /* 7.9.2007 */
    {
    int n,i;
    double s;

    n=(int)x;
    if (n<1) return(MISSING8);
    s=0.0;
    for (i=2; i<=n; ++i) s+=log((double)i);
    return(s);
    }


static double funktio_var(char *s,double x)
        {
/*
        extern double probit();
        extern double uniform();
        extern double sur_rand0();
        extern double round();
        extern double lfact();
*/
        int i;
        double y;
        char S[32];

/* Rprintf("\nf1=%s x=%g|",s,x); getch(); // ++++  */
        if (*s==EOS) return(x);
        if (*s=='M' && strncmp(s,"MAT_",4)==0) /* siirretty alkuun 17.2.2004 */
            {
            mat_function_var(s+4,str_opnd,1,&y);
            return(y);
            }

        if (x==MISSING8) return(x);
        strncpy(S,s,31); S[31]=EOS; muste_strupr(S);
/* RS NYI Randit väliaikaisesti pois käytöstä */
        if (strcmp(S,"RAND")==0) return(sur_rand0(x,1));
        else if (strcmp(S,"URAND")==0) return(sur_rand0(x,2));
        else if (strcmp(S,"SRAND")==0) return(sur_rand0(x,3));
        else if (strcmp(S,"MRAND")==0) return(sur_rand0(x,4));
        else
// Rprintf("FIXME: RAND-functions in VAR unimplemented\n");

        if (strncmp(S,"SQR",3)==0)  /* 29.9.1996 x<0.0 etc. */
            { if (x<0.0) { l_virhe=1; return(0.0); } else return(muste_sqrt(x)); }
        else if (strcmp(S,"LOG")==0)
            { if (x<=0.0) { l_virhe=1; return(0.0); } else return(muste_log(x)); }
        else if (strcmp(S,"EXP")==0) return(muste_exp(x));
        else if (strcmp(S,"SIN")==0) return(muste_sin(x));
        else if (strcmp(S,"COS")==0) return(muste_cos(x));
        else if (strcmp(S,"TAN")==0) return(muste_tan(x));
        else if (strcmp(S,"ARCTAN")==0) return(muste_atan(x));
        else if (strcmp(S,"ARCSIN")==0) return(muste_asin(x));   /* 18.6.92 */
        else if (strcmp(S,"ARCCOS")==0) return(muste_acos(x));
        else if (strcmp(S,"ABS")==0) return(muste_fabs(x));
        else if (strcmp(S,"INT")==0) return(muste_floor(x));
        else if (strcmp(S,"ROUND")==0) return(sur_round(x));
        else if (strcmp(S,"SGN")==0) return(muste_sign(x)); // RS CHA
        else if (strcmp(S,"IND")==0) return(muste_ind(x)); // RS CHA
        else if (strcmp(S,"PROBIT")==0) return(probit(x));
        else if (strcmp(S,"RND")==0) return(uniform(x));

        else if (strcmp(S,"LFACT")==0) return(lfact_var(x)); /* 7.9.2007 */

// RS ADD START
    if (strcmp(S,"TOTIENT")==0) return(totient(x)); // 19.4.2009
    if (strcmp(S,"ZETA")==0) return(zeta(x));
    if (strcmp(S,"LGAMMA")==0) return(muste_lgamma(x)); // RS
    if (strcmp(S,"GAMMA")==0) return(muste_gamma(x)); // RS
    if (strcmp(S,"DIGAMMA")==0) return(muste_digamma(x)); // RS
    if (strcmp(S,"TRIGAMMA")==0) return(muste_trigamma(x)); // RS
    if (strcmp(S,"TETRAGAMMA")==0) return(muste_tetragamma(x)); // RS
    if (strcmp(S,"PENTAGAMMA")==0) return(muste_pentagamma(x)); // RS

            if (*s=='R' && strncmp(s,"R>",2)==0)
                {
                double fx[1]; fx[0]=x;
                y=muste_R_function(s+2,fx,1);
                return(y);
                }
// RS ADD END



/******************************
        if (*s=='M' && strncmp(s,"MAT_",4)==0)
            {
            mat_function_var(s+4,&x,1,&y);
            return(y);
            }
***********************************/
        i=f_edit_var(s,&x,1,&y); if (i>0) return(y);
/*      i=f_tiedosto(s,&x,1,&y); if (i>0) return(y); */

/* RS Väliaikaisesti pois käytöstä
        i=family_f(s,&x,1,&y); if (i>0) return(y);
*/
//Rprintf("FIXME: family_f() in VAR unimplemented\n");

        l_virhe=1;
        return(MISSING8);
        }


static int lag_arvo(char *muuttuja,double *y)
        {
        int i;
        long j;
// RS REM        extern SURVO_DATA d;

        j=jnro+(long)lag;
        if (j<1L || j>d.n) { *y=MISSING8; return(1); }
        i=varfind(&d,muuttuja); if (i<0) return(-1);
        data_load(&d,j,i,y);
        return(1);
        }

static int sup_arvo(char *muuttuja,double *y)
        {
        int i,k;
        long j;
        int sdata;
        char *p;

        p=strchr(muuttuja,':');
        if (p==NULL) { sprintf(sbuf,"Error in %s",muuttuja); sur_print(sbuf); WAIT;
                       l_virhe=1; return(-1); } // RS CHA exit -> lvirhe=1; return(-1)
        *p=EOS; ++p;
        sdata=atoi(muuttuja+1);
        if (sdata<1 || sdata>ndata)
            {
            sprintf(sbuf,"\nIndata D%d: not defined!",sdata); sur_print(sbuf); WAIT;
                    l_virhe=1; return(-1); // RS CHA exit(0) -> lvirhe=1; return(-1)
            }
        k=sdata-1;
        j=jnro+(long)lag;
        lag=0; /* 6.10.1989 */
        if (j<1L || j>sd[k].n) { *y=MISSING8; return(1); }
        i=varfind2(&sd[k],p,0);
        if (i<0)
            {
            sprintf(sbuf,"\nField %s not found in data %s!",p,sdat[k]);
            sur_print(sbuf); WAIT; l_virhe=1; return(-1); // RS CHA exit(0);
            }
        data_load(&sd[k],j,i,y);
        return(1);
        }

/* declarations for laske2_var */
static int laske_var();

static int laske2_var(char *muuttuja,double *y)
        {
        int i,k;
        char *pvar;

        pvar=NULL;
        if (*muuttuja==EARG)
            {
            *y=earg[atoi(muuttuja+1)];
            return(1);
            }
        if (*muuttuja=='D' && (muuttuja[2]==':' || muuttuja[3]==':') )
            { i=sup_arvo(muuttuja,y); return(i); }
        if (lag)
            {
            i=lag_arvo(muuttuja,y); lag=0; if (i<0) l_virhe=1;
            return(i);
            }
        i=spfind(muuttuja);
        if (i<0)
            {
            i=varfind2(&d,muuttuja,0);  /* itse outputmuuttuja */
            if (i<0)
                {
                sprintf(sbuf,"\nValue of %s not found!",muuttuja);
                sur_print(sbuf); WAIT;
                poista_uudet_muuttujat();  /* 18.3.92 */
                l_virhe=1; return(-1); // RS CHA exit(1) -> return(-1)
                }
            data_load(&d,jnro,i,y);
            return(1);
            }

        if (spb[i]==NULL) { *y=arvo[i]; return(1); }
        if (nvar) { pvar=spa[i]; spa[i]=NULL; }
        k=laske_var(spb[i],y);
        if (nvar) spa[i]=pvar;
        arvo[i]=*y;
        spb[i]=NULL;
        return(1);
        }

static double luku_var(char *sana,int len)
        {
        char *p;
        double tulos=1.0;
        int i;

        sana[len]=EOS;
        p=sana; if (*p=='-') ++p;
        if (strchr("1234567890.",*p)==NULL)
            {
            i=laske2_var(p,&tulos); if (i<0) return((double)1.0);
            if (*sana=='-') return(-tulos);
            return(tulos);
            }
        return(atof(sana));
        }


static double power_var(double x, double y)
        {
        short i;
        double f;
        if (y>=0 && y==floor(y) && y<10)
                {
                f=1;
                for (i=0; i<(short)y; ++i) f*=x;
                return(f);
                }
        return (pow(x,y));
        }


static double oper_var(double x1,double x2,char laji)
        {

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
            return(power_var(x1,x2));
            }
        return(0.0);
        }

static void supista_var(int *t,double opnd[],char op[],int v[])
        {

        while (*t>1)
            {
            if (v[*t-1]>v[*t-2]) return;
            opnd[*t-2]=oper_var(opnd[*t-2],opnd[*t-1],op[*t-2]);
            op[*t-2]=op[*t-1]; v[*t-2]=v[*t-1];
            --(*t);
            }
        }

static int syntax_error(char *s)
{
    sprintf(sbuf,"\nsyntax error in %s",s);
    sur_print(sbuf);
    l_virhe=1;
    return(1);
}


/* tarvittavat declarationit laske_var:ille */
static int pos_funktio();
static int varif_var();
static int arifor_var();

static int laske_var(char *lauseke, double *y)
        {
/*
        double luku();
        double oper();
        double funktio();
        double mfunktio();
*/
        char x[LLENGTH];
        char *p,*q;
        char sana[32];
        int len;
        double opnd[MAXARG+4]; char op[MAXARG+4]; int v[MAXARG+4];
        int t,n;
        int narg; /* Usean muuttujan funktion argumenttien lkm     */
        int i;
        double dlag;

static int mat_element;
static int n_mat_par;

/*// Rprintf("\nlaske %s",lauseke); getch();  */

        *sana=EOS;  /* 17.2.2004 ????  */


        if (*lauseke=='i')
            {
            if (strncmp(lauseke,"if(",3)==0)
                return(varif_var(lauseke,y));
            }
        if (*lauseke=='f')
            {
            if (strncmp(lauseke,"for(",3)==0)
                return(arifor_var(lauseke,y));
            }

        strcpy(x,lauseke);
        len=0;
        p=x;
        t=0;
        lag=0;

        while (*p)
            {
            if (l_virhe) return(-1);
            switch (*p)
                {
              case '+':
                if (len==0) { ++p; break; }
                if (len>0) opnd[t]=luku_var(sana,len); len=0;
                op[t]='+'; v[t++]=1;
                supista_var(&t,opnd,op,v);
                ++p;
                break;

              case '-':
                if (len==0) { sana[len++]=*p; ++p; break; }
                if (len>0) opnd[t]=luku_var(sana,len); len=0;
                op[t]='-'; v[t++]=1;
                supista_var(&t,opnd,op,v);
                ++p;
                break;

              case '*':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku_var(sana,len); len=0;
                op[t]='*'; v[t++]=2;
                supista_var(&t,opnd,op,v);
                ++p;
                break;

              case '/':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku_var(sana,len); len=0;
                op[t]='/'; v[t++]=2;
                supista_var(&t,opnd,op,v);
                ++p;
                break;

              case '^':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku_var(sana,len); len=0;
                op[t]='^'; v[t++]=3;
                supista_var(&t,opnd,op,v);
                ++p;
                break;

              case '[':
                q=strchr(p,']');
                if (len==0 || q==NULL) { syntax_error(lauseke); return(-1); }
                *q=EOS; /* lag=atoi(p+1); */
         /*     sdata2=sdata;    */
                laske_var(p+1,&dlag); lag=(int)dlag;
         /*     sdata=sdata2;    */
                p=q+1;
                break;

              case '(':
/* Rprintf("\nsana=%s|",sana); getch(); // ++++  */

                mat_element=0;
        if (strncmp(sana,"MAT_",4)==0) { mat_element=1; n_mat_par=0; }




                if (*sana=='p')                      /* 9.9.89 */
                    {
                    if (strncmp(sana,"pos",3)==0)
                        {
                        p+=pos_funktio(p,&opnd[t]);
                        if (l_virhe) return(-1);
                        len=-1;
                        break;
                        }
                    }
/* Rprintf("\np=%s|",p); getch(); // ++++  */
                q=p+1;
            if (*q==')') { sprintf(sbuf,"\nArguments missing in %s",lauseke);
                               sur_print(sbuf); l_virhe=1; return(-1); }
                n=1;
                narg=1;
                while (n)
                    {
                    ++p;
                    if (*p=='(') { ++n; continue; }
                    if (*p==')') { --n; continue; }
                if (*p==EOS) { sprintf(sbuf,"\n) is missing in %s",lauseke);
                                   sur_print(sbuf); l_virhe=1; return(-1); }
                    if (*p==',' && n==1)
                        {
                        *p=EOS;

                        if (mat_element) str_opnd[n_mat_par++]=q;
                        else laske_var(q,&opnd[t]);
                        ++t;
                        if (t>MAXARG+3)
                         { sprintf(sbuf,"\nToo many arguments in %s",lauseke);
                              sur_print(sbuf); l_virhe=1; return(-1); }
                        ++narg;
                        q=p+1;
                        }

                    }
                if(strchr("+-*/^)\0",*(p+1))==NULL) { syntax_error(lauseke);
                                                      return(-1); }
                *p=EOS; ++p;

                if (mat_element) str_opnd[n_mat_par++]=q;
                else              { i=laske_var(q,&opnd[t]);
                                    if (i<0 || l_virhe) return(-1);
                                  }


/*              i=laske_var(q,&opnd[t]);
//              if (i<0 || l_virhe) return(-1);
*/

                if (len==0) { len=-1; break; }
                sana[len]=EOS;

                if (narg>1)
                    {
/*
             Rprintf("\nArgumentit: ");
             for (i=t-narg+1; i<=t; ++i) Rprintf(" %g",opnd[i]); getch();
*/
                    t=t-narg+1;
                    if (*sana=='-')
                        opnd[t]=-mfunktio_var(sana+1,opnd+t,narg);
                    else
                        opnd[t]=mfunktio_var(sana,opnd+t,narg);
                    if (l_virhe) return(-1);
                    len=-1;
                    break;
                    }

                /* Yhden muuttujan funktiot */
                if (*sana=='-')
                    opnd[t]=-funktio_var(sana+1,opnd[t]);
                else
                    opnd[t]=funktio_var(sana,opnd[t]);
                if (l_virhe) return(-1);
                len=-1;
                break;

              case ')':
                sprintf(sbuf,"\n( missing in %s",lauseke);
                sur_print(sbuf); l_virhe=1; return(-1);

/*            case ':':
                sdata=atoi(sana+1);
                if (!sdata) { sana[len++]=*p; ++p; break; }
                len=0; ++p;
                break;
*/
              case 'e': case 'E':


                if (*sana==EOS) { sana[len++]=*p; ++p; continue; }
                                                         /* 28.2.2004 */
            if (len!=0) // RS ADD (from editor laske, otherwise VAR L=a / a=2*e+2 won't work)
            {

                if (strchr("+-.0123456789",sana[0])!=NULL)
                    {
                    sana[len++]=*p; ++p;
                    if (*p!='+' && *p!='-') break;
                    }
            }
              /* default seurattava suoraan case 'e':n jälkeen */
              default:
                /* tarkistukset puuttuvat */
                sana[len++]=*p;
                ++p;
                }
            }

        if (len<0) { v[t++]=0; }
        else
                   if (len>0) { opnd[t]=luku_var(sana,len); v[t++]=0; }

        supista_var(&t,opnd,op,v);
        *y=opnd[0];
/*   Rprintf("\n%s=%g",lauseke,*y);  // ++++  */
        return(1);
        }


static int if_syntax_error(char *x)
{
    sprintf(sbuf,"\nSyntax error in %s",x);
    sur_print(sbuf);
    WAIT;
    l_virhe=1;
    return(1);
}


/* Declarations */
static int strvert(char *a,char rel,char *b,char *c,char *dd,double *y);

static int varif_var(char *lauseke,double *y)
        {
        char *a,*b,*c,*d;
        char rel=0;
        char *p;
        int sulut;
        char x[LLENGTH];
        double y1;
        int tosi;

/*      Rprintf("\nvarif: %s",lauseke); getch();     */
        /* if(<a><rel><b>)then(<c>)else(<d>)
           <a>,<b>,<c>,<d> lausekkeita
           <rel>: =,>,<,<>,>=,<=
                  = > < E  S  P
        */

        strcpy(x,lauseke);
        a=x+3;  /* if( ohitetaan */
        p=a; sulut=0;
        while (*p)
            {
            switch(*p)
                {
              case '=':
                rel=*p; *p=EOS; break;
              case '<':

                if (*(p+1)=='=') { rel='P'; *p=EOS; ++p; *p=EOS; break; }
                if (*(p+1)=='>') { rel='E'; *p=EOS; ++p; *p=EOS; break; }
                rel=*p; *p=EOS; break;
              case '>':
                if (*(p+1)=='=') { rel='S'; *p=EOS; ++p; *p=EOS; break; }
                rel=*p; *p=EOS; break;
              case ')':
                --sulut; ++p;
                if (sulut<0)
                    {
                    sprintf(sbuf,"\nrelation symbol =<> missing! in %s",x);
                    sur_print(sbuf); WAIT; l_virhe=1; return(-1);
                    }
                break;
              case '(':
                ++sulut; ++p;
                break;
              default:
                ++p;
                }
            }

/*  Rprintf("\na=%s rel=%c",a,rel);  */
        b=p+1;
        p=b;
        while (1)
            {
            p=strchr(p,')');
            if (p==NULL) { if_syntax_error(lauseke); return(-1); }
            if (strncmp(p,")then(",6)==0) { *p=EOS; break; }
            ++p;
            }
/*  Rprintf(" b=%s",b);   getch();  */
        c=p+6;
        p=c; sulut=0;
        while (*p)
            {
            if (*p=='(') { ++sulut; ++p; continue; }
            if (*p==')')
                {
                if (!sulut) break;
                --sulut;
                }
            ++p;
            }
        if (*p==EOS) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;
        if (strncmp(p+1,"else(",5)!=0) { if_syntax_error(lauseke); return(-1); }
        d=p+6;
        p=d; sulut=0;
        while (*p)
            {
            if (*p=='(') { ++sulut; ++p; continue; }
            if (*p==')')
                {
                if (!sulut) break;
                --sulut;
                }
            ++p;
            }
        if (*p==EOS) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;
/* Rprintf(" c=%s d=%s",c,d);
getch();
*/
        if (strncmp(a,"str(",4)==0)             /* 13.3.1991 */
            {
            tosi=strvert(a,rel,b,c,d,y);
            if (tosi<0) return(-1);
            }

        else
            {
            laske_var(a,y);
            laske_var(b,&y1);
            tosi=0;
            switch (rel)
                {
              case '=': if (*y==y1) tosi=1; break;
              case '<': if (*y<y1) tosi=1; break;
              case '>': if (*y>y1) tosi=1; break;
              case 'E': if (*y!=y1) tosi=1; break;
              case 'P': if (*y<=y1) tosi=1; break;
              case 'S': if (*y>=y1) tosi=1; break;
                }
            }

        if (tosi) laske_var(c,y);
        else      laske_var(d,y);
        return(1);
        }

static int parsplit(char *x,char **a,char **b,int max)
{
    int i,sulut;
    char *p;
    char y[LLENGTH];

    strcpy(y,x);
    i=0;
    p=x;
    while (*p)
    {
        a[i]=p;
        while (*p)
        {
            if (*p=='(') break;
            if (*p==')')
            {
                if_syntax_error(y);
                return(-1);
            }
            ++p;
        }
        if (*p==EOS)
        {
            if_syntax_error(y);
            return(-1);
        }
        *p=EOS;
        b[i]=++p;
        sulut=1;
        while (*p)
        {
            if (*p==')')
            {
                --sulut;
                if (!sulut) break;
            }
            else if (*p=='(') ++sulut;
            ++p;
        }
        if (sulut)
        {
            if_syntax_error(y);
            return(-1);
        }
        *p=EOS;
        ++p;
        if (*p==EOS) break;
        ++i;
        if (i>=max)
        {
            if_syntax_error(y);
            return(-1);
        }
    }
    return(i+1);
}

static int arifor_var(char *lauseke,double *y)
        {
/*        int i,  */
        int g;
        char *sana[4],*laus[4];
        char x[LLENGTH];
        long ialku,iloppu,il,imax=0;
        double d;
        char *p;
        char sterm[LLENGTH];
        double term,sum;
        int iterm,iind,tind=0;
        char esana[7];
        int max;

// RS REM        extern int n_earg;

        strcpy(x,lauseke);
        g=parsplit(x,sana,laus,4);
        if (g<0) return(-1);
        if (g<3) { if_syntax_error(lauseke); return(-1); }
/*
   for (i=0; i<g; ++i) Rprintf("\nfor: %d %s %s",i,sana[i],laus[i]); getch();
*/
        p=strchr(laus[0],'=');
        if (p==NULL) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;   /* laus[0]='i'  */
        laske_var(p+1,&d); ialku=d;
        laske_var(laus[1],&d); iloppu=d;

        iterm=0;
        if (strcmp(sana[2],"term")==0)
            {
            iterm=1;
            p=strchr(laus[2],'=');
            if (p==NULL) term=0.0;
            else { *p=EOS; laske_var(p+1,&term); }  /* laus[2]='term' */
            }
        strcpy(sterm,laus[2+iterm]);
        iind=aseta_earg((double)ialku,esana);
        if (iind<0) return(-1);
        korvaa_var(sterm,laus[0],esana);
        if (iterm)
            {
            tind=aseta_earg(term,esana);
            if (iind<0) return(-1);
            korvaa_var(sterm,laus[2],esana);
            }

        max=0; p=sana[2+iterm];
        if (strncmp(p,"max",3)==0 || strncmp(p,"min",3)==0)
            {
            if (p[2]=='n') { max=3; sum=1e300; if (p[3]=='i') max=4; }
            else           { max=1; sum=-1e300; if (p[3]=='i') max=2; }
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
              if (iterm) { earg[tind]=term; if (il>ialku) laske_var(sterm,&term); }
                else laske_var(sterm,&term);
                if (max<3 && term>sum) { sum=term; imax=il; }
                if (max>2 && term<sum) { sum=term; imax=il; }  /* imax=imin */
                }
            }
        else if (strcmp(p,"sum")==0)
            {
            sum=0.0;
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm) { earg[tind]=term;
                               if (il>ialku) laske_var(sterm,&term); }
                else laske_var(sterm,&term);
                sum+=term;
                }
            }
        else if (strcmp(p,"product")==0)
            {
            sum=1.0;
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm) { earg[tind]=term;
                               if (il>ialku) laske_var(sterm,&term); }
                else laske_var(sterm,&term);
                sum*=term;
                }
            }
        else if (strcmp(p,"term")==0)
            {
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
              if (iterm) { earg[tind]=term; if (il>ialku) laske_var(sterm,&term); }
                else laske_var(sterm,&term);
                }
            sum=term;
            }
        else { if_syntax_error(lauseke); return(-1); }

        if (max==2) *y=(double)imax; else *y=sum;
        n_earg-=1+iterm;
        return(1);
        }

static void not_string(char *s)
        {
        sprintf(sbuf,"\n%s is not a string variable!",s);
        sur_print(sbuf); l_virhe=1; WAIT;
        }

static int sulku_split(char *x, char **osa, int n, int *pk)
        {
        char *p;
        int sulut;
        int h;

        p=x+1;
        osa[0]=p;
        h=1; *pk=1;
        sulut=0;
        while (1)
            {
            ++p; ++*pk;
            if (*p=='(') { ++sulut; continue; }
            if (*p==')')
                {
                --sulut; if (sulut<0) { *p=EOS; return(h); }
                }
            if (*p==',')
                {
                if (!sulut)
                    {
                    *p=EOS;
                    osa[h]=p+1;
                    ++h;
                    if (h>n) return(n);
                    }
                }
            }
        }


static int pos_funktio(char *s,double *y)      /* pos(var,char) tai pos(var,start_pos,char) */
        {
/*        char *p, */
        char *q;
        int var;
        char arvo[LLENGTH];
        double a;
        int start_pos;
        char x[LLENGTH],*osa[3];
        char x2[LLENGTH];
        int i,k,h;
/*        char ch; */

        strcpy(x,s);
        i=sulku_split(x,osa,3,&k);
        var=varfind(&d,osa[0]);
        if (var<0) return(-1);
        if (d.vartype[var][0]!='S')  { not_string(osa[0]); return(-1); }
        data_alpha_load(&d,jnro,var,arvo);
        h=d.varlen[var];
        arvo[h]=' '; arvo[h+1]=EOS;
        start_pos=1;
        if (i==3)
            {
            strcpy(x2,osa[1]);
            laske_var(x2,&a);
            start_pos=a;
            }

        strcpy(x2,osa[i-1]);
        if (muste_strcmpi(x2,"sp")==0 || muste_strcmpi(x2,"space")==0) { *x2=' '; x2[1]=EOS; }
        if (muste_strcmpi(x2,"comma")==0) { *x2=','; x2[1]=EOS; }

        q=strstr(arvo+start_pos-1,x2);
        if (q==NULL) *y=0.0;
        else *y=(double)(q-arvo+1);

        return(k+1);
        }


static int tutki_str_lauseke(char *x,int *pvar,int *plag,int *pstart,int *plen,int *pk)
        {
        int i; /* ,k; */
        char *osa[3];
        char x2[LLENGTH];
        double a;
        char *p,*q;

        i=sulku_split(x,osa,3,pk);
        *plag=0;
        p=strchr(osa[0],'[');
        if (p!=NULL)
            {
            *p=EOS; q=strchr(p+1,']');
            if (q==NULL)
                {
                sur_print("\n] missing!");
                WAIT; return(-1);
                }
            *q=EOS; *plag=atoi(p+1);
            }
        *pvar=varfind(&d,osa[0]);
        if (*pvar<0) return(-1);
        if (d.vartype[*pvar][0]!='S')  { not_string(osa[0]); return(-1); }
        *pstart=1;
        if (i>1)
            {
            strcpy(x2,osa[1]);
            laske_var(x2,&a);
            *pstart=(int)a;
            }
        *plen=d.varlen[*pvar]-*pstart+1;
        if (i==3)
            {
            strcpy(x2,osa[2]);
            laske_var(x2,&a);
            *plen=(int)a;
            if (*plen<0)
                {
                sur_print("\nNegative length in str()!");
                WAIT; return(-1);
                }
            }
        return(1);
        }


FILE *codes;

static int load_codes(char *codefile,unsigned char *code)
        {
        int i;
        char x[LLENGTH];

        strcpy(x,codefile);
        if (strchr(x,':')==NULL && *x!='.')
            { strcpy(x,survo_path);
              strcpy(x,"SYS/");  // RS CHA \\ -> /
              strcat(x,codefile);
            }

        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nCode conversion file %s not found!",x); sur_print(sbuf);
            WAIT; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose(codes);
        return(1);
        }

static void conv_var(unsigned char *sana)
        {
        int i;

        for (i=0; i<strlen((char *)sana); ++i) sana[i]=code[sana[i]];
        }

static int str_arvo(char *a,char *s)
        {
        char x[LLENGTH];
        int i,k;
        char *p; /*,*q; */

        if (*a=='"')
            {
            strcpy(s,a+1);
            p=strchr(s,'"');
            if (p!=NULL) *p=EOS;
            }
        else
            {
            if (muste_strnicmp(a,"str(",4)!=0) /* 4.3.1996 */
                {
                not_string(a);
                return(-1);
                }
            strcpy(x,a+3);
            i=tutki_str_lauseke(x,&str_var,&str_lag,&str_var_start,&str_var_len,&k);
            if (i<0) return(-1);
            data_alpha_load(&d,jnro+(long)str_lag,str_var,x);
            strncpy(s,x+str_var_start-1,(unsigned int)str_var_len); s[str_var_len]=EOS;
            }
        if (code_ind==2) conv_var((unsigned char *)s);
        return(1);
        }

static int strvert(char *a,char rel,char *b,char *c,char *dd,double *y)
        {
        int i,tosi;
        char s1[LLENGTH],s2[LLENGTH];
        int v;

        if (!code_ind)
            {
            code_ind=1;
            for (i=0; i<256; ++i) code[i]=(unsigned char)i;
            i=spfind("FILTER");
            if (i>=0) { code_ind=2; i=load_codes(spb[i],code); if (i<0) return(-1); }
            }
        i=str_arvo(a,s1); if (i<0) return(-1);
        i=str_arvo(b,s2); if (i<0) return(-1);

        tosi=0;
        v=strcmp(s1,s2);
        switch (rel)
            {
          case '=': if (!v) tosi=1; break;
          case '<': if (v<0) tosi=1; break;
          case '>': if (v>0) tosi=1; break;
          case 'E': if (v) tosi=1; break;
          case 'P': if (v<=0) tosi=1; break;
          case 'S': if (v>=0) tosi=1; break;
            }
        return(tosi);
        }

static int str_laske(char *lauseke)
        {
        char x[LLENGTH];
        char tulos[LLENGTH];
        char sana[LLENGTH];
        char sana2[LLENGTH];
        char *p,*q;
        int i,k;
        int var1,lag,start,len;

        strcpy(x,lauseke);
        p=x;
        *tulos=EOS;
        strcpy(sana,str_vasen);
        i=tutki_str_lauseke(sana,&str_var,&str_lag,&str_var_start,&str_var_len,&k);
        var[0]=str_var; /* lausekkeen merkintää varten! */
        if (i<0) return(-1);
        while (*p)
            {
            if (*p=='"')
                {
                q=p+1; while (*q && *q!='"') ++q;
                *q=EOS; strcpy(sana,p+1);
                p=q+1;
                }
            else if (muste_strnicmp(p,"str(",4)==0)
                {
                i=tutki_str_lauseke(p+3,&var1,&lag,&start,&len,&k);
                if (i<0) return(-1);
                p+=k+4;
                data_alpha_load(&d,jnro+(long)lag,var1,sana2);
                strncpy(sana,sana2+start-1,(unsigned int)len); sana[len]=EOS;
                }
            else if (muste_strnicmp(p,"comma",5)==0) /* 13.3.1991 */
                {
                strcpy(sana,",");
                p+=5;
                }
            else if (muste_strnicmp(p,"space",5)==0) /* 15.3.1991 */
                {
                strcpy(sana," ");
                p+=5;
                }
            else if (muste_strnicmp(p,"sp",2)==0)
                {
                strcpy(sana," ");
                p+=2;
                }
            else
                {
                sprintf(sbuf,"\nError in %s",lauseke);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (*p=='|')  /* 22.1.1996 */
                {
                ++p;
                i=strlen(sana)-1; while (i>=0 && sana[i]==' ') sana[i--]=EOS;
                strcat(tulos,sana);
                continue;
                }
            strcat(tulos,sana);
            if (*p=='&') { ++p; continue; }
            if (*p==EOS) break;
            sprintf(sbuf,"\n& or \" missing in %s",lauseke);
            sur_print(sbuf); WAIT; return(-1);
            }
        data_alpha_load(&d,jnro+(long)str_lag,str_var,sana);
        for (i=0; i<str_var_len && i<strlen(tulos); ++i)
            sana[i+str_var_start-1]=tulos[i];
            if (d.type==2) fi_alpha_save(&d.d2,jnro+(long)str_lag,str_var,sana);
            else if (d.type==1) ma_save(&d.d1,(int)(jnro+str_lag),str_var,sana);
            else
                {
                sur_print("\nCannot save data values!");
                WAIT; return(-1);
                }
        return(1);
        }



static int str_muuttuja(char *s)
        {
/*        int i,k;
        char x[LLENGTH],*osa[3]; */

        strcpy(str_vasen,s+3);
/*
        strcpy(x,s+3);
        i=tutki_str_lauseke(x,&str_var,&str_var_start,&str_var_len,&k);
*/
/*
printf("\nstr_var=%d start=%d len=%d k=%d",str_var,str_var_start,str_var_len,k);
getch();
*/
        return(1);
        }



static int muunto0()
        {
        int i,prind;  /* k */
        double y,s1,s2;

        nxx=nx2=0L; s1=s2=0.0;
        xx=(double *)muste_malloc((d.l2-d.l1+1L)*sizeof(double));
        oxx=(long *)muste_malloc((d.l2-d.l1+1L)*sizeof(double));
        if (xx==NULL || oxx==NULL)
            {
            sprintf(sbuf,"\nNot enough memory for %lu observations!",d.l2-d.l1+1L);
            sur_print(sbuf); WAIT; return(-1);
            }
        sur_print("\nReading original data values...");
        for (i=0; i<spn; ++i) spb2[i]=spb[i];
        prind=0;  /* 1; RS CHA Vaihdettu piirto oletusarvoisesti pois päältä */
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        for (jnro=d.l1; jnro<=d.l2; ++jnro)
            {
            if (unsuitable(&d,jnro)) continue;
            if (prind) { sprintf(sbuf," %ld",jnro); sur_print(sbuf); }
            for (i=0; i<spn; ++i) spb[i]=spb2[i];
            i=lue_arvot(jnro); if (i<0) return(-1);

            laske_var(lauseke,&y);
            if (l_virhe) { var_error(lauseke); return(-1); }
            oxx[nxx]=jnro; xx[nxx++]=y;
            if (y!=MISSING8)
                {
                s1+=y; s2+=y*y; ++nx2;
                }

            if (sur_kbhit()) { prind=1-prind; sur_getch(); }  // 12.7.2007
            }
        if (nx2<2) { sur_print("\nNot enough observations!"); WAIT; return(-1); }
        mean=s1/nx2; stddev=sqrt((s2-s1*s1/nx2)/(nx2-1.0));
        sum=s1;  /* 11.10.1996 */
        if ((muunnos==9 || muunnos==10) && sum==0.0)
            {
            sur_print("\nSum of data values is 0!. Cannot continue!");
            WAIT; l_virhe=1; return(-1); // RS CHA exit(1);
            }
        return(1);
        }

static int sort_data(int muunnos)
        {
        long i;
        unsigned long h,k,g;
        char ind;
        double y;
        double a;

        h=nxx;
        while (h>1L)
            {
            h/=2;
            while (1)
                {
                ind='1';
                for (k=0L; k<nxx-h; ++k)
                    {
                    if (xx[k]>xx[k+h])
                        {
                        y=xx[k]; xx[k]=xx[k+h]; xx[k+h]=y;
                        i=oxx[k]; oxx[k]=oxx[k+h]; oxx[k+h]=i;
                        ind='0';
                        }
                    }
                if (ind=='1') break;
                }
            }

        if (muunnos==5 || muunnos==6) return(1);
        k=0L;
        while (k<nxx)
            {
            y=xx[k]; h=1L;
            while (k+h<nxx && y==xx[k+h]) ++h;
            --h;
            if (muunnos==4) a=(double)(k+1);
            else a=(double)(k+1+h/2.0);
            for (g=k; g<=k+h; ++g) xx[g]=a;
            k+=h+1L;
            }
        return(1);
        }

/* 1=#RANK 2=#NORMAL 3=#STD 4=#NRANK 5=#TRUNCP 6=#WINSP 7=#TRUNCL 8=#WINSL */
/* 9=#PROPORTION 10=#PERCENT */
static int muunto2(int muunnos)
        {
        int i;
        long jxx,l=0;
/*        extern double inv_std(); */
/*        double a; */

        if (muunnos==1 || muunnos==2 || muunnos==4 || muunnos==5 || muunnos==6)
            {
            sort_data(muunnos);
            }
        if (muunnos==1 || muunnos==4) return(1);
        if (muunnos==2 || muunnos==3)
            {
            if (stddev<1e-15)
                { sprintf(sbuf,"\nVariance=0. Cannot continue!"); sur_print(sbuf); return(-1); }
            }
        if (muunnos==3)
            {
            for (jxx=0L; jxx<nxx; ++jxx)
                if (xx[jxx]!=MISSING8) xx[jxx]=(xx[jxx]-mean)/stddev;
            return(1);
            }
        if (muunnos==2)
            {
            for (jxx=0L; jxx<nxx; ++jxx)
/* RS CHA inv_std korvattu R:n qnorm -funktiolla
                if (xx[jxx]!=MISSING8) xx[jxx]=mean+stddev*inv_std((xx[jxx]-0.5)/nx2);
*/
                if (xx[jxx]!=MISSING8) xx[jxx]=mean+stddev*qnorm(((xx[jxx]-0.5)/nx2),0,1,1,0);
            return(1);
            }
        if (muunnos==5 || muunnos==6)
            {
            level=0.95;
            i=spfind("P"); if (i>=0) level=atof(spb[i]);
            l=0.5*(1.0-level)*nx2;
            if (l>nx2) l=nx2;
            }
        if (muunnos==5)
            {
            for (jxx=0L; jxx<l; ++jxx) xx[jxx]=xx[nx2-1-jxx]=MISSING8;
            return(1);
            }
        if (muunnos==6)
            {
            for (jxx=0L; jxx<l; ++jxx) { xx[jxx]=xx[l]; xx[nx2-1-jxx]=xx[nx2-1-l]; }
            return(1);
            }
        if (muunnos==7 || muunnos==8)
            {
            level=1.96;
            i=spfind("LEVEL"); if (i>=0) level=atof(spb[i]);
            }
        if (muunnos==7)
            {
            for (jxx=0L; jxx<nxx; ++jxx)
                if (xx[jxx]==MISSING8 || fabs(xx[jxx]-mean)>level*stddev) xx[jxx]=MISSING8;
            return(1);
            }
        if (muunnos==8)
            {
            for (jxx=0L; jxx<nxx; ++jxx)
                if (xx[jxx]!=MISSING8)
                    {
                    if (xx[jxx]-mean>level*stddev)
                        xx[jxx]=mean+level*stddev;
                    else if (xx[jxx]-mean<-level*stddev)
                        xx[jxx]=mean-level*stddev;
                    }
            return(1);
            }
        if (muunnos==9)
            {
            for (jxx=0L; jxx<nxx; ++jxx)
                if (xx[jxx]!=MISSING8) xx[jxx]/=sum;
            return(1);
            }
        if (muunnos==10)
            {
            for (jxx=0L; jxx<nxx; ++jxx)
                if (xx[jxx]!=MISSING8) xx[jxx]*=100.0/sum;
            return(1);
            }
        if (muunnos==11)
            {
            for (jxx=0L; jxx<nxx; ++jxx)
                if (xx[jxx]!=MISSING8) xx[jxx]-=mean;
            return(1);
            }
        return(1);
        }


static int muunto()
        {
        int i,k,prind;
        double y;

/*        sur_print("\n");  */
        for (i=0; i<spn; ++i) spb2[i]=spb[i];
/*
for (i=0; i<spn; ++i) Rprintf("\n%d %s=%s",i,spa[i],spb[i]); getch();
*/
        prind=0;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        for (jnro=d.l1; jnro<=d.l2; ++jnro)
            {
            if (unsuitable(&d,jnro))
                {
                if (first_new_var)    /* 29.3.92 */
                    {
                    if (nvar)
                        for (k=0; k<nvar; ++k)
                            {
                            if (var[k]<first_new_var) continue;
                            data_save(&d,jnro,var[k],MISSING8);
                            }
                    else
                        {
                        if (var[0]>=first_new_var)
                            data_save(&d,jnro,var[0],MISSING8);
                        }
                    }
                continue;
                }
            if (prind) { sprintf(sbuf," %ld",jnro); sur_print(sbuf);  muste_flushscreen(); } // RS ADD flushcreen
            for (i=0; i<spn; ++i) spb[i]=spb2[i];
            i=lue_arvot(jnro); if (i<0) return(-1);
            if (!nvar)
                {
                if (str_muunnos)
                    {
                    i=str_laske(lauseke);
                    if (i<0) return(-1);
                    }
                else
                    {
                    laske_var(lauseke,&y);
                    if (l_virhe) { var_error(lauseke); return(-1); }
                    i=data_save(&d,jnro,var[0],y); if (i<0) return(-1);
                    }
                }
            else
                {
                for (k=0; k<nvar; ++k)
                    {
                    int h;
                    char *pvar;

                    h=spfind(word[k+1]);
                    if (h<0)
                        {
                        sprintf(sbuf,"\nValue of %s not found!",word[k+1]);
                        sur_print(sbuf); WAIT;
                        poista_uudet_muuttujat();
                        return(-1);
                        }
/* Rprintf("\nspb[h]=%s|",spb[h]); getch(); // ++++  */
                    if (spb[h]==NULL) y=arvo[h];
                    else
                        {
                        pvar=spa[h]; spa[h]=NULL;
                        laske_var(spb[h],&y);
/* Rprintf("\ny=%g|",y); getch(); // ++++   */
                        if (l_virhe) { var_error(spb[h]); return(-1); }
                        spb[h]=NULL; arvo[h]=y;
                        spa[h]=pvar;
                        if (l_virhe) return(-1);
                        }
                    i=data_save(&d,jnro,var[k],y); if (i<0) return(-1);
                    }
                }
            if (sur_kbhit()) { prind=1-prind; sur_getch(); }
            }
        return(1);
        }


static int talletus()
        {
        int i,prind;  /* k
        double y; */
        long jxx;

        sur_print("\nSaving transformed values...");
/*      prind=1;  */
        prind=0;
        for (jxx=0L; jxx<nxx; ++jxx)
            {
            if (prind) { sprintf(sbuf," %ld",oxx[jxx]); sur_print(sbuf); }
            i=data_save(&d,oxx[jxx],var[0],xx[jxx]); if (i<0) return(-1);
            if (sur_kbhit()) { prind=1-prind; sur_getch(); }
            }
        return(1);
        }

static void uusi_nimi(int i, char *s)
        {
        char x[LLENGTH];
        int k,len,h;

        len=d.d2.l; if (len<10) return;
        fi_rewind(&(d.d2));
fi_gets(&(d.d2),x,d.d2.l,(long)(d.d2.var+(long)i*((long)len+(long)d.d2.extra)+(long)d.d2.extra));
        x[len]=EOS;

        k=8; while (x[k]==' ' && k<len) ++k;
        if (k==len || x[k]==EQ)
            {
            if (x[k]==EQ) for (h=k; h<len; ++h) x[h]=' ';
            x[9]=EQ;
            h=0; while (h<strlen(s) && h+10<len) { x[h+10]=s[h]; ++h; }
            fi_rewind(&(d.d2));
fi_puts(&(d.d2),x,d.d2.l,(long)(d.d2.var+(long)i*((long)len+(long)d.d2.extra)+(long)d.d2.extra));
            }
        }


static int uudet_nimet()
        {
        int i,k;

        if (d.type!=2) return(-1);
        if (!nvar)
            {
            uusi_nimi(var[0],lauseke);
            }
        else
            {
            for (k=0; k<nvar; ++k)
                {
                i=spfind(word[k+1]);
                if (i<0) return(-1);
                uusi_nimi(var[k],spb[i]);
                }
            }
        return(1);
        }

static int muuttujat()
        {
        char *p;

        p=strchr(word[1],'=');
        if (p==NULL)
            {
            while (nvar<g-1 && muste_strcmpi(word[1+nvar],"TO")!=0)
                {
                var[nvar]=varhaku(word[1+nvar]);
                if (var[nvar]<0) return(-1);
                ++nvar;
                }
            if (muste_strcmpi(word[1+nvar],"TO")==0 && ((1+nvar)==1 || (1+nvar)!=(g-2))) // RS ADD
            	{   
            	muste_fixme("\nFIXME: TO not allowed as a variable name in VAR"); // RS FIXME             	
            	sur_print("\nPlease do not use TO as a variable name! (TO:4 works)");
            	WAIT;
            	return(-1);
            	} // RS FIXME     
            return(1);
            }
        *p=EOS;

        if (muste_strnicmp(word[1],"str(",4)==0)
            {
            str_muunnos=1; str_muuttuja(word[1]);
            }
        else
            { var[0]=varhaku(word[1]); if (var[0]<0) return(-1); }
        strcpy(lauseke,p+1);
        nvar=0;
        return(1);
        }

/*
VAR <var>=#F(<expression>) TO <data>
0    1                     2  3
*/

static void op_var2()
        {
        int i,k;
        char *p,*q,*q2;
        char nimi[LLENGTH];
/*        char x[LLENGTH], *pdat[NDATA];  */
        char lauseke2[LLENGTH];

        edread(comline,r1+r-1);
        p=strchr(comline,STAMP); // RS CHA PREFIX -> STAMP
        if (p==NULL) p=comline;

        q2=strstr(p,"##"); if (q2!=NULL) if (q2[2]!=PREFIX) p=q2+1; // RS ADD

        g=splitp(p+1,word,MAXPARM);
        i=0;
        while (i<g && strcmp(word[i],"/")!=0) ++i;
        g=i;

        if (g<2 || strstr(word[1],"=#")==NULL)
            {
            sur_print("\nUsage:");
            sur_print("\nVAR <var>=#F(<expression>) TO <data>");
            sur_print("\nwhere #F is #RANK,#NORMAL or #STD");
            WAIT; return;
            }

// RS REM already in op_var        
//        if (muste_strcmpi(word[g-2],"TO")==0) strcpy(nimi,word[g-1]);
//        else  strcpy(nimi,active_data);

        i=data_open2(nimi,&d,1,0,0); if (i<0) return;

        p=strchr(word[1],'=');
        if (p==NULL) return; /* mahdotonta */
        *p=EOS; ++p; strcpy(lauseke2,p);

        var[0]=varhaku(word[1]); nvar=0;  /* muuttujat() poistettu */
        if (var[0]<0) return;
        q=strchr(p,'(');
        if (q==NULL)
            { sprintf(sbuf,"\n( missing in %s",p); sur_print(sbuf); WAIT; return; }
        *q=EOS;
        if (muste_strcmpi(p,"#RANK")==0) muunnos=1;
        else if (muste_strcmpi(p,"#NORMAL")==0) muunnos=2;
        else if (muste_strcmpi(p,"#STD")==0) muunnos=3;
        else if (muste_strcmpi(p,"#NRANK")==0) muunnos=4;
        else if (muste_strcmpi(p,"#TRUNCP")==0) muunnos=5;
        else if (muste_strcmpi(p,"#WINSP")==0) muunnos=6;
        else if (muste_strcmpi(p,"#TRUNCL")==0) muunnos=7;
        else if (muste_strcmpi(p,"#WINSL")==0) muunnos=8;
        else if (muste_strcmpi(p,"#PROPORTION")==0) muunnos=9; /* 11.10.1996 */
        else if (muste_strcmpi(p,"#PERCENT")==0) muunnos=10;
        else if (muste_strcmpi(p,"#CENTER")==0) muunnos=11;
        else
            { sprintf(sbuf,"\nUnknown transformation %s",p); sur_print(sbuf); WAIT; return; }
        ++q;
        i=strlen(q);
        if (q[i-1]!=')')
            { sur_print("\n( missing!"); WAIT; return; }
        q[i-1]=EOS;
        strcpy(lauseke,q);
        mask(&d);
        poista_var();
        vm_act=d.m_act;

        i=sp_init_var(r1+r-1,d.m_act); if (i<0) { spec_error(); return; }
        i=sp2_init(); if (i<0) { spec_error(); return; } /* MISSING,ORDER,N */

// Rprintf("\nvarspec:"); for (i=0; i<spn; ++i) Rprintf("\n%d:%s",i,spa[i]);

        i=conditions(&d); if (i<0) return;

        ndata=0;
        i=spfind("INDATA");
        if (i>=0)
            {
            strcpy(sdat_list,spb[i]);
            ndata=split(sdat_list,sdat,NDATA);
            for (i=0; i<ndata; ++i)
                {
                k=data_open(sdat[i],&sd[i]); if (k<0) return;
                }
            }

        i=muunto0(); if (i<0) return;
        i=muunto2(muunnos); if (i<0) return;
        for (i=0; i<spn; ++i) spb[i]=spb2[i];
        strcpy(lauseke,lauseke2); uudet_nimet();
        talletus();
        data_close(&d);
        for (i=0; i<ndata; ++i) data_close(&sd[i]);
        }

static int varaa_earg()
        {
        int i;

        earg=(double *)muste_malloc(MAXEARG*sizeof(double));
        if (earg==NULL)
            {
            sur_print("\nNot enough memory! (MAXEARG=255)");
            l_virhe=1;
            WAIT; return(-1);
            }
 for (i=0; i<MAXEARG; ++i) earg[i]=0.0;
        earg_varattu=1;
        return(1);
        }


/*
VAR <var>=<expression> TO <data>
0    1                 2   3
*/

/* int main(int argc, char *argv[]) */
//static 
int muste_var(char *argv)
        {
        int i,k;
        char *p,*q2;
        char nimi[LLENGTH];
        char x[LLENGTH]; /*, *pdat[NDATA]; */

/* RS Modulikohtaisten globaalien muuttujien alustus */
    earg_varattu=0;
    n_earg=0;
    l_virhe=0;
    nvar=0;
    jnro=0;
    lag=0;
    vm_act=0;
    ndata=0;
    muunnos=0;
    nxx=0;
    nx2=0;
    mean=0;
    stddev=0;
    sum=0;
    str_muunnos=0;
    first_new_var=0;
    spn_order=0;
    level=0;
    str_var=0;
    str_lag=0;
    str_var_start=0;
    str_var_len=0;
    code_ind=0;
    nmat_var=0;


/*  RS REM      if (argc==1) return(1); */
        s_init(argv);

/* RS CHA
        s_init(argv[1]);
*/

        if (muste_strcmpi(word[g-2],"TO")==0) strcpy(nimi,word[g-1]);
        else  strcpy(nimi,active_data);
        subst_survo_path(nimi); /* 20.10.2001 */        

        edread(comline,r1+r-1);
// Rprintf("\ncomline:%s",comline);
        p=strchr(comline,STAMP); // RS CHA PREFIX -> STAMP
        if (p==NULL) p=comline;

        q2=strstr(p,"##"); if (q2!=NULL) if (q2[2]!=PREFIX) p=q2+1; // RS ADD
//Rprintf("\ncomline2:%s",p);

        g=splitp(p+1,word,MAXPARM);
        i=0;
        while (i<g && strcmp(word[i],"/")!=0) ++i;
        g=i;

        if (g<2)
            {
            sur_print("\nUsage:");
            sur_print("\nVAR <var>=<expression> TO <data>");
            sur_print("\nor");
            sur_print("\nVAR <list_of_variables> TO <data>");
            WAIT; return(1);
            }

         spec_init(r1+r-1); // RS FIX initialization for specifications!
// Rprintf("\nword1:%s",word[1]);
        if (strstr(word[1],"=#")!=NULL)
            {
            op_var2(); s_end(argv);  /*[1]); */
            return(1);
            }

        spec_rnd(); // 26.7.2011/SM

        i=data_open2(nimi,&d,1,0,0); if (i<0) { s_end(argv); /*[1]);*/ return(1); }
        i=muuttujat(); if (i<0) { s_end(argv); /*[1]);*/ return(1); }
        mask(&d); // RS Uses spfind(), needs initialization above
        poista_var();
        vm_act=d.m_act;
        for (i=0; i<d.m_act; ++i)
            {
            strcpy(x,d.varname[d.v[i]]);
            if (strncmp(x,"IND ",4)==0 || strncmp(x,"CASES ",6)==0 || strncmp(x,"SELECT ",7)==0)
                {
                sur_print("\nField names IND, CASES and SELECT are not allowed!");
                sur_print("\nChange such a name by FILE STATUS and FILE UPDATE.");
                WAIT; return(1);
                }
            }

        i=sp_init_var(r1+r-1,d.m_act); if (i<0) { spec_error(); return(1); }
        i=sp2_init(); if (i<0) { spec_error(); return(1); } /* MISSING,ORDER,N */

// Rprintf("\nvarspec:"); for (i=0; i<spn; ++i) Rprintf("\n%d:%s",i,spa[i]);

/*
printf("\nspec:");
for (i=0; i<spn; ++i) Rprintf("\n%s",spa[i]); getch();
*/
        i=conditions(&d); if (i<0) return(1);

        ndata=0;
        i=spfind("INDATA");
        if (i>=0)
            {
            strcpy(sdat_list,spb[i]);
            ndata=split(sdat_list,sdat,NDATA);
            for (i=0; i<ndata; ++i)
                {
/*              k=data_open(sdat[i],&sd[i]); if (k<0) return;              */
                k=data_read_open(sdat[i],&sd[i]); if (k<0) { s_end(argv); /*[1]);*/ return(1); }
                }
            }
        varaa_earg();   /* 4.11.1998 */
        muunto();
        for (i=0; i<spn; ++i) spb[i]=spb2[i];
        uudet_nimet();
        data_close(&d);
        for (i=0; i<ndata; ++i) data_close(&sd[i]);
        outseed(); /* sur_rand() */
/*********************
printf("\nspn=%d|",spn); getch();  // ++++
for (i=0; i<spn; ++i) Rprintf("\n%s %g",spa[i],arvo[i]); getch();
**************************/
        s_end(argv); /* [1]); */
        return (1);
        }

/*
int muste_var(char *argv)
	{
	int i,j;

	s_init(argv);    
    i=g-1;
    while(i>1)
    	{
		if (muste_strcmpi(word[i],"TO")==0) break;
		i--;
    	}
  
	Rprintf("\ng: %d, i: %d",g,i);
//	for (j=0; j<	
	i=muste_var_main(argv);
	return(i);
	}
*/	
