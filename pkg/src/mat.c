/*  !mat.c 21.9.1985/SM (6.11.1992) (24.11.1995)
.............................
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "survo.h"
#include "survolib.h"
#include "survoext.h"

#define TESTAUS 0

#define ERC 128
#define MAXTILA 65500L
#define MAXCOL 500   // oli 64 -17.3.2004
#define MAXRLAB 8
#define MAXCLAB 8
#define MAXPITUUS 100
#define MAXARG 1
#define MAXMTX  10
#define NMAT 5
#define EQUALS "~"
#define DIM_HUGE 1000000000

#define MAX_PAIRS 200
#define MAX_POWER 4
#define MAX_FLEVELS 100

#define MAXN 10000
#define C_ZERO 1e-100

struct complex
{
   double x;  /*real part*/
   double y;  /*imag part*/
};


struct polynom
    {
    int n;
    struct complex a[MAXN];
    } ;

// RS REM static int n;       /* tilapÑisten tilojen %i% lkm. */
static int n_mat;   /* MAT-komentojen lkm. */
static int *level;
static int *power;
static int *scalar;
static int pos1[MAX_POWER][MAX_PAIRS];
/* static int len1[MAX_POWER][MAX_PAIRS]; poistettu 12.1.1999 */
static int len2[MAX_POWER][MAX_PAIRS];
static int n_pairs[MAX_POWER];
static int func_level[MAX_FLEVELS];
static int min_len[MAX_POWER];
static int i_min[MAX_POWER];
static int max_index;
static char *expr_space;
static int expr_size;
static int n_scalars;
static int scalar_expr[100];
static char x0[LLENGTH];
static int save_all_temp_mat;
static FILE *survomat;


extern double *arvo;
extern int spn;

static FILE *MAT;
static FILE *rowcomments=NULL; // 29.1.2005
static FILE *mtx_file;

static char mtx_info[LLENGTH];
static char *pmtx[MAXMTX];
static int mtx,nmtx;


static char op;
static char opnd1[LNAME], opnd2[LNAME];
static char tulos[LNAME];
static char tnimi[3*ERC];

static double *X;
static char *rlabX, *clabX, exprX[LLENGTH];
static int  mX, nX, typeX, lrX, lcX;
static double *Y;
static char *rlabY, *clabY, exprY[LLENGTH];
static int  mY, nY, typeY, lrY, lcY;
static double *T;
static char *rlabT, *clabT, exprT[LLENGTH];
static int  mT, nT, typeT, lrT, lcT;
/* Z ei suotava tulosmatriisin nimenÑ, koska z on toimituskenttÑ,
   eikÑ LINKER ilman /NOIGNORECASE erota kirjainkokoja
 */

static char mat_argv1[16];
static int mat_argc; // RS ADD

static int sp_read=0;

static char comline2[LLENGTH];

static int n_mat; /* # of commands in combined MAT */
static char *expr_space; // RS extern -> static
static int save_all_temp_mat; // RS extern -> static

static int n_row_comments; // 29.1.2005
static char *row_comments;
static char *p_com;
static long len_row_comments=0L;

static int l_virhe=0;

static char *str_opnd[MAXARG+4];
static int mat_element;
static int n_mat_par;

static double *mat[NMAT];
static char *rlab[NMAT],*clab[NMAT];
static int lr[NMAT],lc[NMAT];
static int m[NMAT],n[NMAT];
static int nmat=0;
static char mat_name[NMAT][9];

static double *pow1,*pow2;
static double *d,*e;

static int *xi;

static char *argv1;
static int *freq;

static char expr[3*LLENGTH];
static int line_nr;

static char *nimet[]={ "TRANSFORM", "MULT", "SAMPLES", "INDVAR", "MERGE", "MINDIFF",
                "COLSORT", "CRSORT", "EIGEN", "CONVOLUTION",
                "MAXDET", "U_TO_F", "INTSCAL", "FRAC_TO_DEC", "SAMPLE",
                "SORT", "MAGIC", "JACKKNIFE", "#EIGFEW",  "#EIGLAN",
                "AGGRE", "PERMORD", "FREQ", "RCSORT", "PRODDIAG",
                "TAB", "SMOOTH",
                "#" };

static char **specs=nimet;



static int laske(); // RS Declaration
static int matload();

static int vapauta()
        {
muste_fixme("FIXME: mat, function vapauta() not freeing memory!");       
        return(1);
        if (X!=NULL) free(X);
        if (rlabX!=NULL) free(rlabX); if(clabX!=NULL) free(clabX);
        if (Y!=NULL) free(Y);
        if (rlabY!=NULL) free(rlabY); if(clabY!=NULL) free(clabY);
        if (T!=NULL) free(T);
        if (rlabT!=NULL) free(rlabT); if(clabT!=NULL) free(clabT);
        return(1);
        }


static int syntax_error2(char *x)
        {
        sur_print("\n");
        sur_print(x); WAIT; return(1);
        }

static int not_enough_space()
        {
        sur_print("\nNot enough space!"); WAIT; return(1);
        }

static void err_mat_parser()
        {
        sprintf(sbuf,"\nValue of mat_parser=%d (in SURVO.APU) too small!",
                                 mat_parser);
        sur_print(sbuf); WAIT;
        }


static int mat_error()
    {
    muste_fixme("\nFIXME: mat_error, close files, free memory"); // RS FIXME    
    sur_print("\nError in a MAT operation!");
    WAIT; // RS REM exit(0);
    return(1);
    }

static int dim_error()
        {
// RS REM        extern char mat_argv1[];

        PR_EBLD;
        sur_print("\nincompatible dimensions in matrix operands!");
        if (etu==2)
             {
    muste_fixme("\nFIXME: mat_error, dim_error, close files, free memory"); // RS FIXME    
             
             strcpy(tut_info,"˛˛˛@99@MAT@Incompatible dimensions!@");
             erun=0;
             s_end(mat_argv1);
             l_virhe=1; return(-1); // RS CHA exit(1) -> l_virhe=1; return(-1);
             }

        WAIT; PR_ENRM; erun=0;
        return(1);
        }


static int ei_sallittu()
        {
        muste_fixme("\nFIXME: mat, ei_sallittu, close files, free memory"); // RS FIXME            
        sur_print("\nIncorrect _MAT operation!"); WAIT;
        return(-1); // RS CHA exit(1);
        }

static char *next_mat_command(char *p,char *x) /* from expr_space */
        {
        char *q;
        muste_fixme("\nFIXME: next_mat_command, check for \\r"); // RS FIXME
        q=x; while (*p!='\n') *q++=*p++; // RS FIXME check for \r
        *q=EOS; ++p;
        return(p);
        }

static int ei_tilaa()
        {
        PR_EBLD;
        sur_print("\nNot enough space for matrices");
        if (etu==2) { strcpy(tut_info,"MATerr@"); return(-1); } /* 20.4.1997 */
        WAIT; PR_ENRM; erun=0; mtx=0;
        return(1);
        }
        
static int not_enough_memory() // RS ADD
	{
	ei_tilaa(); return(-1);
	}

static int varaa_tila(
double **A,  /* matriisitila (alkuosoite) (malloc) */
int m,      /* rivien lkm */
int n,      /* sar. lkm   */
char **rlab, /* rivien otsikot (malloc) */
char **clab, /* sar. otsikot   (malloc) */
int mcr,    /* riviotsikoiden pituus */
int mcl    /* sarakeotsikoiden pituus */
)
        {
/*
printf("varaa_tila\n"); sur_getch();
if (*A==NULL) printf("NULL\n"); else printf("EI_NULL\n"); sur_getch();
*/
        if (*A!=NULL) free(*A);
/*      if ( (long)m*n*sizeof(double)>MAXTILA )
                { ei_tilaa(); return(-1); }
*/
/*
printf("tila=%ld\n",m*n*sizeof(double)); sur_getch();
*/
        *A=(double *)malloc(m*n*sizeof(double));
/*
printf("a\n"); sur_getch();
*/
        if (*A==NULL) { ei_tilaa(); return(-1); }
                               /*  printf("\nmat-tila varattu! %d",m*n); */
        if (rlab!=NULL)
            {
            if (*rlab!=NULL) free(*rlab);
            *rlab=(char *)malloc(m*mcr+1);
            if (*rlab==NULL) { ei_tilaa(); return(-1); }
                               /* printf("\nrlab-tila varattu! %d %d",m,mcr); */
            }
        if (clab!=NULL)
            {
            if (*clab!=NULL) free(*clab);
            *clab=(char *)malloc(n*mcl+1);
            if (*clab==NULL) { ei_tilaa(); return(-1); }
                               /* printf("\nclab-tila varattu! %d %d",n,mcl); */
            }
/* printf("\nAosoite=%lu",*A);
   printf("\n&viim.matriisialkio=%lu",&((*A)[m*n-1]));
   printf("\nclabosoite=%lu",*clab);
   sur_getch();
*/
        return(1);
        }

static int mat_spec_read(int j)
    {
    int i;
// RS REM    extern int sp_read;

    i=sp_init(j); sp_read=1;
    if (i<0)
        {
        sur_print("\nToo many specifications!");
        if (etu==2) { strcpy(tut_info,"MATerr@"); exit(1); }
        mtx=erun=0;
        WAIT;
        return(-1); // RS CHA exit(1);
        }
    return(1);
    }

static int syntax_error(char *s)
        {
        sprintf(sbuf,"\nsyntax error in %s",s); sur_print(sbuf);
        WAIT;
        l_virhe=1;
        return(1);
        }

static int f_tuntematon(char *s)
        {
        sprintf(sbuf,"\nUnknown function %s",s); sur_print(sbuf);
        WAIT;
        l_virhe=1;
        return(1);
        }

static int arg_virhe(char *s)
        {
        sprintf(sbuf,"\n%s: Error in arguments",s); sur_print(sbuf);
        WAIT;
        l_virhe=1;
        return(1);
        }

static void mat_parser_zero()
        {
        if (mat_parser) return;
        sur_print("\nSet mat_parser=4000 (for example) in SURVO.APU or");
        }

static int kirjoita_ilmoitus(char *teksti)
        {
        char x[LLENGTH];
        char *p;
        int i;

        edread(x,r1+r-1);
        p=strstr(x+1," / ");
        if (p!=NULL) i=p-x;
        else
            {
            i=strlen(x);
            while (x[i-1]==' ') --i;
            i+=2;
            }
        edwrite(space,r1+r-1,i);
        sprintf(x,"/ *%s",teksti);
        edwrite(x,r1+r-1,i);
        return(1);
        }


static int next_row_com(char *label,char *text)
    {
    int i;
    char x[129];
    char *p;

    fgets(x,128,rowcomments);
// printf("\nx=%s|",x); sur_getch();
    p=strchr(x,':'); *p=EOS;  // ei virhetarkistusta!!!!!!!!!
    strcpy(label,x);
    i=strlen(label); if (i<8) strncat(label,space,8-i);
    strcpy(text,p+2);

    return(1);
    }

static int fnconv2(double a,int acc,char *s)
    {
    char t[40];

    fnconv(a,acc,s);
//  *s=EOS;

//  if (a<0.0) { strcpy(s,"-"); --acc; a=-a; }
    sprintf(t,"%*.*e",acc,acc,a);
sprintf(sbuf,"\nt=%s| acc=%d a=%g|",t,acc,a); sur_print(sbuf); WAIT; // RS CHA


    return(1);
    }


static int matprint(
double *A,          /* tulostettava matriisi */
int m,
int n,            /* dimensiot */
int m1,
int m2,          /* tulostettavat rivit */
int n1,
int n2,          /* tulostettavat sarakkeet */
char *rlab,
char *clab,    /* otsikkovektorit */
int mrl,
int mcl,        /* otsikkonimien pituudet */
char *muoto,        /* tulostusmuoto */
char *tunnus,       /* otsikkorivien eteen tuleva tunnus esim. /// */
int eol,            /* ensimm.tulostusrivi */
int n_rowrem       // rivikommenttien lkm 29.1.2005
)
        {
        int h,i,j,k,sar;
        char x[LLENGTH];
        char sana[LLENGTH];
        char *p;
        int i_com;
        char com_label[128]; // -27.4.2005 only [9]
        char com_text[128];
        char row_label[128]; // -27.4.2005 only [9]

        if (n_rowrem)
            {
            sprintf(sbuf,"%sROWCOMM.TMP",etmpd);
            rowcomments=fopen(sbuf,"rt");
            next_row_com(com_label,com_text);
            i_com=0;
            }
        output_open(eout);
        if (*muoto=='%') sar=sprintf(x,muoto,fabs(A[0]))+2;
        else sar=strlen(muoto)+1;

        if (*muoto=='*') sar=accuracy+2; // 13.4.2005

        k=sprintf(x,"%-*.*s",mrl,mrl,tunnus);
        for (j=n1-1; j<n2; ++j)
            {
            strncpy(sana,clab+j*mcl,mcl);
            i=mcl-1;
            while (sana[i]==' ' && i>0) --i;
            sana[i+1]=EOS;
            p=sana;
            while (*p==' ') ++p;
            if (k+sar<c2)
                k+=sprintf(x+k,"%*.*s",sar,sar-1,p);
            }
        k+=sprintf(x+k,"%c",EOS);
        output_line(x,eout,eol); if (eol) ++eol;

        for (i=m1-1; i<m2; ++i)
            {
            strncpy(sana,rlab+i*mrl,mrl); sana[mrl]=EOS;
            strcpy(row_label,sana);
// printf("\nsana=%s com_label=%s text=%s|",sana,com_label,com_text); sur_getch();
            k=sprintf(x,"%-*.*s",mrl,mrl,sana);
            for (j=n1-1; j<n2; ++j)
                {
                if (k+sar<c2)
                    {
                    if (*muoto=='*')
                        { fnconv2(A[i+m*j],accuracy+2,sana); h=1; }
                    else
                        h=fconv(A[i+m*j],muoto,sana);
                    if (h<0)
                        {
                        PR_EBLD;
               sprintf(sbuf,"\nFormat %s not wide enough for %f",muoto,A[i+m*j]);
                        sur_print(sbuf); WAIT; PR_ENRM; output_close(eout); return(-1);
                        }
                    k+=sprintf(x+k,"%*.*s",sar,sar-1,sana);
                    }
                }
            if (n_rowrem) // 29.1.2005
                {
                if (strcmp(row_label,com_label)==0)
                    {
                    k+=sprintf(x+k," / %s",com_text);
                    ++i_com;
                    if (i_com<n_rowrem) next_row_com(com_label,com_text);
                    }
                }
            output_line(x,eout,eol); if (eol) ++eol;
            }
        output_close(eout);
        return(1);
        }





static double oper(double x1,double x2,char laji)
        {
// RS REM        extern double power();

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
            return(muste_pow(x1,x2));
            }
        return(0.0); /* ??? */
        }

static int lab_find(char *x,char *lab,int m,int len)
        {
        char s[32];
        int i;

        strcpy(s,x);
        for (i=strlen(s); i<len; ++i) s[i]=' ';
        for (i=0; i<m; ++i)
            if (strncmp(s,lab+i*len,len)==0) break;
        if (i==m) return(-1);
        return(i+1);
        }


static void mat_function(char *f,char **s,int nn,double *yy)
        {
        int i,j,k;
        double xx[2];
        char *lab;
        int type;

// printf("f=%s nn=%d %s %s\n",f,nn,s[0],s[1]); sur_getch();

        for (k=0; k<nmat; ++k)
            {
            if (strcmp(f,mat_name[k])==0) break;
            }
        if (nmat==0 || k==nmat)
            {

            if (nmat==NMAT) nmat=0; /* kiertokulku */
/*
                {
                sprintf(sbuf,"Too many matrices (more than %d)!",NMAT);
                sur_print(sbuf); WAIT; l_virhe=1; nmat=0; return;
                }
*/
   matload(f,&mat[k],&m[k],&n[k],&rlab[k],&clab[k],&lr[k],&lc[k],&type,sbuf);
            strcpy(mat_name[k],f);
            ++nmat;

            }
        if (nn==1 && m[k]==1) { nn=2; s[1]=s[0]; s[0]="1"; }
        i=lab_find(s[0],rlab[k],m[k],lr[k]);
        if (i>0) xx[0]=i; else laske(s[0],&xx[0]);
        if (nn>1)
            {
            i=lab_find(s[1],clab[k],n[k],lc[k]);
            if (i>0) xx[1]=i; else laske(s[1],&xx[1]);
            }

        i=xx[0]; if (nn>1) j=xx[1];
        if (i<1 || i>m[k] || (nn>1 && (j<1 || j>n[k])) )
            {
            sur_print("\nError in matrix index!"); WAIT;
            l_virhe=1;
            return;
            }
        if (nn==1)
            *yy=mat[k][i-1];
        else
            *yy=mat[k][i-1+m[k]*(j-1)];
        }

        
static double funktio(char *s,double x)
        {
        int i;
        double y;
        char S[32]; 
        
        if (*s==EOS) return(x);
        strncpy(S,s,31); S[31]=EOS; muste_strupr(S);
        if (strncmp(S,"SQR",3)==0) return(sqrt(x));
        if (strcmp(S,"LOG")==0) return(log(x));
        if (strcmp(S,"EXP")==0) return(exp(x));
        if (strcmp(S,"SIN")==0) return(sin(x));
        if (strcmp(S,"COS")==0) return(cos(x));
        if (strcmp(S,"TAN")==0) return(tan(x));
        if (strcmp(S,"ATN")==0 || strcmp(S,"ARCTAN")==0) return(atan(x));
        if (strcmp(S,"ABS")==0) return(fabs(x));
        if (strcmp(S,"INT")==0) return(floor(x));
        if (strcmp(S,"ROUND")==0) return(round(x));
        if (*s=='M' && strncmp(s,"MAT_",4)==0)
             {
             mat_function(s+4,str_opnd,1,&y); return(y); // 16.8.2006
             }

/*      i=f_tiedosto(s,&x,1,&y); if (i>0) return(y);
*/
        f_tuntematon(s);
        l_virhe=1;
        return(x);
        }

static double mfunktio(char *s,double *x,int n)
        {
        int i;
        double y;
        char S[32];

/*     printf("\nmfunktio: ");
     for (i=0; i<n; ++i) printf("%g ",x[i]); sur_getch();
*/
        strncpy(S,s,31); S[31]=EOS; muste_strupr(S);

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
        if (strcmp(S,"GCD")==0)
            {
//            extern double gcd();

            return (gcd(x[0],x[1]));
            }
        if (strcmp(S,"MOD")==0)
            {
            return((double)((unsigned long)x[0]%(unsigned long)x[1]));
            }
        if (strcmp(S,"ROOT")==0)   /* 9.12.89 */
            {
//            extern double root();
            return (root(x[0],x[1]));
            }

        if (*s=='M' && strncmp(s,"MAT_",4)==0)
            {
            mat_function(s+4,str_opnd,n,&y);
            return(y);
            }

//      i=f_edit(s,x,n,&y); if (i>0) return(y);
//      i=f_tiedosto(s,x,n,&y);
//      if (i>0 && y!=MISSING8) return(y);

        l_virhe=1;
        return(x[0]);
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

static int laske2(char *muuttuja,double *y)
        {
        int i,k;
// RS REM        extern int sp_read;

        if (!sp_read) 
           { 
             i=mat_spec_read(r1+r-1);
             if (i<0) { l_virhe=1; return(-1); } // RS ADD exit handling
           }  
/***************************************
            {
            i=sp_init(r1+r-1); sp_read=1;
            if (i<0)
                {
                sur_print("\nToo many specifications!");
                if (etu==2) { strcpy(tut_info,"MATerr@"); exit(1); }
                mtx=erun=0;
                WAIT;
                exit(1);
                }
            }
*****************************************/
        i=spfind(muuttuja);
        if (i<0)
            {
            if (etu==2) { strcpy(tut_info,"MATerr@"); l_virhe=1; return(-1); } // RS CHA exit handling /* 21.4.1997 */
            mat_parser_zero();
            sprintf(sbuf,"\nScalar %s not found!",muuttuja);
            mtx=erun=0;
            sur_print(sbuf); WAIT;
            l_virhe=1; return(-1); // RS CHA exit handling exit(1);
            }
        if (spb[i]==NULL) { *y=arvo[i]; return(1); }
        k=laske(spb[i],y);
        arvo[i]=*y;
        spb[i]=NULL;
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
        


static int laske(char *lauseke,double *y)
        {
//        extern double luku();
//        extern double oper();
//        extern double funktio();
//        double mfunktio();

        char x[MAXPITUUS];
        char *p,*q;
        char sana[32];
        int len;
        double opnd[MAXARG+4]; char op[MAXARG+4]; int v[MAXARG+4];
        int t,n;
        int narg; /* Usean muuttujan funktion argumenttien lkm     */
        int i;

        if (*lauseke=='-')  /* 8.12.89 arit, tÑÑllÑ vasta 25.8.1998 */
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
                mat_element=0;
                if (strncmp(sana,"MAT_",4)==0) { mat_element=1; n_mat_par=0; }
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
                        else laske(q,&opnd[t]);
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
/*   printf("\nq=%s",q); sur_getch();   */
                if (mat_element) str_opnd[n_mat_par++]=q;
                else
                    {
                    i=laske(q,&opnd[t]);
                    if (i<0 || l_virhe) return(-1);
                    }
/*   printf("\ntulos1=%f",opnd[t]); sur_getch();  */
                if (len==0) { len=-1; break; }
                sana[len]=EOS;

                if (narg>1)
                    {
/*
             printf("\nArgumentit: ");
             for (i=t-narg+1; i<=t; ++i) printf(" %g",opnd[i]); sur_getch();
*/
                    t=t-narg+1;
                    if (*sana=='-')
                        opnd[t]=-mfunktio(sana+1,opnd+t,narg);
                    else
                        opnd[t]=mfunktio(sana,opnd+t,narg);
                    if (l_virhe) return(-1);
                    *sana=EOS;
                    len=-1;
                    break;
                    }

                /* Yhden muuttujan funktiot */
                if (*sana=='-')
                    opnd[t]=-funktio(sana+1,opnd[t]);
                else
                    opnd[t]=funktio(sana,opnd[t]);
                if (l_virhe) return(-1);
                len=-1;
                break;

              case ')':
                mat_parser_zero();
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

static double arit_atof(char *lauseke)
        {
        double y;

        laske(lauseke,&y);

        return(y);
        }

static int arit_atoi(char *lauseke)
        {
        return((int)arit_atof(lauseke));
        }

static int name(char *matfile,char *matr)
        {
        int i;

        if ( (*matr>='0' && *matr<='9') || *matr=='-' || *matr=='+' ||
              *matr=='.' || (*matr>='a' && *matr<='z') || *matr=='(' ||
              *matr=='.' ) { strcpy(matfile,"*"); return(0); }

        *matfile=EOS;
        if (strchr(matr,':')==NULL) strcpy(matfile,edisk);
        strcat(matfile,matr);
        if (strchr(matr,'.')==NULL) strcat(matfile,".MAT");
        return(1);
        }

static char *suluin(char *s,int taso,char *d)
        {
        int sulkuja=0;
        int h,t;

        h=0;
        t=3;  /* t=2 */
        while (*(s+h))
        switch (*(s+h++))
            {
          case '(': ++sulkuja; break;
          case ')': --sulkuja; break;
          case '+':
          case '-':
                    if (sulkuja>0) break;
                    t=0; break;
          case '*':
                    if (sulkuja>0) break;
                    if (t>1) t=1; break;
          case '^':
                    if (sulkuja>0) break;
                    if (t>2) t=2; break;
            }
/* printf("s=%s t=%d taso=%d\n",s,t,taso); sur_getch(); */
        if (sulkuja) t=0;  /* virheellinen lauseke */
        if (t<taso) { strcpy(d,"("); strcat(d,s); strcat(d,")"); }
        else          strcpy(d,s);


        return(d);
        }


static int rem_load(char *matr,int lin)   /* kirjoittaa matriisitiedostossa olevat kommenttirivit */
/* char *matr;          riviltÑ lin lÑhtien */
/* int lin;             Palattaessa antaa kommenttirivien mÑÑrÑn */
                     /* tulostiedoston eout oltava auki */
        {
        char matfile[LNAME];
        char x[ERC+1], *osa[10];
        int i,j,mname;
        int transp;
        int n_rem; // 29.1.2005
// RS REM        extern int n_row_comments;

        n_row_comments=0; // 29.1.2005
        transp=0; i=strlen(matr)-1; // 26.9.2001
        if (matr[i]=='\'')
        { transp=1; matr[i]=EOS; }

        name(matfile,matr);
        if (transp) strcat(matr,"'"); // 26.9.2001
        MAT=muste_fopen(matfile,"rb");
        for (i=0; i<ERC; ++i) x[i]=(char)getc(MAT); x[ERC]=EOS;
        i=split(x,osa,10);
        mname=atoi(osa[3]);    /* 10.2.91 aikaisemmin mname<=2 jne. */
        if (mname<=1) { fclose(MAT); return(0); }
                       /* 15.12.89 */
        n_rem=mname-1;
        for (j=0; j<mname-1; ++j)
            {
            for (i=0; i<ERC; ++i) x[i]=(char)getc(MAT); x[ERC]=EOS;

            if (*x=='/') // 29.1.2005
                {
                if (rowcomments==NULL)
                    {
                    sprintf(sbuf,"%sROWCOMM.TMP",etmpd);
                    rowcomments=fopen(sbuf,"wt");
                    }
                --n_rem; ++n_row_comments;
                i=strlen(x)-1; while (i>0 && x[i]==' ') x[i--]=EOS;
                fprintf(rowcomments,"%s\n",x+1);
                }
            else
                output_line(x,eout,lin); if (lin) ++lin;
            }
        if (rowcomments!=NULL) fclose(rowcomments);
        fclose(MAT);
        return(n_rem); // 29.1.2005
//      return(mname-1);
        }


static int matload2(
char *matr,  /* matriisin nimi */
double **A,  /* matriisitila (alkuosoite) (malloc) */
int *rdim,   /* rivien lkm */
int *cdim,   /* sar. lkm   */
char **rlab, /* rivien otsikot (malloc) */
char **clab, /* sar. otsikot   (malloc) */
int *lr,     /* riviotsikon pituus */
int *lc,     /* sar.otsikon pituus */
int  *type,  /* tyyppi */
char *expr,  /* lauseke (sis.nimi) max ERC */
int check   /* 1=vain dimensiot etc. luetaan */
)
        {
        char matfile[LNAME];
        char x[ERC+1], *osa[10];
        int i,i1,i2,j,j1,j2;
        int mname,mc,mcl,mr,mrl,mat;
        int m,n;
        char *p;
        double *a;
        register int h;
        char *pl;
        int transp; /* A'  7.1.1999 */
        int m1,n1;
// RS REM        extern char *suluin();

        transp=0; i=strlen(matr)-1;
        if (matr[i]=='\'')
            { transp=1; matr[i]=EOS; }

        i=name(matfile,matr);
        if (transp) strcat(matr,"'"); // 26.9.2001
        if (i==0) /* skalaari */
            {
// RS REM            extern double arit_atof();

            *rdim=*cdim=1;
            *lr=*lc=0;
            *type=30;
            i=varaa_tila(A,1,1,NULL,NULL,0,0); if (i<0) return(-1);
  /*        *A[0]=arit_atof(matr); if (l_virhe) return(-1);  */
            laske(matr,A[0]); /* 7.1.1999 */
            strcpy(expr,matr);
            return(1);
            }

/*  printf("\nmatfile=%s\n",matfile); sur_getch(); */
        MAT=muste_fopen(matfile,"rb");
        if (MAT==NULL)
            {
            if (etu==2) { strcpy(tut_info,"MATerr@"); return(-1); } /* 20.4.1997 */
            PR_EBLD;
            mat_parser_zero();
            sprintf(sbuf,"\nMatrix file %s not found!",matfile);
            mtx=erun=0;
            sur_print(sbuf); WAIT; PR_ENRM; return(-1);
            }
        for (i=0; i<ERC; ++i) x[i]=(char)getc(MAT); x[ERC]=EOS;
 /*     printf("\nx=%s",x); sur_getch();    */
        i=split(x,osa,10);
        if (strncmp(osa[0],"MATRIX84",8)!=0)
            {
            if (etu==2) { strcpy(tut_info,"MATerr@"); return(-1); } /* 20.4.1997 */
            PR_EBLD;
            sprintf(sbuf,"\n%s is not a matrix file!",matfile);
            mtx=erun=0;
            sur_print(sbuf); WAIT; PR_ENRM; return(-1);
            }

/*      newtype=0; if (osa[0][8]=='D') newtype=1;   22.7.1998      */
/*    for (i=0; i<10; ++i) printf("\n%s",osa[i]);  sur_getch(); */
        *rdim=atoi(osa[1]); *cdim=atoi(osa[2]);
        mname=atoi(osa[3]);

        *lr=atoi(osa[4]); *lc=atoi(osa[5]); *type=atoi(osa[6]);
        ++mname; mc=mname+1;

        m1=m=*rdim; n1=n=*cdim;
        mrl=*lr; mcl=*lc;

        if (transp && *type>=10) transp=0;

        if (transp)
            {
            i=*rdim; *rdim=*cdim; *cdim=i;
            i=m; m=n; n=i;
            i=*lr; *lr=*lc; *lc=i;
            i=mrl; mrl=mcl; mcl=i;
            }
/* RS REM Rajoitustarkistuksia
        i=optdim_d();
        if (i && ( i<m || i<n)) err(0);
// printf("\noptdim=%d|",i); sur_getch();
*/

        muste_fseek(MAT,(long)((mname-1)*ERC),0);
        for (i=0; i<ERC; ++i) expr[i]=(char)getc(MAT); expr[ERC-1]=EOS;
        p=strchr(expr,' '); if (p!=NULL && p-expr<ERC) *p=EOS;
        if (transp)
            {
            strcpy(x,expr);
            if (strlen(x)>ERC-5)
                {
                x[ERC-5]=EOS; strcat(x,"...");
                }
            suluin(x,2,expr); strcat(expr,"'");
            }
        if (check) { fclose(MAT); return(1); }
        i=varaa_tila(A,m,n,rlab,clab,mrl,mcl); if (i<0) return(-1);
        a=*A;
        if (*type==20)
            { for (i=0; i<m; ++i) for (j=0; j<n; ++j) a[i+m*j]=0; }

/**************************** aiirretty ylemmÑksi 22.7.1998
        muste_fseek(MAT,(long)((mname-1)*ERC),0);
        for (i=0; i<ERC; ++i) expr[i]=(char)getc(MAT); expr[ERC-1]=EOS;

        p=strchr(expr,' '); if (p!=NULL && p-expr<ERC) *p=EOS;
****************************************************/

        i=muste_fseek(MAT,(long)((mc-1)*ERC),0);
        if (transp)
            for (i=0; i<m*mrl; ++i) (*rlab)[i]=(char)getc(MAT);
        else
            for (i=0; i<n*mcl; ++i) (*clab)[i]=(char)getc(MAT);
        pl=*rlab; if (transp) pl=*clab;
        for (i=0; i<m1; ++i)
            {
            j1=mrl; if (transp) j1=mcl;
            for (j=0; j<j1; ++j) { *pl=(char)getc(MAT); ++pl; }
            j1=0; j2=n1-1;
            if (*type)
                {
                j2=i;
                if (*type==20) j1=i;
                }
            for (j=j1; j<=j2; ++j)
                {
                p=(char *)&a[i+m*j];
                if (transp) p=(char *)&a[j+m*i];
                for (h=0; h<sizeof(double); ++h) *(p+h)=getc(MAT);
                }
            }
        if (*type==10)
            {
            for (i=0; i<m; ++i) for (j=0; j<=i; ++j)
                a[j+m*i]=a[i+m*j];
            }
        fclose(MAT);
        return(1);
        }

static int matload(
char *matr,  /* matriisin nimi */
double **A,  /* matriisitila (alkuosoite) (malloc) */
int *rdim,   /* rivien lkm */
int *cdim,   /* sar. lkm   */
char **rlab, /* rivien otsikot (malloc) */
char **clab, /* sar. otsikot   (malloc) */
int *lr,     /* riviotsikon pituus */
int *lc,     /* sar.otsikon pituus */
int  *type,  /* tyyppi */
char *expr  /* lauseke (sis.nimi) max ERC */
)
        {
        int i;

        i=matload2(matr,A,rdim,cdim,rlab,clab,lr,lc,type,expr,0);
        return(i);
        }

int muste_polload(
char *matr,  /* matriisin nimi */
double **A,  /* matriisitila (alkuosoite) (malloc) */
int *rdim,   /* rivien lkm */
int *cdim,   /* sar. lkm   */
char **rlab, /* rivien otsikot (malloc) */
char **clab, /* sar. otsikot   (malloc) */
int *lr,     /* riviotsikon pituus */
int *lc,     /* sar.otsikon pituus */
int  *type,  /* tyyppi */
char *expr   /* lauseke (sis.nimi) max ERC */
)
        {
        int i;

        i=matload2(matr,A,rdim,cdim,rlab,clab,lr,lc,type,expr,0);
        return(i);
        }

static int load_X(char *nimi)
        {
        int i;

        i=matload(nimi,&X,&mX,&nX,&rlabX,&clabX,&lrX,&lcX,&typeX,exprX);
        return(i);
        }

static int load_X_check(char *nimi)
        {
        int i;

    i=matload2(nimi,&X,&mX,&nX,&rlabX,&clabX,&lrX,&lcX,&typeX,exprX,1);
        return(i);
        }

static int load_Y(char *nimi)
        {
        int i;

        i=matload(nimi,&Y,&mY,&nY,&rlabY,&clabY,&lrY,&lcY,&typeY,exprY);
        return(i);
        }

static int scalar_write(char *s)
        {
// RS REM        extern int sp_read;

        if (strchr(s,'=')==NULL) return(1);
        if (sp_read==0)
            {
            if (mtx) spn=sp_init(0); else spn=sp_init(r1+r-1);
            if (spn<0) return(-1); sp_read=1;
            }
        spn=spread3(s,0); if (spn<0) return(-1);
        return(1);
        }

static int merkitse(char *tulos,char *lauseke,int tyyppi,int m,int n)
        {
        char x[LLENGTH];
        char y[LLENGTH+32];
        char luku[8];
        char *p;
        int i;

        if (tyyppi<0) return(1);
        if (n_mat>1) return(1);
        if (strchr(lauseke,'=')!=NULL)
            scalar_write(lauseke);
        if (mtx) return(1);
        edread(x,r1+r-1);
        p=x+1;
        while (p!=NULL)
            {
            p=strchr(p,'/'); if (p==NULL) break;
            if (strncmp(p-1," / ",3)==0) break;
            ++p;
            }
        if (p==NULL)
            {
            i=strlen(x)-1;
            while (x[i]==' ') --i;
            i+=2; i=(i/10+1)*10;
            }
        else if (*(p+2)!='*') return(1);
        else i=p-x;

        strcpy(y,"/ *"); strcat(y,tulos); strcat(y,EQUALS);
        strcat(y,lauseke); strcat(y," ");
        switch (tyyppi)
            {
          case  0: break;
          case 10: strcat(y,"S"); break;
          case 20: strcat(y,"D"); break;
            }
        strcat(y,muste_itoa(m,luku,10)); strcat(y,"*");
        strcat(y,muste_itoa(n,luku,10));
        edwrite(space,r1+r-1,i);
        edwrite(y,r1+r-1,i);
        return(1);
        }


static int text_labels(char *lab,int n,int len,char *text)
        {
        char *t,*p;
        int pit;
        char label[32];
        int i,j,k;

        if (*text=='"') t=text+1; else t=text;
        p=strchr(t,'"'); if (p!=NULL) *p=EOS;
        pit=strlen(t);


        for (i=0; i<n*len; ++i) lab[i]=' ';
        for (i=0; i<n; ++i)
            {
            sprintf(label,"%s%d",t,i+1);
      /*    k=len-strlen(label); if (k<0) k=0;  */
            for (j=0; j<len; ++j)
                {
                if (label[j]==EOS) break;
                lab[i*len+j]=label[j];
                }
            }
        return(1);
        }

static int lab_atoi(char *x,char *lab,int m,int len) /* 11.5.1999 */
        {
        char s[32];
        int i;

        strcpy(s,x);
        for (i=strlen(s); i<len; ++i) s[i]=' ';
        for (i=0; i<m; ++i)
            if (strncmp(s,lab+i*len,len)==0) break;
        if (i==m) return(arit_atoi(x));
        return(i+1);
        }


static int osamat(    /* return (ensimm.jatkoparametrin indeksi) */
char *word[],
int j,       /* word[j] ensimm. osamatriisinimen osa */
int ind[])  /* indeksirajoja varten */
        {
        int i;
        char *p, *q;

        p=strchr(word[j],'(');
        if (p==NULL) { ind[0]=0; return(j+1); }
        *p=EOS; /* matriisin nimi nyt word[j] */
        ++p;
        if (*p==EOS || *p=='*') { ind[0]=1; ind[1]=DIM_HUGE; }
        else
            {
            q=strchr(p,':');
            if (q==NULL) { ind[0]=ind[1]=lab_atoi(p,rlabX,mX,lrX); }
            else {
                 *q=EOS; ind[0]=lab_atoi(p,rlabX,mX,lrX);
                 ind[1]=lab_atoi(q+1,rlabX,mX,lrX);
                 }
            }
        p=word[j+1];
        if (*p==')' || *p=='*') { ind[2]=1; ind[3]=DIM_HUGE; }
        else
            {
            i=strlen(p)-1; if (p[i]==')') p[i]=EOS;
            q=strchr(p,':');
            if (q==NULL) { ind[2]=ind[3]=lab_atoi(p,clabX,nX,lcX); }
            else {
                 *q=EOS; ind[2]=lab_atoi(p,clabX,nX,lcX);
                 ind[3]=lab_atoi(q+1,clabX,nX,lcX);
                 }
            }
        if (l_virhe) return(-1);
        return(j+2);
        }

static int etsi_muoto(char *muoto,int minlev,double *A,int m,int n)
        {
        int kok=1;
        int neg=0;
        double a, max;
        unsigned int i;
        int lev,lev2;

        max=fabs(A[0]);
        for (i=0; i<m*n; ++i)
            {
            a=A[i]; if (a<0) { neg=1; a=-a; }
            if (a!=floor(a)) kok=0;
            if (a>max) max=a;
            }

        if (max>1e10)
            {
            strcpy(muoto,"%8e");
            return(1);
            }

        if (max==0) max=1;
        if (max) lev=lev2=(int)(log(max+1e-7)/log((double)10))+1;
                                       /* +1e-7 7.3.94 */
        if (lev<minlev) lev=minlev;
        for (i=0; i<lev; ++i) muoto[i]='#'; muoto[lev]=EOS;
        if (kok) return(1);
        if (lev2<1) lev2=1;
        if (lev2<lev-neg) muoto[lev2+neg]='.';
        return(1);
        }


static int remove_minus_one_coeffs(char *expr)   // 16.5.2008
    {
    char x[LLENGTH];
    char *p,*q;

// printf("\nexpr=%s",expr); sur_getch();
    p=strstr(expr,"+(-1)*");
    if (p==NULL) return(1);
    strcpy(x,expr); p=x; *expr=EOS;
    while (1)
        {
        q=strstr(p,"+(-1)*");
        if (q==NULL) break;
        *q=EOS;
        strcat(expr,p);
        strcat(expr,"-");
        p=q+6;
        }
    strcat(expr,p);
// printf("\nexpr=%s|",expr); sur_getch();
    return(1);
    }

static int type_mat(double *A,int m,int n)
        {
        int i,j;
        int type=20;

        if (m!=n) return(0);
        for (i=0; i<m; ++i) for (j=0; j<i; ++j)
            {
            if (A[i+m*j]) type=10;
            if (A[i+m*j]!=A[j+m*i]) { type=0; i=j=m; }
            }
        return(type);
        }

static int mat_save_rem(
char *matr,   /* matriisin nimi */
double *A,    /* matriisitila */
int m,        /* rivien lkm */
int n,        /* sar. lkm   */
char *rlab,   /* rivien otsikot */
char *clab,   /* sar. otsikot  */
int mrl,      /* riviotsikon pituus */
int mcl,      /* sar.otsikon pituus */
int type,     /* tyyppi =-1,jos tuntematon */
char *expr,   /* lauseke (sis.nimi) max ERC */
int nrem,     /* kommenttirivien lkm */
int remrivi,  /* kommenttien alku */
int n_rowrem,    // rivikommenttien lkm
char *p_rowrem  // rivikommentit
)
        {
        char matfile[LNAME];
        char x[ERC+1];
        int i,j,j1,j2;
        int mname;
        register int h;
        char *p;
        char rivi[LLENGTH];
        char *nimi;
        char *pl;

        char *p_com; // 29.1.2005
        int len,k;

        remove_minus_one_coeffs(expr);  // 16.5.2008

        nimi=expr;
        i=strlen(matr);
        if (matr[i-1]=='!') { matr[i-1]=EOS; nimi=matr; }
        name(matfile,matr);
        if (*matfile=='*')
            {
            if (m!=1 || n!=1)
                {
                sprintf(sbuf,"\nName %s not allowed for a matrix!",matr);
                sur_print(sbuf); WAIT; return(-1);
                }
            sprintf(rivi," %s=%.15e",matr,A[0]);
            scalar_write(rivi); /* vain initialisointiin */
            i=spfind(matr);
            if (i<0) scalar_write(rivi);  /* ei tarvita: aina i>=0 */
            else { spb[i]=NULL; arvo[i]=A[0]; }
            sprintf(rivi,"%g",A[0]);
            merkitse(matr,rivi,0,1,1);
            return(-1);
            }
        MAT=muste_fopen(matfile,"wb");
        if (MAT==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s !",matfile);
            sur_print(sbuf); WAIT; return(-1);
            }

        if (nrem<0) nrem=0;
        mname=nrem+1+n_rowrem;
//      mname=nrem+1; // tilapÑinen!!!!!!!!!!!!!!!!!!!!!

        if (type==-1) type=type_mat(A,m,n);

        sprintf(x,"MATRIX84D %d %d %d %d %d %d",
                           m,n,mname,mrl,mcl,type);
        for (i=strlen(x); i<ERC; ++i) x[i]=' ';
        for (i=0; i<ERC; ++i) putc((char)x[i],MAT); // RS CHA (int) -> (char)

        if (nrem)
            {
// printf("\nnrem=%d|",nrem); sur_getch();
            for (j=0; j<nrem; ++j)
                {
                edread(rivi,remrivi+j);
//Rprintf("\nrivi: %s|",rivi); // sur_getch();
           for (i=0; i<ERC; ++i) { if (i==c2) break; putc((char)rivi[i+1],MAT); }
                for (i=c2; i<ERC; ++i) putc((char)' ',MAT);
                }
            }
// printf("\nn_rowrem=%d|",n_rowrem); sur_getch();
        if (n_rowrem) // 29.1.2005
            {
            p_com=p_rowrem;
            for (i=0; i<n_rowrem; ++i)
                {
//              strcpy(sbuf,p_com); printf("\n%s|",sbuf); sur_getch();
                len=strlen(p_com);
           for (k=0; k<ERC; ++k) { if (k==len) break; putc((char)p_com[k],MAT); }
                for (k=len; k<ERC; ++k) putc((char)' ',MAT);
                p_com+=len+1;
                }
            }

/*      j=strlen(expr);    */
        j=strlen(nimi);
/* printf("MS.C: j=%d nimi=%s\n",j,nimi); sur_getch(); */
        for (i=0; i<j; ++i) putc((char)nimi[i],MAT);
        for (i=j; i<ERC; ++i) putc((char)' ',MAT);




        for (i=0; i<n*mcl; ++i) putc((char)clab[i],MAT);

        pl=rlab;
        for (i=0; i<m; ++i)
            {
            for (j=0; j<mrl; ++j) { putc((char)*pl,MAT); ++pl; }
            j1=0; j2=n-1;
            if (type)
                {
                j2=i;
                if (type==20) j1=i;
                }
            for (j=j1; j<=j2; ++j)
                {
                p=(char *)&A[i+m*j];
                for (h=0; h<sizeof(double); ++h) putc((char)(*(p+h)),MAT); // RS CHA 64-BIT sizeof(long)
                }
            }

        if (ferror(MAT))
            {
            sprintf(sbuf,"\nCannot save matrix %s !",matfile);
            sur_print(sbuf); WAIT; type=-1;
            }
        fclose(MAT);
        return(type);
        }

static int mat_save(
char *matr,   /* matriisin nimi */
double *A,    /* matriisitila */
int m,        /* rivien lkm */
int n,        /* sar. lkm   */
char *rlab,   /* rivien otsikot */
char *clab,   /* sar. otsikot  */
int mrl,      /* riviotsikon pituus */
int mcl,      /* sar.otsikon pituus */
int type,     /* tyyppi =-1,jos tuntematon */
char *expr,   /* lauseke (sis.nimi) max ERC */
int nrem,     /* kommenttirivien lkm */
int remrivi  /* kommenttien alku */
)
    {
    int i;

    i=mat_save_rem(matr,A,m,n,rlab,clab,mrl,mcl,type,expr,nrem,remrivi,
                                       0,NULL);
    return(i);
    }


static int op_copy(char *tulos,char *s)
        {
        int i;

        i=load_X(s); if (i<0) return(-1);
        i=mat_save(tulos,X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int op_tell(char *tulos,char *s)
        {
        int i;

        i=load_X_check(s); if (i<0) return(-1);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int nim(char *s,char *d)
        {
        if (strlen(s)<ERC) { strcpy(d,s); return(1); }
        strncpy(d,s,ERC-4); d[ERC-4]=EOS;
        strcpy(d,"...");
        return(1);
        }


static int label_type(char *lab,int n,int len)
        {
        int i,j,h,k;
        char x[LNAME];

        for (i=0,j=0; i<n; ++i,j+=len)
            {
            for (h=0; h<len; ++h) x[h]=lab[j+h];
            x[len]=EOS;
            k=atoi(x);
            if (k==0) return(1);
            }
        return(0);
        }


static int numlab2(char *lab,int n,int len,int base)
        {
        unsigned int h,i,j,k;
        char sana[6];
        int sar;

        --base;
        for (i=0; i<n*len; ++i) lab[i]=' ';
        if (n+base<1000) sar=3;
        else if (n+base<100000) sar=5;
        else sar=8;

        for (i=0; i<n; ++i)
            {
            muste_itoa(i+1+base,sana,10);
            h=strlen(sana);
            for (j=i*len+sar-h, k=0; k<h; ++k, ++j) lab[j]=sana[k];
            }
        return(1);
        }

static int numlab(char *lab,int n,int len)
        {
        numlab2(lab,n,len,1);
        return(1);
        }

static int parm_error()
        {
        PR_EBLD;
        sur_print("\nSyntax error in MAT operation!");
        WAIT; PR_ENRM; erun=0;
        return(1);
        }


static int op_add_sub(int k)
        {
        int i;
        char *lab1,*lab2;
        int lr,lc;
        char expr1[LNAME];
// RS REM        extern char *suluin();

        if (mX!=mY || nX!=nY) { dim_error(); return(-1); }
        if (k==1) mat_add(X,X,Y,mX,nX);
        else      mat_sub(X,X,Y,mX,nX);

        strcpy(tnimi,exprX);
        if (k==1) { strcat(tnimi,"+"); strcat(tnimi,exprY); }
        else { strcat(tnimi,"-"); strcat(tnimi,suluin(exprY,1,expr1)); }
        nim(tnimi,exprX);
        lab1=rlabX; lr=lrX;
        if (label_type(rlabX,mX,lrX)<label_type(rlabY,mY,lrY))
            lab1=rlabY; lr=lrY;
        lab2=clabX; lc=lcX;
        if (label_type(clabX,nX,lcX)<label_type(clabY,nY,lcY))
            lab2=clabY; lc=lcY;
        i=mat_save(tulos,X,mX,nX,lab1,lab2,lr,lc,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int op_add()
        {
        return(op_add_sub(1));
        }
static int op_sub()
        {
        return(op_add_sub(2));
        }

static int op_mlt()
        {
// RS REM        char *suluin();
        char expr1[LLENGTH];
        int i;

        if (mX==1 && nX==1)
            {
            for (i=0; i<mY*nY; ++i) Y[i]*=X[0];
            strcpy(tnimi,suluin(exprX,1,expr1)); strcat(tnimi,"*");
            strcat(tnimi,suluin(exprY,1,expr1));
            nim(tnimi,exprX);
            i=mat_save(tulos,Y,mY,nY,rlabY,clabY,lrY,lcY,-1,exprX,0,0);
            merkitse(tulos,exprX,i,mY,nY);
            return(1);
            }

        if (mY==1 && nY==1) // 29.4.2001 MAT C=A*c
            {
            for (i=0; i<mX*nX; ++i) X[i]*=Y[0];
            strcpy(tnimi,suluin(exprX,1,expr1)); strcat(tnimi,"*");
            strcat(tnimi,suluin(exprY,1,expr1));
            nim(tnimi,exprX);
            i=mat_save(tulos,X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
            merkitse(tulos,exprX,i,mX,nX);
            return(1);
            }

        if (nX!=mY) { dim_error(); return(-1); }
        i=varaa_tila(&T,mX,nY,NULL,NULL,0,0);
        if (i<0) return(-1);
        if (typeX==20)     mat_dmlt(T,X,Y,mY,nY);
        else if(typeY==20) mat_mltd(T,X,Y,mX,nX);
        else               mat_mlt(T,X,Y,mX,nX,nY);
        strcpy(tnimi,suluin(exprX,1,expr1)); strcat(tnimi,"*");
        strcat(tnimi,suluin(exprY,1,expr1));
        nim(tnimi,exprX);

        i=mat_save(tulos,T,mX,nY,rlabX,clabY,lrX,lcY,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nY);
        return(1);
        }

static int op_div() /* 11.1.1999 */
        {
// RS REM        char *suluin();
        char expr1[LLENGTH];
        int i;

        if (mY!=1 || nY!=1) { dim_error(); return(-1); }
        for (i=0; i<mX*nX; ++i) X[i]/=Y[0];
        strcpy(tnimi,suluin(exprX,1,expr1)); strcat(tnimi,"/");
        strcat(tnimi,suluin(exprY,1,expr1));
        nim(tnimi,exprX);
        i=mat_save(tulos,X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int op_transp()
        {
        int i;

// RS REM        char *suluin();
        char expr1[LLENGTH];

        i=varaa_tila(&T,nX,mX,NULL,NULL,0,0);
        if (i<0) return(-1);
        mat_transp(T,X,mX,nX);

        strcpy(tnimi,suluin(exprX,2,expr1)); strcat(tnimi,"'");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,nX,mX,clabX,rlabX,lcX,lrX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,nX,mX);
        return(1);
        }

static int power_result()
        {
        int i;
        char expr1[LLENGTH];

        strcpy(tnimi,suluin(exprX,3,expr1)); strcat(tnimi,"^");
        strcat(tnimi,exprY);
        nim(tnimi,exprX);

        i=mat_save(tulos,X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int general_power(double a)
        {
        int i,n,j,k;
        double eps=1e-16;
        double tol;

        if (mX==1 || nX==1)
            {
            if (mX!=1) n=mX; else n=nX;
            for (i=0; i<n; ++i) X[i]=pow(X[i],a);
            power_result();
            return(1);
            }

        if (typeX==20)
            {
            for (i=0; i<mX; ++i) X[i*(mX+1)]=pow(X[i*(mX+1)],a);
            power_result();
            return(1);
            }

        if (typeX!=10)
            {
            sur_print("\nNot a symmetric matrix!");
            WAIT; return(-1);
            }

        pow1=(double *)malloc(mX*sizeof(double));
        if (pow1==NULL) { ei_tilaa(); return(-1); }
        pow2=(double *)malloc(mX*mX*sizeof(double));
        if (pow2==NULL) { ei_tilaa(); return(-1); }

        tol=(1e-300)/eps;
        mat_tred2(pow1,pow2,X,mX,tol);
        mat_tql2(pow1,pow2,X,mX,eps,30);

        if (pow1[mX-1]<0.0)
            {
            sur_print("\nMatrix is not positive (semi)definite!");
            WAIT; return(-1);
            }
        if (pow1[mX-1]==0.0 && a<0.0)
            {
            sur_print("\nMatrix is not positive definite!");
            WAIT; return(-1);
            }

        for (i=0; i<mX; ++i) pow1[i]=pow(pow1[i],a);
        for (i=0; i<mX*mX; ++i) pow2[i]=0.0;
        for (i=0; i<mX; ++i)
            for (k=0; k<mX; ++k)
                {
                tol=pow1[k]*X[i+mX*k];
                for (j=0,n=mX*k; j<=i; ++j)
                    {
                    pow2[i+mX*j]+=tol*X[n++];
                    }
                }
        for (i=0; i<mX; ++i) for (j=0; j<=i; ++j)
            X[i+mX*j]=X[j+mX*i]=pow2[i+mX*j];
        power_result();
        free(pow1); free(pow2);
        return(1);
        }


static int op_pow()
        {
        int i;
        int n,k,j;
        char bin[100];

        if (mY!=1 || nY!=1)
            {
            sur_print("\nPower must be a positive integer!");
            WAIT; return(-1);
            }
        if ((double)(int)Y[0]!=Y[0] || Y[0]<0.0
             || mX==1 || nX==1 ) // 6.8.2006
            {
            return(general_power(Y[0]));
            }
        if (mX!=nX) { dim_error(); return(-1); }
        pow1=(double *)malloc(mX*mX*sizeof(double));
        if (pow1==NULL) { ei_tilaa(); return(-1); }
        pow2=(double *)malloc(mX*mX*sizeof(double));
        if (pow2==NULL) { ei_tilaa(); return(-1); }

        n=Y[0];
        muste_itoa(n,bin,2);
        k=strlen(bin);
        for (i=0; i<mX*mX; ++i) pow1[i]=X[i];
        for (i=0; i<mX; ++i)
            {
            for (j=0; j<mX; ++j) pow2[i+mX*j]=0.0;
            pow2[i*(mX+1)]=1.0;
            }
        for (j=0; j<k; ++j)
            {
            if (bin[k-1-j]=='1')
                {
                mat_mlt(X,pow1,pow2,mX,mX,mX);
                if (j<k-1) for (i=0; i<mX*mX; ++i) pow2[i]=X[i];
                }
            if (j==k-1) break;
            mat_mlt(X,pow1,pow1,mX,mX,mX);
            for (i=0; i<mX*mX; ++i) pow1[i]=X[i];

            }
        power_result();
        free(pow2); free(pow1);
        return(1);
        }

static int op_kill()   /* MAT KILL A,B,C */
        {
        int i;
        char file[64];
        char command[LLENGTH];

        for (i=2; i<g; ++i)
            {
            muste_strupr(word[i]); /* 14.10.1998 */
            name(file,word[i]);
            if (strcmp(file,"*")==0) continue; /* 31.12.1998 */
            sur_delete(file);
//          strcpy(command,"DEL "); strcat(command,file);
//          system(command);
            }
        return(1);
        }

static int op_dim()
        {
        int i,rank;
        char rtext[LLENGTH];

        *rtext=EOS;
        i=load_X_check(word[2]); if (i<0) return(-1);

        if (typeX==20)
            {
            i=load_X(word[2]); if (i<0) return(-1);
            rank=0;
            for (i=0; i<mX; ++i) if (X[i*(mX+1)]!=0.0) ++rank;
            sprintf(rtext," rank%s=%d",word[2],rank);
            }
        sprintf(tnimi,"%s DIM %s /* row%s=%d col%s=%d%s",
                  word[0],word[2],word[2],mX,word[2],nX,rtext);
        i=scalar_write(tnimi);
        if (mtx) return(i);
        edwrite(space,r1+r-1,1);
        edwrite(tnimi,r1+r-1,1);
        return(i);
        }

static int op_trace()
        {
        int i;
        double tr;

        i=load_X(word[2]); if (i<0) return(-1);
        i=0; tr=0.0; while (i<mX && i<nX) { tr+=X[i+mX*i]; ++i; }
        sprintf(tnimi,"MAT TRACE %s /* tr%s=%.*g",word[2],word[2],accuracy,tr);
        i=scalar_write(tnimi);
        if (mtx) return(i);
        edwrite(space,r1+r-1,1);
        edwrite(tnimi,r1+r-1,1);
        return(i);
        }

static int op_rem()
        {
        scalar_write(comline2);
        return(1);
        }

static int op_con(double arvo)
        {
        int i,j;
        char *nimi;

        if(g<2) { parm_error(); return(-1); }
        mX=arit_atoi(word[1]); nX=arit_atoi(word[2]);

        if (g>2) arvo=arit_atof(word[3]);
        if (l_virhe) return(-1);

        i=varaa_tila(&X,mX,nX,&rlabX,&clabX,MAXRLAB,MAXCLAB);
        if (i<0) return(-1);
        numlab(rlabX,mX,8);
        numlab(clabX,nX,8);
        for (i=0; i<mX; ++i) for (j=0; j<nX; ++j) X[i+mX*j]=arvo;
        if (arvo==0.0) nimi="0"; else nimi="CON";
        mat_save(tulos,X,mX,nX,rlabX,clabX,8,8,-1,nimi,0,0);
        return(1);
        }

static int op_zer()
        {
        return(op_con(0.0));
        }

static int op_idn_tri(int type)
/* int type;  1=idn 2=tri 3=Hilbert */
        {
        double arvo=1.0;
        int i,j;
        char s[10];
        int min_m;

        if(g<2) { parm_error(); return(-1); }
        mX=arit_atoi(word[1]); if (g==1) nX=mX; else nX=arit_atoi(word[2]);
        if (g>2) arvo=arit_atof(word[3]);
        if (l_virhe) return(-1);

        i=varaa_tila(&X,mX,nX,&rlabX,&clabX,MAXRLAB,MAXCLAB);
        if (i<0) return(-1);
        numlab(rlabX,mX,8);
        numlab(clabX,nX,8);
        min_m=mX; if (nX<mX) min_m=nX;
        for (i=0; i<mX; ++i) for (j=0; j<nX; ++j) X[i+mX*j]=0;

        switch (type)
            {
          case 1:
            strcpy(s,"IDN");
            for (i=0; i<min_m; ++i) X[i*(mX+1)]=arvo;
            break;

          case 2:
            {
            strcpy(s,"CON");
            for (i=0; i<min_m; ++i)
                for (j=i; j<min_m; ++j)
                    X[i+mX*j]=arvo;
            break;
            }
          case 3:
            strcpy(s,"HILBERT");
            for (i=0; i<mX; ++i)
                for (j=0; j<nX; ++j)
                    X[i+mX*j]=1.0/(double)(i+j+1);
            break;
            }

        mat_save(tulos,X,mX,nX,rlabX,clabX,8,8,-1,s,0,0);
        return(1);
        }

static int op_dv()
        {
        int n,i,j;
        char *p;
        double dbl;

        i=load_X(word[1]); if (i<0) return(-1);
        if (mX>1 && nX>1) { dim_error(); return(-1); }
        n=mX+nX-1;

        i=varaa_tila(&T,n,n,NULL,NULL,0,0); if (i<0) return(-1);
        for (i=0; i<n; ++i)
            {
            for (j=0; j<n; ++j) T[i+n*j]=0;
            dbl=X[i]; T[i*(n+1)]=dbl;  /* compiler-virheen takia */
            }
        strcpy(tnimi,"DV("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);
        if (mX>1) { p=rlabX; i=lrX; } else { p=clabX; i=lcX; }
        i=mat_save(tulos,T,n,n,p,p,i,i,20,exprX,0,0);
        merkitse(tulos,exprX,i,n,n);
        return(1);
        }

static int op_vd()
        {
        int i,n;
        char *p;

        i=load_X(word[1]); if (i<0) return(-1);
        if (mX<=nX) n=mX; else n=nX;
        for (i=0; i<n; ++i) X[i]=X[i*(mX+1)];
        strcpy(tnimi,"VD("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);
        if (mX<=nX) { p=rlabX; i=lrX; } else { p=clabX; i=lcX; }
        i=mat_save(tulos,X,n,1,p,"diag    ",i,8,-1,exprX,0,0);
        merkitse(tulos,exprX,i,n,1);
        return(1);
        }

static int op_diagvec()  /* 5.3.1995 */
        {
        int n,i,j,k;
        char *p;
        double dbl;

        i=load_X(word[1]); if (i<0) return(-1);
        if (mX>1 && nX>1) { dim_error(); return(-1); }
        n=mX+nX-1;

        i=varaa_tila(&T,n,n,NULL,NULL,0,0); if (i<0) return(-1);
        for (i=0; i<n; ++i)
            {
            for (j=0; j<n; ++j)
                {
                k=i-j; if (k<0) k=-k;
                T[i+n*j]=X[k];
                }
            }
        strcpy(tnimi,"DIAGVEC("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);
        if (mX>1) { p=rlabX; i=lrX; } else { p=clabX; i=lcX; }
        i=mat_save(tulos,T,n,n,p,p,i,i,-1,exprX,0,0);
        merkitse(tulos,exprX,i,n,n);
        return(1);
        }

static int op_diag() /* 3.4.1999 */
        {
        int i,k;

        i=load_X(word[1]); if (i<0) return(-1);
        k=nX; if (mX<nX) k=mX;
        i=varaa_tila(&T,k,1,NULL,NULL,0,0); if (i<0) return(-1);
        for (i=0; i<k; ++i) T[i]=X[(mX+1)*i];
        for (i=0; i<k*k; ++i) X[i]=0.0;
        for (i=0; i<k; ++i) X[(k+1)*i]=T[i];
        strcpy(tnimi,"DIAG("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);
        i=mat_save(tulos,X,k,k,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,k,k);
        return(1);
        }

static int op_load()
        {
        int i,j,lev;
        int ind[4];
        char muoto[LLENGTH];
        int eol;
        char x[LLENGTH];
        char *p;
        char name2[LNAME];  /* 28.5.1999 */

        if (g<3)
            {
            sur_print("\nUsage: _MAT LOAD <matrix_file>");
            WAIT; return(-1);
            }
/* -11.5.99 1) osamat(, 2) load_X( */
        strcpy(name2,word[2]); p=strchr(name2,'(');
        if (p!=NULL)
            {
            *p=EOS;
                           /* MAT LOAD A(R1:R2,C1:C2)' not allowed  */
                           /* 0   1    2p      3                    */

// printf("\ng=%d w2=%s w3=%s|",g,word[2],word[3]); sur_getch();
            if (g==3)
                {
                g=4;
                word[3]="1)";
                word[2][strlen(word[2])-1]=EOS;
                }
            i=strlen(word[3]);
            if (strncmp(word[3]+i-2,")'",2)==0)
                {
                sprintf(sbuf,"\nUsage: MAT LOAD %s'(columns,rows)",name2);
                sur_print(sbuf); WAIT; return(-1);
                }
            }
        i=load_X(name2); if (i<0) return(-1);
        j=osamat(word,2,ind); if (j<0) return(-1);
        if (ind[0]==0) { ind[0]=ind[2]=1; ind[1]=mX; ind[3]=nX; }
        else
            {
            if (ind[0]<1) ind[0]=1;
            if (ind[0]>mX) ind[0]=mX;
            if (ind[1]<ind[0]) ind[1]=ind[0];
            if (ind[1]>mX) ind[1]=mX;
            if (ind[2]<1) ind[2]=1;
            if (ind[2]>nX) ind[2]=nX;
            if (ind[3]<ind[2]) ind[3]=ind[2];
            if (ind[3]>nX) ind[3]=nX;
            }
        *muoto=EOS;
        if (j+2==g) { strcpy(muoto,word[j]); ++j; }

        if (j==g) strcpy(x,"CUR+1"); else strcpy(x,word[j]); /* 4.2.1997 */
        eol=edline2(x,1,1); /* was editline */

/*      eol=editline(word[j],1,1); -4.2.1997 */
        if (eol==0) return(-1);
        if (*muoto==EOS)
            {
            lev=lcX; if (accuracy+1>lev) lev=accuracy+1;
            i=etsi_muoto(muoto,lev,X,mX,nX);
            if (i<0) return(-1);
            }
        strcpy(x,"MATRIX "); strcat(x,name2);
        i=output_open(eout); if (i<0) return(-1);
        output_line(x,eout,eol); if (eol) ++eol;
        i=rem_load(name2,eol); // i on kommenttirivien lkm
        if (eol) eol+=i;
        if (i || muste_strcmpi(exprX,name2)!=0) // 29.1.2005
            { output_line(exprX,eout,eol); if (eol) ++eol; }
        output_close(eout);
        matprint(X,mX,nX,ind[0],ind[1],ind[2],ind[3],
                        rlabX,clabX,lrX,lcX,muoto,"///",eol,
                        n_row_comments);
        return(1);
        }

static int op_copy2(char *matr)        /* C=A(i1:i2,j1:j2)  */
 /* word[1]=i1:i2 word[2]=j1:j2 matr=A */
        {
        int i,j,i1,i2,j1,j2;
        char *p;
        char sana1[64],sana2[64];
        double dbl;

//      if (g<2) return(-1);
        i=load_X(matr);
        if (i<0) return(-1);
        if (g==0) return(-1);
        if (g==1) word[2]="1"; // 23.1.2005

        strcpy(sana1,word[1]); strcpy(sana2,word[2]);
        p=strchr(word[1],':'); if (p!=NULL) *p=EOS;
        if (*word[1]==EOS || *word[1]=='*') { i1=1; i2=mX; }
        else
            {
            i1=i2=lab_atoi(word[1],rlabX,mX,lrX);
            if (p!=NULL) i2=lab_atoi(p+1,rlabX,mX,lrX);
            }
        i=strlen(word[2])-1; if (word[2][i]==')') word[2][i]=EOS;
        p=strchr(word[2],':'); if (p!=NULL) *p=EOS;
        if (*word[2]==EOS || *word[2]=='*') { j1=1; j2=nX; }
        else
            {
            j1=j2=lab_atoi(word[2],clabX,nX,lcX);
            if (p!=NULL) j2=lab_atoi(p+1,clabX,nX,lcX);
            }
        if (l_virhe) return(-1);
        if (i1<1 || j1<1 || i1>i2 || j1>j2 || i2>mX || j2>nX)
            { dim_error(); return(-1); }
        --i1; --i2; --j1; --j2;
        mT=i2-i1+1; nT=j2-j1+1;
        varaa_tila(&T,mT,nT,NULL,NULL,0,0);
        for (i=0; i<mT; ++i) for (j=0; j<nT; ++j)
            {
            dbl=X[i+i1+mX*(j+j1)];  /* Compiler-virheen takia */
            T[i+mT*j]=dbl;
            }
        strcpy(tnimi,matr); strcat(tnimi,"(");
        strcat(tnimi,sana1); strcat(tnimi,","); strcat(tnimi,sana2);
        strcat(tnimi,")");
        nim(tnimi,exprT);

    i=mat_save(tulos,T,mT,nT,rlabX+i1*lrX,clabX+j1*lcX,lrX,lcX,-1,exprT,0,0);

        return(1);
        }

static int op_copy3()  /* MAT C(i,j)=A */
        {
        int i,j,i1,i2,j1,j2;
        char *p;
        double dbl;
        int nimisijoitus=0;
                                        // MAT A(0,2)="nimi"
        if (strstr(word[2],"=\"")!=NULL) nimisijoitus=1;

        p=strchr(word[1],'('); if (p==NULL) { parm_error(); return(-1); }
        *p=EOS;
        i=load_X(word[1]); if (i<0) return(-1);

        if (nimisijoitus && *(p+1)=='0') i1=0;
        else
            i1=lab_atoi(p+1,rlabX,mX,lrX);
        p=strchr(word[2],')'); if (p==NULL) { parm_error(); return(-1); }
        *p=EOS;
        if (nimisijoitus && *word[2]=='0') j1=0;
        else
            j1=lab_atoi(word[2],clabX,nX,lcX); if (l_virhe) return(-1);
        if (i1<0 || i1>mX || j1<0 || j1>nX) { dim_error(); return(-1); }

        if (i1==0 || j1==0)    /* rivi- tai sarakeotsikko */
            {
            p+=2;
            if (*p=='"') ++p;
            i=strlen(p)-1; if (*(p+i)=='"') *(p+i)=EOS;
            if (i1==0 && j1>0)
                {
                strncpy(clabX+(j1-1)*lcX,space,lcX);
                strncpy(clabX+(j1-1)*lcX,p,lcX);
                }
            else if (j1==0 && i1>0)
                {
                strncpy(rlabX+(i1-1)*lrX,space,lrX);
                strncpy(rlabX+(i1-1)*lrX,p,lrX);
                }
            i=mat_save(word[1],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
            return(1);
            }

        i=load_Y(p+2); if (i<0) return(-1);

        if (mY<mX-i1+1) i2=i1+mY-1; else i2=mX;
        if (nY<nX-j1+1) j2=j1+nY-1; else j2=nX;
        --i1; --i2; --j1; --j2;
        for (i=i1; i<=i2; ++i) for (j=j1; j<=j2; ++j)
            {
            dbl=Y[i-i1+mY*(j-j1)];
            X[i+mX*j]=dbl;
            }

        strcpy(tnimi,exprX); strcat(tnimi,"&"); strcat(tnimi,exprY);
        nim(tnimi,exprX);

        if (lrX==lrY && lcX==lcY)   /* tapaus lrX!=lrY etc. hoitamatta */
            {
            if (label_type(rlabY,mY,lrY))
                strncpy(rlabX+i1*lrX,rlabY,(i2-i1+1)*lrY);
            if (label_type(clabY,nY,lcY))
                strncpy(clabX+j1*lcX,clabY,(j2-j1+1)*lcY);
            }
        i=mat_save(word[1],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        return(1);
        }

static int op_name()  /* MAT NAME A AS B */
        {
        int i;

        if (g<5) { parm_error(); return(-1); }
        i=load_X(word[2]); if (i<0) return(-1);
        i=mat_save(word[2],X,mX,nX,rlabX,clabX,lrX,lcX,-1,word[4],0,0);
        return(i);
        }

static int op_rlab()  /* MAT RLABELS FROM A TO B */
           /* MAT RLABELS "text" TO B */
        {
        int i;
        char *p;

        if (g<6)
            {
            if (g<5) { parm_error(); return(-1); }
            i=load_Y(word[4]); if (i<0) return(-1);
            if (muste_strnicmp(word[2],"NUM(",4)==0)
                {
                p=strchr(word[2],')'); if (p!=NULL) *p=EOS;
                numlab2(rlabY,mY,lrY,arit_atoi(word[2]+4));
                }
            else
                text_labels(rlabY,mY,lrY,word[2]);
            i=mat_save(word[4],Y,mY,nY,rlabY,clabY,lrY,lcY,-1,exprY,0,0);
            return(i);
            }
        i=load_X(word[3]); if (i<0) return(-1);
        i=load_Y(word[5]); if (i<0) return(-1);
        if (mX!=mY) { dim_error(); return(-1); }
        i=mat_save(word[5],Y,mY,nY,rlabX,clabY,lrX,lcY,-1,exprY,0,0);
        return(i);
        }

static int op_clab()  /* MAT CLABELS FROM A TO B */
        {
        int i;
        char *p;

        if (g<6)
            {
            if (g<5) { parm_error(); return(-1); }
            i=load_Y(word[4]); if (i<0) return(-1);
            if (muste_strnicmp(word[2],"NUM(",4)==0)
                {
                p=strchr(word[2],')'); if (p!=NULL) *p=EOS;
                numlab2(clabY,nY,lcY,arit_atoi(word[2]+4));
                }
            else
                text_labels(clabY,nY,lcY,word[2]);
            i=mat_save(word[4],Y,mY,nY,rlabY,clabY,lrY,lcY,-1,exprY,0,0);
            return(i);
            }
        i=load_X(word[3]); if (i<0) return(-1);
        i=load_Y(word[5]); if (i<0) return(-1);
        if (nX!=nY) { dim_error(); return(-1); }
        i=mat_save(word[5],Y,mY,nY,rlabY,clabX,lrY,lcX,-1,exprY,0,0);
        return(i);
        }


static int op_inv()
        {
        int i;
        double det;
        char detsana[64];

        i=load_X(word[1]); if (i<0) return(-1);
        if (mX!=nX) { dim_error(); return(-1); }
        i=varaa_tila(&T,mX,mX,NULL,NULL,0,0);
        if (i<0) return(-1);
        mat_inv(T,X,mX,&det);

        strcpy(tnimi,"INV("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);
        i=mat_save(tulos,T,mX,mX,clabX,rlabX,lcX,lrX,-1,exprX,0,0);
        if (g>1)
            {
            sprintf(detsana," %s=%.*g",word[2],accuracy,det);  /* 20.2.1994 */
            strcat(tnimi,detsana);
            nim(tnimi,exprX);
            }
        merkitse(tulos,exprX,i,mX,mX);
        return(1);
        }

static int op_mp_inv()
        {
        int i;
        double eps;
// RS REM        extern double arit_atof();

        i=load_X(word[1]); if (i<0) return(-1);
        i=varaa_tila(&T,nX,mX,NULL,NULL,0,0);
        if (i<0) return(-1);

        eps=0.0; // muutetaan luvuksi d(max)*1e-15;
        if (g>1) eps=arit_atof(word[2]);
        else
            {
            if (!sp_read) mat_spec_read(r1+r-1);
            i=spfind("EPS"); if (i>=0) eps=atof(spb[i]);
            }

        mat_mp_inv(T,X,mX,nX,eps);

        strcpy(tnimi,"MPINV("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);
        i=mat_save(tulos,T,nX,mX,clabX,rlabX,lcX,lrX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,nX,mX);
        return(1);
        }

static int op_dinv()
        {
        int i,j;
        double eps=1e-15;
// RS REM        extern double arit_atof();

        i=load_X(word[1]); if (i<0) return(-1);
        if (mX!=nX) { dim_error(); return(-1); }
        if (g>1) eps=arit_atof(word[2]);

        for (i=0; i<mX; ++i)
            {
            if (fabs(X[i+mX*i])>eps) X[i+mX*i]=1/X[i+mX*i];
            else X[i+mX*i]=0.0;
            for (j=0; j<mX; ++j) if (i!=j) X[i+mX*j]=0.0;
            }

        strcpy(tnimi,"DINV("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,X,mX,mX,clabX,rlabX,lcX,lrX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,mX);
        return(1);
        }


static int lyh_nimi(char *y,char *p1,int i1,int pit)
        {
        char x[9];
        int i,j;

        i=0; while (i<i1 && p1[i]==' ') ++i;
        j=i1-1; while (j>0 && p1[j]==' ') --j;
        *y=EOS; strncat(y,p1+i,j-i+1); y[pit]=EOS;
        return(strlen(y));
        }

static int kr_nimi(char *x,char *p1,int i1,char *p2,int i2)

        {
        int i;
        char y1[17]; // ylipitkÑ 13.4.2005
        char y2[17];

        lyh_nimi(y1,p1,i1,3);
        lyh_nimi(y2,p2,i2,3);
        sprintf(x,"%s*%s",y1,y2);
        for (i=strlen(x); i<8; ++i) x[i]=' ';
        x[8]=EOS;
        return(1);
        }

static int op_kronecker()
        {
        int i,j;
        char x[LNAME];

        i=load_X(word[1]); if (i<0) return(-1);
        i=load_Y(word[2]); if (i<0) return(-1);
        i=varaa_tila(&T,mX*mY,nX*nY,&rlabT,&clabT,lrY,lcY);
        if (i<0) return(-1);

        mat_kronecker(T,X,Y,mX,nX,mY,nY);

        for (i=0; i<mX*mY*lrY; ++i) rlabT[i]='r';
        for (i=0; i<nX*nY*lcY; ++i) clabT[i]='c';
        for (i=0; i<mX; ++i) for (j=0; j<mY; ++j)
            {
            kr_nimi(x,rlabX+i*lrX,lrX,rlabY+j*lrY,lrY);
            strncpy(rlabT+8*(i*mY+j),x,8);
            }

        for (i=0; i<nX; ++i) for (j=0; j<nY; ++j)
            {
            kr_nimi(x,clabX+i*lcX,lcX,clabY+j*lcY,lcY);
            strncpy(clabT+8*(i*nY+j),x,8);
            }

        strcpy(tnimi,"KRONECKER("); strcat(tnimi,exprX);
        strcat(tnimi,","); strcat(tnimi,exprY); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,mX*mY,nX*nY,rlabT,clabT,8,8,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX*mY,nX*nY);

        return(1);
        }

static int op_mtm()
        {
        char *suluin();
        char expr1[LLENGTH];
        int i;

        i=load_X(word[1]); if (i<0) return(-1);
        i=varaa_tila(&T,nX,nX,NULL,NULL,0,0);
        if (i<0) return(-1);
        mat_mtm(T,X,mX,nX);

        strcpy(tnimi,suluin(exprX,2,expr1)); strcat(tnimi,"'*");
        strcat(tnimi,suluin(exprX,1,expr1));
        nim(tnimi,exprX);

        i=mat_save(tulos,T,nX,nX,clabX,clabX,lcX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,nX,nX);
        return(1);
        }

static int op_mmt()
        {
        int i;
        char *suluin();
        char expr1[LLENGTH];

        /* change A*A' to (A')'*A' (mtm to mmt)  2.6.1999 */
        strcpy(expr1,word[1]); i=strlen(expr1)-1;
        if (expr1[i]=='\'') expr1[i]=EOS; else strcat(expr1,"'");
        /* read A' */
        i=load_X(expr1); if (i<0) return(-1);
        i=varaa_tila(&T,nX,nX,NULL,NULL,0,0);
        if (i<0) return(-1);
        mat_mtm(T,X,mX,nX);

        /* 2.6.1999 */
        i=strlen(exprX)-1; if (exprX[i]=='\'') exprX[i]=EOS;
        else strcat(exprX,"'");

        strcpy(tnimi,suluin(exprX,1,expr1)); strcat(tnimi,"*");
        strcat(tnimi,suluin(exprX,2,expr1)); strcat(tnimi,"'");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,nX,nX,clabX,clabX,lcX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,nX,nX);
        return(1);

/****************
        i=load_X(word[1]); if (i<0) return(-1);
        i=varaa_tila(&T,mX,mX,NULL,NULL,0,0);
        if (i<0) return(-1);
        mat_mmt(T,X,mX,nX);
        strcpy(tnimi,suluin(exprX,1,expr1)); strcat(tnimi,"*");
        strcat(tnimi,suluin(exprX,2,expr1)); strcat(tnimi,"'");
        nim(tnimi,exprX);
        i=mat_save(tulos,T,mX,mX,rlabX,rlabX,lrX,lrX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,mX);
        return(1);
***************/
        }

static int op_mtm2()
        {
        int i;
        char *suluin();
        char expr1[LLENGTH];

        i=load_X(word[1]); if (i<0) return(-1);
        i=load_Y(word[2]); if (i<0) return(-1);
        if (mX!=mY) { dim_error(); return(-1); }

        i=varaa_tila(&T,nX,nY,NULL,NULL,0,0);
        if (i<0) return(-1);
        mat_2mtm(T,X,Y,mX,nX,nY);

        strcpy(tnimi,suluin(exprX,2,expr1)); strcat(tnimi,"'*");
        strcat(tnimi,suluin(exprY,1,expr1));
        nim(tnimi,exprX);

        i=mat_save(tulos,T,nX,nY,clabX,clabY,lcX,lcY,-1,exprX,0,0);
        merkitse(tulos,exprX,i,nX,nY);
        return(1);
        }

static int op_mmt2()
        {
        int i;
        char *suluin();
        char expr1[LLENGTH];

        i=load_X(word[1]); if (i<0) return(-1);
        i=load_Y(word[2]); if (i<0) return(-1);
        if (nX!=nY) { dim_error(); return(-1); }

        i=varaa_tila(&T,mX,mY,NULL,NULL,0,0);
        if (i<0) return(-1);
        mat_2mmt(T,X,Y,mX,nX,mY);

        strcpy(tnimi,suluin(exprX,1,expr1)); strcat(tnimi,"*");
        strcat(tnimi,suluin(exprY,1,expr1)); strcat(tnimi,"'");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,mX,mY,rlabX,rlabY,lrX,lrY,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,mY);
        return(1);
        }

static int op_center()
        {
        int i;

        i=load_X(word[1]); if (i<0) return(-1);
        i=varaa_tila(&T,1,nX,NULL,NULL,0,0);
        if (i<0) return(-1);

        mat_center(T,X,mX,nX);

        strcpy(tnimi,"MEAN("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprT);

        i=mat_save("MEAN",T,1,nX,"Mean    ",clabX,lrX,lcX,-1,exprT,0,0);

        strcpy(tnimi,"CENTER("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int op_nrm()
        {
        int i;

        i=load_X(word[1]); if (i<0) return(-1);
        i=varaa_tila(&T,1,nX,NULL,NULL,0,0);
        if (i<0) return(-1);
        mat_nrm(T,X,mX,nX);

        strcpy(tnimi,"NORM("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprT);

        i=mat_save("NORM",T,1,nX,"Norm    ",clabX,lrX,lcX,-1,exprT,0,0);

        strcpy(tnimi,"NRM("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int op_chol()
        {
        int i;
        double eps;
// RS REM        extern double arit_atof();

        i=load_X(word[1]); if (i<0) return(-1);
        if (mX!=nX) { dim_error(); return(-1); }
        i=varaa_tila(&T,mX,mX,NULL,NULL,0,0);
        if (i<0) return(-1);

        eps=0.0; if (g>1) eps=arit_atof(word[2]);
        i=mat_chol2(T,X,mX,eps); /* 9.6.1995 nX removed! */
        if (i<=0)
            {
            sprintf(sbuf,"Error: Linear dependence in %s, row/col %d",word[1],-i+1);
            kirjoita_ilmoitus(sbuf);
            return(-1);

/*          sprintf(sbuf,"\nError in CHOL, row %d",-i+1);
            sur_print(sbuf); WAIT; return(-1);
*/
            }

        strcpy(tnimi,"CHOL("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,mX,mX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int op_max_min(int laji) /* 1=MAX 2=MIN */
        {
        int i,j;
        double u;
        char s[9];

        i=load_X(word[1]); if (i<0) return(-1);
        i=varaa_tila(&T,1,nX,NULL,NULL,0,0);
        if (i<0) return(-1);

        for (j=0; j<nX; ++j)
            {
            u=X[mX*j];
            for (i=0; i<mX; ++i)
                {
                if (laji==1) { if (X[i+mX*j]>u) u=X[i+mX*j]; }
                else         { if (X[i+mX*j]<u) u=X[i+mX*j]; }
                }
            T[j]=u;
            }
        if (laji==1) { strcpy(tnimi,"MAX("); strcpy(s,"Max     "); }
        else         { strcpy(tnimi,"MIN("); strcpy(s,"Min     "); }

        strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,1,nX,s,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,1,nX);
        return(1);
        }

static int op_max_min_ij(int laji) /* 1=MAX 2=MIN */
        {
        int i,j,i_m,j_m;
        double u;
        char s[9];
        char t[25];

        i=load_X(word[1]); if (i<0) return(-1);
        i=varaa_tila(&T,1,1,NULL,NULL,0,0);
        if (i<0) return(-1);

        u=X[0]; i_m=j_m=0;
        for (j=0; j<nX; ++j)
            {
            for (i=0; i<mX; ++i)
                {
                if (laji==1) { if (X[i+mX*j]>u) { u=X[i+mX*j]; i_m=i; j_m=j; } }
                else         { if (X[i+mX*j]<u) { u=X[i+mX*j]; i_m=i; j_m=j; } }
                }
            }
        T[0]=u;

        if (laji==1) strcpy(tnimi,"MAX_IJ(");
        else         strcpy(tnimi,"MIN_IJ(");

        strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,1,1,rlabX+i_m*lrX,clabX+j_m*lcX,8,8,-1,exprX,0,0);
        merkitse(tulos,exprX,i,1,1);
        return(1);
        }

static int op_sum()
        {
        int i,j,k;
        double a;

        i=load_X(word[1]); if (i<0) return(-1);
        i=varaa_tila(&T,1,nX,NULL,NULL,0,0);
        if (i<0) return(-1);

        if (g==1) k=1; else k=atoi(word[2]);

        if (k==1)
            mat_sum(T,X,mX,nX);
        else
            {
            for (j=0; j<nX; ++j)
                {
                a=0.0;
                for (i=0; i<mX; ++i)
                    {
                    a+=pow(X[i+mX*j],(double)k);
                    }
                T[j]=a;
                }
            }

        strcpy(tnimi,"SUM("); strcat(tnimi,exprX);
        if (k!=1)
            {
            strcat(tnimi,","); strcat(tnimi,word[2]);
            }
        strcat(tnimi,")");
        nim(tnimi,exprX);

        strcpy(sbuf,"Sum     ");
        if (k!=1) sbuf[3]=*word[2];
        i=mat_save(tulos,T,1,nX,sbuf,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,1,nX);
        return(1);
        }

static int op_product()
        {
        int i,j,h;
        double a;

        i=load_X(word[1]); if (i<0) return(-1);
        i=varaa_tila(&T,1,nX,NULL,NULL,0,0);
        if (i<0) return(-1);

        for (j=0; j<nX; ++j)
            {
            i=mX*j; a=X[i];
            for (h=1; h<mX; ++h) a*=X[++i];
            T[j]=a;
            }
        strcpy(tnimi,"PROD("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,1,nX,"Product ",clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,1,nX);
        return(1);
        }

static int op_cum(int k) /* 1=CUM 2=UNCUM */
        {
        int i,j,h;
        double a;

        i=load_X(word[1]); if (i<0) return(-1);
        if (k==1)
            for (j=0; j<nX; ++j)
                {
                i=mX*j; a=X[i];
                for (h=1; h<mX; ++h) { a+=X[++i]; X[i]=a; }
                }
        else
            for (j=0; j<nX; ++j)
                {
                i=mX*(j+1)-1;
                for (h=1; h<mX; ++h) { X[i]-=X[i-1]; --i; }
                }

        *tnimi=EOS; if (k!=1) strcat(tnimi,"UN");
        strcat(tnimi,"CUM("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int op_select()
        {
        int i,j,k,h,jj;
        double a;

        i=load_X(word[1]); if (i<0) return(-1);
        j=arit_atoi(word[2]);
        k=0; for (i=0; i<mX; ++i) if (X[i+mX*(j-1)]!=0.0) ++k;
        if (k==0) return(1);
        i=varaa_tila(&T,k,nX,NULL,NULL,0,0);
        if (i<0) return(-1);

        h=0;
        for (i=0; i<mX; ++i)
            {
            if (X[i+mX*(j-1)]==0.0) continue;
            for (jj=0; jj<nX; ++jj) T[h+k*jj]=X[i+mX*jj];
            strncpy(rlabX+lrX*h,rlabX+lrX*i,lrX);
            ++h;
            }
        strcpy(tnimi,"SELECT("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,k,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,k,nX);
        return(1);
        }

static int op_null()
    {
    int i;
    double eps;

    i=load_X(word[1]); if (i<0) return(-1);

    if (!sp_read) mat_spec_read(r1+r-1);
    eps=1e-15;
    i=spfind("EPS"); if (i>=0) eps=atof(spb[i]);

    mat_null_space(&nT,X,mX,nX,eps);

    strcpy(tnimi,"NULL("); strcat(tnimi,exprX); strcat(tnimi,")");
    nim(tnimi,exprX);

    numlab(rlabX,nT,lcX);
    mat_save(tulos,X,nX,nT,clabX,rlabX,lcX,lcX,-1,exprX,0,0);

    return(1);
    }

static int op_basis()
    {
    int i;
    double eps;

    i=load_X(word[1]); if (i<0) return(-1);

    if (!sp_read) mat_spec_read(r1+r-1);
    eps=1e-15;
    i=spfind("EPS"); if (i>=0) eps=atof(spb[i]);

    mat_column_space(&nT,X,mX,nX,eps);

    strcpy(tnimi,"BASIS("); strcat(tnimi,exprX); strcat(tnimi,")");
    nim(tnimi,exprX);

    numlab(clabX,nT,lcX);
    mat_save(tulos,X,mX,nT,rlabX,clabX,lrX,lcX,-1,exprX,0,0);

    return(1);
    }

static int op_det(int ind)
    {
    int i;
    double det;

    i=load_X(word[1]); if (i<0) return(-1);

    if (mX!=nX)
        {
        sprintf(sbuf,"Error: %s not a square matrix!",word[1]);
        kirjoita_ilmoitus(sbuf);
        return(-1);
        }

    if (ind==1)
        {
// 4.1.2011
        i=varaa_tila(&T,mX,mX,NULL,NULL,0,0);
        if (i<0) return(-1);
        mat_inv(T,X,mX,&det);
        }
//      mat_det(X,mX,0,&det);
    else
        {
        i=mat_logdet(X,mX,&det);
        if (i<0)
            {
            kirjoita_ilmoitus("Error: Negative determinant!");
            return(-1);
            }
        }
    X[0]=det;

    *tnimi=EOS; if (ind==2) strcat(tnimi,"L");
    strcat(tnimi,"DET("); strcat(tnimi,exprX); strcat(tnimi,")");
    nim(tnimi,exprX);

    strcpy(rlabX,"det     ");
    mat_save(tulos,X,1,1,rlabX,rlabX,8,8,-1,exprX,0,0);

    return(1);
    }

static int op_rank()
    {
    int i;
    double eps;

    i=load_X(word[1]); if (i<0) return(-1);

    if (!sp_read) mat_spec_read(r1+r-1);
    eps=1e-15;
    i=spfind("EPS"); if (i>=0) eps=atof(spb[i]);

    mat_svd_rank(X,mX,nX,eps);

    strcpy(tnimi,"RANK("); strcat(tnimi,exprX); strcat(tnimi,")");
    nim(tnimi,exprX);

    strcpy(rlabX,"rank    ");
    mat_save(tulos,X,1,1,rlabX,rlabX,8,8,-1,exprX,0,0);

    return(1);
    }

static int op_trace1()
    {
    int i;
    double tr;

    i=load_X(word[1]); if (i<0) return(-1);

    i=0; tr=0.0; while (i<mX && i<nX) { tr+=X[i+mX*i]; ++i; }
    X[0]=tr;

    strcpy(tnimi,"TRACE("); strcat(tnimi,exprX); strcat(tnimi,")");
    nim(tnimi,exprX);

    strcpy(rlabX,"trace   ");
    mat_save(tulos,X,1,1,rlabX,rlabX,8,8,-1,exprX,0,0);

    return(1);
    }


static int op_vec(int type) // 1=labels copied 0=not
        {
        int i,k;
        int n;
        char *p;

        i=load_X(word[1]); if (i<0) return(-1);

        k=mX*nX; if (g>1) k=arit_atoi(word[2]);
        n=mX*nX/k;
        if (k*n!=mX*nX) { dim_error(); return(-1); }
        i=varaa_tila(&T,k,n,&rlabT,&clabT,lrX,lcX);
        if (i<0) return(-1);
        if (type==1)
            {
            for (i=0; i<nX; ++i)
                {
                strncpy(rlabT+i*mX*lrX,rlabX,mX*lrX);
                }
            }
        else numlab(rlabT,k,lrX);
        numlab(clabT,n,lcX);

        strcpy(tnimi,"VEC("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,X,k,n,rlabT,clabT,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,k,n);
        return(1);
        }

static void sort_perm(double *x,int m)
        {
        int i,h,k,ii;
        int ind;
        double y;

        xi=(int *)malloc(m*sizeof(int));
        if (xi==NULL) { not_enough_memory(); exit(0); }
        for (i=0; i<m; ++i) xi[i]=i+1;

        h=m;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind=1;
                for (k=0; k<m-h; ++k)
                    {
                    if (x[k]>x[k+h])
                        {
                        y=x[k]; x[k]=x[k+h]; x[k+h]=y;
                        ii=xi[k]; xi[k]=xi[k+h]; xi[k+h]=ii;
                        ind=0;
                        }
                    }
                if (ind==1) break;
                }
            }
        for (i=0; i<m; ++i) x[i]=xi[i];
        free(xi);
        }


static int op_perm(int mode)
        {
        int i,j;
        int k,m,n;
        double min;

        i=load_X(word[1]); if (i<0) return(-1);
        i=load_Y(word[2]); if (i<0) return(-1);
        if (mY==1) k=1; else if (nY==1) k=0;
        else { dim_error(); return(-1); }
        if ( (k && nY!=nX) || (!k && mY!=mX) ) { dim_error(); return(-1); }
        i=varaa_tila(&T,mX,nX,&rlabT,&clabT,lrX,lcX);
        if (i<0) return(-1);

        if (mode==0)
            {
            for (i=0; i<mX*nX; ++i) T[i]=0.0;
            }

        if (mode)
            {
            if (k) n=nX; else n=mX;
            sort_perm(Y,n);  /* Y values -> order indices (1,2,...,n) */
/*
printf("\n");
for (i=0; i<n; ++i) printf("%g ",Y[i]); printf("\n"); sur_getch();
*/
            }

        if (k)
            {
            for (j=0; j<nX; ++j)
                {
                m=Y[j]-1;
                if (mode<2)   /* 24.1.1999 */
                    {
                    for (i=0; i<mX; ++i) T[i+mX*j]=X[i+mX*m];
                    for (i=0; i<lcX; ++i) clabT[j*lcX+i]=clabX[m*lcX+i];
                    }
                else
                    {
                    for (i=0; i<mX; ++i) T[i+mX*m]=X[i+mX*j];
                    for (i=0; i<lcX; ++i) clabT[m*lcX+i]=clabX[j*lcX+i];
                    }
                }
            for (i=0; i<lrX*mX; ++i) rlabT[i]=rlabX[i];
            }
        else
            {
            for (i=0; i<mX; ++i)
                {
                m=Y[i]-1;
                if (mode<2)
                    {
                    for (j=0; j<nX; ++j) T[i+mX*j]=X[m+mX*j];
                    for (j=0; j<lcX; ++j) rlabT[i*lrX+j]=rlabX[m*lrX+j];
                    }
                else
                    {
                    for (j=0; j<nX; ++j) T[m+mX*j]=X[i+mX*j];
                    for (j=0; j<lcX; ++j) rlabT[m*lrX+j]=rlabX[i*lrX+j];
                    }
                }
            for (j=0; j<lcX*nX; ++j) clabT[j]=clabX[j];
            }
        strcpy(tnimi,"PERM");
        if (mode==2) strcat(tnimi,"2");
        else if (mode==0) strcat(tnimi,"0");
        strcat(tnimi,"(");
        strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,T,mX,nX,rlabT,clabT,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

static int op_p()
        {
        int i;
        char *p;
        int k,k1,k2;

        i=load_X(word[1]); if (i<0) return(-1);
        if (mX!=nX) { dim_error(); return(-1); }
        if (g<2) { parm_error(); return(-1); }
        p=strchr(word[2],':');
        if (p==NULL) { k1=k2=arit_atoi(word[2]); }
        else
            {
            *p=EOS; k1=arit_atoi(word[2]);
            k2=arit_atoi(p+1);
            if (k1<1 || k1>k2 || k2>mX) { parm_error(); return(-1); }
            }

        for (k=k1-1; k<k2; ++k)
            {
            mat_p(X,mX,k);
            }

        strcpy(tnimi,"P("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprX);

        i=mat_save(tulos,X,mX,mX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        merkitse(tulos,exprX,i,mX,nX);
        return(1);
        }

/*
MAT SPECTRAL DECOMPOSITION OF X TO T,Y
    1        2             3  4 5  6 7
                                   S,L
*/

static int op_spectral()
        {
        int i;
        double eps,tol;

        i=load_X(word[4]); if (i<0) return(-1);
        if (mX!=nX) { dim_error(); return(-1); }
        i=varaa_tila(&Y,nX,1,NULL,NULL,0,0);
        if (i<0) return(-1);
/*      i=varaa_tila(&T,nX,nX,NULL,NULL,0,0);
        if (i<0) return(-1);
*/
/*      mat_child("CSPECT");  */

        if (strcmp(word[3],"of")==0) i=1; else i=2;
        eps=1e-16; tol=1e-300/eps;
        d=Y;
        e=(double *)malloc(mX*sizeof(double));
        if (e==NULL) { not_enough_memory(); return(-1); }
/****************
mat_treb(double *a, int n, double *d, double *e)
mat_tqlb(double *d, double *e, int n, double *z)
******************/
        if (i==1)
            {
            mat_treb(X,mX,d,e);
            mat_tqlb(d,e,mX,X);
            }
        else
            {
            mat_tred2(d,e,X,mX,tol);
            mat_tql2(d,e,X,mX,eps,30);
            }
        strcpy(tnimi,"S("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
/*      numlab(clabX,nX,8);    */
        text_labels(clabX,nX,8,"ev");

        mat_save(word[6],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprY,0,0);

        strcpy(tnimi,"L("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
        mat_save(word[7],Y,nX,1,clabX,"eigenval",lrX,lcX,-1,exprY,0,0);
        return(1);
        }

/*
MAT GRAM-SCHMIDT DECOMPOSITION OF X TO T,Y,tol
    1            2             3  4 5  6 7 8
                                       S,U
*/

static int op_gram_schmidt()
        {
        int i,j,h;
        double tol;
        double a;

        i=load_X(word[4]); if (i<0) return(-1);
        if (mX<nX) { dim_error(); return(-1); }
        i=varaa_tila(&T,mX,nX,NULL,NULL,0,0);
        if (i<0) return(-1);
        i=varaa_tila(&Y,nX,nX,NULL,NULL,0,0);
        if (i<0) return(-1);
        tol=1e-15; if (g>8) tol=atof(word[8]);


        j=mat_gram_schmidt(T,Y,X,mX,nX,tol);
        if (j<=0)
            {
            erun=0;
            if (j==0)
                {
                sur_print("\nFirst column=0!");
                WAIT; return(-1);
                }
            j=-j;
            for (i=j-1; i>=0; --i)
                {
                a=Y[i+nX*j];
                for (h=i+1; h<=j-1; ++h) a-=Y[i+nX*h]*T[h];
                T[i]=a/Y[i*(nX+1)];
                }
            T[j]=-1.0; for (i=j+1; i<mX; ++i) T[i]=0.0;

            strcpy(tnimi,"Linear_dependence_in_"); strcat(tnimi,exprX);
            nim(tnimi,exprT);
            mat_save(word[6],T,nX,1,clabX,"Coeff   ",lcX,8,-1,exprT,0,0);
            sur_print("\nColumns linearly dependent.");
            sprintf(sbuf,"\nCoefficients saved in %s",word[6]);
            sur_print(sbuf); WAIT; return(-1);
            }

        strcpy(tnimi,"GS("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprT);
        mat_save(word[6],T,mX,nX,rlabX,clabX,lrX,lcX,-1,exprT,0,0);

        strcpy(tnimi,"GU("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
        mat_save(word[7],Y,nX,nX,clabX,clabX,lcX,lrX,-1,exprY,0,0);
        return(1);
        }

static int read_row_comment1(char *x)
    {
    int i;
    char *p;

    p=strstr(x," / ");
    if (p!=NULL)
        {
        ++n_row_comments;
        p+=3;
        i=strlen(p)-1; while (i>0 && p[i]==' ') --i;
        len_row_comments+=i+2+11;
                           // 11 for row label  "/12345678: "
        }
    return(1);
    }

static int read_row_comment2(char *x)
    {
    int i;
    char *p;
    char y[LLENGTH],*sy[1];

    p=strstr(x," / ");
    if (p==NULL) return(1);
    p+=3;
    strcpy(y,x+1);
    split(y,sy,1);
    sprintf(sbuf,"/%s: ",sy[0]);
    strcpy(p_com,sbuf);
    i=strlen(p)-1; while (i>0 && p[i]==' ') p[i--]=EOS;
    strcat(p_com,p),
// printf("\np=%s|",p); sur_getch();
    p_com+=strlen(sbuf)+i+2;
    return(1);
    }


/* MAT SAVE <name> [AS <filename>]
        1      2    3     4
*/
static int op_save()
        {
        extern double arit_atof();
        int i,j,k,len;
        int matnimi, matrivi;
        char x[LLENGTH], *sana[MAXCOL];
        int ots, arivi, jmax;
        int maxrlab, kesken;
        char *p;
        double dbl; /* compiler-virheen takia */
        int min_dim;

        if (g<3)
            {
            PR_EBLD;
            sur_print("\nCorrect form: MAT SAVE <name> [AS <file>]");
            WAIT; PR_ENRM;
            return(-1);
            }

//  MAT SAVE DATA ... tulkittu erikseen (so.c) 29.2.2000 // RS Back here!
      if (muste_strcmpi(word[2],"DATA")==0)
          { i=muste_matsda(); return(i); } // RS CHA save_data -> matsda

        if (g>=5) matnimi=4; else matnimi=2;

        matrivi=wfind("MATRIX",word[2],1);

        if (matrivi<0)
            {
            PR_EBLD;
            sprintf(sbuf,"\nMatrix %s not in the edit field!",word[2]);
            sur_print(sbuf); WAIT; PR_ENRM; return(-1);
            }
        edread(x,matrivi);
        i=split(x,sana,3);
        if ( i>2 && strncmp(sana[2],"///",3)==0)
            { ots=0; arivi=matrivi+1; }
        else
            {
            ots=1;
            j=matrivi;
            do  {
                ++j;
                edread(x,j);
                i=split(x+1,sana,1);
                }
            while ( strncmp(sana[0],"///",3)!=0 && j<r2 );
            if (j>=r2) return(-1);
            arivi=j+1;
            }

        if (ots)
            {
            edread(x,arivi-1);
            p=strstr(x," / "); if (p!=NULL) *p=EOS;  /* 30.6.1998 */
            i=split(x+1,sana,MAXCOL);
            nX=i-1;
            }
        else
            {
            edread(x,arivi);
            p=strstr(x," / "); if (p!=NULL) *p=EOS;  /* 30.6.1998 */
            nX=split(x+1,sana,MAXCOL);
            }

        maxrlab=8;
        j=arivi;
        while (1)
            {
            edread(x,j);
            read_row_comment1(x); // 29.1.2005

            i=split(x+1,sana,1);
            if (i==0 || muste_strcmpi(sana[0],"END")==0 || j==r2) break;
        /*     toistaiseksi maxrlab=8
            if (strlen(sana[0])>maxrlab) maxrlab=strlen(sana[0]);
        */
            ++j;
            }

// printf("\nn_com=%d len=%ld|",n_row_comments,len_row_comments); sur_getch();
        row_comments=malloc(len_row_comments);
        p_com=row_comments;

        mX=j-arivi; jmax=j-1;

        if (n_row_comments)
            {
            for (i=arivi; i<arivi+mX; ++i)
                {
                edread(x,i);
                read_row_comment2(x); // 29.1.2005
                }
            }

// printf("\ncom: %s|",row_comments); sur_getch();
/*************************************************
  p_com=row_comments;
  for (i=0; i<n_row_comments; ++i)
    {
    strcpy(sbuf,p_com); // printf("\n%s|",sbuf); sur_getch();
    p_com+=strlen(sbuf)+1;
    }
**************************************************/

        lrX=MAXRLAB; lcX=MAXCLAB;
        i=varaa_tila(&X,mX,nX,&rlabX,&clabX,lrX,lcX);
        if (i<0) return(-1);
        numlab(rlabX,mX,lrX);
        numlab(clabX,nX,lcX);

        if (ots)
            {
            edread(x,arivi-1);
            i=split(x+1,sana,MAXCOL);
            for (j=0; j<nX; ++j)
                {
                if (atoi(sana[j+1])!=j+1)
                   {
                   p=sana[j+1]; len=strlen(p);
                   min_dim=len; if (lcX<len) min_dim=lcX;
                   for (i=0; i<min_dim; ++i, ++p) clabX[j*lcX+i]=*p;
                   for (i=len; i<lcX; ++i) clabX[j*lcX+i]=' ';
                   }
                }
            }

        for (k=arivi, i=0; k<=jmax; ++k, ++i)
            {
            edread(x,k);
            p=strstr(x," / "); if (p!=NULL) *p=EOS;  /* 30.6.1998 */
            j=split(x+1,sana,MAXCOL);
            if (j<nX+ots)
                {
                PR_EBLD;
                sprintf(sbuf,"\nIncomplete row (edit line %d) in matrix!",k);
                sur_print(sbuf); WAIT; PR_ENRM; return(-1);
                }
            if (ots && atoi(sana[0])!=i+1)
                {
                p=sana[0]; len=strlen(p);
                min_dim=len; if (lrX<len) min_dim=lrX;
                for (j=0; j<min_dim; ++j, ++p) rlabX[i*lrX+j]=*p;
                for (j=len; j<lrX; ++j) rlabX[i*lrX+j]=' ';
                }
            for (j=0; j<nX; ++j)
                {
                dbl=arit_atof(sana[j+ots]);   /* compiler-virheen takia */
                X[i+mX*j]=dbl;
                }
            }

        if (arivi-matrivi-2>0)
                {
                edread(x,arivi-2);
                i=split(x+1,sana,1);
                if (i)
                    strcpy(exprX,sana[0]);
                else
                    strcpy(exprX,word[matnimi]);
                }
        else strcpy(exprX,word[matnimi]);


        if (n_row_comments) // 29.1.2005
            mat_save_rem(word[matnimi],X,mX,nX,rlabX,clabX,
                     lrX,lcX,-1,exprX,arivi-matrivi-3,matrivi+1,
                     n_row_comments,row_comments);
        else
            mat_save(word[matnimi],X,mX,nX,rlabX,clabX,
                     lrX,lcX,-1,exprX,arivi-matrivi-3,matrivi+1);
                                    // poistettu viim.param. 1

        return(1);
        }


/*
MAT SOLVE X FROM A*X=B
    1     2 3    4
          T      X   Y
*/

static int op_solve()
        {
        int i,j;
        char yhtalo[LLENGTH];
        char *p,*q;
        double eps;

        if (g<5)
            {
            sur_print("\nUsage: MAT SOLVE X FROM A*X=B");
            WAIT; erun=0; return(-1);
            }
        strcpy(yhtalo,word[4]);
        p=strchr(word[4],'*'); if (p==NULL) { parm_error(); return(-1); }
        *p=EOS; ++p;
        q=strchr(p,'='); if (q==NULL) { parm_error(); return(-1); }
        *q=EOS; ++q;
        if (strcmp(word[2],p)!=0) { parm_error(); return(-1); }

        i=load_X(word[4]); if (i<0) return(-1);
        if (strcmp(q,"I")==0)
            {
            mY=nY=mX; lcY=8;
            i=varaa_tila(&Y,mY,nY,&rlabY,&clabY,8,8);
            if (i<0) return(-1);
            numlab(clabY,mY,lcY);
            for (i=0; i<mY; ++i)
                { for (j=0; j<mY; ++j) Y[i+mY*j]=0.0; Y[i*(mY+1)]=1.0; }
            }
        else
            { i=load_Y(q); if (i<0) return(-1); }
        i=varaa_tila(&T,nX,nY,NULL,NULL,0,0);
        if (i<0) return(-1);

/*      mat_child("CSOLVE");  */

        if (mX!=mY)
            {
            sur_print("\nNot same # of rows in matrices A,B (A*X=B)");
            WAIT; return(-1);
            }
        if (mX<nX)
            {
            sur_print("\nNot enough equations (rows)!");
            WAIT; return(-1);
            }

        eps=1e-15;
        mat_solve(T,X,Y,mX,nX,nY,eps);

        strcpy(tnimi,"(");
        strcat(tnimi,word[2]); strcat(tnimi,"_from_"); strcat(tnimi,yhtalo);
        strcat(tnimi,")");
        nim(tnimi,exprT);
        mat_save(word[2],T,nX,nY,clabX,clabY,lcX,lcY,-1,exprT,0,0);
        return(1);
        }

/*
MAT SINGULARVALUE DECOMPOSITION OF X TO X,Y,T
    1             2             3  4 5  6 7 8
                                        U,D,V
also
MAT SVD OF X TO X,Y,T
OR
MAT SINGULAR VALUE DECOMPOSITION OF X TO X,Y,T
*/

static int op_svd()
        {
        int i;
        double eps,tol;

        if (muste_strcmpi(word[1],"SVD")==0) /* 26.5.1999 */
            {
            for (i=8; i>=2; --i) word[i]=word[i-1];
            }
        else if (muste_strnicmp(word[2],"VAL",3)==0)
            {
            for (i=2; i<=8; ++i) word[i]=word[i+1];
            }

        i=load_X(word[4]); if (i<0) return(-1);
        if (mX<nX) { dim_error(); return(-1); }
        i=varaa_tila(&Y,nX,1,NULL,NULL,0,0);
        if (i<0) return(-1);
        i=varaa_tila(&T,nX,nX,NULL,&clabT,0,8);
        if (i<0) return(-1);

/*      mat_child("CMATSVD");    */
        eps=1e-16; tol=1e-300/eps;
        mat_svd(X,Y,T,mX,nX,eps,tol);

        strcpy(tnimi,"Usvd("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
/*      numlab(clabT,nX,8);   3.11.1990    */
        text_labels(clabT,nX,8,"svd");

        mat_save(word[6],X,mX,nX,rlabX,clabT,lrX,lcX,-1,exprY,0,0);

        strcpy(tnimi,"Dsvd("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
        mat_save(word[7],Y,nX,1,clabT,"sing.val",lcX,lrX,-1,exprY,0,0);

        strcpy(tnimi,"Vsvd("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
        mat_save(word[8],T,nX,nX,clabX,clabT,lcX,lcX,-1,exprY,0,0);
        return(1);
        }


/*
MAT TRANSFORM X BY <expression X#>
    1         2 3   4
MAT TRANSFORM X BY Y AND <expression(X#,Y#)>
    1         2 3  4 5    6
*/

static int op_transform()
        {
        int i,j;
        char x[3],y[3];
        char ii[3],jj[3];
        unsigned int h;

        if (g!=5 && g!=7) { parm_error(); return(-1); }
        i=load_X(word[2]); if (i<0) return(-1);

        if (g==7)
            {
            i=load_Y(word[4]); if (i<0) return(-1);
            if (mX!=mY || nX!=nY) { dim_error(); return(-1); }
            }
        if (!sp_read) { i=sp_init(r1+r-1); sp_read=1; }
        strcpy(x,"X#"); strcpy(y,"Y#");
        strcpy(ii,"I#"); strcpy(jj,"J#");
        spa[spn]=x; spb[spn]=NULL; ++spn;
        spa[spn]=ii; spb[spn]=NULL; ++spn;
        spa[spn]=jj; spb[spn]=NULL; ++spn;

        if (g==7) { spa[spn]=y; spb[spn]=NULL; ++spn; }


        for (i=0; i<mX; ++i) for (j=0; j<nX; ++j)
            {
            h=i+mX*j;
            arvo[spn-2]=(double)(i+1); arvo[spn-1]=(double)(j+1);
            if (g==5) arvo[spn-3]=X[h];
            else { arvo[spn-4]=X[h]; arvo[spn-1]=Y[h]; }

            laske(word[g-1],&X[h]);
            if (l_virhe) return(-1); // 11.7.2005
            }
        spn-=3; if (g==7) --spn;

        strcpy(tnimi,"T("); strcat(tnimi,word[2]); strcat(tnimi,"_by_");
        strcat(tnimi,word[4]);
        if (g==7) { strcat(tnimi,"_and_"); strcat(tnimi,word[6]); }
        strcat(tnimi,")");

        nim(tnimi,exprX);
        mat_save(word[2],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        return(1);
        }

/*
MAT QR OF X TO T,Y,tol
    1  2  3 4  5 6 7

*/

static int op_qr()
        {
        int i;
        double tol;

        i=load_X(word[3]); if (i<0) return(-1);
        if (mX<nX) { dim_error(); return(-1); }

        i=varaa_tila(&Y,mX,mX,NULL,NULL,0,0);  /* Q */
        if (i<0) return(-1);

        tol=1e-15; if (g>7) tol=atof(word[7]);
        i=mat_qr(X,Y,mX,nX,tol);
        strcpy(tnimi,"R("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprT);
        mat_save(word[6],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprT,0,0);

        strcpy(tnimi,"Q("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
        mat_save(word[5],Y,mX,mX,rlabX,rlabX,lrX,lrX,-1,exprY,0,0);

        return(1);
        }

/*
MAT QRP OF X TO T,Y,tol
    1   2  3 4  5 6 7

*/

static int op_qrp()
        {
        int i,j,h;
        double tol;
        double a;
        int rank;
        int *piv; // RS CHA moved from global local

        i=load_X(word[3]); if (i<0) return(-1);
//      if (mX<nX) { dim_error(); return(-1); }
//      i=varaa_tila(&T,mX,nX,NULL,NULL,0,0);
//      if (i<0) return(-1);

        i=varaa_tila(&Y,mX,mX,NULL,NULL,0,0);  /* Q */
        if (i<0) return(-1);

        piv=(int *)malloc(nX*sizeof(int));
        if (piv==NULL) { not_enough_memory(); return(-1); } // RS CHA exit -> return

        tol=1e-15; if (g>7) tol=atof(word[7]);

        rank=mat_qrp(X,Y,piv,mX,nX,tol);

        strcpy(tnimi,"R("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprT);
        mat_save(word[6],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprT,0,0);

        strcpy(tnimi,"Q("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
        mat_save(word[5],Y,mX,mX,rlabX,rlabX,lrX,lrX,-1,exprY,0,0);
// printf("\nrank=%d\n",rank);
// printf("piv:");
// for (i=0; i<rank; ++i) printf(" %d",piv[i]); printf("\n"); sur_getch();

        for (i=0; i<nX; ++i) X[i]=i;
        for (i=0; i<rank; ++i)
            {
            a=X[i]; X[i]=X[piv[i]]; X[piv[i]]=a;
            }

        for (i=0; i<nX; ++i) Y[i]=0;
        for (i=0; i<rank; ++i) Y[(int)X[i]]=i+1;

// printf("Selected:");
// for (i=0; i<rank; ++i) printf(" %g",X[i]+1); printf("\n"); sur_getch();
        strcpy(tnimi,"QR_SEL("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
        sprintf(sbuf,"r=%d     ",rank);
        mat_save("QR_SEL.M",Y,1,nX,sbuf,clabX,8,lcX,-1,exprY,0,0);

        for (i=0; i<nX; ++i) ++X[i]; /* 0,1,2,... -> 1,2,3,... */
        strcpy(tnimi,"QR_PERM("); strcat(tnimi,exprX); strcat(tnimi,")");
        nim(tnimi,exprY);
        mat_save("QR_PERM.M",X,1,nX,"qr_perm ",clabX,8,lcX,-1,exprY,0,0);

        free(piv);
        return(1);
        }


static void submat_error(char *w2,char *w1)
        {
        sprintf(sbuf,"\nSelector vector %s not compatible with %s!",
                         w2,w1);
        sur_print(sbuf); WAIT;
        }

static void submat_err2(char *s)
        {
        sprintf(sbuf,"\nNo %s to be selected!",s);
        sur_print(sbuf); WAIT;
        }

static int make_selectors(double *Y,int n,int *sel)
        {
        int i,j,k,j_min;
        double y_min;

        k=0;
        for (i=0; i<n; ++i)
            {
            if (Y[i]!=0.0) ++k;
            else Y[i]=1e306;
            }
        if (k==0) return(0);
        for (i=0; i<k; ++i)
            {
            y_min=1e306; j_min=-1;
            for (j=0; j<n; ++j)
                {
                if (Y[j]<y_min) { y_min=Y[j]; j_min=j; }
                }
            sel[i]=j_min;
            Y[j_min]=1e306;
            }
        return(k);
        }

static void select_labels(
char *lab,
int m,
int lr,
int k,
int *sel,
char *ulab
)
        {
        int i,j;

        for (i=0; i<k; ++i)
            {
            for (j=0; j<lr; ++j) ulab[i*lr+j]=lab[sel[i]*lr+j];
            }
        }



static int op_submat()
        {
        char *suluin();
        char expr1[LLENGTH];
        int i,j,ii,jj;
        int all_rows,all_cols;
        int *r_sel,*c_sel; // RS CHA from local global
        char *r_lab,*c_lab; // RS CHA from local global

        if (g<3)
            {
            sur_print("\nUsage: MAT C=SUB(A,R_select,C_select)");
            WAIT; return(-1);
            }
        i=load_X(word[1]); if (i<0) return(-1);

        all_rows=all_cols=0;
        if (strcmp(word[2],"*")==0) { all_rows=1; mT=mX; }
        else
            {
            i=load_Y(word[2]); if (i<0) return(-1);
            if (mY==1) mY=nY;
            if (mY!=mX) { submat_error(word[2],word[1]); return(-1); }
            r_sel=(int *)malloc(mX*sizeof(int));
            if (r_sel==NULL) { not_enough_memory(); return(-1); }
            mT=make_selectors(Y,mX,r_sel);
            if (mT==0) { submat_err2("rows"); return(-1); }
            }

// for (i=0; i<mT; ++i) printf("%d ",r_sel[i]); printf("\n"); sur_getch();
// return(1);

        if (strcmp(word[3],"*")==0) { all_cols=1; nT=nX; }
        else
            {
            i=load_Y(word[3]); if (i<0) return(-1);
            if (nY==1) nY=mY;
            if (nY!=nX) { submat_error(word[3],word[1]); return(-1); }
            c_sel=(int *)malloc(nX*sizeof(int));
            if (c_sel==NULL) { not_enough_memory(); return(-1); }
            nT=make_selectors(Y,nX,c_sel);
            if (nT==0) { submat_err2("columns"); return(-1); }
            }

        i=varaa_tila(&T,mT,nT,NULL,NULL,0,0);
        if (i<0) return(-1);

        for (i=0; i<mT; ++i)
            {
            if (all_rows) ii=i; else ii=r_sel[i];
            for (j=0; j<nT; ++j)
                {
                if (all_cols) jj=j; else jj=c_sel[j];
                T[i+mT*j]=X[ii+mX*jj];
                }
            }
        if (all_rows) r_lab=rlabX;
        else
            {
            r_lab=malloc(mT*lrX);
            if (r_lab==NULL) { not_enough_memory(); return(-1); }
            select_labels(rlabX,mX,lrX,mT,r_sel,r_lab);
            }

        if (all_cols) c_lab=clabX;
        else
            {
            c_lab=malloc(nT*lcX);
            if (c_lab==NULL) { not_enough_memory(); return(-1); }
            select_labels(clabX,nX,lcX,nT,c_sel,c_lab);
            }
        sprintf(tnimi,"SUB(%s,%s,%s)",exprX,word[2],word[3]);
        nim(tnimi,exprX);

        i=mat_save(tulos,T,mT,nT,r_lab,c_lab,lrX,lcX,-1,exprX,0,0);
//      merkitse(tulos,exprX,i,nX,nX);
        free(c_lab); free(r_lab);
        free(c_sel); free(r_sel);
        return(1);
        }

static int mat_not_found(char *name)
        {
        sprintf(sbuf,"\nMatrix %s not found!",name);
        sur_print(sbuf); WAIT; return(1);
        }
        
static int mat_alloc(
double **A,  /* matriisitila (alkuosoite) (malloc) */
int m,      /* rivien lkm */
int n      /* sar. lkm   */
)
        {
        return(varaa_tila(A,m,n,NULL,NULL,0,0));
        }

static int save_T(char *nimi)
        {
        int i;

        i=matrix_save(nimi,T,mT,nT,rlabT,clabT,8,8,-1,exprT,0,0);
        return(i);
        }



static int mat_alloc_lab(
double **A,  /* matriisitila (alkuosoite) (malloc) */
int m,      /* rivien lkm */
int n,      /* sar. lkm   */
char **rlab, /* rivien otsikot (malloc) */
char **clab /* sar. otsikot   (malloc) */
)
        {
        return(varaa_tila(A,m,n,rlab,clab,8,8));
        }
        

static int mat_comment(char *tulos,char *lauseke,int tyyppi,int m,int n,char *message)
        {
        char x[LLENGTH];
        char y[LLENGTH+32];
        char luku[8];
        char *p;
        int i;

        if (!line_nr) return(1);
        edread(x,line_nr);
        p=x+1;
        while (p!=NULL)
            {
            p=strchr(p,'/'); if (p==NULL) break;
            if (strncmp(p-1," / ",3)==0) break;
            ++p;
            }
        if (p==NULL)
            {
            i=strlen(x)-1;
            while (x[i]==' ') --i;
            i+=2; i=(i/10+1)*10;
            }
        else if (*(p+2)!='*') return(1);
        else i=p-x;

        strcpy(y,"/ *"); strcat(y,tulos); strcat(y,EQUALS);
        strcat(y,lauseke); strcat(y," ");
        switch (tyyppi)
            {
          case  0: break;
          case 10: strcat(y,"S"); break;
          case 20: strcat(y,"D"); break;
            }
        strcat(y,muste_itoa(m,luku,10)); strcat(y,"*");
        strcat(y,muste_itoa(n,luku,10));
        if (message!=NULL)
            strcat(y,message);
        edwrite(space,line_nr,i);
        edwrite(y,line_nr,i);
        return(1);
        }

static int external_mat_init(int type)
        {
        int i;
        char *p;

        if (*info=='*') line_nr=r1+r-1; else line_nr=0;
        p=strstr(info," / "); if (p!=NULL) *p=EOS;
        strcpy(comline,info);

        if (type==1)
            g=split(comline,word,MAXPARM);
        else
            g=splitp(comline,word,MAXPARM);

        if (strchr(word[1],'(')!=NULL)
            {
            strcpy(comline,info);
            p=strchr(comline+1,'('); *p=' ';
            p=comline+strlen(comline)-1;
            while (*p==' ') --p;
            if (*p!=')')
                {
                sprintf(sbuf,"\n')' missing at the end of\n%s",info);
                sur_print(sbuf); WAIT;
                return(-1);
                }
            *p=EOS;
            g=split(comline,word,MAXPARM);
            }
        return(1);
        }

static int external_mat_end(char *argv1)
        {
        int i;
        char mat_prog[LNAME];

        if (line_nr) return(1);
        i=LLENGTH>>1;
        strcpy(info,info+i);
/* info:            MATRUN command
   info+LLENGTH-10: position of current command in mtx file
*/
 // RS REM       exit(0);
/*      strcpy(mat_prog,survo_path); strcat(mat_prog,"!MAT.EXE");
        execl(mat_prog,mat_prog,argv1,NULL);
*/
        return(1);
        }


/* freq 9.10.2006/SM (9.10.2006)
*/
// MAT #FREQ(B,A,n)

static int op__freq()
        {
        int i,j,m,n,k,h,s,u;
        double min;
        int smin;
        char expr1[2*LLENGTH];

        i=external_mat_init(1); if (i<0) return(1);
        if (g<5)
            {
            init_remarks();
            rem_pr("MAT #FREQ(B,A,n");
            rem_pr("computes the frequencies of elements 0,1,2,...,n");
            rem_pr("in vector A to vector B.");

            wait_remarks(2);
            return(1);
            }

        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return(1); }
        n=atoi(word[4]);

        mT=n+1;
        nT=1;
        freq=(int *)malloc((n+1)*sizeof(int));
        for (i=0; i<=n; ++i) freq[i]=0;

        i=mat_alloc_lab(&T,mT,nT,&rlabT,&clabT);
        if (i<0) return(1);

        numlab2(rlabT,n+1,8,0);
        numlab2(clabT,1,8,1);

        for (i=0; i<mX; ++i)
            {
            k=(int)X[i];
            if (k<0 || k>n) continue;
            ++freq[k];
            }

        for (i=0; i<=n; ++i) T[i]=(double)freq[i];

        sprintf(expr,"FREQ(%s)",exprX);
        nim(expr,exprT);
        i=save_T(word[2]);
        mat_comment(word[2],exprT,i,mT,nT,"");
        external_mat_end(argv1);
        return(1);
        }




/* permord 9.10.2006/SM (9.10.2006)
*/

static int *perm;
static double *row;
static int *permord_fact;
static int *perm2;

// MAT #PERMORD(B,A,m,n)

static int perm_lex_rank(int n,int *pi)
    {
    int i,rx,j;

    for (i=0; i<n; ++i) perm2[i]=pi[i];
    rx=0;
    for (j=0; j<n; ++j)
        {
        rx+=(perm2[j]-1)*permord_fact[n-j-1];
        for (i=j+1; i<n; ++i)
            {
            if (perm2[i]>perm2[j]) perm2[i]-=1;
            }
        }
    return(rx);
    }




static int op__permord()
        {
        int i,j,m,n,k,h,s,u;
        double min;
        int smin;
        char expr1[2*LLENGTH];

        i=external_mat_init(1); if (i<0) return(1);
        if (g<6)
            {
            init_remarks();
            rem_pr("MAT #PERMORD(B,A,m,n");
            rem_pr("computes the lexicographic rank for the permutation");
            rem_pr("of columns m,m+1,...,n");
            rem_pr("for each row of A and saves these ranks to vector B.");

            wait_remarks(2);
            return(1);
            }

        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return(1); }
        m=atoi(word[4]);
        n=atoi(word[5]);

// printf("\nm=%d n=%d|",m,n); getch();
        k=n-m+1;
        mT=mX;
        nT=1;
        perm=(int *)malloc((k+1)*sizeof(int));
        perm2=(int *)malloc((k+1)*sizeof(int));
        row=(double *)malloc((k+1)*sizeof(double));
        permord_fact=(int *)malloc((k+1)*sizeof(int));
        permord_fact[1]=1; permord_fact[0]=1;
        for (i=2; i<=k; ++i) permord_fact[i]=i*permord_fact[i-1];


        i=mat_alloc_lab(&T,mT,nT,&rlabT,&clabT);
        if (i<0) return(1);


        rlabT=rlabX;
        strcpy(clabT,"rank");

        for (i=0; i<mX; ++i)
            {
            for (h=0; h<k; ++h) row[h]=X[i+(h+m-1)*mX];
            for (j=0; j<k; ++j)
                {
                min=1e300;
                for (s=0; s<k; ++s)
                    {
                    if (row[s]<min) { min=row[s]; smin=s; }
                    }
                perm[j]=smin+1; row[smin]=1e300;
                }

            u=perm_lex_rank(k,perm);
            T[i]=(double)(u+1);

//          s=(int)T[i];

            }

        sprintf(expr,"PERMORD(%s)",exprX);
        nim(expr,exprT);
        i=save_T(word[2]);
        mat_comment(word[2],exprT,i,mT,nT,"");
        external_mat_end(argv1);
        return(1);
        }






/* rcsort 14.12.2006/SM (14.12.2007)
*/
static double *v,*w;

// MAT #RCSORT(B,A)

static int sort_row(int i,int m,int n)
    {
    int j,h,k,imin;
    double min;

    for (j=0; j<n; ++j) v[j]=X[i+m*j];
    for (h=1; h<n; ++h) if (v[h]<v[h-1]) break;
    if (h==n) return(0);

    for (k=0; k<n; ++k)
        {
        min=v[0]; imin=0;
        for (j=1; j<n; ++j)
            if (v[j]<min) { min=v[j]; imin=j; }
        w[k]=min; v[imin]=1e300;
        }
    for (j=0; j<n; ++j) X[i+m*j]=w[j];
    return(1);
    }

static int sort_column(int i,int m,int n)
    {
    int j,h,k,imin;
    double min;

    for (j=0; j<m; ++j) v[j]=X[m*i+j];
    for (h=1; h<m; ++h) if (v[h]<v[h-1]) break;
    if (h==m) return(0);

    for (k=0; k<m; ++k)
        {
        min=v[0]; imin=0;
        for (j=1; j<m; ++j)
            if (v[j]<min) { min=v[j]; imin=j; }
        w[k]=min; v[imin]=1e300;
        }
    for (j=0; j<m; ++j) X[m*i+j]=w[j];
    return(1);
    }




static int op__rcsort()
        {
        int i,j,m,n,k,mn;
        double min;
        char expr1[2*LLENGTH];

        i=external_mat_init(1); if (i<0) return(1);
        if (g<4)
            {
            init_remarks();
            rem_pr("MAT #RCSORT(B,A");
            rem_pr("sorts elements a_ij in order b_ij so that");
            rem_pr("b_ij<=b_hk for all h>i and k>j.");

            wait_remarks(2);
            return(1);
            }

        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return(1); }

        m=mX; n=nX;
        mn=m; if (n>m) mn=n;
        v=(double *)malloc(n*sizeof(double));
        w=(double *)malloc(n*sizeof(double));

        while (1)
            {
            k=0;
            for (i=0; i<m; ++i)
                {
                k+=sort_row(i,m,n);
// printf("\ni=%d",i);
// mprint(X,m,n);
                }

            for (j=0; j<n; ++j)
                {
                k+=sort_column(j,m,n);
// printf("\nj=%d",j);
// mprint(X,m,n);
                }
            if (k==0) break;
            }


        sprintf(expr,"RCSORT(%s)",exprX);
        nim(expr,exprX);
        i=matrix_save(word[2],X,mX,nX,rlabX,clabX,8,8,-1,exprX,0,0);
        external_mat_end(argv1);
        return(1);
        }



/* proddiag 30.1.2008/SM (30.1.2008)
*/
static double *d;

// MAT #PRODDIAG(D,A,B)

static int op__proddiag()
        {
        int i,j,m,n;
        char expr1[2*LLENGTH];
        double sum;

        i=external_mat_init(1); if (i<0) return(1);
        if (g<5)
            {
            init_remarks();
            rem_pr("MAT #PRODDIAG(D,A,B)");
            rem_pr("computes the diagonal elements as a vector D (mx1)");
            rem_pr("of matrix A*B where A is mxn and B is n*m.");

            wait_remarks(2);
            return(1);
            }

        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return(1); }
        i=load_Y(word[4]); if (i<0) { mat_not_found(word[4]); return(1); }

        m=mX; n=nX;
        if (mY!=n || nY!=m)
            {
            sprintf(sbuf,"Incompatible dimensions in matrices %s,%s",
                             word[3],word[3]);
            sur_print(sbuf); WAIT;
            return(1);
            }
        d=(double *)malloc(m*sizeof(double));

        for (i=0; i<m; ++i)
            {
            sum=0.0;
            for (j=0; j<n; ++j)
                sum+=X[i+m*j]*Y[j+n*i];
            d[i]=sum;
            }

        sprintf(expr,"PRODDIAG(%s,%s)",exprX,exprY);
        nim(expr,exprX);
        i=matrix_save(word[2],d,mX,1,rlabX,"diag    ",8,8,-1,exprX,0,0);
        external_mat_end(argv1);
        return(1);
        }



        
/* #tab.c 24.5.2008/SM (24.5.2008)
*/
/******************
MAT #TAB(D1,DENS1,OSX,OSY) / OSX=-0.9(0.01)0.4 OSY=-0.3(0.01)0.35
                             (0.4+0.9)/0.01+1=131  (0.35+0.3)/0.01+1=66
******************/

static SURVO_DATA dat;
static double min[2],step[2],max[2];

static int tab_limits(int k,char *spb)
    {
    int i;
    char *p;
    char x[LLENGTH],*s[3];

    strcpy(x,spb);
    p=strchr(x,'(');
    if (p==NULL) { sprintf(sbuf,"\n( missing in %s",spb);
                   sur_print(sbuf); WAIT; return(-1);
                 }
    *p=' ';
    p=strchr(p+1,')');
    if (p==NULL) { sprintf(sbuf,"\n) missing in %s",spb);
                   sur_print(sbuf); WAIT; return(-1);
                 }
    *p=' ';
// printf("\nx=%s",x); getch();
    i=split(x,s,3);
    min[k]=atof(s[0]); step[k]=atof(s[1]); max[k]=atof(s[2]);

    if (max[k]<=min[k] || step[k]<=0.0)
        {
        sprintf(sbuf,"\nInvalid classification %s",spb);
        sur_print(sbuf); WAIT; return(-1);
        }
    return(1);
    }

static int op__tab()
        {
        int i,j,ix,iy,m,n;
        char expr1[2*LLENGTH];
        long l;

        i=external_mat_init(1); if (i<0) return(1);
        if (g<6)
            {
            init_remarks();
            rem_pr("MAT #AGGRE(F,DAT,X,Y) / X=min(step)max Y=min(step)max");
            rem_pr("matrix F as a table of frequencies from variables X,Y");
            rem_pr("in Survo data DAT using given classifications.");

            wait_remarks(2);
            return(1);
            }

        i=spec_init(r1+r-1); if (i<0) return(1);
        i=data_open(word[3],&dat); if (i<0) return(1);
        ix=varfind(&dat,word[4]); if (ix<0) return(1);
        iy=varfind(&dat,word[5]); if (iy<0) return(1);
// printf("\nix=%d iy=%d",ix,iy); getch();

        i=spfind(word[4]); if (i<0) return(1);
        tab_limits(0,spb[i]);
// printf("\nlimits: %g %g %g",min[0],step[0],max[0]); getch();
        i=spfind(word[5]); if (i<0) return(1);
        tab_limits(1,spb[i]);
        m=mT=(int)((max[0]-min[0]+1e-12)/step[0])+1;
        n=nT=(int)((max[1]-min[1]+1e-12)/step[1])+1;
        sprintf(sbuf,"\nm=%d n=%d ",m,n); sur_print(sbuf);

        i=mat_alloc_lab(&T,m,n,&rlabT,&clabT);
        if (i<0) return(1);
        numlab(rlabT,m,8);
        numlab(clabT,n,8);

        for (i=0; i<m; ++i) for (j=0; j<n; ++j) T[i+m*j]=0.0;

        for (l=dat.l1; l<=dat.l2; ++l)
            {
            double x,y;

            data_load(&dat,l,ix,&x);
            data_load(&dat,l,iy,&y);
            i=(int)((x-min[0])/step[0]);
            j=(int)((y-min[1])/step[1]);
            if (i<0 || i>=m || j<0 || j>=n) continue;
            ++T[i+m*j];
            }

        sprintf(expr,"TAB(%s)",word[3]);
        nim(expr,exprT);
        i=save_T(word[2]);
        mat_comment(word[2],exprT,i,m,n,"");


        external_mat_end(argv1);
        return(1);
        }


/* #smooth.c 24.5.2008/SM (24.5.2008)
*/

/******************
MAT #SMOOTH(B,A)

******************/


static int op__smooth()
        {
        int i,j,m,n,n_iter,i_iter;
        char expr1[2*LLENGTH];
        int rule;

        i=external_mat_init(1); if (i<0) return(1);
        if (g<4)
            {
            init_remarks();
            rem_pr("MAT #SMOOTH(B,A,rule,iter)");
            rem_pr("                                                     ");
            rem_pr("                                              ");

            wait_remarks(2);
            return(1);
            }


        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return(1); }
        m=mX; n=nX;

        rule=atoi(word[4]);
        n_iter=atoi(word[5]);

        i=mat_alloc(&T,m,n);
        if (i<0) return(1);

        for (i=0; i<m; ++i) for (j=0; j<n; ++j) T[i+m*j]=0.0;

        switch (rule)
            {
          case 0:
            for (i=1; i<m-1; ++i) for (j=1; j<n-1; ++j)
                {
                T[i+m*j]=0.2*(X[i+m*j]+X[i-1+m*j]+X[i+1+m*j]
                                      +X[i+m*(j-1)]+X[i+m*(j+1)]);
                }
            break;

          case 1:
            for (i_iter=0; i_iter<n_iter; ++i_iter)
              {
              sprintf(sbuf," %d",i_iter); sur_print(sbuf);
              for (i=0; i<m; ++i) for (j=0; j<n; ++j) T[i+m*j]=0.0;

            for (i=2; i<m-2; ++i) for (j=2; j<n-2; ++j)
                T[i+m*j]=0.1*(X[i+m*j]+X[i-1+m*j]+X[i+1+m*j]+X[i+m*(j-1)]+X[i+m*(j+1)]
                +0.750*(X[i-1+m*(j-1)]+X[i+1+m*(j-1)]+X[i+1+m*(j-1)]+X[i+1+m*(j+1)])
                +0.500*(X[i-2+m*j]+X[i+2+m*j]+X[i+m*(j-2)]+X[i+m*(j+2)]));

              for (i=0; i<m; ++i) for (j=0; j<n; ++j) X[i+m*j]=T[i+m*j];
              }

            break;
           }
        sprintf(expr,"SMOOTH(%s)",word[3]);
        nim(expr,exprT);
        i=matrix_save(word[2],T,mX,nX,rlabX,clabX,8,8,-1,exprT,0,0);
        mat_comment(word[2],exprT,i,m,n,"");


        external_mat_end(argv1);
        return(1);
        }

static int list_nimet()
       {
       int i,j;

       i=0; j=r1+r;
       while (*nimet[i]!='#')
           {
           if (j==r2) break;
           edwrite(space,j,1);
           strcpy(sbuf,"MAT #"); strcat(sbuf,nimet[i++]);
           edwrite(sbuf,j++,1);
           }
       if (j==r2) return(1);
       edwrite(space,j,1);
       return(1);
       }


static void m_end()
    {
    s_end(mat_argv1);
// RS REM    exit(0);
    }

static void external_op()
        {
        int i;
        char *p,*q,*q2;
        char opfile[LNAME];
        char x[LLENGTH];
        long lpos;
        char *osa[2];

        edread(comline,r1+r-1);
        if (mtx)
            {
            strcpy(x,info);
            strcpy(info,comline2); *info='M';
            i=LLENGTH>>1; strcpy(info+i,x);
            lpos=ftell(mtx_file);
            p=info+LLENGTH-10; q=(char *)&lpos;
            for (i=0; i<sizeof(int); ++i) *p++=*q++; // RS CHA sizeof(long) -> sizeof(int)
/* info               info+LLENGTH>>1      info+LLENGTH-10
   current command    MATRUN command       position in mtx file
*/
            }
        else     { strcpy(info,comline); *info='*'; }

        strcpy(opfile,survo_path); strcat(opfile,"&MATEXT1.EXE");
        s_end(mat_argv1);
        
 
       s_init(mat_argv1);
       argv1=mat_argv1; // RS CHA

// printf("\ninfo=%.50s|\n",info); getch();

       p=strstr(info," / "); // 23.5.2004 esim. / PRIND=0 aiheuttaisi
       if (p!=NULL) *p=EOS;  //                           ongelmia!

// *MAT B=#XXX(A,...) -> *MAT #XXX(B,A,...)  15.3.2003


#if 0 // RS NYI FIXME
       if (strstr(info,"#TRANSFORM") != NULL) // 18.6.2007
           { op__transform(); m_end(); return; }
#endif

       p=strchr(info+1,'=');
       if (p!=NULL)
           {
           *p=EOS;
           q=p; while (*q!=' ') --q; strcpy(sbuf,q+1);
           q=strchr(p+1,'('); *q=EOS;
           sprintf(x,"MAT %s(%s,%s",p+1,sbuf,q+1);
           strcpy(info+1,x);
// printf("\ninfo=%.50s|\n",info); getch();
           }
       strcpy(x,info+1); split(x,osa,2);

       p=strchr(osa[1],'('); if (p!=NULL) *p=EOS;

       if (muste_strcmpi(osa[1],"#")==0) { list_nimet(); m_end(); return; }

#if 0 // RS NYI FIXME
       if (muste_strcmpi(osa[1],"#MULT")==0) { op__mult(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#SAMPLES")==0) { op__samples(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#INDVAR")==0) { op__indvar(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#MERGE")==0) { op__merge(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#MINDIFF")==0) { op__mindiff(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#COLSORT")==0) { op__colsort(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#CRSORT")==0) { op__crsort(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#EIGEN")==0) { op__eigen(); m_end(); return; }
  else if (muste_strnicmp(osa[1],"#CONVOL",7)==0) { op__convol(); m_end(); return; }
  else if (muste_strnicmp(osa[1],"#MAXDET",7)==0) { op__maxdet(); m_end(); return; }
  else if (muste_strnicmp(osa[1],"#U_TO_F",7)==0) { op__u_to_f(); m_end(); return; }
  else if (muste_strnicmp(osa[1],"#INTSCAL",8)==0) { op__intscal(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#FRAC_TO_DEC")==0) { op__frac_to_dec(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#SAMPLE")==0) { op__sample(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#SORT")==0) { op__sort(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#MAGIC")==0) { op__magic(); m_end(); return; }
  else if (muste_strnicmp(osa[1],"#JACK",5)==0) { op__jack(); m_end(); return; }  
  else if (muste_strcmpi(osa[1],"#EIGFEW")==0) { op__eigfew(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#EIGLAN")==0) { op__eiglan(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#AGGRE")==0) { op__aggre(); m_end(); return; }
#endif  // RS FIXME
  else if (muste_strcmpi(osa[1],"#PERMORD")==0) { op__permord(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#FREQ")==0) { op__freq(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#RCSORT")==0) { op__rcsort(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#PRODDIAG")==0) { op__proddiag(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#TAB")==0) { op__tab(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#SMOOTH")==0) { op__smooth(); m_end(); return; }

  else    {
          sprintf(sbuf,"\nMAT %s is unknown operation!",osa[1]);
          sur_print(sbuf); WAIT; return;
          } 
        
//        muste_mat_external(); // RS CHA        s_spawn(opfile,mat_argv1);
        return;
        }

static int op1(char op,char *opnd1,char *opnd2,char *tulos)
        {
        int i;

        if (op=='*')   /* A'*A -> MTM(A), A*A' -> MMT(A) */
            {
            i=strlen(opnd1)-1;
            if (opnd1[i]=='\'' && strlen(opnd2)==i &&
                                    strncmp(opnd1,opnd2,i)==0)
                {
                word[1]=opnd2; return(op_mtm());
                }
            i=strlen(opnd2)-1;
            if (opnd2[i]=='\'' && strlen(opnd1)==i &&
                                    strncmp(opnd1,opnd2,i)==0)
                {
                word[1]=opnd1; return(op_mmt());
                }
            }

        i=load_X(opnd1);
        if (i<0) return(-1);

        if (op!='\'')
            {
            i=load_Y(opnd2);
            if (i<0) return(-1);
            }
/*
   printf("\nmX=%d nX=%d",mX,nX);
   printf("\nX[0]=%f %f",X[0],X[mX*nX-1]);
   printf("\nclab=%.*s",nX*lcX,clabX);
   printf("\nexprX=%s",exprX);
   printf("\nY[0]=%f",Y[0]);
   sur_getch();
*/
        switch (op)
            {
          case '+': i=op_add(); break;
          case '-': i=op_sub(); break;
          case '*': i=op_mlt(); break;
          case '\'':i=op_transp(); break;
          case '^': i=op_pow(); break;
          case '/': i=op_div(); break;

            }
        return(i);
        }

static int op2(char *ops,char *opr1)
        {
        char *parm[10];
        int i;

        word[1]=opr1;
/*******************
     printf("\nops=%s opr1=%s\n",ops,opr1);
     for (i=0; i<g; ++i) printf(" %s",word[i]); sur_getch();
********************/
        i=1;
        while ( word[i][strlen(word[i])-1]!=')' && i<g ) ++i;
        if (i==g)
            {
            PR_EBLD;
            mat_parser_zero();
            sur_print("\n) is missing!");
            WAIT; PR_ENRM; return(-1);
            }
        g=i; word[g][strlen(word[g])-1]=EOS;

//         for (i=1; i<=g; ++i) printf(" %s",word[i]); sur_getch();
        /* Huom! parametrit ovat nyt word[1],...,word[g] */

        if (muste_strcmpi(ops,"CON")==0) { i=op_con(1.0); return(i); }
        else if (muste_strcmpi(ops,"ZER")==0) { i=op_zer(); return(i); }
        else if (muste_strcmpi(ops,"IDN")==0) { i=op_idn_tri(1); return(i); }
        else if (muste_strcmpi(ops,"TRI")==0) { i=op_idn_tri(2); return(i); }
        else if (muste_strcmpi(ops,"HILBERT")==0) { i=op_idn_tri(3); return(i); }
        else if (muste_strcmpi(ops,"INV")==0) { i=op_inv(); return(i); }
        else if (muste_strcmpi(ops,"DV")==0) { i=op_dv(); return(i); }
        else if (muste_strcmpi(ops,"VD")==0) { i=op_vd(); return(i); }
        else if (muste_strcmpi(ops,"MTM")==0) { i=op_mtm(); return(i); }
        else if (muste_strcmpi(ops,"SUM")==0) { i=op_sum(); return(i); }
        else if (muste_strcmpi(ops,"MMT")==0) { i=op_mmt(); return(i); }
        else if (muste_strcmpi(ops,"CENTER")==0) { i=op_center(); return(i); }
        else if (muste_strcmpi(ops,"NRM")==0 || muste_strcmpi(ops,"NORM")==0) { i=op_nrm(); return(i); }
        else if (muste_strcmpi(ops,"CHOL")==0) { i=op_chol(); return(i); }
        else if (muste_strcmpi(ops,"DINV")==0) { i=op_dinv(); return(i); }
        else if (muste_strcmpi(ops,"MAX")==0) { i=op_max_min(1); return(i); }
        else if (muste_strcmpi(ops,"MIN")==0) { i=op_max_min(2); return(i); }
        else if (muste_strcmpi(ops,"VEC")==0) { i=op_vec(0); return(i); }
        else if (muste_strcmpi(ops,"NVEC")==0) { i=op_vec(1); return(i); }
        else if (muste_strcmpi(ops,"PERM")==0) { i=op_perm(1); return(i); }
        else if (muste_strcmpi(ops,"PERM2")==0) { i=op_perm(2); return(i); }
        else if (muste_strcmpi(ops,"PERM0")==0) { i=op_perm(0); return(i); }
        else if (muste_strcmpi(ops,"P")==0 && isupper((int)*word[1])) { i=op_p(); return(i); }
        else if (muste_strcmpi(ops,"MMT2")==0) { i=op_mmt2(); return(i); }
        else if (muste_strcmpi(ops,"MTM2")==0) { i=op_mtm2(); return(i); }
        else if (muste_strcmpi(ops,"DIAGVEC")==0) { i=op_diagvec(); return(i); } /* 5.3.1995 */
        else if (muste_strnicmp(ops,"KRON",4)==0) { i=op_kronecker(); return(i); } /* ?.6.1995 */
        else if (muste_strnicmp(ops,"CUM",3)==0) { i=op_cum(1); return(i); } /* 8.7.1998 */
        else if (muste_strnicmp(ops,"UNCUM",3)==0) { i=op_cum(2); return(i); } /* 8.7.1998 */
        else if (muste_strnicmp(ops,"PROD",4)==0) { i=op_product(); return(i); } /* 8.7.1998 */
        else if (muste_strcmpi(ops,"SELECT")==0) { i=op_select(); return(i); } /* 4.9.1998 */
        else if (muste_strcmpi(ops,"DIAG")==0) { i=op_diag(); return(i); } /* 3.4.1999 */
        else if (muste_strcmpi(ops,"SUB")==0) { i=op_submat(); return(i); } /* 6.6.1999 */
        else if (muste_strcmpi(ops,"MAX_IJ")==0) { i=op_max_min_ij(1); return(i); } /* 5.10.1999 */
        else if (muste_strcmpi(ops,"MIN_IJ")==0) { i=op_max_min_ij(2); return(i); } /* 5.10.1999 */
        else if (muste_strnicmp(ops,"NULL",4)==0) { i=op_null(); return(i); } /* 10.3.2003 */
        else if (muste_strcmpi(ops,"BASIS")==0) { i=op_basis(); return(i); } /* 10.3.2003 */
        else if (muste_strcmpi(ops,"DET")==0) { i=op_det(1); return(i); } /* 10.3.2003 */
        else if (muste_strcmpi(ops,"LDET")==0) { i=op_det(2); return(i); } /* 10.3.2003 */
        else if (muste_strcmpi(ops,"RANK")==0) { i=op_rank(); return(i); } /* 10.3.2003 */
        else if (muste_strcmpi(ops,"TRACE")==0) { i=op_trace1(); return(i); } /* 10.3.2003 */
        else if (muste_strcmpi(ops,"MPINV")==0) { i=op_mp_inv(); return(i); } /* 10.3.2003 */

        else if (*ops=='#') { external_op(); return(-1); /* RS CHA exit(0); */ } // 15.3.2003


/*      else if (*ops>='a' && *ops<='z') { i=scalar_function(ops);
                                                   return(i); }
*/
        i=op_copy2(ops); if (i>0) return(1);

        sprintf(sbuf,"\nUnknown or erroneous matrix or MAT operation %s",ops);
        sur_print(sbuf); WAIT; return(-1);
        }


static int op3()
        {
        char p[LLENGTH];
        int i;
        char *q;

//      if (*word[1]=='#') return(external_op());

        if (*word[1]=='#')
            {
            external_op();
            return(-1); // RS CHA exit(0);
            }
        strcpy(p,word[1]); muste_strupr(p);  /* ennen 4,11,87  strcpy(p,strupr(word[1]); */
        if (strcmp(p,"REM")==0 || strcmp(p,"/*")==0 || strcmp(p,"*")==0) { i=op_rem(); return(1); }
        q=strchr(word[2],'=');
        if (q!=NULL)
            {
            i=op_copy3();  /* MAT C(i,j)=A */
            if (i<0) return(-1);
            return(1);
            }

        if (strcmp(p,"LOAD")==0) { i=op_load(); return(i); }
        if (strcmp(p,"SAVE")==0) { i=op_save(); return(i); }
        if (strncmp(p,"SING",4)==0 || strcmp(p,"SVD")==0) { i=op_svd(); return(i); }
        if (strcmp(p,"NAME")==0) { i=op_name(); return(i); }
        if (strcmp(p,"RLABELS")==0) { i=op_rlab(); return(i); }
        if (strcmp(p,"CLABELS")==0) { i=op_clab(); return(i); }
        if (strcmp(p,"KILL")==0) { i=op_kill(); return(i); }
        if (strncmp(p,"GRAM",4)==0) { i=op_gram_schmidt(); return(i); }
        if (strcmp(p,"SOLVE")==0) { i=op_solve(0); return(i); }
        if (strcmp(p,"SOLVE1")==0) { i=op_solve(1); return(i); } // 13.7.2005
        if (strncmp(p,"SPEC",4)==0) { i=op_spectral(); return(i); }
        if (strncmp(p,"TRAN",4)==0) { i=op_transform(); return(i); }
        if (strcmp(p,"DIM")==0) { i=op_dim(); return(i); }
        if (strcmp(p,"TRACE")==0) { i=op_trace(); return(i); }
        if (strcmp(p,"QR")==0) { i=op_qr(); return(i); }
        if (strcmp(p,"QRP")==0) { i=op_qrp(); return(i); }

        PR_EBLD;
        sprintf(sbuf,"\nMAT %s is unknown operation!",p);
        sur_print(sbuf); WAIT; PR_ENRM;
        return(-1);
        }

static int matrix_op()
        {
        int i;
        char *p;
        char lauseke[LLENGTH];

        if (g<=1) return(-1); // RS CHA exit -> return(-1)
        p=strchr(word[1],'=');
        if (p==NULL)
            {
  if (g==2 && *word[1]!='#') return(op_tell(word[1],word[1]));
            i=op3(); return(i);
            }

        strncpy(tulos,word[1],p-word[1]); tulos[p-word[1]]=EOS;
        strcpy(lauseke,p+1);

/*      if (tulos[strlen(tulos)-1]=='%')
            return(super_matrix());              */
/*
    printf("tulos=%s\n",tulos);
    printf("lauseke=%s\n",lauseke);  sur_getch();
*/
        p=lauseke;

        /* skalaarikertoimen ohitus */
        if (*p=='-'|| *p=='+') ++p;
        if (*p=='(')
            {
            int sulut=1;
            while (sulut)
                {
                ++p; if (*p==EOS) { sur_print("\n) missing!"); WAIT; return(-1); }
                if (*p=='(') ++sulut;
                if (*p==')') --sulut;
                }
            ++p;
            }

        while (*p)
          {
        switch (*p)
            {
          case '\'': if (*(p+1)!=EOS) { ++p; continue; } /* 7.1.1999 */
          case '+':
          case '-':
          case '*':
          case '^':
          case '/':  /* 11.1.1999 */
                  strncpy(opnd1,lauseke,p-lauseke); opnd1[p-lauseke]=EOS;
                  op=*p;
                  strcpy(opnd2,p+1);
      /*          printf("\nopnd1=%s",opnd1);
                  printf("\nop=%c",op);
                  printf("\nopnd2=%s",opnd2);
      */
                  i=op1(op,opnd1,opnd2,tulos);

                  return(i);
          case '(':
 // 7.1.1999      if (*lauseke<'A' || *lauseke>'Z') { ++p; continue; }
 // 16.1.1999 miksi???
                  strncpy(opnd1,lauseke,p-lauseke); opnd1[p-lauseke]=EOS;
                  strcpy(opnd2,p+1);
                  i=op2(opnd1,opnd2);
                  return(i);

          default: ++p;
            }  /* switch p */
          }

        return(op_copy(tulos,lauseke));
        }


static void pol_dim_overflow()
        {
        sprintf(sbuf,"\nMax. degree of a polynomial is %d",MAXN-1);
        sur_print(sbuf); WAIT;
        }

static int zero(double x)
        {
        if (fabs(x)<C_ZERO) return(1);
        return(0);
        }
        
static int c_zero(struct complex *z)
        {
        if (zero(z->x) && zero(z->y)) return(1);
        return(0);
        }        

int muste_pol_load(char *matr,struct polynom *pol)
        {
        int i;

        i=load_X(matr); if (i<0) return(-1);

        pol->n=mX-1;
        if (pol->n>MAXN-1) { pol_dim_overflow(); return(-1); }
        for (i=0; i<mX; ++i)
            {
            pol->a[i].x=X[i];
            if (nX==1) pol->a[i].y=0.0;
            else       pol->a[i].y=X[mX+i];
            }
        i=pol->n;
        while (c_zero(&(pol->a[i])) && i>0) --i;
        pol->n=i;
        return(nX);
        }

static void numlab0(char *lab,int n,int len,int base)
        {
        int h,i,j,k;
        char sana[6];
        int sar;

        for (i=0; i<n*len; ++i) lab[i]=' ';
        if (n<1000) sar=3; else sar=5;

        for (i=0; i<n; ++i)
            {
            muste_itoa(i+base,sana,10);
            h=strlen(sana);
            for (j=i*len+sar-h, k=0; k<h; ++k, ++j) lab[j]=sana[k];
            }
        }

int muste_pol_save2(
char *matr,
struct polynom *pol,
char *ots, /* column labels */
int base,  /* first numeric row label */
char *expr
)
        {
        int i;

        mX=pol->n+1;
        nX=1;
        for (i=0; i<=pol->n; ++i)
            if (!zero(pol->a[i].y)) { nX=2; break; }

        i=varaa_tila(&X,mX,nX,&rlabX,NULL,8,8);
        if (i<0) return(-1);

        numlab0(rlabX,mX,8,base);

        for (i=0; i<mX; ++i)
            {
            X[i]=pol->a[i].x;
            if (nX==2) X[mX+i]=pol->a[i].y;
            }

        i=mat_save(matr,X,mX,nX,rlabX,ots,8,8,0,expr,0,0);

        return(1);
        }

int muste_pol_save(char *matr,struct polynom *pol)
        {
        strcpy(exprX,"Polynom");
        return(muste_pol_save2(matr,pol,"real    imag    ",0,exprX));
        }

static int mtx_tell()
    {
    char *p;
    char s[LNAME];
    int i;

    i=0;
    while (1)
        {
        p=s; while ((*p=(char)fgetc(mtx_file))!='\n' && !feof(mtx_file)) ++p;
        *p=EOS;
        if (feof(mtx_file)) return(1);

        p=s; while (*p && *p==' ') ++p;
            {
            if (*p=='/')
                {
                if (*(p+1)==EOS)
                    sprintf(sbuf,"\n%-*.*s",c3+7,c3+7,space);
                else
                    sprintf(sbuf,"\n%-*.*s",c3+7,c3+7,s+2);
                sur_print(sbuf);
                if (i==0)
                    {
                    edwrite(space,r1+r-1,1);
                    edwrite(sbuf+1,r1+r-1,1);  /* mallikomento */
                    i=1;
                    }
                }
            }
        }
    return(1);
    }

static int mtx_open(long lpos)
        {
        int i;
        char nimi[LLENGTH];
        int tell=0;
        char *p;

        edread(info,r1+r-1);
        strcpy(mtx_info,info+1);
        i=split(mtx_info,pmtx,MAXMTX)-2;
        if (i<0)
            {
            sur_print("\nUsage: MATRUN <MTX_file>,P1,P2,...");
            sur_print("\n       MATRUN <MTX_file>?   gives information on <MTX_file>.");
            WAIT; return(-1);
            }
        for (nmtx=0; nmtx<i; ++nmtx) if (strcmp(pmtx[nmtx+2],"/")==0) break;
  /*    mtx_prog=pmtx[0]; */

        p=strchr(pmtx[1],'?');
        if (p!=NULL) { tell=1; *p=EOS; }

        strcpy(nimi,pmtx[1]);
        if (strchr(nimi,':')==NULL)
             { strcpy(nimi,survo_path); strcat(nimi,"M/"); strcat(nimi,pmtx[1]); } // RS CHA \\ -> /
        if (strchr(nimi+strlen(nimi)-4,'.')==NULL) strcat(nimi,".MTX");

        mtx_file=muste_fopen(nimi,"rt");
        if (mtx_file==NULL)
            {
            sprintf(sbuf,"\nMatrix chain file %s not found!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (tell) { mtx_tell(); WAIT; return(-1); }

/* ???
        if (strstr(info,"%%%")!=NULL)
            { spn=sp_init(0); sp_read=1; }
*/
        muste_fseek(mtx_file,lpos,SEEK_SET); /* 24.11.1995 */
        return(1);
        }



static int mtx_read(char *s)
        {
        char *p,*q;
        char x[LLENGTH];
        int i;
        char par[16];

        p=s; while ((*p=(char)fgetc(mtx_file))!='\n' && !feof(mtx_file)) ++p;
        *p=EOS;
        if (feof(mtx_file)) return(-1);
        p=s; while (*p && *p==' ') ++p;
/*          {                */
            if (*p=='/')
                {
                scalar_write(s);
                return (0);
                }
/*          }                */

        *x=EOS; q=s;
        p=strchr(q,'%');
        while (p!=NULL)
            {
            *p=EOS; strcat(x,q);
            ++p;
            *par='%'; i=0; while (*p>='0' && *p<='9') { par[++i]=*p; ++p; }
            par[++i]=EOS;
            i=atoi(par+1);
            if (i && i<=nmtx)
                {
                strcat(x,pmtx[i+1]);
                }
            else
                {
                i=spfind(par);
                if (i<0) strcat(x,par); else strcat(x,spb[i]);
                }
            q=p;
            p=strchr(q,'%');
            }
        strcat(x,q);
        strcpy(s,x);
        return(1);
        }

static void mtx_scalar_print(char *x)
        {
        char *p,*q,*r,*s;
        char y[LLENGTH];
        char u[LLENGTH];
        double a;

        p=x;
        while ((q=strchr(p,'='))!=NULL)
            {
            *q=EOS;
            r=q+1; s=u;
            while (*r && *r!=' ') *s++=*r++;
            *s=EOS;
            laske(u,&a);
            sprintf(u,"%g",a);
            strcpy(y,x); strcat(y,"="); strcat(y,u); strcat(y,r);
            strcpy(x,y);
            p=q+1;
            }
        }

static char *find_last_command()
        {
        char *p;

        p=expr_space+strlen(expr_space)-2;
        while (p>=expr_space && *p!='\n') --p;
        return(++p);
        }

static int clean_expr_space(char *name)
        {
        char *p,*q;
        char y[LLENGTH];

        if (n_mat==0) return(1);
        p=find_last_command();
        q=strchr(p,'=');
        if (strncmp(q+1,"%%1\n",4)==0)
            { --n_mat; *p=EOS; p=find_last_command(); }
        q=strchr(p,'=');
        strcpy(y,name); strcat(y,q);
        strcpy(p,y);

        return(1);
        }

static int remove_extra_quotas(char *x)
        {
        char y[LLENGTH];
        char *p;

        p=strstr(x,"''"); if (p==NULL) return(1);
        while((p=strstr(x,"''"))!=NULL)
            {
            *y=EOS;
            strncat(y,x,p-x);
            strcat(y,p+2);
            strcpy(x,y);
            }
        return(1);
        }

static int replace_minus_by_minus_one_coeffs(char *x) // 17.5.2008
    {
    char y[LLENGTH];
    char *p,*q,*r;

// Rprintf("\nx=%s ",x); sur_getch();
    p=strchr(x,'-');
    if (p==NULL) return(1);
    strcpy(y,x); *x=EOS; p=y;
    while (1)
        {
        q=strchr(p,'-');
        if (q==NULL) break;
        strncat(x,p,q-p);
        r=q+1;
        if (isalpha((int)*r))
            {
            strcat(x,"+(-1)*");
            }
        else strcat(x,"-");
        p=q+1;
        }
    strcat(x,p);
// Rprintf("\nx2=%s ",x); sur_getch();
    return(1);
    }

static int power1(char ch,int k)
/* int k; 1=right 2=left */
        {
        switch (ch)
            {
          case '+':
              return(1);
          case '-':
              return(k);
          case '*':
          case '/':
              return(3);
          case '^':
              return(4);
          case '\'':
              return(5);
          case ',':
          case ')':
          case '(':
              return(0);
          default:
              syntax_error2("Error after )  !"); return(-1); // RS CHA exit -> return
              break;
            }
        return(-1);
        }

static int remove_pars(char *x)
        {
        int i,i0,len;
        char y[LLENGTH];
        char *p;
        int removed;
        char ch,ch1,ch2;
        int level;
        int pow,pow1,pow2,min_pow;

        removed=0;
        p=x; len=strlen(x);
        while (1)
            {
            p=strchr(p,'('); if (p==NULL) break;
            i=i0=p-x; ++p;
            if (i>0) ch1=x[i-1]; else ch1='+';
            if (strchr("+-*/^(,",ch1)==NULL) continue; /* function */
            level=0; min_pow=9;
            while (1)
                {
                ++i;
/*
Rprintf("i=%d x[i]=%c level=%d\n",i,x[i],level); sur_getch();
*/
                if (i>=len)
                    {
                    syntax_error2(") missing!?");
                    return(-1);
                    }
                switch (x[i])
                    {
                  case '(': ++level; break;
                  case ')':
                      --level;
                      if (i==len-1 && level>=0)
                          {
                          syntax_error2(") missing!?");
                          return(-1);
                          }
                      break;
                    }

                if (level==0)
                    {
                    if (strchr("+-*^",x[i])!=NULL)
                        {
                        pow=power1(x[i],1);
                        if (pow!=0 && pow<min_pow) min_pow=pow;
                        }
                    }

                if (level>=0) continue;

                if (i<len-1) ch2=x[i+1]; else ch2='+';
/*
Rprintf("ch1=%c ch2=%c\n",ch1,ch2);
*/
                pow1=power1(ch1,2);
                if (pow1<0) return(-1); // RS ADD exit handling
                pow2=power1(ch2,1);
                if (pow2<0) return(-1); // RS ADD exit handling
                
/*
Rprintf("pow1=%d pow2=%d min_pow=%d\n",pow1,pow2,min_pow); sur_getch();
*/
                if (pow2>pow1) pow1=pow2;
                if (pow1<=min_pow)
                    {
/*
Rprintf("i=%d i0=%d\n",i,i0); sur_getch();
*/
                    strcpy(y,x); *x=EOS;
                    strncat(x,y,i0);
                    strncat(x,y+i0+1,i-i0-1);
                    strcat(x,y+i+1);
                    return(1);
                    }
                break;
                }
            }
        return(0);
        }

static int remove_parentheses(char *x)
        {
        int i;

        i=1;
        while (i==1) i=remove_pars(x);
        return(i);
        }



static int create_arrays(char *x)
        {
        int i,len;
        int level1,power1;

        len=strlen(x);
        for (i=0; i<len; ++i)
            {
            level[i]=0;
            power[i]=0;
            scalar[i]=0; /* 18.1.1999 */
            }
        level1=power1=0;

        for (i=0; i<len; ++i)
            {
            switch(x[i])
                {
              case '+':
              case '-':
                    power[i]=1; level[i]=level1;
                    break;
              case '*':
/* A(*,1:5) */      if ((x[i-1]=='(' && x[i+1]==',') ||
                        (x[i-1]==',' && x[i+1]==')'))
                          { level[i]=level1; break; }
                    power[i]=2; level[i]=level1;
                    break;
              case '/':
                    power[i]=2; level[i]=level1;
                    break;
              case '^':
                    power[i]=3; level[i]=level1;
                    break;
              case '(':
                    level[i]=++level1;
                    break;
              case ')':
                    level[i]=level1--;
                    if (level1<0)
                        {
                        syntax_error2("( missing!?");
                        return(-1);
                        }
                    break;
              case '\'':
                    if (i==0)
                        {
                        syntax_error2("Incorrect '");
                        return(-1);
                        }
                    if (x[i-1]==')') level[i]=level1+1;
                    else level[i]=level1;
                    break;
              default:
                    level[i]=level1;
                }
            }
        if (level1>0)
            {
            syntax_error2(") missing!?");
            return(-1);
            }

#if TESTAUS
Rprintf("\n");
for (i=0; i<len; ++i) Rprintf("%d",level[i]);
Rprintf("\n");
for (i=0; i<len; ++i) Rprintf("%d",power[i]);
Rprintf("\n"); sur_getch();
#endif
        return(1);
        }



static int test_scalar2(char *x,int pos,int len)
        {
        int i,j;

#if TESTAUS
Rprintf("test_scalar: %.*s|                        \n",len,x+pos);
#endif
        for (i=pos; i<pos+len; ++i)
            {
            if ((x[i]>='A' && x[i]<='Z') || strchr("!_&",x[i])!=NULL)
                                           // 4-5.12.2004
                {
                if (i==pos) break;
                j=i-1;
                while (strchr("(+-*/:,",x[j])==NULL) --j;
                ++j;
                if (x[j]>='A' && x[j]<='Z') break;
                }
            }
        if (i==pos+len)
            {
            for (i=pos; i<pos+len; ++i) scalar[i]=1;
#if TESTAUS
Rprintf("SCALAR!\n"); sur_getch();
#endif
            return(1);
            }
#if TESTAUS
Rprintf("NOT_scalar!\n"); sur_getch();
#endif
        return(0);
        }

static void test_scalar(char *x,int pos,int len,int op_place)  /* 23.1.1999 */

        {
        int i1,i2;
#if TESTAUS
Rprintf("expression: %.*s|          \n",len,x+pos); sur_getch();
#endif
        i1=test_scalar2(x,pos,op_place-pos);
        i2=test_scalar2(x,op_place+1,len-op_place+pos-1);
        if (i1 && i2) scalar[op_place]=1;
        }

static void find_pairs(char *x,int power1,int scalar_test)
/* int scalar_test; 1=yes 0=no */
        {
        int i,j;
        int len;
        int level1,orig_level;
#if TESTAUS
Rprintf("find_pairs: power=%d\n",power1); sur_getch();
#endif
        len=strlen(x);
        n_pairs[power1]=0;
        for (i=0; i<len; ++i)
            {
            if (power[i]!=power1) continue;
            level1=level[i];
            j=i-1; orig_level=1;
            while (1)
                {
                if (j==0)
                    {
                    if (x[0]=='(')
                        {
                  /*    if (orig_level) */ j=1;
                        }
                    break;
                    }
/**********************************************************************
         0         1         2         3         4         5         6
         01234567890123456789012345678901234567890123456789012345678901
         C+A+B*INV((A+B)*(A+B)'*MTM2((A+B),(B*C)))*(A+B*C)-(A+B)*(A+B)'
level    00000000012222212222221111123333323333321011111110111110111111
power     1 1 2      1  2  1   2       1     2    2  1 2  1  1  2  1
**********************************************************************/
                if (level[j]!=level1) orig_level=0;
                if (orig_level)
                    {
                    if (x[j]=='(' || x[j]==',') { ++j; break; }
                    if (power[j]!=0 && power[j]<=power1) { ++j; break; }
                    --j; continue;
                    }
                /* not anymore orig_level */
                if (level[j]!=level1) { --j; continue; }
                if (power[j]!=0 && power[j]<=power1) { ++j; break; }
                if ((x[j]=='(' || x[j]==',') && power[j]<=power1) { ++j; break; }
                --j;
                }
/*
Rprintf("n=%d j=%d  \n",n_pairs,j); sur_getch();
*/
            pos1[power1][n_pairs[power1]]=j;

            j=i+1; orig_level=1;
            while (1)
                {
                if (j==len-1)
                    {
                    if (x[len-1]==')')
                        {
                  /*    if (orig_level) */ --j;
                        }
                    break;
                    }
                if (level[j]!=level1) orig_level=0;
                if (orig_level)
                    {
                    if (x[j]==')' || x[j]==',') { --j; break; }
                    if (power[j]!=0 && power[j]<=power1) { --j; break; }
                    ++j; continue;
                    }
                /* not anymore orig_level */
                if (level[j]!=level1) { ++j; continue; }
                if (power[j]!=0 && power[j]<=power1) { --j; break; }
                if ((x[j]==')' || x[j]==',') && power[j]<=power1) { --j; break; }
                ++j;
                }

       len2[power1][n_pairs[power1]]=j-pos1[power1][n_pairs[power1]]+1;
/*     len1[power1][n_pairs[power1]]=reduced_length(x,
          pos1[power1][n_pairs[power1]],len2[power1][n_pairs[power1]]);
*/
#if TESTAUS
Rprintf("n=%d pos1=%d len2=%d \n",
  n_pairs[power1],pos1[power1][n_pairs[power1]],len2[power1][n_pairs[power1]]);
sur_getch();
#endif
            if (scalar_test)
                {
#if TESTAUS
Rprintf("op=%d power=%d\n",i,power[i]); sur_getch();
#endif
                test_scalar(x,pos1[power1][n_pairs[power1]],
                              len2[power1][n_pairs[power1]],i);
                }
            ++n_pairs[power1];
            }
        }


static void replace_scalars(char *x,char *x0)
        {
        int i,j,k,len;
        char y[LLENGTH];
#if TESTAUS
Rprintf("replace_scalars: %s|    \n",x); sur_getch();
#endif
        n_scalars=0; j=0; len=strlen(x);
        i=0;
        while (i<len)
            {
            if (scalar[i]==0)
                {
                scalar_expr[n_scalars]=i;
                while (i<len && scalar[i]==0) y[j++]=x[i++];
                }
            k=scalar_expr[n_scalars]=i;
            y[j]=EOS;
            if (i==len) break;
            while (i<len && scalar[i]) ++i;
            if (i<len)
                {
                if (x[k-1]=='(' && x[i]==')')
                    {
                    --scalar_expr[n_scalars];
                    --j;
                    ++i;
                    }
                 }
            sprintf(sbuf,"#%d#",n_scalars); ++n_scalars;
            y[j]=EOS; strcat(y,sbuf); j+=strlen(sbuf);
            if (i<len)
                {
                y[j++]=x[i]; x0[i++]=EOS;
                }
            }
        y[j]=EOS;
        strcpy(x,y);
#if TESTAUS
Rprintf("scalars replaced: %s\n",x); sur_getch();
#endif
        }

static int find_functions(char *x)
        {
        int i,j,n,len; // RS CHA n from local global
        int level1;

        len=strlen(x);
        level1=0;
        n=n_pairs[0]=0; /* # of functions */
        for (i=0; i<len; ++i)
            {
            switch (x[i])
                {
              case '(':
          if (i>0 && strchr("+-*^,(",x[i-1])!=NULL) { ++level1; break; }
                  j=i-1;
                  while (j>=0 && strchr("+-*^,(",x[j])==NULL) --j;
                  pos1[0][n]=j+1;
                  func_level[n]=level1;
                  ++n; ++level1;
                  break;
              case ')':
                  --level1;
                  if (level1<0)
                      {
                      syntax_error2("( missing !?");
                      return(-1);
                      }
                  for (j=0; j<n; ++j)
                      if (func_level[j]==level1) break;
                  if (j==n) break;
                  len2[0][j]=i-pos1[0][j]+1;
/*
                  len1[0][j]=reduced_length(x,pos1[0][j],len2[0][j]);
*/
                  func_level[j]=-1; /* not needed anymore */
                  break;
                }
            }
        if (level1>0)
            {
            syntax_error2(") missing !?");
            return(-1);
            }
        n_pairs[0]=n;
#if TESTAUS
Rprintf("\nfunctions:");
for (i=0; i<n; ++i)
    Rprintf("%d %d       |\n",pos1[0][i],len2[0][i]);
sur_getch();
#endif
        return n_pairs[0];
        }

static int substitute(char *x,char *expr,char *s,int k)
        {
        char y[LLENGTH];
        int i,pos,len;
        int pos0;

        pos0=-999;
        /* previous position of expr, avoiding confusions in A+A+A */
        *y=EOS; pos=0;
        len=strlen(expr);
        for (i=0; i<n_pairs[k]; ++i)
            {
            if (len!=len2[k][i]) continue;
            if (strncmp(x+pos1[k][i],expr,len)==0)
                {
                if (pos0<pos1[k][i])
                    {
                    strncat(y,x+pos,pos1[k][i]-pos);
                    strcat(y,s); pos=pos1[k][i]+len2[k][i];
                    pos0=pos;
                    }
                }
            }
        strcat(y,x+pos);
        strcpy(x,y);
        return(1);
        }

static int index_of_matrix(char *x,char *expr,int k)
        {
        int i;
        char y[LLENGTH];
        char pr_name[8];         /*  %ddddd% */

        if (save_all_temp_mat) { ++max_index; return(max_index); }

#if TESTAUS
Rprintf("index1: %s|%s|%d\n",x,expr,k); sur_getch();
#endif
        if (max_index==0) { max_index=1; return(1); }
        strcpy(y,x);
        substitute(y,expr," ",k);
#if TESTAUS
Rprintf("index1: %s|\n",y); sur_getch();
#endif
        for (i=1; i<=max_index; ++i)
            {
            sprintf(pr_name,"%%%d%%",i);
            if (strstr(y,pr_name)==NULL) break;
            }
        if (i>max_index) ++max_index;
#if TESTAUS
Rprintf("index3: i=%d\n",i); sur_getch();
#endif

        return(i);
        }

static int pr_change(char *s)   /* from %number% to %%number */
        {
        int i;
        char pr_name[8];         /*  %ddddd% */
        char pr_name2[8];        /*  %%ddddd */
        char *p;

        for (i=1; i<=max_index; ++i)
            {
            sprintf(pr_name,"%%%d%%",i);
            sprintf(pr_name2,"%%%%%d",i);
            while (1)
                {
                if ((p=strstr(s,pr_name))==NULL) break;
                strncpy(p,pr_name2,strlen(pr_name2));
                }
            }
        return(1);
        }

static int min_lengths(int k)
        {
        int i,min,imin;

        min=min_len[k]=10000;
        if (n_pairs[k]==0) return(1);
        for (i=0; i<n_pairs[k]; ++i)
            if(len2[k][i]<min) { imin=i; min=len2[k][i]; }
   /*       if(len1[k][i]<min) { imin=i; min=len1[k][i]; }  */
        i_min[k]=imin; min_len[k]=min;
        return(1);
        }

static void simplify_to_mmt_mtm(char *s)
        {
        char *p;
        char x[LNAME];
        int len,transp_at_end;

        strcpy(x,s);
        transp_at_end=0;
        len=strlen(x);
        if (s[len-1]=='\'') transp_at_end=1;
        p=strstr(x,"'*");
        if (p!=NULL && !transp_at_end)
            {
            *p=EOS;
            if (strcmp(x,p+2)==0)
                {
                sprintf(s,"MTM(%s)",x);
                }
            return;
            }
        p=strchr(x,'*');
        if (p!=NULL && transp_at_end)
            {
            x[len-1]=EOS;
            *p=EOS;
            if (strcmp(x,p+1)==0)
                {
                sprintf(s,"MMT(%s)",x);
                }
            return;
            }
        return;
        }

static void insert_scalars(char *x,char *x0)
        {
        int i,k;
        char y[16];
        char *p;
        char x2[LLENGTH];

        if (!n_scalars) return;
        for (i=0; i<n_scalars; ++i)
            {
            sprintf(y,"#%d#",i);
            p=strstr(x,y); if (p==NULL) continue;
            k=p-x;
            *x2=EOS; strncat(x2,x,k); p=strchr(p+1,'#');
            strcat(x2,x0+scalar_expr[i]); strcat(x2,p+1);
            strcpy(x,x2);
            }
        }


static int set_mat_command(char *x)
        {
        int i,min,imin,n_laji,k;
        char y[LLENGTH];
        char expr[LNAME];
        char expr2[LNAME];
        char pr_name[8];         /*  %ddddd% */
        char pr_name2[8];        /*  %%ddddd */

        for (i=0; i<4; ++i) min_lengths(i);

        n_laji=0; min=10000;

        for (i=3; i>=0; --i)
            {
            if (n_pairs[i]) ++n_laji;
            if (min_len[i]<min)
                { min=min_len[i]; imin=i_min[i]; k=i; }
            }
#if TESTAUS
Rprintf("n_laji=%d\n",n_laji); sur_getch();
#endif
        if (n_mat && n_laji==0 && x[strlen(x)-1]=='\'')
            {
            i=index_of_matrix(x,expr,k);
            sprintf(pr_name,"%%%d%%",i);
            sprintf(pr_name2,"%%%%%d",i);
            pr_change(x);
            remove_extra_quotas(x);
            insert_scalars(x,x0); /* 18.1.1999 */
            simplify_to_mmt_mtm(expr2);
            sprintf(sbuf,"%s=%s\n",pr_name2,x);
            expr_size+=strlen(sbuf);
            if (expr_size>=mat_parser) { err_mat_parser(); exit(0); }
            strcat(expr_space,sbuf);
/*          fprintf(survomat,"%s",sbuf);     */
            ++n_mat;
            return(0);
            }
        if (n_laji==0) return(0);
        if (n_mat==0 && n_laji==1 && n_pairs[k]==1 && strchr(x,'\'')==NULL) return(0);
#if TESTAUS
Rprintf("min=%.*s|\n",len2[k][imin],x+pos1[k][imin]); sur_getch();
#endif

        *expr=EOS; strncat(expr,x+pos1[k][imin],len2[k][imin]);
        strcpy(expr2,expr);
        remove_parentheses(expr2);
        i=index_of_matrix(x,expr,k);
        sprintf(pr_name,"%%%d%%",i);
        sprintf(pr_name2,"%%%%%d",i);
        pr_change(expr2);
        remove_extra_quotas(expr2);
        insert_scalars(expr2,x0); /* 18.1.1999 */
        simplify_to_mmt_mtm(expr2);
        sprintf(sbuf,"%s=%s\n",pr_name2,expr2);
        expr_size+=strlen(sbuf);
#if TESTAUS
Rprintf("expr_size=%d mat_parser=%d\n",expr_size,mat_parser); sur_getch();
#endif
        if (expr_size>=mat_parser) { err_mat_parser(); exit(0); }
        strcat(expr_space,sbuf);
/*      fprintf(survomat,"%s",sbuf);     */
        ++n_mat;
#if TESTAUS
Rprintf("%s=%s\n",pr_name,expr); sur_getch();
#endif
        substitute(x,expr,pr_name,k);
#if TESTAUS
Rprintf("subs: %s|\n",x); sur_getch();
#endif
        return(1);
        }
            

static int matpar2(char *expr)
        {
        int i;
        char x[LLENGTH];
        int current_power;

        n_pairs[0]=1; /* # of functions for the first find_functions( */

        strcpy(x,"("); strcat(x,expr); strcat(x,")"); /* 9.1.1999 */
        remove_extra_quotas(x);   /* remove '' */
        replace_minus_by_minus_one_coeffs(x); // 17.5.2008
        max_index=0;

        i=create_arrays(x); if (i<0) return(-1);
        n_pairs[1]=0;
        if (strchr(x,'+')!=NULL || strchr(x,'-')!=NULL)
            find_pairs(x,1,1);
        n_pairs[2]=0;
        if (strchr(x,'*')!=NULL || strchr(x,'/')!=NULL)
            find_pairs(x,2,1);
        n_pairs[3]=0;
        if (strchr(x,'^')!=NULL) find_pairs(x,3,1);
        if (n_pairs[0])
            {
            n_pairs[0]=find_functions(x);
            if (n_pairs[0]<0) return(-1);
            }

#if TESTAUS
Rprintf("Scalars\n");
for (i=0; i<strlen(x); ++i) Rprintf("%d",scalar[i]); Rprintf("\n");
sur_getch();
#endif

        strcpy(x0,x);  /* x0 globaali */
        replace_scalars(x,x0);
        n_pairs[0]=1; /* # of functions for the first find_functions( */
while (1)
        {
        i=create_arrays(x); if (i<0) return(-1);
        n_pairs[1]=0;
        if (strchr(x,'+')!=NULL || strchr(x,'-')!=NULL)
            find_pairs(x,1,0);
        n_pairs[2]=0;
        if (strchr(x,'*')!=NULL || strchr(x,'/')!=NULL)
            find_pairs(x,2,0);
        n_pairs[3]=0;
        if (strchr(x,'^')!=NULL) find_pairs(x,3,0);
        if (n_pairs[0])
            {
            n_pairs[0]=find_functions(x);
            if (n_pairs[0]<0) return(-1);
            }

        i=set_mat_command(x);
        if (i==0 && n_mat==0) return(0); /* common MAT operation */
        if (i==0) return(1); /* composite MAT command */
        } /* while */

        return(1);
        }

static int matpar(
char *q, /* name */
char *p /* expression */
)
        {
        int i,n; // RS CHA n from local global
        char y[LNAME];

        if (*p=='+' || *p=='-')  // 17.5.2008
            {
            sur_print("\n+ or - in front of expression not permitted!");
            mat_error();
            }

        if (expr_space==NULL)
            {
            expr_space=malloc(mat_parser);
            if (expr_space==NULL) { not_enough_space(); return(-1); }
            level=(int *)malloc((mat_parser+1)*sizeof(int));
            if (level==NULL) { not_enough_space(); return(-1); }
            power=(int *)malloc((mat_parser+1)*sizeof(int));
            if (power==NULL) { not_enough_space(); return(-1); }
            scalar=(int *)malloc((mat_parser+1)*sizeof(int));
            if (scalar==NULL) { not_enough_space(); return(-1); }
            }

        *expr_space=EOS; expr_size=0;
        n=0; n_mat=0;
        if (strlen(p)>2*mat_parser) { err_mat_parser(); return(-1); } // RS CHA exit -> return
        i=matpar2(p);
        clean_expr_space(q);

        if (n_mat>1)
            {
            strcpy(y,etmpd); strcat(y,"SURVOMAT.TMP");
            survomat=muste_fopen(y,"w+t");
            if (survomat==NULL)
                {
                sprintf(sbuf,"\nCannot open %s!",y);
                sur_print(sbuf); WAIT; exit(0);
                }
            fprintf(survomat,"MAT expression: %s=%s\n%s",q,p,expr_space);
            fclose(survomat);
            }


/*
sur_print("\n");
sur_print(expr_space);
sprintf(sbuf,"n_mat=%d\n",n_mat);
sur_print(sbuf);
*/
#if TESTAUS
Rprintf("i=%d\n",i); sur_getch();
#endif
        return(n_mat);
        }



int muste_mat(int argc,char *argv[])
        {
        int i,jrivi,k;
        char x[LLENGTH],*word2[2];
        int rem_print=0;
        char *p,*q;
// RS REM        extern char *next_mat_command();
        int part_copy; /* MAT A(i,j)=...  5.6.1999 */
        char part_name[LNAME];

// RS Variable init
mtx=nmtx=0;
sp_read=0;
save_all_temp_mat=0;
n_mat=0;
mtx=0;
mX=nX=typeX=lrX=lcX=0;
mY=nY=typeY=lrY=lcY=0;
mT=nT=typeT=lrT=lcT=0;
rowcomments=NULL;
n_row_comments=0;
len_row_comments=0L;
l_virhe=0;
nmat=0;
n_mat=0;
max_index=expr_size=n_scalars=0;
save_all_temp_mat=0;




//      strcpy(info_s,argv[2]); korvattu: s_opt
/*****************************************
printf("\ninfo_s=%s|",info_s);
strcpy(sbuf,info_s);
for (i=0; i<strlen(sbuf); ++i) sbuf[i]-=19;
printf("\n sbuf=%s|",sbuf); sur_getch();
*********************************************/
        s_init(argv[1]);
// RS REM Rajoitustarkistuksia        s_opt(argv[2]);
        if (g==1)
            {
            sur_print("\nIncomplete MAT command!");
            sur_print("\nSee MAT?               ");
            WAIT; return(1);
            }
        if (*word[0]=='%') save_all_temp_mat=1;

        strcpy(mat_argv1,argv[1]);
        mat_argc=argc;
        edread(comline2,r1+r-1); /* 12.1.1999 */
        mtx=0;


// RS REM Rajoitustarkistus        if (!opt("M",0,10)) err(1);

        if (muste_strcmpi(word[0],"MATRUN")==0)
            {
            mtx=1; i=mtx_open(0L);
            if (i<0) return(1);
            }
        while (1)
            {
            if (mtx)
                {
                i=mtx_read(comline);

                if (i<0) break;
                strcpy(comline2,comline);
                if (i==0) continue;
                g=split(comline,word,MAXPARM);
                if (g>2 && muste_strcmpi(word[1],"REM")==0 && muste_strcmpi(word[2],"PRINT")==0)
                    {
                    if (g>3) i=word[3]-comline; else i=word[2]-comline+5;
                    if (rem_print) ++jrivi; else jrivi=1+lastline2();

                    if (jrivi>r2) jrivi=0;
                    strcpy(x,comline2+i);
                    mtx_scalar_print(x);
                    if (!rem_print) k=output_open(eout);
                    if (k>=0)
                        {
                        output_line(x,eout,jrivi);
                        rem_print=1;
                        }
                    }
                else
                    {
                    sprintf(sbuf,"\n%s",comline2); sur_print(sbuf);
                    if (rem_print) { output_close(eout); rem_print=0; }
                    }

                }

            part_copy=0; /* 5.6.1999 */
            if (g>2 && strchr(word[1],'(')!=NULL &&
                strchr(word[2],'=')!=NULL) part_copy=1;
// printf("part_copy=%d\n",part_copy); sur_getch();

/*          if (!opt("M",0,472)) return;   */
            if (!rem_print)
                {
                if (mtx || mat_parser==0)
                    {
                    i=matrix_op();
                    if (i<0) { mat_error(); return(-1); } // RS FIXME Close files, free memory
                    }
                else
                    {
/*
printf("mat_parser=%d\n",mat_parser); sur_getch();
printf("comline2=%s|\n",comline2); sur_getch();
*/
                    p=NULL;     /* 14.4.1999  MAT A=expression? */
                    strcpy(x,comline2);
                    if (part_copy)
                        {
                        word2[0]=x+(word[0]-comline);
                        p=strchr(word2[0],' '); *p=EOS;
                        word2[1]=x+(word[1]-comline);
                        p=strchr(word2[1],'='); *p=EOS;
                        p=comline2+(p-x);
                        }
                    else
                        {
                        i=split(x,word2,2);
                        if (i==2)
                            {
                            p=strchr(word2[1],'=');
                            if (p!=NULL) p=strchr(comline2,'=');
                            }
                        }
               /*
                    p=strchr(comline2,'=');
               */

                    if (p==NULL || *(p+1)=='-')
                        {
                        i=matrix_op();
                        if (i<0) { mat_error(); return(-1); } // RS FIXME Close files, free memory
                        }
                    else
                      {
                      q=strstr(comline2," / ");
                      if (q!=NULL) *q=EOS;
                      q=p-1; while (q>comline2 && *q!=' ') --q; ++q;
                      *p=EOS; ++p;

                      i=strlen(p)-1; while (p[i]==' ') p[i--]=EOS;

                      if (part_copy)
                          {
                          strcpy(part_name,q);
                          q="&&1";
                          }

                      n_mat=matpar(q,p);
// printf("n_mat=%d\n",n_mat); sur_getch();
// printf("expr_space=%s\n",expr_space); sur_getch();
                      if (part_copy && *expr_space==EOS)
                                                { n_mat=0; i=matrix_op();
                                                  if (i<0) { mat_error(); return(-1); } // RS FIXME Close files, free memory
                                                }
                      else if (!part_copy && (n_mat==1 || n_mat==0))
                          { n_mat=0; i=matrix_op(); if (i<0) { mat_error(); return(-1); } } // RS FIXME Close files, free memory }
                      else if (n_mat<0) { s_end(argv[1]); return(0); } // RS CHA exit -> return
                      else
                        {
                        p=expr_space;
                        for (k=0; k<n_mat; ++k)
                          {
                          strcpy(comline,"*MAT ");
                          p=next_mat_command(p,comline+5);
//  printf("command=%s|\n",comline); sur_getch();
                          g=split(comline+1,word,MAXPARM);
                          i=matrix_op();
                          if (i<0) { mat_error(); return(-1); } // RS FIXME Close files, free memory
                          }
                        n_mat=0;
                        if (part_copy)
                            {
                            sprintf(comline,"*MAT %s=&&1",part_name);
                            g=split(comline+1,word,MAXPARM);
                            i=matrix_op();
                            if (i<0) { mat_error(); return(-1); } // RS FIXME Close files, free memory
                            }                            
                        }
                      }
                    }
                }

            if (mtx)
                {
                vapauta();
                continue;
                }
            if (i<0) erun=0;
            ++r;
            if (erun==0) break;

            edread(comline,r1+r-1);
            strcpy(comline2,comline);

            strcpy(x,"RUN: "); strncat(x,comline+1,c3);
            g=split(comline+1,word,MAXPARM);
            for (i=0; i<g; ++i)   /* 6.11.1992 */
                if(word[i][0]=='/' && word[i][1]==EOS) break;
            g=i;
            if (strncmp(muste_strupr(word[0]),"MAT",3)!=0) break;

            /* "lapsenlapsen vÑlttÑmiseksi 29.2.2000 */
            if (g>2 && muste_strcmpi(word[1],"SAVE")==0 &&
                muste_strcmpi(word[2],"DATA")==0 ) break;

            LOCATE(r3+2,1); PR_EINV; sprintf(sbuf,"%s",x); sur_print(sbuf); PR_ENRM;
/*          vapauta();    */
            }
        if (rem_print) { output_close(eout); rem_print=0; }
        if (!mtx)
            {
            --r;
            if (r>r3) { i=r1+r-1; r1=i-r3+1; r=i-r1+1; }
            s_end(argv[1]);
            }
        else
            {
            s_end(argv[1]);
            return(1);
            /* execl(mtx_prog,mtx_prog,argv[1],NULL); */
            }
        s_end(argv[1]);
        return(1);
        }

