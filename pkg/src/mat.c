/*  !mat.c 21.9.1985/SM (6.11.1992) (24.11.1995)
.............................
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
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

#define MAXIT 100

#define MAXEARG 1000 // RS CHA 255
#define EARG '\376'

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
extern char **spb2;

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
static char mat_name_mat[NMAT][9];

static double *pow1,*pow2;
static double *d,*e;

static int *xi;

static char *argv1;
static int *freq;

static char expr[3*LLENGTH];
static int line_nr;

static int typeA,typeB;
static int diagA,diagB;

static long nn;
static int rand_type;
static double seed;
static long *unit;
static int same;

static double *cos_t,*cos_u,*cos_v,*cos_u2;
static int *cos_i,*cos_imax;
static int *sel2;
static int *sel,*sel_max;
static int dim;
static double det;
static int sh;
static FILE *sh_file;

static int ii,jj;
static int mm;

static int earg_varattu=0;
static int n_earg=0;
static double *earg;
char rec_func[16]; /* 5.7.1998 */
static int l_virhe;

/* specifications in the edit field */
extern char *splist;
extern char **spa, **spb, **spshad;
extern char **spb2;
extern int spn;
extern char *spl;
extern int global;
extern double *arvo; /* vain arit.c tarvitsee  */


static char *nimet[]={ "TRANSFORM", "MULT", "SAMPLES", "INDVAR", "MERGE", "MINDIFF",
                "COLSORT", "CRSORT", "EIGEN", "CONVOLUTION",
                "MAXDET", "U_TO_F", "INTSCAL", "FRAC_TO_DEC", "SAMPLE",
                "SORT", "MAGIC", "JACKKNIFE", "#EIGFEW",  "#EIGLAN",
                "AGGRE", "PERMORD", "FREQ", "RCSORT", "PRODDIAG",
                "TAB", "SMOOTH",
                "#" };

// RS REM static char **specs=nimet;


static int laske(); // RS Declaration
static int matload();

static int vapauta()
        {
muste_fixme("FIXME: mat, function vapauta() not freeing memory!");       
        return(1);
        if (X!=NULL) muste_free(X);
        if (rlabX!=NULL) muste_free(rlabX); if(clabX!=NULL) muste_free(clabX);
        if (Y!=NULL) muste_free(Y);
        if (rlabY!=NULL) muste_free(rlabY); if(clabY!=NULL) muste_free(clabY);
        if (T!=NULL) muste_free(T);
        if (rlabT!=NULL) muste_free(rlabT); if(clabT!=NULL) muste_free(clabT);
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

/* RS REM not used?
static int ei_sallittu()
        {
        muste_fixme("\nFIXME: mat, ei_sallittu, close files, free memory"); // RS FIXME            
        sur_print("\nIncorrect _MAT operation!"); WAIT;
        return(-1); // RS CHA exit(1);
        }
*/

static int spfind_mat(char *s) /* 4.3.1995 */
        {
        int i,j;
/*
printf("spec: spfind: s=%s spn=%d\n",s,spn); getch();
for (i=0; i<spn; ++i)
    printf("i=%d spa=%s spb=%s\n",i,spa[i],spb[i]);
getch();
*/
        for (i=0; i<spn; ++i)
                if (strcmp(s,spa[i])==0) break;
        if (i<spn && ( spb[i]==NULL || *spb[i]!='*')) return(i);
        if (i==spn) return(-1);
        for (j=0; j<spn; ++j)
                if (strcmp(spb[i],spa[j])==0) return(j);
        return(i);
        }


static int jatkorivit_mat(int j)
        {
        char x[LLENGTH];
        char *p,*q;

        while (1)
            {
            edread(x,j);
            p=x+1;
            while (*p==' ') ++p;
            q=p; while (*q!=' ') ++q; *q=EOS;
            *(spl-2)=EOS; --spl;
            if (spl-splist+strlen(p)+2>speclist) return(-1);
            strcat(spl-1,p); spl+=strlen(p);
            if (*(spl-2)!='&') break;
            ++j;
            }
        return(1);
        }

static int spread3_mat(char *x,int j) /* rivi. Jos j=0, erikoisrivi (mtx) */
        {
        int i,k,pos;
        char *p;
        char xs[LLENGTH];
        int pit;

        if (j==0) pit=LLENGTH-1; else pit=ed1;
        pos=1;
        while (pos<pit)
            {
            p=strchr(x+pos,'=');
            if (p==NULL) break;

/* Aktivoidun kohdan ohittaminen
            if (j==r1+r-1 && p-x==c1+c-2) break;
*/
            if (spn>=specmax) return(-spn);
            pos=p-x; i=pos-1;
            while (i>0 && x[i]!=' ') --i;
            if (spl-splist+pos-i+1>speclist) return(-spn);
            strncpy(spl,x+i+1,pos-i-1);
            spa[spn]=spl; spl+=pos-i; *(spl-1)=EOS;
            i=pos+1;
            while (i<ed1 && x[i]!=' ') ++i;
            if (spl-splist+i-pos+1>speclist) return(-spn);
            strncpy(spl,x+pos+1,i-pos-1);
            spb[spn++]=spl; spl+=i-pos; *(spl-1)=EOS;

            if (*(spl-2)=='&') { k=jatkorivit_mat(j+1);
                                 if (k<0) return(-spn);
                               }
            spshad[spn-1]=NULL;
            if (j && zs[j]!=0)
                {
                edread(xs,zs[j]);
                if (spl-splist+i-pos+1>speclist) return(-spn);
                strncpy(spl,xs+pos+1,i-pos-1);
                spshad[spn-1]=spl; spl+=i-pos; *(spl-1)=EOS;
                }

            ++pos;
            }
        return(spn);
        }

static int instr_mat (char s[],char c[])
        {
        char *p;
        short lenc=strlen(c);
        short i=0;
        while (*(s+i))
            {
            p=strchr(s+i,*c);
            if (p==NULL) { i=-1; break; }
            if (strncmp(s+i,c,lenc)==0) break;
            ++i;
            }
        return (i);
        }

static int spread2_mat(int lin,int *raja1)
        {
        char raja[12];
        int j,i;
        char x[LLENGTH];

        strcpy(raja,"*..........");
        for (j=lin-1; j>0; --j)
            {
            edread(x,j);
            i=instr_mat(x,raja);
            if (i>=0) break;
            }
        *raja1=j;
        for (j=*raja1+1; j<=ed2; ++j)
            {
            edread(x,j);
            if (global==1)
                {
                i=instr_mat(x,"*GLOBAL*");
                if (i>0) global=0;
                }
            i=instr_mat(x,raja);
            if (i>=0) break;
            spn=spread3_mat(x,j); if (spn<0) return(spn);
            }


/*  printf("\n"); for (i=0; i<spn; ++i) printf("\n%s=%s varjo=%s",
                                         spa[i],spb[i],spshad[i]); getch();
*/
        return (spn);
        }

static int sp_check_mat()
        {
        int i,j;
        unsigned int n,tila,tila0;
        int varjo;
        char x[LLENGTH];
        char *p,*q;

        n=0; tila=0;
        for (j=1; j<=r2; ++j)
            {
            edread(x,j);
            *x=EOS; p=x+1;
            while (1)
                {
                p=strchr(p,'='); if (p==NULL) break;
                if (*(p+1)==EOS) break;
                if (*(p+1)=='=') { p+=1; continue; }
                ++n;
                tila0=tila; varjo=zs[j];
                tila+=2; /* 2*EOS */
                q=p-1; while (*q && *q!=' ') { ++tila; --q; }
                q=p+1; while (*q && *q!=' ') { ++tila; ++q; }

                while (*(q-1)=='&')
                    {
                    ++j; if (j>r2) break;
                    edread(x,j);
                    q=x+1; while (*q && *q==' ') ++q;
                    if (*q==EOS) break;
                    while (*q && *q!=' ') { ++tila; ++q; }
                    }
                if (varjo) tila+=tila-tila0;
                ++p;
                }
            }
        specmax=n; speclist=tila;
        return(1);
        }


static int sp_init_mat(int lin)
        {
        int tila;
        char *p;
        int spn1;
        int raja1;
        char x[LLENGTH];
        int i,k;
        char *s[2];

int own_spec_line1=0; // 3.10.2010
int own_spec_line2=0;

        edread(x,lin);

        if ((p=strstr(x,"SPECS="))!=NULL) // 3.10.2010
            {
            i=0;
            while(p[i]!=' ') ++i; p[i]=EOS;
            i=split(p+6,s,2);
            k=edline2(s[0],1,1); if (k<0) return(-1);

            if (i==1)
                {
                if (k<lin) { own_spec_line1=k; own_spec_line2=lin-1; }
                else if (k>lin) { own_spec_line1=lin+1; own_spec_line2=k; }
                }
            else if (i==2)
                {
                own_spec_line1=k;
                k=edline2(s[1],k,1); if (k<0) return(-1);
                own_spec_line2=k;
                }
//        printf("\nown_lines: %d %d",own_spec_line1,own_spec_line2);
//        getch();
            }

        sp_check_mat();
        specmax+=32; speclist+=2*LLENGTH; /* mtx */
        if (own_spec_line1) { specmax*=2; speclist*=2; } // 11.11.2010

        tila=speclist+3*specmax*sizeof(char **)+specmax*sizeof(double);
        splist=muste_malloc(tila);
        if (splist==NULL)
            {
            sur_print("\nNot enough memory for specifications!");
            WAIT; return(-1);
            }
        p=splist+speclist;
        spa=(char **)p; p+=specmax*sizeof(char *);
        spb=(char **)p; p+=specmax*sizeof(char *);
        spshad=(char **)p; p+=specmax*sizeof(char *);
        arvo=(double *)p; p+=specmax*sizeof(double);
/*   printf("\ntila %d %d",tila,p-splist); getch();     */
        spn=0; spl=splist; global=0;

        if (lin==0) { spn=spread3_mat(info,0); return(spn); }

        edread(x,lin); spn=spread3_mat(x,lin); if (spn<0) return(spn);

        if (own_spec_line1) // 3.10.2010
            {
            for (i=own_spec_line1; i<=own_spec_line2; ++i)
                {
                edread(x,i);
                spn=spread3_mat(x,i);
                if (spn<0) return(spn);
                }
            }

        spn=spread2_mat(lin,&raja1);
        if (spn<0) return(spn);
        if (raja1==0) return(spn);
        spn1=spn;
        global=1;
        spn=spread2_mat(1,&raja1);
        if (spn<0) return(spn);
        if (global==1) spn=spn1;
        return(spn);
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

// RS REM        if (*A!=NULL) { muste_free(*A); *A=NULL; }
/*      if ( (long)m*n*sizeof(double)>MAXTILA )
                { ei_tilaa(); return(-1); }
*/
/*
printf("tila=%ld\n",m*n*sizeof(double)); sur_getch();
*/
// RS CHA        *A=(double *)muste_malloc(m*n*sizeof(double));
		*A=(double *)muste_realloc(*A,m*n*sizeof(double));
/*
printf("a\n"); sur_getch();
*/
        if (*A==NULL) { ei_tilaa(); return(-1); }
                               /*  printf("\nmat-tila varattu! %d",m*n); */
        if (rlab!=NULL)
            {
// RS REM            if (*rlab!=NULL) { muste_free(*rlab); *rlab=NULL; }
// RS CHA            *rlab=(char *)muste_malloc(m*mcr+1);
            *rlab=(char *)muste_realloc(*rlab,m*mcr+1);

            if (*rlab==NULL) { ei_tilaa(); return(-1); }
                               /* printf("\nrlab-tila varattu! %d %d",m,mcr); */
            }
        if (clab!=NULL)
            {
// RS REM            if (*clab!=NULL) { muste_free(*clab); *clab=NULL; }
// RS CHA            *clab=(char *)muste_malloc(n*mcl+1);
            *clab=(char *)muste_realloc(*clab,n*mcl+1);

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

    i=sp_init_mat(j); sp_read=1;
    if (i<0)
        {
        sur_print("\nToo many specifications!");
        if (etu==2) { strcpy(tut_info,"MATerr@"); return(-1); } // RS CHA exit(1)
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
        int i,j=0,k;
        double xx[2];
// RS REM        char *lab;
        int type;

// printf("f=%s nn=%d %s %s\n",f,nn,s[0],s[1]); sur_getch();

        for (k=0; k<nmat; ++k)
            {
            if (strcmp(f,mat_name_mat[k])==0) break;
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
            strcpy(mat_name_mat[k],f);
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
// RS REM        int i;
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
        i=spfind_mat(muuttuja);
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

double muste_arit_atof(char *lauseke)
		{
		return(arit_atof(lauseke));
		}


static int arit_atoi(char *lauseke)
        {
        return((int)arit_atof(lauseke));
        }

int muste_arit_atoi(char *lauseke)
        {
        return((int)arit_atof(lauseke));
        }        
        

static int name(char *matfile,char *matr)
        {
// RS REM        int i;

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
        if (mname<=1) { muste_fclose(MAT); return(0); }
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
        if (rowcomments!=NULL) muste_fclose(rowcomments);
        muste_fclose(MAT);
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
        int i,j,j1,j2; // ,i1,i2
        int mname,mc,mcl,mrl; // ,mr,mat;
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
        if (check) { muste_fclose(MAT); return(1); }
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
        muste_fclose(MAT);
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
            if (mtx) spn=sp_init_mat(0); else spn=sp_init_mat(r1+r-1);
            if (spn<0) return(-1); sp_read=1;
            }
        spn=spread3_mat(s,0); if (spn<0) return(-1);
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

static int text_labels2(char *lab,int n,char *text,int base)
        {
        char *t,*p;
        int pit;
        char label[32];
        int i,j;
        int len;

        len=8;
        if (*text=='"') t=text+1; else t=text;
        p=strchr(t,'"'); if (p!=NULL) *p=EOS;
        pit=strlen(t);
        for (i=0; i<n*len; ++i) lab[i]=' ';
        for (i=0; i<n; ++i)
            {
            sprintf(label,"%s%d",t,i+base);
            for (j=0; j<len; ++j)
                {
                if (label[j]==EOS) break;
                lab[i*len+j]=label[j];
                }
            }
        return(1);
        }


static int text_labels(char *lab,int n,int len,char *text)
        {
        char *t,*p;
        int pit;
        char label[32];
        int i,j;

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

/*
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
*/

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
            i=spfind_mat(matr);
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
        muste_fclose(MAT);
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

        pow1=(double *)muste_malloc(mX*sizeof(double));
        if (pow1==NULL) { ei_tilaa(); return(-1); }
        pow2=(double *)muste_malloc(mX*mX*sizeof(double));
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
        muste_free(pow1); pow1=NULL; muste_free(pow2); pow2=NULL;
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
        pow1=(double *)muste_malloc(mX*mX*sizeof(double));
        if (pow1==NULL) { ei_tilaa(); return(-1); }
        pow2=(double *)muste_malloc(mX*mX*sizeof(double));
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
        muste_free(pow2); pow2=NULL; muste_free(pow1); pow1=NULL;
        return(1);
        }

static int op_kill()   /* MAT KILL A,B,C */
        {
        int i;
        char file[64];
// RS REM        char command[LLENGTH];

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
// RS REM        double dbl;

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
            i=spfind_mat("EPS"); if (i>=0) eps=atof(spb[i]);
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
// RS REM        char x[9];
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
// RS REM        char s[9];
// RS REM        char t[25];

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
// RS REM        double a;

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
    i=spfind_mat("EPS"); if (i>=0) eps=atof(spb[i]);

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
    i=spfind_mat("EPS"); if (i>=0) eps=atof(spb[i]);

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
    i=spfind_mat("EPS"); if (i>=0) eps=atof(spb[i]);

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
// RS REM        char *p;

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

static int sort_perm(double *x,int m)
        {
        int i,h,k,ii;
        int ind;
        double y;

        xi=(int *)muste_malloc(m*sizeof(int));
        if (xi==NULL) { not_enough_memory(); return(-1); } // RS CHA exit(1)
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
        muste_free(xi); xi=NULL;
        return(1);
        }


static int op_perm(int mode)
        {
        int i,j;
        int k,m,n;
// RS REM        double min;

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
            i=sort_perm(Y,n);  /* Y values -> order indices (1,2,...,n) */
            if (i<0) return(-1); // RS ADD
            
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
        e=(double *)muste_malloc(mX*sizeof(double));
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
extern void muste_matsda();
static int op_save()
        {
        extern double arit_atof();
        int i,j,k,len;
        int matnimi, matrivi;
        char x[LLENGTH], *sana[MAXCOL];
        int ots, arivi, jmax;
        int maxrlab; // RS REM , kesken;
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
          { muste_matsda(); return(1); } // RS CHA save_data -> matsda

        if (g>=5) matnimi=4; else matnimi=2;

        matrivi=wfind("MATRIX",word[2],1);

        if (matrivi<0)
            {
            if (muste_strcmpi(word[2],"AS")==0) // RS 
              {
              ots=0; arivi=r+1; matnimi=3; matrivi=arivi;
              }
            else
              {
              PR_EBLD;
              sprintf(sbuf,"\nMatrix %s not in the edit field!",word[2]);
              sur_print(sbuf); WAIT; PR_ENRM; return(-1);
              }
            }
        else
          {    
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
        row_comments=muste_malloc(len_row_comments);
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
        if (!sp_read) { i=sp_init_mat(r1+r-1); sp_read=1; }
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
        int i; // RS REM ,j,h;
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

        piv=(int *)muste_malloc(nX*sizeof(int));
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

        muste_free(piv); piv=NULL;
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
// RS REM        char expr1[LLENGTH];
        int i,j,ii,jj;
        int all_rows,all_cols;
        int *r_sel,*c_sel; // RS CHA from local global
        char *r_lab,*c_lab; // RS CHA from local global

        r_sel=NULL; c_sel=NULL;
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
            r_sel=(int *)muste_malloc(mX*sizeof(int));
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
            c_sel=(int *)muste_malloc(nX*sizeof(int));
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
            r_lab=muste_malloc(mT*lrX);
            if (r_lab==NULL) { not_enough_memory(); return(-1); }
            select_labels(rlabX,mX,lrX,mT,r_sel,r_lab);
            }

        if (all_cols) c_lab=clabX;
        else
            {
            c_lab=muste_malloc(nT*lcX);
            if (c_lab==NULL) { not_enough_memory(); return(-1); }
            select_labels(clabX,nX,lcX,nT,c_sel,c_lab);
            }
        sprintf(tnimi,"SUB(%s,%s,%s)",exprX,word[2],word[3]);
        nim(tnimi,exprX);

        i=mat_save(tulos,T,mT,nT,r_lab,c_lab,lrX,lcX,-1,exprX,0,0);
//      merkitse(tulos,exprX,i,nX,nX);
        muste_free(c_lab); c_lab=NULL; muste_free(r_lab); r_lab=NULL;
        muste_free(c_sel); c_sel=NULL; muste_free(r_sel); r_sel=NULL;
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
// RS REM        int i;
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
// RS REM        char mat_prog[LNAME];

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
        int i,n,k; // RS REM ,h,s,u,j,m;
// RS REM        double min;
// RS REM        int smin;
// RS REM        char expr1[2*LLENGTH];

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
        freq=(int *)muste_malloc((n+1)*sizeof(int));
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
        int smin=0;
// RS REM        char expr1[2*LLENGTH];

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
        perm=(int *)muste_malloc((k+1)*sizeof(int));
        perm2=(int *)muste_malloc((k+1)*sizeof(int));
        row=(double *)muste_malloc((k+1)*sizeof(double));
        permord_fact=(int *)muste_malloc((k+1)*sizeof(int));
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
// RS REM        double min;
// RS REM        char expr1[2*LLENGTH];

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
        v=(double *)muste_malloc(n*sizeof(double));
        w=(double *)muste_malloc(n*sizeof(double));

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
// RS REM        char expr1[2*LLENGTH];
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
        d=(double *)muste_malloc(m*sizeof(double));

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
// RS REM        char expr1[2*LLENGTH];
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

        i=spfind_mat(word[4]); if (i<0) return(1);
        tab_limits(0,spb[i]);
// printf("\nlimits: %g %g %g",min[0],step[0],max[0]); getch();
        i=spfind_mat(word[5]); if (i<0) return(1);
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
// RS REM        char expr1[2*LLENGTH];
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

/* #mult.c 19.11.1995/SM (27.11.1995)
*/

#define DIAGONAL 0
#define MULTIDIAGONAL 1
#define LOWER_TRIANGULAR 2
#define UPPER_TRIANGULAR 3
#define GENERAL 9

// RS REM extern long clock();


static int type_not_known(char *s)
        {
        sprintf(sbuf,"\nType `%s' of matrix not known or not accepted!",s);
        sur_print(sbuf); WAIT; return(1);
        }

static int compute()
        {
        int i,j,k,h,k2,h2; // ,ii;
        double a;

        switch (typeA)
            {
          case DIAGONAL:
            switch (typeB)
                {
              case DIAGONAL:
                for (i=0; i<mT; ++i) T[i*(mT+1)]=X[i*(mT+1)]*Y[i*(mT+1)];
                break;
              case MULTIDIAGONAL:
                for (i=0; i<mT; ++i)
                    {
                    k=i-diagB; if (k<0) k=0; h=i+diagB; if (h>=nT) h=nT-1;
                    a=X[i*(mX+1)];
                    for (j=k; j<=h; ++j) T[i+mT*j]=a*Y[i+mY*j];
                    }
                break;
              case LOWER_TRIANGULAR:
                for (i=0; i<mT; ++i)
                    {
                    a=X[i*(mX+1)];
                    for (j=0; j<=i; ++j) T[i+mT*j]=a*Y[i+mY*j];
                    }
                break;
              case UPPER_TRIANGULAR:
                for (i=0; i<mT; ++i)
                    {
                    a=X[i*(mX+1)];
                    for (j=i; j<nY; ++j) T[i+mT*j]=a*Y[i+mY*j];
                    }
                break;
              case GENERAL:
                for (i=0; i<mT; ++i)
                    {
                    a=X[i*(mX+1)];
                    for (j=0; j<nY; ++j) T[i+mT*j]=a*Y[i+mY*j];
                    }
                break;
                }
            break;

          case MULTIDIAGONAL:
            switch (typeB)
                {
              case DIAGONAL:
                for (j=0; j<nT; ++j)
                    {
                    k=j-diagA; if (k<0) k=0; h=j+diagA; if (h>=nT) h=nT-1;
                    a=Y[j*(mY+1)];
                    for (i=k; i<=h; ++i) T[i+mT*j]=a*X[i+mX*j];
                    }
                break;
              case MULTIDIAGONAL:
                for (i=0; i<mT; ++i)
                    {
                    k=i-diagA; if (k<0) k=0; h=i+diagA; if (h>=nT) h=nT-1;
                    for (j=0; j<nT; ++j)
                        {
                        k2=j-diagB; if (k2<0) k2=0; h2=j+diagB; if (h2>=nT) h2=nT-1;
                        if (k2<k) k2=k; if (h2>h) h2=h;
                        if (h2>=k2)
                            T[i+mT*j]=sis_tulo(X+i+mX*k2,Y+k2+mY*j,mX,1,h2-k2+1);
                        }
                    }
                break;
              case LOWER_TRIANGULAR:
                for (i=0; i<mT; ++i)
                    {
                    k=i-diagA; if (k<0) k=0; h=i+diagA; if (h>=nT) h=nT-1;
                    for (j=0; j<nT; ++j)
                        {
                        k2=j;
                        if (k2<k) k2=k;
                        if (h>=k2)
                            T[i+mT*j]=sis_tulo(X+i+mX*k2,Y+k2+mY*j,mX,1,h-k2+1);
                        }
                    }
                break;
              case UPPER_TRIANGULAR:
                for (i=0; i<mT; ++i)
                    {
                    k=i-diagA; if (k<0) k=0; h=i+diagA; if (h>=nT) h=nT-1;
                    for (j=0; j<nT; ++j)
                        {
                        h2=j;
                        if (h2<h) h2=h;
                        if (h2>=k)
                            T[i+mT*j]=sis_tulo(X+i+mX*k,Y+k+mY*j,mX,1,h2-k+1);
                        }
                    }
                break;
              case GENERAL:
                for (i=0; i<mT; ++i)
                    {
                    k=i-diagA; if (k<0) k=0; h=i+diagA; if (h>=nT) h=nT-1;
                    for (j=0; j<nT; ++j)
                        {
                        if (h>=k)
                            T[i+mT*j]=sis_tulo(X+i+mX*k,Y+k+mY*j,mX,1,h-k+1);
                        }
                    }
                break;
                }
            break;

          case LOWER_TRIANGULAR:
            switch (typeB)
                {
              case DIAGONAL:
                for (j=0; j<nT; ++j)
                    {
                    a=Y[j*(mY+1)];
                    for (i=j; i<nY; ++i) T[i+mT*j]=a*X[i+mX*j];
                    }
                break;
              case MULTIDIAGONAL:
                for (j=0; j<nT; ++j)
                    {
                    k=j-diagB; if (k<0) k=0; h=j+diagB; if (h>=nT) h=nT-1;
                    for (i=0; i<mT; ++i)
                        {
                        h2=i;
                        if (h2>h) h2=h;
                        if (h2>=k)
                            T[i+mT*j]=sis_tulo(X+i+mX*k,Y+k+mY*j,mX,1,h2-k+1);
                        }
                    }
                break;
              case LOWER_TRIANGULAR:
                for (i=0; i<mT; ++i) for (j=0; j<=i; ++j)
                    {
                    T[i+mT*j]=sis_tulo(X+i+mX*j,Y+j+mY*j,mX,1,i-j+1);
                    }
                break;
              case UPPER_TRIANGULAR:
                for (i=0; i<mT; ++i) for (j=0; j<nT; ++j)
                    {
                    h=i; if (j<i) h=j;
                    T[i+mT*j]=sis_tulo(X+i,Y+mY*j,mX,1,h+1);
                    }
                break;
              case GENERAL:
                for (i=0; i<mT; ++i) for (j=0; j<nT; ++j)
                    {
                    T[i+mT*j]=sis_tulo(X+i,Y+mY*j,mX,1,i+1);
                    }
                break;
                }
            break;

          case UPPER_TRIANGULAR:
            switch (typeB)
                {
              case DIAGONAL:
                for (j=0; j<nT; ++j)
                    {
                    a=Y[j*(mY+1)];
                    for (i=0; i<=j; ++i) T[i+mT*j]=a*X[i+mX*j];
                    }
                break;
              case MULTIDIAGONAL:
                for (j=0; j<nT; ++j)
                    {
                    k=j-diagB; if (k<0) k=0; h=j+diagB; if (h>=nT) h=nT-1;
                    for (i=0; i<mT; ++i)
                        {
                        k2=i;
                        if (k2<k) k2=k;
                        if (h>=k2)
                            T[i+mT*j]=sis_tulo(X+i+mX*k2,Y+k2+mY*j,mX,1,h-k2+1);
                        }
                    }
                break;
              case LOWER_TRIANGULAR:
                for (i=0; i<mT; ++i) for (j=0; j<nT; ++j)
                    {
                    h=i; if (j<i) h=j;

                    T[i+mT*j]=sis_tulo(X+i+mX*h,Y+h+mY*j,mX,1,nX-h);
       /*           a=0.0; for (k=h; k<nX; ++k) a+=X[i+mX*k]*Y[k+mY*j];
                    T[i+mT*j]=a;
       */
                    }
                break;
              case UPPER_TRIANGULAR:
                for (i=0; i<mT; ++i) for (j=i; j<nT; ++j)
                    {
                    T[i+mT*j]=sis_tulo(X+i+mX*i,Y+i+mY*j,mX,1,j-i+1);
                    }
                break;
              case GENERAL:
                for (i=0; i<mT; ++i) for (j=0; j<nT; ++j)
                    {
                    T[i+mT*j]=sis_tulo(X+i+mX*i,Y+i+mY*j,mX,1,mY-i);
                    }
                break;
                }
            break;

          case GENERAL:
            switch (typeB)
                {
              case DIAGONAL:
                for (j=0; j<nT; ++j)
                    {
                    a=Y[j*(mY+1)];
                    for (i=0; i<mX; ++i) T[i+mT*j]=a*X[i+mX*j];
                    }
                break;
              case MULTIDIAGONAL:
                for (j=0; j<nT; ++j)
                    {
                    k=j-diagB; if (k<0) k=0; h=j+diagB; if (h>=nT) h=nT-1;
                    for (i=0; i<mT; ++i)
                        {
                        if (h>=k)
                            T[i+mT*j]=sis_tulo(X+i+mX*k,Y+k+mY*j,mX,1,h-k+1);
                        }
                    }
                break;
              case LOWER_TRIANGULAR:
                for (i=0; i<mT; ++i) for (j=0; j<nT; ++j)
                    {
                    T[i+mT*j]=sis_tulo(X+i+mX*j,Y+j+mY*j,mX,1,nX-j);
                    }
                break;
              case UPPER_TRIANGULAR:
                for (i=0; i<mT; ++i) for (j=0; j<nT; ++j)
                    {
                    T[i+mT*j]=sis_tulo(X+i,Y+mY*j,mX,1,j+1);
                    }
                break;
              case GENERAL:
                for (i=0; i<mT; ++i) for (j=0; j<nT; ++j)
                    {
                    T[i+mT*j]=sis_tulo(X+i,Y+mY*j,mX,1,nX);
                    }
                break;
                }
            break;
            } /* switch (typeA) */
        return(1);
        }


static int find_type(char *s,int *pdiag)
        {
        if (muste_strcmpi(s,"D")==0 || strcmp(s,"0")==0) return(DIAGONAL);
        if (muste_strnicmp(s,"D",1)==0)
            {
            *pdiag=atoi(s+1);
            if ((*pdiag>>1)<<1==*pdiag)
                {
                type_not_known(s); return(-1);
                }
            if (*pdiag==1) return(DIAGONAL);
            return(MULTIDIAGONAL);
            }
        if (muste_strcmpi(s,"LT")==0 || strcmp(s,"2")==0) return(LOWER_TRIANGULAR);
        if (muste_strcmpi(s,"UT")==0 || strcmp(s,"3")==0) return(UPPER_TRIANGULAR);
        if (muste_strcmpi(s,"G")==0 || strcmp(s,"9")==0) return(GENERAL);

        type_not_known(s);
        return(-1);
        }

static void op__mult()
        {
        int i;
        char expr1[2*LLENGTH];
        long time1,time2;
        char message[64];

        i=external_mat_init(1); if (i<0) return;

        if (g<5)
            {
            init_remarks();
            rem_pr("MAT #MULT(C,A,B,typeA,typeB)");
            rem_pr("computes matrix C as a product of A and B (C=A*B)");
            rem_pr("by observing types of operands and");
            rem_pr("thus avoiding multiplication of 0's.");
            rem_pr("Valid types are                                  ");
            rem_pr("D    diagonal,                                   ");
            rem_pr("D#   #-diagonal (e.g. D3=tridiagonal),           ");
            rem_pr("LT   lower triangular,                           ");
            rem_pr("UT   upper triangular,                           ");
            rem_pr("G    general.                                    ");
            rem_pr("                                                 ");
            rem_pr("The actual type of an operand is not checked.    ");
            rem_pr("Therefore it is possible, for example, to multiply");
            rem_pr("by the diagonal of a general square matrix by    ");
            rem_pr("giving its type as D.                            ");
            wait_remarks(2);
            return;
            }
/*
word parameter
2    C
3    A
4    B
5    typeA
6    typeB
*/

        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return; }
        i=load_Y(word[4]); if (i<0) { mat_not_found(word[4]); return; }

        typeA=typeB=GENERAL;
        if (g>5) typeA=find_type(word[5],&diagA);
        if (g>6) typeB=find_type(word[6],&diagB);
        if (typeA<0 || typeB<0) return;
        diagA>>=1; diagB>>=1;

        if ( (typeA<GENERAL && mX!=nX) ||
             (typeB<GENERAL && mY!=nY) ||
             (nX!=mY) ) { dim_error(); return; }

        rlabT=rlabX; mT=mX;
        clabT=clabY; nT=nY;

        i=mat_alloc(&T,mT,nT);
        if (i<0) return;
        for (i=0; i<mT*nT; ++i) T[i]=0.0;

        strcpy(expr,suluin(exprX,1,expr1)); strcat(expr,"*");
        strcat(expr,suluin(exprY,1,expr1));
        nim(expr,exprT);
        time1=clock();  // RS CHECK FIXME clock???
        i=compute();
        time2=clock();  // RS CHECK FIXME clock???
        i=save_T(word[2]);
        sprintf(message," %ld ticks",time2-time1);
        mat_comment(word[2],exprT,i,mT,nT,message);
        external_mat_end(argv1);
        }


/* _samples.c 3.1.1996/SM (4.1.1996)
*/
static int samples1()
        {
        int i,j,h,h2=0;
        extern double sur_rand0();
        double u;
        long lu,lu1;
// RS REM        double x;
        int uusi;
        int i1,count,max_count;
// RS REM        long a;

        unit=(long *)muste_malloc(nX*sizeof(long));
        if (unit==NULL) { ei_tilaa(); return(-1); } // RS CHA exit(1)

        max_count=10000;
        i=spfind_mat("MAXCOUNT");
        if (i>=0) max_count=atoi(spb[i]);

        for (i=0; i<mX; ++i)
            {
          sprintf(sbuf,"%d",i+1); sur_print(sbuf);
          count=0;
          while(1)
            {
            ++count;
            for (j=0; j<nX; ++j)
                {
                while (1)
                    {
                    u=sur_rand0(seed,rand_type);
                    unit[j]=lu=1+nn*u;
                    uusi=0;
                    for (h=0; h<j; ++h)
                        {
                        lu1=unit[h];
                        if (lu==lu1) { uusi=1; break; }
                        if (lu>lu1) continue;
/*           printf("\nj=%d lu=%ld lu1=%ld h=%d\n",j,lu,lu1,h);
             for (h2=0; h2<=j; ++h2) printf("%ld ",unit[h2]); getch();
*/
                        for (h2=j-1; h2>=h; --h2) unit[h2+1]=unit[h2];
                        unit[h]=lu;
/*
             printf("\n");
             for (h2=0; h2<=j; ++h2) printf("%ld ",unit[h2]); getch();
*/
                        break;
                        }
                    if (!uusi) break;
                    }
                }
            for (j=0; j<nX; ++j)
                {
                X[i+mX*j]=unit[j];
                }

            if (!same) break;
            for (i1=0; i1<i; ++i1)
                {
/*
printf("i1=%d\n",i1);
for (h=0; h<nX; ++h) printf("%ld ",unit[h]); printf("\n");
for (h=0; h<nX; ++h) printf("%g ",X[i1+mX*h]); printf("\n");
h=getch(); if (h=='.') exit(0);
*/
                h2=0;
                for (j=0; j<nX; ++j) /* ei ota jÑrj. huomioon! */
                    {
                    for (h=0; h<nX; ++h)
                        {
                        if (unit[j]==(long)X[i1+mX*h])
                            { ++h2; if (h2>same) break; }
                        }
                    if (h2>same) break;
                    }
                if (h2>same) break;
                }
            if (sur_kbhit())
                {
                j=sur_getch(); if (j=='.') return(1);
                }

            if (h2<=same) { sur_print("+ "); break; }
            if (count>max_count) { sur_print("- "); break; }
            } /* while */

            }
        return(1);
        }


static void op__samples()
        {
        int i;
        char tnimi[LLENGTH];

        i=external_mat_init(2); if (i<0) return;
        if (g<7)
            {
            init_remarks();
            rem_pr("MAT #SAMPLES <matrix>,N,n,m,<rand>");
            rem_pr("makes m random samples of size n from integers 1,2,...,N");
            rem_pr("using random number generator <rand>.");
            rem_pr("The samples generated are saved as a n x m matrix file <matrix>.");
            rem_pr("");
            rem_pr("Example:");
            rem_pr("MAT #SAMPLES LOTTO,39,7,12,rand(37952049)");
            rem_pr("MAT LOAD LOTTO,123,CUR+1");
            wait_remarks(2);
            return;
            }

        i=spec_init(r1+r-1); if (i<0) return;

        same=0;
        i=spfind_mat("SAME");
        if (i>=0) same=atoi(spb[i]);

        nn=atol(word[3]);  /* population size */
        nX=atoi(word[4]);  /* sample size */
        mX=atoi(word[5]);  /* replicates */

        if ((long)nX>nn)
            {
            sprintf(sbuf,"\nSample size (%d) must not exceed population size (%ld)",
                          nX,nn);
            sur_print(sbuf); WAIT; return; // RS CHA exit
            }
        if (muste_strnicmp(word[6],"rand(",5)==0) { rand_type=1; i=5; }
        else if (muste_strnicmp(word[6],"urand(",6)==0) { rand_type=2; i=6; }
        else { rand_type=1; i=0; }
        seed=atof(word[6]+i);
        i=mat_alloc_lab(&X,mX,nX,&rlabX,&clabX);
        if (i<0) return; // RS CHA exit

        if (same) for (i=0; i<mX*nX; ++i) X[i]=0.0;

        text_labels2(rlabX,mX,"S",1); // RS CHA 2 ,1
        text_labels2(clabX,nX,"U",1); // RS CHA 2 ,1

        i=samples1();
        if (i<0) return; // RS ADD

        sprintf(tnimi,"Samples_of_size_%d_from_1,...,%ld",nX,nn);

        nim(tnimi,exprX);
        i=matrix_save(word[2],X,mX,nX,rlabX,clabX,8,8,-1,exprX,0,0);
        outseed();
        mat_comment(word[2],exprX,i,mX,nX,NULL);
        external_mat_end(argv1);
        }

/* #indvar.c 7.1.1996/SM (8.1.1996)
*/
static void op__indvar()
        {
        
// RS From local globals to locals        
static SURVO_DATA d;
static int *v;
static long *rec;
static int *col;
static int prind=1;        
        
        int i,k;
        char *p;
        char x[LLENGTH];
        long l;

        i=external_mat_init(1); if (i<0) return;

        if (g<6)
            {
            init_remarks();
                /*  0   1       2    3        4  5                      */
            rem_pr("MAT #INDVAR FROM <matrix> TO <Survo_data_file>");
            rem_pr("marks subsets (samples) of size n of <Survoa_data_file>");
            rem_pr("by generating 0-1 indicator variables, say S1,S2,...,Sm,");
            rem_pr("so that Si indicates by 1's the observations whose #'s");
            rem_pr("appear in ascending order as the i'th row of an");
            rem_pr("m x n matrix file <matrix>.");
            rem_pr("");
            rem_pr("The actual names of the indicator variables are the same as");
            rem_pr("the column labels of <matrix> and they are created as");
            rem_pr("new variables in <Survo_data_file> if needed.");
            rem_pr("");
            rem_pr("MAT #INDVAR is a companion operation to MAT #SAMPLES");
            rem_pr("which generates m samples of size n from integers 1,2,...,N");
            rem_pr("and saves them as a m x n matrix with row labels");
            rem_pr("S1,S2,...,Sm.");
            rem_pr("");
            rem_pr("Active records of <Survo_data_file> cannot be limited");
            rem_pr("by IND etc. specifications.");
            wait_remarks(2);
            return;
            }

        i=spec_find("PRIND",x);
        if (i>=0) prind=atoi(x);

        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return; }

        i=data_open2(word[5],&d,1,1,0);
        if (i<0) return;

        if (d.type!=2)
            {
            sprintf(sbuf,"%s must be a Survo data file!",word[5]);
            sur_print(sbuf); WAIT; return;
            }
        v=(int *)muste_malloc(mX*sizeof(int));
        if (v==NULL) { not_enough_memory(); return; }
        rec=(long *)muste_malloc(mX*sizeof(long));
        if (rec==NULL) { not_enough_memory(); return; }
        col=(int *)muste_malloc(mX*sizeof(int));
        if (col==NULL) { not_enough_memory(); return; }

        x[lrX]=EOS;
        for (i=0; i<mX; ++i)
            {
            strncpy(x,rlabX+i*lrX,lrX);
            p=x+lrX-1; while (*p==' ') *p--=EOS;
            v[i]=varfind2(&d,x,0);
            if (v[i]<0)
                {
                v[i]=create_newvar(&d,x,'1',1);
                if (v[i]<0)
                    {
                    sprintf(sbuf,"\nCannot create variable `%s' in data file `%s'!",
                                            x,word[5]);
                    sur_print(sbuf); WAIT; return;
                    }
                }
            sprintf(sbuf,"indicator from row %d of matrix %s",i+1,word[3]);
            update_varname(&d,v[i],sbuf);
            }


        for (i=0; i<mX; ++i) { col[i]=0; rec[i]=X[i]; }

        sur_print("\n");
        for (l=1L; l<=d.n; ++l)
            {
            if (prind)
                {
                sprintf(sbuf,"%ld ",l); sur_print(sbuf);
                }
            for (i=0; i<mX; ++i)
                {
                if (l==rec[i])
                    {
                    k=1; ++col[i];
                    if (col[i]<nX) rec[i]=X[i+mX*col[i]];
                    else rec[i]=d.n+1L;
                    }
                else k=0;
                data_save(&d,l,v[i],(double)k);
                }
            }

        external_mat_end(argv1);
        }


/* #merge.c 9.3.1996/SM (12.3.1996)
*/

static int block_error(char *s,int m,int b)
        {
        sprintf(sbuf,"\n# of rows (%d) of matrix %s not divisible by size of the block (%d)",
                              m,s,b);
        sur_print(sbuf); WAIT;
        return(1);
        }


static void op__merge()
        {
static int blockX,blockY; // RS From local globals to local        
        
        int i,j,k,h,ix,iy;
// RS REM        char expr1[2*LLENGTH];

        i=external_mat_init(1); if (i<0) return;
        if (g<6)
            {
            init_remarks();
            rem_pr("MAT #MERGE A,B TO C");
            rem_pr("combines two m x n matrices A and B to an 2m x n matrix C");
            rem_pr("by merging rows. Thus the rows of C will be:");
            rem_pr("row 1:    row 1 of A");
            rem_pr("row 2:    row 1 of B");
            rem_pr("row 3:    row 2 of A");
            rem_pr("row 4:    row 2 of B");
            rem_pr("...       ...");
            rem_pr("row 2m-1: row m of A");
            rem_pr("row 2m:   row m of B");

            wait_remarks(1);

            rem_pr("MAT #MERGE A,B TO C BY mA,mB");
            rem_pr("combines an mA*k x n matrix A and an mB*k x n matrix B");
            rem_pr("to an (mA+mB)*k x n matrix C by merging rows of A and B");
            rem_pr("in blocks of rows of mA and mB, respectively.");
            rem_pr("Thus the rows of C will be:");
            rem_pr("row 1:       row 1    of A");
            rem_pr("row 2:       row 2    of A");
            rem_pr("...          ...");
            rem_pr("row mA:      row mA   of A");
            rem_pr("row mA+1:    row 1    of B");
            rem_pr("row mA+2:    row 2    of B");
            rem_pr("...          ...");
            rem_pr("row mA+mB:   row mB   of B");
            rem_pr("row mA+mB+1: row mA+1 of A");
            rem_pr("row mA+mB+2: row mA+2 of A");
            rem_pr("...");

            wait_remarks(2);
            return;
            }

        blockX=blockY=1;
        if (g>8)
            {
            blockX=atoi(word[7]);
            blockY=atoi(word[8]);
            if (blockX<1 || blockY<1)
                {
                sur_print("\nBlock parameters must be at least 1 !");
                WAIT; return;
                }
            }
        i=load_X(word[2]); if (i<0) { mat_not_found(word[3]); return; }
        i=load_Y(word[3]); if (i<0) { mat_not_found(word[4]); return; }

        if (mX%blockX!=0) { block_error(word[2],mX,blockX); return; }
        if (mY%blockY!=0) { block_error(word[3],mY,blockY); return; }

        if (mX/blockX!=mY/blockY || nX!=nY)
            {
            sprintf(sbuf,"\nMatrices %s and %s have not compatible dimensions!",
                             word[2],word[3]);
            sur_print(sbuf); WAIT; return;
            }

        nT=nX;
        mT=mX+mY;
        clabT=clabX;
        i=mat_alloc_lab(&T,mT,nT,&rlabT,NULL);
        if (i<0) return;
        k=0; ix=iy=0;
        for (i=0; i<mX/blockX; ++i)
            {
            for (h=0; h<blockX; ++h)
                {
                for (j=0; j<nX; ++j) T[k+j*mT]=X[ix+j*mX];
                strncpy(rlabT+k*8,rlabX+ix*8,8);
                ++k; ++ix;
                }
            for (h=0; h<blockY; ++h)
                {
                for (j=0; j<nX; ++j) T[k+j*mT]=Y[iy+j*mY];
                strncpy(rlabT+k*8,rlabY+iy*8,8);
                ++k; ++iy;
                }
            }

        sprintf(expr,"MERGE(%s,%s)",exprX,exprY);
        nim(expr,exprT);
        i=save_T(word[5]);
        mat_comment(word[2],exprT,i,mT,nT,"");
        external_mat_end(argv1);
        }

/* #mindiff.c 10.3.1996/SM (10.3.1996)
*/
static void op__mindiff()
        {
        int i,j,k;
        double s1,s2,s,a,b;
// RS REM        char expr1[2*LLENGTH];

        i=external_mat_init(1); if (i<0) return;
        if (g<5)
            {
            init_remarks();
            rem_pr("MAT #MINDIFF(C,A,B)");
            rem_pr("computes minimal difference C of m x n matrices of");
            rem_pr("A=[A1,A2,...,An] and");
            rem_pr("B=[B1,B2,...,Bn]");
            rem_pr("by selecting constants s1,s2,..,sn (each either 1 or -1)");
            rem_pr("so that the sum of squares of the elements of");
            rem_pr("C=[A1-s1*B1,A2-s1*B2,...,An-sn*Bn]");
            rem_pr("is as small as possible.");
            wait_remarks(2);
            return;
            }
        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return; }
        i=load_Y(word[4]); if (i<0) { mat_not_found(word[4]); return; }

        if (mX!=mY || nX!=nY)
            {
            sprintf(sbuf,"Matrices %s and %s have not same dimensions!",
                             word[2],word[3]);
            sur_print(sbuf); WAIT; return;
            }

        rlabT=rlabX; clabT=clabX; mT=mX; nT=nX;
        i=mat_alloc(&T,mT,nT);
        if (i<0) return;

        s=0.0;
        for (j=0; j<nX; ++j)
            {
            s1=s2=0.0;
            for (i=0; i<mX; ++i)
                {
                a=X[i+j*mX]; b=Y[i+j*mY];
                s1+=(a-b)*(a-b);
                s2+=(a+b)*(a+b);
                }
            if (s1<=s2) { k=1; s+=s1; } else { k=-1; s+=s2; }
            for (i=0; i<mX; ++i)
                {
                T[i+j*mX]=X[i+j*mX]-k*Y[i+j*mY];
                }
            }

        sprintf(expr,"MINDIFF(%s,%s)",exprX,exprY);
        nim(expr,exprT);
        i=save_T(word[2]);
        sprintf(sbuf," sumsqr%s=%g",word[2],s);
        mat_comment(word[2],exprT,i,mT,nT,sbuf);
        external_mat_end(argv1);
        }

/* #colsort.c 11.4.1996/SM (12.4.1996)
*/
static int sort1(int method)
        {
static double *sum2; // RS CHA From local globals to local        
        
        int i,j,k,j1=0;
        double a,b;

        sum2=(double *)muste_malloc(nT*sizeof(double));
        if (sum2==NULL) { ei_tilaa(); return(-1); }

        for (j=0; j<nT; ++j)
            {
            a=0.0;
            for (i=0; i<mT; ++i)
                {
                if (method==1) a+=X[i+mX*j]*X[i+mX*j];
                else   /* method=2 */
                    {
                    b=fabs(X[i+mX*j]);
                    if (b>a) a=b;
                    }
                }
            sum2[j]=a;
            }

        for (k=0; k<nT; ++k)
            {
            a=-1.0;
            for (j=0; j<nT; ++j)
                {
                if (sum2[j]>a) { a=sum2[j]; j1=j; }
                }
            sum2[j1]=-1.0;
            for (i=0; i<mT; ++i) T[i+mT*k]=X[i+mX*j1];
            for (i=0; i<8; ++i) clabT[i+8*k]=clabX[i+8*j1];

            }
        if (sum2!=NULL) { muste_free(sum2); sum2=NULL; } // RS ADD    
        return(1);
        }

static void op__colsort()
        {
        int i;
// RS REM        char expr1[2*LLENGTH];
        int method;

        i=external_mat_init(1); if (i<0) return;

        method=1;
        if (g>6) { if (muste_strcmpi(word[6],"MAX")==0) method=2; else method=-1; }
        if (g<5 || method==-1)
            {
            init_remarks();
            rem_pr("MAT #COLSORT A TO B");
            rem_pr("sorts the columns of A into descending order of");
            rem_pr("their sums of squares and puts the result to B.");
            rem_pr("MAT #COLSORT A TO B BY MAX");
            rem_pr("sorts the columns of A into descending order of");
            rem_pr("the maximum absolute values of elements and puts the result to B.");
            wait_remarks(2);
            return;
            }

        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return; }

        mT=mX; nT=nX;

        i=mat_alloc_lab(&T,mT,nT,&rlabT,&clabT);
        if (i<0) return;
        rlabT=rlabX;
        sort1(method);

        sprintf(expr,"COLSORT(%s)",exprX);
        nim(expr,exprT);
        i=save_T(word[4]);
        mat_comment(word[4],exprT,i,mT,nT,NULL);
        external_mat_end(argv1);
        }


/* _crsort.c 11.4.1996/SM (12.4.1996)
*/
static int c_sort(int method)
        {
static double *sum2; // RS CHA From local globals to local        
        int i,j,k,j1=0;
        double a,b;

        sum2=(double *)muste_malloc(nT*sizeof(double));
        if (sum2==NULL) { ei_tilaa(); return(-1); }

        for (j=0; j<nT; ++j)
            {
            a=0.0;
            for (i=0; i<mT; ++i)
                {
                if (method==1) a+=X[i+mX*j]*X[i+mX*j];
                else   /* method=2 */
                    {
                    b=fabs(X[i+mX*j]);
                    if (b>a) a=b;
                    }
                }
            sum2[j]=a;
            }

        for (k=0; k<nT; ++k)
            {
            a=-1.0;
            for (j=0; j<nT; ++j)
                {
                if (sum2[j]>a) { a=sum2[j]; j1=j; }
                }
            sum2[j1]=-1.0;
            for (i=0; i<mT; ++i) T[i+mT*k]=X[i+mX*j1];
            for (i=0; i<8; ++i) clabT[i+8*k]=clabX[i+8*j1];

            }
        if (sum2!=NULL) { muste_free(sum2); sum2=NULL; } // RS ADD   
        return(1);
        }


static int crsort()
        {

        c_sort(1);


        return(1);
        }

static void op__crsort()
        {
        int i;
// RS REM        char expr1[2*LLENGTH];

        i=external_mat_init(1); if (i<0) return;

        if (g<5)
            {
            init_remarks();
            rem_pr("MAT #CRSORT A TO B");
            rem_pr("sorts the columns of A into descending order of");
            rem_pr("their sums of squares and after that");
            rem_pr("the rows in descending order by the absolute values");
            rem_pr("of elements.");
            wait_remarks(2);
            return;
            }

        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return; }

        mT=mX; nT=nX;

        i=mat_alloc_lab(&T,mT,nT,&rlabT,&clabT);
        if (i<0) return;
        rlabT=rlabX;
        crsort();

        sprintf(expr,"CRSORT(%s)",exprX);
        nim(expr,exprT);
        i=save_T(word[4]);
        mat_comment(word[4],exprT,i,mT,nT,NULL);
        external_mat_end(argv1);
        }

/* #eigen.c 11.6.1998/SM (15.6.1998)
*/

/* MAT #EIGEN #VALUES D,D2,eps */
static void eigval_columns()
        {
        int i,j,n;
        double eps=1e-10;
        double max,b;

        if (g<5)
            {
            init_remarks();
            rem_pr("MAT #EIGEN #VALUES D,D2,eps");
            rem_pr("stores eigenvalues from n*n matrix D");
            rem_pr("(D obtained by MAT #EIGEN(A,D,...))");
            rem_pr("in n*2 matrix D2 where");
            rem_pr("real parts will be in column 1 (Re) and");
            rem_pr("imaginary parts in column 2 (Im).");
            rem_pr("eps (default 1e-10) is a treshold value for imaginary parts.");

            wait_remarks(2);
            return;
            }

        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return; }
        if (mX!=nX)
            {
            sprintf(sbuf,"%s is not a square matrix!",word[3]);
            sur_print(sbuf); WAIT; return;
            }
        n=mX;
        i=mat_alloc(&T,n,2);
        if (i<0) return;
        if (g>5) eps=atof(word[5]);

        /* oikean muodon testaus puuttuu! */
        for (i=0; i<n; ++i)
            {
            if (i<n-1 && fabs(X[i+n*(i+1)])>eps)
                {
                T[i]=X[(n+1)*i]; T[i+1]=X[(n+1)*(i+1)];
                T[i+n]=fabs(X[i+n*(i+1)]); T[i+n+1]=-fabs(X[i+1+n*i]);
                ++i;
                }
            else { T[i]=X[(n+1)*i]; T[i+n]=0.0; }
            }

        max=0.0;
        for (i=0; i<n; ++i) for (j=0; j<n; ++j)
            {
            if (i==j || i==j-1 || i==j+1) continue;
            b=fabs(X[i+n*j]);
            if (b>max) max=b;
            }

        sprintf(expr,"%s~Eigenvalues(%s)_max_off_diag=%g",
                      word[4],word[3],max);
        nim(expr,exprT);

        strcpy(sbuf,"Re      Im      ");
        i=matrix_save(word[4],T,mX,2,rlabX,sbuf,8,8,-1,exprT,0,0);

        external_mat_end(argv1);
        }



static void op__eigen()
        {

// RS CHA From local globals to locals
static double *U;
static double *pT,*pU;
static char *textlab;        
        
        
        int i;
// RS REM        char expr1[2*LLENGTH];
// RS REM        char message[64];
        int iter;
        double eps;
        int n_small; /* iterations */
// RS REM        double ratio4;

        i=external_mat_init(1); if (i<0) return;

        if (g<3)
            {
            init_remarks();

rem_pr("MAT #EIGEN(A,D)");
rem_pr("computes eigenvalues D");
rem_pr("of an n*n nonsymmetric matrix A.");
rem_pr("MAT #EIGEN(A,D,R)");
rem_pr("computes eigenvalues D and (right) eigenvectors R");
rem_pr("MAT #EIGEN(A,D,R,L)");
rem_pr("computes also left eigenvectors L.");
rem_pr("");
rem_pr("D will be an n*n tridiagonal matrix where real eigenvalues");
rem_pr("occupy diagonal elements while the real and imaginary parts of");
rem_pr("pairs of complex eigenvalues u+iv, u-iv occupy respectively");
rem_pr("the diagonal and off-diagonal corners of 2*2 blocks.");
rem_pr("Afterwards an n*2 matrix D2 of real and imaginary parts of eigenvalues");
rem_pr("can be created by");
rem_pr("MAT #EIGEN #VAL D,D2 .");
rem_pr("");
rem_pr("Matrices A,D,R,L satisfy, for example, the equations");
rem_pr("A=RDL, AR=RD, LA=DL.");

            wait_remarks(1);

rem_pr("The algorithm of MAT #EIGEN is based on a norm reducing Jacobi type method");
rem_pr("presented by P.J.Eberlein and J.Boothroyd in Handbook for Automatic");
rem_pr("Computation, Vol.2 (Wilkinson and Reinsch, Springer 1971, pp.327-338).");
rem_pr("The original algorithm has been speeded up by a factor ca. 7 by SM (1998).");

            wait_remarks(2);
            return;
            }

        if (muste_strnicmp(word[2],"#VAL",4)==0) { eigval_columns(); return; }

        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return; }
        if (mX!=nX)
            {
            sprintf(sbuf,"%s is not a square matrix!",word[2]);
            sur_print(sbuf); WAIT; return;
            }

        pT=pU=NULL;
        if (g>4 && *word[4]!='-')
            {
            i=mat_alloc(&T,mX,mX);
            if (i<0) return;
            pT=T;
            }
        if (g>5 && *word[5]!='-')
            {
            i=mat_alloc(&U,mX,mX);
            if (i<0) return;
            pU=U;
            }

        iter=100000;
        if (g>6) iter=atoi(word[6]);
        eps=1e-15;
        if (g>7) eps=atof(word[7]);
        if (mX<51) n_small=50; else n_small=mX/2-1;
        if (g>8) n_small=atoi(word[8]);

        sprintf(expr,"%s~Eigenvalues(%s)",
                      word[3],word[2]);
        nim(expr,exprT);

        i=mat_nonsymm_eigen(X,pT,pU,mX,iter,eps,n_small);

        sprintf(sbuf,"%d iterations",i);

 /*     for (i=0; i<mX; ++i) X[i]=X[i*(mX+1)];  */

        textlab=muste_malloc(mX*8);
        if (textlab==NULL) { not_enough_memory(); return; }

        text_labels2(textlab,mX,"eigen",1); // RS CHA 2, 1

        sprintf(expr,"%s~Eigenvalues(%s)",
                      word[3],word[2]);
        nim(expr,exprT);
        i=matrix_save(word[3],X,mX,mX,textlab,textlab,8,8,-1,exprT,0,0);
        if (pT!=NULL)
            {
            sprintf(expr,"%s~Eigenvectors(%s)",
                      word[4],word[2]);
            nim(expr,exprT);
            i=matrix_save(word[4],T,mX,mX,rlabX,textlab,8,8,-1,exprT,0,0);
            }
        if (pU!=NULL)
            {
            sprintf(expr,"%s~Left_eigenvectors(%s)",
                      word[5],word[2]);
            nim(expr,exprT);
            i=matrix_save(word[5],U,mX,mX,textlab,clabX,8,8,-1,exprT,0,0);
            }
        mat_comment(word[3],sbuf,i,mX,1,NULL);

// RS ADD free
        if (U!=NULL) { muste_free(U); U=NULL; }
        if (pT!=NULL) { muste_free(pT); pT=NULL; }   
        if (pU!=NULL) { muste_free(pU); pU=NULL; }  
        if (textlab!=NULL) { muste_free(textlab); textlab=NULL; }          

        external_mat_end(argv1);
        }

/* _convol.c 4.7.1998/SM (4.7.1998) (22.2.2003)
*/
static int is_matrix(char *s) // 21.2.2003
    {
    char x[LNAME];

    matrix_name(x,s);
    if (sur_file_exists(x)<0) return(0);
    return(1);
    }

static int last_non_zero(double *aa,int n)
        {
        int i;

        for (i=n-1; i>=0; --i)
            if (aa[i]!=0.0) break;
        return(i+1);
        }

static void op__convol()
        {
static double *TT,*TT2; // RS CHA From local globals to local        
        
        int i,j,k,h,kk,i1,i2;
// RS REM        char expr1[2*LLENGTH];
        double a;
        int n=0;
        char bin[100];
// RS REM        char ch;

        i=external_mat_init(1); if (i<0) return;
        if (g<4)
            {
            init_remarks();
            rem_pr("MAT #CONVOLUTION(C,A,k)");
            rem_pr("computes k first coefficients of the convolution");
            rem_pr("of the columns of matrix A and saves them as");
            rem_pr("a column vector C.");
            rem_pr("It is assumed that elements a0,a1,a2,... of each A column are");
            rem_pr("coefficients of a polynomial a0+a1*x+a2*x^2+...");
            rem_pr("Default for k is k=(m-1)*n+1 where m,n are dimensions of A.");
            rem_pr("");
            rem_pr("Alternatively:");
            rem_pr("MAT #CONVOLUTION(C,A,k,N)");
            rem_pr("when A has only one column computes the N-fold convolution of this column.");

            wait_remarks(1);
            rem_pr("..................................................................");
            rem_pr("Example: Probabilities of binomial distribution");
            rem_pr("         as convolution of n Bernoulli distributions");
            rem_pr("p=1/3 n=13");
            rem_pr("");
            rem_pr("MATRIX P ///");
            rem_pr("1-p");
            rem_pr("p");
            rem_pr("");
            rem_pr("MAT SAVE P  / Save probabilities of Bernoulli distribution");
            rem_pr("MAT #CONVOLUTION(C,P,n+1,n)");
            rem_pr("/MATSHOW C  / See binomial probabilities");
            rem_pr("..................................................................");

            wait_remarks(2);
            return;
            }

//      printf("g=%d\n",g);
//      for (i=0; i<g; ++i) printf("%s\n",word[i]); getch();

        i=spec_init(r1+r-1); if (i<0) return;
        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return; }

        if (g<5) mT=(mX-1)*nX+1;
        else  // 21.2.2003  MAT #CONVOLUTION(C,A,B) eli  C=convol(A,B)
            {
            if (is_matrix(word[4]))
                {
                i=load_Y(word[4]);
                if (i<0) { mat_not_found(word[4]); return; }
                mT=mX+mY-1;
                nT=1;
                i=mat_alloc_lab(&T,mT,nT,&rlabT,&clabT);
                if (i<0) return;
                i=mat_alloc(&TT,mT,nT);
                if (i<0) return;
                for (i=0; i<mT; ++i) T[i]=0.0;
                for (i=0; i<mX; ++i)
                    for (j=0; j<mY; ++j)
                        T[i+j]+=X[i]*Y[j];

                strncpy(clabT,"Convol  ",8);
                text_labels2(rlabT,mT,"C",0);
                sprintf(expr,"CONVOLUTION(%s,%s)",word[3],word[4]);
                nim(expr,exprT);
                i=save_T(word[2]);
                mat_comment(word[2],exprT,i,mT,nT,"");
				
			    if (TT!=NULL) { muste_free(TT); TT=NULL; } // RS ADD

                external_mat_end(argv1);
                return;
                }

            laske(word[4],&a); mT=a; if (mT<mX) mT=mX;
            }
        nT=1;
        i=mat_alloc_lab(&T,mT,nT,&rlabT,&clabT);
        if (i<0) return;
        i=mat_alloc(&TT,mT,1);
        if (i<0) return;

        if (nX==1)
            {
            if (g<6)
                {
                sur_print("Usage: MAT #CONVOLUTION(C,A,k,n)");
                WAIT; return; // RS CHA exit(0)
                }
            laske(word[5],&a); n=a;
            }
        for (i=0; i<mT; ++i) T[i]=TT[i]=0.0;
        for (i=0; i<mX; ++i) TT[i]=T[i]=X[i];
        if (nX>1)
            for (j=1; j<nX; ++j)
                {
                for (i=0; i<mT; ++i) { TT[i]=T[i]; T[i]=0.0; }
                i1=last_non_zero(TT,mT);
                for (k=0; k<mX; ++k)
                    {
                    a=X[k+mX*j]; if (a==0.0) continue;
                    for (h=0; h<i1; ++h)
                        {
                        if (k+h>mT-1) break;
                        T[k+h]+=a*TT[h];
                        }
                    }
                }
        else
            {
/* uusi versio */
            i=mat_alloc(&TT2,mT,1);
            if (i<0) return;
            muste_itoa(n,bin,2);
            kk=strlen(bin);

            for (i=0; i<mT; ++i) TT2[i]=T[i]=0.0;
            for (i=0; i<mX; ++i) TT2[i]=X[i];
            T[0]=1.0;
            for (j=0; j<kk; ++j)
                {
/* printf("%c\n",bin[kk-1-j]);   */
                if (bin[kk-1-j]=='1')
                    {
                    for (i=0; i<mT; ++i) { TT[i]=T[i]; T[i]=0.0; }
                    i1=last_non_zero(TT,mT);
                    i2=last_non_zero(TT2,mT);
                    for (k=0; k<i2; ++k)
                        {
                        a=TT2[k]; if (a==0.0) continue;
                        for (h=0; h<i1; ++h)
                            {
                            if (k+h>mT-1) break;
                            T[k+h]+=a*TT[h];
                            }
                        }
                    }
                if (j==kk) break;
                for (i=0; i<mT; ++i) { TT[i]=TT2[i]; TT2[i]=0.0; }
                i1=last_non_zero(TT,mT);
                for (k=0; k<i1; ++k)
                    {
                    a=TT[k]; if (a==0.0) continue;
                    for (h=0; h<mT; ++h)
                        {
                        if (k+h>mT-1) break;
                        TT2[k+h]+=a*TT[h];
                        }
                    }

                }
/**************************************** vanha versio
            for (j=1; j<n; ++j)
                {
                for (i=0; i<mT; ++i) { TT[i]=T[i]; T[i]=0.0; }
                for (k=0; k<mX; ++k)
                    {
                    a=X[k]; if (a==0.0) continue;
                    for (h=0; h<mT; ++h)
                        {
                        if (k+h>mT-1) break;
                        T[k+h]+=a*TT[h];
                        }
                    }
                }
*********************************************/
            }
        strncpy(clabT,"Convol  ",8);
        text_labels2(rlabT,mT,"C",0);
        sprintf(expr,"CONVOLUTION(%s)",exprX);
        nim(expr,exprT);
        i=save_T(word[2]);
        mat_comment(word[2],exprT,i,mT,nT,"");

		if (TT!=NULL) { muste_free(TT); TT=NULL; } // RS ADD
		if (TT2!=NULL) { muste_free(TT2); TT2=NULL; } // RS ADD


        external_mat_end(argv1);
        }


/* _maxdet.c 11.6.1998/SM (15.6.1998)
*/

/* MAT #MAXDET(A,dim,S) */
/*   0       1 2 3   4  */
/* or
   MAT #MAXDET(A,dim,S,1)
   for all subsets
*/

static int rand_comb(int m,int k,int *sel)
        {
        int i,h,f;
        double u;
// RS REM        extern double sur_rand0();

        for (i=0; i<m; ++i) sel[i]=i;
        for (i=0; i<k; ++i)
            {
            u=sur_rand0(seed,rand_type);
            h=i+(m-i)*u;
            f=sel[h]; sel[h]=sel[i]; sel[i]=f;
            }
// for (i=0; i<k; ++i) printf("%d ",sel[i]); printf("\n"); getch();
        return(1);
        }

static int next_m_comb(int n,int m,int *p)
        {
        int i,k;

        if (p[m-1]<n-1) { ++p[m-1]; return(1); }
        i=m-2;
        while (p[i]==n-m+i && i>=0 ) --i;
        if (i<0) return(-1);
        ++p[i]; k=p[i]; ++i;
        for ( ; i<m; ++i) p[i]=++k;
        return(1);
        }

static void max_cos(double r,double *pf,double *pdf)
        {
        double a;

        a=pow(1.0-r,(double)(dim-3));
        *pf=(1.0+(dim-1)*r)*a*(1.0-r)*(1.0-r)-det;
        *pdf=(dim-1)*(2.0+(dim-2)*r)*a;
        }

static int mat_det_extra(double *X,int m,double *pdet)
        {
        int i,j,k,h;
        double a,b;

        h=1;
        *pdet=1.0;
        for (k=0; k<m; ++k)
            {
            a=X[k*(m+1)];
            *pdet*=a;
            if (a==0.0) return(0);
            if (k==m-1) return(1);
            a=1/a;
            X[k*(m+1)]=a;
            for (j=0; j<m; ++j)
                {
                if (j==k) continue;
                X[k+m*j]*=a;
                }
            for (i=0; i<m; ++i)
                {
                if (i==k) continue;
                b=X[i+m*k];
                for (j=0; j<m; ++j)
                    {
                    if (j==k) continue;
                    X[i+m*j]-=b*X[k+m*j];
                    }
                X[i+m*k]*=-a;
                }
            }
        return(1);
        }

static int improve(
double *cos_m,
int m,
int k,
int *cos_i,
double *pdet,
double *cos_t
)
        {
        int i,j,h,ih,i1;
        int muutos,muutos2;
        double det;

        muutos=1;
        while (muutos)
            {
            muutos=0;
            for (h=0; h<k; ++h)
                {
                ih=cos_i[h]; muutos2=0;
                for (i=0; i<m; ++i)
                    {
                    for (j=0; j<k; ++j) if (cos_i[j]==i) break;
                    if (j<k) continue;
                    cos_i[h]=i;
                    for (i1=0; i1<k; ++i1) for(j=0; j<k; ++j)
                        cos_t[i1+k*j]=cos_m[cos_i[i1]+m*cos_i[j]];
// printf("A\n"); getch();
                    mat_det_extra(cos_t,k,&det);

                    if (det>*pdet)
                        {
                        *pdet=det;
                        ++muutos; muutos2=1;
                        continue;
                        }
                    }
                if (!muutos2) cos_i[h]=ih;
                }
            }
        return(1);
        }


static int maxdet_rand(
double *cos_m,
int m,
int k,
double *s,
double *pdet,
int type
)
        {
        int i,j;
        double detmax,det;
        long ii,nn;
        char x[LLENGTH];

        cos_t=(double *)muste_malloc(k*k*sizeof(double));
        if (cos_t==NULL) { not_enough_memory(); return(-1); }
        sel=(int *)muste_malloc(m*sizeof(int));
        if (sel==NULL) { not_enough_memory(); return(-1); }
        sel_max=(int *)muste_malloc(k*sizeof(int));
        if (sel_max==NULL) { not_enough_memory(); return(-1); }

        i=spfind_mat("RND");
        if (i==0) { seed=123456789; rand_type=1; }
        else
            {
            strcpy(x,spb[i]);
            if (muste_strnicmp(x,"rand(",5)==0) { rand_type=1; i=5; }
            else if (muste_strnicmp(x,"urand(",6)==0) { rand_type=2; i=6; }
            else { rand_type=1; i=0; }
            seed=atof(x+i);
            }

        nn=10000;
        i=spfind_mat("N");
        if (i>=0) nn=atol(spb[i]);

        detmax=-1.0;
        for (ii=0L; ii<nn; ++ii)
            {
            rand_comb(m,k,sel);

            for (i=0; i<k; ++i)
                for (j=0; j<k; ++j)
                    cos_t[i+k*j]=cos_m[sel[i]+m*sel[j]];
            mat_det_extra(cos_t,k,&det);
            if (fabs(det)>detmax)
                {
                detmax=fabs(det);
                for (i=0; i<k; ++i) sel_max[i]=sel[i];
                }
            if (type==2)
                {
                improve(cos_m,m,k,sel,&detmax,cos_t);
                }
            sprintf(sbuf,"\n%ld %g",ii+1,detmax);
            sur_print(sbuf);
            }

        *pdet=detmax;

        for (i=0; i<m; ++i) s[i]=0;
        for (i=0; i<k; ++i) s[sel_max[i]]=1;

        return(1);
        }

static int maxdet_all(
double *cos_m,
int m,
int k,
double *s,
double *pdet
)
        {
        int i,j;
        double detmax,det;

        cos_t=(double *)muste_malloc(k*k*sizeof(double));
        if (cos_t==NULL) { not_enough_memory(); return(-1); }
        sel=(int *)muste_malloc(k*sizeof(int));
        if (sel==NULL) { not_enough_memory(); return(-1); }
        sel_max=(int *)muste_malloc(k*sizeof(int));
        if (sel_max==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<k; ++i) sel[i]=i;
        detmax=-1.0;
        while (1)
            {
            for (i=0; i<k; ++i)
                for (j=0; j<k; ++j)
                    cos_t[i+k*j]=cos_m[sel[i]+m*sel[j]];
            mat_det_extra(cos_t,k,&det);
            if (fabs(det)>detmax)
                {
                detmax=fabs(det);
                for (i=0; i<k; ++i) sel_max[i]=sel[i];
                }
            i=next_m_comb(m,k,sel);
            if (i<0) break;
            }
        *pdet=detmax;

        for (i=0; i<m; ++i) s[i]=0;
        for (i=0; i<k; ++i) s[sel_max[i]]=1;

        return(1);
        }


static int maxdet(
double *cos_m,
int m,
int k,
double *s,
double *pdet,
int type,
int cancel_same
)
        {
        int i,j,h;
        double da; // ,db;
        double detmax,detm=0,det1; // ,det;
        double chh;
        int jmax=0,ii,jj;

        cos_t=(double *)muste_malloc(k*k*sizeof(double));
        if (cos_t==NULL) { not_enough_memory(); return(-1); }
        cos_u=(double *)muste_malloc(k*sizeof(double));
        if (cos_u==NULL) { not_enough_memory(); return(-1); }
        cos_u2=(double *)muste_malloc(k*sizeof(double));
        if (cos_u2==NULL) { not_enough_memory(); return(-1); }
        cos_v=(double *)muste_malloc(k*sizeof(double));
        if (cos_v==NULL) { not_enough_memory(); return(-1); }
        cos_i=(int *)muste_malloc(k*sizeof(int));
        if (cos_i==NULL) { not_enough_memory(); return(-1); }
        cos_imax=(int *)muste_malloc(k*sizeof(int));
        if (cos_imax==NULL) { not_enough_memory(); return(-1); }
        sel2=(int *)muste_malloc(m*sizeof(int));
        if (sel2==NULL) { not_enough_memory(); return(-1); }

        detmax=0.0;
        sur_print("\n");
        for (i=0; i<m; ++i)
            {
            sprintf(sbuf,"%3d ",i+1); sur_print(sbuf);
            if (sh) fprintf(sh_file,"%s",sbuf);
            cos_i[0]=i;
            det1=cos_m[i*(m+1)];
            if (det1==0.0) continue; /* 2.10.1996 */
            cos_t[0]=1/det1;

            for (h=2; h<=k; ++h)
                {
                detm=0.0;
                for (j=0; j<m; ++j)
                    {
                    for (ii=0; ii<h-1; ++ii) if (j==cos_i[ii]) break;
                    if (ii<h-1) continue;
                    for (ii=0; ii<h-1; ++ii) cos_u[ii]=cos_m[j+m*cos_i[ii]];
                    chh=cos_m[j*(m+1)];
                    da=chh;
                    for (ii=0; ii<h-1; ++ii)
                        {
                        da-=cos_t[ii*(k+1)]*cos_u[ii]*cos_u[ii];
                        for (jj=0; jj<ii; ++jj) da-=2*cos_t[ii+k*jj]*cos_u[ii]*cos_u[jj];
                        }
                    if (da>detm) { detm=da; jmax=j; }
                    }
                if (detm==0.0) break;  /* 2.10.1996 */
                sprintf(sbuf,"%3d ",jmax+1); sur_print(sbuf);
                if (sh) fprintf(sh_file,"%s",sbuf);
                if (h==2) sel2[i]=jmax;
                if (h==2 && cancel_same)
                    {
                    if (jmax<i && sel2[jmax]==i) break;
                    }
                cos_i[h-1]=jmax;
                det1*=detm;
                for (ii=0; ii<h-1; ++ii) cos_u[ii]=cos_m[jmax+m*cos_i[ii]];
                for (j=0; j<h-1; ++j)
                    {
                    da=0.0;
                    for (ii=0; ii<h-1; ++ii) da+=cos_t[j+k*ii]*cos_u[ii];
                    cos_u2[j]=da;
                    }
                for (ii=0; ii<h-1; ++ii) for (jj=0; jj<=ii; ++jj)
                    {
                    da=cos_t[ii+k*jj]+cos_u2[ii]*cos_u2[jj]/detm;
                    cos_t[ii+k*jj]=da; cos_t[jj+k*ii]=da;
                    }
                for (j=0; j<h-1; ++j)
                    {
                    da=-cos_u2[j]/detm; cos_t[h-1+k*j]=da; cos_t[j+k*(h-1)]=da;
                    }
                cos_t[(h-1)*(k+1)]=1/detm;
                }
            if (detm==0.0 || h<k+1)
                {
                sur_print("...\n");
                if (sh) fprintf(sh_file,"...\n");
                continue;
                }

            if (type==2)
                {
                improve(cos_m,m,k,cos_i,&det1,cos_t);
                }

            sprintf(sbuf," %g\n",det1); sur_print(sbuf);
            if (sh) fprintf(sh_file,"%s",sbuf); // RS ADD ,"%s"

            if (det1>detmax)
                {
                detmax=det1;
                for (j=0; j<k; ++j) cos_imax[j]=cos_i[j];
                }
            } /* i */

        if (detmax==0.0)
            {
            sur_print("\nHeavily singular matrix!");
            WAIT; return(-1); // RS CHA exit(1);
            }
        *pdet=detmax;

        for (i=0; i<m; ++i) s[i]=0;
        for (i=0; i<k; ++i) s[cos_imax[i]]=1;

        return(1);
        }

static double rtsafe(
void (*funcd)(),
double x1,
double x2,
double xacc
)
        {
        int j;
        double df,dx,dxold,f,fh,fl;
        double temp,xh,xl,rts;

        (*funcd)(x1,&fl,&df);
        (*funcd)(x2,&fh,&df);
        if ((fl>0.0 && fh>0.0) || (fl<0.0 && fh<0.0))
            {
            sprintf(sbuf,"\nRoot not bracketed by %g,%g!",x1,x2);
            sur_print(sbuf); WAIT; return(0.0); // RS FIXME laskuvirhe! exit(1);
            }
        if (fl==0.0) return(x1);
        if (fh==0.0) return(x2);
        if (fl<0.0) { xl=x1; xh=x2; }
        else        { xh=x1; xl=x2; }
        rts=0.5*(x1+x2);
        dxold=fabs(x2-x1);
        dx=dxold;
        (*funcd)(rts,&f,&df);
        for (j=0; j<MAXIT; ++j)
            {
            if ((((rts-xh)*df-f)*((rts-xl)*df-f)>=0.0)
                || (fabs(2*f)>fabs(dxold*df)))
                {
                dxold=dx; dx=0.5*(xh-xl); rts=xl+dx;
                if (xl==rts) return(rts);
                }
            else
                {
                dxold=dx; dx=f/df; temp=rts; rts-=dx;
                if (temp==rts) return(rts);
                }

            if (fabs(dx)<xacc) return(rts);
            (*funcd)(rts,&f,&df);
            if (f<0.0) xl=rts; else xh=rts;
            } /* j */
        sprintf(sbuf,"\nMax # of iterations (%d) exceeded in rtsafe!",
                                            MAXIT);
// RS REM        exit(1);
        return(0.0); // RS FIXME laskuvirhe!
        }


static void op__maxdet()
        {
        int i,j;
// RS REM        char message[64];
        double mcos;
        int fast;

        i=spec_init(r1+r-1); if (i<0) return;
        i=external_mat_init(1); if (i<0) return;

        i=spfind_mat("SHOW");
        if (i>=0) sh=1; else sh=0;

        if (sh)
            sh_file=muste_fopen(spb[i],"wt");

        if (g<5)
            {
            init_remarks();

rem_pr("MAT #MAXDET(C,dim,S)");
rem_pr("finds the principal dim*dim submatrix with the maximal");
rem_pr("determinant from a symmetric matrix C.");
rem_pr("Indices of rows (and columns) belonging to that");
rem_pr("submatrix are saved as a column vector S.");
rem_pr(" ");
rem_pr("The algorithms for this task are explained in");
rem_pr("S. Mustonen: Matrix computations in Survo");
rem_pr("(www.helsinki.fi/survo/matrix99.html).");
rem_pr("The extended forms of MAT #MAXDET are");
rem_pr("MAT #MAXDET(C,dim,S,0) / Exhaustive search");
rem_pr("MAT #MAXDET(C,dim,S,1) / Stepwise procedure (default)");
rem_pr("MAT #MAXDET(C,dim,S,2) / Improved stepwise procedure");
rem_pr("MAT #MAXDET(C,dim,S,3) / N=#_of_replicates, Random search");
rem_pr("MAT #MAXDET(C,dim,S,4) / N=# Improved random search");
            wait_remarks(1);
rem_pr("Applications: See also www.helsinki.fi/survo/matrix99.html");
rem_pr("MAT #MAXDET can be applied to determination of a basis of");
rem_pr("the column space of a m*n matrix A as follows.");
rem_pr("If the rank of A (determined by the SVD of A) is r,");
rem_pr("the most orthogonal subset of columns of A as an m*r matrix B");
rem_pr("is found by the commands");
rem_pr("MAT C=MTM(NRM(A))");
rem_pr("MAT #MAXDET(C,r,S)  / or MAT #MAXDET(C,r,S,2), for example");
rem_pr("MAT B=SUB(A,*,S)");
rem_pr("");
rem_pr("If A is a factor matrix, the commands");
rem_pr("MAT C=MTM(NRM(A'))");
rem_pr("MAT #MAXDET(C,n,S) / Find row space of A");
rem_pr("MAT B=SUB(A,S,*)");
rem_pr("correspond to the cosine rotation of factor analysis");
rem_pr("usually performed by");
rem_pr("ROTATE A,n / METHOD=COS,0");
            wait_remarks(2);
            return;
            }

        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return; }
        if (mX!=nX)
            {
            sprintf(sbuf,"%s is not a square matrix!",word[2]);
            sur_print(sbuf); WAIT; return;
            }

        dim=arit_atoi(word[3]);
        if (dim<2)
            {
            sur_print("\ndim parameter must be at least 2!");
            WAIT; return;
            }
        if (dim>mX)
            {
            sprintf(sbuf,"\ndim parameter must be <= matrix dimension %d",mX);
            sur_print(sbuf); WAIT; return;
            }
        i=mat_alloc(&T,mX,1); if (i<0) return;

        fast=0;
        i=spfind_mat("FAST"); if (i>=0) fast=atoi(spb[i]);

        if (g<6)
            { 
            j=maxdet(X,mX,dim,T,&det,1,fast);
            if (j<0) return; // RS ADD
            }
        else
            {
            i=atoi(word[5]);
            j=0; // RS ADD
            switch (i)
                {
              case 0: j=maxdet_all(X,mX,dim,T,&det); break;
              case 1: j=maxdet(X,mX,dim,T,&det,1,fast); break;
              case 2: j=maxdet(X,mX,dim,T,&det,2,fast); break;
              case 3: j=maxdet_rand(X,mX,dim,T,&det,1); break;
              case 4: j=maxdet_rand(X,mX,dim,T,&det,2); break;
                }
            if (j<0) return; // RS ADD
            }

        if (sh)
            {
            muste_fclose(sh_file);
            sur_print("Selections saved!"); WAIT;
            }

/*
printf("Valinta: \n");
for (i=0; i<mX; ++i) printf("%g ",Y[i]); printf("\n");
getch();
*/
        mcos=rtsafe(*max_cos,0.0,1.0,1e-10);
        sprintf(expr,"maxdet(%s)~%g_orthogonality~%g",word[2],det,
                      acos(mcos)/(3.141592653589793/2)            );
        nim(expr,exprT);

        i=matrix_save(word[4],T,mX,1,rlabX,"maxdet  ",lrX,8,-1,exprT,0,0);

        external_mat_end(argv1);
        }

/* _u_to_f.c 19.7.2001/SM (19.7.2001)
*/
static int u_to_f()
        {
        int k,i;

        for (k=0; k<mX; ++k)
            {
            T[k]=X[k];
            for (i=0; i<k; ++i)
                T[k]-=T[i]*X[k-i-1];
            }
        return(1);
        }


static void op__u_to_f()
        {
        int i;
// RS REM        char expr1[2*LLENGTH];

        i=external_mat_init(1); if (i<0) return;

        if (g<5)
            {
            init_remarks();
            rem_pr("MAT #U_TO_F U TO F");
            rem_pr("computes first occurence probabilities f1,...,fn");
            rem_pr("from occurrence probabilities u1,...,un");
            rem_pr("by the formula fk=uk-f1*u(k-1)-...-f(k-1)*u1,");
            rem_pr("k=1,...,n.");
            wait_remarks(2);
            return;
            }

        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return; }

        mT=mX; nT=nX;

        i=mat_alloc_lab(&T,mT,nT,&rlabT,&clabT);
        if (i<0) return;
        rlabT=rlabX; clabT=clabX;
        u_to_f();

        sprintf(expr,"U_TO_F(%s)",exprX);
        nim(expr,exprT);
        i=save_T(word[4]);
        mat_comment(word[4],exprT,i,mT,1,NULL);
        external_mat_end(argv1);
        }


/* _intscal.c 13.3.2003/SM (13.3.2003)
*/

static int convergents(double b,int nkonv,double eps)
    {
#define CONVERGENTS_N 20
static unsigned long a[CONVERGENTS_N],d[CONVERGENTS_N],s[CONVERGENTS_N];
    
    int i;
    double approx;

    mT=nkonv+1;
    i=mat_alloc_lab(&T,mT,5,&rlabT,&clabT); if (i<0) return(1);
// printf("\nb=%g",b); getch();
    for (i=0; i<=nkonv; ++i)
        {
        ds_ratio(b,&d[i],&s[i],i,&a[i]);
        T[i+0*mT]=(double)a[i];
        T[i+1*mT]=(double)d[i];
        T[i+2*mT]=(double)s[i];
        approx=(double)d[i]/(double)s[i];
        T[i+3*mT]=approx;
        T[i+4*mT]=approx-b;
// printf("\n%d %lu %lu %lu %.16f,%.16e%",i,a[i],d[i],s[i],approx,approx-b);
        }
        text_labels2(rlabT,mT,"fract",0);
        strcpy(clabT,"quotientp       q       p/q     p/q-x  ");
        clabT[39]=' ';
        sprintf(exprX,"%.16f_as_continued_fractions",b);
        matrix_save(word[2],T,mT,5,rlabT,clabT,8,8,-1,exprX,0,0);

// getch();
    return(1);
    }

static int op__intscal()
        {
        int i,k;
// RS REM        char expr1[2*LLENGTH];
// RS REM        char message[64];
        double eps;
        int nkonv;
        double b;

        i=external_mat_init(1); if (i<0) return(1);

        if (g<4)
            {
            init_remarks();
            rem_pr("MAT B=#INTSCAL(A)");
            rem_pr("rescales the columns of A to integers.");
            wait_remarks(2);
            return(1);
            }

        eps=1e-15;
        nkonv=10;
        if (g>4) nkonv=atoi(word[4]);

        k=0;
        if (strchr(".0123456789",*word[3])!=NULL) { k=1; b=atof(word[3]); }
        else
            {
            i=load_X(word[3]);
            if (i<0) { mat_not_found(word[3]); return(1); }
            if (mX==1 && nX==1) { k=1; b=X[0]; }
            }

        if (k)
            {
            convergents(b,nkonv,eps);
            external_mat_end(argv1);
            return(1);
            }

        for (i=0; i<nX; ++i)
            {
            mat_intval(X+i*mX,mX,eps,nkonv);
            }
        sprintf(expr,"#INTSCAL(%s)",exprX);

        nim(expr,exprX);

        i=matrix_save(word[2],X,mX,nX,rlabX,clabX,8,8,-1,exprX,0,0);
        external_mat_end(argv1);
        return(1);
        }



static int op__frac_to_dec()
        {
        int i;
// RS REM        char expr1[2*LLENGTH];
// RS REM        char message[64];
        double b;

        i=external_mat_init(1); if (i<0) return(1);

        if (g<4)
            {
            init_remarks();
            rem_pr("MAT B=#FRAC_TO_DEC(A)");
            rem_pr("converts a chained fraction presented as a vector A");
            rem_pr("of its partial quotients into a decimal number B");
            wait_remarks(2);
            return(1);
            }

        i=load_X(word[3]); if (i<0) return(-1);

        if (mX==1) mX=nX;

        mat_frac_to_dec(X,mX,&b);
        X[0]=b;
        sprintf(expr,"#FRAC_TO_DEC(%s)",exprX);
        nim(expr,exprX);
        matrix_save(word[2],X,1,1,"frac    ","frac    ",8,8,-1,exprX,0,0);
        return(1);
        }

/* _sample.c 23.3.2003/SM (23.3.2003)
*/

static int op_srs(int n_samp, int m)
    {
static int *v; // RS From local global to local    
    
    int i,j,h,k;

    v=muste_malloc(mX*sizeof(int));

    for (i=0; i<n_samp; ++i)
        {
        for (j=0; j<mX; ++j) v[j]=j;
        for (j=0; j<m; ++j)
            {
            k=j+(int)((mX-j)*sur_rand0(seed,rand_type));
            h=v[k]; v[k]=v[j]; v[j]=h;
            for (k=0; k<nX; ++k)
                T[j+i*m*nX+m*k]=X[v[j]+mX*k];
       for (k=0; k<lrX; ++k) rlabT[j*lrX+i*m*lrX+k]=rlabX[v[j]*lrX+k];
            }
        }
    if (v!=NULL) { muste_free(v); v=NULL; } // RS ADD    
    return(1);
    }
/*********************** poistettu 2.10.2003
op_srs0()
    {
    int i,k;
    int j;

    ind=muste_malloc(mX);
    for (i=0; i<mX; ++i) ind[i]=' ';

    for (i=0; i<m; ++i)
        {
        while (1)
            {
            j=(int)(double)(mX*sur_rand0(seed,rand_type));
            if (ind[j]==' ') break;
            }

        ind[j]=EOS;
        for (k=0; k<nX; ++k)
            T[i+m*k]=X[j+mX*k];
        for (k=0; k<lrX; ++k) rlabT[i*lrX+k]=rlabX[j*lrX+k];
        }
    return(1);
    }
*****************************************/
static int op_urs(int n_samp, int m)
    {
    int i,k;
    int j;
    int mm;

    mm=m*n_samp;
    for (i=0; i<mm; ++i)
        {
        j=(int)(double)(mX*sur_rand0(seed,rand_type));
// printf("\nj=%ld",j);
        for (k=0; k<nX; ++k)
            T[i+mm*k]=X[j+mX*k];
        for (k=0; k<lrX; ++k) rlabT[i*lrX+k]=rlabX[j*lrX+k];
        }
    return(1);
    }



static int op__sample()
        {
        int i;
        int k; // =1, jos useita otoksia (n_samp>1)
        char tnimi[LLENGTH];
        static int n_samp; // 2.10.2003 
        int m; // RS ADD

        i=external_mat_init(2); if (i<0) return(1);
        if (g<7)
            {
            init_remarks();
            rem_pr("MAT C=#SAMPLE(A,m,<SRS|URS>,<rand>)");
            rem_pr("makes a random sample of size m from rows of matrix A");
            rem_pr("using random number generator <rand>.");
            rem_pr("The sample is saved as a matrix file C.");
            rem_pr("By SRS a Simple Random Sample (i.e. without replacement) and");
            rem_pr("by URS a Unrestricted Random Sample (i.e. with replacement)");
            rem_pr("is generated.");
            rem_pr("");
            rem_pr("n samples of size m can be selected by");
            rem_pr("MAT C=#SAMPLE(A,m,n,<SRS|URS>,<rand>)");
            rem_pr("The samples then appear as consecutive blocks");
            rem_pr("of m rows in the matrix file C.");

            wait_remarks(2);
            return(1);
            }

        i=spec_init(r1+r-1); if (i<0) return(1);

// 2.10.2003
        n_samp=1; k=0;
        if (g>7) { k=1; n_samp=atoi(word[5]); }

        i=load_X(word[3]);
        if (i<0) { mat_not_found(word[3]); return(1); }

        m=atoi(word[4]);

        if (m>mX && muste_strcmpi(word[5+k],"SRS")==0 )
            {
            sprintf(sbuf,"\nSample size (%d) must not exceed 'population size' (%d)",
                          m,mX);
            sur_print(sbuf); WAIT; return(-1); // RS CHA exit
            }
// TÑssÑ sallitaan vain pelkkÑ luku, koska MAT herjaa "Unknown op.."
        if (muste_strnicmp(word[6+k],"rand(",5)==0) { rand_type=1; i=5; }
        else if (muste_strnicmp(word[6+k],"urand(",6)==0) { rand_type=2; i=6; }
        else { rand_type=1; i=0; }
        seed=atof(word[6+k]+i);

        i=mat_alloc_lab(&T,m*n_samp,nX,&rlabT,NULL);
/************** poistettu 2.10.2003
        ind=muste_malloc(mX);
        for (i=0; i<mX; ++i) ind[i]=' ';
********************/
        if (muste_strcmpi(word[5+k],"SRS")==0)
            op_srs(n_samp,m); // RS ADD ,m
        else
            op_urs(n_samp,m); // RS ADD ,m

//      text_labels(rlabT,m,"");

        sprintf(tnimi,"Sample(s)_of_size_%d_from_%s",m,word[3]);

        nim(tnimi,exprX);
  i=matrix_save(word[2],T,m*n_samp,nX,rlabT,clabX,lrX,lcX,-1,exprX,0,0);
//      outseed();
//      mat_comment(word[2],exprX,i,mX,nX,NULL);
        external_mat_end(argv1);

        return(1);
        }


/* _sort.c 27.3.2003/SM (27.3.2003)
*/

static int shell_sort(double *a,int n,int *nro)
        {
        int i;
        unsigned int h,k;
        char ind;
        double y;

        h=n;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind='1';
                for (k=0; k<n-h; ++k)
                    {
                    if (a[k]>a[k+h])
                        {
                        y=a[k]; a[k]=a[k+h]; a[k+h]=y;
                        i=nro[k]; nro[k]=nro[k+h]; nro[k+h]=i;
                        ind='0';
                        }
                    }
                if (ind=='1') break;
                }
            }
        return(1);
        }



static int op__sort()
        {
        int i,k;
        char tnimi[LLENGTH];
        
        static double *a; // RS From local globals to local
		static int *nro;

        i=external_mat_init(2); if (i<0) return(1);
        if (g<5)
            {
            init_remarks();
            rem_pr("MAT C=#SORT(A,k)");
            rem_pr("sorts the rows of matrix A in ascending order");
            rem_pr("of the elements of its k'th column.");
            wait_remarks(2);
            return(1);
            }

        i=load_X(word[3]);
        if (i<0) { mat_not_found(word[3]); return(1); }

        a=(double *)muste_malloc(mX*sizeof(double));
        nro=(int *)muste_malloc(mX*sizeof(int));

        mat_alloc_lab(&T,mX,nX,&rlabT,NULL);

        k=atoi(word[4])-1;
        if (k<0) k=1;
        if (k+1>nX)
            {
            sprintf(sbuf,"\nOnly %d columns in matrix %s!",
                                 nX,word[3]);
            sur_print(sbuf); WAIT; return(-1);
            }

        for (i=0; i<mX; ++i) { a[i]=X[i+mX*k]; nro[i]=i; }
        shell_sort(a,mX,nro);
// for (i=0; i<mX; ++i) printf("\n%d|",nro[i]); getch();


        for (i=0; i<mX; ++i)
            {
            for (k=0; k<nX; ++k)
                T[i+mX*k]=X[nro[i]+mX*k];
            for (k=0; k<lrX; ++k)
                rlabT[i*lrX+k]=rlabX[nro[i]*lrX+k];
            }

        sprintf(tnimi,"#SORT(%s,%d)",word[3],k+1);

        nim(tnimi,exprX);
        i=matrix_save(word[2],T,mX,nX,rlabT,clabX,lrX,lcX,-1,exprX,0,0);
        
        if (a!=NULL) { muste_free(a); a=NULL; } // RS ADD
        if (nro!=NULL) { muste_free(nro); nro=NULL; } // RS ADD
        
        return(1);
        }



/* _magic.c 26.8.2003/SM (26.8.2003)
*/

/* print half of i'th row */
/**************************/
static void half_row( int first_half, int mid_row, int i, int n, int q1, int q2, int q3, int q4 )
   {
   int N = n>>1, m = n>>2;
   int j, t, val, N1 = (N-1)>>1, N3 = N1 - 1;
   int offset = (n*n)>>2;      /* difference between quarter boards */

   for (j=1; j <= N; j++)
      {
      t = N1-i+j;

      if (t >= N) t -= N;
      else if (t < 0) t += N;

      val = 1 + t + N*((N3+i+j)%N);      /* val is corresponding value
                                            in upper-left quarter */

      if (first_half)
         {
         if ((mid_row && j > 1 && j <= m+1) || (!mid_row && j <= m))
            val += q1*offset;   /* swap square with corr. square */
         else                   /* in other half of board */
            val += q4*offset;   /* don't swap */
         }
      /* second half */
      /***************/
      else if (j > (N-m+1))
         val += q2*offset;     /* swap square */
      else
         val += q3*offset;     /* don't swap */

//    printf( "%5d ", val );
      X[ii+mm*jj]=val; ++jj;
      }
   }



/* print i'th row of this half */
/*******************************/
static void row_i( int mid_row, int i, int n, int q1, int q2, int q3, int q4 )
   {
   half_row( 1, mid_row, i, n, q1, q2, q3, q4 );  /* first half of row */
   half_row( 0, mid_row, i, n, q1, q2, q3, q4 );  /* second half of row */

// printf( "\n" );
   ++ii; jj=0;
   }


/******************************************/
/* quarter-board plus swap pattern method */
/******************************************/

/* print half a board
 */
static void half_even( int n, int q1, int q2, int q3, int q4 )
   {
   int i, halfn = n>>1, m = n>>2;

   for (i=1; i <= m; i++)                 /* upper m rows of half board */
      row_i( 0, i, n, q1, q2, q3, q4 );

   row_i( 1, m+1, n, q1, q2, q3, q4 );    /* middle row of half board */

   for (i=m+2; i <= halfn; i++)
      row_i( 0, i, n, q1, q2, q3, q4 );   /* lower m rows of half board */
   }


/**********************/
/* "Nine block method */
/**********************/
static void doubly_even( int n )
   {
   int i, j, num = 1;
   int nminus = n-1, nn = n*n+1;
   int block1 = (n-2)>>2, block2 = nminus - block1;
   int inside1 = n>>2, inside2 = nminus - inside1;

   for (j=0; j < n; j++)
      {
      for (i=0; i < n; i++)
         {
         if (i >= inside1 && i <= inside2 &&         /* middle block */
             j >= inside1 && j <= inside2)
//          printf( "%5d ", num );
         { X[ii+mm*jj]=num; ++jj; }

         else if ((i > block1 && i < block2) ||      /* not middle block */
                  (j > block1 && j < block2))        /* and not corner block */
//          printf( "%5d ", nn - num );
            { X[ii+mm*jj]=nn-num; ++jj; }
         else                                        /* corner block */
//          printf( "%5d ", num );
            { X[ii+mm*jj]=num; ++jj; }

         num++;
         }
      ++ii; jj=0;
      printf( "\n" );
      }
   }


/********************/
/* "lozenge" method */
/********************/
static void odd_order( int N )
   {
   int i, j, N1, N3, t;

   N1 = (N-1)>>1;
   N3 = N1-1;

   for (i=1; i <= N; i++)
      {
      for (j=1; j <=N; j++)
         {
         t = N1-i+j;

         if (t >= N) t -= N;
         else if (t < 0) t += N;

//       printf( "%5d ", 1 + t + N * ((N3+i+j)%N) );
         X[ii+mm*jj]=1 + t + N * ((N3+i+j)%N);
         ++jj;
         }
//    printf( "\n" );
      ++ii; jj=0;
      }
   }


static int magic(int n)
   {

   if ((n & 1))
      odd_order(n);
   else if (!(n & 2))
      doubly_even(n);
   else
      {
      half_even( n, 3, 1, 2, 0 );
      half_even( n, 0, 2, 1, 3 );
      }

// printf( "\n\nThe sum of each row and column is %8d\n\n", n*(n*n+1) >> 1 );
// exit(0);
   return(1);
   }



static int op__magic()
        {
        int i; // ,k;
// RS REM        char expr1[2*LLENGTH];
// RS REM        char message[64];

        i=external_mat_init(1); if (i<0) return(1);

        if (g<4)
            {
            init_remarks();
            rem_pr("MAT M=#MAGIC(m)");
            rem_pr("makes a m*m magic square.");
            wait_remarks(2);
            return(1);
            }

        mm=atoi(word[3]);

        if (mm<3) return(1);

        varaa_tila(&X,mm,mm,&rlabX,&clabX,8,8);
        text_labels2(rlabX,mm,"M",1); // RS ADD 2 ,1
        text_labels2(clabX,mm,"M",1); // RS ADD 2 ,1

        ii=jj=0;

        magic(mm);

        sprintf(expr,"#MAGIC(%d)",mm);

        nim(expr,exprX);

        i=matrix_save(word[2],X,mm,mm,rlabX,clabX,8,8,-1,exprX,0,0);
        external_mat_end(argv1);
        return(1);
        }


/* _jack.c 2.10.2003/SM (2.10.2003)
*/
static int op__jack()
        {
        int i,j,h,k,m;
        char tnimi[LLENGTH];

        i=external_mat_init(2); if (i<0) return(1);
        if (g<4)
            {
            init_remarks();
            rem_pr("MAT C=#JACK(A) or MAT C=#JACKKNIFE(A)");
            rem_pr("makes the n jackknife samples of the n rows of");
            rem_pr("matrix A and saves them in a matrix file C.");
            rem_pr("C will have n*(n-1) rows.");
            wait_remarks(2);
            return(1);
            }

        i=load_X(word[3]);
        if (i<0) { mat_not_found(word[3]); return(1); }
        m=mX;

        i=mat_alloc_lab(&T,m*(m-1),nX,&rlabT,NULL);
        if (i<0) return(1);

        j=0;
        for (i=0; i<m; ++i)
            {
            for (h=0; h<m; ++h)
                {
                if (h==i) continue;

            for (k=0; k<nX; ++k)
                T[j+m*k]=X[h+mX*k];
       for (k=0; k<lrX; ++k) rlabT[j*lrX+k]=rlabX[h*lrX+k];
                ++j;
                }

            }


        sprintf(tnimi,"Jackknife_samples_of_%s",word[3]);

        nim(tnimi,exprX);
  i=matrix_save(word[2],T,m*(m-1),nX,rlabT,clabX,lrX,lcX,-1,exprX,0,0);
        external_mat_end(argv1);

        return(1);
        }

/* #eigen.c 5.7.2005/SM (5.7.2005)
*/
// MAT #EIGFEW(A,k,S,L,eps,max_iter)

static int op__eigfew()
        {
static double *L,*S,*v,*w; // RS From local global to local
static char *textlab;        
        
        int i,j,h,k,n;
        double t,a,b;
//      char expr1[2*LLENGTH];
//      char message[64];
        int iter,max_iter;
        double eps;
        char message[32];

        i=external_mat_init(1); if (i<0) return(-1);

        if (g<3)
            {
            init_remarks();

rem_pr("MAT #EIGFEW(A,k,S,L)");
rem_pr("computes k largest eigenvalues L and");
rem_pr("their eigenvectors S of matrix A by the power method.");

            wait_remarks(2);
            return(-1);
            }


        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return(-1); }
        if (mX!=nX)
            {
            sprintf(sbuf,"%s is not a square matrix!",word[2]);
            sur_print(sbuf); WAIT; return(-1);
            }

        n=mX;
        k=atoi(word[3]);

        L=(double *)muste_malloc(k*sizeof(double));
        S=(double *)muste_malloc(k*n*sizeof(double));
        v=(double *)muste_malloc(n*sizeof(double));
        w=(double *)muste_malloc(n*sizeof(double));

        textlab=muste_malloc(k*8);

        eps=1e-14;
        if (g>6) eps=atof(word[6]);

        max_iter=100;
        if (g>7) max_iter=atoi(word[7]);


        for (i=0; i<k; ++i)
            {
            for (j=0; j<n; ++j) v[j]=0.0;
            v[i]=1.0;
            iter=0;
            while(1)
                {
                ++iter;
                mat_mlt(w,X,v,n,n,1);
                mat_nrm(&t,w,n,1);
                a=0.0;
                for (j=0; j<n; ++j)
                    {
                    b=v[j]-w[j]; a+=b*b;
                    v[j]=w[j];
                    }

                a=sqrt(a/(double)n); // 23.7.2005
                if (a<eps || iter>max_iter)
                    {
                    L[i]=t;
                    for (j=0; j<n; ++j) S[j+n*i]=v[j];
                    sprintf(message,"\n%d: %d (%1.1e)",i+1,iter,a);
                    sur_print(message);

                    break;
                    }
                } // while

            for (j=0; j<n; ++j)
                for (h=0; h<n; ++h)
                    X[j+n*h]-=t*v[j]*v[h];

            } // i

        text_labels2(textlab,k,"eigen",1); // RS ADD 2 ,1

        strcpy(expr,"Eigenvalues");
        nim(expr,exprT);
        i=matrix_save(word[5],L,k,1,textlab,"eigenval",8,8,-1,exprT,0,0);

        strcpy(expr,"Eigenvectors");
        nim(expr,exprT);
        i=matrix_save(word[4],S,mX,k,rlabX,textlab,8,8,-1,exprT,0,0);

		if (L!=NULL) { muste_free(L); L=NULL; } // RS ADD
		if (S!=NULL) { muste_free(S); S=NULL; } // RS ADD
		if (v!=NULL) { muste_free(v); v=NULL; } // RS ADD	
		if (w!=NULL) { muste_free(w); w=NULL; } // RS ADD	
		if (textlab!=NULL) { muste_free(textlab); textlab=NULL; } // RS ADD		

        external_mat_end(argv1);
        sur_sleep(100L);
        return(1);
        }


/* #eigen.c 5.7.2005/SM (5.7.2005)
*/
// MAT #EIGLAN(A,k,S,L)

/* RS REM not used?
static double frand()
    {
    return( (double)rand()/(double)RAND_MAX );
    }
*/

static int op__eiglan()
        {
static double *L,*S,*W,*X2,*y;
static double *alfa,*beta,*alfa0,*beta0;
static char *textlab;
char l_file[LNAME];        
        
        
        int i,j,h,k,n,nl,j1=0,n0,n1,nd;
        double t=0,a; // ,b;
// RS REM        char message[32];
        double *dp;
        int hilbert;
        double eps,xmax;


        i=external_mat_init(1); if (i<0) return(-1);

        if (g<3)
            {
            init_remarks();

rem_pr("MAT #EIGLAN(A,k,S,L,eps,n_iter,L_file)");
rem_pr("computes k largest eigenvalues L and");
rem_pr("their eigenvectors S of matrix A by the Lanczos method.");
rem_pr("eps (default 1e-13) is the tolerance parameter.");
rem_pr("n_iter (default 430) is the max. number of Lanczos iterations.");
rem_pr("If L_file is given, Lanczos vectors are saved as L_file.");
rem_pr("");
rem_pr("This is an efficient method for computing a few eigenvalues");
rem_pr("and their eigenvectors when the dimension of A is large,");
rem_pr("say, more than 300.");
rem_pr("");
rem_pr("Reference: Golub and van Loan: Matrix computations,");
rem_pr("           Chapter 9.2");
            wait_remarks(2);
            return(-1);
            }


        srand(2005);

        hilbert=0;
        if (strncmp(word[2],"__HIL",5)==0)
            {
            hilbert=1;
            mX=nX=atoi(word[2]+5);
            X=NULL;
            }
        else
            i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return(-1); }
        if (mX!=nX)
            {
            sprintf(sbuf,"%s is not a square matrix!",word[2]);
            sur_print(sbuf); WAIT; return(-1);
            }

        n=mX;
        k=atoi(word[3]);

        xmax=0.0;
        for (i=0; i<n*n; ++i)
            { a=fabs(X[i]); if (a>xmax) xmax=a; }

        a=1.0/xmax;
        for (i=0; i<n*n; ++i)
              X[i]*=a;

        eps=1e-13;
        if (g>6) eps=atof(word[6]);

        nl=500;
        if (g>7) nl=atoi(word[7]);
        if (nl>n) nl=n;
// printf("\nk=%d nl=%d g=%d",k,nl,g); getch();

        *l_file=EOS; W=NULL;
        if (g>7)
            {
            strcpy(l_file,word[8]);
            }
        L=(double *)muste_malloc(nl*sizeof(double));
        S=(double *)muste_malloc(n*nl*sizeof(double));
        X2=(double *)muste_malloc(nl*nl*sizeof(double));
        y=(double *)muste_malloc(n*sizeof(double));
        alfa=(double *)muste_malloc((nl+1)*sizeof(double));
        beta=(double *)muste_malloc((nl+1)*sizeof(double));
        alfa0=(double *)muste_malloc((nl+1)*sizeof(double));
        beta0=(double *)muste_malloc((nl+1)*sizeof(double));
        textlab=muste_malloc(n*8);
        W=(double *)muste_malloc(n*(nl+1)*sizeof(double)); // for Lanczos vectors

        for (h=0; h<k; ++h)
            {
            n0=0; n1=10; nd=0;
            for (i=0; i<k+1; ++i) alfa[i]=beta[i]=0.0;
            sprintf(sbuf,"\n%d: ",h+1); sur_print(sbuf);
          while (n1<=nl)
              {
//          sprintf(sbuf,"%d ",n1); sur_print(sbuf);
            j1=mat_lanczos(X,alfa,beta,n,n0,n1,W);
            for (i=0; i<nl+1; ++i) { alfa0[i]=alfa[i]; beta0[i]=beta[i]; }
            for (i=0; i<j1*j1; ++i) X2[i]=0.0;
            for (i=0; i<j1; ++i) X2[i*(j1+1)]=1.0;
// mprint(alfa0+1,1,j1);
// mprint(beta0+1,1,j1);

            i=mat_tqlb(alfa0+1,beta0,j1,X2);
            if (i<0)
                {
    sprintf(sbuf,"\nTry with a larger precision parameter than %e",eps);
    sur_print(sbuf); WAIT; return(-1);
                }
//          mat_tql2(alfa0+1,beta0,X2,j1,1e-16,30);
            L[h]=alfa0[1];
// printf("\nL[h]=%g",L[h]); getch();
            dp=S+n*h;
            mat_mlt(S+n*h,W,X2,n,j1,1);

            t=0.0;
            for (i=0; i<n; ++i)
                t+=dp[i]*dp[i];
            t=1.0/sqrt(t);
            for (i=0; i<n; ++i) dp[i]*=t;

            t=0.0;
            for (i=0; i<n; ++i)
                {
                a=0.0;
                for (j=0; j<n; ++j) a+=X[i+n*j]*dp[j];
                a=a-L[h]*dp[i];
                t+=a*a;
                }
            t=sqrt(t/(double)n);
            if (t<eps) break;
// RS REM  if (j1<n1) { printf("\nt=%g",t); getch(); break; }
            ++nd;
            n0=n1; n1+=20*nd;
              } // while n1;
            if (n1>nl) { n1=n0; sur_print(" *"); }
            else sur_print(" ");
            sprintf(sbuf,"n=%d (%1.1e)",n1,t); sur_print(sbuf);

            if (h==0 && *l_file!=EOS)
                {
                text_labels2(textlab,j1,"L",1); // RS ADD 2 ,1
                strcpy(expr,"Lanczos_vectors");
                nim(expr,exprT);
              matrix_save(l_file,W,n,j1,rlabX,textlab,8,8,-1,exprT,0,0);
                }

//     mprint(alfa,1,j1+1);
//     mprint(beta,1,j1+1);
//     mprint(W,n,j1);
            if (h==0 && *l_file!=EOS)
                {
                strcpy(expr,"Eigenvectors");
                strcpy(sbuf,l_file);
                strcat(sbuf,"V");
                nim(expr,exprT);
              matrix_save(sbuf,X2,j1,j1,textlab,textlab,8,8,-1,exprT,0,0);
                strcpy(expr,"Eigenvalues");
                strcpy(sbuf,l_file);
                strcat(sbuf,"L");
                nim(expr,exprT);
              for (i=0; i<j1; ++i) alfa0[i+1]*=xmax;
              matrix_save(sbuf,alfa0+1,j1,1,textlab,"eig.val.",8,8,-1,exprT,0,0);
                }

            mat_lanczos(X,alfa,beta,n,-1,nl,W); // -1=free(...)

            if (h<k-1)
                for (i=0; i<n; ++i)
                    {
                    double aa=L[h]*S[i+n*h];
                    for (j=0; j<=i; ++j)
                        {
                        X[i+n*j]-=aa*S[j+n*h];
                        X[j+n*i]=X[i+n*j];
                        }
                    }
            } // h

        text_labels2(textlab,k,"eigen",1); // RS ADD 2 ,1
        strcpy(expr,"Eigenvalues");
        nim(expr,exprT);
        for (i=0; i<k; ++i) L[i]*=xmax;
        i=matrix_save(word[5],L,k,1,textlab,"eigenval",8,8,-1,exprT,0,0);

     if (!hilbert)
        {
        strcpy(expr,"Eigenvectors");
        nim(expr,exprT);
        i=matrix_save(word[4],S,mX,k,rlabX,textlab,8,8,-1,exprT,0,0);
        }
        
        if (L!=NULL) { muste_free(L); L=NULL; } // RS ADD
        if (S!=NULL) { muste_free(S); S=NULL; } // RS ADD
        if (W!=NULL) { muste_free(W); W=NULL; } // RS ADD
        if (X2!=NULL) { muste_free(X2); X2=NULL; } // RS ADD
        if (y!=NULL) { muste_free(y); y=NULL; } // RS ADD
        if (alfa!=NULL) { muste_free(alfa); alfa=NULL; } // RS ADD
        if (beta!=NULL) { muste_free(beta); beta=NULL; } // RS ADD
        if (alfa0!=NULL) { muste_free(alfa0); alfa0=NULL; } // RS ADD
        if (beta0!=NULL) { muste_free(beta0); beta0=NULL; } // RS ADD
        if (textlab!=NULL) { muste_free(textlab); textlab=NULL; } // RS ADD   
        
        external_mat_end(argv1);
        sur_sleep(100L);
        return(1);
        }

/* #aggre.c 9.10.2006/SM (9.10.2006)
*/
static int op__aggre()
        {
        static int *freq; // RS From local global to local
        
        int i,j,m,n,k,h,s,u;
// RS REM        char expr1[2*LLENGTH];

        i=external_mat_init(1); if (i<0) return(1);
        if (g<6)
            {
            init_remarks();
            rem_pr("MAT #AGGRE(B,A,m,n");
            rem_pr("computes frequencies of values m,m+1,m+2m...,n");
            rem_pr("for each row of A and makes a matrix B of these frequencies.");
            rem_pr("B will be a matrix of rowA rows and n-m+1 columns.");

            wait_remarks(2);
            return(1);
            }

        i=load_X(word[3]); if (i<0) { mat_not_found(word[3]); return(1); }
        m=atoi(word[4]);
        n=atoi(word[5]);

// printf("\nm=%d n=%d|",m,n); getch();
        k=n-m+1;
        mT=mX;
        nT=k;
        freq=(int *)muste_malloc(nT*k);
        if (freq==NULL) return(1); // RS ADD

        i=mat_alloc_lab(&T,mT,nT,&rlabT,&clabT);
        if (i<0) return(1);

        rlabT=rlabX;
        numlab2(clabT,k,8,m);

        for (i=0; i<mX; ++i)
            {
            for (h=0; h<k; ++h) freq[h]=0;
            s=i;
            for (j=0; j<nX; ++j)
                {
                u=(int)X[s]-m;
                if (u<0 || u>k-1)
                   {
   sprintf(sbuf,"\nAll elements of %s must be integers from %d to %d!",
                       word[3],m,n);
                   sur_print(sbuf); WAIT; return(-1); // RS CHA exit(0);
                   }
                ++freq[u];
                s+=mX;
                }
            s=i;
            for (h=0; h<k; ++h)
                {
                T[s]=(double)freq[h];
                s+=mT;
                }
            }

        sprintf(expr,"AGGRE(%s)",exprX);
        nim(expr,exprT);
        i=save_T(word[2]);
        mat_comment(word[2],exprT,i,mT,nT,"");
        if (freq!=NULL) { muste_free(freq); freq=NULL; } // RS ADD
        external_mat_end(argv1);
        return(1);
        }

/* #transfo.c 9.12.1995/SM (11.12.1995) (5.7.1998)
*/


/* mvarit#.c 31.10.1985/SM  (3.7.1993) (10.12.1995)
   aritmetiikka (+ - * / ^) ja funktiot
   probit()
*/

static int laske_mvarit(char *lauseke,double *y);
static double luku_mvarit(char *sana,int len);
static double oper_mvarit(double x1,double x2,char laji);
static double power_mvarit(double x,double y);
static void supista_mvarit(int *t,double opnd[],char op[],int v[]);
static double funktio_mvarit(char *s,double x);
static double mfunktio_mvarit(char *s,double *x,int n);
static int laske2_mvarit(char *muuttuja,double *y);
static int varif_mvarit(char *lauseke,double *y);
static void if_syntax_error(char *x);
static int f_edit_mvarit(char *s,double *x,int n,double *py);
static void korvaa(char *s,char *x,char *y);
static int varaa_earg();
static int aseta_earg(double luku,char *sana);
static int arifor_mvarit(char *lauseke,double *y);
static int parsplit(char *x,char **a,char **b,int max);
static int arit_atoi_mvarit(char *s);
        

/* mvarit#.c 31.10.1985/SM  (3.7.1993) (10.12.1995)
   aritmetiikka (+ - * / ^) ja funktiot
   probit()
*/

static int laske_mvarit(char *lauseke,double *y)
        {
//        double luku_mvarit();
//        double oper_mvarit();
//        double funktio_mvarit();
//        double mfunktio_mvarit();

        char x[LLENGTH];
        char *p,*q;
        char sana[32];
        int len;
        double opnd[MAXARG+4]; char op[MAXARG+4]; int v[MAXARG+4];
        int t,n;
        int narg; /* Usean muuttujan funktion argumenttien lkm     */
        int i;
    /*  double dlag; # */
// printf("lauseke=%s\n",lauseke); getch();
        if (*lauseke=='i')
            {
            if (strncmp(lauseke,"if(",3)==0)
                return(varif_mvarit(lauseke,y));
            }
        if (*lauseke=='f')
            {
            if (strncmp(lauseke,"for(",4)==0) // RS CHA 3 -> 4
                return(arifor_mvarit(lauseke,y));
            }
        strcpy(x,lauseke);
        len=0;
        p=x;
        t=0;
     /* lag=0; # */

        while (*p)
            {
            if (l_virhe) return(-1);
            switch (*p)
                {
              case '+':
                if (len==0) { ++p; break; }
                if (len>0) opnd[t]=luku_mvarit(sana,len); len=0;
                op[t]='+'; v[t++]=1;
                supista_mvarit(&t,opnd,op,v);
                ++p;
                break;

              case '-':
                if (len==0) { sana[len++]=*p; ++p; break; }
                if (len>0) opnd[t]=luku_mvarit(sana,len); len=0;
                op[t]='-'; v[t++]=1;
                supista_mvarit(&t,opnd,op,v);
                ++p;
                break;

              case '*':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku_mvarit(sana,len); len=0;
                op[t]='*'; v[t++]=2;
                supista_mvarit(&t,opnd,op,v);
                ++p;
                break;

              case '/':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku_mvarit(sana,len); len=0;
                op[t]='/'; v[t++]=2;
                supista_mvarit(&t,opnd,op,v);
                ++p;
                break;

              case '^':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku_mvarit(sana,len); len=0;
                op[t]='^'; v[t++]=3;
                supista_mvarit(&t,opnd,op,v);
                ++p;
                break;
/* #
              case '[':
                q=strchr(p,']');
                if (len==0 || q==NULL) { syntax_error(lauseke); return(-1); }
                *q=EOS;

                laske(p+1,&dlag); lag=(int)dlag;

                p=q+1;
                break;
*/
              case '(':

/* #            if (*sana=='p')
                    {
                    if (strncmp(sana,"pos",3)==0)
                        {
                        p+=pos_funktio(p,&opnd[t]);
                        if (l_virhe) return(-1);
                        len=-1;
                        break;
                        }
                    }
 */
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

                        laske_mvarit(q,&opnd[t]);
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
/*   printf("\nq=%s",q); getch();   */
                i=laske_mvarit(q,&opnd[t]);
                if (i<0 || l_virhe) return(-1);
/*   printf("\ntulos1=%f",opnd[t]); getch();  */
                if (len==0) { len=-1; break; }
                sana[len]=EOS;

                if (narg>1)
                    {
/*
             printf("\nArgumentit: ");
             for (i=t-narg+1; i<=t; ++i) printf(" %g",opnd[i]); getch();
*/
                    t=t-narg+1;
                    if (*sana=='-')
                        opnd[t]=-mfunktio_mvarit(sana+1,opnd+t,narg);
                    else
                        opnd[t]=mfunktio_mvarit(sana,opnd+t,narg);
                    if (l_virhe) return(-1);
                    len=-1;
                    break;
                    }

                /* Yhden muuttujan funktiot */
                if (*sana=='-')
                    opnd[t]=-funktio_mvarit(sana+1,opnd[t]);
                else
                    opnd[t]=funktio_mvarit(sana,opnd[t]);
                if (l_virhe) return(-1);
                len=-1;
                break;

              case ')':
                sprintf(sbuf,"\n( missing in %s",lauseke); sur_print(sbuf); l_virhe=1; return(-1);

/*            case ':':
                sdata=atoi(sana+1);
                if (!sdata) { sana[len++]=*p; ++p; break; }
                len=0; ++p;
                break;
*/
              case 'e': case 'E':
                if (strchr("+-.0123456789",sana[0])!=NULL)
                    {
                    sana[len++]=*p; ++p;
                    if (*p!='+' && *p!='-') break;
                    }
              /* default seurattava suoraan case 'e':n jÑlkeen */
              default:
                /* tarkistukset puuttuvat */
                sana[len++]=*p;
                ++p;
                }
            }

        if (len<0) { v[t++]=0; }
        else
                   if (len>0) { opnd[t]=luku_mvarit(sana,len); v[t++]=0; }

        supista_mvarit(&t,opnd,op,v);
        *y=opnd[0];

        return(1);
        }

static double luku_mvarit(char *sana,int len)
        {
        char *p;
        double tulos=1.0;
        int i;

        sana[len]=EOS;
        p=sana; if (*p=='-') ++p;
        if (strchr("1234567890.",*p)==NULL)
            {
            i=laske2_mvarit(p,&tulos); if (i<0) return((double)1.0);
            if (*sana=='-') return(-tulos);
            return(tulos);
            }
        return(atof(sana));
        }

static double oper_mvarit(double x1,double x2,char laji)
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
            return(power_mvarit(x1,x2));
            }
        return(0.0);
        }

static double power_mvarit(double x,double y)
        {
        int i;
        double f;
        if (y>=0 && y==floor(y) && y<10)
                {
                f=1;
                for (i=0; i<(int)y; ++i) f*=x;
                return(f);
                }
        return (muste_pow(x,y));
        }


static void supista_mvarit(int *t,double opnd[],char op[],int v[])
        {

        while (*t>1)
            {
            if (v[*t-1]>v[*t-2]) return;
            opnd[*t-2]=oper_mvarit(opnd[*t-2],opnd[*t-1],op[*t-2]);
            op[*t-2]=op[*t-1]; v[*t-2]=v[*t-1];
            --(*t);
            }
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

static double funktio_mvarit(char *s,double x)
        {
        int i;
        double y;
        char S[32];

        if (*s==EOS) return(x);
        if (x==MISSING8) return(x);
        strncpy(S,s,31); S[31]=EOS; muste_strupr(S);

        if (strcmp(S,"RAND")==0) return(sur_rand0(x,1));
        else if (strcmp(S,"URAND")==0) return(sur_rand0(x,2));
        else if (strcmp(S,"SRAND")==0) return(sur_rand0(x,3));
        else if (strcmp(S,"MRAND")==0) return(sur_rand0(x,4));
        else if (strncmp(S,"SQR",3)==0)  /* 29.9.1996 x<0.0 etc. */
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
// RS ADD END

        i=f_edit_mvarit(s,&x,1,&y); if (i>0) return(y);
/*      i=f_tiedosto(s,&x,1,&y); if (i>0) return(y); */
/* RS FIXME Väliaikaisesti pois käytöstä
        i=family_f(s,&x,1,&y); if (i>0) return(y);
*/
        l_virhe=1;
        return(MISSING8);
        }

static double mfunktio_mvarit(char *s,double *x,int n)
        {
        int i;
        double y;
        char S[32];

/*     printf("\nmfunktio: ");
     for (i=0; i<n; ++i) printf("%g ",x[i]); getch();
*/
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

        i=f_edit_mvarit(s,x,n,&y); if (i>0) return(y);
/*      i=f_tiedosto(s,x,n,&y); if (i>0) return(y); */
/*  RS NYI Väliaikaisesti pois käytöstä
        i=family_f(s,x,n,&y); if (i>0) return(y);
*/
        l_virhe=1;
        return(MISSING8);
        }

static int laske2_mvarit(char *muuttuja,double *y)
        {
        int i,k;
        char *pvar;

        if (*muuttuja==EARG) { *y=earg[atoi(muuttuja+1)]; return(1); }
/* #    if (*muuttuja=='D' && (muuttuja[2]==':' || muuttuja[3]==':') )
            { i=sup_arvo(muuttuja,y); return(i); }
        if (lag)
            {
            i=lag_arvo(muuttuja,y); lag=0; if (i<0) l_virhe=1;
            return(i);
            }
*/
// for (i=0; i<spn; ++i) printf("\n%d %s|",i,spa[i]); getch();
// tr_
        i=spfind(muuttuja); // spfind sortuu spb[]=NULL-tapauksiin
        if (i<0)
            {
  /*        i=varfind2(&d,muuttuja,0);
            if (i<0)
                {
  */            sprintf(sbuf,"\nValue of %s not found!",muuttuja);
                sur_print(sbuf); WAIT;
      /*        poista_uudet_muuttujat();  */
                l_virhe=1; return(-1); // RS CHA exit(1) -> return(-1)
      /*        }    */
/*          data_load(&d,jnro,i,y);

            return(1);
*/          }

        if (spb[i]==NULL) { *y=arvo[i]; return(1); }
/* #    if (nvar) { pvar=spa[i]; spa[i]=NULL; }  */
        k=laske_mvarit(spb[i],y);
/* #    if (nvar) spa[i]=pvar; */
        arvo[i]=*y;
        spb[i]=NULL;
        return(1);
        }

/* varif#.c 30.4.1985/SM  (16.5.1986) (13.3.1991) (10.12.1995)
   aritmetiikka if(ehto)then(a)else(b)
*/

static int varif_mvarit(char *lauseke,double *y)
        {
        char *a,*b,*c,*d;
        char rel;
        char *p;
        int sulut;
        char x[LLENGTH];
        double y1;
        int tosi;


 /*     printf("varif: %s\n",lauseke); getch();  */

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

/*  printf("\na=%s rel=%c",a,rel);  */
        b=p+1;
        p=b;
        while (1)
            {
            p=strchr(p,')');
            if (p==NULL) { if_syntax_error(lauseke); return(-1); }
            if (strncmp(p,")then(",6)==0) { *p=EOS; break; }
            ++p;
            }
/*  printf(" b=%s",b);   getch();  */
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
/* printf(" c=%s d=%s",c,d);
getch();
*/
        if (strncmp(a,"str(",4)==0)             /* 13.3.1991 */
            {
      /*    tosi=strvert(a,rel,b,c,d,y); */
            sur_print("\nString comparisons not permitted!"); WAIT; return(-1);
      /*    if (tosi<0) return(-1);  */
            }

        else
            {
            laske_mvarit(a,y);
            laske_mvarit(b,&y1);
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
        if (tosi) laske_mvarit(c,y);
        else      laske_mvarit(d,y);
        return(1);
        }

static void if_syntax_error(char *x)
        {
        sprintf(sbuf,"\nSyntax error in %s",x); sur_print(sbuf);
        WAIT; l_virhe=1;
        }

/* varifct#.c 28.12.1986 (3.1.1987) (10.12.1995)
   kuten arifct.c
*/

static int f_edit_mvarit(char *s,double *x,int n,double *py)
        {
        int i,k,len;
        char lauseke[LLENGTH];
        char xx[LLENGTH], *osa[MAXARG];
        char sana[7];     /*  EARG 1 2 3 4 EARG EOS */
        double y;

/* printf("s=%s %g %g\n",s,x[0],x[1]); getch(); */
        if (strcmp(s,rec_func)==0)
            {

            i=x[0]; if (n==2) k=x[1]; else k=0;
            if (i<0 || k<0) { *py=0.0; return(1); }
            *py=X[i+mX*k];
            if (*py!=MISSING8) return(1);
            }

        len=strlen(s); s[len++]='(';
        i=0;
        while (i<spn &&
               (spa[i][strlen(spa[i])-1]!=':' || strncmp(s,spa[i],len)!=0)) ++i;
        if (i==spn) { s[len-1]=EOS; return(-1); }

        if (!earg_varattu) { k=varaa_earg(); if (k<0) return(-1); }

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
    for (i=0; i<n; ++i) printf("osa %d: %s\n",i+1,osa[i]); getch();
*/
        for (i=0; i<n; ++i)
            {
            k=aseta_earg(x[i],sana); if (k<0) return(-1);
            korvaa(lauseke,osa[i],sana);
            }
        laske_mvarit(lauseke,&y);
        *py=y;
        n_earg-=n;
        return(1);
        }

static void korvaa(char *s,char *x,char *y)
        {
        char *p,*q;
        char z[LLENGTH];
        int len=strlen(x);

        *z=EOS;
        p=s;
        while ((q=strstr(p,x))!=NULL)
            {
            strncat(z,p,q-p);
            strcat(z,y);
            p=q+len;
            }
        strcat(z,p);
        strcpy(s,z);
        }

static int varaa_earg()
        {
        earg=(double *)muste_malloc(MAXEARG*sizeof(double));
        if (earg==NULL)
            {
            sur_print("\nNot enough memory!");
            l_virhe=1;
            WAIT; return(-1);
            }
        earg_varattu=1;
        return(1);
        }

static int aseta_earg(double luku,char *sana)
        {
        char sana2[5];

        sana[0]=EARG;
        if (n_earg>=MAXEARG)
            {
            sur_print("\nStack overflow in editorial functions!");
            WAIT; l_virhe=1;
            return(-1);
            }
        sana[1]=EOS; strcat(sana,muste_itoa(n_earg,sana2,10));
        earg[n_earg++]=luku;
        return(n_earg-1);
        }

/* varifor#.c 31.12.1986/SM  (11.1.1987) (10.12.1995)
        kuten arifor.c
   aritmetiikka for(i=a)to(n)sum(t)
                for(i=a)to(n)term(t=b)sum(f(term,i))
                             sum product max min maxi mini term
*/

static int arifor_mvarit(char *lauseke,double *y)
        {
        int i,g;
        char *sana[4],*laus[4];
        char x[LLENGTH];
        long ialku,iloppu,il,imax;
        double d;
        char *p;
        char sterm[LLENGTH];
        double term,sum;
        int iterm,iind,tind;
        char esana[7];
        int max;

        strcpy(x,lauseke);
        g=parsplit(x,sana,laus,4);
        if (g<0) return(-1);
        if (g<3) { if_syntax_error(lauseke); return(-1); }
/*
   for (i=0; i<g; ++i) printf("for: %d %s %s\n",i,sana[i],laus[i]); getch();
*/
        p=strchr(laus[0],'=');
        if (p==NULL) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;   /* laus[0]='i'  */
        laske_mvarit(p+1,&d); ialku=d;
        laske_mvarit(laus[1],&d); iloppu=d;
        iterm=0;
        if (strcmp(sana[2],"term")==0)
            {
            iterm=1;
            p=strchr(laus[2],'=');
            if (p==NULL) term=0.0;
            else { *p=EOS; laske_mvarit(p+1,&term); }  /* laus[2]='term' */
            }
        strcpy(sterm,laus[2+iterm]);
        iind=aseta_earg((double)ialku,esana);
        if (iind<0) return(-1);
        korvaa(sterm,laus[0],esana);
        if (iterm)
            {
            tind=aseta_earg(term,esana);
            if (iind<0) return(-1);
            korvaa(sterm,laus[2],esana);
            }

        max=0; p=sana[2+iterm];
        if (strncmp(p,"max",3)==0 || strncmp(p,"min",3)==0)
            {
            if (p[2]=='n') { max=3; sum=1e300; if (p[3]=='i') max=4; }
            else           { max=1; sum=-1e300; if (p[3]=='i') max=2; }
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm) { earg[tind]=term; if (il>ialku) laske_mvarit(sterm,&term); }
                else laske_mvarit(sterm,&term);
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
                if (iterm) { earg[tind]=term; if (il>ialku) laske_mvarit(sterm,&term); }
                else laske_mvarit(sterm,&term);
                sum+=term;
                }
            }
        else if (strcmp(p,"product")==0)
            {
            sum=1.0;
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm) { earg[tind]=term; if (il>ialku) laske_mvarit(sterm,&term); }
                else laske_mvarit(sterm,&term);
                sum*=term;
                }
            }
        else if (strcmp(p,"term")==0)
            {
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm) { earg[tind]=term; if (il>ialku) laske_mvarit(sterm,&term); }
                else laske_mvarit(sterm,&term);
                }
            sum=term;
            }
        else { if_syntax_error(lauseke); return(-1); }

        if (max==2) *y=(double)imax; else *y=sum;
        n_earg-=1+iterm;
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
                if (*p==')') { if_syntax_error(y); return(-1); }
                ++p;
                }
            if (*p==EOS) { if_syntax_error(y); return(-1); }
            *p=EOS;
            b[i]=++p;
            sulut=1;
            while (*p)
                {
                if (*p==')') { --sulut; if (!sulut) break; }
                else if (*p=='(') ++sulut;
                ++p;
                }
            if (sulut) { if_syntax_error(y); return(-1); }
            *p=EOS;
            ++p; if (*p==EOS) break;
            ++i;
            if (i>=max) { if_syntax_error(y); return(-1); }
            }
        return(i+1);
        }


static int arit_atoi_mvarit(char *s)
        {
        double a;

        laske_mvarit(s,&a);
        return((int)a);
        }


/****************************************************************
MAT #TRANSFORM P BY RECURSION <function(m,n)>
MIN=min1,min2 START=start1,start2
*****************************************************************/

static void rec_transf()
        {
        int i,j,len;
// RS REM        int min1,min2;
        int start1,start2;
        char x[LLENGTH],*s[2];
        double a; // ,xx[2];
        char f[LNAME];
        int n;
        

        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return; }

        i=spec_init(r1+r-1); if (i<0) return;
        sp_read=1;

        strcpy(rec_func,word[5]);

        strcpy(f,rec_func);
        len=strlen(f); f[len++]='(';
        i=0;
        while (i<spn &&
               (spa[i][strlen(spa[i])-1]!=':' || strncmp(f,spa[i],len)!=0)) ++i;

        if (i==spn)
            {
            sprintf(sbuf,"\nRecurrence relation for %s not defined as a temporary function!",
                             rec_func);
            sur_print(sbuf); WAIT; return; // RS CHA exit(0);
            }
        strcpy(x,spa[i]); n=split(x,s,3);
        if (n>2 || n==0)
            {
            sur_print("\nOnly recurrence relations with 1 or 2 argumants permitted!");
            WAIT; return; // RS CHA exit(0);
            }

        start1=1; start2=1; if (n==1) start2=0;
        i=spfind_mat("START");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,s,2);
            if (i>0) start1=arit_atoi_mvarit(s[0]);
            if (i>1) start2=arit_atoi_mvarit(s[1]);
            }

        for (i=start1; i<mX; ++i)
            for (j=start2; j<nX; ++j)
                X[i+mX*j]=MISSING8;

        for (i=start1; i<mX; ++i)
            for (j=start2; j<nX; ++j)
                {
                if (n==2)
                    sprintf(sbuf,"%s(%d,%d)",rec_func,i,j);
                else
                    sprintf(sbuf,"%s(%d)",rec_func,i);
                laske_mvarit(sbuf,&a);
                X[i+mX*j]=a;
                }
        i=matrix_save(word[2],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        }

static void rand_transf()
        {
        int i,j;
        double x;
        extern double sur_rand0();

        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return; }

/*      i=spec_init(r1+r-1); if (i<0) return;
        sp_read=1;
*/
               /* #RAND(1998) */
        x=atof(word[4]+6);

        for (i=0; i<mX; ++i) for (j=0; j<nX; ++j)
            X[i+mX*j]=sur_rand0(x,1);

        strcpy(exprX,word[4]); /* 28.6.1999 */

        i=matrix_save(word[2],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        }


//  MAT #TRANSFORM A BY #DISTR(P,2006)
//      1          2
static void distr_transf()
        {
        int i,j,h;
        double x,u;
        char *s[2]; // ,y[LLENGTH];
        extern double sur_rand0();

        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return; }


// printf("\nword[4]=%s|",word[4]); getch();
        i=split(word[4]+7,s,2);
// printf("\ns: %s %s|",s[0],s[1]); getch();

        i=load_Y(s[0]); if (i<0) { mat_not_found(s[0]); return; }

        x=0;
        for (i=0; i<mY; ++i) x+=Y[mY+i];
        for (i=0; i<mY; ++i) Y[mY+i]/=x;
        x=0;
        for (i=0; i<mY; ++i) { x+=Y[mY+i]; Y[mY+i]=x; }
/**************
printf("\n");
for (i=0; i<mY; ++i) printf("%g ",Y[mY+i]);
getch();
******************/
        x=atof(s[1]);

        for (i=0; i<mX; ++i) for (j=0; j<nX; ++j)
            {
            u=sur_rand0(x,1);
            for (h=0; h<mY; ++h) if (u<Y[mY+h]) break;
            X[i+mX*j]=Y[h];
            }
//      strcpy(exprX,word[4]); /* 28.6.1999 */
        sprintf(exprX,"#DISTR(%s)",s[0]);

        i=matrix_save(word[2],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        }



static void op__transform()
        {
        int i,j,k;
        char x[3],y[3];
        char ii[3],jj[3];
        unsigned int h;
        char tnimi[LLENGTH];
// RS REM        double a;
        int spn0;

        if (muste_strcmpi(word[3],"BY")!=0) // 24.10.2007
            {
            sprintf(sbuf,"\nBY missing after MAT #TRANSFORM %s",word[2]);
            sur_print(sbuf);
            WAIT; return;
            }

        i=external_mat_init(2); if (i<0) return;
        if (g==6 && muste_strnicmp(word[4],"RECUR",5)==0)
            { rec_transf(); return; }
        if (g>4 && muste_strnicmp(word[4],"#RAND",5)==0)
            { rand_transf(); return; }
        if (muste_strnicmp(word[4],"#DISTR",6)==0)
            { distr_transf(); return; }



        if (g!=5 && g!=7)
            {
            init_remarks();
            rem_pr("MAT #TRANSFORM X BY <expression(X#,I#,J#)>");
            rem_pr("or");
            rem_pr("MAT #TRANSFORM X BY Y AND <expression(X#,Y#,I#,J#)>");
            rem_pr("works as MAT TRANSFORM but allows a larger set of functions to be used.");
            rem_pr("In fact, all features of the VAR operation are available");
            rem_pr("except those related to data sets like lags and leads, etc.");
            rem_pr("Thus, for example, temporary functions defined in the edit field");
            rem_pr("library functions (on disk), and random deviates (rand function etc.)");
            rem_pr("are permitted.");
            rem_pr("Examples:");
            rem_pr("m=8                                    / Generating m x m matrix A");
            rem_pr("MAT A=ZER(m,m)                         / with all elements ");
            rem_pr("MAT #TRANSFORM A BY probit(rand(1995)) / independently distributed as N(0,1) ");
            rem_pr("");
            rem_pr("MAT C=ZER(m,m)                         / Generating m x m matrix C");
            rem_pr("MAT #TRANSFORM C BY C(I#,J#)           / of binomial coefficients");
            rem_pr("");

            wait_remarks(1);

            rem_pr("MAT #TRANSFORM C BY RECURRENCE N");
            rem_pr("transforms matrix C by a recurrence relation N of two integer variables.");
          rem_pr("N must be defined like a temporary function in editorial computing in the form");
            rem_pr("N(m,n):=function(m,n,N(i1,j1),N(i2,j2),...) where i1,i2,...<m, j1,j2,...<=n .");
            rem_pr("Before using this MAT #TRANSFORM operation the initial conditions");
            rem_pr("must be given by filling certain first rows/columns/elements with");
            rem_pr("suitable values. The starting position of iteration is supplied by");
            rem_pr("a START=i0,j0 specification where i0,j0 are row and column indices.");
            rem_pr("Rows and columns are implicitly labelled by 0,1,2,... (i.e. starting");
            rem_pr("from 0 instead of 1). Operations");
            rem_pr("MAT RLABELS NUM(0) TO C");
            rem_pr("MAT CLABELS NUM(0) TO C");
            rem_pr("are available for such labelling.");
            rem_pr("");
            rem_pr("If C is a column vector, also functions of one integer variable are allowed.");

            wait_remarks(1);

            rem_pr(".........................................................................");
            rem_pr("Example: Stirling numbers of the second kind");
            rem_pr("S2(n,k):=S2(n-1,k-1)+k*S2(n-1,k)  Initial condition S2(n,1)=1");
            rem_pr("");
            rem_pr("n=10 k=n             / Numbers for n=1,..,10 k=1,...,10 to be computed");
            rem_pr("MAT A=ZER(n+1,k+1)   / Matrix A initialized by 0's");
            rem_pr("MAT C=CON(n,1)");
            rem_pr("MAT A(2,2)=C         / Column 2 (corresponding to k=1) filled with 1's");
            rem_pr("MAT RLABELS NUM(0) TO A");
            rem_pr("MAT CLABELS NUM(0) TO A");
            rem_pr("/MATSHOW A,12        / See matrix A in initial state");
            rem_pr("");
            rem_pr("START=2,2            / Starting point for recursion");
            rem_pr("MAT #TRANSFORM A BY RECURRENCE S2");
            rem_pr("/MATSHOW A,12345     / See the table of Stirling numbers");
            rem_pr(".........................................................................");

            wait_remarks(1);
            rem_pr("MAT #TRANSFORM X BY #RAND(seed_number)");
            rem_pr("works as                              ");
            rem_pr("MAT #TRANSFORM X BY RAND(seed_number)");
            rem_pr("but is about two times faster.");

            wait_remarks(2);
            return;
            }

        i=load_X(word[2]); if (i<0) { mat_not_found(word[2]); return; }

        if (g==7)
            {
            i=load_Y(word[4]);
            if (mX!=mY || nX!=nY) { dim_error(); return; }
            }
        i=spec_init(r1+r-1); if (i<0) return;
        sp_read=1;
        strcpy(x,"X#"); strcpy(y,"Y#");
        strcpy(ii,"I#"); strcpy(jj,"J#");
        spn0=spn;
        spa[spn]=x; spb[spn]=NULL; ++spn;
        spa[spn]=ii; spb[spn]=NULL; ++spn;
        spa[spn]=jj; spb[spn]=NULL; ++spn;

        if (g==7) { spa[spn]=y; spb[spn]=NULL; ++spn; }
/* jotta ulkoisia X#,I#,J#,Y#-tÑsmennyksiÑ voi kÑyttÑÑ */
        spb2=(char **)muste_malloc((spn0+1)*sizeof(char *));
        if (spb2==NULL) { not_enough_mem_for_spec(); return; }
        for (i=0; i<spn0; ++i) spb2[i]=spb[i];

        for (i=0; i<mX; ++i) for (j=0; j<nX; ++j)
            {
            for (k=0; k<spn0; ++k) spb[k]=spb2[k];
            h=i+mX*j;
            arvo[spn-2]=(double)(i+1); arvo[spn-1]=(double)(j+1);
            if (g==5) arvo[spn-3]=X[h];
            else { arvo[spn-4]=X[h]; arvo[spn-1]=Y[h]; }
            laske_mvarit(word[g-1],&X[h]);
            }
        spn-=3; if (g==7) --spn;
        strcpy(tnimi,"T("); strcat(tnimi,word[2]); strcat(tnimi,"_by_");
        strcat(tnimi,word[4]);
        if (g==7) { strcat(tnimi,"_and_"); strcat(tnimi,word[6]); }
        strcat(tnimi,")");
        nim(tnimi,exprX);
        i=matrix_save(word[2],X,mX,nX,rlabX,clabX,lrX,lcX,-1,exprX,0,0);
        outseed();
        mat_comment(word[2],exprX,i,mX,nX,NULL);
        external_mat_end(argv1);
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
        char *p,*q; // ,*q2;
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


       if (strstr(info,"#TRANSFORM") != NULL) // 18.6.2007
           { op__transform(); m_end(); return; }

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

       if (muste_strcmpi(osa[1],"#MULT")==0) { op__mult(); m_end(); return; }      
  else if (muste_strcmpi(osa[1],"#SAMPLES")==0) { op__samples(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#INDVAR")==0) { op__indvar(); m_end(); return; } 
  else if (muste_strcmpi(osa[1],"#MERGE")==0) { op__merge(); m_end(); return; }
  else if (muste_strcmpi(osa[1],"#MINDIFF")==0) { op__mindiff(); m_end(); return;}
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
// RS REM        char *parm[10];
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

        else if (*ops=='#') { external_op(); return(1); /* RS CHA exit(0); */ } // 15.3.2003


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
            return(1); // RS CHA exit(0);
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

		X=NULL; rlabX=NULL; clabX=NULL; // 14.10.2011/SM

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

		X=NULL; rlabX=NULL; clabX=NULL; // 14.10.2011/SM

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
                i=spfind_mat(par);
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
        char ch1,ch2; // ,ch;
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
        int i,min,imin=0;

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
        int i,min,imin=0,n_laji,k=0;
// RS REM        char y[LLENGTH];
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
            if (expr_size>=mat_parser) { err_mat_parser(); return(-1); } // RS CHA exit 
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
        if (expr_size>=mat_parser) { err_mat_parser(); return(-1); } // RS CHA exit
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
// RS REM        int current_power;

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
        if (i<0) return(-1); // RS ADD
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
            expr_space=muste_malloc(mat_parser);
            if (expr_space==NULL) { not_enough_space(); return(-1); }
            level=(int *)muste_malloc((mat_parser+1)*sizeof(int));
            if (level==NULL) { not_enough_space(); return(-1); }
            power=(int *)muste_malloc((mat_parser+1)*sizeof(int));
            if (power==NULL) { not_enough_space(); return(-1); }
            scalar=(int *)muste_malloc((mat_parser+1)*sizeof(int));
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
                sur_print(sbuf); WAIT; return(-1); // RS CHA exit
                }
            fprintf(survomat,"MAT expression: %s=%s\n%s",q,p,expr_space);
            muste_fclose(survomat);
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
        int i,jrivi=0,k=0;
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
n_row_comments=0;
len_row_comments=0L;
l_virhe=0;
nmat=0;
n_mat=0;
max_index=expr_size=n_scalars=0;
save_all_temp_mat=0;
typeA=typeB=0;
diagA=diagB=0;


level=NULL;
power=NULL;
scalar=NULL;
expr_space=NULL;
survomat=NULL;
arvo=NULL;
MAT=NULL;
rowcomments=NULL;
mtx_file=NULL;
X=NULL;
rlabX=NULL;
clabX=NULL;
Y=NULL;
rlabY=NULL;
clabY=NULL;
T=NULL;
rlabT=NULL;
clabT=NULL;
expr_space=NULL;
row_comments=NULL;
p_com=NULL;
pow1=NULL;
pow2=NULL;
d=NULL;
e=NULL;
xi=NULL;
argv1=NULL;
freq=NULL;
unit=NULL;
cos_t=NULL;
cos_u=NULL;
cos_v=NULL;
cos_u2=NULL;
cos_i=NULL;
cos_imax=NULL;
sel2=NULL;
sel=NULL;
sel_max=NULL;
sh_file=NULL;
earg=NULL;


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


