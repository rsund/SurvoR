/* !linco.c 26.10.1986/SM (3.8.1987) (4.2.1996)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
// #include <process.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;
static double *A;
static char *rlab,*clab;
static int rdim,cdim,lr,lc,type;
static char expr[LLENGTH];
static char matname[LLENGTH];
static int pros=0;

static int *var;
static double *y;
static int *outvar;
static int *lag;
static char act; // new fields activated by act

static int matparam();
static int varaa_tilat();
static int ei_tilaa();
static int find_variables();
static int poimi(char *nimi,char *lab,int l,int i);
static int constant(char *nimi);
static int prosparam(char *nimi);
static int linear_combinations();


// char **specs;
/***************
main(argc,argv)
int argc; char *argv[];
***************/

void muste_linco(char *argv)
        {
        int i;

  //    if (argc==1) return;
        s_init(argv);

        if (g<3)
            {
            sur_print("\nUsage: LINCO <SURVO_data>,<matrix_of_coefficients>");
            WAIT; return;
            }
        i=data_open2(word[1],&d,1,0,0); if (i<0) return;
                                /* tilaa uusille muuttujille */
        i=spec_init(r1+r-1); if (i<0) return;

        pros=0;

        act='A'; i=spfind("ACT");
        if (i>=0) act=*spb[i];

        i=conditions(&d); if (i<0) return;  /* permitted only once */
        i=matparam(); if (i<0) return;
        i=matrix_load(matname,&A,&rdim,&cdim,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return;

        i=varaa_tilat(); if (i<0) return;
        i=find_variables();
        if (i<0)
            {
            if (etu==2)
                {
                sprintf(tut_info,"___@%d@LINCO@Error in LINCO@",-i);
                s_end(argv[1]); return;
                }
            return;
            }
        linear_combinations();
        data_close(&d);
        s_end(argv);
        }

static int matparam()
        {
        char *p;

        strcpy(matname,word[2]);
        p=strchr(word[2],'(');
        if (p!=NULL)
            {
            pros=1;
            *p=EOS; strcpy(matname,word[2]);
            word[2]=p+1;
            p=strchr(word[g-1],')');
            if (p==NULL)
                {
                sur_print("\n) missing in LINCO operation!");
                WAIT; return(-1);
                }
            *p=EOS;
            }
        return(1);
        }

static int varaa_tilat()
        {
        var=(int *)malloc(rdim*sizeof(int));
        if (var==NULL) { ei_tilaa(); return(-1); }
        y=(double *)malloc(rdim*sizeof(double));
        if (y==NULL) { ei_tilaa(); return(-1); }
        outvar=(int *)malloc(cdim*sizeof(int));
        if (outvar==NULL) { ei_tilaa(); return(-1); }
        lag=(int *)malloc(rdim*sizeof(int));
        if (lag==NULL) { ei_tilaa(); return(-1); }
        return(1);
        }

static int ei_tilaa()
        {
        sur_print("\nNot enough space for matrices! (LINCO)");
        WAIT; return(1);
        }

static int find_variables()
        {
        int i,k;
        char nimi[LLENGTH];

        for (i=0; i<rdim; ++i)
            {
            poimi(nimi,rlab,lr,i);
            if (constant(nimi)) k=-1;
            else
                {
                k=prosparam(nimi); if (k<0) return(-1);
                k=varfind(&d,nimi); if (k<0) return(-2);
                }
            var[i]=k;
            }

        for (k=0; k<rdim; ++k) lag[k]=0;
        for (i=0; i<cdim; ++i)
            {
            poimi(nimi,clab,lc,i);
            if (muste_strcmpi(nimi,"#LAG")==0)
                {
                for (k=0; k<rdim; ++k) lag[k]=(int)A[i*rdim+k];
                k=-1;
                }
            else
                {
                k=prosparam(nimi); if (k<0) return(-1);
                if (k==0) { outvar[i]=-1; continue; }
                k=varfind2(&d,nimi,0);
                if (k<0)
                    {
                    k=create_newvar1(&d,nimi,'4',4,act);
                    if (k<0) return(-3);
                    }
                }
            outvar[i]=k;
            }
        return(1);
        }

static int poimi(char *nimi,char *lab,int l,int i)
        {
        int k=0;
        while (lab[i*l+k]!=' ' && k<l && k<8) { nimi[k]=lab[i*l+k]; ++k; }
        nimi[k]=EOS; return(1);
        }

static int constant(char *nimi)
        {
        char x[LLENGTH];
        char *p;

        strcpy(x,nimi); muste_strupr(x);
        if (strncmp(x,"CONST",5)==0) return(1);
        p=x;
        while (*p && *p==' ') ++p;
        if (*p=='-' || *p==EOS) return(1);
        return(0);
        }

static int prosparam(char *nimi)
        {
        int i;

        if (*nimi!='%') return(1);
        i=1+atoi(nimi+1);
        if (!pros || i<2 || i>g-1)
            return(0);
        strcpy(nimi,word[i]);
        return(1);
        }

static int linear_combinations()
        {
        int i,k,m;
        int j;
        double val;
        int miss;
        int keyind;
        int l;
        int prind_count;
        extern double sis_tulo();

        sur_print("\nComputing and saving linear combinations: ");

        keyind=1;
        i=hae_apu("prind",sbuf); if (i) keyind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) keyind=atoi(spb[i]);


        for (i=0; i<cdim; ++i)
            if (outvar[i]>=0) { sprintf(sbuf," %.*s",8,d.varname[outvar[i]]); sur_print(sbuf); }
        sur_print("\n");

        for (i=0; i<cdim; ++i) if (outvar[i]>=0) break;
        if (i==cdim)
            {
            sprintf(sbuf,"\nParameters missing in %s!",word[2]);
            sur_print(sbuf); WAIT; return(-1);
            }

        prind_count=0;
        for (j=d.l1; j<=d.l2; ++j)
            {
            ++prind_count;
            if (prind_count==100)
                {
                prind_count=0;
                if (sur_kbhit()) { sur_getch(); keyind=1-keyind; }
                }
            if (unsuitable(&d,j)) continue;
            if (keyind) { sprintf(sbuf," %d",j); sur_print(sbuf); }
            for (k=0; k<rdim; ++k)
                {
                m=var[k];
                if (m==-1) y[k]=1.0;
                else
                    {
                    l=j+lag[k];
                    if (l>=1L && l<=d.n)
                        data_load(&d,l,m,&y[k]);
                    else
                        y[k]=MISSING8;
                    }
                }
            for (i=0; i<cdim; ++i)
                {
                if (outvar[i]==-1) continue;
                miss=0;

                for (k=0; k<rdim; ++k) if (y[k]==MISSING8) break;
                if (k<rdim) val=MISSING8;
                else val=sis_tulo(A+i*rdim,y,1,1,rdim);
                k=data_save(&d,j,outvar[i],val); if (k<0) return(-1);
                }
            }
        return(1);
        }

