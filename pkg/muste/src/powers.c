#include "muste.h"
/* !powers.c 3.3.2004/SM (3.3.2004)
      muunnelma !corr.c
*/


#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAX_VAR 10
#define MAX_POW 20
#define MAX_TERMS 5000

static SURVO_DATA d;
static char aineisto[LNAME];
static int degree=2;
static char var_type='8';
static int nvar;
static int ncomb;
static char var_name[MAX_VAR][9];
static char var_name2[MAX_VAR][9];
static int var_ind[MAX_VAR];
static char pow_name[MAX_TERMS][9];
static int pow_v[MAX_TERMS][MAX_VAR];
static int pow_ind[MAX_TERMS];
static double val[MAX_VAR][MAX_POW+1];

static int list_of_vars();
static int name_too_long(char *s);
static int power_combinations();
static int next_m_distr(int n,int m,int *elem1);

// char **specs;

// void main(argc,argv)
// int argc; char *argv[];

void muste_powers(char *argv)
        {
        int i,k;
        int j;
        double a,b;

        degree=2;
        var_type='8';

        s_init(argv);
        if (g<2)
            {
            sur_print("\nUsage: POWERS <SURVO_data>");
            sur_print("\n       POW_VARS=<list_of_variables>");
            sur_print("\n       DEGREE=<max_power> TYPE=<1|2|4|8>");
            WAIT; return;
            }

        strcpy(aineisto,word[1]);
        i=data_open3(aineisto,&d,1,1,1,1);
        if (i<0) { s_end(argv[1]); return; }
        i=spec_init(r1+r-1); if (i<0) return;
        i=conditions(&d); if (i<0) { s_end(argv[1]); return; }

        i=list_of_vars(); if (i<0) return;
        i=spfind("DEGREE");
        if (i>=0) degree=atoi(spb[i]);
        if (degree>MAX_POW)
            {
            sprintf(sbuf,"\nMax. degree is %d.",MAX_POW);
            sur_print(sbuf); WAIT; return;
            }

        i=spfind("TYPE");
        if (i>=0) var_type=*spb[i];

        i=power_combinations(); if (i<0) return;

        for (j=1L; j<=d.n; ++j)
            {
            for (i=0; i<nvar; ++i)
                {
                if (unsuitable(&d,j)) continue;
                data_load(&d,j,var_ind[i],&a);
// Rprintf("\nvar=%d a=%g|",vara); getch();
                val[i][1]=a; b=a;
                for (k=2; k<=degree; ++k)
                    {
                    b*=a;
                    val[i][k]=b;
// Rprintf("\nval: %d %d %g %g|",i,k,a,val[i][k]); getch();
                    }
                }

            for (k=0; k<ncomb; ++k)
                {
                a=1.0;
                for (i=0; i<nvar; ++i)
                    if (pow_v[k][i]>0) a*=val[i][pow_v[k][i]];
// Rprintf("\na=%g|",a); getch();
                data_save(&d,j,pow_ind[k],a);
                }
            }
        data_close(&d); // 8.8.2011/SM
        s_end(argv);
        }

static int list_of_vars()
    {
    int i,k;
    char *s[MAX_VAR];
    char x[LLENGTH];
    char *p;
    char nimi1[LLENGTH],nimi2[LLENGTH];

    i=spfind("POW_VARS"); if (i<0) return(-1);
    strcpy(sbuf,spb[i]);
    nvar=split(sbuf,s,MAX_VAR);
    for (i=0; i<nvar; ++i)
        {
        strcpy(x,s[i]);
        p=strchr(x,'(');
        if (p==NULL)
            {
            strcpy(nimi1,x); strcpy(nimi2,x);
            }
        else
            {
            *p=EOS;
            strcpy(nimi1,x); strcpy(nimi2,p+1);
            k=strlen(nimi2)-1; if (nimi2[k]==')') nimi2[k]=EOS;
            }
        if (strlen(nimi1)>8) { name_too_long(nimi1); return(-1); }
        if (strlen(nimi2)>8) { name_too_long(nimi2); return(-1); }
        strcpy(var_name[i],nimi1);
        strcpy(var_name2[i],nimi2);

        k=varfind(&d,var_name[i]);
        if (k<0) return(-1);
        var_ind[i]=k;
        }
/******************************
for (i=0; i<nvar; ++i)
    {
    Rprintf("\n%d %s %s %d",i,var_name[i],var_name2[i],var_ind[i]);
    }
getch();
*******************************/
    return(1);
    }

static int name_too_long(char *s)
    {
    sprintf(sbuf,"\nName %s too long!",s);
    sur_print(sbuf);
    WAIT;
    return(1);
    }

static int power_combinations()
    {
    int i,k,m,n,h;
    int elem1[MAX_VAR];
    char nimi[LLENGTH];
    char nimi2[LLENGTH];

    ncomb=0;
    m=nvar;
    for (n=2; n<=degree; ++n)
        {
        for (i=0; i<m-1; ++i) elem1[i]=0; elem1[m-1]=n;
        while (1)
            {
            for (i=0; i<m; ++i) pow_v[ncomb][m-1-i]=elem1[i];
            ++ncomb;
            if (ncomb>MAX_TERMS)
                {
                sprintf(sbuf,"\nMore than %d terms!",MAX_TERMS);
                sur_print(sbuf); WAIT; return(-1);
                }
            i=next_m_distr(n,m,elem1);
            if (i<0) break;
            }
        }

    for (k=0; k<ncomb; ++k)
        {
        *nimi=EOS; *nimi2=EOS;
        for (i=0; i<nvar; ++i)
            {
            h=pow_v[k][i];
            if (h>0)
                {
                sprintf(sbuf,"%s%d",var_name2[i],h);
                strcat(nimi,sbuf);
                if (h==1) sprintf(sbuf,"%s",var_name2[i]);
                else sprintf(sbuf,"%s^%d",var_name2[i],h);
                strcat(nimi2,sbuf); strcat(nimi2,"*");
                }
            }
        nimi2[strlen(nimi2)-1]=EOS;  // * pois!

        if (strlen(nimi)>8)
            {
            strcpy(nimi,"X");
            for (i=0; i<nvar; ++i)
                {
                if (pow_v[k][i]>9) { strcpy(nimi,"-"); break; }
                sprintf(sbuf,"%d",pow_v[k][i]);
                strcat(nimi,sbuf);
                }
            if (*nimi=='-' || strlen(nimi)>8)
                sprintf(nimi,"XXX%d",k+1);
            }

// Ylipitkät nimet A1B2C3D4 -> X1234 tai XXX<comb+1>

        strcpy(pow_name[k],nimi);

        pow_ind[k]=varfind2(&d,pow_name[k],0);
        if (pow_ind[k]<0)
            pow_ind[k]=create_newvar(&d,pow_name[k],var_type,0);
// Rprintf("\nnro=%d|",pow_ind[k]); getch();
        if (pow_ind[k]<0) return(-1);

        update_varname(&d,pow_ind[k],nimi2);

/*************************************
        Rprintf("\n");
        for (i=0; i<nvar; ++i)
            Rprintf("%d ",pow_v[k][i]);
        Rprintf("%s|",pow_name[k]);
**************************************/
        }
//  getch();

    return(1);
    }

static int next_m_distr(int n,int m,int *elem1)
        {
        int i,k;

        i=m-1;
        while (elem1[i]==0) --i;
        if (i==0) return(-1);
        ++elem1[i-1];
        elem1[m-1]=elem1[i]-1;
        for (k=i; k<m-1; ++k) elem1[k]=0;
        return(1);
        }

