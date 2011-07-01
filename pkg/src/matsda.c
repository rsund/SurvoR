/*  matsda.c 8.4.1986/SM (9.2.1987)
*/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
#include "survo.h"
#include "survoext.h"
// #include "survodat.h"
#include "survolib.h"

/*
MAT SAVE DATA <data file> TO X
    1    2     3          4  5
*/

static SURVO_DATA d;
static int m,n;
static int eka;  /* 1=1.muuttuja riviots. 0=ei */
static int lrX=8;
static int lcX=8;
static double *X;
static char *rlabX, *clabX;
static char exprX[LLENGTH];
static int prind;

static char **specs;



static void matsda_puute()
        {
        sur_print("\nNot enough memory (MAT SAVE DATA)");
        WAIT;
        }

static int varaa_tilat()
        {
/*
        if (m*n>8192)
            {
            sur_print("\nMatrix cannot be saved! Too many elements!");
            sur_print("\nMax=8192 elements");
            WAIT; return(-1);
            }
*/
     X=(double *)malloc((unsigned int)((unsigned int)m*(unsigned int)n*sizeof(double)));
        if (X==NULL) { matsda_puute(); return(-1); }
        rlabX=malloc(lrX*m);
        if (rlabX==NULL) { matsda_puute(); return(-1); }
        clabX=malloc(lcX*n);
        if (clabX==NULL) { matsda_puute(); return(-1); }
        return(1);
        }

static void numlab(char *lab,int n,int len)
        {
        int h,i,j,k;
        char sana[6];
        int sar;

        for (i=0; i<n*len; ++i) lab[i]=' ';
        if (n<1000) sar=3; else sar=5;

        for (i=0; i<n; ++i)
            {
            muste_itoa(i+1,sana,10);
            h=strlen(sana);
            for (j=i*len+sar-h, k=0; k<h; ++k, ++j) lab[j]=sana[k];
            }
        }


static int sijoita()
        {
        long l;
        int i,j;
        char sana[LLENGTH];
        int len;
        char *p;
        double x;
        int miss;

        for (j=eka; j<d.m_act; ++j)
            {
            p=clabX+(j-eka)*lcX;
            strncpy(p,space,lcX);
            strncpy(p,d.varname[d.v[j]],8);
            }
        if (eka)
            {
            len=lrX;
            if (d.varlen!=NULL && len>d.varlen[d.v[0]]) len=d.varlen[d.v[0]];
            }
        else
            numlab(rlabX,m,lrX);
        i=0;
        sur_print("\nReading data file: ");
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            if (i>m-1) break; /* lopussa olevien puutt.hav.sivuutus */
            if (eka)
                {
                data_alpha_load(&d,l,d.v[0],sana);
                p=rlabX+i*lrX;
                strncpy(p,space,lrX);
                strncpy(p,sana,len);
                }
            miss=0;
            for (j=eka; j<d.m_act; ++j)
                {
                data_load(&d,l,d.v[j],&x);
                if (x==MISSING8) { miss=1; break; }
                X[i+m*(j-eka)]=x;
                }
            if (!miss)
                {
                ++i;
                if (prind)
                    {
                    sprintf(sbuf," %ld",l); sur_print(sbuf);
                    }
                }
            }
        return(1);
        }


static int laske_havainnot()
        {
        int i,j;
        long l;
        double x;
        int miss;

        i=0;
        sur_print("\nChecking data...");
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            miss=0;
            for (j=eka; j<d.m_act; ++j)
                {
                data_load(&d,l,d.v[j],&x);
                if (x==MISSING8) { miss=1; break; }
                }
            if (!miss)
                {
                ++i;
                if (prind)
                    {
                    sprintf(sbuf," %d",(int)l); sur_print(sbuf); // RS CHA ld -> d
                    }
                }
            }
        m=i;
        if (m==0)
            {
            sur_print("\nNo valid observations!");
            WAIT; return(-1);
            }

        return(1);
        }

void muste_matsda(int argc,char *argv[])
        {
        int i;
        int rlabels;

// RS ADD Variable init
m=n=0;
eka=0;
lrX=lcX=8;
prind=0;

        if (argc==1) return;
        s_init(argv[1]);

        if (g<6)
            {
            sur_print("\nUsage: MAT SAVE DATA <data_file> TO <matrix>");
            WAIT; return;
            }
        i=data_open(word[3],&d);
        if (i<0) return;
        i=sp_init(r1+r-1); if (i<0) return;
        mask(&d);
        if (d.m_act==0)
            {
            sur_print("\nNo active fields!");
            WAIT; return;
            }
        i=mask_sort(&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;

        prind=1;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        rlabels=0;
        i=spfind("RLABELS");
        if (i>=0) rlabels=1;

        i=laske_havainnot(); if (i<0) return;
        if (d.vartype[d.v[0]][0]=='S' && !rlabels) eka=1; else eka=0;
        n=d.m_act-eka;
        i=varaa_tilat(); if (i<0) return;
        sprintf(sbuf,"\n%s will be a matrix of %d rows and %d columns.",word[5],m,n);
        sur_print(sbuf);
        sijoita();

// 21.10.2009
        for(i=0; i<m*lrX; ++i) if (rlabX[i]==EOS) rlabX[i]=' ';
        for(i=0; i<n*lcX; ++i) if (clabX[i]==EOS) clabX[i]=' ';


        matrix_save(word[5],X,m,n,rlabX,clabX,lrX,lcX,-1,word[5],0,0);
        
        muste_fixme("\nFIXME: matsda.c free memory"); // RS FIXME
        }
