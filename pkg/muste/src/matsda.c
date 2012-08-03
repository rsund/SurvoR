#include "muste.h"
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
// RS REM static char exprX[LLENGTH];
static int prind;

// RS REM static char **specs;

static double *A;
static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[LLENGTH];

static int *v;
static int match;
static int first,last;
static int uusi;
static char numtype;
static int prind;

extern int survo_ferror;


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
     X=(double *)muste_malloc((unsigned int)((unsigned int)m*(unsigned int)n*sizeof(double)));
        if (X==NULL) { matsda_puute(); return(-1); }
        rlabX=muste_malloc(lrX*m);
        if (rlabX==NULL) { matsda_puute(); return(-1); }
        clabX=muste_malloc(lcX*n);
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
        int len=0;
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
// d=NULL;
X=NULL;
rlabX=NULL;
clabX=NULL;
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
        i=sp_init(r1+r-1); if (i<0) { data_close(&d); return; } // RS ADD data_close
        mask(&d);
        if (d.m_act==0)
            {
            sur_print("\nNo active fields!");
            WAIT; return;
            }
        i=mask_sort(&d); if (i<0) { data_close(&d); return; } // RS ADD data_close
        i=conditions(&d); if (i<0) { data_close(&d); return; } // RS ADD data_close

        prind=1;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        rlabels=0;
        i=spfind("RLABELS");
        if (i>=0) rlabels=1;

        i=laske_havainnot(); if (i<0) { data_close(&d); return; } // RS ADD data_close
        if (d.vartype[d.v[0]][0]=='S' && !rlabels) eka=1; else eka=0;
        n=d.m_act-eka;
        i=varaa_tilat(); if (i<0) { data_close(&d); return; } // RS ADD data_close
        sprintf(sbuf,"\n%s will be a matrix of %d rows and %d columns.",word[5],m,n);
        sur_print(sbuf);
        sijoita();

// 21.10.2009
        for(i=0; i<m*lrX; ++i) if (rlabX[i]==EOS) rlabX[i]=' ';
        for(i=0; i<n*lcX; ++i) if (clabX[i]==EOS) clabX[i]=' ';


        matrix_save(word[5],X,m,n,rlabX,clabX,lrX,lcX,-1,word[5],0,0);
        
        data_close(&d);
//        muste_fixme("\nFIXME: matsda.c free memory"); // RS FIXME
        }



/* smat.c 14.2.1987/SM (6.3.1988) (2.10.1996)

*/

static int not_enough_memory()
        {
        sur_print("\nNot enough memory (FILE SAVE MAT)!"); WAIT;
        return(1);
        }

static int varaa_tilat_fsm()
        {
        v=(int *)muste_malloc((n+2)*sizeof(int)); // RS +2
        if (v==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int tutki_muuttujat()
        {
        int i,h;
        char sana[9];
        char *p;

        if (uusi) { for (i=0; i<n; ++i) v[i]=i+1; return(1); }

        for (i=0; i<n; ++i)
            {

            for (h=0; h<8; ++h) sana[h]=clab[i*lc+h];
            sana[8]=EOS;
            p=sana; while (*p==' ') ++p;
            h=7; while (sana[h]==' ') sana[h--]=EOS;
            h=varfind2(&d,p,0);
            if (h<0)
                {
                h=create_newvar(&d,p,numtype,1);
                if (h<0) return(-1);
                }
            v[i]=h;
            }
        return(1);
        }



static int talletus1()    /* match=-2  */
        {
        int i,h,k;
        int label=0,len=0;
        long j;
        char sana[9]; char *p,*q;
        char x[LLENGTH];

        i=spfind("LABEL");
        if (i<0)
            {
            if (d.vartype[0][0]=='S') label=0; else label=-1;
            }
        else
            {
            i=varfind(&d,spb[i]);
            if (i<0) return(-1);
            if (d.vartype[i][0]!='S')
                {
                sur_print("\nLABEL must be a string field!");
                WAIT; return(-1);
                }
            }

        if (label>=0)
            {
            len=d.d2.varlen[label];
            }
        j=d.n;
        for (k=first-1; k<last; ++k)
            {
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",k+1); sur_print(sbuf); }
            ++j;
            d.n=j;
            fi_miss_obs(&(d.d2),j);
            if (label>=0)
                {
                for (h=0; h<8; ++h) sana[h]=rlab[k*lc+h];
                sana[8]=EOS;
                p=sana; while (*p==' ') ++p;
                strncpy(x,space,len); q=x;
                while (*p) *q++=*p++;
                fi_alpha_save(&d.d2,j,label,x);
                }
            for (i=0; i<n; ++i)
                data_save(&d,j,v[i],A[k+m*i]);
            if (survo_ferror) { sur_print("\nCannot save data!"); WAIT; return(-1); }

            }
        fi_rewind(&d.d2);
        fi_puts(&d.d2,&j,4,22L);
        return(1);
        }

static int talletus2()
        {
        long j;
        int i,k,h;
        char jakso[LLENGTH];
        char sana[9], *p;


        j=d.l1;
        for (k=first-1; k<last; ++k)
            {
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",k+1); sur_print(sbuf); }
            if (match>=0)
                {
                for (h=0; h<lr; ++h) sana[h]=rlab[k*lr+h]; sana[lr]=EOS;
                p=sana; while (*p==' ') ++p;
                h=lr-1; while(sana[h]==' ' && h>=0) sana[h--]=EOS;

                while (1)
                    {
                    data_alpha_load(&d,j,match,jakso);
                    jakso[lr]=EOS;
                    h=lr-1; while(jakso[h]==' ' && h>=0) jakso[h--]=EOS;
                    if (strcmp(sana,jakso)==0) break;
                    ++j;
                    if (j>d.l2)
                        {
                        sprintf(sbuf,"\nCase %s not found in data file %s!",
                                        sana,word[5]); sur_print(sbuf);
                        WAIT; return(-1);
                        }
                    }
                }
            for (i=0; i<n; ++i)
                data_save(&d,j,v[i],A[k+m*i]);
            if (survo_ferror) { sur_print("\nCannot save data!"); WAIT; 
                                return(-1); } // RS CHA exit -> return
            ++j; if (j>d.l2) return(1);
            }
        return(1);
        }

static int luo_uusi()
        {
        int i,h;
        int filen,fim1,fim,fil,fiextra,fitextn,fitextlen;
        char **fitext;
        char *privi[1];
        char jakso[LLENGTH];

static char *vartype, **pvartype;
static int *varlen;
static char **varname, *vartila;

        sprintf(sbuf,"\nSince SURVO 84C data file %s does not exist,",word[5]);
        sur_print(sbuf);
        sur_print("\ncreating a new one...");

        fim=n+1;
        vartype=muste_malloc(fim*9);
        if (vartype==NULL) { not_enough_memory(); return(-1); }
        pvartype=(char **)muste_malloc(fim*sizeof(char *));
        if (pvartype==NULL) { not_enough_memory(); return(-1); }
        varlen=(int *)muste_malloc(fim*sizeof(int));
        if (varlen==NULL) { not_enough_memory(); return(-1); }
        varname=(char **)muste_malloc(fim*sizeof(char *));
        if (varname==NULL) { not_enough_memory(); return(-1); }
        vartila=muste_malloc(fim*9);
        if (vartila==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<fim; ++i)
            {
            strncpy(vartype+i*9,space,8); vartype[i*9+8]=EOS;
            vartype[i*9+1]='A';
            vartype[i*9]=numtype;
            varlen[i]=(int)(numtype-'0');
            }
        vartype[0]='S';  varlen[0]=8; vartype[3]='-';
        for (i=0; i<fim; ++i) pvartype[i]=vartype+i*9;

        for (i=0; i<fim; ++i) varname[i]=vartila+i*9;
        strcpy(varname[0],"CASE");
        for (i=1; i<fim; ++i)
            {
            for (h=0; h<8; ++h) jakso[h]=clab[(i-1)*lc+h];
            h=0; while (jakso[h]==' ' && h<8) ++h;
            jakso[8]=EOS;
            strcpy(varname[i],jakso+h);
            }
        filen=0;
        for (i=0; i<fim; ++i) filen+=varlen[i];
        filen+=filen/4+20;
        fim1=fim+fim/4+4;
        fil=64;

        i=spfind("NAMELENGTH");
        if (i>=0)
            {
            fil=atoi(spb[i]);
            }
        fiextra=12;
        fitextn=1;
        fitextlen=c2;
        strcpy(jakso," Copied from matrix file "); strcat(jakso,word[3]); privi[0]=jakso;
        fitext=privi;
        i=fi_create(word[5],filen,fim1,fim,0L,fil,fiextra,fitextn,fitextlen,
                    fitext,varname,varlen,pvartype);
        if (i<0) return(-1);

//        muste_free(vartype); muste_free(pvartype); muste_free(varlen); muste_free(varname); muste_free(vartila);
//        vartype=NULL; pvartype=NULL; varlen=NULL; varname=NULL; vartila=NULL; // RS ADD
        data_open2(word[5],&d,1,0,0);
        return(1);
        }


void muste_file_save_mat(int argc,char *argv[])
        {
        int i;
        char x[LLENGTH];

// RS ADD Variables init
A=NULL;
rlab=NULL;
clab=NULL;
lr=lc=0;
type=0;
// char expr[LLENGTH];
v=NULL;
match=0;
first=last=0;
uusi=0;
numtype=0;
prind=0;		
		
		
        s_init(argv[1]);
        if (g<6)
            {
            sur_print("\nUsage: FILE SAVE MAT <MAT_file> TO <data_file>");
            WAIT; return;
            }
        i=sp_init(r1+r-1); if (i<0) return;
        i=spfind("TYPE");
        if (i<0) numtype='4';
        else
            {
            numtype=*spb[i];
            if (strchr("1248",numtype)==NULL) numtype='4'; /* 2.10.1996 */
            }
        i=matrix_load(word[3],&A,&m,&n,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return; // RS ADD 17.7.2012

// RS REM        i=fi_find(word[5],&d.d2,x);
		int uusi=FALSE; // RS ADD 17.7.2012
		if (strcmp(word[5],"NEW")==0) // RS ADD 17.7.2012
			{
			uusi=TRUE;
			word[5]=word[6];
			}
        if (uusi || !sur_find_svo_file(word[5],x)) // RS CHA i<0 
            {
            i=luo_uusi();
            if (i<0)
                {
                sur_print("\nShorter names for the fields can be selected by");
                sur_print("\nNAMELENGTH=8, for example.");
                WAIT;
                return;
                }
            uusi=1;
            }
        else
            {
// RS REM            muste_fclose(d.d2.survo_data);
            i=data_open2(word[5],&d,1,0,0); if (i<0) return;
            uusi=0;
            }

        if (d.type!=2)
            {
            sprintf(sbuf,"\n%s must be a Survo data file!",word[5]);
            sur_print(sbuf); WAIT; return;
            }
        i=varaa_tilat_fsm(); if (i<0) { data_close(&d); return; } // RS ADD data_close(&d)
        i=tutki_muuttujat(); if (i<0) { data_close(&d); return; } // RS ADD data_close(&d)

        i=spfind("FIRST"); if (i<0) first=1; else first=atoi(spb[i]);
        if (first<1 || first>m) first=1;
        i=spfind("LAST"); if (i<0) last=m; else last=atoi(spb[i]);
        if (last<first || last>m) last=m;

        prind=0;
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        i=spfind("MATCH");
        if (i<0) match=-2;
        else
            {
            if (*spb[i]=='#') match=-1;
            else
                {
                match=varfind(&d,spb[i]);
                if (match<0) { data_close(&d); return; } // RS ADD data_close(&d)
                }
            }
        sprintf(sbuf,"\nSaving matrix %s to data file %s:",word[3],word[5]);
        sur_print(sbuf);
        if (match==-2 || uusi) talletus1();
        else talletus2();

        data_close(&d);
        }
