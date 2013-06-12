#include "muste.h"
/* aggre2.c 14.3.1987/SM (11.9.1991)
   FILE AGGRE2
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define AGGSTEP 80000L
#define AGGSTEP2 1024L

static SURVO_DATA d,d2;
static int aggvar;
static char aggtype;
static int aggperiod;
static int aggmode;
static int agglen;
static int wvar;
static long aggmax=40L*AGGSTEP;
static long aggn;
static int m; /* # off agg variables */
static int *v;  /* variables to be aggregated */
static double *xx,*xx2; /* values */
static char *aspace;
static int aggrec;
static int freqvar;

static int prind;
static FILE *tilap;
static char nimi[LLENGTH];


//static char **specs;


static void not_enough_memory()
        { sur_print("\nNot enough memory!"); WAIT; }


static void ei_tilaa(char *s)
        {
        sprintf(sbuf,"\nNot space enough for file %s!",s); sur_print(sbuf);
        WAIT;
        }


static int lue(long j,double *pweight,double *xx)
        {
        int i,k;
        char *p;

        muste_fseek(tilap,j*(long)((m+1)*sizeof(double)),SEEK_SET);
        p=(char *)pweight;
        for (k=0; k<sizeof(double); ++k) { *p=(char)getc(tilap); ++p; }
        for (i=0; i<m; ++i)
            {
            p=(char *)&xx[i];
            for (k=0; k<sizeof(double); ++k) { *p=(char)getc(tilap); ++p; }
            }
        if (ferror(tilap))
            {
            sur_print("\nCannot load temporary data!");
            WAIT; return(-1);
            }
        return(1);
        }

static int talletus()
        {
        int i;
        long j;
//      int prind=1;
        char label[LLENGTH];
        double weight,x;
        char *hp;

        sprintf(sbuf,"\nSaving aggregated data in %s...",word[3]); sur_print(sbuf);
        for (j=1L; j<=aggn; ++j)
            {
            if (prind) { sprintf(sbuf," %d",(int)j); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
            fi_miss_obs(&d2.d2,j);

            if (aggtype=='S')
                {
                strncpy(label,aspace+(j-1)*(long)aggrec,agglen); label[agglen]=EOS;
                fi_alpha_save(&d2.d2,(long)j,aggvar,label);
                }
            else
                {
                hp=aspace+(j-1)*(long)aggrec;
                x=*(double *)hp;
                fi_save(&d2.d2,(long)j,aggvar,&x);
                }

            i=lue(j-1L,&weight,xx); if (i<0) return(-1);
            if (freqvar>=0)
                fi_save(&d2.d2,j,freqvar,&weight);
            for (i=0; i<m; ++i)
                {
                x=xx[i];
                if (aggmode==1) x/=weight;
                if (d2.vartype[v[i]][0]!='S') fi_save(&d2.d2,(long)j,v[i],&x);
                else
                    {
                    fnconv(x,d2.varlen[v[i]],label);
                    fi_save(&d2.d2,(long)j,v[i],label);
                    }
                }
            }
        fi_puts(&d2.d2,(char *)&aggn,4,22L); // RS 28.1.2013 (char *)
        muste_fclose(tilap);
        strcpy(nimi,etmpd); strcat(nimi,"SURVO.TMP");
        remove(nimi);
/*      sprintf(sbuf,"DEL %s",nimi);
        system(sbuf);
*/
        return(1);
        }


static int uusi_tila()
        {
        long ig,paikka;
        char *hp;

        paikka=muste_ftell(tilap);
        hp=aspace;
        for (ig=0L; ig<aggmax; ++ig) { putc((int)*hp,tilap); ++hp; }
        aggmax+=AGGSTEP2;
        aspace=muste_realloc(aspace,aggmax);
        if (aspace==NULL) { not_enough_memory(); return(-1); }
        muste_fseek(tilap,paikka,SEEK_SET);
        hp=aspace;
        for (ig=0L; ig<aggmax; ++ig) { *hp=(char)getc(tilap); ++hp; }
        muste_fseek(tilap,paikka,SEEK_SET);
        return(1);
        }


/*
siirto(i)
int i;
        {
        int k;

        if (aggn==aggmax-1) { k=uusi_tila(); if (k<0) return(-1); }
        memmove(aspace+(i+2)*aggrec,aspace+(i+1)*aggrec,(aggn-i)*aggrec);
        return(1);
        }
*/

static int talleta(long j,double weight,double *xx)
        {
        int i,k;
        char *p;

        muste_fseek(tilap,j*(long)((m+1)*sizeof(double)),SEEK_SET);
        p=(char *)&weight;
        for (k=0; k<sizeof(double); ++k) { putc((int)*p,tilap); ++p; }
        for (i=0; i<m; ++i)
            {
            p=(char *)&xx[i];
            for (k=0; k<sizeof(double); ++k) { putc((int)*p,tilap); ++p; }
            }
        if (ferror(tilap))
            {
            sur_print("\nCannot save temporary data!");
            WAIT; return(-1);
            }
        return(1);
        }


static int aggregate()
        {
        int i;
        long j;
        double weight;
        long miss=0L;
        char aggs[LLENGTH];
        double aggv;
        long ig;
        int vert;
        char *hp;
        double w2;

        strcpy(nimi,etmpd); strcat(nimi,"SURVO.TMP");
        tilap=muste_fopen(nimi,"w+b");
        if (tilap==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        sur_print("\nScanning observations...");
        aggn=0;
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            if (wvar>=0)
                {
                data_load(&d,j,wvar,&weight);
                if (weight==MISSING8) { ++miss; continue; }
                }
            else weight=1.0;
            for (i=0; i<m; ++i)
                {
                data_load(&d,j,v[i],&xx[i]);
                if (xx[i]==MISSING8) break;
                }
            if (i<m) { ++miss; continue; }

            if (prind) { sprintf(sbuf," %ld",j); sur_print(sbuf); }
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }

            if (aggtype)
                {
                if (aggtype=='S')
                    {
                    data_alpha_load(&d,j,aggvar,aggs);
                    for (ig=0L; ig<aggn; ++ig)
                        {
                        vert=strncmp(aggs,aspace+ig*(long)aggrec,agglen);
                        if (vert==0) break;
                        }
                    }
                else
                    {
                    data_load(&d,j,aggvar,&aggv);
                    for (ig=0L; ig<aggn; ++ig)
                        {
                        hp=aspace+ig*(long)aggrec;
                        if (aggv==*(double *)hp) break;
                        }
                    }

                if (ig<aggn)  /* vanha tapaus */
                    {
                    i=lue(ig,&w2,xx2); if (i<0) return(-1);
                    for (i=0; i<m; ++i)
                        {
                        xx[i]=xx2[i]+weight*xx[i];
                        }
                    weight+=w2;
                    i=talleta(ig,weight,xx); if (i<0) return(-1);
                    }
                else  /* uusi tapaus */
                    {
                    if ((aggn+1L)*(long)aggrec>aggmax)
                        {
                        i=uusi_tila();
                        if (i<0) return(-1);
                        }
                    hp=aspace+aggn*(long)aggrec;
                    if (aggtype=='S') strncpy(hp,aggs,agglen);
                    else *(double *)hp=aggv;
                    if (weight!=1.0) for (i=0; i<m; ++i)
                                         {
                                         xx[i]*=weight;    /* 16.8.91 */
                                         }
                    i=talleta(aggn,weight,xx); if (i<0) return(-1);
                    ++aggn;
                    }
                }
            }

/*
        Rprintf("\n");
        for (ig=0L; ig<aggn; ++ig)
            {
            lue(ig,&weight,xx);
            Rprintf("\n%g ",weight);
            for (i=0; i<m; ++i) Rprintf("%g ",xx[i]);
            }
        getch();
*/
        return(1);
        }


static int avaus(char *nimi)
        {
        long j,alku,paikka;

        FILE *uusi;
        char pathname[LNAME];

        strcpy(pathname,nimi);
        if (!muste_is_path(nimi)) // RS 12.6.2013
            {
            strcpy(pathname,edisk); 
            strcat(pathname,nimi);
            }                
        muste_append_path(pathname,".SVO");

        uusi=muste_fopen(pathname,"wb");
        if (uusi==NULL)
            {
            sprintf(sbuf,"\nCannot save file %s!",pathname);
            sur_print(sbuf); WAIT; return(-1);
            }

        paikka=muste_ftell(d.d2.survo_data); // RS 12.6.2013
//        fi_rewind(&(d.d2));        
        muste_fseek(d.d2.survo_data,0,SEEK_SET); // RS 12.6.2013

        alku=(long)(d.d2.data);
        for (j=0; j<alku; ++j)
            {
            putc(getc(d.d2.survo_data),uusi);
            if (ferror(uusi)) { ei_tilaa(pathname); return(-1); }
            }
        muste_fclose(uusi);
        
        muste_fseek(d.d2.survo_data,paikka,SEEK_SET);  // RS 12.6.2013      
        return(1);
        }


static int varaa_tilat()
        {
        aspace=muste_malloc(aggmax);
        if (aspace==NULL) { not_enough_memory(); return(-1); }
        xx=(double *)muste_malloc(m*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return(-1); }
        xx2=(double *)muste_malloc(m*sizeof(double));
        if (xx2==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int aggvars()
        {
        int i;

        m=0;
        for (i=0; i<d.m_act; ++i)
            if (d.v[i]!=aggvar && d.vartype[d.v[i]][1]=='A') v[m++]=d.v[i];
              /* 11.9.91 */
        if (m==0)
            {
            sur_print("\nNo variables to be aggregated activated by 'A'!");
            WAIT;
            }
        return(m);
        }



/*
FILE AGGRE <data>,<data2>
0    1     2      3

*/

void muste_file_aggre(int argc,char *argv[])
        {
        int i;
        long n;
        char x[LLENGTH], *osa[2];

// RS ADD variable init
aggvar=0;
aggtype=0;
aggperiod=0;
aggmode=0;
agglen=0;
wvar=0;
aggmax=40L*AGGSTEP;
aggn=0;
m=0; /* # off agg variables */
aggrec=0;
freqvar=0;
prind=0;
aspace=NULL;
xx=NULL;
xx2=NULL;

        if (argc==1) return;
        s_init(argv[1]);
        if (g<4)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE AGGRE,<data>,<data2>");
            WAIT; return;
            }

        i=data_read_open(word[2],&d); if (i<0) return;
        if (d.type!=2)
            {
            sprintf(sbuf,"\n%s must be a SURVO 84 data file!",word[2]);
            sur_print(sbuf); WAIT; return;
            }
        i=sp_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;

        if (muste_strcmpi(word[2],word[3])==0)
            {
            sprintf(sbuf,"\nThe original file %s cannot be overwritten",word[2]); sur_print(sbuf);
            sur_print("\nby the aggregated file!");
            WAIT; return;
            }
        wvar=activated(&d,'W');

        i=spfind("AGGRE");
        if (i<0)
            {
            sur_print("\nSpecification AGGRE=<aggregate_variable>,<aggre_oper> missing!");
            WAIT; return;
            }
        strcpy(x,spb[i]);
        i=split(x,osa,2);
        if (i==0) { sur_print("\nError in AGGRE specification!"); WAIT; return; }
        aggvar=varfind2(&d,osa[0]);
        if (aggvar>=0) aggtype=d.vartype[aggvar][0]; else aggtype=EOS;
        if (aggvar<0)
            {
            aggperiod=atoi(osa[0]);
            if (aggperiod<=0)
                {
                sur_print("\nIncorrect aggragation period!");
                WAIT; return;
                }
            }
        aggmode=0;  /* "SUM" */
        if (i>1)
            {
            if (muste_strcmpi(osa[1],"MEAN")==0) aggmode=1;
            else if (muste_strcmpi(osa[1],"SUM")==0) aggmode=0;
            else
                {
                sprintf(sbuf,"\nUnknown aggregation mode %s",osa[1]);
                sur_print(sbuf); WAIT; return;
                }
            }

        agglen=0;
        if (aggvar>=0)
            {
            agglen=d.varlen[aggvar];
        if (aggtype!='S') agglen=8;

/*          if (agglen>8) agglen=8;       */
            }

        i=avaus(word[3]); if (i<0) return;

        i=data_open2(word[3],&d2,1,0,0);
        n=0L;
        fi_puts(&d2.d2,(char *)&n,4,22L); // RS 28.1.2013 (char *)
        d2.n=0L;

        freqvar=-1;
        i=spfind("FREQ");
        if (i>=0)
            {
            freqvar=varfind2(&d2,spb[i],0);
            if (freqvar<0)
                {
                freqvar=create_newvar(&d2,spb[i],'8',8);
                if (freqvar<0) return;
                }
            }
        v=(int *)muste_malloc(d.m*sizeof(int));
        if (v==NULL) { not_enough_memory(); return; }

        m=aggvars(); if (m==0) return;
        aggrec=agglen;
        i=varaa_tilat(); if (i<0) return;
        i=aggregate(); if (i<0) return;
        i=talletus();
        data_close(&d);
        data_close(&d2);
        }
