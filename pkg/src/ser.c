/* !ser.c 29.9.1989/SM (4.10.1989)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include <ctype.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXW 10000

static SURVO_DATA d;
static char nimi[LLENGTH];
static char oper[LLENGTH];
static int invar,outvar;
static double paino[MAXW];
static char *sana[MAXW+4];
static int nw,k;
static int per;
static char lauseke[LLENGTH];
static int prind=0;

static int pol_weights(char *s);
static int varaa_tilat();
static int not_enough_memory();
static int aseta_xx();
static int end_effects();
static int ser_cum();
static int ser_d();
static int ser_ms();
static int kirjoita_lauseke(int outvar,char *lauseke);
static int uusi_nimi(int i,char *s);
// RS REM static int muste_isdigit(int c);

// char **specs;
/****************
main(argc,argv)
int argc; char *argv[];
*******************/

void muste_ser(char *argv)
        {
        int i,ip;
        char x[LLENGTH],rivi[LLENGTH];
        char *p,*q,*p1,*p2;
        double sum,sumw,arvo;
        int j,jj;
        char type;

// RS REM        char m='1';
// RS REM        Rprintf("\ndigit=%d",muste_isdigit((int)m));




        s_init(argv);

        if (g<2)
            {
            sur_print("\nUsage: SER <output_var>=MA(<input_var>,<weights>) TO <data> ");
            sur_print("\n       MA()  is for moving averages                         ");
            sur_print("\n       MAE() is for moving averages with end effects        ");
            sur_print("\n       MS()  is for moving sums                             ");
            sur_print("\n       D()   is for differences                             ");
            sur_print("\n       CUM() is for cumulative sums                         ");
            WAIT;
            return;
            }

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        edread(rivi,r1+r-1);
        p=strchr(rivi,PREFIX); if (p==NULL) p=rivi;

        q=strchr(p,'=');
        if (q==NULL)
            { sur_print("\n= is missing!"); WAIT; return; }
        strcpy(lauseke,q+1);
        q=strchr(lauseke,' ');
        if (q!=NULL) *q=EOS;

        g=split(p+1,sana,MAXW+4);
        i=0;
        while (i<g && strcmp(word[i],"/")!=0) ++i;
        g=i;

        if (muste_strcmpi(sana[g-2],"TO")==0) strcpy(nimi,sana[g-1]);
        else  { strcpy(nimi,active_data); g+=2; }

        i=data_open2(nimi,&d,1,0,0); if (i<0) return;

        i=sp_init(r1+r-1);
        if (i<0) { sur_print("\nToo many specifications!"); WAIT; return; }

        per=1;
        i=spfind("PERIOD");
        if (i>=0) { per=atoi(spb[i]); if (per<1) per=1; }

        strcpy(x,sana[1]);
        p=strchr(x,'=');
        if (p==NULL)
            {
            sur_print("\n= missing in SER!"); WAIT; return;
            }
        *p=EOS; ++p;

        type='4';
        q=strchr(x,':');
        if (q==NULL) q=x;
        else
            {
            *q=EOS;
            type=(char)('0'+atoi(q+1));
            }
        outvar=varfind2(&d,x,0);
        if (outvar<0)
            {
            outvar=create_newvar(&d,x,type,1);
            if (outvar<0) return;
            }
        q=strchr(p,'(');
        if (q==NULL)
            {
            sur_print("\n( missing in SER!"); WAIT; return;
            }
        *q=EOS; ++q;   /* p="MA" q=invar */

        strcpy(oper,p);
        p1=p2=oper+strlen(oper)-1;
        while (isdigit((int)*p2) && p2>=oper) --p2;
        if (p2<p1) { ++p2; per=atoi(p2); *p2=EOS; if (per<1) per=1; }

        i=strlen(q)-1; if (q[i]==')') q[i]=EOS;  /* esim. CUM(X) */
        invar=varfind(&d,q);
        if (invar<0) return;

        if (muste_strcmpi(oper,"CUM")==0)
            { ser_cum(); kirjoita_lauseke(outvar,lauseke); s_end(argv[1]); return; }
        if (muste_strcmpi(oper,"D")==0)
            { ser_d(); kirjoita_lauseke(outvar,lauseke); s_end(argv[1]); return; }
        if (muste_strcmpi(oper,"MS")==0)
            { ser_ms(); kirjoita_lauseke(outvar,lauseke); s_end(argv[1]); return; }

/*
printf("\noper=%s invar=%d",oper,invar); getch();
*/
        if (g>5)
            {
            for (i=2; i<g-2; ++i)
                {
                paino[i-2]=atof(sana[i]);
                }
            if (*sana[g-3]!='*') { nw=g-4; k=nw/2; }
            else
                {
                k=g-6; nw=2*k+1;
                for (i=0; i<k; ++i) paino[nw-1-i]=paino[i];
                }
            }
        else  /* g=5 */
            {
            if (g<5)
                {
                sur_print("\nWeights missing!"); WAIT; return;
                }
            strcpy(x,sana[2]);
            p=strchr(x,')');
            if (p==NULL)
                {
                sur_print("\n) missing in SER!"); WAIT; return;
                }
            *p=EOS;
            if (muste_isnumber(x))
                {
                nw=atoi(x); k=nw/2;
                for (i=0; i<nw; ++i) paino[i]=1.0;
                }
            else
                {
                if (*x=='P' && strchr(x,':')!=NULL)
                    {
                    i=pol_weights(x);
                    if (i<0) return;
                    }
                else
                    {
                    i=spfind(x);
                    if (i<0)
                        {
                        sprintf(sbuf,"\nWeights %s not given!",x);
                        sur_print(sbuf); WAIT; return;
                        }
                    strcpy(x,spb[i]);
                    i=split(x,sana,MAXW+1);
                    nw=atoi(sana[0]);
                    k=nw/2;
                    if (i==1)
                        {
                        for (i=0; i<nw; ++i) paino[i]=1.0;
                        }
                    else
                        {
                        nw=i; k=nw/2;
                        for (i=0; i<nw; ++i)
                            {
                            paino[i]=atof(sana[i]);
                            }
                        if (*sana[nw-1]=='*')  /* oli ; perässä! */
                            {
                            k=nw-2; nw=2*k+1;
                            for (i=0; i<k; ++i) paino[nw-1-i]=paino[i];
                            }
                        }
                    }
                }
            }

        sumw=0.0;
        for (i=0; i<nw; ++i)
            sumw+=paino[i];
        for (i=0; i<nw; ++i)
            paino[i]/=sumw;

        sur_print("\n");
        for (j=d.l1; j<=d.l2; ++j)
            {
            sum=0.0;
            for (i=-k, ip=-k*per; i<=k; ++i, ip+=per)
                {
                jj=j+(int)ip;
                if (jj<1L || jj>d.n) break;
                data_load(&d,j+(int)ip,invar,&arvo);
                if (arvo==MISSING8) break;
                sum+=paino[i+k]*arvo;
                }
            if (i<=k) sum=MISSING8;
            data_save(&d,j,outvar,sum);
            if (prind) { sprintf(sbuf,"%d ",j); sur_print(sbuf); }
            }
        if (strchr(oper,'E')!=NULL) end_effects();
        kirjoita_lauseke(outvar,lauseke);
        s_end(argv);
        }

/* ser2.c 1.10.1989/SM (2.10.1989)
*/


static int m;
static double *xx,*qq,*rr,*tt;
static int pol;

static int pol_weights(char *s)
        {
        int i;
        char x[LLENGTH];
        char *p;
        unsigned int ui;

        pol=1;
        strcpy(x,s);
        p=strchr(x,':');
        if (p==NULL)
            {
            sur_print("\n: missing in Polynomial weights!");
            WAIT; return(-1);
            }
        *p=EOS;

        m=atoi(x+1);
        nw=atoi(p+1);
        k=nw/2;
        if (((nw>>1)<<1)==nw)
            {
            sprintf(sbuf,"\nNumber of points (%d) in %s is not odd!",nw,s);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (m+1>nw)
            {
            sprintf(sbuf,"\nToo few points (%d) in %s for polynomial of degree %d!",nw,s,m);
            sur_print(sbuf); WAIT; return(-1);
            }
        i=varaa_tilat(); if (i<0) return(-1);
        aseta_xx();
        i=mat_gram_schmidt(qq,rr,xx,nw,m+1,1e-15);
        if (i!=1)
            {
            sur_print("\nError in Gram-Schmidt orthogonalization!");
            WAIT; return(-1);
            }
        mat_mmt(tt,qq,nw,m+1);
/*      mprint(tt,nw,nw);
*/
        for (i=0, ui=k*nw; i<nw; ++i, ++ui) paino[i]=tt[ui];

        return(1);
        }

static int varaa_tilat()
        {

/* 3.12.1999-
        if (nw>89)
            {
            sur_print("\nMax. number of Polynomial weights is 89!");
            WAIT; return(-1);
            }
*/
        xx=(double *)muste_malloc((m+1)*nw*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return(-1); }
        qq=(double *)muste_malloc((m+1)*nw*sizeof(double));
        if (qq==NULL) { not_enough_memory(); return(-1); }
        rr=(double *)muste_malloc((m+1)*(m+1)*sizeof(double));
        if (rr==NULL) { not_enough_memory(); return(-1); }
        tt=(double *)muste_malloc(nw*nw*sizeof(double));
        if (tt==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory for computing polynomial weights!");
        WAIT; return(1);
        }

static int aseta_xx()
        {
        int i,j,h;
        double a;

        for (i=-k; i<=k; ++i)
            {
            a=xx[i+k]=1.0;
            h=i+k;
            for (j=1; j<=m; ++j)
                {
                h+=nw;
                a*=(double)i;
                xx[h]=a;
                }
            }
        return(1);
        }

static int end_effects()
        {
        int i,h,iper;
        int j;
        double sum,a;
     // int puuttuu;

        if (!pol)
            {
            sprintf(sbuf,"\nThe first and last %d observations can be obtained",k);
            sur_print(sbuf);
            sur_print("\nfor Polynomial weights only.");
            WAIT; return(1);
            }

        sur_print("\nEnd effects: ");

        if (nw*per>d.l2-d.l1+1) return(1);

        for (j=d.l1, i=0; j<d.l1+(int)(k*per); j+=per, ++i)
        for (iper=0; iper<per; ++iper)
            {
            sum=0.0;
            for (h=0; h<nw; ++h)
                {
                data_load(&d,d.l1+(int)(iper+h*per),invar,&a);
                if (a==MISSING8) break;
                sum+=tt[i*nw+h]*a;
                }
            if (h<nw) sum=MISSING8;
            data_save(&d,j+(int)iper,outvar,sum);
            }

        for (j=d.l2-(int)(k*per-1), i=k+1; j<=d.l2; j+=per, ++i)
        for (iper=0; iper<per; ++iper)
            {
            sum=0.0;
            for (h=0; h<nw; ++h)
                {
                data_load(&d,d.l2-(int)(nw*per-1)+(int)(iper+h*per),invar,&a);
                if (a==MISSING8) break;
                sum+=tt[i*nw+h]*a;
                }
            if (h<nw) sum=MISSING8;
            data_save(&d,j+(int)iper,outvar,sum);
            }
        return(1);
        }

/* ser3.c 3.10.1989/SM (4.10.1989)
*/

static int ser_cum()
        {
        int lag;
        double x,y;
        int j,jj;

        lag=1;
        if (g>4) { lag=atoi(word[2]); if (lag<1) lag=1; }

        sur_print("\n");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (j<d.l1+lag) y=0.0;
            else
                {
                jj=j-lag;
                data_load(&d,jj,outvar,&y);
                if (y==MISSING8) break;
                }
            data_load(&d,j,invar,&x);
            if (x==MISSING8) break;
            data_save(&d,j,outvar,x+y);
            if (prind) { sprintf(sbuf,"%d ",j); sur_print(sbuf); }
            }
        return(1);
        }

static int ser_d()
        {
        int lag;
        double x,y,e;
        int j,jj;

        lag=1;
        if (g>4) { lag=atoi(word[2]); if (lag<1) lag=1; }

        sur_print("\n");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (j<d.l1+lag) e=MISSING8;
            else
                {
                jj=j-lag;
                data_load(&d,jj,invar,&y);
                data_load(&d,j,invar,&x);
                if (x==MISSING8 || y==MISSING8) e=MISSING8;
                else e=x-y;
                }
            data_save(&d,j,outvar,e);
            if (prind) { sprintf(sbuf,"%d ",j); sur_print(sbuf); }
            }
        return(1);
        }

static int ser_ms()    /* painot sana[2],...,sana[g-3] */
        {
        int i,h,ip;
        int np;
        int it;
        double sum,a;
        int j,jj;

        np=g-4;
        i=strlen(sana[g-3])-1;
        if (sana[g-3][i]==')') sana[g-3][i]=EOS;
        else
            {
            sur_print("\n) missing!");
            WAIT; return(1);
            }
        it=-1;
        for (i=0; i<np; ++i)
            {
            h=strlen(sana[i+2])-1;
            if (sana[i+2][h]=='T' || sana[i+2][h]=='t') { it=i; sana[i+2][h]=EOS; }
            paino[i]=atof(sana[i+2]);
            }
        if (it<0) it=np-1;

        sur_print("\n");
        for (j=d.l1; j<=d.l2; ++j)
            {
            sum=0.0;
            jj=j-it*per;
            for (i=0, ip=0; i<np; ++i, ip+=per)
                {
                if (jj<1L || jj>d.n) break;
                data_load(&d,jj,invar,&a);
                if (a==MISSING8) break;
                sum+=a;
                jj+=per;
                }
            if (i<np) sum=MISSING8;
            data_save(&d,j,outvar,sum);
            if (prind) { sprintf(sbuf,"%d ",j); sur_print(sbuf); }
            }
        return(1);
        }

/* serni.c 4.10.1989/SM (4.10.1989)
   muunnelma varni.c
*/

static int kirjoita_lauseke(int outvar,char *lauseke)
        {
        if (d.type!=2) return(1);
        uusi_nimi(outvar,lauseke);
        return(1);
        }

#define EQ '\176'

static int uusi_nimi(int i,char *s)
        {
        char x[LLENGTH];
        int k,len,h;


        len=d.d2.l; if (len<10) return(1);
        fi_rewind(&(d.d2));
fi_gets(&(d.d2),x,d.d2.l,(int)(d.d2.var+(int)i*((int)len+(int)d.d2.extra)+(int)d.d2.extra));
        x[len]=EOS;

        k=8; while (x[k]==' ' && k<len) ++k;
        if (k==len || x[k]==EQ)
            {
            if (x[k]==EQ) for (h=k; h<len; ++h) x[h]=' ';
            x[9]=EQ;
            h=0; while (h<strlen(s) && h+10<len) { x[h+10]=s[h]; ++h; }
        fi_rewind(&(d.d2));
fi_puts(&(d.d2),x,d.d2.l,(int)(d.d2.var+(int)i*((int)len+(int)d.d2.extra)+(int)d.d2.extra));
            }
        return(1);
        }

/* RS REM
int muste_isdigit(int c)
    {
    if (c>=(int)'0' && c<=(int)'9') return(1);
    return(0);
    }
*/
