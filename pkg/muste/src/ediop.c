#include "muste.h"
/*  sort.c 7.10.1985/SM (9.11.1991) (11.3.1996)
    LINK !SORT,,,\SM\SM /STACK:32000
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
/* RS REM
#include <conio.h>
#include <string.h>
#include <malloc.h>
#include <search.h>
#include <math.h>
*/
#include "survo.h"
#include "survoext.h"
#include "survolib.h"


// RS REM extern char **spb;

#define MAXCOL 500
#define MISSING 1e308

#define NTYPES 16  /* 12.11.1990 */
#define EOS '\0'
#define TAB '\t'
#define TABSPACE 8
#define N 500
#define NMAX 100
#define SIZE 1000000

static char *argv1;

static char muste_encoding[LLENGTH];
static char nimi[LLENGTH];
static int codeconv;
static int muste_unix; // 17.9.2008
static int n_bytes; /* default 256 */

// RS REM static char *specs0[]={ "SIZE", "TEXTLIMIT", "PRIND", "DEGREE", "BASE", "!" };
// RS REM static char **specs=specs0;

/* COLX START */
static FILE *bin1,*bin2;
extern char *parm[];
extern int g;
extern int soft_vis;
/* COLX END */


/* TRANSPOSE START */
static char *t;
static char **pt;
static char rivi[10*LLENGTH]; // RS CHECK unsigned?
static char delim_in='\t';
static char delim_out='\t';
static FILE *fil1,*fil2;
static char name1[LNAME],name2[LNAME];
/* TRANSPOSE END */

/* STRDIST START */
static char str1[LLENGTH];
static char str2[LLENGTH];
/* STRDIST END */

/* INTERP START */
static int l1,l2,l3,l4,k;
static int mx,my,n,mx0;
static int xcol[EP4],xlev[EP4];
static int ycol[EP4],ylev[EP4];
static char *ymask[EP4];
static double *X,*Y,*B,*XP;
static char msk[LLENGTH], *msana[EP4];
static int degree;
static int base;
/* INTERP END */



/* TRANSP START */
static char *aputila;
static int *transp_lev,*transp_pos;
static int nr,transp_n,slev;
/* TRANSP END */


/* TXT START */
static FILE *txt1,*txt2;
static int nc;
static char type[MAXCOL],char1[MAXCOL],char2[MAXCOL]; // RS N -> MAXCOL // RS REM unsigned
static char *text1[N],*text2[N]; // RS REM unsigned
static char textspace[64000]; // RS REM unsigned
static char *ptext; // RS REM unsigned
static char textlimit='\"';

static int b_conv, b_start, b_step;

static FILE *txt1,*txt2;
static int pituus;
static char merkki;

static FILE *txt1,*txt2;
static long freq[256];

static FILE *txt1,*txt2;
static int lev,n;

static FILE *txt1,*txt2;
static char line1[LLENGTH];
static char line2[LLENGTH];
static char nimi[LLENGTH];

static int g_conv; // RS 24.1.2013
static int c_conv; // SM 26.5.2012
static unsigned char c_[4]; // SM 26.5.2012
/* TXT END */


/* UPDATE START */
static int l1,l2;
static int keylen;
static char name[LLENGTH];
static FILE *edfile;
static int qed1,qed2;
static char *field2;
/* UPDATE END */


/* NCOPY START */
static FILE *tied1,*tied2;
static char nimi1[LLENGTH],nimi2[LLENGTH];
/* NCOPY END */

/* LOADP START */
static FILE *text;
// RS REM static char rivi[10*LLENGTH];
static char *clip;
static FILE *clip_file;
static char clip_filename[LNAME];
/* LOADP END */

/* LINEDEL START */
static FILE *edt1,*edt2,*edt3;
static char nimi1[LLENGTH],nimi2[LLENGTH],nimi3[LNAME];
static char rivin_loppu[]="\15\12";
static int k1,k2;
static int tyyli;
static char sana[LLENGTH];
static char varjosana[LLENGTH];
static int step;
static int last_empty;
static int e_step;
static int n_sp_end;  // RS ADD int
static char crlf[]="\15\12";

static int steplab;
static char stx[LLENGTH];
static int luettu;

static int linedel_reverse;
/* LINEDEL END */



/* FORM START */
static char maskline[LLENGTH], *fmask[MAXCOL];
static int pos[MAXCOL], len[MAXCOL];
// RS REM static unsigned char type[MAXCOL]; // RS unsigned
static char x[LLENGTH], *form_sana[MAXCOL];
static char y[LLENGTH];
static char luku[LLENGTH];
static int fill; /* 1=Alignment to the left (L) 2=to the right (R) */
/* FORM END */


/* MOVE START */
static int mr1,mc1,mr2,mc2,mr,mc;
static FILE *text;
static int edit;
static  char tfile[LLENGTH];
static int ted1,ted2,j;

static int luettu_nro=0;
static char luettu_rivi[LLENGTH+10];
/* MOVE END */

/* CPLUS START */
static int l,l1,l2,k;
static char cplus_mask[LLENGTH];
static char *cplus_sana[EP4];
static int nsar,colz,colx,coly;
static int sar[EP4], cplus_len[EP4];
static char oper;
static int bin;
static char x[LLENGTH];
static int fnro;

static int lr;
/* CPLUS END */

/* SORT L1,L2,K */

static unsigned char *sortlist;
static unsigned char **sortp;
static char *zz;
static int *zs2;
static int sort_len,n;
static char x[LLENGTH];
static char luku[LLENGTH];
static int apos[10],alen[10],an;
static char code[512]; // RS REM unsigned CHA 256 -> 512 SAVEW,LOADW 6.1.2002 varmuuden vuoksi
static int shadow;

// RS REM static char *specs0[]={ "FILTER", "!" };
// RS REM static char **specs=specs0;

static FILE *codes; /* -4.1.1997 defined as local! */
static FILE *codes2;


/*  ctrim.c 16.10.1985/SM (27.12.1991)
    +trimtav+trim2+sh+trimp+trim2p /STACK:4000
    TRIMx L1,L2,lev
    TRIMx L1,L2,lev,Pi (P=proportional pitch, file PITCH.BIN)
                     default for i is 1
                     pitch_ind=256*(i-1)
 */





/* TRIM START */ 
// RS REM static int rsh; /* vain sh.c */
static int pitch[256*NTYPES], pitch_unit, pitch_ind;

static int sar_virhe=0;

static char *zz;
static unsigned int *zzs;

static int jj1,jj2,lev;
static int inj,outj;
static char inx[2*LLENGTH], outx[LLENGTH];
static char sinx[2*LLENGTH], soutx[LLENGTH];
static int reuna, reuna1;
static int viim_rivi;
static char ch1,ch2;
static int trim_tolerance=2;  //  uu-si   =3 kau-si 7.7.2005

static int tavuohje=0;
static FILE *trimfile;
static char code0[256]; // RS REM unsigned
static unsigned char code00[256]; // RS 4.2.2013
static char vokaalit[256];


static FILE *pfile; /* -4.1.1997 defined as local! */
static FILE *codes;

static char srivi[LLENGTH];
static int vajaus, bl;

static int nwords;
static int trim_words=0; // RS ADD
/* TRIM END */


static int read_char(char *s,unsigned char *pch)
        {
        if (muste_strnicmp(s,"char(",5)==0) *pch=(unsigned char)atoi(s+5);
        else *pch=*s;
        return(1);
        }
        
int chrconv(char *s,char *y)
        {
        char *p,*q,*r; // RS REM unsigned
        char cc[2]; // RS REM unsigned

        *y=EOS; p=s; cc[1]=EOS;
        while (1)
            {
            q=strstr(p,"char(");
            if (q==NULL) { strcat(y,p); return(1); }
            *q=EOS; q+=5; strcat(y,p);
            r=strchr(q,')');
            if (r==NULL)
                {
                sprintf(sbuf,"\n) missing in %s",p);
                sur_print(sbuf); WAIT; return(-1);
                }
            *r=EOS;
            *cc=(unsigned char)atoi(q);
            strcat(y,cc);
            p=r+1;
            }

        return(1);
        }


static int load_codes(char *codefile,unsigned char *code)
        {
        int i;
        char nimi[LLENGTH];

        strcpy(nimi,survo_path); strcat(nimi,"SYS/"); // RS CHA \\ /
        strcat(nimi,codefile); strcat(nimi,".BIN");
        i=spfind("FILTER");
        if (i>=0)
            {
            strcpy(nimi,survo_path); strcat(nimi,"SYS/"); // RS CHA \\ /
            if (strchr(spb[i],':')==NULL) strcat(nimi,spb[i]);
            else strcpy(nimi,spb[i]);
            }
        codes=muste_fopen(nimi,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nFilter file %s not found!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose(codes);
        return(1);
        }


/* trimtav.c 20.10.1985/SM (21.10.1985) tavuttaa sanan  (9.11.1991) */


static void load_codes2(char *code) // RS REM unsigned
        {
        int i;
        char x[LLENGTH];

        strcpy(x,survo_path); strcat(x,"SYS/SORTCODE.BIN"); // RS CHA \\ -> /
        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            for (i=0; i<256; ++i) code[i]=(unsigned char)i;
            return;
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose(codes);
        }


static void tavutus()      /* Alkuvalmistelut */
        {
        int i;

        load_codes2(code);
        for (i=0; i<256; ++i) vokaalit[i]='0';
        strncpy(vokaalit+'A',"10001000100000100000100010111",29);
/*                            abcdefghijklmnopqrstuvwxyzaao        */


        }


static int trimfile_tavutus(char *sana0)
    {
    int i;
    char sana[LLENGTH];
    char sana1[LLENGTH];
    char sana2[LLENGTH];
    char *p,*q;
    int len;

// Rprintf("\nsana=%s|",sana); getch();
    strcpy(sana,sana0);
    len=strlen(sana);
    for (i=0; i<len; ++i) sana[i]=code0[(int)sana[i]];

    rewind(trimfile);
    while (1)
        {
        p=sana1; q=sana2;
        while (1)
            {
            *p=(char)fgetc(trimfile);
// Rprintf("\n*p=%c %d|",*p,(int)*p); getch();
            if (feof(trimfile)) return(-1);
            if ((int)*p==10) break;
            *q=*p; ++q;
            if (*p!='-') ++p;
            }
        *p=EOS; *q=EOS;
// Rprintf("\nsana1=%s|",sana1);
// Rprintf("\nsana2=%s|",sana2); getch();
        if (strncmp(sana,sana1,len)==0)
            {
//         Rprintf("\nsana=%s sana1=%s sana2=%s",sana,sana1,sana2);
// getch();

            q=sana2; i=0;
            strcat(q,"-");
            while (1)
                {
                if (*q!='-') ++i;
                ++q;
                if (i==len) break;
                }
// Rprintf("\nq=%s|",q); getch();
            if (*q=='-') --q;
            while (i>0)
                {
                if (*q=='-') break;
                --i; --q;
// Rprintf("\ni_tavu=%d|",i); getch();
                }
// Rprintf("\nsana=%s i=%d|",sana,i); getch();
            if (i<2) return(0);
            return(i);
            }

        }
    return(-1);
    }
    
static int vokaali(unsigned char kirjain)
        {
        return((int)(vokaalit[(int)code[kirjain]]-'0'));
        }

static int tavuta(char *sana)  /* return(viimeisen tavun alkuindeksi) */
        {
        int i=strlen(sana);
        int k;

        if (tavuohje)
            {
            k=trimfile_tavutus(sana);
            if (k>=0) return(k);
            }
        while (!vokaali(sana[i-1]) && i>0) --i;
        if (i==0) return(0);
        while (vokaali(sana[i-1]) && i>2) --i;
//      if (i<3) return(0);
        if (i<trim_tolerance+1) return(0);
        return(i-1);
        }



/* trimp.c 13.12.1985/SM (9.11.1991)
   proportional pitch routines
*/



static void scratch(unsigned int lin)
        {
        unsigned int i,j,l;

        l=(lin-1)*ed1;
        for (j=lin; j<=ed2; ++j)
            {
            z[l++]='*';
            for (i=0; i<ed1-1; ++i) z[l++]=' ';

            if (zs[j]>0) { z[(zs[j]-1)*ed1]='\0'; zs[j]=0; }

            }
        }

static void zedread(char *x,unsigned int lin)
        {
        strncpy(x,zz+(lin-1)*ed1,ed1);
        x[ed1]=EOS;
        }

static void szedread(char *sx,unsigned int lin)
        {
        if (zzs[lin]) zedread(sx,zzs[lin]);
        else { strncpy(sx,space,ed1); sx[ed1]=EOS; }
        }

static int empty2(char *s)
        {
        while (*s) { if (*s!=' ') return(0); ++s; }
        return(1);
        }

static void korjaa()
        {
        if (!sar_virhe)
            {
            sur_print("\nNot enough free lines in the edit field for TRIM!");
            WAIT;
            }
        memcpy(zs,zzs,(unsigned int)sizeof(int)*(ed2+1));
        memcpy(z,zz,(unsigned int)sizeof(char)*ed1*(ed2+edshad));
        }


static void jatka(char *inx,char *sinx,int j)
        {
        int len;
        char x[LLENGTH], sx[LLENGTH];

        zedread(x,j); szedread(sx,j);
        len=ed1;
        while (x[len-1]==' ') x[--len]=EOS;
        sx[len]=EOS;
        strcpy(inx,x+1); strcpy(sinx,sx+1);
        }

static int palauta(int jzz,int jz)
        {
        int i,j;
        char sx[LLENGTH];
        int varjot=1;

        if (r2-viim_rivi<jz-jzz) return(-2);
        for (j=jzz; j<=viim_rivi; ++j, ++jz)
            {
            memcpy(z+(jz-1)*ed1,zz+(j-1)*ed1,ed1);
            if (zzs[j] && varjot)
                {
                i=creatshad(jz);
                if (i>0)
                    {
                    zedread(sx,zzs[j]);
                    edwrite(sx,zs[jz],0);
                    }
                else varjot=0;
                }
            }
        return(1);
        }

static int tulosta()
        {
        int i;

        if (outj>=r2) return(-1);
        i=strlen(outx);
        if (i>ed1-1 && !(i==ed1 && outx[i-1]==' '))
            {
            sar_virhe=1;
            sur_print("\nToo few columns in the edit field!");
            sprintf(sbuf,"\nUse REDIM %d,%d  (for example)",(int)ed2,(int)strlen(outx));
            sur_print(sbuf); WAIT;
            /* korjaa(); */ return(-2); // RS CHA exit(1) -> return(-2);  /* 2.12.1991 */
            }
        edwrite(outx,outj,1);
        if (!empty2(soutx))
            {
            i=creatshad(outj); if (i<0) return(-1);
            edwrite(soutx,zs[outj],1);
            }
        ++outj;
        strncpy(outx,space,reuna-1); outx[reuna-1]=EOS;
        strncpy(soutx,space,reuna-1); soutx[reuna-1]=EOS;
        nwords=0;        
        return(1);
        }
        

static int strpitch(char *s)   // RS CHECK unsigned?
        {
        int len;

        len=0; while (*s) { len+=pitch[*s+pitch_ind]; ++s; }
        return(len);
        }
        
static int trim_kpl(int tav)
        {
        char x[LLENGTH], sx[LLENGTH];
        int i,j,k,maxj;
        char *p,*q; char *sp,*sq;
        char sana[LLENGTH], ssana[LLENGTH];
        char jakosana[LLENGTH], sjakosana[LLENGTH];
        int vaeli;
        int sp_loppuun;

        zedread(x,inj); szedread(sx,inj);
        while (empty2(x+1) && inj<=jj2) { zedread(x,++inj); szedread(sx,inj); }
        if (inj>jj2) return(0);
        reuna=1;
        while(x[reuna]==' ' && sx[reuna]==' ') ++reuna;
                            /* 27.12.91 */
        reuna1=reuna; /* kpl:een 1. rivin reuna */
        maxj=inj;

                           /* 1.5.89 */
        while (!empty2(x+1) && maxj<=jj2 )
                            { zedread(x,++maxj); szedread(sx,maxj);
                              i=1;
                              while(x[i]==' ' && sx[i]==' ' && i<=reuna) ++i;
                                              /* 27.12.91 */
                              if (i<reuna) reuna=i;
                              else if (i>reuna) break;
                            }
        --maxj;
        if (empty2(x+1)) vaeli=1; else vaeli=0;
                 /* tyhj‰ rivi kappaleen j‰lkeen, kun vaeli=1 */
        *inx=EOS;
        jatka(inx,sinx,inj); p=inx+reuna1-1; j=inj;
                             sp=sinx+reuna1-1;
        strncpy(outx,space,reuna1-1); outx[reuna1-1]=EOS;
        strncpy(soutx,space,reuna1-1); soutx[reuna1-1]=EOS;
        while (1)
            {
            while (*p==' ' && *sp==' ') { ++p; ++sp; }
                          /* 27.12.91 */
            if (*p==EOS)
                {
                *inx=EOS; *sinx=EOS;
                if (j<maxj) { jatka(inx,sinx,++j); p=inx+reuna-1;
                                             sp=sinx+reuna-1; continue; }
                }

            if (*inx==EOS) { i=tulosta(); if (i<0) return(-1); break; }
            *sana=EOS; q=sana; *ssana=EOS; sq=ssana;
            while (*p!=' ' && *p) { *q=*p; ++p; ++q; *sq=*sp; ++sp; ++sq; }
            if (*(q-1)=='-' && empty2(p))
                {       /* sana jatkuu seuraavalle riville */
                --q; --sq;
                *inx=EOS; *sinx=EOS; jatka(inx,sinx,++j); p=inx+reuna-1;
                                                         sp=sinx+reuna-1;
                while (*p!=' ' && *p) { *q=*p; ++p; ++q; *sq=*sp; ++sp; ++sq; }
                }
       sp_loppuun=1;
       while (*sp && *sp!=' ' && *p==' ') { *q=*p; ++p; ++q; *sq=*sp; ++sp; ++sq; sp_loppuun=0; }
            *q=EOS; *sq=EOS;

            if (trim_words)
                {
                if (nwords==trim_words) { i=tulosta(); if (i<0) return(-1); }
                }
            else if
                ((pitch_unit && (strpitch(outx)+strpitch(sana)>pitch_unit*lev))
                 || (!pitch_unit && (strlen(outx)+strlen(sana)>lev)))
                {
                if (!tav) { i=tulosta(); if (i<0) return(-1); }
                else
                    {
                    strcpy(jakosana,sana); strcpy(sjakosana,ssana);
                    do
                        {
                        k=tavuta(jakosana);
                        if (k==0) break;
                        jakosana[k]=EOS; sjakosana[k]=EOS;
                        }
      while ((pitch_unit &&
              (strpitch(outx)+strpitch(jakosana)+pitch['-']>pitch_unit*lev))
                 || (!pitch_unit && (strlen(outx)+k+1>lev)));
                    if (k>0)
                        {
                        strcat(outx,jakosana); strcat(outx,"-");
                        strcat(soutx,sjakosana); strcat(soutx," ");
                        i=tulosta(); if (i<0) return(-1);
                        strcpy(sana,sana+k); strcpy(ssana,ssana+k);
                        }
                    else { i=tulosta(); if (i<0) return(-1); }
                    }
                }
            ++nwords;                
            strcat(outx,sana); if (sp_loppuun) strcat(outx," ");
            strcat(soutx,ssana); if (sp_loppuun) strcat(soutx," ");
            }
        if (vaeli) { i=tulosta(); if (i<0) return(-1); }
        inj=maxj+1;
        return(vaeli);  /* 9.5.90 */
        }

static void trim_not_space()
        {
        PR_EBLD;
        sur_print("\nNot space enough in the memory for TRIM!");
        WAIT; PR_ENRM;
        }

static int varaa_zz()
        {
        zzs=(unsigned int *)muste_malloc(sizeof(int)*(ed2+1));
        zz=muste_malloc(sizeof(char)*ed1*(ed2+edshad));
        if (zz==NULL) { trim_not_space(); return(-1); }
        memcpy(zzs,zs,(unsigned int)sizeof(int)*(ed2+1));
        memcpy(zz,z,(unsigned int)sizeof(char)*ed1*(ed2+edshad));
        return(1);
        }

static int lastline0(char *z,unsigned int ed1,unsigned int ed2)
        {
        int i,j;
        char space[LLENGTH];

        for (i=0; i<ed1; ++i) space[i]=' ';
        j=ed2;
        while (j>0)
                {
                if (strncmp(space,z+(j-1)*ed1+1,ed1-1)!=0) break;

                if (*(z+(j-1)*ed1)!='*') break; /* 12.3.1989 */
                if (zs[j])
                    {
                    if (strncmp(space,z+(zs[j]-1)*ed1+1,ed1-1)!=0) break;
                    }

                --j;
                }
        return(j);
        }

static int trim(int tav) /* 0=ei tavutusta (TRIM), 1=tavutus (TRIM3) */
        {
        int i;
        char x[LNAME];

        if (tav)
            {
            i=hae_apu("trim_tolerance",sbuf);
            if (i) trim_tolerance=atoi(sbuf);

            tavuohje=hae_apu("trim_file",x); // 24.3.2006
            if (tavuohje)
                {
                sprintf(sbuf,"%sSYS/%s",survo_path,x); // RS CHA /
//            Rprintf("\nsbuf=%s|",sbuf); getch();
                trimfile=muste_fopen(sbuf,"rt");
                if (trimfile==NULL)
                    {
                    sprintf(x,"Trim file %s not found",sbuf);
                    sur_print(x); WAIT; return(-2); // RS CHA exit(0); // RS FIXME
                    }
                i=load_codes("LOWCASE",code0); if (i<0) return(-1);
                }
            }
        viim_rivi=lastline0(z,ed1,ed2);
        i=varaa_zz(); if (i<0) return(-1);
        if (tav) tavutus();
        scratch(jj1);
        inj=jj1; outj=jj1;
        while (inj<=jj2)
            {
            nwords=0;
            i=trim_kpl(tav); if (i<0) return(-2);
            }
        if (trim_words) return(1);            
        z[(jj1-1)*ed1]=ch1;
        z[(outj-2-i)*ed1]=ch2;
        i=palauta(jj2+1,outj-i); if (i<0) return(-2);
        return(1);
        }


static int pitch_load()
        {
        int i;
        char x[LLENGTH];

        strcpy(x,survo_path); strcat(x,"SYS/PITCH.BIN"); // RS CHA \\ -> /
        pfile=muste_fopen(x,"rb");    /* b lis‰tty 15.11.90 */
        if (pfile==NULL)
            {
            PR_EBLD;
            sprintf(sbuf,"\nPitch file %s not found!",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        for (i=0; i<256*NTYPES; ++i)
            {
            if (feof(pfile)) break; /* 12.11.90 */
            pitch[i]=getc(pfile);
            }
        muste_fclose(pfile);

        pitch_unit=pitch[0];
        return(1);
        }


/*  trim2.c 21.10.1985/SM (22.10.1985)
 */


static void write2(int j)
        {
        edwrite(rivi,j,0);
        if (zs[j]) edwrite(srivi,zs[j],0);
        }
        
static int lisays(char merkki)
        {
        char x[LLENGTH],*p,*q;
        char sx[LLENGTH],*sp,*sq;

        if (vajaus==0) return(0);
        if (strchr(rivi+1,merkki)==NULL) return(vajaus);
        strcpy(x,rivi); strcpy(sx,srivi);
        p=x+1; sp=sx+1;
        rivi[1]=EOS; srivi[1]=EOS;
        while ( (q=strchr(p,merkki))!=NULL )
            {
            sq=sp+(q-p);
            strncat(rivi,p,q-p+1); strncat(srivi,sp,sq-sp+1);
            if (*(q+1)==' ' || *(q+1)==EOS)
                {
                strcat(rivi," "); strcat(srivi," "); --vajaus;
                if (vajaus==0) { strcat(rivi,q+1); strcat(srivi,sq+1);
                                 return(0); }
                }
            p=q+1; sp=sq+1;
            }
        strcat(rivi,p); strcat(srivi,sp); return(vajaus);
        }

static void sedread(char *srivi,int j)
        {

        if (zs[j]==0) { strncpy(srivi,space,ed1); srivi[ed1]=EOS; }
        else edread(srivi,zs[j]);
        }



static int paras(char *x)
        {
        int max;
        int imax,i,i1,i2,i3,i4;
        int m;
// RS REM        char *p;
        int len;

        strcat(x," X");
        len=strlen(x);
        i=1; max=-9999; imax=-1;
        while (1)
            {
            while (x[i]==' ') ++i;
            if (i>=len) break;
            i1=i;
            while (x[i]!=' ' && x[i]!=EOS) ++i;
            if (i>=len) break;
            i2=i;
            while (x[i]==' ') ++i;
            if (i>=len) break;
            i3=i;
            while (x[i]!=' ' && x[i]!=EOS) ++i;
            if (i>=len) break;
            i4=i;
            m=i4-i3+i2-i1-6*(i3-i2-1);
            if (m>max) { max=m; imax=i2; }
            i=i2;
            }
        x[len-2]=EOS;
        return(imax);
        }

static void lisaa()
        {
        char x[LLENGTH], sx[LLENGTH];
        int i;

        while (vajaus)
            {
            strcpy(x,rivi); strcpy(sx,srivi);
            i=paras(x);
            if (i<0) break;
            rivi[i]=EOS; srivi[i]=EOS;
            strcat(rivi," "); strcat(srivi," ");
            strcat(rivi,x+i); strcat(srivi,sx+i);
            --vajaus;
            }
        }


static void trim2()
        {
        int i,j,len;

        for (j=jj1; j<=jj2; ++j)
            {
            edread(rivi,j);
            sedread(srivi,j);
            if (empty2(rivi+1)) continue;
            len=ed1;
            while (rivi[len-1]==' ') { rivi[--len]=EOS; srivi[len]=EOS; }
            vajaus=lev-len+1;
            if (vajaus>=10) continue;
            if (vajaus<0)
                {
                PR_EBLD;
                sprintf(sbuf,"\nLine %d is too long!",j); sur_print(sbuf);
                WAIT; PR_ENRM; return;
                }
            if (rivi[len-1]=='.') continue;
            for (i=0; i<3; ++i) { vajaus=lisays('.'); if (vajaus==0) break; }
            if (vajaus==0) { write2(j); continue; }
            for (i=0; i<2; ++i) { vajaus=lisays(','); if (vajaus==0) break; }
            if (vajaus==0) { write2(j); continue; }
            for (i=0; i<2; ++i) { vajaus=lisays(':'); if (vajaus==0) break; }
            if (vajaus==0) { write2(j); continue; }
            lisaa();
            write2(j);
            }
        }



/*  trim2p.c 21.10.1985/SM (21.2.1988)
 */
 
static void lisaap()
        {
        char x[LLENGTH], sx[LLENGTH];
        int i;

        while (vajaus>=bl)
            {
            strcpy(x,rivi); strcpy(sx,srivi);
            i=paras(x);
            if (i<0) break;
            rivi[i]=EOS; srivi[i]=EOS;
            strcat(rivi," "); strcat(srivi," ");
            strcat(rivi,x+i); strcat(srivi,sx+i);
            vajaus-=bl;
            }
        }

static int lisaysp(char merkki)
        {
        char x[LLENGTH],*p,*q;
        char sx[LLENGTH],*sp,*sq;

        if (vajaus<bl) return(0);
        if (strchr(rivi+1,merkki)==NULL) return(vajaus);
        strcpy(x,rivi); strcpy(sx,srivi);
        p=x+1; sp=sx+1;
        rivi[1]=EOS; srivi[1]=EOS;
        while ( (q=strchr(p,merkki))!=NULL )
            {
            sq=sp+(q-p);
            strncat(rivi,p,q-p+1); strncat(srivi,sp,sq-sp+1);
            if (*(q+1)==' ' || *(q+1)==EOS)
                {
                strcat(rivi," "); strcat(srivi," "); vajaus-=bl;
                if (vajaus<bl) { strcat(rivi,q+1); strcat(srivi,sq+1);
                                 return(vajaus); }
                }
            p=q+1; sp=sq+1;
            }
        strcat(rivi,p); strcat(srivi,sp); return(vajaus);
        }


static void trim2p()
        {
        int i,j,len;

        bl=pitch[pitch_ind+' '];
        for (j=jj1; j<=jj2; ++j)
            {
            edread(rivi,j);
            sedread(srivi,j);
            if (empty2(rivi+1)) continue;
            len=ed1;
            while (rivi[len-1]==' ') { rivi[--len]=EOS; srivi[len]=EOS; }
            vajaus=pitch_unit*lev-strpitch(rivi+1);
            if (vajaus>=10*pitch_unit) continue;
            if (vajaus<0)
                {
                PR_EBLD;
                sprintf(sbuf,"\nLine %d is too long!",j); sur_print(sbuf);
                WAIT; PR_ENRM; return;
                }
            if (rivi[len-1]=='.') continue;
            for (i=0; i<3; ++i) { vajaus=lisaysp('.'); if (vajaus<bl) break; }
            if (vajaus<bl) { write2(j); continue; }
            for (i=0; i<2; ++i) { vajaus=lisaysp(','); if (vajaus<bl) break; }
            if (vajaus<bl) { write2(j); continue; }
            for (i=0; i<2; ++i) { vajaus=lisaysp(':'); if (vajaus<bl) break; }
            if (vajaus<bl) { write2(j); continue; }
            lisaap();
            write2(j);
            }
        }


static void op_trim()
        {
        int i;
        char OP[LLENGTH];
        char x[LLENGTH];
        
        trim_tolerance=2; // RS ADD
        tavuohje=0; // RS ADD

        lev=c3;
        i=hae_apu("trim_width",x); // 29.12.2000
        if (i) lev=atoi(x);

        pitch_unit=0; /* fixed, not proportional character pitch */
        strcpy(x,word[g-1]);
        if (*x=='P')  { i=pitch_load(); if (i<0) return;
                        if (strlen(x)==1) pitch_ind=1;
                        else pitch_ind=atoi(x+1);
                        if (pitch_ind<=1) pitch_ind=1;
                        pitch_ind=256*(pitch_ind-1);
                        pitch_unit=pitch[pitch_ind];
                        --g;
                       }
        if (g<3)
            {
            jj1=r1+r; if (jj1>r2) return;
            edread(x,jj1); if (empty2(x+1)) return;
            jj2=jj1;
            while (!empty2(x+1) && jj2<r2) edread(x,++jj2);
            if (jj2<r2) --jj2;
            if (g==2) { lev=atoi(word[1]); if (lev<=0) lev=c3; }
            }
        else
            {
            jj1=edline2(word[1],1); if (jj1==0) return;
            jj2=edline2(word[2],jj1); if (jj2==0) return;
            if (g>3) { lev=atoi(word[3]); if (lev<=0) lev=c3; }
            }
        if (lev>LLENGTH-4 && !pitch_unit)
            {
            sprintf(sbuf,"\nWidth %d exceeds the maximum value %d!",lev,LLENGTH-4);
            sur_print(sbuf); WAIT; return;
            }

        edread(x,jj1); ch1=*x;
        edread(x,jj2); ch2=*x;

        strcpy(OP,word[0]); muste_strupr(OP);
        if (strcmp(OP,"T")==0 || strcmp(OP,"TRIM")==0)
                { i=trim(0); if (i<-1) korjaa(); return; }
        if (strcmp(OP,"T3")==0 || strcmp(OP,"TRIM3")==0)
                { i=trim(1); if (i<-1) korjaa(); return; }
        if (strcmp(OP,"T2")==0 || strcmp(OP,"TRIM2")==0)
                { if (pitch_unit) trim2p(); else trim2(); return; }
        PR_EBLD;
        sur_print("\nUnknown TRIM operation!");
        WAIT; PR_ENRM; return;
        }


			

static void div_error()
        {
        sprintf(sbuf,"\nZero divisor on edit line %d!",l); sur_print(sbuf);
        WAIT;
        }

static int num_mask(char **cplus_sana,int nsar)
        {
        int i,k;

        for (i=0; i<nsar; ++i)
            {
            if ((k=muste_isnumber(cplus_sana[i]))) return(i);
            }

      return(-1);
        }

static double val(int i,char *x)
        {
        double z;

        x[sar[i]+cplus_len[i]]=EOS;
        z=atof(x+sar[i]);
        if (!fnro) return(z);
        switch (fnro)
            {
          case 1: if (z<=0.0) return(MISSING);
                  return(muste_log(z));
          case 2: return(muste_exp(z));
          case 3: if (z<0) return(MISSING);
                  return(muste_sqrt(z));
          case 4: return(muste_sin(z));
          case 5: return(muste_cos(z));
          case 6: return(muste_tan(z));
          case 7: return(muste_atan(z));
          case 8: return(muste_floor(z));
          case 9: return(muste_fabs(z));
            }
        return(0.0);
        }

static void compute()
        {
// RS REM        extern double val();
        int i;
        double result=0;
        char tulos[LLENGTH];
        double xval,yval;

        for (l=l1; l<=l2; ++l)
            {
            edread(x,l);
            if (bin)
                {
                xval=val(colx,x); yval=val(coly,x);
                switch (oper)
                    {
                  case '-': result=xval-yval; break;
                  case '/': if (yval==0.0) { div_error(); return; }
                            result=xval/yval; break;
                  case '%': if (yval==0.0) { div_error(); return; }
                            result=100.0*xval/yval; break;
                    }
                }
            else
                {
                if (oper=='+')
                    {
                    result=0.0;
                    for (i=0; i<nsar; ++i)
                        {
                        if (i==colz) continue;
                        result+=val(i,x);
                        }
                    }
                else
                    {
                    result=1.0;
                    for (i=0; i<nsar; ++i)
                        {
                        if (i==colz) continue;
                        result*=val(i,x);
                        }
                    }
                }
            fconv(result,cplus_sana[colz],tulos);
            strcat(tulos," ");
            edwrite(tulos,l,sar[colz]);
            }
        }


static int function(char *s)
        {
        muste_strupr(s);
        if (strcmp(s,"LOG")==0 || strcmp(s,"LN")==0) return(1);
        if (strcmp(s,"EXP")==0) return(2);
        if (strcmp(s,"SQRT")==0 || strcmp(s,"SQR")==0) return(3);
        if (strcmp(s,"SIN")==0) return(4);
        if (strcmp(s,"COS")==0) return(5);
        if (strcmp(s,"TAN")==0) return(6);
        if (strcmp(s,"ARCTAN")==0) return(7);
        if (strcmp(s,"INT")==0) return(8);
        if (strcmp(s,"ABS")==0) return(9);

        return(0);
        }



/* lplus.c 26.12.1986/SM (3.10.1989)
     L+    L1,L2,K,L
 word[] 0  1  2  3 4
*/

static void lcompute()
        {
// RS REM        extern double val();
        int i;
        double result=0;
        char tulos[LLENGTH];
        char x2[LLENGTH];
        double xval,yval;

        edwrite(space,lr,1);
        if (bin)
            {
            edread(x,l1);
            edread(x2,l2);
            for (i=0; i<nsar; ++i)
                {
                xval=val(i,x); yval=val(i,x2);
                switch (oper)
                    {
                  case '-': result=xval-yval; break;
                  case '/': if (yval==0.0) { div_error(); return; }
                            result=xval/yval; break;
                  case '%': if (yval==0.0) { div_error(); return; }
                            result=100.0*xval/yval; break;
                    }
                fconv(result,cplus_sana[i],tulos);
                edwrite(tulos,lr,sar[i]);
                }
            }
        else
            {
            for (i=0; i<nsar; ++i)
                {
                if (oper=='+') result=0.0; else result=1.0;
                for (l=l1; l<=l2; ++l)
                    {
                    edread(x,l);
                    if (oper=='+')
                        result+=val(i,x);
                    else
                        result*=val(i,x);
                    }
                fconv(result,cplus_sana[i],tulos);
                edwrite(tulos,lr,sar[i]);
                }
            }
        }


static void lplus()
        {
        int i;

        if (g<5)
            {
            sprintf(sbuf,"\nUsage: %s L1,L2,K,L",word[0]); sur_print(sbuf);
            WAIT; return;
            }
        oper=word[0][1];
        l1=edline2(word[1],1,1); if (l1==0) return;
        i=l1; if (strchr("-/%",oper)!=NULL) i=1;    /* 3.10.89 */
        l2=edline2(word[2],i,1); if (l2==0) return;
        k=edline2(word[3],1,1); if (k==0) return;
        lr=edline2(word[4],1,1); if (lr==0) return;
        edread(cplus_mask,k);
        nsar=split(cplus_mask+1,cplus_sana,EP4);
        for (i=0; i<nsar; ++i)
            {
            sar[i]=cplus_sana[i]-cplus_mask;
            cplus_len[i]=strlen(cplus_sana[i]);
            }

        bin=0;
        fnro=function(word[0]+2);
        if (strchr("-/%",oper)!=NULL)
            bin=1;

        lcompute();
        }

static void op_cplus()
        {
        int i;

        if (*word[0]=='L' || *word[0]=='l') { lplus(); return; }
        if (g<4)
            {
            sprintf(sbuf,"\nUsage: %s L1,L2,K",word[0]); sur_print(sbuf);
            WAIT; return;
            }
        l1=edline2(word[1],1,1); if (l1==0) return;
        l2=edline2(word[2],l1,1); if (l2==0) return;
        k=edline2(word[3],1,1); if (k==0) return;
        edread(cplus_mask,k);
        nsar=split(cplus_mask+1,cplus_sana,EP4);
        colz=num_mask(cplus_sana,nsar);
        if (colz<0)
            {
            sur_print("\nNo numeric mask for the result!"); // RS CHA Rprintf -> sur_print
            WAIT; return;
            }
        for (i=0; i<nsar; ++i)
            {
            sar[i]=cplus_sana[i]-cplus_mask;
            cplus_len[i]=strlen(cplus_sana[i]);
            }

        bin=0;
        oper=word[0][1];
        fnro=function(word[0]+2);
        if (strchr("-/%",oper)!=NULL)
            {
            bin=1;
            if (nsar!=3)
                {
                sur_print("\nToo many operand columns (One X and one Y column allowed)");
                WAIT; return;
                }
            colx=coly=-1;
            for (i=0; i<3; ++i)
                {
                if (i==colz) continue;
                if (cplus_sana[i][0]=='X') colx=i;
                if (cplus_sana[i][0]=='Y') coly=i;
                }
            if (colx<0)
                {
                sur_print("\nNo X column!");
                WAIT; return;
                }
            if (coly<0)
                {
                sur_print("\nNo Y column!");
                WAIT; return;
                }
            }

        compute();
        }



static int lmove(int j1,int j2)
        {
        int i;
// RS REM        char xs[LLENGTH];
        int lev;

        lev=mc2-mc1+1; if (lev>c2-mc+1) lev=c2-mc+1;
        memcpy(z+(j2-1)*ed1+mc,z+(j1-1)*ed1+mc1,lev);
        if (!zs[j1] && !zs[j2]) return(1);
        if (!zs[j1]) { i=shadow_create(j1); if (i<0) return(-1); }
        if (!zs[j2]) { i=shadow_create(j2); if (i<0) return(-1); }
        memcpy(z+(zs[j2]-1)*ed1+mc,z+(zs[j1]-1)*ed1+mc1,lev);
        shadow_test(j1);
        shadow_test(j2);

        return(1);
        }


static int move()
        {
        int i,j;

        if (mr<=mr1)
            for (j=mr1; j<=mr2; ++j)
                {
                i=lmove(j,mr+j-mr1);
                if (i<0) return(-1);
                }
        else
            for (j=mr2; j>=mr1; --j)
                {
                i=lmove(j,mr+j-mr1);
                if (i<0) return(-1);
                }
        return(1);
        }

static void hae(char m,int *pr,int *pc)
        {
        int lkm=0;
        int j;
        char x[LLENGTH];
        char *p;
        
        lkm=0; // RS ADD

        for (j=1; j<=r2; ++j)
            {
            if (j==r1+r-1) continue;
            edread(x,j);
            p=strchr(x+1,m); if (p==NULL) continue;
            if (lkm || strchr(p+1,m)!=NULL)
                {
                sprintf(sbuf,"\nCharacter %c appears several times in the edit field!",
                                m);
                sur_print(sbuf); WAIT; *pr=-1; return;
                }
            lkm=1; *pc=p-x; *pr=j;
            }
        if (!lkm)
                {
                sprintf(sbuf,"\nCharacter %c not found in the edit field!",m);
                sur_print(sbuf); WAIT; *pr=-1; return;
                }

        }



static int laji()
        {
        int i;
        char rivi[ELE];
        char *sana[3];
        char nimi[LNAME];

		if (!muste_is_path(tfile))
//        if (strchr(tfile,':')==NULL)  // RS CHA
        	{ 
        	strcpy(nimi,edisk); 
        	strcat(nimi,tfile);
            strcpy(tfile,nimi);
            }
        strcpy(nimi,tfile);
        muste_append_path(nimi,".EDT");
//        if (strchr(nimi+strlen(nimi)-4,'.')==NULL) strcat(nimi,".EDT");
        
        
        text=muste_fopen(nimi,"rt");
        if (text==NULL)
            {
            text=muste_fopen(tfile,"rt");
            if (text==NULL)
                {
                sprintf(sbuf,"\nText file %s not found!",tfile);
                sur_print(sbuf); WAIT; return(-1);
                }
            return(0);
            }

        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(text);
        rivi[ELE-1]=EOS;

/* SURVO 98 edit field: 101 1000 30  */
        if (strncmp(rivi,"SURVO 98 edit field:",20)==0)
            {
            i=split(rivi+20,sana,3);
            ted1=atoi(sana[1]); ted2=atoi(sana[2]); rewind(text);
            while (1) { i=getc(text); if (i==(int)'\n') break; }
            return(3);
            }

        i=split(rivi,sana,3);
        if (i<3) { rewind(text); return(0); }
        if (strcmp(sana[0],"SURVO84ED")!=0) { rewind(text); return(0); }
        ted1=atoi(sana[1]); ted2=atoi(sana[2]);
        return(1);
        }

static int etsi(int rivi)
        {
        int merkki;

        if (edit)
            {
            if (edit==3) return(1);
            if (rivi<1) rivi=1;
            if (rivi>ted2) rivi=ted2;
            j=rivi;
            if (edit==2) return(1);   /* current field */
            muste_fseek(text,(long)((unsigned int)j*(unsigned int)ted1),0); 
            return(1);
            }

        while (j<rivi && !feof(text))
            {
            merkki=getc(text);
            if (merkki=='\n') ++j;
            }
        return(1);
        }

static void lue_rivi(char *s)
        {
        int merkki;
        int i;

        if (edit==2)
            { edread(s,j); return; }

        if (edit)
            {
            for (i=0; i<ted1; ++i) { *s=(char)getc(text); ++s; }
            *s=EOS; return;
            }
        while ((merkki=getc(text))!='\n' && !feof(text))
            { *s=(char)merkki; ++s; }
        *s=EOS;
        }


static void lue_rivi3(char *s,int j)
        {
        int i;
        char x[LLENGTH+10];
        char *p;
        int rivi;

        if (luettu_nro==j)
            {
            strcpy(s,luettu_rivi);
            luettu_nro=0; return;
            }
        if (luettu_nro>j)
            {
            strcpy(s,"*"); return;
            }

         while (1)
            {
            fgets(x,LLENGTH+10-1,text);
            if (feof(text)) return; // RS CHA exit(0); // RS FIXME
            p=strchr(x,'|');
            if (p==NULL) { sur_print("\nError in edit file!");
                           WAIT; return; // RS CHA exit(0); // RS FIXME
                         }
            *p=EOS; ++p;
            rivi=atoi(x);
            if (rivi==0) continue; /* varjorivi */
            if (rivi<j) continue;
            i=strlen(p); if (p[i-1]=='\n') p[i-1]=EOS;
            if (rivi==j)
                {
                strcpy(s,p); luettu_nro=0; return;
                }
            /* rivi>j */
            strcpy(luettu_rivi,p); luettu_nro=rivi;
            strcpy(s,"*"); return;
            }
        }

/*
MOVE L1,L2,C1,C2 FROM <file> TO L,C

*/
static void textmove()
        {
        int par,i,lev,len;
// RS REM        char nimi[LNAME];
        char x[LLENGTH];
        char x2[LLENGTH+10];

        if (g<5)
            {
            sur_print("\nUsage: MOVE L1,L2,C1,C2 FROM <edit or text file> TO L,C");
            WAIT; return;
            }
        mr1=atoi(word[1]);
        mr2=atoi(word[2]);
        mc1=1; mc2=c2; par=3;
        if (muste_strcmpi(word[3],"FROM")!=0)
            {
            mc1=atoi(word[3]); mc2=atoi(word[4]);
            par=5;
            }
        if (muste_strcmpi(word[par],"FROM")==0)
            {
            strcpy(tfile,word[par+1]);
            }

        mr=edline2(word[par+3],1,1); if (mr==0) return;
        mc=1; if (g>=par+5) mc=atoi(word[par+4]);
        edit=laji(); if (edit<0) return; // RS CHA exit(0) -> return;

        j=1;
        etsi(mr1);
        lev=mc2-mc1+1; if (lev>ed1-mc+1) lev=ed1-mc+1;
        for (j=mr1; j<=mr2; ++j)
            {
            if (mr+j-mr1>r2) break;
            if (edit==3) lue_rivi3(x2,j);
            else lue_rivi(x2);
            len=strlen(x2);
            if (len<ed1) for (i=len; i<ed1; ++i) x2[i]=' ';
            edread(x,mr+j-mr1);
            if (edit==3) memcpy(x+mc,x2+mc1,lev);
            else memcpy(x+mc,x2+mc1+edit-1,lev);
            edwrite(x,mr+j-mr1,0);
            }
        muste_fclose(text);
        }



static void op_move()
        {
// RS REM        int i;
// RS REM        int *pi;
        
        luettu_nro=0;

        if (g==1 || (g==2 && strlen(word[1])!=3))
            {
            sur_print("\nUsage:");
            sur_print("\n   MOVE xyz / x,y,z any characters");
            sur_print("\n   or");
            sur_print("\n   MOVE L1,L2,C1,C2 FROM <edit or text file> TO L,C");
            WAIT; return;
            }
        if (g==2)
            {
            hae(word[1][0],&mr1,&mc1); if (mr1<0) return;
            hae(word[1][1],&mr2,&mc2); if (mr2<0) return;
            hae(word[1][2],&mr,&mc); if (mr<0) return;
            move();
            return;
            }
        textmove();
        }




static int l_rivi(char *x,int *plab)
    {
// RS REM    int len;

    if (feof(edt1)) return(-1);
    fgets(x,LLENGTH,edt1);
    if (feof(edt1)) return(-1);
    if (*x=='S') *plab=0;
    else *plab=atoi(x);
    return (1);
    }


static int l_rivi2(char *x,int *plab)
    {
    int i;

    i=l_rivi(x,plab);
    if (i<0) return(-1);
    if (n_sp_end>0)
        {
        i=strlen(x)-2; x[i]=EOS; // crlf pois!  // RS FIXME?
        strncat(x,space,n_sp_end);
        strcat(x,crlf);
        }
    return(1);
    }



static int t_rivi(char *x,int lab)
    {
    char *p;
    int form;

    if (*x!='S')
        {
        p=strchr(x,'|');
        form=p-x;
        sprintf(sbuf,"%.*d",form,lab);
        strncpy(x,sbuf,form);
        }
    fputs(x,edt2);
    return(1);
    }

static int dt_rivi(char *x,int lab)
    {
    char *p;
    int form;

    if (*x!='S')
        {
        p=strchr(x,'|');
        form=p-x;
        sprintf(sbuf,"%.*d",form,lab);
        strncpy(x,sbuf,form);
        }
    fputs(x,edt3);
    return(1);
    }
    
static int del_empty_lines()
    {
    int i;
    char x[LLENGTH];
    int lab; //  0=varjorivi
    int lab2;
    int valinta;
    int dlab;
    int jo_luettu;
    int lab0;
    char *p;
    int x0;
    char x2[LLENGTH];
    int talletus;
    int first=1;
    int k;

    k=e_step; // if >k empty lines, k lines left empty

    valinta=0;
    dlab=0; lab0=0; lab=0;

    i=l_rivi(x,&lab); jo_luettu=1;
    p=strchr(x,'|');
    x0=(p-x)+1;

    while (1)
        {
        if (!jo_luettu) { i=l_rivi(x,&lab); if (i<0) return(1); }
        jo_luettu=0;
// Rprintf("\nlab=%d valinta=%d x=%s|",lab,valinta,x); getch();
        switch (valinta)
            {
          case 0:
              if (lab<k1) { t_rivi(x,lab); break; }
              if (lab>=k1)
                  {
                  jo_luettu=1;
                  valinta=1;
                  dlab=lab-k1+1; lab0=lab;
                  break;
                  }
          case 1:
              if (lab>k2)
                  {
                  jo_luettu=1;
                  valinta=2;
                  dlab+=k2-lab0; lab0=lab;
                  break;
                  }

              if (lab<=k2)
                  {
                  talletus=0;
                  lab2=lab;
//                dlab+=lab-lab0; lab0=lab;    -30.12.2003

                  i=lab-lab0;
                  if (!first)
                      { if (i>k) i-=k; else i=1; }
                  first=0;
                  dlab+=i; lab0=lab;

// Rprintf("\nx+x0=%.30s|",x+x0); getch();
// Rprintf("\nx=%d|",(int)*(x+x0+1)); getch();
                  if (*(x+x0)!='*' || (int)*(x+x0+1)!=13)
                      {
                      --dlab;
                      t_rivi(x,lab-dlab);
                      talletus=1;
                      }
                  else strcpy(x2,x);
                  i=l_rivi(x,&lab); if (i<0) return(1);
                  if (lab!=0) jo_luettu=1;
                  else if (talletus)
                      t_rivi(x,lab-dlab);
                  else
                      {
                      --dlab;
                      t_rivi(x2,lab0-dlab);
                      t_rivi(x,lab-dlab);
                      }
                  }
              if (lab2>=k2) valinta=2;
              break;
          case 2:
              t_rivi(x,lab-dlab); break;
            }
        }
    return(1);
    }

static int del_by_control_chars()
    {
    int i;
    char x[LLENGTH];
    int lab; //  0=varjorivi
    int lab2;
    int valinta;
    int dlab;
    int jo_luettu;
//    int lab0;
    char *p;
    int x0;
// RS REM    char x2[LLENGTH];
    int talletus;
// RS REM    char varjot[LLENGTH];
    int labdel=2;

    valinta=0;
    dlab=0; lab=0; // lab0=0;

    i=l_rivi(x,&lab); jo_luettu=1;
    p=strchr(x,'|');
    x0=(p-x)+1;

    while (1)
        {
        if (!jo_luettu) { i=l_rivi(x,&lab); if (i<0) return(1); }
        jo_luettu=0;
// Rprintf("\nlab=%d valinta=%d x=%s|",lab,valinta,x); getch();
        switch (valinta)
            {
          case 0:
              if (lab<k1) { t_rivi(x,lab); break; }
              if (lab>=k1)
                  {
                  jo_luettu=1;
                  valinta=1;
//                dlab=lab-k1+1; lab0=lab;
                  break;
                  }
          case 1:
              if (lab>k2)
                  {
                  jo_luettu=1;
                  valinta=2;
           /*     dlab+=k2-lab0; lab0=lab; */
                  break;
                  }

              if (lab<=k2)
                  {
                  talletus=0;
                  lab2=lab;
           /*     dlab+=lab-lab0;  lab0=lab; */
// Rprintf("\nx=%c|",*(x+x0)); getch();

                  if (linedel_reverse)  // RS ADD
                    {
                    if (strchr(sana,*(x+x0))!=NULL)
                        {
//                      --dlab;
                        t_rivi(x,lab-dlab);
                        talletus=1;
                        }
                    else
                        {
                        ++dlab;
                        if (*nimi3) dt_rivi(x,labdel++);
                        }
                    }
                   else
                    {
                    if (strchr(sana,*(x+x0))==NULL)
                        {
//                      --dlab;
                        t_rivi(x,lab-dlab);
                        talletus=1;
                        }
                    else
                        {
                        ++dlab;
                        if (*nimi3) dt_rivi(x,labdel++);
                        }
                    }
                   
                      
                  i=l_rivi(x,&lab); if (i<0) return(1);
                  if (lab!=0) jo_luettu=1;
                  else if (talletus)
                      t_rivi(x,lab-dlab);
                  else if (*nimi3) dt_rivi(x,0);
                  }
              if (lab2>=k2) valinta=2;
              break;
          case 2:
              t_rivi(x,lab-dlab); break;
            }
        }
    return(1);
    }
    

static int words_in(char *x,int multi,char **mword)
    {
    int i;
    char *p;
    char y[LLENGTH];


    strcpy(y,x);
    *y=' '; p=strchr(y,'\15'); *p=' ';
    for (i=0; i<multi; ++i)
        {
        p=strstr(y,mword[i]);
//Rprintf("\ny: %s, mword: %s",y,mword[i]);        
        if (p==NULL) return(0);
        if (*(p-1)!=' ' || *(p+strlen(mword[i]))!=' ') return(0);
        }
    return(1);
    }


static int del_by_words()
    {
    int i,j;
    char x[LLENGTH];
    int lab; //  0=varjorivi
    int lab2;
    int valinta;
    int dlab;
    int jo_luettu;
    int lab0;
    char *p;
    int x0;
// RS REM    char x2[LLENGTH];
    int talletus;
    int labdel=2;
    char y[LLENGTH];

    int multi; // 26.5.2006
    char *mword[100];

// Rprintf("\ng=%d|",g); getch();
    multi=0;
    labdel=2; // RS ADD

    j=r1+r-1; // 8.7.2006
    edread(y,j);
// Rprintf("\nx=%s|",y); // getch();
    g=3; i=3; p=y+1;
    while (1)
        {
        p=strchr(p,'"');
        if (p==NULL) break;
        ++p;
        word[g]=p; // RS CHA i -> g
        ++g;
        p=strchr(p,'"');
        if (p!=NULL) { ++p; *p=EOS; ++p; }
        }

// Rprintf("\ng=%d|",g); getch();
    if (g>4)
        {
        multi=g-4+1;
        if (multi>10) { sur_print("\nMax. # of words is 10!"); // RS CHA 100! -> 10!
                        WAIT; return(-2); } // RS CHA exit(0); // RS FIXME                    
        for (i=0; i<multi; ++i)
            {
            p=word[i+3];
            if (*p=='"') ++p;
            j=strlen(p)-1;
            if (p[j]=='"') p[j]=EOS;
            mword[i]=p;
//Rprintf("\nword(%d)=%s|",i,mword[i]); // getch();
            }

        }
    valinta=0;
    dlab=0; lab0=0; lab=0;

//Rprintf("\nmulti=%d",multi);

    n_sp_end=0;
    i=strlen(sana)-1; while (i>0 && sana[i]==' ') { --i; ++n_sp_end; }

    i=l_rivi2(x,&lab); jo_luettu=1;
    p=strchr(x,'|');
    x0=(p-x)+1;

    while (1)
        {
        if (!jo_luettu) { i=l_rivi2(x,&lab); if (i<0) return(1); }
    
        
        if (linedel_reverse) dlab+=lab-lab0-1;  // RS ADD
// Rprintf("\nlab: %d, curlab: %d, dlab: %d, revline: %d",lab,lab-dlab,dlab,lab0,revline);        



        jo_luettu=0;
// Rprintf("\nlab=%d valinta=%d x=%s|",lab,valinta,x); getch();
        switch (valinta)
            {
          case 0:
              if (lab<k1) { t_rivi(x,lab); break; }
              if (lab>=k1)
                  {
                  jo_luettu=1;
                  valinta=1;
                  lab0=k1; // RS ADD
                  dlab=lab-k1; // RS ADD
                  break;
                  }
          case 1:
              if (lab>k2)
                  {
                  jo_luettu=1;
                  valinta=2;
                  lab0=lab;
                  break;
                  }

              if (lab<=k2)
                  {
                  talletus=0;
                  lab2=lab;
                  lab0=lab;

                  if (linedel_reverse)  // RS ADD
                    {
                    if (multi)
                        {
                        if (words_in(x+x0,multi,mword))
                            {                           
                            t_rivi(x,lab-dlab);  
                            talletus=1;
                            }
                        else
                            {
                            ++dlab;
                            if (*nimi3) dt_rivi(x,labdel++);
                            }
                        }
                    else
                        {
                        
                        if (strstr(x+x0,sana)!=NULL)
                            {   
                            t_rivi(x,lab-dlab);
// RS Rprintf(" - save line %d as line %d",lab,lab-dlab);                        
                            talletus=1;
                            }
                        else
                            {
                            ++dlab;
                            if (*nimi3) dt_rivi(x,labdel++);
                            }
                        }
                     }
                   else
                     {
                    if (multi)
                        {
                        if (!words_in(x+x0,multi,mword))
                            {
                            t_rivi(x,lab-dlab);
                            talletus=1;
                            }
                        else
                            {
                            ++dlab;
                            if (*nimi3) dt_rivi(x,labdel++);
                            }
                        }
                    else
                        {
                        if (strstr(x+x0,sana)==NULL)
                            {
                            t_rivi(x,lab-dlab);
                            talletus=1;
                            }
                        else
                            {
                            ++dlab;
                            if (*nimi3) dt_rivi(x,labdel++);
                            }
                        }
                     }



                  i=l_rivi2(x,&lab); if (i<0) return(1);
                  if (lab!=0) jo_luettu=1;
                  else if (talletus)
                      t_rivi(x,lab-dlab);
                  else if (*nimi3) dt_rivi(x,0);
                  }
              if (lab2>=k2) valinta=2;
              break;
          case 2:
              t_rivi(x,lab-dlab); break;
            }
        }

    return(1);
    }    

static int del_all_lines()
    {
    int i;
    char x[LLENGTH];
    int lab; //  0=varjorivi
    int lab2;
    int valinta;
    int dlab;
    int jo_luettu;
    int lab0;
    int dlabdel=k1-2;

// Rprintf("\nk1=%d k2=%d",k1,k2); getck();
    valinta=0;
    dlab=0; lab0=0; lab=0;
    jo_luettu=0;
    while (1)
        {
        if (!jo_luettu) { i=l_rivi(x,&lab); if (i<0) return(1); }
        jo_luettu=0;
// Rprintf("\nlab=%d valinta=%d x=%s|",lab,valinta,x); getch();
        switch (valinta)
            {
          case 0:
              if (lab<k1) { t_rivi(x,lab); break; }
              if (lab>=k1)
                  {
                  jo_luettu=1;
                  valinta=1;
                  dlab=lab-k1+1; lab0=lab;
                  break;
                  }
          case 1:
              if (lab>k2)
                  {
                  jo_luettu=1;
                  valinta=2;
                  dlab+=k2-lab0; lab0=lab;
                  break;
                  }

              if (lab<=k2)
                  {
                  lab2=lab;
                  dlab+=lab-lab0; lab0=lab;
                  if (*nimi3) dt_rivi(x,lab-dlabdel);
                  i=l_rivi(x,&lab); if (i<0) return(1);
                  if (lab!=0) jo_luettu=1;
                  else if (*nimi3) dt_rivi(x,lab-dlabdel);
                  }
              if (lab2>=k2) valinta=2;
              break;
          case 2:
              t_rivi(x,lab-dlab); break;
            }
        }
    return(1);
    }




static int del_by_words2()
    {
    int i;
    char x[LLENGTH];
    int lab; //  0=varjorivi
    int lab2;
    int valinta;
    int dlab;
    int jo_luettu;
    int lab0;
    char *p;
    int x0;
    char x2[LLENGTH];
//    int talletus;
    char sana2[2*LLENGTH];
    char xx2[2*LLENGTH];
    int i1,i2,imax;
    int labdel=2;

    n_sp_end=0;
    i=strlen(sana)-1; while (i>0 && sana[i]==' ') { --i; ++n_sp_end; }

    for (i=0; i<strlen(sana); ++i)
        {
        sana2[2*i]=sana[i];
        sana2[2*i+1]=varjosana[i];
        }
    sana2[2*i]=EOS;

    valinta=0;
    dlab=0; lab0=0; lab=0;

    i=l_rivi2(x,&lab); jo_luettu=1;
    p=strchr(x,'|');
    x0=(p-x)+1;

    while (1)
        {
        if (!jo_luettu) { i=l_rivi2(x,&lab); if (i<0) return(1); }
        jo_luettu=0;

        if (linedel_reverse) dlab+=lab-lab0-1;  // RS ADD

        switch (valinta)
            {
          case 0:
              if (lab<k1) { t_rivi(x,lab); break; }
              if (lab>=k1)
                  {
                  jo_luettu=1;
                  valinta=1;
                  break;
                  }
          case 1:
              if (lab>k2)
                  {
                  jo_luettu=1;
                  valinta=2;
                  lab0=lab;
                  break;
                  }

              if (lab<=k2)
                  {
//                  talletus=0;
                  lab2=lab;
                  lab0=lab;

                  i=l_rivi2(x2,&lab);

                  if (i<0)
                      {
                      if (lab2==k2)   // 27.7.2004
                          t_rivi(x2,lab0-dlab);
                      return(1);
                      }
                  if (lab!=0)
                      {
                      t_rivi(x,lab0-dlab);
                      strcpy(x,x2); jo_luettu=1;
                      }
                  else if (strstr(x+x0,sana)==NULL)
                      {
                      t_rivi(x,lab0-dlab);
                      t_rivi(x2,lab0-dlab);
                      }
                  else
                      {
                      i1=strlen(x); i2=strlen(x2);
                      imax=i1; if (i2>i1) imax=i2;
                      for (i=0; i<imax; ++i)
                          {
                          if (2*i>=2*imax) xx2[2*i]=' ';
                          else xx2[2*i]=x[i];
                          if (2*i+1>=2*imax) xx2[2*i+1]=' ';
                          else xx2[2*i+1]=x2[i];
                          }
                      xx2[2*imax]=EOS;
                      
                      if (linedel_reverse) // RS ADD
                        {
                        if (strstr(xx2,sana2)!=NULL)
                            {
                            t_rivi(x,lab0-dlab);
                            t_rivi(x2,lab0-dlab);
                            }
                        else
                            {
                            ++dlab;
                            if (*nimi3)
                                {
                                dt_rivi(x,labdel++);
                                dt_rivi(x2,0);
                                }
                            }
                        }
                        else
                        {
                        if (strstr(xx2,sana2)==NULL)
                            {
                            t_rivi(x,lab0-dlab);
                            t_rivi(x2,lab0-dlab);
                            }
                        else
                            {
                            ++dlab;
                            if (*nimi3)
                                {
                                dt_rivi(x,labdel++);
                                dt_rivi(x2,0);
                                }
                            }                        
                        }
                      
                      }
                  }
              if (lab2>=k2) valinta=2;
              break;
          case 2:
              t_rivi(x,lab-dlab); break;
            }
        }

    return(1);
    }


static int l_steprivi(int j,char *x)
    {

    if (!luettu)
        {
        if (feof(edt1)) return(-1);
        fgets(stx,LLENGTH,edt1);
        if (feof(edt1)) return(-1);
        steplab=atoi(stx);
        luettu=1;
        }
    if (j<steplab) *x=EOS;
    else { strcpy(x,stx); luettu=0; }
    return (1);
    }


static int del_by_steps()
    {
    int i,j;
    char x[LLENGTH];
// RS REM    int lab; //  0=varjorivi
// RS REM    int lab2;
    int dlab;
//    int lab0;
// RS REM    char *p;
// RS REM    int x0;
// RS REM    char x2[LLENGTH];
// RS REM    int talletus;
    int jstep=k1;

    dlab=0; // lab0=0;

    luettu=0;
    j=0;
    while (1)
        {
        ++j;
// Rprintf("\nj=%d ",j);
        i=l_steprivi(j,x); if (i<0) return(1);

        if (j==jstep && j>=k1 && j<=k2)
            {
            jstep+=step; ++dlab;
//          if (*x==EOS) { Rprintf("DEL (empty)"); getch(); }
//          else
            if (*x!=EOS)
                {
            //  Rprintf("DEL %.30s",x); getch();
                i=l_rivi(stx,&steplab); if (i<0) return(1);
                if (steplab!=0) { luettu=1; }
            //  else
            //      Rprintf("DEL %.30s"); getch();
                }
            }
        else
            {
//          if (*x==EOS) { Rprintf("(empty)"); getch(); }
//          else
            if (*x!=EOS)
                {
                t_rivi(x,steplab-dlab);
//              Rprintf("%.30s",x); getch();
                i=l_rivi(stx,&steplab); if (i<0) return(1);
                if (steplab!=0) { luettu=1; }
                else
                    {
                    t_rivi(stx,0);
//                  Rprintf("%.30s"); getch();
                    }
                }
            }


        } // while

    return(1);
    } 
			
static int op_linedel()
        {
        int i,j;
        char x[LLENGTH];
        char *s[4];
        char *p;
        char x2[LLENGTH];
        extern int s_init_orgsplit();

		extern char sur_session[]; 
		extern int muste_dumpcount,muste_dumpcountmax;       

// Rprintf("\nargv1=%s|",argv1); getch();

		s_init_orgsplit();

        if (g==2)
            {
            init_remarks();
            rem_pr("Usage: LINEDEL L1,L2 / deletes lines L1-L2.");
            rem_pr("       LINEDEL L1,L2,EMPTY / deletes empty lines from lines L1-L2.");
            rem_pr("       LINEDEL L1,L2,<string>");
            rem_pr("       deletes lines having any of characters in <string>");
            rem_pr("       in the control column from lines L1-L2.");
            rem_pr("       LINEDEL L1,L2,\"<string>\"");
            rem_pr("       deletes lines containing <string> from lines L1-L2.");
            rem_pr("       LINEDEL L1,L2,STEP,s");
            rem_pr("       deletes lines L1,L1+s,L1+2s,L1+3s,... from lines L1-L2.");
            rem_pr("       !LINEDEL deletes all but lines fulfilling the condition"); // RS ADD
            rem_pr("       from lines L1-L2."); // RS ADD
            wait_remarks(2);
            return(-1);
            }

        tyyli=0;
        if (g==1)
            {
            j=r1+r-1;
            while (1)
                {
                ++j;
                if (j>ed2) return(1);
                edread(sbuf,j);
                if (*sbuf=='*' && strncmp(sbuf+1,space,ed1-1)==0
                               && zs[j]==0 ) continue;
                break;
                }

            if (j==r1+r) return(1);
            last_empty=j-1;
            tyyli=5;
            }

        if (g>3)
            {
            edread(x,r1+r-1);
            split(x+1,s,4);
            i=s[3]-x;
            edread(x,r1+r-1);
            p=strstr(x," / "); if (p!=NULL) *p=EOS;
            p=x+i; j=strlen(p)-1; while (p[j]==' ') p[j--]=EOS;
            i=strlen(p)-1;

            if (muste_strcmpi(word[3],"EMPTY")==0)
                {
                tyyli=1;
                e_step=0;
                if (g>4) e_step=atoi(word[4]);
                }
            else if (muste_strcmpi(word[3],"STEP")==0)
                {
                tyyli=4; step=atoi(word[4]);
                if (step<=0) return(-2); // RS CHA exit(0);  // RS FIXME
                }
            else if (*p!='"' || p[i]!='"')
                {
                tyyli=2;
                strcpy(sana,p);
                }
            else if (*p=='"' && p[i]=='"')
                {
                tyyli=3;
                strcpy(sana,p+1); sana[strlen(sana)-1]=EOS;
                j=r1+r-1; *varjosana=EOS;
                if (zs[j]!=0)
                    {
                    edread(sbuf,zs[j]);
                    strncat(varjosana,sbuf+(p-x)+1,strlen(sana));
//   Rprintf("\nvarjo=%s|",varjosana);getch();

                    }
                }
            else
                {
                sur_print("\nError in LINEDEL command!");
                WAIT; return(1);
                }
            }
// Rprintf("\ntyyli=%d",tyyli); getch();
        if (tyyli==5)
            { k1=r1+r; k2=last_empty; }
        else
            {
            k1=edline2(word[1],1,1); if (!k1) return(1);
            k2=edline2(word[2],k1,1); if (!k2) return(1);
            }
            
    		strcpy(nimi1,etmpd); // RS 16.11.2012
    		strcat(nimi1,sur_session);
			sprintf(sbuf,"MUSTE%.2d.EDT",muste_dumpcount%muste_dumpcountmax); 
			strcat(nimi1,sbuf); 
			strcpy(nimi2,etmpd);
    		strcat(nimi2,sur_session);
			sprintf(sbuf,"MUSTD%.2d.EDT",muste_dumpcount%muste_dumpcountmax); 
			strcat(nimi2,sbuf);          
            
//        sprintf(nimi1,"%s/%sSURVOMM.EDT",etmpd,argv1); // RS CHA add tempdir
//        sprintf(nimi2,"%s/%sSURVOMD.EDT",etmpd,argv1);

        edt1=muste_fopen(nimi1,"rb");
        
        if (edt1==NULL) // RS ADD
           {
             sprintf(sbuf,"\nCannot open file %s!",nimi1);
             sur_print(sbuf);
             WAIT; return(1);
           }  
        
        edt2=muste_fopen(nimi2,"wb");
        if (edt2==NULL) // RS ADD
           {
             sprintf(sbuf,"\nCannot open file %s!",nimi2);
             sur_print(sbuf);
             WAIT; return(1);
           }  


        fgets(sbuf,LLENGTH,edt1); // RS CHA (int)
// Rprintf("\nsbuf=%s|",sbuf); // getch();
        fputs(sbuf,edt2);
        
        i=spec_find("DEL_SAVE",x,LNAME-1);
        if (i>0)
            {
            strcpy(nimi3,x);
            if (strchr(x,':')==NULL)
                { strcpy(nimi3,edisk); strcat(nimi3,x); }
            if (strchr(x,'.')==NULL) strcat(nimi3,".EDT");
            edt3=muste_fopen(nimi3,"wb");
     
            if (edt3==NULL) // RS ADD
               {
               sprintf(sbuf,"\nCannot open file %s!",nimi3);
               sur_print(sbuf);
               WAIT; return(1);
             }              
            
            fputs(sbuf,edt3);
            edread(x2,1); i=split(x2+1,s,2);
            if (i==2 && muste_strcmpi(s[0],"SAVE")==0) sprintf(sbuf," from %s",s[1]);
            else *sbuf=EOS;
            strcpy(x2,sbuf);

// rivinron painoasu!!!
            sprintf(sbuf,"00001|*SAVE %s / Deleted lines%s%s",
                                  x,x2,rivin_loppu);
            fputs(sbuf,edt3);
            }

// RS ADD
        linedel_reverse=FALSE;
        if (muste_strcmpi(word[0],"!LINEDEL")==0)
          {
          if (tyyli==0 || tyyli==1 || tyyli==4 || tyyli==5)
            {
            sur_print("\n!LINEDEL cannot be used with this style!");
            WAIT; return(1);
            }
          linedel_reverse=TRUE;
          }
        

        switch (tyyli)
            {
          case 0:
               del_all_lines(); break;
          case 1:
               del_empty_lines(); break;
          case 2:
               del_by_control_chars(); break;
          case 3:
               if (*varjosana==EOS) del_by_words();
               else                 del_by_words2();
               break;
          case 4:
               del_by_steps(); break;
          case 5:
               del_empty_lines(); break;
            }
        if (*nimi3) muste_fclose(edt3);
        i=muste_fclose(edt2);
        i=muste_fclose(edt1);
        i=sur_delete1(nimi1);
        i=sur_rename(nimi2,nimi1);
                
        return(1);
        }
				

/* FORM L1,L2,K */
/*      1  2  3 */
static void op_form()
        {
        int i,j,j1,j2,k,n;
        char ch;
        char miss[]="-";
        int h;

        if (g<4)
            {
            sur_print("\nUsage: FORM L1,L2,K");
            WAIT; return;
            }

        j1=edline2(word[1],1,1); if (j1==0) return;
        j2=edline2(word[2],j1,1); if (j2==0) return;
        k=edline2(word[3],1,1); if (k==0) return;

        fill=0;
        if (g>4)
            {
            if (muste_strnicmp(word[4],"L",1)==0) fill=1;
            if (muste_strnicmp(word[4],"R",1)==0) fill=2;
            }
        edread(maskline,k);
        n=split(maskline+1,fmask,64);
        if (n==0)
            {
            PR_EBLD;
            sprintf(sbuf,"\nMask missing on line %d",k); // RS CHA Rprintf -> sur_print
            sur_print(sbuf);           
            WAIT; PR_ENRM; return;
            }

        for (i=0; i<n; ++i)
            {
            pos[i]=fmask[i]-maskline-1;
            len[i]=strlen(fmask[i]);
            ch=*fmask[i];
            if ((ch>='A' && ch<='Z') || (ch>='a' && ch<='z'))
                type[i]='A'; else type[i]='N';
            }

        for (j=j1; j<=j2; ++j)
            {
            edread(x,j);
            i=split(x+1,form_sana,MAXCOL);
            if (i!=n)
                {
                if (fill)
                    {
                    if (fill==1)
                        for (h=i; h<n; ++h) form_sana[h]=miss;
                    else
                        {
                        for (h=n-1; h>=i; --h) form_sana[h]=form_sana[h-(n-i)];
                        for (h=0; h<n-i; ++h) form_sana[h]=miss;
                        }

                    }
                else
                    {
                    sprintf(sbuf,"\nIncorrect (%d) # of fields on line %d!",i,j);
                    sur_print(sbuf); WAIT; return;
                    }
                }

            strcpy(y,space);
            for (i=0; i<n; ++i)
                {
                if (type[i]=='A')
                    {
                    k=sprintf(y+pos[i],"%.*s",len[i],form_sana[i]);
                    y[pos[i]+k]=' ';
                    }
                else
                    {
                    if (form_sana[i][0]=='-' && (form_sana[i][1]=='-' || form_sana[i][1]==EOS))
                        {
                        k=len[i];
                        strncpy(luku,space,k);
                        luku[k-1]='-'; luku[k]=EOS;
                        }
                    else
                        fconv(atof(form_sana[i]),fmask[i],luku);
                    if (strlen(luku)>len[i])
                        {
                        sprintf(sbuf,"\nFormat %s not wide enough for %s on line %d",
                                        fmask[i],form_sana[i],j); sur_print(sbuf);
                        WAIT; return;
                        }
                    k=sprintf(y+pos[i],"%s",luku);
                    y[pos[i]+k]=' ';
                    }
                } /* i */

            edwrite(space,j,1);
            edwrite(y,j,1);
            } /* j */
        }


static void op_putend()
        {
        int i,j,j1,j2;
        char rivi[LLENGTH];
        char x[LLENGTH];
        char *p,*q;

        if (g<4)
            {
            sur_print("\nUsage: PUTEND L1,L2,<string>");
            WAIT; return;
            }
        j1=edline2(word[1],1,1); if (j1==0) return;
        j2=edline2(word[2],j1,1); if (j2==0) return;
        edread(rivi,r1+r-1);
        split(rivi+1,word,4);
        i=word[3]-rivi;
        edread(rivi,r1+r-1);
        p=rivi+i;
        q=p;
        while (*q!=' ') ++q; *q=EOS;
        strcpy(sbuf,p); chrconv(sbuf,p); // RS ADD 22.3.2013
        for (j=j1; j<=j2; ++j)
            {
            edread(x,j);
            i=c2;
            while (x[i]==' ' && i>0) --i;
            edwrite(p,j,i+1);
            }
        }


static void ei_tilaa()
            {
            PR_EBLD;
            sur_print("\nNot enough memory for SORT");
            WAIT; PR_ENRM;
            }


static int shadow_move(int j1,int j2,int len2)
        {
        int j,k;

        for (j=j1; j<=j2; ++j) zs2[j-j1]=zs[j];
        for (j=j1; j<=j2; ++j)
            {
            k=*(int *)(sortp[j-j1]+len2);
            zs[j]=zs2[k-j1];
            }
        return(1);
        }

static int shadow_move_s(int j1,int j2,int len2)
        {
        int j,k;

        for (j=j1; j<=j2; ++j) zs2[j-j1]=zs[j];
        for (j=j1; j<=j2; ++j)
            {
            k=atoi(sortp[j-j1]+len2)+j1;
            zs[j]=zs2[k-j1];
            }
        return(1);
        }

static int sort_conv(unsigned char *p,unsigned char *code) // RS REM unsigned
        {

        while (*p)
            {
            *p=(unsigned char)code[(unsigned char)*p];
            ++p;
            }
        return(1);
        }

/* 
static int numcomp (char **arg1,char **arg2)
        {
        double a;

        a=*(double *)(*arg1)-*(double *)(*arg2);
        if (a<0) return(-1);
        if (a==0) return(0);
        return(1);
        }
*/

static int numcomp(const void *a, const void *b) 
{ 
    double vert;
    const double **ia = (const double **)a; // casting pointer types 
    const double **ib = (const double **)b;
    vert=**ia -**ib;
    if (vert<0) return(-1);
    if (vert==0) return(0);
    return(1);    
} 

/*
static int stringcomp (char **arg1,char **arg2)
        {
        return(strcmp(*arg1,*arg2));
        }
*/
static int stringcomp(const void *a, const void *b) 
{ 

    const unsigned char **ia = (const unsigned char **)a;
    const unsigned char **ib = (const unsigned char **)b;
    return strcmp(*ia, *ib);    

//    return(strcmp(*(unsigned char **)a, *(unsigned char **)b));
} 

        
static void avaimet(int k,int *apos,int *alen,int *an)
        {
        char kirjain;
// RS REM        int i;
        char *p;
        char maskline[LLENGTH];

        edread(maskline,k);
        kirjain='A'-1;
        *an=0;
        while (*an<10 && kirjain<'z')
            {
            ++kirjain;
            p=strchr(maskline+1,kirjain);
            if (p==NULL) continue;
            apos[*an]=p-maskline;
            alen[*an]=0; while (*p==kirjain) { ++p; ++alen[*an]; }
            ++*an;
            }
        }


static int op_sort()
        {
// RS REM        int numcomp();
// RS REM        int stringcomp();
        int h,i,j,j1,j2,k,nmask;
        int pos,lenkey,negsort;
        double a;
        char maskline[LLENGTH], *mask[10];
        int sizeint;

        if (g<4)
            {
            sur_print("\nIncomplete SORT operation!");
            WAIT; return(-1);
            }

        i=spec_init(r1+r-1);
        if (i<0) return(-1);

        if (*word[0]=='-') negsort=1; else negsort=0;
        j1=edline2(word[1],1); if (j1==0) return(-1);
        j2=edline2(word[2],j1); if (j2==0) return(-1);
        k=edline2(word[3],1); if (k==0) return(-1);

        shadow=0;
        if (g>4 && (*word[4]=='S' || *word[4]=='s')) shadow=1;
        edread(maskline,k);
        nmask=split(maskline+1,mask,10);
        if (nmask==0)
            {
            PR_EBLD;
            sprintf(sbuf,"\nMask missing on line %d",k); sur_print(sbuf);
            WAIT; PR_ENRM; return(-1);
            }
  if (*mask[0]>='0' && *mask[0]<='9')
         {
        pos=mask[0]-maskline;
        sort_len=strlen(mask[0]);
        lenkey=sizeof(double)+sizeof(int);
        n=j2-j1+1; if (n<0) n=0;
        zz=muste_malloc(n*(ed1-1)+1);
        if (zz==NULL) { ei_tilaa(); return(-1); }
        sortp=(unsigned char **)muste_malloc(n*sizeof(char *)+1);
        if (sortp==NULL) { ei_tilaa(); return(-1); }
        sortlist=(unsigned char *)muste_malloc(n*lenkey+1);
        if (sortlist==NULL) { ei_tilaa(); return(-1); }
        if (shadow)
            {
            zs2=(int *)muste_malloc(n*sizeof(int)+1);
            if (zs2==NULL) { ei_tilaa(); return(-1); }
            }

        for (j=j1, k=0, h=0; j<=j2; ++j, k+=lenkey, h+=ed1-1)
            {
            edread(x,j);
            for (i=0; i<ed1-1; ++i) zz[h+i]=x[i+1];
            for (i=0; i<sort_len; ++i)
                luku[i]=x[pos+i];
            luku[sort_len]=EOS;

            if (negsort) a=-atof(luku); else a=atof(luku);
            *(double *)(sortlist+k)=a+(double)j*1e-10;
                                      /* tasatilanteessa alkup.j‰rjestys */

            *(int *)(sortlist+k+sizeof(double))=j;
            sortp[j-j1]=sortlist+k;
            }

        qsort((char *)sortp,n,sizeof(char *),numcomp);
        for (j=j1; j<=j2; ++j)
            {
            k=*(int *)(sortp[j-j1]+sizeof(double));
            for (i=0; i<ed1-1; ++i) z[(j-1)*ed1+i+1]=zz[(k-j1)*(ed1-1)+i];
            }
        if (shadow) shadow_move(j1,j2,sizeof(double));
        return(1);
         }

  else /* Aakkoslajittelu */
         {
        avaimet(k,apos,alen,&an);
        sort_len=0; for (i=0; i<an; ++i) sort_len+=alen[i];
        sprintf(sbuf,"%d",j2-j1); sizeint=strlen(sbuf);
        lenkey=sort_len+sizeint+1;
        n=j2-j1+1; if (n<0) n=0;
        zz=muste_malloc(n*(ed1-1)+1);
        if (zz==NULL) { ei_tilaa(); return(-1); }
        sortp=(unsigned char **)muste_malloc(n*sizeof(char *)+1);
        if (sortp==NULL) { ei_tilaa(); return(-1); }
        sortlist=(unsigned char *)muste_malloc(n*lenkey+1);
        if (sortlist==NULL) { ei_tilaa(); return(-1); }
        if (shadow)
            {
            zs2=(int *)muste_malloc(n*sizeof(int)+1);
            if (zs2==NULL) { ei_tilaa(); return(-1); }
            }
        i=load_codes("SORTCODE",code); if (i<0) return(-1);

        for (j=j1, k=0, h=0; j<=j2; ++j, k+=lenkey, h+=ed1-1)
            {
            edread(x,j);
            for (i=0; i<ed1-1; ++i) zz[h+i]=x[i+1];
            *luku=EOS;
            for (i=0; i<an; ++i) strncat(luku,x+apos[i],alen[i]);
            sort_conv(luku,code);
            for (i=0; i<sort_len; ++i) *(sortlist+k+i)=(unsigned char)luku[i];
            sprintf(sortlist+k+sort_len,"%*d",sizeint,j-j1);
/*
            *(int *)(sortlist+k+sort_len)=j-j1;
            *(sortlist+k+lenkey-1)=EOS;
*/
            sortp[j-j1]=sortlist+k;
            }

        qsort((unsigned char *)sortp,n,sizeof(char *),stringcomp);

        for (j=j1; j<=j2; ++j)
            {
            k=atoi(sortp[j-j1]+sort_len)+j1;
/*          k=*(int *)(sortp[j-j1]+sort_len)+j1; */
            for (i=0; i<ed1-1; ++i) z[(j-1)*ed1+i+1]=zz[(k-j1)*(ed1-1)+i];
            }

        if (shadow) shadow_move_s(j1,j2,sort_len);
        return(1);
         } /* Aakkoslajittelu */
        }

static void op_erase()
        {
        int i,j;
        char rivi[LLENGTH];
        char x[LLENGTH];
        char *p,*q;
        int j1,j2;

        if (g==1)
            {
            sur_print("\nUsage: ERASE xyz / x,y,z any characters");
            sur_print("\n   or  ERASE L1,L2,xyz");
            WAIT; return;
            }
        j1=1; j2=r2;
        edread(rivi,r1+r-1);
        j=split(rivi+1,word,4);
        if (j>3)
            {
            j1=edline2(word[1],1,1); if (j1==0) return;
            j2=edline2(word[2],1,1); if (j2==0) return;
            j=3;
            }
        else j=1;
        i=word[j]-rivi;
        edread(rivi,r1+r-1);
        p=rivi+i;
        while (*p!=' ')
            {
            for (j=j1; j<=j2; ++j)
                {
                edread(x,j);
                q=x;
                while ((q=strchr(q,*p))!=NULL) { *q=' '; ++q; }
                edwrite(x,j,0);
                }
            ++p;
            }
        }

static void change_lines()
        {
        int i;
        char lab[2];
        int xl1,xl2,yl1,yl2;
        char *z2;
        int *zs2;
        unsigned int size, rivi1, rivi2;
		xl1=xl2=yl1=yl2=0;
        lab[0]=word[1][0]; lab[1]=EOS;
        xl1=edline2(lab,1,1); if (xl1==0) return;
        xl2=edline2(lab,xl1+1,0);
        if (xl2==0) xl2=xl1;
        lab[0]=word[1][1]; lab[1]=EOS;
        yl1=edline2(lab,1,1); if (yl1==0) return;
        yl2=edline2(lab,yl1+1,0);
        if (yl2==0) yl2=yl1;
        if (xl1>yl1)
            {
            i=xl1; xl1=yl1; yl1=i;
            i=xl2; xl2=yl2; yl2=i;
            }
        if (yl1<xl2)
            {
            sur_print("\nOverlapping chapters!");
            sur_print("\nCannot be exchanged!");
            WAIT; return;
            }

        size=(unsigned int)ed1*(unsigned int)ed2;
        z2=muste_malloc(size);
        zs2=(int *)muste_malloc((ed2+1)*sizeof(int));
        if (z2==NULL || zs2==NULL)
            {
            sur_print("\nNot enough memory!");
            WAIT; return;
            }
        memcpy(z2,z,size);
        memcpy(zs2,zs,(ed2+1)*sizeof(int));
        rivi1=xl1;
        rivi2=yl1;
        memcpy(z+(rivi1-1)*ed1,z2+(rivi2-1)*ed1,(unsigned int)(yl2-yl1+1)*ed1);
        memcpy(zs+rivi1,zs2+rivi2,(yl2-yl1+1)*sizeof(int));
        rivi1+=yl2-yl1+1;
        rivi2=xl2+1;
        memcpy(z+(rivi1-1)*ed1,z2+(rivi2-1)*ed1,(unsigned int)(yl1-xl2-1)*ed1);
        memcpy(zs+rivi1,zs2+rivi2,(yl1-xl2-1)*sizeof(int));
        rivi1+=yl1-xl2-1;
        rivi2=xl1;
        memcpy(z+(rivi1-1)*ed1,z2+(rivi2-1)*ed1,(unsigned int)(xl2-xl1+1)*ed1);
        memcpy(zs+rivi1,zs2+rivi2,(xl2-xl1+1)*sizeof(int));
        }

static void change_columns()
        {
        int i,j,j1,j2,k;
        char x[LLENGTH];
        char y[LLENGTH];
        int x1,x2,y1,y2;
        char *p;
        int pos1,pos2;

        if (g<4)
            {
            sur_print("\nUsage: CHANGE L1,L2,<image line of form  XXXX  YY>");
            WAIT; return;
            }

        j1=edline2(word[1],1,1); if (j1==0) return;
        j2=edline2(word[2],j1,1); if (j2==0) return;
        k=edline2(word[3],1,1); if (k==0) return;
        edread(x,k);
        p=strchr(x,'X');
        if (p==NULL)
            {
            sprintf(sbuf,"\nMask XXXXX missing on line %s!",word[3]);
            sur_print(sbuf); WAIT; return;
            }
        x1=p-x;
        x2=ed1; while (x[x2]!='X') --x2;
        p=strchr(x,'Y');
        if (p==NULL)
            {
            sprintf(sbuf,"\nMask YYYYY missing on line %s!",word[3]);
            sur_print(sbuf); WAIT; return;
            }
        y1=p-x;
        y2=ed1; while (x[y2]!='Y') --y2;

        if (y1<x1) { i=x1; x1=y1; y1=i; i=x2; x2=y2; y2=i; }
        if (y1<x2)
            {
            sur_print("\nOverlapping XXX and YYY masks not permitted!");
            WAIT; return;
            }

        for (j=j1; j<=j2; ++j)
            {
            edread(x,j);
            strcpy(y,x);
            pos1=x1; pos2=y1;
            for (i=pos2; i<=y2; ++i) x[i+(pos1-pos2)]=y[i];
            pos1+=y2-y1+1; pos2=x2+1;
            for (i=pos2; i<y1; ++i) x[i+(pos1-pos2)]=y[i];
            pos1+=y1-x2-1; pos2=x1;
            for (i=pos2; i<=x2; ++i) x[i+(pos1-pos2)]=y[i];
            edwrite(x,j,0);
            if (!zs[j]) continue;
            edread(x,zs[j]);
            strcpy(y,x);
            pos1=x1; pos2=y1;
            for (i=pos2; i<=y2; ++i) x[i+(pos1-pos2)]=y[i];
            pos1+=y2-y1+1; pos2=x2+1;
            for (i=pos2; i<y1; ++i) x[i+(pos1-pos2)]=y[i];
            pos1+=y1-x2-1; pos2=x1;
            for (i=pos2; i<=x2; ++i) x[i+(pos1-pos2)]=y[i];
            edwrite(x,zs[j],0);
            }
        }

static void op_change()
        {
// RS REM        int i;
// RS REM        char x[LLENGTH];

        if (g<2)
            {
            sur_print("\nUsage:");
            sur_print("\n   CHANGE XY  / X and Y are line labels");
            sur_print("\n   or");
            sur_print("\n   CHANGE L1,L2,K  / K is mask line of form    XXXX   YYY");
            WAIT; return;
            }
        if (g==2) { change_lines(); return; }
        change_columns();
        }

static int ret()
		{ 
		s_end(argv1);
		return(0); // RS CHA exit(0);
		}
		

/*  !codes.c 13.10.1985/SM (11.8.1987) (30.10.1998)
    CODES LOAD <kooditiedosto>[,<tietue>]
    CODES SAVE <kooditiedosto>[,<tietue>]
    tietue=1,2,3,... oletus 1
    Kussakin tietueessa (n_bytes tavua) on merkkien 0-255 koodatut arvot.
 */


static int nimi_error()
   {
   sur_print("\nDo not use the same name for the output file!");
   WAIT; return(1);
   }


static int nim(char *nimi,char *pathname)
        {
        strcpy(pathname,nimi);
        if (strchr(nimi,':')==NULL)
            { strcpy(pathname,edisk); strcat(pathname,nimi); }
        return(1);
        }
 
static int load(int tietue)
        {
        int i,j,k,code,code2,h,ii,jj;
        char x[LLENGTH];

        codes=muste_fopen(nimi,"rb");  // -4.3.2001 word[2]
        if (codes==NULL)
            {
            sprintf(sbuf,"\nFile %s not found!",word[2]); sur_print(sbuf);
            WAIT; return(-1);
            }
        if (tietue>1) muste_fseek(codes,(long)((long)n_bytes*(long)(tietue-1)),0); 

        j=r1+r-1;
        for (code=0; code<n_bytes; ++code)
            {
            ++j;
            if (j>r2)
                {
                PR_EBLD;
                sur_print("\nNot space enough in the edit field!");
                WAIT; PR_ENRM; muste_fclose(codes); return(-1);
                }
            k=getc(codes);
            i=sprintf(x,"  %4d  %4d  %c",code,k,EOS);
            if (code%256<=32) code2='.'; else code2=code;
            if (k<=32) k='.';
            i=sprintf(x+i-1,"  %c  %c  %c",code2,k,EOS);
            edwrite(x,j,1);
            }
        jj=j;
        i=spfind("LINE");
        if (i>=0)
            {
            h=j; k=atoi(spb[i]);
            for (j=r1+r; j<=h-k; ++j)
                {
                for (ii=0; ii<k; ++ii)
                    x[ii]=z[(j-1+ii)*ed1+20];
                x[k]=EOS;
                edwrite(x,j,20);
                }
            }

        j=jj+1; if (j<=r2)
            { strcpy(x," input output"); edwrite(x,j,1); }

        muste_fclose(codes);
        return(1);

        }

 
 static int save(int tietue)
        {
        int i,j,code;
        char x[LLENGTH],*sana[2];

        codes=muste_fopen(nimi,"r+b"); // -4.3.2001 word[2]
        if (codes==NULL)
            codes=muste_fopen(word[2],"wb");
        if (codes==NULL) return(-1);

        if (tietue>1) muste_fseek(codes,(long)((long)n_bytes*(long)(tietue-1)),0);

        j=r1+r-1;
        for (code=0; code<n_bytes; ++code)
            {
            ++j;
            edread(x,j);
            i=split(x+1,sana,2);
            if (i<2 || atoi(sana[0])!=code)
                {
                PR_EBLD;
                sprintf(sbuf,"\nIncorrect line for code %d!",code);
                sur_print(sbuf); WAIT; PR_ENRM; muste_fclose(codes); return(-1);
                }
            i=atoi(sana[1]);
            if (i<0 || i>255)
                {
                PR_EBLD;
                sprintf(sbuf,"\nIncorrect code value %d on line %d",i,j);
                sur_print(sbuf); WAIT; PR_ENRM; muste_fclose(codes); return(-1);
                }
            putc(i,codes);
            }
        muste_fclose(codes);
        return(1);
        }


static int wsave(int tiet1,int tiet2)
        {
        int i,j,code,len;
        char x[LLENGTH],*sana[2];
        short sh;

        codes=muste_fopen(nimi,"r+b");
        if (codes==NULL)
            codes=muste_fopen(word[2],"wb");
        if (codes==NULL) return(-1);

        if (tiet1>0) muste_fseek(codes,(long)((long)sizeof(int)*(long)(tiet1-1)),0);
        if (tiet2==0) tiet2=1000000;
        len=tiet2-tiet1+1;
        j=r1+r-1;
        for (code=tiet1; code<len; ++code)
            {
            ++j;
            edread(x,j);
            i=split(x+1,sana,2);
            if (i<2 || atoi(sana[0])!=code) break;
            sh=atoi(sana[1]);
            fwrite(&sh,1,2,codes);
            }
        muste_fclose(codes);
        return(1);
        }


static int wload(int tiet1,int tiet2)
        {
        int j,code;
// RS REM int i,k,code,code2,h,ii,jj;
        int len;
        char x[LLENGTH];
        short sh;

        codes=muste_fopen(nimi,"rb");  // -4.3.2001 word[2]
        if (codes==NULL)
            {
            sprintf(sbuf,"\nFile %s not found!",word[2]); sur_print(sbuf);
            WAIT; return(-1);
            }
        if (tiet1>0) muste_fseek(codes,(long)((long)sizeof(int)*(long)(tiet1-1)),0); 

        if (tiet2==0) tiet2=1000000;

        len=tiet2-tiet1+1;

        j=r1+r-1;
        for (code=0; code<len; ++code)
            {
            ++j;
            if (j>r2)
                {
                PR_EBLD;
                sur_print("\nNot space enough in the edit field!");
                WAIT; PR_ENRM; muste_fclose(codes); return(-1);
                }
            fread(&sh,1,2,codes);
            if (feof(codes)) break;
            sprintf(x,"  %6d  %6d",code,sh);
            edwrite(x,j,1);
            }

        muste_fclose(codes);
        return(1);

        }
        
        
// CODES REMOVE old_file,new_file,
// 9.6.2008

// CODES COPY <old>,<new>,B1,B2
static int char_copy()
    {
    int i;
    long j1,j2,j;
    char nimi2[LNAME];

    codes=muste_fopen(nimi,"rb");
    if (codes==NULL)
        {
        sprintf(sbuf,"\nFile %s not found!",word[2]); sur_print(sbuf);
        WAIT; return(-1);
        }

    strcpy(sbuf,word[3]);
    subst_survo_path(sbuf);
    i=nim(sbuf,nimi2); if (i<0) return(-1);

    if (muste_strcmpi(nimi,nimi2)==0) { nimi_error(); return(1); }

    codes2=muste_fopen(nimi2,"wb");

    j1=atol(word[4]); j2=atol(word[5]);
// Rprintf("\nj1=%ld j2=%ld",j1,j2); getch();
    if (j1>0L) for (j=0L; j<j1; ++j) fgetc(codes);
    for (j=j1; j<=j2; ++j) fputc(fgetc(codes),codes2);

    muste_fclose(codes2);
    muste_fclose(codes);
    return(1);
    }

static int char_remove()
    {
    int i,step,h;
    long j1,j2,j;
    char nimi2[LNAME];

    codes=muste_fopen(nimi,"rb");
    if (codes==NULL)
        {
        sprintf(sbuf,"\nFile %s not found!",word[2]); sur_print(sbuf);
        WAIT; return(-1);
        }

    strcpy(sbuf,word[3]);
    subst_survo_path(sbuf);
    i=nim(sbuf,nimi2); if (i<0) return(-1);

    if (muste_strcmpi(nimi,nimi2)==0) { nimi_error(); return(1); }

    codes2=muste_fopen(nimi2,"wb");

    step=0;
    if (muste_strcmpi(word[4],"STEP")==0)
        {
        step=atoi(word[5]);
        h=0;
        while (!feof(codes))
            {
            i=fgetc(codes);
            if (i<0) break;
            ++h;
            if (h<step) fputc(i,codes2);
            else h=0;
            }
        return(1);
        }

    j1=atol(word[4]); j2=atol(word[5]);
// Rprintf("\nj1=%ld j2=%ld",j1,j2); getch();
    if (j1>0L) for (j=0L; j<j1; ++j) fputc(fgetc(codes),codes2);
    for (j=j1; j<=j2; ++j) fgetc(codes);
    while (!feof(codes))
        {
        i=fgetc(codes);
        if (i<0) break;
        if (ferror(codes)) break;
        fputc(i,codes2);
        }
    muste_fclose(codes2);
    muste_fclose(codes);
    return(1);
    }

static int op_codes()
        {
        int i;
        int tietue=1;
        int tietue2;

		tietue=1;
        if (g<3)
            {
            PR_EBLD;
            sur_print("\nCorrect form:");
            sur_print("\nCODES <LOAD/SAVE>,[code_file]");
            WAIT; PR_ENRM; return(1);
            }

        if (g>3) tietue=atoi(word[3]);
        strcpy(sbuf,word[2]);
        subst_survo_path(sbuf);
        i=nim(sbuf,nimi); if (i<0) return(-1);

        i=spec_init(r1+r-1); if (i<0) return(-1);
        n_bytes=256;
        i=spfind("BYTES");
        if (i>=0) n_bytes=atoi(spb[i]);

                                            // 9.6.2008
        if (muste_strcmpi(word[1],"REMOVE")==0) { i=char_remove(); return(i); }
        if (muste_strcmpi(word[1],"COPY")==0) { i=char_copy(); return(i); }
        if (muste_strcmpi(word[1],"SAVE")==0) { i=save(tietue); return(i); }
        if (muste_strcmpi(word[1],"LOAD")==0) { i=load(tietue); return(i); }

        tietue=0;                       // 26.10.2001
        if (g>3) tietue=atoi(word[3]);
        tietue2=0;
        if (g>4) tietue2=atoi(word[4]);
        if (muste_strcmpi(word[1],"WLOAD")==0) { i=wload(tietue,tietue2); return(i); }
        if (muste_strcmpi(word[1],"WSAVE")==0) { i=wsave(tietue,tietue2); return(i); }

        return(1);
        }
        
        
/*  cloadp.c 6.10.1985/SM (6.12.1991) (21.1.1997)
    LOADP, SAVEP
 */
 
static int openp(char s[],char mode[])
        {
        char filename[LNAME];
        char t[LNAME];

        strcpy(t,s);
        subst_survo_path(t);
        *filename=EOS;

        strcat(filename,t);
        text=muste_fopen(filename,mode);
        if (text==NULL)
            {
            PR_EBLD;
            sprintf(sbuf,"\nFile %s not found!",filename); sur_print(sbuf);
            WAIT; PR_ENRM; return(-1);
            }


        return(1);
        }

 static int tab_poisto(char *s)
        {
        char t[10*LLENGTH];
        char *p;
        int i;

        strcpy(t,s);
        p=t; i=0;
        while (*p)
            {
            if (*p==TAB)
                {
                int k=(int)(i/TABSPACE)*TABSPACE+TABSPACE-1;
                for (; i<=k; ++i) s[i]=' ';
                }
            else { s[i]=*p; ++i; }
            ++p;
            }
        s[i]=EOS;
        return(1);
        }

static int convert_load_codes(); // RS declaration
static int w_codes_load(int k)
    {
    char codefile[LNAME];

    strcpy(codefile,survo_path); strcat(codefile,"SYS/WIN.BIN"); // RS CHA \\ -> /
    convert_load_codes(codefile,code,k); // RS CHA load_codes -> convert_load_codes;
    return(1);
    }


static int op_loadr(char *file, int j)
        {
        int i,len;
        char x[LLENGTH];

        i=openp(file,"rt"); if (i<0) return(-1);
        --j;

        fgets(x,LLENGTH-2,text); // RS Skip first (empty) line from R output
        if (feof(text)) return(1);
        if (strlen(x)>1) rewind(text);

        while (1)
            {
            ++j;
            if (j>r2) break;
            fgets(x,LLENGTH-2,text);
            if (feof(text)) break;
            len=strlen(x); x[len-1]=EOS;
            muste_iconv(x,"CP850","");
            edwrite(space,j,1);
            edwrite(x,j,1);
            }
        return(1);
        }

static int op_loadp2()
        {
        int i,j,len;
        char x[LLENGTH];

        if (g<2) {
                   sur_print("\nUsage: LOADP2 <text file>");
                   WAIT; return(-1);
                 }

        i=openp(word[1],"rt"); if (i<0) return(-1);

        j=r1+r;
        if (g>2) { j=edline2(word[2],1,1); if (j==0) return(-1); }
        --j;

        while (1)
            {
            ++j;
            if (j>r2) break;
            fgets(x,LLENGTH-2,text);
            if (feof(text)) break;
            len=strlen(x); x[len-1]=EOS;
            edwrite(space,j,0);
            edwrite(x,j,0);
            fgets(x,LLENGTH-2,text);
            len=strlen(x); x[len-1]=EOS;
            if (*x==EOS) continue;
            if (zs[j]==0)
                {
                i=shadow_create(j); if (i<0) return(-1);
                }
            edwrite(space,zs[j],0);
            edwrite(x,zs[j],0);
            }
        return(1);
        }

static int op_loadp()
        {
        int k,riv,i;
        char *p; // RS CHECK unsigned?
        long jj,jj1,jj2;
        char use[LLENGTH];
        char skip[LLENGTH];
        char x[LLENGTH];
        long pituus,max_pituus;
        int len,ylitys;
        int split_lines=0;
        int rpit,riv1;
        int lev3=0;
        char *s[2];

        *use=EOS; *skip=EOS;
        i=spec_find("USE",use,LLENGTH-1);
        if (i==0)
            {
            i=spec_find("SKIP",skip,LLENGTH-1);
            }

        if (g<2) { PR_EBLD;
                   sur_print("\nCorrect form: LOADP <text file>");
                   sur_print("\nor            LOADW <text file>");
                   WAIT; return(-1);
                 }

        if (strncmp(word[1],"CLIP",4)==0 && strchr(word[1],'.')==NULL)
            {
            i=sur_load_clipboard(&clip);
            if (i<0) return(-1);
// Rprintf("\n%s",clip);
// getch();
            for (i=0; i<strlen(clip); ++i)
                if (clip[i]=='\15') clip[i]=' ';

            sprintf(clip_filename,"%sCLIP.TXT",etmpd); // RS CHA survo_path -> etmpd
            clip_file=muste_fopen(clip_filename,"w+t");
            fprintf(clip_file,"%s",clip);
            fprintf(clip_file,"\n");
            muste_fclose (clip_file);
// RS REM            muste_free(clip);
            word[1]=clip_filename;
            }

        i=spec_find("SPLIT",x,LLENGTH-1);
        if (i>=0 && *x)
            {
            if (muste_strnicmp(x,"SP",2)==0)
                {
                split_lines=3; lev3=c3;
                i=split(x,s,2);
                if (i==2) lev3=atoi(s[1]);
                if (lev3>c2) lev3=c2;
                }
            else if (*x=='-') split_lines=2;
            else split_lines=1;
            }

        if (codeconv)
            {
            if (codeconv!=999) // RS 14.3.2013
            w_codes_load(codeconv);
            }

        k=openp(word[1],"rt"); if (k<0) return(-1);
        jj1=1L; jj2=10000000L;
        if (g>3)
            {
            jj1=atol(word[2]);
            jj2=atol(word[3]);
            if (g>4)
                {
                riv=edline2(word[4],1);
                if (riv==0) return(-1);
                }
            else riv=r1+r;
            }
        else if (g>2)
            {
            riv=edline2(word[2],1);
            if (riv==0) return(-1);
            }
        else riv=r1+r;
        jj=0L; pituus=0L; max_pituus=0L; ylitys=0;
        riv1=riv;
        rpit=LLENGTH-1; if (split_lines) rpit=c2;

        if (split_lines==3) // 30.8.2002
          {
          rpit=lev3+1;
          *sbuf=EOS;
          while (!feof(text))
            {
//    Rprintf("\nsbuf=%s|",sbuf); getch();
            p=sbuf; while(*p==' ' && *p!=EOS) ++p;
            strcpy(rivi,p); p=rivi+strlen(rivi);
            while (p-rivi<rpit-1)
                {
                i=0;
                if (feof(text)) break;
                *p=(char)fgetc(text);

                if (*p==TAB) *p=' ';
                if (muste_unix && *p=='\12') { i=1; *p=EOS; *sbuf=EOS; break; }
                else if (*p=='\n') { i=1; *p=EOS; *sbuf=EOS; break; }
                ++p;
                }
            if (feof(text)) break;

            ++jj; // 17.5.2006
            if (jj<jj1) continue;
            if (jj>jj2) break;

            if (i==0)
                {
                if (p-rivi==rpit-1)
                    {
                    p=rivi+rpit-1; *p=EOS;
// Rprintf("\nrivi=%s|",rivi); getch();
                    while (p>rivi && *p!=' ') --p;
                    if (p>rivi) { strcpy(sbuf,p+1); *(p+1)=EOS; }
                    else *sbuf=EOS;
                    }
                else { *(p+1)=EOS; *sbuf=EOS; }
                }
// Rprintf("\ni=%d rivi=%s|",i,rivi); getch();
            len=strlen(rivi);
            if (codeconv)
                if (codeconv==999) // RS 14.3.2013
                    {
                    i=muste_iconv(rivi,"CP850",muste_encoding);
                    if (i<0) codeconv=0;
                    }
                else    
                for (i=0; i<len; ++i)
                     rivi[i]=(unsigned char)code[(unsigned char)rivi[i]]; // RS CHA char)rivi[i]=code[(unsigned char)rivi[i]];
            if (riv>r2)
                {
                sur_print("\nNot enough lines in the edit field!");
                if (etu==0) { WAIT; }
                muste_fclose(text); return(-1);
                }
            edwrite(space,riv,1);
            edwrite(rivi,riv++,1);
            if (feof(text)) break;
            }
          muste_fclose(text);
          return(1);
          } // split_lines=3

        while (fgets(rivi,rpit,text)!=NULL)
            {
            tab_poisto(rivi);
            len=strlen(rivi);   /* 21.1.1997 */
            if (codeconv)
                if (codeconv==999) // RS 14.3.2013
                    {
                    i=muste_iconv(rivi,"CP850",muste_encoding);
                    if (i<0) codeconv=0;
                    }
                else                
                for (i=0; i<len; ++i)
                    rivi[i]=(unsigned char)code[(unsigned char)rivi[i]]; // RS CHA (unsigned char)rivi[i]=code[(unsigned char)rivi[i]];

/*
if (split_lines) { Rprintf("len=%d\n",len); getch();
                   Rprintf("rivi=%s\n",rivi); getch();
                 }
*/
            if (!split_lines && len>c2+1)
                {
                sprintf(sbuf,"\nLine %ld:",jj+1); sur_print(sbuf);
                sprintf(sbuf,"\n%s",rivi); sur_print(sbuf);
                sur_print("\n- - - - - - - - - - - - - - - -");
                sur_print("\nis too long!");
                sur_print("\nNot all lines loaded.");
                sur_print("\nUse LOADP or LOADW with SPLIT=- or SPLIT=1 or SPLIT=SP,<width> .");
                sur_print("\nThen long lines will be divided.");

                WAIT; return(-1);
                }
            pituus+=len;

            if (muste_unix && rivi[len-1]!='\12' && !feof(text))
                {
                ylitys=1;
                }
            else if (rivi[len-1]!='\n' && !feof(text))
                {
                ylitys=1;
                }
            else
                {
                if (pituus>max_pituus) max_pituus=pituus;
                pituus=0L; ylitys=0;
                }

            ++jj;
            if (jj<jj1) continue;
            if (jj>jj2) break;
            if (*use)
                {
                while (1)
                    {
                    if (riv>r2) break;
                    edread(x,riv);
                    if (strchr(use,*x)!=NULL) break;
                    ++riv;
                    }
                }
            else if (*skip)
                {
                while (1)
                    {
                    if (riv>r2) break;
                    edread(x,riv);
                    if (strchr(skip,*x)==NULL) break;
                    ++riv;
                    }
                }
            if (riv>r2)
                {
                PR_EBLD;
                sur_print("\nNot enough lines in the edit field!");
                if (etu==0) { WAIT; }
                PR_ENRM; muste_fclose(text); return(-1);
                }
            edwrite(space,riv,1);
            if ( (p=strchr(rivi,'\n'))!=NULL ) *p=EOS;
/*
   Rprintf("\nrivi %d=",riv);
   Rprintf("\n%s\n",rivi);
   for (k=0; k<strlen(rivi); ++k) Rprintf("%d ",rivi[k]); getch();
*/

            if (split_lines==2 && riv>riv1 && *rivi && *rivi!=' ')
                {
                edread(x,riv-1);
                if (x[c2-1]!=' ')
                    { x[c2]='-'; edwrite(x,riv-1,0); }
                }
            edwrite(rivi,riv++,1);
            } /* while (fgets(...)) */
        if (ylitys && !etu) /* 21.1.1997 */
            {
            if (max_pituus==0L)
                sprintf(sbuf,"\nObviously not a text file!");
            else
                sprintf(sbuf,"\nToo long lines (max %ld characters)!",max_pituus-1L);
            sur_print(sbuf);
            WAIT;
            }

        muste_fclose(text);
        return(1);
        }


static int op_savep(int shad)   /* SAVEP <text file>,L1,L2 */
        {
        int i,k;
        int j,j1,j2;
        int extra_space; // 1.10.2002

        if (g!=2 && g!=4)
            {
            sur_print("\nCorrect form: SAVEP <text file>");
            sur_print("\nor            SAVEP L1,L2,<text file>");
            WAIT; return(-1);
            }

        if (codeconv)
            {
            if (codeconv!=999) // RS 14.3.2013
            w_codes_load(codeconv);
            }

        extra_space=0;
        i=spec_find("EXTRA_SPACE",sbuf,LLENGTH-1);
        if (i>=0) extra_space=atoi(sbuf);

        if (g==2) i=1; else i=3;
        if (muste_unix) k=openp(word[i],"wb");
        else k=openp(word[i],"wt");
        if (k<0) return(-1);
        j1=r1+r; j2=lastline2();
        if (g==4)
            {
            j1=edline2(word[1],1,1); if (j1==0) return(-1);
            j2=edline2(word[2],j1,1); if (j2==0) return(-1);
            }
        for (j=j1; j<=j2; ++j)
            {
            edread(rivi,j);
            k=strlen(rivi)-1;
            while (k>0 && rivi[k]==' ') --k;
            if (extra_space) // 1.10.2002
                {
                rivi[k+1]=EOS;
                strncat(rivi,space,extra_space);
                k+=extra_space;
                }
            if (muste_unix) rivi[k+1]='\12'; else rivi[k+1]='\n'; rivi[k+2]=EOS;
            if (codeconv)
                if (codeconv==999) // RS 14.3.2013
                    {
                    i=muste_iconv(rivi,muste_encoding,"CP850");
                    if (i<0) codeconv=0;
                    }
                else                
                for (i=0; i<k+1; ++i)
                    {
                    rivi[i]=(unsigned char)code[(unsigned char)rivi[i]]; // RS CHA (unsigned char)rivi[i]=code[(unsigned char)rivi[i]];
                    }
            fputs(rivi+1-shad,text);
            if (ferror(text))
                {
                sur_print("\nCannot save the file!");
                WAIT;
                break;
                }
            if (shad)
                {
                *rivi=EOS;
                if (zs[j]) edread(rivi,zs[j]);
                k=strlen(rivi)-1;
                while (k>0 && rivi[k]==' ') --k;
                if (muste_unix) rivi[k+1]='\12'; else rivi[k+1]='\n'; rivi[k+2]=EOS;
                fputs(rivi,text);
                }
            }

        muste_fclose(text);
        return(1);
        }

static int convert_load_codes(char *codefile,char *code,int col)
        {
        int i;
        char x[LLENGTH];

        strcpy(x,codefile);
        if (strchr(x,':')==NULL && *x!='.' && *x!='/') // RS ADD /
            { strcpy(x,survo_path); strcat(x,"SYS/"); strcat(x,codefile); } // RS CHA \\ -> /

        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nCode conversion file %s not found!",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (col>1) muste_fseek(codes,(long)(col-1)*256L,SEEK_SET);
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose(codes);
        return(1);
        }

static void op_convert()
        {
        int i,j,j1,j2;
        char *codefile;
        char x[LLENGTH]; // RS REM unsigned
        int col;

        if (g<4)
            {
            sur_print("\nCorrect form:");
            sur_print("\nCONVERT L1,L2,<code_file>[,#]");
            WAIT; return;
            }
        j1=edline2(word[1],1,1); if (j1==0) return;
        j2=edline2(word[2],j1,1); if (j2==0) return;
        codefile=word[3];
        col=1;
        if (g>4) { col=atoi(word[4]); if (col<=0) col=1; }

        i=convert_load_codes(codefile,code,col);
        if (i<0) return;

        for (j=j1; j<=j2; ++j)
            {
            edread(x,j);
            for (i=0; i<ed1; ++i) x[i]=code[(int)x[i]];
            edwrite(x,j,0);
            }
        }
        
static int n_strcpy(char *nimi,char *par)
    {
    char *p;

    p=strchr(par,':');
    if (p==NULL)
        {
        strcpy(nimi,edisk); strcat(nimi,par);
        }
    else strcpy(nimi,par);
    return(1);
    }

static int not_open(char *nimi)
    {
    sprintf(sbuf,"\nCannot open file %s !",nimi);
    sur_print(sbuf); WAIT; return(1);
    }

        

static int op_ncopy()
        {
// RS REM        int i;
        long c,n; // RS n to local

        if (g<4)
            {
            sur_print("\nUsage: NCOPY <file>,<new_file>,N");
            sur_print("\nNCOPY simply makes a copy of the first N bytes in the file.");
            WAIT; return(1);
            }
        n_strcpy(nimi1,word[1]);
        n_strcpy(nimi2,word[2]);
        n=atol(word[3]);
        tied1=muste_fopen(nimi1,"rb");
        if (tied1==NULL) { not_open(nimi1); return(1); }
        tied2=muste_fopen(nimi2,"wb");
        if (tied2==NULL) { not_open(nimi1); return(1); }
        for (c=0L; c<n; ++c)
            {
            putc(getc(tied1),tied2);
            }
        muste_fclose(tied1);
        muste_fclose(tied2);
        return(1);
        }

/* !update.c 3.7.1988/SM (3.7.1988)
   UPDATE L1,L2,<key_field_length>,<edit_file>
*/

static void qedread(char *s,int j)
        {
        int i;

        muste_fseek(edfile,(long)(j*qed1),0);
        for (i=0; i<qed1; ++i) s[i]=(char)getc(edfile);
        s[qed1]=EOS;
        }

static void qedsave(char *s,int j)
        {
        int i;

        muste_fseek(edfile,(long)(j*qed1),0);
        for (i=0; i<qed1; ++i) putc((int)s[i],edfile);
        }

static int update_lue()
        {
        int i;
        unsigned int paikka;

        field2=muste_malloc(qed1*qed2);
        if (field2==NULL)
            {
            sur_print("\nNot enough memory!"); WAIT; return(-1);
            }

        paikka=0;
        for (i=1; i<=qed2; ++i)
            { qedread(field2+paikka,i); paikka+=qed1; }
        return(1);
        }

static void update_talleta()
        {
        int i;
        unsigned int paikka;

        paikka=0;
        for (i=1; i<=qed2; ++i)
            { qedsave(field2+paikka,i); paikka+=qed1; }
        }


static int update_avaa(char *edq)    /* lainattu kyselysysteemist‰ cq.c */
        {
        int i;
        char rivi[ELE], *sana[3];

        edfile=muste_fopen(edq,"r+b");
        if (edfile==NULL)
            {
            sprintf(sbuf,"\nFile of measures %s is not found!",edq);
            sur_print(sbuf); WAIT; return(-1);
            }
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(edfile);
        rivi[ELE-1]=EOS;
        i=split(rivi,sana,3);
        qed1=atoi(sana[1]); qed2=atoi(sana[2]);
        return(1);
        }

static void op_update()
        {
        int i,l,k;
        char x[LLENGTH];
// RS REM        char y[LLENGTH];
// RS REM        char key[LLENGTH],vert[LLENGTH];

        if (g<5)
            {
            sur_print("\nUsage: UPDATE L1,L2,<key_field_length>,<edit_file>");
            WAIT; return;
            }
        l1=edline2(word[1],1,1); if (l1==0) return;
        l2=edline2(word[2],1,l1); if (l2==0) return;
        keylen=atoi(word[3]);
        strcpy(name,word[4]);
        subst_survo_path(name);
        if (*name!='.' && strchr(name,':')==NULL) { strcpy(name,edisk); strcat(name,word[4]); }
        if (strchr(name+strlen(name)-4,'.')==NULL) strcat(name,".EDT");
        i=update_avaa(name); if (i<0) return;
        i=update_lue(); if (i<0) return;

        sur_print("\n");
        for (l=l1; l<=l2; ++l)
            {
            unsigned int paikka;

            edread(x,l);
            paikka=0;
            for (k=1; k<=qed2; ++k)
                {
                if (strncmp(x+1,field2+paikka+1,keylen)!=0)
                    {
                    paikka+=qed1; continue;
                    }
                strncpy(field2+paikka,x,qed1);
                sprintf(sbuf,"%.*s ",keylen,x+1); sur_print(sbuf);
                break;
                }
            if (k>qed2)
                {
                sprintf(sbuf,"\nCannot find line %.*s in %s!\n",keylen,x+1,name);
                sur_print(sbuf); WAIT;
                }
            }
        update_talleta();
        muste_fclose(edfile);
        }



/* TXT.C 22.8.1992/SM (22.12.1995) (1.6.1996)
   TXT operations: TXTCONV, TXTRIM, TXTCOUNT, TXTEDTOUT, TXTEDTIN
*/

static int ei_samaan()
        {
        sur_print("\nThe converted file must not be the same as the original!");
        WAIT; return(-1);
        }

static int b_conversion()
        {
        int i;
        unsigned char ch1;
/*
        Rprintf("%d %d %s\n",b_start,b_step,textspace); getch();
*/
        for (i=0; i<b_start; ++i)
            ch1=(unsigned char)getc(txt1);
        while (1)
            {
            for (i=0; i<b_step; ++i)
                {
                ch1=(unsigned char)getc(txt1);
                if (feof(txt1)) break;
                putc((int)ch1,txt2);
                }
            if (feof(txt1)) break;
            for (i=0; i<strlen(textspace); ++i)
                putc((int)textspace[i],txt2);
            }
        return(1);
        }

static int textlimit_missing(unsigned int j)
        {
        sprintf(sbuf,"\nText limit character `%c' missing on edit line %d!",
                            textlimit,j);
        sur_print(sbuf); WAIT;
        return(1);
        }

static int muunnos()
        {
        unsigned char ch,ch1,ch2;
        int i,k;
//        long pos;
        int ok,ok2;
        char *p; // RS REM unsigned
        int n;
        long ln;
        int prind;

        prind=0;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        sur_print("\nCharacters read:"); n=0; ln=0L;
        while (1)
            {
            ch1=(unsigned char)getc(txt1);
            if (feof(txt1)) break;
            ++n;
            if (n==1000)
                {
                ln+=n;
                n=0;
/****************************************
                if (kbhit())
                    {
                    i=getch(); if (i=='.') exit(1);
                    }
*****************************************/
                if (prind)
                    {
                    sprintf(sbuf," %ld",ln); sur_print(sbuf);
                    }
                }
            ok2=0;
            for (i=0; i<nc; ++i)
                {
                if (type[i]=='T')
                    {
                    ok=0;
                    if (ch1!=*text1[i]) continue;
                    k=0; muste_ftell(txt1); // pos=muste_ftell(txt1);
                    while (1)
                        {
                        ++k;
                        ch=text1[i][k];
                        if (ch==EOS)
                            {
                            p=text2[i];
                            while (1)
                                {
                                if (*p==EOS) break;
                                putc((int)*p,txt2);
                                ++p;
                                }
                            ok=1; break;
                            }
                        ch2=(unsigned char)getc(txt1);
                        if (feof(txt1))
                            {
                            putc((int)'\n',txt2);
                            return(1);
                            }
                        if (ch2!=ch) break;
                        }
                    if (!ok) { muste_fseek(txt1,(long)-k,SEEK_CUR); continue; }
                    ok2=1;
                    }
                if (ok2) break;
                if (ch1==(unsigned char)char1[i]) break;
                } /* i */
            if (ok2) continue;
            if (i==nc)
                {
                putc((int)ch1,txt2);
                }
            else
                {
                if (type[i]=='D') continue;
                else putc((int)char2[i],txt2);
                }
            }
        return(1);
        }

static int g_conversion(char infile[],char outfile[]) // RS 24.1.2013
	{
	int i,lines,maxfilesize,perl;
	
	strcpy(line1,infile); muste_expand_path(line1);
	strcpy(line2,outfile); muste_expand_path(line2);

	lines=1; maxfilesize=10000000; perl=0;
	i=spfind("BY_LINES"); if (i>=0) lines=atoi(spb[i]);	
	i=spfind("MAXSIZE"); if (i>=0) maxfilesize=atoi(spb[i]);
	i=spfind("PERL"); if (i>=0) perl=atoi(spb[i]);
	
	sprintf(rivi,".muste.txtconv('%s','%s',c(",line1,line2);
	for (i=0; i<nc; i++)
		{
		if (i>0) strcat(rivi,",");
		strcat(rivi,"'");
		strcat(rivi,text1[i]);
		strcat(rivi,"'");
		}
	strcat(rivi,"),c(");
	for (i=0; i<nc; i++)
		{
		if (i>0) strcat(rivi,",");
		strcat(rivi,"'");
		strcat(rivi,text2[i]);
		strcat(rivi,"'");
		}	
	sprintf(sbuf,"),lines=%d,MAXSIZE=%d,perl=%d)",lines,maxfilesize,perl);
	strcat(rivi,sbuf);	
	i=muste_evalr(rivi);
	if (i<0)
		{
        sprintf(sbuf,"\nTXTCONV conversion g failed!");
        sur_print(sbuf); WAIT;
        return(-1);
		}
	return(1);
	}


static int c_conversion() // SM ADD 26.5.2012
    {
    unsigned char ch;
    int c3_ind=0;

// printf("\nc_1=%c c_2=%c c_3=%c",c_1,c_2,c_3); getch();

    while (!feof(txt1))
        {
        ch=(unsigned char)getc(txt1);
        if (ch==c_[3]) c3_ind=1-c3_ind;
        if (c3_ind==0)
            if (ch==c_[1]) ch=c_[2];
        putc((int)ch,txt2);
        }
    return(1);
    }

static int conv_list()
        {
        int i,j,param;
        char x[LLENGTH], *w[4]; // RS REM unsigned
        unsigned char ch;
        char *p,*q; // RS REM unsigned
        int ttype;
        char y[LLENGTH]; // RS REM unsigned

		param=0; // RS ADD init
		w[0]=w[1]=w[2]=w[3]=x; // RS ADD init
        ptext=textspace;
        j=r1+r-1;

		        
         if ((i=spfind("CONVERSIONS"))>=0) // RS 23.1.2013
         	{
         	j=wfind("CONVERSIONS",spb[i],1)-1; 
			if (j<0)
				{
				sprintf(sbuf,"\nCONVERSIONS list %s not found!",spb[i]);
				sur_print(sbuf); WAIT; return(-1);
				}
			}


        while (j<r2)
            {
            ++j;
            edread(x,j);
            param=i=split(x+1,w,1);
            if (i==0) continue;
            if (strncmp(w[0],"CONVERSIONS",11)==0) break;
            }
        if (j==r2)
            {
            sur_print("\nCONVERSIONS list not found!");
            WAIT; return(-1);
            }
        nc=0;
        g_conv=0; // RS 24.1.2013
        while (j<r2)
            {
            ++j;
            edread(x,j);
            param=i=splitp(x+1,w,4);   // SM CHA 26.5.2012 3->4             
            if (i==0) return(1);
            if (strcmp(w[0],"END")==0) return(1);
            type[nc]=*w[0];                        
            b_conv=0;
            if (type[nc]=='B')
                {
                if (nc>0)
                    {
                    sur_print("\nNo other conversions with B conversion!");
                    WAIT; return(-2); // RS CHA exit(0);  // RS FIXME
                    }
                edread(x,j);
                param=i=splitp(x+1,w,4);
                if (i<4) // RS ADD
                	{
                	sur_print("\nNot enough parameters in conversion!");
                    WAIT; return(-2);
                	}
                b_conv=1;
                b_start=atoi(w[1]);
                b_step=atoi(w[2]);
                i=strlen(w[3]); w[3][i-1]=EOS;  /* "  " pois */
                chrconv(w[3]+1,textspace);
                }
            c_conv=0; // RS ADD 13.7.2012    
            if (type[nc]=='C') // SM ADD 26.5.2012
                {
                if (nc>0)
                    {
                    sur_print("\nNo other conversions with C conversion!");
                    WAIT; return(-1);
                    }
                edread(x,j);
                param=i=splitp(x+1,w,4);
                if (i<4)
                    {
                    sur_print("\nUsage: C c1 c2 c3");
                    sur_print("\nReplace char c1 by char c2 unless c1 is located");
                    sur_print("\nin a substring starting and ending by char c3.");
                    WAIT; return(-1);
                    }
                *sbuf=EOS; i=chrconv(w[1],sbuf); if (i<0) return(-1); c_[1]=*sbuf;
                *sbuf=EOS; i=chrconv(w[2],sbuf); if (i<0) return(-1); c_[2]=*sbuf;
                *sbuf=EOS; i=chrconv(w[3],sbuf); if (i<0) return(-1); c_[3]=*sbuf;
                c_conv=1;
                return(1);
                }
             if (type[nc]!='g') g_conv=0; // RS 24.1.2013
             if (type[nc]=='g') // RS 24.1.2013
                {
                if (nc>0 && g_conv==0)
                    {
                    sur_print("\nNo other type of conversions with g conversions!");
                    WAIT; return(-2);
                    }
                }                     
                                
            if (type[nc]=='T' || type[nc]=='t' || type[nc]=='g') // RS ADD 24.1.2013 case 'g'
                {
                if (type[nc]=='g')
                	{
                	g_conv=1;
                	ttype=0;
                	}
                else
                	{
					if (type[nc]=='T') ttype=0; else ttype=1;
					type[nc]='T';
                	}
                edread(x,j);
                p=strchr(x+2,textlimit);
                if (p==NULL) { textlimit_missing(j); return(-1); }
                ++p; q=strchr(p,textlimit);
                if (q==NULL) { textlimit_missing(j); return(-1); }
                *q=EOS;
                if (ttype) { i=chrconv(p,y); if (i<0) return(-1); p=y; }
                strcpy(ptext,p); text1[nc]=ptext; ptext+=strlen(p)+1;
                p=strchr(q+1,textlimit);
                if (p==NULL) { textlimit_missing(j); return(-1); }
                ++p; q=strchr(p,textlimit);
                if (q==NULL) { textlimit_missing(j); return(-1); }
                *q=EOS;
                if (ttype) { i=chrconv(p,y); if (i<0) return(-1); p=y; }
                strcpy(ptext,p); text2[nc]=ptext; ptext+=strlen(p)+1;
                ++nc;
                continue;
                }
			if (param>=2) // RS ADD
				{
            	read_char(w[1],&ch);
            	char1[nc]=ch;
            	}
			if (param>=3) // RS ADD
				{
            	read_char(w[2],&ch);
            	char2[nc]=ch;
            	}            
            ++nc;
            }
        return(1);
        }
        
/* txtrim.c 18.10.1994/SM (18.10.1994) (30.3.1996)
*/

static int tr_muunnos()
       {
       unsigned char ch;
       int i,k;
       long n;
       int prind;

       prind=0;
       i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
       if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);       

       i=0; n=0L;
       sur_print("\n");
       while (!feof(txt1))
           {
           ch=(unsigned char)getc(txt1);
           if (ch=='\n')
               {
               ++n;
               if (prind)
               {
               sprintf(sbuf,"%ld ",n); sur_print(sbuf);
               }
               if (i<pituus)
                    for (k=0; k<pituus-i; ++k)
                        putc((int)merkki,txt2);
                putc((int)'\n',txt2);
                i=0; continue;
                }
            ++i;
            if (i>pituus)
                {
          sprintf(sbuf,"\nLine %ld is too long (more than %d characters)!",(long)(n+1),pituus);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (feof(txt1)) break;
            putc((int)ch,txt2);
            }
       return(1);
       }

static int tr_avaa(char *nimi,FILE **ptxt,char *moodi,char *polkunimi)
        {
        char name[LLENGTH];
        FILE *txt;

        if (!muste_is_path(nimi)) 
            { strcpy(name,edisk); strcat(name,nimi); }
        else strncpy(name,nimi,LLENGTH); // RS 13.3.2013
        if (muste_strcmpi(name,polkunimi)==0) { ei_samaan(); return(-1); }
        *ptxt=txt=muste_fopen(name,moodi);
        if (txt==NULL)
            {
            sprintf(sbuf,"\nCannot open text file %s!",name);
            sur_print(sbuf); WAIT; return(-1);
            }
        strcpy(polkunimi,name);
        return(1);
        }
        
/* txtoutin.c 1.6.1996/SM (1.6.1996)
*/
static int tr_avaa2(char *nimi,char *extension,FILE **ptxt,char *moodi)
        {
        char name[LLENGTH];
        char *p;

        if (!muste_is_path(nimi)) 
            { strcpy(name,edisk); strcat(name,nimi); }
        else strncpy(name,nimi,LLENGTH); // RS 13.3.2013
        if (*extension)
            {
            p=strchr(nimi,'.');
            if (p==NULL) strcat(name,extension);
            }
        *ptxt=muste_fopen(name,moodi);
        if (*ptxt==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s!",name);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(1);
        }


static int op_txtedtout()
        {
        int i,j;
        char x[LLENGTH],*osa[3];

        if (g<3)
            {
            init_remarks();
            rem_pr("TXTEDTOUT <edt_file>,<ascii_file>");
            rem_pr("copies an edit file (edit field in an .EDT file)");
            rem_pr("to an ascii (text) file. Also the control column and");
            rem_pr("shadow lines are copied.");
            rem_pr("The ascii file can then be sent through a network");
            rem_pr("to another Survo installation where it can be transformed");
            rem_pr("back to an edit file by the command");
            rem_pr("TXTEDTIN <ascii_file>,<edt_file> .");
            rem_pr("");
            rem_pr("If only text (neither the control column nor shadow lines)");
            rem_pr("is to be sent, the standard SAVEP command should be used.");
            wait_remarks(2);
            return(1);
            }
        i=tr_avaa2(word[1],".EDT",&txt1,"rb"); if (i<0) return(1);
        i=tr_avaa2(word[2],"",&txt2,"wt"); if (i<0) return(1);

        for (i=0; i<64; ++i) x[i]=(char)getc(txt1); x[64]=EOS;
        if (strncmp(x,"SURVO84",7)!=0)
            {
            sur_print("\nOnly SURVO 84C edit files accepted!");
            if (strncmp(x,"SURVO 98",8)==0)
                {
                sprintf(sbuf,"\n%s as a SURVO 98 edit file is a standard text file.",
                         word[1]); sur_print(sbuf);
                }
            WAIT; return(-2); // RS CHA exit(1); // RS FIXME
            }
        i=split(x,osa,3);
        lev=atoi(osa[1]);
        n=atoi(osa[2]);

        fprintf(txt2,"--- Edit file %s of SURVO 84C converted to ASCII format ---\n",
                         word[1]);
        rewind(txt1);
        for (j=0; j<=n+1; ++j)
            {
            fread(x,1,lev,txt1); x[lev]=EOS;
            i=lev-1; while (i>=0 && x[i]==' ') x[i--]=EOS;
            fprintf(txt2,"%s\n",x);
            }
        if (strncmp(x,"END",3)==0) return(1);
        while (1)
            {
            fread(x,1,lev,txt1); x[lev]=EOS;
            if (strncmp(x,"END",3)==0) { fprintf(txt2,"END\n"); break; }
            sprintf(sbuf,"%d",(int)((unsigned char)*x)+(int)(256*(unsigned char)*(x+1))); // RS 4.2.2013 CHA *(int *)x);
            i=lev-1; while (i>=sizeof(int) && x[i]==' ') x[i--]=EOS;
            fprintf(txt2,"%s:%s\n",sbuf,x+2);
            }
        return(1);
        }

static int op_txtedtin()
        {
        int i,j;
        char x[LLENGTH+10],*osa[3];
        char y[LLENGTH+10];
        char *p,*q;

        if (g<3)
            {
            init_remarks();
            rem_pr("TXTEDTIN <ascii_file>,<edt_file>");
            rem_pr("restores <edt_file> from an <ascii_file> made by");
            rem_pr("TXTEDTOUT <edt_file>,<ascii_file> .");
            wait_remarks(2);
            return(1);
            }
        i=tr_avaa2(word[2],".EDT",&txt1,"wb"); if (i<0) return(1);
        i=tr_avaa2(word[1],"",&txt2,"rt"); if (i<0) return(1);

        fgets(x,LLENGTH+9,txt2);
        if (strncmp(x,"--- ",4)!=0)
            {
            sprintf(sbuf,"\nFile %s cannot be converted!",word[1]);
            sur_print(sbuf); WAIT; return(1);
            }
        fgets(x,LLENGTH+9,txt2); strcpy(y,x);
        i=split(y,osa,3);
        lev=atoi(osa[1]);
        n=atoi(osa[2]);

        rewind(txt2);
        fgets(x,LLENGTH+9,txt2);
        for (j=1; j<=n+1; ++j)
            {
            fgets(x,LLENGTH+9,txt2);
            for (i=strlen(x)-1; i<lev; ++i) x[i]=' ';
            fwrite(x,1,lev,txt1);
            }
        if (strncmp(x,"END",3)==0) return(1);


        fgets(x,LLENGTH+9,txt2);  /* Shadows */
        for (i=strlen(x)-1; i<lev; ++i) x[i]=' ';
        fwrite(x,1,lev,txt1);

        while (1)
            {
            fgets(x,LLENGTH+9,txt2);
            if (strncmp(x,"END",3)==0)
                {
                for (i=3; i<lev; ++i) x[i]=' ';
                fwrite(x,1,lev,txt1);
                return(1);
                }

            p=strchr(x,':');
            if (p==NULL)
                {
                sprintf(sbuf,"\nError in file %s !",word[1]);
                sur_print(sbuf); WAIT; return(1);
                }
            *p=EOS; ++p;
            i=atoi(x); q=(char *)&i;
            for (j=0; j<sizeof(int); ++j) y[j]=q[j];
            strcpy(y+sizeof(int),p);
            for (i=strlen(p)-1; i<lev-sizeof(int); ++i) y[i+sizeof(int)]=' ';
            fwrite(y,1,lev,txt1);
            }

        return(1);
        }


#define MAXL 1000000
static FILE *tx1,*tx2;

static int shorten(int bytes,char limit)
    {
    int i,n,k;
    char buf[MAXL];
// Rprintf("\nlimit=%d",limit); getch();
// Rprintf("\nbytes=%d ",bytes); getch();
    while (!feof(tx1))
        {
        n=0;
        while (1)
            {
            i=fgetc(tx1);
            if (i==-1) break;
            buf[n]=(char)i;
            ++n;
            if (n>=MAXL)
                {
                sprintf(sbuf,"Too long field (over %d bytes)!",MAXL);
                sur_print(sbuf); WAIT;
                return(-1); // RS CHA exit(0);
                }
            if (i==(int)limit || i==10)
                {
                if (n>bytes+1) n=bytes+1;
                for (k=0; k<n-1; ++k) fputc((int)buf[k],tx2);
                fputc(i,tx2);
                break;
                }
            }
        if (i==-1) break;
        }
    muste_fclose(tx1);
    muste_fclose(tx2);
    return(1);
    }

// 11.4.2011/SM
static int op_txtshort()
        {
        int i; // ,j;
        char x[LLENGTH];
        char nimi[LLENGTH];
        int bytes;
        char limit_char;

        i=spec_init(r1+r-1); if (i<0) return(1);

        if (g<3)
            {
            init_remarks();
            rem_pr("TXTSHORT <input_file>,<output_file>,<n>");
            rem_pr("copies <input_file> to new <output_file>");
            rem_pr("by shortening long fields separated by characters");
            rem_pr("defined by a DELIMITER specification (default is TAB)");
            rem_pr("and line feeds");
            rem_pr("to a length of <n> bytes.");
            wait_remarks(2);
            return(1);
            }

        limit_char='\t';
        i=spfind("DELIMITER");
        if (i<0) i=spfind("LIMIT");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            if (muste_strcmpi(x,"TAB")==0) limit_char='\t';
            else if (muste_strnicmp(x,"SP",2)==0) limit_char=' ';
            else if (muste_strnicmp(x,"char(",5)==0) limit_char=atoi(x+5);
            else limit_char=*x;
            }

        strcpy(nimi,edisk); strcat(nimi,word[1]);
        tx1=muste_fopen(nimi,"rt"); if (tx1==NULL) return(1);
        strcpy(nimi,edisk); strcat(nimi,word[2]);
        tx2=muste_fopen(nimi,"wt"); if (tx2==NULL) return(1);
        if (g<4) bytes=16; else bytes=atoi(word[3]);

        i=shorten(bytes,limit_char);
        if (i<0)
            {
            sprintf(sbuf,"\nCannot save in file %s!",word[2]);
            sur_print(sbuf); WAIT;
            }
        return(1);
        }

        
        
static void op_txtrim()
        {
        int i;
// RS REM        char x[LLENGTH];
        char nimi[LLENGTH];

        if (g<4)
            {
            init_remarks();
            rem_pr("TXTRIM <ascii_file>,<converted_file>,<length>,<char>          ");
            rem_pr("increases the length of each line in <text_file> up to <length> characters");
            rem_pr("by inserting characters <char> at the end of each line.");
            rem_pr("Default <char> is a space.");
            rem_pr("The result is saved in <converted_file>.");
            rem_pr("If any of the lines is longer than <length> characters,");
            rem_pr("an error message is given and the process is interrupted.");
            wait_remarks(2);
            return;
            }
        *nimi=EOS;
        i=tr_avaa(word[1],&txt1,"rt",nimi); if (i<0) return;
        i=tr_avaa(word[2],&txt2,"wt",nimi); if (i<0) return;

        pituus=atoi(word[3]);
        merkki=' ';
        if (g>4) merkki=*word[4];

        tr_muunnos();
        }

 /* txtcount.c 18.12.1995/SM (22.12.1995) (30.3.1996)
*/
static int count()
        {
        int i;
        long n;
        int ch;

        for (i=0; i<256; ++i)
            {

            if (i==0) fprintf(txt2,"C0");
            else fprintf(txt2," C%d",i);
            freq[i]=0L;
            }
        fprintf(txt2,"\n");

        n=0L;
        sur_print("\nLines:");
        while (!ferror(txt2))
            {
            ch=getc(txt1);
            if (ch==EOF || feof(txt1)) break;
            ++freq[ch];
            if (ch==(int)'\n')
                {
                for (i=0; i<256; ++i)
                    {
                    if (i==0) fprintf(txt2,"%ld",freq[0]);
                    else fprintf(txt2," %ld",freq[i]);
                    freq[i]=0L;
                    }
                fprintf(txt2,"\n");
                ++n; sprintf(sbuf," %ld",n); sur_print(sbuf);
                if (sur_kbhit())
                    {
                    ch=sur_getch(); if (ch=='.') return(-2); // RS CHA exit(1); // RS FIXME
                    }
                }
            }
        if (ferror(txt2)) return(-1);
        muste_fclose(txt1);
        muste_fclose(txt2);
        return(1);
        }


static void op_txtcount()
        {
        int i;
// RS REM        char x[LLENGTH];
        char nimi[LLENGTH];

        if (g<3)
            {
            init_remarks();
            rem_pr("TXTCOUNT <ascii_file>,<count_file>");
            rem_pr("calculates line by line frequencies of all ascii characters");
            rem_pr("in <ascii_file> and saves them as a new ascii file <count_file>");
            rem_pr("in the form:");
            rem_pr("C0      C1      C2     ...  C255");
            rem_pr("f(1,0)  f(1,1)  f(1,2) ...  f(1,255)");
            rem_pr("f(2,0)  f(2,1)  f(2,2) ...  f(2,255)");
            rem_pr("...     ...     ...    ...  ...");
            rem_pr("where the first line is a fixed line of labels (names)");
            rem_pr("and f(j,i) is the frequency of char i on line j.");
            rem_pr("");
            rem_pr("To make statistics of <count_file>, convert it into a Survo data file by");
            rem_pr("FILE SAVE <count_file> TO <new_data_file> .");
            wait_remarks(2);
            return;
            }
        *nimi=EOS;
        i=tr_avaa(word[1],&txt1,"rb",nimi); if (i<0) return;
        i=tr_avaa(word[2],&txt2,"wt",nimi); if (i<0) return;

        i=count();
        if (i<0)
            {
            sprintf(sbuf,"\nCannot save in file %s!",word[2]);
            sur_print(sbuf); WAIT;
            }
        return;
        }

       

static int op_txtconv()
        {
        int i;
// RS REM        unsigned char x[LLENGTH],w[1];
        char nimi[LLENGTH];

        if (g<3)
            {
            init_remarks();
            rem_pr("TXTCONV <ascii_file>,<converted_file>   / S.Mustonen 7.5.1992");
            rem_pr("converts the text file according to CONVERSIONS list:");
            rem_pr("CONVERSIONS:");
            rem_pr("R  <char1>  <char2>     Replace char <char1> by <char2>");
            rem_pr("D  <char1>              Delete  char <char1>           ");
            rem_pr("T  \"<text1>\" \"<text2>\"  Replace <text1> by <text2>     ");
            rem_pr("t  \"<text1>\" \"<text2>\"  Replace <text1> by <text2>     ");
            rem_pr("END");
            rem_pr(" ");
            rem_pr("In alternatives R,D, and t, ");
            rem_pr("non-printable characters are denoted as char(n) n=decimal value.");
            rem_pr(" ");
            rem_pr("Default delimiter in texts is character \" .");
            rem_pr("This character can be replaced by another by a TEXTLIMIT");
            rem_pr("specification. Example: TEXTLIMIT=* ");
            rem_pr(" ");
            wait_remarks(1);
            rem_pr("....................................................");
            rem_pr("Example:");
            rem_pr("SAVEP CUR+1,CUR+3,TXT1.TXT");
            rem_pr("AAAAAAAAAAAAAAAABBB SURVO 84C BBBBBBBBBCCCCCCCCCCCCC");
            rem_pr("111111111111112222222222222,,,,,,,,,,::::::::::");
            rem_pr("AAAAAAAAAA SURVO 84C BBBBBBBBBBBBBCCCCCCCCCC");
            rem_pr(" ");
            rem_pr("TXTCONV TXT1.TXT,TXT2.TXT");
            rem_pr("CONVERSIONS:");
            rem_pr("R A a");
            rem_pr("R B b");
            rem_pr("T \"SURVO 84C\" \"Survo\"");
            rem_pr("D ,");
            rem_pr("T \"111\" \"three \"");
            rem_pr("t \"2char(50)2\" \"(char(254))\"");
            rem_pr("END");
            rem_pr(" ");
            rem_pr("LOADP TXT2.TXT");
            rem_pr("aaaaaaaaaaaaaaaabbb Survo bbbbbbbbbCCCCCCCCCCCCC");
            rem_pr("three three three three 11(_)(_)(_)(_)2::::::::::");
            rem_pr("aaaaaaaaaa Survo bbbbbbbbbbbbbCCCCCCCCCC");
            rem_pr("ˇ");
            wait_remarks(2);
            return(1);
            }
        i=spec_init(r1+r-1); if (i<0) return(-1);
        i=spfind("TEXTLIMIT");
        if (i>=0)
            {
            textlimit=*spb[i];
            if (textlimit==EOS) textlimit=' ';
            }          
        i=conv_list(); if (i<0) return(-1);
        *nimi=EOS;
        if (g_conv) { g_conversion(word[1],word[2]); return(1); } // RS 24.1.2013                       
        i=tr_avaa(word[1],&txt1,"rb",nimi); if (i<0) return(-1);
        i=tr_avaa(word[2],&txt2,"wb",nimi); if (i<0) return(-1);
        if (b_conv) { b_conversion(); return(1); }
        if (c_conv) { c_conversion(); return(1); } // SM 26.5.2012 
        muunnos();
        return(1);
        }

static int op_txtdel()
        {
        int i;
        int j1,j2;
        int len1,len2;
        char x[100*LLENGTH]; // RS REM unsigned
        char *p;

        if (g<5)
            {
            init_remarks();
            rem_pr("TXTDEL <ascii_file>,<converted_file>,L1,L2");
            rem_pr("deletes lines from <ascii_file>.");
            rem_pr("The first line to be deleted is given as the edit line L1.");
            rem_pr("The last  line to be deleted is given as the edit line L2.");
            wait_remarks(2);
            return(1);
            }
        i=tr_avaa(word[1],&txt1,"rt",nimi); if (i<0) return(-1);
        i=tr_avaa(word[2],&txt2,"wt",nimi); if (i<0) return(-1);

        j1=edline2(word[3],1,1); if (j1==0) return(-1);
        j2=edline2(word[4],1,1); if (j2==0) return(-1);

        edread(line1,j1);
        edread(line2,j2);

     i=ed1-1; while(line1[i]==' ') line1[i--]=EOS; len1=strlen(line1)-1;
     i=ed1-1; while(line2[i]==' ') line2[i--]=EOS; len2=strlen(line2)-1;

        p=line1+1;
        while (!feof(txt1))
            {
            fgets(x,100*LLENGTH-2,txt1);
            i=strlen(x)-1;
            if (*x==*p && len1==i)
                {
                if (strncmp(x,p,i)==0) break;
                }
            fputs(x,txt2);
            }
        p=line2+1;
        while (!feof(txt1))
            {
            fgets(x,100*LLENGTH-2,txt1);
            i=strlen(x)-1;
            if (*x==*p && len2==i)
                {
                if (strncmp(x,p,i)==0) break;
                }
            }
        while (!feof(txt1))
            {
            fgets(x,100*LLENGTH-2,txt1);
            if (feof(txt1)) break;
            if (ferror(txt1)) break;
            fputs(x,txt2);
            }
        return(1);
        }



static void op_txt()
        {
        char *pw;

        pw=word[0];
        if (muste_strcmpi(pw,"TXTCONV")==0)
            { op_txtconv(); return; }
        if (muste_strcmpi(pw,"TXTRIM")==0)
            { op_txtrim(); return; }
        if (muste_strcmpi(pw,"TXTCOUNT")==0)
            { op_txtcount(); return; }
        if (muste_strnicmp(pw,"TXTEDTOUT",8)==0)
            { op_txtedtout(); return; }
        if (muste_strcmpi(pw,"TXTEDTIN")==0)
            { op_txtedtin(); return; }
        if (muste_strcmpi(pw,"TXTDEL")==0)
            { op_txtdel(); return; }
        if (muste_strcmpi(pw,"TXTSHORT")==0)
            { op_txtshort(); return; }     

        sprintf(sbuf,"\nUnknown TXT command %s",pw);
        sur_print(sbuf); WAIT;
        }


static void transp(int j3)
       {
       int i,j;
       char *sana[EP4];

       for (j=0; j<nr; ++j)
           {
           split(aputila+j*(c2+1),sana,transp_n);
           for (i=0; i<transp_n; ++i)
               {
               edwrite(sana[i],j3+i,transp_pos[j]);
               }
           }

       }

static int siirto(int j1,int j2)
       {
       int i,j,max,k;
       char x[LLENGTH], *sana[EP4];

       slev=0;
       for (j=j1; j<=j2; ++j)
           {
           edread(x,j);
           strcpy(aputila+(j-j1)*(c2+1),x+1);
           i=split(x+1,sana,EP4);
           if (j==j1) transp_n=i;
           else
               {
               if (i!=transp_n)
                   {
                   sprintf(sbuf,"\n# of words on line %d not equal to %d!",
                                               j,transp_n); sur_print(sbuf);
                   WAIT; return(-1);
                   }
               }
           max=0;
           for (i=0; i<transp_n; ++i)
               {
               k=strlen(sana[i]);
               if (k>max) max=k;
               }
           transp_lev[j-j1]=max; slev+=max;
           }

       return(1);
       }


static void not_enough_memory()
       {
       sur_print("\nNot enough memory!");
       WAIT;
       }
       
static int varaa_tilat(int j1,int j2)
       {
       aputila=muste_malloc((j2-j1+1)*(c2+1));
       if (aputila==NULL) { not_enough_memory(); return(-1); }
       transp_lev=(int *)muste_malloc((j2-j1+1)*sizeof(int));
       if (transp_lev==NULL) { not_enough_memory(); return(-1); }
       transp_pos=(int *)muste_malloc((j2-j1+1)*sizeof(int));
       if (transp_pos==NULL) { not_enough_memory(); return(-1); }
       return(1);
       }

static void op_transp()
        {
        int i,j,j1,j2,j3;

        if (g<4)
            {
            sur_print("\nUsage: TRANSP L1,L2,L");
            sur_print("\nTRANSP transposes a table on edit lines L1-L2 and writes");
            sur_print("\nthe transposed table in the current edit field from line L onwards.");
            sur_print("\nThe original table may contain columns of both numbers and words.");
            WAIT; return;
            }
        j1=edline2(word[1],1,1); if (j1==0) return;
        j2=edline2(word[2],j1,1); if (j2==0) return;
        j3=edline2(word[3],1,1); if (j3==0) return;

        nr=j2-j1+1;
        i=varaa_tilat(j1,j2); if (i<0) return;
        i=siirto(j1,j2); if (i<0) return;
        if (j3+transp_n-1>r2)
            {
            sur_print("\nNot enough lines for the result!");
            WAIT; return;
            }
        if (slev+nr-1>c2)
            {
            sur_print("\nNot enough columns for the result!");
            WAIT; return;
            }
        for (j=0; j<transp_n; ++j) edwrite(space,j+j3,1);
        transp_pos[0]=1; for (i=1; i<nr; ++i) transp_pos[i]=transp_pos[i-1]+transp_lev[i-1]+1;
        transp(j3);
        }

/* !interp.c 9.7.1988/SM (10.7.1988)
*/

static void interpoloi()
        {
        int i,j,k;
        char s[LLENGTH];
        char t[LLENGTH];
        double a,b;

        XP[0]=1.0;
        for (j=l3; j<=l4; ++j)
            {
            if (mx0<3)
                {
                if (mx0==1) XP[1]=j-l1+base;
                else
                    {
                    edread(s,j);
                    strncpy(t,s+xcol[1],xlev[1]); t[xlev[1]]=EOS;
                    XP[1]=atof(t);
                    }
                a=b=XP[1];
                for (i=2; i<=degree; ++i)
                    {
                    a=a*b;
                    XP[i]=a;
                    }
                }
            for (i=0; i<my; ++i)
                {
                if (mx0>2)
                    {
                    edread(s,j);
                    for (k=1; k<mx; ++k)
                        {
                        strncpy(t,s+xcol[k],xlev[k]); t[xlev[k]]=EOS;
                        XP[k]=atof(t);
                        }
                    }
                a=0.0; for (k=0; k<mx; ++k) a+=XP[k]*B[i*mx+k];
                fconv(a,ymask[i],t);
                if (strlen(t)>strlen(ymask[i]))
                    {
                    sprintf(sbuf,"\nImage %s in columns %d-%d on the mask line %s",
                           ymask[i],(int)ycol[i],(int)(ycol[i]+strlen(ymask[i])-1),word[5]);
                           sur_print(sbuf);
                    sprintf(sbuf,"\nis too narrow for value %s",t);       
                    sur_print(sbuf);
                    WAIT; return;
                    }
                edwrite(t,j,ycol[i]);
                }
            }
        }


static void lue_datat()
        {
        int i,j;
        char s[LLENGTH];
        char t[LLENGTH];
        double a,b;

        for (j=0; j<n; ++j) X[j]=1.0;  /* vakiosel. */
        if (mx<3)
            {
            if (mx==1) for (j=0; j<n; ++j) X[n+j]=(double)(j+base);
            else /* mx=2 */
                {
                for (j=l1; j<=l2; ++j)
                    {
                    edread(s,j);
                    strncpy(t,s+xcol[1],xlev[1]); t[xlev[1]]=EOS;
                    X[n+j-l1]=atof(t);
                    }
                }
            if (degree>1)
                {
                for (j=0; j<n; ++j)
                    {
                    a=b=X[n+j];
                    for (i=2; i<=degree; ++i)
                        {
                        b=a*b; X[i*n+j]=b;
                        }
                    }
                }
            }
        for (j=l1; j<=l2; ++j)
            {
            edread(s,j);
            if (mx>2)
                {
                for (i=1; i<mx; ++i)
                    {
                    strncpy(t,s+xcol[i],xlev[i]); t[xlev[i]]=EOS;
                    X[i*n+j-l1]=atof(t);
                    }
                }
            for (i=0; i<my; ++i)
                {
                strncpy(t,s+ycol[i],ylev[i]); t[ylev[i]]=EOS;
                Y[i*n+j-l1]=atof(t);
                }
            }
        if (mx<3) mx=1+degree;
        }


static void not_space()
        { sur_print("\nNot enough memory!"); WAIT; }


static int interp_varaa_tilat()
        {
        int mxx;

        mxx=mx+degree;
        X=(double *)muste_malloc(mxx*n*sizeof(double));
        if (X==NULL) { not_space(); return(-1); }
        Y=(double *)muste_malloc(my*n*sizeof(double));
        if (Y==NULL) { not_space(); return(-1); }
        B=(double *)muste_malloc(my*n*sizeof(double));
        if (B==NULL) { not_space(); return(-1); }
        XP=(double *)muste_malloc(mxx*sizeof(double));
        if (XP==NULL) { not_space(); return(-1); }


        return(1);
        }

static void op_interp()
        {
        int i,nf;

        if (g<6)
            {
sur_print("\nUsage: INTERP L1,L2,L3,L4,K");
sur_print("\n where K is the label of a mask line of the form:");
sur_print("\n     XXXXX  XXXX  YY.YYY  XX  YYY.YY");
sur_print("\n");
sur_print("\nINTERP interpolates columns denoted by YY.YYY masks");
sur_print("\nby linear regression analysis (when more than 2 XXX columns exist as regressors)");
sur_print("\nor by polynomial regression (when only one or no XXX column are given).");
sur_print("\nIn the latter case the degree of the polynomial is given");
sur_print("\nby a DEGREE specification. Default is DEGREE=1 .");
sur_print("\nThe source data for with complete X and Y values is given on lines L1-L2.");
sur_print("\nThe interpolated (and extrapolated) Y values will be computed on lines L3-L4");
sur_print("\nusing given X values.");
sur_print("\nIn polynomial regression with no XXX column,");
sur_print("\nL-L1+1 where L is the current line number, is the basic regressor.");
sur_print("\nIn this case it is typical that L3=L2+1.");
WAIT; return;
            }
        i=sp_init(r1+r-1); if (i<0) return;
        degree=1;
        i=spfind("DEGREE"); if (i>=0) { degree=atoi(spb[i]); if (degree<1) degree=1; }
        base=1;
        i=spfind("BASE"); if (i>=0) base=atoi(spb[i]);   /* ei tarpeen */

        l1=edline2(word[1],1,1); if (l1==0) return;
        l2=edline2(word[2],l1,1); if (l2==0) return;
        l3=edline2(word[3],1,1); if (l3==0) return;
        l4=edline2(word[4],l3,1); if (l4==0) return;
        k=edline2(word[5],1,1); if (k==0) return;

        n=l2-l1+1;
        edread(msk,k);
        nf=split(msk+1,msana,EP4);
        mx=1; my=0;
        for (i=0; i<nf; ++i)
            {
            if (strchr(msana[i],'X')!=NULL)
                {
                xcol[mx]=msana[i]-msk; xlev[mx]=strlen(msana[i]); ++mx;
                continue;
                }
            if (strchr(msana[i],'Y')!=NULL)
                {
                ycol[my]=msana[i]-msk; ylev[my]=strlen(msana[i]);
                ymask[my]=msana[i]; ++my;
                continue;
                }
            }
        mx0=mx;
        if (my==0)
            {
            sprintf(sbuf,"\nNo YY.YYY fields on the mask line %s!",word[5]);
            sur_print(sbuf); WAIT; return;
            }
        i=interp_varaa_tilat(); if (i<0) return;
        lue_datat();
        if (mx>n)
            {
            sprintf(sbuf,"\nToo few (%d) observations as a basis for interpolation!",n);
            sur_print(sbuf); WAIT; return;
            }
/*  mprint(X,n,mx); mprint(Y,n,my); getch();  */
        i=ortholin1(X,n,mx,Y,my,1e-15,B,0);
        if (i<0)
            {
            sur_print("\nCannot interpolate. Linear dependencies!");
            WAIT; return;
            }
/*  Rprintf("\n"); mprint(B,mx,my); getch(); */
        interpoloi();
        }


/*  vfind.c 4.11.2000/SM (4.11.2000)
 */

static int not_found(char *t)
    {
    if (etu==2) return(1);
    sprintf(sbuf,"\nVertical text \"%s\" not found!",t);
    sur_print(sbuf); WAIT; return(1);
    }


static int paikka(int rivi,int sar) // Muunnos findin rivsar()-funktiosta
        {
//        int d=0;

        if (rivi>r1+r3-1) { /* d=1; */ r1=rivi; r=1;
                            if (r1>r2-r3+1) { r1=r2-r3+1; r=rivi-r1+1; }
                          }
        else if (rivi<r1) { /* d=1; */ r1=rivi; r=1; }
        else r=rivi-r1+1;
        if (sar==0) sar=1;
        if (sar>=c1 && sar<c1+c3) c=sar-c1+1;
        if (sar<c1) { /* d=1; */ c1=sar; c=1; }
        if (sar>c1+c3-1) { /* d=1; */ c1=sar; c=1;
                           if (c1>c2-c3+1) { c1=c2-c3+1; c=sar-c1+1; }
                         }
//      if (d) disp();
//      cursor(r,c);
        return(1);
        }

 
static int op_vfind()
    {
    int i,j,k,h,len;
    char x[LLENGTH],*s[2];
    char *text;
    char ch;
    char *p;

    if (g<2)
        {
        init_remarks();
        rem_pr("VFIND <string>");
        rem_pr("finds first occurrence of <string> in the edit field");
        rem_pr("in vertical position. Example:");
        rem_pr("VFIND 123");
        rem_pr("tries to find        1");
        rem_pr("                     2");
        rem_pr("                     3");
        wait_remarks(2);
        return(1);
        }
    j=r1+r-1;
    edread(x,j);
    i=split(x+1,s,2);
    k=s[1]-x;
    edread(x,j);
    text=x+k;
    i=strlen(text)-1; while (text[i]==' ') text[i--]=EOS;
    len=strlen(text);
    ch=*text;
    p=z+j*ed1-1;
    while (1)
        {
        p=strchr(p+1,ch);
        if (p==NULL) { not_found(text); return(1); }
        k=(p-z)/ed1+1;
        h=(p-z)-(k-1)*ed1;
        for (i=1; i<len; ++i)
            {
            if (*(z+(k-1+i)*ed1+h)!=text[i]) break;
            }
        if (i<len) continue;
        paikka(k,h);
        break;
        }
    return(1);
    }

/* STRDIST.C 29.5.2004/SM (29.5.2004) (28.6.2004)
*/

static int minimum3(int a,int b,int c)
/*Gets the minimum of three values*/
{
  int min=a;
  if(b<min)
    min=b;
  if(c<min)
    min=c;
  return min;
}

static int levenshtein_distance3(char *s,char *t,int xy)
/* Compute levenshtein distance between s and t */
/* xy=0: swaps xy->yx not allowed, xy=1: swaps allowed */
{
  //Step 1
  int k,i,j,n,m,cost,*d,distance;
  int *swap;

  n=strlen(s);
  m=strlen(t);
  if(n!=0 && m!=0)
    {
    d=muste_malloc((sizeof(int))*(m+1)*(n+1));
    swap=muste_malloc((sizeof(int))*(m+1)*(n+1));
    for (i=0; i<(m+1)*(n+1); ++i) swap[i]=0;
    ++m;
    ++n;

    for(k=0;k<n;++k)
      d[k]=k;
    for(k=0;k<m;++k)
      d[k*n]=k;

    for(i=1;i<n;++i)
      for(j=1;j<m;++j)
      {

      if(s[i-1]==t[j-1])
        cost=0;
      else
        cost=1;
      d[j*n+i]=minimum3(d[(j-1)*n+i]+1,d[j*n+i-1]+1,d[(j-1)*n+i-1]+cost);

      if (xy && cost && s[i-1]==t[j-2] && s[i-2]==t[j-1] // 27.6.2004
         && !swap[(j-1)*n+i-1] && d[j*n+i]>d[(j-1)*n+i-1])
          { --d[j*n+i]; swap[j*n+i]=1; }
      }
/************************
 for (i=0; i<n; ++i)
   {
   Rprintf("\n");
   for (j=0; j<m; ++j)
     Rprintf("%d ",d[j*n+i]);
   }
 getch();
***************************/
    distance=d[n*m-1];
    muste_free(d);
    muste_free(swap);
    return distance;
    }
  else
    return(-1);
}


static int op_strdist()
        {
        int i;
        int j1,j2;
        int d,d2;
        char x[LLENGTH];
        char *p;

        if (g<3)
            {
            init_remarks();
            rem_pr("STRDIST L1,L2");
            rem_pr("computes the Levenshtein distance");
            rem_pr("between strings given on edit lines L1,L2.");
            wait_remarks(2);
            return(1);
            }

        j1=edline2(word[1],1,1); if (j1==0) return(-1);
        j2=edline2(word[2],1,1); if (j2==0) return(-1);

        edread(str1,j1);
        edread(str2,j2);
        i=strlen(str1)-1; while (i>1 && str1[i]==' ') str1[i--]=EOS;
        i=strlen(str2)-1; while (i>1 && str2[i]==' ') str2[i--]=EOS;

        d=levenshtein_distance3(str1+1,str2+1,0);
        d2=levenshtein_distance3(str1+1,str2+1,1); // 27.6.2004
/*******************************  poistettu 27.6.2004
        // Vain per‰kk‰iset merkit vaihtaneet paikkaansa?
        d2=0;
        if (d==2 && strlen(str1)==strlen(str2))
            {
            i=0;
            while (i+1<strlen(str1))
                {
                ++i;
                if (str1[i]==str2[i]) continue;
                if (str1[i]==str2[i+1] && str1[i+1]==str2[i]) d2=1;
                break;
                }
            }
******************************************/
        edread(x,r1+r-1);
        p=strstr(x," / "); if (p!=NULL) *p=EOS;
        i=strlen(x)-1; while (x[i]==' ') x[i--]=EOS;
        sprintf(x+i+1," / Levenshtein distance is %d (%d)",d,d2);
        edwrite(space,r1+r-1,0);
        edwrite(x,r1+r-1,0);
        return(1);
        }


/* _reverse.c 8.5.2007 (17.5.2007)
*/


static int reverse_by_bytes(int j1,int j2)
    {
    int i,i2,len,j;
    char ch;

    for (j=j1; j<=j2; ++j)
        {
        edread(sbuf,j);

        len=ed1-1; while (sbuf[len]==' ') --len;
        i2=len+1;
        for (i=1; i<(len-1)/2; ++i)
            {
            --i2;
            ch=sbuf[i]; sbuf[i]=sbuf[i2]; sbuf[i2]=ch;
            }
        edwrite(sbuf,j,0);
        }
    return(1);
    }

static int reverse_by_lines(int j1,int j2)
    {
    int i,i1,i2,n;
    char x1[LLENGTH];
    char x2[LLENGTH];

    n=j2-j1+1;
    for (i1=j1; i1<=j1+n/2-1; ++i1)
        {
        i2=j2-(i1-j1);
        edread(x1,i1);
        edread(x2,i2);
        i=zs[i1]; zs[i1]=zs[i2]; zs[i2]=i;
        edwrite(space,i1,0);
        edwrite(x2,i1,0);
        edwrite(space,i2,0);
        edwrite(x1,i2,0);
        }
    return(1);
    }

static int op_reverse()
        {
        int i;
        int j1,j2,j,n,k;
        char *s[NMAX];
        char x[LLENGTH];
        int reverse_laji; // RS CHA global to local

        if (g<3)
            {
            init_remarks();
            rem_pr("REVERSE L1,L2");
            rem_pr("changes the words on each line (L1-L2) in opposite order.");
            rem_pr("REVERSE L1,L2,1");
            rem_pr("changes lines L1-L2 in opposite order.");
            rem_pr("REVERSE L1,L2,2");
            rem_pr("changes the characters on each line (L1-L2) in opposite order.");
            wait_remarks(2);
            return(1);
            }
        j1=edline2(word[1],1);
        j2=edline2(word[2],j1);

        if (g>3)
            {
            reverse_laji=atoi(word[3]);
            if (reverse_laji==1) { reverse_by_lines(j1,j2); return(1); }
            if (reverse_laji==2) { reverse_by_bytes(j1,j2); return(1); }
            }
// reverse by words
        for (j=j1; j<=j2; ++j)
            {
            edread(sbuf,j);
            n=split(sbuf+1,s,NMAX);
            if (n==0) continue;
            k=0;
            for (i=n-1; i>=0; --i) k+=sprintf(x+k,"%s ",s[i]);
            edwrite(space,j,1);
            edwrite(x,j,1);
            }
        return(1);
        }


/* transpos.c 19.11.2007/SM (19.11.2007)
*/
static int t_name(char *s,char *t)
    {
    strcpy(t,s);
    if (strchr(s,':')==NULL)
        { strcpy(t,edisk); strcat(t,s); }
    return(1);
    }

static int op_transpose()
    {
    int m,n,i,j,k,h;
    char *p,*q,*s;
    int  ch;
    char x[LLENGTH];
    char *tt[2];

    if (g<3)
        {
        init_remarks();
        rem_pr("TRANSPOSE <text_file>,<new_text_file>");
        rem_pr("transposes a text file of m 'columns' and n lines into");
        rem_pr("a new text file with n 'columns' and m lines.");
        rem_pr("It thus interchages 'rows' and 'columns'.");
        rem_pr("The separators between 'columns' in <text_file> and <new_text_file>,");
        rem_pr("respectively are given by the specification");
        rem_pr("DELIMITER=char1,char2");
        rem_pr("By default char1=char2=TAB (char(9)).");
        rem_pr("Examples: DELIMITER=TAB,SP  (SP=space) is the same as");
        rem_pr("          DELIMITER=char(9),char(32)");
        rem_pr("");
        rem_pr("Example of TRANSPOSE on the next page");

        wait_remarks(1);

        rem_pr("SAVEP CUR+1,CUR+3,TEST.TXT");
        rem_pr("Year	2001	2002	2003	2004	2005	2006	2007");
        rem_pr("Group 1	1.45	1.52	1.48	1.73	1.8	2.01	2.22");
        rem_pr("Group 2	2.21	1.77	1.94	2	2.3	2.41	2.52");
        rem_pr("");
        rem_pr("TRANSPOSE TEST.TXT,TEST2.TXT / DELIMITER=TAB,|");
        rem_pr("LOADP TEST2.TXT");
        rem_pr("Year|Group 1|Group 2");
        rem_pr("2001|1.45|2.21");
        rem_pr("2002|1.52|1.77");
        rem_pr("2003|1.48|1.94");
        rem_pr("2004|1.73|2");
        rem_pr("2005|1.8|2.3");
        rem_pr("2006|2.01|2.41");
        rem_pr("2007|2.22|2.52");
        wait_remarks(2);
        return(1);
        }
    i=spec_init(r1+r-1);
    if (i<0) return(1);

    // DELIMITER=<in>,<out>

    i=spfind("DELIMITER");
    if (i>=0)
        {
        strcpy(x,spb[i]);
        i=split(x,tt,2);
        if (i)
            {
            if (muste_strcmpi(tt[0],"TAB")==0) delim_in='\t';
            else if (muste_strnicmp(tt[0],"SP",2)==0) delim_in=' ';
            else if (muste_strnicmp(tt[0],"char(",5)==0) delim_in=atoi(tt[0]+5);
            else delim_in=*tt[0];
            if (i>1)
                {
                if (muste_strcmpi(tt[1],"TAB")==0) delim_out='\t';
                else if (muste_strnicmp(tt[1],"SP",2)==0) delim_in=' ';
                else if (muste_strnicmp(tt[1],"char(",5)==0) delim_out=atoi(tt[1]+5);
                else delim_out=*tt[1];
                }
            }
        }

    t_name(word[1],name1);
    t_name(word[2],name2);
    fil1=muste_fopen(name1,"rb");
    if (fil1==NULL) { sprintf(sbuf,"\nFile %s not found!",word[1]);
                      sur_print(sbuf); WAIT; return(1);
                    }
    fil2=muste_fopen(name2,"wb");
    if (fil2==NULL) { sprintf(sbuf,"\nCannot open file %s!",word[2]);
                      sur_print(sbuf); WAIT; return(1);
                    }
    t=muste_malloc(SIZE);
    pt=muste_malloc(10000);

    m=0; n=0; pt[0]=t; p=t;
    while (1)
        {
        i=0;
        while (1)
            {
            ch=fgetc(fil1);
// Rprintf("%c",(char)ch);
            if (ch==-1) break;
            if (ch==(int)delim_in) ++i;
            if (ch==(int)'\n')
                {
                ++i;
                if (n==0) m=i;
                else if (i!=m)
                    {
                    if (i<=1) { ch=-1; break; }
                    sprintf(sbuf,"\n%d fields instead of %d on line %d!",
                                    i,m,n+1); sur_print(sbuf); WAIT; return(1);
                    }
                --p; *p++=delim_in; pt[++n]=p; break;
                }
            *p++=(char)ch;
            }
        if (ch==-1) break;
        }
    *p=EOS;
    muste_fclose(fil1);
/********************
  i=strlen(t); Rprintf("\nlen=%d|",i); getch();
  Rprintf("\n%s",t); WAIT;
  Rprintf("\nm=%d n=%d",m,n); getch();
  Rprintf("\nt=%.50s",t); getch();
  Rprintf("\n");
  for (i=0; i<10; ++i) Rprintf("%d|",(int)t[i]); getch();
**************************/
	q=x; // RS 4.2.2013
    for (j=0; j<m; ++j)
        {
        s=rivi;
        for (i=0; i<n; ++i)
            {
            p=pt[i];
            for (k=0; k<=j; ++k)
                {
                q=p; p=strchr(q+1,delim_in); ++p;
                }
            h=(int)(p-q);
            for (k=0; k<h; ++k) *s++=*q++;


// *s=EOS; Rprintf("\nrivi=%s",rivi); getch(); exit(0);
            }
        --s; *s++='\15'; *s++='\12'; *s=EOS;
// Rprintf("\nrivi=%s|",rivi); getch();
// 13(10:8)=15  10(10:8)=12
        k=0;
        while (rivi[k])
            {
            if (rivi[k]==delim_in) rivi[k]=delim_out;
            fputc((int)rivi[k++],fil2);
            }
        }
    muste_fclose(fil2);
    return(1);
    }

/*  !colx.c 29.12.1985/SM (26.5.1995)
    COLX col1,col2   exchanges columns in the current edit field
                     below the current line.
         0,C2  are default parameters
    + other functions
 */

static int find_text_dimensions()
    {
    int i,j,j1,j2,jmax;
    int max;
    char x[LLENGTH];

    j1=edline2(parm[2],1,1); if (!j1) return(1);
    j2=edline2(parm[3],j1,1); if (!j2) return(1);
    max=0; jmax=j1;
    for (j=j1; j<=j2; ++j)
        {
        edread(x,j);
        i=strlen(x)-1;
        while (i>0 && x[i]==' ') --i;
        if (i>max) { max=i; jmax=j; }
        }
    j=r1+r-1; edread(x,j);
    i=strlen(x)-1;
    while (i>0 && x[i]==' ') --i;
    sprintf(sbuf,"/ width=%d lines=%d first_line=%d longest_line=%d",max,j2-j1+1,j1,jmax);
    edwrite(sbuf,j,i+2);
    return(1);
    }


        /* COLX Si,j / column j to shadow characters of column i */
static int move_to_shad()
        {
        int col1,col2;
        int i,j;
        char x[LLENGTH];
        char ch;

        col1=atoi(parm[1]+1);
        col2=atoi(parm[2]);
        for (j=r1+r; j<=r2; ++j)
            {
            edread(x,j);
            ch=x[col2];
            if (ch==' ') continue;
            if (zs[j]==0)
                {
                i=creatshad(j);
                if (i<0) return(-1);
                }
            edread(x,zs[j]);
            x[col1]=ch;
            edwrite(x,zs[j],0);
            }
        return(1);
        }



static int from_line_to_column()  /* COLX Lline,column,first_line */
        {
        int lin,col,first;
        char x[LLENGTH];
        char y[LLENGTH];
        int len,i,j;

        lin=edline2(parm[1]+1,1,1);
        if (lin==0) return(-1);
        col=atoi(parm[2]);
        first=edline2(parm[3],1,1);
        if (first==0) return(-1);
        edread(x,lin);
        len=c2; while (x[len]==' ' && len>0) --len;
        i=1; j=first;
        for (i=1; i<=len; ++i)
            {
            if (j>r2) break;
            edread(y,j);
            y[col]=x[i];
            edwrite(y,j,0);
            ++j;
            }
        return(1);
        }



static int decode_shadows()
        {
        
muste_fixme("FIXME: COLX s (decode_shadows()) not implemented!"); // RS FIXME
/*
        int i,m1,m2;
        char nimi[LLENGTH];

        if (parm[1][1]==EOS)
            {
            if (g==2)
                {
                for (i=0; i<256; ++i) shad_active[i]=(unsigned char)i;
                shad_off=0;
                return(1);
                }
            strcpy(nimi,parm[2]);
            if (strchr(nimi,':')==NULL)
                {
                sprintf(nimi,"%sSYS\\",survo_path);
                strcat(nimi,parm[2]);
                }
            if (strchr(nimi+strlen(nimi)-4,'.')==NULL)
                strcat(nimi,".BIN");
            bin1=muste_fopen(nimi,"rb");
            if (bin1==NULL)
                {
                sprintf(sbuf,"\nCannot open file %s!",nimi);
                sur_print(sbuf); WAIT; return(1);
                }
            for (i=0; i<256; ++i) shad_active[i]=(unsigned char)getc(bin1);
            shad_off=1;
            return(1);
            }
        m1=atoi(parm[1]+1);
        m2=atoi(parm[2]);
        if (m1<0 || m2<0 || m1>255 || m2>255) return(1);
        shad_active[m1]=m2;
        shad_off=1;
*/
        return(1);
        }


static int set_cpu_speed()
        {
muste_fixme("\nFIXME: set_cpu_speed not implemented!\n");
/* RS NYI
        long l,ll,n;
        struct timeb tb;
        long alku,alkums,loppums,sek;
        char *p;
        extern int computation_speed;

        p=parm[1]+1;
        if (*p=='?')
            {
            sprintf(sbuf,"\ncpu_speed=%ld",cpu_speed);
            sur_print(sbuf);
            WAIT; return(1);
            }
        if (*p=='!')
            {
            sprintf(sbuf,"%ld@",cpu_speed);
            strcat(tut_info,sbuf);
            return(1);
            }
        if (*p!=EOS)
            {
            cpu_speed=atol(p); return(1);
            }
        n=8*2000000L;
        ftime(&tb); alku=tb.time;
        alkums=tb.millitm;
        ll=0L; for (l=0L; l<n; ++l) ++ll;
        ftime(&tb); sek=tb.time;
        loppums=tb.millitm;
        cpu_speed=(1000L*(sek-alku)+loppums-alkums)>>1;
*/
        return(1);
        }


static int disp_window_size()
    {
    int j;

    sprintf(sbuf,"COLX w %d %d (window size)",r3,c3);
    j=r1+r-1;
    edwrite(space,j,1);
    edwrite(sbuf,j,1);
    return(1);
    }


static int tell_soft_vis() // 8.2.2001
    {
    int j;

    sprintf(sbuf,"COLX V %d",soft_vis);
    j=r1+r-1;
    edwrite(space,j,1);
    edwrite(sbuf,j,1);
    return(1);
    }


int muuta_apu_tiedostoa(int mode)
// mode: 1=replace 2=del  3=APU tai APUDEL
    {
    int i,len;
    char x[LLENGTH];
//  char apu[LLENGTH];
    char *p,*q;
    int ok;
    char rivi[LLENGTH];
    extern char current_setup[];
    char apu[4];


// Rprintf("\ncurrent_setup=%s|",current_setup); WAIT;

    strcpy(apu,"APU");
    edread(x,r1+r-1);
    if (mode==3) // command SYS or SYSDEL
        {
        strcpy(apu,"SYS");
        if (muste_strcmpi(parm[0],"SYSDEL")==0) mode=2; else mode=1;
        }
    p=strstr(x,apu); if (p==NULL) return(1);
    p+=3; if (mode==2) p+=3; // APUDEL tai SYSDEL
    while (*p==' ') ++p;
    if (*p==EOS) return(1);
    if (mode==1 && strchr(p,'=')==NULL) return(1); // RS 1.11.2012
    i=strlen(p)-1; while (p[i]==' ') p[i--]=EOS;
    bin1=muste_fopen(current_setup,"rb");
    if (bin1==NULL) return(1); // RS 1.11.2012
    sprintf(sbuf,"%sAPU.TMP",etmpd);
    bin2=muste_fopen(sbuf,"wb");
    if (bin2==NULL) return(1); // RS 1.11.2012
    while (1)
        {
        i=getc(bin1);
        if (feof(bin1)) break;
        putc(i,bin2);
        }
    muste_fclose(bin2);
    muste_fclose(bin1);
    if (mode==2 && strchr(p,'=')==NULL) strcat(p,"=");

    q=strchr(p,'='); len=q-p+1; // 15.7.2006

    bin2=muste_fopen(sbuf,"rt");
    if (bin2==NULL) return(1); // RS 1.11.2012
    bin1=muste_fopen(current_setup,"wt");
    if (bin1==NULL) return(1); // RS 1.11.2012
    ok=0;
    while (1)
        {
        fgets(rivi,200,bin2);
        if (feof(bin2)) break;
// Rprintf("\nrivi=%s| p=%s| len=%d|",rivi,p,len);

        if (strncmp(p,rivi,len)==0)
           {
// Rprintf("\nOK!");
// getck();
           ok=1;
           if (mode==2) continue; // APUDEL
           else { strcpy(rivi,p); strcat(rivi,"\n"); }
           }
        fprintf(bin1,"%s",rivi);
        }
    if (!ok)
        {
        fprintf(bin1,"%s\n",p);
        }
    muste_fclose(bin1);
    muste_fclose(bin2);

/********************
SHOW G:\E\U\SURVO.APU
***********************/
    return(1);
    }

static int sijoita_Survo() // 21.10.2001
    {
    char x[LLENGTH];
    int j;

    strcpy(x,parm[2]);
    unsubst_survo_path_in_editor(x);
    j=r1+r-1;
    edwrite(space,j,1);
    edwrite(x,j,1);
    return(1);
    }


int op_colx()
        {
        int j,col1,col2;
        char x[LLENGTH];
        char ch;

        if (strcmp(parm[1],"TEXTDIM")==0)
            {
            find_text_dimensions();
            return(1);
            }

        /* COLX R43,C80 */
        if (g>1 && *parm[1]=='R')
            {
            r3=atoi(parm[1]+1);
            if (g>2 && *parm[2]=='C') c3=atoi(parm[2]+1);
            return(1);
            }

        /* COLX Si,j / column j to shadow characters of column i */
        if (*parm[1]=='S')
            {
            move_to_shad();
            return(1);
            }
        if (*parm[1]=='L')
            {
            from_line_to_column();
            return(1);
            }
        if (*parm[1]=='s')
            {
            decode_shadows();
            return(1);
            }
        if (*parm[1]=='W')
            {
            tut_wait_c=atoi(parm[1]+1);
            return(1);
            }
        if (*parm[1]=='C')
            {
            set_cpu_speed();
            return(1);
            }
        if (strcmp(parm[1],"w")==0) // 14.12.2000
            {
            disp_window_size();
            return(1);
            }
        if (strcmp(parm[1],"V")==0) // 8.2.2001
            {
            tell_soft_vis();
            return(1);
            }

        if (strcmp(parm[1],"APU")==0) // 18.9.2001
            {
            muuta_apu_tiedostoa(1);
            return(1);
            }
        if (strcmp(parm[1],"APUDEL")==0) // 18.9.2001
            {
            muuta_apu_tiedostoa(2);
            return(1);
            }
        if (strcmp(parm[1],"UNSUBST")==0) // 21.10.2001
            {
            sijoita_Survo();
            return(1);
            }

        col1=0; col2=c2;
        if (g>1)
            { col1=atoi(parm[1]); if (col1<0 || col1>c2) return(1); }
        if (g>2)
            { col2=atoi(parm[2]); if (col2<0 || col2>c2) return(1); }

        for (j=r1+r; j<=r2; ++j)
            {
            edread(x,j);
            ch=x[col1]; x[col1]=x[col2]; x[col2]=ch;
            edwrite(x,j,0);
            }
        return(1);
        }




static void op_tones()
		{
		muste_fixme("FIXME: TONES not implemented!\n");
		}	
		
static void op_pcopy()
		{
		muste_fixme("FIXME: PCOPY not implemented!\n");
		}

static void op_delf()
		{
		muste_fixme("FIXME: DELF not implemented!\n");
		}


int op_runr() // RS NEW
	{  
    	int i,k;
        int j,j1,j2,jo;
        char out[LNAME];
        char routtmp[LNAME];
        char space[LLENGTH];
        FILE *ofile;
        char *outfile,*pxx;
        extern int move_r1,move_r2,muste_selection;
		extern int muste_evalsource_output();   
		extern char *muste_rout;
		extern int muste_rbuf1, muste_rbuf2; // RS 25.3.2013
//        extern char *etmpd;

        if ((g<2 || g>5) && g!=99 && g!=100)
            {
//            sur_print("\nCorrect form: R (Runs R code until next empty line)");
//            sur_print("\nCorrect form: R a");
            sur_print("\nCorrect form:  R L1,L2,OUTLINE");
//            sur_print("\n          or:  R L1 (Runs R code until next empty line)");
            sur_print("\n          or:  R L1,L2 TO <file>");
            WAIT; return(-1);
    		}

		outfile=NULL; jo=0;
		strcpy(out,etmpd); strcat(out,"RUNR.CLP");
		strcpy(routtmp,etmpd); strcat(routtmp,"ROUT.TMP");
        ofile=muste_fopen(out,"wt");
        if (ofile==NULL) { sur_print("\nError opening RUNR.CLP!"); WAIT; return(-1); }
extern int move_ind;
		if ((muste_selection || move_ind) && g==99) // RS ADD move_ind 20.9.2012
			{
			j1=move_r1; j2=move_r2;
			}
		else if (g==100) { j1=muste_rbuf1; j2=muste_rbuf2; } // RS 25.3.2013
		else
			{
        	j1=r1+r; 
        	j2=emptyline(j1);
/*
    		for (i=0; i<ed1; ++i) space[i]=' ';
    		j2=j1; k=lastline2();   		
    		while (j2<=k)
    			{
        		if (strncmp(space,z+(j2-1)*ed1+1,(unsigned int)(ed1-1))==0) break;
        		++j2;
    			}
*/
        	if (g==2)
        		{
        		j1=edline2(word[1],1,1); if (j1==0) return(-1);
        		j2=emptyline(j1);
        		}
        	if (g>=3)
            	{
            	j1=edline2(word[1],1,1); if (j1==0) return(-1);
            	if (strcmp(word[2],"TO")==0 && g>3) outfile=word[3];
				else  { j2=edline2(word[2],j1,1); if (j2==0) return(-1); }
            	if (strcmp(word[3],"TO")==0 && g>4) outfile=word[4];
            	else if (*word[3]!='/') jo=edline2(word[3],1,1); // RS ADD if 25.2.2013
            	}
            }	
            
        for (j=j1; j<=j2; ++j)
            {
            edread(space,j);
            pxx=space+1;
//			i=0; while (pxx[i]==' ' && i<strlen(pxx)) i++; pxx=pxx+i;   // RS REM 25.3.2013     
//            if (*(pxx)=='R' && *(pxx+1)=='>') pxx=pxx+2;
            
            strcpy(rivi,pxx);
            
            muste_iconv(rivi,"","CP850");
            
            k=strlen(rivi)-1;
            while (k>0 && rivi[k]==' ') --k;
            rivi[k+1]='\n'; rivi[k+2]=EOS;
            
            fputs(rivi,ofile);
            if (ferror(ofile))
                {
                sur_print("\nCannot save RUNR.CLP!");
                WAIT;
                break;
                }
            }

        muste_fclose(ofile);
        
        if (g==100) return(1); // RS 25.3.2013
        
        if (outfile!=NULL) muste_evalsource_output("RUNR.CLP",outfile);
        else if (jo>0) muste_evalsource_output("RUNR.CLP",routtmp);
        else muste_evalsource_output("RUNR.CLP",muste_rout);
        
        if (jo>0) op_loadr(routtmp,jo);
        return(1);
        }

int muste_sbar=0;

static void op_sbar()
		{
        if (g>2)
            {
//            sur_print("\nCorrect form: R (Runs R code until next empty line)");
//            sur_print("\nCorrect form: R a");
            sur_print("\nCorrect form:  SBAR ON/OFF"); // X/Y/XY");
            WAIT; return;
    		}
    	
    	sprintf(str1,".muste.scrollbar(TRUE)");
    	if (g<2)  
    		{ 
    		if (muste_sbar) { muste_sbar=0; sprintf(str1,".muste.scrollbar(FALSE)"); } 
    		else muste_sbar=1;
    		}
    	else { if (strcmp("OFF",word[1])==0) sprintf(str1,".muste.scrollbar(FALSE)"); }
		muste_evalr(str1);    	
		}

static void op_hline()
		{
		extern int muste_headline;
        if (g>2)
            {
            sur_print("\nCorrect form:  HEADLINE ON/OFF"); // X/Y/XY");
            WAIT; return;
    		}
    	
    	if (g<2)  
    		{ 
    		muste_headline=1-muste_headline;
    		return;
    		}
    	else if (strcmp("OFF",word[1])==0) muste_headline=0;
    	else muste_headline=1;  	
		}

static void op_menu()
		{
        if (g>2)
            {
//            sur_print("\nCorrect form: R (Runs R code until next empty line)");
//            sur_print("\nCorrect form: R a");
            sur_print("\nCorrect form:  MENU ON/OFF"); // X/Y/XY");
            WAIT; return;
    		}
    	if (g<2) sprintf(str1,".muste.menu(\"ONF\")");    	
    	else sprintf(str1,".muste.menu(\"%s\")",parm[1]);

		muste_evalr(str1);    	
		}

int muste_infobar=0;
static void op_infobar()
		{
        if (g>2)
            {
//            sur_print("\nCorrect form: R (Runs R code until next empty line)");
//            sur_print("\nCorrect form: R a");
            sur_print("\nCorrect form:  INFOBAR ON/OFF"); // X/Y/XY");
            WAIT; return;
    		}
    	
    	sprintf(str1,".muste.statusbar(TRUE)");
    	if (g<2)  
    		{ 
    		if (muste_infobar) { muste_infobar=0; sprintf(str1,".muste.statusbar(FALSE)"); } 
    		else muste_infobar=1;
    		}
    	else 
    		{ 
    		if (strcmp("OFF",word[1])==0) 
    			{ 
    			muste_infobar=0; 
    			sprintf(str1,".muste.statusbar(FALSE)");
    			}  			
    		else if (strcmp("ON",word[1])==0) 
    			{
    			if (muste_infobar) return; 
    			muste_infobar=1; 
    			} 
    		}
		muste_evalr(str1);    	
		}


extern int muste_theme();
extern int op_softkeys();
extern int muste_eventloop_enable();
extern int muste_eventloop_disable();
void op_theme()
		{
        if (g<2)
            {
//            sur_print("\nCorrect form: R (Runs R code until next empty line)");
//            sur_print("\nCorrect form: R a");
            sur_print("\nCorrect form:  THEME CLASSIC/WHITE"); // X/Y/XY");
            WAIT; return;
    		}
     	if (strcmp("WHITE",word[1])==0)
     		{ 
     		muste_eventloop_disable();   		
     		muste_theme(0);  
     		g=2; sprintf(sbuf,"ON"); parm[1]=sbuf; op_menu();     		 
     		g=2; sprintf(sbuf,"OFF"); word[1]=sbuf; op_hline();     		
      		g=2; sprintf(sbuf,"OFF"); parm[1]=sbuf; op_softkeys();      		
     		g=2; sprintf(sbuf,"ON"); word[1]=sbuf; op_infobar();     		     		
     		g=2; sprintf(sbuf,"ON"); word[1]=sbuf; op_sbar();      		
     		}
     	else if (strcmp("WHITE-",word[1])==0)
     		{ 
     		muste_eventloop_disable();   		
     		muste_theme(0);  
     		g=2; sprintf(sbuf,"ON"); parm[1]=sbuf; op_menu();     		 
     		g=2; sprintf(sbuf,"OFF"); word[1]=sbuf; op_hline();     		
      		g=2; sprintf(sbuf,"OFF"); parm[1]=sbuf; op_softkeys();      		
     		g=2; sprintf(sbuf,"ON"); word[1]=sbuf; op_infobar();     		     		
     		g=2; sprintf(sbuf,"OFF"); word[1]=sbuf; op_sbar();      		
     		}    	
     	else 
     		{
     		muste_eventloop_enable();
     		muste_theme(1);
     		g=2; sprintf(sbuf,"ON"); word[1]=sbuf; op_hline();
     		g=1; op_softkeys();
     		g=2; sprintf(sbuf,"OFF"); parm[1]=sbuf; op_menu();
     		g=2; sprintf(sbuf,"OFF"); word[1]=sbuf; op_infobar();      		
     		g=2; sprintf(sbuf,"OFF"); word[1]=sbuf; op_sbar();     		
     		} 	
		}

static int muste_getfile()
	{
	char *destfile;
	int i;
	
	if (g<2)
		{
		sur_print("\nCorrect form:  GET url://sourcefile <destfile>"); // X/Y/XY");
		WAIT; return(1);
		}
	if (g<3)
		{
		i=strlen(word[1])-1;
		while (i>=0)
			{
			if (word[1][i]=='/') break;
			i--;
			} 
		destfile=word[1]+i+1;	
		}
	else destfile=word[2];
	sprintf(rivi,".muste.getfile(\"%s\")",word[1]);
	muste_evalr(rivi);
	muste_get_R_string(rivi,".muste$retrievedfile",LLENGTH);
	strcpy(nimi,destfile);
	sur_copy_file(rivi,nimi);
	return(1);
	}

static int split_sp(unsigned char *rivi,char **sana,int max) // RS 4.2.2013 unsigned
       {
       int g=0;
       int p;
       int edell=0; /* väli edellä */
       int len; 
       
       len=strlen((char *)rivi);     
       for (p=0; p<len; ++p)
               {               
               if(rivi[p]==' ')
                       {
                       if (edell==1)
                               {
                               rivi[p]=EOS;
                               ++g;
                               if (g>=max) return(max);
                               edell=0;
                               }
                       }
               else
                       {
                       if (edell==0)
                               {
                               sana[g]=(char *)(rivi+p);
                               edell=1;
                               }
                       }
               }
       if (edell==1) ++g;      
       return(g);
       }

static int w_load_codes(char *codefile,unsigned char *code)
       {
       int i;
       char nimi[LLENGTH];
       FILE *codes;

       strcpy(nimi,survo_path); strcat(nimi,"SYS/");
       strcat(nimi,codefile); if (strchr(nimi,'.')==NULL) strcat(nimi,".BIN");
       codes=muste_fopen(nimi,"rb");
       if (codes==NULL)
           {
           sprintf(sbuf,"\nFilter file %s not found!",nimi);
           sur_print(sbuf); WAIT; return(-1);
           }
       for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
       muste_fclose(codes);
       return(1);
       }

// WORDS L1,L2 -> #nwords #letters #digits #punct #special
//                    code:    0      1       2      3
// WORDS L1,L2,trim_words

static char word_chars[256];
static unsigned char code1[256];
static FILE *tmp;
static int op_words() // 23-24.8.2010
   {
   int i,j,h,k,n,len,f,ii;
   char *s[10000];
   unsigned char x[LLENGTH];
   int  n_letters,n_digits,n_punct,n_special;
   char fname[LNAME];
   char names[LLENGTH];
   char *p;
   char filter[LNAME];

   if (g<3) { sur_print("\nUsage: WORDS L1,L2"); WAIT; return(1); }
   jj1=edline2(word[1],1); if (jj1==0) return(-1);
   jj2=edline2(word[2],jj1); if (jj2==0) return(-1);
   if (g==3) // #words
       {
       i=spec_init(r1+r-1); if (i<0) return(-1);
       *fname=EOS;
       *filter=EOS;
       i=spfind("SAVE");
       if (i>=0)
           {
           strcpy(sbuf,spb[i]);
           if (strchr(sbuf,':')==NULL) { strcpy(fname,edisk); strcat(fname,sbuf); }
           else { strcpy(fname,sbuf); }
           tmp=muste_fopen(fname,"wt");
           if (tmp==NULL) { sur_print("\nFile error!"); WAIT; return(-1); }
           *filter=EOS;
           i=spfind("FILTER");
           if (i>=0)
               {
               strcpy(filter,spb[i]);
               i=w_load_codes(filter,code1); if (i<0) return(-1);
               }
           }
       *word_chars=EOS;
       i=spfind("CHARS");
       if (i>=0)
           {
           strcpy(word_chars,spb[i]);
           }
       strcpy(names,"word       len ");
       if (*word_chars!=EOS)
           {
           strcpy(sbuf,"  ");
           for (i=0; i<strlen(word_chars); ++i)
               {
               sbuf[1]=word_chars[i];
               strcat(names,sbuf);
               }
           }
       if (*fname!=EOS)
           fprintf(tmp,"%s\n",names);
       i=w_load_codes("CHARTYPE.BIN",code00); if (i<0) return(-1);
       n_letters=n_digits=n_punct=n_special=0;

       n=0;
       for (j=jj1; j<=jj2; ++j)
           {
           edread(x,j);            
           i=c2; while (i>0 && x[i]==' ') x[i--]=EOS;
           len=i;          
           for (k=1; k<=i; ++k)
               {
               h=(int)code00[(unsigned char)x[k]];
               switch (h)
                 {
               case 0: ++n_letters; break;
               case 1: ++n_digits; break;
               case 2: ++n_punct; break;
               case 3: ++n_special; break;
                 }
               }
           if (*filter!=EOS)
               {            
               for (i=1; i<=len; ++i)
                   x[i]=code1[(unsigned char)x[i]];
               }             
           k=split_sp(x+1,s,10000);        
           n+=k;
           if (*fname!=EOS)
               {
               for (i=0; i<k; ++i)
                   {
                   len=strlen(s[i]);
                   h=sprintf(sbuf,"%s %d",s[i],len);
                   if (*word_chars!=EOS)
                       {
                       for (ii=0; ii<strlen(word_chars); ++ii)
                           {
                           f=0; p=s[i];
                           while ((p=strchr(p,word_chars[ii]))!=NULL) { ++p; ++f; }
                           h+=sprintf(sbuf+h," %d",f);
                           }
                       }
                   fprintf(tmp,"%s\n",sbuf);
                   }
               }
           }
       k=n_letters+n_digits+n_punct+n_special;
       sprintf((char *)x,"WORDS %s,%s / #words=%d #chars=%d (%d,%d,%d,%d)",
           word[1],word[2],n,k,n_letters,n_digits,n_punct,n_special); // RS 4.2.2013 ADD (char *)
       edwrite(space,r1+r-1,1);
       edwrite(x,r1+r-1,1);

       if (*fname!=EOS) muste_fclose(tmp);
       return(1);
       }

   trim_words=atoi(word[3]);
   trim(0);
   return(1);
   }

static int op_chars()
   {
   int j,n;
   char chars[LNAME];
   char *p;

   if (g<4) { sur_print("\nUsage: CHARS L1,L2,xyz"); WAIT; return(1); }

   jj1=edline2(word[1],1); if (jj1==0) return(-1);
   jj2=edline2(word[2],jj1); if (jj2==0) return(-1);

   strcpy(chars,word[3]);
   n=0;
   for (j=jj1; j<=jj2; ++j)
       {
       edread(sbuf,j);
       p=sbuf+1;
       while (*p!=EOS)
           {
           if (strchr(chars,*p)!=NULL) ++n;
           ++p;
           }
       }
   edread(sbuf,r1+r-1);
   j=strlen(sbuf)-1;
   while (sbuf[j]==' ') sbuf[j--]=EOS;
   p=strstr(sbuf+1," /");
   if (p!=NULL) *p=EOS; else p=sbuf+strlen(sbuf);
   sprintf(p," / # of characters %s is %d.",word[3],n);
   edwrite(sbuf,r1+r-1,0);
   return(1);
   }


int muste_ediop(char *argv)
        {
        char OP[LNAME];
        int i,k;

        s_init(argv); // RS CHA argv1
        
        argv1=argv; // RS CHA argv1=argv1;

        strcpy(OP,word[0]); muste_strupr(OP);
        
// RS DEBUG Rprintf("ediop: %s\n",OP);       

        if (strcmp(OP,"ARIT")==0)
            { muste_fixme("FIXME: ARIT (multiprecision artihmetics) not implemented!"); return(1); }   
        if (strcmp(OP,"SORT")==0 || muste_strcmpi(OP,"-SORT")==0)
            { op_sort(); ret(); return(1); }            
        if (strncmp(OP,"TRIM",4)==0 || (*OP=='T' && strlen(OP)<3))
            { op_trim(); ret(); return(1); }
        if (strcmp(OP,"ERASE")==0)
            { op_erase(); ret(); return(1); }
        if (strcmp(OP,"CHANGE")==0)
            { op_change(); ret(); return(1); }                    
//      if (strcmp(OP,"COLOR")==0)      siirretty editoriin 30.12.2000
//          { op_color(); ret(1); }
//      if (strcmp(OP,"DIR")==0)
//          { op_dir(); ret(1); }
        if (strcmp(OP,"MOVE")==0)
            { op_move(); ret(); return(1); }
        if (strcmp(OP,"FORM")==0)
            { op_form(); ret(); return(1); }
        if (strcmp(OP,"PUTEND")==0)
            { op_putend(); ret(); return(1); }
        if ((*OP=='C' || *OP=='L') && strchr("+-*/%",OP[1])!=NULL)
            { op_cplus(); ret(); return(1); }
        if (strcmp(OP,"LINEDEL")==0 || strcmp(OP,"!LINEDEL")==0) // RS ADD !LINEDEL
            { i=op_linedel(); if (i<0) { ret(); return(1); } else return(1); } // RS CHA return


		codeconv=0;
		
		i=spec_find("ENCODING",muste_encoding,LLENGTH-1); // RS 14.3.2013
        if (i<=0) *muste_encoding=EOS;
        else codeconv=999;

       	if (strcmp(OP,"LOADW")==0) codeconv=2; // codeconv 8.4.2001
        if (strcmp(OP,"SAVEW")==0) codeconv=1;

        muste_unix=0; // 17.9.2008
        if (strcmp(OP,"LOADU")==0) { muste_unix=1; codeconv=2; }
        if (strcmp(OP,"SAVEU")==0) { muste_unix=1; codeconv=1; }

        if (strcmp(OP,"LOADP")==0 || codeconv==2)
            {
            k=op_loadp();
            s_end(argv1);
            return(k); // RS 4.2.2013 1 -> k
            }

        if (strcmp(OP,"LOADP2")==0) /* 24.7.1998 */
            {
            k=op_loadp2();
            s_end(argv1);
            return(k); // RS 4.2.2013 1 -> k
            }

        if (strcmp(OP,"SAVEP")==0 || codeconv==1)
            {
            k=op_savep(0);
            return(k); // RS 4.2.2013 1 -> k
            }

        if (strcmp(OP,"SAVEP2")==0) /* 24.7.1998 */
            {
            k=op_savep(1);
            return(k); // RS 4.2.2013 1 -> k
            }

        if (strcmp(OP,"CODES")==0)
            { op_codes(); s_end(argv1); return(1); }
        if (strcmp(OP,"CONVERT")==0)
            { op_convert(); s_end(argv1); return(1); }
        if (strcmp(OP,"NCOPY")==0)
            { op_ncopy(); return(1); }
        if (strcmp(OP,"UPDATE")==0)
            { op_update(); s_end(argv1); return(1); }
        if (strncmp(OP,"TXT",3)==0)
            { op_txt(); s_end(argv1); return(1); }
        if (strcmp(OP,"TRANSP")==0)
            { op_transp(); s_end(argv1); return(1); }
        if (strcmp(OP,"INTERP")==0)
            { op_interp(); s_end(argv1); return(1); }
        if (strcmp(OP,"TONES")==0)
            { op_tones(); return(1); }
        if (strcmp(OP,"VFIND")==0)
            { op_vfind(); s_end(argv1); return(1); }
        if (strcmp(OP,"PCOPY")==0)
            { op_pcopy(); return(1); }
        if (strcmp(OP,"DELF")==0)
            { op_delf(); return(1); }
        if (strcmp(OP,"STRDIST")==0)
            { op_strdist(); s_end(argv1); return(1); }
        if (strcmp(OP,"REVERSE")==0)  // 16.5.2007
            { op_reverse(); s_end(argv1); return(1); }
        if (strncmp(OP,"TRANSPO",7)==0)  // 19.11.2007
            { op_transpose(); s_end(argv1); return(1); }

        if (strcmp(OP,"R")==0)
            { op_runr(); s_end(argv1); return(1); }        
        if (strcmp(OP,"SBAR")==0)
            { op_sbar(); s_end(argv1); return(1); }
        if (strcmp(OP,"INFOBAR")==0)
            { op_infobar(); s_end(argv1); return(1); }            
        if (strcmp(OP,"HEADLINE")==0)
            { op_hline(); s_end(argv1); return(1); }             
        if (strcmp(OP,"MENU")==0)
            { op_menu(); s_end(argv1); return(1); }  
        if (strcmp(OP,"THEME")==0)
            { op_theme(); s_end(argv1); return(1); }   
        if (strcmp(OP,"WORDS")==0)
            { op_words(); s_end(argv1); return(1); }  
        if (strcmp(OP,"CHARS")==0)
            { op_chars(); s_end(argv1); return(1); } 
        if (strcmp(OP,"GET")==0)
            { muste_getfile(); s_end(argv1); return(1); }             
        return(0);
        }



