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

static char *argv1;

/* LINEDEL START */
static FILE *edt1,*edt2,*edt3;
static char nimi1[LNAME],nimi2[LNAME],nimi3[LNAME];
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
/* LINEDEL END */



/* FORM START */
static char maskline[LLENGTH], *fmask[MAXCOL];
static int pos[MAXCOL], len[MAXCOL];
static char type[MAXCOL];
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

static char *sortlist;
static char **sortp;
static char *zz;
static int *zs2;
static int sort_len,n;
static char x[LLENGTH];
static char luku[LLENGTH];
static int apos[10],alen[10],an;
static char code[256]; // RS REM unsigned
static int shadow;

static char *specs0[]={ "FILTER",
                                             "!" };
static char **specs=specs0;

static FILE *codes; /* -4.1.1997 defined as local! */


/*  ctrim.c 16.10.1985/SM (27.12.1991)
    +trimtav+trim2+sh+trimp+trim2p /STACK:4000
    TRIMx L1,L2,lev
    TRIMx L1,L2,lev,Pi (P=proportional pitch, file PITCH.BIN)
                     default for i is 1
                     pitch_ind=256*(i-1)
 */





/* TRIM START */ 
static int rsh; /* vain sh.c */
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
static unsigned char code0[256];

static char code[256], vokaalit[256];


static FILE *pfile; /* -4.1.1997 defined as local! */
static FILE *codes;

static char rivi[LLENGTH], srivi[LLENGTH];
static int vajaus;

static char rivi[LLENGTH], srivi[LLENGTH];
static int vajaus, bl;

/* TRIM END */


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
        codes=fopen(nimi,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nFilter file %s not found!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        fclose(codes);
        return(1);
        }


/* trimtav.c 20.10.1985/SM (21.10.1985) tavuttaa sanan  (9.11.1991) */


static void load_codes2(unsigned char *code)
        {
        int i;
        char x[LLENGTH];

        strcpy(x,survo_path); strcat(x,"SYS/SORTCODE.BIN"); // RS CHA \\ -> /
        codes=fopen(x,"rb");
        if (codes==NULL)
            {
            for (i=0; i<256; ++i) code[i]=(unsigned char)i;
            return;
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        fclose(codes);
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
    int len,lenq;

// printf("\nsana=%s|",sana); getch();
    strcpy(sana,sana0);
    len=strlen(sana);
    for (i=0; i<len; ++i) sana[i]=code0[sana[i]];

    rewind(trimfile);
    while (1)
        {
        p=sana1; q=sana2;
        while (1)
            {
            *p=(char)fgetc(trimfile);
// printf("\n*p=%c %d|",*p,(int)*p); getch();
            if (feof(trimfile)) return(-1);
            if ((int)*p==10) break;
            *q=*p; ++q;
            if (*p!='-') ++p;
            }
        *p=EOS; *q=EOS;
// printf("\nsana1=%s|",sana1);
// printf("\nsana2=%s|",sana2); getch();
        if (strncmp(sana,sana1,len)==0)
            {
//         printf("\nsana=%s sana1=%s sana2=%s",sana,sana1,sana2);
// getch();

            q=sana2; i=0;
            strcat(q,"-");
            while (1)
                {
                if (*q!='-') ++i;
                ++q;
                if (i==len) break;
                }
// printf("\nq=%s|",q); getch();
            if (*q=='-') --q;
            while (i>0)
                {
                if (*q=='-') break;
                --i; --q;
// printf("\ni_tavu=%d|",i); getch();
                }
// printf("\nsana=%s i=%d|",sana,i); getch();
            if (i<2) return(0);
            return(i);
            }

        }
    return(-1);
    }
    
static int vokaali(unsigned char kirjain)
        {
        return((int)(vokaalit[code[kirjain]]-'0'));
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
            korjaa(); return(-2); // RS CHA exit(1) -> return(-2);  /* 2.12.1991 */
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
        return(1);
        }
        

static int strpitch(unsigned char *s)
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

            if (*inx==EOS) { tulosta(); break; }
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
            if ((pitch_unit && (strpitch(outx)+strpitch(sana)>pitch_unit*lev))
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
            strcat(outx,sana); if (sp_loppuun) strcat(outx," ");
            strcat(soutx,ssana); if (sp_loppuun) strcat(soutx," ");
            }
        if (vaeli) tulosta();
        inj=maxj+1;
        return(vaeli);  /* 9.5.90 */
        }

static void not_space()
        {
        PR_EBLD;
        sur_print("\nNot space enough in the memory for TRIM!");
        WAIT; PR_ENRM;
        }

static int varaa_zz()
        {
        zzs=(unsigned int *)malloc(sizeof(int)*(ed2+1));
        zz=malloc(sizeof(char)*ed1*(ed2+edshad));
        if (zz==NULL) { not_space(); return(-1); }
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
//            printf("\nsbuf=%s|",sbuf); getch();
                trimfile=fopen(sbuf,"rt");
                if (trimfile==NULL)
                    {
                    sprintf(x,"Trim file %s not found",sbuf);
                    sur_print(x); WAIT; exit(0);
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
            i=trim_kpl(tav); if (i<0) return(-2);
            }
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
        pfile=fopen(x,"rb");    /* b lis‰tty 15.11.90 */
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
        fclose(pfile);

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
        char *p;
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
        int i,j;
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
            if (k=isnumber(cplus_sana[i])) return(i);
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
        double result;
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
        double result;
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
            printf("\nNo numeric mask for the result!");
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
        char xs[LLENGTH];
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
        int i,k;
        char rivi[ELE];
        char *sana[3];
        char nimi[LNAME];

        if (strchr(tfile,':')==NULL) { strcpy(nimi,edisk); strcat(nimi,tfile);
                                       strcpy(tfile,nimi);
                                     }
        strcpy(nimi,tfile);
        if (strchr(nimi+strlen(nimi)-4,'.')==NULL) strcat(nimi,".EDT");
        text=fopen(nimi,"rt");
        if (text==NULL)
            {
            text=fopen(tfile,"rt");
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
            fseek(text,(int)((unsigned int)j*(unsigned int)ted1),0); // RS FIXME 64bit long->int
muste_fixme("\nFIXME: MOVE fseek 64 bit?");            
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
            if (feof(text)) exit(0);
            p=strchr(x,'|');
            if (p==NULL) { sur_print("\nError in edit file!");
                           WAIT; exit(0);
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
        char nimi[LNAME];
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
        fclose(text);
        }



static void op_move()
        {
        int i;
        int *pi;
        
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
    int len;

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
        i=strlen(x)-2; x[i]=EOS; // crlf pois!
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
    char x2[LLENGTH];
    int talletus;
    int first=1;
    int k;

    k=e_step; // if >k empty lines, k lines left empty

    valinta=0;
    dlab=0; lab0=0;

    i=l_rivi(x,&lab); jo_luettu=1;
    p=strchr(x,'|');
    x0=(p-x)+1;

    while (1)
        {
        if (!jo_luettu) { i=l_rivi(x,&lab); if (i<0) return(1); }
        jo_luettu=0;
// printf("\nlab=%d valinta=%d x=%s|",lab,valinta,x); getch();
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

// printf("\nx+x0=%.30s|",x+x0); getch();
// printf("\nx=%d|",(int)*(x+x0+1)); getch();
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
    char x2[LLENGTH];
    int talletus;
    char varjot[LLENGTH];
    int labdel=2;

    valinta=0;
    dlab=0; lab0=0;

    i=l_rivi(x,&lab); jo_luettu=1;
    p=strchr(x,'|');
    x0=(p-x)+1;

    while (1)
        {
        if (!jo_luettu) { i=l_rivi(x,&lab); if (i<0) return(1); }
        jo_luettu=0;
// printf("\nlab=%d valinta=%d x=%s|",lab,valinta,x); getch();
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
           /*     dlab+=k2-lab0; */ lab0=lab;
                  break;
                  }

              if (lab<=k2)
                  {
                  talletus=0;
                  lab2=lab;
           /*     dlab+=lab-lab0; */ lab0=lab;
// printf("\nx=%c|",*(x+x0)); getch();
                  if (strchr(sana,*(x+x0))==NULL)
                      {
//                    --dlab;
                      t_rivi(x,lab-dlab);
                      talletus=1;
                      }
                  else
                      {
                      ++dlab;
                      if (*nimi3) dt_rivi(x,labdel++);
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
    char x2[LLENGTH];
    int talletus;
    int labdel=2;
    char y[LLENGTH];

    int multi; // 26.5.2006
    char *mword[100];

// printf("\ng=%d|",g); getch();
    multi=0;

    j=r1+r-1; // 8.7.2006
    edread(y,j);
// printf("\nx=%s|",y); getch();
    g=3; i=3; p=y+1;
    while (1)
        {
        p=strchr(p,'"');
        if (p==NULL) break;
        ++p;
        word[i]=p;
        ++g;
        p=strchr(p,'"');
        if (p!=NULL) { ++p; *p=EOS; ++p; }
        }

// printf("\ng=%d|",g); getch();
    if (g>4)
        {
        multi=g-4+1;
        if (multi>10) { sur_print("\nMax. # of words is 100!");
                        WAIT; exit(0);
                      }
        for (i=0; i<multi; ++i)
            {
            p=word[i+3];
            if (*p=='"') ++p;
            j=strlen(p)-1;
            if (p[j]=='"') p[j]=EOS;
            mword[i]=p;
// printf("\nword(%d)=%s|",i,mword[i]); getch();
            }

        }
    valinta=0;
    dlab=0; lab0=0;

    n_sp_end=0;
    i=strlen(sana)-1; while (i>0 && sana[i]==' ') { --i; ++n_sp_end; }

    i=l_rivi2(x,&lab); jo_luettu=1;
    p=strchr(x,'|');
    x0=(p-x)+1;

    while (1)
        {
        if (!jo_luettu) { i=l_rivi2(x,&lab); if (i<0) return(1); }

        jo_luettu=0;
// printf("\nlab=%d valinta=%d x=%s|",lab,valinta,x); getch();
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
                  talletus=0;
                  lab2=lab;
                  lab0=lab;

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
    int i,j;
    char x[LLENGTH];
    int lab; //  0=varjorivi
    int lab2;
    int valinta;
    int dlab;
    int jo_luettu;
    int lab0;
    int dlabdel=k1-2;

// printf("\nk1=%d k2=%d",k1,k2); getck();
    valinta=0;
    dlab=0; lab0=0;
    jo_luettu=0;
    while (1)
        {
        if (!jo_luettu) { i=l_rivi(x,&lab); if (i<0) return(1); }
        jo_luettu=0;
// printf("\nlab=%d valinta=%d x=%s|",lab,valinta,x); getch();
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
    char x2[LLENGTH];
    int talletus;
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
    dlab=0; lab0=0;

    i=l_rivi2(x,&lab); jo_luettu=1;
    p=strchr(x,'|');
    x0=(p-x)+1;

    while (1)
        {
        if (!jo_luettu) { i=l_rivi2(x,&lab); if (i<0) return(1); }
        jo_luettu=0;

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
                  talletus=0;
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
    int lab; //  0=varjorivi
    int lab2;
    int dlab;
    int lab0;
    char *p;
    int x0;
    char x2[LLENGTH];
    int talletus;
    int jstep=k1;

    dlab=0; lab0=0;

    luettu=0;
    j=0;
    while (1)
        {
        ++j;
// printf("\nj=%d ",j);
        i=l_steprivi(j,x); if (i<0) return(1);

        if (j==jstep && j>=k1 && j<=k2)
            {
            jstep+=step; ++dlab;
//          if (*x==EOS) { printf("DEL (empty)"); getch(); }
//          else
            if (*x!=EOS)
                {
            //  printf("DEL %.30s",x); getch();
                i=l_rivi(stx,&steplab); if (i<0) return(1);
                if (steplab!=0) { luettu=1; }
            //  else
            //      printf("DEL %.30s"); getch();
                }
            }
        else
            {
//          if (*x==EOS) { printf("(empty)"); getch(); }
//          else
            if (*x!=EOS)
                {
                t_rivi(x,steplab-dlab);
//              printf("%.30s",x); getch();
                i=l_rivi(stx,&steplab); if (i<0) return(1);
                if (steplab!=0) { luettu=1; }
                else
                    {
                    t_rivi(stx,0);
//                  printf("%.30s"); getch();
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

// printf("\nargv1=%s|",argv1); getch();

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
                if (step<=0) exit(0);
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
//   printf("\nvarjo=%s|",varjosana);getch();

                    }
                }
            else
                {
                sur_print("\nError in LINEDEL command!");
                WAIT; return(1);
                }
            }
// printf("\ntyyli=%d",tyyli); getch();
        if (tyyli==5)
            { k1=r1+r; k2=last_empty; }
        else
            {
            k1=edline2(word[1],1,1); if (!k1) return(1);
            k2=edline2(word[2],k1,1); if (!k2) return(1);
            }
        sprintf(nimi1,"%sSURVOMM.EDT",argv1);
        sprintf(nimi2,"%sSURVOMD.EDT",argv1);

        edt1=fopen(nimi1,"rb");
        edt2=fopen(nimi2,"wb");

        fgets(sbuf,LLENGTH,edt1);
// printf("\nsbuf=%s|",sbuf); getch();
        fputs(sbuf,edt2);

        i=spec_find("DEL_SAVE",x,LNAME-1);
        if (i>0)
            {
            strcpy(nimi3,x);
            if (strchr(x,':')==NULL)
                { strcpy(nimi3,edisk); strcat(nimi3,x); }
            if (strchr(x,'.')==NULL) strcat(nimi3,".EDT");
            edt3=fopen(nimi3,"wb");
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
        if (*nimi3) fclose(edt3);
        i=fclose(edt2);
        i=fclose(edt1);
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
            sprintf(sbuf,"\nMask missing on line %d",k); // RS CHA printf -> sur_print
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

static int sort_conv(unsigned char *p,unsigned char *code)
        {

        while (*p)
            {
            *p=code[*p];
            ++p;
            }
        return(1);
        }

static int numcomp (char **arg1,char **arg2)
        {
        double a;

        a=*(double *)(*arg1)-*(double *)(*arg2);
        if (a<0) return(-1);
        if (a==0) return(0);
        return(1);
        }

static int stringcomp (char **arg1,char **arg2)
        {
        return(strcmp(*arg1,*arg2));
        }
        
static void avaimet(int k,int *apos,int *alen,int *an)
        {
        char kirjain;
        int i;
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
        zz=malloc(n*(ed1-1)+1);
        if (zz==NULL) { ei_tilaa(); return(-1); }
        sortp=(char **)malloc(n*sizeof(char *)+1);
        if (sortp==NULL) { ei_tilaa(); return(-1); }
        sortlist=malloc(n*lenkey+1);
        if (sortlist==NULL) { ei_tilaa(); return(-1); }
        if (shadow)
            {
            zs2=(int *)malloc(n*sizeof(int)+1);
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
        zz=malloc(n*(ed1-1)+1);
        if (zz==NULL) { ei_tilaa(); return(-1); }
        sortp=(char **)malloc(n*sizeof(char *)+1);
        if (sortp==NULL) { ei_tilaa(); return(-1); }
        sortlist=malloc(n*lenkey+1);
        if (sortlist==NULL) { ei_tilaa(); return(-1); }
        if (shadow)
            {
            zs2=(int *)malloc(n*sizeof(int)+1);
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
            for (i=0; i<sort_len; ++i) *(sortlist+k+i)=luku[i];
            sprintf(sortlist+k+sort_len,"%*d",sizeint,j-j1);
/*
            *(int *)(sortlist+k+sort_len)=j-j1;
            *(sortlist+k+lenkey-1)=EOS;
*/
            sortp[j-j1]=sortlist+k;
            }

        qsort((char *)sortp,n,sizeof(char *),stringcomp);

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
        z2=malloc(size);
        zs2=(int *)malloc((ed2+1)*sizeof(int));
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
        int i;
        char x[LLENGTH];

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

int muste_ediop(char *argv)
        {
        char OP[LNAME];
        int i;

        s_init(argv); // RS CHA argv[1]
        
        argv1=argv; // RS CHA argv1=argv[1];

        strcpy(OP,word[0]); muste_strupr(OP);
        
// RS DEBUG Rprintf("ediop: %s\n",OP);       
        
        if (strcmp(OP,"SORT")==0 || muste_strcmpi(OP,"-SORT")==0)
            { op_sort(); ret(1); return(0); }            
        if (strncmp(OP,"TRIM",4)==0 || (*OP=='T' && strlen(OP)<3))
            { op_trim(); ret(1); return(0); }
        if (strcmp(OP,"ERASE")==0)
            { op_erase(); ret(1); return(0); }
        if (strcmp(OP,"CHANGE")==0)
            { op_change(); ret(1); return(0); }
//      if (strcmp(OP,"COLOR")==0)      siirretty editoriin 30.12.2000
//          { op_color(); ret(1); }
//      if (strcmp(OP,"DIR")==0)
//          { op_dir(); ret(1); }
        if (strcmp(OP,"MOVE")==0)
            { op_move(); ret(1); return(0); }
        if (strcmp(OP,"FORM")==0)
            { op_form(); ret(1); return(0); }
        if (strcmp(OP,"PUTEND")==0)
            { op_putend(); ret(1); return(0); }
        if ((*OP=='C' || *OP=='L') && strchr("+-*/%",OP[1])!=NULL)
            { op_cplus(); ret(1); return(0); }
        if (strcmp(OP,"LINEDEL")==0)
            { i=op_linedel(); if (i<0) { ret(1); return(0); } else return(0); } // RS CHA return

        return(1);
        }



