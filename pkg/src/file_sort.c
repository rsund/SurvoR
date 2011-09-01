/* fsort.c 26.4.1986/SM (13.12.1991) (12.2.95) (10.2.2008)
   FILE SORT
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

// #define SKMAX 10  -25.5.2007
   #define SKMAX 20
#define MAXBLOCK 65500
#define FILEMAX 12


static SURVO_DATA d1;

/*
FILE SORT <data> BY <list of sort keys> TO <new data file>
0    1     2     3   4  5               3+nsk  4+nsk
                        nsk-1 kpl.
*/

static char *key;   /* (malloc) lajitteluavaimet */
static unsigned int *ikey; /* avainten osoitteet */
static int nsk,slen;
static int sk[SKMAX];
static int neg[SKMAX];
static int sl[SKMAX],su[SKMAX]; /*  rajat: [sl:su]  */
static int sp[SKMAX];
static char stype[SKMAX];
static long n;
static unsigned int nsort;
static char code[256]; // RS REM unsigned
static char codefile[LLENGTH];
static char *word4;
static long samplesize=0L;
static int n_osat;
static unsigned int koko;
static long nhav;
static unsigned int workspace;
static int save;
static int prind=1;
static int sort_key_is_string=1; // 26.6.2001 KEY_SAVED

static FILE *sortf;
static long nf[FILEMAX];
static FILE *osaf[FILEMAX];
static int filemax;
static int file1,file2,nfiles;

static FILE *uusi;
static char s_keyvar[9];
static char s_keystring[LNAME];
static long s_paikka;
static int s_keyvar_nr;
static SURVO_DATA d2;

static FILE *codes;

/* RS REM
static char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "FILTER", "NSORT", "WORKSPACE", "SAVE", "KEY_SAVED",
                 "FILEMAX", "PRIND", "!" };
static char **specs=specs0;
*/

static int vertailu(int k0,int k)
        {
        int t,h=0;
        int len;
        double da;
        long la;
        float fa;
        int ia;

        if (nf[k0]==0) return(1);
        if (nf[k]==0) return(0);

        for (t=0; t<nsk; ++t)
            {
            len=su[t]-sl[t]+1;
            switch (stype[t])
                {
              case 'S':
                h=strncmp(key+k0*slen+sp[t],key+k*slen+sp[t],len); break;
              case '8':
                da=*(double *)(key+k0*slen+sp[t])-*(double *)(key+k*slen+sp[t]);
                if (neg[t]) da=-da;
                h=-1;
                if (da==0.0) h=0;
                else if (da>0.0) h=1; break;
              case '4':
                fa=*(float *)(key+k0*slen+sp[t])-*(float *)(key+k*slen+sp[t]);
                if (neg[t]) fa=-fa;
                h=-1;
                if (fa==0.0) h=0;
                else if (fa>0.0) h=1; break;
              case '2':
                ia=*(short *)(key+k0*slen+sp[t])-*(short *)(key+k*slen+sp[t]);
                if (neg[t]) ia=-ia;
                h=-1;
                if (ia==0) h=0;
                else if (ia>0) h=1; break;
              case '1':
                ia=*(unsigned char *)(key+k0*slen+sp[t])-*(unsigned char *)(key+k*slen+sp[t]);
                if (neg[t]) ia=-ia;
                h=-1;
                if (ia==0) h=0;
                else if (ia>0) h=1; break;
              case 'L':
                la=*(long *)(key+k0*slen+sp[t])-*(long *)(key+k*slen+sp[t]);
                if (neg[t]) la=-la;
                h=-1;
                if (la==0L) h=0;
                else if (la>0L) h=1; break;
              default: sprintf(sbuf,"\n %c puuttuu!!! %d",stype[t],t);
                       sur_print(sbuf); sur_getch();

                }
            if (h<0) return(0);
            if (h>0) return(1);
            }
    sur_print("\n???"); sur_getch(); return(0); // RS CHA printf -> sur_print
        }


static void lue_hav(FILE *tied,char *p)
        {
        int i;
        for (i=0; i<slen; ++i) { *p=getc(tied); ++p; }
        }

static int lomita(int file1,int file2,int kierros,int nro)
        {
        int i,k,nfi,ix;
        long unf,l;
        char nimi[LNAME];
        char nimi2[LNAME];
        char *p;

        if (prind)
          {
          sprintf(sbuf,"\nMerging files SORT%d%d - %d%d.TMP to SORT%d%d.TMP: ",
                                  kierros-1,file1,kierros-1,file2,kierros,nro);
          sur_print(sbuf);
          }
        nfi=file2-file1+1;

/* jos nfi==1, riittÑÑ nimen muutos */
        if (nfi==1)
            {
            sprintf(nimi,"%sSORT%d%d.TMP",etmpd,kierros,nro);
            osaf[0]=muste_fopen(nimi,"rb");
            if (osaf[0]!=NULL)
                {
/*              sprintf(nimi,"DEL %sSORT%d%d.TMP",
                                 etmpd,kierros,nro);
                system(nimi);
*/
                sprintf(nimi,"%sSORT%d%d.TMP",
                                 etmpd,kierros,nro);
                remove(nimi);
                }
/*          sprintf(nimi,"REN %sSORT%d%d.TMP SORT%d%d.TMP",
                     etmpd,kierros-1,file1,  kierros,nro);
            system(nimi);
*/
            sprintf(nimi,"%sSORT%d%d.TMP",etmpd,kierros-1,file1);
            sprintf(nimi2,"%sSORT%d%d.TMP",etmpd,kierros,nro);
            i=rename(nimi,nimi2);
            return(1);
            }
        unf=0L;
        for (i=0; i<nfi; ++i)
            {
            sprintf(nimi,"%sSORT%d%d.TMP",etmpd,kierros-1,file1+i);
            osaf[i]=muste_fopen(nimi,"rb");
            if (osaf[i]==NULL)
                {
                sprintf(sbuf,"\nCannot open temporary file %s",nimi); sur_print(sbuf);
                WAIT; return(-1);
                }
            p=(char *)&l;
            for (k=0; k<sizeof(int); ++k) { *p=getc(osaf[i]); ++p; } // RS CHA 64-BIT sizeof(long)
            unf+=l; nf[i]=l;
            p=key+i*slen;
            lue_hav(osaf[i],p);
            }
        if (prind)
            {
            sprintf(sbuf,"n=%d",(int)unf); sur_print(sbuf); // RS CHA %ld -> %d
            }
        sprintf(nimi,"%sSORT%d%d.TMP",etmpd,kierros,nro);
        sortf=muste_fopen(nimi,"wb");
        if (sortf==NULL)
            {
            sprintf(sbuf,"\nCannot create temporary file %s",nimi); sur_print(sbuf);
            WAIT; return(-1);
            }
        p=(char *)&unf;
        for (i=0; i<sizeof(int); ++i) // RS CHA 64-BIT sizeof(long)
            {
            putc((int)*p,sortf);
            ++p;
            }
/* siirrot */
            while (1)
                {
                ix=0;
                for (i=1; i<nfi; ++i)
                    {
                    k=vertailu(ix,i);
                    if (k) ix=i;
                    }
                if (nf[ix]==0) break;
                p=key+ix*slen;
                for (i=0; i<slen; ++i) { putc((int)*p,sortf); ++p; }
                if (nf[ix]>0)
                    {
                    p=key+ix*slen;
                    lue_hav(osaf[ix],p); --nf[ix];
                    }
                }
        muste_fclose(sortf);
        for (i=0; i<nfi; ++i)
            {
            muste_fclose(osaf[i]);

/*          sprintf(nimi,"DEL %sSORT%d%d.TMP",etmpd,kierros-1,file1+i);
            system(nimi);
*/
            sprintf(nimi,"%sSORT%d%d.TMP",etmpd,kierros-1,file1+i);
            remove(nimi);
            }
        return(1);
        }

static int lomitus()
        {
        int i,kierros;
        int nfiles2;

        filemax=4;
        i=spfind("FILEMAX");
        if (i>=0)
            {
            filemax=atoi(spb[i]);
            if (filemax<2) filemax=2;
            if (filemax>FILEMAX) filemax=FILEMAX;
            }
        kierros=0;
        nfiles=n_osat;
        if (prind)
          {
          sprintf(sbuf,"\n\nMerging %d files ...",n_osat); sur_print(sbuf);
          }
        while (1)
            {
            ++kierros;
            if (prind)
                {
                sprintf(sbuf,"\nRound %d:",kierros+1); sur_print(sbuf);
                }
            file1=0;
            nfiles2=0;
            while (file1<nfiles)
                {
                file2=file1+filemax-1;
                if (file2>=nfiles) file2=nfiles-1;
                i=lomita(file1,file2,kierros,nfiles2);
                if (i<0) return(-1);
                ++nfiles2;
                file1=file2+1;
                }
            if (nfiles2==1) break;
            nfiles=nfiles2;
            }
        return(kierros);  /* jotta tiedetÑÑn nimi */
        }


static int osatalletus(unsigned int nsort,int k)
        {
        unsigned int j;
        int i;
        char *p;
        char nimi[LLENGTH];
// RS REM        char x[LLENGTH];
        long lnsort;

        sprintf(nimi,"%sSORT0%d.TMP",etmpd,k);
        if (prind)
            {
            sprintf(sbuf,"\nSaving sort keys (n=%u) in %s",nsort,nimi);
            sur_print(sbuf);
            }
        sortf=muste_fopen(nimi,"wb");
        if (sortf==NULL)
            {
            sprintf(sbuf,"\nCannot create temporary file %s",nimi); sur_print(sbuf);
            WAIT; return(-1);
            }
        lnsort=nsort;
        p=(char *)&lnsort;
        for (i=0; i<sizeof(int); ++i) // RS CHA 64-BIT sizeof(long)
            {
            putc((int)*p,sortf);
            ++p;
            }
        for (j=0; j<nsort; ++j)
            {
            p=key+ikey[j];
            for (i=0; i<slen; ++i)
                {
                putc((int)*p,sortf);
                ++p;
                }
            if (ferror(sortf))
                {
                sprintf(sbuf,"\nCannot save temporary data in %s",nimi);
                sur_print(sbuf); WAIT; return(-1);
                }
            }
        muste_fclose(sortf);
        return(1);
        }


static int load_codes(char *codefile,char *code)
        {
        int i;
        char x[LLENGTH];

        strcpy(x,codefile);
        if (strchr(x,':')==NULL && *x!='.')
            { strcpy(x,survo_path); strcat(x,"SYS\\"); strcat(x,codefile); }

        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nCode conversion file %s not found!",x); sur_print(sbuf);
            WAIT; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose(codes);
        return(1);
        }


static void conv(char *sana) // RS REM unsigned
        {
        int i;

        for (i=0; i<strlen(sana); ++i) sana[i]=code[(int)sana[i]];
        }

static void ei_tilaa(char *s)
        {
        sprintf(sbuf,"\nNot space enough for file %s!",s); sur_print(sbuf);
        WAIT;
        }

static int talletus(char *nimi,int kierros)
/* kierros;  SORT*0.TMP  *=kierros */
        {
        int i,k;
        long j,alku;
        long nro;
        char pathname[LNAME];
        char nimi2[LNAME];
        char x[LLENGTH];
        char *p;
        long pros;
        long pros_step; // 22.2.2008
        int s_len=0,s_pos=0;
        char *q;

        strcpy(pathname,nimi);
        if (strchr(nimi,':')==NULL)
            { strcpy(pathname,edisk); strcat(pathname,nimi); }
        if (strchr(pathname+strlen(pathname)-4,'.')==NULL)
            strcat(pathname,".SVO");

        uusi=muste_fopen(pathname,"wb");
        if (uusi==NULL)
            {
            sprintf(sbuf,"\nCannot save file %s!",pathname); sur_print(sbuf);
            WAIT; return(-1);
            }

        fi_rewind(&(d1.d2));
        alku=(long)(d1.d2.data);
        for (j=0; j<alku; ++j)
            {
            putc(getc(d1.d2.survo_data),uusi);
            if (ferror(uusi)) { ei_tilaa(pathname); return(-1); }
            }
        s_paikka=ftell(uusi);

        fi_rewind(&(d1.d2));

        if (n_osat==0) { muste_fclose(uusi); return(1); } /* 19.4.1997 */

        if (n_osat==1) nhav=nsort;
        else
            {
            sprintf(nimi2,"%sSORT%d0.TMP",etmpd,kierros);
            sortf=muste_fopen(nimi2,"rb");
            if (sortf==NULL)
                {
                sprintf(sbuf,"\nCannot read temporary file %s",nimi); sur_print(sbuf);
                WAIT; return(-1);
                }

            p=(char *)&nhav;
            for (i=0; i<sizeof(int); ++i) // RS CHA 64-BIT sizeof(long)
                {
                *p=getc(sortf);
                ++p;
                }


            }

        *s_keyvar=EOS;
        i=spfind("KEY_SAVED");

        if (i>=0 && sort_key_is_string)
            {
            strncat(s_keyvar,spb[i],8);
            muste_fclose(uusi);
            i=data_open2(pathname,&d2,1,1,1);

            s_keyvar_nr=varfind2(&d2,s_keyvar,0);
            if (s_keyvar_nr>=0)
               {
               i=d2.varlen[s_keyvar_nr];
               if (i<slen-4)
                   {
                   sprintf(sbuf,"Length of %s is too small (%d). %d bytes required!",
                                    s_keyvar,i,slen-4);
                   sur_print(sbuf); WAIT; return(-1); // RS CHA exit(0); -> return(-1);
                   }
               }
            else
               {
               i=create_newvar(&d2,s_keyvar,'S',slen-4);
               if (i<0) return(-1); // RS CHA exit(0); -> return(-1);
               s_keyvar_nr=i;
               }


            s_keyvar_nr=varfind2(&d2,s_keyvar,0);
            s_pos=d2.d2.varpos[s_keyvar_nr];
            s_len=d2.d2.varlen[s_keyvar_nr];

            *sbuf=EOS; k=0;
            for (i=0; i<nsk-1; ++i)
                k+=sprintf(sbuf+k,"%s&",word[4+i]);
            sbuf[k-1]=EOS; // viim. & pois
            update_varname(&d2,s_keyvar_nr,sbuf);

            data_close(&d2);
            uusi=muste_fopen(pathname,"r+b");
            muste_fseek(uusi,s_paikka,SEEK_SET);
            }
        else if (i>=0) // sort_key_is_string=0
            {
            sur_print("\nSORT_SAVED works only when the combined sort key is a string!");
            sur_print("\nThe data will be sorted in any case.");
            WAIT;
            }
        if (samplesize) { if (samplesize<nhav) nhav=samplesize; else samplesize=nhav; }

        sprintf(sbuf,"\n\nSaving %ld sorted records to file %s ...",nhav,pathname); sur_print(sbuf);
//      pros=1;
        pros=1; pros_step=(long)((double)nhav/100.0);
        for (j=0; j<(long)nhav; ++j)
            {
            if (n_osat==1)
                {
                nro=*(long *)(key+ikey[(unsigned int)j]+sp[nsk-1]);
// printf("\nkey=%.4s|",key+ikey[(unsigned int)j]); getch();
                if (*s_keyvar)
                    strncpy(s_keystring,key+ikey[(unsigned int)j],slen-4);
                }
            else
                {
                p=x;
                lue_hav(sortf,p);
                nro=*(long *)(x+sp[nsk-1]);
                if (*s_keyvar)
                    strncpy(s_keystring,x,slen-4);
                }
/***********************************
            if (nhav<1000L)
                {
                if (prind) { sprintf(sbuf," %ld",nro); sur_print(sbuf); }
                }
            else if (100L*j>=pros*nhav)
                {
                if (prind) { sprintf(sbuf," %d%%",pros); sur_print(sbuf); }
                ++pros;
                }
******************************/
            if (prind && j>=pros*pros_step)
                {
                sprintf(sbuf," %d%%",(int)pros); sur_print(sbuf); // RS ADD (int)
                ++pros;
                }
            fi_gets(&(d1.d2),d1.d2.obs,d1.d2.len,
                        (long)(d1.d2.data+(long)(nro-1)*(long)(d1.d2.len)));

            if (*s_keyvar)
                {
                p=d1.d2.obs+s_pos;
                for (i=0; i<s_len; ++i) *p++=' ';
                p=d1.d2.obs+s_pos; q=s_keystring;
                for (i=0; i<slen-4; ++i) *p++=*q++;
                }

            for (i=0; i<d1.d2.len; ++i)
                putc((int)d1.d2.obs[i],uusi);

            if (ferror(uusi)) { ei_tilaa(pathname); return(-1); }

            }
        if (n_osat>1)
            {
            muste_fclose(sortf);
            remove(nimi2);
/*          sprintf(sbuf,"DEL %s",nimi2);
            system(sbuf);
*/
            }
        muste_fclose(uusi);
        return(1);
        }


static void shell_sort(unsigned int j1,unsigned int j2,int t)
        {
        unsigned int n,h,i,k;
        char ind;
        int len;
// RS REM        char *p;
        int iso;

        n=j2-j1+1;
        iso=0; if (n>100) iso=1;

        if (iso && prind)
            {
            if (t<nsk-1) { sprintf(sbuf,"\n%s:",word[4+t]); sur_print(sbuf); }
            else { sprintf(sbuf,"\nOriginal order:"); sur_print(sbuf); }
            sprintf(sbuf," %u-%u ",j1+1,j2+1); sur_print(sbuf);
            sprintf(sbuf," n=%u",n); sur_print(sbuf);
            }
        len=su[t]-sl[t]+1;
        h=n;

        switch(stype[t])
            {
          case 'S':
            while (h>1)
                {
                h/=2; if (iso && prind) { sprintf(sbuf," %u",h); sur_print(sbuf); }
                while (1)
                    {
                    ind='1';
                    for (i=j1; i<=j2-h; ++i)
                        {
                        if (strncmp(key+ikey[i]+sp[t],key+ikey[i+h]+sp[t],len)>0)
                            {
                            k=ikey[i]; ikey[i]=ikey[i+h]; ikey[i+h]=k;
                            ind='0';
                            }
                        }
                    if (ind=='1') break;
                    }
                }
            break;

          case '8':
            while (h>1)
                {
                h/=2; if (iso && prind) { sprintf(sbuf," %u",h); sur_print(sbuf); }
                while (1)
                    {
                    ind='1';
                    for (i=j1; i<=j2-h; ++i)
                        {
                  if ((neg[t] &&
                      *(double *)(key+ikey[i]+sp[t])<*(double *)(key+ikey[i+h]+sp[t]))
                      ||
                      (!neg[t] &&
                      *(double *)(key+ikey[i]+sp[t])>*(double *)(key+ikey[i+h]+sp[t])))
                            {
                            k=ikey[i]; ikey[i]=ikey[i+h]; ikey[i+h]=k;
                            ind='0';
                            }
                        }
                    if (ind=='1') break;
                    }
                }
            break;

          case '4':
            while (h>1)
                {
                h/=2; if (iso && prind) { sprintf(sbuf," %u",h); sur_print(sbuf); }
                while (1)
                    {
                    ind='1';
                    for (i=j1; i<=j2-h; ++i)
                        {
                  if ((neg[t] &&
                      *(float *)(key+ikey[i]+sp[t])<*(float *)(key+ikey[i+h]+sp[t]))
                      ||
                      (!neg[t] &&
                      *(float *)(key+ikey[i]+sp[t])>*(float *)(key+ikey[i+h]+sp[t])))
                            {
                            k=ikey[i]; ikey[i]=ikey[i+h]; ikey[i+h]=k;
                            ind='0';
                            }
                        }
                    if (ind=='1') break;
                    }
                }
            break;

          case '2':
            while (h>1)
                {
                h/=2; if (iso && prind) { sprintf(sbuf," %u",h); sur_print(sbuf); }
                while (1)
                    {
                    ind='1';
                    for (i=j1; i<=j2-h; ++i)
                        {
                  if ((neg[t] &&
                      *(short *)(key+ikey[i]+sp[t])<*(short *)(key+ikey[i+h]+sp[t]))
                      ||
                      (!neg[t] &&
                      *(short *)(key+ikey[i]+sp[t])>*(short *)(key+ikey[i+h]+sp[t])))
                            {
                            k=ikey[i]; ikey[i]=ikey[i+h]; ikey[i+h]=k;
                            ind='0';
                            }
                        }
                    if (ind=='1') break;
                    }
                }
            break;

          case '1':
            while (h>1)
                {
                h/=2; if (iso && prind) { sprintf(sbuf," %u",h); sur_print(sbuf); }
                while (1)
                    {
                    ind='1';
                    for (i=j1; i<=j2-h; ++i)
                        {
                  if ((neg[t] &&
                   *(unsigned char *)(key+ikey[i]+sp[t])<*(unsigned char *)(key+ikey[i+h]+sp[t]))
                      ||
                      (!neg[t] &&
                   *(unsigned char *)(key+ikey[i]+sp[t])>*(unsigned char *)(key+ikey[i+h]+sp[t])))
                            {
                            k=ikey[i]; ikey[i]=ikey[i+h]; ikey[i+h]=k;
                            ind='0';
                            }
                        }
                    if (ind=='1') break;
                    }
                }
            break;

          case 'L':
            while (h>1)
                {
                h/=2; if (iso && prind) { sprintf(sbuf," %u",h); sur_print(sbuf); }
                while (1)
                    {
                    ind='1';
                    for (i=j1; i<=j2-h; ++i)
                        {
                  if ((neg[t] &&
                      *(long *)(key+ikey[i]+sp[t])<*(long *)(key+ikey[i+h]+sp[t]))
                      ||
                      (!neg[t] &&
                      *(long *)(key+ikey[i]+sp[t])>*(long *)(key+ikey[i+h]+sp[t])))
                            {
                            k=ikey[i]; ikey[i]=ikey[i+h]; ikey[i+h]=k;
                            ind='0';
                            }
                        }
                    if (ind=='1') break;
                    }
                }
            break;

            } /* switch */
        }


static void sort1(unsigned int j1,unsigned int j2,int t)
        {
        unsigned int k1,k2;
        int len;

// printf("\nsort1: j1=%d j2=%d t=%d",j1,j2,t); getch();
        shell_sort(j1,j2,t);
        len=su[t]-sl[t]+1;

        k1=j1;
        while (k1<j2)
            {
            k2=k1+1;
            while (k2<=j2 && memcmp(key+ikey[k1]+sp[t],key+ikey[k2]+sp[t],len)==0)
                ++k2;
            --k2;

            if (k2>k1) sort1(k1,k2,t+1);
            k1=k2+1;
            }

        }


static void lajittelu()
        {
// RS REM        unsigned int k;

        if (prind) sur_print("\nInternal sorting ...");
        sort1(0,nsort-1,0);
        }

static int lue_avaimet(long lj1,long lj2)
        {
        int i,h;
        long j;
        unsigned int jj;
        long nro;
        char x[LLENGTH];
// RS REM        char *p;
        float *fp; double *dp;
        if (prind)
          {
          sprintf(sbuf,"\nLoading cases %ld - %ld and making sort keys...",lj1,lj2);
          sur_print(sbuf);
          }

        jj=0; nsort=0;
        for (j=lj1; j<=lj2; ++j)
            {
            if (unsuitable(&d1,j)) continue;
       /*   sprintf(sbuf," %ld",j); sur_print(sbuf);  */
            for (i=0; i<nsk-1; ++i)
                {
                data_alpha_load(&d1,j,sk[i],x);
           //   if (stype[i]=='S') conv(x);
                switch (stype[i])
                    {
                  case 'S': conv(x); break;
                  case '4': fp=(float *)x; if (*fp==0.0) *fp=0.0; break;
                  case '8': dp=(double *)x; if (*dp==0.0) *dp=0.0; break;
                  default: break;
                    }
                for (h=0; h<su[i]-sl[i]+1; ++h)
                    key[jj+sp[i]+h]=x[sl[i]+h];
                }
/*  printf("\n%.*s",slen-2,key+jj);     */
            nro=j;
       /*   p=(char *)&nro;      */
            *(long *)&key[jj+sp[nsk-1]]=nro;

/*          key[jj+sp[nsk-1]]=*p;
            key[jj+sp[nsk-1]+1]=*(p+1);
*/
            ikey[nsort++]=jj;
            jj+=slen;
            }
        return(1);
        }

static void tilanpuute()
        {
        PR_EBLD;
        sur_print("\nNot enough memory for FILE SORT!");
        PR_ENRM; WAIT;
        }

static int varaa_tilat()
        {
        long l;

        l=n*(long)slen;
        if (l>(long)workspace)
            {
            n_osat=l/workspace+1;
            koko=l/(long)(n_osat*slen)+1;
            }
        else
            {
            n_osat=1;
            koko=n;
            }
/*
printf("\nl=%ld n=%ld slen=%d n_osat=%d koko=%u",l,n,slen,n_osat,koko); getch();
*/
        key=muste_malloc((unsigned int)((koko+1)*slen));
        if (key==NULL) { tilanpuute(); return(-1); }
        ikey=(unsigned int *)muste_malloc((koko+1)*sizeof(unsigned int));
        if (ikey==NULL) { tilanpuute(); return(-1); }
        return(1);
		}        

static int avaimet()
        {
        int i;
        char *p,*q,*q1;
        char x[LLENGTH];

        nsk=0; i=4;
        slen=0;
        while (i<g && muste_strcmpi(word[i],"TO")!=0)
            {
            strcpy(x,word[i]);
            p=x;
            neg[nsk]=0; if (*p=='-') { ++p; ++word[i]; neg[nsk]=1; }
            q=strchr(p,'[');
            if (q==NULL)
                {
                sk[nsk]=varfind(&d1,p); if (sk[nsk]<0) return(-1);
                sl[nsk]=0; su[nsk]=d1.varlen[sk[nsk]]-1;
                }
            else
                {
                *q=EOS;
                q1=strchr(q+1,':');
                if (q1==NULL)
                    {
                    sprintf(sbuf,"\n: missing in %s",word[i]); sur_print(sbuf);
                    WAIT; return(-1);
                    }
                *q1=EOS; sl[nsk]=atoi(q+1)-1;
                q=q1;
                q1=strchr(q+1,']');
                if (q1==NULL)
                    {
                    sprintf(sbuf,"\n] missing in %s",word[i]); sur_print(sbuf);
                    WAIT; return(-1);
                    }
                *q1=EOS; su[nsk]=atoi(q+1)-1;
                sk[nsk]=varfind(&d1,p); if (sk[nsk]<0) return(-1);
                }
/*
   printf("\ni=%d sk=%d sl=%d su=%d neg=%d",nsk+1,sk[nsk],sl[nsk],su[nsk],neg[nsk]); getch();
*/
            sp[nsk]=slen; slen+=su[nsk]-sl[nsk]+1;
            stype[nsk]=d1.vartype[sk[nsk]][0];
            if (stype[nsk]!='S') sort_key_is_string=0;
            ++i; ++nsk;
            }  /* while */
        sk[nsk]=-1; /* alkup.jÑrjestysnro. */
        sl[nsk]=0; su[nsk]=3; sp[nsk]=slen; slen+=4; neg[nsk]=0;
        stype[nsk]='L';
        ++nsk;
        return (1);
        }


void muste_file_sort(int argc,char *argv[])
        {
        int i,k;
        long lj1,lj2;

// RS ADD variable init
nsk=slen=0;
n=0;
nsort=0;
samplesize=0L;
n_osat=0;
koko=0;
nhav=0;
workspace=0;
save=0;
prind=1;
sort_key_is_string=1; // 26.6.2001 KEY_SAVED
filemax=0;
file1=file2=nfiles=0;
s_paikka=0;
s_keyvar_nr=0;

        if (argc==1) return;
        s_init(argv[1]);

        if (g<7)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE SORT <data> BY <list_ of_sort_keys> TO <new_data_file>");
            WAIT; return;
            }

        word4=word[4];  /* 1. avain mahd. - merkin kanssa talteen */
        i=data_open3(word[2],&d1,1,1,1,0); if (i<0) { s_end(argv[1]); return; }
        if (d1.type!=2)
            {
            sprintf(sbuf,"\n%s must be a Survo data file!",word[2]);
            sur_print(sbuf); WAIT; return;
            }
        i=spec_init(r1+r-1); if (i<0) return;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);
        i=conditions(&d1); if (i<0) { s_end(argv[1]); return; }

        strcpy(codefile,"SORTCODE.BIN");
        i=spfind("FILTER");
        if (i>=0) strcpy(codefile,spb[i]);
        i=spfind("NSORT");
        if (i>=0) samplesize=atol(spb[i]);
        workspace=MAXBLOCK; i=spfind("WORKSPACE");
        if (i>=0)
            {
            workspace=atoi(spb[i]);
            if (workspace<1000) workspace=MAXBLOCK;
            }

        save=0;
        i=spfind("SAVE");  /* SAVE=1 creates sorted file also for N=0 */
        if (i>=0) save=atoi(spb[i]);

        i=load_codes(codefile,code); if (i<0) return;
        i=avaimet(); if (i<0) return;
        n=d1.l2-d1.l1+1;
        if (muste_strcmpi(word[2],word[4+nsk])==0)
            {
            sprintf(sbuf,"\nThe original file %s cannot be overwritten",word[2]); sur_print(sbuf);
            sur_print("\nby the sorted file!");
            WAIT; return;
            }
        i=varaa_tilat(); if (i<0) return;

        if (!prind) sur_print("\nInternal sorting...");
        lj1=d1.l1;
        for (k=0; k<n_osat; ++k)
            {
            lj2=lj1+koko-1; if (lj2>d1.l2) lj2=d1.l2;
            i=lue_avaimet(lj1,lj2); if (i<0) return;
                {
                if (nsort>1) lajittelu();
                if (n_osat>1)
                    {
                    i=osatalletus(nsort,k);
                    if (i<0) return;
                    }
                }
            lj1+=koko;
            }
        if (n_osat>1)
            {
            i=lomitus();
            if (i<0) return;
            }
        i=talletus(word[4+nsk],i); if (i<0) return;
        data_close(&d1);
        i=data_open2(word[4+nsk],&d1,0,1,1); if (i<0) return;
    /*  i=fi_open3(word[4+nsk],&(d1.d2),0,1,1,1); if (i<0) return; */
        fi_rewind(&d1.d2);
        fi_puts(&d1.d2,&nhav,4,22L);
        rem_update(&d1,"SORT:",word4);
        data_close(&d1);
        if (nhav==0L && !save)
            {
            sur_print("\nNo cases accepted! No data to be sorted!");
            WAIT;
            fi_find(word[4+nsk],&(d1.d2),sbuf);
            fi_close(&(d1.d2));
            remove(sbuf);
            }
        }

