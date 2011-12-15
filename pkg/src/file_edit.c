/* edit.c 8.2.1986/SM (13.9.1992)
   FILE EDIT <Survo data file>
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define PRMODE PR_EIN2

extern void conv();
extern int load_codes();

extern int special;
extern int survo_ferror;

static SURVO_DATA_FILE dat;

static int m_act;
static int m;
static long n;
static long havainto;     /* havainto nro */
static int muuttuja;
static int muutokset;
static int sortvar;   /* lajittelumuuttuja */
static int n_haku;    /* lîydetyt tapaukset */
static int suojaus;
static char tiedotus[LLENGTH]; /* ohjeteksti alarivillÑ */
static char lopetus[]="To stop, press EXIT!";
static char hakutieto[LLENGTH];
static char hakuavain[64];
static char edsana[LLENGTH];  /* editoitava kenttÑ */
static char vertsana[LLENGTH]; /* vertailukenttÑ */
static char kopioitava[8]; /* kopioitava havainto nro */
static int pos,pituus;        /* pos=sijainti ed.kentÑssÑ pituus=kentÑn pituus */
static int mriville;      /* kenttien max lkm /rivi */
static char *ptila;       /* taulukkotilan alku */
static int *v;            /* muuttujien valintavektori v[i]=i. kenttÑ */
static int *rivi;         /* kenttien rivit */
static int *nimsar;       /* kenttÑnimien sarakkeet */
static int *nimpit;       /* nimen pituus */
static int *varsar;       /* itse kenttien alkusarakkeet */
static int *varpit;       /* kentÑn pituus */
static int *muutos;       /* muutokset kentittÑin (ei kÑytîssÑ) */
static double *min;       /* sallitut minimit */
static double *max;       /* sallitut maksimit */
static int *strarvo;      /* S-kentÑn sallittujen arvojen luettelon alku (21.12.91) */
static int viimeiseen;    /* 0=1.hav. 1=viim.hav.+1 2=viim.hav. */
static int kokonimi;      /* 1: nÑytetÑÑn myîs nimen kommentit, jos mriville=1 */
static int vain_selailu;
// RS REM static unsigned int max_obs; /* vain rajoitetuissa versioissa */
static long n_alku;
static unsigned char code[256];
static int koodit=0;


static void tilanpuute()
        {
        PR_EBLD;
        sur_print("\nNot enough memory for FILE EDIT!");
        PR_ENRM; WAIT;
        }

static void putsaa()
        {
        LOCATE(r3+2,1);
        PR_ENRM; sprintf(sbuf,"%.*s",c3+7,space); sur_print(sbuf);
        }

static void float4_muunto(float f,char *sana)
          {
          char *p;

          sprintf(sana,"%g",f);
          p=strchr(sana,'.'); if (p==NULL) return;
          p=strchr(sana,'e'); if (p!=NULL) return;
          p=sana; if (*p=='-') ++p;
          if (strlen(p)<7) return;
          p+=6; if (*p=='9')
              {
              while (*p=='9') --p;
              if (*p=='.') return;
              ++(*p);
              }
          else if (*p=='0')
              {
              while (*p=='0') --p;
              }
          *(p+1)=EOS;
          }

static void rajat(int i,double *pmin,double *pmax)
        {
        char *p,*q;
        char teksti[LLENGTH];
        char *sana[2];
        int k;

        strcpy(teksti,dat.varname[i]);
        p=strchr(teksti,'{'); if (p==NULL) return;
        q=strchr(p,'}'); if (q==NULL) return;
        *q=EOS;
        k=split(p+1,sana,2);
        if (k<2) return;
        *pmin=atof(sana[0]); *pmax=atof(sana[1]);
        }

static void strarvot(int i,int *pi)   /* 22.12.91 */
        {
        char *p,*q;
        char teksti[LLENGTH];

        *pi=0;
        strcpy(teksti,dat.varname[i]);
        if (teksti[9]=='~') return; /* 20.1.1996 */
        p=strchr(teksti+8,'['); if (p==NULL) return;
        q=strchr(p,']'); if (q==NULL) return;
        *pi=p-teksti;
        }

static void poimi(long j,int i,char *sana)
        {
        int vi;
        double luku;
        float luku4;
        int h,len;
        char type;
        int miss;
        int pit;

        vi=v[i];
        type=dat.vartype[vi][0];
        miss=0;
        pit=varpit[i];
        if (j==n+1)
            { strncpy(sana,space,pit); sana[pit]=EOS; return; }
        if (type=='S')
            {
            fi_alpha_load(&dat,j,vi,sana);
            h=strlen(sana);
            if (h<pit) strncat(sana,space,pit-h);
            }
        else
            {
            fi_alpha_load(&dat,j,vi,sana);
            switch (type)
                {
              case '1':
                luku=(unsigned char)(*sana);
                if (luku==MISSING1) { miss=1; break; }
                fconv(luku,"",sana);
                break;
              case '2':
                luku=*(short *)sana;
                if (luku==MISSING2) { miss=1; break; }
                fconv(luku,"",sana);
                break;
              case '4':
                if (not_float(sana)) { miss=1; break; }
                luku4=*(float *)sana;
                if (luku4>MISSING4/10.0) { miss=1; break; }
                float4_muunto(luku4,sana);
                break;
              case '8':
                if (not_double(sana)) { miss=1; break; }
                luku=*(double *)sana;
                if (luku>MISSING8/10.0) { miss=1; break; }
                fconv(luku,"",sana);
                break;
                }

            if (miss)
                {
                strncpy(sana,space,pit); sana[pit]=EOS; return;
                }

            len=strlen(sana);
            if (len<pit)
                {
                for (h=len; h<pit; ++h) sana[h]=' '; sana[pit]=EOS;
                }
            }
        }

static void osoita(int i)
/* i;  akt. muuttuja 0,1,2,...  */
        {
        poimi(havainto,i,edsana);
        strcpy(vertsana,edsana);
        pos=1; pituus=varpit[i];

/*      if (ibm)    */

            write_string(edsana,pituus,'7',rivi[i],varsar[i]);
/*      else
            {
            LOCATE(rivi[i],varsar[i]);
            PR_EINV;
            sprintf(sbuf,"%s",edsana); sur_print(sbuf);
            }
*/
        LOCATE(rivi[i],varsar[i]);

        if (dat.vartype[v[muuttuja]][2]=='P' && havainto<=n_alku) suojaus=1; else suojaus=0;
        }



static void hav_disp(long j)
        {
        int i;
        char sana[LLENGTH];
        char x[LLENGTH];
// RS REM        extern unsigned int max_obs;

int ibm=1;

        fi_rewind(&dat);
        CURSOR_OFF;

/*      if (ibm)
            {
*/
            i=sprintf(x,"Record #%6ld  ",j);
            if (dat.vartype[0][0]=='S')
                {
                fi_alpha_load(&dat,j,0,sana);
                i+=sprintf(x+i,"%.16s",sana);
                }
            write_string(x,i,' ',2,30);
/*
            }
        else
            {
            PR_ENRM; LOCATE(2,30); sprintf(sbuf,"Record #%6ld  ",j); sur_print(sbuf);
            if (dat.vartype[0][0]=='S')
                {
                fi_alpha_load(&dat,j,0,sana);
                sprintf(sbuf,"%.16s",sana); sur_print(sbuf);
                }
            }
*/
        for (i=0; i<m_act; ++i)
            {
            muutos[i]=0;
            poimi(j,i,sana);

            if (ibm)
                {
                strcpy(x,sana); strncat(x,space,varpit[i]-strlen(sana));
                write_string(x,varpit[i],'7',rivi[i],varsar[i]);
                }
            else
                {
                LOCATE(rivi[i],varsar[i]); PR_EINV;
                sprintf(sbuf,"%s%.*s",sana,(int)(varpit[i]-strlen(sana)),space); sur_print(sbuf);
                }

            if (dat.vartype[v[i]][2]=='P' && j<=n_alku)
                {
                if (ibm)
                    {
                    write_string("P",1,'1',rivi[i],varsar[i]-1);
                    }
                else
                    {
                    LOCATE(rivi[i],varsar[i]-1); PR_EBLD;
                    sur_print("P");
                    }
                }
            else if (j>n_alku)
                {
                LOCATE(rivi[i],varsar[i]-1);
                sur_print(" ");
                }

            }
        CURSOR_ON;
        LOCATE(rivi[muuttuja],varsar[muuttuja]);
        PR_ENRM;
        osoita(muuttuja);
        muutokset=0;
        }


static void varinfo()
        {
        int i,vi;

        for (i=0; i<m_act; ++i)
            {
            vi=v[i];
            switch (dat.vartype[vi][0])
                {
              case '1': varpit[i]=3;
                        min[i]=0.0; max[i]=254.0;
                        rajat(vi,&min[i],&max[i]);
                        break;
              case '2': varpit[i]=6;
                        min[i]=-32768.0; max[i]=32767.0;
                        rajat(vi,&min[i],&max[i]);
                        break;
              case '4': varpit[i]=14;
                        min[i]=-1e37; max[i]=1e37;
                        rajat(vi,&min[i],&max[i]);
                        break;
              case '8': varpit[i]=18;
                        min[i]=-1e305; max[i]=1e305;
                        rajat(vi,&min[i],&max[i]);
                        break;
              case 'S': varpit[i]=dat.varlen[vi];
                        min[i]=-1e305; max[i]=1e305;
                        strarvot(vi,&strarvo[i]);   /* 21.12.91 */
                        break;
                }

            }
        }


static int edit_init()
        {
        int i,k;
        int riv,sar,mriv;
        int rivinpit=c3+6;
        char label[9];

        PR_ENRM; CLS;
        LOCATE(2,1);
        sprintf(sbuf,"Data file %s",active_data); sur_print(sbuf);
        LOCATE(2,70);
        sprintf(sbuf,"N=%ld",n); sur_print(sbuf);

        if (m_act<r3-1) mriville=1; else mriville=10; /* tilap. */

        riv=3; sar=1; mriv=0;
        for (i=0; i<m_act; ++i)
            {
            if (mriv>=mriville || sar+8+2+varpit[i]>rivinpit)
                {
                mriv=0;
                ++riv;
                sar=1;
                if (riv>r3)
                    {
                    PR_EBLD;
                    sur_print("\nNot space enough on the screen");
                    sur_print("\nfor all active fields in file!");
                    PR_ENRM; WAIT; return(-1);
                    }
                }

            LOCATE(riv,sar);
            rivi[i]=riv; nimsar[i]=sar; varsar[i]=sar+9;
            if (mriville>1)
                {
                strncpy(label,dat.varname[v[i]],8);
                label[8]=EOS; k=8; while (label[k-1]==' ' && k>0) label[--k]=EOS;
                nimsar[i]=sar+8-k;
                LOCATE(riv,nimsar[i]); sprintf(sbuf,"%s ",label); sur_print(sbuf);
                }
            else
                {
                sprintf(sbuf,"%.8s ",dat.varname[v[i]]); sur_print(sbuf);
                if (kokonimi)
                    {
                    LOCATE(riv,varsar[i]+varpit[i]+1);
                    sprintf(sbuf,"%.*s",c3+8-varsar[i]-varpit[i],dat.varname[v[i]]+8);
                    sur_print(sbuf);
                    }
                }
            sar+=9+varpit[i];
            ++mriv;
            }

        n_alku=dat.n; if (n_alku==0L) n_alku=100000L;  /* tyhjÑ tied. ei suojattu */
        return(1);
        }


static int kirjoitukseen()
        {
        int i;

        i=fi_to_write(word[2],&dat);
        if (i<0) return(-1);
        vain_selailu=0;
        return(1);
        }


static void missing_save(SURVO_DATA_FILE *s,long j)
       {
        int i;
        char   miss1=(char)MISSING1;
        int    miss2=MISSING2;
        float  miss4=MISSING4;
        double miss8=MISSING8;
        double x;

        for (i=0; i<(*s).m; ++i)
            {
            switch ((*s).vartype[i][0])
                {
              case '1': x=(double)miss1; fi_save(s,j,i,&x); break;
              case '2': x=(double)miss2; fi_save(s,j,i,&x); break;
              case '4': x=(double)miss4; fi_save(s,j,i,&x); break;
              case '8': x=(double)miss8; fi_save(s,j,i,&x); break;
              case 'S': fi_save(s,j,i,space); break;
                }
            }
        }

static void n_update(SURVO_DATA_FILE *s,int n) // RS CHA long n -> int n
/* n;  new obs.# */
        {
        fi_rewind(s);
        fi_puts(s,&n,sizeof(int),22);  // RS CHA 64BIT sizeof(long) -> sizeof(int) 22L -> 22
        (*s).n=n;
        LOCATE(2,70); sprintf(sbuf,"N=%6d",n); sur_print(sbuf); // RS CHA %6ld -> %6d
        }

static int talletus()
        {
        int vi;
        double x;
        char type;
// RS REM        extern int survo_ferror;

        if (!muutokset) return(1);
/*      if (strcmp(vertsana,edsana)==0) { muutokset=0; return(1); } */
        if (havainto==n+1)
            {
            ++n;
            n_update(&dat,n);
            missing_save(&dat,n);
            }
        vi=v[muuttuja];
        type=dat.vartype[vi][0];
        if (type=='S')
            {
            if (strarvo[muuttuja])   /* predefined values to be tested  22.12.91 */
                {
                char s[LLENGTH];
                char *p;
                char *sana[EP4];
                int m,i,len;

                len=strlen(edsana);
                while (len>0 && edsana[len-1]==' ') --len;

                strcpy(s,dat.varname[vi]+strarvo[muuttuja]+1);
                p=strchr(s,']'); /* never NULL (strarvot()) */
                *p=EOS;
                m=split(s,sana,EP4);
                if (m)
                    {
                    for (i=0; i<m; ++i)
                        {
                        if (strncmp(edsana,sana[i],len)==0) break;
                        }
                    if (i==m)
                        {
                        if (etu==2)
                            {
                            strcpy(tut_info,"___@8@FILE EDIT@Not a permitted value!@");
                            return(-1); // RS CHA exit(1);
                            }
                        LOCATE(r3+2,1);
                        PR_EBLD;
                        sprintf(sbuf,"%s is not a permitted value for %.8s",
                                edsana,dat.varname[v[muuttuja]]);
                        sur_print(sbuf);
                        BEEP;
                        sur_print("  Press any key!");
                        LOCATE(rivi[muuttuja],varsar[muuttuja]);
                        sur_getch(); // RS CHA nextch("") -> sur_getch()
                        putsaa();
                        osoita(muuttuja);
                        return(-1);
                        }
                    }
                }
            fi_save(&dat,havainto,vi,edsana);
            muutokset=0;
            return(1);
            }
        if (strncmp(edsana,space,varpit[muuttuja])==0)
            {
            switch (type)
                {
              case '1': x=(double)MISSING1; break;
              case '2': x=(double)MISSING2; break;
              case '4': x=MISSING4; break;
              case '8': x=MISSING8; break;
                }
            }
        else
            {
            x=atof(edsana);
            if (x<min[muuttuja] || x>max[muuttuja])
                {
                LOCATE(r3+2,1);
                PR_EBLD;
                if (x<min[muuttuja])
                    { sprintf(sbuf,"%.8s < %g (smallest permitted value)",
                            dat.varname[v[muuttuja]],min[muuttuja]);
                      sur_print(sbuf);
                    }
                else
                    { sprintf(sbuf,"%.8s > %g (greatest permitted value)",
                            dat.varname[v[muuttuja]],max[muuttuja]);
                      sur_print(sbuf);
                    }
                BEEP;
                sur_print("  Press any key!");
                LOCATE(rivi[muuttuja],varsar[muuttuja]);
                sur_getch(); // RS CHA nextch("")->sur_getch
                putsaa();
                osoita(muuttuja);
                return(-1);
                }
            }
        fi_save(&dat,havainto,vi,&x);
        muutokset=0;
        return(1);
        }

        
static void seur_muuttuja()
        {
        int i;
// RS REM        int seur_rivi;

        i=talletus(); if (i<0) return;
        if (muuttuja<m_act-1)
            { ++muuttuja; osoita(muuttuja); return; }
        if (havainto>=n+1) return;
        ++havainto;
        muuttuja=0;
        hav_disp(havainto);
        }

static void alas()
        {
        int i;
        int seur_rivi;

        i=talletus(); if (i<0) return;
        if (muuttuja<m_act-1)
            {
            i=muuttuja;
            while (i<m_act && rivi[i]==rivi[muuttuja]) ++i;
            if (i==m_act) return;
            seur_rivi=rivi[i];
            while (i<m_act && rivi[i]==seur_rivi && varsar[i]<varsar[muuttuja]) ++i;
            if (i==m_act || rivi[i]!=seur_rivi) --i;
            muuttuja=i; osoita(muuttuja); return;
            }
        }

static void edell_muuttuja()
        {
        int i;

        i=talletus(); if (i<0) return;
        if (muuttuja>0)
            {
            --muuttuja; osoita(muuttuja);
            }
        }

static void ylos()
        {
        int i;
        int edell_rivi;

        i=talletus(); if (i<0) return;
        if (muuttuja>0)
            {
            i=muuttuja;
            while (i>=0 && rivi[i]==rivi[muuttuja]) --i;
            if (i<0) return;
            edell_rivi=rivi[i];
            while (i>=0 && rivi[i]==edell_rivi && varsar[i]>varsar[muuttuja]) --i;
            if (i<0 || rivi[i]!=edell_rivi) ++i;
            muuttuja=i; osoita(muuttuja); return;
            }
        }

static int varaa_tilat()
        {
        char *p;
        long tila;

        tila=7L*(long)m_act*(long)sizeof(int)+2L*(long)m_act*(long)sizeof(double);
/*      if (tila>65535L) { tilanpuute(); return(-1); } */

        if (ptila!=NULL) muste_free(ptila);
        ptila=muste_malloc((unsigned int)tila);
        if (ptila==NULL) { tilanpuute(); return(-1); }

        v=(int *)ptila;
        rivi=(int *)(ptila+m_act*sizeof(int));
        nimsar=(int *)(ptila+2*m_act*sizeof(int));
        nimpit=(int *)(ptila+3*m_act*sizeof(int));
        varsar=(int *)(ptila+4*m_act*sizeof(int));
        varpit=(int *)(ptila+5*m_act*sizeof(int));
        muutos=(int *)(ptila+6*m_act*sizeof(int));

        p=ptila+7*m_act*sizeof(int);

        min=(double *)p;
        max=(double *)(p+m_act*sizeof(double));

        p+=2*m_act*sizeof(double);

        strarvo=(int *)muste_malloc(m_act*sizeof(int));         /* 22.12.91 */
        if (strarvo==NULL) { tilanpuute(); return(-1); }

        return(1);
        }



static void move_obs(SURVO_DATA_FILE *s,long j1,long j2,long j3)
/* j1;  1st  obs. to be moved 
   j2;  last obs. to be moved
   j3;  destination */
        {
        int i;
        FILE *apu;
        long j,k;

        if (j3==j1) return;
        apu=muste_fopen("SURVO.XXX","w+b");
        for (j=j1; j<=j2; ++j)
            {
            fi_rewind(s);
            fi_gets(s,(*s).obs,(*s).len,
                        (long)((*s).data+(j-1L)*(long)(*s).len));
            for (i=0; i<(*s).len; ++i)
                putc((int)((*s).obs[i]),apu);
            }

        if (j3<j1)
            {
            k=j3-j1;
            for (j=j1-1; j>=j3; --j)
                {
                fi_rewind(s);
                fi_gets(s,(*s).obs,(*s).len,
                            (long)((*s).data+(j-1L)*(long)(*s).len));
                fi_rewind(s);
                fi_puts(s,(*s).obs,(*s).len,
                      (long)((*s).data+(j+j2-j1+1L-1L)*(long)(*s).len));
                }
            }
        else
            {
            k=j3-j2-1;
            for (j=j2+1; j<=j3-1; ++j)
                {
                fi_rewind(s);
                fi_gets(s,(*s).obs,(*s).len,
                            (long)((*s).data+(j-1L)*(long)(*s).len));
                fi_rewind(s);
                fi_puts(s,(*s).obs,(*s).len,
                      (long)((*s).data+(j-(j2-j1+1L)-1L)*(long)(*s).len));
                }
            }

        rewind(apu);
        for (j=j1; j<=j2; ++j)
            {
            for (i=0; i<(*s).len; ++i)
                (*s).obs[i]=(char)getc(apu);
            fi_rewind(s);
            fi_puts(s,(*s).obs,(*s).len,
                        (long)((*s).data+(j+k-1L)*(long)(*s).len));
            }
        muste_fclose(apu);
        havainto=j1+k;
        }

static void delete_obs(SURVO_DATA_FILE *s,long j1,long j2)
/* j1;  1st  obs. to be deleted
   j2;  last obs. to be deleted */
        {
        long j;

        for (j=j2+1L; j<=(*s).n; ++j)
            {
            fi_rewind(s);
            fi_gets(s,(*s).obs,(*s).len,
                        (long)((*s).data+(j-1L)*(long)(*s).len));
            fi_rewind(s);
            fi_puts(s,(*s).obs,(*s).len,
                        (long)((*s).data+(j-j2+j1-2L)*(long)(*s).len));

        LOCATE(r3+2,70); sprintf(sbuf,"%d%% done!",(int)(100*(j-j2)/((*s).n-j2))); // RS ADD (int)
                                sur_print(sbuf);
            }
        n_update(s,(*s).n-j2+j1-1L);
        }


static void k_insertl()     /* move observations j1,j2 to j3 */
        {
        int i;
        char vastaus[64];
        char *vastsana[2];
        char kysymys[64];
        long j1,j2,j3;

        *vastaus=EOS;
        LOCATE(r3+2,1); PR_EBLD;
        prompt("Observations to be moved (first,last)? ",vastaus,16);
        i=split(vastaus,vastsana,2);
        if (i==0) { putsaa(); osoita(muuttuja); return; }
        j1=j2=atol(vastsana[0]);
        if (i==2) j2=atol(vastsana[1]);
        if (j2<j1) j2=j1;
        putsaa();
        LOCATE(r3+2,1); PR_EBLD;
        strcpy(kysymys,"New place (Record #) for obs. ");
        strcat(kysymys,vastsana[0]); strcat(kysymys," ? ");
        *vastaus=EOS;
        prompt(kysymys,vastaus,16);
        j3=atol(vastaus);
        if (j3<1L || j3>n+1L || (j3>=j1 && j3<=j2) )
            { putsaa(); osoita(muuttuja); return; }
        move_obs(&dat,j1,j2,j3);
        putsaa();
        hav_disp(havainto);
        }

static void k_deletel()
        {
        int i;
        char vastaus[64];
        char *vastsana[2];
        long j1,j2;

        *vastaus=EOS;
        LOCATE(r3+2,1); PR_EBLD;
        prompt("Observations to be deleted (first,last)? ",vastaus,16);
        i=split(vastaus,vastsana,2);
        if (i==0) { putsaa(); osoita(muuttuja); return; }
        j1=j2=atol(vastsana[0]);
        if (i==2) j2=atol(vastsana[1]);
        if (j2<j1) j2=j1;
        if (j2>n) j2=n; /* 31.7.90 */
        delete_obs(&dat,j1,j2);
        n=dat.n; if (havainto>n) havainto=n;
        if (n==0) havainto=1L;
        putsaa();
        hav_disp(havainto);
        }

static void k_copy()
        {
        long hav;

        LOCATE(r3+2,1); PR_EBLD;
        prompt("Observation to be copied here? ",kopioitava,7);
        hav=atol(kopioitava);
        if (hav<1L || hav>n)
            { putsaa(); osoita(muuttuja); return; }
        if (havainto==n+1)
            { ++n; n_update(&dat,n); }
        fi_rewind(&dat);
        fi_gets(&dat,dat.obs,dat.len,
              (long)(dat.data+(hav-1L)*dat.len));
        fi_rewind(&dat);
        fi_puts(&dat,dat.obs,dat.len,
              (long)(dat.data+(havainto-1L)*dat.len));
        putsaa();
        hav_disp(havainto);
        }


static int vertpituus(char *arvo,long hav,int len)
        {
        int i;
        char hakusana[LLENGTH];

        fi_alpha_load(&dat,hav,v[muuttuja],hakusana);
        conv(hakusana,code);
        for (i=0; i<len; ++i)
            {
            if (arvo[i]!=hakusana[i]) break;
            }
        return(i);
        }

static void haettu(long hav)
        {
        havainto=hav;
        putsaa();
        hav_disp(havainto);
        }


static int binhaku(char *arvo)
        {
        int i,k;
        long hav,hav1,hav2;
        char type;
        char hakusana[LLENGTH];
        int len=strlen(arvo);

        type=dat.vartype[muuttuja][0];
        if (type=='S' && !koodit)
            {
            i=load_codes("SORTCODE.BIN",code);
            if (i<0) return(-1);
            koodit=1;
            }
        if (type=='S') conv(arvo,code);
        hav1=1L; hav2=n;
        while (1)
            {
            hav=(hav1+hav2)/2;
            if (type=='S')
                {
                fi_alpha_load(&dat,hav,v[muuttuja],hakusana);
                conv(hakusana,code);
                i=strncmp(arvo,hakusana,len);
                if (i<0) hav2=hav; else hav1=hav;
                if (i==0 || hav1+1>=hav2)
                    {
                    i=vertpituus(arvo,hav1,len);
                    if (hav1!=hav2)
                        {
                        k=vertpituus(arvo,hav2,len);
                        if (k>i) { haettu(hav2); return(1); }
                        if (i==0) { haettu(hav1); return(1); }
                        }
                    hav=hav1;
                    while (1)
                        {
                        --hav; if (hav<1L) { haettu(1L); return(1); }
                        k=vertpituus(arvo,hav,len);
                        if (k<i) { haettu(hav+1); return(1); }
                        }
                    }
                }
            else { sur_print("\nNot available for numeric fields!"); 
                   WAIT; // RS CHAR getch() -> WAIT
                   return(-1);
                       /* type!='S' estetty jo aikaisemmin */
                 }
            }
        }

static int varnro(SURVO_DATA_FILE *s,char *nimi)
        {
        int i;
        char n8[9];
        int len=strlen(nimi);

        strncpy(n8,nimi,8);
        for (i=len; i<8; ++i) n8[i]=' '; n8[8]=EOS;
        for (i=0; i<(*s).m; ++i)
            {
            if (strncmp(n8,(*s).varname[i],8)==0) return(i);
            }
        return(-1);
        }


static int hae(char *sana,char *s)
        {
        char *p;
        int len=strlen(sana);

        p=s;
        while (1)
            {
            if ((p=strchr(p,*sana))==NULL) break;
            if (strncmp(p,sana,len)==0) return((int)(p-s));
            ++p;
            }
        return(-1);
        }


static int relaatio(char *s,char *prel,char *arvo)
/* *s;  hakuavain */
        {
// RS REM        char *p;

        if (*s=='=') { *prel='='; strcpy(arvo,s+1); return(1); }
        if (*s=='>')
            {
            if (*(s+1)=='=')
                { *prel='S'; strcpy(arvo,s+2); return(1); }
            *prel='>'; strcpy(arvo,s+1); return(1);
            }
        if (*s=='<')
            {
            if (*(s+1)=='=')
                { *prel='P'; strcpy(arvo,s+2); return(1); }
            if (*(s+1)=='>')
                { *prel='E'; strcpy(arvo,s+2); return(1); }
            *prel='<'; strcpy(arvo,s+1); return(1);
            }
        if (strncmp(s,"!=",2)==0)
                { *prel='E'; strcpy(arvo,s+2); return(1); }
        *prel=' '; strcpy(arvo,s);
        return(1);
        }

static void textinfo()
        {
        int i,k;
// RS REM        char rivi[LLENGTH];
        char *sana[1];

        sortvar=-1;
        if (dat.textn==0) return;
        i=0;
        while (1)
            {
            k=hae("SORT:",dat.fitext[i]);
            if (k<0) { ++i; if (i==dat.textn) break; else continue; }
            k=split(dat.fitext[i]+k+5,sana,1);
            sortvar=varnro(&dat,sana[0]);
            break;
            }
        }


static void etsi()
        {
        int i;
        long hav;
        char rel; /*  = < > S(>=) P(<=) E(<>) E(!=) */
        char arvo[LLENGTH];
        char hakusana[LLENGTH];
        char type;
        int len;
        double x,y;

        hav=atol(hakuavain);
        if (hav>0L && hav<=dat.n)
            {
            havainto=hav;
            putsaa();
            hav_disp(havainto);
            return;
            }

        if (*hakuavain==EOS) { putsaa(); osoita(muuttuja); return; }
        i=relaatio(hakuavain,&rel,arvo);
        if (i<0) { putsaa(); osoita(muuttuja); return; }

        len=strlen(arvo); y=atof(arvo);
/*      type=dat.vartype[muuttuja][0];    -4.10.1992 */
        type=dat.vartype[v[muuttuja]][0];
        if (rel==' ' && type!='S') rel='=';
        if (sortvar==v[muuttuja] && type=='S' && (rel==' ' || rel=='='))
            {
            binhaku(arvo);
            putsaa(); osoita(muuttuja);
            return;
            }
        hav=havainto;
        while (1)
            {
            if (sur_kbhit()) { sur_getch(); putsaa(); osoita(muuttuja); return; }
            ++hav;
            if (hav>dat.n)
                {
                putsaa();
                LOCATE(r3+2,1); PR_EBLD;
                if (n_haku==0)
                    sur_print("Not found!");
                else
                    { sprintf(sbuf,"%d cases found.",n_haku); sur_print(sbuf); }
                sur_print(" Press any key!");
                osoita(muuttuja);
                sur_getch(); // RS CHA nextch("") -> sur_getch
                putsaa(); osoita(muuttuja); return;
                }
            if (rel==' ')
                {
                fi_alpha_load(&dat,hav,v[muuttuja],hakusana);
                if (strncmp(hakusana,arvo,len)==0)
                    {
                    ++n_haku;
                    havainto=hav;
                    putsaa();
                    hav_disp(havainto);
                    return;
                    }
                LOCATE(r3+2,70); PR_EBLD;
                sprintf(sbuf,"%ld",hav); sur_print(sbuf);
                continue;
                }
            fi_load(&dat,hav,v[muuttuja],&x);
            if (x==MISSING8) continue;
            i=0;
            switch (rel)
                {
              case '=': if (x==y) i=1; break;
              case '<': if (x<y) i=1; break;
              case '>': if (x>y) i=1; break;
              case 'E': if (x!=y) i=1; break;
              case 'P': if (x<=y) i=1; break;
              case 'S': if (x>=y) i=1; break;
                }
            if (i)
                {
                ++n_haku;
                havainto=hav;
                putsaa();
                hav_disp(havainto);
                return;
                }
            LOCATE(r3+2,70); PR_EBLD;
            sprintf(sbuf,"%ld",hav); sur_print(sbuf);
            continue;
            }
        }

static void k_help()
        {
// RS REM        int i;

        PR_ENRM; CLS; PR_EINV;
        sur_print("\nKey codes in FILE EDIT:");
        sur_print("\n");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_EXIT]); sur_print(sbuf); PR_ENRM;
        sur_print(" Return back to SURVO 84 EDITOR");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_NEXT]); sur_print(sbuf); PR_ENRM;
        sur_print(" Next observation (case, record)");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_PREV]); sur_print(sbuf); PR_ENRM;
        sur_print(" Previous observation");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_RETURN]); sur_print(sbuf); PR_ENRM;
        sur_print(" Next variable (field)");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_REF]); sur_print(sbuf); PR_ENRM;
        sur_print(" Description of the current variable");

        sur_print("\n");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_SRCH]); sur_print(sbuf); PR_ENRM;
        sur_print(" Search for observations according to the current field");
        sur_print("\n      or # of observation. Permitted conditions are, for example");
        sur_print("\n      <100  >=25.5  <>1 (not equal to 1)");
        sur_print("\n      If the file is sorted (in ascending order) with respect to");
        sur_print("\n      some field and SORT:<name_of_sort_field> is given on the");
        sur_print("\n      text lines in the file, cases are sought for by a binary search.");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_EXEC]); sur_print(sbuf); PR_ENRM;
        sur_print(" Search for the next case (according to rule given by ");
        PR_EINV; sprintf(sbuf,"%s",key_label[CODE_SRCH]); sur_print(sbuf); PR_ENRM; sur_print(")");

        sur_print("\n");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_DELETEL]); sur_print(sbuf); PR_ENRM;
        sur_print(" Delete one or more observations from the file");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_INSERTL]); sur_print(sbuf); PR_ENRM;
        sur_print(" Move one or more observations to a new place");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_COPY]); sur_print(sbuf); PR_ENRM;
        sur_print(" Copy one observation to replace the current one");


        sur_print("\nArrow keys have their normal functions.");
        PR_EBLD;
        sur_print("\n\nPress any key!"); sur_getch(); // RS CHA nextch("");
        edit_init();
        hav_disp(havainto);
        }


void muste_file_edit(int argc,char *argv[])
        {
        int i,h,ch;
        int kesken;
        int mask;
        char *p;

// RS ADD variable init
m_act=0;
m=0;
n=0;
havainto=0;     /* havainto nro */
muuttuja=0;
muutokset=0;
sortvar=0;   /* lajittelumuuttuja */
n_haku=0;    /* lîydetyt tapaukset */
suojaus=0;
pos=pituus=0;        /* pos=sijainti ed.kentÑssÑ pituus=kentÑn pituus */
mriville=10;      /* kenttien max lkm /rivi */
viimeiseen=0;    /* 0=1.hav. 1=viim.hav.+1 2=viim.hav. */
kokonimi=0;      /* 1: nÑytetÑÑn myîs nimen kommentit, jos mriville=1 */
vain_selailu=0;
// RS REM max_obs; /* vain rajoitetuissa versioissa */
n_alku=0;
koodit=0;



        if (argc==1) return;
        s_init(argv[1]);

        tut_init();
        if (r_soft) r3+=r_soft+1;
        if (g<3)
            {
            sur_print("\nFILE EDIT is an operation for");
            sur_print("\nSurvo data input and editing.");
            sur_print("\nUsage:");
            sur_print("\nFILE EDIT <Survo_data_file>,<mask #>");
            sur_print("\n                          (optional)");
            WAIT; return;
            }

        viimeiseen=0;

        i=strlen(word[2])-1;
        if (word[2][i]=='+') { word[2][i]=EOS; viimeiseen=1; }
        if (word[2][i]=='-') { word[2][i]=EOS; viimeiseen=2; } // 20.6.90

        i=fi_open3(word[2],&dat,0,1,1,0); if (i<0) return; // RS CHA exit(0);

        vain_selailu=1;

        mask=1;
        if (g>3)
            {
            char x[64];

            p=word[3]; if (*p=='#') ++p; mask=atoi(p);
            if (mask<1 || mask>dat.extra-5) mask=1;
            *x=EOS; p=strchr(word[3],'(');
            if (p!=NULL)         /* 11.6.90 lisÑys #2(XY)  */
                {
                strcpy(x,p+1); p=strchr(x,')'); if (p!=NULL) *p=EOS;
                }

            for (i=0; i<dat.m; ++i)
                {
                if (*x)
                    {
                    if (strchr(x,dat.vartype[i][mask])!=NULL) dat.vartype[i][1]='A';
                    else dat.vartype[i][1]='-';
                    }
                else
                    dat.vartype[i][1]=dat.vartype[i][mask];
                }
            }

        kokonimi=0; if (g>4) kokonimi=atoi(word[4]);   /* 20.6.90 */

        m=dat.m; n=dat.n;

        m_act=0;
        for (i=0; i<m; ++i)
            if (dat.vartype[i][1]!='-') ++m_act;
        if (m_act==0)
            {
            sur_print("\nNo active fields!");
            if (mask>1) { sprintf(sbuf," (Mask #%d)",mask); sur_print(sbuf); }
            WAIT; return;
            }
        i=varaa_tilat();
        for (i=0, h=0; i<m; ++i)
            if (dat.vartype[i][1]!='-') v[h++]=i;
        varinfo();  /* rajat ja muodot  (muodot puuttuu!!) */
        textinfo(); /* SORT:muuttuja  */

        i=edit_init(); if (i<0) return;

        if (!viimeiseen) havainto=1L; else havainto=dat.n+2-viimeiseen;
        if (havainto<1L) havainto=1L;

//      i=edit_init(); if (i<0) return;

        muuttuja=0;

//      if (viimeiseen) { hav_disp(1L); getch(); } // kokeilu 31.3.2003

        hav_disp(havainto);

        kesken=1;
        strcpy(tiedotus,lopetus);
        while (kesken)
            {
            if (survo_ferror)
                {
                LOCATE(r3+2,1); PR_EBLD; BEEP;
                sur_print("Cannot save! (Disk full?)  Press any key!"); WAIT;
                break;
                }
            ch=nextch(tiedotus);
            switch (ch)
                {
              case 22: break; // alt-TAB-ongelma (Win2000) 31.10.01

              case CODE_EXIT:
                i=talletus(); if (i<0) break;
                kesken=0; break;
              case CODE_NEXT:
                i=talletus(); if (i<0) break;
                if (havainto<n+1L) ++havainto;
                hav_disp(havainto);
                break;
              case CODE_PREV:
                i=talletus(); if (i<0) break;
                if (havainto>1L) --havainto;
                hav_disp(havainto);
                break;
              case CODE_RETURN:
                seur_muuttuja();
                break;
              case CODE_LEFT:
                if (suojaus) break;
                ++muutokset;
                if (pos==1) { edell_muuttuja(); break; }
                PR_LEFT; --pos; break;
              case CODE_RIGHT:
                if (suojaus) break;
                if (pos==pituus) break;
                PR_RIGHT; ++pos; break;
              case CODE_HOME:
                if (pos==1)
                    {
                    i=talletus(); if (i<0) break;
                    muuttuja=0; osoita(muuttuja);
                    }
                for (;pos>1;--pos) PR_LEFT;
                break;
              case CODE_DOWN:
                if (mriville==1) seur_muuttuja();
                else alas();
                break;
              case CODE_UP:
                if (mriville==1) edell_muuttuja();
                else ylos();
                break;
              case CODE_ERASE:
                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return; }
                if (suojaus) break;
                ++muutokset;
                PRMODE;
                for (i=pos-1; i<pituus; ++i)
                    {
                    edsana[i]=' ';
                    sur_print(" ");
                    }
                for (i=pos-1; i<pituus; ++i) PR_LEFT;
                PR_ENRM;
                break;
              case CODE_INSERT:
                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return; }
                if (suojaus) break;
                if (edsana[pituus-1]!=' ') { BEEP; break; }
                ++muutokset;
                for (i=pituus-1; i>=pos; --i) edsana[i]=edsana[i-1];
                edsana[pos-1]=' ';
                PRMODE;
                for (i=pos; i<=pituus; ++i) { sprintf(sbuf,"%c",edsana[i-1]); sur_print(sbuf); }
                for (i=pos; i<=pituus; ++i) PR_LEFT;
                PR_ENRM;
                break;
              case CODE_DELETE:
                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return; }
                if (suojaus) break;
                ++muutokset;
                for (i=pos; i<pituus; ++i) edsana[i-1]=edsana[i];
                edsana[pituus-1]=' ';
                PRMODE;
                for (i=pos; i<=pituus; ++i) { sprintf(sbuf,"%c",edsana[i-1]); sur_print(sbuf); }
                for (i=pos; i<=pituus; ++i) PR_LEFT;
                PR_ENRM;
                break;
              case CODE_SRCH:
                i=talletus(); if (i<0) break;
                LOCATE(r3+2,1);
                strcpy(hakutieto,"Record to be found ( number or =,<,> ");
                strncat(hakutieto,dat.varname[v[muuttuja]],8);
                strcat(hakutieto,")? ");
                PR_EBLD;
                prompt(hakutieto,hakuavain,16);
                osoita(muuttuja);
                n_haku=0;
                etsi();
                break;
              case CODE_EXEC:
                i=talletus(); if (i<0) break;
                etsi();
                break;
              case CODE_DELETEL:
                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return; }
                k_deletel();
                break;
              case CODE_INSERTL:
                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return; }
                k_insertl();
                break;
              case CODE_COPY:
                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return; }
                k_copy();
                break;
              case CODE_REF:
                putsaa();
                LOCATE(r3+2,1); PR_EINV;
                sprintf(sbuf,"%.*s (Press any key!)",c3-10,dat.varname[v[muuttuja]]);
                    sur_print(sbuf);
                osoita(muuttuja);
                sur_getch(); // RS CHA nextch("");
                putsaa();
                osoita(muuttuja);
                break;
              case CODE_HELP:
                k_help();  // RS ADD (this was commented out in Survo MM
                break;
              default:
                if (special) break; /* 28.9.1996 */
                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return; }
                if ((etu==0 && special==1) || suojaus) break;
                ++muutokset;
                PRMODE; sprintf(sbuf,"%c",ch); sur_print(sbuf); edsana[pos-1]=(char)ch;
                if (pos<pituus) { ++pos; break; }
                PR_ENRM;
                seur_muuttuja();
                break;

                }
            }
        if (survo_ferror && n>0L)
            {
            fi_rewind(&dat);
            --n; n_update(&dat,n);
            }
        fi_close(&dat);
        tut_end();
        if (r_soft) r3-=r_soft+1;
        s_end(argv[1]);
        }
