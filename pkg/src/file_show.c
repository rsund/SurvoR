/* show.c 17.9.1988/SM (13.9.1992) (28.9.1996)
   FILE SHOW <SURVO 84C data file>
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define FORMJAKSO 512
#define MAXLISAYS 1000L

SURVO_DATA_FILE dat;

int m_act;
int m;
long n;
long havainto;     /* havainto nro */
int viimeiseen;
int suojaus;
int muutokset;
int rivi,sar,var,ensrivi;
int prefix, return_firstvar, return_var, return_sar;
int firstvar,lastvar,firstsar,rivinpit;
int sortvar, n_haku;
int saa_kirjoittaa;
int sound_on=0;
int sound_up_down;
int soundbin_luettu=0;
int r_rivi, r_sar, r_var, r_return_firstvar, r_return_var, r_return_sar;
int r_firstvar, r_havainto;
int jatkuva_haku;
int tempo2;
char tiedotus[LLENGTH]; /* ohjeteksti alarivillä */
char lopetus[]="To stop, press EXIT! (F1=HELP)";
char edsana[2*LLENGTH];   /* 2* 18.3.92 */
char vertsana[LLENGTH];
char hakutieto[LLENGTH];
char hakuavain[64];
char polku[LLENGTH];
char siirtop[32];
int *v,*varpit,*varsar,*suojattu;
double *min,*max;
int *strarvo;   /* 21.12.91 */
unsigned int *form;
char *varj,*varj2;
int mnimet,mnimet2;
int max_dim;
char options[LNAME];
int not_found=0;
int oikealle_yli;
int tab_pakko=0;
int pilkku_pisteeksi=0; /* 16.10.2002 */
int suljettu;
char stripe[LLENGTH];

/* RS MISTÄ NÄMÄ LÖYTYVÄT??? 
extern int block_ind; 
extern int ordind;
extern int s_insert_mode;
extern int ndisp;
*/
/* RS dispiin liittyvää???
extern long jj();
extern long b_first,b_last;
extern int s_insert_mode;
extern int oikealle_yli;
extern int tab_pakko;
extern char stripe[];

extern int special;
extern int survo_ferror;
*/


/* RS lisätty */
int block_ind=0;
int ordind=0;
long *ord=NULL;
int s_insert_mode;
int ndisp;
long b_first,b_last;
int s_insert_mode;
int oikealle_yli;
int tab_pakko;
char stripe[];
int special;
int survo_ferror;


extern int sur_ctrl;


char *formtila;
long n_alku;
int nlev,ndisp;
double fs_luku;
char sound_char=' ';

/* RS: SHOWH:sta */
long nmax,n1,nn;


static int hae(char *sana, char *s)
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

static int varnro(SURVO_DATA_FILE *s, char *nimi)
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

static int varnro2(char *s)
        {
        int i,k;

        if (*s=='#') k=atoi(s+1)-1;
        else k=varnro(&dat,s);
        for (i=0; i<m_act; ++i)
            {
            if (v[i]==k) break;
            }
        return(i);
        }


void textinfo()
        {
        int i,k;
/* RS        char rivi[LLENGTH]; */
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


static void default_form(SURVO_DATA_FILE *d, int i, char *muoto)
        {
        int h,k;

        switch (d->vartype[i][0])
            {
          case 'S':
            k=d->varlen[i];
            for (h=0; h<k; ++h) muoto[h]='#'; muoto[k]=EOS;
            break;
          case '1':
            strcpy(muoto,"###");
            break;
          case '2':
            strcpy(muoto,"######");
            break;
          case '4':
            strcpy(muoto,"#######.###");
            break;
         case '8':
            strcpy(muoto,"#######.###");
            break;
            }
        }

void hae_muoto(SURVO_DATA_FILE *d, int i, char *muoto)
        {
        char *p,*q;
        char x[LLENGTH];
        int h,k;

        strcpy(x,d->varname[i]);
        p=x;
        while (1)
            {
            p=strchr(p,'(');
            if (p==NULL) { default_form(d,i,muoto); break; }
            else
                {
                if (*(p+1)!='#') { ++p; continue; }
                q=strchr(p,')');
                if (q==NULL) { ++p; continue; }
                strncpy(muoto,p+1,q-p-1); muoto[q-p-1]=EOS;
                if (muoto[1]!=EOS && muoto[1]!='#' && muoto[1]!='.')
                    {
                    k=atoi(muoto+1); if (k<=0) { ++p; continue; }
                    for (h=0; h<k; ++h) muoto[h]='#'; muoto[k]=EOS;
                    }
                break;
                }
            }
        }


static void n_display()
        {
        PR_ENRM;
        LOCATE(2,1);
        sprintf(sbuf,"File %.16s N=%ld ",active_data,n); sur_print(sbuf);
        }


static int show_init()
        {
        int i; /* RS ,k,h; */
        char varjo, varjo2;
        long li;

        PR_ENRM; CLS;
        n_display();

        li=n; if ((long)dat.m>li) li=dat.m;
        nlev=log((double)(li+1))/log(10.0)+2;
        sar=nlev+1;
        for (i=0; i<m_act; ++i)
            {
    /*      if (sar+varpit[i]>rivinpit) { m_act=i; break; }     */
            varsar[i]=sar;
            sar+=varpit[i];
            }
        ndisp=r3-2; ensrivi=4;
        rivinpit=c3+8-nlev-1;

        varjo='7'; varjo2='4';
        for (i=0; i<m_act; ++i)
            {
            varj[i]=varjo; varj2[i]=varjo2;
            if (suojattu[i]) { if (varjo2=='4') varj[i]='3'; else varj[i]='1'; }
            if (varjo=='7') varjo='8'; else varjo='7';
            if (varjo2=='4') varjo2=' '; else varjo2='4';
            }

        firstvar=0;
        n_alku=dat.n; if (n_alku==0L) n_alku=100000L;  /* tyhjä tied. ei suojattu */
        return(1);
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

static void strarvot(int i,int *pi)   /* 21.12.91 */
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

static void tilanpuute()
        {
        sur_print("\nNot enough memory for FILE SHOW!");
        WAIT;
        }

static int varinfo()
        {
        int i,vi;
        char muoto[LLENGTH];
        char *p;
        unsigned int formpit;
        unsigned int k;

        formpit=2*FORMJAKSO;
        formtila=(char *)malloc(formpit);
        if (formtila==NULL)  { tilanpuute(); return(-1); }
        p=formtila; *p=EOS;
        for (i=0; i<m_act; ++i)
            {
            vi=v[i];
            hae_muoto(&dat,vi,muoto);
            varpit[i]=strlen(muoto);
            if (p-formtila>=formpit-varpit[i]-2)
                {
                k=p-formtila;  /* 24.2.89 */
                formpit+=FORMJAKSO;
                formtila=(char *)realloc(formtila,formpit);
                if (formtila==NULL)  { tilanpuute(); return(-1); }
                p=formtila+k;  /* 24.2.89 */
                }
            form[i]=p-formtila;
            strcpy(p,muoto); p+=varpit[i]; *p=EOS; ++p;
            suojattu[i]=0;
            if (dat.vartype[vi][2]=='P')
                {
                suojattu[i]=1;
                }
            switch (dat.vartype[vi][0])
                {
              case '1':
                        min[i]=0.0; max[i]=254.0;
                        rajat(vi,&min[i],&max[i]);
                        break;
              case '2':
                        min[i]=-32768.0; max[i]=32767.0;
                        rajat(vi,&min[i],&max[i]);
                        break;
              case '4':
                        min[i]=-1e37; max[i]=1e37;
                        rajat(vi,&min[i],&max[i]);
                        break;
              case '8':
                        min[i]=-1e305; max[i]=1e305;
                        rajat(vi,&min[i],&max[i]);
                        break;
              case 'S':
                        min[i]=-1e305; max[i]=1e305;
                        strarvot(vi,&strarvo[i]);   /* 21.12.91 */
                        break;
                }

            }
        return(1);
        }

static int varaa_tilat()
        {
        v=(int *)malloc(m_act*sizeof(int));
        if (v==NULL) { tilanpuute(); return(-1); }
        varpit=(int *)malloc(m_act*sizeof(int));
        if (varpit==NULL) { tilanpuute(); return(-1); }
        varsar=(int *)malloc(m_act*sizeof(int));
        if (varsar==NULL) { tilanpuute(); return(-1); }
        min=(double *)malloc(m_act*sizeof(double));
        if (min==NULL) { tilanpuute(); return(-1); }
        max=(double *)malloc(m_act*sizeof(double));
        if (max==NULL) { tilanpuute(); return(-1); }
        strarvo=(int *)malloc(m_act*sizeof(int));         /* 21.12.91 */
        if (strarvo==NULL) { tilanpuute(); return(-1); }
        form=(unsigned int *)malloc(m_act*sizeof(int));
        if (form==NULL) { tilanpuute(); return(-1); }
        suojattu=(int *)malloc(m_act*sizeof(int));
        if (suojattu==NULL) { tilanpuute(); return(-1); }
        varj=(char *)malloc(m_act);
        if (varj==NULL) { tilanpuute(); return(-1); }
        varj2=(char *)malloc(m_act);
        if (varj2==NULL) { tilanpuute(); return(-1); }
        return(1);
        }

static int optio(char *s)
        {
        int i,len;

        len=strlen(s);
        for (i=0; i<len; ++i)
            {
            if (strchr(options,s[i])!=NULL) return(1);
            }
        return(0);
        }


static int record_nro(long *phav, char *s)
        {
        if (muste_strnicmp(s,"END",3)==0)
            {
            *phav=n;
            if (*(s+3)!=EOS) *phav+=atoi(s+3);
            if (*phav<0L) *phav=1L;
            return(1);
            }
        *phav=atol(s);
        return(1);
        }


int init_ord()
        {
        long j;

        nmax=n+MAXLISAYS;
        ord=(long *)malloc(nmax*sizeof(long));
        if (ord==NULL)
            {
            PR_EINV;
            sur_print("\nNot enough memory for inserting/deleting records!");
            WAIT; return(0);
            }
        for (j=1; j<=n; ++j) ord[j]=j;
        ordind=1; n1=n; nn=n;
        return(1);
        }

int ord_available()
        {
        if (ord!=NULL) return(1);
        return(init_ord());
        }

long jj(long j)
        {
        if (ordind) return(ord[j]);
        return(j);
        }

static void poimi(long j,int i,char *sana)
        {
        int vi;
        float luku4;
        int h,len;
        char type;
        int miss;
        int pit;

        vi=v[i];
        type=dat.vartype[vi][0];

        miss=0;
        pit=varpit[i];
        if (j>n)
            { strncpy(sana,space,(unsigned int)pit); sana[pit]=EOS; return; }
        if (type=='S')
            {
            fi_alpha_load(&dat,jj(j),vi,sana);
            h=strlen(sana);
            if (h<pit) strncat(sana,space,(unsigned int)(pit-h));
            }
        else
            {

            fi_alpha_load(&dat,jj(j),vi,sana);

            switch (type)
                {
              case '1':
                fs_luku=(unsigned char)(*sana);
                if (fs_luku==MISSING1) { miss=1; break; }
                fconv(fs_luku,formtila+form[i],sana);
                break;
              case '2':
               fs_luku=*(short *)sana;
                if (fs_luku==MISSING2) { miss=1; break; }
                fconv(fs_luku,formtila+form[i],sana);
                break;
              case '4':
                if (not_float(sana)) { miss=1; break; }
                luku4=*(float *)sana;
                if (luku4>MISSING4/10.0) { miss=1; break; }
           /*   float4_muunto(luku4,sana);    */
                fs_luku=luku4;
                fconv(fs_luku,formtila+form[i],sana);
                break;
              case '8':
                if (not_double(sana)) { miss=1; break; }
                fs_luku=*(double *)sana;
                if (fs_luku>MISSING8/10.0) { miss=1; break; }
                fconv(fs_luku,formtila+form[i],sana);
                break;
                }
            if (miss)
                {
                strncpy(sana,space,(unsigned int)pit); sana[pit]=EOS; return;
                }
            else if (strlen(sana)>pit)
                {
                strncpy(sana,space,(unsigned int)pit);
                sana[pit-1]='*'; sana[pit]=EOS;
                }
            len=strlen(sana);
            if (len<pit)
                {
                for (h=len; h<pit; ++h) sana[h]=' '; sana[pit]=EOS;
                }
            }
        }


static void putsaa()
        {
        LOCATE(r3+2,1);
        PR_ENRM; sprintf(sbuf,"%.*s",c3+7,space); sur_print(sbuf);
        }


static void disp_field(long j1,int i,int rivi,int sar,char varjo)
        {
        char sana[2*LLENGTH];

        poimi(j1,i,sana);
        write_string(sana,varpit[i],varjo,rivi,sar);
        }

static void disp_hav(long j1,long j)
        {
        int i;
        char x[LLENGTH];
/* RS        char varjo; */
        int rivi;
        char nro_varjo;

        nro_varjo='1';
        if (block_ind>1) { if (j1>=b_first && j1<=b_last) nro_varjo='5'; }
        rivi=j1-j+ensrivi;
        sprintf(x,"%*ld",nlev,j1);

        write_string(x,nlev,nro_varjo,rivi,1);
        for (i=firstvar; i<=lastvar; ++i)
            {
            disp_field(j1,i,rivi,varsar[i]-firstsar,varj2[i]);
            }
        i=varsar[lastvar]-firstsar+varpit[lastvar];
        write_string(space,c3+8-i+1,237,rivi,i);  /* RS oli '\237' */
        }

static int disp_ots()
        {
        int i,k,h;

/*      SCROLL_UP(ensrivi-2,ensrivi+ndisp-2,ndisp+1);   korjattava! */
/*      sur_scroll_up(ndisp+1,ensrivi-2,1,ensrivi+ndisp-2,c3+8,0);  */

        firstsar=varsar[firstvar]-nlev-1;
        k=0;
        for (i=firstvar; i<m_act; ++i)
            {
            k+=varpit[i];
            if (k>rivinpit) { lastvar=i-1; break; }
            }
        if (i==m_act) lastvar=m_act-1;

        for (i=firstvar; i<=lastvar; ++i)
            {
            k=varpit[i];

/* RS muotoiltu tulostus ei toimi ääkkösten kanssa:
  sprintf(sbuf,"%.*s",k,dat.varname[v[i]]);
*/
            sprintf(sbuf,"%s",dat.varname[v[i]]);  /* RS tämä ehkä tulostaa liian pitkiä stringejä */

            for (h=8; h<k; ++h) sbuf[h]=' ';

            write_string(sbuf,k,varj[i],ensrivi-1,varsar[i]-firstsar);

            }
        i=varsar[lastvar]-firstsar+varpit[lastvar];
        if (lastvar<m_act-1)
            write_string(stripe,c3+8-i+1,237,ensrivi-1,i); /* RS oli '\237' */
        else
            write_string(space,c3+8-i+1,237,ensrivi-1,i);  /* RS oli '\237' */
        return(1);
        }

static void disp_recs(long j)
        {
        int i;

/* RS ruudunpäivityksen nopeuden mittaamista 1/2
#include <time.h>
clock_t start, vali, end;
double elapsed1,elapsed2;
start = clock();
*/

        disp_ots();
        fi_rewind(&dat);
/* RS palauta tämä!!!        CURSOR_OFF; */

        for (i=0; i<ndisp; ++i)
            {
            disp_hav((long)(j+i),j);
            }

        muste_flushscreen(); /* RS Updating screen */ 

/* RS ruudunpäivityksen nopeuden mittaamista 1/2
end = clock();
elapsed1 = ((double) (end - start)) / CLOCKS_PER_SEC;
Rprintf("aika:%f\n",elapsed1);
*/

        }

static void disp_nimi()
        {
        int i;
        char sana[LLENGTH];
/* RS       char x[LLENGTH]; */

        if (dat.vartype[0][0]!='S') return;
        if (n<havainto+rivi-ensrivi) { strncpy(sana,space,16); sana[16]=EOS; }
        else fi_alpha_load(&dat,jj(havainto+rivi-ensrivi),0,sana);
        i=strlen(sana); if (i>16) i=16;
        write_string(sana,i,'1',2,30);

        }



static void disp_muuttujan_nimi(char *s)
        {
        int i;

        putsaa();
        sprintf(sbuf,"%.*s%s",c3+8,dat.varname[v[var]],s);
        for (i=strlen(sbuf); i<c3+8; ++i) sbuf[i]=' '; sbuf[c3+8]=EOS;
        if (etu!=2)
            {
            if (strncmp(sbuf+c3-1,space,9)==0)
               strcpy(sbuf+c3-1,"(EXIT=F8)");
            }
        write_string(sbuf,c3+8,'7',r3+2,1);
        LOCATE(rivi,sar);
        }



static void disp_field_up()
        {
        char sana[2*LLENGTH];
        int i;

        poimi(havainto+rivi-ensrivi,var,sana);
        if (dat.vartype[v[var]][0]!='S')
            {
            i=varpit[var];
            if (sana[i-1]=='*')
            fconv(fs_luku,"",sana);
            }
        i=strlen(sana); if (i>24) i=24;
/* printf("s=%s",sana); getch();  */
        write_string(space,32,' ',2,c3-24);
        write_string(dat.varname[v[var]],8,'1',2,c3-24);
        write_string(sana,i,'7',2,c3-16);
        if (sound_on) sound_char=sana[sar-varsar[var]+firstsar];
        sprintf(sbuf,"%*d ",nlev-1,v[var]+1);
        write_string(sbuf,nlev,'4',ensrivi-1,1);
        if (mnimet) disp_muuttujan_nimi("");
        }


static void kirjlupa()
        {
        putsaa(); 
//        BEEP;
        LOCATE(r3+2,1); PR_EBLD;
        sur_print("You get permission for editing by pressing F3. (Press first any key!)");
        LOCATE(rivi,sar); nextch(""); putsaa(); LOCATE(rivi,sar);
        }

static void osoita(int i) /* i = akt. muuttuja 0,1,2,...  */
        {

        poimi(havainto+rivi-ensrivi,i,edsana);
        strcpy(vertsana,edsana);

/*      write_string(edsana,varpit[i],'7',rivi,varsar[i]);    */
        }


static int voi_lisata(long n)
        {
        if (n-nn>=MAXLISAYS)
            {
            PR_EBLD; LOCATE(r3+2,1);
            sur_print("\nCannot insert more records! (Exit and activate again!)");
            WAIT; disp_recs(havainto); return(0);
            }
        return(1);
        }

static int first_var(int last)
        {
        int i,k;

        i=last; k=0;
        while (i>=0) { k+=varpit[i]; if (k>rivinpit) break; --i; }
        if (i<0) return(0);
        return(i+1);
        }


void missing_save(SURVO_DATA_FILE *s, long j)
        {
        int i;
        char   miss1=(char)MISSING1; /* RS: typedef lisätty */
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


static void n_update(SURVO_DATA_FILE *s,
long n /* new obs.# */
)
        {
        fi_rewind(s);
        fi_puts(s,&n,sizeof(long),22L);
        (*s).n=n;
        }

static void apu_error(char *s)
        {
        sur_print("\nCannot save in auxiliary file %s! File not updated!",s);
        WAIT;
        }


static FILE *apu;
static int datasiirto()
        {
        int i;
        long j,alku;
        char datanimi[LLENGTH],apunimi[LLENGTH];
// RS unused        char *p,*q;

        n_update(&dat,n);

        for (j=1; j<=n; ++j)
            if (ord[j]!=j) break;
        if (j>n) return(1);

        strcpy(apunimi,polku); strcat(apunimi,"SURVO.XXX");
        apu=fopen(apunimi,"wb");
        if (apu==NULL)
            {
            apu_error(apunimi); return(-1);
            }

        fi_rewind(&dat);
        alku=(long)(dat.data);
        for (j=0; j<alku; ++j)
            {
            putc(getc(dat.survo_data),apu);
            if (ferror(apu)) { apu_error(apunimi); return(-1); }
            }

        fi_rewind(&dat);

        putsaa();
        PR_EINV; LOCATE(r3+2,1);
        sur_print("Updating data file... "); PR_EBLD;
        for (j=1; j<=n; ++j)
            {
            sprintf(sbuf,"%ld",j); LOCATE(r3+2,24); sur_print(sbuf);
            fi_gets(&dat,dat.obs,dat.len,
                        (long)(dat.data+(long)((jj(j)-1L)*(long)(dat.len))));

            for (i=0; i<dat.len; ++i)
                putc((int)dat.obs[i],apu);
            if (ferror(apu)) { apu_error(apunimi); return(-1); }
            }
        fclose(apu);
        fi_close(&dat);
        fi_find(word[2],&dat,datanimi);
// printf("\ndatanimi=%s|",datanimi); getch();
//      fi_close(&dat);  väärin, koska fi_find ei varaa tiloja!!!
        fclose(dat.survo_data);
        suljettu=1;
        sur_delete1(datanimi);
// printf("apunimi=%s datanimi=%s\n",apunimi,datanimi); getch();
        i=sur_rename(apunimi,datanimi);
        return(1);
        }




static int talletus()
        {
        int vi;
        double x;
        char type;
/* RS        extern int survo_ferror; */
        long j;

/* RS: ei vielä mukana
        if (sound_on)
            {
            if (sound_up_down)
                {
                if (s_insert_mode) var_sound();
                else sound(sound_char);
                }
            else sound((char)((var-firstvar)%12+'0'));
            }
*/
        if (!muutokset) return(1);

        if (oikealle_yli && tab_pakko && varpit[var]>1)
            {
            LOCATE(r3+2,70); PR_EIN2;
            sur_print("Press TAB!");
            LOCATE(rivi,sar);
/* RS: näppäintarkistus jätetty vielä pois;
            BEEP;
            while (1)
                {
                vi=getch();
                if (vi==CODE_TAB || vi==CODE_RETURN) break;
                BEEP;
                }
*/
            LOCATE(r3+2,70); PR_ENRM;
            LOCATE(rivi,sar);
            }

        if (strcmp(vertsana,edsana)==0) { muutokset=0; return(1); }
        j=havainto+rivi-ensrivi;
        if (j==n+1)
            {
/* RS nämä vielä häviksissä?  Kaikki määritelty ylhäällä
            extern long n1;
            extern int ordind;
            extern long *ord;
*/




            ++n;
            n_update(&dat,n);

            if (!ordind) missing_save(&dat,n);
            else
                {
                if (!voi_lisata(n)) { --n; n_update(&dat,n); return(1); }
                ++n1;
                missing_save(&dat,n1);
                ord[n]=n1;
                }
            n_display();
            }

        vi=v[var];
        type=dat.vartype[vi][0];
        if (type=='S')
            {
            if (strarvo[var])   /* predefined values to be tested  21.12.91 */
                {
                char s[LLENGTH];
                char *p;
                char *sana[EP4];
                int m,i,len;

                len=strlen(edsana);
                while (len>0 && edsana[len-1]==' ') --len;

                strcpy(s,dat.varname[vi]+strarvo[var]+1);
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
                            strcpy(tut_info,"___@8@FILE SHOW@Not a permitted value!@");
                            exit(1);
                            }
                        LOCATE(r3+2,1);
                        PR_EBLD;
                        sprintf(sbuf,"%s is not a permitted value for %.8s",
                                edsana,dat.varname[v[var]]);
                        sur_print(sbuf);
// RS NYI                        BEEP;
                        sur_print("  Press any key!");
                        LOCATE(rivi,sar);
                        nextch("");
                        putsaa();
                        return(-1);
                        }
                    }
                }
            fi_save(&dat,jj(j),vi,edsana);
            disp_field(j,var,rivi,varsar[var]-firstsar,varj[var]);
            muutokset=0;
            return(1);
            }
        if (strncmp(edsana,space,varpit[var])==0)
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
            if (x<min[var] || x>max[var])
                {
                if (etu==2)
                    {
                    strcpy(tut_info,"___@8@FILE SHOW@Not a permitted value!@");
                    exit(1);
                    }
                LOCATE(r3+2,1);
                PR_EBLD;
                if (x<min[var])
                    { sprintf(sbuf,"%.8s < %g (smallest permitted value)",
                            dat.varname[v[var]],min[var]);
                      sur_print(sbuf);
                    }
                else
                    { sprintf(sbuf,"%.8s > %g (greatest permitted value)",
                            dat.varname[v[var]],max[var]);
                      sur_print(sbuf);
                    }
// RS NYI                BEEP;
                sur_print("  Press any key!");
                LOCATE(rivi,sar);
                nextch("");
                putsaa();
         /*     osoita(var);     */
                return(-1);
                }
            }
        fi_save(&dat,jj(j),vi,&x);
        disp_field(j,var,rivi,varsar[var]-firstsar,varj[var]);
        muutokset=0;
        return(1);
        }

static int oikealle()
        {
        int i;

        if (sar==varsar[var]-firstsar+varpit[var]-1)
            {
            if (var==lastvar)
                {
                if (lastvar==m_act-1) return(1);
                i=talletus(); if (i<0) return(-1);
                firstvar=first_var(var+1);
                disp_recs(havainto);
                ++var; sar=varsar[var]-firstsar;
                return(1);
                }
            oikealle_yli=1;
            i=talletus(); oikealle_yli=0; if (i<0) return(-1);
            ++var;
            }
        ++sar;
        return(1);
        }

void seur_rivi()
        {
        int i;

        i=(firstvar==return_firstvar);
        firstvar=return_firstvar; sar=return_sar; var=return_var;
        if (!i) disp_recs(havainto);
        if (rivi<ensrivi+ndisp-1) { ++rivi; disp_nimi(); return; }
        if (havainto+ndisp-1==n+ndisp) return;
        SCROLL_UP(ensrivi-1,ensrivi+ndisp-2,1);
        ++havainto;
        disp_hav(havainto+ndisp-1,havainto); /* disp_nimi(); */
        }

void alas()
        {
        int i;

        sound_up_down=1;
        i=talletus(); if (i<0) return;
        if (rivi<ensrivi+ndisp-1) { ++rivi; /* disp_nimi(); */ return; }
        if (havainto+ndisp-1==n+ndisp) return;
        SCROLL_UP(ensrivi-1,ensrivi+ndisp-2,1);
        ++havainto;
        disp_hav(havainto+ndisp-1,havainto); /* disp_nimi(); */
        }

void ylos()
        {
        int i;

        sound_up_down=1;
        i=talletus(); if (i<0) return;
        if (rivi>ensrivi) { --rivi; /* disp_nimi(); */ return; }
        if (havainto==1L) return;
        SCROLL_DOWN(ensrivi-1,ensrivi+ndisp-2,1);
        --havainto;
        disp_hav(havainto,havainto); /* disp_nimi();  */
        }


/* main(argc,argv)
int argc; char *argv[]; */
int muste_file_show(char *argv)
        {
        int i,h,ch,k;
        int kesken;
        int mask;
        char *p;
        int rec_field_indicated=0;
        char ehto[LNAME];

/* RS       if (argc==1) return; 
        s_init(argv[1]); */
        s_init(argv);

/* RS        tut_init(); */
        if (r_soft) r3+=r_soft+1;
        for (i=0; i<LLENGTH-1; ++i) stripe[i]='.'; stripe[LLENGTH-1]=EOS;
        strcpy(siirtop,argv); /* strcpy(siirtop,argv[1]); */
        if (g<3)
            {
            sur_print("\nFILE SHOW is an operation for");
            sur_print("\nSurvo data input and editing.");
            sur_print("\nUsage:");
            sur_print("\nFILE SHOW <Survo_data_file>,<mask #>");
            sur_print("\n                          (optional)");
            WAIT; return(1);
            }
        viimeiseen=0;
        i=strlen(word[2])-1;
        if (word[2][i]=='+') { word[2][i]=EOS; viimeiseen=1; }
        if (word[2][i]=='-') { word[2][i]=EOS; viimeiseen=2; }
        i=fi_open3(word[2],&dat,0,1,1,0); if (i<0) return(-1); /* RS; exit is not working with R: exit(0); */
        if (dat.n==0L) viimeiseen=0;
        suljettu=0;
        saa_kirjoittaa=0; // RS Must be initialized

/* RS Avaako tiedoston oikein? Kyllä avaa 20.3.2009
int apu;
for (apu=0; apu<dat.m; apu++) {
Rprintf("var %d; varpos: %d; varlen: %d; vartype: %s; varname: %s\n",apu,dat.varpos[apu],dat.varlen[apu],dat.vartype[apu],dat.varname[apu]);
}
*/


        *polku=EOS;  /* RS Tyhjä polkunimi KORJAA */  
/*        strcpy(polku,word[2]);  RS Poluksi tiedoston nimi, KORJAA! */
/* RS       etsi_polku(word[2],polku); */
        mask=1;

        return_firstvar=0; /* 7.2.2006 */

        if (g>3)
            {
            char x[64];

            p=word[3]; if (*p=='#') ++p; mask=atoi(p);
            if (mask<1 || mask>dat.extra-5) mask=1;
            *x=EOS; p=strchr(word[3],'(');
            if (p!=NULL)         /* 11.6.90 lisäys #2(XY)  */
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
        m=dat.m; n=dat.n;
        m_act=0;
        for (i=0; i<m; ++i)
            if (dat.vartype[i][1]!='-') ++m_act;
        if (m_act==0)
            {
            if (etu==2)
            sur_print("\nNo active fields!");
            if (mask>1) { sprintf(sbuf," (Mask #%d)",mask); sur_print(sbuf); }
            WAIT; return(1);
            }
        i=varaa_tilat(); if (i<0) return(1);
        for (i=0, h=0; i<m; ++i)
            if (dat.vartype[i][1]!='-') v[h++]=i;
        i=varinfo(); if (i<0) return(1);
        textinfo(); /* SORT:muuttuja  */
        if (!viimeiseen) havainto=1L; else havainto=dat.n+2-viimeiseen;
        i=show_init(); if (i<0) return(1);


/*********************************************/

        i=spec_find("OPTIONS",options,LNAME-1);
        if (i<0) /* 14.1.2001 */
            {
            i=hae_apu("file_show",options);
            }
        if (i>0)
            {
            if (optio("W"))
                {
                saa_kirjoittaa=1;
                i=fi_to_write(word[2],&dat);
                if (i<0) return(1);
                fi_rewind(&dat);
                }
            if (optio("N"))
                {
                mnimet=1; *tiedotus=EOS;
                }
/* RS            if (optio("S")) sound_on_off(); */
            if (optio("I")) s_insert_mode=1;
            if (optio("T")) tab_pakko=1;
            if (optio("C")) pilkku_pisteeksi=1; /* 16.10.2002 */
            }

        *hakuavain=EOS; rivi=ensrivi;
        i=spec_find("RECORD",sbuf,LNAME-1);
        if (i>0)
            {
            for (i=1; i<strlen(sbuf); ++i)
                if (strchr(":=<>",sbuf[i])!=NULL) break;
            if (i<strlen(sbuf))
                {
                strcpy(ehto,sbuf+i); sbuf[i]=EOS;
                if (*ehto==':') *ehto='=';
                }
            else *ehto=EOS;
            if (*ehto==EOS) i=record_nro(&havainto,sbuf);
            else
                {
                i=varnro2(sbuf);
                if (i<m_act)
                    {
                    var=i;
                    strcat(hakuavain,ehto);
/* RS: ei vielä mukana                    etsi();  */
                    }
                }
            }

        i=spec_find("FIELD",sbuf,LNAME-1);
        if (i>0)
            {
            i=varnro2(sbuf);
            if (i<m_act)
                {
                putsaa();
                firstvar=i; var=i; disp_recs(havainto);
                sar=varsar[var]-firstsar;
                rec_field_indicated=1;
                }
            }

/***********************************************/

        disp_recs(havainto);
        if (!rec_field_indicated)
            {
            rivi=ensrivi; sar=varsar[0]-firstsar; var=firstvar;
            }
        kesken=1;  disp_nimi();
        return_sar=varsar[0]; return_var=0;
        if (!mnimet) strcpy(tiedotus,lopetus);

        while (kesken)
            {
            if (survo_ferror)
                {
                if (etu==2)
                    {
                    strcpy(tut_info,"þþþ@6@FILE SHOW@Cannot save more data!");
                    break;
                    }
                LOCATE(r3+2,1); PR_EBLD; // RS NYI   BEEP;
                sur_print("Cannot save! (Disk full?)  Press any key!"); /* RS getch(); */
                break;
                }
/* RS: ei vielä mukana - mitä tekee ja milloin???
            if ( sound_on || (etu==0 && !kbhit()) || (etu==2 && etu1>1) )
                { disp_field_up(); disp_nimi(); }
*/
disp_field_up(); disp_nimi(); /* RS lisätty näytettäväksi joka kerta */

            sound_up_down=0;
            if (*tiedotus==EOS && !mnimet) strcpy(tiedotus,lopetus);

            LOCATE(rivi,sar);

            ch=nextch(tiedotus);
/* sprintf(sbuf,"\n%d|",(int)ch); sur_print(sbuf); */

/* RS: puuttuu vielä           if (prefix) { prefix_code(ch); continue; } */
            switch (ch)
                {

                case 22: break; /* kokeilu Win 2000 ongelmaan alt-TAB */

              case CODE_EXIT:
                i=talletus(); if (i<0) break;
                if (ordind) datasiirto();
                kesken=0; break;
              case CODE_NEXT:
                i=talletus(); if (i<0) break;
                havainto+=ndisp;
                if (havainto>n+1) havainto=n+1;
                disp_recs(havainto); disp_nimi();
                break;
              case CODE_PREV:
                i=talletus(); if (i<0) break;
                havainto-=ndisp;
                if (havainto<1L) havainto=1L;
                disp_recs(havainto); disp_nimi();
                break;
              case CODE_DOWN:
                alas();
                break;
              case CODE_UP:
                ylos();
                break;
              case CODE_RETURN:
                sound_up_down=1;
                i=talletus(); if (i<0) break;
                seur_rivi(); break;

              case CODE_RIGHT:
                oikealle(); break;
              case CODE_LEFT:
                if (sar==varsar[var]-firstsar)
                    {
                    if (var==firstvar)
                        {
                        if (firstvar==0) break;
                        i=talletus(); if (i<0) break;
                        --firstvar;
                        var=firstvar; disp_recs(havainto);
                        sar=varsar[var]-firstsar;
                        break;
                        }
                    i=talletus(); if (i<0) break;
                    --var;
                    }
                --sar;

                break;
              case CODE_INSERT:
                s_insert_mode=1-s_insert_mode;
//RS                if (s_insert_mode) CURSOR_INS; else CURSOR_ON;
                break;
              case CODE_DELETE:
//RS                if (sur_ctrl) del_column_temporarily();
//RS                else delete_char();
                break;
              case CODE_ERASE:
//RS                if (block_ind>2) erase_recs();
//RS                else erase_field();
                break;
              case CODE_DELETEL:
                i=talletus(); if (i<0) break;
//RS                delete_rec();
                break;
              case CODE_INSERTL:
                i=talletus(); if (i<0) break;
//RS                insert_rec();
                break;
              case CODE_HOME:
                i=talletus(); if (i<0) break;
                if (var>firstvar) { var=firstvar; sar=varsar[var]-firstsar; break; }
                if (firstvar>0)
                    {
                    firstvar=first_var(firstvar-1); disp_recs(havainto);
                    var=firstvar; sar=varsar[var]-firstsar;
                    break;
                    }
                if (sar>varsar[var]-firstsar) { sar=varsar[var]-firstsar; break; }
                if (rivi>ensrivi) { rivi=ensrivi; disp_nimi(); break; }
                if (havainto==1L) break;
                havainto=1L; disp_recs(havainto);
                break;
              case CODE_END:
                i=talletus(); if (i<0) break;
                if (var<lastvar) { var=lastvar; sar=varsar[var]-firstsar; break; }
                if (lastvar==m_act-1) break;
                firstvar=lastvar+1; disp_recs(havainto);
                var=lastvar; sar=varsar[var]-firstsar;
                break;
              case CODE_TAB:
                i=talletus(); if (i<0) break;
                if (var==lastvar)
                    {
                    if (var==m_act-1) break;
                    firstvar=first_var(var+1); disp_recs(havainto);
                    }
                ++var; sar=varsar[var]-firstsar;
                break;
              case CODE_SRCH:
                mnimet2=mnimet; mnimet=0;
                jatkuva_haku=0;
                i=talletus(); if (i<0) break;
                putsaa(); LOCATE(r3+2,1);
                strcpy(hakutieto,"Record to be found ( number or =,<,> ");
                strncat(hakutieto,dat.varname[v[var]],8);
                strcat(hakutieto,")? ");
                PR_EBLD;
//RS                prompt(hakutieto,hakuavain,16);
                osoita(var);
                n_haku=0;
//RS                i=etsi(); if (etu>0 && i<0) kesken=0;
                mnimet=mnimet2;
                break;
              case CODE_EXEC:
                mnimet2=mnimet; mnimet=0;
                jatkuva_haku=0;
                i=talletus(); if (i<0) break;
//RS                etsi();
                mnimet=mnimet2;
                break;
              case CODE_TOUCH:
                saa_kirjoittaa=1;
                i=fi_to_write(word[2],&dat);
                if (i<0) return(1);
                fi_rewind(&dat);
                break;
              case CODE_MERGE:
                mnimet=0;
//RS                disp_muuttujan_nimi(" (Press any key!)");
                nextch("");
                putsaa();
                LOCATE(rivi,sar);
                break;
              case CODE_REF:
                if (r_rivi==0)
                    {
                    r_rivi=rivi; r_sar=sar; r_var=var; r_return_var=return_var;
                    r_return_firstvar=return_firstvar;
                    r_return_sar=return_sar; r_firstvar=firstvar; r_havainto=havainto;
                    }
                else if (r_rivi==rivi && r_sar==sar && r_firstvar==firstvar && r_havainto==havainto)
                    r_rivi=0;
                else
                    {
                    i=talletus(); if (i<0) break;
                    rivi=r_rivi; sar=r_sar; var=r_var; return_var=r_return_var;
                    return_firstvar=r_return_firstvar;
                    return_sar=r_return_sar; firstvar=r_firstvar; havainto=r_havainto;
                    disp_recs(havainto); disp_nimi();
                    }
                break;
              case CODE_MOVE:
                mnimet=0;
//RS                block_rec();
                break;
              case CODE_DISK:
//RS                copy_field();
                break;
              case CODE_DISP:
//RS                copy_rec();
                break;
              case CODE_HELP:

                break;
              case CODE_PRE:
                prefix=1;
                break;
              case CODE_CODE:
                poimi(havainto+rivi-ensrivi,var,edsana);
                i=strlen(edsana); while (i>0 && edsana[i-1]==' ') edsana[--i]=EOS;
                p=edsana; while (*p==' ') ++p;
                if (strlen(tut_info)+strlen(p)>254) break;
                strcat(tut_info,p); strcat(tut_info,"@");
                break;

              default:
                if ((etu==0 && special==1) || suojattu[var]) break;
                if (havainto+rivi-ensrivi>n+1) { /* RS NYI BEEP; */ break; }
                if (!saa_kirjoittaa) { kirjlupa(); break; }
                if (!muutokset)
                    {
                    osoita(var);
                    }
                ++muutokset;

                if (pilkku_pisteeksi)
                    {
                    if (ch==(int)',') ch=(int)'.';
                    else if (ch==(int)'.') ch=(int)',';
                                     /* 16.10.2002 */
                    }
                if (!s_insert_mode)
                    {
                    sprintf(sbuf,"%c",ch);
                    write_string(sbuf,1,varj[var],rivi,sar);
                    edsana[sar-varsar[var]+firstsar]=(char)ch;
                    }
                else
                    {
                    k=varpit[var]-1;
                    if (edsana[k]!=' ') { /* RS NYI BEEP; */ break; }
                    h=sar-varsar[var]+firstsar;
                    for (i=k-1; i>=h; --i)
                        edsana[i+1]=edsana[i];
                    edsana[h]=(char)ch;
                    write_string(edsana+h,k-h+1,varj[var],rivi,sar);
                    }
                oikealle();

                break;
                }
            }

        if (survo_ferror && n>0L)
            {
            fi_rewind(&dat);
            --n; /* n_update(&dat,n);  */
            }
        if (!suljettu) fi_close(&dat);
/* RS: vielä poissa        tut_end(); */
        if (r_soft) r3-=r_soft+1;
/* RS       s_end(argv[1]); */
        s_end(argv); 
        return(1);
        }

