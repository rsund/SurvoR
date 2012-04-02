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

extern int sur_ctrl;

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
*/

extern int survo_ferror;

static SURVO_DATA_FILE dat;

static int muste_showlongvar; // RS ADD
static int muste_showlonglen; // RS ADD
static int m_act;
static int m;
static long n;
static long havainto;     /* havainto nro */
static int viimeiseen;
static int suojaus;
static int muutokset;
static int rivi,sar,var,ensrivi;
static int prefix, return_firstvar, return_var, return_sar;
static int firstvar,lastvar,firstsar,rivinpit;
static int sortvar, n_haku;
static int saa_kirjoittaa;
static int sound_on=0;
static int sound_up_down;
static int soundbin_luettu=0;
static int r_rivi, r_sar, r_var, r_return_firstvar, r_return_var, r_return_sar;
static int r_firstvar, r_havainto;
static int jatkuva_haku;
static int tempo2;
static char tiedotus[LLENGTH]; /* ohjeteksti alarivillä */
static char lopetus[]="To stop, press EXIT! (F1=HELP)";
static char edsana[2*LLENGTH];   /* 2* 18.3.92 */
static char vertsana[LLENGTH];
static char hakutieto[LLENGTH];
static char hakuavain[64];
static char polku[LLENGTH];
static char siirtop[32];
static int *v,*varpit,*varsar,*suojattu;
static double *min,*max;
static int *strarvo;   /* 21.12.91 */
static unsigned int *form;
static char *varj,*varj2;
static int mnimet,mnimet2;
static int max_dim;
static char options[LNAME];
static int not_found=0;
static int oikealle_yli;
static int tab_pakko=0;
static int pilkku_pisteeksi=0; /* 16.10.2002 */
static int suljettu;
static char stripe[LLENGTH];

/* RS lisätty */
static int block_ind=0;
static int ordind=0;
static long *ord=NULL;
static int s_insert_mode;
static int ndisp;
static long b_first,b_last;
static int s_insert_mode;
static int oikealle_yli;
static int tab_pakko;
static char stripe[];
static int special;

static char *formtila;
static long n_alku;
static int nlev,ndisp;
static double fs_luku;
static char sound_char=' ';

/* RS: SHOWH:sta */
static long nmax,n1,nn;

static char code[256]; // RS REM unsigned
static int koodit=0;

static int block_ind;
static long b_first, b_last;


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
/* RS REM       char rivi[LLENGTH]; */
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
            if (k>rivinpit) k=rivinpit; // RS ADD Restricted length for wide strings
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


static void disp_nimi();
static void disp_field_up();

static void n_display()
        {
        int i;
        
        PR_ENRM;
        LOCATE(2,1);
        sprintf(sbuf,"File %.16s N=%ld ",active_data,n);
        i=strlen(sbuf)-1; while (i<c3+8) { *(sbuf+i)=' '; i++; } *(sbuf+i)=EOS; // RS ADD
        sur_print(sbuf);
        disp_nimi();
        disp_field_up();
        }


static int show_init()
        {
        int i; /* RS ,k,h; */
        char varjo, varjo2;
        long li;

        PR_ENRM; CLS;
        n_display();

        li=(unsigned int)n; if ((long)dat.m>li) li=dat.m;
 // RS CHA (unsigned int) 
        nlev=log((double)(li+1))/log(10.0)+2;
        sar=nlev+1;
        for (i=0; i<m_act; ++i)
            {
/*            if (sar+varpit[i]>rivinpit) { m_act=i; break; }    */
            varsar[i]=sar;
            sar+=varpit[i];
            }
        ndisp=r3-2; ensrivi=4;
        rivinpit=c3+8-nlev-1;
//Rprintf("\nrivinpit: %d",rivinpit);


        varjo='7'; varjo2='4';
        for (i=0; i<m_act; ++i)
            {
            varj[i]=varjo; varj2[i]=varjo2;
            if (suojattu[i]) { if (varjo2=='4') varj[i]='3'; else varj[i]='1'; }
            if (varjo=='7') varjo='8'; else varjo='7';
            if (varjo2=='4') varjo2=' '; else varjo2='4';
            }

        firstvar=0;
// RS FIXME? dat.n long in struct!        
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
        formtila=(char *)muste_malloc(formpit);
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
                formtila=(char *)muste_realloc(formtila,formpit);
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
        v=(int *)muste_malloc(m_act*sizeof(int));
        if (v==NULL) { tilanpuute(); return(-1); }
        varpit=(int *)muste_malloc(m_act*sizeof(int));
        if (varpit==NULL) { tilanpuute(); return(-1); }
        varsar=(int *)muste_malloc(m_act*sizeof(int));
        if (varsar==NULL) { tilanpuute(); return(-1); }
        min=(double *)muste_malloc(m_act*sizeof(double));
        if (min==NULL) { tilanpuute(); return(-1); }
        max=(double *)muste_malloc(m_act*sizeof(double));
        if (max==NULL) { tilanpuute(); return(-1); }
        strarvo=(int *)muste_malloc(m_act*sizeof(int));         /* 21.12.91 */
        if (strarvo==NULL) { tilanpuute(); return(-1); }
        form=(unsigned int *)muste_malloc(m_act*sizeof(int));
        if (form==NULL) { tilanpuute(); return(-1); }
        suojattu=(int *)muste_malloc(m_act*sizeof(int));
        if (suojattu==NULL) { tilanpuute(); return(-1); }
        varj=(char *)muste_malloc(m_act);
        if (varj==NULL) { tilanpuute(); return(-1); }
        varj2=(char *)muste_malloc(m_act);
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
        ord=(long *)muste_realloc(ord,nmax*sizeof(long)); // RS CHA malloc -> realloc
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
//            h=strlen(sana); if (h<pit) strncat(sana,space,(unsigned int)(pit-h));
            
            len=strlen(sana);
            for (h=0; h<len; ++h) if (sana[h]=='\r') sana[h]=' '; // RS ADD
            if (len<pit)
                {
                for (h=len; h<pit; ++h) sana[h]=' '; sana[pit]=EOS;
                }             
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
//Rprintf("\ndisp_field: varpit: %d, sana: %s",varpit[i],sana);        
        write_string(sana,varpit[i],varjo,rivi,sar);
        }

static int muste_showlongstrings(char *sana, int x, int y, int lev, char shadow)
    {
            int k,pit,vpit,tpit,spit,point,pointlisa; // RS ADD
            pit=strlen(sana); vpit=lev;      
            if (pit>vpit)
                {
                k=0; point=0;
                while (k<ndisp+2 && point<pit)
                    {    
                    strncpy(sbuf,sana+point,vpit); *(sbuf+vpit)=EOS;
                    pointlisa=vpit-1;                   
                    while ((*(sbuf+pointlisa)!=EOS && 
                            *(sbuf+pointlisa)!=' ' &&
                            *(sbuf+pointlisa)!=',' &&
                            *(sbuf+pointlisa)!='.' &&                            
                            *(sbuf+pointlisa)!='\t' &&
                            *(sbuf+pointlisa)!='-') && 
                            pointlisa>0)
                        {                         
                          if ((*(sbuf+pointlisa)=='-') || (*(sbuf+pointlisa)==',') || (*(sbuf+pointlisa)=='.'))                          
                           { *(sbuf+pointlisa+1)=EOS; }
                          else 
                          { *(sbuf+pointlisa)=EOS; pointlisa--;  }                        
                          }    
                    if (pointlisa<=0) { strncpy(sbuf,sana+point,vpit); pointlisa=vpit; }
                    spit=strlen(sbuf);
                    if (spit<vpit)          
                        {
                        for (tpit=spit; tpit<=vpit; tpit++)
                           *(sbuf+tpit)=' ';
                        }                            
                    write_string(sbuf,vpit,shadow,y+k,x);
                    k++; point+=pointlisa+1;
                    }
                 }   
            return(k);
     }    

static void disp_field2(long j1,int i,int rivi,int sar,char varjo) // RS ADD
        {
        char sana[2*LLENGTH];
        int k,point,pit,vpit,tpit;

        poimi(j1,i,sana);
        write_string(sana,varpit[i],varjo,rivi,sar);
        
        if (strlen(sana)>rivinpit) 
            {
            k=muste_showlongstrings(sana,sar+1,rivi+1,varpit[i]-1,';');
//            if (rivi>ensrivi) { rivi=ensrivi; disp_nimi(); }
            }
/*
        if (pit>vpit)
            {
            k=0; point=0;
            while (k<ndisp-1 && point<pit)
                {    
                strncpy(sbuf,sana+point,vpit-1);
                if (pit-point<vpit-1)          
                    {
                    for (tpit=0; tpit<=(vpit-1); tpit++)
                        *(sbuf+(pit-point)+tpit)=' ';
                    }            
                tpit=vpit-1;                 
                write_string(sbuf,tpit,';',rivi+k+1,sar+1);
                k++; point+=vpit-1;
                }
            k++;  
*/            

                
//Rprintf("\nndisp=%d, rivi=%d, pit=%d, varpit[i]=%d",ndisp,rivi,pit,varpit[i]);           

            if (k<ndisp-1)
                {
                poimi(j1+k,i,sana);
                write_string(sana,varpit[i],varjo,rivi+k,sar);
                }

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
            if (j1==j && i==firstvar && saa_kirjoittaa==0 && muste_showlongvar==1) 
                disp_field2(j1,i,rivi,varsar[i]-firstsar,varj2[i]); // RS ADD
            else disp_field(j1,i,rivi,varsar[i]-firstsar,varj2[i]);
            }
        i=varsar[lastvar]-firstsar+varpit[lastvar];       
//        if (i>c3+8) i=c3+8-1; // RS ADD  
//        if (i<0) i=0; // RS ADD
        write_string(space,c3+8-i+1,159,rivi,i);  /* RS oli '\237' */
        }

static int disp_ots()
        {
        int i,k,h;

/*      SCROLL_UP(ensrivi-2,ensrivi+ndisp-2,ndisp+1);   korjattava! */
/*      sur_scroll_up(ndisp+1,ensrivi-2,1,ensrivi+ndisp-2,c3+8,0);  */

        firstsar=varsar[firstvar]-nlev-1;
//Rprintf("\ndisp_ots, firstsar: %d, firstvar: %d, varsar[firstvar]: %d, nlev: %d",firstsar,firstvar,varsar[firstvar],nlev);
        
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
// Rprintf("\ndisp_ots k: %d",k);            
//            if (k<1) k=1; // RS ADD
//            if (k>40) k=40; // RS ADD

/* RS FIXME muotoiltu tulostus ei toimi ääkkösten kanssa:
  sprintf(sbuf,"%.*s",k,dat.varname[v[i]]);
*/
            sprintf(sbuf,"%s",dat.varname[v[i]]);  /* RS FIXME tämä ehkä tulostaa liian pitkiä stringejä */
//            snprintf(sbuf,40,"%s",dat.varname[v[i]]);  

            for (h=8; h<k; ++h) sbuf[h]=' ';

            write_string(sbuf,k,varj[i],ensrivi-1,varsar[i]-firstsar);

            }
        i=varsar[lastvar]-firstsar+varpit[lastvar];
        if (lastvar<m_act-1)
            {
            write_string(stripe,c3+8-i+1,159,ensrivi-1,i); /* RS oli '\237' */
            }
        else
            {
            write_string(space,c3+8-i+1,159,ensrivi-1,i);  /* RS oli '\237' */
            }
        return(1);
        }

static void disp_field_up();
static void disp_recs(long j)
        {
        long i; // RS CHA int i

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

// RS ADD Show wide strings
        fi_rewind(&dat); 
        disp_hav(j,j);

        if (muste_showlongvar==2) disp_field_up(); // RS ADD

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
        char sana[2*LLENGTH];
/* RS       char x[LLENGTH]; */

        if (dat.vartype[0][0]!='S') return;
        if (muste_showlonglen>24) return; // RS ADD
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
        int k,pit,vpit,tpit,point; // RS ADD

        poimi(havainto+rivi-ensrivi,var,sana);
        if (dat.vartype[v[var]][0]!='S')
            {
            i=varpit[var];
            if (sana[i-1]=='*')
            fconv(fs_luku,"",sana);
            }
        i=strlen(sana); 
/* printf("s=%s",sana); getch();  */
        write_string(space,32,' ',2,c3-24);
        write_string(dat.varname[v[var]],8,'1',2,c3-24);
        if (sound_on) sound_char=sana[sar-varsar[var]+firstsar];
        sprintf(sbuf,"%*d ",nlev-1,v[var]+1);
        write_string(sbuf,nlev,'4',ensrivi-1,1);
        if (mnimet) disp_muuttujan_nimi("");
        
        if (i>rivinpit && muste_showlongvar==2 && saa_kirjoittaa==0)
            {
            k=muste_showlongstrings(sana,c3+8-muste_showlonglen,2,muste_showlonglen,'7');

/*
            if (k<ndisp-1)
                {
                poimi(j1+k,var,sana);
                write_string(sana,varpit[i],varjo,rivi+k,sar);
                }
*/
            
/*            
            pit=i; vpit=24;      
            if (pit>vpit)
                {
                k=0; point=0;
                while (k<ndisp+2 && point<pit)
                    {    
                    strncpy(sbuf,sana+point,vpit);
                    if (pit-point<vpit)          
                        {
                        for (tpit=0; tpit<=(vpit); tpit++)
                           *(sbuf+(pit-point)+tpit)=' ';
                        }                            
                    write_string(sbuf,vpit,'7',2+k,c3-16);
                    k++; point+=vpit;
                    }
                 }   
            return;
*/            
            }        
        else
        {
        if (i>24) i=24;
        write_string(sana,i,'7',2,c3-16);
        }
        }


static void kirjlupa()
        {
        putsaa(); 
//        BEEP;
        LOCATE(r3+2,1); PR_EBLD;
        sur_print("You get permission for editing by pressing F3. (Press first any key!)");
        LOCATE(rivi,sar); 
        sur_getch(); // RS CHA nextch(""); 
        putsaa(); LOCATE(rivi,sar);
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
//Rprintf("\nlast: %d, firstvar: %d, firstsar: %d, varpit[last]: %d, rivinpit: %d",last,firstvar,firstsar,varpit[i],rivinpit);        
        if (varpit[i]>rivinpit) return(i);
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
        fi_puts(s,&n,sizeof(int),22); // RS CHA 64-BIT sizeof(long) -> sizeof(int) 22L -> 22
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
        int pros;
        char datanimi[LLENGTH],apunimi[LLENGTH];
// RS unused        char *p,*q;

        n_update(&dat,n);

        for (j=1; j<=n; ++j)
            if (ord[j]!=j) break;
        if (j>n) return(1);

        strcpy(apunimi,polku); strcat(apunimi,"SURVO.XXX");
        apu=muste_fopen(apunimi,"wb");
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
        pros=0; alku=0; 
        for (j=1; j<=n; ++j)
            {
            alku++;
            if (alku>n/100)
                {
                alku=0; pros++;
//                sprintf(sbuf,"%ld",j);
                sprintf(sbuf,"%d%%",pros);
                LOCATE(r3+2,24); sur_print(sbuf);
                }
            
            fi_gets(&dat,dat.obs,dat.len,
                        (long)(dat.data+(long)((jj(j)-1L)*(long)(dat.len))));

            for (i=0; i<dat.len; ++i)
                putc((int)dat.obs[i],apu);
            if (ferror(apu)) { apu_error(apunimi); return(-1); }
            }
        muste_fclose(apu);
        fi_close(&dat);
        fi_find(word[2],&dat,datanimi);
// printf("\ndatanimi=%s|",datanimi); getch();
//      fi_close(&dat);  väärin, koska fi_find ei varaa tiloja!!!
        muste_fclose(dat.survo_data);
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

/* RS FIXME NYI: ei vielä mukana
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

            BEEP;
            while (1)
                {
                vi=sur_getch();
                if (vi==CODE_TAB || vi==CODE_RETURN) break;
                BEEP;
                }

            LOCATE(r3+2,70); PR_ENRM;
            LOCATE(rivi,sar);
            }

        if (strcmp(vertsana,edsana)==0) { muutokset=0; return(1); }
        j=havainto+rivi-ensrivi;
        if (j==n+1)
            {

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
                            return(-1); // RS CHA exit(1); -> return(-1);
                            }
                        LOCATE(r3+2,1);
                        PR_EBLD;
                        sprintf(sbuf,"%s is not a permitted value for %.8s",
                                edsana,dat.varname[v[var]]);
                        sur_print(sbuf);
                        BEEP;
                        sur_print("  Press any key!");
                        LOCATE(rivi,sar);
                        sur_getch(); // RS CHA nextch("");
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
                    return(-1); // RS CHA exit(1); -> return(-1);
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
                sur_getch(); // RS CHA nextch("");
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

        if ((sar==varsar[var]-firstsar+varpit[var]-1) || varpit[var]>rivinpit) // RS ADD second cond
            {
            if (var>=lastvar) // RS CHA == -> >=
                {
                if (lastvar>=m_act-1) return(1); // RS CHA == -> >=
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
        disp_hav(havainto,havainto); // RS ADD
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
        disp_hav(havainto,havainto); // RS ADD
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
        if (muste_showlongvar==2) disp_recs(havainto); // RS ADD
        else disp_hav(havainto,havainto); /* disp_nimi();  */
        }



static int relaatio(char *s,char *prel,char *arvo) /* *s hakuavain */
        {
// RS REM        char *p;

        if (*s=='*') { *prel=' '; return(1); }
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

static void haettu(long hav)
        {
        havainto=hav;
        putsaa();
        disp_recs(havainto); rivi=ensrivi; disp_nimi();
        }


void conv(unsigned char *sana,unsigned char *code)
        {
        int i;

        for (i=0; i<strlen(sana); ++i) sana[i]=code[(int)sana[i]];
        }


int load_codes(char *codefile,unsigned char *code)
        {
        int i;
        char x[LLENGTH];
        FILE *codes;  /* -4.1.1997 defined as local! */


        strcpy(x,codefile);
        if (strchr(x,':')==NULL && *x!='.') // RS FIXME filepaths
            { strcpy(x,survo_path); strcat(x,"SYS/"); strcat(x,codefile); }
            
        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            PR_EBLD;
            sprintf(sbuf,"\nCode conversion file %s not found!",x);
            sur_print(sbuf);
            WAIT; PR_ENRM; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose(codes);
        return(1);
        }

static int vertpituus(char *arvo,long hav,int len)
        {
        int i;
        char hakusana[2*LLENGTH];

        fi_alpha_load(&dat,hav,v[var],hakusana);
        conv(hakusana,code);
        for (i=0; i<len; ++i)
            {
            if (arvo[i]!=hakusana[i]) break;
            }
        return(i);
        }

static int binhaku(char *arvo)
        {
        int i,k;
        long hav,hav1,hav2;
        char type;
        char hakusana[2*LLENGTH];
        int len=strlen(arvo);

//      type=dat.vartype[var][0];
        type=dat.vartype[v[var]][0]; // 18.5.2001
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
                fi_alpha_load(&dat,hav,v[var],hakusana);
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
            else { sur_print("\nNot available for numeric fields!"); WAIT;
                   return(-1);
                       /* type!='S' estetty jo aikaisemmin */
                 }
            }
        return(1);
        }


static int etsi()
        {
        int i;
        long hav;
        char rel; /*  = < > S(>=) P(<=) E(<>) E(!=) */
        char arvo[LLENGTH];
        char hakusana[LLENGTH];
        char type;
        int len;
        double x,y;
        int osahaku;
        char arvo2[LLENGTH];

        osahaku=0; if (*hakuavain=='*') { osahaku=1; strcpy(arvo,hakuavain+1); }
        hav=atol(hakuavain);

        if (hav>0L && hav<=n)
            {
            havainto=hav;
            putsaa();
            disp_recs(havainto); rivi=ensrivi; disp_nimi();
            return(2);
            }

/*      if (*hakuavain==EOS) { putsaa(); osoita(var); return(1); } */
        if (*hakuavain==EOS) rel='M';  /* searching for missing value */
        else i=relaatio(hakuavain,&rel,arvo);
        if (i<0) { putsaa(); osoita(var); return(2); }

        len=strlen(arvo); y=atof(arvo);
 /*     type=dat.vartype[var][0];   -4.10.1992  */
        type=dat.vartype[v[var]][0];
        if (rel==' ' && type!='S') rel='=';
        if (osahaku) rel=' ';
// printf("\nsortvar=%d v[var]=%d type=%c|",sortvar,v[var],type); getch();
        if (!ordind && !osahaku && sortvar==v[var] && type=='S' && (rel==' ' || rel=='='))
            {
            strcpy(arvo2,arvo);
            binhaku(arvo);
            if (etu==2)
                {
                poimi(havainto,var,hakusana);
                i=strlen(hakusana); while (i>0 && hakusana[i-1]==' ') hakusana[--i]=EOS;
                if (strcmp(arvo2,hakusana)!=0)
                    {
                    sprintf(tut_info,"___@9@FILE SHOW@Case %s not found!@",arvo2);
                    return(-1);
                    }
                }
            putsaa(); osoita(var);
            return(2);
            }
        hav=havainto;
        while (1)
            {
            if (sur_kbhit()) { sur_getch(); putsaa(); osoita(var); return(1); }
            ++hav;
            if (hav>dat.n)
                {
                putsaa();
                LOCATE(r3+2,1); PR_EBLD;
                if (n_haku==0)
                    {
                    if (etu==2)  /* 1.5.91 */
                        {
                        sprintf(tut_info,"___@9@FILE SHOW@Case %s not found!@",arvo);
                        return(-1);
                        }
                    sur_print("Not found!");
                    not_found=1;
                    }
                else
                    { sprintf(sbuf,"%.8s%s: %d cases found.",dat.varname[v[var]],hakuavain,n_haku);
                      sur_print(sbuf);
                    }
                sur_print(" Press any key!");
                osoita(var);
                sur_getch(); // RS CHA nextch("");
                putsaa(); osoita(var); return(2);
                }
            if (rel==' ')
                {
      /*        fi_alpha_load(&dat,hav,v[var],hakusana);   */
                poimi(hav,var,hakusana);
                if ( (!osahaku && strncmp(hakusana,arvo,len)==0) ||
                     (osahaku && strstr(hakusana,arvo)!=NULL) )
                    {
                    ++n_haku;
                    havainto=hav;
                    if (jatkuva_haku) return(1);
                    putsaa();
                    disp_recs(havainto); rivi=ensrivi; disp_nimi();
                    return(1);
                    }
                LOCATE(r3+2,70); PR_EBLD;
                sprintf(sbuf,"%ld",hav); sur_print(sbuf);
                if (sur_kbhit()) { i=sur_getch(); if (i=='.') { putsaa(); return(2); } }
                continue;
                }
            fi_load(&dat,hav,v[var],&x);
            if (x==MISSING8)
                {
                if (rel=='M') i=1; else continue;
                }
            else
                {
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
                }
            if (i)
                {
                ++n_haku;
                havainto=hav;
                if (jatkuva_haku) return(1);
                putsaa();
                disp_recs(havainto); rivi=ensrivi; /* disp_nimi(); */
                return(1);
                }
            LOCATE(r3+2,70); PR_EBLD;
            sprintf(sbuf,"%ld",hav); sur_print(sbuf);
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') { putsaa(); return(2); } }
            continue;
            }
        }

static void prefix_code(int ch)
        {
        int i,k;
        char x[LLENGTH];
        long hav0;

        prefix=0;
        switch (ch)
            {
          case CODE_RETURN:
            return_firstvar=firstvar;
            return_var=var;
            return_sar=sar;
            break;
          case CODE_SRCH:
            mnimet2=mnimet; mnimet=0;
            i=talletus(); if (i<0) break;
            putsaa();
            LOCATE(r3+2,1);
            PR_EBLD; *x=EOS;
            prompt("Field to be found ? ",x,8);
            k=varnro(&dat,x);
            for (i=0; i<m_act; ++i)
                {
                if (v[i]==k) break;
                }
            if (i==m_act)
                {
                if (etu==2)
                    {
                    sprintf(tut_info,"___@7@FILE SHOW@Field %.8s not found!@",x);
                    return; // RS CHA exit(1); -> return;
                    }
                putsaa();
                LOCATE(r3+2,1);
                sprintf(sbuf,"Field %.8s not found! (Press any key!)",x);
                sur_print(sbuf);
                sur_getch(); // RS CHA nextch(""); 
                putsaa();
                mnimet=mnimet2;
                break;
                }
            putsaa();
            firstvar=i; var=i; disp_recs(havainto);
            sar=varsar[var]-firstsar;
            mnimet=mnimet2;
            break;
          case CODE_REF:
            sound_on_off();
            break;
          case CODE_EXEC:
            mnimet2=mnimet; mnimet=0;
            jatkuva_haku=1;
            hav0=havainto;
            i=talletus(); if (i<0) break;
            putsaa();
            LOCATE(r3+2,1); PR_EBLD;
            sprintf(sbuf,"Computing number of cases  %.8s%s ...",dat.varname[var],hakuavain);
            sur_print(sbuf);
            while (1)
                {
                i=etsi();
                if (i==2) { mnimet=mnimet2; break; }
                }
            havainto=hav0;
            mnimet=mnimet2;
            break;
          case CODE_DOWN:
            i=talletus(); if (i<0) break;
            if (sound_on)
                {
                while(1)
                    {
                    long li;
                    int m;

                    disp_field_up(); disp_nimi();
                    if (havainto+rivi-ensrivi>n) break;
                    alas();
                    if (rivi<ensrivi+ndisp-1)  /* vain ajan tasaamiseksi */
                        disp_hav(havainto+rivi-ensrivi,havainto);
                    LOCATE(rivi,sar);
                    if (sur_kbhit())
                        {
                        m=sur_getch();
                        if (m=='+') { --tempo2; if (tempo2<0) tempo2=0; }
                        else if (m=='-') ++tempo2;
                        else break;
                        }
                    for (li=0L; li<500L*(long)tempo2; ++li) ;
                    }
                break;
                }
            if (rivi<ensrivi+ndisp-1) { rivi=ensrivi+ndisp-1; break; }
            havainto=n+1; rivi=ensrivi; disp_recs(havainto);
            break;
          case CODE_UP:
            i=talletus(); if (i<0) break;
            if (sound_on)
                {
                while(1)
                    {
                    long li;
                    int m;

                    disp_field_up(); disp_nimi();
                    if (havainto+rivi-ensrivi==1L) break;
                    ylos();
                    if (rivi>ensrivi)  /* vain ajan tasaamiseksi */
                        disp_hav(havainto+rivi-ensrivi,havainto);
                    LOCATE(rivi,sar);
                    if (sur_kbhit())
                        {
                        m=sur_getch();
                        if (m=='+') { --tempo2; if (tempo2<0) tempo2=0; }
                        else if (m=='-') ++tempo2;
                        else break;
                        }
                    for (li=0L; li<500L*(long)tempo2; ++li) ;
                    }
                break;
                }
            if (rivi>ensrivi) { rivi=ensrivi; break; }
            havainto=1L; disp_recs(havainto);
            break;
          case CODE_PRE:
            i=talletus(); if (i<0) break;
            if (var<lastvar) { var=lastvar; sar=varsar[var]-firstsar; break; }
            break;
          case CODE_MERGE:
            mnimet=1-mnimet;
            if (!mnimet)
                {
                putsaa();
                LOCATE(rivi,sar);
                strcpy(tiedotus,lopetus);
                break;
                }
            *tiedotus=EOS;
            disp_muuttujan_nimi("");
            break;
// RS No need anymore FIXME??     case 'T': case 't': if(etu>1) tut_special(); break;   // RS ADD  
            }
          
            
        }

static int del_column_temporarily()  // ctrl-DEL
    {
    int i;
    int rr,ss;

    if (m_act==1) return(1);
    for (i=var+1; i<m_act; ++i)
        {

        v[i-1]=v[i];

        varpit[i-1]=varpit[i];
        varsar[i-1]=varsar[i];
        min[i-1]=min[i];
        max[i-1]=max[i];
        strarvo[i-1]=strarvo[i];
        form[i-1]=form[i];
        suojattu[i-1]=suojattu[i];
        varj[i-1]=varj[i];
        varj2[i-1]=varj2[i];
        }
    --m_act;
// printf("\nvar=%d: ",var);
// for (i=0; i<m_act; ++i) printf("%d ",v[i]); printf("|"); getch();
    rr=rivi; ss=sar;
//  varinfo();
    show_init();
    disp_recs(havainto); disp_nimi();
    rivi=rr; sar=ss;

   if (var==m_act) --var;
   sar=varsar[var];

    LOCATE(rivi,sar);
    return(1);
    }



void disp_nros(long j1,long j2,long j)
        {
// RS REM        int i;
        char x[LLENGTH];
        int rivi;
        long j0;
        char varjo;

        for (j0=j1; j0<=j2; ++j0)
            {
            rivi=j0-j+ensrivi;
            sprintf(x,"%*ld",nlev,j0);
            varjo='1';
            if (block_ind>1) { if (j0>=b_first && j0<=b_last) varjo='5'; }
            write_string(x,nlev,varjo,rivi,1);
            }
        }



static void delete_char()
        {
        int i,k,h;

        if (block_ind)
            {
            block_ind=0; putsaa();
            disp_nros(havainto,havainto+ndisp-1,havainto);
            strcpy(tiedotus,lopetus); return;
            }
        if (suojattu[var]) return;
        if (havainto+rivi-ensrivi>n+1) { BEEP; return; }
        if (!saa_kirjoittaa) { kirjlupa(); return; }
        if (!muutokset)
            {
            osoita(var);
            }
        ++muutokset;
        k=varpit[var]-1;
        h=sar-varsar[var]+firstsar;
        for (i=h; i<k; ++i)
            edsana[i]=edsana[i+1];
        edsana[k]=' ';
        write_string(edsana+h,k-h+1,varj[var],rivi,sar);
        }


static void erase_field()
        {
        int i,k,h;

        if (suojattu[var]) return;
        if (havainto+rivi-ensrivi>n+1) { BEEP; return; }
        if (!saa_kirjoittaa) { kirjlupa(); return; }
        if (!muutokset)
            {
            osoita(var);
            }
        ++muutokset;
        k=varpit[var]-1;
        h=sar-varsar[var]+firstsar;
        for (i=h; i<=k; ++i)
            edsana[i]=' ';
        write_string(edsana+h,k-h+1,varj[var],rivi,sar);
        talletus();
        }


static void erase_recs()
        {
        int m;
        long j,ero;

        if(!ord_available()) return;
        if (!saa_kirjoittaa) { kirjlupa(); return; }
        putsaa(); PR_EBLD; LOCATE(r3+2,1);
        sprintf(sbuf,"Deleting block of records %ld - %ld (Y/N) ? ",b_first,b_last);
        sur_print(sbuf);
        m=sur_getch(); // RS CHA nextch("");
        if (m=='Y' || m=='y')
            {
            ero=b_last-b_first+1;
            for (j=b_first; j<=n-ero; ++j)
                ord[j]=ord[j+ero];
            n-=ero;
            }
        else { putsaa(); return; }
        putsaa();
        strcpy(tiedotus,lopetus);
        block_ind=0;
        if (havainto>n) { havainto=n; if (n==0) havainto=1; }
        disp_recs(havainto);
        }

static void delete_rec()
        {
        long j;

        if(!ord_available()) return;
        j=havainto+rivi-ensrivi;
        if (j>n) return;
        if (!saa_kirjoittaa) { kirjlupa(); return; }
        if (block_ind>1)
            {
            if (j<b_first) { --b_first; --b_last; }
            else if (j<=b_last) return;
            }
        for (j=havainto+rivi-ensrivi; j<=n; ++j) ord[j]=ord[j+1];
        --n; n_display();
        SCROLL_UP(rivi-1,ensrivi+ndisp-2,1);
        disp_hav(havainto+ndisp-1,havainto);
        disp_nros(havainto+rivi-ensrivi,havainto+ndisp-1,havainto);
        disp_nimi();
        }

static void insert_rec()
        {
        long j,j0;

        if(!ord_available()) return;
        j0=havainto+rivi-ensrivi+1;
        if (j0>n) return;
        if (!saa_kirjoittaa) { kirjlupa(); return; }
        if (!voi_lisata(n+1)) return;
        if (block_ind>1)
            {
            if (j0-1<b_first) { ++b_first; ++b_last; }
            else if (j0<=b_last) return;
            }
        for (j=n; j>=j0; --j) ord[j+1]=ord[j];
        ++n1; ord[j0]=n1;
        ++n; n_display();
        fi_rewind(&dat); fi_miss_obs(&dat,n1);
        if (rivi-ensrivi<ndisp-1)
            {
            SCROLL_DOWN(rivi,ensrivi+ndisp-2,1);
            disp_hav(j0,havainto);
            disp_nros(havainto+rivi-ensrivi,havainto+ndisp-1,havainto);
            ++rivi;
            }
 /*     disp_recs(havainto); disp_nimi();     */
        disp_nimi();
        }

/* RS not used?
static void etsi_polku(char *nimi,char *polku)
        {
        char *p,*q;
        char x[LLENGTH];

        strcpy(x,nimi);
        p=strchr(x,':'); if (p==NULL) p=x;
        q=x+strlen(x)-1;
        while (*q!='\\' && q>x) --q;
        if (q>x) p=q;
        if (p==x) strcpy(polku,edisk);
        else { *(p+1)=EOS; strcpy(polku,x); }
        }
*/

static void rot_up(long j1,long j2,long j3,long *ord2,long movemax)
        {
        long j;
        long nmove;

        while (j3)
            {
            nmove=movemax;
            if (j3<nmove) nmove=j3;
            j3-=nmove;
            for (j=j1; j<j1+nmove; ++j) ord2[j-j1]=ord[j];
            for (j=j1+nmove; j<=j2; ++j) ord[j-nmove]=ord[j];
            for (j=0; j<nmove; ++j) ord[j+j2-nmove+1]=ord2[j];
            }
        }

static void rot_down(long j1,long j2,long j3,long *ord2,long movemax)
        {
        long j;
        long nmove;

        while (j3)
            {
            nmove=movemax;
            if (j3<nmove) nmove=j3;
            j3-=nmove;
            for (j=0; j<nmove; ++j) ord2[j]=ord[j+j2-nmove+1];
            for (j=j2-nmove; j>=j1; --j) ord[j+nmove]=ord[j];
            for (j=j1; j<j1+nmove; ++j) ord[j]=ord2[j-j1];
            }
        }


static int block_move(long j1,long j2,long j0)
        {
// RS REM        long j;
// RS REM        int i;
        long *ord2;
        long movemax;


        if(!ord_available()) return(-1);
        if (!saa_kirjoittaa) { kirjlupa(); return(-2); }

        if (j0>=j1 && j0<=j2+1) return(1);

        movemax=nn+MAXLISAYS-n-1;
        ord2=ord+n+1;
        if (j0<j1)
            {
            if (j1-j0<=j2-j1+1) rot_up(j0,j2,j1-j0,ord2,movemax);
            else                rot_down(j0,j2,j2-j1+1,ord2,movemax);
            }
        else if (j0>j2+1)
            {
            if (j2-j1<=j0-j2) rot_up(j1,j0-1,j2-j1+1,ord2,movemax);
            else              rot_down(j1,j0-1,j0-j2-1,ord2,movemax);
            }
        return(1);
        }


static void block_rec()
        {
        int i;
        long j;

        putsaa();
        switch (block_ind)
            {
          case 0:
  strcpy(tiedotus,"Indicate the first record of the block by alt-F4:BLOCK! (Cancel by DELETE)");
            block_ind=1;
            break;
          case 1:
  strcpy(tiedotus,"Indicate the last record of the block by alt-F4:BLOCK! (Cancel by DELETE)");
            b_first=b_last=havainto+rivi-ensrivi;
            if (b_first>n) b_first=n;  /* 31.7.90 */
            block_ind=2;
            disp_nros(havainto,havainto+ndisp-1,havainto);
            break;
          case 2:
 strcpy(tiedotus,"Move the block by alt-F4:BLOCK or delete it by ctrl-END! (Cancel by DELETE)");
            b_last=havainto+rivi-ensrivi;
            if (b_last<b_first) { j=b_first; b_first=b_last; b_last=j; }
            if (b_last>n) b_last=n;  /* 31.7.90 */
            block_ind=3;
            disp_nros(havainto,havainto+ndisp-1,havainto);
            break;
          case 3:
            i=block_move(b_first,b_last,havainto+rivi-ensrivi);
            if (i==-2) break;
            block_ind=0;
            strcpy(tiedotus,lopetus);
            disp_recs(havainto);
            break;
            }
        }


static void copy_field()
        {
        char sana[LLENGTH];
        long j,j0;

        j=havainto+rivi-ensrivi;
        if (j>n+1) return;
        if (block_ind>1) j0=b_first;
        else if (block_ind==0) j0=j-1;
        else return;

        if (j0<1) return;
        if (!saa_kirjoittaa) { kirjlupa(); return; }
        poimi(j,var,sana);
        if (strncmp(sana,space,strlen(sana))!=0) return;
        poimi(j0,var,edsana);
        *vertsana=EOS; muutokset=1;
        talletus();
/*      if (j==n+1)     poistettu 27.5.89, koska talletus() tekee sen jo
            {
            ++n;
            n_update(&dat,n);
            n_display();
            }
*/
        }

static void copy_rec()
        {
        char sana[LLENGTH];
        long j,j0;
        int i;

        j=havainto+rivi-ensrivi;
        if (j>n+1) return;
        if (block_ind>1) j0=b_first;
        else if (block_ind==0) j0=j-1;
        else return;

        if (j0<1) return;
        if (!saa_kirjoittaa) { kirjlupa(); return; }
        for (i=0; i<m_act; ++i)
            {
            poimi(j,i,sana);
            if (strncmp(sana,space,strlen(sana))!=0) return;
            }

        copy_field(); /* 27.5.89 jotta n,n1 pÑivitetÑÑn oikein */

        fi_rewind(&dat);
        fi_gets(&dat,dat.obs,dat.len,
              (long)(dat.data+(jj(j0)-1L)*(long)dat.len));
        fi_rewind(&dat);
        fi_puts(&dat,dat.obs,dat.len,
              (long)(dat.data+(jj(j)-1L)*(long)dat.len));
        disp_hav(j,havainto);
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


// RS ADD Initialize variables
muste_showlongvar=2;
muste_showlonglen=24;
survo_ferror=0;
m_act=0;
m=0;
n=0;
havainto=0;    
viimeiseen=0;
suojaus=0;
muutokset=0;
rivi=sar=var=ensrivi=0;
prefix=return_firstvar=return_var=return_sar=0;
firstvar=lastvar=firstsar=rivinpit=0;
sortvar=n_haku=0;
saa_kirjoittaa=0;
sound_on=0;
sound_up_down=0;
soundbin_luettu=0;
r_rivi=r_sar=r_var=r_return_firstvar=r_return_var=r_return_sar=0;
r_firstvar=r_havainto=0;
jatkuva_haku=0;
tempo2=0;
mnimet=mnimet2=0;
max_dim=0;
not_found=0;
oikealle_yli=0;
tab_pakko=0;
pilkku_pisteeksi=0;
suljettu=0;

block_ind=0;
ordind=0;
s_insert_mode=0;
ndisp=0;
b_first=b_last=0;
s_insert_mode=0;
oikealle_yli=0;
tab_pakko=0;
special=0;

n_alku=0;
nlev=ndisp=0;
fs_luku=0;
nmax=n1=nn=0;

koodit=0;

block_ind=0;
b_first=b_last=0;

v=NULL;
varpit=NULL;
varsar=NULL;
suojattu=NULL;
min=NULL;
max=NULL;
strarvo=NULL;
form=NULL;
varj=NULL;
varj2=NULL;
ord=NULL;
formtila=NULL;

/* RS CHA      if (argc==1) return; 
        s_init(argv[1]); */
        s_init(argv);

        tut_init(); // RS CHECK
        if (r_soft) r3+=r_soft+1;
        for (i=0; i<LLENGTH-1; ++i) stripe[i]='.'; stripe[LLENGTH-1]=EOS;
        strcpy(siirtop,argv); /* RS CHA strcpy(siirtop,argv[1]); */
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
ei avaa mac snow leopardilla 4.12.2009:
long-tyyppiset muuttujat SURVO_DATA_FILE -sturctissa aiheuttavat hankaluuksia!!!


int apu;
for (apu=0; apu<dat.m; apu++) {
Rprintf("var %d; varpos: %d; varlen: %d; vartype: %s; varname: %s\n",apu,dat.varpos[apu],dat.varlen[apu],dat.vartype[apu],dat.varname[apu]);
}
*/


        polku[0]=EOS;  /* RS  FIXME Tyhjä polkunimi KORJAA */  
/*        strcpy(polku,word[2]);  RS FIXME Poluksi tiedoston nimi, KORJAA! */
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
                        
        m=dat.m; 
        n=(unsigned int)dat.n; // RS CHA (unsigned int)
// RS 64-bit kokeilua        sprintf(sbuf,"%u",(unsigned int)dat.n); n=atoi(sbuf);

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


        i=(unsigned int)n; if ((long)dat.m>i) i=dat.m;
        nlev=log((double)(i+1))/log(10.0)+2;
        rivinpit=c3+8-nlev-1; // RS ADD required for varinfo          
        
        i=varinfo(); if (i<0) return(1);
        i=show_init(); if (i<0) return(1);
        
        
        textinfo(); /* SORT:muuttuja  */
        if (!viimeiseen) havainto=1L; else havainto=dat.n+2L-(long)viimeiseen;
                                               // RS CHA   L (long)



/*********************************************/
        i=sp_init(r1+r-1); if (i<0) return(1); // RS ADD

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
            if (optio("S")) sound_on_off();
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
                    etsi();
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


        i=spec_find("SHOWLONG",sbuf,LNAME-1); // RS ADD
        muste_showlonglen=24;
        if (i>0)
            {
            muste_showlonglen=atoi(sbuf);
            if (muste_showlonglen>rivinpit-9) muste_showlonglen=rivinpit-9;
            if (muste_showlonglen<24) muste_showlonglen=24;
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
                    strcpy(tut_info,"___@6@FILE SHOW@Cannot save more data!"); // RS CHA þþþ -> ___
                    break;
                    }
                LOCATE(r3+2,1); PR_EBLD; // RS NYI   BEEP;
                sur_print("Cannot save! (Disk full?)  Press any key!"); WAIT;
                break;
                }
/* RS: ei vielä mukana - mitä tekee ja milloin??? */
            if ( sound_on || (etu==0 && !sur_kbhit()) || (etu==2 && etu1>1) )
                { disp_field_up(); disp_nimi(); }

// disp_field_up(); disp_nimi(); /* RS lisätty näytettäväksi joka kerta */

            sound_up_down=0;
            if (*tiedotus==EOS && !mnimet) strcpy(tiedotus,lopetus);

            LOCATE(rivi,sar);

            ch=nextch(tiedotus);
/* sprintf(sbuf,"\n%d|",(int)ch); sur_print(sbuf); */

            if (prefix) { prefix_code(ch); continue; }
            
//Rprintf("\nkey: %d, crtl: %d",ch, sur_ctrl);            
            
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
                if (s_insert_mode) CURSOR_INS; else CURSOR_ON;
                break;
              case CODE_DELETE:
                        
//Rprintf("\CODE_DELETE key: %d, crtl: %d",ch, sur_ctrl); 
                if (sur_ctrl) del_column_temporarily();
                else delete_char();
                break;
              case CODE_ERASE:
                if (block_ind>2) erase_recs();
                else erase_field();
                break;
              case CODE_DELETEL:
                i=talletus(); if (i<0) break;
                delete_rec();
                break;
              case CODE_INSERTL:
                i=talletus(); if (i<0) break;
                insert_rec();
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
                if (lastvar>=m_act-1) break;
                if (firstvar==lastvar) // RS ADD
                    {
                    firstvar=first_var(var+1); // RS ADD
                    var=firstvar;
                    disp_recs(havainto);
                    }
                else 
                    {
                    firstvar=lastvar; // disp_recs(havainto);
                    var=lastvar; 
                    disp_recs(havainto);
                    }
                sar=varsar[firstvar]-firstsar;
//Rprintf("\nendOUT, firstvar: %d, lastvar: %d, var: %d, sar: %d, firstsar: %d, varsar[var]: %d",firstvar,lastvar,var,sar,firstsar,varsar[firstvar]);                
                break;
              case CODE_TAB:
                i=talletus(); if (i<0) break;
                if (var>=lastvar) // RS CHA == -> >=
                    {
                    if (var>=m_act-1) break; // RS CHA == -> >=
                    firstvar=first_var(var+1); disp_recs(havainto);
                    }   
                ++var; sar=varsar[var]-firstsar;
                if (muste_showlongvar) disp_recs(havainto);                 
//Rprintf("\nvar: %d, lastvar: %d, varsar[var]: %d, firstsar: %d, sar: %d",var,lastvar,varsar[var],firstsar,sar);                
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
                prompt(hakutieto,hakuavain,16);
                osoita(var);
                n_haku=0;
                i=etsi(); if (etu>0 && i<0) kesken=0;
                mnimet=mnimet2;
                break;
              case CODE_EXEC:
                mnimet2=mnimet; mnimet=0;
                jatkuva_haku=0;
                i=talletus(); if (i<0) break;
                etsi();
                mnimet=mnimet2;
                break;
              case CODE_TOUCH:
                saa_kirjoittaa=1;
                n_display(); disp_recs(havainto); // RS ADD
                i=fi_to_write(word[2],&dat);
                if (i<0) return(1);
                fi_rewind(&dat);
                break;
              case CODE_MERGE:
                mnimet=0;
                disp_muuttujan_nimi(" (Press any key!)");
                sur_getch(); // RS CHA nextch("");
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
                block_rec();
                break;
              case CODE_DISK:
                copy_field();
                break;
              case CODE_DISP:
                copy_rec();
                break;
              case CODE_HELP:
                muste_showlongvar--;
                if (muste_showlongvar==1) muste_showlongvar--; // RS Skip showmode=1
                if (muste_showlongvar<0) muste_showlongvar=2; // RS ADD
                n_display();
                disp_recs(havainto); // RS ADD
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
                if (!saa_kirjoittaa) 
                    { 
                    if (muste_showlongvar==2) // RS ADD
                        {
                        if (ch=='+') muste_showlonglen++;
                        if (ch=='-') muste_showlonglen--;
                        if (muste_showlonglen>rivinpit-9) muste_showlonglen=rivinpit-9;
                        if (muste_showlonglen<24) muste_showlonglen=24;
                        n_display();
                        disp_nimi();
                        disp_recs(havainto);
                        break;
                        }
                    kirjlupa(); break; 
                    }
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
        tut_end(); // RS CHECK
        if (r_soft) r3-=r_soft+1;
/* RS CHA       s_end(argv[1]); */
        s_end(argv); 
        return(1);
        }
