/*  _print.c 18.10.1985/SM (8.12.1992) (10.3.1997)
 */

/* RS CLEAN list
exit
fopen
fseek
strcmpi
getch
kbhit
strnicmp
itoa

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"


#define TAB '\t'
#define NLEN 6           /* max. koodijonon pituus NLEN*LLENGTH */
#define KOODITILA 60000
#define KOODISANAT 3000
#define NTYPES 16 /* pitch-koodien lkm 12.11.1990 */
#define TABSPACE 8
#define VASEN_REUNA 100
#define ALAREUNA 2800
#define C_ESC '\33'
#define C_CSI '\233'
#define C_VDM '\175'
#define C_IS2 '\36'

static char muste_nullstring_print[]="";

static int sivulaskin=0;
static int erivi=0;  /* 1=C 2=R */
static int pakkovenytys=0;
static int odotus=0;
static char control_off[32];

static FILE *kirjoitin;
static char laite[LNAME];
static int prind2=1;  // prind toisessa kÑytîssÑ tÑssÑ modulissa!

static char win_printer_name[LNAME];
static char current_field[LNAME];

static unsigned int kooditila;
static unsigned int koodisanat;
static char *shadow[256];    /* varjorivin merkkien koodisanaosoittimet */
static char *shadow2[256];   /* varjorivin merkkien jÑlkikoodisanaosoittimet */
static int len_shadow[256];
static int len_shadow2[256];
static char *pr_tila;    /* koodijonot ja -sanat */
static char **pr_sana;   /* koodisanojen osoittimet */
static char **pr_koodi;  /* koodisanoja vastaavien koodijonojen osoittimet */
static int *len_pr_koodi;
static int n_sana;           /* koodisanojen lukumÑÑrÑ */
static char *pr_osoitin;     /* ens. vapaan paikan osoitin pr_tilassa */
static char code[256]; /* kooditaulukko */
static char *control[256];   /* kontrollisarakkeen merkkien koodisanaosoitteet */
static int len_control[256];
// RS REM static char *raster[256];    /* rasterimerkkien koodisanaosoitteet */
// RS REM static int len_raster[256];

static double line_count;
static int hline1,hline2,hline3,hline4;  /* otsikkorivit 3-4:parilliset sivut */
static int dev_luettu;
static int page_number, ind_page_number;
static int tila; /* 0=field 1=edit file */
static char tabrivi[LLENGTH];
static char tabrivi0[LLENGTH];
static char null_char='\0';
static char echo[LLENGTH];
static int pr_type=0;
static int prind;
static char defspace=EOS; /* 16.10.1996 */
static FILE *shadows_in_use;
static int silent=0; // 22.1.2007

static char header_varatabrivi[LLENGTH];
static double pistekerroin=0.28346456692913;

static double char_pitch, char_width, char_height;
static double line_length;
static double line_spacing, page_length;
static int pitch[256*NTYPES], pitch_unit, pitch_ind;
static double kirjainlev, kirjainkork;
static int gap[LLENGTH];
static int count_off;
static double font_size; /* vain PLOT? */
static double vstrike=0.0;
static double hstrike=0.0;
static int pitch_luettu=0;
static int line_continues=0;
static double end_norm_tol=1.0;
static double end_spec_tol=1.0;
static char euro_shadow='E';
static int printing_on=1;

static FILE *edfield;
static unsigned int ued1,ued2,uedshad;
static int *uzs;

static int ascii_text_saving=0;

static char vector_mode[]={ C_CSI,'0','&',C_VDM,EOS };
static FILE *canon_file;   /* canon or ps file */
static int x_origin,y_origin;

static char repl[LLENGTH];
static char *repl1,*repl2;

static char *ps_str[256];
static int len_ps_str[256];
// RS REM static int erikoismerkki_lopussa;
static double end_tol=1.0;
static double end_tol0=0.0;
static double ps_charwidth=0.0;

static int footnotes,n_footnote,n1_footnote;
static double note_spacing;
static FILE *fn_file;
static FILE *lst_file;
static FILE *ascii_file;
static FILE *text;


// extern int page_number;
static FILE *index_file;
static char rivin_loppu[LLENGTH];
static int loppu_merkitty;
static int index_mukana;  /* 9.7.89 */

static char *gchar[256];   /* graafisten merkkien koodisanaosoitteet */
static int len_gchar[256];
static int t_gchar[256]; /* 'T' tai ' ' */
static unsigned char gmerkit[256];
static int n_gchar;
static unsigned char gcharx[LLENGTH];
static int gchar_ind;
static int vpitch_unit;
static int gchar_on=0;


extern int posnro();
static int win_tulostus();
static int tulosta_rivit(int j1,int j2);
static int tulosta(unsigned char x[],unsigned char xs[]);
static int testaa_sivu();
static int uusi_sivu();
static void testaa_plus();
static int uusi_rivi();
static int dev_file(char *x);
static int load_dev();
static int pr_empty(char *s);
static void vdc(int arvo,char *luku);
static void binopen();
static int alkukoodit();
static int varaa_tilat();
static void not_enough_memory();
static int lue_koodit(char *x);
static void kirjoita(char *y);
static int define(char *x,char **sana,int n,char *rivi);
static int shadows(char *x,char **sana,int n,char *rivi);
static void sh_list_print(unsigned char ch);
static int def_space(char *x,char **sana,int n,char *rivi);
static int controls(char *x,char **sana,int n,char *rivi);
static int codes(char *x,char **sana,int n);
static int muunna(char *sana,char *muunnos);
static void koodivirhe(char *x);
static int null_ch(int n,char *s);
static int pr_test(int k);
static void load_codes(char *codefile,unsigned char *code);
//static int space_split(char rivi[],char *sana[],int max);
static int space_split(char *rivi,char **sana,int max);
static int makro(char *sana,char *muunnos);
static void korvaa(char *muunnos,char *s,char *t);
static int dos(char *x);
static int include(char *x,char **sana,int n);
static int hlines(char *x);
static int header_print();
static void set_page_number(char *x);
static int loppusulku(int j,int j2);
static int tyhjat_rivit(int j);
static int tekstityyppi();
static int p_special(char *s);
static int pitch_load();
static int strpitch(unsigned char *s);
static void p_charsize();
static int ptulosta(unsigned char *x,unsigned char *xs);
static int venytys(char *x,char *xs,int gap[]);
static void p_gap(char merkki,int gap);
static int chapter(char *x);
static void uvirhe(char *rivi,char *tied);
static void uedread(char *x,int j);
static int edt_avaus(char *edfile);
static int edt32_avaus(char *nimi);
static int edt32to16(char *name32,char *name16);
static int error_file_32_16(char *name);
static int sh_malloc(unsigned int ued2);
static int uwfind(char *word1,char *word2,int lin);
static int uedline(char *sana,unsigned int lin);
static int ulastline();
static int pr_list(char *x);
static int lst_file_find(char *lista);
static int pr_list2(char *chp,char *kent);
static int chp_edt_read(char *chp,char *edt);
static int lst_read_line(char *x);
static int edt_numbers(char *edt,int *pi1,int *pi2);
static void pr_filerr();
static int textfile(char *x);
static int textread(char *x,int j);
static int lue_text(char *rivi);
static void tab_poisto(char *s);
static int ascii_text(char *x);
static void ascii_save(char *x);
static int picture(char *x);
static int c_avaa(char *s);
static int etsi(char *s,int kopio);
static int merkki();
static void pict_file_error(char *s);
static int p_origin(int x,int y);
static void cat(char *s,char m);
static int send(char *s);
static int ps_picture(char *x);
static int tabs(int j);
static int tabvenytys(char *x,char *xs,int gap[]);
static int replace(char *x);
static void replacement(char *x);
static void roman(int n,char *x);
static void ps_init();
static int ps_code(char *x,char **sana,int n,char *rivi);
static int ps_replace(unsigned char *x);
static void uusi_vstrike(double a);
static void uusi_hstrike(double a);
static int ps_tulosta(unsigned char *x,unsigned char *xs);
static int ps_kirjoita(unsigned char *s);
static int ps_ptulosta(unsigned char *x,unsigned char *xs);
static int ps_print(unsigned char *xx,unsigned char *xxs,int tosi);
static int ps_tabprint(unsigned char *xx,unsigned char *xxs);
static void set_tabwidth();
static int ps_print2(unsigned char *xx,unsigned char *xxs,double xwidth,int tosi);
static int read_footnotes(char *x,char *xs,int j,int j2);
static void fn_replace(char *x,char *xs,int k,int n);
static int fn_testaa_sivu(int j,int j2);
static int fn_talleta(int j,int j2);
static void fn_save(char *x);
static int fn_tulosta();
static void fn_load(char *x);
static void set_footnote_number(char *s);
static int print_index(char *x,char **sana,int n,char *rivi);
static void pane_hakemistoon(char *x,char *xs);
static void korvaa_varjo(char *xs,char *p);
static void index_save(char *sana,int n);
static void init_gchars();
static int gchars(char *x,char **sana,int n,char *rivi);
static void gchar_erotus(unsigned char *x);
static void gchar_tulosta();
static int ps_autocad();
static int ps_file();
static int ps_epsfile();
static int etsi2(char *s,int kopio);


void muste_print(int argc, char *argv[])
        {
        int i,j1,j2;
        char x[LLENGTH];
        char *p,*q;
        char *s[2];
// RS REM        char print_exe[]="¿±≥™Øµè¶π¶";

// RS Variable init
sivulaskin=0;
erivi=0;
pakkovenytys=0;
odotus=0;
prind2=1;
null_char='\0';
pr_type=0;
defspace=EOS;
silent=0;
pistekerroin=0.28346456692913;
vstrike=0.0;
hstrike=0.0;
pitch_luettu=0;
line_continues=0;
end_norm_tol=1.0;
end_spec_tol=1.0;
euro_shadow='E';
printing_on=1;
ascii_text_saving=0;
end_tol=1.0;
end_tol0=0.0;
ps_charwidth=0.0;
gchar_on=0;

kirjoitin=NULL;
kooditila=0;
koodisanat=0;
pr_tila=NULL;    
pr_sana=NULL;  
pr_koodi=NULL;
len_pr_koodi=NULL;
n_sana=0;
pr_osoitin=NULL;
line_count=0;
hline1=hline2=hline3=hline4=0;  
dev_luettu=0;
page_number=ind_page_number=0;
tila=0; 
prind=0;
shadows_in_use=NULL;
char_pitch=char_width=char_height=0;
line_length=0;
line_spacing=page_length=0;
pitch_unit=pitch_ind=0;
kirjainlev=kirjainkork=0;
count_off=0;
font_size=0; 
edfield=NULL;
ued1=ued2=uedshad=0;
uzs=NULL;
canon_file=NULL;
x_origin=y_origin=0;
repl1=NULL;
repl2=NULL;
footnotes=n_footnote=n1_footnote=0;
note_spacing=0;
fn_file=NULL;
lst_file=NULL;
ascii_file=NULL;
text=NULL;
index_file=NULL;
loppu_merkitty=0;
index_mukana=0;  
n_gchar=0;
gchar_ind=0;
vpitch_unit=0;

for (i=0; i<256; i++) shadow[i]=NULL;
for (i=0; i<256; i++) shadow2[i]=NULL;
for (i=0; i<256; i++) control[i]=NULL;
for (i=0; i<256; i++) ps_str[i]=NULL;
for (i=0; i<256; i++) gchar[i]=NULL;


/* RS Still uninitialized
static char laite[LNAME];
static char control_off[32];
static char win_printer_name[LNAME];
static char current_field[LNAME];
static char *shadow[256];    
static char *shadow2[256];  
static int len_shadow[256];
static int len_shadow2[256];
static char code[256]; 
static char *control[256];  
static int len_control[256];
static char tabrivi[LLENGTH];
static char tabrivi0[LLENGTH];
static char echo[LLENGTH];
static char header_varatabrivi[LLENGTH];
static int pitch[256*NTYPES], 
static int gap[LLENGTH];
static char vector_mode[]={ C_CSI,'0','&',C_VDM,EOS };
static char repl[LLENGTH];
static char *ps_str[256];
static int len_ps_str[256];
static char rivin_loppu[LLENGTH];
static char *gchar[256];  
static int len_gchar[256];
static int t_gchar[256]; 
static unsigned char gmerkit[256];
static unsigned char gcharx[LLENGTH];
*/


        if (argc==1) return;
        s_init(argv[1]);

        if (g<2)
            {
            j1=r1+r; if (j1>r2) return;
            edread(x,j1); if (pr_empty(x+1)) return;
            j2=j1;
            while (!pr_empty(x+1) && j2<r2) edread(x,++j2);
            if (j2<r2) --j2;
            }
        else
            {
            j1=edline2(word[1],1,1); if (j1==0) return;
            j2=edline2(word[2],j1,1); if (j2==0) return;
            }
        if (g>4)
            {
            edread(x,r1+r-1);
            p=strchr(x,'"');
            if (p==NULL)
                strcpy(laite,word[4]);
            else
                {
                q=strchr(p+1,'"');
                if (q==NULL)
                    {
                    sur_print("Character \" missing!");
                    WAIT; return; // RS CHA exit(0);
                    }
                *laite=EOS; strncat(laite,p,q-p+1);
                }
            if (*laite!='"' && strchr(laite,':')==NULL)
                {
                strcpy(laite,edisk); strcat(laite,word[4]);
                }
            }
        else
            {
            i=hae_apu("printer",laite);
            if (i==0)
                sprintf(laite,"%sSURVO_PR.PS",etmpd);  // RS CHA strcpy(laite,"PRN");             
            }
// printf("\nprinter=%s|",laite); getch();
/*****************  10.4.2002 "nimen tarkistus" ilmaisversiota varten
printf("\nargv0=%s|",argv[0]+strlen(argv[0])-10); getch();
XOR +97
_PRINT.EXE
¿±≥™Øµè¶π¶
***********************/
/* RS REM
        strcpy(sbuf,print_exe);
        for (i=0; i<strlen(sbuf); ++i) sbuf[i]-=97;
        i=0;
        if (muste_strcmpi(sbuf,argv[0]+strlen(argv[0])-10)!=0) i=-1;

        if (i<0) return; // RS exit(0); // 10.4.2002
*/        
        *win_printer_name=EOS;
        if (*laite=='"')
            {
            strcpy(win_printer_name,laite);
            sprintf(laite,"%sSURVO_PR.PS",etmpd);
            }

        kirjoitin=muste_fopen(laite,"wt");
        if (kirjoitin==NULL)
            {
            PR_EBLD;
            sur_print("\nPrinter not on!");
            WAIT; PR_ENRM; return;
            }

        if (hae_apu("prind",sbuf)) prind2=atoi(sbuf);

        *current_field=EOS;
        edread(sbuf,1); i=split(sbuf+1,s,2);
        if (i==2 && muste_strcmpi(s[0],"SAVE")==0)
            strcpy(current_field,s[1]);

        i=alkukoodit();
        if (i<0) return; // RS ADD
        
        testaa_plus();
        tulosta_rivit(j1,j2);
        fn_tulosta(); /* 5.11.88 */
        strcpy(x,"[FORM_FEED]"); lue_koodit(x);
        if (pr_type==1)  { strcpy(x,"[END]"); lue_koodit(x); }

        muste_fclose(kirjoitin);
        if (sur_kbhit())
            {
            sprintf(sbuf,"\ncode words=%d code space=%d",(int)n_sana,(int)(pr_osoitin-pr_tila));
            sur_print(sbuf); sur_getch(); WAIT;
            }

        if (*win_printer_name!=EOS) win_tulostus();

        }


static int win_tulostus()
    {
    
muste_fixme("\nFIXME: win_tulostus not implemented!");

/* RS REM
    char name[LNAME];
    char *p;
    int i,k;
//    HANDLE hPrinter;
    char rivi[LLENGTH];

    dinfo1.pDocName="MyApp";
    dinfo1.pOutputFile=NULL;
    dinfo1.pDatatype="RAW";

    p=win_printer_name;
    if (*p=='"') ++p;
    strcpy(name,p);
    i=strlen(name)-1; if (name[i]=='"') name[i]=EOS;
    p=name;
//  while (strchr(p,'_')!=NULL) { p=strchr(p,'_'); *p=' '; } 30.3.2001
// printf("\nname=%s|",name); getch(); exit(0);
    i=OpenPrinter(name,&hPrinter,NULL);
    if (i==0)
        {
        PR_EIN2;
        sur_print("\n*********************************************");
        sprintf(rivi,"\nPrinter \"%s\" is not available!",name);
        sur_print(rivi);
        sur_print("\n*********************************************");
        WAIT; exit(0);
        }
// printf("\nOpenPrinter=%d|",i); getch();
    i=StartDocPrinter(hPrinter,1,(LPBYTE)&dinfo1);
// printf("\nStartDocPrinter=%d|",i); getch();
    i=StartPagePrinter(hPrinter);
// printf("\nStartPagePrinter=%d|",i); getch();
    kirjoitin=muste_fopen(laite,"rt");
    while(!feof(kirjoitin))
        {
        fgets(rivi,LLENGTH-1,kirjoitin);
        WritePrinter(hPrinter,rivi,strlen(rivi),&k);
        }

    muste_fclose(kirjoitin);
    EndPagePrinter(hPrinter);
    EndDocPrinter(hPrinter);
    ClosePrinter(hPrinter);
*/
    return(1);
    }



static int tulosta_rivit(int j1,int j2)
        {
        int i,j;
        char *p;
        char x[4*LLENGTH], xs[4*LLENGTH];

        for (j=j1; j<=j2; ++j)
            {
            erivi=0;
            switch (tila)
                {
              case 0: edread(x,j); break;
              case 1: uedread(x,j); break;
              case 2: i=textread(x,j); if (i<0) return(1); break;
                }
            if (!dev_luettu) { i=dev_file(x);
                               if (i<0) return(-1);
                               dev_luettu=1;
                             }
            p=x;

            if (*control_off)
                {
                if (strchr(control_off,*p)) *p='*';
                }
            if (*p=='-') /* Koodin mÑÑrittelyrivi */
                {
                i=lue_koodit(x+1); if (i<0) return(-1);
                continue;
                }
            if (*p=='!') line_count-=line_spacing;
                         /* !-rivejÑ ei mukaan rivilaskentaan */
            if (*p=='/') { i=uusi_sivu(); if (i<0) return(-1); }
            if (*p=='(') { i=loppusulku(j,j2); if (i<0) return(-1); }
            if (*p=='&' || *p=='%') { i=tyhjat_rivit(j); if (i<0) return(-1);
                           continue;
                         }
            if (*p=='T') { i=tabs(j); if (i<0) return(-1); continue; }
            if (*p=='C') erivi=1;
            if (*p=='R') erivi=2;
            if (*p=='r' && page_number!=((page_number>>1)<<1)) erivi=2;
                           /* -8.12.1992 oli == */

            if (*p=='V') pakkovenytys=1; else pakkovenytys=0;
            if (control[(unsigned char)(*p)]!=NULL)
                {
                testaa_sivu();  /* 28.2.88, muuten < antaa vÑÑrÑn paikan */
                strcpy(xs,control[(unsigned char)(*p)]);  /* xs tilap. */
                i=lue_koodit(xs); if (i<0) return(-1);
                }
            if (tila==0)
                {
                if (zs[j]==0) *xs=EOS;
                else          edread(xs,zs[j]);
                }
            else if (tila==1)
                {
                if (uzs[j]==0) *xs=EOS;
                else           uedread(xs,uzs[j]);
                }
            else *xs=EOS;
            replacement(x);

            if (gchar_on) { strcpy(tabrivi0,tabrivi); gchar_erotus(x); }
            if (pr_type==1)
              {
              i=read_footnotes(x,xs,j,j2); if (i==-1) return(-1);  /* 5.11.88 */
              if (i==-2) i=read_footnotes(x,xs,j,j2);  /* uusi sivu */
              if (i>0) j=i;  /* ohita alaviitteet */
              }
            if (printing_on) // 21.8.2008
              {
              if (ascii_text_saving && !ind_page_number) ascii_save(x+1);
              if ( pitch_unit && (line_length || *tabrivi) )
                  {
                  char varatab=*tabrivi;
                  if (*p=='U') { *tabrivi=EOS; i=tulosta(x,xs); *tabrivi=varatab; }
                  else         { if (gchar_on) gchar_tulosta(); i=ptulosta(x,xs); }
                  if (i<0) return(-1);
                  }
              else tulosta(x,xs);
              }
            if (gchar_on) strcpy(tabrivi,tabrivi0);
            }
        return(1);
        }

static int tulosta(unsigned char x[],unsigned char xs[])
        {
        int i,k,len,slen;
        unsigned char varjo;
        char *p;
        char y[LLENGTH], yy[LLENGTH];

        testaa_sivu();

        if (pr_type==1) { ps_tulosta(x,xs); return(1); }

        len=strlen(x);
        while(x[len-1]==' ') --len;
        if (ind_page_number) set_page_number(x);
        if (prind2 && !silent)
            {
            i=c3+8; if (len<i) i=len;  /* 26.5.92 */
            sprintf(sbuf,"\n%*.*s",i,i,x+1); sur_print(sbuf);
            }
        if (len<=1)
            {
            if (*xs==EOS) { uusi_rivi(); return(1); }
            len=strlen(xs); while (xs[len-1]==' ') --len;
            }
        if (!ind_page_number)
            { i=tekstityyppi(); if (i<0) return(-1); }

        if (*xs==EOS)
            {
            for (i=1; i<len; ++i) putc((int)code[x[i]],kirjoitin);
            uusi_rivi();
            return(1);
            }
        slen=strlen(xs);
        while(xs[slen-1]==' ') --slen;
        ++slen; /* jotta saadaan erikoismoodit pois rivin lopussa */
        if (slen>len) len=slen;
        x[len]=EOS; xs[len]=EOS;
        i=1;
        while (xs[i])
            {
            varjo=xs[i];
/********************************** poistettu 10.9.1999
            if (varjo=='R')
                {
                varjo=x[i];
                *y=varjo; *(y+1)=EOS;
                if (raster[varjo]!=NULL) strcpy(y,raster[varjo]);
                k=fontprint(y); if (k<0) return(-1);
                ++i; continue;
                }
*******************************************************/
            p=shadow[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            k=0;
            while (xs[i]==varjo)
                { putc((int)code[x[i]],kirjoitin); echo[k++]=code[x[i]]; ++i; }
            echo[k]=EOS;
            p=shadow2[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow2[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            }
        uusi_rivi();
        return(1);
        }

static int testaa_sivu()
        {
        if (line_count+line_spacing>page_length) return(uusi_sivu());
        return(1);
        }

static int uusi_sivu()
        {
        int i;
        char y[LLENGTH];

        if (odotus) /* 7.3.1992 */
            {
            sprintf(y,"\n- - - - - Free space at the end of the page is %g (Points) - - - - -",
                   page_length-line_count);
            }

        fn_tulosta(); /* 5.11.88 */

        if (odotus) sur_print(y);  /* 7.3.1992 */

        strcpy(y,"[NEW_PAGE]");
        i=lue_koodit(y); if (i<0) return(-1);
        line_count=0.0;

        header_print();
        ++sivulaskin;

        if (odotus)
            {
            i=sur_getch(); if (i=='-') odotus=0;
            }
        testaa_plus();

        return(1);
        }

static void testaa_plus()
        {
        int i;

        if (sur_kbhit())
            {
            i=sur_getch();
            if (i=='+') odotus=1;
            }
        }

static int uusi_rivi()
        {
        int i;
        char y[LLENGTH], yy[LLENGTH], *p;
        extern int line_continues;

        if (line_continues) { line_continues=0; return(1); }
        strcpy(y,shadow[' ']);
        muunna(y,yy);
        p=yy;
        while (*p) { putc((int)(*p),kirjoitin); ++p; }

        strcpy(y,"[NEL]");
        i=lue_koodit(y); if (i<0) return(-1);

        if (!count_off && prind) line_count+=line_spacing;
        return(1);
        }

static int dev_file(char *x)
        {
        int i;
        char y[LLENGTH], *sana[2];
        char *p;

        if (*x!='-') { i=load_dev(); return(i); }
        strcpy(y,x);
        i=split(y+1,sana,2);
        if (i<2) { i=load_dev(); return(i); }
        if (strcmp(sana[0],"include")!=0) { i=load_dev(); return(i); }
        p=strchr(sana[1],'.'); if (p==NULL) { i=load_dev(); return(i); }
        if (muste_strcmpi(p+1,"dev")!=0) { i=load_dev(); return(i); }
        return(1);
        }

static int load_dev()
        {
        int i;
        char nimi[32];
        char x[128];

        i=hae_apu("print_dev",nimi);
        if (i==0)
            {
            sur_print("\nPrint driver not defined in SURVO.APU!");
            WAIT; return(-1);
            }
        strcpy (x,"include "); strcat(x,nimi);
        i=lue_koodit(x);
        return(i);
        }

static int pr_empty(char *s)
        {
        while (*s) { if (*s!=' ') return(0); ++s; }
        return(1);
        }

static void vdc(int arvo,char *luku)
/* koodaa kokonaisluvun 'arvo' 1-3 tavun VDC-koodisanaksi 'luku' */
        {
        char merkki;
        int m;

        if (arvo<0) { merkki=0; arvo=-arvo; } else merkki=16;
        if (arvo<16)
            {
            *luku=(char)(arvo|merkki|32);
            luku[1]=EOS; return;
            }
        if (arvo<1024)
            {
            *luku=(char)( (arvo>>4)|64 );
            luku[1]=(char)( (arvo&15)|32|merkki );
            luku[2]=EOS; return;
            }
        /* arvo>=1024 */
        luku[2]=(char)( (arvo&15)|32|merkki );
        m=arvo>>4;
        luku[1]=(char)( (m&63)|64 );
        luku[0]=(char)( (m>>6)|64 );
        luku[3]=EOS;
        }

static void binopen()
        {
        muste_fclose(kirjoitin);
        kirjoitin=muste_fopen(laite,"wb");
        }

static int alkukoodit()
        {
        int i;

        i=varaa_tilat();
        if (i<0) return(-1); // RS CHA exit(1);
        *pr_tila=EOS;
        pr_osoitin=pr_tila;
        n_sana=0;
        for (i=0; i<256; ++i) code[i]=(unsigned char)i;
        line_count=0.0;
        hline2=0; /* ei otsikkorivejÑ */
        page_number=1; ind_page_number=0;
        dev_luettu=0;
        tila=0;
        *tabrivi=EOS;
        prind=1;
        ps_init();
        init_gchars();
        return(1);
        }

static int varaa_tilat()
        {
        int i;
        char s[LLENGTH];

        kooditila=KOODITILA;
        koodisanat=KOODISANAT;
        i=hae_apu("printdef",s);
        if (i) { kooditila=atoi(s); koodisanat=kooditila/20; }
        pr_tila=muste_malloc(kooditila);
        if (pr_tila==NULL) { not_enough_memory(); return(-1); }
        pr_sana=(char **)muste_malloc(koodisanat*sizeof(char *));
        if (pr_sana==NULL) { not_enough_memory(); return(-1); }
        pr_koodi=(char **)muste_malloc(koodisanat*sizeof(char *));
        if (pr_koodi==NULL) { not_enough_memory(); return(-1); }
        len_pr_koodi=(int *)muste_malloc(koodisanat*sizeof(int));
        if (len_pr_koodi==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static void not_enough_memory()
        {
        sur_print("\nNot enough memory!"); WAIT;
        }

static int lue_koodit(char *x)
        {
        char *sana[32];
        int i,n;
        char x1[LLENGTH];
        char y[NLEN*LLENGTH];
        char *p;
        char ch;
// printf("\n%s|",x); getch();
        p=x;
        while ( (p=strchr(p,TAB))!=NULL ) { *p=' '; ++p; }
        p=x;
        while( (p=strchr(p,'/'))!=NULL )
            {
            if ( (*(p-1)==' ') &&
                 (*(p+1)==' ' || *(p+1)==EOS) )
            { *p=EOS; break; }
            ++p;
            }
        strncpy(x1,x,LLENGTH);
        n=space_split(x1,sana,16);
if (n)  {
        if (printing_on==0) // 12.1.2011
            {
       //   printf("\nsana[0]=%s",sana[0]); getch();
            if (muste_strcmpi(sana[0],"[PRINT_ON]")==0) printing_on=1;
            return(1);
            }

        if (strcmp(sana[0],"-")==0) return(1); // 15.12.2001
        if (muste_strcmpi(sana[0],"DEFINE")==0) { i=define(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"SHADOW")==0) { i=shadows(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"DEF_SPACE")==0) { i=def_space(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"CODES")==0)  { i=codes(x1,sana,n); return(i); }
        if (muste_strcmpi(sana[0],"INCLUDE")==0)  { i=include(x1,sana,n); return(i); }
        if (muste_strcmpi(sana[0],"HEADER_LINES")==0) { i=hlines(x); return(i); }
        if (muste_strcmpi(sana[0],"CHAPTER")==0) { i=chapter(x); return(i); }
        if (muste_strcmpi(sana[0],"PICTURE")==0) { i=picture(x); return(i); }
        if (muste_strcmpi(sana[0],"TEXT")==0) { i=textfile(x); return(i); }
        if (muste_strcmpi(sana[0],"DOS")==0) { i=dos(x); return(i); }
        if (muste_strcmpi(sana[0],"CONTROL")==0) { i=controls(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"NULL")==0) { i=null_ch(n,sana[1]); return(i); }
        if (muste_strcmpi(sana[0],"REPLACE")==0) { i=replace(x); return(i); }
/*****************
        if (muste_strcmpi(sana[0],"FONT")==0 || muste_strcmpi(sana[0],"RASTER")==0)
                                          { i=rasters(x1,sana,n,x); return(i); }
******************/
        if (muste_strcmpi(sana[0],"BINMODE")==0) { binopen(); return(1); }
        if (muste_strcmpi(sana[0],"TYPE")==0) { pr_type=atoi(sana[1]); return(1); }
        if (muste_strcmpi(sana[0],"PS_CODE")==0) { i=ps_code(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"INDEX")==0) { i=print_index(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"GCHAR")==0) { i=gchars(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"AUTOCAD")==0) { i=picture(x); return(i); }
        if (muste_strcmpi(sana[0],"FILE")==0) { i=picture(x); return(i); }
        if (muste_strcmpi(sana[0],"EPSFILE")==0) { i=picture(x); return(i); }
        if (muste_strcmpi(sana[0],"ASCII_TEXT")==0) { i=ascii_text(x); return(i); }
        if (muste_strcmpi(sana[0],"CONTROL_OFF")==0)
            {
            if (n<2) *control_off=EOS; else strcpy(control_off,sana[1]);
            return(1);
            }
        if (muste_strcmpi(sana[0],"LIST")==0) { i=pr_list(x); return(i); }
// 22.1.2007
        if (muste_strcmpi(sana[0],"SILENT")==0) { silent=1; return(1); }
        }

        p=x;
        i=strlen(x); while(x[i-1]==' ' && i>1) x[--i]=EOS;
        i=0; while (x[i]==' ') ++i;
        if (empty(x+i)) return(1);
// printf("\nx+i=%s",x+i);
        i=muunna(x+i,y); if (i<0) return(-1);
// printf("\ni=%d y=%s|",i,y);
        kirjoita(y);
/*
printf("\nkontr:"); for (i=0; i<strlen(y); ++i) printf("%c",y[i]); getch();
*/
        return(1);
        }

static void kirjoita(char *y)
        {
        int i;
        char ch;

        for (i=0; i<strlen(y); ++i)
            {
            ch=y[i];  if ((unsigned char)ch==null_char) ch=0;
            putc((int)ch,kirjoitin);
            }
        }


static int define(char *x,char **sana,int n,char *rivi)
        {
        int i,k,vanha;

        if (n!=3) { koodivirhe(rivi); return(-1); }

        i=strlen(sana[1]);
        if ( sana[1][0]!='[' || sana[1][i-1]!=']' )
            {
            PR_EBLD;
            sprintf(sbuf,"\nBrackets [] missing in %s",sana[1]);
            sur_print(sbuf); WAIT; PR_ENRM; return(-1);
            }
        sana[1][i-1]=EOS; ++sana[1];
        vanha=1; i=0; while (i<n_sana)
            {
            if (muste_strcmpi(sana[1],pr_sana[i])==0) break;
            ++i;
            }

        if (i==n_sana)
            {
            ++n_sana;
            pr_test(strlen(sana[1]));
            pr_sana[i]=pr_osoitin; strcpy(pr_osoitin,sana[1]);
            pr_osoitin+=strlen(sana[1])+1;
            vanha=0;
            }

        if (vanha && len_pr_koodi[i]>=strlen(sana[2]))
            {
            strcpy(pr_koodi[i],sana[2]);
            }
        else
            {
            len_pr_koodi[i]=strlen(sana[2]);
            pr_test(len_pr_koodi[i]);
            pr_koodi[i]=pr_osoitin; strcpy(pr_osoitin,sana[2]);
            pr_osoitin+=len_pr_koodi[i]+1;
            }
        return(1);
        }

static int shadows(char *x,char **sana,int n,char *rivi)   /* shadow <koodi> <alkukoodisana> <loppukoodisana> */
        {
        int i,k;
        unsigned char varjo;
        char y[LLENGTH];

        if (n==2 && muste_strcmpi(sana[1],"list")==0)
            {
            strcpy(y,etmpd); strcat(y,"SHADOWS.TXT");
            shadows_in_use=muste_fopen(y,"wt");
            if (shadows_in_use==NULL) return(-1);
            sprintf(sbuf,"\nList of shadow characters in use in %s:\n",y);
            sur_print(sbuf); 
            fprintf(shadows_in_use,"\nList of shadow characters in use in %s:\n",y); // RS CHA fprintf(shadows_in_use,sbuf);

            if (pr_type==1)
                {
                sh_list_print((unsigned char)'[');
                sh_list_print((unsigned char)']');
                sh_list_print((unsigned char)'*');
                }
            for (i=0; i<256; ++i)
                {
                if (shadow[i]!=NULL || shadow2[i]!=NULL)
                    sh_list_print((unsigned char)i);
                }
            sh_list_print((unsigned char)'\n');
            muste_fclose(shadows_in_use);
            WAIT; return(1);
            }

        if (n<3) { koodivirhe(rivi); return(-1); }

        i=strlen(sana[1]);
        if (i==1) varjo=sana[1][0];
        else { i=muunna(sana[1],y); if (i<0) return(-1); varjo=*y; }

        if (shadow[varjo]!=NULL && len_shadow[varjo]>=strlen(sana[2]))
            strcpy(shadow[varjo],sana[2]);
        else
            {
            len_shadow[varjo]=strlen(sana[2]);
            pr_test(len_shadow[varjo]);
            shadow[varjo]=pr_osoitin; strcpy(pr_osoitin,sana[2]);
            pr_osoitin+=len_shadow[varjo]+1;
            }
        if (n<4) shadow2[varjo]=NULL;
        else
            {
            if (shadow2[varjo]!=NULL && len_shadow2[varjo]>=strlen(sana[3]))
                strcpy(shadow2[varjo],sana[3]);
            else
                {
                len_shadow2[varjo]=strlen(sana[3]);
                pr_test(len_shadow2[varjo]);
                shadow2[varjo]=pr_osoitin; strcpy(pr_osoitin,sana[3]);
                pr_osoitin+=len_shadow2[varjo]+1;
                }
            }
/*  printf("\nshadow: %d %s %s",varjo,shadow[varjo],shadow2[varjo]); getch();
*/
        return(1);
        }

static void sh_list_print(unsigned char ch)
        {
        sprintf(sbuf,"%c",ch);
        sur_print(sbuf); 
        fprintf(shadows_in_use,"%c",ch); // RS CHA fprintf(shadows_in_use,sbuf);
        }

static int def_space(char *x,char **sana,int n,char *rivi) /* 16.10.1996 */
        {
        if (n>1) defspace=*sana[1];
        else defspace=EOS;
        return(1);
        }

static int controls(char *x,char **sana,int n,char *rivi)   /* control <koodi> <koodisana> */
        {
        int i,k;
        unsigned char varjo;    /* tÑssÑ kontrollimerkki */
        char y[LLENGTH];
        char *p,*q;

        if (*sana[2]=='{')
            {
// printf("\nrivi=%s|",rivi); getch();
            p=strchr(rivi,'{');
            q=strchr(p,'}');
            if (q!=NULL) *q=EOS;
            sana[2]=p+1;
            }


        if (n<3) { koodivirhe(rivi); return(-1); }

        i=strlen(sana[1]);
        if (i==1) varjo=sana[1][0];
        else { i=muunna(sana[1],y); if (i<0) return(-1); varjo=*y; }
        if (control[varjo]!=NULL && len_control[varjo]>=strlen(sana[2]))
            strcpy(control[varjo],sana[2]);
        else
            {
            len_control[varjo]=strlen(sana[2]);
            pr_test(len_control[varjo]);
            control[varjo]=pr_osoitin; strcpy(pr_osoitin,sana[2]);
            pr_osoitin+=len_control[varjo]+1;
            }
        return(1);
        }

static int codes(char *x,char **sana,int n)   /* codes <kooditiedosto> */
        {
        load_codes(sana[1],code);
        return(1);
        }

static int muunna(char *sana,char *muunnos)
        {
        unsigned char koodi;
        char luku_koodi[4];  /* Canon VDC */
        char *s,*p,*q,*y;
        int i;
//      char x[NLEN*LLENGTH];   19.8.2008
//      char z[NLEN*LLENGTH];

        char x[NLEN*1000];    // 19.8.2008
        char z[NLEN*1000];

        s=sana;
        y=muunnos;
        while (*s)
            {
            if (*s=='[')
                {
                p=strchr(s,']');
                if (p==NULL) { koodivirhe(sana); return(-1); }
                *p=EOS;
                i=0;
/****************************** poistettu 10.9.1999
                if (strncmp(s+1,"F:",2)==0)
                    {
                    *y=EOS; kirjoita(muunnos); y=muunnos;
                    i=fontprint(s+3); if (i<0) return(-1);
                    s=p+1; continue;
                    }
*****************************************************/
                if (strchr(s+1,'(')!=NULL)
                    {
                    i=makro(s+1,x); if (i<0) return(-1);
                    i=muunna(x,z);  if (i<0) return(-1);
                    q=z;
                    while (*q) { *y=*q; ++y; ++q; }
                    s=p+1;
                    continue;
                    }

                while (i<n_sana)
                    {
                 /* if (strcmp(s+1,pr_sana[i])==0) break; */
                    if (muste_strcmpi(s+1,pr_sana[i])==0) break;
                    ++i;
                    }
                if (i<n_sana)
                    {
                    strcpy(x,pr_koodi[i]);
                    i=muunna(x,z); if (i<0) return(-1);
                    q=z;
                    while (*q) { *y=*q; ++y; ++q; }
                    s=p+1;
                    continue;
                    }

                if ((q=strchr(s,'/'))!=NULL && q<p)   /* hex  [a/b]  */
                    {
                    *q=EOS; *p=EOS;
                    *y=(unsigned char)(16*atoi(s+1)+atoi(q+1));
                    ++y;
                    s=p+1;
                    continue;
                    }

                if (*(s+1)=='B')  /* binary number [Bn] */
                    {
                    *y=(unsigned char)atoi(s+2);
                    ++y;
                    s=p+1;
                    continue;
                    }

                if (*(s+1)=='N')  /* Canon VDC integer [Nn] */
                    {
                    vdc(atoi(s+2),x);
                    for (i=0; i<strlen(x); ++i, ++y) *y=x[i];
                    s=p+1;
                    continue;
                    }

                if (*(s+1)=='E')  /* ECHO */
                    {
                    for (i=0; i<strlen(echo); ++i, ++y) *y=echo[i];
                    s=p+1;
                    continue;
                    }

                if (*(s+1)=='%') /* special information for printing */
                    {
                    i=p_special(s+2); if (i<0) return(-1);
                    s=p+1;
                    continue;
                    }

                sprintf(sbuf,"\n%s] is unknown!",s); sur_print(sbuf);
                WAIT; return(-1); // 6.2.2001 ennen return(-1); RS CHA exit(0)
                s=p+1;
                continue;
                }
            *y=*s; ++y; ++s;
            }  /* while (*s) */
        *y=EOS;

        return(1);
        }

static void koodivirhe(char *x)
        {
        PR_EBLD;
        sprintf(sbuf,"\nErroneous code line/word:\n%s",x);
        sur_print(sbuf); WAIT; PR_ENRM;
        }


static int null_ch(int n,char *s)
        {
        int i;
        char x[LLENGTH];
        char y[LLENGTH];

        if (n==0) return(1);
        strcpy(x,s);
        i=muunna(x,y); if (i<0) return(-1);
        null_char=*y;
        return(1);
        }

static int pr_test(int k)
        {
        if ((int)(pr_osoitin-pr_tila)+k+2<kooditila) return(1);
        sur_print("\nNot enough space for keywords/codes!");
        WAIT; return(-1); // RS CHA FIXME exit(1);
        }



static void load_codes(char *codefile,unsigned char *code)
        {
        static FILE *codes;
        int i;
        char x[LLENGTH];

/*  printf("\ncodefile=%s",codefile);
*/
        strcpy(x,codefile);
        if (strchr(x,':')==NULL && *x!='.' && *x!='~' && *x!='/' && *x!='\\') // RS ADD unix path FIXME
            { strcpy(x,survo_path); strcat(x,"SYS/"); strcat(x,codefile); }
               /* 16.10.1996 */
        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            for (i=0; i<256; ++i) code[i]=(unsigned char)i;
            return;
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
/*  printf("\Haettu!"); getch(); */
        muste_fclose(codes);
        }

//static int space_split(char rivi[],char *sana[],int max)
static int space_split(char *rivi,char **sana,int max)
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Vain vÑlilyînnit toimivat erottimina.
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
        {
        int g=0;
        int p;
        int edell=0; /* vÑli edellÑ */
        int len=strlen(rivi);

 	for (p=0; p<max; p++) sana[p]=muste_nullstring_print; // RS ADD


        for (p=0; p<len; ++p)
                {
                if (rivi[p]==' ')
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
      /* 16.10.1996 */  if (defspace && g>0 && rivi[p]==defspace) rivi[p]=' ';
                        if (edell==0)
                                {
                                sana[g]=rivi+p;
                                edell=1;
                                }
                        }
                }
        if (edell==1) ++g;

        return(g);
        }


static int makro(char *sana,char *muunnos)
        {
        char *s, *y;
        int i,len;
        char *parm[10]; int nparm;
        char *sparm[10]; int nsparm;
        char prsana[LLENGTH];
        char *p;
        char varasana[LLENGTH];

        strcpy(varasana,sana);
        s=strchr(sana,'('); *s=EOS; ++s;
        strcpy(prsana,sana); strcat(prsana,"(");
        y=muunnos;
        len=strlen(s);
        if (s[len-1]!=')') { koodivirhe(s); return(-1); }
        s[len-1]=EOS;
        nparm=split(s,parm,10);
        len=strlen(prsana);
        i=0; while(i<n_sana)
            {
         /* if (strncmp(prsana,pr_sana[i],len)==0) break; */
            if (muste_strnicmp(prsana,pr_sana[i],len)==0) break;
            ++i;
            }
        if (i==n_sana) { koodivirhe(varasana); return(-1); }
        strcpy(prsana,pr_sana[i]);
        strcpy(muunnos,pr_koodi[i]);
        p=strchr(prsana,'('); if (p==NULL) { koodivirhe(varasana); return(-1); }
        len=strlen(p);
        if (p[len-1]!=')') { koodivirhe(varasana); return(-1); }
        p[len-1]=EOS; ++p;
        nsparm=split(p,sparm,10);
        if (nsparm!=nparm)
            {
            PR_EBLD;
            sprintf(sbuf,"\nIncorrect number of parameters in %s",varasana);
            sur_print(sbuf); WAIT; return(-1);
            }
        for (i=0; i<nparm; ++i) korvaa(muunnos,sparm[i],parm[i]);
        return(1);
        }

static void korvaa(char *muunnos,char *s,char *t)
        {
        char *p, *q;
        char x[LLENGTH];

        p=muunnos; *x=EOS;
        while( (q=strchr(p,*s))!=NULL )
            {
            if (strncmp(q,s,strlen(s))==0)
                {
                strncat(x,p,q-p); strcat(x,t);
                p=q+strlen(s);
                }
            else
                {
                strncat(x,p,q-p+1);
                p=q+1;
                }
            }
        strcat(x,p); strcpy(muunnos,x);
        }

static int dos(char *x)
        {
        char y[LLENGTH];
        char *osa[2];
        int i,len;

        strcpy(y,x);
        i=split(y,osa,2);
        if (i<2) return(1);
        i=osa[1]-y;
        len=strlen(x);
        while (x[len-1]==' ') x[--len]=EOS;
        muste_system(x+i,TRUE); // RS CHA
        return(1);
        }




static int include(char *x,char **sana,int n)
        {
        char rivi[LLENGTH];
        int i,len;
        FILE *ifile; /* lokaalinen, koska kÑyttî rekursiivista */

        strcpy(rivi,sana[1]);
        if (strchr(rivi,':')==NULL && *rivi!='.' && *rivi!='~' && *rivi!='/' && *rivi!='\\') // RS CHA unix path FIXME
            { strcpy(rivi,edisk); strcat(rivi,sana[1]); }
        ifile=muste_fopen(rivi,"rt");
        if (ifile==NULL)
            {
            strcpy(rivi,sana[1]);
            if (strchr(rivi,':')==NULL && *rivi!='.' && *rivi!='~' && *rivi!='/' && *rivi!='\\') // RS CHA unix path FIXME
                { strcpy(rivi,survo_path); strcat(rivi,"SYS/"); strcat(rivi,sana[1]); }
            ifile=muste_fopen(rivi,"rt");
            if (ifile==NULL)
                {
                sprintf(sbuf,"\nInclude file %s not found!",sana[1]);
                sur_print(sbuf); WAIT; return(-1);
                }
            }

// TÑhÑn silent ei tehoa, koska se luetaan vasta tÑmÑn jÑlkeen
        sur_print("\n******************************");
        sprintf(sbuf,"\ninclude %s",rivi); sur_print(sbuf);
        sur_print("\n******************************");


        while (1)
            {
            fgets(rivi,LLENGTH,ifile);
            if (feof(ifile)) break;
            len=strlen(rivi); rivi[len-1]=EOS;
            if (rivi[len-2]=='\r') rivi[len-2]=EOS; // RS ADD 
            i=lue_koodit(rivi);
            if (i<0) { muste_fclose(ifile); return(-1); }
            }

        muste_fclose(ifile);
        return(1);
        }


static int hlines(char *x)     /* header_lines */
        {
        int i;
        char *sana[5];

        i=split(x,sana,5);
        hline1=edline2(sana[1],1); if (hline1==0) return(-1);
        hline2=edline2(sana[2],hline1); if (hline2==0) return(-1);
        hline3=0;
        if (i==5)
            {
            hline3=edline2(sana[3],1); if (hline3==0) return(-1);
            hline4=edline2(sana[4],hline3); if (hline4==0) return(-1);
            }

        if (line_count<=0.0) header_print(); /* ensimmÑistÑ sivua varten */
        return(1);
        }

static int header_print()
        {
        int i;
        int perustila;
        char varatab;

        if (hline2==0) return(1);
        ind_page_number=1;
        perustila=tila;
        tila=0;  /* otsikkorivit kentÑstÑ */

    strcpy(header_varatabrivi,tabrivi);
        varatab=*tabrivi; *tabrivi=EOS;
        if (hline3 && page_number==((page_number>>1)<<1))
            i=tulosta_rivit(hline3,hline4);
        else
            i=tulosta_rivit(hline1,hline2);

        *tabrivi=varatab;
    strcpy(tabrivi,header_varatabrivi);

        tila=perustila;
        ind_page_number=0;
        ++page_number;

        return(i);
        }

static void set_page_number(char *x)
        {
        char *p;
        char nro[10];
        int len,i;

        p=x;
        while ((p=strchr(p,'#'))!=NULL)
            {
            while (*p=='#') {*p=' '; ++p; }
            muste_itoa(page_number,nro,10);
            len=strlen(nro);
            for (i=len-1; i>=0; --i) { --p; *p=nro[i]; }
            }
        p=x;
        while ((p=strchr(p,'@'))!=NULL)
            {
            while (*p=='@') {*p=' '; ++p; }
            roman(page_number,nro);
            len=strlen(nro);
            for (i=len-1; i>=0; --i) { --p; *p=nro[i]; }
            }
        }

static int loppusulku(int j,int j2)
        {
        int i,n;
        char x[LLENGTH];
        char *sana[1];
        double test_count=line_count;

        while (j<=j2)
            {
            if (test_count+line_spacing>page_length)
                {
                i=uusi_sivu();
                return(i);
                }
            if (tila==0) edread(x,j); else uedread(x,j);
            if (*x=='-') { ++j; continue; }  /* lisÑtty 17.5.1987/SM */
            if (*x==')' || *x=='/') return(1);
            if (*x=='<')
                {
                while (j<=j2)
                    {
                    if (tila==0) edread(x,j); else uedread(x,j);
                    if (*x=='>') break;
                    ++j; continue;
                    }
                continue;
                }
            if (*x=='&')
                {
                i=split(x+1,sana,1);
                n=atoi(sana[0]);
                if (n<=0)
                    {
                    sprintf(sbuf,"\nIncorrect & line %d",j); sur_print(sbuf);
                    WAIT; return(-1);
                    }
                test_count+=n*line_spacing;
                ++j; continue;
                }
            if (*x=='%')                     /* 28.6.1992 */
                {
                i=split(x+1,sana,1);
                test_count+=pistekerroin*atof(sana[0]);
                ++j; continue;
                }
            else
                {
                test_count+=line_spacing;
                ++j; continue;
                }
            }
        return(1);
        }

static int tyhjat_rivit(int j)
        {
        int i;
        char x[LLENGTH];
        double test_count=line_count;
        char *sana[1];
        char kontr;
        double dn,a;
        double linsp;

		a=pistekerroin; // RS ADD
        if (tila==0) edread(x,j); else uedread(x,j);
        kontr=*x;
        i=split(x+1,sana,1);
        dn=atof(sana[0]);
        if (dn<=0)
            {
            sprintf(sbuf,"\nIncorrect & or %% line %d for empty lines",j); sur_print(sbuf);
            WAIT; return(-1);
            }
        if (kontr=='&')
            test_count+=dn*line_spacing;
        else if (kontr=='%')
            {
            a=pistekerroin*dn;
            test_count+=a;
            }
        if (test_count>page_length) { i=uusi_sivu(); if (i<0) return(-1); }

        if (kontr=='&')
            for (i=0; i<(int)dn; ++i) uusi_rivi();
        else if (kontr=='%')
            {
            linsp=line_spacing;
            sprintf(x,"[line_spacing(%g)]",a);
            lue_koodit(x);
            uusi_rivi();
            sprintf(x,"[line_spacing(%g)]",linsp);
            lue_koodit(x);
            }
        return(1);
        }

static int tekstityyppi()
        {
        int i;
        char x[LLENGTH];

        strcpy(x,"[TEXT]");
        i=lue_koodit(x);
        if (i<0) return(-1);
        return(1);
        }




static int p_special(char *s) /* tulkkaa laitetiedoston %-sanat */
        {
        int i;
        char *p;
        char x[LLENGTH];
        static double parm[12];
        static int ip=0;

        strcpy(x,s);

        p=strchr(x,'=');
        if (p==NULL)
            {
            sprintf(sbuf,"\nError in %% code %s",x); sur_print(sbuf);
            WAIT; return(-1);
            }
        *p=EOS;
        ++p;

        if (strcmp(x,"printing_on")==0)         // 21.8.2008 12.1.2011
            { printing_on=atoi(p); return(1); }
        if (printing_on==0) return(1); // 12.1.2011

        if (strcmp(x,"char_pitch")==0)
            { char_pitch=atof(p); p_charsize(); return(1); }
        if (strcmp(x,"char_width")==0)
            { char_width=atof(p); p_charsize(); return(1); }
        if (strcmp(x,"char_height")==0)
            { char_height=atof(p); p_charsize(); return(1); }
        if (strcmp(x,"line_length")==0)
            { line_length=atof(p); if (line_length>0.0) pitch_load(); return(1); }
        if (strcmp(x,"pitch_ind")==0)
            { pitch_ind=256*(atoi(p)-1);
              if (pitch_unit) pitch_unit=pitch[pitch_ind];   /* 28.11.88 */
              return(1);
            }
        if (strcmp(x,"line_spacing")==0)
            { line_spacing=atof(p); return(1); }
        if (strcmp(x,"page_length")==0)
            { page_length=atof(p); return(1); }
        if (strcmp(x,"page_number")==0)
            { page_number=atoi(p); return(1); }
        if (strcmp(x,"line_extra")==0)
            { if (!count_off && prind) line_count+=atof(p)*line_spacing; return(1); }
        if (strcmp(x,"count_off")==0)
            { count_off=atoi(p); return(1); }
        if (strcmp(x,"line_count")==0)
            { line_count=atof(p); return(1); }
        if (strcmp(x,"font_size")==0)
            { font_size=atof(p); return(1); }
        if (strcmp(x,"vstrike")==0)
            { uusi_vstrike(atof(p)); return(1); }
        if (strcmp(x,"hstrike")==0)
            { uusi_hstrike(atof(p)); return(1); }
        if (strcmp(x,"line_continues")==0)
            { line_continues=atoi(p); return(1); }
        if (strcmp(x,"footnote_number")==0)
            { set_footnote_number(p); return(1); }
        if (strcmp(x,"gchar_on")==0)
            { gchar_on=atoi(p); return(1); }
        if (strcmp(x,"norm_tol")==0)
            { end_norm_tol=atof(p); return(1); }
        if (strcmp(x,"spec_tol")==0)
            { end_spec_tol=atof(p); return(1); }

        if (strcmp(x,"pstatus")==0)
            {
            i=atoi(p);
            if (i==1)
                {
                parm[0]=char_pitch; parm[1]=char_width; parm[2]=char_height;
                parm[3]=line_length; parm[4]=pitch_ind; parm[5]=line_spacing;
                parm[6]=page_length; parm[7]=page_number; parm[8]=count_off;
                parm[9]=font_size;
                parm[10]=vstrike; parm[11]=hstrike;
                ip=1;
                }
            else if (ip==1)
                {
                char_pitch=parm[0]; char_width=parm[1]; char_height=parm[2];
                line_length=parm[3]; pitch_ind=parm[4]; line_spacing=parm[5];
                page_length=parm[6]; page_number=parm[7]; count_off=parm[8];
                font_size=parm[9];

                uusi_vstrike(parm[10]); uusi_hstrike(parm[11]);
       /*       vstrike=parm[10]; hstrike=parm[11];   -26.5.91 */
                }
            }

        if (strcmp(x,"euro_shadow")==0)
            { euro_shadow=*p; return(1); } /* 10.9.1999 */

/*      printf("\nUnknown %% code %s",s);       */

        return(1);
        }


static int pitch_load()
        {
        int i;
        char x[LLENGTH];
        static FILE *pfile;

        if (pitch_luettu) return(1);
        strcpy(x,survo_path); strcat(x,"SYS/PITCH.BIN"); /* 30.12.91 */
        pfile=muste_fopen(x,"rb");
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

        pitch_unit=pitch[0]; pitch_luettu=1;
        return(1);
        }

static int strpitch(unsigned char *s)
        {
        int len;

        len=0; while (*s) { len+=pitch[*s+pitch_ind]; ++s; }

        return(len);
        }

static void p_charsize()
        {
        double a;

        a=25400.0/char_pitch;
        kirjainlev=a*char_width;
        kirjainkork=a*char_height;
        }

static int ptulosta(unsigned char *x,unsigned char *xs)
        {
        int i,k,len,slen;
        unsigned char varjo;
        char *p;
        char y[LLENGTH], yy[LLENGTH];

        testaa_sivu();

        if (pr_type==1) { ps_ptulosta(x,xs); return(1); }

        len=strlen(x);
        while(x[len-1]==' ') --len;
        if (ind_page_number) set_page_number(x);
        sprintf(sbuf,"\n%.*s",len-1,x+1); if (!silent) sur_print(sbuf);
        if (*xs==EOS)
            {
            x[len]=EOS;
            }
        else
            {
            slen=strlen(xs);
            while(xs[slen-1]==' ') --slen;
            if (slen>len) len=slen;
            x[len]=EOS; xs[len]=EOS;
            }
        if (len<=1) { uusi_rivi(); return(1); }
/*  printf("\n x=%s",x+1);
    printf("\nxs=%s",xs);
    printf("\nstrpitch=%d",strpitch(x+1)); getch();
*/
        if (!ind_page_number)
            { i=tekstityyppi(); if (i<0) return(-1); }

        if (*tabrivi)
            { i=tabvenytys(x,xs,gap); if (i<0) return(-1); }
        else
            { i=venytys(x,xs,gap); if (i<0) return(-1); }

        if (*xs==EOS)
            {
            for (i=1; i<len; ++i)
                {
                if (gap[i]) p_gap(code[x[i]],gap[i]);
                else putc((int)code[x[i]],kirjoitin);
                }
            uusi_rivi();
            return(1);
            }

        i=1;
        while (xs[i])
            {
            varjo=xs[i];
/***************************************** 10.9.1999
            if (varjo=='R')
                {
                varjo=x[i];
                *y=varjo; *(y+1)=EOS;
                if (raster[varjo]!=NULL) strcpy(y,raster[varjo]);
                k=fontprint(y); if (k<0) return(-1);
                ++i; continue;
                }
**************************************************/
            p=shadow[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            k=0;
            while (xs[i]==varjo)
                {
                if (gap[i]) p_gap(code[x[i]],gap[i]);
                else putc((int)code[x[i]],kirjoitin);
                echo[k++]=code[x[i]];
                ++i;
                }
            echo[k]=EOS;
            p=shadow2[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow2[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            }

        uusi_rivi();
        return(1);
        }

static int venytys(char *x,char *xs,int gap[])
/* int gap[];   venytysarvot positioittain */
        {
        int i,len,pitchlen,k,h;
        char gappos[LLENGTH]; /* mahdolliset venytyskohdat=1 */
        int npos; /* venytyskohtien lkm */
        int pitchsum, vajaus;

        len=strlen(x+1);
        pitchlen=strpitch(x+1);   /* pitchlen=strpitch2(x,xs,gappos,&npos);
                          puuttuu vaihtuvien vÑlien ja kokojen mukaisesti */

        pitchsum=line_length*pitch_unit;

        /* vÑliaikaisesti */
        for (i=0; i<len; ++i) gappos[i]=0;
        i=1; npos=0;
        while (x[i]==' ') ++i;
        while (x[i]) { if (x[i]==' ') { ++npos; gappos[i]=1; }
                       ++i;
                     }

        for (i=0; i<len; ++i) gap[i]=0;
/*  printf("\npitchsum=%d pitchlen=%d npos=%d",pitchsum,pitchlen,npos);
    getch();
*/
        if (npos==0) return(1);
        vajaus=pitchsum-pitchlen;
        if (vajaus==0) return(1);
     /* if (vajaus<0)  return(1);       */
  /*
            {
            PR_EBLD;
            printf("\nLine: %.50s... too long!",x+1);
            WAIT; PR_ENRM; return(-1);
            }
  */

        if (strchr(".,:!",x[len])!=NULL &&         /* len=strlen(x+1) */
               vajaus>2*pitch_unit ) return(1);
        else
        if (vajaus>6*pitch_unit) return(1);
/* printf("\nvajaus=%d",vajaus); */
        if (vajaus>0)
            {
            k=(double)vajaus/(double)npos;
            if (k>0)
                for (i=1; i<len; ++i) { if (gappos[i]) gap[i]=k; }
            k=vajaus-k*npos;
            i=1; h=0;
            while (i<len && h<k) { if (gappos[i]) { ++h; ++gap[i]; } ++i; }
            }
        else
            {
            vajaus=-vajaus;
            k=(double)vajaus/(double)npos;
            if (k>0)
                for (i=1; i<len; ++i) { if (gappos[i]) gap[i]=-k; }
            k=vajaus-k*npos;
            i=1; h=0;
            while (i<len && h<k) { if (gappos[i]) { ++h; --gap[i]; } ++i; }
            }
/*
printf("\ngap:"); for (i=0; i<len; ++i) if (gappos[i]) printf(" %d",gap[i]);
getch();
*/
        return(1);
        }

static void p_gap(char merkki,int gap)
        {
        int i;
        unsigned char koodi[32];
        char luku[8];
        int skaala=1; /* tilap. */
        int absgap;

        if (gap>=0) absgap=gap; else absgap=-gap;
        *koodi='\233';  /* CSI */
        koodi[1]=EOS;
   /*   muste_itoa(skaala*gap,luku,10); strcat(koodi,luku); kun skaala!=1  */
        muste_itoa(absgap,luku,10); strcat(koodi,luku);
        if (gap<0) strcat(koodi,";1");
        strcat(koodi,"w");
        *luku=merkki; luku[1]=EOS; strcat(koodi,luku);
        *luku='\233'; strcat(koodi,luku);
        strcat(koodi,"w");

        i=0; while (koodi[i]) { putc((int)koodi[i],kirjoitin); ++i; }

        }


static int chapter(char *x)
        {
        char y[LLENGTH];
        char *sana[5], *def[4];
        int i,k;
        char kpl[LLENGTH];
        int j1,j2;

        strcpy(y,x);
        i=split(x,sana,5);
        if (i==2)
            {
            i=4; sana[3]=current_field;
            }
        if (i<4)
            {
            sprintf(sbuf,"\nError in %s",y); sur_print(sbuf); WAIT; return(-1);
            }
        strcpy(kpl,sana[1]);
        i=edt_avaus(sana[3]); if (i<0) return(-1);

        if (strcmp(kpl,"*")==0)
            {
            j1=1; j2=ued2;
            }
        else
            {
            i=uwfind("DEF",kpl,1);
            if (i<0)
                {
                sprintf(sbuf,"\nDEF %s not found in file %s!",kpl,sana[3]);
                sur_print(sbuf); WAIT; muste_fclose(edfield); return(-1);
                }

            uedread(x,i);
            k=split(x,def,4);
            if (k<3)
                {
                uedread(x,i);
                sprintf(sbuf,"\nFile %s: Error in %s",sana[3],x);
                sur_print(sbuf); WAIT; muste_fclose(edfield); return(-1);
                }
            if (k==3) j1=i+1;
            else
                {
            j1=uedline(def[2],1); if (j1==0) { uvirhe(def[2],sana[3]); return(-1); }
                k=4;
                }
            j2=uedline(def[k-1],j1); if (j2==0) { uvirhe(def[k-1],sana[3]); return(-1); }
            }

        tila=1;
        i=tulosta_rivit(j1,j2);
        muste_fclose(edfield);
        if (i<0) return(-1);
        tila=0;

        return(1);
        }

static void uvirhe(char *rivi,char *tied)
        {
        sprintf(sbuf,"\nLine %s not found in edit file %s",rivi,tied);
        sur_print(sbuf); muste_fclose(edfield);
        WAIT;
        }

static void uedread(char *x,int j)
        {
        int i;

        muste_fseek(edfield,(long)(j*ued1),0);
        for (i=0; i<ued1; ++i) x[i]=(char)getc(edfield); x[ued1]=EOS;
        if (j<=ued2) return;
    /* Shadow line */
        for (i=0; i<ued1-2; ++i) x[i]=x[i+2];
        x[ued1-2]=EOS; strcat(x,"  ");
        }

static int edt_avaus(char *edfile)
        {
        int i;
        char nimi[LNAME];
        char x[LLENGTH], y[LLENGTH];
        char *sana[5];
        int j,k,h;
        char rivi[ELE];
        int g;
        short *pint;


        while (1)
            {
            strcpy(nimi,edfile);
            if (strchr(nimi,':')==NULL && nimi[0]!='.' && nimi[0]!='~' && nimi[0]!='/' && nimi[0]!='\\') // RS unix path FIXME
              { 
              strcpy(nimi,edisk); strcat(nimi,edfile);
              }
            i=strlen(nimi);
            if (strchr(nimi+i-4,'.')==NULL) { strcat(nimi,".EDT"); }
            edfield=muste_fopen(nimi,"rb");
            if (edfield!=NULL) break;

            sprintf(sbuf,"\nEdit file %s not found!",edfile); sur_print(sbuf);
            sur_print("\nChange diskette and press 'space'");
            sur_print("\nor");
            sur_print("\ninterrupt by pressing '.'");
            i=sur_getch(); if (i=='.') return(-1);
            }

        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(edfield);
        rivi[ELE-1]=EOS;

        if (strncmp(rivi,"SURVO 98 edit field:",20)==0)
            {
            muste_fclose(edfield);
            i=edt32_avaus(nimi); if (i<0) return(-1);
            strcpy(sbuf,etmpd); strcat(sbuf,"PRINTTMP.EDT");
            return(edt_avaus(sbuf));
            }

        g=split(rivi,sana,5);
        if (strcmp(sana[0],"SURVO84ED")!=0) {muste_fclose(edfield); return (-1);}
        ued1=atoi(sana[1]);
        ued2=atoi(sana[2]);
        uedshad=ED3;
        if (g>4 && *sana[4]=='S') uedshad=atoi(sana[4]+1);

        k=sh_malloc(ued2); if (k<0) { muste_fclose(edfield); return(-1); }

        for (i=1; i<=ued2; ++i) uzs[i]=0;

        muste_fseek(edfield,(long)ued1*(ued2+1),0);
        if (feof(edfield)) { pr_filerr(); muste_fclose(edfield); return(-1); }

        for (i=0; i<ued1; ++i) x[i]=(char)getc(edfield);
        if (strncmp(x,"Sha",3)!=0) return(1);
        j=1; h=ued2+1;  /* Shadows-rivi vÑlissÑ */
        while (j>0)
            {
            for (i=0; i<ued1; ++i) x[i]=(char)getc(edfield);
            x[ued1]=EOS;
            if (strncmp(x,"END",3)==0) { j=0; continue; }
            pint=(short *)x;
            j=*pint; ++h;
            uzs[j]=h;  /* i=creatshad(j); */
            }
        if (ferror(edfield)) { pr_filerr(); muste_fclose(edfield); return(-1); }
/*  printf("\nuzs:"); for (i=1; i<=ued2; ++i) printf(" i=%d j=%d",i,uzs[i]);
    getch();
*/
        return(1);

        }

static int edt32_avaus(char *nimi)
        {
        char x[LNAME];

        strcpy(x,etmpd); strcat(x,"PRINTTMP.EDT");
        return(edt32to16(nimi,x));
        }


static int edt32to16(char *name32,char *name16)
        {
        int i,j;
        char rivi32[LLENGTH+10];
        int nr,nc,nshad;
        char *p;
        int luettu;
        static FILE *edt32_file;
		static FILE *edt16_file;
		static FILE *shadow_file;


// printf("name32=%s name16=%s\n",name32,name16); getch();
        edt32_file=muste_fopen(name32,"rt");
        if (edt32_file==NULL) { error_file_32_16(name32); return(-1); }
        edt16_file=muste_fopen(name16,"wb");
        if (edt16_file==NULL) { error_file_32_16(name16); return(-1); }

        fgets(rivi32,100,edt32_file);
        if (strncmp(rivi32,"SURVO 98 edit field:",20)!=0)
            { error_file_32_16(name32); return(-1); }
        /* dimensioita ei lueta vaan tutkitaan itse edit-rivit */
        nr=nc=nshad=0;
        while(!feof(edt32_file))
            {
            fgets(rivi32,LLENGTH+9,edt32_file);
            if (feof(edt32_file)) break;
            p=strchr(rivi32,'|');
            if (p==NULL) { error_file_32_16(name32); return(-1); }
            if (*rivi32=='S')
                {
                ++nshad;
                i=strlen(p+1)+1; /* 2 tavua rivinumeroa varten 28.6.1999 */
                if (i>nc) nc=i;
                continue;
                }
            *p=EOS; nr=atoi(rivi32);
            i=strlen(p+1)-1;
            if (i>nc) nc=i;
            }
// printf("nr=%d nc=%d nshad=%d\n",nr,nc,nshad); getch();
        if (nc<40) nc=40;
        sprintf(sbuf,"SURVO84ED %d %d   %d S%d",nc,nr,nc,nshad);
        for (i=strlen(sbuf); i<nc; ++i) sbuf[i]=' '; sbuf[i]=EOS;
        fprintf(edt16_file,"%s",sbuf);

        strcpy(rivi32,etmpd); strcat(rivi32,"SHADOWS.TMP");
        shadow_file=muste_fopen(rivi32,"wb");

        rewind(edt32_file);
        fgets(rivi32,100,edt32_file);
        luettu=0; j=0;
        while (!feof(edt32_file))
            {
// printf("j=%d luettu=%d\n",j,luettu); getch();
            if (luettu)
                {
                if (luettu==-1) /* Shadow line */
                    {
                    p=strchr(rivi32,'|');
                    sprintf(sbuf+2,"%s%.*s",p+1,(int)(nc-strlen(p+1)-2),space);
                    *(unsigned short *)sbuf=(unsigned short)j;
                    for (i=0; i<nc; ++i) putc((int)sbuf[i],shadow_file);
                    luettu=0;
                    continue;
                    }
                ++j;
                while (j<luettu)
                    {
                    fprintf(edt16_file,"*%.*s",nc-1,space);
                    ++j;
                    }
                p=strchr(rivi32,'|');
                fprintf(edt16_file,"%s%.*s",p+1,(int)((int)nc-(int)strlen(p+1)),space);
                luettu=0;
                continue;
                }
/*          if (j>=nr) break;   */
            i=0;
            while (!feof(edt32_file))
                {
                rivi32[i]=(char)getc(edt32_file);
                if (rivi32[i]=='\n') { rivi32[i]=EOS; break; }
                ++i;
                }
            if (*rivi32=='S') luettu=-1;
            else luettu=atoi(rivi32);
            }
        if (nshad>0)
            {
            muste_fclose(shadow_file);
            strcpy(rivi32,etmpd); strcat(rivi32,"SHADOWS.TMP");
            shadow_file=muste_fopen(rivi32,"rb");
            fprintf(edt16_file,"Shadows%.*s",nc-7,space);
            for (i=0; i<nshad*nc; ++i)
                putc(getc(shadow_file),edt16_file);
            fprintf(edt16_file,"END");
            }

        muste_fclose(shadow_file);
        muste_fclose(edt16_file);
        muste_fclose(edt32_file);

        return(1);
        }

static int error_file_32_16(char *name)
        {
        sprintf(sbuf,"\nCannot open file %s or error in file!",name);
        sur_print(sbuf); WAIT; return(1);
        }

static int sh_malloc(unsigned int ued2)
        {        
//        if (uzs!=NULL) { muste_free((char *)uzs); uzs=NULL; }
//        uzs=(int *)muste_malloc(sizeof(int)*(ued2+1));
		uzs=(int *)muste_realloc(uzs,sizeof(int)*(ued2+1)); // RS CHA
            /* Huom! Indeksointi [1],...,[ed2],   [0] ei kÑytîssÑ */
        if (uzs==NULL)
            {
            sur_print("\nNot space enough! (sh_malloc)");
            WAIT; return(-1);
            }
        return(1);
        }
/*
  uwfind(word1,word2,lin) etsii sanaparin rivin alusta riviltÑ lin lÑhtien
*/

static int uwfind(char *word1,char *word2,int lin)
        {
        int j,g;
        char x[LLENGTH];
        char *sana[2];

        for (j=lin; j<=ued2; ++j)
                {
                uedread(x,j);
                g=split(x+1,sana,2);
                if (g==0) continue;
                if (strcmp(word1,sana[0])==0)
                        {
                        if (word2==NULL) break;
                        if (g==2 && strcmp(word2,sana[1])==0) break;
                        }
                }
        if (j>ued2) return (-1);
        return (j);
        }

static int uedline(char *sana,unsigned int lin)
/* unsigned int lin;  alkurivi */
        {
        short j,k;
        char SANA[3];
        char x[LLENGTH];
        int muutos;  /* esim. A-3 -> muutos=-3 */

        if (posnro(sana))
                {
                j=atoi(sana); if (j>=lin && j<=ued2) return(j);
                return(0);
                }
        for (j=0; j<3; ++j) SANA[j]=toupper(sana[j]);
        if (strncmp(SANA,"END",3)==0)
                {
                j=ulastline();
                if (strlen(sana)<5) return(j);
                j+=atoi(sana+3);
                if (j>=lin && j<=ued2) return(j);
                return(0);
                }

        muutos=0;                           /* 19.1.91 */
        if (strlen(sana)>1)
            {
            muutos=atoi(sana+1);
            }
        for (j=lin; j<=ued2; ++j)
            {
            uedread(x,j);
            if ( *x==*sana ) return(j+muutos);
            }
        return(0);
        }

static int ulastline()
        {
        int i,j;
        char x[LLENGTH];

        j=ued2;
        while (j>0)
                {
                uedread(x,j);
                if (strncmp(space,x+1,ued1-1)!=0) break;
                --j;
                }
        return(j);
        }

static int pr_list(char *x)
        {
        int i;
        char *s[2];

        i=split(x,s,2);
        if (i<2)
            {
            sur_print("\nError in - list! Usage: - list <LST_file>");
            WAIT; return(-1);
            }
        lst_file_find(s[1]);

        return(1);
        }


static int lst_file_find(char *lista)
        {
        int i,n,k;
        char nimi[LNAME];
        char *p;
        char chp[LNAME],edt[LNAME];
        char lst_polku[LNAME];
        char kent[LNAME];
        char x[LLENGTH];
        int i1=0,i2=0;

        *nimi=EOS;
        if (strchr(lista,':')!=NULL || strchr(lista,'\\')!=NULL || strchr(lista,'/')!=NULL) // RS ADD / unix path FIXME
            {
            strcpy(nimi,lista);
            strcpy(lst_polku,lista);
            n=strlen(lst_polku); p=lst_polku+n-1;
            while (*p!=':' && *p!='\\' && *p!='/') { *p=EOS; --p; } // RS ADD / unix path FIXME
            }
        else
            { *lst_polku=EOS; strcpy(nimi,edisk); strcat(nimi,lista); }
        p=strchr(nimi+strlen(nimi)-4,'.');
        if (p==NULL) strcat(nimi,".LST");
        lst_file=muste_fopen(nimi,"rt");
        if (lst_file==NULL) return(-1);

        while (!feof(lst_file))
            {
            chp_edt_read(chp,edt);
            if (muste_strcmpi(chp,"END")==0) break;
            k=edt_numbers(edt,&i1,&i2);
            strcpy(kent,edt);
            if (strchr(edt,':')==NULL && strchr(edt,'\\')==NULL && strchr(edt,'/')==NULL) // RS ADD / unix path FIXME
                {
                strcpy(kent,lst_polku); strcat(kent,edt);
                }
            if (k==1)
                {
                i=pr_list2(chp,kent); if (i<0) return(-1);
                continue;
                }
            for (k=i1; k<=i2; ++k)
                {
                sprintf(x,"%s%d",kent,k);
                i=pr_list2(chp,x); if (i<0) return(-1);
                }
            }
        muste_fclose(lst_file);
        return(1);
        }

static int pr_list2(char *chp,char *kent)
        {
        int i;
        char x[LLENGTH];

        sprintf(x,"chapter %s in %s",chp,kent);
    /*  printf("\nx=%s",x); getch();  */
        return(chapter(x));
        }

static int chp_edt_read(char *chp,char *edt)
        {
        int i;
        char x[LLENGTH],*osa[2];

        while (1)
            {
            lst_read_line(x);
            if (feof(lst_file)) break;
            i=split(x,osa,2);
            if (i<2)
                {
                if (muste_strcmpi(osa[0],"END")==0)
                    {
                    strcpy(chp,osa[0]); *edt=EOS; break;
                    }
                else continue;
                }
            if (strcmp(osa[0],"/")==0) continue;
            strcpy(chp,osa[0]);
            strcpy(edt,osa[1]);
            break;
            }

        return(1);
        }

static int lst_read_line(char *x)
        {
        int m;
        char *p;

        p=x;
        while (1)
            {
            m=getc(lst_file);
            if (m==EOF || m=='\n') break;
            *p=(char)m; ++p;
            }
        *p=EOS;
        return(1);
        }

static int edt_numbers(char *edt,int *pi1,int *pi2)
        {
        int i;
        char *p;
        char x[LLENGTH];

        strcpy(x,edt);
        p=strchr(edt,'-');
        if (p==NULL) return(1);
        *p=EOS;
        *pi2=atoi(p+1);
        --p;
        while (p>=edt && *p>='0' && *p<='9') --p;
        if (*p=='0')
            {
            sur_print("\nIndex notations starting with `0' are not permitted");
            sur_print("\nin LIST edit fields!");
            WAIT; return(-1);
            }
        *pi1=atoi(p+1);
        *(p+1)=EOS;
        i=*pi2-*pi1+1;
        if (i<2)
            {
            sprintf(sbuf,"\nIncorrect notation %s !",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(i);
        }

static void pr_filerr()
        {
        PR_EBLD;
        sur_print("\nDisk error when saving/loading an edit field!");
        WAIT; PR_ENRM;
        }



static int textfile(char *x)
        {
        int i,k;
        char y[LLENGTH];
        char *sana[4];
        char nimi[LNAME];
        int j,j1,j2;

        strcpy(y,x);
        k=split(x,sana,4);
        if (k<2)
            {
            sur_print("\nUsage: text <text_file>,<first_line>,<last_line>");
            WAIT; return(-1);
            }
        strcpy(nimi,sana[1]);
        subst_survo_path(nimi);
        if (strchr(nimi,':')==NULL) { strcpy(nimi,edisk); strcat(nimi,sana[1]); }
        while (1)
            {
            text=muste_fopen(nimi,"rt");
            if (text!=NULL) break;
            sprintf(sbuf,"\nText file %s not found!",nimi); sur_print(sbuf);
            sur_print("\nChange diskette and press 'space'");
            sur_print("\nor");
            sur_print("\ninterrupt by pressing '.'");
            i=sur_getch(); if (i=='.') return(-1);
            }

        j1=1; j2=32000;
        if (k>2) j1=atoi(sana[2]);
        if (k>3) j2=atoi(sana[3]);
        if (j1<1) j1=1;
        if (j2<j1) j2=32000;
        tila=2;
        j=1;
        while (j<j1) { i=lue_text(y); if (i<0) { muste_fclose(text); return(-1); } ++j; }
        tulosta_rivit(j1,j2);
        muste_fclose(text);
        tila=0;
        return(1);
        }

static int textread(char *x,int j)
        {
        int i;

        *x='*';
        i=lue_text(x+1);
        return(i);
        }

static int lue_text(char *rivi)
        {
        char *p;

        p=rivi;
        while ((*p=(char)getc(text))!='\n' && p-rivi<LLENGTH-2) ++p;
        *p=EOS;
        tab_poisto(rivi);
        if (feof(text)) return(-1);
        return(1);
        }


static void tab_poisto(char *s)
        {
        char t[LLENGTH];
        char *p;
        int i;

        if (strchr(s,TAB)==NULL) return;
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
        }


static int ascii_text(char *x)
        {
        int i,k;
        char y[LLENGTH];
        char *sana[2];
        char nimi[LNAME];

        strcpy(y,x);
        k=split(x,sana,2);
        if (k<2)
            {
            sur_print("\nUsage: ascii_text <output_file_for_the_print_list>");
            WAIT; return(-1);
            }
        strcpy(nimi,sana[1]);
        if (strchr(nimi,':')==NULL) { strcpy(nimi,edisk); strcat(nimi,sana[1]); }
        ascii_file=muste_fopen(nimi,"wt");
        if (ascii_file==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s!",nimi); sur_print(sbuf);
            WAIT; return(-1);
            }
        ascii_text_saving=1;
        return(1);
        }

static void ascii_save(char *x)
        {
        int len;
        char y[LLENGTH]; /* 16.12.1995 */

        strcpy(y,x);
        len=strlen(y); while (y[len-1]==' ' && len>1) y[--len]=EOS;
        fprintf(ascii_file,"%s\n",y);
        }


static int picture(char *x)  /* picture <canon_file>,<x_home>,<y_home>          */
                             /*                                *=automatic      */
        {
        int i;
        char *sana[4];
        int x_home,y_home;
        int ch;
        char y[LLENGTH];

        if (pr_type==1) return(ps_picture(x));
        i=split(x,sana,4);
        if (i<4)
            {
            sur_print("\nCorrect form: picture <canon_file>,<x_home>,<y_home>");
            WAIT; return(-1);
            }
        x_origin=VASEN_REUNA; y_origin=ALAREUNA;
        x_home=atoi(sana[2]);
        if (*sana[3]!='*') y_home=atoi(sana[3]);
        else               y_home=y_origin-line_count/720.0*254.0;

        i=c_avaa(sana[1]); if (i<0) return(-1);

/*      *y=EOS; cat(y,C_CSI); strcat(y,"0;48x"); send(y);    talleta paikka */

        etsi(vector_mode,0);  /* ei kopioida */
        send(vector_mode);
        etsi("}\"",1);        /* set origin */
        *y=EOS; cat(y,C_IS2);
        etsi(y,0);
        p_origin(x_home,y_home);

        while (!feof(canon_file))
            {
            ch=(unsigned char)merkki();
            putc((int)ch,kirjoitin);
            }
        muste_fclose(canon_file);
/*      *y=EOS; cat(y,C_CSI); strcat(y,"1;48x"); send(y);    hae paikka 48 */
        i=tekstityyppi(); if (i<0) return(-1);
        return(1);
        }

static int c_avaa(char *s)
        {
        char nimi[LLENGTH];

        strcpy(nimi,s);
        if (strchr(s,':')==NULL)
            {
            strcpy(nimi,edisk); strcat(nimi,s);
            }
        if (pr_type==1)
            canon_file=muste_fopen(nimi,"rt");
        else
            canon_file=muste_fopen(nimi,"rb");
        if (canon_file==NULL)
            {
            sprintf(sbuf,"\nFile %s not found!",s); sur_print(sbuf);
            WAIT; return(-1);
            }
        return(1);
        }

static int etsi(char *s,int kopio)
/* int kopio;  1=kopioidaan, 0=ei */
        {
        char ch;
        char *t;

        while (1)
            {
            if (feof(canon_file)) { pict_file_error(s); return(-1); }
            t=s;
            ch=(unsigned char)merkki();
            if (kopio) putc((int)ch,kirjoitin);
            if (ch!=*s) continue;
            while (1)
                {
                if (feof(canon_file)) { pict_file_error(s); return(-1); }
                ++t; if (*t==EOS) break;
                ch=(unsigned char)merkki();
                if (ch!=*t) { ungetc((int)ch,canon_file); break; }
                if (kopio) putc((int)ch,kirjoitin);
                }
            if (*t==EOS) break;
            }

/*  printf("\ni=%ld",ftell(canon_file)); getch();     */
        return(1);
        }

static int merkki()
        {
        return(getc(canon_file));
        }

static void pict_file_error(char *s)
        {
        sprintf(sbuf,"\nCannot find  %s  in picture file!",s); sur_print(sbuf); WAIT;
        }

static int p_origin(int x,int y)
        {
        char s[32];
        char luku[4];

        vdc(x_origin+x,luku); strcpy(s,luku);
        vdc(y_origin-y,luku); strcat(s,luku);
        cat(s,C_IS2);
        send(s);
        return(1);
        }

static void cat(char *s,char m)
        {
        int len=strlen(s);

        s[len]=m; s[len+1]=EOS;
        }

static int send(char *s)
        {
        unsigned char *p;
        p=s; while (*p) { putc((int)(*p),kirjoitin); ++p; }
        return(1);
        }

static int ps_picture(char *x)  /* picture <ps_file>,<x_home>,<y_home>,<x_scale>,<y_scale> */
                                /*                                *=automatic      */
        {
        int i,j;
        char *sana[7];
        int x_home=0,y_home=0;
        double x_scale,y_scale,angle;
        int ch;
        char y[LLENGTH];
        char xvara[LLENGTH];
        char astr[]="*";  /* 4.7.1992 */

        strcpy(xvara,x);

        x_scale=y_scale=1.0; angle=0.0;
        i=strlen(x); while (x[i-1]==' ') x[--i]=EOS;

  if (!silent)
        {
        sur_print("\n***********************");
        sprintf(sbuf,"\n%s",x+1); sur_print(sbuf);
        sur_print("\n***********************");
        }

        i=split(x,sana,7);
        if (i<2)
            {
            sur_print("\nCorrect form: %s <ps_file>,<x_home>,<y_home>,<x_scale>,<y_scale>",sana[0]);
            WAIT; return(-1);
            }
        if (i<3) sana[2]=astr;  /* 4.7.1992 */
        if (i<4) sana[3]=astr;
        x_origin=0; y_origin=0;
        if (*sana[2]!='*') x_home=atoi(sana[2]);
        if (*sana[3]!='*') y_home=atoi(sana[3]);
/*      else               y_home=255.0/72.0*(page_length-line_count);     */
        if (i>4) { x_scale=atof(sana[4]); if (x_scale==0.0) x_scale=1.0; }
        if (i>5) { y_scale=atof(sana[5]); if (y_scale==0.0) y_scale=1.0; }
        if (i>6) angle=atof(sana[6]);

        i=c_avaa(sana[1]); if (i<0) return(-1);

        if (*sana[3]=='*' && *sana[2]=='*')
            {
            i=atoi(sana[3]+1); j=atoi(sana[2]+1);
            sprintf(y,"gsave currentpoint %d m add exch %d m add exch translate\n",
                             i,j);
            }
        else if (*sana[3]=='*')
            {
            i=atoi(sana[3]+1);
            sprintf(y,"gsave %d m currentpoint exch pop %d m add translate\n",
                             x_home,i);
            }
        else if (*sana[2]=='*')
            {
            j=atoi(sana[2]+1);
            sprintf(y,"gsave currentpoint pop %d m add %d m translate\n",
                             j,y_home);
            }
        else
            sprintf(y,"gsave %d m %d m translate\n",
                             x_home,y_home);
        send(y);
        if (angle>0.0)
            {
            sprintf(y,"%g rotate\n",angle); send(y);
            }
        sprintf(y,"%g %g scale\n",x_scale,y_scale); send(y);
        if (angle<0.0)
            {
            sprintf(y,"%g rotate\n",-angle); send(y);
            }

        split(xvara,sana,1);
        if (muste_strcmpi(sana[0],"autocad")==0) return(ps_autocad());
        if (muste_strcmpi(sana[0],"file")==0) return(ps_file());
        if (muste_strcmpi(sana[0],"epsfile")==0) return(ps_epsfile());

        i=etsi("%SURVO 84C",0); if (i<0) return(-1); /* ei kopioida */
        i=etsi("\n",0); if (i<0) return(-1);
        i=etsi("%SURVO 84C",1); if (i<0) return(-1);
        muste_fclose(canon_file);
        send("\ngrestore\n");
        return(1);
        }


static int tabs(int j)
        {
        int poista;

        poista=0;
        if (tila==0) edread(tabrivi,j); else uedread(tabrivi,j);
        if (strchr(tabrivi+1,'T')==NULL && strchr(tabrivi+1,'(')==NULL)
                                        /* 7.5.1991 */
            {
            *tabrivi=EOS;
            poista=1;   /* 29.4.1994 */
            }
        else if (pitch_unit==0) pitch_load();
        if (pr_type==1) set_tabwidth();

        if (poista) ps_charwidth=0.0;   /* 29.4.1994 */
        return(1);
        }

static int tabvenytys(char *x,char *xs,int gap[])
/* int gap[];  venytysarvot positioittain */
        {
        int i,len;
        int sar,sar2,sarpitch;
        int tilanne,tavoite;
        char *p;
        char jakso[LLENGTH];
        int pu;

        len=strlen(x); while (x[len-1]==' ') x[--len]=EOS;
        for (i=1; i<len; ++i) gap[i]=0;
        if (len<=1) return(1);
        sar=0; sarpitch=0;
        pu=pitch_unit=pitch[pitch_ind];
        p=strchr(tabrivi+1,'*');   /* tabrivi T  *30  T   T   */
        if (p!=NULL) { pu=atoi(p+1); if (pu<=0) pu=pitch_unit; }

        while ((p=strchr(tabrivi+sar+1,'T'))!=NULL && sar+1<len )
            {
            sar2=p-tabrivi;
            strncpy(jakso,x+sar+1,sar2-sar); jakso[sar2-sar]=EOS;
            tavoite=strlen(jakso)*pu;
            tilanne=strpitch(jakso);
/*      printf("\ntilanne=%d tavoite=%d",tilanne,tavoite); getch();
*/
            i=sar2-1; if (i>=len) break;
            while(x[i]!=' ') --i;
            gap[i]=tavoite-tilanne;
            if (i>0 && gap[i]>372)
                {
                int h=i;   /* max.lisÑvÑli =6*63=378 */
                while (h>0 && gap[h]>372 && gap[h-1]==0)
                    {
                    gap[h-1]=gap[h]-372; gap[h]=372;
                    --h;
                    }
                }

            sar=sar2;
            }
        return(1);
        }



static int replace(char *x)
        {
        int i;
        char *sana[2];

        repl1=NULL;
        strcpy(repl,x);
        i=split(repl+1,sana,2);
        if (i<2) return(1);
        repl1=sana[1];
        repl2=repl1+strlen(repl1)+1;
        return(1);
        }

static void replacement(char *x)
        {
        int i,len;
        char *p,*q;
        char y[LLENGTH];

        if (repl1==NULL) return;
        p=x;
        while (1)
            {
            p=strstr(p,repl1); if (p==NULL) return;
            len=strlen(repl1);
            q=repl2;
            for (i=0; i<len; ++i) *p++=*q++;
            }
        }

static void roman(int n,char *x)
{
static char a[]="ivxlcdm  ";
char *p, r[21]; int i,j=19;

p=a; r[20]='\0';
do
  {
  i = n%5;
  if (i < 4)
     {
     while (--i >= 0)  r[j--] = *p;
     if ( (n%10) > 4)  r[j--] = *(p+1);
     }
  else {r[j--] = ((n%10)==4) ? *(p+1) : *(p+2); r[j--] = *p;}
  p+=2;
  }
while ((n/=10) > 0);
p = r +(j+1); strcpy(x,p);
}


static void ps_init()
        {
        int i;

        for (i=0; i<256; ++i) ps_str[i]=NULL;
        ps_str[(unsigned int)'\\']="\\\\";
        ps_str[(unsigned int)'(']="\\(";
        ps_str[(unsigned int)')']="\\)";
        }

static int ps_code(char *x,char **sana,int n,char *rivi)   /* ps_code <koodi> <8#xxx> <ps-nimi> */
        {
        int i,k;
        unsigned char merkki;
        char y[LLENGTH];

        if (n<4) { koodivirhe(rivi); return(-1); }
        sprintf(y,"%s %s\n",sana[2],sana[3]); ps_kirjoita(y);

        i=strlen(sana[1]);
        if (i==1) merkki=sana[1][0];
        else { i=muunna(sana[1],y); if (i<0) return(-1); merkki=*y; }

        *y='\\'; y[1]=EOS; strcat(y,sana[2]+2);

        if (ps_str[merkki]!=NULL && len_ps_str[merkki]>=strlen(y))
            strcpy(ps_str[merkki],y);
        else
            {
            len_ps_str[merkki]=strlen(y);
            pr_test(len_ps_str[merkki]);
            ps_str[merkki]=pr_osoitin; strcpy(pr_osoitin,y);
            pr_osoitin+=len_ps_str[merkki]+1;
            }
        return(1);
        }

static int ps_replace(unsigned char *x)
        {
        unsigned char y[4*LLENGTH];
        unsigned char *p,*q;
        int len;
        char koodi[8];

        p=x; *y=EOS; len=0;
        while (*p)
            {
            q=ps_str[(unsigned int)*p];
            if (q==NULL)
                {
                if (*p>31 && *p<128)   /* *p>31 4.4.1996 */
                    { y[len++]=*p; y[len]=EOS; }
                else
                    {
                /*  koodi[0]='\\';
                    muste_itoa((int)*p,koodi+1,8);
                */
                    sprintf(koodi,"\\%03o",(int)*p);  /* 4.4.1996 */
                    strcat(y,koodi); len+=strlen(koodi);
                    }
                }
            else
                { strcat(y,q); len+=strlen(q); }
            ++p;
            }
        strcpy(x,y);
        return(len);
        }

static void uusi_vstrike(double a)
        {
        char x[LLENGTH];

        if (a==vstrike) return;
        vstrike=a;
        sprintf(x,"/vst %g def\n",vstrike);  ps_kirjoita(x);
        }
        
static void uusi_hstrike(double a)
        {
        char x[LLENGTH];

        if (a==hstrike) return;
        hstrike=a;
        sprintf(x,"/hst %g def\n",hstrike);  ps_kirjoita(x);
        }

static int ps_tulosta(unsigned char *x,unsigned char *xs)
		{
        int i,k,len,slen,h,j;
        unsigned char varjo;
        unsigned char *p; // RS ADD unsigned
        char y[LLENGTH], yy[LLENGTH];

        len=strlen(x);
        while(len>1 && x[len-1]==' ') --len;

        p=xs;          /*  anything with euro_shadow E is 'euro' */
        while (1)
            {
            p=strchr(p,euro_shadow);
            if (p==NULL) break;
            x[p-xs]='C'; ++p;
            }

        if (ind_page_number) set_page_number(x);
        if (prind2 && !silent )
            {
            i=c3+8; if (len<i) i=len;  /* 26.5.92 */
            sprintf(sbuf,"\n%*.*s",i,i,x+1); sur_print(sbuf);
            }
        if (len<=1)
            {
            if (*xs==EOS) { uusi_rivi(); return(1); }
            len=strlen(xs); while (xs[len-1]==' ') --len;
            }

        if (!ind_page_number)
            { i=tekstityyppi(); if (i<0) return(-1); }

        if (*xs==EOS)
            {
            ps_kirjoita("(");
            for (i=1; i<len; ++i) x[i]=code[x[i]];
            x[len]=EOS; len=ps_replace(x+1);
            for (i=1; i<=len; ++i) putc((int)x[i],kirjoitin);
            ps_kirjoita(") s_show\n");
            uusi_rivi();
            return(1);
            }
        slen=strlen(xs);
        while(xs[slen-1]==' ') --slen;
        ++slen; /* jotta saadaan erikoismoodit pois rivin lopussa */
        if (slen>len) len=slen;
        x[len]=EOS; xs[len]=EOS;
        i=1;
        while (xs[i])
            {
            varjo=xs[i];
/********************************************
            if (varjo=='R')
                {
                varjo=x[i];
                *y=varjo; *(y+1)=EOS;
                if (raster[varjo]!=NULL) strcpy(y,raster[varjo]);
                k=fontprint(y); if (k<0) return(-1);
                ++i; continue;
                }
**********************************************/
            p=shadow[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            k=0;
            while (xs[i]==varjo) { y[k]=code[x[i]]; echo[k]=y[k]; ++k; ++i; }
            y[k]=EOS; echo[k]=EOS;

            ps_kirjoita("(");
            h=ps_replace(y);
            for (j=0; j<h; ++j) putc((int)y[j],kirjoitin);
            ps_kirjoita(") prnshow\n");

            p=shadow2[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow2[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            }
        uusi_rivi();
        return(1);
        }


static int ps_kirjoita(unsigned char *s)
        {
        int i;
        for (i=0; i<strlen(s); ++i) putc((int)s[i],kirjoitin);
        return(1);
        }

static int ps_ptulosta(unsigned char *x,unsigned char *xs)
        {
        int i,k,len,slen,h,j;
        unsigned char varjo;
        unsigned char *p; // RS ADD unsigned
        char y[LLENGTH], yy[LLENGTH];

        len=strlen(x);
        while(x[len-1]==' ') --len;
        if (ind_page_number) set_page_number(x);
        if (prind2 && !silent )
            {
            sprintf(sbuf,"\n%.*s",len,x+1); sur_print(sbuf);
            }
        p=xs;
        while (1)
            {
            p=strchr(p,euro_shadow);
            if (p==NULL) break;
            x[p-xs]='C'; ++p;
            }

        if (len<=1)
            {
            if (*xs==EOS) { uusi_rivi(); return(1); }
            len=strlen(xs); while (xs[len-1]==' ') --len;
            }

        if (!ind_page_number)
            { i=tekstityyppi(); if (i<0) return(-1); }

        slen=strlen(xs);
        if (!slen) { strncpy(xs,space,len); xs[len]=EOS; slen=len; }
        else while(xs[slen-1]==' ') --slen;
        if (slen>len) len=slen;
        x[len]=EOS; xs[len]=EOS;
        if (strchr(".:",x[len-1])!=NULL) end_tol=end_spec_tol;
        else end_tol=end_norm_tol;
        if (pakkovenytys) end_tol=2000.0;

        if (*tabrivi || erivi) { ps_tabprint(x,xs); return(1); }
        ps_print(x,xs,0);
        ps_print(x,xs,1);
        uusi_rivi();
        return(1);
        }

static int ps_print(unsigned char *xx,unsigned char *xxs,int tosi)
/* int tosi;  0=testaus 1=painatus */
        {
        int i,k,len,slen,h,j,nsp;
        unsigned char varjo;
        char *p;
        char y[LLENGTH], yy[LLENGTH];
        unsigned char x[LLENGTH],xs[LLENGTH];
        char s[LLENGTH];
        int splen;
        static int nspace;

        prind=tosi;
        strcpy(x,xx); strcpy(xs,xxs);
        splen=0; while (x[splen+1]==' ' && xs[splen+1]==' ') ++splen;
                                      /* lisÑtty 6.1.88 */
        if (!tosi)
            {
            i=splen+1;
            ps_kirjoita("/prline [");
            while (xs[i])
                {
                varjo=xs[i];
                k=0;
                while (xs[i]==varjo) { y[k]=code[x[i]]; echo[k]=y[k]; ++k; ++i; }
                y[k]=EOS; echo[k]=EOS;
                ps_kirjoita("(");
                h=ps_replace(y);
                for (j=0; j<h; ++j) putc((int)y[j],kirjoitin);
                ps_kirjoita(")");
                }
            ps_kirjoita("] def\n");

            ps_kirjoita("/pri 0 def /prind false def\n");
            sprintf(y,"/prlev %g di def /prsum 0 def\n",line_length);
            ps_kirjoita(y);
            if (splen)
                {
                sprintf(y,"(%.*s) stringwidth pop prsum add /prsum exch def\n",
                                     splen,space);
                ps_kirjoita(y);
                }
            }
        else  /* tosi */
            {
            if (end_tol!=end_tol0)
                {
                sprintf(y,"\n/endtol %g def\n",end_tol);
                ps_kirjoita(y); end_tol0=end_tol;
                }
            ps_kirjoita("/pri 0 def\n /prind true def\n");
            nspace=0; p=x+splen+1;
            while (*p) { if (*p==' ') ++nspace; ++p; }
            if (splen)
                {
                sprintf(y,"(%.*s) show ",splen,space);
                ps_kirjoita(y);
                }
            if (nspace)
                {
                sprintf(y,"%d prex1\n",nspace);
                ps_kirjoita(y);
                }
            }
        i=splen+1;
        while (xs[i])
            {
            varjo=xs[i];
/**************************************
            if (varjo=='R')
                {
                varjo=x[i];
                *y=varjo; *(y+1)=EOS;
                if (raster[varjo]!=NULL) strcpy(y,raster[varjo]);
                k=fontprint(y); if (k<0) return(-1);
                ++i; continue;
                }
****************************************/
            p=shadow[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }

            k=0; nsp=0;
            while (xs[i]==varjo)
                { y[k]=code[x[i]]; echo[k]=y[k]; if (y[k]==' ') ++nsp; ++k; ++i; }
            y[k]=EOS; echo[k]=EOS;

            h=ps_replace(y);  /* ei tarpeen? */
            if (tosi)
                {
                if (nspace)
                    {
                    sprintf(s,"%d prex2\n",nsp);
                    ps_kirjoita(s);
                    }
                else
                    ps_kirjoita("prshow\n");
                }
            else
                ps_kirjoita("prex3\n");

            p=shadow2[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow2[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            }
        return(1);
        }



static int ps_tabprint(unsigned char *xx,unsigned char *xxs)
        {
        int i,i1,i2,k,j,h,len;
        char *p;
        double width,xwidth;
        unsigned char x[LLENGTH],xs[LLENGTH];
        unsigned char y[LLENGTH],yy[LLENGTH];
        unsigned char s[LLENGTH];
        unsigned char varjo;
        int tila;

        len=strlen(xx);

        if (erivi)
            {
            i=1; while (i<len && (xx[i]==' ' && xxs[i]==' ')) ++i;
            xwidth=line_length;
            ps_print2(xx+i,xxs+i,xwidth,0);
            sprintf(s,"/prlev %g di def\n",xwidth); ps_kirjoita(s);
            if (erivi==1)
                ps_kirjoita("/prlev prlev 2 div def /prsum prsum 2 div def\n");
            ps_print2(xx+i,xxs+i,xwidth,1);
            uusi_rivi();
            return(1);
            }

        width=ps_charwidth;   /* 12.11.88 */

        i1=1;
        ps_kirjoita("\ncurrentpoint /yp exch def /xp exch def\n");

        while (i1<len)
            {
            if (tabrivi[i1]=='T') tila=1;
            else if (tabrivi[i1]=='(') tila=2;
            else tila=0;
            i2=i1+1;
            while (i2<len)
                {
                if (tabrivi[i2]=='T' || tabrivi[i2]=='(' || tabrivi[i2]==')') break;
                ++i2;
                }
            if (tabrivi[i2]!=')') --i2;
            k=i2-i1+1;
            strncpy(x,xx+i1,k); x[k]=EOS;
            strncpy(xs,xxs+i1,k); xs[k]=EOS;

            if (tila=2 && tabrivi[i2]==')')
                {
                xwidth=k*width;
                ps_print2(x,xs,xwidth,0);
                ps_print2(x,xs,xwidth,1);
                }
            else
                {
                i=0;
                while (xs[i])
                    {
                    varjo=xs[i];
/*********************************************
                    if (varjo=='R')
                        {
                        varjo=x[i];
                        *y=varjo; *(y+1)=EOS;
                        if (raster[varjo]!=NULL) strcpy(y,raster[varjo]);
                        k=fontprint(y); if (k<0) return(-1);
                        ++i; continue;
                        }
**********************************************/
                    p=shadow[varjo];
                    if (p!=NULL)
                        {
                        strcpy(y,shadow[varjo]);
                        muunna(y,yy);
                        p=yy;
                        while (*p) { putc((int)(*p),kirjoitin); ++p; }
                        }
                    k=0;
                    while (xs[i]==varjo) { y[k]=code[x[i]]; echo[k]=y[k]; ++k; ++i; }
                    y[k]=EOS; echo[k]=EOS;
                    ps_kirjoita("(");
                    h=ps_replace(y);
                    for (j=0; j<h; ++j) putc((int)y[j],kirjoitin);
                    ps_kirjoita(") prnshow\n");

                    p=shadow2[varjo];
                    if (p!=NULL)
                        {
                        strcpy(y,shadow2[varjo]);
                        muunna(y,yy);
                        p=yy;
                        while (*p) { putc((int)(*p),kirjoitin); ++p; }
                        }
                    }
                }
            if (i2<len-1)
                {
                sprintf(s,"xp yp moveto /xp %d %g mul xp add def xp yp moveto\n",
                                           i2-i1+1,width);
                ps_kirjoita(s);
                }
            i1=i2+1;
            }
        uusi_rivi();
        return(1);
        }

static void set_tabwidth()
        {
        char x[LLENGTH];
        char *p;

        p=strchr(tabrivi+1,'*');
        if (p==NULL) ps_charwidth=0.6*font_size;
        else
            {
            strcpy(x,p+1);
            p=x; while (*p && *p!=' ') ++p; *p=EOS; ps_charwidth=atof(x);
            }
        }

static int ps_print2(unsigned char *xx,unsigned char *xxs,double xwidth,int tosi)
/* int tosi;  0=testaus 1=painatus */
        {
        int i,k,len,slen,h,j,nsp;
        unsigned char varjo;
        char *p;
        char y[LLENGTH], yy[LLENGTH];
        unsigned char x[LLENGTH],xs[LLENGTH];
        char s[LLENGTH];
        int splen;
        static int nspace;

        prind=tosi;
        strcpy(x,xx); strcpy(xs,xxs);
        splen=0;

        if (!tosi)
            {
            i=splen;
            ps_kirjoita("/prline [");
            while (xs[i])
                {
                varjo=xs[i];
                k=0;
                while (xs[i]==varjo) { y[k]=code[x[i]]; echo[k]=y[k]; ++k; ++i; }
                y[k]=EOS; echo[k]=EOS;
                ps_kirjoita("(");
                h=ps_replace(y);
                for (j=0; j<h; ++j) putc((int)y[j],kirjoitin);
                ps_kirjoita(")");
                }
            ps_kirjoita("] def\n");

            ps_kirjoita("/pri 0 def /prind false def\n");
            sprintf(y,"/prlev %g def /prsum 0 def\n",xwidth);
            ps_kirjoita(y);
            }
        else  /* tosi */
            {
            ps_kirjoita("/pri 0 def /prind true def\n");
            sprintf(y,"prlev prsum sub 0 rmoveto\n");
            ps_kirjoita(y);
            }
        i=splen;
        while (xs[i])
            {
            varjo=xs[i];
/******************************************
            if (varjo=='R')
                {
                varjo=x[i];
                *y=varjo; *(y+1)=EOS;
                if (raster[varjo]!=NULL) strcpy(y,raster[varjo]);
                k=fontprint(y); if (k<0) return(-1);
                ++i; continue;
                }
*******************************************/
            p=shadow[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }

            k=0; nsp=0;
            while (xs[i]==varjo)
                { y[k]=code[x[i]]; echo[k]=y[k]; if (y[k]==' ') ++nsp; ++k; ++i; }
            y[k]=EOS; echo[k]=EOS;

            h=ps_replace(y);  /* ei tarpeen? */
            if (tosi)
                    {
                    ps_kirjoita("prshow\n");
                 /* ps_kirjoita("/pri pri 1 add def\n"); */
                    }
            else
                    ps_kirjoita("prex3\n");

            p=shadow2[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow2[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            }
        return(1);
        }


static int read_footnotes(char *x,char *xs,int j,int j2)
        {
        char *p;
        char vx[LLENGTH],vxs[LLENGTH];
        int i;
        int vfn;

        strcpy(vx,x); strcpy(vxs,xs);
        vfn=n_footnote;

        p=xs;
        if (p==NULL) return(0);
        if (strchr(p,'F')==NULL)
            {
            pane_hakemistoon(x,xs);
            return(0);
            }
        n1_footnote=0;
        while (1)
            {
            p=strchr(xs,'F'); if (p==NULL) break;
            ++n_footnote; ++n1_footnote;
            fn_replace(x,xs,(int)(p-xs),n_footnote);
            ++p;
            }
        i=fn_testaa_sivu(j+1,j2);
        if (i<0)
            {
            strcpy(x,vx); strcpy(xs,vxs);
            n_footnote=vfn;
            return(i);  /* i=-1 virhe, i=-2 uusi sivu */
            }
        pane_hakemistoon(x,xs);
        i=fn_talleta(j+1,j2);

/*
CLS;
printf("\nx =%s",x);
printf("\nxs=%s",xs); getch();
*/
        return(i);
        }

static void fn_replace(char *x,char *xs,int k,int n)
        {
        char xx[LLENGTH],xxs[LLENGTH];
        char sn[9];
        int i,len,lenx;

        sprintf(sn,"%d",n);
        len=strlen(sn); lenx=strlen(x);
        for (i=0; i<k; ++i) { xx[i]=x[i]; xxs[i]=xs[i]; }
        for (i=0; i<len; ++i) { xx[k+i]=sn[i]; xxs[k+i]='3'; }
        for (i=k+1; i<lenx; ++i) { xx[i+len-1]=x[i]; xxs[i+len-1]=xs[i]; }
        xx[lenx+len-1]=EOS; xxs[lenx+len-1]=EOS;
        strcpy(x,xx); strcpy(xs,xxs);
        }

static int fn_testaa_sivu(int j,int j2)
        {
        int i,n;
        char x[LLENGTH];
        char *sana[1];
        double test_count;
        int i_fn;

        /* vain note_spacing-arvoa varten */
        strcpy(x,"[NOTE_START]");
        i=lue_koodit(x); if (i<0) return(1);
        note_spacing=line_spacing;
        strcpy(x,"[NOTE_END]");
        i=lue_koodit(x); if (i<0) return(1);

        i_fn=0; /* alaviiteiden lkm = n_footnote1 */
        test_count=line_count+line_spacing; /* 1 tyhjÑ rivi ennen alaviitteitÑ */

        ++j; /* ohitetaan 1.alaviitettÑ edeltÑvÑ (tyhjÑ) rivi */
        while (j<=j2)
            {
            if (test_count+note_spacing>page_length)
                {
                i=uusi_sivu();
                if (i<0) return(-1);
                return(-2);
                }
            if (tila==0) edread(x,j); else uedread(x,j);
            if (*x=='-') { ++j; continue; }
            if (strncmp(x+1,space,strlen(x+1))==0)
                {
                ++i_fn; if (i_fn==n1_footnote) break;
                ++j; continue;
                }
            test_count+=note_spacing;
            ++j;
            }
        return(1);
        }

static int fn_talleta(int j,int j2)
        {
        int i,n;
        char x[LLENGTH],xs[LLENGTH];
        char *sana[1];
        double test_count;
        int i_fn,j_fn;
        int uusi_viite=0;

        i_fn=0; /* alaviiteiden lkm = n_footnote1 */
        j_fn=n_footnote-n1_footnote+1;
        test_count=line_count+line_spacing; /* 1 tyhjÑ rivi ennen alaviitteitÑ */

        ++j; /* ohitetaan 1.alaviitettÑ edeltÑvÑ (tyhjÑ) rivi */
        uusi_viite=1;
        while (j<=j2)
            {
            if (tila==0)
                {
                edread(x,j);
                if (zs[j]==0) { strncpy(xs,space,strlen(x)); xs[strlen(x)]=EOS; }
                else          edread(xs,zs[j]);
                }
            else
                {
                uedread(x,j);
                if (uzs[j]==0) { strncpy(xs,space,strlen(x)); xs[strlen(x)]=EOS; }
                else           uedread(xs,uzs[j]);
                }            /* edread -27.9.92 */
            if (*x=='-') { ++j; continue; }
            pane_hakemistoon(x,xs);
            if (uusi_viite)
                {
                fn_replace(x,xs,1,j_fn);
                ++j_fn;
                uusi_viite=0;
                }
            if (strncmp(x+1,space,strlen(x+1))==0)
                {
                ++i_fn; if (i_fn==n1_footnote) break;
                ++j; uusi_viite=1; continue;
                }
            fn_save(x); fn_save(xs);
            test_count+=note_spacing;
            ++j;
            }

        line_count=test_count;
        return(j);
        }

static void fn_save(char *x)
        {
        char nimi[LLENGTH];

        if (!footnotes)
            {
            footnotes=1;
            strcpy(nimi,etmpd); strcat(nimi,"SURVO.FTN");  /* 22.3.91 */
            fn_file=muste_fopen(nimi,"w+t");
            if (fn_file==NULL)
                {
                sur_print("\nCannot open the footnote file SURVO.FTN!");
                WAIT; return; // RS CHA exit(1);
                }
            }
        fprintf(fn_file,"%s\n",x);
        }

static int fn_tulosta()
        {
        int i;
        char x[LLENGTH],xs[LLENGTH];
        char *p;
        double vline_spacing;

        if (!footnotes) return(1);
        rewind(fn_file);
        line_count=0.0;

        vline_spacing=line_spacing; line_spacing=note_spacing;
        strcpy(x,"[NOTE_START]");
        i=lue_koodit(x); if (i<0) return(1);
        if (pr_type==1) send("footnote_bar\n");

        uusi_rivi();
        while (!feof(fn_file))
            {
            fn_load(x); if (feof(fn_file)) break; fn_load(xs);
            if (strncmp(xs,space,strlen(x))==0) *xs=EOS;
            p=x;
            if (*p=='-')
                {
                i=lue_koodit(x+1); if (i<0) return(-1);
                continue;
                }

            if ( pitch_unit && (line_length || *tabrivi) )
                {
                char varatab=*tabrivi;
                if (*p=='U') { *tabrivi=EOS; i=tulosta(x,xs); *tabrivi=varatab; }
                else                         i=ptulosta(x,xs);
                if (i<0) return(-1);
                }
            else tulosta(x,xs);
            }
        muste_fclose(fn_file);
        footnotes=0;
        strcpy(x,"[NOTE_END]");
        i=lue_koodit(x); if (i<0) return(1);
        line_spacing=vline_spacing;
        return(1);
        }

static void fn_load(char *x)
        {
        char *p;

        p=x;
        while (1)
            {
            if (feof(fn_file)) break;
            *p=(unsigned char)getc(fn_file);
            if (*p=='\n') break;
            ++p;
            }
        *p=EOS;
        }

static void set_footnote_number(char *s)
        {
        n_footnote=atoi(s)-1;
        }


static int print_index(char *x,char **sana,int n,char *rivi)
        {
        char nimi[LLENGTH];

        strcpy(nimi,sana[1]);
        if (strchr(nimi,':')==NULL && *x!='.')
            { strcpy(nimi,edisk); strcat(nimi,sana[1]); }

        index_file=muste_fopen(nimi,"w+t");
        if (index_file==NULL)
            {
            sprintf(sbuf,"\nCannot open index file %s!",nimi); sur_print(sbuf);
            WAIT; return(-1);
            }
        index_mukana=1;
        return(1);
        }

static void pane_hakemistoon(char *x,char *xs)
        {
        int i;
        char *p,*q;
        int i1,i2;
        char sana[LLENGTH],y[LLENGTH];
        char *osa[1];

        if (loppu_merkitty==1)
            {
            strcpy(y,x+1);
            i=split(y,osa,1);
            if (i)
                {
                strcpy(sana,rivin_loppu); strcat(sana,osa[0]);
                index_save(sana,page_number-1);
                }
            }
        else if (loppu_merkitty==2)
            {
            p=x+1; while (*p && *p==' ') ++p;
            q=strchr(xs+(int)(p-x),']');
            if (q!=NULL)
                {
                korvaa_varjo(xs,q);
                i=(int)(q-xs)-(int)(p-x)+1;
                strcpy(sana,rivin_loppu); strncat(sana,p,i);
                index_save(sana,page_number-1);
                }
            }
        loppu_merkitty=0;
        p=xs;
        while (p!=NULL && *p!=EOS)
            {
            p=strchr(p,'*');
            if (p==NULL) break;

            korvaa_varjo(xs,p);

            i2=p-xs;
            while (i2>0) { if (x[i2]!=' ') break; --i2; }
            if (i2==0) { ++p; continue; }
            while (x[i2]!=EOS) { if (x[i2]==' ') break; ++i2; }
            --i2;
            i1=i2;
            while (i1>0) { if (x[i1]==' ') break; --i1; }
            ++i1;
            strncpy(sana,x+i1,i2-i1+1); sana[i2-i1+1]=EOS;
            if (empty(x+i2+1) && x[i2]=='-')
                {
                sana[i2-i1]=EOS; strcpy(rivin_loppu,sana);
                loppu_merkitty=1; break;
                }
            index_save(sana,page_number-1);
            ++p;
            }
        p=xs;
        while (p!=NULL && *p!=EOS)
            {
            p=strchr(p,'[');
            if (p==NULL) break;
            korvaa_varjo(xs,p);
            q=strchr(p,']');
            if (q==NULL)
                {
                strcpy(rivin_loppu,x+(p-xs));
                i=strlen(rivin_loppu)-1;
                if (rivin_loppu[i]=='-') rivin_loppu[i]=EOS;
                while (i && rivin_loppu[i-1]==' ') { --i; rivin_loppu[i]=EOS; }
                if (rivin_loppu[i-1]=='-') rivin_loppu[i-1]=EOS;
                else { rivin_loppu[i]=' '; rivin_loppu[i+1]=EOS; }
                loppu_merkitty=2; break;
                }
            korvaa_varjo(xs,q);
            strncpy(sana,x+(p-xs),(int)(q-p+1));
            sana[q-p+1]=EOS;
            index_save(sana,page_number-1);
            p=q;
            }
        }

static void korvaa_varjo(char *xs,char *p)
        {
        if (p>xs+1 && *(p-1)!=' ') *p=*(p-1);
        else if (*(p+1)!=EOS) *p=*(p+1);
        else *p=' ';
        }

static void index_save(char *sana,int n)
        {
        char *p;

        if (!index_mukana) return;
        p=sana; while (*p) { if (*p==' ') *p='_'; ++p; }
        fprintf(index_file,"%4.4d %s\n",n,sana);
        }


static void init_gchars()
        {
        int i;

        n_gchar=0;
        for (i=0; i<255; ++i) { t_gchar[i]=EOS; gmerkit[i]=EOS; }
        }

static int gchars(char *x,char **sana,int n,char *rivi)   /* gchar <merkki> <koodisana> */
                        /* gchar T<merkki> <koodisana> */
        {
        int i,k;
        unsigned char varjo;    /* tÑssÑ graafinen merkki */
        char y[LLENGTH];
        char *p;
        char t_koodi;

        if (n<3) { koodivirhe(rivi); return(-1); }

        p=sana[1];
        if (*p=='T') { t_koodi='T'; ++p; } else t_koodi=' ';

        i=strlen(p);
        if (i==1) varjo=*p;
        else { i=muunna(p,y); if (i<0) return(-1); varjo=*y; }
        if (gchar[varjo]!=NULL && len_gchar[varjo]>=strlen(sana[2]))
            strcpy(gchar[varjo],sana[2]);
        else
            {
            len_gchar[varjo]=strlen(sana[2]);
            pr_test(len_gchar[varjo]);
            gchar[varjo]=pr_osoitin; strcpy(pr_osoitin,sana[2]);
            pr_osoitin+=len_gchar[varjo]+1;
            }
        if (!t_gchar[varjo]) { gmerkit[n_gchar]=varjo; ++n_gchar; }
        t_gchar[varjo]=t_koodi;
        gchar_on=1;
        return(1);
        }

static void gchar_erotus(unsigned char *x)
        {
        int i,h;
        int len;

        if (!gchar_on) return;
        gchar_ind=0;
        len=strlen(x);
        for (i=0; i<len; ++i)
            {
            gcharx[i]=' ';
            if (strchr(gmerkit,x[i])==NULL) continue;
            gcharx[i]=x[i]; x[i]=' ';
            vpitch_unit=0;
            if (*tabrivi==EOS)
                {
                for (h=0; h<len; ++h) tabrivi[h]=' '; tabrivi[len]=EOS;
                if (ps_charwidth==0.0) ps_charwidth=0.6*font_size;
                if (pitch_unit==0) { vpitch_unit=1; pitch_unit=1; }
                }
            if (t_gchar[gcharx[i]]=='T') tabrivi[i]='T';
            ++gchar_ind;
            }
        gcharx[len]=EOS;
/*
  printf("\nx   :%.70s\nxgr :%.70s\ntab :%.70s",x,gcharx,tabrivi); getch();
*/

        }

static void gchar_tulosta()
        {
        int i;
        char y[LLENGTH],yy[LLENGTH];
        char *p;

        if (!gchar_on) return;
        ps_kirjoita("currentpoint \n");
        sprintf(y,"/chx %g def /chy lsp def\n",ps_charwidth); ps_kirjoita(y);
        for (i=0; i<strlen(gcharx); ++i)
            {
            if (gcharx[i]!=' ')
                {
                ps_kirjoita("cpl\n");
                strcpy(y,gchar[gcharx[i]]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            ps_kirjoita("csp\n");
            }
        ps_kirjoita("moveto\n");
        if (vpitch_unit) pitch_unit=0;
        }


static int ps_autocad()  /* autocad <ps_file>,<x_home>,<y_home>,<x_scale>,<y_scale> */
              /*                                *=automatic      */
        {
        int i;
        char y[LLENGTH];

        sprintf(y,"/autocdict 300 dict def autocdict begin\n"); send(y);

        i=etsi("%%BoundingBox",1); if (i<0) return(-1);
        muste_fclose(canon_file);
        send("\nend grestore\n");
        return(1);
        }


static int ps_file()
        {
        int i;
        char y[LLENGTH];

        sprintf(y,"/filedict 300 dict def filedict begin\n"); send(y);
        send("/showpage { 1 pop } def\n");
        send("/quit { 1 pop } def\n");
        etsi2("\n",0);
        etsi2("%%Ei_mitÑÑn",1);
        muste_fclose(canon_file);
        send("\nend grestore\n");
        return(1);
        }

static int ps_epsfile()
        {
        int i;
        char y[LLENGTH];

        send("/BEGINEPSFILE {\n");
        send(" /EPSFsave save def\n");
      send(" 0 setgray 0 setlinecap 1 setlinewidth 0 setlinejoin 10 setmiterlimit [] 0 setdash\n");
        send(" newpath\n");
        send(" /showpage {} def\n");
        send("} bind def\n");
        send("/ENDEPSFILE {\n");
        send(" EPSFsave restore\n");
        send("} bind def\n");
        send("BEGINEPSFILE\n");
        etsi2("%%Ei_mitÑÑn",1);  /* kopioi loppuun */
        muste_fclose(canon_file);
        send("ENDEPSFILE\n");
        send("\ngrestore\n");
        return(1);
        }

static int etsi2(char *s,int kopio)
        {
        char ch;
        char *t;

        while (1)
            {
            t=s;
            ch=(unsigned char)merkki();
            if (feof(canon_file)) return(1);
            if (ch=='\4') return(1);
            if (kopio) putc((int)ch,kirjoitin);
            if (ch!=*s) continue;
            while (1)
                {
                ++t; if (*t==EOS) break;
                ch=(unsigned char)merkki();
                if (feof(canon_file)) return(1);
                if (ch!=*t) { ungetc((int)ch,canon_file); break; }
                if (kopio) putc((int)ch,kirjoitin);
                }
            if (*t==EOS) break;
            }
        return(1);
        }

/*
_PRINT+PR2+PRC+PRM+PRI+PRS+PRL+PRTX+PRPICT+
PRT+PRR+ROMAN+PRF+PRPS+PRPS2+PRFN+PRIX+PRGR+PRAUTO
*/
