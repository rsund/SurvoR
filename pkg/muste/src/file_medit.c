#include "muste.h"
/* medit.c 30.4.2003/SM (30.4.2003)
   FILE MEDIT <Survo data file>,<medit_field>:<medit_list>
*/

// show_page return -1
// select_next_page return -1
// class_funtion return -1
// n_cases_function return -1
// mean_function return -1
// stddev_function return -1
// corr_function return -1
// min_function return -1
// max_function return -1
// key_special_medit return -1
// check_strarvot return -1
// make_pages return -1
// get_format return -1

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"


#define MAX_PAGES 1000
#define MAX_FIELDS 200
#define MAX_CLASSIF 100
#define MAXARG 10
#define EARG '\376'
#define MAXEARG 255
#define NDATA 12


static SURVO_DATA d;
static FILE *medit_temp;
static long dat_n;
static char data_name[LNAME];
static char edit_name[LNAME];
static char list_name[LNAME];

static char pages_line[LLENGTH];
static char *page_name[MAX_PAGES];
static char namelist[10*LLENGTH];
static int n_pages;
static int j_page;
static int page_start[MAX_PAGES];
static long jnro;
static long n0;
static int page_nro;
static int uusi; // sivu tms.
static int current_field;
static int saa_kirjoittaa=0;
static int press_tab=0;

static int page_by_pg_keys=0; // 30.9.2003
static int pg_key=0;

static char rivivarjo=' ';
static int new_page_wait=0;
static int suunta=1; // 1=eteen 2=taakse (ehdollinen show_page() tarvitsee!)
static char xss[LLENGTH]; // 26.7.2003 kenttien varjomerkit talteen

static long chk_jnro;
static int chk_page_nro; // RS REM , age_nro;
static int chk_current_field;
static int time_var; // 17.2.2005  MEDIT_TIME="time_var"

#define OPTIONS 0
#define RECORD  1
#define CHECK   2
#define MEDIT_VARS 3
#define MEDIT_SAVE 4
#define INDATA 5
#define MEDIT_TIME 6

#define N_MEDIT_SPEC 7

extern char **spa, **spb, **spb2;
extern double *arvo;
extern long jnro;
extern int insert_mode;

static int earg_varattu=0;
static double *earg;
static int nvar;
static int n_earg=0;
static int spn_order;
extern int spn;
extern char *splist;
extern char *spl;

static char spec_string[N_MEDIT_SPEC][LNAME];
static char sdat_list[LLENGTH];

static int n_fields; // per sivu
static int field_line[MAX_FIELDS],field_column[MAX_FIELDS],field_len[MAX_FIELDS];
static int field_var[MAX_FIELDS];
static char field_value[MAX_FIELDS][LNAME]; // 11.6.2003
static char field_format[MAX_FIELDS][LNAME]; // 27.6.2003

// johdetut kentÑt
static int current_field2=-1;
static int n_fields2;
static int field_line2[MAX_FIELDS],field_column2[MAX_FIELDS],field_len2[MAX_FIELDS];
static int field_spec2[MAX_FIELDS];
static char field_name2[MAX_FIELDS][32];
static char default_format[64]; // 28.9.2003 johdettuja kenttiÑ #- varten

static long last_shown_jnro; // 10.7.2003
static int last_shown_page_nro;

static int old_field;
static char old_value[LLENGTH];

static double *min,*max; // 22.6.2003
static int *strarvo;  // sallittujen arvojen []-lettelon alkukohta varname:ssa
static int editing=0;
static char pref=' ';
static int new_case=0;
static char sound_name[LNAME];
static int no_new_values=0;
static int kieli;
static int pilkku_pisteeksi=0;
static char medit_condition[LNAME]; // 4.7.2003
static char medit_sucro[LNAME];     // 4.7.2003
static int toinen_survo_toiminnassa=0;

static int next_pages_line=0; // 18.10.2003

static char class_string[LLENGTH];
static int n_classif;
static char classif_name[MAX_CLASSIF][32];
static int classif_start[MAX_CLASSIF];

static int check_on=0;
static int *check_id;  // havainnon tunnistetiedot (ei tarkisteta)
static int check_var;      // tark.muuttuja, johon merkitÑÑn tarkistus tehdyksi
static int checked_case=0; // tark.muuttujan arvo (0=havaintoa ei tarkistettu)
static int *checked_var;   // havainnon sisÑllÑ: onko muuttuja tarkistettu

static int stat=0;
static long *n_mean;
static double *sum,*sum2;
static double *minv,*maxv;

static long ref_jnro=0L;
static int ref_page,ref_field;
static int ref_r,ref_c;

static char save_edt[LNAME]; // 2.11.2003
static FILE *savefile;
static int next_line;
static int n_shad_lines; // 10.11.2003
static int savepage=0;
static char rivin_loppu2[]="\15\12";

static char stripe[LLENGTH];
static char varjo4[LNAME];
static char varjo7[LNAME];
static char varjop[LNAME];

static int lag=0; // 14.9.2003
static int read_line; // 24.11.2003

extern int special;
static int l_virhe;
extern int r_mouse,c_mouse;
extern int m_double_click;
extern int right_mouse_click;



#define N_SOUNDS 8
static char medit_sound[N_SOUNDS][32];
static int sound_on=0;

static int already_open=0;
static struct tm *aika;

static int m;
static long n;
static int jnro0;

static int var;
static int rivi, ensrivi; // ??
// RS REM static int sortvar;
static int n_haku;
static int jatkuva_haku;
// RS REM static char tiedotus[LLENGTH]; /* ohjeteksti alarivillÑ */
// RS REM static char lopetus[LLENGTH]; // ??
static char hakutieto[LLENGTH];
static char hakuavain[64];
// RS REM static int ordind;
static int not_found;
static int search_on=0;
static int search_on2=0; // 22.6.2003
static int field2_haku=0;
static int pikahaku=0; // 1=haun kÑynnistys promptissa suoraan alt-F5-napilla

static unsigned char code[256];
static int koodit=0;

static int str_var,str_lag,str_var_start,str_var_len;
// RS REM static char str_vasen[LLENGTH];
static int code_ind=0;

static SURVO_DATA sd[NDATA];
static int nvar,pvar[EP4];
// RS REM static char lauseke[LLENGTH];
static long jnro;
static int lag;
static int vm_act; /* ennen uusien muuttujien perustamista */
static int ndata;
/* int sdata;  */
static char sdat_list[LLENGTH], *sdat[NDATA];
static long sup_jnro[NDATA]; // 24.11.2003 ->SD.C

int str_muunnos=0;
int first_new_var=0; /* 18.3.92 */

#define N_SOFT 12
static int n_soft_keys=N_SOFT;
static int miss_wait=TRUE; // RS ADD

//                  0         1         2         3         4         5          6       7         8          9        10           11
//
static char *soft_key[]={ "NEXT P", "PREV P", "NEXT C", "PREV C", "NEXT D", "PREV D",  "LAST", "HOME",   "SEARCH",  "SOUND",  "SAVE",      " EXIT " };
static int soft_start[]={ 1,         8,        15,       22,       29,       36,        43,     48,       53,        60,      66,          71       };
static int soft_len[]={   6,         6,        6,        6,        6,        6,         4,      4,        6,         5,       4,           6        };
static int soft_code[]={ CODE_NEXT, CODE_PREV, 145,      146,      149,      150,       -3,   CODE_HOME,  CODE_SRCH, 1001, CODE_SUCRO1,  CODE_EXIT  };

static char *soft_message[2][N_SOFT]=
  {
  { // RS ADD
  "Seuraava sivu (PgDn)",
  "Edellinen sivu (PgUp)",
  "Seuraava havainto (ctrl-PgDn)",
  "Edellinen havainto (ctrl-PgUp)",
  "Seuraava \"eriarvoinen\" havainto (alt-PgDn)",
  "Edellinen \"eriarvoinen\" havainto (alt-PgUp)",
  "Viimeiseen havaintoon (F2 down)",
  "Askeltaen alkuunpain! (HOME)",
  "Etsi havaintoa! (alt-F5)",
  "Merkkiaanet paalle/pois! (F2 F7)",
  "Talleta tama sivu toimituskenttaan! (F11)",
  "Lopeta katselu, tallennus ja editointi! (F8)",
  }, { // RS ADD

  "Next page (PgDn)",
  "Previous page (PgUp)",
  "Next case (ctrl-PgDn)",
  "Previous case (ctrl-PgUp)",
  "Next case with a different value (alt-PgDn)",
  "Previous case with a different value (alt-PgUp)",
  "To the last case! (F2 down)",
  "Stepwise to the beginning! (HOME)",
  "Searching for the cases (alt-F5)",
  "Sound signals on/off! (F2 F7)",
  "Save this page in an edit file! (F11)",
  "Stop using FILE MEDIT! (F8)"
  } // RS ADD
  };


static int current_soft_key=-1;
static int global=0;

extern int n_select;
extern int *sel_var;
extern char *sel_type; /* 0=ind 1=cases */
extern char *sel_rel;  /* * tai + */
extern double *sel_lower,*sel_upper;
extern char **sel_cases;
extern char **sel_lastcase;
extern char *sel_neg;
extern char cases_space; // 2.1.2003

// *
extern int n_cases_wild; // 8.1.2003  init to 0
extern char cases_wild;
// ?
extern int n_cases_wild2; // 9.1.2003 init to 0
extern char cases_wild2;

static int soft_bottom_line_erase_medit();
static int tilanvaraus();
static int not_enough_space();
static int read_medit_spec();
static int etsi_alkuhav(long *pjnro);
static int check_set(char *x);
static int field_update1();
static int field_update();
static int check_compare(int field,char *new);
static int find_pages();
static int show_page();
static int show_page2();
static int write_string_s1(char *x,int len,char shadow,int row,int col,int save_on,char *saveb,char *savebs);
static int space_shad(char *s);
static int lopeta_toinen_survo();
// RS REM static int poista_kuvat();
static int sanoma_toiselle_survolle(char *sanoma);
static int check_page_line(char *s,char *key,char *value);
static int fill_fields(char *x,char *xs,int riv);
static int set_format(char *format,int var);
static int n_of_chars(char ch,char *x,int len);
static int fill_free_text(char *x);
// RS REM static int fill_free_text2(char *x);
static int read_var(int var,char *format,char *s);
static int read_var_free(int var,char *s,double *pvalue);
static int find_field_by_mouse();
static int replace_value_by_mouse_word();
static int goto_next_field();
static int goto_previous_field();
static int select_next_page();
static int next_ehto(char *s);
static int new_jnro(int k);
static int goto_next_field2();
static int create_new_case();
static int goto_last_case();
static int start_editing();
static int delete_char();
static int erase_chars();
static int field_down();
static int field_up();
static int find_classifications();
static int class_function(char *lauseke);
static int classif_error(char *s);
static int order_function(char *name,double *py);
static int pvm();
static int day_function(char *name,double *py);
static int month_function(char *name,double *py);
static int year_function(char *name,double *py);
static int weekday_function(char *name,double *py);
static int update_time_var();
static int n_cases_function(char *name,double *py);
static int mean_function(char *name,double *py);
static int stddev_function(char *name,double *py);
static int corr_function(char *names,double *py);
static int var_nro(char *name);
static int min_function(char *name,double *py);
static int max_function(char *name,double *py);
static int alusta_stat_tilat();
static int replace_medit_vars(char *p);
static int medit_replace(char *s,char *t);
static int key_special_medit(int m);
static int haku(int k,int jatkuva);
static int save_open();
static int save_page();
static int save_pages();
static int medit_save_end();
static int key_common(int m);
static int ei_saa_editoida();
static int prefix();
static int n_update(SURVO_DATA_FILE *s,long n);
static int medit_cls();
static void sur_set_medit_focus();
static int varinfo();
static int hae_muoto(SURVO_DATA_FILE *d,int i,char *muoto);
static int default_form(SURVO_DATA_FILE *d,int i,char *muoto);
static int rajat(int i,double *pmin,double *pmax);
static int strarvot(int i,int *pi);
static int check_limits(int muuttuja,double x);
static int check_strarvot(int muuttuja,char *edsana);
static int find_sounds();
static int play_sound(int i);
static int play_sound_by_name(char *s,int rr,int cc);
static int play_sound2(char *s);
static int make_pages(char *s0);
static int get_format(char type,char *x,int var,char *format);
static int etsi();
static int osoita(int var) { return(var); }
static int disp_nimi() { return(1); }
static int disp_recs(int jnro) { return(jnro); }
static int putsaa() { soft_bottom_line_erase_medit(); return(1); }
static int etsi_maxmin(int var,int mm);
static int relaatio(char *s,char *prel,char *arvo);
static int poimi(long j,int i,char *sana);
static int float4_muunto(float f,char *sana);
static int laske(char *lauseke,double *y);
static double luku(char *sana,int len);
static double oper(double x1,double x2,char laji);
static void supista(int *t,double opnd[],char op[],int v[]);      
static double funktio(char *s,double x);  
static double mfunktio(char *s,double *x,int n);
static void arg_virhe(char *s);
static void syntax_error(char *s);
static int laske2(char *muuttuja,double *y);
static int parsplit(char *x,char **a,char **b,int max);
static int arifor(char *lauseke,double *y);
static int varif(char *lauseke,double *y);
static void if_syntax_error(char *x);
static int f_edit(char *s,double *x,int n,double *py);
static void korvaa(char *s,char *x,char *y);
static int varaa_earg();
static int aseta_earg(double luku,char *sana);
static int lue_arvot(long j);
static int lag_arvo(char *muuttuja,double *y);
static int sp2_init();
static int sp_write(char *nimi,double y);
static int medit_tut_end();
static int pos_funktio(char *s,double *y);
static int sulku_split(char *x,char **osa,int n,int *pk);
static void not_string(char *s);
static int tutki_str_lauseke(char *x,int *pvar,int *plag,int *pstart,int *plen,int *pk);
static int str_laske(char *lauseke,char *tulos);
static int strvert(char *a,char rel,char *b,char *c,char *dd,double *y);
static int str_arvo(char *a,char *s);
static int load_codes(char *codefile,unsigned char *code);
static void conv(unsigned char *sana);
static int sup_arvo(char *muuttuja,char *s);
static int sup_arvo_double(char *muuttuja,double *y);
static void var2();
static void spec_error();
static void poista_var();
static int update_specs();
static int var_error(char *s);
static int varfind2_medit(SURVO_DATA *d,char *nimi,int virheilm);
int headline_medit();
static int nextch_medit();
static int is_special(int m);
static int nextkey();
static int soft_keys_init_medit();
int mouse_medit_functions(int c_mouse,int r_mouse,int m_click,int m_double_click);
static void cursor_medit(unsigned int r,unsigned int c);
static int conditions_medit(SURVO_DATA *d,char *xx);
static int sp_init_medit(int lin,int m);
static int spread2_medit(int lin,int *raja1);
static int spread3_medit(char *x,int j);
static int varnimet_medit();
static int instr_medit(char s[],char c[]);

extern int tutch();
extern void tutsave();
extern int edsave();
//extern int sel_free();
extern int muste_fclose2();
extern int nextkey2_medit();
extern int bool_norm();
extern int sel_virhe();
extern int tilavirhe();
extern int find_cond();


int muste_file_medit(int argc, char *argv[])
        {
        int i,j;
        int m;
        char *s[3];
        char *p;
        char x[LLENGTH];
        char x2[LLENGTH];
        int alkuhav;

// RS Variable init
saa_kirjoittaa=0;
press_tab=0;
page_by_pg_keys=0;
pg_key=0;
new_page_wait=0;
suunta=1;
earg_varattu=0;
earg=NULL;
nvar=0;
n_earg=0;
n_fields=0;
current_field2=-1;
n_fields2=0;
last_shown_jnro=0;
last_shown_page_nro=0;
old_field=0;
min=NULL;
max=NULL;
strarvo=NULL;  
editing=0;
new_case=0;
no_new_values=0;
pilkku_pisteeksi=0;
toinen_survo_toiminnassa=0;
next_pages_line=0;
check_on=0;
check_id=NULL; 
check_var=0;    
checked_case=0; 
checked_var=NULL;  
stat=0;
n_mean=NULL;
sum=NULL;
sum2=NULL;
minv=NULL;
maxv=NULL;
ref_jnro=0;
savepage=0;
lag=0;
sound_on=0;
already_open=0;
search_on=0;
search_on2=0; 
field2_haku=0;
pikahaku=0;
koodit=0;
code_ind=0;
str_muunnos=0;
first_new_var=0;
n_soft_keys=N_SOFT;
current_soft_key=-1;
global=0;
n_cases_wild=0;
n_cases_wild2=0;

 miss_wait=TRUE;

sel_var=NULL;
sel_type=NULL;
sel_rel=NULL;
sel_lower=NULL;
sel_upper=NULL;
sel_cases=NULL;
sel_lastcase=NULL;
sel_neg=NULL;

        if (argc==1) return(-1);

        s_init(argv[1]);
        tut_init();
        for (i=0; i<LLENGTH; ++i) { space[i]=' ', stripe[i]=(char)STRIPE; }
        for (i=0; i<LNAME; ++i)
            { varjo4[i]='4'; varjo7[i]='7'; varjop[i]=','; }
        kieli=tell_language(); // 1=suomi 2=English
//Rprintf("\nkieli=%d|",kieli);

//Rprintf("\ninfo: %s",info);
        i=split(info,s,3);
        if (strcmp(s[1],"NULL")==0 && strcmp(s[2],"NULL")==0)
            {
            i=make_pages(s[0]); if (i<0) return(-1);
            split(info,s,3);
            }
        strcpy(data_name,s[0]);
        strcpy(edit_name,s[1]);
        strcpy(list_name,s[2]);
//Rprintf("\ndata_name=%s list_name=%s|",data_name,list_name);

        alkuhav=0; // 7.6.2003
        i=strlen(data_name);
        p=data_name+i-1;
        if (*p=='-') { alkuhav=1; *p=EOS; }
        if (*p=='+') { alkuhav=2; *p=EOS; }

        sprintf(sbuf,"MEDIT:%s",list_name);
        for (j=1; j<=r2; ++j)
            {
            edread(x,j); i=split(x+1,s,1);
            if (i && strcmp(sbuf,s[0])==0) break;
            }
        if (j>r2)
            {
            if (kieli==1)
                sprintf(x,"\n%s ei loydy toimituskentasta %s!",sbuf,edit_name);
            else
                sprintf(x,"\n%s not found in edit field %s!",sbuf,edit_name);
            PR_EBLK; sur_print(x); WAIT; return(0);
            }
        j_page=j;
        edread(x,j);
// Rprintf("\n%d: %s",j,x+1);
        strcpy(x2,x);
        strcpy(pages_line,x);
        p=strstr(x2+1,"SIZE=");
        if (p!=NULL)
            {
            split(p+5,s,2);
            r3=atoi(s[0])+3;
            c3=atoi(s[1])+1;
            }
// RS FIXME NYI        set_console_io();

        sprintf(sbuf,"SURVO MM:   FILE  MEDIT  %s  %s : %s",data_name,edit_name,list_name);
        sur_set_console_title(sbuf);
        medit_cls();

        soft_keys_init_medit();
        sprintf(sbuf,"%sMEDITSUC.TXT",etmpd); // varmistus! 13.7.2003
        if (sur_file_exists(sbuf)==1)
            sur_delete1(sbuf);

        i=data_open2(data_name,&d,1,1,0); if (i<0) return(0);

        if (d.type!=2)
            {
            PR_EBLK;
            if (kieli==1)
                sur_print("\nVain Survon datatiedostot sallittuja!");
            else
                sur_print("\nOnly Survo data files permitted!");

            WAIT; return(0);
            }

        read_medit_spec();
        p=spec_string[OPTIONS];
        if (*p)
            {
            if (strchr(p,'W')!=NULL) saa_kirjoittaa=1;
            if (strchr(p,'I')!=NULL) { insert_mode=1; CURSOR_INS; }
            if (strchr(p,'C')!=NULL) pilkku_pisteeksi=1;
            if (strchr(p,'S')!=NULL) sound_on=1;
            if (strchr(p,'T')!=NULL) press_tab=1;
            if (strchr(p,'P')!=NULL) page_by_pg_keys=1;
            }
// Rprintf("\nn=%ld|",d.n); getck();

        p=spec_string[MEDIT_VARS];
        i=replace_medit_vars(p); if (i<0) return(0); 
        p=spec_string[INDATA];
        strcpy(sdat_list,p);
        i=tilanvaraus(); if (i<0) return(0);

        i=varinfo(); if (i<0) return(0);
        i=find_pages(); if (i<0) return(0);
        i=find_sounds(); if (i<0) return(0);
        i=find_classifications(); if (i<0) return(0);
        r1=c1=1;

        var2(); // RS CHA i=var2(); if (i<0) return(0);
          

        i=spfind("FORMAT"); // 28.9.2003 johdettuja kenttia #- varten
        if (i<0) strcpy(default_format,"###.##");
        else strcpy(default_format,spb[i]);

        p=spec_string[CHECK];
        if (*p)
            {
            i=check_set(p);
            if (i<0) return(0);
            }

        r=0; c=0;
        jnro=0;
        new_jnro(1); // 12.6.2003
        n0=dat_n=d.n;  // RS 28.3.2013 moved from after etsi_alkuhav        
        if (alkuhav==1) jnro=d.n;
        if (alkuhav==2) { jnro=d.n+1L; no_new_values=0; create_new_case(); uusi=1; }

        etsi_alkuhav(&jnro);
    
        headline_medit();

        if (d.n==0L)
            {
            no_new_values=0;
            create_new_case();
            }
        page_nro=0;        
        r=field_line[0]; c=field_column[0];
        current_field=0;      
        cursor_medit(r,c);
        r_mouse=c_mouse=0;
        sur_flush_input();
        uusi=1;
        checked_case=0;

        strcpy(x,spec_string[MEDIT_SAVE]); // 4.11.2003
        i=split(x,s,2);
        if (i==2 && muste_strcmpi(s[1],"ALL")==0)
            {
            save_pages();
            return(0);
            }

        strcpy(x,spec_string[MEDIT_TIME]); // 17.2.2005
        time_var=varfind2_medit(&d,x,0);

        while (1)
            {
            if (!search_on) current_field2=-1; // 6.6.2003

            if (r_mouse>0 && c_mouse>=0)
                {
                cursor_medit(r,c);
                find_field_by_mouse();
                r_mouse=c_mouse=0;
                }
            if (editing && current_field!=old_field)
                {
                field_update();
                editing=0;
                }

            cursor_medit(r,c);

            if (uusi)
                {
                update_specs();
                i=show_page(); if (i<0) return(-1);
                headline_medit(); // 25.5.2003

                if (uusi==1)
                    {
                    if (n_fields)
                        {
                        r=field_line[0]; c=field_column[0];
                        cursor_medit(r,c);
                        }
                    current_field=0;
                    }

                uusi=0;
                }
            if (n_fields==0)
                    { r=1; c=0; cursor_medit(r,c); }        
            m=nextch_medit(); if (m==-1) continue;
            if (current_field==-1) current_field=old_field;

            if (special)
                {
                i=key_special_medit(m); if (i<0) return(-1);
                if (m!=CODE_SRCH) search_on=search_on2=0;
                }
            else
                {
                if (n_fields) key_common(m);
                search_on=search_on2=0;
                }
            }
        data_close(&d); // RS ADD   
        return(1);    
        }

static int tilanvaraus()
    {

    min=(double *)muste_malloc(d.m*sizeof(double));
    if (min==NULL) { not_enough_space(); return(-1); }
    max=(double *)muste_malloc(d.m*sizeof(double));
    if (max==NULL) { not_enough_space(); return(-1); }
    strarvo=(int *)muste_malloc(d.m*sizeof(int *));
    if (max==NULL) { not_enough_space(); return(-1); }
    check_id=(int *)muste_malloc(d.m*sizeof(int *));
    if (check_id==NULL) { not_enough_space(); return(-1); }
    checked_var=(int *)muste_malloc(d.m*sizeof(int *));
    if (checked_var==NULL) { not_enough_space(); return(-1); }

    return(1);
    }

static int not_enough_space()
    {
    PR_EBLK; sur_print("\nNot enough space (FILE MEDIT)!"); WAIT;
    return(1);
    }

static int read_medit_spec()
    {
    int i,k;
    char *p;

    sprintf(sbuf,"%sSURVOMSP",etmpd);
    medit_temp=muste_fopen2(sbuf,"rt");
    for (i=0; i<N_MEDIT_SPEC; ++i)
        {
        fgets(sbuf,LNAME,medit_temp);
        k=strlen(sbuf)-1; sbuf[k]=EOS;
        p=strchr(sbuf,'=');
        if (p==NULL) *spec_string[i]=EOS;
        else strcpy(spec_string[i],p+1);
// Rprintf("\nspec=%s|",spec_string[i]); getck();
        }

    muste_fclose2(medit_temp);
    return(1);
    }

static int etsi_alkuhav(long *pjnro)
    {
    int i,len;
    char *p,*q;
    long nro;

    nro=0L;
    p=spec_string[RECORD];
    len=strlen(p);
    for (i=0; i<len; ++i)
        if (strchr(":=><",p[i])!=NULL) break;
    if (i==len) q=NULL;
    else q=p+i;

    if (q==NULL)
        {
        nro=atol(p);
        if (nro<1L && nro>d.n+1L) nro=0L;
        }
    else
        {
        strcpy(hakuavain,q);
//      if (*q==':') { *hakuavain='='; strcpy(hakuavain+1,q+1); }
        if (*q==':') strcpy(hakuavain,q+1);
        *q=EOS;
        i=varfind2_medit(&d,p,0);
        if (i<0) return(1);
        current_field=0; field_var[0]=i;
        etsi();
        search_on=search_on2=0;
        *hakuavain=EOS;
        return(1);
        }
    if (nro>0L) *pjnro=nro;
    return(1);
    }

static int check_set(char *x)
    {
    int i,k,var;
    char *s[EP4];

    for (i=0; i<d.m; ++i) check_id[i]=0;
    k=split(x,s,EP4);
    for (i=0; i<k; ++i)
        {
        var=varfind2_medit(&d,s[i],0);
        if (var<0)
            {
          if (kieli==1)
            sprintf(sbuf,"\nCHECK-muuttujaa %s ei loydy datatiedostosta!",
                               s[i]);
          else
            sprintf(sbuf,"\nCHECK field %s not available in the data file!",
                               s[i]);
            cursor_medit(1,0); PR_EBLK; sur_print(sbuf); WAIT; return(-1);
            }
        else if (i==k-1) { check_var=var; check_id[var]=1; }
                                          // 14.6.2003
        else
            {
            check_id[var]=1;
            d.d2.vartype[var][2]='P';
            }
        }
    check_on=1;
    return(1);
    }

static int field_update1()
        {
        int i;
        old_field=current_field; i=field_update();
        return(i);
        }

static int field_update()
    {
    int var0;
    int i,len;
    double a;

    if (n_fields==0) return(1);
    if (check_on && field_var[old_field]!=check_var)
        {
        i=check_compare(old_field,field_value[old_field]);
        if (i<0) return(-1);
        }

// Rprintf("\njnro=%ld uusi_arvo=%s|",jnro,field_value[old_field]); getck();
    var0=field_var[old_field];
    if (stat)
        {
        if (strcmp(old_value,field_value[old_field])!=0)
            {
            if (strncmp(old_value,space,field_len[old_field])==0)
                {
                ++n_mean[var0];
                a=atof(field_value[old_field]);
                sum[var0]+=a; sum2[var0]+=a*a;
                if (minv[var0]==MISSING8)
                    { minv[var0]=maxv[var0]=a; }
                if (a<minv[var0]) minv[var0]=a;
                if (a>maxv[var0]) maxv[var0]=a;
                }
            else
                {
       if (strncmp(field_value[old_field],space,field_len[old_field])==0)
                    {
                    --n_mean[var0];
                    a=atof(old_value);
                    sum[var0]-=a; sum2[var0]-=a*a;
                    }
       else
                    {
                    a=atof(old_value);
                    sum[var0]-=a; sum2[var0]-=a*a;
                    a=atof(field_value[old_field]);
                    sum[var0]+=a; sum2[var0]+=a*a;
                    if (minv[var0]==MISSING8)
                        { minv[var0]=maxv[var0]=a; }
                    if (a<minv[var0]) minv[var0]=a;
                    if (a>maxv[var0]) maxv[var0]=a;
                    }
                }
            }
        } // stat

    if (d.vartype[var0][0]=='S')
        {
        i=check_strarvot(var0,field_value[old_field]);
        if (i<0) return(-1);
        fi_alpha_save(&d.d2,jnro,var0,field_value[old_field]);
        }
    else
        {
        len=field_len[old_field];
        for (i=0; i<len; ++i)
            if (field_value[old_field][i]!=' ') break;
        if (i==len) fi_miss_save(&d.d2,jnro,var0);
        else
            {
            a=atof(field_value[old_field]);
            i=check_limits(var0,a);
            if (i<0)
                {
                return(-1);
                }
            data_save(&d,jnro,var0,atof(field_value[old_field]));
            }
        }
    strcpy(old_value,field_value[old_field]); // 6.7.2003
    update_specs();
    if (time_var>=0) update_time_var(); // 17.2.2005
    i=show_page(); if (i<0) return(-1);
    uusi=0;
    editing=0; // 21.9.2003
    return(1);
    }

static int check_compare(int field,char *new)
    {
    int i,var;
    char var_name[9];
    char type;
    int ok;
    char vastaus[2];
    double v,u;

    var=field_var[field];
    type=d.vartype[var][0];
    ok=1;

    switch (type)
        {
      case 'S':
          if (strcmp(old_value,new)!=0) ok=0;
          break;
      case '1':
      case '2':
          if (atoi(old_value)!=atoi(new)) ok=0;
          break;
      case '4':
      case '8':
          v=atof(old_value); u=atof(new);
          if (v==0.0 && u==0.0) break;
          if (v==0.0) { ok=0; break; }
          if (u==0.0) { ok=0; break; }
          if (fabs(u-v)/(fabs(u)+fabs(v))>1e-7) ok=0;
          break;
        }

    if (ok) return(1);

    play_sound(6);
    editing=0;
    *var_name=EOS; strncat(var_name,d.varname[var],8);
    i=7; while (var_name[i]==' ') var_name[i--]=EOS;
    if (kieli==1)
        sprintf(sbuf,"\n%s: Vanha=%s Uusi=%s",var_name,old_value,new);
    else
        sprintf(sbuf,"\n%s: Old=%s New=%s",var_name,old_value,new);

    cursor_medit(r,0); PR_EIN2; sur_print(sbuf);

    if (kieli==1)
        {
       *vastaus='K'; vastaus[1]=EOS;
        i=prompt(" Kelpaako Uusi (K/E)? ",vastaus,1);
        if (strchr("Kk",*vastaus)!=NULL) return(1);
        }
    else
        {
       *vastaus='Y'; vastaus[1]=EOS;
        i=prompt(" Is 'New' OK (Y/N)? ",vastaus,1);
        if (strchr("Yy",*vastaus)!=NULL) return(1);
        }

    strcpy(new,old_value);
    jnro=chk_jnro;
    page_nro=chk_page_nro;
    current_field=chk_current_field;
    update_specs();
    i=show_page(); if (i<0) return(-1);

    return(-1);
    }

static int find_pages()
    {
    int i,j,k;
    char *p;
    char x[LLENGTH],*s[2];
    char name[LNAME];

    p=strstr(pages_line,"PAGES=");
    if (p==NULL)
        {
        PR_EBLK;
        if (kieli==1)
            sur_print("\nPAGES=p1,p2,... maarittely puuttuu!");
        else
            sur_print("\nPAGES=p1,p2,... definition missing!");
        WAIT; return(-1);
        }
//  n_pages=split(p+6,page_name,MAX_PAGES);

    strcpy(namelist,p+6);
    p=namelist+strlen(namelist)-1; while (*p==' ') --p; ++p; *p=EOS;
    --p; if (*p=='&') *p=EOS;
    j=j_page+1;
    while(1)
        {
        edread(x,j);
        if (strncmp(x+1,space,c2-1)==0) break;
        p=x+strlen(x)-1; while (*p==' ') --p; ++p; *p=EOS;
        --p; if (*p=='&') *p=EOS;
        p=x+1; while (*p==' ') ++p;
        strcat(namelist,p);
        ++j;
        }
// Rprintf("\nnamelist=%s|",namelist); getck();
    n_pages=split(namelist,page_name,MAX_PAGES);

// Rprintf("\n%d %s %s %s",n_pages,page_name[0],page_name[1],page_name[2]);
// getck();

    for (k=0; k<n_pages; ++k)
        {
        strcpy(name,page_name[k]); strcat(name,":");
        for (j=1; j<ed2; ++j)
            {
            edread(x,j);
            i=split(x+1,s,2);
            if (strcmp(s[0],"PAGE")!=0) continue;
            if (i>1) if (strcmp(s[1],name)==0) break; // RS 28.1.2013 if i>1
            }
        if (j==ed2)
            {
            if (kieli==1)
                sprintf(sbuf,"\nPAGE %s: ei loydy!",page_name[k]);
            else
                sprintf(sbuf,"\nPAGE %s: not found!",page_name[k]);
            PR_EBLK; sur_print(sbuf); WAIT; return(-1);
            }
// Rprintf("\npage %s = %d|",page_name[k],j); getck();
        page_start[k]=j;
        }
    return(1);
    }

static int show_page()
    {
    int i;
    int start;
    char x[LLENGTH]; // RS REM ,x2[LLENGTH];
    char *p;
    char *s[1];

// Page <name>: CONDITION:<select_condition> SUCRO:<name>



    while (1)
        {
        start=page_start[page_nro];
        edread(x,start); // 21.6.2003
        p=strchr(x+1,':');
        ++p;
        *medit_condition=EOS;
        *medit_sucro=EOS;
        check_page_line(p,"CONDITION:",medit_condition);
        check_page_line(p,"SUCRO:",medit_sucro);

        if (*medit_condition==EOS && *medit_sucro==EOS) // 10.7.2003
            {
            i=split(p,s,1);
            if (i==1) strcpy(medit_condition,s[0]);
            }
        if (*medit_condition==EOS) break;
//         Rprintf("\nehto: %s|",medit_condition);
// RS REM        sel_free();
        i=conditions_medit(&d,medit_condition);
        if (i<0) return(0);
//         Rprintf("\ncond=%d",i);
        i=unsuitable(&d,jnro);
//         Rprintf("\nok=%d",1-i);
        if (i==0) break;

// Rprintf("\nNot suitable!"); getck();


        if (suunta==1)
            {
            ++page_nro;
            if (page_nro==n_pages)
                {
                play_sound(2);
                if (jnro<d.n)
                    {
                    new_jnro(1); page_nro=0;
                    update_specs();
                    }
                else
                    {
                    jnro=last_shown_jnro; // 10.7.2003
                    page_nro=last_shown_page_nro;
//                  create_new_case();
                    }
                }
            }
        else
            {
            --page_nro;
            if (page_nro<0)
                {
                play_sound(2);
                if (jnro>1L)
                    {
                    new_jnro(-1); page_nro=n_pages-1;
                    update_specs();
                    }
                else
                    {
// ????????????
                    PR_EIN2;
                    sur_print("Invalid condition for the first page!");
                    WAIT; return(-1);
                    }
                }

            }
        } // while
    return(show_page2());
    }

static int show_page2()
    {
    int riv,riv2,i,k;
    int start;
    char x[LLENGTH],x2[LLENGTH];
    char xs[LLENGTH];
    char *s[1];
    char varjo;
    int v;
    int sucro,sucro2;
// RS REM    char *p,*q;
    int save_on;
    char saveb[LLENGTH];
    char savebs[LLENGTH];


// Rprintf("\nshow_page2:");
    start=page_start[page_nro];
    next_pages_line=0;

    n_fields=0;
    n_fields2=0; // 3.6.2003
    riv=riv2=0;

    sucro=sucro2=0;
    if (*medit_sucro!=EOS)
        {
        sucro=1;
        sprintf(sbuf,"%sMEDITSUC.TXT",etmpd);

        while (1) // 12.7.2003
            {
            if (!sur_file_exists(sbuf)) break;
            sur_delete_files(sbuf); // RS ADD
            sur_sleep(200L);
            }
            
        medit_temp=muste_fopen(sbuf,"w+t");
if (medit_temp==NULL) { sur_print("\nFILE MEDIT fopen1 error!"); getck(); return(-1); }
        }
    while (1)
        {
        ++riv; ++riv2;
        read_line=start+riv; edread(x,read_line); strcpy(x2,x);
//      24.11.2003 (read_line)
        i=split(x2+1,s,1); // 4.7.2003
        if (i==1 && strncmp(s[0],"<START>",7)==0) { ++sucro; sucro2=1; }

        if (i==1 && strncmp(s[0],"<NEXT>",6)==0)
            {
            next_pages_line=start+riv;
            last_shown_jnro=jnro;
            last_shown_page_nro=page_nro;

            return(1);
            }

        strcpy(x2,x);

        i=zs[start+riv];
        if (i) { edread(xs,i); rivivarjo=*xs; }
        else { *xs=EOS; strncat(xs,space,c3); }
        strcpy(xss,xs); // 26.7.2003
// Rprintf("\n%.50s",x+1);
        i=split(x+1,s,1);
        if (i==1 && strcmp(s[0],"END")==0) break; // tÑydennÑ tyhjÑt (puuttuu!)
        strcpy(x,x2);

        if (*x=='%' && !sucro)
            {
            i=split(x+1,s,1);
            v=atoi(s[0]);
            for (i=0; i<v; ++i)
                {
                write_string(space,c3,' ',riv2+1,0); // 22.6.2003
                ++riv2;
                }
            --riv2;
            continue;
            }
        else i=fill_fields(x,xs,riv2);
        if (i<0) { --riv2; continue; }
        if (i==2) ++riv; // luettu %1-rivi, jonka loppu seuraavalla

        save_on=0; if (savepage && sucro<=1) save_on=1; // 2.11.2003
        if (save_on)
            {
            strcpy(saveb,x+1);
            if (*xs==EOS) *savebs=EOS;
            else { *savebs='*'; savebs[1]=EOS; }
            }

        if (sucro>1) // 4.7.2003
            fprintf(medit_temp,"%s\n",x+1);
        else if (rivivarjo!=' ') // 9.6.2003
            { write_string(x+1,c3-1,rivivarjo,riv2+1,1); rivivarjo=' '; }
        else
            {
            varjo=xs[1]; v=1; k=0;
            *saveb=EOS;
            for (i=1; i<=c3; ++i)
                {
                if (xs[i]==varjo) continue;
                if (varjo=='+')
                    write_string_s1(space,i-v,' ',riv2+1,v+k,save_on,saveb,savebs);
                else if (varjo=='-')
                    k-=i-v;
                else
                    write_string_s1(x+v,i-v,varjo,riv2+1,v+k,save_on,saveb,savebs);
                varjo=xs[i]; v=i;
                }
            write_string_s1(x+v,c3-v,varjo,riv2+1,v+k,save_on,saveb,savebs);
            }

        if (save_on)
            {
            i=strlen(saveb)-1; while (i>=0 && saveb[i]==' ') saveb[i--]=EOS;
            fprintf(savefile,"%d|*%s%s",next_line++,saveb,rivin_loppu2);
            if (*savebs && !space_shad(savebs+1))
                {
                fprintf(savefile,"S    | %s%s",savebs+1,rivin_loppu2);
                ++n_shad_lines;
                }
            }

        if (*sound_name) play_sound_by_name(sound_name,riv2,0);
        }


    if (*medit_sucro!=EOS)
        {
        muste_fclose(medit_temp);
        if (sucro2==0)
            {
            sprintf(sbuf,"\nData lines <START> not given for sucro %s!",
                                              medit_sucro);
            PR_EIN2; sur_print(sbuf); WAIT; return(-1);
            }
        }

    if (sucro>1 && sucro2)
        {
        if (!toinen_survo_toiminnassa)
            {
            sprintf(sbuf,"%sS.EXE",survo_path);
            sur_flush_input();
            sur_sleep(200L);
            sur_set_console_title("SURVO MM");
muste_fixme("\nFIXME: Second Muste launch not implemented in FILE MEDIT yet!");            
// RS FIXME NYI            spawnl(P_NOWAIT,sbuf,sbuf,medit_sucro,NULL);
//          sur_sleep(200L);
            sur_sleep(1000L); // 3000L -20.7.2003
            sur_set_medit_focus();
            toinen_survo_toiminnassa=1;
            }
        sur_sleep(100L); // oli 400L -12.7.2003
        }
    last_shown_jnro=jnro;
    last_shown_page_nro=page_nro;
    return(1);
    }

static int write_string_s1(
char *x,
int len,
char shadow,
int row,
int col,
int save_on,
char *saveb,
char *savebs
)
    {
    int i;
    char vs[LLENGTH];

    write_string(x,len,shadow,row,col);
    if (save_on)
        {
        strncat(saveb,x,len);
        if (*savebs!=EOS)
            {
            for (i=0; i<len; ++i) vs[i]=shadow;
            strncat(savebs,vs,len);
            }
        }
    return(1);
    }

static int space_shad(char *s)
    {
    if (strncmp(space,s,strlen(s))==0) return(1);
    return(0);
    }


static int lopeta_toinen_survo()
    {
    sanoma_toiselle_survolle("<STOP>");
    sur_sleep(500L);
    sanoma_toiselle_survolle("<STOP>");
    return(1);
    }

/* RS REM
static int poista_kuvat()
    {
    sanoma_toiselle_survolle("<DEL>");
    return(1);
    }
*/

static int sanoma_toiselle_survolle(char *sanoma)
    {
    sprintf(sbuf,"%sMEDITSUC.TXT",etmpd);
    medit_temp=muste_fopen2(sbuf,"w+t");
if (medit_temp==NULL) { sur_print("\nFILE MEDIT fopen2 error"); getck(); }
    fprintf(medit_temp,"%s\n",sanoma);
    muste_fclose2(medit_temp);
    sur_sleep(200L);
    return(1);
    }

static int check_page_line(char *s,char *key,char *value)
    {
    char x[LLENGTH];
    char *p,*q;

// Rprintf("\ns=%s key=%s|",s,key); getck();
    strcpy(x,s);
    p=strstr(x,key); if (p==NULL) return(1);
    p+=strlen(key);
    q=strchr(p,' '); *q=EOS;
    strcpy(value,p);
// Rprintf("\n%s=%s|",key,value); getck();
    return(1);
    }

/**************************** "kuvasivuihin"
sprintf(sbuf,"%sS.EXE",survo_path);
sur_flush_input();
sur_sleep(200L);
spawnl(P_NOWAIT,sbuf,sbuf,example[i],NULL);
sur_sleep(200L);
***************************************/

static int fill_fields(char *x,char *xs,int riv)
    {
    int i;
    char *p,*q,*s;
    char nimi[LNAME];
    int var;
    char format[LNAME];
    double value=0.0;
    int i_spec=0;

    if (*x==':')
        {
        p=x+1;
// Rprintf("\np=%s|",p); getck();
        while (*p==' ') ++p;
        if (*p=='/' && *(p+1)==' ') return(-1);
        return(1);
        }
    p=x+1;
    while ((q=strchr(p,':'))!=NULL)
        {
        s=q-1;
        while (s>x && *s!=' ') --s;
        *nimi=EOS; strncat(nimi,s+1,q-s-1);

        if (*nimi=='%') { p=q+1; continue; }
// sivuuta merkinnÑt tyyppiÑ %1=D3:muuttuja! 23.11.2003

        if (strcmp(nimi,"|CHECK|")==0)
            {
            if (check_on) var=check_var;
            else return(-1);
            }
        else var=varfind2_medit(&d,nimi,0);
        if (var<0)
            {
            i=spfind(nimi);
            if (i>=0)
                {
                strcpy(sbuf,spb[i]);
                var=varfind2_medit(&d,sbuf,0);
                }
            if (var<0)
                {
                i_spec=i;
                strcpy(field_name2[n_fields2],nimi);
           *class_string=EOS;
                laske(nimi,&value);
            if (*class_string) value=MISSING8;

                if (l_virhe)
                  {
                  var_error(nimi);
                  value=MISSING8; update_specs(); l_virhe=0;
                  }
                }
            }

        ++q;
        while (*q==' ') ++q;
        if (var>=0)
            {
            field_var[n_fields]=var;
            field_line[n_fields]=riv;
            field_column[n_fields]=q-x-1-n_of_chars('-',xs+1,q-x-1);
            }
        else
            {
            field_spec2[n_fields2]=i_spec;
            field_line2[n_fields2]=riv;
            field_column2[n_fields2]=q-x-1-n_of_chars('-',xs+1,q-x-1);
            }

        s=q;
        while (*s!=' ') ++s;

        *format=EOS; strncat(format,q,s-q);

        if (format[0]=='-') // 28.9.2003
            {
//  Rprintf("\nformat=%s|",format); getck();
            set_format(format,var);
            s=q+strlen(format);
            }

        if (var>=0)
            {
            strcpy(field_format[n_fields],format); // 27.6.2003
            strncpy(xs+(int)(q-x),varjo7,(int)(s-q));
            read_var(var,format,sbuf);
            strcpy(field_value[n_fields],sbuf);
            if (xss[(int)(q-x)]=='P') // 26.7.2003
                d.d2.vartype[field_var[n_fields]][2]='P';
            }
        else
            {
            if (*class_string)
                strncpy(xs+(int)(q-x),varjop,(int)(s-q));
            else
                strncpy(xs+(int)(q-x),varjo4,(int)(s-q));

            if (value==MISSING8)
                {
                *sbuf=EOS;
                strncpy(sbuf,space,strlen(format));
              if (*class_string)
                  {
//       Rprintf("\nstr:%s| len=%d",class_string,strlen(format)); getck();
                  strncpy(sbuf,class_string,strlen(class_string));
                  *class_string=EOS;
                  }
                }
            else fconv(value,format,sbuf);

// Rprintf("\n%s|",sbuf); getck();
            }
        if (!check_on || (check_on && checked_case) )
            strncpy(q,sbuf,strlen(format));
        else if (var>=0 && (!check_id[var] && !checked_var[var]))
            strncpy(q,space,strlen(format));
        else
            strncpy(q,sbuf,strlen(format));

        if (var>=0)
            {
            field_len[n_fields]=strlen(format);
            ++n_fields;
            }
        else
            {
            field_len2[n_fields2]=strlen(format);
            ++n_fields2;
            }

//      p=q+1;
        p=q+strlen(format);  // koe 12.12.2003
        }


    p=strchr(x,'%'); q=strstr(x," / ");
    if (p!=NULL && q!=NULL) 
    	{ 
    	fill_free_text(x); 
    	return(1);
    	}

    q=strstr(x," & ");
    if (p!=NULL && q!=NULL)
        {                    // TehdÑÑn jatko seuraavasta rivistÑ!
        *(q+1)='/'; *(q+3)=EOS;

        edread(sbuf,read_line+1);

        i=strlen(sbuf)-1; while (i>1 && sbuf[i]==' ') sbuf[i--]=EOS;
        strcat(x,sbuf+1);
        fill_free_text(x);
        return(2); // 2=jatkorivi
        }
    return(1);
    }

static int set_format(char *format,int var) // 28.9.2003
    {

//  strcpy(format,"####"); // kokeilu vain!

    if (var>=0)
        {
        hae_muoto(&d.d2,var,format);
        }
    else
        {
        strcpy(format,default_format);
        }

// Rprintf("\nmuoto=%s|",format); getck();
    return(1);
    }

static int n_of_chars(char ch,char *x,int len)
    {
    int i,n;
    n=0;
    for (i=0; i<len; ++i) if (x[i]==ch) ++n;
    return(n);
    }

static int fill_free_text(char *x)
    {
    int i;
    char *p,*q;
    char y[LLENGTH]; // %1=VÑestî %2=Ala
    char *sy[10];
    int ny;
    int var;
    char x2[LLENGTH];
    double value;
    char ch;
    char format[LNAME];
    int len;
    char sbuf2[LLENGTH];
    int str_lauseke=0;

    p=strstr(x," / "); if (p==NULL) return(1);
    *p=EOS; strcpy(y,p+3);

    *sound_name=EOS;
    q=strstr(y,"SOUND:");
    if (q!=NULL)
        {
        q+=6; i=strlen(q)-1; while(q[i]==' ') q[i--]=EOS;
//      play_sound_by_name(q);
        strcpy(sound_name,q);
        }

    ny=splitp(y,sy,10);  // ei jakoa sulkujen sisÑltÑ!
    for (i=0; i<ny; ++i)
        {
        p=strchr(sy[i],'='); if (p==NULL) continue;
        *p=EOS;
// 14.9.2003
        q=strchr(p+1,'[');
        if (q!=NULL && q[strlen(q)-1]==']') { *q=EOS; lag=atoi(q+1); }

        var=varfind2_medit(&d,p+1,0);

        if (var<0)
            {
            *class_string=EOS;

            if (strstr(p+1,")&")!=NULL || strstr(p+1,"\"&")!=NULL)
                {
                str_laske(p+1,sbuf2);
                str_lauseke=1;
                value=MISSING8;
// Rprintf("sbuf2=%s|",sbuf2); getck();
                }
            else if (*(p+1)=='<') // %1=<DATA> vain toistaiseksi!
                {
//              Rprintf("\np+1=%s|",p+1); getck();
                if (data_name[1]==':')
                    strcpy(sbuf2,data_name);
                else { strcpy(sbuf2,edisk); strcat(sbuf2,data_name); }
                str_lauseke=1;
                }
            else
                {
//    Rprintf("\nlaskettava %s|",p+1); getck();

                if (*(p+1)=='D' && (*(p+2)==':' || *(p+3)==':') )
                              // muutettu    >              >
                        {
                        sup_arvo(p+1,sbuf2);
                        str_lauseke=1;
                        }

                else
                    laske(p+1,&value);
                }
//    Rprintf("\nkoodi=%s %s=%g|",sy[i],p+1,value); getck();
            if (l_virhe) { var_error(p+1); value=MISSING8; update_specs(); l_virhe=0; }
            if (value==MISSING8)
               {
               if (!str_lauseke)
                   strcpy(sbuf2,"?");
               }
//          else fconv(value,"",sbuf2);
            else if (!str_lauseke) fconv(value,"",sbuf2); // 7.3.2005
            if (*class_string!=EOS)
                {
                strcpy(sbuf2,class_string); *class_string=EOS;
                }
            }
        else
            read_var_free(var,sbuf2,&value);
            
        strcpy(x2,x);
        p=strstr(x2,sy[i]); if (p==NULL) continue;
        *p=EOS;
        len=strlen(sy[i]);

        ch=*sy[i];
        if (ch!='%' && strlen(sy[i])==1 && value!=MISSING8)
            {
            q=p; *q=ch;

            while (*q==ch || *q=='.')
                { if (*q==ch) *q='#'; ++q; }

            *format=EOS; strncat(format,p,q-p);
// Rprintf("\nformat=%s|",format); getck();
            fconv(value,format,sbuf2);
            len=strlen(sbuf2);
            *p=EOS;
            }
        strcpy(x,x2); strcat(x,sbuf2);
        strcat(x,p+len);
        }
    x[c3]=EOS;
    i=strlen(x); if (i<c3) strncat(x,space,c3-i);

    return(1);
    }

/* RS REM
static int fill_free_text2(char *x) // koe 26.2.2005
    {
    int i;
    char *p,*q;
    char y[LLENGTH]; // %1=VÑestî %2=Ala
    char *sy[10];
    int ny;
    int var;
    char x2[LLENGTH];
    double value;
    char ch;
    char format[LNAME];
    int len;
    char sbuf2[LLENGTH];
    int str_lauseke=0;

    p=strstr(x," / "); if (p==NULL) return(1);
    *p=EOS; strcpy(y,p+3);

    *sound_name=EOS;
    q=strstr(y,"SOUND:");
    if (q!=NULL)
        {
        q+=6; i=strlen(q)-1; while(q[i]==' ') q[i--]=EOS;
//      play_sound_by_name(q);
        strcpy(sound_name,q);
        }

    ny=splitp(y,sy,10);  // ei jakoa sulkujen sisÑltÑ!
    for (i=0; i<ny; ++i)
        {
        p=strchr(sy[i],'='); if (p==NULL) continue;
        *p=EOS;
// 14.9.2003
        q=strchr(p+1,'[');
        if (q!=NULL && q[strlen(q)-1]==']') { *q=EOS; lag=atoi(q+1); }

        var=varfind2_medit(&d,p+1,0);

        if (var<0)
            {
            *class_string=EOS;

            if (strstr(p+1,")&")!=NULL || strstr(p+1,"\"&")!=NULL)
                {
                str_laske(p+1,sbuf2);
                str_lauseke=1;
                value=MISSING8;
// Rprintf("sbuf2=%s|",sbuf2); getck();
                }
            else if (*(p+1)=='<') // %1=<DATA> vain toistaiseksi!
                {
//              Rprintf("\np+1=%s|",p+1); getck();
                if (data_name[1]==':')
                    strcpy(sbuf2,data_name);
                else { strcpy(sbuf2,edisk); strcat(sbuf2,data_name); }
                str_lauseke=1;
                }
            else
                {
//    Rprintf("\nlaskettava %s|",p+1); getck();

                if (*(p+1)=='D' && (*(p+2)==':' || *(p+3)==':') )
                              // muutettu    >              >
                        {
           //           sup_arvo(p+1,&value);

                        sup_arvo(p+1,sbuf2);
                        str_lauseke=1;
           //       Rprintf("\nvalue=%g|",value); getck();
                        }

                else
                    laske(p+1,&value);
                }
//    Rprintf("\nkoodi=%s %s=%g|",sy[i],p+1,value); getck();
            if (l_virhe) { var_error(p+1); value=MISSING8; update_specs(); l_virhe=0; }
            if (value==MISSING8)
               {
               if (!str_lauseke)
                   strcpy(sbuf2,"?");
               }
            else fconv(value,"",sbuf2);
            if (*class_string!=EOS)
                {
                strcpy(sbuf2,class_string); *class_string=EOS;
                }
            }
        else
            read_var_free(var,sbuf2,&value);
        strcpy(x2,x);
        p=strstr(x2,sy[i]); if (p==NULL) continue;
        *p=EOS;
        len=strlen(sy[i]);

        ch=*sy[i];
        if (ch!='%' && strlen(sy[i])==1 && value!=MISSING8)
            {
            q=p; *q=ch;

            while (*q==ch || *q=='.')
                { if (*q==ch) *q='#'; ++q; }

            *format=EOS; strncat(format,p,q-p);
// Rprintf("\nformat=%s|",format); getck();
            fconv(value,format,sbuf2);
            len=strlen(sbuf2);
            *p=EOS;
            }
        strcpy(x,x2); strcat(x,sbuf2);
        strcat(x,p+len);
        }
    x[c3]=EOS;
    i=strlen(x); if (i<c3) strncat(x,space,c3-i);

    return(1);
    }
*/

static int read_var(int var,char *format,char *s)
    {
    int i;
    char x[LLENGTH];
    int lenf,lenx;
    double a;


// Rprintf("\ntype=%s|",d.vartype[var]); getck();
    lenf=strlen(format);
    if (d.vartype[var][0]=='S')
        {
        data_alpha_load(&d,jnro,var,x);
        lenx=strlen(x);
        if (lenx<lenf)
            {
            strcpy(s,x); strncat(s,space,lenf-lenx);
            }
        else
            {
            *s=EOS; strncat(s,x,lenf);
            }
        }
    else
        {
        data_load(&d,jnro,var,&a);
        for (i=0; i<lenf; ++i) if (format[i]=='#') format[i]='1';
        if (a==MISSING8)
            { *s=EOS; strncat(s,space,lenf); }
        else
            fconv(a,format,s);
        }
    return(1);
    }

static int read_var_free(int var,char *s,double *pvalue)
    {
    int i;
// RS REM    char x[LLENGTH];
    double a;

    if (d.vartype[var][0]=='S')
        {
        data_alpha_load(&d,jnro+(long)lag,var,s); lag=0;
        i=strlen(s)-1; while (s[i]==' ') s[i--]=EOS;
        return(1);
        }
    else
        {
        data_load(&d,jnro+(long)lag,var,&a); lag=0;
        *pvalue=a; // 17.5.2003
        if (a==MISSING8) strcpy(s,"?");
        else fconv(a,"",s);
        }
    return(1);
    }

static int find_field_by_mouse()
    {
    int i;
    int rr,cc;


    field2_haku=0;
    rr=r_mouse; cc=c_mouse;
    for (i=0; i<n_fields; ++i)
        {
        if (rr!=field_line[i]) continue;
        if (cc>=field_column[i] && cc<field_column[i]+field_len[i]) break;
        }
    if (i==n_fields)
        {
        if (n_fields2 && right_mouse_click)
            {
            for (i=0; i<n_fields2; ++i)
                {
                if (rr!=field_line2[i]) continue;
                if (cc>=field_column2[i] && cc<field_column2[i]+field_len2[i]) break;
                }
            if (i<n_fields2)
                {
                if (editing) field_update();
                r=rr; c=cc;
                cursor_medit(r,c);
                old_field=current_field;
                current_field2=i;
                if (pref==' ') haku(0,0);
                else haku(1,1);
                cursor_medit(r,c);
                return(1);
                }
            }
        r=field_line[current_field]; c=field_column[current_field];
        return(1);
        }
    else
        {
        r=rr; c=cc;
        current_field=i;
        if (right_mouse_click)
            {
            if (pref==' ') haku(0,0);
            else haku(1,1);
            }
        }
    current_field=i;
    return(1);
    }

static int replace_value_by_mouse_word()
    {
// RS REM    int i;
    char x[LLENGTH];
    char *p,*q;

    if (ei_saa_editoida()) return(1);

    read_string(x,NULL,c3,r_mouse+1,1); // 22.11.2003
//     jotta %1,%2,..-merkintîjÑ sisÑltÑvÑt rivit osataan lukea!
//     edread(x,page_start[page_nro]+r_mouse);
//     Rprintf("\nx=%s|",x); getck();
    p=x+c_mouse;
//     Rprintf("\np=%s",p); getck();
    if (*p==' ') return(1);
    while (p>x && *p!=' ') --p;
    ++p; q=p;
    while (*q!=' ') ++q;
    *q=EOS;

    editing=1;
    old_field=current_field;
    strcpy(old_value,field_value[old_field]);

    strcpy(field_value[current_field],p);

    old_field=current_field; // 21.11.2003

    field_update();

    sur_sleep(500L);   // 18.6.2003
    goto_next_field();

    return(1);
    }

static int goto_next_field()
    {
    int i;
    int snd; // sound already played

    if (editing)
        {
        i=field_update();
        if (i<0) return(1);
        }
    snd=0;

    i=pg_key; pg_key=0;
    if (page_by_pg_keys && !i && (current_field==n_fields-1 || n_fields==0))
        return(1);
    ++current_field;

    if (current_field==n_fields || n_fields==0)
        {
        if (new_page_wait) // 18.6.2003
            {
            sur_sleep(500L);
            new_page_wait=0;
            }

        if (next_pages_line)
            {
            page_nro=select_next_page();
            if (page_nro<0) return(-1);
            }
        else ++page_nro;
        
        if (page_nro<n_pages)
            {
            update_specs(); // 2.6.2003
            i=show_page(); if (i<0) return(-1);
            headline_medit();
            }
        if (page_nro==n_pages)
            {
            if (no_new_values && jnro>=d.n) // RS 28.3.2013
                if (ei_saa_editoida()) 
                    {
                    uusi=0;
                    jnro=last_shown_jnro;
                    page_nro=last_shown_page_nro;
                    current_field=0;
                    r=field_line[current_field];
                    c=field_column[current_field];
                    r_mouse=c_mouse=0;
                    return(-1);
                    } 
            
            uusi=1;
            play_sound(2); snd=1;
            if (jnro<d.n) { new_jnro(1); page_nro=0; }
            else 
                { 
                no_new_values=0; // RS 28.3.2013
                create_new_case();
                } 
            }
        current_field=0;
        if (!snd) { play_sound(1); snd=1; }
        }
    if (!snd) play_sound(0);
    r=field_line[current_field];
    c=field_column[current_field];
    r_mouse=c_mouse=0;
    return(1);
    }

static int goto_previous_field()
    {
    if (current_field==0) return(1);
    --current_field;
    r=field_line[current_field];
    c=field_column[current_field]+field_len[current_field]-1;
    r_mouse=c_mouse=0;
    return(1);
    }

static int select_next_page()
    {
    int i,k,row;
    char *s[3];
    char x[LLENGTH];
    char cond[LNAME];
    int comment;
    char *p,*q;
    int comment_line=0;

    update_specs();
    row=next_pages_line;

    edread(x,row);
    i=split(x+1,s,2);
    if (i==2) comment_line=atoi(s[1]);

    while (1)
        {
        ++row;
        comment=0;
        edread(x,row);
        i=split(x+1,s,3);
        if (i==3 && strcmp(s[2],"/")!=0) comment=s[2]-x;

// Rprintf("\ns: %s %s|",s[0],s[1]); getck();

        if (muste_strcmpi(s[0],"END")==0) break;

        i=next_ehto(s[0]);
        if (i<0)
            { // SELECT-tyyppinen
            strcpy(cond,s[0]);
// RS REM            sel_free();
            conditions_medit(&d,cond);
            if (unsuitable(&d,jnro)) continue;
            }
        else if (i==0) continue;

        if (muste_strcmpi(s[1],"NEXT_CASE")==0) // 20.10.2003
            {
            page_nro=n_pages;
            return(page_nro);
            }
// Rprintf("\ncond: %s pages=%d| ",s[0],n_pages); getck();
        for (i=0; i<n_pages; ++i)
            {
// Rprintf("\nvert: %s %s|",page_name[i],s[1]); getck();
            if (strcmp(page_name[i],s[1])==0)
              {
              if (comment)
                {
                edread(x,row);
                p=strstr(x,"SOUND:");
                if (p!=NULL)
                    {
                    q=strchr(p,' ');
                    if (q!=NULL) *q=EOS;
                    play_sound2(p+6);
                    }
                p=x+comment;
                q=strstr(p," / "); if (q!=NULL) *q=EOS;
                k=strlen(p)-1;
                while (p[k]==' ') p[k--]=EOS;
                sprintf(sbuf,"\n%s",p);
                if (comment_line) cursor_medit(comment_line,1);
                PR_EIN2; sur_print(sbuf);
//              WAIT;
                sur_print("\nPress any key!"); // 27.2.2005
                sur_getch(); // RS CHA nextch();
                }
            return(i);
              }
            }
        sprintf(sbuf,"\nPage %s (given in <NEXT>) not found!",s[1]);
        sur_print(sbuf); getck();
        return(-1);
        }
    ++page_nro;
    return(page_nro);
    }


static int next_ehto(char *s)
    {
    char *p1,*p2,*p3;
    int i,i1,i2,i3;
    char eq='=';
    char ch;
    double a1,a2;

    int rel;
    char ehto_vas[LNAME],ehto_oik[LNAME];

    i1=99; p1=strchr(s,eq); if (p1!=NULL) i1=p1-s;
    i2=99; p2=strchr(s,'>'); if (p2!=NULL) i2=p2-s;
    i3=99; p3=strchr(s,'<'); if (p3!=NULL) i3=p3-s;

    i=i1; if (i2<i) i=i2; if (i3<i) i=i3;
    if (i==99) return(-1);

    ch=s[i];
    if (ch==eq) rel=1;
    else if (ch=='>') { if (s[i+1]==eq) rel=4; else rel=2; }
    else { // ch='<';
         if (s[i+1]==eq) rel=5;
         else if (s[i+1]=='>') rel=6; else rel=3;
         }
    s[i]=EOS;
    strcpy(ehto_vas,s);
    if (rel>3) ++i;
    strcpy(ehto_oik,s+i+1);

// Rprintf("\nehto_vas: %s rel: %d ehto_oik: %s",ehto_vas,rel,ehto_oik);
    laske(ehto_vas,&a1);
    if (a1==MISSING8) return(0);
    laske(ehto_oik,&a2);
    if (a2==MISSING8) return(0);  

    switch (rel)
        {
      case 1: if (a1==a2) return(1); break;
      case 2: if (a1>a2) return(1); break;
      case 3: if (a1<a2) return(1); break;
      case 4: if (a1>=a2) return(1); break;
      case 5: if (a1<=a2) return(1); break;
      case 6: if (a1!=a2) return(1); break;
        }
    return(0);
    }


static int new_jnro(int k)
    {
    int i;
    double a;

// Rprintf("\nnew_case=%d|",new_case); getck();
    if (k==1) ++jnro; else --jnro;

//  Poistettu 13.7.2003: Hidastaa toimintaa!
//  if (toinen_survo_toiminnassa) poista_kuvat(); // 5.7.2003


    if (!check_on) return(1);
    checked_var[check_var]=1;
    data_load(&d,jnro,check_var,&a);
    if (a!=0.0) checked_case=1;
    else
        {
        checked_case=0;
        for (i=0; i<d.m; ++i) checked_var[i]=0;
        checked_var[check_var]=1;
        }
    return(1);
    }

static int goto_next_field2()
    {
    int i;
    int lin=0,col=0;

// Rprintf("r=%d c=%d|",r,c); getck();

    if (!n_fields2) return(1);
    for (i=0; i<n_fields2; ++i)
        {
        lin=field_line2[i]; if (lin<r) continue;
        col=field_column2[i];
        if (lin==r && col<=c) continue;
        else break;
        }
    if (i==n_fields2) return(1);
// Rprintf("\ni=%d|",i); getck();
    current_field2=i;
    field2_haku=1;
    r=lin; c=col;
    return(1);
    }

static int create_new_case()
    {
    if (no_new_values) return(1);

    fi_miss_obs(&d.d2,d.n+1L);
    jnro=dat_n=d.n=d.n+1;
    page_nro=0;
    new_case=1;
    editing=0;
    no_new_values=1;
    n_update(&d.d2,d.n);
    
    return(1);
    }

static int goto_last_case()
    {
    int i;
    
    if (editing) field_update();
//  field_update1();
//  uusi=1;
    jnro=d.n;
    page_nro=0;

    update_specs();
    i=show_page(); if (i<0) return(-1);

    return(1);
    }

static int start_editing()
    {
    char *p;

    no_new_values=0;
    editing=1;
    old_field=current_field;
    strcpy(old_value,field_value[old_field]);
    if (check_on) // 12.6 koe!
        {
        if (!checked_var[field_var[old_field]])
            {
            p=field_value[old_field]; *p=EOS;
            strncat(p,space,field_len[old_field]);
            }
        checked_var[field_var[old_field]]=1;

        chk_jnro=jnro;
        chk_page_nro=page_nro;
        chk_current_field=current_field;

        }
    return(1);
    }

static int delete_char()
    {
// RS REM    char *p;
    int i; // RS REM ,k;

    if (!editing) start_editing();
    strcpy(sbuf,field_value[current_field]);
    i=c-field_column[current_field];

// Rprintf("\n%c|",sbuf[i]); getck();
    strcpy(sbuf+i,field_value[current_field]+i+1);
    PR_EIN2; sur_print(sbuf+i); sur_print(" ");
    sbuf[field_len[current_field]-1]=' ';
    strcpy(field_value[current_field],sbuf);
    c_mouse=c; r_mouse=r;

    return(1);
    }

static int erase_chars()
    {
    int i;

    if (!editing) start_editing();
    i=c-field_column[current_field];
    *sbuf=EOS; strncat(sbuf,space,field_len[current_field]-i);
    strcpy(field_value[current_field]+i,sbuf);
    PR_EIN2; sur_print(sbuf);

    return(1);
    }


static int field_down()
    {
    int lin;
    int i;
    int d1,d2;
    int dist_min,i_dist;

//  field_update1();

    for (i=current_field+1; i<n_fields; ++i)
        {
        if (field_line[i]==field_line[current_field]) continue;
        if (c>=field_column[i] && c<field_column[i]+field_len[i])
            break;
        lin=field_line[i];
        d1=c-field_column[i]; if (d1<0) d1=-d1;
        d2=c-field_column[i]-field_len[i]+1; if (d2<0) d2=-d2;
        if (d2<d1) d1=d2;
        i_dist=i; dist_min=d1; ++i;
        while (i<n_fields && field_line[i]==lin)
            {
            d1=c-field_column[i]; if (d1<0) d1=-d1;
            d2=c-field_column[i]-field_len[i]+1; if (d2<0) d2=-d2;
            if (d2<d1) d1=d2;
            if (d1<dist_min) { dist_min=d1; i_dist=i; }
            ++i;
            }
        i=i_dist; break;
        }
    if (i==n_fields) return(1);
    play_sound(0);
    current_field=i;
    c=field_column[i];
    r=field_line[i];
    return(1);
    }

static int field_up()
    {
    int lin;
    int i;
    int d1,d2;
    int dist_min,i_dist;

//  field_update1();

    for (i=current_field-1; i>=0; --i)
        {
        if (field_line[i]==field_line[current_field]) continue;
        if (c>=field_column[i] && c<field_column[i]+field_len[i])
            break;
        lin=field_line[i];
        d1=c-field_column[i]; if (d1<0) d1=-d1;
        d2=c-field_column[i]-field_len[i]+1; if (d2<0) d2=-d2;
        if (d2<d1) d1=d2;
        i_dist=i; dist_min=d1; --i;
        while (i>=0 && field_line[i]==lin)
            {
            d1=c-field_column[i]; if (d1<0) d1=-d1;
            d2=c-field_column[i]-field_len[i]+1; if (d2<0) d2=-d2;
            if (d2<d1) d1=d2;
            if (d1<dist_min) { dist_min=d1; i_dist=i; }
            --i;
            }
        i=i_dist; break;
        }
    if (i<0) return(1);
    play_sound(0);
    current_field=i;
    c=field_column[i];
    r=field_line[i];
    return(1);
    }

static int find_classifications()
    {
    int i,j;
    char x[LLENGTH],*s[2];
    char *p;

    n_classif=0;
    for (j=1; j<=r2; ++j)
        {
        edread(x,j);
        i=split(x+1,s,1);
        if (strcmp(s[0],"CLASSIFICATION")!=0) continue;
        edread(x,j);
        i=split(x+1,s,2);
        if (n_classif>=MAX_CLASSIF)
            {
            if (kieli==1)
                sprintf(sbuf,"Luokitteluja (CLASSIFICATION) enemman kuin %d!",MAX_CLASSIF);
            else
                sprintf(sbuf,"More than %d CLASSIFICATIONs!",MAX_CLASSIF);
            PR_EBLK; sur_print(sbuf); WAIT; return(-1);
            }
        i=strlen(s[1])-1;
        p=s[1]+i; if (*p==':') *p=EOS;
        strcpy(classif_name[n_classif],s[1]);
        classif_start[n_classif++]=j;
// Rprintf("\nj=%d %s|",j,s[1]); getck();
        }
    return(1);
    }

static int class_function(char *lauseke)
    {
    int i,j,j0,k,ok;
    char *p,*q;
    char *s[EP4];
    char lauseke2[LNAME];
    double cl;
    char x[LLENGTH];
    char xs[LLENGTH];
    int alpha=0;  // =1: luokittelu string-muuttujalla ei-numeerisesti
    char t0[]="";

// Rprintf("\nlauseke=%s|",lauseke+7); getck();
    strcpy(x,lauseke+7);
    i=split(x,s,2);

// Rprintf("\ns0=%s s1=%s|",s[0],s[1]); getck();
    if (i<2)
        {
        classif_error(s[0]); return(-1);
        }
    for (i=0; i<n_classif; ++i)
        {
        if (strcmp(s[0],classif_name[i])==0) break;
        }
    if (i==n_classif)
        {
        if (kieli==1)
            sprintf(sbuf,"\nLuokittelua CLASSIFICATION %s ei loydy!",s[0]);
        else
            sprintf(sbuf,"\nCLASSIFICATION %s not found!",s[0]);
        PR_EBLK; sur_print(sbuf); WAIT; return(-1);
        }
    j=j0=classif_start[i]+1;
    strcpy(lauseke2,s[1]);
    i=strlen(lauseke2); p=lauseke2+i-1;
    if (*p==')') *p=EOS;
    
    i=varfind2_medit(&d,lauseke2,0); // 17.2.2005
// Rprintf("\ni=%d|",i); getck();
//  if (i>=0) Rprintf("\nvartype=%s|",d.vartype[i]); getck();
    if (i>=0 && d.vartype[i][0]=='S') // 17.2.2005
        {
        alpha=1; data_alpha_load(&d,jnro,i,xs);
//      strcat(xs,":");
//      Rprintf("\nxs=%s|",xs); getck();
        }
    else
        {
        laske(lauseke2,&cl);
        
// Rprintf("\nluokka=%g",cl);

        if (cl==MISSING8)
            {
            alpha=1;
            cl=0;  // 28.11.2005
            strcpy(xs,"MISSING");
            }
        }

    while (1) // 22.2.2005
        {
        edread(x,j); k=split(x+1,s,EP4);
        if (k<1) { k=1; s[0]=t0; }
        if (k<2) { k=2; s[1]=t0; }
        if (k<3) { k=3; s[2]=t0; }

        if (strcmp(s[0],"END")==0)
            {
            --j; // OTHERS!
            break;
            }
//      if (strcmp(s[0],"OTHERS:")==0) break;

        if (alpha)
            {
            for (i=0; i<k-1; ++i)
                {
                p=s[i]; // '_' -> ' '
                while (1)
                    {
                    p=strchr(p,'_');
                    if (p==NULL) break;
                    *p=' ';
                    }
                p=strchr(s[i],':');
                if (p!=NULL) *p=EOS;
                ok=0; if (strcmp(xs,s[i])==0) { ok=1; break; }
                if (p!=NULL) break;
                }
            if (ok) break;
            }
        else  // numeric
            {
//          if (cl==MISSING8) break;
            p=strchr(s[2],':');
            if (strcmp(s[1],"-")==0 && p!=NULL) // vÑli a - b:
                {
                *p=EOS;
                if (cl>=atof(s[0]) && cl<=atof(s[2])) break;
                }
            for (i=0; i<k-1; ++i)
                {
                p=strchr(s[i],':');
                if (p!=NULL) *p=EOS;
                ok=0; if (cl==atof(s[i])) { ok=1; break; }
                }
            if (ok) break;
            }
        ++j;
        } // while
    edread(x,j);

    if (zs[j]!=0) { edread(xs,zs[j]); rivivarjo=*xs; } // 9.6.2003
    else rivivarjo=' ';

    p=strchr(x+1,':'); if (p==NULL) p=x+1;
    ++p; // 22.2.2005
    while (*p==' ' && *p!=EOS) ++p; // 22.2.2005
    strcpy(class_string,p);

    i=strlen(class_string)-1; while (i>0 && class_string[i]==' ')
                                            class_string[i--]=EOS;

//  strcpy(class_string,"* %1 on tÑllainen kunta.  / %1=Kunta");
// Rprintf("\nclass_string=%s|",class_string);

    if (*class_string==EOS) strcpy(class_string," "); // 24.2.2005
	fill_free_text(class_string); // 26.2.2005

    *sound_name=EOS;
    p=strstr(class_string," / ");  // 26.2.2005
    if (p!=NULL)
        {
        *p=EOS;
        q=strstr(p+3,"SOUND:");
        if (q!=NULL)
            {
            q+=6; i=strlen(q)-1; while(q[i]==' ') q[i--]=EOS;
            strcpy(sound_name,q);
            }
        }

    return(1);
    }

static int classif_error(char *s)
    {
    if (kieli==1)
        sprintf(sbuf,"\nVirhe luokittelussa %s!",s);
    else
        sprintf(sbuf,"\nError in classification %s!",s);
    PR_EBLK; sur_print(sbuf); WAIT; return(1);
    }

static int order_function(char *name,double *py)
    {
    *py=(double)jnro;
    return(1);
    }

static int pvm()
    {
    time_t ltime;

    time(&ltime);
    aika=localtime(&ltime);
    return(1);
    }

static int day_function(char *name,double *py)
    {
    pvm();
    *py=(double)(aika->tm_mday);
    return(1);
    }

static int month_function(char *name,double *py)
    {
    pvm();
    *py=(double)(1+aika->tm_mon);
    return(1);
    }

static int year_function(char *name,double *py)
    {
    pvm();
    *py=(double)(1900+aika->tm_year);
    return(1);
    }

static int weekday_function(char *name,double *py)
    {
    int vp;

    pvm();
    vp=aika->tm_wday; --vp; if (vp<0) vp=6; // viikko alkaa maanantaina
    *py=(double)(1+vp);
    return(1);
    }

static int update_time_var()
    {
    char s[20];

    if (d.d2.vartype[time_var][2]=='P') return(1);

    pvm();
    sprintf(s,"%.4d-%.2d-%.2d %.2d:%.2d:%.2d",
        aika->tm_year+1900,aika->tm_mon+1,aika->tm_mday,
        aika->tm_hour,aika->tm_min,aika->tm_sec);
    fi_alpha_save(&d.d2,jnro,time_var,s);
    return(1);
    }

static int n_cases_function(char *name,double *py)
    {
    int i,k;
    char name2[LNAME];

    if (*name==')') // 3.3.2005
        {
        *py=dat_n;
        return(1);
        }
    if (!stat) { k=alusta_stat_tilat(); if (k<0) return(-1); }
    strcpy(name2,name); name2[strlen(name2)-1]=EOS;
    i=varfind(&d,name2);
    if (i<0) return(-1);
    *py=(double)n_mean[i];
    return(1);
    }

static int mean_function(char *name,double *py)
    {
    int i,k;
    char name2[LNAME];

// Rprintf("\nnimi=%s|",name); getck();
    if (!stat) { k=alusta_stat_tilat(); if (k<0) return(-1); }
    strcpy(name2,name);
    name2[strlen(name2)-1]=EOS;

    i=varfind(&d,name2);
    if (i<0) return(-1);
// Rprintf("\ni=%d %ld %g|",i,n_mean[i],sum[i]); getck();
    if (n_mean[i]==0L) *py=MISSING8;
    else *py=sum[i]/n_mean[i];
    return(1);
    }

static int stddev_function(char *name,double *py)
    {
    int i,k;
    char name2[LNAME];

    if (!stat) { k=alusta_stat_tilat(); if (k<0) return(-1); }
    strcpy(name2,name);
    name2[strlen(name2)-1]=EOS;
    i=varfind(&d,name2);
    if (i<0) return(-1);
// Rprintf("\ni=%d %ld %g %g|",i,n_mean[i],sum[i],sum2[i]); getck();
    if (n_mean[i]<2L) *py=MISSING8;
    else *py=sqrt((sum2[i]-sum[i]*sum[i]/(double)n_mean[i])/(n_mean[i]-1.0));
    return(1);
    }

static int corr_function(char *names,double *py)
    {
    int i,k;
    long j,n;
    double sum1,sum2;
    double s1,s2,s12;
    double a1,a2;
    char names2[LNAME];
    char *name[2];

    strcpy(names2,names);
    names2[strlen(names2)-1]=EOS;
    split(names2,name,2);
// Rprintf("\nnimet: %s %s|",name[0],name[1]); getck();
    i=var_nro(name[0]); if (i<0) return(-1);
    k=var_nro(name[1]); if (k<0) return(-1);
    *py=MISSING8;

    n=0L;
    sum1=sum2=s1=s2=s12=0.0;

    for (j=1L; j<=d.n; ++j)
        {
        data_load(&d,j,i,&a1);
        if (a1==MISSING8) continue;
        data_load(&d,j,k,&a2);
        if (a2==MISSING8) continue;
        ++n;
        sum1+=a1; sum2+=a2;
        s1+=a1*a1; s2+=a2*a2; s12+=a1*a2;
        }
// Rprintf("\nn=%ld s1=%g sum1=%g|",n,s1,sum1); getck();
    if (n<2L) return(1);
    s1=s1-sum1*sum1/(double)n;
// Rprintf("\ns1=%g|",s1); getck();
    if (fabs(s1)<1e-14) return(1);
    s2=s2-sum2*sum2/(double)n;
// Rprintf("\ns2=%g|",s2); getck();
    if (fabs(s2)<1e-14) return(1);
    *py=(s12-sum1*sum2/(double)n)/sqrt(s1*s2);
// Rprintf("\ncorr=%g|",*py); getck();
    return(1);
    }

static int var_nro(char *name)
    {
    int i; // RS REM ,k;

    i=varfind(&d,name);
    if (i<0) return(-1);
    return(i);
    }

static int min_function(char *name,double *py)
    {
    int i,k;
    char name2[LNAME];

    if (!stat) { k=alusta_stat_tilat(); if (k<0) return(-1); }
    strcpy(name2,name);
    name2[strlen(name2)-1]=EOS;
    i=varfind(&d,name2);
    if (i<0) return(-1);
    *py=minv[i];
    return(1);
    }

static int max_function(char *name,double *py)
    {
    int i,k;
    char name2[LNAME];

    if (!stat) { k=alusta_stat_tilat(); if (k<0) return(-1); }
    strcpy(name2,name);
    name2[strlen(name2)-1]=EOS;
    i=varfind(&d,name2);
    if (i<0) return(-1);
    *py=maxv[i];
    return(1);
    }

// int stat=0;
// long *n_mean;
// double *sum,*sum2;

static int alusta_stat_tilat()
    {
    int i;
    long j;
    double a;

    n_mean=(long *)muste_malloc(d.m*sizeof(long));
    if (n_mean==NULL) { not_enough_space(); return(-1); }
    sum=(double *)muste_malloc(d.m*sizeof(double));
    if (sum==NULL) { not_enough_space(); return(-1); }
    sum2=(double *)muste_malloc(d.m*sizeof(double));
    if (sum2==NULL) { not_enough_space(); return(-1); }
    minv=(double *)muste_malloc(d.m*sizeof(double));
    if (minv==NULL) { not_enough_space(); return(-1); }
    maxv=(double *)muste_malloc(d.m*sizeof(double));
    if (maxv==NULL) { not_enough_space(); return(-1); }

    for (i=0; i<d.m; ++i)
        { n_mean[i]=0L; sum[i]=sum2[i]=0.0; minv[i]=MISSING8; maxv[i]=-MISSING8; }

    if (n0==0L)
        {
        for (i=0; i<d.m; ++i) maxv[i]=MISSING8;
        stat=1; return(1);
        }

    for (j=d.l1; j<=d.l2; ++j)
        {
        for (i=0; i<d.m; ++i)
            {
            data_load(&d,j,i,&a);
            if (a==MISSING8) continue;
            ++n_mean[i];
            sum[i]+=a; sum2[i]+=a*a;
            if (a<minv[i]) minv[i]=a;
            if (a>maxv[i]) maxv[i]=a;
            }
        }
    stat=1;
    return(1);
    }

static int replace_medit_vars(char *p)
    {
    int i,n;
    char x[LLENGTH];
    char *s[64];
    int var;
    char mname[9];

    strcpy(x,p);
    if (*x==EOS) return(1);
    n=split(x,s,64);
// Rprintf("\nn=%d %s %s|",n,s[0],s[1]); getck();
    for (i=0; i<n; ++i)
        {
        var=varfind2_medit(&d,s[i],0);
// Rprintf("\nvar=%d|",var); getck();
        if (var<0)
            {
            sprintf(sbuf,"\nMEDIT_VAR %s not found!",s[i]);
            sur_print(sbuf); WAIT; return(-1);
            }
        sprintf(mname,"|X%d|",i+1);
        medit_replace(mname,s[i]);
        }
    return(1);
    }

static int medit_replace(char *s,char *t)
    {
    int j;
    char *p,*q;
    char x[LLENGTH];
    char y[LLENGTH];

// varjorivit kÑsittelemÑttÑ!

    for (j=1; j<=ed2; ++j)
        {
        edread(x,j);
        p=x; *y=EOS;
        while (1)
            {
            q=strstr(p,s);
            if (q==NULL) break;
// Rprintf("\n1: %s|",p); getck();
            strncat(y,p,(int)(q-p));
            strcat(y,t);
// Rprintf("\n2: %s|",y); getck();
            p=q+strlen(s);
            }
        strcat(y,p);
        y[ed1]=EOS;
        edwrite(space,j,0);
        edwrite(y,j,0);
        }
    return(1);
    }

static int key_special_medit(int m)
                {
                int i; // RS REM ,k;
                char x[LLENGTH];
                long j0;

                switch (m)
                  {
                  case 0: return(1);
/* 13.5.2003 */   case -2: replace_value_by_mouse_word(); return(1);
                  case -3: goto_last_case();
                           break;
                  case CODE_EXIT:
//Rprintf("\nn=%d n0: %d editing=%d no_new=%d|",d.n,n0,editing,no_new_values);
                    if (editing) field_update1();
                    medit_tut_end();
                    if (d.n>n0 && no_new_values)
                        {
                        n_update(&d.d2,d.n-1); // RS 23.3.2013 1L -> 1
                        }
                    medit_save_end(); // 5.11.2003
// Rprintf("XXX"); getck();
                    if (toinen_survo_toiminnassa)
                        lopeta_toinen_survo();
                    return(-1);
                  case CODE_NEXT:
                    suunta=1;
                    pg_key=1;
                    if (editing) field_update1();

                    if (next_pages_line)
                        {
                        page_nro=select_next_page();
                        if (page_nro<0) return(-1);
                        }
                    else ++page_nro;

                    uusi=1;
                    if (page_nro==n_pages)
                        {
                        play_sound(2);
                        page_nro=0;
                        if (jnro<d.n)
                            {
                            new_jnro(1);
                            }
                        else create_new_case();
                        }
                    else play_sound(1);
                    break;

                  case CODE_PREV:
                    suunta=-1;
                    pg_key=1;
                    if (editing) field_update1();
                    if (page_nro>0) { --page_nro; uusi=1; play_sound(1); }
                    else if (jnro>1L)
                        {
                        play_sound(2);
                        new_jnro(-1); page_nro=n_pages-1; uusi=1;
                        }
                    else uusi=0;
                    break;

                  case 145: // ctrl-PgDn
                    suunta=1;
                    pg_key=1;
                    if (editing) field_update1();
                    uusi=2; // Keep current page and field!
                    if (jnro<d.n)
                        {
                        new_jnro(1); // page_nro=0;
                        }
                    else { create_new_case(); uusi=1; }
                    play_sound(2);
                    break;

                  case 146: // ctrl-PgUp
                    suunta=-1;
                    pg_key=1; // 30.9.2003
                    if (editing) field_update1();
                    uusi=2; // Keep current page and field!
                    if (jnro>1L)
                        {
                        new_jnro(-1);
                        }
                    else uusi=0;
                    play_sound(2);
                    break;

                  case 149: // alt-PgDn
                    if (check_on) break;
                    suunta=1;
                    pg_key=1;
                    if (editing) field_update1();
                    strcpy(x,field_value[current_field]);

                    j0=jnro;
                    i=current_field;
                    while (1)
                        {
                        if (jnro<d.n) new_jnro(1);
                        else { jnro=j0; break; }
                        read_var(field_var[i],field_format[i],sbuf);
                        if (strcmp(x,sbuf)!=0) break;
                        }
                    if (jnro==j0) break;

                    uusi=2; // Keep current page and field!
                    play_sound(2);
                    break;

                  case 150: // alt-PgUp
                    if (check_on) break;
                    suunta=-1;
                    pg_key=1;
                    if (editing) field_update1();
                    strcpy(x,field_value[current_field]);

                    j0=jnro;
                    i=current_field;
                    while (1)
                        {
                        if (jnro>1L) new_jnro(-1);
                        else { jnro=j0; break; }
                        read_var(field_var[i],field_format[i],sbuf);
                        if (strcmp(x,sbuf)!=0) break;
                        }
                    if (jnro==j0) break;

                    uusi=2; // Keep current page and field!
                    play_sound(2);
                    break;

                  case CODE_DOWN:
                    suunta=1;
                    if (editing) field_update1();
                    field_down();
                    break;
                  case CODE_UP:
                    suunta=-1;
                    if (editing) field_update1();
                    field_up();
                    break;

                  case CODE_HOME:      
                    if (c>field_column[current_field]) // 7.3.2005
                        {
                        c=field_column[current_field];
                        break;
                        }
                    suunta=-1;
                    if (editing) field_update1();
                    r=field_line[0];
                    c=field_column[0];
                    r_mouse=c_mouse=0;

                    if (current_field>0)
                        {
                        current_field=0;
                        break;
                        }
                    else if (page_nro>0)
                        {
                        page_nro=0;
                        uusi=1;
                        break;
                        }
                    else
                        {
                        jnro=1L;
                        uusi=1;
                        break;
                        }
                  case CODE_RETURN:
                  case CODE_TAB:
                    suunta=1;
                    goto_next_field();
                    break;

                  case CODE_RIGHT:
                    suunta=1;
                    if (n_fields && c<field_column[current_field]+field_len[current_field]-1)
                       ++c;
                    else goto_next_field();
                    break;

                  case CODE_LEFT:
                  case CODE_BACKSP:
                    suunta=-1;
                    if (c>field_column[current_field])
                       --c;
                    else goto_previous_field();
                    break;

                  case CODE_END: // 7.3.2005
                    c=field_column[current_field]+field_len[current_field]-1;
                    break;

                  case CODE_DELETE:
                    if (ei_saa_editoida()) break;
                    delete_char();
                    break;

                  case CODE_INSERT:
                    insert_mode=1-insert_mode;
                    if (insert_mode) CURSOR_INS; else CURSOR_ON;
                    break;

                  case CODE_ERASE:
                    if (ei_saa_editoida()) break;
                    erase_chars();
                    break;

                  case 1001: // soft_code!
                    sound_on=1-sound_on;
                    if (sound_on) play_sound(0);
                    break;

                  case CODE_EXEC: if (search_on) haku(1,0); break;
                  case CODE_SRCH: search_on2=1; haku(0,0); break;
                  case CODE_PRE:
                    search_on2=1;
                    suunta=1;
                    pref=(unsigned char)PREFIX;
//                  if (search_on && current_field2>=0) break; // 5.6.2003
                    if (search_on) break; // 5.6.2003
                    prefix(); break;

                  case CODE_TOUCH:
                    saa_kirjoittaa=1-saa_kirjoittaa;
                    if (saa_kirjoittaa) play_sound(5);
                    else play_sound(4);
                    break;

                  case CODE_REF:
                    if (ref_jnro==0L)
                        {
                        ref_jnro=jnro;
                        ref_page=page_nro;
                        ref_field=current_field;
                        ref_r=r; ref_c=c;
                        break;
                        }
                    else
                        {
                        if (ref_jnro==jnro && ref_page==page_nro
                                         && ref_field==current_field)
                            {
                            ref_jnro=0L; break;
                            }
                        else
                            {
                            field_update();
                            jnro=ref_jnro;
                            page_nro=ref_page;
                            current_field=ref_field;
                            r=ref_r; c=ref_c;
                            update_specs();
                            i=show_page(); if (i<0) return(-1);
                            break;
                            }
                        }

                  case CODE_CODE:
                        {
                        char value1[LLENGTH],value2[LLENGTH];
                        int true_current; // ,true_old;

                        if (ref_jnro==0L) break;
/**************************
                   Rprintf("\ncurrent=%d ref=%d",current_field,ref_field);
                   Rprintf("\ncurrent=%s| ref=%s|",
                        field_value[current_field],
                        field_value[ref_field]);
                   getck();
*******************************/
                        true_current=current_field;
//                        true_old=old_field;
                        strcpy(value1,field_value[current_field]);
                        strcpy(value2,field_value[ref_field]);
                        old_field=current_field=ref_field;
                        strcpy(field_value[current_field],value1);
                        field_update();
                        old_field=current_field=true_current;
                        strcpy(field_value[current_field],value2);
                        field_update();

                        ref_jnro=0L;
                        break;
                        }

                  case CODE_SUCRO1: // F11
                    play_sound(7);
                    save_page();
         write_string("Page saved!",11,'5',r3,c3-12);
                    sur_sleep(700L);
         soft_bottom_line_erase_medit();
                    i=key_special_medit(CODE_NEXT); if (i<0) return(-1);
                    break;
                  }
                return(1);
                } /* end special */

static int haku(int k,int jatkuva)
// int k; // k=0: alkaa k=1: jatkuu
    {
    int i,h;
    extern int muste_no_selection,muste_mousewheel; // RS ADD 5.12.2012

    pref=' ';
    if (!n_fields) return(1); // ???
    if (editing) field_update();
    if (current_field2>=0) field2_haku=1;
    else
        {
        field2_haku=0;
        old_field=current_field;
        }
    jatkuva_haku=jatkuva;
    if (k==0)
        {
        i=3;
        while (i==3)
          {
          putsaa();
          cursor_medit(r3-1,0);
          if (kieli==1)
              strcpy(hakutieto,"Etsittava havainto ( numero tai =,<,> ");
          else
              strcpy(hakutieto,"Record to be found ( number or =,<,> ");
          if (field2_haku)
              strcat(hakutieto,field_name2[current_field2]);
          else
              strncat(hakutieto,d.varname[field_var[current_field]],8);
          strcat(hakutieto,")? ");
          PR_EBLD;

  h=search_on2; search_on2=1; // 20.9.2003
          i=prompt(hakutieto,hakuavain,16); // i=3: ctrl-Right          
  search_on2=h;
    muste_no_selection=TRUE;
    muste_mousewheel=FALSE;
  
          if (i==3)
              {
              h=goto_next_field2();
              if (h<0) { putsaa(); return(1); }
              }
          }
        }

    if (!jatkuva_haku && pikahaku) { pikahaku=0; return(1); }
    if (jatkuva_haku)
        while (1)
            {
            i=etsi();
            if (i==2) { jatkuva_haku=0; break; }
            }
    else
        {
        etsi();
        }

    if (!field2_haku)
        {
        editing=0; uusi=2;
        current_field=old_field;
        r=field_line[current_field];
        c=field_column[current_field];
        cursor_medit(r,c);
        }
    else
        {
        update_specs();
        i=show_page(); if(i<0) return(-1);
        cursor_medit(r,c);
//      current_field2=-1;  // <6.6.2003
        }
    search_on2=0;
    return(1);
    }

static int save_open()
    {
    int i; // RS REM ,k;
    char x[LLENGTH],*s[2];
    char save_name0[LNAME];
    int n_lines,g;

    if (*save_edt==EOS)
        {
        sprintf(save_edt,"%sMEDITSAV.EDT",edisk);
        strcpy(save_name0,"MEDITSAV");
        n_lines=9999999;
        strcpy(x,spec_string[MEDIT_SAVE]);
        g=split(x,s,2);
        if (g>0)
            {
            strcpy(save_edt,s[0]);
            strcpy(save_name0,s[0]);
            if (strchr(save_edt,':')==NULL)
                {
                sprintf(save_edt,"%s%s",edisk,s[0]);
                if (strchr(save_edt,'.')==NULL)
                    strcat(save_edt,".EDT");
                }
            }
//      if (g>1) n_lines=atoi(s[1]);
		i=0;
        savefile=muste_fopen(save_edt,"rb");
        if (savefile!=NULL)
            {
            n_shad_lines=0;
            fgets(sbuf,ed2+10,savefile);
            while (!feof(savefile))
                {
                fgets(sbuf,ed2+10,savefile);
                if (*sbuf=='S') { ++n_shad_lines; continue; }
                else i=atoi(sbuf);
                }
            next_line=i+1;

            muste_fclose(savefile);
            }
        else
            {
            savefile=muste_fopen(save_edt,"wb");
            fprintf(savefile,"SURVO 98 edit field:     %d         %d           10 %s",
                                                  ed1,n_lines,rivin_loppu2);
            sprintf(sbuf,"00001|*SAVE %s / FILE MEDIT %s",save_name0,data_name);
/**************************
            k=strlen(sbuf);
            for (i=k; i<ed1+5; ++i) sbuf[i]=' ';
            sbuf[i]='*'; // tÑydeksi riviksi peruutusten vuoksi!
            sbuf[i+1]=EOS;
*************************/
            fprintf(savefile,"%s%s",sbuf,rivin_loppu2);

            muste_fclose(savefile);
            next_line=2;
            n_shad_lines=0;
            }
// Rprintf("\nnext_line=%d n_shad_lines=%d|",next_line,n_shad_lines);
// getck();

        }
    return(1);
    }

static int save_page()
    {
    int i; // RS REM ,k;
// RS REM    char x[LLENGTH],*s[2];
// RS REM    char save_name0[LNAME];
// RS REM    int n_lines,g;

    if (!already_open)
        {
        save_open();
        savefile=muste_fopen(save_edt,"a+b");
        already_open=1;
        }
    muste_fseek(savefile,0L,SEEK_END);
/************************************
    muste_fseek(savefile,(long)(-10-ed1),SEEK_CUR);
    fread(sbuf,1,10+ed1,savefile); sbuf[10+ed1]=EOS;
    i=9+ed1;
    while(i>0 && sbuf[i]!='|') --i;
    sbuf[i]=EOS;
    while (i>0 && sbuf[i]!=10) --i;
    next_line=atoi(sbuf+i+1)+1;
    muste_fseek(savefile,0L,SEEK_END);
**************************************/

    savepage=1;
    update_specs();
    i=show_page2(); if (i<0) return(-1);
    fprintf(savefile,"%d|*%s",next_line++,rivin_loppu2);
    savepage=0;
//  muste_fclose(savefile);

    return(1);
    }

static int save_pages()
    {
    int i;

    save_open();

    savefile=muste_fopen(save_edt,"a+b");

    muste_fseek(savefile,0L,SEEK_END);
/****************************
    muste_fseek(savefile,(long)(-10-ed1),SEEK_CUR);
    fread(sbuf,1,10+ed1,savefile); sbuf[10+ed1]=EOS;
    i=9+ed1;
    while(i>0 && sbuf[i]!='|') --i;
    sbuf[i]=EOS;
    while (i>0 && sbuf[i]!=10) --i;
    next_line=atoi(sbuf+i+1)+1;
    muste_fseek(savefile,0L,SEEK_END);
******************************/
    sur_print("\n");
    while (1)
        {
        savepage=1;
        update_specs();
        i=show_page(); if (i<0) return(-1);
        fprintf(savefile,"%d|*%s",next_line++,rivin_loppu2);
        savepage=0;
        headline_medit();
// nextch();
        if (next_pages_line)
            {
            page_nro=select_next_page();
            if (page_nro<0) return(-1);
            }
        else ++page_nro;

        if (page_nro==n_pages)
            {
            if (jnro<d.n) { new_jnro(1); page_nro=0; continue; }
            else break;
            }

        }
    medit_tut_end();
    medit_save_end();
    return(1);
    }

static int medit_save_end()
    {
    char x[LLENGTH];
    char *p,*q;
    int len,len2;
    int n;

// Rprintf("\nend: next_line=%d n_shad_lines=%d|",next_line,n_shad_lines);
// getck();

    if (*save_edt==EOS) return(1);
    muste_fclose(savefile); // 10.11.2003
    savefile=muste_fopen(save_edt,"r+b");
    fread(x,1,100,savefile);
    p=x; while (*p!=*rivin_loppu2) ++p; *p=EOS;
    len=strlen(x);
// Rprintf("\n%s|",x); getck();
    p=strchr(x,':');    // paikkaa riviluku!
    ++p;
    while (*p==' ') ++p;
    while (*p!=' ') ++p;
    while (*p==' ') ++p;
    q=p; while (*q!=' ') { *q=' '; ++q; }

    n=next_line+100; if (n<1000) n=1000;
    sprintf(sbuf,"%d",n);
    len2=strlen(sbuf);
    strncpy(q-len2,sbuf,len2);
// Rprintf("\n%s|",x); getck();

    p=q;
    while (*p==' ') ++p;
    q=p; while (*q!=' ') { *q=' '; ++q; }
// *q='X';
// Rprintf("\n%s|",x); getck();
    n=n_shad_lines;
    sprintf(sbuf,"%d",n);
    len2=strlen(sbuf);
    strncpy(q-len2,sbuf,len2);
// Rprintf("\n%s|",x); getck();

    rewind(savefile);
    fwrite(x,1,len,savefile);
    muste_fclose(savefile);
    return(1);
    }

static int key_common(int m)
        {
        int i;
        char *p;

        if (pilkku_pisteeksi)
            {
            if (m==(int)',') m=(int)'.';
            else if (m==(int)'.') m=(int)',';
            }

        if (ei_saa_editoida()) return(1);
        if (editing==0)
            {
/***********************
            if (new_case)
                {
                no_new_values=0;
                d.n=jnro;
                n_update(&d.d2,d.n);
                new_case=0;
                }
***************************/
            start_editing();
         // Rprintf("\nval=%s|",old_value); getck();
            }
        PR_EIN2;
        sprintf(sbuf,"%c",(char)m); sur_print(sbuf);
        i=c-field_column[current_field];
        if (insert_mode && i<field_len[current_field]-1)
            {
            p=field_value[current_field]+i;
            strcpy(sbuf,p); sbuf[strlen(sbuf)-1]=EOS;
            *p=(char)m;
            strcpy(p+1,sbuf);
            sur_print(sbuf);
            ++c;
            }
        else
            {
            field_value[current_field][c-field_column[current_field]]=(char)m;
// Rprintf("\nval=%s|",field_value[current_field]); getck();
            if (c<field_column[current_field]+field_len[current_field]-1)
                ++c;
            else
                {
                new_page_wait=1;
                if (press_tab) return(1); // 26.8.2003
                else
                    goto_next_field();
                }
            }

        r_mouse=c_mouse=0;
        return(1);
        }

static int ei_saa_editoida()
    {
    if (!saa_kirjoittaa)
        {
        play_sound(4);
        putsaa();
        cursor_medit(r3-1,0); PR_EBLK;
        if (kieli==1)
            sur_print("Kirjoitusluvan saa painamalla nappia F3!");
        else
            sur_print("Permission for editing by pressing F3!");
        sur_sleep(2000L);
        putsaa();
        return(1);
        }
    if (d.d2.vartype[field_var[current_field]][2]=='P')
        {
        play_sound(4);
        cursor_medit(r3-1,0); PR_EBLK;
        sur_print("This is a protected field! (Cannot be edited!)");
        sur_sleep(2000L);
        putsaa();
        return(1);
        }
    return(0);
    }

/* RS REM
HANDLE hStdOut,hStdIn;
DWORD dwMode;

set_console_io()
        {
        hStdOut = GetStdHandle(STD_OUTPUT_HANDLE);
        hStdIn = GetStdHandle(STD_INPUT_HANDLE);
        GetConsoleMode(hStdIn, &dwMode);
        SetConsoleMode(hStdIn, (dwMode & ~(ENABLE_LINE_INPUT |
            ENABLE_ECHO_INPUT))
            | ENABLE_WINDOW_INPUT | ENABLE_MOUSE_INPUT);
        return(1);
        }

*/

static int prefix()
        {
        int m; // RS REM ,m2;
// RS REM        int i;
// RS REM        char x[LLENGTH];
// RS REM        char msana[3];
// RS REM        char *p,*q;
            {
            m=nextch_medit();
            pref=' ';
            }
        if (special)
            {
            switch(m)
                {
              case CODE_DOWN:
                suunta=1;
                goto_last_case();
                break;
              case CODE_EXEC:
                if (search_on) haku(1,1);
                break;
              case CODE_REF:
                sound_on=1-sound_on;
                if (sound_on) play_sound(0);
                break;
                }
            }
        else /* not special */
            {
            switch(m)
                {
//            case 'T': case 't': tut_special(); break;
              case 'S':
              case 's':
                edwrite(space,1,1);
                edwrite("SAVE _MEDIT / saved by F2 W in FILE MEDIT",1,1);
                edsave("_MEDIT",1,0);
                break;
                }
            }
        return(1);
        }

static int n_update(SURVO_DATA_FILE *s,long n)
/* long n; new obs.# */
        {
        fi_rewind(s);
        fi_puts(s,(char *)&n,sizeof(int),22L); // RS ADD (char *) CHA 64-bit sizeof(long) -> sizeof(int)
        (*s).n=n;
        return(1);
        }

static int medit_cls()
        {
        int i;
        char x[256];

        if (!display_off)
            {
            for (i=0; i<256; ++i) x[i]=' ';
            for (i=1; i<=r3; ++i)
                write_string(x,c3-1,' ',i,1);
            }
        sur_locate(1,1);
        return(1);
        }


static void sur_set_medit_focus()
    {
muste_fixme("\nFIXME: sur_set_medit_focus() STUB!");
	}
/* RS REM
    HWND hwnd;

    hwnd=FindWindow(NULL,"SURVO MM");
    sur_sleep(200L); // 30.9.2003
// Rprintf("\nhwnd=%d|",hwnd); getck();

//  SetFocus(hwnd);
//  SetActiveWindow(hwnd);
//  EnableWindow(hwnd,TRUE);
//  BringWindowToTop(hwnd);
    SetForegroundWindow(hwnd);
    SetForegroundWindow(hwnd);
    return(1);
    }
*/



static int varinfo()
        {
        int i;

        for (i=0; i<d.m; ++i)
            {
            switch (d.vartype[i][0])
                {
              case '1':
                        min[i]=0.0; max[i]=254.0;
                        rajat(i,&min[i],&max[i]);
                        break;
              case '2':
                        min[i]=-32768.0; max[i]=32767.0;
                        rajat(i,&min[i],&max[i]);
                        break;
              case '4':
                        min[i]=-1e37; max[i]=1e37;
                        rajat(i,&min[i],&max[i]);
                        break;
              case '8':
                        min[i]=-1e305; max[i]=1e305;
                        rajat(i,&min[i],&max[i]);
                        break;
              case 'S':
                        min[i]=-1e305; max[i]=1e305;
                        strarvot(i,&strarvo[i]);
                        break;
                }

            }
        return(1);
        }

static int hae_muoto(SURVO_DATA_FILE *d,int i,char *muoto)
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

//      if (muoto[1]==EOS) strcat(muoto,"#");
        // peitettÑvÑ - maskissa #-

        return(1);
        }


static int default_form(SURVO_DATA_FILE *d,int i,char *muoto)
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
        return(1);
        }

static int rajat(int i,double *pmin,double *pmax)
        {
        char *p,*q;
        char teksti[LLENGTH];
        char *sana[2];
        int k;

        strcpy(teksti,d.varname[i]);
        p=strchr(teksti,'{'); if (p==NULL) return(1);
        q=strchr(p,'}'); if (q==NULL) return(1);
        *q=EOS;
        k=split(p+1,sana,2);
        if (k<2) return(1);
        *pmin=atof(sana[0]); *pmax=atof(sana[1]);
        return(1);
        }

static int strarvot(int i,int *pi)
        {
        char *p,*q;
        char teksti[LLENGTH];

        *pi=0;
        strcpy(teksti,d.varname[i]);
        if (teksti[9]=='~') return(1);
        p=strchr(teksti+8,'['); if (p==NULL) return(1);
        q=strchr(p,']'); if (q==NULL) return(1);
        *pi=p-teksti;
        return(1);
        }

static int check_limits(int muuttuja,double x)
    {
    int i;

// Rprintf("\n%d: %g %g|",muuttuja,min[muuttuja],max[muuttuja]); getck();
    if (x<min[muuttuja] || x>max[muuttuja])
        {
        sur_locate(r3,1);
        PR_EBLD;
        if (x<min[muuttuja])
            { sprintf(sbuf,"%.8s < %g (smallest permitted value)",
                    d.varname[muuttuja],min[muuttuja]);
            }
        else
            { sprintf(sbuf,"%.8s > %g (greatest permitted value)",
                    d.varname[muuttuja],max[muuttuja]);
            }
        strcat(sbuf,"  Press any key!");
        play_sound(3);
        for (i=0; i<5; ++i)
            {
            soft_bottom_line_erase_medit();
            sur_wait(70L);
//          write_string(sbuf,strlen(sbuf),'5',r3,0); // 22.6.2003
            write_string(sbuf,strlen(sbuf),'5',r3,1); // 7.7.2003
            sur_wait(300L);
            }

        nextch_medit();
        soft_bottom_line_erase_medit();
        return(-1);
        }
    else return(1);
    }

static int check_strarvot(int muuttuja,char *edsana)
    {
    char s[LLENGTH];
    char *p;
    char *sana[EP4];
    int m,i,len;

    if (!strarvo[muuttuja]) return(1);

    len=strlen(edsana);
    while (len>0 && edsana[len-1]==' ') --len;

    strcpy(s,d.varname[muuttuja]+strarvo[muuttuja]+1);
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
                strcpy(tut_info,"___@8@FILE MEDIT@Not a permitted value!@");
                return(-1);
                }
            sur_locate(r3,1);
            PR_EBLD;
            sprintf(sbuf,"%s is not a permitted value for %.8s",
                    edsana,d.varname[muuttuja]);
            strcat(sbuf,"  Press any key!");
            play_sound(3);
            for (i=0; i<5; ++i)
                {
                soft_bottom_line_erase_medit();
                sur_wait(70L);
                write_string(sbuf,strlen(sbuf),'5',r3,0); // 22.6.2003
                sur_wait(300L);
                }

            nextch_medit();
            soft_bottom_line_erase_medit();
            return(-1);
            }
        }
    return(1);
    }

static int find_sounds()
    {
    int i;
    char name[LNAME];
    char x[101];
    char *p;

    sprintf(name,"%sSYS/MEDITSND.SYS",survo_path);
    medit_temp=muste_fopen(name,"r+t");
    if (medit_temp==NULL)
        {
        sprintf(sbuf,"Text field %s not found!",name);
        PR_EBLK; sur_print(sbuf); WAIT; return(-1);
        }

    fgets(x,100,medit_temp);
    for (i=0; i<N_SOUNDS; ++i)
        {
        fgets(x,100,medit_temp);
        p=strstr(x," / ");
        if (p==NULL) p=x+strlen(x)-2;
        while (*p==' ') --p; ++p; *p=EOS;
        strcpy (medit_sound[i],x);
        }
    muste_fclose(medit_temp);
/***********************
    for (i=0; i<N_SOUNDS; ++i)
        {
        Rprintf("\n%s|",medit_sound[i]); getck();
        }
**********************/
    return(1);
    }

static void PlaySound()
	{
	muste_fixme("\nFIXME: PlaySound()-STUB in FILE MEDIT!");  
	}


static int play_sound(int i)
    {
    char sound[LNAME];

    if (!sound_on) return(1);
    if (strncmp(medit_sound[i],"NULL",4)==0) return(1);
    sprintf(sound,"%sSND/%s",survo_path,medit_sound[i]);

// RS FIXME NYI    PlaySound(sound,NULL,SND_FILENAME | SND_ASYNC);
	PlaySound(); // RS FIXME

    return(1);
    }

static int play_sound_by_name(char *s,int rr,int cc)
    {
    int i;
// RS REM    int r0,c0;

    if (!sound_on) { *s=EOS; return(1); }
    cursor_medit(rr,cc);
    for (i=rr+2; i<r3-1; ++i)
        write_string(space,c3,' ',i,0); // 22.6.2003
    play_sound2(s);
/********************
    sprintf(sound,"%sSND\\%s",survo_path,s);
    PlaySound(sound,NULL,SND_FILENAME | SND_SYNC);
    *s=EOS; // poista ÑÑni!
****************************/
    return(1);
    }

static int play_sound2(char *s)
    {
    char sound[LNAME];

    if (!sound_on) { *s=EOS; return(1); }
    if (s[1]==':') strcpy(sound,s);
    else
        sprintf(sound,"%sSND/%s",survo_path,s);

// RS FIXME NYI    PlaySound(sound,NULL,SND_FILENAME | SND_SYNC);
	PlaySound(); // RS FIXME
	*s=EOS; // poista ÑÑni!
    return(1);
    }

static int make_pages(char *s0)
    {
    int i,j,k,h,chei;
    char orig_name[LNAME];
    char name[LNAME];
    int m_act;
    int m_page,n_pages;
    char x[LLENGTH];
    int lin; // RS REM ,lin_page;
    int var;
    char format[LNAME];
    char type;
    char *p;

    strcpy(name,s0);
    strcpy(orig_name,s0);

    i=strlen(name);
    p=name+i-1;
    if (*p=='-' || *p=='+') *p=EOS;

// Rprintf("\nr3=%d r_soft=%d|",r3,r_soft); // RSRS
    if (r_soft) r3=r3+1+r_soft-1;
// Rprintf("\nUusi r3 on %d.",r3); // RSRS

    i=data_open2(name,&d,1,1,0); if (i<0) return(-1);

    m_act=d.m_act;
    m_page=r3-2;

    n_pages=1+(m_act-1)/m_page;

    *sbuf='*'; sbuf[1]=EOS;
    strncat(sbuf,space,c2+1);

    for (j=1; j<=r2; ++j)
        {
        edwrite(sbuf,j,0);
        zs[j]=0;
        }

    sprintf(sbuf,"SAVE _MEDIT / %s: FILE MEDIT pages",name);
    edwrite(sbuf,1,1);

    i=tell_language();
    if (i==1)
{
edwrite("Tama on Survon ehdottama FILE MEDIT -sivujen maaritys",3,1);
sprintf(sbuf,"datatiedostoa %s (ja muita samanrakenteisia) varten.",name);
edwrite(sbuf,4,1);
edwrite("Voit tarkentaa ja laajentaa tata sivustoa haluamallasi tavalla.",5,1);
edwrite("Jos teet muutoksia, talleta tama _MEDIT-kentta toisella nimella,",6,1);
edwrite("jota kaytat myohemmissa FILE MEDIT -komennoissa esim. tyyliin",7,1);
sprintf(sbuf,"FILE MEDIT %s <uusi_nimi>:%s",name,name);
edwrite(sbuf,8,1);
}
    else
{
edwrite("This is definition of FILE MEDIT pages suggested by Survo",3,1);
sprintf(sbuf,"for data file %s (and for other files with a similar structure).",name);
edwrite(sbuf,4,1);
edwrite("You may adjust and extend this set of pages as you like.",5,1);
edwrite("After making changes, save this _MEDIT field by another name.",6,1);
edwrite("Thereafter you may employ the new setup of pages by activating",7,1);
sprintf(sbuf,"FILE MEDIT %s <new_name>:%s",name,name);
edwrite(sbuf,8,1);
}

// MEDIT:ESIM0 SIZE=32,82 PAGES=Sivu1,Sivu2,Sivu3

    lin=10;
    sprintf(x,"MEDIT:%s SIZE=%d,%d PAGES=",name,r3,c3+8);
    for (i=0; i<n_pages; ++i)
        {
        sprintf(sbuf,"Page%d",i+1);
        strcat(x,sbuf);
        if (i<n_pages-1) strcat(x,",");
        if (strlen(x)>c3-6 || i==n_pages-1)
            {
            edwrite(x,lin++,1);
            strcpy(x,"  ");
            }
        }
    ++lin;

    var=0;
    for (k=0; k<n_pages; ++k)
        {
        sprintf(sbuf,"PAGE Page%d:",k+1);
        edwrite(sbuf,lin++,1);
        sprintf(sbuf,"Page %d/%d",k+1,n_pages);
        edwrite(sbuf,lin++,60);

        for (i=0; i<m_page; ++i)
            {
            if (var>=m_act) { ++lin; continue; }
            strcpy(x,d.varname[d.v[var]]);

            type=d.vartype[d.v[var]][0];
            if (type!='S') 
            	{ chei=get_format(type,x,d.v[var],format); if (chei<0) return (-1);}
            else
                {
                int len; // 22-1.2006

                len=d.varlen[d.v[var]];
                if (len>LNAME-2) len=LNAME-2;
        //      for (h=0; h<d.varlen[d.v[var]]; ++h)
                for (h=0; h<len; ++h)
                    format[h]='#';
                format[h]=EOS;
                }
            *sbuf=EOS; strncat(sbuf,x,9);
            p=sbuf;
            while (*p!=' ') ++p; *p=':';
            sprintf(sbuf+9," %s   %s",format,x+9);

            edwrite(sbuf,lin++,1);

            ++var;
            }
        edwrite("END",lin++,1);
        ++lin;
        }
    data_close(&d);
    sprintf(info,"%s _MEDIT %s",orig_name,name);
    edsave("_MEDIT",1,0);
    return(1);
    }

static int get_format(char type,char *x,int var,char *format)
    {
    char *p,*q;

    p=strstr(x,"(#");
    if (p==NULL)
        {
        switch (type)
            {
          case '1': strcpy(format,"###"); break;
          case '2': strcpy(format,"#####"); break;
          case '4': strcpy(format,"########.######"); break;
          case '8': strcpy(format,"########.######"); break;
            }
        return(1);
        }
    q=p;
    while (*q!=')' && *q!=EOS) ++q;
    if (q==EOS)
        {
        sprintf(sbuf,"\nError in format of field %.s",d.varname[var]);
        PR_EBLK; sur_print(sbuf); WAIT; return(-1);
        }
    *format=EOS; strncat(format,p+1,q-p-1);
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
// RS REM        char arvo2[LLENGTH];
        char short_name[64];
        char *p;
        char vaeli[2];

        search_on=1;
        m=d.m; // yhteensopivasti FILE SHOWn kanssa
        n=d.n;

        if (field2_haku)
            {
            var=-1;
            }
        else var=field_var[current_field];

        osahaku=0; if (*hakuavain=='*') { osahaku=1; strcpy(arvo,hakuavain+1); }
        hav=atol(hakuavain);
        if (hav>0L && hav<=n)
            {
            jnro=jnro0=hav;
            putsaa();
            disp_recs(jnro); rivi=ensrivi; disp_nimi();
            return(2);
            }

/*      if (*hakuavain==EOS) { putsaa(); osoita(var); return(1); } */
        if (*hakuavain==EOS) rel='M';  /* searching for missing value */
        else i=relaatio(hakuavain,&rel,arvo);
        if (i<0) { putsaa(); osoita(var); return(2); }

        if (muste_strcmpi(hakuavain,"=max")==0) // 19.7.2003
            {
            etsi_maxmin(var,1);
            return(1);
            }
        else if (muste_strcmpi(hakuavain,"=min")==0) // 19.7.2003
            {
            etsi_maxmin(var,2);
            return(1);
            }
        else
            {
            len=strlen(arvo); y=atof(arvo);
            }

 /*     type=d.d2.vartype[var][0];   -4.10.1992  */

        if (var>=0)
            type=d.d2.vartype[var][0];
        else type='8';

        if (rel==' ' && type!='S') rel='=';
        if (osahaku) rel=' ';

// Rprintf("\nsortvar=%d var=%d type=%c|",sortvar,var,type); getch();
/************************
        if (!ordind && !osahaku && sortvar==var && type=='S' && (rel==' ' || rel=='='))
            {
            strcpy(arvo2,arvo);
            binhaku(arvo);
            if (etu==2)
                {
                poimi(jnro,var,hakusana);
                i=strlen(hakusana); while (i>0 && hakusana[i-1]==' ') hakusana[--i]=EOS;
                if (strcmp(arvo2,hakusana)!=0)
                    {
                    sprintf(tut_info,"___@9@FILE MEDIT@Case %s not found!@",arvo2);
                    search_on=0;
                    return(-1);
                    }
                }
            putsaa(); osoita(var);
            return(2);
            }
**************************/
        hav=jnro;
        while (1)
            {
            if (sur_kbhit()) { sur_getch(); putsaa(); osoita(var); return(1); }
            ++hav;
            if (hav>d.d2.n)
                {
                putsaa();
                cursor_medit(r3-1,0); PR_EBLD;
                if (n_haku==0)
                    {
                    if (etu==2)  /* 1.5.91 */
                        {
                        sprintf(tut_info,"___@9@FILE SHOW@Case %s not found!@",arvo);
                        return(-1);
                        }
                    sur_print("Not found!");
                    not_found=1;
                    search_on=search_on2=0;
//                  field2_haku=0;
                    }
                else
                    {
                    *short_name=EOS;
                    if (var>=0)
                        strncat(short_name,d.varname[var],8);
                    else
                        strcat(short_name,field_name2[current_field2]);
                    p=short_name+7; while (*p==' ') *p--=EOS;
                    *vaeli=EOS; if (*hakuavain=='*') strcpy(vaeli," ");
                    sprintf(sbuf,"%s%s%s: %d cases found.",short_name,vaeli,hakuavain,n_haku);
                    PR_EBLK; sur_print(sbuf);
                    search_on=search_on2=0; n_haku=0;
                    if (var<0) jnro=jnro0;
                    update_specs();
                    show_page(); // 6.6.2003
//                  field2_haku=0;
                    }
                sur_print(" Press any key!");
                osoita(var);
                nextch_medit();
                putsaa();
                osoita(var); return(2);
                }

            if (unsuitable(&d,hav)) continue; // 21.9.2003

            if (rel==' ')
                {
      /*        fi_alpha_load(&d.d2,hav,var,hakusana);   */
                poimi(hav,var,hakusana);
                if ( (!osahaku && strncmp(hakusana,arvo,len)==0) ||
                     (osahaku && strstr(hakusana,arvo)!=NULL) )
                    {
                    ++n_haku;
                    jnro=jnro0=hav;
// Rprintf("\n%d|",jnro); getck();
                    if (jatkuva_haku) return(1);
                    putsaa();
                    disp_recs(jnro); rivi=ensrivi; disp_nimi();
                    search_on2=0;
                    return(1);
                    }
                cursor_medit(r3-1,69); PR_EBLD;
                sprintf(sbuf,"%ld",hav); sur_print(sbuf);
                if (sur_kbhit()) { i=sur_getch(); if (i=='.') { putsaa(); return(2); } }
                continue;
                }
            if (var<0)
                {
                jnro=hav;
                update_specs();
                laske(field_name2[current_field2],&x);
// LOCATE(1,1); Rprintf("%s x=%g",field_name2[current_field2],x); getck();
                }
            else fi_load(&d.d2,hav,var,&x);
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
                jnro=jnro0=hav;
                if (jatkuva_haku) return(1);
                putsaa();
                disp_recs(jnro); rivi=ensrivi; /* disp_nimi(); */
                return(1);
                }
            cursor_medit(r3-1,69); PR_EBLD;
            sprintf(sbuf,"%ld",hav); sur_print(sbuf);
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') { putsaa(); return(2); } }
            continue;
            }
        }

static int etsi_maxmin(int var,int mm)
// int mm; // 1=max 2=min
    {
    long l;
    double a,b;
    long jnro0;

    jnro0=jnro;


    if (mm==1) a=-1e100; else a=1e100;
    for (l=jnro0+1L; l<=d.n; ++l)
        {
        if (unsuitable(&d,l)) continue;
        if (var<0)
            {
            jnro=l;
            update_specs();
            laske(field_name2[current_field2],&b);
            }
        else fi_load(&d.d2,l,var,&b);
        if (b==MISSING8) continue;
        if (mm==1)
            {
            if (b>a) { a=b; jnro0=l; }
            }
        else
            {
            if (b<a) { a=b; jnro0=l; }
            }
        }


    putsaa();
    jnro=jnro0;
//  update_specs();
//  show_page();

    return(1);
    }

static int relaatio(char *s,char *prel,char *arvo)
//char *s; /* hakuavain */
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

static int poimi(long j,int i,char *sana)
        {
// RS REM        int vi;
        double luku;
        float luku4;
        int h,len;
        char type;
        int miss;
        int pit;

        type=d.vartype[i][0];
        miss=0;
        pit=d.varlen[i];
        if (j==n+1)
            { strncpy(sana,space,pit); sana[pit]=EOS; return(1); }
        if (type=='S')
            {
            fi_alpha_load(&d.d2,j,i,sana);
            h=strlen(sana);
            if (h<pit) strncat(sana,space,pit-h);
            }
        else
            {
            fi_alpha_load(&d.d2,j,i,sana);
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
                strncpy(sana,space,pit); sana[pit]=EOS; return(1);
                }

            len=strlen(sana);
            if (len<pit)
                {
                for (h=len; h<pit; ++h) sana[h]=' '; sana[pit]=EOS;
                }
            }
        return(1);
        }

static int float4_muunto(float f,char *sana)
          {
          char *p;

          sprintf(sana,"%g",f);
          p=strchr(sana,'.'); if (p==NULL) return(1);
          p=strchr(sana,'e'); if (p!=NULL) return(1);
          p=sana; if (*p=='-') ++p;
          if (strlen(p)<7) return(1);
          p+=6; if (*p=='9')
              {
              while (*p=='9') --p;
              if (*p=='.') return(1);
              ++(*p);
              }
          else if (*p=='0')
              {
              while (*p=='0') --p;
              }
          *(p+1)=EOS;
          return(1);
          }

static int laske(char *lauseke,double *y)
        {
//        double luku();
//        double oper();
//        double funktio();
//        double mfunktio();
        char x[LLENGTH];
        char *p,*q;
        char sana[32];
        int len;
        double opnd[MAXARG+4]; char op[MAXARG+4]; int v[MAXARG+4];
        int t,n;
        int narg; /* Usean muuttujan funktion argumenttien lkm     */
        int i;
        double dlag;

//Rprintf("\nlaske: %s",lauseke);
        if (*lauseke=='i')
            {
            if (strncmp(lauseke,"if(",3)==0)
                return(varif(lauseke,y));
            }
        if (*lauseke=='f')
            {
            if (strncmp(lauseke,"for(",3)==0)
                return(arifor(lauseke,y));
            }
        if (*lauseke=='$') // vain FILE MEDIT! 26.5.2003
            {
//     Rprintf("\nlauseke=%s|",lauseke); getck();
            if (muste_strnicmp(lauseke,"$CLASS(",7)==0)
                {
                class_function(lauseke);
                return(0.0);
                }
            if (muste_strnicmp(lauseke,"$MEAN(",6)==0)
                {
                return(mean_function(lauseke+6,y));
                }
            if (muste_strnicmp(lauseke,"$STDDEV(",8)==0)
                {
                return(stddev_function(lauseke+8,y));
                }
            if (muste_strnicmp(lauseke,"$N(",3)==0)
                {
                return(n_cases_function(lauseke+3,y));
                }
            if (muste_strnicmp(lauseke,"$CORR(",6)==0)
                {
                return(corr_function(lauseke+6,y));
                }
            if (muste_strnicmp(lauseke,"$MIN(",5)==0)
                {
                return(min_function(lauseke+5,y));
                }
            if (muste_strnicmp(lauseke,"$MAX(",5)==0)
                {
                return(max_function(lauseke+5,y));
                }
            if (muste_strnicmp(lauseke,"$ORDER(",7)==0)
                {
                return(order_function(lauseke+7,y));
                }
            if (muste_strnicmp(lauseke,"$DAY(",5)==0)
                {
                return(day_function(lauseke+5,y));
                }
            if (muste_strnicmp(lauseke,"$MONTH(",7)==0)
                {
                return(month_function(lauseke+7,y));
                }
            if (muste_strnicmp(lauseke,"$YEAR(",6)==0)
                {
                return(year_function(lauseke+6,y));
                }
            if (muste_strnicmp(lauseke,"$WDAY(",6)==0)
                {
                return(weekday_function(lauseke+6,y));
                }

            }
        strcpy(x,lauseke);
        len=0;
        p=x;
        t=0;
        lag=0;
        while (*p)
            {
            if (l_virhe) return(-1);
            switch (*p)
                {
              case '+':
                if (len==0) { ++p; break; }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='+'; v[t++]=1;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '-':
                if (len==0) { sana[len++]=*p; ++p; break; }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='-'; v[t++]=1;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '*':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='*'; v[t++]=2;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '/':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='/'; v[t++]=2;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '^':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='^'; v[t++]=3;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '[':
                q=strchr(p,']');
                if (len==0 || q==NULL) { syntax_error(lauseke); return(-1); }
                *q=EOS; /* lag=atoi(p+1); */
         /*     sdata2=sdata;    */
                laske(p+1,&dlag); lag=(int)dlag;
         /*     sdata=sdata2;    */
                p=q+1;
                break;

              case '(':

                if (*sana=='p')
                    {
                    if (strncmp(sana,"pos",3)==0)
                        {
                        p+=pos_funktio(p,&opnd[t]);
                        if (l_virhe) return(-1);
                        len=-1;
                        break;
                        }
                    }

                q=p+1;
            if (*q==')') { sprintf(sbuf,"\nArguments missing in %s",lauseke);
                      PR_EBLK; sur_print(sbuf); l_virhe=1; return(-1); }
                n=1;
                narg=1;
                while (n)
                    {
                    ++p;
                    if (*p=='(') { ++n; continue; }
                    if (*p==')') { --n; continue; }
                if (*p==EOS) { sprintf(sbuf,"\n) is missing in %s",lauseke);
                          PR_EBLK; sur_print(sbuf); l_virhe=1; return(-1); }
                    if (*p==',' && n==1)
                        {
                        *p=EOS;

                        laske(q,&opnd[t]);
                        ++t;
                        if (t>MAXARG+3)
                         { sprintf(sbuf,"\nToo many arguments in %s",lauseke);
                     PR_EBLK; sur_print(sbuf); l_virhe=1; return(-1); }
                        ++narg;
                        q=p+1;
                        }

                    }
                if(strchr("+-*/^)\0",*(p+1))==NULL) { syntax_error(lauseke);
                                                      return(-1); }
                *p=EOS; ++p;
                i=laske(q,&opnd[t]);
                if (i<0 || l_virhe) return(-1);
                if (len==0) { len=-1; break; }
                sana[len]=EOS;

                if (narg>1)
                    {
/*
             Rprintf("\nArgumentit: ");
             for (i=t-narg+1; i<=t; ++i) Rprintf(" %g",opnd[i]); getch();
*/
                    t=t-narg+1;
                    if (*sana=='-')
                        opnd[t]=-mfunktio(sana+1,opnd+t,narg);
                    else
                        opnd[t]=mfunktio(sana,opnd+t,narg);
                    if (l_virhe) return(-1);
                    len=-1;
                    break;
                    }

                /* Yhden muuttujan funktiot */
                if (*sana=='-')
                    opnd[t]=-funktio(sana+1,opnd[t]);
                else
                    opnd[t]=funktio(sana,opnd[t]);
                if (l_virhe) return(-1);
                len=-1;
                break;

              case ')':
                sprintf(sbuf,"\n( missing in %s",lauseke);
                PR_EBLK; sur_print(sbuf); l_virhe=1; return(-1);

/*            case ':':
                sdata=atoi(sana+1);
                if (!sdata) { sana[len++]=*p; ++p; break; }
                len=0; ++p;
                break;
*/
              case 'e': case 'E':
                if (strchr("+-.0123456789",sana[0])!=NULL)
                    {
                    sana[len++]=*p; ++p;
                    if (*p!='+' && *p!='-') break;
                    }
              /* default seurattava suoraan case 'e':n jÑlkeen */
              default:
                /* tarkistukset puuttuvat */
                sana[len++]=*p;
                ++p;
                }
            }

        if (len<0) { v[t++]=0; }
        else
                   if (len>0) { opnd[t]=luku(sana,len); 
                   v[t++]=0; }


        supista(&t,opnd,op,v);
        *y=opnd[0];

        return(1);
        }
        
static double luku(char *sana,int len)
        {
        char *p;
        double tulos=1.0;
        int i;

        sana[len]=EOS;
        p=sana; if (*p=='-') ++p;
        if (strchr("1234567890.",*p)==NULL)
            {
            i=laske2(p,&tulos); if (i<0) return((double)1.0);
            if (*sana=='-') return(-tulos);
            return(tulos);
            }
        return(atof(sana));
        }

static double oper(double x1,double x2,char laji)
        {
//        double power();

        if (x1==MISSING8 || x2==MISSING8) return(MISSING8);
        switch (laji)
            {
          case '+':
            return(x1+x2);
          case '-':
            return(x1-x2);
          case '*':
            return(x1*x2);
          case '/':
            if (x2==0.0) { l_virhe=1; return(0.0); }
            return(x1/x2);
          case '^':
            return(muste_pow(x1,x2));
            }
        return(0.0);
        }

static void supista(int *t,double opnd[],char op[],int v[])
        {

        while (*t>1)
            {
            if (v[*t-1]>v[*t-2]) return;
            opnd[*t-2]=oper(opnd[*t-2],opnd[*t-1],op[*t-2]);
            op[*t-2]=op[*t-1]; v[*t-2]=v[*t-1];
            --(*t);
            }
        }
        
static double funktio(char *s,double x)
        {
//        extern double probit();
//        extern double uniform();
//        extern double sur_rand0();
//        extern double round();
        int i;
        double y;
        char S[32];

        if (*s==EOS) return(x);
        if (x==MISSING8) return(x);
        strncpy(S,s,31); S[31]=EOS; muste_strupr(S);

        if (strcmp(S,"RAND")==0) return(sur_rand0(x,1));
        else if (strcmp(S,"URAND")==0) return(sur_rand0(x,2));
        else if (strcmp(S,"SRAND")==0) return(sur_rand0(x,3));
        else if (strcmp(S,"MRAND")==0) return(sur_rand0(x,4));
        else
        if (strncmp(S,"SQR",3)==0)  /* 29.9.1996 x<0.0 etc. */
            { if (x<0.0) { l_virhe=1; return(0.0); } else return(muste_sqrt(x)); }
        else if (strcmp(S,"LOG")==0)
            { if (x<=0.0) { l_virhe=1; return(0.0); } else return(muste_log(x)); }
        else if (strcmp(S,"EXP")==0) return(muste_exp(x));
        else if (strcmp(S,"SIN")==0) return(muste_sin(x));
        else if (strcmp(S,"COS")==0) return(muste_cos(x));
        else if (strcmp(S,"TAN")==0) return(muste_tan(x));
        else if (strcmp(S,"ARCTAN")==0) return(muste_atan(x));
        else if (strcmp(S,"ARCSIN")==0) return(muste_asin(x));   /* 18.6.92 */
        else if (strcmp(S,"ARCCOS")==0) return(muste_acos(x));
        else if (strcmp(S,"ABS")==0) return(muste_fabs(x));
        else if (strcmp(S,"INT")==0) return(muste_floor(x));
        else if (strcmp(S,"ROUND")==0) return(sur_round(x));
        else if (strcmp(S,"SGN")==0) return(muste_sign(x)); // RS CHA
        else if (strcmp(S,"IND")==0) return(muste_ind(x)); // RS CHA
        else if (strcmp(S,"PROBIT")==0) return(probit(x));
        else if (strcmp(S,"RND")==0) return(uniform(x));

        else if (strcmp(S,"LFACT")==0) return(lfact(x)); /* 7.9.2007 */

// RS ADD START
    if (strcmp(S,"TOTIENT")==0) return(totient(x)); // 19.4.2009
    if (strcmp(S,"ZETA")==0) return(zeta(x));
    if (strcmp(S,"LGAMMA")==0) return(muste_lgamma(x)); // RS
    if (strcmp(S,"GAMMA")==0) return(muste_gamma(x)); // RS
    if (strcmp(S,"DIGAMMA")==0) return(muste_digamma(x)); // RS
    if (strcmp(S,"TRIGAMMA")==0) return(muste_trigamma(x)); // RS
    if (strcmp(S,"TETRAGAMMA")==0) return(muste_tetragamma(x)); // RS
    if (strcmp(S,"PENTAGAMMA")==0) return(muste_pentagamma(x)); // RS

            if (*s=='R' && strncmp(s,"R>",2)==0)
                {
                double fx[1]; fx[0]=x;
                y=muste_R_function(s+2,fx,1);
                return(y);
                }

// RS ADD END

/**************************************************
        if (*s=='M' && strncmp(s,"MAT_",4)==0)
            {
            mat_function(s+4,&x,1,&y);
            return(y);
            }
*************************************************/
        i=f_edit(s,&x,1,&y); if (i>0) return(y);
/*      i=f_tiedosto(s,&x,1,&y); if (i>0) return(y); */
// RS REM Not needed anymore        i=family_f(s,&x,1,&y); if (i>0) return(y);
        l_virhe=1;
        return(MISSING8);
        }
        
static double mfunktio(char *s,double *x,int n)
        {
        int i; // RS REM ,k;
        double y;
        char S[32];
/*****************************
       Rprintf("mfunktio: %s\n",s);
     for (i=0; i<n; ++i) Rprintf("%g ",x[i]); Rprintf("\n"); getch();
******************************/
        for (i=0; i<n; ++i) if (x[i]==MISSING8) return(MISSING8);

        strncpy(S,s,31); S[31]=EOS;

// RS ADD START family
    if (strcmp(S,"bin.f")==0 || strcmp(S,"BIN.f")==0 || strcmp(S,"Bin.f")==0 )
    {
        return(muste_pdf_binom(x[2],x[0],x[1]));
    }

    if (strcmp(S,"bin.F")==0 || strcmp(S,"BIN.F")==0 || strcmp(S,"Bin.F")==0 )
    {
        return(muste_cdf_binom(x[2],x[0],x[1]));
    }

    if (strcmp(S,"bin.G")==0 || strcmp(S,"BIN.G")==0 || strcmp(S,"Bin.G")==0 )
    {
        return(muste_inv_binom(x[2],x[0],x[1]));
    }

    if (strcmp(S,"poisson.f")==0 || strcmp(S,"POISSON.f")==0 || strcmp(S,"Poisson.f")==0 )
    {
        return(muste_pdf_poisson(x[1],x[0]));
    }

    if (strcmp(S,"poisson.F")==0 || strcmp(S,"POISSON.F")==0 || strcmp(S,"Poisson.F")==0 )
    {
        return(muste_cdf_poisson(x[1],x[0]));
    }

    if (strcmp(S,"poisson.G")==0 || strcmp(S,"POISSON.G")==0 || strcmp(S,"Poisson.G")==0 )
    {
        return(muste_inv_poisson(x[1],x[0]));
    }

    if (strcmp(S,"N.f")==0 || strcmp(S,"n.f")==0 )
    {
        return(muste_pdf_normal(x[2],x[0],x[1]));
    }

    if (strcmp(S,"N.F")==0 || strcmp(S,"n.F")==0 )
    {
        return(muste_cdf_normal(x[2],x[0],x[1]));
    }

    if (strcmp(S,"N.G")==0 || strcmp(S,"n.G")==0 )
    {
        return(muste_inv_normal(x[2],x[0],x[1]));
    }

    if (strcmp(S,"t.f")==0 || strcmp(S,"T.f")==0 )
    {
        return(muste_pdf_t(x[1],x[0]));
    }

    if (strcmp(S,"t.F")==0 || strcmp(S,"T.F")==0 )
    {
        return(muste_cdf_t(x[1],x[0]));
    }

    if (strcmp(S,"t.G")==0 || strcmp(S,"T.G")==0 )
    {
        return(muste_inv_t(x[1],x[0]));
    }

    if (strcmp(S,"chi2.f")==0 || strcmp(S,"CHI2.f")==0 || strcmp(S,"Chi2.f")==0 )
    {
        return(muste_pdf_chi2(x[1],x[0]));
    }

    if (strcmp(S,"chi2.F")==0 || strcmp(S,"CHI2.F")==0 || strcmp(S,"Chi2.F")==0 )
    {
        return(muste_cdf_chi2(x[1],x[0]));
    }

    if (strcmp(S,"chi2.G")==0 || strcmp(S,"CHI2.G")==0 || strcmp(S,"Chi2.G")==0 )
    {
        return(muste_inv_chi2(x[1],x[0]));
    }

    if (strcmp(S,"F.f")==0 || strcmp(S,"f.f")==0 )
    {
        return(muste_pdf_f(x[2],x[0],x[1]));
    }

    if (strcmp(S,"F.F")==0 || strcmp(S,"f.F")==0 )
    {
        return(muste_cdf_f(x[2],x[0],x[1]));
    }

    if (strcmp(S,"F.G")==0 || strcmp(S,"f.G")==0 )
    {
        return(muste_inv_f(x[2],x[0],x[1]));
    }

    if (strcmp(S,"gamma.f")==0 || strcmp(S,"GAMMA.f")==0 || strcmp(S,"Gamma.f")==0 )
    {
        return(muste_pdf_gamma(x[2],x[0],x[1]));
    }

    if (strcmp(S,"gamma.F")==0 || strcmp(S,"GAMMA.F")==0 || strcmp(S,"Gamma.F")==0 )
    {
        return(muste_cdf_gamma(x[2],x[0],x[1]));
    }

    if (strcmp(S,"gamma.G")==0 || strcmp(S,"GAMMA.G")==0 || strcmp(S,"Gamma.G")==0 )
    {
        return(muste_inv_gamma(x[2],x[0],x[1]));
    }

    if (strcmp(S,"beta.f")==0 || strcmp(S,"BETA.f")==0 || strcmp(S,"Beta.f")==0 )
    {
        return(muste_pdf_beta(x[2],x[0],x[1]));
    }

    if (strcmp(S,"beta.F")==0 || strcmp(S,"BETA.F")==0 || strcmp(S,"Beta.F")==0 )
    {
        return(muste_cdf_beta(x[2],x[0],x[1]));
    }

    if (strcmp(S,"beta.G")==0 || strcmp(S,"BETA.G")==0 || strcmp(S,"Beta.G")==0 )
    {
        return(muste_inv_beta(x[2],x[0],x[1]));
    }

    if (strcmp(S,"weibull.f")==0 || strcmp(S,"WEIBULL.f")==0 || strcmp(S,"Weibull.f")==0 )
    {
        return(muste_pdf_weibull(x[2],x[0],x[1]));
    }

    if (strcmp(S,"weibull.F")==0 || strcmp(S,"WEIBULL.F")==0 || strcmp(S,"Weibull.F")==0 )
    {
        return(muste_cdf_weibull(x[2],x[0],x[1]));
    }

    if (strcmp(S,"weibull.G")==0 || strcmp(S,"WEIBULL.G")==0 || strcmp(S,"Weibull.G")==0 )
    {
        return(muste_inv_weibull(x[2],x[0],x[1]));
    }

    if (strcmp(S,"exp.f")==0 || strcmp(S,"EXP.f")==0 || strcmp(S,"Exp.f")==0 )
    {
        return(muste_pdf_exp(x[1],x[0]));
    }

    if (strcmp(S,"exp.F")==0 || strcmp(S,"EXP.F")==0 || strcmp(S,"Exp.F")==0 )
    {
        return(muste_cdf_exp(x[1],x[0]));
    }

    if (strcmp(S,"exp.G")==0 || strcmp(S,"EXP.G")==0 || strcmp(S,"Exp.G")==0 )
    {
        return(muste_inv_exp(x[1],x[0]));
    }
// RS ADD END

        muste_strupr(S);

// RS ADD START
    /* R-style normal density */
    if (strcmp(S,"DNORM")==0)
    {
        if (n>3) return(muste_density_normal(x[0],x[1],x[2],(int)x[3]));
        return(muste_density_normal(x[0],x[1],x[2],(int)0));
    }

    if (strcmp(S,"MAX")==0) return(muste_max(x,n));
    if (strcmp(S,"MIN")==0) return(muste_min(x,n));
    if (strcmp(S,"MAXN")==0) return(muste_maxn(x,n));
    if (strcmp(S,"MINN")==0) return(muste_minn(x,n));
    if (strcmp(s,"C")==0)
    {
        if (n!=2)
        {
            arg_virhe(s);
        }
        return(muste_C(x[0],x[1]));
    }

    if (strcmp(S,"K_FACT")==0 || strcmp(S,"LK_FACT")==0)
    {
        int h;

        h=0;
        if (*S=='L') h=1;
        if (n!=2)
        {
            arg_virhe(s);
        }
        return(muste_k_fact(x[0],x[1],h));
    }

    if (strcmp(S,"GCD")==0)
    {
        return (gcd(x[0],x[1]));
    }
    if (strcmp(S,"MOD")==0)
    {
        return(muste_mod(x[0],x[1]));
    }
    if (strcmp(S,"ROOT")==0)
    {
        return (root(x[0],x[1]));
    }
    if (strcmp(S,"ROUND")==0)
    {
        return(muste_round(x[0],x[1]));
    }

    /* 14.8.2005 days from 1.1.2000 */
    if (strcmp(S,"DAYS")==0)
    {
        double date;
        sur_julian(x[0],x[1],x[2],&date);
        return(date-2451544.0);
    }

        if (strcmp(S,"NONDIV")==0) // 26.4.2009
            {
            return(nondiv(x[0],x[1]));
            }

        if (strcmp(S,"MTOTIENT")==0) // 30.4.2009
            {
            return(mtotient(x[0],x[1]));
            }

        if (strcmp(S,"BETA")==0) return(muste_beta(x[0],x[1])); // RS
        if (strcmp(S,"LBETA")==0) return(muste_lbeta(x[0],x[1])); // RS

        if (strcmp(S,"FIN.PV")==0) return(muste_fin_pv(x[0],x[1],x[2])); // RS
        if (strcmp(S,"FIN.FV")==0) return(muste_fin_fv(x[0],x[1],x[2])); // RS
        if (strcmp(S,"FIN.PMT")==0) return(muste_fin_pmt(x[0],x[1],x[2])); // RS

        if (strcmp(S,"BOXCOX")==0) return(muste_boxcox(x[0],x[1])); // RS
        if (strcmp(S,"BOXCOX.G")==0) return(muste_inv_boxcox(x[0],x[1])); // RS

        if (strcmp(S,"DISS")==0) return(muste_diss(x[0],x[1],(int)0)); // RS
        if (strcmp(S,"DISS.F")==0) return(muste_diss(x[0],x[1],(int) 1)); // RS

        if (strcmp(S,"BESTVAL")==0) return(muste_bestval(x[0],x[1])); // RS
 
             if (*s=='R' && strncmp(s,"R>",2)==0)
                {
                y=muste_R_function(s+2,x,n);
                return(y);
                }
 
 // RS ADD END
 
/********************************************
        if (*s=='M' && strncmp(s,"MAT_",4)==0)
            {
            mat_function(s+4,x,n,&y);
            return(y);
            }
***********************************************/
        i=f_edit(s,x,n,&y); if (i>0) return(y);
/*      i=f_tiedosto(s,x,n,&y); if (i>0) return(y); */
// RS REM Not needed anymore        i=family_f(s,x,n,&y); if (i>0) return(y);
        l_virhe=1;
        return(MISSING8);
        }


static void arg_virhe(char *s)
		{
        PR_EBLK;
        sprintf(sbuf,"\n%s: Error in arguments",s); sur_print(sbuf);
        l_virhe=1;
    	}

static void syntax_error(char *s)
        {
        sprintf(sbuf,"\nsyntax error in %s",s); PR_EBLK; sur_print(sbuf);
        l_virhe=1;
        }

static int laske2(char *muuttuja,double *y)
        {
        int i; // ,k;
        char *pvar=NULL;

//Rprintf("\nlaske2 muuttuja=%s|",muuttuja);
        if (*muuttuja==EARG)
            {
            *y=earg[atoi(muuttuja+1)];
            return(1);
            }
        if (*muuttuja=='D' && (muuttuja[2]=='>' || muuttuja[3]=='>') )
                              // muutettu    :                   :
            { i=sup_arvo_double(muuttuja,y); return(i); }
        if (lag)
            {
            i=lag_arvo(muuttuja,y); lag=0; if (i<0) l_virhe=1;
            return(i);
            }
        i=spfind(muuttuja);
//Rprintf("\nspfind-muuttuja i=%d|",i);
        if (i<0)
            {
            i=varfind2_medit(&d,muuttuja,0);  /* itse outputmuuttuja */
            if (i<0)
                {
                sprintf(sbuf,"Value of %s not found!",muuttuja);

        		putsaa(); cursor_medit(r3-1,0); // RS ADD
            	PR_EBLK; sur_print(sbuf);
        		if (miss_wait) { play_sound(4); sur_sleep(2000L); miss_wait=FALSE; } // RS ADD
        		else sur_sleep(20L);
        		putsaa(); // RS ADD
                *y=MISSING8; // RS CHA l_virhe=1; return(-1);
                }
            else data_load(&d,jnro,i,y);
            return(1);
            }
        if (spb[i]==NULL) { *y=arvo[i]; return(1); }
        if (nvar) { pvar=spa[i]; spa[i]=NULL; }
        laske(spb[i],y); // k=
        if (nvar) spa[i]=pvar;
        arvo[i]=*y;
        spb[i]=NULL;
        return(1);
        }

static int arifor(char *lauseke,double *y)
        {
        int g; // RS REM ,i;
        char *sana[4],*laus[4];
        char x[LLENGTH];
        long ialku,iloppu,il,imax=0;
        double d;
        char *p;
        char sterm[LLENGTH];
        double term,sum;
        int iterm,iind,tind=0;
        char esana[7];
        int max;

        strcpy(x,lauseke);
        g=parsplit(x,sana,laus,4);
        if (g<0) return(-1);
        if (g<3) { if_syntax_error(lauseke); return(-1); }
/*
   for (i=0; i<g; ++i) Rprintf("\nfor: %d %s %s",i,sana[i],laus[i]); getch();
*/
        p=strchr(laus[0],'=');
        if (p==NULL) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;   /* laus[0]='i'  */
        laske(p+1,&d); ialku=d;
        laske(laus[1],&d); iloppu=d;

        iterm=0;
        if (strcmp(sana[2],"term")==0)
            {
            iterm=1;
            p=strchr(laus[2],'=');
            if (p==NULL) term=0.0;
            else { *p=EOS; laske(p+1,&term); }  /* laus[2]='term' */
            }
        strcpy(sterm,laus[2+iterm]);
        iind=aseta_earg((double)ialku,esana);
        if (iind<0) return(-1);
        korvaa(sterm,laus[0],esana);
        if (iterm)
            {
            tind=aseta_earg(term,esana);
            if (iind<0) return(-1);
            korvaa(sterm,laus[2],esana);
            }

        max=0; p=sana[2+iterm];
        if (strncmp(p,"max",3)==0 || strncmp(p,"min",3)==0)
            {
            if (p[2]=='n') { max=3; sum=1e300; if (p[3]=='i') max=4; }
            else           { max=1; sum=-1e300; if (p[3]=='i') max=2; }
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
              if (iterm) { earg[tind]=term; if (il>ialku) laske(sterm,&term); }
                else laske(sterm,&term);
                if (max<3 && term>sum) { sum=term; imax=il; }
                if (max>2 && term<sum) { sum=term; imax=il; }  /* imax=imin */
                }
            }
        else if (strcmp(p,"sum")==0)
            {
            sum=0.0;
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm) { earg[tind]=term;
                               if (il>ialku) laske(sterm,&term); }
                else laske(sterm,&term);
                sum+=term;
                }
            }
        else if (strcmp(p,"product")==0)
            {
            sum=1.0;
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm) { earg[tind]=term;
                               if (il>ialku) laske(sterm,&term); }
                else laske(sterm,&term);
                sum*=term;
                }
            }
        else if (strcmp(p,"term")==0)
            {
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
              if (iterm) { earg[tind]=term; if (il>ialku) laske(sterm,&term); }
                else laske(sterm,&term);
                }
            sum=term;
            }
        else { if_syntax_error(lauseke); return(-1); }

        if (max==2) *y=(double)imax; else *y=sum;
        n_earg-=1+iterm;
        return(1);
        }

static int parsplit(char *x,char **a,char **b,int max)
        {
        int i,sulut;
        char *p;
        char y[LLENGTH];

        strcpy(y,x);
        i=0;
        p=x;
        while (*p)
            {
            a[i]=p;
            while (*p)
                {
                if (*p=='(') break;
                if (*p==')') { if_syntax_error(y); return(-1); }
                ++p;
                }
            if (*p==EOS) { if_syntax_error(y); return(-1); }
            *p=EOS;
            b[i]=++p;
            sulut=1;
            while (*p)
                {
                if (*p==')') { --sulut; if (!sulut) break; }
                else if (*p=='(') ++sulut;
                ++p;
                }
            if (sulut) { if_syntax_error(y); return(-1); }
            *p=EOS;
            ++p; if (*p==EOS) break;
            ++i;
            if (i>=max) { if_syntax_error(y); return(-1); }
            }
        return(i+1);
        }

static int varif(char *lauseke,double *y)
        {
        char *a,*b,*c,*d;
        char rel=0;
        char *p;
        int sulut;
        char x[LLENGTH];
        double y1;
        int tosi;

/*      Rprintf("\nvarif: %s",lauseke); getch();     */
        /* if(<a><rel><b>)then(<c>)else(<d>)
           <a>,<b>,<c>,<d> lausekkeita
           <rel>: =,>,<,<>,>=,<=
                  = > < E  S  P
        */

        strcpy(x,lauseke);
        a=x+3;  /* if( ohitetaan */
        p=a; sulut=0;
        while (*p)
            {
            switch(*p)
                {
              case '=':
                rel=*p; *p=EOS; break;
              case '<':

                if (*(p+1)=='=') { rel='P'; *p=EOS; ++p; *p=EOS; break; }
                if (*(p+1)=='>') { rel='E'; *p=EOS; ++p; *p=EOS; break; }
                rel=*p; *p=EOS; break;
              case '>':
                if (*(p+1)=='=') { rel='S'; *p=EOS; ++p; *p=EOS; break; }
                rel=*p; *p=EOS; break;
              case ')':
                --sulut; ++p;
                if (sulut<0)
                    {
                    sprintf(sbuf,"\nrelation symbol =<> missing! in %s",x);
                    PR_EBLK; sur_print(sbuf); WAIT; l_virhe=1; return(-1);
                    }
                break;
              case '(':
                ++sulut; ++p;
                break;
              default:
                ++p;
                }
            }

/*  Rprintf("\na=%s rel=%c",a,rel);  */
        b=p+1;
        p=b;
        while (1)
            {
            p=strchr(p,')');
            if (p==NULL) { if_syntax_error(lauseke); return(-1); }
            if (strncmp(p,")then(",6)==0) { *p=EOS; break; }
            ++p;
            }
/*  Rprintf(" b=%s",b);   getch();  */
        c=p+6;
        p=c; sulut=0;
        while (*p)
            {
            if (*p=='(') { ++sulut; ++p; continue; }
            if (*p==')')
                {
                if (!sulut) break;
                --sulut;
                }
            ++p;
            }
        if (*p==EOS) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;
        if (strncmp(p+1,"else(",5)!=0) { if_syntax_error(lauseke); return(-1); }
        d=p+6;
        p=d; sulut=0;
        while (*p)
            {
            if (*p=='(') { ++sulut; ++p; continue; }
            if (*p==')')
                {
                if (!sulut) break;
                --sulut;
                }
            ++p;
            }
        if (*p==EOS) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;
/* Rprintf(" c=%s d=%s",c,d);
getch();
*/

        if (strncmp(a,"str(",4)==0)  // kokeilu 28.6.2003
            {
            tosi=strvert(a,rel,b,c,d,y);
            if (tosi<0) return(-1);
            }
        else

            {
            laske(a,y);
            if (*y==MISSING8) return(1); // 8.6.2003
            laske(b,&y1);
            if (y1==MISSING8) { *y=MISSING8; return(1); } // 8.6.2003
            tosi=0;
            switch (rel)
                {
              case '=': if (*y==y1) tosi=1; break;
              case '<': if (*y<y1) tosi=1; break;
              case '>': if (*y>y1) tosi=1; break;
              case 'E': if (*y!=y1) tosi=1; break;
              case 'P': if (*y<=y1) tosi=1; break;
              case 'S': if (*y>=y1) tosi=1; break;
                }
            }

        if (tosi) laske(c,y);
        else      laske(d,y);
        return(1);
        }

static void if_syntax_error(char *x)
        {
        PR_EBLK;
        sprintf(sbuf,"\nSyntax error in %s",x); sur_print(sbuf);
        WAIT; l_virhe=1;
        }
        

static int f_edit(char *s,double *x,int n,double *py)
        {
        int i,k,len;
        char lauseke[LLENGTH];
        char xx[LLENGTH], *osa[MAXARG];
        char sana[7];     /*  EARG 1 2 3 4 EARG EOS */
        double y;



        len=strlen(s); s[len++]='(';
        i=0;
/**************************************************
for (k=0; k<spn; ++k)
    {
    Rprintf("%d | %s | %s |    \n",k,spa[k],spb[k]);
    }
getch();
***************************************************/

        while (i<spn &&
               ( spa[i]==NULL || *spa[i]==EOS ||
           spa[i][strlen(spa[i])-1]!=':' || strncmp(s,spa[i],len)!=0) )
                 ++i;

        if (i==spn) { s[len-1]=EOS; return(-1); }

/*      if (!earg_varattu) { k=varaa_earg(); if (k<0) return(-1); } */
        strcpy(lauseke,spb[i]);
        strcpy(xx,spa[i]);
        i=split(xx+len,osa,MAXARG);
        if (i!=n)
           {
           PR_EBLK;
           sprintf(sbuf,"\nArgument error in function %s",s); sur_print(sbuf);
           l_virhe=1; WAIT; return(-1);
           }
        osa[n-1][strlen(osa[n-1])-2]=EOS;  /* ): poistetaan */
/*
    for (i=0; i<n; ++i) Rprintf("osa %d: %s\n",i+1,osa[i]); getch();
*/
        for (i=0; i<n; ++i)
            {
            k=aseta_earg(x[i],sana); if (k<0) return(-1);
            korvaa(lauseke,osa[i],sana);
            }
        laske(lauseke,&y);
        *py=y;
        n_earg-=n;
        return(1);
        }

static void korvaa(char *s,char *x,char *y)
        {
        char *p,*q;
        char z[LLENGTH];
        int len=strlen(x);

        *z=EOS;
        p=s;
        while ((q=strstr(p,x))!=NULL)
            {
            strncat(z,p,q-p);
            strcat(z,y);
            p=q+len;
            }
        strcat(z,p);
        strcpy(s,z);
        }

static int varaa_earg()
        {
        int i;

        earg=(double *)malloc(MAXEARG*sizeof(double));
        if (earg==NULL)
            {
            PR_EBLK;
            sur_print("\nNot enough memory! (MAXEARG=255)");
            l_virhe=1;
            WAIT; return(-1);
            }
 		for (i=0; i<MAXEARG; ++i) earg[i]=0.0;
        earg_varattu=1;
        return(1);
        }

static int aseta_earg(double luku,char *sana)
        {
        char sana2[5];

        sana[0]=EARG;
        if (n_earg>=MAXEARG)
            {
            PR_EBLK;
            sur_print("\nStack overflow in editorial functions!");
            sur_print("\nMAXEARG=255");
            WAIT; l_virhe=1;
            return(-1);
            }
        sana[1]=EOS; strcat(sana,muste_itoa(n_earg,sana2,10));
        earg[n_earg++]=luku;
        return(n_earg-1);
        }

/* RS REM
static int varnimet()
        {
        extern SURVO_DATA d;
        int i,k;
        char nimi[LLENGTH];

//      for (i=0; i<d.m_act; ++i)
        for (i=0; i<d.m; ++i)
            {
//          *nimi=EOS; strncat(nimi,d.varname[d.v[i]],8); // 22.5.2003
            *nimi=EOS; strncat(nimi,d.varname[i],8); // 22.5.2003
            k=strlen(nimi); while (nimi[k-1]==' ') nimi[--k]=EOS;

            if (spn>=specmax) return(-spn);
            if (spl-splist+k+1>speclist) return(-spn);

            strncpy(spl,nimi,k);
            spa[spn]=spl; spb[spn]=NULL;
            spl+=k+1; *(spl-1)=EOS;
            ++spn;
            }
        return(spn);
        }
*/

static int lue_arvot(long j)
        {
//        extern SURVO_DATA d;
        int i,k;

//      for (i=0; i<d.m_act; ++i)
        for (i=0; i<d.m; ++i)
            {
//          k=data_load(&d,j,d.v[i],&arvo[i]);
            k=data_load(&d,j,i,&arvo[i]);
            if (k<0) return(-1);
// Rprintf(" %d: %g|",i,arvo[i]);
            }
        arvo[spn_order]=(double)j;  /* ORDER */
        return(1);
        }

static int lag_arvo(char *muuttuja,double *y)
        {
        int i;
        long j;
        extern SURVO_DATA d;

        j=jnro+(long)lag;
        if (j<1L || j>d.n) { *y=MISSING8; return(1); }
        i=varfind(&d,muuttuja); if (i<0) return(-1);
        data_load(&d,j,i,y);
        return(1);
        }

static int sp2_init()
        {
        int i;
        extern SURVO_DATA d;

        i=sp_write("MISSING",MISSING8); if (i<0) return(-1);
        i=sp_write("ORDER",MISSING8); if (i<0) return(-1);
        spn_order=spn-1;
        i=sp_write("N",(double)d.n); if (i<0) return(-1);
        return(1);
        }


static int sp_write(char *nimi,double y)
        {
        int k=strlen(nimi);

        strncpy(spl,nimi,k);
        spa[spn]=spl; spb[spn]=NULL;
        spl+=k+1; *(spl-1)=EOS;
        arvo[spn]=y;
        ++spn;
        return(1);
        }    
        
static int medit_tut_end()
    {
	extern FILE *tutor;
	FILE *temptut;    
    long tutpos;

//  if (!etu) return(1);
    if (!etu) tutpos=0L;
    else { tutpos=muste_ftell(tutor); fclose(tutor); }

    sprintf(sbuf,"%sSURVOMD2",etmpd);
    temptut=muste_fopen2(sbuf,"wt");
    fprintf(temptut,"%ld\n",tutpos);
    muste_fclose2(temptut);
// Rprintf("\ntutpos=%ld|",tutpos); getck();
    return(1);
    }

static int pos_funktio(char *s,double *y)      /* pos(var,char) tai pos(var,start_pos,char) */
        {
        char *q; // RS REM ,*p;
        int var;
        char arvo[LLENGTH];
        double a;
        int start_pos;
        char x[LLENGTH],*osa[3];
        char x2[LLENGTH];
        int i,k,h;
// RS REM        char ch;

        strcpy(x,s);
        i=sulku_split(x,osa,3,&k);
        var=varfind(&d,osa[0]);
        if (var<0) return(-1);
        if (d.vartype[var][0]!='S')  { not_string(osa[0]); return(-1); }
        data_alpha_load(&d,jnro,var,arvo);
        h=d.varlen[var];
        arvo[h]=' '; arvo[h+1]=EOS;
        start_pos=1;
        if (i==3)
            {
            strcpy(x2,osa[1]);
            laske(x2,&a);
            start_pos=a;
            }

        strcpy(x2,osa[i-1]);
        if (muste_strcmpi(x2,"sp")==0 || muste_strcmpi(x2,"space")==0) { *x2=' '; x2[1]=EOS; }
        if (muste_strcmpi(x2,"comma")==0) { *x2=','; x2[1]=EOS; }

        q=strstr(arvo+start_pos-1,x2);
        if (q==NULL) *y=0.0;
        else *y=(double)(q-arvo+1);

        return(k+1);
        }

static int sulku_split(char *x,char **osa,int n,int *pk)
        {
        char *p;
        int sulut;
        int h;

        p=x+1;
        osa[0]=p;
        h=1; *pk=1;
        sulut=0;
        while (1)
            {
            ++p; ++*pk;
            if (*p=='(') { ++sulut; continue; }
            if (*p==')')
                {
                --sulut; if (sulut<0) { *p=EOS; return(h); }
                }
            if (*p==',')
                {
                if (!sulut)
                    {
                    *p=EOS;
                    osa[h]=p+1;
                    ++h;
                    if (h>n) return(n);
                    }
                }
            }
        }

static void not_string(char *s)
        {
        sprintf(sbuf,"\n%s is not a string variable!",s);
        PR_EBLK; sur_print(sbuf); l_virhe=1; WAIT;
        }

static int tutki_str_lauseke(char *x,int *pvar,int *plag,int *pstart,int *plen,int *pk)
        {
        int i; // RS REM ,k;
        char *osa[3];
        char x2[LLENGTH];
        double a;
        char *p,*q;

        i=sulku_split(x,osa,3,pk);
        *plag=0;
        p=strchr(osa[0],'[');
        if (p!=NULL)
            {
            *p=EOS; q=strchr(p+1,']');
            if (q==NULL)
                {
                PR_EBLK;
                sur_print("\n] missing!");
                WAIT; return(-1);
                }
            *q=EOS; *plag=atoi(p+1);
            }
        *pvar=varfind(&d,osa[0]);
        if (*pvar<0) return(-1);
        if (d.vartype[*pvar][0]!='S')  { not_string(osa[0]); return(-1); }
        *pstart=1;
        if (i>1)
            {
            strcpy(x2,osa[1]);
            laske(x2,&a);
            *pstart=(int)a;
            }
        *plen=d.varlen[*pvar]-*pstart+1;
        if (i==3)
            {
            strcpy(x2,osa[2]);
            laske(x2,&a);
            *plen=(int)a;
            if (*plen<0)
                {
                PR_EBLK;
                sur_print("\nNegative length in str()!");
                WAIT; return(-1);
                }
            }
        return(1);
        }

static int str_laske(char *lauseke,char *tulos)
        {
        char x[LLENGTH];
//      char tulos[LLENGTH];
        char sana[LLENGTH];
        char sana2[LLENGTH];
        char *p,*q;
        int i,k;
        int var1,lag,start,len;

        strcpy(x,lauseke);
        p=x;
        *tulos=EOS;
/*********************************
        strcpy(sana,str_vasen);
        i=tutki_str_lauseke(sana,&str_var,&str_lag,&str_var_start,&str_var_len,&k);
        var[0]=str_var; // lausekkeen merkintÑÑ varten!
        if (i<0) return(-1);
******************************/
        while (*p)
            {
            if (*p=='"')
                {
                q=p+1; while (*q && *q!='"') ++q;
                *q=EOS; strcpy(sana,p+1);
                p=q+1;
                }
            else if (muste_strnicmp(p,"str(",4)==0)
                {
                i=tutki_str_lauseke(p+3,&var1,&lag,&start,&len,&k);
                if (i<0) return(-1);
                p+=k+4;
                data_alpha_load(&d,jnro+(long)lag,var1,sana2);
                strncpy(sana,sana2+start-1,len); sana[len]=EOS;
                }
            else if (muste_strnicmp(p,"comma",5)==0) /* 13.3.1991 */
                {
                strcpy(sana,",");
                p+=5;
                }
            else if (muste_strnicmp(p,"space",5)==0) /* 15.3.1991 */
                {
                strcpy(sana," ");
                p+=5;
                }
            else if (muste_strnicmp(p,"sp",2)==0)
                {
                strcpy(sana," ");
                p+=2;
                }
            else
                {
                sprintf(sbuf,"\nError in %s",lauseke);
                PR_EBLK; sur_print(sbuf); WAIT; return(-1);
                }
            if (*p=='|')  /* 22.1.1996 */
                {
                ++p;
                i=strlen(sana)-1; while (i>=0 && sana[i]==' ') sana[i--]=EOS;
                strcat(tulos,sana);
                continue;
                }
            strcat(tulos,sana);
            if (*p=='&') { ++p; continue; }
            if (*p==EOS) break;
            sprintf(sbuf,"\n& or \" missing in %s",lauseke);
            PR_EBLK; sur_print(sbuf); WAIT; return(-1);
            }
//      data_alpha_load(&d,jnro+(long)str_lag,str_var,sana);
        for (i=0; i<str_var_len && i<strlen(tulos); ++i)
            sana[i+str_var_start-1]=tulos[i];
// Rprintf("\ntulos=%s|",tulos); getck();
/***********************************
            if (d.type==2) fi_alpha_save(&d.d2,jnro+(long)str_lag,str_var,sana);
            else if (d.type==1) ma_save(&d.d1,(int)(jnro+str_lag),str_var,sana);
            else
                {
                sur_print("\nCannot save data values!");
                WAIT; return(-1);
                }
*******************************/
        return(1);
        }

static int strvert(char *a,char rel,char *b,char *c,char *dd,double *y)
        {
        int i,tosi;
        char s1[LLENGTH],s2[LLENGTH];
        int v;

        if (!code_ind)
            {
            code_ind=1;
            for (i=0; i<256; ++i) code[i]=(unsigned char)i;
            i=spfind("FILTER");
            if (i>=0) { code_ind=2; i=load_codes(spb[i],code); if (i<0) return(-1); }
            }
        i=str_arvo(a,s1); if (i<0) return(-1);
        i=str_arvo(b,s2); if (i<0) return(-1);

        tosi=0;
        v=strcmp(s1,s2);
        switch (rel)
            {
          case '=': if (!v) tosi=1; break;
          case '<': if (v<0) tosi=1; break;
          case '>': if (v>0) tosi=1; break;
          case 'E': if (v) tosi=1; break;
          case 'P': if (v<=0) tosi=1; break;
          case 'S': if (v>=0) tosi=1; break;
            }
        return(tosi);
        }

static int str_arvo(char *a,char *s)
        {
        char x[LLENGTH];
        int i,k;
        char *p; // RS REM ,*q;

        if (*a=='"')
            {
            strcpy(s,a+1);
            p=strchr(s,'"');
            if (p!=NULL) *p=EOS;
            }
        else
            {
            if (muste_strnicmp(a,"str(",4)!=0) /* 4.3.1996 */
                {
                not_string(a);
                return(-1);
                }
            strcpy(x,a+3);
            i=tutki_str_lauseke(x,&str_var,&str_lag,&str_var_start,&str_var_len,&k);
            if (i<0) return(-1);
            data_alpha_load(&d,jnro+(long)str_lag,str_var,x);
            strncpy(s,x+str_var_start-1,str_var_len); s[str_var_len]=EOS;
            }
        if (code_ind==2) conv((unsigned char *)s); // RS ADD (unsigned char *)
        return(1);
        }


static int load_codes(char *codefile,unsigned char *code)
        {
        FILE *codes;
        int i;
        char x[LLENGTH];

        strcpy(x,codefile);
//        if (strchr(x,':')==NULL && *x!='.')
		if (!muste_is_path(x))
            { strcpy(x,survo_path); strcpy(x,"SYS/"); strcat(x,codefile); }

        codes=muste_fopen2(x,"rb");
        if (codes==NULL)
            {
   sprintf(sbuf,"\nCode conversion file %s not found!",x); PR_EBLK; sur_print(sbuf);
            WAIT; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose2(codes);
        return(1);
        }

static void conv(unsigned char *sana)
        {
        int i;

        for (i=0; i<strlen((char *)sana); ++i) sana[i]=code[sana[i]]; // RS ADD (char *)
        }
        
static int sup_arvo(char *muuttuja,char *s)
        {
        int i,k,i1,i2;
        char type1,type2;
        double a1,a2;
        long j=0;
        int sdata;
        char *p,*q;
        double y;
        char match[LNAME];
        char x[LNAME];

        p=strchr(muuttuja,':');
        if (p==NULL)
 { sprintf(sbuf,"Error in %s",muuttuja); PR_EBLK; sur_print(sbuf); WAIT; return(-1); }
        *p=EOS; ++p;
        sdata=atoi(muuttuja+1);
        if (sdata<1 || sdata>ndata)
            {
 sprintf(sbuf,"\nIndata D%d: not defined!",sdata); PR_EBLK; sur_print(sbuf); WAIT; return(-1);
            }
        k=sdata-1;

        q=strchr(p,'>');
        if (q!=NULL)
            {
            *q=EOS; ++q;
            i2=varfind(&sd[k],q,0);
            if (i2<0) { l_virhe=1; return(-1); }
            type2=*sd[k].vartype[i2];
            i1=varfind(&d,q,0);
            if (i1<0) { l_virhe=1; return(-1); }
            type1=*d.vartype[i1];
            if (type1!='S' || type2!='S') type1='8'; // num.comp
            if (type1=='S')
                {
                data_alpha_load(&d,jnro,i1,match);
                if (sup_jnro[k]>0L)
                    {
                    data_alpha_load(&sd[k],sup_jnro[k],i2,x);
                    if (strcmp(match,x)==0) j=sup_jnro[k];
                    else
                        {
                        ++sup_jnro[k];
                        data_alpha_load(&sd[k],sup_jnro[k],i2,x);
                        if (strcmp(match,x)==0) j=sup_jnro[k];
                        else sup_jnro[k]=0L;
                        }
                    }
                if (sup_jnro[k]==0L)
                    {
                    for (j=sd[k].l1; j<=sd[k].l2; ++j)
                        {
                        data_alpha_load(&sd[k],j,i2,x);
                        if (strcmp(match,x)==0) break;
                        }
                    if (j>sd[k].l2) { strcpy(s,"?"); return(1); }
                    sup_jnro[k]=j;
                    }
                } // type1='S'
            else
                {
                data_load(&d,jnro,i1,&a1);
                if (sup_jnro[k]>0L)
                    {
                    data_load(&sd[k],sup_jnro[k],i2,&a2);
                    if (a1==a2) j=sup_jnro[k];
                    ++sup_jnro[k];
                    data_load(&sd[k],sup_jnro[k],i2,&a2);
                    if (a1==a2) j=sup_jnro[k];
                    else sup_jnro[k]=0L;
                    }
                if (sup_jnro[k]==0L)
                    {
                    for (j=sd[k].l1; j<=sd[k].l2; ++j)
                        {
                        data_load(&sd[k],j,i2,&a2);
                        if (a1==a2) break;
                        }
                    if (j>sd[k].l2) { strcpy(s,"?"); return(1); }
                    sup_jnro[k]=j;
                    }
                } // type1='8'
            }
        else
            {
            j=jnro+(long)lag; // lag: onko tarpeen?
            lag=0; /* 6.10.1989 */
            if (j<1L || j>sd[k].n) { strcpy(s,"?"); return(1); }
            }
        i=varfind2_medit(&sd[k],p,0);
        if (i<0)
            {
            sprintf(sbuf,"\nField %s not found in data %s!",p,sdat[k]);
            PR_EBLK; sur_print(sbuf); WAIT; return(-1);
            }
        if (sd[k].vartype[i][0]=='S')
            {
            data_alpha_load(&sd[k],j,i,s);
            i=strlen(s)-1; while (i>0 && s[i]==' ') s[i--]=EOS;
            }
        else
            {
            data_load(&sd[k],j,i,&y);
            if (y==MISSING8) strcpy(s,"?");
            else fconv(y,"",s);
            }
        return(1);
        }

static int sup_arvo_double(char *muuttuja,double *y)
        {
        int i,k;
        long j;
        int sdata;
        char *p;

        p=strchr(muuttuja,':');
        if (p==NULL) { sprintf(sbuf,"Error in %s",muuttuja); sur_print(sbuf); WAIT;
                       l_virhe=1; return(-1); } 
        *p=EOS; ++p;
        sdata=atoi(muuttuja+1);
        if (sdata<1 || sdata>ndata)
            {
            sprintf(sbuf,"\nIndata D%d: not defined!",sdata); sur_print(sbuf); WAIT;
                    l_virhe=1; return(-1);
            }
        k=sdata-1;
        j=jnro+(long)lag;
        lag=0; /* 6.10.1989 */
        if (j<1L || j>sd[k].n) { *y=MISSING8; return(1); }
        i=varfind2_medit(&sd[k],p,0);
        if (i<0)
            {
            sprintf(sbuf,"\nField %s not found in data %s!",p,sdat[k]);
            sur_print(sbuf); WAIT; l_virhe=1; return(-1); 
            }
        data_load(&sd[k],j,i,y);
        return(1);
        }

static void var2()
        {
        int i,k;
// RS REM        char *p;
// RS REM        char nimi[LLENGTH];
// RS REM        char x[LLENGTH], *pdat[NDATA];

        poista_var();
        vm_act=d.m_act;
/***************************************
        for (i=0; i<d.m_act; ++i)
            {
            strcpy(x,d.varname[d.v[i]]);
            if (strncmp(x,"IND ",4)==0 || strncmp(x,"CASES ",6)==0 || strncmp(x,"SELECT ",7)==0)
                {
                sur_print("\nField names IND, CASES and SELECT are not allowed!");
                sur_print("\nChange such a name by FILE STATUS and FILE UPDATE.");
                WAIT; return;
                }
            }
****************************************/
//      i=sp_init(r1+r-1,d.m_act); if (i<0) { spec_error(); return; }

        i=sp_init_medit(r1+r-1,d.m); if (i<0) { spec_error(); return; }

        i=sp2_init(); if (i<0) { spec_error(); return; } /* MISSING,ORDER,N */
        for (i=0; i<spn; ++i) spb2[i]=spb[i]; // 10.5.2003
/************************
printf("\nspec:");
for (i=0; i<spn; ++i) Rprintf(" %s=%s|",spa[i],spb[i]); getch();
***************************/

        ndata=0;
/**********************************
        i=spfind("INDATA");
        if (i>=0)
            {
            strcpy(sdat_list,spb[i]);
**************************************/
            ndata=split(sdat_list,sdat,NDATA);
            for (i=0; i<ndata; ++i)
                {
                k=data_read_open(sdat[i],&sd[i]); if (k<0) { /* s_end(argv[1]); */ return; }
                }
//          }
        varaa_earg();   /* 4.11.1998 */
/**********************************
        muunto();
        for (i=0; i<spn; ++i) spb[i]=spb2[i];
        uudet_nimet();
        data_close(&d);
        for (i=0; i<ndata; ++i) data_close(&sd[i]);
        outseed();
        s_end(argv[1]);
************************************/
        return;
        }

static void spec_error()
        {
        PR_EBLK;
        sprintf(sbuf,"\nToo many active variables + specifications (specmax=%d)",
                        specmax); sur_print(sbuf);
        sprintf(sbuf,"\nor too much text in specifications (speclist=%d)",speclist);
        sur_print(sbuf); WAIT;
        }

static void poista_var()
        {
        int i,k,h;

        k=0;
        for (i=0; i<d.m_act; ++i)
            {
            if (!nvar) { if (d.v[i]==pvar[0]) { ++k; continue; } }
            else
                {
                for (h=0; h<nvar; ++h)
                    {
                    if (d.v[i]==pvar[h]) { ++k; break; }
                    }
                if (h<nvar) continue;
                }
            d.v[i-k]=d.v[i];
            }
        d.m_act-=k;
        }

static int update_specs()
    {
    int i;
    for (i=0; i<spn; ++i) spb[i]=spb2[i];
    i=lue_arvot(jnro); if (i<0) return(-1);

    return(1);
    }

static int var_error(char *s)
        {
        int i;

        sprintf(sbuf,"\nError in %s",s);

        sur_locate(r3,1);
        PR_EBLD;
        strcat(sbuf,"  Press any key!");
        play_sound(3);
        for (i=0; i<5; ++i)
            {
            soft_bottom_line_erase_medit();
            sur_wait(70L);
            write_string(sbuf,strlen(sbuf),'5',r3,0); // 22.6.2003
            sur_wait(300L);
            }
        nextch_medit();
        soft_bottom_line_erase_medit();
        l_virhe=1; // 20.9.2003
        return(1);
        }

static int varfind2_medit(SURVO_DATA *d,char *nimi,int virheilm)   /* kuten varfind, vain ilm.valinta lisatty */
        {
        int len;
        int i;

        if (*nimi=='#')
            {
            i=atoi(nimi+1)-1;
            if (i<0 || i>d->m-1)
                {
                if (virheilm)
                    { sprintf(sbuf,"\nIllegal var %s",nimi); sur_print(sbuf); WAIT; }
                return(-1);
                }
            return(i);
            }
        len=strlen(nimi);
      if (len<=8) // 22.9.2003
        for (i=0; i<d->m; ++i)
            {
            if ( strncmp(nimi,d->varname[i],len)==0 &&
                ( (d->varname[i][len]==' ' || d->varname[i][len]==EOS ) || len==8 ) )
            return(i);
            }
        if (virheilm)
            {
            sprintf(sbuf,"Variable %.8s not found!",nimi); sur_print(sbuf);
            if (etu==2)
                {
                sprintf(tut_info,"___@3@VARFIND@%s@",sbuf); return(-1);
                }
            sur_print("\n"); sur_print(sbuf);
            WAIT;
            }
        return(-1);
        }

int headline_medit()
        {
        char x[LLENGTH];
        int k; // RS REM ,k2;
//        char dispm2;
        char hshadow; /* 25.11.1992 */
        char aika[64];
//        extern char data_name[];
//        extern long jnro;
//        extern long dat_n;

// int rr,cc;

        pvmaika(aika);
       /* dispm2=' '; */ hshadow='4';

        write_string(space,c3-1,hshadow,1,1);
//      sprintf(x,"      ");
//      write_string(x,6,hshadow,1,1);

        sprintf(x,"   "); write_string(x,3,hshadow,1,7);
        sprintf(x," %s ",system_name);
        write_string(x,strlen(system_name)+2,'7',1,10);

//      k=20+c3-72;

//      strcpy(sbuf,edisk); unsubst_survo_path_in_editor(sbuf);


//      sprintf(x,"  %s %*.*s             ",aika,k,k,space);
        sprintf(x,"  %s ",aika); k=strlen(x);
        write_string(x,k,hshadow,1,20);

        sprintf(x," %s  %ld/%ld ",data_name,jnro,dat_n);

        write_string(x,strlen(x),'7',1,20+k);




/****************************************
        sprintf(x,"%d",dispm);
        if (large_field) dispm2='P'; else dispm2='4';
        write_string(" ",1,dispm2,1,c3+8-1);

        dispm2='0'+dispm; if (dispm==0) dispm2=' ';
        write_string(x,1,dispm2,1,c3+8);
        PR_ENRM;
*******************************************/
/**********************************
        check_alarm(aika);

        if (!etu && !wait_tut_type)
            {
            k=hae_apu("wait_tut",sbuf);
            if (k)
                {
                if (*sbuf)
                    {
                    split(sbuf,parm,3);
                    wait_tut2(parm[0],parm[1],parm[2]);
                    }
                }
            }
************************************/
        return(1);
        }

static int nextch_medit()
        {
        extern int muste_mousewheel;        
        int m;

muste_mousewheel=FALSE;

        if (etu==2)
            {
            m=tutch();
            while (m==255 && etu==2) m=tutch();
            if (m!=0) { special=is_special(m); return(m); }
            }
        if (etu==1)
            {
            m=nextkey();
//          cursor_medit(2,50); sprintf(sbuf,"%d  ",m); sur_print(sbuf); getck();
//          cursor_medit(r,c);
// 4.5.2003 if (wait_save) save_wait(m);
            tutsave(m); return(m);
            }
        m=nextkey();
        muste_mousewheel=TRUE;
        return(m);
        }

static int is_special(int m)
    {
    int i;

    i=0;
    switch (m)
        {
      case CODE_EXIT:
      case CODE_NEXT:
      case CODE_PREV:
      case 145: // ctrl-PgDn
      case 146: // ctrl-PgUp
      case 147: // ctrl-Right
      case 149: // alt-PgDn
      case 150: // alt-PgUp
      case 1001: // sounds on/off (soft_key)
      case CODE_DOWN:
      case CODE_UP:
      case CODE_HOME:
      case CODE_RETURN:
      case CODE_TAB:
      case CODE_RIGHT:
      case CODE_LEFT:
      case CODE_BACKSP:
      case CODE_DELETE:
      case CODE_INSERT:
      case CODE_ERASE:
      case CODE_REF:
      case CODE_EXEC:
      case CODE_SRCH:
      case CODE_TOUCH:
      case CODE_PRE:
      case CODE_SUCRO1: // 3.3.2005
        i=1; break;
        }
    return(i);
    }

static int nextkey()
        {
        int m;

        while (1)
            {
            m=nextkey2_medit();
            if (m!=-1) return(m);
            }
        }        


//static  int rr,cc;
// static int m_double_click,m_click;
//int m_double_click,m_click; // 13.5.2003
//int m_right_click; // 4.6.2003

static int soft_keys_init_medit()
    {
    int i;
    
    muste_resize(c3,r3); // RS 5.12.2012 

    write_string(space,c3,'4',r3-1,0);
//  write_string(" STOP ",6,'7',r3-1,c3-5);

    for (i=0; i<n_soft_keys; ++i)
        write_string(soft_key[i],soft_len[i],'7',r3-1,soft_start[i]+1);

    soft_bottom_line_erase_medit();
    return(1);
    }

int mouse_medit_functions(int c_mouse,int r_mouse,int m_click,int m_double_click)
    {
    int i;
    int orgc;
    int cc; // ,rr;
    char *p;

//Rprintf("\nc_mouse: %d, r_mouse: %d",c_mouse,r_mouse);

	orgc=c; // RS ADD
    if (m_double_click)
      {
      if (r_mouse<r3-2) // 13.5.2003
          {
          special=1;
          return(-2);
          }
      c=c_mouse;
      r=r_mouse;
      if (r==r3-2)
          {
          if (c>c3-7) return(-1); // RS FIXME ??? CHA exit(0); -> return(-1); // tilapainen STOP
          }
      special=1;
      return(CODE_EXEC);
      }

    if (m_click)
      {
      c=c_mouse;
      r=r_mouse;
      special=1;
//    if (r==r3-2 && c<=c3-7) { special=1; return(CODE_NEXT); }

      if (r==r3-2)
          {
          special=1;
          for (i=0; i<n_soft_keys; ++i)
              if (c>=soft_start[i] && c<soft_start[i]+soft_len[i])
              	{
              	c=orgc;
                return(soft_code[i]);
                }
          }
      return(0);
      }

    if (search_on2) return(-1); // 22.6.2003
    if (r_mouse==r3-2)
        {
        cc=c_mouse;
 //       rr=r_mouse;
        special=1;
        for (i=0; i<n_soft_keys; ++i)
            if (cc>=soft_start[i] && cc<soft_start[i]+soft_len[i]) break;

        if (i==current_soft_key) return(-1);
        if (i==n_soft_keys)
            {
            soft_bottom_line_erase_medit();
            current_soft_key=-1;
            return(-1);
            }
        current_soft_key=i;
        soft_bottom_line_erase_medit();
        p=soft_message[kieli-1][i];
        write_string(p,strlen(p),'4',r3,1);
        }
    else if (current_soft_key>=0)
        { soft_bottom_line_erase_medit(); current_soft_key=-1; }

    return(-1);
    }

static void cursor_medit(unsigned int r,unsigned int c)
        {
        if (c>c3) c=c3;
        sur_locate(r+1,c+1);
        }
        
static int soft_bottom_line_erase_medit()
    {
    write_string(space,c3,'\237',r3,0);
    return(1);
    }

static int conditions_medit(SURVO_DATA *d,char *xx)
// char *xx; // SELECT type expression 21.6.2003
        {
        int i,k;
        char x[3*LLENGTH];
        char s[LLENGTH];
        char *p,*q;
// RS REM        char siirtop[16];

        n_select=k=0;
//      i=spfind("IND"); if (i>=0) ++k;
//      i=spfind("CASES"); if (i>=0) ++k;
//      i=spfind("SELECT");
//      if (i>=0)
//          {
//          strcpy(x,spb[i]);
            strcpy(x,xx);
            if (strchr(x,'(')!=NULL)
                {
                bool_norm(x);
/*              strcpy(s,survo_path); strcat(s,"BOOLPAR.EXE");
                sprintf(siirtop,"%p",&x);
                i=spawnl(P_WAIT,s,s,siirtop,NULL);
                if (i<0)
                    {
                    sel_virhe("SELECT (not enough memory)");
                    return(-1);
                    }
*/
                if (*x==EOS)
                    {
                    sel_virhe("SELECT"); return(-1);
                    }
                }
            p=x;
            while (*p) { if (*p=='*' || *p=='+') ++n_select; ++p; }
            ++n_select;

//          }

        cases_space=EOS; // 2.1.2003
        i=spfind("CASES_SPACE");
        if (i>=0) cases_space=*spb[i];

        cases_wild='\377'; // 8.1.2003
        i=spfind("CASES_WILD*");
        if (i>=0) { cases_wild=*spb[i]; n_cases_wild=1; }

        cases_wild2='\377'; // 9.1.2003
        i=spfind("CASES_WILD?");
        if (i>=0) { cases_wild2=*spb[i]; n_cases_wild2=1; }


        if (n_select==0 && k==0) return(1);
        n_select+=2;  /* aina tilat 0 ja 1 IND ja CASES */

        sel_var=(int *)muste_realloc(sel_var,n_select*sizeof(int));
        if (sel_var==NULL) { tilavirhe(); return(-1); }
        sel_type=muste_realloc(sel_type,n_select);
        if (sel_type==NULL) { tilavirhe(); return(-1); }
        sel_rel=muste_realloc(sel_rel,n_select);
        if (sel_rel==NULL) { tilavirhe(); return(-1); }
        sel_lower=(double *)muste_realloc(sel_lower,n_select*sizeof(double));
        if (sel_lower==NULL) { tilavirhe(); return(-1); }
        sel_upper=(double *)muste_realloc(sel_upper, n_select*sizeof(double));
        if (sel_upper==NULL) { tilavirhe(); return(-1); }
        sel_cases=(char **)muste_realloc(sel_cases,n_select*sizeof(char **));
        if (sel_cases==NULL) { tilavirhe(); return(-1); }
        sel_lastcase=(char **)muste_realloc(sel_lastcase,n_select*sizeof(char **));
        if (sel_lastcase==NULL) { tilavirhe(); return(-1); }
        sel_neg=muste_realloc(sel_neg,n_select);
        if (sel_neg==NULL) { tilavirhe(); return(-1); }

        sel_var[0]=sel_var[1]=-2; sel_neg[0]=sel_neg[1]=' ';

        i=find_cond(d,"IND",0);
        if (i==-2) { sel_virhe("IND"); return(-1); }
        i=find_cond(d,"CASES",1);
        if (i==-2) { sel_virhe("CASES"); return(-1); }

        if (n_select==2) return(1);
        p=x; sel_rel[2]='*';
        for (k=2; k<n_select; ++k)
            {
            q=p;
            while (*q && *q!='*' && *q!='+') ++q;
            if (*q) sel_rel[k+1]=*q;
            i=q-p; strncpy(s,p,i); s[i]=EOS; p=q+1;
            i=find_cond(d,s,k);
            if (i<0) { sel_virhe(s); return(-1); }
                /* i==-2 -22.4.1992 */
            }
        return(1);
        } 

static int sp_init_medit(int lin,int m)
        {
// RS REM        int i,tila;
// RS REM        char *p;
        int spn1;
        int raja1;
        char x[LLENGTH];
// RS REM        long l;

        sp_check();
// Rprintf("\nspeclist=%d specmax=%d",speclist,specmax); getck();

        sp_check();
        speclist+=200; /* varnimet() */ // RS ADD
        speclist+=9*(m+3)+4;   /* 3 sp2_init() */
        specmax+=m+3+2;


        splist=muste_malloc((unsigned int)speclist);
        if (splist==NULL) { not_enough_mem_for_spec(); return(-1); }
        spa=(char **)muste_malloc(specmax*sizeof(char *));
        if (spa==NULL) { not_enough_mem_for_spec(); return(-1); }
        spb=(char **)muste_malloc(specmax*sizeof(char *));
        if (spb==NULL) { not_enough_mem_for_spec(); return(-1); }
        spb2=(char **)muste_malloc(specmax*sizeof(char *));
        if (spb2==NULL) { not_enough_mem_for_spec(); return(-1); }
        spshad=(char **)muste_malloc(specmax*sizeof(char *));
        if (spshad==NULL) { not_enough_mem_for_spec(); return(-1); }
        arvo=(double *)muste_malloc(specmax*sizeof(double));
        if (arvo==NULL) { not_enough_mem_for_spec(); return(-1); }

        spn=0; spl=splist; global=0;
// sur_print("A"); // getck();
        spn=varnimet_medit(); if (spn<0) { spxxxx(); return(spn); }
// sur_print("B"); // getck();
        edread(x,lin); spn=spread3_medit(x,lin);
        if (spn<0) { spxxxx(); return(spn); }
        spn=spread2_medit(lin,&raja1);
        if (spn<0) { spxxxx(); return(spn); }
        if (raja1==0) { spxxxx(); return(spn); }
        spn1=spn;
        global=1;
        spn=spread2_medit(1,&raja1);
        if (spn<0) { spxxxx(); return(spn); }
        if (global==1) spn=spn1;
        spxxxx(); return(spn);
        }

static int spread2_medit(int lin,int *raja1)
        {
        char raja[12];
        int j,i;
        char x[LLENGTH];

        strcpy(raja,"*..........");
        for (j=lin-1; j>0; --j)
            {
            edread(x,j);
            i=instr_medit(x,raja);
            if (i>=0) break;
            }
        *raja1=j;
        for (j=*raja1+1; j<=ed2; ++j)
            {
            edread(x,j);
            if (global==1)
                {
                i=instr_medit(x,"*GLOBAL*");
                if (i>0) global=0;
                }
            i=instr_medit(x,raja);
            if (i>=0) break;
            if (j==r1+r-1) continue; /* aktivoidun rivin ohittaminen */
            spn=spread3_medit(x,j); if (spn<0) return(spn);
            }


/*  Rprintf("\n"); for (i=0; i<spn; ++i) Rprintf("\n%s=%s varjo=%s",
                                         spa[i],spb[i],spshad[i]); getch();
*/
        return (spn);
        }

static int spread3_medit(char *x,int j)
        {
        int i,k,pos;
        char *p;
        char xs[LLENGTH];

        pos=1;
        while (pos<ed1)
            {
            p=strchr(x+pos,'=');
            if (p==NULL) break;
            if (*(p+1)==EOS) break;
            if (*(p+1)=='=') { pos+=2; continue; }

/* Aktivoidun kohdan ohittaminen VAR-listan osalta */
            if (j==r1+r-1 && pos==1)   /* 1.3.1992 */
                {
                p=strstr(x+1," / ");
                if (p==NULL) break;
                p=strchr(p+1,'=');
                if (p==NULL) break;
                }

            if (spn>=specmax) return(-spn);
            pos=p-x; i=pos-1;
            while (i>0 && x[i]!=' ') --i;
            if (spl-splist+pos-i+1>speclist) return(-spn);
            strncpy(spl,x+i+1,pos-i-1);
            spa[spn]=spl; spl+=pos-i; *(spl-1)=EOS;
            i=pos+1;
            while (i<ed1 && x[i]!=' ') ++i;
            if (spl-splist+i-pos+1>speclist) return(-spn);
            strncpy(spl,x+pos+1,i-pos-1);
            spb[spn++]=spl; spl+=i-pos; *(spl-1)=EOS;

            if (*(spl-2)=='&') { k=jatkorivit(j+1);
                                 if (k<0) return(-spn);
                               }
            spshad[spn-1]=NULL;
            if (zs[j]!=0)
                {
                edread(xs,zs[j]);
                if (spl-splist+i-pos+1>speclist) return(-spn);
                strncpy(spl,xs+pos+1,i-pos-1);
                spshad[spn-1]=spl; spl+=i-pos; *(spl-1)=EOS;
                }

            ++pos;
            }
        return(spn);
        }

static int varnimet_medit()
        {
        extern SURVO_DATA d;
        int i,k;
        char nimi[LLENGTH];

//      for (i=0; i<d.m_act; ++i)
        for (i=0; i<d.m; ++i)
            {
//          *nimi=EOS; strncat(nimi,d.varname[d.v[i]],8); // 22.5.2003
            *nimi=EOS; strncat(nimi,d.varname[i],8); // 22.5.2003
            k=strlen(nimi); while (nimi[k-1]==' ') nimi[--k]=EOS;

            if (spn>=specmax) return(-spn);
            if (spl-splist+k+1>speclist) return(-spn);

            strncpy(spl,nimi,k);
            spa[spn]=spl; spb[spn]=NULL;
            spl+=k+1; *(spl-1)=EOS;
            ++spn;
            }
        return(spn);
        }

static int instr_medit(char s[],char c[])
        {
        if (strstr(s,c)==NULL) return(-1);
        return(1);
        }
       
        
