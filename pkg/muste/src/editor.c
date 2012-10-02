#include "muste.h"
#include <R.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/timeb.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "survo.h"
#include "survolib.h"

#define MAXTILA 10000
#define MAXARG 10
//#define CR '\15'
#define OPLEN 32 /* 10.10.91 ennen 16 */

// 30.4.2010
#define MAX_FENCE_STOP 20

extern int muste_no_selection;
extern int muste_selection;
extern int muste_expand;
int muste_headline=TRUE;

int arguc=2;
char *arguv[]={ "A","A","A" };
char muste_nullstring[]="";

int n_fence_stop;
char fence_stop_list[MAX_FENCE_STOP][2][16];
int fence_save; // 21.5.2010
int fence_warning;  // 21.4.2010
static char fence1[]="*##########";

int own_spec_line1=0;  // 20.12.2010
int own_spec_line2=0;

int first_word_on_line_search;

int mouse_keys; // 30.10.2010

int muste_lopetus;

static char google[LNAME]; // 10.4.2008
static char wiki[LNAME]; // 12.4.2008
static char dict1[LNAME],dict2; // 12.4.2008

char *z;
int ed1,ed2,edshad;
int r,r1,r2,r3,c,c1,c2,c3;
char s_edisk[LNAME], s_esysd[LNAME], s_eout[LNAME];
char s_muste_rout[LNAME];

char last_disk[3];
char ediskpath[LNAME], survo_path16[LNAME];
// char edisk[LNAME], esysd[LNAME], eopen[LNAME], eout[LNAME];
char *edisk, *esysd, *eout;
char s_muste_startpath[LNAME];
char *muste_startpath, *muste_rout;
char muste_Rpath[LNAME]; // RS ADD 27.9.2012


int etu; // RS TUT
int etuu; /* 1=lapset eivät TUT-moodissa 0=lapset TUT-moodissa */
int alkututor; // RS TUT
int wait_tut_type=0; // RS TUT // 1=cancelled by user's actions 2=activates always
long wait_hetki;
int tut_index=0;

char id_jatko[3];

char start_etufile[LNAME]; // RS ADD
char s_etufile[LNAME];
char *etufile;
int etu1,etu2,etu3; // RS TUT 
long tutpos; // RS TUT 
int r_pause,r1_pause,c_pause,c1_pause; // 21.8.2004 // RS TUT
int sucro_pause=0; // RS TUT

int r_mouse,c_mouse;
int help_window;
char videomode[32];
int search_caps; // 20.4.2002
int help_window_open;
char gplot_layout[LNAME];
int show_lines; // 13.4.2006

char *language;

int *zs,zshn;
int erun;
int erun_start=0; // 1 ensimm?isell?, 0 seuraavilla komennoilla
int edisp; /* 1=screen redisplayed (default)  2=only current line redisplayed */

int child_wait=0;
int pre_ctnue=0;
int child_call=0;
int key_sleep=0;
int display_keys=0; // 1.8.2000 1=näytä nappien koodit riv. 23
int tmp_by_session=0;
int del_tmp=1;
long check_stack=0;



char s_sapu[MAXTILA+2];
char *sapu; // RS

char s_info[LLENGTH];
char *info;
char *key_label[256];
// RS CHA char **key_label; /* ?? */
char key_lab[LENLABEL*MAXLABEL];
// RS CHA char *key_lab;    /* ?? */
char nimeton[]="        ";
char ser_number[LNAME];
char **disp_string; /* ?? */
int speclist, specmax;
char s_active_data[LNAME];
char *active_data;
int scale_check;
int accuracy, results;
int ibm;
int s_shadow_int[10];
int *shadow_int;
unsigned char s_shadow_code[256];
unsigned char *shadow_code;
char s_tut_info[LLENGTH];
char *tut_info;
char s_tut_info2[LLENGTH];  // RS toimiiko?!?
char *tut_info2;            // Vaihdettu pointteriksi
char s_crt_exit[32];
char *crt_exit;
int sdisp;
int scroll_line;
int space_break2;
int space_break;
int ntut=0; // RS TUT 
int move_r1,move_r2;
char s_etmpd[LNAME];
char *etmpd,*pp_etmpd;
int sur_seed;
int *psur_seed;
char *sspace;
int computation_speed;

char s_eopen[LNAME];
char *eopen;

char s_survo_path[LNAME];
char *survo_path;

int display_off;
int sur_alarm;

char s_system_name[256] = "Muste";
char *system_name;

char window_name[128];
char window_name2[8];

int s_cur_par[2];
int *cur_par;
int shad_off;
char s_shad_active[256];
char *shad_active;
int tut_wait_c;
long cpu_speed;
int wait_save;
char survo_type;
int loudness;
int output_level;
int mat_parser;
char os_ver[10];  /* huom.  os_ver[] */
char s_info_2[LLENGTH];
char *info_2;
char info_s[64];
int spec_check; /* 1.6.2004 */

/* RS Local globals */
//char *z; 
//unsigned int r,r1,r2,r3,c,c1,c2,c3;
//unsigned int ed1,ed2,edshad;
unsigned char shadow_code_tila[256];
char space[LLENGTH+1], stripe[LLENGTH];
// char sapu[MAXTILA+2];
char op_sana[256],help_sana[256]; // RS 10 -> 256
//static int edrun;
//int speclist,specmax;
//int scale_check;
//int accuracy, results;
char *prompt_line;
int insert_type;
int ins_lines_on;
int line_labels_off=0; /* 25.9.1994 */
int ver_disp=0;
int paint_on=0; /* 17.11.1996 */

int move_ind=0;
int mr,mc,mr1,mc1,mr2,mc2;
int move_words=0;
int move_r1,move_r2;

int ued1,ued2,uedshad;

FILE *edfield;
FILE *apu;
FILE *survoxxx;

char edit_file32[LNAME];
int save_84ed=0;
int redim_save=0;
// RS static unsigned char rivi [10*LLENGTH];
char rivin_loppu[]="\15\12";
int s84_warning=0; /* 1.10.1997 */
int exit_warning=1;
int save_warning=1;

char sbuf[LLENGTH*3];

int g;
int g_org; // RS ADD
char *spl;
int global;
int r_soft;
int v_results,v_accuracy;
char comline[LLENGTH];
char comline_org[LLENGTH];
char *word[MAXPARM];
char *word_org[MAXPARM]; // RS ADD
char *parm[MAXPARM];
char sur_session[2];
char prompt_shadow='1';
unsigned int rsh=0, zsrsh;
char sh_vara[LLENGTH];
int insert_mode=0;
int large_field=0;
char pref=' ';
int autosave=0;
int autosave32=0;
int autosavefield=0;
int special_code=0;

int numtab=0;
int vnumtab=0; /* 28.10.1994 */
char deleted_line[LLENGTH]; /* 16.1.1998 */

char s_op[MAXARG+4];
char *op;
char prompt_space[EC3+9];

char survoblo[LNAME];
char survowrd[LNAME];

int m_move_ind=0; // no mouse right button pressed!
int m_move_r1,m_mc1;
int m_move_ind2=0;

time_t aika_save;
char aika[26];

/* specifications in the edit field */
char *splist;
char **spa, **spb, **spshad;
int spn;
double *arvo; /* vain arit.c tarvitsee  */
char **spb2;   /* spb-kopiot */
char *spp;
unsigned int *spplace;

int dispm=0;
int type_without_limit=0;
int special;
int prevkey;
int edrun;
int nleft;
int cursor_step=4; /* 9.7.1994 */
int left_edge=1;
int ref_c1=1, ref_c=1, ref_r1=1, ref_r=1;
int ref1_line=1; // 26.11.2009 defined by F2 - and loaded by alt-F5 - ENTER

// RS REM static char *zz;


// int help_window;


int act_sounds_on=0; // 0=ei käytössä, 1=off, 2=on   14.10.2005

static int n_save;
       int c_vasen,c_oikea;
static int r_alku,r_loppu;
static int c_vasen1,c_oikea1;
static int r_alku1,r_loppu1;
static int poistetut_merkit;
static int marg_ero;
static char trim_command[LNAME];
static int move_from_store=0;

int goto_load_ind=0;

int child_call2=0;
int child_call0=0;

char OO[OPLEN+1];       /* ennen lokaaleja!!?? */
char op_tila[OPLEN+1];
char op2[OPLEN+1];
char *op_plot_word;

// RS REM char sur_session[2];
int keysum=0;
int soft_vis=1;
int soft_key_activated=0;
int soft_act=0;
int soft_act2; // op_arit() varten!
int mouse_refresh=0; // 10.5.2008

char soft_actline[LLENGTH];
char actline[LLENGTH];
char sucropath[LNAME];
char break_sucro[LNAME];
char survo_id[128];
char qpath[LLENGTH];  /* -9.5.93 LNAME */
char orig_setup[LNAME], current_setup[LNAME];
char wait_tut_name[32];

char s_muste_clipfile[LNAME];
char s_muste_command[LNAME];

char *muste_clipfile;
char *muste_command;

int ver;

int medit=0; // 30.4.2003
int medit_r1; // 5.6.2003


/* OP_FIND VARIABLES */
#define MAX_PITUUS 50

static char sh_haku[LLENGTH], sh_korvaus[LLENGTH];   /* 20.1.1006 */
static int sh; /* 0=no shadows 1=shadows */

int reverse_search;
char vanha_haku[MAX_PITUUS]=" ";

static int caps_on;

// RS REM static char wordcomp[32];
// RS REM static int fr,fr1,fc,fc1;
// static char rivi[LLENGTH]; // RS CHA [16]->[LLENGTH]

/* OP_FIND VARIABLES END */

static char survopoint; // 15.3.2012
int survopoint_on=0; // 15.3.2012
unsigned int survopoint_disp_n=0; // 15.3.2012 (17.3.2012)
int survopoint_disp=30; // 15.3.2012 (18.3.2012)
static int survopoint_counter[30]; // 15.3.2011
static unsigned int survopoint_timer[30]; // 15.3.2012
static int survopoint_jj[30]; // 15.3.2012
static char *point_par[102];
// RS REM static char x[LLENGTH];
static char y[LLENGTH];

static char kb_mouse_start=EOS; // 2.11.2010
static int minus_paths=0; // 30.9.2010

/* RS: local declarations */
extern int sur_resize1(int cc,int rr); // RS from soft.c
extern int varnimet(); // RS from gplot.c
extern int muste_touch(); // RS from touch.c
extern int tut_special_editor();
extern int tutch_editor();
extern int nextkey_editor();
extern int nextch_editor();
extern int nextch_editor_eventloop();
extern int Wdisp_editor();
extern void tutsave();

static void shadinit();
int lastline2();
static int key_common();
int edsave();
static int edload();
static void op_scratch();
int disp();
int sur_dump();
int restore_dump();
void muste_dump();
void muste_restore_dump();
int sys_save_restore();
int seek_char();
int seek_word();
int miau_koodit();
int op_init();
void init_param1();
void hae_edisk();
int activate();
int set_console_title();

static void muste_showpaths()
{
// RS SHOW PATHS
//edisk,esysd,eout,ediskpath,survo_path16,etufile,gplot_layout,sapu,info,
//active_data,etmpd,eopen,survo_path,edit_file32, survoblo, survowrd, sucropath,
//break_sucro, qpath, orig_setup, current_setup
Rprintf("\n------------------------------",edisk);
Rprintf("\nedisk: %s",edisk);
Rprintf("\nesysd: %s",esysd);
Rprintf("\neout: %s",eout);
Rprintf("\netmpd: %s",etmpd);
Rprintf("\neopen: %s",eopen);
Rprintf("\nediskpath: %s",ediskpath);
Rprintf("\nsurvo_path: %s",survo_path);
Rprintf("\nsurvo_path16: %s",survo_path16);
Rprintf("\netufile: %s",etufile);
Rprintf("\ngplot_layout: %s",gplot_layout);
//Rprintf("\nsapu: %s",sapu);
Rprintf("\ninfo: %s",info);
Rprintf("\nactive_data: %s",active_data);
Rprintf("\neditfile32: %s",edit_file32);
Rprintf("\nsurvoblo: %s",survoblo);
Rprintf("\nsurvowrd: %s",survowrd);
Rprintf("\nsucropath: %s",sucropath);
Rprintf("\nbreak_sucro: %s",break_sucro);
Rprintf("\nqpath: %s",qpath);
Rprintf("\norig_setup: %s",orig_setup);
Rprintf("\ncurrent_setup: %s",current_setup);
Rprintf("\n------------------------------\n",edisk);
return;
}


void label(int m,char nimi[])
        {
        strcpy(nimi,key_label[m]);
        if (nimi[0]==' ') nimi[0]=(char)m;
        }


static int init_shadow_codes()
        {
        int i;
        char x[LLENGTH], *osa[10];
        int shadow;
        int shad_nr;
        char shad_name[10];

        shad_nr=1; strcpy(shad_name,"shadows");
        i=hae_apu("shadows#",x);
        if (i) shad_nr=atoi(x);
        if (shad_nr>1) sprintf(shad_name,"shadows%d",shad_nr);

        i=hae_apu(shad_name,x);
        if (i==0) i=hae_apu("shadows",x);

        i=split(x,osa,10);
        for (i=0; i<10; ++i) shadow_int[i]=atoi(osa[i]);
        for (shadow=0; shadow<256; ++shadow)
            {
            if (shadow==32) shadow_code[shadow]=shadow_int[0];
            else if (shadow>'0' && shadow<='9')
                    shadow_code[shadow]=shadow_int[shadow-'0'];
            else if (shadow>=256-32 && shadow<256-16)
                    shadow_code[shadow]=shadow-256+32;
            else if (shadow<32) shadow_code[shadow]=shadow;
            else shadow_code[shadow]=shadow-16;
           }
        return(1);
        }
        
static int editor_labels()
        {
        int i,j,len;
        char x[LLENGTH];
        char *sana[2];
        char *p;
        FILE *lab; // RS global -> local

        init_shadow_codes();
        *info=EOS;
        p=space+strlen(space)-LENLABEL+1;
        for (i=0; i<256; ++i) key_label[i]=p;

        strcpy(x,survo_path); strcat(x,"SYS/SURVO.LAB");
        lab=muste_fopen2(x,"rt");
        if (lab==NULL)
            {
            sur_print("\nSURVO.LAB missing!"); WAIT; *info='?'; return(-1);
            }
        for (j=0; j<MAXLABEL; ++j)
            {
            fgets(x,32,lab);
            split(x,sana,2);
            len=strlen(sana[1]); sana[1][len-1]=EOS;
            sana[1][LENLABEL-1]=EOS;
            strcpy(key_lab+j*LENLABEL,sana[1]);
            }
        muste_fclose(lab);

        key_label[CODE_RETURN]=key_lab;
        key_label[CODE_EXEC]=key_lab+LENLABEL;
        key_label[CODE_INSERT]=key_lab+2*LENLABEL;
        key_label[CODE_DELETE]=key_lab+3*LENLABEL;
        key_label[CODE_PREV]=key_lab+4*LENLABEL;
        key_label[CODE_NEXT]=key_lab+5*LENLABEL;
        key_label[CODE_RIGHT]=key_lab+6*LENLABEL;
        key_label[CODE_LEFT]=key_lab+7*LENLABEL;
        key_label[CODE_UP]=key_lab+8*LENLABEL;
        key_label[CODE_DOWN]=key_lab+9*LENLABEL;
        key_label[CODE_PRE]=key_lab+10*LENLABEL;
        key_label[CODE_TOUCH]=key_lab+11*LENLABEL;
        key_label[CODE_MERGE]=key_lab+12*LENLABEL;
        key_label[CODE_REF]=key_lab+13*LENLABEL;
        key_label[CODE_EXIT]=key_lab+14*LENLABEL;
        key_label[CODE_SRCH]=key_lab+15*LENLABEL;
        key_label[CODE_CODE]=key_lab+17*LENLABEL;
        key_label[CODE_DISP]=key_lab+18*LENLABEL;
        key_label[CODE_COPY]=key_lab+19*LENLABEL;
        key_label[CODE_HELP]=key_lab+20*LENLABEL;
        key_label[CODE_ACTIV]=key_lab+21*LENLABEL;
        key_label[CODE_ERASE]=key_lab+25*LENLABEL;
        key_label[CODE_HOME]=key_lab+26*LENLABEL;
        key_label[CODE_DISK]=key_lab+28*LENLABEL;
        key_label[CODE_TAB]=key_lab+30*LENLABEL;
        key_label[CODE_TABS]=key_lab+31*LENLABEL;
        key_label[PREFIX]=key_lab+32*LENLABEL; // RS FIXME CHECK changed PREFIX
        key_label[CODE_INSERTL]=key_lab+33*LENLABEL;
        key_label[CODE_DELETEL]=key_lab+34*LENLABEL;
        key_label[CODE_MOVE]=key_lab+35*LENLABEL;
        key_label[CODE_END]=key_lab+36*LENLABEL;
        key_label[CODE_BACKSP]=key_lab+37*LENLABEL;
        key_label[CODE_WORDS]=key_lab+38*LENLABEL;
        key_label[CODE_SOFT_ON]=key_lab+39*LENLABEL;
// 6.6.2003: FILE MEDIT (medit.lab) lis‰tty ctrl-PgDn,ctrl-PgUp
        return(1);
        }

int labels()
        {
        int i,j,len;
        char x[LLENGTH];
        char *sana[2];
        char *p;
        FILE *lab; // RS global -> local


//      init_shadow_codes();   vain editorissa
//      *info=EOS;
        p=space+strlen(space)-LENLABEL+1;
        for (i=0; i<256; ++i) key_label[i]=p;

        strcpy(x,survo_path); strcat(x,"SYS/SURVO.LAB");
        lab=muste_fopen2(x,"rt");
        if (lab==NULL)
            {
            sur_print("\nSURVO.LAB missing!"); WAIT; return(-1);
            }
        for (j=0; j<MAXLABEL; ++j)
            {
            fgets(x,32,lab);
            split(x,sana,2);
            len=strlen(sana[1]); sana[1][len-1]=EOS;
            sana[1][LENLABEL-1]=EOS;
            strcpy(key_lab+j*LENLABEL,sana[1]);
            }
        muste_fclose(lab);

        key_label[CODE_RETURN]=key_lab;
        key_label[CODE_EXEC]=key_lab+LENLABEL;
        key_label[CODE_INSERT]=key_lab+2*LENLABEL;
        key_label[CODE_DELETE]=key_lab+3*LENLABEL;
        key_label[CODE_PREV]=key_lab+4*LENLABEL;
        key_label[CODE_NEXT]=key_lab+5*LENLABEL;
        key_label[CODE_RIGHT]=key_lab+6*LENLABEL;
        key_label[CODE_LEFT]=key_lab+7*LENLABEL;
        key_label[CODE_UP]=key_lab+8*LENLABEL;
        key_label[CODE_DOWN]=key_lab+9*LENLABEL;
        key_label[CODE_PRE]=key_lab+10*LENLABEL;
        key_label[CODE_TOUCH]=key_lab+11*LENLABEL;
        key_label[CODE_MERGE]=key_lab+12*LENLABEL;
        key_label[CODE_REF]=key_lab+13*LENLABEL;
        key_label[CODE_EXIT]=key_lab+14*LENLABEL;
        key_label[CODE_SRCH]=key_lab+15*LENLABEL;
        key_label[CODE_CODE]=key_lab+17*LENLABEL;
        key_label[CODE_DISP]=key_lab+18*LENLABEL;
        key_label[CODE_COPY]=key_lab+19*LENLABEL;
        key_label[CODE_HELP]=key_lab+20*LENLABEL;
        key_label[CODE_ACTIV]=key_lab+21*LENLABEL;
        key_label[CODE_ERASE]=key_lab+25*LENLABEL;
        key_label[CODE_HOME]=key_lab+26*LENLABEL;
        key_label[CODE_DISK]=key_lab+28*LENLABEL;
        key_label[CODE_TAB]=key_lab+30*LENLABEL;
        key_label[CODE_TABS]=key_lab+31*LENLABEL;
        key_label[PREFIX]=key_lab+32*LENLABEL;
        key_label[CODE_INSERTL]=key_lab+33*LENLABEL;
        key_label[CODE_DELETEL]=key_lab+34*LENLABEL;
        key_label[CODE_MOVE]=key_lab+35*LENLABEL;
        key_label[CODE_END]=key_lab+36*LENLABEL;
        key_label[CODE_BACKSP]=key_lab+37*LENLABEL;
        key_label[CODE_WORDS]=key_lab+38*LENLABEL;
        key_label[CODE_SOFT_ON]=key_lab+39*LENLABEL; // 6.6.2003

        return(1);
        }


int subst_survo_path_in_editor(char *s) // 26.2.2001
    {
    char x[LLENGTH];
    char *p;
    int i;
// RS REM    extern char survo_path[];

	p=NULL;
    while (strchr(s,'<')!=NULL) // RS ADD 27.9.2012
        {
        p=strstr(s,"<Temp>");
        if (p==NULL) p=strstr(s,"<TEMP>");
        if (p==NULL) break;
        *p=EOS;
        strcpy(x,s);
        strcat(x,etmpd);
        i=strlen(x); x[i-1]=EOS;  // RS CHA oli x[i-3]
        strcat(x,p+6);
        strcpy(s,x);
        }

    if (p==NULL) 
    while (strchr(s,'<')!=NULL)
        {
        p=strstr(s,"<Survo>");
        if (p==NULL) { p=strstr(s,"<SURVO>"); }
        if (p==NULL) { break; }
        *p=EOS;
        strcpy(x,s);
        strcat(x,survo_path);
        i=strlen(x); x[i-1]=EOS;  // RS oli x[i-3]
        strcat(x,p+7);
        strcpy(s,x);
        }
        
 if (p==NULL) // RS ADD 27.9.2012
    while (strchr(s,'<')!=NULL)
        {
        p=strstr(s,"<R>");
        if (p==NULL) break;
        *p=EOS;
        strcpy(x,s);
        strcat(x,muste_Rpath);
        i=strlen(x); x[i-1]=EOS;  // RS CHA oli x[i-3]
        strcat(x,p+3);
        strcpy(s,x);
        }       
                
    return(1);
    }

int unsubst_survo_path_in_editor(char *s) // 27.2.2001
    {
    int i;
    char x[LNAME];
// RS REM    extern char survo_path[];

    strcpy(x,etmpd); // RS ADD 27.9.2012
    i=strlen(x)-1; x[i]=EOS;  
    if (strncmp(s,x,i)==0)
    	{   
		strcpy(x,"<Temp>"); 
		strcat(x,s+i);
		strcpy(s,x);
		return(1);
		}

    strcpy(x,survo_path);
    i=strlen(x)-1; x[i]=EOS;  // RS oli strlen(x)-2
    if (strncmp(s,x,i)==0) // RS strupr poistettu
    	{   
		strcpy(x,"<Survo>");  // RS oli filesep perässä 
		strcat(x,s+i);
		strcpy(s,x);
		return(1);
		}

    strcpy(x,muste_Rpath); // RS ADD 27.9.2012
    i=strlen(x)-1; x[i]=EOS;
    if (strncmp(s,x,i)==0)
    	{   
		strcpy(x,"<R>");
		strcat(x,s+i);
		strcpy(s,x);
		return(1);
		}

		
    return(1);
    }

int add_survo_path(char *s1,char *s2)
        {
        if (strchr(s2,':')!=NULL || strchr(s2,'/')!=NULL || *survo_path==EOS) // RS FIXME path
            { strcpy(s1,s2); return(1); }
        strcpy(s1,survo_path); strcat(s1,s2);
        return(1);
        }

int del_file()
        {
// RS REM        int i;
        char x[LNAME];
// RS REM        char y[LNAME];
// RS REM        char u[LNAME];
// RS REM        extern int cd_failed;
// RS REM        char *p;

        strcpy(x,parm[2]);
        subst_survo_path_in_editor(x); // 20.10.2001

        if (strchr(x,':')==NULL) { strcpy(x,edisk); strcat(x,parm[2]); } // RS FIXME path
        muste_append_path(x,".SVO"); // RS CHA
// RS REM        if (strchr(x+strlen(x)-4,'.')==NULL) strcat(x,".SVO");
/**************************************************** 25.11.1997
        i=strlen(x)-1; while (i>0 && x[i]!='\\') --i;
        if (i==0) { remove(x); return(1); }
        *y=EOS; strncpy(y,x,i+1); p=x+i+1;
        strcpy(u,edisk);
        g=2; parm[1]=y; op_cd2();
        if (!cd_failed) remove(p);
        parm[1]=u; op_cd2();
   poistettu 4.2.1998 *******************************/

//      strcpy(y,"DEL "); strcat(y,x); /* takaisin DEL 4.2.1998 */
//      system(y);
        sur_delete(x);

        return(1);
        }

int tell_language()
        {
// RS REM        extern char *crt_exit;

        if (*crt_exit=='1') return(1);
        return(2);
        }

static int medit_error()
    {
    sur_print("\nUsage: FILE MEDIT <data>,<medit_field>:<medit_list>");
    WAIT; return(1);
    }
    
static FILE *temptut;    

static int medit_spec_save(char *s)
    {
    int i;
    char x[LLENGTH];

    i=spec_find(s,x,LLENGTH-1);
    if (i<0) fprintf(temptut,"%s\n",s);
    else fprintf(temptut,"%s=%s\n",s,x);
    return(1);
    }


static int medit_restore()
    {
    int rs;
    extern long tutpos;
    extern FILE *tutor;

    if (r_soft) rs=r_soft+1; else rs=0;
    sur_resize1(c3+8,r3+2+rs);

    set_console_title();

    disp_all();
// RS REM    if (strncmp(os_ver,"WIN9",4)==0) sur_win9x_refresh(window_name);

    sprintf(sbuf,"%sSURVOMDT",etmpd);
    edload(sbuf,1);

    r1=medit_r1;
    medit=0;

// Rprintf("\netuu=%d etu=%d tutpos=%ld|",etuu,etu,tutpos); getck();
    if (etu && !etuu)
        {
        sprintf(sbuf,"%sSURVOMD2",etmpd);
        temptut=muste_fopen2(sbuf,"rt");
        fgets(sbuf,20,temptut);
        muste_fclose(temptut);
        tutpos=atol(sbuf);
        muste_fseek(tutor,tutpos,SEEK_SET);
        }

    return(1);
    }


int op_file(char *op)
        {
        int i,j;
        char *s;
        char *p=NULL; // 30.4.2003
        char data_name[LNAME];
        char edit_name[LNAME];
        char list_name[LNAME];
        char x[LLENGTH];
        char x2[LLENGTH];
        int rr,cc;
        char *ss[2];
        int par3;

        if (strcmp(op,"CREATE")==0) 
          {
          muste_file_create(arguc,arguv);
          return(0);
          }
          

        if (g==1)
            {
            sur_print("\nSee FILE?");
            WAIT; return(0);
            }
        soft_vis=0;
        s=parm[1];
        muste_strupr(s);

        if (strcmp(s,"DEL")==0)
            { 
            del_file(); 
            return(0); }

        if (strcmp(s,"SHOW")==0)   // RS direct call
           { 
//              sur_dump(sur_session);
              muste_file_show(sur_session);
//              restore_dump(sur_session);
              return(0);
           } 

        if (strcmp(s,"EDIT")==0)   // RS direct call
           { 
              muste_file_edit(arguc,arguv);
              return(0);
           }    

           
        if (strcmp(s,"LOAD")==0)   // RS direct call
           { 
              muste_file_load(arguc,arguv);
              return(0);
           } 

        if (strcmp(s,"AGGRE")==0)   // RS direct call
           { 
              muste_file_aggre(arguc,arguv);
              return(0);
           }       
           
        if (strcmp(s,"AGGR")==0)   // RS direct call
           { 
              muste_file_aggr(arguc,arguv);
              return(0);
           }            

        if (strcmp(s,"SAVE")==0)   // RS direct call
           {
           if (muste_strcmpi(parm[2],"MAT")==0)
                {
                strcpy(op,"SMAT");
                muste_file_save_mat(arguc,arguv);
                return(0);
                }
           else
           		{
              	muste_file_save(arguc,arguv);
              	return(0);
              	}
           }            
            


        if (strcmp(s,"SELECT")==0)   // RS direct call
           { 
              muste_file_select(arguc,arguv);
              return(0);
           }    

        if (strncmp(s,"ACT",3)==0) s[3]=EOS;
        if (strcmp(s,"UPDATE")==0 || strcmp(s,"STATUS")==0 ||
            strcmp(s,"INIT")==0 || strcmp(s,"REDUCE")==0 ||
            strcmp(s,"ACT")==0 || strcmp(s,"MASK")==0 ||
            strcmp(s,"MAKE")==0 || strcmp(s,"COND")==0 ||
            strcmp(s,"MASKLOAD")==0 || strcmp(s,"CONVERT")==0 ||
            strcmp(s,"CREATE")==0
           )
              {
              
//Rprintf("\nentering FILE CREATE");            
// RS REM              strcpy(op,"CREATE");
              muste_file_create(arguc,arguv);
              return(1);
              }
        if (strcmp(s,"SORT")==0)
            { 
            strcpy(op,"FSORT");
            muste_file_sort(arguc,arguv);
            return(1); 
            }

        if (strcmp(s,"COPY")==0 || strcmp(s,"EXPAND")==0) // 29.12.2003
            {
            muste_file_copy(arguc,arguv);
            muste_expand=0;
            return(1);
            }

        if (strcmp(s,"MEDIT")==0) // 30.4.2003
            {
            par3=1;
            if (g<4)
                {
                if (g==3) par3=0;
                else { medit_error(); return(0); }
                }
        	if (par3==1)
                {
                p=parm[3]+strlen(parm[3])-1;
                while (p>parm[3] && *p!=':') --p;
                if (*p!=':') { medit_error(); return(0); }
                *p=EOS;
                sprintf(info,"%s %s %s",parm[2],parm[3],p+1);
                }
            sprintf(sbuf,"%sSURVOMDT",etmpd);
            edt_talletus(sbuf);
            strcpy(data_name,parm[2]);

            sprintf(sbuf,"%sSURVOMSP",etmpd); // 11.6.2003
            temptut=muste_fopen2(sbuf,"wt");
            medit_spec_save("OPTIONS");
            medit_spec_save("RECORD");
            medit_spec_save("CHECK");
            medit_spec_save("MEDIT_VARS");
            medit_spec_save("MEDIT_SAVE");
            medit_spec_save("INDATA");
            medit_spec_save("MEDIT_TIME");
            muste_fclose(temptut);

            if (par3==1)
                {
                edload(parm[3],1);
                strcpy(edit_name,parm[3]);
                strcpy(list_name,p+1);
                }
            else
                {
                sprintf(info,"%s NULL NULL",parm[2]);
                medit=1;
                medit_r1=r1;
                strcpy(op,s);
                muste_dump();                
            	muste_file_medit(arguc,arguv); // RS ADD Direct call  
//            	muste_restore_dump();
                return(1);
                }
// Rprintf("\ndata_name=%s list_name=%s|",data_name,list_name); getch();
            sprintf(sbuf,"MEDIT:%s",list_name);
            for (j=1; j<=r2; ++j)
                {
                edread(x,j); i=split(x+1,ss,1);
                if (i && strcmp(sbuf,ss[0])==0) break;
                }
            if (j>r2)
                {
                sprintf(x,"\n%s not found in edit field %s!",sbuf,edit_name);
                sur_print(x); WAIT; medit_restore(); return(0);
                }
            medit_r1=r1;
            edread(x,j);
// Rprintf("\n%d: %s",j,x+1); getch();
            strcpy(x2,x);
            p=strstr(x2+1,"SIZE=");
            if (p!=NULL)
                {
                r1=1; // 5.6.2003 jotta "mahtuu"
                split(p+5,ss,2);
                rr=atoi(ss[0]);
                cc=atoi(ss[1]);
                muste_resize(cc,rr+3); // RS CHA sur_resize1(cc,rr+3);
                set_console_title();
                disp_all();
// RS REM                if (strncmp(os_ver,"WIN9",4)==0) sur_win9x_refresh(window_name);
                }


            medit=1;
            strcpy(op,s);
            muste_dump();             
            muste_file_medit(arguc,arguv); // RS ADD Direct call
//            muste_restore_dump();
            return(0); // RS ADD
            }

        strcpy(op,s);
        return(1);
        }

static void file_act(char *s) // RS ADD child-kutsun tauhka
        {
// RS REM        int k=0;
// RS REM        extern char ops[];
// RS REM        extern char active_data[];
// RS REM        op=ops; 
//        strcpy(op,"CREATE");
// RS REM        childp("FI\\");


        soft_vis=0;
        strcpy(info,s);
        
/* RS REM

        if (etu>0) tut_sulje();
        if (etuu) { k=etu; etu=0; }
*/

        muste_dump();
        muste_no_selection=TRUE;
        op_file("CREATE");
        muste_no_selection=FALSE;
        muste_restore_dump();

/* RS REM 
         set_console_title();
        soft_disp(1); // 15.2.2001
        
        if (etuu) etu=k;
        if (etu>0) tut_avaa();
        if (etu==1) lue_hetki(&wait_hetki);  

        if (child_wait)
            {
            sur_wait((long)(100L*child_wait),nop,1);
            }
*/ 
 
        }

static void not_space()
        {
        sur_print("\nNot space enough for the edit field!");
        WAIT;
        }

static int ed_malloc(unsigned int ed1, unsigned int ed2, unsigned int edshad)
        { 
        if (etu) tut_sulje();

/*        
        if (zs!=NULL) muste_free2((char *)zs);
        if (z!=NULL) muste_free2(z);
        z=muste_malloc(sizeof(char)*ed1*(ed2+edshad));
        if (z==NULL) { not_space(); return(-1); }
        zs=(int *)muste_malloc((ed2+1)*sizeof(int)); // RS oli unsigned int *
        if (zs==NULL) { not_space(); return(-1); }
*/
		z=muste_realloc(z,sizeof(char)*ed1*(ed2+edshad));
		if (z==NULL) { not_space(); return(-1); }
		zs=(int *)muste_realloc(zs,(ed2+1)*sizeof(int));
		if (zs==NULL) { not_space(); return(-1); }

        if (ed1>253 || ed1*(ed2+edshad)>65500) large_field=1;
        else large_field=0;
// sprintf(sbuf,"\nz=%lu|",(unsigned long)z); sur_print(sbuf);
        if (etu) tut_avaa();
        return(1);
        }



static int testaa_lisays(int k)
        {
        char *z_lisays;

        z_lisays=muste_malloc(k);
        if (z_lisays==NULL) return(-1);
        muste_free(z_lisays); return(1);
        }


int empty(char x[],unsigned int lev)
        {
        if (strncmp(x,space,lev)==0) return(1);
        return(0);
        }

static int filerr()
        {
        PR_EBLD;
        sur_print("\nEdit field cannot be loaded/saved!");
        WAIT; PR_ENRM;
        return(1);
        }

int netd(char *p)
	{
// muste_fixme("FIXME: netd() not implemented\n");
// RS FIXME NYI (For Muste netd is always true?)
    if (p[0]=='\\' || p[0]=='/' || p[0]=='~') return TRUE;
	return FALSE;
	}

int filename(char *edfile,char *field)
        {
        int i;
        char x[LNAME];
        char *p;

        i=0; p=x;
        strcpy(p,field);

        subst_survo_path_in_editor(p);

        if (netd(p))
            {
            strcpy(edfile,p);
            return(1);
            }
        *edfile=EOS;

        if (strchr(p,':')==NULL && *p!='~' && *p!='/' && *p!='\\')  // RS CHA unix path FIXME
            {
            if (p[0]=='.' && p[1]!='.') { // RS Check relative path
              if (*p=='.') // No relative path, load from survo_path if name starts with .
                  {
                  ++p; // ohita piste 8.11.91
                  strcat(edfile,survo_path);
// RS REM                 i=strlen(edfile)-1;
// RS REM                 if (edfile[i]=='/' || edfile[i]=='\\' ) edfile[i]=EOS;  // RS ADD unixpath FIXME
                  }
              }
              else strcat(edfile,edisk);
            }
        strcat(edfile,p);
        return(1);
        }


static int edit_file_not_found(char *edfile)
        {
        PR_EINV;
        sprintf(sbuf,"\nEdit file %s not found!",edfile); sur_print(sbuf);
        WAIT; return(0);
        }

static int edload32_err(char *s,char *edfile)
        {
        sprintf(sbuf,"\n%s in edit file %s !",s,edfile);
        sur_print(sbuf); WAIT;
        return(1);
        }

static int edload32(char *edfile)
        {
        int i,j;
        char x[LLENGTH+10], *sana[3];
        char *p;
        int rivi_luettu;
        int ei_onnistunut;
        int ved1,ved2,vedshad;
        int lisays;

        ved1=ed1; ved2=ed2; vedshad=edshad;
        rewind(edfield);
        fgets(x,LLENGTH+10-1,edfield); p=strchr(x,':');
        if (p==NULL) edit_file_not_found(edfile);

        i=split(p+1,sana,3);
        ed1=atoi(sana[0]); ed2=atoi(sana[1]); edshad=atoi(sana[2]);
        muste_fclose(edfield);    /* suljetaan malloc-varausten tiivistämiseksi */

        lisays=ed1*(ed2+edshad)+(ed2+1)*sizeof(int)
               -ved1*(ved2+vedshad)-(ved2+1)*sizeof(int);
        if (lisays>0)
            {
            i=testaa_lisays(lisays);
            if (i<0)
                {
                sprintf(sbuf,"\nNot enough memory for edit field %s !",edfile);
                sur_print(sbuf);
                WAIT; return(-1);
                }
            }
        ei_onnistunut=0;

        i=ed_malloc(ed1,ed2,edshad);        
        if (i<0)
            {
            sprintf(sbuf,"\nNot enough space for edit field %s !",edfile);
            sur_print(sbuf);
            WAIT; return(-1);
            ed1=101; ed2=100; edshad=30; ed_malloc(ed1,ed2,edshad);
            ei_onnistunut=1;
            }
        edfield=muste_fopen2(edfile,"rt");
        if (edfield==NULL) { edit_file_not_found(edfile); return(-1); }

        fgets(x,LLENGTH-1,edfield); /* otsikko uudelleen */
        c2=ed1-1; r2=ed2;
        strcpy(eopen,edfile);
        for (i=0; i<ed1*(ed2+edshad); ++i) z[i]=' ';
        for (i=0; i<ed1*ed2; i+=ed1) z[i]='*';
        shadinit();
        if (ei_onnistunut) { muste_fclose(edfield); return(-1); }
        rivi_luettu=0; j=0;
        
        while (1)
            {
            fgets(x,LLENGTH+10-1,edfield);
/* Rprintf("x=%s\n",x); getch(); */
            if (feof(edfield)) break;
            p=strchr(x,'|');
            if (p==NULL) continue;
//          if (p==NULL) { edload32_err("Missing `|'",edfile);
//                         muste_fclose(edfield); return(-1);
//                       }
            *p=EOS; ++p;
            i=strlen(p); 
            if (p[i-1]=='\n') {
               p[i-1]=EOS;
               if (p[i-2]=='\r') p[i-2]=EOS;  // RS unix-fix
            }
            if (rivi_luettu && *x=='S')
                {
                i=creatshad(j);
                if (i<0) break;    /* ei mahdu */
                edwrite(p,zs[j],0);
                rivi_luettu=0;
                continue;
                }
            j=atoi(x);
            if (j>ed2)
                { edload32_err("Too many lines",edfile);
                  muste_fclose(edfield); return(-1);
                }
            edwrite(p,j,0);
            rivi_luettu=1;
            }
//      strcpy(edit_file32,edfile);        
        muste_fclose(edfield);
        return(1);
        }
        

// Muuta START.EDT -> SSTART.EDT tai ESTART.EDT kielen mukaan
int check_start_field_language(char *edfile) // 1.2.2006
    {
    char x[LNAME];
// RS REM    char ch;
// RS    extern char *language;
// RS    extern char survo_path[];

    sprintf(x,"%sSTART.EDT",survo_path);
    if(muste_strcmpi(edfile,x)!=0) return(1);
/* RS REM No need to use different languages    
    ch='S'; if (*language=='2') ch='E';
    sprintf(edfile,"%s%cSTART.EDT",survo_path,ch);
*/    
    return(1);
    }
   

static int edload(char *field,int shad)
        {
        unsigned int i;
        char rivi[ELE];
        char *sana[5]; int g;
        char x[LLENGTH];
        char edfile[LNAME];
        unsigned int j;
        short *pint; /* oli unsigned int * */
        int k;

        filename(edfile,field); file_name_ext(edfile,".EDT");
        
        i=strlen(edfile)-1;
		if (edfile[i]=='.') edfile[i]=EOS;  // RS ADD Drop last .
		
// RS REM pois       if (sur_file_time_check(edfile)==-2) return(-1); // 6.4.2001
        check_start_field_language(edfile); // 1.2.2006

        edfield=muste_fopen2(edfile,"rb");
        if (edfield==NULL) { edit_file_not_found(edfile); return(-1); }
        time(&aika_save);
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(edfield);
        rivi[ELE-1]=EOS;
        
        if (strncmp(rivi,"SURVO 98",8)==0) return(edload32(edfile));

        g=split(rivi,sana,5);
        if (strcmp(sana[0],"SURVO84ED")!=0)
            {
            sur_print("\nNot a Survo edit file!"); WAIT;
            muste_fclose(edfield); return (-1);
            }
        ed1=atoi(sana[1]);
        ed2=atoi(sana[2]);
        if (g>4 && *sana[4]=='S') edshad=atoi(sana[4]+1);

        muste_fclose(edfield);    /* suljetaan malloc-varausten tiivistämiseksi */
        k=ed_malloc(ed1,ed2,edshad); if (k<0) return(-1);
        edfield=muste_fopen2(edfile,"rb");
        if (edfield==NULL) { edit_file_not_found(edfile); return(-1); }

        c2=ed1-1; r2=ed2;
        strcpy(eopen,edfile);

        muste_fseek(edfield,(long)ed1,0);
        for (i=0; i<ed1*ed2; ++i) z[i]=(char)getc(edfield);

        if (ed1<=c3) { CLS;  soft_disp(1); }
        if (feof(edfield)) { filerr(); muste_fclose(edfield); return(-1); }
        if (shad==0) { muste_fclose(edfield); return(1); }
        shadinit();
        for (i=0; i<ed1; ++i) x[i]=(char)getc(edfield);
        if (strncmp(x,"Sha",3)!=0) { muste_fclose(edfield); return(1); }
        j=1;
        while (j>0)
            {
            for (i=0; i<ed1; ++i) x[i]=(char)getc(edfield);
            x[ed1]=EOS;
            if (strncmp(x,"END",3)==0) { j=0; continue; }
            pint=(short *)x;
            j=*pint;
            k=creatshad(j);
            if (k<0) break;    /* ei mahdu */

            strcat(x+2,"  ");
            edwrite(x+2,zs[j],0);
            }
        if (ferror(edfield)) { filerr(); muste_fclose(edfield); return(-1); }
        muste_fclose(edfield); return(1);
        }

static int xsave(char x[],FILE *edfield)
        {
        int i;
        for (i=0; i<ed1; ++i) putc((int)x[i],edfield);
        return(1);
        }

static int edsave32(char *edfile,int shad)
        {
        int i,j,zsj;
        char x[LLENGTH];
        int form;
        int tyhja;
        int d1,d2,d3;
        int message=0;       

        if (s84_warning && !autosavefield && etu<2 && !redim_save)
                 //     && muste_strcmpi(edfile,edit_file32)!=0)
            {
            message=1;
            PR_EUDL;
            sprintf(x,"\n%.79s",stripe); sur_print(x);
            sur_print("\n This edit field exceeds the limits of SURVO 84C! ");
            if (ed1>253)
                {
                sprintf(x,"\n Its width (%d) is more than 252 characters. ",ed1-1);
                sur_print(x);
                }
            if (ed1*(ed2+edshad)>65500)
                {
                sprintf(x,"\n Its size (%d) is over 65500 bytes. ",ed1*(ed2+edshad));
                sur_print(x);
                }
            sur_print("\n The field is saved as an extended SURVO 98 edit file ");
            sur_print("\n and it cannot be used in SURVO 84C at all. ");
            sur_print("\n There may be also limitations for its use in SURVO 98. ");
            }
        if (redim_save) { d1=ued1; d2=ued2; d3=uedshad; }
        else { d1=ed1; d2=ed2; d3=edshad; }

        fprintf(edfield,"SURVO 98 edit field:    %d       %d       %d (32 bit version)%s",
                         d1,d2,d3,rivin_loppu);
        form=3;
        if (ed2>=1000) form=4;
        if (ed2>=10000) form=5;
        if (ed2>=100000) form=6;

        for (j=1; j<=ed2; ++j)
            {
            edread(x,j); i=ed1-1;
            while (i>0 && x[i]==' ') --i;
            x[i+1]=EOS;
            if (i==0 && *x=='*') tyhja=1; else tyhja=0;
            zsj=zs[j];
            if (!tyhja || zsj)
                {
                fprintf(edfield,"%.*d|%s%s",form,j,x,rivin_loppu);
                }
            if (zsj)
                {
                edread(x,zsj); i=ed1-1;
                while (i>0 && x[i]==' ') --i;
                x[i+1]=EOS;
                fprintf(edfield,"%-*s|%s%s",form,"S",x,rivin_loppu);
                }
            }
        if (!autosavefield) strcpy(eopen,edfile);
        if (ferror(edfield)) { filerr(); muste_fclose(edfield); return(-1); }
        muste_fclose(edfield);
        if (message)
            {
            sprintf(x,"\n%.79s",stripe); sur_print(x);
            WAIT;
            }
//      if (!autosave32) strcpy(edit_file32,edfile);
        return(1);
        }


int edsave(char *field,int shad,int check)
        {
        unsigned int i,k;
        char header[LLENGTH];
        char number[10];
        char x[LLENGTH];
        unsigned int j;
        short *pint;
        char ch;
        char edfile[LNAME];

    filename(edfile,field); file_name_ext(edfile,".EDT");
    if (check)
        {
        edfield=muste_fopen2(edfile,"rb");
        if (edfield!=NULL)
                {
                if (muste_strcmpi(edfile,eopen)!=0 && etu!=2 && save_warning)
                    {
                    PR_EBLD;
                    sprintf(sbuf,"\nEdit file %s already exists! Overwrite it (Y/N)?",
                                                                       edfile);
                                sur_print(sbuf);
                    ch=(char)nextch_editor(""); sprintf(sbuf,"%c",ch); sur_print(sbuf);
                    if (ch!='Y' && ch!='y') { muste_fclose(edfield); return(0); }
                    }
                muste_fclose(edfield);
                }
        }

        edfield=muste_fopen2(edfile,"wb");
        if (edfield==NULL)
            {
            sprintf(sbuf,"\nCannot save %s !",edfile);
            sur_print(sbuf); WAIT;
            return(0);
            }

        if (!save_84ed) // 13.4.2002
        if (autosave32 || redim_save || large_field || autosavefield)
            { edsave32(edfile,shad); return(1); }

        save_84ed=0; // seuraavien piilotalletusten vuoksi (varmistus)!
        strcpy(header,"SURVO84ED ");
        strcat(header,muste_itoa(ed1,number,10)); strcat(header," ");
        strcat(header,muste_itoa(ed2,number,10)); strcat(header," ");
        strcat(header,"                    "); header[20]=EOS;
        strcat(header,muste_itoa(ed1,number,10)); strcat(header," S");
        strcat(header,muste_itoa(edshad,number,10)); strcat(header," ");

        for (i=0; i<ed1; ++i) putc(' ',edfield);
        rewind(edfield);
        for (i=0; i<strlen(header); ++i) putc((char)header[i],edfield);

        muste_fseek(edfield,(long)ed1,0);

        k=0;
        for (j=0; j<ed2; ++j)
            {
            for (i=0; i<ed1; ++i, ++k) putc((char)z[k],edfield);
            if (ferror(edfield)) { filerr(); muste_fclose(edfield); return(-1); }
            }
        if (shad==0) { muste_fclose(edfield); return(1); }

        shad=0;
        for (j=1; j<=ed2; ++j)
            {
            if (zs[j]!=0)
                {
                if (shad==0)
                    {
                    strcpy(x,"Shadows"); strncat(x,space,ed1-7);
                    ++shad; xsave(x,edfield);
                    }
                pint=(short *)x;
                *pint=j;
                edread(x+2,zs[j]);
                xsave(x,edfield);
                }
            }
        strcpy(x,"END"); strncat(x,space,ed1-3);
        xsave(x,edfield);
        if (ferror(edfield)) { filerr(); muste_fclose(edfield); return(-1); }
        if (check) strcpy(eopen,edfile);  /* if 17.2.89 */
        muste_fclose(edfield); return(1);
        }





static int numshad()   /* 23.1.93 */
        {
        int j,n;

        n=0; for (j=1; j<=ed2; ++j) if (zs[j]) ++n;
        return(n);
        }

int op_redim(
int komento /* 1=REDIM command 0=internal call(sh.c) */
)
        {
        int i,j,k;
        char x[LLENGTH];

        if (komento)
            {
            if (g<3) {
                   sur_print("\nCorrect form: REDIM <#_of_lines>,<#_of_columns>");
                   WAIT; return(-1);
                     }
            ued1=atoi(parm[2])+1; ued2=atoi(parm[1]);
            if (ued1>LLENGTH-3)
                {
                sprintf(sbuf,"\nMax.# of columns = %d",LLENGTH-4);
                sur_print(sbuf); WAIT; return(-1);
                }
            uedshad=edshad; if (g>3) uedshad=atoi(parm[3]);
            i=numshad();
            if (uedshad<i)
                {
                sprintf(sbuf,"\n%d shadow lines already in use!",i);
                sur_print(sbuf);
                sur_print("\nUse SHADOW ERASE,L1,L2 for erasing shadow lines.");
                WAIT; return(-1);
                }
            }

        j=ued1*(ued2+uedshad)+(ued2+1)*sizeof(int)
               -ed1*(ed2+edshad)-(ed2+1)*sizeof(int);
        if (j>0)
            {
            i=testaa_lisays(j);
            if (i<0)
                {
                sur_print("\nNot enough memory!");
                WAIT; return(-1);
                }
            }

        if (ued1<31)
                { PR_EBLD;
                  sur_print("\nWidth at least 30!");
                  WAIT; return(-1);
                }
        if (ued2<r3)
                { PR_EBLD;
                  sprintf(sbuf,"\nAt least %d lines!",r3);
                  sur_print(sbuf); WAIT; return(-1);
                }
        if (ued1<ed1)
                {
                for (j=1; j<=ed2; ++j)
                    {
                    edread(x,j);
                    if (!empty(x+ued1,ed1-ued1))
                        {
                        k=strlen(x)-1;
                        while (x[k]==' ') x[k--]=EOS;
//                      if (x[0]=='*') { Rprintf("\nOK"; getck(); }


                        i=-1; while (i<k)
                            {
                            ++i;
                            if (i==0) { if (x[0]!='*') break; }
                            if (i>0 && i<11) { if (x[i]!='.') break; }
                            if (i>=11 && x[i]!='.' && x[i]!=' ') break;
                            }
// Rprintf("\nx[0]=%c x=%.50s",x[0],x);
// Rprintf("i=%d k=%d",i,k); getck();

                        if (i==k) x[ued1]=EOS;
                        else
                          {
                          PR_EBLD;
                          sprintf(sbuf,"\nLine %u too long!",j); sur_print(sbuf);
                          WAIT; return(-1);
                          }
                        }
                    }
                }
        if (ued2<ed2)
                {
                for (j=ued2+1; j<=ed2; ++j)
                    {
                    edread(x,j);
                    if (!empty(x+1,ed1-1))
                        { PR_EBLD;
                          sprintf(sbuf,"\nLine %u not empty!",j);
                          sur_print(sbuf); WAIT; return(-1);
                        }
                    }
                }

        strcpy(x,etmpd); strcat(x,SURVOEDT);
        redim_save=1;

        edsave(x,1,0);
        if (ued2<r1+r3-1)
            {
            r=r1=1;
            }
        i=edload(x,1);
        redim_save=0;
// RS REM if (komento && g>4) { Rprintf("z=%u\n",(int)z); getch(); }
        return(1);
        }


void shadow_init()
        {
        unsigned int i,j;
        j=0; while (j<ed2) { ++j; zs[j]=0; }
        i=ed1*ed2; zshn=0;
        while ( zshn<edshad && zshn<ed2 )
            { z[i]='\0'; i+=ed1; ++zshn; }
        }
        
int shadow_create(unsigned int j)
        {
        unsigned int i,k;
        char x[LLENGTH];
        i=ed1*ed2; k=0; while ( k<zshn && z[i]!='\0' ) { ++k; i+=ed1; }
        if (k==zshn)
            {
            BEEP;
            sur_print("\nNot space anymore for special display lines!");
            sprintf(sbuf,"\nMax=%d (Use REDIM, please!)",zshn);
                sur_print(sbuf);
            WAIT; return(-1);
            }
        zs[j]=k+ed2+1;
        strncpy(x,space,ed1);
        edwrite(x,zs[j],0);
        return(1);
        }

int empty_line(char *x,int len)
        {
        int i;
        for (i=0; i<len; ++i)
            {
            if (x[i]==EOS) return(1);
            if (x[i]!=' ') return(0);
            }
        return(1);
        }

void shadow_test(unsigned int j)
        {
// RS REM        int i;
        char x[LLENGTH];
        edread(x,zs[j]);
        if (!empty_line(x,ed1)) return;
        z[(zs[j]-1)*ed1]='\0';
        zs[j]=0;
        }

void testshad(unsigned int j)
        {
// RS REM        int i;
        char x[LLENGTH];

        if (zs[j]==0) return;
        edread(x,zs[j]);
        if (!empty(x,ed1)) return;
        z[(zs[j]-1)*ed1]='\0';
        zs[j]=0;
        }


int creatshad(unsigned int j)
        {
        unsigned int i,k;
        char x[LLENGTH];

        if (j>ed2) return(-1);
        i=ed1*ed2; k=0; while ( k<zshn && z[i]!='\0' ) { ++k; i+=ed1; }
        if (k==zshn)
            {
            ued1=ed1; ued2=ed2; uedshad=zshn+10;
            op_redim(0);
            }

        zs[j]=k+ed2+1;
        strncpy(x,space,ed1);
        edwrite(x,zs[j],0);

        return(1);
        }

static void shadinit()
        {
        unsigned int i,j;
// RS REM        char *p;

        j=0; while (j<ed2) { ++j; zs[j]=0; }
        i=ed1*ed2; zshn=0;
        while ( zshn<edshad && zshn<ed2 )
            { z[i]='\0'; i+=ed1; ++zshn; }
        }

/* SHADOW BLOCK <char>        31.7.1998 */
static int shadow_block()
        {
        char ch;
        int i,j;
        char x[LLENGTH];

        if (g<2) { op_incomplete(); return(-1); }
        ch=*parm[2];
        if (!move_ind) return(1);
        for (j=move_r1; j<=move_r2; ++j)
            {
            if (!zs[j]) creatshad(j);
            edread(x,zs[j]);
            for (i=mc1; i<=mc2; ++i) x[i]=ch;
            edwrite(x,zs[j],0);
            }
        return(1);
        }

static int shadow_erase()
        {
        int j,j1,j2;

        if (g<4) { op_incomplete(); return(-1); }
        j1=edline2(parm[2],1,1); if (j1==0) return(-1);
        j2=edline2(parm[3],j1,1); if (j2==0) return(-1);
        for (j=j1; j<=j2; ++j)
            {
            if (zs[j]==0) continue;
            edwrite(space,zs[j],0);
            shadow_test(j);
            }
        return(1);
        }
        
/* SHADOW REPLACE <char1>,<char2>,L1,L2  20.9.1998 */
static int shadow_replace()
        {
        int i,j,j1,j2;
        char ch1,ch2;
        char x[LLENGTH];

        if (g<6) { op_incomplete(); return(-1); }

        if (muste_strnicmp(parm[2],"char(",5)==0) ch1=atoi(parm[2]+5);
        else ch1=*parm[2];
        if (muste_strnicmp(parm[3],"char(",5)==0) ch2=atoi(parm[3]+5);
        else ch2=*parm[3];
        j1=edline2(parm[4],1,1); if (j1==0) return(-1);
        j2=edline2(parm[5],j1,1); if (j2==0) return(-1);
        for (j=j1; j<=j2; ++j)
            {
            if (zs[j]==0) continue;
            edread(x,zs[j]);
            for (i=0; i<strlen(x); ++i) if (x[i]==ch1) x[i]=ch2;
            edwrite(x,zs[j],0);
            shadow_test(j);
            }
        return(1);
        }

// SHADOW SETC A,B,col,ACTCODE.BIN,col1,col2
// 0      1    2 3 4   5           6    7

static FILE *bin;

static int shadow_setc()  // set -> setc
   {
   int i;
   unsigned char act[256];
   int j,j1,j2;
   int col,col1,col2;
   char nimi[LNAME];
   char x[LLENGTH];
   unsigned char ch;

   if (g<8) { op_incomplete(); return(-1); }

   j1=edline2(parm[2],1,1); if (!j1) return(1);
   j2=edline2(parm[3],j1,1); if (!j2) return(1);
//  col=atoi(parm[4])+1;   - 13.12.2011
   col=atoi(parm[4]);
   col1=atoi(parm[6]);
   col2=atoi(parm[7]);

   strcpy(nimi,parm[5]);
   if (strchr(nimi,':')==NULL)
       {
       sprintf(nimi,"%sSYS/",survo_path); // RS \\ -> /
       strcat(nimi,parm[5]);
       }
   if (strchr(nimi+strlen(nimi)-4,'.')==NULL)
       strcat(nimi,".BIN");
   bin=muste_fopen2(nimi,"rb");
   if (bin==NULL)
       {
       sprintf(sbuf,"\nCannot open file %s!",nimi);
       sur_print(sbuf); WAIT; return(1);
       }
   for (i=0; i<256; ++i) act[i]=(unsigned char)getc(bin);
   muste_fclose(bin);

   for (j=j1; j<=j2; ++j)
       {
       edread(x,j);
       ch=x[col];
       if (zs[j]==0 && ch!=' ')
           creatshad(j);
       if (ch!=' ') ch=act[(int)ch];
       for (i=0; i<col2-col1+1; ++i) sbuf[i]=ch; sbuf[i]=EOS;
       edwrite(sbuf,zs[j],col1);
       testshad(j);
       }
   return(1);
   }

// SHADOW SET  L1,L2,L,[<hardness>]
// 0      1    2  3  4
static int shadow_set()
   {
   int i,j,j1,j2,len;
   char xs[LLENGTH];
   char ys[LLENGTH];
   int hard;

   if (g<5) { op_incomplete(); return(-1); }
   j1=edline2(parm[2],1,1); if (!j1) return(1);
   j2=edline2(parm[3],j1,1); if (!j2) return(1);
   j=edline2(parm[4],1,1); if (!j) return(1);
   edread(xs,j);

   hard=0;
   if (g>5 && atoi(parm[5])==1) hard=1;

   if (!hard)
       {
       len=strlen(xs)-1; while (xs[len]==' ') --len; ++len;
       for (j=j1; j<=j2; ++j)
           {
           if (zs[j]==0)
               {
               creatshad(j);
               edwrite(xs+1,zs[j],1);
               testshad(j);
               }
           else
               {
               edread(ys,zs[j]);
               for (i=0; i<len; ++i)
                   if (xs[i]!=' ') ys[i]=xs[i];
               edwrite(ys+1,zs[j],1);
               testshad(j);
               }
           }
       }
   else
       for (j=j1; j<=j2; ++j)
           {
           if (zs[j]==0)
               creatshad(j);
           edwrite(xs+1,zs[j],1);
           testshad(j);
           }

   return(1);
   }

static int op_shadow()
       {
       if (muste_strcmpi(parm[1],"ERASE")==0) { shadow_erase(); return(1); }
       if (muste_strcmpi(parm[1],"BLOCK")==0) { shadow_block(); return(1); }
       if (muste_strcmpi(parm[1],"REPLACE")==0) { shadow_replace(); return(1); }
       if (muste_strcmpi(parm[1],"SETC")==0) { shadow_setc(); return(1); }
// 14.12.2011
       if (muste_strcmpi(parm[1],"SET")==0) { shadow_set(); return(1); }

       if (g<3) { op_incomplete(); return(-1); }
       shadow_code[atoi(parm[1])]=atoi(parm[2]);
       return(1);
       }



#define FIRST_POS 18
// RS REM #define RND (double)rand()/32768.0
static FILE *codes;

static int display_colors()
        {
        char x[LLENGTH];
        char *p;
        int color,color1,color2;
        int line;
        int i,j,pos;
        unsigned char vara;

        if (muste_strcmpi(parm[1],"#ALL")==0 || muste_strcmpi(parm[1],"ALL")==0)
            {
            line=r+2;
            for (j=0; j<8; ++j)
                {
                pos=FIRST_POS;
                for (i=0; i<16; ++i)
                    {
                    color=16*j+i; sprintf(x,"%3d",color);
                    vara=shadow_code[color]; shadow_code[color]=color;
                    write_string(x,strlen(x),(unsigned char)color,line,pos);
                    pos+=3;
                    shadow_code[color]=vara;
                    }
                ++line;
                }
            return(2);
            }

        strcpy(x,parm[1]);
        p=strchr(x,'-');
        if (p!=NULL) *p=EOS;
        color1=color2=atoi(x+1);
        if (p!=NULL) color2=atoi(p+1);
        if (color2<color1) color2=color1;
        if (color1<0) color1=0;
        if (color2>color1+r3-r-1) color2=color1+r3-r-1;
        if (color2>255) color2=255;

        line=r+2;
        for (color=color1; color<=color2; ++color)
            {
            sprintf(x," %3d ABCDEFGHIJKLMNOPQRSTUVWXYZ 0123456789",color);
            vara=shadow_code[color]; shadow_code[color]=color;
            write_string(x,strlen(x),(unsigned char)color,line,FIRST_POS);
            ++line; shadow_code[color]=vara;
            }
        return(2);
        }

        
static int op_color()
        {
        int i;
        time_t ltime;
        unsigned int *pi;
        int n=128;
        char nimi[LLENGTH];

		n=128;
        if (g>1)
            {
            if (g>2)
                {
                shadow_code[atoi(parm[1])]=atoi(parm[2]);
                return(2);
                }
            if (*parm[1]=='#' || muste_strcmpi(parm[1],"ALL")==0)
                { display_colors(); return(2); }
            n=atoi(parm[1]);
            if (n==0)
                {
                strcpy(nimi,parm[1]);
                if (strchr(nimi,':')==NULL) // RS FIXME path
                { strcpy(nimi,edisk); strcat(nimi,parm[1]); }
                codes=muste_fopen2(nimi,"rb");
                if (codes==NULL) return(2);
                if (g>2)
                    {
                    i=atoi(parm[2]); if (i<1) i=1;
                    muste_fseek(codes,(long)(256*(i-1)),0); 
                    }
                for (i=0; i<256; ++i) shadow_code[i]=(unsigned char)getc(codes);
                muste_fclose(codes);
                return(2);
                }
            }
        time(&ltime);
        pi=(unsigned int *)&ltime;
        srand(*pi);
        for (i=0; i<256; ++i) shadow_code[i]=(unsigned char)((int)(n*RND));
        return(2);
        }        

static int move_disp(unsigned int j)
        {
        int cc1,cc2;
        char disp_mode;

        switch (move_ind)
            {
          case 3:
            if (j<move_r1 || j>move_r2) return(1);
            cc1=mc1-c1+1; if (!move_words && cc1>(int)c3) return(1);
         if (cc1<0) cc1=0; // RS ADD
         if (move_words && (j>move_r1 || cc1<1)) cc1=1;
            cc2=mc2-c1+1; if (cc2<1) return(1);
            if (cc2>(int)c3) cc2=c3;
         if (move_words && j<move_r2) cc2=c3;
            disp_mode='7'; if (move_words) disp_mode='4';
            write_string(z+(j-1)*ed1+c1+cc1-1,cc2-cc1+1,disp_mode,j-r1+2,cc1+8);
            }
        return(1);
        }

static void check_alarm(char *aika)    /* 10.8.1992 */
        {
        int k;
        char x[LLENGTH];

        k=hae_apu("alarm",x);
        if (k && !sur_alarm)
            {
            if (strncmp(x,aika+11,8)<=0)
                {
                display_off=0; BEEP; PR_EBLK;
                sur_print("\n ALARM!!!     Press # "); PR_ENRM;
                while (k!='#') k=getck();
                ++sur_alarm; disp();
                }
            }
        }

static char s_aika[27];
char *s_time(time_t *paika)
        {
        strcpy(s_aika,ctime(paika));
        if (s_aika[8]==' ') s_aika[8]='0';
        return(s_aika);
        }

/*
void pvmaika(char aika[])
        {
        time_t ltime;

        time(&ltime);
        strcpy(aika,s_time(&ltime)); aika[24]=EOS;
        }
*/        

// RS REM static int prev_r,prev_c;
int headline_editor()
        {
        char x[LLENGTH];
        int k,i,lev; // RS REM ,len;
        char dispm2;
        char hshadow; /* 25.11.1992 */

//		if (display_off && etu==0) restore_display(1); // RS NEW CHECK FIXME (if no sucro, restore display)

		muste_updatewd(); // RS ADD 19.9.2012 
		muste_statusbar(TRUE,dispm); // RS ADD 25.9.2012 		
		if (!muste_headline) // RS ADD 23.9.2012
			{	
			write_string(space,c3+8,0,1,1); // Emptly line with shadow 0
			return(1);
			}

		lev=c1+c-1; if (lev>c2) lev=c2; // RS ADD		
        pvmaika(aika);
        dispm2='0'+ntut; hshadow='8'; if (ntut==0) { dispm2=' '; hshadow='4'; }
        sprintf(x,"%c%c%4u",pref,dispm2,lev);  // RS CHA c1+c-1 -> lev
        write_string(x,6,hshadow,1,1);
        sprintf(x,"%3u",c1); write_string(x,3,')',1,7);
    /*  CURSOR_ON; */ sprintf(x," %s ",system_name);

        write_string(x,strlen(system_name)+2,'7',1,10);
        k=23+c3-72; // RS CHA 20 -> 23
          
        strcpy(sbuf,edisk); unsubst_survo_path_in_editor(sbuf);
        sprintf(x,"  %s %*.*s%7d%5d ",aika,k,k,sbuf,r2,c2);
        write_string(x,strlen(x),hshadow,1,17); // RS 20 -> 17

        if (c2>996) // 31.7.2008
            {
            i=strlen(x)+11;  // RS CHA 14->11;
            sprintf(x,"%5d",c2);
            write_string(x,5,'8',1,i);
            }

        sprintf(x,"%d",dispm);

        if (large_field) dispm2='P'; else dispm2='4';

        write_string(" ",1,dispm2,1,c3+8-1);

        dispm2='0'+dispm; if (dispm==0) dispm2=' ';

        write_string(x,1,dispm2,1,c3+8);
        PR_ENRM;
        check_alarm(aika);  /* 10.8.1992 */

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

        return(1);
        }

void init_survopoint_counters() // 15.3.2012
    {
    int i;
    for (i=0; i<30; ++i)
        {
        survopoint_counter[i]=survopoint_timer[i]=0;
        survopoint_jj[i]=0;
        }
    }

void displine(unsigned int j,unsigned int lev)
        {
        char x[LLENGTH];
        char *px,*pxs;
        int i,k;
        char dispx;

        int jj=j; // 15.3.2012
        int rivi;
        char *p;
        char abc[2];
        int n_words;
        int n;
        double rval,sum;
        int interval;


/*      CURSOR_OFF;             */
        if (rsh!=j) sprintf(x,"%6u ",j); else sprintf(x,"Shadow:");
        x[7]=z[(j-1)*ed1]; x[8]=EOS;
        if (x[7]=='?') { write_string(space,c3+8,' ',j-r1+2,1); return; }
        if (!line_labels_off) { write_string(x,8,2,j-r1+2,1); } // RS CHA 23.9.2012 shadow '2' -> 2
                         else { write_string(space,8,0,j-r1+2,1); } // RS CHA 23.9.2012 shadow ' ' -> 0
        if (c3>c2) { write_string(space,c3-c2,'7',j-r1+2,c2+9); }
        if (lev>0)
            {
            px=z+(j-1)*ed1+c1;
            
    // 15.3.2012
//            if (survopoint_on && *(px-1)==survopoint)
            if (survopoint_on && *(px-c1)==survopoint) // 29.3.2012            
                {
                p=strchr(px,survopoint);
                if (p!=NULL && (int)(p-px)<ed1)                
                    {
                    abc[0]=*(p+1); abc[1]=EOS;
                    i=edline2(abc,1,0);
//                    if (i>0)
                    if (i>0 && abc[0]>='a' && abc[0]<='z') // 29.3.2012                    
                      {
                      edread(y,i);
                      n_words=split(y+1,point_par,102);
// 5    3 0.5 0.3 0.2
// time n probabilities
					  if (n_words<3) return; // RS ADD

                      interval=atoi(point_par[0]);
                      rivi=(int)(abc[0]-'a');
//              if (survopoint_disp_n-survopoint_timer[rivi]>=interval)
// 29.3.2012 n_words>2 && lisätty:
              if (n_words>2 && survopoint_disp_n-survopoint_timer[rivi]>=interval)              
                        {
                        survopoint_timer[rivi]=survopoint_disp_n;

                        n=atoi(point_par[1]);
                        if (*point_par[2]=='S') // Systematic
                          {
                          if (survopoint_counter[rivi]>n-1)
                                   survopoint_counter[rivi]=0;
                          ++survopoint_counter[rivi];
                          jj=i+survopoint_counter[rivi];
                          px=z+(jj-1)*ed1+c1;
                          survopoint_jj[rivi]=jj;
                          }
                        else if (*point_par[2]=='U') // Uniform prob.
                          {
                          rval=(double)rand()/(double)RAND_MAX;
                          jj=i+1+(int)((double)n*rval);
                          px=z+(jj-1)*ed1+c1;
                          survopoint_jj[rivi]=jj;
                          }
                        else // given probabilities
                          {
                          rval=(double)rand()/(double)RAND_MAX;
                          sum=0.0;
                          k=0;
                          while (k<n) // 19.3.2012
                            {
//                  if (k<0 || k>10) { Rprintf("\nk=%d",k); }
							if (n_words<k+2+1) return; // RS ADD
                            sum+=atof(point_par[k+2]);
                            if (rval<sum) break;
                            ++k;
                            }
                          jj=i+1+k;
                          px=z+(jj-1)*ed1+c1;
                          survopoint_jj[rivi]=jj;
                          }
                        }
              else
                        {
//                        if (survopoint_jj[rivi]>0)
   /* 29.3.2012 */      if (n_words>2 && survopoint_jj[rivi]>0)                        
                            {
                            jj=survopoint_jj[rivi];                            
                            px=z+(jj-1)*ed1+c1;
                            }
                        }
                      }
                    }
                }

            
            if (*px=='.')
                {
                i=1;
                while (px[i]=='.' && i<lev) ++i;
                if (i==lev) { write_string(stripe,lev,' ',j-r1+2,9); return; }
                }
            if (*px=='#') // 19.4.2010
                {
                i=1;
// RS REM?                while (px[i]=='#' && i<lev) ++i;
// RS REM?                if (i==lev) { write_string(stripe2,lev,' ',j-r1+2,9); return(1); }
                }                                
// RS CHA            if ( zs[j]==0 )
            if ( zs[jj]==0 ) // 15.3.2012
                {
                write_string(px,lev,' ',j-r1+2,9);
                if (move_ind>1) move_disp(j);
                return;
                }
// RS CHA           pxs=z+(zs[j]-1)*ed1+c1;
            pxs=z+(zs[jj]-1)*ed1+c1;  // 15.3.2012            
            dispx=*(pxs-c1);
            if (dispx!=' ')
                {
                write_string(px,lev,dispx,j-r1+2,9);
                if (move_ind>1) move_disp(j);
                return;
                }
            i=0;
            while (i<lev)
                {
                dispx=pxs[i];
                k=i;
                while (k<lev && pxs[k]==dispx) ++k;

               if (shad_off)   /* 18.5.1995 */
                    dispx=shad_active[(unsigned int)dispx];

                write_string(px+i,k-i,dispx,j-r1+2,9+i);
                i=k;
                }

            if (move_ind>1) move_disp(j);
            }
/*      CURSOR_ON;      */
        }

void displine2(unsigned int j)
        {
        unsigned int lev;
        lev=c3; if (c2-c1+1<c3) lev=c2-c1+1;
        LOCATE(j-r1+2,1);
        ERASE;
        displine(j,lev);
        }

static void disp_mode(int dispm)
        {
        switch (dispm)
            {
          case ' ': PR_ENRM; break;
          case '1': PR_EBLD; break;
          case '2': PR_ESUB; break;
          case '3': PR_ESUP; break;
          case '4': PR_EUDL; break;
          case '5': PR_EBLK; break;
          case '6': PR_EOVR; break;
          case '7': PR_EINV; break;
         default: PR_EIN2; break;
            }
        }

void disp_prompt_line(int sh) // RS char sh)
        {
        write_string(space,c3+8,0,r3+2,1); // RS CHA 23.9.2012 shadow ' ' -> 0
        if (prompt_line!=NULL)
            {
            if (sh>='0' && sh<='9') sh-=48; // RS ADD 23.9.2012
            write_string(prompt_line,strlen(prompt_line),sh,r3+2,1);
            }
        }

int dispoint()
        {
        extern int muste_help_running;
        unsigned int i,lev;
        int k;
        char *px;
        if (muste_help_running) return(1);
        if (survopoint_on) ++survopoint_disp_n; // 15.3.2012
        headline_editor();
             
        lev=c3; if (c2-c1+1<c3) lev=c2-c1+1;
        k=r3; if (r2<r3) k=r2;
        
        for (i=0; i<k; ++i) 
        	{
            px=z+(r1+i-1)*ed1+c1;
            if (*(px-1)==survopoint) displine(r1+i,lev);
        	}
        	
        cursor(r,c);

        return(1);
        }

int disp()
        {
        extern int muste_help_running;
        unsigned int i,lev;
        int k;
        if (muste_help_running) return(1);
        if (survopoint_on) ++survopoint_disp_n; // 15.3.2012
		CURSOR_OFF;
        headline_editor();
        
// RS REM?        ohi_on=' '; // 10.10.01        
        lev=c3; if (c2-c1+1<c3) lev=c2-c1+1;
        k=r3; if (r2<r3) k=r2;
        for (i=0; i<k; ++i) displine(r1+i,lev);
        disp_prompt_line(prompt_shadow);
        if (display_off) soft_disp(0);
        
// RS REM?         aikatarkistus(); // 10.10.01
// Rprintf("\nohi_on=%d|",(int)ohi_on); sur_sleep(1000);
// RS REM?        if (aika_loppuu!=NULL && ohi_on!=EOS) aika_loppunut();       
        
       cursor(r,c);
       if (insert_mode) CURSOR_INS; else CURSOR_ON;
        return(1);
        }

void dispch(int m)
        {
        int i;

        if (dispm>0) {
                       disp_mode(dispm+'0');
                       if (m==(int)' ') m=(int)z[(r1+r-2)*ed1+c1+c-1];
                     }
        write_string(&m,1,(unsigned char)sdisp,r+1,c+8);

        z[(r1+r-2)*ed1+c1+c-1]=(char)m;
        if (dispm>0)
            {
            if (zs[r1+r-1]==0)
                {
                i=creatshad(r1+r-1); if (i==0) return;
                }
            z[(zs[r1+r-1]-1)*ed1+c1+c-1]=(char)(dispm+'0');
            }
        else /* dispm=0 */
            {
            if (zs[r1+r-1]==0) return;
            z[(zs[r1+r-1]-1)*ed1+c1+c-1]=' ';
            testshad(r1+r-1);
            }

        }

int disp_shadow()
        {
        int i;
        char x[LLENGTH];
        unsigned int j=r1+r-1;

        if (rsh==0)
            {
            if (j==r2 || r==r3) return(1);
            edread(sh_vara,j+1); rsh=j+1; zsrsh=zs[j+1]; zs[j+1]=0;
            dispm=0;
            if (zs[j]==0)
                {
                i=creatshad(j);
                if (i<0)
                    {
               /*   rsh=0;   creatshad hoitaa  */
                    zs[j+1]=zsrsh;
                    return(1);
                    }
                }
            edread(x,zs[j]);
            edwrite(x,j+1,0); displine2(j+1);
            return(1);
            }

        edread(x,rsh);
        if (zs[rsh-1]==0) creatshad(rsh-1);   /* 13.3.86 */
        edwrite(x,zs[rsh-1],0);
        edwrite(sh_vara,rsh,0); zs[rsh]=zsrsh;
        testshad(rsh-1);
        j=rsh; rsh=0;
        if (j-1>=r1 && j-1<r1+r3) displine2(j-1);
        if (j>=r1 && j<r1+r3) displine2(j);
        return(1);
        }


int disp_all()
        {
        int i,j;
        disp();
        j=soft_vis;
        if (etu==1) i=0; else i=1;
        soft_disp(i);

        soft_vis=j;

        return(1);
        }

int restore_display(int i) /* 19.4.1993 */
        {
 //     if (i) end_graphics();      i 4.9.1993   16.8.2000
        display_off=0; disp_all();
        return(1);
        }

void pyyhi_alarivi()
        {
        LOCATE(r3+2,1); ERASE; /* sprintf(sbuf,"%.*s",c3+6,space); sur_print(sbuf); */
        }

void move_clear()
        {
        move_ind=0;
        prompt_line=NULL;
        *info=EOS;
        disp();
        }


void prompt_editor(char *kysymys,char *vastaus,int pituus)
        {
        int i;
        char tila[LLENGTH];
        int m, pos;

        for (i=0; i<pituus; ++i) tila[i]=' ';
        for (i=0; i<strlen(vastaus); ++i) tila[i]=vastaus[i];
        sprintf(sbuf,"%s%.*s*",kysymys,pituus,tila); sur_print(sbuf);
        for (i=0; i<pituus+1; ++i) PR_LEFT;

        pos=1;
        while (1)
            {
            SAVE_CURSOR;  // RS pois?
            m=nextch_editor("");
            RESTORE_CURSOR;  // RS pois?
                switch (m)
                    {
                  case CODE_LEFT:
                    if (pos==1) break;
                    PR_LEFT; --pos; break;
                  case CODE_RIGHT:
                    if (pos==pituus) break;
                    PR_RIGHT; ++pos; break;
                  case CODE_RETURN:
                    i=pituus;
                    while(tila[i-1]==' ') --i;
                    tila[i]=EOS;
                    strcpy(vastaus,tila);
                    return;
                  case CODE_HOME:
                    for (;pos>1;--pos) PR_LEFT;
                    break;
                  case CODE_ERASE:
                    for (i=pos-1; i<pituus; ++i)
                        {
                        tila[i]=' ';
                        sur_print(" ");
                        }
                    for (i=pos-1; i<pituus; ++i) PR_LEFT;
                    break;
                  case CODE_INSERT:
                    if (tila[pituus-1]!=' ') { BEEP; break; }
                    for (i=pituus-1; i>=pos; --i) tila[i]=tila[i-1];
                    tila[pos-1]=' ';
                    for (i=pos; i<=pituus; ++i) { sprintf(sbuf,"%c",tila[i-1]); sur_print(sbuf); }
                    for (i=pos; i<=pituus; ++i) PR_LEFT;
                    break;
                  case CODE_DELETE:
                    for (i=pos; i<pituus; ++i) tila[i-1]=tila[i];
                    tila[pituus-1]=' ';
                    for (i=pos; i<=pituus; ++i) { sprintf(sbuf,"%c",tila[i-1]); sur_print(sbuf); }
                    for (i=pos; i<=pituus; ++i) PR_LEFT;
                    break;

                  default:
                    sprintf(sbuf,"%c",m); sur_print(sbuf); tila[pos-1]=(char)m;
                    if (pos<pituus) ++pos; else PR_LEFT;
                    break;
                    }
            }
        }



int insertl()
        {
        unsigned int j;
        char x[LLENGTH]; // RS REM , x1[LLENGTH];

        j=r1+r;
        if (j-1==r2) return(1);  /* 30.3.91 */
        if (rsh) disp_shadow();  /* 8.5.90 */
        edread(x,r2);
        if (!empty(x+1,c2))
            {
            if (!etu) // 9.2.2001
                { BEEP; return(-1); }
            }
        memmove(z+j*ed1,z+(j-1)*ed1,(r2-j)*ed1);
        strcpy(x,"*"); strncat(x,space,c2);
        edwrite(x,j,0);
        j=ed2-1;
        for (; j>r1+r-1; --j) zs[j+1]=zs[j];
        zs[r1+r]=0;
        if (move_ind)
            {
            if (move_r1>=r1+r-1)
                {
                if (move_r1<r2) ++move_r1;
                if (move_r2<r2) ++move_r2;
                }
            }

                    if (r<r3 && !display_off)
                        {
                        SCROLL_DOWN(r+1,r3,1);
                        displine2(r1+r);
                        for (j=r1+r+1; j<r1+r3; ++j)
                            displine(j,0);
                        }

                    if (r<r3) ++r;

/*      if (r<r3) { disp(); ++r; }  */
        return(1);
        }

void deletel()
        {
        unsigned int j;
        char x[LLENGTH]; // RS REM, x1[LLENGTH];
        unsigned int l;
// RS        extern char deleted_line[];  /* 16.1.1999 */
// RS        extern unsigned int rsh;

        if (rsh) { disp_shadow(); return; }  /* 8.5.90 */
        edread(deleted_line,r1+r-1); /* 27.3.89, 8.5.90 */

        memmove(z+(r1+r-2)*ed1,z+(r1+r-1)*ed1,(r2-r1-r+2)*ed1);
        strcpy(x,"*"); strncat(x,space,c2);
        edwrite(x,r2,0);

        j=r1+r-1;                       /* 6.11.88 */
        if (zs[j]!=0)
            {
            z[(zs[j]-1)*ed1]='\0'; zs[j]=0;
            }

        l=ed2-1;
        for (j=r1+r-1; j<=l; ++j) zs[j]=zs[j+1];
        zs[l+1]=0;
        if (move_ind)
            {
            if (move_r1>r1+r-1)
                {
                if (move_r1>1) --move_r1;
                if (move_r2>1) --move_r2;
                }
            }

                    if (r<r3 && !display_off)
                        {
                        SCROLL_UP(r,r3,1);
                        for (j=r1+r-1; j<r1+r3; ++j)
                            displine(j,0);
                        }
                    displine2(r1+r3-1);

/*      disp();  */
        }


void shadt(unsigned int j,unsigned int i)
        {
        testshad(j); testshad(i);
        }

void line_merge()
        {
        unsigned int j,len,shad;
        char x[LLENGTH], x1[LLENGTH];
        char sx[LLENGTH], sx1[LLENGTH];
        int ins_line=0;

        j=r1+r-1;
        if (j==r2) return;
        shad=0; if ( zs[j]!=0 || zs[j+1]!=0 ) shad=1;
        edread(x,j);
        edread(x1,j+1);
        if (shad)
            {
            if (zs[j]==0) creatshad(j);
            if (zs[j+1]==0) creatshad(j+1);
            edread(sx,zs[j]); edread(sx1,zs[j+1]);
            }
        if (empty(x+c1+c-1,c2-c1-c+2))
            {
            len=c2+1;
            while (x1[len-1]==' ') x1[--len]=EOS;
            if (len>c2-c1-c+2) { if (shad) shadt(j,j+1); return; }
            x[c1+c-1]=EOS; strcat(x,x1+1);
            edwrite(x,j,0);
            edwrite(space,j+1,1);
            if (shad)
                {
                sx1[len]=EOS;
                sx[c1+c-1]=EOS; strcat(sx,sx1+1);
                edwrite(sx,zs[j],0);
                edwrite(space,zs[j+1],1);
                shadt(j,j+1);
                }
            displine2(j); if (r<r3) displine2(j+1);
            return;
            }
        if (!empty(x1+1,c2-1))
            {
            insertl();     // 2.4.2004
            *x1='*';
            if (r<r3) --r;
            creatshad(j+1);
            ins_line=1;
            }
        x1[1]=EOS; strncat(x1,x+c1+c-1,c2-c1-c+2);
        edwrite(space,j,c1+c-1);
        edwrite(x1,j+1,0);
        if (shad)
            {
            sx1[1]=EOS; strncat(sx1,sx+c1+c-1,c2-c1-c+2);
            edwrite(space,zs[j],c1+c-1);
            edwrite(sx1,zs[j+1],0);
            shadt(j,j+1);
            }
        if (ins_line) testshad(j+1);
        displine2(j); if (r<r3) displine2(j+1);
        }

int insert()
        {
        int i;
        unsigned int j=r1+r-1;
        char x[LLENGTH], x1[LLENGTH];
        int c_insert;

        edread(x,j);
        if (ins_lines_on)
            {
            c_insert=c3; if (c2<c3) c_insert=c2;
            if (!empty(x+c_insert,c2-c_insert) || x[c_insert]!=' ')
                {
                if (r<r3) { i=insertl(); --r; }
                else i=insertl();
                if (i<0) return(-1);
                line_merge();
                edread(x,j);
                }
             }
        else if (x[c2]!=' ') { BEEP; return(-1); }
        strcpy(x1,x); x[c1+c-1]=EOS;
        strcat(x," "); strcat(x,x1+c1+c-1); x[ed1]=EOS;
        edwrite(x,j,0);
        if (zs[j]!=0)
           {
           edread(x,zs[j]);
           strcpy(x1,x); x[c1+c-1]=EOS;
           strcat(x," "); strcat(x,x1+c1+c-1); x[ed1]=EOS;
           edwrite(x,zs[j],0);
           testshad(j);
           }
        displine2(j);
        return(1);
        }

void delete()
        {
// RS        extern int m_move_ind,m_move_ind2;
                    unsigned int j=r1+r-1;
                    char x[LLENGTH]; // RS REM, x1[LLENGTH];
                    extern int muste_selection;  // RS ADD                  

        m_move_ind2=0;
        if (move_ind || m_move_ind || muste_selection) 
            { move_clear(); m_move_ind=0; muste_selection=FALSE; soft_disp(1); return; }
        edread(x,j);
        x[c1+c-1]=EOS; strcat(x,x+c1+c); strcat(x," ");
        edwrite(x,j,0);
        if (zs[j]!=0)
            {
        edread(x,zs[j]);
        x[c1+c-1]=EOS; strcat(x,x+c1+c); strcat(x," ");
        edwrite(x,zs[j],0);
        testshad(j);
            }
        displine2(j);

        }

int step_down(int k)
        {
        int i;

        for (i=0; i<k; ++i)
            {
            if (r+i==r2) return(1); // 22.12.2000
            if (r<r3) { ++r; continue; }
            if (r1<r2-r3+1)
                {
                ++r1;
                if (display_off) continue;
                SCROLL_UP(1,r3,1);
                displine2(r1+r3-1);
                }
            }
        return(1);
        }


void move_right2(int k)
        {
        int i;

        for (i=0; i<k; ++i)
            {
            if (c<c3 && c<c2) { ++c; continue; }
            if (c2-c1+1>c3) { ++c1; disp(); }
            }
        }

void move_up2(int k)
        {
        int i;

        for (i=0; i<k; ++i)
            {
            if (r>1) { --r; continue; }
            if (r1>1)
                {
                --r1;
                if (display_off) continue;
                SCROLL_DOWN(1,r3,1);
                displine2(r1);
                }
            }
        }



int kontr_()    /* 26.3.1992 */
        {
        if (*(z+ed1*(r1+r-2))=='_') return(1);
        return(0);
        }

static int remove_current_session(); // RS declaration
int lopetuskysely()
        {
        int i; // RS REM ,i2,i3;
/*
        extern int etu,ntut;
        extern FILE *tutor;
        extern int exit_warning;
*/
		muste_lopetus=FALSE; // RS 
        if (!exit_warning) return(1);
        PR_EBLK; cursor(r3+1,1);
        sur_print("Exit from Muste (Y/N)?");
        i=nextch_editor(); if (i=='Y' || i=='y')
                     {
                     NORMAL_SCREEN; CURSOR_ON; // RS pois?
                                           
                        remove_current_session();
                        if (tmp_by_session && del_tmp) // 26.3.2004
                            {
                            sur_sleep(200L);
                            sprintf(sbuf,"%s*.*",etmpd);
                            sur_delete_files(sbuf);
                            sur_remove_dir(etmpd);
                            }
/* RS NYI
                        gplot_poisto(); // 18.1.20001 jos close_graphs=1
                        
                        if (help_window_open)
                            {
                            sur_set_message("-",1);
                            }
*/
                     edrun=0; 
                     return(1);
                     }
        disp();
        return(0);
        }


static int line_copy_do(int i, int j)
	{
        unsigned int len,shad;
        char x[LLENGTH], x1[LLENGTH];
        char sx[2*LLENGTH], sx1[2*LLENGTH];
        unsigned int len2;

        if (i==j && zs[i]==0)
            {
            edread(x,j); i=ed1; while (i>1 && x[i-1]==' ') x[--i]=EOS;
            if (i==1) return(1);
            strcpy(sx,x); while (strlen(sx)<ed1) strcat(sx,x+1);
            edwrite(sx,j,0); return(1);
            }

        shad=0; if ( zs[i]!=0 || zs[j]!=0 ) shad=1;
        edread(x,j);
        edread(x1,i);
        len2=0;
        if (shad)
            {
            if (zs[j]==0) creatshad(j);
            if (zs[i]==0) creatshad(i);
            edread(sx,zs[j]); edread(sx1,zs[i]);
            len2=c2+1;
            while (len2>1 && sx1[len2-1]==' ') --len2;
            }
        len=c2+1;
        while (len>1 && x1[len-1]==' ') --len;
        if (len2>len) len=len2; x1[len]=sx1[len]=EOS;

        if (len>c2-c1-c+3) x1[c2-c1-c+3]=EOS;
        x[c1+c-1]=EOS; strcat(x,x1+1);
        edwrite(x,j,0);
        if (shad)
            {
            sx1[len]=EOS;
            sx[c1+c-1]=EOS; strcat(sx,sx1+1);
            if (zs[j]>0) edwrite(sx,zs[j],0);
            testshad(i); testshad(j);
            }
   return(1);
   }

static int op_copy();
int line_copy()
        {
        unsigned int i,j,k; // RS REM len,shad;
        int ekar,vikar; // RS REM ,alkuj;
        char x[LLENGTH]; // RS REM , x1[LLENGTH];
// RS REM        char sx[2*LLENGTH], sx1[2*LLENGTH];
// RS REM        unsigned int len2;
// RS REM        extern int ref_r1,ref_r;
		char *xl;

        if (move_ind) pyyhi_alarivi();

        soft_disp(0);
        cursor(r,c);
        PR_EBLK; sprintf(sbuf,"%c",(char)PREFIX); sur_print(sbuf);
        cursor(r3+1,1); PR_EBLD;
        *x=EOS; prompt_editor("Line to be copied ? ",x,13); // RS CHA (s) & 6->13

        j=r1+r-1;
        
        xl=strchr(x,','); // RS ADD
        if (xl!=NULL)
			{
			*xl=EOS; xl++;
			ekar=edline2(x,1,1); if (ekar==0) return(1);
			vikar=edline2(xl,ekar,1); if (vikar==0) return(1);
			
			for (i=ekar; i<=vikar; i++, j++)
				{
				line_copy_do(i,j);
				}
			return(1);
			}
/*
        	{
        	*xl=EOS; xl++;
			sprintf(sx,"COPY");
			sprintf(sx1,"%d",j);

			parm[0]=sx;
			parm[1]=x;
			parm[2]=xl;
			parm[3]=sx1;
			g=4;        	
        	
        	op_copy();
        	return(1);
        	}
*/        	
        
        if (*x==EOS) i=j;
        else if (*x=='*') // 19.8.2007
            {
            k=ref_r1+ref_r-1;
            if (k<1) i=1;
            else i=k;
            }
        else if (*x=='-') i=ref1_line; // 26.11.2009    
        else
            {
            i=edline2(x,1,1); if (i==0) return(1);
            }


		line_copy_do(i,j);


/*
        if (i==j && zs[i]==0)
            {
            edread(x,j); i=ed1; while (i>1 && x[i-1]==' ') x[--i]=EOS;
            if (i==1) return(1);
            strcpy(sx,x); while (strlen(sx)<ed1) strcat(sx,x+1);
            edwrite(sx,j,0); return(1);
            }

        shad=0; if ( zs[i]!=0 || zs[j]!=0 ) shad=1;
        edread(x,j);
        edread(x1,i);
        len2=0;
        if (shad)
            {
            if (zs[j]==0) creatshad(j);
            if (zs[i]==0) creatshad(i);
            edread(sx,zs[j]); edread(sx1,zs[i]);
            len2=c2+1;
            while (len2>1 && sx1[len2-1]==' ') --len2;
            }
        len=c2+1;
        while (len>1 && x1[len-1]==' ') --len;
        if (len2>len) len=len2; x1[len]=sx1[len]=EOS;

        if (len>c2-c1-c+3) x1[c2-c1-c+3]=EOS;
        x[c1+c-1]=EOS; strcat(x,x1+1);
        edwrite(x,j,0);
        if (shad)
            {
            sx1[len]=EOS;
            sx[c1+c-1]=EOS; strcat(sx,sx1+1);
            if (zs[j]>0) edwrite(sx,zs[j],0);
            testshad(i); testshad(j);
            }
*/            
        return(1);
        }

void col_display()
        {
        while (c>c3) { c1+=c3; c-=c3; }
        if (c2>c3 && c1>c2-c3+1) { c+=c1-c2+c3-1; c1=c2-c3+1; }
        disp();
        }

void seek_line_end()
        {
        char x[LLENGTH];
        int i;

        edread(x,r1+r-1);
        i=c2;
        while (i>0 && x[i]==' ') --i;
        if (i<c2) ++i;
        c1=1; c=i;
        col_display();
        }

int dtest(int vr1,int r1)
        {
        if (vr1==r1) return(2);
        return(1);
        }

int op_incomplete()
        {
        sprintf(sbuf,"\nIncomplete %s operation!",op); sur_print(sbuf);
        WAIT;
        return(1);
        }

int op_goto2(int g,char *parm[])
        {
        int i;
        unsigned int j1,j2=0;
        int col;
        int vr1;
        int vc1;

        if (g<2) { op_incomplete(); return(-1); }
        displine2(r1+r-1);  /* 6.6.1989 */
        vr1=r1;
        if (muste_strcmpi(parm[1],"BORDER")==0) // 19.10.2001
            {
            i=r1+r; j1=0;
            while (i<=r2)
                {
                edread(sbuf,i);
                if (strstr(sbuf,"*..........")!=NULL) { j1=i; break; }
                ++i;
                }
            if (j1==0) return(-1);
            }
        else
            {
            j1=edline2(parm[1],1,1); if (j1==0) return(-1);
            }
            
        if (g>2) { j2=edline2(parm[2],j1,1); if (j2<j1) return(-1); }
        if (j1>r2-r3+1) { r1=r2-r3+1; r=j1-r1+1; j1=r2-r3+1; }
        else            { r1=j1; r=1; }
        if (g==2) return(dtest(vr1,r1));
        if (j2-j1>r3-1) j1=j2-r3+1;
        r1=j1; r=j2-r1+1;
        
        if (g==3) return(dtest(vr1,r1));
        if (g==4)
            {
            col=atoi(parm[3]); if (col<0 || col>c3) return(1);
            c=col; return(dtest(vr1,r1));
            }

        vc1=c1;     /* 21.2.1991 */
        c1=atoi(parm[3]); col=atoi(parm[4])-c1+1;
        if (c1<1 || c1>c2-c3+1 || col<1 || col>c3) { c1=vc1; return(1); }
        c=col;
        i=dtest(vr1,r1); if (i==1 || c1!=vc1) return(1);
        return(2);
        }

int op_goto()
        {
        return(op_goto2(g,parm));
        }

int op_cd()
        {
        return(muste_setwd());
        }

int op_set()
        {
        unsigned int i,j,j1,j2,j3;
        char maskline[LLENGTH];
        char c;

        if (g<4)
            {
            sur_print("\nUsage: SET L1,L2,L");
            WAIT; return(-1);
            }
        j1=edline2(parm[1],1,1); if (j1==0) return(-1);
        j2=edline2(parm[2],j1,1); if (j2==0) return(-1);
        j3=edline2(parm[3],1,1); if (j3==0) return(-1);
        edread(maskline,j3);
        for (i=1; i<ed1; ++i)
            {
            c=maskline[i];
            if (c==' ') continue;
            for (j=(j1-1)*ed1+i; j<=(j2-1)*ed1+i; j+=ed1)
                z[j]=c;
            }
        return(1);
        }

int op_save()
        {
// RS        extern int save_84ed; // 13.4.2002

        if (g<2) { op_incomplete(); return(-1); }
        if (rsh) disp_shadow();
        edread(sbuf,1);
        if (strstr(sbuf,"(84ED)")!=NULL) save_84ed=1; else save_84ed=0;
        edsave(parm[1],1,1);
        return(1);
        }

int op_load()
        {
        int i;

        if (g<2) { op_incomplete(); return(-1); }
        i=edload(parm[1],1); if (i<=0) return(-1);
        r=r1=c=c1=1; if (g==2) return(1);
        while (1)
            {
            i=edline2(parm[2],1,1); if (i<=0) return(-1);
            r1=i;
            if (r1>r2-r3+1) { r1=r2-r3+1; r=i-r1+1; }
            if (g==3) return(1);
            i=edline2(parm[3],1,r1); if (i<=0) return(-1);
            r=i-r1+1; if (r<=0 || r>r3) { r=1; parm[2]=parm[3]; continue; }
                                        /* 13.4.1996 */
            if (g==4) return(1);
            c=atoi(parm[4]); if (c>c3) c=1;  /* 25.11.90 */
            return(1);
            }
        }

int yys(); // RS declaration
int get_console_name(char *x)
    {
    int i;
// RS REM    unsigned char *p;
// RS    extern int ver;
// RS    extern char verstr[];
// RS    extern char *language;
    char dfilename[LNAME], licensed[80], version[80];
    char *buffer;
    
    i=sprintf(x,"%s",system_name);
    if (*sur_session!='A' && *sur_session!=EOS)
        i=sprintf(x,"%s: %s",sur_session,system_name);
    
    strcpy(dfilename,survo_path);
    strcat(dfilename,"DESCRIPTION");

    apu=muste_fopen2(dfilename,"rt");
    buffer=sbuf;
    yys(buffer);
    yys(buffer);
    yys(buffer);
    strcpy(licensed,buffer+7);    
    yys(buffer);
    strcpy(version,buffer+9);
    
    muste_fclose(apu);
    
    sprintf(x+i," - %s - ver. %s",licensed,version);

/* RS REM    
        if (strncmp(os_ver,"WIN9",4)==0)
        {
        p=x; while (*p) { *p=tcode[(int)(*p)]; ++p; }
        }
*/

    strcpy(window_name,x); // 24.7.2000
    sprintf(window_name2,"%sSURVO",sur_session); // 24.1.2001
    return(1);
    }


int set_console_title()
    {
    char x[LNAME];

    get_console_name(x);
    sur_set_console_title(x);

    return(1);
    }


int op_resize()
    {
// RS REM    int i;
    int rr,cc;
    int rs;
    extern int r_soft;
// RS REM   extern char window_name[];

    if (g<3) { rr=23; cc=72; }
    else
        {
        rr=atoi(parm[1]);
        cc=atoi(parm[2]);
        }
    if (rr<1) rr=23;
    if (cc<72) cc=72;
    if (rr>r2) rr=r2; // RS ADD 7.6.2012

    if (r_soft) rs=r_soft+1; else rs=0;
    if (rr+rs+2<25 && cc<72) cc=72; // RS cc>72 sallittu;  pienillä r3, c3 oltava 72
    muste_resize(cc+8,rr+2+rs); // RS CHA   i=sur_resize1(cc+8,rr+2+rs);
    r3=rr; if (r>r3) r=r3;
    c3=cc; if (c>c3) c=c3;

    if (r2-r1<r3) { r+=r3-(r2-r1)-1; r1=r2-r3+1; }  // 22.1.2007

    set_console_title();  // RS pois?

    disp_all();

// RS REM    if (strncmp(os_ver,"WIN9",4)==0) sur_win9x_refresh(window_name);
    return(1);
    }



int goto_control_char(int k) /* 5.4.1999 */
        {
        int j,j0;
        char ch;
        char rivi1[10];
        char rivi2[10];

        j0=r1+r-1;
        if (k) { ch=z[(r1+r-2)*ed1]; ++j0; } else ch=*tut_info;
        for (j=j0; j<=r2; ++j)
            {
            if (ch==z[ed1*(j-1)]) break;
            }
        if (j>r2) return(1);
        g=3; j0=j-5; if (j0<1) j0=1;
        sprintf(rivi1,"%d",j0); parm[1]=rivi1;
        sprintf(rivi2,"%d",j); parm[2]=rivi2;
        op_goto2(g,parm);
        c=1; disp();
        return(1);
        }


void next_tab()
        {
        register int i,j;
        char x[LLENGTH];
        char *p;

        for (i=0, j=1; j<=ed2; i+=ed1, ++j)
                if (z[i]=='T') break;
        if (j>ed2)  /* ei T riviä */
            {
            c+=10; c=10*(c/10)+1; if (c>c3) c=c3; if (c>c2) c=c2;
            numtab=1;
            return;
            }
        /* T rivi olemassa */
        edread(x,j);
        p=strchr(x+c1+c,'T');
        if (p==NULL)
            {
            BEEP;
            if (c1==1 && r<r3) j=0; else j=1;
            c=c1=1; ++r; if (r>r3) { --r; ++r1; }
            if (r1>r2-r3+1) --r1;
            if (j) disp();
            return;
            }
        numtab=1;
        c=p-x-c1+1;
        col_display();
        }

void eras(int j,int col)    /* col=c1+c-1  */
        {
        char x[LLENGTH];
        edread(x,j);
        x[col]=EOS; strncat(x,space,ed1-col);
        edwrite(x,j,0);
        }

void line_erase(int j,int col)  /* col=c1+c-1  sama kuin erase si.c */
        {
        char x1[LLENGTH]; // RS REM , x1[LLENGTH];

        eras(j,col);
        if (zs[j]!=0)
            {
            edread(x1,zs[j]);
            if (!empty_line(x1+col,ed1-col))
                {
                x1[col]=EOS;
                strncat(x1,space,ed1-col);
                edwrite(x1,zs[j],0);
                testshad(j);
                return;
                }
            }
        }

void find_margins(int j1,int j2)
        {
        int i,j,reuna;
        char x[LLENGTH];
        int npos[LLENGTH];

        c_vasen=c2; c_oikea=0;
        r_alku=j1; r_loppu=j2;
        for (i=0; i<ed1; ++i)  npos[i]=0;

        j=j1;
        edread(x,j);
        if (empty_line(x+1,c2))
            { c_vasen=mc; c_oikea=c3; r_alku=r_loppu=j; return; }
        i=1;
        while (i<c2 && x[i]==' ') ++i;
        reuna=i;
        while (j>1)
            {
            --j;
            edread(x,j);
            if (empty_line(x+1,c2) || *x=='-' ) { r_alku=j+1; break; }
            i=1;
            while (i<c2 && x[i]==' ') ++i;
            if (i>reuna) { r_alku=j; break; }
            if (i<reuna) { r_alku=j+1; break; }
            --r_alku;
            }
        j=j2;
        edread(x,j);
        i=1;
        while (i<c2 && x[i]==' ') ++i;
        reuna=i;
        while (j<ed2)
            {
            ++j;
            edread(x,j);
            if (empty_line(x+1,c2) || *x=='-' ) { r_loppu=j-1; break; }
            i=1;
            while (i<c2 && x[i]==' ') ++i;
            if (i>reuna) { r_loppu=j-1; break; }
            if (i<reuna) reuna=i;
            ++r_loppu;
            }
        for (j=r_alku; j<=r_loppu; ++j)
            {
            edread(x,j);
            if (empty_line(x+1,c2)) continue;
            i=1;
            while (i<c2 && x[i]==' ') ++i;
            ++npos[i];
            i=c2;
            while (i>1 && x[i]==' ') --i;
            if (i>c_oikea) c_oikea=i;
            c_vasen=1; j=0;
            for (i=1; i<ed1; ++i)
                if (npos[i]>j) { c_vasen=i; j=npos[i]; }
            }
        }

void del_char(char *s,int pos)
        {
        int i,len;
        len=strlen(s);
        for (i=pos; i<len-1; ++i) s[i]=s[i+1];
        s[len-1]=' ';
        }

void ins_char(char *s,int pos)
        {
        int i,len;
        len=strlen(s);
        for (i=len-1; i>pos; --i) s[i]=s[i-1];
        s[pos]=' ';
        }

void b_insertl(
int rivi       /* norm. r1+r-1 */
)
        {
        unsigned int j;
        char x[LLENGTH]; // RS REM , x1[LLENGTH];

        edread(x,r2);
        if (!empty_line(x+1,c2)) { BEEP; return; } // RS CHA exit(1) -> return
        memmove(z+(rivi+1)*ed1,z+rivi*ed1,(r2-rivi-1)*ed1);
        strcpy(x,"*"); strncat(x,space,c2);
        edwrite(x,rivi+1,0);
        j=ed2-1;
        for (; j>rivi; --j) zs[j+1]=zs[j];
        zs[rivi+1]=0;
        }


int b_insert(int j,int col)
        {
        char x[LLENGTH], x1[LLENGTH];

        edread(x,j);
        if (x[c2]!=' ') { BEEP; return(-1); }
        strcpy(x1,x); x[col]=EOS;
        strcat(x," "); strcat(x,x1+col); x[ed1]=EOS;
        edwrite(x,j,0);
        if (zs[j]!=0)
            {
            edread(x,zs[j]);
            strcpy(x1,x); x[col]=EOS;
            strcat(x," "); strcat(x,x1+col); x[ed1]=EOS;
            edwrite(x,zs[j],0);
            testshad(j);
            }
        return(1);
        }


void b_deletel(int rivi)
        {
        unsigned int j;
        char x[LLENGTH]; // RS REM , x1[LLENGTH];
        unsigned int l;

        memmove(z+(rivi-1)*ed1,z+rivi*ed1,(r2-rivi+1)*ed1);
        strcpy(x,"*"); strncat(x,space,c2);
        edwrite(x,r2,0);

        j=rivi;                       /* 6.11.88 */
        if (zs[j]!=0)
            {
            z[(zs[j]-1)*ed1]='\0'; zs[j]=0;
            }

        l=ed2-1;
        for (j=rivi; j<=l; ++j) zs[j]=zs[j+1];
        zs[l+1]=0;
        }

void b_delete(int j,int col)
        {
        char x[LLENGTH];

        edread(x,j);
        x[col]=EOS; strcat(x,x+col+1); strcat(x," ");
        edwrite(x,j,0);
        if (zs[j]!=0)
            {
            edread(x,zs[j]);
            x[col]=EOS; strcat(x,x+col+1); strcat(x," ");
            edwrite(x,zs[j],0);
            testshad(j);
            }
        }

void b_shadt(unsigned int j,unsigned int i)
        {
        testshad(j); testshad(i);
        }

void b_line_merge(int j,int col)    /* norm. col=c1+c-1 */
        {
        unsigned int len,shad;
        char x[LLENGTH], x1[LLENGTH];
        char sx[LLENGTH], sx1[LLENGTH];

        if (j==r2) return;
        shad=0; if ( zs[j]!=0 || zs[j+1]!=0 ) shad=1;
        edread(x,j);
        edread(x1,j+1);
        if (shad)
            {
            if (zs[j]==0) creatshad(j);
            if (zs[j+1]==0) creatshad(j+1);
            edread(sx,zs[j]); edread(sx1,zs[j+1]);
            }
        if (empty_line(x+col,c2-col+1))
            {
            len=c2+1;
            while (x1[len-1]==' ') x1[--len]=EOS;
            if (len>c2-col+1) { if (shad) b_shadt(j,j+1); return; }
            x[col]=EOS; strcat(x,x1+1);
            edwrite(x,j,0);
            edwrite(space,j+1,1);
            if (shad)
                {
                sx1[len]=EOS;
                sx[col]=EOS; strcat(sx,sx1+1);
                edwrite(sx,zs[j],0);
                edwrite(space,zs[j+1],1);
                b_shadt(j,j+1);
                }
            return;
            }
        if (!empty_line(x1+1,c2)) return;
        x1[1]=EOS; strncat(x1,x+col,c2-col+1);
        edwrite(space,j,col);
        edwrite(x1,j+1,0);
        if (shad)
            {
            sx1[1]=EOS; strncat(sx1,sx+col,c2-col+1);
            edwrite(space,zs[j],col);
            edwrite(sx1,zs[j+1],0);
            b_shadt(j,j+1);
            }
        }



int insert_words()
        {
        int i,k,h,rivi;
        char x[LLENGTH],*osa[6];
        int npois;
        int lisrivit;

        lisrivit=0;
        edread(x,mr);
        if (!empty_line(x+mc+1,c2-mc))
            {
            b_insertl(mr); ++lisrivit;
            b_line_merge(mr,mc);
            edread(x,mr+1);
            for (i=0; i<c_vasen-1; ++i)
                {
                k=b_insert(mr+1,1);
                if (k<0) break;
                }
            edread(x,mr+1);
            i=0;
            while (x[c_vasen]==' ' && i<LLENGTH)
                {
                b_delete(mr+1,c_vasen);
                edread(x,mr+1);
                ++i;
                }
            }

        rivi=mr;
        survoxxx=muste_fopen2(survowrd,"rt");
        if (survoxxx==NULL) return(-1);
        fgets(x,LLENGTH-1,survoxxx);   /* n_save mc1 mc2 mr1 mr2 c_vasen1 */
        if (move_from_store)
            {
            split(x,osa,6);
            n_save=atoi(osa[0]); mc1=atoi(osa[1]); mc2=atoi(osa[2]);
            mr1=atoi(osa[3]); mr2=atoi(osa[4]); c_vasen1=atoi(osa[5]);
            }
        marg_ero=c_vasen1-c_vasen;
        for (i=0; i<n_save; ++i)
            {
            ++rivi;
            b_insertl(rivi-1); ++lisrivit;
            fgets(x,LLENGTH-1,survoxxx);
            x[ed1]=EOS;
            edwrite(x,rivi,0);
            fgets(x,LLENGTH-1,survoxxx);
            if (!empty_line(x,c2+1))
                {
                k=creatshad(rivi); if (k<0) return(-1);
                edwrite(x,zs[rivi],0);
                }
            k=marg_ero;
            if (k>0)
                for (h=0; h<k; ++h) b_delete(rivi,1);
            else if (k<0)
                for (h=0; h<-k; ++h) b_insert(rivi,1);
            }
        muste_fclose(survoxxx);

        k=marg_ero;
        if (k>0)
            for (h=0; h<k; ++h) b_insert(mr+1,1);
        else if (k<0)
            for (h=0; h<-k; ++h) b_delete(mr+1,1);

        npois=0;
        for (i=1; i<mc1; ++i)
            { b_delete(mr+1,1); ++npois; }
        for (i=0; i<c_vasen; ++i)
            { b_insert(mr+1,1); --npois; }
        edread(x,mr+1);
        if (!empty_line(x+c_vasen,c2-c_vasen+1))
            {
            while (x[c_vasen]==' ')
                {
                b_delete(mr+1,c_vasen); ++npois;
                edread(x,mr+1);
                }
            }

        if (mr2!=mr1)
           {
           line_erase(mr+n_save,mc2+1-marg_ero);
           }
        else
           line_erase(mr+n_save,mc2+1-npois);


        return(lisrivit);
        }

void autotrim(int rivi1,int rivi2)  // RS pois?
        {
        int i,k,vr,vr1;
        char x[LLENGTH],*sana[3];
        char y[LLENGTH];

        if (*trim_command!='T') return;
        if (rivi1>rivi2) return;
        if (rivi1==rivi2)
            {
            edread(x,rivi1);
            if (empty_line(x+1,c2)) return;
            }
        vr=r; vr1=r1;
        r=1; r1=rivi1;
        strcpy(x,trim_command);
        i=split(x,sana,3);
        k=sprintf(y,"%s %d %d",sana[0],rivi1+1,rivi2+1);
        if (i>1) k+=sprintf(y+k," %s",sana[1]);
        if (i>2) k+=sprintf(y+k," %s",sana[2]);
        b_insertl(rivi1-1);
        edwrite(y,rivi1,1);
        activate();
        b_deletel(rivi1);
        r=vr; r1=vr1;
        move_ind=0;
        prompt_line=NULL;
        disp();
        }



void store_move()
        {
        int k;
        char x[LLENGTH];

        move_from_store=1;
        find_margins(mr,mr);
        k=insert_words(); if (k<0) return;
        r_loppu+=k;

        edread(x,mr);
        if (empty_line(x+1,c2))  /*  siirrettäessä kappaleen alkuun */
            {
            b_deletel(mr);
            if (r_alku>mr) --r_alku;
            if (r_loppu>mr) --r_loppu;
            }
        if (hae_apu("autotrim",trim_command)) autotrim(r_alku,r_loppu);  // RS pois?
        }

int delete_words()
        {
        int i,rivi;
        char x[LLENGTH];
        int poistetut_rivit;
        int eka_pois;

        poistetut_rivit=0;
        poistetut_merkit=0; /* viim.rivin edestä */
        if (mr1==mr2)
            {
            for (i=0; i<mc2-mc1+1; ++i)
                b_delete(mr1,mc1);
            edread(x,mr1);
            if (empty_line(x+1,c2))
                {
                b_deletel(mr1);
                ++poistetut_rivit;
                return(poistetut_rivit);
                }
            if (empty_line(x+mc1,c2-mc1+1)) return(poistetut_rivit);
            while (x[mc1]==' ')
                {
                b_delete(mr1,mc1);
                edread(x,mr1);
                }
            return(poistetut_rivit);
            }

        line_erase(mr1,mc1);
        eka_pois=mr1+1;
        edread(x,mr1);
        if (empty_line(x+1,c2))
            eka_pois=mr1;
        for (rivi=mr2-1; rivi>=eka_pois; --rivi)
            {
            b_deletel(rivi);
            ++poistetut_rivit;
            }

        rivi=eka_pois;
        edread(x,rivi);
        if (empty_line(x+mc2+1,c2-mc2))
            {
            b_deletel(rivi);
            ++poistetut_rivit;
            return(poistetut_rivit);
            }

        for (i=0; i<mc2-c_vasen+1; ++i)
            { b_delete(rivi,c_vasen); ++poistetut_merkit; }
        edread(x,rivi);
        if (!empty_line(x+c_vasen,c2-c_vasen+1))
            {
            while (x[c_vasen]==' ')
                {
                b_delete(rivi,c_vasen); ++poistetut_merkit;
                edread(x,rivi);
                }
            }
        return(poistetut_rivit);
        }

int m_move_text()
    {

    prompt_line=prompt_space;
    LOCATE(r3+2,1); PR_EBLK;
sprintf(prompt_line,
  "Defining a block by the rightmost mouse button (See MOUSE6?)     Cancel by DEL!");
sur_print(prompt_line);

    PR_ENRM;
    return(1);
    }

int script_save(int mr1,int mr2,int mc1,int mc2)
    {
    char sname[LNAME];
    int j;
    char x[LLENGTH];

    if (hae_apu("script_file",sname)==0) return(1);
    survoxxx=muste_fopen2(sname,"wt");
    if (survoxxx==NULL) return(1);
    if (mr1>mr2) { j=mr1; mr1=mr2; mr2=j; }
    if (mc1>mc2) { j=mc1; mc1=mc2; mc2=j; }
    for (j=mr1; j<=mr2; ++j)
        {
        edread(x,j);
        x[c2+1]=EOS;
        fprintf(survoxxx,"%s\n",x+mc1);
        }
    muste_fclose(survoxxx);
    return(1);
    }


int save_words(char *tiedosto)
        {
        int j;
        char x[LLENGTH];

        survoxxx=muste_fopen2(tiedosto,"wt");
        if (survoxxx==NULL) return(-1);

        n_save=mr2-mr1+1;
        fprintf(survoxxx,"%d %d %d %d %d %d\n",n_save,mc1,mc2,mr1,mr2,c_vasen);
        for (j=mr1; j<=mr2; ++j)
            {
            edread(x,j);
            fprintf(survoxxx,"%s\n",x);
            if (zs[j]==0)
                { strncpy(x,space,ed1); x[ed1]=EOS; }
            else
                edread(x,zs[j]);
            fprintf(survoxxx,"%s\n",x);
            }
        muste_fclose(survoxxx);
        return(1);
        }

void move_in_line()
        {
        int i,k;
        char x[LLENGTH],xs[LLENGTH];
        char x2[LLENGTH],xs2[LLENGTH];
        int npois;

        if (mc>=mc1 && mc<=mc2)
            {
            save_words(survowrd);
            return;
            }
        edread(x,mr);
        if (zs[mr]==0)
            { strncpy(xs,space,ed1); xs[ed1]=EOS; }
        else
            edread(xs,zs[mr]);

        strcpy(x2,x); strcpy(xs2,xs);
        if (mc1>mc2) { i=mc1; mc1=mc2; mc2=i; }
        npois=0;
        for (i=0; i<mc2-mc1+1; ++i) { del_char(x2,mc1); del_char(xs2,mc1); ++npois; }
        if (x[mc1-1]==' ' && xs[mc1-1]==' ' && x[mc2+1]==' ' && xs[mc2+1]==' ')
            { del_char(x2,mc1); del_char(xs2,mc1); ++npois; }
        if (mc>mc1) mc-=npois;

        k=mc1;
        for (i=mc; i<mc+mc2-mc1+1; ++i)
            {
            ins_char(x2,i); ins_char(xs2,i);
            x2[i]=x[k]; xs2[i]=xs[k];
            ++k;
            }
        if (x[mc1-1]==' ') { ins_char(x2,mc); ins_char(xs2,mc); }
        edwrite(x2,mr,0);
        if (zs[mr]>0) edwrite(xs2,zs[mr],0);
        }

int lmove(int j1,int j2)
        {
        int i;
// RS        char xs[LLENGTH];
        int lev;

        lev=mc2-mc1+1; if (lev>c2-mc+1) lev=c2-mc+1;
        memmove(z+(j2-1)*ed1+mc,z+(j1-1)*ed1+mc1,lev);  /* -21.2.90 memcpy */
        if (!zs[j1] && !zs[j2]) return(1);
/*      if (!zs[j1]) { i=shadow_create(j1); if (i<0) return(-1); }
        if (!zs[j2]) { i=shadow_create(j2); if (i<0) return(-1); }
*/
        if (!zs[j1]) { i=creatshad(j1); if (i<0) return(-1); }
        if (!zs[j2]) { i=creatshad(j2); if (i<0) return(-1); }

        memmove(z+(zs[j2]-1)*ed1+mc,z+(zs[j1]-1)*ed1+mc1,lev);
        testshad(j1);
        testshad(j2);

        return(1);
        }



void ei_tilaa()
        {
        sur_print("\nNot space enough for extra lines!");
        WAIT;
        }

int insert_lines(int jj,int k)
        {
// RS REM        int i;
        int j;
        char x[LLENGTH]; // RS REM , x1[LLENGTH];

        if (jj+k-1>r2) { ei_tilaa(); return(-1); } /* 4.1.1997 */
        for (j=r2; j>r2-k; --j)
            {
            edread(x,j);
            if (!empty_line(x+1,c2)) break;
            }
        if (j>r2-k) { ei_tilaa(); return(-1); }

        memmove(z+(jj+k-1)*ed1,z+(jj-1)*ed1,(r2-k-jj+1)*ed1);

        strcpy(x,"*"); strncat(x,space,c2);
        for (j=jj; j<jj+k; ++j)
            edwrite(x,j,0);

        for (j=r2-k; j>=jj; --j) zs[j+k]=zs[j];
        for (j=jj; j<jj+k; ++j) zs[j]=0;
        return(1);
        }


int block_from_store()
        {
        int i,j1,j2;
        char x[LLENGTH],*osa[3];
        int n_save,lev;
        char xs[LLENGTH];
// Rprintf("\nsurvoblo=%s|",survoblo); getck();
        survoxxx=muste_fopen2(survoblo,"rt");
        if (survoxxx==NULL) return(1);
        fgets(x,LLENGTH-1,survoxxx);   /* n_save mc1 mc2 mr1 mr2 c_vasen1 */
        split(x,osa,3);
        n_save=atoi(osa[0]); mc1=atoi(osa[1]); mc2=atoi(osa[2]);

        if (insert_mode)
            {
            ++mr;
            i=insert_lines(mr,n_save);
            if (i<0) { muste_fclose(survoxxx); return(-1); }
            }

        j2=mr;
        for (j1=0; j1<n_save; ++j1)
            {
            if (j2>ed2) break;
            fgets(x,LLENGTH-1,survoxxx);  // x[ed1]=EOS; 30.1.2000
            fgets(xs,LLENGTH-1,survoxxx); // xs[ed1]=EOS;
            lev=mc2-mc1+1; if (lev>c2-mc+1) lev=c2-mc+1;
            memcpy(z+(j2-1)*ed1+mc,x+mc1,lev);
            if (!zs[j2]) { i=creatshad(j2); if (i<0) { muste_fclose(survoxxx); return(1); } }
            memcpy(z+(zs[j2]-1)*ed1+mc,xs+mc1,lev);
            testshad(j2);
            ++j2;
            }
        muste_fclose(survoxxx);
        *info=EOS;
        move_ind=0;
        prompt_line=NULL;
        return(n_save);
        }


int word_move()
        {
        int i,k;
        char x[LLENGTH];

        if (mr==mr1 && mr==mr2 && mc==mc1 && mc==mc2)
            { store_move(); return(1); }
        if (mr1==mr2 && mr1==mr)
            { move_in_line(); return(1); }
        find_margins(mr1,mr2);
        r_alku1=r_alku; r_loppu1=r_loppu; c_vasen1=c_vasen; c_oikea1=c_oikea;
        i=save_words(survowrd); if (i<0) return(-1);
        if (mr>mr1)
            {
            if (mr<mr2) return(1);
            if (mr==mr2 && mc<=mc2) return(1);
            }
        else if (mr==mr1)
            {
            if (mc>=mc1) return(1);
            }
        poistetut_merkit=0;
        if (!insert_mode)
            {
            k=delete_words();
            if (mr==mr2) mc-=poistetut_merkit;
            if (mr>mr1) mr-=k;
            r_loppu1-=k;
            if (mr1<mr2) mr2=mr1+1;
            }

        find_margins(mr,mr);
        k=insert_words(); if (k<0) return(-1);
        r_loppu+=k;
        if (r_alku<r_alku1) { r_alku1+=k; r_loppu1+=k; }
        if (r_alku==r_alku1) r_loppu1+=k;

        edread(x,mr);
        if (empty_line(x+1,c2))  /*  siirrettäessä kappaleen alkuun */
            {
            b_deletel(mr);
            if (r_alku>mr) --r_alku;
            if (r_loppu>mr) --r_loppu;
            if (r_alku1>mr) --r_alku1;
            if (r_loppu1>mr) --r_loppu1;
            }

        if (hae_apu("autotrim",trim_command))  // RS pois?
            {
            if (r_alku1==r_alku)
                autotrim(r_alku,r_loppu);
            else if (r_alku1<r_alku)
                {
                autotrim(r_alku,r_loppu);
                if (!insert_mode) autotrim(r_alku1,r_loppu1);
                }
            else
                {
                if (!insert_mode) autotrim(r_alku1,r_loppu1);
                autotrim(r_alku,r_loppu);
                }
            }

        return(1);
        }


int word_erase()
        {
        int i,k;

        find_margins(mr1,mr2);
        r_alku1=r_alku; r_loppu1=r_loppu; c_vasen1=c_vasen; c_oikea1=c_oikea;
        i=save_words(survowrd); if (i<0) return(-1);
        poistetut_merkit=0;
        k=delete_words();
        if (mr==mr2) mc-=poistetut_merkit;
        if (mr>mr1) mr-=k;
        r_loppu-=k;
        if (mr1<mr2) mr2=mr1+1;
        if (hae_apu("autotrim",trim_command)) autotrim(r_alku,r_loppu);  // RS pois?
        return(1);
        }

int sur_move()
        {
        int i,j,k,h;

        if (move_words) { word_move(); return(1); }
        if (mr==mr1 && mr==mr2 && mc==mc1 && mc==mc2)
            { block_from_store(); soft_disp(1); return(1); }

        save_words(survoblo);

        h=1;
        if (insert_mode)
            {
            if (mr>=mr1 && mr<mr2) return(-1);
            k=mr2-mr1+1;
            if (mr<=mr1 && (mr!=mr1 || k!=1)) { mr1+=k; mr2+=k; h=2; }
                        /** 20.8.1995 *****/
            ++mr;
            i=insert_lines(mr,k);
            if (i<0) return(-1);
            }
        if (mr<=mr1)
            for (j=mr1; j<=mr2; ++j)
                {
                if (mr+j-mr1<1 || mr+j-mr1>r2) continue;
                i=lmove(j,mr+j-mr1);
                if (i<0) return(-1);
                }
        else
            for (j=mr2; j>=mr1; --j)
                {
                if (mr+j-mr1<1 || mr+j-mr1>r2) continue;
                i=lmove(j,mr+j-mr1);
                if (i<0) return(-1);
                }
        return(h);
        }


int block_erase()
        {
        int j;
// RS REM        char xs[LLENGTH];
        int lev;

        m_move_ind2=0;
        if (move_words) { word_erase(); return(1); }
        save_words(survoblo);
        lev=mc2-mc1+1;
        for (j=mr1; j<=mr2; ++j)
            {
            memcpy(z+(j-1)*ed1+mc1,space,lev);
            if (!zs[j]) continue;
            memcpy(z+(zs[j]-1)*ed1+mc1,space,lev);
            testshad(j);
            }
        move_ind=0;
        prompt_line=NULL;
        return(1);
        }

int op_block(int rr,int cc)
        {
// RS REM        char block[LLENGTH];
        int i;

        m_move_ind=0; // 21.3.2004
        mr=rr; mc=cc;
        mr1=move_r1; mr2=move_r2;
        strcpy(survoblo,etmpd); strcat(survoblo,"SURVO.BLO");
        strcpy(survowrd,etmpd); strcat(survowrd,"SURVO.WRD");

        if (mr<0) block_erase();
        else
            {
            i=sur_move();
            if (i==2) return(1);
            }
        disp(); soft_disp(1);
        return(1);
        }

int disp_block_start()
    {
    char ch;

    ch=z[(r1+r-2)*ed1+c1+c-1]; if (ch==' ') ch='*';
    cursor(r,c); PR_EBLK; sprintf(sbuf,"%c",ch); sur_print(sbuf);
    PR_ENRM;
    return(1);
    }

int move_block(
int k  /* 0=move_block 1=move_words */
)
        {
        int i;
        static char sana1[7];  /* corner | word */
        static char sana2[5];  /* copy | move */
        static char sana3[13];  /* Erase block | Delete words */
        static int nappi;      /* CODE_MOVE | CODE_WORDS */
        static char sana4[7];  /* alt-F4 | alt-F2 */
        char ch;
        extern int muste_selection;
        
        if (muste_selection) { move_clear(); muste_selection=FALSE; } // RS
        
        pyyhi_alarivi();
        LOCATE(r3+2,1); /* PR_EBLD; */ PR_EBLK;
        prompt_line=prompt_space;
        switch (move_ind)
            {
          case 0:
            soft_disp(0);
            move_words=k;
            if (move_words)
                {
                strcpy(sana1,"word"); strcpy(sana2,"move"); strcpy(sana3,"Delete words");
                strcpy(sana4,"alt-F2");
                nappi=CODE_WORDS;
                *sbuf=EOS;
                }
            else
                {
                strcpy(sana1,"corner"); strcpy(sana2,"copy"); strcpy(sana3,"Erase block");
                strcpy(sana4,"alt-F4");
                nappi=CODE_MOVE;
                sprintf(sbuf,"  (Exit from %s by F8!)",system_name);
                }

//          sprintf(prompt_line,"Mark %s 1 by %s. %s=Cancel%s",
//           sana1,key_label[nappi],key_label[CODE_DELETE],sbuf);
           sprintf(prompt_line,"Mark %s 1 by %s. %s=Cancel%s",sana1,sana4,key_label[CODE_DELETE],sbuf);
// RS ALT            sprintf(prompt_line,"Mark %s 1 by %s. DEL=Cancel%s",sana1,sana4,sbuf); // RS
            sprintf(sbuf,"%s",prompt_line); sur_print(sbuf);
            move_ind=1;
            break;
          case 1:
            move_r1=r1+r-1; mc1=c1+c-1;
//          sprintf(prompt_line,"Mark %s 2 by %s. %s=Cancel",
//           sana1,key_label[nappi],key_label[CODE_DELETE]);

           sprintf(prompt_line,"Mark %s 2 by %s. %s=Cancel",sana1,sana4,key_label[CODE_DELETE]);
//RS ALT            sprintf(prompt_line,"Mark %s 2 by %s. DEL=Cancel",sana1,sana4); // RS

            sprintf(sbuf,"%s",prompt_line); sur_print(sbuf);
            move_ind=2;
            disp_block_start();
//          ch=z[(r1+r-2)*ed1+c1+c-1]; if (ch==' ') ch='*';
//          cursor(r,c); PR_EBLK; sprintf(sbuf,"%c",ch); sur_print(sbuf);
//          PR_ENRM;
            break;
          case 2:
            move_r2=r1+r-1; mc2=c1+c-1;
            if (move_r2<move_r1) { i=move_r1; move_r1=move_r2; move_r2=i; }
            if (!move_words && mc2<mc1) { i=mc1; mc1=mc2; mc2=i; }

            script_save(move_r1,move_r2,mc1,mc2); // 11.12.2005

          if (m_move_ind) // 21.3.2004
            {
            m_move_text();
            }
          if (!m_move_ind) // 21.3.2004
            {

            sprintf(prompt_line,
             "Select place and %s by %s. %s=%s, %s=Cancel",
              sana2,sana4,key_label[CODE_ERASE],sana3,key_label[CODE_DELETE]);
/* RS ALT
            sprintf(prompt_line,
             "Select place and %s by %s. CTRL-END=%s, DEL=Cancel",
              sana2,sana4,sana3); */ // RS

            sprintf(sbuf,"%s",prompt_line); sur_print(sbuf);
            }

            move_ind=3;
            ch=prompt_shadow; prompt_shadow='5';
            disp();
            if (m_move_ind) // 21.3.2004
              {
              m_move_text();
              }
            prompt_shadow=ch;
       soft_disp(1); // 7.1.2002
            m_move_ind=0; // 21.3.2004
            break;
          case 3:
            op_block(r1+r-1,c1+c-1);
            move_r1=mr1; move_r2=mr2;
            disp();
            break;
            }
        return(1);
        }


static int copy_to_clipboard(); // RS declaration
static int test_empty_space(int r0,int c0)
    {
    int j;
    int kork,lev;
    char x[LLENGTH];

    if (move_r1<=r0 && r0<=move_r2 && mc1<=c0 && c0<=mc2)
        { move_ind=3; copy_to_clipboard(); return(2); }
//      return(2); // paikallaan kop.

    kork=move_r2-move_r1+1; lev=mc2-mc1+1;

    for (j=r0; j<r0+kork; ++j)
        {
        edread(x,j);
        if (strncmp(x+c0,space,lev)!=0) return(-1);
        }
    return(1);
    }

// int m_move_ind=0; // no mouse right button pressed!
// int m_move_r1,m_mc1;
int mouse_define_block()
    {
    int i=0;
    extern int m_double_click;
    extern int muste_selection;

    if (m_double_click) return(1);
    
	if (muste_selection) { move_clear(); muste_selection=FALSE; } // RS

    if (m_move_ind2)
        {
// Rprintf("\nsize: %d %d|",move_r2-move_r1+1,mc2-mc1+1); getck();
// Rprintf("\ninsert: %d %d|",insert_type,insert_mode); getck();
        if (!insert_type || !insert_mode)
           {
           i=test_empty_space(r1+r-1,c1+c-1);
           if (i<0) return(1);
           }
        if (i==2) return(1);
        move_words=0; // 22.6.2004
        op_block(r1+r-1,c1+c-1);
        m_move_text();
        return(1);
        }

    switch (m_move_ind)
        {
      case 0:
// Rprintf("\n0: r=%d c=%d",r,c); getck();
        move_clear();
        disp_block_start();
        m_move_text();
/***************************
        LOCATE(r3+2,1); PR_EBLK;
        sur_print(
"Opposite corner of the block by the rightmost mouse button!  Cancel=DEL");
        PR_ENRM; cursor(r,c);
******************************/
        m_move_r1=r1+r-1; m_mc1=c1+c-1; ++m_move_ind; break;

      case 1:
// Rprintf("\n1: r=%d c=%d",r,c); getck();
        move_r1=m_move_r1; mc1=m_mc1;
        move_ind=2;
        move_block(0);
        m_move_ind2=1;

        break;
      default: m_move_ind=0;
        }

    return(1);
    }





void era(unsigned int j)
                        {
                        char x[LLENGTH];

                        edread(x,j);
                        x[c1+c-1]=EOS; strncat(x,space,ed1-c1-c+1);
                        edwrite(x,j,0);
                        displine2(j);
                        }

void muste_erase()
        {
                    unsigned int j;
                    char x1[LLENGTH]; // RS REM, x1[LLENGTH]; 

                    if (move_ind==3)
                        {      
                        op_block(-1,0);
                        return;
                        }
                    j=r1+r-1;
                    if (zs[j]!=0)
                        {
                        edread(x1,zs[j]);
                        if (!empty(x1+c1+c-1,ed1-c1-c+1))
                            {
                            x1[c1+c-1]=EOS;
                            strncat(x1,space,ed1-c1-c+1);
                            edwrite(x1,zs[j],0);
                            testshad(j);
                            displine2(j);
                            return;
                            }
                        era(j);
                        }
                    else
                        era(j);
        }


void code_code()
        {
        char luku[5];

        if (special_code==0)
            {
            cursor(r3+1,1); PR_EBLD;
            *luku=EOS; prompt_editor("Enter key code: ",luku,5);
            if (*luku==EOS) special_code=(int)(*(z+ed1*(r1+r-2)+c1+c-1));
            else special_code=atoi(luku);

            disp_prompt_line('1'); PR_ENRM; cursor(r,c);

        /*  cursor(r3+1,1); ERASE; PR_ENRM; cursor(r,c);  */
            }
        key_common(special_code);
        }

int s_scroll_right()
    {
    if (c1>c2-c3) return(1);
    ++c1; if (c>1) --c;
    disp();
    return(1);
    }

int s_scroll_left()
    {
    if (c1==1) return(1);
    --c1; if (c<c3) ++c;
    disp();
    return(1);
    }

int s_scroll_up()
    {
    if (r1==1) return(1);
    --r1; if (r<r3) ++r;
    disp();
    return(1);
    }

int s_scroll_down()
    {
    if (r1>r2-r3) return(1);
    ++r1; if (r>1) --r;
    disp();
    return(1);
    }


static FILE *time_file;
int time_file_on=0;
static double file_time1;
static struct timeb file_countb;
double timecount1,timecount2,timestart; /* 7.2.1999 */

int open_time_file(char *s)
    {
    if (muste_strcmpi(s,"CLOSE")==0) { muste_fclose(time_file); time_file_on=0; return(1); }
    time_file_on=1;
    time_file=muste_fopen2(s,"w+t");

    // tarvitaan, jotta itse TIME COUNT FILE ei antaisi hassua aikaa!
    ftime(&file_countb);
    file_time1=file_countb.time;
    return(1);
    }

int file_time_end(char *oper)
    {
    double file_time2;
    char *p;

    if (*oper==' ') return(1); // tyhjä rivi!
    while ((p=strchr(oper,' '))!=NULL) *p='_';

    ftime(&file_countb);
    file_time2=file_countb.time+0.001*(double)file_countb.millitm;
    fprintf(time_file,"%s %.3f\n",oper,file_time2-file_time1);
    file_time1=file_time2;
    return(1);
    }

int file_time_start()
    {
    return(file_time_end("---"));
    }


int op_time()
        {
        char aika[26], x[LLENGTH];
        struct timeb count;

        if (g>2 && muste_strcmpi(parm[1],"COUNT")==0)
            {
            if (g>3 && muste_strcmpi(parm[2],"FILE")==0)
                {
                open_time_file(parm[3]); return(2);
                }
            ftime(&count);
            timecount2=count.time+0.001*(double)count.millitm;
            if (muste_strcmpi(parm[2],"START")==0)
                {
                timecount1=timecount2; timestart=0.0;
                if (g>3) timestart=atof(parm[3]);
                }
            else
                {
                edwrite(space,r1+r-1,1,1);
   sprintf(sbuf,"TIME COUNT END   %.3f",timestart+timecount2-timecount1);
                edwrite(sbuf,r1+r-1,1);
                }
            return(2);
            }

        pvmaika(aika);
        strcpy(x,"TIME "); strcat(x,aika); strncat(x,space,c2-strlen(x));
        edwrite(x,r1+r-1,1);
        return(2);
        }

int sur_wait(
long aika,         /* millisek. */
int (*display)(),  /* näyttötoimitus sekuntien välillä */
int katko         /* 1=keskeytys napilla 0=ei keskeytysmahdollisuutta */
)                 /* Jos keskeytetty, return(1); muuten return(0); */
        {
        struct timeb tb;
        long alku,alkums,loppums,sek;

        if (aika<1000L) { sur_sleep(aika); return(sur_kbhit()); }
        ftime(&tb); alku=tb.time;
        alkums=tb.millitm;
        loppums=alkums+aika;
        ftime(&tb); sek=alku;
        while ( 1000*(tb.time-alku)+tb.millitm<loppums )
            {
            sur_sleep(10);
            if (katko && sur_kbhit()) return(1);
            ftime(&tb);
            if (tb.time>sek) { (*display)(); sek=tb.time; }
            }
        return(0);
        }


int sur_wait2(
long aika,         /* millisek. */
int (*display)()  /* näyttötoimitus sekuntien välillä */
// RS int katko         /* 1=keskeytys napilla 0=ei keskeytysmahdollisuutta */
)                 /* Jos keskeytetty, return(1); muuten return(0); */
        {
        int i;
        struct timeb tb;
        long alku,alkums,loppums,sek;

        ftime(&tb); alku=tb.time;
        alkums=tb.millitm;
        loppums=alkums+aika;
        ftime(&tb); sek=alku;
        while ( 1000*(tb.time-alku)+tb.millitm<loppums )
            {
            sur_sleep(10);
            i=sur_m2kbhit();
            if (i) return(i);
            ftime(&tb);
            if (tb.time>sek) { (*display)(); sek=tb.time; }
            }
        return(0);
        }



int lue_hetki(long *ptime)
        {
        static struct timeb tb;

        ftime(&tb);
        *ptime=1000L*tb.time+tb.millitm;
        return(1);
        }

int save_wait(int m)
        {
        long aika,a0,a1,a2;
        int i;
        static int pre_ind=0;

        if (pre_ind) { pre_ind=0; return(1); }    /* 1.6.1995 */
        if (m==CODE_PRE) pre_ind=1; else pre_ind=0;
        lue_hetki(&aika);
        a0=aika-wait_hetki; wait_hetki=aika;

// RS KORJAA Ei ehkä porttautuvaa koodia!
        a1=a0>>7; a2=a1>>2; /* "jako 100:lla" 1/128+1/512=0.0097.. */

        a0=a1+a2; if (a0==0L) return(1);
        sprintf(sbuf,"TW%ld@",a0);
        tutsave(1);
        for (i=0; i<strlen(sbuf); ++i) tutsave((int)sbuf[i]);
        return(1);
        }


int op_wait()
    {
// RS REM    double kesto;
//RS    extern int headline();

    if (g<2) { op_incomplete(); return(-1); }
    sur_wait((long)(1000.0*atof(parm[1])),headline_editor,1);
    return(1);
    }



int op_jump_end(int i)
        {
        int j,k;
        char x[LLENGTH],*px[3];

        j=i-1;
        while (j>1)
            {
            edread(x,j);
            k=split(x+1,px,1);
            if (k==0 || strcmp(px[0],"-")!=0) break;
            --j;
            }
        if (j<10) j=1;
        sprintf(x,"%d",j); px[1]=x;
        px[2]=x+30; sprintf(px[2],"%d",i);
        op_goto2(3,px);
        return(1);
        }

int op_jump(int ind) // 1=normaali - komento
        {
        char ch;
        int k,i,j;
        char *p,*q;
        char x[LLENGTH]; // RS REM ,*px[3];

        k=ed1*ed2-1; j=r1+r-1;
        ch=z[k]; z[k]=EOS;
        edread(x,j);
        if (strchr(x,'\376')==NULL)
            {
            p=strstr(x," / ");
            if (p==NULL) p=x+ed1;
            --p; while (*p==' ') --p;
            *(p+1)=EOS; q=strchr(x+1,'-'); if (q==NULL) return(1);
            ++q; while (*q==' ') ++q;
            }
        else q=parm[1];
        p=strstr(z,q);
        if (p==NULL) { z[k]=ch; return(-1); }
        i=(p-z)/ed1+1;
        if (i==j)
            {
            p=strstr(z+j*ed1,q);
            if (p==NULL) { z[k]=ch; return(-1); }
            i=(p-z)/ed1+1;
            }
        z[k]=ch;
        op_jump_end(i);
        return(1);
        }


int op_qpath()
        {
        char x[LLENGTH];

        if (g<2) return(-1);
        edread(x,r1+r-1); split(x+1,parm,2);
        edread(x,r1+r-1); strcpy(qpath,x+(int)(parm[1]-x));
        return(2);
        }


/* COUNT L1,L2,K,alkuarvo,askel,jakso */
/*       1  2  3 4        5     6     */
int op_count()
        {
        double alkuarvo=1.0;
        double askel=1.0;
        int jakso=0;
        double arvo;
        char maskline[LLENGTH],*mask[1];
        char sana[LLENGTH];
        int i,j,j1,j2,k,pos,neg;

        if (g<4)
            {
            sur_print("\nCorrect form:");
            sur_print("\nCOUNT L1,L2,K,[init_value],[increment],[period]");
            WAIT; return(-1);
            }

        j1=edline2(parm[1],1,1); if (j1==0) return(-1);
        j2=edline2(parm[2],j1,1); if (j2==0) return(-1);
        if (j2<j1) return(-1);
        k=edline2(parm[3],1,1); if (k==0) return(-1);
        if (g>4) alkuarvo=atof(parm[4]);
        if (g>5) askel=atof(parm[5]);
        if (g>6) jakso=atoi(parm[6]);

        edread(maskline,k);
        i=split(maskline+1,mask,1);
        if (i==0)
            {
            sprintf(sbuf,"\nMask missing on line %d",k); sur_print(sbuf);
            WAIT; return(-1);
            }

        pos=mask[0]-maskline;
        arvo=alkuarvo;
        k=0;
        if (jakso>0) neg=0; else { neg=1; jakso=-jakso; }
        for (j=j1; j<=j2; ++j)
            {
            fconv(arvo,mask[0],sana);
            edwrite(sana,j,pos);

            if (jakso)
                {
                ++k;
                if (k==jakso)
                    {
                    k=0;
                    if (neg) arvo+=askel;
                    else     arvo=alkuarvo;
                    }
                else
                    {
                    if (!neg) arvo+=askel;
                    }
                }
            else arvo+=askel;
            }
        return(1);
        }



static FILE *tied2;
int muuta_survo_apu(char *fil,char *key,char *val)
    {
    char x[LLENGTH];
    char x2[LNAME];
// RS REM    int i;
    char *p;
// RS REM    int j;

    FILE *tied; // RS puuttui?!?

    strcpy(x2,etmpd); strcat(x2,"SURVO.TMP");
    tied2=muste_fopen2(x2,"wt");
// Rprintf("\nfil=%s x2=%s|",fil,x2); WAIT;
    tied=muste_fopen2(fil,"rt");
    if (tied==NULL) return(1);
    while (1)
        {
        fgets(x,LLENGTH-1,tied);
        if (feof(tied)) break;
        p=strchr(x,'=');
        if (p==NULL || strncmp(x,key,strlen(key))!=0)
            {
            fprintf(tied2,"%s",x);
            continue;
            }
        fprintf(tied2,"%s=%s\n",key,val);
        }
    muste_fclose(tied);
    muste_fclose(tied2);
    sur_copy_file(x2,fil);
    return(1);
    }

int valitse_kieli(char *s)
        {
// RS REM        char *p;
        char lang[2];

        *crt_exit=*s;


        strcpy(sbuf,survo_path); strcat(sbuf,"SURVO.APU");
        *lang=*crt_exit; lang[1]=EOS;
        muuta_survo_apu(sbuf,"language",lang);
        set_console_title();  // RS pois?
        soft_keys_init();

        return(1);
        }


int kielenvalinta() // 7.9.2003
    {
// RS REM    char s;
    char kieli[2];

    if (*language!='0') return(1);

    sur_sleep(500L);
    disp();

    LOCATE(3,13); PR_EINV;
    sur_print("Select the working language for SURVO MM: ");
    LOCATE(4,13); PR_EINV;
    sur_print("Valitse SURVO MM:n työkieli: ");
    while (1)
        {
        LOCATE(5,14); PR_ENRM;
        sur_print("English=E Suomi=S ?  "); LOCATE(5,34);
        *kieli=(char)getck();
        sprintf(sbuf,"%c",*kieli); sur_print(sbuf);
        if(strchr("EeSs",*kieli)!=NULL) break;
        sur_sleep(300L);
        }

    if (*kieli=='E' || *kieli=='e') *kieli='2';
    else *kieli='1';
    kieli[1]=EOS;
    *language=*kieli;

    valitse_kieli(kieli);

    return(1);
    }

int survo_apu()
        {
        char x[LLENGTH];
        int i;
        char *p;
        int j;

       FILE *tied; // RS puuttui?!?

        tied=muste_fopen2(parm[2],"r+t");
        if (tied==NULL) return(1);
        while (1)
            {
            fgets(x,LLENGTH-1,tied);
            if (feof(tied)) break;
            p=strchr(x,'=');
            if (p==NULL) continue;
            *p=EOS;
            if (strcmp(x,parm[3])!=0) continue;
            ++p;
            i=strlen(p);
            if (*(p+i-1)=='\n') *(p+i-1)=EOS;
            j=r1+r-1;
            edwrite(space,j,1);
            sprintf(sbuf,"CLEAR SYSTEM %s %s=%s",parm[2],parm[3],p);
            edwrite(sbuf,j,1);
            }
        muste_fclose(tied);
        return(1);
        }


/*  !clear.c 1.6.1986/SM (21.1.1996)
    CLEAR  COPY  SET  OUTPUT   read labels  SAVE  CHECK
    CLEAR SYSTEM <survo.apu_pathname> <word>
 */
int op_clear()
        {
        int i,j,j1,j2,k;
        char x[LLENGTH];

        char clear[LLENGTH];

        char *p;
        int len;
        extern char survo_id[];

        if (strcmp(parm[1],"*")==0)
            {
            p=strchr(survo_id,' '); if (p==NULL) p=survo_id+5;
            len=p-survo_id;
            strncpy(x,survo_id,len); x[len]=EOS;
            if (strlen(tut_info)+strlen(x)>LLENGTH-2) return(1);
            strcat(tut_info,x); strcat(tut_info,"@");
            return(1);
            }
        if (strcmp(parm[1],"LANGUAGE")==0)
            {
            valitse_kieli(parm[2]);
            return(1);
            }
        if (strcmp(parm[1],"SYSTEM")==0)
            {
            survo_apu();
            return(1);
            }

        if (g<3)
            {
            sur_print("\nUsage:");
            sur_print("\n   CLEAR L1,L2  / cursor defines start column");
            sur_print("\n   or");
            sur_print("\n   CLEAR L1,L2,K  / K is mask line of form    XXXX   XXX");
            WAIT; return(1);
            }
        j1=edline2(parm[1],1,1); if (j1==0) return(1);
        j2=edline2(parm[2],j1,1); if (j2==0) return(1);
        strncpy(clear,space,ed1); clear[ed1]=EOS;
        if (g==3)
            for (i=c1+c-1; i<ed1; ++i) clear[i]='X';
        else
            {
            k=edline2(parm[3],1,1); if (k==0) return(1);
            edread(clear,k);
            }

        for (j=j1; j<=j2; ++j)
            {
            edread(x,j);
            for (i=1; i<ed1; ++i) if (clear[i]!=' ') x[i]=' ';
            edwrite(x,j,0);
            if (!zs[j]) continue;
            edread(x,zs[j]);
            for (i=1; i<ed1; ++i) if (clear[i]!=' ') x[i]=' ';
            edwrite(x,zs[j],0);
            testshad(j);
            }
        return(1);
        }

static FILE *tied;
int op_check(int laji)
 // =0 path required (CHECK0), =1 path not required (CHECK)
 // 16.7.2005
        {
        char x[LLENGTH];
        char y[LLENGTH];
        int i,j;
        char *p; // RS REM , *p2;

		if (*parm[1]!=EOS) // RS ADD 21.5.2012
		{
        strcpy(x,parm[1]);
        subst_survo_path_in_editor(x);

        if (laji==1) // 16.7.2005
            {
            p=strchr(x,'\\'); // 16.5.2005
            if (p==NULL) p=strchr(x,'/'); // RS ADD FIXME path
            if (p==NULL)
                {
                strcpy(y,edisk); strcat(y,x); strcpy(x,y);
                }
            }

        i=strlen(x);
        if (muste_strnicmp(x+i-4,"/NUL",4)==0) // RS CHA \\ -> /
            {
            x[i-4]=EOS;
            i=sur_file_exists(x); // tutkii hakemiston x
            if (i<0)
                {
                strcpy(x,"NOT FOUND!");
                }
            else strcpy(x,"OK");
            }
        else
            {
//   Rprintf("\ncheck: %s netd=%d|",x,netd(x)); getck();
            if (strchr(x,':')==NULL && !netd(x)) { strcpy(x,survo_path); strcat(x,parm[1]); }
                                   // 16.2.2006
            tied=muste_fopen2(x,"rb");
            if (tied==NULL) strcpy(x,"NOT FOUND!");
            else { muste_fclose(tied); strcpy(x,"OK"); }
            }
        }
        else { strcpy(x,"EMPTY"); }  // RS ADD 21.5.2012
            
        j=r1+r-1;
        edread(y,j);
        i=strlen(y); while (y[i-1]==' ') --i;
        edwrite(x,j,i+1);
        return(1);
        }

int op_nextfile() // NEXTFILE XYZ,PS
    {
    int i,j;
    char name[LNAME];
    char *p;

    i=1;
    while (1)
        {
        strcpy(sbuf,parm[1]);
        p=strchr(parm[1],':'); // RS FIXME path
        if (p==NULL) { strcpy(sbuf,edisk); strcat(sbuf,parm[1]); }
        sprintf(name,"%s%d.%s",sbuf,i,parm[2]);
        tied=muste_fopen2(name,"rb");
        if (tied==NULL) break;
        else muste_fclose(tied);
        ++i;
        }
    j=r1+r-1;
    edwrite(space,j,1);
    sprintf(sbuf,"NEXTFILE %s,%s / %s%d.%s",
                 parm[1],parm[2],  parm[1],i,parm[2]);
    edwrite(sbuf,j,1);
    return(1);
    }


int ins_empty(char *s)
        {
        while (*s) { if (*s!=' ') return(0); ++s; }
        return(1);
        }

static int insdel_l1,insdel_l2,insdel_k;

int op_insert()
        {
        unsigned int j;
        char x[2*LLENGTH], x1[2*LLENGTH];

        for (j=insdel_l1; j<=insdel_l2; ++j)
            {
            edread(x,j);
            if (!ins_empty(x+ed1-insdel_k))
                {
                sprintf(sbuf,"\nLine %d too long!",j);
                sur_print(sbuf); WAIT; return(-1);
                }
            strcpy(x1,x); x[c1+c-1]=EOS;
            strncat(x,space,insdel_k); strcat(x,x1+c1+c-1); x[ed1]=EOS;
            edwrite(x,j,0);
            if (zs[j]!=0)
                {
                edread(x,zs[j]);
                strcpy(x1,x); x[c1+c-1]=EOS;
                strncat(x,space,insdel_k); strcat(x,x1+c1+c-1); x[ed1]=EOS;
                edwrite(x,zs[j],0);
                testshad(j);
                }
            }
        return(1);
        }

int op_delete()
        {
        unsigned int j;
        char x[2*LLENGTH], x1[2*LLENGTH];

        for (j=insdel_l1; j<=insdel_l2; ++j)
            {
            edread(x,j); strcpy(x1,x); /* jotta myös c=0 kelpaa */
            x[c1+c-1]=EOS; if (strlen(x1)>c1+c+insdel_k-1) strcat(x,x1+c1+c+insdel_k-1);
            strncat(x,space,insdel_k);
            edwrite(x,j,0);
            if (zs[j]!=0)
                {
                edread(x,zs[j]);
                x[c1+c-1]=EOS; if (strlen(x1)>c1+c+insdel_k-1) strcat(x,x+c1+c+insdel_k-1);
                strncat(x,space,insdel_k);
                edwrite(x,zs[j],0);
                testshad(j);
                }
            }
        return(1);
        }

void insdel()
        {
        int i;
        char x[2*LLENGTH];

        if (g<3)
            {
            insdel_l1=insdel_l2=r1+r;
            edread(x,insdel_l2);
            while (!ins_empty(x+1) && insdel_l2<r2) edread(x,++insdel_l2);
            if (insdel_l2<r2) --insdel_l2;
            if (g==2) { insdel_k=atoi(parm[1]); if (insdel_k==0) return; } else insdel_k=1;
            }                          // 21.4.2003
        else
            {
            insdel_l1=edline2(parm[1],1); if (insdel_l1==0) return;
            insdel_l2=edline2(parm[2],insdel_l1); if (insdel_l2==0) return;
            if (g>=4) insdel_k=atoi(parm[3]); else insdel_k=1;
            }
        if (insdel_k<=0) insdel_k=1;
        if (insdel_k>c2) insdel_k=1;
        if (strchr("Ii",*parm[0])!=NULL) { i=op_insert(); return; }
        if (strchr("Dd",*parm[0])!=NULL) { i=op_delete(); return; }
        }

static int op_font() // ja op_window()
    {
    int fontsize,posx,posy;
// RS REM    extern int xp_font,xp_xpos,xp_ypos; // defined by survo.lib
    extern char muste_window_name[];

    if (g>1) 
       {
       if (muste_strcmpi(parm[1],"*")==0)
          {
          muste_choosefont();
          return(1);
          }

       fontsize=atoi(parm[1]);
       muste_font(fontsize);

       if (g>3) 
          { 
          posx=atoi(parm[2]);
          posy=atoi(parm[3]);
          sur_pos_window(muste_window_name,posx,posy);
          }
       }
    return(1);
    }

static int op_win()
    {
    int i,j;
    int par[4];
// RS REM    int win_type;
    char wname[128];

    extern char muste_window_name[];

    if (muste_strcmpi(parm[1],"TASKBAR")==0)
        {
        sur_taskbar_show(atoi(parm[2]));
        return(1);
        }

    strcpy(wname,muste_window_name);

    if (muste_strcmpi(parm[1],"MIN")==0)
        { sur_main_window_show(wname,0); return(1); }
    if (muste_strcmpi(parm[1],"NORMAL")==0)
        { sur_main_window_show(wname,1); return(1); }
    if (muste_strcmpi(parm[1],"MAX")==0)
        { sur_main_window_show(wname,2); return(1); }



    if (muste_strcmpi(parm[1],"FOCUS")==0)
        { sur_set_focus(wname); return(1); }


    if (muste_strcmpi(parm[1],"POS")==0)
        {
        sur_pos_window(wname,atoi(parm[2]),atoi(parm[3]));
        return(1);
        }

    if (muste_strcmpi(parm[1],"CHECK")==0) // 23.11.2002
        {
        i=sur_find_window(parm[2]);
        if (i<0) strcpy(wname,"NOT FOUND!");
        else strcpy(wname,"OK");
        j=r1+r-1;
        edread(sbuf,j);
        i=strlen(sbuf); while (sbuf[i-1]==' ') --i;
        edwrite(wname,j,i+1);
        }
    if (muste_strcmpi(parm[1],"SCREEN")==0)
        {
        sur_screen_dim(&par[0],&par[1]);
        edwrite(space,r1+r,1);
        sprintf(sbuf,"Current screen: %d %d",
                 par[0],par[1]);
        edwrite(sbuf,r1+r,1);
        return(1);
        }


    if (muste_strcmpi(parm[1],"MOVE")==0)
        {
        sur_move_window(wname,atoi(parm[2]),atoi(parm[3]),
                        atoi(parm[4]),atoi(parm[5]));
        return(1);
        }


    if (muste_strcmpi(parm[1],"WINDOW")==0 || muste_strcmpi(parm[1],"SURVO")==0) // 6.7.2006
        {
// RS REM        int wx,wy;

        if (muste_strcmpi(parm[2],"FONT")==0)
            {
            muste_font(atoi(parm[3]));  //RS CHA  set_regkeys(atoi(parm[3]),0,0);
            }

// parametrit esim. Lucinda_Console,54,20,400
         else muste_fixme("FIXME: Set fonts with parameters not implemented!\n"); // RS FIXME
// RS REM        else set_window_regkeys(parm[2],atoi(parm[3]),atoi(parm[4]),atoi(parm[5]));
        return(1);
        }



    if (muste_strcmpi(parm[1],"GET")==0)
        {


        if (g>2 && muste_strcmpi(parm[2],"HELP")==0) // 19.9.2001
            {
muste_fixme("FIXME: WIN GET HELP not implemented; returning main window size!\n"); // RS FIXME
// RS REM            sprintf(wname,"%s - Help Window",system_name);
            }
        else
            { 
            if (g>2 && muste_strcmpi(parm[2],"OS")==0) // 4.10.2001
            {
muste_fixme("FIXME: WIN GET OS not implemented; returning main window size!\n"); // RS FIXME
// RS REM            strcpy(wname,os_win_name);
            }
            }
            
        sur_get_window_rect(wname,par);
        edwrite(space,r1+r,1);
        sprintf(sbuf,"Current window: %d %d %d %d",
                 par[0],par[1],par[2],par[3]);
        edwrite(sbuf,r1+r,1);
        return(1);
        }


    if (muste_strcmpi(parm[1],"FONT")==0)
        {

        if (g>2 && muste_strcmpi(parm[2],"GET")==0)
            {
            if (g>3 && muste_strcmpi(parm[3],"HELP")==0) // 19.9.2001
                {
muste_fixme("FIXME: WIN FONT GET HELP not implemented; returning main window font!\n"); /* RS FIXME 
                sprintf(wname,"%s - Help Window",system_name);
                sur_get_font2(wname,par,0); // ei r_soft+1
*/
                }
            else if (g>3 && muste_strcmpi(parm[3],"OS")==0) // 4.10.2001
                {
muste_fixme("FIXME: WIN FONT GET OS not implemented; returning main window font!\n"); /* RS FIXME
                strcpy(wname,os_win_name);
                sur_get_font2(wname,par,0);
*/
                }
            else sur_get_font(wname,par);
            edwrite(space,r1+r,1);
            sprintf(sbuf,"Current font: %d %d",
                     par[0],par[1]);
            edwrite(sbuf,r1+r,1);
            return(1);
            }
        else
            {
            if (g>3) 
              {
              muste_fixme("\nFIXME: WIN FONT fontx,fonty,homex,homy not implemented!\n");
              }
/* RS FIXME 
            font_x=atoi(parm[2]); font_y=atoi(parm[3]);
            if (font_x==0 || font_y==0) { font_x=0; return(1); }
            if (g>=6) { home_x=atoi(parm[4]); home_y=atoi(parm[5]); }
            if (strcmp(os_ver,"NT")==0) // 15.6.2002
   sur_nt_resize_in_given_font(wname,font_x,font_y,home_x,home_y);
            else
   sur_resize_in_given_font(wname,font_x,font_y,home_x,home_y);
*/
            }
        }
    return(1);
    }


static int op_tempdisk()
    {
    int j;

// RS FIXME path simplify and path expand

    if (g<2) return(-1);
    j=r1+r-1;
    if (muste_strcmpi(parm[1],"GET")==0)
        {
        edwrite(space,j,1);
        sprintf(sbuf,"TEMPDISK GET %s",etmpd);
        edwrite(sbuf,j,1);
        return(1);
        }
    if (muste_strcmpi(parm[1],"SET")==0)
        {
        if (g<3) return(-1);
        strcpy(etmpd,parm[2]);
        }
    return(1);
    }

static int op_paste(int mode)
// 1=PASTE command 2=ctrl+shift
    {

    char *clip;
    int i,len;
    int j,col,k;
    char *p,*q;

    clip=muste_get_clipboard();

/* RS NYI - Ei varmaankaan tarvita
    w_codes_load(2);

    for (i=0; i<len; ++i)
        (unsigned char)clip[i]=code[(unsigned char)clip[i]];
*/


    j=r1+r-1; if (mode==2) --j; // 24.4.2006
    col=c1+c-1;
    if (insert_mode==1)
        {       
        p=clip; k=1;
        while (1)
            {
            q=strchr(p,'\n');
            if (q==NULL) break;
            ++k; p=q+1;
            }
        i=insert_lines(j+1,k);
        if (i<0) return(1);
        }

    p=clip;
    len=strlen(p);
    while (1)
        {
        q=strchr(p,'\n');    
        if (q!=NULL) { len=q-p; }  // RS CHA *q=EOS; p[strlen(p)-1]=EOS;
        strncpy(sbuf,p,len); sbuf[len]=EOS; // RS CHA
        ++j; if (j>r2) break;
        edwrite(sbuf,j,col);
        if (q==NULL) break;
        p=q+1;
        }

    return(1);
    }


static int copytofile(unsigned int j1,unsigned int j2,int alku)
        {
        char x[LLENGTH], out[LNAME];
        unsigned int j;
        int k;

		k=1-alku;
        if (muste_strcmpi(parm[3+k],"TO")==0)
            {
            strcpy(x,parm[4+k]);
            subst_survo_path_in_editor(x);
            *out=EOS;
            if (strchr(x,':')==NULL) strcpy(out,edisk); // RS FIXME path
            strcat(out,x);
            }
            else strcpy(out,eout);
        output_open(out);
        for (j=j1; j<=j2; ++j) { edread(x,j); output_line(x+alku,out,0); }
        output_close(out);
        return(1);
        }


/* RS REM
char code[512];
static FILE *codes;
extern nop();

char text_copied_to_clip[];

*/

static int copy_to_clipboard()
    {
    char *clip;
    int j,len;
    char *p;
    extern int muste_selection; // RS ADD
    
/* RS REM
    HANDLE hGlob;
    char *pGlob;
    extern int m_move_ind2;
*/

   m_move_ind2=0; // 22.3.2004

   pyyhi_alarivi();
   LOCATE(r3+2,1); PR_EBLK;

   if (move_ind!=3)
       {
       sur_print("No text block defined by BLOCK (alt-F4) key!");
       sur_wait(3000L,nop,1);
       move_clear();
       return(1);
       }



    strcpy(survoblo,etmpd); strcat(survoblo,"SURVO.BLO");

// RS ADD
    if (move_r1<1) move_r1=1;
    if (move_r1>r2) move_r1=r2;
    if (move_r2<1) move_r2=1;
    if (move_r2>r2) move_r2=r2;
    if (mc1<0) mc1=0;
    if (mc1>c2) mc1=r2;
    if (mc2<0) mc2=0;
    if (mc2>c2) mc2=r2;
    
    
    mr1=move_r1; mr2=move_r2; c_vasen=1; // koe 8.1.2002
    save_words(survoblo); // 8.1.2002

    len=(mc2-mc1+3)*(move_r2-move_r1+1)+1;
    clip=muste_malloc(len);
    *clip=EOS;
    for (j=move_r1; j<=move_r2; ++j)
        {
        edread(sbuf,j);
        sbuf[mc2+1]=EOS;
        p=sbuf+mc2; while (p>sbuf && *p==' ') *p--=EOS;
        strcat(clip,sbuf+mc1);
        if (j<move_r2) strcat(clip,"\n"); // RS CHA FIXME char cr_lf[]={ '\15', '\12', '\0' };
        }

    muste_copy_to_clipboard(clip);  
    
    muste_free(clip);
    sur_print("The text block is now copied to the clipboard!");
    sur_wait(1000L,nop,1);
    if (!muste_selection) move_clear(); // RS CHA
    disp();
    return(1);
    }



static int op_copy()
        {
        int j,j0,j1,j2,j3;
        int h;
        char x[LLENGTH];
        int alku,k;

        if (g==1) { copy_to_clipboard(); return(1); } // 7.1.2002

        if (g<3)
            {
            if (etu) { strcpy(tut_info,"-@"); return(1); } // 7.1.2002
            sur_print("\nUsage: COPY L1,L2,L");
            WAIT; return(-1);
            }
        if (*parm[1]=='*') alku=0; else alku=1;
        k=1-alku;
        j1=edline2(parm[1+k],1,1);  if (j1==0) return(-1);
        j2=edline2(parm[2+k],j1,1); if (j2==0) return(-1);
        if (g==3+k) { copytofile(j1,j2,alku); return(1); }
        if (muste_strcmpi(parm[3+k],"TO")==0) { copytofile(j1,j2,alku); return(1); }
        j3=edline2(parm[3+k],1,1);  if (j3==0) return(-1);
        if (j3>j1)
            {
            for (j=j2; j>=j1 ; --j)
                {
                edread(x,j);
                j0=j+j3-j1;
                if (j0<=ed2)
                    {
                    edwrite(x,j0,0);
                    if (zs[j0]>0) { z[(zs[j0]-1)*ed1]='\0'; zs[j0]=0; }
                    if (zs[j]>0)
                        {
                        edread(x,zs[j]);
                        h=creatshad(j0); if (h<0) return(-1);
                        edwrite(x,zs[j0],0);
                        }
                    }
                }
            }
        else    /* j3<=j1 */
            {
            for (j=j1; j<=j2; ++j)
                {
                edread(x,j);
                j0=j+j3-j1;
                if (j0>0)
                    {
                    edwrite(x,j0,0);
                    if (zs[j0]>0) { z[(zs[j0]-1)*ed1]='\0'; zs[j0]=0; }
                    if (zs[j]>0)
                        {
                        edread(x,zs[j]);
                        h=creatshad(j0); if (h<0) return(-1);
                        edwrite(x,zs[j0],0);
                        }
                    }
                }
            }
        return(1);
        }

/*        
static int op_markblock() // RS
		{
        int j,j0,j1,j2,j3,s1,s2,s3;
        int h;
        char x[LLENGTH];
        int alku,k;

        if (g<4)
            {
            sur_print("\nUsage: MARKBLO L1,C1,L2,C2");
            WAIT; return(-1);
            }

        j1=edline2(parm[1],1,1); if (j1==0) return(-1);
        s1=atoi(parm[2]); if (s1<0 || s1>c2) return(-1);
        j2=edline2(parm[3],1,1); if (j2==0) return(-1);
        s2=atoi(parm[4]); if (s2<0 || s2>c2) return(-1); 
	
	
		}
*/

static int op_copyblock() // RS
        {
        int j,s,j0,j1,j2,s1,s2;
        int h;
        char x[LLENGTH];
        int alku,k;

        if (g<6)
            {
            sur_print("\nUsage: COPYBLO L1,C1,L2,C2,L,C");
            WAIT; return(-1);
            }

        j1=edline2(parm[1],1,1); if (j1==0) return(-1);
        s1=atoi(parm[2]); if (s1<0 || s1>c2) return(-1);
        j2=edline2(parm[3],1,1); if (j2==0) return(-1);
        s2=atoi(parm[4]); if (s2<0 || s2>c2) return(-1); 
        j=edline2(parm[5],1,1); if (j==0) return(-1);
        s=atoi(parm[6]); if (s<0 || s>c2) return(-1);       

		if (j1<j2)  { mr1=j1; mr2=j2; }
		else { mr1=j2; mr2=j1; }
		
		if (s1<s2) { mc1=s1; mc2=s2; }
		else { mc1=s2; mc2=s1; }
		
		c_vasen=0;

        strcpy(survoblo,etmpd); strcat(survoblo,"COPY.BLO");
        save_words(survoblo);

        mr=j; mc=s;
        j1=insert_mode;
        insert_mode=0;
        j2=spec_find("INSERT",sbuf,LLENGTH-1);
        if (j2>=0) { insert_mode=atoi(sbuf); if (insert_mode==1 && mr>1) mr--; }
        block_from_store();
        insert_mode=j1;
        return(1);
        }        
        
static int op_status()
    {
    int j;

    if (g<2) return(-1);
    j=r1+r-1;
    if (muste_strcmpi(parm[1],"CAPSLOCK")==0)
        {
        edwrite(space,j,1);
        sprintf(sbuf,"STATUS CAPSLOCK %d",s_caps_on());
        edwrite(sbuf,j,1);
        return(1);
        }
    return(1);
    }
    
static int op_net() // 13.2.2006
    {
muste_fixme("\nFIXME: NET not implemented yet!");    
/* RS FIXME NYI    
    int i,j;
    extern char esurd[];

    j=r1+r;

    if (g==2)
        {
        sprintf(sbuf,"\n%s| netd=%d ext=%d|",parm[1],
                            netd(parm[1]),ext(parm[1]));
        sur_print(sbuf); WAIT; return(1);
        }
    write_net_line("Survo system paths:",j++);
    write_net_line("Survo application path:",j++);
    if (*esurd==EOS) // 6.9.2006
        {
        write_net_line("-",j++);
        write_net_line("-",j++);
        }
    else
        {
        write_net_line(esurd,j++); strcpy(sbuf,esurd);
        sur_get_long_name(sbuf);
        write_net_line(sbuf,j++);
        }
    write_net_line("Survo data path:",j++);
    write_net_line(edisk,j++); strcpy(sbuf,edisk);
    sur_get_long_name(sbuf);
    write_net_line(sbuf,j++);

    write_net_line("Survo path for temporary files:",j++);
    write_net_line(etmpd,j++); strcpy(sbuf,etmpd);
    sur_get_long_name(sbuf);
    write_net_line(sbuf,j++);

//  strcpy(sbuf,etmpd);
//  sur_get_long_name(sbuf);
//  Rprintf("\nsbuf=%s|",sbuf); getck();
*/
    return(1);
    }    

extern int muste_evalsource_output();  

int ractivate(int select) // RS NEW
        {
        int i,jj,cumpit;
        char copy[LLENGTH];
        char *p;
        char *mp,*x,*pxx, xx[2*LLENGTH]; // RS      
        extern int muste_selection;
        extern int op_runr();
// RS REM        char pref[32];


		if ((muste_selection || move_ind) && select) // RS ADD move_ind 20.9.2012
			{
			i=g; g=99;
			op_runr();
			g=i;
			return(1);
			}

        *info_2=EOS;
        soft_act2=0;

        scroll_line=r+2; if (scroll_line>r3) scroll_line=r3;

        edread(actline,r1+r-1);

// RS ADD
		x=actline;
/*		
 		p=strchr(x+1,STAMP); 
        if (p==NULL) p=x;  
        q2=strstr(p,"##");
        if (q2!=NULL) 
          {
          if (q2[2]!=PREFIX && q2[2]!='.' && q2[2]!=')' && q2[2]!=',') p=q2+1; // RS ADD           
          }
        strcpy(xx,p); strcpy(x,xx);
*/
		strcpy(xx,x);
        jj=r1+r-1; *x=EOS; pxx=xx+1;
		i=0; while (pxx[i]==' ' && i<strlen(pxx)) i++; pxx=pxx+i;        
		cumpit=strlen(xx);
        while (1)
            {
            if (*(pxx)=='R' && *(pxx+1)=='>') pxx=pxx+2;
            mp=strchr(pxx,'#'); // RS ADD
            p=strstr(pxx," & ");
            if (p==NULL) { strcat(x,pxx); break; }
            if (mp!=NULL) // RS ADD
              {
              if (mp>p)
                {
                mp=strstr(mp," & ");
                if (mp!=NULL) p=mp;
                }
              }
//            *p=EOS;  			  
  			 if (*(p+3)!=' ' && *(p+3)!='/' && *(p+3)!=EOS) // RS ADD
				{
				sur_print("\nSyntax error! Check the use of '&' (put behind '#' if needed).");
				WAIT; return(-1);
				}    
            *(p+1)=EOS; 
            *p='\n';
            strcat(x,pxx);
            ++jj; edread(xx,jj); pxx=xx+1;
		    i=0; while (pxx[i]==' ' && i<strlen(pxx)) i++; pxx=pxx+i;
		    cumpit+=strlen(pxx);
		    if (cumpit>LLENGTH)
		    	{
		    	sur_print("\nActline too long!");
				WAIT; return(-1);
		    	}
            }
// x is pointer to actline above !  	

        p=actline; 
        i=strlen(actline)-1;       
        while (p[i]==' ' && i>0) i--;
        p[++i]=EOS;

		i=0;
        while (p[i]==' ' && i<strlen(p)) i++;
                
        strcpy(copy,p+i);
        
/*        
        g=split(p+1,parm,MAXPARM);

        if (g==0) { erun=0; return(0); }

        for (i=0; i<g; ++i) if (parm[i][0]=='/' && parm[i][1]==EOS) break;
        g=i;
        if (g) copy[parm[g-1]-p+strlen(parm[g-1])]=EOS;
        cursor(r,1);
        PR_EINV;
        if (*actline!='?') { sprintf(sbuf,"%.*s",c3,copy+1); sur_print(sbuf); }

        strncpy(op_tila,parm[0],OPLEN); op_tila[OPLEN-1]=EOS;
        op=op_tila;

        strcpy (OO,op);
        muste_strupr(OO);
*/         

/*       
        sprintf(sbuf,"%.*s",c2,copy+1);
//        snprintf(sbuf,c2,"%s",copy+1);        
        
        if (muste_strnicmp(sbuf,"R>",2)==0)
             {
             mp=strchr(copy+1,'>');
             if (mp==NULL) mp=copy+1;
             sprintf(sbuf,"%.*s\n\n",c2,mp+1);
//             snprintf(sbuf,c2,"%s\n\n",mp+1);
             }
*/
//         muste_iconv(copy,"","CP850");
         muste_copytofile(copy,muste_clipfile); // "MUSTE.CLP");
         muste_evalsource_output(muste_clipfile,muste_rout); // "MUSTE.CLP");
// RS ALT         muste_copy_to_clipboard(sbuf);
// RS ALT         muste_evalclipboard();
         
         
         return(1);
        }
        
static int op_output()
        {
        char x[LLENGTH];
        int j;
        if (g>1)
            {
            strcpy(x,parm[1]);
            subst_survo_path_in_editor(x);
            if (*x=='-') *eout=EOS;
            else if (*x=='.' || strchr(x,'\\')!=NULL || strchr(x,'/')!=NULL 
            || strchr(x,'<')!=NULL || strchr(x,'~')!=NULL ||
                     strchr(x,':')!=NULL) // RS ADD FIXME path
                strcpy(eout,x);
            else { strcpy(eout,edisk); strcat(eout,x); }
            return(1);
            }
        j=r1+r-1; edwrite(space,j,1);
        strcpy(sbuf,eout); unsubst_survo_path_in_editor(sbuf); // 24.11.2001
        strcpy(x,"OUTPUT "); strcat(x,sbuf);
        edwrite(x,j,1);

        return(1);
        }

static int op_rout()
        {
        char x[LLENGTH];
        int j;
        if (g>1)
            {
            strcpy(x,parm[1]);
            subst_survo_path_in_editor(x);
            if (*x=='-') *muste_rout=EOS;
            else if (*x=='.' || strchr(x,'\\')!=NULL || strchr(x,'/')!=NULL 
            || strchr(x,'<')!=NULL || strchr(x,'~')!=NULL ||
                     strchr(x,':')!=NULL) // RS ADD FIXME path
                strcpy(muste_rout,x);
            else { strcpy(muste_rout,edisk); strcat(muste_rout,x); }
            return(1);
            }
        j=r1+r-1; edwrite(space,j,1);
        strcpy(sbuf,muste_rout); unsubst_survo_path_in_editor(sbuf); // 24.11.2001
        strcpy(x,"ROUT "); strcat(x,sbuf);
        edwrite(x,j,1);

        return(1);
        }
        
static int muste_editor_init();        
static int op_setup()
        {
        int i;
        char x[LLENGTH];

        if (g<2) parm[1]=orig_setup;
// RS REM        p_survo_id=NULL;
        strcpy(current_setup,parm[1]);
        
        tut_info=s_tut_info; // RS ADD
        survo_path=s_survo_path; // RS ADD
        strcpy(x,tut_info);
        
        i=muste_editor_init(parm[1],0); // RS CHA init -> muste_editor_init
        strcpy(tut_info,x);
        g=1; op_resize(); // 5.9.2001
        
        return(i);
        }     
        
int muste_reset_pointers()
	{
    tut_info=s_tut_info;
    survo_path=s_survo_path;
    return(1);
	}
        
        
static int op_lowline() // 25.6.2006
    {
// RS REM    extern char *prompt_line;
// RS REM    extern char prompt_space[];
    int j;

    if (g==1) { prompt_line=NULL; return(1); }
    j=edline2(parm[1],1,1); if (j==0) return(1);
    edread(sbuf,j);
    for (j=0; j<8; ++j) prompt_space[j]=' '; prompt_space[j]=EOS;
    strncat(prompt_space,sbuf+1,c3);
    prompt_line=prompt_space;
    return(1);
    }

static int op_filetime() // 30.7.2008
    {
    int i,j;
    char x[LLENGTH];
    char pvm[LNAME],klo[LNAME];

    strcpy(x,parm[1]);
    i=sur_get_file_time(x,&pvm,&klo);
    if (i==1)
        {
        j=r1+r;
        edwrite(space,j,1);
        sprintf(sbuf,"%s %s",pvm,klo);
        edwrite(sbuf,j,1);
        }
    return(1);
    }

/*  textcols.c 16.3.2001/SM (16.3.2001)

TEXTCOLS L1,L2,<width>,<#_of_lines>,<#_of_columns>,<text>
         1  2  3       4            5              6
 */
static int op_textcols()
    {
    int jj,jj1,jj2;
    int kk1,k;
    int ncol;
    int nlin;
    int lev;
// RS REM    int j,j2;
    int col;
    char text[LLENGTH];

    if (g<6)
        {
        sur_print("\nUsage: TEXTCOLS L1,L2,<width>,<#_of_lines>,<#_of_columns>,<text>");
        WAIT;
        return(1);
        }

    jj1=edline2(parm[1],1,1); if (!jj1) return(1);
    jj2=edline2(parm[2],jj1,1); if (!jj2) return(1);
    lev=atoi(parm[3]);
    nlin=atoi(parm[4]);
    ncol=atoi(parm[5]);
    *text=EOS;
    if (g>6) strcpy(text,parm[6]);

    move_words=insert_mode=0;

    jj=jj1; col=0; kk1=jj1; k=0;
    while (1)
        {
        mr1=kk1; mr2=kk1+nlin-1; mc1=1; mc2=lev;
        mr=jj; mc=col*lev+1;
        sur_move(); // RS CHA move -> sur_move
        if (k) { mr=-1; block_erase(); }
        k=1;
        kk1+=nlin;
        if (kk1>jj2) break;
        ++col;
        if (col<ncol) continue;
        col=0;
        jj+=nlin+1;
        if (*text) edwrite(text,jj-1,0);
        }

    return(1);
    }

static char vmerkit[]=" .:;,()[]{}/?%!\"\\$£=+-*'<>|~_";
static int vetu;  
static int ketu;
static int no_wait=0; // RS some other place? FIXME

int sur_set_message(char *str,int num)
	{
	muste_fixme("\nFIXME:sur_set_message STUB"); // RS FIXME
	return(0);
	}

int sur_get_message(char *str,int num)
	{
	muste_fixme("\nFIXME:sur_get_message STUB"); // RS FIXME
	return(0);
	}

static int vmerkki(char ch)
        {
        if (strchr(vmerkit,ch)!=NULL) return(1);
        return(0);
        }

static int nykyinen_sana(char *x,int pos,char *hakusana)
        {
        char *p,*q;

        p=x+pos;
        if (vmerkki(*p))
            { while (p>x && vmerkki(*p))  --p; if (p==x) return(-1); }
        q=p;
        while (!vmerkki(*q)) ++q; *q=EOS;
        q=p;
        while (q>x && !vmerkki(*q)) --q;
        strcpy(hakusana,q+1);
        return(1);
        }

int muste_help_running=FALSE; // RS ADD

static int help2()
        {
        int i;
        char *p;
        int rr1,rr;
        char x[LLENGTH];
        char v_info[LLENGTH];
/* RS REM
        extern char ops[];
        extern int no_wait;
        extern int help_window;
*/        
        extern char *p_soft_key_text;
        extern void muste_help();

help_window=0; help_window_open=0; // RS FIXME TEMP
muste_help_running=TRUE;
		
        if (help_window)
            {
            if (help_window_open)
                {
                strcpy(v_info,info);
                p=strchr(info,'>'); if (p!=NULL) *p=EOS;
                if (*info==EOS)
                    {
                    strcpy(info,parm[0]);
                    p=strchr(info,'?'); if (p!=NULL) *p=EOS;
                    }
                if (strcmp(info,"???")==0)
                    {
                    edread(x,r1+r-1);
                    nykyinen_sana(x,c1+c-1,info);
                    }

                sur_set_message(muste_strupr(info),1);
// Tarkistetaan ettei help-ikkunaa ole suljettu omavaltaisesti!
                sur_sleep(300);
                i=sur_get_message(sbuf,1);

                if (!i) return(1);
                else
                    strcpy(info,v_info);
                }
            help_window_open=1;
            no_wait=1;
            strcpy(info_2,"NEW_WINDOW");
            }            
        rr1=r1; rr=r; r1=r1+r-1; r=1;
        if (!help_window)
            {
            soft_vis=0;
            p_soft_key_text=NULL;
            if (r3<23)
                { g=0; op_resize(); }
            disp();
            }
// RS REM        op=ops; strcpy(op,"Q");
// RS CHA        childp("&");

        muste_dump();
        muste_no_selection=TRUE;        
        muste_help(arguc,arguv);
        muste_no_selection=FALSE;
        muste_restore_dump();
        
        r1=rr1; r=rr;
muste_help_running=FALSE;        
        return(1);
        }

static int help(char *helpword)
        {
// RS REM        extern int m_move_ind,m_move_ind2;

        m_move_ind=m_move_ind2=0; move_clear();
        if (*help_sana==EOS || *helpword=='?') strcpy(info,helpword);
        else if (*help_sana=='>') strcpy(info,"DOS");
        else strcpy(info,help_sana);
        strcat(info,">"); 
        if (qpath[0]!='"') strcat(info,"\""); // RS ADD quotes if missing
        strcat(info,qpath);
        if (qpath[0]!='"') strcat(info,"\""); // RS ADD

help_window=0; help_window_open=0; // RS TEMP


muste_help_running=TRUE; // RS ADD
        help2();
muste_help_running=FALSE; // RS ADD      
        return(1);
        }



static int op_files()
    {
    char path[LNAME];
    char flist[10*LLENGTH];
    char *p,*q;
    int j=r1+r;

    sprintf(path,"%s/*.*",parm[1]); // RS CHA \\ -> /

    sur_find_files(path,flist);
    p=flist;
    while (1)
        {
        q=strchr(p,'\n');
        if (q==NULL) break;
        *q=EOS;
        edwrite(space,j,1);
        edwrite(p,j++,1);
        p=q+1;
        if (j>r2) break;
        }
    return(1);
    }

static int os_error(char *s)
    {
    sprintf(sbuf,"\nError in %s!",s);
    sur_print(sbuf); WAIT; return(1);
    }

static int op_dos()
        {
        char nullpath[]=".";
        int i,k;
        char x[2*LLENGTH]; // 27.5.2005
        char *p,*pxx,*q2;
        int j,jj;
        char xx[2*LLENGTH]; // 27.5.2005
        char parm1[LLENGTH];
        char parm2[LLENGTH];
// RS REM        int cc,cc1;
// RS REM        char optila[32];
// RS REM        extern char *op;
// RS REM        char os_font[LNAME];
        int set_win;
// RS REM        int os_window_message;
// RS REM        char sana[LNAME];
// RS REM        extern char os_ver[];


        j=r1+r-1;
        sur_print("\n");
        edread(x,j);
/* 21.9.1997 */
// RS CHA        p=strchr(x+1,'_'); if (p==NULL) p=x; strcpy(xx,p); strcpy(x,xx);

 		p=strchr(x+1,STAMP); 
        if (p==NULL) p=x;  
        q2=strstr(p,"##");
        if (q2!=NULL) 
          {
          if (q2[2]!=PREFIX && q2[2]!='.' && q2[2]!=')' && q2[2]!=',') p=q2+1; // RS ADD           
          }
        strcpy(xx,p); strcpy(x,xx);


        jj=j; strcpy(xx,x); *x=EOS; pxx=xx; // 27.5.2005
        while (1)
            {
            p=strstr(pxx," & ");
            if (p==NULL) { strcat(x,pxx); break; }
            *p=EOS; strcat(x,pxx);
            ++jj; edread(xx,jj); pxx=xx+1;
            }

        set_win=0;
        if (strncmp(x+1,">>",2)==0) // 17.5.2006
            {
            set_win=1;
            xx[0]=x[0]; xx[1]=EOS; strcat(xx,x+2);
            strcpy(x,xx);
            }
        strcpy(xx,x);

//Rprintf("\nx: %s",x);

//        subst_survo_path_in_editor(x);
        i=splitq(x+1,parm,3);

		if (i>1) { strcpy(parm1,parm[1]); parm[1]=parm1; } // RS ADD
		if (i>2) { strcpy(parm2,parm[2]); parm[2]=parm2; } // RS ADD	

        if (strcmp(parm[0],">START")==0 ||
            strcmp(parm[0],">SET_WIN")==0 ||
            strcmp(parm[0],">SET_EWIN")==0)
            set_win=1;

// 23.4.2004
/* RS NYI
        if (i==3 && strcmp(parm[2],"/NEW_SURVO")==0)
            {
            set_win=1;
            sprintf(x,"%sS.EXE",survo_path);
            spawnl(P_NOWAIT,x,x,parm[1],NULL);
            return(1);
            }
*/

        if (i>1 && (strcmp(parm[0],">COPY")==0 || strcmp(parm[0],">copy")==0)) // RS REM etu &&
            {
            if (i==2) parm[2]=nullpath;
            i=strlen(parm[2])-1;
            if ( strchr(parm[1],'*')==NULL && strchr(parm[1],'+')==NULL
// RS REM    && !sur_is_directory(parm[2])  && *(parm[2]+i)!=':' && *(parm[2]+i)!='\\' && *(parm[2]+i)!='/'
				)
                {
                if (etu) i=sur_copy_file(parm[1],parm[2]);
                else
                	{
                	muste_expand_path(parm[1]);
					muste_expand_path(parm[2]);
    				sprintf(sbuf,"file.copy(\"%s\",\"%s\",overwrite=TRUE) # %s",parm[1],parm[2],parm[0]); 
/*					
					if (strchr(parm[1],' ')!=NULL)
					  {
					  if (strchr(parm[2],' ')!=NULL) 
					    sprintf(sbuf,"file.copy(\"'%s'\",\"'%s'\",overwrite=TRUE) # %s",parm[1],parm[2],parm[0]); 
    				  else sprintf(sbuf,"file.copy(\"'%s'\",\"%s\",overwrite=TRUE) # %s",parm[1],parm[2],parm[0]); 
    				  }
    				else
    				  {
    				  if (strchr(parm[2],' ')!=NULL)
    				    sprintf(sbuf,"file.copy(\"%s\",\"'%s'\",overwrite=TRUE) # %s",parm[1],parm[2],parm[0]); 
    				  else sprintf(sbuf,"file.copy(\"%s\",\"%s\",overwrite=TRUE) # %s",parm[1],parm[2],parm[0]); 
					  }
*/					  
                	muste_copytofile(sbuf,muste_command);
    				i=muste_evalsource(muste_command);
                	}
                if (i==0) os_error(">COPY");
                return(1);
                }
            }

        if (i>1 && ((strcmp(parm[0],">DEL")==0 || strcmp(parm[0],">del")==0)
        		|| (strcmp(parm[0],">RD")==0 || strcmp(parm[0],">rd")==0))
        	) // RS REM etu &&
            {
            muste_expand_path(parm[1]);            
            if (etu) i=sur_delete(parm[1]);
            else
            		{
//                	if (strchr(parm[1],' ')!=NULL && strchr(parm[1],'/')!=NULL)
//                	  sprintf(sbuf,"unlink(\"'%s'\") # %s",parm[1],parm[0]);
//    				else 
    				sprintf(sbuf,"unlink(\"%s\") # %s",parm[1],parm[0]); 
                	muste_copytofile(sbuf,muste_command);
    				i=muste_evalsource(muste_command);
                	}
            return(1);
            }
  
        if (i>1 && (strcmp(parm[0],">MD")==0 || strcmp(parm[0],">md")==0))
            {
            muste_expand_path(parm[1]);            
            if (etu) i=sur_make_dir(parm[1]);
            else
            		{
//                	if (strchr(parm[1],' ')!=NULL && strchr(parm[1],'/')!=NULL)
//                	  sprintf(sbuf,"dir.create(\"'%s'\") # %s",parm[1],parm[0]);
//                	else 
                	sprintf(sbuf,"dir.create(\"%s\") # %s",parm[1],parm[0]);
                	muste_copytofile(sbuf,muste_command);
    				i=muste_evalsource(muste_command);
                	}
            return(1);
            }     

        if (*parm[0]=='>') i=parm[0]-x;
        else { if (g<2) return(-1); i=parm[1]-x-1; }
        
        strcpy(x,xx);
//        subst_survo_path_in_editor(x);
        muste_expand_path(x);

        if (etu>0) tut_sulje();
        p=x+strlen(x)-1; while (*p==' ') { *p=EOS; --p; }
        strcpy(sbuf,x+i+1);
        
		if (g<2) // if only one parameter, remove quotes
		  {
		  for (j=0,k=0; j<strlen(x+i+1); j++)
		  	{
		  	if (*(x+i+1+j) != '"')
		  	  {
		  	  *(sbuf+k) = *(x+i+1+j);
		  	  k++;
		  	  }
		  	}
		  }
        
        
 /* RS REM       
        for (i=0; i<strlen(sbuf); ++i)
            if (sbuf[i]==' ') sbuf[i]=(char)254;
*/



        strcpy(x,survo_path); strcat(x,"OS_COM.EXE");

        soft_disp(0);

// Rprintf("\nx: %s\nsbuf: %s\nxx:%s",x,sbuf,xx);
        if (set_win) i=muste_system(sbuf,FALSE);
        else i=muste_system(sbuf,TRUE);

/* RS CHA
        strcpy(xx,"0");
        if (erun==0 && etu==0 || *tut_info=='\17') strcpy(xx,"1");

        i=hae_apu("os_font",os_font);
        if (!i) i=hae_apu("edit_font",os_font);
        if (!i) strcpy(os_font,"_");

        if (set_win)
            {
            spawnl(P_NOWAIT,x,x,sbuf,xx,survo_path,os_font,sur_session,NULL);
                                                        // 17.5.2006
            }
        else
            {
            os_window_message=1;
            i=hae_apu("os_win_msg",sana);
            if (i) os_window_message=atoi(sana);

            if (erun==0 && os_window_message)
              {
              sur_locate(1,1); PR_EIN2;
              sur_print("\n********************************************************");
              sur_print("\n* Close the Survo OS command window by pressing ENTER! *");
              sur_print("\n********************************************************");
              }
            spawnl(P_WAIT,x,x,sbuf,xx,survo_path,os_font,sur_session,NULL);
            }
   if (strcmp(os_ver,"NT")==0) // 25.5.2002 XP:n vuoksi!
    {
    sur_remove_window("Survo OS command");
    sur_remove_window("Survo OS command ( Exit by pressing ENTER! )");
    }
*/
        set_console_title();

        *op_sana=EOS;
        if (etu>0) tut_avaa();
        soft_disp(1);
        return(1);
        }

int make_fence_stop_list(char *list) // 30.4.2010
    {
    int i;
    char *p;
    char x[LLENGTH];
    char *s[MAX_FENCE_STOP];

    strcpy(x,list);
// Rprintf("\nlist: %s",list);
    n_fence_stop=split(x,s,MAX_FENCE_STOP); // 4.5.2010
    for (i=0; i<n_fence_stop; ++i)
        {
        p=strchr(s[i],'_');
        if (p==NULL) *fence_stop_list[i][1]=EOS;
        else { *p=EOS; strcpy(fence_stop_list[i][1],p+1); }
        strcpy(fence_stop_list[i][0],s[i]);
        }
    return(1);
    }

int modify_command_and_set_fence_line() // 26.4.2010
    {
    char x[LLENGTH];
    int i,j;
// RS REM    extern char stripe2[];

    if (c1!=1 || c!=1) return(1);
    j=r1+r-1;
    edread(x,j);
    if (x[1]==' ' || x[1]=='#') return(1);
    i=insert();
    if (i<0) return(1);
    i=insertl();
    if (i<0)
        {
        delete();
        return(1);
        }
    z[(j-1)*ed1+1]='#';
    i=j*ed1;
    for (j=1; j<ed1; ++j) z[i+j]='#';
    if (r<r3) --r;
    disp();
    return(1);
    }


int fence_restore(int varatalletus,int rj)
    {
    extern int mc1,mc2,mr1,mr2,c_vasen;
    extern char survoblo[];
    extern int mr,mc;

    if (varatalletus)
        {
        strcpy(survoblo,etmpd); strcat(survoblo,"SURVO1.BLO");
        mr=r1+r; mc=0;
        block_from_store();
        }
    strcpy(survoblo,etmpd); strcat(survoblo,"SURVO2.BLO");
    mr=rj; mc=0;
    block_from_store();
    return(1);
    }
    
int fence_activate()
    {
    int j,rj;
    char x[LLENGTH];
    int varatalletus;
    extern int mc1,mc2,mr1,mr2,c_vasen;
    extern char survoblo[];
    extern int mr,mc;

    j=r1+r;
    while (j<r2)
        {
        edread(x,j);
        if (strncmp(x,"*##########",11)==0) break;
        if (fence_warning && strncmp(x,"*#",2)==0)
            {
            PR_EINV;
            sprintf(sbuf,"\nFence line obviously missing above line %d!",
                           j); sur_print(sbuf); WAIT; return(1);
            }
        ++j;
        }
    if (j==r2)
        {
        PR_EINV;
        sur_print("\nFence line *########## missing!"); WAIT; return(1);
        }
    rj=j; // risurivi

// varatalletus:
    mr1=r1+r;
    mr2=j-1;
    if (mr1==mr2+1) varatalletus=0;
    else
        {
        varatalletus=1;
        mc1=0; mc2=c2; c_vasen=0;
        strcpy(survoblo,etmpd); strcat(survoblo,"SURVO1.BLO");
        save_words(survoblo);
        }

    mr1=rj;
    j=r2;
    while (1)
        {
        edread(x,j);
        if (*x=='*' && empty(x+1,c2)) { --j; continue; }
        break;
        }
    mr2=j;
// Rprintf("\nmr1=%d mr2=%d",mr1,mr2); WAIT;
    mc1=0; mc2=c2; c_vasen=0;
    strcpy(survoblo,etmpd); strcat(survoblo,"SURVO2.BLO");
    save_words(survoblo);

    ++r;
    op_scratch();
    --r;

    j=activate();
// Rprintf("\nj=%d",j); getck();
    if (j==1)
        {
        j=r2;
        while (1)
            {
            edread(x,j);
            if (*x=='*' && empty(x+1,c2)) { --j; continue; }
            break;
            }
        mr=j+1; mc=0;
        j=block_from_store();
        if (j+mr>=r2)
            {
            ++r;
            op_scratch();
            --r;
            fence_restore(varatalletus,rj);
            PR_EINV;
            sur_print("\nNot enough free lines in the edit field for new results!");
            WAIT;
            }
        }
    else // j=-1;
        {
        fence_restore(varatalletus,rj);
/*****************************
        if (varatalletus)
            {
            strcpy(survoblo,etmpd); strcat(survoblo,"SURVO1.BLO");
            mr=r1+r; mc=0;
            block_from_store();
            }
        strcpy(survoblo,etmpd); strcat(survoblo,"SURVO2.BLO");
        mr=rj; mc=0;
        block_from_store();
**************************************/
        }
    disp();

    return(1);
    }



static int open_appl()
    {
    int i,ok,len,sar;
    char nimi[LNAME];
    char x[LLENGTH];
    char *p,*q;
    char *command;
    char sbuf2[LLENGTH],open_copy[LLENGTH];
    char *paate[3]={ ".SVO", ".EDT", ".MAT" };
    char *oper[3]={ "*FILE SHOW", "*SHOW", "*/MATSHOW" };
    FILE *open_sys;

    edread(sbuf2,r1+r-1);

    *sbuf2=' '; // 28.3.2003
    sar=c1+c-1;
    p=sbuf2+sar;
    if (*p==' ' && sar<2) return(-1);  
    if (*p==' ' && *(p-1)==' ') return(-1); // RS ADD
    if (*p==' ') p--; // RS ADD
    q=p;

//  while (strchr(" ([{,",*q)==NULL) --q;
//  while (strchr(" )]},",*p)==NULL && *p!=EOS) ++p;

    while (*q!=' ') --q; ++q; // 3.8.2005
    p=q; while (*p!=' ' && *p!=EOS) ++p; --p;
    if (strchr(".,;:",*p)!=NULL) --p;
    if (*q=='(' && *p==')') { ++q; --p; }
    if (*q=='[' && *p==']') { ++q; --p; }
    if (*q=='{' && *p=='}') { ++q; --p; }
    *(p+1)=EOS;
//  if (*(p-1)=='.') --p; // myîs esim. www.survo.fi.

//  *p=EOS; ++q;

    subst_survo_path_in_editor(q); // 20.4.2003
    len=strlen(q);
    ok=0;

    if (strchr(q,'.')==NULL) // 18.4.2003
        {       
        strcpy(x,q);
        if (strchr(q,':')==NULL) { strcpy(x,edisk); strcat(x,q); }
        len=strlen(x);
        for (i=0; i<3; ++i)
            {
            x[len]=EOS; strcat(x,paate[i]);
            open_sys=muste_fopen2(x,"rb"); //
            if (open_sys==NULL) continue;
            muste_fclose(open_sys);
            ok=1; break;
            }
        if (!ok) return(0);
        command=oper[i];
        q=x;
        }
    else
        {
        strcpy(nimi,survo_path); strcat(nimi,"SYS/OPEN.SYS");       
        open_sys=muste_fopen2(nimi,"rt");
        if (open_sys==NULL) return(-1);
        while (1)
            {
            p=fgets(x,100,open_sys); if (p==NULL) { muste_fclose(open_sys); return(-1); }
            i=strlen(x)-1;
            if (x[i]=='\n') x[i]=EOS;
            if (x[i]=='\r') x[i]=EOS; if (x[i-1]=='\r') x[i-1]=EOS; // RS ADD
            command=x+2;
            while (*command!=' ') ++command;
            *command=EOS;

            if (x[2]=='*')
                {
                if (x[1]=='0')
                    {
                    if (strncmp(q+len-strlen(x+3),x+3,strlen(x+3))==0)
                        { ok=1; break; }
                    }
                else
                    {
                    if (muste_strnicmp(q+len-strlen(x+3),x+3,strlen(x+3))==0)
                        { ok=1; break; }
                    }
                }
            else
                {
                if (x[1]=='0')
                    {
                    if (strncmp(q,x+2,strlen(x+2))==0) { ok=1; break; }
                    }
                else
                    {
                    if (muste_strnicmp(q,x+2,strlen(x+2))==0) { ok=1; break; }
                    }
                }
            }
        muste_fclose(open_sys);
        }

    if (ok)
        {
        ++command;
        while (*command==' ') ++command;

        edread(open_copy,r1+r-1);
        edwrite(space,r1+r-1,1);
        if (strncmp(q,"www.",4)==0) 
        	if(strchr(q,'&')!=NULL) sprintf(sbuf,"*%s 'http://%s'",command,q);
        	else sprintf(sbuf,"*%s http://%s",command,q);
        else if(strchr(q,'&')!=NULL) sprintf(sbuf,"*%s '%s'",command,q);
        else sprintf(sbuf,"*%s %s",command,q);
        edwrite(sbuf,r1+r-1,0);
        write_string(space,c3-1,' ',r+1,8);
        activate();
        edwrite(open_copy,r1+r-1,0);

        return(1);
        }
    return(0);
    }


int emptyline(int curline)
	{
	int i,j;
	char space[LLENGTH];
	
    for (i=0; i<ed1; ++i) space[i]=' ';
    i=curline; j=lastline2();   		
	while (i<=j)
    	{
        if (strncmp(space,z+(i-1)*ed1+1,(unsigned int)(ed1-1))==0) break;
        ++i;
    	}
    return(i);	
    }

static int muste_search_rline(int curline)
	{
	int i,j,l,k1,k2,r1bak,rbak;
	char rivi[LLENGTH];
	char *p;
	char *para[3];

	l=lastline2();
	
	for (i=1; i<=l; i++)
		{
        edread(rivi,i);        
        p=strchr(rivi,(char)STAMP);   // RS ADD check STAMP          
        if (p==NULL)
            {
            p=strchr(rivi,(char)PREFIX); // RS ADD check changed PREFIX
            if (p!=NULL)
              {
              if (p[1]==(char)PREFIX && p[2]!=(char)PREFIX && 
                  p[2]!=(char)'.' && p[2]!=(char)')' && p[2]!=(char)',') p++;
             // RS double PREFIX needed for activation, but no triple or other special case
              else { p=rivi; }
              }
            else p=rivi;  
            }

        j=split(p+1,para,3);
        if (j<2) continue;
 
        muste_strupr(para[0]); 
        k1=0; k2=0;
		if (strcmp(para[0],"R")==0)
			{
			r1bak=r1; rbak=r; r1=i; r=1;
			k1=edline2(para[1],1,0);
			r1=r1bak; r=rbak;
			if (k1<=0) continue;
			if (j<3) k2=emptyline(k1);
			else
				{
				r1bak=r1; rbak=r; r1=i; r=1;
				k2=edline2(para[2],1,0);
				r1=r1bak; r=rbak;				
				if (k2<=0) continue;
				}
// Rprintf("\n%d: %s %d,%d",i,para[0],k1,k2);				
			if (curline>=k1 && curline<=k2) return(1);				
			}			
    	}
    return(0);	
	}
        

int survoapu1(); // RS Declaration 
static int op_find(); // RS Declaration
static int op_insertl();
static int op_deletel();
static int op_lineins();
static int op_session();

int activate()
        {
        int i,k=0;
        char copy[LLENGTH];
        char *p;
//        char *mp; // RS
        char pref[32];
        
// RS        extern int act_sounds_on; // 14.10.2005
// RS        extern char *act_sound[];

/* RS NYI
        if (act_sounds_on==2) // 14.10.2005
            {
            sprintf(copy,"%sSND\\%s.WAV",survo_path,act_sound[0]);
            sur_play_sound(copy);
            }
*/

        *info_2=EOS;

        soft_act2=0;
// 9.2  soft_vis=1;
// 9.2  if (display_off) soft_vis=0;
        scroll_line=r+2; if (scroll_line>r3) scroll_line=r3;
        if (soft_act)
            {
            *actline='?'; strcpy(actline+1,soft_actline);
            soft_act=0; soft_act2=1;
//Rprintf("\nsoftact: %s",actline);   
            }
        else
            {
            edread(actline,r1+r-1);
            }
        p=strchr(actline,(char)STAMP);   // RS ADD check STAMP          
        if (p==NULL)
            {
            p=strchr(actline,(char)PREFIX); // RS ADD check changed PREFIX
            if (p!=NULL)
              {
              if (p[1]==(char)PREFIX && p[2]!=(char)PREFIX && 
                  p[2]!=(char)'.' && p[2]!=(char)')' && p[2]!=(char)',') p++;
             // RS double PREFIX needed for activation, but no triple or other special case
              else { p=NULL; }
              }
            }
        if (p==NULL || actline[1]=='#') // RS CHA
            {
            p=actline;
            if (actline[1]=='#' && actline[c1+c-2]!='=') // 18.4.2010
                {               // Do editorial arithmetics 21.10.2011/SM
                if (actline[2]==' ')
                    {
                    PR_EINV;
                    sur_print("\nIncomplete command, see FENCE?");
                    WAIT; return(1);
                    }

          // 30.4.2010
                strcpy(copy,p);
// Rprintf("\nstops=%d|",n_fence_stop); getck();
                g=split(p+1,parm,MAX_FENCE_STOP); if (g==1) parm[1]="";

/**************************
      sprintf(sbuf,"\nparm[0]=%s| parm[1]=%s|",parm[0],parm[1]);
      sur_print(sbuf);
      for (i=0; i<n_fence_stop; ++i)
          {
sprintf(sbuf,"\n|%s|%s|",fence_stop_list[i][0],fence_stop_list[i][1]);
   sur_print(sbuf);
          }
getck();
*********************************/

                for (i=0; i<n_fence_stop; ++i)
                    {
                    if (muste_strcmpi(parm[0],fence_stop_list[i][0])==0)
                      {
                      if (*fence_stop_list[i][1]==EOS ||
                        (*fence_stop_list[i][1]!=EOS &&
                        muste_strcmpi(parm[1],fence_stop_list[i][1])==0) )
                        {
                        sprintf(sbuf,"\nCannot activate %s %s (Remove # and activate again!)",
                                        parm[0],parm[1]);
                        PR_EINV; sur_print(sbuf); WAIT; return(1);
                        }
                      }
                    }

                if (fence_save) // 21.5.2010
                    {
                    sprintf(copy,"%s#SURVO.EDT",edisk);
                    edt_talletus(copy);
                    }

                z[(r1+r-2)*ed1+1]=' ';
                fence_activate();
                z[(r1+r-2)*ed1+1]='#';
                return(1);
                }
            }

        strcpy(copy,p);
        g=splitq(p+1,parm,MAXPARM); // RS CHA splitq

        if (g==0) { erun=0; return(0); }
// 9.12.99 if (g==1 && *parm[0]=='/' && parm[0][1]==EOS) return(0);
        for (i=0; i<g; ++i) if (parm[i][0]=='/' && parm[i][1]==EOS) break;
        g=i;
        if (g) copy[parm[g-1]-p+strlen(parm[g-1])]=EOS; /* 9.12.1999 */
        cursor(r,1);
        PR_EINV;
        if (*actline!='?') { sprintf(sbuf,"%.*s",c3,copy+1); sur_print(sbuf); } 

        strncpy(op_tila,parm[0],OPLEN); op_tila[OPLEN-1]=EOS;
        op=op_tila;
        child_call0=0; if (*actline=='\'') child_call0=1;
        if (*op=='-' && op[1]!=EOS) ++op;  /* 8.12.1998 */

        if (muste_strcmpi(op,"S")==0) strcpy(op_tila,"SHOW"); // 26.4.2006

        strcpy (OO,op);
        muste_strupr(OO);
        
        strncpy(op_sana,OO,8); op_sana[8]=EOS;
        strcpy(help_sana,op_sana);
        strcpy(pref,"_");
        goto_load_ind=0;

// if (strlen(OO)>16)
//   { Rprintf("OO=%s\n",OO); getck(); }
		if ((erun!=0 || etu!=0) && *actline=='/' && *(actline+1)=='/' && *(actline+2)=='/') return(1); // RS Null-activate for comment lines in Sucros

		if (strcmp(OO,"NOP")==0) return(1); // RS No operation activation
else    if (strcmp(OO,"GOTO")==0)    { i=op_goto(); goto_load_ind=1; return(i); }

else    if (strcmp(OO,"REDIM")==0)   { op_redim(1); return(1); }
else    if (strcmp(OO,"FONT")==0 || strcmp(OO,"WINDOW")==0)
            { op_font(); return(1); }
else    if (strcmp(OO,"TKFONT")==0)   { muste_choosefont(); return(1); }

else    if (strcmp(OO,"SAVE")==0)    { op_save();
                                       if (etu==0) sur_wait(100L,nop,1);
                                       return(1);
                                     }

else    if (strcmp(OO,"LOAD")==0)    { op_load(); goto_load_ind=1; return(1); }
else    if (strcmp(OO,"SCRATCH")==0) { op_scratch(); return(1); }
else    if (strcmp(OO,"PATHS")==0) { muste_showpaths(); return(1); }
else    if (strcmp(OO,"CD")==0 || 
            strcmp(OO,"PATH")==0 || 
            strcmp(OO,"DISK")==0)  { op_cd(); return(1); } // RS CHA   { i=op_path(); return(i); }
else    if (strcmp(OO,"MD")==0 || 
            strcmp(OO,"MKDIR")==0)  { sur_make_dir(parm[1]); return(1); } // RS NEW
else    if (strcmp(OO,"RD")==0 ||
            strcmp(OO,"RMDIR")==0) { sur_remove_dir(parm[1]); return(1); } // RS NEW 
else    if (strcmp(OO,"WIN")==0)    return(op_win());            
else    if (strcmp(OO,"RESIZE")==0)    return(op_resize());
else    if (strcmp(OO,"SET")==0)     { op_set(); return(1); }
else    if (strcmp(OO,"TIME")==0)    { i=op_time(); return(i); }
else    if (strcmp(OO,"TUTOR")==0)   { i=op_tutor(); return(i); }
else    if (strcmp(OO,"-")==0)       { i=op_jump(1); return(i); }
else    if (strcmp(OO,"INIT")==0 && g>=3 ) return(op_init()); // g 8.8.03
else    if (strcmp(OO,"WAIT")==0)    { i=op_wait(); return(i); }
else    if (strcmp(OO,"WAIT_TUT")==0)    return(op_wait_tut()); // 30.12.2000
else    if (strcmp(OO,"QPATH")==0)   { i=op_qpath(); return(i); }
else    if (strcmp(OO,"COUNT")==0)    return(op_count());
else    if (strcmp(OO,"CLEAR")==0)    return(op_clear());
else    if (strcmp(OO,"INSERT")==0 || strcmp(OO,"I")==0) { insdel(); return(1); }
else    if (strcmp(OO,"DELETE")==0 || strcmp(OO,"D")==0) { insdel(); return(1); }
else    if (strcmp(OO,"CHECK")==0)    return(op_check(1));
else    if (strcmp(OO,"CHECK0")==0)    return(op_check(0));
else    if (strcmp(OO,"NEXTFILE")==0)    return(op_nextfile()); // 13.9.2000
else    if (strcmp(OO,"SOFTKEYS")==0)    return(op_softkeys()); // 15.3.2000
else    if (strcmp(OO,"TEMPDISK")==0)    return(op_tempdisk()); // 27.4.2004
else    if (strcmp(OO,"COLX")==0)    return(op_colx());
else    if (strcmp(OO,"SYS")==0 || strcmp(OO,"SYSDEL")==0)
            { muuta_apu_tiedostoa(3); return(1); } // 14.7.2006

else    if (strcmp(OO,"SYSTEM")==0)  { return(survoapu1(0,NULL)); }  

else    if (strcmp(OO,"COPY")==0)    { op_copy(); return(1); }
else    if (strcmp(OO,"PASTE")==0)   { op_paste(1); return(1); } // 6.1.2002

else    if (strcmp(OO,"STATUS")==0)  { op_status(); return(1); }

else    if (strcmp(OO,"INSERTL")==0) { op_insertl(); return(1); } // 10.6.2006
else    if (strcmp(OO,"DELETEL")==0) { op_deletel(); return(1); } // 18.6.2006
else    if (strcmp(OO,"LINEINS")==0) { op_lineins(); return(1); } // 10.6.2006

else    if (strcmp(OO,"COPYBLO")==0) { op_copyblock(); return(1); } // 10.6.2006

else    if (strcmp(OO,"OUTPUT")==0)  { i=op_output(); return(i); }
else    if (strcmp(OO,"ROUT")==0)  { i=op_rout(); return(i); }
else    if (strcmp(OO,"SETUP")==0)   { i=op_setup(); return(i); }
else    if (strcmp(OO,"SHADOW")==0)   { i=op_shadow(); return(i); }
else    if (strcmp(OO,"SESSION")==0)    return(op_session()); // 7.4.2000
else    if (strcmp(OO,"PUTENV")==0) { putenv(parm[1]); return(1); } // 24.3.2005
else    if (strcmp(OO,"LOWLINE")==0) { op_lowline(); return(1); } // 25.6.2006
else    if (strcmp(OO,"FILETIME")==0) { op_filetime(); return(1); } // 30.7.2008
else    if (strcmp(OO,"COLOR")==0)    return(op_color()); // 30.12.2000
else    if (strcmp(OO,"TEXTCOLS")==0)    return(op_textcols()); // 17.3.2001
else    if (strcmp(OO,"FILES")==0) { op_files(); return(1); } // 9.6.2005
else    if (strcmp(OO,"NET")==0) { op_net(); return(1); } // 13.2.2006

// RS already in op_dos  else    if (strcmp(OO,">COPY")==0) { sur_copy_file(parm[1],parm[2]); return(1); } // RS NEW       
else    if (strcmp(OO,"DOS")==0 || *OO=='>')
                { 
// RS REM                if (strlen(OO)==1) return(1);
                i=op_dos(); return(i);
                }


else    if (strcmp(OO,"FIND")==0 || strcmp(OO,"REPLACE")==0 ||
            strcmp(OO,"-FIND")==0 || strcmp(OO,"-REPLACE")==0) 
            { first_word_on_line_search=0; return(op_find()); }
            
else    if (strcmp(OO,"MASK")==0 || strncmp(OO,"MASK=",5)==0)
             { file_act("MASK"); return(1); }    
             
else    if (strcmp(OO,"HELP")==0)    { i=help("HELP"); return(1); }
else    if (strcmp(OO,"F")==0)       { i=help("F"); return(1); }   
else    if (strchr(OO,'?')!=NULL && muste_strnicmp(OO,"http://",7)!=0)
            {
            strcpy(info,">"); 
            if (qpath[0]!='"') strcat(info,"\""); // RS ADD
            strcat(info,qpath);
            if (qpath[0]!='"') strcat(info,"\""); // RS ADD            
            help2(); return(1);
            }

else    if (strcmp(OO,"EXIT")==0 || strcmp(OO,"QUIT")==0 || strcmp(OO,"Q()")==0 )
            { 
              if (lopetuskysely())
                 {
                 parm[0]="/EXIT";
                 op_tutor(); 
                 return(-1);
                 }
               return(1);
            }

else    if (strcmp(OO,"FLUSH")==0)
            {
            extern int only_key_events;

            sur_flush_input();
            only_key_events=0;
            return(1);
            }

else    if ((*OO=='C' || *OO=='L') && strchr(OO,'?')==NULL &&
             strchr("+-*/%",OO[1])!=NULL &&
             strchr(OO,'=')==NULL )
                           { strcpy(op,"EDI2"); strcpy(pref,"&"); }

else    if (!soft_act2 && copy[c1+c-2]=='=')
               	{
				extern void muste_save_stack_count();
				extern void muste_restore_stack_count();

  				muste_save_stack_count();  // RS       
               	op_arit(); 
   				muste_restore_stack_count();   // RS        
               	return(1);
               	}

else    if (muste_strnicmp(OO,"R>",2)==0)
             {
			 k=ractivate(0);
             if (k<=1) disp();
/*             
             mp=strchr(copy+1,'>');
             if (mp==NULL) mp=copy+1;
             sprintf(sbuf,"%.*s",c2,mp+1);
         muste_copytofile(sbuf,muste_clipfile); // "MUSTE.CLP");
         muste_evalsource_output(muste_clipfile,muste_rout); // "MUSTE.CLP");             
//             muste_evalr(sbuf); 
*/
             return(1);
             }

else    if (*OO=='/') 
            {                   
            op_tutor(); return(1);
            }

else    if (strcmp(OO,"MATRUN")==0) op[3]=EOS; //  -> MAT

//       "lapsenlapsen" välttämiseksi 29.2.2000 
else    if (g>2 && strcmp(OO,"MAT")==0 && muste_strcmpi(parm[1],"SAVE")==0
            && muste_strcmpi(parm[2],"DATA")==0)
            { strcpy(op,"MATSDA"); strcpy(pref,"&"); }

else    if (strncmp(OO,"TCH",3)==0)
            {
            strcpy(op,"T"); strcpy(pref,"&");
            strcpy(info,"TOUCH");
        	}

/* RS REM
// else 

        if (etu>0) tut_sulje();
// RS REM        if (no_wait) { vetu=etu; etu=0; }
        if (etuu) { k=etu; etu=0; }

// RS NYI        sur_mouse_cursor_wait();
*/
        muste_dump();

/* RS REM        
        if (no_wait)
            {
            no_wait=0;
            sur_sleep(300); // HELP-ikkunan avautumiseksi (sukrot)?

//            i=spawnl(P_NOWAIT,opfile,opfile,siirtop,NULL);
            i=muste_modules();

            set_console_title();
            etu=vetu;
            if (etu>0) tut_avaa();
            return(1);
            }        
*/   

		muste_no_selection=TRUE;
        i=muste_modules();
        muste_no_selection=FALSE;

/* RS REM
        ii=0;
        while (ii<2)
            {
            p=info_s; while ((p=strchr(p,' '))!=NULL) *p='_';
            i=0;

            i=spawnl(P_WAIT,opfile,opfile,siirtop,info_s,ser_number,NULL);
            if (i>=0) break;

            if (!errno) break;
            sur_sleep(300); ++ii;
            }
*/
/* RS REM       
        if (mouse_refresh)
            {
// RS NYI            sur_refresh_window();
            mouse_refresh=0;
            }
*/
        muste_restore_dump();
/*
        set_console_title();
        soft_disp(1); // 15.2.2001
// RS NYI        sur_mouse_cursor_arrow();
        
        if (etuu) etu=k;
        if (etu>0) tut_avaa();
        if (etu==1) lue_hetki(&wait_hetki); 

// RS NYI        if (medit) medit_restore(); // 30.4.2003

        if (child_wait)
            {
            sur_wait((long)(100L*child_wait),nop,1);
            }
*/            
/* RS REM            
         if (i==-1)  
            {
            onnistui=0;
            if (!pre_ctnue && ( etu && etu!=1)) tutclose();
            if (erun) erun=0;
            else if (errno)
                {
                i=open_appl(); // 22.3.2003
                if (i>0) return(1);

                i=op_jump2(); // 27.4.2002
                if (i<0) childerr();
                }
            return(-1);
            }    
*/        
        if (i) return(1);

/* RS NYI 

else    if (strcmp(OO,"D32")==0)    return(op_d32());
// RS REM else    if (strcmp(OO,"EXT_FUNC")==0) return(op_ext_func());

else    if (strcmp(OO,"SHOW")==0)
             { op_show(); return(1); } // 13.4.2006
else    if (strcmp(OO,"CHILD")==0)   { op_child(); return(1); }


*/          



/* -> SURVOEXE.SYS
else    if (strcmp(OO,"LOADP")==0 || strcmp(OO,"SAVEP")==0)
                           { strcpy(op,"LOADP"); strcpy(pref,"C"); }
*/

/* RS NYI
else    if (strcmp(OO,"PLOT")==0 || strcmp(OO,"HISTO")==0 )
              {
              op_plot_word=op;
              check_plot_spec("plot");
              op=op_plot_word;
              i=op_plot(op,pref); if (i==1) childp(pref); return(1);
              }
else    if (strcmp(OO,"GPLOT")==0 || strcmp(OO,"HISTOG")==0 ||
            strcmp(OO,"GHISTO")==0 )
            {
            op_plot_word=op;
            check_plot_spec("gplot");
            op=op_plot_word;
            i=op_gplot(op);
            if (i==1) gplot_child("G\\");
            return(1);
            }

else    if (strcmp(OO,"FIND")==0 || strcmp(OO,"REPLACE")==0)
                           { strcpy(op,"FIND"); strcpy(pref,"C"); }

else    if (strncmp(OO,"TAB",3)==0 && strcmp(OO,"TABLE")!=0 && strchr(OO,'?')==NULL )
                                   // 1.12.2008
            { i=op_tab(op); if (i==1) childp("TAB\\"); return(1); }
*/

/* RS NYI
else    if (strcmp(OO,"LIST")==0)
             {
             i=op_list(op);
             if (i==1) childp("L\\"); return(1);
             }

*/






        if (strcmp(OO,"INIT")==0)
            {
            sprintf(info,"%s %d",ser_number,ver); // 10.8.2003
            }

/* RS NYI
        i=survoexe_sys(OO,op2);
        if (i)
            {
            op=op2;
            *pref=EOS;
            }
*/
        if (strchr(op,':')!=NULL || strchr(op,'\\')!=NULL)
            {
// RS NYI            i=childp(""); return(i);
            }
// RS NYI        i=childp(pref); return(i);

		if (*actline=='R' || *actline=='r' || *actline=='>') // RS
             {
			 k=ractivate(0);
             if (k<=1) disp();
             return(1);
			 }
			 
			 
		i=muste_search_rline(r1+r-1);	 // RS
		if (i>0) 
			{
			k=ractivate(0);
            if (k<=1) disp();
			return(1);
			}		 

		i=open_appl(); // RS ADD (from childp)
		if (i>0) return(1);

        
        sprintf(sbuf,"\nUnknown or unimplemented command %s ",OO);
        sur_print(sbuf);
        sur_print(" - Press ENTER! ");
        sur_getch();
        muste_flushscreen();
        return (-1);
 
        }


char *wordp(int k)  /* 29.5.1992 */
        {
        char *p;
        int i;

        p=tut_info;
        i=0;
        while (i<k)
            {
            if (*p==EOS) break;
            if (*p=='@') ++i;
            ++p;
            }
        return(p);
        }

int print_word(int k)
        {
        char *p;
        int vc;
// RS REM        int i;

// Rprintf("haetaan muistipaikkaa\n");
        p=wordp(k);
// Rprintf("muistipaikka haettu: %s\n",p);
/*      p=tut_info;     -29.5.1992
        i=0;
        while (i<k)
            {
            if (*p==EOS) break;
            if (*p=='@') ++i;
            ++p;
            }
*/
        vc=c;
        while (*p!=EOS && *p!='@' && vc<=c2)
            {
// Rprintf("ed1:%d,r1:%d,r:%d,c1:%d,vc:%d\n",ed1,r1,r,c1,vc);
            z[ed1*(r1+r-2)+c1+vc-1]=*p;
            if (vc<c3) { sprintf(sbuf,"%c",*p); sur_print(sbuf); }
            ++vc; ++p;
            }
        if (vc<=c3) c=vc;
        return(1);
        }

int write_wordk()
        {
        char *p;
 /*     int vc;        */
        int k; // RS REM ,i;

        k=nextch_editor();
        k=(int)(k-'0'-1);

        p=wordp(k);
/*      p=tut_info;   -29.5.1992
        i=0;
        while (i<k)
            {
            if (*p==EOS) break;
            if (*p=='@') ++i;
            ++p;
            }
*/

/*      vc=c;                             */
        numtab=vnumtab;
        while (*p!=EOS && *p!='@')
            {

            key_common((int)(unsigned char)*p);   /* 7.11.88 */
            ++p;

            /*
            z[ed1*(r1+r-2)+c1+vc-1]=*p;
            if (vc<c3) { sprintf(sbuf,"%c",*p); sur_print(sbuf); }
            ++vc; ++p;
            */
            }
  /*    if (vc<=c3) c=vc;    */
        return(1);
        }


int end_empty_lines()
    {
    int j,k;
    char x[LLENGTH];

    j=r2; k=0;
    while (j>0)
        {
        edread(x,j);
        if (!empty(x+1,c2)) break;
        --j; ++k;
        }
    return(k);
    }



/* 5.8.92 */
#define MAX_REF 8
int ref2_r1[MAX_REF],ref2_r[MAX_REF],ref2_c1[MAX_REF],ref2_c[MAX_REF];

int ref_point2(char ch)
        {
        int m;

        m=nextch_editor()-'0'-1;
        if (ch=='R')
            { ref2_r1[m]=r1; ref2_r[m]=r; ref2_c1[m]=c1; ref2_c[m]=c; return(1); }
        if (ch=='D') ref2_r1[m]=0;
        if (ref2_r1[m]==0) return(1);
        r1=ref2_r1[m]; r=ref2_r[m]; c1=ref2_c1[m]; c=ref2_c[m];
        disp();
        return(1);
        }

int sur_paint(char *x,int i1,int i2,char ch)
        {

/* Rprintf("\ni1=%d i2=%d c-i1=%d",i1,i2,c-i1); getch(); */
/*      LOCATE(r+1,i1+8);    */
        write_string(x+i1,i2-i1+1,ch,r+1,i1-c1+9);


        return(1);
        }

int enter_sana(char *x)
        {
        int i,m2;

        i=0;
        while (i<32)  /* ennen i<8  8.5.90 */
            {
            m2=nextch_editor(); if (m2==CODE_RETURN) break;
            x[i++]=(char)m2;
            }
        x[i]=EOS;
        return(1);
        }


int enter_wsana(char *x)   /* 29.5.1992 */
        {
        char *p,*q;
//RS        extern char *wordp();

        enter_sana(x);
        if (*x!='W') return(1);
        p=wordp(atoi(x+1)-1);
        q=x;
        while (*p!=EOS && *p!='@') *q++=*p++;
        *q=EOS;
        return(1);
        }


static long aloitusaika;
int sek_aika(int k)
        {
        struct timeb aika;
        char s[LLENGTH];

        ftime(&aika);
        if (k==0) { aloitusaika=aika.time; return(1); }
        sprintf(s,"%lu%.3u",aika.time-aloitusaika,aika.millitm);
        tutcat(s);
        return(1);
        }


int sur_user_name()
        {
        char s[LLENGTH];
        char *p;
        int i;

        strcpy(s,info+11);
        p=strchr(s,'>'); if (p!=NULL) *p=EOS; /* oli p */
        i=strlen(s)-1; while (s[i]==' ') { s[i]=EOS; --i; }
        strcpy(info,s);
        return(1);
        }

// F2 pressed:
void prefix()
        {
        int m,m2=0;
        int i;
        char x[LLENGTH];
        char msana[3];
        char *p,*q;
        
        m=nextch_editor();
        
        pref=' ';
        if (special)
            {
            switch(m)
                {
              case CODE_EXEC:   /* RUN-aktivointi */

                erun=1; erun_start=1;
                while (erun)
                    {
                    if (s_hit(STOP)) { erun=0; disp(); break; }

                    if (time_file_on) file_time_start(); // 12.11.2003
                    i=activate();
                    if (time_file_on) file_time_end(parm[0]);

                    *info=EOS;
                    PR_ENRM;
                    if (i<0) erun=0;
                    if (erun==0) { disp(); break; }
                    if (i==2) displine2(r1+r-1);

                    if (!goto_load_ind)
                        {
                        ++r;
                        if (r>r3)
                            {
                            r=r3; ++r1;
                            if (r1>r2-r3+1) { --r1; erun=0; }
                            else SCROLL_UP(1,r3,1);
                            }
                        }
                    if (i<=1) disp();
                    }
                break;

              case CODE_PRE:
                if (c<c3) { c=c3; if (c>c2) c=c2; break; }
                if (c1==c2-c3+1) break;
                c1+=c3; if (c1>c2-c3+1) c1=c2-c3+1;
                disp(); break;

              case CODE_CODE:
                special_code=0; code_code(); break;

              case CODE_RETURN:
                left_edge=c; break;

              case CODE_TAB:
                i=sur_getch(); if (i=='\\') strcpy(id_jatko,"  "); break;  // RS getch -> sur_getch
              case CODE_DOWN:
                if (r<r3) { r=r3; break; }
                i=lastline2();
                if (i<r3) { r1=1; r=i+1; if (i<1) r=1; disp(); break; }
                r1=i-6; r=8; if (r1>r2-r3+1) { r1=r2-r3+1; r=i-r1+2; }
                if (r>r3) r=r3;
                disp();
                break;
              case CODE_INSERT:
                insert_type=1; break;
              case CODE_DELETE:
                insert_type=0; break;
              case CODE_END:
                muste_erase(); break;
              case CODE_INSERTL:
                insertl();
                edwrite(deleted_line,r1+r-1,0);
                disp(); break;
              case CODE_MOVE:
                move_block(1); break;
              case CODE_HELP:
                help("???"); disp(); break;
              case CODE_SRCH:   /* 25.7.1998 */
                strcpy(info,"F-"); op_find();
                disp(); break;
              case CODE_ACTIV:
                *active_data=EOS;
                file_act("KEY_ACTIV"); disp();
                break;
              case CODE_SOFT_ON:
                soft_vis=0;
                soft_disp(soft_vis);
                break;
              case CODE_DISK: // RS FIXME This should work differently in Muste!
                strcpy(x,survo_path);
                i=strlen(x); // RS REM -2; x[i]=EOS;
                for (m2=0; m2<i; ++m2)
                    key_common((int)(unsigned char)x[m2]);
                break;

  /*      default: Rprintf("\nPRE: %d %d\n",special,m); getch(); */
                }
            }
        else /* not special */
            {
            
// Rprintf("etu: %d, char: %c\n",etu,m);        
            
            switch(m)
                {
/* In use:
! # £ % & / 0 = @ A a B b C c D d E e F f g h I i J j k L l M m N n o P p
Q q R r S s T t U u v W w x X y ä Ä ö ^ _ ~ > < - \
*/
              case 'T': case 't': tut_special_editor(); 
                                  break;
              case 'S': case 's': disp_shadow(); break;
              case 'P': case 'p':
                        special_code=(int)(*(z+ed1*(r1+r-2)+c1+c-1));
                        break;


              case 'C': case 'c': seek_char(); break;
              case 'W':           seek_word(); break;
              case '=':           print_word(0); break;

              case 'E': case 'e': seek_line_end(); break;

              case '@':           miau_koodit(); break;
              case '#':           write_wordk(); break;
              case 'R': case 'r': sprintf(x,"%d@%d",r1+r-1,c1+c-1);
                                  tutcat(x); break;

              case 'L':           *x=(char)nextch_editor("");

// Rprintf("case L: %c",*x);                                  
                                  
                                  if (*x=='O')
                                     {
                                     soft_vis=1; restore_display(1); 
                                     break; } // 16.5.92 
                                  else if (*x=='o' && etu) // 25.9.1998
                                      {
                                      soft_disp(0);
                                      display_off=1; soft_vis=0;
                                      break;
                                      }
                   /* 26.1.01 */  else if(*x=='V' && etu)
                                      {
                                      display_off=1;
                                      break;
                                      }
                   /* 4.9.93 */    
                                  else if (*x=='0') { soft_vis=1; restore_display(0); break; }
                   /* 5.8.92 */    
                                  else if (*x=='R' || *x=='r' || *x=='D') { ref_point2(*x); break; }

                   /* 25.9.94 */  else if (*x=='l')
                                          { line_labels_off=1; disp(); break; }
                                  else if (*x=='L')
                                          { line_labels_off=0; disp(); break; }
                                  else if (*x=='W') { wait_save=1; break; }
                                  else if (*x=='w') { wait_save=0; break; }
                   /* 17.11.96 */ else if (*x=='P') { paint_on=1; break; }
                   /* 17.11.96 */ else if (*x=='p') { paint_on=0; break; }
                   /*  1.10.97 */ else if (*x=='Q') { loudness=1000; break; }
                   /*  1.10.97 */ else if (*x=='q') { loudness=0; break; }
                   /* 18.8.00  */ else if (*x=='K')
                                         { 
                                         sprintf(x,"%d",end_empty_lines()); tutcat(x);
                                         break; }

                   /*  17.2.99 */ else if (*x=='F')
                                      {
                                      strcpy(x,"0");
                                      if (large_field) strcpy(x,"1");
                                      tutcat(x);
                                      break;
                                      }

                   /* 4.9.2000 */ else if (*x=='f')
                                      { 
                                      tutcat(active_data);
                                      break; }

                   /*   5.4.99 */ else if (*x=='C')
                                      {
                                      goto_control_char(1);
                                      break;
                                      }
                   /*   5.4.99 */ else if (*x=='c')
                                      {
                                      goto_control_char(0);
                                      break;
                                      }
                  /* 23.2.2003 */ else if (*x=='s')
                                      {
                                      sprintf(x,"%.2f",0.01*(double)ver);
                                      tutcat(x);
                                      break;
                                      }
                                  else if (*x=='S')
                                      {
                                      *x=(char)nextch_editor();
                                      switch (*x)
                                          {
                                        case 'R':
                                          s_scroll_right(); break;
                                        case 'L':
                                          s_scroll_left(); break;
                                        case 'U':
                                          s_scroll_up(); break;
                                        case 'D':
                                          s_scroll_down(); break;
                                        case 'H':
                                          c1=c1+c-1; c=1;
                                          disp(); break;
                                          }
                                      break;
                                      }
                                  else if (*x=='Z') // 5.5.2004
                                      {
                                      sprintf(x,"%d",r3);
                                      tutcat(x); break;
                                      }
                                  else if (*x=='z') // 5.5.2004
                                      {
                                      sprintf(x,"%d",c3);
                                      tutcat(x); break;
                                      }
               /* 14.10.2005 */   else if (*x=='E') // sounds on!
                                      {
                                      if (act_sounds_on)
                                          { act_sounds_on=2; break; }
                                      }
                                  else if (*x=='e') // sounds off!
                                      {
                                      if (act_sounds_on)
                                          { act_sounds_on=1; break; }
                                      }
                                      
								  else if (*x=='#') // 30.10.2010
                                      {
                                      mouse_keys=1;
                                      break;
                                      }                                      

               /* RS ADD  */      else if (*x=='N') // RS netpath
                                      {
                                      strcpy(x,survo_path);
              					      muste_simplify_path(x);
              					      tutcat(x);
              					      break;
              					      }
                                  else if (*x=='n') // RS netsurvo
                                      {
                                      tutcat("1");
                                      break;
                                      }                                      

                                  switch (*x)
                                      {
                                    case '1': m2=r1+r-1; break;
                                    case '2': m2=c1+c-1; break;
                                    case '3': m2=r1; break;
                                    case '4': m2=c1; break;
                                    case '5': m2=r2; break;
                                    case '6': m2=c2; break;
                                    case '7': m2=zshn; break;
                                    case '8': m2=numshad(); break;
                                      }
                                  sprintf(x,"%d",m2); 
                                  tutcat(x); 
                                  break;
          //  case 'L' loppuu!

              case 'v':           ver_disp=1; break;
              case 'x':           *x=*(z+ed1*(r1+r-2)+c1+c-1);
                 /* 17.11.1996 */ if (paint_on)
                                  sur_paint(z+ed1*(r1+r-2),c1+c-1,c1+c-1,'7');
                                  *(x+1)=EOS;
                                  tutcat(x); 
                                  break;

              case 'y':           prefix_y();
                                  break;
              case 'w':           edread(x,r1+r-1);
                                  p=q=x+c1+c-1;
                                  if (*p==' ') { tutcat(" "); break; }
                                  while (q>x && *q!=' ') --q;
                                  while (*p && *p!=' ') ++p; *p=EOS;
                                  if (paint_on) sur_paint(x,q-x+1,p-x-1,'7'); // 17.11.1996
                                  tutcat(q+1);

                                  break;
              case '0':           *tut_info=EOS; break;
              case 'I':           insert_mode=0; insert_type=1; break;

              case 'M': case 'm': x[0]=nextch_editor(); x[1]=EOS; sucro_macro(x,0);
                                      break;
              case 'N': case 'n': x[0]='#'; x[1]=nextch_editor(); x[2]=EOS;
                                      sucro_macro(x,0); break;
// 28.9.2005
              case '$':           x[0]='$'; x[1]=nextch_editor(); x[2]=EOS;
                                      sucro_macro(x,0); break;

              case '!': case ';': enter_sana(x);
                                  if (m=='!') m2=0; else m2=1;
                                  sucro_macro(x,m2); break;


              case 'D':           ref_c1=0; break;
              case 'J': case 'j': pre_j(1); disp(); break;
                             //         ^ 8.6.2010              

              case 'Q':           etuu=1; break;
              case 'q':           etuu=0; break;
              case 'd':           
              					  strcpy(x,edisk); 						// RS ADD
              					  muste_simplify_path(x);   			// RS ADD
              					  muste_insertquotes(x);
              					  tutcat(x);							// RS CHA edisk -> x
              					  break;
              case 'g':          
                            	  strcpy(x,esysd); // RS ADD
              					  muste_simplify_path(x);   			// RS ADD
              					  muste_insertquotes(x);
              					  tutcat(x);							// RS CHA esysd -> x
              					  break;
              case 'l':           m2=nextch_editor(); 
                                  label(m2,x);
                                  tutcat(x); break;
              case 'X':           edread(x,r1+r-1);
                                  i=strlen(x)-1;
                                  while (i>c1+c-2 && x[i]==' ') x[i--]=EOS;
                                  tutcat(x+c1+c-1);
                                  break;
              case 'B':           c1=c=1; disp(); break;
              case 'A':           ins_lines_on=1; break;
              case 'a':           ins_lines_on=0; break;

              case 'i':           m2=nextch_editor(); tut_index=m2-'0'; break;
              case 'b':           init_param1(); disp(); break;
              case 'u':           prompt_shadow=nextch_editor(); break;
              case 'o':           
              					  hae_edisk(x);
              					  muste_simplify_path(x);				// RS ADD
              					  muste_insertquotes(x);
              					  tutcat(x);
              					  break;
              case 'H':           strcpy(help_sana,"HELP"); break;
              case 'h':           
              					  strcpy(x,etmpd);						// RS ADD
              					  muste_simplify_path(x);				// RS ADD
              					  muste_insertquotes(x);
              					  tutcat(x);							// RS CHA etmpd -> x
              					  break;
              case '&':           enter_wsana(x);   // 29.5.1992 
                                  edt_talletus(x); break;
              case '%':           enter_sana(x); *sbuf=EOS;
                                  i=hae_apu(x,sbuf);
                                  tutcat(sbuf); break;
              case '^':           
              					  strcpy(x,survo_path);					// RS ADD
              					  muste_simplify_path(x);				// RS ADD
              					  muste_insertquotes(x);
              					  tutcat(x);							// RS CHA survo_path -> x
              					  break;
              case '/':           sek_aika(1); break;
              case '_':           sur_alarm=0; break;  // 10.8.1992 
              case '~':           x[0]=survo_type; x[1]=EOS; tutcat(x);
              					  // RS ALT tutcat("Muste");
                                  break;
              case 156:           x[0]=*crt_exit; x[1]=EOS;   // RS  punta
                                  tutcat(x); break;
              case 132:           
              					  strcpy(x,orig_setup); // RS ADD
              					  muste_simplify_path(x); // RS ADD
              					  muste_insertquotes(x);
              					  tutcat(x); break; // 23.1.93   // RS pikku ä
              case 142:           
              					  strcpy(x,current_setup); // RS ADD
              					  muste_simplify_path(x); // RS ADD
              					  muste_insertquotes(x);
              					  tutcat(x); break;           // RS iso Ä
              case 148:           sur_user_name(); break; // 21.2.93      // RS pikku ö

              case 'F':           type_without_limit=1; break;
              case 'f':           type_without_limit=0; break;


// 17.7.1998
              case '>':           if (hae_apu("pre>",x))
                                      sucro_macro(x,0);
                                  break;
              case '<':           if (hae_apu("pre<",x))
                                      sucro_macro(x,0);
                                  break;
// 22.7.1998 
              case 'U':           if (insert_mode) x[0]='1'; else x[0]='0';
                                  x[1]=EOS; tutcat(x); break;
              case ',':           if (sucro_pause) // muutettu 21.8.2004
                                      {
                                      sucro_pause=0;
                                      r=r_pause; r1=r1_pause;
                                      c=c_pause; c1=c1_pause;
                                      strcpy(x,etmpd);
                                      strcat(x,"S_PAUSE");
                                      edload(x,1);
                                      stack_save_load(2,"S_PAUSE.STK");
                                      disp();
                                      etu=2; tut_avaa();
                                  //  sprintf(sbuf,"%sSND\\SIB23A.WAV",survo_path);
                                  //  sur_play_sound(sbuf);
                                      tut_sound("3");
                                      sur_wait(1000L,Wdisp_editor,1);
                                      }
                                  break;
// 26.12.2000
              case 'k':           tutcat(os_ver); break;
// 25.9.2009
              case '|':
                                  first_word_on_line_search=1;
                                  strcpy(info,"F"); op_find();
                                  disp(); /* 21.9.92 */
                                  soft_disp(1);
                                  break;             
              
              case '-':           ref1_line=r1+r-1; break; // 29.11.2009

               default: 
                        *msana='M'; *(msana+1)=(char)m; *(msana+2)=EOS;

                        if(hae_apu(msana,x))
                            {
                            char *p;

                            p=x; // !<text> lisätty esim. /!DEL_L!  8.1.2007
                            if (*p=='!') { c1=c=1; muste_erase(); muste_erase(); ++p; }

                            m=strlen(p); m2=0;
                            if (p[m-1]=='!') { m2=1; --m; }
                            for (i=0; i<m; ++i)
                                key_common((int)(unsigned char)p[i]);
                            if (m2) { activate(); disp(); }

                            }

                }
            }
        }


int end_graphics()
        {
        disp_all();
        return(1);
        }

static int load_codes2(char *codefile,unsigned char *code,int col)
        {
        int i;
        char x[LLENGTH];
        extern int muste_fclose2();

        strcpy(x,codefile);
// RS REM        if (strchr(x,':')==NULL && *x!='.') // 29.3.2009
// RS REM           { strcpy(x,survo_path); strcat(x,"SYS/"); strcat(x,codefile); }

        codes=muste_fopen2(x,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nCode conversion file %s not found!",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (col>1) muste_fseek(codes,(long)(col-1)*256L,SEEK_SET);
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose2(codes);
        return(1);
        }

static int w_codes_load(int k,unsigned char code[])
    {
    char codefile[LNAME];

    strcpy(codefile,survo_path); strcat(codefile,"SYS/WIN.BIN");
    load_codes2(codefile,code,k);
    return(1);
    }

static int www_remove_spec_chars(char *s)
    {
    char t[LLENGTH];
    char merkit[]="()[]{}.,;:-!%&/=+?'*";
    char *p,*q;

    strcpy(t,s);
    p=t;
    while (1)
        {
        if (strchr(merkit,*p)!=NULL) ++p;
        else break;
        }
    q=t+strlen(t)-1;
    while (1)
        {
        if (strchr(merkit,*q)!=NULL) *q--=EOS;
        else break;
        }
    strcpy(s,p);
    return(1);
    }

static int get_www(char *site,char *s,char sep,char *quotes)  // 10.4.2008
    {
    char clip[LLENGTH];
    unsigned char code[512]; // RS ADD unsigned
    int i,j,len;
    char *p;
    char open_copy[LLENGTH];
    int cc;

// Rprintf("\nmove_r1=%d mr1=%d mc1=%d mr2=%d mc2=%d|",move_r1,mr1,mc1,mr2,mc2);
// getck();
    if (mc1 && mc2)
        {
        edread(sbuf,move_r1);
        *clip=EOS;
        strncat(clip,sbuf+mc1,mc2-mc1+1);
        p=clip;
        while ((p=strchr(p,' '))!=NULL) *p++=sep;
        mr1=mc1=mr2=mc2=m_move_ind=m_move_ind2=0;
        }
    else
        {
        edread(sbuf,r1+r-1);
        i=c1+c-1;
        if (sbuf[i]==' ') sbuf[i]='_';
        while (i>0 && sbuf[i]!=' ') --i;
        ++i; j=0;
        while (j<c2-10 && sbuf[i]!=' ') clip[j++]=sbuf[i++]; // SM ADD j<c2-10 && 22.7.2012
        clip[j]=EOS;
        p=clip;
        while ((p=strchr(p,'_'))!=NULL) *p++=sep;
        }

    if (*s!=' ')
        {
        len=strlen(clip);
        clip[len]=sep; clip[len+1]=EOS; strcat(clip,s);
        }

    www_remove_spec_chars(clip);

    if (*quotes!=EOS)
        {
        strcpy(sbuf,clip);
        strcpy(clip,quotes); strcat(clip,sbuf); strcat(clip,quotes);
        }

    w_codes_load(1,code);
    len=strlen(clip);
    for (i=0; i<len; ++i)
// RS CHA        (unsigned char)clip[i]=code[(unsigned char)clip[i]];
        clip[i]=code[(unsigned char)clip[i]];
    edread(open_copy,r1+r-1);
    edwrite(space,r1+r-1,1);
    sprintf(sbuf,"*%s%s / ",site,clip);
    edwrite(sbuf,r1+r-1,0);
    cc=c; c=1;
    activate();
    c=cc;
    edwrite(open_copy,r1+r-1,0);

    return(1);
    }


static int open_any_file()
    {
    char clip[LLENGTH];
    int i,j; // RS REM ,len;
    char *p;
    char open_copy[LLENGTH];
    int cc;
    int erunn;
    extern int erun;

    edread(sbuf,r1+r-1);
    i=c1+c-1;
    if (sbuf[i]==' ') sbuf[i]='_';
    while (i>0 && sbuf[i]!=' ') --i;
    ++i; j=0;
    while (sbuf[i]!=' ') clip[j++]=sbuf[i++];
    clip[j]=EOS;
    p=clip;

    edread(open_copy,r1+r-1);
    edwrite(space,r1+r-1,1);
// RS CHA    sprintf(sbuf,">>START %s",clip);
    sprintf(sbuf,"*/OPEN %s",clip);
    edwrite(sbuf,r1+r-1,0);
    cc=c; c=1; erunn=erun; erun=1;
    activate();
    c=cc; erun=erunn;
    edwrite(open_copy,r1+r-1,0);
    disp();
    return(1);
    }

static int show_line_labels()
    {
    int i,n,j,pos,step,h;
    char ch;
    char x[LLENGTH];

    step=c2+1;
    pos=-step; n=0; h=0;
    for (j=1; j<=r2; ++j)
        {
        pos+=step;
        ch=z[pos];
// Rprintf("%c",ch); getck();
        if (ch=='*') continue;
        for (i=0; i<n; ++i)
            if (x[i]==ch) break;
        if (i<n) continue;
        x[n++]=ch;
        }
    x[n]=EOS;
// Rprintf("\nx=%s|",x); getck();
    if (insertl()<0) return(1);
    for (j=0; j<n; ++j)
        {
        ch=(char)255;
        for (i=0; i<n; ++i)
            {
            if ((unsigned char)x[i]<(unsigned char)ch) { h=i; ch=x[i]; }
            }
        sbuf[j]=ch; x[h]=(char)255;
        }
    sbuf[n]=EOS;
    edwrite(sbuf,r1+r-1,1);
    disp();
    return(1);
    }

static int display_name_of_variable()
    {
    extern char *op;
    extern char *prompt_line;
    extern int nop();

    char x[LLENGTH];
    char *p,*q;

    edread(x,r1+r-1);
    p=x+c1+c-1;
    if (*p==' ') return(1);
    q=p; while ((int)(q-x)>0 && *q!=' ') --q;
    ++q;
    while (*p!=' ') ++p; *p=EOS;
    sprintf(info,"GET_VAR_NAME %s",q);
    op="CREATE";
//    childp("FI\\");
	muste_dump(sur_session);
	muste_file_create(arguc,arguv);	
	muste_restore_dump(sur_session);
    if (*info==EOS) return(1);

    p=prompt_line;
    prompt_line=info;
    disp_prompt_line('1');
    strcpy(tut_info,info); strcat(tut_info,"@");
    sur_wait(10000L,nop,1);
    prompt_line=p;
    disp_prompt_line('1');

// Rprintf("\ninfo=%s",info); getck();

    return(1);
    }

// survopoint functions 15.3.2012
static int start_survopoint_disp()
    {
    if (survopoint==EOS) return(1);
    survopoint_on=1;
    survopoint_disp_n=0;
    muste_set_R_int(".muste$eventlooptime",survopoint_disp);
    init_survopoint_counters();
    return(1);
    }

static int stop_survopoint_disp()
    {
    if (survopoint==EOS) return(1);
    survopoint_on=0;
    muste_set_R_int(".muste$eventlooptime",1000);    
    disp();
    return(1);
    }

int muste_save_firstline() // RS ADD 26.9.2012
	{
    edread(actline,1);
    g=splitq(actline+1,parm,MAXPARM);
    if (g<2) return(-1);
    strcpy(OO,parm[0]);
    muste_strupr(OO);    
    if (strcmp(OO,"SAVE")!=0) return(-1);
    edsave(parm[1],1,0);
    
    pyyhi_alarivi();
    LOCATE(r3+2,1); PR_EBLK;  
    sprintf(sbuf,"Edit field %s saved!",parm[1]);  
	sur_print(sbuf); PR_ENRM;
    sur_wait(1000L,nop,1);
    disp();    
    return(1);
    }
    
void muste_save_firstline_name(char *name) // RS ADD 26.9.2012
	{
	char *loppu;
	int apupit;
	char empty[]="";
	int oc,oc1,or,or1;

	loppu=strstr(name,".EDT");
	if (loppu!=NULL) *loppu=EOS;	
    edread(actline,1);
    g=splitq(actline+1,parm,MAXPARM);
    sprintf(sbuf,"*SAVE %s",name);
    if (g>0)
    	{
    	strcpy(OO,parm[0]);
    	muste_strupr(OO);    
    	if (strcmp(OO,"SAVE")==0) 
    		{
    		if (g>1) apupit=strlen(parm[1]);
    			else apupit=0;
			edread(actline,1);    			
			loppu=strstr(actline," / ");
			if (loppu==NULL) loppu=empty;
			sprintf(sbuf,"*SAVE %s%s",name,loppu);
			}
		else
			{
			oc=c; oc1=c1; or=r; or1=r1;
			c=1; oc1=1; r=1; r1=1;
			line_merge();
			c=oc; oc1=oc1; r=or; r1=or1;
			}
		}		
	edwrite(space,1,1);			
	edwrite(sbuf,1,0);
	disp();	
	muste_save_firstline();	
	return;
	}
        
int prefix2()
    {

    int m; // RS REM ,m2;
    int i;
    char x[LLENGTH];
// RS REM    char msana[3];
// RS REM    char *p,*q;
    extern int muste_emacs;
    int taltio;
    int j=r1+r-1;

	if (special==1) // RS ADD
        {
        if (muste_emacs) { m='E'; special=0; }
        else m=nextch_editor();
        }
    else
    	{
        strcpy(help_sana,"HELP");    	
    	special=1;
    	m=CODE_HELP;
		}
    pref=' ';

    if (special)
        {
        switch(m)
            {
          case CODE_HELP:
            help("HELP"); disp(); break;
          case CODE_SRCH:                // 25.9.2009
            first_word_on_line_search=1;
            strcpy(info,"F"); op_find();
            disp(); // 21.9.92
            soft_disp(1);
            break;
            }
        return(1);
        }

// get_www(program, additional term, separator, quotes)

// Rprintf("\nm=%d",m); getck();
   if ((char)m>='0' && (char)m<='9')
       {
       char *p,*q;
       char sbuf2[LNAME];
       char term[LNAME],separator,quotes[10];

       sprintf(x,"web%c",(char)m);
       i=hae_apu(x,sbuf2); if (i==0) return(1);
// Rprintf("\nsbuf2=%s|",sbuf2); getck();
       strcpy(term," "); separator='+'; *quotes=EOS;
       p=strstr(sbuf2,"||");
       if (p!=NULL)
           {
           *p=EOS; p+=2;
           q=strstr(p,"||");
           if (q==NULL) strcpy(term,p);
           else
               {
               *q=EOS; strcpy(term,p); p=q+2;
               q=strstr(p,"||");
               separator=*p;
               if (q!=NULL) strcpy(quotes,p+3);
               }
           }

// Rprintf("\nsbuf2=%s term=%s separator=%c quotes=%s ",sbuf,term,separator,quotes);
// getck();
       get_www(sbuf2,term,separator,quotes); return(1);
       }
// Rprintf("\nm=%c",(char)m); getck();
// get_www(program, additional term, separator, quotes)

    switch(m)
        {
      case 'G':
            get_www(google," ",'+',"%22"); return(1);
      case 'g':
            get_www(google," ",'+',""); return(1);
      case 'S':
      case 's':
            get_www(google,"Survo",'+',""); return(1);
      case 'W':
      case 'w':
            get_www(wiki," ",'_',""); return(1);
      case 'D':
      case 'd':
            get_www(dict1," ",dict2,""); return(1);
      case 'J': // 8.5.2010
      case 'j':
            pre_j(2); disp(); return(1);
      case 'L':
      case 'l':
            show_line_labels(); return(1);
      case 'o':
      case 'O':
            open_any_file(); return(1);
      case 'v':
      case 'V':
            display_name_of_variable(); return(1);
      case '#':
            modify_command_and_set_fence_line(); return(1);
      case 'A': // 15.3.2012
            start_survopoint_disp(); return(1);
      case 'a':
            stop_survopoint_disp(); return(1);
           
      case 'E': // RS ADD
      		taltio=muste_emacs;
      		muste_emacs='E';
      		m=nextch_editor();
      		muste_emacs=taltio;
      		m=nextch_editor();      		
      		muste_emacs=FALSE;
      		switch(m)
      			{
      			case 'A':   // beginning-of-line
                    c=1; c1=1; disp();     			
      				return(1);
      			break;
      			case 'C':   // copy selection
      			break;
      			case 'D':   // delete-char-and-row
                    if (kontr_()) return(1);
        			edread(x,j);
        			if (j<r2 && empty(x+c1+c-1,c2-c1-c+2))
        				{
        				line_merge();
        				edread(x,j+1);
        				if (empty(x+1,c2))
        					{
        					r++; deletel(); r--;
        					}
        				}
        			else
        				{
	        			edread(x,j);
        				x[c1+c-1]=EOS; strcat(x,x+c1+c); strcat(x," ");
        				edwrite(x,j,0);
        				if (zs[j]!=0)
           	 				{
        					edread(x,zs[j]);
        					x[c1+c-1]=EOS; strcat(x,x+c1+c); strcat(x," ");
        					edwrite(x,zs[j],0);
        					testshad(j);
            				}
        				displine2(j);
						}                     
                   return(1);
				case '<':   // shift+backspace RS ADD 26.9.2012
                    if (kontr_()) return(1);
                    if (c1==1 && c<2)
                    	{
                    	c=1; r--; j--; seek_line_end();                   	
        				line_merge();
        				edread(x,j+1);
        				if (empty(x+1,c2))
        					{
        					r++; deletel(); r--;
        					}
        				return(1);
						}
        			edread(x,j);
        			if (j<r2 && empty(x+1,c2)) { c=1; c1=1; displine2(r1+r-1); }
        			else        		
        				{
                    	if (c>1) { --c; displine2(r1+r-1); }
                    	else if (c1>1) { --c1; nleft=0; disp(); }         				
        				
	        			edread(x,j);
        				x[c1+c-1]=EOS; strcat(x,x+c1+c); strcat(x," ");
        				edwrite(x,j,0);
        				if (zs[j]!=0)
           	 				{
        					edread(x,zs[j]);
        					x[c1+c-1]=EOS; strcat(x,x+c1+c); strcat(x," ");
        					edwrite(x,zs[j],0);
        					testshad(j);
            				}
        				displine2(j);
						}                     
                   return(1);                   
      			case 'E':   // end-of-line
      			break;
      			case 'K':   // kill-line
                    if (kontr_()) return(1);
        			edread(x,j);
        			if (j<r2 && empty(x+c1+c-1,c2-c1-c+2))
        				{
        				line_merge();
        				edread(x,j+1);
        				if (empty(x+1,c2))
        					{
        					r++; deletel(); r--;
        					}
        				}
        			else era(j);
        			return(1);
      			case 'O':   // open-line
        			edread(x,j);
        			if (j<r2 && !empty(x+c1+c-1,c2-c1-c+2))
        				{
        				if (kontr_()) return(1);
        				edread(x,j+1);
        				if (empty(x+1,c2))
        					{
        					i=insertl();
            			    if (r<r3 && i>0) --r;
        					}        				        				
        				line_merge();
        				}
        			else
        				{                   				
        				i=insertl();
            			if (r<r3 && i>0) --r;
        				}
        			return(1);      			 			
      			case 'R':   // execute R
      			case 'S':   // save edit field
					i=muste_save_firstline();
					if (i<0)
						{
						PR_EINV;
						sur_print("\nERROR! SAVE command not found on the first line!");
						WAIT; PR_ENRM;
						disp();
						}
					return(1);      			
      			case 'T':   // transpose chars
      			case 'V':   // paste
				break;      
      			case '^':   // shift-return
        			edread(x,j);
        			if (j<r2 && !empty(x+c1+c-1,c2-c1-c+2))
        				{
        				if (kontr_()) return(1);
        				edread(x,j+1);
        				if (empty(x+1,c2))
        					{
        					i=insertl();
            			    if (r<r3 && i>0) --r;
        					}        				        				
        				line_merge();
        				}
        			else
        				{                   				
        				i=insertl();
            			if (r<r3 && i>0) --r;
        				}
        			key_special(CODE_RETURN);	
        			return(1);  							
      			} 
      			sprintf(sbuf,"\nF1-E-%c",m); sur_print(sbuf); WAIT; disp();
      		break;
     			           
            return(1);
        }

// RS REM    if (m=='?') { start_editgame(); return(1); } // 23.10.2008

// RS REM    Rprintf("\nF1: %c|",(char)m); getck();
   
    return(1);
    }        

int key_special(int m)
                {
                int i,k;
// RS REM                char x[LLENGTH];
                extern char *p_soft_key_text;

                vnumtab=numtab;
                numtab=0;
                switch (m)
                  {
                  case CODE_EXIT:
                    if (display_off) { restore_display(1); break; }                    
                    i=lopetuskysely();
                    if (i) return(-1); // RS CHA exit(0);                        
                    disp(); break;
                  case CODE_RIGHT:
                    move_right2(1);
                    break;
                  case CODE_BACKSP:
                    if (kontr_()) break;
                    if (muste_selection) { muste_erase(); muste_selection=FALSE; break; } // RS ADD 26.7.2012
                    if (c>1) { --c; z[ed1*(r1+r-2)+c+c1-1]=' ';
                               displine2(r1+r-1); break; }
                  case CODE_LEFT:
                    if (c>1) { --c; nleft=0; break; }
                    if (c1>1) { --c1; nleft=0; disp(); break; }                  
                    if (prevkey==CODE_LEFT)
                        {
                        if (nleft>=2) { c=0; nleft=0; break; }
                        ++nleft;
                        break;
                        }
                    nleft=0;
                    break;
                  case CODE_UP:
                    move_up2(1);
                    break;
                  case CODE_DOWN:
                    step_down(1);
                    break;
                  case CODE_RIGHT2:
                    move_right2(2*cursor_step);
                    break;
                  case CODE_LEFT2:
                    for (i=0; i<2*cursor_step; ++i)
                        {
                        if (c>1) { --c; continue; }
                        if (c1>1) { --c1; disp(); }
                        }
                    break;
                  case CODE_UP2:
                    move_up2(cursor_step);
                    break;
                  case CODE_DOWN2:
                    step_down(cursor_step);
                    break;                    
                  case CODE_HOME:
                    if (c>1) { c=1; break; }
                    if (c1>1) { c1=1; disp(); break; }
                    if (r>1) { r=1; break; }
                    if (r1>1) {r1=1; disp(); break; }
                    break;                  
                  case CODE_INSERT:
                    if (kontr_()) break;
                if (insert_type) { insert_mode=1-insert_mode; disp(); break; }
                    insert(); disp(); break;
                  case CODE_DELETE:
                    if (kontr_()) break;
                    else delete(); break;
                  case CODE_INSERTL:
                    insertl(); break;
                  case CODE_DELETEL:
                    if (kontr_()) break;
                    deletel(); break;
                  case CODE_ERASE:
                    if (kontr_()) break;
                    muste_erase();
                    break;
                  case CODE_NEXT:
                    if (r2<=r3) break; // 22.12.2000
                    r1+=r3;
                    if (r1>r2-r3+1) r1=r2-r3+1;
                    disp();
                    break;
                  case CODE_PREV:
                    if (r1<r3+2) r1=1; else r1-=r3;
                    disp();
                    break;
                  case CODE_EXEC:

                    space_break=space_break2;

                    m_move_ind=0; // 21.3.2004

                    if (time_file_on) file_time_start();
                    k=activate();              
                    if (time_file_on) file_time_end(parm[0]);
                    *op_sana=EOS; // 10.3.1997 
                    if (etu==1) lue_hetki(&wait_hetki);  // 26.5.1995 
                    space_break=0;
                    PR_ENRM;
                    if (k<=1) disp();
                    if (k==3) { WAIT; }  // {} pakolliset!
                    if (k<=3) displine2(r1+r-1);
                    if (k==4 && display_off==0) { end_graphics(); disp(); }
                    break;

                  case CODE_REXEC:    // RS ADD Execute R function
					k=ractivate(1);
                    if (k<=1) disp();
                    if (k<=3) displine2(r1+r-1);
                    break;
                                        
                    
                    
                    
                  case CODE_RETURN:
// RS turha???  sur_sleep(2);
                    dispm=0; PR_ENRM;
                    if (type_without_limit) { c1=1; disp(); } /* 2.4.1997 */
                    if (r==r2) break; // 22.12.2000
                    if (r<r3)
                        {
                        ++r; c=left_edge;
                        break;
                        }
                    if (r1<r2-r3+1)
                        {
                        ++r1; c=left_edge;
                        if (display_off) break;
                        SCROLL_UP(1,r3,1);
                        displine2(r1+r3-1);
                        break;
                        }
                    c=1; break;
                  case CODE_DISP:
                    ++dispm;
                    if (dispm>7) dispm=0;
// RS REM                    CURSOR_OFF; 
                    headline_editor();
// RS REM                    cursor_on();
                    break;
                  case CODE_PRE:
                    pref=(unsigned char)STAMP; // RS CHA PREFIX -> STAMP;  /* 8.3.2000 */
                    prefix(); break;
                  case CODE_TOUCH:
                  
                    PR_ENRM;
                    i=soft_vis;
                    soft_vis=0;
                    p_soft_key_text=NULL;
                    soft_disp(0); // 22.10.2001
                    if (rsh) disp_shadow();  // 27.12.2004
                    // RS REM strcpy(ops,"T"); op=ops;
                    // RS CHA childp("&"); ->                    
                    muste_dump();
                    muste_no_selection=TRUE;
                    muste_touch(arguc,arguv); 
                    muste_no_selection=FALSE;
                    muste_restore_dump();
                    
                    
                    soft_vis=i;
                    disp();   
                    soft_disp(1); // RS ADD
                    break;
                    
                    
                  case CODE_DISK:
/* RS REM   Tarpeeton ominaisuus nykyään
                    if (last_disk[0]=='Z')
                        {
                        vaihda_polku();
                        }
                    else
                        {
                        if (edisk[0]==*esysd)
                            { strcpy(ediskpath,edisk); edisk[2]=EOS; }
                  if (edisk[0]<last_disk[0]) ++edisk[0]; else edisk[0]='A';
                        edisk[1]=':'; edisk[2]=EOS;
                        if (edisk[0]==*esysd) strcpy(edisk,ediskpath);
                        }
                    CURSOR_OFF; headline_editor(); cursor_on();
*/
/* RS Hakemistonvaihtodialogi */
                    sprintf(sbuf,"CD *");
                    g=split(sbuf,parm,MAXPARM);                    
                    muste_setwd();       // RS  Hakemistonvaihtodialogi
                    headline_editor(); 

                    break;
                  case CODE_CODE:
                    code_code();
                    break;
                  case CODE_REF:
                    if (ref_c1==0) { ref_c1=c1; ref_c=c;
                                     ref_r1=r1; ref_r=r; break;
                                   }
                    if (((ref_c1+ref_c==c1+c) && (ref_r1+ref_r==r1+r)) ||
                        (ref_r1+ref_r-1>r2) || (ref_c1+ref_c-1>c2))
                                   { ref_c1=0; break; }
                    c1=ref_c1; c=ref_c; r1=ref_r1; r=ref_r; disp();
                    break;
                  case CODE_REF_SET: // 30.4.2002
                    ref_c1=c1; ref_c=c; ref_r1=r1; ref_r=r; break;
                  case CODE_MERGE:
                    if (kontr_()) break;
                    line_merge(); break;
                  case CODE_COPY:
                    if (kontr_()) break;
                    muste_no_selection=TRUE;
                    line_copy(); 
                    muste_no_selection=FALSE;
// RS turha???              sur_sleep(10); // nopeat sukrot!! 1.8.2000
                    soft_vis=1; disp_all(); break;
                  case CODE_TAB:
                    next_tab(); 
                    break;
                  case CODE_HELP:
					pref=(unsigned char)STAMP; // RS CHA PREFIX -> STAMP;  // PREFIX2?
                    prefix2(); 
                  break;
                  
                  case CODE_ACTIV:
// muste_fixme("FIXME: CODE_ACTIV not yet implemented!\n"); // RS FIXME 
					muste_no_selection=TRUE;
                    file_act("KEY_ACTIV");
                    muste_no_selection=FALSE;
                    p_soft_key_text=NULL;
                    disp();
                  break; 
                  case CODE_MOVE:
                    if (kontr_()) break;
                    move_block(0);
                    break;
                  case CODE_WORDS:
                    if (kontr_()) break;
                    move_block(1); 
                    break;
                  case CODE_END:
                    seek_line_end(); break;
                  case CODE_SRCH:
                    strcpy(info,"F"); op_find();
                    disp(); // 21.9.92
                    soft_disp(1);
                    break;

                  case CODE_SOFT_ON: // 8.2.2001
                    restore_softkeys(); // 1.5.2002
                    break;

                  case 151: copy_to_clipboard(); break; // 24.4.2006
                  case 153: op_paste(2); disp(); break;  // 24.4.2006

                  case CODE_SUCRO1:            /* 3.9.1995 */
                  case CODE_SUCRO2:
                  case CODE_SUCRO3:
                  case CODE_SUCRO4:
                  case CODE_SUCRO5:
                  case CODE_SUCRO6:
                  case CODE_SUCRO7:
                  case CODE_SUCRO8:
                    sucro_key(m-CODE_SUCRO1+1); 
                  break;
                  }
                return(1); // RS ADD  
                } /* end special */


int key_common(int m)
        {
        int h,jj;
        unsigned int i,j;
        char x[LLENGTH], x1[LLENGTH];
        char xs[LLENGTH], x1s[LLENGTH]; // 18.1.2001
        char *pz;

        if (!etu)
            {
            if (m>0 && m<9) { sucro_key(m); return(1); } // tilap. 14.2.1996
            }

        if (numtab)
            {
            if ( (m>='0' && m<='9') || (m=='+') || (m=='-') )
                {
                edread(x,r1+r-1);
                i=c1+c-2;
                while (i>0 && x[i]!=' ') --i;
                if (i==0 || x[i-1]!=' ') { BEEP; numtab=0; return(1); }
                cursor(r,i-c1+1);
                for (; i<c1+c-2; ++i) { x[i]=x[i+1];
                           sprintf(sbuf,"%c",x[i]); sur_print(sbuf); }
                x[c1+c-2]=m; sprintf(sbuf,"%c",m); sur_print(sbuf);
                edwrite(x,r1+r-1,0);
                return(1);
                }
            else numtab=0;
            }

        switch (m)
            {

          default: if (c==0) { dispch(m);
                               if (r<r3) { ++r; break; }
                               if (r1<r2-r3+1)
                                   {
                                   ++r1; SCROLL_UP(1,r3,1);
                                   displine2(r1+r3-1);
                                   break;
                                   }
                               break;
                             }
                   pz=z+ed1*(r1+r-2);
                   if (*pz=='!' || *pz=='_') break;
                   if (c<=c3 && c<=c2)
                       {
                       if (insert_type && insert_mode)
                           {
                           if (insert()<0) break;
                           cursor(r,c);
                           }
                       dispch(m);
                       if (type_without_limit && c==c3 && c<=c2) /* 2.4.1997 */
                           {
                           if (c1+c<=c2) { ++c1; disp(); }
                           else BEEP;
                           }
                       else ++c; // RS ALT only with if(c1+c-1<c2) not working as otherwise no automatic next line
                       break;
                       }
                   if (c>c3 || c>c2)   /* c>c2 lisätty 30.8.87 */
                    {


                    if (r1+r-1==r2 || (c2>=c3 && !empty(pz+c1+c,c2-c1-c)))
                         { BEEP; break; }
                    edread(x1,r1+r);
                    if (!empty(x1+1,c2) )
                        {
                        if (ins_lines_on && insert_type && insert_mode)
                            {
                            if (r==r3) { step_down(1); --r; }
                            h=insertl(); if (h<0) break;
                            --r; strncpy(x1,space,c2); *x1='*';
                            }
                        else { BEEP; c=c3; break; }
                        }
                    edread(x,r1+r-1); i=c1+c-2;
                    while (i>10)
                        {
                        if (x[i]==' ') break;
                        --i;
                        }
                    if (i<=10) { BEEP; break; }

                    jj=r1+r-1;
                    if (zs[jj]==0) creatshad(jj);
                    edread(xs,zs[jj]);
                    if (zs[jj+1]==0) creatshad(jj+1);
                    edread(x1s,zs[jj+1]);

                    for (j=i+1; j<=c1+c-2; ++j)
                        {
                        x1[c1+j-i-1]=x[j];
                        x1s[c1+j-i-1]=xs[j];
                        x[j]=' ';
                        xs[j]=' ';
                        }
                    edwrite(x,r1+r-1,0); edwrite(x1,r1+r,0);
                    edwrite(xs,zs[jj],0); edwrite(x1s,zs[jj+1],0);
                    testshad(jj); testshad(jj+1);

                    if (r<r3) ++r;
                    else
                        {
                        ++r1;
                        SCROLL_UP(1,r3,1);
                        }
                    displine2(r1+r-2); displine2(r1+r-1);
                    c=c1+c-1-i; cursor(r,c); dispch(m); ++c;
                    break;
                    }

                    break;  /* kesken */
            }
        return(1);
        }


static int init_sapu(char *apufile)
        {
        char *p;
        int merkki;
        char afile[LLENGTH];
        FILE *apu0;   

        sapu[MAXTILA]=(unsigned char)254;   // (unsigned char)'_';   /* 26.3.1992 */
        sapu[MAXTILA+1]=(unsigned char)254; // (unsigned char)'_';   /* 10.10.1994 */
        *sapu=EOS; p=sapu;
        add_survo_path(afile,apufile);
        strcpy(afile,apufile); // RS no file path, just the name

        apu0=muste_fopen2(afile,"rt");
        if (apu0==NULL)
            {
            sprintf(sbuf,"\nFile %s missing!",afile); // RS Rprintf -> sprintf
            sur_print(sbuf);
            WAIT;
            return(-1);
            }

        while (1)
            {
            merkki=fgetc(apu0);

//            if (merkki=='\n' || merkki=='\r') merkki=fgetc(apu0); // RS unix fix
//            if (merkki=='\n') merkki=fgetc(apu0);


            if (merkki==EOF) break;
            if (merkki=='/')
                {
                merkki=fgetc(apu0);
//                if (merkki=='\r') merkki='\n'; // RS unix fix  windows fix
                if (merkki==' ')
                    {
                    while (*(p-1)==' ') --p;
                    while (1)
                        {
                        merkki=fgetc(apu0);
                        if (merkki=='\n' || merkki==EOF) break; // RS windows fix
//                        if (merkki=='\n' || merkki==EOF || merkki=='\r') break; // RS unix fix
                                                          
                        }
                    if (merkki==EOF) break;
                    }
                else { *p='/'; ++p; }
                }
            if (merkki!='\r') { *p=merkki; ++p; } // RS ADD
            if (p-sapu>=MAXTILA)
                {
                sur_print("\nFile SURVO.APU is too large!"); // RS Rprintf->sur_print
                WAIT;
                return(-1);
                }
            }
        *p=EOS;
/* Rprintf("\np-sapu=%d %d",p-sapu,MAXTILA); getch();  */
        muste_fclose(apu0);
        return(1);
        }



static void edscratch(
unsigned lin /* ens.tyhjennettävä rivi */
)
        {
        unsigned int  i,j,l;
        l=(lin-1)*ed1;
        for (j=lin; j<=ed2; ++j)
                {
                z[l++]='*';
                for (i=0; i<ed1-1; ++i) z[l++]=' ';
                }
        }

static void op_scratch()
        {
        unsigned int l,j=r1+r-1;

        edscratch(j);
        l=ed2;
        for (; j<=l; ++j)
            if (zs[j]>0) { z[(zs[j]-1)*ed1]='\0'; zs[j]=0; }
        strcpy(help_sana,"HELP");
        }

static int field_init()
        {
        int i;
        i=ed_malloc(ed1,ed2,edshad); if (i<0) return(-1);
        edscratch(1);
        c=c1=r=r1=1;
        shadinit();
        return(1);
        }

void init_param1()
        {
/* RS REM ?!?
        extern int insert_mode,move_ind,dispm,ref_c1,rsh,left_edge;
        extern int ins_lines_on,tut_index,insert_type;
        extern char *prompt_line,prompt_shadow;
        extern int paint_on,type_without_limit;
*/

        insert_mode=0;
        move_ind=0;
        dispm=0;
        ref_c1=0;
        if (rsh) disp_shadow();
        left_edge=1;
        ins_lines_on=0;
        tut_index=0;
        insert_type=1;
        prompt_line=NULL;
        prompt_shadow='1';
        paint_on=0; /* 17.11.1996 */
        type_without_limit=0; /* 3.4.1997 */
        }

int op_init()
        {
        int i;
        int ued2,ued1,uedshad;

        if (g<3) { op_incomplete(); return(-1); }
        ued2=atoi(parm[1]); if (ued2<r3) return(-1);
        ued1=atoi(parm[2])+1; if (ued1>LLENGTH-3) return(-1);
        if (g>3) uedshad=atoi(parm[3]); else uedshad=edshad;
/*      if ((long)ued1*(long)(ued2+uedshad)>65535L) return(1);   */
        r2=ed2=ued2; ed1=ued1; edshad=uedshad; c2=ed1-1;
        init_param1();
        i=field_init(); if (i<0) return(-1); // RS CHA exit(1) -> return(-1)

        return(1);
        }

void hae_edisk(char *s)
        {
        int i;
        i=hae_apu("edisk",s);
        if (!i)
          {
          strcpy(s,survo_path); 
// RS REM         strcat(s,"D\\"); // 6.1.93
          
          }
// Rprintf("\nhae_edisk: %s",s);         
        }


#define N_SESS 20
static FILE *sessions;

static int save_sessions(char *nimi)
    {
    int i,k;
    char x[LLENGTH];

    sessions=muste_fopen2(nimi,"wt");
    if (sessions==NULL) return(-1); // RS ADD

    for (i=0; i<N_SESS; ++i)
        {
        edread(x,i+1);
        k=20; while (x[k]==' ') x[k--]=EOS;
        fprintf(sessions,"%s\n",x+1);
        }
    muste_fclose(sessions);
    return(1);
    }


static int load_sessions(char *nimi)
    {
    int i;
    char x[LLENGTH];

    sessions=muste_fopen2(nimi,"rt");
    if (sessions==NULL) return(-1); // RS CHA exit(0); // 19.3.2004
    for (i=0; i<N_SESS; ++i)
        {
        fgets(x,20,sessions);
        x[strlen(x)-1]=EOS;
        edwrite(x,i+1,1);
        }
    muste_fclose(sessions);
    return(1);
    }

static int remove_current_session()
    {
    int i;
    char nimi[LNAME];
    char x[LLENGTH];
// RS REM    char *s[2];

    for (i=0; i<N_SESS; ++i)
        edwrite(space,i+1,1);

    strcpy(sbuf,etmpd);
    if (tmp_by_session) // 19.3.2004
        {
        i=strlen(sbuf);
        sbuf[i-2]=EOS;
        }

    strcpy(nimi,sbuf); strcat(nimi,"SESSIONS.SYS");
    load_sessions(nimi);
    i=sur_session[0]-'A'+1;
    edwrite(space,i,1);
    sprintf(x,"%c 0",sur_session[0]);
    edwrite(x,i,1);
    save_sessions(nimi);
    return(1);
    }

static int by_session()
    {
    if (tmp_by_session)
        {
        strcat(etmpd,sur_session);
        sur_make_dir(etmpd);
        strcat(etmpd,"/");
        sys_save_restore(1);
        }
	sprintf(muste_clipfile,"%sMUSTE.CLP",sur_session); // RS ADD
	sprintf(muste_command,"%sMUSTE.CMD",sur_session); // RS ADD        
    return(1);
    }

static int set_sur_session()
    {
    int i;
    int nro;
    char nimi[LNAME];
    char x[LLENGTH];
    char *s[2];
    int vapaa,ens,viim,i_ens=0;
    FILE *sessions=NULL;

// RS REM    sur_make_dir(etmpd); // 19.3.2004 (varmuuden vuoksi)
    nro=0;
    strcpy(nimi,etmpd); strcat(nimi,"SESSIONS.SYS");
    i=sur_find_file(nimi);
    if (i==0)
        {
        sessions=muste_fopen2(nimi,"wt");       
        if (sessions==NULL) return(-1); // RS ADD       
        for (i=0; i<N_SESS; ++i)
            {
            if (i==0) nro=1; else nro=0;
            fprintf(sessions,"%c %d\n",(char)('A'+i),nro);
            }
        muste_fclose(sessions);
        strcpy(sur_session,"A"); 
        by_session();       
        return(1);
        }
//  { strcpy(sur_session,"B"); return(1); }  
    load_sessions(nimi); 
    vapaa=-1; ens=10000000; viim=0;
    for (i=0; i<N_SESS; ++i)
        {
        edread(x,i+1);

        split(x+1,s,2);
        nro=atoi(s[1]);
        if (vapaa==-1)
            {
            if (nro==0) vapaa=i;
            }
        if (nro)
            {
            if (nro<ens) { ens=nro; i_ens=i; }
            if (nro>viim) viim=nro;
            }
        }

    if (vapaa==-1) vapaa=i_ens;
    sprintf(sbuf,"%c %d",(char)('A'+vapaa),viim+1);
    edwrite(sbuf,vapaa+1,1);

    save_sessions(nimi);

    sur_session[0]=(char)('A'+vapaa);

    for (i=0; i<N_SESS; ++i)  // tyhjent‰‰ alkurivit!
        edwrite(space,i+1,1);
    by_session();

//	sprintf(muste_clipfile,"%sMUSTE.CLP",sur_session); // RS ADD
//	sprintf(muste_command,"%sMUSTE.CMD",sur_session); // RS ADD
    return(1);
    }

static void muste_set_sysname() // RS ADD 22.9.2011
  {
  char sysname[LLENGTH];
  muste_get_R_string(sysname,".muste$sysname",LLENGTH); muste_strlwr(sysname);
  sprintf(sbuf,"SYSTEM sysname=%s",sysname);
  survoapu1(1,sbuf);
  
  muste_get_R_string(sysname,".muste$Rhome",LLENGTH);
  sprintf(sbuf,"SYSTEM R_path=%s",sysname);
  survoapu1(1,sbuf);    
  }

static int muste_editor_init(char *apufile,int tunnus)
        {
        int i;
        char ch;
        char sana[128];
        char *osa[4]; /* 10.3.1995 */
        char *p;
        FILE *apu;

        for (i=0; i<LLENGTH; ++i) { space[i]=' ', stripe[i]=STRIPE; }
        space[LLENGTH]=EOS; /* 3.3.91 char space[LLENGTH+1] */
        shadow_code=shadow_code_tila;
        *op_sana=EOS;

        PR_ENRM;

        ed1=ED1; ed2=ED2; edshad=ED3;
        r3=ER3; c3=EC3;
// RS REM       p_survo_id=survo_id;
        prompt_line=NULL;

        i=init_sapu(apufile); if (i<0) return(-1);
        
        muste_set_sysname(); // RS

// RS NYI        p_survo_id=NULL;  /* childp() muuttaa! */

/* RS NYI
        i=hae_apu("computation_speed",sana);
        if (i) computation_speed=atoi(sana);
        else computation_speed=mittaa_nopeus();
*/

i=hae_apu("edit_font",sana);
if (i)
	{
    extern char muste_window_name[];
    
    split(sana,osa,4);
    
	sprintf(sbuf,".muste.findfontsize(x=%d,y=%d)",atoi(osa[0]),atoi(osa[1]));
	muste_evalr(sbuf);
	
	i=muste_get_R_int(".muste$font.size");
    muste_font(i);
    sur_pos_window(muste_window_name,atoi(osa[2]),atoi(osa[3]));
	}


        i=hae_apu("ed1",sana); if (i) ed1=atoi(sana);
        i=hae_apu("ed2",sana); if (i) ed2=atoi(sana);
        i=hae_apu("ed3",sana); if (i) edshad=atoi(sana);
        i=hae_apu("er3",sana); if (i) r3=atoi(sana);
        i=hae_apu("ec3",sana); if (i) c3=atoi(sana);

        r2=ed2; c2=ed1-1;

/* RS ADD initialize edit field space */
        z=muste_malloc(sizeof(char)*ed1*(ed2+edshad));
        if (z==NULL) { not_space(); return(-1); }
        zs=(int *)muste_malloc((ed2+1)*sizeof(int)); // RS oli unsigned int *
        if (zs==NULL) { not_space(); return(-1); }
        

        i=field_init(); if (i<0) return(-1);
        

// RS REM turha nykyään?    strcpy(last_disk,"Y:");
        *esysd=EOS;  // RS CHA nyt levytynnus vain jos survo_path:in toinen merkki on : 
        if (survo_path[1]==':') esysd[0]=*survo_path; esysd[1]=':'; esysd[2]=EOS;  // 9.10.91
        if (netd(survo_path)) *esysd=EOS; // 16.2.2006



        *muste_rout=EOS;     
        strcpy(sbuf,survo_path); i=strlen(sbuf); // 6.3.1999 
// RS REM sbuf[i-2]=EOS; // formally <Survo>   // RS i-2 poistaa ihan liikaa
        strcpy(qpath,sbuf); strcat(qpath,"Q/EDQ");   // RS KORJAA filesep
        hae_edisk(edisk);   // 25.3.1991, 6.1.93
        
//        strcpy(etmpd,sbuf); strcat(etmpd,"TMP/"); // 26.10.1996   RS CHA \\ -> /
		i=muste_get_R_int(".muste$writeaccess");
		if (i) { strcpy(etmpd,survo_path); strcat(etmpd,"TMP/"); }
		else muste_get_R_string(etmpd,".muste$Rtempdir",LLENGTH);	
        hae_apu("tempdisk",etmpd); subst_survo_path_in_editor(etmpd);
    	ch=etmpd[strlen(etmpd)-1];
    	if (ch!='/' && ch!='\\') strcat(etmpd,"/");	        
        strcpy(eout,etmpd); strcat(eout,"RESULTS");  // RS CHA \\ -> /        
        
        muste_get_R_string(muste_Rpath,".muste$Rhome",LLENGTH); // RS ADD 27.9.2012
        ch=muste_Rpath[strlen(muste_Rpath)-1];
    	if (ch!='/' && ch!='\\') strcat(muste_Rpath,"/");
        
        
        hae_apu("eout",eout); subst_survo_path_in_editor(eout);
        hae_apu("rout",muste_rout); subst_survo_path_in_editor(muste_rout);
        hae_apu("last_disk",last_disk);
        hae_apu("qpath",qpath); subst_survo_path_in_editor(qpath); speclist=SPECLIST;
        i=hae_apu("speclist",sana); if (i) speclist=atoi(sana);

        specmax=SPECMAX;
        i=hae_apu("specmax",sana); if (i) specmax=atoi(sana);
        scale_check=0; /* no scale type checks */
        i=hae_apu("scale_check",sana); if (i) scale_check=atoi(sana);
        accuracy=7; /* # of significant digits in free format results etc. */
        i=hae_apu("accuracy",sana); if (i) accuracy=atoi(sana);
        if (accuracy<4) accuracy=4;
        if (accuracy>16) accuracy=16;
       
        results=70; /* printout level */
        i=hae_apu("results",sana); if (i) results=atoi(sana);
        if (results<0) results=0;     // RS accuracy -> results
        if (results>100) results=100; // RS accuracy -> results

        v_results=results; // RS ADD
        v_accuracy=accuracy; // RS ADD

        g=2; parm[1]=edisk; op_cd();

        insert_type=1; /* 1=automatic insert, when INSERT pressed */
        i=hae_apu("insert_type",sana); if (i) insert_type=atoi(sana);
        ins_lines_on=0; /* 1=new_lines_automatically_in_insert */
        i=hae_apu("insert_lines",sana); if (i) ins_lines_on=atoi(sana);

/* RS Ei käytössä
        disp_wait=1;
        i=hae_apu("disp_wait",sana);
        if (i) { disp_wait=atoi(sana); if (disp_wait<1) disp_wait=1; }
*/
        
        child_wait=0;
        i=hae_apu("child_wait",sana); if (i) child_wait=atoi(sana);
        autosave=0;
        i=hae_apu("autosave",sana); if (i) autosave=atoi(sana);

        time(&aika_save);
        i=hae_apu("cursor",sana);  // 10.3.1995
        if (i)
            {
            split(sana,osa,2);
            for (i=0; i<2; ++i) cur_par[i]=atoi(osa[i]);
            }
        *eopen=EOS;
        if (tunnus) *etufile='-';
        *tut_info=EOS;
        shad_off=0;
        for (i=0; i<256; ++i) shad_active[i]=(unsigned char)i;
        space_break2=1; i=hae_apu("space_break",sana);
        if (i) space_break2=atoi(sana);
        *sucropath=EOS; i=hae_apu("sucropath",sucropath);
        if (i) // 1.12.2007
           {
           i=strlen(sucropath)-1;
           if (sucropath[i]!='\\') strcat(sucropath,"\\"); // 26.7.2005
           }
        *break_sucro=EOS; i=hae_apu("break_sucro",break_sucro);
        i=hae_apu("pre_ctnue",sana); if (i) pre_ctnue=atoi(sana);
        i=hae_apu("loudness",sana); if (i) loudness=atoi(sana);
        i=hae_apu("s84_warning",sana); if (i) s84_warning=atoi(sana);
//      spawn_max=0; i=hae_apu("spawn_max",sana);
//      if (i) spawn_max=atoi(sana);
        child_call=0; i=hae_apu("child_call",sana);
        if (i) child_call=atoi(sana);
        i=hae_apu("output_level",sana); if (i) output_level=atoi(sana);

        mat_parser=0;
        i=hae_apu("mat_parser",sana); if (i) mat_parser=atoi(sana);
        i=hae_apu("help_window",sana); if (i) help_window=atoi(sana);
        i=hae_apu("gplot_layout",gplot_layout);
        if (!i) *gplot_layout=EOS;
        if (i)
        	{
        	apu=muste_fopen2(gplot_layout,"rt");
        	if (apu==NULL)
        		{
        		sprintf(sbuf,"<Survo>/SYS/%s",gplot_layout);
        		apu=muste_fopen2(sbuf,"rt");
        		if (apu==NULL) *gplot_layout=EOS;
        		else strcpy(gplot_layout,sbuf);
        		}
        	muste_fclose(apu);	
        	}        
        
        hae_apu("videomode",videomode);

        key_sleep=0;
        i=hae_apu("key_sleep",sana); if (i) key_sleep=atoi(sana);

        i=hae_apu("display_keys",sana); if (i) display_keys=atoi(sana);
        i=hae_apu("exit_warning",sana); if (i) exit_warning=atoi(sana);
        i=hae_apu("save_warning",sana); if (i) save_warning=atoi(sana);
        tut_wait_c=10;
        i=hae_apu("sucro_speed",sana); if (i) tut_wait_c=atoi(sana);

        i=hae_apu("search_caps",sana); if (i) search_caps=atoi(sana);
                                               // 20.4.2002
        tmp_by_session=0;
        i=hae_apu("session_tmp",sana); if (i) tmp_by_session=atoi(sana);
        del_tmp=1;
        i=hae_apu("del_tmp",sana); if (i) del_tmp=atoi(sana);
        spec_check=0; // 1.6.2004
        i=hae_apu("spec_check",sana); if (i) spec_check=atoi(sana);
        show_lines=0; // 13.4.2006
        i=hae_apu("show_lines",sana); if (i) show_lines=atoi(sana);

        minus_paths=0; // 30.9.2010
        i=hae_apu("minus_paths",sana); if (i) minus_paths=atoi(sana);

        survopoint='~'; // RS CHA EOS; // 15.3.2012
        i=hae_apu("Survopoint",sana); if (i) survopoint=*sana;
        survopoint_on=0;
        i=hae_apu("Survopoint_on",sana); if (i) survopoint_on=atoi(sana);
        survopoint_disp=30; // RS CHA 0 -> 30
        i=hae_apu("Survopoint_disp",sana); if (i) survopoint_disp=atoi(sana);

        kb_mouse_start='-'; // 10.11.2010
        i=hae_apu("kb_mouse_start",sana);
        if (i && *sana!=EOS) kb_mouse_start=*sana;

        fence_save=1; // 21.5.2010
        i=hae_apu("fence_save",sana); if (i) fence_save=atoi(sana);

        fence_warning=1;  // 21.4.2010
        i=hae_apu("fence_warning",sana); if (i) fence_warning=atoi(sana);

        n_fence_stop=0; // RS ADD
        i=hae_apu("fence_stop",sbuf); if (i) make_fence_stop_list(sbuf);

        strcpy(google,"http://www.google.com/search?q=");
        hae_apu("google",google); // 10.4.2008

        strcpy(wiki,"http://en.wikipedia.org/wiki/");
        hae_apu("wiki",wiki); // 12.4.2008

        strcpy(dict1,"http://www.thefreedictionary.com/");
        dict2='+';
        i=hae_apu("dict",dict1);
        if (i)
            {
            dict2='+';
            p=strchr(dict1,',');
            if (p!=NULL) { *p=EOS; dict2=*(p+1); }
            }

/* RS NYI
        if (!act_sounds_on) // 14.10.2005
            {
            i=hae_apu("act_sounds",act_sounds);
            if (i)
                {
                act_sounds_on=1;
                split(act_sounds,act_sound,3);
                }
            }
*/
        soft_vis=1;


// Rprintf("\ninfo_s=%s|",info_s);

        language=crt_exit; // 1.6.2001

        *language=EOS; strncat(language,space,31); language[30]='X';
        *language='0'; // RS ADD
        p=strchr(info_s,'~');
        if (p!=NULL)
            {
            if (*(p+1)=='E') *language='2';
            else if (*(p+1)=='D') *language='1';
            else *language='0';
            }

        hae_apu("language",language);

        check_stack=1000000L;
        i=hae_apu("check_stack",sana); if (i) check_stack=atol(sana);
        sprintf(sbuf,"%s*.TMP",etmpd);
        sur_delete(sbuf);

        if (!tmp_by_session)  { i=sys_save_restore(1); if (i<0) return(-1); } // 26.2.2001

        return(1);
        }

void s_perusinit() // RS
{
	muste_startpath=s_muste_startpath;
    edisk=s_edisk;
    esysd=s_esysd;
    eout=s_eout;
    muste_rout=s_muste_rout;
    etufile=s_etufile;
    sapu=s_sapu;
    info=s_info;
    active_data=s_active_data;
    shadow_int=s_shadow_int;
    shadow_code=s_shadow_code;
    tut_info=s_tut_info;
    tut_info2=s_tut_info2;
    crt_exit=s_crt_exit;
    etmpd=pp_etmpd=s_etmpd;
    psur_seed=&sur_seed;
    sspace=space;
    eopen=s_eopen;
    survo_path=s_survo_path;
    system_name=s_system_name;
    cur_par=s_cur_par;
    shad_active=s_shad_active;
    info_2=s_info_2;
    op=s_op;
    muste_clipfile=s_muste_clipfile; // RS ADD 28.9.2012
    muste_command=s_muste_command; // RS ADD 28.9.2012
}

static void muste_variableinit() {

s_perusinit();

line_labels_off=0; /* 25.9.1994 */
ver_disp=0;
paint_on=0; /* 17.11.1996 */

move_ind=0; /* Reset block definitions */
move_words=0;
save_84ed=0;
redim_save=0;
s84_warning=0; /* 1.10.1997 */
exit_warning=1;
save_warning=1;

rsh=0;
insert_mode=0;
large_field=0;
autosave=0;
autosave32=0;
autosavefield=0;
special_code=0;
numtab=0;
vnumtab=0; /* 28.10.1994 */
m_move_ind=0; // no mouse right button pressed!
m_move_ind2=0;
dispm=0;
type_without_limit=0;
// special;
// prevkey;
// edrun;
// nleft;
cursor_step=4;
left_edge=1;
ref_c1=0;
soft_vis=1;
etuu=0; /* 1=lapset eivät TUT-moodissa 0=lapset TUT-moodissa */
act_sounds_on=0; // 0=ei käytössä, 1=off, 2=on   14.10.2005
move_from_store=0;
wait_tut_type=0; // 1=cancelled by user's actions 2=activates always
own_spec_line1=0;  // 20.12.2010
own_spec_line2=0;

first_word_on_line_search=0; // RS ADD

survo_type='4'; // RS muste=type 4
muste_lopetus=FALSE;


/* RS FIXME Include variable initialization

int arguc=2;
char *arguv[]={ "A","A","A" };

int n_fence_stop;
char fence_stop_list[MAX_FENCE_STOP][2][16];
int fence_save; // 21.5.2010
int fence_warning;  // 21.4.2010
static char fence1[]="*##########";

int own_spec_line1=0;  // 20.12.2010
int own_spec_line2=0;

int first_word_on_line_search;

int mouse_keys; // 30.10.2010

int muste_lopetus;

char *z;
int ed1,ed2,edshad;
int r,r1,r2,r3,c,c1,c2,c3;
char s_edisk[LNAME], s_esysd[LNAME], s_eout[LNAME];

char last_disk[3];
char ediskpath[LNAME], survo_path16[LNAME];
// char edisk[LNAME], esysd[LNAME], eopen[LNAME], eout[LNAME];
char *edisk, *esysd, *eout;
char s_muste_startpath[LNAME];
char *muste_startpath;

int etu; // RS TUT
int etuu; // 1=lapset eivät TUT-moodissa 0=lapset TUT-moodissa
int alkututor; // RS TUT
int wait_tut_type=0; // RS TUT // 1=cancelled by user's actions 2=activates always
long wait_hetki;
int tut_index=0;

char id_jatko[3];

char s_etufile[LNAME];
char *etufile;
int etu1,etu2,etu3; // RS TUT 
long tutpos; // RS TUT 
int r_pause,r1_pause,c_pause,c1_pause; // 21.8.2004 // RS TUT
int sucro_pause=0; // RS TUT

int r_mouse,c_mouse;
int help_window;
char videomode[32];
int search_caps; // 20.4.2002
int help_window_open;
char gplot_layout[LNAME];
int show_lines; // 13.4.2006

char *language;

int *zs,zshn;
int erun;
int erun_start=0; // 1 ensimm?isell?, 0 seuraavilla komennoilla
int edisp; // 1=screen redisplayed (default)  2=only current line redisplayed

int child_wait=0;
int pre_ctnue=0;
int child_call=0;
int key_sleep=0;
int display_keys=0; // 1.8.2000 1=näytä nappien koodit riv. 23
int tmp_by_session=0;
int del_tmp=1;
long check_stack=0;



char s_sapu[MAXTILA+2];
char *sapu; // RS

char s_info[LLENGTH];
char *info;
char *key_label[256];
char key_lab[LENLABEL*MAXLABEL];
char nimeton[]="        ";
char ser_number[LNAME];
char **disp_string; 
int speclist, specmax;
char s_active_data[LNAME];
char *active_data;
int scale_check;
int accuracy, results;
int ibm;
int s_shadow_int[10];
int *shadow_int;
unsigned char s_shadow_code[256];
unsigned char *shadow_code;
char s_tut_info[LLENGTH];
char *tut_info;
char s_tut_info2[LLENGTH];  // RS toimiiko?!?
char *tut_info2;            // Vaihdettu pointteriksi
char s_crt_exit[32];
char *crt_exit;
int sdisp;
int scroll_line;
int space_break2;
int space_break;
int ntut=0; // RS TUT 
int move_r1,move_r2;
char s_etmpd[LNAME];
char *etmpd,*pp_etmpd;
int sur_seed;
int *psur_seed;
char *sspace;
int computation_speed;

char s_eopen[LNAME];
char *eopen;

char s_survo_path[LNAME];
char *survo_path;

int display_off;
int sur_alarm;

char s_system_name[256] = "Muste";
char *system_name;

char window_name[128];
char window_name2[8];

int s_cur_par[2];
int *cur_par;
int shad_off;
char s_shad_active[256];
char *shad_active;
int tut_wait_c;
long cpu_speed;
int wait_save;
char survo_type;
int loudness;
int output_level;
int mat_parser;
char os_ver[10]; 
char s_info_2[LLENGTH];
char *info_2;
char info_s[64];
int spec_check; 


unsigned char shadow_code_tila[256];
char space[LLENGTH+1], stripe[LLENGTH];
char op_sana[10],help_sana[10];

char *prompt_line;
int insert_type;
int ins_lines_on;
int line_labels_off=0; 
int ver_disp=0;
int paint_on=0; 

int move_ind=0;
int mr,mc,mr1,mc1,mr2,mc2;
int move_words=0;
int move_r1,move_r2;

int ued1,ued2,uedshad;

FILE *edfield;
FILE *apu;
FILE *survoxxx;

char edit_file32[LNAME];
int save_84ed=0;
int redim_save=0;
static unsigned char rivi [10*LLENGTH]; // RS ADD
char rivin_loppu[]="\15\12";
int s84_warning=0; 
int exit_warning=1;
int save_warning=1;

char sbuf[LLENGTH*3];

int g;
char *spl;
int global;
int r_soft;
int v_results,v_accuracy;
char comline[LLENGTH];
char *word[MAXPARM];
char *parm[MAXPARM];
char sur_session[2];
char prompt_shadow='1';
unsigned int rsh=0, zsrsh;
char sh_vara[LLENGTH];
int insert_mode=0;
int large_field=0;
char pref=' ';
int autosave=0;
int autosave32=0;
int autosavefield=0;
int special_code=0;

int numtab=0;
int vnumtab=0; 
char deleted_line[LLENGTH]; 

char s_op[MAXARG+4];
char *op;
char prompt_space[EC3+9];

char survoblo[LNAME];
char survowrd[LNAME];

int m_move_ind=0; // no mouse right button pressed!
int m_move_r1,m_mc1;
int m_move_ind2=0;

time_t aika_save;
char aika[26];


char *splist;
char **spa, **spb, **spshad;
int spn;
double *arvo; 
char **spb2;   
char *spp;
unsigned int *spplace;

int dispm=0;
int type_without_limit=0;
int special;
int prevkey;
int edrun;
int nleft;
int cursor_step=4; 
int left_edge=1;
int ref_c1=1, ref_c=1, ref_r1=1, ref_r=1;
int ref1_line=1; // 26.11.2009 defined by F2 - and loaded by alt-F5 - ENTER

static char *zz;


int help_window;


int act_sounds_on=0; // 0=ei käytössä, 1=off, 2=on   14.10.2005

static int n_save;
       int c_vasen,c_oikea;
static int r_alku,r_loppu;
static int c_vasen1,c_oikea1;
static int r_alku1,r_loppu1;
static int poistetut_merkit;
static int marg_ero;
static char trim_command[LNAME];
static int move_from_store=0;

int goto_load_ind=0;

int child_call2=0;
int child_call0=0;

char OO[OPLEN+1];     
char op_tila[OPLEN+1];
char op2[OPLEN+1];
char *op_plot_word;

// RS REM char sur_session[2];
int keysum=0;
int soft_vis=1;
int soft_key_activated=0;
int soft_act=0;
int soft_act2; // op_arit() varten!
int mouse_refresh=0; // 10.5.2008

char soft_actline[LLENGTH];
char actline[LLENGTH];
char sucropath[LNAME];
char break_sucro[LNAME];
char survo_id[128];
char qpath[LLENGTH]; 
char orig_setup[LNAME], current_setup[LNAME];
char wait_tut_name[32];


int ver;

int medit=0; // 30.4.2003
int medit_r1; // 5.6.2003

*/


}


extern void muste_initstack();

int muste_editor(char *argv)  // RS oli parametrit: int argc; char *argv[];
        {

        unsigned int i;
        char x[LLENGTH], x1[LLENGTH];
// RS REM        int m=0;
        int k;
// RS REM        char *p; 

        write_string("Initializing Muste...",21,'1',2,3);        
        LOCATE(4,13); PR_EINV; // RS ADD

        muste_variableinit(); // RS
        muste_initstack();


// RS NYI        sek_aika(0); // aloitusaika (spre.c)

        strcpy(muste_startpath,muste_getwd()); // RS save current (start) dir
        strcpy(survo_path,muste_getmustepath());  // RS current dir for survo_path
        strcpy(survo_path16,survo_path); // RS Only one dir used (no U in use)



/* RS CHA REM
        strcpy(survo_path,argv[0]);
        sur_get_short_name(survo_path);
        i=strlen(survo_path)-1;
        while (survo_path[i]!='\\' && survo_path[i]!=':') survo_path[i--]=EOS;

        strcpy(survo_path16,survo_path);
        i=strlen(survo_path16)-3;
        if (muste_strcmpi(survo_path16+i,"\\U\\")==0 )
            survo_path16[i+1]=EOS;
*/

        *orig_setup=EOS;  // RS Alustetaan varmuuden vuoksi
        if (*orig_setup==EOS)
            {
            strcpy(orig_setup,survo_path); strcat(orig_setup,"SURVO.APU");
            }

        strcpy(current_setup,orig_setup);

/* RS NYI   - Ei eri SURVO.APUa tai aloitussukroa
        if (argc>1 && muste_strnicmp(argv[1],"/S:",3)==0)
            {
            strcpy(orig_setup,argv[1]+3); survoapu=0;
            --argc; argv[1]=argv[2];
            }
        alkututor=argc-1;
*/

	    alkututor=0; // RS CHA alkusukro=MUSTE.STA aloitushakemistossa
		sprintf(sbuf,"%sMUSTE.STA",muste_startpath);
		if (sur_find_file(sbuf)) alkututor=1; // RS ADD

        k=muste_editor_init(orig_setup,1); if (k<0) return(-1);
        edrun=1;

/* RS REM Nykyisellään turhia?
        g=2; parm[1]=survo_path; op_cd();
        sur_console_init(system_name);
        g=1; op_cd();
*/

        strcpy(ediskpath,edisk);

/* RS REM Järjestelmätarkistukset voinee jättää pois
        sur_os_version(os_ver);
        sur_ctrl_handling();
// Rprintf("\nos_ver=%s",os_ver);
        if (strncmp(os_ver,"WIN9",4)!=0)
            {
            char *s[3];

            xp_font=20; xp_xpos=xp_ypos=0;
            i=hae_apu("window",x); // 6.7.2006
            if (i)
                {
                i=split(x,s,3);
                xp_font=atoi(s[0]);
                if (i>=3) { xp_xpos=atoi(s[1]); xp_ypos=atoi(s[2]); }
                }
            set_regkeys(xp_font,xp_xpos,xp_ypos); // 5.8.2000 6.7.2006
            }
*/
        LOCATE(3,13); PR_EINV; // RS
        editor_labels();

//		valitse_kieli("2"); // RS English
        kielenvalinta();    

        soft_keys_init();
        set_sur_session();
        if (r_soft)
         {
         muste_resize(c3+8,r3+2+r_soft+1); // RS CHA sur_resize1(c3+8,r3+2+r_soft+1);
         }

        set_console_title();      
        disp_all();  // RS        
        
        if (!alkututor)
            {
            i=hae_apu("start_sucro",x1);   // 14.7.1992
                                           // x1 säilytettävä -> XXXX alla 
            if (i)
                {
                alkututor=-1;
                }
            else
                {
                if (*survo_path) strcpy(x,survo_path);
                else strcpy(x,esysd);
                strcat(x,"START.EDT");
                edload(x,1); disp_all();
                }
            }

// LOCATE(5,5); PR_EINV; scroll_line=3; sur_print("XXXXXXXXX"); getch();
// Rprintf("\nsurvo_path=%s| esysd=%s|",survo_path,esysd); getch();

// RS        set_window();

            if (alkututor)
                {

FILE *apu;   // RS ADD
	sprintf(sbuf,"%s/MUSTE.STA",muste_startpath);
    apu=muste_fopen2(sbuf,"rt");
    yys(sbuf);
    muste_fclose(apu);
    snprintf(start_etufile,LNAME,"%s %s","alkutut",sbuf);

	g=split(start_etufile,parm,10); // RS CHA 
/* RS CHA
                g=2; parm[1]=start_etufile; // RS CHA argv[1] -> argv;

                if (parm[1]!=NULL) // 21.11.2002
                    {
                    while (1)
                        {
                        p=strchr(parm[g-1],'|');
                        if (p==NULL) break;
                        *p=EOS; parm[g++]=p+1;
                        }
                    }
*/
//Rprintf("\nalkututor=%d parm1=%s|",alkututor,parm[1]);

                if (alkututor==-1) parm[1]=x1;  //  XXXX 14.7.92 
// RS REM                else strcpy(edisk,esysd);
                op_tutor();
                alkututor=0;
                }            
                
        return(1);
        }

/* wfind.c 16.7.85/SM (28.9.85)
   wfind(word1,word2,lin) etsii sanaparin rivin alusta riviltä lin lähtien
*/
int wfind(char word1[], char word2[], int lin)
        {
        short j,g;
        char x[LLENGTH];
        char *sana[2];

        for (j=lin; j<=ed2; ++j)
                {
                edread(x,j);
                g=split(x+1,sana,2);
                if (g==0) continue;
                if (strcmp(word1,sana[0])==0)
                        {
                        if (word2==NULL) break;
                        if (g==2 && strcmp(word2,sana[1])==0) break;
                        }
                }
        if (j>ed2) return (-1);
        return (j);
        }

/* itoa:  convert n to characters in s */
char *muste_itoa(int n, char s[], int base)
{
    int c, i, j, sign;

    if ((sign = n) < 0)  /* record sign */
        n = -n;          /* make n positive */
    i = 0;
    do         /* generate digits in reverse order */
    {
        s[i++] = n % base + '0';   /* get next digit */
    }
    while ((n /= base) > 0);     /* delete it */
    if (sign < 0)
        s[i++] = '-';
    s[i] = '\0';

    for (i = 0, j = strlen(s)-1; i<j; i++, j--)
    {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
    }

    return s;
}


/* fconv.c 17.8.1985/SM (9.11.1989)
   fconvd(luku,muoto,sana);
          double char[] char[]
   muuntaa luvun sanaksi käyttäen muotoa muoto="1234.123" tai "%8.3f" esim.
                                         muoto="" tarkoittaa vapaata.
   return -1, jos sana ei mahdu muotoon.
*/
int fconv(double luku,char muoto[],char sana[])
{
    /*        char x[256]; */
    char *p;
    int len=strlen(muoto);
    int dec;
    /*      int kok; */

    if (*muoto==EOS)
    {
        if (fabs(luku)>=1 || fabs(luku)<1e-14)
            sprintf(sana,"%.14g",luku);
        else
        {
            if (luku==0)
            {
                strcpy(sana,"0");
                return(1);
            }
            sprintf(sana,"%.14f",luku);
            len=strlen(sana)-1;
            if (strchr(sana,'.')!=NULL)  /* 9.11.89 */
            {
                while (sana[len]=='0') sana[len--]=EOS;
                if (sana[len]=='.') sana[len]=EOS;
            }
        }
        return(1);
    }

    if (*muoto=='%')
    {
        sprintf(sana,muoto,luku);
        return(1);
    }

    p=strchr(muoto,'.');
    if (p==NULL) dec=0;
    else dec=len-1-(p-muoto);
    sprintf(sana,"%*.*f",len,dec,luku);
    if (strlen(sana)>len) return(-1);
    return(1);
}

int fconv0(double luku,char muoto[],char sana[])
        {
        if (fabs(luku)<1e-15) luku=0.0;
        return(fconv(luku,muoto,sana));
        }

/* fnconv.c 29.3.1986/SM (29.3.1986)
   fnconvd(luku,pituus,sana);
          double int   *char
   muuntaa luvun sanaksi, jolla on kiinteä pituus.
*/
int fnconv(double luku,int pituus,char *sana)
{
    int i;
    int kok=2;
    int des;
    double its;
    int logl;
    char muoto[LLENGTH];

    des=pituus-3;
    if (luku!=0.0)
    {
        its=luku;
        if (luku<0.0) its=-luku;
        logl=(int)log10(its);
        if (logl>0)
        {
            kok=logl+2;
            des=pituus-kok-1;
        }
        if (kok>pituus)
        {
            fconv(luku,"%g",sana);
            return(1);
        }
    }
    for (i=0; i<kok; ++i) muoto[i]='#';
    if (kok<pituus)
    {
        if (kok+1==pituus) muoto[i++]='#';
        else
        {
            muoto[i++]='.';
            for (; i<pituus; ++i) muoto[i]='#';
        }
    }
    muoto[i]=EOS;
    fconv(luku,muoto,sana);
    return(1);
}

int linerr2(char sana[])
{
    /*
              PR_EINV;
              sprintf(sbuf,"Line '%s' not found!",sana);
              if (dsp) return(1);
              sur_print("\n"); sur_print(sbuf);
              WAIT;
    */
    return(1);
}

int lastline2()
{
    int i,j;
    char space[LLENGTH];

    for (i=0; i<ed1; ++i) space[i]=' ';
    j=ed2;
    while (j>0)
    {
        if (strncmp(space,z+(j-1)*ed1+1,(unsigned int)(ed1-1))!=0) break;
        --j;
    }
    return(j);
}

int posnro(char s[])
{
    short h;
    if (*s=='0') return(0);
    for (h=0; *(s+h); ++h)
        if ((*(s+h)<'0') || (*(s+h)>'9')) return(0);
    return(1);
}

int edline2(char sana[],unsigned int lin,int virheilm)
/* lin alkurivi */
/* virheilm 1=virheilmoitus 0=ei virheilmoitusta */
{
    int j,k;
    char SANA[3];
    int lin1;    /* 5.3.91 */

    if (posnro(sana))
    {
        j=atoi(sana);
        if (j>=lin && j<=ed2) return(j);
        linerr2(sana);
        return(0);
    }
    for (j=0; j<3; ++j) SANA[j]=toupper(sana[j]);
    if (strncmp(SANA,"END",3)==0)
    {
        k=3;
        j=lastline2();
    }
    else if (strncmp(SANA,"CUR",3)==0)
    {
        k=3;
        j=r1+r-1;
    }
    else
    {
    	if (SANA[1]=='>') lin1=r1+r-1;
    	else
    		{
        	lin1=lin-atoi(sana+1);
        	if (lin1<1) lin1=1;
        	}
        for (j=lin1; j<=ed2; ++j)
            if ( *(z+(j-1)*ed1)==*sana ) break;
        if (j>ed2)
        {
            if (virheilm) linerr2(sana);
            return(0);
        }
        k=1;
    }
    if (strlen(sana)<k+2) return(j);
    j+=atoi(sana+k);
    if (j>=lin && j<=ed2) return(j);
    if (virheilm) linerr2(sana);
    return(0);
}


int hae_apu(char *s,char *t)
{
    char *p, *q;
    char sana[64];  /* 11.5.2006 */
    int len;

    strncpy(sana,s,63); // RS CHA strcpy -> strncpy

	if (strcmp(sana,"sysname")==0) // RS ADD
		{
  		muste_get_R_string(t,".muste$sysname",64); muste_strlwr(t);
//		sprintf(t,"windows");
  		return(1);
		}

/*
	if (strcmp(sana,"R_path")) // RS ADD
		{
  		muste_get_R_string(t,".muste$Rhome",LNAME);
  		return(1);
		}  
*/		  
    
    strcat(sana,"=");
    len=strlen(sana);
    p=sapu;

    while ((p=strchr(p,*sana))!=NULL)
    {
        if (strncmp(sana,p,(unsigned int)len)==0)
        {
            if (p>sapu && *(p-1)!='\n')
            {
                ++p;    /* 14.7.2006 */
                continue;
            }
            p+=len;
            q=t;
            while (*p && *p!='\n')
            {
                *q=*p;
                ++q;
                ++p;
            }
            *q=EOS;
                p=strchr(t,'<'); // 17.10.2001
                if (p!=NULL)
                    {
                    subst_survo_path_in_editor(t);
                    }

            return(1);
        }
        ++p;
    }
    return(0);
}



void not_space_for_param()
        {
        sur_print("\nNot space enough for this system parameter value!");
        WAIT; return;
        }

/* Macro codes already in use */
static char macros[]="AaBbCcDdEeghIiJjLlMmOoPpQqRrSsTtuvWwXxy=@!;&%^/";

int survoapu1(int h,char *s)
        {
        int i,i0;
        char x[LLENGTH], y[LLENGTH];
        char *p,*q,*q2;
        char *sana[2];
        char par[LLENGTH];
        int maxtila,varattu;
        int len1,len2;

        if (!h) edread(x,r1+r-1); else strcpy(x,s);
        i=split(x+1,sana,2);
        if (i<2)
            {
            sur_print("\nUsage: SYSTEM <system_word>=<value>");
            WAIT; return(1);
            }
        p=sana[1];

        if (strcmp(p,"SAVE")==0) { sys_save_restore(1); return(1); }
        if (strcmp(p,"RESTORE")==0) { sys_save_restore(2); return(1); }

        if (muste_strnicmp(p,"ALARM=",6)==0) sur_alarm=0;

        if (!h) edread(x,r1+r-1); else strcpy(x,s);

        q=sapu;
        while ( ((unsigned char)*q!=(unsigned char)254 ||     // RS CHECK '_' -> 254???
                 (unsigned char)*(q+1)!=(unsigned char)254) &&
                 (int)(q-sapu)<16000 ) ++q;

// RS REM        if ((int)(q-sapu)>=16000) { Rprintf("???"); getck(); }
        maxtila=q-sapu;
        varattu=strlen(sapu);
        q=strchr(p,'=');
        if (q==NULL)
            {
            q=strchr(sana[1],' '); if (q!=NULL) *q=EOS;
            i=hae_apu(sana[1],y);
            if (i)
                {
                *(p+strlen(sana[1]))=EOS; strcat(x,"="); strcat(x,y);
                edwrite(space,r1+r-1,0);
                edwrite(x,r1+r-1,0);
                }
            else
                {
                if (etu==0)
                    {
                    sprintf(sbuf,"\nSystem word `%s' not defined!",sana[1]);
                    sur_print(sbuf); WAIT; return(1);
                    }
                }
            return(1);
            }
        strcpy(par,q+1);
        *(q+1)=EOS;
        q=strstr(par," / ");
        if (q!=NULL) *q=EOS;
        len2=strlen(par); while (par[len2-1]==' ') par[--len2]=EOS;

        if (*p=='M' && strlen(p)==3)     /* Mc= */
            {
            if (strchr(macros,p[1])!=NULL)
                {
          sprintf(sbuf,"\nKey combination PREFIX %c has a predetermined function!",p[1]);
                sur_print(sbuf); WAIT; return(1);
                }
            }
        q=strstr(sapu,p);
        if (q!=NULL)
            {
            q+=strlen(p);
            q2=q; while (*q2 && *q2!='\n') ++q2;
            len1=q2-q;
            if (len2<=len1)
                {
                for (i=0; i<len2; ++i) q[i]=par[i];
                if (len2<len1)
                    {
                    while (q[i+len1-len2]) { q[i]=q[i+len1-len2]; ++i; }
                    q[i]=EOS;
                    }
                }
            else
                {
                if (maxtila-varattu<=len2-len1)
                    {
                    not_space_for_param(); return(1);
                    }
                i=varattu; i0=q-sapu;
                while (i>i0) { sapu[i-1+len2-len1]=sapu[i-1]; --i; }
                for (i=0; i<len2; ++i) q[i]=par[i];
                }
            }
        else
            {
            if (maxtila-varattu<=strlen(p)+len2+2)
                    {
                    not_space_for_param(); return(1);
                    }
            sprintf(sbuf,"%s%s\n",p,par);
            strcat(sapu,sbuf);
            }

        if (strcmp(p,"speclist=")==0) speclist=atoi(par);
        if (strcmp(p,"specmax=")==0) specmax=atoi(par);
        if (strcmp(p,"scale_check=")==0) scale_check=atoi(par);
        if (strcmp(p,"accuracy=")==0) accuracy=atoi(par);
        if (strcmp(p,"results=")==0) results=atoi(par);
        if (strcmp(p,"output_level=")==0) output_level=atoi(par);
        if (strcmp(p,"mat_parser=")==0) mat_parser=atoi(par);
        if (strcmp(p,"help_window=")==0) help_window=atoi(par);
        if (strcmp(p,"videomode=")==0) strcpy(videomode,par);
        if (strcmp(p,"search_caps=")==0) search_caps=atoi(par);
        if (strcmp(p,"spec_check=")==0) spec_check=atoi(par);
        if (strcmp(p,"show_lines=")==0) show_lines=atoi(par);

        if (strcmp(p,"cursor=")==0)
            {
            i=split(par,sana,2);
            if (i>0) cur_par[0]=atoi(sana[0]);
            if (i>1) cur_par[1]=atoi(sana[1]);
            }

        return(1);
        }


static FILE *temp_apu;
static char sys_str1[256];

int sys_save_restore(int k) // 1=SAVE 2=RESTORE
    {
//RS    extern char *parm[];
//RS    extern char sapu[];
    int rr3,cc3;
    int i;
    char buffer[LLENGTH]; // RS ADD
    extern int muste_fclose2();

    strcpy(sbuf,etmpd); strcat(sbuf,"SUR_SYS.SYS");
    if (k==1)
        {
        temp_apu=muste_fopen2(sbuf,"wt");
        if (temp_apu==NULL)
        	{
        	sprintf(buffer,"\nCannot open %s in read/write mode!",sbuf);
        	sur_print(buffer);
        	WAIT;
        	return(-1);
        	}        	
        fprintf(temp_apu,"%s\n",videomode);
        fprintf(temp_apu,"%d\n",accuracy);
        fprintf(temp_apu,"%d\n",search_caps);
        fprintf(temp_apu,"%d\n",help_window);
        strcpy(sbuf,gplot_layout);
        if (*sbuf==EOS) strcpy(sbuf,"-");
        for (i=0; i<strlen(sbuf); i++) if (sbuf[i]==' ') sbuf[i]='*'; // RS ADD
        fprintf(temp_apu,"%s\n",sbuf);
        strcpy(sbuf,eout); if (*sbuf==EOS) strcpy(sbuf,"-");
        for (i=0; i<strlen(sbuf); i++) if (sbuf[i]==' ') sbuf[i]='*';  // RS ADD      
        fprintf(temp_apu,"%s\n",sbuf);
        fprintf(temp_apu,"%d %d\n",r3,c3);
        i=hae_apu("color98",sbuf); if (i) i=atoi(sbuf); else i=0;
        fprintf(temp_apu,"%d\n",i);
        }
     else
        {
        temp_apu=muste_fopen2(sbuf,"rt");
        if (temp_apu==NULL)
        	{
        	sprintf(buffer,"\nCannot open %s for reading!",sbuf);
        	sur_print(buffer);
        	WAIT;
        	return(-1);
        	} 
        if (temp_apu==NULL) return(1);
        fscanf(temp_apu,"%s\n",videomode);
        sprintf(sbuf,"SYSTEM videomode=%s",videomode);
        survoapu1(1,sbuf);
        fscanf(temp_apu,"%d\n",&accuracy);
        fscanf(temp_apu,"%d\n",&search_caps);
        fscanf(temp_apu,"%d\n",&help_window);
        fscanf(temp_apu,"%s\n",gplot_layout);
        if (*gplot_layout=='-') *gplot_layout=EOS;
        for (i=0; i<strlen(gplot_layout); i++) if (gplot_layout[i]=='*') gplot_layout[i]=' '; // RS ADD
        fscanf(temp_apu,"%s\n",sbuf);
        if (strcmp(sbuf,"-")==0) *sbuf=EOS;
        for (i=0; i<strlen(sbuf); i++) if (sbuf[i]=='*') sbuf[i]=' ';  // RS ADD       
        strcpy(eout,sbuf);
        fscanf(temp_apu,"%d %d\n",&rr3,&cc3);
        if (rr3!=r3 || cc3!=c3)
           {
           sprintf(sys_str1,"%d,%d",rr3,cc3);
           split(sys_str1,parm+1);
           g=3; op_resize();
           }
        fscanf(temp_apu,"%d\n",&i);
        sprintf(sbuf,"SYSTEM color98=%d",i);
        survoapu1(1,sbuf);
        muste_set_sysname(); // RS
        }

    muste_fclose2(temp_apu);

    return(1);
    }


void edread(char *x,unsigned int lin)
{
    strncpy(x,z+(lin-1)*ed1,(unsigned int)ed1);
    x[ed1]=EOS;
}

int edwrite(char *x,unsigned int lin,unsigned int col)
{
    unsigned int i,h;
    unsigned int len=strlen(x);

    if (lin<1 || lin>(unsigned int)(ed2+edshad))
    {
        sprintf(sbuf,"Line number error! (%u)\n",lin);
        /*            if (dsp) return(1);    */
        sur_print(sbuf);
        WAIT;
        return(1);
    }
    if (len>ed1-col) len=ed1-col;
    for (i=0, h=(lin-1)*ed1+col; i<len; ++i, ++h) z[h]=x[i];
    return(1);
}

int split(char *rivi,char **sana,int max)
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
{
    int g=0;
    int p;
    int edell=0; /* väli edellä */
    int len=strlen(rivi);

	for (p=0; p<max; p++) sana[p]=muste_nullstring; // RS ADD

    for (p=0; p<len; ++p)
    {

        if ( (rivi[p]==' ') || (rivi[p]==',') )
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
                sana[g]=rivi+p;
                edell=1;
            }
        }
    }
    if (edell==1) ++g;

    
    return(g);
}

int splitq(char *rivi,char **sana,int max)
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
{
    int g=0;
    int p;
    int edell=0; /* väli edellä */
    int len=strlen(rivi);
 
 	for (p=0; p<max; p++) sana[p]=muste_nullstring; // RS ADD
 
    int lainaus=0; // RS ADD Deal with spaces
    for (p=0; p<len; ++p)
    	{
    	if (rivi[p]=='"') lainaus=1-lainaus;
    	if (lainaus && rivi[p]==' ') rivi[p]='\032';
    	}

    for (p=0; p<len; ++p)
    {
    if (rivi[p]=='"') { rivi[p]=EOS; continue; }

        if ( (rivi[p]==' ') || (rivi[p]==',') )
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
                sana[g]=rivi+p;
                edell=1;
            }
        }
    }
    if (edell==1) ++g;
    
    for (p=0; p<len; ++p)
    	{
    	if (rivi[p]=='\032') rivi[p]=' ';
    	}    
    
    return(g);
}

int splitqq(char *rivi,char **sana,int max)
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
{
    int g=0;
    int p;
    int edell=0; /* väli edellä */
    int len=strlen(rivi);

	for (p=0; p<max; p++) sana[p]=muste_nullstring; // RS ADD
    
    int lainaus=0; // RS ADD Deal with spaces
    for (p=0; p<len; ++p)
    	{
    	if (rivi[p]=='"') lainaus=1-lainaus;
    	if (lainaus && rivi[p]==' ') rivi[p]='\032';
    	}

    for (p=0; p<len; ++p)
    {
//    if (rivi[p]=='"') { rivi[p]=EOS; continue; } // Don't remove quotes

        if ( (rivi[p]==' ') || (rivi[p]==',') )
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
                sana[g]=rivi+p;
                edell=1;
            }
        }
    }
    if (edell==1) ++g;
    
    for (p=0; p<len; ++p)
    	{
    	if (rivi[p]=='\032') rivi[p]=' ';
    	}    
    
    return(g);
}


int splitp(char *rivi,char **sana,int max)
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
{
    int g=0;
    int p;
    int edell=0; /* väli edellä */
    int len=strlen(rivi);
    int sulut;

	for (p=0; p<max; p++) sana[p]=muste_nullstring; // RS ADD

    sulut=0;
    for (p=0; p<len; ++p)
    {
        if (rivi[p]=='(')
        {
            ++sulut;
            continue;
        }
        if (rivi[p]==')')
        {
            --sulut;
            continue;
        }
        if (sulut) continue;
        if ( (rivi[p]==' ') || (rivi[p]==',') )
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
                sana[g]=rivi+p;
                edell=1;
            }
        }
    }
    if (edell==1) ++g;
    return(g);
}

char* muste_strrev(char* str)
	{
    int i,j,length;
    char temp;
    length=strlen(str);
    for(i=0,j=length-1; i<(length-1)/2; i++, j--)
    	{
        temp = str[i];
        str[i]=str[j];
        str[j] = temp;
    	}
    return str;
	}

int muste_strcmpi(const char *s1, const char *s2)
{
    for (; *s1 && *s2 && (toupper((unsigned char)*s1) ==
                          toupper((unsigned char)*s2)); ++s1, ++s2);

    return *s1 - *s2;
}


int muste_strnicmp(const char *s1, const char *s2, int count)
{
    char c1, c2;
    int v;

    if (count == 0) return 0;

    do {
        c1 = *s1++;
        c2 = *s2++;
        v = (unsigned int)tolower(c1) - (unsigned int)tolower(c2);
    } while ((v == 0) && (c1 != '\0') && (--count > 0));
    return v;
}

int file_name_ext(char *name,char *ext)
{
// RS REM    int i;
// RS REM    char *p;
    
    muste_append_path(name,ext); // RS CHA

/* RS REM
    i=strlen(name);
    if (i<4) p=name;
    else p=name+i-4;
    if (strchr(p,'.')==NULL) // RS Not quite general FIXME
        strcat(name,ext);
*/        
    return(1);
}

void edt_talletus(char *s)
{
    char snimi[LLENGTH];

    strcpy(snimi,s);
            
    if (strchr(s,':')==NULL && strchr(s,'\\')==NULL && strchr(s,'/')==NULL)  // RS ADD / unixpath FIXME
    { strcpy(snimi,etmpd); strcat(snimi,s); }
    edsave(snimi,1,0);   
}



/* RS Dumppi ja spesifikaatiot */

int xxd(int i)
{
    fprintf(apu,"%d\n",i);
    return(1);
}

int xxl(long li)
{
    fprintf(apu,"%ld\n",li);
    return(1);
}

int xxs(char *x)
{
    fprintf(apu,"%s\n",x);
    return(1);
}

int xxe(double a)
{
    fprintf(apu,"%.16e\n",a);
    return(1);
}

int yys(char *x)
{
    char *p;

    p=x;
    while (1)
    {
        *p=(char)getc(apu);
        if (*p=='\n') break;
        ++p;
    }
    *p=EOS;

    return(1);
}

int yyl(long *pli)
{
    char x[LLENGTH];
    yys(x);
    *pli=atol(x);
    return(1);
}

int yyu(unsigned int *pi)
{
    char x[LLENGTH];
    yys(x);
    *pi=atoi(x);
    return(1);
}

int yye(double *pa)
{
    char x[LLENGTH];
    yys(x);
    *pa=atof(x);
    return(1);
}

int yyd(int *pi)
{
    char x[LLENGTH];
    yys(x);
    *pi=atoi(x);
    return(1);
}


static void childp_dump()
        {
        if (etu>0) tut_sulje();

        if (no_wait) { vetu=etu; etu=0; }

        if (etuu) { ketu=etu; etu=0; }
//        onnistui=1;
//        sur_mouse_cursor_wait();
        }        

static void childp_restore()
		{
		
/* RS Not needed		
        if (no_wait)
            {
            no_wait=0;
            sur_sleep(300); // HELP-ikkunan avautumiseksi (sukrot)?

            i=spawnl(P_NOWAIT,opfile,opfile,siirtop,NULL);

            set_console_title();
            etu=vetu;
            if (etu>0) tut_avaa();
            return(1);
            }


// Kokeilu 9.5.2009
        if (mouse_refresh)
            {
            sur_refresh_window();
            mouse_refresh=0;
            }
*/
        set_console_title();
        soft_disp(1); // 15.2.2001
//        sur_mouse_cursor_arrow();
        if (etuu) etu=ketu;
        if (etu>0) tut_avaa();
        if (etu==1) lue_hetki(&wait_hetki);  /* 27.5.1995 */

        if (medit) medit_restore(); // 30.4.2003
        if (child_wait)
            {
            sur_wait((long)(100L*child_wait),nop,1);
            }
        }
        
int sur_dump()
{
    int i,h;
    char x[LNAME];

    results=v_results;
    accuracy=v_accuracy;

    autosave32=1;  // RS Autosave as in editor
    strcpy(x,etmpd); // RS ADD
    strcat(x,sur_session); // RS CHA
    strcat(x,"SURVOMM.EDT");
    edt_talletus(x);
    autosave32=0;


    strcpy(x,etmpd); // RS ADD
    strcat(x,sur_session); // RS CHA
    strcat(x,"SURVOMM.DMP");
    apu=muste_fopen2(x,"wt");

    xxd(ed1);
    xxd(ed2);
    xxd(r);
    xxd(r1);
    xxd(r2);
    xxd(r3);
    xxd(r_soft);
    xxd(c);
    xxd(c1);
    xxd(c2);
    xxd(c3);
    xxs(edisk);
    xxs(esysd);
    xxs(eout);
    xxd(etu);
    xxs(etufile);
    xxd(etu1);
    xxd(etu2);
    xxd(etu3);
    xxl(tutpos);
    xxd(zshn);
    xxd(erun);
    xxd(edshad);
    /* int eddisp ?? */
    xxs(info);
    /* key_label key_lab survo_id  ??? */
    xxd(speclist);
    xxd(specmax);
    xxs(active_data);
    xxd(scale_check);
    xxd(accuracy);
    xxd(results);
    /* ibm poistetaan? */
    for (i=0; i<10; ++i) xxd(shadow_int[i]);
    for (i=0; i<256; ++i)
    {
        h=shadow_code[i];
        xxd(h);
        /*          h=shad_active[i]; xxd(h); */
    }

    xxs(tut_info);
    xxs(crt_exit);
    xxd(sdisp);
    xxd(scroll_line);
    xxd(space_break);
    xxd(ntut);
    xxd(move_r1);
    xxd(move_r2);
    xxs(etmpd);
    xxd(sur_seed);
    /* space generoidaan lapsessa */
    xxd(computation_speed);
    xxs(eopen);
    xxs(survo_path);
    xxd(display_off);
    xxd(sur_alarm);
    xxs(system_name);
    xxd(cur_par[0]);
    xxd(cur_par[1]);
    xxd(shad_off);
    xxd(tut_wait_c);
    xxl(cpu_speed);
    xxd(wait_save);
    *sbuf=survo_type;
    sbuf[1]=EOS;
    xxs(sbuf);
    xxd(loudness);
    xxd(output_level);
    xxd(mat_parser);
    xxs(os_ver);
    xxs(info_2);
    xxd(spec_check);
    fprintf(apu,"%s",sapu);
    muste_fclose(apu);
    return(1);
}


int restore_dump()
{
    char x[LNAME];
    int i,h,k;
    char *p;
    /*        long li;
            int k;  */

    strcpy(x,etmpd); // RS ADD
    strcat(x,sur_session);
    strcat(x,"SURVOMM.DMP");

    apu=muste_fopen2(x,"rt");
    if (apu==NULL) return(0);
    yyd(&ed1);
    yyd(&ed2);
    yyd(&r);
    yyd(&r1);
    yyd(&r2);
    yyd(&r3);
    yyd(&r_soft);
    yyd(&c);
    yyd(&c1);
    yyd(&c2);
    yyd(&c3);
    yys(edisk);
    yys(esysd);
    yys(eout);
    yyd(&etu);
    yys(etufile);
    yyd(&etu1);
    yyd(&etu2);
    yyd(&etu3);
    yyl(&tutpos);
    yyd(&zshn);
    yyd(&erun);
    yyd(&edshad);
    yys(info);
    /* key_label key_lab survo_id  ??? */
    yyd(&speclist);
    yyd(&specmax);
    yys(active_data);
    yyd(&scale_check);
    yyd(&accuracy);
    yyd(&results);
    /* ibm poistetaan? */
    for (i=0; i<10; ++i) yyd(&shadow_int[i]);
    for (i=0; i<256; ++i)
    {
        yyd(&h);
        shadow_code[i]=h;
    }
    /*      yys(shadow_code);  po. jokainen merkki erikseen, jos tarv.? */
    yys(tut_info);
    yys(crt_exit);
    yyd(&sdisp);
    yyd(&scroll_line);
    yyd(&space_break);
    yyd(&ntut);
    yyd(&move_r1);
    yyd(&move_r2);
    yys(etmpd);
    yyd(&sur_seed);
    /* space generoidaan lapsessa */
    yyd(&computation_speed);
    yys(eopen);
    yys(survo_path);
    yyd(&display_off);
    yyd(&sur_alarm);
    yys(system_name);
    yyd(&cur_par[0]);
    yyd(&cur_par[1]);
    yyd(&shad_off);

    yyd(&tut_wait_c);
    yyl(&cpu_speed);
    yyd(&wait_save);
    yys(sbuf);
    survo_type=*sbuf;
    yyd(&loudness);
    yyd(&output_level);
    yyd(&mat_parser);
    yys(os_ver);
    yys(info_2);
    yyd(&spec_check);

    p=sapu;
    while (1)
    {
        *p=(char)getc(apu);
        if (feof(apu)) break;
        ++p;
    }
    *p=EOS;
    muste_fclose(apu);

    strcpy(x,etmpd);
    strcat(x,sur_session);
    strcat(x,"SURVOMM.EDT");
    k=etu; etu=0;
    
    edload(x,1);
    etu=k;

    /* RS FIXME Tarvitaanko      disp();
          g=2; parm[1]=edisk; op_cd(); */

    v_results=results;
    v_accuracy=accuracy;

    return(1);
}



extern void muste_save_stack_count();
extern void muste_restore_stack_count();


void muste_dump()
  { 
  childp_dump(); // RS ADD sucro handling etc.  
  sur_dump();
  muste_save_stack_count();  
  }

void muste_restore_dump()
  {  
  muste_restore_stack_count();   
  restore_dump(); 
  childp_restore(); // RS ADD sucro handling etc.
  }

extern int muste_remarks_return;
int s_edt(char *siirtop)
{
    restore_dump(sur_session); // RS CHA siirtop -> sur_session
    muste_remarks_return=FALSE;    
    return(1);
}

int s_end(char *siirtop)
{
    redim_save=1; // RS
    ued1=ed1; ued2=ed2; uedshad=edshad; // RS
    sur_dump(sur_session); // RS CHA siirtop -> sur_session
    redim_save=0; // RS
    muste_remarks_return=FALSE;   
    return(1);
}



int s_init(char *siirtop)
{
    int i;
    char *p,*q2;

/* RS CHA REM
    strcpy(sur_session,siirtop);
    *sur_session=siirtop[strlen(siirtop)-1];
    sur_session[1]=EOS;
*/

    s_perusinit();  // RS
// RS FIXME    strcpy(etmpd,siirtop); /* RS FIXME tilapäinen ratkaisu temp-tiedosto */

    s_edt(sur_session);  // RS CHA siirtop -> sur_session

    for (i=0; i<LLENGTH; ++i) space[i]=' ';
    space[LLENGTH-1]=EOS;
    edread(comline,(unsigned int)(r1+r-1));
    p=strchr(comline,STAMP); // RS CHA PREFIX -> STAMP
    if (p==NULL) p=comline;  
    q2=strstr(p,"##"); if (q2!=NULL) 
      {
      if (q2[2]!=PREFIX && q2[2]!='.' && q2[2]!=')' && q2[2]!=',') p=q2+1; // RS ADD 
      }
    
    g=splitq(p+1,word,MAXPARM);
    i=0;

    while (i<g && strcmp(word[i],"/")!=0) ++i;
    g=i;

/*
    edread(comline_org,(unsigned int)(r1+r-1));
    p=strchr(comline_org,STAMP); // RS CHA PREFIX -> STAMP
    if (p==NULL) p=comline_org;  
    q2=strstr(p,"##"); if (q2!=NULL) 
      {
      if (q2[2]!=PREFIX && q2[2]!='.' && q2[2]!=')') p=q2+1; // RS ADD 
      }
    
    g_org=split(p+1,word_org,MAXPARM);
    i=0;

    while (i<g_org && strcmp(word_org[i],"/")!=0) ++i;
    g_org=i;
*/    

    /* RS FIXME NYI       if (console) sur_console_child_init(); */
    return(1);
}

int s_init_orgsplit()
	{
	int i;
    char *p,*q2;
    
    edread(comline,(unsigned int)(r1+r-1));
    p=strchr(comline,STAMP); // RS CHA PREFIX -> STAMP
    if (p==NULL) p=comline;  
    q2=strstr(p,"##"); if (q2!=NULL) 
      {
      if (q2[2]!=PREFIX && q2[2]!='.' && q2[2]!=')' && q2[2]!=',') p=q2+1; // RS ADD       
      }
    
    g=split(p+1,word,MAXPARM);
    i=0;

    while (i<g && strcmp(word[i],"/")!=0) ++i;
    g=i;
    return(1);
    }
    
int s_init_extrasplit()
	{
	int i;
    char *p,*q2;
    
    edread(comline_org,(unsigned int)(r1+r-1));
    p=strchr(comline_org,STAMP); // RS CHA PREFIX -> STAMP
    if (p==NULL) p=comline_org;  
    q2=strstr(p,"##"); if (q2!=NULL) 
      {
      if (q2[2]!=PREFIX && q2[2]!='.' && q2[2]!=')' && q2[2]!=',') p=q2+1; // RS ADD       
      }
    
    g_org=splitqq(p+1,word_org,MAXPARM);
    i=0;

    while (i<g_org && strcmp(word_org[i],"/")!=0) ++i;
    g_org=i;
    return(1);
    }    

int sp_check()
{
    int j;
    unsigned int n,tila,tila0;
    int varjo;
    char x[LLENGTH];
    char *p,*q;
    char raja[12]="*.........."; // RS ADD
    int i,global_area; // RS ADD
 
    if (*z==' ')     /* 22.6.1998 */  /* No check if empty control char at first line */
    {
        specmax=10000;
        speclist=1000000;
        
                    if (hae_apu("specmax",x)) specmax=atoi(x);
                    else specmax=10000;
                    if (hae_apu("speclist",x)) speclist=atoi(x);
                    else speclist=1000000;
        
        /*   Rprintf("specmax=%d speclist=%d\n",specmax,speclist); getch();  */
        return(1);
    }

	global_area=TRUE; // RS ADD
    n=0;
    tila=0;
    for (j=1; j<=r2; ++j)                                       /* Loop through the edit field lines          */
    {
        edread(x,(unsigned int) j);

        if (global_area) // RS ADD
        	{
        	i=muste_instr(x,raja);
        	if (i>=0) global_area=FALSE;
        	}
        
        *x=EOS;
        p=x+1;                                                  /* Put EOS to control column                  */
        while (1)
        {
            p=strchr(p,'=');
            if (p==NULL) break;                                 /* Next line if no more  "="-marks            */
            if (*(p+1)==EOS) break;                             /* Next line if at the end of line            */
            if (*(p+1)=='=')
            {
                p+=1;                                           /* Allow "==" and longer repeats of "="       */
                continue;
            }
            /* Rprintf("C: j=%d pos=%d\n",j,p-x); getch(); */
			
			i=1; // RS ADD all i:s below added in order to count global and specs areas
			if (global_area) ++i; // RS ADD
			if (j>=own_spec_line1 && j<=own_spec_line2) ++i; // RS ADD

            //++n;                                              /* Increase the number of found specifications */
			n+=i;
			
            tila0=tila;
            varjo=zs[j];
            tila+=(2*i);                                           /* Space for two EOS                           */
            q=p-1;
            while (*q && *q!=' ')
            {
                //++tila;                                        /* Scan left until space or EOS                */
                tila+=i;
                --q;
            }
            q=p+1;
            while (*q && *q!=' ')
            {
                //++tila;                                        /* Scan right until space or EOS               */
                tila+=i;
                ++q;
            }

            while (*(q-1)=='&')                                /* Expression splitted to several lines        */
            {
                tila+=(c2*i);                                      /* Allow some extra space (bug fix?)           */
                ++j;
                if (j>r2) break;                               /* Go to next line and check if last           */
                edread(x,(unsigned int) j);
                q=x+1;
                while (*q && *q==' ') ++q;                     /* Skip spaces in the beginning of line        */
                if (*q==EOS) break;                            /* Next line if at the end of line             */
                while (*q && *q!=' ')
                {
                    //++tila;                                    /* Scan right until space or EOS               */
                    tila+=i;
                    ++q;
                }
            }
            if (varjo) tila+=((tila-tila0)*i);                       /* Double space needed if shadows in use       */
            ++p;
        }
    }
    specmax=n;                                                 /* Number of specs and needed space            */
    speclist=tila;

    return(1);
}


int spfind(char *s) /* 4.3.1995  6.7.2000 */
        {
        int i,j;

        for (i=0; i<spn; ++i)
            {
            if (spa[i]==NULL) continue;
            if (strcmp(s,spa[i])==0) break;
            }
        if (i<spn && spb[i]==NULL) return(i);
        if (i<spn && *spb[i]!='*') return(i);
        if (i==spn) return(-1);
        for (j=0; j<spn; ++j)
                if (strcmp(spb[i],spa[j])==0) return(j);
        return(i);
        }

int not_enough_mem_for_spec()
{
    sur_print("\nNot enough memory for specifications!");
    WAIT;
    return(1);
}

int spxxxx()
{
    int i;

    i=spfind("SPXXXX");
    if (i<0) return(1);
    sprintf(sbuf,"\nSpec allocated: specmax=%d speclist=%d",specmax,speclist);
    sur_print(sbuf);
    sprintf(sbuf,"\n        In use:         %d          %d",spn,(int)(spl-splist));
    sur_print(sbuf);
    WAIT;
    return(1);
}


int jatkorivit(int j)
{
    char x[LLENGTH];
    char *p,*q;

    while (1)
    {
        edread(x,(unsigned int)j);
        p=x+1;
        while (*p==' ') ++p;
        q=p;
        while (*q!=' ') ++q;
        *q=EOS;
        *(spl-2)=EOS;
        --spl;
        if (spl-splist+strlen(p)+2>speclist) return(-1);
        strcat(spl-1,p);
        spl+=strlen(p);
        if (*(spl-2)!='&') break;
        ++j;
    }
    return(1);
}


int muste_instr(char s[],char c[])
{
    if (strstr(s,c)==NULL) return(-1);
    return(1);
}


int spread3(char *x,int j)
{
    int i,k,pos;
    char *p;
    char xs[LLENGTH];

    pos=1;
    while (pos<ed1)
    {
        p=strchr(x+pos,'=');                                    /* Search for "="                               */
        if (p==NULL) break;                                     /* Ready if no more found                       */
        if (*(p+1)==EOS) break;                                 /* Ready if at the end of line                  */
        if (*(p+1)=='=')
        {
            pos+=2;                                             /* Jump over "="                                */
            continue;
        }
        /* Rprintf("j=%d pos=%d\n",j,p-x); getch(); */
        if (j==r1+r-1 && p-x==c1+c-2)
        {
            pos=p-x+1;                                          /* Skip the place of activation                */
            continue;
        }
        if (spn>=specmax) return(-spn);                         /* Return if too many specifications           */

        /* Specific for editorial arithmetics */
        spp[spn]=*(p-1);                                        /* Save char preceding "=" such as ":",".","|" */
        spplace[spn]=(unsigned int)((j-1)*ed1+p-x);             /* Edit field pointer to specification "="     */

        pos=p-x;
        i=pos-1;
        while (i>0 && x[i]!=' ') --i;                           /* Scan left                                   */
        if (spl-splist+pos-i+1>speclist) return(-spn);
        strncpy(spl,x+i+1,(unsigned int)(pos-i-1));             /* Copy specification to spl                   */
        spa[spn]=spl;
        spl+=pos-i;
        *(spl-1)=EOS;                                           /* Update pointers, spa=left side              */

        i=pos+1;
        while (i<ed1 && x[i]!=' ') ++i;                         /* Scan right                                  */
        if (spl-splist+i-pos+1>speclist) return(-spn);
        strncpy(spl,x+pos+1,(unsigned int)(i-pos-1));           /* Copy specification to spl                   */
        spb[spn++]=spl;
        spl+=i-pos;
        *(spl-1)=EOS;                                           /* Update pointers, spb=right side             */

        if (*(spl-2)=='&')
        {
            k=jatkorivit(j+1);                                  /* Expression continues on the next line       */
            if (k<0) return(-spn);
        }
        spshad[spn-1]=NULL;
        if (zs[j]!=0)                                           /* Shadows                                     */
        {
            edread(xs,(unsigned int)zs[j]);
            if (spl-splist+i-pos+1>speclist) return(-spn);
            strncpy(spl,xs+pos+1,(unsigned int)(i-pos-1));
            spshad[spn-1]=spl;
            spl+=i-pos;
            *(spl-1)=EOS;
        }

        ++pos;
    }
    return(spn);
}

int spread2(int lin,int *raja1)
{
    char raja[12]="*..........";
    int j,i;
    char x[LLENGTH];

//    strcpy(raja,"*..........");
    for (j=lin-1; j>0; --j)             /* Scan upwards until borderline or top of edit field */
    {  
        edread(x,(unsigned int)j);
        i=muste_instr(x,raja);
        if (i>=0) break;
    }
    *raja1=j;
    for (j=*raja1+1; j<=ed2; ++j)      /* Read the specifications */
    {
        edread(x,(unsigned int)j);
        if (global==1)                 /* Zero global if found */
        {
            i=muste_instr(x,"*GLOBAL*");
            if (i>0) global=0;
        }
        i=muste_instr(x,raja);
        if (i>=0) break;
        if (j==r1+r-1) continue;       /* Skip the activated line */
        spn=spread3(x,j);
        if (spn<0) return(spn);
    }

    /*  Rprintf("\n"); for (i=0; i<spn; ++i) Rprintf("\n%s=%s %c %u varjo=%s",
                           spa[i],spb[i],spp[i],spplace[i],spshad[i]); getch();
    */
    return (spn);
}

int spfind_part(char *s) // 17.9.2010
    {
    int i,len;

    len=strlen(s);
    for (i=0; i<spn; ++i)
        {
        if (spa[i]==NULL) continue;
        if (strncmp(s,spa[i],len)==0) break;
        }
    if (i<spn) return(i);
    return(-1);
    }
    
int spnfind(char *s) // RS ADD 2010
        {
        int i,len;

        len=strlen(s);
        for (i=0; i<spn; ++i)
                {
                if (spa[i]==NULL) continue;
                if (strncmp(s,spa[i],len)==0) return(i);
                }
        return(-1);
        }

int sp_init_extra(int lin,int extra_bytes,int extra_specs)
        {
        
        extern int muste_gplot_init;
        extern int muste_gplot_init2;
        
        int i,k;
// RS REM        int tila;
        char *p;
        int spn1;
        int raja1;
        char x[LLENGTH];
        char *s[2];
        
        int par[4]; // RS ADD
        
own_spec_line1=0; // RS ADD
own_spec_line2=0; // RS ADD        

        edread(x,lin);

        if ((p=strstr(x,"SPECS="))!=NULL) // 20.12.2010
            {
            i=0;
            while(p[i]!=' ') ++i; p[i]=EOS;
            i=split(p+6,s,2);
            k=edline2(s[0],1,1); if (k<0) return(-1);

            if (i==1)
                {
                if (k<lin) { own_spec_line1=k; own_spec_line2=lin-1; }
                else if (k>lin) { own_spec_line1=lin+1; own_spec_line2=k; }
                }
            else if (i==2)
                {
                own_spec_line1=k;
                k=edline2(s[1],k,1); if (k<0) return(-1);
                own_spec_line2=k;
                }
            }

        sp_check();
        i=1; if (own_spec_line1) i=2; // 20.12.2010
        speclist+=i*extra_bytes;
        specmax+=i*extra_specs;

// RS GPLOT
        speclist+=200; /* varnimet() */
        specmax+=3;
        specmax+=2; // WX,WY


    /* Allocate memory for specifications */

    splist=muste_malloc((unsigned int)speclist);
    if (splist==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spa=(char **)muste_malloc(specmax*sizeof(char *));
    if (spa==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spb=(char **)muste_malloc(specmax*sizeof(char *));
    if (spb==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    
// RS ADD spb2 
    spb2=(char **)muste_malloc(specmax*sizeof(char *));
    if (spb2==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }

    
    spshad=(char **)muste_malloc(specmax*sizeof(char *));
    if (spshad==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    arvo=(double *)muste_malloc(specmax*sizeof(double));
    if (arvo==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }

/* Extra allocation for editorial arithmetics */
    spp=muste_malloc((unsigned int)specmax);
    if (spp==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spplace=(unsigned int *)muste_malloc(specmax*sizeof(unsigned int));
    if (spplace==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
/* RS Siirretty arit.c:hen
    i=varaa_earg();
    if (i<0) return(-1); */

    spn=0;
    spl=splist;
    global=0;
    
if (muste_gplot_init)
	{
    spn=varnimet(); if (spn<0) return(spn); // RS FROM GPLOT
	if (muste_gplot_init2>0)
		{
    	sur_screen_dim(&par[0],&par[1]);
    	sp_add_value("WX",(double)par[0]);
    	sp_add_value("WY",(double)par[1]);
    	muste_gplot_init2=0;
    	}
	if (muste_gplot_init>1) muste_gplot_init2=1;    	
    muste_gplot_init=0;
    
	}
    
    edread(x,(unsigned int)lin);
    spn=spread3(x,lin);		/* Specifications from the current line */
    if (spn<0)
    {
        spxxxx();
        return(spn);
    }

/* RS ADD SPECS override */
        if (own_spec_line1) // 20.12.2010
            {
            for (i=own_spec_line1; i<=own_spec_line2; ++i)
                {
                edread(x,i);
                spn=spread3(x,i);
                if (spn<0) { spxxxx(); return(spn); }
                }
            }    
    
    spn=spread2(lin,&raja1);			/* Local specifications	*/
    if (spn<0)
    {
        spxxxx();
        return(spn);
    }
    if (raja1==0)
    {
        spxxxx();
        return(spn);
    }
    spn1=spn;
    
    global=1;
    spn=spread2(1,&raja1);				/* Specifications in *GLOBAL* area */
    if (spn<0)
    {
        spxxxx();
        return(spn);
    }
    if (global==1) spn=spn1;
    spxxxx();

    return(spn);
}


int spec_word_dist(int speck_check)
	{
	muste_fixme("FIXME: spec_word_dist() not yet implemented\n"); // RS FIXME
	return(1);
	}

int sp_add_value(char *s,double value) // 10.7.2000
        {
        int k;
        k=strlen(s);
        strncpy(spl,s,k);
        spa[spn]=spl; spb[spn]=NULL; arvo[spn]=value;
        spl+=k+1; *(spl-1)=EOS;
        ++spn;
        return(1);
        }

int sp_init(int lin)
    {
    int i;

    i=sp_init_extra(lin,60,10);

/* Väärinkirjoitettujen spesifikaatioiden arvailua */
if (i>=0 && spec_check) i=spec_word_dist(spec_check);  

    return(i);
    }

int spec_init(int lin)
        {
        int i;
        i=sp_init(lin);
        if (i==-2) return(-1); /* <- spec_word_dist() */

        if (i<0)
            {
            sur_print("\nToo many specifications!");
            WAIT;
            }
        return(i);
        }


static char raja[]="*..........";

static int spec_jatkorivit(char *t, int j, int len)
        {
        char x[LLENGTH];
        char *p,*q;

        while (1)
            {
            edread(x,j);
            p=x+1;
            while (*p==' ') ++p;
            q=p; while (*q!=' ') ++q; *q=EOS;
            if (strlen(t)+strlen(p)>len) return(1);
            *(t+strlen(t)-1)=EOS; strcat(t,p);
            if (*(t+strlen(t)-1)!='&') break;
            ++j;
            }
        return(1);
        }

static int spec_read3(char *s,char *t, char *x, int j, int len)
        {
        int i,pos;
        char *p;
/* RS        char xs[LLENGTH]; */
        char spa[LLENGTH];
        int len1;

        pos=1;
        while (pos<ed1)
            {
            p=strchr(x+pos,'=');
            if (p==NULL) break;
            pos=p-x; i=pos-1;
            while (i>0 && x[i]!=' ') --i;
            *spa=EOS; strncat(spa,x+i+1,pos-i-1);
            if (strcmp(spa,s)==0)
                {
                i=pos+1;
                while (i<ed1 && x[i]!=' ') ++i;
                len1=i-pos-1; if (len<len1) len1=len;
                *t=EOS; strncat(t,x+pos+1,len1);
                if (*(t+strlen(t)-1)=='&') spec_jatkorivit(t,j+1,len);
                return(1);
                }
            ++pos;
            }
        return(-1);
        }

static int spec_instr(char *s,char *c)
        {
        if (strstr(s,c)!=NULL) return(1);
        return(-1);
        }

static int spec_read2(char *s,char *t,int lin,int *raja1,int len)
        {
        int j,i;
        char x[LLENGTH];

        for (j=lin-1; j>0; --j)
            {
            edread(x,j);
            i=spec_instr(x,raja);
            if (i>=0) break;
            }
        *raja1=j;
        for (j=*raja1+1; j<=ed2; ++j)
            {
            edread(x,j);
            i=spec_instr(x,raja);
            if (i>=0) break;
            i=spec_read3(s,t,x,j,len); if (i>=0) return(1);
            }
        return (-1);
        }

static int find_global()
        {
        char *p,*q;
        char *pch;
        char ch;

        pch=z+r2*(c2+1)-1;
        ch=*pch; *pch=EOS; /* last element in edit field temporarily EOS */
        p=strstr(z,"*GLOBAL*");
        if (p==NULL) { *pch=ch; return(-1); }
        q=strstr(z,raja); *pch=ch;
        if (q==NULL || p<q) return(1);
        return(-1);
        }

int spec_find(
char *s, /* spec to be found */
char *t, /* value of spec */
int len
)
        {
        int i,j;
        char *p;
        int raja1;
        char x[LLENGTH];
        int lin;
        char *rs[2];

own_spec_line1=0; // RS ADD
own_spec_line2=0; // RS ADD

        lin=r1+r-1;
        edread(x,lin);
        

// RS ADD
        if ((p=strstr(x,"SPECS="))!=NULL) // 20.12.2010
            {
            i=0;
            while(p[i]!=' ') ++i; p[i]=EOS;
            i=split(p+6,rs,2);
            j=edline2(rs[0],1,1); if (j<0) return(-1);

            if (i==1)
                {
                if (j<lin) { own_spec_line1=j; own_spec_line2=lin-1; }
                else if (j>lin) { own_spec_line1=lin+1; own_spec_line2=j; }
                }
            else if (i==2)
                {
                own_spec_line1=j;
                j=edline2(rs[1],j,1); if (j<0) return(-1);
                own_spec_line2=j;
                }
            }
// RS ADD END
        
        
        
        i=spec_read3(s,t,x,lin,len); if (i>=0) return(1);
        
/* RS ADD SPECS override */
        if (own_spec_line1) // 20.12.2010
            {
            for (j=own_spec_line1; j<=own_spec_line2; ++j)
                {
                edread(x,j);
                i=spec_read3(s,t,x,j,len); if (i>=0) return(1);
                }
            }          
        
        
        i=spec_read2(s,t,lin,&raja1,len);
        if (i>=0) return(i);
        if (raja1==0) return(-1);
        i=find_global(); if (i<0) return(-1);
        i=spec_read2(s,t,1,&raja1,len);
        return(i);
        }


int seek_char2(int m)  /* 18.2.90 */
        {
        char x[LLENGTH];
        char *p;

        edread(x,r1+r-1);
        p=strchr(x+c1+c,(char)m); if (p==NULL) return(1);
        if (p-x>c1+c3-1) { c1=p-x-c3+1; disp(); } /* 28.1.89 */
        c=p-x-c1+1;
        return(1);
        }

int seek_char()
        {
        int m;

        m=nextch_editor();
        if (m==CODE_CODE) m=special_code;
        seek_char2(m);
        return(1);
        }

void seek_string(char *s)    /* 17.5.1992 */
        {
        char x[LLENGTH];
        char *p;

        edread(x,r1+r-1);
        p=strstr(x+c1+c,s); if (p==NULL) return;
        c=p-x-c1+1;
        col_display();
        }

int seek_word0(int mode)
        {
        char x[LLENGTH];
        char *p;

        edread(x,r1+r-1); p=x+c1+c-1;
        while (*p!=' ') ++p;
        while (*p==' ') ++p;
        if (mode==1) { if (p-x<=c1+c3-1) c=p-x-c1+1; return(1); }
        if (p-x>c2) return(1);
        c=p-x-c1+1;
        col_display();
        return(1);
        }

int seek_word()
        {
        seek_word0(1);  /* 16.5.92 */
        return(1);
        }


int miau_koodit()
        {
        int m;

        m=nextch_editor();
        if (m=='!') { strcpy(tut_info2,tut_info); return(1); }
        if (m=='&')
            {
            if (strlen(tut_info)+strlen(tut_info2)>=LLENGTH) return(1);
            strcat(tut_info,tut_info2); return(1);
            }
        if (m=='~') /* 16.5.92 */
            {
            seek_word0(0); return(1);
            }
        print_word((int)(m-'0'-1));
        return(1);
        }


static void disp_nappihaku(char *haku)
        {
        cursor(r3+1,17); sprintf(sbuf,"%s",haku); sur_print(sbuf);
        }

static void sp_vaihto(char *s)
        {
        while ((s=strchr(s,'_'))!=NULL) *s=' ';
        }

static void putsaa()
        {
        cursor(r3+1,0);
        sprintf(sbuf,"%.*s",c3,space); sur_print(sbuf);
        }
        
static int mahtuu(char *x,int len,int rivi)
        {
        int i,j;

        j=strlen(x)-1;
        for (i=0; i<len; ++i)
            {
            if (x[j--]!=' ')
                {
                sprintf(sbuf,"\nNot enough space on edit line %d",rivi);
                PR_EINV; sur_print(sbuf); WAIT; return(-1);
                }
            }
        return(1);
        }
        
static char upf(char ch) // 17.4.2002
    {
    char s[2];
    s[0]=ch; s[1]=EOS;
    struprf(s);
    return(s[0]);
    } 

static void rivisar(unsigned int uusi)
        {
        int rivi,sar;
        int d=0;

        d=0;
        rivi=uusi/ed1+1; sar=uusi-(rivi-1)*ed1;
        if (reverse_search)
            {
            if (rivi<r1) { r1=rivi; r=1; d=1; }
            else r=rivi-r1+1;
            c=sar;
            if (c<=c3) { if (c1!=1) d=1; c1=1; }
            else
                {
                d=c1; c1=sar;
                if (c1>c2-c3+1) c1=c2-c3+1;
                if (c1!=d) d=1; else d=0;
                c=sar-c1+1;
                }
            }
        else
            {
            if (rivi>r1+r3-1) { d=1; r1=rivi; r=1;
                                if (r1>r2-r3+1) { r1=r2-r3+1; r=rivi-r1+1; }
                              }
            else if (rivi<r1) { d=1; r1=rivi; r=1; }
            else r=rivi-r1+1;
            if (sar==0) sar=1;
            if (sar>=c1 && sar<c1+c3) c=sar-c1+1;
            if (sar<c1) { d=1; c1=sar; c=1; }
            if (sar>c1+c3-1) { d=1; c1=sar; c=1;
                               if (c1>c2-c3+1) { c1=c2-c3+1; c=sar-c1+1; }
                             }
            }
        if (d) disp();
        cursor(r,c);
        }
    
      
static int vaihda(char *haku,char *korvaus,unsigned int uusi)
        {
        int i,k,rivi,sar;
        char x[LLENGTH];
        char y[2*LLENGTH];

        rivi=uusi/ed1+1; sar=uusi-(rivi-1)*ed1;
        edread(x,rivi);

        i=strlen(korvaus)-strlen(haku);  /* 11.1.1997 */
        if (i>0)
            { i=mahtuu(x,i,rivi); if (i<0) return(-1); }

        strcpy(y,korvaus); strcat(y,x+sar+strlen(haku));
        edwrite(space,rivi,sar);
        edwrite(y,rivi,sar);

        if (sh)
            {
            i=zs[rivi];
            if (i==0)
                {
                k=creatshad(rivi);
                if (k<0) return(-1);
                }
            }

        i=zs[rivi];
        if ( (i>0 && strlen(haku)!=strlen(korvaus)) || sh )
            {
            edread(x,i);
            *y=EOS;
            if (sh && *sh_korvaus)
                strncat(y,sh_korvaus,strlen(korvaus));
            else
                strncat(y,space,strlen(korvaus));
            strcat(y,x+sar+strlen(haku));
            edwrite(space,i,sar);
            edwrite(y,i,sar);
            testshad(rivi);
            }
        if (rivi>=r1 && rivi<r1+r3) displine2(rivi);
        return(1);
        }

static int etsi(char *haku,unsigned int paikka)
        {
        unsigned int u;
        int i,j;
        char xs[LLENGTH];
        char sh_kohde[LLENGTH];
        char vert_sana[LNAME]; // 17.4.2002
// Rprintf("\nhaku=%s paikka=%u",haku,paikka); getck();
        u=paikka;

        while (1)
            {
            if (reverse_search)
                {
                if (caps_on)
                    while (upf(z[u])!=*haku && u>0) --u;
                else
                    while (z[u]!=*haku && u>0) --u;
                if (u==0) return(0);
                }
            else
                {
                if (caps_on)
                    while (upf(z[u])!=*haku && u<ed1*ed2) ++u;
                else
                    while (z[u]!=*haku && u<ed1*ed2) ++u;
                if (u==ed1*ed2) return(0);
                }
            *vert_sana=EOS; strncat(vert_sana,z+u,strlen(haku));
// Rprintf("\nhaku=%s vert=%s|",haku,struprf(vert_sana)); getck();
            if ((!caps_on && strcmp(haku,vert_sana)==0) ||
                (caps_on && strcmp(haku,struprf(vert_sana))==0))
                {
                if (u%ed1!=0)
                    {
                    if (!sh) return(u);
                    j=u/ed1+1;
                    i=u%ed1;
      /*        Rprintf("\nj=%d i=%d",j,i); getch();  */
                    if (zs[j]>0)
                        {
                        edread(xs,zs[j]);
                        *sh_kohde=EOS; strncat(sh_kohde,xs+i,strlen(haku));
                        if (strncmp(sh_kohde,space,strlen(haku))==0) *sh_kohde=EOS;
                        if (strcmp(sh_haku,sh_kohde)==0) return(u);
                        }
                    else /* zs[j]=0 */
                        {
                        if (*sh_haku==EOS) return(u);
                        }
                    }
                }
            if (reverse_search) --u;
            else ++u;
            }
        return(0); /* ei tarvita? */
        }

static int op_find()
        {
        int i,len,m=0;
        unsigned int n;
        int kesken=1;
        int jatkuva=0;
        unsigned int paikka,uusi,vanha,loppu=0;
        char x[LLENGTH];
        char *haku, *korvaus=NULL;
        int korvaa, replace;

        char xs[LLENGTH];
        char *pline;

        char *p,*p2,*p3;
        int lain;
        char y[LLENGTH];
        int nhaku;
        char nappihaku[LLENGTH];
        char ch[2];
// RS REM        int cc,rr;
        int jatkuva2;
        int n_act;
        unsigned int first,last=0;
        char *osa[2];

        if (search_caps)
          caps_on=s_caps_on();
        else caps_on=0;
        edread(x,r1+r-1);
        if (*x=='#') caps_on=1; // myˆs # kontr.sar. -> case insensitive

        soft_disp(0);
        first=0;
        sh=0;
        reverse_search=0;

        if (*info!='F')
            {
            i=spec_find("LINES",x,LLENGTH-1);
            if (i>=0)
                {
         /*     strcpy(x,spb[i]);   */
                i=split(x,osa,2);
                if (i>0)
                    {
                    first=edline2(osa[0],1,1);
                    if (first==0) return(1);
                    last=first;
                    if (i>1)
                        {
                        last=edline2(osa[1],first,1);
                        if (last==0) return(1);
                        }
                    }
                }
            }
        jatkuva2=0;
        nhaku=0; n_act=0;

        if ( parm[0]!=NULL && *parm[0]=='-' && *info!='F') reverse_search=1;

        if (*info=='F')
            {
            nhaku=1; haku=nappihaku; *haku=EOS; replace=0;
            if (info[1]=='-') reverse_search=1;
            }
        else if (muste_strcmpi(parm[0]+reverse_search,"FIND")==0)
            {
            replace=0;
            if (g<2)
                {
                sur_print("\nCorrect form:");
                sur_print("\nFIND <string> or -FIND <string>");
                WAIT; return(1);
                }
            edread(actline,r1+r-1);
            i=split(actline+1,parm,2);
            edread(actline,r1+r-1);
            p=strstr(actline," / ");
            if (p!=NULL) *p=EOS;
            haku=parm[1];
            if (caps_on) struprf(haku);
            len=strlen(actline);
            while (actline[len-1]==' ') --len; actline[len]=EOS;
            pline=actline;

            i=strlen(haku);  /* 20.1.1996 */

            if (i>1 && haku[i-1]=='~') // 12.6.2004 23.6.2004
                {
            muste_fixme("\nFIXME: ~ FIND not implemented!"); // RS FIXME
/* RS FIXME                
                fr=r; fr1=r1; fc=c; fc1=c1; sprintf(rivi," %d",r1+r);
                strcpy(wordcomp,"WORDCOMP");
                op=wordcomp;
                strcpy(sbuf,"100: "); // mitan alkuarvo
                strcpy(info,"find "); strcat(info,sbuf); strcat(info,haku);
                                      strcat(info,rivi);
                while (1)
                    {
                    childp("");
                    i=atoi(info+5);
                    if (i==100) // takaisin alkuun!
                        {
                        r=fr; r1=fr1; c=fc; c1=fc1;
                        disp();
                        putsaa();
                        return(1);
                        }
                    disp();
                    cursor(r3+1,0);
                    PR_EBLD;
   sprintf(sbuf," d=%d   Next case by N!   Interrupt by ENTER!",i);
                    sur_print(sbuf);
                    cursor(r,c); SAVE_CURSOR;
                    m=nextch_editor(); if (m!='N' && m!='n') break;
                    }
*/                    
                putsaa();
                return(1);
               
                }

            if (*haku=='"' && haku[i-1]=='"')
                { haku[i-1]=EOS; ++haku; }

/*          strcpy(y,haku);   -20.1.1996
            if (*y=='"' && y[strlen(y)-1]=='"')
                { y[strlen(y)-1]=EOS; haku=y+1; }
*/
            i=zs[r1+r-1];
            if (i>0)
                {
                edread(xs,i);
                *sh_haku=EOS; strncat(sh_haku,xs+(haku-pline),strlen(haku));
                if (strncmp(sh_haku,space,strlen(haku))!=0) sh=1;
/* Rprintf("\nsh_haku=%s",sh_haku); getch(); */
                }
            }

        else /* REPLACE */
            {
            replace=1;
            
                edread(y,r1+r-1); // RS ADD
                p=strstr(y," / ");
            	if (p!=NULL) *p=EOS;                

                g=split(y+1,parm,MAXPARM); // RS ADD            
          
            if (g<3)
                {
                sur_print("\nCorrect form:");
                sur_print("\nREPLACE <old_string>,<new_string> or");
                sur_print("\n-REPLACE <old_string>,<new_string>");
                sur_print("\n        Spaces replaced by _'s");
                WAIT; return(1);
                }

            if (g>3 && strcmp(parm[g-1],"C")==0) /* 19.10.90 */
                {
                jatkuva2=jatkuva=1;
                korvaa=1;
                }
            if (g>3 && strcmp(parm[g-1],"N")==0)
                {
                jatkuva2=2; jatkuva=1;
                korvaa=1;
                }

            pline=y; // RS CHA actline -> y
                
            haku=parm[1]; korvaus=parm[2];
            if (caps_on) struprf(haku); // 20.4.2002
            lain=0;
            if (*haku=='"')
                {
                pline=y;
                edread(y,r1+r-1);
                split(y+1,parm,2);
                p=parm[1];
                edread(y,r1+r-1);
                ++p;
                p2=strchr(p+1,'"');
                if (p2!=NULL)
                    {
                    *p2=EOS;
                    ++p2;
                    while (*p2 && (*p2==' ' || *p2==',')) ++p2;
                    if (*p2=='"')
                        {
                        ++p2;
                        p3=strchr(p2,'"');
                        if (p3!=NULL) { *p3=EOS; haku=p; korvaus=p2; lain=1; }
                        }
                    }
                }
            if (!lain) { sp_vaihto(haku); sp_vaihto(korvaus); }

int muste_noshadow; // RS ADD
			muste_noshadow=0;
			i=spec_find("NOSHADOW",sbuf,LLENGTH-1);
            if (i>=0) muste_noshadow=atoi(sbuf);
            			
            i=zs[r1+r-1];
            if (i>0 && !muste_noshadow)
                {
                edread(xs,i);
                
    *sh_haku=EOS; strncat(sh_haku,xs+(haku-pline),strlen(haku));
    *sh_korvaus=EOS; strncat(sh_korvaus,xs+(korvaus-pline),strlen(korvaus));
    if (strncmp(sh_haku,space,strlen(haku))==0) *sh_haku=EOS;  
    if (strncmp(sh_korvaus,space,strlen(korvaus))==0) *sh_korvaus=EOS; 
    if (*sh_haku || *sh_korvaus) sh=1;
/* Rprintf("\nsh_haku=%s sh_korvaus=%s sh=%d\n",sh_haku,sh_korvaus,sh); getch(); */
                }
                
            }

        n=0; /* tapauksia */
        paikka=(r1+r-1)*ed1;
        if (reverse_search) paikka-=ed1;
        if (first)
            {
            paikka=vanha=(first-1)*ed1;
            if (!jatkuva) rivisar(paikka);
            loppu=last*ed1;
            }
        if (nhaku) paikka=(r1+r-2)*ed1+c1+c;
        while (kesken)
            {

            if (nhaku)
                {
                soft_disp(0);
        if (move_ind) { pyyhi_alarivi(); }
                cursor(r3+1,0);
                PR_EBLD;
                sprintf(sbuf,"Search for word: %s",haku); sur_print(sbuf);
                cursor(r,c); SAVE_CURSOR;

                if (etu && etuu==1) m=nextkey_editor();  // 22.9.2009
                else                              // 22.9.2009
                    m=nextch_editor();
                RESTORE_CURSOR;
                if (m==CODE_RETURN || m==CODE_HELP)
                    {
                    if (m==CODE_HELP)
                        {
                        strcat(tut_info,"Break@"); // 25.9.2009
//                   Rprintf("\nBREAK!"); getck();
                        }
                    putsaa();
                    soft_disp(1);
                    return(1);
                    }
                if (m==CODE_EXEC || m==CODE_DELETE)
                    {
                    if (first_word_on_line_search) paikka+=ed1; // 25.9.2009
                    else
                        {
                        ++n_act;
                        if (reverse_search) --paikka;
                        else ++paikka;  /* 26.9.92 */
                        }
                    }
               else if (m==CODE_SRCH)
                    {
                    edread(x,r1+r-1);
                    p=x+c1+c-1; *x=' ';
                    if (*p!=' ')
                        {
                        p2=p; while (*p2!=' ') --p2;
                        p3=p; while (*p3!=' ') ++p3; *p3=EOS;
                        strcpy(haku,p2+1); haku[MAX_PITUUS]=EOS;
                        if (reverse_search) paikka-=(int)(p-p2)+1;
                        disp_nappihaku(haku);
                        }
                    }
                else if (m==CODE_DISP)   /* F5 */
                    {
                    strcpy(haku,vanha_haku);
                    }
                else if (m==CODE_HOME)
                    {
                    paikka=0L; n_act=0;
                    if (reverse_search)
                        {
                        paikka=ed1*ed2-1;
                        reverse_search=0; rivisar(paikka);
                        reverse_search=1;
                        }
                    }
                else
                    {
                    n_act=0;
                    *ch=(char)m; ch[1]=EOS;
                    strcat(haku,ch);
            if (caps_on) muste_strupr(haku); // koe 15.1.2009
                    disp_nappihaku(haku);
                    }
      /*        LOCATE(rr,cc);    */
                strcpy(vanha_haku,haku);
                }
            uusi=etsi(haku,paikka);
            if (uusi==0) break;
            if (first)
                {
                if (uusi>=loppu) { uusi=vanha; break; }
                vanha=uusi;
                }
            if (nhaku) { rivisar(uusi); paikka=uusi; continue; }

            ++n;

            while (!jatkuva)
                {
                rivisar(uusi);
                cursor(r3+1,0);
                PR_EBLD;

                if (replace)
  {   sprintf(sbuf,"n=%u: Replace=R Continuous=C Next=N Interrupt=ENTER",n);
      sur_print(sbuf);
  }
                else
  {  sprintf(sbuf,"n=%u: Next case=N Continuous search=C Interrupt by ENTER",n);
     sur_print(sbuf);
  }

                cursor(r,c);
                korvaa=0;
                SAVE_CURSOR;
                m=nextch_editor();
                RESTORE_CURSOR;
                if (/* special && */ m==CODE_RETURN) { kesken=0; break; }
                if (m=='N' || m=='n') break;
                if (m=='C' || m=='c') { jatkuva=1; korvaa=1; break; }
                if (m=='R' || m=='r') { korvaa=1; break; }
                }

            if (replace && korvaa)
                {
                i=vaihda(haku,korvaus,uusi); if (i<0) { kesken=0; break; }
                paikka=uusi+strlen(korvaus);    /* 15.10.88 */
                if (reverse_search) paikka=uusi-strlen(haku);
                }
            else
                {
                paikka=uusi+strlen(haku);
                if (reverse_search) paikka=uusi-strlen(haku);
                }
            }
        if (!jatkuva2)
            {
            cursor(r3+1,0);
            sprintf(sbuf,"%.*s",c3,space); sur_print(sbuf); cursor(r3+1,0);
            PR_EBLK;
            if (n>0)
                {
                sprintf(sbuf,"%u cases of %.32s found!   Press ENTER!",n,haku);
                sur_print(sbuf);
                }
            else
                {
                if (n_act==0)
                    { sprintf(sbuf,"%.32s not found!   Press ENTER!",haku); sur_print(sbuf); m=0; }
                else
                    {
                    sprintf(sbuf,"%u cases of %.32s found!   Press ENTER!",n_act,haku);
                    sur_print(sbuf);
                    }
                }
            }


        if (jatkuva2==2)
                {
                sprintf(sbuf,"%d@",n);
                strcat(tut_info,sbuf);
                }
        else if (m!=CODE_RETURN && jatkuva2==0) while (1)
            {
            m=nextch_editor();
            if (m==CODE_RETURN) break;
            if (m=='#')
                {
                sprintf(sbuf,"%d@",n);
                strcat(tut_info,sbuf);
                }
            }

    /*  if (nhaku) */ putsaa();
        soft_disp(1);
        return(1);
        }


/* LINEINST START */
static int k1,k2,step;
// static char rivi[LLENGTH];
static char txt[LLENGTH],stxt[LLENGTH];
/* LINEINS END */




static int op_insertl()
    {
    int j,k;

    if (g<2)
        {
        sur_print("\nSee INSERT?");
        WAIT;
        return(1);
        }
    j=r1+r;
    if (g==2) k=atoi(parm[1]);
    else
        {
        j=edline2(parm[1],1,1); if (!j) return(1);
        ++j;
        k=atoi(parm[2]);
        }
    insert_lines(j,k);  // smo.edt

    return(1);
    }

static int copy_line(unsigned int i,unsigned int j)
        {
        unsigned int len,shad;
        char x[LLENGTH];
        char sx[LLENGTH];

        shad=0; if ( zs[i]!=0 || zs[j]!=0 ) shad=1;
        edread(x,i);
        if (shad)
            {
            if (zs[j]==0) creatshad(j);
            if (zs[i]==0) creatshad(i);
            edread(sx,zs[i]);
            }
        len=c2+1;

        edwrite(x,j,0);
        if (shad)
            {
            if (zs[i]>0) { edwrite(sx,zs[j],0); testshad(j); }
            testshad(i);
            }
        return(1);
        }

static int delete_lines(int j1,int j2)
        {
        int i,j,k;
        char x[LLENGTH];

        if (j1>j2) return(1);
        k=j2-j1+1;

        for (j=j1; j<=r2-k; ++j) copy_line(j+k,j);
        strcpy(x,"*"); strncat(x,space,c2-1);
        i=r2-k+1;
        edwrite(x,i,0); zs[i]=0;
        for (j=i+1; j<=r2; ++j) copy_line(i,j);
        return(1);
        }

static int delete_fenced_output(int j1,int j2) // 20.4.2010
    {
    int i1,i2;
    char ch;
    char x[LLENGTH];

    i2=j1;
  while (1)
   {
    i1=i2;
// Rprintf("\nstart from %d:",i1); getck();
    while (i1<=j2)
        {
        ch=z[(i1-1)*ed1+1];
        if (ch!='#') { ++i1; continue; }
        edread(x,i1);
        if (strncmp(x,fence1,11)==0)
            {
sprintf(sbuf,"\nFence line %s (on line %d) before any #command!",fence1,i1);
            sur_print(sbuf); WAIT; return(1);
            }
        break;
        }
    if (i1>=j2) return(1);
    ++i1;
// Rprintf("\ni1=%d j2=%d",i1,j2); getck();

    i2=i1;
    while (i2<=j2)
        {
        ch=z[(i2-1)*ed1+1];
        if (ch!='#') { ++i2; continue; }
        edread(x,i2);
        if (strncmp(x,fence1,11)!=0)
            {
sprintf(sbuf,"\nA possible #command (on line %d) before a fence line!",i2);
            sur_print(sbuf); WAIT; return(1);
            }
        delete_lines(i1,i2-1);
        j2-=i2-i1; i2=i1;
        break;
        }
    if (i2>=j2) return(1);
   ++i2;
   }
    return(1);
    }


static int op_deletel()
    {
    int j1,j2;

    if (g<3)
        {
        sur_print("\nSee DELETE?");
        WAIT;
        return(1);
        }

    j1=edline2(parm[1],1,1); if (!j1) return(1);
    j2=edline2(parm[2],j1,1); if (!j2) return(1);
    if (g>3 && muste_strnicmp(parm[3],"FENCE",5)==0) // 20.4.2010
          {
          delete_fenced_output(j1,j2);
          return(1);
          }

    delete_lines(j1,j2);
    return(1);
    }

// ns_by_text(tyyli,j,nj,txt+1,stxt+1,k,'X');
static int ins_by_text(int tyyli,int j0,int nj,char *t,char *ts,int k,char ch)
// cd = control char (tyyli=2)
    {
    int i,j,n,j2,j1,h,jh;
    int len,shad;
    char x[LLENGTH],xs[LLENGTH];
    char *p;
    FILE *tmp;
// RS REM    extern char etmpd[];


// Rprintf("\nj0=%d",j0);
// Rprintf("\n%s|",t);
// Rprintf("\n%s|",ts);
// getck();

    if (k<0) ++k;

    sprintf(sbuf,"%sSURVO.TMP",etmpd);
    tmp=muste_fopen2(sbuf,"wb");

    len=strlen(t);
    shad=0; if (*ts!=EOS) shad=1;
    n=0;
    for (j=k1; j<=k2; ++j)
        {
        edread(x,j);
        if (tyyli==1)
            {
            if ((p=strstr(x+1,t))==NULL) continue;
            if (shad)
                {
                if (zs[j]>0)
                    {
                    edread(xs,zs[j]);
                    if (strncmp(ts,xs+(p-x),len)!=0) continue;
                    }
                }
            }
       else // tyyli=2
         if (*x!=ch) continue;

        fwrite(&j,sizeof(int),1,tmp);
        ++n;
        }
    muste_fclose(tmp);
    tmp=muste_fopen2(sbuf,"rb");

// Rprintf("\nn=%d",n); getck();
    i=insert_lines(k2+1,n); if (i<0) return(1);

    j2=j+n-1;
// Rprintf("\nj2=%d|",j2); getck();
    j1=k2;

    if (nj==1) jh=0;
    else jh=(n-1)%nj;

    for (i=n-1; i>=0; --i)
        {
        muste_fseek(tmp,(long)(i*sizeof(int)),SEEK_SET);
        fread(&j,sizeof(int),1,tmp);
//  Rprintf("\nj_read=%d|",j);  getck();
        j+=k;   // oli j+=k-1;
        for (h=j1; h>=j; --h)
            {
            copy_line(h,j2--);
            --j1;
            }
        copy_line(j0+jh,j2--);

        if (nj>1)
            {
            --jh; if (jh<0) jh=nj-1;
            }
        }



    muste_fclose(tmp);
    return(1);
    }

static int ins_by_steps(int j0,int nj)
    {
    int i,j,j2;
    int n;
    int jh;

    j=k1-1+step;
    if (j>k2) return(1);
    n=1;
// Rprintf("\nj=%d|",j); getck();
    while (j<=k2-step)
        {
        j+=step;
        ++n;
// Rprintf("\nj=%d|",j); getck();
        }
    k2=j;
    i=insert_lines(k2+1,n); if (i<0) return(1);
    j2=j+n;
// Rprintf("\nj2=%d|",j2); getck();

    if (nj==1) jh=0;
    else jh=(n-1)%nj;
// Rprintf("\njh=%d|",jh); getck();
    while (j>=k1)
        {
        copy_line(j0+jh,j2--);
        if (nj>1)
            {
            --jh; if (jh<0) jh=nj-1;
            }
        for (i=0; i<step; ++i)
            {
            copy_line(j--,j2--);
            }
        }
    return(1);
    }

static int op_lineins()
        {
        int i=0,j,h,nj,k=0;
// RS REM        char x[LLENGTH];
        int tyyli;
        char *p;

        if (g<5)
            {
            sur_print("\nSee LINEINS?");
            WAIT;
            return(1);
            }

        k1=edline2(parm[1],1,1); if (!k1) return(1);
        k2=edline2(parm[2],k1,1); if (!k2) return(1);

        tyyli=0; h=4;
        step=atoi(parm[3]);
        if (muste_strnicmp(parm[3],"TEXT",4)==0)
            {
            k=1;
            strcpy(sbuf,parm[3]+4);
            if (strlen(sbuf)>0) k=atoi(sbuf);
            tyyli=1; ++h;
            j=edline2(parm[4],1,1); if (!j) return(1);
            edread(txt,j);
            *stxt=EOS; stxt[1]=EOS;
            if (zs[j]!=0) edread(stxt,zs[j]);
            if ((p=strchr(stxt,'!'))!=NULL)
                {
                j=p-stxt;
                txt[j]=EOS; stxt[j]=EOS;
                if (strncmp(stxt,space,j)==0)
                    { *stxt=EOS; stxt[1]=EOS; }
                }
            else
                {
                j=strlen(txt)-1;
                while (txt[j]==' ') txt[j--]=EOS;
                stxt[j+1]=EOS;
                }
            }

        if (muste_strnicmp(parm[3],"CONTROL",7)==0)
            {
            k=1;
            strcpy(sbuf,parm[3]+7);
            if (strlen(sbuf)>0) k=atoi(sbuf);
            tyyli=2; ++h;
            }

        strcpy(sbuf,parm[h]);
        p=strchr(sbuf,':');
        if (p!=NULL) *p=EOS;
        j=edline2(sbuf,1,1); if (!j) return(1);
        nj=1;
        if (p!=NULL)
            {
            if (*(p+1)==EOS) nj=1000000000;
            else
                {
                i=edline2(p+1,1,1); if (!i) return(1);
                }
            nj=i-j+1; if (nj<1) nj=1;
            }
//      edread(rivi,j);

        switch (tyyli)
            {
          case 0:
               ins_by_steps(j,nj); break;
          case 1:
               ins_by_text(tyyli,j,nj,txt+1,stxt+1,k,'X'); break;
          case 2:
               ins_by_text(tyyli,j,nj,txt+1,stxt+1,k,*parm[4]); break;
            }

        return(1);
        }

static int op_session()
    {
    if (g==1)
        {
        edwrite(space,r1+r-1,1);
        sprintf(sbuf,"SESSION %s",sur_session);
        edwrite(sbuf,r1+r-1,1); return(1);
        }
    sur_session[0]=*parm[1]; sur_session[1]=EOS;
    return(1);
    }
        

int muste_editor_eventhandler()
        {
        int i,m;
        m=0;
// RS        while (edrun)
// RS            {

            cursor(r,c);

            
                {
                special=FALSE;
                m=nextch_editor_eventloop();              
                if (m==-5 || m==-1) return(edrun); // RS REM continue; /* 13.4.1996 */

                prevkey=m;                

                scroll_line=r+2; if (scroll_line>r3) scroll_line=r3;  // RS Needs to be known!


                if (m==-7)
                    {
                    wait_tut_type=0;
                    parm[0]=wait_tut_name;
                    op_tutor();
                    return(edrun); // RS REM continue;
                    }
                *info=EOS;

                }
              if (special) 
                   {
                   i=key_special(m);
                   if (i<0) edrun=FALSE; // RS exit
                   } 
              else
                   {
                   key_common(m);
                   }
// RS            }
          cursor(r,c);
        return(edrun); 
        }

