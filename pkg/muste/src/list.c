/* LFILE.C (LIST operations) 25.8.1991/SM (15.8.1992)
*/

#include "muste.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define OMAX 5
#define SKMAX 10

/* survolst.h 18.7.1991/SM (27.8.1995) (24.3.1996) */

#define MAX_LLENGTH 65500
#define LEN_LISTNAME 33
#define LEN_FIELDNAME 33
#define MAX_SPACE 65500L
#define MAXBLOCK 65500
#define CONTROL 0
#define SHADOW 1
#define TEXT 2
#define nLINES 3
#define END 4
#define WORDn 5
#define nWORDS 6
#define nSHADOW 7
#define MSHADOW 8
#define START 9
#define LIST_CHAPTER 98
#define EDIT_LINE 99

#define MAXTILA 65500
#define MAXL 64

#define N_ERIK                    12
#define CHAPTERS                   1
#define RECORDS                    2
#define LINES                      3
#define WORDS                      4
#define NUMBERS                    5
#define INTEGERS                   6
#define CHARACTERS                 7
#define LETTERS                    8
#define SPACES                     9
#define DIGITS                    10
#define SPECIALS                  11

#define REUNA '\263'
#define NELIO '\376'

#define SORT 1
#define SAVE 2
#define FILEMAX 12

#define SURVO_LIST struct survolst

SURVO_LIST
        {
        int n;              /* # of chapters */
        char **chapter;     /* names of chapters */
        char **editfile;    /* names of edit files */
        char *listspace1;
        char *listspace2;
        int *line1;         /* first line */
        int *line2;         /* last  line */
        int i;              /* current chapter  2.4.1995 */
        int j;              /* current line in current chapter 2.4.1995 */
        int r2;             /* # of lines in current list edit field 8.4.1995 */
        int c2;             /* # of cols in current list edit field 8.4.1995 */
        int shad;           /* # of shadow lines in curr.ed,field 8.4.1995 */
        char case_start;
        char case_end;
        long nro;           /* current case # */
        int case_var;       /* used when scanning data, default=-1 (order) */

        int m;              /* # of fields 22.4.1995 */
        char **varname;     /* names of fields */
        char *varnamespace;
        char *vartype;      /* types of fields -,1,2,4,8,S */
        int *varlen;        /* field lengths */

        int n_lines;        /* # of lines in current case */
        int *host_var;      /* field that contains current field 25.8.1995 */
        int *start_line;    /* location of current field */
        int *start_pos;
        int *end_line;
        int *end_pos;

        char **value;       /* values of current case */
        int *valueind;      /* 0=not loaded 1=loaded -1=missing value */
        char *valuespace;
        unsigned int tot_valuelen;

        int *sk_type;       /* CONTROL, SHADOW, TEXT etc. <-sk_name */

        char **sk_text;
        char *p_text;
        char *textspace;    /* TEXT:<keytext> */

        int *sk_int0;
        char *sk_char0;
        } ;


extern char **spb;
extern char sur_session[2];

/* surlist0.h  1.9.1997/SM */
static FILE *tempf;
static int lwidth;
static int n_lines,n_lines2;
static int list_check;

/* sk_names.h  2.4.1995/SM (30.4.1995) (22.10.1995)
*/
char *sk_name[]={ "CONTROL", "SHADOW", "TEXT", "#LINES", "END", "WORD#",
                  "#WORDS",  "#SHADOW", "MSHADOW", "START",
                  "" };

static char list_path[LLENGTH];
static char *pl1,*pl2;
static FILE *lst_file;
static FILE *tmp3;
static FILE *sanasto;
static FILE *aputied;
static char sanav[]=" ,.:;-?!()";
static char *psana[512];
static int ptype[512];
static int ted1,ted2; /* kentta, jonka dimensiot ted1,ted2 */
static int tedshad,*tzs,n_shad;
static FILE *text;

/* listsor0.h 3.4.1995/SM (1.5.1995)

LIST SORT <data> BY <list of sort keys> TO <new data file>
0    1     2     3   4  5               3+nsk  4+nsk
                        nsk-1 kpl.
*/

static char lista[LLENGTH],lista1[LLENGTH],lista2[LLENGTH];
static SURVO_LIST list;
static char *key;   /* (muste_malloc) lajitteluavaimet */
static int *ikey;
static int nsk,slen;
static int neg[SKMAX];
static int sk_type[SKMAX];           /*    list.sk_type[sk_v[]]  TEXT  */
static char sk_format[SKMAX];        /*    list.vartype[sk_v[]]  1,2,4,8,S */
/*
char *key_text[SKMAX];              l->key_text   (TEXT:<keytext>)
int sk_int[SKMAX][3];               l->sk_int0 etc.
char sk_char[SKMAX][3];             l->sk_char0 etc.
*/
static char key_textspace[32*SKMAX];
static char *p_textspace;
static int sl[SKMAX],su[SKMAX];
static int sp[SKMAX];
static int sk_v[SKMAX];  /* selects sort keys from fields */
static long n;
static unsigned int nsort;
static unsigned char code[256];
static char codefile[LLENGTH];
static long samplesize;
static int n_osat;
static unsigned int koko;
static long nhav;
static unsigned int workspace;
static double l_wait;
static char case_start,case_end; /* control chars; 0=default */
static int lr2, lshad;
static int chapter, first_line;
static char *pkey;
static char newlist[LNAME];
static char newlist_path[LNAME];
static int prev_chp;
static FILE *edt,*shadows;

static char **specs;

static int list_copycount=0;
static char orgpath[LNAME];
static char copypath[LNAME];
static int orgsave;

// LIST SHOW
static int rdisp,ndisp,r3disp,ndisp1;
static int il;
static int sar1;
static int ensrivi,viimrivi,ensil;
static int sr,sc;
static int varjot=1;
static int lev;
static int end_of_list;
static char message[LLENGTH];
static char rrivi[LLENGTH];
static int ril;
static long n_haku;
static int autom_haku;
static int lopputavut;
static int isot_kirjaimet;
static int rivin_alusta,alkupos;
static int varjohaku;
static int ref_ensil,ref_ensrivi,ref_sar1,ref_sr,ref_sc;
static int hriv,hril;
static int seuraava_rivi;
static int vain_varjo;
static int vsr,vsc;
static int vvensil,vvensrivi,vvsr,vvsc;
static char haku[LLENGTH];         /* lokaaleja -9.11.1996 */
static char hakuvarjo[LLENGTH];
static int hlen,vlen;
static int hpos;
static char *val1[]= { "Lower/upper case observed  (To change, press 'U')",
                "Lower/upper case ignored   (To change, press 'U')" };
static char *val2[]= { "Hyphenated words ignored   (To change, press 'H')",
                "Hyphenated words observed  (To change, press 'H')" };
static char *val3[]= { "Shadow characters ignored  (To change, press 'V')",
                "Shadow characters observed (To change, press 'V')" };
static char muutos[LLENGTH];
static int murivi=0, musar=0;
static char vyk='\311';
static char oyk='\273';
static char vak='\310';
static char oak='\274';
static char vaaka='\315';
static char pysty='\272';

// LIST REPLACE
static char t1[LLENGTH],t2[LLENGTH];
static char st1[LLENGTH],st2[LLENGTH];

// LIST MAKE
static int lr2,lwidth,shad;
static FILE *tfile,*tempf,*edt;
static int edtnro;
static char newlist[LNAME];
static char newlist_path[LNAME];
static int n_edit_lines,n_lines;
static char long_line[MAX_LLENGTH];
static long n_tlines;
static int text_name_line;

// LIST COUNT
static int erik2[N_ERIK];

static char *erik_nimi[]= { "*#chapters", "*#records", "*#lines",
                     "*#words", "*#numbers", "*#integers",
                     "*#characters", "*#letters", "*#spaces",
                     "*#digits", "*#special characters" };

static int rivi1;
static int nphr;
static long *freq;
static char *phrtila,*ph;
static long ltila;
static char **phr,**phrpos,**sphr;
static int *phrlen;
static char *zp,*zsp;
static char delimiter;
static int isot; /* 0=case-sensitive */
static int jaetut_sanat;
static int col1,col2,col22;
static int erikoishaut;
static int *erik;
static long nlines,nrecords,nchapters;
static int erik_alku;
static long nwords,nnumbers,nintegers,ncharacters,nletters,nspaces,ndigits,nspecials;

static char sana[LLENGTH];
static  int kok,luku;
static  int piste;

/*
static char *specs0[]={ "HYPHENS", "DELIMITER", "COLS",
                                             "!" };
static char **specs=specs0;
*/

// LIST SORT
static int n_show;
static int *show_var;
static int komento; /* 1=SORT 2=SAVE */
static FILE *sortf;
static long nf[FILEMAX];
static FILE *osaf[FILEMAX];
static int filemax;
static int file1,file2,nfiles;
static FILE *uusi;
static FILE *codes;

static char txt_file[LNAME];
static int *v;
static int md;
static char erotin;
static FILE *txtfile;

static int n_select;
static int *sel_var;
static char *sel_type; /* 0=ind 1=cases */
static char *sel_rel;  /* * tai + */
static double *sel_lower,*sel_upper;
static char **sel_cases;
static char **sel_lastcase;
static char *sel_neg;

/*
char *specs0[]={ "CHECK", "FILTER", "NSORT", "WORKSPACE", "WAIT",
                 "FILEMAX", "FILE", "IND", "CASES", "SELECT", "PATH",
                 "CASE_START", "CASE_END", "LISTDIM", "SHOW", "!" };
char **specs=specs0;
*/

static int avaa(SURVO_LIST *l, int k, char *muoto);
static int etsi_kpl(SURVO_LIST *l, int k, char *chapter);
static void line_not_found(char *s);
static void tedread(char *s, int j);
static int etsi_rivi(char *sana, unsigned int lin);
static int tlastline();
static int list_open(SURVO_LIST *l, char *lista, int mode);
static int list_find(SURVO_LIST *l, char *lista);
static void list_not_found(char *s);
static int list_find2(int j, SURVO_LIST *l, int kk);
static int list_set(SURVO_LIST *l, int n, char *kpl, char *kent);
static int list_space_alloc(SURVO_LIST *l);
static int list_space_alloc2(SURVO_LIST *l);
static int list_space_alloc3(SURVO_LIST *l);
static void not_enough_memory();
static int lst_file_find(SURVO_LIST *l, char *lista, int mode);
static int chp_edt_read(char *chp, char *edt);
static int lst_read_line(char *x);
static int edt_numbers(char *edt, int *pi1, int *pi2);
static int list_read_fields(SURVO_LIST *l);
static int list_field_def(SURVO_LIST *l, char *s, int k);
static int list_varfind(SURVO_LIST *l, char *s, int ilm);
static int list_case_init(SURVO_LIST *l);
static int read_control(SURVO_LIST *l, int k, char *s);
static int read_shadow(SURVO_LIST *l, int k, char *s, int keytype);
static int read_text(SURVO_LIST *l, int k, char *s);
static int read_wordn(SURVO_LIST *l, int k, char *s);
static int read_start(SURVO_LIST *l, int k, char *s);
static int read_nwords(SURVO_LIST *l, int k, char *s);
static int read_nwords_from_file(SURVO_LIST *l, int k, char *s);
static int lue_rivi_sanastosta(char *x);
static int korvaa_sp(char *s);
static int list_data_load(SURVO_LIST *l, int i);
static int list_data_load8(SURVO_LIST *l, int i, double *px);
static int list_alpha_load(SURVO_LIST *l, int k, char *x);
static int read2_control(SURVO_LIST *l, int k, char *x);
static int read2_shadow(SURVO_LIST *l, int k, char *x);
static int spaces_over(char *x, char *xs, int i1, int i2);
static int read2_text(SURVO_LIST *l, int k, char *x);
static int read2_nlines(SURVO_LIST *l, int k, char *x);
static int read2_wordn(SURVO_LIST *l, int k, char *x);
static int read2_start(SURVO_LIST *l, int k, char *x);
static int read2_nwords(SURVO_LIST *l, int k, char *x);
static int list_rewind(SURVO_LIST *l, char *muoto);
static void list_close(SURVO_LIST *l);
static int list_next_line_read(SURVO_LIST *l, char *x, char *xs, int *pchapter);
static int list_seek_line(SURVO_LIST *l, int chapter, int line);
static int list_chapter_open(SURVO_LIST *l, int k, char *muoto);
static int list_edit_open(SURVO_LIST *l, int k, char *muoto);
static int list_init_shadows();
static int list_find_chapter(SURVO_LIST *l, int k, char *chapter);
static int list_txt_find(SURVO_LIST *l, FILE *tempf, int type, int var, char *text, int len, char *value, int *loc);
static int host_start_read(SURVO_LIST *l, int var, int *pl1, int *ppos1, int *pl2, int *ppos2);
static int list_control_find(SURVO_LIST *l, FILE *tempf, int var, int len, char *value);
static int list_shad_word_find(SURVO_LIST *l, FILE *tempf, int var, char *x);
static int list_field_end(SURVO_LIST *l, FILE *tempf, int var, int *loc);
static int list_field_end_text(SURVO_LIST *l, FILE *tempf, int type, int var, int *loc);
static int list_field_end_control(SURVO_LIST *l, FILE *tempf, int var);
static int lst_save(char *comment, char *newlist_path, char *newlist, int n, char case_start, char case_end);

static void op_list_show(int argc, char *argv[]);
static void ei_listaa(char *s);
static void listaan(SURVO_LIST *l, int n, char *kpl, char *kent);
static int varaa_tilat(SURVO_LIST *l);
static int selaa();
static int prefix();
static void askel_alas();
static void paikka(int sr, int sc);
static void luo_ikkuna();
static void putsaa();
static void disp_label(SURVO_LIST *l, int k, int rivi);
static void vrivit();
static int sur_prin2(char *s, int row, int col);
static int nayta();
static void nayta_rivi(int riv, int i);
static int seur_kentta();
static void vaihehaku(int jatkohaku);
static int etsi_sanaa1(char *haku, char *hakuvarjo);
static int etsi_sanaa(char *haku, char *hakuvarjo);
static int avaa_text(int ik);
static int etsi_varjosanaa(char *haku, char *hakuvarjo);
static void tedreads(char *x, int riv);
static void stedread(char *x, int riv);
static void listan_talletus(int k);
static int hae_valmis_lista();
static void help();
static void optiot();
static void avaa_ikkuna(int riv, int sar, int lev, int kork,char varjo);

static void op_list_replace(int argc,char *argv[]);
static int spt_pois(char *s);
static int hae_lista(SURVO_LIST *l, char *lista);
static int lue_lista(int j,SURVO_LIST *l,int kk);
static void korvaa(char *t1,char *st1,char *t2,char *st2);
static int talletus_replace(int riv,char *x,char *x2);
static int tedwrite(char *s,int j);

static void op_list_make(int argc,char *argv[]);
static int read_case_make(int max);
static int save_tempf(char *x);
static int txt_open(char *x);
static int find_line_start(char *s,int lin);
static int create_edit_file_make(int edtnro);
static int close_edit_file_make(int n_edit_lines);
static int xsave2(char *x,FILE *f);

static void op_list_count(int argc,char *argv[]);
static int etsi_phrases(char *s);
static int count_tilat();
static int ei_tilaa();
static int count();
static int count2();
static int count2e();
static int tulostus();
static int init_erik();
static int tutki_erik(char *x,int k);
static int erik_count(char c);
static int erik_freq();

static void op_list_sortsave(int argc,char *argv[]);
static int tempf_open();
static int avaimet();
static int varaa_tilat_sort();
static void tilanpuute();
static int lue_avaimet(int koko,int *pnsort);
static int read_case(int filter);
static int tutki_show();
static int show_case(long n);
static int make_key();
static void lajittelu();
static void sort1(unsigned int j1,unsigned int j2,int t);
static void shell_sort(unsigned int j1,unsigned int j2,int t);
static int read_key(SURVO_LIST *l, int k);
static int talletus(int kierros);
static void conv(unsigned char *sana);
static int load_codes(char *codefile,unsigned char *code);
static int osatalletus(unsigned int nsort,int k);
static int lomitus();
static int lomita(int file1,int file2,int kierros,int nro);
static void lue_hav(FILE *tied,char *p);
static int vertailu(int k0,int k);
static int create_edit_file_sort(int edtnro);
static int close_edit_file_sort(int n_edit_lines);
static int shadsave(char *xs,int h);
static int read_shad(char *xs);
static int tutki_txtfile(int k);
static int save_txtfile(long n);
static int list_save();
static int list_conditions(SURVO_LIST *d);
static int ltilavirhe();
static void list_sel_virhe(char *s);
static int list_find_cond(SURVO_LIST *d, char *nimi, int nro);
static int l_unsuitable(SURVO_LIST *d);
static int lt_neg(int i,int nro);
static int l_unsuit(SURVO_LIST *d, int nro);
static void list_sel_free();


extern int posnro();
extern int muste_fclose2(void *p);
extern int bool_norm(char *s);

extern int arguc;
extern char *arguv[];

void muste_list(int parmn, char *parm[])
	{
	int i;
// variable inits

	tempf=NULL;
	lwidth=0;
	n_lines=n_lines2=0;
	list_check=0;
	*list_path=EOS;
	pl1=NULL;
	pl2=NULL;
	lst_file=NULL;
	tmp3=NULL;
	sanasto=NULL;
// static char *psana[512];
    *ptype=EOS;
	ted1=ted2=0;
	tedshad=0;
	tzs=NULL;
	n_shad=0;
	text=NULL;
	*lista=EOS; *lista1=EOS; *lista2=EOS;
// static SURVO_LIST list;
	key=NULL;
	ikey=NULL;
	nsk=slen=0;
	*neg=EOS;
	*sk_type=EOS;
	*sk_format=EOS;
	*key_textspace=EOS;
	p_textspace=NULL;
	*sl=EOS; *su=EOS;
	*sp=EOS;
	*sk_v=EOS;
	n=0;
	nsort=0;
	*code=EOS;
	*codefile=EOS;
	samplesize=0;
	n_osat=0;
	koko=0;
	nhav=0;
	workspace=0;
	l_wait=0;
	case_start=case_end=0;
	lr2=lshad=0;
	chapter=first_line=0;
	pkey=NULL;
	*newlist=EOS;
	*newlist_path=EOS;
	prev_chp=0;
	edt=NULL;
	shadows=NULL;
	specs=NULL;
	list_copycount=0;

	rdisp=ndisp=r3disp=ndisp1=0;
	il=0;
	sar1=0;
	ensrivi=viimrivi=ensil=0;
	sr=sc=0;
	varjot=1;
	lev=0;
	end_of_list=0;
	*message=EOS;
	*rrivi=EOS;
	ril=0;
	n_haku=0;
	autom_haku=0;
	lopputavut=0;
	isot_kirjaimet=0;
	rivin_alusta=alkupos=0;
	varjohaku=0;
	ref_ensil=ref_ensrivi=ref_sar1=ref_sr=ref_sc=0;
	hriv=hril=0;
	seuraava_rivi=0;
	vain_varjo=0;
	vsr=vsc=0;
	vvensil=vvensrivi=vvsr=vvsc=0;
	*haku=EOS;
	*hakuvarjo=EOS;
	hlen=vlen=0;
	hpos=0;
	*muutos=EOS;
	murivi=musar=0;
	vyk='\311';
	oyk='\273';
	vak='\310';
	oak='\274';
	vaaka='\315';
	pysty='\272';

    list_copycount=0;
    *orgpath=EOS;
    *copypath=EOS;
    orgsave=0;
    *copypath=EOS;
    *t1=EOS; *t2=EOS; *st1=EOS; *st2=EOS;

    lr2=lwidth=shad=0;
    tfile=NULL;
    tempf=NULL;
    edt=NULL;
    edtnro=0;
    *newlist=EOS;
    *newlist_path=EOS;
    n_edit_lines=n_lines=0;
    *long_line=EOS;
    n_tlines=0;
    text_name_line=0;

    *erik2=EOS;
    rivi1=0;
    nphr=0;
    freq=NULL;
    phrtila=NULL;
    ph=NULL;
    ltila=0;
    phr=NULL; phrpos=NULL; sphr=NULL;
    phrlen=NULL;
    zp=NULL;
    zsp=NULL;
    delimiter=0;
    isot=0;
    jaetut_sanat=0;
    col1=col2=col22=0;
    erikoishaut=0;
    erik=NULL;
    nlines=nrecords=nchapters=0;
    erik_alku=0;
    nwords=nnumbers=nintegers=ncharacters=nletters=nspaces=ndigits=nspecials=0;
    *sana=EOS;
    kok=luku=0;
    piste=0;

    n_show=0;
    show_var=NULL;
    komento=0; 
    sortf=NULL;
    codes=NULL;

    for (i=0; i<FILEMAX; i++)
        {
        nf[i]=0;
        osaf[i]=NULL;
        }
    filemax=0;
    file1=file2=nfiles=0;
    uusi=NULL;

    *txt_file=EOS;
    v=NULL;
    md=0;
    erotin=0;
    txtfile=NULL;

    n_select=0;
    sel_var=NULL;
    sel_type=NULL;
    sel_rel=NULL;
    sel_lower=NULL;
    sel_upper=NULL;
    sel_cases=NULL;
    sel_lastcase=NULL;
    sel_neg=NULL;

	if (parmn<2)
		{
		sur_print("\nIncomplete LIST operation! See LIST?"); WAIT;
		return;
		}
		
	if (muste_strcmpi(parm[1],"SHOW")==0) op_list_show(arguc, arguv);
	else if (muste_strcmpi(parm[1],"REPLACE")==0) op_list_replace(arguc, arguv);
	else if (muste_strcmpi(parm[1],"MAKE")==0) op_list_make(arguc, arguv);
	else if (muste_strcmpi(parm[1],"COUNT")==0) op_list_count(arguc, arguv);
	else if (muste_strcmpi(parm[1],"SORT")==0) op_list_sortsave(arguc, arguv);
	else if (muste_strcmpi(parm[1],"SAVE")==0) op_list_sortsave(arguc, arguv);			
	else
		{
		sprintf(sbuf,"\nUnknown LIST operation %s! See LIST?",parm[1]);
		sur_print(sbuf); WAIT; return;
		}



	
// delete temp-files	
	return;
	}




static FILE *list_fopen(char *path, char *mode) // RS 21.1.2014
	{
    char rivi[ELE], *sana[3], edfile[LNAME];	
    char number[10];
	int h,i,j,k;
	FILE *kopio;
	char *p;
	int ed1,ed2,edshad,zshn;
	char x[LLENGTH+10];
	int rivi_luettu;
 	char *listz;
 	int *listzs;
 	int shad;   
 	short *pint;  
	
	if (strstr(path,".EDT")!=NULL)
		{
		kopio=muste_fopen2(path,"rb");
        if (kopio==NULL) 
            {
            sur_print("\nNo edit field found! Creating a new one..");
	        *copypath=EOS;	
	        kopio=muste_fopen(path,mode);            
            return(kopio);
            }
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(kopio);
        rivi[ELE-1]=EOS;
        muste_fclose2(kopio);
      
        if (strncmp(rivi,"SURVO 98",8)==0) // RS 10.1.2014
            {
            p=strchr(rivi,':');
            if (p==NULL) { return(NULL); }
            i=split(p+1,sana,3);
            ed1=atoi(sana[0]); ed2=atoi(sana[1]); edshad=atoi(sana[2]);

// Rprintf("\nOriginal large field: ed1=%d, ed2=%d, edshad=%d",ed1,ed2,edshad);

			kopio=muste_fopen2(path,"rb");
			fgets(x,LLENGTH-1,kopio); /* otsikko uudelleen */			
			while (1)
				{
				fgets(x,LLENGTH+10-1,kopio);
				if (feof(kopio)) break;
				p=strchr(x,'|');
				if (p==NULL) continue;
				*p=EOS;
				if (*x!='S') j=atoi(x);
				}
//			if ((j+10)<=ed2) j+=10;
			ed2=j; if (edshad>ed2) edshad=ed2;
			muste_fclose2(kopio);				

// Rprintf("\nNon empty large field: ed1=%d, ed2=%d, edshad=%d",ed1,ed2,edshad);
				
                       
			listz=malloc(sizeof(char)*ed1*(ed2+edshad));
			if (listz==NULL) { return(NULL); }
			listzs=(int *)malloc((ed2+1)*sizeof(int));
			if (listzs==NULL) { return(NULL); }
		
			kopio=muste_fopen2(path,"rt");
			if (kopio==NULL) { return(NULL); }

			fgets(x,LLENGTH-1,kopio); /* otsikko uudelleen */
			for (i=0; i<ed1*(ed2+edshad); ++i) listz[i]=' ';
			for (i=0; i<ed1*ed2; i+=ed1) listz[i]='*';

			j=0; while (j<ed2) { ++j; listzs[j]=0; }
			i=ed1*ed2; zshn=0;
			while ( zshn<edshad && zshn<ed2 ) { listz[i]='\0'; i+=ed1; ++zshn; }

			rivi_luettu=0; j=0;
		
			while (1)
				{
				fgets(x,LLENGTH+10-1,kopio);
				if (feof(kopio)) break;
				p=strchr(x,'|');
				if (p==NULL) continue;
				*p=EOS; ++p;
				i=strlen(p); 
				if (p[i-1]=='\n') {
				   p[i-1]=EOS;
				   if (p[i-2]=='\r') p[i-2]=EOS;  // RS unix-fix
				}
				if (rivi_luettu && *x=='S')
					{
				
					i=ed1*ed2; k=0; while ( k<zshn && listz[i]!='\0' ) { ++k; i+=ed1; }
					listzs[j]=k+ed2+1;
// Rprintf("\nlistzs[%d]=%d, p:|%s|",j,listzs[j],p);					
					for (i=0, h=(listzs[j]-1)*ed1; i<ed1; ++i, ++h) listz[h]=' ';
					for (i=0, h=(listzs[j]-1)*ed1; i<strlen(p); ++i, ++h) listz[h]=p[i];
					rivi_luettu=0;
					continue;
					}
				j=atoi(x);
				if (j>ed2) { muste_fclose2(kopio); free(listz); free(listzs); return(NULL); }
				for (i=0, h=(j-1)*ed1; i<strlen(p); ++i, ++h) listz[h]=p[i];
				rivi_luettu=1;
				}
			muste_fclose2(kopio);

            strcpy(orgpath,path); orgsave=0;
			strcpy(edfile,etmpd);
			strcat(edfile,sur_session);
//			list_copycount++;	
			sprintf(sbuf,"LIST%d.EDT",list_copycount);
			strcat(edfile,sbuf);
			  
			kopio=muste_fopen2(edfile,"wb");
			if (kopio==NULL)
				{
				sprintf(sbuf,"\nCannot save %s !",edfile);
				sur_print(sbuf); WAIT;
				free(listz); free(listzs);
				return(NULL);
				}

            strcpy(copypath,edfile);

			strcpy(x,"SURVO84ED ");
			strcat(x,muste_itoa(ed1,number,10)); strcat(x," ");
			strcat(x,muste_itoa(ed2,number,10)); strcat(x," ");
			strcat(x,"                    "); x[20]=EOS;
			strcat(x,muste_itoa(ed1,number,10)); strcat(x," S");
			strcat(x,muste_itoa(edshad,number,10)); strcat(x," ");

			for (i=0; i<ed1; ++i) putc(' ',kopio);
			rewind(kopio);
			for (i=0; i<strlen(x); ++i) putc((char)x[i],kopio);

			muste_fseek(kopio,(long)ed1,0);

			k=0;
			for (j=0; j<ed2; ++j)
				{
				for (i=0; i<ed1; ++i, ++k) putc((char)listz[k],kopio);
				if (ferror(kopio)) { muste_fclose2(kopio); free(listz); free(listzs); return(NULL); }
				}

			shad=0;
			for (j=1; j<=ed2; ++j)
				{
				if (listzs[j]!=0)
					{
					if (shad==0)
						{
						strcpy(x,"Shadows"); strncat(x,space,ed1-7);
						++shad;
						for (i=0; i<ed1; ++i) putc((int)x[i],kopio);
						}
					pint=(short *)x;
					*pint=j;	
					strncpy(x+2,listz+(listzs[j]-1)*ed1,(unsigned int)ed1);
					x[ed1]=EOS;
					for (i=0; i<ed1; ++i) putc((int)x[i],kopio);
					}
				}
			strcpy(x,"END"); strncat(x,space,ed1-3);
            for (i=0; i<ed1; ++i) putc((int)x[i],kopio);			
			if (ferror(kopio)) { muste_fclose2(kopio); free(listz); free(listzs); return(NULL); }
			muste_fclose2(kopio); 
			free(listz); free(listzs);
			
			kopio=muste_fopen(edfile,mode);

// Rprintf("\no id: %ld, orgpath: %s, copypath: %s",(long)kopio,orgpath,copypath);	
			
			return(kopio);           
            }
		}
	*copypath=EOS;	
	kopio=muste_fopen(path,mode);
// Rprintf("\no id: %ld, path: %s",(long)&kopio,path);			
	return(kopio);
	}

static int list_fclose(void *p)
	{
	FILE *source, *target;
	char ch;
	
// Rprintf("\nc id: %ld, orgpath: %s, copypath: %s",(long)p,orgpath,copypath);			
		
	if (orgsave)
	    {
	    muste_fclose((FILE *)p);
	    
	    source = muste_fopen2(copypath, "r");
        target = muste_fopen2(orgpath, "w");

        if( target != NULL )
            {
            while( ( ch = getc(source) ) != EOF ) putc(ch, target);
            }
            
        muste_fclose2(target); 
        muste_fclose2(source);
        orgsave=0;
        return(1); 
	    }
	return(muste_fclose((FILE *)p));
	}

static int avaa(SURVO_LIST *l, int k, char *muoto)
        {
        int i;
        char tfile[LLENGTH];
        char chapter[LNAME];
        char rivi[ELE];
        char *sana[5];
        char nimi[LNAME];

        strcpy(chapter,l->chapter[k]);
        strcpy(tfile,l->editfile[k]);
/*      disp_label(l,k,rdisp+1);       */
        if (!muste_is_path(tfile))
            {
            if (*tfile=='.') strcpy(nimi,esysd); else strcpy(nimi,list_path);
                                                                  /* 15.8.92 */
            strcat(nimi,tfile);
            strcpy(tfile,nimi);
            }
        strcpy(nimi,tfile);
        muste_append_path(nimi,".EDT");
        text=muste_fopen2(nimi,muoto);
        if (text==NULL)
            {
            sprintf(sbuf,"\nEdit file %s not found!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        muste_fclose2(text);

		text=list_fopen(nimi,muoto);
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(text);
        rivi[ELE-1]=EOS;
        i=split(rivi,sana,5);
        if (i<3) { rewind(text); list_fclose(text); return(-1); }
        if (strcmp(sana[0],"SURVO84ED")!=0) { rewind(text); list_fclose(text); return(-1); }
        ted1=atoi(sana[1]); ted2=atoi(sana[2]);
        if (i>4 && *sana[4]=='S') tedshad=atoi(sana[4]+1);
		rewind(text);
        list_init_shadows();
        rewind(text);
/*
printf("\n");
for (i=0; i<n_shad; ++i) printf("%d ",shad_int[i]); getch();
*/
        i=etsi_kpl(l,k,chapter); if (i<0) return(-1);
        return(1);
        }


static int etsi_kpl(SURVO_LIST *l, int k, char *chapter)
        {
        int i,j;
        char x[LLENGTH],*osa[4];

        if (l->line1[k]!=0) return(1);

        if (strcmp(chapter,"*")==0)
            {
            l->line1[k]=1;
            l->line2[k]=tlastline();
            return(1);
            }
        for (j=1; j<=ted2; ++j)
            {
            tedread(x,j);
            i=split(x+1,osa,1);
            if (strcmp(osa[0],"DEF")!=0) continue;
            tedread(x,j);
            i=split(x+1,osa,4);
            if (strcmp(osa[1],chapter)!=0) continue;
            if (i<4)
                {
                tedread(x,j);
                sprintf(sbuf,"\nError in chapter definition: %s",x+1);
                sur_print(sbuf); WAIT; return(-1);
                }
            i=etsi_rivi(osa[2],1); if (i==0) { line_not_found(osa[2]); return(-1); }
            l->line1[k]=i;
            i=etsi_rivi(osa[3],i); if (i==0) { line_not_found(osa[3]); return(-1); }
            l->line2[k]=i;
            return(1);
            }
        sprintf(sbuf,"\nChapter %s not found!",chapter);
        sur_print(sbuf); WAIT; return(-1);
        }

static void line_not_found(char *s)
        {
        sprintf(sbuf,"\nLine '%s' not found!",s); sur_print(sbuf); WAIT;
        }

static void tedread(char *s, int j)
        {
        int i;

        muste_fseek(text,(long)ted1*(long)j,0);
        for (i=0; i<ted1; ++i) { *s=(char)getc(text); ++s; }
        *s=EOS;
        }

static int etsi_rivi(char *sana, unsigned int lin) /* alkurivi */
        {
        int j,k;
        char SANA[3];
        char x[LLENGTH];
        int muutos;  /* esim. A-3 -> muutos=-3 */

        if (posnro(sana))
                {
                j=atoi(sana); if (j>=lin && j<=ted2) return(j);
                return(0);
                }
        for (j=0; j<3; ++j) SANA[j]=toupper(sana[j]);
        if (strncmp(SANA,"END",3)==0)
                {
                j=tlastline();
                if (strlen(sana)<5) return(j);
                j+=atoi(sana+3);
                if (j>=lin && j<=ted2) return(j);
                return(0);
                }

        muutos=0;
        if (strlen(sana)>1)
            {
            muutos=atoi(sana+1);
            }
        for (j=lin; j<=ted2; ++j)
            {
            tedread(x,j);
            if ( *x==*sana ) return(j+muutos);
            }
        return(0);
        }

static int tlastline()
        {
        int i,j;
        char x[LLENGTH];

        j=ted2;
        while (j>0)
                {
                tedread(x,j);
                if (strncmp(space,x+1,ted1-1)!=0) break;
                --j;
                }
        return(j);
        }


/* list1.c  2.4.1995/SM (30.4.1995) (26.8.1995) (24.3.1996)
*/

static int list_open(SURVO_LIST *l, char *lista, int mode) /* 1=only chapters 2=also fields */
        {
        int i;

        i=list_find(l,lista);       
        if (i<0) i=lst_file_find(l,lista,mode);       
        return(i);
        }

static int list_find(SURVO_LIST *l, char *lista)
        {
        int i,j;
        char x[LLENGTH],*osa[2];
        char s[LLENGTH];

        strcpy(list_path,edisk);  /* 24.5.1992 */
        i=spfind("PATH");
        if (i>=0) strcpy(list_path,spb[i]);
        strcpy(s,lista); strcat(s,":");
        for (j=1; j<=r2; ++j)
            {
            edread(x,j);
            i=split(x+1,osa,2);
            if (i<2) continue;
            if (strcmp(osa[0],"LIST")!=0) continue;
            if (strcmp(osa[1],s)==0) break;
            }
        if (j>r2) return(-1);

        l->n=list_find2(j,l,0); if (l->n<0) return(-1);
        if (l->n==0)
            {
            sprintf(sbuf,"\nLIST %s is empty!",lista);
            sur_print(sbuf); WAIT; return(-1);
            }
// printf("\nn=%d",l->n); getch();
        i=list_space_alloc(l); if (i<0) return(-1);
        list_find2(j,l,1);
/*
for (i=0; i<l->n; ++i)
    printf("\n%d %s %s",i,l->chapter[i],l->editfile[i]);
    getch();
*/
        return(j);
        }

static void list_not_found(char *s)
        {
        sprintf(sbuf,"\nLIST %s:  not defined!",s);
        if (etu==2)
            {
            strcpy(tut_info,"˛˛˛@13@LIST SHOW@");
            strcat(tut_info,sbuf); strcat(tut_info,"@");
            return;
            }
        sur_print(sbuf);
        WAIT; return;
        }

static int list_find2(int j, SURVO_LIST *l, int kk) /* kk=0: lasketaan vain lukumaara */
        {
        int n,k,i,h;
        char x[LLENGTH];
        char *p,*q,*s;
        char *osa[EP4];
        char sana[LLENGTH];
        int ind1,ind2;

        if (kk!=0) { pl1=l->listspace1; pl2=l->listspace2; }
        n=0;
        edread(x,j);
        p=strchr(x+1,':');
        if (p==NULL)
            { sur_print(": missing in LIST definition!"); WAIT; return(-1); }
        ++p;
        while (1)
            {
            k=split(p,osa,EP4); h=0;
            while (h<k)
                {
                if (strcmp(osa[h],"END")==0) return(n);
                strcpy(sana,osa[h+1]);
                q=strchr(sana,'-');
                if (q!=NULL)
                    {
                    ind2=atoi(q+1);
                    *q=EOS; --q;
                    while (*q>='0' && *q<='9') --q;
                    ind1=atoi(q+1);
                    if (ind2<ind1)
                        {
                        sprintf(sbuf,"\nError in LIST notation %s",osa[h+1]);
                        sur_print(sbuf); WAIT; return(-1);
                        }
                    }
                else ind1=-1;

                if (kk==0)
                    {
                    if (ind1==-1) ++n; else n+=ind2-ind1+1;
                    }
                else
                    {
                    if (ind1==-1)
                        {
                        list_set(l,n,osa[h],osa[h+1]);
                        ++n;
                        }
                    else
                        {
                        *(q+1)=EOS;
                        for (i=ind1; i<=ind2; ++i)
                            {
                            sprintf(sbuf,"%s%d",sana,i);
                            list_set(l,n,osa[h],sbuf);
                            ++n;
                            }
                        }
                    }
                h+=2;
                }
            ++j; if (j>=r2) return(n);
            edread(x,j);
            if (empty_line(x+1,c2)) return(n);
            p=x+1;
            }
        }


static int list_set(SURVO_LIST *l, int n, char *kpl, char *kent)
        {
        strncpy(pl1,kpl,(LEN_LISTNAME-1)/2);
        *(pl1+(LEN_LISTNAME-1)/2)=EOS;
        l->chapter[n]=pl1; pl1+=(LEN_LISTNAME-1)/2+1;
        strncpy(pl2,kent,LEN_LISTNAME-1);
        *(pl2+LEN_LISTNAME-1)=EOS;
        l->editfile[n]=pl2; pl2+=LEN_LISTNAME;
        l->line1[n]=0;
        return(1);
        }

static int list_space_alloc(SURVO_LIST *l)        {
        int nmax,i;

        nmax=65500/LEN_LISTNAME-1;
        if (l->n > nmax)
            {
            sprintf(sbuf,"\nToo many (>%d) chapters in LIST definition!",nmax);
            sur_print(sbuf); WAIT; return(-1);
            }
        l->listspace1=muste_malloc(l->n*((LEN_LISTNAME-1)/2+1));
        if (l->listspace1==NULL) { not_enough_memory(); return(-1); }
        l->listspace2=muste_malloc(l->n*LEN_LISTNAME);
        if (l->listspace2==NULL) { not_enough_memory(); return(-1); }
        l->chapter=(char **)muste_malloc(l->n*sizeof(char **));
        if (l->chapter==NULL) { not_enough_memory(); return(-1); }
        l->editfile=(char **)muste_malloc(l->n*sizeof(char **));
        if (l->editfile==NULL) { not_enough_memory(); return(-1); }
        l->line1=(int *)muste_malloc(l->n*sizeof(int));
        if (l->line1==NULL) { not_enough_memory(); return(-1); }
        l->line2=(int *)muste_malloc(l->n*sizeof(int));
        if (l->line2==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<l->n; ++i) l->line1[i]=l->line2[i]=0;
        l->case_start=l->case_end=EOS;
/* printf("\ntilat 1:"); */
        return(1);
        }

static int list_space_alloc2(SURVO_LIST *l)
        {
        int m;

        m=l->m;
        l->varname=(char **)muste_malloc(m*sizeof(char **));
        if (l->varname==NULL) { not_enough_memory(); return(-1); }
        l->varnamespace=muste_malloc(m*LEN_FIELDNAME);
        if (l->varnamespace==NULL) { not_enough_memory(); return(-1); }
        l->vartype=muste_malloc(m);
        if (l->vartype==NULL) { not_enough_memory(); return(-1); }
        l->varlen=(int *)muste_malloc(m*sizeof(int));
        if (l->varlen==NULL) { not_enough_memory(); return(-1); }

        l->host_var=(int *)muste_malloc(m*sizeof(int)); /* 25.8.1995 */
        if (l->host_var==NULL) { not_enough_memory(); return(-1); }
        l->start_line=(int *)muste_malloc(m*sizeof(int));
        if (l->start_line==NULL) { not_enough_memory(); return(-1); }
        l->start_pos=(int *)muste_malloc(m*sizeof(int));
        if (l->start_pos==NULL) { not_enough_memory(); return(-1); }
        l->end_line=(int *)muste_malloc(m*sizeof(int));
        if (l->end_line==NULL) { not_enough_memory(); return(-1); }
        l->end_pos=(int *)muste_malloc(m*sizeof(int));
        if (l->end_pos==NULL) { not_enough_memory(); return(-1); }

        l->value=(char **)muste_malloc(m*sizeof(char **));
        if (l->value==NULL) { not_enough_memory(); return(-1); }
        l->sk_type=(int *)muste_malloc(2*m*sizeof(int));
        if (l->sk_type==NULL) { not_enough_memory(); return(-1); }
        l->sk_text=(char **)muste_malloc(2*m*sizeof(char **));
        if (l->sk_text==NULL) { not_enough_memory(); return(-1); }

        /* 26.8.1995  24.3.1996 */
        l->textspace=muste_malloc((unsigned int)MAX_SPACE);
        if (l->textspace==NULL) { not_enough_memory(); return(-1); }

        l->sk_char0=muste_malloc(2*m);
        if (l->sk_char0==NULL) { not_enough_memory(); return(-1); }
        l->sk_int0=(int *)muste_malloc(2*m*sizeof(int));
        if (l->sk_int0==NULL) { not_enough_memory(); return(-1); }

        l->p_text=l->textspace;
/* printf("\ntilat 2:"); */

        return(1);
        }

static int list_space_alloc3(SURVO_LIST *l)
        {
        int i,m;


        l->valueind=(int *)muste_malloc(l->m*sizeof(int));
        if (l->valueind==NULL) { not_enough_memory(); return(-1); }
        l->valuespace=muste_malloc(l->tot_valuelen);
        if (l->valuespace==NULL) { not_enough_memory(); return(-1); }

        m=0;
        for (i=0; i<l->m; ++i)
            {
            l->value[i]=l->valuespace+m;
            m+=l->varlen[i];
            }
/*
printf("\nvert: %d %d",m,l->tot_valuelen);
printf("\ntilat 3:"); getch();
*/
        return(1);
        }

static void not_enough_memory()
        {
        sur_print("\nNot enough memory for LIST!");
        WAIT;
        }



static int lst_file_find(SURVO_LIST *l, char *lista, int mode)
        {
        int i,n,k,n0;
        char nimi[LNAME];
        char *p;
        char chp[LNAME],edt[LNAME];
        char lst_polku[LNAME];
        char kent[LNAME];
        char x[LLENGTH];
        int i0,i1,i2;

        *nimi=EOS;
        if (muste_is_path(lista))        
            {
            strcpy(nimi,lista);
            strcpy(lst_polku,lista);
            n=strlen(lst_polku); p=lst_polku+n-1;
            while (*p!=':' && *p!='\\' && *p!='/' && p>=lst_polku) { *p=EOS; --p; }
            }
        else
            { *lst_polku=EOS; strcpy(nimi,edisk); strcat(nimi,lista); }         
//        p=strchr(nimi+strlen(nimi)-4,'.');
//        if (p==NULL) strcat(nimi,".LST");
        muste_append_path(nimi,".LST");
        
        lst_file=muste_fopen2(nimi,"rt");
        if (lst_file==NULL) return(-1);

        n=n0=0;
        while (!feof(lst_file))
            {
            chp_edt_read(chp,edt);
            if (muste_strcmpi(chp,"END")==0) break;
            k=edt_numbers(edt,&i1,&i2);
            if (k<0) { muste_fclose2(lst_file); return(-1); }
            n+=k; ++n0;
            }



            
        l->n=n;
        rewind(lst_file);
        i=list_space_alloc(l); if (i<0) { muste_fclose2(lst_file); return(-1); }
        
        pl1=l->listspace1; pl2=l->listspace2;
        i0=0;
        for (i=0; i<n0; ++i)
            {
            chp_edt_read(chp,edt);
    /*      fscanf(lst_file,"%s %s\n",chp,edt);  */
            k=edt_numbers(edt,&i1,&i2);
            strcpy(kent,edt);
            if (!muste_is_path(edt))              
                {
                strcpy(kent,lst_polku); strcat(kent,edt);
                }
            if (k==1)
                {
                list_set(l,i0,chp,kent);
                ++i0;
                continue;
                }
            for (k=i1; k<=i2; ++k)
                {
                sprintf(x,"%s%d",kent,k);
                list_set(l,i0,chp,x);
                ++i0;
                }
            }

       lst_read_line(x);  /* END sivuutetaan */

       l->case_var=-1;

       while (!feof(lst_file))
            {
            lst_read_line(x);
            p=strchr(x,'=');
            if (p!=NULL)
                {
                *p=EOS; ++p;
                if (strcmp(x,"CASE_START")==0)
                    {
                    l->case_start=*p; continue;
                    }
                if (strcmp(x,"CASE_END")==0)
                    {
                    l->case_end=*p; continue;
                    }
                if (strcmp(x,"CASE_VAR")==0)
                    {
                    i=list_varfind(l,p,1);
                    if (i==-1) { muste_fclose2(lst_file); return(-1); }
                    l->case_var=i;
                    continue;
                    }
                }
            if (mode==2 && (strncmp(x,"FIELDS",6)==0 || strncmp(x,"LIST FIELDS",11)==0))
                {
                i=list_read_fields(l); if (i<0) { muste_fclose2(lst_file); return(-1); }
                continue;
                }
            }
        muste_fclose2(lst_file);
/***************
printf("\nLIST:");
for (i=0; i<l->n; ++i)
    printf("\n%d %s %s",i,l->chapter[i],l->editfile[i]);
    getch();
****************/
        return(1);
        }

static int chp_edt_read(char *chp, char *edt)
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

static int edt_numbers(char *edt, int *pi1, int *pi2)
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


static int list_read_fields(SURVO_LIST *l)
        {
        int i,m;
        long lp;
        char x[LLENGTH];

        l->tot_valuelen=0;

        i=m=0; lp=muste_ftell(lst_file);
        sprintf(x,"%sSURVO3.TMP",etmpd);
        tmp3=muste_fopen2(x,"wt");
        if (tmp3==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        while (!feof(lst_file))
            {
            lst_read_line(x);
            if (strcmp(x,"END")==0) { i=1; break; }
            fprintf(tmp3,"%s\n",x);
            ++m;
            }
        muste_fclose2(tmp3);

        if (!i)
            {
            sur_print("\nEND line missing in LIST FIELDS section!");
            WAIT; return(-1);
            }
        l->m=m;
        i=list_space_alloc2(l); if (i<0) return(-1);

        muste_fseek(lst_file,lp,SEEK_SET);

        for (i=0; i<l->m; ++i)
            {
            lst_read_line(x);
            list_field_def(l,x,i);

            l->tot_valuelen+=l->varlen[i];
            }

        i=list_space_alloc3(l); if (i<0) return(-1);
        return(1);
        }

/* list1b.c (LIST) 3.4.1995/SM (30.4.1995) (20.10.1995) (24.3.1996)
*/

static int list_field_def(SURVO_LIST *l, char *s, int k)
        {
        int i,h,keytype;
        char x[LLENGTH],*osa[OMAX];
        char y[LLENGTH];
        char *p,*q;
        char *kuvaus;
        int m;
        int n;

        m=l->m;
        strcpy(x,s);
        i=split(x,osa,5); /* nro type len name */
        if (i<5)
            {
            sprintf(sbuf,"\nInvalid field definition: %s",s);
            WAIT; return(-1);
            }
        kuvaus=osa[4];
// printf("\nfield_def: %s %s %s %s",osa[0],osa[1],osa[2],osa[3]); getch();
        /* tarkastus k=atoi(osa[0])-1 puuttuu! */
        l->vartype[k]=*osa[1];
        i=atoi(osa[2]); if (i==2) i=sizeof(int); /* eli 4 */
        l->varlen[k]=i;
        if (*osa[1]=='N') l->vartype[k]=*osa[2];
        if (*osa[1]=='L') { l->vartype[k]='L'; l->varlen[k]=sizeof(long); }

        strcpy(y,osa[3]); y[LEN_FIELDNAME-1]=EOS;
        l->varname[k]=l->varnamespace+k*LEN_FIELDNAME;
        strcpy(l->varname[k],y);

        strcpy(x,s);
        kuvaus=osa[4];
        n=splitsp(kuvaus,osa,OMAX);

        if (*osa[n-1]=='[')  /* 25.8.1995 */
            {
            p=osa[n-1]+1;
            q=strchr(p,']');
            if (q==NULL)
                {
                sprintf(sbuf,"\n`]' missing in `%s'!",osa[n-1]);
                sur_print(sbuf);
                WAIT; return(-1);
                }
            *q=EOS;
            h=list_varfind(l,p,1);
            if (h<0) return(-1);
            if (h>=k)
                {
                sprintf(sbuf,"\nField %s in [%s] must be defined before %s",
                              p,p,l->varname[k]);
                sur_print(sbuf); WAIT; return(-1);
                }
            l->host_var[k]=h;
            --n; /* poistetaan [..] */
            }
        else l->host_var[k]=-1;

        p=strchr(osa[0],':');
        if (p==NULL)
            {
            sprintf(sbuf,"\n`:' missing in `%s'!",s);
            sur_print(sbuf);
            WAIT; return(-1);
            }
        *p=EOS; ++p;
        keytype=0;
        while (*sk_name[keytype])
            {
// printf("\nosa[0]=%s sk_name=%s",osa[0],sk_name[keytype]); getch();
            if (muste_strcmpi(osa[0],sk_name[keytype])==0) break;
            ++keytype;
            }
        if (*sk_name[keytype]==EOS)
            {
            sprintf(sbuf,"\nUnknown key type `%s' in\n%s",osa[0],s);
            sur_print(sbuf);
            WAIT; return(-1);
            }

        l->sk_type[k]=keytype;
        i=p-x;
// printf("\ns+i=%s p=%s!",s+i,p); getch();
        switch (keytype)
            {
          case CONTROL:
            i=read_control(l,k,p); /* s+i -> p 30.9.1995 */
            break;
          case SHADOW:
          case nSHADOW:
          case MSHADOW:
            i=read_shadow(l,k,p,keytype);
            break;
          case TEXT:
            i=read_text(l,k,p);
            break;
          case nLINES:
            break;
          case WORDn:
            i=read_wordn(l,k,p);
            break;
          case nWORDS:
            i=read_nwords(l,k,p);
            break;
          case START:
            i=read_start(l,k,p); // onko tarpeen?
            break;
            }
        if (i<0) return(-1);
/* 26.8.1995 loppuun asti */
        l->sk_type[k+m]=-1;
        if (n==1) return(1);
/* printf("\nn=%d",n); getch(); */
        if (n<3 || strcmp(osa[1],"-")!=0)
            {
            sprintf(sbuf,"\nError in field definition: %s",s);
            sur_print(sbuf); WAIT; return(-1);
            }
        p=strchr(osa[2],':');
        if (p==NULL)
            {
            sprintf(sbuf,"\n`:' missing in `%s'!",osa[2]);
            sur_print(sbuf);
            WAIT; return(-1);
            }
        *p=EOS; ++p;
        keytype=0;
        while (*sk_name[keytype])
            {
/* printf("\nosa[2]=%s sk_name=%s",osa[2],sk_name[keytype]); getch(); */
            if (muste_strcmpi(osa[2],sk_name[keytype])==0) break;
            ++keytype;
            }
        if (*sk_name[keytype]==EOS)
            {
            sprintf(sbuf,"\nUnknown key type `%s' in\n%s",osa[2],s);
            sur_print(sbuf);
            WAIT; return(-1);
            }

        l->sk_type[k+m]=keytype;
        switch (keytype)
            {
          case CONTROL:
            i=read_control(l,k+m,p); return(i);
            break;
          case SHADOW:
          case MSHADOW:
            i=read_shadow(l,k+m,p,keytype); return(i);
            break;
          case TEXT:
            i=read_text(l,k+m,p); return(i);
            break;

            }
        return(1);
        }

static int list_varfind(SURVO_LIST *l, char *s, int ilm)
        {
        int i,k;

        k=strlen(s);
        for (i=0; i<l->m; ++i)
            {
            if (strncmp(s,l->varname[i],k)==0) break;
            }
        if (i==l->m)
            {
            if (ilm)
                {
                sprintf(sbuf,"\nField %s not defined!",s);
                sur_print(sbuf); WAIT;
                }
            return(-1);
            }
        return(i);
        }

static int list_case_init(SURVO_LIST *l)
        {
        int i;
        for (i=0; i<l->m; ++i) l->valueind[i]=0;
        return(1);
        }

static int read_control(SURVO_LIST *l, int k, char *s)
        {
               /*
                                                                 sk_int0[k]
                   CONTROL:<char>   (koko rivi tai rivin alusta) 1
               */
        int i;
        char x[LLENGTH],*osa[2];

//   printf("\nCONTROL: k=%d s=%s",k,s); getch(); */
        l->sk_int0[k]=1;
        l->sk_char0[k]=*s;

        return(1);
        }

static int read_shadow(SURVO_LIST *l, int k, char *s, int keytype) /* SHADOW || nSHADOW */
        {
               /*
                                                             sk_int0[k]
                   SHADOW:char,STRING (painted by char)      0
                   SHADOW:string,WORD                        1
                   SHADOW:string (usually 1 char)            2
                   MSHADOW:string (usually 1 char)           3
                   #SHADOW                                   +10
               */
        int i;
        char x[LLENGTH],*osa[2];
        int nplus;

        if (keytype==nSHADOW) nplus=10; else nplus=0;
        strcpy(x,s);
        i=split(x,osa,2);
        if (i==1)
            {
            l->sk_int0[k]=2+nplus;
            if (keytype==MSHADOW) l->sk_int0[k]=3;
            l->sk_text[k]=l->p_text;
            strcpy(l->p_text,osa[0]); l->p_text+=strlen(osa[0])+1;
            }
        else
            {
            if (muste_strcmpi(osa[1],"WORD")==0)
                {
                l->sk_int0[k]=1+nplus;
                l->sk_text[k]=l->p_text;
                strcpy(l->p_text,osa[0]); l->p_text+=strlen(osa[0])+1;
           /*   l->sk_char0[k]=*osa[0]; -15.10.1995 */
                return(1);
                }
            else if (muste_strnicmp(osa[1],"STR",3)==0)
                {
                l->sk_int0[k]=0+nplus;
                l->sk_text[k]=l->p_text; osa[0][1]=EOS;
                strcpy(l->p_text,osa[0]); l->p_text+=strlen(osa[0])+1;
                                                     /* =2 */
                return(1);
                }
            else
                {
                sprintf(sbuf,"\nUnknown definition: %s",s);
                sur_print(sbuf); WAIT;
                return(-1);
                }
            }
        return(1);
        }

static int read_text(SURVO_LIST *l, int k, char *s)
        {
        int i;
        char x[LLENGTH],*osa[2];

//   printf("\nTEXT: k=%d s=%s",k,s); getch();
        strcpy(x,s);
        i=split(x,osa,2);
        korvaa_sp(osa[0]);
        l->sk_text[k]=l->p_text;
        strcpy(l->p_text,osa[0]); l->p_text+=strlen(osa[0])+1;
        if (i>1) l->sk_int0[k]=atoi(osa[1]);
        return(1);
        }

static int read_wordn(SURVO_LIST *l, int k, char *s)
        {

/*      printf("\nWORD#: k=%d s=%s",k,s); getch();  */

        l->sk_int0[k]=atoi(s);

        return(1);
        }

static int read_start(SURVO_LIST *l, int k, char *s) // onko tarpeen?
        {

/*      printf("\nWORD#: k=%d s=%s",k,s); getch();  */

        l->sk_int0[k]=atoi(s);

        return(1);
        }

static int read_nwords(SURVO_LIST *l, int k, char *s)
        {
        int i;
        char x[LLENGTH],*osa[64];
        char *p;

/*      printf("\n#WORDS: k=%d s=%s",k,s); getch();  */

        if (*s==EOS) { l->sk_int0[k]=0; return(1); }
        if (muste_strnicmp(s,"C:",2)==0) { s+=2; l->sk_char0[k]='C'; }
        else l->sk_char0[k]='U';  /* C=case-sensitive, U=Not */

        if (*s=='#') return(read_nwords_from_file(l,k,s));
        strcpy(x,s); if (l->sk_char0[k]=='U') { muste_strupr(x); muste_strupr(s); }
        i=split(x,osa,64);
        l->sk_int0[k]=i;
        p=l->sk_text[k]=l->p_text;
        strcpy(l->p_text,s); l->p_text+=strlen(s)+1;
        while (*p) { if (*p==',') *p=EOS; else if (*p=='_') *p=' '; ++p; }
        return(1);
        }


static int read_nwords_from_file(SURVO_LIST *l, int k, char *s)
        {
        int i,h;
        char x[LLENGTH],*osa[64];
        char *p;
        char koodisana[64];
        char sanat[LNAME];
        int code_found;
        int n_sanat;

        p=strchr(s,'/');
        if (p==NULL)
            {
            sprintf(sbuf,"/file missing in %s",s);
            sur_print(sbuf); WAIT; return(-1);
            }
        *p=EOS; ++p;
        strcpy(koodisana,s); strcat(koodisana,":");
        strcpy(sanat,p);
        if (strchr(p,':')==NULL) { strcpy(sanat,edisk); strcat(sanat,p); }
/* printf("\nkoodisana=%s sanat=%s",koodisana,sanat); getch(); */
        sanasto=list_fopen(sanat,"rt");
        if (sanasto==NULL)
            {
            sprintf(sbuf,"\nFile %s not found!",sanat);
            sur_print(sbuf); WAIT; return(-1);
            }
        code_found=0; n_sanat=0;
        p=l->sk_text[k]=l->p_text;
        while (1)
            {
            i=lue_rivi_sanastosta(x);
/* printf("\n%d: %s",code_found,x); getch(); */
            p=x;
            if (!code_found)
                {
                if (feof(sanasto))
                    {
                    sprintf(sbuf,"\nCode word %s not found in file %s",
                                            koodisana,sanat);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                while (*p && *p==' ') ++p;
                if (strncmp(koodisana,p,strlen(koodisana))!=0) continue;
                p+=strlen(koodisana);
                code_found=1;
                }

            if (l->sk_char0[k]=='U') muste_strupr(p);

            i=split(p,osa,64);
            for (h=0; h<i; ++h)
                {
                if (strcmp(osa[h],"#END")==0)
                    {
                    l->sk_int0[k]=n_sanat;
                    list_fclose(sanasto);
                    return(1);
                    }
                strcpy(l->p_text,osa[h]); l->p_text+=strlen(osa[h])+1;
                ++n_sanat;
                }
            }
        return(1);
        }

static int lue_rivi_sanastosta(char *x)
        {
        int m;
        char *p;

        p=x;
        while (1)
            {
            m=getc(sanasto);
            if (m==EOF || m=='\n') break;
            *p=(char)m; ++p;
            }
        *p=EOS;
        return(1);
        }

static int korvaa_sp(char *s)
        {
        int i;
        for (i=0; i<strlen(s); ++i) if (s[i]=='_') s[i]=' ';
        return(1);
        }


/* list1c.c (LIST) 3.4.1995/SM (30.4.1995) (23.10.1995) (24.3.1996) (28.4.1996)
*/

static int list_data_load(SURVO_LIST *l, int i)
        {
        int h;
        char x[LLENGTH];
        char *pval;
        int len;
        int ii; float fi; long li; double di;

        if (l->valueind[i]) return(1);
        h=list_alpha_load(l,i,x);
        if (h<0) { l->valueind[i]=-1; return(1); }
        if (l->vartype[i]!='S' && strncmp(x,space,strlen(x))==0)
            { l->valueind[i]=-1; return(1); }
        len=l->varlen[i];
        pval=l->value[i];
        switch(l->vartype[i])
            {
          case 'S': memcpy(pval,x,len); break;
          case '1': *pval=(unsigned char)atoi(x); break;
          case '2': ii=atoi(x); memcpy(pval,&ii,len); break;
          case '4': fi=atof(x); memcpy(pval,&fi,len); break;
          case 'L': li=atol(x); memcpy(pval,&li,len); break;
          case '8': di=atof(x); memcpy(pval,&di,len); break;
            }
        l->valueind[i]=1;
        return(1);
        }

static int list_data_load8(SURVO_LIST *l, int i, double *px)
        {
        int len;
        char *pval;
        char x[LLENGTH];

        list_data_load(l,i);
/* puuttuvat tiedot !!! */
        len=l->varlen[i];
        pval=l->value[i];

        switch(l->vartype[i])
            {
          case 'S': *x=EOS; strncat(x,pval,len); *px=atof(x); break;
          case '1': *px=(double)(unsigned int)*pval; break;
          case '2': *px=*(int *)pval; break;
          case '4': *px=*(float *)pval; break;
          case 'L': *px=*(long *)pval; break;
          case '8': *px=*(double *)pval; break;
            }
        return(1);
        }

static int list_alpha_load(SURVO_LIST *l, int k, char *x)
        {
        int i,h;

        h=l->host_var[k];
        if (h>=0 && l->valueind[h]==0)
            {
            i=list_alpha_load(l,h,NULL);
            if (i<0) l->valueind[h]=-1;
            else l->valueind[h]=1;
            }

        switch (l->sk_type[k])
            {
          case CONTROL:
            i=read2_control(l,k,x);
            break;
          case SHADOW:
          case nSHADOW:
          case MSHADOW:
            i=read2_shadow(l,k,x);
            break;
          case TEXT:
            i=read2_text(l,k,x);
            break;
          case nLINES:
            i=read2_nlines(l,k,x);
            break;
          case WORDn:
            i=read2_wordn(l,k,x);
            break;
          case nWORDS:
            i=read2_nwords(l,k,x);
            break;
          case START:
            i=read2_start(l,k,x);
            break;
            }
        return(i);
        }

static int read2_control(SURVO_LIST *l, int k, char *x)
        {
        int i;

        switch (l->sk_int0[k])
            {
          case 1:
            i=list_control_find(l,tempf,k,l->c2-1,x);
                                        /* len */
            if (i<0) { *x=EOS; break; }
            i=strlen(x); while (i>=0 && x[i-1]==' ') x[--i]=EOS;


            break;
            }
        if (i<0) return(-1);

        return(1);
        }

static int read2_shadow(SURVO_LIST *l, int k, char *x)
        {
        int i,h;
        int loc[4];
        int n_lines2v;
        char y[LLENGTH],ys[LLENGTH];

        switch (l->sk_int0[k])
            {
          case 0:
          case 1:
          case 10:  /* nSHADOW */
          case 11:
    /*      i=list_shad_word_find(tempf,lwidth,n_lines,l->sk_char0[k],x); -15.10.1995 */
            i=list_shad_word_find(l,tempf,k,x);
            break;
          case 2:
            loc[0]=loc[1]=0;
            i=list_txt_find(l,tempf,1,k,l->sk_text[k],0,x,loc);
                                 /* 1=shadow */    /* 0=len */
            break;
          case 3:  /* MSHADOW */
            loc[0]=loc[1]=0;
            n_lines2v=n_lines2;
            while (1)
                {
                i=list_txt_find(l,tempf,1,k,l->sk_text[k],0,x,loc);
                if (loc[2]<0) break;
/*    printf("\nn_lines=%d loc:",n_lines); for (i=0; i<4; ++i) printf(" %d",loc[i]); getch(); */
          /* kopioi tempf:n perÑÑn loc[0] - loc[3] */
                i=loc[0];
                while (i<=loc[2])
                    {
                    muste_fseek(tempf,(long)(2*i)*(long)lwidth,SEEK_SET);
                    fread(y,1,lwidth,tempf);
                    fread(ys,1,lwidth,tempf);
                    if (i==loc[0] && loc[1]>1) spaces_over(y,ys,1,loc[1]-1);
                    if (i==loc[2] && loc[3]<lwidth-1) spaces_over(y,ys,loc[3]+1,lwidth-1);
/*
printf("\n y=%.50s",y);
printf("\nys=%.50s",ys); getch();
*/
                    muste_fseek(tempf,(long)(2*n_lines2)*(long)lwidth,SEEK_SET);
                    for (h=0; h<lwidth; ++h) putc((int)y[h],tempf);
                    for (h=0; h<lwidth; ++h) putc((int)ys[h],tempf);
                    rewind(tempf);
                    ++n_lines2;

                    ++i;
                    }
                loc[0]=loc[2]; loc[1]=loc[3]+1;
                }
            l->start_line[k]=n_lines2v; l->start_pos[k]=1;
            l->end_line[k]=n_lines2-1; l->end_pos[k]=lwidth-1;
/* printf("\nstart=%d end=%d",l->start_line[k],l->end_line[k]); getch(); */
            break;

            }
        if (i<0) return(-1);

        return(1);
        }

static int spaces_over(char *x, char *xs, int i1, int i2)
        {
        int i;

        for (i=i1; i<=i2; ++i) x[i]=xs[i]=' ';
        return(1);
        }

static int read2_text(SURVO_LIST *l, int k, char *x)
        {
        int i;
        int len;

        len=l->sk_int0[k];
/*        list_txt_find(l,tempf,type,var,text,len,x)  */

        i=list_txt_find(l,tempf,0,k,l->sk_text[k],len,x,NULL);


        if (i<0) return(-1);

        return(1);
        }

static int read2_nlines(SURVO_LIST *l, int k, char *x)
        {
        int n;
        int host;

        host=l->host_var[k];
        if (host==-1) n=l->n_lines;
        else n=l->end_line[host]-l->start_line[host]+1;
        sprintf(x,"%d",n);
/* printf("\nnlines=%d",n); getch(); */

        return(1);
        }

static int read2_wordn(SURVO_LIST *l, int k, char *x) /* etsii vain 1. rivilta! */
        {
        char s[LLENGTH];
        int i,lev;
        int host;
        int l1,pos1;
        char *osa[20];
        int m;

        host=l->host_var[k];
        if (host==-1) { l1=0; pos1=1; }
        else { l1=l->start_line[host]; pos1=l->start_pos[host]; }

        rewind(tempf);

        muste_fseek(tempf,(long)(2*l1)*(long)lwidth,SEEK_SET);
        lev=l->c2;
        fread(s,1,lev,tempf); s[lev]=EOS;
        m=l->sk_int0[k];
        if (m>20) { sur_print("max m in WORD#:m is 20!"); WAIT; return(-1); }
        i=split(s+pos1,osa,m);
        if (x!=NULL)
            {
            if (i<m) *x=EOS;
            else strcpy(x,osa[m-1]);
            }
        return(1);
        }

static int read2_start(SURVO_LIST *l, int k, char *x)
        {
        char s[LLENGTH];
        int i,lev;
        int host;
        int l1,pos1;
        char *osa[20];
        int m;

        host=l->host_var[k];
        if (host==-1) { l1=0; pos1=1; }
        else { l1=l->start_line[host]; pos1=l->start_pos[host]; }

        rewind(tempf);

        muste_fseek(tempf,(long)(2*l1)*(long)lwidth,SEEK_SET);
        lev=l->c2;
        fread(s,1,lev,tempf); s[lev]=EOS;
        m=l->sk_int0[k];
        *x=EOS; strncat(x,s+1,m); // ei kontrollisaraketta!
        return(1);
        }



static int read2_nwords(SURVO_LIST *l, int k, char *x)
        {
        int n;
        int host;
        int i,i1,pos1,i2,pos2;
        char s[LLENGTH],*osa[64];
        int pos;
        int type;
        char *p,*q;
        int h,h2;
        int typ;
        char *pp;
        char ch1,ch2;
        int len;
        char sana2[64];
        int ii;

        type=l->sk_int0[k];
        host=l->host_var[k];
        if (host>=0 && l->valueind[host]==-1) { strcpy(x,"0"); return(1); }

        i=host_start_read(l,k,&i1,&pos1,&i2,&pos2);
        n=0;
        if (type>0)
            {
            p=l->sk_text[k];
            for (i=0; i<type; ++i)
                {
                q=p; psana[i]=p; while (*p) ++p;
                ii=0;
                if (*q=='*') ii=1;
                if (*(p-1)=='*') ii+=2;
                ptype[i]=ii;
                ++p;
                }
/*
printf("\nsanat: "); for (i=0; i<type; ++i) printf("%s ",psana[i]); getch();
*/
            }

        for (i=i1; i<=i2; ++i)
            {
            muste_fseek(tempf,(long)(2*i)*(long)lwidth,SEEK_SET);
            fread(s,1,lwidth,tempf); s[lwidth]=EOS;

            if (l->sk_char0[k]=='U') muste_strupr(s);

/* printf("\nlen=%d s+1=%s",strlen(s+1),s+1); getch(); */
            if (i==i1) pos=pos1; else pos=1;
            if (i==i2) s[pos2]=EOS;
/* printf("\ntype=%d k=%d: s+pos=%s",type,k,s+pos); getch(); */
            if (type==0) n+=split(s+pos,osa,64);
            else
                {
                for (h=0; h<type; ++h)
                    {
                    strcpy(sana2,psana[h]);
                    q=sana2; typ=ptype[h]; len=strlen(q);
                    if (*q=='*') { ++q; --len; }
                    if (q[len-1]=='*') q[--len]=EOS;

                    p=pp=s+pos;
                    while (1)
                        {
/*       printf("\np=%s\n k=%d typ=%d q=%s",p,k,typ,q); getch(); */

                        p=strstr(p,q);
                        if (p==NULL) break;
                        ch1=*(p-1); ch2=*(p+len);
                        switch (typ)
                            {
                          case 0:  /* text */
                            if ( (p==pp || strchr(sanav,ch1)!=NULL) &&
                                 (ch2==EOS || strchr(sanav,ch2)!=NULL) ) ++n;
                            p+=len; break;
                          case 1:  /* *text */
                            if (ch2==EOS || strchr(sanav,ch2)!=NULL) ++n;
                            p+=len; break;
                          case 2:  /* text* */
                            if (p==pp || strchr(sanav,ch1)!=NULL) ++n;
                            p+=len; break;
                          case 3: ++n; p+=len; break;
                            }

                         /* if (strcmp(q,osa[h2])==0) ++n; break; */

                        }
                    }
                }
            }
/* if (type) { printf("\nn=%d",n); getch(); } */
/*             printf("\nn=%d",n); getch();  */

        sprintf(x,"%d",n);
        return(1);
        }

/* list2.c (LIST operations) 25.8.1991/SM (2.4.1995)
*/

static int list_rewind(SURVO_LIST *l, char *muoto)
        {
        int i;
        i=list_chapter_open(l,0,muoto);
        if (i<0) return(-1);
        l->i=0;
        l->j=l->line1[0];
        return(1);
        }

static void list_close(SURVO_LIST *l)
        {
        list_fclose(text);
        }

/* ei kaytossa!
list_read2(l,x,xs,pchapter)
SURVO_LIST *l;
char *x;
char *xs;
int *pchapter;
        {
        int i,j;
        char xs2[LLENGTH];

        j=l->j;
        if (j>l->line2[l->i])
            {
            list_fclose(text);
            ++l->i;
            i=l->i;
            if (i>=l->n) return(0);
            i=list_chapter_open(l,l->i,"r+b");
            if (i<0) return(-1);
            l->j=l->line1[l->i];
            }
        tedread(x,l->j);
        *pchapter=l->i;

        if (xs!=NULL)
            {
            if (tzs[l->j]==0) { strncpy(xs,space,ted1); xs[ted1]=EOS; }
            else { tedread(xs2,tzs[l->j]); strcpy(xs,xs2+2); strcat(xs,"  "); }
            }
        ++l->j;
        return((l->j)-1);
        }
*/

static int list_next_line_read(SURVO_LIST *l, char *x, char *xs, int *pchapter)
        {
        int i,j;
        char xs2[LLENGTH];

// Rprintf("\ni=%d j=%d line2=%d",l->i,l->j,l->line2[l->i]);

        j=l->j;
        if (j>l->line2[l->i])
            {
            list_fclose(text);
            ++l->i;
            i=l->i;
            if (i>=l->n) return(0);
            i=list_chapter_open(l,l->i,"r+b");
            if (i<0) return(-1);
            l->j=l->line1[l->i];
            }
        tedread(x,l->j);
        *pchapter=l->i;
        if (xs!=NULL)
            {
            if (tzs[l->j]==0) { strncpy(xs,space,ted1); xs[ted1]=EOS; }
            else { tedread(xs2,tzs[l->j]); strcpy(xs,xs2+2); strcat(xs,"  "); }
            }
        ++l->j;
/*
printf("LIST2:\n");
printf(" x=%.60s\n",x);
printf("xs=%.60s\n",xs); getch();
*/
        return((l->j)-1);
        }

static int list_seek_line(SURVO_LIST *l, int chapter, int line)
        {
        int i;
/* char x[LLENGTH]; */

        i=list_edit_open(l,chapter,"rb");
        if (i<0) return(-1);
        muste_fseek(text,(long)ted1*(long)line,0);
/* fread(x,1,41,text); printf("\nx=%.40s",x); getch(); */
        l->i=chapter; l->j=line;
        return(1);
        }

static int list_chapter_open(SURVO_LIST *l, int k, char *muoto)
        {
        int i;
        char chapter[LNAME];
        i=list_edit_open(l,k,muoto);
        if (i<0) return(-1);
        strcpy(chapter,l->chapter[k]);
        i=list_find_chapter(l,k,chapter); if (i<0) return(-1);
        return(1);
        }

static int list_edit_open(SURVO_LIST *l, int k, char *muoto)
        {
        int i;
        char tfile[LLENGTH];
        char rivi[ELE];
        char *sana[5];
        char nimi[LNAME];

        strcpy(tfile,l->editfile[k]);
/*      disp_label(l,k,rdisp+1);       */

        if (!muste_is_path(tfile))
            {
            if (*tfile=='.') strcpy(nimi,esysd); else strcpy(nimi,list_path);
                                                                  /* 15.8.92 */
            strcat(nimi,tfile);
            strcpy(tfile,nimi);
            }
        strcpy(nimi,tfile);
        muste_append_path(nimi,".EDT");
        text=muste_fopen2(nimi,muoto);
        if (text==NULL)
            {
            sprintf(sbuf,"\nEdit file %s not found!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        muste_fclose2(text);

		text=list_fopen(nimi,muoto);
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(text);
        rivi[ELE-1]=EOS;
        i=split(rivi,sana,5);
        if (i<3) { rewind(text); list_fclose(text); return(-1); }
        if (strcmp(sana[0],"SURVO84ED")!=0) { rewind(text); list_fclose(text); return(-1); }
        ted1=atoi(sana[1]); ted2=atoi(sana[2]);
        if (i>4 && *sana[4]=='S') tedshad=atoi(sana[4]+1);
        l->c2=ted1-1; l->r2=ted2; l->shad=tedshad;
		rewind(text);
        list_init_shadows();
        rewind(text);

/*
printf("\n");
for (i=0; i<n_shad; ++i) printf("%d ",shad_int[i]); getch();
*/
        return(1);
        }

static int list_init_shadows()
        {
        int i,k;
        long l;
        unsigned int j;
        unsigned short *pint;
        char x[3];

        tzs=(int *)muste_realloc(tzs,(ted2+1)*sizeof(int));
        if (tzs==NULL) return(-1);        
        for (i=1; i<=ted2; ++i) tzs[i]=0;

        pint=(unsigned short *)x;
        n_shad=0;
        l=(long)ted1*(long)(ted2+2);
        while (1)
            {
            muste_fseek(text,l,0);
            for (i=0; i<3; ++i) x[i]=(char)getc(text);
            if (strncmp(x,"END",3)==0) break;
            if (feof(text)) break;
            j=*pint;
            tzs[j]=ted2+2+n_shad; ++n_shad;
     /*     shad_int[n_shad++]=j;        */
            l+=ted1;
            }
        return(1);
        }

static int list_find_chapter(SURVO_LIST *l, int k, char *chapter)
        {
        int i,j;
        char x[LLENGTH],*osa[4];

        if (l->line1[k]!=0) return(1);

        if (strcmp(chapter,"*")==0)
            {
            l->line1[k]=1;
            l->line2[k]=tlastline()+1; /* +1 28.8.1995 */
            return(1);
            }
        for (j=1; j<=ted2; ++j)
            {
            tedread(x,j);
            i=split(x+1,osa,1);
            if (strcmp(osa[0],"DEF")!=0) continue;
            tedread(x,j);
            i=split(x+1,osa,4);
            if (strcmp(osa[1],chapter)!=0) continue;
            if (i<4)
                {
                tedread(x,j);
        sprintf(sbuf,"\nError in chapter definition: %s (Edit field %s)",x+1,l->editfile[k]);
                sur_print(sbuf); WAIT; return(-1);
                }
            i=etsi_rivi(osa[2],1); if (i==0) { line_not_found(osa[2]); return(-1); }
            l->line1[k]=i;
            i=etsi_rivi(osa[3],i); if (i==0) { line_not_found(osa[3]); return(-1); }
            l->line2[k]=i+1; /* +1 28.8.1995 */
            return(1);
            }
        sprintf(sbuf,"\nChapter %s not found in %s !",chapter,l->editfile[k]);
        sur_print(sbuf); WAIT; return(-1);
        }

/* list3.c (LIST operations) 4.4.1995/SM (10.4.1995) (23.10.1995) (26.3.1996)
*/

static int list_txt_find(
SURVO_LIST *l,
FILE *tempf,
int type,  /* 0=edit line, 1=shadow line */
int var,   /* current field */
char *text, /* text to be found */
int len,    /* length of value after text */
char *value, /* NULL if not needed */
int *loc) /* MSHADOW: start and end positions */
        {
        char s[LLENGTH];
        char *p;
        int i,k,h,host;
        int l1,pos1,l2,pos2;
        int ps1,ps2;
        int lev;
        int end_type;

        end_type=l->sk_type[var+l->m];
        i=host_start_read(l,var,&l1,&pos1,&l2,&pos2);
        if (i<0) { l->valueind[var]=-1; return(-1); }

        if (loc!=NULL)
            {
            if (loc[0]>l1) { l1=loc[0]; pos1=loc[1]; }
            else if (loc[0]==l1 && loc[1]>pos1) pos1=loc[1];
            }
/* printf("\nlist3: l1=%d l2=%d",l1,l2); getch(); */

        rewind(tempf);
        k=type;
        for (i=l1; i<=l2; ++i)
            {
            ps1=0; if (i==l1) ps1=pos1;
            ps2=l->c2-1; if (i==l2) ps2=pos2;
            muste_fseek(tempf,(long)(2*i+k)*(long)lwidth+(long)ps1,SEEK_SET);
            lev=ps2-ps1+1;
            fread(s,1,lev,tempf); s[lev]=EOS;
            p=strstr(s,text);
            if (p!=NULL)
                {
                l->start_line[var]=i;
                l->start_pos[var]=(p-s)+ps1+(1-type)*strlen(text);

                if (loc!=NULL)
                    {
                    loc[0]=l->start_line[var]; loc[1]=l->start_pos[var];
                    }

                if (end_type==-1 && value!=NULL)
                    {
                    strncpy(value,s+l->start_pos[var],len); value[len]=EOS;
                    }
/*     printf("\nvar=%d l=%d pos=%d",var,l->start_line[var],l->start_pos[var]); getch();  */
                if (end_type==-1) return(1);
                h=list_field_end(l,tempf,var,loc);
                if (h<0) return(-1);
                if (value!=NULL)
                    {
                    if (type==1)
                        {
                        i=l->start_line[var];
                        muste_fseek(tempf,(long)(2*i+0)*(long)lwidth+(long)ps1,SEEK_SET);
                        lev=l->c2;
                        fread(s,1,lev,tempf); s[lev]=EOS;
                        }
                    if (l->end_line[var]==l->start_line[var])
                        {
                        h=l->end_pos[var]-l->start_pos[var]+1;
                        strncpy(value,s+l->start_pos[var],h); value[h]=EOS;
                        }
                    }
                return(1);
                }  /* p!=NULL */
            } /* i */

        if (loc!=NULL) { loc[2]=-1; return(1); }

        l->valueind[var]=-1; if (!list_check) return(-1);

        sprintf(sbuf,"\nStart of field %s not found!",text);
        sur_print(sbuf); WAIT; return(-1);
        }

static int host_start_read(SURVO_LIST *l, int var, int *pl1, int *ppos1, int *pl2, int *ppos2)
        {
        int host;

        host=l->host_var[var];
        if (host>=0 && l->valueind[host]==-1) return(-1);
        if (host==-1)
            { *pl1=0; *ppos1=0; *pl2=l->n_lines-1; *ppos2=l->c2-1; }
/* -23.10.95 { *pl1=0; *ppos1=0; *pl2=2*(l->n_lines-1); *ppos2=l->c2-1; }  */
        else
            { *pl1=l->start_line[host]; *ppos1=l->start_pos[host];
              *pl2=l->end_line[host]; *ppos2=l->end_pos[host];
            }
        return(1);
        }

static int list_control_find(
SURVO_LIST *l,
FILE *tempf,
int var,   /* current field */
int len,
char *value) /* NULL if not needed */
        {
        char s[LLENGTH];
        int i,lev;
        int l1,pos1,l2,pos2;
        int end_type;
        char ch;

        end_type=l->sk_type[var+l->m];
        ch=l->sk_char0[var];
        host_start_read(l,var,&l1,&pos1,&l2,&pos2);
        rewind(tempf);

        for (i=l1; i<=l2; ++i)
            {
            muste_fseek(tempf,(long)(2*i)*(long)lwidth,SEEK_SET);
            lev=l->c2;
            fread(s,1,lev,tempf); s[lev]=EOS;
            if (*s==ch)
                {
                l->start_line[var]=i; l->start_pos[var]=1;
                break;
                }
            } /* i */
        if (i>l2)
            {
            l->valueind[var]=-1; if (!list_check) return(-1);
            sprintf(sbuf,"\nControl character %c not found!",ch);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (end_type==-1 && value!=NULL)
            {
            strncpy(value,s+l->start_pos[var],len); value[len]=EOS;
            }
/*     printf("\nvar=%d l=%d pos=%d",var,l->start_line[var],l->start_pos[var]); getch(); */
        if (end_type==-1)
            {
            l->end_line[var]=l->start_line[var];
            l->end_pos[var]=l->c2;
/*     printf("\n pos2=%d",l->end_pos[var]); getch();  */

            return(1);
            }
        i=list_field_end(l,tempf,var,NULL);
 /*     i=list_field_end_control(l,tempf,var); */
        return(i);
        }

static int list_shad_word_find(SURVO_LIST *l, FILE *tempf, int var, char *x)
        {
        int i,k,h,h1,h2,pos;
        int l1,pos1,l2,pos2;
        char s[LLENGTH],s2[LLENGTH];
        char *p,*chs;
        int type;
        int count,n;

        type=l->sk_int0[var];
        count=0; if (type>9) { count=1; n=0; type-=10; }

        if (x==NULL)
            {
            sur_print("\nlist3.c: list_shad_word_find: x=NULL");
            WAIT; return(-1);
            }
        host_start_read(l,var,&l1,&pos1,&l2,&pos2);
        rewind(tempf);
        chs=l->sk_text[var];
        i=0; k=2*l1+1;
        for (i=l1; i<=l2; ++i)
            {
            muste_fseek(tempf,(long)k*(long)lwidth,SEEK_SET);
            fread(s,1,lwidth,tempf); s[lwidth]=EOS;
            if (i==l1) pos=pos1; else pos=1;
            if (i==l2) s[pos2+1]=EOS;
            while (1)
                {
                p=strstr(s+pos,chs);
                if (p!=NULL)
                    {
                    h=p-s;
                    muste_fseek(tempf,(long)(k-1)*(long)lwidth,SEEK_SET);
                    fread(s2,1,lwidth,tempf); s2[lwidth]=EOS;
                    if (type==0)
                        {
                        h1=h+1;
                        while (s[h1]==*chs) ++h1;
                        if (!count)
                            {
                         /* strncpy(x,s2+h,h1-h+1); x[h1-h+1]=EOS; -26.3.96 */
                            strncpy(x,s2+h,h1-h); x[h1-h]=EOS;
                            return(1);
                            }
                        ++n;
                        pos=h1;
                        continue;
                        }
                    /* type=1 */
                    h2=h;
                    while (h>0 && s2[h]==' ') --h;
                    if (h==0)
                        {
                        if (!count)
                            { *x=EOS; return(1); }
                        ++n; pos=h2+1;
                        continue;
                        }
                    h1=h;
                    while (h1>0 && s2[h1]!=' ') --h1;
                    while (s2[h]>0 && s2[h]!=' ') ++h;
                    if (!count)
                        {
                        strncpy(x,s2+h1,h-h1+1); x[h-h1+1]=EOS;
                        return(1);
                        }
                    ++n;
                    pos=h; if (h2+1>h) pos=h2+1;
                    continue;
                    } /* p!=NULL */
                if (p==NULL) break;
                } /* while */
            k+=2;
            } /* for i  */
        if (count)
            {
            sprintf(x,"%d",n);
/* printf("\ncount=%s",x); getch(); */
            return(1);
            }
        if (i>l2)
            {
            *x=EOS;
            if (!list_check) return(1);
            if (type==1)
                sprintf(sbuf,"\nWord with shadows `%s' not found!",chs);
            else
                sprintf(sbuf,"\nString painted by shadow `%s' not found!",chs);
            sur_print(sbuf); WAIT; return(-1);

            }
        return(1);
        }

static int list_field_end(SURVO_LIST *l, FILE *tempf, int var, int *loc)
        {
        int i;
/* printf("\nloppukoodi=%d",l->sk_type[var+l->m]); getch(); */
        switch (l->sk_type[var+l->m])
            {
          case TEXT:
            i=list_field_end_text(l,tempf,0,var,NULL);
            break;
          case END:
            l->end_line[var]=l->n_lines-1; l->end_pos[var]=l->c2;
            i=1;
            break;
          case SHADOW:
            i=list_field_end_text(l,tempf,1,var,NULL);
                                       /* 1=shadow */
            break;
          case MSHADOW:
            i=list_field_end_text(l,tempf,1,var,loc);
                                       /* 1=shadow */
            break;
          case CONTROL:
            i=list_field_end_control(l,tempf,var);
            break;
            }
        return(i);
        }


static int list_field_end_text(
SURVO_LIST *l,
FILE *tempf,
int type,  /* 0=edit line, 1=shadow line */
int var,   /* current field */
int *loc)
        {
        char s[LLENGTH];
        char *p;
        int i,k,h,host;
        int l1,pos1,l2,pos2;
        int ps1,ps2;
        int lev;
        char *text;

        text=l->sk_text[var+l->m];
/* printf("\ntext=%s",text); getch(); */
        host=l->host_var[var];
        l1=l->start_line[var]; pos1=l->start_pos[var];
        if (host==-1)
            { l2=l->n_lines-1; pos2=l->c2-1; }
/* -23.10,95 { l2=2*(l->n_lines-1); pos2=l->c2-1; } */
        else
            {
            l2=l->end_line[host]; pos2=l->end_pos[host];
            }
        rewind(tempf);
        k=type;
        for (i=l1; i<=l2; ++i)
            {
            ps1=0; if (i==l1) ps1=pos1;
            ps2=l->c2-1; if (i==l2) ps2=pos2;
            muste_fseek(tempf,(long)(2*i+k)*(long)lwidth+(long)ps1,SEEK_SET);
            lev=ps2-ps1+1;
            fread(s,1,lev,tempf); s[lev]=EOS;
            p=strstr(s,text);
            if (p!=NULL)
                {
                l->end_line[var]=i; l->end_pos[var]=ps1+(p-s)-(1-type);
                if (l->start_pos[var]<0)
                    { --l->end_line[var]; l->end_pos[var]=l->c2-1; }

                if (loc!=NULL)
                    {
                    loc[2]=l->end_line[var]; loc[3]=l->end_pos[var];
                    }

/*        printf("\nend: l=%d pos=%d",l->end_line[var],l->end_pos[var]); getch(); */
                return(1);
                }
            }
        l->valueind[var]=-1; if (!list_check) return(-1);
        sprintf(sbuf,"\nEnd of field %s not found!",text);
        sur_print(sbuf); WAIT; return(-1);
        }


static int list_field_end_control(SURVO_LIST *l, FILE *tempf, int var)   /* current field */
        {
        int i,lev;
        int l1,pos1,l2,pos2;
        char ch;
        char s[LLENGTH];

        host_start_read(l,var,&l1,&pos1,&l2,&pos2);
        l1=l->start_line[var];
        ch=l->sk_char0[var+l->m];

        for (i=l1; i<=l2; ++i)
            {
            muste_fseek(tempf,(long)(2*i)*(long)lwidth,SEEK_SET);
            lev=l->c2;
            fread(s,1,lev,tempf); s[lev]=EOS;
            if (*s==ch)
                {
                l->end_line[var]=i; l->end_pos[var]=l->c2;
                break;
                }
            } /* i */
        if (i>l2)
            {
            l->valueind[var]=-1; if (!list_check) return(-1);
            sprintf(sbuf,"\nControl character %c not found!",ch);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(1);
        }

/* list4.c 16.4.1995/SM (29.4.1995)
*/

static int lst_save(
char *comment,
char *newlist_path,
char *newlist,
int n,
char case_start,
char case_end)
        {
        int i;
        char nimi[LLENGTH];
/*
        printf("\nnew: %s %s %d %c %c",newlist_path,newlist,n,case_start,case_end); getch();
*/
        sprintf(nimi,"%s%s.LST",newlist_path,newlist);
        lst_file=muste_fopen2(nimi,"wt");
        if (lst_file==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s for a new list structure!",
                          nimi);
            sur_print(sbuf); WAIT; return(-1);
            }

        fprintf(lst_file,"/ %s\n",comment);

        if (n==1)
            fprintf(lst_file,"A %s%d\n",newlist,n);
        else
            fprintf(lst_file,"A %s%d-%d\n",newlist,1,n);
        fprintf(lst_file,"END\n");
        if (case_start!=EOS)
            fprintf(lst_file,"CASE_START=%c\n",case_start);
        if (case_end!=EOS)
            fprintf(lst_file,"CASE_END=%c\n",case_end);

        fprintf(lst_file,"LIST FIELDS:\n");
        sprintf(nimi,"%sSURVO3.TMP",etmpd);
        tmp3=muste_fopen2(nimi,"rt");
        while (1) { i=getc(tmp3); if (i==EOF) break; putc(i,lst_file); }
        muste_fclose2(tmp3);
        fprintf(lst_file,"END\n");

        muste_fclose2(lst_file);
        return(1);
        }


/* SHOW.C (LIST SHOW) 18.7.1991/SM (24.5.1992)
*/

static void op_list_show(int argc, char *argv[])
        {
        int i;
        char x[LLENGTH];

//        if (argc==1) return;
        s_init(argv[1]);
        if (r_soft) r3+=r_soft+1;

        if (g<3)
            {
            init_remarks();
            rem_pr("Usage: LIST SHOW <name_of_list>");
rem_pr("List of text chapters has to be defined in the current edit field in the form:");
            rem_pr("LIST <name_of_list>:");
            rem_pr("  <name_of_chapter_1>,<name_of_edit_file_1>");
            rem_pr("  <name_of_chapter_2>,<name_of_edit_file_2>");
            rem_pr("  ...");
            rem_pr("END (or empty line)");
            rem_pr(" ");
            rem_pr("Each chapter must be defined by DEF in the corresponding edit field.");
            rem_pr("* as a name of a chapter implies the entire edit field to be included.");
            rem_pr(" ");
            rem_pr("LIST SHOW *");
            rem_pr("works with the list used most recently by LIST SHOW.");

            wait_remarks(2);
            return;
            }
        tut_init();
        strcpy(lista,word[2]);
        i=spec_init(r1+r-1); if (i<0) return;

        if (strcmp(lista,"*")==0)
            i=hae_valmis_lista();
        else
            i=list_open(&list,lista,1);
        if (i<0) { ei_listaa(lista); return; }
        selaa();
        tut_end();
        if (r_soft) r3-=r_soft+1;
        s_end(argv[1]);
        }

static void ei_listaa(char *s)
        {
        sprintf(sbuf,"\nLIST %s:  not defined!",s);
        if (etu==2)
            {
            strcpy(tut_info,"˛˛˛@13@LIST SHOW@");
            strcat(tut_info,sbuf); strcat(tut_info,"@");
            return;
            }
        sur_print(sbuf);
        WAIT; return;
        }

static void listaan(SURVO_LIST *l, int n, char *kpl, char *kent)   /* show5.c tarvitsee */
        {
        strncpy(pl1,kpl,(LEN_LISTNAME-1)/2);
        *(pl1+(LEN_LISTNAME-1)/2)=EOS;
        l->chapter[n]=pl1; pl1+=(LEN_LISTNAME-1)/2+1;
        strncpy(pl2,kent,LEN_LISTNAME-1);
        *(pl2+LEN_LISTNAME-1)=EOS;
        l->editfile[n]=pl2; pl2+=LEN_LISTNAME;
        l->line1[n]=0;
        }

static int varaa_tilat(SURVO_LIST *l)  /* show5.c tarvitsee */
        {
        int nmax;

        nmax=65536/LEN_LISTNAME-1;
        if (l->n > nmax)
            {
            sprintf(sbuf,"\nToo many (>%d) chapters in LIST definition!",nmax);
            sur_print(sbuf); WAIT; return(-1);
            }
        l->listspace1=muste_malloc(l->n*((LEN_LISTNAME-1)/2+1));
        if (l->listspace1==NULL) { not_enough_memory(); return(-1); }
        l->listspace2=muste_malloc(l->n*LEN_LISTNAME);
        if (l->listspace2==NULL) { not_enough_memory(); return(-1); }
        l->chapter=(char **)muste_malloc(l->n*sizeof(char **));
        if (l->chapter==NULL) { not_enough_memory(); return(-1); }
        l->editfile=(char **)muste_malloc(l->n*sizeof(char **));
        if (l->editfile==NULL) { not_enough_memory(); return(-1); }
        l->line1=(int *)muste_malloc(l->n*sizeof(int));
        if (l->line1==NULL) { not_enough_memory(); return(-1); }
        l->line2=(int *)muste_malloc(l->n*sizeof(int));
        if (l->line2==NULL) { not_enough_memory(); return(-1); }

        return(1);
        }
/*
list_varfind() {}     //  list1.c
list_field_def() {}
*/

/* SHOW2.C (LIST SHOW) 19.7.1991/SM (24.5.1992) (9.11.1996)
*/

static int selaa()
        {
        int i,k,m;
        char x[LLENGTH];

        varjohaku=0;
        r3disp=r3;
        luo_ikkuna();
        ndisp1=ndisp;
        lopputavut=1;
        isot_kirjaimet=0;
        rivin_alusta=0;
        alkupos=1;

        if (strcmp(lista,"*")!=0)
            {
            il=0;
            sar1=1;
            end_of_list=0;
            sr=sc=1;
            i=avaa(&list,il,"rb"); if (i<0) return(-1);
            ensrivi=list.line1[il];
            }
        else  /* valmis lista */
            {
            strcpy(lista,lista2);
            i=avaa(&list,il,"rb"); if (i<0) return(-1);
            }
        n_haku=0L;
        strcpy(message,"To stop, press F8!    F1=HELP");
        ensil=il;
        i=nayta(); if (i<0) return(-1);
        while (1)
            {
            putsaa();
            paikka(sr,sc);
            write_string(message,strlen(message),'7',r3disp+2,1);
            m=nextch("");
            switch (m)
                {
              case CODE_EXIT: break;
              case CODE_NEXT:
                    if (il==list.n-1 && viimrivi==list.line2[il]) break;
                    ensrivi=viimrivi+1;
                    ensil=il;
                    i=nayta(); if (i<0) return(-1); break;
              case CODE_PREV:
                    if (ensil<il)
                        {
                        end_of_list=0;
                        il=ensil;
                        list_fclose(text);
                        i=avaa(&list,il,"rb"); if (i<0) return(-1);
                        k=ensrivi-ndisp+1;
                        if (k<list.line1[il]) k=list.line1[il];
                        ensrivi=k;
                        i=nayta(); if (i<0) return(-1); break;
                        }
                    if (ensrivi==list.line1[il])
                        {
                        if (il==0) break;
                        end_of_list=0;
                        --il;
                        list_fclose(text);
                        i=avaa(&list,il,"rb"); if (i<0) return(-1);
                        k=list.line2[il]-ndisp+2;
                        if (k<list.line1[il]) k=list.line1[il];
                        }
                    else
                        {
                        end_of_list=0;
                        k=ensrivi-ndisp+1;
                        if (k<list.line1[il]) k=list.line1[il];
                        }
                    end_of_list=0;
                    ensrivi=k;
                    i=nayta(); if (i<0) return(-1); break;
              case CODE_RIGHT:
                    if (sc<c3) { ++sc; break; }
                    ++sar1; nayta(); break;
              case CODE_LEFT:
                    if (sc>1) { --sc; break; }
                    if (sar1==1) break;
                    --sar1; nayta(); break;
              case CODE_DOWN:
                    askel_alas();
                    break;
              case CODE_UP:
                    if (sr>1) { --sr; break; }
                    if (ensil==il && ensrivi>list.line1[il])
                        {
                        --ensrivi;
                        SCROLL_DOWN(rdisp+1,r3disp,1);
                        --viimrivi;
                        nayta_rivi(ensrivi,rdisp+2);
                        }
                    else if (ensil<il)
                        {
                        il=ensil-1;
                        seur_kentta();
                        if (ensrivi>list.line1[il]) --ensrivi;
                        nayta();
                        }
                    else   /* ensil=il ensrivi=line1 */
                        {
                        if (il==0) break;
                        il-=2;
                        seur_kentta();
                        ensrivi=list.line2[il];
                        nayta();
                        }
                    break;
              case CODE_HOME:
                    if (sc>1) { sc=1; break; }
                    if (sar1==1)
                        {
                        if (sr>1) { sr=1; break; }
                        if (ensrivi==list.line1[il])
                            {
                            list_fclose(text);
                            il=0; avaa(&list,il,"rb");
                            ensrivi=list.line1[il];
                            }
                        else
                            {
                            ensrivi=list.line1[il];
                            }
                        }
                    else
                        {
                        sar1=1;
                        }
                    nayta(); break;
              case CODE_RETURN:
                    sc=1; askel_alas(); break;
              case CODE_PRE:
                    i=prefix(); if (i<0) return(-1);
                    break;
              case 'N':
              case 'n':
                    i=seur_kentta();
                    if (i==-2) break;
                    if (i<0) return(-1);
                    ensrivi=list.line1[il];
                    ensil=il;
                    nayta();
                    break;
              case 'P':
              case 'p':
                    il=ensil-2; if (il<-1) il=-1;
                    i=seur_kentta();
                    if (i<0) return(-1);
                    ensrivi=list.line1[il];
                    nayta();
                    break;

              case 'Q':    /* 24.5.1992 */
              case 'q':
              case '.':
                    listan_talletus(0);
                    return(999);

              case CODE_END:
                    i=nayta();
                    if (i==0) break;
                    i=strlen(rrivi);
                    while (i>0 && rrivi[i-1]==' ') --i;

                    if (i>=sar1 && i<sar1+c3) { sc=i-sar1+1; break; }
                    if (i<sar1)
                        {
                        sar1=i-c3+1;
                        if (sar1<1) sar1=1;
                        sc=i-sar1+1;
                        nayta();
                        break;
                        }
                    sar1=i-c3+1;
                    sc=c3;
                    nayta();
                    break;

              case CODE_SRCH:
                    vaihehaku(0); /* 0 9.11.1996 */
                    break;

              case CODE_EXEC:
                    vaihehaku(1); /* 9.11.1996 */
                    break;

              case CODE_REF:
                    if (!ref_ensrivi)
                        {
                        ref_ensil=ensil;
                        ref_ensrivi=ensrivi;
                        ref_sar1=sar1;
                        ref_sr=sr;
                        ref_sc=sc;
                        break;
                        }
                    if (ref_ensil==ensil && ref_ensrivi==ensrivi &&
                        ref_sar1==sar1 && ref_sr==sr && ref_sc==sc)
                        {
                        ref_ensrivi=0;
                        break;
                        }
                    ensil=ref_ensil; ensrivi=ref_ensrivi;
                    sar1=ref_sar1; sr=ref_sr; sc=ref_sc;
                    nayta();
                    break;

              case CODE_HELP:
                    help();
                    break;
              case 'S':
              case 's':
                    optiot();
                    break;

              case '0':
                    n_haku=0L;
                    break;
              case 'E':
              case 'e':
                    lopputavut=1-lopputavut;
                    break;
              case 'U':
              case 'u':
                    isot_kirjaimet=1-isot_kirjaimet;
                    break;
              case 'B':
              case 'b':
                    rivin_alusta=1-rivin_alusta;
                    break;
              case 'V':
              case 'v':
                    varjohaku=1-varjohaku;
                    vrivit();
                    nayta();
                    break;

                }
            if (m==CODE_EXIT) break;
            }
        listan_talletus(1);
        return(1);
        }

static int prefix()
        {
        int i,m;

        write_string(message,strlen(message),'7',r3disp+2,1);
        m=nextch("");
        switch (m)
            {
          case CODE_DOWN:
                if (sr<ndisp-1) { sr=ndisp-1; break; }
                if (viimrivi==list.line2[il])
                    {
                    il=list.n-1;  ensil=il;
                    avaa_text(il);
                    }
                ensrivi=list.line2[il]-ndisp+2;
                if (ensrivi<list.line1[il]) ensrivi=list.line1[il];
                i=nayta(); if(i<0) return(-1);
                break;
          case CODE_PRE:
                if (sc<c3) { sc=c3; break; }
                sar1+=c3;
                if (sar1+c3>ted1)
                    {
                    sar1=ted1-c3;
                    if (sar1<1) sar1=1;
                    }
                nayta();
                break;
          case '0':
                n_haku=0L;
                break;
          case 'E':
          case 'e':
                lopputavut=1-lopputavut;
                break;
          case 'U':
          case 'u':
                isot_kirjaimet=1-isot_kirjaimet;
                break;
          case 'B':
          case 'b':
                rivin_alusta=1-rivin_alusta;
                break;


            }
        return(1);
        }

static void askel_alas()
        {
        char ms[10],ms2[10];

        if (sr<ndisp-1) { ++sr; return; }
        if (viimrivi==list.line2[il])
            {
            if (il==list.n-1)
                {
                if (end_of_list) return;
                SCROLL_UP(rdisp+1,r3disp,1);
                LOCATE(r3disp+1,1); PR_EIN2;
                sprintf(sbuf,"%.*s",c3+8,space);
                sur_prin2(sbuf,r3disp+1,1);
                LOCATE(r3disp+1,1);
                sprintf(sbuf," END OF LIST %s",lista);
                sur_prin2(sbuf,r3disp+1,1);
                PR_ENRM;
                end_of_list=1;
                return;
                }
            ++ensrivi;
            SCROLL_UP(rdisp+1,r3disp,1);
            seur_kentta();
            disp_label(&list,il,r3disp+1);
            viimrivi=list.line1[il]-1;
            PR_ENRM;
            return;
            }
        read_string(ms,ms2,1,rdisp+2,1);
        if (*ms==NELIO)
            {
            read_string(ms,ms2,7,rdisp+3,1);
            ensrivi=atoi(ms);
            ++ensil;
            ++viimrivi;
            SCROLL_UP(rdisp,r3disp,1);
            }
        else
            {
            SCROLL_UP(rdisp+1,r3disp,1);
            ++ensrivi;
            ++viimrivi;
            }
        nayta_rivi(viimrivi,r3disp+1);
        }

static void paikka(int sr, int sc)
        {
        LOCATE(sr+rdisp+1,sc+8);
        }

static void luo_ikkuna()
        {
        int i;

        if (r3disp-r<5) rdisp=1; else rdisp=r+1;
        ndisp=r3disp-rdisp+1;

        sprintf(sbuf,"%.*s",c3+8,space);
        LOCATE(rdisp+1,1); PR_EIN2;
        sur_prin2(sbuf,rdisp+1,1);
        PR_ENRM;
        for (i=rdisp+2; i<r3disp+2; ++i)
            {
            LOCATE(i,1);
            sur_prin2(sbuf,i,1);
            }
        putsaa();
        }

static void putsaa()
        {
        int i;

        LOCATE(r3disp+2,1); PR_EINV;
        sprintf(sbuf,"%.*s",c3+8,space); sur_prin2(sbuf,r3disp+2,1);

        if (r3disp<r3)
            {
            PR_ENRM;
            for (i=r3disp+1; i<=r3; ++i)
                {
                LOCATE(i+2,1);
                sur_prin2(sbuf,i+2,1);
                }
            }
        }

static void disp_label(SURVO_LIST *l, int k, int rivi)
        {
        int i;

        i=sprintf(sbuf,"%c%8d   LIST %s: %s in %s",
                          NELIO,sar1,lista,l->chapter[k],l->editfile[k]);
        if (l->line1[k]!=0)
            i+=sprintf(sbuf+i," (lines %d-%d)",l->line1[k],l->line2[k]);
        sprintf(sbuf+i,"%.*s",c3+8-i,space);
        write_string(sbuf,c3+8,'8',rivi,1);
        }

static void vrivit()
        {
        if (varjohaku)
            {
            r3disp=r3-1; ndisp=ndisp1-1;
            if (sr>ndisp-1) sr=ndisp-1;
            }
        else
            {
            r3disp=r3; ndisp=ndisp1;
            }
        }

static int sur_prin2(char *s, int row, int col)
    {
    write_string(s,strlen(s),(unsigned char)sdisp,row,col);
    return(1);
    }

/* SHOW3.C (LIST SHOW) 19.7.1991/SM (24.5.1992)
*/

static int nayta()
        {
        int i,k;
        int riv;
        int nykyrivi;
        int ir;

        nykyrivi=sr;   /* 24.1.1992 */
        if (ensil<il)
            {
            list_fclose(text);
            il=ensil;
            avaa(&list,il,"rb");
            }
        disp_label(&list,il,rdisp+1);
        riv=ensrivi; ensil=il;
        CURSOR_OFF;
        PR_ENRM;
        lev=c3; if (ted1-sar1<c3) lev=ted1-sar1;
        for (i=rdisp+2,ir=1; i<r3disp+2; ++i,++ir)
            {
            if (riv>list.line2[il])
                {
                k=seur_kentta();
                if (k==-2)
                    {
                    LOCATE(i,1); PR_EIN2;
                    sprintf(sbuf,"%.*s",c3+8,space);
                    sur_prin2(sbuf,i,1);
                    LOCATE(i,1);
                    sprintf(sbuf," END OF LIST %s",lista);
                    sur_prin2(sbuf,i,1);
                    if (sr>ir) sr=ir;
                    if (sr==ir) nykyrivi=0;
                    PR_ENRM;
                    sprintf(sbuf,"%.*s",c3+8,space);
                    for (k=i+1; k<r3disp+2; ++k)
                        {
                        LOCATE(k,1);
                        sur_prin2(sbuf,k,1);
                        }
                    end_of_list=1;
                    return(nykyrivi);
                    }
                if (k<0) return(-1);
                disp_label(&list,il,i);
                PR_ENRM;
                if (sr==ir) nykyrivi=0;
                riv=list.line1[il];
                ++i; ++ir; if (i==r3disp+2) { viimrivi=riv-1; break; }
                }
            nayta_rivi(riv,i);
            if (ir==sr) { nykyrivi=riv; ril=il; tedread(rrivi,riv); }
            viimrivi=riv;
            ++riv;
            CURSOR_ON;
            }
        return(nykyrivi);
        }

static void nayta_rivi(int riv, int i)
        {
        int k,h;
        char x[LLENGTH],xs[LLENGTH];
        char *px,*pxs;
        char v;

        tedread(x,riv);
        sprintf(sbuf,"%6d %c",riv,*x);
        write_string(sbuf,8,'2',i,1);
        if (varjot==1)
            {
            if (tzs[riv]!=0)
                {
                tedread(xs,tzs[riv]); strcat(xs+2,"  ");
                k=9; px=x+sar1; pxs=xs+2+sar1;
                while (k<lev+9)
                    {
                    v=*pxs; h=1;
                    while (pxs[h]==v && k+h<lev) ++h;
                    write_string(px,h,v,i,k);
                    k+=h; px+=h; pxs+=h;
                    }
                }
            else
                write_string(x+sar1,lev,' ',i,9);
            if (lev<c3)
                {
                sprintf(sbuf,"%c%.*s",REUNA,c3-lev-1,space);
                write_string(sbuf,c3-lev,' ',i,lev+9);
                }
            }
        else  /* varjot=0 */
            {
            LOCATE(i,1);
            k=sprintf(sbuf,"%6d %c%.*s",riv,*x,lev,x+sar1);
            if (lev<c3) sprintf(sbuf+k,"%c%.*s",REUNA,c3-lev-1,space);
            sur_prin2(sbuf,i,1);
            }
        }

static int seur_kentta()
        {
        int i;

        if (il==list.n-1) return(-2);
        list_fclose(text);
        ++il;
        i=avaa(&list,il,"rb"); if (i<0) return(-1);
        lev=c3; if (ted1-sar1<c3) lev=ted1-sar1;
        return(1);
        }


/* SHOW4.C (LIST SHOW) 25.7.1991/SM (18.2.1992) (9.11.1996)
*/

static void vaihehaku(int jatkohaku) /* 0=uusi haku 1=jatkoa */
        {
        int i,m;
        char ch[2];

        if (!jatkohaku || hpos==0)
            {
            *haku=EOS; *hakuvarjo=EOS;
            hpos=18; vain_varjo=0;
            }
        if (varjohaku)
            {
            vsr=1; vsc=1;
            if (jatkohaku) { hril=il; hriv=ensrivi+sr; }
            }
        while (1)
            {
            if (!varjohaku)
                {
                putsaa();
                LOCATE(r3disp+2,1);
                PR_EINV;
                sur_print("Search for word: ");
                PR_EUDL;
                sprintf(sbuf,"%s",haku); sur_print(sbuf);
                PR_EBLK;
                sur_print("_");
                }
            else /* varjohaku */
                {
                putsaa();
                hlen=strlen(haku); vlen=strlen(hakuvarjo);

                write_string("Search for word: ",16,'7',r3disp+2,1);
                for (i=0; i<hlen; ++i)
                    {
                    if (i<vlen)
                        write_string(haku+i,1,hakuvarjo[i],r3disp+2,hpos+i);
                    else
                        {
                        write_string(haku+i,hlen-vlen,' ',r3disp+2,hpos+i);
                        break;
                        }
                    }

                write_string(space,c3+8,'7',r3disp+3,1);
                write_string("         Shadow: ",16,'7',r3disp+3,1);
                write_string(hakuvarjo,strlen(hakuvarjo),' ',r3disp+3,hpos);

                if (vsr==1 && vsc-1<hlen && haku[vsc-1]!=' ')
                    write_string(haku+vsc-1,1,'5',r3disp+2,hpos-1+vsc);
                else if (vsr==2 && vsc-1<vlen && hakuvarjo[vsc-1]!=' ')
                    write_string(hakuvarjo+vsc-1,1,'5',r3disp+3,hpos-1+vsc);
                else
                    write_string("_",1,'5',r3disp+1+vsr,hpos-1+vsc);
                }
            if (*haku==EOS)
                {
                strcpy(sbuf,"F1=HELP    Cancel by ENTER");
                write_string(sbuf,strlen(sbuf),'7',r3disp+2,30);
                }
            else
                {
                strcpy(sbuf," Cancel by ENTER");
                write_string(sbuf,strlen(sbuf),'7',r3disp+2,40);
                }

   if (varjohaku) nayta();
            paikka(sr,sc);
            SAVE_CURSOR;
//          m=getch("");
            m=nextch("");
            RESTORE_CURSOR;
            if (m==CODE_RETURN || m==CODE_EXIT)
                {                 /* 18.2.92 */
                putsaa();
                return;
                }
            if (m==CODE_EXEC)
                {
                vvensil=ensil; vvensrivi=ensrivi; vvsr=sr; vvsc=sc;
                if (!rivin_alusta) ++sc;
                else seuraava_rivi=1;
                i=etsi_sanaa1(haku,hakuvarjo);
                if (i<=0)
                    {
                    ensil=vvensil; ensrivi=vvensrivi;
                    sr=vvsr; sc=vvsc;
                    return;
                    }
                ++n_haku;
                continue;
                }
            if (m==CODE_LEFT && !varjohaku)
                {
                i=strlen(haku);
                if (i>0) haku[i-1]=EOS;
                continue;
                }
            if (m==CODE_HELP)
                {
                optiot();
                nayta();
                continue;
                }
            if (m==CODE_SRCH) continue;
            if (m==CODE_PRE)
                {
                m=nextch("");
                if (m==CODE_EXEC)
                    {
                    autom_haku=1;
                    ++n_haku; /* 10.6.1996 */
                    hriv=nayta(); hril=ril;
                    write_string(space,40,'7',r3disp+2,40);
                    while (1)
                        {
                        if (varjohaku /* && *haku!=EOS */ )
                            {
                            vvensil=ensil; vvensrivi=ensrivi; vvsr=sr; vvsc=sc;
                            }
                        if (!rivin_alusta) ++sc;
                        else seuraava_rivi=1;
                        i=etsi_sanaa1(haku,hakuvarjo);
                        if (i<0) return;
                        if (i==0)
                            {
                            autom_haku=0;
                            if (!rivin_alusta) --sc;
                            if (varjohaku /* && *haku!=EOS */ )
                                {
                                ensil=vvensil; ensrivi=vvensrivi;
                                sr=vvsr; sc=vvsc;
                                il=ensil; avaa_text(il);
                                }
                            nayta();
                            LOCATE(r3disp+2,hpos); PR_EINV;
                            sprintf(sbuf,"%.62s",space); sur_print(sbuf);
                            LOCATE(r3disp+2,hpos); PR_EUDL;
                            sprintf(sbuf,"%s",haku); sur_print(sbuf);
                            PR_EIN2;
                            sprintf(sbuf,"   # of cases found %ld",n_haku);
                            sur_print(sbuf);
                            PR_EINV; sur_print("    Press ENTER!");
                            paikka(sr,sc);
                            m=nextch("");
                            break;
                            }
                        ++n_haku; PR_EIN2;
                        LOCATE(r3disp+2,51); sprintf(sbuf,"N=%ld",n_haku);
                        sur_print(sbuf);
                        }
                    autom_haku=0;
                    continue;
                    }
                if (m=='0')
                    {
                    n_haku=0L;
                    }
                continue;
                }

            if (varjohaku)
                {
                if (m==CODE_DOWN)
                    {
                    if (vsr==2) continue;
                    if (vlen+1<vsc) vsc=vlen+1;
                    vsr=2; continue;
                    }
                else if (m==CODE_UP)
                    {
                    if (vsr==1) continue;
                    if (hlen+1<vsc) vsc=hlen+1;
                    vsr=1; continue;
                    }
                else if (m==CODE_LEFT)
                    {
                    if (vsc==1) continue;
                    --vsc; continue;
                    }
                else if (m==CODE_RIGHT)
                    {
                    if (vsr==1 && vsc-1>=hlen) continue;
                    else if (vsr==2 && vsc-1>=vlen) continue;
                    ++vsc; continue;
                    }
                else
                    {
                    if (vsr==1)
                        {
                        haku[vsc-1]=(char)m;
                        if (vsc==hlen+1) { haku[vsc]=EOS; ++hlen; }
                        if (vsc-1<vlen)
                            write_string(haku+vsc-1,1,hakuvarjo[vsc-1],r3disp+2,hpos+vsc-1);
                        else
                            write_string(haku+vsc-1,1,' ',r3disp+2,hpos+vsc-1);
                        }
                    else
                        {
                        hakuvarjo[vsc-1]=(char)m;
                        if (vsc==vlen+1) { hakuvarjo[vsc]=EOS; ++vlen; }
                        write_string(hakuvarjo,vlen,' ',r3disp+3,hpos);
                        if (vsc-1<hlen)
                            write_string(haku+vsc-1,1,hakuvarjo[vsc-1],r3disp+2,hpos+vsc-1);
                        }
                    ++vsc;
                    i=etsi_sanaa1(haku,hakuvarjo);
                    if (i<=0) return;
/* nayta(); */
                    continue;
                    }
                }
            else /* !varjohaku */
                {
                *ch=(char)m; ch[1]=EOS;
                strcat(haku,ch); PR_EUDL;
                LOCATE(r3disp+2,18); sprintf(sbuf,"%s",haku); sur_print(sbuf);
                PR_EINV;
                }
            i=etsi_sanaa(haku,hakuvarjo);
            if (i<=0) return;
            }
        }

static int etsi_sanaa1(char *haku, char *hakuvarjo)
        {
        int i;

        if (*hakuvarjo==EOS)
            {
            i=etsi_sanaa(haku,hakuvarjo);
            return(i);
            }
        i=etsi_varjosanaa(haku,hakuvarjo);
        return(i);
        }

static int etsi_sanaa(char *haku, char *hakuvarjo)
        {
        int i,riv,sar,m;
        char x[2*LLENGTH];
        char x2[LLENGTH];
        char x3[2*LLENGTH];
        char *p,*q;
        int len;
        int ariv;
        char tyhja[1];
        int sp_sanassa;

        *tyhja=EOS;
        len=strlen(haku);
        sp_sanassa=0;
        p=strchr(haku,' ');
        if (p!=NULL && p-haku>0 && *(p+1)!=' ' && *(p+1)!=EOS)
            {
            sp_sanassa=1;
            }
        if (!autom_haku) riv=nayta();
        else { riv=hriv; ril=hril; }
        ariv=riv;
        if (ril!=il)
            {
            il=ril;
            avaa_text(il);
            }
        tedreads(x,riv);
        sar=sar1+sc-1;
        p=x+sar;
        if (rivin_alusta)
            {
            p=x+alkupos;
            if (seuraava_rivi) { p=tyhja; seuraava_rivi=0; }
            }
        if (isot_kirjaimet)
             { if (muste_strnicmp(p,haku,len)==0) return(1); }
        else
             { if (strncmp(p,haku,len)==0) return(1); }

        while (1)
            {
            if (!rivin_alusta)
                {
                if (isot_kirjaimet)
                    q=strstr(muste_strupr(p),muste_strupr(haku));
                else
                    q=strstr(p,haku);

                if (q!=NULL) { p=q; break; }

                if (sp_sanassa && !vain_varjo)
                    {
                    q=p+strlen(p)-1;
                    while (*q && *q==' ') --q;
                    i=q-p+1;
                    *x3=EOS; strncat(x3,p,i); x3[i]=' '; x3[i+1]=EOS;
                    tedread(x2,riv+1);
                    q=x2+1; while (*q==' ') ++q;
                    strcat(x3,q);
                    if (isot_kirjaimet)
                        q=strstr(muste_strupr(x3),muste_strupr(haku));
                    else
                        q=strstr(x3,haku);
                    if (q!=NULL && (q-x3)<=i) { p+=q-x3; break; }
                    }

                if (lopputavut && !vain_varjo)
                    {
                    q=strstr(p,"- ");
                    if (q!=NULL)
                        {
                        *q=EOS;
                        i=q-p;
                        tedread(x2,riv+1);
                        q=x2+1; while (*q==' ') ++q;
                        strcat(p,q);
                        if (isot_kirjaimet)
                            q=strstr(muste_strupr(p),muste_strupr(haku));
                        else
                            q=strstr(p,haku);
                        if (q!=NULL && (q-p)<=i) { p=q; break; }
                        }
                    }
                }
            else /* rivin_alusta */
                {
                if (isot_kirjaimet) i=muste_strnicmp(p,haku,len);
                else i=strncmp(p,haku,len);
                if (i==0) break;

                }
/***************************************************
            if (sur_kbhit())
                {
                m=sur_getch();
                if (m=='.') return(0);
                else ungetch(m);
                }
****************************************************/
            if (riv==list.line2[il])
                {
                if (il==list.n-1)
                    {
                    if (autom_haku) return(0);
                    LOCATE(r3disp+2,18+1+strlen(haku));
                    PR_EBLK; sur_print("  Not found!");
                    sprintf(sbuf," Press ENTER!%.*s",(int)(c3+8-18-1-strlen(haku)-12-13),space);
                    PR_EINV; sur_print(sbuf);
                    paikka(sr,sc);
                    m=nextch("");
                    if (m==CODE_HELP)
                        {
                        optiot();
                        nayta();
                        return(1);
                        }
                    return(0);
                    }
                ++il;

                PR_EIN2;
                LOCATE(r3disp+2,60);
                sprintf(sbuf,"%.20s",space); sur_print(sbuf);
                LOCATE(r3disp+2,60);
                sprintf(sbuf,"%s in %s",list.chapter[il],list.editfile[il]);
                sur_print(sbuf);

                i=avaa_text(il); if (i<0) return(-1);
                riv=list.line1[il]-1;
                }
            ++riv;
            tedreads(x,riv);
            if (!rivin_alusta)
                {
                p=x+1;
                if (*haku==' ')
                    {
                    tedreads(x2,riv-1);
                    i=strlen(x2); while (i>0 && x2[i-1]==' ') --i;
                    if (x2[i-1]!='-')
                        {
                        *x=' ';
                        p=x;
                        }
                    }
                }
            else p=x+alkupos;
            }

        ensil=il; ensrivi=riv;
        i=p-x;
        if (i-sar1+len>c3) sar1=i+len-c3;
        else if (i<sar1) sar1=i+len-c3;
        if (sar1<1) sar1=1;
        sc=i-sar1+1;
        sr=1; /* 10.8.91 */
        if (!autom_haku && !varjohaku)
            nayta();
        hril=il; hriv=riv;
        return(1);
        }

static int avaa_text(int ik)
        {
        int i;

        list_fclose(text);
        i=avaa(&list,ik,"rb");
        return(i);
        }

static int etsi_varjosanaa(char *haku, char *hakuvarjo)
        {
        int i;
        int riv;
        char x[2*LLENGTH];
        char sx[2*LLENGTH];
        char *p;
        int vlen;
        int autom_haku2;

        if (*haku==EOS)
            {
            vain_varjo=1;
            i=etsi_sanaa(hakuvarjo,hakuvarjo);
            return(i);
            }

        vlen=strlen(hakuvarjo);
        while(1)
            {
            autom_haku2=autom_haku; autom_haku=1;
            i=etsi_sanaa(haku,hakuvarjo);
            autom_haku=autom_haku2;
            if (i<=0) return(i);
            if (ensil!=il)  { avaa_text(ensil); il=ensil; }
            riv=ensrivi;
            tedread(x,riv);
            p=sx+2+sar1+sc-1;
            if (tzs[riv]==0) { *sx=EOS; strncat(sx,space,ted1); }
            else tedread(sx,tzs[riv]);
            if (strncmp(p,hakuvarjo,vlen)==0) { if (!autom_haku) nayta(); break; }
            if (!rivin_alusta) ++sc;
            else seuraava_rivi=1;
            }
        vvensil=ensil; vvensrivi=ensrivi; vvsr=sr; vvsc=sc;
        return(1);
        }

static void tedreads(char *x, int riv)
        {
        if (vain_varjo) stedread(x,riv);
        else tedread(x,riv);
        }

static void stedread(char *x, int riv)
        {
        char x2[LLENGTH];

        if (tzs[riv]==0)
            {
            *x=EOS; strncat(x,space,ed1-2);
            }
        else
            {
            tedread(x2,tzs[riv]);
            strcpy(x,x2+2);
            }
        }

/* SHOW5.C (LIST SHOW) 26.7.1991/SM (24.5.1992)
*/

static void listan_talletus(int k)
        {
        int i;
        char x[LLENGTH];


        i=nayta();
        list_fclose(text);
        strcpy(x,etmpd); strcat(x,"SURVO.LST");
        aputied=muste_fopen2(x,"wt");

        if (k==0) /* 24.5.1992 */
            {
            strcpy(tut_info,"BREAK@");
            }
        if (i==0) { ril=il; i=list.line1[il]; ensrivi=ril; ensil=ril; }
        if (ensil<ril) ensrivi=i;
        fprintf(aputied,"%s %s %d %d %d %d\n",
                   list_path,list.editfile[ril],ensrivi,i,sar1,sc);
        fprintf(aputied,"%s %d %d\n",lista,list.n,ril);
        for (i=0; i<list.n; ++i)
            {
            fprintf(aputied,"%s %s\n",list.chapter[i],list.editfile[i]);
            }
        muste_fclose2(aputied);
        }

static int hae_valmis_lista()
        {
        int i,riv;
        char x[LLENGTH];
        char file0[LLENGTH];
        char kpl[LLENGTH];
        char tied[LLENGTH];
        extern char *pl1,*pl2;

        strcpy(x,etmpd); strcat(x,"SURVO.LST");
        aputied=muste_fopen2(x,"rt");
        if (aputied==NULL) return(-1);
        fscanf(aputied,"%s %s %d %d %d %d\n",
                   list_path,file0,&ensrivi,&riv,&sar1,&sc);
        sr=riv-ensrivi+1;

        fscanf(aputied,"%s %d %d\n",lista2,&list.n,&il);
        i=varaa_tilat(&list); if (i<0) { muste_fclose2(aputied); return(-1); }
        pl1=list.listspace1; pl2=list.listspace2;
        for (i=0; i<list.n; ++i)
            {
            fscanf(aputied,"%s %s\n",kpl,tied);
            listaan(&list,i,kpl,tied);
            }
        muste_fclose2(aputied); // RS 22.1.2014
        return(1);
        }

/* SHOWH.C (LIST SHOW) 27.7.1991/SM (24.5.1992)
*/

static void help()
        {
        int riv0,sar0;
        char varjo;
        int m;
        int nrivi;

        nrivi=13;
        varjo='4';
        riv0=r3-nrivi+3; sar0=1;

        avaa_ikkuna(riv0,sar0,c3+8,nrivi,varjo);

        strcpy(sbuf,"LIST SHOW key codes:");
        write_string(sbuf, strlen(sbuf),varjo,riv0+1,sar0+1);
        strcpy(sbuf,"Move the cursor by arrow keys etc. as in the edit field.");
        write_string(sbuf, strlen(sbuf),varjo,riv0+2,sar0+2);
        strcpy(sbuf,"N finds the start of the next chapter in the list.");
        write_string(sbuf, strlen(sbuf),varjo,riv0+3,sar0+2);
        strcpy(sbuf,"P finds the start of the previous chapter in the list.");
        write_string(sbuf, strlen(sbuf),varjo,riv0+4,sar0+2);
        strcpy(sbuf,"alt-F5 (SEARCH) initiates a search through the list. ESC repeats the search.");
        write_string(sbuf, strlen(sbuf),varjo,riv0+5,sar0+2);
        strcpy(sbuf,"   (Information on various search options by 'S')");
        write_string(sbuf, strlen(sbuf),varjo,riv0+6,sar0+2);
        strcpy(sbuf," ");
        write_string(sbuf, strlen(sbuf),varjo,riv0+7,sar0+2);
        strcpy(sbuf,"When called by the /LIST sucro:");
        write_string(sbuf, strlen(sbuf),varjo,riv0+8,sar0+2);
        strcpy(sbuf,"    F8  interrupts and displays the current edit field.");
        write_string(sbuf, strlen(sbuf),varjo,riv0+9,sar0+2);
        strcpy(sbuf,"    '.' interrupts and restores the original setup.");
        write_string(sbuf, strlen(sbuf),varjo,riv0+10,sar0+2);
        strcpy(sbuf," Continue by ENTER ");
        write_string(sbuf, strlen(sbuf),varjo,riv0+nrivi-1,sar0+50);

        m=nextch("");
        if (m=='S' || m=='s')
            {
            optiot();
            }
        nayta();
        }


static void optiot()
        {
        int riv0,sar0;
        int m;
        char varjo;
        int nrivi;


        nrivi=17;
        riv0=r3-nrivi+3; sar0=1;
        varjo='4';

        while (1)
            {
            avaa_ikkuna(riv0,sar0,c3+8,nrivi,varjo);

            strcpy(sbuf,"Searching for a word in the current list:");
            write_string(sbuf, strlen(sbuf),varjo,riv0+1,sar0+1);
            strcpy(sbuf,"After pressing the search (alt-F5) key, enter the word");
            write_string(sbuf, strlen(sbuf),varjo,riv0+2,sar0+2);
            strcpy(sbuf,"character by character. In each stage, the first occurrence");
            write_string(sbuf, strlen(sbuf),varjo,riv0+3,sar0+2);
            strcpy(sbuf,"of the given string will be displayed.");
            write_string(sbuf, strlen(sbuf),varjo,riv0+4,sar0+2);
            strcpy(sbuf,"Next case of the word is found by the ESC key. All the cases");
            write_string(sbuf, strlen(sbuf),varjo,riv0+5,sar0+2);
            strcpy(sbuf,"will be sought and the number of cases found displayed by F2 ESC.");
            write_string(sbuf, strlen(sbuf),varjo,riv0+6,sar0+2);

            strcpy(sbuf,"Current search options:");
            write_string(sbuf, strlen(sbuf),varjo,riv0+8,sar0+1);
            strcpy(sbuf,val1[isot_kirjaimet]);
            write_string(sbuf, strlen(sbuf),varjo,riv0+9,sar0+2);
            strcpy(sbuf,val2[lopputavut]);
            write_string(sbuf, strlen(sbuf),varjo,riv0+10,sar0+2);
            strcpy(sbuf,val3[varjohaku]);
            write_string(sbuf, strlen(sbuf),varjo,riv0+11,sar0+2);

            if (rivin_alusta)
                {
                sprintf(sbuf,"Only words starting from column %d are observed.",alkupos);
                if (alkupos==0) strcat(sbuf," (0 = control column)");
                write_string(sbuf, strlen(sbuf),varjo,riv0+12,sar0+2);
                sprintf(sbuf,"   (Change the start column by '+' and '-'. Cancel by 'B'.)");
                write_string(sbuf, strlen(sbuf),varjo,riv0+13,sar0+2);
                }
            else
                {
                strcpy(sbuf,"No restrictions for the start position. (Change by 'B')");
                write_string(sbuf, strlen(sbuf),varjo,riv0+12,sar0+2);
                }

            sprintf(sbuf,"# of cases = %ld (Clear by '0')",n_haku);
            write_string(sbuf, strlen(sbuf),varjo,riv0+nrivi-2,sar0+2);

            strcpy(sbuf," Continue by ENTER ");
            write_string(sbuf, strlen(sbuf),varjo,riv0+nrivi-1,sar0+50);

            if (*muutos)
                {
                write_string(muutos,strlen(muutos),'5',riv0+murivi,musar+2);
                }
            m=nextch("");
            switch (m)
                {
              case CODE_RETURN: break;
              case 'U':
              case 'u':
                isot_kirjaimet=1-isot_kirjaimet;
                murivi=9; musar=18;
                if (isot_kirjaimet)
                    strcpy(muutos,"ignored");
                else
                    strcpy(muutos,"observed");
                break;
              case 'H':
              case 'h':
                lopputavut=1-lopputavut;
                murivi=10; musar=18;
                if (lopputavut)
                    strcpy(muutos,"observed");
                else
                    strcpy(muutos,"ignored");
                break;
              case 'V':
              case 'v':
                varjohaku=1-varjohaku;
                murivi=11; musar=19;
                if (varjohaku)
                    {
                    strcpy(muutos,"observed");
                    vsr=vsc=1;
                    }
                else
                    strcpy(muutos,"ignored");
                vrivit();
                break;
              case '0':
                n_haku=0L;
                murivi=nrivi-2; musar=14;
                strcpy(muutos,"0");
                break;
              case 'B':
              case 'b':
                rivin_alusta=1-rivin_alusta;
                if (rivin_alusta)
                    {
                    murivi=12; musar=33;
                    sprintf(muutos,"%d",alkupos);
                    }
                else
                    {
                    *muutos=EOS;
                    }
                break;
              case '+':
                if (!rivin_alusta) break;
                ++alkupos;
                murivi=12; musar=33;
                sprintf(muutos,"%d",alkupos);
                break;
              case '-':
                if (!rivin_alusta) break;
                if (alkupos>0) --alkupos;
                murivi=12; musar=33;
                sprintf(muutos,"%d",alkupos);
                break;

                }
            if (m==CODE_RETURN) break;
            }
        *muutos=EOS;
        nayta();
        }

static void avaa_ikkuna(int riv, int sar, int lev, int kork,char varjo)
        {
        int i,j;
        char x[LLENGTH];

        *x=vyk; for (i=0; i<lev-2; ++i) x[i+1]=vaaka; x[lev-1]=oyk;
        write_string(x,lev,varjo,riv,sar);
        *x=pysty; for (i=0; i<lev-2; ++i) x[i+1]=' '; x[lev-1]=pysty;
        for (j=riv+1; j<riv+kork-1; ++j)
            write_string(x,lev,varjo,j,sar);
        *x=vak; for (i=0; i<lev-2; ++i) x[i+1]=vaaka; x[lev-1]=oak;
        write_string(x,lev,varjo,riv+kork-1,sar);
        }

/* REPLACE.C (LIST REPLACE) 25.8.1991/SM (6.9.1991) (15.4.1995)
*/

static void op_list_replace(int argc,char *argv[])
        {
        int i,len;
        char x[LLENGTH];
        int j1,j2;

//        if (argc==1) return;
        s_init(argv[1]);

        if (g<5)
            {
            init_remarks();
            rem_pr("Usage: LIST REPLACE <name_of_list>,L1,L2");
            rem_pr("all the occurrences of the text given on the edit line L1");
            rem_pr("by that given on line L2.");
            wait_remarks(2);
            s_end(argv[1]);
            return;
            }
        i=spec_init(r1+r-1); if (i<0) return;
        strcpy(lista,word[2]);
        i=hae_lista(&list,lista);
        if (i<0) { ei_listaa(lista); return; }
        j1=edline2(word[3],1,1); if (j1==0) return;
        j2=edline2(word[4],1,1); if (j2==0) return;
        edread(x,j1); strcpy(t1,x+1);
        *st1=EOS; if (zs[j1]!=0) { edread(x,zs[j1]); strcpy(st1,x+1); }
        if (g>5) len=atoi(word[5]);
        else len=spt_pois(t1);
        t1[len]=EOS; st1[len]=EOS;
        if (*st1==EOS) strncat(st1,space,len);
        edread(x,j2); strcpy(t2,x+1);
        *st2=EOS; if (zs[j2]!=0) { edread(x,zs[j2]); strcpy(st2,x+1); }
        if (g>6) len=atoi(word[6]);
        else len=spt_pois(t2);
        t2[len]=EOS; st2[len]=EOS;
        if (*st2==EOS) strncat(st2,space,len);

        korvaa(t1,st1,t2,st2);
        }

static int spt_pois(char *s)
        {
        int i;

        i=strlen(s);
        while (i>0 && s[i-1]==' ') --i;
        return(i);
        }

static int hae_lista(SURVO_LIST *l, char *lista)
        {
        int i,j;
        char x[LLENGTH],*osa[2];
        char s[LLENGTH];


        if (strcmp(lista,"*")==0)
            {
            i=hae_valmis_lista();
            return(i);
            }
        strcpy(s,lista); strcat(s,":");
        for (j=1; j<=r2; ++j)
            {
            edread(x,j);
            i=split(x+1,osa,2);
            if (i<2) continue;
            if (strcmp(osa[0],"LIST")!=0) continue;
            if (strcmp(osa[1],s)==0) break;
            }
        if (j>r2) return(-1);
        strcpy(list_path,edisk);  /* 24.5.1992 */
        i=spfind("PATH");
        if (i>=0) strcpy(list_path,spb[i]);
        l->n=lue_lista(j,l,0); if (l->n<0) return(-1);
        if (l->n==0)
            {
            sprintf(sbuf,"\nLIST %s is empty!",lista);
            sur_print(sbuf); WAIT; return(-1);
            }
/* printf("\nn=%d",l->n); getch(); */
        i=varaa_tilat(l); if (i<0) return(-1);
        lue_lista(j,l,1);
/*
for (i=0; i<l->n; ++i)
    printf("\n%d %s %s",i,l->chapter[i],l->editfile[i]);
    getch();
*/
        return(j);
        }


static int lue_lista(int j,SURVO_LIST *l,int kk)
/* kk=0: lasketaan vain lukumaara */
        {
        int n,k,i,h;
        char x[LLENGTH];
        char *p,*q,*s;
        char *osa[EP4];
        char sana[LLENGTH];
        int ind1,ind2;

        if (kk!=0) { pl1=l->listspace1; pl2=l->listspace2; }
        n=0;
        edread(x,j);
        p=strchr(x+1,':');
        if (p==NULL)
            { sur_print(": missing in LIST definition!"); WAIT; return(-1); }
        ++p;
        while (1)
            {
            k=split(p,osa,EP4); h=0;
            while (h<k)  /* k-1 -15.4.1995 */
                {
                if (strcmp(osa[h],"END")==0) return(n);
                strcpy(sana,osa[h+1]);
                q=strchr(sana,'-');
                if (q!=NULL)
                    {
                    ind2=atoi(q+1);
                    *q=EOS; --q;
                    while (*q>='0' && *q<='9') --q;
                    ind1=atoi(q+1);
                    if (ind2<ind1)
                        {
                        sprintf(sbuf,"\nError in LIST notation %s",osa[h+1]);
                        sur_print(sbuf); WAIT; return(-1);
                        }
                    }
                else ind1=-1;

                if (kk==0)
                    {
                    if (ind1==-1) ++n; else n+=ind2-ind1+1;
                    }
                else
                    {
                    if (ind1==-1)
                        {
                        listaan(l,n,osa[h],osa[h+1]);
                        ++n;
                        }
                    else
                        {
                        *(q+1)=EOS;
                        for (i=ind1; i<=ind2; ++i)
                            {
                            sprintf(sbuf,"%s%d",sana,i);
                            listaan(l,n,osa[h],sbuf);
                            ++n;
                            }
                        }
                    }
                h+=2;
                }
            ++j; if (j>=r2) return(n);
            edread(x,j);
            if (empty_line(x+1,c2)) return(n);
            p=x+1;
            }
        }


/* REP2.C (LIST REPLACE) 25.8.1991/SM (7.9.1991) (8.4.1996)
*/


static void korvaa(char *t1,char *st1,char *t2,char *st2)
        {
        int i,riv;
        char x[2*LLENGTH],x2[2*LLENGTH];
        char *p;
        int len1,len2,pos;
        int muutos;
        char kontr_merkki;
        char *px2;
/*
printf("\n t1=%s*\nst1=%s*\n t2=%s*\nst2=%s*",t1,st1,t2,st2); getch();
*/
        len1=strlen(t1);
        len2=strlen(t2);
sprintf(sbuf,"\nReplacing string `%s' by `%s' ...",t1,t2); sur_print(sbuf);
        for (il=0; il<list.n; ++il)
            {
            i=avaa(&list,il,"r+b"); if (i<0) return;
       sprintf(sbuf,"\nChapter %s in %s",list.chapter[il],list.editfile[il]); sur_print(sbuf);
            for (riv=list.line1[il]; riv<=list.line2[il]; ++riv)
                {                
                tedread(x,riv);
                if (tzs[riv]==0)
                    { *x2=EOS; strncat(x2,space,ted1+2); }
                else
                    {
                    tedread(x2,tzs[riv]);
/*     sprintf(sbuf,"\n%.70s",x); sur_print(sbuf);
       sprintf(sbuf,"\n%.70s",x2+2); sur_print(sbuf); getch();
*/
                    px2=x2+2; x2[ted1]=' '; x2[ted1+1]=' '; x2[ted1+2]=EOS;
                    }
                px2=x2+2;

                kontr_merkki=*x; *x=' '; p=x; muutos=0;
                while (1)
                    {
                    p=strstr(p,t1);
                    if (p==NULL) break;
                    if (*st1)
                        {

//         Rprintf("\nst: len1=%d",len1);
//         Rprintf("\n%.*s",len1,x2+(p-x)+2);
//         Rprintf("\n%.*s",len1,st1); 
                                                        /* -8.4.96 break; */
                        if (strncmp(x2+(p-x)+2,st1,len1)!=0) { ++p; continue; }
                        }
/*     sprintf(sbuf,"\n%.70s l=%d",x+1,strlen(x)); sur_print(sbuf);
       sprintf(sbuf,"\n%.70s l=%d",px2,strlen(px2)); sur_print(sbuf); getch();
*/
                    pos=p-x;
                    if (len2<len1)
                        {
                        for (i=pos+len2; i<ted1-len1+len2; ++i)
                            {
                            x[i]=x[i+len1-len2];
                            px2[i]=px2[i+len1-len2];
                            }
                        strncpy(x+ted1-len1+len2,space,len1-len2);
                        strncpy(px2+ted1-len1+len2,space,len1-len2);
                        }
                    else if (len2>len1)
                        {
                        for (i=ted1-1; i>=pos+len2; --i)
                            {
                            x[i]=x[i-len2+len1];
                            px2[i]=px2[i-len2+len1];
                            }
                        }
                    strncpy(x+pos,t2,len2);
                    strncpy(px2+pos,st2,len2);
/*     sprintf(sbuf,"\n%.70s l=%d",x+1,strlen(x)); sur_print(sbuf);
       sprintf(sbuf,"\n%.70s l=%d",px2,strlen(px2)); sur_print(sbuf); getch();
*/
                    p+=len2;
                    ++muutos;
                    }
                if (muutos)
                    {
                    *x=kontr_merkki;
                    i=talletus_replace(riv,x,x2);
                    if (i<0) return;
                    }
                }
            list_fclose(text);
            }
        }

static int talletus_replace(int riv,char *x,char *x2)
        {
        int i;
        char y[LLENGTH];

        i=tedwrite(x,riv);
        if (i<0) return(-1);

        if (tzs[riv])
            {
            i=tedwrite(x2,tzs[riv]);
            if (i<0) return(1);
            return(1);
            }
        if (empty_line(x2+3,strlen(x2+3))) return(1);

   sur_print("\nNo saving of new shadows!"); WAIT; return(-1);


        return(1);
        }

static int tedwrite(char *s,int j)
        {
        int i;

        muste_fseek(text,(long)ted1*(long)j,0);
        for (i=0; i<ted1; ++i) { putc((int)(*s),text); ++s; }
        if (ferror(text)) return(-1);
        if (*copypath!=EOS) orgsave=1; // RS 21.1.2014
        return(1);
        }


/* MAKE.C (LIST MAKE) 17.3.1996/SM (20.3.1996) (20.4.1996) (18.11.1996)
LIST MAKE <list> OF <text_file>
0    1     2     3   4
*/

static void op_list_make(int argc,char *argv[])
        {
        int i,k;
        char x[LLENGTH],*osa[3];

//        if (argc==1) return;
        s_init(argv[1]);

        if (g<5)
            {
            init_remarks();
            rem_pr("Usage: LIST MAKE <list> OF <text_file>");
            wait_remarks(2);
            s_end(argv[1]);
            return;
            }
        i=spec_init(r1+r-1); if (i<0) return;

        strcpy(x,word[2]);
        i=strlen(x)-1;
        while (i>0 && x[i]!='\\' && x[i]!='/' && x[i]!=':') --i;
        if (i==0) { strcpy(newlist_path,edisk); strcpy(newlist,x); }
        else { ++i; strncpy(newlist_path,x,i); newlist_path[i]=EOS;
               strcpy(newlist,x+i);
             }
/* printf("\nnewlist=%s path=%s",newlist,newlist_path); getch(); */


        lr2=300; lwidth=100; shad=100;
        i=spfind("LISTDIM");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            i=split(x,osa,3);
            if (i>0) lr2=atoi(osa[0]);
            if (i>1) lwidth=atoi(osa[1]);
            if (i>0) shad=atoi(osa[2]);
            if (lr2<50 || lwidth<30 || shad<0 || shad>lr2 ||
                ((long)lr2+(long)shad)*(long)(lwidth+1)>65500L )
                {
                sur_print("\nIncorrect LISTDIM specification!");
                WAIT; return;
                }
            }
        edtnro=1;

        strcpy(x,word[4]);
        if (*x=='#')
            {
            text_name_line=find_line_start(x,1);
            ++text_name_line;
            edread(x,text_name_line); split(x+1,osa,1);
            i=txt_open(osa[0]);
            }
        else
            {
            i=txt_open(x);
            text_name_line=0;
            }
        if (i<0) return;


        strcpy(x,etmpd); strcat(x,"SURVO.TMP");
        tempf=muste_fopen(x,"w+b");
        if (tempf==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s!",x);
            sur_print(sbuf); WAIT; return;
            }
        n_tlines=0L;

        edtnro=create_edit_file_make(edtnro);
        if (edtnro<0) { muste_fclose(tempf); return; }
        n_edit_lines=4;
        while (1)
            {
            n_lines=read_case_make(lr2-4);
/* printf("\nn_lines=%d %d %d",n_lines,n_edit_lines,lr2); getch(); */
            if (n_edit_lines+n_lines>lr2)
                {
                i=close_edit_file_make(n_edit_lines); if (i<0) { muste_fclose(tempf); return; }
                edtnro=create_edit_file_make(edtnro);
                if (edtnro<0) { muste_fclose(tempf); return; }
                n_edit_lines=4;
                }
            n_edit_lines+=n_lines;
            rewind(tempf);
            for (k=0; k<n_lines; ++k)
                {
                *x='*';
                for (i=0; i<lwidth; ++i) x[i+1]=(char)getc(tempf); x[lwidth+1]=EOS;
                getc(tempf);
                i=xsave2(x,edt); if (i<0) { muste_fclose(tempf); return; }
                }
            if (feof(tfile))
                {
                if (text_name_line)
                    {
                    muste_fclose(tfile);
                    ++text_name_line;
                    edread(x,text_name_line); i=split(x+1,osa,1);
                    if (i>0 && muste_strcmpi(osa[0],"END")!=0)
                        {
                        i=txt_open(osa[0]); if (i<0) return;
                        continue;
                        }
                    }

                i=close_edit_file_make(n_edit_lines); if (i<0) { muste_fclose(tempf); return; }
                muste_fclose(tempf);
                strcpy(x,newlist_path); strcat(x,newlist);
                if (strchr(newlist,'.')==NULL) strcat(x,".LST");
                tempf=muste_fopen2(x,"wt");
                if (tempf==NULL)
                    {
                    sprintf(sbuf,"\nCannot open file %s!",x);
                    sur_print(sbuf); WAIT; return;
                    }
                if (edtnro>2)
                    fprintf(tempf,"A,%s1-%d\n",newlist,edtnro-1);
                else
                    fprintf(tempf,"A,%s1\n",newlist);  /* 18.11.1996 */

                fprintf(tempf,"END\n");
                muste_fclose2(tempf);
                return;
                }
            }

        }

static int read_case_make(int max)
        {
        unsigned int i,ip;
        int n,m,len;
        char x[LLENGTH];
        char *p,*q;

        rewind(tempf);
        n=0;
        while (n<max)
            {
            i=0;
            while (!feof(tfile) && i<MAX_LLENGTH-1)
                {
                m=getc(tfile);
                long_line[i++]=(char)m;
                if (m=='\n') break;
                }
            --i; ++n_tlines;
            if (i>=MAX_LLENGTH-1)
                {
                sprintf(sbuf,"\nLine %ld longer than %u characters!",n_tlines,MAX_LLENGTH-1);
                sur_print(sbuf);
                sprintf(sbuf,"\n%s is obviously not a text file.",word[4]);
                sur_print(sbuf); WAIT; return(-2);
                }
            ip=0; long_line[i]=EOS;
            len=i; if (i>LLENGTH-1) len=LLENGTH-1;
            if (*long_line==EOS || strncmp(long_line,space,len)==0)
                {
                ++n; save_tempf("");
                fflush(tempf);
                return(n);
                }
            while (1)
                {
                if (i-ip+1>lwidth)
                    {
                    p=long_line+ip;
                    q=p+lwidth-1;
                    while (q>p+1 && *q!=' ') --q;
                    if (q==p+1) q=p+lwidth-1;
                    strncpy(x,p,q-p+1); x[q-p]=EOS;
                    save_tempf(x);
                    ++n;
                    ip+=q-p+1;
                    }
                else
                    {
                    save_tempf(long_line+ip);
                    ++n;
                    break;
                    }
                }
            if (feof(tfile)) break;
            }
        fflush(tempf);
        return(n);
        }

static int save_tempf(char *x)
        {
        xsave2(x,tempf);
        return(1);
        }

static int txt_open(char *x)
        {
        if (!muste_is_path(x)) { strcpy(x,edisk); strcat(x,word[4]); }
        tfile=muste_fopen(x,"rt");
        if (tfile==NULL)
            {
            sprintf(sbuf,"\nText file %s not found!",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(1);
        }

static int find_line_start(char *s,int lin)
        {
        int i,k;
        char x[LLENGTH],*osa[1];
        int len;

        len=strlen(s);
        for (i=lin; i<=r2; ++i)
            {
            edread(x,i); k=split(x+1,osa,1);
            if (strncmp(s,osa[0],len)==0) break;
            }
        if (i>r2) return(-1);
        return(i);
        }

/* make2.c 5.12.1990/SM (17.3.1996) (20.4.1996)
   sort3.c-tiedostosta muunnettu
   LIST MAKE
*/

static int create_edit_file_make(int edtnro)
        {
        int i,k;
        char *p,*q;
        char nimi0[LNAME];
        char nimi1[LNAME];
        char nimi2[LNAME];

        strcpy(nimi1,newlist); sprintf(sbuf,"%d",edtnro); strcat(nimi1,sbuf);
        strcpy(nimi2,newlist); sprintf(sbuf,"%d",edtnro+1); strcat(nimi2,sbuf);
        strcpy(nimi0,newlist_path); strcat(nimi0,nimi1); strcat(nimi0,".EDT");

sprintf(sbuf,"\nSaving in edit file %s ...",nimi0); sur_print(sbuf);
/* Mahdollinen tarkistus, ettei talleteta input-kenttien paalle, puuttuu!!! */
/* for (i=0; i<list.n; ++i) printf("\n%s",list.editfile[i]); getch(); */
        edt=list_fopen(nimi0,"wb");
        if (edt==NULL)
            {
            sprintf(sbuf,"\nCannot open edit file %s for a sorted list!",
                                     nimi0);
            sur_print(sbuf); WAIT; return(-1);
            }


        /* header kirjoitetaan vasta sulkiessa, kun varjot tiedossa" */
        muste_fseek(edt,(long)(lwidth+1),SEEK_SET);
        sprintf(sbuf,"*SAVE %s / LIST MAKE %s OF %s",nimi1,word[2],word[4]);
        i=xsave2(sbuf,edt); if (i<0) { list_fclose(edt); return(-1); }
        sprintf(sbuf,"*LOAD %s",nimi2);
        i=xsave2(sbuf,edt); if (i<0) { list_fclose(edt); return(-1); }
        i=xsave2("*",edt); if (i<0) { list_fclose(edt); return(-1); }
        i=xsave2("*DEF A,5,END",edt); if (i<0) { list_fclose(edt); return(-1); }

/*      strcpy(nimi3,etmpd); strcat(nimi3,"SURVO2.TMP");
        shadows=fopen(nimi3,"w+b");
        if (shadows==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s!",
                                     nimi3);
            sur_print(sbuf); WAIT; return(-1);
            }

        for (k=0; k<4; ++k)
            {
            i=xsave2(" ",shadows); if (i<0) return(-1);
            }
*/
        ++edtnro;
        return(edtnro);
        }

static int close_edit_file_make(int n_edit_lines)
        {
        int i,h;
        char header[LLENGTH];
        char number[16];
        char xs[LLENGTH];


        for (h=n_edit_lines; h<lr2; ++h)
            {
            i=xsave2("*",edt); if (i<0) { list_fclose(edt); return(-1); }
            }

        xsave2("END",edt);

        strcpy(header,"SURVO84ED ");
        strcat(header,muste_itoa(lwidth+1,number,10)); strcat(header," ");
        strcat(header,muste_itoa(lr2,number,10)); strcat(header," ");
        strcat(header,"                    "); header[20]=EOS;
        strcat(header,muste_itoa(lwidth+1,number,10)); strcat(header," S");
        strcat(header,muste_itoa(shad,number,10)); strcat(header," ");

        rewind(edt);
        i=xsave2(header,edt); if (i<0) { list_fclose(edt); return(-1); }


        list_fclose(edt); /* fclose(shadows); */
        return(1);
        }

static int xsave2(char *x,FILE *f)
        {
        int i,k;

        k=strlen(x); if (lwidth+1<k) k=lwidth+1;
        for (i=0; i<k; ++i) putc((int)x[i],f);
        for (i=k; i<lwidth+1; ++i) putc((int)' ',f);
        if (ferror(f)) { sur_print("\nCannot save results on disk! (end of space?)");
                         WAIT; return(-1);
                       }
        return(1);
        }


// extern char *struprf(char *);
// extern char *strnuprf(char *,int);

/* COUNT.C (LIST COUNT) 8.6.1996/SM (16.6.1996) (15.11.1996)
*/
static void op_list_count(int argc,char *argv[])
        {
        int i,j;
        char x[LLENGTH],*osa[2];

//        if (argc==1) return;
        s_init(argv[1]);

        if (g<4)
            {
            init_remarks();
            rem_pr("LIST COUNT,<Survo_list>,<phrases_name>");
            rem_pr("PHRASES <phrases>:");
            rem_pr("<phrase 1>");
            rem_pr("<phrase 2>");
            rem_pr("...");
            rem_pr("");
            rem_pr("counts the frequencies of given phrases (strings) in <Survo_list>");
            rem_pr("and writes the results as a column of frequencies after phrases.");
            rem_pr("");
            rem_pr("Hyphenated words (as Fin-");
            rem_pr("land) divided into two consecutive lines are connected (as Finland)");
            rem_pr("by default (HYPHENS=1). To ignore hyphenating, set HYPHENS=0 .");
            rem_pr("");
            rem_pr("By default LIST COUNT equates upper and lower case letters.");
            rem_pr("To make it work in a case-sensitive way, insert an extra");
            rem_pr("parameter `C' as follows:");
            rem_pr("LIST COUNT,<Survo_list>,<phrases_name>,C");
            rem_pr("");
            rem_pr("More information about LIST COUNT by LIST?");

            wait_remarks(2);
            s_end(argv[1]);
            return;
            }
        strcpy(lista,word[2]);
        i=spec_init(r1+r-1); if (i<0) return;

        isot=1;
        if (g>4 && ( *word[4]=='C' || *word[4]=='c') ) isot=0;

        jaetut_sanat=1;
        i=spfind("HYPHENS"); if (i>=0) jaetut_sanat=atoi(spb[i]);

        i=list_open(&list,lista,1);
        if (i<0) { ei_listaa(lista); return; }

        delimiter='|';
        i=spfind("DELIMITER");
        if (i>=0) delimiter=*spb[i];

        col1=1; col2=0;
        i=spfind("COLS");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            i=split(x,osa,2);
            if (i>0) { col1=atoi(osa[0]); if (col1<0) col1=0; }
            if (i==2)
                {
                col2=atoi(osa[1]);
                if (col1==0 && col2==0) col22=1;
                else
                    {
                    if (col2<col1) col2=0;
                    if (col2>LLENGTH-1) col2=0;
                    if (col2) col22=1;
                    }
                }
            }
        rivi1=etsi_phrases(word[3]); if (rivi1<0) return;

/*
printf("\nn=%d",list.n); getch();
   for (i=0; i<nphr; ++i)
     {
     printf("\n%d",phrlen[i]);
     printf("\n%.*s",phrlen[i],phr[i]);
 getch();
     }
*/

        count();       
/*
printf("\n%ld %ld %ld",nchapters,nrecords,nlines);
printf("\n%ld %ld %ld",nwords,nspaces,ncharacters); getch();
*/
        tulostus();       
        s_end(argv[1]);
        }

static int etsi_phrases(char *s)
        {
        int i,j,k,h,len,len1;
        char x[LLENGTH],*osa[2];
        char xs[LLENGTH];
        char *p,*q;

        i=init_erik();
        len=strlen(s);
        for (j=1; j<=r2-1; ++j)
            {
            edread(x,j);
            i=split(x+1,osa,2);
            if (i<2) continue;
            if (strcmp(osa[0],"PHRASES")!=0) continue;
            if (strncmp(osa[1],s,len)==0 && (osa[1][len]==':' || osa[1][len]==EOS) )
                break;
            }

        if (j>r2-1)
            {
            sprintf(sbuf,"\nDefinition PHRASES %s: missing!",s);
            sur_print(sbuf); WAIT; return(-1);
            }

        k=j+1; nphr=0; erikoishaut=0;  /* oli == */
        while (1)
            {
            edread(x,k);
            if (zs[k]!=0) edread(xs,zs[k]);
            p=strstr(x+1," // ");
            if (p==NULL) i=c2; else i=p-x;
            while (i>0 && x[i]==' ') --i;
            len=i;
            if (zs[k]!=0)
                {
                edread(xs,zs[k]);
                i=c2; while (i>0 && xs[i]==' ') --i;
                if (i>len) len=i;
                }
            if (len==0 || k==r2) break;
            ++k;
            }
        nphr=k-j-1;
/* printf("\nnphr=%d",nphr); getch(); */
        i=count_tilat();
        ph=phrtila;
        k=j+1; h=0;

        while (1)
            {
            edread(x,k);

            if (h<nphr)
                {
                erik[h]=0;
                i=tutki_erik(x,h);
                if (i==0) { ++k; ++h; continue; }
                if (i<0) return(-1);

                }

            if (zs[k]!=0) edread(xs,zs[k]);
            p=strchr(x+1,delimiter);
            if (p==NULL)
                {
                p=strstr(x+1," // ");
                if (p==NULL) i=c2; else i=p-x;
                while (i>0 && x[i]==' ') --i;
                }
            else
                {
                i=p-x-1;
                if (i==0) i=1; /* pelkka | -> laske N(|)  15.11.1996 */
                }
            len=len1=i;
            q=NULL;
            if (zs[k]!=0)
                {
                edread(xs,zs[k]);
                i=c2; while (i>0 && xs[i]==' ') --i;
                if (i>len) len=i;
                q=z+(zs[k]-1)*ed1+1;
                }
            if (len==0 || k==r2) break;
            if (len1==0) phr[h]=NULL;
            else /* omaan tilaan, jotta voidaan rekoodata esim. isoiksi kirjaimiksi */
                {
                if ((unsigned int)(ph-phrtila)+(unsigned int)len>=(unsigned int)ltila)
                    {
                    sur_print("\nToo many phrases!"); WAIT; return(-1);
                    }
                strncpy(ph,z+(k-1)*ed1+1,len); phr[h]=ph; ph+=len;              
                if (isot) strnuprf(phr[h],len);                   
                }
            sphr[h]=q;
            phrlen[h]=len;
            ++k; ++h;
            }

        return(j+1);
        }

static int count_tilat()
        {
        freq=(long *)muste_malloc(nphr*sizeof(long));
        if (freq==NULL) { ei_tilaa(); return(-1); }

        ltila=(long)nphr*(long)MAXL;
        if (ltila>(long)MAXTILA) ltila=MAXTILA;
        phrtila=muste_malloc((unsigned int)ltila);
        if (phrtila==NULL) { ei_tilaa(); return(-1); }

        phr=(char **)muste_malloc(nphr*sizeof(char *));
        if (phr==NULL) { ei_tilaa(); return(-1); }
        phrpos=(char **)muste_malloc(nphr*sizeof(char *));
        if (phrpos==NULL) { ei_tilaa(); return(-1); }
        sphr=(char **)muste_malloc(nphr*sizeof(char *));
        if (sphr==NULL) { ei_tilaa(); return(-1); }
        phrlen=(int *)muste_malloc(nphr*sizeof(int));
        if (phrlen==NULL) { ei_tilaa(); return(-1); }
        erik=(int *)muste_malloc(nphr*sizeof(int));
        if (erik==NULL) { ei_tilaa(); return(-1); }

        zp=muste_malloc((unsigned int)MAXTILA);
        if (zp==NULL) { ei_tilaa(); return(-1); }
        zsp=muste_malloc((unsigned int)MAXTILA);
        if (zsp==NULL) { ei_tilaa(); return(-1); }
        return(1);
        }

static int ei_tilaa()
        { sur_print("\nNot enough memory (LIST COUNT)!"); WAIT; return(1); }

static int count()
        {
        int i,chp,len;
        char x[LLENGTH],xs[LLENGTH];
        char *pz,*pzs;
        int skesken;  /* 1=sana kesken rivin lopussa */
        char *px,*pxs;
        int kpl;

        for (i=0; i<nphr; ++i) freq[i]=0L;
        nlines=nrecords=nchapters=0L;
        *x=EOS; *xs=EOS;
        list_rewind(&list,"rb");
        pz=zp; pzs=zsp;
        for (i=0; i<nphr; ++i) phrpos[i]=zp;
        skesken=0; kpl=0;
        while (1)
            {
            list_next_line_read(&list,x,xs,&chp);
// Rprintf("\n%d %d %d %.50s len=%d",list.i,list.j-1,list.line2[list.i],x,strlen(x));
            if (list.j-1<list.line2[list.i])
                {
                ++nlines;
                px=x+col1; pxs=xs+col1;
                if (col22) { x[col2+1]=EOS; xs[col2+1]=EOS; }
                if (jaetut_sanat && skesken)
                    {
                    while (*px && *px==' ') { ++px; ++pxs; }
                    if (*px==EOS) { px=x+1; pxs=xs+1; }
                                        /* tyhja - unohdetaan tavutus */
                    }
                i=strlen(px); while (i>0 && px[i-1]==' ') --i;
                len=i; skesken=0;
                if (jaetut_sanat && px[len-1]=='-')
                    {
                    --len; skesken=1;
                    }
                i=strlen(pxs); while (i>0 && pxs[i-1]==' ') --i;
                if (i>len) len=i;
                if (len==0)
                    {
                    if (kpl) { ++nrecords; kpl=0; }
                    }
                else ++kpl;
                strncpy(pz,px,len);
                if (isot) strnuprf(pz,len);
                pz+=len; if (!skesken) { *pz=' '; ++pz; }
                strncpy(pzs,pxs,len); pzs+=len; if (!skesken) { *pzs=' '; ++pzs; }
                continue;
                }
            ++nchapters; if (kpl) ++nrecords;
            sprintf(sbuf,"\nChapter %s in edit file %s",
                    list.chapter[list.i],list.editfile[list.i]);
            sur_print(sbuf);

            *pz=EOS; *pzs=EOS;
            if (list.i+1>=list.n) break;
            count2(); // RS 27.1.2014 move below if           
            pz=zp; pzs=zsp;
            for (i=0; i<nphr; ++i) phrpos[i]=zp;
            }
        if (erikoishaut) erik_freq();
        return(1);
        }

static int count2()
        {
        int i;
        char *p;


// int j=0; Rprintf("\ncount2 in");

        if (erikoishaut) return(count2e());
        p=zp;
        while (*p)
            {                        
            for (i=0; i<nphr; ++i)
                {
// j++; Rprintf("\n%d",j);                
                if (p<phrpos[i]) continue;
                if (phr[i]!=NULL)
                    {
/*
*sbuf=EOS;
strcpy(sbuf,p); sbuf[phrlen[i]]=EOS;
strcat(sbuf,"|");                        
strncat(sbuf,phr[i],phrlen[i]);                        
Rprintf(" |%s|",sbuf);                     
*/                    
                    if (*p!=*phr[i]) continue;                   
                    if (strncmp(p,phr[i],phrlen[i])==0)
                        {

// Rprintf(" --------------------------- MATCH!!!");
                      
                        
                        if (sphr[i]==NULL) 
                        	{
                        	++freq[i]; 
                        	phrpos[i]=p+phrlen[i]; 
                        	}
                        else if (strncmp(zsp+(p-zp),sphr[i],phrlen[i])==0)
                            {
                            ++freq[i];
                            phrpos[i]=p+phrlen[i];
                            }
                        }
                    }
                else  /* phr[i]=NULL */
                    {
                    if (strncmp(zsp+(p-zp),sphr[i],phrlen[i])==0)
                        {
                        ++freq[i];
                        phrpos[i]=p+phrlen[i];
                        }
                    }
                }
            ++p;
            }
        return(1);
        }

static int count2e()
        {
        int i;
        char *p;

        p=zp; erik_alku=1;
        while (*p)
            {
            erik_count(*p);
            for (i=0; i<nphr; ++i)
                {
                if (p<phrpos[i] || erik[i]) continue;
                if (phr[i]!=NULL)
                    {
                    if (*p!=*phr[i]) continue;
                    if (strncmp(p,phr[i],phrlen[i])==0)
                        {
                        if (sphr[i]==NULL) { ++freq[i]; phrpos[i]=p+phrlen[i]; }
                        else if (strncmp(zsp+(p-zp),sphr[i],phrlen[i])==0)
                            {
                            ++freq[i];
                            phrpos[i]=p+phrlen[i];
                            }
                        }
                    }
                else  /* phr[i]=NULL */
                    {
                    if (strncmp(zsp+(p-zp),sphr[i],phrlen[i])==0)
                        {
                        ++freq[i];
                        phrpos[i]=p+phrlen[i];
                        }
                    }
                }
            ++p;
            }
        erik_count(' '); --ncharacters; --nspaces;
        return(1);
        }

static int tulostus()
        {
        int i,j,pos;
        char *p;
        char x[LLENGTH];

        j=rivi1;
        for (i=0; i<nphr; ++i)
            {
            edread(x,j);
            p=strstr(x," // ");
            if (p==NULL)
                {
                edwrite(" // ",j,51);
                pos=55;
                }
            else pos=p-x+4;
            edwrite(space,j,pos);
            sprintf(sbuf,"%8ld",freq[i]);
            edwrite(sbuf,j,pos);
            ++j;
            }
        return(1);
        }

/* COUNT2.C (LIST COUNT) 15.6.1996/SM (16.6.1996) (15.11.1996)
*/


static int init_erik()
        {
        int i;

        for (i=0; i<N_ERIK; ++i) erik2[i]=0;
        nwords=nnumbers=nintegers=ncharacters=nletters=nspaces=ndigits=nspecials=0L;
        return(1);
        }

static int tutki_erik(char *x,int k)
        {
        int i;

        for (i=0; i<N_ERIK-1; ++i)
            {
            if (muste_strnicmp(x,erik_nimi[i],strlen(erik_nimi[i]))==0)
                {
                ++erikoishaut; erik[k]=i+1; erik2[i+1]=k+1; return(0);
                }
            }
        return(1);
        }


static int erik_count(char c)
        {
        int i,k;
        int digit;
        int letter;
        int dc;

        ++ncharacters;

/* nletters etc.  */

        i=0;
        if (c==' ')
            {
            ++nspaces;
            if (!erik_alku)
                {
                ++nwords;
                sana[i]=EOS;
/* printf("\nsana=%s %d %d",sana,kok,luku); getch(); */
                if (kok) ++nintegers;
                if (luku) ++nnumbers;
                erik_alku=1;
                }
            return(1);
            }
        dc=(int)(unsigned char)c; /* 15.11.1996 */
        digit=isdigit(dc); if (digit) ++ndigits;
        letter=isletterf(dc); if (letter)  ++nletters;

        if (erik_alku)
            {
            *sana=EOS; erik_alku=0; i=0;
            k=0; if (digit || c=='+' || c=='-') k=1;
            kok=luku=piste=0;
            if (k) { kok=luku=1; }
            else if (k || c=='.') { luku=1; piste=1; }
            }
        else if (luku && !digit)
            {
            kok=0;
            if (c!='.' || (c=='.' && piste) ) luku=0;
            }
/* printf("\n%c %d",c,kok); getch(); */
        sana[i++]=c;
        return(1);
        }

static int erik_freq()
        {
        int i;
        long l;

        nspecials=ncharacters-nspaces-ndigits-nletters;

        for (i=0; i<nphr; ++i)
            {
/* printf("\n%d",erik[i]); getch(); */
            if (erik[i]==0) continue;
            switch (erik[i])
                {
              case CHAPTERS: l=nchapters; break;
              case RECORDS:  l=nrecords; break;
              case LINES:    l=nlines; break;
              case WORDS:    l=nwords; break;
              case NUMBERS:  l=nnumbers; break;
              case INTEGERS: l=nintegers; break;
              case CHARACTERS: l=ncharacters; break;
              case LETTERS:  l=nletters; break;
              case SPACES:   l=nspaces; break;
              case DIGITS:   l=ndigits; break;
              case SPECIALS: l=nspecials; break;

              default: l=-1L;
                }
            freq[i]=l;
            }

        return(1);
        }

/* SORT.C (LIST SORT) 2.4.1995/SM (1.5.1995) (23.10.1995) (16.3.1996)

LIST SORT <list> BY <list of sort keys> TO <new data file>
0    1     2     3   4  5               2+nsk  3+nsk
                        nsk-2 kpl.
or
LIST SAVE <list> TO <text_file>[,<delimiter>]
0    1    2      3  4            5

*/

static void op_list_sortsave(int argc,char *argv[])
        {
        int i,k,h;
        char x[LLENGTH],*osa[3];
        char xs[LLENGTH];
        int kierros; /* lomitus */

//        if (argc==1) return;
        s_init(argv[1]);

        muste_strupr(word[1]);
        if (strcmp(word[1],"SORT")==0) komento=SORT;
        else if (strcmp(word[1],"SAVE")==0) komento=SAVE;
        else
            {
            sur_print("\nUnknown LIST operation!");
            WAIT; return;
            }

        switch(komento)
         {
        case SORT:

         if (g<7)
             {
             init_remarks();
             rem_pr("Usage: LIST SORT <list1> BY <list_of_sort_keys> TO <list2>");
             wait_remarks(2);
             s_end(argv[1]);
             return;
             }
         break;

        case SAVE:

         if (g<5)
             {
             init_remarks();
             rem_pr("Usage: LIST SAVE <structured_list> TO <text_file>");
             wait_remarks(2);
             s_end(argv[1]);
             return;
             }
         break;

         }

        i=spec_init(r1+r-1); if (i<0) return;
        strcpy(lista1,word[2]);

        i=list_open(&list,lista1,2);
        if (i<0) { list_not_found(lista1); return; }
/*
printf("\nLIST: m=%d",list.m);
for (i=0; i<list.m; ++i)
printf("\nname=%s type=%c len=%d sk_type=%d",
    list.varname[i],list.vartype[i],list.varlen[i],list.sk_type[i]);
getch();
*/

        i=list_conditions(&list);
        if (i<0) return;
        list_check=0;  /* 10.10.1995 */
        i=spfind("CHECK"); if (i>=0) list_check=atoi(spb[i]);
        *codefile=EOS;
        i=spfind("FILTER");
        if (i>=0) strcpy(codefile,spb[i]);
        i=spfind("NSORT");
        if (i>=0) samplesize=atol(spb[i]);
        workspace=MAXBLOCK; i=spfind("WORKSPACE");
        if (i>=0)
            {
            workspace=atoi(spb[i]);
            if (workspace<10) workspace=MAXBLOCK; /* po. 1000 */
            }
        l_wait=0.0; i=spfind("WAIT");
        if (i>=0) l_wait=atof(spb[i]);
        case_start=list.case_start; i=spfind("CASE_START");
        if (i>=0) case_start=list.case_start=*spb[i];
        case_end=list.case_end; i=spfind("CASE_END");
        if (i>=0) case_end=list.case_end=*spb[i];

        if (*codefile) { i=load_codes(codefile,code); if (i<0) return; }

        if (komento==SAVE) { list_save(); if (txtfile!=NULL) muste_fclose(txtfile); return; }
        i=avaimet(); if (i<0) return;
        i=list_rewind(&list,"r+b"); if (i<0) return;
        lwidth=list.c2; lr2=list.r2; lshad=list.shad;
        i=spfind("LISTDIM");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,osa,3);
            if (i>0) lr2=atoi(osa[0]);
            if (i>1) lwidth=atoi(osa[1]);
            if (i>2) lshad=atoi(osa[2]);

            if (lwidth>252 || lwidth<30 || lr2<23 || lshad>lr2
                   || (long)(lr2+lshad)*(long)(lwidth+1)>65535L || lshad<0)
                {
                sprintf(sbuf,"\nIncorrect or spurious LISTDIM=%d,%d,%d setting!",
                             lr2,lwidth,lshad);
                sur_print(sbuf);
                sur_print("\nCorrect form is the same as in the REDIM operation.");
                WAIT; return;
                }
            }
/*
printf("\ndim: %d %d %d",lwidth,lr2,lshad); getch();
*/
        i=varaa_tilat_sort(); if (i<0) return;
        i=tempf_open(); if (i<0) return;
        i=tutki_show(); if (i<0) return;
        i=tutki_txtfile(1);
                            /* 23.3.1996 FILE=<output_data_text>,<deliniter> */
        if (i<0) return;
        n_osat=0; nhav=0L; prev_chp=-1;
        list.nro=0L;

        for (i=0; i<r3; ++i) sur_print("\n");

        sur_print("\nLoading cases:");
        while (1)
            {
            for (k=0; k<koko; ++k)
                {
                nsort=0; if (list.i>=list.n) break;
                i=lue_avaimet(koko,&nsort);
                if (i<0) return;
                if (nsort==0) break;
                h=0; for (i=0; i<nsort; ++i) { ikey[i]=h; h+=slen; }
                lajittelu();
                i=osatalletus(nsort,n_osat);
                if (i<0) return;
                ++n_osat;
      /*        nhav+=nsort;
                sprintf(sbuf," %ld",nhav); sur_print(sbuf);
      */
                }
            if (nsort==0) break;
            }

        if (nhav==0L)
            {
            sur_print("\nNo accepted cases in the list!");
            WAIT; return;
            }

        if (n_osat>1)
            {
            kierros=lomitus();
            if (i<0) return;
            }
        else kierros=0;
        list_close(&list);

        strcpy(x,word[3+nsk]);

        i=strlen(x)-1;
        while (i>0 && x[i]!='\\' && x[i]!='/' && x[i]!=':') --i;
        if (i==0) { strcpy(newlist_path,edisk); strcpy(newlist,x); }
        else { ++i; strncpy(newlist_path,x,i); newlist_path[i]=EOS;
               strcpy(newlist,x+i);
             }
/* printf("newlist=%s path=%s\n",newlist,newlist_path); getch(); */
        i=talletus(kierros); if (i<0) return;
        if (txtfile!=NULL) muste_fclose(txtfile); // RS 26.1.2014
        return;
        }

static int tempf_open()
        {
        char x[LNAME];

        strcpy(x,etmpd); strcat(x,"SURVO.TMP");
        tempf=muste_fopen(x,"w+b");
        if (tempf==NULL)
            {
            sprintf(sbuf,"\nCannot open %s!",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(1);
        }

static int avaimet()
        {
        int i,h;
        char *p,*q,*q1;
        char x[LLENGTH];

        nsk=0; i=4;
        slen=0;
        p_textspace=key_textspace;
        while (i<g && muste_strcmpi(word[i],"TO")!=0)
            {
            strcpy(x,word[i]);
            p=x;
            neg[nsk]=0; if (*p=='-') { ++p; ++word[i]; neg[nsk]=1; }
/*          q=strchr(p,':');
            if (q==NULL)
                {
                sprintf(sbuf,"\n`:' missing in sort key `%s'!",x);
                WAIT; return(-1);
                }
            *q=EOS;
            ++q; sk_format[nsk]=*q;
            if (*q=='S')
                {
                sl[nsk]=atoi(q+1);
                }
            else sl[nsk]=*q-'0';
*/
            h=list_varfind(&list,p,0);
/* printf("\nsort_key=%d vartype=%c",h,list.vartype[h]); getch(); */
            if (h<0)
                {
                sprintf(sbuf,"\nSort key `%s' not defined as a field!",p);
                sur_print(sbuf); WAIT; return(-1);
                }

    /*
            h=tutki_key(spb[h],nsk); if (h<0) return(-1);
    */
            sl[nsk]=list.varlen[h];
            sp[nsk]=slen; slen+=list.varlen[h];
            sk_v[nsk]=h;
            sk_type[nsk]=list.sk_type[h];
            sk_format[nsk]=list.vartype[h];
            ++i; ++nsk;
            }

            sk_type[nsk]=LIST_CHAPTER; /* # of chapter */
            sl[nsk]=sizeof(int); sk_format[nsk]='2'; neg[nsk]=0;
            sp[nsk]=slen; slen+=sizeof(int); ++nsk;
            sk_type[nsk]=EDIT_LINE;
            sl[nsk]=sizeof(int); sk_format[nsk]='2'; neg[nsk]=0;
            sp[nsk]=slen; slen+=sizeof(int); ++nsk;

/*
for (i=0; i<nsk; ++i)
printf("\ni=%d sk_type=%d sk_format=%c sl=%d neg=%d",
          i,sk_type[i],sk_format[i],sl[i],neg[i]);
getch();
*/
        return(1);
        }

static int varaa_tilat_sort()
        {
        int i;
        unsigned int k;

        koko=workspace/slen;

        key=malloc((unsigned int)(koko*slen));
        if (key==NULL) { tilanpuute(); return(-1); }
        ikey=(int *)malloc(koko*sizeof(unsigned int));
        if (ikey==NULL) { tilanpuute(); return(-1); }

        return(1);
        }

static void tilanpuute()
        {
        PR_EBLD;
        sur_print("\nNot enough memory for LIST SORT!");
        PR_ENRM; WAIT;
        }

static int lue_avaimet(int koko,int *pnsort)
        {
        int k,h;
        unsigned int i,n;

        pkey=key; n=0;
        for (i=0; i<koko; ++i)
            {
            list_case_init(&list);

            k=read_case(1);
            list.n_lines=n_lines; /* 25.8.1995 */
            n_lines2=n_lines; /* 23.10.1995 */
            if (k<0) return(-1);
            if (k==0) break;

            if (list.i!=prev_chp)
                {
                h=prev_chp=list.i; sprintf(sbuf,"\nChapter %s in %s:",
                                     list.chapter[h],list.editfile[h]);
                sur_print(sbuf);
                }

            ++list.nro;
            if (l_unsuitable(&list)) PR_EINV;
            else
                {
                PR_EIN2;
                k=make_key();
                if (k<0) return(-1);
                ++nhav;
                ++n;
                if (n_show) show_case(list.nro);
                save_txtfile(list.nro);
                }

            if (list.case_var==-1) sprintf(sbuf," %ld",list.nro);
            else
                {
                k=list.case_var;
                list_data_load(&list,k);
                h=list.varlen[k];
                strcpy(sbuf," "); strncat(sbuf+1,list.value[k],h);
                while (h>1 && sbuf[h]==' ') sbuf[h--]=EOS;
                }
            sur_print(sbuf);
            PR_EINV;
            if (l_wait) sur_wait((long)(1000.0*l_wait),nop,1);
            }
        *pnsort=n;
        return(1);
        }

static int read_case(int filter)
        {
        int i,k,h;
        unsigned char x[LLENGTH],xs[LLENGTH];
        int len;

        rewind(tempf);
        n_lines=0;
        first_line=0;

        while (1)
            {
            k=list_next_line_read(&list,x,xs,&h);
            len=strlen(x);
            if (*codefile && filter)  /* talletettaessa filter=0 */
                for (i=0; i<len; ++i) x[i]=code[(int)x[i]];
            if (k<0) return(-1);
            if (!k) return(0);
            if (first_line==0)
                {
                if (case_start==*x) first_line=k;
                else if (case_start==EOS && strncmp(x+1,space,c2)!=0) first_line=k;
                if (first_line) chapter=h;
                }
            if (first_line==0) continue;

            if (lwidth<len) len=lwidth;
            for (i=0; i<len; ++i) putc((int)x[i],tempf);
            for (i=len; i<lwidth; ++i) putc((int)' ',tempf);
/*          len=strlen(xs);
            if (lwidth<len) len=lwidth;
*/
            for (i=0; i<len; ++i) putc((int)xs[i],tempf);
            for (i=len; i<lwidth; ++i) putc((int)' ',tempf);
            ++n_lines;
/*
{
printf("\n x=%s",x);
printf("\nxs=%s",xs); getch();
}
*/
            if (case_end==*x) return(1);
            else if (case_end==EOS && strncmp(x+1,space,list.c2)==0) return(1);
            }
        return(1);
        }

static int tutki_show()
        {
        int i,j;
        char x[LLENGTH],*osa[64];

        n_show=0;
        i=spfind("SHOW");
        if (i<0) return(1);
        strcpy(x,spb[i]); n_show=split(x,osa,64);
        show_var=(int *)muste_malloc(n_show*sizeof(int));
        if (show_var==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<n_show; ++i)
            {
            j=list_varfind(&list,osa[i],1);
            if (j<0) return(-1);
            show_var[i]=j;
            }
        return(1);
        }

static int show_case(long n)
        {
        int i,k,h;
        char x[LLENGTH],xs[LLENGTH];
        int len;

        rewind(tempf); sur_print("\n"); PR_ENRM;
        for (k=0; k<n_lines; ++k)
            {
            for (i=0; i<lwidth; ++i) x[i]=(char)getc(tempf);
            for (i=0; i<lwidth; ++i) xs[i]=(char)getc(tempf);
            sprintf(sbuf,"\n%.*s",c3+8,x); sur_print(sbuf);
            }
        PR_EUDL;
        sprintf(sbuf,"\nSelected fields in case %ld:\n",n);
        sur_print(sbuf);
        PR_EIN2;
        for (i=0; i<n_show; ++i)
            {
            k=show_var[i];
            list_alpha_load(&list,k,x);
            sprintf(sbuf,"%s=%s ",list.varname[k],x); sur_print(sbuf);
            }
        PR_EINV; sur_print("\nTo continue, press ENTER!");
        WAIT; // getch();
        return(1);
        }

static int make_key()
        {
        int i,k,h;
        char *p;

        for (k=0; k<nsk-2; ++k)
            {
            i=read_key(&list,k);
            if (i<0) return(-1);
            }
        p=pkey+sp[nsk-2]; *(int *)p=chapter;
        p=pkey+sp[nsk-1]; *(int *)p=first_line;
        pkey+=slen;
        return(1);
        }

static void lajittelu()
        {
        unsigned int k;

        sur_print("\nInternal sorting ...");
        sort1(0,nsort-1,0);
/*
for (k=0; k<nsort; ++k)
    {
    printf("\n%d %.*s",k,slen,key+ikey[k]);
    }
getch();
*/
        }

static void sort1(unsigned int j1,unsigned int j2,int t)
        {
        unsigned int k1,k2;
        int len;


        shell_sort(j1,j2,t);
    /*  len=su[t]-sl[t]+1; */
        len=sl[t];

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

static void shell_sort(unsigned int j1,unsigned int j2,int t)
        {
        unsigned int n,h,i,k;
        char ind;
        int len;
        char *p;
        int iso;

        n=j2-j1+1;
        iso=0; if (n>100) iso=1;
        if (iso)
            {
            if (t<nsk-2) { sprintf(sbuf,"\n%s:",word[4+t]); sur_print(sbuf); }
            else { sprintf(sbuf,"\nOriginal order:"); sur_print(sbuf); }
            sprintf(sbuf," %u-%u ",j1+1,j2+1); sur_print(sbuf);
            sprintf(sbuf," n=%u",n); sur_print(sbuf);
            }
        len=sl[t];
        h=n;
/* printf("\nsort_format=%c",sk_format[t]); getch(); */
        switch(sk_format[t])
            {
          case 'S':
            while (h>1)
                {
                h/=2; if (iso) { sprintf(sbuf," %u",h); sur_print(sbuf); }
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
                h/=2; if (iso) { sprintf(sbuf," %u",h); sur_print(sbuf); }
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
                h/=2; if (iso) { sprintf(sbuf," %u",h); sur_print(sbuf); }
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
                h/=2; if (iso) { sprintf(sbuf," %u",h); sur_print(sbuf); }
                while (1)
                    {
                    ind='1';
                    for (i=j1; i<=j2-h; ++i)
                        {
                  if ((neg[t] &&
                      *(int *)(key+ikey[i]+sp[t])<*(int *)(key+ikey[i+h]+sp[t]))
                      ||
                      (!neg[t] &&
                      *(int *)(key+ikey[i]+sp[t])>*(int *)(key+ikey[i+h]+sp[t])))
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
                h/=2; if (iso) { sprintf(sbuf," %u",h); sur_print(sbuf); }
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
                h/=2; if (iso) { sprintf(sbuf," %u",h); sur_print(sbuf); }
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

            }
        }

/* sort1.c (LIST SORT) 3.4.1995/SM (30.4.1995)
*/

static int read_key(SURVO_LIST *l, int k)
        {
        int i;
        char h;
        char x[LLENGTH];
        double a;
        int sk;


        sk=sk_v[k];
        h=l->vartype[sk];

        list_data_load(l,sk);

        /* puuttuvan tiedon testaus puuttuu !!! */

        memcpy(pkey+sp[k],l->value[sk],sl[k]);

        return(1);
        }

/* sort2.c 5.12.1990/SM (17.4.1995) (20.10.1995)
   LIST SORT   (muunnelma sort2.c:sta FILE SORT:issa)
*/

static int talletus(int kierros)
/* SORT*0.TMP  *=kierros */
        {
        int i,k,h;
        long j,alku;
        int kpl,edit_line;
        char pathname[LNAME];
        char nimi2[LNAME];
        char x[LLENGTH];
        char xs[LLENGTH];
        char *p;
        int pros;
        long nro;
        int n_edit_lines;
        int edtnro;

        sprintf(nimi2,"%sSORT%d0.TMP",etmpd,kierros);

        if (n_osat>1)
            {
            sortf=muste_fopen(nimi2,"rb");
            if (sortf==NULL)
                {
                sprintf(sbuf,"\nCannot read temporary file %s",nimi2); sur_print(sbuf);
                WAIT; return(-1);
                }

            p=(char *)&nhav;
            for (i=0; i<sizeof(long); ++i)
                {
                *p=getc(sortf);
                ++p;
                }
            }
        if (samplesize) { if (samplesize<nhav) nhav=samplesize; else samplesize=nhav; }

        sprintf(sbuf,"\n\nSaving %ld sorted cases as a list %s ...",nhav,newlist); sur_print(sbuf);


        edtnro=1;
        edtnro=create_edit_file_sort(edtnro);
        if (edtnro<0) return(-1); /* oli (i<0) */
        n_edit_lines=4;

        pros=1;
        for (j=0; j<(long)nhav; ++j)
            {
            nro=j+1;
            if (n_osat==1)
                {
                kpl=*(int *)(key+ikey[(unsigned int)j]+sp[nsk-2]);
                edit_line=*(int *)(key+ikey[(unsigned int)j]+sp[nsk-1]);

/*     printf("kpl=%s edit_line=%d\n",list.editfile[kpl],edit_line); getch();
*/
                }
            else
                {
                p=x;
                lue_hav(sortf,p);
                kpl=*(int *)(x+sp[nsk-2]);
                edit_line=*(int *)(x+sp[nsk-1]);
                }

            if (nhav<1000L)
                {
                sprintf(sbuf," %ld",nro); sur_print(sbuf);
                }
            else if (100L*j>=pros*nhav)
                {
                sprintf(sbuf," %d%%",pros); sur_print(sbuf);
                ++pros;
                }
            i=list_seek_line(&list,kpl,edit_line);
            if (i<0) return(-1);
            i=read_case(0); /* 20.10.1995 talletettaessa filter=0 */
            if (i<0) return(-1);

            if (n_edit_lines+n_lines>lr2)
                {
                i=close_edit_file_sort(n_edit_lines); if (i<0) return(-1);
                edtnro=create_edit_file_sort(edtnro);
                if (edtnro<0) return(-1);  /* oli (i<0) */
                n_edit_lines=4;
                }
            n_edit_lines+=n_lines;
            rewind(tempf);
            for (k=0; k<n_lines; ++k)
                {
                for (i=0; i<lwidth; ++i) x[i]=(char)getc(tempf); x[lwidth]=EOS;
                i=xsave2(x,edt); if (i<0) return(-1);
                for (i=0; i<lwidth; ++i) xs[i]=(char)getc(tempf); xs[lwidth]=EOS;
                i=xsave2(xs,shadows); if (i<0) return(-1);
                }
            list_close(&list);
            }
        i=close_edit_file_sort(n_edit_lines); if (i<0) return(-1);

        muste_fclose(sortf);
        sur_delete1(nimi2);
//      sprintf(sbuf,"DEL %s",nimi2);
//      system(sbuf);

        edread(sbuf,r1+r-1);
        lst_save(sbuf+1,newlist_path,newlist,edtnro-1,list.case_start,list.case_end);
        return(1);
        }

/*
ei_tilaa(s)
char *s;
        {
        sprintf(sbuf,"\nNot space enough for file %s!",s); sur_print(sbuf);
        WAIT;
        }
*/

static void conv(unsigned char *sana)
        {
        int i;

        for (i=0; i<strlen((char *)sana); ++i) sana[i]=code[sana[i]];
        }



static int load_codes(char *codefile,unsigned char *code)
        {
        int i;
        char x[LLENGTH];

        strcpy(x,codefile);
        if (!muste_is_path(x))
            { strcpy(x,survo_path); strcat(x,"SYS\\"); strcat(x,codefile); }

        codes=muste_fopen2(x,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nCode conversion file %s not found!",x); sur_print(sbuf);
            WAIT; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose2(codes);
        return(1);
        }



static int osatalletus(unsigned int nsort,int k)
        {
        unsigned int j;
        int i;
        char *p;
        char nimi[LLENGTH];
        char x[LLENGTH];
        long lnsort;

        sprintf(nimi,"%sSORT0%d.TMP",etmpd,k);
        sprintf(sbuf,"\nSaving sort keys (n=%u) in %s",nsort,nimi);
        sur_print(sbuf);
        sortf=muste_fopen2(nimi,"wb");
        if (sortf==NULL)
            {
            sprintf(sbuf,"\nCannot create temporary file %s",nimi); sur_print(sbuf);
            WAIT; return(-1);
            }
        lnsort=nsort;
        p=(char *)&lnsort;
        for (i=0; i<sizeof(long); ++i)
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
                sur_print(sbuf); WAIT; muste_fclose2(sortf); return(-1);
                }
            }
        muste_fclose2(sortf);
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


        sprintf(sbuf,"\n\nMerging %d files ...",n_osat); sur_print(sbuf);

        while (1)
            {
            ++kierros;

            sprintf(sbuf,"\nRound %d:",kierros+1);
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
        return(kierros);  /* jotta tiedetaan nimi */
        }

static int lomita(int file1,int file2,int kierros,int nro)
        {
        int i,k,nfi,ix;
        long unf,l;
        char nimi[LLENGTH];
        char nimi2[LLENGTH];
        char *p;


        sprintf(sbuf,"\nMerging files SORT%d%d - %d%d.TMP to SORT%d%d.TMP: ",
                                kierros-1,file1,kierros-1,file2,kierros,nro);
        sur_print(sbuf);

        nfi=file2-file1+1;
/* jos nfi==1, riittaa nimen muutos */
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
            osaf[i]=muste_fopen2(nimi,"rb");
            if (osaf[i]==NULL)
                {
                sprintf(sbuf,"\nCannot open temporary file %s",nimi); sur_print(sbuf);
                WAIT; return(-1);
                }
            p=(char *)&l;
            for (k=0; k<sizeof(long); ++k) { *p=getc(osaf[i]); ++p; }
            unf+=l; nf[i]=l;
            p=key+i*slen;
            lue_hav(osaf[i],p);
            }

        sprintf(sbuf,"n=%ld",unf); sur_print(sbuf);
        sprintf(nimi,"%sSORT%d%d.TMP",etmpd,kierros,nro);
        sortf=muste_fopen2(nimi,"wb");
        if (sortf==NULL)
            {
            sprintf(sbuf,"\nCannot create temporary file %s",nimi); sur_print(sbuf);
            WAIT; return(-1);
            }
        p=(char *)&unf;
        for (i=0; i<sizeof(long); ++i)
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
        muste_fclose2(sortf);
        for (i=0; i<nfi; ++i)
            {
            muste_fclose2(osaf[i]);
/*          sprintf(nimi,"DEL %sSORT%d%d.TMP",etmpd,kierros-1,file1+i);
            system(nimi);
*/
            sprintf(nimi,"%sSORT%d%d.TMP",etmpd,kierros-1,file1+i);
            remove(nimi);
            }
        return(1);
        }

static void lue_hav(FILE *tied, char *p)
        {
        int i;
        for (i=0; i<slen; ++i) { *p=getc(tied); ++p; }
        }


static int vertailu(int k0,int k)
        {
        int t,h;
        int len;
        double da;
        long la;
        float fa;
        int ia;

        if (nf[k0]==0) return(1);
        if (nf[k]==0) return(0);

        for (t=0; t<nsk; ++t)
            {
            len=sl[t];
            switch (sk_format[t])
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
              case '2':  /* varlen=sizeof(int) eli 4 */
                ia=*(int *)(key+k0*slen+sp[t])-*(int *)(key+k*slen+sp[t]);
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
              default: sprintf(sbuf,"\n %c puuttuu!!! %d",sk_format[t],t); sur_print(sbuf); WAIT;
                }
            if (h<0) return(0);
            if (h>0) return(1);
/*
 if (t==nsk-1) { printf("\nformat=%c",sk_format[t]);
     printf(" %d %d",*(int *)(key+k0*slen+sp[t]),*(int *)(key+k*slen+sp[t]));
        getch();
               }
*/
            }
    sprintf(sbuf,"\n???t=%d format=%c",t,sk_format[t]); sur_print(sbuf); WAIT; return(0);
        }

/* sort3.c 5.12.1990/SM (10.4.1995)
   LIST SORT
*/
static int create_edit_file_sort(int edtnro)
        {
        int i,k;
        char *p,*q;
        char nimi0[LNAME];
        char nimi1[LNAME];
        char nimi2[LNAME];
        char nimi3[LNAME];

        strcpy(nimi1,newlist); sprintf(sbuf,"%d",edtnro); strcat(nimi1,sbuf);
        strcpy(nimi2,newlist); sprintf(sbuf,"%d",edtnro+1); strcat(nimi2,sbuf);
        strcpy(nimi0,newlist_path); strcat(nimi0,nimi1); strcat(nimi0,".EDT");

sprintf(sbuf,"\nSaving in edit file %s ...",nimi0); sur_print(sbuf);
/* Mahdollinen tarkistus, ettei talleteta input-kenttien pÑÑlle, puuttuu!!! */
/* for (i=0; i<list.n; ++i) printf("\n%s",list.editfile[i]); getch(); */
        edt=list_fopen(nimi0,"wb");
        if (edt==NULL)
            {
            sprintf(sbuf,"\nCannot open edit file %s for a sorted list!",
                                     nimi0);
            sur_print(sbuf); WAIT; return(-1);
            }


        /* header kirjoitetaan vasta sulkiessa, kun varjot tiedossa" */
        muste_fseek(edt,(long)(lwidth+1),SEEK_SET);
        sprintf(sbuf,"*SAVE %s / LIST SORT %s",nimi1,word[2]);
        i=xsave2(sbuf,edt); if (i<0) return(-1);
        sprintf(sbuf,"*LOAD %s",nimi2);
        i=xsave2(sbuf,edt); if (i<0) return(-1);
        i=xsave2("*",edt); if (i<0) return(-1);
        i=xsave2("*DEF A,5,END",edt); if (i<0) return(-1);

        strcpy(nimi3,etmpd); strcat(nimi3,"SURVO2.TMP");
        shadows=muste_fopen(nimi3,"w+b");
        if (shadows==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s!",
                                     nimi3);
            sur_print(sbuf); WAIT; return(-1);
            }

        for (k=0; k<4; ++k)
            {
            i=xsave2(" ",shadows); if (i<0) return(-1);
            }
        ++edtnro;
        return(edtnro);
        }

static int close_edit_file_sort(int n_edit_lines)
        {
        int i,h;
        char header[LLENGTH];
        char number[16];
        char xs[LLENGTH];
        int empty,shad;


        for (h=n_edit_lines; h<lr2; ++h)
            {
            i=xsave2("*",edt); if (i<0) return(-1);
            }

        shad=0; empty=1;
        rewind(shadows);

        for (h=0; h<n_edit_lines; ++h)
            {
            empty=read_shad(xs+2);
            if (!empty) break;
            }

        if (!empty)
            {
            i=xsave2("Shadows",edt); if(i<0) return(-1);
            shad=1;
            i=shadsave(xs,h+1); if(i<0) return(-1);
            ++h;
            for (; h<n_edit_lines; ++h)
                {
                empty=read_shad(xs+2);
                if (empty) continue;
                i=shadsave(xs,h+1); if(i<0) return(-1);
                ++shad;
                }
            xsave2("END",edt);
            }

        if (shad<lshad) shad=lshad;
        else
            {
            if ((long)(lr2+shad)*(long)lwidth>65535L)
                {
                sur_print("\n# of shadow lines in the current edit file will make the file over 64KB!");
                sur_print("\nGive a smaller value for #lin in LISTDIM=#lin,#col,#shad");
                sprintf(sbuf,"\nCurrent status is LISTDIM=%d,%d,%d",lr2,lwidth,lshad);
                sur_print(sbuf); WAIT; return(-1);
                }
            }
        strcpy(header,"SURVO84ED ");
        strcat(header,muste_itoa(lwidth+1,number,10)); strcat(header," ");
        strcat(header,muste_itoa(lr2,number,10)); strcat(header," ");
        strcat(header,"                    "); header[20]=EOS;
        strcat(header,muste_itoa(lwidth+1,number,10)); strcat(header," S");
        strcat(header,muste_itoa(shad,number,10)); strcat(header," ");

        rewind(edt);
        i=xsave2(header,edt); if (i<0) return(-1);


        fclose(edt); fclose(shadows);
        return(1);
        }


static int shadsave(char *xs,int h)
        {
        int i;
        unsigned short *pint;

        pint=(unsigned short *)xs;
        *pint=h;
        for (i=0; i<lwidth+1; ++i) putc((int)xs[i],edt);
        if (ferror(edt)) { sur_print("\nCannot save results on disk! (end of space?)");
                         WAIT; return(-1);
                         }
        return(1);
        }

static int read_shad(char *xs)
        {
        int i;
        int empty;

        empty=1;
        for (i=0; i<lwidth+1; ++i)
            {
            xs[i]=(char)getc(shadows);
            if (empty && xs[i]!=' ') empty=0;
            }
        return(empty);
        }

/* sort4.c 16.3.1996/SM (23.3.1996)
   LIST SORT    FILE=<text_file>,<delimiter>
*/

static int tutki_txtfile(int k)
        {
        int i;
        char x[LLENGTH],*osa[2];

        if (k==1) /* LIST SORT */
            {
            i=spfind("FILE");
            if (i<0) { *txt_file=EOS; return(1); }
            strcpy(x,spb[i]); i=split(x,osa,2);
            }
        else /* LIST SAVE */
            {
            osa[0]=word[4]; i=1;
            if (g>5) { osa[1]=word[5]; i=2; }
            }
        erotin='|';
        if (i==2)
            {
            if (muste_strnicmp(osa[1],"char(",5)==0) erotin=atoi(osa[1]+5);
            else erotin=*osa[1];
            }
        strcpy(txt_file,osa[0]);
        if (!muste_is_path(txt_file)) { strcpy(txt_file,edisk); strcat(txt_file,osa[0]); }
        txtfile=muste_fopen(txt_file,"wt");
        if (txtfile==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s for output data!",txt_file);
            sur_print(sbuf); WAIT; return(-1);
            }

        v=(int *)muste_malloc(list.m*sizeof(int));
        if (v==NULL) { tilanpuute(); return(-1); }

        md=0;
        for (i=0; i<list.m; ++i)
            if (list.vartype[i]!='-') v[md++]=i;
        for (i=0; i<md; ++i)
            {
            fprintf(txtfile,"%s",list.varname[v[i]]);
            if (i<md-1) fprintf(txtfile,"%c",erotin);
            }
        fprintf(txtfile,"\n");
        return(1);
        }

static int save_txtfile(long n)
        {
        int i;
        char x[LLENGTH];

        if (*txt_file==EOS) return(1);
        rewind(tempf);
        for (i=0; i<md; ++i)
            {
            list_alpha_load(&list,v[i],x);
            fprintf(txtfile,"%s",x);
            if (i<md-1) fprintf(txtfile,"%c",erotin);
            }
        fprintf(txtfile,"\n");

        return(1);
        }

/* SAVE.C (LIST SAVE) 25.3.1996/SM (25.3.1996)

LIST SAVE <list> TO <text_file>,<delimiter>
0    1    2      3  4           5
*/

static int list_save()
        {
        int i,h,k;
        char x[LLENGTH],*osa[3];


        lwidth=252;
        i=list_rewind(&list,"r+b"); if (i<0) return(-1);

        i=tutki_show(); if (i<0) return(-1);
        i=tutki_txtfile(2);
                      /* luetaan <text_file>,<delimiter> */
        if (i<0) return(-1);

        i=tempf_open(); if (i<0) return(-1);

        for (i=0; i<r3; ++i) sur_print("\n");

        sur_print("\nLoading cases:");
        while (1)
            {
            list_case_init(&list);
            k=read_case(1);
            list.n_lines=n_lines;
            n_lines2=n_lines;
            if (k<=0) break;

            if (list.i!=prev_chp)
                {
                h=prev_chp=list.i; sprintf(sbuf,"\nChapter %s in %s:",
                                     list.chapter[h],list.editfile[h]);
                sur_print(sbuf);
                }

            ++list.nro;

            if (l_unsuitable(&list)) PR_EINV;
            else
                {
                PR_EIN2;
                ++n;
                if (n_show) show_case(list.nro);
                save_txtfile(list.nro);
                }

            if (list.case_var==-1) sprintf(sbuf," %ld",list.nro);
            else
                {
                k=list.case_var;
                list_data_load(&list,k);
                h=list.varlen[k];
                strcpy(sbuf," "); strncat(sbuf+1,list.value[k],h);
                while (h>1 && sbuf[h]==' ') sbuf[h--]=EOS;
                }
            sur_print(sbuf);
            PR_EINV;
            }
        return(1);
        }

/* lcond.c 12.5.90/SM (29.4.1995)
   derived from \da\dat6.c
*/

static int list_conditions(SURVO_LIST *d)
        {
        int i,k;
        char x[3*LLENGTH];
        char s[LLENGTH];
        char *p,*q;
        int n_select_space;
/*        char siirtop[16];  */
        n_select=k=0;
        i=spfind("IND"); if (i>=0) ++k;
        i=spfind("CASES"); if (i>=0) ++k;
        i=spfind("SELECT");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            
            if (strchr(x,',')!=NULL || strchr(x,':')!=NULL) // RS ADD
              {
              strcpy(x,"SELECT");
              }

            if (strchr(x,'(')!=NULL)
                {
                bool_norm(x);
/*              strcpy(s,survo_path); strcat(s,"BOOLPAR.EXE");
                sprintf(siirtop,"%p",&x);
                i=spawnl(P_WAIT,s,s,siirtop,NULL);
                if (i<0)
                    {
                    list_sel_virhe("SELECT (not enough memory)");
                    return(-1);
                    }
*/
                if (*x==EOS)
                    {
                    sur_print("\nSELECT error"); WAIT; return(-1);
                    }
                }
            p=x;
            while (*p) { if (*p=='*' || *p=='+') ++n_select; ++p; }
            ++n_select;

            }
        
        
        if (n_select==0 && k==0) return(1);
        n_select+=2;  /* aina tilat 0 ja 1 IND ja CASES */

        sel_var=(int *)muste_malloc(n_select*sizeof(int));
        if (sel_var==NULL) { ltilavirhe(); return(-1); }
        sel_type=muste_malloc(n_select);
        if (sel_type==NULL) { ltilavirhe(); return(-1); }
        sel_rel=muste_malloc(n_select);
        if (sel_rel==NULL) { ltilavirhe(); return(-1); }
        sel_lower=(double *)muste_malloc(n_select*sizeof(double));
        if (sel_lower==NULL) { ltilavirhe(); return(-1); }
        sel_upper=(double *)muste_malloc(n_select*sizeof(double));
        if (sel_upper==NULL) { ltilavirhe(); return(-1); }
        sel_cases=(char **)muste_malloc(n_select*sizeof(char **));
        if (sel_cases==NULL) { ltilavirhe(); return(-1); }
        sel_lastcase=(char **)muste_malloc(n_select*sizeof(char **));
        if (sel_lastcase==NULL) { ltilavirhe(); return(-1); }
        sel_neg=muste_malloc(n_select);
        if (sel_neg==NULL) { ltilavirhe(); return(-1); }

        sel_var[0]=sel_var[1]=-2; sel_neg[0]=sel_neg[1]=' ';
        i=list_find_cond(d,"IND",0);
        if (i==-2) { list_sel_virhe("IND"); return(-1); }
        i=list_find_cond(d,"CASES",1);
        if (i==-2) { list_sel_virhe("CASES"); return(-1); }

        if (n_select==2) return(1);
        p=x; sel_rel[2]='*';
        for (k=2; k<n_select; ++k)
            {
            q=p;
            while (*q && *q!='*' && *q!='+') ++q;
            if (*q) sel_rel[k+1]=*q;
            i=q-p; strncpy(s,p,i); s[i]=EOS; p=q+1;
            i=list_find_cond(d,s,k);
            if (i<0) { list_sel_virhe(s); return(-1); }
                /* i==-2 -22.4.1992 */
            }
        return(1);
        }

static int ltilavirhe()
        {
        sur_print("\nSpace not enough for LIST conditions!");
        WAIT; return(1);
        }

static void list_sel_virhe(char *s)
        {
        sprintf(sbuf,"Error in %s specification!",s);
        if (etu==2)
            {
            sprintf(tut_info,"˛˛˛@10@CONDITIONS@%s@",sbuf);
            return;
            }
        sur_print("\n"); sur_print(sbuf); WAIT;
        return;
        }

static int list_find_cond(SURVO_LIST *d, char *nimi, int nro)
        {
        int i,k;
        char x[LLENGTH],*sana[3];
        char *p,*q;
        char *nimi2;

        nimi2=nimi; sel_neg[nro]=' ';
        if (*nimi2=='!') { ++nimi2; sel_neg[nro]='!'; }

        i=spfind(nimi2);
        if (i<0) return(-1);
        strcpy(x,spb[i]);
        k=split(x,sana,3);
        if (k==0) return(-2);

        p=strchr(sana[0],':');
        if (p==NULL) /* IND-tyyppinen */
            {
            sel_type[nro]='0';
            if (muste_strcmpi(sana[0],"ORDER")==0) sel_var[nro]=-1;
            else
                {
                sel_var[nro]=list_varfind(d,sana[0],0); if(sel_var[nro]<0) return(-2);
                }
            sel_lower[nro]=sel_upper[nro]=1.0;
            if (k>1)
                {
                sel_lower[nro]=sel_upper[nro]=atof(sana[1]);
                if (k>2) sel_upper[nro]=atof(sana[2]);
                }
            }

        else  /* CASES-tyyppinen */
            {
            sel_type[nro]='1';
            *p=EOS;
            sel_cases[nro]=spb[i]+(p-x+1);
            p=q=sel_cases[nro];
            while (*p)
                {
                if (*p==',') q=p+1;
                ++p;
                }
            sel_lastcase[nro]=q;
            sel_var[nro]=list_varfind(d,sana[0],0); if (sel_var[nro]<0) return(-2);
            if (d->vartype[sel_var[nro]]!='S')
                {
                sprintf(sbuf,"Variable %s not a string!",sana[0]);
                if (etu==2)
                    {
                    sprintf(tut_info,"˛˛˛@11@CONDITIONS@%s@",sbuf); return(-2);
                    }
                sur_print("\n"); sur_print(sbuf); WAIT; return(-2);
                }
            }
        return(1);
        }

static int l_unsuitable(SURVO_LIST *d)
        {
        int i,k,h;

        if (n_select==0) return(0);
        if (sel_var[0]>-2 && l_unsuit(d,0)) return(1);
        if (sel_var[1]>-2 && l_unsuit(d,1)) return(1);

        if (n_select==2) return(0);
        h=0; k=2;
        while (k<n_select)
            {
            if (h && sel_rel[k]=='+') return(0);
            if (l_unsuit(d,k))
                {
                ++k;
                while (k<n_select && sel_rel[k]=='*') ++k;
                if (k==n_select) return(1);
                h=0; continue;
                }
            ++k; h=1;
            }
        return(0);
        }

static int lt_neg(int i,int nro)
        {
        if (sel_neg[nro]=='!') return(1-i);
        else return(i);
        }

static int l_unsuit(SURVO_LIST *d, int nro)
        {
        int len;
        double x;
        char *p;
        char sana[LLENGTH];

        if (sel_type[nro]=='0')
            {
            if (sel_var[nro]==-1) x=d->nro; /* ORDER */
            else list_data_load8(d,sel_var[nro],&x);
            if (x<sel_lower[nro] || x>sel_upper[nro]) return(lt_neg(1,nro));
            return(lt_neg(0,nro));
            }
        p=sel_cases[nro];

        list_alpha_load(d,sel_var[nro],sana+1); *sana=',';
        len=strlen(sana); while(sana[len-1]==' ') sana[--len]=EOS;
        if (strcmp(sel_lastcase[nro],sana+1)==0) return(lt_neg(0,nro));
        sana[len]=','; ++len; sana[len]=EOS;
        if (strncmp(p,sana+1,len-1)==0) return(lt_neg(0,nro));
        if (strstr(p,sana)!=NULL) return(lt_neg(0,nro));
        return(lt_neg(1,nro));
        }

static void list_sel_free()
        {
        if (n_select==0) return;

        muste_free(sel_var);
        muste_free(sel_type);
        muste_free(sel_rel);
        muste_free(sel_lower);
        muste_free(sel_upper);
        muste_free(sel_cases);
        muste_free(sel_lastcase);
        muste_free(sel_lastcase);
        muste_free(sel_neg);
        n_select=0;
        }
