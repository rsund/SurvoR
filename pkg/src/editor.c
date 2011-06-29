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
#define CR '\15'
#define OPLEN 32 /* 10.10.91 ennen 16 */

char *z;
int ed1,ed2,edshad;
int r,r1,r2,r3,c,c1,c2,c3;
char s_edisk[LNAME], s_esysd[LNAME], s_eout[LNAME];

// char edisk[LNAME], esysd[LNAME], eopen[LNAME], eout[LNAME], last_disk[3];
char *edisk, *esysd, *eout;

int etu; // RS TUT
int etuu; /* 1=lapset eivät TUT-moodissa 0=lapset TUT-moodissa */
int alkututor; // RS TUT
int wait_tut_type=0; // RS TUT // 1=cancelled by user's actions 2=activates always
long wait_hetki;
int tut_index=0;

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

char s_sapu[MAXTILA+2];
char *sapu; // RS

char s_info[LLENGTH];
char *info;
char **key_label; /* ?? */
char *key_lab;    /* ?? */
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
char tut_info2[LLENGTH];  // RS toimiiko?!?
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

char s_system_name[32] = "Muste"; // RS;
// char system_name[32] = "Muste"; // RS
char *system_name;


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
char op_sana[10],help_sana[10];
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
char rivin_loppu[]="\15\12";
int s84_warning=0; /* 1.10.1997 */
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
int ref_c1=0, ref_c, ref_r1, ref_r;

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

char OO[OPLEN+1];       /* ennen lokaaleja!!?? */
char op_tila[OPLEN+1];
char op2[OPLEN+1];
char *op_plot_word;

char sur_session[2];
int soft_vis=1;
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



int ver;


/* RS: local declarations */
static int creatshad();
static void shadinit();
static int lastline2();
static int key_common();
static int edsave();
static int edload();
static void op_scratch();
int disp();
int sur_dump();
int restore_dump();
int sys_save_restore();
int seek_char();
int seek_word();
int miau_koodit();
int op_init();
void init_param1();
void hae_edisk();


int op_file(char *op)
        {
        int i,j;
        char *s;
        char *p; // 30.4.2003
        char data_name[LNAME];
        char edit_name[LNAME];
        char list_name[LNAME];
        char x[LLENGTH];
        char x2[LLENGTH];
        int rr,cc;
        char *ss[2];
        int par3;

        if (g==1)
            {
            sur_print("\nSee FILE?");
            WAIT; return(0);
            }
        soft_vis=0;
        s=parm[1];
        strupr(s);

        if (strcmp(s,"SHOW")==0)   // RS direct call
           { 
              sur_dump("A");
              muste_file_show("A");
              restore_dump("A");
              return(0);
           } 

        if (strncmp(s,"ACT",3)==0) s[3]=EOS;
        if (strcmp(s,"UPDATE")==0 || strcmp(s,"STATUS")==0 ||
            strcmp(s,"INIT")==0 || strcmp(s,"REDUCE")==0 ||
            strcmp(s,"ACT")==0 || strcmp(s,"MASK")==0 ||
            strcmp(s,"MAKE")==0 || strcmp(s,"COND")==0 ||
            strcmp(s,"MASKLOAD")==0 || strcmp(s,"CONVERT")==0 )
            { strcpy(op,"CREATE"); return(1); }
        if (strcmp(s,"SORT")==0)
            { strcpy(op,"FSORT"); return(1); }
        if (strcmp(s,"SAVE")==0)
            if (sur_strcmpi(parm[2],"MAT")==0)
                {
                strcpy(op,"SMAT"); return(1);
                }
        if (strcmp(s,"DEL")==0)
            { 
// RS NYI            del_file(); 
            return(0); }

        if (strcmp(s,"EXPAND")==0) // 29.12.2003
            {
            strcpy(op,"COPY"); return(1);
            }

        if (strcmp(s,"MEDIT")==0) // 30.4.2003
            {
/* RS NYI
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
            temptut=fopen(sbuf,"wt");
            medit_spec_save("OPTIONS");
            medit_spec_save("RECORD");
            medit_spec_save("CHECK");
            medit_spec_save("MEDIT_VARS");
            medit_spec_save("MEDIT_SAVE");
            medit_spec_save("INDATA");
            medit_spec_save("MEDIT_TIME");
            fclose(temptut);

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
                return(1);
                }
// printf("\ndata_name=%s list_name=%s|",data_name,list_name); getch();
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
// printf("\n%d: %s",j,x+1); getch();
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
                if (strncmp(os_ver,"WIN9",4)==0)
                    sur_win9x_refresh(window_name);
                }


            medit=1;
*/
            }

        strcpy(op,s);
        return(1);
        }


int soft_disp(int visibility)
{
// RS NYI
return(1);
}

static void not_space()
        {
        sur_print("\nNot space enough for the edit field!");
        WAIT;
        }

static int ed_malloc(unsigned int ed1, unsigned int ed2, unsigned int edshad)
        {

        if (etu) tut_sulje();
        if (zs!=NULL) free((char *)zs);
        if (z!=NULL) free(z);
        z=malloc(sizeof(char)*ed1*(ed2+edshad));
        if (z==NULL) { not_space(); return(-1); }
        zs=(int *)malloc((ed2+1)*sizeof(int)); // RS oli unsigned int *
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

        z_lisays=malloc(k);
        if (z_lisays==NULL) return(-1);
        free(z_lisays); return(1);
        }


static int empty(char x[],unsigned int lev)
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


int filename(char *edfile,char *field)
        {
        int i;
        char x[LNAME];
        char *p;

        p=x;
        strcpy(p,field);
/* RS NYI
        subst_survo_path_in_editor(p);

        if (netd(p))
            {
            strcpy(edfile,p);
            return(1);
            }
*/
        *edfile=EOS;
/* RS NYI
        if (strchr(p,':')==NULL)
            {
            if (*p=='.')
                {
                ++p; // ohita piste 8.11.91
                strcat(edfile,survo_path);
                i=strlen(edfile)-1;
                if (edfile[i]=='\\') edfile[i]=EOS;
                }
            else strcat(edfile,edisk);
            }
*/
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
        fclose(edfield);    /* suljetaan malloc-varausten tiivistämiseksi */

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
        edfield=fopen(edfile,"rt");
        if (edfield==NULL) { edit_file_not_found(edfile); return(-1); }

        fgets(x,LLENGTH-1,edfield); /* otsikko uudelleen */
        c2=ed1-1; r2=ed2;
        strcpy(eopen,edfile);
        for (i=0; i<ed1*(ed2+edshad); ++i) z[i]=' ';
        for (i=0; i<ed1*ed2; i+=ed1) z[i]='*';
        shadinit();
        if (ei_onnistunut) { fclose(edfield); return(-1); }
        rivi_luettu=0; j=0;
        while (1)
            {
            fgets(x,LLENGTH+10-1,edfield);
/* printf("x=%s\n",x); getch(); */
            if (feof(edfield)) break;
            p=strchr(x,'|');
            if (p==NULL) continue;
//          if (p==NULL) { edload32_err("Missing `|'",edfile);
//                         fclose(edfield); return(-1);
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
                  fclose(edfield); return(-1);
                }
            edwrite(p,j,0);
            rivi_luettu=1;
            }
//      strcpy(edit_file32,edfile);
        fclose(edfield);
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

// RS pois       if (sur_file_time_check(edfile)==-2) return(-1); // 6.4.2001
// RS NYI       check_start_field_language(edfile); // 1.2.2006

        edfield=fopen(edfile,"rb");
        if (edfield==NULL) { edit_file_not_found(edfile); return(-1); }
        time(&aika_save);
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(edfield);
        rivi[ELE-1]=EOS;
        if (strncmp(rivi,"SURVO 98",8)==0) return(edload32(edfile));
        g=split(rivi,sana,5);
        if (strcmp(sana[0],"SURVO84ED")!=0)
            {
            sur_print("\nNot a Survo edit file!"); WAIT;
            fclose(edfield); return (-1);
            }
        ed1=atoi(sana[1]);
        ed2=atoi(sana[2]);
        if (g>4 && *sana[4]=='S') edshad=atoi(sana[4]+1);

        fclose(edfield);    /* suljetaan malloc-varausten tiivistämiseksi */
        k=ed_malloc(ed1,ed2,edshad); if (k<0) return(-1);
        edfield=fopen(edfile,"rb");
        if (edfield==NULL) { edit_file_not_found(edfile); return(-1); }

        c2=ed1-1; r2=ed2;
        strcpy(eopen,edfile);

        fseek(edfield,(long)ed1,0);
        for (i=0; i<ed1*ed2; ++i) z[i]=(char)getc(edfield);

        if (ed1<=c3) { CLS;  soft_disp(1); }
        if (feof(edfield)) { filerr(); fclose(edfield); return(-1); }
        if (shad==0) { fclose(edfield); return(1); }
        shadinit();
        for (i=0; i<ed1; ++i) x[i]=(char)getc(edfield);
        if (strncmp(x,"Sha",3)!=0) { fclose(edfield); return(1); }
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
        if (ferror(edfield)) { filerr(); fclose(edfield); return(-1); }
        fclose(edfield); return(1);
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
        extern int nop();

        if (s84_warning && !autosavefield && etu<2 && !redim_save)
                 //     && strcmpi(edfile,edit_file32)!=0)
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
        if (ferror(edfield)) { filerr(); fclose(edfield); return(-1); }
        fclose(edfield);
        if (message)
            {
            sprintf(x,"\n%.79s",stripe); sur_print(x);
            WAIT;
            }
//      if (!autosave32) strcpy(edit_file32,edfile);
        return(1);
        }


static int edsave(char *field,int shad,int check)
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
        edfield=fopen(edfile,"rb");
        if (edfield!=NULL)
                {
                if (sur_strcmpi(edfile,eopen)!=0 && etu!=2 && save_warning)
                    {
                    PR_EBLD;
                    sprintf(sbuf,"\nEdit file %s already exists! Overwrite it (Y/N)?",
                                                                       edfile);
                                sur_print(sbuf);
                    ch=(char)nextch(""); sprintf(sbuf,"%c",ch); sur_print(sbuf);
                    if (ch!='Y' && ch!='y') { fclose(edfield); return(0); }
                    }
                fclose(edfield);
                }
        }

        edfield=fopen(edfile,"wb");
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

        fseek(edfield,(long)ed1,0);

        k=0;
        for (j=0; j<ed2; ++j)
            {
            for (i=0; i<ed1; ++i, ++k) putc((char)z[k],edfield);
            if (ferror(edfield)) { filerr(); fclose(edfield); return(-1); }
            }
        if (shad==0) { fclose(edfield); return(1); }

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
        if (ferror(edfield)) { filerr(); fclose(edfield); return(-1); }
        if (check) strcpy(eopen,edfile);  /* if 17.2.89 */
        fclose(edfield); return(1);
        }





static int numshad()   /* 23.1.93 */
        {
        int j,n;

        n=0; for (j=1; j<=ed2; ++j) if (zs[j]) ++n;
        return(n);
        }

static int op_redim(
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
//                      if (x[0]=='*') { printf("\nOK"; getck(); }


                        i=-1; while (i<k)
                            {
                            ++i;
                            if (i==0) { if (x[0]!='*') break; }
                            if (i>0 && i<11) { if (x[i]!='.') break; }
                            if (i>=11 && x[i]!='.' && x[i]!=' ') break;
                            }
// printf("\nx[0]=%c x=%.50s",x[0],x);
// printf("i=%d k=%d",i,k); getck();

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
// RS if (komento && g>4) { printf("z=%u\n",(int)z); getch(); }
        return(1);
        }


static void testshad(unsigned int j)
        {
        int i;
        char x[LLENGTH];

        if (zs[j]==0) return;
        edread(x,zs[j]);
        if (!empty(x,ed1)) return;
        z[(zs[j]-1)*ed1]='\0';
        zs[j]=0;
        }

static int creatshad(unsigned int j)
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
        char *p;

        j=0; while (j<ed2) { ++j; zs[j]=0; }
        i=ed1*ed2; zshn=0;
        while ( zshn<edshad && zshn<ed2 )
            { z[i]='\0'; i+=ed1; ++zshn; }
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
            if (cc1<1) cc1=1;
         if (move_words && j>move_r1) cc1=1;
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
                while (k!='#') k=nextch(""); // RS k=getck();
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


static void pvmaika(char aika[])
        {
        time_t ltime;

        time(&ltime);
        strcpy(aika,s_time(&ltime)); aika[24]=EOS;
        }

static int prev_r,prev_c;
int headline()
        {
        char x[LLENGTH];
        int k,i,len;
        char dispm2;
        char hshadow; /* 25.11.1992 */

        pvmaika(aika);
        dispm2='0'+ntut; hshadow='8'; if (ntut==0) { dispm2=' '; hshadow='4'; }
        sprintf(x,"%c%c%4u",pref,dispm2,c1+c-1);
        write_string(x,6,hshadow,1,1);
        sprintf(x,"%3u",c1); write_string(x,3,')',1,7);
    /*  CURSOR_ON; */ sprintf(x," %s ",system_name);

        write_string(x,strlen(system_name)+2,'7',1,10);
        k=20+c3-72;

        strcpy(sbuf,edisk); // RS NYI unsubst_survo_path_in_editor(sbuf);
        sprintf(x,"  %s %*.*s%7d%5d ",aika,k,k,sbuf,r2,c2);
        write_string(x,strlen(x),hshadow,1,20);

        if (c2>996) // 31.7.2008
            {
            i=strlen(x)+14;
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


static void displine(unsigned int j,unsigned int lev)
        {
        char x[LLENGTH];
        char *px,*pxs;
        int i,k;
        char dispx;

/*      CURSOR_OFF;             */
        if (rsh!=j) sprintf(x,"%6u ",j); else sprintf(x,"Shadow:");
        x[7]=z[(j-1)*ed1]; x[8]=EOS;
        if (x[7]=='?') { write_string(space,c3+8,' ',j-r1+2,1); return; }
        if (!line_labels_off) write_string(x,8,'2',j-r1+2,1);
                         else write_string(space,8,' ',j-r1+2,1);

        if (c3>c2) { write_string(space,c3-c2,'7',j-r1+2,c2+9); }
        if (lev>0)
            {
            px=z+(j-1)*ed1+c1;

            if (*px=='.')
                {
                i=1;
                while (px[i]=='.' && i<lev) ++i;
                if (i==lev) { write_string(stripe,lev,' ',j-r1+2,9); return; }
                }
            if ( zs[j]==0 )
                {
                write_string(px,lev,' ',j-r1+2,9);
                if (move_ind>1) move_disp(j);
                return;
                }

            pxs=z+(zs[j]-1)*ed1+c1;
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
        write_string(space,c3+8,' ',r3+2,1);
        if (prompt_line!=NULL)
            {
            write_string(prompt_line,strlen(prompt_line),sh,r3+2,1);
            }
        }

int disp()
        {
        unsigned int i,lev;
        int k;

        CURSOR_OFF;

        headline();

        lev=c3; if (c2-c1+1<c3) lev=c2-c1+1;
        k=r3; if (r2<r3) k=r2;
        for (i=0; i<k; ++i) displine(r1+i,lev);
        disp_prompt_line(prompt_shadow);
        if (display_off) soft_disp(0);

        cursor(r,c);
        if (insert_mode) CURSOR_INS; else CURSOR_ON;
        return(1);
        }

static void dispch(int m)
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

void prompt(char *kysymys,char *vastaus,int pituus)
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
// RS NYI            SAVE_CURSOR;
            m=nextch("");
// RS NYI            RESTORE_CURSOR;
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
        char x[LLENGTH], x1[LLENGTH];

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
        char x[LLENGTH], x1[LLENGTH];
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
        unsigned int i,j,len,shad;
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
                    char x[LLENGTH], x1[LLENGTH];

        m_move_ind2=0;
        if (move_ind || m_move_ind)
            { move_clear(); m_move_ind=0; soft_disp(1); return; }
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

int lopetuskysely()
        {
        int i,i2,i3;
/*
        extern int etu,ntut;
        extern FILE *tutor;
        extern int exit_warning;
*/

        if (!exit_warning) return(1);

        PR_EBLK; cursor(r3+1,1);
        sur_print("Exit from SURVO MM (Y/N)?");
        i=nextch(); if (i=='Y' || i=='y')
                     {
// RS NYI                     NORMAL_SCREEN; CURSOR_ON; 
                     edrun=0; return(1);
                     }
        disp();
        return(0);
        }

int line_copy()
        {
        unsigned int i,j,len,shad,k;
        char x[LLENGTH], x1[LLENGTH];
        char sx[2*LLENGTH], sx1[2*LLENGTH];
        unsigned int len2;
        extern int ref_r1,ref_r;


        if (move_ind) pyyhi_alarivi();

        soft_disp(0);
        cursor(r,c);
        PR_EBLK; sprintf(sbuf,"%c",(char)PREFIX); sur_print(sbuf);
        cursor(r3+1,1); PR_EBLD;
        *x=EOS; prompt("Line to be copied ? ",x,6);

        j=r1+r-1;
        if (*x==EOS) i=j;
        else if (*x=='*') // 19.8.2007
            {
            k=ref_r1+ref_r-1;
            if (k<1) i=1;
            else i=k;
            }
        else
            {
            i=edline2(x,1,1); if (i==0) return(1);
            }

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
        unsigned int j1,j2;
        int col;
        int vr1;
        int vc1;

        if (g<2) { op_incomplete(); return(-1); }
        displine2(r1+r-1);  /* 6.6.1989 */
        vr1=r1;
        if (sur_strcmpi(parm[1],"BORDER")==0) // 19.10.2001
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

int op_resize()
    {
    int i;
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

    if (r_soft) rs=r_soft+1; else rs=0;
    if (rr+rs+2<25 && cc<72) cc=72; // RS cc>72 sallittu;  pienillä r3, c3 oltava 72
    muste_resize(cc+8,rr+2+rs); // RS CHA   i=sur_resize1(cc+8,rr+2+rs);
    r3=rr; if (r>r3) r=r3;
    c3=cc; if (c>c3) c=c3;

    if (r2-r1<r3) { r+=r3-(r2-r1)-1; r1=r2-r3+1; }  // 22.1.2007

// RS REM    set_console_title();

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

void eras(int j,int col)    /* col=c1+c-1  */
        {
        char x[LLENGTH];
        edread(x,j);
        x[col]=EOS; strncat(x,space,ed1-col);
        edwrite(x,j,0);
        }

void line_erase(int j,int col)  /* col=c1+c-1  sama kuin erase si.c */
        {
        char x[LLENGTH], x1[LLENGTH];

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
        char x[LLENGTH], x1[LLENGTH];

        edread(x,r2);
        if (!empty_line(x+1,c2)) { BEEP; exit(1); }
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
        char x[LLENGTH], x1[LLENGTH];
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
        unsigned int i,len,shad;
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
        survoxxx=fopen(survowrd,"rt");
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
        fclose(survoxxx);

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

void store_move()
        {
        int i,k;
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
// RS NYI        if (hae_apu("autotrim",trim_command)) trim(r_alku,r_loppu);
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
    survoxxx=fopen(sname,"wt");
    if (survoxxx==NULL) return(1);
    if (mr1>mr2) { j=mr1; mr1=mr2; mr2=j; }
    if (mc1>mc2) { j=mc1; mc1=mc2; mc2=j; }
    for (j=mr1; j<=mr2; ++j)
        {
        edread(x,j);
        x[c2+1]=EOS;
        fprintf(survoxxx,"%s\n",x+mc1);
        }
    fclose(survoxxx);
    return(1);
    }


int save_words(char *tiedosto)
        {
        int j;
        char x[LLENGTH];

        survoxxx=fopen(tiedosto,"wt");
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
        fclose(survoxxx);
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
        char xs[LLENGTH];
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
        int i;
        int j;
        char x[LLENGTH], x1[LLENGTH];

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
// printf("\nsurvoblo=%s|",survoblo); getck();
        survoxxx=fopen(survoblo,"rt");
        if (survoxxx==NULL) return(1);
        fgets(x,LLENGTH-1,survoxxx);   /* n_save mc1 mc2 mr1 mr2 c_vasen1 */
        split(x,osa,3);
        n_save=atoi(osa[0]); mc1=atoi(osa[1]); mc2=atoi(osa[2]);

        if (insert_mode)
            {
            ++mr;
            i=insert_lines(mr,n_save);
            if (i<0) { fclose(survoxxx); return(-1); }
            }

        j2=mr;
        for (j1=0; j1<n_save; ++j1)
            {
            if (j2>ed2) break;
            fgets(x,LLENGTH-1,survoxxx);  // x[ed1]=EOS; 30.1.2000
            fgets(xs,LLENGTH-1,survoxxx); // xs[ed1]=EOS;
            lev=mc2-mc1+1; if (lev>c2-mc+1) lev=c2-mc+1;
            memcpy(z+(j2-1)*ed1+mc,x+mc1,lev);
            if (!zs[j2]) { i=creatshad(j2); if (i<0) { fclose(survoxxx); return(1); } }
            memcpy(z+(zs[j2]-1)*ed1+mc,xs+mc1,lev);
            testshad(j2);
            ++j2;
            }
        fclose(survoxxx);
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
/* RS NYI
        if (hae_apu("autotrim",trim_command))
            {
            if (r_alku1==r_alku)
                trim(r_alku,r_loppu);
            else if (r_alku1<r_alku)
                {
                trim(r_alku,r_loppu);
                if (!insert_mode) trim(r_alku1,r_loppu1);
                }
            else
                {
                if (!insert_mode) trim(r_alku1,r_loppu1);
                trim(r_alku,r_loppu);
                }
            }
*/
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
// RS NYI        if (hae_apu("autotrim",trim_command)) trim(r_alku,r_loppu);
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
        int i,j;
        char xs[LLENGTH];
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
        char block[LLENGTH];
        int i;

        m_move_ind=0; // 21.3.2004
        mr=rr; mc=cc;
        mr1=move_r1; mr2=move_r2;
// RS        strcpy(survoblo,etmpd); strcat(survoblo,"SURVO.BLO");
// RS        strcpy(survowrd,etmpd); strcat(survowrd,"SURVO.WRD");
        strcpy(survoblo,"SURVO.BLO"); // RS ei temp-hakemistoa, KORJAA!
        strcpy(survowrd,"SURVO.WRD");  // RS ei temp-hakemistoa, KORJAA!

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
// RS CHA           sprintf(prompt_line,"Mark %s 1 by %s. %s=Cancel%s",sana1,sana4,key_label[CODE_DELETE],sbuf);
            sprintf(prompt_line,"Mark %s 1 by %s. DELETE=Cancel%s",sana1,sana4,sbuf); // RS
            sprintf(sbuf,"%s",prompt_line); sur_print(sbuf);
            move_ind=1;
            break;
          case 1:
            move_r1=r1+r-1; mc1=c1+c-1;
//          sprintf(prompt_line,"Mark %s 2 by %s. %s=Cancel",
//           sana1,key_label[nappi],key_label[CODE_DELETE]);

//RS CHA           sprintf(prompt_line,"Mark %s 2 by %s. %s=Cancel",sana1,sana4,key_label[CODE_DELETE]);
            sprintf(prompt_line,"Mark %s 2 by %s. DELETE=Cancel",sana1,sana4); // RS

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
/* RS CHA
            sprintf(prompt_line,
             "Select place and %s by %s. %s=%s, %s=Cancel",
              sana2,sana4,key_label[CODE_ERASE],sana3,key_label[CODE_DELETE]); */

            sprintf(prompt_line,
             "Select place and %s by %s. CTRL-END=%s, DELETE=Cancel",
              sana2,sana4,sana3); // RS

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
                    char x[LLENGTH], x1[LLENGTH];

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
            *luku=EOS; prompt("Enter key code: ",luku,5);
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
    if (sur_strcmpi(s,"CLOSE")==0) { fclose(time_file); time_file_on=0; return(1); }
    time_file_on=1;
    time_file=fopen(s,"w+t");

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

        if (g>2 && sur_strcmpi(parm[1],"COUNT")==0)
            {
            if (g>3 && sur_strcmpi(parm[2],"FILE")==0)
                {
                open_time_file(parm[3]); return(2);
                }
            ftime(&count);
            timecount2=count.time+0.001*(double)count.millitm;
            if (sur_strcmpi(parm[2],"START")==0)
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
int (*display)(),  /* näyttötoimitus sekuntien välillä */
int katko         /* 1=keskeytys napilla 0=ei keskeytysmahdollisuutta */
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

// RS Ei porttautuvaa koodia, KORJAA!
        a1=a0>>7; a2=a1>>2; /* "jako 100:lla" 1/128+1/512=0.0097.. */

        a0=a1+a2; if (a0==0L) return(1);
        sprintf(sbuf,"TW%ld@",a0);
        tutsave(1);
        for (i=0; i<strlen(sbuf); ++i) tutsave((int)sbuf[i]);
        return(1);
        }


int op_wait()
    {
    double kesto;
//RS    extern int headline();

    if (g<2) { op_incomplete(); return(-1); }
    sur_wait((long)(1000.0*atof(parm[1])),headline,1);
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
        char x[LLENGTH],*px[3];

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
    int i;
    char *p;
    int j;

    FILE *tied; // RS puuttui?!?

    strcpy(x2,etmpd); strcat(x2,"SURVO.TMP");
    tied2=fopen(x2,"wt");
// printf("\nfil=%s x2=%s|",fil,x2); WAIT;
    tied=fopen(fil,"rt");
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
    fclose(tied);
    fclose(tied2);
// RS NYI  KORJAA!    sur_copy_file(x2,fil);
    return(1);
    }

int valitse_kieli(char *s)
        {
        char *p;
        char lang[2];

        *crt_exit=*s;


        strcpy(sbuf,survo_path); strcat(sbuf,"SURVO.APU");
        *lang=*crt_exit; lang[1]=EOS;
        muuta_survo_apu(sbuf,"language",lang);
// RS REM        set_console_title();
        soft_keys_init();

        return(1);
        }

int survo_apu()
        {
        char x[LLENGTH];
        int i;
        char *p;
        int j;

       FILE *tied; // RS puuttui?!?

        tied=fopen(parm[2],"r+t");
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
        fclose(tied);
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
// RS NYI            valitse_kieli(parm[2]);
            return(1);
            }
        if (strcmp(parm[1],"SYSTEM")==0)
            {
// RS NYI            survo_apu();
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



int activate()
        {
        int i;
        char copy[LLENGTH];
        char *p;
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
            }
        else
            edread(actline,r1+r-1);

        p=strchr(actline,(char)PREFIX);
        if (p==NULL) p=actline;

        strcpy(copy,p);
        g=split(p+1,parm,MAXPARM);
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

        if (sur_strcmpi(op,"S")==0) strcpy(op_tila,"SHOW"); // 26.4.2006

        strcpy (OO,op);
        strupr(OO);

        strncpy(op_sana,OO,8); op_sana[8]=EOS;
        strcpy(help_sana,op_sana);
        strcpy(pref,"_");
        goto_load_ind=0;

// if (strlen(OO)>16)
//   { printf("OO=%s\n",OO); getck(); }

        if (strcmp(OO,"GOTO")==0)    { i=op_goto(); goto_load_ind=1; return(i); }

else    if (strcmp(OO,"REDIM")==0)   { op_redim(1); return(1); }
else    if (strcmp(OO,"SAVE")==0)    { op_save();
                                       // RS NYI if (etu==0) sur_wait(100L,nop,1);
                                       return(1);
                                     }

else    if (strcmp(OO,"LOAD")==0)    { op_load(); goto_load_ind=1; return(1); }
else    if (strcmp(OO,"SCRATCH")==0) { op_scratch(); return(1); }
else    if (strcmp(OO,"CD")==0 || 
            strcmp(OO,"PATH")==0 || 
            strcmp(OO,"DISK")==0)    return(op_cd()); // RS CHA   { i=op_path(); return(i); }
            
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


/* RS NYI 
else    if (strcmp(OO,"COPY")==0)    { op_copy(); return(1); }
else    if (strcmp(OO,"PASTE")==0)   { op_paste(1); return(1); } // 6.1.2002
else    if (strcmp(OO,"SHOW")==0)
             { op_show(); return(1); } // 13.4.2006
else    if (strcmp(OO,"CHILD")==0)   { op_child(); return(1); }
else    if (strcmp(OO,"DOS")==0 || *OO=='>')
                { if (strlen(OO)==1) return(1);i=op_dos(); return(i); }
else    if (strcmp(OO,"OUTPUT")==0)  { i=op_output(); return(i); }
else    if (strcmp(OO,"HELP")==0)    { i=help("HELP"); return(1); }
else    if (strcmp(OO,"F")==0)       { i=help("F"); return(1); }
else    if (strcmp(OO,"SETUP")==0)   { i=op_setup(); return(i); }
else    if (strcmp(OO,"SHADOW")==0)   { i=op_shadow(); return(i); }
else    if (strcmp(OO,"SYSTEM")==0)    return(survoapu1(0,NULL));
else    if (strcmp(OO,"CHECK")==0)    return(op_check(1));
else    if (strcmp(OO,"CHECK0")==0)    return(op_check(0));
else    if (strcmp(OO,"COLX")==0)    return(op_colx());
else    if (strcmp(OO,"FIND")==0 || strcmp(OO,"REPLACE")==0 ||
            strcmp(OO,"-FIND")==0 || strcmp(OO,"-REPLACE")==0) return(op_find());
else    if (strcmp(OO,"EXT_FUNC")==0) return(op_ext_func());
else    if (strcmp(OO,"D32")==0)    return(op_d32());
else    if (strcmp(OO,"SOFTKEYS")==0)    return(op_softkeys()); // 15.3.2000
else    if (strcmp(OO,"SESSION")==0)    return(op_session()); // 7.4.2000
else    if (strcmp(OO,"WIN")==0)    return(op_win());
else    if (strcmp(OO,"NEXTFILE")==0)    return(op_nextfile()); // 13.9.2000
else    if (strcmp(OO,"COLOR")==0)    return(op_color()); // 30.12.2000
else    if (strcmp(OO,"TEXTCOLS")==0)    return(op_textcols()); // 17.3.2001
else    if (strcmp(OO,"TEMPDISK")==0)    return(op_tempdisk()); // 27.4.2004
else    if (strcmp(OO,"PUTENV")==0) { op_putenv(); return(1); } // 24.3.2005
else    if (strcmp(OO,"FILES")==0) { op_files(); return(1); } // 9.6.2005
else    if (strcmp(OO,"INSERTL")==0) { op_insertl(); return(1); } // 10.6.2006
else    if (strcmp(OO,"DELETEL")==0) { op_deletel(); return(1); } // 18.6.2006
else    if (strcmp(OO,"LINEINS")==0) { op_lineins(); return(1); } // 10.6.2006
else    if (strcmp(OO,"LOWLINE")==0) { op_lowline(); return(1); } // 25.6.2006
else    if (strcmp(OO,"FILETIME")==0) { op_filetime(); return(1); } // 30.7.2008
else    if (strcmp(OO,"FONT")==0 || strcmp(OO,"WINDOW")==0)
            { op_font(); return(1); } // 11.7.2006

else    if (strcmp(OO,"SYS")==0 || strcmp(OO,"SYSDEL")==0)
            { muuta_apu_tiedostoa(3); return(1); } // 14.7.2006
else    if (strcmp(OO,"FLUSH")==0)
            {
            extern int only_key_events;

            sur_flush_input();
            only_key_events=0;
            return(1);
            }

else    if (strcmp(OO,"MATRUN")==0) op[3]=EOS; //  -> MAT

//       "lapsenlapsen" välttämiseksi 29.2.2000 
else    if (g>2 && strcmp(OO,"MAT")==0 && strcmpi(parm[1],"SAVE")==0
            && strcmpi(parm[2],"DATA")==0)
            { strcpy(op,"MATSDA"); strcpy(pref,"&"); }

else    if (strchr(OO,'?')==NULL &&
             (strncmp(OO,"TUTS",4)==0 || strncmp(OO,"TUTL",4)==0 ||
              strncmp(OO,"TUTD",4)==0 || strncmp(OO,"TUTI",4)==0 ) )
            { strcpy(op,"TUT"); strcpy(pref,"&"); }

else    if (strncmp(OO,"TCH",3)==0)
            { strcpy(op,"T"); strcpy(pref,"&"); strcpy(info,"TOUCH"); }

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

else    if (strcmp(OO,"FILE")==0 || strcmp(OO,"F")==0)
            { 
              i=op_file(op); 
// RS NYI              if (i==1) childp("FI\\");
              soft_disp(1); 
              return(1); 
            }

else    if ((*OO=='C' || *OO=='L') && strchr(OO,'?')==NULL &&
             strchr("+-*/%",OO[1])!=NULL &&
             strchr(OO,'=')==NULL )
                           { strcpy(op,"EDI2"); strcpy(pref,"&"); }

else    if (!soft_act2 && copy[c1+c-2]=='=')
               { op_arit(); return(1); }

/* RS NYI
else    if (strcmp(OO,"LIST")==0)
             {
             i=op_list(op);
             if (i==1) childp("L\\"); return(1);
             }

else    if (strcmp(OO,"MASK")==0 || strncmp(OO,"MASK=",5)==0)
             { file_act("MASK"); return(1); }


else    if (strchr(OO,'?')!=NULL && strnicmp(OO,"http://",7)!=0)
            {
            strcpy(info,">"); strcat(info,qpath);
            help2(); return(1);
            }
*/
else    if (*OO=='/') return(op_tutor());

        if (strcmp(OO,"INIT")==0)
            {
            sprintf(info,"%s %d",ser_number,ver); // 10.8.2003
            }

// RS NYI        i=survoexe_sys(OO,op2);
        if (i)
            {
            op=op2;
            *pref=EOS;
            }
        if (strchr(op,':')!=NULL || strchr(op,'\\')!=NULL)
            {
// RS NYI            i=childp(""); return(i);
            }
// RS NYI        i=childp(pref); return(i);
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
        int i;

        p=wordp(k);
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
        int i,k;

        k=nextch();
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
    int i,j,k;
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

        m=nextch()-'0'-1;
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

/* printf("\ni1=%d i2=%d c-i1=%d",i1,i2,c-i1); getch(); */
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
            m2=nextch(); if (m2==CODE_RETURN) break;
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
        int m,m2;
        int i;
        char x[LLENGTH];
        char msana[3];
        char *p,*q;

        m=nextch("");
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
// RS NYI                i=getch(); if (i=='\\') strcpy(id_jatko,"  "); break;
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
// RS NYI                help("???"); disp(); break;
              case CODE_SRCH:   /* 25.7.1998 */
// RS NYI                strcpy(info,"F-"); op_find();
                disp(); break;
              case CODE_ACTIV:
                *active_data=EOS;
// RS NYI                file_act("KEY_ACTIV"); disp();
                break;
              case CODE_SOFT_ON:
                soft_vis=0;
                soft_disp(soft_vis);
                break;
              case CODE_DISK:
                strcpy(x,survo_path);
                i=strlen(x)-2; x[i]=EOS;
                for (m2=0; m2<i; ++m2)
                    key_common((int)(unsigned char)x[m2]);
                break;

  /*      default: printf("\nPRE: %d %d\n",special,m); getch(); */
                }
            }
        else /* not special */
            {
            switch(m)
                {
/* In use:
! # £ % & / 0 = @ A a B b C c D d E e F f g h I i J j k L l M m N n o P p
Q q R r S s T t U u v W w x X y ä Ä ö ^ _ ~ > < \
*/
              case 'T': case 't': tut_special(); 
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

              case 'L':           *x=(char)nextch("");
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
                                      *x=(char)nextch();
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

              case 'M': case 'm': x[0]=nextch(); x[1]=EOS; sucro_macro(x,0);
                                      break;
              case 'N': case 'n': x[0]='#'; x[1]=nextch(); x[2]=EOS;
                                      sucro_macro(x,0); break;
// 28.9.2005
              case '$':           x[0]='$'; x[1]=nextch(); x[2]=EOS;
                                      sucro_macro(x,0); break;

              case '!': case ';': enter_sana(x);
                                  if (m=='!') m2=0; else m2=1;
                                  sucro_macro(x,m2); break;


              case 'D':           ref_c1=0; break;
              case 'J': case 'j': pre_j(); disp(); break;
              case 'Q':           etuu=1; break;
              case 'q':           etuu=0; break;
              case 'd':           tutcat(edisk); break;
              case 'g':           tutcat(esysd); break;
              case 'l':           m2=nextch(); 
                               // RS NYI KORJAA!  label(m2,x);
                                  sprintf(x,"F-%d)",m2); // RS Väliaikaisratkaisu
                                  tutcat(x); break;
              case 'X':           edread(x,r1+r-1);
                                  i=strlen(x)-1;
                                  while (i>c1+c-2 && x[i]==' ') x[i--]=EOS;
                                  tutcat(x+c1+c-1);
                                  break;
              case 'B':           c1=c=1; disp(); break;
              case 'A':           ins_lines_on=1; break;
              case 'a':           ins_lines_on=0; break;

              case 'i':           m2=nextch(); tut_index=m2-'0'; break;
              case 'b':           init_param1(); disp(); break;
              case 'u':           prompt_shadow=nextch(); break;
              case 'o':           hae_edisk(x); tutcat(x); break;
              case 'H':           strcpy(help_sana,"HELP"); break;
              case 'h':           tutcat(etmpd); break;
              case '&':           enter_wsana(x);   // 29.5.1992 
                                  edt_talletus(x); break;
              case '%':           enter_sana(x); *sbuf=EOS;
                                  i=hae_apu(x,sbuf);
                                  tutcat(sbuf); break;
              case '^':           tutcat(survo_path); break;
              case '/':           sek_aika(1); break;
              case '_':           sur_alarm=0; break;  // 10.8.1992 
              case '~':           x[0]=survo_type; x[1]=EOS; tutcat(x);
                                  break;
              case 156:           x[0]=*crt_exit; x[1]=EOS;   // RS  punta
                                  tutcat(x); break;
              case 132:           tutcat(orig_setup); break; // 23.1.93   // RS pikku ä
              case 142:           tutcat(current_setup); break;           // RS iso Ä
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
                                      sur_wait(1000L,Wdisp,1);
                                      }
                                  break;
// 26.12.2000
              case 'k':           tutcat(os_ver); break;

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

int key_special(int m)
                {
                int i,k;
                char x[LLENGTH];
// RS                extern char *p_soft_key_text;

                vnumtab=numtab;
                numtab=0;
                switch (m)
                  {
                  case CODE_EXIT:
                    if (display_off) { restore_display(1); break; }
                    i=lopetuskysely();
                    if (i)
                        {
/* RS NYI
                        gplot_poisto(); // 18.1.20001 jos close_graphs=1
                        remove_current_session();
                        if (help_window_open)
                            {
                            sur_set_message("-",1);
                            }

                        if (tmp_by_session && del_tmp) // 26.3.2004
                            {
                            sur_sleep(200L);
                            sprintf(sbuf,"%s*.*",etmpd);
                            delete_files(sbuf);
                            sur_remove_dir(etmpd);
                            }

                        exit(0);
*/
                        return(-1);  // RS exit

                        }
                    disp(); break;
                  case CODE_RIGHT:
                    move_right2(1);
                    break;
                  case CODE_BACKSP:
                    if (kontr_()) break;
                    if (c>1) { --c; z[ed1*(r1+r-2)+c+c1-1]=' ';
                               displine2(r1+r-1); break; }
                  case CODE_LEFT:
                    if (c>1) { --c; break; }
                    if (c1>1) { --c1; disp(); break; }
                    if (prevkey==CODE_LEFT)
                        {
                        if (nleft>=2) { c=0; break; }
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
                    muste_erase(); break;
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
                    headline();
// RS REM                    cursor_on();
                    break;
                  case CODE_PRE:
                    pref=(unsigned char)PREFIX;  /* 8.3.2000 */
                    prefix(); break;
                  case CODE_TOUCH:
/* RS NYI  Touch moodi puuttuu vielä
                    PR_ENRM;
                    i=soft_vis;
                    soft_vis=0;
                    p_soft_key_text=NULL;
                    soft_disp(0); // 22.10.2001
                    if (rsh) disp_shadow();  // 27.12.2004
                    strcpy(ops,"T"); op=ops;
                    childp("&");
                    soft_vis=i;
                    disp();
*/
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
                    CURSOR_OFF; headline(); cursor_on();
*/
/* RS Hakemistonvaihtodialogi */
                    sprintf(sbuf,"CD *");
                    g=split(sbuf,parm,MAXPARM);                    
                    muste_setwd();       // RS  Hakemistonvaihtodialogi
                    headline(); 

                    break;
                  case CODE_CODE:
                    code_code();
                    break;
                  case CODE_REF:
                    if (ref_c1==0) { ref_c1=c1; ref_c=c;
                                     ref_r1=r1; ref_r=r; break;
                                   }
                    if (ref_c1+ref_c==c1+c && ref_r1+ref_r==r1+r ||
                        ref_r1+ref_r-1>r2 || ref_c1+ref_c-1>c2)
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
                    line_copy(); 
// RS turha???              sur_sleep(10); // nopeat sukrot!! 1.8.2000
                    soft_vis=1; disp_all(); break;
                  case CODE_TAB:
                    next_tab(); 
                    break;
                  case CODE_HELP:
/* RS NYI  HELP ja "F1-Kausi" puuttuvat toistaiseksi
                    pref=(unsigned char)PREFIX;  // PREFIX2?
                    prefix2(); 
*/
                  break;
                  case CODE_ACTIV:
/* RS NYI  Kutsuu lapsiprosessina CREATEa FI-alihakemistossa; puuttuu toistaiseksi
                    file_act("KEY_ACTIV");
                    p_soft_key_text=NULL;
                    disp();
*/
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
/* RS NYI
                    strcpy(info,"F"); op_find();
                    disp(); // 21.9.92
                    soft_disp(1);
*/
                    break;

                  case CODE_SOFT_ON: // 8.2.2001
                    restore_softkeys(); // 1.5.2002
                    break;

/* RS NYI
                  case 151: copy_to_clipboard(); break; // 24.4.2006
                  case 153: op_paste(2); disp(); break;  // 24.4.2006
*/

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
                       else ++c;
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

        sapu[MAXTILA]=254;   // (unsigned char)'_';   /* 26.3.1992 */
        sapu[MAXTILA+1]=254; // (unsigned char)'_';   /* 10.10.1994 */
        *sapu=EOS; p=sapu;
// RS   add_survo_path(afile,apufile);
        strcpy(afile,apufile); // RS no file path, just the name

        apu0=fopen(afile,"rt");
        if (apu0==NULL)
            {
            warning("\nFile %s missing!",afile); // RS printf -> warning
// RS            getch(); 
            return(-1);
            }

        while (1)
            {
            merkki=fgetc(apu0);
            if (merkki=='\n' || merkki=='\r') merkki=fgetc(apu0); // RS unix fix
            if (merkki==EOF) break;
            if (merkki=='/')
                {
                merkki=fgetc(apu0);
                if (merkki=='\r') merkki='\n'; // RS unix fix
                if (merkki==' ')
                    {
                    while (*(p-1)==' ') --p;
                    while (1)
                        {
                        merkki=fgetc(apu0);
                        if (merkki=='\n' || merkki==EOF || merkki=='\r') break;
                                                          // RS unix fix
                        }
                    if (merkki==EOF) break;
                    }
                else { *p='/'; ++p; }
                }
            *p=merkki; ++p;
            if (p-sapu>=MAXTILA)
                {
                warning("\nFile SURVO.APU is too large!"); // RS printf->warning
// RS                WAIT;
                return(-1);
                }
            }
        *p=EOS;
/* printf("\np-sapu=%d %d",p-sapu,MAXTILA); getch();  */
        fclose(apu0);
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
        i=field_init(); if (i<0) exit(1);
        return(1);
        }

void hae_edisk(char *s)
        {
        int i;
        i=hae_apu("edisk",s);
        if (!i)
          {
          strcpy(s,survo_path); 
          // RS poistettu: strcat(s,"D\\"); /* 6.1.93 */
          }   
        }

static int muste_editor_init(char *apufile,int tunnus)
        {
        int i;
        char sana[128];
        char *osa[4]; /* 10.3.1995 */
        char *p;

        for (i=0; i<LLENGTH; ++i) { space[i]=' ', stripe[i]=STRIPE; }
        space[LLENGTH]=EOS; /* 3.3.91 char space[LLENGTH+1] */
        shadow_code=shadow_code_tila;
        *op_sana=EOS;

        i=init_sapu(apufile); if (i<0) return(-1);

// RS NYI        i=labels(); if (i<0) return(-1);   /* ->!CLEAR.C sis. shadow_codes */

// RS NYI        p_survo_id=NULL;  /* childp() muuttaa! */

/* RS NYI
        i=hae_apu("computation_speed",sana);
        if (i) computation_speed=atoi(sana);
        else computation_speed=mittaa_nopeus();
*/

        PR_ENRM;

        ed1=ED1; ed2=ED2; edshad=ED3;
        r3=ER3; c3=EC3;
// RS        p_survo_id=survo_id;
        prompt_line=NULL;

        i=hae_apu("ed1",sana); if (i) ed1=atoi(sana);
        i=hae_apu("ed2",sana); if (i) ed2=atoi(sana);
        i=hae_apu("ed3",sana); if (i) edshad=atoi(sana);
        i=hae_apu("er3",sana); if (i) r3=atoi(sana);
        i=hae_apu("ec3",sana); if (i) c3=atoi(sana);

        r2=ed2; c2=ed1-1;


        i=field_init(); if (i<0) return(-1);


// RS NYI        strcpy(last_disk,"Y:");
        esysd[0]=*survo_path; esysd[1]=':'; esysd[2]=EOS;  // 9.10.91
// RS NYI        if (netd(survo_path)) *esysd=EOS; // 16.2.2006

        strcpy(eout,survo_path); strcat(eout,"RESULTS"); // RS muutos: strcat(eout,"TMP\\RESULTS");
        strcpy(sbuf,survo_path); i=strlen(sbuf); // 6.3.1999 
        sbuf[i-2]=EOS; // formally <Survo> 
// RS NYI        strcpy(qpath,sbuf); strcat(qpath,"Q\\EDQ");
        strcpy(etmpd,sbuf); // RS NYI strcat(etmpd,"TMP\\"); // 26.10.1996 
        hae_edisk(edisk);   // 25.3.1991, 6.1.93 

/* RS NYI
        hae_apu("tempdisk",etmpd); subst_survo_path_in_editor(etmpd);
        hae_apu("eout",eout); subst_survo_path_in_editor(eout);
        hae_apu("last_disk",last_disk);
        hae_apu("qpath",qpath); subst_survo_path_in_editor(qpath);
*/
        speclist=SPECLIST;
// RS        i=hae_apu("speclist",sana); if (i) speclist=atoi(sana);

        specmax=SPECMAX;
// RS        i=hae_apu("specmax",sana); if (i) specmax=atoi(sana);
        scale_check=0; /* no scale type checks */
// RS        i=hae_apu("scale_check",sana); if (i) scale_check=atoi(sana);
        accuracy=7; /* # of significant digits in free format results etc. */
// RS        i=hae_apu("accuracy",sana); if (i) accuracy=atoi(sana);

        if (accuracy<4) accuracy=4;
        if (accuracy>16) accuracy=16;
        results=70; /* printout level */
// RS        i=hae_apu("results",sana); if (i) results=atoi(sana);
        if (results<0) results=0;     // RS accuracy -> results
        if (results>100) results=100; // RS accuracy -> results
// RS        g=2; parm[1]=edisk; op_cd();

        insert_type=1; /* 1=automatic insert, when INSERT pressed */
// RS        i=hae_apu("insert_type",sana); if (i) insert_type=atoi(sana);
        ins_lines_on=1; /* 1=new_lines_automatically_in_insert */
// RS        i=hae_apu("insert_lines",sana); if (i) ins_lines_on=atoi(sana);

/* RS NYI
        disp_wait=1;
        i=hae_apu("disp_wait",sana);
        if (i) { disp_wait=atoi(sana); if (disp_wait<1) disp_wait=1; }
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
        hae_apu("gplot_layout",gplot_layout);
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
*/
/* RS NYI
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
*/

/* ES NYI
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
// RS     soft_vis=1;


// printf("\ninfo_s=%s|",info_s);


/* RS NYI
        language=crt_exit; // 1.6.2001

        *language=EOS; strncat(language,space,31); language[30]='X';
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

        if (!tmp_by_session) sys_save_restore(1); // 26.2.2001
*/
        return(1);
        }

void s_perusinit() // RS
{
    edisk=s_edisk;
    esysd=s_esysd;
    eout=s_eout;
    etufile=s_etufile;
    sapu=s_sapu;
    info=s_info;
    active_data=s_active_data;
    shadow_int=s_shadow_int;
    shadow_code=s_shadow_code;
    tut_info=s_tut_info;
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
}

void muste_perusinit() {

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
// etuu=1; /* 1=lapset eivät TUT-moodissa 0=lapset TUT-moodissa */
act_sounds_on=0; // 0=ei käytössä, 1=off, 2=on   14.10.2005
move_from_store=0;
wait_tut_type=0; // 1=cancelled by user's actions 2=activates always
}



int muste_editor()  // RS oli parametrit: int argc; char *argv[];
        {

        unsigned int i,j;
        char x[LLENGTH], x1[LLENGTH];
        int m=0;
        int k;
        char *p;

        s_perusinit(); // RS
        muste_perusinit(); // RS

//RS  turha?!?        extern int nop();

        strcpy(survo_path,muste_getwd());  // RS current dir for survo_path


/* RS NYI - not yet implemented
        sek_aika(0); // aloitusaika (spre.c)
        strcpy(survo_path,argv[0]);

        sur_get_short_name(survo_path);
        i=strlen(survo_path)-1;
        while (survo_path[i]!='\\' && survo_path[i]!=':') survo_path[i--]=EOS;

        strcpy(survo_path16,survo_path);
        i=strlen(survo_path16)-3;
        if (strcmpi(survo_path16+i,"\\U\\")==0 )
            survo_path16[i+1]=EOS;

        if (*orig_setup==EOS)
            {
            strcpy(orig_setup,survo_path); strcat(orig_setup,"SURVO.APU");
            }

        if (argc>1 && strnicmp(argv[1],"/S:",3)==0)
            {
            strcpy(orig_setup,argv[1]+3); survoapu=0;
            --argc; argv[1]=argv[2];
            }

        strcpy(current_setup,orig_setup);

        alkututor=argc-1;
*/

        edrun=1;
// RS   k=init(orig_setup,1); if (k<0) return;
        k=muste_editor_init("SURVO.APU",1); if (k<0) return(-1);

// RS NYI    g=2; parm[1]=survo_path; op_cd();

//RS    sur_console_init(system_name);
//        muste_console_init("Muste");  // RS

//RS NYI        g=1; op_cd();

//RS NYI        strcpy(ediskpath,edisk);


/* RS REM Järjestelmätarkistukset voinee jättää pois
        sur_os_version(os_ver);
        sur_ctrl_handling();
// printf("\nos_ver=%s",os_ver);
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

/* RS NYI
        soft_keys_init();
        set_sur_session();
        if (r_soft)
         {
         muste_resize(c3+8,r3+2+r_soft+1); // RS CHA sur_resize1(c3+8,r3+2+r_soft+1);
         }

        set_console_title();
        kielenvalinta();



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
// printf("\nsurvo_path=%s| esysd=%s|",survo_path,esysd); getch();

*/

// RS        set_window();

        disp();  // RS

        while (edrun)
            {

//Rprintf("edrun loop\n");

            cursor(r,c);
/*
            if (alkututor)
                {
                g=2; parm[1]=argv[1];

                if (parm[1]!=NULL) // 21.11.2002
                    {
                    while (1)
                        {
                        p=strchr(parm[g-1],'|');
                        if (p==NULL) break;
                        *p=EOS; parm[g++]=p+1;
                        }
                    }
// printf("\nalkututor=%d parm=%s|",alkututor,parm[1]); getck();

                if (alkututor==-1) parm[1]=x1;  //  XXXX 14.7.92 
                else strcpy(edisk,esysd);
                op_tutor();
                alkututor=0;
                }

*/
            prevkey=m;
                {
                special=FALSE;
                m=nextch(); if (m==-1) continue; /* 13.4.1996 */


                scroll_line=r+2; if (scroll_line>r3) scroll_line=r3;  // RS Needs to be known!


                if (m==-7)
                    {
                    wait_tut_type=0;
                    parm[0]=wait_tut_name;
                    op_tutor();
                    continue;
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
            }
        return(0);
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

char *strupr(char *str)
{
    char *string = str;

    if (str)
    {
        for ( ; *str; ++str)
            *str = toupper(*str);
    }
    return string;
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
        lin1=lin-atoi(sana+1);
        if (lin1<1) lin1=1;
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

    strcpy(sana,s);
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
        int i,i0,k;
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

        if (sur_strnicmp(p,"ALARM=",6)==0) sur_alarm=0;

        if (!h) edread(x,r1+r-1); else strcpy(x,s);

        q=sapu;
        while ( ((unsigned char)*q!=(unsigned char)'_' ||
                 (unsigned char)*(q+1)!=(unsigned char)'_') &&
                 (int)(q-sapu)<16000 ) ++q;

// RS REM        if ((int)(q-sapu)>=16000) { printf("???"); getck(); }
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
char sys_str1[32];

int sys_save_restore(int k) // 1=SAVE 2=RESTORE
    {
//RS    extern char *parm[];
//RS    extern char sapu[];
    int rr3,cc3;
    int i;

    strcpy(sbuf,etmpd); strcat(sbuf,"SUR_SYS.SYS");
    if (k==1)
        {
        temp_apu=fopen(sbuf,"wt");
        fprintf(temp_apu,"%s\n",videomode);
        fprintf(temp_apu,"%d\n",accuracy);
        fprintf(temp_apu,"%d\n",search_caps);
        fprintf(temp_apu,"%d\n",help_window);
        strcpy(sbuf,gplot_layout);
        if (*sbuf==EOS) strcpy(sbuf,"-");
        fprintf(temp_apu,"%s\n",sbuf);
        strcpy(sbuf,eout); if (*sbuf==EOS) strcpy(sbuf,"-");
        fprintf(temp_apu,"%s\n",sbuf);
        fprintf(temp_apu,"%d %d\n",r3,c3);
        i=hae_apu("color98",sbuf); if (i) i=atoi(sbuf); else i=0;
        fprintf(temp_apu,"%d\n",i);
        }
     else
        {
        temp_apu=fopen(sbuf,"rt");
        if (temp_apu==NULL) return(1);
        fscanf(temp_apu,"%s\n",videomode);
        sprintf(sbuf,"SYSTEM videomode=%s",videomode);
        survoapu1(1,sbuf);
        fscanf(temp_apu,"%d\n",&accuracy);
        fscanf(temp_apu,"%d\n",&search_caps);
        fscanf(temp_apu,"%d\n",&help_window);
        fscanf(temp_apu,"%s\n",gplot_layout);
        if (*gplot_layout=='-') *gplot_layout=EOS;
        fscanf(temp_apu,"%s\n",sbuf);
        if (strcmp(sbuf,"-")==0) *sbuf=EOS;
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
        }

    fclose(temp_apu);

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

int sur_strcmpi(const char *s1, const char *s2)
{
    for (; *s1 && *s2 && (toupper((unsigned char)*s1) ==
                          toupper((unsigned char)*s2)); ++s1, ++s2);

    return *s1 - *s2;
}


int sur_strnicmp(const char *s1, const char *s2, int count)
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
    int i;
    char *p;

    i=strlen(name);
    if (i<4) p=name;
    else p=name+i-4;
    if (strchr(p,'.')==NULL)
        strcat(name,ext);
    return(1);
}

void edt_talletus(char *s)
{
    char snimi[LLENGTH];

    strcpy(snimi,s);
    /*       if (strchr(s,':')==NULL && strchr(s,'\\')==NULL)   */ /* 7.1.1992 */
    /*            { strcpy(snimi,etmpd); strcat(snimi,s); }
    */
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

int sur_dump(char *siirtop)
{
    int i,h;
    char x[LNAME];

    results=v_results;
    accuracy=v_accuracy;

    autosave32=1;  // RS Autosave as in editor
    strcpy(x,siirtop);
    strcat(x,"SURVOMM.EDT");
    edt_talletus(x);
    autosave32=0;


    strcpy(x,siirtop);
    strcat(x,"SURVOMM.DMP");
    apu=fopen(x,"wt");

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
    fclose(apu);
    return(1);
}


int restore_dump(char *siirtop)
{
    char x[LNAME];
    int i,h;
    char *p;
    /*        long li;
            int k;  */

    strcpy(x,siirtop);
    strcat(x,"SURVOMM.DMP");
    apu=fopen(x,"rt");
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
    fclose(apu);

    strcpy(x,siirtop);
    strcat(x,"SURVOMM.EDT");
    edload(x,1);




    /*      disp();
          g=2; parm[1]=edisk; op_cd(); */

    v_results=results;
    v_accuracy=accuracy;

    return(1);
}

int s_edt(char *siirtop)
{
    restore_dump(siirtop);
    return(1);
}

int s_end(char *siirtop)
{
    redim_save=1; // RS
    ued1=ed1; ued2=ed2; uedshad=edshad; // RS
    sur_dump(siirtop);
    redim_save=0; // RS
    return(1);
}



int s_init(char *siirtop)
{
    int i;
    char *p;

    strcpy(sur_session,siirtop);
    *sur_session=siirtop[strlen(siirtop)-1];
    sur_session[1]=EOS;

    s_perusinit();  // RS

    strcpy(etmpd,siirtop); /* tilap. */

    s_edt(siirtop);

    for (i=0; i<LLENGTH; ++i) space[i]=' ';
    space[LLENGTH-1]=EOS;
    edread(comline,(unsigned int)(r1+r-1));
    p=strchr(comline,PREFIX);
    if (p==NULL) p=comline;
    g=split(p+1,word,MAXPARM);
    i=0;

    while (i<g && strcmp(word[i],"/")!=0) ++i;
    g=i;

    /*        if (console) sur_console_child_init(); */
    return(1);
}

int sp_check()
{
    int j;
    unsigned int n,tila,tila0;
    int varjo;
    char x[LLENGTH];
    char *p,*q;
 
    if (*z==' ')     /* 22.6.1998 */                            /* No check if empty control char at first line */
    {
        specmax=10000;
        speclist=1000000;
        /*
                    if (hae_apu("specmax",x)) specmax=atoi(x);
                    else specmax=10000;
                    if (hae_apu("speclist",x)) speclist=atoi(x);
                    else speclist=1000000;
        */
        /*   printf("specmax=%d speclist=%d\n",specmax,speclist); getch();  */
        return(1);
    }

    n=0;
    tila=0;
    for (j=1; j<=r2; ++j)                                       /* Loop through the edit field lines          */
    {
        edread(x,(unsigned int) j);
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
            /* printf("C: j=%d pos=%d\n",j,p-x); getch(); */
            ++n;                                               /* Increase the number of found specifications */
            tila0=tila;
            varjo=zs[j];
            tila+=2;                                           /* Space for two EOS                           */
            q=p-1;
            while (*q && *q!=' ')
            {
                ++tila;                                        /* Scan left until space or EOS                */
                --q;
            }
            q=p+1;
            while (*q && *q!=' ')
            {
                ++tila;                                        /* Scan right until space or EOS               */
                ++q;
            }

            while (*(q-1)=='&')                                /* Expression splitted to several lines        */
            {
                tila+=c2;                                      /* Allow some extra space (bug fix?)           */
                ++j;
                if (j>r2) break;                               /* Go to next line and check if last           */
                edread(x,(unsigned int) j);
                q=x+1;
                while (*q && *q==' ') ++q;                     /* Skip spaces in the beginning of line        */
                if (*q==EOS) break;                            /* Next line if at the end of line             */
                while (*q && *q!=' ')
                {
                    ++tila;                                    /* Scan right until space or EOS               */
                    ++q;
                }
            }
            if (varjo) tila+=tila-tila0;                       /* Double space needed if shadows in use       */
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


int sur_instr(char s[],char c[])
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
        /* printf("j=%d pos=%d\n",j,p-x); getch(); */
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
    char raja[12];
    int j,i;
    char x[LLENGTH];

    strcpy(raja,"*..........");
    for (j=lin-1; j>0; --j)             /* Scan upwards until borderline or top of edit field */
    {
        edread(x,(unsigned int)j);
        i=sur_instr(x,raja);
        if (i>=0) break;
    }
    *raja1=j;
    for (j=*raja1+1; j<=ed2; ++j)      /* Read the specifications */
    {
        edread(x,(unsigned int)j);
        if (global==1)                 /* Zero global if found */
        {
            i=sur_instr(x,"*GLOBAL*");
            if (i>0) global=0;
        }
        i=sur_instr(x,raja);
        if (i>=0) break;
        if (j==r1+r-1) continue;       /* Skip the activated line */
        spn=spread3(x,j);
        if (spn<0) return(spn);
    }

    /*  printf("\n"); for (i=0; i<spn; ++i) printf("\n%s=%s %c %u varjo=%s",
                           spa[i],spb[i],spp[i],spplace[i],spshad[i]); getch();
    */
    return (spn);
}


int sp_init_extra(int lin,int extra_bytes,int extra_specs)
        {
/*        int tila;
        char *p;
*/
        int spn1;
        int raja1;
        char x[LLENGTH];

  /* Check the number of specifications and needed space */
        sp_check();
        speclist+=extra_bytes+4;
        specmax+=extra_specs+3;   /* +1 @r 4.10.1999 */

    /* Allocate memory for specifications */

    splist=malloc((unsigned int)speclist);
    if (splist==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spa=(char **)malloc(specmax*sizeof(char *));
    if (spa==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spb=(char **)malloc(specmax*sizeof(char *));
    if (spb==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spshad=(char **)malloc(specmax*sizeof(char *));
    if (spshad==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    arvo=(double *)malloc(specmax*sizeof(double));
    if (arvo==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }

/* Extra allocation for editorial arithmetics */
    spp=malloc((unsigned int)specmax);
    if (spp==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spplace=(unsigned int *)malloc(specmax*sizeof(unsigned int));
    if (spplace==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
/* Siirretty arit.c:hen
    i=varaa_earg();
    if (i<0) return(-1); */

    spn=0;
    spl=splist;
    global=0;
    edread(x,(unsigned int)lin);
    spn=spread3(x,lin);		/* Specifications from the current line */
    if (spn<0)
    {
        spxxxx();
        return(spn);
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

int sp_init(int lin)
    {
    int i;

    i=sp_init_extra(lin,60,10);

/* Väärinkirjoitettujen spesifikaatioiden arvailua
if (i>=0 && spec_check) i=spec_word_dist(spec_check);  
*/
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
        int i;
/* RS        char *p; */
        int raja1;
        char x[LLENGTH];
        int lin;

        lin=r1+r-1;
        edread(x,lin);
        i=spec_read3(s,t,x,lin,len); if (i>=0) return(1);
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

        m=nextch();
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

        m=nextch();
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

