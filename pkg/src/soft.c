#include <R.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "survo.h"
#include "survolib.h"

#define SOFTLEN 81
#define SOFTLINES 10
#define SOFTKEYS 200
#define MENUS 100
/************************************************************
SL=81 LI=10 SK=200
2*SL*LI+2*SL*SK+4*5*SK+2*SK=38420


*************************************************************/

extern char edisk[];
extern char *language;

extern int g,r,c,r1,c1,r3,c3;
extern char *parm[];
extern char sbuf[];
extern char space[];
extern char survo_path[];
extern char *language;
extern char tut_info[];
extern int soft_key_activated;
extern char system_name[];
extern int etu;
extern int sdisp;
extern char etmpd[];
extern int r_mouse,c_mouse;
extern int display_off;
extern char window_name[];
extern int soft_vis;
extern char os_ver[];


int r_soft=0;
int r_soft_old=0;
int soft_multiple=1;
int soft_multiple2=1;
char soft_line[SOFTLINES][SOFTLEN];
char soft_shad[SOFTLINES][SOFTLEN];

char soft_key_text[SOFTKEYS][SOFTLEN];
char soft_key_command[SOFTKEYS][SOFTLEN];
int  soft_key_line[SOFTKEYS];
int  soft_key_start[SOFTKEYS];
int  soft_key_end[SOFTKEYS];
char soft_key_act[SOFTKEYS];
char soft_key_text_shadow[SOFTKEYS];
int  soft_key_state_n[SOFTKEYS];
int  soft_key_state[SOFTKEYS];
int n_soft_keys;
int soft_message=0;

int soft_menu_n;
char soft_field_name[MENUS][LNAME];
char soft_list_name[MENUS][16];

char soft_prev_field[LNAME];  // kts. op_softkeys()
char soft_prev_list[16];

int soft_code;
char soft_char;
char *p_soft_key_text;
char soft_current_key_text_shadow;

int header_line_ind;

extern int soft_act; // RS EDITOR
char soft_actline[LLENGTH];

char soft_info[LLENGTH];

char soft_stack_file[LNAME];

char soft_edit_file[LNAME]; // 1.6.2001
static char control_char;

FILE *stemp;
FILE *soft_temp_file;

int soft_stack_save_load(int k,char *name)
    {
    char x[LNAME];
    char *p;

    strcpy(x,edisk); strcat(x,name);

    if (k==1)
        {
        soft_temp_file=fopen(x,"wb");
        if (soft_temp_file==NULL) { tutstack_error(x,1); return(-1); }
        p=soft_info;
        while (*p) { putc((int)(*p),soft_temp_file); ++p; }
        fclose(soft_temp_file);
        return(1);
        }

    soft_temp_file=fopen(x,"rb");
    if (soft_temp_file==NULL) { *soft_info=EOS; return(1); }
    p=soft_info;
    while (!feof(soft_temp_file)) { *p=(char)getc(soft_temp_file); ++p; }
    *(p-1)=EOS;
    fclose(soft_temp_file);
    return(1);
    }

int read_from_soft_stack(char *nro,char *s)
        {
        int i,h;
        char *p,*q;
        h=atoi(nro)-1;
        i=0; p=soft_info;
        while (i<h)
            {
            if (*p==EOS) { *s=EOS; return(1); }
            while (*p!='@') ++p;
            ++i;
            ++p;
            }
        q=strchr(p,'@');
        if (q==NULL) { *s=EOS; return(1); }
        *q=EOS; strcpy(s,p); *q='@';
        return(1);
        }


int soft_stack_set(char *s,int k)
    {
    char x[LLENGTH];
    int i,h;
    char *p,*q;

    strcpy(x,soft_info);
    p=x; i=0; q=soft_info; h=0;
    if (k>1) while (*p)
        {
        q[h++]=*p;
        if (*p=='@')
            {
            ++i; if (i==k-1) { ++p; break; }
            }
        ++p;
        }
    q[h]=EOS;
    for (;i<k-1; ++i) strcat(q,"@");
    strcat(q,s); strcat(q,"@");
    while (*p && *p!='@') ++p;
    if (*p=='@') ++p;
    strcat(q,p);
    return(1);
    }


int soft_keys_init()
    {
    int i;
    char x[LLENGTH], *s[4];
    char nimi[LNAME];

    soft_menu_n=0;
    i=hae_apu("soft_keys",x);
    if (!i) { r_soft=0; return(1); }
    i=split(x,s,4);
    if (i<2)
        {
        sur_print("\nError in SURVO.APU (soft_keys)!");
        sur_print("\nPress ENTER!");
        WAIT; return(1);
        }
    if (i==2) { s[2]=s[0]; s[3]=s[1]; }

    if (*language=='2') { s[0]=s[2]; s[1]=s[3]; }

// RS NYI    return(soft_keys_set(s));
return(1); // RS ed.rivin korvaaja
    }

int restore_softkeys()
    {
    r_soft=r_soft_old;
    if (!r_soft) { soft_keys_init(); return(1); }
    muste_resize(c3+8,r3+3+r_soft);  // RS CHA sur_resize1(c3+8,r3+3+r_soft);
    disp_all();
/* RS REM 
    set_console_title();
    if (strncmp(os_ver,"WIN9",4)==0)
        sur_win9x_refresh(window_name);
*/
    return(1);
    }


