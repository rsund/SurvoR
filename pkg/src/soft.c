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

extern int nextch_editor();

extern char *edisk; // RS CHA edisk[] -> *edisk
extern char *language;

extern int g,r,c,r1,c1,r3,c3;
extern char *parm[];
extern char sbuf[];
extern char space[];
extern char *survo_path;
extern char *language;
extern char tut_info[];
extern int soft_key_activated;
extern char system_name[];
extern int etu;
extern int sdisp;
extern char *etmpd;
extern int r_mouse,c_mouse;
extern int display_off;
extern char window_name[];
extern int soft_vis;
extern char os_ver[];

extern int shadow_int[];
extern int special;
extern int only_key_events;
extern int insert_mode;
extern short cur_par[];
extern int keysum;


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
    FILE *soft_loadsave_temp_file; // RS ADD

    strcpy(x,edisk); strcat(x,name); 

    if (k==1)
        {
        soft_loadsave_temp_file=muste_fopen(x,"wb");
        if (soft_loadsave_temp_file==NULL) { tutstack_error(x,1); return(-1); }
        p=soft_info;
        while (*p) { putc((int)(*p),soft_loadsave_temp_file); ++p; }
        muste_fclose(soft_loadsave_temp_file);
        return(1);
        }

    if (*soft_stack_file==EOS) { *soft_info=EOS; return(1); } // RS ADD

    soft_loadsave_temp_file=muste_fopen(x,"rb");
    if (soft_loadsave_temp_file==NULL) { *soft_info=EOS; return(1); }
    p=soft_info;
    while (!feof(soft_loadsave_temp_file)) { *p=(char)getc(soft_loadsave_temp_file); ++p; }
    *(p-1)=EOS;
    muste_fclose(soft_loadsave_temp_file);
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

int soft_line_get(char *line)
    {
    char x[LLENGTH];
    char *p;
// RS REM    int i;

    fgets(x,LLENGTH-1,stemp);
    if (feof(stemp)) return(-1); // 22.9.2001
    p=strchr(x,'|'); control_char=*(p+1);
    strcpy(line,p+2);
    p=strchr(line,'\n'); if (p!=NULL) *p=EOS;
    p=strchr(line,'\r'); if (p!=NULL) *p=EOS; // RS unix fix
    return(1);
    }

int soft_find_line(char *s,char *x)
    {
    char line[LLENGTH];
// RS REM    char *p;
    int len;
    int i;

    len=strlen(s);

    rewind(stemp);
    fgets(line,LLENGTH-1,stemp); // Ohitetaan otsikkorivi
    while (1)
        {
        i=soft_line_get(x);
        if (i<0) return(-1);
        if (strncmp(s,x,len)==0) break;
        }
    return(1);
    }

int soft_full_length(char *y)
    {
    int k;

    y[SOFTLEN-1]=EOS;
    if (strlen(y)<SOFTLEN-1)
        {
        for (k=strlen(y); k<SOFTLEN; ++k) y[k]=' ';
        y[k]=EOS;
        }
    return(1);
    }

int ext(char *name)
    {
    int i;
    char *p;

    i=strlen(name);
    if (i<4) p=name; else p=name+i-4;
    if (strchr(p,'.')==NULL) return(0);
    return(1);
    }

int soft_keys_set(char *s[])
    {
    int i,k,h,h0;
    char nimi2[LNAME];
    char nimi[LNAME];
    char x[LLENGTH];
    char *t[SOFTLINES];
    char y[LLENGTH];
    char y2[LLENGTH];
    char mask[LLENGTH];
    char *t2[3];
    char *p,*q,*q2;
    char ch;
    int i1,i2;

// if (soft_menu_n) {    printf("\n%s %s|\n",s[0],s[1]); getck(); }

    if (strcmp(s[0],"*")==0) s[0]=soft_edit_file;
    else strcpy(soft_edit_file,s[0]);

    strcpy(nimi,s[0]);
//  if (strchr(nimi,':')==NULL) { strcpy(nimi,survo_path); strcat(nimi,s[0]); }
//  if (strchr(nimi,'.')==NULL) strcat(nimi,".EDT");
    if (strchr(nimi,':')==NULL && !netd(nimi)) { strcpy(nimi,survo_path); strcat(nimi,s[0]); }
    if (!ext(nimi)) strcat(nimi,".EDT");

    stemp=muste_fopen(nimi,"rt");

// RS ADD FIXME? Search from Survo path if not found
    if (stemp==NULL)
    	{ 
    	strcpy(nimi,survo_path);
    	strcat(nimi,s[0]);
    	if (!ext(nimi)) strcat(nimi,".EDT");
   	    stemp=muste_fopen(nimi,"rt");
    	}
    
    if (stemp==NULL)
        {
        sprintf(sbuf,"\nFile %s (soft_keys) not found!",nimi); 
        sur_print(sbuf);                       // RS printf -> sur_print
        sur_print("\nPress ENTER!");           // RS printf -> sur_print
        sur_getch();                         // RS getch -> sur_getch
        r_soft=0;
        return(1);
        }

    sprintf(nimi2,"KEYS %s:",s[1]);
// printf("nimi2=%s|\n",nimi2); getch();
    i=soft_find_line(nimi2,x);
    if (i<0) { sprintf(sbuf,"\n%s not found!",nimi2);
               sur_print(sbuf); WAIT; return(-1); }  // 22.9.2001
    if (soft_menu_n<MENUS)
        {
        strcpy(soft_field_name[soft_menu_n],nimi);
        strcpy(soft_list_name[soft_menu_n],s[1]);
        ++soft_menu_n;
        }

//Rprintf("\nx=%s",x);
    p=strchr(x,':')+1;
//Rprintf("\np=%s",p);    
    r_soft=split(p,t,SOFTLINES);
//Rprintf("\nr_soft=%d, *t[r_soft-1]=%s",r_soft,t[0]);    
    if (*t[r_soft-1]=='*') // soft_stack_file name
        {
        --r_soft;
        strcpy(soft_stack_file,t[r_soft]+1);
        if (strchr(soft_stack_file,'.')==NULL) strcat(soft_stack_file,".STK");
        }
    else *soft_stack_file=EOS;
//Rprintf("\nsoft_stack_file=%s",soft_stack_file);

    header_line_ind=0;
    if (*t[r_soft-1]=='!') header_line_ind=1;

    h=0;
    for (i=0; i<r_soft; i++)
        {
        strcpy(nimi2,t[i]); strcat(nimi2,":");
        k=soft_find_line(nimi2,y);
// printf("\ni=%d %s",i,y); getch();
        soft_line_get(y);
        soft_full_length(y);
        strcpy(soft_line[i],y);

// printf("\n%s",soft_line[i]); getch();
        soft_line_get(soft_shad[i]);
// printf("\n%s",soft_shad[i]); getch();
        soft_line_get(mask);
        while (1)
            {
            soft_line_get(y2); strcpy(y,y2);
// printf("\ny=%s",y); getch();
// Rprintf("\ny=%s",y);
            q=strchr(y,'|'); if (q!=NULL) *(q-1)=EOS;
            k=split(y,t2,3);
            if (strncmp(t2[0],"END",3)==0) break;  // RS CHA strcmp -> strncmp ,3  (unix \r fix)
            ch=*t2[0];
            soft_key_act[h]='-';
            if (k>1) soft_key_act[h]=*t2[1];

            strcpy(soft_key_command[h],"-");
            if (k>2) strcpy(soft_key_command[h],y2+(t2[2]-y));
            p=strchr(soft_key_command[h],'|');
            if (p!=NULL) *(p-1)=EOS;
// printf("\ncommand: %s",soft_key_command[h]); getch();
            soft_line_get(soft_key_text[h]);
            soft_full_length(soft_key_text[h]);
            soft_key_text[h][SOFTLEN-1]=EOS;
            soft_key_text_shadow[h]=control_char;
            p=strchr(mask,ch);
            if (p==NULL)
                {
                sprintf(sbuf,"\nMask %c not found!",ch);
                sur_print(sbuf); WAIT; return(-1);
                }
            i1=i2=p-mask;
            while (mask[i2]==ch) ++i2;
            soft_key_start[h]=i1+1;
            soft_key_end[h]=i2;
// printf("\nh=%d start=%d end=%d",h,soft_key_start[h],soft_key_end[h]);
// getch();

            soft_key_line[h]=i;
            soft_key_state_n[h]=1;
            soft_key_state[h]=0;
            h0=h; ++h;
            if (q==NULL) continue;
            q2=strchr(y2,'|')+1;
            while (1)
                {
                strcpy(y,q2+1);
// cursor(1,50); sprintf(sbuf,"\ny=%s",y); sur_print(sbuf); cursor(r,c);
                q=strchr(y,'|'); if (q!=NULL) { *(q-1)=EOS; q2=q+1; }
                strcpy(soft_key_command[h],y);
                soft_key_act[h]=soft_key_act[h-1];
                soft_line_get(soft_key_text[h]);
                soft_full_length(soft_key_text[h]);
                soft_key_text[h][SOFTLEN-1]=EOS;
                soft_key_text_shadow[h]=soft_key_text_shadow[h-1];
                soft_key_start[h]=soft_key_start[h-1];
                soft_key_end[h]=soft_key_end[h-1];
                soft_key_state_n[h]=1;
                soft_key_state[h]=0;
                ++soft_key_state_n[h0];
                ++h;
                if (q==NULL) break;
                }
            }

        }
    if (header_line_ind) --r_soft;
    n_soft_keys=h;
    muste_fclose(stemp);
    return(1);
    }


int soft_keys_init()
    {
    int i;
    char x[LLENGTH], *s[4];
// RS REM    char nimi[LNAME];

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

    return(soft_keys_set(s));
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

int soft_bottom_line_erase()
    {
    write_string(space,c3+8,'\237',r3+3+r_soft,1);
    return(1);
    }



int soft_key_answers()
    {
    int i,h,k;
// RS REM    int stack_ind;
    char xx[SOFTLEN];

    if (*soft_stack_file==EOS) return(1);
    soft_stack_save_load(2,soft_stack_file);
    for (h=0; h<n_soft_keys; ++h)
        {
        if (soft_key_act[h]!='P') continue;
        read_from_soft_stack(soft_key_command[h]+1,xx);
        k=soft_key_end[h]-soft_key_start[h]+1;
        i=strlen(xx);
        strncat(xx,space,k-i);
        cursor(r3+2+soft_key_line[h],soft_key_start[h]-8);
        PR_ENRM; sur_print(xx);
        }
     cursor(r,c);
     return(1);
     }


int soft_disp(int visibility)
// 0="gray"
    {
    int i;
    int line0;
    char *x,*xs;
    char *p,*q;
    char ch;

    soft_vis=visibility; // 9.2.2001

    if (!r_soft) return(1);

    if (display_off) visibility=0;
    line0=r3+3;
    for (i=0; i<r_soft; ++i)
        {
        if (!visibility)
            {
            write_string(space,c3+8,' ',line0+i,1);
            continue;
            }
        x=soft_line[i];
        xs=soft_shad[i];
        p=xs;
        while (1)
            {
            ch=*p; // if (!visibility || etu==1) ch='2';
            q=p;
            while (*q && *q==*p) ++q;
            write_string(x+(p-xs),q-p,ch,line0+i,p-xs+1);
            if (*q==EOS) break;
            p=q;
            }
        if (c3>72)
            write_string(space,c3-72,'\237',line0+i,81);
        }
    soft_bottom_line_erase();

    if (visibility)
        soft_key_answers();
    return(1);
    }


int show_items_on_header_line(int cc)
     {
     int i,j;
     int c0,h;
     char shadow;
     char *p;
// RS REM     char x[81];

     if (!header_line_ind) return(1);

     c0=cc+8;
     for (h=0; h<n_soft_keys; ++h)
         {
         i=soft_key_line[h];
         if (i!=r_soft) continue;
         j=soft_key_start[h];
         if (j<47)
             {
             if (c0<j) continue;
             if (c0>soft_key_end[h]) continue;
             }
         else
             {
             if (c0-c3+72<j) continue;
             if (j<48)
                 {
                 if (c0-c3+72>soft_key_end[h]) continue;
                 }
             else
                 { if (c0-c3+72>soft_key_end[h]) continue; }
             }
         shadow=soft_shad[i][j-1];
         if (soft_key_text_shadow[h]!='*')
             shadow=soft_key_text_shadow[h];

        p=soft_key_text[h];
        if (soft_key_act[h]=='p')
            {            
            i=strlen(edisk);
            p=edisk;
            if (i>80-11)
               sprintf(sbuf,"Data path: ...%s",p+i-80+11+3);  // RS CHA p-i+80-11
            else
               sprintf(sbuf,"Data path: %s",edisk);
            p=sbuf;
            shadow='1';
            }
         write_string(p,
                      strlen(p),
                      shadow,
                      r3+2+r_soft+1,1);
         break;
         }

     soft_message=1;
     return(1);
     }

int soft_prompt(char *vastaus,int pituus,int pos)
        {
        int i;
        char tila[LLENGTH];
        int m;
        int row,col;
        int row0,col0;

        sur_cursor_position(&row0,&col0);
        for (i=0; i<pituus; ++i) tila[i]=' '; tila[pituus]=EOS;
        for (i=0; i<strlen(vastaus); ++i) tila[i]=vastaus[i];
        sprintf(sbuf,"%.*s",pituus,tila); PR_EBLK; sur_print(sbuf);
        sur_locate(row0,col0+pos-1);
//      for (i=0; i<pituus; ++i) PR_LEFT;

//      pos=1;
        while (1)
            {
            keysum=0;
            i=sur_event();
            if (i==2 || i==3) // mouse_click
                {
                i=pituus;
                while(tila[i-1]==' ') --i;
                tila[i]=EOS;
                strcpy(vastaus,tila);
                return(1);
                }


            if (i==0) continue;

            SAVE_CURSOR;
            only_key_events=1;
            m=nextch_editor();
            only_key_events=0;
            RESTORE_CURSOR;
                switch (m)
                    {
                  case -2: break;
                  case CODE_BACKSP:
                    if (pos==1) break;
                    tila[pos-2]=' ';
                    PR_LEFT; PR_EBLK; sur_print(" "); PR_LEFT; --pos;
                    break;
                  case CODE_LEFT:
                    if (pos==1) break;
                    PR_LEFT; --pos; break;
                  case CODE_RIGHT:
                    if (pos==pituus) break;
                    PR_RIGHT; ++pos; break;
                  case CODE_DELETE:
                    for (i=pos-1; i<pituus-1; ++i) tila[i]=tila[i+1];
                    tila[pituus-1]=' ';
                    sur_cursor_position(&row,&col);
                    sprintf(sbuf,"%s",tila+pos-1); PR_EBLK; sur_print(sbuf);
                    sur_locate(row,col);
                    break;
                  case CODE_RETURN:
                  case CODE_EXIT:
                  case CODE_TAB:
                    if (!special) break;
                    i=pituus;
                    while(i>0 && tila[i-1]==' ') --i;
                    tila[i]=EOS;
                    strcpy(vastaus,tila);
                    if (m==CODE_TAB) return(2);
                    return(1);
                  case CODE_INSERT:
                    insert_mode=1-insert_mode;
                    if (insert_mode) CURSOR_INS;
                    else CURSOR_ON;
                    break;
                  case CODE_HOME:
                    sur_locate(row0,col0);
                    pos=1;
                    break;
                  case CODE_END:
                    i=pituus;
                    while(i>0 && tila[i-1]==' ') --i;
                    sur_locate(row0,col0+i);
                    pos=i+1;
                    break;
                  case CODE_ERASE:
                    PR_EBLK;
                    for (i=pos-1; i<pituus; ++i)
                        {
                        tila[i]=' ';
                        sur_print(" ");
                        }
                    sur_locate(row0,col0+pos-1);
                    break;

                  case CODE_UP:
                  case CODE_DOWN:
                  case CODE_PREV:
                  case CODE_NEXT:
                  case CODE_EXEC:
                    break;

                  default:
                    if (insert_mode)
                        {
                        if (pos<pituus && tila[pituus-1]!=' ') break;
                        for (i=pituus-2; i>=pos-1; --i) tila[i+1]=tila[i];
                        tila[pos-1]=(char)m;
                        sur_cursor_position(&row,&col);
                        sprintf(sbuf,"%s",tila+pos-1); PR_EBLK; sur_print(sbuf);
                        sur_locate(row,col+1);
                        }
                    else
                        {
                        PR_EBLK;
                        sprintf(sbuf,"%c",m); sur_print(sbuf); tila[pos-1]=(char)m;
                        }
                    if (pos<pituus) ++pos; else PR_LEFT;
                    break;
                    }
            }
        return(1);
        }

int soft_key_task(int h,int m_click,int m_dbl)
    {
    int rivi;
    char act;
    int hh;
    char command[SOFTLEN];
    int i,k;
    char xx[SOFTLEN];
    int stack_ind;
    int row1,col1;
    int pos;
    int tab_move;

    rivi=r1+r-1;
    act=soft_key_act[h];
    soft_key_activated=1;

    hh=h+soft_key_state[h];
    strcpy(command,soft_key_command[hh]);

    ++soft_key_state[h];
    if (soft_key_state[h]>=soft_key_state_n[h])
        soft_key_state[h]=0;

    switch(act)
        {
      case 't':
        edwrite(command,rivi,c1+c-1);
        c+=strlen(command); if (c>c3) c=c3;
        disp();
        break;
      case 'T':
        c=1;
        edwrite(space,rivi,c1+c-1);
        edwrite(command,rivi,c1+c-1);
        disp();
        break;
      case 'a':
        c=1;
        edwrite(space,rivi,1);
        edwrite(command,rivi,c1+c-1);
        disp();
        return(2);
      case 'A':
        if (!m_dbl) break;
        c=1;
        edwrite(space,rivi,1);
        edwrite(command,rivi,c1+c-1);
        disp();
        return(2);
      case 'g':
        soft_stack_save_load(2,soft_stack_file);
        strcpy(tut_info,soft_info);
        soft_multiple2=soft_multiple;
        soft_multiple=0;
      case 'h':
        strcpy(soft_actline,command);
        soft_act=1;
        return(2);
      case 'H':
        if (!m_dbl) break;
        strcpy(soft_actline,command);
        soft_act=1;
        return(2);

      case 'I':
        if (strcmp(command,"PREV")==0)
            {
            if (soft_menu_n<2) break;
            if (*soft_list_name[soft_menu_n-1]=='-') break;
            k=soft_menu_n-1;
            while (soft_menu_n>1)
                {
                --soft_menu_n;
                i=soft_menu_n;
                if (*soft_list_name[i-1]=='-') continue;
            if (strcmp(soft_list_name[i-1],soft_list_name[k])==0 &&
                strcmp(soft_field_name[i-1],soft_field_name[k])==0)
                    continue;
                break;
                }

            g=3;
            parm[1]=soft_field_name[soft_menu_n-1];
            parm[2]=soft_list_name[soft_menu_n-1];
            --soft_menu_n;
            op_softkeys();
            return(1);
            }
      case 'K':
        soft_code=atoi(command);
        return(3);
      case 'k':
        if (strcmp(command,"ENTER")==0) *command='\15';
        soft_char=*command;
        return(4);
      case 'p': // show datapath! p siis näin "varattu"!
        return(1);
      case 'P':
        tab_move=0;
        while (1)
            {
            soft_stack_save_load(2,soft_stack_file);
            stack_ind=atoi(command+1);
            read_from_soft_stack(command+1,xx);
            k=soft_key_end[h]-soft_key_start[h]+1;
            i=strlen(xx);

            strncat(xx,space,k-i);
            row1=r3+2+soft_key_line[h]; col1=soft_key_start[h]-8;
            cursor(row1,col1);
            write_string(soft_key_text[h],
                         strlen(soft_key_text[h]),
                         ' ',
                         r3+2+r_soft+1,1);
            if (i==0 || r_mouse!=row1 || c_mouse>col1+7+k-1 || tab_move)
                pos=1;
            else
                {
                pos=c_mouse-col1-7+1;
                if (pos>i+1) pos=i+1;
                }

            i=soft_prompt(xx,k,pos);
            cursor(row1,col1);
            PR_ENRM; sur_print(xx);
            sprintf(sbuf,"%.*s",(int)(k-strlen(xx)),space); sur_print(sbuf);
            cursor(r,c);
            soft_stack_set(xx,stack_ind); // 31.7.00 kokeilu!
            soft_stack_save_load(1,soft_stack_file);
            if (i!=2) break;
            while (h<n_soft_keys)
                {
                ++h;
                if (soft_key_act[h]=='P')
                    {
                    strcpy(command,soft_key_command[h]);
                    break;
                    }
                }
            if (h==n_soft_keys) break;
            tab_move=1;
            }
        cursor(r,c);
        break;
        }

    return(1);
    }


int soft_key_activate(int rr,int cc,int m_click,int m_dbl)
    {
    int i,j,h,hh;
    int r0,c0;
    char shadow;
    char *p;

    if (!r_soft) return(1);
    if (rr==0) { show_items_on_header_line(cc); return(1); }
    if (rr<r3+2 || rr>r3+r_soft+1) return(1);

    r0=rr-r3-2; c0=cc+8;
    for (h=0; h<n_soft_keys; ++h)
        {
        i=soft_key_line[h];
        if (r0!=i) continue;
        j=soft_key_start[h];
        if (c0<j) continue;
        if (c0>soft_key_end[h]) continue;

        if (m_click || m_dbl) { return(soft_key_task(h,m_click,m_dbl)); }

        shadow=soft_shad[i][j-1];
        if (soft_key_text_shadow[h]!='*')
            shadow=soft_key_text_shadow[h];

        hh=h+soft_key_state[h];

        p=soft_key_text[hh];
        if (soft_vis) // 9.2.2001
        write_string(p,
                     strlen(p),
                     shadow,
                     r3+2+r_soft+1,1);

 p_soft_key_text=soft_key_text[hh]; soft_current_key_text_shadow=shadow;
        soft_message=1;
        break;
        }
    if (h==n_soft_keys && soft_message)
        write_string(space,c3+8,' ',r3+2+r_soft+1,1);
    return(1);
    }

int sur_resize1(int cc,int rr) // vain WIN 9X - CONAGENT.PIF-tiedoston vuoksi
    {
    muste_resize(cc,rr,survo_path); // RS CHA sur_resize
    return(1);
    }

extern int set_console_title();
// SOFTKEYS <nn.edt>,<key>
int op_softkeys() // 15.3.2000
        {
        int i;
        int rs;
        int vr_soft;

        vr_soft=r_soft;
        if (g==2)
            {
            if (muste_strcmpi(parm[1],"OFF")==0)
                {
                r_soft_old=r_soft;
                r_soft=0;
                }
            else if (muste_strcmpi(parm[1],"MULTIPLE")==0)
                {
                soft_multiple=soft_multiple2=1;
                sur_flush_input();
                return(1);
                }
            else if (muste_strcmpi(parm[1],"SINGLE")==0)
                {
                soft_multiple=soft_multiple2=0;
                return(1);
                }
            else if (muste_strcmpi(parm[1],"PREV")==0)
                {
                parm[1]=soft_prev_field;
                parm[2]=soft_prev_list;
                g=3;
                op_softkeys(); return(1);
                }

            else if (muste_strcmpi(parm[1],"DELSTACK")==0)
                {
                *soft_info=EOS;
                }
            }

        else if (g<3) soft_keys_init();
        else
            {
            if (*parm[2]!='-')
                {
                strcpy(soft_prev_field,parm[1]);
                strcpy(soft_prev_list,parm[2]);
                }
            soft_keys_set(&parm[1]);
            }

        if (r_soft) rs=r_soft+1; else rs=0;
        i=r3+2+rs; if (i<25) c3=72;
        i=sur_resize1(c3+8,i);
        set_console_title();
        disp_all();
/* RS REM
        if (strncmp(os_ver,"WIN9",4)==0)
            sur_win9x_refresh(window_name);
*/
        return(1);
        }



