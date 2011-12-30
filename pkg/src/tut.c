#include <R.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include "survo.h"
#include "survolib.h"

#define SURVO84C 1
/* 1=SURVO 84C taustalla 0=ei */

#define TUT_COMMENT_CODE 252
#define MAXTUT 5

extern char *z;
extern unsigned int r,r1,r2,r3,c,c1,c2,c3;
extern unsigned int ed1,ed2;

extern int special;
extern unsigned int *zs;
extern char *op;

extern char *survo_path;
extern char *tut_info; // RS CHA extern char tut_info[];
extern char *etufile;  // RS CHA etufile[LNAME];

extern char break_sucro[];
extern char sucropath[];
extern char *parm[];
extern char sbuf[];
extern char survo_id[];
extern char prompt_space[];
extern char op_sana[];

extern int g;
extern int ref_c1;
extern char *prompt_line;
extern int sdisp;
extern int alkututor;
extern long wait_hetki;
extern int soft_vis;


FILE *tutor;
extern int etu; //RS EDITOR /* 0=TUT mode off, 1=TUT file definition, 2=TUT run */
extern int etuu; // RS EDITOR /* 1=lapset eivät TUT-moodissa 0=lapset TUT-moodissa */
// RS EDITOR char etufile[LNAME];
extern int etu1,etu2,etu3; // RS EDITOR
extern long tutpos; // RS EDITOR
long tutalku=0L; /* 10.2.90 */
int tut_special_code=0;
int tut_not_break=0;
int tut_loppu;  /* 12.10.88 */
int sukro_esto=0;
int tut_not_break2=0;
int help_on=0;
extern int tut_index; // RS EDITOR
char error_handler[LNAME];  /* 29.4.91 */

#define MAXTUT 5
extern int ntut; // RS EDITOR
char tutnimi[MAXTUT][LNAME];
char tutmemb[MAXTUT][LNAME]; // 14.11.2004 vain HOME-ilmoituksiin!
long tutposi[MAXTUT];
int tuttila[MAXTUT];
int tutetu2[MAXTUT];  /* 10.11.88 */
long tutalut[MAXTUT];
char etusukro[LNAME];
int ei_odotusta;
int tut_wait_c;

extern int r_pause,r1_pause,c_pause,c1_pause; // 21.8.2004  // RS EDITOR

int rajoitettu_vastausaika=0;
time_t vastauksen_alkuhetki,max_vastausaika;

// Ei saa muuttaa: käytössä demoversiossa xor SURVO.ID2 27.3.2002
char s_functions[]=
"Sucro functions: E=End of definition, C=Control codes, R=Repeat ? ";

extern int sucro_pause; // RS EDITOR
int sucro_menu=0;

extern char *wait_tut_name; // RS EDITOR toimiiko?!?  char[] -> char *
long wait_tut_time;
extern int wait_tut_type; // RS EDITOR // 1=cancelled by user's actions 2=activates always

extern int nextkey_editor();
extern int nextch_editor();
int tutch_editor();
extern void prompt_editor();
extern void headline_editor();

extern FILE *muste_fopen2();
extern int muste_fclose2();

// WAIT_TUT name,time,type
int wait_tut2(char *p1,char *p2,char *p3)
    {
    long i;

    strcpy(wait_tut_name,p1);
    time(&i); wait_tut_time=i+atol(p2);
    wait_tut_type=atoi(p3);
    return(1);
    }

int op_wait_tut()
    {
    wait_tut2(parm[1],parm[2],parm[3]);
    return(1);
    }


int tut_sulje()
        {
        tutpos=ftell(tutor);
        muste_fclose2(tutor);
		return(1);
        }

int tut_avaa()  // RS Added check for successful (re)opening of sucro
        {
        char x[LLENGTH];
        
        if (!etu) return(1);

        strcpy(x,etufile);
        
        if (etu==1) tutor=muste_fopen2(etufile,"r+b");
        if (etu==2)
        	{ 
        	tutor=muste_fopen2(etufile,"rb"); 
        	if (tutor==NULL)
        		{ 
        		strcpy(etufile,survo_path); strcat(etufile,"S/");
        		strcat(etufile,x);
                tutor=muste_fopen2(etufile,"rb");
                }
             if (tutor==NULL) 
        		{ 
        		strcpy(etufile,survo_path); strcat(etufile,"TUT/");
        		strcat(etufile,x);
                tutor=muste_fopen2(etufile,"rb");
                }
            }
        if (tutor==NULL) 
        	{ 
        	sprintf(sbuf,"\nCannot (re)open sucro %s!",etufile);
        	sur_print(sbuf); WAIT; return(-1);
            }    
        muste_fseek(tutor,(long)tutpos,SEEK_SET);        
        return(1);
        }

int s_tut_init()
        {
        if (etu==0 && sucro_pause==0) return(1);

        if (etu==1) tutor=muste_fopen2(etufile,"r+b");
        if (etu==2 || sucro_pause) tutor=muste_fopen2(etufile,"rb");
        if (tutor==NULL) { sur_print("Sucro error!"); WAIT; return -1; } // RS ADD
        muste_fseek(tutor,(long)tutpos,0); return(1);
        }

int tut_init()
        {
        extern int space_break; // RS ADD
        
        space_break=0;
        if (etu==0) return(1);

        if (etu==1) tutor=muste_fopen2(etufile,"r+b");
        if (etu==2) tutor=muste_fopen2(etufile,"rb");
        if (tutor==NULL) { sur_print("Sucro error!"); WAIT; return -1; } // RS ADD
        muste_fseek(tutor,tutpos,0); return(1);
        }

int tut_end()
        {
        if (etu==0) return(1);
        tutpos=ftell(tutor); muste_fclose2(tutor);
        return(1);
        }

void tutclose()
        {
        if (etu==0)
            { PR_EBLD; sur_print("\nSucro closing error!"); WAIT; }
        else muste_fclose2(tutor);
        ntut=0; etu=0;
        soft_disp(1);
//      if (!kbhit_on) disp();    5.11.2000 kbhit_on poistettu!
        }

void tut_virhe(int i)
        {
        sprintf(sbuf,"\nError in Sucro! (%d)",i);
        sur_print(sbuf); WAIT; tutclose(); disp();
        }


void prompt_clear()
        {
        if (prompt_line!=NULL) { prompt_line=NULL; disp(); }
        }


int tutstack_error(char *x,int k)
        {
        tutclose();
        sprintf(sbuf,"\ntutstack error %d!",k);
        sur_print(sbuf); WAIT; 
        disp(); // RS ADD
        return(-1);
        }

int parm_from_soft_stack(char *s)
    {
    char *p,*q,*pp;
    char x[LLENGTH];

    p=s+2; q=strchr(p,':');
    if (q==NULL)
        {
        sur_print("\nError in sucro parameter 1!");
        WAIT; return(-1);
        }
    *q=EOS; ++q;
    pp=strchr(q,']');
    if (q==NULL)
        {
        sur_print("\nError in sucro parameter 2!");
        WAIT; return(-1);
        }
    *pp=EOS;
    soft_stack_save_load(2,p);
    read_from_soft_stack(q,x);
    strcat(tut_info,x);
    return(1);
    }

int read_from_stack(char *nro,char *s)  /* 17.9.1995 */
        {
        int i,h;
        char *p,*q;
        h=atoi(nro)-1;
        i=0; p=tut_info;
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


int read_tutword(char s[])
        {
        int i,m;
        i=0;
        while ((m=getc(tutor))!=(int)'@' && i<200)
            {
            s[i++]=m;
            }
        s[i]=EOS;

        if (i==200)
            {
            sprintf(sbuf,"\ns=%s",s); sur_print(sbuf); WAIT; // RS oli getck()
            tut_virhe(2);
            }
        return(1);
        }

void tutsave(int m)
        {
        putc(m,tutor);
        }



int read_cond(char *s)
        {
        char sana[LLENGTH];
        read_tutword(sana);
        if (*sana=='C') { strcpy(s,sana+1); return(1); }
        read_from_stack(sana,s);  /* 18.9.1995 */
        return(1);
        }

int sukrohaku()
        {
        int i,n,k;
        char x[LLENGTH];
        char *p;

        read_tutword(x);  /* SURVO 84C SUCROS */        
        read_tutword(x);
        
        n=atoi(x);
        for (i=0; i<n; ++i)
            {
            read_tutword(x);
                        
// Rprintf("\n%d: %s\n",i,x);            

/*            
            k=1;            
            while (*(x+k)==' ' || *(x+k)=='\n' || *(x+k)=='\r' || *(x+k)=='\t') ++k; // RS FIXME tässä ohi myös \r:stä, \t:stä ja \n:stä             
            while(*(x+k)!=EOS) { Rprintf("%d",*(x+k)); k++; }

Rprintf("\n"); 
            
            k=0;
            while(*(etusukro+k)!=EOS) { Rprintf("%d",*(etusukro+k)); k++; }            
*/


			k=1;
			p=x+1;
            while (*(x+k)==' ' || *(x+k)=='\n' || *(x+k)=='\r' || *(x+k)=='\t') ++k; // RS FIXME tässä ohi myös \r:stä, \t:stä ja \n:stä 
            while (*p!=' ') ++p;  
            tutalku=atol(p+1); ++p; *p=EOS;
            
// Rprintf("\ntutalku:%ld",tutalku);  
            
            if (muste_strcmpi(etusukro,(x+k))==0) break;  // RS 1 -> k
            }
        if (i==n) return(-1);
        muste_fseek(tutor,(long)tutalku,SEEK_SET);
        return(1);
        }


int tutopen2(char *name,char *mode,char *path)
        {
        strcpy(etufile,path); strcat(etufile,name);
        file_name_ext(etufile,".TUT"); /* 16.11.88 */

//Rprintf("\n\ntutopen etufile: %s",etufile);
      
        tutor=muste_fopen(etufile,mode);
        if (tutor==NULL) return(0);
        return(1);
        }


int tutopen(char *name,char *mode)
        {       
        int i,minus_paths;
        char x[LLENGTH];
        char *p;
        char name2[LNAME];
		extern char* muste_startpath;        

        tut_not_break2=0;       /* 15.11.88 */
        strcpy(name2,name);     /* 10.2.1990 */
        *etusukro=EOS; tutalku=0L;
        p=strchr(name,'-');
 
        minus_paths=1;   // RS
 		if (minus_paths) // 30.9.2010 uutta koodia
            {
            p=name+strlen(name)-1;
            
            while (p>=name)
                {
                if (*p=='-') break;
                if (strchr("\\:/",*p)!=NULL) { p=NULL; break; } // RS FIXME path "/" lisätty
                --p;
                }
                
            if (p<=name) p=NULL;
            }
 
        i=1;
        if (p!=NULL)
            {
            *p=EOS;
            if (*name=='*') i=0; else i=1; /* i=0: previous etufile 23.1.93 */
            strcpy(etusukro,p+1); strcat(etusukro," ");
            }
        if (i) { filename(etufile,name); file_name_ext(etufile,".TUT"); }

//Rprintf("\n\ntutopen etufile: %s",etufile);
//Rprintf("\ntutopen etusukro: %s",etusukro);
//Rprintf("\ntutopen name: %s",name);

        tutor=muste_fopen(etufile,mode);        
        i=0; if (tutor!=NULL) i=1;
                						 
 		if (i!=1) i=tutopen2(name,mode,muste_getwd()); // RS CHA because filename() is not working

        if (*sucropath && i!=1) i=tutopen2(name,mode,sucropath);  /* 10.2.90 */

        if (i!=1 && alkututor) { strcpy(x,muste_startpath); 
        						 i=tutopen2(name,mode,x); }   // RS ADD  

        if (i!=1 && alkututor) { strcpy(x,survo_path); i=tutopen2(name,mode,x); }  // RS ADD     						 

        if (i!=1) { strcpy(x,survo_path); strcat(x,"S/"); // RS CHA  \\ -> /
                    i=tutopen2(name,mode,x); }
        if (i!=1) { strcpy(x,survo_path); strcat(x,"TUT/"); // RS CHA  \\ -> /
                    i=tutopen2(name,mode,x); }
               
                    
                    
/* RS REM
        if (i!=1) { strcpy(x,survo_path16); strcat(x,"S\\");
                    i=tutopen2(name,mode,x); }
        if (i!=1) { strcpy(x,survo_path16); strcat(x,"TUT\\");
                    i=tutopen2(name,mode,x); }
*/

        if (i==1 && *etusukro)      
            i=sukrohaku();

// Rprintf("\ntutopen etufile: %s",etufile);
// Rprintf("\ntutopen etusukro: %s",etusukro);            
            
        if (i==1) return(1);


        PR_EINV; sprintf(sbuf,"\nSucro %s not found!",name2);
        sur_print(sbuf); WAIT; disp();
        return(0);
        }


void tut_continue(char *sana)
        {
        char *s;
        int m;
        char label[LLENGTH];
        if (*sana==EOS) return;
        s=sana;
        if (*s=='G')
            {
            ++s;
            muste_fseek(tutor,(long)tutalku,SEEK_SET);
            while (!feof(tutor))
                {
                m=getc(tutor);
                if (m!=CODE_PRE) continue;
                m=getc(tutor);
                if (m!='T') continue;
                m=getc(tutor);
                if (m!='X') continue;
                read_tutword(label);
                if (strcmp(label,s)==0) return;
                }
            tutclose(); return; /* label not found! */
            }
        if (*s=='L') ++s;
        muste_fclose(tutor);
        m=tutopen(s,"rb");
        if (m==0) { etu=0; ntut=0; soft_disp(1); }
        }


void tut_select(int n,int k)
        {
        int i;
        char sana[LLENGTH],jatko[LLENGTH];
        for (i=0; i<n; ++i)
            {
            read_tutword(sana);
            if (i==k) strcpy(jatko,sana);
            }
        tut_continue(jatko);
        }

int op_tutor()
        {
        int i,k;
        int del_ref;

        if (sukro_esto) return(1);
        if (ntut>0)
            {
            if (ntut>MAXTUT)
                {
                sur_print("\nToo many nested sucros!");
                WAIT; tutclose(); return(0);
                }
            tut_sulje(); tutposi[ntut-1]=tutpos;
            strcpy(tutmemb[ntut-1],etusukro);  // 14.11.2004
            tutalut[ntut-1]=tutalku;
            strcpy(tutnimi[ntut-1],etufile);
            tuttila[ntut-1]=etu;
            tutetu2[ntut-1]=etu2; etu2=0; /* 10.11.88 */
            }
        else strcpy(error_handler,"SURVOERR");  /* 30.4.91 */


        if (parm[0]!=NULL && *parm[0]=='/')
            {
            for (i=g; i>0; --i) parm[i]=parm[i-1];
            ++parm[1];
            ++g;
            }

        if (g<2) { op_incomplete(); return(-1); }
      
        del_ref=1;
        if (g>2)
            {
            if (strcmp(parm[2],"@")!=0)
                {
                *tut_info=EOS;
                for (i=2; i<g; ++i)
                    {
                    if (parm[i][0]=='[' && parm[i][1]=='[')
                        {
                        k=parm_from_soft_stack(parm[i]);
                        if (k<0) return(0);
                        }
                    else strcat(tut_info,parm[i]);
                    strcat(tut_info,"@");
                    }
                }
            else del_ref=0;
            }
        else if (alkututor) strcpy(tut_info,"(start)@");
        else
        {
//muste_reset_pointers();
//Rprintf("\ntut_info %ld : %s",&(*tut_info),tut_info); 
        strcpy(tut_info,"(empty)@");
        }

        etu=0;
        if (ntut<1)
            {
            etu1=2;   /* nopeus  */
            etu2=0;   /* char display off */
            tut_not_break=0;
            }

//Rprintf("\nop_tutor ent tutopen; parm: %s",parm[1]);
        i=tutopen(parm[1],"rb");
        if (i) { etu=2; ++ntut; } else ntut=0;  /* ntut=0; 8.11.88 */
        if (del_ref) ref_c1=0; /* mahd. refer.piste unohdetaan */

        return(i);
        }

int tut_soft_restore() // 25.3.2001
    {
    soft_vis=1;
    soft_keys_init(); // 13.2.2001

    g=3;
    sprintf(sbuf,"%d",r3); parm[1]=sbuf;
    sprintf(sbuf+12,"%d",c3); parm[2]=sbuf+12;
    op_resize();

    sys_save_restore(2); // 2.3.2001
    return(1);
    }

int stack_save_load(int k,char *nimi) //  k:  1=save 2=load 
        {
//      char nimi[LLENGTH];   21.8.2004
        char x[LLENGTH];
        FILE *edfield; // RS REM extern
        extern char *etmpd;  // RS CHA etmpd[] -> *etmpd 
        char *p;
        int n;

//      read_cond(nimi);      21.8.2004
        strcpy(x,nimi); // 15.8.2000
//Rprintf("\nx:%s, x[0]=%c",x,x[0]);
		if (!muste_is_path(x)) { strcpy(x,etmpd); strcat(x,nimi); } 
// RS CHA        if ((strchr(x,':')==NULL) && (x[0]!='/') && (x[0]!='~'))  // RS ADD Unix paths
         muste_expand_path(x);
 
//Rprintf("\nx:%s",x);            
        if (k==1)
            {
            edfield=muste_fopen(x,"wb");
            if (edfield==NULL) { tutstack_error(x,1); return(-1); }
            p=tut_info;
            while (*p) { putc((int)(*p),edfield); ++p; }
            muste_fclose(edfield);
            return(1);
            }

        edfield=muste_fopen(x,"rb");
        if (edfield==NULL) { *tut_info=EOS; return(1); }
        p=tut_info; n=0;
        while (!feof(edfield) && n<LLENGTH-1) { ++n; *p=(char)getc(edfield); ++p; }
        *(p-1)=EOS;
        muste_fclose(edfield);
        return(1);
        }


int tut_sound(char *t)
    {
muste_fixme("FIXME: tut_sound stub\n");    

/* RS NYI    
    int i,k;
    char x[LLENGTH], *s[3];
    char nimi[LNAME];

    k=atoi(t);

    if (k>0)
        {
        i=hae_apu("tut_sounds",x);
        if (i==0) return(1);
        i=split(x,s,3);
        sprintf(nimi,"%sSND\\%s.WAV",survo_path,s[k-1]);
        }
    else
        {
        if (strchr(t,':')!=NULL) strcpy(nimi,t);
        else
        sprintf(nimi,"%sSND\\%s.WAV",survo_path,t);
        }
    sur_play_sound(nimi);
*/
    return(1);
    }


int tut_pause() // 21.8.2004
    {
    r_pause=r; r1_pause=r1;
    c_pause=c; c1_pause=c1;
    edt_talletus("S_PAUSE");
    stack_save_load(1,"S_PAUSE.STK");
    tut_sulje(); etu=0;
//  sprintf(sbuf,"%sSND\\SIB23B.WAV",survo_path);
//  sur_play_sound(sbuf);
    tut_sound("2");
    return(1);
    }


int nop()
     { return(1); } 

int Wdisp_editor()
        {
        CURSOR_OFF; headline_editor(); cursor_on(); cursor(r,c); return(1);
        }

static void press_key(int m)
        {
        int ch,spec;
        int k;

        spec=special;

        if (etu1<2) { sur_sleep(10); Wdisp_editor(); return; }  /* 23.10.89 */
        k=sur_wait2((long)(1000L*tut_wait_c),Wdisp_editor);
        if (k==2) ch=CODE_EXEC; // mouse_click

        else if (k) ch=nextkey_editor(); else ch=m; // RS sur_wait2:ssa ei saa lukea näppäintä

        while (ch!=m && ch!=CODE_LEFT)
            {
            if (ch==CODE_HELP) { help_on=0; etu2=0; ch=m; break; }
//          BEEP;
//          sprintf(sbuf,"%sSND\\PROTECT.WAV",survo_path);
//          sur_play_sound(sbuf);
            tut_sound("1");

            if (ch==CODE_HOME)          /* 26.4.90 */
                {
                CURSOR_OFF; cursor(r3+1,1);
                PR_EUDL; sprintf(sbuf,"%s-%s  ",etufile,etusukro);
                sur_print(sbuf);
                PR_ENRM; cursor(r,c); CURSOR_ON;
                }
            k=sur_wait((long)(1000L*tut_wait_c),Wdisp_editor,1);
           if (k) ch=nextkey_editor(); else ch=m;
            }
        special=spec;
        }

int mouse_key_select(int rr,int cc)
    {
    int i;
    char x[LLENGTH];
    char *p;

    read_string(x,NULL,c3+8,rr+1,1); x[c3+8]=EOS; // RS Rivin luku suoraan näytöltä disp.c:ssä
//  printf("\nx=%s|",x+cc); getck();
// Rprintf("\nx=%s|",x+cc);

    // valikoissa tilanteet: ... 2*  ..: 2 *
    p=strstr(x,"... ");
    if (p!=NULL) return((int)*(p+4));
    p=strstr(x,"..: ");
    if (p!=NULL) return((int)*(p+4));

    i=cc;
    if (x[i+1]=='.') return((int)x[i]);

    while (i>=0)
        {
        if (x[i]=='.') break;
        --i;
        }
    if (i>=0) p=x+i;
    else
        {
        p=strchr(x+1,'.');
        if (p==NULL)
            {
            return(-1);
            }
        }
    return((int)*(p-1));
    }


int tut_set();
int tutcat(char *s)
        {
        int i;

        if (tut_index>=1)
            {
            i=tut_index; tut_index=0;  /* tut_set() calls sometimes tutcat() */           
// muste_fixme("FIXME: tut_index>=1 in tut.c!");            
            i=tut_set(s,i);
            return(i);  /* oli -1 !!!! */
            }
        if (strlen(tut_info)+strlen(s)+1>LLENGTH-1)
            { sur_print("\nTutstack overflow!"); WAIT; return(-1); }
        strcat(tut_info,s); strcat(tut_info,"@");
        return(1);
        }


int tut_set(char *sana,int i)
        {
        char x[LLENGTH+64];
        int k;
        char *p,*q,*r;
        char ch;

        q=x; p=tut_info;
        for (k=0; k<i-1; ++k)
            {
            while (*p && *p!='@') *q++=*p++;
            if (*p==EOS) break;
            *q++=*p++;  /* @ */
            }
        if (*p==EOS)
            {
            for ( ; k<i-1; ++k) tutcat("");
            tutcat(sana); return(1);
            }
        r=sana;
        while (*r) *q++=*r++;
        while (*p && *p!='@') ch=*p++;
        while (*p) *q++=*p++;
        *q=EOS;
        if (strlen(x)>LLENGTH-1) { sur_print("\nTutstack overflow!");
                                   WAIT; return(-1); }
        strcpy(tut_info,x); return(1);
        }


int tut_specification(char *sana)
        {
        int i;
        char x[LLENGTH];

        i=spec_find(sana+1,x,LLENGTH-1);
        if (i>=0)
            i=tutcat(x);
        else
            i=tutcat("");
        if (i<0) { tutclose(); }
        return(1);
        }

static void time_prompt(char *kysymys,char *vastaus,int pituus,int vastausaika)
// vastausaika; /*  0.1 sek  0=rajaton */
        {
        int i;
        char tila[LLENGTH];
        int m, pos;
        extern int r_mouse,c_mouse;
        

        for (i=0; i<pituus; ++i) tila[i]=' ';
        for (i=0; i<strlen(vastaus); ++i) tila[i]=vastaus[i];
        sprintf(sbuf,"%s%.*s*",kysymys,pituus,tila); sur_print(sbuf);
        for (i=0; i<pituus+1; ++i) PR_LEFT;
        time(&vastauksen_alkuhetki);
                     /* -25.5.95: maxt=0.1*maxaika; */

        if (vastausaika)
            {
            max_vastausaika=0.01*vastausaika*tut_wait_c;
            rajoitettu_vastausaika=1;
            }
        else rajoitettu_vastausaika=0;

        pos=1;
        while (1)
            {
// RS REM Tarvitaanko tosiaan globaalina, kun ei näyttäisi olevan käytössä muualla: keysum=0;
            SAVE_CURSOR;            
            m=nextch_editor();         
// if (m<0) { printf("\nmouse: %d %d|",r_mouse,c_mouse); WAIT; }
            RESTORE_CURSOR;  
            if (m<0) // mouse_click
                {
                m=mouse_key_select(r_mouse,c_mouse);
                if (m<0) continue;
                *vastaus=(char)m; vastaus[1]=EOS;
                rajoitettu_vastausaika=0;
                return;
//              edread(tila,r1+r_mouse-1);
//              printf("\nrivi=%.50s|",tila); WAIT;
                }

            switch (m)
                    {
                  case CODE_BACKSP:
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
                    rajoitettu_vastausaika=0;
                    return;
                  case CODE_HOME:
                    for (;pos>1;--pos) PR_LEFT;
                    break;
                  case CODE_ERASE:
                    for (i=pos-1; i<pituus; ++i)
                        {
                        tila[i]=' ';
                        printf(" ");
                        }
                    for (i=pos-1; i<pituus; ++i) PR_LEFT;
                    break;
                  case CODE_INSERT:
                    if (tila[pituus-1]!=' ') { BEEP; break; }
                    for (i=pituus-1; i>=pos; --i) tila[i]=tila[i-1];
                    tila[pos-1]=' ';
                    for (i=pos; i<=pituus; ++i)
                     { sprintf(sbuf,"%c",tila[i-1]); sur_print(sbuf); }
                    for (i=pos; i<=pituus; ++i) PR_LEFT;
                    break;
                  case CODE_DELETE:
                    for (i=pos; i<pituus; ++i) tila[i-1]=tila[i];
                    tila[pituus-1]=' ';
                    for (i=pos; i<=pituus; ++i)
                     { sprintf(sbuf,"%c",tila[i-1]); sur_print(sbuf); }
                    for (i=pos; i<=pituus; ++i) PR_LEFT;
                    break;
                  case CODE_EXEC:
                    strcpy(vastaus,"(break)");
                    rajoitettu_vastausaika=0;
                    return;
                  default:
                    sprintf(sbuf,"%c",m); sur_print(sbuf); tila[pos-1]=(char)m;
                    if (pos<pituus) ++pos; else PR_LEFT;
                    break;
                    }
            }
        }



int tut_question()
        {
        int i;
// RS REM        char *p;
        char kysymys[LLENGTH];
        char vastaus[LLENGTH];
        char sana[LLENGTH];
        int pituus;
        int maxaika;

/*      tut_index=atoi(info);       */
                       /*  K<aika>@<kysymys>@<vastaus>@<pituus>@
                             0.1s
                       */
        read_cond(sana);
        if (*sana=='~') { tut_specification(sana); return(1); }
        maxaika=atoi(sana); if (maxaika==0) maxaika=600;
        read_tutword(kysymys);

        i=read_cond(vastaus);
        if (i>0)
            {
            read_tutword(sana);
            pituus=atoi(sana);
            etu=0;
            sucro_menu=1;
            time_prompt(kysymys,vastaus,pituus,maxaika);
            sucro_menu=0;
            etu=2;
            tutcat(vastaus);
            }
        return(1);
        }


int tut_specification2(char *sana) // 13.6.2005
        {
        int i;
        char x[LLENGTH];

        i=spec_find(sana,x,LLENGTH-1);
        if (i>=0)
            strcpy(sana,x);
        else
            *sana=EOS;
        return(1);
        }


int tut_arit(char *s1,char *s2,char *op)
        {
        double a=0,a1,a2;
// RS REM        char *p,*q;
// RS REM        int virhe;


        a1=atof(s1); a2=atof(s2);
        switch (*op)
            {
          case '+': a=a1+a2; break;
          case '-': a=a1-a2; break;
          case '*': a=a1*a2; break;
          case '/': if (a2!=0.0) a=a1/a2;
                    else
                      {
                      PR_EUDL; sur_print("\nDivision by 0 in a sucro!");
                      PR_ENRM; WAIT; tutclose();
                      disp(); return(-1);
                      }
            }
        fconv(a,"",sbuf);
        return(1);
        }


int tut_laske4(char *s,double *px)
    {
    if (*s=='W')
        {
        read_from_stack(s+1,sbuf); *px=atof(sbuf);
        }
    else *px=atof(s);
    return(1);
    }

int tut_laske3(char *s1, char *s2, double *px, double *py)
    {
    tut_laske4(s1,px);
    tut_laske4(s2,py);
    return(1);
    }

int tut_laske2(char *s,double *px)
    {
    char *p;
    double x,y;

    s[strlen(s)-1]=EOS;   // |lauseke|  |:t pois
    ++s;
    p=strchr(s,'+');
    if (p!=NULL)
        {
        *p=EOS;
        tut_laske3(s,p+1,&x,&y); *px=x+y; return(1);
        }
    p=strchr(s,'-');
    if (p!=NULL)
        {
        *p=EOS;
        tut_laske3(s,p+1,&x,&y); *px=x-y; return(1);
        }
    p=strchr(s,'*');
    if (p!=NULL)
        {
        *p=EOS;
        tut_laske3(s,p+1,&x,&y); *px=x*y; return(1);
        }
    p=strchr(s,'/');
    if (p!=NULL)
        {
        *p=EOS;
        tut_laske3(s,p+1,&x,&y); *px=x/y; return(1);
        }
    tut_laske4(s,px); return(1);


    return(1);
    }

void tut_laske(char *tulos,char *s1,char *s2,char *oper)
        {
        long l0=0,l1,l2;
// RS REM        char sana[16];

        if (*oper=='&')
            { strcpy(tulos,s1); strcat(tulos,s2); return; }

        if (strchr(s1,'.')==NULL && strchr(s2,'.')==NULL && *oper!='/')
            {
            l1=atol(s1); l2=atol(s2);
            switch (*oper)
                {
              case '+': l0=l1+l2; break;
              case '-': l0=l1-l2; break;
              case '*': l0=l1*l2; break;
              case '%': l0=l1%l2; break;
                }
            sprintf(tulos,"%ld",l0);
            return;
            }
        tut_arit(s1,s2,oper);
        strcpy(tulos,sbuf);
        }



void sucro_macro(char *s,int old_stack)
        {
        char x[LNAME];
        char y[2];

        x[0]='/';
        if (*s=='W' && s[1]!=EOS && atoi(s+1)>0)  // 28.11.2000
            read_from_stack(s+1,x+1);
        else
            strcpy(x+1,s);
        parm[0]=x; g=1;
        if (old_stack) { strcpy(y,"@"); parm[1]=y; g=2; }
        op_tutor();
        }

int sucro_key(int k)
        {
        int i;
        char x[11],y[LNAME];
// RS REM        extern char tut_info[];

        strcpy(x,"sucro_key_"); x[9]='0'+k;
        i=hae_apu(x,y); if (!i) return(1);
        *tut_info=EOS;
        sucro_macro(y,1);
        return(1);
        }

void prefix_y()
        {
        int i,m;
        char x[LLENGTH];
        long tutpos2;
        int etu22;


        if (ntut<2 || etuu==1) m=nextkey_editor();
//      if (ntut<2 || etuu==1) m=t_nextkey();  kokeilu 20.9.2000
        else
            {
            if (tuttila[ntut-2]==1)
                {
                m=nextkey_editor();
                tut_sulje(); tutpos2=tutpos;
                tutor=muste_fopen(tutnimi[ntut-2],"r+b");
                if (tutor==NULL) { sur_print("Sucro error!"); WAIT; return; } // RS ADD
                muste_fseek(tutor,(long)tutposi[ntut-2],SEEK_SET);
                putc(m,tutor);
                tut_sulje(); tutposi[ntut-2]=tutpos;
                tutor=muste_fopen(etufile,"rb");
                if (tutor==NULL) { sur_print("Sucro error!"); WAIT; return; } // RS ADD
                muste_fseek(tutor,(long)tutpos2,SEEK_SET);
                }
            else
                {
                tut_sulje(); tutpos2=tutpos;
                tutor=muste_fopen(tutnimi[ntut-2],"rb");
                if (tutor==NULL) { sur_print("Sucro error!"); WAIT; return; } // RS ADD                
                muste_fseek(tutor,(long)tutposi[ntut-2],SEEK_SET);
                etu22=etu2; etu2=tutetu2[ntut-2];
                m=tutch_editor();
                etu2=etu22;
                tut_sulje(); tutposi[ntut-2]=tutpos;
                tutor=muste_fopen(etufile,"rb");
                if (tutor==NULL) { sur_print("Sucro error!"); WAIT; return; } // RS ADD
                muste_fseek(tutor,(long)tutpos2,SEEK_SET);
                }
            }
        if (special)
            {
            i=etu; etu=0; sukro_esto=1; key_special(m); etu=i;
            sukro_esto=0; tutcat("SK"); return;
            }
        *x=(char)m; *(x+1)=EOS;
        tutcat(x);
        }

static int uudelleen;
static char jrivi[LLENGTH];
static int uusi_c;
static char *paikka0; // 20.2.2007

static char *sanahaku2(char *paikka)
        {
        char s[LLENGTH];
        char *p,*q,*pp,*pv,*qv;
        char ch;
        char *pralku, *prloppu;
        int m;
        int k;
// RS REM        int eisp;
        int cc;
        int c_1;

        c_1=c;
        p=z+(r1+r-2)*ed1+c1+c-1;
// printf("\nr1=%d|",r1); getck();
        if (*p!=' ') --p;
        pv=p-1;
        pralku=z+(r1+r-2)*ed1; prloppu=pralku+ed1;
        ch=*pv;
        while (*p==' ' && p>pralku) --p;
        if (p==pralku) return(NULL);
        q=p;

/*      while (*q!=' ' && q>pralku) --q;   25.9.89 */
        while (strchr(" ,:;-=+-*/",*q)==NULL && q>pralku) --q;

        ++q; qv=q;
        k=p-q+1;   /* valmiiden merkkien lkm */
        pp=s;
        while (q<=p) *pp++=*q++;
        *pp=EOS;
        *pv='*';
        p=strstr(paikka,s);
        *pv=ch;
        if (p==NULL)
            {
            if (*s>='a' && *s<='z') *s=(char)toupper((int)*s);
            else if (*s>='A' && *s<='Z') *s=(char)tolower((int)*s);
            else return(NULL);
            *pv='*';
            p=strstr(paikka,s);
            *pv=ch;
            if (p==NULL) return(NULL);
            }
        paikka=p;
        q=p;
        q+=k; qv+=k;  /* 16.4.89 */
        while (*q!=' ' && qv<prloppu)
            {
            *qv++=*q++;
            }
        uusi_c=qv-pralku;
        *qv=' '; /* loppuun space alla olevan sanan osittaiseksi peittämiseksi */
        while (1)
            {
            if (qv>=prloppu) return(paikka);
            edread(s,r1+r-1);
            cc=c3; if (c2<c3) cc=c2;
            write_string(s+c,cc-c+1,'7',r+1,8+c);

            LOCATE(r3+2,1); PR_EINV;
            sur_print("Next word by the space bar. Cancel by DELETE. Accept and stop by ENTER!");
            m=nextch_editor();
            if (m==CODE_BACKSP) { paikka=paikka0-1; uusi_c=0; uudelleen=1; return(paikka); }
            if (m!=' ') paikka0=paikka;
            if (m==CODE_DELETE) { uusi_c=0; uudelleen=1; return(paikka); }

            if (m==' ')
                {
                int rivi1,rivi2,c_2,rivi0;
                char s2[LLENGTH];
                char sana[LLENGTH];
                int len;
                char *p;

//              LOCATE(r3+2,1); PR_EINV;
//              sur_print("Next word by the space bar. Exit by ENTER!                       ");

                rivi1=r1+r-1; rivi0=rivi1;
                c_1=qv-pralku;
                rivi2=(q-z)/ed1+1;
                c_2=(q-z)%ed1;
// getck();
// printf("\nc_1=%d rivi1=%d c_2=%d rivi2=%d",uusi_c,rivi1,c_2,rivi2); getck();
                edread(s,rivi1);    edread(s2,rivi2);
// printf("\ns=%s|",s); getck();
// printf("\ns2=%s|",s2); getck();
                while (1)
                    {
                    p=s+c_1;
//                  *sana=' '; sana[1]=EOS; len=1;
                    *sana=EOS; len=0;
                    while (s2[c_2]==' ') { sana[len]=' '; ++c_2; ++len; }

                    if (s2[c_2]==EOS)
                        {
                        ++rivi2;
                        edread(s2,rivi2);
// printf("\nuusi s2=%s|",s2); getck();
                        c_2=1; *sana=' '; len=1;
                        while (s2[c_2]==' ') ++c_2;
                        if (s2[c_2]==EOS)
                            { uusi_c=c_1; return(NULL); }
                        }
                    while (s2[c_2]!=' ' && s2[c_2]!=EOS)
                        { sana[len]=s2[c_2]; ++c_2; ++len; }
                    if (s2[c_2]==EOS) { s2[c_2]=' '; s2[c_2+1]=EOS; }
                    sana[len]=EOS;
// printf("\nsana=%s|",sana); getck();
                    if (c_1+len>c3)
                        {
                        ++r;
                        if (r>r3) { uusi_c=c_1; return(NULL); }
                        ++rivi1;
                        edread(s,rivi1);
                        c_1=1; c=1;
                        p=s+c_1;
                        strcpy(sbuf,sana+1); strcpy(sana,sbuf); --len;
                        }
                    strncpy(p,sana,len);
// printf("\ns=%s|",s); getck();
                    edwrite(s,rivi1,0);
                    write_string(s+c,cc-c+1,'7',r+1,8+c);
                    c_1+=len;
                    m=nextch_editor();
                    if (m==CODE_BACKSP && rivi1==rivi0)
                        {
                        paikka=paikka0-1; uusi_c=0; uudelleen=1; return(paikka);
                        }
                    if (m==CODE_DELETE && rivi1==rivi0)
                        { paikka0=paikka-1; uusi_c=0; uudelleen=1; return(paikka); }
                    if (m!=' ') { uusi_c=c_1; return(NULL); }


                    }

                }

            else { uusi_c=qv-pralku; return(paikka); }
            }
        }

int pre_j()
        {
//RS        extern char *sanahaku2();
        char *paikka;
        int j;

        j=r1+r-1;
        edread(jrivi,j);
        paikka=paikka0=z; uudelleen=1;
        while (1)
            {
            uudelleen=0;
            paikka=sanahaku2(paikka);
            if (!uudelleen) break;
            if (paikka==NULL) return(1);
            ++paikka;
            edwrite(jrivi,j,0);
            }

        if (uusi_c==0) return(1);
        if (uusi_c>c1+c3-1) return(1);
        c=uusi_c-c1+1;
        return(1);
        }

int tutch_editor()
        {
// RS        int Wdisp();
// RS        int nop();
        int i,m,ch;
        char nimi[LNAME];

        int r_tut,c_tut;

/* 29.4.1991 */
A:      if ((unsigned char)*tut_info==(unsigned char)'_' 
			&& (unsigned char)*(tut_info+3)!=(unsigned char)'_' )   // RS ADD
            {
            if (strncmp(tut_info,"___",3)==0)
                {
                strncpy(tut_info,"ERR",3);
                if (strcmp(error_handler,"CTNUE!")!=0)
                    {
                    ntut=1;
                    tut_continue(error_handler);
                    }
                }
            }
        m=getc(tutor);    
            {
            while (m==TUT_COMMENT_CODE) m=getc(tutor);  /* 12.10.88 */
            if (m==CODE_PRE)
                {
                ch=getc(tutor); ungetc(ch,tutor);
                if (ch=='T') tut_special_code=1;
                }
            }

        if (!feof(tutor) && !tut_loppu)   /* 13.10.88 */
            {
            if (sur_kbhit())  // RS Tämä ei saa päivittää tapahtumaa, jotta nextkey saa luettua sen
                {
                ch=nextkey_editor();
              if (!tut_not_break2)
                switch (ch)
                    {
/* 21.1.2001 */   case '+': --tut_wait_c; if (tut_wait_c<1) tut_wait_c=1;
                            break;
                  case '-': ++tut_wait_c; break;
/****************************
                  case '+': --etu1; if (etu1<=0) etu1=1; break;
                  case '-': ++etu1; if (etu1>40) etu1=40; break;
******************************/
                  case '*': --etu1; if (etu1<=0) etu1=1; break;
                  case '/': ++etu1; if (etu1>40) etu1=40; break;

                  case '&': ei_odotusta=1;
/*                case '*': etu1=0; break;     23.10.89 */
                  case '.':
                            soft_vis=1; restore_display(1); // 24.10.2007
                            r_tut=r; c_tut=c;
                            prompt_clear();
                            cursor(r3+1,1); ERASE; PR_EBLD;
                            strcpy(nimi,"N");
                            etu=0;
                            prompt_editor("Stop the current sucro (Y/N) ? ",nimi,32);
                            etu=2; PR_ENRM;
                            if (*nimi!='Y' && *nimi!='y')
                                { r=r_tut; c=c_tut;
                                  prompt_line=NULL;
                                  cursor(r,c); disp(); break;
                                }

                            tut_soft_restore();
                            if (*break_sucro) /* 30.3.90 */
                                {
                                ntut=1;
                                tut_continue(break_sucro); break;
                                }
                            prompt_line=NULL; // 30.8.2001
                            disp_prompt_line("");

                            tutclose();
                            return(0);
                  case CODE_HELP:
                            help_on=1; ei_odotusta=0;
                            etu2=2; if (etu1<2) etu1=2; break;

                  case ',': sucro_pause=1;   // uusittu 21.8.2004
                            break;

                  default: if (tut_not_break) break;
                           nextkey_editor(); break;   /* 23.10.89 */   //RS Mitä tämä tekee???

                    }
                }
            special=0;
        if (etu1>1 && !tut_special_code) sur_wait((long)tut_wait_c*etu1,nop,0);

// if (m==CODE_SOFT_ON || m==CODE_WORDS) { printf("\nm=%d|",m); getck(); }
//Rprintf("\nm: %d",m);
            switch (m)
                {
              case CODE_EXIT:
              case CODE_RIGHT:
              case CODE_LEFT:
              case CODE_UP:
              case CODE_DOWN:
              case CODE_HOME:
              case CODE_INSERT:
              case CODE_DELETE:
              case CODE_ERASE:
              case CODE_NEXT:
              case CODE_PREV:
              case CODE_EXEC:
              case CODE_DISP:
              case CODE_PRE:
              case CODE_TOUCH:
              case CODE_DISK:
              case CODE_RETURN:
              case CODE_BACKSP:
              case CODE_REF:
              case CODE_MERGE:
              case CODE_COPY:
              case CODE_TAB:
              case CODE_TABS:
              case CODE_CODE:
              case CODE_SRCH:
              case CODE_ACTIV:
              case CODE_MOVE:
              case CODE_END:
              case CODE_HELP:
              case CODE_WORDS:
              case CODE_SOFT_ON:
                    special=1;
                    break;
              case CODE_INSERTL:              // 1.8.2000
              case CODE_DELETEL:
                    special=1; 
                    // RS REM ?!? sur_sleep(10);
                    break;

              case TUT_EFFECTS_OFF: etu2=0; etu3=0; m=255; break;

              default:
                    special=0;
                    break;
                }

            if (etu2>0)
                {
                if (tut_special_code) return(m);
                if(etu1<2) return(m); // 17.7.2000
                CURSOR_OFF; cursor(r3+1,c3-16);

                if (etu2!=1 && m==CODE_EXEC)
                    {
                    PR_EBLK; strcpy(sbuf,"Activate!");
                    }
                else
                    {
                    if (etu2==1) sur_print("      ");
                                 else { PR_EBLK; sur_print("Press "); }
                    PR_EINV;

                    label(m,nimi); sprintf(sbuf,"%s",nimi);

                    }
                sur_print(sbuf);
                PR_ENRM; cursor(r,c); CURSOR_ON;
                if (etu2==1)
                  { if (etu1>1) sur_wait((long)(4*tut_wait_c)*etu1,Wdisp_editor,0); }
                else press_key(m);
                CURSOR_OFF; cursor(r3+1,c3-16);
                sur_print("              ");
                cursor(r,c); cursor_on();
                }

            return(m);
            }
        tut_loppu=0;
        muste_fclose(tutor);
        --ntut;
        if (ntut>0)
            {
            etu=tuttila[ntut-1];
            tutalku=tutalut[ntut-1];  /* 10.2.90 */
            strcpy(etusukro,tutmemb[ntut-1]); // 14.11.2004
            strcpy(etufile,tutnimi[ntut-1]);
            if (etu==1)
                tutor=muste_fopen(etufile,"r+b");
            else
                tutor=muste_fopen(etufile,"rb");
            if (tutor==NULL)
                { sprintf(sbuf,"\nSucro %s not found!",etufile);
                  sur_print(sbuf); WAIT; return(-1); }
            i=muste_fseek(tutor,(long)tutposi[ntut-1],SEEK_SET);
            if (i!=0) { tutclose(); return(0); }
            etu2=tutetu2[ntut-1]; /* 10.11.88 */
            if (etu==1) return(0);
            else goto A;
            }

        etu=0; ntut=0; disp_all(); *op_sana=EOS; return(0);
        }


int tut_special_editor()
        {
// RS        int Wdisp();
        int m,ar,ac,ch,i,h,k;
        long l;
        char *p; // RS REM ,*q;
        char sana[LLENGTH];
        char jatko[LLENGTH];
        char *s1,*s2;
        double da;
        double dx,dy;
        extern char soft_stack_file[];
        extern int r_mouse,c_mouse;

//Rprintf("tut_special, etu=%d",etu);

        *sana=EOS; /* 11.12.1998 */
        switch(etu)
            {
          case 0:
            if (sukro_esto) break;
            soft_disp(0);
            cursor(r3+1,1); PR_EBLD;
            sur_print("Sucro functions: S=Start definition, R=Run ? ");
            ch=getck();
            switch (ch)
                {
              case 'S':
              case 's':
                cursor(r3+1,1); ERASE; PR_EBLD;
 /* sprompt */  prompt_editor("Define a sucro: Name of file ? ",sana,32);

                disp();
                soft_disp(0);
                if (*sana==EOS) break;
                i=tutopen(sana,"wb");
                if (i==0) { etu=0; soft_disp(1); break; }
             /* cursor(r3+1,1); ERASE; */
                etu=1; ntut=1;
                lue_hetki(&wait_hetki);
                break;
              case 'R':
              case 'r':
                cursor(r3+1,1); ERASE; PR_EBLD;
                prompt_editor("Run a sucro: Name of file ? ",sana,32);
                disp();
                i=tutopen(sana,"rb");
                if (i==0) { etu=0; soft_disp(1); break; }
           /*   cursor(r3+1,1); ERASE;  */
                etu=2; etu1=2; ntut=1;  /* ntut=1 lisätty 13.2.89 */
                break;
              default: etu=0; disp_all(); break; // 2.5.2002

                }  /* switch (ch) */
            PR_ENRM;
            cursor(r3+1,1); ERASE;
            break;

          case 1:
            cursor(r3+1,1); PR_EBLD;
            sur_print(s_functions);
            ch=getck();

            if (ch=='E' || ch=='e')
                {
                tutsave(255);
                tutclose();
                disp_all();  /* 26.5.1999 */
                }

            else if (ch=='C' || ch=='c')
                {
                cursor(r3+1,1); ERASE; PR_EBLD;
                prompt_editor("Control word ? ",sana,64);
                disp();
                if (*sana=='X') { *sana=(char)TUT_EFFECTS_OFF;
                                  sana[1]=EOS; muste_fseek(tutor,(long)-2L,1); }

                for (i=0; i<strlen(sana); ++i) tutsave((int)sana[i]);
                }

            else if (ch=='R' || ch=='r')   /* Repeat ad infinitum */
                {
                tutsave(254);
                tutsave(255);
                muste_fclose(tutor);
                tutor=muste_fopen(etufile,"rb");
                etu=2; ntut=0;
                }
     /*     cursor(r3+1,1); ERASE; */ PR_ENRM;
            break;

          case 2:
            tut_loppu=0;
            m=tutch_editor();
            switch (m)
                {
              case 'W':
                read_tutword(sana);
                if (etu1>1 && etu2!=2)
                    {
                    if (sucro_pause) tut_pause(); // 21.8.2004
                    else sur_wait((long)(5*tut_wait_c)*atoi(sana)*etu1,Wdisp_editor,1);
                    }
                else { sur_sleep(10); Wdisp_editor(); }
                break;
              case 'L':
                read_tutword(sana);
/* 17.9.1995 */ if (*sana=='?')
                    {
                    read_from_stack(sana+1,sana);
                    }

                tut_continue(sana);
                break;
              case 's':
                read_tutword(sana);
                if (etu1>1) etu1=atoi(sana);
                break;
              case 'a':
                read_tutword(sana);
                if (etu1>0) { etu1-=atoi(sana); if (etu1<0) etu1=0; }
                break;
              case 't':   /* ehdoton nopeuden muutos */
                read_cond(sana);
                etu1=atoi(sana);
                break;
              case 'D':
                read_tutword(sana);
                if (!help_on) etu2=atoi(sana);
                break;
              case 'P':
                read_tutword(sana); ar=atoi(sana);
                read_tutword(sana); ac=atoi(sana);
                cursor(ar,ac);
                read_tutword(sana);
                PR_EBLD; sprintf(sbuf,"%s",sana); sur_print(sbuf); PR_ENRM;
                cursor(r,c);
                m=getc(tutor);
                cursor(r,c);
                press_key(m);
                displine2(r1+ar-1);
                ungetc(m,tutor);
                break;
              case 254:
                rewind(tutor);
                break;
              case 255:
                tut_loppu=1; break;
              case 'M':  /* äänet puuttuvat */
                read_tutword(sana); break;
              case 'V': /*  V<aika>@123@<G|L>jatko1@<G|L>jatko2@<G|L>jatko3@ */
                soft_disp(0);
                read_cond(sana); l=atol(sana)*(long)(10*tut_wait_c);
                if (l==0L) l=(long)(6000L*tut_wait_c);
                read_tutword(sana);
                if (ei_odotusta) k=0;
                else k=sur_wait2(l,Wdisp_editor);
                if (k)
                    {
                    sucro_menu=1;
                    ch=nextkey_editor();
                    sucro_menu=0;
                    if (ch<0)
                        {
                        ch=mouse_key_select(r_mouse,c_mouse);
                        }
                    if (ch=='.' && !tut_not_break2) /* && 15.1.1996 */
                        {
                        prompt_clear();
                        if (*break_sucro)
                            { ntut=1; tut_continue(break_sucro); return(0); }
                        else
                            {
                            tut_soft_restore();
                            tutclose(); return(0);
                            }
                        }

                    p=strchr(sana,(char)ch);
                    if (p==NULL) k=0; else k=p-sana;
                    }
                /* else k=0; */
                h=strlen(sana);
                tut_select(h,k);
                soft_disp(1);
                break;
              case 'I':  /*  I<N|A><c1>@<c2>@<less>@<equal>@<greater>@  */
                i=getc(tutor);  /* N tai A */
                read_cond(sana); s1=sana;
                read_cond(jatko); s2=jatko;
                if (i=='N')
                    {
                    da=atof(s1)-atof(s2);
                    if (da<0.0) k=0;
                    else
                        {
                        if (da==0.0) k=1; else k=2;
                        }
      /* ei enää    k=numcomp(s1,s2);   vältetään atof() tilankäyttö! */
                    }
                else if (i=='A')
                    {
                    i=strcmp(s1,s2);
                    if (i<0) k=0; else if (i==0) k=1; else k=2;
                    }
                else { tut_virhe(1); break; }
                tut_select(3,k);
                break;


 //  - if 2*W1 <> W2+5 then goto A
 //  *²GTi/2*W1/@/W2+5/@GA@@GA@


              case 'i':  /*  i/expr1/@/expr2/@<less>@<equal>@<greater>@  */
                read_tutword(sana);
                read_tutword(jatko);
// printf("\ncase i: %s %s|",sana,jatko); getck();
                tut_laske2(sana,&dx);
                tut_laske2(jatko,&dy);
// printf("\nvert: %g %g|",dx,dy); getck();
                da=dx-dy;
                if (da<0.0) k=0;
                else
                    {
                    if (da==0.0) k=1; else k=2;
                    }
                tut_select(3,k);
                break;

              case 'J':  /*  J<nro>@val1@val2@...@valn@@j1@j2@...   */
                read_cond(sana); s1=sana;
                k=0;
                while (1)
                    {
                    read_tutword(jatko);
                    if (*jatko==EOS) break;
                    if (strcmp(jatko,s1)==0) break;
                    ++k;
                    }
                h=k; while (*jatko) { read_tutword(jatko); ++h; }
                tut_select(h+1,k);
                break;

              case 'Z':
                read_tutword(sana);
                h=atoi(sana)-1;
                i=0; p=tut_info;
                while (i<h && *p!=EOS)
                    {
                    if (*p=='@') ++i;
                    ++p;
                    }
                if (i==h) *p=EOS;
                break;

              case 'K':
                soft_disp(0);
                tut_question();
                tut_index=0;
                soft_disp(1);
                break;
              case 'B':
                read_tutword(sana);
                tut_not_break=atoi(sana);
                break;
              case 'b':
                read_tutword(sana);
                tut_not_break2=atoi(sana);
                break;
              case 'X':  /*  label */
                read_tutword(sana);
                break;
              case 'O':
                i=strlen(survo_id); strcpy(sana,survo_id+i+1);
                p=sana; while (*p) { *p-=19; ++p; }
                strcat(tut_info,sana); strcat(tut_info,"@");
                break;
              case 'C':  /* C<string>@<tutword>@<jump_if_not_found>@
                            If at least one character in <string> appears in
                            <tutword>, OK, else <jump..>
                         */
                read_tutword(sana);
                read_cond(jatko);
                p=sana;
                i=0; while (*p)
                   { if (strchr(jatko,*p)!=NULL) { i=1; break; } ++p; }
                if (i==1) read_tutword(sana);
                break;
              case 'p':  /* p<text_to_the_bottom_line>@ */
              case 'q':  /* q<tut_word_index>@ */        /* 30.4.91 */
                if (m=='p') read_tutword(sana);
                else read_cond(sana);
                sana[EC3+8]=EOS; strcpy(prompt_space,sana);  /* 23.2.93 */
                if (*prompt_space) prompt_line=prompt_space;
                else prompt_line=NULL;
                disp();
                break;
              case 'g':  /* g<first_row>@<row>@<column>@ */
                read_cond(sana);
                read_cond(jatko);
                read_cond(jatko+32);
                parm[1]=sana; parm[2]=jatko; parm[3]=jatko+32;
                if (*parm[3]!='!')
                    op_goto2(4,parm);
                else     /* g<first_row>@<row>@C!@<first_column>@<column>@ */
                    {
                    read_cond(jatko+32); /* parm[3]=jatko+32; */
                    read_cond(jatko+64); parm[4]=jatko+64;
                    op_goto2(5,parm);
                    }
                disp();
                break;
              case 'k':
                if (sur_kbhit()) *sana='1'; else *sana='0';
                sana[1]='@'; sana[2]=EOS; strcat(tut_info,sana);
                break;
              case 'l':
                *sana=(char)getck();
                sana[1]='@'; sana[2]=EOS; strcat(tut_info,sana);
                break;
              case '=':   /* =<tutword>@<tutword>@<op>@<tutword>@  */
                read_tutword(sana); i=atoi(sana);
                read_cond(jatko);
                read_tutword(jatko+64);
                read_cond(jatko+128);
                tut_laske(sana,jatko,jatko+128,jatko+64);
                tut_set(sana,i);
                break;
              case '!':   /* !<tutword>@<tutword>@  */
                read_tutword(sana); i=atoi(sana);
                read_cond(sana);
                tut_set(sana,i);
                break;
              case 'v': // vV1@2@ tai v1@V2@ tai vV1@V2@ "softstack"
                        // {soft V1=W2} {soft W1=V2} {soft V1=W2}
                read_tutword(sana);
                if (*sana=='V')
                    {
                    i=atoi(sana+1);
                    read_tutword(sana);
                    if (*sana=='V')
                        read_from_soft_stack(sana+1,sbuf);
                    else
                        read_from_stack(sana,sbuf);
                    soft_stack_set(sbuf,i);
                    soft_stack_save_load(1,soft_stack_file);
          //        disp_all();
                    }
                else
                    {
                    i=atoi(sana);
                    read_tutword(sana);
                    if (*sana=='V')
                        read_from_soft_stack(sana+1,sbuf);
                    else
                        read_from_stack(sana,sbuf);
                    tut_set(sbuf,i);
                    }
                break;

              case 'c':   /* c<tutword>@  seek char */
                read_cond(sana);
                seek_char2((int)*sana);
                break;
              case 'w':   /* w<tutword>@ seek string  17.5.92 */
                read_cond(sana);
                seek_string(sana);
                break;
              case '>':   /* >file@   save stack <file>  */
                read_cond(sana); // 21.8.2004
                i=stack_save_load(1,sana); if (i<0) return(0); break;
              case '<':   /* <file@   load stack <file>  */
                read_cond(sana); // 21.8.2004
                i=stack_save_load(2,sana); if (i<0) return(0); break;
              case 'E':      /* 29.4.91 */
                read_tutword(error_handler);
                break;
              case 'S': // {sound <file>}
              case 'T':  // T<tut_word_index>@ */        /* 30.4.91 */
                if (m=='S') read_tutword(sana);
                else read_cond(sana);
                tut_sound(sana);
                break;
              case 'y':  // 13.6.2005
// printf("\n{save spec2 Wx Wy}"); getck();
                read_cond(sana);
// printf("\nsana=%s|",sana); getck();
                tut_specification2(sana);
                read_tutword(jatko);
                tut_set(sana,atoi(jatko));
// printf("\nsana=%s|",sana); getck();
                break;
              default: etu=0; soft_disp(1); break;  /* 25.2.90 */
                }
            }
        tut_special_code=0;
        return(1);
        }


