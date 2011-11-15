/*  cq.c 26.12.1985/SM (9.5.1993) (6.7.1996)
*/
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define RIVITILA 10000
#define MUISTI 200
#define MULEN 8
        /* -31.3.93 MULEN 3 */
#define SYSMAX 9

/* char edq[LNAME]; - 9.5.93 */
static char *pedq[SYSMAX];
static int sysmax=1;
static int isys;
static char syslist[LLENGTH];

static char edq2[LNAME];
static char hakusana[LLENGTH];
static char avainrivi[LLENGTH];
static char avainsana[LNAME];
static char malli[LLENGTH];
static char valinta[2*LLENGTH];

static int qed1,qed2;   /* hakusanakent„n koko */
static int qrivi1,qrivi2; /* hakuv„li hakukent„ss„ */
static int jmin,jmax; /* jatkohakuehdotukset */
static int ted1,ted2;  /* tekstikent„n koko */
static int tila; /* 1=haku                      */
static int rdisp; /* 1. n„ytt”rivi */
static int ndisp; /* n„ytt”rivien lukum */
static int mdisp; /* n„yt”n rivilaskuri */

static char rivit[RIVITILA];
static int rivitila, nrivit, ntila, oline;

static char mufile[MULEN*MUISTI];  /* askelmuisti */
static char muf[MULEN+1];
static int murivi[MUISTI];
static int musys[MUISTI]; /* 9.5.93 */
static char musana[MUISTI][12]; // 23.11.2000
static int nmu, muisti, mulen;

static FILE *keywords;
static FILE *text;
static char *info2;    /* qpath */
static char qprefix[4];
static int space_break2;
static char vmerkit[]=" .:;,()[]{}?%!\"\\$œ=+-*'<>~_`";  // | ja / poistettu 27.11.2000
static int ei_saa_kirjoittaa=0;
static char argv1[16];
static int own_window=0;
static int font_x,font_y,home_x,home_y;

// RS REM extern int mouse_in_use;
// RS REM extern char os_ver[];

void muste_help(int argc, char *argv[])
        {
        int i,len,m;
        char x[LLENGTH];
        char *p;
        char *osa[4];

        s_init(argv[1]);

/* RS REM
        shadow_code[(int)'0'+4]=(int)'/'-16;

        if (strcmp(info_2,"NEW_WINDOW")==0)
            {
            
            c3=72; r3=23;
            own_window=1;
            if (!etu) mouse_in_use=1;


            }
        else { labels(); }

        if (own_window)
            {
*/            
            sprintf(sbuf,".muste.help.init(\"%s - Help Window\")",system_name); // RS CHA
            
            muste_evalr(sbuf); // RS ADD

/* RS REM
strcpy(x,survo_path); i=strlen(x)-1; if (x[i]=='\\') x[i]=EOS;
            sur_console_init1(sbuf,x);

            i=hae_apu("help_font",x);
            if (i==0) i=hae_apu("edit_font",x);
            if (i>0)
                {
                i=split(x,osa,4);
                font_x=atoi(osa[0]); font_y=atoi(osa[1]);
                home_x=home_y=0;
                if (i==4) { home_x=atoi(osa[2]); home_y=atoi(osa[3]); }
                i=r_soft; r_soft=0;
                sur_small_screen_font_modify(&font_x,&font_y);
                sur_sleep(25);
     sur_resize_in_given_font(sbuf,font_x,font_y,home_x,home_y);
                           // sbuf muodostettu jo yll„!
                sur_sleep(25);
     sur_resize_in_given_font(sbuf,font_x,font_y,home_x,home_y);
                r_soft=i;
                }
            }
        else
            {
            sprintf(sbuf,"%s - Help",system_name);
            sur_set_console_title(sbuf);
            }
*/  
#if 0
        strcpy(argv1,argv[1]); /* 6.7.1996 */
// RS NYI        tut_init();
        space_break2=space_break;
        space_break=0;
        rivitila=RIVITILA;
        oline=r1+r; /* tulostusrivi (load_lines) */
        nmu=0; muisti=MUISTI; mulen=MULEN;
        *edq2=EOS;
        info2=strchr(info,'>'); if (info2==NULL) return;
        *info2=EOS; ++info2;
/*      i=strlen(info2)-3;
        strcpy(qprefix,info2+i); info2[i]=EOS;
- 9.5.93 */

        if (*info==EOS)
            {
            strcpy(hakusana,word[0]);
            len=strlen(hakusana);
            strupr(hakusana);
            p=strchr(hakusana,'?'); if (p!=NULL) *p=EOS;
            }
        else strcpy(hakusana,info);
        if (strcmp(hakusana,"???")==0)
            {
            edread(x,r1+r-1);
            i=c1+c-1;
            if (x[i]==' ' && x[i-1]==' ') strcpy(hakusana,"SURVO");
            else
                {
                i=nykyinen_sana(x,c1+c-1,hakusana); if (i<0) return;
                }
            ei_saa_kirjoittaa=1;
            }

        strcat(hakusana," ");
        i=strlen(info2); while (info2[--i]==' ') info2[i]=EOS;
        strcpy(syslist,info2);
        sysmax=split(syslist,pedq,SYSMAX);
        isys=0;
/*      strcpy(edq,info2); strcat(edq,qprefix); strcat(edq,".EDT");
        i=avaa(edq); if (i<0) return;
-9.5.93 */
        i=avaa(isys); if (i<0) return;

        luo_ikkuna();
        tila=1;

        while (tila)
            switch (tila)
                {
              case 1: etsi(hakusana); break;
              case 2: i=nayta();
                      if (i==3) { tila=1; i=lyhenna(hakusana); if (!i) tila=0; break; }
                      if (i==2) { tila=5; ++nmu; }
                      break;
              case 3: i=hae(avainsana); if (i<0) { tila=0; break; }
                      tdisp(i); break;
              case 4: monivalinta(); break;
              case 5: i=ota_muistista(); if (i<0) { tila=0; break; }
                      text_open(muf);
                      tdisp(i); break;
              case 6: vaihda_sys(); break;
              case 7: uusi_sys(); break;
                }


        fclose(keywords);
        if (text!=NULL) fclose(text);
           /* 31.3.93 */
/* RS REM
        if (own_window)
            {
            sur_set_message("-",2);
            return;
            }
        space_break=space_break2;
        kopioi_malli();
*/
#endif
// RS NYI        tut_end();
        s_end(argv[1]);
// RS REM        exit(0);
        }

#if 0
static int nykyinen_sana(xhar *x,int pos,char *hakusana)
        {
  /*    char x[LLENGTH];    */
        char *p,*q;

/*      edread(x,r1+r-1);
        p=x+c1+c-1;
*/
        p=x+pos;

        if (vmerkki(*p))
            {
            while (p>x && vmerkki(*p))  --p;
            if (p==x) { *hakusana=EOS; return(-1); }
            }
        q=p;
        while (*q!=EOS && !vmerkki(*q)) ++q; *q=EOS;
        q=p;
        while (q>x && !vmerkki(*q)) --q;
        strcpy(hakusana,q+1);
        strupr(hakusana);
        if ((unsigned int)*hakusana>127) *hakusana=EOS; // 23.1.2001
// printf("\nhakusana=%s|",hakusana); getch();
        return(1);
        }

static int vmerkki(char ch)
        {
        if (strchr(vmerkit,ch)!=NULL) return(1);
        return(0);
        }


get_console_name() { return(1); }  // konsolikirjaston
disp_all() { return(1); }          // k„ytt„m„tt”mi„ funktioita

new_start()
    {
    int i;
    char x[LLENGTH];

// printf("info_2=%s|",info_2); getch();
    if (strcmp(info_2,"-")==0) { tila=0; return(0); }
    strcpy(hakusana,info_2); strcat(hakusana," ");
    tila=1;
    return(0);
    }


lyhenna(s)
char *s;
        {
        int i;
        i=strlen(s); if (i==0) return(0);
        s[1]=' '; s[2]=EOS;
        return(1);
        }

avaa(isys)
int isys;
        {
        int i;
        char rivi[ELE], *sana[3];
        char edq[LLENGTH];

        strcpy(info2,pedq[isys]);
        i=strlen(info2)-3;
        strcpy(qprefix,info2+i); info2[i]=EOS;
        strcpy(edq,info2); strcat(edq,qprefix); strcat(edq,".EDT");

        if (keywords!=NULL) fclose(keywords);
        keywords=fopen(edq,"rb");
        if (keywords==NULL)
            {
            PR_EBLD;
            sprintf(sbuf,"\nInquiry system missing on path %s%s!",info2,qprefix); sur_print(sbuf);
            WAIT; PR_ENRM; return(-1);
            }
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(keywords);
        rivi[ELE-1]=EOS;
        i=split(rivi,sana,3);
        qed1=atoi(sana[1]); qed2=atoi(sana[2]);

        qedread(rivi,2);
        i=split(rivi+1,sana,2);
        qrivi1=atoi(sana[0]); qrivi2=atoi(sana[1]);
/*
printf("\n qed1=%d qed2=%d qrivi1=%d qrivi2=%d",qed1,qed2,qrivi1,qrivi2);
getch();
*/
        return(1);
        }

qedread(s,j)
char *s;
int j;
        {
        int i;

        fseek(keywords,(long)(j*qed1),0);
        for (i=0; i<qed1; ++i) s[i]=(char)getc(keywords);
        s[qed1]=EOS;
        }

tedread(s,j)
char *s;
int j;
        {
        int i;

        fseek(text,(long)(j*ted1),0);
        for (i=0; i<ted1; ++i) s[i]=(char)getc(text);
        s[ted1]=EOS;
        }

luo_ikkuna()
        {
        rdisp=r+1;
        if (r==r3) { SCROLL_UP(1,r3,1); rdisp=r; }
        ndisp=r3-rdisp+1;
        }

etsi(s)
char *s;
        {
        int j;
        int alku, loppu;
        int vert, len;

        if (*s==' ') { jmin=qrivi1+1; jmax=qrivi2-1; tila=4; return; }
        alku=qrivi1; loppu=qrivi2;
        len=strlen(s);
        while (1)
            {
            if (loppu-alku<=1) { jmin=alku; jmax=loppu; break; }
            j=(alku+loppu)/2;
            qedread(avainrivi,j);
/*  printf("\nalku=%d loppu=%d %s",alku,loppu,avainrivi+1); getch();    */
            vert=strncmp(s,avainrivi+1,len);
            if (vert==0) { jmin=j; jmax=j; break; }
            if (vert<0) loppu=j; else alku=j;
            }
//  printf("\njmin=%d jmax=%d",jmin,jmax); getch();
        if (jmin==jmax) { tila=2; return; }
        tila=4; return;
        }

kopioi_malli()
        {
        char x[LLENGTH];

        if (ei_saa_kirjoittaa) return;
        if (*malli==EOS) return;
        edread(x,r1+r-1);
        if (!empty_h(x+c1+c-1)) return;
        x[1]=EOS; strcat(x,malli);
        edwrite(x,r1+r-1,0);
        }

empty_h(s)
char *s;
        {
        while (*s) { if (*s!=' ') return(0); ++s; }
        return(1);
        }

vaihda_sys()
        {
        int i;

        if (isys<sysmax-1) ++isys;
        i=avaa(isys); if (i<0) exit(1);
        tila=1;
        return(1);
        }

uusi_sys()
        {
        int i,m;

        tila=1;
        m=nextch("");
        i=m-'0'-1;
        if (i<0 || i>sysmax-1) return(1);
        isys=i;
        i=avaa(isys); if (i<0) exit(1);
        return(1);
        }

#endif
