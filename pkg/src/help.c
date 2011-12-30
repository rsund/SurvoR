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

static int mouse_in_use=0; // RS CHA extern -> static with init 0
// RS REM extern char os_ver[];

static int samat;
static int nruutu;

static int n_ex, j_help;
static char example[20][LNAME]; // 19.11.2002
static int example_line[20];
static char uusi_hakusana[32];

static int riv_lis=0;
static FILE *lines;

static int row,col;

static int mouse_click=FALSE; // RS ADD


extern int sur_set_message();
static int nykyinen_sana(char *x,int pos,char *hakusana);
static int vmerkki(char ch);
static int new_start();
static int lyhenna(char *s);
static int avaa(int isys);
static void qedread(char *s,int j);
static void tedread(char *s,int j);
static void luo_ikkuna();
static void etsi(char *s);
static void kopioi_malli();
static int empty_h(char *s);
static int vaihda_sys();
static int uusi_sys();
static int nayta();
static int text_open(char *s);
static int hae(char *s);
static int tdisp(int j);
static int dclear();
static int mouse_help_lines();
static int tdisp2(int j);
static int show_example();
static void kasvata_ikkunaa();
static int C_koodi();
static int E_koodi();
static int G_koodi(char *s);
static void valinnat(int j);
static int aktivoi(char *uusi);
static int etsim(int m,char *uusi);
static void vie_muistiin(int j);
static int ota_muistista();
static int mouse_help(char *sana);
static void uusi_haku();
static void putsaa();
static int monivalinta();
static int fit(int j);
static void tulosta(char *x);
static int load_lines();
static int pidenna(char *s,int len);
static void print_lines();
static void slab_muunnos(char *s);
static void disp_tied_nimi(char *s);
static void help();
static int insert_lines(int jj,int k);
static void print_varjorivi(char *rivi,int j,char *varjo);
static void qdisp_mode(int dispm);
static int kur2(int m,char *sana);
static void label2(int m,char nimi[]);


void muste_help(int argc, char *argv[])
        {
        int i,len,m;
        char x[LLENGTH];
        char *p;
        char *osa[4];
        unsigned char old_shadow_code; // RS ADD

		sysmax=1;
		isys=0;
		qed1=qed2=0;   
		qrivi1=qrivi2=0;
		jmin=jmax=0;
		ted1=ted2=0;
		tila=0;
		rdisp=0;
		ndisp=0;
		mdisp=0;
		rivitila=nrivit=ntila=oline=0;
		nmu=muisti=mulen=0;
		keywords=NULL;
 		text=NULL;
 		info2=NULL;
		space_break2=0;
		ei_saa_kirjoittaa=0;
		own_window=0;
		font_x=font_y=home_x=home_y=0;
		mouse_in_use=0;
		samat=0;
		nruutu=0;
		n_ex=j_help=0;
		riv_lis=0;
 		lines=NULL;
		row=col=0;
		mouse_click=FALSE;
			
	    s_init(argv[1]);

		old_shadow_code=shadow_code[(int)'0'+4]; // RS ADD
        shadow_code[(int)'0'+4]=(int)'/'-16;

//Rprintf("\ninfo: %s\ninfo_2: %s\nword[0]: %s",info,info_2,word[0]);

        if (strcmp(info_2,"NEW_WINDOW")==0)
            {
            
            c3=72; r3=23;
            own_window=1;
            if (!etu) mouse_in_use=1;


            }
        else { labels(); }

        if (own_window)
            {
            
            sprintf(sbuf,".muste.help.init(\"%s - Help Window\")",system_name); // RS CHA            
            muste_evalr(sbuf); // RS CHA

/* RS REM
strcpy(x,survo_path); i=strlen(x)-1; if (x[i]=='\\') x[i]=EOS;
            sur_console_init1(sbuf,x);
*/            

            i=hae_apu("help_font",x);
            if (i==0) i=hae_apu("edit_font",x);
            if (i>0)
                {
                i=split(x,osa,4);
                font_x=atoi(osa[0]); font_y=atoi(osa[1]);
                home_x=home_y=0;
                if (i==4) { home_x=atoi(osa[2]); home_y=atoi(osa[3]); }
                i=r_soft; r_soft=0;
Rprintf("\nFIXME: help font control missing!");                
/* RS REM                
                sur_small_screen_font_modify(&font_x,&font_y);
                sur_sleep(25);
     sur_resize_in_given_font(sbuf,font_x,font_y,home_x,home_y);
                           // sbuf muodostettu jo yll„!
                sur_sleep(25);
     sur_resize_in_given_font(sbuf,font_x,font_y,home_x,home_y);
*/     
                r_soft=i;
                }
            }
        else
            {
            sprintf(sbuf,"%s - Help",system_name);
            sur_set_console_title(sbuf);
            }
 
        strcpy(argv1,argv[1]); /* 6.7.1996 */
        tut_init();
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
            muste_strupr(hakusana);
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


        muste_fclose(keywords);
        if (text!=NULL) muste_fclose(text);
           /* 31.3.93 */

        if (own_window)
            {
            sur_set_message("-",2);
            return;
            }
        space_break=space_break2;
        kopioi_malli();

        tut_end();
		shadow_code[(int)'0'+4]=old_shadow_code; // RS ADD
        s_end(argv[1]);
// RS REM        exit(0);
        }

static int nykyinen_sana(char *x,int pos,char *hakusana)
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
        muste_strupr(hakusana);
        if ((unsigned int)*hakusana>127) *hakusana=EOS; // 23.1.2001
// printf("\nhakusana=%s|",hakusana); getch();
        return(1);
        }

static int vmerkki(char ch)
        {
        if (strchr(vmerkit,ch)!=NULL) return(1);
        return(0);
        }


// RS REM get_console_name() { return(1); }  // konsolikirjaston
// RS REM disp_all() { return(1); }          // k„ytt„m„tt”mi„ funktioita

static int new_start()
    {
    int i;
    char x[LLENGTH];

// printf("info_2=%s|",info_2); getch();
    if (strcmp(info_2,"-")==0) { tila=0; return(0); }
    strcpy(hakusana,info_2); strcat(hakusana," ");
    tila=1;
    return(0);
    }


static int lyhenna(char *s)
        {
        int i;
        i=strlen(s); if (i==0) return(0);
        s[1]=' '; s[2]=EOS;
        return(1);
        }

static int avaa(int isys)
        {
        int i;
        char rivi[ELE], *sana[3];
        char edq[LLENGTH];

        strcpy(info2,pedq[isys]);
        i=strlen(info2)-3;
        strcpy(qprefix,info2+i); info2[i]=EOS;
        strcpy(edq,info2); strcat(edq,qprefix); strcat(edq,".EDT");

        if (keywords!=NULL) muste_fclose(keywords);
        keywords=muste_fopen(edq,"rb");
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

static void qedread(char *s,int j)
        {
        int i;

        muste_fseek(keywords,(long)(j*qed1),0);
        for (i=0; i<qed1; ++i) s[i]=(char)getc(keywords);
        s[qed1]=EOS;
        }

static void tedread(char *s,int j)
        {
        int i;

        muste_fseek(text,(long)(j*ted1),0);
        for (i=0; i<ted1; ++i) s[i]=(char)getc(text);
        s[ted1]=EOS;
        }

static void luo_ikkuna()
        {
        rdisp=r+1;
        if (r==r3) { SCROLL_UP(1,r3,1); rdisp=r; }
        ndisp=r3-rdisp+1;
        }

static void etsi(char *s)
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

static void kopioi_malli()
        {
        char x[LLENGTH];

        if (ei_saa_kirjoittaa) return;
        if (*malli==EOS) return;
        edread(x,r1+r-1);
        if (!empty_h(x+c1+c-1)) return;
        x[1]=EOS; strcat(x,malli);
        edwrite(x,r1+r-1,0);
        }

static int empty_h(char *s)
        {
        while (*s) { if (*s!=' ') return(0); ++s; }
        return(1);
        }

static int vaihda_sys()
        {
        int i;

        if (isys<sysmax-1) ++isys;
        i=avaa(isys); if (i<0) { sur_print("\nCannot open sys!"); return(-1); } // RS CHA exit(1);
        tila=1;
        return(1);
        }

static int uusi_sys()
        {
        int i,m;

        tila=1;
        m=nextch("");
        i=m-'0'-1;
        if (i<0 || i>sysmax-1) return(1);
        isys=i;
        i=avaa(isys); if (i<0) { sur_print("\nCannot open sys!"); return(-1); } // RS CHA exit(1);
        return(1);
        }

static int nayta()
        {
        int i;
        char *sana[3];

        nruutu=0;
        i=split(avainrivi,sana,3);
        samat=0; if (muste_strcmpi(hakusana,sana[0])==0) samat=1;
        i=text_open(sana[1]); samat=0;
        if (i==-2) return(3);
        if (i<0) return(2);

        strcpy(avainsana,sana[2]); strcat(avainsana,"?");
        i=hae(avainsana); if (i<0) { tila=0; return(-1); }
        i=tdisp(i); if (i<0) { tila=0; return(-1); }
        return(1);
        }

static int text_open(char *s)
        {
        int i;
        char rivi[ELE];
        char *sana[3];
        char uedq2[LNAME];

        strcpy(muf,s);
        strcpy(uedq2,info2);
        strcat(uedq2,qprefix); strcat(uedq2,s); strcat(uedq2,".EDT");
        if (strcmp(edq2,uedq2)==0) return(1);
        if (*edq2!=EOS) muste_fclose(text);
        strcpy(edq2,uedq2);
        text=muste_fopen(edq2,"rb");
        if (text==NULL)
            {
            if (!samat) return(-2);

            PR_EINV;
            sprintf(sbuf,"\nNot available in this version! (File %s missing)",edq2);
                        sur_print(sbuf);
            WAIT; PR_ENRM; return(-1);
            }
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(text);
        rivi[ELE-1]=EOS;
        i=split(rivi,sana,3);
        ted1=atoi(sana[1]); ted2=atoi(sana[2]);
        return(1);
        }


static int hae(char *s)
        {
        int i;
        char rivi[LLENGTH];
        int len=strlen(s);

        i=1;
        while (i<=ted2)
            {
            tedread(rivi,i);
            if (strncmp(s,rivi+1,len)==0) return(i);
            ++i;
            }
        sprintf(sbuf,"\nKeyword %s not found in file %s",s,edq2); sur_print(sbuf);
        WAIT; return(-1);
        }

static int tdisp(int j)
        {
        int i;

        dclear();
        vie_muistiin(j);
        i=tdisp2(j);
        return(i);
        }

static int dclear()
        {
        int i;
        PR_EUDL; LOCATE(rdisp+1,1);

        n_ex=0; j_help=0; // 21.12.2002
// printf("\nndisp+1=%d rdisp+1=%d r3+2=%d",ndisp+1,rdisp+1,r3+2); getch();
//    sur_scroll_up(ndisp+1,rdisp+1,1,r3+2,c3+8,(int)shadow_code[sdisp]);

//    for (i=0; i<ndisp; ++i)
//        sur_scroll_up(1,rdisp+1,1,r3+2,c3+8,(int)shadow_code[sdisp]);


        for (i=0; i<ndisp+1; ++i)
            write_string(space,c3+8,shadow_code[sdisp],rdisp+1+i,1);

        mdisp=0;
        *malli=EOS;
        nrivit=0; ntila=0;

        if (mouse_in_use) mouse_help_lines();

// printf("A"); getch();
        return(1);
        }

static int mouse_help_lines()
    {
    strcpy(sbuf,"|Prev| |Next| |Load| |Exit| ");
    strncat(sbuf,space,c3+8-strlen(sbuf));
    write_string(sbuf,c3+8,'7',rdisp-1,1);
    strncpy(sbuf,space,c3+8); strncpy(sbuf+c3-15,avainsana,strlen(avainsana));
    write_string(sbuf,c3+8,' ',rdisp,1);
    return(1);
    }

static int tdisp2(int j)
        {
        int i,len;
        char rivi[LLENGTH];
        char *p, *q, koodi[16];
        char *parametri;
        int toistoj;  /* mahdollisen toiston alkurivi */
        char ch;
        int crivi,csar;
        char varjo[LLENGTH];
        int alkupos;

        n_ex=0; // 21.12.2002
        alkupos=1;
        ++j; toistoj=j; j_help=0;
        while (1)
            {
            tedread(rivi,j);

                /* Vanhojen versioiden rivien erottaminen */
                if (rivi[ted1-1]==' ') { PR_EUDL; ch='4'; }
// koe 31.7.00  if (rivi[ted1-1]==' ') { sdisp=(int)'\300'; ch='\300'; }
                else if (rivi[ted1-1]=='B')
                    { sdisp=')'; ch=')'; rivi[ted1-1]=' '; }
                else if (rivi[ted1-1]=='C')
                    { PR_EIN2; ch='8'; rivi[ted1-1]=' '; }
                else if (rivi[ted1-1]=='D')
                    { PR_EIN2; ch='8'; rivi[ted1-1]=' '; }

            if (*rivi=='@') strcpy(malli,rivi+1);
            if (*rivi=='%') { valinnat(j); return(1); }

            if (*rivi==' ') alkupos=0;

     if (*rivi=='>')
         {
/************************************
         strcpy(example[n_ex],rivi+1);
         p=strstr(example[n_ex]," / ");
         if (p!=NULL)
             {
             len=p-example[n_ex]+2;
             for (i=0; i<len; ++i) rivi[i+1]=' ';

             }
         i=len; while (i>0 && example[n_ex][i]==' ') example[n_ex][i--]=EOS;
         example_line[n_ex]=j_help; ++n_ex;
********************************************/
         strcpy(sbuf,rivi+1);
         p=strstr(sbuf," / ");
         if (p!=NULL)
             {
             *p=EOS;
             strcpy(example[n_ex],sbuf);
             example_line[n_ex]=j_help; ++n_ex;
             strcpy(rivi+1,p+2);
             strncat(rivi,space,p+2-sbuf);
             }
         }
            *koodi=EOS;
            p=strchr(rivi+1,'@');

            if (p!=NULL && *rivi!='a')   /*   a in control col. preserves @ */
                {
                *p=EOS;
                ++p; q=koodi;
                while (*p && *p!=' ') { *q=*p; ++q; ++p; }
                *q=EOS;
                }
            else p=NULL;

            parametri=p;
            if (*(rivi+1)!=EOS)  /* muutakin kuin @koodi */
                {
                len=strlen(rivi);

                while (rivi[len-1]==' ') --len; rivi[len]=EOS;

                slab_muunnos(rivi); /* nappien nimet */

          /*    p=rivi+1;  30.9.89  */
                p=rivi;

                while (*p)
                    {
                    rivit[ntila++]=*p; ++p;
                    if (ntila>=rivitila) ntila-=2;
                    }
                rivit[ntila++]=EOS; ++nrivit;

                if (*rivi=='S')
                    {
                    print_varjorivi(rivi,j,varjo);
                    p=varjo;
                    while (*p)
                        {
                        rivit[ntila++]=*p; ++p;
                        if (ntila>=rivitila) ntila-=1;
                        }
                    rivit[ntila++]=EOS;
                    }
                else
                    {
                    int rlen=strlen(rivi+alkupos);

                    CURSOR_POS(&crivi,&csar);
                    if (rlen>c3+8) rlen=c3+8;
                    write_string(rivi+alkupos,rlen,ch,crivi,1);
                    if (rlen<c3+6) { write_string(space,c3+6-rlen,ch,crivi,rlen+1);
                                   }
                    sur_print("\n");
                    }
                ++mdisp;
                }
            ++j; ++j_help;
            if (*koodi!=EOS)
                {
                if (*koodi=='C') /* @CONTINUE */
                    {
                    i=C_koodi();
                    if (i==0) return(1);
                    dclear(); vie_muistiin(j-1);
                    }
           else if (*koodi=='E') /* @END */
                    {
                    i=E_koodi();
                    if (i==0) return(1);
                    dclear();
                    j=toistoj; vie_muistiin(j);
                    }
           else if (*koodi=='G') /* @GOTO */
                    {
                    i=G_koodi(parametri); if (i<0) { tila=0; return(1); }
                    j=i+1;
                    }
           else
                    {
                    sprintf(sbuf,"\nUnknown @code %s!",koodi); sur_print(sbuf); WAIT;
                    }
                }
            if (mdisp>ndisp-1) kasvata_ikkunaa();
            }
        return(1);
        }

static int show_example()
    {

muste_fixme("\nFIXME: Show help example STUB"); // RS FIXME 
/* RS NYI
    int i;
    extern int help_mouse_x,help_mouse_y;

    if (n_ex==0) return(1);

// printf("\nmouse: %d %d|\n",help_mouse_x,help_mouse_y);
// for (i=0; i<n_ex; ++i) printf("%d ",example_line[i]);
// getch();

    for (i=0; i<n_ex; ++i) // 21.12.2002
        {
        if (example_line[i]+2==help_mouse_y) break;
        }
    if (i==n_ex) i=0;

    sprintf(sbuf,"%sS.EXE",survo_path);
    sur_flush_input(); // 19.11.2002
    sur_sleep(200L);
    spawnl(P_NOWAIT,sbuf,sbuf,example[i],NULL);
    sur_sleep(200L);
*/    
    return(1);
    }

static void kasvata_ikkunaa()
        {
// RS REM        int row,col;

/*      SCROLL_UP(1,r3+1,1); PR_UP;    */
        PR_EUDL; sur_scroll_up(1,2,1,r3+2,c3+8,(int)shadow_code[sdisp]); PR_UP;

        if (rdisp<=1) return;
        --rdisp; ++ndisp;
        }

static int C_koodi()
        {
        int m;

        PR_EINV;
        sur_print("To continue, press SPACE, otherwise ENTER! ");
        PR_ENRM;
        ++nruutu;
        PR_LEFT;
        while (1)
            {
            m=nextch("");
            if (m==-2) { m=mouse_help(hakusana); }
            m=kur2(m,hakusana);
            if (m==-9) return(new_start());
            if (m==254) { tila=1; return(0); }
            if (m==CODE_RETURN) { tila=0; return(0); }
            if (m==' ') break;
            if (m=='?') { uusi_haku(); return(0); }
            if (m=='+') { load_lines(); continue; }
            if (m=='.') { print_lines(); continue; }
            if (m==CODE_HOME) { disp_tied_nimi(muf); continue; }
            if (m==CODE_HELP) { help(); return(0); }
            if (m==CODE_BACKSP) { tila=5; return(0); }
            if (m=='#') { tila=7; return(0); }
            if (m=='>') { show_example(); continue; } // 19.11.2002
            }
        return(1);
        }

static int E_koodi()
        {
        int m;

        PR_EINV;
        if (nruutu>0)
            {
            sur_print("To repeat, press SPACE, otherwise ENTER! ");
            }
        else
            {
            if (nmu>1)
                sur_print("Previous page by BACKSPACE, otherwise ENTER!");
            else
                {
                sur_print("Press ENTER! ");
                }
            }
        PR_ENRM;
        PR_LEFT;
        while (1)
            {
            m=nextch("");
            if (m==-2) { m=mouse_help(hakusana); }
            m=kur2(m,hakusana);
            if (m==-9) return(new_start());
            if (m==254) { tila=1; return(0); }
            if (m==CODE_RETURN) { tila=0; return(0); }
            if (m==' ') break;
            if (m=='?') { uusi_haku(); return(0); }
            if (m=='+') { load_lines(); continue; }
            if (m=='.') { print_lines(); continue; }
            if (m==CODE_HOME) { disp_tied_nimi(muf); continue; }
            if (m==CODE_HELP) { help(); return(0); }
            if (m==CODE_BACKSP) { tila=5; return(0); }
            if (m=='#') { tila=7; return(0); }
            if (m=='>') { show_example(); continue; } // 19.11.2002
            }
        return(1);
        }

static int G_koodi(char *s)
        {
        int i;
        char *sana[1];

        split(s,sana,1); strcpy(avainsana,sana[0]);
        strcat(avainsana,"?");
        i=hae(avainsana);
        return(i);
        }

static void valinnat(int j)
        {
        char rivi[LLENGTH];
        char *p;
        int i,m;
        char v1,v2;
        char uusi[LNAME];

        *valinta=EOS;
        while (1)
            {
            tedread(rivi,j);
            if (*rivi!='%') break;
            p=strchr(rivi,'@');
            if (p==NULL)
                {
                i=strlen(rivi);
                while (rivi[i-1]==' ') --i; rivi[i]=EOS;
                }
            else *p=EOS;
            strcat(valinta,rivi+1); strcat(valinta," ");
            ++j;
            }

        p=strchr(valinta,'='); v1=*(p-1);
        i=strlen(valinta)-1;
        while (valinta[i]!='=') --i; v2=valinta[i-1];
        PR_EINV;
        if (v1!=v2)
            {
            sprintf(sbuf,"Select %c-%c or press ENTER! ",v1,v2); sur_print(sbuf);
            }
        else
            {
            sprintf(sbuf,"Select %c or press ENTER! ",v1); sur_print(sbuf);
            }
        PR_ENRM;
        PR_LEFT;
        i=-1;
        while (i<0)
            {
            m=nextch("");
            if (m==-2) { m=mouse_help(hakusana); }
            m=kur2(m,hakusana);
            if (m==-9) { new_start(); return; }
            if (m==254) { tila=1; return; }
            if (m==CODE_RETURN) { tila=0; return; }
            if (m=='?') { uusi_haku(); return; }
            if (m=='+') { load_lines(); continue; }
            if (m=='.') { print_lines(); continue; }
            if (m==CODE_HOME) { disp_tied_nimi(muf); continue; }
            if (m==CODE_HELP) { help(); return; }
            if (m==CODE_BACKSP) { tila=5; return; }
            if (m=='#') { tila=7; return; }
            if (m=='>') { show_example(); continue; } // 19.11.2002
            i=etsim(m,uusi);
            }

        p=strchr(uusi,':'); /* 6.7.1996 */
        if (p!=NULL)
            {
            CLS;
            LOCATE(2,1); headline(""); PR_EIN2;
            sur_print("Information on optional Survo modules:");

            i=aktivoi(uusi);
/*
printf("\ni=%d uusi=%s p+1=%s avainsana=%s tila=%d",i,uusi,p+1,avainsana,tila); getch();
*/
            if (i==0) {tila=1; strcpy(hakusana,avainsana); CLS; return; }
            strcpy(avainsana,p+1);
            strcpy(uusi,avainsana);
            }

        i=strlen(uusi);
        if (uusi[i-1]=='?')
            {
            strcpy(hakusana,uusi);
            hakusana[i-1]=' ';
            tila=1; return;
            }
        strcpy(avainsana,uusi); strcat(avainsana,"?"); tila=3;

/* printf("\navainsana=%s",avainsana); getch(); */
        return;
        }

static int aktivoi(char *uusi)
        {
        sur_print("\nInquiries about additional operations not supported in SURVO 98");
        WAIT; // RS REM exit(0);
        return(1);
/*
        int i,j;
        char *p;
        char opfile[LNAME];
        char x[LLENGTH];
        p=strchr(uusi,':'); *p=EOS;
        if (*uusi=='!') *uusi='_';
        strcpy(opfile,survo_path); strcat(opfile,uusi);
        strcat(opfile,".EXE");
        j=r1+r-1;
        edread(x,j);
        edwrite(space,j,0);
        i=s_spawn(opfile,argv1);
        edwrite(x,j,0);
        return(0);
*/
        }

static int etsim(int m,char *uusi)
        {
        char *p;

        while (1)
            {
            p=valinta;
            while ((p=strchr(p,'='))!=NULL)
                {
                if ((char)m==*(p-1))
                    {
                    ++p;
                    while (*p!=' ') { *uusi=*p; ++p; ++uusi; }
                    *uusi=EOS;
                    return(1);
                    }
                ++p;
                }
            if (m>='a' && m<='z') m-='a'-'A'; else break;
            }
        return(-1);
        }

static void vie_muistiin(int j)
        {
        int i;
        for (i=0; i<mulen; ++i)
            {
            if (i<strlen(muf)) mufile[mulen*nmu+i]=muf[i];
            else               mufile[mulen*nmu+i]=' ';
            }
        musys[nmu]=isys;  /* 9.5.93 */
/* printf("\nvie: isys=%d",isys); getch(); */
        *sbuf=EOS; strncat(sbuf,avainsana,9);
        strcpy(musana[nmu],sbuf);
        murivi[nmu++]=j;
        if (nmu>=muisti) --nmu;

/* printf("\nmufile: %.100s",mufile); getch();  */
        }

static int ota_muistista()
        {
        int i;

        nmu-=2;
        if (nmu<0) return(-1);
        for (i=0; i<mulen; ++i) muf[i]=mufile[mulen*nmu+i];
        i=mulen; while (muf[i-1]==' ') muf[--i]=EOS;
/* printf("\nisys=%d musys=%d",isys,musys[nmu]); getch(); */
        if (isys!=musys[nmu]) { isys=musys[nmu]; avaa(isys); } /* 9.5.93 */
        strcpy(avainsana,musana[nmu]);

        return(murivi[nmu]);
        }

static int mouse_help(char *sana)
    {
    int m=0;
    char x[LLENGTH],x2[LLENGTH];
    char *p;

muste_fixme("\nFIXME: Help mouse STUB"); // RS FIXME
/*
// RS REM    extern int help_mouse_x,help_mouse_y;

//  printf("\nmouse: %d %d|",help_mouse_x,help_mouse_y); getch();

    read_string(x+1,x2+1,c3+2,help_mouse_y+1,1); *x=' ';
    p=strchr(x+1,'=');
    if (p!=NULL && (int)(p-x-1)<7 && (int)(p-x-1)>=help_mouse_x)
        {
        p=x+1;
        while (*p==' ') ++p;
        m=(int)*p;
        return(m);
        }
    if (x[help_mouse_x+1]==' ') return(-1);
// printf("mouse=%s|",x+help_mouse_x+1); getch();
    nykyinen_sana(x,help_mouse_x+1,sana);
    strcat(sana," "); strupr(sana);
// printf("SANA=%s|",sana); getch();
    if (strcmp(sana,"CONTINUE ")==0 ||
        strcmp(sana,"SPACE ")==0 || strcmp(sana,"|NEXT| ")==0) m=(int)' ';
    else if (strncmp(sana,"ENTER",5)==0 || strcmp(sana,"|EXIT| ")==0) m=KEY_RETURN;
    else if (strncmp(sana,"BACK",4)==0 || strcmp(sana,"|PREV| ")==0) m=KEY_BACKSP;
    else if (strcmp(sana,"|LOAD| ")==0) m='+';
    else if (strcmp(sana,"|EXAMPLE| ")==0) m='>'; // 19.11.2002
    else m=254;
*/


    return(m);
    }


static void uusi_haku()
        {
        int len;
        char *p;

        putsaa(); LOCATE(r3+2,1);
        prompt("New keyword? ",uusi_hakusana,31);
        strcpy(hakusana,uusi_hakusana);
        p=strchr(hakusana,'?'); if (p!=NULL) *p=EOS;
        strcat(hakusana," "); muste_strupr(hakusana);
        tila=1;
        }

static void putsaa()
        {
        PR_ENRM; LOCATE(r3+2,1);
        sprintf(sbuf,"%.*s",c3+6,space); sur_print(sbuf);
        }

static int monivalinta()
        {
        int i,j, len;
        int f,f1,fmax;
        char x[LLENGTH];
        char sana[32], *p,*q;
        char ehdotus[32];
        int listarivi;

        f=fit(jmin); f1=fit(jmax);
        fmax=(f>f1)? (f):(f1);
        if (fmax==0) { jmin=qrivi1+1; jmax=qrivi2-1; }
        if (*hakusana!=' ' && fmax>0 )
            {
            if (f==fmax)
                {
                --jmin;
                while (fit(jmin)==fmax) --jmin;
                ++jmin;
                }
            else ++jmin;
            if (f1==fmax)
                {
                ++jmax;
                while (fit(jmax)==fmax) ++jmax;
                --jmax;
                }
            else --jmax;
            }

        if (jmax==jmin) { qedread(avainrivi,jmin); tila=2; return(1); }

        if (isys<sysmax-1) { tila=6; return(1); }  /* 9.5.93 */

        if (mouse_click) { tila=2; return(1); }

        dclear();
        strcpy(x,"Keywords: "); len=10;
        listarivi=0;
        for (j=jmin; j<=jmax; ++j)
            {
            qedread(avainrivi,j);
            p=sana; q=avainrivi+1;
            while (*q!=' ') { *p=*q; ++p; ++q; }
            *p=' '; *(p+1)=EOS;

            if (j==jmin) strcpy(ehdotus,sana);
            if (len+strlen(sana)<c3+6)
                {
                strcat(x,sana); len+=strlen(sana);
                }
            else
                {
                tulosta(x); *x=EOS; len=0;
                ++listarivi;
                if (listarivi>=r3-2)
                    {
                    i=nextch("More keywords by pressing any key!");
                    if (i==-9) return(new_start());
                    if (i==CODE_BACKSP) { ++nmu; tila=5; return(1); }
                    listarivi=0;
                    }
                }

            }
        if (*x!=EOS) tulosta(x);

// 20.4.2010
if (*ehdotus=='#') { // printf("\nhakusana=%s  ###",hakusana); getch();
                     strcpy(x,hakusana+1); strcpy(hakusana,x);
                     tila=1; return(1);
                   }
        PR_EINV;
        i=prompt("Select keyword: ",ehdotus,31);

        if (i==2) { ++nmu; tila=5; return(1); }
        PR_ENRM;
        strcpy(hakusana,ehdotus);
        if (*hakusana==EOS) { tila=0; return(1); }
        len=strlen(hakusana); if (hakusana[len-1]=='?') hakusana[len-1]=EOS;
        strcat(hakusana," "); muste_strupr(hakusana);
        tila=1;
        return(1);
        }

static int fit(int j)
        {
        int i=0;
        char *p,*q;

        qedread(avainrivi,j);
        p=avainrivi+1; q=hakusana;
        while (*p==*q) { ++i; ++p; ++q; }
/*  printf("\n%s %s %d",hakusana,avainrivi,i); getch(); */
        return(i);
        }


static void tulosta(char *x)
        {
        sprintf(sbuf,"%.*s\n",c3+6,x); sur_print(sbuf);
        ++mdisp;
        if (mdisp>ndisp-1) kasvata_ikkunaa();
        }


static int load_lines()
        {
        int i,j,kesken;
        char *p,*q;
        char rivi[LLENGTH];
        char x[LLENGTH];
        char kmerkki;
        int tilaa_varjoille;
        int m;
        int alkupos;
        char contr[2];
		extern char sur_session[2];        

        p=rivit; kesken=0;
        tilaa_varjoille=1;

        if (own_window)
            {
            sprintf(x,"%s%sLINES.TMP",etmpd,sur_session);
// printf("\n%s|",x);
            lines=muste_fopen(x,"wt");
            fprintf(lines,"%d 0 %d 1 %d 0\n",
                        nrivit,ted1-1,nrivit);
            }

        alkupos=1; contr[1]=EOS;
        for (j=0; j<nrivit; ++j)
            {
            kmerkki=*p; ++p;
            if (kmerkki==' ') alkupos=0;
            q=rivi;
            while (*p) { *q=*p; ++q; ++p; }
            *q=EOS; ++p;
   if (!own_window)
         {
            if (oline>r2) { kesken=1; break; }
            edread(x,oline);
            if (!empty_h(x+1)) /* { kesken=1; break; }  */
                {
                if (!riv_lis)
                    {
                 /* PR_EINV; LOCATE(r3+2,1);  */
 sprintf(sbuf,"Not enough empty lines! Insert space for %d new lines (Y/N)?  ",
                          nrivit-j);
             write_string(sbuf,62,'7',r3+2,1); LOCATE(r3+2,61);
                    m=nextch("");
             write_string(space,62,' ',r3+2,1);
                    if (m=='N' || m=='n') { kesken=2; break; }
                    riv_lis=1;
                    }
                i=insert_lines(oline,nrivit-j);
                if (i<0) { kesken=1; break; }
                }
            edwrite(rivi,oline,1);
         }
            if (own_window)
                {
                if (kmerkki=='>') strcpy(sbuf,"*");
                else
                    sprintf(sbuf,"%c%s",kmerkki,rivi);

                pidenna(sbuf,ted1);
                fprintf(lines,"%s\n",sbuf);
                }

            if (alkupos==0 && !own_window) { *contr=kmerkki; edwrite(contr,oline,0); }

            if (kmerkki=='S')
                {
                q=rivi;
                while (*p) { *q=*p; ++q; ++p; }
                *q=EOS; ++p;

                if (tilaa_varjoille && !own_window)
                    {
                    if (zs[oline]==0)
                        {
                        i=shadow_create(oline);
                        if (i<0) tilaa_varjoille=0;
                        }
                    if (tilaa_varjoille)
                        edwrite(rivi,zs[oline],0);
                    }

                if (own_window)
                    {
                    sprintf(sbuf,"%s",rivi);
                    pidenna(sbuf,ted1);
                    fprintf(lines,"%s\n",sbuf);
                    }
                }
            else if (own_window)
                fprintf(lines,"%.*s\n",ted1,space);

            ++oline;
            }
        if (kesken==1)
            {
            PR_EBLK; LOCATE(r3+2,55);
            sur_print("Not enough empty lines!"); PR_ENRM;
            }
        else if (!kesken)
            {
            PR_EINV; LOCATE(r3+2,20);
            sur_print("Lines loaded!"); PR_ENRM;
            }
        if (own_window)
            {
            muste_fclose(lines);
//   printf("\nNEW"); getch();
            sur_set_message("NEW",2);
//          m=nextch();
//          if (m==-9) return(new_start());
            }
        return(1);
        }

static int pidenna(char *s,int len)
    {
    int i;

    for (i=strlen(s); i<len; ++i) s[i]=' ';
    s[len]=EOS;
    return(1);
    }


static void print_lines()
        {
        int row,col;

        CURSOR_POS(&row,&col); PR_EBLK; LOCATE(r3+2,1);
        sur_print("Not possible to print the lines. ");
        sur_print("(Load them first to the field by '+', please.)!"); PR_ENRM;
        sur_getch(); LOCATE(r3+2,1); ERASE; LOCATE(row,col);
        }

/* poistettu 11.9.1989
print_lines()
        {
        FILE *kirjoitin;
        int j;
        char *p,*q;
        char rivi[LLENGTH];

        kirjoitin=muste_fopen("PRN","wt");
        p=rivit;
        for (j=0; j<nrivit; ++j)
            {
            q=rivi;
            while (*p) { *q=*p; ++q; ++p; }
            *q=EOS; ++p;
            q=rivi;
            while (*q) { putc((int)(*q),kirjoitin); ++q; }
            putc((int)'\n',kirjoitin);
            }
        muste_fclose(kirjoitin);
        }
*/

static void slab_muunnos(char *s)
        {
        char t[LLENGTH];
        char *p,*q;
        char lab[16];
        int len;

        p=strchr(s,'{'); if (p==NULL || *s=='a') return;
        p=s; *t=EOS;
        while ((q=strchr(p,'{'))!=NULL)
            {
            if (*(q+1)=='{') { strncat(t,p,q-p+1); p=q+2; continue; } /* {{xx}={xx} */
            strncat(t,p,q-p);
            p=q+1;
            label2(atoi(p),lab);
            len=strlen(lab); while (lab[len-1]==' ') lab[--len]=EOS;
            strcat(t,lab);
            while (*p!='}' && *p) ++p;
            ++p;
            }
        strcat(t,p); strcpy(s,t);
        }

static void disp_tied_nimi(char *s)
        {
        LOCATE(r3+2,1);
        sprintf(sbuf," Help file: %s%s   Keyword: %s",pedq[isys],s,avainsana);
        sur_print(sbuf);
        LOCATE(rdisp+1,1);
        }

static void help()
        {

        if (strncmp(hakusana,"HELP",4)==0 && isys>0)
            {
            --isys; avaa(isys);
            }
        strcpy(hakusana,"HELP "); tila=1;
        }

static int insert_lines(int jj,int k)
        {
        int i;
        int j;
        char x[LLENGTH], x1[LLENGTH];

        for (j=r2; j>r2-k; --j)
            {
            edread(x,j);
            if (!empty_h(x+1)) break;
            }
        if (j>r2-k) return(-1);

/*      memmove(z+(r1+r)*ed1,z+(r1+r-1)*ed1,(r2-r1-r)*ed1);    */
        memmove(z+(jj+k-1)*ed1,z+(jj-1)*ed1,(r2-k-jj+1)*ed1);

        strcpy(x,"*"); strncat(x,space,c2);
        for (j=jj; j<jj+k; ++j)
            edwrite(x,j,0);

        for (j=r2-k; j>=jj; --j) zs[j+k]=zs[j];
        for (j=jj; j<jj+k; ++j) zs[j]=0;
        return(1);
        }

static void print_varjorivi(char *rivi,int j,char *varjo)
        {
        long l;
        int i;
        short is;
        char *p;
 /*     char varjo[LLENGTH];     */
        int k;
        char dispx;
        int len;

        p=(char *)&is;

        l=(long)ted1*(long)(ted2+2);
        while (1)
            {
            muste_fseek(text,l,0);
            *p=(char)getc(text);
            *(p+1)=(char)getc(text);
            i=is;
            if (i<0) { /* printf("%.*s\n",c3+6,rivi+1); */ return; }
            if (i==j) break;
            l+=ted1;
            }
        for (i=0; i<ted1-2; ++i) varjo[i]=(char)getc(text);
        len=strlen(rivi); if (len>c3+7) len=c3+7;
        for (i=1; i<len; ++i)
            {
            dispx=varjo[i];
            k=i;
            while (k<len && varjo[k]==dispx) ++k;
            qdisp_mode((int)dispx);
            sprintf(sbuf,"%.*s",k-i,rivi+i); sur_print(sbuf);
            i=k-1;
            }
        PR_EUDL; if (len<c3+7) { sprintf(sbuf,"%.*s",c3+7-len,space); sur_print(sbuf); }
        sur_print("\n");
        }

static void qdisp_mode(int dispm)
        {
        switch (dispm)
            {
          case ' ': PR_EUDL; break;
          case '1': PR_EBLD; break;
          case '2': PR_ESUB; break;
          case '3': PR_ESUP; break;
          case '4': PR_EUDL; break;
          case '5': PR_EBLK; break;
          case '6': PR_EOVR; break;
          case '7': PR_EINV; break;
/*      default: PR_EIN2; break;  */
        default: sdisp=dispm; break;
            }
        }

static int kur2(int m,char *sana)
        {
        int i;
        char x[LLENGTH],x2[LLENGTH];

        CURSOR_POS(&row,&col);
        switch (m)
            {
          case CODE_UP:
            if (row>1) --row; break;
          case CODE_DOWN:
            if (row<r3+2) ++row; break;
          case CODE_LEFT:
            if (col>1) --col; m=253; break;
          case CODE_RIGHT:
            if (col<c3+8) ++col; break;

          case CODE_PRE:
            m=nextch("");
            if (m!=CODE_HELP) break;
          case CODE_EXEC:
            read_string(x+1,x2+1,c3+2,row,1); *x=' ';
            i=nykyinen_sana(x,col,sana);
            strcat(sana," "); muste_strupr(sana);
            m=254; break;
            }
        LOCATE(row,col);
        return(m);
        }

/* slab2.c 27.8.1985/SM (7.1.1986)
*/
#define MAX MAXLABEL
#define LEN LENLABEL
/************************************
label(m,nimi)
int m;
char nimi[];
        {
        strcpy(nimi,key_label[m]);
        if (nimi[0]==' ') nimi[0]=(char)m;
        }
*************************************/
static void label2(int m,char nimi[])
        {
        if (m<0 || m>=MAX) { strcpy(nimi,"???"); return; }
        strcpy(nimi,key_lab+LEN*m);
        }
