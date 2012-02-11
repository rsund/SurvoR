/*  ctut.c 2.9.1985/SM (6.3.1994) (6.6.1995)
 */
#define TUTCODE 253
#define TUT_COMMENT_CODE 252
#define MAXNIMET 100
#define MAXPITUUS 32
#define MAXOSAT 500
#define JAKSO1 24
#define JAKSO2 18
#define LABMAX 200
#define LABTILA 5000
#define NWORD 111



#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <unistd.h> // RS
/* RS REM
#include <conio.h>
#include <string.h>
#include <process.h>
*/
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static FILE *tutor;
static FILE *tutor2;
static int eol;
static char rivi[32*LLENGTH];
static char loppumerkit[3];
static char labelcode[5];
static char tutpref[256];
static char tutcode[256];
static int koodi; /* 1=laaja (default), 0=suppea  */
static char crivi[24*LLENGTH];
static int maxcrivi=24*LLENGTH-1;
static int t_virhe=0;
static int n_comments=0;
static int n_end_comments=0;
static char comment_code[]={ '\374', EOS };
static long koodi_pos,kommentti_pos;
static char etusukro[LLENGTH]; /* 10.2.90 */
static int tut_index=0;

static char wnimi[MAXNIMET][MAXPITUUS];
static int max_indeksi;

static int nosat,is;
static char osasukro[MAXOSAT][9];
static long osaosoite[MAXOSAT];
static char tiednimi[LLENGTH];
static char apunimi[LLENGTH];
static int uusi_tiedosto,uusi_sukro;
static long uusi_koodialku,uusi_paikka1,uusi_paikka2;
static int com_warning=0;

static char txt_file_name[LNAME];
static FILE *txt_file;
// RS REM extern char ser_number[];

static int o1,o2;


static int crivin_alku=0;
static int cursor_line=0;
static int corner_line=0;
static int max_shadows=0;
static int dim_index=0;
static int win_lines=0; // 6.5.2004
static char wstr[LLENGTH];

static char labspace[LABTILA];
static char *tutlab[LABMAX],*tutgo[LABMAX];
static int goline[LABMAX];
static int nlab,ngo,no_labcheck;
static char *plab;

static char *at[NWORD],*bt[NWORD];
static int nword=NWORD;

static int tulkitse_rivi();
static int tutor_op_arit();

static void init_words()
        {
        at[0]="R"; bt[0]="\375R";
        at[1]="home"; bt[1]="\375H";
        at[2]="pgdn"; bt[2]="\375n";
        at[3]="pgup"; bt[3]="\375p";
        at[4]="erase"; bt[4]="\375e";
        at[5]="ins"; bt[5]="\375i";
        at[6]="del"; bt[6]="\375v";      /* viitataan indeksiin 6 */
        at[7]="ins line"; bt[7]="\375I";
        at[8]="del line"; bt[8]="\375V";
        at[9]="pre"; bt[9]="\375G";
        at[10]="act"; bt[10]="\375D";
        at[11]="("; bt[11]="{";
        at[12]=")"; bt[12]="}";
        at[13]="r"; bt[13]="\375r";
        at[14]="l"; bt[14]="\375l";
        at[15]="u"; bt[15]="\375u";
        at[16]="d"; bt[16]="\375d";
        at[17]="touch"; bt[17]="\3753";
        at[18]="end"; bt[18]="\375GT\375S";
        at[19]="form"; bt[19]="\3755";
        at[20]="tab"; bt[20]="\375x";
        at[21]="line end"; bt[21]="\375E";
        at[22]="next word"; bt[22]="\375GW";
        at[23]="stack char"; bt[23]="\375Gx";
        at[24]="stack word"; bt[24]="\375Gw";
        at[25]="del stack"; bt[25]="\375G0";
        at[26]="merge"; bt[26]="\3756";
        at[27]="ref"; bt[27]="\3757";
        at[28]="exit"; bt[28]="\3758";
        at[29]="code"; bt[29]="\375F";
        at[30]="copy"; bt[30]="\375B";
        at[31]="disk"; bt[31]="\3754";
        at[32]="help"; bt[32]="\375h";
        at[33]="search"; bt[33]="\3759";
        at[34]="file act"; bt[34]="\375z";
        at[35]="block"; bt[35]="\375M";
        at[36]="get key"; bt[36]="\375Gy";
        at[37]="break off"; bt[37]="\375GTb1@";
        at[38]="break on"; bt[38]="\375GTb0@";
        at[39]="save stack"; bt[39]="\375G@!";
        at[40]="load stack"; bt[40]="\375G@&";
        at[41]="words"; bt[41]="\375W";
        at[42]="sp"; bt[42]=" ";
        at[43]="save char"; bt[43]="\375Gx";     /* =stack char */
        at[44]="save word"; bt[44]="\375Gw";     /* =stack word */
        at[45]="save cursor"; bt[45]="\375GR";
        at[46]="stack cursor"; bt[46]="\375GR";
        at[47]="save line"; bt[47]="\375GX";
        at[48]="stack line"; bt[48]="\375GX";
        at[49]="save datapath"; bt[49]="\375Gd";
        at[50]="stack datapath"; bt[50]="\375Gd";
        at[51]="save systemdisk"; bt[51]="\375Gg";
        at[52]="stack systemdisk"; bt[52]="\375Gg";
        at[53]="save corner"; bt[53]="\375GL3\375GL4";
        at[54]="stack corner"; bt[54]="\375GL3\375GL4";
        at[55]="init"; bt[55]="\375Gb";
        at[56]="line start"; bt[56]="\375GB";
        at[57]="save dim"; bt[57]="\375GL5\375GL6";
        at[58]="stack dim"; bt[58]="\375GL5\375GL6";
        at[59]="message"; bt[59]="\375GTp";
        at[60]="comma"; bt[60]=",";
        at[61]="interaction on"; bt[61]="\375GQ";
        at[62]="interaction off"; bt[62]="\375Gq";
        at[63]="stack default datapath"; bt[63]="\375Go";
        at[64]="save default datapath"; bt[64]="\375Go";
        at[65]="stack tempdisk"; bt[65]="\375Gh";
        at[66]="save tempdisk"; bt[66]="\375Gh";
        at[67]="message shadow"; bt[67]="\375Gu ";
        at[68]="save systempath"; bt[68]="\375G^";
        at[69]="stack systempath"; bt[69]="\375G^";
        at[70]="save time"; bt[70]="\375G/";
        at[71]="stack time"; bt[71]="\375G/";
        at[72]="repeat"; bt[72]="\375GT_\375S";
        at[73]="disp off"; bt[73]="\375GLo";
        at[74]="disp on"; bt[74]="\375GLO";
        at[75]="next col"; bt[75]="\375G@~";
        at[76]="save survotype"; bt[76]="\375G~";
        at[77]="stack survotype"; bt[77]="\375G~";
        at[78]="save language"; bt[78]="\375G\234";   /* £ */
        at[79]="stack language"; bt[79]="\375G\234";
        at[80]="save survo.apu"; bt[80]="\375G\204";   /* ä */
        at[81]="stack survo.apu"; bt[81]="\375G\204";
        at[82]="save shadowdim"; bt[82]="\375GL7\375GL8";
        at[83]="stack shadowdim"; bt[83]="\375GL7\375GL8";
        at[84]="save user"; bt[84]="\375G\224";   /* ö */
        at[85]="stack user"; bt[85]="\375G\224";
        at[86]="disp reset"; bt[86]="\375GL0";
        at[87]="labels off"; bt[87]="\375GLl";
        at[88]="labels on"; bt[88]="\375GLL";
        at[89]="save survo2.apu"; bt[89]="\375G\216";   /* Ä */
        at[90]="stack survo2.apu"; bt[90]="\375G\216";
        at[91]="waitrec on"; bt[91]="\375GLW";
        at[92]="waitrec off"; bt[92]="\375GLw";
        at[93]="paint on"; bt[93]="\375GLP";   /* 17.11.1996 */
        at[94]="paint off"; bt[94]="\375GLp";
        at[95]="save insertmode"; bt[95]="\375GU";
        at[96]="stack insertmode"; bt[96]="\375GU";
        at[97]="save fieldtype"; bt[97]="\375GLF";
        at[98]="stack fieldtype"; bt[98]="\375GLF";
        at[99]="save dataname"; bt[99]="\375GLf";
        at[100]="stack dataname"; bt[100]="\375GLf";
        at[101]="save os"; bt[101]="\375Gk";
        at[102]="stack os"; bt[102]="\375Gk";
        at[103]="soft_on"; bt[103]="\375s";
        at[104]="soft_off"; bt[104]="\375G\375s";
        at[105]="version"; bt[105]="\375GLs";
        at[106]="save winsize"; bt[106]="\375GLZ\375GLz";
        at[107]="stack winsize"; bt[107]="\375GLZ\375GLz";
        at[108]="play sound"; bt[108]="\375GTS";
        at[109]="stack netsurvo"; bt[109]="\375GLn";
        at[110]="stack netpath"; bt[110]="\375GLN";

        }


static void init_rivi(char *rivi)
        {
        rivi[0]='*'; rivi[1]=EOS;
        }

static void tutname(char *file,char *name)
        {
        subst_survo_path(name);  // 19.2.2001

        *file=EOS;
        if (!muste_is_path(name)) // RS CHA if (strchr(name,':')==NULL && *name!='/' && *name!='\\' && *(name+1)!='.' && *name!='~') // RS ADD FIXME path
            {
            if (*name=='.') strcat(file,esysd);
            else strcat(file,edisk);
            }
        strcat(file,name);
        file_name_ext(file,".TUT");
        }

static int tutopen(char s[],char mode[])
        {
        char filename[LNAME];
        char x[LNAME]; // 19.2.2001

        strcpy(x,s);
        tutname(filename,x);   /* 12.5.91 */

        strcpy(tiednimi,filename); /* 11.2.90 */
        tutor=muste_fopen(filename,mode);
        if (tutor==NULL) return(-1);
        o1=1;
        return(1);
// RS REM        kommentti_pos=0L;
        }

static void read_tutword(char s[])
        {
        int i,m;

        i=0;
        while ((m=getc(tutor))!=(int)'@' && i<100) s[i++]=m; s[i]=EOS;
/*      if (i==100) tut_virhe();      */
        }

static int tee_sucrodir(char *nimi)
        {
        int i; // RS REM ,n;
        char x[LLENGTH];
        char *p;

        i=tutopen(nimi,"rb"); if (i<0) return(-1);

        read_tutword(x);  /* SURVO 84C SUCROS */
        read_tutword(x);
        nosat=atoi(x);
        for (i=0; i<nosat; ++i)
            {
            read_tutword(x);
            p=x+1; while (*p!=' ') ++p; *p=EOS;
            osaosoite[i]=atol(p+1); ++p;
            strcpy(osasukro[i],x+1);
            }
        return(1);
        }


static void alusta_nimet()
        {
        int i;

        for (i=0; i<MAXNIMET; ++i) wnimi[i][0]=EOS;
        max_indeksi=0;
        }

static void codetut(int code,char ch,int mode)
        {
        if (mode==1)
            { tutpref[code]='1'; tutcode[code]=ch; }
        else
              tutcode[(int)ch]=(char)code;
        }


static void tutcoding(int m)
        {
        int i;

        for (i=0; i<256; ++i) { tutpref[i]='0'; tutcode[i]=(char)i; }
        codetut(CODE_EXEC,'D',m);
        codetut(CODE_RETURN,'R',m);
        codetut(CODE_RIGHT,'r',m);
        codetut(CODE_LEFT,'l',m);
        codetut(CODE_UP,'u',m);
        codetut(CODE_DOWN,'d',m);
        codetut(CODE_PRE,'G',m);
        codetut(CODE_INSERT,'i',m);
        codetut(CODE_INSERTL,'I',m);
        codetut(CODE_DELETE,'v',m);
        codetut(CODE_DELETEL,'V',m);
        codetut(CODE_PREV,'p',m);
        codetut(CODE_NEXT,'n',m);
        codetut(CODE_TOUCH,'3',m);
        codetut(CODE_MERGE,'6',m);
        codetut(CODE_REF,'7',m);
        codetut(CODE_EXIT,'8',m);
        codetut(CODE_CODE,'F',m);
        codetut(CODE_DISP,'5',m);
        codetut(CODE_COPY,'B',m);
        codetut(CODE_ERASE,'e',m);
        codetut(CODE_HOME,'H',m);
        codetut(CODE_DISK,'4',m);
        codetut(CODE_TAB,'x',m);
        codetut(CODE_TABS,'y',m);
        codetut(CODE_HELP,'h',m);
        codetut(255,'S',m);
        codetut(CODE_SRCH,'9',m);
        codetut(CODE_ACTIV,'z',m);
        codetut(CODE_MOVE,'M',m);
        codetut(CODE_END,'E',m);
        codetut(CODE_WORDS,'W',m);
        codetut(CODE_SOFT_ON,'s',m);
        }


static void not_found(char *filename)
        {
        sprintf(sbuf,"\nCannot open sucro file %s!",filename); sur_print(sbuf);
        WAIT;
        }

static void tilavirhe2()
        {
        PR_EBLD;
        sur_print("\nNot enough space for the sucro in the edit field!");
        WAIT; PR_ENRM;
        }

static void lue_kommentti(char *s)
        {
        char *p;

        koodi_pos=ftell(tutor);
        muste_fseek(tutor,(long)kommentti_pos,SEEK_SET);
        p=s;
        while (1)
            {
            *p=(char)getc(tutor);
            if (*p=='\n') { *p=EOS; break; }   // RS FIXME ADD CHECK FOR \r
            ++p;
            }
        kommentti_pos=ftell(tutor);
        muste_fseek(tutor,(long)koodi_pos,SEEK_SET);
        }

static int etsi_koodin_loppu()
        {
        int m;

        koodi_pos=ftell(tutor);
        while (1)
            {
            if (feof(tutor))
                {
                sur_print("\nEnd code missing!");
                muste_fclose(tutor);
                WAIT; return(-1);
                }
            m=getc(tutor);
            if (m!=255) continue;
/* 13.10.88 m=getc(tutor);
            if (m!=255)
                {
                sur_print("\nError in sucro!"); muste_fclose(tutor); WAIT; return(-1);
                }
*/
            n_end_comments=0;
            while (1)
                {
                m=getc(tutor);
                if (m==TUT_COMMENT_CODE) { ++n_end_comments; continue; }
                break;
                }
            break;
            }
        kommentti_pos=ftell(tutor)-1L;
        muste_fseek(tutor,(long)koodi_pos,SEEK_SET);
        return(1);
        }

static int kirjoita(char *x)
        {
        if (*x=='*' && x[1]==EOS) return(1);
        if (x[strlen(x)-1]==' ')
            {
            if (koodi==0) strcat(x,loppumerkit);
            else strcat(x,"{}");
            }
        if (!*txt_file_name)
            {
            edwrite(space,eol,1);
            edwrite(x,eol,0);
            ++eol; if (eol>r2) { tilavirhe2(); t_virhe=1; return(-1); }
            return(1);
            }
        else
            {
//Rprintf("%s\n",x);        
            fprintf(txt_file,"%s\n",x);
            }
        return(1);
        }


static int rcat(char *sana)
        {
        int k;
        int lev;

        lev=ed1-1; if (lev>c3) lev=c3;       // 20.1.2002
//      if (strlen(rivi)+strlen(sana)>c3-2)
        if (strlen(rivi)+strlen(sana)>lev-2) // 20.1.2002
            {
            k=kirjoita(rivi); if (k<0) return(-1);
            init_rivi(rivi);
            }
        strcat(rivi,sana);
        return(1);
        }

static void hae_wnimi(int i,char *s)
        {
        if (*wnimi[i-1]==EOS) { sprintf(s,"W%d",i); return; }
        strcpy(s,wnimi[i-1]);
        }

static char *w_prompt(char *rivi,char *s)
        {
        int i;
        char *p,*q;
        char x[LLENGTH];
// RS REM        char sana[16];
        char *prompt;
        char *wait;
        char *def;
        char *length;
        char y[LLENGTH];


        i=kirjoita(rivi); if (i<0) return(NULL);
        init_rivi(rivi);
        p=s+3;
        q=strchr(p,'@'); *q=EOS;
        wait=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        prompt=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        def=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        length=p;

        if (*prompt==EOS)
            sprintf(x,"- prompt");
        else
            sprintf(x,"- prompt %s",prompt);
        i=kirjoita(x); if (i<0) return(NULL);
        if (*def=='C')
            sprintf(x,"-   default %s",def+1);
        else
            {
            hae_wnimi(atoi(def),y);
            sprintf(x,"-   default %s",y);
            }
        i=kirjoita(x); if (i<0) return(NULL);

        if (tut_index)
            {
            hae_wnimi(tut_index,y);
            sprintf(x,"-   answer %s",y);
            i=kirjoita(x); if (i<0) return(NULL);
            tut_index=0;
            }

        sprintf(x,"-   length %s",length);
        i=kirjoita(x); if (i<0) return(NULL);
        if (*wait=='C')
            sprintf(x,"-   wait %s",wait+1);
        else
            {
            hae_wnimi(atoi(wait),y);
            sprintf(x,"-   wait %s",y);
            }
        i=kirjoita(x); if (i<0) return(NULL);

        p=q+1;
        return(p);
        }


static char *w_switch(char *rivi,char *s)
        {
        int i,h;
        char *p,*p2,*q,*q2;
        char x[LLENGTH];
        char sana[16];

        i=kirjoita(rivi); if (i<0) return(NULL);
        init_rivi(rivi);
        p=s+3;
        p2=strstr(p,"@@");
        if (p2==NULL) { sprintf(sbuf,"\n@@ missing in switch code: %s",s);
                        sur_print(sbuf); WAIT; return(NULL);
                      }
        p2+=2;
        q=strchr(p,'@'); *q=EOS;
        hae_wnimi(atoi(p),wstr);
        sprintf(x,"- switch %s",wstr);
        i=kirjoita(x); if (i<0) return(NULL);
        p=q+1;
        while (1)
            {
            if (*p=='@') break;
            q=strchr(p,'@'); *q=EOS;
            while ((q2=strchr(p,' '))!=NULL) *q2='_';
            q2=strchr(p2,'@');
            if (q2==NULL) { sur_print("\nError in switch code!"); WAIT; return(NULL);
                          }
            *q2=EOS; h=1;
            if (*p2=='G') strcpy(sana,"goto");
            else if (*p2=='L') strcpy(sana,"load");
            else if (*p2==EOS) { h=0; strcpy(sana,"continue"); }
            else { sur_print("\nError in switch code (G or L missing)!");
                   WAIT; return(NULL);
                 }
            if (h)
                sprintf(x,"-   case %s: %s %s",p,sana,p2+1);
            else
                sprintf(x,"-   case %s: continue",p);
            i=kirjoita(x); if (i<0) return(NULL);
            p=q+1; p2=q2+1;
            }
        q2=strchr(p2,'@');
        if (q2==NULL) { sur_print("\nError in switch code!"); WAIT; return(NULL);
                      }
        *q2=EOS; h=1;
        if (*p2=='G') strcpy(sana,"goto");
        else if (*p2=='L') strcpy(sana,"load");
        else if (*p2==EOS) h=0;
        else { sprintf(sbuf,"\nError in switch code (G or L missing)! %s",p2);
               sur_print(sbuf); WAIT; return(NULL);
             }
        if (h)
            sprintf(x,"-   default: %s %s",sana,p2+1);
        else
            sprintf(x,"-   default: continue");
        i=kirjoita(x); if (i<0) return(NULL);
        p=q2+1;
        return(p);
        }

static char *w_key(char *rivi,char *s)
        {
        int i,n,k,h;
        char *p,*q;
        char x[LLENGTH];
        char sana[16];
        char *wait;
        char *keys;
        char nappi[LLENGTH];
        char y[LLENGTH];


        i=kirjoita(rivi); if (i<0) return(NULL);
        init_rivi(rivi);
        p=s+3;
        q=strchr(p,'@'); *q=EOS;
        wait=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        keys=p;
        p=q+1;
        k=kirjoita("- on key"); if (k<0) return(NULL);
        n=strlen(keys);
        for (i=0; i<n; ++i)
            {
            q=strchr(p,'@');
            if (q==NULL) { sur_print("\nError in 'on key' code!");
                           WAIT; return(NULL);
                         }
            *q=EOS; h=1;
            if (*p=='G') strcpy(sana,"goto");
            else if (*p=='L') strcpy(sana,"load");
            else if (*p==EOS) { h=0; strcpy(sana,"continue"); }
            else { sur_print("\n G or L missing in 'on key' code!");
                   WAIT; return(NULL);
                 }

            *nappi=keys[i]; nappi[1]=EOS;
            if (keys[i]==' ') strcpy(nappi,"SPACE");
            else if (keys[i]==':') strcpy(nappi,"COLON");
                    // RS CHA '\010'
            else if (keys[i]==10) strcpy(nappi,"BACKSP");
                                                   // RS CHA '\375'
            else if ((unsigned char)keys[i]==(unsigned char) TUTCODE)
                {
                ++i;
                if (keys[i]=='R') strcpy(nappi,"ENTER");
                if (keys[i]=='r') strcpy(nappi,"RIGHT");
                if (keys[i]=='l') strcpy(nappi,"LEFT");
                if (keys[i]=='u') strcpy(nappi,"UP");
                if (keys[i]=='d') strcpy(nappi,"DOWN");
                if (keys[i]=='H') strcpy(nappi,"HOME");
                if (keys[i]=='h') strcpy(nappi,"HELP");
                if (keys[i]=='D') strcpy(nappi,"ESC");
                if (keys[i]=='i') strcpy(nappi,"INSERT");
                if (keys[i]=='I') strcpy(nappi,"INS_LINE");
                if (keys[i]=='v') strcpy(nappi,"DELETE");
                if (keys[i]=='V') strcpy(nappi,"DEL_LINE");
                if (keys[i]=='e') strcpy(nappi,"ERASE");
                if (keys[i]=='n') strcpy(nappi,"NEXT");
                if (keys[i]=='p') strcpy(nappi,"PREV");
                if (keys[i]=='4') strcpy(nappi,"DISK");
                    // RS CHA '\010'
                if (keys[i]==10) strcpy(nappi,"BACKSP");
                if (keys[i]=='7') strcpy(nappi,"REF");
                if (keys[i]=='6') strcpy(nappi,"MERGE");
                if (keys[i]=='B') strcpy(nappi,"COPY");
                if (keys[i]=='x') strcpy(nappi,"TAB");
                if (keys[i]=='h') strcpy(nappi,"HELP");
                if (keys[i]=='9') strcpy(nappi,"SRCH");
                if (keys[i]=='z') strcpy(nappi,"ACTIV");
                if (keys[i]=='M') strcpy(nappi,"MOVE");
                if (keys[i]=='E') strcpy(nappi,"END");
                if (keys[i]=='W') strcpy(nappi,"WORDS");
                if (keys[i]=='s') strcpy(nappi,"SOFT");
                }
            if (h)
                sprintf(x,"-    key %s: %s %s",nappi,sana,p+1);
            else
                sprintf(x,"-    key %s: continue",nappi);
            k=kirjoita(x); if (k<0) return(NULL);
            p=q+1;
            }
        if (*wait=='C')
            sprintf(x,"-   wait %s",wait+1);
        else
            {
            hae_wnimi(atoi(wait),y);
            sprintf(x,"-   wait %s",y);
            }
        k=kirjoita(x); if (k<0) return(NULL);
        return(p);
        }

static void branch3(int tapaus)
        {
    sur_print("\n3 different alternatives not possible!");
    WAIT;

        }

static void if_error()
        { sur_print("\nError in if statement!"); WAIT; }

static void korvaa2_sp(char *s)
        {
        if (strcmp(s,"_")==0) { strcpy(s,"{sp}"); return; }
        if (strcmp(s,",")==0) { strcpy(s,"{comma}"); return; }
        }

static int lg_muunto(char *jump,char *s)
        {
        if (*jump=='G') sprintf(s,"goto %s",jump+1);
        else if (*jump=='L') sprintf(s,"load %s",jump+1);
        else { if_error(); return(-1); }
        return(1);
        }


static int set_wexpr2(char *w)
    {
// RS REM    int i;
    char s[32];

    strcpy(s,w);
    if (*s!='W') return(1);

    hae_wnimi(atoi(s+1),w);
    return(1);
    }




static int if2_muunto2(char *s,char *t)
    {
    int i;
    char *p;
    char w1[32],w2[32];
    char op;

    strcpy(sbuf,s+1); sbuf[strlen(sbuf)-1]=EOS; // |:t pois
    p=sbuf;
    while (*p)
        {
        if(strchr("+-*/",*p)!=NULL)
            {
            op=*p;
            *p=EOS; strcpy(w1,sbuf); strcpy(w2,p+1);
            i=set_wexpr2(w1); if (i<0) return(-1);
            i=set_wexpr2(w2); if (i<0) return(-1);
            sprintf(t,"%s%c%s",w1,op,w2);
            return(1);
            }
        ++p;
        }
    i=set_wexpr2(sbuf); if (i<0) return(-1); strcpy(t,sbuf);
    return(0);
    }


static char *w_if2(char *rivi,char *s)
        {
        int i,k;
        char *p,*q;
        char x[LLENGTH];
//        char sana[16];
        char *word1;
        char *word2;
        char *jump[3];
// RS REM        char laji;
        char vert[8];
        int j1,j2;
        int tapaus;
        char w1[64],w2[64];
        char go[64];

        i=kirjoita(rivi); if (i<0) return(NULL);
        init_rivi(rivi);
        p=s+3;
//      laji=*p; ++p;
        q=strchr(p,'@'); *q=EOS;
        word1=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        word2=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        jump[0]=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        jump[1]=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        jump[2]=p;
        tapaus=0;
        if (*jump[0]) ++tapaus;
        if (*jump[1]) tapaus+=2;
        if (*jump[2]) tapaus+=4;

        j2=-1; j1=0; // RS j1 init
        switch (tapaus)
            {
          case 0: if_error(); return(NULL);
          case 1: strcpy(vert,"<"); j1=0; break;
          case 2: strcpy(vert,"="); j1=1; break;
          case 3: if (strcmp(jump[0],jump[1])==0) { strcpy(vert,"<="); j1=0; }
                  else branch3(tapaus);
                  break;
          case 4: strcpy(vert,">"); j1=2; break;
          case 5: if (strcmp(jump[0],jump[2])==0) { strcpy(vert,"<>"); j1=0; }
                  else branch3(tapaus);
                  break;
          case 6: if (strcmp(jump[1],jump[2])==0) { strcpy(vert,">="); j1=1; }
                  else branch3(tapaus);
                  break;
          case 7: if (strcmp(jump[0],jump[1])==0) { strcpy(vert,">"); j1=2; j2=0; }
             else if (strcmp(jump[0],jump[2])==0) { strcpy(vert,"="); j1=1; j2=0; }
             else if (strcmp(jump[1],jump[2])==0) { strcpy(vert,"<"); j1=0; j2=1; }
             else branch3(tapaus);

            }

//      if (laji=='A') sprintf(vert2,"'%s'",vert); else strcpy(vert2,vert);
// printf("\nword1=%s word2=%s|",word1,word2); getch();
        if2_muunto2(word1,w1);
        if2_muunto2(word2,w2);

        i=lg_muunto(jump[j1],go); if (i<0) return(NULL);
        k=sprintf(x,"- if %s %s %s then %s",w1,vert,w2,go);
        if (j2>=0)
            {
            i=lg_muunto(jump[j2],go); if (i<0) return(NULL);
            sprintf(x+k," else %s",go);
            }
        i=kirjoita(x); if (i<0) return(NULL);
        p=q+1;
        return(p);
        }

static char *w_if(char *rivi,char *s)
        {
        int i,k;
        char *p,*q;
        char x[LLENGTH];
// RS REM        char sana[16];
        char *word1;
        char *word2;
        char *jump[3];
        char laji;
        char vert[8],vert2[8];
        int j1,j2;
        int tapaus;
        char w1[64],w2[64];
        char go[64];

        i=kirjoita(rivi); if (i<0) return(NULL);
        init_rivi(rivi);
        p=s+3;
        laji=*p; ++p;
        q=strchr(p,'@'); *q=EOS;
        word1=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        word2=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        jump[0]=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        jump[1]=p;
        p=q+1; q=strchr(p,'@'); *q=EOS;
        jump[2]=p;
        tapaus=0;
        if (*jump[0]) ++tapaus;
        if (*jump[1]) tapaus+=2;
        if (*jump[2]) tapaus+=4;

        j2=-1; j1=0; // RS j1 init
        switch (tapaus)
            {
          case 0: if_error(); return(NULL);
          case 1: strcpy(vert,"<"); j1=0; break;
          case 2: strcpy(vert,"="); j1=1; break;
          case 3: if (strcmp(jump[0],jump[1])==0) { strcpy(vert,"<="); j1=0; }
                  else branch3(tapaus);
                  break;
          case 4: strcpy(vert,">"); j1=2; break;
          case 5: if (strcmp(jump[0],jump[2])==0) { strcpy(vert,"<>"); j1=0; }
                  else branch3(tapaus);
                  break;
          case 6: if (strcmp(jump[1],jump[2])==0) { strcpy(vert,">="); j1=1; }
                  else branch3(tapaus);
                  break;
          case 7: if (strcmp(jump[0],jump[1])==0) { strcpy(vert,">"); j1=2; j2=0; }
             else if (strcmp(jump[0],jump[2])==0) { strcpy(vert,"="); j1=1; j2=0; }
             else if (strcmp(jump[1],jump[2])==0) { strcpy(vert,"<"); j1=0; j2=1; }
             else branch3(tapaus);

            }

        if (laji=='A') sprintf(vert2,"'%s'",vert); else strcpy(vert2,vert);
        if (*word1=='C') strcpy(w1,word1+1);
        else hae_wnimi(atoi(word1),w1);
        if (*w1==EOS) strcpy(w1,"{}");
        while ((p=strchr(w1,' '))!=NULL) *p='_';
        korvaa2_sp(w1);
        if (*word2=='C') strcpy(w2,word2+1);
        else hae_wnimi(atoi(word2),w2);
        if (*w2==EOS) strcpy(w2,"{}");
        while ((p=strchr(w2,' '))!=NULL) *p='_';
        korvaa2_sp(w2);
        i=lg_muunto(jump[j1],go); if (i<0) return(NULL);
        k=sprintf(x,"- if %s %s %s then %s",w1,vert2,w2,go);
        if (j2>=0)
            {
            i=lg_muunto(jump[j2],go); if (i<0) return(NULL);
            sprintf(x+k," else %s",go);
            }
        i=kirjoita(x); if (i<0) return(NULL);
        p=q+1;
        return(p);
        }


static char *w_label(char *rivi,char *s)
        {
        int i;
        char *p,*q;
        char x[LLENGTH];

        i=kirjoita(rivi); if (i<0) return(NULL);
        strcpy(rivi,"+");
        p=s+3;
        q=strchr(p,'@');
    if (q==NULL) { sprintf(sbuf,"\nError in label %s",s); sur_print(sbuf); WAIT; return(NULL); }
        *q=EOS;
        sprintf(x," %s: ",p);
        rcat(x);
        p=q+1;
        return(p);
        }

static char *w_gotoload(char *rivi,char *s)    /* \375GTL  */
        {
        int i;
        char *p,*q;
        char x[LLENGTH];
        char sana[16];

        p=s+3;
        q=strchr(p,'@');
        if (q==NULL) { sprintf(sbuf,"\nError in label/sucro_name %s",s); sur_print(sbuf); WAIT;
                       return(NULL); }
        *q=EOS;
        if (*p=='G') { strcpy(sana,"goto"); ++p; }
        else strcpy(sana,"load");
        if (*p=='?')
            {
            ++p;
            i=*p-'0';
            hae_wnimi(i,wstr);
            sprintf(x,"{load sucro %s}",wstr);
            }
        else
            sprintf(x,"{%s %s}",sana,p);
        rcat(x);
        p=q+1;
        return(p);
        }

static int t_error(char *x)
        {
        sprintf(sbuf,"\nSucro error (%s)",x); sur_print(sbuf);
        WAIT; return(1);
        }

static int selkomuunto(char *rivi,char *crivi)
        {
        int i,k;
        char sana2[2];
        char *p,*q;
        char ch;
        char x[LLENGTH];
        char x1[LLENGTH],x2[LLENGTH];
        char x3[10];
/* RS REM
        char *w_switch();
        char *w_label();
        char *w_gotoload();
        char *w_prompt();
        char *w_key();
        char *w_if();
        char *w_if2();
*/
        sana2[1]=EOS;
        p=crivi;
        while (1)
            {
            if (t_virhe) return(-1);
            if (*p==EOS)
                {
                *crivi=EOS; return(1);
                }
            switch ((unsigned char)*p)
                {
              case (unsigned char) TUTCODE:  // RS CHA '\375'
                    ++p; if (*p==EOS) { strcpy(crivi,p-1); return(1); }
                    if (strncmp(p,"G\375G",3)==0)  /* 17.9.1995 */
                        {
                        rcat("{pre}{pre}"); p+=3;
                        break;
                        }
                    if (*p=='G' && *(p+1)==EOS)   /* 13.2.1994 */
                        {
                        rcat("{pre}"); *crivi=EOS; return(1);
                        }
                    switch ((unsigned char)*p)
                        {
                      case 'G':
                            ++p; if (*p==EOS) { strcpy(crivi,p-2); return(1); }
                            if (*p=='T')
                                {
                                if (!crivin_alku)
                                    { strcpy(crivi,p-2); crivin_alku=1; return(1); }
                                crivin_alku=0;
                                ++p;

                                switch ((unsigned char)*p)
                                    {
                                  case 'J': p=w_switch(rivi,p-2);
                                            if (p==NULL) return(-1);
                                            break;
                                  case 'K':
                                            if (strncmp(p+1,"C~",2)==0)
                                                {
                                                q=strchr(p+3,'@');
                                                if (q==NULL) { t_error("save spec");
                                                               return(-1);
                                                             }
                                                *q=EOS;
                                                hae_wnimi(tut_index,wstr);
                                                sprintf(x,"{save spec %s %s}",p+3,wstr);
                                                rcat(x);
                                                p=q+1;
                                                tut_index=0;
                                                break;
                                                }
                                            p=w_prompt(rivi,p-2);
                                            if (p==NULL) return(-1);
                                            break;
                                  case 'V': p=w_key(rivi,p-2);
                                            if (p==NULL) return(-1);
                                            break;
                                  case 'I': p=w_if(rivi,p-2);
                                            if (p==NULL) return(-1);
                                            break;
                          // 17.11.2000
                                  case 'i': p=w_if2(rivi,p-2);
                                            if (p==NULL) return(-1);
                                            break;

                                  case 'X': p=w_label(rivi,p-2);
                                            if (p==NULL) return(-1);
                                            break;
                                  case 'L': p=w_gotoload(rivi,p-2);
                                            if (p==NULL) return(-1);
                                            break;
                                  case (unsigned char) TUTCODE: ++p;  // RS CHA '\375'
                                            if (*p=='S')
                                                {
                                                rcat("{end}");
                                                ++p;
                                                break;
                                                }
                                            break;  /* virheilm. puuttuu */
                                  case 'W': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("wait");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                          /* 26.5.1995 */   if (wait_save) sprintf(x,"{%s}",p);
                                            else sprintf(x,"{wait %s}",p);
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case 'Z': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("del stack");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            hae_wnimi(atoi(p),wstr);
                                        /*  sprintf(x,"{del stack W%d}",atoi(p)); */
                                            sprintf(x,"{del stack %s}",wstr);
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case 's': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("tempo");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            sprintf(x,"{tempo %s}",p);
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case 't': ++p;       /* 24.9.1992 */
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("tempo");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            strcpy(x,"{tempo ");
                                            if (*p=='C') strcat(x,p+1);
                                            else
                                                {
                                                hae_wnimi(atoi(p),wstr);
                                                strcat(x,wstr);
                                                }
                                            strcat(x,"}");
                                            rcat(x);
                                            p=q+1;
                                            break;

                                  case 'a': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("wait");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                       if (*p=='-') {ch='+'; ++p;} else ch='-';
                                            sprintf(x,"{tempo %c%s}",ch,p);
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case 'b': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("break");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            i=atoi(p);
                                            if (i) rcat("{break off}");
                                            else rcat("{break on}");
                                            p=q+1;
                                            break;
                                  case 'g':
                                            strcpy(x,"{jump ");
                                            for (i=0; i<5; ++i)
                                                {
                                                ++p;
                                                q=strchr(p,'@');
                                                if (q==NULL) { t_error("jump");
                                                               return(-1);
                                                             }
                                                *q=EOS;
                                                if (*p=='C')
                                                    {
                                                    if (i==2)
                                                        {
                                                        if (*(p+1)=='!')
                                                            { p=q; continue; }
                                                        else i+=2;
                                                        }
                                                    strcat(x,p+1);
                                                    }
                                                else
                                                    {
                                                    if (i==2) i+=2;
                                                    hae_wnimi(atoi(p),wstr);
                                                    strcat(x,wstr);
                                                    }
                                                if (i<4) strcat(x,",");
                                                p=q;
                                                }
                                            strcat(x,"}");
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case '!':
                                            strcpy(x,"{");
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("arithmetics");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            hae_wnimi(atoi(p+1),wstr);
                                            strcat(x,wstr);
                                            strcat(x,"=");
                                            p=q+1;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("arithmetics");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            if (*p=='C') strcat(x,p+1);
                                            else
                                                {
                                                hae_wnimi(atoi(p),wstr);
                                                strcat(x,wstr);
                                                }
                                            strcat(x,"}");
                                            rcat(x);
                                            p=q+1;
                                            break;

                                  case 'v':
                                            ++p;
                                            strcpy(x,"{soft ");
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("arithmetics");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            if (*p=='V')
                                                strcat(x,p);
                                            else
                                              {
                                              hae_wnimi(atoi(p),wstr);
                                              strcat(x,wstr);
                                              }
                                            strcat(x,"=");
                                            p=q+1;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("arithmetics");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            if (*p=='V')
                                                strcat(x,p);
                                            else
                                              {
                                              hae_wnimi(atoi(p),wstr);
                                              strcat(x,wstr);
                                              }
                                            strcat(x,"}");
                                            rcat(x);
                                            p=q+1;
                                            break;

                                  case '=':
                                            strcpy(x,"{");
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("arithmetics");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            hae_wnimi(atoi(p+1),wstr);
                                            strcat(x,wstr);
                                            strcat(x,"=");
                                            p=q+1;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("arithmetics");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            if (*p=='C')
                                                {
                                     if (strcmp(p+1,"0")!=0 || *(q+1)!='-')
                                                    strcat(x,p+1);
                                                }
                                            else
                                                {
                                                hae_wnimi(atoi(p),wstr);
                                                strcat(x,wstr);
                                                }
                                            p=q+1;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("arithmetics");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            strcat(x,p);
                                            p=q+1;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("arithmetics");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            if (*p=='C') strcat(x,p+1);
                                            else
                                                {
                                                hae_wnimi(atoi(p),wstr);
                                                strcat(x,wstr);
                                                }
                                            p=q+1;
                                            strcat(x,"}");
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case 'c': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("find");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            hae_wnimi(atoi(p),wstr);
                                            sprintf(x,"{find %s}",wstr);
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case 'w': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("find string");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            strcpy(x,"{find string ");
                                            if (*p=='C')
                                                {
                                                strcat(x,p+1);
                                                }
                                            else
                                                {
                                                hae_wnimi(atoi(p),wstr);
                                                strcat(x,wstr);
                                                }
                                            strcat(x,"}");
                                            rcat(x);
                                            p=q+1;
                                            break;

                                  case '>': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("save stack");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            if (*p=='C') strcpy(wstr,p+1);
                                            else hae_wnimi(atoi(p),wstr);
                                            sprintf(x,"{save stack %s}",wstr);
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case '<': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("load stack");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            if (*p=='C') strcpy(wstr,p+1);
                                            else hae_wnimi(atoi(p),wstr);
                                            sprintf(x,"{load stack %s}",wstr);
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case 'D': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("keys");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            sprintf(x,"{keys %s}",p);
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case 'p': rcat("{message}");
                                            ++p;
                                            break;
                                  case 'q': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("message");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            if (*p=='C') strcpy(wstr,p+1);
                                            else hae_wnimi(atoi(p),wstr);
                                            sprintf(x,"{message %s}",wstr);
                                            rcat(x);
                                            p=q+1;
                                            break;
                                  case 'E': ++p;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("error handler");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            sprintf(x,"{error handler %s}",p);
                                            rcat(x);
                                            p=q+1;
                                            break;

                                  case (unsigned char)'_': ++p;
                                            if (strcmp(p,"\375S")!=0)
                                                { t_error("repeat"); return(-1); }
                                            rcat("{repeat}");
                                            p+=2;
                                            break;

                                  case 'S': ++p;   // 10-11.11.2004
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("play sound");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            sprintf(x,"{play sound %s}",p);
                                            rcat(x);
                                            p=q+1;
                                            break;

                                  case 'T': ++p;   // 11.11.2004
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("play sound");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            if (*p=='C') strcpy(wstr,p+1);
                                            else hae_wnimi(atoi(p),wstr);
                                            sprintf(x,"{play sound %s}",wstr);
                                            rcat(x);
                                            p=q+1;
                                            break;

                                  case 'y': ++p;  // 13.6.2005
                                            strcpy(x,"{save spec2 ");
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("save spec2");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            hae_wnimi(atoi(p),wstr);
                                            strcat(x,wstr);
                                            strcat(x," ");
                                            p=q+1;
                                            q=strchr(p,'@');
                                            if (q==NULL) { t_error("save spec2");
                                                           return(-1);
                                                         }
                                            *q=EOS;
                                            hae_wnimi(atoi(p),wstr);
                                            strcat(x,wstr);
                                            strcat(x,"}");
                                            p=q+1;
                                            rcat(x);
                                            break;


                                  default:  rcat("{pre}T");
                                            sana2[0]=*p; rcat(sana2);
                                            ++p;
                                            break;
                                    }
                                break;
                                }
                                                                      // RS CHA '\375'
                            if (*p=='t' && (unsigned char)*(p+1)==(unsigned char) TUTCODE 
                                        && *(p+2)=='S')
                                {
                                rcat("{end}");
                                p+=3;
                                break;
                                }

                            if (*p=='t' && (unsigned char)*(p+1)==(unsigned char)'_'
                                        && strcmp(p+2,"\375S")==0 )
                                {
                                rcat("{repeat}");
                                p+=4;
                                break;
                                }

                            if (*p=='i')
                                {
                                ++p;
                                if (*p<'0') return(-3);  /* 6.3.1991 */  //RS CHA exit
                                tut_index=*p-'0'; ++p;
                                break;
                                }
                            if (*p=='=')
                                {
                                hae_wnimi(1,wstr);
                                sprintf(x,"{print %s}",wstr);
                                rcat(x);
                                ++p;
                                break;
                                }
                            if (*p=='@')
                                {
                                ++p;
                                if (*p=='!')
                                    {
                                    rcat("{save stack}");
                                    ++p; break;
                                    }
                                if (*p=='&')
                                    {
                                    rcat("{load stack}");
                                    ++p; break;
                                    }
                                if (*p=='~')  /* 16.5. 92 */
                                    {
                                    rcat("{next col}");
                                    ++p; break;
                                    }
                                i=*p-'0';
                                hae_wnimi(i,wstr);
                                sprintf(x,"{print %s}",wstr);
                                rcat(x);
                                ++p;
                                break;
                                }
                            if (*p=='#')
                                {
                                ++p;
                                i=*p-'0';
                                hae_wnimi(i,wstr);
                                sprintf(x,"{write %s}",wstr);
                                rcat(x);
                                ++p;
                                break;
                                }
                            if (*p=='W')
                                {
                                rcat("{next word}"); ++p;
                                break;
                                }
                            if (*p=='w')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                 /* sprintf(x,"{save word W%d}",tut_index); */
                                    sprintf(x,"{save word %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{stack word}");
                                ++p;
                                break;
                                }
                            if (*p=='x')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save char %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{stack char}");
                                ++p;
                                break;
                                }
                            if (*p=='C')
                                {
                                ++p; x3[0]=*p; x3[1]=EOS;
                                if (*p==' ') strcpy(x3,"sp");
                                sprintf(x,"{find %s}",x3); ++p;
                                rcat(x);
                                break;
                                }
                            if (*p=='0')
                                {
                                rcat("{del stack}"); ++p;
                                break;
                                }
                            if (*p=='y')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{get key %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{get key}");
                                ++p;
                                break;
                                }
                            if (*p=='R')
                                {
                                rcat("{stack cursor}"); ++p;
                                break;
                                }
                            if (*p=='L')
                                {
                                ++p;
                                if (*p=='1')
                                    {
                                    cursor_line=tut_index;  /* varmistus puuttuu! */
                                    tut_index=0;
                                    ++p;
                                    break;
                                    }
                                if (*p=='3')
                                    {
                                    corner_line=tut_index;  /* varmistus puuttuu! */
                                    tut_index=0;
                                    ++p;
                                    break;
                                    }
                                if (*p=='5')
                                    {
                                    dim_index=tut_index;  /* varmistus puuttuu! */
                                    tut_index=0;
                                    ++p;
                                    break;
                                    }
                                else if (*p=='Z') // 6.5.2004
                                    {
                                    win_lines=tut_index;
                                    tut_index=0;
                                    ++p;
                                    break;
                                    }
                                if (*p=='2')
                                    {
                                    hae_wnimi(cursor_line,x1);
                                    hae_wnimi(tut_index,x2);
                                    sprintf(x,"{save cursor %s,%s}",x1,x2);
                                    tut_index=0; cursor_line=0;
                                    }
                                else if (*p=='4')
                                    {
                                    hae_wnimi(corner_line,x1);
                                    hae_wnimi(tut_index,x2);
                                    sprintf(x,"{save corner %s,%s}",x1,x2);
                                    tut_index=0; corner_line=0;
                                    }
                                else if (*p=='6')
                                    {
                                    hae_wnimi(dim_index,x1);
                                    hae_wnimi(tut_index,x2);
                                    sprintf(x,"{save dim %s,%s}",x1,x2);
                                    tut_index=0; dim_index=0;
                                    }
                                else if (*p=='7')  /* 23.1.93 */
                                    {
                                    max_shadows=tut_index;  /* varmistus puuttuu! */
                                    tut_index=0;
                                    ++p;
                                    break;
                                    }
                                else if (*p=='8')
                                    {
                                    hae_wnimi(max_shadows,x1);
                                    hae_wnimi(tut_index,x2);
                                    sprintf(x,"{save shadowdim %s,%s}",x1,x2);
                                    tut_index=0; max_shadows=0;
                                    }
                                else if (*p=='z') // 6.5.2004
                                    {
                                    hae_wnimi(win_lines,x1);
                                    hae_wnimi(tut_index,x2);
                                    sprintf(x,"{save winsize %s,%s}",x1,x2);
                                    tut_index=0; win_lines=0;
                                    }

                                else if (*p=='O')          /* 16.5.1992 */
                                    {
                                    strcpy(x,"{disp on}");
                                    }
                                else if (*p=='o')
                                    {
                                    strcpy(x,"{disp off}");
                                    }
                                else if (*p=='0')          /* 4.9.1993 */
                                    {
                                    strcpy(x,"{disp reset}");
                                    }
                                else if (*p=='R' || *p=='r' || *p=='D')
                                    {
                                    switch (*p)
                                        {
                                      case 'R': sprintf(x,"{ref set %c}",*(p+1)); break;
                                      case 'r': sprintf(x,"{ref jump %c}",*(p+1)); break;
                                      case 'D': sprintf(x,"{ref del %c}",*(p+1)); break;
                                        }
                                    ++p;
                                    }
                                else if (*p=='L')          /* 25.9.1994 */
                                    {
                                    strcpy(x,"{labels on}");
                                    }
                                else if (*p=='l')
                                    {
                                    strcpy(x,"{labels off}");
                                    }
                                else if (*p=='W')          /* 27.4.1996 */
                                    {
                                    strcpy(x,"{waitrec on}");
                                    }
                                else if (*p=='w')
                                    {
                                    strcpy(x,"{waitrec off}");
                                    }
                                else if (*p=='P')          /* 17.11.1996 */
                                    {
                                    strcpy(x,"{paint on}");
                                    }
                                else if (*p=='p')
                                    {
                                    strcpy(x,"{paint off}");
                                    }
                                else if (*p=='F') /* 1.3.1999 */
                                    {
                                    if (tut_index)
                                        {
                                        hae_wnimi(tut_index,wstr);
                                        sprintf(x,"{save fieldtype %s}",wstr);
                                        tut_index=0;
                                        }
                                    else strcpy(x,"{save fieldtype}");
                                    }
                                else if (*p=='f') // 4.9.2000
                                    {
                                    if (tut_index)
                                        {
                                        hae_wnimi(tut_index,wstr);
                                        sprintf(x,"{save dataname %s}",wstr);
                                        tut_index=0;
                                        }
                                    else strcpy(x,"{save dataname}");
                                    }
                                else if (*p=='s') // 23.2.2003
                                    {
                                    if (tut_index)
                                        {
                                        hae_wnimi(tut_index,wstr);
                                        sprintf(x,"{save version %s}",wstr);
                                        tut_index=0;
                                        }
                                    else strcpy(x,"{save version}");
                                    }
                                else if (*p=='n') // 7.2.2006
                                    {
                                    if (tut_index)
                                        {
                                        hae_wnimi(tut_index,wstr);
                                        sprintf(x,"{save netsurvo %s}",wstr);
                                        tut_index=0;
                                        }
                                    else strcpy(x,"{save netsurvo}");
                                    }
                                else if (*p=='N') // 7.2.2006
                                    {
                                    if (tut_index)
                                        {
                                        hae_wnimi(tut_index,wstr);
                                        sprintf(x,"{save netpath %s}",wstr);
                                        tut_index=0;
                                        }
                                    else strcpy(x,"{save netpath}");
                                    }

                                else
                                    {
                                    sprintf(x,"{pre}L%c",*p);
                               /*   t_error("save cursor/corner, disp or ref set/jump");
                                    return(-1);
                               */
                                    }
                                rcat(x);
                                ++p;
                                break;
                                }
                            if (*p=='X')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save line %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save line}");
                                ++p;
                                break;
                                }
                            if (*p=='d')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save datapath %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save datapath}");
                                ++p;
                                break;
                                }
                            if (*p=='g')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save systemdisk %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save systemdisk}");
                                ++p;
                                break;
                                }
                            if (*p=='^')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save systempath %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save systempath}");
                                ++p;
                                break;
                                }
                            if (*p=='/')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save time %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save time}");
                                ++p;
                                break;
                                }
                            if (*p=='h')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save tempdisk %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save tempdisk}");
                                ++p;
                                break;
                                }
                            if (*p=='b')
                                {
                                rcat("{init}"); ++p;
                                break;
                                }
                            if (*p=='B')
                                {
                                rcat("{line start}"); ++p;
                                break;
                                }
                            if (*p=='Q')
                                {
                                rcat("{interaction on}"); ++p;
                                break;
                                }
                            if (*p=='q')
                                {
                                rcat("{interaction off}"); ++p;
                                break;
                                }
                            if (*p=='!' || *p==';')
                                {
                                strcpy(x2,"call"); if (*p=='!') strcat(x2,"s");
                                ++p; i=0;
                                                               // RS CHA '\375'
                                while ((unsigned char)*p!=(unsigned char) TUTCODE) 
                                    { x1[i]=*p; ++p; ++i; }
                                x1[i]=EOS;
                                sprintf(x,"{%s %s}",x2,x1);
                                rcat(x);
                                ++p; /* R */
                                ++p; break;
                                }
                            if (*p=='&')
                                {
                                ++p; i=0;
                                                               // RS CHA '\375'
                                while ((unsigned char)*p!=(unsigned char) TUTCODE)
                                    { x1[i]=*p; ++p; ++i; }
                                x1[i]=EOS;
                                sprintf(x,"{save field %s}",x1);
                                rcat(x);
                                ++p; /* R */
                                ++p; break;
                                }
                            if (*p=='%')
                                {
                                ++p; i=0;
                                                               // RS CHA '\375'
                                while ((unsigned char)*p!=(unsigned char) TUTCODE)
                                    { x1[i]=*p; ++p; ++i; }
                                x1[i]=EOS;
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save system %s %s}",x1,wstr);
                                    }
                                else sprintf(x,"{save system %s}",x1);
                                rcat(x);
                                ++p; /* R */
                                ++p; break;
                                }
                            if (*p=='o')
                                {
                                rcat("{stack default datapath}"); ++p;
                                break;
                                }
                            if (*p=='u')
                                {
                                ++p;
                                if (*p==' ')
                                    rcat("{message shadow}");
                                else
                                    {
                                    sprintf(x,"{message shadow %c}",*p);
                                    rcat(x);
                                    }
                                ++p;
                                break;
                                }
                            if (*p=='~')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save survotype %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save survotype}");
                                ++p;
                                break;
                                }
                                                          // RS CHA '\234'
                            if ((unsigned char)*p==(unsigned char) 234)
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save language %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save language}");
                                ++p;
                                break;
                                }
                                                          // RS CHA '\204'
                            if ((unsigned char)*p==(unsigned char) 204)
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save survo.apu %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save survo.apu}");
                                ++p;
                                break;
                                }
                                                          // RS CHA '\216'
                            if ((unsigned char)*p==(unsigned char) 216)
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save survo2.apu %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save survo2.apu}");
                                ++p;
                                break;
                                }
                                                          // RS CHA '\224'
                            if ((unsigned char)*p==(unsigned char) 224)
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save user %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save user}");
                                ++p;
                                break;
                                }
                            if (*p=='U')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save insertmode %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save insertmode}");
                                ++p;
                                break;
                                }
                            if (*p=='k')
                                {
                                if (tut_index)
                                    {
                                    hae_wnimi(tut_index,wstr);
                                    sprintf(x,"{save os %s}",wstr);
                                    rcat(x);
                                    tut_index=0;
                                    }
                                else rcat("{save os}");
                                ++p;
                                break;
                                }
                                                           // RS CHA '\375'
                            if ( (unsigned char)*p==(unsigned char) TUTCODE   // \375G\375
                                 && *(p+1)=='s') // 10-11.2.2001
                                {
                                rcat("{soft_off}");
                                p+=2; break;
                                }


                            rcat("{pre}");
                            break;

                      case 'R':
                            rcat("{R}"); kirjoita(rivi);
                            init_rivi(rivi); ++p;
                            break;
                      case 'H':
                            rcat("{home}"); ++p;
                            break;
                      case 'n':
                            rcat("{pgdn}"); ++p;
                            break;
                      case 'p':
                            rcat("{pgup}"); ++p;
                            break;
                      case 'e':
                            rcat("{erase}"); ++p;
                            break;
                      case 'i':
                            rcat("{ins}"); ++p;
                            break;
           /*         case 'v':
                            rcat("{del}"); ++p;
                            break;
           */
                      case 'I':
                            rcat("{ins line}"); ++p;
                            break;
                      case 'V':
                            rcat("{del line}"); ++p;
                            break;
                      case 'D':
                            rcat("{act}"); ++p;
                            break;
                      case '3':
                            rcat("{touch}"); ++p;
                            break;
            case 'r': case 'l': case 'u': case 'd': case 'v': case '5':
                            x3[0]=*p; x3[1]=EOS;
                            if (*p=='v') strcpy(x3,"del");
                            if (*p=='5') strcpy(x3,"form");
                            if (!crivin_alku)
                                { strcpy(crivi,p-1); crivin_alku=1; return(1); }
                            crivin_alku=0;
                            ch=*p; k=1;
                            while (1)
                                {
                                                               // RS CHA '\375'
                                if ((unsigned char)*(p+1)==(unsigned char) TUTCODE && *(p+2)==ch)
                                    { ++k; p+=2; }
                                else break;
                                }
                            if (k==1) sprintf(x,"{%s}",x3);
                            else sprintf(x,"{%s%d}",x3,k);
                            rcat(x); ++p;
                            strcpy(crivi,p); return(1);

/*                    case '5':
                            rcat("{form}"); ++p;
                            break;
*/
                      case 'x':
                            rcat("{tab}"); ++p;
                            break;
                      case 'E':
                            rcat("{line end}"); ++p;
                            break;
                      case '6':
                            rcat("{merge}"); ++p;
                            break;
                      case '7':
                            rcat("{ref}"); ++p;
                            break;
                      case '8':
                            rcat("{exit}"); ++p;
                            break;
                      case 'F':
                            rcat("{code}"); ++p;
                            break;
                      case 'B':
                            rcat("{copy}"); ++p;
                            break;
                      case '4':
                            rcat("{disk}"); ++p;
                            break;
                      case 'h':
                            rcat("{help}"); ++p;
                            break;
                      case '9':
                            rcat("{search}"); ++p;
                            break;
                      case 'z':
                            rcat("{file act}"); ++p;
                            break;
                      case 'M':
                            rcat("{block}"); ++p;
                            break;
                      case 'W':
                            rcat("{words}"); ++p;
                            break;
                      case 's': // 10.2.2001
                            rcat("{soft_on}"); ++p;
                            break;


                      default: sprintf(x,"\375%c",*p); rcat(x); ++p;
                        }
                    break;

              case '{': rcat("{(}"); ++p; break;
              case '}': rcat("{)}"); ++p; break;

              default:


                    sana2[0]=*p;
// RS DEBUG Rprintf("Selkodefault: sana2[0]=%c\n",sana2[0]);
                    rcat(sana2);
                    ++p;
                    break;
                } /* switch *p */
            } /* while (1) */
        return(1);
        }


static void talleta_dir()
        {
        int i;

        rewind(tutor);
        fprintf(tutor,"%cSURVO 84C SUCROS@%4d@%c",CODE_RETURN,nosat,CODE_RETURN);
        for (i=0; i<nosat; ++i)
            {
            fprintf(tutor,"%-8.8s %7ld@%c",osasukro[i],osaosoite[i],CODE_RETURN);
            }
        fputc(1,tutor); fputc(84,tutor); fputc(255,tutor);
        uusi_koodialku=ftell(tutor);
/*
printf("\nuusi_koodialku=%ld",uusi_koodialku); getch();
*/
        }

static int kopioi_alku()
        {
        int i;
        long li,l1;

/* printf("\nuusi_sukro=%d uusi_tiedosto=%d",uusi_sukro,uusi_tiedosto);
*/
        strcpy(apunimi,etmpd); strcat(apunimi,"SURVO.XXX");
        tutor=muste_fopen(apunimi,"wb");
        if (tutor==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s!",apunimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        o1=1;
        if (!uusi_tiedosto)
            {
            tutor2=muste_fopen(tiednimi,"rb");
            if (tutor2==NULL)
                {
                sprintf(sbuf,"\nCannot open file %s!",tiednimi);
                sur_print(sbuf); WAIT; return(-1);
                }
            o2=1;
            }
        if (uusi_tiedosto) osaosoite[0]=JAKSO1+JAKSO2+3;
        talleta_dir();

        if (!uusi_tiedosto)
            {
            muste_fseek(tutor2,(long)osaosoite[0],SEEK_SET);
            if (!uusi_sukro) l1=osaosoite[is];
            else l1=10000000L;
            for (li=osaosoite[0]; li<l1; ++li)
                {
                i=fgetc(tutor2);
                if (feof(tutor2)) break;
                fputc(i,tutor);
                }
/* printf("\nväli: %ld - %ld",osaosoite[0],li); getch();
*/
            }
            uusi_paikka1=ftell(tutor);

        return(1);
        }

static void lopeta_talletus()
        {
// RS REM        char x[LLENGTH];

//      if (tutor!=NULL) muste_fclose(tutor); if (tutor2!=NULL) muste_fclose(tutor2);
//      if (o1) muste_fclose(tutor); if (o2) muste_fclose(tutor2);

// RS FIXME Check closing of files
        muste_fclose(tutor); muste_fclose(tutor2); muste_fclose(txt_file); // RS CHA fcloseall();


// printf("\napunimi=%s tiednimi=%s|",apunimi,tiednimi);
// sur_sleep(1000); // virheen metsästys!
//      sur_sleep(100);
        sur_copy_file(apunimi,tiednimi);
//      sprintf(x,"COPY %s %s >NUL",apunimi,tiednimi); system(x);
        }

static int etsi_end_koodi()
        {
// RS REM        char *p;
        int j;
        char x[LLENGTH];
        char x2[]="\375GT\375S";
        char x3[]="\375GT_\375S";


        for (j=r1+r; j<=r2; ++j)
             {
             edread(x,j);
             if (strstr(x+1,"{end}")!=NULL) return(1);
             if (strstr(x+1,"{repeat}")!=NULL) return(1);
             if (strstr(x+1,x2)!=NULL) return(1);
             if (strstr(x+1,x3)!=NULL) return(1);
             }
        return(-1);
        }

static int tutki_wnimet(char *rivi)
        {
        int i;
        char x[LLENGTH];
        char *osa[1];
        char *p,*q;

        strcpy(x,rivi);
        i=split(x,osa,1);
        if (i==0) return(1);
        if (strcmp(osa[0],"def")!=0) return(1);
        strcpy(x,rivi);
        p=x;
        while (1)
            {
            q=strstr(p,"=W");
            if (q==NULL) break;
            *q=EOS;
            i=atoi(q+2);
            if (i<1 || i>MAXNIMET)
                {
                sprintf(sbuf,"\nIllegal W index (%d) on line:\n%.80s",
                                     i,rivi);
                sur_print(sbuf); WAIT; return(-1);
                }
            p=q; while (p>x && *p!=' ') --p;
            ++p;
            strncpy(wnimi[i-1],p,MAXPITUUS); wnimi[i-1][MAXPITUUS-1]=EOS;
            if (i>max_indeksi) max_indeksi=i;
            p=q+2;
            }
        return(1);
        }

static int lue()
        {
        int i;

        if (eol>r2)
            {
            PR_EBLD;
            sur_print("\nEnd of sucro text missing!");
            WAIT; PR_ENRM;
            return(-1);
            }
// RS FIXME check if koodi=0 is needed as it was commented out in the beginning
        if (koodi==0)
            {
            edread(rivi,eol); ++eol;
            if (*rivi=='/')
                {
                strcpy(rivi+1,comment_code); ++n_comments;
                return(1);
                }
            if (*rivi=='%') return(1);
            i=strlen(rivi); while (rivi[i-1]==' ') rivi[--i]=EOS;
            return(1);
            }
  /* koodi=1 */
        i=tulkitse_rivi(rivi);
        return(i);
        }

static void kopioi_loppu()
        {
// RS REM        long li,
        long muutos;
        int i;


        if (uusi_tiedosto) return;
        uusi_paikka2=ftell(tutor);
/*
printf("\nuusi koko on %d",(int)(uusi_paikka2-uusi_paikka1));
printf("\nvanha koko oli %d",(int)(osaosoite[is+1]-osaosoite[is]));
getch();
*/
        if (!uusi_sukro && is<nosat-1)
            {
            muste_fseek(tutor2,(long)osaosoite[is+1],SEEK_SET);
            while (1)
                {
                i=fgetc(tutor2);
                if (feof(tutor2)) break;
                fputc(i,tutor);
                }
                muutos=uusi_paikka2-uusi_paikka1-osaosoite[is+1]+osaosoite[is];
            for (i=is+1; i<nosat; ++i)
                {
                osaosoite[i]+=muutos;
                }
            }

        if (uusi_sukro)
            {
            osaosoite[is]=uusi_paikka1-JAKSO2;
            for (i=0; i<nosat; ++i) osaosoite[i]+=JAKSO2;
            }

        if (uusi_sukro || is<nosat-1)
            talleta_dir();
        }

static void talleta_kommentit(int j1,int j2)
        {
        int j,i;

        while (j2<=r2)
            {
            edread(rivi,j2);
            if (*rivi!='/') break;
            putc(TUT_COMMENT_CODE,tutor);
            ++n_comments;
            ++j2;
            }

        if (!n_comments) return;
/*      putc(255,tutor);       13.10.88 */
        j=j1;
        while (1)
            {
            if (j>r2) return;
            edread(rivi,j);
            if (j>j2-1 && *rivi!='/') return;
            if (*rivi!='/') { ++j; continue; }
            i=strlen(rivi);
            while (i>0 && rivi[i-1]==' ') rivi[--i]=EOS;
            fprintf(tutor,"%s\n",rivi+1);
            ++j;
            }
        }

static void check_gotos()
        {
        int i,j;
        int ilm=0;
        char *pgo;
/*
printf("\ngoto: ");
for (i=0; i<ngo; ++i) printf("%s ",tutgo[i]);
printf("\nlab: ");
for (i=0; i<nlab; ++i) printf("%s ",tutlab[i]);
getch();
*/
        for (i=0; i<ngo; ++i)
            {
            pgo=tutgo[i];
            for (j=0; j<nlab; ++j)
                if (strcmp(pgo,tutlab[j])==0) break;
            if (j==nlab)
                {
                if (!ilm) { PR_EBLD; sur_print("\nMissing labels:"); ++ilm; }
                sprintf(sbuf,"\ngoto %s (on line %d)",pgo,goline[i]);
                        sur_print(sbuf);
                }
            }
        if (ilm) { WAIT; }
        }


static void init_labels()
        {
        nlab=ngo=0;
        plab=labspace;
        no_labcheck=0;
        }

static void labcat(char *s)
        {
        char *p;

        if (plab-labspace>LABTILA-strlen(s)-2)
            {
            no_labcheck=1;
            return;
            }
        p=s; while (*p) *plab++=*p++;
        *plab++=EOS;
        }


static int add_label(char *s)
        {
        int i;

        if (no_labcheck) return(1);
        for (i=0; i<nlab; ++i)
            {
            if (strcmp(s,tutlab[i])==0)
                {
                sprintf(sbuf,"\nError on line %d: Label %s already in use!",
                            eol-1,s); sur_print(sbuf);
                WAIT; return(-1);
                }
            }
        if (nlab>=LABMAX) { no_labcheck=1; return(1); }
        tutlab[nlab++]=plab;
        labcat(s);
        return(1);
        }

static int indeksi(char *s)
        {
        int i;

        if (*s!='W') return(atoi(s));
        i=atoi(s+1); if (i>0) return(i);
        for (i=0; i<max_indeksi; ++i)
            {
            if (strcmp(s,wnimi[i])==0) return(i+1);
            }
        sprintf(sbuf,"\nWord %s is not defined!",s);
        sur_print(sbuf); WAIT; return(-1);
        }


static int indeksi2(char *s) // without error message!
        {
        int i;

        if (*s!='W') return(atoi(s));
        i=atoi(s+1); if (i>0) return(i);
        for (i=0; i<max_indeksi; ++i)
            {
            if (strcmp(s,wnimi[i])==0) return(i+1);
            }
        return(-1);
        }

static int indeksich(char *s,char *t)
        {
        int i;

        i=indeksi(s);
        if (i<0) return(-1);
        sprintf(t,"%d",i);
        return(1);
        }

static int indeksich2(char *s,char *t)
        {
        int i;

        i=indeksi2(s);
        if (i<0) return(-1);
        sprintf(t,"%d",i);
        return(1);
        }


static int tee_indeksi(char *s,char *koodi)
        {
        int i;

        i=indeksi(s); if (i<0) return(-1);
        strcpy(koodi,"\375Gi");
        koodi[3]=(char)('0'+i); koodi[4]=EOS;
        return(1);
        }

static int add_goto(char *s)
        {
        int i;

        if (no_labcheck) return(1);
        for (i=0; i<ngo; ++i) if (strcmp(s,tutgo[i])==0) return(1);
        if (ngo>=LABMAX) { no_labcheck=1; return(1); }
        goline[ngo]=eol-1;
        tutgo[ngo++]=plab;
        labcat(s);
        return(1);
        }


static int c_switch(char *rivi,char *x)
        {
        int i,k,h;
        char *p;
        char y[LLENGTH], *osa[4];
        char swlist[8*LLENGTH];
        int ncase;
        char s[LLENGTH];

        *swlist=EOS;
        strcpy(y,x+1);
        k=split(y,osa,2);
        if (k<2) { sprintf(sbuf,"\nError on line %d: switch without parameter!",eol-1);
                   sur_print(sbuf); WAIT; return(-1);
                 }

        strcpy(s,osa[1]);
        if (*osa[1]=='W')
            i=indeksich(osa[1],s);
        else i=atoi(osa[1]);
        if (i<=0) { sprintf(sbuf,"\nError on line %d: Illegal tutword index!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                  }
        strcat(rivi,"\375GTJ"); strcat(rivi,s); strcat(rivi,"@");
        ncase=0;
        while (1)
            {
            if (eol>r2)
                {
                sur_print("\nIncomplete switch statement!"); WAIT; return(-1);
                }
            edread(x,eol);
            ++eol;
            strcpy(y,x+1);
            k=split(y,osa,4);
            if (k==0)
                {
                if (ncase)  /* Tyhjän kommenttirivin takia  5.5.91 */
                    {
                    strcat(rivi,"@"); strcat(swlist,"@"); strcat(rivi,swlist);
                    --eol;
                    return(1);
                    }
                sprintf(sbuf,"\nError on line %d: Incomplete switch statement!",
                                eol-1); sur_print(sbuf); WAIT; return(-1);
                }
            if (strcmp(osa[0],"case")==0)
                {
                ++ncase;
                if (k<3) { sprintf(sbuf,"\nError on line %d: Incomplete case line!",eol-1);
                           sur_print(sbuf); WAIT; return(-1);
                         }
                i=strlen(osa[1]);
                if (osa[1][i-1]!=':')
                    {
                    sprintf(sbuf,"\nError on line %d: : missing!",eol-1); sur_print(sbuf); WAIT;
                    return(-1);
                    }
                osa[1][i-1]=EOS;
                while ((p=strchr(osa[1],'_'))!=NULL) *p=' ';
                strcat(rivi,osa[1]); strcat(rivi,"@");
                h=1;
                if (strcmp(osa[2],"goto")==0)
                    { strcat(swlist,"G"); add_goto(osa[3]); }
                else if (strcmp(osa[2],"load")==0)
                    strcat(swlist,"L");
                else if (strcmp(osa[2],"continue")==0)
                    h=0;
                else
                    {
                    sprintf(sbuf,"\nError on line %d: goto, load or continue missing!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                if (h && k<4) { sprintf(sbuf,"\nError on line %d: Incomplete case line!",eol-1);
                                sur_print(sbuf); WAIT; return(-1);
                              }
                if (h) strcat(swlist,osa[3]); strcat(swlist,"@");
                continue;
                }
            h=1;
            if (strcmp(osa[0],"default:")==0)
                {
                if (strcmp(osa[1],"goto")==0)
                    { strcat(swlist,"G"); add_goto(osa[2]); }
                else if (strcmp(osa[1],"load")==0)
                    strcat(swlist,"L");
                else if (strcmp(osa[1],"continue")==0)
                    h=0;
                else
                    {
                    sprintf(sbuf,"\nError on line %d: goto, load or continue missing!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                if (h) strcat(swlist,osa[2]);
                strcat(swlist,"@");
                strcat(rivi,"@"); strcat(rivi,swlist);
                return(1);
                }

            if (ncase)
                {
                strcat(rivi,"@"); strcat(swlist,"@"); strcat(rivi,swlist);
                --eol;
                return(1);
                }
            else
                {
                sprintf(sbuf,"\nError on line %d: Incorrect switch statement!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }

            }  /* while */
        return(1);
        }

static int c_prompt(char *rivi,char *x)
        {
        int i,k;
        char y[LLENGTH], *osa[2];
        char prompt[LLENGTH];
        char def[LLENGTH];
        char length[LLENGTH];
        char wait[LLENGTH];
        char *p;

        strcpy(y,x+1);
        p=y; while (*p==' ') ++p;
        while (*p && *p!=' ') ++p;
        if (*p==EOS) { *prompt=EOS; }
        else
            {
            ++p; i=strlen(p); while (p[i-1]==' ') p[--i]=EOS;
            if (p[i-1]=='}') p[i-2]=EOS;
            strcpy(prompt,p);
            }
        edread(x,eol); ++eol;
        strcpy(y,x+1);
        k=split(y,osa,2);
        if (strcmp(osa[0],"default")!=0)
            {
            sprintf(sbuf,"\nError on line %d: default missing!",eol-1); sur_print(sbuf); WAIT;
            return(-1);
            }
        if (k==1) { *def='C'; def[1]=EOS; }
        else
            {
            if (*osa[1]!='W')
                {
                k=osa[1]-y; strcpy(y,x+k+1);
                *def='C'; def[1]=EOS; strcat(def,y);
                k=strlen(def); while(k>1 && def[k-1]==' ') def[--k]=EOS;
                if (strcmp(def,"C{}")==0) def[1]=EOS;
                }
            else
                {
                i=indeksich(osa[1],def); if (i<0) return(-1);
                }
            }

        edread(x,eol); ++eol;
        strcpy(y,x+1);
        k=split(y,osa,2);

        if (strcmp(osa[0],"answer")==0)
            {
            i=tee_indeksi(osa[1],length); if (i<0) return(-1);
            strcat(rivi,length);
            edread(x,eol); ++eol;
            strcpy(y,x+1);
            k=split(y,osa,2);
            }

        if (strcmp(osa[0],"length")!=0 || k<2)
            {
            sprintf(sbuf,"\nError on line %d: length missing!",eol-1); sur_print(sbuf); WAIT;
            return(-1);
            }
        strcpy(length,osa[1]);

        edread(x,eol); ++eol;
        strcpy(y,x+1);
        k=split(y,osa,2);
        if (strcmp(osa[0],"wait")!=0 || k<2)
            {
            sprintf(sbuf,"\nError on line %d: wait missing!",eol-1); sur_print(sbuf); WAIT;
            return(-1);
            }
        if (*osa[1]!='W') { *wait='C'; wait[1]=EOS; strcat(wait,osa[1]); }
     /* else  strcpy(wait,osa[1]+1);  */
        else
            {
            i=indeksich(osa[1],wait); if (i<0) return(-1);
            }
        sprintf(y,"\375GTK%s@%s@%s@%s@",wait,prompt,def,length);
        strcat(rivi,y);
        return(1);
        }

static int c_key(char *rivi,char *x)
        {
        int i,k,h;
        char y[4*LLENGTH], *osa[4];    /* -16.4.1996 2*LLENGTH */
        char keys[LLENGTH];
        char keylist[4*LLENGTH];       /* -16.4.1996 2*LLENGTH */
        char wait[LLENGTH];

        i=0; *keylist=EOS;
        while (1)
            {
            edread(x,eol); ++eol;
            strcpy(y,x+1);
            k=split(y,osa,4);
            if (strcmp(osa[0],"wait")==0) break;
            if (k<3 || strcmp(osa[0],"key")!=0)
                {
                sprintf(sbuf,"\nError on line %d: wait or key expected!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (muste_strnicmp(osa[1],"ENTER",5)==0) keys[i]=CODE_RETURN;
            else if (muste_strnicmp(osa[1],"RIGHT",5)==0) keys[i]=CODE_RIGHT;
            else if (muste_strnicmp(osa[1],"LEFT",4)==0) keys[i]=CODE_LEFT;
            else if (muste_strnicmp(osa[1],"UP",2)==0) keys[i]=CODE_UP;
            else if (muste_strnicmp(osa[1],"DOWN",4)==0) keys[i]=CODE_DOWN;
            else if (muste_strnicmp(osa[1],"HOME",4)==0) keys[i]=CODE_HOME;
            else if (muste_strnicmp(osa[1],"HELP",4)==0) keys[i]=(unsigned char)CODE_HELP;
            else if (muste_strnicmp(osa[1],"ESC",3)==0) keys[i]=(unsigned char)CODE_EXEC;
            else if (muste_strnicmp(osa[1],"SP",2)==0) keys[i]=' ';
            else if (muste_strnicmp(osa[1],"COLON",5)==0) keys[i]=':';
            else if (muste_strnicmp(osa[1],"INSERT",6)==0) keys[i]=CODE_INSERT;
            else if (muste_strnicmp(osa[1],"INS_LINE",8)==0) keys[i]=CODE_INSERTL;
            else if (muste_strnicmp(osa[1],"DELETE",6)==0) keys[i]=CODE_DELETE;
            else if (muste_strnicmp(osa[1],"DEL_LINE",8)==0) keys[i]=CODE_DELETEL;
            else if (muste_strnicmp(osa[1],"ERASE",5)==0) keys[i]=CODE_ERASE;
            else if (muste_strnicmp(osa[1],"NEXT",4)==0) keys[i]=CODE_NEXT;
            else if (muste_strnicmp(osa[1],"PREV",4)==0) keys[i]=CODE_PREV;
            else if (muste_strnicmp(osa[1],"DISK",4)==0) keys[i]=(unsigned char)CODE_DISK;
            else if (muste_strnicmp(osa[1],"BACKSP",6)==0) keys[i]=CODE_BACKSP;
            else if (muste_strnicmp(osa[1],"REF",3)==0) keys[i]=(unsigned char)CODE_REF;
            else if (muste_strnicmp(osa[1],"MERGE",5)==0) keys[i]=(unsigned char)CODE_MERGE;
            else if (muste_strnicmp(osa[1],"COPY",4)==0) keys[i]=(unsigned char)CODE_COPY;
            else if (muste_strnicmp(osa[1],"TAB",3)==0) keys[i]=CODE_TAB;
            else if (muste_strnicmp(osa[1],"HELP",4)==0) keys[i]=(unsigned char)CODE_HELP;
            else if (muste_strnicmp(osa[1],"SRCH",4)==0) keys[i]=(unsigned char)CODE_SRCH;
            else if (muste_strnicmp(osa[1],"ACTIV",5)==0) keys[i]=CODE_ACTIV;
            else if (muste_strnicmp(osa[1],"MOVE",4)==0) keys[i]=(unsigned char)CODE_MOVE;
            else if (muste_strnicmp(osa[1],"END",3)==0) keys[i]=(unsigned char)CODE_END;
            else if (muste_strnicmp(osa[1],"WORDS",3)==0) keys[i]=(unsigned char)CODE_WORDS;

            else
                keys[i]=*osa[1];
            h=1;
            if (strcmp(osa[2],"goto")==0)
                { strcat(keylist,"G"); add_goto(osa[3]); }
            else if (strcmp(osa[2],"load")==0) strcat(keylist,"L");
            else if (strcmp(osa[2],"continue")==0) h=0;
            else     /* 26.5.93 */
                {
                sprintf(sbuf,"\nError on line %d: `%s' not permitted!",eol-1,osa[2]);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (h && k<4)
                {
                sprintf(sbuf,"\nError on line %d: %s ???",eol-1,osa[2]);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (h) strcat(keylist,osa[3]);
            strcat(keylist,"@");
            ++i;
            }
        if (k<2)
            {
            sprintf(sbuf,"\nError on line %d: wait ???",eol-1);
            sur_print(sbuf); WAIT; return(-1);
            }
        keys[i]=EOS;
        if (*osa[1]=='W')
            {
            i=indeksich(osa[1],wait); if (i<0) return(-1);
            }
        else { strcpy(wait,"C"); strcat(wait,osa[1]); }
        sprintf(y,"\375GTV%s@%s@%s",wait,keys,keylist);
        strcat(rivi,y);
        return(1);
        }

static int set_wexpr(char *w)
    {
    int i;
    char s[32];

    strcpy(s,w);
    if (*s!='W') return(1);

    i=indeksich(s,w+1); if (i<0) return(-1);
    *w='W';
    return(1);
    }


static int if2_muunto1(char *s,char *t)
    {
    int i;
    char *p;
    char w1[5],w2[5];
    char op;

    strcpy(sbuf,s);
    p=sbuf;
    while (*p)
        {
        if(strchr("+-*/",*p)!=NULL)
            {
            op=*p;
            *p=EOS; strcpy(w1,sbuf); strcpy(w2,p+1);
            i=set_wexpr(w1); if (i<0) return(-1);
            i=set_wexpr(w2); if (i<0) return(-1);
            sprintf(t,"%s%c%s",w1,op,w2);
            return(1);
            }
        ++p;
        }
    i=set_wexpr(sbuf); if (i<0) return(-1); strcpy(t,sbuf);
    return(0);
    }

static int lgmuunto(char *s1,char *s2,char *jump)
        {
        if (strcmp(s1,"goto")==0) { strcpy(jump,"G"); add_goto(s2); }
        else if (strcmp(s1,"load")==0) strcpy(jump,"L");
        else { sur_print("\ngoto or load missing!"); return(-1); }
        strcat(jump,s2);
        return(1);
        }

static void iferror(int i)
        { sprintf(sbuf,"\nError on line %d:",i); sur_print(sbuf);
          sur_print("\nCorrect form: if <word1> <condition> <word2> then goto <label>");
          sur_print("\n          or: if <word1> <condition> <word2> then load <tutor>");
          sur_print("\nPossible continuation: else goto <label>    or    else load <tutor>");
          WAIT;
        }



static int c_if2(char *rivi,char *x)
        {
        int i,k; // RS REM ,h;
// RS REM        char *p;
        char y[LLENGTH], *osa[10];
// RS REM        char word1[LLENGTH];
        char vert[LLENGTH];
// RS REM        char word2[LLENGTH];
        char jump[2][LLENGTH];
// RS REM        char laji;
        int j[3];
        char expr1[32],expr2[32];



/*
    if expr1  rel  expr2 then jump1 else jump2
    0    1     2     3     4    5 6   7    8 9
*/
/* printf("\nx=%s",x); getch();   */
        strcpy(y,x+1);
        k=split(y,osa,10);

        if (*osa[2]=='\'') return(0); // sivuutetaan ei-num.ehdot

        i=if2_muunto1(osa[1],expr1);
        i+=if2_muunto1(osa[3],expr2);
// printf("\ni=%d expr1=%s expr2=%s|",i,expr1,expr2); getch();
        if (i==0) return(0); // simple if


        strcpy(vert,osa[2]);
        if (strcmp(vert,"<")==0) { j[0]=0; j[1]=j[2]=1; }
        else if (strcmp(vert,"=")==0) { j[1]=0; j[0]=j[2]=1; }
        else if (strcmp(vert,">")==0) { j[2]=0; j[0]=j[1]=1; }
        else if (strcmp(vert,"<=")==0) { j[2]=1; j[0]=j[1]=0; }
        else if (strcmp(vert,">=")==0) { j[0]=1; j[1]=j[2]=0; }
        else if (strcmp(vert,"<>")==0) { j[1]=1; j[0]=j[2]=0; }
        else
            {
            sprintf(sbuf,"\nError on line %d: Incorrect relation %s",eol-1,osa[2]);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (strcmp(osa[4],"then")!=0)
            {
            sprintf(sbuf,"\nError on line %d: then missing!",eol-1);
            sur_print(sbuf); WAIT; return(-1);
            }
        i=lgmuunto(osa[5],osa[6],jump[0]);
        if (i<0) { iferror(eol-1); return(-1); } // RS eol-1
        *jump[1]=EOS;
        if (k>7)
            {
            if (k<10)
                {
                sprintf(sbuf,"\nError on line %d: else!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (strcmp(osa[7],"else")!=0)
                {
                sprintf(sbuf,"\nError on line %d: else missing!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            i=lgmuunto(osa[8],osa[9],jump[1]);
            if (i<0) { iferror(eol-1); return(-1); } // RS eol-1
            }

        sprintf(y,"\375GTi|%s|@|%s|@%s@%s@%s@",
                       expr1,expr2,jump[j[0]],jump[j[1]],jump[j[2]]);
// printf("\ny=%s",y); getch();
        strcat(rivi,y);


        return(1);
        }

static void korvaa_sp(char *s)
        {
        if (strcmp(s,"C{sp}")==0) { s[1]=' '; s[2]=EOS; return; }
        if (strcmp(s,"C{comma}")==0) { s[1]=','; s[2]=EOS; return; }
        }


static int c_if(char *rivi,char *x)
        {
        int i,k; // RS REM ,h;
        char *p;
        char y[LLENGTH], *osa[10];
        char word1[LLENGTH];
        char vert[LLENGTH];
        char word2[LLENGTH];
        char jump[2][LLENGTH];
        char laji;
        int j[3];

/*
    if word1  rel  word2 then jump1 else jump2
    0    1     2     3     4    5 6   7    8 9
*/
/* printf("\nx=%s",x); getch();   */
        strcpy(y,x+1);
        k=split(y,osa,10);
        if (k<7) { iferror(eol-1); return(-1); }

// 17.11.2000:
        i=c_if2(rivi,x); // generalized if lauseke rel lauseke ...
        if (i==1) return(1);
        if (i<0) return(-1);

        if (strcmp(osa[1],"{}")==0) *osa[1]=EOS;
        if (*osa[1]=='W') /* strcpy(word1,osa[1]+1);  */
            {
            i=indeksich(osa[1],word1); if (i<0) return(-1);
            }
        else { strcpy(word1,"C"); strcat(word1,osa[1]); }
        while ((p=strchr(word1,'_'))!=NULL) *p=' ';
        korvaa_sp(word1);
        if (strcmp(osa[3],"{}")==0) *osa[3]=EOS;
        if (*osa[3]=='W' && osa[3][1]!=EOS) /* strcpy(word2,osa[3]+1); */
            {
            i=indeksich(osa[3],word2); if (i<0) return(-1);
            }
        else { strcpy(word2,"C"); strcat(word2,osa[3]); }
        while ((p=strchr(word2,'_'))!=NULL) *p=' ';
        korvaa_sp(word2);
        if (*osa[2]=='\'')
            {
            laji='A';
            i=strlen(osa[2]);
            if (osa[2][i-1]=='\'') { osa[2][i-1]=EOS; strcpy(vert,osa[2]+1); }
            else { iferror(eol-1); return(-1); }
            }
        else { laji='N'; strcpy(vert,osa[2]); }
        if (strcmp(vert,"<")==0) { j[0]=0; j[1]=j[2]=1; }
        else if (strcmp(vert,"=")==0) { j[1]=0; j[0]=j[2]=1; }
        else if (strcmp(vert,">")==0) { j[2]=0; j[0]=j[1]=1; }
        else if (strcmp(vert,"<=")==0) { j[2]=1; j[0]=j[1]=0; }
        else if (strcmp(vert,">=")==0) { j[0]=1; j[1]=j[2]=0; }
        else if (strcmp(vert,"<>")==0) { j[1]=1; j[0]=j[2]=0; }
        else
            {
            sprintf(sbuf,"\nError on line %d: Incorrect relation %s",eol-1,osa[2]);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (strcmp(osa[4],"then")!=0)
            {
            sprintf(sbuf,"\nError on line %d: then missing!",eol-1);
            sur_print(sbuf); WAIT; return(-1);
            }
        i=lgmuunto(osa[5],osa[6],jump[0]);
        if (i<0) { iferror(eol-1); return(-1); } // RS eol-1
        *jump[1]=EOS;
        if (k>7)
            {
            if (k<10)
                {
                sprintf(sbuf,"\nError on line %d: else!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (strcmp(osa[7],"else")!=0)
                {
                sprintf(sbuf,"\nError on line %d: else missing!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            i=lgmuunto(osa[8],osa[9],jump[1]);
            if (i<0) { iferror(eol-1); return(-1); } // RS eol-1
            }

        sprintf(y,"\375GTI%c%s@%s@%s@%s@%s@",
                       laji,word1,word2,jump[j[0]],jump[j[1]],jump[j[2]]);
/*  printf("\ny=%s",y); getch(); */
        strcat(rivi,y);
        return(1);
        }


static int spec_code(char *rivi,char *x)
        {
        int i,k;
        char *p,*q;
        char y[LLENGTH], *osa[2];

        if (*x=='+')  /* label */
            {
            p=x+1; while (*p && *p==' ') ++p;
            if (*p==EOS) { sprintf(sbuf,"\nError on line %d: Label missing!",eol);
                           sur_print(sbuf); WAIT; return(-1);
                         }
            q=strchr(p,':');
            if (q==NULL) { sprintf(sbuf,"\nError on line %d: : missing!",eol);
                           sur_print(sbuf); WAIT; return(-1);
                         }
            *q=EOS;
            strcat(rivi,"\375GTX"); strcat(rivi,p); strcat(rivi,"@");
            i=add_label(p); if (i<0) return(-1);
            p=q+2;  /* + label: jatko  */
            *x='*'; strcpy(x+1,p); return(2);
            }
        /* *x='-' */
            {
            strcpy(y,x+1);
            k=split(y,osa,2);
            if (k==0) return(1);  /* tyhjä rivi */
            if (strcmp(osa[0],"switch")==0) { i=c_switch(rivi,x); return(i); }
            if (strcmp(osa[0],"prompt")==0) { i=c_prompt(rivi,x); return(i); }
            if (strcmp(osa[0],"on")==0) { i=c_key(rivi,x); return(i); }
            if (strcmp(osa[0],"if")==0) { i=c_if(rivi,x); return(i); }


            sprintf(sbuf,"\nUnknown control line %d: %s",eol-1,x); sur_print(sbuf); WAIT;
            return(-1);
            }
        return(1);
        }

static int arit_error(char *s)
        {
        sprintf(sbuf,"\nError in tutstack arithmetics on line %d. %s",eol-1,s);
        sur_print(sbuf); WAIT; return(1);
        }

static int tutor_op_arit(char *rivi,char *p)
        {
        int i;
        char *q;
// RS REM        char x[2];
        char op;
        char *loppu;
        char y[LLENGTH];
        int jono;

        loppu=rivi+strlen(rivi);
        strcat(rivi,"\375GT=");
        q=strchr(p,'=');
        if (q==NULL) { arit_error("= missing!"); return(-1); }
        *q=EOS; ++q;
/*      strcat(rivi,p+1); strcat(rivi,"@");    */
        i=indeksich(p,y); if (i<0) return(-1); strcat(rivi,y); strcat(rivi,"@");

        if (*q==EOS) /* {Wi=} 27.4.1994 */
            {
            *(loppu+3)='!'; strcat(rivi,"C@");
            return(1);
            }

        if (*q=='-' && *(q+1)=='W') { --q; *q='0'; } /* 15.3.91 -W1 --> 0-W1 */

        p=q;

        jono=0; // jono=1, jos esim. {Wi=Survo-ohjelma}
        if (strchr("+-0123456789W",*p)==NULL) jono=1;
        ++q;   /* ohita mahdollinen + tai - */
        while (*q)
            {
            if (strchr("+-*/%&",*q)!=NULL) break;
            ++q;
            }
        if (!jono && *q)
            {
            op=*q;
            *q=EOS;
            ++q;
            }
        else
            {
            *(loppu+3)='!';       /* \375GT! */
     /*     if (*p=='W') strcat(rivi,p+1);     */
            if (*p=='W') { i=indeksich(p,y); if (i<0) return(-1); strcat(rivi,y); }
            else { strcat(rivi,"C"); strcat(rivi,p); }
            strcat(rivi,"@");
            return(1);
            }
    /*  if (*p=='W') strcat(rivi,p+1);      */
        if (*p=='W') { i=indeksich(p,y); if (i<0) return(-1); strcat(rivi,y); }
        else { strcat(rivi,"C"); strcat(rivi,p); }
        strcat(rivi,"@");
        *p=op; *(p+1)=EOS; strcat(rivi,p); strcat(rivi,"@");
   /*   if (*q=='W') strcat(rivi,q+1);     */
        if (*q=='W') { i=indeksich(q,y); if (i<0) return(-1); strcat(rivi,y); }
        else { strcat(rivi,"C"); strcat(rivi,q); }
        strcat(rivi,"@");
        return(1);
        }


static int muunna(char *rivi,char *s)
        {
        int i,k;
        int n;
        char sana[8];
        char *p;
        char *osa[4];
        char *q;
        char x[LLENGTH];
        char y[2];

        if (*s==EOS) return(1); /* {} rivin lopussa */
        i=atoi(s);
        if (i>0)  /* {23} = {wait 23} 26.5.1995 */
            {
            strcat(rivi,"\375GTW"); strcat(rivi,s); strcat(rivi,"@");
            return(1);
            }
        for (i=0; i<nword; ++i)
            {
            if (strcmp(s,at[i])==0)
                {
                strcat(rivi,bt[i]);
                return(1);
                }
            }
        if (*s=='r' || *s=='l' || *s=='u' || *s=='d')
            {
            n=atoi(s+1);
            if (n>0)
                {
               // RS CHA '\375'
                sana[0]=(unsigned char)TUTCODE; sana[1]=*s; sana[2]=EOS;
                for (i=0; i<n; ++i) strcat(rivi,sana);
                return(1);
                }
            }

        if (strncmp(s,"del",3)==0)
            {
            n=atoi(s+3);
            if (n>0)
                {
                for (i=0; i<n; ++i) strcat(rivi,bt[6]);
                return(1);
                }
            }
        if (strncmp(s,"form",4)==0)
            {
            n=atoi(s+4);
            if (n>0)
                {
                for (i=0; i<n; ++i) strcat(rivi,bt[19]);
                return(1);
                }
            }

        if (strncmp(s,"goto ",5)==0)
            {
            p=s+5; while (*p==' ') ++p;
            strcat(rivi,"\375GTLG"); strcat(rivi,p); strcat(rivi,"@");
            add_goto(p);
            return(1);
            }
        if (strncmp(s,"load ",5)==0)
            {
            n=split(s,osa,3);
            if (n<2)
                {
                sprintf(sbuf,"\nError on line %d: Invalid load!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (n==3 && strcmp(osa[1],"stack")==0)
                {
                strcat(rivi,"\375GT<");
                p=osa[2]; strcpy(x,p);
                if (*p=='W')
                    {
                    n=indeksich(p,x); if (n<0) return(-1);
                    }
                else strcat(rivi,"C");
                strcat(rivi,x); strcat(rivi,"@");
                return(1);
                }
            else if (n==3 && strcmp(osa[1],"sucro")==0)  /* 17.9.1995 */
                {
                p=osa[2]; strcpy(x,p);
                if (*p=='W')
                    {
                    strcat(rivi,"\375GTL?");
                    n=indeksich(p,x); if (n<0) return(-1);
                    strcat(rivi,x); strcat(rivi,"@");
                    return(1);
                    }
                }
            else     /* {load <sucro>} */
                {
                strcat(rivi,"\375GTL"); strcat(rivi,osa[1]); strcat(rivi,"@");
                return(1);
                }
            }
        if (strncmp(s,"save ",5)==0)
            {
            n=split(s,osa,4);
            if (n<3)
                {
                sprintf(sbuf,"\nError on line %d: Invalid save!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (strcmp(osa[1],"word")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375Gw");
                return(1);
                }
            if (strcmp(osa[1],"char")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375Gx");
                return(1);
                }
            if (strcmp(osa[1],"cursor")==0)
                {
                if (n<4)
                    {
                    sprintf(sbuf,"\nError on line %d: Invalid save cursor!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GL1");
                i=tee_indeksi(osa[3],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GL2");
                return(1);
                }
            if (strcmp(osa[1],"corner")==0)
                {
                if (n<4)
                    {
                    sprintf(sbuf,"\nError on line %d: Invalid save corner!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GL3");
                i=tee_indeksi(osa[3],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GL4");
                return(1);
                }
            if (strcmp(osa[1],"dim")==0)
                {
                if (n<4)
                    {
                    sprintf(sbuf,"\nError on line %d: Invalid save dim!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GL5");
                i=tee_indeksi(osa[3],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GL6");
                return(1);
                }
            if (strcmp(osa[1],"shadowdim")==0)
                {
                if (n<4)
                    {
                    sprintf(sbuf,"\nError on line %d: Invalid save shadowdim!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GL7");
                i=tee_indeksi(osa[3],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GL8");
                return(1);
                }
            if (strcmp(osa[1],"winsize")==0) // 6.5.2004
                {
                if (n<4)
                    {
                    sprintf(sbuf,"\nError on line %d: Invalid save winsize!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GLZ");
                i=tee_indeksi(osa[3],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GLz");
                return(1);
                }
            if (strcmp(osa[1],"line")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GX");
                return(1);
                }
            if (strcmp(osa[1],"datapath")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375Gd");
                return(1);
                }
            if (strcmp(osa[1],"systemdisk")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375Gg");
                return(1);
                }
            if (strcmp(osa[1],"systempath")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375G^");
                return(1);
                }
            if (strcmp(osa[1],"time")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375G/");
                return(1);
                }
            if (strcmp(osa[1],"tempdisk")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375Gh");
                return(1);
                }
            if (strcmp(osa[1],"stack")==0)
                {
                strcat(rivi,"\375GT>");
                p=osa[2]; strcpy(x,p);
                if (*p=='W')
                    {
                    n=indeksich(p,x); if (n<0) return(-1);
                    }
                else strcat(rivi,"C");
                strcat(rivi,x); strcat(rivi,"@");
                return(1);
                }
            if (strcmp(osa[1],"field")==0)
                {
                strcat(rivi,"\375G&");
                strcat(rivi,osa[2]); strcat(rivi,"\375R");
                return(1);
                }
            if (strcmp(osa[1],"system")==0)
                {
                if (n<4)
                    {
                    sprintf(sbuf,"\nError on line %d: Invalid save system!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                i=tee_indeksi(osa[3],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375G%");
                strcat(rivi,osa[2]); strcat(rivi,"\375R");
                return(1);
                }
            if (strcmp(osa[1],"spec")==0)
                {
                if (n<4)
                    {
                    sprintf(sbuf,"\nError on line %d: Invalid save spec!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                i=tee_indeksi(osa[3],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GTKC~");
                strcat(rivi,osa[2]); strcat(rivi,"@");
                return(1);
                }

            if (strcmp(osa[1],"spec2")==0)  // {save spec2 Wx Wy}
                {
                if (n<4)
                    {
                    sprintf(sbuf,"\nError on line %d: Invalid save spec!",eol-1);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                strcat(rivi,"\375GTy");
                i=indeksich(osa[2],y); if (i<0) return(-1); strcat(rivi,y); strcat(rivi,"@");
                i=indeksich(osa[3],y); if (i<0) return(-1); strcat(rivi,y); strcat(rivi,"@");
                return(1);
                }

            if (strcmp(osa[1],"survotype")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375G~");
                return(1);
                }
            if (strcmp(osa[1],"language")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375G\234");
                return(1);
                }
            if (strcmp(osa[1],"survo.apu")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375G\204");
                return(1);
                }
            if (strcmp(osa[1],"survo2.apu")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375G\216");
                return(1);
                }
            if (strcmp(osa[1],"user")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375G\224");
                return(1);
                }
            if (strcmp(osa[1],"insertmode")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GU");
                return(1);
                }
            if (strcmp(osa[1],"fieldtype")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GLF");
                return(1);
                }
            if (strcmp(osa[1],"dataname")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GLf");
                return(1);
                }
            if (strcmp(osa[1],"os")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375Gk");
                return(1);
                }
            if (strcmp(osa[1],"version")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GLs");
                return(1);
                }
            if (strcmp(osa[1],"netsurvo")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GLn");
                return(1);
                }
            if (strcmp(osa[1],"netpath")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375GLN");
                return(1);
                }

     sprintf(sbuf,"\nUnknown command on line %d: save %s",eol-1,osa[1]);
     sur_print(sbuf); WAIT;
     return(-1);


            }

        if (strncmp(s,"get ",4)==0)
            {
            n=split(s,osa,3);
            if (n<3)
                {
                sprintf(sbuf,"\nError on line %d: Invalid get!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (strcmp(osa[1],"key")==0)
                {
                i=tee_indeksi(osa[2],sana); if (i<0) return(-1);
                strcat(rivi,sana); strcat(rivi,"\375Gy");
                return(1);
                }

     sprintf(sbuf,"\nUnknown command on line %d: get %s",eol-1,osa[1]);
     sur_print(sbuf); WAIT;
     return(-1);


            }

        if (strncmp(s,"print ",6)==0)
            {
            p=s+6; while (*p==' ') ++p;
         /* if (*p=='W') ++p;
            n=atoi(p);
         */ n=indeksi(p);
            if (n<=0)
                {
                sprintf(sbuf,"\nError on line %d: Illegal tutindex in print!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            if (n==1)
                {
                strcat(rivi,"\375G=");
                }
            else
                {
                *s=(char)('0'+n); s[1]=EOS;
                strcat(rivi,"\375G@"); strcat(rivi,s);
                }
            return(1);
            }
        if (strncmp(s,"write ",6)==0)
            {
            p=s+6; while (*p==' ') ++p;
        /*  if (*p=='W') ++p;
            n=atoi(p);
        */  n=indeksi(p);
            if (n<=0)
                {
                sprintf(sbuf,"\nError on line %d: Illegal tutindex in write!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            *s=(char)('0'+n); s[1]=EOS;
            strcat(rivi,"\375G#"); strcat(rivi,s);
            return(1);
            }
        if (strncmp(s,"wait ",5)==0)
            {
            strcat(rivi,"\375GTW"); strcat(rivi,s+5); strcat(rivi,"@");
            return(1);
            }
        if (strncmp(s,"tempo ",6)==0)
            {
            p=s+6; while (*p==' ') ++p;
            if (*p=='+' || *p=='-')
                {
                if (*p=='+') *p='-'; else ++p;
                strcat(rivi,"\375GTa"); strcat(rivi,p); strcat(rivi,"@");
                }
            else
                {
/* -24.9.92     strcat(rivi,"\375GTs"); strcat(rivi,s+6); strcat(rivi,"@"); */
                strcat(rivi,"\375GTt");
                if (*p=='W') { i=indeksich(p,x); if (i<0) return(-1); strcat(rivi,x); }
                else { strcat(rivi,"C"); strcat(rivi,p); }
                strcat(rivi,"@");
                }
            return(1);
            }
        if (strncmp(s,"find ",5)==0)
            {
            p=s+5; while (*p && *p==' ') ++p;
            if (*p==EOS || strcmp(p,"sp")==0) *p=' ';

            if (strncmp(p,"string ",7)==0)
                {
                p+=7; while (*p && *p==' ') ++p;
                strcat(rivi,"\375GTw");
                if (*p=='W') { i=indeksich(p,x); if (i<0) return(-1); strcat(rivi,x); }
                else { strcat(rivi,"C"); strcat(rivi,p); }
                strcat(rivi,"@");
                return(1);
                }

            if (*p=='W' && *(p+1) && *(p+1)!=' ')   /* 18.2.90 */
                {
                strcat(rivi,"\375GTc");

          /*    strcat(rivi,p+1); strcat(rivi,"@");   */
                i=indeksich(p,x); if (i<0) return(-1);
                strcat(rivi,x); strcat(rivi,"@");

                return(1);
                }

            strcat(rivi,"\375GC"); *(p+1)=EOS; strcat(rivi,p);
            return(1);
            }
        if (strncmp(s,"del stack ",10)==0)
            {
            p=s+10; while (*p==' ') ++p;
     /*     if (*p=='W') ++p;
            n=atoi(p);
     */     n=indeksich(p,x);
            if (n<=0)
                {
                printf("\nError on line %d: Illegal tutindex in del stack!",eol-1);
                WAIT; return(-1);
                }
            strcat(rivi,"\375GTZ"); strcat(rivi,x); strcat(rivi,"@");
            return(1);
            }
        if (strncmp(s,"jump ",5)==0)
            {
            p=s+5; while (*p==' ') ++p;
            strcat(rivi,"\375GTg");
            k=split(p,osa,4);
            if (k<3)
                {
                sprintf(sbuf,"\nError on line %d: Less than 3 parameters in jump!",eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            for (i=0; i<k; ++i)
                {
                if (k==4 && i==2) strcat(rivi,"C!@");
                p=osa[i]; strcpy(x,p);
                if (*p=='W')
                    {
                    n=indeksich(p,x); if (n<0) return(-1);
                    }
                else strcat(rivi,"C");
                strcat(rivi,x); strcat(rivi,"@");
                }
            return(1);
            }
        if (*s=='W' && strchr(s,'=')!=NULL)
            {
            return(tutor_op_arit(rivi,s));
            }
        if (strncmp(s,"keys ",5)==0)
            {
            p=s+5; while (*p==' ') ++p;
            strcat(rivi,"\375GTD"); strcat(rivi,p); strcat(rivi,"@");
            return(1);
            }
        if (strncmp(s,"call ",5)==0)
            {
            strcat(rivi,"\375G;"); p=s+5; while (*p==' ') ++p;

            strcpy(x,p);
            if (*p=='W')
                {
                n=indeksich2(p,x);
                if (n<0) strcpy(x,p);
                else strcat(rivi,"W");
                }

            strcat(rivi,x); strcat(rivi,"\375R");
            return(1);
            }
        if (strncmp(s,"calls ",6)==0)
            {
            strcat(rivi,"\375G!"); p=s+6; while (*p==' ') ++p;
            strcpy(x,p); // 20.11.2001
            if (*p=='W')
                {
                n=indeksich2(p,x);
                if (n<0) strcpy(x,p);
                else strcat(rivi,"W");
                }

            strcat(rivi,x); strcat(rivi,"\375R");
            return(1);
            }
        if (strncmp(s,"error handler ",14)==0)
            {
            p=s+14; while (*p==' ') ++p;
            if (*p=='G') *p='g';  /* jotta tut_continue toimisi */
            strcat(rivi,"\375GTE"); strcat(rivi,p); strcat(rivi,"@");
            return(1);
            }
        if (strncmp(s,"message shadow",14)==0)
            {
            p=s+14; while (*p==' ') ++p;
            if (*p==EOS) *sana=' '; else *sana=*p;
            sprintf(x,"\375Gu%c",*sana); strcat(rivi,x);
            return(1);
            }
        if (strncmp(s,"message ",8)==0)
            {
            p=s+8; while (*p==' ') ++p;
            strcat(rivi,"\375GTq"); strcpy(x,p);
            if (*p=='W')
                {
                n=indeksich(p,x); if (n<0) return(-1);
                }
            else strcat(rivi,"C");
            strcat(rivi,x); strcat(rivi,"@");
            return(1);
            }
        if (strncmp(s,"ref ",4)==0)    /* {ref set i} {ref jump i}  */
            {
            p=s+4; while (*p==' ') ++p;
            i=0;
            if (strncmp(p,"set ",4)==0) { i=1; p+=4; }
            if (strncmp(p,"jump ",5)==0) { i=2; p+=5; }
            if (strncmp(p,"del ",4)==0) { i=3; p+=4; }
            if (i)
                {
                switch (i)
                    {
                  case 1: strcat(rivi,"\375GLR"); break;
                  case 2: strcat(rivi,"\375GLr"); break;
                  case 3: strcat(rivi,"\375GLD"); break;
                    }
                strcat(rivi,p);
                return(1);
                }
            }

        if (strncmp(s,"soft ",5)==0)    /* {soft Wi=Vj} etc. */
            {
            p=s+5; while (*p==' ') ++p;
// printf("\np=%s|",p); getch();
            q=strchr(p,'=');
            if (q==NULL)
                {
                sprintf(sbuf,"\nError on line %d: = missing in {soft ...}",
                                       eol-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            *q=EOS; ++q;

            if (*p=='V')
                { strcpy(x,p+1); strcpy(y,"V"); }
            else // *p=='W'
                {
                n=indeksich(p,x); if (n<0) return(-1);
                *y=EOS;
                }
            sprintf(sbuf,"\375GTv%s%s@",y,x); strcat(rivi,sbuf);

            if (*q=='V')
                { strcpy(x,q+1); strcpy(y,"V"); }
            else // *q=='W'
                {
                n=indeksich(q,x); if (n<0) return(-1);
                *y=EOS;
                }
            sprintf(sbuf,"%s%s@",y,x); strcat(rivi,sbuf);

            return(1);
            }

        if (strncmp(s,"play sound",10)==0)
                {
                p=s+10; while (*p==' ') ++p;
                if (*p==EOS) return(-1);
                strcat(rivi,"\375GT");
                if (*p=='W')
                    {
                    strcat(rivi,"T");
                    n=indeksich(p,x); if (n<0) return(-1);
                    strcat(rivi,x);
                    }
                else { strcat(rivi,"S"); strcat(rivi,p); }
                strcat(rivi,"@");
                return(1);
                }

        sprintf(sbuf,"\nError on line %d: Unknown code word {%s}",eol-1,s);
        sur_print(sbuf); WAIT;
        return(-1);
        }


static int tulkitse_rivi(char *rivi)
        {
        int i;
        char x[LLENGTH];
        char *p,*q;

        strcpy(rivi,"*");  /* kontr.merkki */
        edread(x,eol); ++eol;
        i=strlen(x); while (x[i-1]==' ') x[--i]=EOS;

        if (*x=='/')
            {
            i=tutki_wnimet(x+1);
            if (i<0) return(-1);
            ++n_comments; strcat(rivi,comment_code);
            return(1);
            }
        if (*x=='%') return(1);
        if (*x=='-' || *x=='+')
            {
            i=spec_code(rivi,x);
            if (i<0) return(-1);
            if (i==1) return(1);
            /* i=2 label-rivi jatkaa  */
            }
        p=x+1;
        while (1)
            {
            q=strchr(p,'{');
            if (q==NULL) { strcat(rivi,p); break; }
            *q=EOS; ++q;
            strcat(rivi,p);
            p=strchr(q,'}');
            if (p==NULL)
                {
                sprintf(sbuf,"\nError on line %d: } missing in %s",eol,q);
                sur_print(sbuf); WAIT; return(-1);
                }
            *p=EOS;
            i=muunna(rivi,q);
            if (i<0) return(-1);
            ++p;

            }
        return(1);
        }




static int op_tutload()
        {
        int i,k, kesken, m;
        char x[LLENGTH];
        char sana[LLENGTH];
        char *p;
        char nimi[LLENGTH];
        char *name; // RS ADD
// RS ID        extern long survoid();

        if (g<2) { PR_EBLD;
                   sur_print("\nCorrect form: TUTLOAD <sucro file>");
                   WAIT; return(-1);
                 }

        koodi=1;
        *txt_file_name=EOS;

        if (g>2)
            {
            if (!isdigit(*word[2]))
                {
                strcpy(txt_file_name,word[2]);
                if (!muste_is_path(word[2])) // RS CHA p=strchr(word[2],':'); if (p==NULL && *word[2]!='/' && *word[2]!='\\' && *word[2]!='.' && *word[2]!='~') // RS ADD FIXME path
                	{ 
                	strcpy(txt_file_name,edisk);
                    strcat(txt_file_name,word[2]);
                    }
                txt_file=muste_fopen(txt_file_name,"wt");
                if (txt_file==NULL)
                    {
                    sprintf(sbuf,"\nCannot open file %s !",txt_file_name);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                }
// RS REM            else if (atol(ser_number)+(long)r2==atol(word[2])) koodi=0;
            }

        alusta_nimet();
        *etusukro=EOS;
        strcpy(nimi,word[1]);
        muste_removequotes(nimi); // RS ADD FIXME Turha???
        name=nimi; // RS ADD
        p=strchr(nimi,'-');

/* RS Add minus paths */ 
            p=name+strlen(name)-1;
            
            while (p>=name)
                {
                if (*p=='-') break;
                if (strchr("\\:/",*p)!=NULL) { p=NULL; break; } // RS FIXME path "/" lisätty
                --p;
                }
                
            if (p<=name) p=NULL;
/* RS Add minus paths end */  
         
        if (p!=NULL)
            {
            *p=EOS;
            strcpy(etusukro,p+1);
            k=tee_sucrodir(nimi); if (k<0) { not_found(nimi); return(-1); }

// for (i=0; i<nosat; ++i) Rprintf("\n%s %ld",osasukro[i],osaosoite[i]); // RS DEBUG

            for (i=0; i<nosat; ++i)
                {
                if (muste_strcmpi(etusukro,osasukro[i])==0) break;
                }
            if (i==nosat)
                {
                sprintf(sbuf,"\nSucro %s not found in sucro file %s!",
                                      etusukro,nimi);
                sur_print(sbuf); WAIT; return(-1);
                }
            muste_fseek(tutor,(long)osaosoite[i],SEEK_SET);
            }
        else
            { k=tutopen(word[1],"rb");
                 if (k<0) { not_found(word[1]); return(-1); } }
        eol=r1+r;
        loppumerkit[0]=(unsigned char)TUTCODE;
        loppumerkit[1]=(unsigned char)TUTCODE;
        loppumerkit[2]=EOS;
        labelcode[0]=(unsigned char)TUTCODE;
        labelcode[1]='G';
        labelcode[2]='T';
        labelcode[3]='X';
        labelcode[4]=EOS;

        tutcoding(1);
        if (eol>r2) { tilavirhe2(); return(-1); }

        init_rivi(rivi);
        *crivi=EOS;
        kesken=1;
        while (kesken)
            {
            if (feof(tutor))
                {
                sur_print("\nEnd code missing!");
                muste_fclose(tutor);
                WAIT; return(-1);
                }
            m=getc(tutor);
            if (m==TUT_COMMENT_CODE)
                {
                if (kommentti_pos==0L)
                    {
                    i=etsi_koodin_loppu(); if (i<0) return(-1);
                    }
                lue_kommentti(x);
                if (koodi)
                    while (*crivi)
                     { k=selkomuunto(rivi,crivi); if (k<0) return(-1); }
                kirjoita(rivi);
                tutki_wnimet(x);   /* 24.3.91 aikaisemmin ennen selkomuuntoa */
                *rivi='/'; strcpy(rivi+1,x);
                kirjoita(rivi);
                init_rivi(rivi);
                continue;
                }
            if (m==255)
                kesken=0;
            if (tutpref[m]=='1')
                {
                sana[0]=(unsigned char)TUTCODE; sana[1]=tutcode[m]; sana[2]=EOS;
                }
            else
                {
                sana[0]=(char)m; sana[1]=EOS;
                }

// RS FIXME check if koodi=0 is needed as it was commented out in the beginning
            if (koodi==0)
                {
                k=rcat(sana); if (k<0) return(1);
                if (m==CODE_RETURN)
                 { k=kirjoita(rivi); if (k<0) return(-1); init_rivi(rivi); }
                p=strstr(rivi+2,labelcode); /* +2 olennainen */
                if (p!=NULL)
                    {
                    strcpy(x,p); *p=EOS;
                    k=kirjoita(rivi); if (k<0) return(-1);
                    init_rivi(rivi);
                    strcat(rivi,x);
                    }
                continue;
                }

            /* koodi=1 */
            strcat(crivi,sana);
            i=strlen(crivi);  /* 5.3.1994 */

// Rprintf("\ncrivi_length: %d",i);            

            if (i>24*200)   /* 200 -12.2.90  400 -5.8.92 600 -6.3.94 */
                {
                if (!com_warning)
                    {
          sur_print("\nWarning: Add more comment lines!  Press ENTER!");
                    sur_getch(); com_warning=1;
                    }
                k=selkomuunto(rivi,crivi); if (k<0) return(-1);
                }
            }
        if (koodi==1) while (*crivi)
            { k=selkomuunto(rivi,crivi); if (k<0) return(-1); }
        kirjoita(rivi);
        for (i=0; i<n_end_comments; ++i)
            {
            lue_kommentti(x);
            *rivi='/'; strcpy(rivi+1,x);
            kirjoita(rivi);
            }
        muste_fclose(tutor); o1=0;
/*
for (i=0; i<10; ++i) printf("\nW%d=%s",i+1,wnimi[i]); getch();
*/
		if (txt_file!=NULL) muste_fclose(txt_file); // RS ADD

        return(1);
        }

static int op_tutsave()
        {
        int i,k,kesken,m,pos;
// RS REM        char x[LLENGTH];
// RS REM        char sana[LLENGTH];
        int ens_rivi,viim_rivi;
        char nimi[LLENGTH];
        char *p;
        char *name; // RS ADD

        if (g<2) { PR_EBLD;
                   sur_print("\nCorrect form: TUTSAVE <sucro file>");
                   WAIT; return(-1);
                 }

        i=etsi_end_koodi();
        if (i<0)
            {
            sur_print("\n{end} missing in sucro code! Cannot save!");
            WAIT; return(-1);
            }
        alusta_nimet();
        *etusukro=EOS; uusi_tiedosto=0; uusi_sukro=0;
        strcpy(nimi,word[1]);
        muste_removequotes(nimi); // RS ADD FIXME turha?

        name=nimi; // RS ADD
        p=strchr(nimi,'-');

/* RS Add minus paths */ 
            p=name+strlen(name)-1;
            
            while (p>=name)
                {
                if (*p=='-') break;
                if (strchr("\\:/",*p)!=NULL) { p=NULL; break; } // RS FIXME path "/" lisätty
                --p;
                }
                
            if (p<=name) p=NULL;
/* RS Add minus paths end */         
        
        
        if (p!=NULL)
            {
            *p=EOS;
            strcpy(etusukro,p+1);
            k=tee_sucrodir(nimi);
            if (k<0)
                {
                nosat=0;
                uusi_tiedosto=1; uusi_sukro=1;
                }
/*   for (i=0; i<nosat; ++i) printf("\n%s %ld",osasukro[i],osaosoite[i]);
     getch();
*/
            for (is=0; is<nosat; ++is)
                {
                if (muste_strcmpi(etusukro,osasukro[is])==0) break;
                }
            if (is==nosat)
                {
                ++nosat; uusi_sukro=1;
                strcpy(osasukro[is],etusukro); osasukro[is][8]=EOS;
                }
/*  printf("\nis=%d nosat=%d",is,nosat); getch();
*/
            if (tutor!=NULL) { muste_fclose(tutor); o1=0; }
            i=kopioi_alku(); if (i<0) return(-1);
            }
        else
            { k=tutopen(word[1],"wb"); if (k<0) return(-1); }
        ens_rivi=eol=r1+r;
        if (eol>r2) { tilavirhe2(); return(-1); }

        koodi=1;
        if (g>2) koodi=atoi(word[2]);
        init_words();
        init_labels();
        tutcoding(2);
        k=lue(); if (k<0) return(-1);
        pos=1;
        kesken=1;
        while (kesken)
            {
            m=(unsigned char)rivi[pos++];
// RS DEBUG Rprintf("char: %d\n",m);
            if (m==EOS) { k=lue(); if (k<0) return(-1); pos=1; continue; }
            if (m==TUTCODE)
                {
                m=(unsigned char)rivi[pos++];
// RS DEBUG Rprintf("tutcode-char: %d\n",m);
                m=(unsigned char)tutcode[m];
// RS DEBUG Rprintf("tutcode[m]: %d\n",m);
                if (m==255) kesken=0;
                if (m==TUTCODE || m==EOS)
                    {
                    k=lue(); if (k<0) return(-1);
                    pos=1;
                    continue;
                    }
                }
            putc(m,tutor);
            }
        viim_rivi=eol;
        talleta_kommentit(ens_rivi,viim_rivi);
        if (*etusukro) kopioi_loppu();
        else { muste_fclose(tutor); o1=0; }
        check_gotos();  /* vain ilmoitukset */
        if (*etusukro) lopeta_talletus();
/*
for (i=0; i<10; ++i) printf("\nW%d=%s",i+1,wnimi[i]); getch();
*/
        return(1);
        }

static int op_tutinfo()
        {
        char x[LLENGTH], *osa[2];
        int i;

        if (g>1)
            {
            edread(x,r1+r-1);
            split(x+1,osa,2);
            edread(x,r1+r-1);
            i=osa[1]-x;
            strcpy(tut_info,x+i);
            i=strlen(tut_info); while (tut_info[i-1]==' ') tut_info[--i]=EOS;
            if (tut_info[i-1]=='@') return(1);
            tut_info[i]='@'; tut_info[i+1]=EOS;
            return(1);
            }
        if (r1+r-1>=r2) return(1);
        edwrite(space,r1+r,1);
        edwrite(tut_info,r1+r,1);
        return(1);
        }

/********************************* TUTDEL ************************************/

static int op_tutdel()
        {
        int i,k;
        char nimi[LLENGTH];
// RS REM        char x[LLENGTH];
        char *p;
        char *name; // RS ADD
        long muutos;
        long v11,v12,v2;
        long l;

        p=NULL; 
        if (g>1) 
          { 
          strcpy(nimi,word[1]);
          muste_removequotes(nimi);

        name=nimi; // RS ADD
        p=strchr(nimi,'-');

/* RS Add minus paths */ 
            p=name+strlen(name)-1;
            
            while (p>=name)
                {
                if (*p=='-') break;
                if (strchr("\\:/",*p)!=NULL) { p=NULL; break; } // RS FIXME path "/" lisätty
                --p;
                }
                
            if (p<=name) p=NULL;
/* RS Add minus paths end */          
          
          }
        if (g<2 || p==NULL) {
                   sur_print("\nCorrect form: TUTDEL <sucro file>-<sucro name>");
                   WAIT; return(-1);
                            }

        *etusukro=EOS;

        *p=EOS;
        strcpy(etusukro,p+1);
        k=tee_sucrodir(nimi);
        if (k<0)
            {
            not_found(nimi); return(-1);
            }

        for (is=0; is<nosat; ++is)
            {
            if (muste_strcmpi(etusukro,osasukro[is])==0) break;
            }
        if (is==nosat)
            {
            sprintf(sbuf,"\nSucro %s not in sucro file %s!",etusukro,nimi);
            sur_print(sbuf); WAIT; return(-1);
            }

        muste_fclose(tutor);
        o1=0;
        if (nosat==1)
            {
            sur_delete1(tiednimi);
//          remove(tiednimi);
/*          sprintf(x,"DEL %s",tiednimi); system(x);  */
            return(1);
            }

        if (is==0) { v11=-1L; v12=-1L; }  // RS v12 init
        else { v11=osaosoite[0]; v12=osaosoite[is]; }
        if (is==nosat-1) v2=-1L;
        else v2=osaosoite[is+1];

        if (is<nosat-1)
            {
            muutos=osaosoite[is+1]-osaosoite[is];
            for (i=is+1; i<nosat; ++i)
                {
                strcpy(osasukro[i-1],osasukro[i]);
                osaosoite[i-1]=osaosoite[i]-muutos;
                }
            }
        --nosat;
        for (i=0; i<nosat; ++i) osaosoite[i]-=JAKSO2;

        strcpy(apunimi,etmpd); strcat(apunimi,"SURVO.XXX");
        tutor=muste_fopen(apunimi,"wb");
        if (tutor==NULL)
            {
            sprintf(sbuf,"\nCannot open temporary file %s!",apunimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        o1=1;
        tutor2=muste_fopen(tiednimi,"rb");
        if (tutor2==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s!",tiednimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        o2=1;
        talleta_dir();

        if (v11!=-1L)
            {
            muste_fseek(tutor2,(long)v11,SEEK_SET);
            for (l=v11; l<v12; ++l)
                {
                fputc(fgetc(tutor2),tutor);
                }
            }

        if (v2!=-1L)
            {
            muste_fseek(tutor2,(long)v2,SEEK_SET);
            while (1)
                {
                i=fgetc(tutor2);
                if (feof(tutor2)) break;
                fputc(i,tutor);
                }
            }

        lopeta_talletus();
        return(1);
        }


/* RS CHA
main(argc,argv)
int argc; char *argv[];
*/
int muste_tutor(char *argv)
        {
        int k;
// RS REM        int row,col;

// RS Initialization of local globals
        tutor=NULL;
        tutor2=NULL;
        eol=0;
        koodi=1;
        maxcrivi=24*LLENGTH-1;
        t_virhe=0;
        n_comments=0;
        n_end_comments=0;
        koodi_pos=0;
        kommentti_pos=0;
        tut_index=0;
        max_indeksi=0;
        nosat=0;
        is=0;
        uusi_tiedosto=0;
        uusi_sukro=0;
        uusi_koodialku=0;
        uusi_paikka1=0;
        uusi_paikka2=0;
        com_warning=0;
        txt_file=NULL;
        o1=0;
        o2=0;
        crivin_alku=0;
        cursor_line=0;
        corner_line=0;
        max_shadows=0;
        dim_index=0;
        win_lines=0;
        nlab=0;
        ngo=0;
        no_labcheck=0;
        nword=NWORD;

// RS REM        if (argc==1) return(1);
        s_init(argv); // RS CHA argv[1]
        
// RS REM        get_ser_number(argv[3]);
        if (muste_strcmpi(word[0],"TUTLOAD")==0)
            {
            k=op_tutload(); /* if (k>0) edsave2(argv[1]); */
            s_end(argv); // RS CHA argv[1]
            return(1);
            }
        if (muste_strcmpi(word[0],"TUTSAVE")==0)
            {
            k=op_tutsave();
            return(1);
            }
        if (muste_strcmpi(word[0],"TUTINFO")==0 || muste_strcmpi(word[0],"TUTSTACK")==0)
            {
            k=op_tutinfo();
            s_end(argv); // RS CHA argv[1]
            return(1);
            }
        if (muste_strcmpi(word[0],"TUTDEL")==0)
            {
            k=op_tutdel();
            return(1);
            }
        return(1);
        }
