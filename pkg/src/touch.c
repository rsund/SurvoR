/*  t.c 15.8.1985/SM (24.8.1987) (1.9.1994) (27.5.1995) (26.9.1998)
 */
 
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <ctype.h>
#include <time.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h" 

/* touch.h 21.8.1985/SM (25.8.1985)
*/

#define T_STOP '.'
#define T_DISP 'C'

#define NCHAIN 2000
#define NMEMORY 10
#define LMEMORY 32

#define K_RIGHT 'r'
#define K_LEFT  'l'
#define K_UP    'u'
#define K_DOWN  'd'
#define K_DEF   'D'
#define K_RUN   '!'
#define K_STOP  'e'
#define WMAX 4096

 
static char stripe[LLENGTH];
static int special;
// RS REM static int dispm=0;
static char pref=' ';
// RS REM static int lupa=1;
static int rikottu=0;
static int worm=0;
static int collect=0;


static long wait_hetki;
static FILE *tutor;

static char chain[NCHAIN];
static int nch;
static int s; /* 0=yksittÑin, 1=ketjun mÑÑrittely, 2=ajo */
static int t; /* opnd lkm */
static int v; /* 1=+ 2=- 3=* 4=/ */
static double opnd[3];
static int op[3];
static char tsana[LLENGTH];
static char trivi[LLENGTH];
static char wsana[LLENGTH];
static char oform[32]; // RS REM , tconst[32];
static unsigned int ar,ac,P_ind,pr,pc,R_ind,R_nch;
static int tdisp;
static unsigned int fr,fc;
static unsigned int F_ind;
static unsigned int C_ind;
static unsigned int M_ind,K_ind,f_ind;
static char memory[NMEMORY][LMEMORY];
static double dmemory[NMEMORY];
static char tchain[LNAME];
static int last_right, last_down;
static int key;

static int prompts=1; // 2.11.2005
static FILE *touchdata; /* 19.8.1994 */
static int tch_open=0;
static int take_also_plus=0; // 17.10.2001

static char tch_data[LLENGTH];
static int del_permitted;
static FILE *collect_file;
static int col_open=0;

static int nmax;
static int wn;
static int wf;
static int *wr,*wc;
static char *wch,*wsh;
static int wrr,wcc,wprevr,wprevc;
static int wpr,wpc;
static int worm_shad=0;
static int erase_by_worm=0;
static int erase_temporarily=0;


static char mode[3][5]={{"TOUCH"},{"DEF  "},{"RUN  "},};
static long wait_hetki;
// RS REM extern int wait_save;
static time_t time1,time2;

#define TUT_COMMENT_CODE 252
static int tut_special_code=0;



static int touch_res();
static int tutch_touch();


static void tutname(char file[],char name[])
        {
        *file=EOS;
// RS ADD Unixpath FIXME        
        if ((strchr(name,':')==NULL) && (name[0]!='/') && (name[0]!='~') ) strcat(file,edisk);
        strcat(file,name);
        if (strchr(name,'.')==NULL) strcat(file,".TUT");
        }
        
static int tutopen(char name[],char mode[])
        {
        tutname(etufile,name);
        tutor=fopen(etufile,mode);
        if (tutor==NULL) return(0);
        return(1);
        }

static void read_tutword(char s[])
        {
        int i=0,m;

        while ((m=getc(tutor))!='@') s[i++]=m; s[i]=EOS;
        }

static void tutsave(int m)
        {
        putc(m,tutor);
        }

static int read_cond(char *s)
        {
        char sana[LLENGTH];
        read_tutword(sana);
        if (*sana=='C') { strcpy(s,sana+1); return(1); }
//      read_from_stack(sana,s);
        return(1);
        }



static int headline_touch()
        {
        char aika[26];
        char dispm2;
        int k;

        pvmaika(aika);
        sur_locate(1,1);
        if (ntut==0) { PR_EUDL; } else { PR_EIN2; }  /* 28.11.92 */
        dispm2='0'+ntut; if (ntut==0) dispm2=' ';
        sprintf(sbuf,"%c%c%4u%3u SURVO TOUCH MODE   ",pref,dispm2,c1+c-1,c1); sur_print(sbuf);
        k=c3-72;
        sprintf(sbuf,"%s     %.5s   %*.*s%7d%5d ",aika,mode[s],k,k,space,r2,c2); sur_print(sbuf);
//      sprintf(sbuf,"%s     %.5s   %7d%5d ",aika,mode[s],r2,c2); sur_print(sbuf);
        PR_EINV; sprintf(sbuf,"%c",key); sur_print(sbuf);
        PR_ENRM;
        return(1);
        }

static int Wdisp()
        {
        CURSOR_OFF; headline_touch(); CURSOR_ON; cursor(r,c); return(1);
        }

static int disp_touch()
        {
        unsigned int i,lev;

        CLS;
        headline_touch();
        lev=c3; if (c2-c1+1<c3) lev=c2-c1+1;
        for (i=0; i<r3; ++i) displine(r1+i,lev);
        touch_res("");
        cursor(r,c);
        return(1);
        }

static int nextkey_touch()
        {
        int m,aika; // ,no_key;

        aika=0;

        time(&time1);
// RS REM        sur_flush_input();
        while (1)
            {
            if (sur_kbhit()) break;
            time(&time2);
            if (difftime(time2,time1)>0.5)
                {
                headline_touch(); cursor(r,c);
                time1=time2;
                }
            sur_sleep(10);

//          ++aika;
//          CURSOR_ON;
//          if (aika>180) { CURSOR_OFF; headline_touch(); cursor(r,c); aika=0;
//                          CURSOR_ON;
//                        }
            }

        special=0;
        m=s_getch();
// printf("m=%d|",m); getck();
        switch (m)
            {
          case EXTEND_CH: m=sur_getch_ext();
                          special=1;
                  switch (m)
                      {
/*                      
                    case  KEY_EXIT:      m=CODE_EXIT; break;
                    case  KEY_RIGHT:     m=CODE_RIGHT; break;
                    case  KEY_LEFT:      m=CODE_LEFT; break;
                    case  KEY_UP:        m=CODE_UP; break;
                    case  KEY_DOWN:      m=CODE_DOWN; break;
                    case  KEY_HOME:      m=CODE_HOME; break;
                    case  KEY_INSERT:
                    case  KEY_INSERT2:   m=CODE_INSERT; break;
                    case  KEY_INSERTL:   m=CODE_INSERTL; break;
                    case  KEY_DELETE:
                    case  KEY_DELETE2:   m=CODE_DELETE; break;
                    case  KEY_DELETEL:   m=CODE_DELETEL; break;
                    case  KEY_ERASE:     m=CODE_ERASE; break;
                    case  KEY_NEXT:      m=CODE_NEXT; break;
                    case  KEY_PREV:      m=CODE_PREV; break;
                    case  KEY_EXEC2:
                    case  KEY_EXEC3:     m=CODE_EXEC; break;
                    case  KEY_DISP:      m=CODE_DISP; break;
                    case  KEY_PRE:       m=CODE_PRE; break;
                    case  KEY_TOUCH:     m=CODE_TOUCH; break;
                    case  KEY_DISK:      m=CODE_DISK; break;
                    case  KEY_HELP:      m=CODE_HELP; break;
                    case  KEY_END:       m=CODE_END; break;
                    case  KEY_WORDS:     m=CODE_WORDS; break;
*/



case CODE_EXIT:
case CODE_RIGHT:
case CODE_LEFT:
case CODE_UP:
case CODE_DOWN:
case CODE_HOME:
case CODE_INSERT:
case CODE_INSERTL:
case CODE_DELETE:
case CODE_DELETEL:
case CODE_ERASE:
case CODE_NEXT:
case CODE_PREV:
case CODE_EXEC:
case CODE_DISP:
case CODE_PRE:
case CODE_TOUCH:
case CODE_DISK:
case CODE_HELP:
case CODE_END: 
case CODE_WORDS: 
case CODE_RETURN:  break;
case CODE_BACKSP: m=CODE_LEFT; special=1; break;


                    default: m=-1; // RS CHA FIXME ????
//  RS REM                  sprintf(sbuf,"\nSPECIAL: %d\n",m); sur_print(sbuf); sur_getch();
//  RS REM FIXME?                           m=32; disp_touch();
                      }
                      break;

          case CODE_RETURN: special=1; m=CODE_RETURN; break;
          case CODE_BACKSP: special=1; m=CODE_LEFT; break;
          case CODE_EXEC: special=1; m=CODE_EXEC; break;

          default: ; /* mahd.dekoodaus */
            }
  /*    printf("%d ",m);   */

        return(m);
        }

#if 0
static int lue_hetki(long *ptime)
        {
        static struct timeb tb;

        ftime(&tb);
        *ptime=1000L*tb.time+tb.millitm;
        return(1);
        }


static int save_wait(int m)
        {
        long aika,a0,a1,a2;
        int i;
        static pre_ind=0;

/*      if (pre_ind && m==(int)'T') { pre_ind=0; return(1); }
        if (m==CODE_PRE) pre_ind=1; else pre_ind=0;               */
        if (pre_ind) { pre_ind=0; return(1); }    /*  27.4.1996 as s2.c 1.6.1995 */
        if (m==CODE_PRE) pre_ind=1; else pre_ind=0;

        lue_hetki(&aika);
        a0=aika-wait_hetki; wait_hetki=aika;
        a1=a0>>7; a2=a1>>2; /* "jako 100:lla" 1/128+1/512=0.0097.. */
        a0=a1+a2; if (a0==0L) return(1);
        sprintf(sbuf,"TW%ld@",a0);
        tutsave(1);
        for (i=0; i<strlen(sbuf); ++i) tutsave((int)sbuf[i]);
        return(1);
        }
#endif

static void stop_res();
static int nextch_touch()
        {
        int m;

        if (s==2)
            {
            if (sur_kbhit())
                {
                m=s_getch();
                switch (m)
                    {
                  case T_STOP: stop_res(); special=1; m=0; return(0);
                  case T_DISP: tdisp=1-tdisp; break;
                    }
                }
            if (chain[nch]=='1') special=1; else special=0;
            nch+=2;
            m=(signed char)chain[nch-1];
            if (m<0) m=256+m;
            return(m);
            }

        if (etu==2)
            {
            m=tutch_touch();           
            while (m==255 || m==-1) m=tutch_touch();
            if (m!=0) return(m);
            }
        if (etu==1)
            {
            m=nextkey_touch(); if (wait_save) save_wait(m); tutsave(m); return(m);
            }
         m=nextkey_touch(); ;         
        return (m);
        }


static void press_key(int m)
        {
        int ch,spec;
        int k;

        spec=special;
        if (etu1==1) { Wdisp(); return; }
        k=sur_wait(1000L*tut_wait_c,Wdisp,1);

//      if (k) ch=nextkey(); else ch=m;  piti painaa kahdesti!!??
        ch=m; // RS REM sur_flush_input();
        while (ch!=m && ch!=CODE_LEFT)
            {
            if (ch==CODE_HELP) { etu2=0; ch=m; break; }
            BEEP;
            k=sur_wait(1000L*tut_wait_c,Wdisp,1);
            if (k) ch=nextkey_touch(); else ch=m;
            }
        special=spec;
        }

static void tut_special_touch()
        {
//        int Wdisp();
        int m,ar,ac,ch,i;
        long l;

        char sana[LLENGTH];

        switch(etu)
            {
          case 0:   break;

          case 1:
            cursor(r3+1,1); PR_EBLD;
            sur_print("Tutor functions: E=End of definition, C=control codes ? ");
            ch=sur_getch();

            if (ch=='E')
                {
                tutsave(255);
                muste_fclose(tutor); etu=0;
                }

            else if (ch=='C')
                {
                cursor(r3+1,1); ERASE;
                prompt("Control word ? ",sana,64); // RS CHA FIXME? sprompt -> prompt
                if (*sana=='X') { *sana=(char)TUT_EFFECTS_OFF;
                                  sana[1]=EOS; muste_fseek(tutor,-2L,1); }
                for (i=0; i<strlen(sana); ++i) tutsave((int)sana[i]);
                }

            cursor(r3+1,1); ERASE; PR_ENRM;
            break;


          case 2:
            m=tutch_touch();
            switch (m)
                {
              case 'W':
                read_tutword(sana);
                if (etu1>1 && etu2!=2)
                    sur_wait((long)(5*tut_wait_c)*atoi(sana)*etu1,Wdisp,1);
                else Wdisp();
                break;
              case 'L':
                read_tutword(sana);
                muste_fclose(tutor);
                tutopen(sana,"rb");
                break;
              case 's':
                read_tutword(sana);
                if (etu1>1) etu1=atoi(sana);
                break;
              case 'D':
                read_tutword(sana);
                etu2=atoi(sana);
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
             case 't':   // ehdoton nopeuden muutos 8.4.2005
               read_cond(sana);
               etu1=atoi(sana);
               break;
              case 255:
                break;
              case 'M':  /* ÑÑnet puuttuvat */
                read_tutword(sana); break;

              default: sprintf(sbuf,"\nTUT CODE: %d",m); sur_print(sbuf); sur_getch();
                }
            }
        tut_special_code=0;
        }


static int tutch_touch()
        {
//        int Wdisp();
//        int nop();
        int m,ch;
        char nimi[16];

        m=getc(tutor);
        while (m==TUT_COMMENT_CODE) m=getc(tutor);
        if (m==CODE_PRE)
            {
            ch=getc(tutor); ungetc(ch,tutor);
            if (ch=='T') tut_special_code=1;
            }
        if (!feof(tutor))
            {
            if (sur_kbhit()) // RS FIXME ei pitäisi tulla tänne
                {
                ch=sur_getch(); // nextkey_touch();
                switch (ch)
                    {
                  case '+': --etu1; --etu1; if (etu1<=0) etu1=1; break;
                  case '-': ++etu1; ++etu1; if (etu1>40) etu1=40; break;
                  case '.': etu=0; muste_fclose(tutor); return(0);
                  case CODE_HELP:
                            etu2=2; if (etu1<2) etu1=2; break;

                  default: break; // RS CHA FIXME  while(!sur_kbhit()) ; getck(); break;
                    }
                }
            special=0;
            if (etu1>1 && etu2!=2) sur_wait((long)tut_wait_c*etu1,nop,0);
            switch (m)
                {
              case CODE_EXIT:
              case CODE_RIGHT:
              case CODE_LEFT:
              case CODE_UP:
              case CODE_DOWN:
              case CODE_HOME:
              case CODE_INSERT:
              case CODE_INSERTL:
              case CODE_DELETE:
              case CODE_DELETEL:
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
              case CODE_HELP:
              case CODE_WORDS:  /* 19.9.1994 */
                    special=1;
                    break;
              case TUT_EFFECTS_OFF: etu2=0; etu3=0; m=255; break;

              default:
                    special=0;
                    break;
                }
            if (etu2>0)
                {
                if (etu2==2 && tut_special_code) return(m);
                CURSOR_OFF; cursor(r3+1,c3-16);
             if (etu2==1) sur_print("      "); else { PR_EBLD; sur_print("Press "); }
                PR_EINV; label(m,nimi); sprintf(sbuf,"%s",nimi); sur_print(sbuf);
                PR_ENRM; cursor(r,c); CURSOR_ON;
                if (etu2==1) { if (etu1>1) sur_wait((long)tut_wait_c*etu1,Wdisp,0); }
                else press_key(m);
                CURSOR_OFF; cursor(r3+1,c3-16);
            /*  printf("%s","              "); */
                sur_print("              ");
                cursor(r,c); CURSOR_ON;
                }
            return(m);
            }
        muste_fclose(tutor);
        etu=0; return(0);
        }

static int touch_res(char r[])
        {
        static char alarivi[LLENGTH];

        if (*r!=EOS) strcpy(alarivi,r);
   /*   if (s==2 && R_ind!=1) return;     */
        sur_locate(r3+2,1); ERASE;
        if (prompts)
            {
            sur_locate(r3+2,1);
            PR_EINV;
            sprintf(sbuf,"%s",alarivi); sur_print(sbuf); PR_ENRM;
            cursor(r,c);
            }
        return(1);
        }

static void touch_run()
        {
        s=2;
        touch_res("RUN: To interrupt, press '.'   C=display off/on");
        CURSOR_OFF; headline_touch(); CURSOR_ON; cursor(r,c);
        nch=0;
        ar=r; ac=c;
        P_ind=0;
        R_ind=0;
        }

static void seek_line_end()
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

static int clear_res()
        {
        t=0;
// printf("\nprompts=%d s=%d|",prompts,s); getch();
        if (s==2) return(1);
        *trivi=EOS;
        if (prompts)
            touch_res("TOUCH MODE  (To stop, press ENTER)");
        else
            touch_res("");
        return(1);
        }

static void touch_init()
        {
        s=0;
        *oform=EOS;
        P_ind=0;
        R_ind=0;
        F_ind=0;
        C_ind=0;
        M_ind=K_ind=f_ind=0;
        tdisp=1;
        chain[0]='\0';
        last_right=10;
        last_down=1;
        *wsana=EOS;
        clear_res();
        }

static void touch_clear()
        {
        char s[LLENGTH];

        sur_locate(r3+2,1); PR_ENRM;
        strncpy(s,space,c3+7); s[c3+7]=EOS;
        sprintf(sbuf,"%s",s); sur_print(sbuf); cursor(r,c);
        }

static void t_div(int i,int j)
        {
        if (opnd[j]==0.0)
            {
            PR_EINV;
            sur_print("  Division by zero! Result set to 0. Press any key!");
            nextch_touch(); opnd[i]=0.0; disp_touch(); return;
            }
        opnd[i]/=opnd[j];
        }

static void eval2()
                {
                switch (op[1])
                    {
                  case 1: opnd[0]+=opnd[1]; break;
                  case 2: opnd[0]-=opnd[1]; break;
                  case 3: opnd[0]*=opnd[1]; break;
                  case 4: t_div(0,1);       break;
                    }
                }

static void eval()
        {
        switch (t)
            {
          case 0: return;
          case 1: return;
          case 2: eval2(); break;
            }
        t=1;
        }

static void stop_res()
        {
        s=0; r=ar; c=ac;
        if (tdisp==0) { tdisp=1; disp_touch(); }
        if (t==0)
            strcpy(tsana,"0");
        else
            {
            eval();
            fconv(opnd[0],oform,tsana);
            }

        strcpy(trivi,tsana);  /* 22.10.88 */

        touch_res(tsana);
        }


static int touch()
        {
        unsigned int jmin,j,j1,j2,jmax,haku;

        jmin=(r1+r-2)*ed1+1; jmax=jmin+ed1-2;
        j=j1=j2=jmin+c1+c-2;
        haku=1;
        while (haku)
            {
            if ( (z[j1]=='e' || z[j1]=='E') &&  /* 11.7.1994 */
                 (isdigit((int)z[j1+1]) || z[j1+1]=='+' || z[j1-1]=='-') )
              // oli isnumber !!!
                { --j1; continue; }
            if ( (z[j1]=='+' || z[j1]=='-') &&
                 (z[j1-1]=='e' || z[j1-1]=='E') )
                                         /* 27.8.94 */
                { j1-=2; continue; }
            if (isdigit((int)z[j1]))
                {
                if (j1==jmin) { haku=0; break; }
                --j1; continue;
                }
            switch (z[j1])
                {
              case '-': --j1;  /* lisÑtty 9.2.90 */
              case '+':
              case ' ':
              case '=':
              case ',':
              case ';':
              case '(':
              case ')':
              case '/':
              case '*':
                if (z[j1]=='+' && take_also_plus) --j1; // 17.10.2001
                if (s==2 && j==j1)
                    {
                    if (R_ind)
                        {
                        s=R_ind; nch=R_nch; R_ind=0; return(-1);
                        }
                    stop_res();
                    return(-1);
                    }
                ++j1; haku=0; break;
              default: if (j1==jmin) { haku=0; break; }
                       --j1;
                }
            }
        haku=1;
        while (haku)
            {
            if ( (z[j2]=='E' || z[j2]=='e') &&
                 (isdigit((int)z[j2+1]) || z[j2+1]=='+' || z[j2+1]=='-') )
                 { j2+=2; continue; }
            if ( (z[j2]=='+' || z[j2]=='-') &&
                 (z[j2-1]=='E' || z[j2-1]=='e') )
                 { ++j2; continue; }

            if (isdigit((int)z[j2]))
                {
                if (j2==jmax) { haku=0; break; }
                ++j2; continue;
                }
            switch (z[j2])
                {
              case ' ':
              case '=':
              case ',':
              case ';':
              case '(':
              case ')':
              case '+':  /* + - / * lisÑtty 8.2.90 */
              case '-':
              case '/':
              case '*':
                --j2; haku=0; break;
              default: if (j2==jmax) { haku=0; break; }
                       ++j2;
                }
            }

        if (j1>j2) return(-1);
        strncpy(tsana,z+j1,j2-j1+1); tsana[j2-j1+1]=EOS;
        if (r>r3 || r<1 || tdisp==0) return(1);
/*      if (j1-jmin-c1+2<1 || j2-jmin-c1+2>c3) return(1);     */
        if (j2-jmin-c1+2>c3) return(1);
        if ((int)(j1-jmin-c1+2)<1) return(1);   /* miksi (int) tarvitaan??? 24.7.88 */

        write_string(tsana,j2-j1+1,'7',r+1,8+j1-jmin-c1+2);
        return(1);
        }


static void comp(int m)
        {
        char u[LLENGTH];
        int len=strlen(trivi);

        opnd[t]=atof(tsana);
        op[t]=v;
/*      if (v>1 && !lupa) { rikottu=1; return; }   */

     if ( s!=2 || (s==2 && R_ind==1) )
        {
        if (len+strlen(tsana)+3>c3+7) { strcpy(trivi,"..."); len=3; }
        trivi[len]=(char)m; trivi[len+1]=EOS;
        if (*tsana=='-') { strcpy(u,tsana); strcpy(tsana,"(");
                           strcat(tsana,u); strcat(tsana,")"); }
        strcat(trivi,tsana);
        touch_res(trivi);
        }

        switch (t)
            {
          case 0:
            if (v==2) opnd[0]=-opnd[0];
            t=1;
            break;
          case 1:
            if (v>2)
                {
                if (v==3) { opnd[0]*=opnd[1]; break; }
                 /* v==4 */ t_div(0,1);       break;
                }
            t=2;
            break;
          case 2:
            if (v>op[1] && v>2)
                {
                if (v==3) { opnd[1]*=opnd[2]; break; }
                            t_div(1,2);       break;
                }
                switch (op[1])
                    {
                  case 1: opnd[0]+=opnd[1]; break;
                  case 2: opnd[0]-=opnd[1]; break;
                  case 3: opnd[0]*=opnd[1]; break;
                  case 4: t_div(0,1);       break;
                    }
                op[1]=op[2];
                opnd[1]=opnd[2];
                break;
            }
        }
                
                
static void print_res(int m)    /* m on = tai ! tai ; */
        {
        char *p;
        int len, piste;
        unsigned int i,j,j0,j1,j2,jmin,jmax;
        char x[LLENGTH];

        if (*wsana)
            {
            strcpy(tsana,wsana);
            len=strlen(tsana);
            piste=1;
            }
        else
            {
            if (t==0) return;
            fconv(opnd[0],oform,tsana);
            len=strlen(tsana);
            p=strchr(tsana,'.');
            if (p==NULL) piste=len; else piste=p-tsana;
            }

        if (m=='!' || m==';')
            {
            if (m=='!') *tut_info=EOS;
            if (strlen(tut_info)+strlen(tsana)+1>LLENGTH-1)
                {
                PR_EINV;
                sur_print("\nNo space anymore for touch results!");
                WAIT; disp_touch(); return;
                }
            strcat(tut_info,tsana); strcat(tut_info,"@");
            return;
            }
        jmin=(r1+r-2)*ed1+1; jmax=jmin+ed1-2;
        i=0; j=j0=jmin+c1+c-1-piste;
        if (jmin+c1+c-1<=piste) return; /* 29.5.89 liikaa kentÑn alussa */
        if (j<jmin) j=jmin;


        while ( i<len && j<=jmax )
            {
            z[j++]=tsana[i++];

            }
        j2=j;
        j=j0-1;
        while ( j>=jmin && z[j]!=' ' ) { z[j]=' '; --j; } j1=j+1;
        j=j2;
        while ( j<=jmax && z[j]!=' ' ) { z[j]=' '; ++j; } j2=j-1;

        if ((int)(j1-jmin+1)<c1) j1=jmin+c1-1;          /* (int) 24.7.88 */
        if ((int)(j2-jmin+1)>c1+c3-1) j2=jmin-1+c1+c3-1;

     if (r<=r3 && r>0 && c<=c3 && c>0 && tdisp==1 && j1<=j2)
        {
        if (ibm)
            write_string(z+j1,j2-j1+1,'1',r+1,8+j1-jmin-c1+2);
        else
            {
            PR_EBLD;
            cursor(r,j1-jmin-c1+2);
            strncpy(x,z+j1,j2-j1+1); x[j2-j1+1]=EOS;
            sprintf(sbuf,"%s",x); sur_print(sbuf);
            cursor(r,c);
            PR_ENRM;
            }
        }

        clear_res();

        }

static void disp_res()
        {
        if (s==2) return;
        fconv(opnd[0],oform,trivi);
        touch_res(trivi);
        }


static int touch_data_close()
        {
        if (!tch_open) return(1);
        muste_fclose(touchdata);
        if (tch_open==1 && del_permitted)
             {
         /*  sprintf(sbuf,"DEL %s",tch_data); system(sbuf); */
             remove(tch_data);
             }
        return(1);
        }

static int touch_data_open(char *tsana)
        {
        if (tch_open)
            {
            touch_data_close();
            tch_open=0;
            touch_res("TOUCH MODE: Text file for touch data closed!");
            return(1);
            }
        strcpy(tch_data,tsana);
        if (strchr(tsana,':')==NULL) { strcpy(tch_data,edisk); strcat(tch_data,tsana); }

        touchdata=muste_fopen(tch_data,"r");
        if (touchdata==NULL) del_permitted=1;
        else { muste_fclose(touchdata); del_permitted=0; }

        touchdata=muste_fopen(tch_data,"at+");
        if (touchdata==NULL)
            {
            sprintf(sbuf,"\nCannot open text file %s for touch data!",tch_data);
            WAIT; disp_touch(); return(-1);
            }
        sprintf(sbuf,"TOUCH MODE: Text file %s for touch data opened!",tch_data);
        touch_res(sbuf);
        tch_open=1;
        return(1);
        }

static int touch_data_save(char *word)
        {
        fprintf(touchdata,"%s\n",word);
        ++tch_open;
        return(1);
        }

static int collect_open(char *mode)
        {
        char x[LLENGTH];

        strcpy(x,etmpd); strcat(x,"TCHLINES.TXT");
        collect_file=fopen(x,mode);
        if (collect_file==NULL)
            {
            sprintf(sbuf,"\nCannot open %s!",x);
            WAIT; return(-1);
            }
        col_open=1;
        return(1);
        }

static int collect_lines()
        {
        char x[LLENGTH];
        int j,col,k;
        char *p;
        static int n_lines=0;

        if (collect==2) return(1);
        j=r1+r-1; col=c1+c-1;
        edread(x,j);
        if (!collect && empty_line(x+1,c2))
            {
            collect=2;
            touch_res("Print collected lines by `='. Exit Touch mode by ENTER");
            return(1);
            }
        else if (collect==0)
            {
            k=collect_open("wt");
            if (k<0) return(-1);
            collect=1;
            col_open=1;
            }
        edread(x,j);
        p=x+col;
        fprintf(collect_file,"%s\n",p);
        k=strlen(p); while (p[k-1]==' ') p[--k]=EOS;
        PR_EINV; sprintf(sbuf,"%.*s",c3-c+1,p); sur_print(sbuf); PR_ENRM;
        if (zs[j]==0) { *x=EOS; strncat(x,space,c2-col); }
        else edread(x,zs[j]);
        fprintf(collect_file,"%s\n",x+col);
        ++n_lines;
        sprintf(sbuf,"Collecting lines: %d   (Print collected lines by `='. Exit by ENTER)",
                      n_lines);
        touch_res(sbuf);
        return(1);
        }

static int collect_results() /* by '=' */
        {
        int i,j,col,k;
        char x[LLENGTH],xs[LLENGTH];

        if (col_open) muste_fclose(collect_file);
        i=collect_open("rt"); if (i<0) return(-1);
        j=r1+r-1; col=c1+c-1;
        while (!feof(collect_file))
            {
            fgets(x,LLENGTH,collect_file);
            k=strlen(x)-1; x[k]=EOS;
            if (feof(collect_file)) break;
            while (x[k-1]==' ') x[--k]=EOS;
            edwrite(x,j,col);
            fgets(x,LLENGTH,collect_file);
            if (zs[j]==0) shadow_create(j);
            edread(xs,zs[j]);
            strncpy(xs+col,x,k);
            edwrite(xs,zs[j],0);
            shadow_test(j);
            ++j;
            }
        disp_touch();
        muste_fclose(collect_file);
        col_open=0;
        if (collect!=2)
            {
            touch_res("Print collected lines by `='. Exit Touch mode by ENTER");
            collect=2;
            }
        return(1);
        }

static int print_words() // 25.11.2009
    {
    char *p;

    while (1)
        {
        p=strchr(trivi+1,'+');
        if (p==NULL) break;
        *p=' ';
        }

    edwrite(trivi+1,r1+r-1,c1+c-1);

    PR_EBLD;
    sur_print(trivi+1);
    cursor(r,c);
    PR_ENRM;

    clear_res();
    return(1);
    }



static void not_enough_space()
        {
        sur_print("\nNot enough space for worm mode!");
        WAIT;
        }
        
static int waraa_tilat()
        {

        if (wr!=NULL) return(1);
        nmax=WMAX;
        wr=(int *)muste_malloc(nmax*sizeof(int));
        if (wr==NULL) { not_enough_space(); return(-1); }
        wc=(int *)muste_malloc(nmax*sizeof(int));
        if (wc==NULL) { not_enough_space(); return(-1); }
        wch=muste_malloc(nmax);
        if (wch==NULL) { not_enough_space(); return(-1); }
        wsh=muste_malloc(nmax);
        if (wsh==NULL) { not_enough_space(); return(-1); }

        return(1);
        }

static int no_prompts()
    {
    prompts=0;
    clear_res();
    return(1);
    }

static int write_worm()
        {
        int i,ii;
        int j,k;
        char sch;

        --wf; if (wf<0) wf=wn-1;
        ii=wf;
        for (i=wn-1; i>=0; --i)
            {
            j=wr[ii];
            *(z+(j-1)*ed1+wc[ii])=wch[i];
            sch=wsh[i];
            if (sch==' ')
                {
                if (zs[j]!=0)
                    {
                    *(z+(zs[j]-1)*ed1+wc[ii])=' ';
                    shadow_test(j);
                    }
                }
            else
                {
                if (zs[j]==0)
                    {
                    k=shadow_create(j);
                    if (k<0) return(-1);
                    }
                *(z+(zs[j]-1)*ed1+wc[ii])=sch;
                }
            --ii; if (ii<0) ii=wn-1;
            }
        disp_touch();
        ++wf; if (wf==wn) wf=0;  /* 21.8.1994 */
        return(1);
        }

static int reverse_worm()
        {
        int i;
        char ch;

        for (i=0; i<wn/2; ++i)
            {
            ch=wch[i]; wch[i]=wch[wn-i-1]; wch[wn-i-1]=ch;
            ch=wsh[i]; wsh[i]=wsh[wn-i-1]; wsh[wn-i-1]=ch;
            }
        return(1);
        }






static int worm_mode(int vaihe,int m) /* int m;  edell. nappi */
        {
        int i,ii;
        int rr,cc;
        char ch,sh;

        if (vaihe==0)
            {
            switch(worm)
                {
              case 0:
                i=waraa_tilat();
                wn=0;
                worm=1;
                wprevr=wprevc=-1;
                if (i>0) touch_res("Define a worm by arrow keys! Stop definition by alt-F2!");
                return(i);
              case 1:
                worm=2;
                wf=0;
           if (prompts)
           touch_res("Move the worm by arrow keys! Print by '='! Cancel by alt-F2! Reverse by R!");
                return(1);
              case 2:
                worm=0;
                disp_touch();
                if (*trivi) touch_res(trivi); else clear_res();
                return(1);
                }
            }

        if (m=='S') { worm_shad=1-worm_shad; return(1); }
        if (m=='C')
            {
            SCROLL_UP(1,r3+1,r3+1);
            touch_clear(); return(1);
            }
        if (m=='D') { display_off=1-display_off; return(1); }
        if (m=='E') { erase_by_worm=1-erase_by_worm; return(1); }
        if (m=='e') { erase_temporarily=1-erase_temporarily; return(1); }

        if (m=='Z') { no_prompts(); return(1); } // 2.11.2005

        switch (worm)
            {
          case 1:
            if (wn>=nmax)
                {
                sprintf(sbuf,"\nToo long worm (more than %d characters)!",
                                nmax);
                PR_EINV; sur_print(sbuf); WAIT; worm=0; return(-1);
                }
            wrr=r1+r-1;
            wcc=c1+c-1;
            if (wrr==wprevr && wcc==wprevc) break;
            wprevr=wrr; wprevc=wcc;
            wr[wn]=wrr; wc[wn]=wcc;
            wch[wn]=*(z+(wrr-1)*ed1+wcc);
            if (zs[wrr]==0) wsh[wn]=' ';
            else wsh[wn]=*(z+(zs[wrr]-1)*ed1+wcc);
            if (!worm_shad) write_string(&wch[wn],1,'8',r+1,c+8);
            ++wn;
            break;
          case 2:
            if (m=='=') { write_worm(); return(1); }
            wrr=r1+r-1;
            wcc=c1+c-1;
            if (m=='R')
                {
                reverse_worm();
                --wf; if (wf<0) wf=wn-1;
                }
            else if (wrr==wprevr && wcc==wprevc) break;
            wprevr=wrr; wprevc=wcc;
            wpr=wr[wf]; wpc=wc[wf];
            wr[wf]=wrr; wc[wf]=wcc;
            ii=wf;
            for (i=wn-1; i>=0; --i)
                {
                rr=wr[ii]-r1+1;  /* testaa rajat */
                cc=wc[ii]-c1+1;
                if (worm_shad) sh=wsh[i]; else sh='8';
                if (rr<r3+1) write_string(&wch[i],1,sh,rr+1,cc+8);
                --ii; if (ii<0) ii=wn-1;
                }
            if (m!='R')
                {
                rr=wpr-r1+1;  /* testaa rajat */
                cc=wpc-c1+1;
                ch=*(z+(wpr-1)*ed1+wpc);
                if (erase_by_worm) { ch=' '; *(z+(wpr-1)*ed1+wpc)=' '; }
                if (erase_temporarily) ch=' ';
                if (zs[wpr]==0) sh=' ';
                else sh=*(z+(zs[wpr]-1)*ed1+wpc);
                if (rr<r3+1) write_string(&ch,1,sh,rr+1,cc+8);
                }
            ++wf; if (wf==wn) wf=0;
            break;
            }
        return(1);
        }

static void tfilerr()
        {
        PR_EBLD;
        sur_print("\nNot enough space on the disk!");
        WAIT; PR_ENRM;
        }

static void tfilename(char chainfile[],char tchain[])
        {
        *chainfile=EOS;
        if (strchr(tchain,':')==NULL) strcat(chainfile,edisk);
        strcat(chainfile,tchain);
        if (strchr(tchain+strlen(tchain)-4,'.')==NULL) strcat(chainfile,".TCH");
        }

static int tsave(char chain[],char tchain[])
        {
        FILE *file;
        char chainfile[LNAME];
        int i;

        tfilename(chainfile,tchain);
        file=fopen(chainfile,"wb");
        if (file==NULL) return(0);
        i=0;
        while (chain[i]!='\0') putc((int)chain[i++],file);
        putc('\0',file);
        i=1;
        if (ferror(file)) { tfilerr(); i=-1; }
        muste_fclose(file);
        return(i);
        }

static int tload(char chain[],char tchain[])
        {
        FILE *file;
        char chainfile[LNAME];
        int i;

        tfilename(chainfile,tchain);
        file=fopen(chainfile,"rb");
        if (file==NULL) return(0);
        i=0;
        while (!feof(file)) chain[i++]=(char)getc(file);
        if (ferror(file)) { tfilerr(); i=-1; }
        muste_fclose(file);
        return(i-1);
        }



static void ch_save(char type,int m)
        {
// RS REM        int i;

        nch+=2;
        chain[nch-2]=(char)type; chain[nch-1]=(char)m;
        }       

static void save_word(char word[])
        {
        int i;
        for (i=0; i<strlen(word); ++i) chain[nch++]=word[i];
        chain[nch++]='@';
        }

static void load_word(char word[])
        {
        int i=0;
        while (chain[nch]!='@') word[i++]=chain[nch++];
        word[i]=EOS; ++nch;
        }          

static int compf(char f[])
        {
        int i;
        char F[32];
        double mvara;
        extern double probit(),uniform();

        eval();
        strncpy(F,muste_strupr(f),32);
        if (strncmp(F,"SQR",3)==0)
              { opnd[0]=sqrt(opnd[0]); return(1); }
        if (strncmp(F,"LOG",3)==0)
              { opnd[0]=log(opnd[0]); return(1); }
        if (strncmp(F,"EXP",3)==0)
              { opnd[0]=exp(opnd[0]); return(1); }
        if (strncmp(F,"SIN",3)==0)
              { opnd[0]=sin(opnd[0]); return(1); }
        if (strncmp(F,"COS",3)==0)
              { opnd[0]=cos(opnd[0]); return(1); }
        if (strncmp(F,"TAN",3)==0)
              { opnd[0]=tan(opnd[0]); return(1); }
        if (strncmp(F,"ATN",3)==0 || strncmp(F,"ARCTAN",6)==0)
              { opnd[0]=atan(opnd[0]); return(1); }
        if (strncmp(F,"INT",3)==0)
              { opnd[0]=floor(opnd[0]); return(1); }
        if (strncmp(F,"ABS",3)==0)
              { opnd[0]=fabs(opnd[0]); return(1); }
        if (strncmp(F,"SGN",3)==0)
              { if (opnd[0]>0.0) opnd[0]=1.0;
                else if (opnd[0]<0.0) opnd[0]=-1.0;
                return(1);
              }
        if (strncmp(F,"INT",3)==0)
              { if (opnd[0]>0.0) opnd[0]=1.0;
                else opnd[0]=0.0;
                return(1);
              }
        if (strncmp(F,"RND",3)==0)
              { opnd[0]=uniform(opnd[0]); return(1); }
        if (strncmp(F,"PROBIT",3)==0)
              { opnd[0]=probit(opnd[0]); return(1); }


        mvara=dmemory[0]; dmemory[0]=opnd[0];
        i=-1;
        muste_fixme("\nFIXME: Extra funtions not implemented in touch mode!");
// RS NYI FIXME        i=f_tiedosto(f,dmemory,0,&opnd[0]);
                             /* 0= n tuntematon */
        dmemory[0]=mvara;
        if (i>0) return(1);

        sprintf(sbuf,"Error in function %s!",f); sur_print(sbuf);
        WAIT; return(-1);
        }

static void funct()
        {
        int k;

        if (s!=2)
            {
            if (t==0) return;
            if (f_ind==0)
                {
                fr=r; fc=c; f_ind=s+1; s=0;
                touch_res("Go to touch a function name by '@'!");
                return;
                }

            k=touch(); if (k<0) *tsana=EOS;
            r=fr; c=fc; cursor(r,c); s=f_ind-1; f_ind=0;

            if (s==1) save_word(tsana);
            k=compf(tsana); if (k<0) { s=0; ar=r; ac=c; stop_res(); return; }
            fconv(opnd[0],"",tsana);
            strcpy(trivi,tsana);
            touch_res(tsana);

            return;
            }
        /* s=2 */
        load_word(tsana);
        k=compf(tsana); if (k<0) { s=0; stop_res(); return; }
        fconv(opnd[0],"",tsana);
        }      


static void key_M()
        {
        int k;
        char x[LLENGTH];
        static char memsana[LMEMORY];

        if (s!=2)
            {
            if (t==0) return;
            if (M_ind==0)
                {
                fr=r; fc=c; M_ind=s+1; s=0;
                eval(); fconv(opnd[0],"",memsana);
                strcpy(x,"Save result "); strcat(x,memsana);
                strcat(x," by touching index 0,1,...,9 by 'M'!");
                touch_res(x);
                return;
                }


            k=touch(); if (k<0) k=0; else k=atoi(tsana);
            if (k>=0 && k<NMEMORY)
                {
                strncpy(memory[k],memsana,LMEMORY);
                dmemory[k]=opnd[0];
                }
            r=fr; c=fc; s=M_ind-1; M_ind=0;
            clear_res();

            strcpy(x,"0: ");
            strcat(x,memsana); strcat(x," saved in memory location ");
            fconv((double)k,"",tsana); strcat(x,tsana);
            touch_res(x);

            if (s==1) save_word(tsana);
            return;
            }

        /* s=2 */
        load_word(tsana); k=atoi(tsana);
        eval(); fconv(opnd[0],"",memsana);
        strncpy(memory[k],memsana,LMEMORY);
        clear_res();
        }

static void key_K()
        {
        int k;
// RS REM        char x[LLENGTH];

        if (s!=2)
            {
            fr=r; fc=c; K_ind=s+1; s=0;
            touch_res("Go to touch a memory location index by +,-,* or / !");
            return;
            }

        K_ind=3; load_word(tsana); k=atoi(tsana); strcpy(tsana,memory[k]);
        }

static void touch_memory(int m)
        {
        int k;

        if (s!=2)
            {
            k=touch(); if (k<0) k=0; else k=atoi(tsana);
            r=fr; c=fc; cursor(r,c); s=K_ind-1; K_ind=0;

            if (s==1) { save_word(tsana); ch_save('0',m); }

            strcpy(tsana,memory[k]);
            return;
            }

        K_ind=0;
        }

static void key_F()
        {
        int k;
        char x[LLENGTH];

        if (s!=2)
            {
            if (F_ind==0)
                {
                fr=r; fc=c; F_ind=s+1; s=0;
                touch_res("Touch a format by 'F'!");
                return;
                }


            k=touch(); if (k<0) *oform=EOS; else strcpy(oform,tsana);
            r=fr; c=fc; cursor(r,c); s=F_ind-1; F_ind=0;

            if (*oform==EOS)
                touch_res("Free format selected!");
            else
                {
                strcpy(x,"Format "); strcat(x,oform); strcat(x," selected!");
                touch_res(x);
                }
            if (s==1) save_word(oform);
            return;

            }

        /* s=2 */
        load_word(oform);
        }

static void key_C()
        {
// RS REM        char x[LLENGTH];

        if (s!=2)
            {
            fr=r; fc=c; C_ind=s+1; s=0;
            touch_res("Go to touch a number as a constant by +,-,* or / !");
            return;
            }
        /* s=2 */
        C_ind=3; load_word(tsana);
        }

static void touch_const(int m)
        {
        int k;

        if (s!=2)
            {
            k=touch(); if (k<0) strcpy(tsana,"0");
            r=fr; c=fc; cursor(r,c); s=C_ind-1; C_ind=0;

            if (s==1) { save_word(tsana); ch_save('0',m); }

            return;
            }
        /* s=2 */
        C_ind=0;
        }

static void key_T()
        {
        int k;
        char x[LLENGTH];

        k=touch(); if (k<0) return;
        strcpy(tchain,tsana);
        k=tsave(chain,tchain);
        if (k<=0)
            {
            PR_EBLD;
            sprintf(sbuf,"\nCannot save chain %s!",tchain); sur_print(sbuf);
            WAIT; PR_ENRM; disp_touch(); return;
            }
        strcpy(x,"Chain "); strcat(x,tchain); strcat(x," saved!");
        touch_res(x);
        }

static int key_L()
        {
        int k;
        char x[LLENGTH];

        k=touch(); if (k<0) return(-1);
        strcpy(tchain,tsana);
        k=tload(chain,tchain);
        if (k<=0)
            {
            PR_EBLD;
            sprintf(sbuf,"\nChain file %s not found!",tchain); sur_print(sbuf);
            WAIT; PR_ENRM; disp_touch(); return(-1);
            }
        strcpy(x,"Chain "); strcat(x,tchain); strcat(x," loaded!");
        touch_res(x);
        return(k);
        }

static void chain_init()
        {
        s=1;
        nch=0;
        P_ind=0;
        R_ind=0;
        ar=r; ac=c;
        }

static void arrow_save(int m)
        {

        if (nch>2)
            {
            if (chain[nch-2]==(char)m && chain[nch-3]=='1')
                { chain[nch-1]++; return; }
            }
        nch+=3;
        chain[nch-3]='1'; chain[nch-2]=(char)m; chain[nch-1]=1;
        }

static void std_keys(int m)
                {
                unsigned int i,j;
                int k;
                char x[LLENGTH], x1[LLENGTH];

                if (worm) return;
                if (collect && m=='=') { collect_results(); return; }
                if (s==1) ch_save('0',m);
                if (s!=2) key=m;
                switch (m)
                    {
                  case '+': v=1;
                    if (K_ind) touch_memory(m);
                    else
                    if (C_ind) touch_const(m);
                    else {  k=touch(); if (k<0) return; }
                    comp(m); break;
                  case '-': v=2;
                    if (K_ind) touch_memory(m);
                    else
                    if (C_ind) touch_const(m);
                    else {  k=touch(); if (k<0) return; }
                    comp(m); break;
                  case '*': if (t==0) return;
                    v=3;
                    if (K_ind) touch_memory(m);
                    else
                    if (C_ind) touch_const(m);
                    else {  k=touch(); if (k<0) return; }
                    comp(m); break;
                  case '/': if (t==0) return;
                    v=4;
                    if (K_ind) touch_memory(m);
                    else
                    if (C_ind) touch_const(m);
                    else {  k=touch(); if (k<0) return; }
                    comp(m); break;
                  case '=':
                  case '!':
                  case ';':
                            if (isalpha(trivi[1])) // 25.11.2009
                                { print_words(); break; }
                            eval(); print_res(m);
                            break;
                  case 'S': eval(); disp_res();
                            break;
                  case 'P': if (R_ind) break;
                            if (P_ind) { P_ind=0; r=pr; c=pc; break; }
                            P_ind=1; pr=r; pc=c; break;
                  case 'R':
/*
printf("\ns=%d R_ind=%d nch=%d\n",s,R_ind,nch); getch();
*/


                            if (s==0) break;
                            if (s==1) { R_ind=1; s=2; R_nch=nch; nch=0; break; }
                  /* s=2 */ if (R_ind==0) { R_ind=2; R_nch=nch; nch=0; break; }
                            if (R_ind==1) { R_ind=1; nch=0; break; }
                            R_ind=2; s=2; nch=0; break;
                  case 'T': key_T(); break;
                  case 'L': k=key_L();
                            nch=k;
                            break;
                  case 'F': key_F();
                            break;
                  case 'C': key_C();
                            break;
                  case 'M': key_M(); break;  /* tf.c */
                  case 'K': key_K(); break;  /* tf.c */
                  case '@': funct(); break;  /* tf.c */
                  case 'U': take_also_plus=1; // 17.10.2001
                  case 'W': k=touch(); if (k<0) { *wsana=EOS; break; }
                            strcpy(wsana,tsana);
                            if (tch_open) touch_data_save(wsana);
                            take_also_plus=0;
                            break;
                  case 'B': *wsana=*(z+ed1*(r1+r-2)+c1+c-1);
                            wsana[1]=EOS;
                            PR_EINV; sprintf(sbuf,"%c",*wsana); sur_print(sbuf); PR_ENRM;
                            if (tch_open) touch_data_save(wsana);
                            break;
                  case 'E': edread(x,r1+r-1);
                            strcpy(wsana,x+c1+c-1);
                            k=strlen(wsana); while (wsana[k-1]==' ') wsana[--k]=EOS;
                            PR_EINV; sprintf(sbuf,"%.*s",c3-c+1,wsana); sur_print(sbuf); PR_ENRM;
                            if (tch_open) touch_data_save(wsana);
                            break;
                  case 'A': edread(x,r1+r-1);
                            *wsana=EOS; strncat(wsana,x+1,c1+c-1);
                            write_string(wsana+c1-1,c,'7',r+1,9);
                            if (tch_open) touch_data_save(wsana);
                            break;

                  case 'D': k=touch(); if (k<0) strcpy(tsana,"TOUCH.TXT");
                            touch_data_open(tsana);
                            break;

                  case 'V': k=collect_lines();
                            if (k<0) return; // RS CHA exit(0);
                            break;

                  default: key=' ';
                           if (c==0) { dispch(m);
                                       if (r<r3) { ++r; break; }
                                       if (r1<r2-r3+1) { ++r1; disp_touch(); break; }
                                       break;
                                     }
                           if (c<=c3 && c<=c2) { dispch(m); ++c; break; }
                           if (c>c3)
                            {
                            if (s==2)
                                {
                                if (c1+c-1>c2)
                                    { s=0; r=ar; c=ac; break; }
                                dispch(m); ++c; break;
                                }
                            if (r1+r-1==r2) { BEEP; break; }
                            edread(x1,r1+r);
                            if (!empty(x1+1,c2) ) { BEEP; break; }
                            edread(x,r1+r-1); i=c1+c-2;
                            while (i>10)
                                {
                                if (x[i]==' ') break;
                                --i;
                                }
                            if (i<=10) { BEEP; break; }
                            for (j=i+1; j<=c1+c-2; ++j)
                                { x1[c1+j-i-1]=x[j]; x[j]=' '; }
                            edwrite(x,r1+r-1,0); edwrite(x1,r1+r,0);
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
                }

static void insert()
        {
                    unsigned j=r1+r-1;
                    char x[LLENGTH], x1[LLENGTH];
                    edread(x,j);
                    if (x[c2]!=' ') { BEEP; return; }
                    strcpy(x1,x); x[c1+c-1]=EOS;
                    strcat(x," "); strcat(x,x1+c1+c-1); x[ed1]=EOS;
                    edwrite(x,j,0);
                    if (zs[j]!=0)
                        {
                    edread(x,zs[j]);
                    strncpy(x1,x,ed1); x[c1+c-1]=EOS;
                    strcat(x," "); strcat(x,x1+c1+c-1); x[ed1]=EOS;
                    edwrite(x,zs[j],0);
                    testshad(j);
                        }
                    displine2(j);
        }

static void delete()
        {
                    unsigned int j=r1+r-1;
                    char x[LLENGTH]; // RS REM , x1[LLENGTH];
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

static void insertl()
        {
                    unsigned int j;
                    char x[LLENGTH]; // RS REM , x1[LLENGTH];
                    edread(x,r2);
                    if (!empty(x+1,c2)) { BEEP; return; }
                    memmove(z+(r1+r)*ed1,z+(r1+r-1)*ed1,(r2-r1-r)*ed1);
                    strcpy(x,"*"); strncat(x,space,c2);
                    edwrite(x,r1+r,0);
                    j=ed2-1;
                    for (; j>r1+r-1; --j) zs[j+1]=zs[j];
                    zs[r1+r]=0;
                    if (r<r3)
                        {
                        SCROLL_DOWN(r+1,r3,1);
                        displine2(r1+r);
                        for (j=r1+r+1; j<r1+r3; ++j)
                            displine(j,0);
                        }

                    if (r<r3) ++r;
        }

static void deletel()
        {
                    unsigned int j;
                    char x[LLENGTH]; // RS REM , x1[LLENGTH];
                    unsigned int l;
                    memmove(z+(r1+r-2)*ed1,z+(r1+r-1)*ed1,(r2-r1-r+2)*ed1);
                    strcpy(x,"*"); strncat(x,space,c2);
                    edwrite(x,r2,0);
                    l=ed2-1;
                    for (j=r1+r-1; j<=l; ++j) zs[j]=zs[j+1];
                    zs[l+1]=0;
                    if (r<r3)
                        {
                        SCROLL_UP(r,r3,1);
                        for (j=r1+r-1; j<r1+r3; ++j)
                            displine(j,0);
                        }
                    displine2(r1+r3-1);
        }

static void era(unsigned int j)
                        {
                        char x[LLENGTH];
                        edread(x,j);
                        x[c1+c-1]=EOS; strncat(x,space,ed1-c1-c+1);
                        edwrite(x,j,0);
                        displine2(j);
                        }

static void erase()
        {
                    unsigned int j;
                    char x1[LLENGTH]; // RS REM , x[LLENGTH];

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


static int find_right(int c)
        {
        unsigned int i;
        int k, lim;
        if (s==2) lim=c2-c1; else lim=c3;
        i=ed1*(r1+r-2)+c1+c-1; k=c;
        while ( z[i]!=' ' && z[i]!=',' && k<lim ) { ++i; ++k; }
        if (k==lim) return(-1);
        while ( (z[i]==' ' || z[i]==',') && k<lim ) { ++i; ++k; }
        if (k==lim) return(-1);
        while ( z[i]!=' ' && z[i]!=',' && z[i]!='.' && k<lim ) { ++i; ++k; }
        if (k==lim) return(-1);
        --k;
        return(k-c);
        }

static int find_down(int r)
        {
        unsigned int i;
        int k, lim, haku=1;

        if (s==2) lim=r2-r1; else lim=r3;
        i=ed1*(r1+r-1)+c1+c-1; k=r+1;
        while (haku)
            {
            while ( z[i]==' ' &&  k<lim ) { i+=ed1; ++k; }
            if (z[i]!='.') break;
            if (strncmp(z+i-c1-c+2,"..........",10)==0) k=lim;
            break;
            }
        if (k==lim) return(-1);
        return(k-r);
        }

static void free_save(int m)
        {
        nch+=3;
        chain[nch-3]='1'; chain[nch-2]=(char)m; chain[nch-1]=(unsigned char)255;
        }


static void prefix()
        {
        int m;
// RS REM        unsigned int i,tc;
        int k;

        m=nextch_touch();
/* printf("\n%d\n",m); getch(); */
        pref=' ';

        if (special)
            {
            switch(m)
                {
              case CODE_PRE:
                if (c<c3) { c=c3; if (c>c2) c=c2; break; }
                if (c1==c2-c3+1) break;
                c1+=c3; if (c1>c2-c3+1) c1=c2-c3+1;
                disp_touch(); break;
              case CODE_RIGHT:
                k=find_right(c);
                if (k<0) { BEEP; break; }
                c+=k;
                if (s==1)
                    last_right=k;
                    free_save(m);
                break;
              case CODE_DOWN:
                k=find_down(r);
                if (k<0) { BEEP; break; }
                r+=k;
                if (s==1)
                    last_down=k;
                    free_save(m);
                break;
              case CODE_EXEC:
                if (s==0) { if (nch==0) break;
                            tdisp=0; touch_run();
                            break;
                          }
                /* s=1 */   ch_save('1',m);
                            chain[nch]='\0';
                            tdisp=0;
                            touch_run();
                            break;

          default: sprintf(sbuf,"\nTSPRE: %d %d\n",special,m); sur_print(sbuf); sur_getch();
                }
            }
        else /* not special */
            {
            switch(m)
                {
              case '0': clear_res();
                        if (s==1) ch_save('1',ERASE);
                        break;
              case 'T': tut_special_touch(); break;

          default: ; /* printf("\nTSPRE: %d %d\n",special,m); getch(); */
                }
            }
        }



static void scat(char *sana,int *pi)
        {
        int i=*pi;
        int len=strlen(sana);

        while (chain[++i]!='@') sana[len++]=chain[i]; sana[len]=EOS;
        *pi=i;
        }

static void cat(char *sana,int i)
        {
        char luku[5];
        int k;

        k=chain[i];
        if (k==1) return;
        if (k==255) { strcat(sana,"n"); return; }
        sprintf(luku,"%d",k);
        strcat(sana,luku);
        }


static void load(char *nimi)
        {
        int i,j;
        char x[LLENGTH];
        char sana[LLENGTH];
        int kesken;

        i=tload(chain,nimi);
        if (i<=0)
            {
            sprintf(sbuf,"\nTouch chain %s not found!",nimi);
            sur_print(sbuf); WAIT; return;
            }
        j=r1+r-1;
        i=0; *x=EOS;
        kesken=1;
        while (kesken)
            {
            if (chain[i]=='0')
                {
                ++i;
                *sana=chain[i]; sana[1]=EOS;
                switch ((unsigned int)chain[i])
                    {
                  case '@':
                  case 'F':
                  case 'C':
                  case 'M':
                  case 'K':
                            scat(sana,&i); break;
                    }
                ++i;
                }
            else
                {
                ++i;
                switch ((unsigned int)chain[i])
                    {
                  case CODE_EXEC:   strcpy(sana,"CTNUE"); kesken=0; break;
                  case CODE_EXIT:   strcpy(sana,"END"); kesken=0; break;
                  case CODE_LEFT:   strcpy(sana,"l"); cat(sana,++i); ++i; break;
                  case CODE_RIGHT:  strcpy(sana,"r"); cat(sana,++i); ++i; break;
                  case CODE_UP:     strcpy(sana,"u"); cat(sana,++i); ++i; break;
                  case CODE_DOWN:   strcpy(sana,"d"); cat(sana,++i); ++i; break;

                  default: printf("\nUnknown char %d",(int)chain[i]); WAIT; return;
                    }
                }
            if (strlen(x)+strlen(sana)>=c3)
                {
                ++j;
                if (j>=r2)
                    {
                    sprintf(sbuf,"\nNot enough lines in the edit field!");
                    sur_print(sbuf); WAIT; return;
                    }
                edwrite(space,j,1);
                edwrite(x,j,1);
                *x=EOS;
                }
            strcat(x,sana); strcat(x," ");
            }
        ++j;
        edwrite(space,j,1);
        edwrite(x,j,1);
        }

static void jatka(char m,int k,int *pi)
        {
        int i=*pi;

        chain[i++]=m; chain[i++]=(unsigned char)k;
        *pi=i;
        }

static void jatka2(int koodi,char *sana,int *pi)
        {
        int i;
        int h;

        i=*pi;
        chain[i++]='1';
        chain[i++]=(unsigned char)koodi;
        if (*sana=='n') h=255;
        else if (*sana==EOS) h=1;
        else h=atoi(sana);
        chain[i++]=(unsigned char)h;
        *pi=i;
        }

static void jatka3(char *sana,int *pi)
        {
        int i;
        char *p;

        i=*pi;
        p=sana;
        chain[i++]='0';
        while (*p) chain[i++]=*p++;
        chain[i++]='@';
        *pi=i;
        }


static void save(char *nimi)
        {
        int i,j,g,h;
        char x[LLENGTH];
        char *sana[EP4];
        int kesken;

        j=r1+r;
        edread(x,j);
        g=split(x+1,sana,EP4);
        if (g==0) { sprintf(sbuf,"\nNo chain below the TCHSAVE line!");
                    sur_print(sbuf); WAIT; return; }
        i=0;
        kesken=1;
        while (kesken)
            {
            for (h=0; h<g; ++h)
                {
                if (muste_strcmpi(sana[h],"CTNUE")==0)
                    {
                    jatka('1',CODE_EXEC,&i);
                    kesken=0;
                    break;
                    }
                if (muste_strcmpi(sana[h],"END")==0)
                    {
                    jatka('1',CODE_EXIT,&i);
                    kesken=0;
                    break;
                    }
                switch (sana[h][0])
                    {
                  case 'l': jatka2(CODE_LEFT,sana[h]+1,&i); break;
                  case 'r': jatka2(CODE_RIGHT,sana[h]+1,&i); break;
                  case 'u': jatka2(CODE_UP,sana[h]+1,&i); break;
                  case 'd': jatka2(CODE_DOWN,sana[h]+1,&i); break;
                  case '@':
                  case 'F':
                  case 'C':
                  case 'M':
                  case 'K':
                            jatka3(sana[h],&i); break;
                  default : chain[i++]='0'; chain[i++]=sana[h][0];
                    }
                }
            if (kesken)
                {
                ++j;
                if (j>r2)
                   {
                   sprintf(sbuf,"\nChain not ending to END or CTNUE");
                   sur_print(sbuf); WAIT; return;
                   }
                edread(x,j);
                g=split(x+1,sana,EP4);    /* x -13.2.1992 */
                }
            }
        chain[i]=EOS;
        tsave(chain,nimi);
        }


static void tsaveload(char *siirtop)
        {
        s_init(siirtop);

        if (muste_strcmpi(word[0],"TCHLOAD")==0) { load(word[1]); s_end(siirtop); }
        else if (muste_strcmpi(word[0],"TCHSAVE")==0) save(word[1]);
        }

static void help()
        {
        CLS;
        SCROLL_UP(1,r3+1,r3+1);
        sur_locate(2,1); PR_EIN2;

sur_print("\n");
sur_print("Operations in touch mode:\n"); PR_EUDL;
sur_print("Move the cursor with the arrow keys. When it touches a number, activate\n");
sur_print("by +,-,* or /. The current expression will be displayed on the bottom line.\n");
sur_print("To print the result in the edit field, indicate the place by the cursor\n");
sur_print("and press =. Return back to normal editing mode by RETURN.\n");
sur_print("Control keys:\n");
sur_print("S evaluates the current expression without printing.\n");
sur_print("C enters a constant,  F enters a format for results.\n");
sur_print("@ enters a function (sqrt,log,exp,sin,cos,tan,arctan,abs,int).\n");
sur_print("M saves the current result to any of memory locations 0,1,2,...,9.\n");
sur_print("K gets a value saved by M.\n");
sprintf(sbuf,"%s 0 clears the current expression to 0.\n",key_label[CODE_PRE]);
        sur_print(sbuf);
PR_EIN2; sur_print("Touch chains:\n"); PR_EUDL;
sprintf(sbuf,"%s initiates the definition of a touch chain.\n",key_label[CODE_DISK]);
        sur_print(sbuf);
sprintf(sbuf,"%s terminates the definition.\n",key_label[CODE_EXIT]);
        sur_print(sbuf);
sprintf(sbuf,"%s starts execution of the chain. If it is pressed under definition,\n",
        key_label[CODE_EXEC]);
        sur_print(sbuf);
sur_print("    definition is terminated and the chain executed repetitively.\n");
sur_print("R repeats the chain under definition; definition can be continued.\n");
sur_print("P saves the current cursor position; if P is pressed again, the cursor\n");
sur_print("  return to the saved position.\n");
sur_print("T saves the current chain,  L loads a chain saved by T.\n");

        sur_locate(r3+2,1); PR_EIN2;
        sur_print("Next page by pressing any key!");
        nextch_touch();
        CLS;
        SCROLL_UP(1,r3+1,r3+1);
        sur_locate(2,1); PR_EIN2;

sur_print("\n");
sur_print("Collecting data in a text file in touch mode:\n"); PR_EUDL;
sur_print("Numbers, words, characters and parts of edit lines can be collected\n");
sur_print("in a selected text file as consecutive lines. If the text file already\n");
sur_print("exists, new items are appended to it.\n");
sur_print("This procedure starts in touch mode by touching the (path)name of the\n");
sur_print("text file by `D'. Thereafter items are selected as follows:\n");
sur_print("W saves the current word (or number) touched by the cursor,\n");
sur_print("B saves the current character,\n");
sur_print("E saves the current line to the right from the cursor.\n");
sur_print("A saves the current line to the left  from the cursor.\n");
sur_print("\n");
sur_print("To stop data collecting, activate any word by `D' again or exit\n");
sur_print("the touch mode by ENTER.\n");
sur_print("The text file thus created can be processed by other means of Survo.\n");
sur_print("For example, it is loaded into the edit field by the LOADP command.\n");
sur_print("\n");
PR_EIN2;
sur_print("Working in `worm' mode:\n"); PR_EUDL;
sprintf(sbuf,"Worm mode is entered from touch mode by the %s key!\n",key_label[CODE_WORDS]);
sur_print(sbuf);
sur_print("More information by activating TOUCH?\n");

        sur_locate(r3+2,1); PR_EINV;
        sur_print("Press any key!  (More information by TOUCH?)");
        nextch_touch();
        PR_ENRM; disp_touch();
        }


void muste_touch(int argc, char *argv[])
        {
        unsigned int i,trun;
// RS REM        char x[LLENGTH], x1[LLENGTH];
        int m=0;
        int prevkey, nleft=0;
        int k;
// RS REM        int worm2;

        for (i=0; i<LLENGTH; ++i) { space[i]=' '; stripe[i]=STRIPE; }
        s_init(argv[1]);
        labels(); // 17.7.2000
//      s_edt(argv[1]);

        sprintf(sbuf,"%s - Touch mode",system_name);
        sur_set_console_title(sbuf);
        headline_touch(); // RS ADD

        if (strcmp(info,"TOUCH")==0) { tsaveload(argv[1]); return; }
        space_break=0;
        if (etu==1) { tutor=muste_fopen(etufile,"r+b"); muste_fseek(tutor,tutpos,0); }
        if (etu==2) { tutor=muste_fopen(etufile,"rb");  muste_fseek(tutor,tutpos,0); }

        touch_init();
        trun=1;
        m=0;
        if (etu==1) lue_hetki(&wait_hetki);

        while (trun)
            {
            if (worm)
                {
                k=worm_mode(1,m);
                if (k<0) return;
                }
            if (rikottu) return;
            if ((s!=2 || !ibm) && r<=r3 && r>0 && tdisp==1) cursor(r,c);
/*
printf("\n"); ERASE; printf("Chain:");
for (k=0; k<nch; ++k) printf(" %d",(int)(signed char)chain[k]); printf("\n"); getch();
*/
            prevkey=m;
            m=nextch_touch(); key=' ';
/* printf("\n%d\n",m); getch(); */
            if (worm && strchr("SCDREe=",(char)m)!=NULL) continue;
            if (special)
                {
                switch (m)
                  {
                  case CODE_RETURN:
                    touch_clear();
                    if (etu>0) 
                      { 
                      tutpos=ftell(tutor);
                      muste_fclose(tutor);
                      }
                    touch_data_close();
                    s_end(argv[1]);
                    return;
                  case CODE_RIGHT:
                    if (s==2) { k=(int)chain[nch++];
                                if (k==-1 || k==255)
                                    {
                                    k=find_right(c);
                                    if (k<0) { k=last_right;
                                               if (k<=0) k=10;
                                             }
                                    last_right=k;
                                    }
                                c+=k;
                                if (c1+c-1>c2) { stop_res(); break; }
                                break;
                              }
                    if (s==1) arrow_save(m);
                    key=K_RIGHT;
                    if (c<c3 && c<c2) { ++c; break; }
                    if (c2-c1+1>c3) { ++c1; disp_touch(); break; }

                    break;
                  case CODE_LEFT:
                    if (s==2) { c-=(int)chain[nch++];
                                if (c1+c-1<1) { stop_res(); break; }
                                break;
                              }
                    if (s==1) arrow_save(m);
                    key=K_LEFT;
                    if (c>1) { --c; break; }
                    if (c1>1) { --c1; disp_touch(); break; }
                    if (prevkey==CODE_LEFT)
                        {
                        if (nleft>=2) { c=0; break; }
                        ++nleft;
                        break;
                        }
                    nleft=0;
                    break;
                  case CODE_UP:
                    if (s==2) { r-=(int)chain[nch++];
                                if (r1+r-1<1) { stop_res(); break; }
                                break;
                              }
                    if (s==1) arrow_save(m);
                    key=K_UP;
                    if (r>1) { --r; break; }
                    if (r1>1)
                        {
                        --r1;
                        SCROLL_DOWN(1,r3,1);
                        displine2(r1);
                        break;
                        }
                    break;
                  case CODE_DOWN:
                    if (s==2) { k=(int)chain[nch++];
                                if (k==-1)
                                    {
                                    k=find_down(r);
                                    if (k<0) { k=last_down;
                                               if (k<=0) k=1;
                                             }
                                    last_down=k;
                                    }
                                r+=k;
                                i=ed1*(r1+r-2);
                                if (z[i]=='E' || r1+r-1>r2)
                                    { stop_res(); break; }
                                if (z[i+c1+c-1]!='.') break;
                                if (strncmp(z+i+1,"..........",10)==0)
                                      stop_res();
                                break;
                              }
                    if (s==1) arrow_save(m);
                    key=K_DOWN;
                    if (r<r3) { ++r; break; }
                    if (r1<r2-r3+1)
                        {
                        ++r1;
                        SCROLL_UP(1,r3,1);
                        displine2(r1+r3-1);
                        break;
                        }
                    break;
                  case CODE_HOME:
                    if (s==1) ch_save('1',m);
                    if (c>1) { c=1; break; }
                    if (c1>1) { c1=1; disp_touch(); break; }
                    if (r>1) { r=1; break; }
                    if (r1>1) {r1=1; disp_touch(); break; }
                    break;
                  case CODE_INSERT:
                    insert(); break;
                  case CODE_DELETE:
                    delete(); break;
                  case CODE_INSERTL:
                    insertl(); break;
                  case CODE_DELETEL:
                    deletel(); break;
                  case CODE_ERASE:
                    if (s==2) { clear_res(); break; }
                    erase(); break;
                  case CODE_NEXT:
                    r1+=r3;
                    if (r1>r2-r3+1) r1=r2-r3+1;
                    disp_touch();
                    break;
                  case CODE_PREV:
                    if (r1<r3+2) r1=1; else r1-=r3;
                    disp_touch();
                    break;
                  case CODE_DISK:
                    chain_init();
                    key=K_DEF;
                    break;
                  case CODE_EXEC:
                  /* touch_run() mÑÑritelty tÑssÑ tiedostossa! */
                    key=K_RUN;
                    if (s==0) { if (nch>0) touch_run(); break; }
                    if (s==1) { ch_save('1',m);
                                chain[nch]='\0';
       /*       printf("\n%s",chain); getch();          */
                                touch_run();
                                break;
                              }
                    /* s=2 */   nch=0;
                                break;

                  case CODE_EXIT:
                    key=K_STOP;
                    if (s==0) break;
                    if (s==1) { ch_save('1',m);
                                chain[nch]='\0';
                                s=0;
                                break;
                              }
                    /* s=2 */   s=0;
                                stop_res();
                                break;

                  case CODE_PRE:
                    pref=PREFIX;
                    prefix();
                    break;

                  case CODE_HELP:
                    help();
                    break;

                  case CODE_END:        /* 11.7.1994 */
                    seek_line_end(); break;

                  case CODE_WORDS:
                    worm_mode(0,' '); break; // RS ADD ,' '

                  case 0: break;  /* vain keskeytykseen */


                  }
                } /* end special */

            else std_keys(m);

            }
        }

