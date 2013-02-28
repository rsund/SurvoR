#include "muste.h"
/* prompts.c 25.12.1985/SM (17.3.1993)

*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"


#define TUT_COMMENT_CODE 252

extern int sur_get_message();

extern int etu;
static int special;
extern FILE *tutor;
static int tut_special_code=0;


extern int read_tutword();
int tutch();
static int tut_special();
static int Wdisp();

static void tutsave(int m)
        {
        putc(m,tutor);
        }

int prompt(char *kysymys,char *vastaus,int pituus)
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
            SAVE_CURSOR;
            m=nextch("");
            RESTORE_CURSOR;      
                switch (m)
                    {
                  case -9:
                    return(-9);
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
                    return(1);
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
                  case CODE_BACKSP: return(2);

                  default:
                    sprintf(sbuf,"%c",m); sur_print(sbuf); tila[pos-1]=(char)m;
                    if (pos<pituus) ++pos; else PR_LEFT;
                    break;
                    }
            }
        return(1);
        }

void pvmaika(char aika[])
        {
        time_t ltime;

        time(&ltime);
        strcpy(aika,s_time(&ltime)); aika[24]=EOS;
        }

static void check_alarm(char *aika)    /* 14.8.1992 */
        {
        int k;
        char x[LLENGTH];

        k=hae_apu("alarm",x);
        if (k && !sur_alarm)
            {
            if (strncmp(x,aika+11,8)<=0)
                {
                display_off=0; 
                LOCATE(1,1); BEEP; PR_EBLK;
                sur_print("\n ALARM!!!     Press # "); PR_ENRM;
                while (k!='#') k=sur_getch();
//                LOCATE(1,1); PR_ENRM; sur_print("\n                      ");
                ++sur_alarm;  disp();
                }
            }
        }


void headline(char *valinta)
        {
        char aika[26];
        int rr,cc;
        char x[LLENGTH];
        int k;        

        pvmaika(aika);
        CURSOR_POS(&rr,&cc);
        LOCATE(1,1);
        PR_EUDL;        

        sprintf(x,"  %4u",c1+c-1);
        write_string(x,6,'4',1,1);        

        sprintf(x,"%3u",c1); write_string(x,3,')',1,7);
        sprintf(x," %s ",system_name);

        write_string(x,strlen(system_name)+2,'7',1,10);
        k=23+c3-72; // RS CHA 20 -> 23

        strcpy(sbuf,edisk); unsubst_survo_path_in_editor(sbuf);
        sprintf(x,"  %s %*.*s%7d%5d  ",aika,k,k,sbuf,r2,c2);
        write_string(x,strlen(x),'4',1,17); // RS 20 -> 17

        if (*valinta==EOS) { check_alarm(aika); LOCATE(rr,cc); PR_ENRM; return; }

        LOCATE(r3+2,1); PR_EINV;
        snprintf(sbuf,c3+8,"%s",valinta); sur_print(sbuf);
        
        check_alarm(aika);
        LOCATE(rr,cc);
		PR_ENRM;

/* RS Original version
        char aika[26];
        int rr,cc;
        char x[LLENGTH];
        int k;
        
        pvmaika(aika);
        CURSOR_POS(&rr,&cc);
        LOCATE(1,1);
        PR_EUDL;
        sprintf(x,"  %4u",c1+c-1);
        write_string(x,6,'4',1,1);
        sprintf(x,"%3u",c1); write_string(x,3,')',1,7);
        CURSOR_ON; sprintf(x," %s ",system_name);
        write_string(x,strlen(system_name)+2,'7',1,10);
        k=20+c3-72;
        sprintf(x,"  %s %*.*s%7d%5d ",aika,k,k,edisk,r2,c2);
        write_string(x,strlen(x),'4',1,20);

        if (*valinta==EOS) { check_alarm(aika); LOCATE(rr,cc); PR_ENRM; return; }
        LOCATE(r3+2,1); PR_EINV;
        sprintf(sbuf,"%s",valinta); sur_print(sbuf);
        check_alarm(aika);
        LOCATE(rr,cc);
*/        
        }

static int nextkey(char *valinta)
        {
        int m;
        long time1,time2;
// RS REM        char x[LLENGTH];
        extern int muste_help_running; // RS ADD

        time((time_t *)&time1);
        while (1)
            {
            if (*info_2)
                {
                if (sur_get_message(info_2,1))
                    {
                    return(-9);
                    }
                }

if (muste_get_R_int(".muste$exitpressed")) // RS 27.2.2013
    {
    muste_set_R_int(".muste$exitpressed",0);
    return(CODE_EXIT);
    }
    
            if (sur_kbhit()) break;
            time((time_t *)&time2);
            if (difftime(time2,time1)>0.5)
                {
                headline(valinta);
                time1=time2;
                }
            sur_sleep(10);
            }

        special=0;
        m=s_getch();       
        switch (m)
            {
          case EXTEND_CH: m=sur_getch_ext();                 
                          special=1;                          
                  switch (m)
                      {
                        case CODE_EXIT:
                        case CODE_RETURN: 
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
                        case CODE_CODE: break;
                        case CODE_BACKSP: if (!muste_help_running) m=CODE_LEFT; break; // RS ADD
                        case CODE_REF:
                        case CODE_MERGE:
                        case CODE_COPY:
                        case CODE_TAB:
                        case CODE_TABS:
                        case CODE_HELP:
                        case CODE_SRCH:
                        case CODE_ACTIV:
                        case CODE_MOVE:
                        case CODE_END:
                        case CODE_WORDS:
                        case CODE_SOFT_ON:
                        case CODE_RIGHT2:
                        case CODE_LEFT2:
                        case CODE_UP2:
                        case CODE_DOWN2:
                        case CODE_SUCRO1:
                        case CODE_SUCRO2:
                        case CODE_SUCRO3:
                        case CODE_SUCRO4:
                        case CODE_SUCRO5:
                        case CODE_SUCRO6:
                        case CODE_SUCRO7:
                        case CODE_SUCRO8:
                        case CODE_REF_SET:
                        case CODE_REXEC:    
                        case CODE_RIGHT3:   
                        case CODE_PASTE:    
                        case CODE_CLIPCOPY: break;
                        default: m=32; break;
					  }
					  
          case CODE_RETURN: special=1; break;
          case CODE_BACKSP: special=1; m=CODE_LEFT; break;
          case CODE_EXEC: special=1; break;
          case CODE_TAB: special=1; break;
          default: break;
            }                    
        return(m);
        }					  
                      
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
                    case  KEY_CODE:      m=CODE_CODE; break;
                    case  KEY_REF:       m=CODE_REF; break;
                    case  KEY_MERGE:     m=CODE_MERGE; break;
                    case  KEY_COPY:      m=CODE_COPY; break;
                    case  KEY_TABS:      m=CODE_TABS; break;
                    case  KEY_HELP:      m=CODE_HELP; break;
                    case  KEY_SRCH:      m=CODE_SRCH; break;
                    case  KEY_ACTIV:     m=CODE_ACTIV; break;
                    case  KEY_END:       m=CODE_END; break;
                    default: m=32;
                      }
                      break;
*/

int nextch(char *valinta)
        {
        extern int muste_mousewheel;
        int m;

muste_mousewheel=FALSE;

        if (etu==2)
            {
            headline(valinta);
            m=tutch();
            while (m==255) m=tutch();
            if (m!=0) return(m);
            }
        if (etu==1) { m=nextkey(valinta); tutsave(m); return(m); }

		m=nextkey(valinta);
		
muste_mousewheel=TRUE;		
        return (m);
        }


static int press_key(int m)
        {
        int ch;
        int k;

        if (etu1<2) { Wdisp(); return(1); }   /* 23.10.89 ennen etu1==1 */
        k=sur_wait(1000L*(long)tut_wait_c,Wdisp,1);

        if (k) ch=nextkey(""); else ch=m;
        while (ch!=m && ch!=CODE_LEFT)
            {
            if (ch==CODE_HELP) { etu2=0; ch=m; break; }
            BEEP;
            k=sur_wait(1000L*(long)tut_wait_c,Wdisp,1);
            if (k) ch=nextkey(""); else ch=m;
            }

        return(1);
        }


int tutch()
        {
        int Wdisp();
        int nop();
        int m,ch;
        char nimi[16];

        m=getc(tutor);
        while (m==TUT_COMMENT_CODE) m=getc(tutor);  /* 22.10.88 */
        
        
        if (m==CODE_PRE)
            {
            ch=getc(tutor); ungetc(ch,tutor);
            if (ch=='T') tut_special_code=1;
            }
        if (!feof(tutor))
            {

            if (sur_kbhit())
                {
                ch=sur_getch(); // RS CHA nextkey("");
                switch (ch)
                    {
/* 23.1.2001 */   case '+': --tut_wait_c; if (tut_wait_c<1) tut_wait_c=1;  // RS CHA 2 -> 1
                            break;
                  case '-': ++tut_wait_c; break;

                  case '*': if (etu1<0) break;
                            --etu1; if (etu1<=0) etu1=1; break;
                  case '/': if (etu1<0) break;
                            ++etu1; if (etu1>20) etu1=20; break;
                  case '.': etu=0; muste_fclose(tutor); return(0);
                  case CODE_HELP:
                            etu2=2; if (etu1<2) etu1=2; break;

                  default: break; // RS CHA FIXME???while(!sur_kbhit()) ; sur_getch(); break;
                    }
                }


            if (m==TUT_EFFECTS_OFF) { etu2=etu3=0; return(255); }
            if (m==CODE_PRE && tut_special_code) { tut_special(); return(255); }
                            /* lis. 4.10.88  */
            if (etu>1 && etu2!=2) sur_wait((long)tut_wait_c*etu1,nop,0);

            if (etu2>0)
                {
                int rr,cc;

                if (tut_special_code) return(m); /* 23.10.89 ennen etu2==2 && */
                CURSOR_POS(&rr,&cc);
                CURSOR_OFF; LOCATE(r3+2,c3-10);
             if (etu2==1) sur_print("       "); else { PR_EBLK; sur_print(" Press "); }
                PR_EINV; label(m,nimi); sprintf(sbuf,"%s",nimi); sur_print(sbuf);
                LOCATE(rr,cc);
                PR_ENRM; CURSOR_ON;
                if (etu2==1) { if (etu1>1) sur_wait((long)4*(long)(tut_wait_c*etu1),Wdisp,0); }
                else press_key(m);
                CURSOR_OFF; LOCATE(r3+2,c3-10);
             /* Rprintf("%s","               "); */
                sur_print("               ");
                LOCATE(rr,cc);
                CURSOR_ON;
                }
            return(m);
            }
        muste_fclose(tutor);
        etu=0; return(0);
        }
        


static int tut_special()
        {
        int m; // RS REM ,ar,ac,ch,i;
// RS REM        long l;

        char sana[LLENGTH];

        tutch(); /* T */

            m=tutch();
            switch (m)
                {
              case 'W':
                read_tutword(sana);
                if (etu1>1 && etu2!=2)
                    sur_wait((long)(5*tut_wait_c)*atol(sana)*(long)etu1,Wdisp,1);
                   
                else Wdisp();
                break;
/*            case 'L':
                read_tutword(sana);
                muste_fclose(tutor);
                tutopen(sana,"rb");
                break;
*/
              case 's':
                read_tutword(sana);
                if (etu1>1) etu1=atoi(sana);
                break;
              case 't':   /* t1=maksiminopeuteen ja t2=pois */
                read_tutword(sana);
                if (*sana=='1') { if (etu1>0) etu1=-etu1; }
                else { if (etu1<0) etu1=-etu1; }
                break;
              case 'D':
                read_tutword(sana);
                etu2=atoi(sana);
                break;
/*            case 'P':
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
*/
              case 255:
                break;
              case 'M':  /* ÑÑnet puuttuvat */
                read_tutword(sana); break;

              default: sprintf(sbuf,"\nTUT CODE: %d",m); sur_print(sbuf); sur_getch();
                }
        tut_special_code=0;
        return(1);
        }

static int Wdisp()
        {
        int rr,cc;
                                  /* 29.10.89 */
/*      CURSOR_OFF; headline("");  cursor(r,c);  CURSOR_ON;   */
        CURSOR_OFF; CURSOR_POS(&rr,&cc); headline("");  LOCATE(rr,cc);  CURSOR_ON;
                                                        /* 4.6.90 */
        return(1);
        }

