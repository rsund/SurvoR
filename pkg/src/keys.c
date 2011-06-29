#include <R.h>
#include <Rinternals.h>
// #include <Rinterface.h>
#include <R_ext/Riconv.h>
#include <time.h>
#include "survo.h"
#include "kscodes.h"
#include "survolib.h"

extern SEXP Muste_EvalRExpr();

extern int special;
extern int r,r1,r2,r3,c,c1,c2,c3;


/* Table to convert one character to UPPERCASE */
unsigned char uc_cp850[256] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c,
  0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
  0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26,
  0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33,
  0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40,
  0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d,
  0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57, 0x58, 0x59, 0x5a,
  0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 0x60, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
  0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f, 0x50, 0x51, 0x52, 0x53, 0x54,
  0x55, 0x56, 0x57, 0x58, 0x59, 0x5a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x80, 0x9a,
  0x90, 0xb6, 0x8e, 0xb7, 0x8f, 0x80, 0xd2, 0xd3, 0xd4, 0xd8, 0xd7, 0xde, 0x8e,
  0x8f, 0x90, 0x92, 0x92, 0xe2, 0x99, 0xe3, 0xea, 0xeb, 0x3f, 0x99, 0x9a, 0x9d,
  0x9c, 0x9d, 0x9e, 0x3f, 0xb5, 0xd6, 0xe0, 0xe9, 0xa5, 0xa5, 0xa6, 0xa7, 0xa8,
  0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5,
  0xb6, 0xb7, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc1, 0xc2,
  0xc3, 0xc4, 0xc5, 0xc7, 0xc7, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  0xd1, 0xd1, 0xd2, 0xd3, 0xd4, 0x49, 0xd6, 0xd7, 0xd8, 0xd9, 0xda, 0xdb, 0xdc,
  0xdd, 0xde, 0xdf, 0xe0, 0x53, 0xe2, 0xe3, 0xe5, 0xe5, 0x3f, 0xe8, 0xe8, 0xe9,
  0xea, 0xeb, 0xed, 0xed, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6,
  0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
  };

/* Table to convert one character to lowercase */
unsigned char lc_cp850[256] = {
  0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c,
  0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19,
  0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26,
  0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33,
  0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x40,
  0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67, 0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d,
  0x6e, 0x6f, 0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77, 0x78, 0x79, 0x7a,
  0x5b, 0x5c, 0x5d, 0x5e, 0x5f, 0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
  0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f, 0x70, 0x71, 0x72, 0x73, 0x74,
  0x75, 0x76, 0x77, 0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f, 0x87, 0x81,
  0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x84,
  0x86, 0x82, 0x91, 0x91, 0x93, 0x94, 0x95, 0x96, 0x97, 0x98, 0x94, 0x81, 0x9b,
  0x9c, 0x9b, 0x9e, 0x9f, 0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa4, 0xa6, 0xa7, 0xa8,
  0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf, 0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xa0,
  0x83, 0x85, 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf, 0xc0, 0xc1, 0xc2,
  0xc3, 0xc4, 0xc5, 0xc6, 0xc6, 0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
  0xd0, 0xd0, 0x88, 0x89, 0x8a, 0xd5, 0xa1, 0x8c, 0x8b, 0xd9, 0xda, 0xdb, 0xdc,
  0xdd, 0x8d, 0xdf, 0xa2, 0xe1, 0x93, 0x95, 0xe4, 0xe4, 0xe6, 0xe7, 0xe7, 0xa3,
  0x96, 0x97, 0xec, 0xec, 0xee, 0xef, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6,
  0xf7, 0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff
  };

unsigned char latin1_to_cp850[96] = {
0xFF, 0xAD, 0xBD, 0x9C, 0xCF, 0xBE, 0xDD, 0xF5, 0xF9, 0xB8, 0xA6, 0xAE,
0xAA, 0xF0, 0xA9, 0xEE, 0xF8, 0xF1, 0xFD, 0xFC, 0xEF, 0xE6, 0xF4, 0xFA,
0xF7, 0xFB, 0xA7, 0xAF, 0xAC, 0xAB, 0xF3, 0xA8, 0xB7, 0xB5, 0xB6, 0xC7,
0x8E, 0x8F, 0x92, 0x80, 0xD4, 0x90, 0xD2, 0xD3, 0xDE, 0xD6, 0xD7, 0xD8,
0xD1, 0xA5, 0xE3, 0xE0, 0xE2, 0xE5, 0x99, 0x9E, 0x9D, 0xEB, 0xE9, 0xEA,
0x9A, 0xED, 0xE8, 0xE1, 0x85, 0xA0, 0x83, 0xC6, 0x84, 0x86, 0x91, 0x87,
0x8A, 0x82, 0x88, 0x89, 0x8D, 0xA1, 0x8C, 0x8B, 0xD0, 0xA4, 0x95, 0xA2,
0x93, 0xE4, 0x94, 0xF6, 0x9B, 0x97, 0xA3, 0x96, 0x81, 0xEC, 0xE7, 0x98
};


char *muste_strupr(char *str)
{
    char *string = str;

    if (str)
    {
        for ( ; *str; ++str)
          *str = uc_cp850[(unsigned char)*str]; // RS CHA *str = toupper(*str);
    }
    return string;
}

char *muste_strlwr(char *str)
{
    char *string = str;

    if (str)
    {
        for ( ; *str; ++str)
          *str = lc_cp850[(unsigned char)*str];
    }
    return string;
}



int muste_iconv(char *teksti,char *to,char *from) 
{
// RS unused    static wchar_t kohde[LLENGTH+1];

    char y[3*LLENGTH];
    const char *inbuf;
    char *outbuf;
    long inb, outb, res; // RS CHA    size_t inb, outb, res;

    void *obj;


    strcpy(y,teksti); 
    obj = Riconv_open(to,from);
//    if(obj == (void *)(-1)) error("Unsupported conversion!");

    inbuf=y;
    inb = strlen(inbuf)+1; outb = 2*LLENGTH+1;
    outbuf = (char *) teksti;
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    Riconv_close(obj);
//    if(res == -1) error("Conversion problem");
//    if(inb > 0) error("Conversion problem -- too long?");


/*
   int luuppi=0;
   while (jakso[luuppi]!='\0') { 
       if ((unsigned char)jakso[luuppi]>127) jakso[luuppi]='?'; 
       luuppi++;
   }
*/
   return(1);
}

/*
#include <R_ext/Riconv.h>

    void *cd = NULL;
    size_t  i_len, o_len, status;
    char buf[256];
    const char *i_buf;
    char *o_buf;

    strcpy(buf,jakso);
    i_buf=(char *)buf;
    o_buf=(char *)jakso;
    cd = Riconv_open("", "CP850");
    o_len = i_len = strlen(i_buf);
    status = Riconv(cd, &i_buf, (size_t *)&i_len, &o_buf, (size_t *)&o_len);

    Riconv_close(cd);

******************

    static wchar_t filename[PATH_MAX+1];

    const char *from = "", *inbuf;
    char *outbuf;
    size_t inb, outb, res;

    void *obj;

    obj = Riconv_open("UCS-2LE", from);
    if(obj == (void *)(-1))
	error("unsupported conversion from '%s' in 'filenameToWchar' in codepage %d", 
	      from, localeCP);

    if(expand) inbuf = R_ExpandFileName(CHAR(fn)); else inbuf = CHAR(fn);

    inb = strlen(inbuf)+1; outb = 2*PATH_MAX+1;
    outbuf = (char *) filename;
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    Riconv_close(obj);
    if(res == -1) error(_("file name conversion problem"));
    if(inb > 0) error(_("file name conversion problem -- name too long?"));

*/    



void muste_sleep(int time)
    {
    char buf[32];
    sprintf(buf,"after %d",time);
    Muste_EvalTcl("update idletasks",FALSE);
    Muste_EvalTcl("update",FALSE);
    R_FlushConsole();
    R_ProcessEvents();    
    Muste_EvalTcl(buf,FALSE);      
    }

int sur_sleep(int time)
    {
    muste_sleep(time);
    return(1);
    }

SEXP muste_eventtimesexp;
SEXP muste_eventtypesexp;
SEXP muste_keycharsexp;
SEXP muste_keykeysymsexp;
SEXP muste_mousesexp;


int muste_eventpeek=FALSE;
int muste_eventtime=0;
int muste_eventtype=0;
unsigned int muste_char;

static  int rr,cc;
int m_double_click;
static int m_click;
static int right_mouse_click=0;
int only_key_events=0;
int c_mouse, r_mouse;


#define KEY_EVENT 1
#define MOUSE_EVENT 2
#define SPECIAL_KEY_EVENT 3

int muste_peekinputevent(int readevent)
   {

   int newtime;
   int mousebutton,mousedouble;

   unsigned char merkki;
// RS unused   unsigned int ch;
   char keybuffer[16];

   muste_eventtimesexp=R_NilValue;
   muste_eventtypesexp=R_NilValue;
   muste_keycharsexp=R_NilValue;
   muste_keykeysymsexp=R_NilValue;
   muste_mousesexp=R_NilValue;
	
   muste_eventtimesexp = findVar(install(".muste.event.time"),R_GlobalEnv);
   newtime = INTEGER(muste_eventtimesexp)[0];
   if (newtime==muste_eventtime) return(FALSE);

   if (readevent) muste_eventtime=newtime;

   muste_eventtypesexp = findVar(install(".muste.event.type"),R_GlobalEnv);
   muste_eventtype = INTEGER(muste_eventtypesexp)[0];

   switch (muste_eventtype)
      {
      case KEY_EVENT: 
     
         muste_keycharsexp = findVar(install(".muste.key.char"),R_GlobalEnv);
         strcpy(keybuffer,CHAR(STRING_ELT(muste_keycharsexp,0)));
         
         merkki=(unsigned char)keybuffer[0];
         muste_char=merkki;
         if (strlen(keybuffer)>0) break;

      case SPECIAL_KEY_EVENT:
         muste_keykeysymsexp = findVar(install(".muste.key.keysym"),R_GlobalEnv);
         muste_char = INTEGER(muste_keykeysymsexp)[0];
         muste_eventtype=KEY_EVENT;
         break;

      case MOUSE_EVENT:
         muste_char=0;
         muste_mousesexp = findVar(install(".muste.mouse.col"),R_GlobalEnv);
         cc = (INTEGER(muste_mousesexp)[0])-7;
         muste_mousesexp = findVar(install(".muste.mouse.row"),R_GlobalEnv);
         rr = (INTEGER(muste_mousesexp)[0])-1;
         muste_mousesexp = findVar(install(".muste.mouse.button"),R_GlobalEnv);
         mousebutton = INTEGER(muste_mousesexp)[0];
         m_click=FALSE;
         if (mousebutton==1 || mousebutton==3) m_click=TRUE;
         if (mousebutton==3) right_mouse_click=TRUE;
         muste_mousesexp = findVar(install(".muste.mouse.double"),R_GlobalEnv);
         mousedouble = INTEGER(muste_mousesexp)[0];
         m_double_click=FALSE;
         if (m_click && mousedouble==1) m_double_click=TRUE;

      }
//Rprintf("muste_char: %d\n",muste_char);
   return(TRUE);
}


int sur_event()
	{
	Rprintf("FIXME: sur_event() not implemented\n");
	return 1;
	} 

int sur_flush_input()
	{
	Rprintf("FIXME: sur_flush_input() not implemented\n");
	return 1;
	} 


int sur_kbhit() // RS Painettu näppäintä
    {
/* RS CHA
    PeekConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);
    if (dwInputEvents)
        {
        if (inputBuffer.EventType==KEY_EVENT &&
            inputBuffer.Event.KeyEvent.bKeyDown)
                return(1);
        sur_flush_input();
        }
*/
    if (muste_peekinputevent(FALSE))
      {
      if (muste_eventtype==KEY_EVENT) return(TRUE);
      }
    return(FALSE);
    }

int s_hit(unsigned int c) // RS Onko painettu haluttua näppäintä
    {
    if (!sur_kbhit()) return(FALSE);
    muste_peekinputevent(TRUE);
    if ((char)muste_char==(char)c) return(TRUE);
/* RS CHA
    ReadConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);
    if(inputBuffer.Event.KeyEvent.uChar.AsciiChar==(char)c)
        return(1);
*/
    return(FALSE);
    }

int sur_mkbhit() // 20.11.2000 // RS Painettu näppäintä tai liikutettu/painettu hiirtä
    {
/* RS CHA
    PeekConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);
    if (dwInputEvents)
        {
        if (inputBuffer.EventType==KEY_EVENT &&
            inputBuffer.Event.KeyEvent.bKeyDown)
                return(1);
        if (inputBuffer.EventType==MOUSE_EVENT)
                return(1);
        sur_flush_input();
        }
*/
    if (muste_peekinputevent(TRUE)) return(1);
    return(FALSE);
    }

int sur_m2kbhit() // 31.12.2000 on key (sucros) // RS Näppäimen tai hiiren painallus
    {
/* RS CHA
    PeekConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);
    if (dwInputEvents)
        {
        if (inputBuffer.EventType==KEY_EVENT &&
            inputBuffer.Event.KeyEvent.bKeyDown)
                return(1);
        if (inputBuffer.EventType==MOUSE_EVENT &&
            inputBuffer.Event.MouseEvent.dwEventFlags == 0 &&
            inputBuffer.Event.MouseEvent.dwButtonState)
                return(2);
        sur_flush_input();
        }
*/
    if (muste_peekinputevent(FALSE))
      {
      if (muste_eventtype==KEY_EVENT) return(1);
      if (muste_eventtype==MOUSE_EVENT && m_click) return(2);
      }
    return(FALSE);
    }


int getck2(int mouse) // 1=mouse click accepted 0=not
    {
    int m;
    while (1)
      {
      m=muste_peekinputevent(TRUE);
      if (!m)
        {
        muste_sleep(10);
        continue;
        }
      if (muste_eventtype==KEY_EVENT)
        {
        m=muste_char;
        if (m==-1) continue;
        break;
        }
      if (mouse)
        {
        if (muste_eventtype==MOUSE_EVENT && m_click)
          {
          if (mouse==2) m=-2;
          else m=' ';
          break;
          }
        }
      }
    return(m);
    }

int getck() { muste_sleep(500); return(getck2(0)); }
int getcm() { muste_sleep(500); return(getck2(1)); }
int sur_getch() { return(getck2(0)); }




// RS extern int soft_code;
// RS extern char soft_char;

extern long wait_tut_time;
extern int wait_tut_type;
extern int soft_multiple;
extern int soft_multiple2;
extern int etu;
extern int rajoitettu_vastausaika;
extern time_t vastauksen_alkuhetki,max_vastausaika,aika_save;
extern int insert_mode;
extern int autosave;
extern int autosavefield;
extern int sucro_menu;
extern int soft_message;
extern int r_soft;
extern int soft_code;
extern char soft_char;


int nextkey2()
        {
        int ch,m,aika1,no_key;
        time_t aika2,aika3;
        time_t time1,time2;
        int i;
        char s[8];
        int jo_talletettu;
        int erotus;
        static int loading_help_lines=0;
        extern int nop();

        aika1=0;
        time(&aika2);
        time1=aika2;
        
        headline();

        while (1) // 16.2.1997
            {
// RS NYI            if (key_sleep) sur_sleep(key_sleep);

            if (muste_eventpeek==FALSE) muste_sleep(10); // RS oli Windowsin oma Sleep(10)
//          continue;    // poistoyritys 20.11.2001


            time(&time2);
            if (difftime(time2,time1)>0.5)
                {
                headline();


                if (wait_tut_type) // 14.2.2001
                    {
                    if (time2>=wait_tut_time)
                        return(-7);
                    }


                time1=time2;
 
                soft_multiple=soft_multiple2;

                time(&aika3);
                erotus=aika3-aika2;
                if (!etu && !rajoitettu_vastausaika && erotus>3
                       && autosave && aika3-aika_save>60*autosave)
                    {
// Rprintf("SURVOEDT!,erotus: %d,a-a:%d\n",erotus,aika3-aika_save); getck();
                    autosavefield=1;
                    edt_talletus(SURVOEDT);
                    autosavefield=0;
                    aika_save=aika3;
                    aika2=aika3;
                    }

                }


/* RS NYI
            if (sur_get_message(sbuf,2))
                {
                if (strcmp(sbuf,"-")==0) help_window_open=0;
                else if (strcmp(sbuf,"NEW")==0)
                    {
                    import_lines(loading_help_lines);
                    loading_help_lines=1;
                    }
                }

            sur_get_error_message();
*/
            if (rajoitettu_vastausaika)
                {
                time(&aika3);
                if (difftime(aika3,vastauksen_alkuhetki)>max_vastausaika)
                    {
                    m=CODE_RETURN; special=1; return(m);
                    }
                }


// Rprintf("nextkey2 while %d\n",difftime(time2,time1));
            if (muste_peekinputevent(TRUE)) break;  // RS
            if (muste_eventpeek==TRUE) { /* muste_eventpeek=FALSE; */ return(-5); }
////            PeekConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);
////            if (dwInputEvents) break;

            ++aika1;

            }

//return(0); // RS väliaikainen lisäys

/* RS NYI
    if (only_key_events)
        {

        if (inputBuffer.EventType!=KEY_EVENT)
          {
          ReadConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);
          sur_sleep(10);
          return(-2);
          }
        }
*/

////    ReadConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);

/* RS NYI
    if (soft_key_activated && soft_multiple)
        {
        if (inputBuffer.EventType==MOUSE_EVENT &&
            inputBuffer.Event.MouseEvent.dwButtonState)
            {
            sur_flush_input();
            i=soft_key_activate(rr,cc,m_click,m_double_click);
            if (i==2)
                {
                special=1;
                return(CODE_EXEC);
                }
            return(-1);
            }
        else soft_key_activated=0;
        }
*/

        switch (muste_eventtype)
          {
          case KEY_EVENT:
          
muste_eventpeek=TRUE;

/* RS REM ??
            if (inputBuffer.Event.KeyEvent.bKeyDown)
              {

*/
              if (wait_tut_type!=2) wait_tut_type=0; // 14.2.2001
              if (insert_mode) CURSOR_INS; else CURSOR_ON;

              special=FALSE;
              m=ch=muste_char;  // RS Aikaisemmin vain muuttujaan m

/* RS NYI
              if (m==9999) // EURO altGr e     28.1.2002
                  {
                  PR_EBLK;
                  sur_print("\nEuro character in Survo is e with shadow E.");
                  WAIT; PR_ENRM; disp(); m='e';
                  }

              loading_help_lines=0;
*/


/* RS ORG
              switch (m)
                  {
                case EXTEND_CH: m=sur_getch();
                                special=1;
                        switch (m)
                            {
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
                          case  KEY_MOVE:      m=CODE_MOVE; break;
                          case  KEY_END:       m=CODE_END; break;
                          case  KEY_WORDS:     m=CODE_WORDS; break;
                          case  KEY_RIGHT2:    m=CODE_RIGHT2; break;
                          case  KEY_LEFT2:     m=CODE_LEFT2; break;
                          case  KEY_UP2:       m=CODE_UP2; break;
                          case  KEY_DOWN2:     m=CODE_DOWN2; break;
                          case  KEY_SUCRO1:    m=CODE_SUCRO1; break; // 3.9.1995
                          case  KEY_SUCRO2:    m=CODE_SUCRO2; break;
                          case  KEY_SUCRO3:    m=CODE_SUCRO3; break;
                          case  KEY_SUCRO4:    m=CODE_SUCRO4; break;
                          case  KEY_SUCRO5:    m=CODE_SUCRO5; break;
                          case  KEY_SUCRO6:    m=CODE_SUCRO6; break;
                          case  KEY_SUCRO7:    m=CODE_SUCRO7; break;
                          case  KEY_SUCRO8:    m=CODE_SUCRO8; break;
                          case  KEY_SOFT_ON:   m=CODE_SOFT_ON; break;
                          case  KEY_REF_SET:   m=CODE_REF_SET; break;

                          case  151: m=151; break;
                          case  153: m=153; break;


                          default: // printf("\nSPECIAL: %d\n",m); getch();
                                   m=32;
                            }
                          break;

                case KEY_RETURN: special=1; m=CODE_RETURN; break;
                case KEY_BACKSP: special=1; m=CODE_BACKSP; break;
                case KEY_EXEC: special=1; m=CODE_EXEC; break;
                case KEY_TAB: special=1; m=CODE_TAB; break;

                default: ; // mahd.dekoodaus
                  }
*/
      if (ch>31 && ch<256 && ch!=127) return(ch);
 
      special=TRUE;

//muste_eventpeek=FALSE;


      switch (ch)
         {
         case KEY_EXEC:
         case KS_Escape:      ch=CODE_EXEC; muste_eventpeek=FALSE; break;
         case KSM_Control_r:
         case KSM_Control_R:  ch=CODE_REXEC; muste_eventpeek=FALSE; break;
         case KEY_RETURN:
         case KS_Return:      ch=CODE_RETURN; break;
         case KS_F1:          ch=CODE_HELP; break;
         case KS_F2:          ch=CODE_PRE; muste_eventpeek=FALSE; break;
         case KS_F3:          ch=CODE_TOUCH; break;
         case KS_F4:          ch=CODE_DISK; break;
         case KS_F5:          ch=CODE_DISP; break;
         case KS_F6:          ch=CODE_MERGE; break;
         case KS_F7:          ch=CODE_REF; break;
         case KS_F8:          ch=CODE_EXIT; muste_eventpeek=FALSE; break;
         case KS_F9:          ch=CODE_INSERT; break;
         case KS_F10:         ch=CODE_DELETE; break;
         case KSM_1:
         case KSM_F1:         ch=CODE_SOFT_ON; break; 
         case KSM_2:
         case KSM_F2:         ch=CODE_WORDS; break;
         case KSM_3:
         case KSM_F3:         ch=CODE_COPY; muste_eventpeek=FALSE; break;
         case KSM_4:
         case KSM_F4:         ch=CODE_MOVE; break;
         case KSM_5:
         case KSM_F5:         ch=CODE_SRCH; break;
         case KSM_6:
         case KSM_F6:         ch=CODE_ACTIV; break;
         case KSM_7:
         case KSM_F7:         ch=CODE_CODE; break;
// RS NYI         case KS_CtrlF7:      ch=CODE_REF_SET; break;         
         case KSM_8:
         case KSM_F8:         ch=CODE_EXIT; muste_eventpeek=FALSE; break;  // RS  jotain muuta?
         case KSM_9:
         case KSM_F9:         ch=CODE_INSERTL; break;
         case KSM_0:
         case KSM_F10:        ch=CODE_DELETEL; break;
         case KEY_TAB:
         case KS_Tab:         ch=CODE_TAB; break;
         case KS_Insert:      ch=CODE_INSERT; break;
         case KEY_BACKSP:
         case KS_BackSpace:   ch=CODE_BACKSP; break;
         case KEY_DEL:
         case KS_Delete:      ch=CODE_DELETE; break;
         case KS_End:         ch=CODE_END; break;
         case KS_Home:        ch=CODE_HOME; break;
         case KS_Prior:       ch=CODE_PREV; break;
         case KS_Next:        ch=CODE_NEXT; break;
         case KS_Left:        ch=CODE_LEFT; break; 
         case KS_Right:       ch=CODE_RIGHT; break; 
         case KS_Down:        ch=CODE_DOWN; break; 
         case KS_Up:          ch=CODE_UP; break;
         case KSM_End:        ch=CODE_ERASE; break;
         case KSM_Right:      ch=CODE_RIGHT2; break;
         case KSM_Left:       ch=CODE_LEFT2; break;
         case KSM_Up:         ch=CODE_UP2; break;
         case KSM_Down:       ch=CODE_DOWN2; break;

         default:
            ch=-1;
//            muste_eventpeek=TRUE;
            break;
          }
 
          return(ch);
          break;

          case MOUSE_EVENT:

           muste_sleep(10); // RS ADD
           
           if (muste_eventpeek==FALSE) return(-1); // RS ADD 15.8.2010

/* RS jo peekissä
            m_double_click=0; m_click=0;

     
            if (inputBuffer.Event.MouseEvent.dwButtonState==RIGHTMOST_BUTTON_PRESSED)
            right_mouse_click=1; else right_mouse_click=0;

            if (inputBuffer.Event.MouseEvent.dwEventFlags==DOUBLE_CLICK)
                m_double_click=1;


            if (inputBuffer.Event.MouseEvent.dwEventFlags == 0 &&
                inputBuffer.Event.MouseEvent.dwButtonState)
                m_click=1;

*/
            if (m_double_click && right_mouse_click)
                    {
// RS NYI                  help("???"); return(-1);
                    }

            if (!m_double_click && m_click)  // 14.10.2005
                {
                if (right_mouse_click) i=2; else i=1;
/* RS NYI
                if (act_sounds_on==2)
                    {
                    char nimi[100];

                    sprintf(nimi,"%sSND\\%s.WAV",survo_path,act_sound[i]);
                    sur_play_sound(nimi);
                    }
*/
                }


            if (m_double_click || m_click) if (wait_tut_type!=2) wait_tut_type=0; // 14.2.2001


/* RS jo peekissä
            cc=inputBuffer.Event.MouseEvent.dwMousePosition.X-7;
            rr=inputBuffer.Event.MouseEvent.dwMousePosition.Y;
*/


            c_mouse=cc+7;
            r_mouse=rr;


            if (sucro_menu && m_click) return(-2);

            if (rr<=r3+1 && soft_message)
               {
               soft_message=0; soft_bottom_line_erase();
               }


            if (rr==r3+1 && m_click)
                {

/* RS NYI
                if (m_move_ind2 || m_move_ind) // 23.3.2004
                    {
                    if (c_mouse>=48 && c_mouse<=59)
                        { *help_sana=EOS; help("MOUSE6"); return(-1); }
                    move_clear();
                    m_move_ind=m_move_ind2=0;
                    }
*/
                if (!r_soft)
                    restore_softkeys();
                else
                    soft_disp(1);
                return(-1);
                }




            if (cc>0 && cc<=c3 && rr>0 && rr<=r3)
                {
                if (soft_message) soft_bottom_line_erase();
                if (m_double_click)
                  {
                  c=cc;
                  r=rr;
                  special=1;
//                  muste_eventpeek=FALSE; // RS ADD  
                  return(CODE_EXEC);
                  break;
                  }

                if (m_click)
                  {
                  c=cc;
                  r=rr;
//                  muste_eventpeek=TRUE; // RS ADD

                  // 21.3.2004
                  if (right_mouse_click) 
                    { 
// RS NYI                    mouse_define_block(); cursor(r,c);
                    }
                  else
                      disp();
                  }
                }


            else
                {
                
//                muste_eventpeek=TRUE; // RS ADD 15.8.2010
                i=soft_key_activate(rr,cc,m_click,m_double_click);
                                
                if (i==2)
                    {
                                        
                    special=1;
//                    muste_eventpeek=FALSE; // RS ADD                                       
                    return(CODE_EXEC);
                    }
                if (i==3)
                    {
                    special=1;
//                    muste_eventpeek=FALSE; // RS ADD
                    return(soft_code);
                    }
                if (i==4)
                    {
                    special=0;
                    return((int)soft_char);
                    }
                }

          default:
//            muste_eventpeek=TRUE; // RS ADD
            break;
            }

        return(-1);
        }



int nextkey()
        {
        int m;

        while (1)
            {
//Rprintf("nextkey while\n");
            m=nextkey2();
            if (m!=-1) return(m);
            }
        }


extern int etu;
extern int wait_save;

static int nextch_common()
        {
        int m;
        
        if (etu==2)
            {            
            m=tutch();
            while (m==255 && etu==2) m=tutch();
            if (m!=0) return(m);
            }
        if (etu==1)
            {
            m=nextkey();
//          cursor(2,50); sprintf(sbuf,"%d  ",m); sur_print(sbuf); getck();
//          cursor(r,c);
            if (wait_save) save_wait(m);
            tutsave(m);
            return(m);
            }

//Rprintf("entering nextch\n");
        m=nextkey();
                
muste_eventpeek=FALSE;        
        return(m);
        }


int nextch()
        {
        int m;
        muste_eventpeek=FALSE;
        m=nextch_common();
        return(m);
        }

int nextch_eventloop()
        {
        int m;
        muste_eventpeek=TRUE;
        m=nextch_common();
        return(m);
        }

