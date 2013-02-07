#include "muste.h"
#include <R.h>
#include <Rinternals.h>
// #include <Rinterface.h>
#include <R_ext/Riconv.h>
#include <time.h>
#include <sys/timeb.h>
#include "survo.h"
#include "kscodes.h"
#include "survolib.h"

extern SEXP Muste_EvalRExpr();
extern void tutsave();
extern int muste_get_R_char_noencode();

extern int special;
extern int r,r1,r2,r3,c,c1,c2,c3;
extern int key_sleep;

extern int muste_mousewheel;
extern int muste_eventlooprunning;
extern int muste_no_selection;

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

static unsigned char iso[]=
{ '\0', '\1', '\2', '\3', '\4', '\5', '\6', '\7', '\10', '\11',
'\12', '\13', '\14', '\15', '\16', '\17', '\20', '\21', '\22',
'\23', '\24', '\25', '\26', '\27', '\30', '\31', '\32', '\33',
'\34', '\35', '\36', '\37', '\40', '\41', '\42', '\43', '\44',
'\45', '\46', '\47', '\50', '\51', '\52', '\53', '\54', '\55',
'\56', '\57', '\60', '\61', '\62', '\63', '\64', '\65', '\66',
'\67', '\70', '\71', '\72', '\73', '\74', '\75', '\76', '\77',
'\100', '\101', '\102', '\103', '\104', '\105', '\106', '\107',
'\110', '\111', '\112', '\113', '\114', '\115', '\116', '\117',
'\120', '\121', '\122', '\123', '\124', '\125', '\126', '\127',
'\130', '\131', '\132', '\133', '\134', '\135', '\136', '\137',
'\140', '\101', '\102', '\103', '\104', '\105', '\106', '\107',
'\110', '\111', '\112', '\113', '\114', '\115', '\116', '\117',
'\120', '\121', '\122', '\123', '\124', '\125', '\126', '\127',
'\130', '\131', '\132', '\173', '\174', '\175', '\176', '\177',
'\200', '\232', '\202', '\203', '\216', '\205', '\217', '\207',
'\210', '\211', '\212', '\213', '\214', '\215', '\216', '\217',
'\220', '\221', '\222', '\223', '\231', '\225', '\226', '\227',
'\230', '\231', '\232', '\233', '\234', '\235', '\236', '\237',
'\240', '\241', '\242', '\243', '\244', '\245', '\246', '\247',
'\250', '\251', '\252', '\253', '\254', '\255', '\256', '\257',
'\260', '\261', '\262', '\263', '\264', '\265', '\266', '\267',
'\270', '\271', '\272', '\273', '\274', '\275', '\276', '\277',
'\300', '\301', '\302', '\303', '\304', '\305', '\306', '\307',
'\310', '\311', '\312', '\313', '\314', '\315', '\316', '\317',
'\320', '\321', '\322', '\323', '\324', '\325', '\326', '\327',
'\330', '\331', '\332', '\333', '\334', '\335', '\336', '\337',
'\340', '\341', '\342', '\343', '\344', '\345', '\346', '\347',
'\350', '\351', '\352', '\353', '\354', '\355', '\356', '\357',
'\360', '\361', '\362', '\363', '\364', '\365', '\366', '\367',
'\370', '\371', '\372', '\373', '\374', '\375', '\376', '\377' };

char *struprf(char *s) /* as strupr() but observes ÜÑî... */
        {
        char *p;
        p=s;
        while (*p!=FALSE) { *p=iso[(int)*p]; p++; }
        return(s);
        }
        
char *strnuprf(char *s,int len)
        {
        int i;
        for (i=0; i<len; ++i) s[i]=iso[(int)s[i]];
        return(s);
        }



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

    char *y;
    const char *inbuf;
    char *outbuf;
    size_t inb, outb; // , res; // RS CHA    size_t inb, outb, res; unsigned long
	int len;

    void *obj;
 
	len=strlen(teksti);
	if (len<1) return(-1); // RS ADD 6.11.2012 
    y=(char *)malloc(len+2); 
    if (y==NULL) return(-1); // RS ADD 4.10.2012

    strcpy(y,teksti); 
//	pit=strlen(y);

    obj = Riconv_open(to,from);
//    if(obj == (void *)(-1)) error("Unsupported conversion!");

    inbuf=y;
    inb = len+1; outb = 2*len+2; // 2*LLENGTH+1;
    outbuf=(char *)teksti;

    Riconv(obj, (const char **)&inbuf, &inb, &outbuf, &outb); // RS 4.2.2013 REM res=
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
   free(y);
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

void muste_refreshinput()
	{
    Muste_EvalTcl("update",FALSE);	
	}

int muste_sleep(int time)
    {
    extern int muste_emergency_stop;
    char buf[32];
    
    muste_mousewheel=FALSE;
//    sprintf(buf,"after %d",time);
    sprintf(buf,".muste.sleep(%f)",(double)time/1000);
    Muste_EvalTcl("update idletasks",FALSE);
    Muste_EvalTcl("update",FALSE);
    R_FlushConsole();
    R_CheckUserInterrupt(); // RS CHA R_ProcessEvents();    
//    Muste_EvalTcl(buf,FALSE);
	muste_evalr(buf);
	if (muste_emergency_stop)
		{
		muste_emergency_stop=0;
		return(-1);
		}
    muste_mousewheel=TRUE;
    return(1);
    }

int sur_sleep(int time)
    { 
    return(muste_sleep(time));
    }

SEXP muste_eventtimesexp;
SEXP muste_eventtypesexp;
SEXP muste_keycharsexp;
SEXP muste_keykeysymsexp;
SEXP muste_mousesexp;
SEXP muste_keystatusexp;

int muste_keystatus=0;
int muste_eventpeek=FALSE;
int muste_eventtime=0;
int muste_eventtype=0;
unsigned int muste_char;

static  int rr,cc;
int m_double_click;
int m_click;
int right_mouse_click=0;
int only_key_events=0;
int c_mouse, r_mouse;

int sur_ctrl;

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
   muste_keystatusexp=R_NilValue;
	
//   muste_eventtimesexp = findVar(install(".muste$event.time"),R_GlobalEnv);
//   newtime = INTEGER(muste_eventtimesexp)[0];
   
   newtime=muste_get_R_int(".muste$event.time");
   if (newtime==muste_eventtime) return(FALSE);

   if (readevent) muste_eventtime=newtime;

//   muste_eventtypesexp = findVar(install(".muste$event.type"),R_GlobalEnv);
//   muste_eventtype = INTEGER(muste_eventtypesexp)[0];
   muste_eventtype = muste_get_R_int(".muste$event.type");

   if (muste_eventtype == 1 || muste_eventtype == 3)
     {
//     muste_keystatusexp = findVar(install(".muste$key.status"),R_GlobalEnv);
//     muste_keystatus = INTEGER(muste_keystatusexp)[0];
     muste_keystatus = muste_get_R_int(".muste$key.status");

     sur_ctrl=0;
     if (muste_keystatus==4) sur_ctrl=1;
//Rprintf("\nkeystatus: %d, crtl: %d",muste_keystatus, sur_ctrl);
     }

   switch (muste_eventtype)
      {
      case KEY_EVENT: 
     
//         muste_keycharsexp = findVar(install(".muste$key.char"),R_GlobalEnv);
//         strcpy(keybuffer,CHAR(STRING_ELT(muste_keycharsexp,0)));

//          muste_get_R_string(keybuffer,".muste$key.char",64);
		  muste_get_R_char_noencode(keybuffer,".muste$key.char",64); // RS 27.12.2012         
         
         merkki=(unsigned char)keybuffer[0];
         muste_char=merkki;
         if (strlen(keybuffer)>0 && muste_char!='?') break;

      case SPECIAL_KEY_EVENT:
//         muste_keykeysymsexp = findVar(install(".muste$key.keysym"),R_GlobalEnv);
//         muste_char = INTEGER(muste_keykeysymsexp)[0];
         muste_char = muste_get_R_int(".muste$key.keysym");
         
         muste_eventtype=KEY_EVENT;
         break;

      case MOUSE_EVENT:
         muste_char=0;
//         muste_mousesexp = findVar(install(".muste$mouse.col"),R_GlobalEnv);
//         cc = (INTEGER(muste_mousesexp)[0])-7;
         cc = muste_get_R_int(".muste$mouse.col")-7;         
         
//         muste_mousesexp = findVar(install(".muste$mouse.row"),R_GlobalEnv);
//         rr = (INTEGER(muste_mousesexp)[0])-1;
         rr = muste_get_R_int(".muste$mouse.row")-1; 
         
//         muste_mousesexp = findVar(install(".muste$mouse.button"),R_GlobalEnv);
//         mousebutton = INTEGER(muste_mousesexp)[0];
         mousebutton = muste_get_R_int(".muste$mouse.button");          
         
         m_click=FALSE; right_mouse_click=FALSE;
         if (mousebutton==1 || mousebutton==3) m_click=TRUE;
         if (mousebutton==3) right_mouse_click=TRUE;
//         muste_mousesexp = findVar(install(".muste$mouse.double"),R_GlobalEnv);
//         mousedouble = INTEGER(muste_mousesexp)[0];
         mousedouble = muste_get_R_int(".muste$mouse.double");          
         
         m_double_click=FALSE;
         if (m_click && mousedouble==1) m_double_click=TRUE;

      }
//Rprintf("muste_char: %d\n",muste_char);
   return(TRUE);
}


int sur_event()
	{
	muste_fixme("FIXME: sur_event() not implemented\n");
	return 1;
	} 

static time_t kbhit_time=0;

int sur_flush_input()
	{
	kbhit_time=0;
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

        time_t intime;

        time(&intime);        
        if (kbhit_time==0 || difftime(intime,kbhit_time)>0.1)
                {
                muste_refreshinput();
                kbhit_time=intime;
                }

    
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


int getck2(int mouse,int max) // 1=mouse click accepted 0=not
    {
    int m;
    
    muste_no_selection=TRUE;
    muste_mousewheel=FALSE;
    
    while (1)
      {
      m=muste_peekinputevent(TRUE);             
      if (m)
        {
		if (muste_eventtype==KEY_EVENT)
		  {
		  m=muste_char;
//Rprintf("\nm: %d",m);		   
		  if (m<=0 || m>max) continue; // RS CHA m==-1 -> m<=0  23.11.2012 || m>max
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
      muste_sleep(100); // RS 22.11.2012  
      }  
    muste_mousewheel=TRUE;
    muste_no_selection=FALSE;      
    return(m);
    }

int getck() { muste_sleep(500); return(getck2(0,255)); }
int getcm() { muste_sleep(500); return(getck2(1,255)); }

int s_caps_on()
    {
    muste_peekinputevent(FALSE);
// Rprintf("\nkeystatus: %d, bit: %d",muste_keystatus,(muste_keystatus & 2));    
    if ((muste_keystatus & 2)==2) return(TRUE);
    return(FALSE);
/* RS REM   
    DWORD state;

    PeekConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);
    state=inputBuffer.Event.KeyEvent.dwControlKeyState;
    return(state & CAPSLOCK_ON);
*/    
    }



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


extern void headline_editor();

static double ticktimecount1,ticktimecount2,ticktimestart; /* 7.2.1999 */

int muste_GetTickCount_start(int start)
        {
        struct timeb count;

        ftime(&count);
        ticktimecount2=count.time+0.001*(double)count.millitm;
        if (start) ticktimecount1=ticktimecount2; ticktimestart=0.0;
   		return((int)((ticktimestart+ticktimecount2-ticktimecount1)*1000));
        }

int muste_GetTickCount()
		{
		return(muste_GetTickCount_start(0));
		}
        
unsigned int ptime1,ptime2; // 15.3.2012 
int muste_emacs=FALSE; // RS ADD

static int nextkey2()
        {
        int ch,m,aika1;
// RS REM        int no_key;
        time_t aika2,aika3;
        time_t time1,time2;         
        int i;
// RS REM        char s[8];
// RS REM        int jo_talletettu;
        int erotus;
// RS REM        static int loading_help_lines=0;
        extern int nop();

extern int survopoint_on;
extern int survopoint_disp; 
extern int dispoint();

//		muste_emacs=FALSE; // RS ADD
		
        aika1=0;
        time(&aika2);
        time1=aika2;
        
        headline_editor();
        if (survopoint_on) 
        	{ 
        	ptime1=muste_GetTickCount(); 
        	if (muste_eventpeek==TRUE) dispoint(); 
        	} // 15.3.2012        
    
        while (1) // 16.2.1997
            {
            if (key_sleep) sur_sleep(key_sleep);

            if (muste_eventpeek==FALSE) muste_sleep(10); // RS oli Windowsin oma Sleep(10)
//          continue;    // poistoyritys 20.11.2001


            time(&time2);
        	if (survopoint_on) { ptime2=muste_GetTickCount(); } // 15.3.2012

            if ( (!survopoint_on && difftime(time2,time1)>0.5)
       			|| (survopoint_on && ptime2-ptime1>survopoint_disp) ) // 15.3.2012
                {
                if (survopoint_on) 
                	{ 
                	ptime1=ptime2; 
                	dispoint(); 
                	} // 18.3.2012
                else headline_editor();
            

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


                          default: // Rprintf("\nSPECIAL: %d\n",m); getch();
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
// Rprintf("\nkey: %d",ch);
      switch (ch)
         {
         case KEY_EXEC:
         case KS_Escape:      ch=CODE_EXEC; muste_eventpeek=FALSE; break;
         case KSM_Control_r:
         case KSM_Control_R:  ch=CODE_REXEC; muste_eventpeek=FALSE; break;
         case KSM_Control_v:
         case KSM_Control_V:  
         case KSM_Shift_Insert:
         					  ch=153; break;   
 		 case KSM_Control_c:
         case KSM_Control_C:  
         case KSM_Control_Insert:
         					  ch=151; break; 
 		 case KSM_Control_z:
         case KSM_Control_Z:  
         					  ch=CODE_UNDO; break;            					  
         case KSM_Control_d:
         case KSM_Control_D: muste_emacs='D'; ch=CODE_HELP; break;  
         case KSM_Control_a: 
         case KSM_Control_A: muste_emacs='A'; ch=CODE_HELP; break;  
         case KSM_Control_k:
         case KSM_Control_K: muste_emacs='K'; ch=CODE_HELP; break;  
         case KSM_Control_o:
         case KSM_Control_O: muste_emacs='O'; ch=CODE_HELP; break;
         case KSM_Control_s:
         case KSM_Control_S: muste_emacs='S'; ch=CODE_HELP; break;
         case KSM_Shift_Return: muste_emacs='^'; ch=CODE_HELP; break;
         case KSM_Shift_BackSpace: muste_emacs='<'; ch=CODE_HELP; break;
         case KSM_Control_x:
         case KSM_Control_X: muste_emacs='X'; ch=CODE_HELP; break;  
         case KSM_Shift_Control_x:
         case KSM_Shift_Control_X: muste_emacs='x'; ch=CODE_HELP; break;
         case KSM_Shift_Control_v:
         case KSM_Shift_Control_V: muste_emacs='v'; ch=CODE_HELP; break;
         case KSM_Shift_Control_z:
         case KSM_Shift_Control_Z: ch=CODE_REDO; break;         
         
         
                   					             					  
         					  
         case KS_F1:          ch=CODE_HELP; break;
         case KS_F2:          ch=CODE_PRE; muste_eventpeek=FALSE; break;
         case KS_F3:          ch=CODE_TOUCH; break;
         case KS_F4:          ch=CODE_DISK; break;
         case KS_F5:          ch=CODE_DISP; break;
         case KS_F6:          ch=CODE_MERGE; break;
         case KS_F7:          ch=CODE_REF; break;
         case KS_F8:          ch=CODE_EXIT; muste_eventpeek=FALSE; break;
         case KSM_Control_m:
         case KSM_Control_M:
         case KS_KP_Enter:
         case KS_Insert:
         case KS_F9:          ch=CODE_INSERT; break;         
         case KS_F10:         
         case KSM_Control_F10: ch=CODE_DELETE; break;
         case KSM_F1:         ch=CODE_SOFT_ON; break; 
         case KSM_F2:         ch=CODE_WORDS; break;
         case KSM_F3:         ch=CODE_COPY; muste_eventpeek=FALSE; break;
         case KSM_F4:         ch=CODE_MOVE; break;
         case KSM_F5:         ch=CODE_SRCH; muste_eventpeek=FALSE; break;
         case KSM_F6:         ch=CODE_ACTIV; break;
         case KSM_F7:         ch=CODE_CODE; break;
         case KSM_Control_F7:      ch=CODE_REF_SET; break;         
         case KSM_F8:         ch=CODE_EXIT; muste_eventpeek=FALSE; break;         
         case KSM_F9:         
         case KSM_Insert:
         					  ch=CODE_INSERTL; break;         					  
         case KSM_F10:    
         case KSM_Alt_Delete:
         					  ch=CODE_DELETEL; break;
         case KEY_TAB:
         case KS_Tab:         ch=CODE_TAB; break;
         case KEY_BACKSP:
         case KS_BackSpace:   ch=CODE_BACKSP; break;
         case KEY_DEL:
         case KS_Delete:      
         case KSM_Control_Delete: ch=CODE_DELETE; break;
         case KSM_Control_e:
         case KSM_Control_E:  // Emacs control identical         
         case KS_End:         ch=CODE_END; break;         
         case KS_Home:		  ch=CODE_HOME; break;
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
         
                          case  KS_F11:    ch=CODE_SUCRO1; break; // F11
                          case  KS_F12:    ch=CODE_SUCRO2; break; // F12
                          case  KSM3_F11:    ch=CODE_SUCRO3; break; // Shift+F11
                          case  KSM3_F12:    ch=CODE_SUCRO4; break; // Shift+F12
                          case  KSM2_F11:    ch=CODE_SUCRO5; break; // Ctrl+F11
                          case  KSM2_F12:    ch=CODE_SUCRO6; break; // Ctrl+F12
                          case  KSM_F11:    ch=CODE_SUCRO7; break; // Alt+F11
                          case  KSM_F12:    ch=CODE_SUCRO8; break; // Alt+F12        
         case KEY_RETURN:
         case KS_Return:      ch=CODE_RETURN; break;

         default:
            ch=-1;
//            muste_eventpeek=TRUE;
            break;
          }
 
          return(ch);
          break;

          case MOUSE_EVENT:

           muste_sleep(10); // RS ADD

// RS Siirretty alempaa tähän
            c_mouse=cc+7;
            r_mouse=rr;
            if (sucro_menu && m_click) return(-2);
            if (m_double_click || m_click) if (wait_tut_type!=2) wait_tut_type=0; // 14.2.2001

           
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
				if (i==1) // 13.11.2012
					{
					extern int one_click_copy;
					extern int m_move_ind;
					extern char *prompt_line;
					extern int m_copy_word2();
					extern void delete();
					if (m_move_ind==1)
						{
						if (one_click_copy)
							{
							if (cc>=0 && cc<=c3 && rr>0 && rr<=r3) m_copy_word2();
							else if (cc<-1) delete(); // RS 14.11.2012
							}
						else prompt_line=NULL;
						}
					}
                }




/* RS jo peekissä
            cc=inputBuffer.Event.MouseEvent.dwMousePosition.X-7;
            rr=inputBuffer.Event.MouseEvent.dwMousePosition.Y;
*/




            if (rr<=r3+1 && soft_message)
               {
               soft_message=0; soft_bottom_line_erase();
               }


            if (rr==r3+1 && m_click)
                {

/* RS NYI
extern int m_move_ind2,m_move_ind;

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
                  return(CODE_EXEC);
                  break;
                  }

                if (m_click)
                  {
                  c=cc;
                  r=rr;

                  // 21.3.2004
                  if (right_mouse_click) 
                    { 
                    mouse_define_block(); cursor(r,c);
                    }
                  else
                      disp();
                  }
                }


            else
                {               
                i=soft_key_activate(rr,cc,m_click,m_double_click);
                                
                if (i==2)
                    {                                        
                    special=1;                       
                    return(CODE_EXEC);
                    }
                if (i==3)
                    {
                    special=1;
                    if (soft_code==CODE_HELP) special=2;
                    return(soft_code);
                    }
                if (i==4)
                    {
                    special=0;
                    return((int)soft_char);
                    }
                }

          default:
            break;
            }

        return(-1);
        }

int nextkey_editor()
        {
        int m;

		if (muste_emacs) { m=muste_emacs; muste_emacs=FALSE; return(m); } // RS ADD CHA 22.11.2012

        while (1)
            {
//Rprintf("nextkey while\n");
            m=nextkey2();
            if (m!=-1) return(m); 
// Rprintf("\nnextkey m: %d",m);            
            }
        }

int read_nextkey_editor()
        {
        int m;
        extern int muste_debug;
        extern int muste_debug_print();
        extern char sbuf[];

//		muste_no_selection=TRUE; // RS 22.11.2012
//		muste_mousewheel=FALSE; // RS 22.11.2012
        while (1)
            {
            
			if (muste_debug) // RS 26.1.2013
				{
				sprintf(sbuf,"read nextkey");
				muste_debug_print(sbuf);
				}              
            
            m=nextkey2();
            
            if (muste_debug) // RS 26.1.2013
				{
				sprintf(sbuf,"read nextkey done: %d (%c)",m,m);
				muste_debug_print(sbuf);
				}                          
            
            if (m>=0 || m==-5) 
            	{
//              muste_no_selection=FALSE;
//    			muste_mousewheel=TRUE;
            	return(m); 
            	}
            }
        }


extern int etu;
extern int wait_save;
extern int muste_lopetus;
extern int tutch_editor();

static int nextch_common()
        {
        int m;
        
        if (etu==2)
            {            
            m=tutch_editor();
            while (m==255 && etu==2) m=tutch_editor();
//Rprintf("\ntutm: %d",m);            
            if (m>0) return(m); // RS CHA m!=0 -> m>0
            }
        if (etu==1)
            {
            m=nextkey_editor();
//          cursor(2,50); sprintf(sbuf,"%d  ",m); sur_print(sbuf); getck();
//          cursor(r,c);
            if (m<0) return(m); // RS FIXME Allow only "true" events to be saved!
            if (wait_save) save_wait(m);
            tutsave(m);
            return(m);
            }

        if (muste_lopetus) { special=1; m=CODE_EXIT; }
        else { m=nextkey_editor(); }
                
muste_eventpeek=FALSE;     

        return(m);
        }


int nextch_editor()
        {
        int m;
        extern int muste_mousewheel;
        
        muste_no_selection=TRUE;
        muste_mousewheel=FALSE;
        
        while (1)
        	{
        	muste_eventpeek=FALSE;
        	m=nextch_common();            	
        	if (m>0) break;
        	if (sucro_menu)
            	{                  	
                if (m==-2) break;  // RS ADD sucromenu mouse
        		}          		
        	}

		muste_mousewheel=TRUE;
		muste_no_selection=FALSE;
//Rprintf("\nnextch m: %d",m);
        return(m);
        }

int nextch_editor_eventloop()
        {
        int m;
        muste_eventpeek=TRUE;
        m=nextch_common();
        return(m);
        }


int sur_getch() { return(getck2(0,255)); }

#define EURO 9999
#define MUSTE_SHIFT 1
#define MUSTE_CAPSLOCK 2
#define MUSTE_CTRL 4
#define MUSTE_CMD 16
#define MUSTE_ALT 8192

static int sur_prefix_code=0;
static int sur_key=0;

// RS REM static int pre_vkey=0;
// RS REM static int pre_vkey_state=0;
extern int display_keys;
// RS REM int sur_ctrl; // 10.1.2001

extern char sbuf[];


// 9.4.2000 touch modia varten
int sur_getch();
int s_getch()
    {
    int m;

    while (1)
        {
//        ReadConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);
        m=sur_getch_ext();      
        if (m!=-1) return(m);
        }
    }

static int sur_getch2();
int sur_getch_ext()
    {

    int i;
    int special;
    char ascii;

    if (sur_prefix_code) { sur_prefix_code=0; return(sur_key); }
    i=sur_getch2(&sur_key,&special,&ascii);
    if (i==EURO) return(EURO);

    if (sur_key<1) return(-1);  // RS CHA !sur_key
    if (special) { sur_prefix_code=1; return(EXTEND_CH); }
    
    return(sur_key);
    }

static int sur_getch2(int *psur_key,int *pspecial,char *pascii)
    {
    int vkey; // RS CHA    WORD vkey;
// RS REM    int caps;
    int state; // RS CHA DWORD state;
//    int caps_on, shift_pressed;
// RS REM    char    num[]="1234567890?_:>*;";
// RS REM    char num_up[]="!\"#œ%&/()=+-.<',";
// RS REM    int virt_code; // RS CHA WORD virt_code;
    int ch; // RS

    ch=vkey=getck2(0,1000000); // RS Read character
    state=muste_keystatus;



// RS NYI if (euro-merkki) return(EURO);

    sur_ctrl=0;
    if (state & MUSTE_CTRL) sur_ctrl=1; // At least CTRL pressed  

//    caps_on=state & MUSTE_CAPSLOCK;
//    shift_pressed=state & MUSTE_SHIFT;
	
    
    if (ch>31 && ch<256 && ch!=127)
      {  
      *pascii=ch;
      *psur_key=ch;
      *pspecial=0;
      }
    else 
      { 
      *pascii=' ';
      *pspecial=1;      
      }
      switch (ch)
         {
         case KSM_F1: *psur_key=CODE_SOFT_ON; *pspecial=1; break;  /* alt-f1: soft_on */
         case KSM_F2: *psur_key=CODE_WORDS; *pspecial=1; break;    /* alt-f2: words */
         case KSM_F3: *psur_key=CODE_COPY; *pspecial=1; break;     /* alt-f3: copy */
         case KSM_F4: *psur_key=CODE_MOVE; *pspecial=1; break;     /* alt-f4: move */
         case KSM_F5: *psur_key=CODE_SRCH; *pspecial=1; break;     /* alt-f5: srch */
         case KSM_F6: *psur_key=CODE_ACTIV; *pspecial=1; break;    /* alt-f6: f_act */
         case KSM_F7: *psur_key=CODE_CODE; *pspecial=1; break;     /* alt-f7: code */
         case KSM_Control_F7: *psur_key=CODE_REF_SET; *pspecial=1; break;
         case KSM_F8: *psur_key=CODE_EXIT; *pspecial=1; break;     /* alt-f8: ---- */
         case KSM_F9: *psur_key=CODE_INSERTL; *pspecial=1; break;  /* alt-f9: insl */
         case KSM_F10: *psur_key=CODE_DELETEL; *pspecial=1; break; /* alt-f10: dell */
         case KSM_F11: *psur_key=CODE_SUCRO7; *pspecial=1; break;  /* alt-f11: suc7 */
         case KSM_F12: *psur_key=CODE_SUCRO8; *pspecial=1; break;  /* alt:f12 suc8 */
         case KSM_Next: *psur_key=149; *pspecial=1; break;         /* alt-PgDn */
         case KSM_Prior: *psur_key=150; *pspecial=1; break;        /* alt-PgUp */         
         case KEY_RETURN:
         case KS_Return: *psur_key=CODE_RETURN; break;
         case KEY_BACKSP:
         case KS_BackSpace: *psur_key=CODE_BACKSP; break;
                 
         case KS_Right: *psur_key=CODE_RIGHT; *pspecial=1; break;
         case KSM_Right: *psur_key=CODE_RIGHT2; *pspecial=1; break;
         case KSM_Control_Right: *psur_key=CODE_RIGHT3; *pspecial=1; break;
         case KS_Left: *psur_key=CODE_LEFT; *pspecial=1; break;
         case KSM_Left: *psur_key=CODE_LEFT2; *pspecial=1; break;
         case KS_Up: *psur_key=CODE_UP; *pspecial=1; break;
         case KSM_Up: *psur_key=CODE_UP2; *pspecial=1; break;
         case KS_Down: *psur_key=CODE_DOWN; *pspecial=1; break;
         case KSM_Down: *psur_key=CODE_DOWN2; *pspecial=1; break;
         case KS_Home: *psur_key=CODE_HOME; *pspecial=1; break;
         case KS_KP_Enter:
         case KS_F9:
         case KS_Insert: *psur_key=CODE_INSERT; *pspecial=1; break;       /* ins2 */
         case KSM_Insert: *psur_key=CODE_INSERTL; *pspecial=1; break;         
//       case (ctrl+insert) *psur_key=151; // COPY clipboard
//       case (shift+insert) *psur_key=153; // PASTE
         case KEY_EXEC:
         case KS_Escape:      *psur_key=CODE_EXEC; break;
         case KSM_Control_r:
         case KSM_Control_R:  *psur_key=CODE_REXEC; break;
         case KSM_Control_v:
         case KSM_Control_V:  *psur_key=CODE_PASTE; break;
         case KS_F10:
         case KEY_DEL:
         case KSM_Control_Delete:
         case KS_Delete:      *psur_key=CODE_DELETE; break;
         case KSM_Delete:     *psur_key=CODE_DELETEL; break;
         case KS_Next:        *psur_key=CODE_NEXT; break;
         case KS_Prior:       *psur_key=CODE_PREV; break;
//       case (ctrl+pagedown):  *psur_key=145; *pspecial=1; break;
//       case (ctrl+pageup): *psur_key=146; *pspecial=1; break;
// sur_ctrl alternatives above used in FILE MEDIT only!


         case KS_End: *psur_key=CODE_END; *pspecial=1; break;
         case KSM_End: *psur_key=CODE_ERASE; break;         /* erase */
        
		 case KEY_TAB:
         case KS_Tab: *psur_key=CODE_TAB; break;
         case KSM_Tab: *psur_key=0; break;
//       case (shift+tab): *psur_key=15; break;         

		 case KS_F1: *psur_key=CODE_HELP; break;
		 case KS_F2: *psur_key=CODE_PRE; break;
		 case KS_F3: *psur_key=CODE_TOUCH; break;
		 case KS_F4: *psur_key=CODE_DISK; break;
		 case KS_F5: *psur_key=CODE_DISP; break;		 
		 case KS_F6: *psur_key=CODE_MERGE; break;		 
		 case KS_F7: *psur_key=CODE_REF; break;		 
		 case KS_F8: *psur_key=CODE_EXIT; break;
//		 case VK_F8:
//                      if (!state || state==CAPSLOCK_ON)
//                          { *psur_key=66; *pspecial=1; break; }
//                      else { *psur_key=0; break; }
// RS ABOVE	case KS_F9: *psur_key=CODE_INSERT; break;		 
// RS ABOVE case KS_F10: *psur_key=CODE_DELETE; break;		 

         case KS_F11: *psur_key=CODE_SUCRO1; *pspecial=1; break;        /* suc1 */
		 case KS_F12: *psur_key=CODE_SUCRO2; *pspecial=1; break;
//          if (shift F11) *psur_key=CODE_SUCRO3; break;
//          if (shift F12) *psur_key=CODE_SUCRO4; break;
//          if (ctrl F11) *psur_key=CODE_SUCRO5; break;
//          if (ctrl F12) *psur_key=CODE_SUCRO6; break;
//          if (caps F11) *psur_key=CODE_SUCRO7; break;
//          if (caps F12) *psur_key=CODE_SUCRO8; break;

         default:
            if (*pspecial==1) *psur_key=-1;
            break;
          }

/*
 display_keys=1;
 if (display_keys)
        {
        cursor(23,1);
        sprintf(sbuf,"%d  %d  %d  %c              ",vkey,state,*pspecial,*pascii);
        sur_print(sbuf);
        cursor(r,c);
        }
*/

/* RS NYI ???
    state=state & ~NUMLOCK_ON & ~SCROLLLOCK_ON & ~CAPSLOCK_ON;

    if (state==LEFT_ALT_PRESSED+CAPSLOCK_ON ||
        state==RIGHT_ALT_PRESSED+CAPSLOCK_ON) caps=1;
    else caps=0;

    // crtl-A = 1 -B =2 -D=4 ... -H=8 luonnostaan
*/


#if 0 
    switch (state)
        {
      case LEFT_ALT_PRESSED:
      case RIGHT_ALT_PRESSED:
        switch (vkey)
            {
          case VK_F1: *psur_key=104; *pspecial=1; break; /* soft_on */
          case VK_F2: *psur_key=105; *pspecial=1; break; /* words */
          case VK_F3: *psur_key=106; *pspecial=1; break; /* copy */
          case VK_F4: *psur_key=107; *pspecial=1; break; /* move */
          case VK_F5: *psur_key=108; *pspecial=1; break; /* srch */
          case VK_F6: *psur_key=109; *pspecial=1; break; /* f_act */
          case VK_F7: *psur_key=110; *pspecial=1; break; /* code */
          case VK_F8: *psur_key=111; *pspecial=1; break; /* ---- */
          case VK_F9: *psur_key=112; *pspecial=1; break; /* insl */
         case VK_F10: *psur_key=113; *pspecial=1; break; /* dell */
         case VK_F11: *psur_key=139; *pspecial=1; break; /* suc7 */
         case VK_F12: *psur_key=140; *pspecial=1; break; /* suc8 */
         case VK_NEXT: *psur_key=149; *pspecial=1; break; /* alt-PgDn */
         case VK_PRIOR: *psur_key=150; *pspecial=1; break; /* alt-PgUp */

          default: break;
            }
        break;

      default:
        switch (vkey)
            {
          case VK_RETURN: *psur_key=13; break;
          case VK_BACK: *psur_key=8; break;
          case VK_RIGHT: *psur_key=77; *pspecial=1;
                  if (state & ALT_PRESSED) *psur_key=157;
                  if (sur_ctrl) *psur_key=147; // 6.6.2003
                  break;
          case VK_LEFT: *psur_key=75; *pspecial=1;
                  if (state & ALT_PRESSED) *psur_key=155;
                  break;
          case VK_UP: *psur_key=72; *pspecial=1;
                  if (state & ALT_PRESSED) *psur_key=152;
                  break;
          case VK_DOWN: *psur_key=80; *pspecial=1;
                  if (state & ALT_PRESSED) *psur_key=160;
                  break;
          case VK_HOME: *psur_key=71; *pspecial=1; break;
          case VK_INSERT: *psur_key=82; *pspecial=1;        /* ins2 */
 /* 24.4.2006 */          if (sur_ctrl) *psur_key=151; // COPY clipboard
 /* 26.6.2006 */          if (shift_pressed) *psur_key=153; // PASTE
/*  4.2.2007 */           if (state & ALT_PRESSED) *psur_key=112; // ins line
                          break;
          case VK_DELETE: *psur_key=83; *pspecial=1; /* del2 */
/*  4.2.2007 */           if (state & ALT_PRESSED) *psur_key=113; // del line
                          break;
          case VK_NEXT:
                        if (state & ALT_PRESSED) { *psur_key=149; *pspecial=1; break; }
                        if (sur_ctrl) { *psur_key=145; *pspecial=1; break; }
                        *psur_key=81; *pspecial=1; break;
          case VK_PRIOR:
                        if (state & ALT_PRESSED) { *psur_key=150; *pspecial=1; break; }
                        if (sur_ctrl) { *psur_key=146; *pspecial=1; break; }
                        *psur_key=73; *pspecial=1; break;
// sur_ctrl alternatives above used in FILE MEDIT only!

          case VK_END: *psur_key=79; *pspecial=1;
if (state & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)) *psur_key=117;
                       break;                           /* erase */

          case VK_TAB: *psur_key=9;
      /* 5.11.2001 */ if (state & ALT_PRESSED) { *psur_key=0; break; }
                       if (state & SHIFT_PRESSED)
                                *psur_key=15;
                                 break;

          case VK_F1: *psur_key=59; *pspecial=1; break; /* help  */

          case VK_F2: *psur_key=60; *pspecial=1;        /* pre   */
                      if (caps) *psur_key=105; break;
          case VK_F3: *psur_key=61; *pspecial=1;        /* touch */
                      if (caps) *psur_key=106; break;
          case VK_F4: *psur_key=62; *pspecial=1;        /* disk */
                      if (caps) *psur_key=107; break;
          case VK_F5: *psur_key=63; *pspecial=1;        /* disp */
                      if (caps) *psur_key=108; break;
          case VK_F6: *psur_key=64; *pspecial=1;        /* merge */
                      if (caps) *psur_key=109; break;
                      // ctrl-F7 (REF_SET)lisÑtty 30.4.2002
          case VK_F7: if (sur_ctrl)
                          { *psur_key=141; *pspecial=1; break; }
                      *psur_key=65; *pspecial=1;        /* ref  */
                      if (caps) *psur_key=110; break;
          case VK_F8:
                      if (!state || state==CAPSLOCK_ON)
                          { *psur_key=66; *pspecial=1; break; }
                      else { *psur_key=0; break; }
          case VK_F9: *psur_key=67; *pspecial=1;        /* ins  */
                      if (caps) *psur_key=112; break;
         case VK_F10: *psur_key=68; *pspecial=1;        /* del  */
                      if (caps) *psur_key=113; break;
         case VK_F11: *psur_key=133; *pspecial=1;        /* suc1 */
                      if (caps) *psur_key=139;
                      if (state & SHIFT_PRESSED) *psur_key=135;
if (state & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED)) *psur_key=137;
                      break;

         case VK_F12: *psur_key=134; *pspecial=1;        /* suc2 */
                      if (caps) *psur_key=140;
                      if (state & SHIFT_PRESSED) *psur_key=136;
if (state & (RIGHT_CTRL_PRESSED | LEFT_CTRL_PRESSED)) *psur_key=138;
                      break;

          default: break;
            }
        break;
        }
#endif        
        
    return(vkey);
    }

int nextkey2_medit()
        {
        extern int headline_medit();
        extern int mouse_medit_functions();
        int m,aika1,ch; // RS REM ,no_key;
        time_t aika2,aika3;
        time_t time1,time2;      
        int i;
// RS REM        char s[8];
// RS REM        int jo_talletettu;
        int erotus;
//        static int loading_help_lines=0;
//        extern int nop();

        aika1=0;
        time(&aika2);
        time1=aika2;

        while (1) /* 16.2.1997 */
            {
            if (key_sleep) sur_sleep(key_sleep);
            
            if (muste_peekinputevent(TRUE)) break;
// RS CHA            PeekConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);
// RS CHA            if (dwInputEvents) break;
            
            time(&time2);
            if (difftime(time2,time1)>0.5)
                {
                headline_medit();

                if (wait_tut_type) // 14.2.2001
                    {
                    if (time2>=wait_tut_time)
                        return(-7);
                    }

                time1=time2;

                time(&aika3);
                erotus=aika3-aika2;
                if (!etu && !rajoitettu_vastausaika && erotus>3
                       && autosave && aika3-aika_save>60*autosave)
                    {
                    autosavefield=1;
// 4.5.2003         edt_talletus(SURVOEDT);
                    autosavefield=0;
// Rprintf("\nSURVOEDT!"); getck();
                    aika_save=aika3;
                    aika2=aika3;
                    }
                }
/*******************************************
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
**********************************************/
            sur_sleep(10);
//          continue;    // poistoyritys 20.11.2001

            if (rajoitettu_vastausaika)
                {
                time(&aika3);
                if (difftime(aika3,vastauksen_alkuhetki)>max_vastausaika)
                    {
                    m=CODE_RETURN; special=1; return(m);
                    }
                }
            ++aika1;

            }


// RS REM    ReadConsoleInput(hStdIn, &inputBuffer, 1, &dwInputEvents);

        switch (muste_eventtype)
          {
          case KEY_EVENT:
          
//            if (inputBuffer.Event.KeyEvent.bKeyDown)
//              {
              
              if (wait_tut_type!=2) wait_tut_type=0; // 14.2.2001
              if (insert_mode) CURSOR_INS; else CURSOR_ON;
              special=0;
              m=ch=muste_char; // sur_getch();
              /* if (!key_code) break; */
/********************************
              if (m==9999) // EURO altGr e     28.1.2002
                  {
                  PR_EBLK;
                  sur_print("\nEuro character in Survo is e with shadow E.");
                  WAIT; PR_ENRM; disp(); m='e';
                  }
***********************************/
//              loading_help_lines=0;

      if (ch>31 && ch<256 && ch!=127) return(ch);
 
      special=TRUE;

      switch (ch)
         {
         case KEY_EXEC:
         case KS_Escape:      ch=CODE_EXEC; muste_eventpeek=FALSE; break;
         case KSM_Control_r:
         case KSM_Control_R:  ch=CODE_REXEC; muste_eventpeek=FALSE; break;
         case KSM_Control_v:
         case KSM_Control_V:  
         case KSM_Shift_Insert:
         					  ch=153; break;   
 		 case KSM_Control_c:
         case KSM_Control_C:  
         case KSM_Control_Insert:
         					  ch=151; break;            					  
         					  
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
         case KS_Insert:
         case KS_F9:          ch=CODE_INSERT; break;
         case KS_F10:         
         case KSM_Control_F10: ch=CODE_DELETE; break;
         case KSM_F1:         ch=CODE_SOFT_ON; break; 
         case KSM_F2:         ch=CODE_WORDS; break;
         case KSM_F3:         ch=CODE_COPY; muste_eventpeek=FALSE; break;
         case KSM_F4:         ch=CODE_MOVE; break;
         case KSM_F5:         ch=CODE_SRCH; muste_eventpeek=FALSE; break;
         case KSM_F6:         ch=CODE_ACTIV; break;
         case KSM_F7:         ch=CODE_CODE; break;
         case KSM_Control_F7:      ch=CODE_REF_SET; break;         
         case KSM_F8:         ch=CODE_EXIT; muste_eventpeek=FALSE; break;
         case KSM_F9:         
         case KSM_Insert:	  ch=CODE_INSERTL; break;
         case KSM_F10:     
//         case KSM_Delete:
         case KSM_Alt_Delete:        
         					  ch=CODE_DELETEL; break;
         case KEY_TAB:
         case KS_Tab:         ch=CODE_TAB; break;
         case KEY_BACKSP:
         case KS_BackSpace:   ch=CODE_BACKSP; break;
         case KEY_DEL:
         case KS_Delete:      
         case KSM_Control_Delete: ch=CODE_DELETE; break;
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
         
                          case  KS_F11:    ch=CODE_SUCRO1; break; // F11
                          case  KS_F12:    ch=CODE_SUCRO2; break; // F12
                          case  KSM3_F11:    ch=CODE_SUCRO3; break; // Shift+F11
                          case  KSM3_F12:    ch=CODE_SUCRO4; break; // Shift+F12
                          case  KSM2_F11:    ch=CODE_SUCRO5; break; // Ctrl+F11
                          case  KSM2_F12:    ch=CODE_SUCRO6; break; // Ctrl+F12
                          case  KSM_F11:    ch=CODE_SUCRO7; break; // Alt+F11
                          case  KSM_F12:    ch=CODE_SUCRO8; break; // Alt+F12        

                          case  KSM2_Next: ch=145; break; // ctrl-PgDn
                          case  KSM2_Prior: ch=146; break; // ctrl-PgUp
                          case  KSM2_Right: ch=147; break; // ctrl-Right
                          case  KSM_Next: ch=149; break; // alt-PgDn
                          case  KSM_Prior: ch=150; break; // alt-PgUp

         default:
            ch=-1;
//            muste_eventpeek=TRUE;
            break;
          }
 
        return(ch);
        break;

/*
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
                          case  KEY_SUCRO1:    m=CODE_SUCRO1; break;
                          case  KEY_SUCRO2:    m=CODE_SUCRO2; break;
                          case  KEY_SUCRO3:    m=CODE_SUCRO3; break;
                          case  KEY_SUCRO4:    m=CODE_SUCRO4; break;
                          case  KEY_SUCRO5:    m=CODE_SUCRO5; break;
                          case  KEY_SUCRO6:    m=CODE_SUCRO6; break;
                          case  KEY_SUCRO7:    m=CODE_SUCRO7; break;
                          case  KEY_SUCRO8:    m=CODE_SUCRO8; break;
                          case  KEY_SOFT_ON:   m=CODE_SOFT_ON; break;
                          case  KEY_REF_SET:   m=CODE_REF_SET; break;

                          case  145: m=145; break; // ctrl-PgDn
                          case  146: m=146; break; // ctrl-PgUp
                          case  147: m=147; break; // ctrl-Right
                          case  149: m=149; break; // alt-PgDn
                          case  150: m=150; break; // alt-PgUp

                          default: 
                                   m=32;
                            }
                          break;

                case KEY_RETURN: special=1; m=CODE_RETURN; break;
                case KEY_BACKSP: special=1; m=CODE_BACKSP; break;
                case KEY_EXEC: special=1; m=CODE_EXEC; break;
                case KEY_TAB: special=1; m=CODE_TAB; break;

                default: ;
                  }
              return(m);
              }
            break;
*/            
            
          case MOUSE_EVENT:
          
            muste_sleep(10); // RS ADD
            
/*            
            m_double_click=0; m_click=0;
            m_right_click=0;

            if (inputBuffer.Event.MouseEvent.dwEventFlags==DOUBLE_CLICK)
                {
                m_double_click=1;
                }

            if (inputBuffer.Event.MouseEvent.dwEventFlags == 0 &&
                inputBuffer.Event.MouseEvent.dwButtonState)
                m_click=1;

    if (inputBuffer.Event.MouseEvent.dwButtonState==RIGHTMOST_BUTTON_PRESSED)
                m_right_click=1;



// sprintf(sbuf,"%d",m_click); sur_print(sbuf);

            cc=inputBuffer.Event.MouseEvent.dwMousePosition.X; // 5.5.03
            rr=inputBuffer.Event.MouseEvent.dwMousePosition.Y;
*/
            c_mouse=cc+7; // 5.5.2003
            r_mouse=rr;

            i=mouse_medit_functions(c_mouse,r_mouse,m_click,m_double_click);
            return(i);

            break;
            }
        return(-1);
        }

