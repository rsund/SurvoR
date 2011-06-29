#include <R.h>
#include <Rinternals.h>
#include <Rinterface.h>
#include <R_ext/Riconv.h>
#include "survo.h"
#include "kscodes.h"
#include "survolib.h"

extern SEXP Muste_EvalRExpr();

int muste_externalchar(char *jakso) 
{
    static wchar_t kohde[LLENGTH+1];

    char y[3*LLENGTH];
    const char *inbuf;
    char *outbuf;
    size_t inb, outb, res;

    void *obj;


    strcpy(y,jakso); 
    obj = Riconv_open("", "CP850");
    if(obj == (void *)(-1)) error("Unsupported conversion!");

    inbuf=y;
    inb = strlen(inbuf)+1; outb = 2*LLENGTH+1;
    outbuf = (char *) jakso;
    res = Riconv(obj, &inbuf , &inb, &outbuf, &outb);
    Riconv_close(obj);
    if(res == -1) error("Conversion problem");
    if(inb > 0) error("Conversion problem -- too long?");


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


int nextch()
{
   SEXP avar=R_NilValue;
   unsigned char merkki;
   int ch, valmis;
   const char komento[] = "MusteGetKey()";
   char apu[16];

   do
   {
      ch=INTEGER(Muste_EvalRExpr(komento))[0];
    
      if (ch==0) 
      {
         avar = findVar(install("mustekeychar"),R_GlobalEnv);
         strcpy(apu,CHAR(STRING_ELT(avar,0)));
//Rprintf("apu: %s,pituus:%d",apu,strlen(apu));
         merkki=(unsigned char)apu[0];
         ch=merkki;
      }
      valmis=TRUE;

//Rprintf("ch: %d",ch);

      switch (ch)
         {
         case KS_F3:          ch=CODE_TOUCH; break;
         case KS_F8:          ch=CODE_EXIT; break;
         case KS_Tab:         ch=CODE_TAB; break;
         case KS_Insert:      ch=CODE_INSERT; break;

	 case KS_adiaeresis:  ch=132; break;

         case KS_Shift_L:
         case KS_Shift_R:
         case KS_Control_L:
         case KS_Control_R:
         case KS_Caps_Lock:
         case KS_Shift_Lock:
         case KS_Meta_L:
         case KS_Meta_R:
         case KS_Alt_L:
         case KS_Alt_R:
         case KS_Alt_Gr:
         case KS_Super_L:
         case KS_Super_R:
         case KS_Hyper_L:
         case KS_Hyper_R:
         case KS_Win_L:
         case KS_Win_R:
         case KS_Menu:
            ch=0; valmis=FALSE;
            break;
 
         default: break;
      }
   } while (!valmis);
   return(ch);
}
