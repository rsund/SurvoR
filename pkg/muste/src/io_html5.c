#include "muste.h"
#include "survo.h"
#include <stdio.h>

extern int muste_evalr(char *cmd);
extern int muste_iconv(char *teksti,char *to,char *from);
extern int muste_requirepackage(char *package);
extern int muste_get_R_int(char *sour);
extern double muste_get_R_real(char *sour);
extern int muste_get_R_string(char *dest,char *sour,int length);
extern int muste_get_R_int_vec(char *sour,int element);
extern double muste_get_R_real_vec(char *sour,int element);
extern int muste_get_R_string_vec(char *dest,char *sour,int length,int element);
extern void muste_set_R_int(char *dest,int luku);
extern void muste_set_R_string(char *dest,char *sour);
extern double muste_R_function(char *s,double *x,int n);
extern void muste_Survo2R(char *dest,char *sour);
extern void muste_R2Survo(char *dest,char *sour);
extern void muste_init_plotwindows(void);
extern int muste_beep(void);
extern int muste_debug_print(char *teksti);

static char komento[3*LLENGTH];
//static char tclkomento[3*LLENGTH]; 
//static char plotkomento[3*LLENGTH]; 

int sur_locate_html5(int row,int col)
{
    sprintf(komento,"%d\t%d\tcursor",row-1,col-1);
	muste_set_R_string(".muste$ajaxmsg",komento);
    muste_evalr("muste:::survo.sendajax()");    
    return(1);
}

