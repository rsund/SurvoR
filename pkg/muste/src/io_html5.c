#include "muste.h"
#include "survo.h"
#include <stdio.h>

extern int muste_evalr();
extern int muste_iconv();
extern int muste_get_R_int();
extern double muste_get_R_real();
extern int muste_get_R_string();
extern int muste_get_R_int_vec();
extern double muste_get_R_real_vec();
extern int muste_get_R_string_vec();
extern void muste_set_R_int();
extern void muste_set_R_string();
extern double muste_R_function();
extern void muste_Survo2R();
extern void muste_R2Survo();


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

