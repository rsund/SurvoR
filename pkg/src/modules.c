#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

extern int arguc;
extern char *arguv[];

extern char sur_session[];
extern char OO[];
extern char *op;

extern int op_file();
extern int op_gplot();

extern int muste_var();
extern int muste_corr();
extern void muste_mean();
extern void muste_date();
extern int muste_tutor();
extern int muste_ediop();
extern int muste_mat();
extern int muste_pol();
extern int muste_touch();
extern void muste_stat();
extern void muste_statmsf();
extern int muste_linreg();
extern void muste_classify();
extern void muste_estimate();
extern void muste_desktop();
extern void muste_xall();

int muste_modules()
        {
        int i,found;

        found=1;


        if (strcmp(OO,"VAR")==0) { muste_var(sur_session);
                                   return(1); }  // RS lisätty testiksi


else    if (strcmp(OO,"CORR")==0) { muste_corr(sur_session);
                                   return(1); }  // RS lisätty testiksi

else    if (strcmp(OO,"MEAN")==0) { muste_mean(sur_session);
                                   return(1); }  // RS lisätty testiksi

else    if (strcmp(OO,"DATE")==0 || strcmp(OO,"PVM")==0)  // KV
                                { muste_date(sur_session); // KV
                                  return(1); }             // KV

else    if (strchr(OO,'?')==NULL &&
             (strncmp(OO,"TUTS",4)==0 || strncmp(OO,"TUTL",4)==0 ||
              strncmp(OO,"TUTD",4)==0 || strncmp(OO,"TUTI",4)==0 ) )
// RS CHA { strcpy(op,"TUT"); strcpy(pref,"&"); }
            { muste_tutor(sur_session); return(1); }

else    if (strcmp(OO,"FILE")==0 || strcmp(OO,"F")==0)
            {
              i=op_file(op);
// RS REM              if (i==1) childp("FI\\");
              soft_disp(1);
              return(1);
            }

else    if (strncmp(OO,"MAT",3)==0)
            {
              i=muste_mat(arguc,arguv);
              return(1);
            }

else    if (strcmp(OO,"POL")==0)
            {
              muste_pol(arguc,arguv);
              return(1);
            }

else    if ( // KV
           (strcmp(OO,"INDEX")==0) ||
           (strcmp(OO,"SEARCH")==0) ||
           (strcmp(OO,"DD")==0) ||
           (strcmp(OO,"WHERE")==0) ||
           (strcmp(OO,"TREE")==0) ||
           (strcmp(OO,"DM")==0)
          )
          {
            muste_desktop(sur_session);
            return(1);
          }

else    if (strcmp(OO,"XALL")==0)  // KV
           {
             muste_xall(sur_session);
             return(1);
           }

else    if (strcmp(OO,"LINREG")==0)  // SM
            {
              muste_linreg(sur_session);
              return(1);
            }

else    if (strcmp(OO,"REGDIAG")==0)  // SM
            {
              muste_linreg(sur_session);
              return(1);
            }

else    if (strcmp(OO,"STAT")==0)  // SM
            {
              muste_stat(sur_session);
              return(1);
            }

else    if (strcmp(OO,"STATMSF")==0)  // SM
            {
              muste_statmsf(sur_session);
              return(1);
            }

else    if ((strcmp(OO,"ESTIMATE")==0) || (strcmp(OO,"DER")==0))  // SM
            {
              muste_estimate(sur_session);
              return(1);
            }

else    if (strcmp(OO,"CLASSIFY")==0)  // SM
            {
              muste_classify(sur_session);
              return(1);
            }





else    if (
           (strcmp(OO,"SORT")==0) || (muste_strcmpi(OO,"-SORT")==0) ||
           (strncmp(OO,"TRIM",4)==0) || (*OO=='T' && strlen(OO)<3) ||
           (strcmp(OO,"ERASE")==0) || (strcmp(OO,"CHANGE")==0) ||
           (strcmp(OO,"MOVE")==0) || (strcmp(OO,"FORM")==0) ||
           (strcmp(OO,"PUTEND")==0) ||
           ((*OO=='C' || *OO=='L') && (strchr("+-*/%",OO[1])!=NULL)) ||
           (strcmp(OO,"LINEDEL")==0) || (strcmp(OO,"!LINEDEL")==0) ||
           (strcmp(OO,"LOADW")==0) ||(strcmp(OO,"SAVEW")==0) ||
           (strcmp(OO,"LOADU")==0) || (strcmp(OO,"SAVEU")==0) ||
           (strcmp(OO,"LOADP")==0) || (strcmp(OO,"LOADP2")==0) ||
           (strcmp(OO,"SAVEP")==0) || (strcmp(OO,"SAVEP2")==0) ||
           (strcmp(OO,"CODES")==0) || (strcmp(OO,"CONVERT")==0) ||
           (strcmp(OO,"NCOPY")==0) || (strcmp(OO,"UPDATE")==0) ||
           (strncmp(OO,"TXT",3)==0) || (strcmp(OO,"TRANSP")==0) ||
           (strcmp(OO,"INTERP")==0) || (strcmp(OO,"TONES")==0) ||
           (strcmp(OO,"VFIND")==0) || (strcmp(OO,"PCOPY")==0) ||
           (strcmp(OO,"DELF")==0) || (strcmp(OO,"STRDIST")==0) ||
           (strcmp(OO,"R")==0) ||
           (strcmp(OO,"REVERSE")==0) || (strncmp(OO,"TRANSPO",7)==0)
           )
//        if (strcmp(op,"EDI2")==0)
                {
                i=muste_ediop(sur_session);
                return(1);

//              if (i==1) return(1);
                }

else    if (strncmp(OO,"TCH",3)==0)
            {
            strcpy(op,"T"); // RS REM strcpy(pref,"&");
            strcpy(info,"TOUCH");
            i=muste_touch(arguc,arguv);
            }

         // RS GPLOT added to avoid some sucro errors with Survo tour
else    if (strcmp(OO,"GPLOT")==0) {  //  && etu==2

op_gplot(op);
// muste_fixme("FIXME: GPLOT not implemented!\n"); // RS FIXME

return(1); }

found=0;
return(found);

}

