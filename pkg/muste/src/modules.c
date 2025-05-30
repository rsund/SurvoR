#include "muste.h"
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
extern int muste_undo;
extern char *parm[];

extern int op_file(char *op);
extern int op_gplot(char *op);

extern int muste_var(char *argv);
extern int muste_corr(char *argv);
extern void muste_mean(char *argv);
extern void muste_date(char *argv);
extern int muste_tutor(char *argv);
extern int muste_ediop(char *argv);
extern int muste_mat(int argc,char *argv[]);
extern void muste_pol(int argc,char *argv[]);
extern void muste_touch(int argc, char *argv[]);
extern void muste_stat(char *argv);
extern void muste_statmsf(char *argv);
extern void muste_linreg(char *argv);
extern void muste_classify(char *argv);
extern void muste_estimate(char *argv);
extern void muste_desktop(char *argv);
extern void muste_xall(char *argv);
extern void muste_regdiag(char *argv);
extern void muste_facta(char *argv);
extern void muste_rotate(char *argv);
extern void muste_loadm(int argc, char *argv[]);
extern void muste_intrel(char *argv);
extern void muste_compare(char *argv);
extern void muste_show(int argc,char *argv[]);
extern void muste_tab(char *argv);
extern void muste_tabb(char *argv);
extern void muste_tabtest(char *argv);
extern void muste_tabs(char *argv);
extern void muste_plot(int argc, char *argv[]);
extern void muste_eps(int argc, char *argv[]);
extern void muste_print(int argc, char *argv[]);
extern void muste_nterm(int argc, char *argv[]);
extern int muste_disco(int argc, char *argv[]);
extern void muste_mnsimul(char *argv);
extern void muste_corresp(char *argv);
extern void muste_canon(char *argv);
extern void muste_linco(char *argv);
extern void muste_transform(char *argv);
extern void muste_varstat(char *argv);
extern void muste_burt(char *argv);
extern void muste_ser(char *argv);
extern void muste_genreg(char *argv);
extern void muste_powers(char *argv);
extern void muste_lscal(char *argv);
extern void muste_dist(char *argv);
extern void muste_distv(char *argv);
extern void muste_hcluster(char *argv);
extern void muste_cluster(char *argv);
extern void muste_quanta(char *argv);
extern void muste_classi(char *argv);
extern void muste_mntest(char *argv);
extern void muste_multvar(char *argv);
extern void muste_corrtest(char *argv);
extern void muste_dcluster(char *argv);
extern void muste_corrmv(char *argv);
extern void muste_minstat(char *argv);
extern void muste_t2test(char *argv);
extern void muste_covtest(char *argv);
extern void muste_xcorr(char *argv);
extern void muste_forecast(char *argv);
extern void muste_smooth(char *argv);
extern void muste_runtest(char *argv);
extern void muste_robreg(char *argv);
extern void muste_logmean(char *argv);
extern void muste_rndtest(char *argv);
extern void muste_markov(char *argv);
extern void muste_lue(char *argv);
extern void muste_geom(char *argv);
extern void muste_comb(char *argv);
extern void muste_simplex(char *argv);
extern void muste_reliab(char *argv);
extern void muste_movreg(char *argv);
extern void muste_lowess(char *argv);
extern void muste_mtab(int argc,char *argv[]);
extern int muste_magic(char *argv);
extern int muste_rplot(char *argv);
extern void muste_list(int parmn, char *parm[]);
extern int muste_risksimu(int argc, char *argv[]);
extern void muste_survival(int argc, char *argv[]);
extern int muste_piso(int argc, char *argv[]);


int muste_dd_call=FALSE;

static int op_tab(char *OO); // 14.7.2011/SM

int muste_modules()
        {
        int found;

		muste_dd_call=FALSE;
        found=1;


        if (strcmp(OO,"VAR")==0) { muste_var(sur_session); return(1); }  
else    if (strcmp(OO,"CORR")==0) { muste_corr(sur_session); return(1); }  
else    if (strcmp(OO,"MEAN")==0) { muste_mean(sur_session); return(1); }  
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
              op_file(op);            
// RS REM              if (i==1) childp("FI\\");
              soft_disp(1);
              return(1);
            }
else    if (strncmp(OO,"MAT",3)==0) { muste_mat(arguc,arguv); return(1); }
else    if (strcmp(OO,"POL")==0) { muste_pol(arguc,arguv); return(1); }
else    if (strcmp(OO,"SHOW")==0) { muste_show(arguc,arguv); return(1); }
else    if (strcmp(OO,"EPS")==0) { muste_eps(arguc,arguv); return(1); }
else    if (strcmp(OO,"PRINT")==0) { muste_print(arguc,arguv); return(1); }
else    if (strcmp(OO,"DISCO")==0) { muste_disco(arguc,arguv); return(1); }
else    if (strcmp(OO,"NTERM")==0) { muste_nterm(arguc,arguv); return(1); }
else    if (strcmp(OO,"RISKSIMU")==0) { muste_risksimu(arguc,arguv); return(1); }
else    if (strcmp(OO,"PISO")==0) { muste_piso(arguc,arguv); return(1); }
else    if (strcmp(OO,"SURVIVAL")==0) { muste_survival(arguc,arguv); return(1); }
else    if (strcmp(OO,"LIST")==0) { muste_list(g,parm); return(1); }
else    if (strcmp(OO,"RPLOT")==0 || strcmp(OO,"RHISTO")==0) { muste_rplot(sur_session); return(1); }
else    if ((strcmp(OO,"PLOT")==0) || strcmp(OO,"HISTO")==0)			
			{ muste_plot(arguc,arguv); return(1); }
else    if ( // KV
           (strcmp(OO,"INDEX")==0) ||
           (strcmp(OO,"DIR")==0) ||
           (strcmp(OO,"SEARCH")==0) ||
           (strcmp(OO,"DD")==0) ||
           (strcmp(OO,"WHERE")==0) ||
           (strcmp(OO,"TREE")==0) ||
           (strcmp(OO,"DM")==0)
          )
          {
            if (strcmp(OO,"DD")==0) muste_dd_call=TRUE; // RS 30.5.2014
          	muste_undo=FALSE; // RS 9.10.2012
            muste_desktop(sur_session);
            muste_undo=TRUE; // RS 9.10.2012
            muste_dd_call=FALSE;
            return(1);
          }
else    if (strcmp(OO,"MAGIC")==0) { muste_magic(sur_session); return(1); } // RS          
else    if (strcmp(OO,"XALL")==0) { muste_xall(sur_session); return(1); } // KV
else    if (strcmp(OO,"BURT")==0) { muste_burt(sur_session); return(1); } // KV
else    if (strcmp(OO,"RELIAB")==0) { muste_reliab(sur_session); return(1); }  // KV
else    if (strcmp(OO,"MOVREG")==0) { muste_movreg(sur_session); return(1); }  // KV
else    if (strcmp(OO,"LOWESS")==0) { muste_lowess(sur_session); return(1); }  // JP (KV)
else    if (strcmp(OO,"HCLUSTER")==0) { muste_hcluster(sur_session); return(1); }  // FÅ (KV)
else    if (strcmp(OO,"MTAB")==0) { muste_mtab(arguc,arguv); return(1); }  // MK (RS)
else    if (strcmp(OO,"LINREG")==0) { muste_linreg(sur_session); return(1); } // SM
else    if (strcmp(OO,"REGDIAG")==0) { muste_regdiag(sur_session); return(1); } // SM
else    if (strcmp(OO,"STAT")==0) { muste_stat(sur_session); return(1); } // SM
else    if (strcmp(OO,"STATMSF")==0) { muste_statmsf(sur_session); return(1); } // SM
else    if ((strcmp(OO,"ESTIMATE")==0) || (strcmp(OO,"DER")==0))  // SM
            {
              muste_estimate(sur_session);
              return(1);
             }
else    if ((strcmp(OO,"LOADM")==0) || (strcmp(OO,"POSDIR")==0))
            {
              muste_loadm(arguc,arguv);
              return(1);
            }
else    if (strcmp(OO,"CLASSIFY")==0) { muste_classify(sur_session); return(1); } //SM
else    if (strcmp(OO,"FACTA")==0) { muste_facta(sur_session); return(1); }  // SM
else    if (strcmp(OO,"ROTATE")==0) { muste_rotate(sur_session); return(1); }  // SM
else    if (strcmp(OO,"INTREL")==0) { muste_intrel(sur_session); return(1); }  // SM
else    if (strcmp(OO,"COMPARE")==0) { muste_compare(sur_session); return(1); }  // SM
else    if (strncmp(OO,"TAB",3)==0) { op_tab(OO); return(1); } // SM
else    if (strcmp(OO,"MNSIMUL")==0) { muste_mnsimul(sur_session); return(1); }  // SM
else    if (strcmp(OO,"CORRESP")==0) { muste_corresp(sur_session); return(1); }  // SM
else    if (strcmp(OO,"CANON")==0) { muste_canon(sur_session); return(1); }  // SM
else    if (strcmp(OO,"LINCO")==0) { muste_linco(sur_session); return(1); }  // SM
else    if (strcmp(OO,"TRANSFORM")==0) { muste_transform(sur_session); return(1); }  // SM
else    if (strcmp(OO,"VARSTAT")==0) { muste_varstat(sur_session); return(1); }  // SM
else    if (strcmp(OO,"SER")==0) { muste_ser(sur_session); return(1); }  // SM
else    if (strcmp(OO,"GENREG")==0) { muste_genreg(sur_session); return(1); }  // SM
else    if (strcmp(OO,"POWERS")==0) { muste_powers(sur_session); return(1); }  // SM
else    if (strcmp(OO,"LSCAL")==0) { muste_lscal(sur_session); return(1); }  // SM
else    if (strcmp(OO,"DIST")==0) { muste_dist(sur_session); return(1); }  // SM
else    if (strcmp(OO,"DISTV")==0) { muste_distv(sur_session); return(1); }  // SM
else    if (strcmp(OO,"QUANTA")==0) { muste_quanta(sur_session); return(1); }  // SM
else    if (strcmp(OO,"CLUSTER")==0) { muste_cluster(sur_session); return(1); }  // SM
else    if (strcmp(OO,"CLASSI")==0 || strcmp(OO,"MAHAL")==0) { muste_classi(sur_session); return(1); }  // SM
else    if (strcmp(OO,"MNTEST")==0) { muste_mntest(sur_session); return(1); }  // SM
else    if (strcmp(OO,"MULTVAR")==0) { muste_multvar(sur_session); return(1); }  // SM
else    if (strcmp(OO,"CORRTEST")==0) { muste_corrtest(sur_session); return(1); }  // SM
else    if (strcmp(OO,"DCLUSTER")==0) { muste_dcluster(sur_session); return(1); }  // SM
else    if (strcmp(OO,"CORRMV")==0) { muste_corrmv(sur_session); return(1); }  // SM
else    if (strcmp(OO,"MINSTAT")==0) { muste_minstat(sur_session); return(1); }  // SM
else    if (strcmp(OO,"T2TEST")==0) { muste_t2test(sur_session); return(1); }  // SM
else    if (strcmp(OO,"COVTEST")==0) { muste_covtest(sur_session); return(1); }  // SM
else    if (strcmp(OO,"XCORR")==0) { muste_xcorr(sur_session); return(1); }  // SM
else    if (strcmp(OO,"FORECAST")==0) { muste_forecast(sur_session); return(1); }  // SM
else    if (strcmp(OO,"SMOOTH")==0) { muste_smooth(sur_session); return(1); }  // SM
else    if (strcmp(OO,"RUNTEST")==0) { muste_runtest(sur_session); return(1); }  // SM
else    if (strcmp(OO,"ROBREG")==0) { muste_robreg(sur_session); return(1); }  // Reino Siren, SM
else    if (strcmp(OO,"LOGMEAN")==0) { muste_logmean(sur_session); return(1); }  // SM
else    if (strcmp(OO,"RNDTEST")==0) { muste_rndtest(sur_session); return(1); }  // SM
else    if (strcmp(OO,"MARKOV")==0) { muste_markov(sur_session); return(1); }  // SM
else    if (strcmp(OO,"LUE")==0) { muste_lue(sur_session); return(1); }  // SM
else    if (strcmp(OO,"GEOM")==0) { muste_geom(sur_session); return(1); }  // SM
else    if (strcmp(OO,"COMB")==0) { muste_comb(sur_session); return(1); }  // SM
else    if (strncmp(OO,"EGYPT",5)==0) { muste_comb(sur_session); return(1); }  // SM
else    if (strncmp(OO,"SIMPLEX",5)==0) { muste_simplex(sur_session); return(1); }  // SM

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
           (strcmp(OO,"GET")==0) ||
           (strcmp(OO,"R")==0) || (strcmp(OO,"SBAR")==0) ||
           (strcmp(OO,"MENU")==0) || (strcmp(OO,"HEADLINE")==0) ||
           (strcmp(OO,"WORDS")==0) || (strcmp(OO,"CHARS")==0) ||
           (strcmp(OO,"THEME")==0) || (strcmp(OO,"INFOBAR")==0) ||
           (strcmp(OO,"REVERSE")==0) || (strncmp(OO,"TRANSPO",7)==0)
           )
//        if (strcmp(op,"EDI2")==0)
                {
                muste_ediop(sur_session);
                return(1);

//              if (i==1) return(1);
                }

else    if (strncmp(OO,"TCH",3)==0) { muste_touch(arguc,arguv); return(1); }

         // RS GPLOT added to avoid some sucro errors with Survo tour
else    if (strcmp(OO,"GPLOT")==0 || strcmp(OO,"GHISTO")==0 || strcmp(OO,"HISTOG")==0 ) {  //  && etu==2

extern int muste_rplotcall;
muste_rplotcall=FALSE; // RS 10.1.2013
op_gplot(op);
// muste_fixme("FIXME: GPLOT not implemented!\n"); // RS FIXME

return(1); }

found=0;
return(found);

}

static int op_tab(char *OO) // 14.7.2011/SM
        {
        int i;
        char x[LLENGTH], *w[1];

        if (strcmp(OO,"TABTEST")==0)
            { muste_tabtest(sur_session); return(1); }
        if (strlen(OO)>3)
            { muste_tabs(sur_session); return(1); }

        i=spec_find("VARIABLES",x,LLENGTH-1);
        if (i<0 || *x==EOS)  // 16.12.2009
            {
            sur_print("\nUsage: TAB <data>,L                     ");
            sur_print("\nVARIABLES=<list of classifiers> missing!");
            WAIT; return(-1);
            }
        i=split(x,w,1);
        if (strchr(w[0],':')!=NULL)
            muste_tabb(sur_session);
        else
            muste_tab(sur_session);
        return(1);
        }

