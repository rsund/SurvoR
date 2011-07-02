/* "X all that apply" by K.Vehkalahti (9.5.1997)
   converted for Muste 25.6.2011/KV (25.6.2011)
 */

#include <stdio.h>
#include <stdlib.h>

#include "survo.h"
#include "survoext.h"
#include "survolib.h"

extern void muste_kv_s_disp();
extern void muste_kv_s_err();

static SURVO_DATA d;
/* RS REM
static char *spec_xall[]={ "!" };
static char **specs=spec_xall;
*/

static int check_parameters(void);
static int xall(void);
static void usage_info(void);

void muste_xall(char *argv)
{
    int i;

    s_init(argv);
    i=spec_init(r1+r-1); if (i<0) return;
    i=check_parameters(); if (i<0) return;
    xall();
    s_end(argv);
    return;
}

static int check_parameters(void)
{
    if (g>3) {
        if (word[1][0]=='?') {
            usage_info();
            return -1;
        }
    } else {
        usage_info();
        return -1;
    }
    return 1;
}

static void usage_info(void)
{
    init_remarks();
    rem_pr("XALL <data>,<name>,<total>,<extra>");
    rem_pr("moves active observations in given contiguous variables in <data>");
    rem_pr("so that value i is moved to var[i], where i=1,...,<total>");
    rem_pr("and the name of var[1] is given as <name>.");
    rem_pr("By default, values of one variable only is moved at a time.");
    rem_pr("Values outside the range {1,<total>} are skipped.");
    rem_pr("If there are some extra variables after each var[i], their values");
    rem_pr("can also be moved without using them as indexing variables. This");
    rem_pr("is achieved by the <extra> parameter, which is by default 1.");
    rem_pr("The <total> number of variables is always the number of indexing");
    rem_pr("variables only.");
    wait_remarks(2);
}

static int xall(void)
{
    int i,j,k;
    double x,y;

    int var1;
    int vartotal;
    int howmany;

    i=data_open(word[1],&d); if (i<0) return -1;
    i=mask(&d); if (i<0) return -1;
    i=conditions(&d); if (i<0) return -1;
    var1=varfind(&d,word[2]); if (var1<0) return -1;
    vartotal=atoi(word[3]);
    if (g>4) howmany=atoi(word[4]); else howmany=1;
    if (howmany<1) return -1;

    muste_kv_s_disp("\nConverting \"X ALL\" responses in %d variables...", vartotal);
    muste_kv_s_disp("\nMoving values of %d variables at a time...", howmany);

    if ((var1+howmany*vartotal) > d.m) {
        muste_kv_s_err("Not enough variables after %s!", word[2]);
        return -1;
    }

    for (j=d.l1; j<=d.l2; j++) {
        if (unsuitable(&d,j)) continue;
        muste_kv_s_disp(" %d",j);
        for (i=(var1+howmany*vartotal-howmany); i>=var1; i-=howmany) {
            data_load(&d,j,i,&x);
            if (x==MISSING8) {
                data_save(&d,j,i,MISSING8); /* (data in edit field) */
                for (k=1; k<howmany; k++) { /* 31.5.97 */
                    data_save(&d,j,i+k,MISSING8);
                }
                continue;
            }
            if (howmany==1 && ((int)x == i)) continue;
            if (((int)x < 1) || ((int)x > vartotal)) continue; /* 20.5.97 */
            data_save(&d,j,i,MISSING8);
            data_save(&d,j,var1+howmany*(int)x-howmany,x);
            for (k=1; k<howmany; k++) { /* 20.5.97 */
                data_load(&d,j,i+k,&y);
                data_save(&d,j,i+k,MISSING8);
                data_save(&d,j,var1+howmany*(int)x-howmany+k,y);
            }
        }
    }
    data_close(&d);
    return 1;
}
