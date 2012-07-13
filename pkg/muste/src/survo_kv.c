#include "muste.h"
/* miscellaneous functions by KV (1993-)
   converted for Muste 1.5.2011/KV (8.5.2011) (15.12.2011)
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

void muste_kv_s_err(char *, ...);
void muste_kv_s_disp(char *, ...);
void muste_kv_usage_info(void);
int muste_kv_space_split(char *, char **, int);
int muste_kv_edline(char *, int, int);

void muste_kv_s_err (char *fmt, ...)
{
  char bigbuffer[LLENGTH];
  va_list ap; /* points to each unnamed arg in turn */
  va_start(ap, fmt);
  vsprintf(bigbuffer, fmt, ap);
  va_end(ap);
  sur_print("\n");
  sur_print(bigbuffer);
  WAIT;
}

void muste_kv_s_disp (char *fmt, ...)
{
  char bigbuffer[LLENGTH];
  va_list ap; /* points to each unnamed arg in turn */
  va_start(ap, fmt);
  vsprintf(bigbuffer, fmt, ap);
  va_end(ap);
  sur_print(bigbuffer);
}

void muste_kv_usage_info(void) {
    strcpy(sbuf, word[0]);
    muste_strupr(sbuf);
    muste_kv_s_err("See %s?", sbuf);
}

int muste_kv_space_split(char *row, char **wrd, int max)
{
    int i,k,prev,len;

    k=prev=0;
    len=strlen(row);
    for (i=0; i<len; i++) {
        if (row[i]==' ') {
            if (prev) {
                row[i]='\0';
                if (++k>=max) return max;
                prev=0;
            }
        } else {
            if (!prev) {
                wrd[k]=row+i;
                prev=1;
            }
        }
    }
    if (prev) k++;
    return k;
}

int muste_kv_edline(char *label, int j, int error)
// more pedantic wrap for edline2() - needed at least in INDEX and RELIAB
{
    int i, len, word_int, colonfirst, lab, minus1, minus2, minus3, cur1, end1;
    char *cur, *end, *plus, *minus, *colon;
    char labl[LNAME];

    len=strlen(label);
    word_int=atoi(label);
    strcpy(labl,label);
    muste_strupr(labl);
    cur=strstr(labl, "CUR");
    end=strstr(labl, "END");
    plus=strchr(label, '+');
    minus=strchr(label, '-');
    colon=strchr(label, ':');
    colonfirst=!strncmp(label, ":", 1);
    lab=(!word_int && len==1);   /* e.g. A or \ or %, but not * ! */
    lab=(lab && strncmp(label, "*", 1));
    cur1=(cur && !colon);          /* e.g. CUR+3 */
    end1=(end && !colon);          /* e.g. END-2 */
    minus1=(minus && colonfirst);  /* e.g. :-3   */
    minus2=(minus && !colon);      /* e.g. A-1   */
    minus3=(minus1 || minus2);
    if (word_int || lab || cur1 || end1 || plus || minus3) {
        i=edline2(label,j,error);
        return i;
    } else return 0;
}
