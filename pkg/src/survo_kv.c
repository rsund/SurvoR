/* miscellaneous functions by KV (1993-)
   converted for Muste 1.5.2011/KV (8.5.2011)
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
