#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static int rand_in_use=0;
static FILE *seedfile;
static unsigned long i1,i2;

extern char **spb;



void seedfile_err(char *s)
        {
        sprintf(sbuf,"\nSeed file error in %s!",s);
        sur_print(sbuf); WAIT;
        exit(1);
        }

int outseed()
        {
        int i;
        char x[LLENGTH];

        if (!rand_in_use) return(1);
        i=spfind("OUTSEED"); if (i<0) return(1);
        strcpy(x,spb[i]); if (strchr(x,':')==NULL) { strcpy(x,edisk); strcat(x,spb[i]); }
        seedfile=muste_fopen(x,"wb");
        if (seedfile==NULL) seedfile_err(x);
        fprintf(seedfile,"%lu %lu",i1,i2);  // RS FIXME 64bit
        fclose(seedfile);
        return(1);
        }

