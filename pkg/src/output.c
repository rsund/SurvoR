/* output.c 31.8.1985/SM (22.3.1986) (3.2.1996)

    output_open(file);
    output_close(file);
    output_line(string,file,editline);   (alunperin wline)
*/

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <string.h>
#include "survo.h"

extern char *z;
extern int ed1,ed2;
extern int r2,c2;
extern char sbuf[];
extern int output_level;

FILE *output_file;


int sur_print(char *x)
{
    Rprintf(x);
    return(1);
}

int output_open(char *file)
        {
        if (file==NULL || *file==EOS) return(1);
        output_file=fopen(file,"at");
        if (output_file==NULL)
            {
            sprintf(sbuf,"\nCannot open output file/device %s",file); sur_print(sbuf);
            WAIT; return(-1);
            }
        return(1);
        }

int output_close(char *file)
        {
        if (file==NULL || *file==EOS) return(1);
        fclose(output_file);
        return(1);
        }

int output_line(char *string,char *file,unsigned int editline)
        {
        int i,lev;
        unsigned int j;
        char s[LLENGTH];

        if (*file==EOS && editline==0) return(1); /* 3.2.1996 */
        strcpy(s,string);
        lev=strlen(s);
        if (editline>0 && editline<=r2)
            {
            for (i=0, j=(editline-1)*ed1+1; i<c2; ++i, ++j)
                {
                if (i<lev) z[j]=s[i]; else z[j]=' ';
                }
            }
        if (output_level==0) return(1);
        while (s[lev-1]==' ' && lev>0) s[--lev]=EOS;
        sprintf(sbuf,"\n%s",s);
        if (output_level==2) sur_print(sbuf);
        if (file==NULL || *file==EOS) return(1);
        s[lev]='\n'; s[lev+1]=EOS;
        fputs(s,output_file);
        return(1);
        }
