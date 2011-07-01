/* output.c 31.8.1985/SM (22.3.1986) (3.2.1996)

    output_open(file);
    output_close(file);
    output_line(string,file,editline);   (alunperin wline)
*/

/* remark.c 25.12.1988/SM (22.3.1991)
*/

#include <R.h>
#include <Rinternals.h>
#include <stdio.h>
#include <string.h>
#include "survo.h"
#include "survolib.h"


extern char *z;
extern int ed1,ed2;
extern int r2,c2;
extern char sbuf[];
extern int output_level;

extern int r,r1,r2,r3,c3,sdisp;
extern unsigned char *shadow_code;
extern int scroll_line;
extern int space_break;
extern char *etmpd;

static FILE *sur_remarks;
static int rem_first_line, rem_line;
static int rem_edit_line;
static riveja_poimittu;

FILE *output_file;

/*
int sur_print(char *x)
{
    Rprintf(x);
    return(1);
}
*/

int output_open(char *file)
        {
        if (file==NULL || *file==EOS) return(1);
        output_file=muste_fopen(file,"at");
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

int init2_remarks()
        {
        char nimi[LLENGTH];
        int i;

        strcpy(nimi,etmpd); strcat(nimi,"SURVO.REM");
        sur_remarks=muste_fopen(nimi,"w+t");
        if (sur_remarks==NULL) return(-1);
        LOCATE(rem_first_line+1,1); rem_line=rem_first_line+1; PR_EUDL;
     for (i=0; i<r3+1-rem_first_line; ++i)
        sur_scroll_up(1,rem_first_line+3,1,r3+2,c3+8,(int)shadow_code[sdisp]);
        LOCATE(rem_first_line+1,1);
        return(1);
        }

void init_remarks()
        {
        space_break=0;
        rem_first_line=r;
        rem_edit_line=r1+r;
        scroll_line=2;
        riveja_poimittu=0;
        init2_remarks();
        }

int rem_pr(char *x)
        {
        sur_print("\n"); sur_print(x);
        ++rem_line; if (rem_line>=r3+1) --rem_first_line;
        fputs(x,sur_remarks);
        fputc((int)'\n',sur_remarks);
        return(1);
        }

int rem_load()
        {
        char x[LLENGTH];

        rewind(sur_remarks);
        while (1)
            {
            if (rem_edit_line==r2) break;
            edread(x,rem_edit_line);
            if (!empty_line(x+1,strlen(x+1)))
                {
                PR_EIN2;
                sur_print("\nNot enough empty lines!");
                sur_getch(); return(-1); // RS FIX exit
                }
            fgets(x,LLENGTH-1,sur_remarks);
            if (feof(sur_remarks)) break;
            x[strlen(x)-1]=EOS;
            edwrite(x,rem_edit_line,1);
            ++rem_edit_line;
            riveja_poimittu=1;
            }
        return(1);
        }

int wait_remarks(int k)
        {
        int m, paluu;

        sur_print("\n"); PR_EINV;
        if (k==1) sur_print("Next page by 'space'  |  Load lines by '+'  |  Interrupt by ENTER!");
        else sur_print("Load lines by '+'  |  Interrupt by ENTER!");
    /*  m=nextch("");    */
        m=sur_getch();
        paluu=FALSE;
        switch (m)
            {
          case CODE_RETURN:
                if (!riveja_poimittu) paluu=TRUE; // RS FIX exit
                break; 
          case '+': 
                if (rem_load()<0) paluu=TRUE; // RS FIX exit
                break;
            }
        fclose(sur_remarks);
        if (paluu) return(-1);
        init2_remarks();
        return(1);
        }
