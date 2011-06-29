/*  !colx.c 29.12.1985/SM (26.5.1995)
    COLX col1,col2   exchanges columns in the current edit field
                     below the current line.
         0,C2  are default parameters
    + other functions
 */

#include "survo.h"
#include "survoext.h"
#include "survolib.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
// RS REM #include <types.h>
// RS REM #include <timeb.h>

extern char *parm[];
extern int g;
extern int soft_vis;

static FILE *bin,*bin2;


static int find_text_dimensions()
    {
    int i,j,j1,j2,jmax;
    int max;
    char x[LLENGTH];

    j1=edline2(parm[2],1,1); if (!j1) return(1);
    j2=edline2(parm[3],j1,1); if (!j2) return(1);
    max=0; jmax=j1;
    for (j=j1; j<=j2; ++j)
        {
        edread(x,j);
        i=strlen(x)-1;
        while (i>0 && x[i]==' ') --i;
        if (i>max) { max=i; jmax=j; }
        }
    j=r1+r-1; edread(x,j);
    i=strlen(x)-1;
    while (i>0 && x[i]==' ') --i;
    sprintf(sbuf,"/ width=%d lines=%d first_line=%d longest_line=%d",max,j2-j1+1,j1,jmax);
    edwrite(sbuf,j,i+2);
    return(1);
    }


        /* COLX Si,j / column j to shadow characters of column i */
static int move_to_shad()
        {
        int col1,col2;
        int i,j;
        char x[LLENGTH];
        char ch;

        col1=atoi(parm[1]+1);
        col2=atoi(parm[2]);
        for (j=r1+r; j<=r2; ++j)
            {
            edread(x,j);
            ch=x[col2];
            if (ch==' ') continue;
            if (zs[j]==0)
                {
                i=creatshad(j);
                if (i<0) return(-1);
                }
            edread(x,zs[j]);
            x[col1]=ch;
            edwrite(x,zs[j],0);
            }
        return(1);
        }



static int from_line_to_column()  /* COLX Lline,column,first_line */
        {
        int lin,col,first;
        char x[LLENGTH];
        char y[LLENGTH];
        int len,i,j;

        lin=edline2(parm[1]+1,1,1);
        if (lin==0) return(-1);
        col=atoi(parm[2]);
        first=edline2(parm[3],1,1);
        if (first==0) return(-1);
        edread(x,lin);
        len=c2; while (x[len]==' ' && len>0) --len;
        i=1; j=first;
        for (i=1; i<=len; ++i)
            {
            if (j>r2) break;
            edread(y,j);
            y[col]=x[i];
            edwrite(y,j,0);
            ++j;
            }
        return(1);
        }



static int decode_shadows()
        {
        int i,m1,m2;
        char nimi[LLENGTH];

Rprintf("FIXME: COLX s (decode_shadows) not implemented!"); // RS FIXME
/* 
        if (parm[1][1]==EOS)
            {
            if (g==2)
                {
                for (i=0; i<256; ++i) shad_active[i]=(unsigned char)i;
                shad_off=0;
                return(1);
                }
            strcpy(nimi,parm[2]);
            if (strchr(nimi,':')==NULL)
                {
                sprintf(nimi,"%sSYS\\",survo_path);
                strcat(nimi,parm[2]);
                }
            if (strchr(nimi+strlen(nimi)-4,'.')==NULL)
                strcat(nimi,".BIN");
            bin=fopen(nimi,"rb");
            if (bin==NULL)
                {
                sprintf(sbuf,"\nCannot open file %s!",nimi);
                sur_print(sbuf); WAIT; return(1);
                }
            for (i=0; i<256; ++i) shad_active[i]=(unsigned char)getc(bin);
            shad_off=1;
            return(1);
            }
        m1=atoi(parm[1]+1);
        m2=atoi(parm[2]);
        if (m1<0 || m2<0 || m1>255 || m2>255) return(1);
        shad_active[m1]=m2;
        shad_off=1;
*/
        return(1);
        }


static int set_cpu_speed()
        {
Rprintf("FIXME: set_cpu_speed not implemented!\n");
/* RS NYI
        long l,ll,n;
        struct timeb tb;
        long alku,alkums,loppums,sek;
        char *p;
        extern int computation_speed;

        p=parm[1]+1;
        if (*p=='?')
            {
            sprintf(sbuf,"\ncpu_speed=%ld",cpu_speed);
            sur_print(sbuf);
            WAIT; return(1);
            }
        if (*p=='!')
            {
            sprintf(sbuf,"%ld@",cpu_speed);
            strcat(tut_info,sbuf);
            return(1);
            }
        if (*p!=EOS)
            {
            cpu_speed=atol(p); return(1);
            }
        n=8*2000000L;
        ftime(&tb); alku=tb.time;
        alkums=tb.millitm;
        ll=0L; for (l=0L; l<n; ++l) ++ll;
        ftime(&tb); sek=tb.time;
        loppums=tb.millitm;
        cpu_speed=(1000L*(sek-alku)+loppums-alkums)>>1;
*/
        return(1);
        }


static int disp_window_size()
    {
    int j;

    sprintf(sbuf,"COLX w %d %d (window size)",r3,c3);
    j=r1+r-1;
    edwrite(space,j,1);
    edwrite(sbuf,j,1);
    return(1);
    }


static int tell_soft_vis() // 8.2.2001
    {
    int j;

    sprintf(sbuf,"COLX V %d",soft_vis);
    j=r1+r-1;
    edwrite(space,j,1);
    edwrite(sbuf,j,1);
    return(1);
    }


int muuta_apu_tiedostoa(int mode)
// mode: 1=replace 2=del  3=APU tai APUDEL
    {
    int i,len;
    char x[LLENGTH];
//  char apu[LLENGTH];
    char *p,*q;
    int ok;
    char rivi[LLENGTH];
    extern char current_setup[];
    char apu[4];


// printf("\ncurrent_setup=%s|",current_setup); WAIT;

    strcpy(apu,"APU");
    edread(x,r1+r-1);
    if (mode==3) // command SYS or SYSDEL
        {
        strcpy(apu,"SYS");
        if (muste_strcmpi(parm[0],"SYSDEL")==0) mode=2; else mode=1;
        }
    p=strstr(x,apu); if (p==NULL) return(1);
    p+=3; if (mode==2) p+=3; // APUDEL tai SYSDEL
    while (*p==' ') ++p;
    if (*p==EOS) return(1);
    i=strlen(p)-1; while (p[i]==' ') p[i--]=EOS;
    bin=fopen(current_setup,"rb");
    sprintf(sbuf,"%sAPU.TMP",etmpd);
    bin2=fopen(sbuf,"wb");
    while (1)
        {
        i=getc(bin);
        if (feof(bin)) break;
        putc(i,bin2);
        }
    fclose(bin2);
    fclose(bin);
    if (mode==2 && strchr(p,'=')==NULL) strcat(p,"=");

    q=strchr(p,'='); len=q-p+1; // 15.7.2006

    bin2=fopen(sbuf,"rt");
    bin=fopen(current_setup,"wt");
    ok=0;
    while (1)
        {
        fgets(rivi,200,bin2);
        if (feof(bin2)) break;
// printf("\nrivi=%s| p=%s| len=%d|",rivi,p,len);

        if (strncmp(p,rivi,len)==0)
           {
// printf("\nOK!");
// getck();
           ok=1;
           if (mode==2) continue; // APUDEL
           else { strcpy(rivi,p); strcat(rivi,"\n"); }
           }
        fprintf(bin,"%s",rivi);
        }
    if (!ok)
        {
        fprintf(bin,"%s\n",p);
        }
    fclose(bin);
    fclose(bin2);

/********************
SHOW G:\E\U\SURVO.APU
***********************/
    return(1);
    }

static int sijoita_Survo() // 21.10.2001
    {
    char x[LLENGTH];
    int j;

    strcpy(x,parm[2]);
    unsubst_survo_path_in_editor(x);
    j=r1+r-1;
    edwrite(space,j,1);
    edwrite(x,j,1);
    return(1);
    }


int op_colx()
        {
        int j,col1,col2;
        char x[LLENGTH];
        char ch;

        if (strcmp(parm[1],"TEXTDIM")==0)
            {
            find_text_dimensions();
            return(1);
            }

        /* COLX R43,C80 */
        if (g>1 && *parm[1]=='R')
            {
            r3=atoi(parm[1]+1);
            if (g>2 && *parm[2]=='C') c3=atoi(parm[2]+1);
            return(1);
            }

        /* COLX Si,j / column j to shadow characters of column i */
        if (*parm[1]=='S')
            {
            move_to_shad();
            return(1);
            }
        if (*parm[1]=='L')
            {
            from_line_to_column();
            return(1);
            }
        if (*parm[1]=='s')
            {
            decode_shadows();
            return(1);
            }
        if (*parm[1]=='W')
            {
            tut_wait_c=atoi(parm[1]+1);
            return(1);
            }
        if (*parm[1]=='C')
            {
            set_cpu_speed();
            return(1);
            }
        if (strcmp(parm[1],"w")==0) // 14.12.2000
            {
            disp_window_size();
            return(1);
            }
        if (strcmp(parm[1],"V")==0) // 8.2.2001
            {
            tell_soft_vis();
            return(1);
            }

        if (strcmp(parm[1],"APU")==0) // 18.9.2001
            {
            muuta_apu_tiedostoa(1);
            return(1);
            }
        if (strcmp(parm[1],"APUDEL")==0) // 18.9.2001
            {
            muuta_apu_tiedostoa(2);
            return(1);
            }
        if (strcmp(parm[1],"UNSUBST")==0) // 21.10.2001
            {
            sijoita_Survo();
            return(1);
            }

        col1=0; col2=c2;
        if (g>1)
            { col1=atoi(parm[1]); if (col1<0 || col1>c2) return(1); }
        if (g>2)
            { col2=atoi(parm[2]); if (col2<0 || col2>c2) return(1); }

        for (j=r1+r; j<=r2; ++j)
            {
            edread(x,j);
            ch=x[col1]; x[col1]=x[col2]; x[col2]=ch;
            edwrite(x,j,0);
            }
        return(1);
        }
