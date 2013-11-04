#include "muste.h"
/* create.c 1.3.1986/SM (17.2.1993) (25.2.1995) (16.8.2007)
   FILE CREATE
   FILE UPDATE
   FILE STATUS <Survo data file>
   FILE INIT <Survo data file>,N  (N more missing observations)
   FILE REDUCE <data file>,M,N
   FILE EXPAND <data file>,M,N
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define PRMODE PR_EIN2

extern int special;
extern int r_soft;
extern void fi_init_save();

static int prind;

static SURVO_DATA_FILE dat;
static int m;
static int tulosrivi;

static SURVO_DATA d;

static SURVO_DATA datc;
static unsigned char code[256];

/* create */
static int fields_rivi;
static int n_fields;
static int actsar; /* aktivointisarakkeiden lkm */

// RS REM static char *pcre;
static char **fitext;
static char **varname;
static int *varlen;
static char **vartype;
static char *argv1;

static FILE *conds;
static FILE *temp;
static char *condspace;
static char **cond;

static int m_act;
static int m_pro;
static int m;
static int m_disp; /* # of var.disp.lines */
static int i1,i2;  /* 1st and last disp.var. */
static int col0;   /* first edit column on the screen */
static int n_col;  /* # of columns displayed */
static int col;    /* current column */
static int muuttuja; /* current variable */
static long n;
// RS REM static int muuttuja;
static int muutokset;
static char *tiedotus; /* ohjeteksti alarivillÑ */
static char lopetus[]="To stop, press EXIT!";
static char aktiv[]="Denote variables by - and A,X,Y etc. | Return MASK by F8 or VARS by F9!";
static char suoja[]="Denote protected variables by P. Stop by EXIT!";
static char skaala[]="Enter scale types: N=nominal, O=order, I=interval, R=ratio, -=no";
static char *message[64];
static char tiedosto[LNAME];
static char fieldno[9];
static int i0;
static int tila;
static int masknro=-1;
static int vain_selailu;
static char act_char[]="-"; /* 23.10.1994 */
static int *mask0;



/* 31.3.1991 */
static char huonot_merkit[]="0123456789.+-*/^()\"\'<>=[]{}#@:;,";
static char huonot_vmerkit[]="+-*/^()\"\'<>=[]{}@:;,";

/* RS REM
static char *specs0[]={ "NEWSPACE", "FORMAT", "LIMITS", "BASE", "ALL", "GETVAR",
                 "PRIND", "!" };
static char **specs=specs0;
*/


// FILE COND <cond_file>,<cond>
static int file_cond()
    {
    int i,j,len;
    char x[LLENGTH];
    int n, tila;
    char *p,*q;
    char ehto[LNAME];
    char *s[5];
    char y[LLENGTH];
    char y2[LLENGTH];
    char *ehtosana[]={ "Ehto", "Condition" };
    int k;

    strcpy(x,word[2]);
    if (!muste_is_path(word[2]))    
        { strcpy(x,edisk); strcat(x,word[2]); }
    muste_append_path(x,".CND");  // RS 18.10.2013   
//    if (strchr(word[2],'.')==NULL) strcat(x,".CND");

    strcpy(ehto,word[3]);

    conds=fopen(x,"rt");
    if (conds==NULL)
        {
        sprintf(sbuf,"\nCannot open %s!",x); sur_print(sbuf);
        WAIT; return(-1);
        }
    strcpy(x,etmpd); strcat(x,"COND.TMP");
    temp=fopen(x,"wt");

    fgets(x,LLENGTH-1,conds);
    fgets(x,LLENGTH-1,conds);
    n=0; tila=0;
    while (!feof(conds))
        {
        fgets(x,LLENGTH-1,conds);
        if (feof(conds)) break;
        tila+=strlen(x)+2;
        fputs(x,temp);
        ++n;
        }

    muste_fclose(temp); muste_fclose(conds);
    cond=(char **)muste_malloc(n*sizeof(char **));
    condspace=(char *)muste_malloc(tila);

    strcpy(x,etmpd); strcat(x,"COND.TMP");
    temp=fopen(x,"rt");
    p=condspace;
    for (i=0; i<n; ++i)
        {
        cond[i]=p;
        fgets(x,LLENGTH-1,temp); len=strlen(x);
        x[len-1]=EOS;
        strcpy(p,x);
        p+=len;
        }

    muste_fclose(temp);
/******************************
for (i=0; i<n; ++i)
    Rprintf("\n%s|",cond[i]);
getch();
*******************************/
    for (i=0; i<n; ++i)
        {
        strcpy(x,cond[i]);
        split(x,s,1);
        if (strcmp(ehto,s[0])==0) break;
        }
    if (i==n) return(-1);

    k=tell_language()-1;
    j=r1+r;
    strcpy(x,cond[i]);
    split(x,s,5);
    switch(*s[2])
        {
      case 'N':
        sprintf(sbuf," %s:%s IND=%s,%s,%s",ehtosana[k],s[0],s[1],s[3],s[4]);
        edwrite(space,j,1);
        edwrite(sbuf,j,1);
        return(1);
      case 'S':
        strcpy(y,cond[i]);
        sprintf(sbuf," %s:%s CASES=%s:%s",ehtosana[k],s[0],s[1],y+(s[3]-x));
        edwrite(space,j,1);
        edwrite(sbuf,j,1);
        return(1);
      case 'L':
        sprintf(sbuf," %s:%s SELECT=%s",ehtosana[k],s[0],s[3]);
        edwrite(space,j,1);
        edwrite(sbuf,j,1);
        ++j;
        strcpy(y,s[3]);
        strcat(y,"!"); // lopun varmistus!
        p=y;
        while (*p)
            {
            if (strchr("!()+*",*p)!=NULL) { ++p; continue; }
            q=p;
            while (*q)
                {
                if (strchr("!()+*",*q)==NULL) { ++q; continue; }
                *q=EOS;
//          Rprintf("\nosaehto=%s",p); getch();
                for (i=0; i<n; ++i)
                    {
                    strcpy(x,cond[i]);
                    split(x,s,1);
                    if (strcmp(p,s[0])==0) break;
                    }
                if (i==n) return(-1);
                strcpy(x,cond[i]);
                split(x,s,5);
                if (*s[2]=='N')
                    sprintf(sbuf," %s=%s,%s,%s",s[0],s[1],s[3],s[4]);
                else if (*s[2]=='S')
                    {
                    strcpy(y2,cond[i]);
                    sprintf(sbuf," %s=%s:%s",s[0],s[1],y2+(s[3]-x));
                    }
                else return(-1);

                edwrite(space,j,1);
                edwrite(sbuf,j,1);
                ++j;
                p=q+1; break;
                }
            }




        }

    return(1);
    }


static void make_varn(char *t,char *s)
        {
        int i;

        for (i=0; i<8; ++i)
            {
            if (s[i]==' ') break;
            t[i]=s[i];
            }
        t[i]=EOS;
        }
        
static void tilavaj()
        {
        sur_print("\nNot enough memory!");
        WAIT;
        }        

static int create_tilat(int filen,int m,int l,int actsar,int textn)
        {
        int i;
        int t_teksti,t_fitext,t_nimet,t_varname,t_varlen,t_mtila,t_vartype;
        char *p;

        t_teksti=textn*ed1;
        t_fitext=textn*sizeof(char *);
        t_nimet=m*(l+1);
        t_varname=m*sizeof(char *);
        t_varlen=m*sizeof(int);
        t_mtila=m*(actsar+1+1);
        t_vartype=m*sizeof(char *);

        fitext=(char **)muste_malloc(t_fitext+t_teksti+1);
        if (fitext==NULL) { tilavaj(); return(-1); }
        varname=(char **)muste_malloc(t_varname+t_nimet);
        if (varname==NULL) { tilavaj(); return(-1); }
        varlen=(int *)muste_malloc(t_varlen);
        if (varlen==NULL) { tilavaj(); return(-1); }
        vartype=(char **)muste_malloc(t_vartype+t_mtila);
        if (vartype==NULL) { tilavaj(); return(-1); }
        p=(char *)fitext; p+=t_fitext;
        for (i=0; i<textn; ++i) fitext[i]=p+i*ed1;
        p=(char *)varname; p+=t_varname;
        for (i=0; i<m; ++i) varname[i]=p+i*(l+1);
        p=(char *)vartype; p+=t_vartype;
        for (i=0; i<m; ++i) vartype[i]=p+i*(actsar+1+1);
        return(1);
        }

        

/* FILE MAKE <nimi>,<m>,<n>,<X>,<type>,<l>,<actsar>
   0    1     2      3   4   5   6      7   8
*/
static int fmake()
        {
        int i,j,l,filen,m,m1;
        long n,npros;
        char x[LLENGTH],*osa[2];
        char name_start[9];
        int actsar;
        int type;
        char stype;
        int newspace;
        int pros;
        char **fitext;
        char *privi[1];
        int fitextn, fitextlen;
        char form_limits[LNAME];
        char base[8];
        int i_base;
        char varn[3][9];
        int prind=0;

        if (g<5)
            {
            sur_print("\nUsage: FILE MAKE <name>,m,n,X,type");
            WAIT; return(-1);
            }
        i=spec_init(r1+r-1);
        if (i<0) return(-1);

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        m=atoi(word[3]);
        n=atol(word[4]);
        npros=n/100; if (m*n<10000) npros=0;
        pros=1;

        strcpy(name_start,"X"); type=4; stype='4';
        if (g>5)
            {
            if (strlen(word[5])>7) word[5][7]=EOS;
            strcpy(name_start,word[5]);
            }
        if (g>6)
            {
            stype=*word[6]; type=atoi(word[6]);
            if (stype=='S') { type=atoi(word[6]+1); if (type<=0) type=1; }
            if (strchr("1248S",stype)==NULL)
                {
                sprintf(sbuf,"\nInvalid type %c of fields! (Must be 1,2,4,8, or S)",
                                             stype);
                sur_print(sbuf); WAIT; return(-1);
                }
            }
        filen=type*m;
        newspace=filen/4+20; m1=m+m/4+4;

        i=spfind("NEWSPACE");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            i=split(x,osa,2);
            newspace=atoi(osa[0]);
            m1=m+atoi(osa[1]);
            }
        filen+=newspace;
        l=64; actsar=7;

        if (g>7) l=atoi(word[7]);

        if (g>8) actsar=atoi(word[8]);
        if (actsar<3) actsar=3;

        *form_limits=EOS;
        i=spfind("FORMAT");
        if (i>=0)
            {
            sprintf(sbuf,"(%s)",spb[i]);
            strcpy(form_limits,sbuf);
            }
        i=spfind("LIMITS");
        if (i>=0)
            {
            sprintf(sbuf,"{%s}",spb[i]);
            strcat(form_limits,sbuf);
            }
        if (l<8+strlen(form_limits))
            {
            sprintf(sbuf,"\nLength of field name %d too small!",l);
            sur_print(sbuf); WAIT; return(-1);
            }

        *base=EOS; i_base=1;
        i=spfind("BASE"); /* BASE=011 -> X011, X012,... */
        if (i>=0)
            {
            strcpy(base,spb[i]);
            i_base=atoi(base);
            }
        i=create_tilat(filen,m,l,actsar,0);
        if (i<0) return(-1);

// Rprintf("actsar=%d",actsar); getch();

        for (i=0; i<m; ++i)
            {
            varlen[i]=type;

            strcpy(vartype[i]," A_");
            strncat(vartype[i],space,actsar-1-2); // 3.5.2001
            vartype[i][0]=stype;

            if (strlen(base)>1 && *base=='0')
                {
                sprintf(x,"%s%0*d",name_start,(int)strlen(base),i_base);
                }
            else
                sprintf(x,"%s%d",name_start,i_base);
            ++i_base;
            if (m==1) strcpy(x,name_start); // 24.5.2001 pelkkÑ X esim.
            for (j=strlen(x); j<l; ++j) x[j]=' '; x[l]=EOS;
            strcpy(varname[i],x);
            if (*form_limits) strcpy(varname[i]+9,form_limits);
            }


        fitextn=1;
        fitextlen=c2;
        sprintf(x,"File with %d fields and %ld cases created by FILE MAKE",
                            m,n);
        privi[0]=x;
        fitext=privi;
// Rprintf("1"); getch();
        i=fi_create(word[2],filen,m1,m,0L,l,actsar+5,fitextn,fitextlen,
                    fitext,varname,varlen,vartype);
// Rprintf("2");
        if (i<0 && etu==0)
            { sprintf(sbuf,"\nFile %s not created!",word[2]); sur_print(sbuf); WAIT; return(1); }

        sur_print("\n");
        sprintf(sbuf,"\nCreating data file %s with %d variables",
                           word[2],m);
        sur_print(sbuf);

        if (m>5)
            {
            make_varn(varn[0],varname[0]);
            make_varn(varn[1],varname[1]);
            make_varn(varn[2],varname[m-1]);

            sprintf(sbuf,"\n%s, %s,..., %s",varn[0],varn[1],
                                    varn[2]);
            sur_print(sbuf);
            }
        sprintf(sbuf,"\n%ld observations...",n);
        sur_print(sbuf);

        i=fi_open(word[2],&dat); if (i<0) return(-1);

        for (l=1L; l<=n; ++l)
            {
            fi_miss_obs(&dat,l);
            if (l==npros && prind)
                {
                sprintf(sbuf," %d%%",pros); sur_print(sbuf);
                ++pros;
                npros=pros*n/100;
/*****************************************
                if (kbhit())
                    {
                    i=getch();
                    break;
                    }
***********************************/
                }
            }
        fi_rewind(&dat);
        fi_puts(&dat,(char *)&n,sizeof(int),22); // RS ADD (char *) CHA 64-BIT sizeof(long)->sizeof(int) 22L -> 22
        dat.n=n;

        fi_close(&dat);

        return(1);
        }


/* FILE REDUCE <data>,m,n */
static int reduce()
        {
        int m,i;
        long n;

        if (g<4)
            {
            sur_print("\nUsage: FILE REDUCE <data>,m,n");
            sur_print("\n       reduces the Survo data file to m first fields");
            sur_print("\n       and n first records.");
            WAIT; return(1);
            }

        i=fi_open(word[2],&dat); if (i<0) return(-1);
        m=atoi(word[3]);
        if (m>0 && m<dat.m1)
            fi_puts(&dat,(char *)&m,2,20L); // RS 28.1.2013 (char *)
        if (g>=5)
            {
            n=atol(word[4]);
            if (n>=0L && n<dat.n)
                fi_puts(&dat,(char *)&n,4,22L); // RS 28.1.2013 (char *)
            }
        fi_close(&dat);
        return(1);
        }


static void check_varname(char *s)
        {
// RS REM        int i;
        char nimi[9];
        char *p;

        if (etu) return;

        strncpy(nimi,s,8); nimi[8]=EOS;

        if (strcmp(nimi,"IND     ")==0 ||       /* 14.12.1999 */
            strcmp(nimi,"CASES   ")==0 ||
            strcmp(nimi,"SELECT  ")==0 )
            {
            sur_print("\nWARNING!");
            sur_print("\nTo avoid confusions,");
            sur_print("\nIND, CASES, and SELECT not suitable as names of fields!");
            WAIT;
            }

        if (strchr(huonot_merkit,*s)!=NULL)
            {
            sur_print("\nWARNING!");
            sur_print("\nTo avoid confusions,");
            sprintf(sbuf,"\nnames of variables starting by characters %s   ",huonot_merkit);
            sur_print(sbuf);
            sprintf(sbuf,"\n(as name \"%s\" in this case) should be avoided!",nimi);
            sur_print(sbuf);
            WAIT;
            }
        p=nimi+1;
        while (*p)
            {
            if (strchr(huonot_vmerkit,*p)!=NULL)
                {
                sur_print("\nWARNING!");
                sur_print("\nTo avoid confusions,");
                sprintf(sbuf,"\nnames of variables having characters %s   ",huonot_vmerkit);
                sur_print(sbuf);
                sprintf(sbuf,"\n(as name \"%s\" in this case) should be avoided!",nimi);
                sur_print(sbuf);
                WAIT; break;
                }
            ++p;
            }
        p=strchr(nimi,' ');
        if (p==NULL) return;
        while (*p) { if (*p!=' ') break; ++p; }
        if (*p==EOS) return;

        sur_print("\nWARNING!");
        sur_print("\nSpaces (blanks) within the 8 first characters of names");
        sprintf(sbuf,"\n(as name \"%s\" in this case) should be avoided!",nimi);
        sur_print(sbuf);
        WAIT;
        }

static void number_error(int j,int k)
        {
        sprintf(sbuf,"\nError on edit line %d:",j); sur_print(sbuf);
        sprintf(sbuf,"\nIncorrect # of field (%d expected)",k); sur_print(sbuf);
        WAIT;
        }

static void field_error(int j)
        {
        char x[LLENGTH];
        int len;

        sprintf(sbuf,"\nIncomplete field definition on edit line %d",j); sur_print(sbuf);
        edread(x,j); len=strlen(x); while (x[len-1]==' ') x[--len]=EOS;
        sprintf(sbuf,"\n%s",x); sur_print(sbuf);
        sur_print("\nCorrect form:");
        sur_print("\n<number_of_field> <type> <field_length> <name>");
        WAIT;
        }


static int check_varlen(int varlen,char type,int j)
        {
        if (varlen<1)
            {
            sprintf(sbuf,"\nError on edit line %d:",j); sur_print(sbuf);
            sprintf(sbuf,"\nIncorrect field length %d on edit line %d!",varlen,j);
            sur_print(sbuf); WAIT; return(-1);
            }

        if (type=='S')
           {
/****************************** 16.8.2007
           if (varlen>64 && etu<2)
               {
               sur_print("\nWarning! Field lengths > 64 should be avoided!");
               WAIT;
               }
**********************************/
           return(1);
           }

        if (varlen==1 || varlen==2 || varlen==4 || varlen==8) return(1);
        sprintf(sbuf,"\nError on edit line %d:",j); sur_print(sbuf);
        sprintf(sbuf,"\nIncorrect field length %d for a numeric field!"
                    ,varlen); sur_print(sbuf);
        sur_print("\nPermitted field lengths are 1,2,4 and 8 bytes.");
        WAIT; return(-1);
        }


static int check_vartype(char *sana,int j)
        {
        if (strchr("SN1248",*sana)!=NULL) return(1);
        sprintf(sbuf,"\nIncorrect field type %c on edit line %d",*sana,j);
        sur_print(sbuf); WAIT;
        return(-1);
        }


static int fields_rivit()
        {
        int i,j;
        char rivi[LLENGTH], *sana[1];

        j=r1+r;
        while (j<=ed2)
            {
            edread(rivi,j);
            i=split(rivi+1,sana,1);
            if (i>0 && strncmp("FIELDS",muste_strupr(sana[0]),6)==0 ) break;
            ++j;
            }
        if (j>ed2)
            {
            sur_print("\nLine FIELDS missing!");
            WAIT; return(-1);
            }
        fields_rivi=j;
        while (j<=ed2)
            {
            edread(rivi,j);
            i=split(rivi+1,sana,1);
            if (i>0 && strncmp("END",muste_strupr(sana[0]),3)==0 ) break;
            ++j;
            }
        if (j>ed2)
            {
            sur_print("\nLine END missing!");
            WAIT; return(-1);
            }
        n_fields=j-fields_rivi-1;
        return(1);
        }


/* FILE CREATE <nimi>,<filen>,<m1>,<l>,<actsar>,<n>
   0    1       2      3       4    5   6        7
*/
static int create()
        {
        int i,j,k,filen,m1,l,m,textn;
        long n;
        char x[LLENGTH], *sana[4];
        int fipituus;
//        int prind=0;

        i=spec_init(r1+r-1);  // 26.4.2001
        if (i<0) return(-1);
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        if (g<3)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE CREATE <filename>,rec,m1,namelength,actcol,n");
            sur_print("\n parameters            rec,m1,... optional");
            sur_print("\nrec = record length in bytes (default 5*min/4+20)");
            sur_print("\nm1 = max # of fields (variables) (default 5*min/4+4)");
            sur_print("\nnamelength = max length of field names (default 64)");
            sur_print("\nactcol = # of mask columns (default 7)");
            sur_print("\nn = intial # of records (cases, observations) (default 0)");
            WAIT; return(-1);
            }
        i=fields_rivit(); if (i<0) return(-1);
        m=n_fields;

        if (g>3) filen=atoi(word[3]);
        else     filen=0; /* lasketaan myoh. */

        if (g>4) m1=atoi(word[4]);
        else     m1=m+m/4+4;

        if (g>5) l=atoi(word[5]);
        else     l=64;

        if (g>6) actsar=atoi(word[6]);
        else     actsar=7;
        if (actsar<3) actsar=3;

        if (g>7) n=atol(word[7]);
        else     n=0L;

        textn=fields_rivi-r1-r;
        create_tilat(filen,m,l,actsar,textn);
        for (i=0; i<textn; ++i)
            {
            edread(x,r1+r+i);
            strcpy(fitext[i],x+1);
            }
        fipituus=0;
        for (i=0; i<m; ++i)
            {
            j=fields_rivi+i+1;
            edread(x,j);
            k=split(x+1,sana,4);
            if (k<4)
                {
                field_error(j);
                return(-1);
                }
            k=atoi(sana[0]);
            if (k!=i+1) { number_error(j,i+1); return(-1); }
            k=check_vartype(sana[1],j);
            if (k<0) return(-1);
            strncpy(vartype[i],space,actsar+1); vartype[i][actsar+1]=EOS;
            vartype[i][1]='A';
            k=0; while (k<actsar+1 && sana[1][k])
                { vartype[i][k]=sana[1][k]; ++k; }
            if (vartype[i][0]=='N') vartype[i][0]=*sana[2];
            varlen[i]=atoi(sana[2]);
            k=check_varlen(varlen[i],vartype[i][0],j);
            if (k<0) return(-1);
            fipituus+=varlen[i];
            edread(x,j);
            strncpy(varname[i],sana[3],l); varname[i][l]=EOS;
            check_varname(varname[i]);
            }

        if (fipituus>filen)
            {
            if (filen==0) filen=fipituus+fipituus/4+20;
            else
                {
  sprintf(sbuf,"\nRecord length %d less than total length %d of fields already defined!",
                        filen,fipituus);
                sur_print(sbuf); WAIT; return(-1);
                }
            }
        if (m1<m)
            {
          sprintf(sbuf,"\nMax. # of fields %d less than # of fields %d already defined!",
                                m1,m); sur_print(sbuf);
            WAIT; return(-1);
            }

        if (l>64)
            {
sprintf(sbuf,"\nName length %d (greater than 64) may cause troubles in certain operations!",l);
            sur_print(sbuf);
            if (etu==0) { WAIT; }
            }
        if (actsar>20)
            {
            sprintf(sbuf,"\n# of mask columns %d suggested is exceptionally high!",actsar);
            sur_print(sbuf);
            if (etu==0) { WAIT; }
            if (actsar>64) return(1); // RS CHA exit(1);  /* 25.2.1995 */
            }
/*
        if ((long)m1*(long)l>32000L)
            {
            i=32000/m1; if (i<8) i=8;
            sprintf(sbuf,
"\nWhen max. # of fields (%d) is so high, a smaller length than %d (say %d) is recommended!",
                            m1,l,i);
            sur_print(sbuf);
            if (etu==0) { WAIT; }
            }
*/
        i=fi_create(word[2],filen,m1,m,n,l,actsar+5,textn,c2,
                    fitext,varname,varlen,vartype);
        if (i<0 && etu==0)
            { sprintf(sbuf,"\nFile %s not created!",word[2]); sur_print(sbuf); WAIT; }
        strcpy(active_data,word[2]);
        return(1);
        }


static int load_codes(char *codefile,unsigned char *code)
        {
        FILE *codes;
        int i;
        char x[LLENGTH];

        strcpy(x,codefile);
        if (!muste_is_path(x))
            { strcpy(x,survo_path); strcat(x,"SYS/"); strcat(x,codefile); } // RS CHA \\ -> /

        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            PR_EBLD;
            sprintf(sbuf,"\nCode conversion file %s not found!",x);
            sur_print(sbuf); WAIT; PR_ENRM; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose(codes);
        return(1);
        }

static int conv(unsigned char *sana)
        {
        int i;

        for (i=0; i<strlen((char *)sana); ++i) sana[i]=(unsigned char)code[(unsigned char)sana[i]];
        return(1);
        }

static int convert()
        {
        int i,vi;
// RS REM        char *p;
        long j;
// RS REM        double x;
        unsigned char sana[LLENGTH];
// RS REM        char label[17];

        if (g<4)
            {
            sur_print("\nFILE CONVERT converts the values of active string variables");
            sur_print("\nand active records of a Survo data according to a code file (.BIN)");
            sur_print("\nUsage:");
            sur_print("\nFILE CONVERT <Survo_data_file>,<code_file>");
            WAIT; return(1);
            }

        i=spec_init(r1+r-1); if (i<0) return(1);
        i=load_codes(word[3],code); if (i<0) return(1);
        i=data_open(word[2],&datc); if (i<0) return(1);
        sur_print("\n");
        i=mask(&datc); if (i<0) return(1);
        i=conditions(&datc); if (i<0) return(1);

        for (j=1L; j<=datc.n; ++j)    /* havainnot */
            {
            if (unsuitable(&datc,j)) continue;
            for (i=0; i<datc.m_act; ++i)
                {
                vi=datc.v[i];
                if (datc.vartype[vi][0]=='S')
                    {
                    data_alpha_load(&datc,j,vi,sana);
                    conv(sana);
                    data_alpha_save(&datc,j,vi,sana);
                    }
                }
            }
        data_close(&datc);
        return(1);
        }

static int maskload()
        {
        int i,j,k,h,kk;
// RS REM        char x[LLENGTH];
        char maskstr[LLENGTH];
        char mask[LLENGTH];
        char vars[LLENGTH];
        char ch;
        char *p;
        char nimi[9];

        if (g<3)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE MASKLOAD <data>,<string_of_maskchars>     ");
            sur_print("\nor                                             ");
            sur_print("\nFILE MASKLOAD <data>,<string_of_maskchars>,MASK");
            WAIT; return(1);
            }
        i=data_read_open(word[2],&d); if (i<0) return(1);
        if (d.type!=2) return(1);

        *maskstr=EOS;
        if (g>3) strcpy(maskstr,word[3]);

        k=sprintf(mask," MASK=");
        if (d.m>LLENGTH-7) return(1);
        for (i=0; i<d.m; ++i)
           {
           ch=d.d2.vartype[i][1];
           if (*maskstr && strchr(maskstr,ch)==NULL) ch='-';
           k+=sprintf(mask+k,"%c",ch);
           }

        *vars=EOS;
        i=1;
        if (g>4 && muste_strcmpi(word[4],"MASK")==0) i=0; // 19.10.2001
        if (*maskstr && i)
            {
            k=sprintf(vars," VARS=");
            for (i=0; i<strlen(maskstr); ++i)
                {
                for (h=0; h<d.m; ++h)
                    {
                    if (mask[h+6]==maskstr[i])
                      {
                      strcpy(nimi,d.varname[h]);
                      kk=7; while (kk>0 && nimi[kk]==' ') nimi[kk--]=EOS;
                      sprintf(sbuf,"%s(%c),",nimi,mask[h+6]);
                      if (strlen(vars)+strlen(sbuf)<=c3)
                          k+=sprintf(vars+k,"%s",sbuf);
                      else { *vars=EOS; break; }
                      }
                    }
                if (*vars==EOS) break;
                }
            vars[strlen(vars)-1]=EOS;
            }


        if (*vars!=EOS) p=vars; else p=mask;

        j=r1+r; if (j>r2+1) return(1);
        edwrite(space,j,1);
        edwrite(p,j,1);

        data_close(&d);
        return(1);
        }

static int putsaa()
        {
        LOCATE(r3+2,1); PR_ENRM; sprintf(sbuf,"%.*s",c3+7,space); sur_print(sbuf);
        return(1);
        }

static int osoita(int i)
        {
        LOCATE(i-i1+3,col0+col);
        return(1);
        }

static int var_disp(int i)
        {
        int h;
        char x[LLENGTH];

        sprintf(x,"%4d %c ",i+1,dat.vartype[i][0]);
        write_string(x,7,' ',i-i1+3,1);
        sprintf(x,"%.*s",n_col,dat.vartype[i]+1);
        write_string(x,n_col,'7',i-i1+3,8);
        h=c3-n_col; if (dat.l<h) h=dat.l;
        while (dat.varname[i][h-1]==' ') --h;
        sprintf(x," %.*s",h,dat.varname[i]);
        write_string(x,h+1,' ',i-i1+3,8+n_col);
        LOCATE(i-i1+3,col0+col);
        return(1);
        }


static int act_disp()
        {
        PR_EBLD; LOCATE(2,40); sprintf(sbuf,"M(active)=%3d",m_act); sur_print(sbuf);
        return(1);
        }

static int pro_disp()
        {
        PR_EBLD; LOCATE(2,55); sprintf(sbuf,"M(protected)=%3d",m_pro); sur_print(sbuf);
        return(1);
        }

static int col_disp()
        {
        PR_EBLD; LOCATE(2,30); sprintf(sbuf," Col=%2d",col+1); sur_print(sbuf);
        return(1);
        }

static int all_disp()
        {
        int i;
/*
 Rprintf("\ni1=%d m_disp=%d m=%d",i1,m_disp,m); getch();
*/      PR_ENRM; CLS;
        LOCATE(2,1); PR_EBLD;
        sprintf(sbuf,"Data file %s  M=%3d",active_data,m); sur_print(sbuf);
        act_disp(); pro_disp(); col_disp();
        PR_ENRM;
        i2=i1+m_disp-1; if (i2>m-1) i2=m-1;
        for (i=i1; i<=i2; ++i)
            {
            var_disp(i);
            }
        tiedotus=message[col+1];
        muuttuja=i1;
        osoita(muuttuja);
        return(1);
        }

static int nimen_nro(char *s)
        {
        int i;
        char t[9];

        strcpy(t,s);
        for (i=strlen(s); i<9; ++i) t[i]=' '; t[8]=EOS;

        for (i=0; i<dat.m; ++i)
            {
            if (strncmp(dat.varname[i],t,8)==0) return(i);
            }
        return(-1);
        }


static int kirjoitukseen()
        {
        int i;

        i=fi_to_write(tiedosto,&dat);
        if (i<0) return(-1);
        vain_selailu=0;
        return(1);
        }

static int varsrivit;
static int curvarsrivit;
static char varsch;
static int varsr;
static int fmask;

static int fmask_write() // RS 8.5.2013
    {
    char x[LLENGTH];
    char name[10];
    int i,j;
            
    curvarsrivit=1;
    masknro=col+1;

    strcpy(x,"MASKING");
    if (masknro>1)
        {
        sprintf(sbuf," (%d)",masknro);
        strcat(x,sbuf);
        }
    edwrite(space,r1+r+curvarsrivit-1,1);
    edwrite(x,r1+r+curvarsrivit-1,1);
    curvarsrivit++;  
    for (i=0; i<dat.m; ++i) 
        {       
        if (dat.vartype[i][masknro]=='-' || dat.vartype[i][masknro]=='_' || dat.vartype[i][masknro]==' ') continue;
        strncpy(name,dat.varname[i],8);
        for (j=0; j<=8; j++) if (name[j]==' ' || j==8) name[j]=EOS;
        strcpy(x,name);
        strcat(x,": ");
        j=10-strlen(x);
        while (j>0) { strcat(x," "); j--; }
        name[0]=dat.vartype[i][masknro];
        name[1]=EOS;
        strcat(x,name);
        edwrite(space,r1+r+curvarsrivit-1,1);
        edwrite(x,r1+r+curvarsrivit-1,1);
        curvarsrivit++;
        }
    strcpy(x,"OTHERS:   -");
    edwrite(space,r1+r+curvarsrivit-1,1);
    edwrite(x,r1+r+curvarsrivit-1,1);
    curvarsrivit++;
    strcpy(x,"END");
    edwrite(space,r1+r+curvarsrivit-1,1);
    edwrite(x,r1+r+curvarsrivit-1,1);
    curvarsrivit++;
    varsrivit=curvarsrivit;
    muutokset=0;
    return(1);    
    }

static int mask_write()
        {
        char x[LLENGTH];
        char name[10];
        char *p;
        int i,j=0,k=0;
        extern int insertl();

        curvarsrivit=1; varsch='!'; varsr=r;
        if (masknro<0) masknro=1; // RS 7.5.2013
        if (tila==3) masknro=col+1; // RS 7.5.2013
        if (tila==99)
        	{
        	masknro=col+1;
        	strcpy(x,"VARS=");     	
        	for (i=0; i<dat.m; ++i) 
        		{
        		if (dat.vartype[i][masknro]=='-' || dat.vartype[i][masknro]=='_') continue;
        		strncpy(name,dat.varname[i],8);
        		for (j=0; j<=8; j++) if (name[j]==' ' || j==8) name[j]=EOS;
        		strcat(x,name);
        		if (dat.vartype[i][masknro]!='A') 
        			{
        			name[0]='(';
        			name[1]=dat.vartype[i][masknro];
        			name[2]=')';
        			name[3]=EOS;
        			strcat(x,name);
        			}
        		strcat(x,",");
        		j=strlen(x);
        		if (j>c2-15)
        			{ 
        			if (varsrivit<=curvarsrivit) // RS 7.5.2013
        			    {
        			    edread(sbuf,r1+r+curvarsrivit-1);
        			    if (sbuf[0]=='*' && strncmp(sbuf+1,space,c2)==0 )
        			        {
        			        varsrivit++;
        			        }
        			    else
        			        {
        			        if (varsch=='!')
        			            {
                                LOCATE(4,1); PR_EBLK;
                                sur_print("Not enough space for VARS! Insert line(s)? (Y/N) ");
                                varsch=sur_getch();
                                }
                            if (varsch!='n' && varsch!='N')
                                {
                                varsr=r;
                                r+=varsrivit-1;
                                k=insertl();
                                r=varsr;
                                if (k<=0) varsch='n';
                                else varsrivit++;
                                }
                            if (varsch=='n' || varsch=='N')
                                {    
                                LOCATE(6,1); PR_EBLK;
                                sur_print("Not enough space for VARS! Returning MASK!");
                                WAIT;
                                varsrivit=1; curvarsrivit=1;
                                strcpy(x,"MASK=");
                                for (i=0; i<dat.m; ++i) x[i+5]=dat.vartype[i][masknro]; x[i+5]=EOS;
                                break;
                                }
        			        }
        		
        			    }
        			if (varsrivit>curvarsrivit) // RS 7.5.2013
        			    {
        			    strcat(x,"&");
        			    edwrite(space,r1+r+curvarsrivit-1-1,1);
		                edwrite(x,r1+r+curvarsrivit-1-1,1);
		                curvarsrivit++;
		                x[0]=EOS; j=1;
        			    }
        			}
        		}
        	x[j-1]=EOS;	        	
        	}
        else if (tila==2 || (tila==3 && masknro==1)) // RS 7.5.2013 tila==3
			{
			strcpy(x,"MASK=");
			for (i=0; i<dat.m; ++i) x[i+5]=dat.vartype[i][masknro]; x[i+5]=EOS;
        	}  
        else if (tila==3) // RS 7.5.2013
			{
			sprintf(x,"MASK=#%d",masknro);
        	}
        if (x[0]==EOS && curvarsrivit>1) // RS 7.5.2013
            {
            edread(sbuf,r1+r+curvarsrivit-2-1);
            p=strchr(sbuf,'&');
            if (p!=NULL)
                {
                *(p-1)=EOS;
                curvarsrivit--;
                strcpy(x,sbuf+1);
                }
            }         	        	     	
        edwrite(space,r1+r+curvarsrivit-1-1,1);
		edwrite(x,r1+r+curvarsrivit-1-1,1);
		if (varsrivit>1) // RS 7.5.2013
		    {
		    while (curvarsrivit<varsrivit)
		        {
		        edwrite(space,r1+r+curvarsrivit-1,1);
		        curvarsrivit++;
		        }
		    }
		muutokset=0;
		return(1);	
        }

static int mask_read()
        {
        char x[LLENGTH];
        char *parm[MAXPARM];
        char *p,*q;
        int i,j,k;
        char m;

        varsrivit=1;
        edread(x,r1+r-1);
        p=strstr(x+1,"VARS=");
        if (p==NULL)
        	{
			p=strchr(x+1,'='); if (p==NULL) return(1);
			++p;
			if (*p=='#')
				{
				masknro=atoi(p+1);
				if (masknro<1 || masknro>dat.extra-5)
					{
					sur_print("\nIllegal MASK #"); WAIT; return(-1);
					}
				if (masknro>1) // RS 7.5.2013	
                    {
                    tila=3; 
                    for (i=0; i<dat.m; ++i)
                        {
//  RS 7.5.2013                      m=dat.vartype[i][1];
                        dat.vartype[i][1]=dat.vartype[i][masknro];
//  RS 7.5.2013                      dat.vartype[i][masknro]=m;
                        }
                    }
				return(1);
				}
			i=0;
			while (*p!=' ' && i<dat.m) dat.vartype[i++][1]=*p++;
			while (i<dat.m) dat.vartype[i++][1]='-';
			return(1);
        	}
        else // RS 25.11.2012
        	{
        	p=strchr(x+1,'='); if (p==NULL) return(1);
			++p;
			varsrivit=1; q=strchr(p,'&'); // RS 7.5.2013
			while (q!=NULL) // RS 7.5.2013
			    {
			    edread(sbuf,r1+r+varsrivit-1);
			    i=strlen(sbuf)-1; while (sbuf[i]==' ' && i>0) { sbuf[i]=EOS; i--; }
			    *q=EOS;
			    strcat(p,sbuf+1);
			    varsrivit++;
			    q=strchr(p,'&');
			    }
			i=0; while (i<dat.m) dat.vartype[i++][1]='-';
			j=split(p,parm,MAXPARM);
			if (j<0) return(1);
			for (i=0; i<j; i++)
				{
				p=strchr(parm[i],'(');
				if (p==NULL) m='A';
				else { *p=EOS; m=*(p+1); }
//Rprintf("\nparm[%d]: %s, m: %c",i,parm[i],m);				
				k=0;
				while (k<dat.m)
					{
//Rprintf("\nk: %d, %8s | %s, pit: %d",k,dat.varname[k],parm[i],strlen(parm[i]));				
					if (strncmp(dat.varname[k],parm[i],strlen(parm[i]))==0) break;
					k++;
					}  
				if (k<dat.m) dat.vartype[k][1]=m;
				}
        	}
        return(1); // RS 28.1.2013
        }

static int laske_akt()
        {
        int i;

        m_act=m_pro=0;
        for (i=0; i<m; ++i)
            {
            if (dat.vartype[i][1]!='-') ++m_act;
            if (dat.vartype[i][2]=='P') ++m_pro;
            }
        return(1);
        }

static int k_copy()
        {
        char kysymys[64];
        char vastaus[3];
        int i,col2;
        char cc;

        putsaa();
        LOCATE(r3+2,1); PR_EBLD;
        strcpy(kysymys,"Column "); muste_itoa(col+1,kysymys+7,10);
        strcat(kysymys," to be exchanged with column? ");
        *vastaus=EOS;
        prompt(kysymys,vastaus,2);
        col2=atoi(vastaus)-1;
        if (col2<0 || col2>n_col-1 || col2==col)
            { putsaa(); osoita(muuttuja); return(1); }
        for (i=0; i<m; ++i)
            {
            cc=dat.vartype[i][col+1];
            dat.vartype[i][col+1]=dat.vartype[i][col2+1];
            dat.vartype[i][col2+1]=cc;
            }
        ++muutokset;
        putsaa();
        laske_akt();
        all_disp();
        return(1);
        }



static int act_save(SURVO_DATA_FILE *s)
        {
        int i;

        if (masknro>0)
            for (i=0; i<dat.m; ++i)
                {
                m=(*s).vartype[i][1];
                (*s).vartype[i][1]=(*s).vartype[i][masknro];
                (*s).vartype[i][masknro]=m;
                }

        (*s).mode=0;
        for (i=0; i<(*s).m; ++i)
            {
            fi_puts(s,(*s).vartype[i],(*s).extra-4,
              (long)((*s).var+4L+(long)i*((long)(*s).l+(long)(*s).extra)));
            }
        return(1);
        }


static int down()
        {
        if (muuttuja<i2) { ++muuttuja; osoita(muuttuja); return(1); }
        if (i2<m-1)
            {
            SCROLL_UP(2,i2-i1+2,1);
            ++i1; ++i2; ++muuttuja;
            var_disp(muuttuja);
            }
        return(1);
        }


/* achelp.c 21.2.1986/SM (24.10.1994)
   FILE ACTIVATE (help)
*/

static int k_help()
        {
// RS REM        int i;
        extern char *key_label[];

        labels();
        PR_ENRM; CLS;
        PR_EINV;
        sur_print("\nKey codes in FILE ACTIVATE:");
        sur_print("\n");
        PR_ENRM;
        sur_print("\nFILE ACTIVATE permits the user to alter the attributes of the variables");
        sur_print("\nby editing the mask columns displayed in front of variable descriptions.");
        sur_print("\nCol. 1  is for activation.");
        sur_print("\nCol. 2  is for protection.");
        sur_print("\nCol. 3  is for scale type.");
        sur_print("\nRemaining columns can be used for storing of alternative masks.");
        sur_print("\n");

        PR_EINV; sprintf(sbuf,"\n%s (F8)",key_label[CODE_EXIT]); sur_print(sbuf); PR_ENRM;
        sur_print(" Return back to SURVO MM EDITOR");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_NEXT]); sur_print(sbuf); PR_ENRM;
        sur_print(" Next page");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_PREV]); sur_print(sbuf); PR_ENRM;
        sur_print(" Previous page");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_RETURN]); sur_print(sbuf); PR_ENRM;
        sur_print(" or ");
        PR_EINV; sprintf(sbuf,"%s",key_label[CODE_DOWN]); sur_print(sbuf); PR_ENRM;
        sur_print(" Next variable (field)");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_UP]); sur_print(sbuf); PR_ENRM;
        sur_print(" Previous variable (field)");
        PR_EINV; sprintf(sbuf,"\n%s (alt-F5)",key_label[CODE_SRCH]); sur_print(sbuf); PR_ENRM;
        sur_print(" Search for a given field # or name");

        sur_print("\n");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_RIGHT]); sur_print(sbuf); PR_ENRM;
        sur_print(" Go to the next mask column");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_LEFT]); sur_print(sbuf); PR_ENRM;
        sur_print(" Go to the previous mask column");
        PR_EINV; sprintf(sbuf,"\n%s",key_label[CODE_EXEC]); sur_print(sbuf); PR_ENRM;
        sur_print(" All mask columns on/off");
        PR_EINV; sprintf(sbuf,"\n%s (alt-F3)",key_label[CODE_COPY]); sur_print(sbuf); PR_ENRM;
        sur_print(" Exchange current mask column with another");
        PR_EINV; sprintf(sbuf,"\n%s (alt-F6)",key_label[CODE_ACTIV]); sur_print(sbuf); PR_ENRM;
        sur_print(" Fill the current mask column with a given character");

        PR_EBLD;
        sur_print("\n\nPress any key!"); WAIT;
        all_disp();
        return(1);
        }

static int activate()
        {
        int i,h,ch;
        int kesken;
// RS REM        extern int *nop();

        tut_init();
        varsrivit=curvarsrivit=1; // RS 9.5.2013
        if (r_soft) r3+=r_soft+1;
// Rprintf("data=%s|",active_data); getck();

        sur_flush_input();  // 25.1.2002
        if (etu!=2) sur_wait(200L,nop,0);
// RS REM        while (sur_kbhit()) { *active_data=EOS; sur_getch(); }
        tila=0; fmask=0;
        if (strcmp(info,"KEY_ACTIV")==0) tila=1;
        if (strcmp(info,"FMASK")==0) { tila=1; fmask=1; } // RS 8.5.2013
        if (strcmp(info,"MASK")==0) tila=2;
        if (strcmp(info,"VARS")==0) tila=2; // RS 25.11.2012
        if (tila)
            {
                        
            *info=EOS;
            if (*active_data!=EOS) strcpy(tiedosto,active_data);
            else
                {
                LOCATE(r3+2,1); PR_EINV;
                prompt("Survo data file? ",tiedosto,32);
                if (*tiedosto==EOS) return(1);
                }
            }
        else
            {
            if (g<3)
                {
                sur_print("\nUsage:");
                sur_print("\nFILE ACTIVATE <Survo_data_file>");
                WAIT; return(1);
                }
            strcpy(tiedosto,word[2]);
            }
        i=fi_open3(tiedosto,&dat,0,1,0,0); if (i<0) return(-1);
        vain_selailu=1;
        m=dat.m; n=dat.n;
        m_disp=r3-3;

        if (tila==2 && m>c2-6)
            {
            sprintf(sbuf,"\n# of fields in file = %d. Edit field width = %d.",
                        m,c2); sur_print(sbuf);
            sur_print("\nNot space enough for MASK specifications!");
            fi_close(&dat);
            WAIT; return(-1);
            }

        n_col=3;
/*      if (g>3) n_col=atoi(word[3]);
        if (n_col<2) n_col=2;
        if (n_col>dat.extra-5) n_col=dat.extra-5;
*/
        if (tila==2) { n_col=1; i=mask_read(); if (i<0) { fi_close(&dat); return(-1); } }
        if (fmask) // RS 9.5.2013
            {
            SURVO_DATA apudat;
            
            sp_init(r1+r-1);
            data_read_open(tiedosto,&apudat); if (i<0) { fi_close(&dat); return(-1); }
            mask(&apudat);
            for (i=0; i<m; i++) dat.vartype[i][1]=apudat.vartype[i][1];
            data_close(&apudat);             
            n_col=1; 
            }

        col0=8; col=0;
        muutokset=0;
        for (i=0; i<64; ++i)
            message[i]=lopetus;
        message[1]=aktiv;
        message[2]=suoja;
        message[3]=skaala;

        laske_akt();
        i1=0;
        i=all_disp();
        kesken=1;
        while (kesken)
            {
            ch=nextch(tiedotus);
            switch (ch)
                {
              case CODE_EXIT:
                kesken=0; break;
              case CODE_INSERT: // RS 25.11.2012
                kesken=0; tila=99; break;   
              case CODE_RETURN:
              case CODE_DOWN:
                down();
                break;
              case CODE_UP:
                if (muuttuja>i1) { --muuttuja; osoita(muuttuja); break; }
                if (i1>0)
                    {
                    SCROLL_DOWN(2,i2-i1+2,1);
                    --i1; --i2; --muuttuja;
                    var_disp(muuttuja);
                    }
                break;
              case CODE_HOME:
                if (muuttuja==0) break;
                if (muuttuja==i1) { i1=0; all_disp(); break; }
                muuttuja=i1;
                osoita(muuttuja);
                break;
              case CODE_NEXT:
                if (i2==m-1) break;
                i1=i2+1;
                all_disp();
                break;
              case CODE_PREV:
                if (i1==0) break;
                i1-=m_disp; if (i1<0) i1=0;
                all_disp();
                break;
              case CODE_RIGHT:
                if (col==n_col-1) break;
                ++col; col_disp();
                putsaa();
                osoita(muuttuja);
                tiedotus=message[col+1];
                break;
              case CODE_LEFT:
                if (col==0) break;
                --col; col_disp();
                putsaa();
                osoita(muuttuja);
                tiedotus=message[col+1];
                break;
              case CODE_COPY:
                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return(-1); }
                k_copy();
                break;
              case CODE_EXEC:
                if (n_col==dat.extra-5)
                    {
                    n_col=3;
                    if (col>2) col=2;
                    }
                else n_col=dat.extra-5;
                all_disp();
                if (tila==2) tila=3;
                break;
              case CODE_SRCH:
                putsaa();
                LOCATE(r3+2,1); PR_EBLD;
                prompt("Field # or name ? ",fieldno,8);
                if (strchr("0123456789",*fieldno)==NULL)
                    i0=nimen_nro(fieldno);
                else
                    i0=atoi(fieldno)-1;
                if (i0<0) i0=0;
                if (i0>m-1) i0=m-1;
                i1=i0;
                if (i1>m-m_disp) i1=m-m_disp;
                if (i1<0) i1=0;

                putsaa();
                all_disp();
                muuttuja=i0; osoita(muuttuja);
                break;
              case CODE_HELP:
                k_help();
                break;

              case CODE_ACTIV: /* Filling a mask column */
                PR_EBLK; sur_print("*");
                putsaa();
                LOCATE(r3+2,1); PR_EBLD;
                prompt("Activating character for current column ? ",act_char,1);
                if (*act_char==EOS) *act_char=' ';
                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return(-1); }
                for (i=muuttuja; i<dat.m; ++i)
                    dat.vartype[i][col+1]=*act_char;
                ++muutokset;
                putsaa();
                i=muuttuja;
                if (col<2) laske_akt();
                all_disp();
                muuttuja=i; osoita(muuttuja);
                break;

              case CODE_END:
                col=n_col-1;
                col_disp();
                putsaa();
                osoita(muuttuja);
                tiedotus=message[col+1];
                break;

              case CODE_PRE:
                ch=nextch(tiedotus);
                switch (ch)
                    {
                  case CODE_DOWN:
                    while (muuttuja<m-1) down();
                  default: break;
                    }
                break;
              default:

         /*     if (special) break;      22.5.90 */

                if (vain_selailu) { i=kirjoitukseen(); if (i<0) return(-1); }

                h=dat.vartype[muuttuja][col+1];
                if (col==0)
                    {
                    if (h=='-' && ch!='-') { ++m_act; act_disp(); osoita(muuttuja); }
                    else if (h!='-' && ch=='-') { --m_act; act_disp(); osoita(muuttuja); }
                    }

                if (col==1)
                    {
                    if (h!='P' && ch=='P') { ++m_pro; pro_disp(); osoita(muuttuja); }
                    else if (h=='P' && ch!='P') { --m_pro; pro_disp(); osoita(muuttuja); }
                    }

                dat.vartype[muuttuja][col+1]=ch;
                ++muutokset;
                PRMODE;
                sprintf(sbuf,"%c",ch); sur_print(sbuf);
                PR_ENRM;
                if (muuttuja==m-1) { osoita(muuttuja); break; }
                down();
                break;
                }
            }

        if (m_act==0) { dat.vartype[0][1]='A'; ++muutokset; }
        if (fmask) fmask_write(); // RS 8.5.2013
        else if (tila>=2) mask_write();
        if (muutokset || tila==3) act_save(&dat);
        fi_close(&dat);
        tut_end();
        if (r_soft) r3-=r_soft+1;
        s_end(argv1);
        return(1);
        }


static int file_varfind(SURVO_DATA_FILE *s,char *name)
    {
    int i;
    char x[LNAME];

    i=strlen(name);
    strcpy(x,name);
    if (i<8) for (;i<8;++i) x[i]=' ';
    x[8]=EOS;

    for (i=0; i<(*s).m; ++i)
        {
//   Rprintf("\nname=%s| varname=%s|",x,(*s).varname[i]); getch();
        if (strcmp(x,(*s).varname[i])==0) return(i);
        }
    sprintf(x,"\nVariable %s not found",name);
    sur_print(x); WAIT;
    return(-1);
    }



static int fc_varfind2(char *s) // RS CHA fc_
    {
    int var;

    if (*s=='#')
        {
        var=atoi(s+1)-1; if (var<0) var=0;
        if (var>dat.m-1)
            {
            sprintf(sbuf,"\nToo big number #%d (max %d)",
                              var,dat.m);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(var);
        }
    var=file_varfind(&dat,s);
    return(var);
    }


static int error_on_line(int j,char *mname)
    {
    sprintf(sbuf,"\nError on line %d in MASKING %s",j,mname);
    WAIT; 
//    muste_fixme("\nFIXME: Check FILE MASK error exit! error_on_line()");
    return(-1); // RS CHA exit(0);
    }

static int mask_talletus(int i)
    {
    fi_puts(&dat,dat.vartype[i],dat.extra-4,
    (long)(dat.var+4L+(long)i*((long)dat.l+(long)dat.extra)));
    return(1);
    }

static int mask_with_list()  // 4.4.2005 (6.4.2005)
    {
    int i,j,j1,j2,k,h,var1,var2,estat;
    char mname[LNAME];
    char raja[12]="*..........";

// RS REM    int mline;
    char x[LLENGTH];
    char *s[EP4];
// RS REM    char vname[9];
    char *p,*q;

    if (g==3) // RS 8.5.2013
        {
        j=r1+r-1;
        while (1)
            {
            ++j;
            if (j>r2)
                {
//                sprintf(sbuf,"\nMASKING not found!");
//                sur_print(sbuf); WAIT; return(0);
                strcpy(active_data,word[2]);

                strcpy(info,"FMASK");
                activate();
                j=r1+r-1;
                }
            edread(x,j);
            i=muste_instr(x,raja);
            if (i>=0) { j=r2+1; continue; }
            i=split(x+1,s,3);
            if (i<1) continue;
            if (strcmp(s[0],"MASKING")==0)
                {
                p=NULL; q=NULL; mname[0]=EOS;
                if (i>1) p=strchr(s[1],'(');
                if (p==NULL && i>2) { strcpy(mname,s[1]); p=strchr(s[2],'('); }
                if (p!=NULL) q=strchr(p,')');
                if (p!=NULL && q!=NULL)
                    {
                    *q=EOS;
                    masknro=atoi(p+1);
                    }
                else masknro=1;
                break;
                }
            }
        }
    else
        {
        if (g==4) masknro=1; else masknro=atoi(word[4]);
        strcpy(mname,word[3]);
        j=0;
        while (1)
            {
            ++j;
            if (j>r2)
                {
                sprintf(sbuf,"\nMASKING %s not found!",mname);
                sur_print(sbuf); WAIT; return(0); // RS CHA exit(0);
                }
            edread(x,j);
            i=split(x+1,s,2);
            if (i<2) continue;
            if (strcmp(s[0],"MASKING")==0 && strcmp(s[1],mname)==0) break;
            }
        }
    j1=j+1;
    while (1)
        {
        ++j;
        if (j>r2)
            {
            sprintf(sbuf,"\nEND of MASKING %s not found!",mname);
            sur_print(sbuf); WAIT; return(0); // RS CHA exit(0);
            }
        edread(x,j);
        i=split(x+1,s,1);
        if (i<1) continue;
        if (strcmp(s[0],"END")==0) break;
        }
    j2=j;

    i=fi_open(word[2],&dat); if (i<0) return(-1);
    dat.mode=0;

    mask0=(int *)muste_malloc(dat.m*sizeof(int));
    for (i=0; i<dat.m; ++i) mask0[i]=0;

    for (j=j1; j<j2; ++j)
        {
        edread(x,j);
        p=strstr(x+1," / "); if (p!=NULL) *p=EOS;
        k=split(x+1,s,EP4);
        if (k<2)
            {
            estat=error_on_line(j,mname);
            if (estat<0) return(-1); // RS ADD
            }
        if (strcmp(s[0],"OTHERS:")==0)
            {
            if (strcmp(s[1],"SAME")!=0)
                for (i=0; i<dat.m; ++i)
                    {
                    if (!mask0[i])
                        {
                        dat.vartype[i][masknro]=*s[1];
                        mask_talletus(i);
                        }
                    }
            continue;
            }
        else
            {
            p=strchr(s[k-2],':'); if (p!=NULL) *p=EOS;
            for (h=0; h<k-1; ++h)
                {
                p=strchr(s[h],'-');
                if (p!=NULL)
                    {
                    *p=EOS;
                    var1=fc_varfind2(s[h]); if (var1<0) return(0); // RS CHA exit(0);
                    var2=fc_varfind2(p+1); if (var2<0) return(0); // RS CHA exit(0);
                    if (var1>var2) { i=var1; var1=var2; var2=i; }
                    for (i=var1; i<=var2; ++i)
                        {
                        if (!mask0[i])
                            {
                            dat.vartype[i][masknro]=*s[k-1];
                            mask_talletus(i);
                            }
                        mask0[i]=1;
                        }
                    continue;
                    }

                i=fc_varfind2(s[h]);
                if (i<0) return(0); // RS CHA exit(0);
                if (!mask0[i])
                    dat.vartype[i][masknro]=*s[k-1];
                mask0[i]=1;
                mask_talletus(i);
                }
            }
        } // j
    
    fi_close(&dat); // RS ADD
    return(1);
    }



// FILE MASK <data>,<1st_var>,<mask_column>,<masks>
// or
// FILE MASK <data>,(XYZ),<mask_column>,<mask>  4.4.2005
//

static int file_mask()
        {
        int i,k,var=0;
        char name[LNAME];
        char ch;
        int xyz;   // 1: (XYZ) -> A  0: <1st_var> -> <masks>
        char *sxyz;
        char *p;

        if (g==3 || g==4 || g==5)
            {
            mask_with_list(); return(1);
            }

        if (g<6)
            {
            sur_print("\nUsage: FILE MASK <data>,<1st_var>,<mask_column>,<masks>");
            sur_print("\n       writes <masks> in <mask_column> from <1st_var>");
            sur_print("\n       onwards.");
            sur_print("\n       Example: FILE MASK DATA1,CASE,1,-");
            sur_print("\nOther forms: See FMASK?                              ");
            WAIT; return(1);
            }

        i=fi_open(word[2],&dat); if (i<0) return(-1);

        strcpy(name,word[3]);
        if (*name=='(')
            {
            xyz=1;
            p=strchr(name,')'); if (p!=NULL) *p=EOS;
            sxyz=name+1;
            }
        else
            {
            xyz=0;
            if (*name=='#')
                {
                var=atoi(name+1)-1; if (var<0) var=0;
                if (var>dat.m-1)
                    {
                    sprintf(sbuf,"\nToo big number #%d (max %d)",
                                      var,dat.m);
                    sur_print(sbuf); WAIT; return(0); // RS CHA exit(0);
                    }
                }
            else
                {
                for (i=strlen(name); i<8; ++i) name[i]=' '; name[8]=EOS;
                for (i=0; i<dat.m; ++i)
                    {
                    if (strncmp(name,dat.varname[i],8)==0) break;
                    }
                if (i==dat.m)
                    {
                    sprintf(sbuf,"\nField %s not found!",word[3]);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                var=i;
                if (var<0) return(-1);
                }
            }
        masknro=atoi(word[4]);
        if (masknro<1 || masknro>=dat.extra-4)
            {
            sur_print("\nInvalid mask column!");
            WAIT; return(-1);
            }
        if (xyz)
            {
            ch=*word[5];
            for (i=0; i<dat.m; ++i)
                {
                if (strchr(sxyz,dat.vartype[i][masknro])!=NULL)
                dat.vartype[i][masknro]=ch;
                }
            }
        else
            {
            k=strlen(word[5]);
            if (k==4 && strcmp(word[5]+1,"...")==0)
                {
                ch=*word[5];
                if(ch=='_') ch=' ';
                for (i=var; i<dat.m; ++i)
                    dat.vartype[i][masknro]=ch;
                }
            else for (i=0; i<k; ++i)
                {
                if (i+var>=dat.m) break;
                ch=word[5][i];
                if(ch=='_') ch=' ';
                dat.vartype[i+var][masknro]=ch;
                }
            }
        dat.mode=0;
        for (i=0; i<dat.m; ++i)
            {
            mask_talletus(i);
// vanha    fi_puts(&dat,dat.vartype[i],dat.extra-4,
// koodi      (long)(dat.var+4L+(long)i*((long)dat.l+(long)dat.extra)));
            }

		fi_close(&dat); // RS ADD

        return(1);
        }

//  FILE MASK DECA,M1,1
//
//  MASKING M1
//  Points: Y
//  100m,L_jump,Shot_put: X
//  Weight: W
//  OTHERS: SAME
//  END

/* FILE UPDATE <nimi>
   0    1       2
*/
static int update()
        {
        int i,j,k,h,textn; // RS REM ,m1,l,m,filen;
        long obs;
        char x[LLENGTH], *sana[4];
        int fipituus, i1, edell_i;
        int uusi;
// RS REM        int pos;
        char vartype[LLENGTH], varname[LLENGTH];
        int varlen;
// RS REM         unsigned char jakso[LLENGTH];
        int m0;  /* vanha muuttujalkm */
        char *p;

        if (g<3)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE UPDATE <filename>");
            WAIT; return(-1);
            }
        i=sp_init(r1+r-1); if (i<0) return(-1);
        prind=0; uusi=0;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);
        i=fields_rivit(); if (i<0) return(-1);

        strcpy(x,word[2]);
        subst_survo_path(x);
        i=fi_open2(x,&dat,1,1,1); if (i<0) return(-1);
                             /* 1=laaja eli m1 muuttujan tilat */
        m0=dat.m;

        actsar=dat.extra-5;
        textn=fields_rivi-r1-r;
        if (textn>dat.textn) textn=dat.textn;

        for (i=0; i<textn; ++i)
            {
            edread(x,r1+r+i);
            for (k=strlen(x+1); k<dat.textlen; ++k) x[k+1]=' ';
            fi_puts(&dat,x+1,dat.textlen,(long)(dat.text+i*dat.textlen));
            }

        edell_i=0;
        for (i=0; i<n_fields; ++i)
            {
            j=fields_rivi+i+1;
            edread(x,j);
            k=split(x+1,sana,4);
            if (k<4)
                {
                field_error(j);                
                fi_reduce_m(&dat,m0); fi_close(&dat); // RS 26.3.2013
                return(-1);
                }
            i1=atoi(sana[0]);
            if (i1<=edell_i)
                {
                sprintf(sbuf,"\nIncorrect field number on edit line %d",j);
                sur_print(sbuf); WAIT; fi_reduce_m(&dat,m0); fi_close(&dat); return(-1); // RS 26.3.2013 fi_reduce+close
                }
            if (i1>dat.m && i1!=dat.m+1)
                {
                sprintf(sbuf,"\nIncorrect field number on edit line %d (%d expected)"
                           ,j,dat.m+1); sur_print(sbuf);
                WAIT; fi_reduce_m(&dat,m0); fi_close(&dat); return(-1); // RS 26.3.2013 fi_close
                }
            if (i1>dat.m1)
                {
                sur_print("\nNot enough space for new fields in current file!");
                sprintf(sbuf,"\nCreate a new one with %d fields at least",
                                i1+n_fields+fields_rivi-j); sur_print(sbuf);
                sur_print("\nor use FILE EXPAND!"); // 30.8.2008
                WAIT; fi_reduce_m(&dat,m0); fi_close(&dat); return(-1); // RS 26.3.2013 fi_close
                }
            if (i1>dat.m) uusi=1; else uusi=0;

            k=check_vartype(sana[1],j);
            if (k<0) { fi_reduce_m(&dat,m0); fi_close(&dat); return(-1); } // RS 26.3.2013 fi_reduce+close
            p=sana[1]; while (*p) { if (*p=='_') *p=' '; ++p; }
            if (sana[1][0]=='N') sana[1][0]=*sana[2];
            if (!uusi && sana[1][0]!=dat.vartype[i1-1][0])
                {
                sprintf(sbuf,"\nError on edit line %d:",j); sur_print(sbuf);
                sprintf(sbuf,"\nChange of field type (from %c to %c) not allowed!",
                                dat.vartype[i1-1][0],sana[1][0]);
                sur_print(sbuf); WAIT; fi_reduce_m(&dat,m0); fi_close(&dat); return(-1); // RS 26.3.2013 fi_close
                }
            if (uusi) { strncpy(vartype,space,actsar+1); vartype[actsar+1]=EOS; }
            else strcpy(vartype,dat.vartype[i1-1]);
            vartype[1]='A';
            k=0; while (k<actsar+1 && sana[1][k])
                { vartype[k]=sana[1][k]; ++k; }
            if (vartype[0]=='N') vartype[0]=*sana[2];
            k=atoi(sana[2]);
            if (!uusi && k!=dat.varlen[i1-1])
                {
                sprintf(sbuf,"\nError on edit line %d:",j); sur_print(sbuf);
                sprintf(sbuf,"\nChange of field length (from %d to %d) not allowed!",
                                dat.varlen[i1-1],k);
                sur_print(sbuf); WAIT; fi_reduce_m(&dat,m0); fi_close(&dat); return(-1); // RS 26.3.2013 fi_close
                }
            varlen=k;
            if (uusi)
                fipituus+=k;
            k=check_varlen(k,vartype[0],j); 
            if (k<0) { fi_reduce_m(&dat,m0); fi_close(&dat); return(-1); } // RS 26.3.2013 fi_close
            edread(x,fields_rivi+i+1);
            strncpy(varname,sana[3],dat.l);
            check_varname(varname);

            k=fi_var_save(&dat,i1-1,vartype,varlen,varname);
            if (k<0) { fi_reduce_m(&dat,m0); fi_close(&dat); return(-1); } // RS 26.3.2013 fi_close
            strncpy(dat.varname[i1-1],space,8);    /* 31.3.91 vain testia varten */
            strncpy(dat.varname[i1-1],varname,8);  /* 31.3.91 vain testia varten */
            edell_i=i1;
            }

        if (dat.m>m0 && dat.n>0L)
            {
            char *initval; // RS 22.12.2012
            initval=NULL;
         	i=spfind("MISSING");
         	if (i>=0)
         		{
         		initval=spb[i];
         		if (strlen(initval)==0) initval=NULL;
         		else if (strcmp(initval,"MISSING")==0) initval=NULL;
         		}           
            
            sur_print("\nSaving missing values for new fields... ");
            fi_rewind(&dat);
            for (obs=1L; obs<=dat.n; ++obs)
                {
                if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
                if (prind) { sprintf(sbuf,"%ld ",obs); sur_print(sbuf); }

				if (initval==NULL)
                	{ for (i=m0; i<dat.m; ++i) fi_miss_save(&dat,obs,i); }
                else
                	{ for (i=m0; i<dat.m; ++i) fi_init_save(&dat,obs,i,initval); } // RS 22.12.2012
                }
            }
/*      fi_close(&dat);   siirretty loppuun! */

        if (!etu)  /* 31.3.91 */
            {
            k=0;
            for (i=1; i<dat.m; ++i)
                {
                for (h=0; h<i; ++h)
                    {
                    if (strncmp(dat.varname[h],dat.varname[i],8)!=0) continue;
                    if (!k)
                        {
                        sur_print("\nWARNING!  Field names appearing at least twice: ");
                        k=1;
                        }
                    sprintf(sbuf,"%.8s ",dat.varname[i]); sur_print(sbuf);
                    }
                }
            if (k) { WAIT; }
            }
        fi_close(&dat);
        return(1);
        }

/* FILE INIT <nimi>,N
   0    1     2     3
*/
static int init()
        {
        int i;
        long n_add,l,n;
        
        if (g<4)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE INIT <filename>,N / add N missing obs.");
            WAIT; return(-1);
            }

        n_add=atol(word[3]);
        if (n_add<=0) return(1); // 30.1.2000

        i=fi_open(word[2],&dat); if (i<0) return(-1);
        if (n_add<=0L) return(-1);

        n=dat.n+n_add;
        for (l=dat.n+1L; l<=n; ++l)
            fi_miss_obs(&dat,l);

        fi_rewind(&dat);
        fi_puts(&dat,(char *)&n,sizeof(int),22); // RS ADD (char *) CHA 64-BIT  sizeof(long) -> sizeof(int) 22L -> 22
        dat.n=n;

        fi_close(&dat);
        return(1);
        }

static int getvar(int k)  // 27.11.2001-  MASK,VARS otettu huomioon!
    {
    int i,j,h;
    int sar;
    char x[LLENGTH],*s[3];
    char ch,ch2;

    sar=1; ch='A';
    strcpy(x,spb[k]);
    i=split(x,s,3);
    if (i>0) ch=*s[0];
    if (i>1) h=atoi(s[1]); else h=1;
    if (h<1) h=1;
    if (i>2) sar=atoi(s[2]);
    j=0;
    for (i=0; i<d.m_act; ++i)
        {
        ch2=d.d2.vartype[d.v[i]][sar];
        if (ch!=ch2) continue;
        ++j;
        if (j==h) break;
        }
    if (i==d.m_act)
        strcpy(sbuf,"NOT FOUND!");
    else
        sprintf(sbuf,"%c %d %s",d.vartype[d.v[i]][0],d.varlen[d.v[i]],
                                d.varname[d.v[i]]);
    j=r1+r;
    edwrite(space,j,1);
    edwrite(sbuf,j,1);
    return(1);
    }

static int tulosta(char *rivi)
        {
        if (tulosrivi>r2) { data_close(&d); return(-1); }
        edwrite(space,tulosrivi,1);
        edwrite(rivi,tulosrivi++,1);
        return(1);
        }

static void status()
        {
        int i,k;
        char x[LLENGTH];
        int all;

        if (g<3)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE STATUS <Survo_data_file>");
            WAIT; return;
            }
        strcpy(x,word[2]);
        subst_survo_path(x);       
        i=data_open3(x,&d,0,1,1,0); if (i<0) return;        
        if (d.type!=2) return;
        m=d.m;
        i=sp_init(r1+r-1); if (i<0) return;
        all=0;
        i=spfind("ALL");
        if (i>=0)
            { if (atoi(spb[i])) all=1; }
        else mask(&d);

        i=spfind("GETVAR"); // 20.10.2001
        if (i>=0) { getvar(i); return; }

        actsar=3;
        if (g>3) actsar=atoi(word[3])+1;
        if (actsar>d.d2.extra-4) actsar=d.d2.extra-4;

        tulosrivi=r1+r;
        if (g>4) { tulosrivi=edline2(word[4],1,1); if (tulosrivi==0) return; }        
        for (i=0; i<d.d2.textn; ++i)
            { k=tulosta(d.d2.fitext[i]); if (k<0) return; }            
        k=tulosta("FIELDS: (active)"); if (k<0) return;
        for (i=0; i<m; ++i)
            {
            char type[LLENGTH];
            char *p; 
            if (!all) if (d.vartype[i][1]=='-') continue;
            strcpy(type,d.vartype[i]);
            if (*type=='1' || *type=='2' || *type=='4' || *type=='8')
                *type='N';
            while ((p=strchr(type,' '))!=NULL) *p='_';
            sprintf(x,"%4d %.*s %3d %s",
                  i+1,actsar,type,d.varlen[i],d.varname[i]);                 
            k=tulosta(x); if (k<0) return;
            }
        k=tulosta("END"); if (k<0) return;
                                                               // RS CHA N=%ld -> N=%d
        sprintf(x,"Survo data file %s: record=%d bytes, M1=%d L=%d  M=%d N=%d",
                         active_data,d.d2.len,d.d2.m1,d.d2.l,m,d.d2.n);
        tulosta(x);
        data_close(&d);
        }

static int get_var_name()
    {
    int i,len;
    char s[LNAME];
    char tiedosto[LNAME];
    if (*info==EOS) return(1);

    strcpy(s,info+13);
// Rprintf("\ns=%s|",s); getch();
    *info=EOS;
    if (*active_data!=EOS) strcpy(tiedosto,active_data);
    if (!sur_find_svo_file(tiedosto)) return(-1);
// RS CHA    i=fi_find(tiedosto,&dat,sbuf); if (i<0) return(-1);
// RS CHA   fi_close(&dat);
    i=fi_open3(tiedosto,&dat,0,1,0,0); if (i<0) return(-1);
    *info=EOS;
    strcat(s," "); len=strlen(s);
    for (i=0; i<dat.m; ++i)
        {
// Rprintf("\n%d: %s| %s|",len,s,dat.varname[i]); getch();
        if (strncmp(s,dat.varname[i],len)==0) break;
        }
    if (i==dat.m) { fi_close(&dat); return(1); }
    strcpy(info,dat.varname[i]);
    fi_close(&dat);
// Rprintf("\ninfo=%s",info); getch();
    return(1);
    }


void muste_file_create(int argc,char *argv[])
        {
/* RS REM        char x[LNAME];
        char *p; */        
        int i;

        if (argc==1) return;

// RS ADD Init variables
m=0;
tulosrivi=0;
fields_rivi=0;
n_fields=0;
actsar=0; /* aktivointisarakkeiden lkm */
m_act=0;
m_pro=0;
m=0;
m_disp=0; /* # of var.disp.lines */
i1=i2=0;  /* 1st and last disp.var. */
col0=0;   /* first edit column on the screen */
n_col=0;  /* # of columns displayed */
col=0;    /* current column */
muuttuja=0; /* current variable */
n=0;
muuttuja=0;
muutokset=0;
i0=0;
tila=0;
masknro=-1;
vain_selailu=0;
prind=0;
         
        s_init(argv[1]);
        argv1=argv[1];
        
        i=labels(); // 28.12.2000
        if (i<0) return; // RS ADD

        if (strncmp(info,"GET_VAR_NAME",12)==0)
            {
            get_var_name(); s_end(argv[1]); return;
            }

        if (strncmp(info,"KEY_ACTIV",9)==0 || strncmp(info,"MASK",4)==0)
            { activate(); return; }
        if (muste_strcmpi(word[1],"CREATE")==0)
            { create(); s_end(argv[1]); return; }
        if (muste_strcmpi(word[1],"MAKE")==0)
            { fmake(); return; }
        if (muste_strcmpi(word[1],"UPDATE")==0)
            { update(); return; }
        if (muste_strcmpi(word[1],"STATUS")==0)
            { status(); s_end(argv[1]); return; }
        if (muste_strcmpi(word[1],"INIT")==0)
            { init(); return; }
        if (muste_strcmpi(word[1],"REDUCE")==0)
            { reduce(); return; }
        if (muste_strnicmp(word[1],"ACT",3)==0)
            { activate(); return; }
        if (muste_strcmpi(word[1],"MASK")==0)
            { file_mask(); return; }
        if (muste_strcmpi(word[1],"COND")==0)
            { file_cond(); s_end(argv[1]); return; }
        if (muste_strcmpi(word[1],"MASKLOAD")==0)
            { maskload(); s_end(argv[1]); return; }
        if (muste_strcmpi(word[1],"CONVERT")==0)
            { convert(); return; }

        }

