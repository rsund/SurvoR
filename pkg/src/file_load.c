/* load.c 8.2.1986/SM (13.11.1992) (22.1.1996) (29.9.1996)
   FILE LOAD <SURVO 84C data file>,L
   FILE LOAD <SURVO 84C data file> TO <text file>
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;
static FILE *tekstit;
static int m;
static int ep4;
static char *formtila;
static unsigned int maxtila;
static char **form;
static char muoto[LLENGTH];
static int *pos, *len, *limit_pos;
static int tulosrivi;
static int leveys,kleveys;
static char rivi[100*LLENGTH];   // oli 40 -8.1.2001
static unsigned char limit_char=' ';
static int no_last_limit=1; /* 1=ei viimeisen perÑÑn */
static char space_char=' ';
static int prind=0;

static unsigned char code[256]; /* 13.11.92 */
static char filter1[LNAME];
static int names8=0;
static char missing_str[32];
static int is_delimiter=1; // 23.4.2002
static char str_comma[2]="";
static char str_space[2]="";

static char kielletty[128]; // 11.5.2006

static FILE *codes;    /* 13.11.1992 */

static double *minx,*maxx;

static int nf;
static int h1,h2;
static char **privi;
static int *frivi,*fpos,*flen;
static char *sformtila;
static char **sprivi;

/* RS REM
static char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "MISSING", "STR_COMMA", "STR_SPACE", "FORMAT",
                 "FILTER", "DELIMITER", "SPACE", "LIST_VARIABLES",
                 "NAMES8", "PRIND", "!" };
static char **specs=specs0;
*/

static int space_muunto(char *rivi)
        {
        char *p,*q;

        p=rivi;
        while (*p==' ' && *p) *p++=space_char;
        while (1)
            {
            q=strchr(p,limit_char);
            if (q==NULL) break;
            p=q+1;
            --q;
            while (*q==' ') *q--=space_char;
            while (*p==' ' && *p) *p++=space_char;
            }
        p=rivi+strlen(rivi)-1;
        while (*p==' ') *p--=space_char;
        return(1);
        }


static void koodimuunto(char *rivi)
        {
        char *p,*q; // RS REM unsigned
        unsigned char ch;

        p=rivi; q=rivi;
        while (*p)
            {
            ch=code[(int)*p]; ++p;
            if (ch==EOS) continue;
            *q=ch; ++q;
            }
        *q=EOS;
        }

static int kirjoita(char *rivi)
        {

        if (*filter1) koodimuunto(rivi);   /* 13.11.92 */

        if (space_char!=' ') space_muunto(rivi);

        if (!tulosrivi)
            {
            fputs(rivi,tekstit);
            putc((int)'\n',tekstit);
            return(1);
            }
        if (tulosrivi>r2)
            {
            sur_print("\nNot enough lines in the edit field!");
            WAIT; data_close(&d); return(-1);
            }

        edwrite(space,tulosrivi,1);
        edwrite(rivi,tulosrivi,1);
        ++tulosrivi;
        return(1);
        }

static int kirjoita2(char *rivi) /* 22.1.1996  also control characters in formatted list */
        {

        if (*filter1) koodimuunto(rivi+1);

        if (!tulosrivi)
            {
            fputs(rivi+1,tekstit);
            putc((int)'\n',tekstit);
            return(1);
            }
        if (tulosrivi>r2)
            {
            sur_print("\nNot enough lines in the edit field!");
            WAIT; data_close(&d); return(-1);
            }

        edwrite(space,tulosrivi,0);
        edwrite(rivi,tulosrivi,0);
        ++tulosrivi;
        return(1);
        }


static int format_vec(char *nimi,char *txt)
    {
    int i,vi,h;
    long j;
    double a;

    tekstit=muste_fopen(txt,"wt");

    sprintf(sbuf,"\nLoading observations from file %s: ",nimi); sur_print(sbuf);
    m=d.m_act;
    for (i=0; i<m; ++i)
        {
        vi=d.v[i];
// Rprintf("\nvarname=%s",d.varname[vi]); getch();
        *sbuf=EOS; strncat(sbuf,d.varname[vi],8);
        h=7; while (sbuf[h]==' ') sbuf[h--]=EOS;
        fprintf(tekstit,"%s\n",sbuf);

        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            if (prind) { sprintf(sbuf,"%ld ",j); sur_print(sbuf); }

            if (d.vartype[vi][0]=='S')
                {
                fi_alpha_load(&d.d2,j,vi,sbuf);
                if (strncmp(sbuf,space,d.varlen[vi])==0)
                    strcpy(sbuf,"-");
                }
            else
                {
                fi_load(&d.d2,j,vi,&a);
                if (a==MISSING8) strcpy(sbuf,"-");
                else sprintf(sbuf,"%.10g",a);
                }
            fprintf(tekstit,"%s\n",sbuf);
            }
        }
    muste_fclose(tekstit);
    return(1);
    }

static void ltilavajaus()
        {
        sur_print("\nNot enough memory for FILE LOAD!");
        WAIT;
        }


static int varaa_tilat()
        {
        int i;
        char sana[LLENGTH];

        ep4=EP4; i=hae_apu("ep4",sana); if (i) ep4=atoi(sana);

        maxtila=100*ep4;
        formtila=muste_malloc(maxtila);
        if (formtila==NULL) { ltilavajaus(); return(-1); }
        form=(char **)muste_malloc(m*sizeof(char *));
        if (form==NULL) { ltilavajaus(); return(-1); }
        pos=(int *)muste_malloc(m*sizeof(int));
        if (pos==NULL) { ltilavajaus(); return(-1); }
        len=(int *)muste_malloc(m*sizeof(int));
        if (len==NULL) { ltilavajaus(); return(-1); }
        limit_pos=(int *)muste_malloc(m*sizeof(int));
        if (limit_pos==NULL) { ltilavajaus(); return(-1); }

        return(1);
        }


static void default_form(SURVO_DATA *d,int i,char *muoto)
        {
        int h,k;

        switch (d->vartype[i][0])
            {
          case 'S':
            k=d->varlen[i];
            for (h=0; h<k; ++h) muoto[h]='#'; muoto[k]=EOS;
            break;
          case '1':
            strcpy(muoto,"###");
            break;
          case '2':
            strcpy(muoto,"######");
            break;
          case '4':
            strcpy(muoto,"#######.###");
            break;
         case '8':
            strcpy(muoto,"#######.###");
            break;
            }
        }
/* kirjastoon sm.lib 31.3.86  (korvattu 11.5.2006 tÑssÑ lopussa
                               olevalla next_load_label-funktiolla)
char next_label(ch)
char ch;
        {
        int i;

        while (1)
            {
            for (i=0; i<r2; ++i)
                if (ch==*(z+i*ed1)) break;
            if (i==r2) return(ch);
            ++ch;
            }
        }
*/


static void ltilavirhe()
        {
        sur_print("\nNot enough space for formats (max 100*ep4)");
        WAIT;
        }


static void hae_muoto(SURVO_DATA *d,int i,char *muoto)
        {
        char *p,*q;
        char x[LLENGTH];
        int h,k;

        strcpy(x,d->varname[i]);
        p=x;
        while (1)
            {
            p=strchr(p,'(');
            if (p==NULL) { default_form(d,i,muoto); break; }
            else
                {
                if (*(p+1)!='#') { ++p; continue; }
                q=strchr(p,')');
                if (q==NULL) { ++p; continue; }
                strncpy(muoto,p+1,q-p-1); muoto[q-p-1]=EOS;
                if (muoto[1]!=EOS && muoto[1]!='#' && muoto[1]!='.')
                    {
                    k=atoi(muoto+1); if (k<=0) { ++p; continue; }
                    for (h=0; h<k; ++h) muoto[h]='#'; muoto[k]=EOS;
                    }
                break;
                }
            }
        }

static int etsi_muodot()
        {
        int i;
// RS REM        char x[LLENGTH];
        char *p,*pf;

        *formtila=EOS;
        pf=formtila;
        for (i=0; i<m; ++i)
            {
            hae_muoto(&d,d.v[i],muoto);
            p=muoto;
            form[i]=pf;
            if (pf-formtila>maxtila-strlen(p)-1) { ltilavirhe(); return(-1); }
            while (*p) *pf++=*p++; *pf++=EOS;
            }
        return(1);
        }



static void format_list(char *nimi)
        {
        extern int *len;
        extern char **form;
        int i,h,k;
        int lev;
        char x[LLENGTH], *osa[2];
        char sana[LLENGTH];
        long j;
        char y[LLENGTH];
        char *p,*q;
        double u;

        i=spfind("FORMAT"); if (i<0) return;
        strcpy(x,spb[i]);
        i=split(x,osa,2);
        if (i<2) lev=c3; else { lev=atoi(osa[1]); if (lev<0 || lev>c2) lev=c3; }

        i=varaa_tilat(); if (i<0) return;
        i=etsi_muodot(); if (i<0) return;
        for (i=0; i<m; ++i) len[i]=strlen(form[i]);

        if (m==1)
            {
            sprintf(y,"DATA ");
            }
        else
            {
            sprintf(y,"DATA %s*:(",nimi);
            }
        for (i=0; i<m; ++i)
            {
            strcpy(sana,d.varname[d.v[i]]); sana[8]=EOS;
            k=strlen(sana); while (sana[k-1]==' ') sana[--k]=EOS;
            strcat(y,sana);
            if (i<m-1) strcat(y,",");
            else if (m>1) strcat(y,") "); else strcat(y,": ");
            }

        sprintf(sbuf,"\nLoading observations from file %s: ",nimi); sur_print(sbuf);
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            if (prind) { sprintf(sbuf,"%ld ",j); sur_print(sbuf); }

            for (i=0; i<m; ++i)
                {
                int vi=d.v[i];

                if (d.vartype[vi][0]=='S')
                    {
                    fi_alpha_load(&d.d2,j,vi,sana);
                    if (strncmp(sana,space,len[i])==0)
                        *sana='-';
                    }
                else
                    {
                    fi_load(&d.d2,j,vi,&u);
                    if (u==MISSING8)
                                     {
                                       strncpy(sana,space,len[i]);
                                       k=strlen(missing_str);
                                       h=len[i]-k; if (h<0) { h=0; k=len[i]; }
                                       strncpy(sana+h,missing_str,k);
                                       sana[len[i]]=EOS;
                                     }

                    else
                        fconv(u,form[i],sana);
                    }
                p=sana; while (*p==' ') ++p;
                if (p>sana && *p==EOS) { --p; *p='-'; }
                k=strlen(p); while (p[k-1]==' ') p[--k]=EOS;
                while ((q=strchr(p,' '))!=NULL) *q='_';
                if (strlen(y)+k+1>lev)
                    {
                    k=kirjoita(y); if (k<0) return; *y=EOS;
                    }
                strcat(y,p);
                if (i==m-1) strcat(y," "); else strcat(y,",");
                }
            }  /* j */
        if (strlen(y)+3+1>lev) { k=kirjoita(y); if (k<0) return; *y=EOS; }
        strcat(y,"END");
        kirjoita(y);
        }


static int tutki_ftilat()
        {
        int i,h,k,g;
        char *p,*q,*r;
        char x[LLENGTH];

        m=0;
        for (h=h1,k=0; h<=h2; ++h,++k)
            {
            privi[k]=formtila+k*(c2+2);
            edread(privi[k],h);
            p=privi[k];
            if (tulosrivi)
                {
                sprivi[k]=sformtila+k*(c2+2);
                *sprivi[k]=EOS;
                if (zs[h]) edread(sprivi[k],zs[h]);
                }
            while ((q=strchr(p,':'))!=NULL)
                {
                r=q;
                while (r>p) { if (*r!=' ') --r; else break; }
                ++r;
                for (i=0; r<q; ++i,++r) x[i]=*r;
                x[i]=EOS;
                i=varfind(&d,x); if (i<0) return(-1);
                d.v[m]=i;
                ++q;
                p=q;
                while (*q && *q==' ') ++q;
                if (*q!='#')
                    {
                    hae_muoto(&d,i,x);
                    q=r=p+1;
                    for (g=0; g<strlen(x); ++g) { *r=x[g]; ++r; }
                    frivi[m]=k; fpos[m]=q-privi[k];
                    flen[m]=strlen(x);
                    }
                else
                    {
                    r=q;
                    while (*r!=' ') ++r;
                    frivi[m]=k; fpos[m]=q-privi[k];
                    flen[m]=r-q;
                    }
                p=q+1;
                ++m;
                }
            p=privi[k];
            while ((q=strchr(p,'['))!=NULL)
                {
                r=strchr(q,']');
                if (r==NULL)
                    {
                    sprintf(sbuf,"\n] missing on format line \n%.*s",c3,privi[k]);
                        sur_print(sbuf);
                    WAIT; return(-1);
                    }
                for (i=0; i<r-q-1; ++i) x[i]=*(q+1+i); x[i]=EOS;
                i=varfind(&d,x); if (i<0) return(-1);
                d.v[m]=i;
                hae_muoto(&d,i,x);
                frivi[m]=k;
                fpos[m]=q-privi[k];
                i=strlen(x); if (i+fpos[m]>c2) i=c2-fpos[m];
                flen[m]=i;
                for (g=0; g<i; ++g) *(q+g)=x[g];
                q+=g; while (q<=r) *q++=' ';
                p=q;
                ++m;
                }
            }
/* for (i=0; i<nf; ++i) Rprintf("\n%s",privi[i]+1); getch();
   for (i=0; i<m; ++i) Rprintf("\nfrivi=%d fpos=%d flen=%d",frivi[i],fpos[i],flen[i]); getch();
*/
        return(1);
        }

static int varaa_ftilat()
        {
// RS REM      int i;

        formtila=muste_malloc(nf*(c2+2));
        if (formtila==NULL) { ltilavajaus(); return(-1); }
        privi=(char **)muste_malloc(nf*sizeof(char *));
        if (privi==NULL) { ltilavajaus(); return(-1); }
        form=(char **)muste_malloc(3*m*sizeof(char *));
        if (form==NULL) { ltilavajaus(); return(-1); }
        frivi=(int *)muste_malloc(3*m*sizeof(int));
        if (frivi==NULL) { ltilavajaus(); return(-1); }
        fpos=(int *)muste_malloc(3*m*sizeof(int));
        if (fpos==NULL) { ltilavajaus(); return(-1); }
        flen=(int *)muste_malloc(3*m*sizeof(int));
        if (flen==NULL) { ltilavajaus(); return(-1); }
        if (tulosrivi)
            {
            sformtila=muste_malloc(nf*(c2+2));
            if (sformtila==NULL) { ltilavajaus(); return(-1); }
            sprivi=(char **)muste_malloc(nf*sizeof(char *));
            if (sprivi==NULL) { ltilavajaus(); return(-1); }
            }
        return(1);
        }


static void format_load(char *nimi)
        {
        int i,k,g,ii;
        char *sana[3];
        char x[LLENGTH];
        char y[LLENGTH];
        long j;
        char *p;

        i=spfind("FORMAT");
        k=split(spb[i],sana,3);
        if (k==3) { sur_print("\nPuuttuu!"); return; }

        h1=wfind("FORMAT",sana[0],1);
        if (h1<0)
            {
            sprintf(sbuf,"\nFORMAT %s not found!",sana[0]);
            sur_print(sbuf); WAIT; return;
            }
        ++h1;
        h2=h1;
        while (h2<=r2)
            {
            edread(x,h2);
            k=split(x+1,sana,1);
            if (strcmp(sana[0],"END")==0) break;
            ++h2;
            }
        if (h2>r2)
            {
            sur_print("\nEND missing in FORMAT!");
            WAIT; return;
            }
        --h2;
        nf=h2-h1+1;
        m=d.m;
        i=varaa_ftilat(); if (i<0) return;
        i=tutki_ftilat(); if (i<0) return;

        sprintf(sbuf,"\nLoading observations from file %s: ",nimi); sur_print(sbuf);
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            if (prind) { sprintf(sbuf,"%ld ",j); sur_print(sbuf); }
            i=0;
            for (k=0; k<nf; ++k)
                {
                strncpy(y,privi[k],c2+1);
                while (i<m && frivi[i]==k)
                    {
                    int vi=d.v[i];
                    char sn[LLENGTH];
                    char muoto[LLENGTH];
                    double a;
                    int hh,kk;

                    if (d.vartype[vi][0]=='S')
                        {
                        fi_alpha_load(&d.d2,j,vi,sn);
                        if (strncmp(sn,space,flen[i])==0)
                            *sn='-';
                        }
                    else
                        {
                        fi_load(&d.d2,j,vi,&a);
                        if (a==MISSING8) {
                                           strncpy(sn,space,flen[i]);
                                           kk=strlen(missing_str);
                                           hh=flen[i]-kk; if (hh<0) { hh=0; kk=flen[i]; }
                                           strncpy(sn+hh,missing_str,kk);
                                           sn[flen[i]]=EOS;
                                         }
                        else
                            {
                            for (g=0; g<flen[i]; ++g) muoto[g]=privi[k][fpos[i]+g];
                            muoto[flen[i]]=EOS;
                            fconv(a,muoto,sn);
                            }
                        }
                    for (g=0; g<strlen(sn); ++g) y[fpos[i]+g]=sn[g];
                    ++i;
                    }  /* while i */
                if (!tulosrivi)
                    { p=y+c2; while (*p==' ' && p>y+1) *p--=EOS; }

                if (tulosrivi && *sprivi[k])
                    {
                    if (!zs[tulosrivi])
                        {
                        ii=shadow_create(tulosrivi);
                        if (ii<0) return;
                        }
                    edwrite(sprivi[k],zs[tulosrivi],0);
                    }
           if (*y=='*') g=kirjoita(y+1);  /* 6.6.1996 */
           else g=kirjoita2(y); if (g<0) return;  /* 22.1.1996 kirjoita->kirjoita2 */
                }

            }
        }


static int format_order()
        {
        int i,k;
        char *sana[EP4];


        i=spfind("FORMAT");
        m=split(spb[i]+6,sana,EP4);
        if (m>d.m) return(-1);
        for (i=0; i<m; ++i)
            {
            k=varfind(&d,sana[i]);
            if (k<0) return(-1);
            d.v[i]=k;
            }
        return(1);
        }

char next_load_label(char ch)  // 11.5.2006
        {
        int i;

        while (1)
            {
            while (ch<127)
                {
                if (strchr(kielletty,ch)==NULL) break;
                ++ch;
                }
            if (ch==127) return(' ');
            for (i=0; i<r2; ++i)
                if (ch==*(z+i*ed1)) break;
            if (i==r2) return(ch);
            ++ch;
            }
        return('X');
        }


static int str_korvaus(char *x,char ch1,char ch2)
    {
    char *p;

    p=x;
    while (1)
        {
        p=strchr(p,ch1);
        if (p==NULL) return(1);
        *p=ch2;
        ++p;
        }
    return(1);
    }

static int find_limits(double min,double max)
    {
    int i,k;
    char *p;
    char name[9];
    char x[LLENGTH], *s[2];

    minx=(double *)muste_malloc(m*sizeof(double));
    if (minx==NULL) { ltilavajaus(); return(-1); }
    maxx=(double *)muste_malloc(m*sizeof(double));
    if (maxx==NULL) { ltilavajaus(); return(-1); }

    for (i=0; i<m; ++i)
        {
        minx[i]=min; maxx[i]=max;
        *name=EOS; strncat(name,d.varname[d.v[i]],8);
        p=name+7; while (*p==' ') *p--=EOS;
        k=spfind(name);
// Rprintf("\nk=%d name=%s|",k,name); getch();
        if (k<0) continue;

        strcpy(x,spb[k]);
        k=split(x,s,2);
        if (k<2) minx[i]=maxx[i]=atof(s[0]);
        else { minx[i]=atof(s[0]); maxx[i]=atof(s[1]); }
// Rprintf("\ni=%d minx=%g maxx=%g",i,minx[i],maxx[i]); getch();

        }

    return(1);
    }


static int list_names_of_variables(int ii)
    {
    int i,k;
    char x[LLENGTH], *s[4];
    int case_var;
    char case_var_type;
    int len;
    long j;
    double a;
    double min,max;
    char name[9];
    char *p;
    int long_names;
    char sana[LNAME];

    strcpy(x,spb[ii]);
    i=split(x,s,4);
    if (i<3)
        {
        sur_print("\nError in LIST_VARIABLES specification!");
        sur_print("\nUsage: LIST_VARIABLES=<case_var>,<lower_limit>,<upper_limit>");
        WAIT; return(1);
        }

    long_names=0;
    if (i==4) long_names=1;

    case_var=varfind(&d,s[0]);
    if (i<0) return(1);
    case_var_type=d.vartype[case_var][0];
// Rprintf("\ncase_var=%d type=%c",case_var,case_var_type); getch();
    len=d.varlen[case_var];
    if (case_var_type!='S') len=0;

    min=atof(s[1]);
    max=atof(s[2]);
    i=find_limits(min,max); if (i<0) return(1); // RS CHA exit(0); -> return(1);

if (!long_names)
    for (j=d.l1; j<=d.l2; ++j)
        {
        if (unsuitable(&d,j)) continue;
        if (prind) { sprintf(sbuf,"%ld ",j); sur_print(sbuf); }

        if (case_var_type=='S')
            {
            data_alpha_load(&d,j,case_var,&rivi);
            for (i=strlen(rivi); i<len; ++i) rivi[i]=' ';
            rivi[len]=EOS;
            }
        else
            {
            data_load(&d,j,case_var,&a);
            sprintf(rivi,"%g",a);
            }
        strcat(rivi,": ");
        for (i=0; i<m; ++i)
            {
            data_load(&d,j,d.v[i],&a);
            if (a<minx[i] || a>maxx[i]) continue;

            *name=EOS; strncat(name,d.varname[d.v[i]],8);
            p=name+7; while (*p==' ') *p--=EOS;
            strcat(rivi,name); strcat(rivi," ");
            }
        i=kirjoita(rivi); if (i<0) return(1); // RS CHA exit(0); -> return(1);
        }

else // long_names
    {
    maxtila=30*m;
    formtila=muste_malloc(maxtila);
    if (formtila==NULL) { ltilavajaus(); return(-1); }
    form=(char **)muste_malloc(m*sizeof(char *));
    if (form==NULL) { ltilavajaus(); return(-1); }
    i=etsi_muodot(); if (i<0) { data_close(&d); return(-1); }

    for (j=d.l1; j<=d.l2; ++j)
        {
        if (unsuitable(&d,j)) continue;
        if (prind) { sprintf(sbuf,"%ld ",j); sur_print(sbuf); }

        if (case_var_type=='S')
            {
            data_alpha_load(&d,j,case_var,&rivi);
            for (i=strlen(rivi); i<len; ++i) rivi[i]=' ';
            rivi[len]=EOS;
            }
        else
            {
            data_load(&d,j,case_var,&a);
            sprintf(rivi,"%g",a);
            }
        i=kirjoita(rivi); if (i<0) return(1); // RS CHA exit(0); -> return(1);
        for (i=0; i<m; ++i)
            {
            data_load(&d,j,d.v[i],&a);
            if (a<minx[i] || a>maxx[i]) continue;

            sprintf(rivi,"  %s",d.varname[d.v[i]]);
            p=strstr(rivi,"(#");
            if (p!=NULL) // jos maski, korvataan se arvolla!
                {
                fconv(a,form[i],sana);
                strncpy(p+1,sana,strlen(sana));
                }
            k=kirjoita(rivi); if (k<0) return(1); // RS CHA exit(0); -> return(1);
            }
        } // j
    } // long names
    return(1);
    }


static int avaa_tekstit(char *t)
        {
        char nimi[LLENGTH];

        strcpy(nimi,t);
        if (strchr(t,':')==NULL) { strcpy(nimi,edisk); strcat(nimi,t); }
        tekstit=muste_fopen(nimi,"a+t");
        if (tekstit==NULL)
            {
            sprintf(sbuf,"\nCannot open text file %s!",nimi); sur_print(sbuf);
            WAIT; return(-1);
            }
        return(1);
        }


/* FILE LOAD #NAMES <data_file> TO <txt_file> */
/*           2      3           4  5          */
static void load_names_to_textfile()
        {
        int i,m;
        char *ww[2];

        if (g<6)
            {
            sur_print("\nUsage: FILE LOAD #NAMES <data_file> TO <txt_file>");
            WAIT; return;
            }

        i=data_open3(word[3],&d,0,1,0,0); if (i<0) return;
        i=avaa_tekstit(word[5]); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;

        i=spfind("DELIMITER");
        if (i>=0)
            {
            strcpy(sbuf,spb[i]);
            if (*sbuf==EOS) limit_char=' ';
            else
                {
                i=split(sbuf,ww,2);
                if (muste_strcmpi(ww[0],"TAB")==0) limit_char='\t';
                else if (muste_strnicmp(ww[0],"SP",3)==0) limit_char=' ';
                else if (muste_strnicmp(ww[0],"char(",5)==0) limit_char=atoi(ww[0]+5);
                else limit_char=*ww[0];
                if (i>1) no_last_limit=0;  /* 0=myîs viimeisen perÑÑn */

                i=spfind("SPACE");
                if (i>=0)
                    space_char=*spb[i];
                }
            }
        m=d.m_act;
        for (i=0; i<m-1; ++i)
            {
            fprintf(tekstit,"%.8s%c",d.varname[d.v[i]],limit_char);
            }
        if (no_last_limit)
            fprintf(tekstit,"%.8s\n",d.varname[d.v[m-1]]);
        else
            fprintf(tekstit,"%.8s%c\n",d.varname[d.v[m-1]],limit_char);
        }


static int load_codes(char *codefile,unsigned char *code)
        {
        int i;
        char x[LLENGTH];

        strcpy(x,codefile);
        if (strchr(x,':')==NULL && *x!='.')
            { strcpy(x,survo_path); strcat(x,"SYS/"); strcat(x,codefile); } // RS CHA \\ -> /

        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nCode conversion file %s not found!",x); sur_print(sbuf);
            WAIT; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose(codes);
        return(1);
        }



void muste_file_load(int argc,char *argv[])
        {
        int i,h,k;
        long j;
        double x;
        char sana[LLENGTH];
// RS REM        char filename[64];
        char label[3];
// RS REM        int data_rivi;
        char *nimi;
        int jatko;
        char *ww[2];
        int printnames=0; // RS ADD

// RS REM        extern char next_load_label();
// RS ADD variable init
m=0;
ep4=0;
maxtila=0;
tulosrivi=0;
leveys=kleveys=0;
limit_char=' ';
no_last_limit=1; /* 1=ei viimeisen perÑÑn */
space_char=' ';
prind=0;
names8=0;
is_delimiter=1; // 23.4.2002
nf=0;
h1=h2=0;
label[0]=label[1]=label[2]=0;



        if (argc==1) return;
        s_init(argv[1]);

        if (g<3)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE LOAD <Survo_data_file>");
            sur_print("\nFILE LOAD <Survo_data_file> TO <Destination_file>");
            sur_print("\nFILE LOAD <Survo_data_file> TO R><R_data_frame>");
            WAIT; return;
            }
        if (muste_strcmpi(word[2],"#NAMES")==0) { load_names_to_textfile(); return; }
        tulosrivi=r1+r;
        leveys=c2;
        if (g>3)
            {
            if (g>4 && muste_strcmpi(word[3],"TO")==0)
                {
                if (*word[4]=='R' && *(word[4]+1)=='>')  // RS ADD
                	{
                	nimi=(word[4]+2);
                	sprintf(sbuf,"\nLoading observations from file %s to R data frame %s: ",word[2],nimi); 
                	sur_print(sbuf);
                	muste_Survo2R(nimi,word[2]);
                	return;
                	}
                
                i=avaa_tekstit(word[4]);
                if (i<0) return;
                tulosrivi=0;
                leveys=100*LLENGTH-1;  // oli 40 -8.1.2002
                }
            else
                {
                if (*word[3]!='/')
                    {
                    tulosrivi=edline2(word[3],1,1);
                    if (tulosrivi==0) return;
                    }
                }
            }
        nimi=word[2]; if (*nimi=='-') { ++nimi; jatko=1; } else jatko=0;
        subst_survo_path(nimi); // 20.10.2001
        i=data_open3(nimi,&d,0,1,0,0); if (i<0) { s_end(argv[1]); return; }
        if (d.type!=2) return;

        i=spec_init(r1+r-1); if (i<0) return;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);
        strcpy(missing_str,"-");
        i=spfind("MISSING");
        if (i>=0) { strcpy(sbuf,spb[i]); sbuf[31]=EOS; strcpy(missing_str,sbuf); }

        i=mask(&d); if (i<0) { s_end(argv[1]); return; }
        i=conditions(&d); if (i<0) { s_end(argv[1]); return; }
        m=d.m_act;

        i=spfind("STR_COMMA");
        if (i>=0) strcpy(str_comma,spb[i]);
        i=spfind("STR_SPACE");
        if (i>=0) strcpy(str_space,spb[i]);

        i=spfind("FORMAT");
        if (i>=0)
            {
            if (strcmp(spb[i],"LIST")==0)
                {
                format_list(nimi); s_end(argv[1]); return;
                }
            else if (strncmp(spb[i],"ORDER:",6)==0)
                { i=format_order(); if (i<0) return; }

            else if (strncmp(spb[i],"VEC:",4)==0)  // 8.5.2008
                {
                format_vec(nimi,spb[i]+4); s_end(argv[1]); return;
                }

            else
                {
                format_load(nimi); s_end(argv[1]); return;
                }
            }

        *filter1=EOS; /* 13.11.1992 */
        i=spfind("FILTER");
        if (i>=0) strcpy(filter1,spb[i]);
        if (*filter1)
            {
            i=load_codes(filter1,code); if (i<0) return;
            }

        is_delimiter=1;
        i=spfind("DELIMITER");  /* 29.9.1996 */
        if (i>=0)
            {
            strcpy(sbuf,spb[i]);
            if (*sbuf==EOS) limit_char=' ';
            else if (*sbuf==',') limit_char=',';  // 26.11.2006
            else
                {
                i=split(sbuf,ww,2);
                if (muste_strcmpi(ww[0],"TAB")==0) limit_char='\t';
                else if (muste_strnicmp(ww[0],"SP",2)==0) limit_char=' ';
                else if (muste_strnicmp(ww[0],"char(",5)==0) limit_char=atoi(ww[0]+5);
                else limit_char=*ww[0];
                if (i>1) no_last_limit=0;  /* 0=myîs viimeisen perÑÑn */


                if (muste_strcmpi(ww[0],"NULL")==0)
                    {
                    is_delimiter=0;
                    limit_char=' '; // todellisuudessa ei mitÑÑn!
                    }
                i=spfind("SPACE");
                if (i>=0)
                    space_char=*spb[i];
                }
            }

        i=spfind("LIST_VARIABLES");
        if (i>=0)
            {
            list_names_of_variables(i);
            if (!tulosrivi) muste_fclose(tekstit);
            data_close(&d);
            s_end(argv[1]);
            return;
            }

        i=spfind("NAMES8");
        if (i>=0) names8=atoi(spb[i]);
 
 		printnames=0; // RS ADD
        i=spfind("LABELS");
        if (i>=0) printnames=atoi(spb[i]);
        

        i=varaa_tilat(); if (i<0) return;
        i=etsi_muodot(); if (i<0) { data_close(&d); return; }
        k=1;
        for (i=0; i<m; ++i)
            {
            pos[i]=k; len[i]=strlen(form[i]); k+=len[i]+1;
            if (!is_delimiter) --k; // 23.4.2002
            limit_pos[i]=k-1;
            }
        if (k-1>leveys)
            {
            if (tulosrivi)
                {
                sur_print("\nToo small line length in current edit field!");
                sprintf(sbuf,"\nThe %d active fields of %s require at least %d positions.",
                            m,word[2],k-1); sur_print(sbuf);
                WAIT; return;
                }
            else
                {
                sprintf(sbuf,"\nThe %d active fields of %s require at least %d positions.",
                            m,word[2],k-1); sur_print(sbuf);
                sprintf(sbuf,"\nMax %d positions permitted.",leveys); sur_print(sbuf);
                WAIT; return;
                }
            }

        kleveys=k-1;
        if (!jatko || printnames) // RS ADD printnames
            {
            if (tulosrivi && !printnames)
                {
                hae_apu("not_used_in_FILE_LOAD",kielletty); // 11.5.2006

                label[0]=next_load_label('A');
                label[1]=next_load_label((char)(label[0]+1));
                label[2]=next_load_label((char)(label[1]+1));

                sana[0]='*'; sana[1]=','; sana[2]=label[0]; sana[3]=',';
                sana[4]=label[1]; sana[5]=','; sana[6]=label[2]; sana[7]=EOS;

                strcpy(rivi,"DATA "); strcat(rivi,word[2]); strcat(rivi,sana);
                if (tulosrivi>0 && tulosrivi<=r2) edwrite(space,tulosrivi,1);
                h=kirjoita(rivi); if (h<0) return;
                }

    /*      strncpy(rivi,space,kleveys); rivi[kleveys]=EOS;   */
            for (i=0; i<kleveys; ++i) rivi[i]=' '; rivi[kleveys]=EOS;
            for (i=0; i<m; ++i)
                {
                int vi=d.v[i];

                strcpy(sana,d.varname[vi]); sana[8]=EOS;
                if (names8)
                    {
                    if (names8==2 && i==0) // 25.3.2005 R-ohjelmaa varten
                        strncpy(rivi+i*9,space,8);
                    else
                        strncpy(rivi+i*9,sana,8);
                    if (i==m-1 && no_last_limit) continue;
                    rivi[i*9+8]=limit_char; continue;
                    }
                k=strlen(sana); while (sana[k-1]==' ') sana[--k]=EOS;
                if (d.vartype[vi][0]!='S' && k<len[i])
                    k=len[i]-k;
                else
                    k=0;

                for (h=0; h<len[i]; ++h)
                    {
                    if (sana[h]==EOS) break;
                    rivi[pos[i]+k+h]=sana[h];
                    }
/* 29.9.1996 */ if (limit_char!=' ' && i<m-no_last_limit) rivi[limit_pos[i]]=limit_char;
                }
            h=kirjoita(rivi); if (h<0) return;
            if (printnames) jatko=1; // RS ADD
            else
            	{
            	if (tulosrivi>1) *(z+(tulosrivi-2)*ed1)=label[2];
            	if (tulosrivi>0 && tulosrivi<=r2) *(z+(tulosrivi-1)*ed1)=label[0];
            	}
            }

        sprintf(sbuf,"\nLoading observations from file %s: ",nimi); sur_print(sbuf);
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            if (prind) { sprintf(sbuf,"%ld ",j); sur_print(sbuf); }
       /*   strncpy(rivi,space,kleveys); rivi[kleveys]=EOS;  */
            for (i=0; i<kleveys; ++i) rivi[i]=' '; rivi[kleveys]=EOS;

            for (i=0; i<m; ++i)
                {
                int vi=d.v[i];

                if (d.vartype[vi][0]=='S')
                    {
                    fi_alpha_load(&d.d2,j,vi,sana);
                    if (strncmp(sana,space,len[i])==0)
                        *sana='-';
/* 9.11.2002 */     if (*str_comma) str_korvaus(sana,',',*str_comma);
                    if (*str_space) str_korvaus(sana,' ',*str_space);
                    }
                else
                    {
                    fi_load(&d.d2,j,vi,&x);
                    if (x==MISSING8) { strncpy(sana,space,len[i]);
                                       k=strlen(missing_str);
                                       h=len[i]-k; if (h<0) { h=0; k=len[i]; }
                                       strncpy(sana+h,missing_str,k);
                                   //  sana[len[i]-1]='-';
                                       sana[len[i]]=EOS;
                                     }
                    else
                        fconv(x,form[i],sana);

                    if (strlen(sana)>len[i])
                        {
                        sprintf(sbuf,"\nFormat %s not wide enough for %g in variable %.8s!",
                                    form[i],x,d.varname[vi]); sur_print(sbuf);
                        sur_print("\nUse FILE STATUS and FILE UPDATE to specify a new format.");
                        WAIT; return;
                        }
                    }
                for (h=0; h<len[i]; ++h)
                    {
                    if (sana[h]==EOS) break;
                    rivi[pos[i]+h]=sana[h];
                    }
/* 29.9.1996 */ if (limit_char!=' ' && i<m-no_last_limit) rivi[limit_pos[i]]=limit_char;
                }
            h=kirjoita(rivi); if (h<0) return;
            }

        if (!jatko && tulosrivi>0) *(z+(tulosrivi-2)*ed1)=label[1];
        if (!tulosrivi) muste_fclose(tekstit);
        data_close(&d);
        s_end(argv[1]);
        }
