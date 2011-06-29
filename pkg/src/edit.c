/*  survos.c 13.9.1985/SM (21.11.1998) */
/* spec.c 16.7.85/SM (24.10.1991) (10.3.1995)
   specifications  (malloc)
*/

/*
#include <R.h>
#include <Rinternals.h>
*/

#include "survo.h"
#include "survolib.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

/* SURVO.APU-parametreille */
#define MAXTILA 10000

char *z;
int ed1,ed2,edshad;
int r,r1,r2,r3,c,c1,c2,c3;
char s_edisk[LNAME], s_esysd[LNAME], s_eout[LNAME];
char *edisk, *esysd, *eout;
int etu;
char s_etufile[LNAME];
char *etufile;
int etu1,etu2,etu3;
long tutpos;
int *zs,zshn;
int erun;
int edisp; /* 1=screen redisplayed (default)  2=only current line redisplayed */
char s_sapu[MAXTILA+2];
char *sapu;
char s_info[LLENGTH];
char *info;
char **key_label; /* ?? */
char *key_lab;    /* ?? */
char ser_number[LNAME];
char **disp_string; /* ?? */
int speclist, specmax;
char s_active_data[LNAME];
char *active_data;
int scale_check;
int accuracy, results;
int ibm;
int s_shadow_int[10];
int *shadow_int;
unsigned char s_shadow_code[256];
unsigned char *shadow_code;
char s_tut_info[LLENGTH];
char *tut_info;
char s_crt_exit[32];
char *crt_exit;
int sdisp;
int scroll_line;
int space_break;
int ntut;
int move_r1,move_r2;
char s_etmpd[LNAME];
char *etmpd,*pp_etmpd;
int sur_seed;
int *psur_seed;
char *sspace;
int computation_speed;
char s_eopen[LNAME];
char *eopen;
char s_survo_path[LNAME];
char *survo_path;
int display_off;
int sur_alarm;
char s_system_name[32];
char *system_name;
int s_cur_par[2];
int *cur_par;
int shad_off;
char s_shad_active[256];
char *shad_active;
int tut_wait_c;
long cpu_speed;
int wait_save;
char survo_type;
int loudness;
int output_level;
int mat_parser;
char os_ver[10];  /* huom.  os_ver[] */
char s_info_2[LLENGTH];
char *info_2;
char info_s[64];
int spec_check; /* 1.6.2004 */

char err_str[]=
    "This is an environment for statistical analysis SURVO MM created by Seppo Mustonen in 2000.";

int r_soft;
long inputBuffer;
int dwInputEvents;


char sbuf[LLENGTH];

char space[LLENGTH];
char comline[LLENGTH];
int g;
char *word[MAXPARM];
char sur_session[2];

/* specifications in the edit field */
char *splist;
char **spa, **spb, **spshad;
int spn;
double *arvo; /* vain arit.c tarvitsee  */
char **spb2;   /* spb-kopiot */
char *spp;
unsigned int *spplace;

/*
extern int speclist,specmax;
*/

char *spl;
int global;


/* Muuttuvat parametrit: */
int v_results,v_accuracy;

FILE *apu;
FILE *edfield;

char rivin_loppu[]="\15\12";


/* wfind.c 16.7.85/SM (28.9.85)
   wfind(word1,word2,lin) etsii sanaparin rivin alusta riviltä lin lähtien
*/
int wfind(char word1[], char word2[], int lin)
        {
        short j,g;
        char x[LLENGTH];
        char *sana[2];

        for (j=lin; j<=ed2; ++j)
                {
                edread(x,j);
                g=split(x+1,sana,2);
                if (g==0) continue;
                if (strcmp(word1,sana[0])==0)
                        {
                        if (word2==NULL) break;
                        if (g==2 && strcmp(word2,sana[1])==0) break;
                        }
                }
        if (j>ed2) return (-1);
        return (j);
        }

/* itoa:  convert n to characters in s */
char *muste_itoa(int n, char s[], int base)
{
    int c, i, j, sign;

    if ((sign = n) < 0)  /* record sign */
        n = -n;          /* make n positive */
    i = 0;
    do         /* generate digits in reverse order */
    {
        s[i++] = n % base + '0';   /* get next digit */
    }
    while ((n /= base) > 0);     /* delete it */
    if (sign < 0)
        s[i++] = '-';
    s[i] = '\0';

    for (i = 0, j = strlen(s)-1; i<j; i++, j--)
    {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
    }

    return s;
}

char *strupr(char *str)
{
    char *string = str;

    if (str)
    {
        for ( ; *str; ++str)
            *str = toupper(*str);
    }
    return string;
}


/* fconv.c 17.8.1985/SM (9.11.1989)
   fconvd(luku,muoto,sana);
          double char[] char[]
   muuntaa luvun sanaksi käyttäen muotoa muoto="1234.123" tai "%8.3f" esim.
                                         muoto="" tarkoittaa vapaata.
   return -1, jos sana ei mahdu muotoon.
*/
int fconv(double luku,char muoto[],char sana[])
{
    /*        char x[256]; */
    char *p;
    int len=strlen(muoto);
    int dec;
    /*      int kok; */

    if (*muoto==EOS)
    {
        if (fabs(luku)>=1 || fabs(luku)<1e-14)
            sprintf(sana,"%.14g",luku);
        else
        {
            if (luku==0)
            {
                strcpy(sana,"0");
                return(1);
            }
            sprintf(sana,"%.14f",luku);
            len=strlen(sana)-1;
            if (strchr(sana,'.')!=NULL)  /* 9.11.89 */
            {
                while (sana[len]=='0') sana[len--]=EOS;
                if (sana[len]=='.') sana[len]=EOS;
            }
        }
        return(1);
    }

    if (*muoto=='%')
    {
        sprintf(sana,muoto,luku);
        return(1);
    }

    p=strchr(muoto,'.');
    if (p==NULL) dec=0;
    else dec=len-1-(p-muoto);
    sprintf(sana,"%*.*f",len,dec,luku);
    if (strlen(sana)>len) return(-1);
    return(1);
}

/* fnconv.c 29.3.1986/SM (29.3.1986)
   fnconvd(luku,pituus,sana);
          double int   *char
   muuntaa luvun sanaksi, jolla on kiinteä pituus.
*/
int fnconv(double luku,int pituus,char *sana)
{
    int i;
    int kok=2;
    int des;
    double its;
    int logl;
    char muoto[LLENGTH];

    des=pituus-3;
    if (luku!=0.0)
    {
        its=luku;
        if (luku<0.0) its=-luku;
        logl=(int)log10(its);
        if (logl>0)
        {
            kok=logl+2;
            des=pituus-kok-1;
        }
        if (kok>pituus)
        {
            fconv(luku,"%g",sana);
            return(1);
        }
    }
    for (i=0; i<kok; ++i) muoto[i]='#';
    if (kok<pituus)
    {
        if (kok+1==pituus) muoto[i++]='#';
        else
        {
            muoto[i++]='.';
            for (; i<pituus; ++i) muoto[i]='#';
        }
    }
    muoto[i]=EOS;
    fconv(luku,muoto,sana);
    return(1);
}

int linerr2(char sana[])
{
    /*
              PR_EINV;
              sprintf(sbuf,"Line '%s' not found!",sana);
              if (dsp) return(1);
              sur_print("\n"); sur_print(sbuf);
              WAIT;
    */
    return(1);
}

int lastline2()
{
    int i,j;
    char space[LLENGTH];

    for (i=0; i<ed1; ++i) space[i]=' ';
    j=ed2;
    while (j>0)
    {
        if (strncmp(space,z+(j-1)*ed1+1,(unsigned int)(ed1-1))!=0) break;
        --j;
    }
    return(j);
}

int posnro(char s[])
{
    short h;
    if (*s=='0') return(0);
    for (h=0; *(s+h); ++h)
        if ((*(s+h)<'0') || (*(s+h)>'9')) return(0);
    return(1);
}

int edline2(char sana[],unsigned int lin,int virheilm)
/* lin alkurivi */
/* virheilm 1=virheilmoitus 0=ei virheilmoitusta */
{
    int j,k;
    char SANA[3];
    int lin1;    /* 5.3.91 */

    if (posnro(sana))
    {
        j=atoi(sana);
        if (j>=lin && j<=ed2) return(j);
        linerr2(sana);
        return(0);
    }
    for (j=0; j<3; ++j) SANA[j]=toupper(sana[j]);
    if (strncmp(SANA,"END",3)==0)
    {
        k=3;
        j=lastline2();
    }
    else if (strncmp(SANA,"CUR",3)==0)
    {
        k=3;
        j=r1+r-1;
    }
    else
    {
        lin1=lin-atoi(sana+1);
        if (lin1<1) lin1=1;
        for (j=lin1; j<=ed2; ++j)
            if ( *(z+(j-1)*ed1)==*sana ) break;
        if (j>ed2)
        {
            if (virheilm) linerr2(sana);
            return(0);
        }
        k=1;
    }
    if (strlen(sana)<k+2) return(j);
    j+=atoi(sana+k);
    if (j>=lin && j<=ed2) return(j);
    if (virheilm) linerr2(sana);
    return(0);
}


int hae_apu(char *s,char *t)
{
    char *p, *q;
    char sana[64];  /* 11.5.2006 */
    int len;

    strcpy(sana,s);
    strcat(sana,"=");
    len=strlen(sana);
    p=sapu;

    while ((p=strchr(p,*sana))!=NULL)
    {
        if (strncmp(sana,p,(unsigned int)len)==0)
        {
            if (p>sapu && *(p-1)!='\n')
            {
                ++p;    /* 14.7.2006 */
                continue;
            }
            p+=len;
            q=t;
            while (*p && *p!='\n')
            {
                *q=*p;
                ++q;
                ++p;
            }
            *q=EOS;
            return(1);
        }
        ++p;
    }
    return(0);
}


void edread(char *x,unsigned int lin)
{
    strncpy(x,z+(lin-1)*ed1,(unsigned int)ed1);
    x[ed1]=EOS;
}

int edwrite(char *x,unsigned int lin,unsigned int col)
{
    unsigned int i,h;
    unsigned int len=strlen(x);

    if (lin<1 || lin>(unsigned int)(ed2+edshad))
    {
        sprintf(sbuf,"Line number error! (%u)\n",lin);
        /*            if (dsp) return(1);    */
        sur_print(sbuf);
        WAIT;
        return(1);
    }
    if (len>ed1-col) len=ed1-col;
    for (i=0, h=(lin-1)*ed1+col; i<len; ++i, ++h) z[h]=x[i];
    return(1);
}

int split(char *rivi,char **sana,int max)
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
{
    int g=0;
    int p;
    int edell=0; /* väli edellä */
    int len=strlen(rivi);

    for (p=0; p<len; ++p)
    {
        if ( (rivi[p]==' ') || (rivi[p]==',') )
        {
            if (edell==1)
            {
                rivi[p]=EOS;
                ++g;
                if (g>=max) return(max);
                edell=0;
            }
        }
        else
        {
            if (edell==0)
            {
                sana[g]=rivi+p;
                edell=1;
            }
        }
    }
    if (edell==1) ++g;
    return(g);
}

int splitp(char *rivi,char **sana,int max)
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
{
    int g=0;
    int p;
    int edell=0; /* väli edellä */
    int len=strlen(rivi);
    int sulut;

    sulut=0;
    for (p=0; p<len; ++p)
    {
        if (rivi[p]=='(')
        {
            ++sulut;
            continue;
        }
        if (rivi[p]==')')
        {
            --sulut;
            continue;
        }
        if (sulut) continue;
        if ( (rivi[p]==' ') || (rivi[p]==',') )
        {
            if (edell==1)
            {
                rivi[p]=EOS;
                ++g;
                if (g>=max) return(max);
                edell=0;
            }
        }
        else
        {
            if (edell==0)
            {
                sana[g]=rivi+p;
                edell=1;
            }
        }
    }
    if (edell==1) ++g;
    return(g);
}

int sur_strcmpi(const char *s1, const char *s2)
{
    for (; *s1 && *s2 && (toupper((unsigned char)*s1) ==
                          toupper((unsigned char)*s2)); ++s1, ++s2);

    return *s1 - *s2;
}


int sur_strnicmp(const char *s1, const char *s2, int count)
{
    char c1, c2;
    int v;

    if (count == 0) return 0;

    do {
        c1 = *s1++;
        c2 = *s2++;
        v = (unsigned int)tolower(c1) - (unsigned int)tolower(c2);
    } while ((v == 0) && (c1 != '\0') && (--count > 0));
    return v;
}


int filename(char *edfile, char *field)
{
    int i;

    *edfile=EOS;
    if (strchr(field,':')==NULL)
    {
        if (*field=='.')
        {
            ++field; /* ohita piste 8.11.91 */
            strcat(edfile,survo_path);
            i=strlen(edfile)-1;
            if (edfile[i]=='\\') edfile[i]=EOS;
        }
        else strcat(edfile,edisk);
    }
    strcat(edfile,field);
    return(1);
}

int file_name_ext(char *name,char *ext)
{
    int i;
    char *p;

    i=strlen(name);
    if (i<4) p=name;
    else p=name+i-4;
    if (strchr(p,'.')==NULL)
        strcat(name,ext);
    return(1);
}

int edit_file_not_found(char *edfile)
{
    PR_EINV;
    sprintf(sbuf,"\nEdit file %s not found!",edfile);
    sur_print(sbuf);
    WAIT;
    return(0);
}

int edload32_err(char *s,char *edfile)
{
    sprintf(sbuf,"\n%s in edit file %s !",s,edfile);
    sur_print(sbuf);
    WAIT;
    return(1);
}

int ed_not_space()
{
    printf("\nNot space enough for the edit field!");
    WAIT;
    return(1);
}

int ed_malloc(unsigned int ed1,unsigned int ed2,unsigned int edshad)
{
    if (zs!=NULL) free((char *)zs);
    if (z!=NULL) free(z);
    z=(char *)malloc(sizeof(char)*ed1*(ed2+edshad));
    if (z==NULL)
    {
        ed_not_space();
        return(-1);
    }
    zs=(int *)malloc((ed2+1)*sizeof(int));
    if (zs==NULL)
    {
        ed_not_space();
        return(-1);
    }
    return(1);
}

int shadinit()
{
    unsigned int i,j;
    /*        char *p; */

    j=0;
    while (j<(unsigned int)ed2)
    {
        ++j;
        zs[j]=0;
    }
    i=ed1*ed2;
    zshn=0;
    while ( zshn<edshad && zshn<ed2 )
    {
        z[i]='\0';
        i+=ed1;
        ++zshn;
    }
    return(1);
}

int creatshad(unsigned int j)
{
    unsigned int i,k;
    char x[LLENGTH];
    /*         extern int ued1,ued2,uedshad;  */

    if (j>(unsigned int)ed2) return(-1);
    i=ed1*ed2;
    k=0;
    while ( k<(unsigned int)zshn && z[i]!='\0' )
    {
        ++k;
        i+=ed1;
    }

    zs[j]=k+ed2+1;
    strncpy(x,space,(unsigned int)ed1);
    edwrite(x,(unsigned int)zs[j],0);

    return(1);
}

int edload32(char *edfile)
{
    int i,j;
    char x[LLENGTH+10], *sana[3];
    char *p;
    int rivi_luettu;
    int ei_onnistunut;

    ei_onnistunut=0;
    rewind(edfield);  /*   PUUTTUVA  */
    fgets(x,LLENGTH+10-1,edfield);
    p=strchr(x,':');
    if (p==NULL) edit_file_not_found(edfile);

    i=split(p+1,sana,3);
    ed1=atoi(sana[0]);
    ed2=atoi(sana[1]);
    edshad=atoi(sana[2]);

    fclose(edfield);    /* suljetaan malloc-varausten tiivistämiseksi */

    i=ed_malloc((unsigned int)ed1,(unsigned int)ed2,(unsigned int)edshad);
    if (i<0)
    {
        sprintf(sbuf,"\nNot enough space for edit field %s !",edfile);
        sur_print(sbuf);
        WAIT;
        return(-1);
        ed1=101;
        ed2=100;
        edshad=30;
        ed_malloc((unsigned int)ed1,(unsigned int)ed2,(unsigned int)edshad);
        ei_onnistunut=1;
    }
    edfield=fopen(edfile,"rt");
    if (edfield==NULL)
    {
        edit_file_not_found(edfile);
        return(-1);
    }
    fgets(x,LLENGTH-1,edfield); /* otsikko uudelleen */
    c2=ed1-1;
    r2=ed2;
    strcpy(eopen,edfile);
    for (i=0; i<ed1*(ed2+edshad); ++i) z[i]=' ';
    for (i=0; i<ed1*ed2; i+=ed1) z[i]='*';
    shadinit();
    if (ei_onnistunut)
    {
        fclose(edfield);    /* ???  */
        return(-1);
    }
    rivi_luettu=0;
    j=0;
    while (1)
    {
        fgets(x,LLENGTH+10-1,edfield);
        /* printf("x=%s\n",x); getch(); */
        if (feof(edfield)) break;
        p=strchr(x,'|');
        if (p==NULL)   /*  edload32_err("Missing `|'",edfile); */
        {
            fclose(edfield);
            return(-1);
        }
        *p=EOS;
        ++p;
        i=strlen(p);
        if (p[i-1]=='\n') p[i-1]=EOS;

        if (rivi_luettu && *x=='S')
        {
            i=creatshad((unsigned int)j);
            if (i<0) break;    /* ei mahdu */
            edwrite(p,(unsigned int)zs[j],0);
            rivi_luettu=0;
            continue;
        }
        j=atoi(x);
        if (j>ed2)
        {
            /* edload32_err("Too many lines",edfile); */
            fclose(edfield);
            return(-1);
        }
        edwrite(p,(unsigned int)j,0);
        rivi_luettu=1;
    }
    fclose(edfield);
    return(1);
}


int edload(char *field,int shad)
{
    int i;
    char rivi[ELE];
    char edfile[LNAME];
    /*        char *sana[5]; int g;  */
    /*        char edtpaate[5]=".EDT";


            filename(edfile,field); file_name_ext(edfile,edtpaate);  */ /* ,".EDT" */
    strcpy(edfile,field);
    edfield=fopen(edfile,"rb");
    if (edfield==NULL)
    {
        edit_file_not_found(edfile);
        return(-1);
    }
    for (i=0; i<ELE; ++i) rivi[i]=(char)getc(edfield);
    rivi[ELE-1]=EOS;
    if (strncmp(rivi,"SURVO 98",8)==0)
    {
        i=edload32(edfile);
    }
    return(i);
}


int edsave32(char *edfile,int shad)
{
    int i,j,zsj;
    char x[LLENGTH];
    int form;
    int tyhja;
    /*        int d1,d2,d3; */

    fprintf(edfield,"SURVO 98 edit field: %d %d %d (32 bit version)%s",
            ed1,ed2,edshad,rivin_loppu);
    form=3;
    if (ed2>=1000) form=4;
    if (ed2>=10000) form=5;
    if (ed2>=100000) form=6;

    for (j=1; j<=ed2; ++j)
    {
        edread(x,(unsigned int)j);
        i=ed1-1;

        while (i>0 && x[i]==' ') --i;
        x[i+1]=EOS;
        if (i==0 && *x=='*') tyhja=1;
        else tyhja=0;
        zsj=zs[j];
        if (!tyhja || zsj)
        {
            fprintf(edfield,"%.*d|%s%s",form,j,x,rivin_loppu);
        }
        if (zsj)
        {
            edread(x,(unsigned int)zsj);
            i=ed1-1;
            while (i>0 && x[i]==' ') --i;
            x[i+1]=EOS;
            fprintf(edfield,"%-*s|%s%s",form,"S",x,rivin_loppu);
        }
    }
    fclose(edfield);
    return(1);
}



int edsave(char *field,int shad,int check)
{
    /*
            unsigned int i,k;
            char header[LLENGTH];
            char number[10];
            char x[LLENGTH];
            unsigned int j;
            short *pint;
            char ch;
    */
    char edfile[LNAME];
    /*        char edtpaate[5]=".EDT";

        filename(edfile,field); file_name_ext(edfile,edtpaate); */

    strcpy(edfile,field);
    if (check)
    {
        edfield=fopen(edfile,"rb");
        if (edfield!=NULL)
        {
            /* PUUTTUVA strcmpi korvattu sur_strcmpi:llä
                            if (sur_strcmpi(edfile,eopen)!=0 && etu!=2)
                                {
                                PR_EBLD;
                                sprintf(sbuf,"\nEdit file %s already exists! Overwrite it (Y/N)?",
                                                                                   edfile);
                                            sur_print(sbuf);
                                ch=(char)getck(); sprintf(sbuf,"%c",ch); sur_print(sbuf);
                                if (ch!='Y' && ch!='y') { fclose(edfield); return(0); }
                                }
            */
            fclose(edfield);
        }
    }

    edfield=fopen(edfile,"wb");
    if (edfield==NULL)
    {
        sprintf(sbuf,"\nCannot save %s !",edfile);
        sur_print(sbuf);
        WAIT;
        return(0);
    }

    edsave32(edfile,shad);
    return(1);
}

void edt_talletus(char *s)
{
    char snimi[LLENGTH];

    strcpy(snimi,s);
    /*       if (strchr(s,':')==NULL && strchr(s,'\\')==NULL)   */ /* 7.1.1992 */
    /*            { strcpy(snimi,etmpd); strcat(snimi,s); }
    */
    edsave(snimi,1,0);
}

int xxd(int i)
{
    fprintf(apu,"%d\n",i);
    return(1);
}

int xxl(long li)
{
    fprintf(apu,"%ld\n",li);
    return(1);
}

int xxs(char *x)
{
    fprintf(apu,"%s\n",x);
    return(1);
}

int xxe(double a)
{
    fprintf(apu,"%.16e\n",a);
    return(1);
}

int yys(char *x)
{
    char *p;

    p=x;
    while (1)
    {
        *p=(char)getc(apu);
        if (*p=='\n') break;
        ++p;
    }
    *p=EOS;

    return(1);
}

int yyl(long *pli)
{
    char x[LLENGTH];
    yys(x);
    *pli=atol(x);
    return(1);
}

int yyu(unsigned int *pi)
{
    char x[LLENGTH];
    yys(x);
    *pi=atoi(x);
    return(1);
}

int yye(double *pa)
{
    char x[LLENGTH];
    yys(x);
    *pa=atof(x);
    return(1);
}

int yyd(int *pi)
{
    char x[LLENGTH];
    yys(x);
    *pi=atoi(x);
    return(1);
}

int sur_dump(char *siirtop)
{
    int i,h;
    char x[LNAME];

    results=v_results;
    accuracy=v_accuracy;

    strcpy(x,siirtop);
    strcat(x,"SURVOMM.EDT");
    edt_talletus(x);


    strcpy(x,siirtop);
    strcat(x,"SURVOMM.DMP");
    apu=fopen(x,"wt");

    xxd(ed1);
    xxd(ed2);
    xxd(r);
    xxd(r1);
    xxd(r2);
    xxd(r3);
    xxd(r_soft);
    xxd(c);
    xxd(c1);
    xxd(c2);
    xxd(c3);
    xxs(edisk);
    xxs(esysd);
    xxs(eout);
    xxd(etu);
    xxs(etufile);
    xxd(etu1);
    xxd(etu2);
    xxd(etu3);
    xxl(tutpos);
    xxd(zshn);
    xxd(erun);
    xxd(edshad);
    /* int eddisp ?? */
    xxs(info);
    /* key_label key_lab survo_id  ??? */
    xxd(speclist);
    xxd(specmax);
    xxs(active_data);
    xxd(scale_check);
    xxd(accuracy);
    xxd(results);
    /* ibm poistetaan? */
    for (i=0; i<10; ++i) xxd(shadow_int[i]);
    for (i=0; i<256; ++i)
    {
        h=shadow_code[i];
        xxd(h);
        /*          h=shad_active[i]; xxd(h); */
    }

    xxs(tut_info);
    xxs(crt_exit);
    xxd(sdisp);
    xxd(scroll_line);
    xxd(space_break);
    xxd(ntut);
    xxd(move_r1);
    xxd(move_r2);
    xxs(etmpd);
    xxd(sur_seed);
    /* space generoidaan lapsessa */
    xxd(computation_speed);
    xxs(eopen);
    xxs(survo_path);
    xxd(display_off);
    xxd(sur_alarm);
    xxs(system_name);
    xxd(cur_par[0]);
    xxd(cur_par[1]);
    xxd(shad_off);
    xxd(tut_wait_c);
    xxl(cpu_speed);
    xxd(wait_save);
    *sbuf=survo_type;
    sbuf[1]=EOS;
    xxs(sbuf);
    xxd(loudness);
    xxd(output_level);
    xxd(mat_parser);
    xxs(os_ver);
    xxs(info_2);
    xxd(spec_check);
    fprintf(apu,"%s",sapu);
    fclose(apu);
    return(1);
}


int restore_dump(char *siirtop)
{
    char x[LNAME];
    int i,h;
    char *p;
    /*        long li;
            int k;  */

    strcpy(x,siirtop);
    strcat(x,"SURVOMM.DMP");
    apu=fopen(x,"rt");
    if (apu==NULL) return(0);
    yyd(&ed1);
    yyd(&ed2);
    yyd(&r);
    yyd(&r1);
    yyd(&r2);
    yyd(&r3);
    yyd(&r_soft);
    yyd(&c);
    yyd(&c1);
    yyd(&c2);
    yyd(&c3);
    yys(edisk);
    yys(esysd);
    yys(eout);
    yyd(&etu);
    yys(etufile);
    yyd(&etu1);
    yyd(&etu2);
    yyd(&etu3);
    yyl(&tutpos);
    yyd(&zshn);
    yyd(&erun);
    yyd(&edshad);
    yys(info);
    /* key_label key_lab survo_id  ??? */
    yyd(&speclist);
    yyd(&specmax);
    yys(active_data);
    yyd(&scale_check);
    yyd(&accuracy);
    yyd(&results);
    /* ibm poistetaan? */
    for (i=0; i<10; ++i) yyd(&shadow_int[i]);
    for (i=0; i<256; ++i)
    {
        yyd(&h);
        shadow_code[i]=h;
    }
    /*      yys(shadow_code);  po. jokainen merkki erikseen, jos tarv.? */
    yys(tut_info);
    yys(crt_exit);
    yyd(&sdisp);
    yyd(&scroll_line);
    yyd(&space_break);
    yyd(&ntut);
    yyd(&move_r1);
    yyd(&move_r2);
    yys(etmpd);
    yyd(&sur_seed);
    /* space generoidaan lapsessa */
    yyd(&computation_speed);
    yys(eopen);
    yys(survo_path);
    yyd(&display_off);
    yyd(&sur_alarm);
    yys(system_name);
    yyd(&cur_par[0]);
    yyd(&cur_par[1]);
    yyd(&shad_off);

    yyd(&tut_wait_c);
    yyl(&cpu_speed);
    yyd(&wait_save);
    yys(sbuf);
    survo_type=*sbuf;
    yyd(&loudness);
    yyd(&output_level);
    yyd(&mat_parser);
    yys(os_ver);
    yys(info_2);
    yyd(&spec_check);

    p=sapu;
    while (1)
    {
        *p=(char)getc(apu);
        if (feof(apu)) break;
        ++p;
    }
    *p=EOS;
    fclose(apu);

    strcpy(x,siirtop);
    strcat(x,"SURVOMM.EDT");
    edload(x,1);




    /*      disp();
          g=2; parm[1]=edisk; op_cd(); */

    v_results=results;
    v_accuracy=accuracy;

    return(1);
}

int s_edt(char *siirtop)
{
    restore_dump(siirtop);
    return(1);
}

int s_end(char *siirtop)
{
    sur_dump(siirtop);
    return(1);
}


int s_init(char *siirtop)
{
    int i;
    char *p;

    strcpy(sur_session,siirtop);
    *sur_session=siirtop[strlen(siirtop)-1];
    sur_session[1]=EOS;
    edisk=s_edisk;
    esysd=s_esysd;
    eout=s_eout;
    etufile=s_etufile;
    sapu=s_sapu;
    info=s_info;
    active_data=s_active_data;
    shadow_int=s_shadow_int;
    shadow_code=s_shadow_code;
    tut_info=s_tut_info;
    crt_exit=s_crt_exit;
    etmpd=pp_etmpd=s_etmpd;
    psur_seed=&sur_seed;
    sspace=space;
    eopen=s_eopen;
    survo_path=s_survo_path;
    system_name=s_system_name;
    cur_par=s_cur_par;
    shad_active=s_shad_active;
    info_2=s_info_2;

    strcpy(etmpd,siirtop); /* tilap. */

    s_edt(siirtop);

    for (i=0; i<LLENGTH; ++i) space[i]=' ';
    space[LLENGTH-1]=EOS;
    edread(comline,(unsigned int)(r1+r-1));
    p=strchr(comline,PREFIX);
    if (p==NULL) p=comline;
    g=split(p+1,word,MAXPARM);
    i=0;

    while (i<g && strcmp(word[i],"/")!=0) ++i;
    g=i;

    /*        if (console) sur_console_child_init(); */
    return(1);
}

int sp_check()
{
    int j;
    unsigned int n,tila,tila0;
    int varjo;
    char x[LLENGTH];
    char *p,*q;
 
    if (*z==' ')     /* 22.6.1998 */                            /* No check if empty control char at first line */
    {
        specmax=10000;
        speclist=1000000;
        /*
                    if (hae_apu("specmax",x)) specmax=atoi(x);
                    else specmax=10000;
                    if (hae_apu("speclist",x)) speclist=atoi(x);
                    else speclist=1000000;
        */
        /*   printf("specmax=%d speclist=%d\n",specmax,speclist); getch();  */
        return(1);
    }

    n=0;
    tila=0;
    for (j=1; j<=r2; ++j)                                       /* Loop through the edit field lines          */
    {
        edread(x,(unsigned int) j);
        *x=EOS;
        p=x+1;                                                  /* Put EOS to control column                  */
        while (1)
        {
            p=strchr(p,'=');
            if (p==NULL) break;                                 /* Next line if no more  "="-marks            */
            if (*(p+1)==EOS) break;                             /* Next line if at the end of line            */
            if (*(p+1)=='=')
            {
                p+=1;                                           /* Allow "==" and longer repeats of "="       */
                continue;
            }
            /* printf("C: j=%d pos=%d\n",j,p-x); getch(); */
            ++n;                                               /* Increase the number of found specifications */
            tila0=tila;
            varjo=zs[j];
            tila+=2;                                           /* Space for two EOS                           */
            q=p-1;
            while (*q && *q!=' ')
            {
                ++tila;                                        /* Scan left until space or EOS                */
                --q;
            }
            q=p+1;
            while (*q && *q!=' ')
            {
                ++tila;                                        /* Scan right until space or EOS               */
                ++q;
            }

            while (*(q-1)=='&')                                /* Expression splitted to several lines        */
            {
                tila+=c2;                                      /* Allow some extra space (bug fix?)           */
                ++j;
                if (j>r2) break;                               /* Go to next line and check if last           */
                edread(x,(unsigned int) j);
                q=x+1;
                while (*q && *q==' ') ++q;                     /* Skip spaces in the beginning of line        */
                if (*q==EOS) break;                            /* Next line if at the end of line             */
                while (*q && *q!=' ')
                {
                    ++tila;                                    /* Scan right until space or EOS               */
                    ++q;
                }
            }
            if (varjo) tila+=tila-tila0;                       /* Double space needed if shadows in use       */
            ++p;
        }
    }
    specmax=n;                                                 /* Number of specs and needed space            */
    speclist=tila;

    return(1);
}


int spfind(char *s) /* 4.3.1995  6.7.2000 */
        {
        int i,j;

        for (i=0; i<spn; ++i)
            {
            if (spa[i]==NULL) continue;
            if (strcmp(s,spa[i])==0) break;
            }

        if (i<spn && spb[i]==NULL) return(i);
        if (i<spn && *spb[i]!='*') return(i);
        if (i==spn) return(-1);
        for (j=0; j<spn; ++j)
                if (strcmp(spb[i],spa[j])==0) return(j);
        return(i);
        }

/* Editoriaalisen aritmeriikan spfind
int spfind(char *s)
{
    int i;
    for (i=0; i<spn; ++i)
        if (strcmp(s,spa[i])==0) return(i);
    return(-1);
}
*/

int not_enough_mem_for_spec()
{
    sur_print("\nNot enough memory for specifications!");
    WAIT;
    return(1);
}

int spxxxx()
{
    int i;

    i=spfind("SPXXXX");
    if (i<0) return(1);
    sprintf(sbuf,"\nSpec allocated: specmax=%d speclist=%d",specmax,speclist);
    sur_print(sbuf);
    sprintf(sbuf,"\n        In use:         %d          %d",spn,(int)(spl-splist));
    sur_print(sbuf);
    WAIT;
    return(1);
}


int jatkorivit(int j)
{
    char x[LLENGTH];
    char *p,*q;

    while (1)
    {
        edread(x,(unsigned int)j);
        p=x+1;
        while (*p==' ') ++p;
        q=p;
        while (*q!=' ') ++q;
        *q=EOS;
        *(spl-2)=EOS;
        --spl;
        if (spl-splist+strlen(p)+2>speclist) return(-1);
        strcat(spl-1,p);
        spl+=strlen(p);
        if (*(spl-2)!='&') break;
        ++j;
    }
    return(1);
}


int sur_instr(char s[],char c[])
{
    if (strstr(s,c)==NULL) return(-1);
    return(1);
}


int spread3(char *x,int j)
{
    int i,k,pos;
    char *p;
    char xs[LLENGTH];

    pos=1;
    while (pos<ed1)
    {
        p=strchr(x+pos,'=');                                    /* Search for "="                               */
        if (p==NULL) break;                                     /* Ready if no more found                       */
        if (*(p+1)==EOS) break;                                 /* Ready if at the end of line                  */
        if (*(p+1)=='=')
        {
            pos+=2;                                             /* Jump over "="                                */
            continue;
        }
        /* printf("j=%d pos=%d\n",j,p-x); getch(); */
        if (j==r1+r-1 && p-x==c1+c-2)
        {
            pos=p-x+1;                                          /* Skip the place of activation                */
            continue;
        }
        if (spn>=specmax) return(-spn);                         /* Return if too many specifications           */

        /* Specific for editorial arithmetics */
        spp[spn]=*(p-1);                                        /* Save char preceding "=" such as ":",".","|" */
        spplace[spn]=(unsigned int)((j-1)*ed1+p-x);             /* Edit field pointer to specification "="     */

        pos=p-x;
        i=pos-1;
        while (i>0 && x[i]!=' ') --i;                           /* Scan left                                   */
        if (spl-splist+pos-i+1>speclist) return(-spn);
        strncpy(spl,x+i+1,(unsigned int)(pos-i-1));             /* Copy specification to spl                   */
        spa[spn]=spl;
        spl+=pos-i;
        *(spl-1)=EOS;                                           /* Update pointers, spa=left side              */

        i=pos+1;
        while (i<ed1 && x[i]!=' ') ++i;                         /* Scan right                                  */
        if (spl-splist+i-pos+1>speclist) return(-spn);
        strncpy(spl,x+pos+1,(unsigned int)(i-pos-1));           /* Copy specification to spl                   */
        spb[spn++]=spl;
        spl+=i-pos;
        *(spl-1)=EOS;                                           /* Update pointers, spb=right side             */

        if (*(spl-2)=='&')
        {
            k=jatkorivit(j+1);                                  /* Expression continues on the next line       */
            if (k<0) return(-spn);
        }
        spshad[spn-1]=NULL;
        if (zs[j]!=0)                                           /* Shadows                                     */
        {
            edread(xs,(unsigned int)zs[j]);
            if (spl-splist+i-pos+1>speclist) return(-spn);
            strncpy(spl,xs+pos+1,(unsigned int)(i-pos-1));
            spshad[spn-1]=spl;
            spl+=i-pos;
            *(spl-1)=EOS;
        }

        ++pos;
    }
    return(spn);
}

int spread2(int lin,int *raja1)
{
    char raja[12];
    int j,i;
    char x[LLENGTH];

    strcpy(raja,"*..........");
    for (j=lin-1; j>0; --j)             /* Scan upwards until borderline or top of edit field */
    {
        edread(x,(unsigned int)j);
        i=sur_instr(x,raja);
        if (i>=0) break;
    }
    *raja1=j;
    for (j=*raja1+1; j<=ed2; ++j)      /* Read the specifications */
    {
        edread(x,(unsigned int)j);
        if (global==1)                 /* Zero global if found */
        {
            i=sur_instr(x,"*GLOBAL*");
            if (i>0) global=0;
        }
        i=sur_instr(x,raja);
        if (i>=0) break;
        if (j==r1+r-1) continue;       /* Skip the activated line */
        spn=spread3(x,j);
        if (spn<0) return(spn);
    }

    /*  printf("\n"); for (i=0; i<spn; ++i) printf("\n%s=%s %c %u varjo=%s",
                           spa[i],spb[i],spp[i],spplace[i],spshad[i]); getch();
    */
    return (spn);
}


int sp_init_extra(int lin,int extra_bytes,int extra_specs)
        {
/*        int tila;
        char *p;
*/
        int spn1;
        int raja1;
        char x[LLENGTH];

  /* Check the number of specifications and needed space */
        sp_check();
        speclist+=extra_bytes+4;
        specmax+=extra_specs+3;   /* +1 @r 4.10.1999 */

    /* Allocate memory for specifications */

    splist=malloc((unsigned int)speclist);
    if (splist==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spa=(char **)malloc(specmax*sizeof(char *));
    if (spa==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spb=(char **)malloc(specmax*sizeof(char *));
    if (spb==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spshad=(char **)malloc(specmax*sizeof(char *));
    if (spshad==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    arvo=(double *)malloc(specmax*sizeof(double));
    if (arvo==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }

/* Extra allocation for editorial arithmetics */
    spp=malloc((unsigned int)specmax);
    if (spp==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
    spplace=(unsigned int *)malloc(specmax*sizeof(unsigned int));
    if (spplace==NULL)
    {
        not_enough_mem_for_spec();
        return(-1);
    }
/* Siirretty arit.c:hen
    i=varaa_earg();
    if (i<0) return(-1); */

    spn=0;
    spl=splist;
    global=0;
    edread(x,(unsigned int)lin);
    spn=spread3(x,lin);		/* Specifications from the current line */
    if (spn<0)
    {
        spxxxx();
        return(spn);
    }
    spn=spread2(lin,&raja1);			/* Local specifications	*/
    if (spn<0)
    {
        spxxxx();
        return(spn);
    }
    if (raja1==0)
    {
        spxxxx();
        return(spn);
    }
    spn1=spn;
    global=1;
    spn=spread2(1,&raja1);				/* Specifications in *GLOBAL* area */
    if (spn<0)
    {
        spxxxx();
        return(spn);
    }
    if (global==1) spn=spn1;
    spxxxx();

    return(spn);
}

int sp_init(int lin)
    {
    int i;

    i=sp_init_extra(lin,60,10);

/* Väärinkirjoitettujen spesifikaatioiden arvailua
if (i>=0 && spec_check) i=spec_word_dist(spec_check);  
*/
    return(i);
    }

int spec_init(int lin)
        {
        int i;
        i=sp_init(lin);
        if (i==-2) return(-1); /* <- spec_word_dist() */

        if (i<0)
            {
            sur_print("\nToo many specifications!");
            WAIT;
            }
        return(i);
        }


static char raja[]="*..........";

static int spec_jatkorivit(char *t, int j, int len)
        {
        char x[LLENGTH];
        char *p,*q;

        while (1)
            {
            edread(x,j);
            p=x+1;
            while (*p==' ') ++p;
            q=p; while (*q!=' ') ++q; *q=EOS;
            if (strlen(t)+strlen(p)>len) return(1);
            *(t+strlen(t)-1)=EOS; strcat(t,p);
            if (*(t+strlen(t)-1)!='&') break;
            ++j;
            }
        return(1);
        }

static int spec_read3(char *s,char *t, char *x, int j, int len)
        {
        int i,pos;
        char *p;
/* RS        char xs[LLENGTH]; */
        char spa[LLENGTH];
        int len1;

        pos=1;
        while (pos<ed1)
            {
            p=strchr(x+pos,'=');
            if (p==NULL) break;
            pos=p-x; i=pos-1;
            while (i>0 && x[i]!=' ') --i;
            *spa=EOS; strncat(spa,x+i+1,pos-i-1);
            if (strcmp(spa,s)==0)
                {
                i=pos+1;
                while (i<ed1 && x[i]!=' ') ++i;
                len1=i-pos-1; if (len<len1) len1=len;
                *t=EOS; strncat(t,x+pos+1,len1);
                if (*(t+strlen(t)-1)=='&') spec_jatkorivit(t,j+1,len);
                return(1);
                }
            ++pos;
            }
        return(-1);
        }

static int spec_instr(char *s,char *c)
        {
        if (strstr(s,c)!=NULL) return(1);
        return(-1);
        }

static int spec_read2(char *s,char *t,int lin,int *raja1,int len)
        {
        int j,i;
        char x[LLENGTH];

        for (j=lin-1; j>0; --j)
            {
            edread(x,j);
            i=spec_instr(x,raja);
            if (i>=0) break;
            }
        *raja1=j;
        for (j=*raja1+1; j<=ed2; ++j)
            {
            edread(x,j);
            i=spec_instr(x,raja);
            if (i>=0) break;
            i=spec_read3(s,t,x,j,len); if (i>=0) return(1);
            }
        return (-1);
        }

static int find_global()
        {
        char *p,*q;
        char *pch;
        char ch;

        pch=z+r2*(c2+1)-1;
        ch=*pch; *pch=EOS; /* last element in edit field temporarily EOS */
        p=strstr(z,"*GLOBAL*");
        if (p==NULL) { *pch=ch; return(-1); }
        q=strstr(z,raja); *pch=ch;
        if (q==NULL || p<q) return(1);
        return(-1);
        }

int spec_find(
char *s, /* spec to be found */
char *t, /* value of spec */
int len
)
        {
        int i;
/* RS        char *p; */
        int raja1;
        char x[LLENGTH];
        int lin;

        lin=r1+r-1;
        edread(x,lin);
        i=spec_read3(s,t,x,lin,len); if (i>=0) return(1);
        i=spec_read2(s,t,lin,&raja1,len);
        if (i>=0) return(i);
        if (raja1==0) return(-1);
        i=find_global(); if (i<0) return(-1);
        i=spec_read2(s,t,1,&raja1,len);
        return(i);
        }


/*
int main(int argc, char *argv[])
{
s_init(argv[1]);
s_end(argv[1]);
return(0);
}

*/
