#include "muste.h"
/* save.c 8.3.1986/SM (23.10.1994) (21.12.1995) (28.4.1997)
   FILE SAVE
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

// 30.9.2009-
#define NIMIMAX2 128
#define NL 127  /* NL*LLENGTH=max.line length 21.12.1995 */
#define NIMIMAX 64 // RS CHA 40
#define EOS '\0'

static FILE *text;
static SURVO_DATA d2;

static int ep4;
static int *v2;  /* malloc */
static long j,j2;
static char jakso[NL*LLENGTH];
static char jakso2[LLENGTH];  /* 2.4.91 */

static int *v;
static char *nimitila;
static char **varname;
static char **erotin;
static int *pituus;
static int m,m_act;
static long l1,l2,l3,l4; /* FIRST=l1,LAST=l2,NAMES=l3(,l4) */
static char *sanatila;
static char **tsana;
static int muoto; /* 0=ei erottimia 1=erottimet */
static unsigned char code[256];
static int koodi; /* 1=koodimuunnos 0=ei */
static long paikka;
static char names[NL*LLENGTH];
static int fields;
static char cmissing[LLENGTH];
static int moodi; /* 1=oletus (vanha), 2=uusi */
static int perusmuoto;
static int nskip;
static char skip[LLENGTH];

static int fixed_delimiter=0;
static unsigned char limit_char;
static int prind=0;
static int prind_count=0;
static int suora_siirto=0; // 1.5.2001
static int skip_errors=0; // 20.11.2001

static int *kok,*des,*tyyppi,*neg;
static char *vartype,**pvartype;
static int *varlen;
static char *ntila; /* 19.4.1992 */

static char match_name[LNAME];
static int match_var,match_var2;

static int name_field;
static int new_file=0; // 28.9.2009

static char *namespace; // 30.9.2009
static char **varname2;
static int check_varnames=1;
static int max_len;
static int max_varlen;
static int muste_nofields; // RS ADD
static int muste_noformat; // RS ADD
static int muste_quotes; // RS ADD
static int muste_dec; // RS 11.3.2013
static char muste_numsep[LLENGTH]; // RS 11.3.2013
static int n_muste_numsep; // RS 11.3.2013

static char *specs0[]={ "MAXFIELDS", "PRIND", "FIRST", "LAST", "NAMES",
                "FILTER", "MISSING", "SKIP", "MODE", "FORMAT",
                "DELIMITER", "LIMIT", "SKIP_ERRORS", "MATCH",
                "VARLEN", "NEWSPACE", "NOFIELDS", "REMOVE_QUOTES",
                "NOFORMAT", "DEC","NUMSEP",
                "!"
              };
              
static char **specs;


extern void conv();
static void skip_char(char *s,char *skip);

static int split_by_char(char *rivi,char **sana,int max,char ch) // RS CHA short -> int
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1].
   Sanojen erottimena merkki ch.
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
        {
        int g=0;
        int p;
        int len;
        
        len=strlen(rivi);

        sana[g]=rivi;
        for (p=0; p<len; ++p)
                {
                if (rivi[p]==ch)
                    {
                    rivi[p]=EOS;
                    ++g;
                    if (g>=max) return(max);
                    sana[g]=rivi+p+1;
                    }
                }
        ++g;
        return(g);
        }

static int split_by_char_quotes(char *rivi,char **sana,int max,char ch) // RS
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1].
   Sanojen erottimena merkki ch.
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
        {
        int g=0;
        int p;
        int len;
		int lainaus=0;
        
        len=strlen(rivi);

        sana[g]=rivi;
        for (p=0; p<len; ++p)
                {
                if (rivi[p]=='"')
                	{
                	lainaus=1-lainaus;
                	rivi[p]=EOS;
                	if (lainaus)
                		{
                    	sana[g]=rivi+p+1;
                    	}
                	}
                else if (!lainaus && rivi[p]==ch)
                    {
                    rivi[p]=EOS;
                    ++g;
                    if (g>=max) return(max);
                    sana[g]=rivi+p+1;
                    }
                }
        rivi[len]=EOS; // RS ADD 24.5.2012        
        ++g;
        return(g);
        }


int split_quotes(char *rivi,char **sana,int max) // RS
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
{
    int g=0;
    int p;
    int edell=0; /* väli edellä */
    int len=strlen(rivi);
 
 
    int lainaus=0; // RS ADD Deal with spaces


    for (p=0; p<len; ++p)
    {
    
//Rprintf("\n%c g=%d lainaus=%d edell=%d",rivi[p],g,lainaus,edell);    
    if (rivi[p]=='"') 
    	{
   	
    	lainaus=1-lainaus;
    	rivi[p]=EOS;
    	if (lainaus)
    		{ 
        	sana[g]=rivi+p+1;
        	edell=1;
        	}
    	continue; 
    	}
		if (lainaus) continue;
		
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



/*
tpr(text)
FILE *text;
        {
        int i;
        for (i=0; i<10; ++i) Rprintf("%c",(char)getc(text)); getch();
        }
*/
static void sanatilaylitys(int maxtila)
        {
        sprintf(sbuf,"\nToo long record in text file (max length =%d  8*ep4)",
                        maxtila); sur_print(sbuf);
        WAIT;
        }


static int sa_missing(char *p)
        {
        int len,puuttuu;
        char *q;
// RS REM        extern char cmissing[];

        len=strlen(p);
        if (len<1) return(1); // RS ADD 11.3.2013
        if (*cmissing)   /* MISSING=<string> */
            {
            if (strstr(p,cmissing)!=NULL) return(1);
            return(0);
            }

        /* puuttuva esim. "   " tai " - " tai "---" tai "123-"  */
        /*                    11.5.1992            "123-" ei enÑÑ puuttuva */

        puuttuu=0;
        if (strncmp(p,space,len)==0) puuttuu=1;

            {
            q=strchr(p,'-');
            if (q!=NULL)
                {
                if (q>p && strchr(" -",*(q-1))==NULL) return(0); /* 11.5.1992 */
                if (*(q+1)==' ' || *(q+1)=='-' || q-p==len-1) puuttuu=1;
                }
            }

        return(puuttuu);
        }


static int erotin_muunnos(char *t,char *s)
        {
        char *p;

        if (muste_strcmpi(s,"LF")==0) { *t='\n'; *(t+1)=EOS; return(0); }
        *t=EOS;
        while (*s)
            {
            if (*s=='[')
                {
                p=strchr(s,']');
                if (p==NULL) { *t++=*s++; continue; }
                *p=EOS;
                if (muste_strcmpi(s+1,"LF")==0) { *t++='\n'; s=p+1; continue; }
                *t++=(unsigned char)atoi(s+1);
                s=p+1;
                continue;
                }
            if (strchr("123456789",*s)!=NULL)
                return(atoi(s));
            *t++=*s++;
            }
        *t=EOS;
        return(0);
        }


static int split1(char *rivi,char **sana,int max) // RS CHA short -> int
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   Pilkku ei ole erotin, vain ' '.
   return (sanojen lkm)
*/
        {
        int g=0;
        int p;
        int edell=0; /* vÑli edellÑ */
        int len;
        
        len=strlen(rivi);

        for (p=0; p<len; ++p)
                {
                if (rivi[p]==' ')
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

static int sasplit2(FILE *text,char *sana[],int max,char *sanatila,int maxtila,
                   char *erotin[],int pituus[],unsigned char code[])
        {
        char *p;
        int i,h;
        char merkki;
        int ch;
        int nlf;

        nlf=0;
        p=sanatila; *p=EOS;
        for (i=0; i<max; ++i)
            {
            sana[i]=p; *p=EOS;


            if (pituus[i]>0)
                {
                for (h=0; h<pituus[i]; ++h)
                    {
                    merkki=code[getc(text)];
                    if (nskip)
                        {
                        while (!feof(text) && strchr(skip,merkki)!=NULL)
                            merkki=code[getc(text)];
                        }

/*  Rprintf("\nmerkki1=%c",merkki); getch();       */
                    if (feof(text)) return(-1);
                    if (merkki=='\n') return(-2);
                    if (p-sanatila>maxtila-1) { sanatilaylitys(maxtila); return(-3); }
                    *p++=merkki;
                    }
                *p++=EOS;
/*     Rprintf("\ni=%d %s",i+1,sana[i]); getch();  */
                if (i==max-1) while (1)
                    {
                    merkki=code[getc(text)];
                    if (nskip)
                        {
                        while (!feof(text) && strchr(skip,merkki)!=NULL)
                            merkki=code[getc(text)];
                        }
                    if (feof(text)) { ++nlf; break; }
                    if (merkki!='\n') continue;
                    ++nlf; break;
                    }
                else
                    {
                    if (*erotin[i]=='\n')  /* kiint.kentÑn perÑssÑ lf */
                        {
                        ++nlf;
                        merkki=code[getc(text)];
                        if (nskip)
                            {
                            while (!feof(text) && strchr(skip,merkki)!=NULL)
                                merkki=code[getc(text)];
                            }
                        if (merkki!='\n') return(-2);
                        }
                    }
                continue;
                }
            while (1)
                {
                merkki=code[getc(text)];
                if (nskip)
                    {
                    while (!feof(text) && strchr(skip,merkki)!=NULL)
                        merkki=code[getc(text)];
                    }
/*  Rprintf("\nmerkki2=%d",(int)merkki); getch();  */
                if (feof(text)) return(-1);
                if (merkki=='\n') { ++nlf; if (*erotin[i]=='\n') break; }
                if ((*erotin[i]==EOS && merkki==' ') || strchr(erotin[i],merkki)!=NULL
                        || merkki=='\t')
                    {
                    if (*sana[i]==EOS) continue;
                    break;
                    }
                if (merkki=='\n') return(-2);
                if (p-sanatila>maxtila-1) { sanatilaylitys(maxtila); return(-3); }
                *p++=merkki;
                }
            *p++=EOS;
/*      Rprintf("\ni=%d %s",i+1,sana[i]); getch();   */

            if (i==max-1)  /* viimeisen kentÑn jÑlkeen etsii seur. rivin */
                {
                if (merkki=='\n') continue;
                while (1)
                    {
                    if (feof(text)) break;
                    merkki=code[getc(text)];
                    if (nskip)
                        {
                        while (!feof(text) && strchr(skip,merkki)!=NULL)
                            merkki=code[getc(text)];
                        }
                    if (merkki!='\n') continue;
                    break;  /* lisÑtty 22.8.87 */
                    }
                ++nlf; continue;
                }

            if (*erotin[i]==EOS)
                while (1)  /* vÑlikentÑssÑ kelaa blankojen yli seuraavaan kenttÑÑn  */
                    {
                    ch=getc(text);
                    merkki=code[ch];
                    if (nskip)
                        {
                        while (!feof(text) && strchr(skip,merkki)!=NULL)
                            {
                            ch=getc(text);
                            merkki=code[ch];
                            }
                        }

                    if (feof(text))
                      {
                        if (i==max-1) { return(1); } 
                        else { return(-1); }
                      }  
                    if (merkki==' ') continue;
                    ungetc(ch,text);
                    break;
                    }
            }
        return(nlf);
        }

static int sasplit(FILE *text,char *sana[],int max,char *sanatila,int maxtila,
                   char *erotin[],int pituus[],unsigned char code[])
        {
        char *p;
        int i,h;
        char merkki;
        int ch;
        int nlf;

        nlf=0;
        p=sanatila; *p=EOS;
        for (i=0; i<max; ++i)
            {
            sana[i]=p; *p=EOS;


            if (pituus[i]>0)
                {
                for (h=0; h<pituus[i]; ++h)
                    {
                    merkki=code[getc(text)];
                    if (nskip)
                        {
                        while (!feof(text) && strchr(skip,merkki)!=NULL)
                            merkki=code[getc(text)];
                        }
/*  Rprintf("\nmerkki1=%c",merkki); getch(); */
                    if (feof(text)) return(-1);
                    if (merkki=='\n') return(-2);
                    if (p-sanatila>maxtila-1) { sanatilaylitys(maxtila); return(-3); }
                    *p++=merkki;
                    }
                *p++=EOS;
/*     Rprintf("\ni=%d %s",i+1,sana[i]); getch();  */
                if (i==max-1) while (1)
                    {
                    merkki=code[getc(text)];
                    if (nskip)
                        {
                        while (!feof(text) && strchr(skip,merkki)!=NULL)
                            merkki=code[getc(text)];
                        }
                    if (feof(text)) { ++nlf; break; }
                    if (merkki!='\n') continue;
                    ++nlf; break;
                    }
                else
                    {
                    if (*erotin[i]=='\n')  /* kiint.kentÑn perÑssÑ lf */
                        {
                        ++nlf;
                        merkki=code[getc(text)];
                        if (nskip)
                            {
                            while (!feof(text) && strchr(skip,merkki)!=NULL)
                                merkki=code[getc(text)];
                            }
                        if (merkki!='\n') return(-2);
                        }
                    }
                continue;
                }

            while (1)
                {
                merkki=code[getc(text)];
                if (nskip)
                    {
                    while (!feof(text) && strchr(skip,merkki)!=NULL)
                        merkki=code[getc(text)];
                    }
/*  Rprintf("\nmerkki2=%d",(int)merkki); getch();  */
                if (feof(text)) return(-1);
                if (merkki=='\n') ++nlf;
                if (merkki==' ' || strchr(erotin[i],merkki)!=NULL
                        || merkki=='\t')
                    {
                    if (*sana[i]==EOS) continue;
                    break;
                    }
                if (merkki=='\n') return(-2);
                if (p-sanatila>maxtila-1) { sanatilaylitys(maxtila); return(-3); }
                *p++=merkki;
                }
            *p++=EOS;
/*      Rprintf("\ni=%d %s",i+1,sana[i]); getch();   */

            if (i==max-1)  /* viimeisen kentÑn jÑlkeen etsii seur. rivin */
                {
                if (merkki=='\n') continue;
                while (1)
                    {
                    if (feof(text)) break;
                    merkki=code[getc(text)];
                    if (nskip)
                        {
                        while (!feof(text) && strchr(skip,merkki)!=NULL)
                            merkki=code[getc(text)];
                        }
                    if (merkki!='\n') continue;
                    break;  /* lisÑtty 22.8.87 */
                    }
                ++nlf; continue;
                }
            while (1)  /* vÑlikentÑssÑ kelaa erottimien yli seuraavaan kenttÑÑn */
                {
                ch=getc(text);
                merkki=code[ch];
                if (nskip)
                    {
                    while (!feof(text) && strchr(skip,merkki)!=NULL)
                        {
                        ch=getc(text);
                        merkki=code[ch];
                        }
                    }
                if (merkki=='\n') ++nlf;
                if (feof(text))
                  {
                    if (i==max-1) { return(1); } 
                    else { return(-1); }
                  }
                if (merkki==' ' || strchr(erotin[i],merkki)!=NULL
                        || merkki=='\t') continue;
                if (merkki=='\n') return(-2);
                ungetc(ch,text);
                break;
                }
            }
        return(nlf);
        }


static void tilanpuute()
        {
        PR_EBLD;
        sur_print("\nNot enough memory for FILE SAVE!");
        PR_ENRM; WAIT;
        }

static void aseta_n()
        {
        fi_rewind(&d2.d2);
        fi_puts(&d2.d2,(char *)&j2,4,22L); // RS 28.1.2013 (char *)
        }

static void sulje()
        {
        muste_fclose(text);
        data_close(&d2);
        }

static int varaa_tilat()
        {
        int i;
        char sana[16];
        int ep41;

        ep4=EP4; i=hae_apu("ep4",sana); if (i) ep4=atoi(sana);
        i=spfind("MAXFIELDS");
        if (i>=0) ep4=atoi(spb[i]);
        ep41=ep4+1;

        v2=(int *)muste_malloc(ep41*sizeof(int));
        if (v2==NULL) { tilanpuute(); return(-1); }
        v=(int *)muste_malloc(ep41*sizeof(int));
        if (v==NULL) { tilanpuute(); return(-1); }
        nimitila=muste_malloc(8*ep41);
        if (nimitila==NULL) { tilanpuute(); return(-1); }
        varname=(char **)muste_malloc(ep41*sizeof(char *));
        if (varname==NULL) { tilanpuute(); return(-1); }
        erotin=(char **)muste_malloc(ep41*sizeof(char *));
        if (erotin==NULL) { tilanpuute(); return(-1); }
        pituus=(int *)muste_malloc(ep41*sizeof(int));
        if (pituus==NULL) { tilanpuute(); return(-1); }
        sanatila=muste_malloc(8*ep41);
        if (sanatila==NULL) { tilanpuute(); return(-1); }
        tsana=(char **)muste_malloc(ep41*sizeof(char *));
        if (tsana==NULL) { tilanpuute(); return(-1); }
        return(1);
        }

static void nimiylitys()
        {
        sur_print("\nOverflow of text of FIELDS list in FILE SAVE (max 8*ep4)");
        WAIT;
        }

static void format_error()
        {
// RS REM        char x[LLENGTH];
        long paikka2,pos;
        char ch;

        sprintf(sbuf,"\nFormat error in text file %s:",word[2]); sur_print(sbuf);
        sprintf(sbuf,"\nErroneous record: (First line #%ld)\n",j); sur_print(sbuf);
        paikka2=muste_ftell(text);
        muste_fseek(text,paikka,0);
        for (pos=paikka; pos<paikka2; ++pos)
            {
            ch=(char)getc(text);
            sprintf(sbuf,"%c",ch); sur_print(sbuf);
            }
        WAIT;
        }

static int varaa_tilat2()
        {

/*  aikaisemmin
int kok[EP4],des[EP4],tyyppi[EP4],neg[EP4];
char vartype[EP4][9], *pvartype[EP4];
int varlen[EP4];
*/
        kok=(int *)muste_malloc(ep4*sizeof(int));
        if (kok==NULL) { tilanpuute(); return(-1); }
        des=(int *)muste_malloc(ep4*sizeof(int));
        if (des==NULL) { tilanpuute(); return(-1); }
        tyyppi=(int *)muste_malloc(ep4*sizeof(int));
        if (tyyppi==NULL) { tilanpuute(); return(-1); }
        neg=(int *)muste_malloc(ep4*sizeof(int));
        if (neg==NULL) { tilanpuute(); return(-1); }
        vartype=(char *)muste_malloc(ep4*9);
        if (vartype==NULL) { tilanpuute(); return(-1); }
        pvartype=(char **)muste_malloc(ep4*sizeof(char **));
        if (pvartype==NULL) { tilanpuute(); return(-1); }
        varlen=(int *)muste_malloc(ep4*sizeof(int));
        if (varlen==NULL) { tilanpuute(); return(-1); }
        ntila=(char *)muste_malloc(ep4*NIMIMAX);     /* 19.4.1992 */
        if (ntila==NULL) { tilanpuute(); return(-1); }

        return(1);
        }

static int tvarfind(char *nimi)
        {
// RS REM        int len;
        int i;

        for (i=0; i<m_act; ++i)
            {
            if ( strcmp(nimi,varname[v[i]])==0 ) return(v[i]);
            }

        return(-1);
        }


/*
int apukoe(SURVO_DATA *d)
        {
        int i;

        for (i=0; i<d->m; ++i)
            {
//            if ( strncmp(nimi,d->varname[i],(unsigned int)len)==0 &&
Rprintf("\nvarname : %s",d->varname[i]);
Rprintf("\nvarname2: %s",d2.varname[i]);
			}
		return(1);
		}
*/

static int tutki_muuttujat()
        {
        int i,h,j;

        if (fields==0 && new_file==1) // 29.8.2009
            {
            for (i=0; i<m_act; ++i) v2[i]=i;
            return(1);
            }

        for (i=0; i<m_act; ++i)
            {
            
            h=varfind(&d2,varname[i]); if (h<0) { sulje(); return(-1); }
            v2[i]=h;
            
            for (j=0; j<i; j++) // RS ADD
            	{           	
            	if (v2[i]==v2[j])
            		{            		
            		h=varfindlong(&d2,varname[i],64); if (h<0) { sulje(); return(-1); }
            		v2[i]=h;
            		break;
            		}
            	}
            
            }
        return(1);
        }

static int etsi_rivi(long l1)
        {
        long j;
        char *p;
        int i;

        sprintf(sbuf,"\nSearching for line %ld in text file %s: ",l1,word[2]);
                sur_print(sbuf);
                                 
        j=0L;
        while (j<l1-1)
            {
            p=fgets(jakso,NL*LLENGTH,text);
            if (p==NULL)
                {
                sprintf(sbuf,"\nFIRST line %ld not found in %s!",l1,word[2]);
                sur_print(sbuf); WAIT; sulje(); return(-1);
                }              
            ++j;
            if (*names==EOS && j==l3)
                {
                for ( ; j<=l4; ++j)
                    {
                    i=strlen(jakso)-1;
                    if (jakso[i]=='\n') jakso[i]=' '; else strcat(jakso," ");
                    strcat(names,jakso);
                    if (j==l4) break;
                    fgets(jakso,NL*LLENGTH,text);
                    if (strlen(names)+strlen(jakso)>NL*LLENGTH)
                        {
                        sprintf(sbuf,"\nMore than %d bytes on NAMES lines %ld,%ld !",
                                             NL*LLENGTH,l3,l4);
                        sur_print(sbuf); WAIT; sulje(); return(-1);
                        }
                    }
                }
            if (prind)
                {
                ++prind_count;
                if (prind_count==prind)
                    {
                    sprintf(sbuf,"%ld ",j); sur_print(sbuf);
                    prind_count=0;
                    }
                }
            }
        return(1);
        }


static int match_copy()
        {
        int i,h,k;
        char vert[LLENGTH];
// RS REM        int vertpit;
        char x[LLENGTH];
        char *p;
        int ii;
        int nummatch;
        double x1=0,x2;
// RS REM        char *apu[2];

        if (!fields)
            {
            sur_print("\nIf MATCH is used, also a FIELDS list must be given!");
            WAIT; return(-1);
            }
        nummatch=0;
        i=spfind("MATCH"); if (i<0) return(-1);
        if (spb[i][0]=='#' && spb[i][1]==EOS) /* nro:n mukaan */
            match_var=match_var2=-1;
        else
            {
            strcpy(match_name,spb[i]);
            match_var=tvarfind(match_name);

            if (match_var<0)
                {
                sprintf(sbuf,"\nMATCH field %s not found in text file %s",match_name,word[2]);
                sur_print(sbuf); WAIT; return(-1);
                }

            }

        i=data_open(word[3],&d2); if (i<0) return(-1);
        if (d2.type!=2)
            {
            sprintf(sbuf,"\nDestination %s must be a data file!",word[3]);
            sur_print(sbuf); WAIT; sulje(); return(-1); // RS ADD sulje()
            }
        if (match_var>=0)
            {
            match_var2=varfind2(&d2,match_name,0);
            if (match_var2<0)
                {
                sprintf(sbuf,"\nMATCH field %s not in data file %s",match_name,word[3]);
                sur_print(sbuf); WAIT; sulje(); return(-1); // RS ADD sulje
                }
            if (d2.vartype[match_var2][0]=='S') nummatch=0; else nummatch=1;
            }

        i=tutki_muuttujat(); if (i<0) return(-1);

        if (l1>1L) { i=etsi_rivi(l1); if (i<0) { sulje(); return(-1); } } // RS ADD sulje()
        prind_count=0;
        sprintf(sbuf,"\n%d active fields to be copied",m_act); sur_print(sbuf);
        sprintf(sbuf,"\nCopying records from %s to %s:",word[2],word[3]); sur_print(sbuf);

        j2=0L;
        if (match_var==-1) j2=d2.l1-1L;
        for (j=l1; j<=l2; j+=(long)ii)
            {
            if (!muoto)
                {
                p=fgets(jakso,LLENGTH,text);
                if (p==NULL) break;
                i=strlen(jakso);  while (jakso[i-1]=='\n' || jakso[i-1]=='\r') jakso[--i]=EOS; // RS ADD \r
        if (koodi) conv(jakso,code); // RS ADD
        if (nskip) skip_char(jakso,skip); // RS ADD 
        		if (muste_quotes) // RS ADD
        			{
        			if (fixed_delimiter) k=split_by_char_quotes(jakso,tsana,m,limit_char);
                	else k=split_quotes(jakso,tsana,m);
        			}
        		else
        			{
/* 16.3.1996 */ 	if (fixed_delimiter) k=split_by_char(jakso,tsana,m,limit_char);
                	else k=split(jakso,tsana,m);
                	}
        /*      k=split(jakso,tsana,m);  */
                if (k<m)
                    {
                    sprintf(sbuf,"\nNot enough fields on line %ld in text file %s",
                                    j,word[2]); sur_print(sbuf);
                    WAIT; sulje(); return(-1);
                    }
                ii=1;
                }
            else
                {
                paikka=muste_ftell(text);
                if (moodi==1)
                    ii=sasplit(text,tsana,m,sanatila,8*ep4,erotin,pituus,code);
                else
                    ii=sasplit2(text,tsana,m,sanatila,8*ep4,erotin,pituus,code);

                if (ii==-1) break;
                if (ii==-2) { format_error(); sulje(); return(-1); } // RS ADD sulje()
                if (ii==-3) { sulje(); return(-1); } // RS ADD sulje()
                }

            if (match_var>=0)
                {
                strcpy(x,tsana[match_var]);
                if (nummatch) x1=atof(x);
                }
            while (1)
                {
                ++j2;
                if (j2>d2.n)
                    {
                    sprintf(sbuf,"\nRecord #%ld of %s not found in %s",
                        j,word[2],word[3]); sur_print(sbuf);
                    if (match_var>=0)
                        {
                        sur_print("\nMATCH value is ");
                            sprintf(sbuf,"%s",tsana[match_var]); sur_print(sbuf);
                        }
                    WAIT; sulje(); return(-1);
                    }
                if (match_var<0) break;
                if (nummatch)
                    {
                    data_load(&d2,j2,match_var2,&x2);
                    if (x1==x2) break;
                    }
                else
                    {
                    data_alpha_load(&d2,j2,match_var2,vert);
                    i=strlen(vert); while (vert[i-1]==' ') vert[--i]=EOS;
                    if (strcmp(x,vert)==0) break;
                    }
                }
// 2.3.2001 if (kbhit()) { i=getch(); if (i=='.') prind=1-prind; }
            if (prind)
                {
                ++prind_count;
                if (prind_count==prind)
                    {
                    sprintf(sbuf,"%ld ",j); sur_print(sbuf);
                    prind_count=0;
                    }
                }

            for (i=0; i<m_act; ++i)
                {
                int vi;

                vi=v[i];
                if (d2.vartype[v2[i]][0]=='S')
                    {
                    strcpy(jakso2,tsana[vi]);
                    for (h=strlen(jakso2); h<d2.varlen[v2[i]]; ++h)
                        jakso2[h]=' ';
                    fi_alpha_save(&d2.d2,j2,v2[i],jakso2);
                    }
                else
                    {
                    double x;

                    if (sa_missing(tsana[vi]))
                        fi_miss_save(&d2.d2,j2,v2[i]);
                    else
                        { x=atof(tsana[vi]);  fi_save(&d2.d2,j2,v2[i],&x); }
                    }
                }
            }
        sulje();
        return(1);
        }

static void skip_char(char *s,char *skip)
        {
        char *p,*q;

        p=q=s;
        while (*p)
            {
            if (strchr(skip,*p)==NULL) { *q=*p; ++q; }
            ++p;
            }
        *q=EOS;
        }

static int lue_seuraava_rivi(long j,char *jakso,char **tsana)
/* j vain virheilm.varten */
        {
        int i,k;
        char *p;

        p=fgets(jakso,NL*LLENGTH,text);
        if (p==NULL) return(-1);
        i=strlen(jakso);  while (jakso[i-1]=='\n' || jakso[i-1]=='\r') jakso[--i]=EOS; // RS ADD \r
        if (koodi) conv(jakso,code);
        if (nskip) skip_char(jakso,skip);

				if (muste_quotes) // RS ADD
        			{
        			if (fixed_delimiter) k=split_by_char_quotes(jakso,tsana,m,limit_char);
                	else k=split_quotes(jakso,tsana,m);
        			}
        		else
        			{
/* 16.3.1996 */ 	if (fixed_delimiter) k=split_by_char(jakso,tsana,m,limit_char);
                	else k=split(jakso,tsana,m);
                	}
/*      k=split(jakso,tsana,m); */

        if (k<m && skip_errors==2) // 25.8.2002
            {
            for (i=k; i<m; ++i) tsana[i]=" ";
            k=m;
            }

        if (k<m && !skip_errors)
            {
            sprintf(sbuf,"\nNot enough fields on line %ld in text file %s (%d<%d)",
                            j,word[2],k,m);  sur_print(sbuf);
            WAIT; sulje(); return(-1);
            }
        return(1);
        }


static int tutki_perusmuoto()
        {
        int i;

        i=lue_seuraava_rivi(1L,jakso,tsana); if (i<0) return(-1);
        for (i=0; i<m; ++i) if (muste_isnumber_dec(tsana[i],muste_dec)) return(1);
        i=lue_seuraava_rivi(2L,jakso,tsana); if (i<0) return(-1);
        for (i=0; i<m; ++i) if (muste_isnumber_dec(tsana[i],muste_dec)) break;
        if (i==m) return(1); /* 2.rivissÑ ei lukuja */
        l1=2L; l3=1L;
        rewind(text);
        i=lue_seuraava_rivi(1L,names,varname); if (i<0) return(-1);
        return(1);
        }

static int edit_varnames(char **varname,int m)
    {
    char *p;
    char ch;
    int i,j,ok; // RS REM ,len;
// RS REM    char s[LNAME];
    int nimimax2;
    int k,ic;
    char cc[]="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";


    nimimax2=NIMIMAX2; if (max_varlen+32>nimimax2) nimimax2=max_varlen+32;
    namespace=(char *)muste_malloc(m*nimimax2);
    if (namespace==NULL) { tilanpuute(); return(-1); }
    varname2=(char **)muste_malloc(m*sizeof(char **));
    if (varname2==NULL) { tilanpuute(); return(-1); }

    p=namespace;
    for (i=0; i<m; ++i)
        {
        varname2[i]=p; strcpy(p,varname[i]);
        p+=nimimax2;
        }
// Rprintf("\nvarnames:");
    for (i=0; i<m; ++i)
        {
        p=varname2[i];
        for (j=strlen(p); j<9; ++j) varname2[i][j]=' ';

        for (j=0; j<8; ++j)
            if (strchr("()[]*^><|%=#?&+-/,",varname2[i][j])!=NULL)
                     varname2[i][j]=' ';
        if (*varname2[i]==' ') *varname2[i]='X';

        if (i==0) continue;
        ok=0; k=0;
        while (ok==0)
            {
            for (j=i-1; j>=0; --j)
                {
                ok=1;
                if (strncmp(varname2[i],varname2[j],8)==0)
                    {
                    ++k; if (k>52) { ok=1; break; } // luopuminen
                    ok=0;
                    ch=varname2[i][7];
                    if (ch<'A'|| ch>'z') ch=cc[0];
                    else if (ch>'Z' && ch<'a') ch=cc[0];
                    else
                        {
                        p=strchr(cc,ch); if (p==NULL) { ok=1; break; }
                        ic=p-cc;
                        ch=cc[ic];
                        if (ch=='z') ch=cc[0];
                        else { ++ic; ch=cc[ic]; }
                        }

                    varname2[i][7]=ch;
                    break;
                    }
                }
            } // while
        }

    for (i=0; i<m; ++i)
        {
        p=varname2[i];
        varname2[i][8]=' '; varname2[i][9]=EOS;
        for (j=7; j>=0; --j)
            if (p[j]!=' ') break;
        for (; j>=0; --j)
            if (p[j]==' ') p[j]='_';
// Rprintf("\n%d: %s|",i,varname2[i]); getch();
        }

    for (i=0; i<m; ++i)
        {
        strcat(varname2[i],varname[i]);
        varname[i]=varname2[i];
// Rprintf("\n%d %d: %s",i,strlen(varname[i]),varname[i]); getch();
        }

// getch();
    return(1);
    }


static int tutki_textdata()
        {
        int i,k,len;
        char *p,*q;
        int ii;
        char x[LLENGTH];
/*      int kok1,des1,tyyppi1,neg1;     */

/*      i=spfind("STYPE");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            if (strchr("1248S",*x)==NULL)
                {
                sur_print("\nPermitted STYPE values are 1,2,4,8,S<integer>");
                WAIT; return(-1);
                }
            switch (*x)
                {
            case '1':
                kok1=2; des1=0; tyyppi1=1; neg1=0;
                break;
            case '2':
                kok1=3; des1=0; tyyppi1=1; neg1=1;
                break;
            case '4':
                kok1=3; des1=1; tyyppi1=1; neg1=1;
                break;
            case '8':
                kok1=7; des1=1; tyyppi1=1; neg1=1;
                break;
            case 'S':
                k=atoi(x+1); if (k<=0) k=1; if (k>64) k=64;
                kok1=k; des1=0; tyyppi1=2; neg1=0;
                break;
                }
            for (i=0; i<m_act; ++i)
                { kok[i]=kok1; des[i]=des1; tyyppi[i]=tyyppi1; neg[i]=neg1; }

            return(1);
            }
*/
        sprintf(sbuf,"\nTesting structure of data matrix %s...\n",word[2]);
            sur_print(sbuf);

        if (perusmuoto)
            {
            i=tutki_perusmuoto();
            if (i<0) return(-1);
            rewind(text);
/*          if (l1>1L) etsi_rivi(l1);   19.4.1992  */
            }

        i=spfind("CHECK_VAR_NAMES"); // 30.9.2009
        if (i>=0) check_varnames=atoi(spb[i]);

        if (check_varnames)
            { i=edit_varnames(varname,m_act); if (i<0) return(-1); }
            
            
        if (m_act>ep4)
            {
            sprintf(sbuf,"\nToo many (fields) columns in text file! (max=%d)",ep4);
            sur_print(sbuf);
            sprintf(sbuf,"\nUse the MAXFIELDS=<#_of_fields> specification!");
            sur_print(sbuf); WAIT; return(-1);
            }
        for (i=0; i<m_act; ++i) kok[i]=des[i]=tyyppi[i]=neg[i]=0;
                   /* tyyppi: 0=pos.luku 1=luku 2=string */
        k=l3; l3=0;  /* 6.8.1998 */

        i=etsi_rivi(l1); if (i<0) return(-1);
        l3=k;
        for (j=l1; j<=l2; j+=(long)ii)
            {          
            
            if (!muoto)
                {
                p=fgets(jakso,NL*LLENGTH,text);
                if (p==NULL) break;
                i=strlen(jakso); while (jakso[i-1]=='\n' || jakso[i-1]=='\r') jakso[--i]=EOS; // RS ADD \r   11.3.2013: =='\n' || jakso[i-1]=='\r' -> <32 || jakso[i-1]==limit_char
                if (koodi) conv(jakso,code);
                if (nskip) skip_char(jakso,skip);
                if (n_muste_numsep) skip_char(jakso,muste_numsep); // RS 11.3.2013
                
                
				if (muste_quotes) // RS ADD
        			{
        			if (fixed_delimiter) k=split_by_char_quotes(jakso,tsana,m,limit_char);
                	else k=split_quotes(jakso,tsana,m);
        			}
        		else
        			{
/* 16.3.1996 */ 	if (fixed_delimiter) k=split_by_char(jakso,tsana,m,limit_char);
                	else k=split(jakso,tsana,m);
                	}
                if (k<m && !skip_errors) // 20.11.2001
                    {
                    sprintf(sbuf,"\nNot enough fields on line %ld in text file %s (%d<%d)",
                                    j,word[2],k,m);  sur_print(sbuf);
                    WAIT; sulje(); return(-1);
                    }
                ii=1;
                }
            else
                {
                paikka=muste_ftell(text);
                if (moodi==1)
                    ii=sasplit(text,tsana,m,sanatila,8*ep4,erotin,pituus,code);
                else
                    ii=sasplit2(text,tsana,m,sanatila,8*ep4,erotin,pituus,code);
/*
    Rprintf("\ni=%d",i);
    for (k=0; k<m; ++k) Rprintf(" %s",tsana[k]); Rprintf("\n"); getch();
*/
                if (ii==-1) break;
                if (ii==-2) { format_error(); return(-1); }
                if (ii==-3) return(-1);
                }

// 2.3.2001 if (kbhit()) { i=getch(); if (i=='.') prind=1-prind; }
            if (prind)
                {
                ++prind_count;
                if (prind_count==prind)
                    {
                    sprintf(sbuf,"%ld ",j); sur_print(sbuf);
                    prind_count=0;
                    }
                }

            for (i=0; i<m_act; ++i)
                {
                strcpy(jakso,tsana[v[i]]);
       /*       if (k<0) return(-1);     */
                if (strlen(jakso)<1 || sa_missing(jakso)) continue;    
                if (tyyppi[i]==2)
                    {
                    len=strlen(jakso); if (len>kok[i]) kok[i]=len;
                    continue;
                    }
                if (muste_isnumber_dec(jakso,muste_dec))
                    {
                    p=jakso; while (*p==' ') ++p;
                    len=strlen(p); while (p[len-1]==' ') p[--len]=EOS;
                    if (*p=='+') ++p;
                    else if (*p=='-') { ++p; tyyppi[i]=1; neg[i]=1; }
                    q=strchr(p,muste_dec); // RS 11.3.2013 '.' -> muste_dec
                    if (q==NULL)
                        {
                        k=strlen(p); if (k>kok[i]) kok[i]=k;
                        }
                    else
                        {
                        k=q-p; if (k>kok[i]) kok[i]=k;
                        k=strlen(p)-(k+1); if (k>des[i]) des[i]=k;
                        }
                    }
                else
                    {
                    tyyppi[i]=2;
                    len=strlen(jakso); if (len>kok[i]) kok[i]=len;
                    }
                }
            }

        rewind(text);
        p=ntila;
        max_len=0;        
        
        for (i=0; i<m_act; ++i)       /* (###.##) formaatit  19.4.1992 */
            {           
            if (tyyppi[i]==2) continue;
            len=kok[i]+neg[i];
            k=len;
            if (des[i]) k+=des[i]+1;
            if (check_varnames) // 30.9.2009
                {
                ii=7; while (1) { if (varname[i][ii]==' ') --ii; else break; }
                ii=ii+1-k;
                }
            else ii=strlen(varname[i])-k;
            if (ii>0) len+=ii;

            *x=EOS;  // 9/2009          
            x[0]='('; ii=0;
            for (k=0; k<len; ++k) x[++ii]='#';
            if (des[i]>0)
                {
                x[++ii]='.';
                for (k=0; k<des[i]; ++k) x[++ii]='#';
                }
            x[++ii]=')'; x[++ii]=EOS;

            if (check_varnames) // 10/2009
                {
                strcat(varname[i]," ");
                strcat(varname[i],x);
                ii=strlen(varname[i]);
                if (ii>max_len) max_len=ii;
                }
            else
                {
/*                
                len=strlen(x);
                if (len>NIMIMAX-9) continue;
                k=sprintf(p,"%-8.8s %s",varname[i],x);
                while (*p==' ') ++p; // 30.8.2008
                varname[i]=p;
                p+=k+1;
                }
*/            
            
            	len=strlen(x)+strlen(varname[i]); // RS ADD +strlen(varname[i])
            	if (len>NIMIMAX) continue;
//            if (len>NIMIMAX-9) continue;
            	if (strlen(varname[i])<=8)  // RS ADD
            		k=sprintf(p,"%-8.8s %s",varname[i],x);
				else k=snprintf(p,NIMIMAX,"%s %s",varname[i],x);

            	while (*p==' ') ++p; // 30.8.2008

            	varname[i]=p;
// Rprintf("\ni=%d varname=%s|",i,varname[i]); // getch();
            	p+=k+1;
            	}
            }            

/*
Rprintf("\ntyypit:");
for (i=0; i<m_act; ++i) Rprintf("\ni=%d tyyppi=%d kok=%d des=%d neg=%d",
                              i+1,tyyppi[i],kok[i],des[i],neg[i]);
*/
        return(1);
        }


static int luo_uusi()
        {
        int i;
        int filen,fim1,fim,fil,fiextra,fitextn,fitextlen;
        char **fitext;
        char *privi[1];
        char xx[LLENGTH], *xosa[2];
/*        
        int max_varlen;

        max_varlen=64;
        i=spfind("VARLEN"); if (i>=0) max_varlen=atoi(spb[i]);
*/
        sprintf(sbuf,"\nSince Survo data file %s does not exist,",word[3]); sur_print(sbuf);
        sur_print("\ncreating a new one...");
        i=varaa_tilat2(); if (i<0) return(-1);
        i=tutki_textdata(); if (i<0) return(-1);

        fim=m_act;
        for (i=0; i<fim; ++i)
            {
            strncpy(vartype+i*9,space,8); vartype[i*9+8]=EOS;
            vartype[i*9+1]='A';
            if (tyyppi[i]==2)
                {
                vartype[i*9+0]='S'; varlen[i]=kok[i];
                if (varlen[i]>max_varlen)
                    {
                    sprintf(sbuf,"\nThe length of the field %s (%d) is more than %d.",
                                        varname[i],varlen[i],max_varlen);
                    sur_print(sbuf);
                    sprintf(sbuf,"\nYou may increase the limit by specification VARLEN=%d",
                                        varlen[i]);
                    sur_print(sbuf);
                    sur_print("\nHowever, field lengths greater than 64 should be avoided.");
                    WAIT; return(-1);
                    }
                continue;
                }
            if (des[i]>0)
                {
                if (kok[i]+des[i]>6)
                    { vartype[i*9+0]='8'; varlen[i]=8; continue; }
                vartype[i*9+0]='4'; varlen[i]=4; continue;
                }
            else
                {
                if (kok[i]>4)
                    { vartype[i*9+0]='8'; varlen[i]=8; continue; }
                if (kok[i]<3 && neg[i]==0)
                    { vartype[i*9+0]='1'; varlen[i]=1; continue; }
                vartype[i*9+0]='2'; varlen[i]=2; continue;
                }
            }

        for (i=0; i<m_act; ++i) pvartype[i]=vartype+i*9;

        filen=0;
        for (i=0; i<fim; ++i) filen+=varlen[i];

        i=spfind("NEWSPACE");  /* 23.10.1994 */
        if (i>=0)
            {
            strcpy(xx,spb[i]); i=split(xx,xosa,2);
            if (i<2)
                {
                sur_print("\nError in NEWSPACE! Usage NEWSPACE=<extra_space>,<#_of_extra_fields>");
                WAIT; return(-1);
                }
            filen+=atoi(xosa[0]);
            fim1=fim+atoi(xosa[1]);
            }
        else
            {
            filen+=filen/4+20;
            fim1=fim+fim/4+4;
            }
        if (check_varnames) // 30.9.2009
            {
            fil=max_varlen;
            if (max_len>fil) fil=max_len;
            }
        else fil=max_varlen;
//      else fil=64;  -1.10.2009            
        fiextra=12;
        fitextn=1;
        fitextlen=c2;
        strcpy(jakso," Copied from text file "); strcat(jakso,word[2]); privi[0]=jakso;
        fitext=privi;                
        i=fi_create(word[3],filen,fim1,fim,0L,fil,fiextra,fitextn,fitextlen,
                    fitext,varname,varlen,pvartype);
        if (i<0) return(-1);           
        data_open(word[3],&d2);       
        return(1);
        }


static int lue_lista()
        {
        int i,k;
        int rivi;
        char *p,*q;
        char x[LLENGTH];
        char *sana[4];

        rivi=r1+r;
        p=nimitila; *p=EOS;
        m=m_act=0;

        while (1)
            {
            if (rivi>r2) break;
            edread(x,rivi);
            k=split(x+1,sana,1);
            if (k==0) { ++rivi; continue; }
            if (strncmp(sana[0],"FIELDS",6)==0 && !muste_nofields) // RS ADD muste_nofields
                {
                ++rivi;

                edread(x,rivi); k=split(x+1,sana,4);
                if (k==4 && muste_strcmpi(sana[3],"LF")!=0) continue;

                fields=1; perusmuoto=0; break;
                }
            ++rivi;
            }

        if (rivi<=r2) while (1)
            {
            if (rivi>r2)
                {
                sur_print("\nEND line missing from the list of fields!");
                WAIT; return(-1);
                }
            edread(x,rivi);
            k=split1(x+1,sana,4);  /* pilkku ei ole erotin */
            if (k==0) break;
            if (strcmp(sana[0],"END")==0) break;
            if (k<2)
                {
                sprintf(sbuf,"\nInvalid line %d!",rivi); sur_print(sbuf);
                WAIT; return(-1);
                }
            i=atoi(sana[0]);
            if (i!=m+1)
                {
                sprintf(sbuf,"\nError on edit line %d:",rivi); sur_print(sbuf);
                sprintf(sbuf,"\nField # %d expected!",m+1); sur_print(sbuf);
                WAIT; return(-1);
                }

            if (m>ep4)
                {
                sprintf(sbuf,"\nMax.# of fields = %d (ep4)",ep4); sur_print(sbuf);
                WAIT; return(-1);
                }
            if (*sana[1]!='-' || sana[1][1]!=EOS)
                {
                v[m_act++]=m;
                varname[m_act-1]=p; q=sana[1];
                if (p-nimitila>8*ep4-strlen(q)-1) { nimiylitys(); return(-1); }
                while (*q) *p++=*q++; *p++=EOS;

                for (i=0; i<m_act-1; ++i)  /* 28.4.1997 */
                    {
                    if (strcmp(varname[i],varname[m_act-1])==0)
                        {
                        sprintf(sbuf,"\nField name `%s' at least twice in the list!",
                                             varname[i]);
                        sur_print(sbuf); WAIT; return(-1);
                        }
                    }
                }
            erotin[m]=p;
            pituus[m]=0;
      /*    pituus[i]=0;   korjattu 16.8.87  */

            if (k>2)
                {
                char y[LLENGTH];

                muoto=1;
                i=erotin_muunnos(y,sana[2]); if (i<0) return(-1);
                if (i==0)
                    {
                    pituus[m]=0;
                    q=y;
                    if (p-nimitila>8*ep4-strlen(y)-1) { nimiylitys(); return(-1); }
                    while (*q) *p++=*q++;
                    }
                else
                    {
                    pituus[m]=i;
                    if (k>3) /* Tapaus i var n LF  eli lf kiint.kentÑn jÑlk. */
                        {
                        i=erotin_muunnos(y,sana[3]); if (i<0) return(-1);
                        if (strlen(y)!=1 || *y!='\n')
                            {
                    sur_print("\nOnly LF permitted as a delimiter after a fixed length field!");
                            WAIT; return(-1);
                            }
                        if (p-nimitila>8*ep4-1-1) { nimiylitys(); return(-1); }
                        *p++='\n';
                        }
                    }
                }
            *p++=EOS;
            ++m;
            ++rivi;
            }

        if (m>0 && *erotin[m-1]==EOS)
            {
            p=erotin[m-1];
            *p='\n'; *(p+1)=EOS;
            }

        if (m==0)
            {
            i=etsi_rivi(l1); if (i<0) return(-1);
            p=fgets(jakso,NL*LLENGTH,text);
            if (p==NULL) return(-1);
        i=strlen(jakso); while (jakso[i-1]=='\n' || jakso[i-1]=='\r') jakso[--i]=EOS; // RS ADD CHA 11.3.2013 =='\n' -> <32 || jakso[i-1]==limit_char
        if (koodi) conv(jakso,code); // RS ADD
        if (nskip) skip_char(jakso,skip);  // RS ADD           

		     if (muste_quotes) // RS ADD
        	 {
        	 if (fixed_delimiter) m=split_by_char_quotes(jakso,tsana,ep4+1,limit_char);
             else m=split_quotes(jakso,tsana,ep4+1);
        	 }
        	 else
        	 {
/* 16.3.96*/ if (fixed_delimiter) m=split_by_char(jakso,tsana,ep4+1,limit_char);
             else m=split(jakso,tsana,ep4+1);  /* +1: 12.1.92 */
             }

            m_act=m;
            for (i=0; i<m; ++i) v[i]=i;

            if (*names)
                {
				if (koodi) conv(names,code); // RS ADD
        		if (nskip) skip_char(names,skip);  // RS ADD           

		     	if (muste_quotes) // RS ADD
        	 	{
        	 	if (fixed_delimiter) i=split_by_char_quotes(names,varname,ep4,limit_char);
             	else i=split_quotes(names,varname,ep4);
        	 	}
        	 	else
        	 	{
/* 16.3.96*/ 	if (fixed_delimiter) i=split_by_char(names,varname,ep4,limit_char);
             	else i=split(names,varname,ep4); 
             	}                                            
//		    	if (fixed_delimiter) i=split_by_char(names,varname,ep4,limit_char);
//              else i=split(names,varname,ep4);
/*
     for (i=0; i<m; ++i)
     Rprintf("%d %s\n",i+1,varname[i]); getch();
*/
                if (i<m)
                    {
              sprintf(sbuf,"\nNAMES line has only %d names but FIRST data line %d fields!",i,m);
              sur_print(sbuf);
              sprintf(sbuf,"\nTry to use the MAXFIELDS=<#_of_fields> specification!");
                    sur_print(sbuf); WAIT; return(-1);
                    }
                }
            else
                {
                p=nimitila; *p=EOS;
                for (i=0; i<m; ++i)
                    {
                    muste_itoa(i+1,jakso,10);
                    strcpy(x,"X"); strcat(x,jakso);
                    varname[i]=p; q=x;
                    if (p-nimitila>8*ep4-strlen(q)-1) { nimiylitys(); return(-1); }
                    while (*q) *p++=*q++; *p++=EOS;
                    }
                }
            rewind(text);
            }
        return(1);
        }

static int avaa_teksti(char *s)
        {
        char nimi[LLENGTH];

        strcpy(nimi,s);
        if (strchr(s,':')==NULL) { strcpy(nimi,edisk); strcat(nimi,s); }
        text=muste_fopen(nimi,"rt");
        if (text==NULL)
            {
            sprintf(sbuf,"\nCannot open text file %s!",s); sur_print(sbuf);
            WAIT; return(-1);
            }
        return(1);
        }

static int lue_prefix_lista()
        {
        int i,k;
        int rivi;
        char *p,*q;
        char x[LLENGTH];
        char *sana[3];
        char *p2;

        rivi=r1+r;
        p=nimitila; *p=EOS;
        m=0;
        p2=sanatila;

        while (1)
            {
            if (rivi>r2) break;
            edread(x,rivi);
            k=split(x+1,sana,1);
            if (k==0) { ++rivi; continue; }
            if (strncmp(sana[0],"FIELDS",6)==0 && !muste_nofields) break; // RS ADD muste_nofields
            ++rivi;
            }

        while (rivi<=r2)
            {
            ++rivi;
            if (rivi>r2)
                {
                sur_print("\nEND line missing from the list of fields!");
                WAIT; return(-1);
                }
            edread(x,rivi);
            k=split1(x+1,sana,3);  /* pilkku ei ole erotin */
            if (k==0) return(0);
            if (strcmp(sana[0],"END")==0) break;
            if (k<3)
                {
                sprintf(sbuf,"\nInvalid line %d!",rivi); sur_print(sbuf);
                WAIT; return(-1);
                }
            i=atoi(sana[0]);
            if (i!=m+1)
                {
                sprintf(sbuf,"\nError on edit line %d:",rivi); sur_print(sbuf);
                sprintf(sbuf,"\nField # %d expected!",m+1); sur_print(sbuf);
                WAIT; return(-1);
                }

            if (m>ep4)
                {
                sprintf(sbuf,"\nMax.# of fields = %d (ep4)",ep4); sur_print(sbuf);
                WAIT; return(-1);
                }

            varname[m]=p; q=sana[1];
            if (p-nimitila>8*ep4-strlen(q)-1) { nimiylitys(); return(-1); }
            while (*q) *p++=*q++; *p++=EOS;

            erotin[m]=p2; q=sana[2];  /* erotin=prefix_text */
            if (p2-sanatila>8*ep4-strlen(q)-1) { nimiylitys(); return(-1); }
            while (*q) *p2++=*q++; *p2++=EOS;




            for (i=0; i<m-1; ++i)  /* 28.4.1997 */
                {
                if (strcmp(varname[i],varname[m-1])==0)
                    {
                    sprintf(sbuf,"\nField name `%s' at least twice in the list!",
                                         varname[i]);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                }
            ++m;
            }

        name_field=-1;
        for (i=0; i<m; ++i)
            {
            k=varfind(&d2,varname[i]);
            if (k<0) return(-1);
            v[i]=k;
            if (muste_strcmpi(erotin[i],"#START")==0) name_field=k;
            }
        return(1);
        }


static int format_prefix()
        {
        int i,miss,len,h;
        long j;
        char *p,*q,*p0;
        char x[LLENGTH];
        long nn;
        double xx;

        lue_prefix_lista();

        nn=d2.d2.n;
/**********************************
        for (i=0; i<m; ++i)
            {
            Rprintf("%d %s | %s |\n",i+1,varname[i],erotin[i]);
            }
        getch();
 **********************************/
        i=spfind("DELIMITER");
        if (i<0) i=spfind("LIMIT");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            if (muste_strcmpi(x,"TAB")==0) limit_char='\t';
            else if (muste_strnicmp(x,"SP",2)==0) limit_char=' ';
            else if (muste_strnicmp(x,"char(",5)==0) limit_char=atoi(x+5);
            else limit_char=*x;
            }
        else limit_char=',';

        if (l1>1L) { i=etsi_rivi(l1); if (i<0) return(-1); }

        for (j=l1; j<=l2; ++j)
            {
            p=fgets(jakso,NL*LLENGTH,text);
            if (p==NULL) break;

            ++nn; d2.n=nn;
            fi_miss_obs(&d2.d2,nn);

            i=strlen(jakso); while(jakso[i-1]=='\n' || jakso[i-1]=='\r') jakso[--i]=EOS;
            if (koodi) conv(jakso,code);

// Rprintf("jakso=%s|\n",jakso); getch();
            p=p0=jakso; len=strlen(jakso);
            for (i=0; i<m; ++i)
                {
                miss=0;
                if (v[i]==name_field)
                    {
                    q=strchr(jakso,limit_char);
                    if (q==NULL) miss=1;
                    else
                        {
                        strncpy(x,jakso,q-jakso); x[q-jakso]=EOS;
                        p0=q;
                        }
                    }
                else
                    {
                    p=strstr(p0,erotin[i]);
                    if (p==NULL) miss=1;
                    else
                         {
                         q=strchr(p+1,limit_char);
                         if (q==NULL) q=p0+len;
                         p+=strlen(erotin[i]);
                         strncpy(x,p,q-p); x[q-p]=EOS;
                         }
                    }
                if (miss) *x=EOS;
// Rprintf("i=%d var=%d val=%s\n",i,v[i],x); getch();

                if (d2.vartype[v[i]][0]=='S')
                    {
                    strcpy(jakso2,x);
                    for (h=strlen(jakso2); h<d2.varlen[v[i]]; ++h)
                        jakso2[h]=' ';
                    fi_alpha_save(&d2.d2,nn,v[i],jakso2);
                    }
                else
                    {
                    xx=atof(x); fi_save(&d2.d2,nn,v[i],&xx);
                    }
                }
            }

        fi_rewind(&d2.d2);
        fi_puts(&d2.d2,(char *)&nn,4,22L); // RS 28.1.2013 (char *)

        return(1);
        }



static int next_value(char *delimiter,int len,char *x)
        {
        int i;
        unsigned char ch;

        if (len)
            {
            for (i=0; i<len; ++i)
                {
                ch=code[(unsigned char)getc(text)];
                if (ch=='\n') { if (i==0) --i; if (feof(text)) return(-1); continue; }
                x[i]=ch;
                }
            }
        else
            {
            i=0;
            while (1)
                {
                ch=code[(unsigned char)getc(text)];
                if (feof(text)) return(-1);
                if (ch=='\n' || strchr(delimiter,ch)!=NULL)
                    { if (i==0) continue;
                      else break; }
                if (i>=LLENGTH)
                    {
                    sur_print("\nObviously not a text file!");
                    WAIT; return(-1);
                    }
                x[i++]=ch;
                }
            }
        if (feof(text)) return(-1);
        x[i]=EOS;
        return(1);
        }


static int format_save2(char *delimiter,int len,int var)
        {
        int i,h;
        long l;
        char x[LLENGTH];

        if (l1>1L) { i=etsi_rivi(l1); if (i<0) return(-1); }

        l=d2.n;  sur_print("\n");
        while (1)
            {
            i=next_value(delimiter,len,x);
            if (i<0)
                {
                fi_rewind(&d2.d2);
                fi_puts(&d2.d2,(char *)&l,4,22L); // RS 28.1.2013 (char *)
                data_close(&d2);
                return(1);
                }
            if (*x==EOS || (*x==' ' && x[1]==EOS) ) continue;

            ++l; ++d2.n;
// 2.3.2001 if (kbhit()) { i=getch(); if (i=='.') prind=1-prind; }
            if (prind) { sprintf(sbuf,"%d ",d2.n); sur_print(sbuf); } // RS CHA %ld -> %d

            if (d2.vartype[var][0]=='S')
                {
                for (h=strlen(x); h<d2.varlen[var]; ++h)   /* 12.2.1992 */
                    x[h]=' ';
                fi_alpha_save(&d2.d2,l,var,x);
                }
            else
                {
                double y;

                if (sa_missing(x))
                    fi_miss_save(&d2.d2,l,var);
                else
                    { y=atof(x);  fi_save(&d2.d2,l,var,&y); }
                }
            }
        return(0);    
        }


static int format_save(char *format)
        {
        int i;
        char x[LLENGTH], *osa[3];
        int var;
        int len;
        char delimiter[256];
        char *p;


        if (muste_strcmpi(format,"PREFIX")==0)
            {
            return(format_prefix());
            }
        strcpy(x,format);
        i=split(x,osa,3);
        if (i<2) { sur_print("\nError in FORMAT - check specification!"); WAIT; return(-1); }
        if (i>1 && strncmp(osa[0],"WORD",4)!=0 && strncmp(osa[0],"CHAR",4)!=0)
        	{
        	sur_print("\nUnknown FORMAT - check specification!"); WAIT;
        	return(-1);
        	}
        
        i=data_open(word[3],&d2);
        if (i<0) return(-1);      
        if (d2.type!=2)
            {
            sprintf(sbuf,"\n%s is not a Survo data file!",word[3]);
            sur_print(sbuf); WAIT; sulje(); return(-1); // RS ADD sulje
            }        
        
        var=varfind(&d2,osa[1]); if (var<0) return(-1);

        if (strncmp(osa[0],"WORD",4)==0)
            {
            strcpy(delimiter," ,.:;?!-"); p=delimiter;
            if (i>2)
                {
                strcpy(delimiter,osa[2]); p=delimiter;
                if (*p=='"') ++p;
                if (p[strlen(p)-1]=='"') p[strlen(p)-1]=EOS;
                }
            i=format_save2(p,0,var);
            return(i);
            }
        else if (strncmp(osa[0],"CHAR",4)==0)
            {
            len=1;
            if (i>2) len=atoi(osa[2]);
            *delimiter=EOS;
            i=format_save2(delimiter,len,var);
            return(i);
            }
        sur_print("\nUnknown FORMAT!");
        WAIT; return(-1);
        }

static int chrconv(char *s,char *y)
        {
        char *p,*q,*r; // RS REM unsigned
        char cc[2]; // RS REM unsigned

        p=s; cc[1]=EOS;
        while (1)
            {
            q=strstr(p,"char(");
            if (q==NULL) { strcat(y,p); return(1); }
            *q=EOS; q+=5; strcat(y,p);
            r=strchr(q,')');
            if (r==NULL)
                {
                sprintf(sbuf,"\n) missing in %s",p);
                sur_print(sbuf); WAIT; return(-1);
                }
            *r=EOS;
            *cc=(unsigned char)atoi(q);
            strcat(y,cc);
            p=r+1;
            }

        return(1);
        }

extern int load_codes();

void muste_file_save(int argc,char *argv[])
        {
        int i,h,k;
        int ii=0;
        char *p,*nimi;
//        int disp;
        char x[LLENGTH],*sana[2];
        int vi;
        double xx;

// RS ADD variable init
ep4=0;
j=j2=0;
m=m_act=0;
l1=l2=l3=l4=0; /* FIRST=l1,LAST=l2,NAMES=l3(,l4) */
muoto=0; /* 0=ei erottimia 1=erottimet */
koodi=0; /* 1=koodimuunnos 0=ei */
paikka=0;
fields=0;
moodi=2; /* 1=oletus (vanha), 2=uusi */
perusmuoto=0;
nskip=0;
fixed_delimiter=0;
limit_char=0;
prind=0;
prind_count=0;
suora_siirto=0; // 1.5.2001
skip_errors=0; // 20.11.2001
match_var=match_var2=0;
name_field=0;
new_file=0;
namespace=NULL;
varname2=NULL;
check_varnames=1;
max_len=0;
muste_nofields=0;

text=NULL;
v2=NULL;  /* malloc */
v=NULL;
nimitila=NULL;
varname=NULL;
erotin=NULL;
pituus=NULL;
sanatila=NULL;
tsana=NULL;
kok=des=tyyppi=neg=NULL;
vartype=NULL;
pvartype=NULL;
varlen=NULL;
ntila=NULL;

        if (argc==1) return;
        s_init(argv[1]);
        specs=specs0;

        if (g<3)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE SAVE <text_file>,<Survo_data_file>");
            sur_print("\n  and list of fields (variables)");
            sur_print("\nSpecifications: FIRST,LAST,NAMES,MATCH,FILTER,MISSING");

            WAIT; return;
            }

                   /* 21.11.1993 */
        if (g>4)   /* FILE SAVE <text_file> TO <data_file */
            {
            if (muste_strcmpi(word[3],"TO")!=0)
                {
                sur_print("\nUsage:");
                sur_print("\nFILE SAVE <text_file> TO <Survo_data_file>");
                sur_print("\nFILE SAVE R><R_data_frame> TO <Survo_data_file>");                
                WAIT; return;
                }
            word[3]=word[4];
            if (g>5) word[4]=word[5];
            }

        if (strcmp(muste_strupr(word[3]),"NEW")==0) // 4.1.2005
            {
            word[3]=word[4];
            strcpy(sbuf,word[3]);
            if (strchr(sbuf,'.')==NULL) strcat(sbuf,".SVO");
            sur_delete(sbuf);
            }

        if (*word[2]=='R' && *(word[2]+1)=='>')  // RS ADD
                	{
                	nimi=(word[2]+2);
                	sprintf(sbuf,"\nSaving R data frame %s to file %s: ",nimi,word[3]); 
                	sur_print(sbuf);
                	muste_R2Survo(word[3],nimi);
                	return;
                	}

        i=avaa_teksti(word[2]); if (i<0) return;
        l1=1L; l2=1000000000L; l3=0L;
        muoto=0; *names=EOS; fields=0;
        i=sp_init(r1+r-1); if (i<0) return;
        i=varaa_tilat(); if (i<0) return;

        perusmuoto=1;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);




        i=spfind("FIRST");
        if (i>=0) { l1=atol(spb[i]); if (l1<1L) l1=1L; perusmuoto=0; }
        i=spfind("LAST");
        if (i>=0) { l2=atol(spb[i]); if (l2<l1) l2=l1; }
        suora_siirto=0;
        i=spfind("NAMES");
        if (i>=0)
            {
            if (muste_strcmpi(spb[i],"DEFAULT")==0) suora_siirto=1;
            else
                {
                strcpy(x,spb[i]); i=split(x,sana,2);
                l3=atol(sana[0]); l4=l3;
                if (i>1) l4=atol(sana[1]);
                if (l4<l3) l4=l3;
                if (l4>=l1 || l4<0L)
                    { sur_print("\nNAMES line(s) not before FIRST line!"); WAIT; return; }
                perusmuoto=0;
                }
            }
        koodi=0; for (i=0; i<256; ++i) code[i]=(unsigned char)i;
        i=spfind("FILTER");
        if (i>=0) { koodi=1; i=load_codes(spb[i],code); if (i<0) return; }

        i=spfind("MISSING");
        if (i>=0) strcpy(cmissing,spb[i]); else *cmissing=EOS;

        strcpy(skip,"\r"); // RS ADD
        i=spfind("SKIP");   /* 8.5.92 */
        if (i>=0) 
            { 
            chrconv(spb[i],skip); // RS 11.3.2013
            nskip=strlen(skip); 
            } // RS CHAR strcpy -> strcat
        else nskip=1; // RS CHA nskip=0

        i=spfind("NUMSEP"); // RS 11.3.2013
        if (i>=0) 
            { 
            chrconv(spb[i],muste_numsep); 
            n_muste_numsep=strlen(muste_numsep); 
            } // RS CHAR strcpy -> strcat
        else n_muste_numsep=0;

        moodi=1;
        i=spfind("MODE");
        if (i>=0) moodi=atoi(spb[i]);

		muste_noformat=0; // RS ADD
        i=spfind("NOFORMAT");
        if (i>=0) muste_noformat=atoi(spb[i]);		


        i=spfind("FORMAT");
        if (i>=0 && !muste_noformat) // RS ADD muste_noformat
            {
            format_save(spb[i]);
            sulje(); // RS ADD
            return;
            }

        max_varlen=64;
        i=spfind("VARLEN"); if (i>=0) max_varlen=atoi(spb[i]);

        i=spfind("DELIMITER");
        if (i<0) i=spfind("LIMIT");
        if (i>=0)
            {
            fixed_delimiter=1;
            strcpy(x,spb[i]);
            if (muste_strcmpi(x,"TAB")==0) limit_char='\t';
            else if (muste_strnicmp(x,"SP",2)==0) limit_char=' ';
            else if (muste_strnicmp(x,"char(",5)==0) limit_char=atoi(x+5);
            else limit_char=*x;
            }

        skip_errors=0; // 20.11.2001
        i=spfind("SKIP_ERRORS");
        if (i>=0) skip_errors=atoi(spb[i]);

        muste_dec='.';
        i=spfind("DEC");
        if (i>=0) muste_dec=*spb[i]; // RS 11.3.2013

		muste_nofields=0; // RS ADD
        i=spfind("NOFIELDS");
        if (i>=0) muste_nofields=atoi(spb[i]);		

		muste_quotes=0; // RS ADD
        i=spfind("REMOVE_QUOTES");
        if (i>=0) muste_quotes=atoi(spb[i]);	

        i=lue_lista(); if (i<0) return;

//Rprintf("\nlue lista:");
//for (i=0; i<m; ++i)
//    Rprintf("\n%d  %s   %d   %s",i+1,varname[i],pituus[i],erotin[i]);
//    getch();


        i=spfind("MATCH");
        if (i>=0) { match_copy(); sulje(); return; } // RS ADD sulje()
/*
     for (i=0; i<m_act; ++i)
     Rprintf("%d %d %s\n",i+1,v[i],varname[i]); getch();
*/

// RS REM        i=fi_find(word[3],&d2.d2,jakso);

        if (!sur_find_svo_file(word[3],jakso)) // RS CHA i<0
            {
            i=luo_uusi(); if (i<0) { sulje(); return; } // RS ADD
            new_file=1; // 28.9.2009            
            }
        else
            {
// RS REM            muste_fclose(d2.d2.survo_data);
            i=data_open(word[3],&d2);
            }


// RS ADD
        data_close(&d2);
        data_open3(word[3],&d2,0,1,1,1); 

        if (d2.type!=2)
            {
            sprintf(sbuf,"\nDestination %s must be a data file!",word[3]);
            sur_print(sbuf); WAIT; sulje(); return;
            }

        if (suora_siirto) for (i=0; i<m_act; ++i) v2[i]=i;
        else
            {
            i=tutki_muuttujat(); if (i<0) { sulje(); return; } // RS ADD sulje 
            }
        if (l1>1L) { i=etsi_rivi(l1); if (i<0) { sulje(); return; } } // RS ADD sulje
        prind_count=0;

// RS ADD
        data_close(&d2);
        data_open(word[3],&d2);
        
        sprintf(sbuf,"\nCopying records from %s to %s: ",word[2],word[3]); sur_print(sbuf);
        j2=d2.n;
//        disp=0;
        for (j=l1; j<=l2; j+=(long)ii)
            {

            if (!muoto)
                {
                p=fgets(jakso,NL*LLENGTH,text);
                if (p==NULL) break;
                i=strlen(jakso); while(jakso[i-1]=='\n' || jakso[i-1]=='\r') jakso[--i]=EOS; // RS ADD \r
                if (koodi) conv(jakso,code);
                if (nskip) skip_char(jakso,skip);
                              
				if (muste_quotes) // RS ADD
        			{
        			if (fixed_delimiter) k=split_by_char_quotes(jakso,tsana,m,limit_char);
                	else k=split_quotes(jakso,tsana,m);
        			}
        		else
        			{
/* 16.3.1996 */ 	if (fixed_delimiter) k=split_by_char(jakso,tsana,m,limit_char);
                	else k=split(jakso,tsana,m);
                	}

                if (k<m && skip_errors==2) // 25.8.2002
                    {
                    for (i=k; i<m; ++i) tsana[i]=" ";
                    k=m;
                    }

                if (k<m)
                    {
                    if (skip_errors) continue;

                    sprintf(sbuf,"\nNot enough fields on line %ld in text file %s (%d<%d)",
                                    j,word[2],k,m); sur_print(sbuf);
                    WAIT; sulje(); return;
                    }
                ii=1;
                }
            else
                {
                paikka=muste_ftell(text);
                if (moodi==1)
                    ii=sasplit(text,tsana,m,sanatila,8*ep4,erotin,pituus,code);
                else
                    ii=sasplit2(text,tsana,m,sanatila,8*ep4,erotin,pituus,code);

// Rprintf("\nii=%d",ii); getch();
                if (ii==-1) break;
                if (ii==-2)
                    {
                 if (!skip_errors) { format_error(); aseta_n(); sulje(); return; } // RS ADD sulje()
                    continue;
                    }
                if (ii==-3) { sulje(); return; } // RS ADD sulje()
                }
        /*
            if (kbhit()) { while (kbhit()) getch(); disp=1-disp; }
            if (disp)
                {
                sur_print("\n");
                for (i=0; i<m; ++i) { sprintf(sbuf," %s",tsana[i]); sur_print(sbuf); }
                sur_print("\n");
                }
        */

            ++j2;
            if (j2>d2.n) d2.n=j2;  /* fi_save vaatii j2<=d2.n  */
            if (d2.m>m_act) fi_miss_obs(&d2.d2,j2);

            for (i=0; i<m_act; ++i)
                {
                vi=v[i];
                if (d2.vartype[v2[i]][0]=='S')
                    {
                    strcpy(jakso2,tsana[vi]);  /* ennen 2.4.91 jakso */	
// Rprintf("\nv[i]: %d, v2[i]: %d, jakso2: %s, strlen: %d varlen: %d",v[i],v2[i],jakso2,strlen(jakso2),d2.varlen[v2[i]]);                                       
                    for (h=strlen(jakso2); h<(unsigned int)d2.varlen[v2[i]]; ++h)
                        jakso2[h]=' ';
                    jakso2[(unsigned int)d2.varlen[v2[i]]]=EOS; // RS ADD 23.5.2012                                                    
                    fi_alpha_save(&d2.d2,j2,v2[i],jakso2);
                    }
                else
                    {
                    if (sa_missing(tsana[vi]))
                        {
                        fi_miss_save(&d2.d2,j2,v2[i]);
                        }
                    else
                        {
                        if (muste_dec!='.') // RS 11.3.2013
                            {
                            p=tsana[vi];
                            while (*p!=EOS) { if (*p==muste_dec) *p='.'; p++; } 
                            }
                        if (n_muste_numsep) skip_char(tsana[vi],muste_numsep); // RS 11.3.2013
                        xx=atof(tsana[vi]);
                        fi_save(&d2.d2,j2,v2[i],&xx); 
                        }
                    }
                }
// 2.3.2001 if (kbhit()) { i=getch(); if (i=='.') prind=1-prind; }
            if (prind)
                {
                ++prind_count;  // 2.2.2008
                if (prind_count==prind)
                    {
                    sprintf(sbuf,"%ld ",j); sur_print(sbuf);
                    prind_count=0;
                    }
                }
            }
        aseta_n();
        sulje();
// RS FIXME Should this be used: fcloseall(); // 6.7.2009        
        return;
        }
