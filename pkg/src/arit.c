#include <R.h>
#include <Rinternals.h>
#include "survo.h"
#include "sinit.h"
#include <math.h>
/* #include <conio.h>*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* #include <process.h> */
#include <errno.h>
#include <time.h>
#include <ctype.h>

#define MAXPITUUS 200
/*                25.7.2006 */
#define MAXARG 10
#define EOS '\0'
#define EARG '\376'

/* #define MISSING8 1e306 is one bit less in Watcom */
#define MISSING8 *(double *)"\51\220\43\312\345\310\166\177"

#define RND (double)rand()/32768.0
#define MAXEARG 1000

extern char *z;
extern int r,r1,r2,r3,c,c1,c2,c3;
extern int ed1,ed2;
extern int sur_seed;
extern unsigned int *zs;
extern char survo_path[];

/* specifications in the edit field */
char *splist;
char **spa, **spb, **spshad;
int spn;
double *arvo; /* vain arit.c tarvitsee  */
char *spp;
unsigned int *spplace;
extern int speclist,specmax;
extern int etu;
extern char tut_info[];

/* int earg_varattu=0; */
int n_earg=0;
double *earg;

char *spl;
int global;

extern int errno;
extern char sbuf[];
extern int child_call;
extern char help_sana[];

int l_virhe;
int tarkkuus;

int remember; /* 5.10.1998 */
char *remember_space;
int n_remember,remember_width;

int puhdas_dat_kysely; /* 8.12.2002 */

extern int nmat; /* 8.5.1999 */
extern char *language;

/* #pragma check_stack(on) */
extern long check_stack; /* 19.1.2003 */
unsigned char *stackp1;

static char tuntematon_muuttuja[LNAME]; /* 2.12.2008 */

char *str_opnd[MAXARG+4];


/* reverse:  reverse string s in place */
void reverse(char s[])
{
    int c, i, j;

    for (i = 0, j = strlen(s)-1; i<j; i++, j--) {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
    }
}

/* itoa:  convert n to characters in s */
void itoa(int n, char s[])
{
    int i, sign;

    if ((sign = n) < 0)  /* record sign */
        n = -n;          /* make n positive */
    i = 0;
    do {       /* generate digits in reverse order */
        s[i++] = n % 10 + '0';   /* get next digit */
    } while ((n /= 10) > 0);     /* delete it */
    if (sign < 0)
        s[i++] = '-';
    s[i] = '\0';
    reverse(s);
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
                if (luku==0) { strcpy(sana,"0"); return(1); }
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
        if (p==NULL) dec=0; else dec=len-1-(p-muoto);
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
            if (logl>0) { kok=logl+2; des=pituus-kok-1; }
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
                j=atoi(sana); if (j>=lin && j<=ed2) return(j);
                linerr2(sana); return(0);
                }
        for (j=0; j<3; ++j) SANA[j]=toupper(sana[j]);
        if (strncmp(SANA,"END",3)==0) { k=3; j=lastline2(); }
        else if (strncmp(SANA,"CUR",3)==0) { k=3; j=r1+r-1; }
        else
            {
            lin1=lin-atoi(sana+1);
            if (lin1<1) lin1=1;
            for (j=lin1; j<=ed2; ++j)
                if ( *(z+(j-1)*ed1)==*sana ) break;
            if (j>ed2) { if (virheilm) linerr2(sana); return(0); }
            k=1;
            }
        if (strlen(sana)<k+2) return(j);
        j+=atoi(sana+k);
        if (j>=lin && j<=ed2) return(j);
        if (virheilm) linerr2(sana); return(0);
        }


int spfind(char *s)
        {
        int i;
        for (i=0; i<spn; ++i)
                if (strcmp(s,spa[i])==0) return(i);
        return(-1);
        }

int sp_check()
        {
        int j;
        unsigned int n,tila,tila0;
        int varjo;
        char x[LLENGTH];
        char *p,*q;

        if (*z==' ')     /* 22.6.1998 */				/* No check if empty control char at first line	*/
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

        n=0; tila=0;
        for (j=1; j<=r2; ++j)                                      	/* Loop through the edit field lines		*/
            {
            edread(x,j);						
            *x=EOS; p=x+1;						/* Put EOS to control column 			*/
            while (1)
                {
                p=strchr(p,'='); if (p==NULL) break;			/* Next line if no more  "="-marks		*/
                if (*(p+1)==EOS) break;					/* Next line if at the end of line		*/
                if (*(p+1)=='=') { p+=1; continue; }			/* Allow "==" and longer repeats of "="		*/
/* printf("C: j=%d pos=%d\n",j,p-x); getch(); */
                ++n;							/* Increase the number of found specifications  */
                tila0=tila; varjo=zs[j];
                tila+=2; 						/* Space for two EOS 				*/
                q=p-1; while (*q && *q!=' ') { ++tila; --q; }		/* Scan left until space or EOS			*/
                q=p+1; while (*q && *q!=' ') { ++tila; ++q; }		/* Scan right until space or EOS		*/

                while (*(q-1)=='&')					/* Expression splitted to several lines 	*/
                    {
                    tila+=c2; 						/* Allow some extra space (bug fix?)		*/
                    ++j; if (j>r2) break;				/* Go to next line and check is it the last one */
                    edread(x,j);
                    q=x+1; while (*q && *q==' ') ++q;			/* Skip spaces in the beginning of line		*/
                    if (*q==EOS) break;					/* Next line if at the end of line		*/
                    while (*q && *q!=' ') { ++tila; ++q; }		/* Scan right until space or EOS		*/
                    }
                if (varjo) tila+=tila-tila0;				/* Double space needed if shadows in use        */
                ++p;
                }
            }
        specmax=n; speclist=tila;					/* The number of spcifications and needed space */

Rprintf("specmax=%d, speclist=%d",specmax,speclist);

        return(1);
        }



int sur_instr(char s[],char c[])
        {
        if (strstr(s,c)==NULL) return(-1);
        return(1);
        }

int jatkorivit(int j)
        {
        char x[LLENGTH];
        char *p,*q;

        while (1)
            {
            edread(x,j);
            p=x+1;
            while (*p==' ') ++p;
            q=p; while (*q!=' ') ++q; *q=EOS;
            *(spl-2)=EOS; --spl;
            if (spl-splist+strlen(p)+2>speclist) return(-1);
            strcat(spl-1,p); spl+=strlen(p);
            if (*(spl-2)!='&') break;
            ++j;
            }
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
            p=strchr(x+pos,'=');					/* Search for "="				*/
            if (p==NULL) break;						/* Ready if no more found			*/
            if (*(p+1)==EOS) break;					/* Ready if at the end of line			*/
            if (*(p+1)=='=') { pos+=2; continue; }			/* Jump over "="				*/
/* printf("j=%d pos=%d\n",j,p-x); getch(); */
            if (j==r1+r-1 && p-x==c1+c-2) { pos=p-x+1; continue; }	/* Skip the place of activation			*/
            if (spn>=specmax) return(-spn);				/* Return if too many specifications		*/
            spp[spn]=*(p-1);   						/* Save char preceding "=" such as ":",".","|"  */
            spplace[spn]=(unsigned int)((j-1)*ed1+p-x);			/* Edit field pointer to specification "="      */

            pos=p-x; i=pos-1;
            while (i>0 && x[i]!=' ') --i;				/* Scan left					*/
            if (spl-splist+pos-i+1>speclist) return(-spn);
            strncpy(spl,x+i+1,(unsigned int)(pos-i-1));			/* Copy specification to spl			*/
            spa[spn]=spl; spl+=pos-i; *(spl-1)=EOS;			/* Update pointers, spa=left side     		*/

            i=pos+1;
            while (i<ed1 && x[i]!=' ') ++i;				/* Scan right					*/
            if (spl-splist+i-pos+1>speclist) return(-spn);
            strncpy(spl,x+pos+1,(unsigned int)(i-pos-1));		/* Copy specification to spl			*/
            spb[spn++]=spl; spl+=i-pos; *(spl-1)=EOS;			/* Update pointers, spb=right side     		*/

            if (*(spl-2)=='&') { k=jatkorivit(j+1);			/* Expression continues on the next line	*/
                                 if (k<0) return(-spn);
                               }
            spshad[spn-1]=NULL;
            if (zs[j]!=0)						/* Shadows					*/
                {
                edread(xs,zs[j]);
                if (spl-splist+i-pos+1>speclist) return(-spn);
                strncpy(spl,xs+pos+1,(unsigned int)(i-pos-1));
                spshad[spn-1]=spl; spl+=i-pos; *(spl-1)=EOS;
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
        for (j=lin-1; j>0; --j)			/* Scan upwards until borderline or top of edit field */
            {
            edread(x,j);
            i=sur_instr(x,raja);
            if (i>=0) break;
            }
        *raja1=j;
        for (j=*raja1+1; j<=ed2; ++j)		/* Read the specifications */
            {
            edread(x,j);
            if (global==1)			/* Zero global if found */
                {
                i=sur_instr(x,"*GLOBAL*");
                if (i>0) global=0;
                }
            i=sur_instr(x,raja);
            if (i>=0) break;
            if (j==r1+r-1) continue; 		/* Skip the activated line */
            spn=spread3(x,j); if (spn<0) return(spn);
            }

/*  printf("\n"); for (i=0; i<spn; ++i) printf("\n%s=%s %c %u varjo=%s",
                       spa[i],spb[i],spp[i],spplace[i],spshad[i]); getch();
*/
        return (spn);
        }

int replace_function_name(char *sana,int *plen) /* 13.2.2005 esim. M()=MAT_RG.M() */
    {
    int i;
    char sana2[32];
    int len;
    char *p;
    char x[32];

    *sana2=EOS; strncat(sana2,sana,(unsigned int)*plen); strcat(sana2,"()");
    i=spfind(sana2);
    if (i<0) return(1);
/* printf("\nfunc=%s|",spb[i]); getch(); */
    strcpy(x,spb[i]); p=strchr(x,'('); if (p!=NULL) *p=EOS;
    len=strlen(x);
    strcpy(sana2,spb[i]); strcat(sana2,sana+*plen); strcpy(sana,sana2);
    *plen+=len-*plen;
    return(1);
    }


int conversions()
        {
/*
        char x[LNAME];
        extern char *op;

        op=x; strcpy(op,"CONV1");
        childp("");
*/
        return(1);
        }

double oper(double x1,double x2,char laji)
        {
        double power();

        switch (laji)
            {
          case '+':
            return(x1+x2);
          case '-':
            return(x1-x2);
          case '*':
            return(x1*x2);
          case '/':
            if (x2==0.0) { l_virhe=1; return(0.0); }
            return(x1/x2);
          case '^':
            return(pow(x1,x2));
            }
        return(0.0);
        }

int virhe_not_found(char *muuttuja)
            {

 if (strcmp(muuttuja,"@")==0)
     {
     int i;

     for (i=0; i<spn; ++i)
         {
         sprintf(sbuf,"\n%d spa=%s spb=%s",i,spa[i],spb[i]);
         sur_print(sbuf);
         }
/*     getck();  */
     return(1);
     }

            strcpy(tuntematon_muuttuja,muuttuja); /* 2.12.2008 */
            sprintf(sbuf,"\n%s not found!",muuttuja); sur_print(sbuf);
            l_virhe=1;
            if (remember) { remember=0; free(remember_space); } return(1);
            return(1);
            }


int laske();

int laske2(char *muuttuja,double *y)
        {
        int i,k;
/* LOCATE(1,50); sprintf(sbuf,"muuttuja=%s|",muuttuja);
 sur_print(sbuf); cursor(r,c); getck();
*/
     if (*muuttuja==EARG) { *y=earg[atoi(muuttuja+1)]; return(1); }

        i=spfind(muuttuja);
        if (i<0)
            {
       if ((strlen(muuttuja)==1 || sur_strcmpi(muuttuja,"CUR")==0  /* 24.3.1998 */
                                || sur_strcmpi(muuttuja,"END")==0)
             && (i=edline2(muuttuja,1,0))!=0) /* 16.5.1997 */
                {
                *y=(double)i; return(1);
                }
            virhe_not_found(muuttuja); return(1);
            }
        if (spb[i]==NULL) { *y=arvo[i]; return(1); }
        if (*spb[i]==EOS) { virhe_not_found(muuttuja); return(1); }
        k=laske(spb[i],y);
        arvo[i]=*y;
        spb[i]=NULL;
        return(1);
        }


double luku(char *sana,int len)
        {
        char *p;
        double tulos=1.0;

        sana[len]=EOS;
        p=sana; if (*p=='-') ++p;
        if (strchr("1234567890.",*p)==NULL)
            {
            laske2(p,&tulos);
            if (*sana=='-') return(-tulos);
            return(tulos);
            }
        if (sana[len-1]=='%') return(atof(sana)/100.0);   /* 28.2.1992 */
        return(atof(sana));
        }

int supista(int *t,double opnd[],char op[],int v[])
        {
        while (*t>1)
            {
            if (v[*t-1]>v[*t-2]) return(1);
            opnd[*t-2]=oper(opnd[*t-2],opnd[*t-1],(char)(op[*t-2]));
            op[*t-2]=op[*t-1]; v[*t-2]=v[*t-1];
            --(*t);
            }
        return(1);
        }



int syntax_error(char *s)
        {
        sprintf(sbuf,"\nsyntax error in %s",s); sur_print(sbuf);
        l_virhe=1;
        return(1);
        }

/* declarations for laske */
int varif();
int laske_integral();
int arifor();
int root_of();
double funktio();
double mfunktio();

int laske(char *lauseke,double *y)
        {
/*        double luku();
        double oper();
        double funktio();
        double mfunktio();
*/
        char x[MAXPITUUS];
        char *p,*q;
        char sana[32];
        int len;
        double opnd[MAXARG+4]; char op[MAXARG+4]; int v[MAXARG+4];
        int t,n;
        int narg; /* Usean muuttujan funktion argumenttien lkm */
        int i;

        int mat_element; /* 10.5.2005 */
        int n_mat_par;



/* sprintf(sbuf,"%ld ",(long)(stackp1-x)); sur_print(sbuf); sur_wait(10L); */


/*        if (check_stack>0L)  && (long)(stackp1-x)>(long)check_stack) */ /* pinon koko */
/*            {
            l_virhe=1; sur_print("\nStack overflow!");
            return(-1);
            }
*/
        if (*lauseke=='i')
            {
            if (strncmp(lauseke,"if(",3)==0)
                return(varif(lauseke,y));
            else if (strncmp(lauseke,"integral(",9)==0)    /* 2.1.1995 */
                return(laske_integral(lauseke,y));
            }
        if (*lauseke=='f')
            {
            if (strncmp(lauseke,"for(",4)==0)
                return(arifor(lauseke,y));
            }

        if (*lauseke=='r')
            {
            if (strncmp(lauseke,"root_of(",8)==0)
                return(root_of(lauseke,y));
            }

        if (strlen(lauseke)>MAXPITUUS-2)
            {
            sur_print("\nExpression");
            sprintf(sbuf,"\n%s",lauseke); sur_print(sbuf);
            sprintf(sbuf,"\nis too long! (more than %d characters)",MAXPITUUS-1);
            sur_print(sbuf); WAIT; l_virhe=1; return(-1);
            }

        if (*lauseke=='-')  /* 8.12.89 */
            {
            *x='0'; strcpy(x+1,lauseke);
            }
        else strcpy(x,lauseke);

        len=0;
        p=x;
        t=0;
/* printf("\nlauseke=%s|",lauseke); getch(); */
        while (*p)
            {
            if (l_virhe) return(-1);
            switch (*p)
                {
              case '+':
                if (len==0) { ++p; break; }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='+'; v[t++]=1;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '-':
                if (len==0) { sana[len++]=*p; ++p; break; }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='-'; v[t++]=1;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '*':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='*'; v[t++]=2;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '/':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='/'; v[t++]=2;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '^':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='^'; v[t++]=3;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '(':
                sana[len]=EOS; /* 15.2.2005 */
                replace_function_name(sana,&len); /* 13.2.2005 */

                mat_element=0;
        if (strncmp(sana,"MAT_",4)==0) { mat_element=1; n_mat_par=0; }
        if (strncmp(sana,"DAT_",4)==0) { mat_element=1; n_mat_par=0; }
                q=p+1;
            if (*q==')') { sprintf(sbuf,"\nArguments missing in %s",lauseke);
                           sur_print(sbuf); l_virhe=1; return(-1); }
                n=1;
                narg=1;
                while (n)
                    {
                    ++p;
                    if (*p=='(') { ++n; continue; }
                    if (*p==')') { --n; continue; }
                if (*p==EOS) { sprintf(sbuf,"\n) is missing in %s",lauseke);
                               sur_print(sbuf); l_virhe=1; return(-1); }
                    if (*p==',' && n==1)
                        {
                        *p=EOS;

                        if (mat_element) str_opnd[n_mat_par++]=q;
                        else
                            {
               /*     printf("\nq=%s|",q); getch(); */
                            laske(q,&opnd[t]);
                            }
                        ++t;
                        if (t>MAXARG+3)
                         { sprintf(sbuf,"\nToo many arguments in %s",lauseke);
                              sur_print(sbuf); l_virhe=1; return(-1); }
                        ++narg;
                        q=p+1;
                        }
                    if (*p==':') { conversions(); return(2222); }
                    }
                if(strchr("+-*/^)\0",*(p+1))==NULL) { syntax_error(lauseke);
                                                      return(-1); }
                *p=EOS; ++p;
/*   printf("\nq=%s",q); getch();   */

                if (mat_element) str_opnd[n_mat_par++]=q;
                else              { i=laske(q,&opnd[t]);
                                    if (i<0 || l_virhe) return(-1);
                                  }
/*   printf("\ntulos1=%f",opnd[t]); getch();  */
                if (len==0) { len=-1; break; }
                sana[len]=EOS;
                if (narg>1)
                    {
/********************
             printf("\nFunktio: t=%d %s:",t,sana);
             printf("\nArgumentit: ");
             for (i=t-narg+1; i<=t; ++i) printf(" %g",opnd[i]); getch();
************************/
                    t=t-narg+1;
                    if (*sana=='-')
                        opnd[t]=-mfunktio(sana+1,opnd+t,narg);
                    else
                        opnd[t]=mfunktio(sana,opnd+t,narg);
                                               if (l_virhe) return(-1);

                    *sana=EOS;
                    len=-1;
                    break;
                    }

                /* Yhden muuttujan funktiot */
                if (*sana=='-')
                    opnd[t]=-funktio(sana+1,opnd[t]);
                else
                    opnd[t]=funktio(sana,opnd[t]);
                if (l_virhe) return(-1);
                len=-1;
                break;

              case ')':
                sprintf(sbuf,"\n( missing in %s",lauseke);
                sur_print(sbuf); l_virhe=1; return(-1);
              case 'e': case 'E':
                if (len!=0)
                    {
                    if (strchr("+-.0123456789",sana[0])!=NULL)
                        {
                        sana[len++]=*p; ++p;
                        if (*p!='+' && *p!='-') break;
                        }
                    }
              default:     /* case 'e','E' oltava juuri edellä */
                sana[len++]=*p;
                ++p;
                }
            }

        if (len<0) { v[t++]=0; }
        else
                   if (len>0) { opnd[t]=luku(sana,len); v[t++]=0; }

        supista(&t,opnd,op,v);
        *y=opnd[0];
        return(1);
        }

int if_syntax_error(char *x)
        {
        sprintf(sbuf,"\nSyntax error in %s",x); sur_print(sbuf);
        WAIT; l_virhe=1;
        return(1);
        }


int varif(char *lauseke,double *y)
        {
        char *a,*b,*c,*d;
        char rel;
        char *p;
        int sulut;
        char x[LLENGTH];
        double y1;
        int tosi;

/*      printf("\nvarif: %s",lauseke); getch();     */
        /* if(<a><rel><b>)then(<c>)else(<d>)
           <a>,<b>,<c>,<d> lausekkeita
           <rel>: =,>,<,<>,>=,<=
                  = > < E  S  P
        */

        strcpy(x,lauseke);
        a=x+3;  /* if( ohitetaan */
        p=a; sulut=0;
        while (*p)
            {
            switch(*p)
                {
              case '=':
                rel=*p; *p=EOS; break;
              case '<':

                if (*(p+1)=='=') { rel='P'; *p=EOS; ++p; *p=EOS; break; }
                if (*(p+1)=='>') { rel='E'; *p=EOS; ++p; *p=EOS; break; }
                rel=*p; *p=EOS; break;
              case '>':
                if (*(p+1)=='=') { rel='S'; *p=EOS; ++p; *p=EOS; break; }
                rel=*p; *p=EOS; break;
              case ')':
                --sulut; ++p;
                if (sulut<0)
                    {
                    sprintf(sbuf,"\nrelation symbol =<> missing! in %s",x);
                    sur_print(sbuf); WAIT; l_virhe=1; return(-1);
                    }
                break;
              case '(':
                ++sulut; ++p;
                break;
              default:
                ++p;
                }
            }

/*  printf("\na=%s rel=%c",a,rel);      */
        b=p+1;
        p=b;
        while (1)
            {
            p=strchr(p,')');
            if (p==NULL) { if_syntax_error(lauseke); return(-1); }
            if (strncmp(p,")then(",6)==0) { *p=EOS; break; }
            ++p;
            }
/*  printf(" b=%s",b);  */
        c=p+6;
        p=c; sulut=0;
        while (*p)
            {
            if (*p=='(') { ++sulut; ++p; continue; }
            if (*p==')')
                {
                if (!sulut) break;
                --sulut;
                }
            ++p;
            }
        if (*p==EOS) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;
        if (strncmp(p+1,"else(",5)!=0)
            { if_syntax_error(lauseke); return(-1); }
        d=p+6;
        p=d; sulut=0;
        while (*p)
            {
            if (*p=='(') { ++sulut; ++p; continue; }
            if (*p==')')
                {
                if (!sulut) break;
                --sulut;
                }
            ++p;
            }
        if (*p==EOS) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;
/* printf(" c=%s d=%s",c,d);
getch();
*/
        laske(a,y);
        laske(b,&y1);
        tosi=0;
        switch (rel)
            {
          case '=': if (*y==y1) tosi=1; break;
          case '<': if (*y<y1) tosi=1; break;
          case '>': if (*y>y1) tosi=1; break;
          case 'E': if (*y!=y1) tosi=1; break;
          case 'P': if (*y<=y1) tosi=1; break;
          case 'S': if (*y>=y1) tosi=1; break;
            }

        if (tosi) laske(c,y);
        else      laske(d,y);
        return(1);
        }




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
        sur_print(sbuf); WAIT;
        return(1);
        }


int varaa_earg()
        {
        earg=(double *)malloc(MAXEARG*sizeof(double));
        if (earg==NULL)
            {
            sur_print("\nNot enough memory!");
            l_virhe=1;
            WAIT; return(-1);
            }
     /* earg_varattu=1; */
        return(1);
        }

int sp_init(int lin)
        {
        int i;
/*        unsigned int tila;
        char *p; */
        int spn1;
        int raja1;
        char x[LLENGTH];

	/* Check the number of specifications and needed space */
        sp_check();

        speclist+=4;
        specmax+=3;   /* +1 @r 4.10.1999 */

	/* Allocate memory for specifications */
        splist=malloc((unsigned int)speclist);
        if (splist==NULL) { not_enough_mem_for_spec(); return(-1); }
        spa=(char **)malloc(specmax*sizeof(char *));
        if (spa==NULL) { not_enough_mem_for_spec(); return(-1); }
        spb=(char **)malloc(specmax*sizeof(char *));
        if (spb==NULL) { not_enough_mem_for_spec(); return(-1); }
        spshad=(char **)malloc(specmax*sizeof(char *));
        if (spshad==NULL) { not_enough_mem_for_spec(); return(-1); }
        arvo=(double *)malloc(specmax*sizeof(double));
        if (arvo==NULL) { not_enough_mem_for_spec(); return(-1); }
        spp=malloc((unsigned int)specmax);
        if (spp==NULL) { not_enough_mem_for_spec(); return(-1); }
        spplace=(unsigned int *)malloc(specmax*sizeof(unsigned int));
        if (spplace==NULL) { not_enough_mem_for_spec(); return(-1); }
        i=varaa_earg(); if (i<0) return(-1);

        spn=0; spl=splist; global=0;
        edread(x,lin); spn=spread3(x,lin);		/* Specifications from the current line */
        if (spn<0) { spxxxx(); return(spn); }
        spn=spread2(lin,&raja1);			/* Local specifications	*/
        if (spn<0) { spxxxx(); return(spn); }
        if (raja1==0) { spxxxx(); return(spn); }
        spn1=spn;
        global=1;			
        spn=spread2(1,&raja1);				/* Specifications in *GLOBAL* area */
        if (spn<0) { spxxxx(); return(spn); }
        if (global==1) spn=spn1;
        spxxxx();

        return(spn);
        }


int spec_init(int lin)
        {
        int i;


/* sur_print("\nspec_init!"); getck(); */
        i=sp_init(lin);

Rprintf("Spesifikaatioita löytyi: %d\n",i);

        if (i<0)
            {
            sur_print("\nToo many specifications!");
            WAIT;
            }
        else
            {
            spp[spn]='='; /* multiple (.=) activations!!! 18.7.2000 */
            spa[spn++]="@r"; /* root_of() argument x replacement */
            ++i;
            }

        return(i);
        }

int kirjoita2(char *sana,int j,int sar)
        {
        int i,len;
        char rivi[LLENGTH];

        edread(rivi,j);
        len=strlen(sana);
        strncpy(rivi+sar,sana,(unsigned int)len);
        i=sar+len;
        while (i<LLENGTH && rivi[i]!=' ') rivi[i++]=' ';
        edwrite(rivi,j,0);
        return(1);
        }

int kirjoita(double tulos,int j,int sar)
        {
        int i;
        char sana[LLENGTH];
        char x[LLENGTH];
        char *p;
        double a;
/*        int e;  */

        if (puhdas_dat_kysely) return(1);
        if (*(z+(j-1)*ed1)=='\'')
            {
            sprintf(sana,"%.16e",tulos);
            kirjoita2(sana,j,sar);
            return(1);
            }

        if (spn)
            {
            i=spfind("ACCURACY");
            if (i>=0)
                {
                laske("ACCURACY",&a); tarkkuus=a;
                }
            }

        if (tarkkuus)     /* 9.11.1989 */
            {
            if (tarkkuus<0) /* 31.3.1996 */
                {
                sprintf(x,"%.*f",-tarkkuus,tulos);
                p=x; if (*p==' ') ++p;
                strcpy(sana,p);
                }
            else
                {
                if (tarkkuus>20) tarkkuus=20; /* 11.7.2002 */

                fnconv(tulos,tarkkuus+3,x);
                p=x; while(*p && *p==' ') ++p;
                strcpy(sana,p);
                if (strchr(sana,'.')!=NULL && strchr(sana,'e')==NULL)
                    {
                    p=sana+strlen(sana)-1;
                    while (*p=='0') { *p=EOS; --p; }
                    if (*p=='.') *p=EOS;
                    }
                }
            }
        else
            fconv(tulos,"",sana);
        kirjoita2(sana,j,sar);
        return(1);
        }


int op_arit()
        {
        char lauseke[LLENGTH];
        char rivi[LLENGTH],*osa[1];
        double tulos;
        int i,k;
        int monia=0;

        stackp1=(unsigned char *)lauseke; /* 16.1.2003 pinon pituuden mittaamiseen! */

        remember=0;
/*        strcpy(help_sana,"ARIT");  */
        l_virhe=0; errno=0; tarkkuus=0;
/*        nmat=0; */ /* 8.5.1999 */
        i=spec_init(r1+r-1); if (i<0) return(-1);  /* 14.1.92 */

Rprintf("Spec_initin paluuarvo: %d\n",i);

        if (spn)
            {
            i=spfind("ACCURACY");
            if (i>=0)
                {
                laske("ACCURACY",&tulos); tarkkuus=(int)tulos;

Rprintf("ACCURACYlla asetettu tarkkuus=%d\n",(int)tulos);

                }

            if (tarkkuus>16) /* 11.8.2002 */
                {
/*
                char op_sana[LNAME];
                extern char *op;

                strcpy(op_sana,"MARIT"); op=op_sana;
                childp("");
                return(1);
*/
                }
            }

        edread(rivi,r1+r-1);
        strcpy(lauseke,rivi);
        i=c1+c-2;
        lauseke[i]=EOS;

        if (lauseke[i-1]=='.') { lauseke[--i]=EOS; monia=1; }
        while (lauseke[i]!=' '&& i>0) --i;
/*  printf("\nlauseke=%s\n",lauseke+i+1); WAIT; */

        puhdas_dat_kysely=0; /* 8.12.2002 */
        strcpy(sbuf,lauseke+i+1);
/* printf("\nsbuf=%s|",sbuf); WAIT; */
        if (sbuf[0]=='D' && strncmp(sbuf,"DAT_",4)==0)
            {
            for (k=4; k<strlen(sbuf); ++k)
                if (strchr("+-*/%",sbuf[k])!=NULL) break;
            if (k==strlen(sbuf)) puhdas_dat_kysely=1;
            }
/* printf("\npuhdas=%d|",puhdas_dat_kysely); WAIT; */
        i=laske(lauseke+i+1,&tulos);
/*
        if (i==2222) { strcpy(help_sana,"CONV"); return(1); } *//* conversions by CONV1 */
/*
printf("i=%d l_virhe=%d errno=%d tulos=%g\n",i,l_virhe,errno,tulos); getch();
*/
        if (i<0 || l_virhe || errno)
            {
/* printf("\nmuuttuja=%s|",tuntematon_muuttuja); getch();  2.12.2008 */
            i=split(rivi+1,osa,1);
            if (sur_strcmpi(osa[0],"VAR")==0 || sur_strcmpi(osa[0],"MAT")==0
            || strcmp(tuntematon_muuttuja,"VARS")==0
            || strcmp(tuntematon_muuttuja,"IND")==0
            || strcmp(tuntematon_muuttuja,"CASES")==0
            || strcmp(tuntematon_muuttuja,"SELECT")==0  )

                {
/*
if (*language=='1')
sur_print("\nSiirrä kohdistin pois merkin = perästä ja aktivoi uudelleen!");
else
*/
sur_print("\nMove the cursor to the right or to the left and activate again!");
                WAIT; return(-1);
                }

      /*    kirjoita2("error",r1+r-1,c1+c-1);   poistettu 30.11.2008 */
            if (etu==2)
                {
                strcpy(tut_info,"þþþ@12@MATH@Error in editorial computing!@");
                return(-1);
                }
            if (remember) { remember=0; free(remember_space); }
            WAIT; return(1);
            }
        kirjoita(tulos,r1+r-1,c1+c-1);
/*      edisp=2;    */
        if (monia)
            {
            for (k=0; k<spn; ++k)
                {
                if (spp[k]!='.') continue;
                strcpy(lauseke,spa[k]);
                lauseke[strlen(lauseke)-1]=EOS;
                i=laske(lauseke,&tulos);
                if (i<0 || l_virhe || errno) { WAIT; return(-1); }
                kirjoita(tulos,(int)(spplace[k]/ed1+1),(int)spplace[k]%ed1+1);
                }
/*          edisp=1;     */
            }
        if (remember) { remember=0; free(remember_space); }
        free(earg);
        free(spplace); free(spp); free(arvo);
        free(spshad); free(spb); free(spa); free(splist);
        return(1);
        }

int aseta_earg(double luku,char *sana)
        {
        char sana2[5];

        sana[0]=EARG;
        if (n_earg>=MAXEARG)
            {
            sur_print("\nStack overflow in editorial functions!");
            WAIT; l_virhe=1;
            return(-1);
            }
          sana[1]=EOS; /* strcat(sana,itoa(n_earg,sana2,10)); */

       itoa(n_earg,sana2);
       strcat(sana,sana2);

        earg[n_earg++]=luku;
        return(n_earg-1);
        }


int korvaa(char *s,char *x,char *y)
        {
        char *p,*q;
        char z[LLENGTH];
        int len=strlen(x);

        *z=EOS;
        p=s;
        while ((q=strstr(p,x))!=NULL)
            {
            if (strchr(",+-*/^)=<>!",*(q+len))!=NULL || *(q+len)==EOS)
                {
                strncat(z,p,(unsigned int)(q-p));
                strcat(z,y);
                p=q+len;
                }
            else  /* x osa funktion nimeä */
                {
                strncat(z,p,(unsigned int)(q-p));
                strcat(z,x);
                p=q+len;
                }
            }
        strcat(z,p);
        strcpy(s,z);
        return(1);
        }



int parsplit(char *x,char **a,char **b,int max)
        {
        int i,sulut;
        char *p;
        char y[LLENGTH];

        strcpy(y,x);
        i=0;
        p=x;
        while (*p)
            {
            a[i]=p;
            while (*p)
                {
                if (*p=='(') break;
                if (*p==')') { if_syntax_error(y); return(-1); }
                ++p;
                }
            if (*p==EOS) { if_syntax_error(y); return(-1); }
            *p=EOS;
            b[i]=++p;
            sulut=1;
            while (*p)
                {
                if (*p==')') { --sulut; if (!sulut) break; }
                else if (*p=='(') ++sulut;
                ++p;
                }
            if (sulut) { if_syntax_error(y); return(-1); }
            *p=EOS;
            ++p; if (*p==EOS) break;
            ++i;
            if (i>=max) { if_syntax_error(y); return(-1); }
            }
        return(i+1);
        }

int integral_syntax_error(char *s)
        {
        sprintf(sbuf,"\nSyntax error in %s",s); sur_print(sbuf);
        sur_print("\nCorrect forms:");
        sur_print("\nintegral(f(x))from(a)to(b)");
    sur_print("\nintegral(f(x))from(a)to(b)eps(eps)     (eps=relative error)");
    sur_print("\nintegral(f(x))from(a)to(b)eps(eps)n(n) (2^n point division)");
        sur_print("\nDefault: eps=1e-10, n=12");
        WAIT;
        return(1);
        }


/* arint.c 2.1.1995/SM  (4.1.1995)
   aritmetiikka integral(f(x))from(a)to(b)
                integral(f(x))from(a)to(b)eps(eps)
                integral(f(x))from(a)to(b)eps(eps)n(n)
*/
int laske_integral(char *lauseke,double *y)
        {
/*        int i,  */
	int g;
        char *sana[5],*laus[5];
        char x[LLENGTH];
        double alku,loppu,eps;
/*        char *p;   */
        char esana[7];
        int iind;
        char sterm[LLENGTH];
        double fa,fb,h,t,m,xx,s;
        int n,n0;

        strcpy(x,lauseke);
        g=parsplit(x,sana,laus,5);
/* for (i=0; i<g; ++i) printf("\n%s %s",sana[i],laus[i]); getch(); */
        if (g<0) return(-1);
        if (g<3) { integral_syntax_error(lauseke); return(-1); }
        strcpy(sterm,laus[0]);
        laske(laus[1],&alku);
        laske(laus[2],&loppu);
        eps=1e-10; n0=12;
        if (g>3) laske(laus[3],&eps);
        if (g>4) { laske(laus[4],&s); n0=s; }
/* printf("\n%g %g %g",alku,loppu,eps); getch(); */
        iind=aseta_earg(alku,esana);
        if (iind<0) return(-1);
        korvaa(sterm,"x",esana);
/* printf("\nsterm=%s",sterm); getch(); */
/* koe:
    earg[iind]=3.5; laske(sterm,&ss);
printf("\nss=%g",ss); getch();
*/

        h=loppu-alku;
        earg[iind]=alku; laske(sterm,&fa);
        earg[iind]=loppu; laske(sterm,&fb);
        t=(fa+fb)*h; m=0.0;
        n=0;
        while (n<n0)
            {
            t=(t+m)/2; m=0.0;
            for (xx=alku+h/2; xx<=loppu; xx+=h)
                {
                earg[iind]=xx; laske(sterm,&fa);
                m+=fa;
                }
            m*=h; s=(t+2*m)/3;
            h/=2;
   /*       printf("\ns=%.16g",s);   */
            if (fabs(t-m)/(fabs(s)+eps*eps)<eps) break;
            ++n;
            }
        *y=s;
        n_earg-=1;
        return(1);
        }

/* arifor.c 31.12.1986/SM  (11.1.1987)
   aritmetiikka for(i=a)to(n)sum(t)
                for(i=a)to(n)term(t=b)sum(f(term,i))
                             sum product max
*/

int arifor(char *lauseke,double *y)
        {
/*        int i, */
        int g;
        char *sana[4],*laus[4];
        char x[LLENGTH];
        long ialku,iloppu,il,imax;
        double d;
        char *p;
        char sterm[LLENGTH];
        double term,sum;
        int iterm,iind,tind;
        char esana[7];
        int max;

        strcpy(x,lauseke);
        g=parsplit(x,sana,laus,4);
        if (g<0) return(-1);
        if (g<3) { if_syntax_error(lauseke); return(-1); }
/*
   for (i=0; i<g; ++i) printf("\nfor: %d %s %s",i,sana[i],laus[i]); getch();
*/
        p=strchr(laus[0],'=');
        if (p==NULL) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;   /* laus[0]='i'  */
        laske(p+1,&d); ialku=d;
        laske(laus[1],&d); iloppu=d;

        iterm=0;
        if (strcmp(sana[2],"term")==0)
            {
            iterm=1;
            p=strchr(laus[2],'=');
            if (p==NULL) term=0.0;
            else { *p=EOS; laske(p+1,&term); }  /* laus[2]='term' */
            }
        strcpy(sterm,laus[2+iterm]);
        iind=aseta_earg((double)ialku,esana);
        if (iind<0) return(-1);
        korvaa(sterm,laus[0],esana);
        if (iterm)
            {
            tind=aseta_earg(term,esana);
            if (iind<0) return(-1);
            korvaa(sterm,laus[2],esana);
            }

        max=0; p=sana[2+iterm];
        if (strncmp(p,"max",3)==0 || strncmp(p,"min",3)==0)
            {
            if (p[2]=='n') { max=3; sum=1e300; if (p[3]=='i') max=4; }
            else           { max=1; sum=-1e300; if (p[3]=='i') max=2; }
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm)
                 { earg[tind]=term; if (il>ialku) laske(sterm,&term); }
                else laske(sterm,&term);
                if (max<3 && term>sum) { sum=term; imax=il; }
                if (max>2 && term<sum) { sum=term; imax=il; }  /* imax=imin */
                }
            }
        else if (strcmp(p,"sum")==0)
            {
            sum=0.0;
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm)
                 { earg[tind]=term; if (il>ialku) laske(sterm,&term); }
                else laske(sterm,&term);
                sum+=term;
                }
            }
        else if (strcmp(p,"product")==0)
            {
            sum=1.0;
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm)
                 { earg[tind]=term; if (il>ialku) laske(sterm,&term); }
                else laske(sterm,&term);
                sum*=term;
                }
            }
        else if (strcmp(p,"term")==0)
            {
            for (il=ialku; il<=iloppu; ++il)
                {
                earg[iind]=(double)il;
                if (iterm)
                 { earg[tind]=term; if (il>ialku) laske(sterm,&term); }
                else laske(sterm,&term);
                }
            sum=term;
            }
        else { if_syntax_error(lauseke); return(-1); }

        if (max==2 || max==4) *y=(double)imax; else *y=sum;
        n_earg-=1+iterm;
        return(1);
        }

int root_of(char *lauseke,double *y)  /* root_of(f(x),x1,x2,accuracy) */
        {
        int i,i_x;
        char *p,*q;
        char s[LNAME],*par[4];
        double x1,x2;
        char fct[LNAME];
        char fct0[LNAME];
        double acc;
        double dx,f,fmid,xmid,rtb;

/* printf("lauseke=%s\n",lauseke); getch(); */

        p=strchr(lauseke,'(');
        strcpy(s,p+1);
        i=split(s,par,4);
        if (i<3)
            {
            sur_print("\nUsage: root_of(f(x),x1,x2,accuracy)");
            WAIT; l_virhe=1; return(-1);
            }
        x1=atof(par[1]);
        x2=atof(par[2]);
        strcpy(fct,par[0]);

        q=fct0;
        p=fct;
        while (*p)
            {
            if (*p=='x') { *q++='@'; *q++='r'; ++p; }
            else *q++=*p++;
            }
        *q=EOS;

        if (i==4) acc=atof(par[3]); else acc=1e-14;

        i_x=spfind("@r"); spb[i_x]=NULL;

/* printf("i_x=%d fct0=%s\n",i_x,fct0); getch(); */

        strcpy(fct,fct0); arvo[i_x]=x1; laske(fct,&f);
        if (f==0.0) { *y=x1; return(1); }
        strcpy(fct,fct0); arvo[i_x]=x2; laske(fct,&fmid);
        if (fmid==0.0) { *y=x2; return(1); }
        if (f*fmid>0.0)
            {
            sprintf(sbuf,"\nRoot not bracketed by %g and %g!",
                                x1,x2);
            sur_print(sbuf); WAIT; l_virhe=1; return(-1);
            }
        rtb=f<0.0? (dx=x2-x1,x1) : (dx=x1-x2,x2);
        for (i=0; i<100; ++i)
            {
            dx/=2;
            xmid=rtb+dx;
            strcpy(fct,fct0); arvo[i_x]=xmid; laske(fct,&fmid);
            if (fmid<=0.0) rtb=xmid;
            if (fabs(dx)<acc || fmid==0.0) { *y=rtb; return(1); }
            }
        sur_print("\nRoot not found!"); WAIT; l_virhe=1;
        return(-1);
        }




double funktio(char *s, double x)
        {
        char S[32];
/*
        int i;
        double y;
        double xx;
*/

/*
        extern double probit();
        extern double uniform();
        extern double sur_rand0();
        extern double round();
        extern double fact();
        extern double lfact();
        extern double zeta();
        extern double nfactors();
*/

        if (*s==EOS) return(x);
        strncpy(S,s,31); S[31]=EOS; strupr(S);

        if (strncmp(S,"SQR",3)==0) return(sqrt(x));
        if (strcmp(S,"LOG")==0) return(log(x));
        if (strcmp(S,"EXP")==0) return(exp(x));
        if (strcmp(S,"SIN")==0) return(sin(x));
        if (strcmp(S,"COS")==0) return(cos(x));
        if (strcmp(S,"TAN")==0) return(tan(x));
        if (strcmp(S,"ARCTAN")==0) return(atan(x));
        if (strcmp(S,"ARCSIN")==0) return(asin(x));
        if (strcmp(S,"ARCCOS")==0) return(acos(x));
        if (strcmp(S,"ABS")==0) return(fabs(x));
        if (strcmp(S,"INT")==0) return(floor(x));
        if (strcmp(S,"SGN")==0)
            {
            if (x>0.0) return(1.0);
            if (x<0.0) return(-1.0);
            return (0.0);
            }
        if (strcmp(S,"IND")==0)
            { if (x>0.0) return(1.0); else return(0.0); }

/*
        if (strcmp(S,"RND")==0) return(uniform(x));
        if (strcmp(S,"PROBIT")==0) return(probit(x));
        if (strcmp(S,"RAND")==0) return(sur_rand0(x)); 
        if (strcmp(S,"ROUND")==0) return(round(x)); 
        if (strcmp(S,"FACT")==0) return(fact(x)); 
        if (strcmp(S,"LFACT")==0 || strcmp(S,"FACT.L")==0)
                      return(lfact(x)); 
        if (strcmp(S,"NFACTORS")==0) return(nfactors(x));
        if (strcmp(S,"ZETA")==0) return(zeta(x));


        xx=x;

        if (*s=='M' && strncmp(s,"MAT_",4)==0)
            {
            mat_function(s+4,str_opnd,1,&y);
            return(y);
            }

        i=f_edit(s,&xx,1,&y); if (i>0) return(y);
        i=f_tiedosto(s,&xx,1,&y);
        set_console_title();
        if (i>0 && y!=MISSING8) return(y);

*/
        l_virhe=1;
        return(x);
        }


int arg_virhe(char *s)
        {
        sprintf(sbuf,"\n%s: Error in arguments",s); sur_print(sbuf);
        l_virhe=1;
        return(1);
        }



double mfunktio(char *s,double *x,int n)
        {
        int i,k;
        double y;
        char S[32];

/*     printf("\nmfunktio: %s:",S);
   for (i=0; i<n; ++i) printf("%g ",x[i]); getch();
*/

        strncpy(S,s,31); S[31]=EOS; strupr(S);

        if (strcmp(S,"MAX")==0)
            {
            y=x[0];
            for (i=1; i<n; ++i) y=(x[i]>y)? (x[i]):(y);
            return(y);
            }
        if (strcmp(S,"MIN")==0)
            {
            y=x[0];
            for (i=1; i<n; ++i) y=(x[i]<y)? (x[i]):(y);
            return(y);
            }
        if (strcmp(S,"MAXN")==0)  /* 6.4.2003 */
            {
            y=x[0]; k=0;
            for (i=1; i<n; ++i) if (x[i]>y) { y=x[i]; k=i; }
            return((double)(k+1));
            }
        if (strcmp(S,"MINN")==0)
            {
            y=x[0]; k=0;
            for (i=1; i<n; ++i) if (x[i]<y) { y=x[i]; k=i; }
            return((double)(k+1));
            }
        if (strcmp(s,"C")==0)
            {
            double u,v;
            int iu,iv;

            if (n!=2) { arg_virhe(s); }
            iv=v=x[0]; iu=u=x[1];
            if ((double)iu!=u) return(0.0);
            if ((double)iv!=v) return(0.0);
            if (u>v/2) u=v-u;
            if (u<0 || v<0) return(0.0);
            if (u==0.0) return(1.0);
            y=1.0;
            for (; u>0; --u, --v) y*=(v/u);
            return(y);
            }

        if (strcmp(S,"K_FACT")==0 || strcmp(S,"LK_FACT")==0)
            {
            double u,v;
            int h;

            h=0; if (*S=='L') h=1;
            if (n!=2) { arg_virhe(s); }
            v=x[0]; u=x[1];
            if (u<1.0) return(0.0);
            if (h)
                {
                y=log(v);
                for (i=1; i<u; ++i) y+=log(v-i);
                }
            else
                {
                y=v;
                for (i=1; i<u; ++i) y*=v-i;
                }
            return(y);
            }

/*
        if (strcmp(S,"GCD")==0)
            {
            extern double gcd();

            return (gcd(x[0],x[1]));
            }
        if (strcmp(S,"MOD")==0)
            {
            return((double)((unsigned long)x[0]%(unsigned long)x[1]));
            }
        if (strcmp(S,"ROOT")==0)  
            {
            extern double root();
            return (root(x[0],x[1]));
            }
        if (strcmp(S,"ROUND")==0) 
            {
            extern double round();
            y=pow(10,x[1]);
            return(round(x[0]*y)/y);
            }
*/
/* 14.8.2005 days from 1.1.2000
        if (strcmp(S,"DAYS")==0)
            {
            double date;
            sur_julian(x[0],x[1],x[2],&date);
            return(date-2451544.0);
            }

        if (strcmp(S,"X")==0)
            {
            extern double ed_number();
            return (ed_number(x[0],x[1]));
            }

        if (*s=='M' && strncmp(s,"MAT_",4)==0)
            {
            mat_function(s+4,str_opnd,n,&y);
            return(y);
            }


        if (*s=='D' && strncmp(s,"DAT_",4)==0) // 5.12.2002
            {
            dat_function(s+4,str_opnd,n,&y);
            return(y);
            }

        i=f_edit(s,x,n,&y); if (i>0) return(y);
        i=f_tiedosto(s,x,n,&y);
        set_console_title();
        if (i>0 && y!=MISSING8) return(y);
*/

        l_virhe=1;
        return(x[0]);
        }

