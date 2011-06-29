#include <R.h>
#include <Rinternals.h>
#include <Rmath.h>
#include <math.h>
/* #include <conio.h>*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
/* #include <process.h> */
#include <errno.h>
#include <time.h>
#include <ctype.h>
#include "survo.h"
#include "survolib.h"

#define MAXPITUUS 200
/*                25.7.2006 */
#define MAXARG 10
#define EOS '\0'
#define EARG '\376'

/* #define MISSING8 1e306 is one bit less in Watcom */
#define MISSING8 *(double *)"\51\220\43\312\345\310\166\177"

#define RND (double)rand()/RAND_MAX  /* RAND_MAX=32768.0 */

#define MAXEARG 1000
/* int earg_varattu=0; */
int n_earg=0;
double *earg;

extern char *z;
extern int r,r1,r2,r3,c,c1,c2,c3;
extern int ed1,ed2;
extern int *psur_seed;
extern unsigned int *zs;
extern char survo_path[];

/* specifications in the edit field */
extern char *splist;
extern char **spa, **spb, **spshad;
extern int spn;

extern char *spp;
extern unsigned int *spplace;


extern int errno;
extern char sbuf[];
extern int child_call;
extern char help_sana[];

double *arvo; /* vain arit.c tarvitsee  */
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

extern int etu;
extern char tut_info[];





int varaa_earg()
{
    int i;
    earg=(double *)malloc(MAXEARG*sizeof(double));
    if (earg==NULL)
    {
        sur_print("\nNot enough memory!");
        l_virhe=1;
        WAIT;
        return(-1);
    }
    for (i=0; i<MAXEARG; i++) earg[i]=0.0;
    /* earg_varattu=1; */
    return(1);
}


int replace_function_name(char *sana,int *plen) /* 13.2.2005 esim. M()=MAT_RG.M() */
{
    int i;
    char sana2[32];
    int len;
    char *p;
    char x[32];

    *sana2=EOS;
    strncat(sana2,sana,(unsigned int)*plen);
    strcat(sana2,"()");
    i=spfind(sana2);
    if (i<0) return(1);
    /* printf("\nfunc=%s|",spb[i]); getch(); */
    strcpy(x,spb[i]);
    p=strchr(x,'(');
    if (p!=NULL) *p=EOS;
    len=strlen(x);
    strcpy(sana2,spb[i]);
    strcat(sana2,sana+*plen);
    strcpy(sana,sana2);
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
        if (x2==0.0)
        {
            l_virhe=1;
            return(0.0);
        }
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
    sprintf(sbuf,"\n%s not found!",muuttuja);
    sur_print(sbuf);
    l_virhe=1;
    if (remember)
    {
        remember=0;
        free(remember_space);
    }
    return(1);
    return(1);
}


int laske();

int laske2(char *muuttuja,double *y)
{
    int i,k;
    /* LOCATE(1,50); sprintf(sbuf,"muuttuja=%s|",muuttuja);
     sur_print(sbuf); cursor(r,c); getck();
    */
    if (*muuttuja==EARG)
    {
        *y=earg[atoi(muuttuja+1)];
        return(1);
    }

    i=spfind(muuttuja);
    if (i<0)
    {
        if ((strlen(muuttuja)==1 || muste_strcmpi(muuttuja,"CUR")==0  /* 24.3.1998 */
                || muste_strcmpi(muuttuja,"END")==0)
                && (i=edline2(muuttuja,1,0))!=0) /* 16.5.1997 */
        {
            *y=(double)i;
            return(1);
        }
        virhe_not_found(muuttuja);
        return(1);
    }
    if (spb[i]==NULL)
    {
        *y=arvo[i];
        return(1);
    }
    if (*spb[i]==EOS)
    {
        virhe_not_found(muuttuja);
        return(1);
    }
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
    p=sana;
    if (*p=='-') ++p;
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
        op[*t-2]=op[*t-1];
        v[*t-2]=v[*t-1];
        --(*t);
    }
    return(1);
}



int syntax_error(char *s)
{
    sprintf(sbuf,"\nsyntax error in %s",s);
    sur_print(sbuf);
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
    double opnd[MAXARG+4];
    char op[MAXARG+4];
    int v[MAXARG+4];
    int t,n;
    int narg; /* Usean muuttujan funktion argumenttien lkm */
    int i;

    int mat_element; /* 10.5.2005 */
    int n_mat_par;



    /* sprintf(sbuf,"%ld ",(long)(stackp1-x)); sur_print(sbuf); sur_wait(10L); */

    /* Tarkista pinon koko */
    /*        if (check_stack>0L)  && (long)(stackp1-x)>(long)check_stack)
                {
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
        sprintf(sbuf,"\n%s",lauseke);
        sur_print(sbuf);
        sprintf(sbuf,"\nis too long! (more than %d characters)",MAXPITUUS-1);
        sur_print(sbuf);
        WAIT;
        l_virhe=1;
        return(-1);
    }

    if (*lauseke=='-')  /* 8.12.89      "-x" to the form "0-x" */
    {
        *x='0';
        strcpy(x+1,lauseke);
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
            if (len==0)
            {
                ++p;
                break;
            }
            if (len>0) opnd[t]=luku(sana,len);
            len=0;
            op[t]='+';
            v[t++]=1;
            supista(&t,opnd,op,v);
            ++p;
            break;

        case '-':
            if (len==0)
            {
                sana[len++]=*p;
                ++p;
                break;
            }
            if (len>0) opnd[t]=luku(sana,len);
            len=0;
            op[t]='-';
            v[t++]=1;
            supista(&t,opnd,op,v);
            ++p;
            break;

        case '*':
            if (len==0)
            {
                syntax_error(lauseke);
                return(-1);
            }
            if (len>0) opnd[t]=luku(sana,len);
            len=0;
            op[t]='*';
            v[t++]=2;
            supista(&t,opnd,op,v);
            ++p;
            break;

        case '/':
            if (len==0)
            {
                syntax_error(lauseke);
                return(-1);
            }
            if (len>0) opnd[t]=luku(sana,len);
            len=0;
            op[t]='/';
            v[t++]=2;
            supista(&t,opnd,op,v);
            ++p;
            break;

        case '^':
            if (len==0)
            {
                syntax_error(lauseke);
                return(-1);
            }
            if (len>0) opnd[t]=luku(sana,len);
            len=0;
            op[t]='^';
            v[t++]=3;
            supista(&t,opnd,op,v);
            ++p;
            break;

        case '(':
            sana[len]=EOS; /* 15.2.2005 */
            replace_function_name(sana,&len); /* 13.2.2005 */

            mat_element=0;
            if (strncmp(sana,"MAT_",4)==0)
            {
                mat_element=1;
                n_mat_par=0;
            }
            if (strncmp(sana,"DAT_",4)==0)
            {
                mat_element=1;
                n_mat_par=0;
            }
            q=p+1;
            if (*q==')')
            {
                sprintf(sbuf,"\nArguments missing in %s",lauseke);
                sur_print(sbuf);
                l_virhe=1;
                return(-1);
            }
            n=1;
            narg=1;
            while (n)
            {
                ++p;
                if (*p=='(')
                {
                    ++n;
                    continue;
                }
                if (*p==')')
                {
                    --n;
                    continue;
                }
                if (*p==EOS)
                {
                    sprintf(sbuf,"\n) is missing in %s",lauseke);
                    sur_print(sbuf);
                    l_virhe=1;
                    return(-1);
                }
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
                    {
                        sprintf(sbuf,"\nToo many arguments in %s",lauseke);
                        sur_print(sbuf);
                        l_virhe=1;
                        return(-1);
                    }
                    ++narg;
                    q=p+1;
                }
                if (*p==':')
                {
                    conversions();
                    return(2222);
                }
            }
            if (strchr("+-*/^)\0",*(p+1))==NULL)
            {
                syntax_error(lauseke);
                return(-1);
            }
            *p=EOS;
            ++p;
            /*   printf("\nq=%s",q); getch();   */

            if (mat_element) str_opnd[n_mat_par++]=q;
            else
            {
                i=laske(q,&opnd[t]);
                if (i<0 || l_virhe) return(-1);
            }
            /*   printf("\ntulos1=%f",opnd[t]); getch();  */
            if (len==0)
            {
                len=-1;
                break;
            }
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
            sur_print(sbuf);
            l_virhe=1;
            return(-1);
        case 'e':
        case 'E':
            if (len!=0)
            {
                if (strchr("+-.0123456789",sana[0])!=NULL)
                {
                    sana[len++]=*p;
                    ++p;
                    if (*p!='+' && *p!='-') break;
                }
            }
        default:     /* case 'e','E' oltava juuri edellä */
            sana[len++]=*p;
            ++p;
        }
    }

    if (len<0)
    {
        v[t++]=0;
    }
    else
        if (len>0)
        {
            opnd[t]=luku(sana,len);
            v[t++]=0;
        }

    supista(&t,opnd,op,v);
    *y=opnd[0];
    return(1);
}

int if_syntax_error(char *x)
{
    sprintf(sbuf,"\nSyntax error in %s",x);
    sur_print(sbuf);
    WAIT;
    l_virhe=1;
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
    p=a;
    sulut=0;
    while (*p)
    {
        switch (*p)
        {
        case '=':
            rel=*p;
            *p=EOS;
            break;
        case '<':

            if (*(p+1)=='=')
            {
                rel='P';
                *p=EOS;
                ++p;
                *p=EOS;
                break;
            }
            if (*(p+1)=='>')
            {
                rel='E';
                *p=EOS;
                ++p;
                *p=EOS;
                break;
            }
            rel=*p;
            *p=EOS;
            break;
        case '>':
            if (*(p+1)=='=')
            {
                rel='S';
                *p=EOS;
                ++p;
                *p=EOS;
                break;
            }
            rel=*p;
            *p=EOS;
            break;
        case ')':
            --sulut;
            ++p;
            if (sulut<0)
            {
                sprintf(sbuf,"\nrelation symbol =<> missing! in %s",x);
                sur_print(sbuf);
                WAIT;
                l_virhe=1;
                return(-1);
            }
            break;
        case '(':
            ++sulut;
            ++p;
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
        if (p==NULL)
        {
            if_syntax_error(lauseke);
            return(-1);
        }
        if (strncmp(p,")then(",6)==0)
        {
            *p=EOS;
            break;
        }
        ++p;
    }
    /*  printf(" b=%s",b);  */
    c=p+6;
    p=c;
    sulut=0;
    while (*p)
    {
        if (*p=='(')
        {
            ++sulut;
            ++p;
            continue;
        }
        if (*p==')')
        {
            if (!sulut) break;
            --sulut;
        }
        ++p;
    }
    if (*p==EOS)
    {
        if_syntax_error(lauseke);
        return(-1);
    }
    *p=EOS;
    if (strncmp(p+1,"else(",5)!=0)
    {
        if_syntax_error(lauseke);
        return(-1);
    }
    d=p+6;
    p=d;
    sulut=0;
    while (*p)
    {
        if (*p=='(')
        {
            ++sulut;
            ++p;
            continue;
        }
        if (*p==')')
        {
            if (!sulut) break;
            --sulut;
        }
        ++p;
    }
    if (*p==EOS)
    {
        if_syntax_error(lauseke);
        return(-1);
    }
    *p=EOS;
    /* printf(" c=%s d=%s",c,d);
    getch();
    */
    laske(a,y);
    laske(b,&y1);
    tosi=0;
    switch (rel)
    {
    case '=':
        if (*y==y1) tosi=1;
        break;
    case '<':
        if (*y<y1) tosi=1;
        break;
    case '>':
        if (*y>y1) tosi=1;
        break;
    case 'E':
        if (*y!=y1) tosi=1;
        break;
    case 'P':
        if (*y<=y1) tosi=1;
        break;
    case 'S':
        if (*y>=y1) tosi=1;
        break;
    }

    if (tosi) laske(c,y);
    else      laske(d,y);
    return(1);
}




int spec_init_arit(int lin)
{
    int i;


    /* sur_print("\nspec_init!"); getck(); */
    i=sp_init(lin);

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
            laske("ACCURACY",&a);
            tarkkuus=a;
        }
    }

    if (tarkkuus)     /* 9.11.1989 */
    {
        if (tarkkuus<0) /* 31.3.1996 */
        {
            sprintf(x,"%.*f",-tarkkuus,tulos);
            p=x;
            if (*p==' ') ++p;
            strcpy(sana,p);
        }
        else
        {
            if (tarkkuus>20) tarkkuus=20; /* 11.7.2002 */

            fnconv(tulos,tarkkuus+3,x);
            p=x;
            while (*p && *p==' ') ++p;
            strcpy(sana,p);
            if (strchr(sana,'.')!=NULL && strchr(sana,'e')==NULL)
            {
                p=sana+strlen(sana)-1;
                while (*p=='0')
                {
                    *p=EOS;
                    --p;
                }
                if (*p=='.') *p=EOS;
            }
        }
    }
    else
        fconv(tulos,"",sana);
    kirjoita2(sana,j,sar);
    return(1);
}

int aseta_earg(double luku,char *sana)
{
    char sana2[5];

    sana[0]=EARG;
    if (n_earg>=MAXEARG)
    {
        sur_print("\nStack overflow in editorial functions!");
        WAIT;
        l_virhe=1;
        return(-1);
    }
    sana[1]=EOS; /* strcat(sana,itoa(n_earg,sana2,10)); */

    muste_itoa(n_earg,sana2,10);
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
            if (*p==')')
            {
                if_syntax_error(y);
                return(-1);
            }
            ++p;
        }
        if (*p==EOS)
        {
            if_syntax_error(y);
            return(-1);
        }
        *p=EOS;
        b[i]=++p;
        sulut=1;
        while (*p)
        {
            if (*p==')')
            {
                --sulut;
                if (!sulut) break;
            }
            else if (*p=='(') ++sulut;
            ++p;
        }
        if (sulut)
        {
            if_syntax_error(y);
            return(-1);
        }
        *p=EOS;
        ++p;
        if (*p==EOS) break;
        ++i;
        if (i>=max)
        {
            if_syntax_error(y);
            return(-1);
        }
    }
    return(i+1);
}

int integral_syntax_error(char *s)
{
    sprintf(sbuf,"\nSyntax error in %s",s);
    sur_print(sbuf);
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
    if (g<3)
    {
        integral_syntax_error(lauseke);
        return(-1);
    }
    strcpy(sterm,laus[0]);
    laske(laus[1],&alku);
    laske(laus[2],&loppu);
    eps=1e-10;
    n0=12;
    if (g>3) laske(laus[3],&eps);
    if (g>4)
    {
        laske(laus[4],&s);
        n0=s;
    }
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
    earg[iind]=alku;
    laske(sterm,&fa);
    earg[iind]=loppu;
    laske(sterm,&fb);
    t=(fa+fb)*h;
    m=0.0;
    n=0;
    while (n<n0)
    {
        t=(t+m)/2;
        m=0.0;
        for (xx=alku+h/2; xx<=loppu; xx+=h)
        {
            earg[iind]=xx;
            laske(sterm,&fa);
            m+=fa;
        }
        m*=h;
        s=(t+2*m)/3;
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
    if (g<3)
    {
        if_syntax_error(lauseke);
        return(-1);
    }
    /*
       for (i=0; i<g; ++i) printf("\nfor: %d %s %s",i,sana[i],laus[i]); getch();
    */
    p=strchr(laus[0],'=');
    if (p==NULL)
    {
        if_syntax_error(lauseke);
        return(-1);
    }
    *p=EOS;   /* laus[0]='i'  */
    laske(p+1,&d);
    ialku=d;
    laske(laus[1],&d);
    iloppu=d;

    iterm=0;
    if (strcmp(sana[2],"term")==0)
    {
        iterm=1;
        p=strchr(laus[2],'=');
        if (p==NULL) term=0.0;
        else
        {
            *p=EOS;    /* laus[2]='term' */
            laske(p+1,&term);
        }
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

    max=0;
    p=sana[2+iterm];
    if (strncmp(p,"max",3)==0 || strncmp(p,"min",3)==0)
    {
        if (p[2]=='n')
        {
            max=3;
            sum=1e300;
            if (p[3]=='i') max=4;
        }
        else
        {
            max=1;
            sum=-1e300;
            if (p[3]=='i') max=2;
        }
        for (il=ialku; il<=iloppu; ++il)
        {
            earg[iind]=(double)il;
            if (iterm)
            {
                earg[tind]=term;
                if (il>ialku) laske(sterm,&term);
            }
            else laske(sterm,&term);
            if (max<3 && term>sum)
            {
                sum=term;
                imax=il;
            }
            if (max>2 && term<sum)
            {
                sum=term;    /* imax=imin */
                imax=il;
            }
        }
    }
    else if (strcmp(p,"sum")==0)
    {
        sum=0.0;
        for (il=ialku; il<=iloppu; ++il)
        {
            earg[iind]=(double)il;
            if (iterm)
            {
                earg[tind]=term;
                if (il>ialku) laske(sterm,&term);
            }
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
            {
                earg[tind]=term;
                if (il>ialku) laske(sterm,&term);
            }
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
            {
                earg[tind]=term;
                if (il>ialku) laske(sterm,&term);
            }
            else laske(sterm,&term);
        }
        sum=term;
    }
    else
    {
        if_syntax_error(lauseke);
        return(-1);
    }

    if (max==2 || max==4) *y=(double)imax;
    else *y=sum;
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
        WAIT;
        l_virhe=1;
        return(-1);
    }
    x1=atof(par[1]);
    x2=atof(par[2]);
    strcpy(fct,par[0]);

    q=fct0;
    p=fct;
    while (*p)
    {
        if (*p=='x')
        {
            *q++='@';
            *q++='r';
            ++p;
        }
        else *q++=*p++;
    }
    *q=EOS;

    if (i==4) acc=atof(par[3]);
    else acc=1e-14;

    i_x=spfind("@r");
    spb[i_x]=NULL;

    /* printf("i_x=%d fct0=%s\n",i_x,fct0); getch(); */

    strcpy(fct,fct0);
    arvo[i_x]=x1;
    laske(fct,&f);
    if (f==0.0)
    {
        *y=x1;
        return(1);
    }
    strcpy(fct,fct0);
    arvo[i_x]=x2;
    laske(fct,&fmid);
    if (fmid==0.0)
    {
        *y=x2;
        return(1);
    }
    if (f*fmid>0.0)
    {
        sprintf(sbuf,"\nRoot not bracketed by %g and %g!",
                x1,x2);
        sur_print(sbuf);
        WAIT;
        l_virhe=1;
        return(-1);
    }
    rtb=f<0.0? (dx=x2-x1,x1) : (dx=x1-x2,x2);
    for (i=0; i<100; ++i)
    {
        dx/=2;
        xmid=rtb+dx;
        strcpy(fct,fct0);
        arvo[i_x]=xmid;
        laske(fct,&fmid);
        if (fmid<=0.0) rtb=xmid;
        if (fabs(dx)<acc || fmid==0.0)
        {
            *y=rtb;
            return(1);
        }
    }
    sur_print("\nRoot not found!");
    WAIT;
    l_virhe=1;
    return(-1);
}


double probit(double z)
{
    double z1,z2,f;

    z1=z;
    if (z>0.5) z1=1-z;
    z2=sqrt(log(1.0/(z1*z1)));
    f=1.0+z2*(1.432788+z2*(0.189269+z2*0.001308));
    f=z2-(2.515517+z2*(0.802853+z2*0.010328))/f;
    if (z<=0.5) f=-f;
    return(f);
}

double sur_round(double x) /* 8.9.1998 */
{
    long l;
    double a;

    l=x;
    a=l;
    if (x>0.0 && x-a>0.5) ++a;
    else if (x<0.0 && a-x>0.5) --a;
    return(a);
}


double fact(double x)
{
    double a;
    int i,n;

    n=(int)x;
    a=1.0;
    for (i=2; i<=n; ++i) a*=i;
    return(a);
}

/* 21.10.1998 
double lfact(double x) 
{
    double a,di;

    a=0.0;
    for (di=2.0; di<=x; ++di) a+=log(di);
    return(a);
}
*/

double lfact(double x) /* 7.9.2007 */
    {
    int n,i;
    double s;

    n=(int)x;
    if (n<1) return(MISSING8);
    s=0.0;
    for (i=2; i<=n; ++i) s+=log((double)i);
    return(s);
    }


int nfact(unsigned long *pluku,unsigned long factor)
{
    int n=0;

    while (*pluku%factor==0)
    {
        ++n;
        *pluku/=factor;
    }
    return(n);
}

double nfactors(double d)
{
    unsigned long luku,factor,maxfactor;
    int i,k;
    /*
    	int is;
            int jatko=0;
    */
    double freq;

    if (d>4294967295.0)
    {
        sur_print("\nMax. permitted integer 4294967295=2^32-1");
        WAIT;
        return(0.0);
    }
    if (d<0.0) return(1.0);
    if (d<2.0) return(1.0);

    luku=(unsigned long)d;

    freq=1.0;
    k=0;
    i=nfact(&luku,2L);
    if (i)
    {
        /* printf("\ni2=%i",i); getck(); */
        freq*=(double)(i+1);
    }
    factor=3L;
    maxfactor=(unsigned long)sqrt((double)(luku));
    while (maxfactor>=factor)
    {
        i=nfact(&luku,factor);
        if (i)
        {
            /* printf("\ni=%i factor=%ld",i,factor); getck(); */
            freq*=(double)(i+1);
            maxfactor=(unsigned long)sqrt((double)(luku));
        }
        factor+=2L;
    }
    if (luku>1L) freq*=2L;
    return(freq);
}

#define N_ZETA 19
double zeta(double x)
{
    int n,i;
    double s,t,u;
    double zc[N_ZETA+1];

    n=N_ZETA;
    s=t=u=1.0;
    for (i=0; i<=n; ++i)
    {
        zc[i]=s;
        /* printf("\ni=%d zc=%g|",i,s); getck(); */
        t*=(n-i)*(n+i)*4;
        u*=(i+i+1)*(i+i+2);
        s+=t/u;
    }
    s=0.0;
    for (i=n-1; i>=0; --i)
        s=(zc[n]-zc[i])/pow((double)(i+1),x)-s;
    return(s/zc[n]/(1.0-2.0/pow(2.0,x)));
}



int f_edit(char *s,double *x,int n,double *py)
{
    int i,k,len;
    char lauseke[LLENGTH];
    char xx[LLENGTH], *osa[MAXARG];
    char sana[7];     /*  EARG 1 2 3 4 EARG EOS */
    double y;
    int remember_this;
    char *p,*q;
    int h;

    remember_this=0;
    len=strlen(s);
    s[len++]='(';
    i=0;
    while (i<spn && ((spp[i]!=':' && spp[i]!='|') || strncmp(s,spa[i],(unsigned int)len)!=0)) ++i;
    if (i==spn)
    {
        s[len-1]=EOS;
        return(-1);
    }
    /*
    printf("spa=%s spp=%c spb=%s\n",spa[i],spp[i],spb[i]); getch();
    */
    /*      if (!earg_varattu) { k=varaa_earg(); if (k<0) return(-1); } */

    if (spp[i]=='|')
    {
        remember_this=1;
        if (remember)
        {
            for (k=0; k<n_remember; ++k)
            {
                p=remember_space+k*remember_width;
                for (h=0; h<n; ++h)
                {
                    if (*(int *)p!=(int)x[h]) break;
                    p+=sizeof(int);
                }
                if (h==n)
                {
                    *py=*(double *)p;
                    /*
                    printf("remembered: %g %g %g\n",x[0],x[1],*py); getch();
                    */
                    return(1);
                }
            }
        }
        else
        {
            k=spfind("REMEMBER");
            if (k>=0) remember=atoi(spb[k]);
            else remember=10000;
            /* olettaa, että argumentteja ei enempää kuin 1. funktiossa!
               korjaus?
            */
            remember_width=n*sizeof(int)+sizeof(double);
            remember_space=malloc((unsigned int)(remember*remember_width));
            if (remember_space==NULL)
            {
                sur_print("REMEMBER space too large!");
                WAIT;
                return(-1);
            }
            n_remember=0;
            /*
            printf("remember=%d\n",remember); getch();
            */
        }
    }
    strcpy(lauseke,spb[i]);
    strcpy(xx,spa[i]);
    i=split(xx+len,osa,MAXARG);
    if (i!=n)
    {
        sprintf(sbuf,"\nArgument error in function %s",s);
        sur_print(sbuf);
        l_virhe=1;
        WAIT;
        return(-1);
    }
    osa[n-1][strlen(osa[n-1])-2]=EOS;   /* ): poistetaan */
    /*
        for (i=0; i<n; ++i) printf("\nosa %d: %s",i+1,osa[i]); getch();
    */
    for (i=0; i<n; ++i)
    {
        k=aseta_earg(x[i],sana);
        if (k<0) return(-1);
        korvaa(lauseke,osa[i],sana);
    }
    /* printf("x[0]=%g x[1]=%g\n",x[0],x[1]); getch();  */
    laske(lauseke,&y);
    /* printf(" y=%g\n",y); getch(); */
    *py=y;
    n_earg-=n;
    if (remember_this)
    {
        if (n_remember>=remember)
        {
            sprintf(sbuf,"REMEMBER=%d space exceeded!",remember);
            WAIT;
            free(remember_space);
            remember=0;
            return(-1);
        }
        p=remember_space+n_remember*remember_width;
        for (i=0; i<n; ++i)
        {
            len=x[i];
            q=(char *)&len;
            for (k=0; k<sizeof(int); ++k) *p++=*q++;
        }
        q=(char *)&y;
        for (k=0; k<sizeof(double); ++k) *p++=*q++;
        ++n_remember;
        /*
        printf("n_remember=%d %g %g %g\n",n_remember,x[0],x[1],*py); getch();
        */
    }
    return(1);
}

double uniform(double x)
{
    time_t ltime;
    time_t *pi;
    static int next=0;

    if (x==0.0 && next==0)
    {
        time(&ltime);
        pi=&ltime;
        srand((unsigned int)(*pi+*psur_seed));
        rand();
        *psur_seed=rand();

        *psur_seed+=17;
        next=1;
    }
    else
    {
        if (next) return((double)(RND+1e-6));
        if (x!=0.0)
        {
            srand((unsigned int)(x));
            rand();
        }
        next=1;
    }
    return((double)(RND+1e-6));
}

/* 14.9.94
double sur_rand0(double x)
{
    return(uniform(x)); 
}
*/


#define NMAT 5
static double *mat[NMAT];
static char *rlab[NMAT],*clab[NMAT];
static int lr[NMAT],lc[NMAT];
static int m[NMAT],n[NMAT];
int nmat=0;
static char mat_name[NMAT][9];

int lab_find(char *x, char *lab, int m, int len)
        {
        char s[32];
        int i;

        strcpy(s,x);
        for (i=strlen(s); i<len; ++i) s[i]=' ';
        for (i=0; i<m; ++i)
            if (strncmp(s,lab+i*len,(unsigned int)len)==0) break;
        if (i==m) return(-1);
        return(i+1);
        }

int dat_function(char *f, char **s, int nn, double *yy)
        {
/*
        extern char *str_opnd[];

 printf("\nf=%s s[0]=%s s[1]=%s nn=%d|",f,s[0],s[1],nn);
 WAIT;
*/
        if (nn!=2)
            {
            sur_print("\nError in DAT_ function!");
            WAIT; l_virhe=1;
            return(-1);
            }
        str_opnd[2]=s[1]; str_opnd[1]=s[0]; str_opnd[0]=f;
/*
        f_tiedosto("DAT_",NULL,3,yy);
*/

        return(1);
        }


void mat_function(char *f, char **s, int nn, double *yy)
        {
        int i,j,k;
        double xx[2];
/*        char *lab;  */

/* printf("f=%s nn=%d %s %s\n",f,nn,s[0],s[1]); getch(); */

        for (k=0; k<nmat; ++k)
            {
            if (strcmp(f,mat_name[k])==0) break;
            }
        if (nmat==0 || k==nmat)
            {

            if (nmat==NMAT) nmat=0; /* kiertokulku */
/*
                {
                sprintf(sbuf,"Too many matrices (more than %d)!",NMAT);
                sur_print(sbuf); WAIT; l_virhe=1; nmat=0; return;
                }
*/
   mat_load(f,&mat[k],&m[k],&n[k],&rlab[k],&clab[k],&lr[k],&lc[k]);

            strcpy(mat_name[k],f);
            ++nmat;

            }
        if (nn==1 && m[k]==1) { nn=2; s[1]=s[0]; s[0]="1"; }
        i=lab_find(s[0],rlab[k],m[k],lr[k]);
        if (i>0) xx[0]=i;
        else
            {
            laske(s[0],&xx[0]);
            sprintf(sbuf,"%g",xx[0]);    /* 9.9.1999 */
            i=lab_find(sbuf,rlab[k],m[k],lr[k]);
            if (i>0) xx[0]=i;
            }
        if (nn>1)
            {
            i=lab_find(s[1],clab[k],n[k],lc[k]);
            if (i>0) xx[1]=i;
            else
                {
                laske(s[1],&xx[1]);
                sprintf(sbuf,"%g",xx[1]);    /* 9.9.1999 */
                i=lab_find(sbuf,clab[k],n[k],lc[k]);
                if (i>0) xx[1]=i;
                }
            }

        i=xx[0]; if (nn>1) j=xx[1];
        if (i<1 || i>m[k] || (nn>1 && (j<1 || j>n[k])) )
            {
            sur_print("\nError in matrix index!"); WAIT;
            l_virhe=1;
            return;
            }
        if (nn==1)
            *yy=mat[k][i-1];
        else
            *yy=mat[k][i-1+m[k]*(j-1)];
        }


double funktio(char *s, double x)
{
    char S[32];

    int i;
    double y;
    double xx;


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
    strncpy(S,s,31);
    S[31]=EOS;
    muste_strupr(S);

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
    {
        if (x>0.0) return(1.0);
        else return(0.0);
    }


    if (strcmp(S,"RND")==0) return(uniform(x));
    if (strcmp(S,"RAND")==0) return(uniform(x));
/* Tämä kutsui vain uniformia
    if (strcmp(S,"RAND")==0) return(sur_rand0(x));
*/

    if (strcmp(S,"PROBIT")==0) return(probit(x));
    if (strcmp(S,"ROUND")==0) return(sur_round(x));
    if (strcmp(S,"FACT")==0) return(fact(x));
    if (strcmp(S,"LFACT")==0 || strcmp(S,"FACT.L")==0)
        return(lfact(x));
    if (strcmp(S,"NFACTORS")==0) return(nfactors(x));
    if (strcmp(S,"ZETA")==0) return(zeta(x));

    if (strcmp(S,"LGAMMA")==0) return(lgammafn(x));



    
            if (*s=='M' && strncmp(s,"MAT_",4)==0)
                {
                mat_function(s+4,str_opnd,1,&y);
                return(y);
                }
    

    xx=x;
    i=f_edit(s,&xx,1,&y);
    if (i>0) return(y);  	/* Temporary functions */
    /*        i=f_tiedosto(s,&xx,1,&y);
            if (i>0 && y!=MISSING8) return(y);

            set_console_title();  */



    l_virhe=1;
    return(x);
}


int arg_virhe(char *s)
{
    sprintf(sbuf,"\n%s: Error in arguments",s);
    sur_print(sbuf);
    l_virhe=1;
    return(1);
}


double gcd(double a,double b) /*  Greatest Common Divisor */
{
    unsigned long u,v,w;

    u=fabs(a);
    v=fabs(b);
    if (u<v)
    {
        w=u;
        u=v;
        v=w;
    }
    while ((w=u%v)!=0)
    {
        u=v;
        v=w;
    }
    return((double)v);
}

double root(double dn,double x)
{
    int n;

    n=dn;
    if (x>0.0 || (double)n!=dn || n<0 || ((n>>1)<<1)==n) return(pow(x,1/dn));
    if (x==0.0) return(0.0);
    return(-pow(-x,1/dn));
}



double ed_number(double x1,double x2)
{
    int i,k;
    double tulos;
    static char rivi[LLENGTH],*s[EP4];  /* globaaleja ?!? */

    k=x1;
    if (k<1 || k>r2)
    {
        sur_print("\nIncorrect line # in X function!");
        WAIT;
        l_virhe=1;
        return(0.0);
    }
    edread(rivi,k);
    k=x2;
    i=splitp(rivi+1,s,EP4);
    if (i<k)
    {
        sur_print("\nIncorrect index in X function!");
        WAIT;
        l_virhe=1;
        return(0.0);
    }
    laske(s[k-1],&tulos);
    return(tulos);
}

int sur_julian(double d,double m,double y,double *pdate)
{
    double extra;

    extra=100.0*y+m-190002.5;
    *pdate=367.0*y-(int)(7.0*(y+(int)((m+9.0)/12.0))/4.0);
    *pdate+=(int)(275.0*m/9.0)+d+1721013.5;
    *pdate-=0.5*extra/fabs(extra);
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

    strncpy(S,s,31);
    S[31]=EOS;


    /* Statistical distribution functions from Rmath */

    if (strcmp(S,"bin.f")==0 || strcmp(S,"BIN.f")==0 || strcmp(S,"Bin.f")==0 )
    {
        return(dbinom(x[2],x[0],x[1],(int)0));
    }

    if (strcmp(S,"bin.F")==0 || strcmp(S,"BIN.F")==0 || strcmp(S,"Bin.F")==0 )
    {
        return(pbinom(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"bin.G")==0 || strcmp(S,"BIN.G")==0 || strcmp(S,"Bin.G")==0 )
    {
        return(qbinom(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"poisson.f")==0 || strcmp(S,"POISSON.f")==0 || strcmp(S,"Poisson.f")==0 )
    {
        return(dpois(x[1],x[0],(int)0));
    }

    if (strcmp(S,"poisson.F")==0 || strcmp(S,"POISSON.F")==0 || strcmp(S,"Poisson.F")==0 )
    {
        return(ppois(x[1],x[0],(int)1,(int)0));
    }

    if (strcmp(S,"poisson.G")==0 || strcmp(S,"POISSON.G")==0 || strcmp(S,"Poisson.G")==0 )
    {
        return(qpois(x[1],x[0],(int)1,(int)0));
    }

    if (strcmp(S,"N.f")==0 || strcmp(S,"n.f")==0 )
    {
        return(dnorm(x[2],x[0],x[1],(int)0));
    }

    if (strcmp(S,"N.F")==0 || strcmp(S,"n.F")==0 )
    {
        return(pnorm(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"N.G")==0 || strcmp(S,"n.G")==0 )
    {
        return(qnorm(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"t.f")==0 || strcmp(S,"T.f")==0 )
    {
        return(dt(x[1],x[0],(int)0));
    }

    if (strcmp(S,"t.F")==0 || strcmp(S,"T.F")==0 )
    {
        return(pt(x[1],x[0],(int)1,(int)0));
    }

    if (strcmp(S,"t.G")==0 || strcmp(S,"T.G")==0 )
    {
        return(qt(x[1],x[0],(int)1,(int)0));
    }

    if (strcmp(S,"chi2.f")==0 || strcmp(S,"CHI2.f")==0 || strcmp(S,"Chi2.f")==0 )
    {
        return(dchisq(x[1],x[0],(int)0));
    }

    if (strcmp(S,"chi2.F")==0 || strcmp(S,"CHI2.F")==0 || strcmp(S,"Chi2.F")==0 )
    {
        return(pchisq(x[1],x[0],(int)1,(int)0));
    }

    if (strcmp(S,"chi2.G")==0 || strcmp(S,"CHI2.G")==0 || strcmp(S,"Chi2.G")==0 )
    {
        return(qchisq(x[1],x[0],(int)1,(int)0));
    }

    if (strcmp(S,"F.f")==0 || strcmp(S,"f.f")==0 )
    {
        return(df(x[2],x[0],x[1],(int)0));
    }

    if (strcmp(S,"F.F")==0 || strcmp(S,"f.F")==0 )
    {
        return(pf(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"F.G")==0 || strcmp(S,"f.G")==0 )
    {
        return(qf(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"gamma.f")==0 || strcmp(S,"GAMMA.f")==0 || strcmp(S,"Gamma.f")==0 )
    {
        return(dgamma(x[2],x[0],x[1],(int)0));
    }

    if (strcmp(S,"gamma.F")==0 || strcmp(S,"GAMMA.F")==0 || strcmp(S,"Gamma.F")==0 )
    {
        return(pgamma(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"gamma.G")==0 || strcmp(S,"GAMMA.G")==0 || strcmp(S,"Gamma.G")==0 )
    {
        return(qgamma(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"beta.f")==0 || strcmp(S,"BETA.f")==0 || strcmp(S,"Beta.f")==0 )
    {
        return(dbeta(x[2],x[0],x[1],(int)0));
    }

    if (strcmp(S,"beta.F")==0 || strcmp(S,"BETA.F")==0 || strcmp(S,"Beta.F")==0 )
    {
        return(pbeta(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"beta.G")==0 || strcmp(S,"BETA.G")==0 || strcmp(S,"Beta.G")==0 )
    {
        return(qbeta(x[2],x[0],x[1],(int)1,(int)0));
    }

    if (strcmp(S,"weibull.f")==0 || strcmp(S,"WEIBULL.f")==0 || strcmp(S,"Weibull.f")==0 )
    {
        return(dweibull(x[2],x[1],1/x[0],(int)0));
    }

    if (strcmp(S,"weibull.F")==0 || strcmp(S,"WEIBULL.F")==0 || strcmp(S,"Weibull.F")==0 )
    {
        return(pweibull(x[2],x[1],1/x[0],(int)1,(int)0));
    }

    if (strcmp(S,"weibull.G")==0 || strcmp(S,"WEIBULL.G")==0 || strcmp(S,"Weibull.G")==0 )
    {
        return(qweibull(x[2],x[1],1/x[0],(int)1,(int)0));
    }

    if (strcmp(S,"exp.f")==0 || strcmp(S,"EXP.f")==0 || strcmp(S,"Exp.f")==0 )
    {
        return(dexp(x[1],1/x[0],(int)0));
    }

    if (strcmp(S,"exp.F")==0 || strcmp(S,"EXP.F")==0 || strcmp(S,"Exp.F")==0 )
    {
        return(pexp(x[1],1/x[0],(int)1,(int)0));
    }

    if (strcmp(S,"exp.G")==0 || strcmp(S,"EXP.G")==0 || strcmp(S,"Exp.G")==0 )
    {
        return(qexp(x[1],1/x[0],(int)1,(int)0));
    }

    muste_strupr(S);  /* No more case sensitive function names */

    /* R-style normal density */
    if (strcmp(S,"DNORM")==0)
    {
        if (n>3) return(dnorm(x[0],x[1],x[2],(int)x[3]));
        return(dnorm(x[0],x[1],x[2],(int)0));
    }


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
        y=x[0];
        k=0;
        for (i=1; i<n; ++i) if (x[i]>y)
            {
                y=x[i];
                k=i;
            }
        return((double)(k+1));
    }
    if (strcmp(S,"MINN")==0)
    {
        y=x[0];
        k=0;
        for (i=1; i<n; ++i) if (x[i]<y)
            {
                y=x[i];
                k=i;
            }
        return((double)(k+1));
    }
    if (strcmp(s,"C")==0)
    {
        double u,v;
        int iu,iv;

        if (n!=2)
        {
            arg_virhe(s);
        }
        iv=v=x[0];
        iu=u=x[1];
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

        h=0;
        if (*S=='L') h=1;
        if (n!=2)
        {
            arg_virhe(s);
        }
        v=x[0];
        u=x[1];
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


    if (strcmp(S,"GCD")==0)
    {
        return (gcd(x[0],x[1]));
    }
    if (strcmp(S,"MOD")==0)
    {
        return((double)((unsigned long)x[0]%(unsigned long)x[1]));
    }
    if (strcmp(S,"ROOT")==0)
    {
        return (root(x[0],x[1]));
    }
    if (strcmp(S,"ROUND")==0)
    {
        y=pow(10.0,x[1]);
        return(sur_round(x[0]*y)/y);
    }

    if (strcmp(S,"X")==0)
    {
        extern double ed_number();
        return (ed_number(x[0],x[1]));
    }

    /* 14.8.2005 days from 1.1.2000 */
    if (strcmp(S,"DAYS")==0)
    {
        double date;
        sur_julian(x[0],x[1],x[2],&date);
        return(date-2451544.0);
    }

    
            if (*s=='M' && strncmp(s,"MAT_",4)==0)
                {
                mat_function(s+4,str_opnd,n,&y);
                return(y);
                }


            if (*s=='D' && strncmp(s,"DAT_",4)==0) /* 5.12.2002 */
                {
                dat_function(s+4,str_opnd,n,&y);
                return(y);
                }
    
    i=f_edit(s,x,n,&y);
    if (i>0) return(y);
    /*        i=f_tiedosto(s,x,n,&y);
            set_console_title();
            if (i>0 && y!=MISSING8) return(y);
    */

    l_virhe=1;
    return(x[0]);
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
    l_virhe=0;
    errno=0;
    tarkkuus=0;
    /*        nmat=0; */ /* 8.5.1999 */
    i=spec_init_arit(r1+r-1);
    if (i<0) return(-1);  /* 14.1.92 */

    i=varaa_earg();
    if (i<0) return(-1);

    if (spn)
    {
        i=spfind("ACCURACY");
        if (i>=0)
        {
            laske("ACCURACY",&tulos);
            tarkkuus=(int)tulos;

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

    if (lauseke[i-1]=='.')
    {
        lauseke[--i]=EOS;
        monia=1;
    }
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
        if (muste_strcmpi(osa[0],"VAR")==0 || muste_strcmpi(osa[0],"MAT")==0
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
            WAIT;
            return(-1);
        }

        /*    kirjoita2("error",r1+r-1,c1+c-1);   poistettu 30.11.2008 */
        if (etu==2)
        {
            strcpy(tut_info,"þþþ@12@MATH@Error in editorial computing!@");
            return(-1);
        }
        if (remember)
        {
            remember=0;
            free(remember_space);
        }
        WAIT;
        return(1);
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
            if (i<0 || l_virhe || errno)
            {
                WAIT;
                return(-1);
            }
            kirjoita(tulos,(int)(spplace[k]/ed1+1),(int)spplace[k]%ed1+1);
        }
        /*          edisp=1;     */
    }
    if (remember)
    {
        remember=0;
        free(remember_space);
    }
    free(earg);
    free(spplace);
    free(spp);
    free(arvo);
    free(spshad);
    free(spb);
    free(spa);
    free(splist);
    return(1);
}
