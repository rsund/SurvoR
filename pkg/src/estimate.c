/* !EST.C 16.7.85/SM (2.8.1987) (5.12.1993) (7.6.1997)

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
// #include <conio.h>
#include <math.h>
//#include <malloc.h>
#include <time.h>
// #include <process.h> ???
// #include "survo.h"
#include "estimate.h"
#include "survoext.h"
#include "survolib.h"

#define TEST OK
#define NVAR 50
#define DATASTEP 10000

#define N MAXA



/********************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "LOGDENSITY", "METHOD", "WEIGHT", "UPDATE",
                 "CRITERION", "ACCURACY", "FSPACE", "STEP",
                 "RESULTS", "PRIND", "!" };
char **specs=specs0;
***********************/
static int eol,eoutind;

/* specifications in the edit field */
/*
extern int spn;
extern char *splist;
extern char **spa, **spb;
*/
extern int spn;

extern int ed2;     /* lines      */
extern int ed1;     /* columns    */
extern int r,r1,r2,r3,c1,c2,c3;
extern char *z;

extern char *edisk, *esysd, *eout;
extern char sbuf[];

/* data table/file */
static double *xx;    /* datat */  // 18.6.2011 oli xx[]
static int em,em2,en; // l1,l2,edat;
static int obs1,obs2;
static int prind=0;

/* parsed expressions */
static int nlaus=0;
static char *laji=NULL;
static int *ind;
static int *lag;

static SURVO_DATA sdata;

static char sbuf2[DLENGTH];

static double a[MAXA]; char *aname[MAXA]; int na; /* model parameters */
static double s_a[MAXA]; // std_errors of a
static double cc[MAXC]; char *cname[MAXC]; int nc; /* model constants */
static char aclist[MAXAC]; char *acl;
static int lause, laus[3+1+MAXA+(MAXA+1)*MAXA/2];
static char moddef[MODLGTH], mod1[MLENGTH],  mod2[MLENGTH],  dmod[DLENGTH];
static char                  mmod1[MLENGTH], mmod2[MLENGTH], mmod[MLENGTH];
static char                  cmod1[MLENGTH], cmod2[MLENGTH], cmod[DLENGTH];
static int linind;
static char d1mod[DLENGTH];
static int weightind;
static char modw[MLENGTH], mmodw[MLENGTH], cmodw[MLENGTH];
static char malli[LLENGTH];
static double criterion=2;
static int logd=0; /* =1 logdensity */
static int obs1,obs2;
static int est_disp_all=0;
static int eaccuracy=0;

static int n_laus=0;

static int saveind; char mods[MLENGTH], mmods[MLENGTH], cmods[MLENGTH];
static int save_var[MAXSAVE];  // ESTIMATE internal
static int save_var2[MAXSAVE]; // # in data

static double HH[N*(N+1)];
static double weightsum;
static int weightind;

static int *vv;


static char *argv1;

static int modread(int j);
static int modread2();
static int modread3(char *s);
static int modread4(char *s);
static int der1(int nd);
static int check(char s[]);
static int tutki(char sana[]);
static int varnro(char sana[]);
static int syntax(char s[]);
static int muunna(char d[],char s[]);
static int tutki2(char sana[],char d[]);
static int jatka(char s[],char m);
static int luku(char s[]);
static int acmerk(char s[]);
static int fcmuunna(char d[],char s[]);
static int tutki3(char sana[],char d[]);
static int funktio(char sana[]);
static int viive(char s[],int *ph);
static int eoutput(char line[]);
static int corrnorm(double *H,int m);
static int datopen(char data[]);
static int datain();
static int not_enough_memory();
static int muunnos(char *s,int alku,char *sanoma);
static int pinoa(int u);
static int siirto(int k);
static int indeksi(char *s);
static int virhe(char *sanoma);
extern double sur_st_norm(); // 19.2.2002;
double earvo(int j,int alku,double a[],double c[],double *xx);
static double power(double x,double y);
static double est_round(double x) /* 8.9.1998 */;
static int hooke();
static double rss(double a[]);
static int rss0();
static double wsum();
static int hoojee(double x[],int dim,double (*f)(),double step[]);
static int numhess(double a[],double H[],int m,double step[]);
static int emove(double b1[],double b2[],double (*f)(),double step[]);
static int pmove(double b1[],double b2[],double p1[],double (*f)(),double step[]);
static int vcopy(double x[],double y[]);
static int stopdisp(char rivi[]);
static int deri(char d[],char s[],char x[]);
static int korvaa(char *s,char *x,char *m);
static char *deriv(char *d,char *s);
static char *summa(char *r,char *s1,char *s2);
static char *erotus(char *r,char *s1,char *s2);
static char *tulo(char *r,char *s1,char *s2);
static char *suhde(char *r,char *s1,char *s2);
static char *nelio(char *s);
static char *potenssi(char *r,char *s1,char *s2);
static int sulutp(char *s)  /* poistaa tarpeettomat sulut */;
static int liikaa(char *s);
static int hajoita(char *s);
static char *sul(char *s,int u);
static int suluin(char *s);
static int siir(char *s,int k);
static int staso(char *s);
static int vakio(char *s);
static int nolla(char *s);
static int yksi(char *s);
static int myksi(char *s);
static int kok(char *s)    /* onko s kokonaisluku? */;
static int hess(double a[],double H[],int m);
static int grad(double g[],double a[]);
static int cholinv(double a[],int n);
static double Rsquare();
static int save_update1(int k);
static int davidon(int nd);
static char *spois(char *s);
static int dfp(double x[],int m,double (*f)(),int (*grad)(double *,double *),double step,int maxnf,int *stop);
static int newton(int linind,int type);
static int corr_save(double *R,double *a,double *s,int m,char *names[]);
static int newt(double x[],int m,double (*f)(),int (*grad)(double *,double *),double step,int maxnf,int linind,int *stop,int type);
static int modified_inv_hess(double *HH,int m);
static int residuals(char *data);
static int op_der();
static int grid_search();
static int a_virhe(char *s);
static int save_matrices(char *aname[],int na,double *a,double *s,int n,double rss,double r2);
static int n_strcat(char *x,int len,char *s);
extern int matrix_save0(char *matr,double *A,int m,int n,char *rlab,char *clab,int mrl,int mcl,int type,char *expr,int nrem,int remrivi,char *ptext);

/*******************
void main(argc,argv)
int argc; char *argv[];
***********************/
void muste_estimate(char *argv)
        {
        int cline,i,j,avaus;
        char *p;
        char sanoma[LLENGTH];
        char method;
//      char name[4], index[3];   18.6.2011

        s_init(argv);
        argv1=argv;
        if (muste_strcmpi(word[0],"DER")==0) { op_der(); s_end(argv[1]); return; }

        cline=r+r1-1;
        if (g<3) {
                   sur_print("\nIncomplete ESTIMATE operation!");
                   sur_print("\nSee ESTIMATE?                 ");
                   WAIT; return;
                 }
        if (g==3) eol=0; else {
                              eol=edline2(word[3],1);
                              if (eol<0) return;
                              }

        spn=sp_init(cline);
        if (spn<0) {
                  sur_print("\nToo many specifications in the edit field!\n");
                     WAIT; return; }
        i=spfind("DISPLAY"); if (i>=0) est_disp_all=1;

        i=spfind("RESULTS"); if (i>=0) results=atoi(spb[i]); // 16.5.2004

        strcpy(malli,word[2]);
        j=wfind("MODEL",word[2],1);
        if (j<0) {
                   sprintf(sbuf2,"\nModel '%s' not found in the edit field!",word[2]);
                   sur_print(sbuf2); WAIT; return;
                 }
        i=modread(j+1); if (i<0) return;
        p=strchr(moddef,'=');
        if (p==NULL) {
                       sprintf(sbuf2,"\n= missing from model definition!");
                       sur_print(sbuf2); WAIT; return;
                     }
        *p=EOS; strcpy(mod1,moddef); strcpy(mod2,p+1);

        if (muste_strcmpi(mod1,"LOGDENSITY")==0) logd=1;

        i=spfind("WEIGHT");
        if (i<0) { weightind=0; strcpy(modw,"1"); }
        else     { weightind=1; strcpy(modw,spb[i]); }

        i=modread4(modw); if (i<0) return; // 10.5.2004

    if (est_disp_all)
        {
    sprintf(sbuf2,"mod1=%s\n",mod1); sur_print(sbuf2);
    sprintf(sbuf2,"mod2=%s\n",mod2); sur_print(sbuf2);
    sprintf(sbuf2,"modw=%s\n",modw); sur_print(sbuf2);
        }

        avaus=datopen(word[1]);
        if (avaus<=0) return;
    if (est_disp_all)
        {
    for (i=0; i<em; ++i) { sprintf(sbuf2,"%s  ",sdata.varname[i]);
        sur_print(sbuf2); } sur_print("\n");
        }
        em2=0; if (!logd) { i=check(mod1); if (i<0) return; }
               i=check(mod2); if (i<0) return;
               i=check(modw); if (i<0) return;
    if (est_disp_all)
        {
    for (i=0; i<em2; ++i) { sprintf(sbuf2,"%d  ",sdata.v[i]); sur_print(sbuf2); } sur_print("\n");
        }
        na=0;
        nc=1; cc[0]=0; aclist[0]='0'; aclist[1]=EOS; cname[0]=aclist;
        acl=aclist+2;  /* param. ja vakioiden nimien merkint‰ */
        obs1=obs2=0;  /* aluksi max lag ja lead */
        if (!logd) { i=muunna(mmod1,mod1); if (i<0) return; }
        i=muunna(mmod2,mod2); if (i<0) return;
        i=muunna(mmodw,modw); if (i<0) return;
        ++obs1; obs2=en-obs2;  /* nyt ens. ja viim. havainto */
    if (est_disp_all)
        {
    sprintf(sbuf2,"obs1=%d obs2=%d\n",obs1,obs2); sur_print(sbuf2);
    if (!logd) { sprintf(sbuf2,"mmod1=%s\n",mmod1); sur_print(sbuf2); }
    sprintf(sbuf2,"mmod2=%s\n",mmod2); sur_print(sbuf2);
    sprintf(sbuf2,"mmodw=%s\n",mmodw); sur_print(sbuf2);
    sur_print("Parameters:\n");
    for (i=0; i<na; ++i) { sprintf(sbuf2,"%s  %f\n",aname[i],a[i]); sur_print(sbuf2); }
    sur_print("Constants:\n");
    for (i=0; i<nc; ++i) { sprintf(sbuf2,"%s  %f\n",cname[i],cc[i]); sur_print(sbuf2); }
        }

        if (!logd) { i=fcmuunna(cmod1,mmod1); if (i<0) return; }
        i=fcmuunna(cmod2,mmod2); if (i<0) return;
        i=fcmuunna(cmodw,mmodw); if (i<0) return;
    if (est_disp_all)
        {
    if (!logd) { sprintf(sbuf2,"cmod1=%s\n",cmod1); sur_print(sbuf2); }
    sprintf(sbuf2,"cmod2=%s\n",cmod2); sur_print(sbuf2);
    sprintf(sbuf2,"cmodw=%s\n",cmodw); sur_print(sbuf2);
    sur_print("Constants:\n");
    for (i=0; i<nc; ++i) { sprintf(sbuf2,"%s  %f\n",cname[i],cc[i]); sur_print(sbuf2); }
        }

        laus[0]=0; j=0;
    if (!logd)
        {
        j=muunnos(cmod1,laus[0],sanoma);
        if (strcmp(sanoma,"OK")!=0) { sprintf(sbuf2,"\n%s",sanoma); sur_print(sbuf2);
                                      WAIT; return; }
        }

        laus[1]=j;
        j=muunnos(cmod2,laus[1],sanoma);
        if (strcmp(sanoma,"OK")!=0) { sprintf(sbuf2,"\n%s",sanoma); sur_print(sbuf2);
                                      WAIT; return; }
        laus[2]=j;
        j=muunnos(cmodw,laus[2],sanoma);
        if (strcmp(sanoma,"OK")!=0) { sprintf(sbuf2,"\n%s",sanoma); sur_print(sbuf2);
                                      WAIT; return; }
        n_laus=3;

        saveind=0;   // SAVE -> UPDATE 10.5.2004
        i=spfind("UPDATE"); // 8.5.2004
        if (i>=0)
            {
            char x[LLENGTH],*s[MAXSAVE];
            int k;
            char sk[32];

            strcpy(x,spb[i]);
            saveind=split(x,s,MAXSAVE);
            for (k=0; k<saveind; ++k)
                {
                *sbuf2=EOS;
                strcpy(sk,s[k]);
                tutki2(sk,sbuf2); // sk tyhjenee!
                if (*sbuf2=='x')
                    {
                    save_var[k]=atoi(sbuf2+1);
                    sprintf(sbuf2,"{%s}",s[k]);
                    i=spfind(sbuf2);
                    if (i<0)
                        {
                        sprintf(sbuf2,"\nExpression {%s}=... for UPDATE variable %s not found!",
                                         s[k],s[k]);
                        sur_print(sbuf2); WAIT; return;
                        }

                    save_var2[k]=varfind(&sdata,s[k]); // lopull.tall.varten
                    if (save_var2[k]<0) return;

                    strcpy(mods,spb[i]);
                    i=modread4(mods); if (i<0) return; // 10.5.2004
    // printf("\ntalletettava: %s|",spb[i]); getch();
                    i=muunna(mmods,mods); if (i<0) return;
                    i=fcmuunna(cmods,mmods); if (i<0) return;
                    laus[n_laus]=j;
                    j=muunnos(cmods,laus[n_laus],sanoma);
                    if (strcmp(sanoma,"OK")!=0) { sprintf(sbuf2,"\n%s",sanoma); sur_print(sbuf2);
                                                  WAIT; return; }
                    ++n_laus;
                    }
                } // k
            }

        lause=j;  /* seur. lausekkeita varten */

        i=datain(); if (i<0) return;

        i=spfind("METHOD"); if (i>=0) method=*spb[i]; else method='-';
        i=spfind("CRITERION");
        if (i>=0)
            {
            if (strcmp(spb[i],"ABS")==0 || strcmp(spb[i],"MAD")==0)
                criterion=1;
            if (spb[i][0]=='L') criterion=atof(spb[i]+1);
            }
        if (criterion!=2) method='H';

        eaccuracy=0; // 22.10.2001
        i=spfind("ACCURACY");
        if (i>=0) eaccuracy=atoi(spb[i]);

        switch (method)
            {
        case '-':
                  i=der1(2); if (i<0) { sur_print("case -:\n"); sur_getch(); }
                  if (linind==1)
                          i=newton(1,1);
                  else    i=davidon(1);
                  if (i<0) return;
                  break;
        case 'H':                             /*Hooke-Jeeves */
                  i=hooke(); if (i<0) return;
                  break;
        case 'D':                             /* Davidon-Fletcher-Powell */
                  i=der1(2); if (i<0) return;
                  i=davidon(2); if (i<0) return;
                  break;
        case 'd':                             /* Davidon-Fletcher-Powell */
                  i=der1(1); if (i<0) return;
                  i=davidon(1); if (i<0) return;
                  break;
        case 'N':                             /* Newton-Raphson */
                  i=der1(2); if (i<0) return;
                  i=newton(linind,1); if (i<0) return;
                  break;
        case 'M':                             /* Modified Newton */
                  i=der1(2); if (i<0) return;
                  i=newton(linind,2); if (i<0) return;
                  break;
        case 'G':
                  i=grid_search(); if (i<0) return;
                  break;




        default:  sprintf(sbuf2,"\nMETHOD=%c not available!",method); sur_print(sbuf2);
                  return;
            }
    residuals(word[1]);
    data_close(&sdata);
/***********************
    if (kbhit())
        { sur_print("\n Ready! Press any key!\n"); getch(); getch(); }
**********************/
        s_end(argv);
        }

// TILAPƒISESTI:

double sur_st_norm(double x,double y)
    {
    return(0.0);
    }
double sur_lg_gamma(double x)
    {
    return(0.0);
    }

static int modread(int j)
        {
        int kesken=1;
        char x[LLENGTH];
        int i,len;
        char *p;

        *moddef=EOS;
        while (kesken)
            {
            edread(x,j); ++j;
            i=1; while (x[i]==' ') ++i;
            p=strchr(x+i,'&');
            if (p==NULL)
                {
                kesken=0;
                len=strlen(x);
                while (x[len-1]==' ') x[--len]=EOS;
                }
            else { --p; while (*p==' ') --p;
                   *(p+1)=EOS;
                 }
            if (strlen(moddef)+strlen(x+i)>MODLGTH-1)
                {
                  sprintf(sbuf2,"\nModel too long (more than %d characters)",MODLGTH);
                  sur_print(sbuf2); WAIT; return(-1);
                }
            strcat(moddef,x+i);
            }
        i=modread2(); /* 4.9.1998 */
        return(i);
        }

static int modread2()
        {
        char *p;
        int i;

        p=strchr(moddef,'{'); if (p==NULL) return(1);
        i=1;
        while (i==1)
            {
            i=modread3(moddef);
            if (i<0) return(-1);
            }
        return(1);
        }

static int modread3(char *s)
        {
        char *p,*q,*x;
        int i;

        x=dmod;
        strcpy(x,s);
        p=strchr(x,'{'); if (p==NULL) return(0);

// 9.5.2004
        q=strchr(p,'{');
        if (q!=NULL) { i=modread4(s); return(i); }

        q=strchr(p,'}');
        if (q==NULL)
            {
            sprintf(sbuf2,"\n} missing in %s!",x); sur_print(sbuf2);
            WAIT; return(-1);
            }
        *p=EOS; *q=EOS;
        strcpy(s,x);
        strcpy(cmod,"{"); strcat(cmod,p+1); strcat(cmod,"}");
        i=spfind(cmod);
        if (i<0)
            {
            sprintf(sbuf2,"\n%s not found!",p+1); sur_print(sbuf2);
            WAIT; return(-1);
            }
        strcat(s,spb[i]);
        strcat(s,q+1);
        return(1);
        }

static int modread4(char *s)
    {
    char t[DLENGTH];
    char *p,*q;
    int i;

// printf("\n%s|",s); getch();
    p=strchr(s,'{');
    if (p==NULL) return(1);
    q=p+1;
    while (*q!=EOS)
        {
        if (*q=='}') // korvaa v‰li (p,q)
            {
            *t=EOS;
            strncat(t,s,(int)(p-s));
            *sbuf2=EOS;
            strncat(sbuf2,p,(int)(q-p)+1);
            i=spfind(sbuf2);
            if (i<0)
                {
                sprintf(t,"\n%s not found!",sbuf2); sur_print(t);
                WAIT; return(-1);
                }
            strcat(t,spb[i]);
            strcat(t,q+1);
            strcpy(s,t);
            i=modread4(s);
            return(i);
            }
        if (*q=='{') { strcpy(s,t); i=modread4(s); if (i<0) return(-1); }
        ++q;
        }
sur_print("\nError in {} expression! } missing?");
WAIT; return(-1);

    }

/* EST2.C 16.7.85/SM (10.10.1986,24.3.1989) (7.6.1997) (8.9.1998)
*/

static int der1(int nd)
/* nd=1: Vain 1. derivaatat   nd=2: Myˆs 2. derivaatat */
        {
        int i,j,k;  // l;  18.6.2011
        char sanoma[LLENGTH];
        char name[5], index[5];

        if (!logd)
            {
            linind=1;  /* model linear */
            strcpy(mmod,mmod1); strcat(mmod,"-(");
            strcat(mmod,mmod2); strcat(mmod,")");
            }
        else { linind=0; strcpy(mmod,mmod2); }
        for (i=0; i<na; ++i)
             {
             strcpy(name,"a"); strcat(name,muste_itoa(i,index,10));
             if (est_disp_all)
                 { sprintf(sbuf2,"mmod=%s\n",mmod); sur_print(sbuf2); }
             deri(dmod,mmod,name);
             if (est_disp_all)
                 { sprintf(sbuf2,"dmod=%s\n",dmod); sur_print(sbuf2); }

             fcmuunna(cmod,dmod);
             if (est_disp_all)
                 { sprintf(sbuf2,"cmod=%s\n",cmod); sur_print(sbuf2); }
//           laus[i+3]=lause;
             laus[i+n_laus]=lause;
             lause=muunnos(cmod,lause,sanoma);
             if (strcmp(sanoma,"OK")!=0) { sprintf(sbuf2,"\n%s",sanoma);
                                           sur_print(sbuf2); WAIT; return(-1); }
             }

             if (nd==1) return(1);

             if (est_disp_all)
                 sur_print("Second derivatives:\n");
//           k=3+na;
             k=n_laus+na;
             for (i=0; i<na; ++i)
              {
              strcpy(name,"a"); strcat(name,muste_itoa(i,index,10));
              deri(d1mod,mmod,name);
              for (j=0; j<=i; ++j)
                {
                strcpy(name,"a"); strcat(name,muste_itoa(j,index,10));
                deri(dmod,d1mod,name);

                if (strcmp(dmod,"0")!=0) linind=0;

                if (est_disp_all)
                    { sprintf(sbuf2,"%s  ",dmod); sur_print(sbuf2); }

                fcmuunna(cmod,dmod);
                laus[k++]=lause;
                lause=muunnos(cmod,lause,sanoma);
                if (strcmp(sanoma,"OK")!=0) { CLS; sprintf(sbuf2,"\n%s",sanoma);
                                              sur_print(sbuf2); WAIT; return(-1); }
                }
              }
             #if defined(TEST)
             sprintf(sbuf2,"\n%s",dmod); sur_print(sbuf2);
             #endif

        if (!logd)
             {
             if (linind==1) *name=EOS; else strcpy(name,"not ");
        sprintf(sbuf2,"\nModel is %slinear with respect to parameters!\n",name); sur_print(sbuf2);
             }
       if (est_disp_all)
           {
       for (i=0; i<lause; ++i) { sprintf(sbuf2,"(%c %d %d) ",laji[i],ind[i],lag[i]);
              sur_print(sbuf2); }

       sprintf(sbuf2,"\n# of items = %d\n",lause); sur_print(sbuf2);
           }

        return(1);
        }

static int check(char s[])
        {
        int t=0;
        char sana[20];
        int kesken=1;
        int h,l;

        l=strlen(s); s[l]=' '; s[l+1]=EOS;
        *sana=EOS;
        for (h=0; *(s+h) && kesken; ++h)
        switch (*(s+h))
          {
        case '(': ++t;
                  /* funktioita ei tutkita */
                  *sana=EOS; break;
        case ')': --t;
                  tutki(sana); break;
        case '+':
        case '-':
                  tutki(sana); break;
        case '*':
        case '/':
        case '^':
                  tutki(sana); break;
        case ' ':
        case ',':
                  tutki(sana);
                  kesken=0; break;
        case '[': tutki(sana);
                  while (*(s+h) && *(s+h)!=']') ++h;
                  if (*(s+h)==EOS) { sprintf(sbuf2,"\n] missing in\n  %s",s);
                                     sur_print(sbuf2); WAIT; return(-1); }
                  break;
        default:
                  l=strlen(sana); if (l>20) { syntax(s); return(-1); }
                  sana[l]=*(s+h); sana[l+1]=EOS;
          }
        if (t>0) { sprintf(sbuf2,"\n) missing in\n  %s",s);
                   sur_print(sbuf2); WAIT; return(-1); }
        if (t<0) { sprintf(sbuf2,"\n( missing in\n  %s",s);
                   sur_print(sbuf2); WAIT; return(-1); }
        l=strlen(s); s[l-1]=EOS;
        return(em2);
        }

static int tutki(char sana[])
        {
        int i,h;
        i=varnro(sana);
        if (i<0) { *sana=EOS; return(1); }
        for (h=0; h<em2; ++h)
                { if (i==sdata.v[h]) break; }
        if (h==em2) sdata.v[em2++]=i;
        *sana=EOS;
        return(1);
        }

static int varnro(char sana[])
        {
        int i;
        for (i=0; i<em; ++i)
                if (strcmp(sana,sdata.varname[i])==0) return(i);
        return(-1);
        }

static int syntax(char s[])
        {
        sprintf(sbuf2,"\nSyntax error in\n  %s",s);
        sur_print(sbuf2); WAIT;
        s_end(argv1);
        return(1); // formally
        }

static int muunna(char d[],char s[])
                /* muuttujat --> x0,x1,... parametrit --> a0,a1,...  */
                /* #parametri --> vakio c1,c2,... (c0=0)  */
                /* viiveet ja eteet [aika-5] --> [m5]  [+6] --> [p6] */
                /* muut vakiot j‰‰v‰t muuntamatta */
        {
        int t=0;
        char sana[20];
        int kesken=1;
        int h,l,i;
        int lag;

        *d=EOS;
        l=strlen(s); s[l]=' '; s[l+1]=EOS;
        *sana=EOS;
        for (h=0; *(s+h) && kesken; ++h)
        switch (*(s+h))
          {
        case '(': ++t;
                  /* funktioita ei tutkita */
                  strcat(d,sana); *sana=EOS; jatka(d,'('); break;
        case ')': --t;
                  i=tutki2(sana,d); if (i<0) return(-1);
                  jatka(d,')'); break;
        case '+':
        case '-':
                  i=tutki2(sana,d); if (i<0) return(-1);
                  jatka(d,*(s+h)); break;
        case '*':
        case '/':
        case '^':
                  i=tutki2(sana,d); if (i<0) return(-1);
                  jatka(d,*(s+h)); break;
        case ' ':
        case ',':
                  i=tutki2(sana,d); if (i<0) return(-1);
                  kesken=0; break;
        case '[': i=tutki2(sana,d); if (i<0) return(-1);
                  lag=viive(s,&h);
                  if (lag<0) { if (-lag>obs1) obs1=-lag; }
                  else if (lag>obs2) obs2=lag;
                  if (lag)
                      {  /* merk. [+3]=[p3] [-3]=[m3] (+- sotkisivat deriv) */
                      jatka(d,'[');
                      if (lag<0) { jatka(d,'m'); lag=-lag; } else jatka(d,'p');
                      strcat(d,muste_itoa(lag,sana,10)); jatka(d,']');
                      }
                  *sana=EOS;
                  break;
        default:
                  l=strlen(sana); if (l>20) { syntax(s); return(-1); }
                  sana[l]=*(s+h); sana[l+1]=EOS;
          }
        if (t>0) { sprintf(sbuf2,"\n) missing in\n  %s",s);
                   sur_print(sbuf2); WAIT; return(-1); }
        if (t<0) { sprintf(sbuf2,"\n( missing in\n  %s",s);
                   sur_print(sbuf2); WAIT; return(-1); }
        l=strlen(s); s[l-1]=EOS;
        return(na);
        }

static int tutki2(char sana[],char d[])
        {
        int i,j;
        char index[3];
        char para[20];
        /* muuttuja */
        for (i=0; i<em2; ++i)
                {
                if (strcmp(sana,sdata.varname[sdata.v[i]])==0)
                        {
                        jatka(d,'x'); strcat(d,muste_itoa(i,index,10));
                        *sana=EOS;
                        return(1);
                        }
                }
        /* parametri tai vakio */
        if (luku(sana)) { strcat(d,sana); *sana=EOS; return(1); }
        /* parametri */
        for (i=0; i<na; ++i)
                {
                if (strcmp(sana,aname[i])==0)
                        {
                        jatka(d,'a'); strcat(d,muste_itoa(i,index,10));
                        *sana=EOS;
                        return(1);
                        }
                }
        strcpy(para,"#"); strcat(para,sana);
        j=spfind(para);
        if (j>=0)
                {
                for (i=0; i<nc; ++i)
                        {
                        if (strcmp(sana,cname[i])==0)
                                {
                                jatka(d,'c'); strcat(d,muste_itoa(i,index,10));
                                *sana=EOS;
                                return(1);
                                }
                        }
                if (nc>MAXC)
                        {
                          sur_print("\nToo many constants!");
                          WAIT; return(-1);
                        }
                cname[nc]=acl;
                i=acmerk(sana); if (i<0) return(-1);
                cc[nc]=atof(spb[j]);
                i=nc; ++nc;
                jatka(d,'c'); strcat(d,muste_itoa(i,index,10));
                *sana=EOS;
                return(1);
                }
        if (na>MAXA)
                {
                  sur_print("\nToo many parameters!");
                  WAIT; return(-1);
                }
        aname[na]=acl;
        i=acmerk(sana); if (i<0) return(-1);
        /* alkuarvo */
        j=spfind(sana); if (j>=0) a[na]=atof(spb[j]); else a[na]=0;
        i=na; ++na;
        jatka(d,'a'); strcat(d,muste_itoa(i,index,10));
        *sana=EOS;
        return(1);
        }

static int jatka(char s[],char m)
        {
        int l;
        l=strlen(s);
        s[l]=m; s[l+1]=EOS;
        return(1);
        }

static int luku(char s[])
        {
        char *h;
        int piste=0;
        for (h=s; *h; ++h)
            {
            if (*h=='.') { if (piste) return(0);
                           piste=1; continue; }
            if ( *h<'0' || *h>'9' ) return(0);
            }
        return(1);
        }

static int acmerk(char s[])
        {
        int len=strlen(s);
        if (acl-aclist+len+1>MAXAC)
                {
                  sur_print("\nToo much text in parameters!");
                  WAIT; return(-1);
                }
        strncpy(acl,s,len); acl+=len+1; *(acl-1)=EOS;
        return(1);
        }

/* funktiot ja numeeriset vakiot */
static int fcmuunna(char d[],char s[])
        {
        int t=0;
        char index[3];
        char sana[20];
        int kesken=1;
        int h,l,i;

        *d=EOS;
        l=strlen(s); s[l]=' '; s[l+1]=EOS;
        *sana=EOS;
        for (h=0; *(s+h) && kesken; ++h)
        switch (*(s+h))
          {
        case '(': ++t;
                  if (*sana==EOS) { jatka(d,'('); break; }
                  i=funktio(sana); if (i<0) return(-1);
                  jatka(d,'f'); strcat(d,muste_itoa(i,index,10));
                  jatka(d,'('); *sana=EOS; break;
        case ')': --t;
                  i=tutki3(sana,d); if (i<0) return(-1);
                  jatka(d,')'); break;
        case '+':
        case '-':
                  i=tutki3(sana,d); if (i<0) return(-1);
                  jatka(d,*(s+h)); break;
        case '*':
        case '/':
        case '^':
                  i=tutki3(sana,d); if (i<0) return(-1);
                  jatka(d,*(s+h)); break;
        case ' ':
        case ',':
                  i=tutki3(sana,d); if (i<0) return(-1);
                  kesken=0; break;
        case '[':
                  i=tutki3(sana,d); if (i<0) return(-1);
                  while (*(s+h)!=']') { jatka(d,*(s+h)); ++h; }
                  jatka(d,']');
                  break;
        default:
                  l=strlen(sana); if (l>20) { syntax(s); return(-1); }
                  sana[l]=*(s+h); sana[l+1]=EOS;
          }

        if (t>0) { sprintf(sbuf2,"\n) missing in\n  %s",s);
                   sur_print(sbuf2); WAIT; return(-1); }
        if (t<0) { sprintf(sbuf2,"\n( missing in\n  %s",s);
                   sur_print(sbuf2); WAIT; return(-1); }
        l=strlen(s); s[l-1]=EOS;
        return(1);
        }

static int tutki3(char sana[],char d[])
        {
        int i;
        char index[3];

        if (*sana==EOS) return(1);
        if (luku(sana)==0) { strcat(d,sana); *sana=EOS; return(1); }
        for (i=0; i<nc; ++i)
                {
                if (strcmp(sana,cname[i])==0)
                        {
                        jatka(d,'c'); strcat(d,muste_itoa(i,index,10));
                        *sana=EOS;
                        return(1);
                        }
                }
        if (nc>MAXC)
                {
                  sur_print("\nToo many constants!");
                  WAIT; return(-1);
                }
        cname[nc]=acl;
        i=acmerk(sana); if (i<0) return(-1);
        cc[nc]=atof(sana);
        i=nc; ++nc;
        jatka(d,'c'); strcat(d,muste_itoa(i,index,10));
        *sana=EOS;
        return(1);
        }

static int funktio(char sana[])
        {
        if (muste_strcmpi(sana,"sqrt")==0) return(1);
        if (muste_strcmpi(sana,"sqr")==0) return(1);
        if (muste_strcmpi(sana,"log")==0) return(2);
        if (muste_strcmpi(sana,"ln")==0) return(2);
        if (muste_strcmpi(sana,"abs")==0) return(3);
        if (muste_strcmpi(sana,"int")==0) return(4);
        if (muste_strcmpi(sana,"exp")==0) return(5);
        if (muste_strcmpi(sana,"sin")==0) return(6);
        if (muste_strcmpi(sana,"cos")==0) return(7);
        if (muste_strcmpi(sana,"tan")==0) return(8);
        if (muste_strcmpi(sana,"atan")==0) return(9);
        if (muste_strcmpi(sana,"arctan")==0) return(9);
        if (muste_strcmpi(sana,"sinh")==0) return(10);
        if (muste_strcmpi(sana,"cosh")==0) return(11);
        if (muste_strcmpi(sana,"tanh")==0) return(12);
        if (muste_strcmpi(sana,"asin")==0) return(13);
        if (muste_strcmpi(sana,"arcsin")==0) return(13);
        if (muste_strcmpi(sana,"acos")==0) return(14);
        if (muste_strcmpi(sana,"arccos")==0) return(14);
        if (muste_strcmpi(sana,"lgamma")==0) return(15);
        if (muste_strcmpi(sana,"round")==0) return(16); /* 8.9.1998 */
        if (strcmp(sana,"PHI")==0 || strcmp(sana,"Phi")==0) return(17); /* 19.2.2002 */
        if (strcmp(sana,"phi")==0) return(18); /* 20.2.2002 */

        if (strcmp(sana,"OVERFLOW")==0)
            {
            sur_print("\nNot enough space for (second) derivatives!"); WAIT; s_end(argv1);
            }
        sprintf(sbuf2,"\nUnknown function '%s'",sana); sur_print(sbuf2); WAIT; s_end(argv1);
        return(-1);
        }

static int viive(char s[],int *ph)
        {
        char sana[10];
        int h,i;

        h=*ph+1; i=0;
        while (*(s+h)!=']')
            {
            if ( (i==0) && ( *(s+h)!='+' && *(s+h)!='-'
                             && !(*(s+h)>='0' && *(s+h)<='9') ) )
               { ++h; continue; }
            sana[i++]=*(s+h); ++h;
            }
        sana[i]=EOS;
        *ph=h;
        return(atoi(sana));
        }

/* ed.c   16.7.85/SM (21.8.1986)

*/

static int eoutput(char line[])
        {
        output_line(line,eout,eol);
        if (eol>0) { ++eol; ++eoutind; }
        if (eol>r2) eol=0;
        return(1);
        }

static int corrnorm(double *H,int m)
        {
        int i,j;

        for (i=0; i<m; ++i)
            {
            if (H[i*m+i]<=0.0)
                {
                sur_print("\nDiagonal elements of a covariance matrix not positive!");
                sur_print("\nPress any key!"); sur_getch();
                return(-1);
                }
            }
        for (i=0; i<m; ++i)
            for (j=0; j<m; ++j)
                {
                if (i!=j)
                    H[i*m+j]/=sqrt(H[i*(m+1)]*H[j*(m+1)]);
                }
        for (i=0; i<m; ++i) H[i*(m+1)]=1.0;
        return(1);
        }

/* dat2.c 17.7.85/SM (12.10.1986) (1.1.1997)

*/

/* data table/file */

static int datopen(char data[])
        {
        int i;
        char *p,*q;

        i=data_read_open(data,&sdata); if (i<0) return(-1);
        i=conditions(&sdata); if (i<0) return(-1);

        em=sdata.m; em2=sdata.m_act;
        en=sdata.l2-sdata.l1+1; /* tilap. */
        for (i=0; i<em; ++i)    /* nimien lyhennys */
            {
            p=sdata.varname[i]; q=p+7; *(q+1)=EOS;
            while (*q==' ' && q>p) *q--=EOS;
            }
        return(em);
        }

static int datain()
        {
        int j=0;
        int i;
        long l;
        unsigned int tila;
        double val;
        int j0;

        tila=DATASTEP;
        xx=(double *)muste_malloc(tila*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return(-1); }

        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);
        obs2=en-obs2;

        en=0;
        sur_print("\nLoading observations...");
        for (l=sdata.l1; l<=sdata.l2; ++l)
            {
            if (unsuitable(&sdata,l)) continue;
   //       if (kbhit()) { getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf2," %ld",l); sur_print(sbuf2); }
            ++en; j0=j;
            for (i=0; i<em2; ++i)
                {
                if (j>=tila)
                    {
                    tila+=DATASTEP;
            /*
                    if (tila>8150) { not_enough_memory(); return(-1); }
            */
                    xx=(double *)muste_realloc(xx,tila*sizeof(double));
                    if (xx==NULL) { not_enough_memory(); return(-1); }
                    }
                data_load(&sdata,l,sdata.v[i],&val);
                if (val==MISSING8)
                    {
                    if (est_disp_all) sur_print(" Missing!   ");
                    --en; j=j0;
                    break;
                    }
                xx[j++]=val;
                }
            }
        obs2=en-obs2;
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory for data/expressions!");
        WAIT; return(1);
        }

/* mu.c  8.7.1985/SM (25.3.89) (1.1.1997) (8.9.1998) */

#define MUUTTUJA 'x'
#define PARAMETRI 'a'
#define VAKIO 'c'
#define FUNKTIO 'f'
#define LOPPU 'l'
#define PINOMAX 30
#define N1 50

extern double sur_st_norm();
extern double sur_lg_gamma();
/* extern int errno; */


static char laji2[N1];
static int ind2[N1];
static int lag2[N1];

static int taso[N1];
static int t=0;
static int j;
static int h,i_laus;

static int muunnos(char *s,int alku,char *sanoma)
// char *s;       /* muunnettava lauseke */
// int alku;    /* j‰sennetyn lausekkeen alku */
// char *sanoma;  /* virheilmoituksia varten */
{
int f1=0; /* virhe, jos x a c ) edelt‰‰ x a c f (        */
int f2=0; /* virhe, jos + - * / ^ edelt‰‰ + - * / ^ )    */
int f3=0; /* sulkujen tarkastus */
int f4=1; /* virhe, jos ( tai alku edelt‰‰ * / ^ )       */

i_laus=alku; j=0;

strcpy(sanoma,"OK");
/*  printf("alku=%d\n",i);  */
for (h=0; *(s+h) && j<N1 ; ++h)
 {
/* printf("merkki:%c\n",*(s+h)); */
        switch (*(s+h))
        {
     case MUUTTUJA:
        if (f1) virhe(sanoma);
        f1=1; f2=0; f4=0;
        laji2[j]=MUUTTUJA;
        ind2[j]=indeksi(s);
        lag2[j]=0;
        if (*(s+h+1)=='[')
            {
            char sana[8];
            int i=0, merkki;

            h+=2; if (*(s+h)=='m') merkki=-1; else merkki=1;
            ++h;
            while (*(s+h)!=']' && i<7) { sana[i++]=*(s+h); ++h; }
            sana[i]=EOS; lag2[j]=merkki*atoi(sana);
            }
        taso[j]=t;
        ++j; break;
     case PARAMETRI:
        if (f1) virhe(sanoma);
        f1=1; f2=0; f4=0;
        laji2[j]=PARAMETRI;
        ind2[j]=indeksi(s);
        taso[j]=t;
        ++j; break;
     case VAKIO:
        if (f1) virhe(sanoma);
        f1=1; f2=0; f4=0;
        laji2[j]=VAKIO;
        ind2[j]=indeksi(s);
        taso[j]=t;
        ++j; break;
     case '+':
     case '-':
        if (f2) virhe(sanoma);
        f1=0; f2=1; f4=0;
                  if ( (j==0) || (laji2[j-1]==FUNKTIO) )
                       {laji2[j]=VAKIO; ind2[j]=0; taso[j]=t; ++j;}

        if (t>=1) pinoa(1);
        laji2[j]=*(s+h);
        taso[j]=t=1;
        ++j; break;
     case '*':
     case '/':
        if (f2 || f4) virhe(sanoma);
        f1=0; f2=1; f4=0;
        if (t>=2) pinoa(2);
        laji2[j]=*(s+h);
        taso[j]=t=2;
        ++j; break;
     case '^':
        if (f2 || f4) virhe(sanoma);
        f1=0; f2=1; f4=0;
        if (t==3) pinoa(3);
        laji2[j]=*(s+h);
        taso[j]=t=3;
        ++j; break;
     case '(':
        if (f1) virhe(sanoma);
        f1=0; f2=0; ++f3; f4=1;
        laji2[j]=FUNKTIO;
        ind2[j]=0;
        taso[j]=t; t=0;
        ++j; break;
     case ')':
        if (f2 || f4) virhe(sanoma);
        f1=1; f2=0; --f3; f4=0;
        if (f3<0) strcpy(sanoma,"( is missing!");
        pinoa(1);
        --j;
        if (ind2[j-1]!=0) siirto(j-1);
        laji2[j-1]='p';
        t=taso[j-1];
        break;
     case FUNKTIO:
        if (f1) virhe(sanoma);
        f1=0; f2=0; ++f3; f4=1;
        laji2[j]=FUNKTIO;
        ind2[j]=indeksi(s);
        ++h;
        if (*(s+h)!='(') strcpy(sanoma,"( is missing!");
        taso[j]=t; t=0;
        ++j; break;

     default:
        strcpy(sanoma,"Unknown character ");
        ++j; break;

        } /* switch */
 }

if (j>=N1) {strcpy(sanoma,"Expression is too nested!"); return(0); }
if (f3>0) strcpy(sanoma,") is missing!");
if ( f2 || f4 || (f1==0) ) virhe(sanoma);
pinoa(1);
laji[i_laus]=LOPPU; ++i_laus;
return(i_laus); /* seuraavan lausekkeen alkua varten */

} /* muunnos() */


static int pinoa(int u)
{
if (j<1) return(1);
if (taso[j-1]==0) siirto(j-1);
else
 {
 while ( (j>1) && (taso[j-1]>=u) )
  {
  if (laji2[j-3]=='p')
   {
   if (laji2[j-1]=='p') { siirto(j-2); j-=2; }
   else                 { siirto(j-1); siirto(j-2); j-=2; }
   }
  else
   {
   siirto(j-3);
   if (laji2[j-1]=='p')
    {
    if (laji2[j-2]=='-') laji2[j-2]='m';
    if (laji2[j-2]=='/') laji2[j-2]='d';
    if (laji2[j-2]=='^') laji2[j-2]='e';
    siirto(j-2); j-=2;
    }
   else
    { siirto(j-1); siirto(j-2); j-=2; }
   }
  laji2[j-1]='p';
  t=taso[j-1];
  } /* while */
 }
return(1);
}

static int siirto(int k)
        {
        int h;

        if (laji2[k]=='p') return(1);
/* printf("i=%d k=%d laji=%c ind=%d\n",i,k,laji2[k],ind2[k]); getch(); */

        if (laji==NULL)
            {
            h=spfind("FSPACE");
            if (h<0) nlaus=FSPACE; else nlaus=atoi(spb[h]);
                                                              /* 1.1.1997 */

            laji=(char *)calloc(nlaus,sizeof(char));
            if (laji==NULL) { not_enough_memory(); s_end(argv1); }
            ind=(int *)calloc(nlaus,sizeof(int));
            if (laji==NULL) { not_enough_memory(); s_end(argv1); }
            lag=(int *)calloc(nlaus,sizeof(int));
            if (lag==NULL) { not_enough_memory(); s_end(argv1); }
            }
        if (i_laus>=nlaus)
            {
            sur_print("\nNot space enough for functions/expressions!");
        sprintf(sbuf2,"\nCurrent maximum=%d. Use FSPACE=<integer> to allocate more space!",nlaus);
            sur_print(sbuf2); WAIT;
            s_end(argv1);
            }
        laji[i_laus]=laji2[k];
        ind[i_laus]=ind2[k];
        lag[i_laus]=lag2[k];
        ++i_laus;
        return(1);
        }

static int indeksi(char *s)
        {

        int ind=0;
        ++h;
        for ( ; (*(s+h)>='0') && (*(s+h)<='9') ; ++h)
                ind=10*ind+(*(s+h))-'0';
        --h;
        return (ind);
        }

static int virhe(char *sanoma)
        {
        strcpy(sanoma,"Syntax error");
        return(1);
        }

extern double sur_st_norm(); // 19.2.2002

/* arvo.c  8.7.1985/SM (20.7.85)  */

double earvo(int j,int alku,double a[],double c[],double *xx)
/* muutos arvo->earvo 10.10.86 vrt. spec.c */
// int j;        /* havainnon nro */
// int alku;     /* j‰sennetyn lausekkeen alku */
// double a[];     /* parametrit */
// double c[];     /* vakiot */

{
double pino[PINOMAX];
double power();
int p;
int i;
// int h;


p=0;
for (i=alku; (laji[i]!=LOPPU); ++i)
 {  /* for (h=0; h<=p; ++h)
       printf("h=%d laji=%c ind=%d j=%d em2=%d pino=%f\n",h,laji[h],ind[h],
                               j,em,pino[h]);
       getch(); */
     /* if (errno) { WAIT; fcloseall(); abort(); }  poistettu 31.7.88 */
        switch (laji[i])
        {
     case MUUTTUJA:
        pino[p]=*(xx+(j+lag[i]-1)*em2+ind[i]); ++p; break;
     case PARAMETRI:
        pino[p]=*(a+ind[i]); ++p; break;
     case VAKIO:
        pino[p]=*(c+ind[i]); ++p; break;
     case '+':
        --p; pino[p-1]+=pino[p]; break;
     case '-':
        --p; pino[p-1]-=pino[p]; break;
     case 'm':
        --p; pino[p-1]=pino[p]-pino[p-1]; break;
     case '*':
        --p; pino[p-1]*=pino[p]; break;
     case '/':
        --p; pino[p-1]/=pino[p]; break;
     case 'd':
        --p; pino[p-1]=pino[p]/pino[p-1]; break;
     case '^':
   /*      printf("pow: %G %G\n",pino[p-2],pino[p-1]); getch();   */
        --p; pino[p-1]=power(pino[p-1],pino[p]); break;
     case 'e':
        --p; pino[p-1]=power(pino[p],pino[p-1]); break;

     case FUNKTIO:
          switch (ind[i])
            {
        case  1:
           pino[p-1]=sqrt(pino[p-1]); break;
        case  2:
           pino[p-1]=log(pino[p-1]); break;
        case  3:  /* abs */
           pino[p-1]=fabs(pino[p-1]); break;
        case  4:  /* int */
           pino[p-1]=floor(pino[p-1]); break;
        case  5:
           pino[p-1]=exp(pino[p-1]); break;
        case  6:
           pino[p-1]=sin(pino[p-1]); break;
        case  7:
           pino[p-1]=cos(pino[p-1]); break;
        case  8:
           pino[p-1]=tan(pino[p-1]); break;
        case  9:
           pino[p-1]=atan(pino[p-1]); break;
        case 10:
           pino[p-1]=sinh(pino[p-1]); break;
        case 11:
           pino[p-1]=cosh(pino[p-1]); break;
        case 12:
           pino[p-1]=tanh(pino[p-1]); break;
        case 13:
           pino[p-1]=asin(pino[p-1]); break;
        case 14:
           pino[p-1]=acos(pino[p-1]); break;
        case 15:
           pino[p-1]=sur_lg_gamma(pino[p-1]); break;
        case 16:
           pino[p-1]=est_round(pino[p-1]); break;
        case 17:
           pino[p-1]=sur_st_norm(pino[p-1],0.0); break; // 19.2.2002 Phi
        case 18: // 20.2.2002  phi
// 1/sqrt(2*3.141592653589793)=0.3989422804014327         ACCURACY=16
           pino[p-1]=0.3989422804014327*exp(-0.5*pino[p-1]*pino[p-1]); break;



            } /* switch ind(i) (funktiot) */
        } /* switch laji(i) */
 }
return (pino[0]);
}

static double power(double x,double y)
        {
        int i;
        double f;
        if (y>=0 && y==floor(y) && y<10)
                {
                f=1;
                for (i=0; i<(int)y; ++i) f*=x;
                return(f);
                }
        return (pow(x,y));
        }

static double est_round(double x) /* 8.9.1998 */
        {
        long l;
        double a;

        l=x;
        a=l;
        if (x>0.0 && x-a>0.5) ++a;
        else if (x<0.0 && a-x>0.5) --a;
        return(a);
        }

/* ho.c 20.7.85/SM (23.5.1987) (5.12.1993)

*/





/********************
extern double a[];
extern double s_a[];
extern char *aname[];
extern int na;
extern double cc[];
extern char *cname[];
extern int nc;
extern char aclist[];
extern char *acl;
extern int laus[];
extern char malli[];
extern double criterion;
extern int logd;
extern obs1,obs2;
extern int eaccuracy;
extern int results;
*************************/



/* extern int errno;   */



static int hooke()
        {
        double rss();
        double wsum();
        double Rsquare();
        double step[N], maxstep, minstep, initstep;
        int i,j,nf;
        double av[N];
        char *sana[2]; int g;
        char rivi[LLENGTH];
        double s,c;
        int inv;
        int n=obs2-obs1+1;
        double r2=0.0;
   //   double step2[N];
        int stop;
        double aa;
        char t1[32],t2[32];

        inv=0;
        output_open(eout);
        initstep=1.0; minstep=0.00001;
        i=spfind("STEP");
        if (i>=0)
            {
            g=split(spb[i],sana,2);
            if (g>0) { initstep=atof(sana[0]);
                       if (initstep<=0.0) initstep=1.0;
                       minstep=0.00001*initstep;
                     }
            if (g>1) { minstep=atof(sana[1]);
                       if (minstep<=0.0) minstep=0.00001*initstep;
                     }
            }
        weightsum=wsum();
        rss0(); /* Tulos alkuarvoilla */
        for (i=0; i<na; ++i) { step[i]=initstep; av[i]=a[i]; }
        nf=0;
        maxstep=initstep;
        while ( maxstep>minstep && !sur_kbhit() )
            {
            i=hoojee(a,na,rss,step);
            if (i<0) { stop=1; i=-i; stopdisp(rivi);  break; }
            nf+=i;
            sur_print("\nparam: ");
            for (i=0; i<na; ++i) { sprintf(sbuf2,"%10f ",a[i]); sur_print(sbuf2); }
                 sur_print("\n");
            printf("steps: ");
          for (i=0; i<na; ++i) { sprintf(sbuf2,"%10f ",step[i]); sur_print(sbuf2); }
                 sur_print("\n");
            sprintf(sbuf2,"rss=%f  nf=%d\n",rss(a),nf); sur_print(sbuf2);
            for (i=0; i<na; ++i) { step[i]/=2;
                                   if (av[i]==a[i]) step[i]/=2;
                                   av[i]=a[i];
                                 }
            maxstep=0; for (i=0; i<na; ++i)
                           if (step[i]>maxstep) maxstep=step[i];
            }

        if (criterion==2.0)
            {
            numhess(a,HH,na,step); nf+=(na+1)*(na+1);
            inv=cholinv(HH,na);
            }
        s=rss(a); if (logd) c=1; else c=2*s/(n-na);

        sprintf(rivi,"Estimated parameters of model %s:%c",malli,EOS);
        eoutput(rivi);
        if (sur_kbhit())
            {
            sur_getch();
            stopdisp(rivi);
            }

        for (i=0; i<na; ++i)
            {
            s_a[i]=sqrt(c*HH[na*(i+1)+i]);
            if (eaccuracy)
              {
              fnconv(a[i],eaccuracy+2,t1);
              if (criterion!=2.0) strcpy(t2,"-");
              else
                  {
                  aa=sqrt(c*HH[na*(i+1)+i]);
                  fnconv(aa,eaccuracy+2,t2);
                  }
              sprintf(rivi,"%s=%s (%s)",aname[i],spois(t1),spois(t2));
              }
            else
              {
              if (criterion!=2.0)
                  sprintf(rivi,"%s=%G",aname[i],a[i]);
              else
                  sprintf(rivi,"%s=%G (%G)",aname[i],a[i],sqrt(c*HH[na*(i+1)+i]));
              }
            eoutput(rivi);
            }

        if (logd)
            sprintf(rivi,"n=%d log(L)=%f nf=%d",n,-s,nf);
        else if (criterion==2.0)
            {
            r2=Rsquare();
            if (r2<=1e-5)
                sprintf(rivi,"n=%d rss=%f nf=%d",n,s,nf);
            else
                sprintf(rivi,"n=%d rss=%f R^2=%.5f nf=%d",n,s,r2,nf);
            }
        else
           sprintf(rivi,"n=%d Norm L%G: min=%f nf=%d",n,criterion,s,nf);
        eoutput(rivi);

        if (inv<0 || criterion!=2.0) return(1);
        for (i=0; i<na; ++i) for (j=0; j<na; ++j)
            HH[na*i+j]=HH[na*(i+1)+j];
        for (i=0; i<na; ++i) for (j=0; j<=i; ++j)
            HH[na*j+i]=HH[na*i+j];
        output_close(eout);

        save_matrices(aname,na,a,s_a,n,s,r2); // 12.4.2005

        if (na>1)
            {
            i=corrnorm(HH,na);
            if (i<0)
                {
                sur_print("\nObviously not a true solution!");
                WAIT; return(-1);
                }
            if (results>=0)
                corrp(HH,na,aname,c3,7,3,"Correlations:");
            corr_save(HH,a,s_a,na,aname);
            }
        return(1);
        }

static double rss(double a[])
        {
        double earvo();
        double wsum();
        extern int weightind;
        extern double criterion;
        int j;
        double b;
        double sum2=0;
        int n=obs2-obs1+1;

/* printf("\nrss:"); for (j=0; j<na; ++j) printf(" %g",a[j]);  getch(); */
        if (!logd)
         {
         if (criterion==2)
            {
            for (j=obs1; j<=obs2; ++j)
                    {
                    b=earvo(j,laus[0],a,cc,xx)-earvo(j,laus[1],a,cc,xx);
                    if (weightind==0) sum2+=b*b;
                    else              sum2+=earvo(j,laus[2],a,cc,xx)*b*b;
                    }
            if (weightind==0) return (sum2);
            else   return (sum2*n/weightsum);
            }
         else
            {
            for (j=obs1; j<=obs2; ++j)
                    {
                    b=earvo(j,laus[0],a,cc,xx)-earvo(j,laus[1],a,cc,xx);
                    if (weightind==0) sum2+=pow(fabs(b),criterion);
                    else sum2+=earvo(j,laus[2],a,cc,xx)*pow(fabs(b),criterion);
                    }
            if (weightind==0) return (sum2);
            else   return (sum2*n/weightsum);
            }
         } /* regressiomallit */

        else /* maximum likelihood (logd=1) */
         {
         for (j=obs1; j<=obs2; ++j)
                 {
                 save_update1(j); // 9.5.2004
                 b=-earvo(j,laus[1],a,cc,xx);
// printf("\nj=%d b=%g|",j,b); getch();
                 if (weightind==0) sum2+=b;
                 else              sum2+=earvo(j,laus[2],a,cc,xx)*b;
                 }
/*  printf("  %g",sum2); getch();       */
            if (weightind==0) return (sum2);
            else   return (sum2*n/weightsum);
         }
        }

static int rss0()
        {
        double f;

        if (!logd) f=rss(a); else f=-rss(a);
        sprintf(sbuf2,"\nValue of criterion function on initial estimates = %g\n",f);
            sur_print(sbuf2);
        return(1);
        }

static double wsum()
        {
        int j;
        double sum;

        sum=0;
        for (j=obs1; j<=obs2; ++j)
            sum+=earvo(j,laus[2],a,cc,xx);
        return(sum);
        }

static double y;
static int m,n;
static int hoojee(double x[],int dim,double (*f)(),double step[])
        {
        int i,kesken=1;
        double b1[N],b2[N],p1[N];

        m=dim;
        y=(*f)(x); n=1;
        vcopy(x,b1);

        while (kesken)
            {
            i=emove(b1,b2,f,step);
            if (i==0) { kesken=0; continue; }
            i=pmove(b1,b2,p1,f,step);
            if (i==-2) return(-n);
            vcopy(b2,b1);
            }
        vcopy(b1,x);
        return(n);
        }

static int numhess(double a[],double H[],int m,double step[])
        {
        double rss();

        int i,j;
        double f0,f1,f2,ai,aj;

        for (i=0; i<m; ++i) for (j=0; j<=i; ++j) H[m*i+j]=0;
        f0=rss(a);
        for (i=0; i<m; ++i)
            {
            ai=a[i]; a[i]+=step[i]; f1=rss(a); a[i]=ai;
            for (j=0; j<=i; ++j)
                {
                aj=a[j]; a[j]+=step[j]; f2=rss(a);
                ai=a[i]; a[i]+=step[i];
                H[m*i+j]=H[m*j+i]=(rss(a)-f2-f1+f0)/(step[i]*step[j]);
                a[i]=ai; a[j]=aj;
                }
            }
        return(1);
        }

static int emove(double b1[],double b2[],double (*f)(),double step[])
        {
        int i;
        double yb2,bb;
        int success=0;

        vcopy(b1,b2);
        for (i=0; i<m; ++i)
            {
            bb=b2[i]; b2[i]+=step[i];
            yb2=(*f)(b2); ++n;
            if (yb2<y)
                {
                success=1;
                y=yb2;
                }
            else
                {
                b2[i]-=2*step[i];
                yb2=(*f)(b2); ++n;
                if (yb2<y)
                    {
                    success=1;
                    y=yb2;
                    }
                else b2[i]=bb;
                }
            }
        return (success);
        }

static int pmove(double b1[],double b2[],double p1[],double (*f)(),double step[])
        {
        int i;
        int pkesken=1;
        double b3[N];
        double y0;
        int k;

        while (pkesken)
            {
            for (i=0; i<m; ++i) p1[i]=2*b2[i]-b1[i];
            y0=y;
            y=(*f)(p1);    /* lis‰ys */
            i=emove(p1,b3,f,step);
            if (sur_kbhit())
                {
                k=sur_getch();
                if (k=='.') return(-2);
                }
            if (y0<=y) { y=y0; pkesken=0; continue; }
            if (i==1)
                {
                vcopy(b2,b1);
                vcopy(b3,b2);
                continue;
                }
            pkesken=0;
            }
        return(1);
        }

static int vcopy(double x[],double y[])
        {
        int i;
        for (i=0; i<m; ++i) y[i]=x[i];
        return(1);
        }

static int stopdisp(char rivi[])
        {
        sprintf(rivi,"(Estimation interrupted by the user)%c",EOS);
        eoutput(rivi);
        return(1);
        }

/* DR.C Derivatives 6.7.85/SM (29.7.85) */
/* (ESTIMATE) kuten DERI.C (vain RIVI A alla muutettu */

#define NFUNC 40*1024
#define EOS '\0'
#define MUUTT '@'
#define MUUTTUJA1 "@"
#define MMUUTTUJA "-@"
#define NEND NFUNC-20         /* for overflow checking */
#define OF "OVERFLOW(@)"

static int deri(char d[],char s[],char x[])
{
char *deriv();
korvaa(s,x,MUUTTUJA1);
deriv(d,s);
korvaa(d,MUUTTUJA1,x);
korvaa(s,MUUTTUJA1,x); /* RIVI A: jotta voidaan toistaa eri muuttujalla */
return(1);
}

static int korvaa(char *s,char *x,char *m)
        {
        char t[NFUNC];
        char *p,*q;
        int len=strlen(x);
        strcpy(t,s);
        *s=EOS;
        p=t;
        while (*p!=EOS)
                {
                q=strchr(p,*x); /* sanan x 1.merkki */
                if (q==NULL) { strcat(s,p); return(1); }
                if ( (q==t) ||
                     (strchr("+-*/^(",*(q-1))!=NULL) )
                      {
                      if (strncmp(q,x,len)==0)
                        {
                          q+=len;
                          if ( (*q==EOS) ||
                               (strchr("+-*/^)",*q)!=NULL) )
                               {
                                 strncat(s,p,q-p-len);
                                 strcat(s,m);
                                 p=q; continue;
                               }
                          else { strncat(s,p,q-p);
                                 p=q; continue;
                               }
                        }
                      else
                        { strncat(s,p,q-p+1);
                          p=q+1; continue;
                        }
                      }
                else
                  { strncat(s,p,q-p+1);
                    p=q+1; continue;
                  }
                } /* while */

        return(1);
        }

static char *deriv(char *d,char *s)
        {
        char *summa();
        char *erotus();
        char *tulo();
        char *suhde();
        char *nelio();
        char *potenssi();
        char *sul();
        char op,d1[NFUNC],s1[NFUNC],s2[NFUNC];
        int p;

        sulutp(s);
        if (*s=='-' || *s=='+')
               { strcpy(s1,"0"); strcat(s1,s); strcpy(s,s1); }
        if (vakio(s)) {strcpy(d,"0"); return(d);}
        if (strcmp(s,MUUTTUJA1)==0) {strcpy(d,"1"); return(d);}
        if (strcmp(s,MMUUTTUJA)==0) {strcpy(d,"-1"); return(d);}
        p=hajoita(s);
        if (p==-1) {strcpy(d,"*** ( missing! ***");return(d);}
        if (p==-2) {strcpy(d,"*** ) missing! ***");return(d);}
        if (p!=0)
                {
                strcpy(s1,s);
                op=*(s+p);
                *(s1+p)=EOS;
                strcpy(s2,s1+p+1);
                sulutp(s1);
                sulutp(s2);
            switch (op)
                {
              case '+':
                return (summa(d,deriv(d,s1),deriv(d1,s2)));
              case '-':
                return (erotus(d,deriv(d,s1),deriv(d1,s2)));
              case '*':
                deriv(d1,s2); tulo(d1,d1,s1);
                deriv(d,s1); tulo(d,d,s2); return (summa(d,d,d1));
              case '/':
                deriv(d,s1); tulo(d,d,s2); deriv(d1,s2); tulo(d1,d1,s1);
                erotus(d,d,d1); return (suhde(d,d,nelio(s2)));
              case '^':
                if (nolla(s1)) {strcpy(d,s1); return(d);}
                if (nolla(s2)) {strcpy(d,"0"); return(d);}
                if (yksi(s2)) {deriv(d,s1); return(d);}
                if (kok(s2)) {muste_itoa(atoi(s2)-1,d,10); potenssi(d1,s1,d);
                              tulo(d1,d1,deriv(d,s1)); strcpy(d,s2);
                              tulo(d,d,d1); return(d);}
                if (vakio(s2)) {erotus(d,s2,"1"); potenssi(d1,s1,d);
                                strcpy(d,s2); tulo(d,d,d1); deriv(d1,s1);
                                tulo(d,d,d1); return(d);}
                strcpy(d,"log("); strcat(d,s1); strcat(d,")");
                tulo(d1,deriv(d1,s2),d);
                deriv(d,s1); suhde(d,d,s1); tulo(d,d,s2);
                summa(d1,d1,d); tulo(d,s,d1); return(d);

                }  /* end of switch(op) */
                }  /* end of op!=0 */

        else    /* funktiot */
                {
                p=0;
                strcpy(s1,s);
                while (*(s1+p)!='(') ++p;
                *(s1+p)=EOS;
                strcpy(s2,s+p+1);
                *(s2+strlen(s2)-1)=EOS;
                sulutp(s2);

        if ( !strcmp(s1,"sqr") || !strcmp(s1,"sqrt") )
                { strcpy(d,deriv(d,s2)); suhde(d,d,s);
                  return (suhde(d,d,"2"));}
        if ( !strcmp(s1,"log") || !strcmp(s1,"ln") )
                return (suhde(d,deriv(d,s2),s2));
        if ( !strcmp(s1,"exp") )
                return (tulo(d,s,deriv(d1,s2)));
        if ( !strcmp(s1,"sin") )
                return (tulo(d,deriv(d,s2),
                               strcat(strcpy(d1,"cos"),sul(s2,4))));
        if ( !strcmp(s1,"cos") )
                return (tulo(d,deriv(d,s2),
                               strcat(strcpy(d1,"-sin"),sul(s2,4))));
        if ( !strcmp(s1,"tan") )
                { strcpy(d1,"cos"); strcat(d1,sul(s2,4));
                  potenssi(d1,d1,"-2"); return (tulo(d,deriv(d,s2),d1));}
        if ( !strcmp(s1,"arctan") || !strcmp(s1,"atn") || !strcmp(s1,"atan"))
                { strcpy(d,s2); nelio(d); summa(d1,"1",d);
                  return (suhde(d,deriv(d,s2),d1));}

        if ( !strcmp(s1,"arcsin") || !strcmp(s1,"asin") )
                { strcpy(d,s2); nelio(d); erotus(d1,"1",d);
                  strcpy(d,d1); strcpy(d1,"sqrt("); strcat(d1,d);
                  strcat(d1,")");
                  return (suhde(d,deriv(d,s2),d1));}
        if ( !strcmp(s1,"arccos") || !strcmp(s1,"acos") )
                { strcpy(d,s2); nelio(d); erotus(d1,"1",d);
                  strcpy(d,d1); strcpy(d1,"-sqrt("); strcat(d1,d);
                  strcat(d1,")");
                  return (suhde(d,deriv(d,s2),d1));}
// 1/sqrt(2*3.141592653589793)=0.39894228040143
        if ( !strcmp(s1,"PHI") || !strcmp(s1,"Phi") )
//              { strcpy(d,s2); nelio(d);
//                strcpy(d1,"0.39894228040143*exp(-1/2*");
//                strcat(d1,d); strcat(d1,")");
//                return(tulo(d,deriv(d,s2),d1)); }
                return (tulo(d,deriv(d,s2),
                               strcat(strcpy(d1,"phi"),sul(s2,4))));

        if ( !strcmp(s1,"phi") )
                {
// printf("\ns=%s s1=%s s2=%s",s,s1,s2); getch();
                strcpy(d,"-"); strcat(d,sul(s2,2));
                tulo(d1,d,s);
                return(tulo(d,deriv(d,s2),d1));
                }
        /* tuntematon funktio */
                strcpy(d,s1); strcat(d,"'("); strcat(d,s2); strcat(d,")");
                return (tulo(d,d,deriv(d1,s2)));

                }
        return(NULL);
        }

static char *summa(char *r,char *s1,char *s2)
        {
        if ( strlen(s1)+strlen(s2) > NEND ) { strcpy(r,OF); return(r); }
        if (nolla(s1)) {strcpy(r,s2); return(r);}
        strcpy(r,s1);
        if (*s2=='-') {strcat(r,s2); return(r);}
        if (!nolla(s2)) {strcat(r,"+"); strcat(r,s2);}
        return (r);
        }

static char *erotus(char *r,char *s1,char *s2)
        {
        if ( strlen(s1)+strlen(s2) > NEND ) { strcpy(r,OF); return(r); }
        if (nolla(s2)) {strcpy(r,s1); return(r);}
        if (nolla(s1)) {strcpy(r,"-"); strcat(r,sul(s2,1)); return(r);}
        if (!strcmp(s1,s2)) {strcpy(r,"0"); return(r);}
        strcpy(r,s1); strcat(r,"-"); strcat(r,sul(s2,1)); return(r);
        }

static char *tulo(char *r,char *s1,char *s2)
        {
        if ( strlen(s1)+strlen(s2) > NEND ) { strcpy(r,OF); return(r); }
        if (nolla(s1)) {strcpy(r,s1); return(r);}
        if (yksi(s1)) {strcpy(r,s2); return(r);}
        if (myksi(s1)) {strcpy(r,"-"); strcat(r,sul(s2,1)); return(r);}
        if (nolla(s2)) {strcpy(r,s2); return(r);}
        if (yksi(s2)) {strcpy(r,s1); return(r);}
        if (myksi(s2)) {strcpy(r,s1); siir(sul(r,1),1); *r='-'; return(r);}
        if (!strcmp(s1,s2)) {strcpy(r,nelio(s1)); return(r);}

        strcpy(r,sul(s1,1)); strcat(r,"*"); strcat(r,sul(s2,1));
        return(r);
        }

static char *suhde(char *r,char *s1,char *s2)
        {
        if ( strlen(s1)+strlen(s2) > NEND ) { strcpy(r,OF); return(r); }
        if (nolla(s1)) {strcpy(r,s1); return(r);}
        if (yksi(s2)) {strcpy(r,s1); return(r);}
        if (myksi(s2)) {strcpy(r,s1); siir(sul(r,1),1); *r='-'; return(r);}
        if (!strcmp(s1,s2)) {strcpy(r,"1"); return(r);}

        strcpy(r,sul(s1,1)); strcat(r,"/"); strcat(r,sul(s2,2)); return(r);
        }

static char *nelio(char *s)
        {
        if (nolla(s)) return(s);
        if (yksi(s)) return(s);
        if (myksi(s)) {strcpy(s,"1"); return(s);}

        sul(s,2); strcat(s,"^2"); return(s);
        }

static char *potenssi(char *r,char *s1,char *s2)
        {
        if ( strlen(s1)+strlen(s2) > NEND ) { strcpy(r,OF); return(r); }
        if (nolla(s1)) {strcpy(r,s1); return(r);}
        if (yksi(s1)) {strcpy(r,s1); return(r);}
        if (nolla(s2)) {strcpy(r,"1"); return(r);}
        if (yksi(s2)) {strcpy(r,s1); return(r);}
        if (myksi(s2)) {strcpy(r,s1); siir(sul(r,2),2);
                        *r='1'; *(r+1)='/'; return(r);}
        strcpy(r,sul(s1,2)); strcat(r,"^"); strcat(r,sul(s2,3)); return(r);
        }

static int sulutp(char *s)  /* poistaa tarpeettomat sulut */
        {
        int h;
        while (liikaa(s))
                {
                for (h=1;*(s+h);++h)
                *(s+h-1)=*(s+h);
                *(s+h-2)=EOS;
                }
        return(1);
        }

static int liikaa(char *s)
        {
   //   int taso;
        int l,h,alin,t;
        if (*s!='(') return (0);
        l=strlen(s);
        if (*(s+l-1)!=')') return (0);
        alin=t=1;
        for (h=1;h<l-1;++h)
                switch (*(s+h))
                {
                case '(': ++t; break;
                case ')': --t; if (alin>t) alin=t; break;
                }
        return (alin);
        }

static int hajoita(char *s)
        {
        int alin,pos,h,t;
        t=0; alin=4; pos=0;
        for (h=0;*(s+h);++h)
                {
                switch (*(s+h))
                {
                case '(': ++t; break;
                case ')': --t; break;
                case '+':
                case '-': if (h==0) break;
                          if (t>0) break;
                          alin=1; pos=h;
                          break;
                case '*':
                case '/': if (t>0) break;
                          if (alin>=2) {alin=2; pos=h;}
                          break;
                case '^': if (t>0) break;
                          if (alin>=3) {alin=3; pos=h;}
                          break;
                }
                }
        if (t<0) return (-1);
        if (t>0) return (-2);
        return (pos);
        }

static char *sul(char *s,int u)
        {
        if (staso(s)<=u) suluin(s);
        return(s);
        }

static int suluin(char *s)
        {
        int h,l;
        l=strlen(s);
        for (h=l;h>=0;--h)
                *(s+h+1)=*(s+h);
        *s='('; *(s+l+1)=')'; *(s+l+2)=EOS;
        return(1);
        }

static int siir(char *s,int k)
        {
        int h;
        for (h=strlen(s);h>=0;--h)
                *(s+h+k)=*(s+h);
        return(1);
        }

static int staso(char *s)
        {
        int alin,h,t;
        t=0; alin=4;
        for (h=0;*(s+h);++h)
     switch (*(s+h))
        {
        case '(': ++t; break;
        case ')': --t; break;
        case '+':
        case '-': if (t==0) return (1); break;
        case '*':
        case '/': if (t>0) break;
                  if (alin>2) alin=2;
                  break;
        case '^': if (t>0) break;
                  if (alin>3) alin=3;
                  break;
        }
        return (alin);
        }

static int vakio(char *s)
        {
        if (strchr(s,MUUTT)==NULL) return (1);
        return (0);
        }

static int nolla(char *s)
        {
        if (strcmp(s,"0")==0) return (1);
        return (0);
        }

static int yksi(char *s)
        {
        if (strcmp(s,"1")==0) return (1);
        return (0);
        }

static int myksi(char *s)
        {
        if (strcmp(s,"-1")==0) return (1);
        return (0);
        }

static int kok(char *s)    /* onko s kokonaisluku? */
        {
        int h=0;
        if ((*s=='-') || (*s=='+')) h=1;
        for (;*(s+h);++h)
                if ((*(s+h)<'0') || (*(s+h)>'9')) return (0);
        return (1);
        }

/* hess.c 22.7.85/SM (10.9.85)
   +grad +cholinv +Rsquare
*/

static int hess(double a[],double H[],int m)
        {
        double earvo();
        double b,w,der[N];
        int h,i,j,k;
        int n=obs2-obs1+1;

        for (i=0; i<m; ++i) for (j=0; j<=i; ++j) H[m*i+j]=0;

       if (!logd)
         {
         for (k=obs1; k<=obs2; ++k)
            {
            b=earvo(k,laus[0],a,cc,xx)-earvo(k,laus[1],a,cc,xx);
            w=earvo(k,laus[2],a,cc,xx);
//          h=m+2;
            h=m+n_laus-1;
            for (i=0; i<m; ++i)
                {
                der[i]=earvo(k,laus[n_laus+i],a,cc,xx);
                for (j=0; j<=i; ++j)
                    if (weightind==1)
          H[m*i+j]+=w*n/weightsum*(b*earvo(k,laus[++h],a,cc,xx)+der[i]*der[j]);
                    else
          H[m*i+j]+=b*earvo(k,laus[++h],a,cc,xx)+der[i]*der[j];
                }
            }

         for (i=0; i<m; ++i) for (j=0; j<=i; ++j) H[m*j+i]=H[m*i+j]*=2;
         }

        else /* logdensity */
         {
         for (k=obs1; k<=obs2; ++k)
            {
//          save_update1(k); // 8.5.2004
            w=earvo(k,laus[2],a,cc,xx);
//          h=m+2;
            h=m+n_laus-1;
            for (i=0; i<m; ++i)
                {
                for (j=0; j<=i; ++j)
                    if (weightind==1)
          H[m*i+j]-=w*n/weightsum*earvo(k,laus[++h],a,cc,xx);
                    else
          H[m*i+j]-=earvo(k,laus[++h],a,cc,xx);
                }
            }

         for (i=0; i<m; ++i)
             for (j=0; j<=i; ++j)
                 { b=H[m*i+j]; H[m*j+i]=b; }

         }
        return(1);
        }

static int grad(double g[],double a[])
        {
        double earvo();
        int i,j;
        double b,w;
        int n=obs2-obs1+1;

       if (!logd)
         {
         for (i=0; i<na; ++i) g[i]=0;
         for (j=obs1; j<=obs2; ++j)
            {
            b=earvo(j,laus[0],a,cc,xx)-earvo(j,laus[1],a,cc,xx);
            if (weightind==1)
                {
                w=earvo(j,laus[2],a,cc,xx)*n/weightsum;
                for (i=0; i<na; ++i)
                    g[i]+=2*w*b*earvo(j,laus[n_laus+i],a,cc,xx);
                }
            else
                {
                for (i=0; i<na; ++i)
                    g[i]+=2*b*earvo(j,laus[n_laus+i],a,cc,xx);
                }
            }
         }

       else  /* logdensity */
         {
         for (i=0; i<na; ++i) g[i]=0;
         for (j=obs1; j<=obs2; ++j)
            {
            if (weightind==1)
                {
                w=earvo(j,laus[2],a,cc,xx)*n/weightsum;
                for (i=0; i<na; ++i)
                    g[i]-=w*earvo(j,laus[n_laus+i],a,cc,xx);
                }
            else
                {
                for (i=0; i<na; ++i)
                    g[i]-=earvo(j,laus[n_laus+i],a,cc,xx);
                }
            }


         }
        return(1);
        }

static int cholinv(double a[],int n)
        {
        int i,j,k,i1,j1;
        double z,x,y;
        y=0.0; // formally
        for (i=0; i<n; ++i)
            {
            i1=i+1;
            for (j=i; j<n; ++j)
                {
                j1=j+1;
                x=a[n*i+j];
                for (k=i-1; k>=0; --k)
                    x-=a[n*j1+k]*a[n*i1+k];
                if (j==i)
                    {
                    if (x<=0) return(-1);
                    a[n*i1+i]=y=1/sqrt(x);
                    }
                else a[n*j1+i]=x*y;
                }
            }

        for (i=0; i<n; ++i)
        for (j=i+1; j<n; ++j)
            {
            z=0;
            j1=j+1;
            for (k=j-1; k>=i; --k)
                z-=a[n*j1+k]*a[n*(k+1)+i];
            a[n*j1+i]=z*a[n*j1+j];
            }
        for (i=0; i<n; ++i)
        for (j=i; j<n; ++j)
            {
            z=0;
            j1=n;
            for (k=j+1; k<=j1; ++k)
                z+=a[n*k+j]*a[n*k+i];
            a[n*(j+1)+i]=z;
            }
        return(1);
        }

static double Rsquare()
        {
        double earvo();
        double rss();
        double y,w,sy,sy2,R2;
        int j;
        int n;

        n=obs2-obs1+1;
        sy=0; sy2=0;
        for (j=obs1; j<=obs2; ++j)
            {
            y=earvo(j,laus[0],a,cc,xx);
            if (weightind==1)
                {
                w=earvo(j,laus[2],a,cc,xx)*n/weightsum;
                sy+=w*y; sy2+=w*y*y;
                }
            else
                {
                sy+=y; sy2+=y*y;
                }
            }

        R2=1-rss(a)/(sy2-sy*sy/n);
        if (R2<0) return(0);
        if (R2>1) return(1);
        return(R2);
        }

static int save_update1(int k)
    {
    int i;
    double w;
    extern int saveind,save_var[],em2;

    if (saveind==0) return(1);
    for (i=0; i<saveind; ++i)
        {
        w=earvo(k,laus[3+i],a,cc,xx);
        *(xx+(k-1)*em2+save_var[i])=w;
        }
    return(1);
    }

/* dfp1.c 22.7.85/SM (11.10.1986)

*/

static int davidon(int nd)
/* nd=1: 1. derivaatat nd=2: Myˆs 2. derivaatat k‰ytˆss‰ */
        {
        double rss();
        double wsum();
        double Rsquare();

        int grad(double *,double *);
        int i,j,nf,inv;
        double g[N];
        double s,c;

        double step=1;
        int maxnf=1000;
        int est_interrupt=0;
        int n=obs2-obs1+1;

        char rivi[LLENGTH];
        double r2=0.0;
        double aa;
        char t1[32],t2[32];

        output_open(eout);
   sur_print("Optimization by DFP algorithm:\n");
        if (weightind==1) { weightsum=wsum();
                            if (weightsum<=0) {
                                                sur_print("\nSum of weights <=0!");
                                                WAIT;
                                              }
                          }
        rss0();
        nf=dfp(a,na,rss,grad,step,maxnf,&est_interrupt);
        if (nd==2)
            { hess(a,HH,na); nf+=na*(na+1)/2; }
        else
            {
            for (i=0; i<na; ++i)
                {
                g[i]=fabs(a[i]/10000.0);
                if (g[i]<1e-8) g[i]=1e-8;
                }
            numhess(a,HH,na,g); nf+=(na+1)*(na+1);
            }
        inv=cholinv(HH,na);

        s=rss(a); if (logd) c=1; else c=2*s/(n-na);

        sprintf(rivi,"Estimated parameters of model %s:",malli);
        eoutput(rivi);
        if (est_interrupt) stopdisp(rivi);

        for (i=0; i<na; ++i)
            {
            s_a[i]=sqrt(c*HH[na*(i+1)+i]);
            if (eaccuracy)
              {
              fnconv(a[i],eaccuracy+2,t1);
              aa=sqrt(c*HH[na*(i+1)+i]);
              fnconv(aa,eaccuracy+2,t2);
              sprintf(rivi,"%s=%s (%s)",aname[i],spois(t1),spois(t2));
              }
            else
              sprintf(rivi,"%s=%G (%G)",aname[i],a[i],sqrt(c*HH[na*(i+1)+i]));
            eoutput(rivi);
            }

        if (logd)
            sprintf(rivi,"n=%d log(L)=%f nf=%d",n,-s,nf);
        else
            {
            r2=Rsquare();
            if (r2<1e-5)
                sprintf(rivi,"n=%d rss=%f nf=%d",n,s,nf);
            else
                sprintf(rivi,"n=%d rss=%f R^2=%.5f nf=%d",n,s,r2,nf);
            }
        eoutput(rivi);
        for (i=0; i<na; ++i) for (j=0; j<na; ++j)
                HH[na*i+j]=HH[na*(i+1)+j];
        for (i=0; i<na; ++i) for (j=0; j<=i; ++j)
                HH[na*j+i]=HH[na*i+j];

        output_close(eout);

        save_matrices(aname,na,a,s_a,n,s,r2); // 12.4.2005

        if (na>1)
            {
            i=corrnorm(HH,na);
            if (i<0)
                {
                sur_print("\nObviously not a true solution!");
                WAIT; return(-1);
                }
            if (results>=70)
                corrp(HH,na,aname,c3,7,3,"Correlations:");
            corr_save(HH,a,s_a,na,aname);
            }
        return(1);
        }

static char *spois(char *s)
        {
        while (*s && *s==' ') ++s;
        return(s);
        }

/* dfp2.c 22.7.85/SM (11.10.1986)

*/
static int dfp(double x[],int m,double (*f)(),int (*grad)(double *,double *),double step,int maxnf,int *stop)
        {
        double fs[N],s2[N],s3[N],s4[N];
        double x1[N],            x4[N];
        double step1,step2,l1;
        double y,y1,y2,y3,g2,g3;
        double eps=1e-10;         /* gradientti */
        double eps2=1e-20;        /* askel */
        double eps3=1e-6;         /* askel */
        double eps4=1e-20;        /* gamma'*sigma */
        int kesken=1;
        int haku;
        int i,j,n,nit;
        double a,a1,a2;

        n=0; nit=0;
        for (i=0; i<m; ++i)
            {
            for (j=0; j<m; ++j) HH[m*i+j]=0;
            HH[m*i+i]=1;
            }     /* HH=I */

        y=(*f)(x); ++n;
        (*grad)(fs,x); n+=m;

        while (kesken)
      {
        y1=y;
        step1=step2=step;
        ++nit; sprintf(sbuf2,"Iteration %d:\n",nit); sur_print(sbuf2);
        a1=0;
        for (i=0; i<m; ++i) { x1[i]=x[i]; s3[i]=fs[i]; a=0;
                              for (j=0; j<m; ++j) a-=HH[m*i+j]*fs[j];
                              s2[i]=a; a1+=a*a;
                            }
        if (a1<eps) { kesken=0; continue; }
        a1=sqrt(a1);
        sur_print("Direction: ");
        for (i=0; i<m; ++i) { s2[i]/=a1; sprintf(sbuf2,"%f ",s2[i]); sur_print(sbuf2); }
           sur_print("\n");
        y3=y1; g3=0; for (i=0; i<m; ++i) g3+=fs[i]*s2[i];
        if (g3>0) { sur_print("Gradient>0!"); WAIT; kesken=0; continue; }

        haku=1;
        while (haku)
            {
            y2=y3; g2=g3;
            for (i=0; i<m; ++i) x[i]+=step1*s2[i];
            y=(*f)(x); ++n;
            (*grad)(fs,x); n+=m;
            y3=y; g3=0; for (i=0; i<m; ++i) g3+=fs[i]*s2[i];
            if (g3>=0 || y3>=y2) { haku=0; continue; }
            step1*=4; step*=4; sprintf(sbuf2,"S=%f\n",step); sur_print(sbuf2);
            step2+=step1;
            } /* haku */

        haku=1;
        while (haku)
            {
            if (step1<eps) { haku=0; kesken=0; continue; }
            a=3*(y2-y3)/step1+g2+g3;
            a1=sqrt(a*a-g2*g3);
            l1=step1*(g3+a1-a)/(g3-g2+2*a1); sprintf(sbuf2,"L=%f\n",l1); sur_print(sbuf2);
            for (i=0; i<m; ++i) x[i]-=l1*s2[i];
            step2-=l1;
            y=(*f)(x); ++n;
            (*grad)(fs,x); n+=m;
            if (y<=y2 && y<=y3) { y2=y; haku=0; continue; }
            step/=4; sprintf(sbuf2,"S=%f\n",step); sur_print(sbuf2);
            if (step<eps2) { haku=0; kesken=0; continue; }
            if (y3<y2)
                {
                for (i=0; i<m; ++i) x[i]+=l1*s2[i];
                step2+=l1; y=y3;
                y2=y;
                y=(*f)(x); ++n;         /* tarpeeton, jos talletettu? */
                (*grad)(fs,x); n+=m;
                haku=0; continue;
                }
            g3=0; for (i=0; i<m; ++i) g3+=fs[i]*s2[i];
            if (g3<0 && step<eps3) { haku=0; kesken=0; continue; }
            y3=y; step1-=l1;
            } /* haku */

            if (kesken==0) continue;

            for (i=0; i<m; ++i) { sprintf(sbuf2,"%f ",x[i]); sur_print(sbuf2); }
            sprintf(sbuf2,"y=%g\n",y); sur_print(sbuf2);

            for (i=0; i<m; ++i) s3[i]=fs[i]-s3[i];
            a=0; for (i=0; i<m; ++i)
                {
                x4[i]=x[i]-x1[i]; a+=x4[i]*s3[i];
                }
            if (fabs(a)<eps4) { kesken=0; continue; }
            a1=0;
            for (i=0; i<m; ++i)
                for (j=0; j<m; ++j) a1+=s3[i]*HH[i+m*j]*s3[j];
            if (fabs(a1)<eps4) { kesken=0; continue; }
            for (i=0; i<m; ++i)
                {
                a2=0;
                for (j=0; j<m; ++j) a2+=HH[m*i+j]*s3[j];
                s4[i]=a2;
                }
            for (i=0; i<m; ++i)
            for (j=0; j<=i; ++j)
                HH[m*i+j]=HH[m*j+i]+=x4[i]*x4[j]/a-s4[i]*s4[j]/a1;

            y=y2; step=step2;
            if (sur_kbhit()) { kesken=0; sur_getch(); *stop=1; }

      } /* kesken */

        return (n);
        }

/* newt.c 22.7.85/SM (21.8.1986)
*/

// extern double as[];  // 12.4.2005  // 18.6.2011 ei k‰ytˆss‰

static int newton(int linind,int type)
// int linind; /* 1=linear model */
// int type; /* 1=pure 2=modified Newton */
        {
        double rss();
        double wsum();
        double Rsquare();

        int grad();
        int i,nf;
//      double g[N];
        double s,c;

        double step=1;
        int maxnf=1000;
        int est_interrupt=0;
        int n=obs2-obs1+1;

        char rivi[LLENGTH];
        double r2=0.0;
        extern char *spois();
        double aa;
        char t1[32],t2[32];

        output_open(eout);
   sur_print("Optimization by Newton-Raphson algorithm:\n");
        if (weightind==1) { weightsum=wsum();
                            if (weightsum<=0) {
                                                sur_print("\nSum of weights <=0!");
                                                WAIT;
                                              }
                          }
        if (linind!=1) rss0();

        nf=newt(a,na,rss,grad,step,maxnf,linind,&est_interrupt,type);

        s=rss(a); if (logd) c=1; else c=2*s/(n-na);

        sprintf(rivi,"Estimated parameters of model %s:",malli);
        eoutput(rivi);
        if (est_interrupt) stopdisp(rivi);

        for (i=0; i<na; ++i)
        {
        s_a[i]=sqrt(c*HH[i+na*i]);
        if (eaccuracy)
          {
          fnconv(a[i],eaccuracy+2,t1);
          aa=sqrt(c*HH[i+na*i]);
          fnconv(aa,eaccuracy+2,t2);
          sprintf(rivi,"%s=%s (%s)",aname[i],spois(t1),spois(t2));
          }
        else
          sprintf(rivi,"%s=%G (%G)",aname[i],a[i],sqrt(c*HH[i+na*i]));
        eoutput(rivi);
        }
        if (logd)
            sprintf(rivi,"n=%d log(L)=%f nf=%d",n,-s,nf);
        else
            {
            r2=Rsquare();
            if (r2<1e-5)
                sprintf(rivi,"n=%d rss=%f nf=%d",n,s,nf);
            else
                sprintf(rivi,"n=%d rss=%f R^2=%.5f nf=%d",n,s,r2,nf);
            }
        eoutput(rivi);
        output_close(eout);

        save_matrices(aname,na,a,s_a,n,s,r2); // 12.4.2005

        if (na>1)
            {
            i=corrnorm(HH,na);
            if (i<0)
                {
                sur_print("\nObviously not a true solution!");
                WAIT; return(-1);
                }
            if (results>=70)
                corrp(HH,na,aname,c3,7,3,"Correlations:");
            corr_save(HH,a,s_a,na,aname);
            }
        return(1);
        }

static int corr_save(double *R,double *a,double *s,int m,char *names[])
    {
    int i,k,len;

    for (i=0; i<8*m; ++i) sbuf2[i]=' ';
    for (i=0; i<m; ++i)
        {
        len=strlen(names[i]); if (len>8) len=8;
        for (k=0; k<len; ++k) sbuf2[8*i+k]=names[i][k];
        }
    matrix_save("EST_CORR.M",R,m,m,sbuf2,sbuf2,8,8,-1,
        "R_of_parameter_estimates",0,0);
    for (i=0; i<m; ++i)
        {
        R[i]=a[i]; R[m+i]=s[i];
        }
    matrix_save("EST_PAR.M",R,m,2,sbuf2,"param   std.err.",8,8,-1,
        "Parameters_and_std.errors",0,0);

    return(1);
    }

/*
mprint(a,m,n)
double *a; int m,n;
        {
        int i,j;
        for (i=0; i<m; ++i)
            { printf("\n"); for (j=0; j<n; ++j) printf("%g ",a[i+m*j]); }
        printf("\n"); getch();
        }
*/

static int newt(double x[],int m,double (*f)(),int (*grad)(),double step,int maxnf,int linind,int *stop,int type)
// double x[];
// int m;
// double (*f)();
// double (*grad)();
// double step;
// int    maxnf;
// int    linind;
// int    *stop;
// int    type; /* 1=pure 2=modified Newton */
        {
        double fs[N],s2[N],s3[N];  //  s4[N];
        double x1[N];    //      x4[N];
        double step1,step2,l1;
        double y,y1,y2,y3,g2,g3;
        double eps=1e-10;         /* gradientti */
        double eps2=1e-20;        /* askel */
        double eps3=1e-6;         /* askel */
//      double eps4=1e-20;        /* gamma'*sigma */
        int kesken=1;
        int haku;
        int i,j,n,nit;
        double a,a1;

        n=0; nit=0;

        hess(x,HH,m); n+=m*(m+1)/2;

        if (type==2)
            modified_inv_hess(HH,m);
        else /* type=1 */
            {
            i=cholinv(HH,m);
            for (i=0; i<m; ++i) for (j=0; j<m; ++j)
                        HH[m*i+j]=HH[m*(i+1)+j];
            for (i=0; i<m; ++i) for (j=0; j<=i; ++j)
                        HH[m*j+i]=HH[m*i+j];
            }
        y=(*f)(x); ++n;
        (*grad)(fs,x); n+=m;

        while (kesken)
      {
        y1=y;
        step1=step2=step;
        ++nit; sprintf(sbuf2,"Iteration %d:\n",nit); sur_print(sbuf2);
        a1=0;
        for (i=0; i<m; ++i) { x1[i]=x[i]; s3[i]=fs[i]; a=0;
                              for (j=0; j<m; ++j) a-=HH[m*i+j]*fs[j];
                              s2[i]=a; a1+=a*a;
                            }
        if (a1<eps) { kesken=0; continue; }
        if (linind==1)
                {
                for (i=0; i<m; ++i) x[i]+=s2[i];
                y=(*f)(x); ++n;
                kesken=0; continue;
                }
        a1=sqrt(a1);
        sur_print("Direction: ");
        for (i=0; i<m; ++i) { s2[i]/=a1; sprintf(sbuf2,"%f ",s2[i]); sur_print(sbuf2); }
            sur_print("\n");
        y3=y1; g3=0; for (i=0; i<m; ++i) g3+=fs[i]*s2[i];
        if (g3>0) { sur_print("Gradient>0!"); WAIT; kesken=0; continue; }

        haku=1;
        while (haku)
            {
            y2=y3; g2=g3;
            for (i=0; i<m; ++i) x[i]+=step1*s2[i];
            y=(*f)(x); ++n;
            (*grad)(fs,x); n+=m;
            y3=y; g3=0; for (i=0; i<m; ++i) g3+=fs[i]*s2[i];
            if (g3>=0 || y3>=y2) { haku=0; continue; }
            step1*=4; step*=4; sprintf(sbuf2,"S=%f\n",step); sur_print(sbuf2);
            if (step>1e10) { sur_print("\nNo convergence!");
                             WAIT;
                             s_end(argv1);
                           }
            step2+=step1;
            } /* haku */

        haku=1;
        while (haku)
            {
            if (step1<eps) { haku=0; kesken=0; continue; }
            a=3*(y2-y3)/step1+g2+g3;
            a1=sqrt(a*a-g2*g3);
            l1=step1*(g3+a1-a)/(g3-g2+2*a1); sprintf(sbuf2,"L=%f\n",l1); sur_print(sbuf2);
            for (i=0; i<m; ++i) x[i]-=l1*s2[i];
            step2-=l1;
            y=(*f)(x); ++n;
            (*grad)(fs,x); n+=m;
            if (y<=y2 && y<=y3) { y2=y; haku=0; continue; }
            step/=4; sprintf(sbuf2,"S=%f\n",step); sur_print(sbuf2);
            if (step<eps2) { haku=0; kesken=0; continue; }
            if (y3<y2)
                {
                for (i=0; i<m; ++i) x[i]+=l1*s2[i];
                step2+=l1; y=y3;
                y2=y;
                y=(*f)(x); ++n;         /* tarpeeton, jos talletettu? */
                (*grad)(fs,x); n+=m;
                haku=0; continue;
                }
            g3=0; for (i=0; i<m; ++i) g3+=fs[i]*s2[i];
            if (g3<0 && step<eps3) { haku=0; kesken=0; continue; }
            y3=y; step1-=l1;
            } /* haku */

            if (kesken==0) continue;

            for (i=0; i<m; ++i) { sprintf(sbuf2,"%f ",x[i]); sur_print(sbuf2); }
            sprintf(sbuf2,"y=%g\n",y); sur_print(sbuf2);
            hess(x,HH,m); n+=m*(m+1)/2;

            if (type==2)
                modified_inv_hess(HH,m);
            else /* type=1 */
                {
                i=cholinv(HH,m);
                for (i=0; i<m; ++i) for (j=0; j<m; ++j)
                            HH[m*i+j]=HH[m*(i+1)+j];
                for (i=0; i<m; ++i) for (j=0; j<=i; ++j)
                            HH[m*j+i]=HH[m*i+j];
                }

            y=y2; step=step2;
            if (sur_kbhit()) { kesken=0; sur_getch(); *stop=1; }

      } /* kesken */

        return (n);
        }

static double hh2[N*N],hd[N],he[N];
#define EPS 1e-16
#define TOL (1e-300)/EPS

static int modified_inv_hess(double *HH,int m)
        {
        int i,j,k;
        double s;

        for (i=0; i<=m*m; ++i) hh2[i]=HH[i];
        mat_tred2(hd,he,hh2,m,TOL);
        mat_tql2(hd,he,hh2,m,EPS,30);
/*************
        sur_print("\n");
        for (i=0; i<m; ++i)
            { sprintf(sbuf,"%g ",hd[i]); sur_print(sbuf); }
 getch();
****************/
        for (i=0; i<m; ++i)
            if (hd[i]<=0.0) hd[i]=-1.0/(hd[i]-0.1); else hd[i]=1.0/hd[i];
        for (i=0; i<m; ++i)
        for (j=0; j<=i; ++j)
            {
            s=0.0;
            for (k=0; k<m; ++k) s+=hd[k]*hh2[i+m*k]*hh2[j+m*k];
            HH[i+m*j]=s;
            }
        for (i=0; i<m; ++i)
        for (j=0; j<i; ++j) HH[j+m*i]=HH[i+m*j];
        return(1);
        }
/* res.c 12.10.1986/SM (12.10.1989)
    residuals()
*/

static int residuals(char *data)
        {
        extern double earvo();
        int i,k,h;
        int resvar,gvar,fvar;
        long j;
        double x;

        i=data_to_write(data,&sdata);
        if (i<0) { sprintf(sbuf2,"\nCannot write residuals etc. in %s!",data);
                   sur_print(sbuf2); WAIT; return(-1);
                 }

        vv=(int *)muste_malloc(sdata.m*sizeof(int));
        if (vv==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<sdata.m; ++i) vv[i]=sdata.v[i];
        i=mask(&sdata); if (i<0) return(-1);

        resvar=activated(&sdata,'R');
        fvar=activated(&sdata,'F');
        gvar=activated(&sdata,'G');
        for (i=0; i<sdata.m; ++i) sdata.v[i]=vv[i];
        if (resvar<0 && fvar<0 && gvar<0 && saveind==0) return(-1);

        sur_print("\nSaving ");

        if (resvar>=0) { sprintf(sbuf2,"\nresiduals as variable %.8s...",
                                        sdata.varname[resvar]); sur_print(sbuf2); }
        if (fvar>=0) { sprintf(sbuf2,"\npredicted values of model as variable %.8s...",
                                        sdata.varname[fvar]); sur_print(sbuf2); }
        if (gvar>=0) { sprintf(sbuf2,"\nlefthandside values of model as variable %.8s...",
                                        sdata.varname[gvar]); sur_print(sbuf2); }
        k=0;
        for (j=sdata.l1; j<=sdata.l2; ++j)
            {
            if (unsuitable(&sdata,j)) continue;

            for (i=0; i<em2; ++i)     /* 12.10.1989 puuttuvien ohitus */
                {
                data_load(&sdata,j,sdata.v[i],&x);
                if (x==MISSING8) break;
                }
            if (i<em2) continue;

            ++k;
            if (k<obs1 || k>obs2) continue;
            if (est_disp_all) { sprintf(sbuf2," %ld",j); sur_print(sbuf2); }

            if (resvar>=0)
                {
                x=earvo(k,laus[0],a,cc,xx)-earvo(k,laus[1],a,cc,xx);
                i=data_save(&sdata,j,resvar,x);
                if (i<0) return(-1);
                }
            if (fvar>=0)
                {
                x=earvo(k,laus[1],a,cc,xx);
                i=data_save(&sdata,j,fvar,x);
                if (i<0) return(-1);
                }
            if (gvar>=0)
                {
                x=earvo(k,laus[0],a,cc,xx);
                i=data_save(&sdata,j,gvar,x);
                if (i<0) return(-1);
                }

            if (saveind)
                {
                for (h=0; h<saveind; ++h)
                    {
                    x=earvo(k,laus[3+h],a,cc,xx);
                    i=data_save(&sdata,j,save_var2[h],x);
                    if (i<0) return(-1);
                    }
                }

            }
        return(1);
        }

/* DER.C 13.7.85/SM (14.9.85)
*/
static int op_der()
        {
        int line,p,cline;
        char x[LLENGTH];
        char der[1024],s[LLENGTH];
        char wordi2[LLENGTH]; // RS ADD

        if (g<=1) {
                  sur_print("\nIncomplete DER operation!");
                  sur_print("\nSee DER?                 ");
                  WAIT; return(1);
                  }

        if (g==2) strcpy(wordi2,"x"); else strcpy(wordi2,word[2]); // RS CHA strcpy(word[2],"x");        
        strcpy(s,word[1]);      
        deri(der,s,wordi2); // RS CHA deri(der,s,word[2]);

        cline=r1+r-1;
        strcpy(x,"Derivative of "); strcat(x,word[1]);
        edwrite(space,cline+1,1);
        edwrite(x,cline+1,2);
        strcpy(x,"with respect to "); strcat(x,wordi2); strcat(x," is"); // RS CHA word[2]
        edwrite(space,cline+2,1);
        edwrite(x,cline+2,2);
        line=cline+2; p=0;
        while (p<strlen(der))
            {
            if (strlen(der+p)>c3-4)    { strncpy(x,der+p,c3-4);
                                         *(x+c3-4)=EOS; p+=c3-4;
                                       }
            else                       { strcpy(x,der+p); p=strlen(der);
                                       }

            ++line;
            edwrite(space,line,1);
            edwrite(x,line,5);
            }
        return(1);
        }

/* grid.c 5.9.1998/SM (5.9.1998)

*/

static int grid_search()
        {
        double rss();
        double wsum();
        double Rsquare();
     // double maxstep, minstep, initstep;
        int i,j,nf;
     // double av[N];
     // char *sana[2]; int g;
        char rivi[LLENGTH];
        double s;   // c;
     // int inv;
        int n=obs2-obs1+1;
     // double r2;
     // double step2[N];
     // int stop;
        char y[LNAME],*osa[3];
        double amin[N],amax[N],step[N];
        double a_opt[N], s_opt;
        int count;
        long nn,cn;
        double bb,bb0;
        time_t timecount1,timecount2;
        double time_left;
        struct tm *endtime;
        int row,col;

        nn=1L;
        for (j=0; j<na; ++j)
            {
            i=spfind(aname[j]);
            if (i<0) { a_virhe(aname[j]); return(-1); }
            strcpy(y,spb[i]);
            i=split(y,osa,3);
            if (i<3) { a_virhe(aname[j]); return(-1); }
            amin[j]=a[j]=atof(osa[0]);
            amax[j]=atof(osa[1]);
            step[j]=atof(osa[2]);
            nn*=(int)((amax[j]-amin[j])/step[j]+1.5);
            }

        sprintf(sbuf2,"\nGrid search: %d parameters, %ld combinations",
                                      na,nn);
        sur_print(sbuf2);

        time(&timecount1);

        output_open(eout);
        weightsum=wsum();

        sur_print("\n");
        sur_cursor_position(&row,&col);

        s_opt=1e100; count=0; cn=0L; bb0=0.0;
        while (1)
            {
      /*    if (!logd) s=rss(a); else s=-rss(a); */
            s=rss(a);
            if (s<s_opt)
                {
                for (j=0; j<na; ++j) a_opt[j]=a[j];
                s_opt=s;
                }
            for (j=0; j<na; ++j)
                {
                a[j]+=step[j];
                if (a[j]<=amax[j]) break; else a[j]=amin[j];
                }
            if (j==na) break;

            ++cn;
            bb=100.0*(double)cn/(double)nn;
            if (bb-(int)bb<bb0-(int)bb0)
                {
                i=(int)bb;
                time(&timecount2);
 sur_locate(row,col);
 time_left=(timecount2-timecount1)*(100.0-(double)i)/(double)i;
              sprintf(sbuf2,"%d%% done, %5.1f minutes left,",(int)bb,time_left/60.0);
                sur_print(sbuf2);
 timecount2+=(time_t)time_left;
 endtime=localtime(&timecount2);
 sprintf(sbuf2,"  Ready on %s",asctime(endtime)); sur_print(sbuf2);
 sur_locate(row,col);
                }
            bb0=bb;

            ++count; if (count==1000) count=0;
            if (sur_kbhit())
                {
                i=sur_getch(); if (i=='.') prind=1-prind;
                if (i=='x' || i=='X') break;
                }
            if (prind && count==0)
                {
                sprintf(sbuf2,"\nCurrent optimum value %f (Interrupt by 'X')",s_opt);
                sur_print(sbuf2);
                sur_print("\nParameters:");
                i=0;
                for (j=0; j<na; ++j) i+=sprintf(sbuf2+i," %f",a_opt[j]);
                sur_print(sbuf2);

                sprintf(sbuf2,"\nCurrent value         %f",s);
                sur_print(sbuf2);
                sur_print("\nParameters:");
                i=0;
                for (j=0; j<na; ++j) i+=sprintf(sbuf2+i," %f",a[j]);
                sur_print(sbuf2);
                }
            }

//      nf=count;
        nf=nn; // 18.5.2004
        sprintf(rivi,"Estimated parameters of model %s:%c",malli,EOS);
        eoutput(rivi);
        for (i=0; i<na; ++i)
        {
        sprintf(rivi,"%s=%G",aname[i],a_opt[i]);
        eoutput(rivi);
        }
        if (logd)
            sprintf(rivi,"n=%d log(L)=%f nf=%d",n,-s_opt,nf);
        else if (criterion==2.0)
            {
            sprintf(rivi,"n=%d rss=%f nf=%d",n,s_opt,nf);
            }
        else
           sprintf(rivi,"n=%d Norm L%G: min=%f nf=%d",n,criterion,s_opt,nf);
        eoutput(rivi);
        output_close(eout);
        return(1);
        }

static int a_virhe(char *s)
        {
        sprintf(sbuf2,"\nError in specification %s",s);
        sur_print(sbuf2); WAIT;
        return(1);
        }

/* est_m.c 12.4.2005 (12.4.2005)

*/


#define ERC 128
#define N_LINES 9
#define N_RG 8

extern char *word[];

static int save_matrices(char *aname[],int na,double *a,double *s,int n,double rss,double r2)
    {
 // int i;
    char label[8*N_RG+1];
    char text[N_LINES*ERC+1];
    char text1[50];
    double et[N_RG];

    et[0]=(double)n; et[1]=(double)na; et[2]=(double)(n-na);
    et[3]=rss; et[4]=rss/et[2]; et[5]=et[4]; et[6]=sqrt(r2);
    et[7]=r2;

//  for (i=0; i<LLENGTH; ++i) space[i]=' '; 18.6.2011 globaali

    strcpy(label,"n       k       df      SSE     MSE     Resvar  ");
    strcat(label,"R       R2      ");

    *text=EOS;
    sprintf(text1,"ESTIMATE statistics from data %s",word[1]);
    n_strcat(text,ERC,text1);
    n_strcat(text,ERC,"/n: # of cases");
    n_strcat(text,ERC,"/k: # of estimated parameters");
    n_strcat(text,ERC,"/df: n-k");
    n_strcat(text,ERC,"/SSE: residual sum of squares");
    n_strcat(text,ERC,"/MSE: mean square error");
    n_strcat(text,ERC,"/Resvar: MSE");
    n_strcat(text,ERC,"/R: multiple correlation coefficient");
    n_strcat(text,ERC,"/R2: R^2");

    matrix_save0("EST.M",et,N_RG,1,label,"ESTIMATE",8,8,-1,"EST.M",
                  N_LINES,0,text);

    return(1);
    }

static int n_strcat(char *x,int len,char *s)
    {
    strcat(x,s); strncat(x,space,len-strlen(s));
    return(1);
    }
