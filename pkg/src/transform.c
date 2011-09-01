/* _transfo.c 25.4.1988/SM (31.5.1988) (25.3.1997)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static int l_virhe;
static SURVO_DATA d;
static int prind=1;
static char lauseke[LLENGTH];
static double xarvo,yarvo;
static int *v;
static char letter;
static char type;
static int muuttuja_mukana;
static int cent_std; /* BY CENTERING or BY STD */
static int special;
static int df;

/* #DISTR distr() varten */
static double *aa;
static char *rlab,*clab;
static int rdim,cdim,lr,lc,mtype;
static char expr[LLENGTH];
static char matname[LNAME];
static int jj; // hav.indeksi globaaliksi 17.3.2010 (lisämuuttujat lausekkeessa)

static int centstd();
static int special_transf();
static int init_rnd();
static int tr_uniform();
static int distr();
static int bin_search(double a,double *tab,int n);
static int markov();
static int tr_linear();
static int tr_diff();
static int not_enough_memory();
static int lue_muuttujan_arvo(char *s,double *py);
static int laske(char *lauseke,double *y);
static double luku(char *sana,int len);
static double oper(double x1,double x2,char laji);
static double power(double x,double y);
// static double round(double x);
static int supista(int *t,double opnd[],char op[],int v[]);
static double funktio(char *s,double x);
static int f_tuntematon(char *s);
// static int arg_virhe(char *s);
static int syntax_error(char *s);
static int laske2(char *muuttuja,double *y);
// static double probit(double z);
// static double uniform(double x);
// static double sur_rand0(double x,int type);
static int varif(char *lauseke,double *y);
static int if_syntax_error(char *x);

/*************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "DF_CORRECTION", "RND", "BINLIMIT", "START",
                 "PRIND", "!" };
char **specs=specs0;
************************/

/********************
void main(argc,argv)
int argc; char *argv[];
*******************/

void muste_transform(char *argv)
        {
        int i,k;
    //  int j;
        char newname[9];

    //  if (argc==1) return;
        s_init(argv);
        if (g<4)
            {
            init_remarks();

rem_pr("Usage (alternatives):");
rem_pr("TRANSFORM <data> BY <function(X)>");
rem_pr("TRANSFORM <data> BY CENTERING");
rem_pr("TRANSFORM <data> BY STANDARDISING (or STD)");
rem_pr("TRANSFORM <data> BY #UNIFORM(a,b)");
rem_pr("TRANSFORM <data> BY #LINEAR(a,b) i.e. X -> a*X+b");
rem_pr("TRANSFORM <data> BY #DISTR(P)   P is n*2 matrix of values and probabilities");
rem_pr("TRANSFORM <data> BY #MARKOV(P)  P is n*n matrix of transition probabilities");
rem_pr("TRANSFORM <data> BY #MARKOV(P,<var>,<n>)  P is as above");
rem_pr("          and <var> variable to save state after <n> steps (starting from 1)");

            wait_remarks(2);
            s_end(argv);
            return;
            }
        l_virhe=0;
        strcpy(lauseke,word[3]);

        cent_std=0;  /* 24.3.1997 */
        if (muste_strnicmp(lauseke,"CENTER",6)==0) cent_std=1;
        else if (muste_strnicmp(lauseke,"STD",3)==0 || muste_strnicmp(lauseke,"STAND",5)==0) cent_std=2;

        special=0; /* 10.5.1998 */
        if (*lauseke=='#') special=1;

        if (strchr(lauseke,'X')!=NULL) muuttuja_mukana=1; else muuttuja_mukana=0;
        if (strstr(lauseke,"MISSING")!=NULL) muuttuja_mukana=0;
        i=data_open2(word[1],&d,1,1,0); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;

        i=conditions(&d); if (i<0) return;
        v=(int *)muste_malloc(d.m_act*sizeof(int));
        if (v==NULL)
            {
            sur_print("\nNot enough memory! (TRANSFORM)");
            WAIT; return;
            }
        for (i=0; i<d.m_act; ++i) v[i]=d.v[i];
  /* TRANSFORM <data> BY <function X#> AS <letter>  */
  /* 0         1      2  3             4  5         */
        if (g>5)
            {
            if (muste_strcmpi(word[4],"AS")==0)
                {
                letter=*word[5];
                if (word[5][1]==':') type=word[5][2]; else type='4';

                for (i=0; i<d.m_act; ++i)
                    {
                    *newname=letter;
                    for (k=0; k<7; ++k) newname[k+1]=d.varname[d.v[i]][k];
                    newname[8]=EOS;
                    v[i]=varfind2(&d,newname,0);
                    if (v[i]<0)
                        {
                        v[i]=create_newvar(&d,newname,type,1);
                        if (v[i]<0) return;
                        }
                    }
                }
            }
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);
        if (cent_std) { centstd(); return; }
        if (special) { special_transf(); return; }

        sur_print("\n");
        for (jj=d.l1; jj<=d.l2; ++jj)  // j->jj globaaliksi! 17.3.2010
            {
            if (unsuitable(&d,jj)) continue;
    //      if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",jj); sur_print(sbuf); }

            for (i=0; i<d.m_act; ++i)
                {
                data_load(&d,jj,d.v[i],&xarvo);
  // 8.9.2009   if (xarvo==MISSING8 && muuttuja_mukana) continue;
                if (xarvo==MISSING8 && muuttuja_mukana) yarvo=MISSING8;
                else
                  {
                  l_virhe=0;
                  k=laske(lauseke,&yarvo);
                  if (k<0)
                      {
                      sprintf(sbuf,"\nError in expression %s",lauseke);
                      sur_print(sbuf); WAIT; return;
                      }
                  }
                k=data_save(&d,jj,v[i],yarvo); if (k<0) return;
                }
            }
        data_close(&d);
        s_end(argv); // 31.7.2011/SM
        return;
        }

static double *mean,*stddev;
static int *n;

static int centstd()
        {
        int i,k,m;
        int j;

        df=1L;
        i=spfind("DF_CORRECTION");
        if (i>=0) df=atol(spb[i]);

        m=d.m_act;
        mean=(double *)muste_malloc(m*sizeof(double));
        if (mean==NULL) { not_enough_memory(); return(-1); }
        n=(int *)muste_malloc(m*sizeof(int));
        if (n==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<m; ++i) { mean[i]=0.0; n[i]=0L; }
        if (cent_std==2)
            {
            stddev=(double *)muste_malloc(m*sizeof(double));
            if (stddev==NULL) { not_enough_memory(); return(-1); }
            for (i=0; i<m; ++i) stddev[i]=0.0;
            }

        sur_print("\nComputing means etc. ... ");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
    //      if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",j); sur_print(sbuf); }

            for (i=0; i<d.m_act; ++i)
                {
                data_load(&d,j,d.v[i],&xarvo);
                if (xarvo==MISSING8) continue;
                ++n[i]; mean[i]+=xarvo;
                if (cent_std==2) stddev[i]+=xarvo*xarvo;
                }
            }

        for (i=0; i<m; ++i)
            {
            if (n[i]==0L && (cent_std==2 && n[i]<2))
                {
                sprintf(sbuf,"\nToo few observations in variable %s!",
                                 d.varname[d.v[i]]);
                sur_print(sbuf); WAIT; return(-1);
                }
            mean[i]/=n[i];
            if (cent_std==2)
                {
                stddev[i]=sqrt( (stddev[i]-(double)n[i]*mean[i]*mean[i])/(n[i]-df) );
                if (stddev[i]<1e-10)
                    {
                    sprintf(sbuf,"\nVariable %s is constant %g!",
                                     d.varname[d.v[i]],mean[i]);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                }
            }


        sur_print("\nMaking transformations ... ");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
   //       if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",j); sur_print(sbuf); }

            for (i=0; i<d.m_act; ++i)
                {
                data_load(&d,j,d.v[i],&xarvo);
                if (xarvo==MISSING8) continue;
                yarvo=xarvo-mean[i];
                if (cent_std==2) yarvo/=stddev[i];
                k=data_save(&d,j,d.v[i],yarvo); if (k<0) return(-1);
                              // 3.12.2004 v[i]=d.v[i]: ei siis virhe!
                }
            }
        data_close(&d);
        return(1);
        }

/* TRANSFORM <data> BY #transf(par1,par2,...) */
static int special_transf()
        {
     // int i,j;

        if (muste_strnicmp(word[3],"#UNIFORM",4)==0)
            {
            init_rnd();
            tr_uniform();
            return(1);
            }
        if (muste_strnicmp(word[3],"#DISTR",4)==0)
            {
            init_rnd();
            distr();
            return(1);
            }
        if (muste_strnicmp(word[3],"#MARKOV",4)==0)
            {
            init_rnd();
            markov();
            return(1);
            }
        if (muste_strnicmp(word[3],"#LINEAR",4)==0)
            {
            tr_linear();
            return(1);
            }

        if (muste_strnicmp(word[3],"#DIFF",5)==0)
            {
            tr_diff();
            return(1);
            }
        return(1);
        }

static double seed;
static int rand_type;

static int init_rnd()
        {
        int i;
        char x[LLENGTH];

        i=spfind("RND");
        if (i<0) strcpy(x,"123456789"); else strcpy(x,spb[i]);
        if (muste_strnicmp(x,"rand(",5)==0) { rand_type=1; i=5; }
        else if (muste_strnicmp(x,"urand(",6)==0) { rand_type=2; i=6; }
        else if (muste_strnicmp(x,"mrand(",6)==0) { rand_type=4; i=6; }
        else { rand_type=1; i=0; }
        seed=atof(x+i);
        return(1);
        }

static int tr_uniform()
        {
        int i,k;
        int j;
        char x[LLENGTH];
        char *p;
        double par1,par2;
        double a;
        extern double sur_rand0();

        strcpy(x,word[3]);
        p=strchr(x,'('); if (p==NULL) return(-1); // RS CHA exit(0)
        par1=atof(p+1);
        par2=atof(word[4]);
        par2-=par1;

        sur_print("\nMaking transformations ... ");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
  //        if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",j); sur_print(sbuf); }


            for (i=0; i<d.m_act; ++i)
                {
                a=sur_rand0(seed,rand_type);
                a=par2*a+par1;
                k=data_save(&d,j,v[i],a); if (k<0) return(-1);
                }
            }
        data_close(&d);
        return(1);
        }

static int distr()
        {
        int i,k;
        int j;
        char x[LLENGTH];
        char *p;
        double *prob;
        double a;
        int binlimit;
        extern double sur_rand0();

        strcpy(x,word[3]);
        p=strchr(x,'('); if (p==NULL) return(-1); // RS CHA exit(0);
        strcpy(matname,p+1);
        i=strlen(matname)-1; if (matname[i]==')') matname[i]=EOS;
        i=matrix_load(matname,&aa,&rdim,&cdim,&rlab,&clab,&lr,&lc,&mtype,expr);
        if (i<0) return(-1);
        if (cdim<2)
            {
            sur_print("The distribution must be given as n x 2 matrix!");
            sur_print("1st column: values, 2nd column: probabilities");
            WAIT; return(-1);
            }
        i=spfind("BINLIMIT");
        binlimit=30; if (i>=0) binlimit=atoi(spb[i]);

        prob=aa+rdim;
        a=0; for (i=0; i<rdim; ++i) a+=prob[i];
        for (i=0; i<rdim; ++i) prob[i]/=a;
        a=0; for (i=0; i<rdim; ++i) { a+=prob[i]; prob[i]=a; }

        sur_print("\nMaking transformations ... ");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
  //        if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",j); sur_print(sbuf); }

            for (i=0; i<d.m_act; ++i)
                {
                a=sur_rand0(seed,rand_type);
                if (rdim<binlimit)
                    {
                    for (k=0; k<rdim; ++k)
                        if (a<prob[k]) break;
                    }
                else
                    k=bin_search(a,prob,rdim);

                k=data_save(&d,j,v[i],aa[k]); if (k<0) return(-1);
                }
            }
        data_close(&d);
        return(1);
        }

static int bin_search(double a,double *tab,int n)
        {
        int k,k1,k2;

        k1=-1; k2=n;
        while (k2-k1>1)
            {
            k=(k1+k2)/2;
            if (a<tab[k]) k2=k; else k1=k;
            }
        if (a<tab[k1+1]) return(k1+1);
        return(k1+2);
        }

static int markov()
        {
        int i,k,h;
        int j;
        char x[LLENGTH];
        char *p;
        double a;
        extern double sur_rand0();
        int start_state,prev_state; // 1.4.2001
        int var,n,ind_var;
        char type_ind_var;
        int aste;
        int prev_states[8],start_states[8];
     // char *s[8];

        type_ind_var=' ';
        strcpy(x,word[3]);
        p=strchr(x,'('); if (p==NULL) return(-1); // RS CHA exit(0);
        strcpy(matname,p+1);
        i=strlen(matname)-1; if (matname[i]==')') matname[i]=EOS;
        i=matrix_load(matname,&aa,&rdim,&cdim,&rlab,&clab,&lr,&lc,&mtype,expr);
        if (i<0) return(-1);

        aste=1; k=cdim;
        while (1)
            {
            if (rdim==k) break;
            if (rdim<k) { aste=0; break; }
            ++aste; k*=cdim;
            }

        if (!aste)
            {
            sur_print("\nError in the matrix of transition probabilities!");
            sprintf(sbuf,"\n# of columns is %d.",cdim); sur_print(sbuf);
            sprintf(sbuf,"\n# of rows must be the same %d",cdim); sur_print(sbuf);
            sprintf(sbuf,"\nor of the form %d^k where k=2,3,..,8 is the order of the chain.",cdim);
            sur_print(sbuf);
            WAIT; return(1);
            }

        var=0; n=d.m_act; ind_var=0;
        if (g>5) /* TRANSFORM <data> BY #MARKOV(P,var,n) */
            {
            var=varfind(&d,word[4]);
            if (var<0) return(-1);
            ind_var=1;
            type_ind_var=d.vartype[var][0];
            n=atoi(word[5]);
            }

        for (i=0; i<rdim; ++i)
            {
            a=0.0;
            for (j=0; j<cdim; ++j)
                { a+=aa[i+rdim*j]; aa[i+rdim*j]=a; }
            if (fabs(a-1.0)>1e-5)
                {
                sprintf(sbuf,"\nMatrix %s: row sum %d not =1!",
                                matname,i+i);
                sur_print(sbuf); WAIT; return(-1);
                }
            }

// if (aste>1) { printf("\naste=%d",aste); WAIT; return(1); }

        start_state=0; // 1.4.2001
        if (aste>1) for (k=0; k<aste; ++k) start_states[k]=0; // 24.4.2002

        i=spfind("START");
        if (i>=0) start_state=atoi(spb[i])-1;

        if (aste>1 && i>=0)
            {
            muste_ltoa((int)start_state,sbuf,cdim);
            for (i=0; i<cdim; ++i) x[i]='0'; x[cdim]=EOS;
            strcpy(x+aste-strlen(sbuf),sbuf);
// printf("\nx=%s|",x); getch();
            for (k=0; k<aste; ++k) start_states[k]=(int)(x[k]-'0');
            }

        sur_print("\nMaking transformations ... ");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
  //        if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",j); sur_print(sbuf); }

            prev_state=start_state; /* states 0,1,2,...,rdim-1 internally */
            if (aste>1) for (k=0; k<aste; ++k)
                               prev_states[k]=start_states[k];

            for (i=0; i<n; ++i)
                {
                a=sur_rand0(seed,rand_type);
                if (aste==1)
                    {
                    for (k=0; k<rdim; ++k)
                        if (a<aa[prev_state+rdim*k]) break;
                    prev_state=k;
                    }
                else
                    {
                    prev_state=prev_states[0];
                    for (k=1; k<aste; ++k)
                        prev_state=cdim*prev_state+prev_states[k];
                    for (k=0; k<rdim; ++k)
                        if (a<aa[prev_state+rdim*k]) break;
                    for (h=0; h<aste-1; ++h)
                        prev_states[h]=prev_states[h+1];
                    prev_states[aste-1]=k;
                    }
                if (ind_var) continue;
                if (d.vartype[v[i]][0]=='S')
                     k=data_alpha_save(&d,j,v[i],clab+lc*k);
                else k=data_save(&d,j,v[i],(double)(k+1));
                if (k<0) return(-1);
                }
            if (ind_var)
                {
                if (type_ind_var=='S')
                     k=data_alpha_save(&d,j,var,rlab+lr*prev_state);
//                   k=data_alpha_save(&d,j,var,clab+lc*k);
                else k=data_save(&d,j,var,(double)(prev_state+1));
           //   else k=data_save(&d,j,var,(double)(k+1));
                if (k<0) return(-1);
                }
            }
        data_close(&d);
        return(1);
        }

static int tr_linear()
        {
        int i,k;
        int j;
        char x[LLENGTH];
        char *p;
        double par1,par2;
        double a;

        strcpy(x,word[3]);
        p=strchr(x,'('); if (p==NULL) return(-1); // RS CHA exit(0);
        par1=atof(p+1);
        par2=atof(word[4]);

        sur_print("\nMaking transformations ... ");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
  //        if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",j); sur_print(sbuf); }


            for (i=0; i<d.m_act; ++i)
                {
                data_load(&d,j,v[i],&a);
                a=par1*a+par2;
                k=data_save(&d,j,v[i],a); if (k<0) return(-1);
                }
            }
        data_close(&d);
        return(1);
        }

static double *obs_lag;
static int tr_diff()
        {
        int i,k;
        int j;
     // char x[LLENGTH];
     // char *p;
        double a;

        obs_lag=(double *)muste_malloc(d.m_act*sizeof(double));

        sur_print("\nMaking transformations ... ");

        for (i=0; i<d.m_act; ++i)
            {
            data_load(&d,d.l1,v[i],&obs_lag[i]);
            }

        for (j=d.l1+1L; j<=d.l2; ++j)
            {
//          if (unsuitable(&d,j)) continue;
   //       if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",j); sur_print(sbuf); }

            for (i=0; i<d.m_act; ++i)
                {
                data_load(&d,j,v[i],&a);
                k=data_save(&d,j,v[i],a-obs_lag[i]); if (k<0) return(-1);
                obs_lag[i]=a;
                }
            }
        data_close(&d);
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(1);
        }


static char v2_name[100][9];
static int v2[100];
static int n_v2=0;

static int lue_muuttujan_arvo(char *s,double *py)  // 17.3.2010
    {
    int i;

    for (i=0; i<n_v2; ++i)
        if (strcmp(s,v2_name[i])==0) break;
    if (i==n_v2)
        {
        if (n_v2>=100)
            {
            sur_print("\nToo many (more than 100) variables in expression!");
            WAIT; return(-1); // RS CHA exit(0);
            }
        i=varfind2(&d,s,0);
// printf("\ns=%s i=%d",s,i);
        if (i<0) return(-1);
        strcpy(v2_name[n_v2],s);
        v2[n_v2]=i;
        i=n_v2++;
        }
    data_load(&d,jj,v2[i],py);
// printf("\ni=%d v2[i]=%d *py=%g",i,v2[i],*py); getch();
    return(1);
    }

/* tarit.c 31.10.1985/SM  (22.3.1989) (12.2.1994) (8.9.1998)
   aritmetiikka (+ - * / ^) ja yhden muuttujan funktiot
*/

#define MAXPITUUS 100
#define MAXARG 1
// #define RND (double)rand()/32768.0

extern char **spb;

static int laske(char *lauseke,double *y)
        {
        char x[MAXPITUUS];
        char *p,*q;
        char sana[32];
        int len;
        double opnd[MAXARG+4]; char op[MAXARG+4]; int v[MAXARG+4];
        int t,n;
/*      int narg;    Usean muuttujan funktion argumenttien lkm     */
        int i;

        if (*lauseke=='i')
            {
            if (strncmp(lauseke,"if(",3)==0)
                return(varif(lauseke,y));
            }

        strcpy(x,lauseke);
        len=0;
        p=x;
        t=0;

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
                q=p+1;
                if (*q==')') { sprintf(sbuf,"\nArguments missing in %s",lauseke);
                               sur_print(sbuf); l_virhe=1; return(-1); }
                n=1;
/*              narg=1;
*/              while (n)
                    {
                    ++p;
                    if (*p=='(') { ++n; continue; }
                    if (*p==')') { --n; continue; }
                    if (*p==EOS) { sprintf(sbuf,"\n) is missing in %s",lauseke);
                                   sur_print(sbuf); l_virhe=1; return(-1); }
/*                  if (*p==',' && n==1)
                        {
                        *p=EOS;

                        laske(q,&opnd[t]);
                        ++t;
                        if (t>MAXARG+3)
                            { sprintf(sbuf,"\nToo many arguments in %s",lauseke);
                              sur_print(sbuf); l_virhe=1; return(-1); }
                        ++narg;
                        q=p+1;
                        }
*/
                    }
                if(strchr("+-*/^)\0",*(p+1))==NULL) { syntax_error(lauseke);
                                                      return(-1); }
                *p=EOS; ++p;
/*   printf("\nq=%s",q); getch();   */
                i=laske(q,&opnd[t]);
                if (i<0 || l_virhe) return(-1);
/*   printf("\ntulos1=%f",opnd[t]); getch();  */
                if (len==0) { len=-1; break; }
                sana[len]=EOS;

/*              if (narg>1)
                    {

             printf("\nArgumentit: ");
             for (i=t-narg+1; i<=t; ++i) printf(" %g",opnd[i]); getch();

                    t=t-narg+1;
                    if (*sana=='-')
                        opnd[t]=-mfunktio(sana+1,opnd+t,narg);
                    else
                        opnd[t]=mfunktio(sana,opnd+t,narg);
                    if (l_virhe) return(-1);
                    len=-1;
                    break;
                    }
*/
                /* Yhden muuttujan funktiot */
                if (*sana=='-')
                    opnd[t]=-funktio(sana+1,opnd[t]);
                else
                    opnd[t]=funktio(sana,opnd[t]);
                if (l_virhe) return(-1);
                len=-1;
                break;

              case ')':
                sprintf(sbuf,"\n( missing in %s",lauseke); sur_print(sbuf); l_virhe=1; return(-1);

              case 'e': case 'E':
                if (strchr("+-.0123456789",sana[0])!=NULL)
                    {
                    sana[len++]=*p; ++p;
                    if (*p!='+' && *p!='-') break;
                    }
              default:
                /* tarkistukset puuttuvat */
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

static double luku(char *sana,int len)
        {
        char *p;
        double tulos=1.0;
        int i;

        sana[len]=EOS;
        p=sana; if (*p=='-') ++p;
        if (strchr("1234567890.",*p)==NULL)
            {
            i=laske2(p,&tulos); if (i<0) return((double)1.0);
            if (*sana=='-') return(-tulos);
            return(tulos);
            }
        return(atof(sana));
        }

static double oper(double x1,double x2,char laji)
        {
        double power();

        if (x1==MISSING8 || x2==MISSING8) return(MISSING8); // 19.3.2010

        switch (laji)
            {
          case '+':
            return(x1+x2);
          case '-':
            return(x1-x2);
          case '*':
            return(x1*x2);
          case '/':
            if (x2==0.0) { l_virhe=1; return(MISSING8); }
            return(x1/x2);
          case '^':
            return(power(x1,x2));
            }
        return(MISSING8);
        }

static double power(double x,double y)
        {
        short i;
        double f;
        if (y>=0 && y==floor(y) && y<10)
                {
                f=1;
                for (i=0; i<(short)y; ++i) f*=x;
                return(f);
                }
        return (pow(x,y));
        }
/**************************
static double round(double x)
        {
        int l;
        double a;

        l=x;
        a=l;
        if (x>0.0 && x-a>0.5) ++a;
        else if (x<0.0 && a-x>0.5) --a;
        return(a);
        }
****************************/
static int supista(int *t,double opnd[],char op[],int v[])
        {

        while (*t>1)
            {
            if (v[*t-1]>v[*t-2]) return(1);
            opnd[*t-2]=oper(opnd[*t-2],opnd[*t-1],op[*t-2]);
            op[*t-2]=op[*t-1]; v[*t-2]=v[*t-1];
            --(*t);
            }
        return(1);
        }

static double funktio(char *s,double x)
        {
   //   int i;
   //   double y;
        char S[32];
        extern double probit();
        extern double uniform();
        extern double sur_rand0();

        if (*s==EOS) return(x);
        strncpy(S,s,31); S[31]=EOS; muste_strupr(S);

        if (strcmp(S,"RAND")==0) return(sur_rand0(x,1));
        if (strcmp(S,"URAND")==0) return(sur_rand0(x,2));
        if (strcmp(S,"SRAND")==0) return(sur_rand0(x,3));
        if (strncmp(S,"SQR",3)==0) return(muste_sqrt(x));
        if (strcmp(S,"LOG")==0) return(muste_log(x));
        if (strcmp(S,"EXP")==0) return(muste_exp(x));
        if (strcmp(S,"SIN")==0) return(muste_sin(x));
        if (strcmp(S,"COS")==0) return(muste_cos(x));
        if (strcmp(S,"TAN")==0) return(muste_tan(x));
        if (strcmp(S,"ATN")==0) return(muste_atan(x));
        if (strcmp(S,"ABS")==0) return(fabs(x));
        if (strcmp(S,"INT")==0) return(floor(x));
        if (strcmp(S,"PROBIT")==0) return(probit(x));
        if (strcmp(S,"RND")==0) return(uniform(x));
        if (strcmp(S,"ROUND")==0) return(round(x));

/*      i=f_tiedosto(s,&x,1,&y); if (i>0) return(y);
*/
        f_tuntematon(s);
        l_virhe=1;
        return(x);
        }

static int f_tuntematon(char *s)
        {
        sprintf(sbuf,"\nUnknown function %s",s);
        sur_print(sbuf); WAIT;
        l_virhe=1; return(1);
        }
/******************
static int arg_virhe(char *s)
        {
        sprintf(sbuf,"\n%s: Error in arguments",s);
        sur_print(sbuf); WAIT;
        l_virhe=1; return(1);
        }
**************************/
static int syntax_error(char *s)
        {
        sprintf(sbuf,"\nsyntax error in %s",s);
        sur_print(sbuf); WAIT;
        l_virhe=1; return(1);
        }

static int laske2(char *muuttuja,double *y)
        {
        int i;

        if (*muuttuja=='X' && (muuttuja[1]=='#' || muuttuja[1]==EOS) )
            {
            *y=xarvo;
      /*    if (xarvo==MISSING8) { l_virhe=1; return(-1); }   */
            return(1);
            }

        if (*muuttuja=='M' && strcmp(muuttuja,"MISSING")==0)
            {
            *y=MISSING8;
            return(1);
            }
        i=spfind(muuttuja);
        if (i<0)
            {
            i=lue_muuttujan_arvo(muuttuja,y); // 17.3.2010
            if (i==1) return(1);
            sprintf(sbuf,"\nValue for %s not found!",muuttuja);
            sur_print(sbuf); WAIT;
            return(1); // RS CHA exit(1);
            }
        laske(spb[i],y);
        return(1);
        }
/*****************************
static double probit(double z)
        {
        double z1,z2,f;

        z1=z; if (z>0.5) z1=1-z;
        z2=sqrt(log(1.0/(z1*z1)));
        f=1.0+z2*(1.432788+z2*(0.189269+z2*0.001308));
        f=z2-(2.515517+z2*(0.802853+z2*0.010328))/f;
        if (z<=0.5) f=-f;
        return(f);
        }
*********************************/
/***********************************
static double uniform(double x)
        {
        time_t ltime;
        unsigned int *pi;
        static int next=0;

        if (x==0.0)
            {
            time(&ltime);
            pi=(unsigned int *)&ltime;
            srand(*pi+*psur_seed); rand(); *psur_seed=rand();
            *psur_seed+=17;
            }
        else
            {
            if (next) return((double)(RND+1e-6));
            if (x!=1.0) { srand((unsigned int)(x)); rand(); }
            next=1;
            }
        return((double)(RND+1e-6));
        }
*********************************/
/*********************************
static double sur_rand0(double x,int type)
        {
        static int next=0;
        extern double sur_rand();
        extern double sur_urand();
        extern double sur_srand();
        extern void init_genrand();
        extern double genrand_real2();

        if (x==0.0)
            {
            sur_print("\nArgument 0 not permitted in this rand function!");
            WAIT; exit(1);
            }

        switch (type)
            {
          case 1:
            if (!next)
               { sur_rand_seed((unsigned int)x); next=1; }
            return (sur_rand());
          case 2:
            if (!next)
               { sur_urand_seed((unsigned int)x); next=1; }
            return (sur_urand());
          case 3:
            if (!next)
               { sur_srand_seed((unsigned int)x); next=1; }
            return (sur_srand());
          case 4:
            if (!next)
               { init_genrand((unsigned int)x); next=1; }
            return (genrand_real2());

            }
        return(0.0);
        }
********************************************/
/* varif.c 30.4.1985/SM  (16.5.1986)
   aritmetiikka if(ehto)then(a)else(b)
*/

extern char **spb;
extern double *arvo;

static int varif(char *lauseke,double *y)
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
        rel='=';
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
        if (strncmp(p+1,"else(",5)!=0) { if_syntax_error(lauseke); return(-1); }
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

//      if (*y==MISSING8 || y1==MISSING8) // 7.9.2010
//          { laske(d,y); return(1); }

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

static int if_syntax_error(char *x)
        {
        sprintf(sbuf,"\nSyntax error in %s",x); sur_print(sbuf);
        WAIT; l_virhe=1; return(1);
        }

