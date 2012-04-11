/* _comb.c 12.4.1998  (12.4.1998)
*/

#include <math.h>
// #include <conio.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXPAR 100
#define MAXELEM 10000

static char type[LNAME];
static int npar;
static char *par[MAXPAR];
static char symblist[8*MAXELEM];  // 4
static char *symbol[MAXELEM];  /* a b c d */
static int nsymb;              /* esim. 4 */
static int ind_elem;
static char *symbol2[MAXELEM];
static int line;
static int n_comb;
static int res;
static int kb_count;
static int pot[MAXELEM];
static int npart0;
static int coeff;
static double *comb_lfact;
static int multin;
static int distinct_symbols; // 13.11.2004

static double *freq,*freq3;

static int elem0[MAXELEM];
static int elem1[MAXELEM];
static int elem2[MAXELEM];
static int vmax[MAXELEM];
static int vmin[MAXELEM];
static char *vv[MAXELEM];
static int off[MAXELEM];
static int fast=1; // 22.7.2006

static char fname[LNAME];
static FILE *txt;

static double *aa;
static char *rlab,*clab;
static int rdim,cdim,lr,lc,mtype;
static char expr[LLENGTH];
static char matname[LNAME];
static int *restr_mat ; /* aa int-tyyppisen„ */
static int parts_by_off=0; // 4.8.2006
static int greatest_part=0;
static int survo; // 11.11.2006

static int txt_open;

static double *mlt_prob;
static int multin_min,multin_max;
static int nnn;
static double *dprob;
static int *mlt_min, *mlt_max;
static double *mlt_pr;

static int tclose();
static int permutations();
static int next_perm(int n,int * p);
static int permutations2(int n,int m);
static int next_perm_m(int n,int m,int *p,int *q);
static int permutations_with_repetitions(int n,int m);
static int next_perm_m_rep(int n,int m,int *koko,int *perm,int *apu);
static int r_permutations();
static int next_restr_perm_from(int k,int n);
// static int sort_symbols(char **symb,int nsymb);
static int subsets();
static int next_m_comb(int n,int m,int *p);
static int subsets_with_repetitions(int n,int m);
static int next_subset_m_rep(int n,int m,int k,int *elem0,int *elem1,int *elem2);
static int partitions();
static int next_m_part(int n,int m,int *p);
static double multin_prob1(int n,int m);
static int tee_comb_lfact(int n);
static int min_max_partitions(int n,int m,int min,int max);
static int next_min_max_partition(int n,int m,int k,int min,int max);
static int partitions2();
static int partitions_with_given_distinct_parts(int n,int m,int npart,int a[]);
static int n_part(int n,int m);
static int n_dist_part(int n,int m);
static int n_part_p(int n,int m);
static int n_dist_part_p(int n,int m);
static int print_n_part();
static int count_partitions(int n,int distinct,double *pa);
static int distributions();
static int next_m_distr(int n,int m,int *elem1);
static double laske_multin_prob(int n,int m);
static double laske_multin_prob2(int n,int m);
static double laske_multin_prob3(int n,int m);
static int multin_comp();
static int multin_comp2();
static int multin_comp3();
static double multin_prob2(int n,int m);
static int distributions2(int n,int m);
static int set_distr(int n,int m,int k);
static int integers();
static int next_integer(int n,int m,int *elem1);
static int next_integer_with_gap(int n,int m,int *elem1,int gap);
static int lattices();
static int next_lattice_point(int n,int *elem1);
static int print_comb(int *p,int k,int s);
static int spec_error(char *s);
static int load_restr_matrix(int n);
static int not_enough_memory();
static int find_first();

static void egypt();


/***********************************
char *specs0[]={ "ELEMENTS", "SYMBOLS", "RESULTS", "RESTRICTIONS",
                 "PARTS", "MAX", "MIN", "DISTINCT", "POWER", "MULTIN",
                 "SUMSTEP", "COEFF", "PROB", "MOVE", "SUM", "MIN_GAP",
                 "!" };
char **specs=specs0;
***********************************/

extern char **spb;
/************************
void main(argc,argv)
int argc; char *argv[];
*************************/

void muste_comb(char *argv)
        {
        int i;
        char x[LLENGTH];
        char *p;

//      if (argc==1) return;
        s_init(argv);
        if (muste_strnicmp(word[0],"EGYPT",5)==0)
            {
            egypt();
            s_end(argv);
            return;
            }
        if (g<3)
            {
            init_remarks();
            rem_pr("Usage: COMB <name>,L  / <name>=<type>,...");
            wait_remarks(2);
            s_end(argv[1]);
            return;
            }
        i=spec_init(r1+r-1); if (i<0) return;

    comb_lfact=NULL;
    freq=NULL;
    freq3=NULL;
    mlt_prob=NULL;
    mlt_pr=NULL;
    mlt_min=NULL;
    mlt_max=NULL;
    dprob=NULL;


//       for (i=0; i<4*MAXELEM; ++i) symblist[i]=EOS; // 17.10.2011

        line=0;
        txt_open=0;
        if (muste_strcmpi(word[2],"TO")==0)
            {
            if (g<4)
                {
                sur_print("\nUsage: COMB <name> TO <txt_file>");
                WAIT; return;
                }
            strcpy(fname,word[3]);
            if (strchr(fname,':')==NULL)
                {
                strcpy(fname,edisk); strcat(fname,word[3]);
                }
            txt=muste_fopen(fname,"wt");
            if (txt==NULL)
                {
                sprintf(sbuf,"\nCannot open text file %s!",fname);
                sur_print(sbuf); WAIT; return;
                }
            txt_open=1;
            }
        else
            {
            line=edline2(word[2],1,1);
            if (line==0) return;
            }
        i=spfind(word[1]);
        if (i<0)
            {
            sprintf(sbuf,"\nCOMB name %s not given!",word[1]);
            sur_print(sbuf); WAIT; return;
            }
        strcpy(x,spb[i]);
        p=strchr(x,',');
        if (p==NULL) { strcpy(type,x); npar=0; }
        else
            {
            *p=EOS; strcpy(type,x);
            npar=split(p+1,par,MAXPAR);
            }

        ind_elem=0;
        i=spfind("ELEMENTS");
        if (i>=0) ind_elem=1; else i=spfind("SYMBOLS");
        distinct_symbols=1;
        if (!ind_elem && i<0)   /* 1 2 3 4 ... */
            {
            p=symblist;
            for (i=0; i<MAXELEM; ++i)
                {
                sprintf(sbuf,"%d",i+1);
                strcpy(p,sbuf); symbol[i]=p; p+=strlen(sbuf)+1;
                }
            }
        else
            {
            strcpy(symblist,spb[i]);
            nsymb=split(symblist,symbol,MAXELEM);

            for (i=1; i<nsymb; ++i) // 17.10.2011
              {
              if (strcmp(symbol[i],symbol[i-1])==0) { distinct_symbols=0; break; }
              }
            }

        res=70;
        i=spfind("RESULTS");
        if (i>=0) res=atoi(spb[i]);

        if (res==1) --line;

        i=spfind("FAST");
        if (i>=0) fast=atoi(spb[i]);


        n_comb=0L; kb_count=0;
        if (muste_strnicmp(type,"PERM",4)==0) { permutations(); tclose(); s_end(argv); return; }
        if (muste_strnicmp(type,"SUBS",4)==0) { subsets(); tclose(); s_end(argv); return; }
        if (muste_strnicmp(type,"PART",4)==0) { partitions(); tclose();  s_end(argv); return; }
        if (muste_strnicmp(type,"DIST",4)==0) { distributions(); tclose(); s_end(argv); return; }
        if (muste_strnicmp(type,"INTE",4)==0) { integers(); tclose(); s_end(argv); return; }
        if (muste_strnicmp(type,"LATT",4)==0) { lattices(); tclose(); s_end(argv); return; }
        if (muste_strnicmp(type,"R_PERM",6)==0) { r_permutations(); tclose(); s_end(argv); return; }

        if (muste_strnicmp(type,"#FIRST",6)==0) { find_first(); tclose(); s_end(argv); return; }
        if (muste_strnicmp(type,"MULTIN_PROB",8)==0) { multin_comp(); tclose(); s_end(argv); return; }
        sprintf(sbuf,"\nCOMB type %s unknown!",type);
        sur_print(sbuf); WAIT; tclose(); return;
        }

static int tclose()
    {
    if (txt_open==1) muste_fclose(txt); return(1);
    }

static int permutations()
        {
        int i,n,line1;
        int restr;
        double dp;

        n=atoi(par[0]);
        if (n<=0) return(-1);

        if (npar>1) { permutations2(n,atoi(par[1])); return(1); }

        restr=0;
        i=spfind("RESTRICTIONS");
        if (i>=0)
            {
            strcpy(matname,spb[i]);
            load_restr_matrix(n);
            restr=1;
            }

        line1=line;
        if (!restr && !res)
            {
            dp=1.0;
            for (i=2; i<=n; ++i) dp*=(double)i;
            edwrite(space,line1,1);
            sprintf(sbuf,"Permutations of %d elements: N[%s]=%.15g",
                              n,word[1],dp);
            edwrite(sbuf,line1,1);
            return(1);
            }

        for (i=0; i<n; ++i) elem1[i]=i;
        while (1)
            {
            if (restr)
                {
                for (i=0; i<n; ++i)
                    if (restr_mat[elem1[i]+n*i]) break;
                if (i==n) print_comb(elem1,n,1);
                }
            else print_comb(elem1,n,1);
            i=next_perm(n,elem1);
    /*      i=vanha_next_perm(n,elem1,elem2); */
            if (i<0) break;
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        sprintf(sbuf,"Permutations of %d elements: N[%s]=%d",
                          n,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int next_perm(int n,int * p)
// int n;
// int *p; /* permutaatio p[0], p[1],..., p[n-1] */
        {
        int i,j,k,pi;

        i=n-1;
        while (i>0 && p[i-1]>p[i]) --i;
        if (i==0) return(-1); /* kaikki permutaatiot k„yty l„pi */
        --i;
        j=n-1;
        pi=p[i];
        while (p[j]<pi) --j;
        p[i]=p[j]; p[j]=pi;

        j=i+1; k=n-1;
        while (j<k)
            {
            pi=p[j]; p[j]=p[k]; p[k]=pi;
            ++j; --k;
            }
        return(1);
        }

static int permutations2(int n,int m)
        {
        int i,line1;
        double dp;

        if (m>n) return(1);
//      if (ind_elem) { permutations_with_repetitions(n,m); return(1); }
// 17.10.2011
 if (!distinct_symbols) { permutations_with_repetitions(n,m); return(1); }

        line1=line;
        if (!res)
            {
            dp=1.0;
            for (i=n-m+1; i<=n; ++i) dp*=(double)i;
            edwrite(space,line1,1);
            sprintf(sbuf,"%d-permutations of %d elements: N[%s]=%.15g",
                              m,n,word[1],dp);
            edwrite(sbuf,line1,1);
            return(1);
            }
        for (i=0; i<m; ++i) elem1[i]=i;

        while (1)
            {
            print_comb(elem1,m,1);
            i=next_perm_m(n,m,elem1,elem2);
            if (i<0) break;
            }

        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        sprintf(sbuf,"%d-permutations of %d elements: N[%s]=%d",
                          m,n,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int next_perm_m(int n,int m,int *p,int *q)
        {
        int i,j,k,h;

        for (j=0; j<n; ++j) q[j]=0;
        for (j=0; j<m; ++j) q[p[j]]=1;
        for (k=m-1; k>=0; --k)
            {
            if (p[k]==n-1) { q[n-1]=0; continue; }
            for (i=p[k]+1; i<n; ++i)
                if (!q[i]) break;
            if (i==n) { q[p[k]]=0; continue; }
            q[p[k]]=0; p[k]=i; q[i]=1;
            if (k==m-1) return(1);
            h=k;
            for (j=0; j<n; ++j)
                {
                if (q[j]) continue;
                p[++h]=j;
                if (h==m) break;
                }
            break;
            }
        if (k==-1) return(-1);
        return(1);
        }

static int permutations_with_repetitions(int n,int m)
        {
        int i,j;
        int line1;
        char *ps;


        line1=line;
        if (nsymb<n) { sur_print("\n# of symbols (elements) too small!");
                       WAIT; return(-1);
                     }

//      sort_symbols(symbol,n);      // 17.10.2011

        j=0; ps=symbol[0]; symbol2[0]=ps;
        for (i=0; i<n; ++i)
            {
            if (strcmp(ps,symbol[i])!=0) { ++j; ps=symbol[i]; symbol2[j]=ps; }
            elem0[i]=j;
            }
        for (i=0; i<m; ++i) elem1[i]=elem0[i];
        while (1)
            {
            print_comb(elem1,m,2);
            i=next_perm_m_rep(n,m,elem0,elem1,elem2);
            if (i<0) break;
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        sprintf(sbuf,"%d-permutations of %d elements (with repetitions): N[%s]=%d",
                          m,n,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }


static int next_perm_m_rep(int n,int m,int *koko,int *perm,int *apu)
        {
        int i,j=0,k=0,h; // RS ADD inits

        for (i=0; i<n; ++i) apu[i]=0;
        for (i=0; i<m; ++i)
            {
            k=perm[i];
            for (j=0; j<n; ++j)
                {
                if (!apu[j] && k==koko[j]) { apu[j]=1; break; }
                }
            }
        for (h=j+1; h<n; ++h)
            if (!apu[h] && koko[h]>k) break;
        if (h<n)
            {
            perm[m-1]=koko[h];
            return (1);
            }
/* viimeist„ ei voinut korottaa */
        for (i=0; i<n; ++i) apu[i]=0;
        for (j=m-1; j>0; --j)
            if (perm[j-1]<perm[j]) break;
        if (j==0) return(-1);
        h=j-1;
        for (i=0; i<h; ++i) /* alkup„„ paikallaan */
            {
            k=perm[i];
            for (j=0; j<n; ++j)
                {
                if (!apu[j] && k==koko[j]) { apu[j]=1; break; }
                }
            }
        k=perm[h];
        for (i=0; i<n; ++i)
            if (!apu[i] && k<koko[i]) break;
        apu[i]=1; perm[h]=koko[i];
        j=h+1;
        for (i=0; i<n; ++i)
            {
            if (j==m) break;
            if (!apu[i]) { perm[j]=koko[i]; ++j; }
            }
        return(1);
        }

static int r_permutations()
        {
        int i,n,line1;
   //   int restr;

        n=atoi(par[0]);
        if (n<=0) return(-1);
        i=spfind("RESTRICTIONS");
        if (i>=0)
            {
            strcpy(matname,spb[i]);
            load_restr_matrix(n);
            }
        else { sur_print("\nRESTRICTIONS=<matrix_file> missing!");
               WAIT; return(-1);
             }
        line1=line;
        for (i=0; i<n; ++i)
            if (restr_mat[i]==0) break;
        if (i<n)
            {
            elem1[0]=i-1;
            i=next_restr_perm_from(0,n);
            if (i>0)
                {
                print_comb(elem1,n,1);
                while (1)
                    {
                    i=next_restr_perm_from(n-1,n);
                    if (i<0 || elem1[1]<0) break;
                    print_comb(elem1,n,1);
                    }
                }
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        sprintf(sbuf,"Permutations of %d elements with restrictions %s: N[%s]=%d",
                          n,matname,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int next_restr_perm_from(int k,int n)
        {
        int i,h;

        if (k<0) return(-1);
        elem1[k+1]=-1; /* tahallinen ylitys, kun k=n-1 */
        for (i=elem1[k]+1; i<n; ++i)
            {
            if (restr_mat[i+n*k]) continue;
            for (h=0; h<k; ++h)
                if (elem1[h]==i) break;
            if (h<k) continue;
            elem1[k]=i; if (k==n-1) return(1);
            if (next_restr_perm_from(k+1,n)==1) return(1);
            }
        if (i<n) return(1);
        return(next_restr_perm_from(k-1,n));
        }

// sort_symbols() removed 17.10.2011

static int subsets()
        {
        int i,n,m,line1;
        int m0,m1,m2;
        int all;
        double u,v;
        int iu,iv;
        double y;

		m=m0=0; // RS ADD
        n=atoi(par[0]);
        if (n<=0) return(-1);
        if (npar==1)
            { all=1; m1=1; m2=n; }
        else
            { all=0; m0=m=atoi(par[1]); if (m>n) return(-1); m1=m2=m; }

        if (!distinct_symbols) { subsets_with_repetitions(n,m); return(1); }

        line1=line;
        if (!res)
            {
            if (!all)
                {
                iv=v=n; iu=u=m;
                if ((double)iu!=u) return(0.0);
                if ((double)iv!=v) return(0.0);
                if (u>v/2) u=v-u;
                if (u<0.0 || v<0.0) return(0.0);
                if (u==0.0) return(1.0);
                y=1.0;
                for (; u>0; --u, --v) y*=(v/u);
            sprintf(sbuf,"Subsets of size %d of %d elements: N[%s]=%.15g",
                              m,n,word[1],y);
                }
            else
                {
                y=pow(2.0,(double)n)-1;
             sprintf(sbuf,"Non-empty subsets of %d elements: N[%s]=%.15g",
                                  n,word[1],y);
                }
            edwrite(sbuf,line1,1);
            return(1);
            }

        for (m=m1; m<=m2; ++m)
            {
            for (i=0; i<m; ++i) elem1[i]=i;
            while (1)
                {
                print_comb(elem1,m,1);
                i=next_m_comb(n,m,elem1);
                if (i<0) break;
                }
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        if (all)
            sprintf(sbuf,"Non-empty subsets of %d elements: N[%s]=%d",
                                 n,word[1],n_comb);
        else
            sprintf(sbuf,"Subsets of size %d of %d elements: N[%s]=%d",
                              m0,n,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int next_m_comb(int n,int m,int *p)
        {
        int i,k;

        if (p[m-1]<n-1) { ++p[m-1]; return(1); }
        i=m-2;
        while (p[i]==n-m+i && i>=0 ) --i;
        if (i<0) return(-1);
        ++p[i]; k=p[i]; ++i;
        for ( ; i<m; ++i) p[i]=++k;
        return(1);
        }


static int subsets_with_repetitions(int n,int m)
        {
        int i,j;
        int line1;
        char *ps;
        int k;

        line1=line;
        if (nsymb<n) { sur_print("\n# of symbols (elements) too small!");
                       WAIT; return(-1);
                     }

//      sort_symbols(symbol,n); // 14.11.2004

        j=0; ps=symbol[0]; symbol2[0]=ps; elem2[0]=0;
        for (i=0; i<n; ++i)
            {
            if (strcmp(ps,symbol[i])!=0)
                {
                ++j; ps=symbol[i]; symbol2[j]=ps; elem2[j]=i;
                }
            elem0[i]=j;
            }
        k=j; /* max elem */
/* for (j=0; j<n; ++j) printf("%d\n",elem0[j]); getch();
*/

        for (i=0; i<m; ++i) elem1[i]=elem0[i];
        while (1)
            {
            print_comb(elem1,m,2);
            i=next_subset_m_rep(n,m,k,elem0,elem1,elem2);
            if (i<0) break;
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        sprintf(sbuf,"%d-subsets of %d elements (with repetitions): N[%s]=%d",
                          m,n,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int next_subset_m_rep(int n,int m,int k,int *elem0,int *elem1,int *elem2)
        {
        int i,j,h;

        i=m;
        while (i>0)
            {
            --i;
            if (elem1[i]<k)
                {
                h=elem2[elem1[i]+1];
                if (n-h<m-i) continue;
                for (j=i; j<m; ++j) elem1[j]=elem0[h++];
                return(1);
                }
            }
        return(-1);
        }

static int partitions()
        {
        int i,n,m,line1;
        int m0,m1,m2;
        int all;
        int rajoitukset,raj2;
        int max,min;
        int imax,imin,idistinct;
        int ok;
        int j,k,power;
        double da;
        int n_off; // 25.4.2006

        i=spfind("PARTS");
        if (i>=0) { partitions2(); return(1); }
        greatest_part=0;
        rajoitukset=0;
        imax=imin=idistinct=0; max=min=m=m0=0; da=0.0;
        i=spfind("MAX");
        if (i>=0) { imax=1; max=atoi(spb[i]); ++rajoitukset; }
        i=spfind("MIN");
        if (i>=0) { imin=1; min=atoi(spb[i]); ++rajoitukset; }
        i=spfind("DISTINCT");
        if (i>=0) { idistinct=atoi(spb[i]); rajoitukset+=idistinct; }
        i=spfind("GREATEST");  // 14.8.2006
        if (i>=0) {
                  greatest_part=atoi(spb[i]);
                  if (res==0)
                      {
                      sprintf(sbuf,"\nCompute this by PARTITIONS,%d,%d !",
                                     atoi(par[0]),greatest_part);
                      sur_print(sbuf); WAIT;
                      return(-1);
                      }
                  rajoitukset+=greatest_part;
                  }


        n_off=0; // 25.4.2006
        i=spfind("OFF");
        if (i>=0)
            {
            strcpy(sbuf,spb[i]);
            n_off=split(sbuf,vv,MAXELEM);
            for (i=0; i<n_off; ++i) off[i]=atoi(vv[i]);

            if (imax && idistinct)
                {
                parts_by_off=max-n_off;
                for (i=0; i<max; ++i) elem2[i]=i+1;
                for (i=0; i<n_off; ++i)
                    elem2[off[i]-1]=0;
                k=0;
                for (i=0; i<max; ++i)
                    if (elem2[i]!=0) elem2[k++]=elem2[i];
                parts_by_off=k;
                partitions2(); return(1);
                }

            }

        n=atoi(par[0]);
        if (npar==1)
            { all=1; m1=1; m2=n; }
        else
            { all=0; m0=m=atoi(par[1]); m1=m2=m; }

        raj2=0;
        i=spfind("POWER");
        if (i>=0)
            {
            power=atoi(spb[i]); ++rajoitukset; ++raj2;
            for (i=1; i<=MAXELEM-1; ++i)
                {
                k=1; for (j=0; j<power; ++j) k*=i;
                if (k>n) break;
                pot[i]=k;
                }
            }

        i=spfind("SURVO");  // 11.11.2006
        if (i>=0)
            {
            int sum,k,kk;

            survo=atoi(spb[i]); ++rajoitukset;
            sum=0; k=0; kk=0;
            for (i=0; i<n; ++i)
                {
                sum+=i+1; ++k;
                if (k==survo) { off[kk++]=sum; k=0; }
                if (kk==m) break;
                }
/*********************
printf("\nsurvo=%d n=%d",survo,n);
printf("\nsummat %d: ",kk);
for (i=0; i<kk; ++i) printf("%d ",off[i]); getch();
***********************/

            }

        line1=line;

        if (imax && imin && !raj2 && !all && !n_off && !idistinct)
            {
            min_max_partitions(n,m,min,max);
            return(1);
            }

        if (!imax && !imin && !raj2 && res==0 && all)
            {
            count_partitions(n,idistinct,&da);
            edwrite(space,line1,1);
            sprintf(sbuf,"Partitions of %d: N[%s]=%.15g",
                                   n,word[1],da);
            edwrite(sbuf,line1,1);
            return(1);
            }
        for (m=m1; m<=m2; ++m)
            {
            for (i=0; i<m-1; ++i) elem1[i]=1; elem1[m-1]=n-m+1;
            while (1)
                {
                if (rajoitukset)
                    {
                    ok=1;
                    if (imax && elem1[m-1]>max) ok=0;
                    if (imin && elem1[0]<min) ok=0;
                    if (greatest_part && elem1[m-1]!=greatest_part) ok=0; // 14.8.2006

                    if (idistinct)
                        {
                        for (i=0; i<m-1; ++i)
                            {
                            if (elem1[i]==elem1[i+1])
                                { ok=0; break; }
                            }

                        if (ok && survo) // 11.11.2006
                            {
                            int k;

                            k=0;
                            for (i=0; i<m; ++i)
                                {
                                k+=elem1[i];
//                 printf("\n%d %d",elem1[i],off[i]);
                                if (k<off[i]) { ok=0; break; }
                                }
//                 getch();
                            }

                        if (n_off) // 25.4.2006
                            {
                            int u,v;

                            for (i=0; i<m; ++i)
                              {
                              v=elem1[i];
                              for (u=0; u<n_off; ++u)
                                  {
                                  if (v==off[u]) { ok=0; break; }
                                  }
                              if (ok==0) break;
                              }
                            }
                        }
                    if (ok)
                        {
                        if (!raj2) print_comb(elem1,m,0);
                        else
                            {
                            for (i=0; i<m; ++i)
                                {
                                j=1;
                                while (elem1[i]>pot[j]) ++j;
                                if (elem1[i]<pot[j]) break;
                                }
                            if (i==m) print_comb(elem1,m,0);
                            }
                        }
                    }
                else print_comb(elem1,m,0);
                i=next_m_part(n,m,elem1);
                if (i<0) break;
                }
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        if (all)
            sprintf(sbuf,"Partitions of %d: N[%s]=%d",
                                   n,word[1],n_comb);
        else
            sprintf(sbuf,"Partitions %d of %d: N[%s]=%d",
                              m0,n,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int next_m_part(int n,int m,int *p)
        {
        int i,j,k;

        i=m-2;
        while (i>=0 && p[m-1]-p[i]<2) --i;
        if (i>-1)
            {
            for (j=m-2; j>=i; --j) p[j]=p[i]+1;
            k=0; for (j=0; j<m-1; ++j) k+=p[j];
            p[m-1]=n-k;
            return(1);
            }
        else return(-1);
        }

static double sum1=0.0;
static double n_log_m;

static double multin_prob1(int n,int m)
    {
    int i,k,k2,nk;
    double lcoeff1,lcoeff2,a;
//  double prob;

//  n=atoi(par[0]);
//  m=atoi(par[1]);
    sum1=0.0;
    lcoeff1=comb_lfact[m];
    lcoeff2=comb_lfact[n];
    k=elem1[0]; nk=1; lcoeff2-=comb_lfact[k];
    for (i=1; i<m; ++i)
        {
        k2=elem1[i]; lcoeff2-=comb_lfact[k2];
        if (k2==k) { ++nk; continue; }
        if (nk>1) lcoeff1-=comb_lfact[nk];
        k=k2; nk=1;
        }
    if (nk>1) lcoeff1-=comb_lfact[nk];
    a=exp(lcoeff1+lcoeff2-n_log_m);
    if (multin==2) sum1+=exp(lcoeff1);
    return(a);
    }

static int tee_comb_lfact(int n)
    {
    int i;

    comb_lfact=(double *)muste_malloc((n+1)*sizeof(double));
    if (comb_lfact==NULL) { not_enough_memory(); return(-1); }
    comb_lfact[0]=comb_lfact[1]=0.0;
    for (i=2; i<=n; ++i) comb_lfact[i]=log((double)i)+comb_lfact[i-1];
    return(1);
    }

static int min_max_partitions(int n,int m,int min,int max)
    {
    int i,max0;
    double nn;
    int line1;
    double prob;
    double prob0;
    int n0,i0;

    i=spfind("MULTIN"); if (i>=0) multin=atoi(spb[i]);
    if (multin)
        {
        tee_comb_lfact(n);
        n_log_m=(double)n*log((double)m);
        }

    n0=1000L;
    i=spfind("SUMSTEP");
    if (i>=0) n0=atol(spb[i]);
    if (n0<1L) n0=1L;

    line1=line;
    elem1[0]=min; i=0;
    nn=0.0;
    prob=prob0=0.0; i0=0L;
    while (1)
        {
        ++nn;
        next_min_max_partition(n,m,i+1,elem1[i],max);
        if(res) print_comb(elem1,m,0);
        if (multin)
            {
            prob0+=multin_prob1(n,m);
            ++i0;
            if (i0==n0)
                {
                prob+=prob0;
                i0=0L; prob0=0.0;
                }
            }
        max0=elem1[m-1];
        for (i=m-1; i>=0; --i)
            {
            if (elem1[i]<max0-1) break;
            }
        if (i<0) break;
        ++elem1[i];
        }

    prob+=prob0;
    if (!line || res==1) return(1);
    edwrite(space,line1,1);
    if (multin)
        {
        if (multin==1)
        sprintf(sbuf,"Restricted partitions of %d: N[%s]=%.15g P=%.15g",
                      n,word[1],nn,prob);
        else
        sprintf(sbuf,"Restricted partitions of %d: N[%s]=%.15g P=%.15g NM=%.15g",
                      n,word[1],nn,prob,sum1);
        }
    else
        sprintf(sbuf,"Restricted partitions of %d: N[%s]=%.15g",
                      n,word[1],nn);
    edwrite(sbuf,line1,1);
// printf("\nsum1=%.15g",sum1); getch();
    return(1);
    }

static int next_min_max_partition(int n,int m,int k,int min,int max)
    {
    int i,s,j;
    int ero;

    s=0; ero=max-min;
    for (i=0; i<k; ++i) s+=elem1[i];
    for (i=k; i<m; ++i) elem1[i]=elem1[k-1];
    s=n-s-(m-k)*elem1[k-1];
    j=s/ero;
// printf("\ns=%d j=%d",s,j); getch();
    for (i=m-1; i>m-j-1; --i) elem1[i]=max;
    elem1[m-j-1]+=s%ero;
// printf("\n");
// for (i=0; i<m; ++i) printf("%d ",elem1[i]); getch();


    return(1);
    }

static int partitions2()
        {
        char x[LLENGTH];
        int npart,s;
        int i,n,m,line1;
        int m0,m1,m2;
        int all;
        int idistinct;
        int k,h,ok;
        int max_elem0;

        idistinct=0; m0=0;
        i=spfind("DISTINCT");
        if (i>=0) idistinct=atoi(spb[i]);
        coeff=0;
        i=spfind("COEFF");
        if (i>=0) coeff=atoi(spb[i]);

        n=atoi(par[0]);
        if (npar==1)
            { all=1; m1=1; m2=n; }
        else if (npar==2)
            { all=0; m0=m=atoi(par[1]); m1=m2=m; }
        else
            { all=0; m0=m=atoi(par[1]); m1=m; m2=atoi(par[2]); }

        if (!parts_by_off)
            {
            i=spfind("PARTS");
            strcpy(x,spb[i]);
            npart=split(x,vv,MAXELEM);
            }
        else
            {
            npart=parts_by_off; vv[0]="";
            }
        if (muste_strnicmp(vv[0],"POWER",5)==0)
            {
            k=atoi(vv[1]);
            for (i=1; i<MAXELEM+1; ++i)
                {
                s=1; for (h=0; h<k; ++h) s*=i;
                if (s>n) break;
                elem2[i-1]=s;
                }
            if (npart>2)
                {
                k=atoi(vv[2]);
                for (h=0; h<MAXELEM+1; ++h) elem2[h]=elem2[h+k-1];
                npart=i-1-k+1;
                }
            else npart=i-1;
            }
        else if (!parts_by_off)
            for (i=0; i<npart; ++i) elem2[i]=atoi(vv[i]);
        ok=1;
        for (i=1; i<npart; ++i)
            if (elem2[i]<elem2[i-1]) { ok=0; break; }
        if (!ok)
            {
            sur_print("\nPARTS not in ascending order!");
            WAIT; return(-1);
            }

/*      if (idistinct)
            {
            s=0; i=0;
            while (s<=n && i<npart) { s+=elem2[i]; ++i; }
            if (i<m2) { if (i<npart) m2=i-1; else m2=i; }
            }
*/

        npart0=npart;

        line1=line;
        if (all)
            {
            if (res==0)
                {
                if (idistinct)
                    i=n_dist_part(n,npart);  /* recursively */
                else
                    {
                    i=n_part(n,npart);  /* recursively */
                    }
                sprintf(sbuf,"Partitions of %d: N[%s]=%d",
                                       n,word[1],i);
                edwrite(space,line1,1);
                edwrite(sbuf,line1,1);
                return(1);
                }
            else
                {
                if (idistinct)
                    {
                    i=n_dist_part_p(n,npart);  /* recursively */
                    }
                else
                    i=n_part_p(n,npart);  /* recursively */
                sprintf(sbuf,"Partitions of %d: N[%s]=%d",
                                       n,word[1],i);
                edwrite(space,line1,1);
                edwrite(sbuf,line1,1);
                return(1);
                }
            }

        if (idistinct && m1==m2 && fast) // 22.6.2006
            partitions_with_given_distinct_parts(n,m1,npart,elem2);
     else

        for (m=m1; m<=m2; ++m)
            {
            sprintf(sbuf,"%d ",m); sur_print(sbuf);

            if (m==1)  /* 24.7.1999 */
                {
                for (i=0; i<npart; ++i) if (n<=elem2[i]) break;
                if (i<npart && n==elem2[i])
                    {
                    elem1[0]=n;
                    print_comb(elem1,m,0);
                    }
                continue;
                }
/*********************************** rekur. yritys 25.7.1999 **
            if (res==0)              on hitaampi
                {
                n_comb+=n_part_k(n,npart,m);
                continue;
                }
**************************************/
            for (i=0; i<m-1; ++i) elem0[i]=0;
            for (i=0; i<npart; ++i) if (m*elem2[i]>n) break;
            max_elem0=i-1;

            s=n-(m-1)*elem2[0];
            for (i=0; i<npart; ++i) if (elem2[i]>s) break;
            npart=i;

            while (1)
                {
                if (elem0[0]>max_elem0) break;
                s=n; for (i=0; i<m-1; ++i) s-=elem2[elem0[i]];
                if (s>=elem2[elem0[m-2]])
                    {
                    for (i=elem0[m-2]; i<npart; ++i)
                        if (s==elem2[i])
                            {
                            elem0[m-1]=i;
                            for (k=0; k<m; ++k) elem1[k]=elem2[elem0[k]];
                            ok=1;
                            if (idistinct)
                                {
                                for (i=0; i<m-1; ++i)
                                    {
                                    if (elem1[i]==elem1[i+1])
                                        { ok=0; break; }
                                    }
                                }
                            if (ok) print_comb(elem1,m,0);
                            break;
                            }
                        else if (s<elem2[i]) break;
                    }
                i=m-2;
                while (i>=0 && elem0[i]==npart-1) --i;
                if (i<0) break;
                ++elem0[i]; s=elem0[i]; ++i;
                for ( ; i<m-1; ++i) elem0[i]=s;
                }
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        if (all)
            sprintf(sbuf,"Partitions of %d: N[%s]=%d",
                                   n,word[1],n_comb);
        else
            sprintf(sbuf,"Partitions %d of %d: N[%s]=%d",
                              m0,n,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int partitions_with_given_distinct_parts(int n,int m,int npart,int a[])  // 22.6.2006
    {
    int i,max,s,h;
    int *p[MAXPAR];
    int nk=0;

    max=a[npart-1];
    for (i=0; i<m-1; ++i) p[i]=&a[i];

    while (1)
        {
        s=0; for (i=0; i<m-1; ++i) s+=*p[i];
        s=n-s;
        if (s<=*p[m-2])
            {
          while (1)
              {
            h=m-2;
            while (p[h]-p[h-1]==1) --h;
            if (h==0) return(nk);
            --h;
            ++p[h];
            ++h;
            for ( ; h<m-1; ++h) p[h]=p[h-1]+1;
            s=0; for (i=0; i<m-1; ++i) s+=*p[i];
            s=n-s;
            if (s>*p[m-2]) break;
              }
            }
        if (s<=max && s>*p[m-2])
            {
            for (i=npart-1; i>=0; --i)
                {
                if (s==a[i])
                    {
                    ++nk;

                    p[m-1]=&a[i];
//                  printf("\n partitio:");
//                  for (h=0; h<m; ++h) printf(" %d",*p[h]);
                    for (h=0; h<m; ++h) elem1[h]=*p[h];
                    print_comb(elem1,m,0);

                    break;
                    }
                if (a[i]<*p[m-2]) break;
                }
            }
        ++p[m-2];
        }

    return(1);
    }


static int n_part(int n,int m)
        {
        int s,t;

        if (n==0) return(1);
        if (m==1) { if (n%elem2[0]==0) return(1); else return(0); }
        s=0; t=elem2[m-1];
        while (n>=0)
            {
            s+=n_part(n,m-1);
            n-=t;
            }
        return(s);
        }

static int n_dist_part(int n,int m)
        {
        int s,t;

        if (n==0) return(1);
        if (m==1) { if (n==elem2[0]) return(1); else return(0); }
        s=0; t=elem2[m-1];
        s+=n_dist_part(n,m-1);
        if (n>=t) s+=n_dist_part(n-t,m-1);
        return(s);
        }

static int n_part_p(int n,int m)
        {
        int s,t;

        if (n==0) { for (s=0; s<m; ++s) elem0[s]=0; print_n_part();  return(1); }
        if (m==1)
            {
            if (n%elem2[0]==0)
                {
                elem0[0]=n/elem2[0];
                print_n_part(); return(1);
                }
            else return(0);
            }
        s=0; t=elem2[m-1]; elem0[m-1]=0;
        while (n>=0)
            {
            s+=n_part_p(n,m-1);
            n-=t; if (n>=0) ++elem0[m-1];
            }
        return(s);
        }

static int n_dist_part_p(int n,int m)
        {
        int s,t;

        if (n==0) { for (s=0; s<m; ++s) elem0[s]=0; print_n_part();  return(1); }
        if (m==1)
            {
            if (n==elem2[0])
                {
                elem0[0]=1;
                print_n_part(); return(1);
                }
            else return(0);
            }

        s=0; t=elem2[m-1];
        elem0[m-1]=0; s+=n_dist_part_p(n,m-1);
        if (n>=t) { elem0[m-1]=1; s+=n_dist_part_p(n-t,m-1); }
        return(s);
        }

static int print_n_part()
        {
        int i,j,k;

        if (coeff)
            {
            print_comb(elem0,npart0,0);
            return(1);
            }
        j=0; k=0;
        for (i=0; i<npart0; ++i)
            {
            for (j=0; j<elem0[i]; ++j)
                elem1[k++]=elem2[i];
            }
        print_comb(elem1,k,0);
        return(1);
        }


static int count_partitions(int n,int distinct,double *pa)
        {
        int i,j,k;
        int n1;
        double *f1,*f2;
        int ii;
		f1=NULL; // RS ADD

        n1=n+1;
        freq=(double *)muste_malloc(n1*sizeof(double));
        if (freq==NULL) { not_enough_memory(); return(-1); }
        freq3=(double *)muste_malloc(n1*sizeof(double));
        if (freq3==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<n1; ++i) freq[i]=0.0;
        freq[0]=1.0;
        k=1;
        if (distinct)
            for (i=1; i<n1; ++i)
                {
                if (k) { f1=freq3; f2=freq; k=0; }
                else   { f2=freq3; f1=freq; k=1; }
                for (j=0; j<n1; ++j) f1[j]=f2[j];
                for (ii=0; ii<=n-i; ++ii)
                    f1[ii+i]+=f2[ii];
                }
        else /* not distinct */
            for (i=1; i<n1; ++i)
                {
                if (k) { f1=freq3; f2=freq; k=0; }
                else   { f2=freq3; f1=freq; k=1; }
                for (j=0; j<n1; ++j) f1[j]=f2[j];
                for (j=i; j<=n; j+=i)
                    for (ii=0; ii<=n-j; ++ii)
                        f1[ii+j]+=f2[ii];
                }
        *pa=f1[n];
        return(1);
        }

static int distributions()
        {
        int i,n,m,line1;
        int m0=0,m1,m2; // RS ADD init
        int all;
        int rajoitukset;
        int max=0,min=0; // RS ADD init
        int imax,imin,idistinct;
        int ok;
 //     int maxi,mini;
        char x[LLENGTH];

        rajoitukset=0;
        imax=imin=idistinct=0;

        if (npar>1) m=atoi(par[1]); else m=atoi(par[0]);

        i=spfind("MAX");
            {
            if (i>=0)
              {
              strcpy(x,spb[i]); n=split(x,vv,MAXELEM);
              imax=m;
              if (n==1)
                  {
                  max=atoi(spb[i]);
                  if (npar>1) for (i=0; i<m; ++i) vmax[i]=max;
                  }
              else for (i=0; i<n; ++i) vmax[i]=atoi(vv[i]);
              ++rajoitukset;
              }
            }
        i=spfind("MIN");
            {
            if (i>=0)
              {
              strcpy(x,spb[i]); n=split(x,vv,MAXELEM);
              imin=m;
              if (n==1)
                  {
                  min=atoi(spb[i]);
                  if (npar>1) for (i=0; i<m; ++i) vmin[i]=min;
                  }

              else for (i=0; i<n; ++i) vmin[i]=atoi(vv[i]);
              ++rajoitukset;
              }
            }

/*
        i=spfind("MAX");
        if (i>=0) { imax=1; max=atoi(spb[i]); ++rajoitukset; }
        i=spfind("MIN");
        if (i>=0) { imin=1; min=atoi(spb[i]); ++rajoitukset; }
*/
        i=spfind("DISTINCT");
        if (i>=0) { idistinct=atoi(spb[i]); rajoitukset+=idistinct; }
        n=atoi(par[0]);
        if (npar==1)
            { all=1; m1=1; m2=n; }
        else
            { all=0; m0=m=atoi(par[1]); m1=m2=m; }

        line1=line;

        if (rajoitukset && !all) { distributions2(n,m); return(1); }


        for (m=m1; m<=m2; ++m)
            {
            for (i=0; i<m-1; ++i) elem1[i]=0; elem1[m-1]=n;
            while (1)
                {
                if (rajoitukset)
                    {
                    ok=1;
                    if (imax)
                        {
                        if (imax>1)
                            {
                            for (i=0; i<m; ++i) if (elem1[i]>vmax[i]) { ok=0; break; }
                            }
                        else
                            for (i=0; i<m; ++i) if (elem1[i]>max) { ok=0; break; }
                        }
                    if (imin && ok)
                        {
                        if (imin>1)
                            {
                            for (i=0; i<m; ++i) if (elem1[i]<vmin[i]) { ok=0; break; }
                            }
                        else
                            for (i=0; i<m; ++i) if (elem1[i]<min) { ok=0; break; }
                        }
                    if (ok) print_comb(elem1,m,0);
                    }
                else print_comb(elem1,m,0);
                i=next_m_distr(n,m,elem1);
                if (i<0) break;
                }
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        if (all)
            sprintf(sbuf,"Distributions of %d elements: N[%s]=%d",
                                   n,word[1],n_comb);
        else
            sprintf(sbuf,"Distributions of %d elements into %d cells: N[%s]=%d",
                              n,m0,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int next_m_distr(int n,int m,int *elem1)
        {
        int i,k;

        i=m-1;
        while (elem1[i]==0) --i;
        if (i==0) return(-1);
        ++elem1[i-1];
        elem1[m-1]=elem1[i]-1;
        for (k=i; k<m-1; ++k) elem1[k]=0;
        return(1);
        }


/******************************************************
F(N,M)|=if(M*min>N)then(0)else(f1(N,M))
f1(N,M)|=if(M*max<N)then(0)else(f2(N,M))
f2(N,M)|=if(M=1)then(1)else(F1(N,M))
F1(N,M)|=for(I=min)to(max)sum(exp(lfact(N)-lfact(I)-lfact(N-I)&
                              +I*log(1/M)+(N-I)*log(1-1/M))*F(N-I,M-1))

min=180 max=220

TIME COUNT START
F(1200,6)=0.5207990703723
TIME COUNT END   11.320
*********************************************************/






static double laske_multin_prob(int n,int m)  // samat p:t ja rajat
    {
    int i,ii;
    double lm,lm1,a;

    if (m*multin_min>n || m*multin_max<n) return(0.0);
    if (m==1) return(1.0);
// ++count1;
    ii=(m-1)*nnn+n-1;
    a=mlt_prob[ii]; if (a>=0.0) return(a);
    lm=log((double)m); lm1=log(1.0-1.0/(double)m);
    a=0.0;
    for (i=multin_min; i<=multin_max; ++i)
        a+=exp(comb_lfact[n]-comb_lfact[i]-comb_lfact[n-i]-i*lm+(n-i)*lm1)
                              *laske_multin_prob(n-i,m-1);
    mlt_prob[ii]=a;
// ++count2;
// printf("\nn=%d m=%d p=%g",n,m,a); getch();
    return(a);
    }

static double laske_multin_prob2(int n,int m) // eri p:t, samat rajat
    {
    int i,ii;
    double pr1,a;
// double b,d;

    if (m*multin_min>n || m*multin_max<n) return(0.0);
    if (m==1) return(1.0);

    pr1=0.0; for (i=0; i<m; ++i) pr1+=mlt_pr[i];
    pr1=mlt_pr[m-1]/pr1;

    ii=(m-1)*nnn+n-1;
// printf("\nm=%d n=%d nnn=%d ii=%d|",m,n,nnn,ii);
    a=mlt_prob[ii]; if (a>=0.0) return(a);
    a=0.0;
    for (i=multin_min; i<=multin_max; ++i)
        a+=exp(comb_lfact[n]-comb_lfact[i]-comb_lfact[n-i]
               +i*log(pr1)+(n-i)*log(1.0-pr1))
                              *laske_multin_prob2(n-i,m-1);
/*********************************
        {
        b=exp(comb_lfact[n]-comb_lfact[i]-comb_lfact[n-i]
               +i*log(pr1)+(n-i)*log(1.0-pr1));
        d=laske_multin_prob2(n-i,m-1);
        a+=b*d;
printf("\ni=%d b=%g d=%g|",i,b,d);
        }
***********************************/
    mlt_prob[ii]=a;
// printf("\nn=%d m=%d p=%g",n,m,a); getch();
    return(a);
    }


static double laske_multin_prob3(int n,int m) // eri p:t ja eri rajat
    {
    int i,ii;
    double pr1,a;

    ii=0; for (i=0; i<m; ++i) ii+=mlt_min[i];
    if (ii>n) return(0.0);
    ii=0; for (i=0; i<m; ++i) ii+=mlt_max[i];
    if (ii<n) return(0.0);

    if (m==1) return(1.0);

    pr1=0.0; for (i=0; i<m; ++i) pr1+=mlt_pr[i];
    pr1=mlt_pr[m-1]/pr1;

    ii=(m-1)*nnn+n-1;

    a=mlt_prob[ii]; if (a>=0.0) return(a);
    a=0.0;
    for (i=mlt_min[m-1]; i<=mlt_max[m-1]; ++i)
        a+=exp(comb_lfact[n]-comb_lfact[i]-comb_lfact[n-i]
               +i*log(pr1)+(n-i)*log(1.0-pr1))
                              *laske_multin_prob3(n-i,m-1);
    mlt_prob[ii]=a;
// printf("\nn=%d m=%d p=%g",n,m,a); getch();
    return(a);
    }

static int multin_comp()
    {
    int n,m,i;
    double a;

    i=spfind("PROB"); if (i>=0)  return(multin_comp2());

    n=atoi(par[0]);
    m=atoi(par[1]);
    nnn=n; // 12.7.2001
// printf("\nn=%d m=%d",n,m); getch();

    multin_min=0; i=spfind("MIN"); if (i>=0) multin_min=atoi(spb[i]);
    multin_max=n; i=spfind("MAX"); if (i>=0) multin_max=atoi(spb[i]);

    mlt_prob=(double *)muste_malloc(m*n*sizeof(double));
    if (mlt_prob==NULL) { not_enough_memory(); return(-1); }
    for (i=0; i<m*n; ++i) mlt_prob[i]=-1.0;
    tee_comb_lfact(n);

    a=laske_multin_prob(n,m);

// printf("\na=%.15g",a); getch();

    sprintf(sbuf,"Distributions of %d elements into %d cells: P[%s]=%.15g",
                          n,m,word[1],a);
    edwrite(space,line,1);
    edwrite(sbuf,line,1);
// printf("\ncount1=%.15g count2=%.15g|",count1,count2); getch();


    return(1);
    }


static int multin_comp2()
    {
    int n,m,i;
    double a;
    char *p;


    n=atoi(par[0]);
    m=atoi(par[1]);
    nnn=n; // 12.7.2001
    i=spfind("PROB");
    mlt_pr=(double *)muste_malloc(m*sizeof(double));
    if (mlt_pr==NULL) { not_enough_memory(); return(-1); }

    strcpy(sbuf,spb[i]);
    i=split(sbuf,vv,m);

    if (i<m)
        {
        sur_print("\nError in PROB!"); WAIT;
        return(-1);
        }

    for (i=0; i<m; ++i)
        {
        p=strchr(vv[i],'/');   // esim. 1/6 sallittu
        if (p==NULL) mlt_pr[i]=atof(vv[i]);
        else
            {
            *p=EOS;
            mlt_pr[i]=atof(vv[i])/atof(p+1);
            }
        }

    i=spfind("MIN");
    if (i>=0)
        {
        strcpy(sbuf,spb[i]);
        i=split(sbuf,vv,m);
        if (i==m)
            {
            return(multin_comp3());
            }
        }

    multin_min=0; i=spfind("MIN"); if (i>=0) multin_min=atoi(spb[i]);
    multin_max=n; i=spfind("MAX"); if (i>=0) multin_max=atoi(spb[i]);

    mlt_prob=(double *)muste_malloc(m*n*sizeof(double));
    if (mlt_prob==NULL) { not_enough_memory(); return(-1); }
    for (i=0; i<m*n; ++i) mlt_prob[i]=-1.0;
    tee_comb_lfact(n);

    a=laske_multin_prob2(n,m);

// printf("\na=%.15g",a); getch();

    sprintf(sbuf,"Distributions of %d elements into %d cells: P[%s]=%.15g",
                          n,m,word[1],a);
    edwrite(space,line,1);
    edwrite(sbuf,line,1);

    return(1);
    }

static int multin_comp3()
    {
    int n,m,i;
    double a;

    n=atoi(par[0]);
    m=atoi(par[1]);
    nnn=n;
    mlt_min=(int *)muste_malloc(m*sizeof(int));
    if (mlt_min==NULL) { not_enough_memory(); return(-1); }
    mlt_max=(int *)muste_malloc(m*sizeof(int));
    if (mlt_max==NULL) { not_enough_memory(); return(-1); }

    i=spfind("MIN");
    if (i<0) for (i=0; i<m; ++i) mlt_min[i]=0;
    else
        {
        strcpy(sbuf,spb[i]);
        i=split(sbuf,vv,m);
        if (i==1) for (i=0; i<m; ++i) mlt_min[i]=atoi(vv[0]);
        else if (i>=m)
            for (i=0; i<m; ++i) mlt_min[i]=atoi(vv[i]);
        else { sur_print("\nError in MIN!"); WAIT; return(-1); }
        }
    i=spfind("MAX");
    if (i<0) for (i=0; i<m; ++i) mlt_max[i]=0;
    else
        {
        strcpy(sbuf,spb[i]);
        i=split(sbuf,vv,m);
        if (i==1) for (i=0; i<m; ++i) mlt_max[i]=atoi(vv[0]);
        else if (i>=m)
            for (i=0; i<m; ++i) mlt_max[i]=atoi(vv[i]);
        else { sur_print("\nError in MAX!"); WAIT; return(-1); }
        }

    mlt_prob=(double *)muste_malloc(m*n*sizeof(double));
    if (mlt_prob==NULL) { not_enough_memory(); return(-1); }
    for (i=0; i<m*n; ++i) mlt_prob[i]=-1.0;
    tee_comb_lfact(n);

    a=laske_multin_prob3(n,m);

// printf("\na=%.15g",a); getch();

    sprintf(sbuf,"Distributions of %d elements into %d cells: P[%s]=%.15g",
                          n,m,word[1],a);
    edwrite(space,line,1);
    edwrite(sbuf,line,1);

    return(1);
    }


static double multin_prob2(int n,int m)
    {
    int i;
    double lprob;

    lprob=comb_lfact[n];
    for (i=0; i<m; ++i)
        lprob+=(double)elem1[i]*dprob[i]-comb_lfact[elem1[i]];
    return(exp(lprob));
    }

static int distributions2(int n,int m) // 13.6.2001
    {
    int i,j=0; // RS ADD init
    double nn;
    int line1;
    double prob;
    char *p;
    int n0,i0;
    double prob0,prob1;

    multin=0;
    i=spfind("PROB");
    if (i>=0)
        {
        multin=1;
        strcpy(sbuf,spb[i]);
        i=split(sbuf,vv,m);
        if (i<m)
            {
            sur_print("\nError in PROB!");
            WAIT; return(-1);
            }
        dprob=(double *)muste_malloc(m*sizeof(double));
        if (dprob==NULL) { not_enough_memory(); WAIT; return(-1); }
        for (i=0; i<m; ++i)
            {
            p=strchr(vv[i],'/');   // esim. 1/6 sallittu
            if (p==NULL) dprob[i]=atof(vv[i]);
            else
                {
                *p=EOS;
                dprob[i]=atof(vv[i])/atof(p+1);
                }
            }
        // ei tarkistusta, ett„ summa=1
        for (i=0; i<m; ++i) dprob[i]=log(dprob[i]);
        tee_comb_lfact(n);
        }

    n0=1000L;
    i=spfind("SUMSTEP");
    if (i>=0) n0=atol(spb[i]);
    if (n0<1L) n0=1L;

    line1=line;
    nn=0.0; i0=0L; prob=prob0=0.0;
    set_distr(n,m,0);
    while (1)
        {
        i=print_comb(elem1,m,0);
        ++nn;
        if (multin)
            {
            prob1=multin_prob2(n,m);

            if (res && line && line<=r2)
                {
                sprintf(sbuf,"%.15e",prob1);
                edwrite(sbuf,line,i+1);
                }

            prob0+=prob1;
            ++i0;
            if (i0==n0)
                {
//              printf("\nsuhde=%e",prob1/prob0); getch();
//              hyvin ep„tasaisia arvoja!
                prob+=prob0; prob0=0.0; i0=0L;
                }
            }
        for (i=m-2; i>=0; --i)
            {
            if (elem1[i]==vmax[i])
                {
                if (i==0) { j=-1; break; }
                else continue;
                }
            ++elem1[i];
            j=set_distr(n,m,i+1);
            if (j==1) break;
            }
        if (j<0) break;
        }
    prob+=prob0;
    edwrite(space,line1,1);
    if (multin)
        sprintf(sbuf,"Distributions of %d elements into %d cells: N[%s]=%.15g P=%.15g",
                          n,m,word[1],nn,prob);
    else
        sprintf(sbuf,"Distributions of %d elements into %d cells: N[%s]=%.15g",
                          n,m,word[1],nn);

    edwrite(sbuf,line1,1);
    return(1);
    }

static int set_distr(int n,int m,int k)
    {
    int i,s,h;

    s=0; for (i=0; i<k; ++i) s+=elem1[i];
    for (i=k; i<m; ++i) { elem1[i]=vmin[i]; s+=vmin[i]; }
    n-=s;

// printf("\n");
// for (i=0; i<m; ++i) printf("%d ",elem1[i]);
// printf("\nn=%d k=%d",n,k); getch();

    if (n<0) return(-1);
    for (i=m-1; i>=0; --i)
        {
        if (n==0) break;
        elem1[i]+=n;
        h=elem1[i]-vmax[i];
        if (h>0) { n=h; elem1[i]=vmax[i]; }
        else break;
        }

    return(1);
    }

static int integers()
        {
        int i,n,m,line1,k;
        int move;
        int sum,sum_ind;
        int gap;

		sum=0; // RS ADD
        n=atoi(par[0]);
        if (n<=0) return(-1);
        m=atoi(par[1]);
        if (m<=1) return(-1);

        move=0;
        i=spfind("MOVE"); if (i>=0) move=atoi(spb[i]);
        sum_ind=0;
        i=spfind("SUM"); if (i>=0) { sum=atoi(spb[i]); sum_ind=1; }
        gap=0;
        i=spfind("MIN_GAP"); if (i>=0) gap=atoi(spb[i]);


        line1=line;
        for (i=0; i<n; ++i) elem1[i]=0;
        while (1)
            {
            if (sum_ind)
                {
                k=0; for (i=0; i<n; ++i) k+=elem1[i];
                if (move) k+=n*move;
                if (k!=sum)
                     {
                     i=next_integer(n,m,elem1);
                     if (i<0) break; else continue;
                     }
                }
            if (move)
                {
                for (i=0; i<n; ++i) elem2[i]=elem1[i]+move;
                print_comb(elem2,n,0);
                }
            else print_comb(elem1,n,0);
            if (gap)
                i=next_integer_with_gap(n,m,elem1,gap);
            else
                i=next_integer(n,m,elem1);
            if (i<0) break;
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        sprintf(sbuf,"Integers of %d digits in base %d: N[%s]=%d",
                          n,m,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int next_integer(int n,int m,int *elem1)
        {
        int i;

        i=n-1;
        while (elem1[i]==m-1 && i>=0) --i;
        if (i<0) return(-1);
        ++elem1[i];
        ++i; for (; i<n; ++i) elem1[i]=0;
        return(1);
        }

static int next_integer_with_gap(int n,int m,int *elem1,int gap)  // 7.8.2004
        {
        int i,ero;

        while (1)
            {
            i=n-1;
            while (elem1[i]==m-1 && i>=0) --i;
            if (i<0) return(-1);
            ++elem1[i];
            ++i; for (; i<n; ++i) elem1[i]=0;
            for (i=1; i<n; ++i)
                {
                ero=elem1[i]-elem1[i-1];
                if (ero<=0) continue;
                if (ero<gap) break;
                }
            if (i==n) return(1);
            }

        return(1);
        }

static int lattices()
        {
        int i,n,line1;
        char x[LLENGTH];

        n=atoi(par[0]);
        if (n<=0) return(-1);

        i=spfind("MIN");
        if (i<0) { spec_error("MIN"); return(-1); }
        strcpy(x,spb[i]); i=split(x,vv,n);
        if (i<n) { spec_error("MIN"); return(-1); }
        for (i=0; i<n; ++i) vmin[i]=atoi(vv[i]);

        i=spfind("MAX");
        if (i<0) { spec_error("MAX"); return(-1); }
        strcpy(x,spb[i]); i=split(x,vv,n);
        if (i<n) { spec_error("MAX"); return(-1); }
        for (i=0; i<n; ++i) vmax[i]=atoi(vv[i]);

        line1=line;
        for (i=0; i<n; ++i) elem1[i]=vmin[i];
        while (1)
            {
            print_comb(elem1,n,0);
            i=next_lattice_point(n,elem1);
            if (i<0) break;
            }
        if (!line || res==1) return(1);
        edwrite(space,line1,1);
        sprintf(sbuf,"Lattice points in %d dimensions: N[%s]=%d",
                          n,word[1],n_comb);
        edwrite(sbuf,line1,1);
        return(1);
        }

static int next_lattice_point(int n,int *elem1)
        {
        int i;

        i=n-1;
        while (elem1[i]==vmax[i] && i>=0) --i;
        if (i<0) return(-1);
        ++elem1[i];
        ++i; for (; i<n; ++i) elem1[i]=vmin[i];
        return(1);
        }
// PRINT
static int print_comb(int *p,int k,int s)
        {
        int i,h;

        ++n_comb;
        if (line) ++line;
        if (line && (res==0 || line>r2)) return(1);
        h=0;
        for (i=0; i<k; ++i)
            {
            switch (s)
                {
            case 1:
                h+=sprintf(sbuf+h,"%s ",symbol[p[i]]); break;
            case 0:
                h+=sprintf(sbuf+h,"%d ",p[i]); break;
            case 2:
                h+=sprintf(sbuf+h,"%s ",symbol2[p[i]]); break;
                }
            if (h>LLENGTH-10) break;
            }
        if (!line)
            {
            sbuf[h-1]=EOS; /* ylim. sp pois */
            fprintf(txt,"%s\n",sbuf);
            return(1);
            }
        edwrite(space,line,1);
        edwrite(sbuf,line,1);
        return(h); // 25.6.2001
        }

static int spec_error(char *s)
        {
        sprintf(sbuf,"\nMissing or incomplete specification %s!",s);
        sur_print(sbuf); WAIT; return(1);
        }

static int load_restr_matrix(int n)
        {
        int i;

        i=matrix_load(matname,&aa,&rdim,&cdim,&rlab,&clab,&lr,&lc,&mtype,expr);
        if (i<0) return(-1);
        if (cdim!=rdim || rdim!=n)
            {
            sprintf(sbuf,"\nSize of restriction matrix %s must be %d!",
                  matname,n);
            sur_print(sbuf); WAIT; return(-1);
            }

        restr_mat=(int *)aa;
        for (i=0; i<n*n; ++i) restr_mat[i]=aa[i];
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT; return(1);
        }

static int find_first()
        {
        int i,k;
        char x[LLENGTH];
        char y[LLENGTH];
        char *p,*q;

        i=edline2(par[0],1,1); if (i<0) return(-1);
        edread(x,i);
        i=strlen(x)-1; while (i>0 && x[i]==' ') x[i--]=EOS;
        p=x+1; /* string to be searched for */
        while (1)
            {
            edread(y,line++);
            if (empty_line(y+1,c2)) return(1);
            q=strstr(y,p);
            if (q==NULL) i=-1;
            else i=q-y+2;
            k=strlen(y)-1; while (k>0 && y[k]==' ') --k;
            k+=2;
            if (i>0) sprintf(sbuf,"%4d",i); else strcpy(sbuf,"   -");
            edwrite(sbuf,line-1,k);
            }
        return(1);
        }

/* egypt.c 7.1.2005  (8.1.2005)
*/

typedef unsigned int num;

#define MAXN 100
#define MAXJ 2147483647

static num terms0[MAXN];
static num terms1[MAXN];
static num aaa[MAXN];
static num bb[MAXN];
static num cc[MAXN];
static num minj;
static num nmax; // lis„ys 7.1.2005
static num nmin; // lis„ys 8.1.2005
static num count=0;
static int lopeta=0;

static FILE *list;
static char name[64];
static int save=0;
static num max_term;
static double sum0,sum;
static int brief=0;

extern char **spb;
extern int scroll_line;
/***********************************
char *specs_eg[]={ "NMIN", "NMAX", "SAVE", "SHORT",
                    "!" };
extern char **specs;
************************************/

static num egy_gcd(num a, num b)
    {
    if (b==0) return(a);
    return(egy_gcd(b,a%b));
    }

static void sub(num ax,num ay,num bx,num by,num *cx,num *cy)
   {
   *cx=ax*by-bx*ay;
   *cy=ay*by;
   }

void fill(num gx,num gy,num *prx,num *pry,int n,int i,int m)
    {
    int h;
    num j,k;
    num cx,cy;
    int s;
    int kelpaa=0;
    int hh;
    int d,hmin;

    if (lopeta) return;
    hmin=0; d=0;
    ++count;
    if (count>100000)
        {
        count=0;
        if (sur_kbhit()) { sur_getch(); lopeta=1; return; }
        }

    if (i==n)
        {
        if (gx==0)
            {
            for (h=0; h<n; ++h)
                {
//              printf("%u/%u ",prx[h],pry[h]);
                aaa[h]=pry[h];
                }
            for (h=0; h<n; ++h)
                {
                d=MAXJ;
                for (s=0; s<n; ++s)
                    {
                    if (aaa[s]<d) { hmin=s; d=aaa[s]; }
                    }
                bb[h]=d; aaa[hmin]=MAXJ;
                if (h>0 && d==bb[h-1]) break;
                }

            if (h==n)
                {
                kelpaa=0;
                sum=0.0;
                for (h=n-1; h>=0; --h) // parempi tarkkuus?
                    sum+=(double)1.0/(double)bb[h];
                if(fabs(sum-sum0)/sum0<1e-15) kelpaa=1;
// kelpaa=1;
                if (save && kelpaa && d<=max_term)
                    {
                    for (h=0; h<n; ++h)
                        {
                        fprintf(list,"%u",bb[h]);
                        if (h==n-1) fprintf(list,"\n");
                        else fprintf(list," ");
                        }
                    }
                if (d<=minj && kelpaa)
                    {
//                  minj=d;

                    if (d==minj) // 12.1.2005
                        for (h=n-2; h>=0; --h)
                            {
                            if (bb[h]<cc[h])
                                {
                                for (hh=h; hh>=0; --hh)
                                    cc[hh]=bb[hh];
                                break;
                                }
                            else if (bb[h]>cc[h]) break;
                            }
                    else
                        {

                        for (h=0; h<n; ++h) cc[h]=bb[h];

                        s=sprintf(sbuf,"\n");
                        for (h=0; h<n; ++h)
                            {
                            s+=sprintf(sbuf+s,"1/%u",cc[h]);
                            if (h<n-1) s+=sprintf(sbuf+s,"+");
                            }
                        sur_print(sbuf);
                        }
                    minj=d;
                    }
                }
            }
        return;
        }
    if (gx==0) return;
    j=(gy+gx-1)/gx;
    k=j;
    if (k<m) k=m;
    if (k<nmin) k=nmin;

    while (k<=j*(n-i) && k<=nmax)
        {
        prx[i]=1; // prx[] tarpeeton!?
        pry[i]=k;
        sub(gx,gy,prx[i],pry[i],&cx,&cy);
        d=egy_gcd(cx,cy);
        cx=cx/d; cy=cy/d;
        fill(cx,cy,prx,pry,n,i+1,k+1);
        if (lopeta) break; // 22.1.2005
        ++k;
        }
    }

static void egypt()
    {
    int i,j,h,s;
    num goal1,goal2;
    int nterms;
    char x[LLENGTH];
    char *p;
    char *w[2];

    if (g<3)
        {
        sur_print("\nUsage: EGYPT p/q,n_terms,L");
        WAIT; return;
        }

    count=0;
    lopeta=0;
    save=0;
    brief=0;

    i=spec_init(r1+r-1); if (i<0) return;
    sprintf(sbuf,"\n%s as Egyptian fraction:   ",word[1]);
    sur_print(sbuf);
    ++scroll_line; // display one step downwards!

    strcpy(x,word[1]);
    p=strchr(x,'/');
    if (p==NULL)
        {
        sur_print("\n/ missing!");
        WAIT; return;
        }
    *p=EOS;
    goal1=atoi(x);
    goal2=atoi(p+1);
    if (goal1==0 || *x=='-' || goal2==0 || *(p+1)=='-')
        {
        sur_print("\nOnly positive ratios accepted!");
        WAIT; return;
        }
    sum0=(double)goal1/(double)goal2;

    nterms=atoi(word[2]);

    nmin=1;
    i=spfind("NMIN"); if (i>=0) nmin=atoi(spb[i]);
    nmax=2147483647;
    i=spfind("NMAX"); if (i>=0) nmax=atoi(spb[i]);
    minj=MAXJ;
    for (i=0; i<nterms; ++i) cc[i]=MAXJ;

    i=spfind("SAVE");
    if (i>=0)
        {
        strcpy(x,spb[i]); i=split(x,w,2);
        strcpy(name,w[0]);
        max_term=MAXJ; if (i==2) max_term=atol(w[1]);

        list=muste_fopen(name,"wt");
        fprintf(list,"Denominators of Egyptian fractions for %s:\n",
                            word[1]);
        save=1;
        }
    i=spfind("SHORT"); // 14.1.2005
    if (i>=0) brief=atoi(spb[i]);

    fill(goal1,goal2,terms0,terms1,nterms,0,2);

    if (minj==MAXJ)
        sprintf(sbuf,"No solution found!");
    else
        {
        if (brief)
            {
            s=0;
            for (h=0; h<nterms; ++h)
                s+=sprintf(sbuf+s,"%u ",cc[h]);
            }
        else
            {
            s=sprintf(sbuf,"%s=",word[1]);
            for (h=0; h<nterms; ++h)
                {
                s+=sprintf(sbuf+s,"1/%u",cc[h]);
                if (h<nterms-1) s+=sprintf(sbuf+s,"+");
                }
            }
        }
    if (lopeta==1) strcat(sbuf," ???");
    --scroll_line;
    j=r1+r-1;
    if (g>3)
        {
        j=edline2(word[3],1,0);
        if (j==0) j=r1+r-1;
        }
    if (j==r1+r-1)
        {
        edread(x,j);
        h=ed1-1; while(x[h]==' ') x[h--]=EOS;
        p=strstr(x," /");
        if (p!=NULL) *p=EOS;
        strcat(x," / ");
        strcat(x,sbuf);
        edwrite(space,j,1);
        edwrite(x,j,0);
        }
    else
        {
        edwrite(space,j,1);
        edwrite(sbuf,j,1);
        }
    if (save) muste_fclose(list);
    return;
    }

