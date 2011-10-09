/* !markov.c 30.9.1986/SM (4.10.1986) (15.8.1996)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <time.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static int degree;
static int l1,l2;
static int results_line;
static char *mstart,*mend;
static char delete[LLENGTH];
static char *mspace;
static char line_end[16];
static unsigned int tila;
static char *prev;

/* STUDY  ja PROB ja MATRIX */
static double *aa;
static char *rlab,*clab;
static int rdim,cdim,lr,lc,mtype;
static char expr[LLENGTH];
static char matname[LNAME];
static int *ip1,*ip2,*ip3,*ip4;
static int m,m2;
static double tol_svd=1e-12;
extern int spn;
extern char **spa;
static char maski[]="0123456789";

static double *ps2,*aa2;
static double *dd,*vv;
static double *qq,*qq2,*ppn;
static char *plab;
static double *ps;

static int varaa_tilat();
static int markov();
static char *haku(char *state,char *p,int degree);
static int print_line(char *line);
static int comp_matrix(char *mat);
static int seuraava_tila();
static int next_lattice_point(int n,int *elem1,int vmax);
static int simul_matrix(char *mat);
static int study();
static int svd_study();
static int study_results(int k,int svd);
static char *lyh(char *s);
static int ei_tilaa();
static int p_error();
static int n_step_probabilities();
static int p_label(char *tila,int m,char *lab,int k);
static int is_integer(char *x);
static int numlab2(char *lab,int n,int len,int base);
static int markov_teach();
static int laske_tn(char *s,double *ptn);
static int arvonta(char *sana);
static int muunna(double muunnos,char *sana);
static int state_nr(unsigned char ch);
static int mprint2(double *aa,int m,int n);
static int make_suggestion(char *vast,char *sana);
static int n_xor(int n,char *x1,char *x2,char *p);

/******************
XOR CUR+1,CUR+2,CUR+3
01234
TEACH
dtsp|
***********************/
static char t_teach[]="dtsp|";

void muste_markov(char *argv)
        {
        int i;

        s_init(argv);

        i=spec_init(r1+r-1); if (i<0) return;
        spec_rnd();

        if (g==1)
            {
            sur_print("\nUsage: MARKOV L1,L2");
            sur_print("\nVarious options: See MARKOV?");
            WAIT; return;
            }

    mspace=NULL;
    aa=NULL;
    rlab=NULL;
    clab=NULL;
    ip1=NULL;
    ip2=NULL;
    ip3=NULL;
    ip4=NULL;
    dd=NULL;
    vv=NULL;
    qq=NULL;
    qq2=NULL;
    ppn=NULL;
    plab=NULL;
    aa=NULL;
    ps=NULL;
    aa2=NULL;
    ps2=NULL;


        if (muste_strcmpi(word[1],"STUDY")==0)
            {
            study(); s_end(argv); return;
            }
        if (muste_strnicmp(word[1],"PROB",4)==0) // 22.4.2001
            {
            n_step_probabilities(); s_end(argv); return;
            }
        n_xor(5,maski,t_teach,sbuf); // TEACH
        if (muste_strnicmp(word[1],sbuf,5)==0) // 1.11.2002
            {
            markov_teach(); s_end(argv); return;
            }

        results_line=0;
        if (g>3 && muste_strcmpi(word[3],"BY")!=0)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return;
            }

        degree=1;
        i=spfind("DEGREE");
        if (i<0) i=spfind("ORDER");
        if (i>=0) degree=atoi(spb[i]);
        if (degree<0) degree=0;
        i=spfind("DELETE"); if (i>=0) strcpy(delete,spb[i]);
        l1=edline2(word[1],1,1); if (l1==0) return;
        l2=edline2(word[2],l1,1); if (l2==0) return;
        strcpy(line_end," ");
        i=spfind("LINE_END"); if (i>=0) strcpy(line_end,spb[i]);
        i=varaa_tilat(); if (i<0) return;

    //  MARKOV L1,L2 BY <matrix>
        if (g>4 && muste_strcmpi(word[3],"BY")==0)
            {
            simul_matrix(word[4]); s_end(argv); return;
            }

        i=spfind("MATRIX");
        if (i>=0)
            {
            comp_matrix(spb[i]);
            s_end(argv);
            return;
            }
        markov();
        s_end(argv);
        }

static int varaa_tilat()
        {
        int l,i;
        char x[LLENGTH];

        tila=(l2-l1+1)*(ed1-1+strlen(line_end));
        mspace=muste_malloc(tila);
        if (mspace==NULL)
            {
            sur_print("\nNot enough memory!");
            WAIT; return(-1);
            }
        *mspace=EOS;
        for (l=l1; l<=l2; ++l)
            {
            edread(x,l);
            i=ed1; while (x[i-1]==' ' && i>0) x[--i]=EOS;
            strcat(mspace,x+1);
            strcat(mspace,line_end);
            }
        return(1);
        }

static int markov()
        {
        int i,k,h;
        char x[LLENGTH];
        char *p;
//      char last_char;
        char state[10];
//      char m;
        extern char *haku();
        int last_line=0; // 14.8.2010

        output_open(eout);
        mstart=mspace;
        mend=mspace+strlen(mspace);
        strncpy(state,mstart,degree); state[degree]=EOS;
        strncpy(x,mstart,degree);
        i=degree;

        if (g>4)
            {
            last_line=edline2(word[4],results_line,0);
             // if (last_line==0) return;
            }
        p=mstart+1; prev=mstart;
        while(1)
            {
            if (i>c3)
                {
                x[i]=EOS;
                k=i; while (k>0 && x[k-1]!=' ') --k;
                if (k==0) { print_line(x); i=0; }
                else
                    {
                    x[k-1]=EOS; print_line(x);
                    for (h=k; h<i; ++h) x[h-k]=x[h];
                    i=i-k;
                    }
                if (results_line>last_line) return(1); // 14.8.2010
                }
/*    printf("\nstate=%s",state); getch();      */
            p=haku(state,p,degree);
/*   printf("\nmerkki=%c p-mstart=%d",*p,p-mstart); getch();    */
            if (degree>0)
                {
                for (k=0; k<degree-1; ++k) state[k]=state[k+1];
                x[i++]=state[degree-1]=*p;
                }
            else x[i++]=*p;
            prev=p;
            p+=1+(int)(100*uniform_dev());
            if (p>=mend) p=mstart+(int)(tila/5*uniform_dev());
            if (sur_kbhit()) { sur_getch(); break; }
            }
        if (i>0) { x[i]=EOS; print_line(x); }
        output_close(eout);
        return(1);
        }

static char *haku(char *state,char *p,int degree)
        {
        char *q;
/*
printf("\nd=%d p-mstart=%d state=%s",degree,(int)(p-mstart),state);
printf("\np=%.*s",60,p);
getch();
*/
        if (degree==0)
            {
            q=p+1; if (q>=mend) q=mstart;
            return(q);
            }
        q=strstr(p,state);
        if (q==NULL)
            {
            q=strstr(mstart,state);
            if (q==NULL || q+degree-1==prev) return(haku(state+1,p+1,degree-1));
            }
        q+=degree;
        if (q>=mend) q=mstart;
        return(q);
        }


static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }


static unsigned char state[100];
static int tila_nro[256];
static int ntila[10];
static int paikka;

static int comp_matrix(char *mat)
    {
    int i,h,k;
    int n; // # of states
    int m; // n^degree
    int rivi;
    double a,b;
    char state[LNAME];

// printf("\nMatrix %s!",mat);

// printf("\nmspace=%s|",mspace); WAIT;

    i=spfind("STATES");
    if (i<0)
        {
        sur_print("\nList of states STATES=_abc.. missing!");
        WAIT; return(-1);
        }
    n=strlen(spb[i]);
    if (n<101) strcpy(state,spb[i]);
    else
        {
        sur_print("\nMax.number of states is 100.");
        WAIT; return(-1);
        }

    for (i=0; i<256; ++i) tila_nro[i]=-1;
    for (i=0; i<n; ++i) tila_nro[(int)state[i]]=i;

    m=1; for (i=0; i<degree; ++i) m*=n;

    aa=(double *)muste_malloc(m*n*sizeof(double));
    if (aa==NULL) { ei_tilaa(); return(-1); }
    rlab=muste_malloc(m*8+1);
    if (rlab==NULL) { ei_tilaa(); return(-1); }
    clab=muste_malloc(n*8+1);
    if (clab==NULL) { ei_tilaa(); return(-1); }

    for (i=0; i<m*n; ++i) aa[i]=0.0;

    paikka=-1;
    for (i=0; i<degree; ++i)
        {
        k=seuraava_tila();
        ntila[i]=k;
        }
    while(1)
        {
        k=seuraava_tila();
        if (k<0) break;
        rivi=ntila[0];
        for (i=1; i<degree; ++i) rivi=n*rivi+ntila[i];
        ++aa[rivi+m*k];
        for (i=0; i<degree-1; ++i) ntila[i]=ntila[i+1];
        ntila[degree-1]=k;
        }

    b=1.0/(double)n;
    for (i=0; i<m; ++i)
        {
        a=0.0;
        for (k=0; k<n; ++k) a+=aa[i+m*k];
//      if (a==0.0) { aa[i]=1.0; continue; }
        if (a==0.0) { for (k=0; k<n; ++k) aa[i+m*k]=b; continue; }
        for (k=0; k<n; ++k) aa[i+m*k]/=a;
        }

// mprint(aa,m,n);

    for (i=degree; i<8; ++i) sbuf[i]=' '; sbuf[8]=EOS;
    for (i=0; i<degree; ++i) ntila[i]=0;
    h=0;
    while (1)
        {
        for (i=0; i<degree; ++i) sbuf[i]=state[ntila[i]];
        for (i=0; i<8; ++i) rlab[h++]=sbuf[i];
        i=next_lattice_point(degree,ntila,n-1);
        if (i<0) break;
        }
    for (i=1; i<8; ++i) sbuf[i]=' '; sbuf[8]=EOS;
    h=0;
    for (i=0; i<n; ++i)
        {
        sbuf[0]=state[i];
        for (k=0; k<8; ++k) clab[h++]=sbuf[k];
        }

    strcpy(sbuf,mat);
    if (strchr(mat,':')==NULL) { strcpy(sbuf,edisk); strcat(sbuf,mat); }
    if (strchr(mat,'.')==NULL) strcat(sbuf,".MAT");
    matrix_save(sbuf,aa,m,n,rlab,clab,8,8,0,"Estimated_transition_probabilities",0,0);


    return(1);
    }

static int seuraava_tila()
    {
    int i;
    unsigned char ch;

    while(1)
        {
        ++paikka;
        ch=mspace[paikka];
        if (ch==' ') ch='_';
        if (ch==EOS) return(-1);
        i=tila_nro[(int)ch];
        if (i>=0) break;
        }
    return(i);
    }

static int next_lattice_point(int n,int *elem1,int vmax)
        {
        int i;

        i=n-1;
        while (elem1[i]==vmax && i>=0) --i;
        if (i<0) return(-1);
        ++elem1[i];
        ++i; for (; i<n; ++i) elem1[i]=0;
        return(1);
        }


static int start_state;
static int start_states[10];

static int simul_matrix(char *mat)
        {
        int i,j,h,k;
        double a;
        char x[LLENGTH];
        int n;
        int pos;
        char sp_char;
        extern double uniform_dev();

        i=matrix_load(mat,&aa,&rdim,&cdim,&rlab,&clab,&lr,&lc,&mtype,expr);

// mprint(aa,rdim,cdim);
        n=rdim;

        degree=1; k=cdim;
        while (1)
            {
            if (rdim==k) break;
            if (rdim<k) { degree=0; break; }
            ++degree; k*=cdim;
            }

        if (!degree)
            {
            sur_print("\nError in the matrix of transition probabilities!");
            sprintf(sbuf,"\n# of columns is %d.",cdim); sur_print(sbuf);
            sprintf(sbuf,"\n# of rows must be the same %d",cdim); sur_print(sbuf);
            sprintf(sbuf,"\nor of the form %d^k where k=2,3,..,8 is the order of the chain.",cdim);
            sur_print(sbuf);
            WAIT; return(1);
            }

        sp_char=EOS;
        i=spfind("SPACE");
        if (i>=0) sp_char=*spb[i];

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

        start_state=0;
        if (degree>1) for (k=0; k<degree; ++k) start_states[k]=0; // 24.4.2002

        i=spfind("START");
        if (i>=0) start_state=atoi(spb[i])-1;

        if (degree>1 && i>=0)
            {
            muste_itoa((int)start_state,sbuf,cdim);
            for (i=0; i<cdim; ++i) x[i]='0'; x[cdim]=EOS;
            strcpy(x+degree-strlen(sbuf),sbuf);
// printf("\nx=%s|",x); getch();
            for (k=0; k<degree; ++k) start_states[k]=(int)(x[k]-'0');
            }

        sur_print("\nGenerating Markov chain... ");
        j=l1; pos=0;
        while (1)
            {
   //       if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }
   //       if (prind) { sprintf(sbuf," %ld",j); sur_print(sbuf); }


            a=uniform_dev();
            if (degree==1)
                {
                for (k=0; k<rdim; ++k)
                    if (a<aa[start_state+rdim*k]) break;
                start_state=k;
                }
            else
                {
                start_state=start_states[0];
                for (k=1; k<degree; ++k)
                    start_state=cdim*start_state+start_states[k];
                for (k=0; k<cdim; ++k)
                    if (a<aa[start_state+rdim*k]) break;
                for (h=0; h<degree-1; ++h)
                    start_states[h]=start_states[h+1];
                start_states[degree-1]=k;
                }
// sprintf(sbuf,"%d|",k); sur_print(sbuf);
            sprintf(sbuf,"%c",*(clab+lc*k));
            sur_print(sbuf);
            if (*sbuf==sp_char) *sbuf=' ';
            x[pos++]=*sbuf;
            if (pos>c3-2)
                {
                x[pos]=EOS;
                if (sp_char)
                    {
                    if (x[pos-1]==' ') { edwrite(x,j,1); pos=0; }
                    else
                        {
                        --pos;
                        while (x[pos]!=' ') --pos;
                        x[pos]=EOS;
                        edwrite(x,j,1);
                        strcpy(x,x+pos+1); pos=strlen(x);
                        }
                    }
                else { edwrite(x,j,1); pos=0; }
                ++j;
                if (j>l2) break;
                }
            } // j

        return(1);
        }

static int study()
        {
        int i,j,k,h,is,e;
        double a,b;
  //    char x[LLENGTH];
        extern char *lyh();

        if (g<3)
            {
            init_remarks();
rem_pr("MARKOV STUDY P,L");
rem_pr("studies the matrix of transition probabilities P of");
rem_pr("a Markov chain and writes the results from line L onwards.");
            wait_remarks(2);
            return(1);
            }
        strcpy(matname,word[2]);
        i=matrix_load(matname,&aa,&rdim,&cdim,&rlab,&clab,&lr,&lc,&mtype,expr);
        if (i<0) return(-1);
        if (cdim!=rdim)
            {
            p_error();
            return(-1);
            }
        m=cdim; m2=m*m;
        for (i=0; i<m; ++i)
            {
            a=0.0;
            for (j=0; j<m; ++j)
                {
                b=aa[i+m*j]; if (b<0.0) { p_error(); return(-1); }
                a+=b;
                }
            if (fabs(a-1.0)>1e-5) { p_error(); return(-1); }
            }

        results_line=0;
        if (g>3)
            {
            results_line=edline2(word[3],1,1);
            if (results_line==0) return(-1);
            }

        ip1=(int *)muste_malloc(m2*sizeof(int));
        if (ip1==NULL) { ei_tilaa(); return(-1); }
        ip2=(int *)muste_malloc(m2*sizeof(int));
        if (ip2==NULL) { ei_tilaa(); return(-1); }

        i=spfind("SVD");
        if (i>=0 && atoi(spb[i])>0)
            {
            k=svd_study(); if (k<0) return(-1);
            study_results(k,1);
            return(1);
            }

        ip3=(int *)muste_malloc(m2*sizeof(int));
        if (ip3==NULL) { ei_tilaa(); return(-1); }
        ip4=(int *)muste_malloc(m2*sizeof(int));
        if (ip4==NULL) { ei_tilaa(); return(-1); }

        for (i=0; i<m2; ++i) if (aa[i]==0.0) ip1[i]=0; else ip1[i]=1;

        i=spfind("POWER");
        if (i<0)
            {   /* Transitive closure, Reingold & al. p.338- */
            for (i=0; i<m2; ++i) if (aa[i]==0.0) ip4[i]=0; else ip4[i]=1;
            for (k=0; k<m; ++k) for(i=0; i<m; ++i)
                {
                if (ip4[i+m*k])
                    {
                    for (j=0; j<m; ++j)
                        if (ip4[i+m*j] || ip4[k+m*j]) ip4[i+m*j]=1;
                        else ip4[i+m*j]=0;
                    }
                }
            }

else { /* POWER */
        for (i=0; i<m2; ++i) if (aa[i]==0.0) ip1[i]=0; else ip1[i]=1;
        for (i=0; i<m; ++i)
            {
            for (j=0; j<m; ++j) ip4[i+m*j]=ip1[i+m*j];
            ip4[(m+1)*i]=1; /* P+I */
            }
        for (k=0; k<m-2; ++k)
            {
            sprintf(sbuf," %d",k+1); sur_print(sbuf);
                for (i=0; i<m; ++i)
                    for (j=0; j<m; ++j)
                        {
                        is=0;
                        for (h=0; h<m; ++h)
                            {
                            if (ip1[i+m*h] && ip4[h+m*j])
                                { is=1; break; }
                            }
                        ip3[i+m*j]=is;
                        }
                for (i=0; i<m2; ++i) ip4[i]=ip3[i]; /* (P+I)P */
                for (i=0; i<m; ++i) ip4[(m+1)*i]=1; /* (P+I)P+I */
            }
        for (i=0; i<m2; ++i) if (ip4[i]>0) ip4[i]=1;
     }

/*
sur_print("\n");
for (i=0; i<m; ++i)
    {
    sur_print("\n");
    for (j=0; j<m; ++j) { sprintf(sbuf,"%d ",ip4[i+m*j]); sur_print(sbuf); }
    }
sur_print("\n");
getch();
*/

        k=0; /* current class */
        for (i=0; i<m; ++i) ip1[i]=0; /* class indices */
        for (i=0; i<m; ++i)
            {
            if (ip1[i]!=0) continue;
            h=0;
            for (j=i+1; j<m; ++j)
                {
                for (e=0; e<m; ++e)
                    if (ip4[i+m*e]!=ip4[j+m*e]) break;
                if (e==m)
                    { ip3[h]=j; ++h; }
                }
            for (j=0; j<h; ++j)
                if (ip4[i+m*ip3[j]]==0) break;
            if (j==h)
                {
                e=0; for (j=0; j<m; ++j) if (ip4[i+m*j]>0) ++e;
                if (e==h+1)
                    {
                    ++k;
                    ip1[i]=k;
                    for (j=0; j<h; ++j) ip1[ip3[j]]=k;
                    ip2[k]=h+1;
                    }
                else
                    {
                    ip1[i]=-1;
                    }
                }
            }
        h=0;
        for (i=0; i<m; ++i) if (ip1[i]==-1) { ip1[i]=0; ++h; }
        ip2[0]=h;
/*
printf("\n");
for (i=0; i<m; ++i) printf("%d ",ip1[i]);
printf("\n"); getch();
*/

        study_results(k,0);
        return(1);
        }

static int svd_study()
        {
        int i,ii,j,h,k,kk,n1;
        double eps,tol;


        for (i=0; i<m; ++i) aa[(m+1)*i]-=1.0;

        dd=(double *)muste_malloc(m*sizeof(double));
        if (dd==NULL) { ei_tilaa(); return(-1); }
        vv=(double *)muste_malloc(m*m*sizeof(double));
        if (vv==NULL) { ei_tilaa(); return(-1); }
        eps=1e-16; tol=(1e-300)/eps;
        mat_svd(aa,dd,vv,m,m,eps,tol);
/*
mprint(aa,m,m);
mprint(dd,m,1);
mprint(vv,m,m);
*/
        for (i=0; i<m; ++i) ip1[i]=-1; /* class indices */
        for (i=m-1; i>=0; --i) if (dd[i]>tol_svd) break;
        n1=m-i-2;
        /* n1+1= # of 'zero' singular values = # of recurrent classes */

        h=0;
        for (i=0; i<m; ++i)
            {
            tol=0.0;
            for (j=m-n1-1; j<m; ++j)
                { eps=aa[i+m*j]; tol+=eps*eps; }
            if (tol<tol_svd*tol_svd*(double)(n1+1))
                {
                ++h;
                ip1[i]=0; /* transient */
                }
            }
        ip2[0]=h;
        k=0; /* current class */
        for (i=0; i<m; ++i)
            {
            if (ip1[i]!=-1) continue;
            ++k; h=i; ip1[i]=k; ip2[k]=1;
            for (ii=h+1; ii<m; ++ii)
                {
                if (ip1[ii]!=-1) continue;
                tol=0.0;
                for (j=m-n1-1; j<m; ++j)
                    {
                    eps=vv[ii+m*j]-vv[h+m*j];
                    tol+=eps*eps;
                    }
                if (tol<tol_svd*tol_svd*(double)(n1+1))
                    { ip1[ii]=k; ++ip2[k]; }
                }
            }

        if (k!=n1+1)
            {
            sur_print("\nSVD method failed!!!");
            WAIT; return(-1);
            }

        /* steady state probabilities */
        for (i=0; i<m; ++i) { vv[i]=ip1[i]; vv[i+m]=0.0; }
        for (kk=1; kk<=k; ++kk)
            {
            for (i=0; i<m; ++i)
                {
                if (ip1[i]!=kk) continue;
                tol=-1.0;
                for (j=m-n1-1; j<m; ++j)
                    {
                    eps=fabs(aa[i+m*j]);
                    if (eps>tol) { h=j; tol=eps; }
                    }
                tol=0.0;
                for (j=i; j<m; ++j)
                    {
                    if (ip1[j]!=kk) continue;
                    tol+=aa[j+m*h];
                    }
                for (j=i; j<m; ++j)
                    {
                    if (ip1[j]!=kk) continue;
                    vv[j+m]=aa[j+m*h]/tol;
                    ii=j;
                    }
                i=ii;
                }
            } /* kk */

        return(k);
        }
/***********************
mprint(aa,m,n)
double *aa;
int m,n;
        {
        int i,j;

        for (i=0; i<m; ++i)
            {
            printf("\n"); for (j=0; j<n; ++j) printf("%g ",aa[i+m*j]);
            }
        printf("\n"); getch(); return(1);
        }
***********************/

static int study_results(int k,int svd)
        {
        int i,j,e;
        char x[LLENGTH];

        i=output_open(eout); if (i<0) return(-1);
        sprintf(sbuf,"Structure of Markov chain %s of %d states:",
                        word[2],m);
        print_line(sbuf);
        if (ip2[1]==m)
            {
            print_line("The chain is irreducible.");
            output_close(eout);
            if (svd)
                i=matrix_save("MCLASS.M",vv,rdim,2,rlab,"Class   Prob    ",lr,8,-1,expr,0,0);
            return(1);
            }
        else
            print_line("Class structure saved in matrix file MCLASS.M");
        if (k==1)
            sprintf(sbuf,"1 recurrent class of states:");
        else
            sprintf(sbuf,"%d recurrent classes of states:",k);
        print_line(sbuf);

        for (i=1; i<=k; ++i)
            {
            j=sprintf(sbuf,"%d (%d):",i,ip2[i]);
            for (e=0; e<m; ++e)
                if (ip1[e]==i)
                    {
                    *x=EOS; strncat(x,rlab+lr*e,lr);
                    j+=sprintf(sbuf+j," %s",lyh(x));
                    if (j>c2-8) { sprintf(sbuf+j,"..."); break; }
                    }
            print_line(sbuf);
            }
        if (ip2[0]==0) print_line("No transient states");
        else
            {
            if (ip2[0]==1)
                sprintf(sbuf,"1 transient state:");
            else
                sprintf(sbuf,"%d transient states:",ip2[0]);
            print_line(sbuf);

            j=0;
            for (e=0; e<m; ++e)
                if (ip1[e]==0)
                    {
                    *x=EOS; strncat(x,rlab+lr*e,lr);
                    j+=sprintf(sbuf+j," %s",lyh(x));
                    if (j>c2-8) { sprintf(sbuf+j,"..."); break; }
                    }
            print_line(sbuf);
            }
        output_close(eout);

        for (i=0; i<m; ++i)
            {
            aa[i]=ip1[i];
            }

        sprintf(expr,"Class_structure_of_%s_(Transient_states=0)",
                                 word[2]);
        if (svd)
            i=matrix_save("MCLASS.M",vv,rdim,2,rlab,"Class   Prob    ",lr,8,-1,expr,0,0);
        else
            i=matrix_save("MCLASS.M",aa,rdim,1,rlab,"Class",lr,lr,-1,expr,0,0);

        return(1);
        }

static char *lyh(char *s)
        {
        int i;

        i=strlen(s)-1;
        while (s[i]==' ') s[i--]=EOS;
        return(s);
        }

static int ei_tilaa()
        {
        sur_print("\nNot enough memory!"); WAIT; return(1);
        }

static int p_error()
        {
        sur_print("\nThe matrix of transition probabilities must be square");
        sur_print("\nwith sums of rows = 1");
        WAIT; return(1);
        }




// MARKOV PROB,P,i,j,n,PN
static int n_step_probabilities()
    {
    int i,j,n;
    double a,b;
    int tila1,tila2;
    char s[LNAME];

    if (g<7)
        {
        init_remarks();
rem_pr("MARKOV PROB P,i,j,n,PN");
rem_pr("from a transition probability matrix P of a Markov chain");
rem_pr("computes k-step transition probabilities from state i to j");
rem_pr("for k=1,2,...,n and saves them as a new vector PN.");
        wait_remarks(2);
        return(1);
        }
    strcpy(matname,word[2]);
    i=matrix_load(matname,&aa,&rdim,&cdim,&rlab,&clab,&lr,&lc,&mtype,expr);
    if (i<0) return(-1);
    if (cdim!=rdim)
        {
        p_error();
        return(-1);
        }
    m=cdim; m2=m*m;
    for (i=0; i<m; ++i)
        {
        a=0.0;
        for (j=0; j<m; ++j)
            {
            b=aa[i+m*j]; if (b<0.0) { p_error(); return(-1); }
            a+=b;
            }
        if (fabs(a-1.0)>1e-5) { p_error(); return(-1); }
        }

    tila1=p_label(word[3],m,rlab,lr);
    if (tila1<0) return(1);
    tila2=p_label(word[4],m,rlab,lr);
    if (tila2<0) return(1);

    n=atoi(word[5]);

    qq=(double *)muste_malloc(m2*sizeof(double));
    if (qq==NULL) { ei_tilaa(); return(1); }
    qq2=(double *)muste_malloc(m2*sizeof(double));
    if (qq2==NULL) { ei_tilaa(); return(1); }
    ppn=(double *)muste_malloc(n*sizeof(double));
    if (ppn==NULL) { ei_tilaa(); return(1); }
    plab=muste_malloc(8*n);
    if (plab==NULL) { ei_tilaa(); return(1); }

    for (i=0; i<m2; ++i) qq[i]=aa[i];
    ppn[0]=aa[tila1+m*tila2];

    for (i=1; i<n; ++i)
        {
        mat_mlt(qq2,qq,aa,m,m,m);
        ppn[i]=qq2[tila1+m*tila2];
        for (j=0; j<m*m; ++j) qq[j]=qq2[j];
        }

    strcpy(sbuf,word[6]);
    if (strchr(sbuf,'.')==NULL) strcat(sbuf,".MAT");
    numlab2(plab,n,8,1);
    sprintf(s,"P(to_%s_in_n_steps)",word[4]);

    matrix_save(sbuf,ppn,n,1,plab,rlab+tila2*lr,8,lr,-1,s,0,0);

    return(1);
    }

static int p_label(char *tila,int m,char *lab,int k)
    {
    int i;
    char x[LNAME];

    strcpy(x,tila);
    if (is_integer(x))
        {
        i=atoi(x);
        if (i<=m) return(i-1);
        }
    for (i=strlen(x); i<lr; ++i) strcat(x," ");
    for (i=0; i<m; ++i)
        if (strncmp(x,lab+i*k,k)==0) return(i);

    sprintf(sbuf,"\nState %s not found!",tila);
    sur_print(sbuf); WAIT; return(-1);
    }

static int is_integer(char *x)
    {
    int i;

    for (i=0; i<strlen(x); ++i)
        if (strchr("0123456789 ",x[i])==NULL) return(0);
    return(1);
    }

static int numlab2(char *lab,int n,int len,int base)
        {
        unsigned int h,i,j,k;
        char sana[6];
        int sar;

        --base;
        for (i=0; i<n*len; ++i) lab[i]=' ';
        if (n+base<1000) sar=3;
        else if (n+base<100000) sar=5;
        else sar=8;

        for (i=0; i<n; ++i)
            {
            muste_itoa(i+1+base,sana,10);
            h=strlen(sana);
            for (j=i*len+sar-h, k=0; k<h; ++k, ++j) lab[j]=sana[k];
            }
        return(1);
        }

// MARKOV TEACH <word>  / STATES=<string_of_letters>

static char kohdesana[LNAME];
static double m_coeff=0.01;
static int ns;
static int suggest;
static char suggestion[LLENGTH];
static char *sug[50];
static double sug_value[50];

static int stop_when_found=0;
static char edellinen_sana[LNAME];

/********************************
XOR CUR+1,CUR+2,CUR+3
0123456
SUGGEST
cdutqfb
**********************************/
static char t_suggest[]="cdutqfb";
/********************************
XOR CUR+1,CUR+2,CUR+3
0123
STOP
ce}c
**********************************/
static char t_stop[]="ce}c";
/********************************
XOR CUR+1,CUR+2,CUR+3
0123
SHOW
cy}d
**********************************/
static char t_show[]="cy}d";

static int jitter,jitter_seed;
static char *jj[2];

static int markov_teach()
    {
    int i,j,n;
    double a;
    char sana[LLENGTH];
    char vast[8];
    double muutos,tn;
    int row,col;
    int rivi;
    int nn;
    int show=1;
    int nnmax;
    char xx[LLENGTH];
    char state[LNAME];

    if (g<3) return(-1);
/***********************
    if (g<3)
        {
        sur_print("\nUsage: MARKOV TEACH <word> / STATES=<string_of_letters>");
        WAIT;
        return(-1);
        }
***********************/
    jitter=0;
    i=spfind("JITTER");
    if (i>=0)
        {
        strcpy(sana,spb[i]); split(sana,jj,2);
        jitter_seed=atoi(jj[0]); srand(jitter_seed);
        jitter=atoi(jj[1]);
        }

    i=spfind("STATES");
    if (i<0)
        {
        sur_print("\nSpecification STATES=<string_of_letters> missing!");
        WAIT;
        return(-1);
        }
    strcpy(state,spb[i]); strcat(state," ");
    m=strlen(state);

    stop_when_found=0;
    n_xor(4,maski,t_stop,sbuf);
    i=spfind(sbuf); // STOP
    if (i>=0) stop_when_found=atoi(spb[i]);

    show=1;
    n_xor(4,maski,t_show,sbuf);
    i=spfind(sbuf); // SHOW
    if (i>=0) show=atoi(spb[i]);

    suggest=0;
    n_xor(7,maski,t_suggest,sbuf);
    i=spfind(sbuf); // SUGGEST
    if (i>=0)
        {
        strcpy(suggestion,spb[i]);
        suggest=split(suggestion,sug,50);
        for (i=0; i<suggest; ++i) sug_value[i]=m_coeff*atof(sug[i]);
        }
    strcpy(kohdesana,word[2]);
    ns=strlen(kohdesana);

    nnmax=1000000000L;
    i=spfind("NMAX");
    if (i>=0) nnmax=atol(spb[i]);

    sur_cursor_position(&row,&col);
    rivi=r1+r-1;

    write_string(space,c3+8,'\237',r3+3,1);

    aa=(double *)muste_malloc(m*m*sizeof(double));
    ps=(double *)muste_malloc(m*sizeof(double));

    if (suggest)
        {
        aa2=(double *)muste_malloc(m*m*sizeof(double));
        ps2=(double *)muste_malloc(m*sizeof(double));
        }
    for (i=0; i<m-1; ++i) ps[i]=1.0/(double)(m-1); ps[m-1]=0.0;
    a=0.25;
    for (i=0; i<m; ++i) aa[i+m*(m-1)]=a;
    a=(1.0-a)*1.0/(double)(m-1);
    for (i=0; i<m-1; ++i) for (j=0; j<m-1; ++j)
        aa[i+m*j]=a;
    for (j=0; j<m-1; ++j) aa[m-1+m*j]=0.0; aa[m*m-1]=1.0;

// mprint(ps,m,1);
// mprint(aa,m,m);
    nn=0; *edellinen_sana=EOS;

    while (1)
        {
        PR_EBLD;
        if (show)
            {
            sur_locate(r3+2,1);
            strcpy(sbuf,space); sbuf[c3+7]=EOS;
            sur_print(sbuf);
            }
        laske_tn(kohdesana,&tn);
        if (show)
            {
            sur_locate(r3+2,c3-5);
            sprintf(sbuf,"%e",tn); sur_print(sbuf);
            }
        n=arvonta(sana); if (n<0) return(-1);
        if (show)
            {
            sprintf(sbuf," %s",sana);
            sur_locate(r3+2,8);
            sur_print(sbuf);
            }
        if (suggest) make_suggestion(vast,sana);
        else *vast=EOS;
        if (show)
            {
            while (1)
                {
                sur_locate(r3+2,9+n);
                prompt("? ",vast,6);
                if (*vast!=EOS) break;
                }
            PR_EINV;
            if (*vast!='.')
                {
                muutos=m_coeff*atof(vast);
                muunna(muutos,sana);
                }
            }
        else
            {
            muutos=m_coeff*atof(vast);
            muunna(muutos,sana);
            }
        ++nn;
        if (show)
            {
            sur_locate(row,col);
            i=sprintf(sbuf,"%d %s %s",nn,sana,vast);
            if (tn<1e-6) sprintf(xx,"%e",tn);
            else sprintf(xx,"%g",tn);
            sprintf(sbuf+i,"%*.*s %s",57-i,57-i,space,xx);

            sur_print("\n");
            sur_print(sbuf);
            sur_cursor_position(&row,&col);
            ++rivi;
            edwrite(space,rivi,1);
            edwrite(sbuf,rivi,1);
            }
        else
            {
            sur_locate(r3+2,1); if (nn>nnmax) break;
            sprintf(sbuf,"%d %e",nn,tn);
            sur_print(sbuf);
            }
        if (stop_when_found && strcmp(kohdesana,sana)==0) break;
        if (show)
            {
            if (*vast=='.') break;
            if (*vast=='p') mprint2(aa,m,m);
            if (*vast=='s') mprint2(ps,m,1);
            }
        }
        if (!show)
            {
            ++rivi;
            if (nn>nnmax)
             sprintf(sbuf,"Trials %d No success!. Probability=%e",
                                 nn,tn);
            else
             sprintf(sbuf,"Success in trial %d. Probability=%e",
                                 nn,tn);
            edwrite(space,rivi,1);
            edwrite(sbuf,rivi,1);
            }
    return(1);
    }

static int laske_tn(char *s,double *ptn)
    {
    double tn;
    int i,j,k,n;

    i=state_nr(*s);
    tn=ps[i];
    n=strlen(s);
    for (k=1; k<=n; ++k)
        {
        if (k<n) j=state_nr(s[k]);
        else j=m-1;
        tn*=aa[i+m*j];
        i=j;
        }
    *ptn=tn;
    return(1);
    }

static int arvonta(char *sana)
    {
    int i,j,n;
    double a,sum;
    int nn;

 while (1)
  {
    a=uniform_dev();
// printf("\na=%g",a);
    sum=0.0;
    for (i=0; i<m; ++i)
        {
        sum+=ps[i];
        if (a<=sum) break;
        }
    if (i==m) { sur_print("\nProb. error!"); WAIT; return(-1); }
    *sana=state[i]; n=1;

    nn=0;
    while (1)
        {
        a=uniform_dev();
// printf("\na=%g",a);
        sum=0.0;
        for (j=0; j<m; ++j)
            {
            sum+=aa[i+m*j];
            if (a<=sum) break;
            }
        if (j==m) { sur_print("\nProb. error!"); WAIT; return(-1); }
        if (j==m-1) break;
        sana[n++]=state[j];
//      if (n>=ns) break;
        i=j;
        }

    sana[n]=EOS;
// printf("\n sana=%s|\nedell=%s|",sana,edellinen_sana); getch();
    if ( strcmp(sana,edellinen_sana)!=0 ||
         strcmp(sana,kohdesana)==0 ) break;
  }
    strcpy(edellinen_sana,sana);
    return(n);
    }

static int muunna(double muunnos,char *sana)
    {
    double a,b;
    int i,n,j,k,h;

    n=strlen(sana);
    i=state_nr(*sana);
    a=1.0-ps[i];
    ps[i]=exp(muunnos+log(ps[i]));
    a+=ps[i]; a=1.0/a;
    for (j=0; j<m-1; ++j) ps[j]*=a;
// mprint2(ps,1,m);
// mprint2(aa,m,m);
    for (k=1; k<=n; ++k)
        {
        if (k<n) j=state_nr(sana[k]);
        else j=m-1;
        b=aa[i+m*j];
        a=1.0-b;
        b=exp(muunnos+log(b));
        a+=b; aa[i+m*j]=b; a=1.0/a;
        for (h=0; h<m; ++h) aa[i+m*h]*=a;
        i=j;

        }
// mprint2(aa,m,m);
    return(1);
    }

static int state_nr(unsigned char ch)
    {
    int i;

    for (i=0; i<m-1; ++i)
       if (ch==state[i]) break;
    return(i);
    }

static int mprint2(double *aa,int m,int n)
        {
        int i,j;
        double sum;
        int row,col;

        sur_cursor_position(&row,&col);
        sur_locate(2,1);
        strcpy(sbuf,"\n       "); j=6;
        for (i=0;i<m; ++i) j+=sprintf(sbuf+j,"%c       ",state[i]);
        sur_print(sbuf);
        for (i=0; i<m; ++i)
            {
            sum=0.0;
            sprintf(sbuf,"\n%c ",state[i]); sur_print(sbuf);
            for (j=0; j<n; ++j)
                {
                sum+=aa[i+m*j];
                sprintf(sbuf,"%7.5f ",aa[i+m*j]); sur_print(sbuf);
                }
            sprintf(sbuf,"*%g  |",sum); sur_print(sbuf);
            }
        sur_locate(row,col);
        return(1);
        }

static int make_suggestion(char *vast,char *sana)
    {
    int i,j,k,imax;
    double max,tn;

    for (i=0; i<m; ++i)
        {
        ps2[i]=ps[i];
        for (j=0; j<m; ++j) aa2[i+m*j]=aa[i+m*j];
        }

    max=-1.0; imax=0;
    for (k=0; k<suggest; ++k)
        {

        muunna(sug_value[k],sana);

        laske_tn(kohdesana,&tn);
        if (tn>max) { max=tn; imax=k; }

        for (i=0; i<m; ++i)
            {
            ps[i]=ps2[i];
            for (j=0; j<m; ++j) aa[i+m*j]=aa2[i+m*j];
            }
        }

    if (jitter)
        {
        i=((double)rand()/16384.0-0.5)*(double)(jitter)+0.5;
        i+=atoi(sug[imax]);
        if (i<-30) i=-30; if (i>30) i=30;
        sprintf(vast,"%d",i);
// printf("\nvast=%s sug=%s|",vast,sug[imax]); getch();
        return(1);
        }
    strcpy(vast,sug[imax]);

    return(1);
    }

static int n_xor(int n,char *x1,char *x2,char *p)
    {
    int j;

    for (j=0; j<n; ++j)
        {
        p[j]=x1[j]^x2[j];
        }
    p[n]=EOS;
    return(1);
    }

