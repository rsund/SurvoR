#include "muste.h"
/* !varstat.c 14.3.1993/SM (16.3.1993) (22.4.1997)
*/

#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;
static int m;
static double *x,*x2;
#define MAXTASK 30
static int outvar[MAXTASK];
static int task[MAXTASK];
static double tp1[MAXTASK];
static double tp2[MAXTASK];
static int ntask;
static int npar[MAXTASK];
static int laji;  /* word[2]=* : laji=2 */

static int nmiss,m1;
static double pot,sum;
static double b;
static int sort;
static int collective;
static char aritvar[LLENGTH];
static int prind;
static int alku; /* =1 ennen 1. havaintoa; kukin oper. voi asettaa alku=0 */

// SM ADD 22.5.2012
static int strval=0; // =1 for #VAL,STR_VAL_LIST
static char str_space[4*LLENGTH];
static char *str_x[1000];
static char str_tp1[32];
static double val_list[100];
static int n_list;
static char *str_val_list[100];
static char str_var_list_space[4*LLENGTH];
static int str_val_type=0;

static int muste_exit=FALSE; // RS ADD

static int tasks();
static int create_val_list();
static int create_str_val_list(char type);
static int muuttuja(char *s);
static int taskfind(char *s);
static double xsum(int it);
// static double xsum1(int it);
static double xsum2(int it);
static double xsum3(int it);
static double xsum4(int it);
static double xmean(int it);
static double xpos_side(int it);
static double xfirst(int it);
static double xlast(int it);
static double xcyclen(int it);
static double xmodeval(int it);
static double xnumseq(int it);
static double xnumrun(int it);
static double xrun_up_down(int it);
static void not_enough_memory();
static int varaa_tilat();
static double xstddev(int it);
static double xmax(int it);
static double xmin(int it);
static double xrange(int it);
static double xmedian(int it);
static int sort_x();
static double xval(int it);
static void xdivsum();
static void xdivmax();
static void xnorm();
static void xcenter();
static void xstandard();
static int xarit(int op,int l);
static int cumulative_sum();


/********************
char **specs;
*********************/
static char *taskname[]={ "SUM", "SUM1", "SUM2", "SUM3", "SUM4",
                   "MEAN", "STDDEV", "MAX", "MIN", "RANGE", "MEDIAN",
                   "#MISS", "#N", "#VAL", "#POS_SIDE", "FIRST", "LAST",
                   "CYCLEN", "#MODEVAL", "#SEQ", "#RUN", "#RUN_UP_DOWN",
                   "DIVSUM", "DIVMAX", "NORM", "CENTER", "STANDARD",
                   "MLT", "DIV", "ADD", "SUB", "SORT", "CUM", "" };

#define XSUM 0
#define XSUM1 1
#define XSUM2 2
#define XSUM3 3
#define XSUM4 4
#define XMEAN 5
#define XSTDDEV 6
#define XMAX 7
#define XMIN 8
#define XRANGE 9
#define XMEDIAN 10
#define XMISS 11
#define XN 12
#define XVAL 13
#define XPOS_SIDE 14
#define XFIRST 15
#define XLAST  16
#define XCYCLEN 17
#define XMODEVAL 18
#define XSEQ 19
#define XRUN 20
#define XRUN_UP_DOWN 21
#define XDIVSUM 22
#define XDIVMAX 23
#define XNORM 24
#define XCENTER 25
#define XSTANDARD 26
#define XMLT 27
#define XDIV 28
#define XADD 29
#define XSUB 30
#define XSORT 31
#define XCUM 32

/*************************
void main(argc,argv)
int argc; char *argv[];
*************************/

void muste_varstat(char *argv)
        {
        int i,vi,it;
        int l;

// RS ADD 13.7.2012 variable init        
		strval=0;        
		m=0;
		x=NULL;
		x2=NULL;
		ntask=0;
		laji=0;	
		nmiss=m1=0;
		pot=sum=0;
		b=0;
		sort=0;
		collective=0;	
		prind=0;
		alku=1;
		muste_exit=FALSE;
		n_list=0;
		str_val_type=0;		
/*		
static int outvar[MAXTASK];
static int task[MAXTASK];
static double tp1[MAXTASK];
static double tp2[MAXTASK];
static int npar[MAXTASK];
static char aritvar[LLENGTH];
static char str_space[4*LLENGTH];
static char *str_x[1000];
static double val_list[100];
static char *str_val_list[100];
static char str_var_list_space[4*LLENGTH];
*/

 
        
/*************
        if (argc==1)
            {
            Rprintf("This program can be used as a SURVO 98 module only.");
            return;
            }
*****************/
        s_init(argv);

        if (g<2)
            {
            init_remarks();
            rem_pr("VARSTAT <data>,<output_var>,<operation>   / S.Mustonen 14.3.1993");
            rem_pr("More information by VARSTAT?                                    ");
            wait_remarks(2);
            s_end(argv[1]);
            return;
            }
        i=data_open2(word[1],&d,1,0,0); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return; /* spec_init gives also an error message! */
        i=mask(&d); if (i<0) return;
        i=conditions(&d); if (i<0) return;  /* permitted only once */
        i=tasks(); if (i<0) return;
        prind=0;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);
/*
printf("\noutvar=%d task=%s tp1=%g tp2=%g",outvar[0],taskname[task[0]],tp1[0],tp2[0]); getch();
*/
        m=d.m_act;
        i=varaa_tilat(); if (i<0) return;


        sur_print("\n"); alku=1;
        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;

            if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
            if (prind) { sprintf(sbuf," %d",l); sur_print(sbuf); }

            nmiss=0; sum=0.0;
 
            if (strval)  // SM ADD 22.5.2012
                {
                char *p;
                int h;
                double a;

                p=str_space;
                for (i=0; i<m; ++i)
                    {
                    vi=d.v[i];

// Rprintf("\nvar=%d type=%s",vi,d.vartype[vi]); sur_getch();
                    if (d.vartype[vi][0]=='S')
                        data_alpha_load(&d,l,vi,p);
                    else // numeric variable
                        {
                        data_load(&d,l,vi,&a);
                        h=(int)a;
                        sprintf(p,"%d",h);
                        }
                    h=strlen(p)-1; while (p[h]==' ') p[h--]=EOS;
                    str_x[i]=p;
                    p+=strlen(p)+1;
                    }
//      for (i=0; i<m; ++i) Rprintf("%s|",str_x[i]);
//      sur_getch();
                }
            else
                {                       
				for (i=0; i<m; ++i)
					{
					vi=d.v[i];
					data_load(&d,l,vi,&x[i]);
					if (x[i]==MISSING8) { ++nmiss; continue; }
					sum+=x[i];
					}
                }
            m1=m-nmiss;
            if (m1==0) sum=MISSING8; // 13.9.2010
            sort=0;

            if (collective)
                {
                switch (task[0])
                    {
                  case XDIVSUM:
                    xdivsum(); break;
                  case XDIVMAX:
                    xdivmax(); break;
                  case XNORM:
                    xnorm(); break;
                  case XCENTER:
                    xcenter(); break;
                  case XSTANDARD:
                    xstandard(); break;
                  case XMLT:
                    i=xarit(1,l); if(i<0) return; break;
                  case XDIV:
                    i=xarit(2,l); if (i<0) return; break;
                  case XADD:
                    i=xarit(3,l); if (i<0) return; break;
                  case XSUB:
                    i=xarit(4,l); if (i<0) return; break;
                  case XSORT:
                    sort_x(); for (i=0; i<m1; ++i) x[i]=x2[i];
                    for (i=m1; i<m; ++i) x[i]=MISSING8;
                    break;
                  case XCUM:
                    i=cumulative_sum(); if (i<0) return; break;
                    }
                for (i=0; i<m; ++i)
                    {
                    data_save(&d,l,d.v[i],x[i]);
                    }
                continue;
                }

            for (it=0; it<ntask; ++it)
                {
                muste_exit=FALSE; // RS ADD
                switch (task[it])
                    {
                  case XSUM:
                    b=xsum(it); break;
                  case XSUM1:
                    b=sum; break;
                  case XSUM2:
                    b=xsum2(it); break;
                  case XSUM3:
                    b=xsum3(it); break;
                  case XSUM4:
                    b=xsum4(it); break;
                  case XMEAN:
                    b=xmean(it); break;
                  case XSTDDEV:
                    b=xstddev(it); break;
                  case XMAX:
                    b=xmax(it); break;
                  case XMIN:
                    b=xmin(it); break;
                  case XRANGE:
                    b=xrange(it); break;
                  case XMEDIAN:
                    b=xmedian(it); break;
                  case XMISS:
                    b=nmiss; break;
                  case XN:
                    b=m1; break;
                  case XVAL:
                    b=xval(it); break;
                  case XPOS_SIDE:
                    b=xpos_side(it); break;
                  case XFIRST:
                    b=xfirst(it); break;
                  case XLAST:
                    b=xlast(it); break;
                  case XCYCLEN:
                    b=xcyclen(it); break;
                  case XMODEVAL:
                    b=xmodeval(it); break;
                  case XSEQ:
                    b=xnumseq(it); break;
                  case XRUN:
                    b=xnumrun(it); break;
                  case XRUN_UP_DOWN:
                    b=xrun_up_down(it); break;

                    }  /* switch */
                if (muste_exit) {  data_close(&d); s_end(argv); return; } // RS ADD    
                data_save(&d,l,outvar[it],b);
                }      /* it */
            }          /* l */
        data_close(&d);
        s_end(argv);
        }

static int tasks()
        {
        int i,k,ip;
        char x[LLENGTH],*osa[MAXTASK];
        char y[LLENGTH],*yosa[3];

        collective=0;
        if (g<3)
            {
            i=spfind("VARSTAT");
            if (i<0)
                {
                sur_print("\nVARSTAT=<list of target variables> missing!");
                sur_print("\nExample:");
                sur_print("\n VARSTAT=MEAN1:8,DEV1,N0:1 (type of new variable after ':')");
                sur_print("\n MEAN1=MEAN DEV1=STDDEV N0=#VAL,0");
                WAIT; return(-1);
                }
            strcpy(x,spb[i]); ntask=split(x,osa,MAXTASK);
            for (k=0; k<ntask; ++k)
                {
                i=muuttuja(osa[k]); if (i<0) return(-1);
                outvar[k]=i;
                i=spfind(osa[k]);  /* muuttuja() poistanut :<tyyppi> merkinnõn */
                if (i<0)
                    {
                    sprintf(sbuf,"Task of variable %s is not given as %s=<task> !",
                                              osa[k],osa[k]);
                    sur_print(sbuf); WAIT; return(-1);
                    }

                strcpy(y,spb[i]);
                ip=split(y,yosa,3);
                i=taskfind(yosa[0]); if(i<0) return(-1);
                task[k]=i; npar[k]=0;
                if (ip<2) continue;
                tp1[k]=atof(yosa[1]); ++npar[k];
                if (ip<3) continue;
                tp2[k]=atof(yosa[2]); ++npar[k];
                }
            return(1);
            }
        if (strcmp(word[2],"*")==0)
            collective=1;

        laji=1;
        if (g<4)
            {
            if (collective)
                sur_print("\nUsage: VARSTAT <data>,*,<operation>");
            else
                sur_print("\nUsage: VARSTAT <data>,<variable>,<operation>");
            WAIT; return(-1);
            }
        ntask=1;
        if (!collective)
            {
            i=muuttuja(word[2]); if (i<0) return(-1);
            outvar[0]=i;
            }
        else if (g>4) strcpy(aritvar,word[4]);  /* VARSTAT <data>,*,/,<aritvar>  */

        i=taskfind(word[3]); if(i<0) return(-1);
        task[0]=i; npar[0]=0;
        if (g<5) return(1);

// SM ADD 22.5.2012
        strcpy(str_tp1,word[4]); // 22.5.2012  #VAL,VAL_LIST
        if (strcmp(str_tp1,"VAL_LIST")==0)
            {
            i=create_val_list(); // 22.5.2012
            return(i);
            }

        strcpy(str_tp1,word[4]); // 22.5.2012  #VAL,STR_VAL_LIST
        if (strcmp(str_tp1,"STR_VAL_LIST")==0 ||
            strcmp(str_tp1,"SUBSTR_VAL_LIST")==0)
            {
            i=create_str_val_list(str_tp1[1]); // 22.5.2012
            return(i);
            }        
        
        
        
        tp1[0]=atof(word[4]); ++npar[0];
        if (g<6) return(1);
        tp2[0]=atof(word[5]); ++npar[0];
        return(1);
        }

static int create_val_list() // SM ADD 22.5.2012
        {
        int i; // 22.5.2012
        char lst[LLENGTH];
        char *v[100];

        i=spfind("VAL_LIST");
        if (i<0)
                {
      sur_print("\nVAL_LIST=val1,val2,val3,... not given as a specification");
      WAIT; return(-1);
                }
        strcpy(lst,spb[i]);
        n_list=split(lst,v,100);
        for (i=0; i<n_list; ++i) val_list[i]=atof(v[i]);
        return(1);
        }
        
static int create_str_val_list(char type) // SM ADD 22.5.2012
        {
        int i; // 22.5.2012
        char lst[LLENGTH];
        char *v[100];

        if (type=='T') str_val_type=1; else str_val_type=2;

        if (type=='T') i=spfind("STR_VAL_LIST");
        else if (type=='U') i=spfind("SUBSTR_VAL_LIST");
        if (i<0)
                {
      sur_print("\nSTR_VAL_LIST=strval1,strval2,strval3,... or");
      sur_print("\nSUBSTR_VAL_LIST=strval1,strval2,strval3,... not given as a specification");
      WAIT; return(-1);
                }
        strcpy(str_var_list_space,spb[i]);
        n_list=split(str_var_list_space,str_val_list,100);
        strval=1;
        return(1);
        }

static int muuttuja(char *s)
        {
        int i,len;
        char type;
        char *p;

        type='4'; len=4;
        p=strchr(s,':');
        if (p!=NULL)
            {
            *p=EOS;
            type=*(p+1);
            if (type=='S') len=atoi(p+2);
            }
        i=varfind2(&d,s,0);
        if (i<0)
            {
            i=create_newvar(&d,s,type,len);
            if (i<0) return(-1);
            }
        return(i);
        }

static int taskfind(char *s)
        {
        int i;

        i=0;
        while (*taskname[i]!=EOS && muste_strcmpi(taskname[i],s)!=0) ++i;
        if (*taskname[i]==EOS)
            {
            sprintf(sbuf,"\nUnknown task name %s !",s); sur_print(sbuf);
            WAIT; return(-1);
            }
        return(i);
        }

static double xsum(int it)
        {
        int j;
        double a,b;

        if (m1==0) return(MISSING8); // 13.9.2010
        if (npar[it]==0) pot=1.0;
        else pot=tp1[it];
        b=0.0;
        for (j=0; j<m; ++j)
            {
            a=x[j];
            if (a==MISSING8) continue;
            if (pot==2.0) { a=a*a; task[it]=XSUM2; }
            else if (pot==3.0) { a=a*a*a; task[it]=XSUM3; }
            else if (pot==4.0) { a=a*a*a*a; task[it]=XSUM4; }
            else if (pot!=1.0) a=pow(a,pot);
            else task[it]=XSUM1;
            b+=a;
            }
        return(b);
        }
/**********************************
static double xsum1(int it)
        {
        int j;
        double a,b;

        if (m1==0) return(MISSING8); // 13.9.2010
        b=0.0;
        for (j=0; j<m; ++j)
            {
            a=x[j];
            if (a==MISSING8) continue;
            b+=a;
            }
        return(b);
        }
**********************************/
static double xsum2(int it)
        {
        int j;
        double a,b;

        if (m1==0) return(MISSING8); // 13.9.2010
        b=0.0;
        for (j=0; j<m; ++j)
            {
            a=x[j];
            if (a==MISSING8) continue;
            b+=a*a;
            }
        return(b);
        }

static double xsum3(int it)
        {
        int j;
        double a,b;

        if (m1==0) return(MISSING8); // 13.9.2010
        b=0.0;
        for (j=0; j<m; ++j)
            {
            a=x[j];
            if (a==MISSING8) continue;
            b+=a*a*a;
            }
        return(b);
        }

static double xsum4(int it)
        {
        int j;
        double a,b;

        if (m1==0) return(MISSING8); // 13.9.2010
        b=0.0;
        for (j=0; j<m; ++j)
            {
            a=x[j];
            if (a==MISSING8) continue;
            a=a*a; b+=a*a;
            }
        return(b);
        }

static double xmean(int it)
        {
        int j;
        double a,b;

        if (m1==0) return(MISSING8);
        if (npar[it]==0) pot=1.0;
        else pot=tp1[it];
        if (pot==1.0) return(sum/(double)m1);
        b=0.0;
        for (j=0; j<m; ++j)
            {
            a=x[j];
            if (a==MISSING8) continue;
            if (pot==2.0) a=a*a;
            else if (pot==0.0) { if (a<=0.0) return(MISSING8); else a=log(a); }
            else if (pot==3.0) a=a*a*a;
            else if (pot==4.0) { a=a*a; a=a*a; }
            else a=pow(a,pot);
            b+=a;
            }
        if (pot==2.0) return(sqrt(b/(double)m1));
        if (pot==0.0) return(exp(b/(double)m1));
        return(pow(b/(double)m1,1/pot));
        }

static double xpos_side(int it)
        {
        int j;
        double b,x1,x2;

        b=0.0; if (x[0]>0.0) b=1.0;
        for (j=1; j<m; ++j)
            {
            x1=x[j]; x2=x[j-1];
            if ((x1>0.0 && x2>=0.0) || (x1>=0.0 && x2>0.0)) ++b;
            }
        return(b);
        }

static int nval;
/* VARSTAT <data>,Y,FIRST,<list> / <list>=val1,val2,...       */
/* finds the start position of val1,val2,... among variables. */
/* - indicates missing value in <list>                        */
static double xfirst(int it)
        {
        int i,j;
        char y[LLENGTH],*val[100];

        if (alku)
            {
            i=spfind(word[4]);
            if (i<0) { sur_print("\nUsage: VARSTAT <data>,Y,FIRST,LIST / LIST=V1,V2,...");
                       WAIT; muste_exit=TRUE; return(0); // RS CHA exit(0);
                     }
            strcpy(y,spb[i]);
            nval=split(y,val,m);
            for (i=0; i<nval; ++i)
                {
                if (strcmp(val[i],"-")==0) x2[i]=MISSING8;
                else x2[i]=atof(val[i]);
                }
            alku=0;
            }

        i=0;
        while (i<m-nval+1)
            {
            if (x[i]==x2[0])
                {
                for (j=1; j<nval; ++j) if (x[i+j]!=x2[j]) break;
                if (j<nval) { ++i; continue; }
                return((double)(i+1));
                }
            ++i;
            }
        return(MISSING8);
        }

static double xlast(int it)
        {
        int i,j,ilast;
        char y[LLENGTH],*val[100];

        if (alku)
            {
            i=spfind(word[4]);
            if (i<0) { sur_print("\nUsage: VARSTAT <data>,Y,LAST,LIST / LIST=V1,V2,...");
                       WAIT; muste_exit=TRUE; return(0); // RS CHA exit(0);
                     }
            strcpy(y,spb[i]);
            nval=split(y,val,m);
            for (i=0; i<nval; ++i)
                {
                if (strcmp(val[i],"-")==0) x2[i]=MISSING8;
                else x2[i]=atof(val[i]);
                }
            alku=0;
            }

        i=0; ilast=-1;
        while (i<m-nval+1)
            {
            if (x[i]==x2[0])
                {
                for (j=1; j<nval; ++j) if (x[i+j]!=x2[j]) break;
                if (j<nval) { ++i; continue; }
                ilast=i+1; i+=nval; continue;
                }
            ++i;
            }
        if (ilast==-1) return(MISSING8);
        return((double)ilast);
        }

static double xcyclen(int it)
        {
        int j,k,h,m2,f;

        m2=m/2+1;
        for (k=1; k<m2; ++k)
            {
            f=0;
            for (j=k; j<m; j+=k)
                {
                for (h=0; h<k && j+h<m; ++h)
                    if (x[h]!=x[j+h]) { f=1; break; }
                }
            if (f==0) break;
            }
        if (k==m2) k=m;
        return((double)k);
        }

static double xmodeval(int it)
        {
        int i,j,k,max;
        double a;

        max=0;
        for (i=0; i<m; ++i)
            {
            a=x[i]; k=0;
            for (j=0; j<m; ++j)
                if (a==x[j]) ++k;
            if (k>max) max=k;
            }
        return((double)max);
        }

/* VARSTAT <data>,Y,#SEQ,<list> / <list>=val1,val2,...        */
/* finds number of sequences val1,val2,... among variables.   */
/* - indicates missing value in <list>                        */

static double xnumseq(int it)
        {
        int i,j;
        char y[LLENGTH],*val[100];
        int nseq,i0;
        int overlap; // 21.5.2001

        overlap=0;
        if (alku)
            {
            i=spfind(word[4]);
            if (i<0) { sur_print("\nUsage: VARSTAT <data>,Y,#SEQ,LIST / LIST=V1,V2,...");
                       WAIT; muste_exit=TRUE; return(0); // RS CHA exit(0);
                     }
            strcpy(y,spb[i]);
            nval=split(y,val,m);
            for (i=0; i<nval; ++i)
                {
                if (strcmp(val[i],"-")==0) x2[i]=MISSING8;
                else x2[i]=atof(val[i]);
                }
            alku=0;
            overlap=0;
            i=spfind("OVERLAP"); if (i>=0) overlap=atoi(spb[i]);
            }

        i=0; nseq=0;
        while (i<m-nval+1)
            {
            i0=i;
            if (x[i]==x2[0])
                {
                for (j=1; j<nval; ++j) if (x[i+j]!=x2[j]) break;
                if (j<nval) { ++i; continue; }
                ++nseq;
                if (overlap) ++i; else i=i0+nval;
                continue;
                }
            ++i;
            }
        return((double)nseq);
        }

/* VARSTAT <data>,Y,#RUN,<list> / <list>=val1,val2,...             */
/* finds number of runs of values val1,val2,... among variables.   */
/* - indicates missing value in <list>                             */

static double xnumrun(int it)
        {
        int i,j;
        char y[LLENGTH],*val[100];
        int nrun,run;

        if (alku)
            {
            i=spfind(word[4]);
            if (i<0) { sur_print("\nUsage: VARSTAT <data>,Y,#SEQ,LIST / LIST=V1,V2,...");
                       WAIT; muste_exit=TRUE; return(0); // RS CHA exit(0);
                     }
            strcpy(y,spb[i]);
            nval=split(y,val,m);
            for (i=0; i<nval; ++i)
                {
                if (strcmp(val[i],"-")==0) x2[i]=MISSING8;
                else x2[i]=atof(val[i]);
                }
            alku=0;
            }

        nrun=0; run=0;
        for (i=0; i<m; ++i)
            {
            if (run)
                {
                for (j=0; j<nval; ++j) if (x[i]==x2[j]) break;
                if (j==nval) run=0;
                }
            else
                {
                for (j=0; j<nval; ++j) if (x[i]==x2[j]) break;
                if (j<nval) { run=1; ++nrun; }
                }
            }
        return((double)nrun);
        }


static double xrun_up_down(int it)
        {
        int i,numrun,up;

        numrun=1;
        if (x[1]>=x[0]) up=1; else up=0;
        for (i=2; i<m; ++i)
            {
            if (up)
                {
                if (x[i-1]>x[i]) { up=0; ++numrun; }
                }
            else
                {
                if (x[i-1]<x[i]) { up=1; ++numrun; }
                }
            }
        return((double)numrun);
        }

static void not_enough_memory()
        {
        sur_print("\nNot enough memory (VARSTAT)"); WAIT;
        }

static int varaa_tilat()
        {
        x=(double *)muste_malloc(m*sizeof(double));
        if (x==NULL) { not_enough_memory(); return(-1); }
        x2=(double *)muste_malloc(m*sizeof(double));
        if (x2==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static double xstddev(int it)
        {
        int j;
        double a,b,c;

        if (m1==0) return(MISSING8);
        if (m1==1) return(0.0);
        b=0.0; c=sum/(double)m1;
        for (j=0; j<m; ++j)
            {
            a=x[j];
            if (a==MISSING8) continue;
            b+=(a-c)*(a-c);
            }
        return(sqrt(b/(double)(m1-1)));
        }

static double xmax(int it)
        {
        if (m1==0) return(MISSING8);
        if (!sort) sort_x();
        return(x2[m1-1]);
        }

static double xmin(int it)
        {
        if (m1==0) return(MISSING8);
        if (!sort) sort_x();
        return(x2[0]);
        }

static double xrange(int it)
        {
        if (m1==0) return(MISSING8);
        if (!sort) sort_x();
        return(x2[m1-1]-x2[0]);
        }

static double xmedian(int it)
        {
        if (m1==0) return(MISSING8);
        if (!sort) sort_x();
        if (((m1>>1)<<1)==m1)
            {
            if (npar[it]>0 && tp1[it]!=0.0)
                return((x2[m1/2-1]+x2[m1/2])/2);
            else return(x2[m1/2-1]);
            }
        else
            return(x2[m1/2]);
        }

static int sort_x()
        {
        int i,h,k;
        int ind;
        double y;

        h=0; for (i=0; i<m; ++i) { if (x[i]==MISSING8) continue;
                                   x2[h]=x[i]; ++h;
                                 }
        if (m1<=1) return(1);
        h=m1;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind=1;
                for (k=0; k<m1-h; ++k)
                    {
                    if (x2[k]>x2[k+h])
                        {
                        y=x2[k]; x2[k]=x2[k+h]; x2[k+h]=y;
                        ind=0;
                        }
                    }
                if (ind==1) break;
                }
            }
/*
printf("\nSort:");
for (i=0; i<m1; ++i) Rprintf(" %g",x2[i]); getch();
*/
        sort=1;
        return(1);
        }

static double xval(int it)
        {
        int j,n;
        int i; // SM ADD 22.5.2012
        double a=0.0,b=0.0,c;

// SM ADD 22.5.2012
        if (strcmp(str_tp1,"VAL_LIST")==0)
            {
            n=0;
            for (j=0; j<m; ++j)
                {
                for (i=0; i<n_list; ++i)
                    if (x[j]==val_list[i]) ++n;
                }
            return((double)n);
            }

        if (str_val_type)
            {
// Rprintf("\nstr_val_type=%d",str_val_type);
            n=0;
            for (j=0; j<m; ++j)
                {
                for (i=0; i<n_list; ++i)
                    {
            //      Rprintf("\nstr=%s|val=%s|",str_x[j],str_val_list[i]);
                    if (str_val_type==1)
                        {
                        if (strcmp(str_x[j],str_val_list[i])==0) ++n;
                        }
                    else
                        {
                        if (strstr( str_x[j],str_val_list[i] )!=NULL) ++n;
// Rprintf("\nn=%d",n);
                        }
                    }
                }
            return((double)n);
            }

        switch (npar[it])
            {
          case 0: a=b=1.0; break;
          case 1: a=b=tp1[it]; break;
          case 2: a=tp1[it]; b=tp2[it];
            }
/* Rprintf("a=%g b=%g\n",a,b);  */
        n=0;
        for (j=0; j<m; ++j)
            {
            c=x[j];
            if (c==MISSING8) continue;
            if (c>=a && c<=b) ++n;
            }
/* Rprintf("n=%d\n",n); getch();  */
        return((double)n);
        }

/* varstat3.c 16.3.1993/SM (16.3.1993)
*/

static void xdivsum()
        {
        int j;
        double coeff;

        coeff=atof(word[4]); // 19.12.2009
        if (coeff==0.0) coeff=1.0;

        if (m1==0) return;

        for (j=0; j<m; ++j)
            {
            if (x[j]==MISSING8) continue;
            if (sum==0.0) x[j]=MISSING8; else x[j]=coeff*x[j]/sum;
            }
        }

static void xdivmax()
        {
        int j;
        double max;
        double coeff;

        coeff=atof(word[4]); // 19.12.2009
        if (coeff==0.0) coeff=1.0;

        if (m1==0) return;
        if (!sort) sort_x();
        max=x2[m1-1];
        for (j=0; j<m; ++j)
            {
            if (x[j]==MISSING8) continue;
            if (max==0.0) x[j]=MISSING8; else x[j]=coeff*x[j]/max;
            }
        }

static void xnorm()
        {
        int j;
        double k,a,b,c;

        if (m1==0) return;
        k=2.0; a=1.0;
        if (npar[0]) k=tp1[0];
        if (npar[0]>1) a=tp2[0];
        c=0.0;
        for (j=0; j<m; ++j)
            {
            b=x[j];
            if (b==MISSING8) continue;
            c+=pow(fabs(b),k);
            }
        c=pow(c,1.0/k);
        if (c==0.0) return;
        for (j=0; j<m; ++j)
            {
            if (x[j]==MISSING8) continue;
            x[j]*=a/c;
            }
        }

static void xcenter()
        {
        int j;
        double mean;

        if (m1==0) return;
        mean=sum/(double)m1;
        for (j=0; j<m; ++j)
            {
            if (x[j]==MISSING8) continue;
            x[j]-=mean;
            }
        }

static void xstandard()
        {
        int j;
        double a,mean,stddev;

        if (m1<2) return;
        mean=sum/(double)m1;
        stddev=0.0;
        for (j=0; j<m; ++j)
            {
            if (x[j]==MISSING8) continue;
            a=x[j]-mean; stddev+=a*a;
            }
        stddev=sqrt(stddev/(double)(m1-1));
        for (j=0; j<m; ++j)
            {
            if (x[j]==MISSING8) continue;
            if (stddev==0.0) x[j]=0.0;
            else x[j]=(x[j]-mean)/stddev;
            }
        }

static int xarit(int op,int l)
// int op;  /* 1=* 2=/ 3=+ 4=- */
// int l;
        {
        static int eka=1;
        static int iv;
        static double a;
        int j;

        if (m1==0) return(1);
        if (eka)
            {
            if (npar[0]==0)
                {
                sur_print("\nOperand or constant missing!");
                WAIT; return(-1);
                }
            iv=varfind2(&d,aritvar,0);
            if (iv<0)
                {
                a=atof(aritvar);
                if (a==0.0)
                    {
                    sprintf(sbuf,"\nInvalid variable or constant =%s",aritvar);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                }
            eka=0;
            }
        if (iv>=0) data_load(&d,l,iv,&a);
        for (j=0; j<m; ++j)
            {
            if (x[j]==MISSING8) continue;
            if (a==MISSING8) { x[j]=MISSING8; continue; }
            switch (op)
                {
              case 1: x[j]*=a; break;
              case 2: if (a==0.0) x[j]=MISSING8; else x[j]/=a; break;
              case 3: x[j]+=a; break;
              case 4: x[j]-=a; break;
                }
            }
        return(1);
        }

static int cumulative_sum()
        {
        int j;
        double sum;

        sum=0.0;
        for (j=0; j<m; ++j)
            {
            if (x[j]==MISSING8) continue;
            sum+=x[j]; x[j]=sum;
            }
        return(1);
        }

