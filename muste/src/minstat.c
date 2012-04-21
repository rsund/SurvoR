/* _minstat.c 18.3.2002/SM (18.3.2002)
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
static int m_act;

static int *f;
static double *sum, *sum2;
static double *min,*max;
static double *values;
static int jitter;
static int *nval;
static double *x0;

static int n;
static int results_line;
static int prind=0;

static double *aa;
static int mm,nn;
static char *rlab,*clab;
// static char expr[128];

static int space_allocation();
static int not_enough_memory();
static int compute_stat();
static int printout();
static int print_line(char *line);
static int find_values(int i,double x);

/****************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "JITTER", "PRIND", "!" };
char **specs=specs0;
*******************************/
/********************
void main(argc,argv)
int argc; char *argv[];
************************/

void muste_minstat(char *argv)
        {
        int i;

//      if (argc==1) return;
        s_init(argv);
        if (g<2)
            {
            sur_print("\nUsage: MINSTAT <SURVO_data>,<output_line>");
            WAIT; return;
            }
        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }
        prind=0;

    f=NULL;
    sum=NULL;
    sum2=NULL;
    min=NULL;
    max=NULL;
    x0=NULL;
    values=NULL;
    nval=NULL;
    aa=NULL;
    rlab=NULL;
    clab=NULL;

        i=data_read_open(word[1],&d); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;
        m_act=d.m_act;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        i=conditions(&d); if (i<0) return;  /* permitted only once */
        i=space_allocation(); if (i<0) return;

        i=compute_stat(); if (i<0) return;
        printout();
        data_close(&d);
        s_end(argv);
        }

static int space_allocation()
        {
        int i;

        f=(int *)muste_malloc(m_act*sizeof(int));
        if (f==NULL) { not_enough_memory(); return(-1); }
        sum=(double *)muste_malloc(m_act*sizeof(double));
        if (sum==NULL) { not_enough_memory(); return(-1); }
        sum2=(double *)muste_malloc(m_act*sizeof(double));
        if (sum2==NULL) { not_enough_memory(); return(-1); }
        min=(double *)muste_malloc(m_act*sizeof(double));
        if (min==NULL) { not_enough_memory(); return(-1); }
        max=(double *)muste_malloc(m_act*sizeof(double));
        if (max==NULL) { not_enough_memory(); return(-1); }
        x0=(double *)muste_malloc(m_act*sizeof(double));
        if (x0==NULL) { not_enough_memory(); return(-1); }

        jitter=0;
        i=spfind("JITTER");
        if (i>=0)
            {
            jitter=atoi(spb[i]);
            values=(double *)muste_malloc(jitter*m_act*sizeof(double));
            if (values==NULL) { not_enough_memory(); return(-1); }
            nval=(int *)muste_malloc(m_act*sizeof(int));
            if (nval==NULL) { not_enough_memory(); return(-1); }
            }

        mm=m_act;
        nn=5; if (jitter) nn=7;
        aa=(double *)muste_malloc(mm*nn*sizeof(double));
        if (aa==NULL) { not_enough_memory(); return(-1); }
        rlab=(char *)muste_malloc(mm*8+1);
        if (rlab==NULL) { not_enough_memory(); return(-1); }
        clab=(char *)muste_malloc(nn*8+1);
        if (clab==NULL) { not_enough_memory(); return(-1); }

        return(1);
        }

static int not_enough_memory()
    { sur_print("\nNot enough memory!"); WAIT; return(1); }


static int compute_stat()
        {
        int i;
        int l;
        double x,a;

        n=0L;
        for (i=0; i<d.m_act; ++i)
            {
            sum[i]=sum2[i]=0.0; f[i]=0L;
            min[i]=1e300; max[i]=-1e300;
            }

        if (jitter) for (i=0; i<m_act; ++i) nval[i]=0;

        for (l=d.l1; l<=d.l2; ++l) // tukihavainnon haku!
            {
            if (unsuitable(&d,l)) continue;
            for (i=0; i<m_act; ++i)
                {
                data_load(&d,l,d.v[i],&x);
                if (x==MISSING8) x=0.0;
                x0[i]=x;
                }
            break;
            }
        if (l>d.l2)
            {
            sur_print("\nNo acceptable observations!");
            WAIT; return(-1);
            }
        sur_print("\n");

        for (l=d.l1; l<=d.l2; ++l)
            {
            if (unsuitable(&d,l)) continue;
            ++n;
            if (prind)
                {
                sprintf(sbuf,"%d ",l); sur_print(sbuf);
                }
            for (i=0; i<d.m_act; ++i)
                {
                data_load(&d,l,d.v[i],&x);
                if (x==MISSING8) continue;
                ++f[i]; a=x-x0[i]; sum[i]+=a; sum2[i]+=a*a;
                if (x<min[i]) min[i]=x;
                if (x>max[i]) max[i]=x;
                if (jitter) find_values(i,x);
                }
            }
        return(1);
        }

static int printout()
        {
        int i,k,h;
        char line[LLENGTH];
        char stat[32];
        double a0,b0;

        output_open(eout);
        sprintf(line,"Basic statistics of data %s N=%d",
                          word[1],n);
        print_line(line);

        if (d.m_act==0) // 26.1.2003
            {
            output_close(eout);
            return(1);
            }

        strcpy(line,"Variable   ");
        strncat(line,"mean               ",accuracy+3);
        strncat(line,"stddev             ",accuracy+3);
        strncat(line,"      N  ",9);
        strncat(line,"minimum            ",accuracy+3);
        strncat(line,"maximum            ",accuracy+3);
        strcpy(clab,"mean    stddev  N       minimum maximum ");

        if (jitter)
            {
            strncat(line,"#val  ",6);
            strncat(line,"jitter             ",accuracy+3);
            strcat(clab,"#val    jitter  ");
            }
        print_line(line);

        for (i=0; i<mm*nn; ++i) aa[i]=0.0;
        *rlab=EOS;
        for (i=0; i<d.m_act; ++i)
            {
            strcpy(sbuf,d.varname[d.v[i]]);
            for (h=strlen(sbuf); h<8; ++h) sbuf[h]=' '; sbuf[8]=EOS;
            strcat(rlab,sbuf);

            if (f[i]==0L)
                sprintf(line,"%-8.8s            -  %6d",d.varname[d.v[i]],
                         n-f[i]);
            else
                {
                a0=sum[i]/(double)f[i];
                fnconv(a0+x0[i],accuracy+2,stat);
                aa[i+mm*0]=a0+x0[i];
                k=sprintf(line," %-8.8s %s",d.varname[d.v[i]],
                                stat);
                b0=f[i]-1; if (b0==0.0) b0=1.0;
                a0=sqrt((sum2[i]-a0*a0*(double)f[i])/b0);
                fnconv(a0,accuracy+2,stat);
                aa[i+mm*1]=a0;

                k+=sprintf(line+k," %s",stat);
                sprintf(stat,"%8d",f[i]);
                aa[i+mm*2]=(double)f[i];
                k+=sprintf(line+k," %s",stat);

                fnconv(min[i],accuracy+2,stat);
                aa[i+mm*3]=min[i];
                k+=sprintf(line+k," %s",stat);
                fnconv(max[i],accuracy+2,stat);
                aa[i+mm*4]=max[i];
                k+=sprintf(line+k," %s",stat);

                if (jitter)
                    {
                    h=nval[i]; if (h==-1) h=0;
                    aa[i+mm*5]=(double)h;
                    sprintf(stat,"%5d",h);
                    k+=sprintf(line+k," %s",stat);

                    if (h<2) a0=0.0;
                    else a0=(max[i]-min[i])/(nval[i]-1);
                    fnconv(a0,accuracy+2,stat);
                    aa[i+mm*6]=a0;
                    k+=sprintf(line+k," %s",stat);
                    }
                }
            print_line(line);
            }
        output_close(eout);

        sprintf(line,"%sSTAT.M",edisk);
        sprintf(sbuf,"Basic_statistics_of_%s",word[1]);
        matrix_save(line,aa,mm,nn,rlab,clab,8,8,-1,sbuf,0,0);

        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

static int find_values(int i,double x)
        {
        double *px,*px0;
        int k,h;

        if (nval[i]<0) return(1);
        if (nval[i]==0)
            {
            values[i*jitter]=x;
            nval[i]=1; return(1);
            }
        px=px0=&values[i*jitter];
        k=0;
        while (k<nval[i])
            {
            if (x==*px) return(1);
            if (x<*px)
                {
                if (nval[i]==jitter) { nval[i]=-1; return(1); }
                for (h=nval[i]-1; h>=k; --h) px0[h+1]=px0[h];
                *px=x; ++nval[i]; return(1);
                }
            ++px; ++k;
            }
        if (nval[i]==jitter) { nval[i]=-1; return(1); }
        *px=x; ++nval[i];
        return(1);
        }

