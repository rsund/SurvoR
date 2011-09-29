/* _xcorr.c 19.9.1993/SM (20.9.1993)
    XCORR <data>,X,Y,L
*/
#include <stdio.h>
// #include <string.h>
#include <stdlib.h>
// #include <conio.h>
#include <math.h>
//#include <malloc.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;
static int xvar,yvar;
static int maxlag;
static int tulosrivi;
static int autocorr;

static double *xx,*yy,*xy1,*xy2;
static double xsum,ysum,xsum2,ysum2;
static double *x2,*y2;
static double *xs1,*xs2;
static double *ys1,*ys2;
static int n;
static double xmean,ymean;
static double xxs,yys;
static double corr;

static int varaa_tilat_xcorr();
static int ei_tilaa();
static int lue_datat();
static int var_const(char *s);
static int tulostus();
static int print_line(char *line);

void muste_xcorr(char *argv)
        {
        int i;

        s_init(argv);

        if (g<4)
            {
            sur_print("\nUsage: XCORR <data>,<xvar>,<yvar>,L");
            WAIT; return;
            }
        i=sp_init(r1+r-1); if (i<0) return;
        i=data_read_open(word[1],&d); if (i<0) return;
        xvar=varfind(&d,word[2]); if (xvar<0) return;
        yvar=varfind(&d,word[3]); if (yvar<0) return;
        if (xvar==yvar) autocorr=1; else autocorr=0;

        if (g<5) tulosrivi=0;
        else
            {
            tulosrivi=edline2(word[4],1);
            if (tulosrivi==0) return;
            }
        i=conditions(&d); if (i<0) return;
        i=spfind("MAXLAG");
        if (i<0) maxlag=12; else maxlag=atoi(spb[i]);
        if (maxlag<1) maxlag=1;

    xx=NULL;
    yy=NULL;
    xy1=NULL;
    xy2=NULL;
    x2=NULL;
    y2=NULL;
    xs1=NULL;
    xs2=NULL;
    ys1=NULL;
    ys2=NULL;

        i=varaa_tilat_xcorr(); if (i<0) return;
        n=0;
        for (i=0; i<maxlag; ++i) xx[i]=yy[i]=xy1[i]=xy2[i]=0.0;
        for (i=0; i<maxlag; ++i) xs1[i]=xs2[i]=ys1[i]=ys2[i]=0.0;
        xsum=ysum=xsum2=ysum2=0.0;
        corr=0.0;
        i=lue_datat();
        data_close(&d);
        if (i<0) return;
        if ((int)maxlag>n-1) maxlag=n-1;
        tulostus();
        s_end(argv);
        }

static int varaa_tilat_xcorr()
        {
        xx=(double *)muste_malloc(maxlag*sizeof(double));
        if (xx==NULL) { ei_tilaa(); return(-1); }
        yy=(double *)muste_malloc(maxlag*sizeof(double));
        if (yy==NULL) { ei_tilaa(); return(-1); }
        xy1=(double *)muste_malloc(maxlag*sizeof(double));
        if (xy1==NULL) { ei_tilaa(); return(-1); }
        xy2=(double *)muste_malloc(maxlag*sizeof(double));
        if (xy2==NULL) { ei_tilaa(); return(-1); }
        x2=(double *)muste_malloc(maxlag*sizeof(double));
        if (x2==NULL) { ei_tilaa(); return(-1); }
        y2=(double *)muste_malloc(maxlag*sizeof(double));
        if (y2==NULL) { ei_tilaa(); return(-1); }
        xs1=(double *)muste_malloc(maxlag*sizeof(double));
        if (xs1==NULL) { ei_tilaa(); return(-1); }
        xs2=(double *)muste_malloc(maxlag*sizeof(double));
        if (xs2==NULL) { ei_tilaa(); return(-1); }
        ys1=(double *)muste_malloc(maxlag*sizeof(double));
        if (ys1==NULL) { ei_tilaa(); return(-1); }
        ys2=(double *)muste_malloc(maxlag*sizeof(double));
        if (ys2==NULL) { ei_tilaa(); return(-1); }

        return(1);
        }

static int ei_tilaa()
        {
        sur_print("\nNot enough memory!"); WAIT; return(1);
        }

static int lue_datat()
        {
        int j;
        double x,y;
        int i,k,h;

        k=0;
        if (autocorr) { sprintf(sbuf,"\nLoading values of %s ...",word[2]); sur_print(sbuf); }
      else { sprintf(sbuf,"\nLoading values of %s and %s ...",word[2],word[3]); sur_print(sbuf); }
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            data_load(&d,j,xvar,&x);
            if (x==MISSING8) { if (n==0) continue; break; }
            if (!autocorr)
                {
                data_load(&d,j,yvar,&y);
                if (y==MISSING8) { if (n==0) continue; break; }
                }

            xsum+=x; xsum2+=x*x;
            if (!autocorr) { ysum+=y; ysum2+=y*y; corr+=x*y; }

            for (i=0; i<maxlag; ++i)
                {
                if ((int)n<i+1) break;
                h=k-i-1; if (h<0) h+=maxlag;
                xx[i]+=x*x2[h];
                xs1[i]+=x; xs2[i]+=x2[h];
                if (!autocorr)
                    {
                    yy[i]+=y*y2[h];
                    ys1[i]+=y; ys2[i]+=y2[h];
                    xy1[i]+=x*y2[h];
                    xy2[i]+=x2[h]*y;
                    }
                }
            x2[k]=x;
            if (!autocorr) y2[k]=y;
            ++k; if (k==maxlag) k=0;
            ++n;
            }

        if (n==0)
            {
            sur_print("\nNo valid observations!");
            WAIT; return(-1);
            }
        xmean=xsum/(double)n;
        xxs=xsum2-xsum*xmean;
        if (fabs(xxs)<1e-10) { var_const(word[2]); return(-1); }
        if (!autocorr)
            {
            ymean=ysum/(double)n;
            yys=ysum2-ysum*ymean;
            if (fabs(yys)<1e-10) { var_const(word[3]); return(-1); }
            }
        for (i=0; i<maxlag; ++i)
            {
            xx[i]=(xx[i]-xmean*(xs1[i]+xs2[i])+(n-i-1)*xmean*xmean)/xxs;
            if (!autocorr)
                {
                yy[i]=(yy[i]-ymean*(ys1[i]+ys2[i])+(n-i-1)*ymean*ymean)/yys;
                xy1[i]=(xy1[i]-ymean*xs1[i]-xmean*ys2[i]+(n-i-1)*xmean*ymean)
                         /sqrt(xxs*yys);
                xy2[i]=(xy2[i]-ymean*xs2[i]-xmean*ys1[i]+(n-i-1)*xmean*ymean)
                         /sqrt(xxs*yys);

                }
            }
        if (!autocorr) corr=(corr-n*xmean*ymean)/sqrt(xxs*yys);

        sprintf(sbuf,"\n%u observations loaded!",n); sur_print(sbuf);


        return(1);
        }

static int var_const(char *s)
        {
        sprintf(sbuf,"\nVariable %s is constant!",s);
        sur_print(sbuf); WAIT; return(1);
        }

static int tulostus()
        {
        int i,t;
        char x[LLENGTH];

        i=output_open(eout); if (i<0) return(1);
        if (autocorr) sprintf(x,"Autocorrelations of %s in data %s:",word[2],word[1]);
    else sprintf(x,"Auto- and crosscorrelations of %s and %s in data %s:",word[2],word[3],word[1]);
        print_line(x);

        if (autocorr) sprintf(x," Lag  %.8s",word[2]);
        else sprintf(x," Lag %8.8s   %8.8s       Cross+     Cross-",word[2],word[3]);
        print_line(x);
        t=accuracy-3;
        if (autocorr)
            sprintf(x,"%3d  %10.*f",0,t,1.0);
        else
            sprintf(x,"%3d  %10.*f %10.*f %10.*f %10.*f",0,t,1.0,t,1.0,t,corr,t,corr);

        print_line(x);
        for (i=0; i<maxlag; ++i)
            {
            if (autocorr)
                sprintf(x,"%3d  %10.*f",i+1,t,xx[i]);
            else
               sprintf(x,"%3d  %10.*f %10.*f %10.*f %10.*f",i+1,t,xx[i],t,yy[i],t,xy1[i],t,xy2[i]);
            print_line(x);
            }
        output_close(eout);
        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

