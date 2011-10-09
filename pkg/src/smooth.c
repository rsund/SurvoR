/* _smooth.c 11.8.1987/SM (1.4.1992)
    SMOOTH <data>,X,Y,pts
*/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
// #include <conio.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXN 1000000

static SURVO_DATA d;
static int xvar,svar;
static double x[MAXN];
static unsigned int n;
static double pts;
static int disp0;

static int lue_datat();
static int talletus();
static int smooth(double *y,unsigned int n,double pts);
static int realft(double *data,unsigned int n,int isign);
static int four1(double *data,unsigned int nn,int isign);

// char **specs;
/*******************
main(argc,argv)
int argc; char *argv[];
**************************/

void muste_smooth(char *argv)
        {
        int i;

   //   if (argc==1) return;
        s_init(argv);

        if (g<4)
            {
            sur_print("\nUsage: SMOOTH <data>,<var>,<smoothened_var>,<pts>");
            WAIT; return;
            }
        i=data_open(word[1],&d);
        xvar=varfind(&d,word[2]); if (xvar<0) return;
        svar=varfind(&d,word[3]); if (svar<0) return;

        i=conditions(&d); if (i<0) return;
        i=lue_datat(); if (i<0) return;

        if (g<5) pts=n/10.0; else pts=atof(word[4]);
        smooth(x,n,pts);
        talletus();
        data_close(&d);
        s_end(argv); // 5.2.2002
        }

static int lue_datat()
        {
        int j;
        double a;

        n=0; disp0=0;
        sprintf(sbuf,"\nLoading values of %.8s ...",d.varname[xvar]);
        sur_print(sbuf);
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            data_load(&d,j,xvar,&a);
            if (a==MISSING8) continue;
            x[n++]=a;
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); disp0=1-disp0; }
            if (disp0) { sprintf(sbuf," %d",j); sur_print(sbuf); }

            if (n>MAXN)
                {
                sprintf(sbuf,"\nToo many (>%u) observations!",MAXN);
                sur_print(sbuf); WAIT; return(-1);
                }
            }
        if (n==0)
            {
            sur_print("\nNo valid observations!");
            WAIT; return(-1);
            }
        sprintf(sbuf,"\n%u observations of %.8s loaded!",n,d.varname[xvar]);
        sur_print(sbuf);
        return(1);
        }

static int talletus()
        {
        int j;
        double a;

        n=0;
        sprintf(sbuf,"\nSaving smoothed values in %.8s ...",d.varname[svar]);
        sur_print(sbuf);
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            data_load(&d,j,xvar,&a);
            if (a==MISSING8) continue;
            a=x[n++];
            data_save(&d,j,svar,a);
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); disp0=1-disp0; }
            if (disp0) { sprintf(sbuf," %d",j); sur_print(sbuf); }
            }
        return(1);
        }

static int smooth(double *y,unsigned int n,double pts)
        {
        unsigned int m,nmin,mo2,k,j;
        double yn,y1,rn1,fac,cnst;

        m=2;
        nmin=n+2*pts;
        sprintf(sbuf,"\nSmoothing %u observations with parameter %g ...",n,pts);
        sur_print(sbuf);
        while (m<nmin) m*=2;
        cnst=pts*pts/m/m; y1=y[0]; yn=y[n-1]; rn1=1.0/(n-1);
        for (j=0; j<n; ++j) y[j]-=rn1*(y1*(n-j-1)+yn*j);
        if (n+1<=m)
            for (j=n; j<m; ++j) y[j]=0.0;
        mo2=m/2;
        sur_print("\nFourier transformation...");
        realft(y,mo2,1); y[0]/=mo2; fac=1.0;
        for (j=1; j<=mo2-1; ++j)       /*  y[j] --> y[j-1] */
            {
            k=2*j+1;
            if (fac!=0.0)
                {
                fac=(1-cnst*j*j)/mo2;
                if (fac<0.0) fac=0.0;
                y[k-1]*=fac; y[k]*=fac;
                }
            else
                {
                y[k-1]=y[k]=0.0;
                }
            }
        fac=(1-0.25*pts*pts)/mo2;
        if (fac<0.0) fac=0.0;
        y[1]=fac*y[1];
        sur_print("\nInverse Fourier transformation...");
        realft(y,mo2,-1);
        for (j=0; j<n; ++j)
            y[j]+=rn1*(y1*(n-j-1)+yn*j);
        return(1);
        }

static int realft(double *data,unsigned int n,int isign)
        {
        int i,i1,i2,i3,i4;
        double wr,wi,wpr,wpi,wtemp,theta;
        double c1,c2,h1r,h1i,h2r,h2i;
        double a;

        theta=6.28318530717959/(2*n); c1=0.5;
        if (isign==1)
            { c2=-0.5; four1(data,n,1); }
        else
            { c2=0.5; theta=-theta; }
        a=sin(0.5*theta); wpr=-2.0*a*a; wpi=sin(theta);
        wr=1+wpr; wi=wpi;
        for (i=2; i<=n/2+1; ++i)
            {
            i1=i+i-1; i2=i1+1; i3=n+n+3-i2; i4=i3+1;
            h1r=c1*(data[i1-1]+data[i3-1]);
            h1i=c1*(data[i2-1]-data[i4-1]);
            h2r=-c2*(data[i2-1]+data[i4-1]);
            h2i=c2*(data[i1-1]-data[i3-1]);
            data[i1-1]=h1r+wr*h2r-wi*h2i;
            data[i2-1]=h1i+wr*h2i+wi*h2r;
            data[i3-1]=h1r-wr*h2r+wi*h2i;
            data[i4-1]=-h1i+wr*h2i+wi*h2r;
            wtemp=wr; wr=wr*wpr-wi*wpi+wr; wi=wi*wpr+wtemp*wpi+wi;
            }
        if (isign==1)
            {
            h1r=data[0]; data[0]=h1r+data[1];
            data[1]=h1r-data[1];
            }
        else
            {
            h1r=data[0]; data[0]=c1*(h1r+data[1]);
            data[1]=c1*(h1r-data[1]); four1(data,n,-1);
            }
        return(1);
        }

static int four1(double *data,unsigned int nn,int isign)
        {
        int n,mmax,m,j,istep,i;
        double wtemp,wr,wpr,wpi,wi,theta;
        double tempr,tempi;
        double a;

        n=2*nn; j=1;
        for (i=1; i<=n; i+=2)
            {
            if (j>i)
                {
                tempr=data[j-1]; tempi=data[j]; data[j-1]=data[i-1];
                data[j]=data[i]; data[i-1]=tempr; data[i]=tempi;
                }
            m=n/2;
            while (m>=2 && j>m) { j-=m; m/=2; }
            j+=m;
            }
        mmax=2;
        while (n>mmax)
            {
            istep=2*mmax; theta=6.28318530717959/(isign*mmax);
            a=sin(0.5*theta); wpr=-2*a*a; wpi=sin(theta); wr=1.0; wi=0.0;
            for (m=1; m<=mmax; m+=2)
                {
                for (i=m; i<=n; i+=istep)
                    {
                    j=i+mmax;
                    tempr=wr*data[j-1]-wi*data[j];
                    tempi=wr*data[j]+wi*data[j-1];
                    data[j-1]=data[i-1]-tempr; data[j]=data[i]-tempi;
                    data[i-1]+=tempr; data[i]+=tempi;
                    }
                wtemp=wr; wr=wr*wpr-wi*wpi+wr; wi=wi*wpr+wtemp*wpi+wi;
                }
            mmax=istep;
            }
        return(1);
        }

