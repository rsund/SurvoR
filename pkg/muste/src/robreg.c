#include "muste.h"
/* _robreg.c 16.12.1991/RS (17.3.1992/SM  6.2.1994/RS 21.10.2001)
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

#define M 80        /* selittäjien max lkm */
#define M1 8000     /* havaintojen max lkm */
#define M2 8000     /* regressiomatriisin alkioiden max lkm */

static SURVO_DATA d;
static int results_line;
static int weight_variable;
static int nxvar,xvar[100];
static int yvar;
static int m;  /* # of regressors */
static int cc; /* vakiotermin osoitin: cc=0 ei vakiota,jos vakio cc=1 */
static int trials; /* kokeiden lkm */
static int disc ;   /* hylättyjen otosten lkm */
static int di;  /* di=1, jos hylätty, muuten di=0  */
static int otos_nro; /* poimittujen otosten lkm */
static unsigned int sluku[M];
static int sampled[M1];
static double yy,xx[M];
static double ydata[M1],xdata[M2];
static double *xsample,*ysample,*btrial,*blms,scale_estimate;
static double *resid,*sovite,*resid2,*std_resid;
static double order_statistics;
static int sort_sample(double *x_space,int n_total);
static FILE *datat;
static int n;
static double *ou;
static double *op;
static double *opx;
static double *oq;

static int select_model();
static int space_allocation1();
static int space_allocation2();
static int space_allocation3();
static int not_enough_memory();
static int read_data();
static int read_data2();
static int n_of_trials();
static int trial();
static int constant_term();
static int residuals();
static int printout();
static int print_line(char *line);
static int sort_sample(double *x_space,int n_total);
static int ordstat(double *x_space,int n_total,int order);
static int ortho(double *a,int n,int m,double *b,int k,double eps,double *x,int improvement);
static int ortho_malloc=0;

extern char **spb;


/**********************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "CONSTANT", "TRIALS", "SEED", "!" };
char **specs=specs0;
***********************/
/***************************
void main(argc,argv)
int argc; char *argv[];
*****************************/

void muste_robreg(char *argv)
        {
        int i;

        s_init(argv);
        if (g<2)
            {
            init_remarks();
            rem_pr("Usage: ROBREG <data>,L                                ");
            wait_remarks(2);
            return;
            }

    xsample=NULL;
    ysample=NULL;
    btrial=NULL;
    blms=NULL;
    resid2=NULL;
    resid=NULL;
    sovite=NULL;
    std_resid=NULL;
    ou=NULL;
    op=NULL;
    opx=NULL;
    oq=NULL;

        ortho_malloc=0;
        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return;
            }
        i=data_read_open(word[1],&d); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;
        i=space_allocation1(); if (i<0) return;
        i=select_model(); if (i<0) return;
/*      i=test_scaletypes(); if (i<0) return;     */
        i=conditions(&d); if (i<0) return;  /* permitted only once */
        i=space_allocation2(); if (i<0) return;
        i=read_data(); if (i<0) return;
        i=read_data2(); if (i<0) return;
        i=n_of_trials();if (i<0) return;
        i=space_allocation3(); if (i<0) return;
        i=trial(); if (i<0) return;
        i=constant_term(); if (i<0) return;
        i=residuals();if (i<0) return;
        printout();
        data_close(&d);
        s_end(argv);
        }

static int select_model()
        {
        int i,vi;
        char ch;

        weight_variable=activated(&d,'W');
        nxvar=0; yvar=-1;
        for (i=0; i<d.m_act; ++i)
            {
            vi=d.v[i];
            if (vi==weight_variable) continue;
            ch=d.vartype[vi][1];
            if (ch=='X')
                {
                xvar[nxvar]=vi;
                ++nxvar;
                }
            else if (ch=='Y')
                {
                if (yvar>=0)
                    {
                    sur_print("\nMore than 1 regressand!");
                    WAIT; return(-1);
                    }
                yvar=vi;
                }
            } /* i */
        if (yvar==-1)
            {
            sur_print("\nNo regressand (activated by 'Y')!");
            WAIT; return(-1);
            }
        if (nxvar==0)
            {
            sur_print("\nNo regressors (activated by 'X' or max.power)!");
            WAIT; return(-1);
            }

        i=spfind("CONSTANT");
        if (i>=0) {cc=atoi(spb[i]);}
                  else cc=1;
        if (cc>0)  {cc=1;}
        m=nxvar+cc;  /* cc=0 jos ei vakiotermiä mukana */
        return(1);
        }

static int space_allocation1()
        {

        return(1);
        }

static int space_allocation2()
        {
        xsample=(double *)muste_malloc(m*m*sizeof(double));
        if (xsample==NULL) {not_enough_memory(); return(-1);}
        ysample=(double *)muste_malloc(m*sizeof(double));
        if (ysample==NULL) {not_enough_memory(); return(-1);}
        btrial=(double *)muste_malloc(m*sizeof(double));
        if (btrial==NULL) {not_enough_memory(); return(-1);}
        blms=(double *)muste_malloc(m*sizeof(double));
        if (blms==NULL) {not_enough_memory(); return(-1);}
        return(1);
        }
static int space_allocation3()
        {
        resid2=(double *)muste_malloc((int)n*sizeof(double)+0);
        if (resid2==NULL) {not_enough_memory(); return(-1);}
        resid=(double *)muste_malloc((int)n*sizeof(double)+0);
        if (resid==NULL) {not_enough_memory(); return(-1);}
        sovite=(double *)muste_malloc((int)n*sizeof(double)+0);
        if (sovite==NULL) {not_enough_memory(); return(-1);}
        std_resid=(double *)muste_malloc((int)n*sizeof(double)+0);
        if (std_resid==NULL) {not_enough_memory(); return(-1);}
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory! (ROBREG)");
        WAIT; return(1);
        }

static int read_data()
        {
        int i,miss;
        int j;
        char nimi[LLENGTH];
        double a;

        strcpy(nimi,etmpd); strcat(nimi,"SURVO.XXX");
        datat=muste_fopen(nimi,"wb");
        if (datat==NULL)
            {
            sprintf(sbuf,"Cannot open temporary file %s!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        n=0L;
        sur_print("\n");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            data_load(&d,j,yvar,&yy);
            if (yy==MISSING8) continue;
            miss=0;
            for (i=0; i<nxvar; ++i)
                {
                data_load(&d,j,xvar[i],&a);
                if (a==MISSING8) {miss=1; break; }
                xx[i]=a;
                }
            if (miss==1) continue;

            ++n;
            fwrite(&yy,sizeof(double),1,datat);
            fwrite(&xx[0],sizeof(double),nxvar,datat);
            if (ferror(datat))
                {
                sur_print("\nCannot save temporary file!");
                WAIT; muste_fclose(datat); return(-1);
                }
            }
        muste_fclose(datat);
        if (n<(int)(m+1))
            {
            sprintf(sbuf,"\nInvalid # (%d) of observations!",n);
            sur_print(sbuf); WAIT; return(-1);
            }
        if ((int)n>(int)M1 || (int)n*(int)m>(int)M2)
            {
            sprintf(sbuf,"\nToo many observations*regressors (%d)!",n*(int)m);
            sur_print(sbuf); WAIT; return(-1);
            }

        return(1);
        }

static int read_data2()
        {
        int i;
        int j;
        char nimi[LLENGTH];

        strcpy(nimi,etmpd); strcat(nimi,"SURVO.XXX");
        datat=muste_fopen(nimi,"rb");
        if (datat==NULL)
            {
            sprintf(sbuf,"Cannot open temporary file %s!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }

        for (j=0L; j<n; ++j)
            {
            fread(&yy,sizeof(double),1,datat);
            fread(&xx[0],sizeof(double),nxvar,datat);
            ydata[(int)j]=yy;
            for (i=0; i<nxvar; ++i)
                xdata[(int)j*nxvar+i]=xx[i];
            }
        muste_fclose(datat);

        return(1);
        }

static int n_of_trials()      /* palauttaa poimittavien otosten lkm:n, "trials"  */
        {
        int i;
        i=spfind("TRIALS");
        if (i<0) {trials=1500;}
        else trials=atoi(spb[i]);
        return(1);
        }
/*******************
static int mat_print(double *a,int m,int n)
    {
    int i,j;

    for (i=0; i<m; ++i)
        {
        Rprintf("\n");
        for (j=0; j<n; ++j) Rprintf("%g ",a[i+m*j]);
        }
    sur_getch(); return(1);
    }
****************************/

static int trial()
        {
  /*    int trials=300;            muutettu = 300  20.3.93 */
  /*    int otos_nro=1;            */
        double lms_kriteeri=1.0;
        int i,j,h,t;
        unsigned int seed,aika;
    //  time_t *kello;
        time_t time(time_t *kello);

        aika=time(NULL);
        i=spfind("SEED");
        if (i<0) {
                 seed=time(NULL)%32767;    /* asetetaan siemenluku */
                 if (seed%2==0)
                 seed=seed+1;
                 }
        else seed=atoi(spb[i]);

        sprintf(sbuf,"\n        constant=%d",cc);
        sur_print(sbuf);
        sprintf(sbuf,"\n        seed=%u",seed);
        sur_print(sbuf);
        sprintf(sbuf,"\n        number of trials=%d",trials);
        sur_print(sbuf);

 /*       Rprintf ("\n        constant=%d",cc);                */
 /*       Rprintf ("\n        seed=%lu",seed);                */
 /*       Rprintf ("\n        number of trials=%d",trials);   */
        sur_print("\n\n");

        h=n/2+(m+1)/2;
        disc=0;
        otos_nro=0;
/* tästä alkaa looppi:            */

        for(t=1;t<=trials;++t)
           {
           for(i=0;i<(int)n;++i) /* nollataan sovitteet ja "otantamuuttuja" */
              {
              sovite[i]=0.0;
              sampled[i]=0;
              }

           for(i=0;i<m;++i)
              {
              do
               {
                sluku[i]=663608941u*seed%65536u;
                seed=sluku[i]+1;
                sluku[i]=sluku[i]%n;
                }
                while(sampled[sluku[i]]>0);
               sampled[sluku[i]]=1;
               }

           for(i=0;i<m;++i)
              {
              ysample[i]=ydata[sluku[i]];
              xsample[i]=1.0;
                 for(j=cc;j<m;++j)
                 xsample[i+m*j]=xdata[sluku[i]*nxvar+j-cc];  // ???
              }


// mat_print(ysample,1,m);
// mat_print(xsample,m,m);








           di=0;
           ortho(xsample,m,m,ysample,1,1e-15,btrial,0);
           if (di==0)
              {
              for(i=0;i<(int)n;++i)  /* lasketaan residuaalit,sovitteet */
                 {
                 for(j=cc;j<m;++j)
                    {
                    sovite[i]+=xdata[i*nxvar+j-cc]*btrial[j];
                    }
                    sovite[i]+=btrial[0]*cc;
                    resid[i]=ydata[i]-sovite[i];
                    resid2[i]=resid[i]*resid[i];
                 }

  /* *****    sort_sample(resid2,(int)n);     ***** */
 /*  Rprintf("\norder_statistics=%g  h=%d",order_statistics,h);getch(); */
              ordstat(resid2,(int)n,h);
 /*  Rprintf("\norder_statistics=%g",order_statistics);getch(); */
          /* residuaalien2:h vertailu   */
              ++otos_nro;
 //        Rprintf("\nstat=%g lms=%g",order_statistics,lms_kriteeri);
              if(otos_nro==1 || order_statistics<lms_kriteeri)
                {
                for(i=0;i<m;++i)
                   {
                   blms[i]=btrial[i];
                   }
                   lms_kriteeri=order_statistics;
                }
         //   Rprintf("\n        trial=%d      trial lms=%g",
         //           otos_nro,lms_kriteeri);
              }
            else
               {++disc;}

/*          ++otos_nro;      */

            if(sur_kbhit())       /*   pysäytys ja keskeytys    */
              {
              i=sur_getch();
              if (i=='.') return(1);
              if (i==' ') sur_getch();
              }
           }
/* looppi loppuu tähän */

          return(1);
        }


static int constant_term()
             {
             if (cc==1)
                {
                int i,j,g;
                double vali,pienin_vali;
                for (i=0;i<(int)n;++i)
                    {
                    sovite[i]=0.0;
                    for (j=1;j<m;++j)
                        {
                        sovite[i]+=xdata[i*nxvar+j-1]*blms[j];
                        }
                    resid[i]=ydata[i]-sovite[i];
                    }

                 sort_sample(resid,(int)n);

                 pienin_vali=resid[n-1]-resid[0];
                 if(n%2==0)
                 g=n/2;
                 else g=n/2+1;

                 for(i=0;i<g;++i)
                    {
                    vali=resid[n/2+i]-resid[i];
                    if(vali<pienin_vali)
                      {
                      pienin_vali=vali;
                      blms[0]=resid[i]+vali/2;
                      }
                    }
                }
               return(1);
             }
static int residuals()
         {
         int i,j;
         int k;
         int std_residual,resvar,predvar;
         double med_resid2;
         for(i=0;i<(int)n;++i)
            {
            sovite[i]=0.0;
            for(j=cc;j<m;++j)
               {
               sovite[i]+=xdata[i*nxvar+j-cc]*blms[j];
               }
               sovite[i]+=blms[0]*cc;
               resid[i]=ydata[i]-sovite[i];
               resid2[i]=resid[i]*resid[i];
             }

         sort_sample(resid2,(int)n);
         if(n%2==0)
           med_resid2=(resid2[n/2-1]+resid2[n/2])/2.0;
           else med_resid2=resid2[n/2];

         scale_estimate=1.4826*(1.0+5.0/(n-m))*sqrt(med_resid2);

         std_residual=activated(&d,'T');
         resvar=activated(&d,'U');
         predvar=activated(&d,'V');
         if (std_residual<0 && resvar<0 && predvar<0) return(1);

         sur_print("\nSaving ");
         if (resvar>=0)
            {sprintf(sbuf,"\nlms-residuals as variable %.8s...",
            d.varname[resvar]);
            sur_print(sbuf);
            }
         if (std_residual>=0)
            {sprintf(sbuf,"\nstandardized lms-residuals as variable %.8s...",
            d.varname[std_residual]);
            sur_print(sbuf);
            }
         if (predvar>=0)
            {sprintf(sbuf,"\nlms-predicted values of model as variable %.8s...",
            d.varname[predvar]);
            sur_print(sbuf);
            }


         i=data_open(word[1],&d); if (i<0) return(-1);

         for(k=0;k<n;++k)
            {
            if (std_residual>=0)
               {
               std_resid[k]=resid[k]/scale_estimate;
               data_save(&d,k+1,std_residual,std_resid[k]);
               if (i<0) return(-1);
               }
            if (resvar>=0)
               {
               data_save(&d,k+1,resvar,resid[k]);
               if (i<0) return(-1);
               }
            if (predvar>=0)
               {
               data_save(&d,k+1,predvar,sovite[k]);
               if (i<0) return(-1);
               }
            }
         return(1);
         }


static int printout()
        {
        int i;
        char line[LLENGTH];
        char coefficient[32];
        output_open(eout);
        sprintf(line,"Least Median of Squares Regression: Data %s   N=%d",
                      word[1],n);
        print_line(line);
        sprintf(line,"Regressand is %-8.8s",d.varname[yvar]);
        print_line(line);
        strcpy(line,"Variable      Regr.coeff.");
        print_line(line);
        if (cc==1)
                {
                fnconv(blms[0],accuracy+2,coefficient);
                sprintf(line,"Constant      %s",coefficient);
                print_line(line);
                }
        for(i=0;i<nxvar;++i)
           {
           fnconv(blms[i+cc],accuracy+2,coefficient);
           sprintf(line,"%-8.8s      %s",d.varname[xvar[i]],coefficient);
           print_line(line);
           }
        fnconv(scale_estimate,accuracy+2,coefficient);
        sprintf(line,"Scale estimate=%-8.8s",coefficient);
        print_line(line);
        strcpy(line," ");
        print_line(line);
        sprintf(line,"%d samples were used",otos_nro);
        print_line(line);
        sprintf(line,"%d samples were discarded due to singularity",disc);
        print_line(line);
        output_close(eout);
        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }


static int sort_sample(double *x_space,int n_total)
        {
        unsigned int h,k;
        int ind;
        double y;
  //    float a;

        h=n_total;
        while (h>1)
            {
            h/=2;
            while (1)
                {
                ind=1;
                for (k=0; k<n_total-h; ++k)
                    {
                    if (x_space[k]>x_space[k+h])
                        {
                        y=x_space[k]; x_space[k]=x_space[k+h]; x_space[k+h]=y;
                        ind=0;
                        }
                    }
                if (ind==1) break;
                }
            }
        return(1);
        }

/* ordstat  järjestystunnusluvun laskeminen 12.5.93(21.6.93)/RS  */

#define BIG 1.0e30
#define eps0 1.0e-10  /*  1/(2.72^10)=0.00004511396099   */

static int ordstat(double *x_space,int n_total,int order)
{
int nm,na=0,j,loops;
extern double order_statistics;
double xxx,xp1,xp2,xm1,xm2,ap,am,aa,a;
a=x_space[0];
am=0.0;        /* koska järjestettävät luvut ovat ei-negat. */
ap=BIG;
loops=1;      /* kierrosten lkm */
for(;;)
      {
      nm=0;
      xm1=xm2=0;  /*     -"-        */
      xp1=xp2=BIG;
      for (j=0;j<n_total;++j)
          {
          xxx=x_space[j];
          if (xxx > a )
             {
             if (xxx < xp1)    /* xp1=a:ta lähinnä suurin luku  */
                {
                xp2=xp1;xp1=xxx;
                }
             else if (xxx < xp2) xp2=xxx;
             }
          else if (xxx <= a)
                  {
                  ++nm;                     /* niiden lukujen lkm jotka <=a) */
                  if (xxx > xm1)    /* xm1=a:ta lähinnä pienin luku */
                     {
                     xm2=xm1;xm1=xxx;
                     }
                  else if(xxx > xm2) xm2=xxx; /* xm2=a:ta lähinnä 2. pienin luku */
                  if (xxx==a) ++na;
                  }
          }
      if (order-nm > 2)
         {
/*       Rprintf("\na=%g",a);     */
         am=a;
         aa=loops>9 ? xp1: xp1+(order-nm-1)*(xp2-xp1);  /* kasvatetaan a:ta */
         if (aa > ap) aa=0.5*(a+ap);
         a=aa;
/*       Rprintf("\nliian pieni: ");printf("  uusi a=%g",a);  */
         }
      else if (order-nm == 2)
              {
              order_statistics=xp2;
/*            Rprintf("\n\ngot it (=xp2)! o_statistics=%g",order_statistics);  */
              return(1);
              }
      else if (nm-order > 1)
              {
              if (nm-na <= order)
                 {
                 order_statistics=a;
/*               Rprintf("\ngot it (=a)! o_statistics=%g",order_statistics);  */
                 return(1);
                 }
              ap=a;
   /* pienennetään a:ta  */

              aa=loops>9 ? xm1-eps0: xm1-eps0-(nm-order)*(xm1-xm2);
              if (aa < am) aa=0.5*(a+am);
              a=aa;
/*            Rprintf("\nliian suuri: "); Rprintf("  uusi a=%g",a);  */
              }
      else if (nm-order==1)
              {
              order_statistics=xm2;
/*            Rprintf("\n\ngot it (=xm2)! o_statistics=%g",order_statistics);   */
              return(1);
              }
      else if (order-nm==1)
              {
              order_statistics=xp1;
/*            Rprintf("\ngot it (=xp1)! o_statistics=%g",order_statistics);   */
              return(1);
              }
           else
              {
              order_statistics=xm1;
/*            Rprintf("\n\ngot it (=xm1)! o_statistics=%g",order_statistics);  */
              return(1);
              }
      ++loops;
      }
}


/* ortho.c 25.10.1986/SM (8.3.1992)(19.11.92)(29.4.93/RS)
*/

extern double sis_tulo();

static int ortho(double *a,int n,int m,double *b,int k,double eps,double *x,int improvement)
        {
        int g,h,i,j,l,ll,mm;
        double s,t;
        extern int di;
/*      Rprintf("\n 1.di=%d",di);      */
/* Rprintf("a");    */
  if (ortho_malloc==0) // allocation only once 7.10.2011/SM
        {
        ou=(double *)muste_malloc(m*n*sizeof(double));
        if (ou==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<m*n; ++i) ou[i]=a[i];
        op=(double *)muste_malloc(n*sizeof(double));
        if (op==NULL) { not_enough_memory(); return(-1); }
        opx=(double *)muste_malloc(m*sizeof(double));
        if (opx==NULL) { not_enough_memory(); return(-1); }
        oq=(double *)muste_malloc(m*(m+1)/2*sizeof(double));
        if (oq==NULL) { not_enough_memory(); return(-1); }
        ortho_malloc=1;
        }
  else
        {
        for (i=0; i<m*n; ++i) ou[i]=a[i];
        }


/* Rprintf("b");    */
        l=-1;
        for (i=0; i<m; ++i)
            {
            s=0.0;
            for (j=0; j<n; ++j)
                {
                op[j]=ou[j+n*i]; t=op[j]; s+=t*t;
                }
/*          if (s<eps) { lin_dep(i); return(-1); }   */
            if (s<eps) {di=1;return(1);}
/*          Rprintf("\n 3.di=%d",di);        */
            oq[++l]=s;
            for (g=0; g<k; ++g)
                {
                t=0.0;
                for (j=0; j<n; ++j)
                    t+=op[j]*b[j+n*g];
                x[i+m*g]=t;
                }
            for (g=i+1; g<m; ++g)
                {
                t=0.0;
                for (j=0; j<n; ++j)
                    t+=op[j]*ou[j+n*g];
                oq[++l]=t; t/=s;
                for (j=0; j<n; ++j)
                    ou[j+n*g]-=op[j]*t;
                }
            }
        ll=l; mm=m+2;
        for (i=m-1; i>=0; --i)
            {
            h=l-i; t=oq[l];
            for (j=0; j<k; ++j)
                {
                s=x[i+m*j];
                for (g=i+1; g<m; ++g)
                    s-=oq[g+h]*x[g+m*j];
                x[i+m*j]=s=s/t;
                if (j==0) opx[i]=s;
                }
            l+=i+1-mm;
            }
//      muste_free(oq); muste_free(opx); muste_free(op); muste_free(ou);
        return(1);
        }

