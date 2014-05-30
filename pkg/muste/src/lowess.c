#include "muste.h"
/* locally weighted linear regression smooth  */
/* Juha Puranen  (18.2.1991)                 */
/* Converted to 32bit SURVO 98 by K.Vehkalahti 15.2.1998 */
/* texts in English to add LOWESS to base-version (98) / kv 13.8.1998 */
/* Converted to Win32 SURVO MM by K.Vehkalahti 16.8.2000 */
/* Converted to Muste by KV 17.12.2011 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static int check_mask ( void );
static int lue ( void );
static void not_enough_memory ( void );
static void sort ( int p [], double a [], int n );
static double rank_order ( int p [], double a [], int n , int k );
static double paino2 ( double a );
static double paino3 ( double a );
static void summaus ( double x , double y , double w , double *sum );
static void init ( int ip [], int n );
static void f_nearest ( int p [], double a [], int l , int *m1 , int *m2 , int n , int fn );
static double maksimi ( int p [], double *a , int l , int l1 , int l2 );
static void smooth ( double xx [], double yy [], double ye [], double ee [], int ip [], int id );
static void talletus ( void );
static int talleta2 ( void );
static void story ( void );

static double *sxx,*sxy,*sy,*sx,*sw;
static double sum[5];
static double ff;
static int nn,n1,f;
static double w,w1,w2,med;
static int x_variable,y_variable;
static int s_variable,e_variable;
static char aineisto[121];
static double *xx,*yy,*ye,*ee;
static int *ip,*ir;
static int *ihav;
static char *mat_file="SMOOTH.M";
static char *rlab,*clab;
static int out_mat=0;
static SURVO_DATA d;
static int prind;

// 4.6.2004:
/**************************************
char *spec_lowess[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                              "PRIND", "ITERATIONS", "!" };

char **specs=spec_lowess;
***************************************/

void muste_lowess(char *argv)
        {
        int i,iter;
 
// RS Variable init
sxx=NULL;
sxy=NULL;
sy=NULL;
sx=NULL;
sw=NULL;
// static double sum[5];
ff=0;
nn=n1=f=0;
w=w1=w2=med=0;
x_variable=y_variable=0;
s_variable=e_variable=0;
// static char aineisto[121];
xx=NULL;
yy=NULL;
ye=NULL;
ee=NULL;
ip=NULL;
ir=NULL;
ihav=NULL;
// static char *mat_file="SMOOTH.M";
rlab=NULL;
clab=NULL;
out_mat=0;
// static SURVO_DATA d;
                
        s_init(argv);
        if (g<2 || strchr(word[1],'?'))
           {
           story();
           return;
           }
        ff=60.;
        if(g>2) ff=atof(word[2]);
        if(ff>1.00) ff/=100.;
        if(g>3)
           {
           mat_file = word[3];
           out_mat=1;
           }
        strcpy(aineisto,word[1]);
        i=data_open(aineisto,&d); if (i<0) return;
        i=spec_init(r1+r-1); if (i<0) return;
        prind=0;
        i=spfind("PRIND");
        if(i>=0) prind=atoi(spb[i]);
        i=mask(&d); if (i<0) return;
        scales(&d);
        if (d.m_act==0)
            {
            sur_print("\nNo active variables!");
            WAIT; return;
            }
        if((i=check_mask())==-1)return;
        i=conditions(&d); if (i<0) return;  /* permitted only once */
        i=lue(); if (i<0) return;

        /* mallirivi analysoitu  ...................... */
        sxx=sum;sxy=sum+1;sx=sum+2;sy=sum+3;sw=sum+4;
        iter=2;
        i=spfind("ITERATIONS");
        if(i>=0)iter=atoi(spb[i]);
        if(iter<1) iter=1;
        if(iter>2) iter=2;
 /*  data  luettu muistiin */

        if(prind) sur_print("\nSorting data... ");
        sort(ip,xx,n1);     /* lajitellaan aineisto  x suhteen  */
        med=0.0;

        if(prind) sur_print("\n Phase 1 ");
/* ensimmäinen tasoitus ..............................................*/
        smooth(xx,yy,ye,ee,ip,0);
if (iter==2){
        if(prind) sur_print("\n Phase 2 ");
        init(ir,n1);
        med=rank_order(ir,ee,n1,(n1+1)/2);
        sprintf(sbuf," median = %f n1=%d nn= %d \n",med,n1,nn);
        if(prind) sur_print(sbuf);
/* toinen  tasoitus ..................................................*/
        smooth(xx,yy,ye,ee,ip,1);
 } /* toinen tasoitus loppuu */
        if(prind) sur_print("\n Phase 3 ");
        talletus();
        if(out_mat)
               if((i=talleta2())==-1){
                    sur_print("\nMatrix saving failed!");
                    WAIT;
                    }
        s_end(argv);
        return;
        }


/*                                                         */
/* aliohjelmia    ..................................       */
/*                                                         */


static int check_mask()
        {
        x_variable=activated(&d,'X');
        if (x_variable<0)
            {
            sur_print("\nNo X-variable!");
            WAIT; return(-1);
            }
        y_variable=activated(&d,'Y');
        if (y_variable<0)
            {
            sur_print("\nNo Y-variable!");
            WAIT; return(-1);
            }
        s_variable=activated(&d,'S');
        if (s_variable<0) out_mat=1;
        e_variable=activated(&d,'E');

        muste_free(xx);muste_free(yy);muste_free(ye);muste_free(ee);
        muste_free(ip);muste_free(ir);muste_free(ihav);
        return(1);
       }
static int lue()
   {
   int l,nkpl;
   unsigned int koko1,koko2,koko3;
   double xarvo,yarvo;
   nkpl=d.l2-d.l1+1L;
   nn=nkpl;
   koko1=(unsigned int)nn*sizeof(double);
   koko2=(unsigned int)nn*sizeof(int);
   koko3=(unsigned int)nn*sizeof(int);
   xx=(double *)muste_malloc(koko1);
   if (xx==NULL) { not_enough_memory(); return(-1); }
   yy=(double *)muste_malloc(koko1);
   if (yy==NULL) { not_enough_memory(); return(-1); }
   ye=(double *)muste_malloc(koko1);
   if (ye==NULL) { not_enough_memory(); return(-1); }
   ee=(double *)muste_malloc(koko1);
   if (ee==NULL) { not_enough_memory(); return(-1); }
   ip=(int *)muste_malloc(koko2);
   if (ip==NULL) { not_enough_memory(); return(-1); }
   ir=(int *)muste_malloc(koko2);
   if (ir==NULL) { not_enough_memory(); return(-1); }
   ihav=(int *)muste_malloc(koko3);
   if (ihav==NULL) { not_enough_memory(); return(-1); }
   if(prind) sur_print("\n Reading data... \n");
   for(l=0;l<nn;l++)
      {
      xx[l]=MISSING8;
      yy[l]=MISSING8;
      }
  n1=0;
  for (l=d.l1; l<=d.l2; ++l)
     {
     if (unsuitable(&d,l)) continue;
     if (prind) { sprintf(sbuf," %d",l); sur_print(sbuf); }
     data_load(&d,l,x_variable,&xarvo);
     data_load(&d,l,y_variable,&yarvo);
     if (xarvo==MISSING8 || yarvo==MISSING8) continue;
     xx[n1]=xarvo;
     yy[n1]=yarvo;
     ip[n1]=n1;
     ihav[n1]=l;
     n1++;
     }
  if(n1==0) {
      sprintf(sbuf, "\nNo suitable observations!");
      sur_print(sbuf); WAIT;
      return(-1);
  }
  f=(int)(ff*(double)n1);
  return 1;
  }
static void not_enough_memory()
      {
      sur_print("\nNot enough memory! (LOWESS)");
      WAIT;
      }
static void sort(int p[],double a[], int n)
     {
     int i,j,gap,nextj;
     int temp,inda,indb;
     gap=n;
     while (1)
          {
          gap/=2;
          if (gap==0) return;
          if(prind){
          sprintf(sbuf,"\n gap = %d",gap);
          sur_print(sbuf);
          }
          for (i=0;i<n-gap;i++)
               {
               j=i;
               while (j>=0)
                  {
                   inda=p[j];
                   nextj=j+gap;
                   indb=p[nextj];
                   if (a[inda]>a[indb])
                      {
                      temp=p[j];p[j]=p[nextj];p[nextj]=temp;
                      }
                   else j=0;
                   j=j-gap;
                   }
               }
         }
    }
static double rank_order(int p[],double a[],int n,int k)
        {
        int indeksi;
        if(prind) sur_print("\nSorting data... ");
        sort(p,a,n);
        indeksi=p[k];
        return a[indeksi];
        }
static double paino2(double a)   /* jasketaan bisquar painokerroin */
        {
        double b,c ;
        b = a<0  ? -a:a ;
        if (b >1) return (0.0);
        c=b*b ;
        b=1-c ;
        c=b*b ;
        return (c);
        }
static double paino3(double a)   /*  lasketaan tricube  painokerroin  */
        {
        double b,c ;
        b=(a<0) ? -a:a ;
        if (b >1) return (0.0);
        c=b*b*b ;
        b=1-c ;
        c=b*b*b ;
        return (c);
        }
static void summaus(double x,double y,double w, double *sum)  /* kerataan  tulosummat */
        {
        *sum+=w*x*x;
        *(sum+1)+=w*x*y;
        *(sum+2)+=w*x;
        *(sum+3)+=w*y;
        *(sum+4)+=w;
        return;
        }
static void init(int ip[],int n)  /* alkuarvot vektorille ip */
        {
        int i=0;
        while (i<n)
              {
              ip[i]=i;i++;
              }
        return;
        }
static void f_nearest(int p[],double a[],int l,int *m1,int *m2,int n,int fn)
        {
        int i,l1,l2;
        l1=l2=l;
        for (i=0; i<fn-1;i++)
              {
              if (l1<=0) l2+=1;
              else if (l2>=n-1) l1-=1;
              else if (*(a+p[l])-*(a+p[l1-1])>=*(a+p[l2+1])-*(a+p[l])) l2+=1;
              else l1-=1;
              }
        *m1 = l1;
        *m2 = l2;
        return;
        }
static double maksimi(int p[],double *a,int l,int l1,int l2)
        {
        double max1,temp1,temp2;
        temp1 = *(a+p[l])-*(a+p[l1]);
        temp2 = *(a+p[l2])-*(a+p[l]);
        max1 = temp1 > temp2 ? temp1 : temp2;
        return (max1);
        }
/* tasoitus ...............................................*/
static void smooth(double xx[],double yy[],double ye[],double ee[],int ip[],int id)
        {
        double x,y,yee,e,b,max1;
        double apu,ssx;
        double paino2();
        double paino3();
        int ii,i,l1,l2,idp,irp;
        for (ii=0; ii <n1; ii++) /* yli kaikkien havaintojen */
                {
                irp=ip[ii];
                sum[0]=sum[1]=sum[2]=sum[3]=sum[4]=0.0;
                if (prind) { sprintf(sbuf," %d",ii+1); sur_print(sbuf); }
                f_nearest(ip,xx,ii,&l1,&l2,n1,f);  /* f lahinta */
                max1=maksimi(ip,xx,ii,l1,l2);
                for( i=l1; i<=l2;i++)          /* osa-alueen yli */
                        {
                        idp=ip[i];
                        if(max1!=0.0)
                                {
                                x =(*(xx+idp)-*(xx+irp))/max1;
                                w1= paino3(x);
                                }
                        else w1 = 1.0;
                        e = 0.0;
                        if(id)
                                {
                                e = *(ee+idp);
                                if(med==0.0)
                                     {
                                     if(e==0.0) w2=1.0;
                                     else w2=0.0;
                                     }
                                else
                                     {
                                     x= e/(6*med);
                                     if (x > 1) w2 = 0.0;
                                     else w2=paino2(x);
                                     }
                                }
                        else w2=1.0;
                        x = *(xx+idp);
                        y = *(yy+idp);
                        w=w1*w2;
                        summaus(x,y,w,sum);
                        }
                yee = *(yy+irp);
                b = 0.0;
                if(*sw != 0.0)
                        {
                        apu=(*sx)*(*sx)/(*sw);
                        ssx=*sxx-apu;
                        if (ssx != 0.0)
                               {
                               b=(*sxy-(*sx)*(*sy)/(*sw))/ssx;
                               yee = b*(*(xx+irp)-(*sx)/(*sw))+(*sy)/(*sw);
                               }
                        else yee = (*sy)/(*sw);
                        }
                *(ye+irp) = yee;
                }
        for (ii=0; ii <n1; ii++) /* yli kaikkien havaintojen */
                {
                e = *(ye+ii) - *(yy+ii);
                if (id==0) e = (e > 0 ? e : -e);
                *(ee+ii) = e ;
                }
        return;
        }
static void talletus()
   {
   int i;
   int l,kl;
   if (s_variable<0 && e_variable<0 ) return; /* nothing to save!! */
   if(prind) sur_print("\nSaving values of output variables\n");
   i=d.l1;
   for (l=0;l<n1;l++,i+=1)
      {
      kl = l;
      if (prind) { sprintf(sbuf," %d",ihav[kl]+1); sur_print(sbuf); }
      if(s_variable>=0)
            {
            data_save(&d,ihav[kl],s_variable,ye[kl]);
            }
      if(e_variable>=0)
            {
            data_save(&d,ihav[kl],e_variable,ee[kl]);
            }
       }
    }
static int talleta2()
  {
  unsigned int koko,tila;
  int i,idp;
  double *A,*ar,*br,*cr;
  char *rl;
  char *cp;
  char mnimi[128];
  tila=(unsigned)n1*8+1;
  rlab=muste_malloc(tila);
  if (rlab==NULL) { not_enough_memory(); return(-1); }
  tila=25;
  clab=muste_malloc(tila);
  if (clab==NULL) { not_enough_memory(); return(-1); }

  if(prind) {
    sprintf(sbuf,"\nSaving variables to matrix file %s\n",mat_file);
    sur_print(sbuf);
  }
  koko=(unsigned)n1*sizeof(double);
  A=(double *)muste_malloc(3*koko);
  if (A==NULL) { not_enough_memory(); return(-1); }
  ar=A;
  br=ar+n1;
  cr=br+n1;
     for(i=0;i<n1;i++,ar++,br++,cr++)
         {
         idp=ip[i];
         *ar=*(xx+idp);
         *br=*(yy+idp);
         *cr=*(ye+idp);
         }

    rl = rlab;
    for (i=0;i<n1;i++) {
        sprintf(rl,"%8d",i+1);
        rl+=8;
        if (prind) { sprintf(sbuf," %d",i+1);sur_print(sbuf);}
    }


  strcpy(clab,"X       Y       S       ");
  sprintf(mnimi,"DATA %s; n=%d; X-var=%s; Y-var=%s",
      word[1], n1, d.varname[x_variable], d.varname[y_variable]);
  for(cp=mnimi;*cp;cp++) if(*cp==' ') *cp='_';
  for (cp--; *cp=='_'; --cp) *cp=' '; cp++; *cp=EOS;
  if(prind) { sur_print("\n"); sur_print(mnimi);
              sur_print("\n"); sur_print(clab); }

  i=matrix_save(mat_file,A,n1,3,rlab,clab,8,8,-1,mnimi,0,0);
  if (i<0) return(-1);
  muste_free(rlab); muste_free(clab);
  muste_free(A);
  return(1);
  }

static void story() { strcpy(sbuf,"\nSee: LOWESS?"); sur_print(sbuf); WAIT; }
