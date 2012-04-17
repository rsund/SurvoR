/* !canon.c 16.2.1987/SM (12.5.1988)
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
// #include <conio.h>
#include <math.h>
// #include <malloc.h>
// #include <process.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

SURVO_DATA dat;

static int n; /* hav. lukum‰‰r‰ */
static int *vxyz,*v,nxyz;
static int m1,m2,m3;
static int results_line;
static int m1lessm2;
static char cx,cy;

static double *A;
static char *rlab,*clab;

static int max_m; // 31.7.2011/SM
static int varaus; // 31.7.2011/SM

static int varaa_tilat();
static int not_enough_memory(int k);
static int tee_vxyz(int *v,char *s);
static int tutki_havainnot();
static int talleta_matriisi(char *s);
static int mat_oper();
static int load_X(char *name);
static int load_Y(char *name);
static int matr_painot(double *S,double *T,double *X,double *Y,int m1,int m2,double nn);
static int zregress();
static int tulostus();
static int tulosta_rivi(char *x);

static int text_labels(char *lab,int n,char *text);
static int text_labels2(char *lab,int n,char *text,int base);
static int matrix_space(double **A,int m,int n,char **rlab,char **clab,int mcr,int mcl);
static int matrix_nospace();


/***************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "RESULTS", "PRIND", "!" };
char **specs=specs0;
*******************/
/*********************
main(argc,argv)
int argc; char *argv[];
***************************/

void muste_canon(char *argv)
        {
        int i;
        char ch;
        char x[LLENGTH];

   //   if (argc==1) return;

        s_init(argv);

        if (g<2)
            {
            sur_print("\nUsage: CANON <data>,L");
                  /* g=3    word[0] word[1] word[2]   */
            WAIT; return;
            }

        varaus=1; // 31.7.2011/SM

        i=sp_init(r1+r-1); if (i<0) return;
        i=data_open(word[1],&dat); if (i<0) return;
        i=mask(&dat);
        i=conditions(&dat);
        i=varaa_tilat(); if (i<0) return;

        max_m=m1=tee_vxyz(vxyz,"X");
        m2=tee_vxyz(vxyz,"Y"); if (m2>max_m) max_m=m2;
        m1lessm2=0; cx='X'; cy='Y';
        if (m1<m2)
            {
            m1lessm2=1; cx='Y'; cy='X';
            for (i=0; i<dat.m; ++i)
                {
                ch=dat.vartype[i][1];
                if (ch=='X') dat.vartype[i][1]='Y';
                else if (ch=='Y') dat.vartype[i][1]='X';
                }
            }

        i=tee_vxyz(vxyz,"Z"); if (i>max_m) max_m=i;

        nxyz=tee_vxyz(vxyz,"XYZ"); if (nxyz==0) return;

        i=tutki_havainnot(); if (i<0) return;
        m1=talleta_matriisi("X");
        if (m1<=0)
            {
            sur_print("\nNo X variables!"); WAIT; return;
            }
        m2=talleta_matriisi("Y");
        if (m2<=0)
            {
            sur_print("\nNo Y variables!"); WAIT; return;
            }
        m3=talleta_matriisi("Z"); if (m3<0) return;
        data_close(&dat); muste_free(vxyz);
        muste_free(A); muste_free(rlab); muste_free(clab); /* ei saa k‰ytt‰‰ en‰‰ */
        i=mat_oper(); if (i<0) return;
        tulostus();
        i=spfind("RESULTS"); if (i>=0) results=atoi(spb[i]);
        if (results<100)
            {
            strcpy(x,edisk); strcat(x,"&*.MAT");
            sur_delete(x);
            }
        muste_free(v); // 30.7.2011/SM
        s_end(argv);
        }

static int varaa_tilat()
        {
        vxyz=(int *)muste_malloc(dat.m*sizeof(int));
        if (vxyz==NULL) { not_enough_memory(1); return(-1); }
        v=(int *)muste_malloc(dat.m*sizeof(int));
        if (v==NULL) { not_enough_memory(2); return(-1); }
        return(1);
        }

static int not_enough_memory(int k)
        {
        sprintf(sbuf,"\nNot enough memory! (%d)",k); sur_print(sbuf);
        WAIT; return(1);
        }

static int tee_vxyz(int *v,char *s)
        {
        int i,n;

        n=0;
        for (i=0; i<dat.m; ++i)
           if (strchr(s,dat.vartype[i][1])!=NULL) v[n++]=i;
        return(n);
        }

static int tutki_havainnot()
        {
        int i;
        int j;
        double x;

        sprintf(sbuf,"\nChecking data %s...",word[1]); sur_print(sbuf);
        n=0;
        for (j=dat.l1; j<=dat.l2; ++j)
            {
            if (unsuitable(&dat,j)) continue;
            for (i=0; i<nxyz; ++i)
                {
                data_load(&dat,j,vxyz[i],&x);
                if (x==MISSING8) break;
                }
            if (i==nxyz) ++n;
            }
        if (n==0)
            {
            sprintf(sbuf,"\nNo acceptable observations in %s!",word[1]);
            sur_print(sbuf); WAIT; return(-1);
            }
        return(1);
        }

static int talleta_matriisi(char *s)
        {
        int i,m,irivi,h;
        unsigned int tila;
        int vrivi;
        char label[LLENGTH];
        int j;
        double x;
        char nimi[3];

        *nimi='&'; nimi[1]=*s; nimi[2]=EOS;
        m=tee_vxyz(v,s); if (m==0) return(0);

        sprintf(sbuf,"\nSaving %s variables...",s); sur_print(sbuf);

  if (varaus==1) // 31.7.2011/SM
        {
        tila=max_m*n;
            A=(double *)muste_malloc(tila*sizeof(double));
        if (A==NULL) { not_enough_memory(3); return(-1); }
            rlab=muste_malloc(n*8);
        if (rlab==NULL) { not_enough_memory(4); return(-1); }
            clab=muste_malloc(n*8);
        if (clab==NULL) { not_enough_memory(5); return(-1); }
        }
        vrivi=activated(&dat,'A');

        varaus=0; // 31.7.2011/SM

        if (vrivi>=0)
            if (dat.vartype[vrivi][0]!='S') vrivi=-1;
        irivi=0;
        for (j=dat.l1; j<=dat.l2; ++j)
            {
            if (unsuitable(&dat,j)) continue;
            for (i=0; i<nxyz; ++i)
                {
                data_load(&dat,j,vxyz[i],&x);
                if (x==MISSING8) break;
                }
            if (i<nxyz) continue;  /* lis‰tty 12.5.88 */
            for (i=0; i<m; ++i)
                data_load(&dat,j,v[i],&A[irivi+n*i]);

            for (h=0; h<8; ++h) rlab[8*irivi+h]=' ';
            if (vrivi>=0)
                data_alpha_load(&dat,j,vrivi,label);
            else
                sprintf(label,"%d",irivi+1);
            for (h=0; h<8; ++h)
                 {
                 if (label[h]==EOS) break;
                 rlab[8*irivi+h]=label[h];
                 }
            ++irivi;
            }
        for (i=0; i<m; ++i)
            {
            for (h=0; h<8; ++h) clab[8*i+h]=' ';
            for (h=0; h<8; ++h)
                 {
                 if (dat.varname[v[i]][h]==EOS) break;
                 clab[8*i+h]=dat.varname[v[i]][h];
                 }
            }

        i=matrix_save(nimi,A,n,m,rlab,clab,8,8,-1,s,0,0);
        if (i<0) return(-1);
        return(m);
        }

/*  canonm.c 23.3.1987/SM (27.3.1987)
*/

static double *X;
static char *rlabX, *clabX, exprX[LLENGTH];
static int  mX, nX, typeX, lrX, lcX;
static double *Y;
static char *rlabY, *clabY, exprY[LLENGTH];
static int  mY, nY, typeY, lrY, lcY;
static double *T;
static char *rlabT;
// static int  mT, nT, typeT, lrT, lcT;


static double *S;
static char *rlabS, *clabS;

static char *xlab,*ylab;


static int mat_oper()
        {
        int i;
        char mnimi[16];


        if (m3) { i=zregress(); if (i<0) return(-1); }
        sur_print("\nMeans, stddevs and correlations of X variables...");
        i=load_X("&X"); if (i<0) return(-1);
        xlab=muste_malloc(lcX*nX); if (xlab==NULL) { not_enough_memory(6); return(-1); }
        for (i=0; i<lcX*nX; ++i) xlab[i]=clabX[i];
        i=matrix_space(&T,nX,1,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_center(T,X,mX,nX);
        i=matrix_save("&MX",T,nX,1,clabX,"mean    ",lcX,lcX,-1,"mean",0,0);
        i=mat_nrm(T,X,mX,nX);
        i=matrix_save("&SX",T,nX,1,clabX,"norm    ",lcX,lcX,-1,"norm",0,0);
        i=matrix_space(&T,nX,nX,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_mtm(T,X,mX,nX);
        i=matrix_save("&RX",T,nX,nX,clabX,clabX,lcX,lcX,-1,"RX",0,0);
        sur_print("\nOrthogonalization of X variables to QX,UX...");
        i=matrix_space(&T,nX,nX,NULL,NULL,8,8); if (i<0) return(-1);
        i=matrix_space(&Y,mX,nX,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_gram_schmidt(Y,T,X,mX,nX,1e-15);
        if (i<=0)
            {
            sprintf(sbuf,"\nX variable %.8s linearly dependent on the previous ones!",
                        rlabX-i*lrX);
            sur_print(sbuf); WAIT; return(-1);
            }
        i=matrix_save("&QX",Y,mX,nX,rlabX,clabX,lrX,lcX,-1,"QX",0,0);
        i=matrix_save("&UX",T,nX,nX,clabX,clabX,lcX,lcX,-1,"UX",0,0);
        sur_print("\nMeans, stddevs and correlations of Y variables...");
        i=load_X("&Y"); if (i<0) return(-1);
        ylab=muste_malloc(lcX*nX); if (ylab==NULL) { not_enough_memory(7); return(-1); }
        for (i=0; i<lcX*nX; ++i) ylab[i]=clabX[i];
        i=matrix_space(&T,nX,1,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_center(T,X,mX,nX);
        i=matrix_save("&MY",T,nX,1,clabX,"mean    ",lcX,lcX,-1,"mean",0,0);
        i=mat_nrm(T,X,mX,nX);
        i=matrix_save("&SY",T,nX,1,clabX,"norm    ",lcX,lcX,-1,"norm",0,0);
        i=matrix_space(&T,nX,nX,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_mtm(T,X,mX,nX);
        i=matrix_save("&RY",T,nX,nX,clabX,clabX,lcX,lcX,-1,"RY",0,0);
        sur_print("\nOrthogonalization of Y variables to QY,UY...");
        i=matrix_space(&T,nX,nX,NULL,NULL,8,8); if (i<0) return(-1);
        i=matrix_space(&Y,mX,nX,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_gram_schmidt(Y,T,X,mX,nX,1e-15);
        if (i<=0)
            {
            sprintf(sbuf,"\nY variable %.8s linearly dependent on the previous ones!",
                        rlabX-i*lrX);
            sur_print(sbuf); WAIT; return(-1);
            }
        i=matrix_save("&QY",Y,mX,nX,rlabX,clabX,lrX,lcX,-1,"QY",0,0);
        i=matrix_save("&UY",T,nX,nX,clabX,clabX,lcX,lcX,-1,"UY",0,0);
        sur_print("\nComputing W=QX'*QY...");
        i=load_X("&QX"); if (i<0) return(-1);
        i=matrix_space(&T,m1,m2,NULL,NULL,8,8); if (i<0) return(-1);
        mat_mtm2(T,X,Y,n,m1,m2);
        i=matrix_save("&W",T,m1,m2,xlab,ylab,8,8,-1,"W",0,0);

        i=matrix_space(&X,m1,m2,NULL,NULL,8,8); if (i<0) return(-1);
        mat_copy(X,T,m1,m2);
        i=matrix_space(&Y,m2,m2,NULL,NULL,8,8); if (i<0) return(-1);
        i=matrix_space(&T,m2,1,&rlabT,NULL,8,8); if (i<0) return(-1);
        sur_print("\nSingular value decomposition of W...");
        mat_svd(X,T,Y,m1,m2,1e-16,1e-300);
        text_labels(rlabT,m2,"CAN");
        i=matrix_save("LCAN.M",T,m2,1,rlabT,"can.corr",8,8,-1,"can.corr",0,0);

        i=matrix_save("&U",X,m1,m2,xlab,rlabT,8,8,-1,"U",0,0);
        i=matrix_save("&V",Y,m2,m2,ylab,rlabT,8,8,-1,"V",0,0);
        sur_print("\nSolving canonical coefficients for X...");
        load_Y("&UX");
        i=matrix_space(&T,m1,m2,NULL,NULL,8,8); if (i<0) return(-1);
        i=solve_upper(T,Y,X,m1,m2,1e-15);
        i=matrix_space(&S,m1+1,m2,&rlabS,&clabS,8,8); if (i<0) return(-1);
        strncpy(rlabS,"Constant",8);
        for (i=0; i<8*m1; ++i) rlabS[i+8]=xlab[i];
        text_labels(clabS,m2,"%");
        load_X("&MX");
        load_Y("&SX");
        matr_painot(S,T,X,Y,m1,m2,sqrt((double)(n-1)));
        strcpy(mnimi,"XCOEFF.M"); *mnimi=cx;
        i=matrix_save(mnimi,S,m1+1,m2,rlabS,clabS,8,8,-1,"can.coeff",0,0);
        load_Y("&RX");
        i=matrix_space(&X,m1,m2,NULL,NULL,8,8); if (i<0) return(-1);
        mat_mlt(X,Y,T,m1,m1,m2);
        strcpy(mnimi,"XCAN.M"); *mnimi=cx;
        i=matrix_save(mnimi,X,m1,m2,xlab,rlabT,8,8,-1,"can.corr",0,0);
        sur_print("\nSolving canonical coefficients for Y...");
        load_Y("&UY");
        load_X("&V");
        i=matrix_space(&T,m2,m2,NULL,NULL,8,8); if (i<0) return(-1);
        i=solve_upper(T,Y,X,m2,m2,1e-15);
        i=matrix_space(&S,m2+1,m2,&rlabS,&clabS,8,8); if (i<0) return(-1);
        strncpy(rlabS,"Constant",8);
        for (i=0; i<8*m2; ++i) rlabS[i+8]=ylab[i];
        text_labels(clabS,m2,"%");
        load_X("&MY");
        load_Y("&SY");
        matr_painot(S,T,X,Y,m2,m2,sqrt((double)(n-1)));
        strcpy(mnimi,"YCOEFF.M"); *mnimi=cy;
        i=matrix_save(mnimi,S,m2+1,m2,rlabS,clabS,8,8,-1,"can.coeff",0,0);
        load_Y("&RY");
        i=matrix_space(&X,m2,m2,NULL,NULL,8,8); if (i<0) return(-1);
        mat_mlt(X,Y,T,m2,m2,m2);
        strcpy(mnimi,"YCAN.M"); *mnimi=cy;
        i=matrix_save(mnimi,X,m2,m2,ylab,rlabT,8,8,-1,"can.corr",0,0);
        return(1);
        }

static int load_X(char *name)
        {
        int i;

        i=matrix_load(name,&X,&mX,&nX,&rlabX,&clabX,&lrX,&lcX,&typeX,exprX);
        return(i);
        }

static int load_Y(char *name)
        {
        int i;

        i=matrix_load(name,&Y,&mY,&nY,&rlabY,&clabY,&lrY,&lcY,&typeY,exprY);
        return(i);
        }
/****************************
static int load_T(char *name)
        {
        int i;

        i=matrix_load(name,&T,&mT,&nT,&rlabT,&clabT,&lrT,&lcT,&typeT,exprT);
        return(i);
        }
*****************************/

static int matr_painot(double *S,double *T,double *X,double *Y,int m1,int m2,double nn)
        {
        int i,j;
        double u;

        for (j=0; j<m2; ++j)
            {
            u=0.0;
            for (i=0; i<m1; ++i)
                {
                S[i+1+(m1+1)*j]=nn*T[i+m1*j]/Y[i];
                u-=T[i+m1*j]*X[i]/Y[i];
                }
            S[0+(m1+1)*j]=nn*u;
            }
        return(1);
        }

static int zregress()
        {
        int i;

        sur_print("\nEliminating effects of confounding variables Z...");
        i=load_X("&Z"); if (i<0) return(-1);
        i=matrix_space(&T,nX,1,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_center(T,X,mX,nX);
        i=matrix_space(&Y,mX,nX,NULL,NULL,8,8); if (i<0) return(-1);
        i=matrix_space(&T,nX,nX,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_gram_schmidt(Y,T,X,mX,nX,1e-15);

        if (i<=0)
            {
            sprintf(sbuf,"\nZ variable %.8s linearly dependent on the previous ones!",
                        rlabX-i*lrX);
            sur_print(sbuf); WAIT; return(-1);
            }
        i=matrix_save("&Z",Y,n,m3,rlabX,clabX,lrX,lcX,-1,"QZ",0,0);

        i=load_X("&X"); if (i<0) return(-1);
        i=matrix_space(&T,m1,1,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_center(T,X,n,m1);
        i=matrix_space(&T,m3,m1,NULL,NULL,8,8); if (i<0) return(-1);
        mat_mtm2(T,Y,X,n,m3,m1);
        mat_mlt(X,Y,T,n,m3,m1);
        load_Y("&X");
        i=matrix_space(&T,n,m1,NULL,NULL,8,8); if (i<0) return(-1);
        mat_sub(T,Y,X,n,m1);
        i=matrix_save("&X",T,n,m1,rlabX,clabX,lrX,lcX,-1,"X(Z)",0,0);

        i=load_Y("&Z"); if (i<0) return(-1);
        i=load_X("&Y"); if (i<0) return(-1);
        i=matrix_space(&T,m2,1,NULL,NULL,8,8); if (i<0) return(-1);
        i=mat_center(T,X,n,m2);
        i=matrix_space(&T,m3,m2,NULL,NULL,8,8); if (i<0) return(-1);
        mat_mtm2(T,Y,X,n,m3,m2);
        mat_mlt(X,Y,T,n,m3,m2);
        load_Y("&Y");
        i=matrix_space(&T,n,m2,NULL,NULL,8,8); if (i<0) return(-1);
        mat_sub(T,Y,X,n,m2);
        i=matrix_save("&Y",T,n,m2,rlabX,clabX,lrX,lcX,-1,"Y(Z)",0,0);
        return(1);
        }

/* canon2.c 23.2.1987/SM (30.11.1987)
*/
static double *A0;
static char *rlab0,*clab0;

static int tulostus()
        {
        int i,k;
        int m,nn;
        int lr,lc;
        int type;
        char expr[129];
        char x[LLENGTH];
   //   char *p;
   //   char *osa[4];
        int mr,df;
        double q,a;
        char tulos1[32],tulos2[32],tulos3[32];
        int ac;

        results_line=0;
        if (g>2)
            {
            results_line=edline2(word[2],1,1);
            if (results_line==0) return(-1);
            }

        i=output_open(eout); if (i<0) return(-1);

        sprintf(x,"Canonical analysis on %s:",word[1]);
        tulosta_rivi(x);

        if (m3)
            {
            i=matrix_load("&Z",&A0,&m,&nn,&rlab0,&clab0,&lr,&lc,&type,expr);
            if (i<0) return(-1);

            k=sprintf(x,"Confounding variables: ");
            for (i=0; i<m3; ++i)
                {
                char ots[LLENGTH];
                int h;
                if (k>c3-10) { tulosta_rivi(x); k=0; }
                strncpy(ots,clab0+i*lc,lc); ots[lc]=EOS;
                h=strlen(ots); while (ots[h-1]==' ' && h>0) ots[--h]=EOS;
                k+=sprintf(x+k,"%s",ots);
                if (i<m3-1) k+=sprintf(x+k,", ");
                }
            tulosta_rivi(x);
            }

        i=matrix_load("LCAN.M",&A0,&nn,&m,&rlab0,&clab0,&lr,&lc,&type,expr);
        if (i<0) return(-1);

        tulosta_rivi("Correlation    CHI^2     P       df      (LCAN.M)");
        mr=m2; if (m1<m2) mr=m1;

        for (i=0; i<mr; ++i)
            {
            q=1.0; for (k=i; k<mr; ++k) q*=1.0-A0[k]*A0[k];
            if (q<=0.0) q=1e300;
            else q=-(n-1-(m1+m2+1)/2.0)*log(q);
            df=(m1-i)*(m2-i);
            a=muste_cdf_chi2(q,(double)df,1e-7);

            ac=accuracy+1;
            fnconv(A0[i],accuracy,tulos1);
            fnconv(q,accuracy,tulos2);
            fnconv(a,ac,tulos3);
            sprintf(x,"%2d %*.*s %*.*s %*.*s  %4d",i+1,ac,ac,tulos1,ac,ac,tulos2,ac,ac,tulos3,df);
            tulosta_rivi(x);
            }
        if (!m3)
            tulosta_rivi("Coefficients (LINCO) for canonical variables saved in XCOEFF.M,YCOEFF.M")
;

        tulosta_rivi(" ");
        output_close(eout);

        i=matrix_load("XCAN.M",&A0,&m,&nn,&rlab0,&clab0,&lr,&lc,&type,expr);
        if (i<0) return(-1);

        results_line=matrix_print(A0,m,nn,rlab0,clab0,lr,lc,m,nn,NULL,NULL,"12.123",c3,
                                results_line,eout,
                "Correlations of canonical variables with X variables XCAN.M");

        i=matrix_load("YCAN.M",&A0,&m,&nn,&rlab0,&clab0,&lr,&lc,&type,expr);
        if (i<0) return(-1);

        results_line=matrix_print(A0,m,nn,rlab0,clab0,lr,lc,m,nn,NULL,NULL,"12.123",c3,
                                results_line,eout,
                "Correlations of canonical variables with Y variables YCAN.M");

        return(1);
        }

static int tulosta_rivi(char *x)
        {
        output_line(x,eout,results_line);
        if (results_line) ++results_line;
        if (results_line>r2) results_line=0;
        return(1);
        }

// N‰m‰ funktiot kirjastoon!

static int text_labels(char *lab,int n,char *text)
        {
        text_labels2(lab,n,text,1);
        return(1);
        }

static int text_labels2(char *lab,int n,char *text,int base)
        {
        char *t,*p;
//        int pit;
        char label[32];
        int i,j;
        int len;

        len=8;
        if (*text=='"') t=text+1; else t=text;
        p=strchr(t,'"'); if (p!=NULL) *p=EOS;
//        pit=strlen(t);
        for (i=0; i<n*len; ++i) lab[i]=' ';
        for (i=0; i<n; ++i)
            {
            snprintf(label,32,"%s%d",t,i+base);
            for (j=0; j<len; ++j)
                {
                if (label[j]==EOS) break;
                lab[i*len+j]=label[j];
                }
            }
        return(1);
        }

static int matrix_space(double **A,int m,int n,char **rlab,char **clab,int mcr,int mcl)
// double **A;  /* matriisitila (alkuosoite) (malloc) */
// int m;       /* rivien lkm */
// int n;       /* sar. lkm   */
// char **rlab; /* rivien otsikot (malloc) */
// char **clab; /* sar. otsikot   (malloc) */
// int mcr;     /* riviotsikoiden pituus */
// int mcl;     /* sarakeotsikoiden pituus */
        {
        if (*A!=NULL) *A=(double *)muste_realloc(*A,m*n*sizeof(double));
        else *A=(double *)muste_malloc(m*n*sizeof(double));
        if (*A==NULL) { matrix_nospace(); return(-1); }
        if (rlab!=NULL)
            {
            if (*rlab!=NULL) *rlab=(char *)muste_realloc(*rlab,m*mcr+1);
            else *rlab=(char *)muste_malloc(m*mcr+1);
            if (*rlab==NULL) { matrix_nospace(); return(-1); }
            }
        if (clab!=NULL)
            {
            if (*clab!=NULL) *clab=(char *)muste_realloc(*clab,n*mcl+1);
            else *clab=(char *)muste_malloc(n*mcl+1);
            if (*clab==NULL) { matrix_nospace(); return(-1); }
            }
        return(1);
        }

static int matrix_nospace()
        {
        sur_print("\nNot enough space for matrices");
        WAIT; return(1);
        }

