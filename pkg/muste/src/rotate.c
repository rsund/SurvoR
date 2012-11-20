#include "muste.h"
/* _rotate.c 15.2.1987/SM (4.5.1992) (22.3.1994)
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define PI 3.14159265358979

static double *F;
static int m,n;
static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];
static int k; /* number of factors */
static double *T;
static char *trlab,*tclab;
static int tlr,tlc;
static int tt,tt2; /*  1=init T orthogonal,  2=oblique T  */
static int ortho; /* 1=orthogonal 0=oblique */
static int method;
static double *fh;
static char *rlabh,*clabh;
static char graph_options[32];
static int vshow=0;
static double *FS;  // SHOW matrix
static char *shrlab,*shclab;
static int *sh_ind;
static int weights_on=0; // CLF-rotaatioihin (k‰ytet‰‰n FS-vektoria)
static int m_ext; // 30.11.2004
static double *F_ext;
static char *extrlab,*extclab;
// static int extlr,extlc;
static int corr_saved=0; // factor correlations RFACT.M
static double *corr;
static double *apu;
static int results_line;
// static double l_luku=0.0;

static double *cos_m,*cos_t,*cos_u,*cos_v,*cos_u2;
static int *cos_i,*cos_imax;
static int *ok;

static double *ff0,*cc,*h,*s,*u,*v,*x,*spq,*cc2;
static double dd,gg,hh,f0,f1,f2;

static double *T1,*T2,*L,*L1,*G,*TdT,*Gp,*X,*Tt;
static double *T_min;

static double (*pff)();
static double bb;   // band width


static int print_results();
static int not_enough_memory();
static int ttype(double *T,int k1,int k2,char *name);
static int extend_fmatrix(double *f,int m,int k,char *rlab,char *clab,double **pfh,char **prlabh,char **pclabh);
static int print_line(char *line);
static int varimax(double *F,int m,int k,double *T,double eps);
static int rotate(double *F,int m,int k,int ix,int iy,double angle,double *T);
static int cos_rot(double *F,int m,int k,double *T,double h2_limit);
static int save_factcorr_1(int k);
static int oblimin(double *F,int n,int m,double *T,double delta,int max_iter,double eps);
static int save_factcorr(double *cc,int m,char *clab,int lc);
static int oblimin_tilat(int n,int m);
static double rot_root(double dn,double x);

static int rot_gp(double *A,int m,int k,double *T,int type);
static int rot_ortho_clf(double *A,int m,int k,double *T,double b,int type);
static double ortho_ff(double *A,double *T,int m,int k);
static double linear_right_constant(double u);
static double quartimax_ff(double *A,double *T,int m,int k);
static double entropy_ff(double *A,double *T,int m,int k);
static double ff(double *A,double *T,int m,int k);
static double linear_clf(double u);
static double vff(double *A,double *T,int m,int k);
static int gf(double *A,double *G,double *T,int m,int k);
static double root3(double a1,double a2,double a3);

/************************
char *specs0[]={ "ROTATION", "METHOD", "FSHOW", "EXTERNAL", "WEIGHTS",
                 "RESULTS", "!" };
char **specs=specs0;
***************************/
extern char **spb;
/**********
void main(argc,argv)
int argc; char *argv[];
****************/
void muste_rotate(char *argv)
        {
        int i,j,h;
        char x[LLENGTH];
        char *sana[3];
        double delta;
        int max_iter;
        double h2_limit;

// RS 19.11.2012 Variable init
F=NULL;
m=n=0;
rlab=NULL;
clab=NULL;
lr=lc=0;
type=0;
//static char expr[129];
k=0;
T=NULL;
trlab=NULL;
tclab=NULL;
tlr=tlc=0;
tt=tt2=0;
ortho=0;
method=0;
fh=NULL;
rlabh=NULL;
clabh=NULL;
//static char graph_options[32];
vshow=0;
FS=NULL;
shrlab=NULL;
shclab=NULL;
sh_ind=NULL;
weights_on=0; 
m_ext=0;
F_ext=NULL;
extrlab=NULL;
extclab=NULL;
corr_saved=0;
corr=NULL;
apu=NULL;
results_line=0;
cos_m=NULL;
cos_t=NULL;
cos_u=NULL;
cos_v=NULL;
cos_u2=NULL;
cos_i=NULL;
cos_imax=NULL;
ok=NULL;
ff0=NULL;
cc=NULL;
//h=NULL;
s=NULL;
u=NULL;
v=NULL;
//x=NULL;
spq=NULL;
cc2=NULL;
dd=gg=hh=f0=f1=f2=0;
T1=NULL;
T2=NULL;
L=NULL;
L1=NULL;
G=NULL;
TdT=NULL;
Gp=NULL;
X=NULL;
Tt=NULL;
T_min=NULL;
pff=NULL;
bb=0;

//      if (argc==1) return;
        s_init(argv);

        if (g<2)
            {
            init_remarks();
            rem_pr("Usage: ROTATE F,k,L");
            rem_pr(" where F=factor matrix (matrix file) to be rotated");
            rem_pr("       k=number of factors");
            rem_pr("       L=first line for the results");
            rem_pr(" Rotated factor matrix is saved as AFACT.M");
            rem_pr(" Rotation matrix is saved as TFACT.M");
            rem_pr("");
            rem_pr(" ROTATION=VARIMAX automatic Varimax rotation (default)");
            rem_pr(" ROTATION=COS oblique Cosine rotation");
            rem_pr(" Missing: ROTATION=GRAPHICAL interactive orthogonal/oblique graphical rotation.");
            rem_pr(" ROTATION=OBLIMIN Direct Oblimin solution (oblique)");
            rem_pr(" T=<initial rotation matrix> (default I)");
            rem_pr("More information by ROTATE?");
            wait_remarks(2); s_end(argv);
            return;
            }
        tut_init();
        i=sp_init(r1+r-1); if (i<0) return;
        i=spfind("RESULTS"); if (i>=0) results=atoi(spb[i]);

        vshow=0;
        i=matrix_load(word[1],&F,&m,&n,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return;
        k=n;
        if (g>2)
            {
            k=atoi(word[2]);
            if (k<2)
                {
                sprintf(sbuf,"At least 2 factors needed!");
                if (etu==2)
                    {
                    sprintf(tut_info,"___@1@ROTATE@%s@",sbuf);
                    s_end(argv); return;
                    }
                sur_print("\n"); sur_print(sbuf);
                }
            if (k>n)
             { sprintf(sbuf,"\nMax. %d factors possible!",n); sur_print(sbuf); WAIT; return; }
            }

        results_line=0;
        if (g>3)
            {
            results_line=edline2(word[3],1);
            if (results_line==0) return;
            }

        T=(double *)muste_malloc(k*k*sizeof(double));
        if (T==NULL) { not_enough_memory(); return; }
        for (i=0; i<k; ++i)
            { for (j=0; j<k; ++j) T[i+k*j]=0.0; T[(k+1)*i]=1.0; }

        tt=tt2=0;
        i=spfind("T");
        if (i>=0)
            {
            int k1,k2;

            j=matrix_load(spb[i],&T,&k1,&k2,&trlab,&tclab,&tlr,&tlc,&type,expr);
            if (j<0)
                {
                sprintf(sbuf,"\nInitial rotation matrix T=%s not found!",spb[i]);
                sur_print(sbuf); WAIT; return;
                }
            tt=tt2=ttype(T,k1,k2,spb[i]);
            if (tt<0) return;
            }
        else
            {
            tlr=lc; tlc=lc;
            trlab=(char *)muste_malloc(lc*k);
            if (trlab==NULL) { not_enough_memory(); return; }
            tclab=(char *)muste_malloc(lc*k);
            if (tclab==NULL) { not_enough_memory(); return; }
            for (i=0; i<lc*k; ++i) trlab[i]=tclab[i]=clab[i];
            }

// For graphical rotation:  25.11.2004
        sh_ind=(int *)muste_malloc(m*sizeof(int));
        if (sh_ind==NULL) { not_enough_memory(); return; }
        for (i=0; i<m; ++i) sh_ind[i]=1; // all variables shown!
        weights_on=0;
        i=spfind("FSHOW");
        if (i<0) i=spfind("WEIGHTS");
        if (i>=0)
            {

            i=matrix_load(spb[i],&FS,&h,&j,&shrlab,&shclab,&lr,&lc,&type,expr);
            if (i<0)
                {
                sprintf(sbuf,"\nMatrix FSHOW=%s or WEIGHTS=%s not found!"
                                            ,spb[i],spb[i]);
                sur_print(sbuf); WAIT; return;
                }
            if (h!=m)
                {
                sprintf(sbuf,"\nErroneous # of rows in matrix %s!",spb[i]);
                sur_print(sbuf); WAIT; return;
                }
            for (i=0; i<m; ++i) sh_ind[i]=(int)FS[i];
            weights_on=1; // CLF-rotaatioihin
            }

        m_ext=0; // 30.11.2004
        i=spfind("EXTERNAL");
        if (i>=0)
            {
            i=matrix_load(spb[i],&F_ext,&m_ext,&j,&extrlab,&extclab,&lr,&lc,&type,expr);
            if (j!=k)
                {
                sprintf(sbuf,"\nErroneous # of columns in matrix %s!",spb[i]);
                sur_print(sbuf); WAIT; return;
                }
            }

        i=spfind("ROTATION");
        if (i<0) i=spfind("METHOD");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            h=split(x,sana,3);

            if (muste_strnicmp(sana[0],"VAR",3)==0)
                { i=varimax(F,m,k,T,1e-10); method=0; ortho=1; }

            else if (muste_strnicmp(sana[0],"GRA",3)==0)
                  {
                  *graph_options=EOS;
                  if (h>1) { strcpy(graph_options,sana[1]); }
muste_fixme("\nGraphical rotation not yet implemented!"); // RS FIXME                  
sur_print("\nGraphical rotation in Survo (not yet available in Muste)");
WAIT; return;

         //       i=graph_rot(F,m,k,T,rlab,clab,lr,lc); method=1;
                  }

            else if (muste_strnicmp(sana[0],"COS",3)==0)
                {
                h2_limit=0.3;
                if (h>1) h2_limit=atof(sana[1]);
                i=cos_rot(F,m,k,T,h2_limit); if (i<0) return; method=2; ortho=0;
                }
            else if (muste_strnicmp(sana[0],"OBL",3)==0)
                {
                delta=0.0; max_iter=30;
                if (h>1) delta=atof(sana[1]);
                if (h>2) max_iter=atoi(sana[2]);
                i=oblimin(F,m,k,T,delta,max_iter,1e-6);
                method=3; ortho=0;
                }
            else if (muste_strnicmp(sana[0],"CLF",3)==0)
                {
                i=rot_gp(F,m,k,T,1);
                method=4; ortho=0;
                }
            else if (muste_strnicmp(sana[0],"QUARTIMIN",9)==0)
                {
                i=rot_gp(F,m,k,T,2);
                method=5; ortho=0;
                }
            else if (muste_strnicmp(sana[0],"ORTHO_CLF",9)==0)
                {
                delta=0.3; // Jennrich b
                if (h>1) delta=atof(sana[1]);
                i=rot_ortho_clf(F,m,k,T,delta,1);
                method=6; ortho=1;
                }
            else if (muste_strnicmp(sana[0],"QUARTIMAX",9)==0)
                {
                i=rot_ortho_clf(F,m,k,T,0.0,2);
                method=7; ortho=1;
                }
            else if (muste_strnicmp(sana[0],"ENTROPY",7)==0)
                {
                i=rot_ortho_clf(F,m,k,T,0.0,3);
                method=8; ortho=1;
                }
            else
                { i=varimax(F,m,k,T,1e-10); method=0; ortho=1; }
            }
        else
            { i=varimax(F,m,k,T,1e-10); method=0; ortho=1; }
        if (i<0) return;
        matrix_save("AFACT.M",F,m,k,rlab,tclab,lr,tlc,0,"A",0,0);
        matrix_save("TFACT.M",T,k,k,trlab,tclab,tlr,tlc,0,"T",0,0);

        if (!corr_saved && !ortho)
            {
            corr=(double *)muste_malloc(k*k*sizeof(double));
            mat_mtm(corr,T,k,k);
            save_factcorr(corr,k,clab,lc);
            }
        if (ortho) save_factcorr_1(k);

        print_results();
        tut_end();
        s_end(argv);
        }

static int print_results()
        {
        int i;
        char x[LLENGTH];
        char acc[24];

        if (ortho==1)
            sprintf(x,"Rotated factor matrix AFACT.M=%s*TFACT.M",word[1]);
        else
            sprintf(x,"Rotated factor matrix AFACT.M=%s*inv(TFACT.M)'",word[1]);
        strcpy(acc,"12.1234567890123456");
        acc[accuracy-1]=EOS;

        i=extend_fmatrix(F,m,k,rlab,tclab,&fh,&rlabh,&clabh); if (i<0) return(-1);
        i=matrix_print(fh,m+1,k+1,rlabh,clabh,lr,tlc,m+1,k+1,NULL,NULL,acc,c3,
                        results_line,eout,x);
        if (results_line) results_line=i;
        if (results<=70)
            {
            print_line("Rotation matrix saved as TFACT.M");
            if (ortho)
                {
                print_line("Factors are orthogonal (RFACT.M=I).");
                }
            else
                {
                print_line("Factor correlation matrix saved as RFACT.M");
                }
            return(1);
            }

        sprintf(x,"Rotation matrix TFACT.M");
        i=matrix_print(T,k,k,trlab,tclab,tlr,tlc,k,k,NULL,NULL,acc,c3,
                        results_line,eout,x);
        if (results_line) results_line=i;

        if (ortho)
            {
            print_line("Factor correlation matrix RFACT.M is an identity matrix.");
            }
        else
            {
            i=matrix_load("RFACT.M",&T,&k,&k,&rlab,&clab,&lr,&lc,&type,expr);
            if (i<0) return(-1);
            strcpy(x,"Factor correlation matrix RFACT.M");
            i=matrix_print(T,k,k,rlab,clab,lr,lc,k,k,NULL,NULL,acc,c3,
                            results_line,eout,x);
            if (results_line) results_line=i;
            print_line("The factor structure matrix SFACT.M is obtained by the commands:");
            print_line("MAT SFACT.M=AFACT.M*RFACT.M");
            sprintf(x,"MAT LOAD SFACT.M,%s,CUR+1",acc); print_line(x);
            }
        print_line("");
        return(1);
        }


static int not_enough_memory()
        {
        sur_print("\nNot enough memory!"); WAIT; return(1);
        }


static int ttype(double *T,int k1,int k2,char *name)
        {
        int i,j,h;
        double s;
        int type;

        if (k1!=k || k2!=k)
            {
            sprintf(sbuf,"\nInitial rotation matrix T=%s not a %d*%d matrix!",
                                name,k,k); sur_print(sbuf); WAIT; return(-1);
            }
        type=1;
        for (i=0; i<k; ++i) for (j=0; j<=i; ++j)
            {
            s=0; for (h=0; h<k; ++h) s+=T[h+k*i]*T[h+k*j];
            if (i==j)
                {
                if (fabs(s-1.0)>1e-3)
                    {
                    sprintf(sbuf,"\nColumn %d of T=%s not of length 1!",i,name);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                else continue;
                }
            if (fabs(s)>1e-3) { type=2; ortho=0; } /* T not orthogonal! */
            }
        return(type);
        }

static int extend_fmatrix(double *f,int m,int k,char *rlab,char *clab,double **pfh,char **prlabh,char **pclabh)
        {
        int i,j;
        double a,b;
        double *fh;
        char *rlabh,*clabh;

        *pfh=fh=(double *)muste_malloc((m+1)*(k+1)*sizeof(double));
        if (fh==NULL) { not_enough_memory(); return(-1); }
        *prlabh=rlabh=muste_malloc((m+1)*lr+1);
        if (rlabh==NULL) { not_enough_memory(); return(-1); }
        *pclabh=clabh=muste_malloc((k+1)*lc+1);
        if (clabh==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<m; ++i) for (j=0; j<k; ++j) fh[i+j*(m+1)]=f[i+j*m];

        b=0.0;
        for (i=0; i<m; ++i)
            {
            a=0.0;
            for (j=0; j<k; ++j) a+=f[i+j*m]*f[i+j*m];
            fh[i+k*(m+1)]=a; b+=a;
            }
        fh[m+k*(m+1)]=b;
        for (j=0; j<k; ++j)
            {
            a=0.0;
            for (i=0; i<m; ++i) a+=f[i+j*m]*f[i+j*m];
            fh[m+j*(m+1)]=a;
            }
        for (i=0; i<m*lr; ++i) rlabh[i]=rlab[i];
        strcpy(rlabh+m*lr,"Sumsqr  ");
        for (i=0; i<k*lc; ++i) clabh[i]=clab[i];
        strcpy(clabh+k*lc,"Sumsqr  ");
        return(1);
        }

static int print_line(char *line)
        {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
        return(1);
        }

/* rotv.c    ?.?.1987   (2.5.1992) (15.9.1994) (2.10.1996)
   VARIMAX
   COS Ahmavaara: Kosinirotaatio
*/

#define PI 3.14159265358979

static int varimax(double *F,int m,int k,double *T,double eps)
        {
        int i,j,ix,iy;
        double max_angle, angle;
        double a,a0,a1,a2,a3,a4,x1,x2,u,v;
        double *h;
        double eps1=0.001;

        for (i=0; i<k; ++i)
            { for (j=0; j<k; ++j) T[i+k*j]=0.0; T[i*(k+1)]=1.0; }
        sur_print("\nVarimax rotation:");
        sprintf(sbuf,"  m=%d k=%d",m,k); sur_print(sbuf);
        while (sur_kbhit()) sur_getch();
                       /* 15.9.94 */
        sur_print("\nTo interrupt, press any key!");

        h=(double *)muste_malloc(m*sizeof(double));
        if (h==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<m; ++i)
            {
            a1=0.0; for (j=0; j<k; ++j) a1+=F[i+j*m]*F[i+j*m];
            h[i]=sqrt(a1);
            }

        while (1)
            {
            max_angle=0.0;
            for (ix=0; ix<k; ++ix) for(iy=ix+1; iy<k; ++iy)
                {
                a1=a2=a3=a4=0.0;
                for (i=0; i<m; ++i)
                    {
                    x1=F[i+m*ix]/h[i];
                    x2=F[i+m*iy]/h[i];
                    u=x1*x1-x2*x2; v=2*x1*x2;
                    a1+=u; a2+=v; a3+=u*u-v*v; a4+=2*u*v;
                    }
                a0=a3-(a1*a1-a2*a2)/m; a=a4-2*a1*a2/m;
                if (fabs(a0)<eps1) angle=0.0;
                else
                    {
                    a2=atan(a/a0);
                    if (a0<0) a2+=PI;
                    if (a2>=PI) a2-=2*PI;
                    angle=a2/4;
/*
                    a2=atan(a/a0)+PI/4;
                    if (a0<0) a2+=PI/4; else a2-=PI/4;
                    if (a2>PI/2) a2-=PI;
                    angle=a2/4;
*/
                    }

                if (fabs(angle)>max_angle) max_angle=fabs(angle);
                rotate(F,m,k,ix,iy,angle,T);
                } /* next iy,ix  */
            sprintf(sbuf,"\nMax. rotation (in degrees) %f",360.0*max_angle/(2*PI));
            sur_print(sbuf);
            if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); break; }

            if (max_angle<eps) break;
            }
//      save_factcorr_1(k);

        return(1);
        }


static int rotate(double *F,int m,int k,int ix,int iy,double angle,double *T)
        {
        double co,si;
        int i;
        double ax,ay;

        co=cos(-angle); si=sin(-angle);
        for (i=0; i<m; ++i)
            {
            ax=F[i+m*ix]; ay=F[i+m*iy];
            F[i+m*ix]=ax*co-ay*si; F[i+m*iy]=ax*si+ay*co;
            }
        for (i=0; i<k; ++i)
            {
            ax=T[i+k*ix]; ay=T[i+k*iy];
            T[i+k*ix]=ax*co-ay*si; T[i+k*iy]=ax*si+ay*co;
            }
        return(1);
        }

static int cos_rot(double *F,int m,int k,double *T,double h2_limit)
        {
        int i,j,h;
        double da,db;
        double detmax,detm=0,det1,det;
        double chh;
        int jmax=0,ii,jj;
        int oksum;

        cos_m=(double *)muste_malloc(m*m*sizeof(double));
        if (cos_m==NULL) { not_enough_memory(); return(-1); }
        cos_t=(double *)muste_malloc(k*k*sizeof(double));
        if (cos_t==NULL) { not_enough_memory(); return(-1); }
        cos_u=(double *)muste_malloc(k*sizeof(double));
        if (cos_u==NULL) { not_enough_memory(); return(-1); }
        cos_u2=(double *)muste_malloc(k*sizeof(double));
        if (cos_u2==NULL) { not_enough_memory(); return(-1); }
        cos_v=(double *)muste_malloc(k*sizeof(double));
        if (cos_v==NULL) { not_enough_memory(); return(-1); }
        cos_i=(int *)muste_malloc(k*sizeof(int));
        if (cos_i==NULL) { not_enough_memory(); return(-1); }
        cos_imax=(int *)muste_malloc(k*sizeof(int));
        if (cos_imax==NULL) { not_enough_memory(); return(-1); }
        ok=(int *)muste_malloc(m*sizeof(double));
        if (ok==NULL) { not_enough_memory(); return(-1); }

        sur_print("\nCosine rotation (Ahmavaara):");
        oksum=0;
        for (i=0; i<m; ++i) for (j=0; j<=i; ++j)
            {
            da=0.0; for (h=0; h<k; ++h) da+=F[i+m*h]*F[j+m*h];
            cos_m[i+m*j]=da; cos_m[j+m*i]=da;
            if (i==j) /* 21.3.1994 */
                {
                if (da<h2_limit) ok[i]=0; else ok[i]=1;
                oksum+=ok[i];
                }
            }
        if (oksum<k)
            {
 sprintf(sbuf,"\nOnly %d (<%d) of variables exceeding communality level %g . Cannot continue!",
                                oksum,k,h2_limit);
            sur_print(sbuf);
            WAIT; return(-1);
            }
        detmax=0.0;
        for (i=0; i<m; ++i)
            {
            if (!ok[i]) continue;  /* 21.3.1994 */
            sprintf(sbuf,"\n%2d",i+1); sur_print(sbuf);
            cos_i[0]=i;
            det1=cos_m[i*(m+1)];
            if (det1==0.0) continue; /* 2.10.1996 */
            cos_t[0]=1/det1;

            for (h=2; h<=k; ++h)
                {
                detm=0.0;
                for (j=0; j<m; ++j)
                    {
                    if (!ok[j]) continue;  /* 21.3.1994 */
                    for (ii=0; ii<h-1; ++ii) if (j==cos_i[ii]) break;
                    if (ii<h-1) continue;
                    for (ii=0; ii<h-1; ++ii) cos_u[ii]=cos_m[j+m*cos_i[ii]];
                    chh=cos_m[j*(m+1)];
                    da=chh;
                    for (ii=0; ii<h-1; ++ii)
                        {
                        da-=cos_t[ii*(k+1)]*cos_u[ii]*cos_u[ii];
                        for (jj=0; jj<ii; ++jj) da-=2*cos_t[ii+k*jj]*cos_u[ii]*cos_u[jj];
                        }
                    if (da>detm) { detm=da; jmax=j; }
                    }
                if (detm==0.0) break;  /* 2.10.1996 */
                sprintf(sbuf,"%3d",jmax+1); sur_print(sbuf);
                cos_i[h-1]=jmax;
                det1*=detm;
                for (ii=0; ii<h-1; ++ii) cos_u[ii]=cos_m[jmax+m*cos_i[ii]];
                for (j=0; j<h-1; ++j)
                    {
                    da=0.0;
                    for (ii=0; ii<h-1; ++ii) da+=cos_t[j+k*ii]*cos_u[ii];
                    cos_u2[j]=da;
                    }
                for (ii=0; ii<h-1; ++ii) for (jj=0; jj<=ii; ++jj)
                    {
                    da=cos_t[ii+k*jj]+cos_u2[ii]*cos_u2[jj]/detm;
                    cos_t[ii+k*jj]=da; cos_t[jj+k*ii]=da;
                    }
                for (j=0; j<h-1; ++j)
                    {
                    da=-cos_u2[j]/detm; cos_t[h-1+k*j]=da; cos_t[j+k*(h-1)]=da;
                    }
                cos_t[(h-1)*(k+1)]=1/detm;
                }
            if (detm==0.0) continue; /* 2.10.1996 */
            sprintf(sbuf," det=%g",det1); sur_print(sbuf);
            if (det1>detmax)
                {
                detmax=det1;
                for (j=0; j<k; ++j) cos_imax[j]=cos_i[j];
                }
            } /* i */

        if (detmax==0.0)
            {
            sur_print("\nHeavily singular factor matrix! Cannot rotate!");
            WAIT; return(-1); // RS CHA exit
            }

/* names of factors from factor variables  5.4.1999 */
        for (i=0; i<32; ++i) sbuf[i]=' ';  /* possible trailing spaces */
        for (i=0; i<k; ++i)
            {
            for (h=0; h<lr; ++h) sbuf[h]=rlab[cos_imax[i]*lr+h];
            for (h=0; h<tlc; ++h) tclab[i*tlc+h]=sbuf[h];
            }

        for (j=0; j<k; ++j)
            {
            da=0.0;
            for (i=0; i<k; ++i)
                { db=F[cos_imax[j]+m*i]; T[i+k*j]=db; da+=db*db; }
            da=sqrt(da);
            for (i=0; i<k; ++i) T[i+k*j]/=da;
            }
        for (i=0; i<k*k; ++i) cos_t[i]=T[i];
        mat_inv(cos_m,cos_t,k,&det);
        mat_transp(cos_t,cos_m,k,k);
        for (i=0; i<m*k; ++i) cos_m[i]=F[i];
        mat_mlt(F,cos_m,cos_t,m,k,k);
        for (i=0; i<k*k; ++i) cos_m[i]=T[i];  /* 2.5.1992 */
        mat_nrm(cos_t,cos_m,k,k);
        mat_mtm(cos_t,cos_m,k,k);
        save_factcorr(cos_t,k,tclab,tlc);
        if (sur_kbhit()) { while (sur_kbhit()) sur_getch(); WAIT; }

        return(1);
        }

static int p_close() { return(1); }

static int save_factcorr_1(int k)
        {
        int i,h;

        apu=(double *)muste_malloc(k*k*sizeof(double));
        if (apu==NULL) { not_enough_memory(); p_close(); return(1); }
        for (i=0; i<k; ++i)
            { for (h=0; h<k; ++h) apu[i+h*k]=0.0; apu[i+i*k]=1.0; }
        save_factcorr(apu,k,clab,lc);
        return(1);
        }

// p_close() { return(1); } // 29.6.2011/SM

/* roto.c  29.4.1992/SM (2.5.1992)
   OBLIMIN
*/

#define PI 3.14159265358979

#define M 10
#define N 50

static int oblimin(double *F,int n,int m,double *T,double delta,int max_iter,double eps)
        {
        int i,k,n_iter;
        double a1,a2,a3;
        double dn;
        int p,q;
        double dpq,gpq,ypq,zpq,tt,zz,pp,rr,pw,qw,rw,a,aa,t1,t2;

        i=oblimin_tilat(n,m); if (i<0) return(-1);
        for (i=0; i<m*n; ++i) ff0[i]=F[i];

        dn=delta/n;
        for (i=0; i<m; ++i)
            { for (k=0; k<m; ++k) cc[i+m*k]=0.0; cc[i*(m+1)]=1.0; }
        sur_print("\nOblimin rotation:");
        sprintf(sbuf,"  n=%d m=%d",n,m); sur_print(sbuf);

        while (sur_kbhit());
        sur_print("\nTo interrupt, press any key!");

        for (k=0; k<n; ++k)
            {
            a1=0.0; for (i=0; i<m; ++i) a1+=F[k+i*n]*F[k+i*n];
            h[k]=sqrt(a1);
            }
        for (k=0; k<n; ++k)
            for (i=0; i<m; ++i) F[k+i*n]/=h[k];

        for (k=0; k<n; ++k) s[k]=1.0;
        for (i=0; i<m; ++i)
            {
            a1=a2=0.0;
            for (k=0; k<n; ++k) { a3=F[k+i*n]*F[k+i*n]; a1+=a3; a2+=a3*a3; }
            u[i]=a1; v[i]=a2;
            }

        for (i=0; i<m; ++i) x[i]=v[i]-dn*u[i]*u[i];
        dd=gg=hh=0.0;
        for (i=0; i<m; ++i)
            { dd+=u[i]; gg+=x[i]; hh+=s[i]*s[i]; }
        hh-=dn*dd*dd;
        f0=f1=hh-gg;

        n_iter=0;
        while (1)
            {
            for (p=0; p<m; ++p)
            for (q=0; q<m; ++q)
                {
                if (p==q) continue;
                dpq=dd-u[p]-u[q];
                gpq=gg-x[p]-x[q];
                for (i=0; i<n; ++i) spq[i]=s[i]-F[i+p*n]*F[i+p*n]
                                               -F[i+q*n]*F[i+q*n];
                ypq=0.0; for (i=0; i<n; ++i) ypq+=F[i+p*n]*F[i+q*n];
                zpq=0.0; for (i=0; i<n; ++i) zpq+=F[i+p*n]*F[i+p*n]*F[i+q*n]*F[i+q*n];
                tt=0.0; for (i=0; i<n; ++i) tt+=F[i+p*n]*F[i+p*n]*spq[i];
                tt-=dn*u[p]*dpq;
                zz=0.0; for (i=0; i<n; ++i) zz+=F[i+p*n]*F[i+q*n]*spq[i];
                zz-=dn*ypq*dpq;
                pp=0.0; for (i=0; i<n; ++i) pp+=F[i+p*n]*F[i+p*n]*F[i+p*n]*F[i+q*n];
                pp-=dn*u[p]*ypq;
                rr=zpq-dn*u[p]*u[q];
                pw=1.5*(cc[p+q*m]-pp/x[p]);
                qw=0.5*(x[p]-4*cc[p+q*m]*pp+rr+2*tt)/x[p];
                rw=0.5*(cc[p+q*m]*(tt+rr)-pp-zz)/x[p];

                a=root3(pw,qw,rw);
/*    Rprintf("\np=%d q=%d a=%g %g %g %g",p,q,a,pw,qw,rw); getch();   */
                aa=1+2*cc[p+q*m]*a+a*a;
                t1=sqrt(fabs(aa));
                t2=a/t1;
                for (k=0; k<n; ++k)
                    {
                    a1=t1*F[k+p*n];
                    a2=F[k+q*n]-a*F[k+p*n];
                    F[k+p*n]=a1; F[k+q*n]=a2;
                    }
                u[p]*=fabs(aa);
                x[p]*=aa*aa;
                a1=a2=0.0;
                for (i=0; i<n; ++i)
                    {
                    a3=F[i+q*n]*F[i+q*n]; a1+=a3; a2+=a3*a3;
                    }
                u[q]=a1; v[q]=a2;
                x[q]=v[q]-dn*u[q]*u[q];
                for (k=0; k<n; ++k)
                    s[k]=spq[k]+F[k+p*n]*F[k+p*n]+F[k+q*n]*F[k+q*n];
                dd=dpq+u[p]+u[q];
                gg=gpq+x[p]+x[q];

                for (i=0; i<m; ++i)
                    {
                    if (i==p) continue;
                    cc[i+p*m]=cc[i+p*m]/t1+t2*cc[i+q*m];
                    cc[p+i*m]=cc[i+p*m];
                    }
                cc[p+p*m]=1.0;
                }  /* p,q */
            ++n_iter;
            if (n_iter>max_iter) break;
            hh=0.0; for (k=0; k<n; ++k) hh+=s[k]*s[k];
            hh-=dn*dd*dd;
            f2=hh-gg;
            if (fabs((f2-f1)/f0)<eps) break;
            sprintf(sbuf,"\nIteration %d   %g",n_iter,fabs((f2-f1)/f0));

            sur_print(sbuf);
            f1=f2;

            if (sur_kbhit()) break;


            }

        for (k=0; k<n; ++k)
            for (i=0; i<m; ++i) F[k+i*n]*=h[k];

        save_factcorr(cc,m,clab,lc);
        corr_saved=1;

        mat_2mtm(T,F,ff0,n,m,m);      /* A'F1 */
        mat_inv(cc,T,m,&a1);         /* inv(A'F1) */
        mat_mtm(cc2,ff0,n,m);         /* F1'F1 */
        mat_mlt(T,cc2,cc,m,m,m);     /* T=F1'F1*inv(A'F1) */

        return(1);
        }

static int save_factcorr(double *cc,int m,char *clab,int lc)
        {
        matrix_save("RFACT.M",cc,m,m,clab,clab,lc,lc,0,"RFACT",0,0); return(1);
        }

static int oblimin_tilat(int n,int m)
        {
        ff0=(double *)muste_malloc(m*n*sizeof(double));
        if (ff0==NULL) { not_enough_memory(); return(-1); }
        cc=(double *)muste_malloc(m*m*sizeof(double));
        if (cc==NULL) { not_enough_memory(); return(-1); }
        h=(double *)muste_malloc(n*sizeof(double));
        if (h==NULL) { not_enough_memory(); return(-1); }
        s=(double *)muste_malloc(n*sizeof(double));
        if (s==NULL) { not_enough_memory(); return(-1); }
        u=(double *)muste_malloc(m*sizeof(double));
        if (u==NULL) { not_enough_memory(); return(-1); }
        v=(double *)muste_malloc(m*sizeof(double));
        if (v==NULL) { not_enough_memory(); return(-1); }
        x=(double *)muste_malloc(m*sizeof(double));
        if (x==NULL) { not_enough_memory(); return(-1); }
        spq=(double *)muste_malloc(n*sizeof(double));
        if (spq==NULL) { not_enough_memory(); return(-1); }
        cc2=(double *)muste_malloc(m*m*sizeof(double));
        if (cc2==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

/*        Z^3+a1*Z^2+a2*Z+a3=0      */
static double root3(double a1,double a2,double a3)
        {
        double p,q,d,u0,v0;
        extern double rot_root();

        p=a2-a1*a1/3;  q=a3-a1*a2/3+2.0/27.0*a1*a1*a1;
        d=q*q/4+p*p*p/27; /* (q/2)^2+(p/3)^3  */
        if (d<0) { sur_print("*"); return (0.0); }
        u0=rot_root(3.0,-q/2+sqrt(d));
        v0=rot_root(3.0,-q/2-sqrt(d));
        return(u0+v0-a1/3);
        }
/*
          Z^3+a1*Z^2+a2*Z+a3=0
          p=a2-a1^2/3  q=a3-a1*a2/3+2/27*a1^3
          D=(q/2)^2+(p/3)^3

Jos D>0,
          Z=u0+v0-a1/3 ,
          u0=rot_root(3,-q/2+sqrt(D))  (rot_root(3,x) on x:n kuutiojuuri)
          v0=rot_root(3,-q/2-sqrt(D))
*/

static double rot_root(double dn,double x)
        {
        int n;

        n=dn;
        if (x>0.0 || (double)n!=dn || n<0 || ((n>>1)<<1)==n) return(pow(x,1/dn));
        if (x==0.0) return(0.0);
        return(-pow(-x,1/dn));
        }

/* rot_gp.c 17.12.2004/SM (17.12.2004)
   oblique rotations (Jennrich GP algorithm + CLF)
*/
#define PI 3.14159265358979
/*
extern double ff();  // CLF
extern double vff(); // QUARTIMIN
extern double ortho_ff();  // linear right constant CLF
extern double quartimax_ff();
extern double entropy_ff();
extern double uniform_dev();

extern int weights_on; // =1, kun muuttujien painot vektorissa FS
extern double *FS;
*/

static int rot_gp(double *A,int m,int k,double *T,int type)
    {
    int i,j,iter,ii,rep;
    double a1,f,s,s2,ft,det;
    double f_min;

    if (type==1) pff=ff;
    else pff=vff;

    ft=0.0; // 29.6.2011/SM

//  for (i=0; i<k*k; ++i) T[i]=0.0;
//  for (i=0; i<k; ++i) T[i*(k+1)]=1.0;    // T=I  tilap.

    T1=muste_malloc(k*k*sizeof(double));
    if (T1==NULL) { not_enough_memory(); return(-1); }
    T2=muste_malloc(k*k*sizeof(double));
    if (T2==NULL) { not_enough_memory(); return(-1); }
    L=muste_malloc(m*k*sizeof(double));
    if (L==NULL) { not_enough_memory(); return(-1); }
    L1=muste_malloc(m*k*sizeof(double));
    if (L1==NULL) { not_enough_memory(); return(-1); }
    G=muste_malloc(k*k*sizeof(double));
    if (G==NULL) { not_enough_memory(); return(-1); }
    TdT=muste_malloc(k*k*sizeof(double));
    if (TdT==NULL) { not_enough_memory(); return(-1); }
    Gp=muste_malloc(k*k*sizeof(double));
    if (Gp==NULL) { not_enough_memory(); return(-1); }
    X=muste_malloc(k*k*sizeof(double));
    if (X==NULL) { not_enough_memory(); return(-1); }
    Tt=muste_malloc(k*k*sizeof(double));
    if (Tt==NULL) { not_enough_memory(); return(-1); }
    T_min=muste_malloc(k*k*sizeof(double));
    if (T_min==NULL) { not_enough_memory(); return(-1); }

    f_min=1e100;
    spec_rnd();
for (rep=0; rep<100; ++rep)
  {

  for (i=0; i<k*k; ++i) T[i]=0.5-uniform_dev();
  for (j=0; j<k; ++j)
      {
      s=0.0;
      for (i=0; i<k; ++i) s+=T[i+k*j]*T[i+k*j];
      s=1.0/sqrt(s);
      for (i=0; i<k; ++i) T[i+k*j]*=s;
      }

    a1=1;
    for (iter=0; iter<100; ++iter)
        {
        f=pff(A,T,m,k);
// Rprintf("\nf=%g|",f); getch();
        gf(A,G,T,m,k);
// mprint(G,k,k);
        for (j=0; j<k; ++j)
            {
            s=0.0;
            for (i=0; i<k; ++i)
                s+=T[i+k*j]*G[i+k*j];
            for (i=0; i<k; ++i)
                T1[i+k*j]=s*T[i+k*j];
            }
        mat_sub(Gp,G,T1,k,k);
// mprint(Gp,k,k);
        s=0.0;
        for (i=0; i<k*k; ++i) s+=Gp[i]*Gp[i];
        s=sqrt(s);
// Rprintf("\n%d %g %g %g|",iter,f,log(s)/log(10.0),a1);
// getch();
        if (s<1e-5) break;

        a1=2*a1;
        for (ii=0; ii<10; ++ii)
            {

            for (i=0; i<k*k; ++i) X[i]=T[i]-a1*Gp[i];

            for (j=0; j<k; ++j)
                {
                s2=0.0;
                for (i=0; i<k; ++i) s2+=X[i+k*j]*X[i+k*j];

                s2=1.0/sqrt(s2);
                for (i=0; i<k; ++i) Tt[i+k*j]=s2*X[i+k*j];

                }
            ft=pff(A,Tt,m,k);

            if (ft<f-0.5*s*s*a1) break;
            a1/=2.0;
            }
        for (i=0; i<k*k; ++i) T[i]=Tt[i];
        }

  if (ft<f_min) { for (i=0; i<k*k; ++i) T_min[i]=T[i]; f_min=ft; }
  } // rep

    for (i=0; i<k*k; ++i) T[i]=T_min[i];
    for (i=0; i<k*k; ++i) T1[i]=T[i];
    mat_inv(T2,T1,k,&det);
    mat_2mmt(L,A,T2,m,k,k);
    for (i=0; i<m*k; ++i) A[i]=L[i];
    return(1);
    }

static int rot_ortho_clf(double *A,int m,int k,double *T,double b,int type)
    {
    int i,j,iter,ii,rep;
    double a1,f,s,ft;
    double f_min;
    double eps=1e-16;
    double tol=(1e-300)/eps;

extern void muste_save_stack_count();
extern void	muste_restore_stack_count();

    bb=b; // globaaliksi!
    if (type==1) pff=ortho_ff;
    else if (type==2) pff=quartimax_ff;
    else pff=entropy_ff;

    ft=0.0; // 29.6.2011/SM

//  for (i=0; i<k*k; ++i) T[i]=0.0;
//  for (i=0; i<k; ++i) T[i*(k+1)]=1.0;    // T=I  tilap.

    T1=muste_malloc(k*k*sizeof(double));
    if (T1==NULL) { not_enough_memory(); return(-1); }
    T2=muste_malloc(k*k*sizeof(double));
    if (T2==NULL) { not_enough_memory(); return(-1); }
    L=muste_malloc(m*k*sizeof(double));
    if (L==NULL) { not_enough_memory(); return(-1); }
    L1=muste_malloc(m*k*sizeof(double));
    if (L1==NULL) { not_enough_memory(); return(-1); }
    G=muste_malloc(k*k*sizeof(double));
    if (G==NULL) { not_enough_memory(); return(-1); }
    TdT=muste_malloc(k*k*sizeof(double));
    if (TdT==NULL) { not_enough_memory(); return(-1); }
    Gp=muste_malloc(k*k*sizeof(double));
    if (Gp==NULL) { not_enough_memory(); return(-1); }
    X=muste_malloc(k*k*sizeof(double));
    if (X==NULL) { not_enough_memory(); return(-1); }
    Tt=muste_malloc(k*k*sizeof(double));
    if (Tt==NULL) { not_enough_memory(); return(-1); }
    T_min=muste_malloc(k*k*sizeof(double));
    if (T_min==NULL) { not_enough_memory(); return(-1); }

    f_min=1e100;
    spec_rnd();  
    for (rep=0; rep<100; ++rep)
        {
        for (i=0; i<k*k; ++i) T1[i]=0.5-uniform_dev();
        mat_qr(T1,T,k,k,1e-15);
//  mprint(T,k,k);
//  mat_mmt(T1,T,k,k);
//  mprint(T1,k,k);

        a1=1.0;             
        muste_save_stack_count(); // RS 19.11.2012
        for (iter=0; iter<100; ++iter)
            {            
//          f=ortho_ff(A,T,m,k);
            f=pff(A,T,m,k);
    // Rprintf("\nf=%g|",f); getch();
//          ortho_gf(A,G,T,m,k);
            gf(A,G,T,m,k);              
    // mprint(G,k,k);
            mat_2mtm(X,T,G,k,k,k);   // M
            for (i=0; i<k; ++i)
                for (j=0; j<i; ++j)
                    X[i+k*j]=X[j+k*i]=(X[i+k*j]+X[j+k*i])/2.0;  // S
            mat_mlt(T1,T,X,k,k,k);
            mat_sub(Gp,G,T1,k,k);
            s=0.0;
            for (i=0; i<k*k; ++i) s+=Gp[i]*Gp[i];
            s=sqrt(s);
//     Rprintf("\n%d %g %g %g|",iter,f,log(s)/log(10.0),a1);
//     getch();
            if (s<1e-5) break;
            a1=2*a1;
            for (ii=0; ii<10; ++ii)
                {
                for (i=0; i<k*k; ++i) X[i]=T[i]-a1*Gp[i];
                mat_svd(X,T1,T2,k,k,eps,tol);
                mat_2mmt(Tt,X,T2,k,k,k);
                ft=pff(A,Tt,m,k);
                if (ft<f-0.5*s*s*a1) break;
                a1/=2.0;
                }
            for (i=0; i<k*k; ++i) T[i]=Tt[i];
            }         
         if (ft<f_min) { for (i=0; i<k*k; ++i) T_min[i]=T[i]; f_min=ft; }
         muste_restore_stack_count(); // RS 19.11.2012
         }
    for (i=0; i<k*k; ++i) T[i]=T_min[i];
//  for (i=0; i<k*k; ++i) T1[i]=T[i];
//  mat_inv(T2,T1,k,&det);
//  mat_2mmt(L,A,T2,m,k,k);
    mat_mlt(L,A,T,m,k,k);
    for (i=0; i<m*k; ++i) A[i]=L[i];

    return(1);
    }

static double ortho_ff(double *A,double *T,int m,int k)  // linear right constant CLF
    {
    int i,j;
    double s,aa;
    extern double linear_right_constant();

    mat_mlt(L,A,T,m,k,k);
    s=0.0;
    if (weights_on)
        {
        for (i=0; i<m; ++i)
            {
            aa=FS[i];
            for (j=0; j<k; ++j) s+=aa*linear_right_constant(L[i+m*j]);
            }
        }
    else
        for (i=0; i<m*k; ++i) s+=linear_right_constant(L[i]);
    return(s);
    }

static double linear_right_constant(double u)
    {
// Rprintf("\nbb=%g|",bb); getch();
    if (fabs(u)<bb) return((u/bb)*(u/bb));
    return(1.0);
    }

static double quartimax_ff(double *A,double *T,int m,int k)  // quartimax
    {
    int i;
    double s,aa;

    mat_mlt(L,A,T,m,k,k);
    s=0.0;
    for (i=0; i<m*k; ++i) { aa=L[i]*L[i]; s-=aa*aa; }
    return(s);
    }

static double entropy_ff(double *A,double *T,int m,int k)
    {
    int i,j;
    double s,aa,bb;

    mat_mlt(L,A,T,m,k,k);
    s=0.0;
    if (weights_on)
        {
        for (i=0; i<m; ++i)
            {
            aa=FS[i];
            for (j=0; j<k; ++j)
                {
                bb=L[i+m*j]; bb*=bb;
                s-=aa*bb*log(bb);
                }
            }
        }
    else
        for (i=0; i<m*k; ++i) { aa=L[i]*L[i]; s-=aa*log(aa); }
    return(s);
    }

static double ff(double *A,double *T,int m,int k)  // linear CLF
    {
    int i,j;
    double det;
    double s,aa;
    extern double linear_clf();

    for (i=0; i<k*k; ++i) T1[i]=T[i];
    mat_inv(T2,T1,k,&det);
    mat_2mmt(L,A,T2,m,k,k);
    s=0.0;
    if (weights_on)
        {
        for (i=0; i<m; ++i)
            {
            aa=FS[i];
            for (j=0; j<k; ++j) s+=aa*linear_clf(fabs(L[i+m*j]));
            }
        }
    else
        for (i=0; i<m*k; ++i) s+=linear_clf(fabs(L[i]));
    return(s);
    }

static double linear_clf(double u)
    {
    double eps=0.01;
// eps2=eps*(1-eps/2) eps2=0.00995
    double eps2=0.00995;
// eps3=eps/2   eps3=0.005
    double eps3=0.005;

    if (u>eps) return(u);
    return(eps2+eps3*u*u);
    }

static double vff(double *A,double *T,int m,int k)  // quartimin
    {
    int i;
    double det;
    double s;

    for (i=0; i<k*k; ++i) T1[i]=T[i];
    mat_inv(T2,T1,k,&det);
    mat_2mmt(L,A,T2,m,k,k);              // L=L2
    for (i=0; i<m*k; ++i) L[i]=L[i]*L[i];
    for (i=0; i<k*k; ++i) T1[i]=1.0;
    for (i=0; i<k; ++i) T1[i*(k+1)]=0.0;   // T1=N
    mat_mlt(L1,L,T1,m,k,k);
    s=0.0;
    for (i=0; i<m*k; ++i) s+=L[i]*L1[i];
    return(s);
    }

static int gf(double *A,double *G,double *T,int m,int k)
    {
    int i,j,h;
    double ep;
    double f1,f2;

    ep=0.0001;
    for (i=0; i<k; ++i)
        for (j=0; j<k; ++j)
            {
            for (h=0; h<k*k; ++h) TdT[h]=T[h];
            TdT[i+k*j]+=ep;
            f1=pff(A,TdT,m,k);
            TdT[i+k*j]-=2*ep;
            f2=pff(A,TdT,m,k);
            G[i+k*j]=(f1-f2)/(2.0*ep);
            }

    return(1);
    }

