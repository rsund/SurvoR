/* !mnsimul.c 31.1.1996/SM (5.2.1996)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
// #include <memory.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

// extern double muste_sis_tulo();

static SURVO_DATA dat;
static int m;
static int n;

#define ERC 128
#define MAX_SPACE 65500L

static double *X;
static char *rlabX, *clabX, exprX[LLENGTH];
static int  mX, nX, typeX, lrX, lcX;
static double *Y;
static char *rlabY, *clabY, exprY[LLENGTH];
static int  mY, nY, typeY, lrY, lcY;
// static double *T;
// static char *rlabT, *clabT, exprT[LLENGTH];
// static int  mT, nT, typeT, lrT, lcT;
static int msn;
static double *d,*e;
static int ind;
// static int rand_type;
// extern unsigned int seed;
static int prind;
// static int transform;
static int cov;

static int varaa_tilat();
static int comp_coefficients();
static int not_corr_mat();
static int load_coefficients();
static int create_file();
static int etsi_tyypit(char *s);
static int sampling();
static int load_X(char *nimi);
static int load_Y(char *nimi);
static int save_X(char *nimi);
static int save_Y(char *nimi);
static int text_labels(char *lab,int n,char *text);
static int ei_tilaa();

// static double sur_rand0x(unsigned int x,int type);
// static double normal_devx();


/********************
char *specs0[]={ "RND", "TRANSFORM", "NAMELENGTH", "NAMELEN", "TYPES",
                 "NEWSPACE", "PRIND", "!" };
char **specs=specs0;
************************/
/*******************
void main(argc,argv)
int argc; char *argv[];
*********************/

void muste_mnsimul(char *argv)
        {
        int i;
    //  char x[LLENGTH];
/****************
        if (argc==1)
            {
            printf("This program can be used as a SURVO MM module only.");
            return;
            }
**************************/

X=NULL;
rlabX=NULL;
clabX=NULL;
Y=NULL;
rlabY=NULL;
clabY=NULL;
d=NULL;
e=NULL;

        s_init(argv);

        if (g<6)
            {
            init_remarks();
rem_pr("MNSIMUL <corr_file>,<msn_file>,<data_file>,N,<ind>      / S.Mustonen 31.1.1996");
rem_pr("simulates multivariate normal distribution by generating N obser-");
rem_pr("vations according to a given CORR file and MSN file (of means and");
rem_pr("std.devs).");
rem_pr("   The simulated observations will be saved in <data_file>. If");
rem_pr("<data_file> already exists, its previous structure and contents");
rem_pr("will be destroyed.");
rem_pr("   If <msn_file> is given as *, means are assumed to be =0 and");
rem_pr("std.devs=1, i.e. standardized data will be produced.");
rem_pr("");
rem_pr("When <ind> is 0, the covariance matrix S is computed from the given");
rem_pr("correlation matrix and standard deviations. Then S is decomposed");
rem_pr("into form S=CC' by spectral decomposition. The data values are");
rem_pr("generated by the formula X=C*U+M where U is multivariate N(0,I)");
rem_pr("and M is the vector of means.");
rem_pr("C is saved as a matrix file MNCOEFF.M on the temporary disk (tempdisk)");
rem_pr("of Survo. Similarly, M (if given) is saved as MNMEAN.M .");
rem_pr("Also a new data file <data_file> is created.");
rem_pr("");
rem_pr("After these preparations MNSIMUL generates the N observations.");

            wait_remarks(1);

rem_pr("When <ind> is 1, the above preparations are omitted and N observations");
rem_pr("are generated directly by using ready-made MNCOEFF.M and MNMEAN.M files.");
rem_pr("Thus in simulation experiments where many samples from the same");
rem_pr("multivariate normal distribution are required, the first sample");
rem_pr("must be created by setting <ind> to 0.");
rem_pr("Samples #2, #3, etc. can then be generated more quickly by setting <ind> to 1.");
rem_pr("For each sample, a different seed for (pseudo)random numbers must be used.");
rem_pr("");
rem_pr("The random number generator and its seed number is selected by specification");
rem_pr("RND=rand(1041994), for example. Default is RND=rand(123456789).");
rem_pr("Also INSEED and OUTSEED specifications are available (see RAND?).");
rem_pr("The normal random deviates are computed by the Box-M�ller method.");

            wait_remarks(1);

rem_pr("The names of variables in <data_file> are the row labels of <corr_file>.");
rem_pr("By default each variable is of the numerical type 4.");
rem_pr("The types can be changed by TYPES=<1,2,4 or 8>");
rem_pr("or by TYPES=<name_of_data_file>.");
rem_pr("In the latter case the types are selected according to variables");
rem_pr("in another data file.");
rem_pr("");
rem_pr("The size of the data file is minimal, i.e. no space for additional variables");
rem_pr("is reserved.");
rem_pr("However, by NEWSPACE=<#_of_additional_bytes>,<#_of_additional_variables>");
rem_pr("such additional space can be created for each observation.");
rem_pr("");
rem_pr("Another form of MNSIMUL is");
rem_pr("MNSIMUL <cov>,-,<data_file>,N,<ind>");
rem_pr("where <cov> is the covariance matrix of the distribution to be simulated.");
rem_pr("In this case it is assumed that means will be 0.");

            wait_remarks(2);
            s_end(argv);
            return;
            }
        i=spec_init(r1+r-1); if (i<0) return;

        ind=atoi(word[5]);
        if (ind==0) { i=load_X(word[1]); if (i<0) return; }
        if (*word[2]=='*') msn=0; else msn=1;
        if (*word[2]=='-') { msn=0; cov=1; } else cov=0;


        if (ind==1) { i=load_coefficients(); if (i<0) return; }
        if (msn==0)
            {
            Y=(double *)muste_malloc(mX*sizeof(double));
            if (Y==NULL) { ei_tilaa(); return; }
            for (i=0; i<mX; ++i) Y[i]=0.0;
            mY=mX;
            }
        else
            {
            if (ind==0) { i=load_Y(word[2]); if (i<0) return; }
            }

        if (msn && mX!=mY)
            {
            sprintf("\nUnequal # of variables (rows) in matrices %s and %s",
                             word[1],word[2]);

            }
        m=mX;
        n=atol(word[4]);

        i=varaa_tilat(); if (i<0) return;
        if (ind==0) { i=comp_coefficients(); if (i<0) return; }
        spec_rnd(); // 23.7.2011
/********************
        i=spfind("RND");
        if (i<0) strcpy(x,"123456789"); else strcpy(x,spb[i]);
        if (muste_strnicmp(x,"rand(",5)==0) { rand_type=1; i=5; }
        else if (muste_strnicmp(x,"urand(",6)==0) { rand_type=2; i=6; }
        else if (muste_strnicmp(x,"mrand(",6)==0) { rand_type=4; i=6; }
        else { rand_type=1; i=0; }
        seed=atol(x+i);
************************/
/*************************
        transform=1;
        i=spfind("TRANSFORM");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            if (atoi(x)==1 || muste_strcmpi(x,"BM")==0 || muste_strnicmp(x,"BOX",3)==0)
                      transform=1;
            else if (atoi(x)==2 || muste_strcmpi(x,"PROBIT")==0) transform=2;
            }
***************************/
        prind=0;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);
        if (ind==0) { i=create_file(); if (i<0) return; }

        i=sampling(); if (i<0) return;

        outseed();
        }

static int varaa_tilat()
        {

        d=(double *)muste_malloc(m*sizeof(double));
        if (d==NULL) { ei_tilaa(); return(-1); }
        e=(double *)muste_malloc(m*sizeof(double));
        if (e==NULL) { ei_tilaa(); return(-1); }

        return(1);
        }

static int comp_coefficients()
        {
        int i,j;
        double eps,tol;
        char x[LLENGTH];

        sprintf(sbuf,"\nPrincipal components for %d variables ... ",m);
        sur_print(sbuf);

        if (!cov)
            for (i=0; i<m; ++i)
                {
                if (fabs(X[i*(m+1)]-1.0)<1e-3) continue;
                not_corr_mat(); return(-1);
                }

        if (msn && !cov)
            {
            for (i=0; i<m; ++i) d[i]=Y[i+mY];
            for (i=0; i<m; ++i) for (j=0; j<m; ++j) X[i+m*j]*=d[i]*d[j];
            }

        eps=1e-16; tol=(1e-300)/eps;
        mat_tred2(d,e,X,m,tol);
        i=mat_tql2(d,e,X,m,eps,30);
        if (i<0)
            {
            sur_print("\nNo convergence when computing eigenvalues!");
            WAIT; return(-1);
            }
        if (d[m-1]/d[0]<-1e-3) { not_corr_mat(); return(-1); }
        for (i=0; i<m; ++i) d[i]=sqrt(fabs(d[i]));
        for (j=0; j<m; ++j)
            for (i=0; i<m; ++i) X[i+m*j]*=d[j];

        strcpy(x,etmpd); strcat(x,"MNCOEFF.M");
        text_labels(clabX,m,"COMP");
        sprintf(exprX,"Coefficients_for_simulated_data_%s",word[3]);
        save_X(x);
        if (msn)
            {
            strcpy(x,etmpd); strcat(x,"MNMEAN.M");
            nY=1;
            sprintf(exprY,"Means_for_simulated_data_%s",word[3]);
            strcpy(clabY,"Mean    ");
            save_Y(x);
            }
        return(1);
        }

static int not_corr_mat()
        {
        sprintf(sbuf,"\n%s is not a proper correlation matrix!",word[1]);
        sur_print(sbuf); WAIT; return(1);
        }

static int load_coefficients()
        {
        int i;
        char x[LLENGTH];

        strcpy(x,etmpd); strcat(x,"MNCOEFF.M");
        i=load_X(x); if (i<0) return(-1);
        if (msn)
            {
            strcpy(x,etmpd); strcat(x,"MNMEAN.M");
            i=load_Y(x); if (i<0) return(-1);
            }
        return(1);
        }


static char *namespace;
static char **varname;
static int *varlen;
static char **vartype;
static char *typespace;

static int create_file()
        {
        int i;
        char common_type[13];
        int types;
        int filen;
        char x[LNAME],*s[2];
        int extra_bytes,extra_fields;
        int namelength;

        namelength=64;
        i=spfind("NAMELENGTH");
        if (i<0) i=spfind("NAMELEN");
        if (i>=0) namelength=atoi(spb[i]);
        if (namelength<8) namelength=8;

        strcpy(common_type,"4A          ");
        namespace=muste_malloc(m*9);
        if (namespace==NULL) { ei_tilaa(); return(-1); }
        varname=(char **)muste_malloc(m*sizeof(char **));
        if (varname==NULL) { ei_tilaa(); return(-1); }
        for (i=0; i<m; ++i)
            {
            varname[i]=namespace+9*i;
            strncpy(varname[i],rlabX+8*i,8); varname[i][8]=EOS;
            }
        varlen=(int *)muste_malloc(m*sizeof(int));
        if (varlen==NULL) { ei_tilaa(); return(-1); }
        for (i=0; i<m; ++i) varlen[i]=4;
        vartype=(char **)muste_malloc(m*sizeof(char **));
        if (vartype==NULL) { ei_tilaa(); return(-1); }


        filen=4*m;
        for (i=0; i<m; ++i) vartype[i]=common_type;
        types=spfind("TYPES");
        if (types>=0)
            {
            strcpy(x,spb[types]);
            if (strlen(x)==1 && strchr("1248",*x)!=NULL)
                {
                *common_type=*x; filen=atoi(x)*m;
                for (i=0; i<m; ++i) varlen[i]=atoi(x);
                }
            else { filen=etsi_tyypit(x); if (filen<0) return(-1); }
            }

        extra_bytes=extra_fields=0;
        i=spfind("NEWSPACE");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,s,2);
            if (i>0) extra_bytes=atoi(s[0]);
            if (i>1) extra_fields=atoi(s[1]);
            }

        i=fi_create(word[3],filen+extra_bytes,m+extra_fields,m,0L,
               namelength,12,0,0,NULL,varname,varlen,vartype);

        if (i<0) return(-1);


        return(1);
        }

static SURVO_DATA dat2;
static int etsi_tyypit(char *s)
        {
        int i,j;
        char x[LLENGTH];
        int typelen,filen;

        i=data_open(s,&dat2);
        if (i<0) return(-1);

        if (dat2.type!=2)
            {
            sprintf(sbuf,"%s in TYPES specification is not a data file!",s);
            sur_print(sbuf); WAIT; return(-1);
            }
        typelen=13;
        typespace=muste_malloc(typelen*m);
        if (typespace==NULL) { ei_tilaa(); return(-1); }
        filen=0;
        for (i=0; i<m; ++i)
            {
            strcpy(x,varname[i]);
            for (j=0; j<dat2.m; ++j)
                {
                if (strncmp(x,dat2.varname[j],8)==0) break;
                }
            if (j==dat2.m) return(-1);
            varlen[i]=dat2.d2.varlen[j]; filen+=varlen[i];
            strcpy(x,dat2.d2.vartype[j]);
            j=strlen(x); if (j<typelen-1) strncat(x,space,typelen-1-j);
                         else x[typelen]=EOS;
            vartype[i]=typespace+i*typelen;
            strcpy(vartype[i],x);
            }
        return(filen);
        }

static int sampling()
        {
        int i,k;
        int j;
        double a;
        int prind_gap,prind_count;

        i=data_open(word[3],&dat); if (i<0) return(-1);
// printf("\ntype=%d|",dat.type); WAIT;
        if (dat.type!=2)
            {
            sprintf(sbuf,"\nData matrix/list with the same name %s already exists!",
                                   word[3]);
            sur_print(sbuf);
            sur_print("\nSelect another name!");
            WAIT; return(1);
            }

        if (dat.d2.n==0L)
            {
            dat.d2.n=dat.n=n;
            fi_rewind(&dat.d2);
            fi_puts(&dat.d2,&n,sizeof(int),22); // oli 22L
            }
        sprintf(sbuf,"\nSaving simulated data in %s ...",word[3]);
        sur_print(sbuf);
        prind_gap=100; prind_count=0;
        for (j=0; j<n; ++j)
            {
            ++prind_count;
            if (prind_count==prind_gap)
                {
                prind_count=0;
                if (sur_kbhit()) { sur_getch(); if (sur_kbhit()) sur_getch(); prind=1-prind; }
                }

            if (prind) { sprintf(sbuf,"%d ",j+1); sur_print(sbuf); }

            for (i=0; i<m; ++i)
                {
                e[i]=normal_dev();
                }

            for (k=0; k<m; ++k)
                {
                a=Y[k]+sis_tulo(X+k,e,m,1,m);
                data_save(&dat,j+1L,k,a);
                }

            }
        data_close(&dat); // 24.7.2011
        return(1);
        }

static int load_X(char *nimi)
        {
        int i;

        i=matrix_load(nimi,&X,&mX,&nX,&rlabX,&clabX,&lrX,&lcX,&typeX,exprX);
        return(i);
        }

static int load_Y(char *nimi)
        {
        int i;

        i=matrix_load(nimi,&Y,&mY,&nY,&rlabY,&clabY,&lrY,&lcY,&typeY,exprY);
        return(i);
        }

static int save_X(char *nimi)
        {
        int i;

        i=matrix_save(nimi,X,mX,nX,rlabX,clabX,8,8,-1,exprX,0,0);
        return(i);
        }

static int save_Y(char *nimi)
        {
        int i;

        i=matrix_save(nimi,Y,mY,nY,rlabY,clabY,8,8,-1,exprY,0,0);
        return(i);
        }

static int text_labels(char *lab,int n,char *text)
        {
        char *t,*p;
        int pit;
        char label[32];
        int i,j;
        int len;

        len=8;
        if (*text=='"') t=text+1; else t=text;
        p=strchr(t,'"'); if (p!=NULL) *p=EOS;
        pit=strlen(t);
        for (i=0; i<n*len; ++i) lab[i]=' ';
        for (i=0; i<n; ++i)
            {
            snprintf(label,32,"%s%d",t,i+1);
            for (j=0; j<len; ++j)
                {
                if (label[j]==EOS) break;
                lab[i*len+j]=label[j];
                }
            }
        return(1);
        }

static int ei_tilaa()
        {
        PR_EBLD;
        sur_print("\nNot enough space for matrices");
        WAIT; PR_ENRM; erun=0;
        return(1);
        }

