/* _cluster.c 2.2.1989/SM (30.4.1994) (21.8.1997)
*/
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
// #include <process.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

static SURVO_DATA d;
static int tulosrivi;
static char aineisto[LLENGTH];
static double *T;
static double *xx;
static double *v1,*v2;
static double *S;
static double *N1;
static double *H1;
static double *H2;
static double *Q;
static int ng,m,ng2,mn;
static int n_saved;
static int ivar, gvar, *gvar2;
static double *lambda2;
static int *freq,*ii;
static FILE *hav;
static int n;
static int hav_len;
static int n_saved_len;
static double f2;
static int maxiter=1;
static int it;
static char tempfile[LLENGTH];
static int ivar_init=0;
static int hav_muistissa=0;
static int first_line;
static int prind=0;
static int n_show;

static int varaa_tilat();
static int not_enough_memory();
static int lue_havainnot();
static int havainnot_muistiin();
static int hav_read1(int jj,int *pgroup);
static int hav_read2(int jj,int *pj);
static int hav_read3(int jj,double *y);
static int hav_read4(int jj,int k,int *pgroup);
static int hav_write3(int jj,double *y);
static int hav_write4(int jj,int k,int *pgroup);
static int ortogonalisoi();
static int tulosta();
static int init_gr();
static int vertaa_muihin();
static int kopioi_gr(int k);
static int eoutput(char *rivi);
static int init_tilat();
static int iteroi();
static int init_obs(int gr);
static int sift(int gr,int d);
static int shift(int gr,int d);
static int alustava_luokittelu();
static int kirjoita_lauseke(int outvar,char *lauseke);
static int uusi_nimi(int i,char *s);
static int rand_init();
static double rand_uniform();
static double clu_uniform(double x);


/****************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "PRIND", "TRIALS", "TEMPFILE", "GROUPS", "RND", "SEED",
                                          "!" };
char **specs=specs0;
*******************************/

/**************
main(argc,argv)
int argc; char *argv[];
*******************/

void muste_cluster(char *argv)
        {
        int i,k;
        double a;
        char ch;

//      if (argc==1) return;
        s_init(argv);

        if (g<2)
            {
            sur_print("\nUsage: CLUSTER <SURVO_data>,<output_line>");
            WAIT; return;
            }
        tulosrivi=0;
        if (g>2)
            {
            tulosrivi=edline2(word[2],1,1);
            if (tulosrivi==0) return;
            }

        strcpy(aineisto,word[1]);
        i=data_open(aineisto,&d); if (i<0) return;
        i=sp_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;
        scales(&d);
        i=conditions(&d); if (i<0) return;

        gvar=activated(&d,'G');
        if (gvar<0)
            {
            sur_print("\nNo grouping variable (activated by 'G') given!");
            WAIT; return;
            }

        ivar=-1; ivar=activated(&d,'I');

        i=spfind("TRIALS");
        if (i>=0) maxiter=atoi(spb[i]);

        i=rand_init(); if (i<0) return;   /* 30.4.1994 */

        i=spfind("TEMPFILE");
        if (i>=0) strcpy(tempfile,spb[i]);
        else { strcpy(tempfile,etmpd); strcat(tempfile,"SURVO.CLU"); }

        i=spfind("PRIND");
        if (i>=0 && atoi(spb[i])>0) prind=1;

        data_load(&d,1L,gvar,&a);
        i=data_save(&d,1L,gvar,a);
        if (i<0) return;

        gvar2=(int *)muste_malloc(d.m_act*sizeof(int));
        if (gvar2==NULL) { not_enough_memory(); return; }

        k=0; n_saved=0; m=0;
        for (i=0; i<d.m_act; ++i)
            {
            ch=d.vartype[d.v[i]][1];
            if (ch=='G')
                {
                ++k;
                gvar2[n_saved]=d.v[i];    /* gvar=gvar2[0] */
                ++n_saved; continue;
                }
            if (ch=='I') { ++k; continue; }
            d.v[m++]=d.v[i];
            }
/*
printf("\nivar=%d gvar=%d m=%d\n",ivar,gvar,m); getch();
for (i=0; i<m; ++i) printf(" %d",d.v[i]); getch();
printf("\n"); for (i=0; i<n_saved; ++i) printf(" %d",gvar2[i]); getch();
*/

        i=spfind("GROUPS");
        if (i<0) ng=2; else ng=atoi(spb[i]);
        if (ng<2) ng=2;
        ng2=ng+2;
        mn=m; if (mn<ng) mn=ng;

        first_line=r+1; if (r+n_saved>r3) first_line=1;
        n_show=n_saved; if (n_show>r3) n_show=r3;

        i=varaa_tilat(); if (i<0) return;

        i=lue_havainnot(); if (i<0) return;
        hav_muistissa=havainnot_muistiin();
        ortogonalisoi();
        if (ivar_init) alustava_luokittelu();
        LOCATE(first_line,1);
        SCROLL_UP(first_line,r3+1,r3);
        sur_print("\nCluster analysis: Iteration 1:");
        while (sur_kbhit()) sur_getch();
        it=0;
        while (1)
            {
            while (1)
                {
                if (it) init_gr();
                i=init_tilat();
                if (i>=0) break;
                if (maxiter==1) return;
                }
            iteroi();
            ++it;
            if (maxiter>1) vertaa_muihin();
            if (it==maxiter) break;
            LOCATE(first_line,1);
            sprintf(sbuf,"\nIteration %d (Cluster analysis)",it);
            sur_print(sbuf);
            for (i=0; i<n_show; ++i)
               {
               if (freq[i]==0) break;
               sprintf(sbuf,"\n%d %g %d        ",i+1,lambda2[i],freq[i]); sur_print(sbuf);
               }
            if (sur_kbhit())
                {
                i=sur_getch(); if (i=='.') break;
                }
            }
        tulosta();

        data_close(&d);
        sur_delete(tempfile);
        s_end(argv);
        }

static int varaa_tilat()
        {

        T=(double *)muste_malloc(m*m*sizeof(double));
        if (T==NULL) { not_enough_memory(); return(-1); }
        xx=(double *)muste_malloc(m*sizeof(double));
        if (xx==NULL) { not_enough_memory(); return(-1); }
        v1=(double *)muste_malloc(mn*sizeof(double));
        if (v1==NULL) { not_enough_memory(); return(-1); }
        v2=(double *)muste_malloc(mn*sizeof(double));
        if (v2==NULL) { not_enough_memory(); return(-1); }
        S=(double *)muste_malloc(ng*m*sizeof(double));
        if (S==NULL) { not_enough_memory(); return(-1); }
        N1=(double *)muste_malloc(ng*sizeof(double));
        if (N1==NULL) { not_enough_memory(); return(-1); }
        H1=(double *)muste_malloc(ng*ng*sizeof(double));
        if (H1==NULL) { not_enough_memory(); return(-1); }
        H2=(double *)muste_malloc(ng*ng*sizeof(double));
        if (H2==NULL) { not_enough_memory(); return(-1); }
        Q=(double *)muste_malloc(ng2*ng2*sizeof(double));
        if (Q==NULL) { not_enough_memory(); return(-1); }
        lambda2=(double *)muste_malloc(n_saved*sizeof(double));
        if (lambda2==NULL) { not_enough_memory(); return(-1); }
        freq=(int *)muste_malloc(n_saved*sizeof(int));
        if (freq==NULL) { not_enough_memory(); return(-1); }
        ii=(int *)muste_malloc(n_saved*sizeof(int));
        if (ii==NULL) { not_enough_memory(); return(-1); }

        return(1);
        }

static int not_enough_memory()
        { sur_print("\nNot enough memory!"); WAIT; return(1); }

static int lue_havainnot()
        {
        int j;
        int i,h;
        double y;
        int miss;
        int group;
        char *p;

        hav=muste_fopen(tempfile,"wb");
        if (hav==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s for temporary data!",tempfile);
            sur_print(sbuf); WAIT; return(-1);
            }

        sur_print("\nReading observations... ");
        for (i=0; i<m*m; ++i) T[i]=0.0;
        n=0L;
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            miss=0;
            for (i=0; i<m; ++i)
                {
                data_load(&d,j,d.v[i],&y);
                if (y==MISSING8) { miss=1; break; }
                xx[i]=y;
                }
            if (miss) continue;

            if (ivar>=0)
                {
                data_load(&d,j,ivar,&y);
                if (y==MISSING8) continue;
                group=y;

                if (group<=0 || group>ng)
                    { ++ivar_init; group=0; }
                }
            else
                group=ng*rand_uniform()+1;

            p=(char *)&group;
            for (h=0; h<sizeof(int); ++h) putc((int)p[h],hav);
            group=0; p=(char *)&group;
            for (i=0; i<n_saved; ++i)
                for (h=0; h<sizeof(int); ++h) putc((int)p[h],hav);

            p=(char *)&j;
            for (h=0; h<sizeof(int); ++h) putc((int)p[h],hav);
            for (i=0; i<m; ++i)
                {
                p=(char *)&xx[i];
                for (h=0; h<sizeof(double); ++h) putc((int)p[h],hav);
                }
            ++n;

            for (i=0; i<m; ++i)
                for (h=0; h<=i; ++h) T[i+m*h]+=xx[i]*xx[h];

            if (sur_kbhit())
                {
                i=sur_getch(); prind=1-prind;
                }
            if (prind) { sprintf(sbuf,"%d ",j); sur_print(sbuf); }
            }

        n_saved_len=n_saved*sizeof(int);
        hav_len=sizeof(int)+n_saved_len+sizeof(int)+m*sizeof(double);
        muste_fclose(hav);

        for (i=0; i<m; ++i)
            for (h=0; h<i; ++h) T[h+m*i]=T[i+m*h];

        for (i=0; i<n_saved; ++i) { lambda2[i]=1e100; freq[i]=0; }

        return(1);
        }

static short *s_group;  /* n */
static double *s_obs;   /* m*n */
static short *s_groups; /* n_saved*n */

static int havainnot_muistiin()
        {
        int j;
        int i,k;
        int l;
        double a;
     // char *p;

        s_obs=(double *)muste_malloc(m*n*sizeof(double));
        if (s_obs==NULL) return(0);
        s_group=(short *)muste_malloc(n*sizeof(short));
        if (s_group==NULL) return(0);
        s_groups=(short *)muste_malloc(n_saved*n*sizeof(double));
        if (s_groups==NULL) return(0);

        hav=muste_fopen(tempfile,"r+b");
        for (j=0L; j<n; ++j)
            {
            fread(&k,sizeof(int),1,hav); s_group[j]=k;

            for (i=0; i<n_saved; ++i)
                {
                fread(&k,sizeof(int),1,hav);
                s_groups[j*n_saved+i]=k;
                }
            fread(&l,sizeof(int),1,hav); /* iter. ei tarv. hav.nroa */
            for (i=0; i<m; ++i)
                {
                fread(&a,sizeof(double),1,hav);
                s_obs[j*m+i]=a;
                }
            }
        muste_fclose(hav);
        return(1);
        }

static int hav_read1(int jj,int *pgroup)
        {
        char *p;
        int i;

        if (hav_muistissa)
            { *pgroup=s_group[jj]; return(1); }
        muste_fseek(hav,jj*(int)hav_len,SEEK_SET);
        p=(char *)pgroup;
        for (i=0; i<sizeof(int); ++i) p[i]=(char)getc(hav);
        return(1);
        }

static int hav_read2(int jj,int *pj)
        {
        char *p;
        int i;

        muste_fseek(hav,jj*(int)hav_len+(int)sizeof(int)+(int)n_saved_len,SEEK_SET);
        p=(char *)pj;
        for (i=0; i<sizeof(int); ++i) p[i]=(char)getc(hav);
        return(1);
        }

static int hav_read3(int jj,double *y)
        {
        char *p;
        int i,h;

        if (hav_muistissa)
            {
            for (i=0; i<m; ++i) y[i]=s_obs[jj*m+i];
            return(1);
            }
        muste_fseek(hav,jj*(int)hav_len+(int)sizeof(int)+(int)n_saved_len+(int)sizeof(int),SEEK_SET);
        for (i=0; i<m; ++i)
            {
            p=(char *)&y[i];
            for (h=0; h<sizeof(double); ++h) p[h]=(char)getc(hav);
            }
        return(1);
        }

static int hav_read4(int jj,int k,int *pgroup)
        {
        char *p;
        int i;

        if (hav_muistissa)
            { *pgroup=s_groups[jj*n_saved+k]; return(1); }
        muste_fseek(hav,jj*(int)hav_len+(int)((k+1)*sizeof(int)),SEEK_SET);
        p=(char *)pgroup;
        for (i=0; i<sizeof(int); ++i) p[i]=(char)getc(hav);
        return(1);
        }

static int hav_write1(int jj,int *pgroup)
        {
        char *p;
        int i;

        if (hav_muistissa)
            { s_group[jj]=*pgroup; return(1); }
        muste_fseek(hav,jj*(int)hav_len,SEEK_SET);
        p=(char *)pgroup;
        for (i=0; i<sizeof(int); ++i) putc((int)p[i],hav);
        return(1);
        }

static int hav_write3(int jj,double *y)
        {
        char *p;
        int i,h;

        if (hav_muistissa)
            {
            for (i=0; i<m; ++i) s_obs[jj*m+i]=y[i];
            return(1);
            }
        muste_fseek(hav,jj*(int)hav_len+(int)sizeof(int)+(int)n_saved_len+(int)sizeof(int),SEEK_SET);
        for (i=0; i<m; ++i)
            {
            p=(char *)&y[i];
            for (h=0; h<sizeof(double); ++h) putc((int)p[h],hav);
            }
        return(1);
        }

static int hav_write4(int jj,int k,int *pgroup)
        {
        char *p;
        int i;

        if (hav_muistissa)
            { s_groups[jj*n_saved+k]=*pgroup; return(1); }

        muste_fseek(hav,jj*(int)hav_len+(int)((k+1)*sizeof(int)),SEEK_SET);
        p=(char *)pgroup;
        for (i=0; i<sizeof(int); ++i) putc((int)p[i],hav);
        return(1);
        }

static int ortogonalisoi()
        {
        double eps,tol;
        int i,j;
        int jj;

/*      sur_print("\nOrthogonalizing data...");  */
        eps=1e-16; tol=1e-300/eps;
        mat_tred2(v1,v2,T,m,tol);
        mat_tql2(v1,v2,T,m,eps,30);
        for (j=0; j<m; ++j)
            {
            eps=1/sqrt(v1[j]);
            for (i=0; i<m; ++i) T[i+m*j]*=eps;
            }

        hav=muste_fopen(tempfile,"r+b");
        for (jj=0L; jj<n; ++jj)
            {
            hav_read3(jj,xx);
            mat_mlt(v2,xx,T,1,m,m);
            hav_write3(jj,v2);
            }
        muste_fclose(hav);
        return(1);
        }

static int tulosta()
        {
        int i,k,imin,h;
        int j,jj;
        int gr;
        int n_used;
        double min;
        char rivi[LLENGTH];
        char x[LLENGTH];

        hav=muste_fopen(tempfile,"r+b");
        output_open(eout);

        sprintf(rivi,"Stepwise cluster analysis by Wilks' Lambda criterion");
        eoutput(rivi);
        sprintf(rivi,"Data %s  N=%d",aineisto,n); eoutput(rivi);

        k=sprintf(rivi,"Variables: ");
        for (i=0; i<m; ++i)
            {
            strcpy(x,d.varname[d.v[i]]);
            h=strlen(x); while (h && x[h-1]==' ') x[--h]=EOS;
            k+=sprintf(rivi+k,"%s",x);
            if (i<m-1) k+=sprintf(rivi+k,", ");
            if (k>c3-10) { eoutput(rivi); k=0; }
            }
        if (k) eoutput(rivi);

        n_used=0;
        for (i=0; i<n_saved; ++i)
            {
            if (freq[i]==0) break;
            ++n_used;
            }

        if (it==1)
            {
            sprintf(rivi," Lambda=%g  Clustering saved in %s",f2,d.varname[gvar]);
            eoutput(rivi);
            }
        if (it>1)
            {
            sprintf(rivi,"Best clusterings found in %d trials are saved as follows:",it);
            eoutput(rivi);
            eoutput(" Lambda          freq  Grouping var");
            imin=0;
            for (i=0; i<n_used; ++i)
                {
                min=1e100;
                for (k=0; k<n_used; ++k)
                    {
                    if (lambda2[k]<min) { imin=k; min=lambda2[k]; }
                    }
                lambda2[imin]+=1000.0; ii[i]=imin;
                         /* ennen 7.11.89 =1e100 */
                sprintf(rivi,"%10.10f %6d   %s",min,freq[imin],d.varname[gvar2[i]]);
                eoutput(rivi);
                }
            }

        sur_print("\nSaving clusterings...");
        for (jj=0L; jj<n; ++jj)
            {
            if (sur_kbhit())
                {
                i=sur_getch(); prind=1-prind;
                }
            if (prind) { sprintf(sbuf," %d",jj+1); sur_print(sbuf); }
            hav_read2(jj,&j);
            if (it==1)
                {
                hav_read1(jj,&gr);
                data_save(&d,j,gvar,(double)gr);
                }
            else
                {
                for (i=0; i<n_used; ++i)
                    {
                    hav_read4(jj,ii[i],&gr);
                    data_save(&d,j,gvar2[i],(double)gr);
                    }
                }
            }


        if (it==1)
            {
            sprintf(x,"clustering in %d groups: Lambda=%g",ng,f2);
            kirjoita_lauseke(gvar,x);
            }
        else
            {
            for (i=0; i<n_used; ++i)
                {
                sprintf(x,"clustering %d in %d groups: Lambda=%g",i+1,ng,lambda2[ii[i]]-1000.0);
                kirjoita_lauseke(gvar2[i],x);
                }
            }       /* 7.11.89 */


        output_close(eout);
        muste_fclose(hav);
        return(1);
        }

static int init_gr()
        {
        int jj;
        int group;

        hav=muste_fopen(tempfile,"r+b");
        for (jj=0L; jj<n; ++jj)
            {
            group=ng*rand_uniform()+1;
            hav_write1(jj,&group);
            }
        muste_fclose(hav);
        return(1);
        }

static int vertaa_muihin()
        {
        int i,imax;
        double max;
        int n_used;

        for (i=0; i<n_saved; ++i)
            {
            if (freq[i]==0) break;
            if (fabs(f2-lambda2[i])<1e-8) { ++freq[i]; return(1); }
            }
        max=-1e10; imax=-1;
        n_used=0;
        for (i=0; i<n_saved; ++i)
            {
            if (freq[i]==0) break;
            ++n_used;
            if (lambda2[i]>max) { imax=i; max=lambda2[i]; }
            }
        if (imax==-1)
            {
            kopioi_gr(0); lambda2[0]=f2; freq[0]=1; return(1);
            }
        if (n_used<n_saved)
            {
            i=n_used; kopioi_gr(i); lambda2[i]=f2; freq[i]=1; return(1);
            }
        if (f2<max)
            {
            kopioi_gr(imax);
            lambda2[imax]=f2;
            freq[imax]=1; return(1);
            }
        return(1);
        }

static int kopioi_gr(int k)
        {
        int jj;
        int group;

        hav=muste_fopen(tempfile,"r+b");
        for (jj=0L; jj<n; ++jj)
            {
            hav_read1(jj,&group);
            hav_write4(jj,k,&group);
            }
        muste_fclose(hav);
        return(1);
        }

static int eoutput(char *rivi)
        {
        output_line(rivi,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

/* clu2.c 3.2.1989/SM (7.2.1989)
*/

static double rr,vss;


static int init_tilat()
        {
        int jj;
        int i,j,gr;
     // double det;

/*      sur_print("\nInitializing work matrices...");  */
        hav=muste_fopen(tempfile,"r+b");
        for (i=0; i<ng; ++i)
            {
            N1[i]=0.0;
            for (j=0; j<m; ++j) S[i+ng*j]=0.0;
            }

        for (jj=0L; jj<n; ++jj)
            {
            hav_read1(jj,&gr); --gr;
            hav_read3(jj,xx);
            ++N1[gr];
            for (i=0; i<m; ++i) S[gr+ng*i]+=xx[i];
            }

        for (i=0; i<ng; ++i)
            {
            if (N1[i]==0.0)
                {
                if (maxiter>1) { muste_fclose(hav); return(-1); }
                sprintf(sbuf,"\nNo cases in initial setting for group %d",i+1);
                sur_print(sbuf); WAIT; return(-1);
                }
            }

        mat_mmt(H1,S,ng,m);
        for (i=0; i<ng; ++i)
            for (j=0; j<ng; ++j)
                {
                if (i==j) H1[i+ng*j]=N1[i]-H1[i+ng*j];
                else      H1[i+ng*j]=-H1[i+ng*j];
                }
        mat_inv(H2,H1,ng,&f2);
        for (i=0; i<ng; ++i) f2/=N1[i];


/* for (i=0; i<ng2*ng2; ++i) Q[i]=0.0;     tarpeeton */


        for (i=0; i<ng; ++i)
            for (j=0; j<ng; ++j)
                Q[i+ng2*j]=H2[i+ng*j];

/*  sprintf(sbuf,"\nlambda=%g",f2); sur_print(sbuf); */
/* printf("\nH2:"); matprint(H2,ng,ng);   */

        muste_fclose(hav);
        return(1);
        }

static int iteroi()
        {
        int jj,nj;
        int i,gr,d;


        hav=muste_fopen(tempfile,"r+b");
        jj=0L; nj=0L;
        while (1)
            {
/*          sprintf(sbuf," %ld",jj+1); sur_print(sbuf); */
            hav_read1(jj,&gr); --gr;
            hav_read3(jj,xx);

            init_obs(gr);
            for (d=0; d<ng; ++d)
                {
                if (d==gr) continue;
                i=sift(gr,d);
                if (i) break;
                }
            if (d==ng) { ++nj; if (nj>=n) break; }
            else
                {
                nj=0L;
                shift(gr,d);
                ++d;
                hav_write1(jj,&d);
/*              sprintf(sbuf,"\nL=%g shift %d -> %d ",f2,gr+1,d); sur_print(sbuf); */
                }
            ++jj; if (jj==n) jj=0L;
            }
        muste_fclose(hav);
        return(1);
        }

static int init_obs(int gr)
        {
        int i,j;
        double a;

        for (i=0; i<ng; ++i)
            {
            if (i==gr) a=1.0; else a=0.0;
            for (j=0; j<m; ++j)
                a-=xx[j]*S[i+ng*j];
            v1[i]=a;  /* m(s) */
            }
        rr=0.0;
        for (i=0; i<m; ++i) rr+=xx[i]*xx[i];

        for (i=0; i<ng; ++i)
            {
            a=0.0;
            for (j=0; j<ng; ++j)
                a+=v1[j]*Q[i+ng2*j];
            v2[i]=a;  /* b(s) */
            }
        vss=0.0;
        for (i=0; i<ng; ++i) vss+=v1[i]*v2[i];
/* matprint(Q,ng2,ng2); */
        return(1);
        }

static int sift(int gr,int d)
        {
        double f;
        double a;

        if (N1[gr]==1.0) return(0);
        a=v2[d]-v2[gr]+1;
        f=f2*N1[gr]*N1[d]/(N1[gr]-1.0)/(N1[d]+1.0)*
          (a*a-(vss+rr-1)*(Q[gr+ng2*gr]+Q[d+ng2*d]-2*Q[d+ng2*gr]));


        if (f<f2) { f2=f; return(1); }
        return(0);
        }

static int shift(int gr,int d)
        {
        int i;
        double vsd,vdd;

        for (i=0; i<ng; ++i)
            { Q[ng+ng2*i]=v2[i]; Q[i+ng2*ng]=v2[i]; }
        Q[ng+ng2*ng]=-1.0+rr+vss;
        for (i=0; i<ng; ++i) v1[i]=v2[i]-Q[i+ng2*gr]+Q[i+ng2*d];  /* b(d) */
        vsd=vss-v2[gr]+v2[d];
        vdd=vss-2*(v2[gr]-v2[d])+Q[gr+ng2*gr]+Q[d+ng2*d]-2*Q[gr+ng2*d];
        for (i=0; i<ng; ++i)
            { Q[ng+1+ng2*i]=v1[i]; Q[i+ng2*(ng+1)]=v1[i]; }
        Q[ng+1+ng2*ng]=Q[ng+ng2*(ng+1)]=rr+vsd;
        Q[ng2*ng2-1]=1+rr+vdd;

        mat_p(Q,ng2,ng2-1);
        mat_p(Q,ng2,ng2-2);

        --N1[gr]; ++N1[d];
        for (i=0; i<m; ++i)
            {
            S[gr+ng*i]-=xx[i];
            S[d+ng*i]+=xx[i];
            }

/*
printf("\nuusi Q:");
matprint(Q,ng2,ng2);
*/      return(1);
        }

static int alustava_luokittelu()
        {
        int jj,jj2;
        double min,dist,a;
        int gr_min,gr,i;

        sur_print("\nPreliminary classification...");
        hav=muste_fopen(tempfile,"r+b");
        for (jj=0L; jj<n; ++jj)
            {
            hav_read1(jj,&gr);
            if (gr>0) continue;

            min=1e100;
            hav_read3(jj,v1);
            for (jj2=0L; jj2<n; ++jj2)
                {
                hav_read1(jj2,&gr);
                if (gr==0) continue;
                hav_read3(jj2,xx);
                dist=0.0;
                for (i=0; i<m; ++i) { a=v1[i]-xx[i]; dist+=a*a; }
                if (dist<min) { gr_min=gr; min=dist; }
                }
            hav_write1(jj,&gr_min);
/*          sprintf(sbuf,"\n%ld --> %d",jj,gr_min); sur_print(sbuf); */
            }
        muste_fclose(hav);
        return(1);
        }

/* cluni.c 4.10.1989/SM (7.11.1989)
   muunnelma varni.c
*/

static int kirjoita_lauseke(int outvar,char *lauseke)
        {
        if (d.type!=2) return(1);
        uusi_nimi(outvar,lauseke);
        return(1);
        }

#define EQ '\176'

static int uusi_nimi(int i,char *s)
        {
        char x[LLENGTH];
        int k,len,h;


        len=d.d2.l; if (len<10) return(1);
        fi_rewind(&(d.d2));
        fi_gets(&(d.d2),x,d.d2.l,(int)(d.d2.var+(int)i*((int)len+(int)d.d2.extra)+(int)d.d2.extra));
        x[len]=EOS;

        k=8; while (x[k]==' ' && k<len) ++k;
        if (k==len || x[k]==EQ)
            {
            if (x[k]==EQ) for (h=k; h<len; ++h) x[h]=' ';
            x[9]=EQ;
            h=0; while (h<strlen(s) && h+10<len) { x[h+10]=s[h]; ++h; }
        fi_rewind(&(d.d2));
        fi_puts(&(d.d2),x,d.d2.l,(int)(d.d2.var+(int)i*((int)len+(int)d.d2.extra)+(int)d.d2.extra));
            }
        return(1);
        }

/* rand_ini.c 30.4.1994/SM  (30.4.1994)
*/

extern char **spb;

static int type; /* 0=rnd 1=rand 2=urand */
static int seed;

static int rand_init()
        {
        int i;
        char s[LLENGTH];
        char *p;

        i=spfind("RND");
        if (i<0) i=spfind("SEED");
        if (i<0) { type=0; seed=0L; return(1); }

        strcpy(s,spb[i]);
        p=strchr(s,'(');
        if (p==NULL) { type=0; seed=atol(s); return(1); }
        seed=atol(p+1);
        if (muste_strnicmp(s,"rnd(",4)==0) { type=0; return(1); }
        if (muste_strnicmp(s,"rand(",5)==0) { type=1; return(1); }
        if (muste_strnicmp(s,"urand(",6)==0) { type=2; return(1); }

        sprintf(sbuf,"\nUnknown random number generator %s !",s);
        sur_print(sbuf); WAIT;
        return(-1);
        }

static double rand_uniform()
        {
        static int next=0;
        extern double sur_rand();
        extern double sur_urand();
  //    extern double clu_uniform();

        switch (type)
            {
          case 0:
            return(clu_uniform((double)seed));
          case 1:
            if (!next)
               { sur_rand_seed((unsigned int)seed); next=1; }
            return (sur_rand());
          case 2:
            if (!next)
               { sur_urand_seed((unsigned int)seed); next=1; }
            return (sur_urand());
            }
        return(0.0);
        }

#define CLU_RND (double)rand()/32768.0

static double clu_uniform(double x)
        {
        static unsigned int u=0;
        if (x!=1.0) { srand((unsigned int)(x+u++)); rand(); }
        return((double)(CLU_RND+1e-6));
        }

