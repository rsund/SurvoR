#include "muste.h"
/* !clu.c 21.2.1996 / Fredrik Åberg (10.5.1996)
   Converted to 32bit SURVO 98  4.12.1997/kv
   Converted to Win32 SURVO MM  16.8.2000/kv
   Converted for Muste 25.8.2011/KV (27.8.2011/RS)
*/

#define MAXSPACE 16200L
#define PUUDATA "#TREE#"

#define plats(a,b) ( (a)>(b) ? ((dist[a])[b]) : ((dist[b])[a]) )
#define NIMIPIT 16    /* Pituus piirrettävässä datassa */

#define SINGLE_LINK         1
#define COMPLETE_LINK       2
#define GROUP_AVERAGE       3
#define WEIGHTED_AVERAGE    4
#define UNWEIGHTED_CENTROID 5
#define WEIGHTED_CENTROID   6
#define MIN_VARIANCE        7

#define SQREUCLIDIAN  1
#define EUCLIDIAN     2
#define CITYBLOCK     3
#define CANBERRA      4

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <float.h>
// #include <conio.h>
// #include <malloc.h>
#include <math.h>
#include <memory.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"
/* #include <survomat.h> */     /* (does not exist in 98) */

static char *methodname[]={"","Single_linkage","Complete_linkage","Group_average",
                              "Weighted_average","Unweighted_centroid",
                              "Weighted_centroid","Minimum_variance_(Wards_method)"};
static char *distancename[]={"","Squared_Euclidian","Euclidian","City_Block",
                                "Canberra_metric"};

static SURVO_DATA d;

static int results_line;

static FILE *temp;
static FILE *temp_dist;

static float **dist;    /* distans matris */
static float *zz;      /* data */
static int ind_zz;
static float *xx;      /* havainto vektori */
static double *sumx;
static double *sumx2;
static float *xx2;      /* havainto vektori */
static char *nimet;

static float *ss;      /*  minsta värdet */
static int *ii;        /* lista 0..n-1 för rad med minsta värdet */
static int *jj;
static int *il;
static int *jl;
static int *last;
static int *next;
static int *aktiva;
static int *nn;        /* hur många det är i gruppen */
static float *mittpunkt;
static int *gjord;
static float maxss=0;
static char treedata[LNAME];
static char distfile[LNAME];
static char weightmat[LNAME];

static char *rlab,*clab;    /* om word[1] är en matris */
static int lr,lc;              /* eller om distance matrisen sparas */
static double *aa;         /* matris om word[1] är matris */
static int rdim,cdim;
static int type;
static char expr[129];

static char *rlabwei,*clabwei;   /*   behövs om weighting matris ges */
static int lrwei,lcwei;              /* eller om distance matrisen sparas */
static double *aawei;         /* matris om word[1] är matris */
static int rdimwei,cdimwei;
static int typewei;
static char exprwei[129];

static int n;
static int m;        /* antal aktiverade variabler */
static int met=0;
static int dis=0;
static int ps=0;
static int landscape=0;
static int savedist=0;
static int distfiletxt=0;
static int equalize=0;
static int weights=0;
static int weightwarning=0;
static int namevar;
static int w1mat=0;
static int warning_names=0;
static int warning_missing=0;
static int label_obs=0;

static int ismatrix(char *);
static float distance(float,float,int);
static int mat_read_distance(void);
static void equal(float *);
static void weight(float *);
static int mem_read_distance(void);
static int disk_read_distance(void);
static int disk2mem_dist(void);
static float dd(int,int,int,int);
static void do_cluster(void);
static int tee_data(void);
static int is_path(char *);
static int mojena(float);
static int maxstep(void);
static void printout(void);
static int talleta_data(void);
static int siirto_zz(void);
static int lue(float *,int);
static int lue_nimet(void);
static void vika_samatnimet(int,int);
static int varaa_tilat(void);
static int varaa_nimet(void);
static int alloc_dist(void);
static void free_dist(void);
static int nollaa(int *,int);
static int varaa2_tilat(void);
static int varaa3_tilat(void);
static int ei_tilaa(int,int);
static void print_line(char *);
static int metstring(char *);
static int disstring(char *);
static int samaalku(char *,char *);
static int samaloppu(char *,char *);
static double pyoristaylos(double);
static int poistapaate(char *);
static void raja_rivi(void);
static void teepskaavio(void);
static void teegplotkaavio(void);
static int txt_save_distance_matrix(void);
static int alloc_save_dist(void);
static int mat_save_distance_matrix(void);
static int lue_havainto_nimi(char *,int);
static void free_input_matrix(void);
static void int_to_string(int,char *,char *);


// 4.6.2004:
/**************************************
char *spec_clu[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                   "METHOD", "DISTANCE", "RESULTS", "TREEDATA", "PLOT",
                   "SAVEDIST", "EQUALIZE", "SCALING", "WEIGHTS", "!" };
char **specs=spec_clu;
***************************************/

void muste_hcluster(char *argv) {
        int i,k,l;

        s_init(argv);
        if ( g<2 || *word[1]=='?' ) {
            sprintf(sbuf, "\nSee: HCLUSTER?"); sur_print(sbuf); WAIT;
            return;
        }

        results_line=0;
 met=0;
 dis=0;
 ps=0;
 landscape=0;
 savedist=0;
 distfiletxt=0;
 equalize=0;
 weights=0;
 weightwarning=0;
 w1mat=0;
 warning_names=0;
 warning_missing=0;
 label_obs=0;

zz=NULL;
rlab=NULL;
clab=NULL;
aa=NULL;

        if (g>2) {
                results_line=edline2(word[2],1,1);
                if (results_line==0) return;
        }
        if ( spec_init(r1+r-1) < 0 ) return;
                /* spec_init gives also an error message! */
        if (!ismatrix(word[1]) ) {
                if ( data_open(word[1],&d) < 0 ) return;
                if ( mask(&d) < 0 ) return;
                if ( conditions(&d) < 0 ) return;  /* permitted only once */
                namevar=activated(&d,'L');
                if (namevar<0) {
                        if (d.vartype[ d.v[0] ][0] == 'S') {
                                namevar=d.v[0];
                                d.vartype[ d.v[0] ][1]='L';
                        }
                        else label_obs=1;
                }
                else if ( d.vartype[namevar][0] != 'S' ) label_obs=1;

                m=d.m_act;         /* hoppar över variabel aktiverad med L */
                for (i=0,k=0,l=m; i<l; ++i) {
                        if (d.vartype[ d.v[i] ][1]=='L')
                                --m;
                        else
                                d.v[k++]=d.v[i];
                }
        }
        if ( (i=spfind("METHOD")) >=0 ) {
                met=atoi(spb[i]);
                if ( met < 1 || met > 7 ) met=metstring(spb[i]);
                if ( met < 1 || met > 7 ) met=1;

        }
        if (met==0) met=1;

        if ( (i=spfind("DISTANCE")) >=0 ) {
                dis=atoi(spb[i]);
                if (dis<1 || dis>4) dis=disstring(spb[i]);
                if ( dis < 1 || dis > 4 ) dis=1;
        }
        if (dis==0) dis=1;

        if ( (i=spfind("RESULTS")) >=0 ) results=atoi(spb[i]);

        strcpy(treedata,PUUDATA);
        if ( (i=spfind("TREEDATA")) >=0 ) {
                strcpy(treedata,spb[i]);
                poistapaate(treedata);
        }

        if ( (i=spfind("PLOT")) >=0 ) {
                if ( samaalku(spb[i],"PS") || samaalku(spb[i],"POST") ) {
                        ps=1;
                        for(;spb[i];spb[i]++) {
                                if (*spb[i]==',') {
                                        if ( samaalku(spb[i]+1,"LAND") )
                                                landscape=1;
                                        break;
                                }
                                if (*spb[i]==EOS) break; // 1.02.003/KV
                        }
                }
        }

        if ( (i=spfind("SAVEDIST")) >= 0 ) {
                strcpy(distfile,spb[i]);
                if ( samaloppu(distfile,"TXT") ) distfiletxt=1;
                poistapaate(distfile);
                savedist=1;
        }

        if ( spfind("EQUALIZE") >=0 ) {
                equalize=1;
        }

        if ( spfind("SCALING") >=0 ) {
                equalize=1;
        }


        if (ismatrix(word[1])) {
                w1mat=1;
                if ( matrix_load( word[1],&aa,&rdim,&cdim,&rlab,&clab
                                ,&lr,&lc,&type,expr) < 0 ) {
                        sprintf(expr,"\nError in opening matrix %s",word[1]);
                        sur_print(expr);
                        WAIT;
                        return;
                }
                if ( rdim != cdim ) {
                        sprintf(expr,"\nError: matrix %s not square",word[1]);
                        sur_print(expr);
                        WAIT;
                        return;
                }
                n=rdim;
                if ( (i=alloc_dist()) < 0 ) {ei_tilaa(1,i); return; }
                if ( mat_read_distance() < 0 ) return;
                if ( (i=varaa_nimet()) < 0 ) {ei_tilaa(2,i); return; }
                if ( lue_nimet() < 0 ) warning_names=1;
        }
        else {
                if ( (i=varaa_tilat()) < 0 ) {ei_tilaa(3,i); return; }
                if ( talleta_data() < 0 ) return;

                if ( (i=spfind("WEIGHTS")) >=0) {
                        strcpy(weightmat,spb[i]);
                        i=matrix_load(weightmat,&aawei,&rdimwei,&cdimwei,&rlabwei,
                                &clabwei,&lrwei,&lcwei,&typewei,exprwei);
                        if (i<0) {
                                sprintf(exprwei,"Error in opening weightmatrix %s",
                                        weightmat);
                                sur_print(exprwei);
                                WAIT;
                                return;
                        }
                        if ( (rdimwei==m) && (cdimwei==1)) weights=1;
                        else weightwarning=1;
                }

                ind_zz=0;
                if ( n*m <= MAXSPACE ) {
                        zz = (float *)muste_malloc( (unsigned int)n*m*sizeof(float) );
                        if (zz!=NULL) ind_zz=1;
                }

                if (ind_zz) {
                        siirto_zz();
                        if ( disk_read_distance() < 0 ) return;
                        if (zz!=NULL) { muste_free(zz); zz=NULL; } // RS CHA
                        if ( (i=alloc_dist()) < 0 ) {ei_tilaa(4,i); return; }
                        disk2mem_dist();
                        muste_fclose(temp);
                }
                else  {
                        if ( (i=alloc_dist()) < 0 ) {ei_tilaa(5,i); return; }
                        if ( mem_read_distance() < 0 ) return;
                        muste_fclose(temp);
                }
                if ( (i=varaa_nimet()) < 0 ) {ei_tilaa(6,i); return; }
                if ( lue_nimet() < 0 ) warning_names=1;
        }

        if (w1mat) free_input_matrix();
        else data_close(&d);

        if (savedist) {
                if (distfiletxt) txt_save_distance_matrix();
                else {
                        if ((i=alloc_save_dist()) < 0 ) {ei_tilaa(7,i);return;}
                        mat_save_distance_matrix();
                }
        }
        if ( (i=varaa2_tilat()) < 0 ) {ei_tilaa(8,i); return;}
        do_cluster();
        free_dist();
        if ( (i=varaa3_tilat()) < 0 ) {ei_tilaa(9,i); return;}
        tee_data();
        printout();
        s_end(argv);
        return;
} /* main */


static int ismatrix(char *w1) {
        if (samaloppu(w1,".MAT")) return 1;
        if (samaloppu(w1,".M")) return 1;
        return 0;
}

static float distance(float a,float b,int dmeasure) {
        float c,d;

        switch( dmeasure ) {
        case SQREUCLIDIAN :
                return ( (a-b)*(a-b) );
        case EUCLIDIAN :
                return ( (a-b)*(a-b) );
        case CITYBLOCK :
                c=a-b;
                c= ( c<0 ? -c : c);
                return ( c );
        case CANBERRA :
                c=a-b;
                c= ( c<0 ? -c : c);
                d=a+b;
                if ( ( d < 0.00001 ) && ( d > -0.00001 ) ) d=0.00001;
                return ( c/d );
        default :
                break;
        }
        return 0.0;
}

static int mat_read_distance(void) {
        int i,j;

        for ( i=1; i<n; ++i)
                for (j=0 ; j<i; ++j)
                        (dist[i])[j]=(float)aa[i+j*cdim];
        return 1;
}

static void equal(float *x) {
        int i;
        double mean;

        for (i=0;i<m;i++) {
                mean=sumx[i]/n;
                x[i]=(x[i]-mean)/sqrt(sumx2[i]/(n-1)-mean*mean*n/(n-1));
        }
}

static void weight(float *x) {
        int i;

        for (i=0;i<m;i++)
                x[i]*=aawei[i];
}

static int mem_read_distance(void) {
          int i,j,k;
          float summa;

          for ( i=1; i<n; ++i) {
                lue(xx,i);
                if (equalize) equal(xx);
                if (weights) weight(xx);
                for (j=0 ; j<i; ++j) {
                  lue(xx2,j);
                  if (equalize) equal(xx2);
                  if (weights) weight(xx2);
                  for (summa=0,k=0;k<m;++k) {
                        summa=summa+distance(xx[k],xx2[k],dis);
                  }
                  if (dis==EUCLIDIAN) summa=sqrt(summa);
                  (dist[i])[j]=summa;
                }
          }
         return 1;
}

static int disk_read_distance(void) {
        char nimi[LLENGTH];
        int i,j,k;
        float summa;

        strcpy(nimi,etmpd); strcat(nimi,"HCLUSTER.TMP");
        temp_dist=muste_fopen(nimi,"wb");
        if (temp_dist==NULL) {
            sprintf(sbuf,"Cannot open %s for temporary data files!",nimi);
            sur_print(sbuf); WAIT; return(-1);
        }

        for ( i=1; i<n; ++i) {
                lue(xx,i);
                if (equalize) equal(xx);
                if (weights) weight(xx);
                for (j=0 ; j<i; ++j) {
                        lue(xx2,j);
                        if (equalize) equal(xx2);
                        if (weights) weight(xx2);
                        for (summa=0,k=0;k<m;++k)
                                summa=summa+distance(xx[k],xx2[k],dis);
                        if (dis==EUCLIDIAN) summa=sqrt(summa);
                        fwrite(&summa,sizeof(float),1,temp_dist);
                }
        }
        muste_fclose(temp_dist);
        temp_dist=muste_fopen(nimi,"rb");
        if (temp_dist==NULL) {
                sur_print("\nCannot reopen %s in disk_read_distance()",nimi);
                WAIT;
                return -2;
        }
        return 1;
}

static int disk2mem_dist(void) {
        int i;

        for ( i=1; i<n; ++i) {
                fread( (dist[i]),sizeof(float),i,temp_dist);
        }
        muste_fclose(temp_dist);
        return 1;
}


static float dd(int method,int k,int r, int s) {
        int summa;

        switch( method ) {

        case SINGLE_LINK :   /* Se sid 79-80 i Jain Anil.  */
                if ( plats(k,r) <= plats(k,s) )
                        return plats(k,r);
                else
                        return plats(k,s);

        case COMPLETE_LINK :
                if ( plats(k,r) >= plats(k,s) )
                        return plats(k,r);
                else
                        return plats(k,s);

        case GROUP_AVERAGE :
                summa = nn[r] + nn[s];
                return ( (float)nn[r] / summa *  plats(k,r)
                        + (float)nn[s] / summa *  plats(k,s) );

        case WEIGHTED_AVERAGE :
                return ( .5 *  plats(k,r)
                        + .5 *  plats(k,s)  );

        case UNWEIGHTED_CENTROID :
                summa = nn[r] + nn[s];
                return ( (float)nn[r] / summa *  plats(k,r)
                        + (float)nn[s] / summa *  plats(k,s)
                        - (float)( nn[r] * nn[s] ) / ( summa * summa ) *  plats(r,s)  );

        case WEIGHTED_CENTROID :
        return ( .5  *  plats(k,r)
                + .5  *  plats(k,s)
                - .25 *  plats(r,s)  );

        case MIN_VARIANCE :
                summa = nn[r] + nn[s] + nn[k];
                return ( (float)( nn[r] + nn[k] ) / summa *  plats(k,r)
                        + (float)( nn[s] + nn[k] ) / summa *  plats(k,s)
                        - (float)( nn[k] ) / summa *  plats(r,s)  );

        default :
                break;
        }
        return 0.0;
}

static void do_cluster(void) {
        int i,  k , j , imin=0, jmin=0,ip=0,jp=0;
        float min;

        for(i=0;i<n;i++)
                aktiva[i]=i;

        for (k=1 ; k<n ; k++){
                min=FLT_MAX;
                for(i=1; i < n-k+1 ; i++)
                        for (j=0; j < i ;j++)
                                if (  (dist[ aktiva[i] ])[aktiva[j]] <= min) {
                                        min =  (dist[aktiva[i]])[aktiva[j]]  ;
                                                /* avigt för att imin < jmin */
                                        ip=j;
                                        jp=i;
                                }
                jmin = aktiva[jp];
                imin = aktiva[ip];
                ii[k]=imin;
                jj[k]=jmin;
                ss[k]=min;
                il[k]=last[imin];
                jl[k]=last[jmin];
                last[imin]=k;
                if (il[k]!=0) next[ il[k] ] = k;
                if (jl[k]!=0) next[ jl[k] ] = k;

                for (i=ip+1;i<n-k+1;i++)
                        if (aktiva[i]!=jmin)
                                ( dist[ aktiva[i] ] )[ imin ] =
                                dd(met,aktiva[i],imin,jmin);
                for (j=0;j<ip;j++)
                        if (aktiva[j]!=jmin)
                                ( dist[ imin ] )[ aktiva[j] ] =
                                dd(met,aktiva[j],imin,jmin);
                nn[imin]+=nn[jmin];
                for (i=0; i < n-k ; i++)            /* hoppar över jmin */
                        if ( aktiva[i]==jmin ) break;
                for (j=(i+1); i < n-k ; j++,i++)
                        aktiva[i]=aktiva[j];
        }
}

#define PUUTTUU 9999999680285692e22

static int tee_data(void) {
        /* Gör en SURVO DATA som kan plottas med GPLOT / LINE=3 POINT=Label */
        int k,l;
        int gang;
        float oksa[8];
        char tom[]="                                                   ";
        char text[]="SURVO 84C DATA";
      short luvut[]={ 17920,8+NIMIPIT,3,3,0,0,8,8,0,100,64,0,64,0,112,0 };
      short luvut2[]={0,4};
      short luvut3[]={4,4};
      short luvut4[]={8,NIMIPIT};
        char nimi[LLENGTH];
        FILE *temp_puu;

        if (!is_path(treedata)) {
                strcpy(nimi,edisk);
                strcat(nimi,treedata);
        }
        else
                strcpy(nimi,treedata);
        strcat(nimi,".SVO");
        temp_puu=muste_fopen(nimi,"wb");
        if (temp_puu==NULL) {
                sprintf(sbuf,"Cannot open %s for temporary data files!",nimi);
                sur_print(sbuf); WAIT; return(-1);
        }
        luvut[4]=6*(n-1);
        fprintf(temp_puu,"%s",text);
        fwrite(luvut,sizeof(short),16,temp_puu);
        fprintf(temp_puu,"%s","                  ");
        fwrite(luvut2,sizeof(short),2,temp_puu);
        fprintf(temp_puu,"%s","4A_ X       ");
        fwrite(luvut3,sizeof(short),2,temp_puu);
        fprintf(temp_puu,"%s","4A_ Y       ");
        fwrite(luvut4,sizeof(short),2,temp_puu);
        fprintf(temp_puu,"%s","SA_ Label   ");

        for (l=1,k=1,gang=1;gang<n;gang++){
                while ( ( il[k]!=0 && !gjord[ il[k] ] ) ||
                        ( jl[k]!=0 && !gjord[ jl[k] ] )) {
                        while ( il[k]!=0 && !gjord[ il[k] ] ) k = il[k];
                        while ( jl[k]!=0 && !gjord[ jl[k] ] ) k = jl[k];
                }
                oksa[0]=-(il[k]==0 ? 0 : ss[il[k]]);
                oksa[1]=(il[k]==0 ? l++ : mittpunkt[ il[k] ]);

                oksa[6]=-(jl[k]==0 ? 0 : ss[jl[k]] );
                oksa[7]=(jl[k]==0 ? l++ : mittpunkt[ jl[k] ]);

                mittpunkt[k] = (oksa[1]+oksa[7])/2;

                oksa[2]=-ss[k];
                oksa[3]=mittpunkt[k];

                oksa[4]=oksa[2];
                oksa[5]=PUUTTUU;

                fwrite(&oksa[0],sizeof(float),2,temp_puu);

                if (il[k]==0) fwrite((nimet+ii[k]*NIMIPIT),1,NIMIPIT,temp_puu);
                else fwrite(tom,1,NIMIPIT,temp_puu);

                fwrite(&oksa[2],sizeof(float),2,temp_puu);
                fwrite(tom,1,NIMIPIT,temp_puu);

                fwrite(&oksa[4],sizeof(float),2,temp_puu);
                fwrite(tom,1,NIMIPIT,temp_puu);

                fwrite(&oksa[6],sizeof(float),2,temp_puu);
                if (jl[k]==0) fwrite(nimet+jj[k]*NIMIPIT,1,NIMIPIT,temp_puu);
                else fwrite(tom,1,NIMIPIT,temp_puu);

                fwrite(&oksa[2],sizeof(float),2,temp_puu);
                fwrite(tom,1,NIMIPIT,temp_puu);

                fwrite(&oksa[4],sizeof(float),2,temp_puu);
                fwrite(tom,1,NIMIPIT,temp_puu);

                gjord[k]=1;
                if (ss[k]>maxss) maxss=ss[k];
                k=next[k];
        }
        muste_fclose(temp_puu);
        return 1;
}

static int is_path(char *a) {
        for(;*a;a++)
                if ( *a == ':' || *a == '\\' ) return 1;
        return 0;
}

static int mojena(float k) {
        int i;
        float stddev,mean,stop;
        float sumss=0,sumss2=0;
        for (i=1;i<n;i++) {
                sumss+=ss[i];
                sumss2+=ss[i]*ss[i];
        }
        mean=sumss/(n-1);
        stddev=sqrt(sumss2/(n-2)-mean*mean*(n-1)/(n-2));
        stop=mean+k*stddev;
        for (i=1;i<n;i++) {
                if (ss[i]>stop) return (n-i+1);
        }
        return 0;
}

static int maxstep(void) {
        int i,maxi=0;
        float maxstep,step;
        ss[0]=0;
        for (maxstep=0,i=1 ; i<n ;i++ ) {
                step = ss[i]-ss[i-1] ;
                if ( step > maxstep ) {
                        maxstep=step;
                        maxi=i;
                }
        }
        return n-maxi+1 ;
}


static void printout(void) {
        int k;
        char line[LLENGTH];
        char line2[LLENGTH];
        char luku[32];

        output_open(eout);

        if (w1mat)
                sprintf(line,"  Hierarchical cluster analysis "
                        "of distance matrix %s N=%d",word[1],n);
        else
                sprintf(line,"  Hierarchical cluster analysis "
                        "of observations in %s N=%d",word[1],n);
        print_line(line);

        if (equalize)
                print_line("  Variables equalized to zero mean and"
                " unit variance.");

        if (weights) {
                sprintf(line,"  Variables weighted with matrix: %s",weightmat);
                print_line(line);
        }

        sprintf(line,"  Method : %s    ",methodname[met]);
        if (!w1mat) {
                sprintf(line2,"  Distance : %s",distancename[dis]);
                strcat(line,line2);
        }
        print_line(line);
        if (warning_missing) {
                if (warning_missing==1) {
                        sprintf(line,"Note: %d observation is skipped due "
                                "to missing value.",warning_missing);
                        print_line(line);
                }
                else  {
                        sprintf(line,"Note: %d observations are skipped due "
                                "to missing values.",warning_missing);
                        print_line(line);
                }
        }
        if (warning_names)
                print_line("Warning: observation labels are not unique!");

        if (weightwarning)
                print_line("Warning: Weightmatrix wrong "
                        "dimension; weighting not performed.");

        if (label_obs)
                print_line("Note: label is observation number in data.");

        if (ps) teepskaavio();
        else teegplotkaavio();

        if (savedist) {
                if (distfiletxt)
                        sprintf(line,"  Distances saved in textfile "
                                        "%s   '\376' SHOW %s",distfile,distfile);
                else sprintf(line,"  Distances saved as "
                        "matrix %s   '\376' MAT LOAD %s,END+2",distfile,distfile);
                print_line(line);
                print_line("");
        }

        if (results>10) {
          print_line(" Stage    Cluster 1   Cluster 2    Distance");
          for ( k=1;k<n;k++) {
                sprintf(line," %3d    ",k);
                if (il[k]==0) sprintf(line2,"%12.11s",(nimet+ii[k]*NIMIPIT));
                else sprintf(line2,"   GROUP %-3d",il[k]);
                strcat(line,line2);
                if (jl[k]==0) sprintf(line2,"%12.11s",(nimet+jj[k]*NIMIPIT));
                else sprintf(line2,"   GROUP %-3d",jl[k]);
                strcat(line,line2);
                fnconv(ss[k],accuracy+2,luku);
                sprintf(line2,"   %-13.13s",luku);
                strcat(line,line2);
                print_line(line);
          }
        }

        if (results>20) {
         print_line ( " +--------------------------------------------+");
         print_line ( " | Mojena's| Value of k   |1.25|2.0 |2.75|3.5 |");
         print_line ( " | stopping|--------------+----+----+----+----|");
         sprintf(line," |   rule  | No of groups |%3d |%3d |%3d |%3d |",
                mojena(1.25),mojena(2.0),mojena(2.75),mojena(3.5));
         print_line(line);
         print_line ( " +--------------------------------------------+");


         print_line  (" +-------------------------------------+");
         sprintf(line," |  Max stepsize indicates %3d groups  |",maxstep() );
         print_line(line);
         print_line  (" +-------------------------------------+");
        }
        print_line("");
        output_close(eout);
}


static int talleta_data(void) {
        char nimi[LLENGTH];
        int i;
        int j;
        double a;

        strcpy(nimi,etmpd); strcat(nimi,"HCLUSTER.TMP");
        temp=muste_fopen(nimi,"wb");
        if (temp==NULL) {
                sprintf(sbuf,"Cannot open %s for temporary data files!",nimi);
                sur_print(sbuf); WAIT; return(-1);
        }
        n=0L;
        sur_print("\nSaving active data in a temporary file... ");
        for (j=d.l1; j<=d.l2; ++j) {
                if (unsuitable(&d,j)) continue;
                sprintf(sbuf,"%d ",j); sur_print(sbuf);
                for (i=0; i<m; ++i) {
                        data_load(&d,j,d.v[i],&a);
                        if (a==MISSING8) break;
                        xx[i]=(float)a;
                }
                if (i<m) {
                        warning_missing++;
                        sprintf(sbuf,"\nValue of variable %s missing "
                                "in observation #%d!\n",d.varname[d.v[i]],j);
                        sur_print(sbuf);
                        continue;
                }
                ++n;
                if (equalize) {
                        for (i=0;i<m;i++) {
                                sumx[i]+=xx[i];
                                sumx2[i]+=xx[i]*xx[i];
                        }
                }
                fwrite(xx,sizeof(float),m,temp);
        }
        muste_fclose(temp);
        if (n<2) {
                sur_print("\nLess than 2 valid observations!"); WAIT;
                return(-1);
        }
        temp=muste_fopen(nimi,"rb");
        if (temp==NULL) {
                        sur_print("\nCannot reopen %s in talleta_data()",nimi);
                        WAIT;
                        return -2;
        }
        return(1);
}

static int siirto_zz(void) {

        rewind(temp);
        fread(zz,sizeof(float),m*n,temp);
        return(1);
}

static int lue(float *xx, int j) {

        if (ind_zz) {
                memcpy(xx,zz+(unsigned int)j*m,m*sizeof(float));
                return(1);
        }
        muste_fseek(temp,j*m*sizeof(float),SEEK_SET);
        fread(xx,sizeof(float),m,temp);
        return 0;
}

static int lue_nimet(void) {
        char *p;
        char *q;
        int j,k;
        int i,eri;
        char label[LLENGTH];
        double a;

        p=nimet;
        if (w1mat)
                for (j=0;j<n;j++) {
                        *p++ = ' ';
                        *p++ = ' ';
                        for (i=0 ; i < NIMIPIT-2 ; i++)
                                if (i < lr )
                                        *p++=*rlab++;
                                else
                                        *p++=' ';
                        for(;i<lr;i++,rlab++);
                }
        else
                for (j=d.l1; j<=d.l2; ++j) {
                        q=label;
                        if (unsuitable(&d,j)) continue;

                        for (i=0; i<m; ++i) {
                                data_load(&d,j,d.v[i],&a);
                                if (a==MISSING8) break;
                        }
                        if (i<m) continue;


                        if (label_obs) {
                                *p++=' ';
                                *p++=' ';
                                int_to_string(j,p,p+NIMIPIT-3);
                                p+=NIMIPIT-2;
                        }
                        else {
                                data_alpha_load(&d,j,namevar,label);
                                *p++ = ' ';
                                *p++ = ' ';
                                for (i=0 ; i < NIMIPIT-2 ; i++ )
                                        if (*q)
                                                *p++=*q++ ;
                                        else
                                                *p++=' ';
                        }
                }
        for (j=0;j<n-1;j++)
                for(k=j+1;k<n;k++) {
                        eri=0;
                        for (i=0;i<NIMIPIT;i++)
                                if(*(nimet+j*NIMIPIT+i) != *(nimet+k*NIMIPIT+i))
                                        {eri=1;break;}
                        if (!eri) {vika_samatnimet(j,k);return -1;}
                }
        return 1;
}

static void vika_samatnimet(int j,int k) {
        char nimi1[NIMIPIT+1];
        char nimi2[NIMIPIT+1];
        char line[LLENGTH];
        int i;
        for (i=0;i<NIMIPIT;i++)
                nimi1[i]=*(nimet+j*NIMIPIT+i);
        nimi1[NIMIPIT]='\0';
        for (i=0;i<NIMIPIT;i++)
                nimi2[i]=*(nimet+k*NIMIPIT+i);
        nimi2[NIMIPIT]='\0';
        sprintf(line,"\nWarning: same labels %s and %s"
                ,nimi1,nimi2);
        sur_print(line);
        WAIT;
}

static int varaa_tilat(void) {
        int i;

        xx=(float *)muste_malloc(m*sizeof(float));
        if (xx==NULL) return(-1);

        xx2=(float *)muste_malloc(m*sizeof(float));
        if (xx==NULL) return(-2);

        if (equalize) {
                sumx=(double *)muste_malloc(m*sizeof(double));
                if (sumx==NULL) return(-3);

                sumx2=(double *)muste_malloc(m*sizeof(double));
                if (sumx2==NULL) return(-4);

                for (i=0;i<m;i++) {
                        sumx[i]=0.0;
                        sumx2[i]=0.0;
                }
        }

        return 1;
}

static int varaa_nimet(void) {
        nimet=(char *)muste_malloc( (unsigned int)(n+1) * NIMIPIT * sizeof(char));
        if (nimet==NULL) return(-1);
        return 1;
}

static int alloc_dist(void) {
        int i;

        dist=(float **)muste_malloc( (unsigned int)(n+1) * sizeof(float *));
        if (dist==NULL) return(-1);

        for (i=n-1;i>=0;i--) {  // RS CHA i>0 -> i>=0
                dist[i]=(float *)muste_malloc( (unsigned int)i * sizeof(float) );
                if (dist[i]==NULL) return(-2);
        }

        return 1;
}

static void free_dist(void) {
        int i;

        for (i=0;i<n;i++)
                if (dist[i]!=NULL) { muste_free(dist[i]); dist[i]=NULL; } // RS CHA
}

static int nollaa(int *uusi,int maara) {
        int i;

        for (i=0;i<maara;i++)
                uusi[i]=0;
        return 0;
}

static int varaa2_tilat(void) {
        int j;

        ii=(int *)muste_malloc( (unsigned int)n * sizeof(int));
        if (ii==NULL) return(-1);
        nollaa(ii,(int)n);

        jj=(int *)muste_malloc( (unsigned int)n * sizeof(int));
        if (jj==NULL) return(-2);
        nollaa(jj,(int)n);

        il=(int *)muste_malloc( (unsigned int)n * sizeof(int));
        if (il==NULL) return(-3);
        nollaa(il,(int)n);

        jl=(int *)muste_malloc( (unsigned int)n * sizeof(int));
        if (jl==NULL) return(-4);
        nollaa(jl,(int)n);

        last=(int *)muste_malloc( (unsigned int)n * sizeof(int));
        if (last==NULL) return(-5);
        nollaa(last,(int)n);

        next=(int *)muste_malloc( (unsigned int)n * sizeof(int));
        if (next==NULL) return(-6);
        nollaa(next,(int)n);

        ss=(float *)muste_malloc( (unsigned int)n * sizeof(float));
        if (ss==NULL) return(-7);

        aktiva=(int *)muste_malloc( (unsigned int)n * sizeof(int));
        if (aktiva==NULL) return(-8);
        nollaa(aktiva,(int)n);

        nn=(int *)muste_malloc( (unsigned int)n * sizeof(int) );
        if (nn==NULL) return (-9);
        for (j=0;j<n;j++) nn[j]=1;

        return(1);
}

static int varaa3_tilat(void) {
        mittpunkt=(float *)muste_malloc( (unsigned int)n * sizeof(float) );
        if (mittpunkt==NULL) return (-1);

        gjord=(int *)muste_malloc( (unsigned int)n * sizeof(int) );
        if (gjord==NULL) return (-2);
        nollaa(gjord,(int)n);

        return 1;
}

static int ei_tilaa(int a,int b) {
        char line[LLENGTH];
        sprintf(line,"\nNot enough memory (at %d.%d)!",a,-b);
        sur_print(line); WAIT; return(1);
}

static void print_line(char *line) {
        output_line(line,eout,results_line);
        if (results_line) ++results_line;
}

static int metstring(char *t) {
        if (samaalku(t,"SIN")) return 1;
        if (samaalku(t,"COM")) return 2;
        if (samaalku(t,"AVE")) return 3;
        if (samaalku(t,"GRO")) return 3;
        if (samaalku(t,"UNWEIGHTED_AVER")) return 3;
        if (samaalku(t,"WAV")) return 4;
        if (samaalku(t,"WEIGHTED_AVER")) return 4;
        if (samaalku(t,"CEN")) return 5;
        if (samaalku(t,"UNWEIGHTED_CENT")) return 5;
        if (samaalku(t,"WCE")) return 6;
        if (samaalku(t,"WEIGHTED_CENT")) return 6;
        if (samaalku(t,"MIN")) return 7;
        if (samaalku(t,"WAR")) return 7;
        if (samaalku(t,"MINIMUM_VAR")) return 7;
        return -1;
}

static int disstring(char *t) {
        if (samaalku(t,"SQR")) return SQREUCLIDIAN;
        if (samaalku(t,"SQU")) return SQREUCLIDIAN;
        if (samaalku(t,"EUC")) return EUCLIDIAN;
        if (samaalku(t,"CIT")) return CITYBLOCK;
        if (samaalku(t,"CAN")) return CANBERRA;
        return -1;
}

static int samaalku(char *a, char *b) {
        for (;*a==*b || toupper(*a)==*b;a++,b++)
                if (*b==0) return 1;
        if (*b==0) return 1;
        return 0;
}

static int samaloppu(char *a, char *b) {
        char *atmp,*btmp;

        for (atmp=a;*a!='\0';a++);
        for (btmp=b;*b!='\0';b++);
        for (;*a==*b || toupper(*a)==*b;a--,b--) {
                if (b==btmp) return 1;
                if (a==atmp) return 0;
        }
        return 0;
}

static double pyoristaylos(double x) {
        int k;
        double kerroin;

        if (fabs(x)>=1000) kerroin=.1;
        if (fabs(x)<100) kerroin=10;
        for (k=0;fabs(x)>=1000 || fabs(x)<100;k++,x*=kerroin);
        x=ceil(x);
        for (;k>0;k--,x/=kerroin);
        return x;
}

static int poistapaate(char *t) {
        for (;*t;t++) {
                if (*t=='.') {
                        *t='\0';
                        return 1;
                }
        }
        return 0;
}

static void raja_rivi(void) {
        int i;
        char line[LLENGTH];
        for (*line='\0',i=0;i<c3;i++)
                strcat(line,".");
        print_line(line);
}




#define MAXXSIZE 1700.0
#define MAXYSIZE 2800.0
#define MINXSIZE 800.0
#define MINYSIZE 800.0
#define MAXFONT 12.0
#define MINFONT 3.0
#define NRAJA 50L

static void teepskaavio(void) {
        int font=MAXFONT,xsize,ysize,xhome,yhome;
        char line[LLENGTH];
        char line2[LLENGTH];

        if (n>NRAJA) font=MINFONT+(350.0-n)/(350.0-NRAJA)*(MAXFONT-MINFONT);
        raja_rivi();
        sprintf(line,"  Activate the line below to plot the tree to file %s.PS"
                        ,treedata);
        print_line(line);
        sprintf(line," PLOT %s,X,Y",treedata);
        print_line(line);
        print_line("  Change YSCALE and XSCALE below to plot subtrees.");
        sprintf(line,"   YSCALE=1,%d     POINT=Label  FRAME=0 "
        "XLABEL=<-_Distance",n);
        print_line(line);
        if (landscape)
                sprintf(line,"   XSCALE=-%-1g/1,0   PEN=[Landscape][Swiss(%d)] "
                "MODE VGA YLABEL= ",pyoristaylos(maxss),font-2);
        else
                sprintf(line,"   XSCALE=-%-1g/1,0     PEN=[Swiss(%d)] "
                "MODE VGA YLABEL= ",pyoristaylos(maxss),font);

        print_line(line);
        if (!landscape) {
                xsize=MAXXSIZE;
                ysize=MAXYSIZE;
                if (n<NRAJA) {
                        xsize=MINXSIZE+n/(float)NRAJA*(MAXXSIZE-MINXSIZE);
                        ysize=MINYSIZE+n/(float)NRAJA*(MAXYSIZE-MINYSIZE);
                }
                xhome=100;
                yhome=100+MAXYSIZE-ysize;
        }
        else {
                xsize=MAXYSIZE;
                ysize=MAXXSIZE;
                if (n<NRAJA) {
                        xsize=MINYSIZE+n/(float)NRAJA*(MAXYSIZE-MINYSIZE);
                        ysize=MINXSIZE+n/(float)NRAJA*(MAXXSIZE-MINXSIZE);
                }
                xhome=-300;
                yhome=100+MAXXSIZE-ysize;
        }
        if (!landscape)
                sprintf(line,"XDIV=100,%d,100 YDIV=100,%d,200"
                ,xsize-200,ysize-300);
        else
                sprintf(line,"XDIV=100,%d,200 YDIV=100,%d,100"
                ,xsize-200,ysize-300);
        print_line("  Change LINE spec. below to 1 for a 'triangular' tree.");
        sprintf(line2,"   LINE=3   %s",line);
        print_line(line2);
        sprintf(line,"   HEADER=[Swiss(18)],%s;_Input:_%s"
                ,methodname[met],word[1]);
        print_line(line);
        sprintf(line,"   DEVICE=PS,%s.PS  SIZE=%d,%d  HOME=%d,%d"
                ,treedata,xsize,ysize,xhome,yhome);
        print_line(line);
        raja_rivi();
}

static void teegplotkaavio(void) {
        char line[LLENGTH];
        raja_rivi();
        print_line("  Activate the line below to plot the tree on screen."); // CRT.");
        sprintf(line," GPLOT %s,X,Y",treedata);

        print_line(line);
        print_line("  Change YSCALE and XSCALE below to plot subtrees.");
        sprintf(line,"   YSCALE=1,%d     POINT=Label FRAME=0 "
                "XLABEL=<-_Distance",n);
        print_line(line);
        sprintf(line,"   XSCALE=-%-1g/1,0     PEN=[Swiss(10)] "
                "MODE VGA YLABEL= ",pyoristaylos(maxss));
        print_line(line);
        print_line("  Change LINE spec. below to 1 for a 'triangular' tree.");
        sprintf(line,"   LINE=3     XDIV=1,65,10 YDIV=2,20,1");
        print_line(line);
        sprintf(line,"   HEADER=[Swiss(10)],%s;_Input:_%s"
                        ,methodname[met],word[1]);
        print_line(line);
        raja_rivi();
}

static int txt_save_distance_matrix(void) {
        char blanko[]=" ";
        char nimi[LLENGTH];
        char havainto[LNAME];
        int i,j;
        FILE *temp_txt;

        distfiletxt=1;
        strcat(distfile,".TXT");
        if (!is_path(distfile)) {
                strcpy(nimi,edisk);
                strcat(nimi,distfile);
        }
        else
                strcpy(nimi,distfile);
        temp_txt=muste_fopen(nimi,"w");
        if (temp_txt==NULL) {
                sprintf(sbuf,"Cannot open %s for distance matrix!",nimi);
                sur_print(sbuf); WAIT; return(-1);
        }
        fprintf(temp_txt,"%s distances computed from %s by HCLUSTER.\n\n",
                distancename[dis],word[1]);
        fprintf(temp_txt,"%12.10s",blanko);
        for (i=0;i<n;i++) {
                lue_havainto_nimi(havainto,i);
                fprintf(temp_txt,"%-12.10s",havainto);
        }
        for (i=0;i<n;i++) {
                fprintf(temp_txt,"\n");
                lue_havainto_nimi(havainto,i);
                fprintf(temp_txt,"%-12.10s",havainto);

                for (j=0;j<n;j++) {
                        if (i==j) {
                                fprintf(temp_txt,"%-12g",0.0);
                                continue;
                        }
                        fprintf(temp_txt,"%-12g",plats(i,j));
                }
        }
        muste_fclose(temp_txt);
        return 1;
}

static int alloc_save_dist(void) {
        aa=(double *)muste_malloc( (unsigned int)(n*n) * sizeof(double) );
        if (aa==NULL) return (-1);
        return 1;
}


static int mat_save_distance_matrix(void) {
        int i,j;
        char nimi[LNAME];

        strcpy(nimi,distfile);
        for(i=0;i<n;i++)
                for(j=0;j<n;j++) {
                        if (i==j) {
                                aa[i+j*n]=0.0;
                                continue;
                        }
                        aa[i+j*n]=plats(i,j);
                }
        type=10;
        sprintf(expr,"%s_distances_computed_by_HCLUSTER.",distancename[dis]);
        i=n;j=n;
        i=matrix_save(nimi,aa,i,j,nimet,nimet,NIMIPIT,NIMIPIT,type,expr,0,0);
        if (i<0) {
                sprintf(expr,"Cannot save distance matrix to %s!",nimi);
                sur_print(expr);WAIT;return -1;
        }
        return 1;
}

static int lue_havainto_nimi(char *a, int i ) {
        char *p;
        int k;       /* +1 koska eka merkki blanko */
        for (k=0,p=(nimet+NIMIPIT*i+1);k<NIMIPIT-1;k++)
                *a++=*p++;
        *a='\0';
        return 1;
}

static void free_input_matrix(void) {
        if (aa!=NULL) { muste_free( aa); aa=NULL; } // RS CHA
        if (rlab!=NULL) { muste_free( rlab); rlab=NULL; } // RS CHA
        if (clab!=NULL) { muste_free( clab); clab=NULL; } // RS CHA
}

static void int_to_string(int luku,char *eka,char *vika) {
        char lukustring[12];
        int i;

        for (i=0; i<12 && luku ;luku/=10,i++)
                lukustring[i]=luku%10+'0';
        for (i--;i>=0 && eka<=vika ;i--)
                *eka++=lukustring[i];
        for (;eka<=vika;)
                *eka++=' ';
}
#if 0
aputekstit() {
        int i;
        char *apu[]={
"HCLUSTER <input>,<output_line>                    /    F. Åberg 8.11 1996",
"",
"Performs hierarchical clustering of observations in the specified data",
"or on a distance matrix.",
"HCLUSTER lets you plot a dendrogram on CRT or a PostScript printer.",
"",
"When data is used, the variable with the label should be activated with",
"letter L, and the variables to compute the distances from with letter A.",
"If no L activated variable is found, the first activated variable is",
"used if it is of string type.",
"If no suitable label is found, the observation numbers are used as labels.",
"Note: the label variable must be of string (S) type.",
"",
"The HCLUSTER module recognizes a distance matrix as input when the name",
"ends with the .MAT extension.",
"(eg. DIST and DISTV modules by S.Mustonen are useful for making",
"distance matrices.)",
"",
"On next screen about various specifications.",
"",
"&",
"Specification  Different values     Abbrevation      Remarks",
" METHOD         SINGLE_LINKAGE      SIN or 1          (default)",
"                COMPLETE_LINKAGE    COM or 2 ",
"                AVERAGE_LINKAGE     AVE or 3 ",
"                WEIGHTED_AVERAGE    WAV or 4 ",
"                CENTROID            CEN or 5 ",
"                WEIGHTED_CENTROID   WCE OR 6 ",
"                MINIMUM_VARIANCE    MIN or 7         Also called Ward's method.",
" SAVEDIST       <matrix>                             Default: no saving.",
"                <textfile>                           With extension .TXT",
" DISTANCE       SQUARED_EUCLIDIAN   SQU or SQR or 1  (default)",
"                EUCLIDIAN           EUC or 2",
"                CITY_BLOCK          CIT OR 3",
"                CANBERRA_METRIC     CAN or 4",
" TREEDATA       <datafile>                           Default: #TREE#",
"                                                     Used also for PS file.",
" RESULTS        0..10:short output, 11..20:medium, >20:Long output",
" PLOT           PS  or  POSTSCRIPT                   Output for PostScript.",
"                PS,LANDSCAPE                         Print format: Landscape",
" EQUALIZE       1                                    Any value will do.",
" WEIGHTS        <weight matrix>                      Vector with weights.",
"&",
"HCLUSTER DECA,CUR+1    /  METHOD=MINIMUM_VARIANCE  SAVEDIST=MAT1",
" The distance matrix is saved in matrix file MAT1.MAT. If n>90 then distances",
" are saved as a text file MAT1.TXT",
"",
"HCLUSTER D.MAT,CUR+1   /  TREEDATA=C:\\TMP\\TREE1  RESULTS=0  ",
" Performs cluster analysis based on distance matrix D. The data that contains",
" the dendrogram is saved in data file TREE1.SVO in current datapath.",
" Only the lines relevant for plotting the dendrogram are as output.",
" Note that TREEDATA and SAVEDIST can include a path name.",
"",
"HCLUSTER MYDATA,CUR+1  /  DISTANCE=CIT  PLOT=PS  SAVEDIST=DIST1.TXT",
" Uses method single linkage and the distances are CITY BLOCK measures.",
" The dendrogram is 'printed' to a PostScript file. The name (and path)",
" is the same as in TREEDATA but with the .PS extension.",
" The distance matrix is saved as a textfile in DIST1.TXT (in datapath).",
" Note that the distance matrix is not saved by default.",

"",
" More about HCLUSTER on next screen.",
"",
"&",
"The HCLUSTER module uses an agglomerative algorithm. ",
"Other distance measures can be used by making a",
"distance matrix with the DIST module.",
"",
"Note that HCLUSTER only work with dissimilarity measures.",
"",
"Literature used for programming the HCLUSTER module:",
" Anderberg Michael R. : Cluster Analysis for Applications,AP, NY & London, 1973",
" Jain Anil K. : Algorithms for Clustering Data, Prentce-Hall, New Jersey, 1988",
" Everitt Brian S. : Cluster Analysis, Edward Arnold, London, 1983",
"",
"$"};
        init_remarks();
        for(i=0; *apu[i] != '$';i++)
                if (*apu[i] == '&') { wait_remarks(1);continue; }
                else rem_pr(apu[i]);
        wait_remarks(2);
        return 1;
}
#endif
