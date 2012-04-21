/* _dcluster.c 8.1.1998/SM (11.1.1998)
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
static char aineisto[LNAME];
static int ng;
static int m;
static int gvar,label_var;
static int silh_var; /* silhouette values */
static int *group;
static double *dd; /* i distances */
static double *ssum;
static int *g1; /* method=3 */
static int *size3,*g3; /* method=3 */

static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];
static double *xx;
static int *medoid;
static int *gsize;
static double min;
static int method; /* 1=partial 2=complete SWAP */
static int tulosrivi;
static int prind=0;

static int varaa_tilat();
static int initial_medoids();
static int iterate1();
static int iterate2();
static int single_linkage();
static int init_groups1(int var);
static int save_grouping();
static int save_silhouettes();
static int print_results();
static int print_results3();
static int eoutput(char *rivi);
static int not_enough_memory();

/****************************
char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "GROUPS", "METHOD", "INIT", "WEIGHT", "!" };
char **specs=specs0;
*********************************/
/**********************
void main(argc,argv)
int argc; char *argv[];
***********************/

void muste_dcluster(char *argv)
        {
   //   char ch;
        int i;
        int m2;

//      if (argc==1) return;
        s_init(argv);

    medoid=NULL;
    gsize=NULL;
    group=NULL;
    dd=NULL;
    ssum=NULL;
    g1=NULL;
    size3=NULL;
    g3=NULL;
    rlab=NULL;
    clab=NULL;
    xx=NULL;


        if (g<3)
            {
            init_remarks();
            rem_pr("DCLUSTER <data>,<distance_matrix>,L");
            rem_pr("performs cluster analysis by different methods.");
            rem_pr("By default is is done  by means of `medoids'.");
            rem_pr(" ");
            rem_pr("This technique is presented by Kaufman and Rousseeuw in 1987.");
            rem_pr("See their book `Finding Groups in Data' (Wiley 1990).");
            rem_pr(" ");
            rem_pr("This method is based entirely on a ready-computed <distance_matrix>");
            rem_pr("(obtained in Survo by the DIST operation, for example) from Survo");
            rem_pr("<data>.");
            rem_pr(" ");
            rem_pr("The number of clusters is given by specification GROUPS.");
            rem_pr("Default is GROUPS=2. The group indices 1,2,... are saved in <data>");
            rem_pr("as a variable given by mask `G' and the (optional) `silhouette'");
            rem_pr("as values of variable with mask `S'.");
            wait_remarks(1);

            rem_pr("DCLUSTER <data>,<distance_matrix>,L");
            rem_pr("with specification METHOD=3 makes cluster analysis");
            rem_pr("by the single linkage (nearest neighbour) method.");
            rem_pr("The setup is otherwise similar as described above.");
            rem_pr("To avoid certain weaknesses of this method like");
            rem_pr("the chaining effect the distance between two groups");
            rem_pr("of sizes n1 and n2 is multiplied by");
            rem_pr("1+weight*[log2(min(n1,n2)+1)-1]  (suggestion of S.M. 1998)");
            rem_pr("by using the specification WEIGHT=weight. Default is WEIGHT=0");
            rem_pr("(i.e. standard single linkage) but WEIGHT=1 is recommended.");
            wait_remarks(2);
            s_end(argv);
            return;
            }

        prind=0;
        tulosrivi=0;
        if (g>3)
            {
            tulosrivi=edline2(word[3],1,1);
            if (tulosrivi<0) return;
            }

        strcpy(aineisto,word[1]);
        i=data_open(aineisto,&d); if (i<0) return;
        i=sp_init(r1+r-1); if (i<0) return;
        i=mask(&d); if (i<0) return;

        gvar=activated(&d,'G');
        if (gvar<0)
            {
            sur_print("\nNo grouping variable (activated by 'G') given!");
            WAIT; return;
            }

        label_var=activated(&d,'L');
        if (label_var<0) label_var=0;
        if (d.vartype[label_var][0]!='S') label_var=-1;

        i=spfind("GROUPS");
        if (i<0) ng=2; else ng=atoi(spb[i]);
  /*    if (ng<2) ng=2;   */

        method=1;
        i=spfind("METHOD");
        if (i>=0) method=atoi(spb[i]);

        silh_var=activated(&d,'S');
        if (method==3) silh_var=-1;

        i=matrix_load(word[2],&xx,&m2,&m,&rlab,&clab,&lr,&lc,&type,expr);
        if (i<0) return;

        if (m2!=m)
            {
            sprintf(sbuf,"\n# of rows in %s (%d) not equal to %d!",
                               word[2],m2,m);
            sur_print(sbuf); WAIT; return;
            }
        i=varaa_tilat(); if (i<0) return;

        switch (method)
            {
          case 1: initial_medoids();
                  iterate1(); break; /* short cut technique by S.M. */
          case 2: initial_medoids();
                  iterate2(); break;
          case 3: i=single_linkage(); if (i<0) return; break;
            }
        i=save_grouping(); if (i<0) return;
        if (method==3)
            {
            print_results3(); s_end(argv); return;
            }
        if (silh_var>=0) save_silhouettes();
        print_results();
        s_end(argv);
        }

static int varaa_tilat()
        {
        medoid=(int *)muste_malloc(ng*sizeof(int));
        if (medoid==NULL) { not_enough_memory(); return(-1); }
        gsize=(int *)muste_malloc(ng*sizeof(int));
        if (gsize==NULL) { not_enough_memory(); return(-1); }
        group=(int *)muste_malloc(m*sizeof(int));
        if (group==NULL) { not_enough_memory(); return(-1); }
        if (silh_var>=0)
            {
            dd=(double *)muste_malloc(ng*sizeof(double));
            if (dd==NULL) { not_enough_memory(); return(-1); }
            ssum=(double *)muste_malloc(ng*sizeof(double));
            if (ssum==NULL) { not_enough_memory(); return(-1); }
            }
        if (method==3)
            {
            g1=(int *)muste_malloc(ng*sizeof(int));
            if (g1==NULL) { not_enough_memory(); return(-1); }
            }
        return(1);
        }

static int initial_medoids()
        {
        int i,j,k,h,kk,i_min;
        double sum,min2,a;

        min=1e100; i_min=0;
        for (i=0; i<m; ++i)
            {
            sum=0.0;
            for (j=0; j<m; ++j) sum+=xx[i+m*j];
            if (sum<min) { min=sum; i_min=i; }
            }

        medoid[0]=i_min; min=1e100;
        for (k=1; k<ng; ++k)
            {
            for (i=0; i<m; ++i)
                {
                for (j=0; j<k; ++j) if (i==medoid[j]) break;
                if (j!=k) continue;
                medoid[k]=i;
                sum=0.0;
                for (h=0; h<m; ++h)
                    {
                    min2=xx[h+m*medoid[0]];
                    for (kk=1; kk<=k; ++kk)
                        {
                        a=xx[h+m*medoid[kk]];
                        if (a<min2) min2=a;
                        }
                    sum+=min2;
                    }
                if (sum<min) { min=sum; i_min=i; }
                }
            medoid[k]=i_min;
            }
        sur_print("\ninitial medoids: ");
        for (i=0; i<ng; ++i)
            {
            sprintf(sbuf," %d",medoid[i]); sur_print(sbuf);
            }

        return(1);
        }

static int iterate1()
        {
        int i,h,kk,i_min;
        double sum,min2,a;
        int med0;
        int iter,no_change;

        iter=0; no_change=0;
        while (1)
            {
            med0=medoid[0]; ++iter;
            for (i=1; i<ng; ++i) medoid[i-1]=medoid[i];
            i_min=-1;
            for (i=0; i<m; ++i)
                {
                medoid[ng-1]=i;
                sum=0.0;
                for (h=0; h<m; ++h)
                    {
                    min2=xx[h+m*medoid[0]];
                    for (kk=1; kk<ng; ++kk)
                        {
                        a=xx[h+m*medoid[kk]];
                        if (a<min2) min2=a;
                        }
                    sum+=min2;
                    }

      //      sprintf(sbuf,"\nsum=%g min=%g",sum,min); // sur_print(sbuf);
                if (1.0000001*sum<min) { min=sum; i_min=i; }


                }
            if (i_min>=0)
                {
                medoid[ng-1]=i_min; no_change=0;
                }
            else
                {
                medoid[ng-1]=med0; ++no_change;
                if (no_change>ng) return(1);
                }
            sprintf(sbuf,"\n%d:",iter); sur_print(sbuf);
            for (i=0; i<ng; ++i)
                {
                sprintf(sbuf," %d",medoid[i]); sur_print(sbuf);
                }

            if (sur_kbhit())
                {
                i=sur_getch();
                if (i=='.') break;
                }
            }
        return(1);
        }

static int iterate2()
        {
        int i,h,k,kk,i_min,i_med;
        double sum,min2,a;
        int med0;
        int iter,no_change;

		i_med=0;
        iter=0; no_change=0;
        while (1)
            {
            i_min=-1; ++iter;
            for (i=0; i<m; ++i)
                {
                for (k=0; k<ng; ++k)
                    {
                    med0=medoid[k]; medoid[k]=i;
                    sum=0.0;
                    for (h=0; h<m; ++h)
                        {
                        min2=xx[h+m*medoid[0]];
                        for (kk=1; kk<ng; ++kk)
                            {
                            a=xx[h+m*medoid[kk]];
                            if (a<min2) min2=a;
                            }
                        sum+=min2;
                        }
                    if (1.0000001*sum<min) { min=sum; i_min=i; i_med=k; }
                    medoid[k]=med0;
                    }
                }
            if (i_min>=0)
                {
                medoid[i_med]=i_min; no_change=0;
                }
            else
                {
                ++no_change;
                if (no_change>ng) return(1);
                }
            sprintf(sbuf,"\n%d:",iter); sur_print(sbuf);
            for (i=0; i<ng; ++i)
                {
                sprintf(sbuf," %d",medoid[i]); sur_print(sbuf);
                }
            if (sur_kbhit())
                {
                i=sur_getch();
                if (i=='.') break;
                }
            }
        return(1);
        }

static double weight;
static int weight_ind;
static int single_linkage()
        {
        int i,j,k,gi,gj,kk;
        int i_min,j_min;
        double a,min2,b;
        int init_group_var;
  //    int lj;
        double weight;
        int weight_ind;
        double inv_log2=1.0/log(2.0);
        int n1,n2,nn;

		weight=0; i_min=j_min=0;
        init_group_var=-1;
        i=spfind("INIT");
        if (i>=0)
            {
            init_group_var=varfind2(&d,spb[i],0);
            }
        if (init_group_var>=0)
            {
            k=init_groups1(init_group_var);
            if (k<0) return(-1);
            ++k;
            }
        else
            {
            for (i=0; i<m; ++i) group[i]=i;
            k=m;
            }

        weight_ind=0;
        i=spfind("WEIGHT");
        if (i>=0)
            {
            weight_ind=1; weight=atof(spb[i]);
            size3=(int *)muste_malloc(k*sizeof(int));
            if (size3==NULL) { not_enough_memory(); return(-1); }
            g3=(int *)muste_malloc(k*sizeof(int));
            if (g3==NULL) { not_enough_memory(); return(-1); }
            }
        sur_print("\n");
        while (k>ng)
          {
          if (weight_ind)
              {
              kk=0;
              for (i=0; i<k; ++i) size3[i]=0;
              for (i=0; i<m; ++i)
                  {
                  gi=group[i];
                  for (j=0; j<kk; ++j) if (gi==g3[j]) break;
                  if (j==kk) { g3[kk]=gi; ++kk; }
                  group[i]=j;
                  ++size3[j];
                  }
              }

          min2=1e100;
          if (sur_kbhit())
              {
              i=sur_getch(); if (i=='#') return(-1);
              if (i=='.') prind=1-prind;
              }
          if (prind) { sprintf(sbuf,"%d ",k); sur_print(sbuf); }
if (weight_ind)
          for (i=0; i<m; ++i)
              {
              gi=group[i]; n1=size3[gi];
              for (j=i+1; j<m; ++j)
                  {
                  if (group[j]==gi) continue;
                  a=xx[i+m*j];
                  n2=size3[group[j]];
                  nn=n1; if (n2<n1) nn=n2;
                  if (nn==1)
                      {
                      if (a<min2) { i_min=i; j_min=j; min2=a; }
                      }
                  else
                      {
                      b=1+weight*(inv_log2*log((double)(nn+1))-1)*a;
                      if (b<min2) { i_min=i; j_min=j; min2=b; }
                      }
                  }
              }
else /* !weight_ind */
          for (i=0; i<m; ++i)
              {
              gi=group[i];
              for (j=i+1; j<m; ++j)
                  {
                  if (group[j]==gi) continue;
                  a=xx[i+m*j];
                  if (a<min2) { i_min=i; j_min=j; min2=a; }
                  }
              }

          gi=group[i_min]; gj=group[j_min];
          for (i=0; i<m; ++i)
              if (group[i]==gj) group[i]=gi;
          --k;
          } /* while (k>ng) */

        k=0;
        for (i=0; i<ng; ++i) gsize[i]=0;
        for (i=0; i<m; ++i)
            {
            gi=group[i];
            for (j=0; j<k; ++j) if (gi==g1[j]) break;
            if (j==k) { g1[k]=gi; ++k; }
            group[i]=j;
            ++gsize[j];
            }
        return(1);
        }

static int init_groups1(int var)
        {
        int i,h,k,ok,len;
        int j;
        double a;
        char ots[16],*pots;
        char ots2[LNAME];

        k=0; j=d.l1;
        for (i=0; i<m; ++i)
            {
            strncpy(ots,rlab+lr*i,lr); ots[lr]=EOS;
            pots=ots; while (*pots==' ') ++pots;
            len=strlen(pots);
            while (1)
                {
                ok=0;
                if (label_var>=0)
                    {
                    data_alpha_load(&d,j,label_var,ots2);
                    if (strncmp(pots,ots2,len)==0) ok=1;
                    }
                else
                    {
                    if (atol(ots)==j) ok=1;
                    }
                if (ok)
                    {
                    data_load(&d,j,var,&a);
                    h=group[i]=(int)a-1;
                    if (h>k) k=h;
                    ++j;
                    break;
                    }
                ++j;
                if (j>d.l2)
                    {
                    sprintf(sbuf,"\nCannot find observation %s!",ots);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                }
            }
        return(k);
        }

static int save_grouping()
        {
        int j;
        int i,kk,g_min,ok,len;
        double min2,a;
        char ots[16],*pots;
        char ots2[LNAME];

        if (method!=3) { for (i=0; i<ng; ++i) gsize[i]=0; }

        j=d.l1;
        for (i=0; i<m; ++i)
            {
            if (method==3) g_min=group[i];
            else
                {
                min2=xx[i+m*medoid[0]]; g_min=0;
                for (kk=1; kk<ng; ++kk)
                    {
                    a=xx[i+m*medoid[kk]];
                    if (a<min2) { min2=a; g_min=kk; }
                    }

                if (silh_var>=0) group[i]=g_min;
                ++gsize[g_min];
                }

            strncpy(ots,rlab+lr*i,lr); ots[lr]=EOS;
            pots=ots; while (*pots==' ') ++pots;
            len=strlen(pots);
            while (1)
                {
                ok=0;
                if (label_var>=0)
                    {
                    data_alpha_load(&d,j,label_var,ots2);
                    if (strncmp(pots,ots2,len)==0) ok=1;
                    }
                else
                    {
                    if (atol(ots)==j) ok=1;
                    }
                if (ok)
                    {
                    data_save(&d,j,gvar,(double)(g_min+1));
                    ++j;
                    break;
                    }
                ++j;
                if (j>d.l2)
                    {
                    sprintf(sbuf,"\nCannot find observation %s!",ots);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                }
            }
        return(1);
        }

static int save_silhouettes()
        {
        int j;
        int i,h,k,g_min,ok,len;
        double a,aa,bb;
        char ots[16],*pots;
        char ots2[LNAME];

        for (k=0; k<ng; ++k) ssum[k]=0.0;
        j=d.l1;
        for (i=0; i<m; ++i)
            {

            strncpy(ots,rlab+lr*i,lr); ots[lr]=EOS;
            pots=ots; while (*pots==' ') ++pots;
            len=strlen(pots);
            while (1)
                {
                ok=0;
                if (label_var>=0)
                    {
                    data_alpha_load(&d,j,label_var,ots2);
                    if (strncmp(pots,ots2,len)==0) ok=1;
                    }
                else
                    {
                    if (atol(ots)==j) ok=1;
                    }
                if (ok)
                    {
                    g_min=group[i];
                    for (h=0; h<ng; ++h) dd[h]=0.0;
                    for (k=0; k<m; ++k)
                        dd[group[k]]+=xx[i+m*k];
                    aa=dd[g_min]/(gsize[g_min]-1);
                    bb=1e100;
                    for (h=0; h<ng; ++h)
                        {
                        if (h==g_min) continue;
                        a=dd[h]/gsize[h];
                        if (a<bb) bb=a;
                        }
                    a=aa; if (bb>aa) a=bb;
                    a=(bb-aa)/a;
                    data_save(&d,j,silh_var,a);
                    ssum[g_min]+=a;
                    ++j;
                    break;
                    }
                ++j;

                if (j>d.l2)
                    {
                    sprintf(sbuf,"\nCannot find observation %s!",ots);
                    sur_print(sbuf); WAIT; return(-1);
                    }

                }
            }
        return(1);
        }

static int print_results()
        {
        int i;
        char med_name[16];
        double mean;

        i=output_open(eout); if (i<0) return(-1);
        eoutput("Cluster analysis by medoids of Kaufman and Rousseeuw (1987)");
        sprintf(sbuf,"Data %s  N=%d",word[1],m);
        eoutput(sbuf);
        if (silh_var>=0)
          {
          eoutput("Group Medoid       n Mean (of silhouette values)");
          mean=0.0;
          for (i=0; i<ng; ++i)
              {
              strncpy(med_name,rlab+medoid[i]*lr,lr); med_name[lr]=EOS;
              sprintf(sbuf,"%2d    %*.*s%6d %3.3g",
                      i+1,lr,lr,med_name,gsize[i],ssum[i]/gsize[i]);
              eoutput(sbuf);
              mean+=ssum[i];
              }
          mean/=m;
          sprintf(sbuf,"Mean of all silhouette values is %3.3g",mean);
          eoutput(sbuf);
          }
        else
          {
          eoutput("Group Medoid       n");
          for (i=0; i<ng; ++i)
              {
              strncpy(med_name,rlab+medoid[i]*lr,lr); med_name[lr]=EOS;
              sprintf(sbuf,"%2d    %*.*s%6d",
                      i+1,lr,lr,med_name,gsize[i]);
              eoutput(sbuf);
              }
          }
        eoutput(" ");
        output_close(eout);
        return(1);
        }

static int print_results3()
        {
        int i;

        i=output_open(eout); if (i<0) return(-1);

        i=sprintf(sbuf,"Cluster analysis by the single linkage method ");
        if (weight_ind) sprintf(sbuf+i,"(WEIGHT=%g)",weight);
        eoutput(sbuf);
        sprintf(sbuf,"Data %s  N=%d",word[1],m);
        eoutput(sbuf);
        eoutput("Group      n");
        for (i=0; i<ng; ++i)
            {
            sprintf(sbuf,"%2d    %6d",i+1,gsize[i]);
            eoutput(sbuf);
            }
        eoutput(" ");
        output_close(eout);
        return(1);
        }

static int eoutput(char *rivi)
        {
        output_line(rivi,eout,tulosrivi);
        if (tulosrivi) ++tulosrivi;
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!"); WAIT; return(1);
        }

