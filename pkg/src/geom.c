/* _geom.c 10.2.2008/SM (25.8.2008)
*/
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
// #include <conio.h>
#include <math.h>
// #include <malloc.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXPAR 10
#define LEN 16
#define MAX 1000

// char **specs;
static char name[LLENGTH];
static char rivi[LLENGTH],*s[MAXPAR];

static char *object_name[]={ "point", "point_l", "point_c", "line", "circle", "circle_p", "edge", "cross",
                      "cross_cl", "cross_cc", "save", "sample", "symbol",
                      "perpendicular", "midpoint", "parallel", "bisect_a", "stddev", "ortho" };
#define N_OBJ 19

#define POINT 0
#define POINT_L 1
#define POINT_C 2
#define LINE 3
#define CIRCLE 4
#define CIRCLE_P 5
#define EDGE 6
#define CROSS 7
#define CROSS_CL 8
#define CROSS_CC 9
#define SAVE 10
#define SAMPLE 11
#define SYMBOL 12
#define PERPENDICULAR 13
#define MIDPOINT 14
#define PARALLEL 15
#define BISECT_A 16
#define STDDEV 17
#define ORTHO 18

static int n_objects;
static int object[MAX];
static int n_object[MAX];
static char param[MAX][MAXPAR][LEN];

static int n_points;
static char point_name[MAX][LEN];
static double point_x[MAX];
static double point_y[MAX];
static double point_k1[MAX];
static double point_k2[MAX];

static int n_lines;
static char line_name[MAX][LEN];
static double line_x1[MAX];
static double line_y1[MAX];
static double line_x2[MAX];
static double line_y2[MAX];

static int n_circles;
static char circle_name[MAX][LEN];
static double circle_x[MAX];
static double circle_y[MAX];
static double circle_r[MAX];
static double circle_a1[MAX];
static double circle_a2[MAX];

static int n_edges;
static char edge_name[MAX][LEN];
static double edge_x1[MAX];
static double edge_y1[MAX];
static double edge_x2[MAX];
static double edge_y2[MAX];
static double edge_len[MAX];

static SURVO_DATA_FILE fi;
static int create_files=1;
static int samples,n_samples;
static int sample_i;
static double stddev;
static FILE *result;
static char result_name[LNAME];
static int res_line;
static int disp_count;
static int n_disp=10000;
static char data_line[LLENGTH];
static int distr=1;
static int model=0;
static int point_on_line=0;

// Geometrographical measures
static int gs1,gs2,gc1,gc2,gc3;
static int current_center;
static int star_point=0; // used in perpendicular etc.

// Range of the construction
static double rx_min,rx_max,ry_min,ry_max;
static double dx1,dy1,dx2,dy2;

static const double pi=3.141592653589793;
static const double pih=0.5*3.141592653589793;
static const double pi4=0.25*3.141592653589793;



static int get_midpoint(double x1,double y1,double x2,double y2,char *pname,double *px,double *py);
static int minmax_x(double x);
static int minmax_y(double y);
static int write_line(int j);
static int get_slope_tangent(double *p_slope,double x,double y,double x0,double y0);
static int get_slope(double *p_slope,double x1,double y1,double x2,double y2);
static int randomize(double *px,double *py,double x,double y,double a1,double a2,double h);
static int save_files();
static int range_save(int n,double *rr);
static int get_point(int *exact,double *px,double *py,char *pname);
static int get_line(double *px1,double *py1,double *px2,double *py2,char *pname);
static int get_edge(double *px,char *pname,int error);
static int get_circle(double *px,double *py,double *pr,char *pname);
static int parsi(int j);
static int islower(int ch);

/********************************************************
static double muste_arit_atof(char *lauseke);
static int muste_arit_atoi(char *lauseke);
static int laske(char *lauseke,double *y);
static double luku(char *sana,int len);
static double oper(double x1,double x2,char laji);
static double power(double x,double y);
static int supista(int *t,double opnd[],char op[],int v[]);
static double funktio(char *s,double x);
static int f_tuntematon(char *s);
static int arg_virhe(char *s);
static int syntax_error(char *s);
static int laske2(char *muuttuja,double *y);
static double probit(double z);
static int varif(char *s,double a)  { return(1); };
static int read_loopar();
static int varnimet();
static int f_edit(char *s,char *x,int n,double *py)  { return(1); };
static int p_error(char *s);
static int p_end() { return(1); };
***********************************************************/
extern double muste_arit_atof();

// GEOM L1,L2,L,<file>
/*****************************
void main(argc,argv)
int argc; char *argv[];
*****************************/

void muste_geom(char *argv)
    {
    int i,j,k,j1,j2;
    int il,ic;
    int n_param;
    double x,y,x0,y0,radius,radius2;
    double x1,y1,x2,y2,x3,y3,x4,y4;
    double xv,yv; // apupiste
    double xx1,yy1,xx2,yy2;
    double a,b,c,d,f,aa,bb,cc,dd;
    double sl11,sl12,sl21,sl22;  // slopes
    char *p;
    int exact,ex;
    char ch;

//  if (argc==1) return;
    s_init(argv);

    i=spec_init(r1+r-1); if (i<0) return;
    spec_rnd();

    n_disp=10000;
    distr=1;
    model=0;
    point_on_line=0;
    star_point=0;

    samples=0;
    i=spfind("SAMPLES");
    if (i>=0) samples=n_samples=atoi(spb[i]);
    if (samples)
        {
        stddev=0.001;
        i=spfind("STDDEV");
        if (i>=0) stddev=muste_arit_atof(spb[i]);

        distr=1; model=0;
        i=spfind("DISTR");
        if (i>=0) distr=atoi(spb[i]);
        i=spfind("MODEL"); // alternative spec.
        if (i>=0)
            {
            model=i=atoi(spb[i]);
            if (i==0) distr=1;
            else if (i==2) distr=3;
            else if (i==3) distr=4; // 29.6.2008
            else distr=2;
            }
        sprintf(sbuf,"\nMODEL=%d",model); sur_print(sbuf);
        strcpy(result_name,"Geom.TXT");
        if (g>=5) strcpy(result_name,word[4]);
        result=muste_fopen(result_name,"wt");
        }
    else n_samples=1;

    i=spfind("RESULTS"); if (i>=0) results=atoi(spb[i]);

    j1=edline2(word[1],1,1); if (j1==0) return;
    j2=edline2(word[2],j1,1); if (j2==0) return;
    res_line=0;
    if (g>3 && strcmp("0",word[3])!=0 )
      {
      res_line=edline2(word[3],1,1); if (res_line==0) return;
// printf("\nres_line=%d j1=%d J2=%d|",res_line,j1,j2); getch();
      if (res_line<j2+1)
          {
          sprintf(sbuf,"\nLine %s not accepted for the summary of objects!",
             word[3]); sur_print(sbuf); WAIT; return;
          }
      }
// printf("\nj1=%d j2=%d|",j1,j2); getch();
    edread(sbuf,j1); strcpy(name,sbuf+1);

    n_objects=0;  strcpy(data_line," ");
    for (j=j1+1; j<=j2; ++j)
        {
        n_param=parsi(j);
        if (n_param==0) continue;

//     printf("\ns[0]=%s s[1]=%s s[2]=%s",s[0],s[1],s[2]); getch();
        if (muste_strcmpi(s[0],"save")==0)
            {
           if (muste_strcmpi(s[1],"edge")==0)
               {
               strcat(data_line,s[2]); strcat(data_line," ");
               }
           else if (muste_strcmpi(s[1],"point")==0)
               {
               strcat(data_line,s[2]); strcat(data_line,"X ");
               strcat(data_line,s[2]); strcat(data_line,"Y ");
               }
           else if (muste_strcmpi(s[1],"line")==0)
               {
               strcat(data_line,s[2]); strcat(data_line,"X1 ");
               strcat(data_line,s[2]); strcat(data_line,"Y1 ");
               strcat(data_line,s[2]); strcat(data_line,"X2 ");
               strcat(data_line,s[2]); strcat(data_line,"Y2 ");
               }
           else if (muste_strcmpi(s[1],"circle")==0)
               {
               strcat(data_line,s[2]); strcat(data_line,"X ");
               strcat(data_line,s[2]); strcat(data_line,"Y ");
               strcat(data_line,s[2]); strcat(data_line,"R ");
               }
           else
               {
               sur_print("\nOnly points, edges, lines, and circles can be saved!");
               WAIT; return;
               }
//          strcat(data_line," ");
            p=s[0]; s[0]=s[1]; s[1]=p;
            }

        if (muste_strcmpi(s[0],"symbol")==0)
            {
            s[1]=s[0];
            }

        if (muste_strcmpi(s[0],"stddev")==0)
            {
            s[2]=s[1];
            s[1]=s[0]; n_param=3;

            }

//      printf("\nrivi %d: ",j);
//      for (i=0; i<k; ++i) printf("%s ",s[i]);

        for (i=0; i<N_OBJ; ++i)
            {
            if (muste_strcmpi(s[1],object_name[i])==0)
                {
                ++n_object[i];
                break;
                }
            }
        if (i==N_OBJ)
            {
            sprintf(sbuf,"\nUnknown object %s on line %d",s[1],j);
            sur_print(sbuf); WAIT; return;
            }
        object[n_objects]=i;

        strcpy(param[n_objects][0],s[0]);
        for (i=2; i<n_param; ++i) strcpy(param[n_objects][i-1],s[i]);
        for (i=n_param; i<MAXPAR; ++i) *param[n_objects][i-1]=EOS;

        ++n_objects;
        } // j

   if (samples)
       {
       i=strlen(data_line)-1; data_line[i]=EOS;
       fprintf(result,"%s\n",data_line);
       }
   else
       {
       rx_min=1000.0; rx_max=-1000.0;
       ry_min=1000.0; ry_max=-1000.0;
       }

   disp_count=0;
   for (sample_i=0; sample_i<n_samples; ++sample_i)
   {
    *data_line=EOS;
    ++disp_count;
    if (samples && disp_count==n_disp)
        {
        sprintf(sbuf," %d",sample_i+1); sur_print(sbuf);
        disp_count=0;
        }
    n_points=n_lines=n_circles=n_edges=0;
    for (j=0; j<n_objects; ++j)
        {
        if (!samples)
            {
            k=sprintf(sbuf,"\n%d: %s ",j,object_name[object[j]]);
            for (i=0; i<MAXPAR-1; ++i) k+=sprintf(sbuf+k,"%s ",param[j][i]);
            sur_print(sbuf);
            }
        switch (object[j])
            {
          case POINT: strcpy(point_name[n_points],param[j][0]);
                          x=point_x[n_points]=muste_arit_atof(param[j][1]);
                          y=point_y[n_points]=muste_arit_atof(param[j][2]);
                          minmax_x(x);
                          minmax_y(y);

                          point_k1[n_points]=pi4;
                          point_k2[n_points]=-pi4;
                     //   point_used_as_center[n_points]=0;
                      ++n_points;
                      break;
// P=point_l(L,x0,y0)
          case POINT_L: strcpy(point_name[n_points],param[j][0]);
                          if (!samples)
                              {
                              x=point_x[n_points]=muste_arit_atof(param[j][2]);
                              y=point_y[n_points]=muste_arit_atof(param[j][3]);
                              minmax_x(x);
                              minmax_y(y);
                              ++gc2; point_on_line=1;
                              }
                          else
                              {
                              il=get_line(&x1,&y1,&x2,&y2,param[j][1]);
                              if (il<0) return;
                              x0=muste_arit_atof(param[j][2]);
                              y0=muste_arit_atof(param[j][3]);
                              randomize(&xv,&yv,x0,y0,1.0,-1.0,stddev);
                            // projection:
//            printf("\nx1=%g y1=%g x2=%g y2=%g x0=%g y0=%g xv=%g yv=%g",x1,y2,x2,y2,x0,y0,xv,yv); getch();

                              a=(x2-x1)*(x2-x1); b=(y2-y1)*(y2-y1);
                              c=(x2-x1)*(y2-y1);
                              point_x[n_points]=(c*(y0-yv)+a*x0+b*xv)/(a+b);
                              point_y[n_points]=(c*(x0-xv)+b*y0+a*yv)/(a+b);
                              }

                          point_k1[n_points]=pi4;
                          point_k2[n_points]=-pi4;
                      ++n_points;
                      break;

// P=point_c(C,x0,y0)
          case POINT_C: strcpy(point_name[n_points],param[j][0]);
                          if (!samples)
                              {
                              x=point_x[n_points]=muste_arit_atof(param[j][2]);
                              y=point_y[n_points]=muste_arit_atof(param[j][3]);
                              minmax_x(x);
                              minmax_y(y);
                              ++gc2; point_on_line=1;
                              }
                          else
                              {
                              ic=get_circle(&x1,&y1,&radius,param[j][1]);
                              if (ic<0) return;
                              x2=muste_arit_atof(param[j][2]);
                              y2=muste_arit_atof(param[j][3]);

                              randomize(&radius2,&a,radius,0.0,1.0,-1.0,stddev); // a ylim„„r„isesti
                              if (x1!=x2) a=atan((y2-y1)/(x2-x1)); else a=pih;

                              point_x[n_points]=x1+radius2*cos(a);
                              point_y[n_points]=y1+radius2*sin(a);
                              }

                          point_k1[n_points]=pi4;
                          point_k2[n_points]=-pi4;
                      ++n_points;
                      break;

          case LINE:  strcpy(line_name[n_lines],param[j][0]);
                      exact=0;
                      if (*line_name[n_lines]=='_')
                          {
                          exact=1;
                          strcpy(line_name[n_lines],param[j][0]+1);
                          }
                      if (!samples)
                          {
                          ++gs2;
                          }

                      k=0;
                      if (!samples || (samples && exact) )
                          {
                          i=get_point(&ex,&line_x1[n_lines],&line_y1[n_lines],param[j][1]);
                          if (i<0) return;
                          if (!ex) ++k;
                          i=get_point(&ex,&line_x2[n_lines],&line_y2[n_lines],param[j][2]);
                          if (i<0) return;
                          if (!ex) ++k;
                          if (!exact) gs1+=k;
                          }
                      else
                          {
                          i=get_point(&ex,&x,&y,param[j][1]);
                          if (i<0) return;
                          if (ex) { line_x1[n_lines]=x; line_y1[n_lines]=y; }
                          else randomize(&line_x1[n_lines],&line_y1[n_lines],x,y,point_k1[i],point_k2[i],stddev);
                          i=get_point(&ex,&x,&y,param[j][2]);
                          if (i<0) return;
                          if (ex) { line_x2[n_lines]=x; line_y2[n_lines]=y; }
                          else randomize(&line_x2[n_lines],&line_y2[n_lines],x,y,point_k1[i],point_k2[i],stddev);
                          }

//              printf("\nLine: %g %g %g %g",line_x1[n_lines],line_y1[n_lines],line_x2[n_lines],line_y2[n_lines]);
//              getch();
                      ++n_lines;
                      break;

          case CIRCLE:  strcpy(circle_name[n_circles],param[j][0]);
                        exact=0;
                        if (*circle_name[n_circles]=='_')
                            {
                            exact=1;
                            strcpy(circle_name[n_circles],param[j][0]+1);
                            }
                        strcpy(sbuf,param[j][1]);
                        if (!samples)
                            {
                            ++gc3;
                            if (!exact)
                                {
                                if (*sbuf!='*' && !point_on_line) ++gc1;
                                }
                            }
                        point_on_line=0;
              //        if (*sbuf=='*') point_name[current_center];   // ???????
                        if (!samples || (samples && exact) )
                            {
                            i=get_point(&ex,&circle_x[n_circles],&circle_y[n_circles],sbuf);
                            if (i<0) return;
                            }
                        else
                            {
                            i=get_point(&ex,&x,&y,sbuf);
                            if (i<0) return;
                            if (ex) { circle_x[n_circles]=x; circle_y[n_circles]=y; }
                       else randomize(&circle_x[n_circles],&circle_y[n_circles],x,y,point_k1[i],point_k2[i],stddev);
                            }

                       current_center=i;

                       strcpy(sbuf,param[j][2]);
                       if (!samples && !exact)
                            {
                            if (*sbuf!='*') gc1+=2;
                            }

                       if (*sbuf=='*') strcpy(sbuf,param[j][2]+1);
// printf("\nsbuf=%s spfind=%d isnumber=%d",sbuf,spfind(sbuf),isnumber(sbuf)); getch();
                       i=get_edge(&circle_r[n_circles],sbuf,0);
                       if (i<0)
                         {
                         if (spfind(sbuf)>=0 || muste_isnumber(sbuf))
                             circle_r[n_circles]=muste_arit_atof(sbuf);
                         else
                            {
                            i=get_edge(&circle_r[n_circles],sbuf,1); // To error message only!
                            if (i<0) return;
                            }
                         }
                        circle_a1[n_circles]=1.0;
                        circle_a2[n_circles]=0.0;

                        if (*param[j][3]!=EOS && *param[j][4]!=EOS)
                            {
                            x1=muste_arit_atof(param[j][3]);
                            x2=muste_arit_atof(param[j][4]);
                            circle_a1[n_circles]=(x2-x1)/2;
                            circle_a2[n_circles]=x1*pi;
                            }
                        ++n_circles;

// printf("\nnc=%d",n_circles); getch();
                        break;

          case CIRCLE_P:  strcpy(circle_name[n_circles],param[j][0]);
                        exact=0;
                        if (*circle_name[n_circles]=='_')
                            {
                            exact=1;
                            strcpy(circle_name[n_circles],param[j][0]+1);
                            }

                        strcpy(sbuf,param[j][1]);
                        if (!samples)
                            {
                            ++gc3;
                            if (*param[j][2]!='*') ++gc1;
                            if (*sbuf!='*' && !point_on_line) ++gc1;
                     //     if (*sbuf!='*') ++gc1;

                            }
                        if (*sbuf=='*') strcpy(sbuf,point_name[current_center]);

                        if (!samples || (samples && exact) )
                            {
                            ic=get_point(&ex,&x1,&y1,sbuf);
                            if (ic<0) return;
                            get_point(&ex,&x2,&y2,param[j][2]);
                            }
                        else
                            {
                            ic=get_point(&ex,&x,&y,sbuf);
                            if (ic<0) return;
                            if (ex) { x1=x; y1=y; }
                            else randomize(&x1,&y1,x,y,point_k1[ic],point_k2[ic],stddev);
                            i=get_point(&ex,&x,&y,param[j][2]);
                            if (i<0) return;
                            if (ex) { x2=x; y2=y; }
                            else randomize(&x2,&y2,x,y,point_k1[i],point_k2[i],stddev);
                            }

                        current_center=ic;

                        circle_x[n_circles]=x1; circle_y[n_circles]=y1;
                        circle_r[n_circles]=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));

                        circle_a1[n_circles]=1.0;
                        circle_a2[n_circles]=0.0;

                        if (*param[j][3]!=EOS && *param[j][4]!=EOS)
                            {
                            x1=muste_arit_atof(param[j][3]);
                            x2=muste_arit_atof(param[j][4]);
                            circle_a1[n_circles]=(x2-x1)/2;
                            circle_a2[n_circles]=x1*pi;
                            }
                        ++n_circles;

// printf("\nnc=%d",n_circles); getch();
                        break;

          case EDGE:  strcpy(edge_name[n_edges],param[j][0]);
                      if (!samples)
                          {
                          i=get_point(&ex,&x1,&y1,param[j][1]);
                          if (i<0) return;
                          i=get_point(&ex,&x2,&y2,param[j][2]);
                          if (i<0) return;
                     //   gs1+=2;   30.3.2008
                          }
                      else
                          {
                          i=get_point(&ex,&x,&y,param[j][1]);
                          if (i<0) return;
                          if (ex) { x1=x; y1=y; }
                          else randomize(&x1,&y1,x,y,point_k1[i],point_k2[i],stddev);
                          i=get_point(&ex,&x,&y,param[j][2]);
                          if (i<0) return;
                          if (ex) { x2=x; y2=y; }
                          randomize(&x2,&y2,x,y,point_k1[i],point_k2[i],stddev);
                          }
                      edge_x1[n_edges]=x1;  edge_y1[n_edges]=y1;
                      edge_x2[n_edges]=x2;  edge_y2[n_edges]=y2;
                      edge_len[n_edges]=sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));
// printf("\nedge=%g",edge_len[n_edges]); getch();
                      ++n_edges;
                      break;
// L=bisect_a(L1,L2,angle)
          case BISECT_A:
// P=cross(L1,L2)
          case CROSS:  strcpy(point_name[n_points],param[j][0]);
                       il=get_line(&x1,&y1,&x2,&y2,param[j][1]);
                       if (il<0) return;
                       ic=get_line(&x3,&y3,&x4,&y4,param[j][2]);
                       if (ic<0) return;
                       b=(x1-x2)*(y4-y3)+(x4-x3)*(y2-y1);
                       if (fabs(b)<0.0000000000001)  // 6.10.2008
                           {
                           sprintf(sbuf,"\nLine %s does not intersect line %s!",
                                                  line_name[il],line_name[ic]);
                           sur_print(sbuf); WAIT; return;
                           }

                       point_x[n_points]=(x2*x4*(y3-y1)+x2*x3*(y1-y4)+x1*x4*(y2-y3)+x1*x3*(y4-y2))/b;
                       point_y[n_points]=(y1*y4*(x3-x2)+y1*y3*(x2-x4)+y2*y4*(x1-x3)+y2*y3*(x4-x1))/b;
                       get_slope(&point_k1[n_points],x1,y1,x2,y2);
                       get_slope(&point_k2[n_points],x3,y3,x4,y4);
         if (!samples)
               {
                       minmax_x(point_x[n_points]);
                       minmax_y(point_y[n_points]);

               sprintf(sbuf,"\ncross: %g %g",point_x[n_points],point_y[n_points]);
               sur_print(sbuf);
               }

                       if (object[j]==CROSS) { ++n_points; break; }

// Bisect angle   (point above temporary only!)

                       if (!samples)
                           { gs1+=2; gs2+=1; gc1+=3; gc3+=3; }

                       strcpy(line_name[n_lines],param[j][0]);
                       a=muste_arit_atof(param[j][3]);
                       il=n_points;
                       x1=point_k1[il];
                       x2=point_k2[il];
                       y1=(x1+x2)/2.0;
                       y2=y1+pih;
                       y=y1; if (fabs(y2-a)<fabs(y1-a)) y=y2;
                       il=n_points;
        randomize(&x1,&y1,point_x[il],point_y[il],point_k1[il],point_k2[il],stddev);
                       line_x1[n_lines]=x1;
                       line_y1[n_lines]=y1;
                       line_x2[n_lines]=x1+cos(y);
                       line_y2[n_lines]=y1+sin(y);
                       ++n_lines;
                       break;


          case CROSS_CL:  strcpy(point_name[n_points],param[j][0]);
                        ic=get_circle(&x0,&y0,&radius,param[j][1]);
                        if (ic<0) return;
                        il=get_line(&x1,&y1,&x2,&y2,param[j][2]);
                        if (il<0) return;
                    //  get_point(&ex,&xv,&yv,param[j][3]);
                        xv=muste_arit_atof(param[j][3]); yv=muste_arit_atof(param[j][4]);

                        if (fabs(x1-x2)>fabs(y1-y2))
                            {
                            b=(y2-y1)/(x2-x1);
                            aa=1+b*b;
                            bb=-2*x0+2*b*(y1-y0)-2*b*b*x1;
                            cc=x0*x0+(y1-y0)*(y1-y0)-2*b*x1*(y1-y0)+(b*x1)*(b*x1)-radius*radius;
                            dd=bb*bb-4*aa*cc;
                            if (dd<0.0)
                                {
                                sprintf(sbuf,"\nLine %s does not intersect circle %s!",
                                                       line_name[il],circle_name[ic]);
                                sur_print(sbuf); WAIT; return;
                                }
                            xx1=(-bb+sqrt(dd))/(2*aa); yy1=y1+b*(xx1-x1);
                            xx2=(-bb-sqrt(dd))/(2*aa); yy2=y1+b*(xx2-x1);
                            }
                        else
                            {
                            b=(x2-x1)/(y2-y1);
                            aa=1+b*b;
                            bb=-2*y0+2*b*(x1-x0)-2*b*b*y1;
                            cc=y0*y0+(x1-x0)*(x1-x0)-2*b*y1*(x1-x0)+(b*y1)*(b*y1)-radius*radius;
                            dd=bb*bb-4*aa*cc;
                            if (dd<0.0)
                                {
                                sprintf(sbuf,"\nLine %s does not intersect circle %s!",
                                                       line_name[il],circle_name[ic]);
                                sur_print(sbuf); WAIT; return;
                                }
                            yy1=(-bb+sqrt(dd))/(2*aa); xx1=x1+b*(yy1-y1);
                            yy2=(-bb-sqrt(dd))/(2*aa); xx2=x1+b*(yy2-y1);
                            }

                            x=xx1; y=yy1;
                            get_slope(&sl11,x1,y1,x2,y2);
                            get_slope_tangent(&sl12,x,y,x0,y0);

                            x=xx2; y=yy2;
                            sl21=sl11;
                            get_slope_tangent(&sl22,x,y,x0,y0);

                        if ((xx1-xv)*(xx1-xv)+(yy1-yv)*(yy1-yv)<(xx2-xv)*(xx2-xv)+(yy2-yv)*(yy2-yv))
                            {
                            point_x[n_points]=xx1;  point_y[n_points]=yy1;
                            point_k1[n_points]=sl11; point_k2[n_points]=sl12;
                            }
                        else
                            {
                            point_x[n_points]=xx2;  point_y[n_points]=yy2;
                            point_k1[n_points]=sl21; point_k2[n_points]=sl22;
                            }
         if (!samples)
              {
                            minmax_x(point_x[n_points]);
                            minmax_y(point_y[n_points]);

              sprintf(sbuf,"\ncross_cl: %g %g",point_x[n_points],point_y[n_points]);
              sur_print(sbuf);
              }
                        ++n_points;
                        break;

          case CROSS_CC:  strcpy(point_name[n_points],param[j][0]);
                        ic=get_circle(&x1,&y1,&radius,param[j][1]);
                        if (ic<0) return;
                        il=get_circle(&x2,&y2,&radius2,param[j][2]);
                        if (il<0) return;
         //             get_point(&ex,&xv,&yv,param[j][3]);
                        xv=muste_arit_atof(param[j][3]); yv=muste_arit_atof(param[j][4]);

                        if (fabs(x1-x2)>fabs(y1-y2))
                            {
                            a=2*(x2-x1); b=2*(y2-y1);
                            f=(x1-x2)*(x1+x2)+(y1-y2)*(y1+y2)+(radius2-radius)*(radius2+radius);
                            c=-b/a; d=-f/a;
                            aa=1+c*c; bb=2*c*(d-x1)-2*y1; cc=(d-x1)*(d-x1)+y1*y1-radius*radius;

                            dd=bb*bb-4*aa*cc;
// printf("\ndd1=%g",dd); getch();
                            if (dd<0.0)
                                {
                                sprintf(sbuf,"\nCircle %s does not intersect circle %s!",
                                                       circle_name[ic],circle_name[il]);
                                sur_print(sbuf); WAIT; return;
                                }
                            yy1=(-bb+sqrt(dd))/(2*aa); xx1=c*yy1+d;
                            yy2=(-bb-sqrt(dd))/(2*aa); xx2=c*yy2+d;
                            }

                        else
                            {
                            a=2*(y2-y1); b=2*(x2-x1);
                            f=(y1-y2)*(y1+y2)+(x1-x2)*(x1+x2)+(radius2-radius)*(radius2+radius);
                            c=-b/a; d=-f/a;
                            aa=1+c*c; bb=2*c*(d-y1)-2*x1; cc=(d-y1)*(d-y1)+x1*x1-radius*radius;

                            dd=bb*bb-4*aa*cc;
// printf("\ndd2=%g",dd); getch();
                            if (dd<0.0)
                                {
                                sprintf(sbuf,"\nCircle %s does not intersect circle %s!",
                                                       circle_name[ic],circle_name[il]);
                                sur_print(sbuf); WAIT; return;
                                }
                            xx1=(-bb+sqrt(dd))/(2*aa); yy1=c*xx1+d;
                            xx2=(-bb-sqrt(dd))/(2*aa); yy2=c*xx2+d;
                            }

                            x=xx1; y=yy1;
                            get_slope_tangent(&sl11,x,y,x1,y1);
                            get_slope_tangent(&sl12,x,y,x2,y2);

                            x=xx2; y=yy2;
                            get_slope_tangent(&sl21,x,y,x1,y1);
                            get_slope_tangent(&sl22,x,y,x2,y2);


                        if ((xx1-xv)*(xx1-xv)+(yy1-yv)*(yy1-yv)<(xx2-xv)*(xx2-xv)+(yy2-yv)*(yy2-yv))
                            {
                            point_x[n_points]=xx1;  point_y[n_points]=yy1;
                            point_k1[n_points]=sl11; point_k2[n_points]=sl12;
                            }
                        else
                            {
                            point_x[n_points]=xx2;  point_y[n_points]=yy2;
                            point_k1[n_points]=sl21; point_k2[n_points]=sl22;
                            }
if (!samples)
              {
                            minmax_x(point_x[n_points]);
                            minmax_y(point_y[n_points]);
              sprintf(sbuf,"\ncross_cc: %g %g",point_x[n_points],point_y[n_points]);
              sur_print(sbuf);
              }
                        ++n_points;
                        break;

          case SAVE: // printf("\nsave: %s %s %s",param[j][0],param[j][1],param[j][2]); getch();
                     if (!samples) break;
                     if (muste_strcmpi(param[j][0],"edge")==0)
                         {
                         i=get_edge(&x,param[j][1],1);
                         if (i<0) return;
                         sprintf(sbuf," %16.16g",x);
                         strcat(data_line,sbuf);
                         }
                     else if (muste_strcmpi(param[j][0],"point")==0)
                         {
                         i=get_point(&ex,&x,&y,param[j][1]);
                         if (i<0) return;
                         sprintf(sbuf," %16.16g",x);
                         strcat(data_line,sbuf);
                         sprintf(sbuf," %16.16g",y);
                         strcat(data_line,sbuf);
                         }
                     else if (muste_strcmpi(param[j][0],"line")==0)
                         {
                         i=get_line(&x1,&y1,&x2,&y2,param[j][1]);
                         if (i<0) return;
                         sprintf(sbuf," %16.16g",x1);
                         strcat(data_line,sbuf);
                         sprintf(sbuf," %16.16g",y1);
                         strcat(data_line,sbuf);
                         sprintf(sbuf," %16.16g",x2);
                         strcat(data_line,sbuf);
                         sprintf(sbuf," %16.16g",y2);
                         strcat(data_line,sbuf);
                         }
                     else // circle
                         {
                         i=get_circle(&x,&y,&radius,param[j][1]);
                         if (i<0) return;
                         sprintf(sbuf," %16.16g",x);
                         strcat(data_line,sbuf);
                         sprintf(sbuf," %16.16g",y);
                         strcat(data_line,sbuf);
                         sprintf(sbuf," %16.16g",radius);
                         strcat(data_line,sbuf);
                         }
                     break;
// S=sample(P)
          case SAMPLE: if (!samples) break;
                       strcpy(point_name[n_points],param[j][0]);
                       i=get_point(&ex,&x,&y,param[j][1]);
                       if (i<0) return;
                       randomize(&point_x[n_points],&point_y[n_points],x,y,point_k1[i],point_k2[i],stddev);
                       point_k1[n_points]=point_k1[i];  point_k2[n_points]=point_k2[i];
                       ++n_points;

                     break;

          case SYMBOL: if (samples) break;
                     gs1=gs2=gc1=gc2=gc3=0;
                     break;


// L2=perpendicular(L,P)
          case PERPENDICULAR:
                     strcpy(line_name[n_lines],param[j][0]);

                     if (!samples)
                         {
                         i=get_point(&ex,&x,&y,param[j][2]);
                         if (i<0) return;
                         if (star_point)
                             { gs1+=2; gs2+=1; gc1+=2; gc3+=2; star_point=0; }
                         else
                             { gs1+=2; gs2+=1; gc1+=3; gc3+=3; }
                         }
                     else
                         {
                         i=get_point(&ex,&x2,&y2,param[j][2]);
                         if (i<0) return;
                         if (!ex)
                         randomize(&x,&y,x2,y2,point_k1[i],point_k2[i],stddev);
                         }
                     il=get_line(&x1,&y1,&x2,&y2,param[j][1]);
                     if (il<0) return;
                     if (fabs(x2-x1)<1e-10) a=-pih;
                     else a=atan((y2-y1)/(x2-x1));
                     a+=pih;
                     line_x1[n_lines]=x; line_y1[n_lines]=y;
                     line_x2[n_lines]=x+cos(a); line_y2[n_lines]=y+sin(a);

                     ++n_lines;

                     break;


// R=midpoint(P,Q,L)   8.7.2008-
          case MIDPOINT:
                     strcpy(point_name[n_points],param[j][0]);

                     if (!samples)
                         {
                         i=get_point(&ex,&x1,&y1,param[j][1]);
                         if (i<0) return;
                         if (star_point)
                             { gs1+=2; gs2+=1; gc1+=2; gc3+=1; }
                         else
                             { gs1+=2; gs2+=1; gc1+=2; gc3+=2; }

                         i=get_point(&ex,&x2,&y2,param[j][2]);
                         if (i<0) return;
                         x=(x1+x2)/2.0; y=(y1+y2)/2.0;
                         }
                     else
                         {
                         i=get_point(&ex,&x1,&y1,param[j][1]);
                         if (i<0) return;
                         if (!ex)
                             {
                             randomize(&x0,&y0,x1,y1,point_k1[i],point_k2[i],stddev);
                             x1=x0; y1=y0;
                             }
                         i=get_point(&ex,&x2,&y2,param[j][2]);
                         if (i<0) return;
                         if (!ex)
                             {
                             randomize(&x0,&y0,x2,y2,point_k1[i],point_k2[i],stddev);
                             x2=x0; y2=y0;
                             }
// printf("\n%g %g %g %g",x1,y1,x2,y2); getch();

                         if (*param[j][3]==EOS)
                             {
                             sur_print("\nError in midpoint: Third parameter, name of line, missing!");
                             WAIT; return;
                             }
                         i=get_midpoint(x1,y1,x2,y2,param[j][3],&x,&y); // 8.7.2008
                         if (i<0) return;

                         }
                     point_x[n_points]=x; point_y[n_points]=y;
                     point_k1[n_points]=pi4;
                     point_k2[n_points]=-pi4;
                     ++n_points;
                     break;

// L2=parallel(L,P)
          case PARALLEL:
                     strcpy(line_name[n_lines],param[j][0]);

                     if (!samples)
                         {
                         gs1+=2; gs2+=1; gc1+=3; gc3+=3;
                         }

                     if (!samples)
                         {
                         i=get_point(&ex,&x,&y,param[j][2]);
                         if (i<0) return;
                         }
                     else
                         {
                         i=get_point(&ex,&x2,&y2,param[j][2]);
                         if (i<0) return;
                         if (!ex)
                         randomize(&x,&y,x2,y2,point_k1[i],point_k2[i],stddev);
                         }
                     il=get_line(&x1,&y1,&x2,&y2,param[j][1]);
                     if (il<0) return;
                     line_x1[n_lines]=x; line_y1[n_lines]=y;
                     line_x2[n_lines]=x2+x-x1;  line_y2[n_lines]=y2+y-y1;

                     ++n_lines;

                     break;

          case STDDEV:
                     stddev=muste_arit_atof(param[j][1]);
                     break;


          case ORTHO:  strcpy(point_name[n_points],param[j][0]);
                     i=get_point(&ex,&x,&y,param[j][1]);
                     if (i<0) return;
                     point_x[n_points]=x; point_y[n_points]=y;
                     point_k1[n_points]=pi4;
                     point_k2[n_points]=-pi4;
                     ++n_points;
                     break;

            }
        }
  if (samples) fprintf(result,"%s\n",data_line);
  } // sample_i
    if (samples) muste_fclose(result);
    if (create_files) { /* max_distance(); */ save_files(); }
                        // 28.5.2008
    if (res_line && !samples)
        {
        j=res_line;
        sur_print("\n");
        sprintf(sbuf,"GEOM construction: %s",name);
        write_line(j++);

        if (results==100) // 25.8.2008
            {
            sprintf(sbuf,"%3d objects (total)",n_objects-n_object[SAVE]-n_object[SYMBOL]-n_object[SAMPLE]);
            write_line(j++);

            sprintf(sbuf,"%3d points (%d+%d+%d+%d+%d+%d)",n_object[POINT]+n_object[POINT_L]+n_object[POINT_C]+
                    +n_object[CROSS]+
                    n_object[CROSS_CL]+n_object[CROSS_CC],
                    n_object[POINT],n_object[POINT_L],n_object[POINT_C],n_object[CROSS],
                    n_object[CROSS_CL],n_object[CROSS_CC]);
            write_line(j++);
            i=n_object[LINE];
            if (i)
                {
                ch='s'; if (i==1) ch=' ';
                sprintf(sbuf,"%3d line%c",n_object[LINE],ch);
                write_line(j++);
                }
            i=n_object[CIRCLE]+n_object[CIRCLE_P];
            if (i)
                {
                ch='s'; if (i==1) ch=' ';
            sprintf(sbuf,"%3d circle%c (%d+%d)",n_object[CIRCLE]+n_object[CIRCLE_P],ch,
                                               n_object[CIRCLE],n_object[CIRCLE_P]);
                write_line(j++);
                }
            i=n_object[EDGE];
            if (i)
                {
                ch='s'; if (i==1) ch=' ';
                sprintf(sbuf,"%3d edge%c",n_object[EDGE],ch);
                write_line(j++);
                }
            }
        sprintf(sbuf,"Geometrographic symbol = %dS1+%dS2+%dC1+%dC2+%dC3=%d",
                            gs1,gs2,gc1,gc2,gc3,  gs1+gs2+gc1+gc2+gc3);
        write_line(j++);
        sprintf(sbuf,"Simplicity = %d  Exactitude = %d",
                      gs1+gs2+gc1+gc2+gc3,gs1+gc1+gc2);
        write_line(j++);

        s_end(argv);
        }
    }


static int get_midpoint(double x1,double y1,double x2,double y2,char *name,double *px,double *py)
    {
    int i;
    double xl1,yl1,xl2,yl2;
    double radius;
    double xx1,yy1,xx2,yy2,a,b,c,d,f,aa,bb,cc,dd;

    i=get_line(&xl1,&yl1,&xl2,&yl2,name);
    if (i<0) return(-1);
// printf("\nP: %g %g Q: %g %g line: (%g,%g) - (%g,%g)",x1,y1,x2,y2,xl1,yl1,xl2,yl2); getch();

    radius=sqrt(2.0)*sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2));
// printf("\nradius=%g",radius); getch();

    if (fabs(x1-x2)>fabs(y1-y2))
        {
        a=2*(x2-x1); b=2*(y2-y1);
        f=(x1-x2)*(x1+x2)+(y1-y2)*(y1+y2);
        c=-b/a; d=-f/a;
        aa=1+c*c; bb=2*c*(d-x1)-2*y1; cc=(d-x1)*(d-x1)+y1*y1-radius*radius;

        dd=bb*bb-4*aa*cc;
        yy1=(-bb+sqrt(dd))/(2*aa); xx1=c*yy1+d;
        yy2=(-bb-sqrt(dd))/(2*aa); xx2=c*yy2+d;
        }
    else
        {
        a=2*(y2-y1); b=2*(x2-x1);
        f=(y1-y2)*(y1+y2)+(x1-x2)*(x1+x2);
        c=-b/a; d=-f/a;
        aa=1+c*c; bb=2*c*(d-y1)-2*x1; cc=(d-y1)*(d-y1)+x1*x1-radius*radius;

        dd=bb*bb-4*aa*cc;
        xx1=(-bb+sqrt(dd))/(2*aa); yy1=c*xx1+d;
        xx2=(-bb-sqrt(dd))/(2*aa); yy2=c*xx2+d;
        }

// printf("\nP': %g %g Q': %g %g",xx1,yy1,xx2,yy2); getch();


        randomize(&x1,&y1,xx1,yy1,pi/4,-pi4,stddev);
        randomize(&x2,&y2,xx2,yy2,pi/4,-pi4,stddev);

        b=(xl1-xl2)*(y2-y1)+(x2-x1)*(yl2-yl1);

        *px=(xl2*x2*(y1-yl1)+xl2*x1*(yl1-y2)+xl1*x2*(yl2-y1)+xl1*x1*(y2-yl2))/b;
        *py=(yl1*y2*(x1-xl2)+yl1*y1*(xl2-x2)+yl2*y2*(xl1-x1)+yl2*y1*(x2-xl1))/b;

// printf("\nR: %g %g",*px,*py); getch();

    return(1);
    }


static int minmax_x(double x)
    {
    if (x<rx_min) rx_min=x;
    if (x>rx_max) rx_max=x;
    return(1);
    }

static int minmax_y(double y)
    {
    if (y<ry_min) ry_min=y;
    if (y>ry_max) ry_max=y;
    return(1);
    }

static int write_line(int j)
    {
    edwrite(space,j,1);
    edwrite(sbuf,j,1);
    sur_print("\n"); sur_print(sbuf);
    return(1);
    }

static int get_slope_tangent(double *p_slope,double x,double y,double x0,double y0)
    {
    double a;

    a=y-y0; if (fabs(a)<1e-4) { *p_slope=pih; return(1); }
    *p_slope=atan(-(x-x0)/a);
    return(1);
    }

static int get_slope(double *p_slope,double x1,double y1,double x2,double y2)
    {
    double a;

    a=x2-x1; if (fabs(a)<1e-4) { *p_slope=pih; return(1); }
    *p_slope=atan((y2-y1)/a);
    return(1);
    }


static int randomize(double *px,double *py,double x,double y,double a1,double a2,double h)
    {
    double p,alpha;
    double d1,d2,s,c;
    extern double normal_dev();

    if (distr==1)           // DISTR=1  or MODEL=0
        {
        *px=x+h*normal_dev();
        *py=y+h*normal_dev();
        return(1);
        }

    else if (distr==3)     // DISTR=3 or MODEL=2
        {
 //     a1=atan(k1); a2=atan(k2);
        s=1.0/sin(a1-a2);
        d1=h*normal_dev();
        d2=h*normal_dev();
        *px=x+s*(cos(a1)*d1-cos(a2)*d2);
        *py=y+s*(sin(a1)*d1-sin(a2)*d2);
        return(1);
        }

    else if (distr==4)     // DISTR=4 or MODEL=3   29.6.2008
        {
        if (a1<a2) { s=a1; a1=a2; a2=s; }
        if (fabs(a1-a2)<=pih) p=(a1+a2)/2.0;
        else p=(a1+a2-pi)/2.0;

        alpha=fabs(p-a1); d2=fabs(p-a2);
        if (d2<alpha) alpha=d2;

        d2=h*normal_dev();
        d1=h/sin(2*alpha)*normal_dev();

        s=sin(p);
        c=cos(p);
        *px=x+c*d1-s*d2;
        *py=y+s*d1+c*d2;
        return(1);
        }

// DISTR=2 or MODEL=1
    if (a1<a2) { s=a1; a1=a2; a2=s; }
    if (fabs(a1-a2)<=pih) p=(a1+a2)/2.0;
    else p=(a1+a2-pi)/2.0;

    alpha=fabs(p-a1); d2=fabs(p-a2);
    if (d2<alpha) alpha=d2;

    d2=h*normal_dev();
    d1=h/tan(alpha)*normal_dev();

    s=sin(p);
    c=cos(p);
    *px=x+c*d1-s*d2;
    *py=y+s*d1+c*d2;


    return(1);
    }

static int save_files()
    {
    int j;
    int n;
    char *p;
    double rr[4];

    fi_open("_Points",&fi);
    n=0L;
    for (j=0; j<n_points; ++j)
        {
        p=point_name[j];
        if (islower((int)*p)) continue;
        ++n;
        fi_alpha_save(&fi,n,0,p);
        fi_save(&fi,n,1,&point_x[j]);
        fi_save(&fi,n,2,&point_y[j]);
        fi_save(&fi,n,3,&point_k1[j]);
        fi_save(&fi,n,4,&point_k2[j]);
        }

    fi_rewind(&fi);
    fi_puts(&fi,&n,sizeof(int),22L);
    fi_close(&fi);


    fi_open("_Lines",&fi);
    n=0L;
    for (j=0; j<n_lines; ++j)
        {
        p=line_name[j];
        if (islower((int)*p)) continue;
        ++n;
        fi_alpha_save(&fi,n,0,p);
        fi_save(&fi,n,1,&line_x1[j]);
        fi_save(&fi,n,2,&line_y1[j]);
        fi_save(&fi,n,3,&line_x2[j]);
        fi_save(&fi,n,4,&line_y2[j]);
        }

    fi_rewind(&fi);
    fi_puts(&fi,&n,sizeof(int),22L);
    fi_close(&fi);

    fi_open("_Circles",&fi);
    n=0L;
    for (j=0; j<n_circles; ++j)
        {
        p=circle_name[j];
        if (islower((int)*p)) continue;
        ++n;
        fi_alpha_save(&fi,n,0,p);
        fi_save(&fi,n,1,&circle_x[j]);
        fi_save(&fi,n,2,&circle_y[j]);
        fi_save(&fi,n,3,&circle_r[j]);
        fi_save(&fi,n,4,&circle_a1[j]);
        fi_save(&fi,n,5,&circle_a2[j]);
        }
    fi_rewind(&fi);
    fi_puts(&fi,&n,sizeof(int),22L);
    fi_close(&fi);

    fi_open("_Edges",&fi);
    n=0L;
    for (j=0; j<n_edges; ++j)
        {
        p=edge_name[j];
        if (islower((int)*p)) continue;
        ++n;
        fi_alpha_save(&fi,n,0,p);
        fi_save(&fi,n,1,&edge_x1[j]);
        fi_save(&fi,n,2,&edge_y1[j]);
        fi_save(&fi,n,3,&edge_x2[j]);
        fi_save(&fi,n,4,&edge_y2[j]);
        fi_save(&fi,n,5,&edge_len[j]);
        }

    fi_rewind(&fi);
    fi_puts(&fi,&n,sizeof(int),22L);
    fi_close(&fi);


    fi_open("_Range",&fi);    // 6.5.2008
//        X1            Y1            X2            Y2
    n=1L; rr[0]=rx_min; rr[1]=ry_min; rr[2]=rx_max; rr[3]=ry_min;

    range_save(n,rr);

    n=2L; rr[0]=rx_min; rr[1]=ry_min; rr[2]=rx_min; rr[3]=ry_max;
    range_save(n,rr);

    n=3L; rr[0]=rx_max; rr[1]=ry_max; rr[2]=rx_max; rr[3]=ry_min;
    range_save(n,rr);
    n=4L; rr[0]=rx_max; rr[1]=ry_max; rr[2]=rx_min; rr[3]=ry_max;
    range_save(n,rr);

    n=5L; rr[0]=dx1; rr[1]=dy1; rr[2]=dx2; rr[3]=dy2;
    range_save(n,rr);

    fi_rewind(&fi); n=5L;
    fi_puts(&fi,&n,sizeof(int),22L);
    fi_close(&fi);

    return(1);
    }

static int range_save(int n,double *rr)
    {
    double a;

    a=rr[0]; fi_save(&fi,n,0,&a);
    a=rr[1]; fi_save(&fi,n,1,&a);
    a=rr[2]; fi_save(&fi,n,2,&a);
    a=rr[3]; fi_save(&fi,n,3,&a);

    return(1);
    }

static int get_point(int *exact,double *px,double *py,char *pname)
    {
    int i;

    *exact=0;
    if (*pname=='_') { ++pname; *exact=1; }
    star_point=0; if (*pname=='*') { ++pname; star_point=1; } // 3.5.2008
    for (i=0; i<n_points; ++i)
        if (muste_strcmpi(pname,point_name[i])==0) break;
    if (i==n_points) { sprintf(sbuf,"\nPoint %s not found!",pname); sur_print(sbuf); WAIT; return(-1); }
// printf("\nget_point: %s %g %g",pname,point_x[i],point_y[i]); getch();
    *px=point_x[i];
    *py=point_y[i];
    return(i);
    }

static int get_line(double *px1,double *py1,double *px2,double *py2,char *pname)
    {
    int i;

    for (i=0; i<n_lines; ++i)
        if (muste_strcmpi(pname,line_name[i])==0) break;
    if (i==n_lines) { sprintf(sbuf,"\nLine %s not found!",pname); sur_print(sbuf); WAIT; return(-1); }
    *px1=line_x1[i];
    *py1=line_y1[i];
    *px2=line_x2[i];
    *py2=line_y2[i];
    return(i);
    }

static int get_edge(double *px,char *pname,int error)
    {
    int i;

    for (i=0; i<n_edges; ++i)
        if (muste_strcmpi(pname,edge_name[i])==0) break;
    if (i==n_edges)
        {
        if (error)
          { sprintf(sbuf,"\nEdge %s not found!",pname); sur_print(sbuf); WAIT; return(-1); }
        else return(-1);
        }
    *px=edge_len[i];
    return(i);
    }

static int get_circle(double *px,double *py,double *pr,char *pname)
    {
    int i;

    for (i=0; i<n_circles; ++i)
        if (muste_strcmpi(pname,circle_name[i])==0) break;
    if (i==n_circles) { sprintf(sbuf,"\nCircle %s not found!",pname); sur_print(sbuf); WAIT; return(-1); }
// printf("\nCircle: %s x=%g y=%g r=%g",pname,circle_x[i],circle_x[i],circle_r[i]); getch();

    *px=circle_x[i];
    *py=circle_y[i];
    *pr=circle_r[i];
    return(i);
    }

static int parsi(int j)
    {
    int k;
    char *p;

    edread(rivi,j);

    p=strstr(rivi+1," / ");  // 28.3.2008
    if (p!=NULL) *p=EOS;
    p=strstr(rivi+1," // ");
    if (p!=NULL) *p=EOS;

    p=strchr(rivi+1,'=');
    if (p!=NULL) *p=' ';
    p=strchr(rivi+1,'(');
    if (p!=NULL) *p=' ';
    p=strchr(rivi+1,')');
    if (p!=NULL) *p=' ';

    k=split(rivi+1,s,MAXPAR);
    if (k==0) return(0);
    if (*s[0]=='/') return(0);

    return(k);
    }

static int islower(int ch)
    {
    if (ch>=(int)'a' && ch<=(int)'z') return(1);
    return(0);
    }






