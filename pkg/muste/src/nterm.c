
/* !nterm.c 18.6.1997/RS (7.1.1997)*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MIN 5
#define MAX 30
#define TOLERANCE 1e-15
#define SOFTOLERANCE 1e-10
#define MINTOLERANCE 1e-5
#define FIXTOLERANCE 1e-3


struct murtoluku {
  int osoittaja;
  int nimittaja;
};

//enum boolean {FALSE,TRUE};
enum seqtype {UNIDENTIFIED,POLYNOMIAL,FIBONACCI};



static char comment_1[] = " / UNIDENTIFIED SEQUENCE";
static char comment_2[] = " / POLYNOMIAL";


static char nimi[LLENGTH];
static char *nterm_comment;
static char nterm_output_buffer[100];
// RS REM static char nterm_number[33];
static int nterm_output_line;
static int nterm_type;

static int syvyys;
static double *regressor[MAX];
static int sequence[MAX];
static int seq_n; /* Terms in sequence */
static int muutnim;


static SURVO_DATA data;




static int init_sequence();
static void construct_formula(double *B, int k, int n);
static double* matrixmalloc(int m, int n);
static int linear_regression();
// static void showmatrix(double* X,int m,int n);
static void mat_store(double *X, double *Y,int m, int n);
static int init_regressors();
static double solve_term(double *X,double *B,int m,int n,int i);
static struct murtoluku ketju(double x);
static int linreg(double *X,double *Y,double *S,double *U,double *T,double *B,double *R,int m,int n);
static int residual(double *R, double *B, double *Y, double *X,int m, int n);

void muste_nterm(int argc, char *argv[])
  {
  int i;
/*
  if (argc==1)
      {
      Rprintf("This program can be used as a SURVO 84C module only.");
      return;
      }
*/      
  s_init(argv[1]);

  i=init_sequence();
  if (i<0) { s_end(argv[1]); return; }
  i=init_regressors();
  if (i<0) return;

  i=linear_regression(seq_n,10);
  if (i<0) return;

  data_close(&data);

  edwrite(nterm_output_buffer,nterm_output_line,1);

  s_end(argv[1]);
}


static int init_regressors() {
  int i,j;
  double dlapu;
  double *dlapu2;

  strcpy(nimi,survo_path);
  strcat(nimi,"SYS/NTERMLIB");

  if (data_open(nimi,&data)<0) {
    sur_print("\nError in opening file NTERMLIB!");
    WAIT; return(-1);
  }
  if (data.m_act>=MAX) {
    sur_print("\nToo many active regressors!");
    WAIT; return(-1);
  }
  for (i=0 ;i<data.m_act; ++i) {
    regressor[i]=(double *)muste_malloc((seq_n+1)*sizeof(double));
    if (regressor[i] == NULL) {
      sur_print("\nOut of memory!");
      WAIT; return(-1);
    }
    for (j=0;j<=seq_n;++j) {
      if (data_load(&data,data.l1+j,data.v[i],&dlapu)!=1) {
        sur_print("\nInvalid value in regressors!");
        WAIT; return(-1);
      }
      dlapu2=regressor[i];
      *(dlapu2+j)=dlapu;
    }
  }
  return(1);
}


static int init_sequence() {
  int i,j,h;
  int rivi,rivi1;
  char x[LLENGTH],*sana[MAX];
seq_n=0; rivi=rivi1=r1+r-1;
  while (1) {
    edread(x,rivi);
    i=split(x+1,sana,MAX);
    if (i==0) {
      nterm_output_line=rivi;
      break;
    }
    
    j=0; if (rivi==rivi1) j=1;
    for (h=j; h<i; ++h) {    
      if (strcmp(sana[h],"/")==0) break;     
      if ((*sana[h] < 48) || (*sana[h] > 57)) {
        sprintf(sbuf,"\n\nSyntax error in line %d!",rivi);
        sur_print(sbuf);
        WAIT; return(-1);
      }
      if (seq_n>=MAX) {
        sur_print("\nToo many terms already!");
        WAIT; return(-1);
      }
      sequence[seq_n++]=atoi(sana[h]); // RS CHA atol
    }
    ++rivi;
  } 
  if (seq_n<MIN) {
    if (seq_n==0) {
      init_remarks();
      rem_pr("USAGE: NTERM <at least five integers> / <comment>");
      rem_pr("");
      rem_pr("NTERM aims to infer the next as well as general term in a given");
      rem_pr("integer sequence.");
      rem_pr("");
      rem_pr("The given integers should be on the command line and/or on the");
      rem_pr("lines immediately below that line separated by spaces or commas.");
      rem_pr("Anything after the slash (/) in a line will be considered as");
      rem_pr("comments. Output is directed to the first free line.");
      rem_pr("Output follows the style <integer> / <general term>, where n is");
      rem_pr("the order of the term and a(n) is the current term (and a(n-x) is");
      rem_pr("the term preceding x terms of the current term.");
      rem_pr("");
      rem_pr("NTERM bases its inferences on linear regression analysis where");
      rem_pr("polynomials earlier terms and active variables in the file");
      rem_pr("<Survo>/SYS/NTERMLIB.SVO are used as regressors.");
      rem_pr("");
      wait_remarks(1);
      rem_pr("Examples:");
      rem_pr("");
      rem_pr("NTERM 1 2 3 4 5 6 7 8              / Basic");
      rem_pr("");
      rem_pr("NTERM 1 2 4 7 11 16                / Intelligence test");
      rem_pr("");
      rem_pr("NTERM 1 2 3 5 8 13 21              / Fibonacci");
      rem_pr("");
      rem_pr("NTERM 1 2 3 10 22 51 125 293 696   / Title");
      rem_pr("");
      rem_pr("NTERM 1 3 9 27 81 243              / Own regressor");
      rem_pr("");
      rem_pr("NTERM 2 6 15 40 145 756 5089 40384 / Superseeker test");
      rem_pr("");
      rem_pr("NTERM 3 4 6 7 9 10 12 13 15        / Twin");
      rem_pr("");
      rem_pr("NTERM 1 1 2 2 4 4 8 8 16           / Double");
      rem_pr("");
      wait_remarks(1);
      rem_pr("It is possible to include own regressors to the NTERMLIB. For example");
      rem_pr("the command");
      rem_pr("VAR 2^n:8=2^ORDER TO <Survo>/SYS/NTERMLIB");
      rem_pr("adds the regressor corresponding to the powers of two.");
      rem_pr("");
      rem_pr("In some cases a few first terms of the sequence are neglected in the");
      rem_pr("inference, so they may not follow the derived general rule.");
      rem_pr("");
      wait_remarks(1);
      rem_pr("NTERM by Reijo Sund 1997  -  Email: reijo.sund@helsinki.fi");
      rem_pr("");
      rem_pr("More about integer sequences:");
      rem_pr("N.J.A. Sloane (http://www.research.att.com/~njas/sequences)");
      rem_pr("");
      wait_remarks(2);
      return(-1);
    }
    else {
      sur_print("\nNot enough initial terms!");
      WAIT; return(-1);
    }
  }  
  return(1);
}


static void construct_spec(double *B,int eka) {
  struct murtoluku ml;
  int length;
  char os[25];
  char nim[25];
  char tyhja[100]="        ";

  ml=ketju(fabs(*B));
  if (ml.osoittaja>0) {
    muste_ltoa(ml.osoittaja,os,10);
    muste_ltoa(ml.nimittaja,nim,10);
    if (*B<0) {
      strcat(nterm_output_buffer,"-");
    }
    else if (eka==TRUE) {
      strcat(nterm_output_buffer,"+");
    }
    eka=TRUE;

    if (ml.nimittaja>1) {
      strcat(nterm_output_buffer,"(");
      strcat(nterm_output_buffer,os);
      strcat(nterm_output_buffer,"/");
      strcat(nterm_output_buffer,nim);
      strcat(nterm_output_buffer,")");
    }
    else {
      if (ml.osoittaja!=1)  {
        strcat(nterm_output_buffer,os);
      }
    }
     strcat(nterm_output_buffer,"(");
/*
     length=strcspn(data.varname[muutnim],tyhja);
     strncpy(tyhja,data.varname[muutnim],length);
*/
     strcpy(tyhja,data.varname[muutnim]);
     length=1;
     do {
       length++;
     } while ((tyhja[length]!=' ') && (length<=8));
     tyhja[length]='\0';
     strcat(nterm_output_buffer,tyhja);
     strcat(nterm_output_buffer,")");
  }
}

static void construct_poly(double *B,int k,int n) {
  int i,j;
  struct murtoluku ml;
  char os[25];
  char nim[25];
  char pot[10];

  j=FALSE;
  for (i=0;i<(n-k);i++) {
    ml=ketju(fabs(*(B+k+i)));
    if (ml.osoittaja>0) {
      muste_ltoa(ml.osoittaja,os,10);
      muste_ltoa(ml.nimittaja,nim,10);
      muste_itoa(i,pot,10);

      if (*(B+k+i)<0) {
        strcat(nterm_output_buffer,"-");
      }
      else if (j==TRUE) {
        strcat(nterm_output_buffer,"+");
      }
      j=TRUE;

      if (ml.nimittaja>1) {
        strcat(nterm_output_buffer,"(");
        strcat(nterm_output_buffer,os);
        strcat(nterm_output_buffer,"/");
        strcat(nterm_output_buffer,nim);
        strcat(nterm_output_buffer,")");
      }
      else {
        if (ml.osoittaja!=1 || i==0)  {
          strcat(nterm_output_buffer,os);
        }
      }
      if (i>0) {
        strcat(nterm_output_buffer,"n");
        if (i>1) {
          strcat(nterm_output_buffer,"^");
          strcat(nterm_output_buffer,pot);
        }
      }
    }
  }
  if (k>0) construct_spec(B,j);
}


static void construct_fibo(double *B,int k,int n) {
  int i,j;
  struct murtoluku ml;
  char os[25];
  char nim[25];
  char pot[10];

  j=FALSE;
  for (i=0;i<=n;i++) {
    ml=ketju(fabs(*(B+i)));
    if (ml.osoittaja>0) {
      muste_ltoa(ml.osoittaja,os,10);
      muste_ltoa(ml.nimittaja,nim,10);
      muste_itoa(i,pot,10);

      if (*(B+i)<0) {
        strcat(nterm_output_buffer,"-");
      }
      else if (j==TRUE) {
        strcat(nterm_output_buffer,"+");
      }
      j=TRUE;


      if (ml.nimittaja>1) {
        strcat(nterm_output_buffer,"(");
        strcat(nterm_output_buffer,os);
        strcat(nterm_output_buffer,"/");
        strcat(nterm_output_buffer,nim);
        strcat(nterm_output_buffer,")");
      }
      else {
        if (ml.osoittaja!=1 || i==0) {
          strcat(nterm_output_buffer,os);
        }
      }
      if (i>0) {
        strcat(nterm_output_buffer,"a(n-");
        strcat(nterm_output_buffer,pot);
        strcat(nterm_output_buffer,")");
      }
    }
  }
}


static void construct_formula(double *B,int k,int n) {
  strcat(nterm_output_buffer," / ");
  switch(nterm_type) {
    case POLYNOMIAL: {
      construct_poly(B,k,n);
      nterm_comment=comment_2;
      break;
    }
    case FIBONACCI: {
      construct_fibo(B,k,n);
      break;
    }
    default:
      strcpy(nterm_output_buffer,comment_1);
  }
}




static void polyrow(double *X, int m, int n, int j, int k, int p) {
  int i,l;
  l=j+k*m;
  for (i=0;i<(n-k);i++) *(X+l+m*i)=pow((p),i);
}

static void polyfill(double *X, int m, int n, int k) {
  int j;
  for (j=0;j<m;j++) polyrow(X,m,n,j,k,j+1);
}



static void fixmat(double *B, int m) {
  int apuf,apuc;
  int i;
  for (i=0; i<m; i++) {
    apuf=(int)floor(*(B+i));
    apuc=(int)ceil(*(B+i));
    if (pow((*(B+i)-apuf),2) < FIXTOLERANCE ) *(B+i)=apuf;
    else if (pow((*(B+i)-apuc),2) < FIXTOLERANCE ) *(B+i)=apuc;

  }
}


static int linear_regression(int m, int n) {
  double *Y, *X, *S, *U, *T, *B, *R;
  double *oma;
  int real_m,real_n;
  int i,j,k,a;
  int tulos;
  double tulosterm;
  int tulosl;
// RS REM  struct murtoluku tulosml;

  oma=regressor[0];
  if (n>=m) n=m-1;
  nterm_type=UNIDENTIFIED;
  strcpy(nterm_output_buffer,comment_1);


  Y=matrixmalloc(m,1);
  if (Y==NULL) return(-1);
  for (i=0;i<m;i++) {
    *(Y+i)=sequence[i];
  }

  X=matrixmalloc(m,n);
  if (X==NULL) return(-1);
  S=matrixmalloc(m,n);
  if (S==NULL) return(-1);
  U=matrixmalloc(n,n);
  if (U==NULL) return(-1);  
  T=matrixmalloc(m,n);
  if (T==NULL) return(-1);  
  B=matrixmalloc(m,1);
  if (B==NULL) return(-1);
  R=matrixmalloc(m,1);
  if (R==NULL) return(-1);

  m-=1;n-=1;
  i=0;
  tulos=FALSE;




  k=0;
  polyfill(X,m,n,k);
  tulos=linreg(X,Y,S,U,T,B,R,m,n);
  if (tulos<0) return(-1);

  if (!tulos) {

    j=k;
    k++;

    do {
      j++;

      oma=regressor[j-1];
      for (i=0;i<m;i++) {
        *(X+i)=*(oma+i);
      }
      muutnim=data.v[j-1];

      polyfill(X,m,n,k);

/*
    showmatrix(X,m,n);
*/

      tulos=linreg(X,Y,S,U,T,B,R,m,n);
      if (tulos<0) return(-1);

    } while (tulos!=TRUE && j<data.m_act);
  }

/*
 showmatrix(B,m,1);
*/

  if (tulos) {

    if (k>0) *(X+0)=*(oma+m);

    polyrow(X,1,n,0,k,m+1);

/*
 showmatrix(X,1,n);
*/
    if ( pow( *(Y+m)-solve_term(X,B,1,n,0) ,2) < MINTOLERANCE ) {

      if (k>0) *(X+0)=*(oma+m+1);
      polyrow(X,1,n,0,k,m+2);
      tulosterm=solve_term(X,B,1,n,0);

      tulosl=(int)floor(tulosterm);
      if (fabs(tulosterm-tulosl)>.5) tulosl=(int)ceil(tulosterm);

      muste_ltoa(tulosl,nterm_output_buffer,10);
      nterm_type=POLYNOMIAL;



      construct_formula(B,k,n);


    }
  }
  else {

    real_m=m;real_n=n;

  for (n=1;n<(real_m-n-1);n++) {

    a=n+1;
    m=real_m-n;

    for (i=0;i<m+1;i++) {
      *(Y+i)=sequence[i+n];
    }

/*
    showmatrix(Y,m,1);
*/


    for (i=0;i<m;i++) *(X+i)=1;

    for (j=1;j<=n;j++) {
      for (i=0;i<m;i++) {
        *(X+i+m*j)=sequence[n-j+i];
      }
    }

/*
    showmatrix(X,m,a);
*/

    tulos=linreg(X,Y,S,U,T,B,R,m,a);
    if (tulos<0) return(-1);

/*
    showmatrix(B,a,1);
*/

    if (tulos) {
      tulosterm=*(B+0);
      for (k=0;k<n;k++) {
        tulosterm+=(*(Y+m-1-k))*(*(B+1+k));
      }
/*
printf("\ntark:%f\n",pow(tulosterm-*(Y+m+1),2));WAIT;
*/

      if (pow(tulosterm-*(Y+m+1),2) < MINTOLERANCE) {
        tulosterm=*(B+0);
        for (k=0;k<n;k++) {
         tulosterm+=(*(Y+m-k))*(*(B+1+k));
        }
        tulosl=(int)floor(tulosterm);
        if (fabs(tulosterm-tulosl)>.5) tulosl=(int)ceil(tulosterm);

        muste_ltoa(tulosl,nterm_output_buffer,10);

        nterm_type=FIBONACCI;

        construct_formula(B,0,n);
        break;
      }
    }
  }

}
return(1);
}


static int linreg(double *X,double *Y,double *S,double *U,double *T,double *B,double *R,int m,int n) {
// RS REM  int i;
  mat_store(X,T,m,n);
  if (mat_gram_schmidt(S,U,T,m,n,TOLERANCE)<0) {
    sur_print("\nUnable to solve!");
//    showmatrix(X,m,n);    
    WAIT; return(-1);
  }
  mat_transp(T,S,m,n);
  mat_mlt(S,T,Y,n,m,1);
  solve_upper(B,U,S,n,1,TOLERANCE);

  fixmat(B,n);

  return(residual(R,B,Y,X,m,n));
}



static double solve_term(double *X,double *B,int m,int n,int i) {
  int j;
  double y;
  y=0;
  for (j=0;j<n;j++) {
    y+=(*(X+i+m*j))* (*(B+j));
  }
  return(y);
}


static int residual(double *R, double *B, double *Y, double *X,int m, int n) {
  int i,tulos;
  double r,h;

  tulos=FALSE;
  r=0;
  for (i=0;i<m;i++) {
    h=*(R+i)=*(Y+i)-solve_term(X,B,m,n,i);
    r+=h*h;
  }

  if (r<MINTOLERANCE) tulos=TRUE;


/*
 showmatrix(R,m,1);
 Rprintf("r=%f",r);WAIT;
*/

  return(tulos);
}


static double* matrixmalloc(int m, int n) {
  double *t;
  int i;
  t=(double *)muste_malloc(m*n*sizeof(double));
  if (t == NULL) {
    sur_print("\nOut of memory!");
    WAIT; return(NULL);
  }
  for (i=0; i<n*m; i++) *(t+i)=0;
  return t;
}


static void mat_store(double *X, double *Y,int m, int n) {
  int i;
  for (i=0; i<n*m; i++) *(Y+i)=*(X+i);
}


/*
static void showmatrix(double* X,int m,int n) {
  int i,j;
  for (i=0;i<m;i++) {
    Rprintf("\n");
    for (j=0;j<n;j++) {
      Rprintf("%12lf",*(X+i+j*m),4);
    }
  }
  WAIT;
}
*/

static struct murtoluku ketju(double x) {
  struct murtoluku a,b;
  int c;
  double y;

  b.osoittaja=(int)floor(x);
  b.nimittaja=1;
  a.osoittaja=1;
  a.nimittaja=0;
  y=(x-b.osoittaja);
  if ( (fabs(y)>MINTOLERANCE) && (syvyys<10)) {
    syvyys++;
    a=ketju(1/y);
    syvyys=0;
  }
  c=a.osoittaja;
  a.osoittaja=a.nimittaja;
  a.nimittaja=c;

  b.osoittaja*=a.nimittaja;
  a.osoittaja+=b.osoittaja;

  return(a);
}

