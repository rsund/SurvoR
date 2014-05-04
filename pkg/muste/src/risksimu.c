/* _BOOT.C 12.6.2001/RS
Kustannusbootstrapperi
*/

#include "muste.h"


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"



#define YMAX 100
#define MAXKUN 500
#define MAXIN 50

// static SURVO_DATA d;
static FILE *fp;

static int tulosrivi;
// static int m;
// static char aineisto[LNAME];

static int kuno[MAXKUN];
static int popul[MAXKUN];
static int obspopul[MAXKUN];
static double *obsdata[MAXKUN];

static int munc, iter, kuntia, iterations, satu;
static double summa;

static unsigned long nextr=1;
static int randa(int max) {
  nextr=nextr*69069+1;
  return (int)(nextr%((unsigned long)max))+1;
}

static void *safe_malloc(size_t size) {
  void *a;
  a=(void *)muste_malloc(size);
  if (a == NULL) {
    sur_print("\n?Out of memory."); WAIT;
    return(NULL);
  }
  else return a;
}


int muste_risksimu(int argc, char *argv[]) {
  int i,j,k,l;
//  char *p;
//  char ch;
  int apu; //, ero;
//  int max_dim;
//  float mf;
  double raha, apu3;
  char apu2real[MAXIN];
  char *apu2;

  s_init(argv[1]);
  if (g<2) {
    init_remarks();
    rem_pr("RISKSIMU <indata1.txt>,<indata2.txt>             / Reijo Sund 2001");
    wait_remarks(2);
    return 0;
  }

  fp=NULL;

  for (i=0; i<MAXKUN; i++)
    {
    kuno[i]=0;
    popul[i]=0;
    obspopul[i]=0;
    obsdata[i]=NULL;
    }

  munc=iter=kuntia=iterations=satu=0;
  summa=0;
  nextr=1;

  i=j=k=l=apu=0;
  raha=apu3=0;

  apu2=apu2real;  
  tulosrivi=0;
/*  if (g>2) tulosrivi=edline2(word[2],1,1);  */


  i=spec_init(r1+r-1); if (i<0) return -1;

/* Kuno, potilaiden m„„r„, kunnan asukkaiden m„„r„ */

  fp=muste_fopen(word[1], "r");
  if (fp == NULL) { sur_print("\nInfile1 error!");  WAIT; return(-1); }

  sprintf(sbuf,"\nReading %s...",word[1]); sur_print(sbuf);

  apu=1; j=0;
  while (apu != EOF) {
    apu=fscanf(fp,"%i\t%i\t%i\n",&kuno[j],&obspopul[j],&popul[j]);
    j++;
    if (j>MAXKUN) { sur_print("\nCapacity overflow error!");  WAIT; return(-1); }
    if (ferror(fp)) { sur_print("\nInfile1 error!");  WAIT; return(-1); }
  }
  muste_fclose(fp);
  kuntia=j-1;

/* Muistin varaus */

  for (j=0; j<kuntia; j++) {
/*  sprintf(sbuf,"\n%i, %i, %i",kuno[j],obspopul[j],popul[j]); sur_print(sbuf); */

    obsdata[j]=(double *)safe_malloc((size_t)((unsigned int)((1+obspopul[j]))*sizeof(double)));
    if (obsdata[j]==NULL) return(-1); // RS 4.5.2014
  }

/* Havaitut hinnat */

  fp=muste_fopen(word[2], "r");
  if (fp == NULL) { sur_print("\nInfile2 error!");  WAIT; return(-1); }

  sprintf(sbuf,"\nReading %s...",word[2]); sur_print(sbuf);

  raha=0;
  apu=1; j=0; k=0; i=0; l=-1;
  while (apu != EOF) {
    j=k;
    apu=fscanf(fp,"%i\t%s\n",&k,apu2);    
    apu3=atof(apu2);
// Rprintf("\n%s ; %f",apu2,apu3);    
    if (k != j) {
      i=0; l++;
    sprintf(sbuf,"%i ",kuno[l]); sur_print(sbuf);
    }
    *(obsdata[l]+i)=(double) apu3;
// Rprintf("; %f",*(obsdata[l]+i));    



/*  sprintf(sbuf,"%f %f \n",(double)apu3, *(obsdata[l]+i)); sur_print(sbuf); */
    i++;
    if (ferror(fp)) { sur_print("\nInfile2 error!");  WAIT; return(-1); }
  }

/* HUOM! oltava: obspopul <= popul */

  i=spfind("SEED");
  if (i < 0) { nextr=10; } /* (unsigned long) time(NULL); } */
  else { nextr=(unsigned long)atoi(spb[i]); }

  i=spfind("ITERATIONS");
  if (i < 0) { iterations=100; }
  else { iterations=atoi(spb[i]); }


  i=spfind("OUTFILE");
  if (i >= 0) {
    sprintf(sbuf,"\nWriting results to the file %s...",spb[i]); sur_print(sbuf);
    fp=muste_fopen(spb[i], "w");
    if (fp == NULL) { sur_print("\nOutfile error!");  WAIT; return(-1); }


    for (munc=0; munc<kuntia; munc++) {
      sprintf(sbuf,"%i ",kuno[munc]); sur_print(sbuf);
      for (iter=0; iter<iterations; iter++) {
        summa=0;
        for (i=0; i<popul[munc]; i++) {
          satu=randa(popul[munc])-1;
           if (satu<obspopul[munc]) {
            summa+=*(obsdata[munc]+(unsigned int)satu);
          }
//    sprintf(sbuf,"%i %12.0f \n",satu,summa); sur_print(sbuf);
        }


//    sprintf(sbuf,"%3i %12.0f\n",kuno[munc],summa); sur_print(sbuf); 
        fprintf(fp,"%i\t%15.0f\n",kuno[munc],summa);  // (double)(summa/popul[munc]));
        if (ferror(fp)) { sur_print("\nOutfile error!");  WAIT; return(-1); }
      }
    }
    muste_fclose(fp);
    if (ferror(fp)) { sur_print("\nOutfile error!");  WAIT; return(-1); }
    sur_print("DONE!\n");
  }

  s_end(argv[1]);

// WAIT;
return 0;
}


