/* _PISO.C 23.9.2004/RS
Picture sort
*/

#include "muste.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define YMAX 100

#define XKOKO 365
#define MAXN 30000

static SURVO_DATA d;
static FILE *fp;

static int tulosrivi;
static int m, maara;
static char aineisto[LNAME];

static int dama[MAXN*XKOKO];

struct solmu {
  int index;
  int prev;
  int next;
};

static struct solmu order[MAXN];
static int orderalku;

static struct solmu sorted[MAXN];
static int sortedalku;

static int readfile() {

  int i,j,l,apu;
  char c;

  sprintf(sbuf,"\nReading data %s...",aineisto); sur_print(sbuf);

  fp=muste_fopen(aineisto, "r");
  if (fp == NULL) { sur_print("\nFile error!");  WAIT; return(-1); }

    apu=1; i=XKOKO; j=1;
    while ((apu=fgetc(fp)) != EOF) {
      if ( apu >= '0') { dama[i]=apu-'0'; i++; }
      if ( apu == '\n') j++;
    }
  muste_fclose(fp);
  if (ferror(fp)) { sur_print("\nFile error!");  WAIT; return(-1); }
  sur_print("DONE!\n");
  return j;
}


static void writefile() {

  int i,j,l,solmu,apu;

  i=spfind("OUTFILE");
  if (i >= 0) {
    sprintf(sbuf,"\nWriting sort-order to the file %s...",spb[i]); sur_print(sbuf);
    fp=muste_fopen(spb[i], "w");
    if (fp == NULL) { sur_print("\nFile error!");  WAIT; return; }
    fprintf(fp,"SORD\n");

    solmu=sortedalku;
    for (j=1; j < maara; j++) {
      apu=sorted[solmu].index;
      solmu=sorted[solmu].next;
      fprintf(fp,"%d\n",apu);
      if (ferror(fp)) { sur_print("\nFile error!");  WAIT; return; }
    }
    fclose(fp);
    if (ferror(fp)) { sur_print("\nFile error!");  WAIT; return; }
    sur_print("DONE!\n");
  }
}

static int verttrans () {
  int i,j,kust,solmu,prev,cur;
  kust=0;
  for (j=0; j<XKOKO; j++) {
    solmu=sortedalku;
    prev=dama[sorted[solmu].index*XKOKO+j];
    solmu=sorted[solmu].next;
    for (i=2; i<maara; i++) {
      cur=dama[sorted[solmu].index*XKOKO+j];
      if(cur!=prev) kust++;
      prev=cur;
      solmu=sorted[solmu].next;
    }
  }
 return kust;
}

static int comparerivi (int rivi1, int rivi2) {
  int i,kust;
  kust=0;
  for (i=0; i<XKOKO; i++) {
    if (dama[rivi1*XKOKO+i]!=dama[rivi2*XKOKO+i]) kust++;
//    if (dama[rivi1*XKOKO+i]!=dama[rivi2*XKOKO+i]) kust+=abs((dama[rivi1*XKOKO+i]-dama[rivi2*XKOKO+i])^2);
  }
 return kust;
}

/*
static void sortdataminadd() {
   int i,j,solmu,sortsolmu,sortmaara,kust,minkust,minsolmu,apusolmuprev,apusolmunext;

   solmu=orderalku;
   sortsolmu=1;
   sorted[sortsolmu].index=order[solmu].index;
   sorted[sortsolmu].prev=sortsolmu;
   sorted[sortsolmu].next=sortsolmu;
   sortedalku=1;
   sortmaara=1;
   minsolmu=1;

   for (j=2; j<maara; j++) {
     solmu=order[solmu].next;
     minkust=2*XKOKO+1;
     sortedalku=1;

     for (i=1; i<sortmaara; i++) {
       sorted[sortmaara+1].index=order[solmu].index;
       kust=comparerivi(sorted[sortmaara+1].index,sorted[sortsolmu].index);
       kust+=comparerivi(sorted[sortmaara+1].index,sorted[sorted[sortsolmu].next].index);

       if (kust<minkust) {
         minkust=kust;
         minsolmu=sortsolmu;
       }

     sortsolmu=sorted[sortsolmu].next;
     }

     apusolmunext=sorted[minsolmu].next;

     sorted[minsolmu].next=sortmaara+1;
     sortsolmu=sortmaara+1;
     sorted[sortsolmu].prev=minsolmu;
     sorted[sortsolmu].next=apusolmunext;

     sorted[apusolmunext].prev=sortsolmu;

     sortmaara++;

     sprintf(sbuf,"%d ",j); sur_print(sbuf);
   }
}
*/
/*
static void sortdataminremove() {
   int i,j,solmu,sortsolmu,sortmaara,kust,minkust,minsolmu,apusolmuprev,apusolmunext;

   sortsolmu=0;
   sorted[sortsolmu].index=maara+1;
   sorted[sortsolmu].prev=-1;
   sorted[sortsolmu].next=sortsolmu+1;
   sortedalku=1;
   for (i=0; i<XKOKO; i++) dama[(maara+1)*XKOKO+i]=4;

   sortmaara=maara;

   for (j=1; j<maara; j++) {
     minkust=XKOKO+1;
     solmu=orderalku;

     for (i=1; i<sortmaara; i++) {
       kust=comparerivi(sorted[sortsolmu].index,order[solmu].index);

       if (kust<minkust) {
         minkust=kust;
         minsolmu=solmu;
       }
     solmu=order[solmu].next;
     }
     apusolmuprev=order[minsolmu].prev;
     apusolmunext=order[minsolmu].next;
     order[apusolmuprev].next=apusolmunext;
     order[apusolmunext].prev=apusolmuprev;
     if (minsolmu==orderalku) orderalku=apusolmunext;

     sortsolmu++;
     sorted[sortsolmu].index=order[minsolmu].index;
     sorted[sortsolmu].prev=sortsolmu-1;
     sorted[sortsolmu].next=sortsolmu+1;

     sortmaara--;

     sprintf(sbuf,"%d ",j); sur_print(sbuf);
   }
}
*/

static void sortdatamin() {

   int i,j,k,solmu,sortsolmu,sortmaara,kust,minkust,minsolmu1,minsolmu2,apusolmuprev,apusolmunext;

   sortsolmu=0;
   sorted[sortsolmu].index=maara+1;
   sorted[sortsolmu].prev=-1;
   sorted[sortsolmu].next=0;
   sortedalku=0;
   for (i=0; i<XKOKO; i++) dama[(maara+1)*XKOKO+i]=4;

   sortmaara=1;
   kust=0; minsolmu1=0; minsolmu2=0;

//   sprintf(sbuf,"\nenter loop\n"); sur_print(sbuf);

   for (k=1; k<maara; k++) {
     sortsolmu=sortedalku;
     minkust=2*XKOKO+1;

     for (j=0; j<sortmaara; j++) {

       solmu=orderalku;

       for (i=1; i<maara-sortmaara; i++) {

       kust=comparerivi(sorted[sortsolmu].index,order[solmu].index);
       if (sorted[sortsolmu].next!=0) {
       kust+=comparerivi(sorted[sorted[sortsolmu].next].index,order[solmu].index);
       }
         if (kust<minkust) {
           minkust=kust;
           minsolmu1=solmu;
           minsolmu2=sortsolmu;
         }
       solmu=order[solmu].next;
       }

       sortsolmu=sorted[sortsolmu].next;
//       sprintf(sbuf,"%d:%d ",k,j); sur_print(sbuf);
     }



     apusolmuprev=order[minsolmu1].prev;
     apusolmunext=order[minsolmu1].next;
     order[apusolmuprev].next=apusolmunext;
     order[apusolmunext].prev=apusolmuprev;
     if (minsolmu1==orderalku) orderalku=apusolmunext;

     sorted[sortmaara+1].index=order[minsolmu1].index;

     apusolmunext=sorted[minsolmu2].next;

     sorted[minsolmu2].next=sortmaara+1;
     sorted[sortmaara+1].prev=minsolmu2;
     sorted[sortmaara+1].next=apusolmunext;

     sorted[apusolmunext].prev=sortmaara+1;

     sortmaara++;


     sprintf(sbuf,"%d:%d ",k,minkust); sur_print(sbuf);
   }


}





/*
static void printrivi(int rivi) {
  int i,solmu;
  sprintf(sbuf,"\nRivi %5d: ",rivi);
  sur_print(sbuf);
  solmu=orderalku;
  i=1;
  while (i<rivi) {
    i++;
    solmu=order[solmu].next;
  }
  for (i=0; i<XKOKO; i++) {
    sprintf(sbuf,"%i",dama[(order[solmu].index)*XKOKO+i]);
    sur_print(sbuf);
  }
}
*/
int muste_piso(int argc, char *argv[]) {
  int i,j,k,l;
  char *p, *apuc;
  char ch;
  int apu, apu2, ero;
  int max_dim;
  double mf;

/*
  if (argc==1) {
    printf("This program can be used as a SURVO module only.");
    return -1;
  }
*/
  s_init(argv[1]);
  if (g<2) {
    init_remarks();
    rem_pr("PISO");
    rem_pr("Picture sort");
    rem_pr("Heuristic sort algorithm");
    wait_remarks(2);
    return 0;
  }

  tulosrivi=0;
  if (g>2) tulosrivi=edline2(word[2],1,1);

  i=spec_init(r1+r-1); if (i<0) return -1;

  strcpy(aineisto,word[1]);

  maara=readfile();
  if (maara<=0) return(-1);

  for (j=1; j<=maara; j++) {
   order[j].index=j;
   order[j].next=j+1;
   order[j].prev=j-1;
  }
  order[1].prev=maara;
  order[maara].next=1;
  orderalku=1;


  for (j=1; j<=maara; j++) {
   sorted[j].index=j;
   sorted[j].next=j+1;
   sorted[j].prev=j-1;
  }
  sorted[1].prev=maara;
  sorted[maara].next=1;
  sortedalku=1;
/*
  i=verttrans();
  mf=(double)100*i/(XKOKO*maara);
  sprintf(sbuf,"\nKust: %d, %3.1f",i,mf);
  sur_print(sbuf);
  WAIT;
*/

  sortdatamin();
/*

  for (j=1; j<=maara; j++) {
   order[j].index=sorted[j].index;
   order[j].next=sorted[j].next;
   order[j].prev=sorted[j].prev;
  }
  order[1].prev=maara;
  order[maara].next=1;
  orderalku=sortedalku;

  sortdataminadd();
*/

/*
  i=verttrans();
  mf=(double)100*i/(XKOKO*maara);
  sprintf(sbuf,"\nKust: %d, %3.1f",i,mf);
  sur_print(sbuf);
*/
// WAIT;

  writefile();

  s_end(argv[1]);

// WAIT;
return 0;
}


