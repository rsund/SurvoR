/* _DISCO.C 9.1.2001/RS (9.1.2001) (17.10.2002)
Knowledge discovery tools
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define YMAX 100

static SURVO_DATA d;
static FILE *fp;

static int tulosrivi;
static int m;
static char aineisto[LNAME];
static char groupvarname[YMAX];
static char oldgroupvarname[YMAX] = " ";
static char filebuffer[YMAX];

static int blocko, blocklevels, sizeofitem, sizeofcuritem, maxlen, maxobs, maxitems;
static int blockcount, obscount, sizeofkama, items, minfreq, maxvars, looposa, loopcount;
static int looposacount, method, met, groupvar, grov;
static int *kama;
static char *map, *remap, *curitem, *palsti;
static char **kamamap;
static int *mapi, *groupcheck;
static char curblock;
static int cumblockcount[YMAX];
static int *candpointers[YMAX];
static int canditems[YMAX];
static int freqitems[YMAX];


static int check;

static int checkmap(char* item, int block) {
  int i, loyto, apu, pala;

  loyto=0; i=0; pala=-1;
  while (i<items && loyto==0) {
    if (mapi[i*sizeofitem+2] == block) {
      apu=strcmp(&map[(i*sizeofitem+3)*sizeof(int)],item);
      if (apu == 0) {
//        sprintf(sbuf,"* "); sur_print(sbuf);
        if (groupvar > 0) {
          if (groupcheck[i] == 0) {
            mapi[i*sizeofitem+1]++;
            groupcheck[i]=1;
          }
        } else {
          mapi[i*sizeofitem+1]++;
        }
        loyto=1;
        pala=i+1;
      }
    }
  i++;
  }
  if (loyto==0) {
    items++;
    pala=items;
    if (items > maxitems) {
      sur_print("\nMAXITEMS specification too small!");
      WAIT; return(-1);
    }
//    sprintf(sbuf,"%i ",items); sur_print(sbuf);
    memcpy(&map[(i*sizeofitem+3)*sizeof(int)],item,maxlen);
    mapi[i*sizeofitem]=items;
    mapi[i*sizeofitem+1]=1;
    mapi[i*sizeofitem+2]=block;
//    apu=mapi[i*sizeofitem];
//    sprintf(sbuf,"%i ",apu); sur_print(sbuf);

  }
  return pala;
}


static int searchmap(char* item, int block) {
  int i, vertpit, ekapit, tokapit, loyto, apu, pala;

  loyto=0; i=0; pala=-1;

  tokapit=strlen(item);
  while (item[tokapit-1]==' ' && tokapit>0) {tokapit--;}

  while (i<items && loyto==0) {
    if (mapi[i*sizeofitem+2] == block) {

// sprintf(sbuf,"\n(%s <-> %s)",&map[(i*sizeofitem+3)*sizeof(int)],item); sur_print(sbuf);

 ekapit=strlen(&map[(i*sizeofitem+3)*sizeof(int)]);
 while (item[ekapit-1]==' ' && ekapit>0) {ekapit--;}


// sprintf(sbuf,"\n(%i <-> %i)",ekapit,tokapit); sur_print(sbuf);

      if (ekapit==tokapit) {
      vertpit=ekapit;
//      if (ekapit>tokapit) { vertpit=tokapit; }
//      else { vertpit=ekapit; }
      apu=strncmp(&map[(i*sizeofitem+3)*sizeof(int)],item,vertpit);
      if (apu == 0) {
//  sprintf(sbuf,"* "); sur_print(sbuf);
        loyto=1;
        pala=mapi[i*sizeofitem];

       palsti=&remap[i*sizeofcuritem];
// sprintf(sbuf,"\nSEARCHMAP: %s %i",palsti,pala); sur_print(sbuf);

      }
      }

    }
  i++;
  }
  return pala;
}


static void swap(int v[], int i, int j) {
  int temp;
  temp=v[i];
  v[i]=v[j];
  v[j]=temp;
}

static void specintsort(int satsi[], int left, int right) {
  int i, last;

  if (left >= right) return;
  swap(satsi,left,(left+right)/2);
  last=left ;
  for (i=left+1; i<=right; i++)
    if ((unsigned int)satsi[i] < (unsigned int)satsi[left])
      swap(satsi,++last,i);
  swap(satsi,left,last);
  specintsort(satsi,left,last-1);
  specintsort(satsi,last+1,right);
}


static void testcands(int level, int obs, int candlkm) {

  int i,j,k,loyto,haku,eiole;

  for (i=0; i<candlkm; i++) {
    j=0; eiole=0; k=0;
    while (k<=level && eiole==0) {
      loyto=0;
      haku=candpointers[level][i*(level+2)+k+1];
      while (j<maxvars && loyto==0 && kama[obs*sizeofkama+j+1]!=-1 ) {
        if (haku == kama[obs*sizeofkama+j+1]) {
          loyto=1;
//          sur_print("!");
        }
        j++;
      }
      if (loyto==0) eiole=1;
      k++;
    }
    if (eiole==0) {
      candpointers[level][i*(level+2)]++;
//      sur_print("!");
    }
  }
}

static int gencand(int level) {
  int i,j,k,ac,apu,apu2;
  int cc;

  cc=0;
  for (i=0; i<canditems[level-1]; i++) {
    if (candpointers[level-1][i*(level+1)]>=minfreq) {
      for (j=i+1; j<canditems[level-1]; j++) {
        if (candpointers[level-1][j*(level+1)]>=minfreq) {

          if (level == 1) {
            candpointers[1][cc*3]=0;
            candpointers[1][cc*3+1]=candpointers[0][i*2+1];
            candpointers[1][cc*3+2]=candpointers[0][j*2+1];

/*
for (apu=1; apu < 3; apu++) {
  apu2=candpointers[1][cc*3+apu];
  sprintf(sbuf,"%i, ",apu2); sur_print(sbuf);
}
sur_print("\n");
*/
            cc++;
          }
          else {

            ac=0; k=1;

            while (k<level && ac==0) {
              apu=candpointers[level-1][i*(level+1)+k];
              if (apu != candpointers[level-1][j*(level+1)+k]) ac=1;
              candpointers[level][cc*(level+2)+k]=apu;
              k++;
            }

            if (ac == 0) {
              candpointers[level][cc*(level+2)]=0;
              candpointers[level][cc*(level+2)+k]=candpointers[level-1][i*(level+1)+k];
              candpointers[level][cc*(level+2)+k+1]=candpointers[level-1][j*(level+1)+k];
/*
for (apu=1; apu < level+2; apu++) {
  apu2=candpointers[level][cc*(level+2)+apu];
  sprintf(sbuf,"%i, ",apu2); sur_print(sbuf);
}
sur_print("\n");
*/


              cc++;
            }


          }
        }
      }
    }
  }
  return cc;
}

static int freqitemsets() {
  int i,j,k,l;
  i=1; l=canditems[0];
  freqitems[0]=canditems[0];

  while ( l>0 && i<maxvars ) {

    candpointers[i]=muste_malloc(l*l/2*(i+2)*sizeof(int));
    if (candpointers[i] == NULL) { sur_print("\nNot enough memory!");  WAIT; return(-1); }

    k=gencand(i);
    canditems[i]=k;

    sprintf(sbuf,"\nLevel %i canditems=%i ",i+1,k); sur_print(sbuf);

    if (k>0) {

      loopcount=10;
      if (((double)k*(double)obscount/10000) > 5000) { looposa=(int)obscount/10+1; looposacount=looposa; }
      else { looposacount=obscount+1; }

      for (j=0; j<=obscount; j++) {
        if (j > looposacount) {
          looposacount=looposacount+looposa;
          loopcount--;
          sprintf(sbuf,"%i ",loopcount); sur_print(sbuf);
        }
        testcands(i,j,k);
      }
    }

    l=0;
    for (j=0; j<k; j++) {
      if (candpointers[i][j*(i+2)]>=minfreq) l++;
    }
    freqitems[i]=l;
    i++;
    sprintf(sbuf,"freqitems=%i",l); sur_print(sbuf);
  }
  return(1);
}

static int opendatafile() {

  int i,apu;

  i=data_read_open(aineisto,&d); if (i<0) return(-1);
  i=mask(&d); if (i<0) return(-1);
  i=conditions(&d); if (i<0) return(-1);


  if (d.m_act < 1) {
    sprintf(sbuf,"\nNo active variables!"); sur_print(sbuf);
    WAIT; return(-1);
  }

  maxvars=d.m_act;

  check=0; maxlen=0;
  for (i=0; i<d.m_act; ++i) {

    if (d.vartype[d.v[i]][0] != 'S') {
      sprintf(sbuf,"\nOnly string variables supported! (%s)",d.varname[d.v[i]]); sur_print(sbuf);
      check=+1;
    }

    apu=d.varlen[d.v[i]];
    if (apu>maxlen) maxlen=apu;

  }
  if (check>0) { WAIT; return(-1); }
  maxlen++;

  maxobs=d.l2-d.l1+1;
  return(1);
}

static int writeitemfile() {

  int i,j,l,apu;

  i=spfind("ITEMOUTFILE");
  if (i >= 0) {
    sprintf(sbuf,"\nWriting item mapping to the file %s...",spb[i]); sur_print(sbuf);
    fp=muste_fopen(spb[i], "w");
    if (fp == NULL) { sur_print("\nItemoutfile error!");  WAIT; return(-1); }
    fprintf(fp,"BLOCK\tORG\tMAP\tFREQ\n");
    for (j=0; j < items; j++) {
      apu=mapi[j*sizeofitem+1];
      l=mapi[j*sizeofitem+2];
      i=mapi[j*sizeofitem];
      fprintf(fp,"%i\t%s\t%i\t%i\n",l,&map[(j*sizeofitem+3)*sizeof(int)],i,apu);
      if (ferror(fp)) { sur_print("\nItemoutfile error!");  WAIT; return(-1); }
    }
    muste_fclose(fp);
    if (ferror(fp)) { sur_print("\nItemoutfile error!");  WAIT; return(-1); }
    sur_print("DONE!\n");
  }
  return(1);
}


static int readitemfile() {

  int i,j,l,apu;
  char c;

  i=spfind("MAPFILE");
  if (i < 0) {
    sprintf(sbuf,"\nCan't find MAPFILE specification!"); sur_print(sbuf);
    WAIT; return(-1);
  }
  sprintf(sbuf,"\nReading item mapping from the file %s...",spb[i]); sur_print(sbuf);

  fp=muste_fopen(spb[i], "r");
  if (fp == NULL) { sur_print("\nMapfile error!");  WAIT; return(-1); }

    fgets(filebuffer,YMAX,fp);
    if (filebuffer == NULL) { sur_print("\nMapfile error!");  WAIT; return(-1); }

    apu=1; j=0;
    while (apu != EOF) {
      apu=fscanf(fp,"%i\t%s\t%s\t%i\n",&mapi[j*sizeofitem+2],&map[(j*sizeofitem+3)*sizeof(int)],
                                       &remap[j*sizeofcuritem],&mapi[j*sizeofitem+1]);
      if (ferror(fp)) { sur_print("\nMapfile error!");  WAIT; return(-1); }
/*
      sprintf(sbuf,"\n%i: %i %s ->  %s",j,mapi[j*sizeofitem+2],
                                       &map[(j*sizeofitem+3)*sizeof(int)],
                                       &remap[j*sizeofcuritem]);
      sur_print(sbuf);
*/
      mapi[j*sizeofitem]=atoi(&remap[j*sizeofcuritem]);
      j++;

    }
    items=j-1;

  muste_fclose(fp);
  if (ferror(fp)) { sur_print("\nMapfile error!");  WAIT; return(-1); }
  sur_print("DONE!\n");
  return(1);
}

static int mapmalloc() {

  sizeofitem=(4*sizeof(int)+maxlen)/(sizeof(int));
  map=(char *)muste_malloc(maxitems*sizeofitem*sizeof(int));
  mapi=(int *)map;
  if (map == NULL) {
    sur_print("\nNot enough memory!");
    WAIT; return(-1);
  }

  sizeofcuritem=maxlen;
  curitem=(char *)muste_malloc(d.m_act*2*sizeofcuritem);
  if (curitem == NULL) {
    sur_print("\nNot enough memory!");
    WAIT; return(-1);
  }
 return(1);
}

int muste_disco(int argc, char *argv[]) {
  int i,j,k,l;
  char *p, *apuc;
  char ch;
  int apu, apu2, ero;
  int max_dim;
  double mf;

  if (argc==1) {
    printf("This program can be used as a SURVO module only.");
    return(-1);
  }

  s_init(argv[1]);
  if (g<2) {
    init_remarks();
    rem_pr("DISCO <SURVO_data>,<output_line>             / Reijo Sund 2001");
    rem_pr("Knowledge discovery tools for Survo");
    rem_pr("Performs level-wise frequent itemset counting for boolean items");
    wait_remarks(2);
    return(0);
  }

  tulosrivi=0;
  if (g>2) tulosrivi=edline2(word[2],1,1);

  i=spec_init(r1+r-1); if (i<0) return(-1);

  strcpy(aineisto,word[1]);

  met=spfind("METHOD");
  if (met<0) { method=1; }
  else {
     method=1;
     if (strcmp(spb[met],"FIND") == 0) method=1;
     if (strcmp(spb[met],"EXTRACT") == 0) method=2;
     if (strcmp(spb[met],"MAPFIND") == 0) method=3;
     if (strcmp(spb[met],"REMAP") == 0) method=4;
  }

//  sprintf(sbuf,"\n%i\n",method); sur_print(sbuf); WAIT;

  blocko=spfind("BLOCKORDER");
  if (blocko<0) 
    { 
    blocklevels=1;
    sur_print("\nPlease use BLOCKORDER specification"); WAIT; // RS FIXME Add standard behaviour
    return(-1);
    }
  else { blocklevels=strlen(spb[blocko]); }

 //   sprintf(sbuf,"\n%i\n",blocklevels); sur_print(sbuf);


  i=spfind("MAXITEMS");
  if (i < 0) { maxitems=0; }
  else { maxitems=atoi(spb[i]); }

  i=spfind("MINFREQ");
  if (i < 0) { minfreq=1; }
  else { minfreq=atoi(spb[i]); }

  if (method < 3) {

/**********************************************************************************************

  Item mapping extraction

**********************************************************************************************/

    i=opendatafile();
    if (i<0) return(-1);

    if (maxitems==0) { maxitems=maxvars*maxobs; }
  //  sprintf(sbuf,"\n %i",maxitems); sur_print(sbuf);
    i=mapmalloc();
    if (i<0) return(-1);

     groupvar=spfind("GROUP");
     if (groupvar < 0) { grov=0; groupvar=0;   }
     else {
       grov=varfind(&d,spb[groupvar]);
       // sprintf(sbuf,"\n %i",grov); sur_print(sbuf); WAIT;

       groupvar=1;
       groupcheck=(int *)muste_malloc((maxitems+1)*sizeof(int));
       if (groupcheck == NULL) {
         sur_print("\nNot enough memory!");
         WAIT; return(-1);
       }


     }

    sizeofkama=maxvars+1;
    kama=(int *)muste_malloc(maxobs*sizeofkama*sizeof(int));
    if (kama == NULL) {
      sur_print("\nNot enough memory!");
      WAIT; return(-1);
    }

    sur_print("\n\nScanning items...");

    obscount=-1; items=0;
    for (l=d.l1; l<=d.l2; ++l) {
      if (unsuitable(&d,l)) continue;
      obscount++;
      if (groupvar > 0) {
        if (data_alpha_load(&d,l,grov,groupvarname) < 1) {
          sur_print("\nError with GROUP-variable!");
          WAIT; return(-1);
        }

//  sprintf(sbuf,"\n dataalphaload: var %s",d.varname[grov]); sur_print(sbuf); WAIT;
//  sprintf(sbuf,"\n %s",groupvarname); sur_print(sbuf); WAIT;

        if (strcmp(groupvarname,oldgroupvarname) != 0) {
          i=0; while (i<=items) { groupcheck[i]=0; i++; }
          strcpy(oldgroupvarname,groupvarname);
        }


//  sprintf(sbuf,"\n %s gv: %i",groupvarname,groupvar); sur_print(sbuf); WAIT;

      }

      blockcount=0; cumblockcount[0]=0;
      for (i=0; i < blocklevels; i++) {
        curblock=spb[blocko][i];
        for (j=0; j < d.m_act; j++) {
          if (d.vartype[d.v[j]][1] != curblock) continue;
          data_alpha_load(&d,l,d.v[j],&curitem[sizeofcuritem*blockcount]);
          blockcount++;
        }
        cumblockcount[i+1]=blockcount;
      }
      kama[obscount*sizeofkama]=l;
      for (i=0; i < blocklevels; i++) {
        ero=cumblockcount[i+1]-cumblockcount[i];
        for (j=0; j < ero; j++) {
          ch=curitem[sizeofcuritem*(j+cumblockcount[i])];
          if (ch == ' ') {
            kama[obscount*sizeofkama+cumblockcount[i]+j+1]=-1;
          }
          else {
            apu=checkmap(&curitem[sizeofcuritem*(j+cumblockcount[i])],i);
            if (apu<0) return(-1);
//    sprintf(sbuf,"%3.0i ",apu); sur_print(sbuf);
            kama[obscount*sizeofkama+cumblockcount[i]+j+1]=apu;
          }

  //      sprintf(sbuf,"%i ",kama[obscount*sizeofkama+j+1]  ); sur_print(sbuf);
        }
      specintsort(&kama[obscount*sizeofkama+cumblockcount[i]+1],0,ero-1);
      }

    }


    sprintf(sbuf,"DONE!\n%i different items in the data file %s.\n",items,aineisto); sur_print(sbuf);

    data_close(&d);

    i=writeitemfile();
    if (i<0) return(-1);

/**********************************************************************************************/
  }

  if (method == 4) {

    i=opendatafile();
    if (i<0) return(-1);
    if (maxitems==0) { maxitems=maxvars*maxobs; }
    i=mapmalloc();
    if (i<0) return(-1);

  sizeofcuritem=maxlen;
  remap=(char *)muste_malloc(maxitems*sizeofcuritem);
  if (remap == NULL) {
    sur_print("\nNot enough memory!");
    WAIT; return(-1);
  }
    i=readitemfile();
    if (i<0) return(-1);

//  writeitemfile();


    sizeofkama=maxvars+1;
    kamamap=(char **)muste_malloc(8*sizeofkama*sizeof(char **));
    if (kamamap == NULL) {
      sur_print("\nNot enough memory!");
      WAIT; return(-1);
    }

    kama=(int *)muste_malloc(8*sizeofkama*sizeof(int));
    if (kama == NULL) {
      sur_print("\nNot enough memory!");
      WAIT; return(-1);
    }


    i=spfind("REMAPOUTFILE");
    if (i < 0) {
      sprintf(sbuf,"\nCan't find REMAPOUTFILE specification!"); sur_print(sbuf);
      WAIT; return(-1);
    }


    fp=muste_fopen(spb[i], "w");
    if (fp == NULL) { sur_print("\nRemapoutfile error!");  WAIT; return(-1); }


    sprintf(sbuf,"\nRemapping data to the file %s...",spb[i]); sur_print(sbuf);


    fprintf(fp,"OBS");
    for (i=1; i<=maxvars; i++) {
      fprintf(fp,"\tX%d",i);
    }
    fprintf(fp,"\n");
    if (ferror(fp)) { sur_print("\nRemapoutfile error!");  WAIT; return(-1); }

    for (l=d.l1; l<=d.l2; ++l) {
      if (unsuitable(&d,l)) continue;



      blockcount=0; cumblockcount[0]=0;
      for (i=0; i < blocklevels; i++) {
        curblock=spb[blocko][i];
        for (j=0; j < d.m_act; j++) {
          if (d.vartype[d.v[j]][1] != curblock) continue;
          data_alpha_load(&d,l,d.v[j],&curitem[sizeofcuritem*blockcount]);

// sprintf(sbuf,"%s ",&curitem[sizeofcuritem*blockcount]); sur_print(sbuf);

          blockcount++;
        }
        cumblockcount[i+1]=blockcount;
      }
      kama[0]=l;

      for (i=0; i < blocklevels; i++) {
        ero=cumblockcount[i+1]-cumblockcount[i];
        for (j=0; j < ero; j++) {

// sprintf(sbuf,"%s ",&curitem[sizeofcuritem*(j+cumblockcount[i])]); sur_print(sbuf);

          ch=curitem[sizeofcuritem*(j+cumblockcount[i])];
          if (ch == ' ') {
            kama[cumblockcount[i]+j+1]=-1;
          }
          else {
          apu=searchmap(&curitem[sizeofcuritem*(j+cumblockcount[i])],i);
// sprintf(sbuf,"%3.0i ",apu); sur_print(sbuf);
        kama[cumblockcount[i]+j+1]=apu;
        kamamap[cumblockcount[i]+j+1]=palsti;
// sprintf(sbuf,"BACK:%s - %s",palsti,kamamap[cumblockcount[i]+j+1]); sur_print(sbuf); WAIT;
    //      sprintf(sbuf,"%i ",kama[obscount*sizeofkama+j+1]  ); sur_print(sbuf);
          }
        }
//      specintsort(&kama[cumblockcount[i]+1],0,ero-1);
      }

      fprintf(fp,"%i",kama[0]);
      for (j=1; j < sizeofkama; j++) {
        apuc=kamamap[j];
        apu=kama[j];
        if (apu<0) { fprintf(fp,"\t"); }
        else { fprintf(fp,"\t%s",apuc); }
      }
      fprintf(fp,"\n");
    }

//    sprintf(sbuf,"DONE!\n%i different items in the data file %s.\n",items,aineisto); sur_print(sbuf);

    muste_fclose(fp);
    data_close(&d);

  }



  if ((method == 1) || (method == 3)) {

/**********************************************************************************************

  Frequent itemset search

**********************************************************************************************/

    mf=(double)minfreq/items*100;
    sprintf(sbuf,"\nSearching for frequent itemsets [ MINFREQ=%i (%2.2f%%) ]:",minfreq,mf);
    sur_print(sbuf);

    sprintf(sbuf,"\nLevel 1 canditems=%i ",items); sur_print(sbuf);

    candpointers[0]=(int *)muste_malloc(items*2*sizeof(int));
    if (candpointers[0] == NULL) { sur_print("\nNot enough memory!");  WAIT; return(-1); }
    j=0;
    for (i=0; i<items; i++) {
      apu=mapi[i*sizeofitem+1];
      if (apu >= minfreq) {
        apu2=mapi[i*sizeofitem];
        candpointers[0][j*2+1]=apu2;
        candpointers[0][j*2]=apu;
        j++;
      }
    }
    canditems[0]=j;

    sprintf(sbuf,"freqitems=%i",j); sur_print(sbuf);

    i=freqitemsets();
    if (i<0) return(-1);

    i=spfind("FREQOUTFILE");
    if (i >= 0) {
      sprintf(sbuf,"\nWriting results to the file %s...",spb[i]); sur_print(sbuf);
      fp=muste_fopen(spb[i], "w");
      if (fp == NULL) { sur_print("\nResultfile error!");  WAIT; return(-1); }

      i=0;
      while (freqitems[i]>0) {
        fprintf(fp,"%i frequent level %i itemsets [ MINFREQ=%i (%2.2f%%) ]\nFREQ\tITEMSET\n",freqitems[i],i+1,minfreq,mf);
        for (j=0; j<canditems[i]; j++) {
          apu=candpointers[i][j*(i+2)];
          if (apu>=minfreq) {
            fprintf(fp,"%i\t",apu);
            for (k=0; k<i+1; k++) {
              fprintf(fp,"%s ",&map[((candpointers[i][j*(i+2)+k+1]-1)*sizeofitem+3)*sizeof(int)]);
            }
            fprintf(fp,"\n");
          }
        }
        fprintf(fp,"\n");
        i++;
      }

      muste_fclose(fp);
      if (ferror(fp)) { sur_print("\nResultfile error!");  WAIT; return(-1); }
      sur_print("DONE!\n");
    }

/**********************************************************************************************/
  }

/*
sur_print("\n");
for (i=0; i < canditems[0]; i++) {
  for (j=0; j < 2; j++) {
    apu=candpointers[0][i*2+j];
    sprintf(sbuf,"%i ",apu); sur_print(sbuf);
  }
  sur_print("\n");
}

sur_print("\n");
for (i=0; i <= obscount; i++) {
  for (j=0; j < sizeofkama; j++) {
    apu=kama[i*sizeofkama+j];
    sprintf(sbuf,"%5.0i ",apu); sur_print(sbuf);
  }
  sur_print("\n");
}

for (j=0; j < items; j++) {
  apu=map[j*sizeofitem+maxlen+sizeof(int)];
  l=map[j*sizeofitem+maxlen+2*sizeof(int)];
  i=map[j*sizeofitem+maxlen];
  sprintf(sbuf,"%i: %s = %i: %i\n",l,&map[j*sizeofitem],i,apu); sur_print(sbuf);
}

  muste_free(map);
  muste_free(kama);
*/
  s_end(argv[1]);

// WAIT;
return 0;
}


