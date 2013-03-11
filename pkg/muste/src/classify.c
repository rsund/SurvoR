#include "muste.h"
/* !classif.c 13.3.1987/SM (2.10.1993) (19.1.1996)
*/
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
// #include <conio.h>
#include <math.h>
// #include <malloc.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

/*  - 19.1.1996
#define MAXTILA 16*EP4
#define MAXCLASS 2*EP4
*/

#define MAXTILA 320000
#define MAXCLASS 20000

static SURVO_DATA d;
static int invar,outvar;
static char intype,outtype;
static char *stila;
static char **class;
static double *numclass;
static int nclass;
static int *classtype; /* 0=point 1=interval */
static char **lower,**upper;
static double *al,*bl;
static char *others;
static double othvalue;
static int *same;
static char sp_char,wild,partial;
static int othersame=0;
static int prind;
static int muste_rawclass;

static int tutki_tyyppi();
static int varaa_tilat();
static int not_enough_memory();
static int tutki_luokitus();
static char *sijoita(char *p,char *s);
static int create_outvar();
static int muunto();
static int w_strncmp(char *wsana,char *sana,int len,char w);
static int line_error(int j);
static int sp_muunto(char *s);   /* 12.2.1992 */

// char **specs;
/*
main(argc,argv)
int argc; char *argv[];
*/
void muste_classify(char *argv)
        {
        int i;
        char *p; // 4.2.2010

//      if (argc==1) return;
        s_init(argv);

        if (g<5)
            {
            sur_print("\nUsage:");
            sur_print("\nCLASSIFY <data>,<classification>,<input_var>,<output_var>");
            WAIT; return;
            }

        i=data_open2(word[1],&d,1,0,0); if (i<0) return;
        if (d.type==1 && d.d1.mask==NULL)
            {
            sur_print("\nCannot write in the data matrix!");
            sur_print("\nMask line in DATA <name>,L1,L2,<label line>,<mask line> missing!");
            WAIT; return;
            }
        i=sp_init(r1+r-1); if (i<0) return;
        i=conditions(&d); if (i<0) return;

        prind=1;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND"); if (i>=0) prind=atoi(spb[i]);

        invar=varfind2(&d,word[3],1); if (invar<0) return;
        intype=d.vartype[invar][0];

        if (d.type==1) { i=tutki_tyyppi(); if (i==0) intype='8'; }

        strcpy(sbuf,word[4]); // 4.2.2010 vanhan muuttujan tyyppi pois
        p=strchr(sbuf,':'); if (p!=NULL) *p=EOS;
        outvar=varfind2(&d,sbuf,0);

//      outvar=varfind2(&d,word[4],0);

        if (outvar>=0) outtype=d.vartype[outvar][0];

        i=varaa_tilat(); if (i<0) return;

        muste_rawclass=0;
        i=spfind("RAW"); if (i>=0) muste_rawclass=atoi(spb[i]); // RS 11.3.2013

        sp_char=EOS; i=spfind("SPACE");  /* 12.2.1992 */
        if (i>=0) sp_char=*spb[i];
        wild=EOS; i=spfind("WILD");
        if (i>=0) wild=*spb[i];
        partial=EOS; i=spfind("PARTIAL");
        if (i>=0) partial=*spb[i];

        i=tutki_luokitus(); if (i<0) return;

//Rprintf("\noutvar=%d",outvar); getch();
        if (outvar<0) { i=create_outvar(); if (i<0) return; }
        if (outtype!='S')
            for (i=0; i<nclass; ++i)
                {
                if (*class[i]==EOS) numclass[i]=MISSING8;
                else numclass[i]=atof(class[i]);
                }
        if (others!=NULL)
          {
          if (strcmp(others,"MISSING")==0) { othvalue=MISSING8; *others=EOS; }
          else if (strcmp(others,"SAME")==0) othersame=1;  /* 2.10.1993 */
          else if (strcmp(others,"NO_CHANGE")==0 ||
                   strcmp(others,"NO CHANGE")==0) othersame=2;  // 30.9.2000
                              // syy: SPACE=_
          else othvalue=atof(others);
          }

        muunto();
        data_close(&d);
        s_end(argv);
        }

static int tutki_tyyppi()
        {
        int k;
        long j;
        char x[LLENGTH];
        double a;

        k=0;
        for (j=d.l1; j<=d.l2; ++j)
            {
            data_load(&d,j,invar,&a);
            if (a==MISSING8) continue;
            data_alpha_load(&d,j,invar,x);
            if (!muste_isnumber(x)) { k=1; break; }
            }
        return(k);
        }

static int varaa_tilat()
        {
        stila=muste_malloc(MAXTILA);
        if (stila==NULL) { not_enough_memory(); return(-1); }
        class=(char **)muste_malloc(MAXCLASS*sizeof(char *));
        if (class==NULL) { not_enough_memory(); return(-1); }
        numclass=(double *)muste_malloc(MAXCLASS*sizeof(double));
        if (numclass==NULL) { not_enough_memory(); return(-1); }
        classtype=(int *)muste_malloc(MAXCLASS*sizeof(int));
        if (classtype==NULL) { not_enough_memory(); return(-1); }
        lower=(char **)muste_malloc(MAXCLASS*sizeof(char *));
        if (lower==NULL) { not_enough_memory(); return(-1); }
        upper=(char **)muste_malloc(MAXCLASS*sizeof(char *));
        if (upper==NULL) { not_enough_memory(); return(-1); }
        al=(double *)muste_malloc(MAXCLASS*sizeof(double));
        if (al==NULL) { not_enough_memory(); return(-1); }
        bl=(double *)muste_malloc(MAXCLASS*sizeof(double));
        if (bl==NULL) { not_enough_memory(); return(-1); }
        same=(int *)muste_malloc((MAXCLASS+1)*sizeof(int)); /* same[nclass] required */
        if (same==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static int not_enough_memory()
        {
        sur_print("\nNot enough memory!");
        WAIT;
        return(1);
        }

static int tutki_luokitus()
        {
        extern char *sijoita();
        int i,j;
        char *p,*q;
        char x[LLENGTH], xx[LLENGTH], *osa[EP4];
        int k,h;

        j=wfind("CLASSIFICATION",word[2],1);
        if (j<0)
            {
            sprintf(sbuf,"\nCLASSIFICATION %s not found!",word[2]);
            sur_print(sbuf); WAIT; return(-1);
            }

        p=stila; nclass=0; others=NULL;
        while (1)
            {
            ++j; if (j>r2) break;
            edread(x,j);

            q=strchr(x+1,':');  // poista kommentit! 3.9.2002
            if (q!=NULL)
                {
                ++q; while (*q==' ') ++q;
                if (*q!=EOS)
                    {
                    while (*q!=EOS && *q!=' ') ++q;
                    if (*q!=EOS)
                        {
                        q=strstr(q," / ");
                        if (q!=NULL) *q=EOS;
                        }
                    }
                }

            strcpy(xx,x);
            
            if (muste_rawclass) // RS 11.3.2013
                {
                q=strrchr(xx+1,':');
                if (q==NULL) break;
                *q=EOS;
                k=splitq(q+1,osa,EP4);
                q=osa[0];
                k=2; osa[0]=xx+1; osa[1]=q;          
                }
            else k=splitq(xx+1,osa,EP4);

            if (sp_char) /* 12.2.1992 */
                {
                for (i=0; i<k; ++i) sp_muunto(osa[i]);
                }

            if (k==0 || strcmp(osa[0],"END")==0 || strncmp(osa[0],"END ",4)==0) break; // RS 11.3.2013 strncmp
            if (k<2)
                { line_error(j); return(-1); }
            i=strlen(osa[k-2])-1; if (osa[k-2][i]==':') osa[k-2][i]=EOS;

            if (k==2 && (strncmp(osa[0],"OTHER",5)==0 || strcmp(osa[0],"DEFAULT")==0))
                { others=p; p=sijoita(p,osa[1]); if (p==NULL) return(-1); continue; }


            if (strcmp(osa[k-1],"MISSING")==0) *osa[k-1]=EOS;
            if (strcmp(osa[k-1],"SAME")==0)
                {
                same[nclass]=1;
                if (outvar>=0)
                    if ((outtype=='S' && intype!='S') || (outtype!='S' && intype=='S'))
                        {
                        sur_print("\nIf SAME is used, mapping from string to");
                        sur_print("\nnumeric variables or vice versa is not permitted!");
                        WAIT; return(-1);
                        }
                }
            else same[nclass]=0;
            class[nclass]=p; p=sijoita(p,osa[k-1]); if (p==NULL) return(-1);
            classtype[nclass]=0;
            if (!muste_rawclass) q=NULL; // RS 11.3.2013           
            else q=strchr(osa[0],'-');
            if (strcmp(osa[0],"FROM")==0 && strcmp(osa[2],"TO")==0 && k==5)
                {
                classtype[nclass]=1;
                lower[nclass]=p; p=sijoita(p,osa[1]); if (p==NULL) return(-1);
                upper[nclass]=p; p=sijoita(p,osa[3]); if (p==NULL) return(-1);
                }
            else if (strcmp(osa[1],"-")==0 && k==4)
                {
                classtype[nclass]=1;
                lower[nclass]=p; p=sijoita(p,osa[0]); if (p==NULL) return(-1);
                upper[nclass]=p; p=sijoita(p,osa[2]); if (p==NULL) return(-1);
                }
            else if (intype!='S' && (q!=NULL && q-osa[0]>0) )
                {
                classtype[nclass]=1;
                *q=EOS; ++q;
                lower[nclass]=p; p=sijoita(p,osa[0]); if (p==NULL) return(-1);
                upper[nclass]=p; p=sijoita(p,q); if (p==NULL) return(-1);
                }
            else
                {
                for (h=0; h<k-1; ++h)
                    {
                    if (h>0)
                        {
                        ++nclass;
                        if (nclass>MAXCLASS)
                            {
                            sur_print("\nToo many cases in classification!");
                            WAIT; return(-1);
                            }
                        class[nclass]=p; p=sijoita(p,class[nclass-1]); if (p==NULL) return(-1);
                        same[nclass]=same[nclass-1];
                        }
                    classtype[nclass]=0;
                    if (strcmp(osa[h],"MISSING")==0) *osa[h]=EOS;
                    lower[nclass]=p; p=sijoita(p,osa[h]); if (p==NULL) return(-1);
                    upper[nclass]=lower[nclass];
                    }
                }
            ++nclass;
            }

        for (i=0; i<nclass; ++i)
            {
            if (*lower[i]==EOS) al[i]=bl[i]=MISSING8;
            else
                {
                al[i]=atof(lower[i]);
                bl[i]=atof(upper[i]);
                }
            }
/* 
   Rprintf("\nnclass=%d",nclass);
    for (i=0; i<nclass; ++i)
       Rprintf("\n%d: %d %s %s : %s",i,classtype[i],lower[i],upper[i],class[i]);
      
    getch();
 Rprintf("\n");
  for (i=0; i<p-stila; ++i) { if (stila[i])Rprintf("%c",stila[i]); elseRprintf("@"); }
  getch();
*/
        return(1);
        }

static char *sijoita(char *p,char *s)
        {
        int len;

        len=strlen(s);
        if (p-stila>MAXTILA-len-1)
            {
            sur_print("\nNot space enough for classification!");
            WAIT; return(NULL);
            }
        while (*s) *p++=*s++; *p++=EOS;
        return(p);
        }

static int line_error(int j)
        {
        sprintf(sbuf,"\nError on edit line %d",j); sur_print(sbuf);
        WAIT;
        return(1);
        }

static int sp_muunto(char *s)    /* 12.2.1992 */
        {
        char *p;

        p=s;
        while ((p=strchr(p,sp_char))!=NULL) { *p=' '; ++p; }
        return(1);
        }

/* cla2.c 13.3.1987/SM (2.10.1993) (22.1.1996)
*/

static int create_outvar()
        {
        int i;
        int des=0;
        int neg=0;
        int num=1;
        char type;
        int len=0;
        double max=-1e300;
        double x;
        int sametype=0;
        char y[LNAME];
        char *p;

        same[nclass]=0;
        if (others!=NULL && strcmp(others,"SAME")==0)
            {
            type=d.vartype[invar][0];
            len=d.varlen[invar];
            same[nclass]=1;
            sametype=1;
            }
        else
            for (i=0; i<nclass; ++i)
                {
                if (*class[i]==EOS) continue;  /* 12.2.1992 */
                if (strcmp(class[i],"SAME")==0)
                    {
                    type=d.vartype[invar][0];
                    len=d.varlen[invar];
                    sametype=1;
                    break;
                    }
                if (!muste_isnumber(class[i])) num=0;
                if (*class[i]=='-') neg=1;
                if (strchr(class[i],'.')!=NULL) des=1;
                if (strlen(class[i])>len) len=strlen(class[i]);
                x=fabs(atof(class[i])); if (x>max) max=x;
                }
        strcpy(y,word[4]); // 4.2.2010
//    Rprintf("\nword4=%s|",word[4]); getch();
        if (!sametype)
            {
//    Rprintf("\nword4=%s|",word[4]); getch();
//          strcpy(y,word[4]); // 16.12.2009

            p=strchr(y,':');
            if (p!=NULL)
                {
                *p=EOS;
                type=*(p+1);
                if (strchr("1248S",type)==NULL)
                    {
                    sprintf(sbuf,"\nInvalid type %c for %s!",type,y);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                if (type=='S') len=atoi(p+2);
                else { *sbuf=type; sbuf[1]=EOS; len=atoi(sbuf); }
                if (len==0) len=1;
                }
          else
           {
            if (num)
                {
                if (!neg && !des && max<255.0) { type='1'; len=1; }
                else if (!des && max<32767.0) { type='2'; len=2; }
                else { type='4'; len=4; }
                }
            else type='S';
            }
           }

        p=strchr(y,':'); if (p!=NULL) *p=EOS; // 4.2.2010
        outvar=create_newvar(&d,y,type,len);
        if (outvar<0) return(-1);
        outtype=type;
        return(1);
        }

static int muunto()
        {
        int i,k;
        long j;
        double x,y;
        char jakso[LLENGTH];
        char *py;
        char other_str[LLENGTH];

        sur_print("\nClassifying observations...");
        if (intype!='S')
            {
            for (j=d.l1; j<=d.l2; ++j)
                {
                if (unsuitable(&d,j)) continue;
                if (prind) { sprintf(sbuf," %ld",j); sur_print(sbuf); }
                data_load(&d,j,invar,&x);

                for (i=0; i<nclass; ++i)
                    {
                    if (classtype[i]==0)
                        {
                        if (x==al[i]) break; else continue;
                        }
                    if (x>=al[i] && x<=bl[i]) break; else continue;
                    }
                if (i==nclass)
                    {
                    if (others)
                        {
                        if (othersame==2) // NO_CHANGE 30.9.2000
                            {
                            continue;
                            }
                        else if (othersame)  /* 2.10.93 */
                            {
                            y=x;
                            sprintf(other_str,"%g",x); py=other_str; /* 22.1.1996 */
                            }
                        else { y=othvalue; py=others; }
                        }
                    else
                        {
                        sprintf(sbuf,"\nValue %g in observation #%ld cannot be classified!",
                                        x,j); sur_print(sbuf);
                        WAIT; return(-1);
                        }
                    }
                else
                    { y=numclass[i]; py=class[i]; }

                if (i<nclass && same[i]) y=x;   /* both numeric */
                 /* 17.2.92 */
                if (outtype!='S')
                    {
                    data_save(&d,j,outvar,y);
                    }
                else
                    {
                    if (*py==EOS) py=space;  /* 12.2.1992 */
                    if (d.type==2)
                        {
                        strcpy(sbuf,py);
                        k=d.d2.varlen[outvar]-strlen(sbuf); /* 22.1.1996 */
                        if (k>0) strncat(sbuf,space,k);
                        fi_alpha_save(&d.d2,j,outvar,sbuf);
                        }
                    else if (d.type==1) ma_save(&d.d1,(int)j,outvar,py);
                    else
                        {
                        sur_print("\nCannot save data values!");
                        WAIT; return(-1);
                        }
                    }
                }
            }
        else /* intype='S' */
            {
            for (j=d.l1; j<=d.l2; ++j)
                {
                if (unsuitable(&d,j)) continue;
                if (prind) { sprintf(sbuf," %ld",j); sur_print(sbuf); }
                data_alpha_load(&d,j,invar,jakso);
// Rprintf("\nclassifying: %s",jakso);
                for (i=0; i<nclass; ++i)
                    {
                    if (classtype[i]==0)
                        {
                 /*     if (*jakso!=*lower[i]) continue;  -12.2.92 wild,partial */
                        if (strncmp(lower[i],jakso,strlen(lower[i]))==0) break;
                        if (wild)
                            {
             if (w_strncmp(lower[i],jakso,strlen(lower[i]),wild)==0) break;
                            }
                        if (partial)
                            {
                            if (*lower[i]==partial &&
                                strstr(jakso,lower[i]+1)!=NULL) break;
                            }
                        continue;

                        }
                    if (strncmp(jakso,lower[i],strlen(lower[i]))>=0 &&
                       strncmp(jakso,upper[i],strlen(upper[i]))<=0) break; else continue;
                    }
// Rprintf("\nclass: %d",i);
                if (i==nclass)
                    {
                    if (othersame==2) // NO_CHANGE 30.9.2000
                        {
                        continue;
                        }
                    else if (others)
                        {
                        if (othersame)  /* 2.10.93 */
                            {
                            py=jakso; y=atof(jakso);
                            }
                        else { y=othvalue; py=others; }
                        }
                    else
                        {
                        sprintf(sbuf,"\nValue %s in observation #%ld cannot be classified!",
                                        jakso,j); sur_print(sbuf);
                        WAIT; return(-1);
                        }
                    }
                else
                    { y=numclass[i]; py=class[i]; }
// Rprintf("\ny: %f, py: %d",y,py);
                if (i<nclass && same[i]) py=jakso;  /* both strings */
                 /* 17.2.92 */
                if (outtype!='S')
                    {
                    data_save(&d,j,outvar,y);
                    }
                else
                    {
                    if (*py==EOS) py=space;  /* 12.2.1992 */
                    if (d.type==2)
                        {
                        strcpy(sbuf,py);
                        k=d.d2.varlen[outvar]-strlen(sbuf); /* 22.1.1996 */
                        if (k>0) strncat(sbuf,space,k);
                        fi_alpha_save(&d.d2,j,outvar,sbuf);
                        }
                    else if (d.type==1) ma_save(&d.d1,(int)j,outvar,py);
                    else
                        {
                        sur_print("\nCannot save data values!");
                        WAIT; return(-1);
                        }
                    }
                }
            }
        return(1);
        }

static int w_strncmp(char *wsana,char *sana,int len,char w)
        {
        int i;
        char *p,*q;

        p=wsana; q=sana;
        i=0;
        while (i<len)
            {
            if (*p!=w && *p!=*q) return(-1);
            ++i; ++p; ++q;
            }
        return(0);
        }

