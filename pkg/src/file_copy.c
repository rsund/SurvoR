/* copy.c 8.3.1986/SM (16.11.1993)
   FILE COPY
*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define NIMIMAX 40
#define N_MATCH 10

extern int survo_ferror;


static SURVO_DATA d1,d2;

static int *v2;  /* malloc */
static long j,j2;
static char jakso[LLENGTH];
static char **form;
static int prind=0;
static char word2[LLENGTH];
static char word3[LLENGTH];

static int expand=0;
static int expm,expn;
static char tempn[LNAME];
static char tempn1[LNAME];

static int new_file=0; // FILE COPY FILE1 TO NEW FILE2  23.2.2004

int kok[EP4],des[EP4],tyyppi[EP4],neg[EP4];
char ntila[EP4*NIMIMAX];
char **uvarname;
int *uvarlen;
char **uvartype;

char match_name[LNAME];
int match_var,match_var2;
int odd_var, odd_mode;
long last_found;

static int n_match;
static int m_var[N_MATCH];
static int m_var2[N_MATCH];
static int num_match[N_MATCH];
static char m_name[N_MATCH][9];
static double mx1[N_MATCH];
static char m_jakso[N_MATCH][LLENGTH];

/*
static char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "MATCH", "NEWSPACE", "MODE", "ODD",
                 "PRIND", "!" };
static char **specs=specs0;
*/

static void tilanpuute()
        {
        PR_EBLD;
        sur_print("\nNot enough memory for FILE COPY!");
        PR_ENRM; WAIT;
        }

static int varaa_tilat()
        {
        v2=(int *)malloc(d1.m_act*sizeof(int));
        if (v2==NULL) { tilanpuute(); return(-1); }
        form=(char **)malloc(d1.m_act*sizeof(char **));
        if (form==NULL) { tilanpuute(); return(-1); }
        return(1);
        }


static int varaa_tilat2()
        {
        uvarname=(char **)malloc(d1.m_act*sizeof(char **));
        if (uvarname==NULL) { tilanpuute(); return(-1); }
        uvarlen=(int *)malloc(d1.m_act*sizeof(int));
        if (uvarlen==NULL) { tilanpuute(); return(-1); }
        uvartype=(char **)malloc(d1.m_act*sizeof(char **));
        if (uvartype==NULL) { tilanpuute(); return(-1); }
        return(1);
        }

static int tutki_madata()
        {
        int i,k,m,len,ii;
        long j;
        char *p,*q;
        char jakso[LLENGTH];
        char x[LLENGTH];

        sprintf(sbuf,"\nTesting structure of data matrix %s...\n",word[2]);
        sur_print(sbuf);
        m=d1.m;
        if (m>EP4)
            {
            sprintf(sbuf,"\nToo many columns in data matrix! (max=%d)",EP4);
            sur_print(sbuf); WAIT; return(-1);
            }
        for (i=0; i<m; ++i) kok[i]=des[i]=tyyppi[i]=neg[i]=0;
                   /* tyyppi: 0=pos.luku 1=luku 2=string */
        for (j=d1.l1; j<=d1.l2; ++j)
            {
            if (prind) { sprintf(sbuf,"%ld ",j); sur_print(sbuf); }
            if (unsuitable(&d1,j)) continue;
            for (i=0; i<m; ++i)
                {
                k=ma_load(&d1.d1,(int)j,i,jakso,1);
                if (k<0) return(-1);
                if (tyyppi[i]==2)
                    {
                    len=strlen(jakso); if (len>kok[i]) kok[i]=len;
                    continue;
                    }
                if (ma_missing(jakso)) continue;
                if (muste_isnumber(jakso))
                    {
                    p=jakso; while (*p==' ') ++p;
                    if (*p=='-') { ++p; neg[i]=1; }
                    len=strlen(p); while (p[len-1]==' ') p[--len]=EOS;
                    if (*p=='+') ++p;
                    else if (*p=='-') { ++p; tyyppi[i]=1; }
                    q=strchr(p,'.');
                    if (q==NULL)
                        {
                        k=strlen(p); if (k>kok[i]) kok[i]=k;
                        }
                    else
                        {
                        k=q-p; if (k>kok[i]) kok[i]=k;
                        k=strlen(p)-(k+1); if (k>des[i]) des[i]=k;
                        }
                    }
                else
                    {
                    tyyppi[i]=2;
                    len=strlen(jakso); if (len>kok[i]) kok[i]=len;
                    }
                }
            }

        p=ntila;
        for (i=0; i<m; ++i)       /* (###.##) formaatit  19.4.1992 */
            {
            if (tyyppi[i]==2) continue;
            len=kok[i]+neg[i];
            k=len;
            if (des[i]) k+=des[i]+1;
            ii=strlen(d1.varname[i]);
            while (d1.varname[i][ii-1]==' ') --ii;
            ii-=k;
            if (ii>0) len+=ii;
            x[0]='('; ii=0;
            for (k=0; k<len; ++k) x[++ii]='#';
            if (des[i]>0)
                {
                x[++ii]='.';
                for (k=0; k<des[i]; ++k) x[++ii]='#';
                }
            x[++ii]=')'; x[++ii]=EOS;
            len=strlen(x);
            if (len>NIMIMAX-9) continue;
            k=sprintf(p,"%-8.8s %s",d1.varname[i],x);
            d1.varname[i]=p;
            p+=k+1;
            }

/*
printf("\ntyypit:");
for (i=0; i<m; ++i) printf("\ni=%d tyyppi=%d kok=%d des=%d",
                              i+1,tyyppi[i],kok[i],des[i]);
getch();
*/
        return(1);
        }



static int luo_uusi()
        {
        int i;
        int filen,fim1,fim,fil,fiextra,fitextn,fitextlen;
        char **fitext;
        char *privi[1];
        char xx[LLENGTH],*xword[2];
        int new_l=-1,new_f=-1;

        if (!new_file && etu==0 && !expand)
            {
        sprintf(sbuf,"\nSince Survo data file %s does not exist,",word[3]); sur_print(sbuf);
        sur_print("\ncreating a new one...");
        sur_sleep(1000L);
            }
        if (d1.type==4) { sur_print("\nNot permitted for matrices! Use FILE SAVE MAT operation!");
                          WAIT; return(-1); }
        if (d1.type==2 || d1.type==1)
            {
            new_l=-999;
            i=spfind("NEWSPACE"); /* 16.11.1993 */
            if (i>=0)
                {
                strcpy(xx,spb[i]);
                if (muste_strcmpi(xx,"SAME")==0) // 15.1.2001
                    {
                    new_l=-1; new_f=-1;
                    }
            else
                    {
                    i=split(xx,xword,2);
                    if (i<2)
                        {
                        sur_print("\nUsage: NEWSPACE=<# of new bytes>,<# of new fields>");
                        WAIT; return(-1);
                        }
                    new_l=atoi(xword[0]); new_f=atoi(xword[1]);
                    }
                }
            }

        if (expand) // 29.12.2003
            {
            if (expm==-9999) expm=d1.m_act/4+4;
            else if (expm==-11111) expm=d1.d2.m-d1.d2.m1; // minimoi
            else if (expm<d1.d2.m-d1.d2.m1) expm=d1.d2.m-d1.d2.m1;

            filen=0;
            for (i=0; i<d1.m; ++i) filen+=d1.varlen[i];

            if (expn==-9999) expn=d1.d2.len/4+20;
            else if (expn==-11111) // minimoi
                expn=filen-d1.d2.len;
            else if (expn<filen-d1.d2.len)
                expn=filen-d1.d2.len;
            new_f=expm;
            new_l=expn;
            }

        if (d1.type==2)
            {
            filen=0;
            for (i=0; i<d1.m_act; ++i) filen+=d1.varlen[d1.v[i]];
            if (new_l!=-999)
                {
                filen=d1.d2.len;
                if (new_l!=-1)
                    filen+=new_l;
                fim=d1.m_act;
                fim1=d1.d2.m1;
                if (new_f!=-1)
                    {
                    if (expand) // 29.12.2003
                        fim1+=new_f;
                    else
                        fim1=fim+new_f;
                    }

                }
            else
                {
                filen+=filen/4+20;
                fim=d1.m_act; fim1=fim+fim/4+4;
                }

            fil=d1.d2.l;
            fiextra=d1.d2.extra;
            fitextn=d1.d2.textn;
            fitextlen=d1.d2.textlen;
            fitext=d1.d2.fitext;

            i=varaa_tilat2(); if (i<0) return(-1);   /* 15.3.1993 */

            for (i=0; i<d1.m_act; ++i)
                {
/*  -15.3.93    d1.d2.varname[i]=d1.d2.varname[d1.v[i]];     JÑrjestys
                d1.d2.varlen[i]=d1.d2.varlen[d1.v[i]];       ei sama, jos
                d1.d2.vartype[i]=d1.d2.vartype[d1.v[i]];     VARS kÑytîssÑ
*/
                uvarname[i]=d1.d2.varname[d1.v[i]];
                uvarlen[i]=d1.d2.varlen[d1.v[i]];
                uvartype[i]=d1.d2.vartype[d1.v[i]];
                }
// printf("\nexpand=%d tempn=%s|",expand,tempn); getch();
//            if (expand) i=sur_delete1(tempn); // 29.12.2003
// printf("\ndelete=%d|",i); getch();
            i=fi_create(word3,filen,fim1,fim,0L,fil,fiextra,fitextn,fitextlen,
                        fitext,uvarname,uvarlen,uvartype);
            if (i<0) return(-1);
            data_close(&d1);
            data_read_open(word2,&d1);  /* 11.9.91 */
            if (expand) // 29.12.2003
                {
                for (i=0; i<d1.d2.m; ++i)
                    {
//                  if (d1.d2.vartype[i][1]=='-') d1.d2.vartype[i][1]='_';
                    d1.v[i]=i;
                    }
                d1.m_act=d1.d2.m;
                }
            else
                mask(&d1);
            data_open(word3,&d2);
            return(1);
            }
        else if (d1.type==1)
            {
            i=tutki_madata(); if (i<0) return(-1);
            fim=d1.m;
            for (i=0; i<fim; ++i)       /* des=varlen */
                {
                if (tyyppi[i]==2)
                    {
                    d1.vartype[i][0]='S'; des[i]=kok[i];
                    continue;
                    }
                if (des[i]>0)
                    {
                    if (kok[i]+des[i]>6)
                        { d1.vartype[i][0]='8'; des[i]=8; continue; }
                    d1.vartype[i][0]='4'; des[i]=4; continue;
                    }
                else
                    {
                    if (kok[i]>4)
                        { d1.vartype[i][0]='8'; des[i]=8; continue; }
                    if (kok[i]>2 || neg[i])
                        { d1.vartype[i][0]='2'; des[i]=2; continue; }
                    d1.vartype[i][0]='1'; des[i]=1; continue;
                    }
                }
            filen=0;
            for (i=0; i<fim; ++i) filen+=des[i];
            if (new_l!=-999)
                {
                filen+=new_l;
            /*  fim1+=new_f;  */
                fim1=fim+new_f;
                }
            else
                {
                filen+=filen/4+20;
                fim1=fim+fim/4+4;
                }
            fil=64;
            fiextra=12;
            fitextn=1;
            fitextlen=c2;
            strcpy(jakso," Copy of data matrix "); strcat(jakso,word[2]); privi[0]=jakso;
            fitext=privi;

            i=fi_create(word3,filen,fim1,fim,0L,fil,fiextra,fitextn,fitextlen,
                        fitext,d1.varname,des,d1.vartype);

            if (i<0) return(-1);
            data_close(&d1);
            data_open(word2,&d1);
            data_open(word3,&d2);
            return(1);
            }
        else  /* d1.type==3 */
            {
/*          printf("\nConvert data to a data matrix first!");
            WAIT; return(-1);
*/
            fim=d1.m;
            for (i=0; i<fim; ++i) { d1.vartype[i][0]='4'; d1.varlen[i]=4; }
            filen=0;
            for (i=0; i<fim; ++i) filen+=d1.varlen[i];
            filen+=filen/4+20;
            fim1=fim+fim/4+4;
            fil=64;
            fiextra=12;
            fitextn=1;
            fitextlen=c2;
            strcpy(jakso," Copy of sample "); strcat(jakso,word[2]); privi[0]=jakso;
            fitext=privi;
            uvarlen=(int *)malloc(fim*sizeof(int));  /* 8.12.1998 */
            if (uvarlen==NULL) { tilanpuute(); return(-1); }
            for (i=0; i<fim; ++i) uvarlen[i]=d1.varlen[i];
            i=fi_create(word3,filen,fim1,fim,0L,fil,fiextra,fitextn,fitextlen,
                        fitext,d1.varname,uvarlen,d1.vartype);
            if (i<0) return(-1);
            data_close(&d1);
            data_open(word2,&d1);
            data_open(word3,&d2);
            return(1);
            }

        }

static void sulje()
        {
        data_close(&d1);
        data_close(&d2);
        }

static int tutki_muuttujat()
        {
        int i,h;
        char nimi[9];
        char *p;

        for (i=0; i<d1.m_act; ++i)
            {
            strncpy(nimi,d1.varname[d1.v[i]],8); nimi[8]=EOS;
            h=varfind(&d2,nimi); if (h<0) { sulje(); return(-1); }
            v2[i]=h;
            if (d1.type==2)    /* 10.1.91 */
                {
                if (d2.vartype[v2[i]][0]=='S')
                    {
                    p=strchr(d2.varname[v2[i]],'#');
                    form[i]=p;
                    if (p!=NULL)
                        {
                        while (*p!=')')
                            {
                            ++p;
                            if (*p==EOS)  /* 27.5.1993 */
                                {
         sprintf(sbuf,"\n) missing in format of field %.8s !",d2.varname[v2[i]]);
                                sur_print(sbuf); WAIT; return(-1);
                                }
                            }
                        *p=EOS;
                        }
                    }
                }
/*          if (d1.type==2)
                {
                if (d1.vartype[d1.v[i]][0]!=d2.vartype[v2[i]][0])
                    {
                    sur_print("\nConflicting field types:");
                    sprintf(sbuf,"\nType of field %.8s in source file %s is %c.",
                        d1.varname[d1.v[i]],word[2],d1.vartype[d1.v[i]][0]);
                        sur_print(sbuf);
                    sprintf(sbuf,"\nType of field %.8s in destination file %s is %c.",
                        d2.varname[v2[i]],word[3],d2.vartype[v2[i]][0]);
                        sur_print(sbuf);
                    WAIT; sulje(); return(-1);
                    }
                }
*/
            }
        return(1);
        }

static int kopioi(long j,long j2,int i)
        {
        int h,vi;
        char type1,type2;

        vi=d1.v[i];
        type1=d1.vartype[vi][0]; type2=d2.vartype[v2[i]][0];

//Rprintf("\ntype1: %c, type2: %c, d1.type: %d",type1,type2,d1.type);           


        if (d1.type==2)
            {
            if (type1==type2)
                {
                fi_alpha_load(&d1.d2,j,vi,jakso);
                if (type1=='S')
                    {
                    for (h=d1.varlen[vi]; h<d2.varlen[v2[i]]; ++h)
                        jakso[h]=' ';
                    }

// Rprintf("\n1sour: %ld, dest: %ld, jakso:%c%c%c",j,j2,jakso[0],jakso[1],jakso[2]);                       
                    
                fi_alpha_save(&d2.d2,j2,v2[i],jakso);
                }
            else if (type2=='S')
                {
                double x;

                h=data_load(&d1,j,vi,&x);
                if (h<0) return(-1);

                if (form[i]!=NULL) fconv(x,form[i],jakso);
                else fconv(x,"",jakso);
                if (strlen(jakso)>d2.varlen[v2[i]])
                    strcpy(jakso,space);
                else
                    {
                    for (h=strlen(jakso); h<d2.varlen[v2[i]]; ++h)
                        jakso[h]=' ';
                    }
                fi_alpha_save(&d2.d2,j2,v2[i],jakso);
                }
            else
                {
                double x;

                h=data_load(&d1,j,vi,&x);
//      Rprintf("\n1sour: %ld, dest: %ld, h: %d, x:%f",j,j2,h,x);           


                if (h<0) return(-1);
                data_save(&d2,j2,v2[i],x);
                }
            }
        else if (d1.type==1)
            {
            if (d2.vartype[v2[i]][0]=='S')
                {
                h=data_alpha_load(&d1,j,vi,jakso);
                if (h<0) return(-1);
                for (h=strlen(jakso); h<d2.varlen[v2[i]]; ++h)
                    jakso[h]=' ';
                fi_alpha_save(&d2.d2,j2,v2[i],jakso);
                }
            else
                {
                double x;

                h=data_load(&d1,j,vi,&x);
//      Rprintf("\n2sour: %ld, dest: %ld, h: %d, x:%f",j,j2,h,x);           


                if (h<0) return(-1);
                data_save(&d2,j2,v2[i],x);
                }
            }
        else if (d1.type==3)
            {
            double x;

            h=data_load(&d1,j,vi,&x);
//      Rprintf("\n3sour: %ld, dest: %ld, h: %d, x:%f",j,j2,h,x);           

            if (h<0) return(-1);
            data_save(&d2,j2,v2[i],x);
            }
        return(1);
        }



static int match_copy2()
        {
        int i;
        char vert[LLENGTH];
// RS REM        int vertpit;
// RS REM        int nummatch;
        double x2;
        int odd_toisto;
        char y[LLENGTH], *osa[2];
        char xx[LLENGTH], *sx[N_MATCH];
        int k;

        i=spfind("MATCH");
        strcpy(xx,spb[i]);
        n_match=split(xx,sx,N_MATCH);

        for (k=0; k<n_match; ++k)
            {
            m_var[k]=varfind2(&d1,sx[k],0);

            if (m_var[k]<0)
                {
                sprintf(sbuf,"\nMATCH field %s not found in %s",sx[k],word[2]);
                sur_print(sbuf); WAIT; return(-1);
                }
            strncpy(m_name[k],d1.varname[m_var[k]],8); m_name[k][8]=EOS;
            }

        odd_var=-1; odd_mode=1;

        i=spfind("MODE");
        if (i>=0)
            {
            odd_mode=atoi(spb[i]);
            if (odd_mode!=1) odd_var=32766;
            }
        else
            {
            i=spfind("ODD");
            if (i>=0)
                {
                strcpy(y,spb[i]); i=split(y,osa,2);
                if (muste_strcmpi(osa[0],"NUL")!=0)
                    {
                    odd_var=varfind2(&d1,osa[0],0);
                    if (odd_var<0)
                        {
                        sprintf(sbuf,"\nField %s in %s for odd cases not found!",
                                           osa[0],word[2]);
                        sur_print(sbuf); WAIT; return(-1);
                        }
                    }
                else odd_var=32766;
                if (i>1) odd_mode=atoi(osa[1]);
                }
            }

        if (odd_mode!=3)
            {
            i=conditions(&d1); if (i<0) return(-1);
            }

        i=data_open2(word3,&d2,0,1,0); if (i<0) return(-1);
                       /* 1.12.1999 */
        if (d2.type!=2)
            {
            sprintf(sbuf,"\nDestination %s must be a data file!",word[3]);
            sur_print(sbuf); WAIT; return(-1);
            }
        for (k=0; k<n_match; ++k)
            {
            m_var2[k]=varfind2(&d2,m_name[k],0);
            if (m_var2[k]<0)
                {
                sprintf(sbuf,"\nMATCH field %s not in file %s",m_name[k],word[3]);
                sur_print(sbuf); WAIT; return(-1);
                }

            if (d2.vartype[m_var2[k]][0]=='S') num_match[k]=0; else num_match[k]=1;
            }

        if (odd_mode==3)
            {
            i=conditions(&d2); if (i<0) return(-1);
            }

        i=varaa_tilat(); if (i<0) return(-1);
        i=tutki_muuttujat(); if (i<0) return(-1);

        sprintf(sbuf,"\n%d active fields to be copied",d1.m_act); sur_print(sbuf);
        sprintf(sbuf,"\nCopying records from %s to %s:",word[2],word[3]); sur_print(sbuf);

        if (odd_var>=0 && odd_var<32766)
            {
            i=data_to_write(word2,&d1); if (i<0) return(-1);  /* 21.11.91 */
            }

        if (odd_mode==3)
            {
            for (j=d2.l1; j<=d2.l2; ++j)
                {
                if (unsuitable(&d2,j)) continue;
                j2=0L;
                for (k=0; k<n_match; ++k)
                  {
                  if (num_match[k])
                      data_load(&d2,j,m_var2[k],&mx1[k]);
                  else
                      {
                      data_alpha_load(&d2,j,m_var2[k],m_jakso[k]);
                      i=strlen(m_jakso[k]); while (i && m_jakso[k][i-1]==' ') m_jakso[k][--i]=EOS;
                      }
                  }

                while (1)
                    {
                    ++j2;
                    if (j2>d1.n)
                        {
                        if (prind) { sprintf(sbuf,"\nODD:%s ",jakso); sur_print(sbuf); }
                        if (odd_var<32766)
                            data_save(&d1,j,odd_var,1.0);
                        break;
                        }
                for (k=0; k<n_match; ++k)
                      {
                      if (num_match[k])
                          {
                          data_load(&d1,j2,m_var[k],&x2);
                          if (mx1[k]!=x2) break;
                          }
                      else
                          {
                          data_alpha_load(&d1,j2,m_var[k],vert);
                          i=strlen(vert); while (i && vert[i-1]==' ') vert[--i]=EOS;
                          if (strcmp(m_jakso[k],vert)!=0) break;
                          }
                      } // k
                    if (k==n_match) break;
                    }

                if (j2>d1.n) continue;
                if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
                if (prind)
                    { sprintf(sbuf," %ld",j); sur_print(sbuf); }
                    
//         Rprintf("\nj2: %ld, j1: %ld",j2,j1);           
                    
                for (i=0; i<d1.m_act; ++i)
                    {
                    if (d2.type==2 && d2.d2.vartype[v2[i]][2]=='P')
                         {
                         sprintf(sbuf,"\nField %.8s is protected!",d2.varname[v2[i]]);
                         sur_print(sbuf); WAIT; return(-1);
                         }

                    kopioi(j2,j,i);
                    }
                }
            return(1);
            }

        j2=0L;
        last_found=0L;
//      if (match_var==-1) j2=d1.l1-1L;
        for (j=d1.l1; j<=d1.l2; ++j)
            {
            if (odd_mode==2) j2=0L;
            odd_toisto=0;
            if (unsuitable(&d1,j)) continue;
            for (k=0; k<n_match; ++k)
                {
                if (num_match[k])
                    data_load(&d1,j,m_var[k],&mx1[k]);
                else
                    {
                    data_alpha_load(&d1,j,m_var[k],m_jakso[k]);
                    i=strlen(m_jakso[k]); while (i && m_jakso[k][i-1]==' ') m_jakso[k][--i]=EOS;
                    }
                }
            while (1)
                {
                ++j2;
                if (j2>d2.n)
                    {
                    if (odd_var<0)
                        {
                        sprintf(sbuf,"\nRecord #%ld of %s not found in %s",
                            j,word[2],word[3]); sur_print(sbuf);
/*****************************************
                        if (match_var>=0)
                            {
                            sur_print("\nMATCH value is ");
                            if (!nummatch)
                                { sprintf(sbuf,"%s",jakso); sur_print(sbuf); }
                            else
                                {
                                double x;
                                data_load(&d1,j,match_var,&x);
                                sprintf(sbuf,"%g",x); sur_print(sbuf);
                                }
********************************************/
 sur_print("\nUse the ODD=<variable> specification to ignore these cases.");
 sur_print("\nFor such odd cases, FILE COPY will write value 1 in the ODD variable");
 sprintf(sbuf,"\nof the file %s . To omit the ODD variable, write ODD=NUL.",word[2]);
 sur_print(sbuf);
 sur_print("\nIf the common cases are not in the same order in both files,");
 sur_print("\nthe ODD specification must be given as ODD=<variable>,2 .");
 sur_print("\nIn this case the process will be much slower.");
/************************************
                            }
*************************************/
                        WAIT; sulje(); return(-1);
                        }
                    else /* ODD */
                        {
                        sprintf(sbuf,"\nODD:%s ",jakso); sur_print(sbuf);
                        if (odd_var<32766)
                            data_save(&d1,j,odd_var,1.0);
                        if (odd_mode==1) j2=last_found;
                        odd_toisto=1; break;
                        }
                    }
//              if (match_var<0) break;
                for (k=0; k<n_match; ++k)
                    {
                    if (num_match[k])
                        {
                        data_load(&d2,j2,m_var2[k],&x2);
                        if (mx1[k]!=x2) break;
                        }
                    else
                        {
                        data_alpha_load(&d2,j2,m_var2[k],vert);
                        i=strlen(vert); while (i && vert[i-1]==' ') vert[--i]=EOS;
                        if (strcmp(m_jakso[k],vert)!=0) break;
                        }
                    }
                if (k==n_match) break;

                }

            if (odd_toisto) continue;
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
            if (prind)
                { sprintf(sbuf," %ld",j); sur_print(sbuf); }

            last_found=j2;
            for (i=0; i<d1.m_act; ++i)
                {
                if (d2.type==2 && d2.d2.vartype[v2[i]][2]=='P')
                     {
                     sprintf(sbuf,"\nField %.8s is protected!",d2.varname[v2[i]]);
                     sur_print(sbuf); WAIT; return(-1);
                     }

                kopioi(j,j2,i);

                } /* i */
            } /* j */
        sulje();
        return(1);
        }


static int match_copy()
        {
        int i,h;
        char vert[LLENGTH];
// RS REM        int vertpit;
        int nummatch;
        double x1,x2;
        int odd_toisto;
        char y[LLENGTH], *osa[2];

        nummatch=0;
        i=spfind("MATCH"); if (i<0) return(-1);

        strcpy(y,spb[i]); // 30.12.2001
        h=split(y,osa,2);
        if (h==2) { match_copy2(); return(1); }

        if (spb[i][0]=='#' && spb[i][1]==EOS) /* nro:n mukaan */
            match_var=match_var2=-1;
        else
            {
            match_var=varfind2(&d1,spb[i],0);
            if (match_var<0 || strlen(spb[i])>LNAME-1)
                {
                sprintf(sbuf,"\nMATCH field %s not found in %s",spb[i],word[2]);
                sur_print(sbuf); WAIT; return(-1);
                }
            }

        if (match_var>=0)
            { strncpy(match_name,d1.varname[match_var],8); match_name[8]=EOS; }

        odd_var=-1; odd_mode=1;
        i=spfind("MODE");
        if (i>=0)
            {
            odd_mode=atoi(spb[i]);
            if (odd_mode!=1) odd_var=32766;
            }
        else
            {
            i=spfind("ODD");
            if (i>=0)
                {
                strcpy(y,spb[i]); i=split(y,osa,2);
                if (muste_strcmpi(osa[0],"NUL")!=0)
                    {
                    odd_var=varfind2(&d1,osa[0],0);
                    if (odd_var<0)
                        {
                        sprintf(sbuf,"\nField %s in %s for odd cases not found!",
                                           osa[0],word[2]);
                        sur_print(sbuf); WAIT; return(-1);
                        }
                    }
                else odd_var=32766;
                if (i>1) odd_mode=atoi(osa[1]);
                }
            }

        if (odd_mode!=3)
            {
            i=conditions(&d1); if (i<0) return(-1);
            }

        i=data_open2(word3,&d2,0,1,0); if (i<0) return(-1);
                       /* 1.12.1999 */
        if (d2.type!=2)
            {
            sprintf(sbuf,"\nDestination %s must be a data file!",word[3]);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (match_var>=0)
            {
            match_var2=varfind2(&d2,match_name,0);
            if (match_var2<0)
                {
                sprintf(sbuf,"\nMATCH field %s not in file %s",match_name,word[3]);
                sur_print(sbuf); WAIT; return(-1);
                }

            if (d2.vartype[match_var2][0]=='S') nummatch=0; else nummatch=1;
                                                /* oli == */
            }

        if (odd_mode==3)
            {
            i=conditions(&d2); if (i<0) return(-1);
            }

        i=varaa_tilat(); if (i<0) return(-1);
        i=tutki_muuttujat(); if (i<0) return(-1);

        sprintf(sbuf,"\n%d active fields to be copied",d1.m_act); sur_print(sbuf);
        sprintf(sbuf,"\nCopying records from %s to %s:",word[2],word[3]); sur_print(sbuf);

        if (odd_var>=0 && odd_var<32766)
            {
            i=data_to_write(word2,&d1); if (i<0) return(-1);  /* 21.11.91 */
            }

        if (odd_mode==3)
            {
            if (match_var==-1)
                {
                sur_print("\nMatch variable must be given in this case!");
                WAIT; return(-1);
                }

            for (j=d2.l1; j<=d2.l2; ++j)
                {
                if (unsuitable(&d2,j)) continue;
                j2=0L;
                if (nummatch)
                    data_load(&d2,j,match_var2,&x1);
                else
                    {
                    data_alpha_load(&d2,j,match_var2,jakso);
                    i=strlen(jakso); while (i && jakso[i-1]==' ') jakso[--i]=EOS;
                    }
                while (1)
                    {
                    ++j2;
                    if (j2>d1.n)
                        {
                        if (prind) { sprintf(sbuf,"\nODD:%s ",jakso); sur_print(sbuf); }
                        if (odd_var<32766)
                            data_save(&d1,j,odd_var,1.0);
                        break;
                        }

                    if (nummatch)
                        {
                        data_load(&d1,j2,match_var,&x2);
                        if (x1==x2) break;
                        }
                    else
                        {
                        data_alpha_load(&d1,j2,match_var,vert);
                        i=strlen(vert); while (i && vert[i-1]==' ') vert[--i]=EOS;
/*   printf("\njakso=%s vert=%s",jakso,vert); getch(); */
                        if (strcmp(jakso,vert)==0) break;
                        }
                    }

                if (j2>d1.n) continue;
                if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
                if (prind)
                    { sprintf(sbuf," %ld",j); sur_print(sbuf); }
                    
//         Rprintf("\nj2: %ld, j: %ld",j2,j);           
                    
                for (i=0; i<d1.m_act; ++i)
                    {
                    if (d2.type==2 && d2.d2.vartype[v2[i]][2]=='P')
                         {
                         sprintf(sbuf,"\nField %.8s is protected!",d2.varname[v2[i]]);
                         sur_print(sbuf); WAIT; sulje(); return(-1);
                         }

                    h=kopioi(j2,j,i); if (h<0) { sulje(); return(-1); }
                    }
                }
            sulje(); // RS ADD
            return(1);
            }

        j2=0L;
        last_found=0L;
        if (match_var==-1) j2=d1.l1-1L;
        for (j=d1.l1; j<=d1.l2; ++j)
            {
            if (odd_mode==2) j2=0L;
            odd_toisto=0;
            if (unsuitable(&d1,j)) continue;
            if (match_var>=0)
                {
                if (nummatch)
                    data_load(&d1,j,match_var,&x1);
                else
                    {
                    data_alpha_load(&d1,j,match_var,jakso);
                    i=strlen(jakso); while (i && jakso[i-1]==' ') jakso[--i]=EOS;
                    }
                }
            while (1)
                {
                ++j2;
                if (j2>d2.n)
                    {
                    if (odd_var<0)
                        {
                        sprintf(sbuf,"\nRecord #%ld of %s not found in %s",
                            j,word[2],word[3]); sur_print(sbuf);
                        if (match_var>=0)
                            {
                            sur_print("\nMATCH value is ");
                            if (!nummatch)
                                { sprintf(sbuf,"%s",jakso); sur_print(sbuf); }
                            else
                                {
                                double x;
                                data_load(&d1,j,match_var,&x);
                                sprintf(sbuf,"%g",x); sur_print(sbuf);
                                }
 sur_print("\nUse the ODD=<variable> specification to ignore these cases.");
 sur_print("\nFor such odd cases, FILE COPY will write value 1 in the ODD variable");
 sprintf(sbuf,"\nof the file %s . To omit the ODD variable, write ODD=NUL.",word[2]);
 sur_print(sbuf);
 sur_print("\nIf the common cases are not in the same order in both files,");
 sur_print("\nthe ODD specification must be given as ODD=<variable>,2 .");
 sur_print("\nIn this case the process will be much slower.");
                            }
                        WAIT; sulje(); return(-1);
                        }
                    else /* ODD */
                        {
                        if (prind) { sprintf(sbuf,"\nODD:%s ",jakso); sur_print(sbuf); }
                        if (odd_var<32766)
                            data_save(&d1,j,odd_var,1.0);
                        if (odd_mode==1) j2=last_found;
                        odd_toisto=1; break;
                        }
                    }
                if (match_var<0) break;
                if (nummatch)
                    {
                    data_load(&d2,j2,match_var2,&x2);
                    if (x1==x2) break;
                    }
                else
                    {
                    data_alpha_load(&d2,j2,match_var2,vert);
                    i=strlen(vert); while (i && vert[i-1]==' ') vert[--i]=EOS;
                    if (strcmp(jakso,vert)==0) break;
                    }
                }

            if (odd_toisto) continue;
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
            if (prind)
                { sprintf(sbuf," %ld",j); sur_print(sbuf); }

            last_found=j2;
            for (i=0; i<d1.m_act; ++i)
                {
                if (d2.type==2 && d2.d2.vartype[v2[i]][2]=='P')
                     {
                     sprintf(sbuf,"\nField %.8s is protected!",d2.varname[v2[i]]);
                     sur_print(sbuf); WAIT; return(-1);
                     }

                kopioi(j,j2,i);

                } /* i */
            } /* j */
        sulje();
        return(1);
        }


void muste_file_copy(int argc,char *argv[])
        {
        int i,h;
// RS REM        char *p;

// RS ADD Init variables
survo_ferror=0;
j=j2=0;
prind=0;
expand=0;
new_file=0; // FILE COPY FILE1 TO NEW FILE2  23.2.2004
match_var=match_var2=0;
odd_var=odd_mode=0;
last_found=0;
n_match=0;


        if (argc==1) return;
        s_init(argv[1]);


        if (muste_strcmpi(word[1],"EXPAND")==0) expand=1;

        if (expand)
            {
            if (g<3)
                {
                sur_print("\nUsage:");
                sur_print("\nFILE EXPAND <data_file>,m,n");
                sur_print("\nexpands a Survo <data_file> by increasing the size");
                sur_print("\nreserved for each case by m fields and n bytes.");
                WAIT; return;
                }

            expm=-9999;
            if (g>3)
                {
                if (muste_strcmpi(word[3],"MIN")==0)
                    expm=-11111;
                else
                    expm=atoi(word[3]);
                }
            expn=-9999;
            if (g>4)
                {
                if (muste_strcmpi(word[4],"MIN")==0)
                    expn=-11111;
                else
                    expn=atoi(word[4]);
                }

            strcpy(sbuf,word[2]);
            h=strlen(sbuf)-1;
            while (h>=0 && sbuf[h]!=':' && sbuf[h]!='\\') sbuf[h--]=EOS;
            strcat(sbuf,"SURVO.TMP");
// printf("\nsbuf=%s|",sbuf); getch();
            strcpy(tempn,sbuf);
            if (strchr(tempn,':')==NULL)
                { strcpy(tempn,edisk); strcat(tempn,sbuf); }
//          if (strchr(tempn+strlen(tempn)-4,'.')==NULL)
//              strcat(tempn,".SVO");
// printf("\ntempn=%s|",tempn); getch();
            word[3]=tempn;
            g=4;
            }

        if (g<4)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE COPY <source_data>,<destination_file>");
            sur_print("\nor");
            sur_print("\nFILE COPY <source_data> TO <destination_file>");
            WAIT; return;
            }
                   /* 16.11.1993 */
        if (g>4)   /* FILE COPY <source_data> TO <destination_file */
            {
            if (g<6 && muste_strcmpi(word[3],"TO")!=0)
                {
                sur_print("\nUsage:");
                sur_print("\nFILE COPY <source_data> TO <destination_file>");
                sur_print("\nor");
                sur_print("\nFILE COPY <source_data> TO NEW <destination_file>");
                WAIT; return;
                }
            word[3]=word[4];

            if (g>5)
               {
               if (strcmp(muste_strupr(word[3]),"NEW")==0) new_file=1;
               word[3]=word[5];
               }
            }

        strcpy(word2,word[2]); // 20.10.2001
        subst_survo_path(word2);
        strcpy(word3,word[3]);

        subst_survo_path(word3);
/*************************************************
        new_file=0;              // 14.2.2004
        p=strchr(word3,'/');
        if (p!=NULL)
            {
            *p=EOS;
            if (*(p+1)=='N' || *(p+1)=='n')
                {
                new_file=1;
                strcpy(sbuf,word3);
                if (strchr(sbuf,'.')==NULL) strcat(sbuf,".SVO");
                sur_delete(sbuf);
                }
            }
**********************************************/

        if (new_file) // 23.2.2004
            {
            strcpy(sbuf,word3);
            if (strchr(sbuf,'.')==NULL) strcat(sbuf,".SVO");
            sur_delete(sbuf);
            }

        i=data_open3(word2,&d1,0,1,1,0);
        if (i<0) { s_end(argv[1]); return; }

        if (expand && d1.type!=2)
            {
            sprintf(sbuf,"\n%s is not a data file!",word[2]);
            sur_print(sbuf); WAIT; sulje(); return;
            }

        i=sp_init(r1+r-1); if (i<0) return;

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        if (expand) // 29.12.2003
            {
            for (i=0; i<d1.d2.m; ++i)
                {
//              printf("\n%d %.3s",i,d1.vartype[i]); getch();
//              if (d1.d2.vartype[i][1]=='-') d1.d2.vartype[i][1]='_';
                d1.v[i]=i;
                }
            d1.m_act=d1.d2.m;
            }
        else
            {
            i=mask(&d1); if (i<0) { s_end(argv[1]); return; }
            if (d1.m_act==0)
                {
                sur_print("\nNo active variables!");
                WAIT; return;
                }
            }

        if (!expand) // 29.12.2003
            {
            i=spfind("MATCH");
            if (i>=0) { match_copy(); return; } 
            }

        if (expand) { sur_delete1(tempn); i=luo_uusi(); if (i<0) return; }
        else
            {
//          i=fi_find(word3,&d2.d2,jakso);  - 14.2.2004
            if (new_file || fi_find(word3,&d2.d2,jakso)<0)
                {
                i=luo_uusi(); if (i<0) return;
                }
            else
                {
                fclose(d2.d2.survo_data);
                i=data_open2(word3,&d2,0,1,0); if (i<0) { s_end(argv[1]); return; }
                }
            }
        if (d2.type!=2)
            {
            sprintf(sbuf,"\nDestination %s must be a data file!",word[3]);
            sur_print(sbuf); WAIT; sulje(); return;
            }

        i=conditions(&d1); if (i<0) { s_end(argv[1]); return; }
        i=varaa_tilat(); if (i<0) return;

        i=tutki_muuttujat(); if (i<0) return;
    if (!expand)
          {
        sprintf(sbuf,"\n%d active fields to be copied",d1.m_act); sur_print(sbuf);
        sprintf(sbuf,"\nCopying records from %s to %s:\n",word[2],word[3]); sur_print(sbuf);
          }
        j2=d2.n;
        for (j=d1.l1; j<=d1.l2; ++j)
            {
            if (survo_ferror) { sur_print("\nCannot save data!"); WAIT; s_end(argv[1]); return; } // RS CHA exit(0) -> s_end return
            if (!expand && unsuitable(&d1,j)) continue;
            if (sur_kbhit()) { i=sur_getch(); if (i=='.') prind=1-prind; }
            if (prind)
                {
                sprintf(sbuf,"%ld ",j); sur_print(sbuf);
                }
            ++j2;
            if (j2>d2.n) d2.n=j2;   /* fi_save vaatii j2<=d2.n */
            if (d2.m>d1.m_act) fi_miss_obs(&d2.d2,j2);
            for (i=0; i<d1.m_act; ++i)
                {
                h=kopioi(j,j2,i);
                if (h<0) return; // RS FIXME Error msg???
                }
            }
        fi_rewind(&d2.d2);
        fi_puts(&d2.d2,&j2,4,22L); /* new # of obs. */
        sulje();
        if (expand)
            {
            strcpy(tempn1,word2);
            if (strchr(tempn1,':')==NULL)
                { strcpy(tempn1,edisk); strcat(tempn1,word2); }
            if (strchr(tempn1+strlen(tempn1)-4,'.')==NULL)
                strcat(tempn1,".SVO");
            sur_delete1(tempn1);
            sur_rename(tempn,tempn1);
            }
        }


