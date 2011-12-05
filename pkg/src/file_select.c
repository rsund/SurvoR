/* select.c 12.11.1999/SM (12.11.1999) (25.7.2001)
   FILE SELECT
*/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define N_SEL 20
#define L_STR 100


static SURVO_DATA d_uusi,d_vanha,d_sel;
static char uusinimi[LNAME];
static int v_var[N_SEL]; /* muuttujien nrot. d_vanha */
static char type[N_SEL]; /* muuttujien tyypit */
static int s_var[N_SEL]; /* muuttujien nrot. d_sel */
static int n_sel;
static int i_sel[N_SEL];
static int ts_len;
static int s_len[N_SEL];
static char *s_tila;
static char *v_tila;

static int mark_var;
static int nmatch_var;

static int prind=1;

static int neg=0; // KEYS!=... -> neg=1 eli kÑÑnteinen valinta) 25.1.2003

static FILE *temp;

/* RS REM
static char *specs0[]={ "VARS", "MASK", "IND", "CASES", "SELECT",
                 "KEYS", "KEYS!", "MARK", "NMATCH",
                 "PRIND", "!" };
static char **specs=specs0;
*/


static void ei_tilaa()
        {
        sur_print("\nNot enough space!");
        WAIT;
        }


static void sel_virhe2(char *s,char *x)
        {
        sprintf(sbuf,"\nField %s not found in data %s!",s,x);
        WAIT; return;
        }

static int luo_uusi(char *uusi,char *uusinimi)
        {
        int i;
        long alku,j;

        strcpy(uusinimi,uusi);
        if ((strchr(uusi,':')==NULL) && *uusi!='<' && *uusi!='.'
        && *uusi!='~' && *uusi!='\\' && *uusi!='/')        // RS FIXME path CHA unixpath         
            { strcpy(uusinimi,edisk); strcat(uusinimi,uusi); }
        muste_append_path(uusinimi,".SVO"); // RS CHA if (strchr(uusinimi+strlen(uusinimi)-4,'.')==NULL) strcat(uusinimi,".SVO");

        temp=muste_fopen(uusinimi,"rb");
        if (temp!=NULL)
            {
            muste_fclose(temp);
            if (etu==0)
                {
                sprintf(sbuf,"\nData file %s already exists!",uusinimi);
                sur_print(sbuf);
                sur_print("\nIf continued, its present contents will be destroyed.");
                sur_print("\nContinue (Y/N) ");
                i=sur_getch();
                if (i!='Y' && i!='y')
                    {
                    sur_print("N");
                    sur_print("\nFILE SELECT interrupted!");
                    WAIT; return(-1);
                    }
                }
            }
        d_uusi.d2.survo_data=muste_fopen(uusinimi,"wb");

        if (d_uusi.d2.survo_data==NULL)
            {
            sprintf(sbuf,"\nCannot save file %s!",uusinimi); sur_print(sbuf);
            WAIT; return(-1);
            }

        fi_rewind(&(d_vanha.d2));
        alku=(long)(d_vanha.d2.data);
        for (j=0; j<alku; ++j)
            {
            putc(getc(d_vanha.d2.survo_data),d_uusi.d2.survo_data);
            if (ferror(d_uusi.d2.survo_data)) { ei_tilaa(uusinimi); return(-1); }
            }

        fi_rewind(&(d_vanha.d2));

        muste_fclose(d_uusi.d2.survo_data);
        return(1);
        }


void muste_file_select(int argc,char *argv[])
        {
        int i,k,writefile;
        char x[LLENGTH],*osa[N_SEL];
        int l,l2,nn,n2; // RS CHA long
        double a,b;
        int len;
        char *p,*q;

// RS ADD variable init
n_sel=0;
ts_len=0;
mark_var=0;
nmatch_var=0;
prind=1;
neg=0;
s_tila=NULL;
v_tila=NULL;
temp=NULL;
writefile=TRUE;


        if (argc==1) return;
        s_init(argv[1]);

        if (g<6)
            {
            sur_print("\nUsage:");
            sur_print("\nFILE SELECT <new_data> FROM <old_data> BY <select_file>");
            sur_print("\n            KEYS=<var1>,<var2>,...                     ");
            WAIT; return;
            }
/* FILE SELECT <new_data> FROM <old_data> BY <select_file>");
                2         3     4         5   6
   KEYS=<var1>,<var2>,...

*/

        i=data_open(word[4],&d_vanha); if (i<0) { s_end(argv[1]); return; }
        i=spec_init(r1+r-1); if (i<0) return;
        i=conditions(&d_vanha); if (i<0) return;

        i=data_open(word[6],&d_sel); if (i<0) { s_end(argv[1]); return; }

        i=spfind("KEYS");
        if (i<0)
            {
            i=spfind("KEYS!"); // 25.1.2003
            if (i<0)
                {
                sur_print("\nKEYS=<var1>,<var2>,... missing!");
                WAIT; return;
                }
            else neg=1;
            }

        strcpy(x,spb[i]);
        n_sel=split(x,osa,N_SEL);
        for (k=0; k<n_sel; ++k)
            {
            i=varfind2(&d_vanha,osa[k],0);
            if (i<0) { sel_virhe2(osa[k],word[4]); return; }
            v_var[k]=i; type[k]=d_vanha.vartype[i][0];
            i=varfind2(&d_sel,osa[k],0);
            if (i<0) { sel_virhe2(osa[k],word[6]); return; }
            s_var[k]=i;

            if ( type[k]!=d_sel.vartype[i][0] ||
                d_vanha.varlen[v_var[k]]!=d_sel.varlen[i]     )
                {
                sprintf(sbuf,"\nType/length of field %s not same in %s and %s!",
                          osa[k],word[4],word[6]);
                sur_print(sbuf); WAIT; return;
                }

            }

        mark_var=-1;
        i=spfind("MARK");
        if (i>=0)
            {
            mark_var=varfind2(&d_vanha,spb[i],0);

            if (mark_var<0)
                {
                sprintf(sbuf,"MARK variable %s not found in data file %s!",
                                spb[i],word[4]);
                sur_print(sbuf); WAIT; return;
                }
            }

        nmatch_var=-1;
        i=spfind("NMATCH");
        if (i>=0)
            {
            nmatch_var=varfind2(&d_sel,spb[i],0);
            if (nmatch_var<0)
                {
                sprintf(sbuf,"NMATCH variable %s not found in data file %s!",
                                spb[i],word[6]);
                sur_print(sbuf); WAIT; return;
                }

// nollaus kÑyttÑjÑn vastuulle!
//          for (l=d_sel.l1; l<=d_sel.l2; ++l)
//              data_save(&d_sel,l,nmatch_var,0.0);
            }

/**************************************************************
for (k=0; k<n_sel; ++k)
    {
    sprintf(sbuf,"\n%d %d %c %d",k+1,v_var[k],type[k],s_var[k]);
    sur_print(sbuf);
    }
WAIT;
***************************************************************/

        n2=d_sel.n;
        ts_len=0;
        for (k=0; k<n_sel; ++k)
            {
            i_sel[k]=ts_len;
            if (type[k]=='S') s_len[k]=d_sel.varlen[s_var[k]];
            else s_len[k]=sizeof(double);
            ts_len+=s_len[k];
            }
        s_tila=muste_malloc(n2*ts_len);
        if (s_tila==NULL) { ei_tilaa(); return; }
        p=s_tila;
        v_tila=muste_malloc(ts_len);
        if (v_tila==NULL) { ei_tilaa(); return; }

        for (l2=0; l2<n2; ++l2) // RS CHA l2=0L;
            {
            for (k=0; k<n_sel; ++k)
                {
                if (type[k]=='S')
                    {
                    data_alpha_load(&d_sel,l2+1,s_var[k],x);
                    strncpy(p,x,s_len[k]); p+=s_len[k];
                    }
                else
                    {
                    data_load(&d_sel,l2+1,s_var[k],&b);
                    q=(char *)&b;
                    for (i=0; i<sizeof(double); ++i)
                        *p++=*q++;
                    }
                }
            }
        i=spfind("PRIND");
        if (i>=0) prind=atoi(spb[i]);

        i=luo_uusi(word[2],uusinimi); if (i<0) return;
        data_open(word[2],&d_uusi);
        muste_fseek(d_uusi.d2.survo_data,d_uusi.d2.data,SEEK_SET);
        len=d_vanha.d2.len;
//      n2=d_sel.n;
        nn=0; // RS CHA nn=0L;

        sur_print("\nSelecting observations: ");

        for (l=d_vanha.l1; l<=d_vanha.l2; ++l)
            {
            if (unsuitable(&d_vanha,l)) continue;
            if (prind) { sprintf(sbuf," %d",l); sur_print(sbuf); } // RS ld->d
            if (sur_kbhit())
                {
                i=sur_getch();
                if (i=='.') prind=1-prind;
                }
            p=v_tila;
            for (k=0; k<n_sel; ++k)
                {
                if (type[k]=='S')
                    {
                    data_alpha_load(&d_vanha,l,v_var[k],x);
                    strncpy(p,x,s_len[k]); p+=s_len[k];
                    }
                else
                    {
                    data_load(&d_vanha,l,v_var[k],&b);
                    q=(char *)&b;
                    for (i=0; i<sizeof(double); ++i)
                        *p++=*q++;
                    }
                }

            for (l2=1; l2<=n2; ++l2) // RS CHA l2=1L
                {
                for (k=0; k<n_sel; ++k)
                    {
                    if (type[k]=='S')
                        {
                  //    data_alpha_load(&d_vanha,l,v_var[k],x);
                  //    data_alpha_load(&d_sel,l2,s_var[k],sbuf);
// if (strncmp(x,s_tila+(l2-1)*ts_len+i_sel[k],s_len[k])!=0) break;
   if (strncmp(v_tila+i_sel[k],s_tila+(l2-1)*ts_len+i_sel[k],s_len[k])!=0) break;
                  //    if (strcmp(x,sbuf)!=0) break;
                        }
                    else
                        {
                  //    data_load(&d_vanha,l,v_var[k],&a);
                  //    data_load(&d_sel,l2,s_var[k],&b);
// if (a!=*(double *)(s_tila+(l2-1)*ts_len+i_sel[k])) break;
   if (*(double *)(v_tila+i_sel[k])!=*(double *)(s_tila+(l2-1)*ts_len+i_sel[k])) break;
                        }
                    } /* k */
                if (k==n_sel) break;
                } /* l2 */

            if (neg) // 25.1.2003
                {
                if (l2<=n2) continue;
                }
            else if (l2>n2) continue;

   muste_fseek(d_vanha.d2.survo_data,d_vanha.d2.data+(l-1)*len,SEEK_SET);
   for (i=0; i<len; ++i)
       putc(getc(d_vanha.d2.survo_data),d_uusi.d2.survo_data);
   ++nn;

            if (mark_var>=0)
                data_save(&d_vanha,l,mark_var,1.0);
            if (nmatch_var>=0)
                {
                data_load(&d_sel,l2,nmatch_var,&a);
                data_save(&d_sel,l2,nmatch_var,a+1.0);
                }
                
            } /* l */

        fi_rewind(&(d_uusi.d2));
        fi_puts(&(d_uusi.d2),&nn,sizeof(int),22); // RS CHA 64-BIT sizeof(long)  22L -> 22

// RS ADD
		data_close(&d_sel);
		data_close(&d_vanha);
		data_close(&d_uusi);
        }
