/*  survos.c 13.9.1985/SM (21.11.1998)
 */

#include <R.h>
#include <Rinternals.h>
#include "survo.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/* SURVO.APU-parametreille */
#define MAXTILA 10000

char *z;
int ed1,ed2,edshad;
int r,r1,r2,r3,c,c1,c2,c3;
char s_edisk[LNAME], s_esysd[LNAME], s_eout[LNAME];
char *edisk, *esysd, *eout;
int etu;
char s_etufile[LNAME];
char *etufile;
int etu1,etu2,etu3;
long tutpos;
int *zs,zshn;
int erun;
int edisp; /* 1=screen redisplayed (default)  2=only current line redisplayed */
char s_sapu[MAXTILA+2];
char *sapu;
char s_info[LLENGTH];
char *info;
char **key_label; /* ?? */
char *key_lab;    /* ?? */
char ser_number[LNAME];
char **disp_string; /* ?? */
int speclist, specmax;
char s_active_data[LNAME];
char *active_data;
int scale_check;
int accuracy, results;
int ibm;
int s_shadow_int[10];
int *shadow_int;
unsigned char s_shadow_code[256];
unsigned char *shadow_code;
char s_tut_info[LLENGTH];
char *tut_info;
char s_crt_exit[32];
char *crt_exit;
int sdisp;
int scroll_line;
int space_break;
int ntut;
int move_r1,move_r2;
char s_etmpd[LNAME];
char *etmpd,*pp_etmpd;
int sur_seed;
int *psur_seed;
char *sspace;
int computation_speed;
char s_eopen[LNAME];
char *eopen;
char s_survo_path[LNAME];
char *survo_path;
int display_off;
int sur_alarm;
char s_system_name[32];
char *system_name;
int s_cur_par[2];
int *cur_par;
int shad_off;
char s_shad_active[256];
char *shad_active;
int tut_wait_c;
long cpu_speed;
int wait_save;
char survo_type;
int loudness;
int output_level;
int mat_parser;
char os_ver[10];  /* huom.  os_ver[] */
char s_info_2[LLENGTH];
char *info_2;
char info_s[64];
int spec_check; /* 1.6.2004 */

char err_str[]=
"This is an environment for statistical analysis SURVO MM created by Seppo Mustonen in 2000.";

int r_soft;
long inputBuffer;
int dwInputEvents;


char sbuf[LLENGTH];

char space[LLENGTH];
char comline[LLENGTH];
int g;
char *word[MAXPARM];
char sur_session[2];

/* Muuttuvat parametrit: */
int v_results,v_accuracy;

FILE *apu;
FILE *edfield;

char rivin_loppu[]="\15\12";


int sur_print(char *x) 
        {
        Rprintf(x);
        return(1);
        }

void edread(char *x,unsigned int lin)
        {
        strncpy(x,z+(lin-1)*ed1,(unsigned int)ed1);
        x[ed1]=EOS;
        }

int edwrite(char *x,unsigned int lin,unsigned int col)
        {
        unsigned int i,h;
        unsigned int len=strlen(x);

        if (lin<1 || lin>(unsigned int)(ed2+edshad))
            {
            sprintf(sbuf,"Line number error! (%u)\n",lin);
/*            if (dsp) return(1);    */
            sur_print(sbuf);
            WAIT; return(1);
            }
        if (len>ed1-col) len=ed1-col;
        for (i=0, h=(lin-1)*ed1+col; i<len; ++i, ++h) z[h]=x[i];
        return(1);
        }

int split(char *rivi,char **sana,int max)
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
        {
        int g=0;
        int p;
        int edell=0; /* väli edellä */
        int len=strlen(rivi);

        for (p=0; p<len; ++p)
                {
                if ( (rivi[p]==' ') || (rivi[p]==',') )
                        {
                        if (edell==1)
                                {
                                rivi[p]=EOS;
                                ++g;
                                if (g>=max) return(max);
                                edell=0;
                                }
                        }
                else
                        {
                        if (edell==0)
                                {
                                sana[g]=rivi+p;
                                edell=1;
                                }
                        }
                }
        if (edell==1) ++g;
        return(g);
        }

int sur_strcmpi(const char *s1, const char *s2)
        {
        for (; *s1 && *s2 && (toupper((unsigned char)*s1) == 
        toupper((unsigned char)*s2)); ++s1, ++s2);

        return *s1 - *s2;
        }


int filename(char *edfile, char *field)
        {
        int i;

        *edfile=EOS;
        if (strchr(field,':')==NULL)
            {
            if (*field=='.')
                {
                ++field; /* ohita piste 8.11.91 */
                strcat(edfile,survo_path);
                i=strlen(edfile)-1;
                if (edfile[i]=='\\') edfile[i]=EOS;
                }
            else strcat(edfile,edisk);
            }
        strcat(edfile,field);
        return(1);
        }

int file_name_ext(char *name,char *ext)
    {
    int i;
    char *p;

    i=strlen(name);
    if (i<4) p=name; else p=name+i-4;
    if (strchr(p,'.')==NULL)
        strcat(name,ext);
    return(1);
    }

int edit_file_not_found(char *edfile)
        {
        PR_EINV;
        sprintf(sbuf,"\nEdit file %s not found!",edfile); sur_print(sbuf);
        WAIT; return(0);
        }

int edload32_err(char *s,char *edfile)
        {
        sprintf(sbuf,"\n%s in edit file %s !",s,edfile);
        sur_print(sbuf); WAIT;
        return(1);
        }

int ed_not_space()
        {
        printf("\nNot space enough for the edit field!");
        WAIT;
        return(1);
        }

int ed_malloc(unsigned int ed1,unsigned int ed2,unsigned int edshad)
        {
        if (zs!=NULL) free((char *)zs);
        if (z!=NULL) free(z);
        z=(char *)malloc(sizeof(char)*ed1*(ed2+edshad));
        if (z==NULL) { ed_not_space(); return(-1); }
        zs=(int *)malloc((ed2+1)*sizeof(int));
        if (zs==NULL) { ed_not_space(); return(-1); }
        return(1);
        }

int shadinit()
        {
        unsigned int i,j;
/*        char *p; */

        j=0; while (j<(unsigned int)ed2) { ++j; zs[j]=0; }
        i=ed1*ed2; zshn=0;
        while ( zshn<edshad && zshn<ed2 )
            { z[i]='\0'; i+=ed1; ++zshn; }
        return(1);
        }

int creatshad(unsigned int j)
        {
        unsigned int i,k;
        char x[LLENGTH];
/*         extern int ued1,ued2,uedshad;  */

        if (j>(unsigned int)ed2) return(-1);
        i=ed1*ed2; k=0; while ( k<(unsigned int)zshn && z[i]!='\0' ) { ++k; i+=ed1; }

        zs[j]=k+ed2+1;
        strncpy(x,space,(unsigned int)ed1);
        edwrite(x,(unsigned int)zs[j],0);

        return(1);
        }

int edload32(char *edfile)
        {
        int i,j;
        char x[LLENGTH+10], *sana[3];
        char *p;
        int rivi_luettu;
        int ei_onnistunut;

        ei_onnistunut=0;
        rewind(edfield);  /*   PUUTTUVA  */
        fgets(x,LLENGTH+10-1,edfield); p=strchr(x,':');
        if (p==NULL) edit_file_not_found(edfile);

        i=split(p+1,sana,3);
        ed1=atoi(sana[0]); ed2=atoi(sana[1]); edshad=atoi(sana[2]);

        fclose(edfield);    /* suljetaan malloc-varausten tiivistämiseksi */

        i=ed_malloc((unsigned int)ed1,(unsigned int)ed2,(unsigned int)edshad);
        if (i<0)
            {
            sprintf(sbuf,"\nNot enough space for edit field %s !",edfile);
            sur_print(sbuf);
            WAIT; return(-1);
            ed1=101; ed2=100; edshad=30; ed_malloc((unsigned int)ed1,(unsigned int)ed2,(unsigned int)edshad);
            ei_onnistunut=1;
            }
        edfield=fopen(edfile,"rt");
        if (edfield==NULL) { edit_file_not_found(edfile); return(-1); }
        fgets(x,LLENGTH-1,edfield); /* otsikko uudelleen */
        c2=ed1-1; r2=ed2;
        strcpy(eopen,edfile);
        for (i=0; i<ed1*(ed2+edshad); ++i) z[i]=' ';
        for (i=0; i<ed1*ed2; i+=ed1) z[i]='*';
        shadinit();
        if (ei_onnistunut) { fclose(edfield); return(-1); }  /* ???  */
        rivi_luettu=0; j=0;
        while (1)
            {
            fgets(x,LLENGTH+10-1,edfield);
/* printf("x=%s\n",x); getch(); */
            if (feof(edfield)) break;
            p=strchr(x,'|');
            if (p==NULL) { /*  edload32_err("Missing `|'",edfile); */
                           fclose(edfield); return(-1);
                         }
            *p=EOS; ++p;
            i=strlen(p); if (p[i-1]=='\n') p[i-1]=EOS;

            if (rivi_luettu && *x=='S')
                {
                i=creatshad((unsigned int)j);
                if (i<0) break;    /* ei mahdu */
                edwrite(p,(unsigned int)zs[j],0);
                rivi_luettu=0;
                continue;
                }
            j=atoi(x);
            if (j>ed2)
                { /* edload32_err("Too many lines",edfile); */
                  fclose(edfield); return(-1);
                }
            edwrite(p,(unsigned int)j,0);
            rivi_luettu=1;
            }
        fclose(edfield);
        return(1);
        }


int edload(char *field,int shad)
        {
        int i;
        char rivi[ELE];
        char edfile[LNAME];
/*        char *sana[5]; int g;  */
/*        char edtpaate[5]=".EDT";


        filename(edfile,field); file_name_ext(edfile,edtpaate);  */ /* ,".EDT" */
        strcpy(edfile,field);
        edfield=fopen(edfile,"rb");
        if (edfield==NULL) { edit_file_not_found(edfile); return(-1); }
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(edfield);
        rivi[ELE-1]=EOS;
        if (strncmp(rivi,"SURVO 98",8)==0)
            {
            i=edload32(edfile);
            }
        return(i);
        }


int edsave32(char *edfile,int shad)
        {
        int i,j,zsj;
        char x[LLENGTH];
        int form;
        int tyhja;
/*        int d1,d2,d3; */

        fprintf(edfield,"SURVO 98 edit field: %d %d %d (32 bit version)%s",
                         ed1,ed2,edshad,rivin_loppu);
        form=3;
        if (ed2>=1000) form=4;
        if (ed2>=10000) form=5;
        if (ed2>=100000) form=6;

        for (j=1; j<=ed2; ++j)
            {
            edread(x,(unsigned int)j); i=ed1-1;

            while (i>0 && x[i]==' ') --i;
            x[i+1]=EOS;
            if (i==0 && *x=='*') tyhja=1; else tyhja=0;
            zsj=zs[j];
            if (!tyhja || zsj)
                {
                fprintf(edfield,"%.*d|%s%s",form,j,x,rivin_loppu);
                }
            if (zsj)
                {
                edread(x,(unsigned int)zsj); i=ed1-1;
                while (i>0 && x[i]==' ') --i;
                x[i+1]=EOS;
                fprintf(edfield,"%-*s|%s%s",form,"S",x,rivin_loppu);
                }
            }
        fclose(edfield);
        return(1);
        }



int edsave(char *field,int shad,int check)
        {
/*
        unsigned int i,k;
        char header[LLENGTH];
        char number[10];
        char x[LLENGTH];
        unsigned int j;
        short *pint;
        char ch;
*/
        char edfile[LNAME];
/*        char edtpaate[5]=".EDT";

    filename(edfile,field); file_name_ext(edfile,edtpaate); */

strcpy(edfile,field);
    if (check)
        {
        edfield=fopen(edfile,"rb");
        if (edfield!=NULL)
                {
/* PUUTTUVA strcmpi korvattu sur_strcmpi:llä
                if (sur_strcmpi(edfile,eopen)!=0 && etu!=2)  
                    {
                    PR_EBLD;
                    sprintf(sbuf,"\nEdit file %s already exists! Overwrite it (Y/N)?",
                                                                       edfile);
                                sur_print(sbuf);
                    ch=(char)getck(); sprintf(sbuf,"%c",ch); sur_print(sbuf);
                    if (ch!='Y' && ch!='y') { fclose(edfield); return(0); }
                    }
*/
                fclose(edfield);
                }
        }

        edfield=fopen(edfile,"wb");
        if (edfield==NULL)
            {
            sprintf(sbuf,"\nCannot save %s !",edfile);
            sur_print(sbuf); WAIT;
            return(0);
            }

        edsave32(edfile,shad); return(1);
        }

void edt_talletus(char *s)
        {
        char snimi[LLENGTH];

        strcpy(snimi,s);
/*       if (strchr(s,':')==NULL && strchr(s,'\\')==NULL)   */ /* 7.1.1992 */
/*            { strcpy(snimi,etmpd); strcat(snimi,s); }
*/
        edsave(snimi,1,0);
        }

int xxd(int i)
        {
        fprintf(apu,"%d\n",i);
        return(1);
        }

int xxl(long li)
        {
        fprintf(apu,"%ld\n",li);
        return(1);
        }

int xxs(char *x)
        {
        fprintf(apu,"%s\n",x);
        return(1);
        }

int xxe(double a)
        {
        fprintf(apu,"%.16e\n",a);
        return(1);
        }

int yys(char *x)
        {
        char *p;

        p=x;
        while (1)
            {
            *p=(char)getc(apu);
            if (*p=='\n') break;
            ++p;
            }
        *p=EOS;

        return(1);
        }

int yyl(long *pli)
        {
        char x[LLENGTH];
        yys(x);
        *pli=atol(x);
        return(1);
        }

int yyu(unsigned int *pi)
        {
        char x[LLENGTH];
        yys(x);
        *pi=atoi(x);
        return(1);
        }

int yye(double *pa)
        {
        char x[LLENGTH];
        yys(x);
        *pa=atof(x);
        return(1);
        }

int yyd(int *pi)
        {
        char x[LLENGTH];
        yys(x);
        *pi=atoi(x);
        return(1);
        }

int sur_dump(char *siirtop)
        {
        int i,h;
        char x[LNAME];

        results=v_results;
        accuracy=v_accuracy;

        strcpy(x,siirtop); strcat(x,"SURVOMM.EDT");
        edt_talletus(x);


        strcpy(x,siirtop); strcat(x,"SURVOMM.DMP");
        apu=fopen(x,"wt");

        xxd(ed1); xxd(ed2);
        xxd(r); xxd(r1); xxd(r2); xxd(r3); xxd(r_soft);
        xxd(c); xxd(c1); xxd(c2); xxd(c3);
        xxs(edisk); xxs(esysd); xxs(eout);
        xxd(etu); xxs(etufile);
        xxd(etu1); xxd(etu2); xxd(etu3);
        xxl(tutpos);
        xxd(zshn);
        xxd(erun); xxd(edshad);
/* int eddisp ?? */
        xxs(info);
/* key_label key_lab survo_id  ??? */
        xxd(speclist); xxd(specmax);
        xxs(active_data); xxd(scale_check); xxd(accuracy);
        xxd(results);
/* ibm poistetaan? */
        for (i=0; i<10; ++i) xxd(shadow_int[i]);
        for (i=0; i<256; ++i)
            {
            h=shadow_code[i]; xxd(h);
/*          h=shad_active[i]; xxd(h); */
            }

        xxs(tut_info); xxs(crt_exit);
        xxd(sdisp); xxd(scroll_line);
        xxd(space_break);
        xxd(ntut);
        xxd(move_r1); xxd(move_r2);
        xxs(etmpd); xxd(sur_seed);
/* space generoidaan lapsessa */
        xxd(computation_speed);
        xxs(eopen); xxs(survo_path);
        xxd(display_off); xxd(sur_alarm);
        xxs(system_name);
        xxd(cur_par[0]); xxd(cur_par[1]);
        xxd(shad_off);
        xxd(tut_wait_c); xxl(cpu_speed);
        xxd(wait_save);
        *sbuf=survo_type; sbuf[1]=EOS; xxs(sbuf);
        xxd(loudness);
        xxd(output_level); xxd(mat_parser);
        xxs(os_ver);
        xxs(info_2);
        xxd(spec_check);
        fprintf(apu,"%s",sapu);
        fclose(apu);
        return(1);
        }


int restore_dump(char *siirtop)
        {
        char x[LNAME];
        int i,h;
        char *p;
/*        long li;
        int k;  */

        strcpy(x,siirtop); strcat(x,"SURVOMM.DMP");
        apu=fopen(x,"rt");
        if (apu==NULL) return(0);
        yyd(&ed1); yyd(&ed2);
        yyd(&r); yyd(&r1); yyd(&r2); yyd(&r3); yyd(&r_soft);
        yyd(&c); yyd(&c1); yyd(&c2); yyd(&c3);
        yys(edisk); yys(esysd); yys(eout);
        yyd(&etu); yys(etufile);
        yyd(&etu1); yyd(&etu2); yyd(&etu3);
        yyl(&tutpos);
        yyd(&zshn);
        yyd(&erun); yyd(&edshad);
        yys(info);
/* key_label key_lab survo_id  ??? */
        yyd(&speclist); yyd(&specmax);
        yys(active_data); yyd(&scale_check); yyd(&accuracy);
        yyd(&results);
/* ibm poistetaan? */
        for (i=0; i<10; ++i) yyd(&shadow_int[i]);
        for (i=0; i<256; ++i)
            {
            yyd(&h); shadow_code[i]=h;
            }
/*      yys(shadow_code);  po. jokainen merkki erikseen, jos tarv.? */
        yys(tut_info); yys(crt_exit);
        yyd(&sdisp); yyd(&scroll_line);
        yyd(&space_break);
        yyd(&ntut);
        yyd(&move_r1); yyd(&move_r2);
        yys(etmpd); yyd(&sur_seed);
/* space generoidaan lapsessa */
         yyd(&computation_speed);
        yys(eopen); yys(survo_path);
        yyd(&display_off);
        yyd(&sur_alarm);
        yys(system_name);
        yyd(&cur_par[0]); yyd(&cur_par[1]);
        yyd(&shad_off);

        yyd(&tut_wait_c); yyl(&cpu_speed);
        yyd(&wait_save);
        yys(sbuf); survo_type=*sbuf;
        yyd(&loudness);
        yyd(&output_level); yyd(&mat_parser);
        yys(os_ver);
        yys(info_2);
        yyd(&spec_check);

        p=sapu;
        while (1)
            {
            *p=(char)getc(apu);
            if (feof(apu)) break;
            ++p;
            }
        *p=EOS;
        fclose(apu);

        strcpy(x,siirtop); strcat(x,"SURVOMM.EDT");
        edload(x,1);




/*      disp();
      g=2; parm[1]=edisk; op_cd(); */

        v_results=results;
        v_accuracy=accuracy;

        return(1);
        }

int s_edt(char *siirtop)
        {
        restore_dump(siirtop);
        return(1);
        }

int s_end(char *siirtop)
        {
        sur_dump(siirtop);
        return(1);
        }


int s_init(char *siirtop)
        {
        int i;
        char *p;

        strcpy(sur_session,siirtop);
        *sur_session=siirtop[strlen(siirtop)-1]; sur_session[1]=EOS;
        edisk=s_edisk;  esysd=s_esysd;  eout=s_eout;
        etufile=s_etufile;
        sapu=s_sapu; info=s_info;
        active_data=s_active_data; shadow_int=s_shadow_int;
        shadow_code=s_shadow_code;
        tut_info=s_tut_info; crt_exit=s_crt_exit;
        etmpd=pp_etmpd=s_etmpd;
        psur_seed=&sur_seed;
        sspace=space;
        eopen=s_eopen;
        survo_path=s_survo_path; system_name=s_system_name;
        cur_par=s_cur_par; shad_active=s_shad_active;
        info_2=s_info_2;

        strcpy(etmpd,siirtop); /* tilap. */

        s_edt(siirtop);

        for (i=0; i<LLENGTH; ++i) space[i]=' '; space[LLENGTH-1]=EOS;
        edread(comline,(unsigned int)(r1+r-1));
        p=strchr(comline,PREFIX); if (p==NULL) p=comline;
        g=split(p+1,word,MAXPARM);
        i=0;

        while (i<g && strcmp(word[i],"/")!=0) ++i;
        g=i;

/*        if (console) sur_console_child_init(); */
        return(1);
        }

/*
int main(int argc, char *argv[])
{
s_init(argv[1]);
s_end(argv[1]);
return(0);
}

*/
