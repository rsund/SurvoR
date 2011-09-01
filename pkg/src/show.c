/*  _show.c 23.12.1985/SM (22.3.1993) (4.2.1996) (4.3.1996)
    SHOW          displays the output file
    SHOW *        displays the current edit field
    SHOW <file>   displays edit file/ text file
    SHOW <file>,L displays edit file/ text file from line L onwards
*/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define MAXL 40000
#define TABSPACE 8
#define EDITLEV 72
#define CR '\15'
#define TAB '\t'

static char tfile[LNAME];
static int rdisp; /* 1. n„ytt”rivi */
static int ndisp; /* n„ytt”rivien maxluku */
static int mdisp; /* n„ytt”rivien t„m„nhetkinen luku */
static long j, jmax;  /* j=nykyisen rivin nro jmax=viimeisen rivin nro */
static int edit,ted1,ted2; /* edit=1: kentt„, jonka dimensiot ted1,ted2 */
static int tedshad, /* varjorivien max lkm */ *shad_int; /* varjorivit */
static int n_shad;
static int jj;             /* tulostusrivi */

static int cdisp=0; /* 1. n„ytt”sarake */

static char line[TABSPACE*MAXL];
static int testi=0;
static char cfile[LNAME];
static char siirtop[32];

static FILE *text;
static FILE *alut;
static long jjmax;
static char survoxxx[LNAME];

static long last_line32;
static long *alut32=NULL;
static int s98_file=0;

// static char load_lines[LLENGTH];


static char ttab[TABSPACE*MAXL];
static FILE *copyfile;

static int empty32;
static int disp_frame=0;

static char win_aeao[]="ÄÖäö";  // 26.3.2003
static char code[512]; // RS CHA unsigned
static int win_conv=0;
static int text_found; // 20.7.2006

static FILE *codes;


static int tyhjenna_ikkuna();
static int text_show(long rivi1);
static void nayta_cdisp();
static int laji();
static int edit32_alut();
static int avaa_alut();
static void poista_alut();
static int talleta_alku(long j,long os);
static long rivit(long j);
static int direct_showload(char *line);
static int etsi_rivi(char *s,int j0);
static int number_of_text_lines();
static int number_of_edit84_lines();
static void luo_ikkuna();
static int disp_show(long jseur);
static int etsi(long rivi);
static int lue_rivi(char *s);
static int w_codes_load(int k);
static int tutki(char *s);
static int load(char *s);
static int copy_file(char *s,char *nimi);
static int empty_s(char *s);
static void ei_tilaa();
static int lis_rivit(int jj,long kpl);
static int putsaa();
static long search(char *s,long jseur);
static void tab_poisto(char *s);
static int insert_lines(int jj,int k);
static int load_codes(char *codefile,char *code,int col);
static int init_shadows();
static int shadow_write(unsigned int j,unsigned int jj,int disp_frame);
static int get_editline_from_file(char *tfile,char *label,int line1);
static int lue_editrivi(char *s,int edit);

extern int sur_play_sound();


void muste_show(int argc,char *argv[])
        {
        int i;
        int space_break0;
        long li;

		rdisp=0;
		ndisp=0;
		mdisp=0;
		j=jmax=0;
		edit=ted1=ted2=0;
		tedshad=0;
		shad_int=NULL;
		n_shad=0;
		jj=0;
		cdisp=0;
		testi=0;
		text=NULL;
		alut=NULL;
		jjmax=0;
		last_line32=0;
		alut32=NULL;
		s98_file=0;
		copyfile=NULL;
		empty32=0;
		disp_frame=0;
		win_conv=0;
		text_found=0;
		codes=NULL;


        if (argc==1) return;
        s_init(argv[1]);
        labels();

        tut_init();
        if (r_soft) r3+=r_soft+1;
        strcpy(siirtop,argv[1]);
        space_break0=space_break; space_break=0;
        jmax=1000000L;
        if (hae_apu("max_show_lines",sbuf)) jmax=atol(sbuf); // 22.1.2007

        i=avaa_alut(); if (i<0) return;
        talleta_alku(1L,0L);

        i=spec_find("WINCONV",sbuf,LLENGTH);
        if (i>=0 && atoi(sbuf)>0)
            { win_conv=1; w_codes_load(2); }

        jj=r1+r;
        if (g==1)
            {
            strcpy(tfile,eout);
            if (*tfile==EOS)  // 28.10.2002
                {
                sur_print("\nNo output file in use!");
                WAIT;
                return;
                }
            text_show(1L);
            }
        else
            {
            strcpy(tfile,word[1]);
            subst_survo_path(tfile);
            li=1L; if (g>2) li=atol(word[2]);
            if (li<1) li=1L;

            if (g>2 && !isdigit((int)*word[2]) && *word[2]!='"')
                {
                li=get_editline_from_file(tfile,word[2],1);
                if (li<0) return; // RS ADD
                if (li==0)
                    {
                    sprintf(sbuf,"\n%s not found!",word[2]);
                    sur_print(sbuf); WAIT;
                    }
                }

            i=text_show(li);
            if (i>0 && edit>=0 && edit!=2) muste_fclose(text);
            }
        poista_alut();
        space_break=space_break0;
        tut_end();
        if (r_soft) r3-=r_soft+1;
        s_end(argv[1]);
        }

static int tyhjenna_ikkuna()
    {
    int i;

    for (i=r+2; i<r3+2; ++i)
        write_string(space,c3+8,'\237',i,1);
    return(1);
    }

static int text_show(long rivi1)
        {
        int i;
        long jseur;
        int m;
        char llines[16], sline[51];
        char luku[8];
        long li;
        char y1[LLENGTH],y2[LLENGTH];
        char ch;
        int len;
        long jd;
        char *p;

        *sline=EOS;
        edit=laji(); if (edit<0) return(-1);
        if (edit==1 || edit==2) jmax=ted2;

        putsaa();
        j=1l; jseur=rivi1;

        i=spec_find("SHOWLOAD",sbuf,LLENGTH);
        if (i>=0)
            { direct_showload(sbuf); return(1); }

        luo_ikkuna();
        tyhjenna_ikkuna();
        putsaa();
        disp_show(jseur);

        if (g>2 && *word[2]=='"') // 19.7.2006
            {
            strcpy(sline,word[2]+1);
            p=strchr(sline,'"');
            if (p!=NULL)
                {
                *p=EOS;
                while ((p=strchr(sline,'_'))!=NULL) *p=' ';

                li=search(sline,jseur);
                if (li<=0L) li=1;
                jseur=li;
                disp_show(jseur);
                }
            }
        while (1)
            {
if (edit!=0)
m=nextch("SHOW: ENTER=Exit N=Next P=Prev E=End L=Load S=Search C=Copy");
else
m=nextch("SHOW: ENTER=Exit N=Next P=Prev E=End L=Load S=Search C=Copy D,d=Edit W=Win_char");
            switch (m)
                {
              case CODE_EXIT:
              case CODE_RETURN: return(1);
              case CODE_NEXT:
              case 'N': case 'n':
jseur+=(long)ndisp; if (jseur+(long)(ndisp-1)>jmax) jseur=jmax-(long)ndisp+1L;
                if (jseur<1L) jseur=1L;
                disp_show(jseur);
                break;

              case CODE_PREV:
              case 'P': case 'p':
                jseur-=(long)ndisp; if (jseur<1L) jseur=1L;
                          else if (jseur>jmax-(long)ndisp) jseur=jmax-(long)ndisp+1L;
                disp_show(jseur);
                break;

              case CODE_HOME:
                if (cdisp) { cdisp-=c3+1; if (cdisp<0) cdisp=0; nayta_cdisp(); }
                else jseur=1L;
                disp_show(jseur);
                break;

              case CODE_DOWN:
                if (jseur+(long)mdisp>jmax) break;
                j=jseur+(long)mdisp;
                i=etsi(j); if (i<0) { muste_fclose(text); return(-1); }
                                                /* 4.3.1996 */
                i=lue_rivi(line);
                if (edit!=2 && feof(text) && !*line) { jmax=j; break; }
                if (i<0) return(-1);
                SCROLL_UP(rdisp,r3,1);
                PR_EUDL;

//              i=sprintf(sbuf,"%6ld %s",j,line+cdisp);
//                 i=sprintf(sbuf,"%6ld ",j);
                i=sprintf(sbuf,"%6d ",(int)j);                
                strncat(sbuf,line+cdisp,c3+1);
                i=strlen(sbuf);
                if (i>c3+8)
//                    write_string(sbuf,c3+8,shadow_code[sdisp],rdisp+mdisp,1);
				  write_string(sbuf,c3+8,'4',rdisp+mdisp,1);
                else
                    {
//                    write_string(sbuf,i,shadow_code[sdisp],rdisp+mdisp,1);
                    write_string(sbuf,i,'4',rdisp+mdisp,1);
//                    write_string(space,c3+8-i,shadow_code[sdisp],rdisp+mdisp,1+i);
					write_string(space,c3+8-i,'4',rdisp+mdisp,1+i);
                    }

/*****************************************************
                PR_EUDL; LOCATE(rdisp+mdisp,1);
                strncpy(sbuf,space,c3+8);
                i=sprintf(sbuf,"%6ld ",j);

                if (strlen(line)<cdisp)
                    sprintf(sbuf+i,"%.*s",c3+2,space);
                else
                    sprintf(sbuf+i,"%.*s",c3+2,line+cdisp);

                write_string(sbuf,c3+8,shadow_code[sdisp],rdisp+mdisp,1);
*******************************************************/

                ++j; ++jseur;
                if (edit!=3 && edit!=2) talleta_alku((long)j,ftell(text));
                break;

              case CODE_UP:
                if (jseur<=1L) break;
                --jseur;
                i=etsi(jseur);
                lue_rivi(line);

                SCROLL_DOWN(rdisp,r3,1);
                PR_EUDL;
//              i=sprintf(sbuf,"%6ld %s",j,line+cdisp);
//                i=sprintf(sbuf,"%6ld ",j);
                i=sprintf(sbuf,"%6d ",(int)j);
                strncat(sbuf,line+cdisp,c3+1);
                i=strlen(sbuf);

                if (i>c3+8)
//                    write_string(sbuf,c3+8,shadow_code[sdisp],rdisp+1,1);
					write_string(sbuf,c3+8,'4',rdisp+1,1);
                else
                    {
//                    write_string(sbuf,i,shadow_code[sdisp],rdisp+1,1);
					write_string(sbuf,i,'4',rdisp+1,1);
//                    write_string(space,c3+8-i,'\237',rdisp+1,1+i);
					write_string(space,c3+8-i,'4',rdisp+1,1+i);
                    }

                j=jseur+1L;
                break;

              case CODE_RIGHT:
                ++cdisp;
                nayta_cdisp();
                disp_show(jseur);
                break;
              case CODE_LEFT:
                --cdisp; if (cdisp<0) cdisp=0;
                nayta_cdisp();
                disp_show(jseur);
                break;
              case CODE_END:
                cdisp+=c3+1;
                nayta_cdisp();
                disp_show(jseur);
                break;
/*****************************************************
              case CODE_HELP:
                show_help();
                dispw();
                nayta_cdisp();
                disp_show(jseur);
                break;
******************************************************/

           /* tavalliset napit !special */

              case 'L':
              case 'l':
                strcpy(llines,muste_ltoa(jseur,luku,10));
                strcat(llines,",");
                strcat(llines,muste_ltoa(jseur+(long)mdisp-1L,luku,10));
                putsaa(); LOCATE(r3+2,1);
                prompt("Lines to be loaded ? ",llines,15);
                i=load(llines); if (i<0) return(1);
                putsaa();
                break;

              case 'S':
              case 's':
                putsaa(); LOCATE(r3+2,1);
                prompt("Text to be found? ",sline,50);
                li=search(sline,jseur);
                putsaa();
                if (li<=0L) break;
                jseur=li;
                disp_show(jseur);
                break;

              case 'E':
              case 'e':
                i=etsi(jmax);
                if (i<0) jseur=j-(long)ndisp+1L;
                else jseur=jmax-(long)ndisp+1L;
                disp_show(jseur);
                break;

              case 'C':
              case 'c':
                strcpy(llines,muste_ltoa(jseur,luku,10));
                strcat(llines,",");
                strcat(llines,muste_ltoa(jseur+(long)mdisp-1L,luku,10));
                putsaa(); LOCATE(r3+2,1);
                prompt("Lines to be copied ? ",llines,15);
                LOCATE(r3+2,35); PR_EINV; prompt("To file ? ",cfile,32);
                i=copy_file(llines,cfile);
                putsaa();
                break;

              case 'D':
              case 'd':
                if (edit) break;
                jd=jseur; if (m=='d') jd=jseur+(long)(mdisp-1);
                li=rivit(jd);
                muste_fseek(text,li,0);
                lue_rivi(line);
                for (i=0; i<EDITLEV; ++i)
                    {
                    ch=line[i+cdisp];
                    y1[i]=ch;
                    if (ch==EOS) break;
                    }
                len=i; y1[i]=EOS;
                strcpy(y2,y1);
                putsaa(); LOCATE(r3+2,1);
                prompt(" Edit: ",y1,len);
                if (strlen(y1)<EDITLEV) strncat(y1,space,EDITLEV-strlen(y1));
                if (strlen(y2)<EDITLEV) strncat(y2,space,EDITLEV-strlen(y2));
                if (strcmp(y1,y2)!=0)
                    {
                    muste_fclose(text);
                    text=muste_fopen(tfile,"r+t");
                    muste_fseek(text,li+(long)cdisp,0);
                    for (i=0; i<len; ++i) putc((int)y1[i],text);
                    muste_fclose(text);
                    text=muste_fopen(tfile,"rt");
                    muste_fseek(text,li+(long)cdisp,0);
                    }
                putsaa();
                disp_show(jseur);
                break;

              case 'W': // 26.3.2003;
              case 'w':
                win_conv=1-win_conv;
                if (win_conv)
                    w_codes_load(2);
                disp_show(jseur);
                break;


/*
              case 'A': for (i=0; i<100; ++i) printf(" %ld",rivit[i]);
                        printf(" jmax=%d mdisp=%d",jmax,mdisp);
                        break;
*/
                }
            }
        }

static void nayta_cdisp()
        {
        LOCATE(rdisp,1); PR_EIN2; sprintf(sbuf,"%3d ",cdisp+1); sur_print(sbuf);
        }

static int laji()
        {
        int i;
        char rivi[ELE];
        char *sana[5];
        char nimi[LNAME];
        char *p;

        if (*tfile=='*') { ted1=ed1; ted2=ed2; return(2); }
/********************************  20.3.2007
        if (strchr(tfile,':')==NULL && !netd(tfile))
                                    // 23.2.2006
            {
            if (*tfile=='.') strcpy(nimi,esysd); else strcpy(nimi,edisk);
            strcat(nimi,tfile);
            strcpy(tfile,nimi);
            }
*********************************/
        strcpy(nimi,tfile);
        if (strchr(nimi+strlen(nimi)-4,'.')==NULL) strcat(nimi,".EDT");
        text=muste_fopen(nimi,"rb");
        if (text==NULL)
            {
            text=muste_fopen(tfile,"rb");
            if (text==NULL)
                {
                sprintf(sbuf,"\nText file %s not found!",tfile);
                sur_print(sbuf); WAIT; return(-1);
                }
            return(0);
            }

        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(text);
        rivi[ELE-1]=EOS;

        if (strncmp(rivi,"SURVO 98",8)==0)
            {
            s98_file=1;
            p=strchr(rivi,':'); if (p==NULL) { rewind(text); return(0); }
            i=split(p+1,sana,3);
            ted1=atoi(sana[0]); ted2=atoi(sana[1]);
            tedshad=atoi(sana[2]);
            rewind(text);
            lue_rivi(rivi);
            i=edit32_alut(); if (i<0) return(-1);
            return(3);
            }
        i=split(rivi,sana,5);
        if (i<3) { rewind(text); return(0); }
        if (strcmp(sana[0],"SURVO84ED")!=0) { rewind(text); return(0); }
        ted1=atoi(sana[1]); ted2=atoi(sana[2]);
        if (i>4 && *sana[4]=='S') tedshad=atoi(sana[4]+1);

        muste_fclose(text);
        text=muste_fopen(nimi,"rb");
        init_shadows(); rewind(text);
        return(1);
        }

static int edit32_alut()
        {
        long alku;
        long luettu;
        char *p=NULL;
        long n; /* luettuja tekstirivej„ */
        long l,il;
        char rivi[LLENGTH];

        alut32=(long *)muste_malloc(ted2*sizeof(long));

        luettu=0L; n=0L;
        while (1)
            {
            while (1)
                {
                alku=ftell(text);
                lue_rivi(rivi); ++n;
                if (feof(text)) break;
/* printf("rivi: %s\n",rivi); getch(); */
                p=strchr(rivi,'|');
                if (p==NULL)
                    {
                    sprintf(sbuf,"| missing on (text) line %ld!",n);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                if (*rivi!='S') break; /* omit shadow lines! */
                }
            if (feof(text)) break;
            *p=EOS; il=atol(rivi);
/* printf("il=%ld\n",il); getch(); */
            for (l=luettu+1L; l<il; ++l)
                {
                talleta_alku(l,0L);
                }
            talleta_alku(il,alku);
            luettu=il;
            }
        last_line32=luettu;
        jjmax=jmax=luettu;
        return(1);
        }

/**************************************************
#include <process.h>
show_help()
        {
        char qpath[LNAME];
        int cv;

        strcpy(qpath,survo_path); strcat(qpath,"Q\\EDQ");
        strcpy(info,"SHOWKEYS");
        strcat(info,">"); strcat(info,qpath);
        cv=c; c=1; s_end(siirtop);
        strcpy(qpath,survo_path); strcat(qpath,"CQ.EXE");
        spawnl(P_WAIT,qpath,qpath,siirtop,NULL);
        c=cv; s_end(siirtop);
        }
****************************************************/

static int avaa_alut()
        {
        strcpy(survoxxx,etmpd); strcat(survoxxx,"SURVO.XXX");
        alut=muste_fopen(survoxxx,"wb+");
        if (alut==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s!",survoxxx); sur_print(sbuf);
            WAIT; return(-1);
            }
        jjmax=0L;
        return(1);
        }

static void poista_alut()
        {
        muste_fclose(alut);
/*      sprintf(sbuf,"DEL %s",survoxxx); system(sbuf); */
        remove(survoxxx);
        }
        
static int talleta_alku(long j,long os)
        {
        if (alut32!=NULL)
            {
            alut32[j]=os;
            return(1);
            }
        muste_fseek(alut,(long)(j-1L)*(long)sizeof(int),SEEK_SET); // RS CHA sizeof(long)
        fwrite(&os,sizeof(int),1,alut); // RS CHA sizeof(long)
        if (j>jjmax) jjmax=j;
/* PR_EUDL; sprintf(sbuf,"j=%ld %ld",j,os); sur_print(sbuf); getch(); */
        return(1);
        }

static long rivit(long j)
        {
        long os;

        if (j>jjmax) return(-1L);
        if (alut32!=NULL)
            {
            return(alut32[j]);
            }
        muste_fseek(alut,(long)(j-1L)*(long)sizeof(int),SEEK_SET); // RS CHA sizeof(long)
        fread(&os,sizeof(int),1,alut); // RS CHA sizeof(long)

        return(os);
        }

static int direct_showload(char *line) // 20.7.2006
    {
    int k;
    char rivi[LLENGTH];
    char *s[2];
    int j1,j2;
    char x[LLENGTH];

    strcpy(rivi,line);

    k=split(rivi,s,2);
    if (k==0) return(1);
    j1=etsi_rivi(s[0],1);
    if (j1<=0) return(0); // RS CHA ==
    if (k==1) j2=j1;
    else
        {
        j2=etsi_rivi(s[1],j1);
        if (j2<=0) return(0); // RS CHA ==
        }
    sprintf(x,"%d,%d",j1,j2);
    load(x);

    return(1);
    }


static int etsi_rivi(char *s,int j0)
    {
    int j;
    char *p;

    if (isdigit((int)*s)) return(atoi(s));

    if (*s=='+') return(j0+atoi(s));

    if (edit==3 && muste_strcmpi(s,"END")==0 ) return((int)jjmax);

    if (edit==0 && muste_strcmpi(s,"END")==0 )
        {
        return(number_of_text_lines());
        }
    if (edit==1 && muste_strcmpi(s,"END")==0 )
        {
        return(number_of_edit84_lines());
        }

    if (*s!='"')
        {
        j=get_editline_from_file(tfile,s,j0);
        return(j);
        }

    j=0;
    p=strchr(s+1,'"');
    if (p!=NULL)
        {
        *p=EOS;
        while ((p=strchr(s,'_'))!=NULL) *p=' ';
        p=s+1;
        j=search(p,1);
        if (!text_found) return(0);
        }

    return(j);
    }

static int number_of_text_lines()
    {
    int k,i;

    k=0;
    rewind(text);
    while (!feof(text))
        {
        i=getc(text);
        if (i==(int)'\n') ++k;
        if (i<0) break;
        }
    return(k);
    }

static int number_of_edit84_lines()
    {
    int i,k;

    k=0;
    lue_rivi(sbuf);
    for (i=0; i<ted2; ++i)
        {
        lue_rivi(sbuf);
        if (strncmp(sbuf+1,space,ted1-1)!=0) k=i+1;
        }
    return(k);
    }

static void luo_ikkuna()
        {

   /*   if (r3-r<5) rdisp=1; else  */ rdisp=r+1;
        ndisp=r3-rdisp+1;
        }

static int disp_show(long jseur)
        {
        int i;
        long k,max;
        char x[LLENGTH];
        int crivi,csar;

        CURSOR_OFF;
        if (jseur<1L || jseur>jmax) jseur=1L;
        LOCATE(rdisp+1,1);
        if (!sur_kbhit()) tyhjenna_ikkuna();
                     // SCROLL_UP(rdisp,r3,ndisp);
        max=jseur+(long)ndisp; if (max>jmax+1L) max=jmax+1L;
        if (edit==3) { if (max>=last_line32) max=last_line32+1; }
        mdisp=0;
        for (k=jseur; k<max; ++k)
            {
            i=etsi(k); if (i<0) { muste_fclose(text); return(-1); }
            i=lue_rivi(line);
            if (edit!=2 && feof(text) && !*line) { rewind(text); jmax=k-1; break; }
            if (i<0) return(-1);
            i=sprintf(x,"%6ld ",k); /* if (!edit) i+=sprintf(x+i," "); */
            if (strlen(line)<cdisp)
                i+=sprintf(x+i,"%.*s",c3+1,space);  /* -13.7.94 c-1 */
            else
                i+=sprintf(x+i,"%.*s",c3+1,line+cdisp);  /* -13.7.94 c-1 */
            CURSOR_POS(&crivi,&csar);
            write_string(x,i,'4',crivi,1);
            if (i<c3+8) write_string(space,c3+8-i,'4',crivi,i+1);
            if (k<max-1) sur_print("\n");
            ++mdisp;
            j=k+1L;

       if (edit!=2 && edit!=3 && rivit((long)j)<0L) talleta_alku((long)j,ftell(text));

            }
        CURSOR_ON;
        return(1);
        }

static int etsi(long rivi)
        {
        int merkki;
        int i,m;
        long li;
        int erivi;

        if (edit)
            {
            erivi=rivi;
            if (erivi<1) erivi=1;
            if (erivi>ted2) erivi=ted2;
            j=erivi;
            if (edit==2) return(1);   /* current field */
            if (edit==3)
                {
                while (1)
                    {
                    li=rivit((long)rivi);
                    if (li==-1) return(-1); // 21.7.2006
                    if (li>=0) break;
                    --rivi;
                    }
                if (li>0L) { muste_fseek(text,li,0); empty32=0; j=rivi; return(1); }
                empty32=1; return(1);
                }
            muste_fseek(text,(long)((long)j*(long)ted1),0);
            return(1);
            }

        li=rivit((long)rivi);
        if (li>=0L) { muste_fseek(text,li,0); j=rivi; return(1); }

        if (rivi>j)
            {
            i=0;
            while (j<rivi && !feof(text) && i<MAXL)
                {
                merkki=getc(text);
                ++i;
                if (merkki=='\n')
                    {
                    if (rivit((long)(j+1L))<0L) talleta_alku((long)(j+1L),ftell(text));
                    ++j; i=0;
                    if (sur_kbhit())
                        {
                        m=sur_getch(); if (m=='.') return(-1);
                        }
                    }
                }
            if (i>MAXL-1)
                {
                sprintf(sbuf,"\nToo long line (more than %d characters)!",MAXL-1);
                sur_print(sbuf); WAIT; return(-1);
                }
            }
        if (feof(text)) { text_found=0; jmax=j-1L; rewind(text); }
        return(1);
        }

static int lue_rivi(char *s)
        {
        int merkki;
        int i;
        int k;

        *s=EOS; // 21.7.2006
        if (edit==2)
            { edread(s,j); return(1); }

        if (edit)
            {
            if (edit==3)
                {
                if (empty32)
                    {
                    strncpy(line,space,ted1); line[ted1]=EOS; line[0]='*';
                    return(1);
                    }
                while (1)
                    {
                    k=getc(text); if (k<0) return(-1);
                    *s=(char)k;
                    if (*s=='|') break;
                    }
                i=0;
                while (1)
                    {
                    k=getc(text); if (k<0) return(-1);
                    *s=(char)k;
                    if (*s==CR) break;
                    ++i; ++s;
                    }
                for ( ; i<ted1; ++i) *s++=' '; *s=EOS;
                return(1);
                }
            for (i=0; i<ted1; ++i) { *s=(char)getc(text); ++s; }
            *s=EOS; return(1);
            }

        i=0;
        while ((merkki=getc(text))!='\n' && !feof(text) && i<MAXL-1)
            {
            if ((char)merkki!=CR) s[i++]=(char)merkki;
            }
        s[i]=EOS;

        if (win_conv) // 26-3-2003
            for (k=0; k<i; ++k)
                s[k]=code[(unsigned char)s[k]]; // RS CHA (unsigned char)s[k]=code[(unsigned char)s[k]];

        if (testi<10) tutki(s);
        if (i==MAXL-1)
            {
            sprintf(sbuf,"\nToo long line (more than %d characters)!",MAXL-1);
            sur_print(sbuf); WAIT; return(-1);
            }
        tab_poisto(s);
        return(1);
        }

static int w_codes_load(int k)
    {
    char codefile[LNAME];

    strcpy(codefile,survo_path); strcat(codefile,"SYS/WIN.BIN");
    load_codes(codefile,code,k);
    return(1);
    }

static int tutki(char *s)
        {
        char *p;
        int len,n,riv,sar;

        if (s98_file) return(1);
        len=strlen(s);
        if (len<12) return(1);
        ++testi;
        p=s; n=0;
        while (*p) { if(((unsigned char)*p>30 && (unsigned char)*p<167) || (unsigned char)*p==(unsigned char)TAB // RS CHA (int)
                      || strchr(win_aeao,*p)!=NULL ) ++n; ++p; }
/* PR_EIN2; sprintf(sbuf,"\nlen=%d n=%d",len,n); sur_print(sbuf); getch(); */
        if ((double)n/(double)len<0.9 || len-n>15)
            {
            CURSOR_POS(&riv,&sar);
            LOCATE(r3-5,1); PR_EIN2;
            sur_print("\n----------------------------------------");
            sur_print("\n      Obviously not a text file!        ");
            sur_print("\n      Press ENTER!                      ");
            sur_print("\n----------------------------------------");
            sur_getch();
            testi=10;
            LOCATE(riv,sar);
            return(-1);
            }
        return(1);
        }

static int load(char *s)
        {
        int i,k;
        long kk;
        long k1,k2;
        char *p;
        int alku;
        int shad_permit=1;
        char x[LLENGTH];
        char aika[26];
        int lev=0;

        if (*s==EOS) return(1);
        disp_frame=0;
        p=strchr(s,',');
        if (p==NULL)
            {
            k1=atol(s); if (k1<1L) k1=1L;
            k2=jmax;
            }
        else
            {
            *p=EOS; k1=atol(s); if (k1<1L) k1=1L;
            k2=atol(p+1); if (k2<k1) k2=jmax;
            if (strchr(p+1,'*')!=NULL) disp_frame=1;
            }

        if (edit) alku=0; else alku=1;

        if (disp_frame)
            {
            pvmaika(aika);
            k=sprintf(line,"*   -  - SURVO MM  ");
            k+=sprintf(line+k,"%s %20.20s%7d%5d 0",aika,edisk,ted2,ted1-1);
            edwrite(line,jj,0);
            if (zs[jj]==0) { i=shadow_create(jj); if (i<0) return(-1); }
            for (i=0; i<k; ++i) line[i]=' '; line[k]=EOS;
            line[1]='{'; line[k-1]='}';
            lev=k;
            edwrite(line,zs[jj],0);
            ++jj;
            }

        for (kk=k1; kk<=k2; ++kk)
            {
            if (jj>r2) { ei_tilaa(); break; }
            edread(line,jj);
            if (!empty_s(line+1))
                {
            /*  ei_tilaa();       */
                i=lis_rivit(jj,k2-kk+1L+(long)disp_frame);  /* 29.6.90 */
                if (i<0) break;
                }
            i=etsi(kk); if (i<0) { muste_fclose(text); return(-1); }
            i=lue_rivi(line); if (feof(text) && !*line) { jmax=kk-1; break; }
            if (i<0) return(-1);

            if (!disp_frame) edwrite(line,jj,alku);
            else
                {
                sprintf(x,"*%4ld %s",kk,line);   /* k -> kk 24.5.93 */
                edwrite(x,jj,0);
                }
            j=kk+1;
            if (edit!=3) talleta_alku((long)j,ftell(text));
            if ((edit==1 || (edit==3 && !empty32)) && shad_permit)
                shad_permit=shadow_write((int)kk,jj,disp_frame); /* showsh */
            ++jj;
            }
        if (!shad_permit) return(-1);
        if (disp_frame)
            {
            strncpy(line,space,lev); line[lev]=EOS; line[0]='*';
            edwrite(line,jj,0);
            if (zs[jj]==0) { i=shadow_create(jj); if (i<0) return(-1); }
            for (i=0; i<lev; ++i) line[i]=' '; line[lev]=EOS;
            line[lev-1]='}';
            edwrite(line,zs[jj],0);
            }
        return(1);
        }

static int copy_file(char *s,char *nimi)
        {
        int i;
        long k,k1,k2;
        char *p;
        char x[LLENGTH];
        int alku;

        if (*s==EOS) return(1);
        p=strchr(s,',');
        if (p==NULL)
            {
            k1=atol(s); if (k1<1L) k1=1L;
            k2=jmax;
            }
        else
            {
            *p=EOS; k1=atol(s); if (k1<1L) k1=1L;
            k2=atol(p+1); if (k2<k1) k2=jmax;
            }

        if (edit) alku=1; else alku=0;

        strcpy(x,nimi);
        if (strchr(x,':')==NULL) { strcpy(x,edisk); strcat(x,nimi); }
        copyfile=muste_fopen(x,"at");
        if (copyfile==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s !",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        for (k=k1; k<=k2; ++k)
            {
            i=etsi(k); if (i<0) { muste_fclose(text); return(-1); }
            i=lue_rivi(line); if (feof(text) && !*line) { jmax=k-1; break; }
            fprintf(copyfile,"%s\n",line+alku);
            if (i<0) return(-1);
            j=k+1;
            if (edit!=3)  talleta_alku((long)j,ftell(text));
            }
        muste_fclose(copyfile);
        return(1);
        }

static int empty_s(char *s)
        {
        while (*s) { if (*s!=' ') return(0); ++s; }
        return(1);
        }

static void ei_tilaa()
        {
        putsaa();
        PR_EBLK; LOCATE(r3+2,1);
        sur_print("Not lines enough in the edit field! (Press any key!)");
        WAIT; PR_ENRM;
        }

static int lis_rivit(int jj,long kpl)
        {
        char x[LLENGTH];
        int m;

        putsaa(); PR_EBLD; LOCATE(r3+2,1);
        if (kpl>(long)r2) { ei_tilaa(); return(-1); }
        m=spec_find("INSERT",x,LLENGTH); // 20.7.2006
        if (m<0 || (m>=0 && atoi(x)==0) )
            {
            sprintf(x,"%sSND\\NEWALERT.WAV",survo_path);
            sur_play_sound(x);
            sprintf(x,"Not enough empty lines. Insert space for %ld lines (Y/N) ?",kpl);
            LOCATE(r3+2,strlen(x)+2);
            m=nextch(x);
            if (m!='y' && m!='Y') return(-1);
            PR_ENRM;
            }
        return(insert_lines(jj,(int)kpl));
        }

static int putsaa()
        {
//      LOCATE(r3+2,1); PR_EINV;
//      sprintf(sbuf,"%.*s",c3+7,space); sur_print(sbuf);
        PR_EINV;
        write_string(space,c3+8,shadow_code[sdisp],r3+2,1);
        return(1);
        }


static long search(char *s,long jseur)
        {
        int i;
        long k;
        char *p;
        int len=strlen(s);
        long li;

        text_found=1;
        if (*s==EOS) return(0L);
        if (*s=='#')
            {
            li=atol(s+1); if (li<1L) li=1;
            if (li>jmax) li=jmax-ndisp+1;
            etsi(li);
            if (j>jmax-(long)ndisp) return(jmax-ndisp+1); else return(j);
            }
        k=jseur+1;
        while ((edit==2 && k<=ted2) || (edit!=2 &&!feof(text)))
            {
            if (sur_kbhit())
                { i=sur_getch(); if (i=='.') return(k); }
            i=etsi(k); if (i<0) { text_found=0; return(k); }
            i=lue_rivi(line); if (edit!=2 && feof(text) && !*line) { jmax=k-1; text_found=0; break; }
            if (i<0) return(-1);
            p=line;
            while ((p=strchr(p,*s))!=NULL)
                {
                if (strncmp(s,p,len)!=0) { ++p; continue; }
                return(k);
                }
            j=k+1;
        /*  rivit[j]=ftell(text);  */
            if (edit!=2 && edit!=3) talleta_alku((long)j,ftell(text));
            ++k;
            if (edit && k>ted2) break;
            if (edit==3 && k>last_line32) { text_found=0; break; }
            }
        if (edit!=2 && feof(text)) { text_found=0; rewind(text); }
        return(jseur);
        }


static void tab_poisto(char *s)
        {
        char *p;
        int i;

        strcpy(ttab,s);
        p=ttab; i=0;
        while (*p)
            {
            if (*p==TAB)
                {
                int k=(int)(i/TABSPACE)*TABSPACE+TABSPACE-1;
                for (; i<=k; ++i) s[i]=' ';
                }
            else { s[i]=*p; ++i; }
            ++p;
            }
        s[i]=EOS;
        }


static int insert_lines(int jj,int k)
        {
        int j;
        char x[LLENGTH];

        for (j=r2; j>r2-k; --j)
            {
            edread(x,j);
            if (!empty_s(x+1)) break;
            }
        if (j>r2-k) { ei_tilaa(); return(-1); }

/*      memmove(z+(r1+r)*ed1,z+(r1+r-1)*ed1,(r2-r1-r)*ed1);    */
        memmove(z+(jj+k-1)*ed1,z+(jj-1)*ed1,(r2-k-jj+1)*ed1);

        strcpy(x,"*"); strncat(x,space,c2);
        for (j=jj; j<jj+k; ++j)
            edwrite(x,j,0);

        for (j=r2-k; j>=jj; --j) zs[j+k]=zs[j];
        for (j=jj; j<jj+k; ++j) zs[j]=0;
        return(1);
        }

static FILE *codes;
static int load_codes(char *codefile,char *code,int col)
        {
        int i;
        char x[LLENGTH];

        strcpy(x,codefile);
        if (strchr(x,':')==NULL && *x!='.' && *x!='/' && *x!='\\') // RS FIXME path
            { strcpy(x,survo_path); strcat(x,"SYS/"); strcat(x,codefile); }

        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nCode conversion file %s not found!",x);
            sur_print(sbuf); WAIT; return(-1);
            }
        if (col>1) muste_fseek(codes,(long)(col-1)*256L,SEEK_SET);
        for (i=0; i<256; ++i) code[i]=(char)getc(codes); // RS REM unsigned
        muste_fclose(codes);
        return(1);
        }
        
static int init_shadows()
        {
        int i;
        long l;
        unsigned int j;
        short *pint;
        char x[3];

        shad_int=(int *)muste_malloc(tedshad*sizeof(int));
        if (shad_int==NULL) return(-1);

        pint=(short *)x;
        n_shad=0;
        l=(long)ted1*(long)(ted2+2);
        while (1)
            {
            muste_fseek(text,l,0);
            for (i=0; i<3; ++i) x[i]=(char)getc(text);
            if (strncmp(x,"END",3)==0) break;
            if (feof(text)) break;
            j=(int)*pint;
            shad_int[n_shad++]=j;
            l+=ted1;
            }
        return(1);
        }

static int shadow_write(unsigned int j,unsigned int jj,int disp_frame)
        {
        int i,k;
        long l;
        char x[LLENGTH];
        char y[LLENGTH];
        char *p;

        if (edit!=3)
            {
            for (k=0; k<n_shad; ++k)
                {
                if (j==shad_int[k]) break;
                }
            if (k==n_shad) return(1);
            }

        if (edit==3)
            {
            getc(text); /* LF */
            *x=(char)getc(text);
/* printf("*x=%c\n",*x); getch(); */
            if (*x!='S') return(1);
            while (1) { *x=(char)getc(text); if (*x=='|') break; }
            i=0; p=x;
            while (1)
                {
                *p=(char)getc(text);
                if (*p==CR) break;
                ++i; ++p;
                }
            for ( ; i<ted1; ++i) *p++=' '; *p=EOS;
            }
        else
            {
            l=(long)ted1*(long)(ted2+2+k)+2L;
            muste_fseek(text,l,0);
            for (i=2; i<ted1; ++i) x[i-2]=(char)getc(text);
            x[ted1-2]=EOS; strcat(x,"  ");
            }
        if (zs[jj]==0) { i=shadow_create(jj); if (i<0) return(0); }
        if (disp_frame)
            { strcpy(y,"      "); strcat(y,x); strcpy(x,y); }
        edwrite(x,zs[jj],0);
        return(1);
        }
        
static int ted1,ted2;
static FILE *text;

static int get_editline_from_file(char *tfile,char *label,int line1)
        {
        int i;
        char rivi[ELE];
        char *sana[5];
        char nimi[LNAME];
        char *p;
        int edit;
        int li=0;
        int fend;

        fend=0;
        if (muste_strcmpi(label,"END")==0) fend=1;

/********************* 20.3.2007
        if (strchr(tfile,':')==NULL && !netd(tfile))
            {
            if (*tfile=='.') strcpy(nimi,esysd); else strcpy(nimi,edisk);
            strcat(nimi,tfile);
            strcpy(tfile,nimi);
            }
********************/

        strcpy(nimi,tfile);
        if (strchr(nimi+strlen(nimi)-4,'.')==NULL) strcat(nimi,".EDT");
        text=muste_fopen(nimi,"rb");
        if (text==NULL)
            return(0);

        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(text);
        rivi[ELE-1]=EOS;

        if (strncmp(rivi,"SURVO 98",8)==0)
            {
            p=strchr(rivi,':'); if (p==NULL) { rewind(text); return(0); }
            i=split(p+1,sana,3);
            ted1=atoi(sana[0]); ted2=atoi(sana[1]);
//          tedshad=atoi(sana[2]);
            rewind(text);
            edit=3;


            while (!feof(text))
                {
                i=lue_editrivi(rivi,edit);
                if (fend && i<0) return(li);

                if (*rivi=='S') continue;
                p=strchr(rivi,'|');
                if (p==NULL) { sur_print("\nError in edit file!"); WAIT; return(-1); } // RS CHA exit
                *p=EOS; ++p;
                if (fend)
                    {
                    li=atoi(rivi); // lopulta viimeinen rivi
                    }
                else if (*label==*p)
                    {
                    li=atoi(rivi); if (li<=line1) continue;
                    if (strlen(label)>1) li+=atoi(label+1);
                    break;
                    }
                }
            muste_fclose(text);
            return(li);
            }
        i=split(rivi,sana,5);
        if (i<3) { rewind(text); return(0); }
        if (strcmp(sana[0],"SURVO84ED")!=0) { rewind(text); return(0); }
        ted1=atoi(sana[1]); ted2=atoi(sana[2]);
//      if (i>4 && *sana[4]=='S') tedshad=atoi(sana[4]+1);

        rewind(text);
        lue_editrivi(rivi,1);

        for (i=0; i<ted1; ++i)
            {
            lue_editrivi(rivi,1);
            if (*label==*rivi)
                {
                li=i+1; if (li<=line1) continue;
                if (strlen(label)>1) li+=atoi(label+1);
                }
            }
        muste_fclose(text);
        if (i>=ted1) return(0);
        return(li);
        }

static int lue_editrivi(char *s,int edit)
        {
        int i;
        int k;

        if (edit==3)
            {
/*
            while (1)
                {
                *s=(char)getc(text);
                if (*s=='|') break;
                }
*/
            i=0;
            while (1)
                {
                k=getc(text); if (k<0) return(-1);
                *s=(char)k;
                if (*s==CR) break;
                ++i; ++s;
                }
            for ( ; i<ted1; ++i) *s++=' '; *s=EOS;
            return(1);
            }
        for (i=0; i<ted1; ++i) { *s=(char)getc(text); ++s; }
        *s=EOS; return(1);
        }

        
