#include "muste.h"
/* _eps.c 17.12.1989/SM (25.5.1993) (26.4.1997)
*/
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "survo.h"
#include "survolib.h"
#include "survoext.h"

static FILE *ps,*eps;
static char psnimi[LLENGTH],epsnimi[LLENGTH];

static char version_comment[LLENGTH];
static char bounding_box[LLENGTH];
static char title[LLENGTH];
static char creator[LLENGTH];
static char creation_date[LLENGTH];
static char end_comments[LLENGTH];
static char document_fonts[LLENGTH];
static int prind=0;

static char rivi[LLENGTH];

static char *psnimi2;
static char bb_nimi[LNAME]; // 24.8.2008;
static int xhome1,yhome1,xhome2,yhome2;
static double xscale1,yscale1,xscale2,yscale2;
static double bb[4],bb2[4];  // %%BoundingBox bb[0] bb[1] bb[2] bb[3]  24.8.2009

static int bbb=0; // bbb=1 (JOIN2) BoundingBox to be updated  (24.8.2009)

static char xrivi[100000];

//extern char **spb;

/*
char *specs0[]={ "BOUNDINGBOX", "VERSION_COMMENT", "TITLE", "CREATOR",
                 "CREATION_DATE", "DOCUMENT_FONTS",
                 "PRIND", "!" };
char **specs=specs0;
*/

static int eps_virhe(char *s);
static void nimea(char *nimi,char *s,char *ext);
static char *ala_pois(char *s);
static void specifications();
static void bbspec();
static int etsi(char *s,char *t);
static int lue(char *rivi);
static int add_page_comments();
static int page_comment(int n);
static int join();
static int mahtuu();
static int psp_filter();
static int lue_x(char *rivi);


void muste_eps2(int argc, char *argv[])
        {
        int i;
        char *p;
        long n;
        char x[LLENGTH],*sana[4];
        double a;

ps=eps=NULL;
prind=0;
psnimi2=NULL;
bbb=0;
/*
        if (argc==1)
            {
            Rprintf("This program can be used as a SURVO MM module only.");
            return;
            }
*/            
        s_init(argv[1]);
        i=spec_init(r1+r-1); if (i<0) return;  // RS ADD

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        if (strncmp(info,"EPS ",4)==0)   /* child of PLOT */
            {
            strcpy(comline+1,info);
            g=split(comline+1,word,3);
            }
            
        if (g<2)
            {
            init_remarks();
            rem_pr("EPS <PSfile>,<EPSfile>           / S.Mustonen 17.12.1989");
   rem_pr("converts a Survo PostScript file (made by PLOT with DEVICE=PS,<Psfile>)");
            rem_pr("to an Encapsulated PostScript file.                     ");
            rem_pr("More information by EPS?                                ");
            wait_remarks(2);
            s_end(argv[1]);
            return;
            }

        if (muste_strcmpi(word[1],"JOIN")==0)
            {
            join(); return;
            }
        if (muste_strcmpi(word[1],"JOIN2")==0) // 24.8.2009
            {
            bbb=1; join(); return;
            }

        if (g>3 && muste_strcmpi(word[3],"PAGES")==0)
            {
            add_page_comments();
            return;
            }
        if (muste_strcmpi(word[1],"PSP")==0) // PaintShopPro filter 1.1.2005
            {
            psp_filter();
            return;
            }
        sp_init(r1+r-1);
        nimea(psnimi,word[1],".PS");
        i=2; if (g<3) i=1;
        nimea(epsnimi,word[i],".EPS");
        if (muste_strcmpi(psnimi,epsnimi)==0)
            {
            sur_print("\nThe file cannot be copied onto itself!");
            WAIT; return;
            }

        ps=muste_fopen(psnimi,"rt");
        if (ps==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s for reading!",psnimi);
            sur_print(sbuf); WAIT; return;
            }
        eps=muste_fopen(epsnimi,"wt");
        if (eps==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s for writing!",epsnimi);
            sur_print(sbuf); WAIT; return;
            }

        i=lue(rivi); if (i<0) return;
        if (strncmp(rivi,"%! SURVO 84C",12)!=0)
            {
            sprintf(sbuf,"\n%s is not a Survo PostScript file!",psnimi);
            sur_print(sbuf); WAIT; return;
            }

        sprintf(sbuf,"\nCopying Survo PostScript file %s",psnimi);
        sur_print(sbuf);
        sprintf(sbuf,"\nto an Encapsulated PostScript file %s: ",epsnimi);
        sur_print(sbuf);

        specifications();
        i=spfind("BOUNDINGBOX");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            i=split(x,sana,4);
            if (i<4)
                {
                bbspec();
                WAIT; return;
                }
            a=0.2834646;  /* dmm -> Point */
            sprintf(bounding_box,"%%%%BoundingBox: %d %d %d %d",
                     (int)(a*atof(sana[0])), (int)(a*atof(sana[1])),
                     (int)(a*atof(sana[2])), (int)(a*atof(sana[3])));
            }
        else
            {
            i=etsi("%%BoundingBox:",bounding_box);

            if (i<0)
                {
/*********************************** 5.2.2004
                sprintf(sbuf,"\n%%%%BoundingBox:  not found in %s!",psnimi);
                sur_print(sbuf);
                sur_print("\nYou can replace it by entering a BOUNDINGBOX specification!");
                bbspec();
                WAIT; return;
****************************/
                strcpy(bounding_box,"-");
                }

//          rewind(ps);
            else
                {
                p=bounding_box;
                p[strlen(p)-1]=EOS; /* poista lf */
                }
            rewind(ps);
            }
        fprintf(eps,"%s\n",version_comment);
        fprintf(eps,"%s\n",title);
        if (*creator) fprintf(eps,"%s\n",creator);
        fprintf(eps,"%s\n",creation_date);
        if (*bounding_box!='-') fprintf(eps,"%s\n",bounding_box);
        if (*document_fonts) fprintf(eps,"%s\n",document_fonts);
        fprintf(eps,"%s\n",end_comments);

        while (1)
            {
            i=lue(rivi); if (i<0) { eps_virhe("/survodict"); return; }
            if (strncmp(rivi,"/survodict ",11)==0) break;
            }
        n=5;
        while (1)
            {
            fputs(rivi,eps);
            ++n; if (prind) { sprintf(sbuf,"%ld ",n); sur_print(sbuf); }
            i=lue(rivi); if (i<0) { eps_virhe("%SURVO 84C Graphics END"); return; }
            if (muste_strnicmp(rivi,"%SURVO ",6)==0)
                {
                if (strncmp(rivi,"%SURVO 84C Graphics END",23)==0) break;
                if (strncmp(rivi,"%Survo Graphics END",19)==0) break;
                }
            }
        fprintf(eps,"end\n");
        muste_fclose(ps); muste_fclose(eps);
        }

void muste_eps(int argc, char *argv[]) // RS ADD Confirm that files are closed
        {
		ps=NULL;
		eps=NULL;
		muste_eps2(argc,argv);
		if (ps!=NULL) muste_fclose(ps);
		if (eps!=NULL) muste_fclose(eps);
		}
		

static int eps_virhe(char *s)
        {
        sprintf(sbuf,"\n%s    not found!",s);
        sur_print(sbuf); WAIT;
        return(1);
        }


static void nimea(char *nimi,char *s,char *ext)
        {
        strcpy(nimi,s);
        if (strchr(s,':')==NULL) { strcpy(nimi,edisk); strcat(nimi,s); }
        if (strlen(s)<4 || strchr(s+strlen(s)-4,'.')==NULL) strcat(nimi,ext);
        }

static char *ala_pois(char *s)
        {
        char *p;

        p=s;
        while (*p) { if (*p=='_')  *p=' '; ++p; }
        return(s);
        }

static void specifications()
        {
        int i;
        time_t aika;
        char x[LLENGTH];


        strcpy(version_comment,"%!PS-Adobe-2.0 EPSF-2.0");
        i=spfind("VERSION_COMMENT");
        if (i>=0) strcpy(version_comment,ala_pois(spb[i]));
        strcpy(title,"%%Title: Survo PostScript file");
        i=spfind("TITLE");
        if (i>=0) strcpy(title+9,ala_pois(spb[i]));
        *creator=EOS;
        i=spfind("CREATOR");
        if (i>=0) { strcpy(creator,"%%Creator: "); strcat(creator,ala_pois(spb[i])); }
        time(&aika);
        strcpy(x,ctime(&aika)); x[24]=EOS;
        strcpy(creation_date,"%%CreationDate: "); strcat(creation_date,x);
        i=spfind("CREATION_DATE");
        if (i>=0) strcpy(creation_date+16,ala_pois(spb[i]));
        strcpy(end_comments,"%%EndComments");
        *document_fonts=EOS;
        i=spfind("DOCUMENT_FONTS");
        if (i>=0) { strcpy(document_fonts,"%%DocumentFonts: ");
                    strcat(document_fonts,ala_pois(spb[i])); }
        }

static void bbspec()
        {
        sur_print("\nUsage: BOUNDINGBOX=LLx,LLy,URx,URy");
        sur_print("\n  where LLx is lower left x coordinate,");
        sur_print("\n        LLy is lower left y coordinate,");
        sur_print("\n        URx is upper right x coordinate,");
        sur_print("\n        URy is upper right y coordinate");
        sur_print("\n  in 0.1 mm units. EPS converts them to Points.");
        }

static int etsi(char *s,char *t)
        {
        int i;
        char rivi[LLENGTH];
        int len;

        len=strlen(s);
        rewind(ps);
        while (!feof(ps))
            {
            i=lue(rivi); if (i<0) return(-1);
            if (strncmp(rivi,s,len)==0) break;
            }
        if (feof(ps)) return(-1);
        strcpy(t,rivi);
        return(1);
        }

static int lue(char *rivi)
        {
        if (feof(ps)) return(-1);
        fgets(rivi,LLENGTH-1,ps);
        return(1);
        }

static int add_page_comments()
        {
        int i;
// RS REM        char *p;
// RS REM        long n;
// RS REM        char x[LLENGTH],*sana[4];
        int page_number;

        sp_init(r1+r-1);
        nimea(psnimi,word[1],".PS");
        i=2; if (g<3) i=1;
        nimea(epsnimi,word[i],".PS");
        if (muste_strcmpi(psnimi,epsnimi)==0)  /* epsnimi edelleen ps-tiedosto */
            {
            sur_print("\nThe file cannot be copied onto itself!");
            WAIT; return(-1);
            }

        ps=muste_fopen(psnimi,"rt");
        if (ps==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s for reading!",psnimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        eps=muste_fopen(epsnimi,"wt");
        if (eps==NULL)
            {
            sprintf(sbuf,"\nCannot open file %s for writing!",epsnimi);
            sur_print(sbuf); WAIT; return(-1);
            }

        i=lue(rivi); if (i<0) return(-1);
        if (strncmp(rivi,"%! SURVO 84C",12)!=0)
            {
            sprintf(sbuf,"\n%s is not a SURVO 84C PostScript file!",psnimi);
            sur_print(sbuf); WAIT; return(-1);
            }

        sprintf(sbuf,"\nCopying Survo PostScript file %s",psnimi);
        sur_print(sbuf);
        sprintf(sbuf,"\nwith PAGE comments to PostScript file %s: ",epsnimi);
        sur_print(sbuf);

        fprintf(eps,"%%!PS-Adobe-3.0\n");

        while (1)
            {
            i=lue(rivi); if (i<0) { eps_virhe("%PRINT:"); return(-1); }
            fputs(rivi,eps);
            if (strncmp(rivi,"%PRINT:",7)==0) break;
            }
        page_number=1;
        page_comment(page_number);

        while (1)
            {
            i=lue(rivi); if (i<0) break;
            if (strncmp(rivi,"%HOME:",6)==0 || strncmp(rivi,"%SCALING:",9)==0)
                { i=lue(rivi); if (i<0) break; continue; }

            if (strncmp(rivi,"%END:",5)==0)
                {
                fprintf(eps,"%%%%Trailer\n"); // RS REM ,page_number);
                fprintf(eps,"%%%%Pages: %d\n",page_number);
                fprintf(eps,"%%%%EndDocument\n"); // RS REM ,page_number);
         /*     break;   */  continue;
                }
            fputs(rivi,eps);
            if (strncmp(rivi,"%NEWPAGE:",9)==0)
                {
                ++page_number;
                page_comment(page_number);
                }
            }
        muste_fclose(ps); muste_fclose(eps);
        return(1);
        }

static int page_comment(int n)
        {
        fprintf(eps,"%%%%Page: %d %d\n",n,n);
        return(1);
        }


/* eps2.c 24.5.1993/SM (27.9.1993) (16.9.1994)
   EPS JOIN ABC.PS,A,B,C  / A=<PS_file>,<x>,<y> B=... C=...
*/

static int join()
        {
        int i,k;
        char x[LLENGTH],*sana[5];
        double a,a1,a2;

        if (g<5)
            {
            init_remarks();
            rem_pr("EPS JOIN <target.ps>,A1,A2,A3,...         / S.Mustonen 24.5.1993");
            rem_pr("    A1=<ps1.ps>,<x1>,<y1>,<kx1>,<ky1>  A2=<ps2.ps>,<x2>,<y2>,<kx2>,<ky2>  ...");
            rem_pr("combines Survo PS files (made by PLOT) into one PostScript file.");
            rem_pr("In A1, <x1>,<y1> are (relative) coordinates of the graphs and");
            rem_pr("       <kx1>,<ky1> are scaling coefficients.");
            rem_pr("       Default values: <x1>=<y1>=0, <xk1>=<yk1>=1");
            rem_pr("To copy the target file into an EPS file, use EPS <target.ps>,<EPSfile>.");
            wait_remarks(2);
            return(-1);
            }

        psnimi2=epsnimi;
        i=spec_init(r1+r-1); if (i<0) return(-1);
        nimea(psnimi2,word[2],".PS");
        if (bbb==1) // 24.8.2009
          { strcpy(bb_nimi,etmpd); strcat(bb_nimi,"K.PS"); psnimi2=bb_nimi; }

        xhome1=yhome1=0;
        xscale1=yscale1=1.0;

        bb[0]=bb[1]=1e10; bb[2]=bb[3]=0.0; // 24.8.2009

        for (k=3; k<g; ++k)
            {
            xhome2=yhome2=0; xscale2=yscale2=1.0;
            i=spfind(word[k]);
            if (i<0)
                {
                nimea(psnimi,word[k],".PS");
                }
            else
                {
                strcpy(x,spb[i]);
                i=split(x,sana,5);
                if (i>1) xhome2=atoi(sana[1]);
                if (i>2) yhome2=atoi(sana[2]);
                if (i>3) xscale2=atof(sana[3]);
                if (i>4) yscale2=atof(sana[4]);
                nimea(psnimi,sana[0],".PS");
                }
// Rprintf("\npsnimi=%s psnimi2=%s|",psnimi,psnimi2); getch();
            if (muste_strcmpi(psnimi,psnimi2)==0)
                {
                sprintf(sbuf,"\nTarget file name %s cannot be equal to any of the components!",
                                      psnimi);
                sur_print(sbuf); WAIT; return(-1);
                }
            ps=muste_fopen(psnimi,"rt");
            if (ps==NULL)
                {
                sprintf(sbuf,"\nCannot open file %s for reading!",psnimi);
                sur_print(sbuf); WAIT; return(-1);
                }

            lue(rivi);
            if (strncmp(rivi,"%! SURVO 84C",12)!=0)
                {
                sprintf(sbuf,"\n%s is not a Survo PostScript file!",psnimi);
                sur_print(sbuf); WAIT; return(-1);
                }


            if (k==3)
                {
                eps=muste_fopen(psnimi2,"wt");
                if (eps==NULL)
                    {
                    sprintf(sbuf,"\nCannot open file %s for writing!",epsnimi);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                }
            while (1)
                {
                if (k==3) { fputs(rivi,eps); i=mahtuu(); if (i<0) return(-1); }
                lue(rivi);
                if (feof(ps)) // 19.2.2002
                    {
                    sprintf(sbuf,"\n%s not a valid Survo PS file!",psnimi);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                if (muste_strnicmp(rivi,"%SURVO ",7)==0) break;
                }
            if (k==3) fputs(rivi,eps);

            fprintf(eps,"%%File %s\n",psnimi);
            fprintf(eps,"gsave\n");

            if (xhome2!=0 || yhome2!=0)
                {
                fprintf(eps,"%d m %d m translate\n",xhome2,yhome2);
                xhome1=xhome2; yhome1=yhome2;
                }

            if (xscale2!=1.0 || yscale2!=1.0)
                {
                fprintf(eps,"%g %g scale\n",xscale2,yscale2);
                xscale1=xscale2; yscale1=yscale2;
                }

            while (1)
                {
                lue(rivi);
          // 24.8.2009 JOIN2
                if (bbb==1 && strncmp(rivi,"%%BoundingBox:",14)==0) // 24.8.2009
                    {
                    a=0.2834646;
                    strcpy(x,rivi); i=split(x,sana,5);
                    bb2[0]=atof(sana[1]);
                    bb2[1]=atof(sana[2]);
                    a1=atof(sana[3]); a2=xscale2*(a1-bb2[0]);
                    bb2[2]=a*(double)xhome2+a2;
                    a1=atof(sana[4]); a2=yscale2*(a1-bb2[1]);
                    bb2[3]=a*(double)yhome2+a2;

                    if (bb2[0]<bb[0]) bb[0]=bb2[0];
                    if (bb2[1]<bb[1]) bb[1]=bb2[1];
                    if (bb2[2]>bb[2]) bb[2]=bb2[2];
                    if (bb2[3]>bb[3]) bb[3]=bb2[3];
                    }
// (dmm:Point)=0.28346456692913

                if (feof(ps)) // 19.2.2002
                    {
                    sprintf(sbuf,"\n%s not a valid Survo PS file!",psnimi);
                    sur_print(sbuf); WAIT; return(-1);
                    }
                if (muste_strnicmp(rivi,"%SURVO ",7)==0) break;
                fputs(rivi,eps);
                i=mahtuu(); if (i<0) return(-1);
                }
            fprintf(eps,"grestore\n");
            if (k<g-1) { muste_fclose(ps); continue; }
            fputs(rivi,eps);
            while (1)
                {
                lue(rivi);
                if (feof(ps)) break;
                fputs(rivi,eps);
                }
            }  /* k */
        putc((int)'\004',eps);
        muste_fclose(eps);

    if (bbb==1)
        {

        ps=muste_fopen(bb_nimi,"rt");
        psnimi2=epsnimi;
        nimea(psnimi2,word[2],".PS");
        eps=muste_fopen(psnimi2,"wt");

        k=0;
        while (1)
            {
            lue(rivi);
            if (feof(ps)) break;
            if (strncmp(rivi,"%%BoundingBox:",14)==0) // 24.8.2009
                {
                if (k==1) continue;
                sprintf(rivi,"%%%%BoundingBox: %g %g %g %g\n",
                                   bb[0],bb[1],bb[2],bb[3]);
                k=1;
                }
            fputs(rivi,eps);
            }
        }

        return(1);
        }

static int mahtuu()
        {
        if (!ferror(eps)) return(1);
        sur_print("\nCannot save in the target file!"); WAIT; return(-1);
        }

static int psp_filter() // EPS PSP <eps1>,<eps2>
    {
    int i,m;
// RS REM    char rivi[64];

    if (g<4)
        {
        sur_print("\nUsage: EPS PSP <eps_file>,<filtered_eps_file>");
        WAIT; return(1);
        }
    nimea(psnimi,word[2],".EPS");
    nimea(epsnimi,word[3],".EPS");

    ps=muste_fopen(psnimi,"rt");
    if (ps==NULL)
        {
        sprintf(sbuf,"\nCannot open file %s for reading!",psnimi);
        sur_print(sbuf); WAIT; return(1);
        }
    eps=muste_fopen(epsnimi,"wt");
    if (eps==NULL)
        {
        sprintf(sbuf,"\nCannot open file %s for writing!",epsnimi);
        sur_print(sbuf); WAIT; return(1);
        }

    for (i=0; i<32; ++i) fgetc(ps);
    for (i=0; i<4; ++i)
        {
        m=fgetc(ps);
        xrivi[i]=m;
        }
    xrivi[4]=EOS;
// Rprintf("\nrivi=%s|",rivi); getch();
    if (strcmp(xrivi,"%!PS")!=0)
        { sur_print("Input file not valid: Bytes 32- should be %!PS");
          WAIT; return(1);
        }
    for (i=0; i<4; ++i)
        fputc(xrivi[i],eps);

    while (1)
        {
        lue_x(xrivi);
        if (feof(ps)) break;
        if (strncmp(xrivi,"%%Trailer",9)==0) break;
        fputs(xrivi,eps);
        }
    muste_fclose(ps);
    muste_fclose(eps);
    return(1);
    }

static int lue_x(char *rivi)
        {
        if (feof(ps)) return(-1);
        fgets(rivi,99999,ps);
        return(1);
        }

