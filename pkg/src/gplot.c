#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"



// #define TAB '\t'

// #define RGB(r,g,b) (((long) ((b) << 8 | (g)) << 8) | (r))

#define RGB(r,g,b) ( ((int)(unsigned char)r)|((int)((unsigned char)g)<<8)|((int)((unsigned char)b)<<16) )
/*
#define RGB(r,g,b)          ((DWORD)(((BYTE)(r)|((WORD)(g)<<8))|(((DWORD)(BYTE)(b))<<16)))
#define PALETTERGB(r,g,b)   (0x02000000UL | RGB(r,g,b))
#define PALETTEINDEX(i)     ((DWORD)(0x01000000UL | (WORD)(i)))

#define GetRValue(rgb)      ((BYTE)(rgb))
#define GetGValue(rgb)      ((BYTE)(((WORD)(rgb)) >> 8))
#define GetBValue(rgb)      ((BYTE)((rgb)>>16))
*/

#define WHITE_BRUSH         0
#define LTGRAY_BRUSH        1
#define GRAY_BRUSH          2
#define DKGRAY_BRUSH        3
#define BLACK_BRUSH         4
#define NULL_BRUSH          5
#define HOLLOW_BRUSH        NULL_BRUSH
#define WHITE_PEN           6
#define BLACK_PEN           7
#define NULL_PEN            8
#define OEM_FIXED_FONT      10
#define ANSI_FIXED_FONT     11
#define ANSI_VAR_FONT       12
#define SYSTEM_FONT         13
#define DEVICE_DEFAULT_FONT 14
#define DEFAULT_PALETTE     15
#define SYSTEM_FIXED_FONT   16
#define DEFAULT_GUI_FONT    17
#define DC_BRUSH            18
#define DC_PEN              19
#define STOCK_LAST          19



#define DEFSCREENXSIZE 1024
#define DEFSCREENYSIZE 768
#define DEFWINXSIZE 300
#define DEFWINYSIZE 225
#define DEFWINXHOME 0
#define DEFWINYHOME 0

#define MAXPLOTWINDOWS 300
#define MAX_HDL MAXPLOTWINDOWS

#define VASEN_REUNA 0
#define ALAREUNA 0
#define PII 3.14159265
#define MAX 255  /* max consecutive charaters in .WPX files */
#define WPX_CONST -32091
#define N_SURCOLORS 64
#define N_STOCK_COLOR 16
#define NPAR 100
#define SCALESPACE 300
#define SHADEMAX 32
#define MAXTEXTS 32

#define MAXLOOP 10
#define N_MESS 10
#define MAXSCALELIST 100

#define MAXPITUUS 100
#define MAXARG 10

#define NPEN 1000
#define NBRUSH 200


int muste_gplot_init=0;
extern char **spa,**spb,**spshad,**spb2;
extern int spn;
extern double *arvo;
extern char *spl;

extern char gplot_layout[];
extern int etu;
extern char *op;
extern char sur_session[];
extern int r,r1;
extern char space[];
extern int sdisp;
extern char *parm[];
// RS REM extern double arit_atof();

// extern double integral();
// extern int integral_function;
// extern int line_color;
// extern int char_color;
// extern int capability[];
static int l_virhe;
// extern char curve_fill_attr[];


static int muste_xpos,muste_ypos;

static char curve_fill_attr[LNAME]; // 20.5.2005

static int capability[2];
    /*
      capability[0]   1=vÑlitulostukset sallittu 0=ei
      capability[1]   1=autom.fill               0=ei

    */
    
static int scalemove_x,scalemove_y; /* 12.2.1993 */    

static int odota_tuloksia;
int fixed_plot=0;
int fixed_plot_number;
int first_plot_number=1;
int gplot_count;

static unsigned long hdl[MAX_HDL];
static unsigned long hdl2[MAX_HDL];
int max_hdl=MAX_HDL;

static FILE *his;
static FILE *gpl;

static int x_pos,y_pos;
static int x_home, y_home;     /* koko kuvan vasen alakulma */
static int xx,yy;              /* kuva-alueen vasen alakulma */
static int x_size, y_size;     /* kuvan koko */

int x_wsize,y_wsize;    /* ikkunan fyysinen koko */
int x_whome,y_whome;    /* ikkunan fyysinen paikka */
static int x_size, y_size;     /* kuvan virtuaalinen koko */
static int x_metasize,y_metasize; /* ikkunan koko metatiedostossa */

static double y_ratio;
static int ps_emul=0;

static FILE *err_msg;
static FILE *temp;
static FILE *temp2;

static char siirtop[100];
// static char plot_id[10];
static int plot_id;
static char layout[LNAME];
static char meta_name[LNAME];


static int wst;
static char mouse_file_name[LNAME];
static int show_picture=1;


static double t_start, t_end, t_step, t;

static int filltype;  /* 0=- 1=FILL 2=YFILL 3=OFILL 4=IFILL */
static int fill_step;
static double fill_start, fill_end;
static int x_fill, y_fill; /* fill-viivojen kiintopiste OFILL,IFILL */


static int nloop;
static int loopar[MAXLOOP];
static double loop_start[MAXLOOP], loop_end[MAXLOOP], loop_step[MAXLOOP];

static int data=0;     /* 26.5.92 */
static int nvar=0;
static char cur_data[LNAME];
static SURVO_DATA curd;
static int curd_var[MAXLOOP];
static int sp_ind[MAXLOOP];
static int lag[MAXLOOP];  /* 21.6.92 */
static long obs;

static int integral_ind;
static double integral_const, integral_value;
/*
extern int ncfpar;
extern char cfnimet[];
extern char *cfparnimi[];
extern double cfpar[];
*/
static int out;
static char color_change[LLENGTH];
static int color_max;

static int lopetus=0;
static int kosketus=0;

static int n_mess=0;
static char c_message[N_MESS][16],c_text[N_MESS][32];
static int c_step[N_MESS],c_x[N_MESS],c_y[N_MESS],c_i[N_MESS];

static char curve_attr[LNAME];



static double char_pitch, char_height, char_width;
static int marker_type, marker_size;   /* from POINT-specification */
static double xdiv1,xdiv2,xdiv3;
static double ydiv1,ydiv2,ydiv3;
static int x_kuva, y_kuva;
static double kirjainlev, kirjainkork;
static int tikki;              /* tick-viivan pituus (min. viiva tai raon koko) */
static int scalespace=SCALESPACE;
static char xscales[SCALESPACE], *xscal[NPAR];
static double xscaleval[NPAR];
static int xscalen;            /* skaala-arvojen lkm */
static char yscales[SCALESPACE], *yscal[NPAR];
static double yscaleval[NPAR];
static int yscalen;            /* skaala-arvojen lkm */
static int frametype;          /* 0,1,2 */
static int shadeval[SHADEMAX], shadepull[SHADEMAX], shadecolor[SHADEMAX];
static unsigned char code[512];
static char *pen_code;         /* PEN=pen_code */
static char *line_code;        /* LINETYPE=line_code */

static char muuttujanimi[LLENGTH];
static  char xlauseke[LLENGTH], ylauseke[LLENGTH];
static  int cfunktio; /* 1=C-kielinen 0=tulkattava */
static  int integral_function;

static  double xmin,xmax,ymin,ymax,xmumin,xmumax,ymumin,ymumax;
static  char xmuunnos[LLENGTH], ymuunnos[LLENGTH];

// int aika=0; /* psc2.c */
// char *argv1;



static int videomode;
static int y_const;

static int gg_char;
static int gg_marker_type,gg_marker_size;
static int pieborder=0;

static int x_origin,y_origin;
static int char_color=0;
static int line_type=0;
static int line_width=1;
static int line_color=0;
static int background=1;
static int mark_type=-1, mark_size=1 ,mark_color=2;
static int fill_interior=1, fill_color=0, fill_style=0;
static int fonts_on;
static char font_type[64];
static double ycharwidth=0.0;  // muita varten
static int font_weight=400;
static int font_italic=0;

static int x_move=0; // 7.6.2002
static int y_move=0;
static int rotation=0;

static double autom_color=0.0; // 16.11.2002

static int overlay;
static int alaviivat_pois=1;

static int line_slow=0; // 31.3.2010

static char pr_tila[6000];   /* koodijonot ja -sanat */
static char *pr_sana[300];   /* koodisanojen osoittimet */
static char *pr_koodi[300];  /* koodisanoja vastaavien koodijonojen osoittimet */
static int n_sana;           /* koodisanojen lukumÑÑrÑ */
static char *pr_osoitin;     /* ens. vapaan paikan osoitin pr_tilassa */
static int pr_type;          /* 1=PS, 0=muu */

static double yscalepos1,yscalepos2;
static int scalemove_x,scalemove_y; /* 12.2.1993 */
static int tickturn;  /* 24.9.1993 */
static int pyramid=0; // 18.10.2005

static char framecode[LLENGTH];

static char *shadow[256];    /* varjorivin merkkien koodisanaosoittimet */
static char *shadow2[256];   /* varjorivin merkkien jÑlkikoodisanaosoittimet */


static int markermax=12;
static int shademax=7;   /*   SHADEMAX=16 */
static int shade[]={ 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15 };

/*
long color[256]={0L, 4144959L, 41L, 10496L, 2686976L, 16191L, 2697472L, 2687017,
   2434341L, 4135193L, 1654553L, 1644825L, 4144921L, 1644863L, 9509L, 4135231L};
*/

static int g_color[256];
// int co[N_SURCOLORS]={ 0,15,4,2,1,14,3,5,7,9,10,8,11,12,6,13 };  /* 23.10.91 */


static int line_style[8] = { 0xffff, 0xfcfc, 0x9248, 0xe4e4, 0xf8f8, 0xf110, 0xcccc, 0xf248 };
/******************************************************************
                         0       1       2       3       4       5       6       7
1: 1111110011111100(2:hex)=FCFC
2: 1001001001001000(2:hex)=9248
3: 1110010011100100(2:hex)=E4E4
4: 1111100011111000(2:hex)=F8F8
5: 1111000100010000(2:hex)=F110
6: 1100110011001100(2:hex)=CCCC
7: 1111001001001000(2:hex)=F248
 PostScript: (yhdenmukaistus)
0{ [LB][RB] 0 setdash }[LF]  / solid
1{ [LB]6 2[RB] 0 setdash }[LF] / long dash
2{ [LB]1 2[RB] 0 setdash }[LF] / dotted
3{ [LB]4 2 1 2[RB] 0 setdash }[LF] / dash dotted
4{ [LB]4 2[RB] 0 setdash }[LF] / medium dash
5{ [LB]4 2 1 2 1 2[RB] 0 setdash }[LF] / dash with two dots
6{ [LB]2 1[RB] 0 setdash }[LF] / short dash
7{ [LB]4 2 1 2 1 2 1 2[RB] 0 setdash }[LF] / dash with three dots
*******************************************************************/


static int vari[N_STOCK_COLOR][3]= {
        {  0,  0,  0},
        {255,255,255},
        {170,  0,  0},
        {  0,170,  0},
        {  0,  0,170},
        {255,255,  0},
        {  0,170,170},
        {170,  0,170},
        {153,153,153},
        {102,102,255},
        {102,255,102},
        {102,102,102},
        {102,255,255},
        {255,102,102},
        {153,153,  0},
        {255,102,255}};

static int vari2[3]; // neg. fill_colors
static int vari3[3]; // VALUES,LABELS text colors 16.9.2010




// RS REM HPEN hPens[NPEN];
static int pen_line_type[NPEN],pen_line_width[NPEN],pen_line_color[NPEN];
static int n_pens=0;


// RS REM HBRUSH hBrushes[NBRUSH];
static int brush_color[NBRUSH];
static int n_brushes=0;

static int stock_pen=0;
static int valittu_rgb; // RS CHA DWORD -> int

static char marker_rot_variable[16]; // 3.9.2010 kÑytîssÑ vain PS-puolella
static int marker_rot_var;
static double marker_rot_angle;
static int arrowlen;

static FILE *muste_outfile;
static int muste_outfile_error=FALSE;

/*  *  *  *  *  *  *  */

static void muste_send(char *s)
	{
	fprintf(muste_outfile,"%s\n",s);
	
	if (!muste_outfile_error && ferror(muste_outfile))
    	{
    	muste_outfile_error=TRUE;
    	PR_EBLD; sur_print("\nCannot write GPLOT outfile!");
        WAIT; return;
        }
	}

static int muste_close_outfile(char *perm_outfile)
	{	
	if (muste_outfile!=NULL) fclose(muste_outfile); 
	muste_outfile_error=FALSE;
	
	if (perm_outfile[0]!='-') sur_copy_file(meta_name,perm_outfile);
//	sur_delete(meta_name);


	return 1;
	}

static int muste_open_outfile(char *s)
	{
	muste_outfile=muste_fopen(s,"wt");
	if (muste_outfile==NULL)
		{
		sprintf(sbuf,"\nCannot open GPLOT outfile!");
		PR_EBLD; sur_print(sbuf);
		WAIT; return(-1);
		}
	muste_outfile_error=FALSE;	
	return 1;
	}

static int muste_line(int x1,int y1,int x2,int y2)
	{
	double xkerroin,ykerroin;
	xkerroin=(double)((double)x_wsize/(double)x_size);
	ykerroin=(double)((double)y_wsize/(double)y_size);
	
//Rprintf("\nlineto, x_size: %d, x_wsize: %d, xkerroin: %g, ykerroin: %g",x_size,x_wsize,xkerroin,ykerroin);	
	muste_line_plot(plot_id,xkerroin*(double)x1,ykerroin*(double)y1,xkerroin*(double)x2,ykerroin*(double)y2);
//	muste_line_plot(plot_id,x1,y1,x2,y2);
	
	sprintf(sbuf,"line %d %d %d %d",x1,y1,x2,y2);
	muste_send(sbuf);
	return (1);
	}

static int muste_rectangle(int x1,int y1,int x2,int y2)
	{
	muste_rectangle_plot(plot_id,(double)x1,(double)y1,(double)x2,(double)y2);
	
	sprintf(sbuf,"rectangle %d %d %d %d",x1,y1,x2,y2);
	muste_send(sbuf);
	return (1);
	}	
	
	
/*  *  *  *  *  *  *  */	


static int p_text(char *text,int x1,int y1,int i)
	{

//Rprintf("\np_text, text: %s",text);
    if (*text==EOS) return(1);

//	strcpy(teksti,text);

//    muste_text_plot(plot_id,(double)x1,(double)y1,text);
    muste_text_plot(plot_id,(double)((int)x1+(int)x_move),(double)((int)y_const-(int)y_move-(int)y1-(int)char_height),text);
    
//	sprintf(sbuf,"text %d %d \"%s\"",x1,y1,text);	
//	Rprintf("\ntext1 %d \"%s\"",x1+x_move,text);
//	Rprintf("\ntext2 %d \"%s\"",(int)y_const-(int)y_move-(int)y1-(int)char_height,text);

	sprintf(sbuf,"text %d %d \"%s\"",(int)x1+(int)x_move,(int)y_const-(int)y_move-(int)y1-(int)char_height,text);
	muste_send(sbuf);


//	muste_fixme("\nFIXME: gplot p_text() not implemented!");
//	TextOut(hdcMeta,x1+x_move,y_const-y_move-y1-(int)(1.00*char_height),text,strlen(text));

	return(1);
	}

static void muste_moveto(int x,int y)
	{
	muste_xpos=x;
	muste_ypos=y;	
	}

static void muste_lineto(int x, int y)
	{
//Rprintf("\nlineto, x_size: %d, x_wsize: %d, xkerroin: %g, ykerroin: %g",x_size,x_wsize,xkerroin,ykerroin);	
	muste_line(muste_xpos,muste_ypos,x,y);
	muste_moveto(x,y);	
	}



static int p_square2(int x,int y,int wx)
        {
        int i,y0;
        y0=y_const-y;
        i=wx/4; if (i<1) i=1;
        muste_rectangle(x-i,y0-i,x+i,y0+i);
//        Rectangle(hdcMeta,x-i,y0-i,x+i,y0+i);
        return(1);
        }


static int p_square(int x,int y,int wx)
        {
        static int old_linewidth=0;
        static int old_color=-999;
        static int xv=-9999;
        static int yv=-9999;

        int ix,iy,wy,x0,y0;
        int dx,dy;

        wx=0.5*wx;
        wy=y_ratio*wx;
        if (wx<2) wx=2; if (wy<2) wy=2;
        x0=x-wx/2; y0=y_const-y-wy/2;
        dx=x0-xv; dy=y0-yv;

        if (wx!=old_linewidth || line_color!=old_color
               || dx>1 || dx<-1 || dy>1 || dy<-1)
            {
            for (iy=0; iy<wy; ++iy)
                { muste_moveto(x0,y0+iy); muste_lineto(x0+wx-1,y0+iy); }
            old_linewidth=wx; old_color=line_color;
            }
        else
            {
            if (dx==1)
                {
                if (dy==1)
                 {muste_moveto(xv+1,yv+wy);muste_lineto(xv+wx,yv+wy);muste_lineto(xv+wx,yv+1); }
                else if (dy==-1)
                 {muste_moveto(xv+1,yv-1);muste_lineto(xv+wx,yv-1);muste_lineto(xv+wx,yv+wy-2); }
                else
                 {muste_moveto(xv+wx,yv);muste_lineto(xv+wx,yv+wy-1);}
                }
            else if (dx==-1)
                {
                if (dy==1)
                 {muste_moveto(xv-1,yv+1);muste_lineto(xv-1,yv+wy);muste_lineto(xv+wx-2,yv+wy); }
                else if (dy==-1)
                 {muste_moveto(xv-1,yv+wy-2);muste_lineto(xv-1,yv-1);muste_lineto(xv+wx-2,yv-1); }
                else
                 {muste_moveto(xv-1,yv);muste_lineto(xv-1,yv+wy-1);}
                }
            else
                {
                if (dy==1)
                 {muste_moveto(xv,yv+wy);muste_lineto(xv+wx-1,yv+wy); }
                else if (dy==-1)
                 {muste_moveto(xv,yv-1);muste_lineto(xv+wx-1,yv-1); }
                }
            }
        xv=x0; yv=y0;
        return(1);
        }


static int p_line2(int x1,int y1,int x2,int y2,int i)  /* line from (x1,y1) to (x2,y2) */
/* int i;    attribute index */
        {

        int len,k;
        double x,y,xd,yd;
        int h,linebit[16],ibit,ic;

        if (line_type==0)
            {
// BeginPath(hdcMeta);
            muste_moveto(x1,y_const-y1);
            muste_lineto(x2,y_const-y2);
// EndPath(hdcMeta);
// StrokePath(hdcMeta);
            }

        else if (x1!=x2 || y1!=y2)
            {
            len=x2-x1; if (len<0) len=-len;
            k=y2-y1; if (k<0) k=-k;
            if (k>len) len=k;
            xd=(double)(x2-x1)/len; yd=(double)(y2-y1)/len;
            x=x1+0.5; y=y1+0.5;

            if (line_type==8) // yhtenÑinen viiva square-tekniikalla!
                {
                for (k=0; k<len; ++k)
                    {
                    p_square2((int)x,(int)y,line_width);
                    x+=xd; y+=yd;
                    }
                }
            else
                {
                k=line_style[line_type];
                for (i=0; i<16; ++i)
                    {
                    h=(k>>1)<<1; if (k==h) linebit[15-i]=0; else linebit[15-i]=1;
                    k=k>>1;
                    }
                ibit=0; ic=0;
                for (k=0; k<len; ++k)
                    {
                    if (linebit[ibit])
                        {
                        p_square((int)x,(int)y,line_width);
                        }
                    x+=xd; y+=yd;
                    ++ic; if (ic==line_width) { ic=0; ++ibit; if (ibit==16) ibit=0; }
                    }
                }
            }

        muste_moveto(x_pos,y_const-y_pos);
        x_pos=x2; y_pos=y2;

        return(1);
        }


static int p_line(int x2,int y2,int i)     /* line from (x_pos,y_pos) to (x2,y2)  */
/* int i;    attribute index */
        {
        if (line_type!=0) return(p_line2(x_pos,y_pos,x2,y2,i));
        if (line_slow==0)
            {
            muste_moveto(x_pos,y_const-y_pos);
            muste_lineto(x2,y_const-y2);
            }

        else for (i=0; i<line_slow ; ++i) // 31.3.2010
            {
            muste_moveto(x_pos,y_const-y_pos);
            muste_lineto(x2,y_const-y2);
            }

        x_pos=x2; y_pos=y2;
        return(1);
        }


static int p_error(char *s)
    {
    char perror[LLENGTH];
/* RS CHA
    char name[LNAME];
    extern char sur_session[];

    sprintf(name,"%s%sERR.TMP",etmpd,sur_session);
    err_msg=muste_fopen(name,"wt");
    fprintf(err_msg,"GPLOT error: %s",s);
    fclose(err_msg);
*/
    sprintf(perror,"\nGPLOT error: %s",s);
	sur_print(perror);
	WAIT;
    
// RS FIXME Check that exit is OK   ExitProcess(0);
    return(-1);
    }

/* RS REM
static int p_error2(char *s)
 { return(p_error(s)); }
 */

static void sp_virhe(char *a,char *b)
        {
        sprintf(sbuf,"\nError in specification %s=%s",a,b);
        p_error(sbuf); // RS CHA p_error2 -> p_error
//        WAIT;
        }

static void f_tuntematon(char *s)
        {
//      printf("\nUnknown function %s\n",s);
        sprintf(sbuf,"Unknown function %s",s);
        p_error(sbuf);
        l_virhe=1;
        }

static void arg_virhe(char *s)
        {
//      printf("\n%s: Error in arguments\n",s);
        sprintf(sbuf,"%s: Error in arguments",s);
        p_error(sbuf);
        l_virhe=1;
        }

static void syntax_error(char *s)
        {
//      printf("\nsyntax error in %s\n",s);
        sprintf(sbuf,"syntax error in %s",s);
        p_error(sbuf);
        l_virhe=1;
        }


static int laske(); // RS ADD declaration
static double arit_atof();
static int arit_atoi();

static int find_datapar(int i)
        {
        int k;
        char x[LLENGTH], *sana[2];
        char *p;

        sp_ind[nvar]=i;
        strcpy(x,spb[i]);
        k=split(x,sana,2);

        if (!data)
            {
            p=strchr(sana[0],':');
            k=data_read_open(p+1,&curd);
            if (k<0) exit(1);
            k=conditions(&curd); if (k<0) exit(1);
            obs=curd.l1;
            while (obs<=curd.l2 && unsuitable(&curd,obs)) ++obs;
            data=1;
            loopar[nloop]=-1;
            ++nloop;
            }

        lag[nvar]=0;
        p=strchr(sana[1],'[');
        if (p!=NULL) { *p=EOS; lag[nvar]=atoi(p+1); }
        k=varfind(&curd,sana[1]); if (k<0) exit(1);
        curd_var[nvar]=k;
        i=data_load(&curd,obs+(long)lag[nvar],curd_var[nvar],&arvo[sp_ind[nvar]]);
        ++nvar;
        return(i);
        }







static int read_loopar(int i)
        {
//        extern double arit_atof();
        int k;
        char x[LLENGTH], *sana[3];
        char *p,*q;
        int ok;

// printf("\nread_loopar=%d",nloop); getch();
// printf("\nspb[i]=%s|",spb[i]); getch();

        if (nloop==MAXLOOP)
            {
//          printf("\nToo many (>%d) loop parameters!",MAXLOOP);
            sprintf(sbuf,"Too many (>%d) loop parameters!",MAXLOOP);
            p_error(sbuf);
            WAIT; return(-1);
            }
        loopar[nloop]=i;
        strcpy(x,spb[i]);

        k=split(x,sana,3);
        if (strncmp(sana[0],"DATA:",5)==0)  /* 26.5.1992 */
            {
            k=find_datapar(i);
            spb[i]=spb2[i]=NULL;
  /*        data_load(&curd,obs,curd_var[i],&arvo[loopar[nloop]]);
            ++nloop;
  */
            return(k);
            }

        if (k<3)
            {
            ok=0;
            p=strchr(x,'(');
            if (p!=NULL)
                {
                *p=EOS; ++p;
                q=strchr(p,')');
                if (q!=NULL)
                    {
                    *q=EOS; ++q;
                    sana[0]=x; sana[1]=q; sana[2]=p; ok=1;
                    }
                }

//          sprintf(sbuf,"Loop parameter in form: %s=<start>,<end>,<step>"
//                      ,spa[i]);
            if (!ok)
                {
                sprintf(sbuf,"Syntax error in %s",spb[i]);
                p_error(sbuf);
                WAIT; return(-1);
                }
            }
        loop_start[nloop]=arit_atof(sana[0]);
        loop_end[nloop]=arit_atof(sana[1]);
        loop_step[nloop]=arit_atof(sana[2]);

        spb[i]=spb2[i]=NULL;
        arvo[i]=loop_start[nloop];
        ++nloop;
        return(1);
        }




static int laske2(char *muuttuja,double *y)
        {
        int i,k;

        i=spfind(muuttuja);
        
//Rprintf("\nlaske2, muuttuja: \"%s\", i: %d, spb[%d]: \"%s\"",muuttuja,i,i,spb[i]);       
        if (i<0)
            {
//          printf("\nParameter %s not found!\n",muuttuja);
//          getch();
            sprintf(sbuf,"Parameter %s not found!",muuttuja);
            p_error(sbuf);

            l_virhe=1; return(-1);
            }
        if (spb[i]==NULL) { *y=arvo[i]; return(1); }
        k=laske(spb[i],y);
        if (k==2) { k=read_loopar(i);
                    if (k<0) { l_virhe=1; return(-1); } *y=arvo[i];
                    return(1);
                  }
        arvo[i]=*y;
        spb[i]=NULL;
        return(1);
        }


static double oper(double x1,double x2,char laji)
        {
// RS REM        extern double power();

        if (x1==MISSING8 || x2==MISSING8) return(MISSING8);
        switch (laji)
            {
          case '+':
            return(x1+x2);
          case '-':
            return(x1-x2);
          case '*':
            return(x1*x2);
          case '/':
            if (x2==0.0) { l_virhe=1; return(0.0); }
            return(x1/x2);
          case '^':
            return(muste_pow(x1,x2));
            }
        return(0.0);
        }

static double funktio(char *s,double x)
        {
        int i;
        double y;
        char S[32];

        if (*s==EOS) return(x);
        if (x==MISSING8) return(x);
        strncpy(S,s,31); S[31]=EOS; muste_strupr(S);

        if (strncmp(S,"SQR",3)==0) return(muste_sqrt(x));
        if (strcmp(S,"LOG")==0) return(muste_log(x));
        if (strcmp(S,"EXP")==0) return(muste_exp(x));
        if (strcmp(S,"SIN")==0) return(muste_sin(x));
        if (strcmp(S,"COS")==0) return(muste_cos(x));
        if (strcmp(S,"TAN")==0) return(muste_tan(x));
        if (strcmp(S,"ATN")==0 || strcmp(S,"ARCTAN")==0) return(muste_atan(x));
        if (strcmp(S,"ARCSIN")==0) return(muste_asin(x));
        if (strcmp(S,"ARCCOS")==0) return(muste_acos(x));
        if (strcmp(S,"ABS")==0) return(muste_fabs(x));
        if (strcmp(S,"INT")==0) return(muste_floor(x));
        if (strcmp(S,"PROBIT")==0) return(probit(x));
        if (strcmp(S,"FACT")==0) return(fact(x));
        if (strcmp(S,"LFACT")==0) return(lfact(x));

/*      i=f_tiedosto(s,&x,1,&y); if (i>0) return(y);
*/
        f_tuntematon(s);
        l_virhe=1;
        return(x);
        }

static double mfunktio(char *s,double *x,int n)
        {
        int i;
        double y;
        char S[32];

/*     printf("\nmfunktio: ");
     for (i=0; i<n; ++i) printf("%g ",x[i]); getch();
*/
        strncpy(S,s,31); S[31]=EOS; muste_strupr(S);

        if (strcmp(S,"MAX")==0)
            {
            y=x[0];
            for (i=1; i<n; ++i) y=(x[i]>y)? (x[i]):(y);
            return(y);
            }
        if (strcmp(S,"MIN")==0)
            {
            y=x[0];
            for (i=1; i<n; ++i) y=(x[i]<y)? (x[i]):(y);
            return(y);
            }
        if (strcmp(s,"C")==0)
            {
            double u,v;
            int iu,iv;

            if (n!=2) { arg_virhe(s); }
            iv=v=x[0]; iu=u=x[1];
            if ((double)iu!=u) return(0.0);
            if ((double)iv!=v) return(0.0);
            if (u>v/2) u=v-u;
            if (u<0 || v<0) return(0.0);
            if (u==0.0) return(1.0);
            y=1.0;
            for (; u>0; --u, --v) y*=(v/u);
            return(y);
            }
/***************************************
        if (strcmp(S,"GCD")==0)
            {
            extern double gcd();

            return (gcd(x[0],x[1]));
            }
****************************************/
        if (strcmp(S,"MOD")==0)
            {
            return((double)((unsigned long)x[0]%(unsigned long)x[1]));
            }
/***********************************************
        if (strcmp(S,"ROOT")==0)
            {
            extern double root();
            return (root(x[0],x[1]));
            }
        if (strcmp(S,"X")==0)
            {
            extern double ed_number();
            return (ed_number(x[0],x[1]));
            }

        if (*s=='M' && strncmp(s,"MAT_",4)==0)
            {
            mat_function(s+4,str_opnd,n,&y);
            return(y);
            }

        i=f_edit(s,x,n,&y); if (i>0) return(y);
        i=f_tiedosto(s,x,n,&y);
        if (i>0 && y!=MISSING8) return(y);
*************************************************/
        l_virhe=1;
        return(x[0]);
        }


static void supista(int *t,double opnd[],char op[],int v[])
        {

        while (*t>1)
            {
            if (v[*t-1]>v[*t-2]) return;
            opnd[*t-2]=oper(opnd[*t-2],opnd[*t-1],op[*t-2]);
            op[*t-2]=op[*t-1]; v[*t-2]=v[*t-1];
            --(*t);
            }
        }


static double luku(char *sana,int len)
        {
        char *p;
        double tulos=1.0;
        int i;

        sana[len]=EOS;
        p=sana; if (*p=='-') ++p;
        if (strchr("1234567890.",*p)==NULL)
            {
            i=laske2(p,&tulos); if (i<0) return((double)1.0);
            if (*sana=='-') return(-tulos);
            return(tulos);
            }
        return(atof(sana));
        }

static void if_syntax_error(char *x)
        {
//      printf("\nSyntax error in %s\n",x);
        sprintf(sbuf,"Syntax error in %s",x);
        p_error(sbuf);
        WAIT; l_virhe=1;
        }

static int varif(char *lauseke,double *y)
        {
        char *a,*b,*c,*d;
        char rel;
        char *p;
        int sulut;
        char x[LLENGTH];
        double y1;
        int tosi;

/*      printf("\nvarif: %s",lauseke); getch();     */
        /* if(<a><rel><b>)then(<c>)else(<d>)
           <a>,<b>,<c>,<d> lausekkeita
           <rel>: =,>,<,<>,>=,<=
                  = > < E  S  P
        */

        strcpy(x,lauseke);
        a=x+3;  /* if( ohitetaan */
        p=a; sulut=0;
        while (*p)
            {
            switch(*p)
                {
              case '=':
                rel=*p; *p=EOS; break;
              case '<':

                if (*(p+1)=='=') { rel='P'; *p=EOS; ++p; *p=EOS; break; }
                if (*(p+1)=='>') { rel='E'; *p=EOS; ++p; *p=EOS; break; }
                rel=*p; *p=EOS; break;
              case '>':
                if (*(p+1)=='=') { rel='S'; *p=EOS; ++p; *p=EOS; break; }
                rel=*p; *p=EOS; break;
              case ')':
                --sulut; ++p;
                if (sulut<0)
                    {
//                  printf("\nrelation symbol =<> missing! in %s\n",x);
                    sprintf(sbuf,"relation symbol =<> missing! in %s",x);
                    p_error(sbuf);
                    WAIT; l_virhe=1; return(-1);
                    }
                break;
              case '(':
                ++sulut; ++p;
                break;
              default:
                ++p;
                }
            }

/*  printf("\na=%s rel=%c",a,rel);      */
        b=p+1;
        p=b;
        while (1)
            {
            p=strchr(p,')');
            if (p==NULL) { if_syntax_error(lauseke); return(-1); }
            if (strncmp(p,")then(",6)==0) { *p=EOS; break; }
            ++p;
            }
/*  printf(" b=%s",b);  */
        c=p+6;
        p=c; sulut=0;
        while (*p)
            {
            if (*p=='(') { ++sulut; ++p; continue; }
            if (*p==')')
                {
                if (!sulut) break;
                --sulut;
                }
            ++p;
            }
        if (*p==EOS) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;
        if (strncmp(p+1,"else(",5)!=0) { if_syntax_error(lauseke); return(-1); }
        d=p+6;
        p=d; sulut=0;
        while (*p)
            {
            if (*p=='(') { ++sulut; ++p; continue; }
            if (*p==')')
                {
                if (!sulut) break;
                --sulut;
                }
            ++p;
            }
        if (*p==EOS) { if_syntax_error(lauseke); return(-1); }
        *p=EOS;
/* printf(" c=%s d=%s",c,d);
getch();
*/
        laske(a,y);
        laske(b,&y1);
        tosi=0;
        switch (rel)
            {
          case '=': if (*y==y1) tosi=1; break;
          case '<': if (*y<y1) tosi=1; break;
          case '>': if (*y>y1) tosi=1; break;
          case 'E': if (*y!=y1) tosi=1; break;
          case 'P': if (*y<=y1) tosi=1; break;
          case 'S': if (*y>=y1) tosi=1; break;
            }

        if (tosi) laske(c,y);
        else      laske(d,y);
        return(1);
        }



static int laske(char *lauseke,double *y)
        {
/* RE REM        
        double luku();
        double oper();
        double funktio();
        double mfunktio();
*/        

        char x[MAXPITUUS];
        char *p,*q;
        char sana[32];
        int len;
        double opnd[MAXARG+4]; char op[MAXARG+4]; int v[MAXARG+4];
        int t,n;
        int narg; /* Usean muuttujan funktion argumenttien lkm     */
        int i;




        if (*lauseke=='i')
            {
            if (strncmp(lauseke,"if(",3)==0)
                return(varif(lauseke,y));
            }

        if (*lauseke=='-')  /* 11.2.91 */
            {
            *x='0'; strcpy(x+1,lauseke);
            }
        else strcpy(x,lauseke);
/*      strcpy(x,lauseke);       */
        len=0;
        p=x;
        t=0;


        while (*p)
            {
//Rprintf("\nlaske p: \"%s\",len: %d",p,len);

            
            if (l_virhe) return(-1);
            switch (*p)
                {
              case '+':
                if (len==0) { ++p; break; }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='+'; v[t++]=1;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '-':
                if (len==0) { sana[len++]=*p; ++p; break; }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='-'; v[t++]=1;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '*':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='*'; v[t++]=2;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '/':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='/'; v[t++]=2;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case '^':
                if (len==0) { syntax_error(lauseke); return(-1); }
                if (len>0) opnd[t]=luku(sana,len); len=0;
                op[t]='^'; v[t++]=3;
                supista(&t,opnd,op,v);
                ++p;
                break;

              case ',':
                /* loop parameter */
                return(2);

              case '(':
                q=p+1;


//              if (*q==')') { printf("\nArguments missing in %s\n",lauseke);
//                             l_virhe=1; return(-1); }
                if (*q==')')
                    {
                    sprintf(sbuf,"Arguments missing in %s",lauseke);
                    p_error(sbuf);
                    }
                n=1;
                narg=1;
                while (n)
                    {
                    ++p;
                    if (*p=='(') { ++n; continue; }
                    if (*p==')') { --n; continue; }
//                  if (*p==EOS) { printf("\n) is missing in %s\n",lauseke);
//                                 l_virhe=1; return(-1); }
                    if (*p==EOS)
                        {
                        sprintf(sbuf,") is missing in %s",lauseke);
                        p_error(sbuf);
                        }
                    if (*p==',' && n==1)
                        {
                        *p=EOS;

                        laske(q,&opnd[t]);
                        ++t;
                        if (t>MAXARG+3)
                            { sur_print("\nToo many arguments in %s\n",lauseke); WAIT;
                              l_virhe=1; return(-1); }
                        ++narg;
                        q=p+1;
                        }

                    }
                if(strchr("+-*/^)\0",*(p+1))==NULL) { syntax_error(lauseke);
                                                      return(-1); }
                *p=EOS; ++p;
/*   printf("\nq=%s",q); getch();   */
                i=laske(q,&opnd[t]);
                if (i<0 || l_virhe) return(-1);
                if (i==2) { sur_print("\nret2\n"); WAIT; }

/*   printf("\ntulos1=%f",opnd[t]); getch();  */
                if (len==0) { len=-1; break; }
                sana[len]=EOS;

                if (narg>1)
                    {

//           printf("\nArgumentit: ");
//           for (i=t-narg+1; i<=t; ++i) printf(" %g",opnd[i]); getch();

                    t=t-narg+1;
                    if (*sana=='-')
                        opnd[t]=-mfunktio(sana+1,opnd+t,narg);
                    else
                        opnd[t]=mfunktio(sana,opnd+t,narg);
                    if (l_virhe) return(-1);
                    *sana=EOS;
                    len=-1;
                    break;
                    }

                /* Yhden muuttujan funktiot */
                if (*sana=='-')
                    opnd[t]=-funktio(sana+1,opnd[t]);
                else
                    opnd[t]=funktio(sana,opnd[t]);
                if (l_virhe) return(-1);
                len=-1;
                break;

              case ')':
//              printf("\n( missing in %s\n",lauseke); l_virhe=1; return(-1);
                sprintf(sbuf,"( missing in %s",lauseke);
                p_error(sbuf);
              case 'e': case 'E':
                if (strchr("+-.0123456789",sana[0])!=NULL)
                    {
                    sana[len++]=*p; ++p;
                    if (*p!='+' && *p!='-') break;
                    }
              default:
                /* tarkistukset puuttuvat */
                sana[len++]=*p;
                ++p;
//Rprintf("\ndefault p: \"%s\",len: %d",p,len);
                
                
                }
            }

        if (len<0) { v[t++]=0; }
        else
                   if (len>0) { opnd[t]=luku(sana,len); v[t++]=0; }

        supista(&t,opnd,op,v);
        *y=opnd[0];

        return(1);
        }


        
static double arit_atof(char *lauseke)
        {
//        extern int laske(); // RS ADD Suoraan editoriaalisesta aritmetiikasta
        double y;

// muste_fixme("FIXME: laske() for GPLOT directly from editorial artihmetics\n");
        laske(lauseke,&y);
        return(y);
        }

static int arit_atoi(char *lauseke)
        {
//        extern double arit_atof();
        return((int)arit_atof(lauseke));
        }
        

static int spfind2(char *s,int k)
        {
        int i;
        for (i=k; i<spn; ++i)
                if (strcmp(s,spa[i])==0) return(i);
        return(-1);
        }


static double xmu(double x)
        {
        double z;
        if (*xmuunnos==EOS) return(x);
        arvo[1]=x; laske(xmuunnos,&z);
        if (l_virhe) { *xmuunnos=EOS; return(x); }
        return(z);
        }

static double ymu(double x)
        {
        double z;
        if (*ymuunnos==EOS) return(x);
        arvo[2]=x; laske(ymuunnos,&z);
        if (l_virhe) { *ymuunnos=EOS; return(x); }
        return(z);
        }


static void xy_arvot(double t,double *px,double *py)
        {
        int i;

//Rprintf("\nEntering xy_arvot, t: %g, xlauseke: %s, ylauseke: %s",t,xlauseke,ylauseke);

        for (i=0; i<spn; ++i) spb[i]=spb2[i];
        arvo[0]=t;
        
        
        if (*xlauseke==EOS) *px=t; else laske(xlauseke,px);
        
//Rprintf("\nxy_arvot, t: %g,px: %g,ylauseke: %s",t,*px,ylauseke);        
        laske(ylauseke,py);
        
//Rprintf("\nxy_arvot, t: %g,px: %g,py: %g",t,*px,*py);        
        }

static void outline(double xs,double ys,double xu,double yu,int *px,int *py)
/* xs,ys;  sisÑpiste */
/* xu,yu;  ulkopiste */
/* *px,*py;  rajakoordinaatit */
        {
        double xsmu,ysmu,xumu,yumu;
        double x1,y1,t1,t2;

        xsmu=xmu(xs); ysmu=ymu(ys); xumu=xmu(xu); yumu=ymu(yu);
        if (xsmu==xumu)
            {
            x1=xsmu;
            if (yumu<ymumin) y1=ymumin;
            else y1=ymumax;
            }
        else if (ysmu==yumu)
            {
            y1=ysmu;
            if (xumu<xmumin) x1=xmumin;
            else x1=xmumax;
            }
        else
            {
            t1=(xmumin-xsmu)/(xumu-xsmu); if (t1<0 || t1>1) t1=1;
            t2=(xmumax-xsmu)/(xumu-xsmu); if (t2<0 || t2>1) t2=1;
            if (t2<t1) t1=t2;
            t2=(ymumin-ysmu)/(yumu-ysmu); if (t2<0 || t2>1) t2=1;
            if (t2<t1) t1=t2;
            t2=(ymumax-ysmu)/(yumu-ysmu); if (t2<0 || t2>1) t2=1;
            if (t2<t1) t1=t2;
            x1=xsmu+(xumu-xsmu)*t1;
            y1=ysmu+(yumu-ysmu)*t1;
            }
        *px=xx+x_kuva*(x1-xmumin)/(xmumax-xmumin);
        *py=yy+y_kuva*(y1-ymumin)/(ymumax-ymumin);
        }

static int coord(double t,int *px,int *py)
        {
        static double x,y,x1,y1,s;
        static double xv,yv;

        xy_arvot(t,&x,&y);

        if (integral_function)
            {
            if (t==t_start)
                {
                x1=x;
                y1=y;
                s=y=0.0;
                }
            else
                {
                s+=(x-x1)*(y+y1)/2.0;
                x1=x;
                y1=y;
                y=s;
                }
            }
        if (integral_ind) y*=integral_const/integral_value;
        if (x<xmin || x>xmax || y<ymin || y>ymax)
            {
            double xq,yq;

            if (!out)
                {
                outline(xv,yv,x,y,px,py); out=1; xv=x; yv=y;
                return(1);
                }


   /* fillien takia */
            xq=x; yq=y;
            if (xq<xmin) xq=xmin;
            if (xq>xmax) xq=xmax;
            if (yq<ymin) yq=ymin;
            if (yq>ymax) yq=ymax;
            x_pos=(int)(xx+x_kuva*(xmu(xq)-xmumin)/(xmumax-xmumin));
            y_pos=(int)(yy+y_kuva*(ymu(yq)-ymumin)/(ymumax-ymumin));

            out=1; xv=x; yv=y; return(0);
            }
        if (out)
            {
            if (out==1)   /* out=2 t_start */
                {
                outline(x,y,xv,yv,&x_pos,&y_pos);
                }
            }
        xv=x; yv=y; out=0;
//Rprintf("\ncoord2: xx: %d, yy: %d, x_kuva: %d, y_kuva: %d,\n xmuunnos: %s, ymuunnos: %s, xmumax: %g, xmumin: %g,\n ymumax %g, ymumin %g, x: %g, y: %g",xx,yy,x_kuva,y_kuva,xmuunnos,ymuunnos,xmumax,xmumin,ymumax,ymumin,x,y);
        
        *px=(int)(xx+x_kuva*(xmu(x)-xmumin)/(xmumax-xmumin));
        *py=(int)(yy+y_kuva*(ymu(y)-ymumin)/(ymumax-ymumin));
        return(1);
        }

static int etsi_loppusulku(char *x,char **pp)
        {
        int sulut=1;
        char *q;

        *pp=x;
        if (**pp=='[')
            {
            while (1)
                {
                q=strchr(*pp,']');
                if (q==NULL)
                    {
                    sprintf(sbuf,"] missing in %s!",x); p_error(sbuf);
//                  printf("\n] missing in %s!\n",x); WAIT; return(-1);
                    }
                *pp=q+1;
                if (*(q+1)!='[') break;
                }
            return(1);
            }
        if (**pp!='(') return (1);
        ++(*pp);
        while (**pp)
            {
          if (**pp=='(') { ++sulut; ++(*pp); continue; }
          if (**pp==')') { --sulut; ++(*pp);
                           if (sulut==0) break; else continue; }
          ++(*pp);
            }
        if (sulut)
            {
            sprintf(sbuf,"Syntax error in %s!",x); p_error(sbuf);
//          printf("\nSyntax error in %s\n",x);
            WAIT; return(-1);
            }
        return(1);
        }
        
	
static int p_text2(unsigned char *x,unsigned char *xs,int x1,int y1,int attr)
	{
// RS NYI	
	muste_fixme("\nFIXME: gplot p_text2() not implemented!");
	return(1);
	}	
        
static void koodivirhe(char *x)
        {
        PR_EBLD;
//      printf("\nErroneous code line/word:\n%s",x);
        sprintf(sbuf,"Invalid code line/word: %s",x);
        p_error(sbuf);
//        WAIT; 
        PR_ENRM;
        }

static int p_charcolor()
        {
        muste_fixme("\nFIXME: p_charcolor() not implemented!"); // RS FIXME
/* RS NYI       
        if (line_color>=0)
            SetTextColor(hdcMeta,
       RGB(vari[line_color][0],vari[line_color][1],vari[line_color][2]));
        else
            {
            crt_select_pen(line_color);
            SetTextColor(hdcMeta,
                  RGB(vari2[0],vari2[1],vari2[2]));
            }
*/            
        return(1);
        }

static int cmyk_to_rgb(double *cmyk,int *rgb)
    {
    int i;
    double a;
    int rgb_control;

    rgb_control=1;
    i=spfind("RGB"); if (i>=0) rgb_control=atoi(spb[i]);

  if (rgb_control==1)
    {
    for (i=0; i<3; ++i) { a=cmyk[i]+cmyk[3]; if (a>1.0) a=1.0;
                          rgb[i]=255*(1.0-a);
                        }
    }
  else if (rgb_control==2)
    {
    for (i=0; i<3; ++i) { a=(1.0-cmyk[i])*(1.0-cmyk[3]);
                          rgb[i]=255*a;
                        }
    }
  else
    {
    a=0;
    for (i=0; i<3; ++i) { cmyk[i]+=cmyk[3]; if (cmyk[i]>a) a=cmyk[i]; }
    if (a>1.0)
        for (i=0; i<3; ++i) cmyk[i]/=a;
    for (i=0; i<3; ++i) rgb[i]=255*(1.0-cmyk[i]);
    }

// fprintf(temp2,"cmyk_to_rgb: %d %d %d",rgb[0],rgb[1],rgb[2]);
    return(1);
    }



static int set_cmyk_color(char *y)  // 5.9.2004
    {
    int i;
    char *s[4];
    double cmyk[4];

// fprintf(temp2,"\ny=%s|",y);
    i=split(y,s,4);
    if (i<4)
        {
        p_error("Error in [color(c,m,y,k)]!");
        }
    for (i=0; i<4; ++i) cmyk[i]=atof(s[i]);
    cmyk_to_rgb(cmyk,vari2);
    valittu_rgb=RGB(vari2[0],vari2[1],vari2[2]);

/* RS NYI FIXME 
    hPens[n_pens]=CreatePen(PS_SOLID,line_width,valittu_rgb);
// fprintf(temp2,"\nhPens=%u",hPens[n_pens]);
    SelectObject(hdcMeta,hPens[n_pens]);
*/    
    
    line_color=-1; p_charcolor();
    ++n_pens;

    return(1);
    }


        
static int p_charsize()
        {

        kirjainkork=char_height;
        kirjainlev=char_width;
        return(1);
/******************************************************
        kirjainlev=8.0;
        if (char_height>11.0) { kirjainkork=14.0; gg_char=2; }
        else { kirjainkork=8.0; gg_char=1; }
*******************************************************/
//      return(1);
        }        

        
static int p_lineattr()
        {
        muste_fixme("\nFIXME: p_lineattr() not implemented!"); // RS FIXME
        
// RS NYI        crt_select_pen();

/******************
        _setcolor((int)g_color[line_color]);
        _setlinestyle(line_style[line_type]);
*************/
        return(1);
        }        

static int p_markattr()
        {
        return(1);
        }

static int p_fill_bar(int x1,int y1,int x2,int y2,int fill)
        {
        muste_fixme("\nFIXME: gplot p_fill_bar() not implemented!");
/* RS NYI        
        RECT rect;
        int i;

// fprintf(temp2,"\nrectangle: %d %d %d %d|",x1,y1,x2,y2);
//      if (x1<0 || y1<0 || x2>x_metasize || y2>y_metasize) return(1);
        if (x1<0) x1=0;
        if (y1<0) y1=0;
        if (x2>x_metasize) x2=x_metasize;
        if (y2>y_metasize) y2=x_metasize;
        SetRect(&rect,x1,y_const-y1+1,x2+1,y_const-y2);
        fill_color=fill;
        i=crt_select_brush();

        FillRect(hdcMeta,&rect,hBrushes[i]);
*/        
        return(1);
        }        


static int p_background()
        {

// fprintf(temp2,"\nbackground: %d,%d,%d,%d|",x_home,y_home,x_size,y_size);
        p_fill_bar(x_home,y_home,x_home+x_size,y_home+y_size,background);

/*******************
        if (overlay) return;
        _setcolor((int)g_color[background]);
        _rectangle(_GFILLINTERIOR,0,0,g_config.numxpixels,g_config.numypixels);
        _setcolor((int)g_color[char_color]);
*****************/
        return(1);
        }


static int muunna(); // RS DECLARATION

static int p_fillattr(int fill)
        {

        int i;
        char fword[LLENGTH];
        char y[3*LLENGTH];
        char *s[4];
        double cmyk[4];

        sprintf(fword,"COLOR(%d)",fill); // 27.8.2010
        i=spfind(fword);
        if (i>=0)
            {
            strcpy(y,spb[i]);
            i=split(y,s,4);
            if (i<4)
                {
                sprintf(sbuf,"Error in %s!",fword);
                p_error(sbuf);
                }
            for (i=0; i<4; ++i) cmyk[i]=atof(s[i]);
            cmyk_to_rgb(cmyk,vari2);
            return(2); // 28.8.2010
            }

        sprintf(fword,"[FILL%d]",fill);
        i=spfind(fword);
        if (i<0)
            {
            sprintf(fword,"FILL(%d)",fill);
            i=spfind(fword);
            }
        if (i>=0)
            {
            if (fill<0)
                {
                strcpy(y,spb[i]);

                if (muste_strnicmp(y,"FILL(",5)==0) // 23.11.2007 FILL(i)=FILL(j)
                    {
                    i=spfind(y);
                    if (i>=0) strcpy(y,spb[i]);
                    else p_error(y);
                    }

                i=split(y,s,4);
                if (i<4)
                    {
                    sprintf(sbuf,"Error in %s!",fword);
                    p_error(sbuf);
                    }
                for (i=0; i<4; ++i) cmyk[i]=atof(s[i]);
                cmyk_to_rgb(cmyk,vari2);
                return(1);
                }
            }
        if (i>=0) strcpy(fword,spb[i]);  // tarvitaanko?
        muunna(fword,y);                 // --"--

        return(1);
        }


static int crt_select_pen()
    {
    int i;
//  LOGBRUSH logbrush;    ENDCAP-koe 27.6.2000 ei onnistunut!

    if (stock_pen)
        {
        switch (stock_pen)
            {
          case 1: i=ANSI_FIXED_FONT; break;
          case 2: i=ANSI_VAR_FONT; break;
          case 3: i=DEFAULT_GUI_FONT; break;
          case 4: i=SYSTEM_FONT; break;
          default: i=ANSI_FIXED_FONT; break;
            }
// RS NYI FIXME        SelectObject(hdcMeta,GetStockObject(i));
        stock_pen=0;
        return(1);
        }

// fprintf(temp2,"\ncrt_select: n_pens=%d|",n_pens);

//  if (line_color<0) p_fillattr(line_color);  28.8.2010
    i=p_fillattr(line_color);
    if (i==2) line_color=-line_color; // 28.8.2010 kokeilu!

    for (i=0; i<n_pens; ++i)
        {
        if (pen_line_color[i]!=line_color) continue;
        if (pen_line_width[i]!=line_width) continue;
        if (pen_line_type[i]==line_type) break;
        }
    if (i<n_pens)
        {
// RS NYI FIXME         SelectObject(hdcMeta,hPens[i]);
        return(1);
        }

    if (n_pens>NPEN-1)
        {
        sprintf(sbuf,"Too many pens (max. %d)",NPEN);
        p_error(sbuf);
        }
    pen_line_color[n_pens]=line_color; // "uusi kynÑ"
    pen_line_width[n_pens]=line_width;
    pen_line_type[n_pens]=line_type;
 if (line_color>=0)
    {
    valittu_rgb=RGB(vari[line_color][0],vari[line_color][1],vari[line_color][2]);
// RS NYI FIXME     hPens[n_pens]=CreatePen(PS_SOLID,line_width,valittu_rgb);
    }

 else // line_color<0
    {
    valittu_rgb=RGB(vari2[0],vari2[1],vari2[2]);
// RS NYI FIXME     hPens[n_pens]=CreatePen(PS_SOLID,line_width,valittu_rgb);
    }
// fprintf(temp2,"\npen=%d hPen=%ld",n_pens,hPens[n_pens]);
// RS NYI FIXME     SelectObject(hdcMeta,hPens[n_pens]);
    ++n_pens;

    return(1);
    }

static int crt_delete_pens()
    {
    int i;
/************************************
fprintf(temp2,"\nDelete pens=%d",n_pens);
for (i=0; i<n_pens; ++i) fprintf(temp2,"\n%ld",hPens[i]);
fprintf(temp2,"\nDelete brushes=%d",n_brushes);
for (i=0; i<n_brushes; ++i) fprintf(temp2,"\n%ld",hBrushes[i]);
***************************************/
/* RS NYI FIXME 
    SelectObject(hdcMeta,GetStockObject(BLACK_PEN));
    for (i=0; i<n_pens; ++i) DeleteObject(hPens[i]);
    for (i=0; i<n_brushes; ++i) DeleteObject(hBrushes[i]);
*/    
    return(1);
    }

static int crt_select_brush()
    {
    int i;

// fprintf(temp2,"\nbrush_select: n_brushes=%d|",n_brushes);

//  if (fill_color<0)  28.8.2010
         i=p_fillattr(fill_color);
     if (i==2) fill_color=-fill_color;

    for (i=0; i<n_brushes; ++i)
        {
        if (brush_color[i]==fill_color) break;
        }
    if (i<n_brushes)
        {
// RS NYI FIXME         SelectObject(hdcMeta,hBrushes[i]);
        return(i);
        }

    if (n_brushes>NBRUSH-1)
        {
        sprintf(sbuf,"Too many fill colors (max. %d)",NBRUSH);
        p_error(sbuf);
        }
    brush_color[n_brushes]=fill_color;

/* RS NYI FIXME
 if (fill_color>=0)
     hBrushes[n_brushes]=CreateSolidBrush(RGB(vari[fill_color][0],vari[fill_color][1],vari[fill_color][2]));
 else
     hBrushes[n_brushes]=CreateSolidBrush(RGB(vari2[0],vari2[1],vari2[2]));
*/

// RS NYI FIXME     SelectObject(hdcMeta,hBrushes[n_brushes]);
    ++n_brushes;

    return(n_brushes-1);
    }


static int g_font_type()
    {
muste_fixme("\nFIXME: g_font_type() not implemented!");
 /*   
//  HANDLE hFont;
    TEXTMETRIC tm;
// fprintf(temp2,"\nrotation=%d|"); // 7.6.2002
    if (hFont2!=0) {
                    SelectObject(hdcMeta,GetStockObject(SYSTEM_FONT));
                    DeleteObject(hFont2);
                  }
    hFont2=CreateFont(
      (int)char_height,0,
      rotation,rotation, // 7.6.2002
      font_weight,
      font_italic,   // 1=italic
      FALSE,
      FALSE,
      ANSI_CHARSET,
      OUT_DEFAULT_PRECIS,
      CLIP_DEFAULT_PRECIS,
      PROOF_QUALITY,
      DEFAULT_PITCH,
      font_type
      );
    SelectObject(hdcMeta,hFont2);

    GetTextMetrics(hdcMeta,&tm);
    char_height=tm.tmHeight; char_width=1.2*tm.tmAveCharWidth;
*/
    return(1);
    }


static int p_special(char *s) /* tulkkaa laitetiedoston %-sanat */
        {

        char *p;
        char x[LLENGTH];

// fprintf(temp2,"\n*** s=%s|",s);


        strcpy(x,s);
        p=strchr(x,'=');
        if (p==NULL)
            {
            printf("\nError in %% code %s",x);
            WAIT; return(-1);
            }
        *p=EOS;
        ++p;
        if (strcmp(x,"char_width")==0) // ei kÑytîssÑ ??
            { char_width=atof(p); p_charsize(); return(1); }
        if (strcmp(x,"char_height")==0)
            { char_height=arit_atof(p); if (ps_emul) char_height*=4;
              p_charsize(); return(1);
            }
        if (strcmp(x,"char_color")==0)
            { char_color=line_color=atoi(p); p_charcolor(); p_lineattr(); return(1); }
        if (strcmp(x,"line_type")==0)
            { line_type=atoi(p); p_lineattr(); return(1); }
        if (strcmp(x,"line_width")==0)
            { line_width=atoi(p); if (ps_emul) line_width=4*atof(p);
              p_lineattr(); return(1);
            }
        if (strcmp(x,"line_color")==0)
            { line_color=atoi(p); p_lineattr(); return(1); }
        if (strcmp(x,"background")==0)
            { background=atoi(p); p_background(); return(1); }
        if (strcmp(x,"marker_type")==0)
            { mark_type=atoi(p); p_markattr(); return(1); }
        if (strcmp(x,"marker_size")==0)
            { mark_size=atoi(p); p_markattr(); return(1); }
        if (strcmp(x,"marker_color")==0)
            { mark_color=atoi(p); p_markattr(); return(1); }
        if (strcmp(x,"fill_interior")==0)
            { fill_interior=atoi(p); return(1); }
        if (strcmp(x,"fill_color")==0)
            { fill_color=atoi(p); crt_select_brush(); return(1); }
        if (strcmp(x,"fill_style")==0)
            { fill_style=atoi(p); return(1); }
        if (strcmp(x,"fonts_on")==0)
            { fonts_on=atoi(p); return(1); }
        if (strcmp(x,"font_type")==0)
            {
            if (strcmp(p,"same")!=0)
                {
                strcpy(font_type,p);
                p=font_type; while ((p=strchr(p,'_'))!=NULL)  *p=' ';
                }
            g_font_type();
            return(1);
            }
        if (strcmp(x,"w")==0)
            { font_weight=atoi(p); return(1); }
        if (strcmp(x,"i")==0)
            { font_italic=atoi(p); return(1); }
        if (strcmp(x,"stock_pen")==0)
            { stock_pen=atoi(p); crt_select_pen(); return(1); }

        if (strcmp(x,"x_move")==0)         // 7.6.2002
            { x_move=atoi(p); return(1); }
        if (strcmp(x,"y_move")==0)
            { y_move=atoi(p); return(1); }
        if (strcmp(x,"rotation")==0)
            { rotation=(int)(10.0*atof(p)+0.5);
              g_font_type();
              return(1);
            }
        if (strcmp(x,"autom_color")==0) // 16.11.2002
            { autom_color=atof(p); return(1); }
        if (strcmp(x,"color")==0) // 5.9.2004
            { set_cmyk_color(p); return(1); }

        sprintf(sbuf,"\nUnknown %% code %s",s);
        sur_print(sbuf); WAIT; // RS CHA printf -> sur_print
        
        return(1);
        }

static void korvaa(char *muunnos,char *s,char *t)
        {
        char *p, *q;
        char x[LLENGTH];

        p=muunnos; *x=EOS;
        while( (q=strchr(p,*s))!=NULL )
            {
            if (strncmp(q,s,strlen(s))==0)
                {
                strncat(x,p,q-p); strcat(x,t);
                p=q+strlen(s);
                }
            else
                {
                strncat(x,p,q-p+1);
                p=q+1;
                }
            }
        strcat(x,p); strcpy(muunnos,x);
        }

static int makro(char *sana,char *muunnos)
        {
        char *s, *y;
        int i,len;
        char *parm[10]; int nparm;
        char *sparm[10]; int nsparm;
        char prsana[LLENGTH];
        char *p;
        char varasana[LLENGTH];

        strcpy(varasana,sana);
        s=strchr(sana,'('); *s=EOS; ++s;
        strcpy(prsana,sana); strcat(prsana,"(");
        y=muunnos;
        len=strlen(s);
        if (s[len-1]!=')') { koodivirhe(s); return(-1); }
        s[len-1]=EOS;
        nparm=split(s,parm,10);
        len=strlen(prsana);
        i=0; while(i<n_sana)
            {
            if (muste_strnicmp(prsana,pr_sana[i],len)==0) break;
            ++i;
            }
        if (i==n_sana) { koodivirhe(varasana); return(-1); }
        strcpy(prsana,pr_sana[i]);
        strcpy(muunnos,pr_koodi[i]);
        p=strchr(prsana,'('); if (p==NULL) { koodivirhe(varasana); return(-1); }
        len=strlen(p);
        if (p[len-1]!=')') { koodivirhe(varasana); return(-1); }
        p[len-1]=EOS; ++p;
        nsparm=split(p,sparm,10);
        if (nsparm!=nparm)
            {
//          PR_EBLD;
//          printf("\nIncorrect number of parameters in %s\n",varasana);
            sprintf(sbuf,"Incorrect number of parameters in %s",varasana);
            p_error(sbuf);

            WAIT; return(-1);
            }
        for (i=0; i<nparm; ++i) korvaa(muunnos,sparm[i],parm[i]);
        return(1);
        }


static int muunna(char *sana,char *muunnos)
        {
        unsigned char koodi;
        char luku_koodi[4];  /* Canon VDC */
        char *s,*p,*q,*y;
        int i;
        char x[3*LLENGTH];
        char z[3*LLENGTH];
// Rprintf("\n%s",sana);
        s=sana;
        y=muunnos;
        while (*s)
            {
            if (*s=='[')
                {
                p=strchr(s,']');
                if (p==NULL) { koodivirhe(sana); return(-1); }
                *p=EOS;
                i=0;
                if (strchr(s+1,'(')!=NULL)
                    {
                    i=makro(s+1,x); if (i<0) return(-1);
                    i=muunna(x,z);  if (i<0) return(-1);
                    q=z;
                    while (*q) { *y=*q; ++y; ++q; }
                    s=p+1;
                    continue;
                    }

                while (i<n_sana)
                    {
                 /* if (strcmp(s+1,pr_sana[i])==0) break; */
                    if (muste_strcmpi(s+1,pr_sana[i])==0) break;
                    ++i;
                    }
                if (i<n_sana)
                    {
                    strcpy(x,pr_koodi[i]);
                    i=muunna(x,z); if (i<0) return(-1);
                    q=z;
                    while (*q) { *y=*q; ++y; ++q; }
                    s=p+1;
                    continue;
                    }

                if ((q=strchr(s,'/'))!=NULL && q<p)   /* hex  [a/b]  */
                    {
                    *q=EOS; *p=EOS;
                    *y=(unsigned char)(16*atoi(s+1)+atoi(q+1));
                    ++y;
                    s=p+1;
                    continue;
                    }

                if (*(s+1)=='B')  /* binary number [Bn] */
                    {
                    *y=(unsigned char)atoi(s+1);
                    ++y;
                    s=p+1;
                    continue;
                    }

                if (*(s+1)=='N')  /* Canon VDC integer [Nn] */
                    {
/* RS CHA                    
                    vdc(atoi(s+2),x);
                    for (i=0; i<strlen(x); ++i, ++y) *y=x[i];
*/                  
                    *y=(unsigned char)atoi(s+1);
                    ++y;
                    
                    s=p+1;
                    continue;
                    }

                if (*(s+1)=='%') /* Special code for plotting */
                    {
                    i=p_special(s+2); if (i<0) return(-1);
                    s=p+1;
                    continue;
                    }

//              printf("\n%s] is unknown!\n",s); WAIT; return(-1);
                sprintf(sbuf,"%s] is unknown!",s);
                p_error(sbuf);

                s=p+1;
                continue;
                }
            *y=*s; ++y; ++s;
            }  /* while (*s) */
        *y=EOS;

        return(1);
        }


        

static int p_textcontrol(char *s) /* tÑsmennysten alussa suluissa olevat ohjauskoodit */
        {

        int i;
        char y[3*LLENGTH];

        i=muunna(s,y); if (i<0) return(-1);

        return(1);
        }

static int p_linecontrol(char *s) /* tÑsmennysten alussa suluissa olevat ohjauskoodit */
        {

        int i;
        char y[3*LLENGTH];

        i=muunna(s,y); if (i<0) return(-1);

        return(1);
        }


static int control_code(char *x,char **pp,int laji)
/* laji; 0=text_mode 1=vector_mode 2=no immediate output*/
        {
        int k,sulkuind;

        k=etsi_loppusulku(x,pp); if (k<0) return(-1);
        if (*x=='(') sulkuind=1; else sulkuind=0;
        if (*pp==x) return(1);
        if (**pp!=',' && **pp!=EOS) return(-1);
        ++(*pp);
        *(*pp-1-sulkuind)=EOS;

        if (laji==0) { k=p_textcontrol(x+sulkuind); if (k<0) return(-1); }
        else if (laji==1) { k=p_linecontrol(x+sulkuind); if (k<0) return(-1); }
        return(1);
        }

static int p_linetype()
        {

        int i;
        char x[LLENGTH], y[3*LLENGTH];

        if (line_code==NULL) strcpy(x,"[LINE]");
        else                 strcpy(x,line_code);
        i=muunna(x,y);
        if (i<0) return(-1);
// RS NYI        crt_select_pen();
        return(1);
        }


static int plotting_range()
        {
        int i,k;
        char x[LLENGTH], *sana[3];
        char *p,*q,*q2;

        t_start=xscaleval[0]; t_end=xscaleval[xscalen-1];
        t_step=(t_end-t_start)/100.0;
        k=p_linetype(); if (k<0) return(-1);

        i=spfind2(muuttujanimi,3);
        if (i>=0)
            {
            strcpy(x,spb[i]);
            strcpy(curve_attr,x); // 20.5 2005
            k=control_code(x,&p,1);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            k=split(p,sana,3);
            if (k==1)   // x=a(step)b
                {
                q=strchr(p,'(');
                if (q!=NULL)
                    {
                    *q=EOS; ++q;
                    q2=strchr(q,')');
                    if (q2!=NULL)
                        {
                        *q2=EOS; ++q2;
                        k=3; sana[0]=p; sana[1]=q2; sana[2]=q;
                        }
                    }
                }

            if (k<2)
                {
//              printf("\nEnter plotting range in form:");
//              printf("\n%s=<lower_limit>,<upper_limit>,<step>",muuttujanimi);

    sprintf(sbuf,"Plotting range in form: %s=<lower_limit>,<upper_limit>,<step>"
                                        ,muuttujanimi);
                p_error(sbuf);

                WAIT; return(-1);
                }
            t_start=arit_atof(sana[0]);
            t_end=arit_atof(sana[1]);
            t_step=(t_end-t_start)/100.0;
            if (k>2) t_step=arit_atof(sana[2]);
            }

        return(1);
        }


static void change_color()
        {
        double color;

        color=arit_atof(color_change);
        if (color_max==1) line_color=color;
//      else line_color=(int)fmod(color,(double)color_max);
        else line_color=(int)color%color_max; // 7.7.2010
        p_lineattr();
        }

static int x_coord(double x)
        {
        return(xx+x_kuva*(xmu(x)-xmumin)/(xmumax-xmumin));
        }

static int y_coord(double y)
        {
        return(yy+y_kuva*(ymu(y)-ymumin)/(ymumax-ymumin));
        }
        
static void fill_init()
        {

        if (filltype==0) return;
        switch (filltype)
            {
          case 1: if (ymin>=0.0) y_fill=yy;
                  else if (ymax<=0.0) y_fill=yy+y_kuva;
                  else y_fill=y_coord((double)0.0);
                  break;
          case 2: if (xmin>=0.0) x_fill=xx;
                  else if (xmax<=0.0) x_fill=xx+x_kuva;
                  else x_fill=x_coord((double)0.0);
                  break;
          case 3: x_fill=x_coord((double)0.0);
                  y_fill=y_coord((double)0.0);
                  break;
            }
        }

static double integral()
        {
        int i;
        double s,t;
        double x,y,x1,y1;

        s=0.0;
        t=t_start;
        xy_arvot(t,&x1,&y1);
        while (1)
            {
            t+=t_step; if (t>t_end) t=t_end;
            xy_arvot(t,&x,&y);
            s+=(x-x1)*(y+y1)/2.0;
            if (t==t_end) break;
            x1=x; y1=y;
            }
        return(s);
        }



static int read_datapar()
        {
        int i;

        ++obs;
        while (obs<=curd.l2 && unsuitable(&curd,obs)) ++obs;
        if (obs>curd.l2) return(-1);

        for (i=0; i<nvar; ++i)
            {
            data_load(&curd,obs+(long)lag[i],curd_var[i],&arvo[sp_ind[i]]);
            }
        return(1);
        }

static int plot_curves()
        {
        int i,k;
        int x,y;
        int fill_count;
        double y_integral;
        char xx[LLENGTH], *osa[N_MESS];
        char xx2[LLENGTH], *osa2[4];
        int prind=0;
        int max_len=0; // 2.8.2009

        i=plotting_range(); if (i<0) return(-1);
        
        integral_ind=0;
        i=spfind("INTEGRAL");
        if (i>=0)
            {
            integral_ind=1;
            integral_const=arit_atof(spb[i]);
            }
        i=spfind("COLOR_CHANGE");
        if (i<0) *color_change=EOS;
        else
            {
            strcpy(xx,spb[i]);
            i=split(xx,osa,2);
            strcpy(color_change,osa[0]);
            if (i>1) color_max=atoi(osa[1]);
            if (color_max<=0) color_max=1;
            }

        i=spfind("MESSAGES");          /* 18.6.1992 */
        if (i<0) n_mess=0;
        else
            {
            strcpy(xx,spb[i]);
            n_mess=split(xx,osa,N_MESS);
            for (k=0; k<n_mess; ++k)
                {
                i=spfind(osa[k]); if (i<0) continue;
                strcpy(xx2,spb[i]);
                i=split(xx2,osa2,4);
                strcpy(c_message[k],osa2[0]);
                if (i>1) c_step[k]=arit_atoi(osa2[1]);
                if (i>2) c_x[k]=arit_atoi(osa2[2]);
                if (i>3) c_y[k]=arit_atoi(osa2[3]);
                c_i[k]=0; *c_text[k]=EOS;
                }
            }

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        nloop=0;
        data=0; nvar=0;


        for (i=0; i<spn; ++i) spb2[i]=spb[i];

        while (1)
            {
                        
            fill_init();  /* filltype=1,2,3 */
            if (integral_ind) integral_value=integral();
            t=t_start; out=2;

            for (i=0; i<n_mess; ++i)
                {
                double a;
                ++c_i[i];
                if (c_i[i]==c_step[i])
                    {
                    c_i[i]=0;
                    char_color=1; p_charcolor();
Rprintf("\nplot_curves, c_text[%d]=%s",i,c_text[i]);                    
                    
                    p_text(c_text[i],c_x[i],c_y[i],1);
                    laske(c_message[i],&a);

                    sprintf(c_text[i],"%g",a);

// 2.8.2009 needed in "Buffon's needle problem"
       k=strlen(c_text[i]); if (k>max_len) max_len=k;
       p_fill_bar(c_x[i],c_y[i],c_x[i]+max_len*(int)kirjainlev,c_y[i]+(int)kirjainkork,1);

                    char_color=0; p_charcolor();
                    
Rprintf("\nplot_curves2, c_text[%d]=%s",i,c_text[i]);                    
                    
                    p_text(c_text[i],c_x[i],c_y[i],1);
                    }
                }

            if (*color_change) change_color();

            coord(t,&x_pos,&y_pos);

//Rprintf("\ncoord done, x_pos: %d, y_pos: %d",x_pos,y_pos);  

  /*        printf("\nt=%g x=%d y=%d",t,x_pos,y_pos); getch();  */
            if (filltype)
                {
                fill_count=fill_step-1;
                if (filltype==4) { x_fill=x_pos; y_fill=y_pos; }
                }

            if (l_virhe) return(-1);

            while (1)
                {

                if (filltype && t>=fill_start && t<fill_end)
                    {
                    char *p; // vain muodon vuoksi
                    char attr[LNAME];

                    strcpy(attr,curve_fill_attr); // 20.5.2005
                    control_code(attr,&p,1);

                    ++fill_count;
                    if (fill_count==fill_step)
                        {
                        fill_count=0;
                        x=x_pos; y=y_pos;
                        switch (filltype)
                            {
                          case 1:
                            p_line(x,y_fill,1);
                            break;
                          case 2:
                            p_line(x_fill,y,1);
                            break;
                          case 3:
                          case 4:
                            p_line(x_fill,y_fill,1);
                            break;
                            }
                        x_pos=x; y_pos=y;
                        }
                    strcpy(attr,curve_attr); // 20.5.2005
                    control_code(attr,&p,1);
                    } /* filltype */

                t+=t_step; if (t>t_end) t=t_end;
                i=coord(t,&x,&y);
//Rprintf("\nplotcoord done, x: %d, y: %d",x,y);                 
                
                if (i) p_line(x,y,1);
/*              printf("\nt=%g x=%d y=%d",t,x_pos,y_pos);       */
/********************************************
                if (kbhit())
                    {
                    i=getch(); if (i=='.') { lopetus=1; break; }
                    kosketus=1;
                    }
**********************************************/
                if (t==t_end) break;
                }


            if (!nloop || lopetus) break;
            i=0;
            while (i<nloop)
                {
                int lpar;

                k=loopar[i];

                if (k==-1)            /* 26.5.92 */
                    {
                    k=read_datapar();
                    if (k<0) { ++i; continue; }
   else if (capability[0] && prind) { sprintf(sbuf," obs#=%ld",obs); sur_print(sbuf); }
                    }
                else
                    {
                    if (arvo[k]==loop_end[i]) { ++i; continue; }
                    arvo[k]+=loop_step[i];
                    if (arvo[k]>loop_end[i]-loop_step[i]/10.0)
                            arvo[k]=loop_end[i];
  if (capability[0] && prind) { sprintf(sbuf,"\n%s=%g",spa[k],arvo[k]); sur_print(sbuf); }
                    }
                for (k=0; k<i; ++k)
                    {
                    lpar=loopar[k];
                    if (lpar==-1) { obs=curd.l1-1; read_datapar(); }
                    else arvo[lpar]=loop_start[k];
                    }

                break;
                }
            if (i==nloop) break;
            }
        if (kosketus) tikki=0;
        return(1);
        }

static void rajavirhe(char c)
        {
        sprintf(sbuf,"\nIncorrect range of values in %cSCALE!",c);
        sur_print(sbuf);
        WAIT;
        }

static int xrajat()
        {
// RS REM        extern double xmu();
        xmin=xmumin=xscaleval[0];
        xmax=xmumax=xscaleval[xscalen-1];
        if (xmax<=xmin) { rajavirhe('X'); return(-1); }
        if (*xmuunnos==EOS) return(1);
        xmumin=xmu(xmin);
        xmumax=xmu(xmax);
        return(1);
        }

static int yrajat()
        {
// RS REM        extern double ymu();
        ymin=ymumin=yscaleval[0];
        ymax=ymumax=yscaleval[yscalen-1];
        if (ymax<=ymin) { rajavirhe('Y'); return(-1); }
        if (*ymuunnos==EOS) return(1);
        ymumin=ymu(ymin);
        ymumax=ymu(ymax);
        return(1);
        }

static int p_pen()
        {
        int i;
        char x[LLENGTH], y[3*LLENGTH];

        if (pen_code==NULL)
            {
            if (ps_emul) strcpy(x,"[PS_PEN]");
            else
                strcpy(x,"[PEN]");
            }
        else                strcpy(x,pen_code);
        i=muunna(x,y);
        if (i<0) return(-1);
        return(1);
        }
        
static int find_tickturn()    /* 24.9.1993 */
        {
        int i;

        tickturn=1;
        i=spfind("TICKTURN"); if (i>=0) tickturn=atoi(spb[i]);
        return(1);
        }

static void plot_xscale(int n,double value[],char *label[],int x0,int y0,int pituus)
/* int n;               arvojen lkm */
/* double value[];      skaala-arvot */
/* char *label[];       skaalanimet */
/* int x0,y0;           alkupiste */
/* int pituus;          asteikon pituus */
        {
        int i;
        double min,max;
        int x1;
        char *q;

/*  printf("\nxscale: n=%d\n",n);
    for (i=0; i<n; ++i) printf("%g ",value[i]); getch();
*/
        if ((frametype==0 || frametype==3) && !scalemove_y) return;
        find_tickturn();
        for (i=0; i<n; ++i)
            {
            x1=x0+(int)((xmu(value[i])-xmumin)/(xmumax-xmumin)*pituus);
            q=label[i];
            if (*q=='?') ++q;
            else if (tikki) p_line2(x1,y0,x1,y0-2*tickturn*tikki,1);
            // 25.4.2003
            if (pyramid && *q=='-') ++q; // 18.10.2005
            
// Rprintf("\nplot_xscale, q=%s",q);                    
            
            p_text(q,x1,y0-2*tikki-(int)(1.2*kirjainkork),1);
            }
        }


static void plot_yscale(int n,double value[],char *label[],int x0,int y0,int pituus)
/* int n;               arvojen lkm */
/* double value[];      skaala-arvot */
/* char *label[];       skaalanimet */
/* int x0,y0;           alkupiste */
/* int pituus;          asteikon pituus */
        {
        int i;
        double min,max;
        int y1;
        int dmax;
        char x[LLENGTH],*osa[2];
        double klev;
  /*      extern */ double ycharwidth;
        int k;
        char *p,*q;
        int x1;
        int len;

/*  printf("\nyscale: n=%d\n",n);
    for (i=0; i<n; ++i) printf("%g ",value[i]); getch();
*/
        if ((frametype==0 || frametype==3) && !scalemove_x) return;
        find_tickturn();
        dmax=0; /* max desimaaleja + 1 */
        for (i=0; i<n; ++i)
            {
            
            p=strchr(label[i],'.');
            if (p!=NULL) { k=strlen(label[i])-(p-label[i]);
                           if (k>dmax) dmax=k; }
            }

        klev=kirjainlev;    /* 22.4.92 */
        if (ycharwidth>0.0) klev=ycharwidth/0.28346;
        i=spfind("YSCALEPOS");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,osa,2);
            yscalepos1=atof(osa[0]); if (i>1) yscalepos2=atof(osa[1]);
            }
        for (i=0; i<n; ++i)
            {
            len=strlen(label[i]);

            y1=y0+(int)((ymu(value[i])-ymumin)/(ymumax-ymumin)*pituus);
            q=label[i];
            if (*q=='?') { ++q; --len; }
            else if (tikki) p_line2(x0,y1,x0-2*tickturn*tikki,y1,1);
            // 25.4.2003
            p=strchr(q,'.');
            if (p!=NULL) k=len-(p-q); else k=0;
                x1=x0-(int)(klev*(1+len+dmax-k))-tikki;
            if (yscalepos1!=0) x1=x0+(int)yscalepos1;
            
// Rprintf("\nplot_yscale, q=%s",q);                                
            
            p_text(q,x1,y1-(int)(kirjainkork/2.0),1);
            }

        }
 
 
static void scale_err(char *s)
        {
//      printf("\nError in scale notation %s: More than %d values",
//                                         s,MAXSCALELIST);

        sprintf(sbuf,"Error in scale notation %s: More than %d values",
                                           s,MAXSCALELIST);
        p_error(sbuf);
//      printf("\n or text exceeding space available!\n");
//        WAIT;
        }


/* skaala_arvot(s,list,osa,n,scalespace) muodostaa asteikkolistauksen
   s=input-merkkijono tyyppiÑ 0(1)5,6(2)10,20,50
   list=output tyyppiÑ 0 1 2 3 4 5 6 8 10 20 50
   osa[]=osoittimet listaan
   *n=lukumÑÑrÑ (11)
   scalespace=SCALESPACE
*/
static int skaala_arvot(char *s,char *list,char *osa[],int *n,int scalespace)
        {
//        extern double arit_atof();
        int i,h,k;
        char *p,*q,*r;
        double dp,dstep,dr;
        char *p_list;
        char x[LLENGTH], *pala[MAXSCALELIST];
        char sana[32];
        int plus;

        strcpy(x,s);
        *n=0;
        p_list=list;
        k=split(x,pala,MAXSCALELIST);
        for (h=0; h<k; ++h)
            {
            p=pala[h];
            q=strchr(p,'(');
            if (q==NULL || strchr(p,':')!=NULL)
                          /* : ei sallittu lyhennysmerkinnîissÑ */
                {
                osa[(*n)++]=p_list;
                if (*n>=MAXSCALELIST ||
                    p_list-list>scalespace-strlen(p)-1)
                    { scale_err(s); return(-1); }
                while (*p) { *p_list=*p; ++p_list; ++p; }
                *p_list=EOS; ++p_list;
                continue;
                }
            *q=EOS; ++q;
            r=strchr(q,')');
            if (r==NULL)
                {
//              printf("\n) is missing in %s\n",s);
                sprintf(sbuf,") is missing in %s\n",s);
                p_error(sbuf);
                WAIT; return(-1);
                }
            *r=EOS; ++r;
            dp=arit_atof(p); dstep=arit_atof(q); dr=arit_atof(r);
            if (dstep<=0.0)
                {
//              printf("\nNegative step not allowed in %s\n",s);
                sprintf(sbuf,"Negative step not allowed in %s",s);
                p_error(sbuf);
                WAIT; return(-1);
                }
            if (*q=='+' || *p=='+' || *r=='+') plus=1; else plus=0;
            while (dp<=dr+muste_fabs(dstep)*1e-7)   /* fabs(dr) -> fabs(dstep) 15.5.93 */
                {
                if (muste_fabs(dp)<1e-10) dp=0.0;
                fconv(dp,"",sana);
                if (plus && dp>=0)
                    {
                    for (i=strlen(sana); i>=0; --i) sana[i+1]=sana[i];
                    sana[0]='+';
                    }
                osa[(*n)++]=p_list;
                if (*n>=MAXSCALELIST ||
                    p_list-list>scalespace-strlen(p)-1)
                    { scale_err(s); return(-1); }
                p=sana; while (*p) { *p_list=*p; ++p_list; ++p; }
                *p_list=EOS; ++p_list;

                dp+=dstep;
                }
            }
        return(1);
        }
 

static int xyscale(char *suunta)
/* char *suunta;  "X" tai "Y" */
        {
//        extern double arit_atof();
        int i,k;
        char x[LLENGTH];
        char *p,*q;
        char muunnos[LLENGTH];

        i=p_pen(); if (i<0) return(-1);
        i=p_linetype(); if (i<0) return(-1);  /* merkintÑviivoihin */
        i=spfind("SCALE");
        if (i<0)   /* haetaan joko XSCALE tai YSCALE */
            {
            char snimi[16];
            strcpy(snimi,suunta); strcat(snimi,"SCALE");
            i=spfind(snimi);
            }
        if (i>=0) strcpy(x,spb[i]);
        else
            {
            strcpy(x,"-10,0,10");

/*          if (*suunta=='X') k=x_kuva/kirjainlev;
            else              k=2*y_kuva/kirjainkork;

            i=autom_scale(x,min,max,k); if (i<0) return(-1);
*/
            }
        k=control_code(x,&p,0);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        if (*p=='*')
            {
            ++p;
            q=p;
            while (*q && *q!=',') ++q;
            *q=EOS;
            strcpy(muunnos,p);
            p=q+1;
            }
        else *muunnos=EOS;
        if (*p==EOS)
            { printf("\n%sSCALE values missing!",suunta); WAIT; return(-1); }

        scalemove_x=scalemove_y=0;     /* 12.2.1993 */
        i=spfind("AXES");
        if (i>=0)
            {
            char ax[32],*axx[2];

            strcpy(ax,spb[i]); i=split(ax,axx,2);
            if (i>0) scalemove_y=arit_atoi(axx[0]);
            if (i>1) scalemove_x=arit_atoi(axx[1]);
            }

        if (*suunta=='X')
            {
            strcpy(xmuunnos,muunnos);
            k=skaala_arvot(p,xscales,xscal,&xscalen,scalespace);
            if (k<0) return(-1);

            for (i=0; i<xscalen; ++i)
                {
                q=xscal[i];
                p=strchr(xscal[i],':'); if (p!=NULL) { xscal[i]=p+1; *p=EOS; }
                xscaleval[i]=arit_atof(q);
                }
            i=xrajat(); if (i<0) return(-1);

        if (scalemove_y) p_line2(xx,yy+scalemove_y,xx+x_kuva,yy+scalemove_y,1);
            plot_xscale(xscalen,xscaleval,xscal,xx,yy+scalemove_y,x_kuva);
            }
        else
            {
            strcpy(ymuunnos,muunnos);
            k=skaala_arvot(p,yscales,yscal,&yscalen,scalespace);
            if (k<0) return(-1);

            for (i=0; i<yscalen; ++i)
                {
                q=yscal[i];
                p=strchr(yscal[i],':'); if (p!=NULL) { yscal[i]=p+1; *p=EOS; }
                yscaleval[i]=arit_atof(q);
                }
            i=yrajat(); if (i<0) return(-1);
        if (scalemove_x) p_line2(xx+scalemove_x,yy,xx+scalemove_x,yy+y_kuva,1);
            plot_yscale(yscalen,yscaleval,yscal,xx+scalemove_x,yy,y_kuva);
            return(1);
            }
        return(1);
        }


static void tee_otsikko(char *ots)
        {
        char s[2*LLENGTH];

        *s=EOS;
        if (integral_function) strcpy(s,"INTEGRAL ");

        if (cfunktio)
            {
            strcat(s,xlauseke);
            strcpy(ots,s);
            return;
            }
        if (*xlauseke!=EOS)
            {
            strcat(s,"X("); strcat(s,muuttujanimi); strcat(s,")=");
            strcat(s,xlauseke); strcat(s,", ");
            }
        strcat(s,"Y("); strcat(s,muuttujanimi); strcat(s,")=");
        strcat(s,ylauseke);
        if (strlen(s)>LLENGTH) s[LLENGTH-1]=EOS;
        strcpy(ots,s);
        }

static int p_end()
        {
// fprintf(temp2,"\nP_END");
// RS NYI        crt_delete_pens();

        return(1);
/*************************************
        int i,k;
        extern int crt_nop();
        long lwait;
        extern int l_virhe;

        if (l_virhe)
            {
            p_close(); exit(0);
            }
        if (*p_outfile) p_save(p_outfile,x_out,y_out,xd_out,yd_out);
        i=spfind("CALL"); if (i>=0) p_call(spb[i]);
        if (etu<2 && !erun)
            {
            while (1)
                {
                i=getch();
                if (i<'A' || i>'F') break;
                k=spfind("PALETTE"); if (k<0) break;
                k=p_palette(spb[k],(int)(i-'A')); if (k<0) break;
                }
            }
        else
            {
            lwait=3000L;
            i=spfind("WAIT");
            if (i>=0) lwait=1000L*atoi(spb[i]);
            sur_wait((long)(tut_wait_c*lwait/10),crt_nop,1);
            if (kbhit() || tikki==0)
                {
                if (kbhit()) getch(); getch();
                }
            }
        if (kbhit()) getch();
        p_close();
        if (*crt_exit) system(crt_exit);
****************************************************************/
        }

static int xlabel(char *s)
/* char *s;    default label */
        {
        int i,k;
        char x[LLENGTH];
        char *p,*ps;


        i=spfind("XLABEL");
        if (i<0) { ps=NULL; p=x; strcpy(x,s); if (*s==EOS) return(1); }
        else
            {
            strcpy(x,spb[i]);
            k=control_code(x,&p,0);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            if (spshad[i]==NULL) ps=NULL;
            else { k=p-x; ps=spshad[i]+k; }
            }
        p_text2(p,ps,xx+x_kuva-(int)(strlen(p)*kirjainlev),
                     yy-4*tikki-(int)(2.8*kirjainkork),1);
        return(1);
        }

static int ylabel(char *s)
        {
        int i,k;
        char x[LLENGTH];
        char *p,*ps;

        i=spfind("YLABEL");
        if (i<0) { ps=NULL; p=x; strcpy(x,s); if (*s==EOS) return(1); }
        else
            {
            strcpy(x,spb[i]);
            k=control_code(x,&p,0);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            if (spshad[i]==NULL) ps=NULL;
            else { k=p-x; ps=spshad[i]+k; }
            }
        p_text2(p,ps,xx,yy+y_kuva+2*tikki,1);

        return(1);
        }


static int xdiv()
        {
// RS REM        extern double arit_atof();
        int i,k;
        char x[LLENGTH], *osa[3];

        i=spfind("XDIV");
        if (i<0) return(1);
        strcpy(x,spb[i]);
        k=split(x,osa,3);
        if (k!=3) { sp_virhe(spa[i],spb[i]); return(-1); }
        xdiv1=arit_atof(osa[0]); xdiv2=arit_atof(osa[1]); xdiv3=arit_atof(osa[2]);
        return(1);
        }

static int ydiv()
        {
// RS REM        extern double arit_atof();
        int i,k;
        char x[LLENGTH], *osa[3];

        i=spfind("YDIV");
        if (i<0) return(1);
        strcpy(x,spb[i]);
        k=split(x,osa,3);
        if (k!=3) { sp_virhe(spa[i],spb[i]); return(-1); }
        ydiv1=arit_atof(osa[0]); ydiv2=arit_atof(osa[1]); ydiv3=arit_atof(osa[2]);
        return(1);
        }

static int plot_box(int x1,int y1,int lev,int kork)
        {
        p_line2(x1,y1,x1+lev,y1,1);
        p_line(x1+lev,y1+kork,1);
        p_line(x1,y1+kork,1);
        p_line(x1,y1,1);
        return(1);
        }

static int plot_halfbox(int x1,int y1,int lev,int kork)
        {
        p_line2(x1,y1,x1+lev,y1,1);
        p_line2(x1,y1,x1,y1+kork,1);
        return(1);
        }

static int p_origin(int x,int y)
        {
        return(1);
        }

static int frame(int frtype)
/* int frtype; FRAME: 0=ei 1=vain sisÑ 2=myîs ulko 3=vain ulko */
        {
        int i,k;
        char x[LLENGTH], *osa[3];
        double a;
        char *p;
        char y[LLENGTH];

        i=spfind("FRAME");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            *framecode=EOS;
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(framecode,x+i); ++p;
                }
            frtype=atoi(p);
            strcpy(x,framecode);
            k=p_linecontrol(x);
            }

        frametype=frtype;
        i=spfind("HOME");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=split(x,osa,2); if (k<2) { sp_virhe(spa[i],spb[i]); return(-1); }
            x_home=arit_atoi(osa[0]); y_home=arit_atoi(osa[1]);
            }
        i=spfind("SIZE");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=split(x,osa,2); if (k<2) { sp_virhe(spa[i],spb[i]); return(-1); }
            x_size=arit_atoi(osa[0]); y_size=arit_atoi(osa[1]);
            }
        p_origin(x_home,y_home);

        if (pr_type==2)
            {
            i=spfind("COLORS");
//          if (i<0) strcpy(x,"[COLORS]"); else strcpy(x,spb[i]);
            if (i>=0) strcpy(x,spb[i]);
                i=muunna(x,y); if (i<0) { p_end(); return(-1); }
            }

        a=xdiv1+xdiv2+xdiv3; xdiv1=xdiv1/a; xdiv2=xdiv2/a; xdiv3=xdiv3/a;
        a=ydiv1+ydiv2+ydiv3; ydiv1=ydiv1/a; ydiv2=ydiv2/a; ydiv3=ydiv3/a;
        xx=(int)(x_home+xdiv1*x_size);
        yy=(int)(y_home+ydiv1*y_size);
        x_kuva=(int)(xdiv2*x_size);
        y_kuva=(int)(ydiv2*y_size);

/*      printf("\nxx=%d yy=%d",xx,yy);
        printf("\nkuva-ala: %d %d",x_kuva,y_kuva);
        printf("\nxdiv: %g %g %g",xdiv1,xdiv2,xdiv3);
        printf("\nydiv: %g %g %g",ydiv1,ydiv2,ydiv3);
        getch();
*/
        if (*framecode) { strcpy(x,framecode); k=p_linecontrol(x); }  /* 25.2.90 */
        else k=p_linetype();
        if (k<0) return(-1);
        if (pr_type!=1)
            {
            if (frtype>=2 && frtype<6) plot_box(x_home,y_home,x_size,y_size);
            if (frtype==5 || frtype==6) plot_halfbox(xx,yy,x_kuva,y_kuva); // 27.11.02
            if (frtype>0 && frtype<3) plot_box(xx,yy,x_kuva,y_kuva);
            }
        p_linetype(); /* 25.2.1990 */
        return(1);
        }

static int curves()
        {
        int i;
        char otsikko[LLENGTH];

        tee_otsikko(otsikko);
/* RS NYI        
        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
*/        
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
/* RS NYI        
        if (pr_type==1 || pr_type==2)
         { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
*/        
        i=xyscale("X"); if (i<0) { p_end(); return(-1); }
        i=xlabel(xmuunnos); if (i<0) { p_end(); return(-1); }
// RS NYI        i=xyscale2("X"); if (i<0) { p_end(); return(-1); }
        i=xyscale("Y"); if (i<0) { p_end(); return(-1); }
        i=ylabel(ymuunnos); if (i<0) { p_end(); return(-1); }
// RS NYI        i=xyscale2("Y"); if (i<0) { p_end(); return(-1); }

/* RS NYI        
        i=xgrid(); if (i<0) { p_end(); return(-1); }
        i=ygrid(); if (i<0) { p_end(); return(-1); }
        i=xtick(1); if (i<0) { p_end(); return(-1); }
        i=ytick(1); if (i<0) { p_end(); return(-1); }
        i=xtick(2); if (i<0) { p_end(); return(-1); }
        i=ytick(2); if (i<0) { p_end(); return(-1); }
        i=fill(); if (i<0) { p_end(); return(-1); }
*/

        i=plot_curves(); if (i<0) { p_end(); return(-1); }

/* RS NYI
        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1 && pr_type!=2)
         { i=frames(); if (i<0) { p_end(); return(-1); } }
        i=fills(); if (i<0) { p_end(); return(-1); }
        i=polygons(); if (i<0) { p_end(); return(-1); }
        p_end();
*/        
        return(1);
        }
        

static void missing_char(char ch,int j)
        {
//      error_line(j);
//      printf("\n%c missing in equation!",ch);
        sprintf(sbuf,"%c missing in equation!",ch);
        p_error(sbuf);
        WAIT;
        }
        
static void incorrect_varname(int j)
        {
//      error_line(j);
//      printf("\nIncorrect name for a variable (X,x,Y,y allowed)");
        sprintf(sbuf,"Incorrect name for a variable (X,x,Y,y allowed)"); 
        p_error(sbuf);
        WAIT;
        }

static int tutki_yhtalo()
        {
        char x[LLENGTH];
        int i,j;
        char *p,*q;
        int nlaus;

        j=r1+r-1;
        edread(x,j);
        p=strchr(x+1,'=');
        if (p==NULL) { missing_char('=',j); return(-1); }

        if (*(p-1)!=')') { missing_char(')',j); return(-1); }
        q=p-1;
        *q=EOS;
        while (*q!='(' && q>x) --q;
        if (q==x) { missing_char('(',j); return(-1); }
        strcpy(muuttujanimi,q+1);
        --q;
        nlaus=0;
        if (*q=='y' || *q=='Y') nlaus=1;
        else if (*q=='x' || *q=='X') nlaus=2;
        if (nlaus==0 || *(q-1)!=' ')
            { incorrect_varname(j); return(-1); }
        q=p+1;
        while (*q!=' ' && *q!=',') ++q;
        *q=EOS;
        if (nlaus==1)
            {
            strcpy(ylauseke,p+1);
            *xlauseke=EOS;    /* oli == */
            return(1);
            }
        strcpy(xlauseke,p+1);
        p=strchr(q+1,'=');
        if (p!=NULL)
            strcpy(ylauseke,p+1);
        else
            {
            ++j;
            edread(x,j); p=strchr(x+1,'=');
            if (p==NULL) { missing_char('=',j); return(-1); }
            strcpy(ylauseke,p+1);
            }
        p=ylauseke;
        while (*p && *p!=' ') ++p; *p=EOS;
        return(1);
        }


static void alkukoodit()
        {
        int i;

        *pr_tila=EOS;
        pr_osoitin=pr_tila;
        n_sana=0;
        for (i=0; i<256; ++i) code[i]=(unsigned char)i;
        }

static int define(char *x,char **sana,int n,char *rivi)
        {
        int i,k;

        if (n!=3) { koodivirhe(rivi); return(-1); }

        i=strlen(sana[1]);
        if ( sana[1][0]!='[' || sana[1][i-1]!=']' )
            {
//          PR_EBLD;
//          printf("\nBrackets [] missing in %s\n",sana[1]);

            sprintf(sbuf,"Brackets [] missing in %s",sana[1]);
            p_error(sbuf);

            WAIT; PR_ENRM; return(-1);
            }
        sana[1][i-1]=EOS; ++sana[1];
        i=0; while (i<n_sana)
            {
            if (muste_strcmpi(sana[1],pr_sana[i])==0) break;
            ++i;
            }

        if (i==n_sana) ++n_sana;
        pr_sana[i]=pr_osoitin; strcpy(pr_osoitin,sana[1]);
        pr_osoitin+=strlen(sana[1])+1;
        pr_koodi[i]=pr_osoitin; strcpy(pr_osoitin,sana[2]);
        pr_osoitin+=strlen(sana[2])+1;
        return(1);
        }

static int shadows(char *x,char **sana,int n,char *rivi)   /* shadow <koodi> <alkukoodisana> <loppukoodisana> */
        {
        int i,k;
        unsigned char varjo;
        char y[LLENGTH];

        if (n<3) { koodivirhe(rivi); return(-1); }

        i=strlen(sana[1]);
        if (i==1) varjo=sana[1][0];
        else { i=muunna(sana[1],y); if (i<0) return(-1); varjo=*y; }

        shadow[varjo]=pr_osoitin; strcpy(pr_osoitin,sana[2]);
        pr_osoitin+=strlen(sana[2])+1;
        if (n<4) shadow2[varjo]=NULL;
        else
            {
            shadow2[varjo]=pr_osoitin; strcpy(pr_osoitin,sana[3]);
            pr_osoitin+=strlen(sana[3])+1;
            }
        return(1);
        }

static int load_codes_gplot(char *codefile,unsigned char *code)
        {
        FILE *codes;
        int i;
        char x[LLENGTH];

//Rprintf("\nload_codes_gplot!");

        strcpy(x,codefile);
        if (strchr(x,':')==NULL && *x!='.')
            { strcpy(x,survo_path); strcat(x,"SYS/"); strcat(x,codefile); } // RS CHA \\ -> /

        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            PR_EBLD;
            sprintf(sbuf,"\nCode conversion file %s not found!",x);
            sur_print(sbuf); WAIT; PR_ENRM; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        fclose(codes);
        return(1);
        }

static int codes(char *x,char **sana,int n)   /* codes <kooditiedosto> */
        {
        load_codes_gplot(sana[1],code);
        return(1);
        }


static int space_split(char rivi[],char *sana[],int max)  // RS short -> int
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Vain vÑlilyînnit toimivat erottimina.
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
        {
        int g;
        int p;
        int edell; /* vÑli edellÑ */
        int len;
        
        g=0;
        edell=0;
        len=strlen(rivi);
        
        for (p=0; p<len; ++p)
                {
                if (rivi[p]==' ' || rivi[p]=='\t' || rivi[p]=='\r' || rivi[p]=='\n') // RS ADD other than ' '
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


static int include(); // RS Declaration
        
static int lue_koodit(char *x)
        {
        char *sana[16];
        int i,n;
        char x1[2*LLENGTH];
        char y[3*LLENGTH];
        char *p;


        p=x;
// RS CHA        while ( (p=strchr(p,TAB))!=NULL ) { *p=' '; ++p; }
// RS won't work on windows   while ( (p=strchr(p,"\t\r\n"))!=NULL ) { *p=' '; ++p; } // RS CHA TAB -> "\t\r\n"

        while ( (p=strchr(p,'\t'))!=NULL ) { *p=' '; ++p; }  // RS 
        p=x; while ( (p=strchr(p,'\r'))!=NULL ) { *p=' '; ++p; } // RS
        p=x; while ( (p=strchr(p,'\n'))!=NULL ) { *p=' '; ++p; } // RS


        p=x;
        while( (p=strchr(p,'/'))!=NULL )
            {
            if ( (*(p-1)==' ') &&
                 (*(p+1)==' ' || *(p+1)==EOS) )
            { *p=EOS; break; }
            ++p;
            }

        strcpy(x1,x);
        n=space_split(x1,sana,16);
        if (n==0) return(1);

/* vanha
        if (muste_strcmpi(sana[0],"DEFINE")==0) { i=define(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"SHADOW")==0) { i=shadows(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"CODES")==0)  { i=codes(x1,sana,n); return(i); }
        if (muste_strcmpi(sana[0],"INCLUDE")==0)  { i=include(x1,sana,n); return(i); }
        if (muste_strcmpi(sana[0],"DOS")==0)  { i=dos(x); return(i); }
  tÑmÑ oli poistettu
        p=x;
        i=strlen(x); while(x[i-1]==' ' && i>1) x[--i]=EOS;
        i=0; while (x[i]==' ') ++i;
        if (p_empty(x+i)) return(1);
        i=muunna(x+i,y); if (i<0) return(-1);
        for (i=0; i<strlen(y); ++i) putc((int)y[i],kirjoitin);
*/
        if (muste_strcmpi(sana[0],"DEFINE")==0) { i=define(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"SHADOW")==0) { i=shadows(x1,sana,n,x); return(i); }
        if (muste_strcmpi(sana[0],"CODES")==0)  { i=codes(x1,sana,n); return(i); }
        if (muste_strcmpi(sana[0],"INCLUDE")==0)  { i=include(x1,sana,n); return(i); }
/*      if (muste_strcmpi(sana[0],"HEADER_LINES")==0) { i=hlines(x); return(i); }  */
/*      if (muste_strcmpi(sana[0],"CHAPTER")==0) { i=chapter(x); return(i); }      */
/*      if (muste_strcmpi(sana[0],"PICTURE")==0) { i=picture(x); return(i); }      */
/*      if (muste_strcmpi(sana[0],"TEXT")==0) { i=textfile(x); return(i); }        */
/*      if (muste_strcmpi(sana[0],"DOS")==0) { i=dos(x); return(i); }              */
        if (muste_strcmpi(sana[0],"CONTROL")==0) { /* i=controls(x1,sana,n,x); */ return(1); }
        if (muste_strcmpi(sana[0],"NULL")==0) { /* i=null_ch(n,sana[1]); */ return(1); }
/*      if (muste_strcmpi(sana[0],"REPLACE")==0) { i=replace(x); return(i); }      */
/*      if (muste_strcmpi(sana[0],"FONT")==0 || muste_strcmpi(sana[0],"RASTER")==0)
                                          { i=rasters(x1,sana,n,x); return(i); }
*/
/*      if (muste_strcmpi(sana[0],"BINMODE")==0) { binopen(); return(1); }         */
        if (muste_strcmpi(sana[0],"TYPE")==0) { pr_type=atoi(sana[1]); return(1); }
// RS REM?        if (muste_strcmpi(sana[0],"PS_CODE")==0) { i=ps_code(x1,sana,n,x); return(i); }

        if (pr_type==1)
            {
            i=strlen(x); while(x[i-1]==' ' && i>1) x[--i]=EOS;
            if (i==1) return(1);  /* tyhjÑ rivi */
            p=x; while (*p==' ') ++p;
            i=muunna(p,y); if (i<0) return(-1);
// RS REM?            send(y);     /* kirjoita(y) */
            }
        return(1);
        }


static int include(char *x,char **sana,int n)
        {
        FILE *ifile;
        char rivi[2*LLENGTH];
        int i,len;

        strcpy(rivi,sana[1]);
        if (strchr(rivi,':')==NULL && *rivi!='.')
            {
            strcpy(rivi,survo_path); strcat(rivi,"SYS/"); // RS CHA \\ -> /
            strcat(rivi,sana[1]);
            }
        ifile=muste_fopen(rivi,"rt");
        if (ifile==NULL)
            {
            PR_EBLD;
            sprintf(sbuf,"Include file %s not found!",rivi);
            p_error(sbuf);
            }

        while (1)
            {
            fgets(rivi,LLENGTH,ifile);
            if (feof(ifile)) break;
            len=strlen(rivi); rivi[len-1]=EOS;
            i=lue_koodit(rivi); if (i<0) { fclose(ifile); return(-1); }
            }

        fclose(ifile);
        return(1);
        }

static int p_palette(char *list,int nro)
        {
        FILE *pal;
        int i,k;
        int rgb_in[3];
        char s[LLENGTH], *osa[4];
        char *p;
        char x[LLENGTH], *nimi[6];
        int n;

        strcpy(x,list); n=split(x,nimi,6);
        if (nro>n-1) return(1);

        p=strchr(nimi[nro],':');
        if (p!=NULL) { k=0; strcpy(s,nimi[nro]); }
        else { k=1; strcpy(s,edisk); strcat(s,nimi[nro]); }
        p=strchr(s+strlen(s)-4,'.'); if (p==NULL) strcat(s,".PAL");
        pal=muste_fopen(s,"rt");
        if (pal==NULL && k)
            {
            strcpy(s,survo_path); strcat(s,"SYS/"); strcat(s,nimi[nro]);
            p=strchr(s+strlen(s)-4,'.'); if (p==NULL) strcat(s,".PAL");
            pal=muste_fopen(s,"rt");
            }
        if (pal==NULL) { sprintf(sbuf,"\nPalette file %s not found!",s); 
                         sur_print(sbuf); WAIT; return(-1); }

        while (1)
            {
            fgets(s,LLENGTH-5,pal);
            if (feof(pal)) break;
            s[strlen(s)-1]=EOS;
            k=split(s,osa,4);
            i=atoi(osa[0]);
            for (k=0; k<3; ++k) vari[i][k]=255.0*atoi(osa[k+1])/1000.0;
            }
        fclose(pal);
//      _remapallpalette(color);

        return(1);
        }

static int p_init(char *laite)
/* *laite; ei kaytossa */
        {
        int i,j;
        int infiles;
        char x[LLENGTH], *sana[10];
        char y[LLENGTH];

        capability[0]=0;
        capability[1]=0;

        alkukoodit();
        for (i=0; i<256; ++i) g_color[i]=i;

//      char_height=14.0; char_width=8.0;
        gg_char=2;

        i=spfind("INCLUDE");
        if (i>=0) sana[1]=spb[i];
        else
            {
            i=hae_apu("crt_dev",x); if (i==0) strcpy(x,"CRT16.DEV"); // RS CHA CRT.DEV -> CRT16.DEV
            sana[1]=x;
            }
        i=include(x,sana,2); if (i<0) return(-1);

        i=spfind("PALETTE"); if (i>=0) strcpy(x,spb[i]);
        if (i<0) { i=hae_apu("crt_palette",x); --i; }
        if (i>=0)
            {
            if (muste_strnicmp(x,"NUL",3)!=0)
                {
                i=p_palette(x,0);
                if (i<0) { p_end(); return(-1); }
                }
            }
        i=spfind("PIEBORDER"); if (i>=0) pieborder=atoi(spb[i]);

        i=spfind("SHADEMAX");
        if (i>=0) { shademax=atoi(spb[i]); if (shademax<=1) shademax=7; }
        else
            {
            strcpy(x,"[SHADEMAX]");
            muunna(x,y); shademax=atoi(y);
            }

        ps_emul=0;
        i=spfind("MODE");
        if (i>=0 && muste_strcmpi(spb[i],"PS")==0) ps_emul=1;

        tikki=2; /* tick-viivan pituus (min. viivan tai raon pituus) */
        marker_type=3; marker_size=2;
        if (ps_emul)
            {
            tikki=10; marker_size=5;
            i=muunna("[Swiss(10)]",y); if (i<0) return(-1);
            }
        x_pos=0; y_pos=0;
        x_home=0; y_home=0;

//      i=spfind("RATIO"); if (i>=0) y_ratio=arit_atof(spb[i]);
        x_size=x_metasize; y_size=y_metasize;
        y_const=y_size;
        xdiv1=300.0; xdiv2=1000.0; xdiv3=200.0;
        ydiv1=300.0; ydiv2=1000.0; ydiv3=200.0;
        x_origin=VASEN_REUNA; y_origin=ALAREUNA;
        i=spfind("TICKLENGTH");
        if (i>=0) tikki=atoi(spb[i]);

/* RS NYI
        p_charsize();


        p_back2();
        p_edit();

        crt_select_brush();
*/

        return(1);
        }


static int sp_listaus(char *s)
        {
        int k;
        k=strlen(s);
        strncpy(spl,s,k);
        spa[spn]=spl; spb[spn]=NULL;
        spl+=k+1; *(spl-1)=EOS;
        ++spn;
        }

int varnimet()
        {
        sp_listaus(muuttujanimi);    /* spa[0] */
        sp_listaus("x");
        sp_listaus("y");
        return(spn);
        }


static int muste_gplot_curves()
        {
        int i,k,v,ind;
        char laite[LLENGTH];
        char gtype[16];

        ind=integral_function=0;
        if (muste_strcmpi(word[1],"INTEGRAL")==0)
            ind=integral_function=1;

        i=tutki_yhtalo(); if (i<0) return(-1);

        spa[0]=muuttujanimi;

        i=spfind("DEVICE");
        if (i<0) strcpy(laite,"PRN");
        else
            {
            strcpy(laite,spb[i]);
            if (strchr(laite,':')==NULL)
                {
                strcpy(laite,edisk);
                strcat(laite,spb[i]);
                }
            }
        i=p_init(laite); if (i<0) return(-1);

        curves();

        edisp=1;
        return(1);
        }



static int read_videomode(char *x)
        {
        int i,k;
        char *s[2];
        char y[LLENGTH];

        y_ratio=1.0;
        if (muste_strcmpi(x,"EGA")==0) { x_metasize=640; y_metasize=350; y_ratio=0.788066; return(1); }
        if (muste_strcmpi(x,"VGA")==0) { x_metasize=640; y_metasize=480; y_ratio=1.01863; return(1); }
        if (muste_strcmpi(x,"CGA")==0) { x_metasize=320; y_metasize=200; y_ratio=0.901176; return(1); }
        if (muste_strcmpi(x,"SVGA")==0) { x_metasize=800; y_metasize=600; return(1); }
        if (muste_strcmpi(x,"XRES")==0) { x_metasize=1024; y_metasize=768; return(1); }
        if (muste_strcmpi(x,"PS")==0) // PostScript-emulointi
            {
            i=spfind("SIZE");
            if (i>=0)
                {
                strcpy(y,spb[i]);
                k=split(y,s,2); if (k<2) { sp_virhe(spa[i],spb[i]); return(-1); }
                x_metasize=arit_atoi(s[0]); y_metasize=arit_atoi(s[1]);
                }
            else { x_metasize=1500; y_metasize=1500; }
            ps_emul=1;
            }
        i=split(x,s,2);
        if (i==2) { x_metasize=arit_atoi(s[0]); y_metasize=arit_atoi(s[1]); }
        return(1);
        }

static int set_metasize()
    {
    int i;
    char x[LNAME];

    x_metasize=1000; y_metasize=1000; y_ratio=1.0;

    i=hae_apu("videomode",x);
    if (i) read_videomode(x);
    i=spfind("MODE");
    if (i>=0)
        {
        strcpy(x,spb[i]);
        read_videomode(x);
        }

    return(1);
    }

static int use_layout(char *layout,int id)
    {
    int i,n,k;
    double xs,ys;
    char x[100];
    char *s[4];
    double x_pxls,y_pxls;
    int par[4];

    if (*layout=='-') return(1);
    if (*layout==EOS) return(1);

    temp=muste_fopen(layout,"rt");
    if (temp==NULL) // RS ADD      
    	{ sprintf(sbuf,"\nLayout file %s not found!",layout); sur_print(sbuf); WAIT; return(-1); }
    fgets(x,99,temp);
    split(x,s,3);
    n=atoi(s[0]); x_pxls=atof(s[1]); y_pxls=atof(s[2]);

    sur_screen_dim(&par[0],&par[1]);

/*
    xs=GetSystemMetrics(SM_CXSCREEN)/x_pxls; // esim. 1024
    ys=GetSystemMetrics(SM_CYSCREEN)/y_pxls; // esim.  768
*/
    xs=par[0]/x_pxls; 
    ys=par[1]/y_pxls;

    i=id; // RS CHA atoi(id);
    for (k=1; k<=n; ++k)
        {
        fgets(x,99,temp);
        if (k<i) continue;
        i=split(x,s,4);
        if (i<4) return(1);
        x_whome=atoi(s[0])*xs; y_whome=atoi(s[1])*ys;
        x_wsize=atoi(s[2])*xs; y_wsize=atoi(s[3])*ys;
        break;
        }
    return(1);
    }



static int muste_gplot(int id)
     {
     int i,k;
     char *s[4];
     char x[LLENGTH];
     int wstyle;
     int top,transparent;
     int iXframe,iYframe; // 3.2.2002
     int x_wsize2,y_wsize2; // 24.3.2002
     int par[4];
     double xs,ys;
     double rnd1,rnd2,rnd3,rnd4;


// RS variable init     
muste_gplot_init=0;
l_virhe=0;
muste_xpos=muste_ypos=0;
capability[0]=0;
capability[1]=0;
scalemove_x=scalemove_y=0;
odota_tuloksia=0;
//fixed_plot=0;
//first_plot_number=1;
//gplot_count=0;
//max_hdl=MAX_HDL;
x_pos=y_pos=0;
x_home=y_home=0;     /* koko kuvan vasen alakulma */
xx=yy=0;              /* kuva-alueen vasen alakulma */
x_size=y_size=0;     /* kuvan koko */
x_wsize=y_wsize=0;    /* ikkunan fyysinen koko */
x_whome=y_whome=0;    /* ikkunan fyysinen paikka */
x_metasize=y_metasize=0; /* ikkunan koko metatiedostossa */
y_ratio=1;
ps_emul=0;
//plot_id=0;
wst=0;
show_picture=1;
t_start=t_end=t_step=t=0;
filltype=0;  /* 0=- 1=FILL 2=YFILL 3=OFILL 4=IFILL */
fill_step=0;
fill_start=fill_end=0;
x_fill=y_fill=0; /* fill-viivojen kiintopiste OFILL,IFILL */
nloop=0;
data=0;     /* 26.5.92 */
nvar=0;
obs=0;
integral_ind=0;
integral_const=integral_value=0;
out=0;
color_max=0;
lopetus=0;
kosketus=0;
n_mess=0;
char_pitch=char_height=char_width=0;
marker_type=marker_size=0;   /* from POINT-specification */
xdiv1=xdiv2=xdiv3=0;
ydiv1=ydiv2=ydiv3=0;
x_kuva=y_kuva=0;
kirjainlev=kirjainkork=0;
tikki=0;              /* tick-viivan pituus (min. viiva tai raon koko) */
scalespace=SCALESPACE;
xscalen=0;            /* skaala-arvojen lkm */
yscalen=0;            /* skaala-arvojen lkm */
frametype=0;          /* 0,1,2 */
cfunktio=0; /* 1=C-kielinen 0=tulkattava */
integral_function=0;
xmin=xmax=ymin=ymax=xmumin=xmumax=ymumin=ymumax=0;
videomode=0;
y_const=0;
gg_char=0;
gg_marker_type=gg_marker_size=0;
pieborder=0;
x_origin=y_origin=0;
char_color=0;
line_type=0;
line_width=1;
line_color=0;
background=1;
mark_type=-1; mark_size=1; mark_color=2;
fill_interior=1; fill_color=0; fill_style=0;
fonts_on=0;
ycharwidth=0.0;  // muita varten
font_weight=400;
font_italic=0;
x_move=0; // 7.6.2002
y_move=0;
rotation=0;
autom_color=0.0; // 16.11.2002
overlay=0;
alaviivat_pois=1;
line_slow=0; // 31.3.2010
n_sana=0;           /* koodisanojen lukumÑÑrÑ */
pr_type=0;          /* 1=PS, 0=muu */
yscalepos1=yscalepos2=0;
scalemove_x=scalemove_y=0; /* 12.2.1993 */
tickturn=0;  /* 24.9.1993 */
pyramid=0; // 18.10.2005
markermax=12;
shademax=7;

n_pens=0;
n_brushes=0;
stock_pen=0;
valittu_rgb=0;
marker_rot_var=0;
marker_rot_angle=0;
arrowlen=0;


/* RS REM
     split(szCmdLine,s,4);
     strcpy(siirtop,s[0]);
     strcpy(plot_id,s[1]);
     strcpy(layout,s[2]);
     s_init0(siirtop,0);
     s_opt(s[3]);  // info_s opt-tarkistuksiin!
*/

      
     strcpy(layout,gplot_layout);
     plot_id=id;

     sprintf(x,"%sT.TMP",etmpd);
     temp2=muste_fopen(x,"wt");
     fprintf(temp2,"\nGPLOT check list:");
     
     muste_gplot_init=1;
     i=sp_init(r1+r-1);
     muste_gplot_init=0;
     
     
     if (i<0)
         {
         strcpy(info,"PLOT: Too many specifications!");
         s_end(siirtop);
         return(-1);
         }

     k=0;

     sur_screen_dim(&par[0],&par[1]);
     
     sp_add_value("WX",(double)par[0]);
     sp_add_value("WY",(double)par[1]);     
     
     xs=par[0]/DEFSCREENXSIZE; 
     ys=par[1]/DEFSCREENYSIZE;    

     x_whome=DEFWINXHOME*xs; y_whome=DEFWINYHOME*ys;
     x_wsize=DEFWINXSIZE*xs; y_wsize=DEFWINYSIZE*ys;

     i=spfind("WSIZE");
     if (i>=0)
         {
         strcpy(x,spb[i]); i=split(x,s,2);
         if (i==2)
             { x_wsize=arit_atoi(s[0]); y_wsize=arit_atoi(s[1]); ++k; }
         }

     i=spfind("WHOME");
     if (i>=0)
         {
         strcpy(x,spb[i]); i=split(x,s,2);
         if (i==2)
             { x_whome=arit_atoi(s[0]); y_whome=arit_atoi(s[1]); ++k; }
         }

     wstyle=1; wst=1;
     i=spfind("WSTYLE");
     if (i>=0)
         {
         if (atoi(spb[i])==0)
             wst=0;
         }

     *mouse_file_name=EOS;
     i=spfind("MOUSE");
     if (i>=0)
         {
         strcpy(mouse_file_name,spb[i]);
         }

     show_picture=1;
     i=spfind("SHOW");
     if (i>=0) show_picture=atoi(spb[i]);

     if (!k) if(use_layout(layout,plot_id)<0) return(0);

     set_metasize();
     x_size=x_metasize;  y_size=y_metasize;
// fprintf(temp2,"\nmetasize: %d %d",x_metasize,y_metasize);


     sprintf(meta_name,"%sSUR%s%d.MOF",etmpd,sur_session,plot_id);
     i=muste_open_outfile(meta_name); if (i<0) return(-1);
     
/*
     wndclass.cbSize        = sizeof (wndclass) ;
     wndclass.style         = CS_HREDRAW | CS_VREDRAW ;
     wndclass.lpfnWndProc   = WndProc ;
     wndclass.cbClsExtra    = 0 ;
     wndclass.cbWndExtra    = 0 ;
     wndclass.hInstance     = hInstance ;
     wndclass.hIcon         = LoadIcon (hInstance, "GPLOT") ;
     wndclass.hCursor       = LoadCursor (NULL, IDC_ARROW) ;
     wndclass.hbrBackground = (HBRUSH) GetStockObject (WHITE_BRUSH) ;
     wndclass.lpszMenuName  = NULL ;
     wndclass.lpszClassName = szAppName ;
     wndclass.hIconSm       = LoadIcon (hInstance,"GPLOT" ) ;
*/


     i=muste_get_window_caption(); // GetSystemMetrics(SM_CYCAPTION); // palkin korkeus
     if (wst==0) i=0;
     iXframe=muste_get_window_xframe(); // GetSystemMetrics(SM_CXFRAME);  // 3.2.2002
     iYframe=muste_get_window_yframe(); // GetSystemMetrics(SM_CYFRAME);
     if (wst==0) iXframe=iYframe=0;

     top=0; k=spfind("TOP"); // k oli i -2.2.2002
     if (k>=0) top=atoi(spb[k]);
     transparent=0; k=spfind("TRANSPARENT");
     if (k>=0) transparent=atoi(spb[k]);

// fprintf(temp2,"\nx_wsize=%d y_wsize=%d|",x_wsize,y_wsize);


     x_wsize2=DEFWINXSIZE;
     if (x_wsize!=DEFWINXSIZE) x_wsize2=x_wsize+2*iXframe;
     y_wsize2=DEFWINYSIZE;
     if (y_wsize!=DEFWINYSIZE) y_wsize2=y_wsize+i+iYframe;
     
      *x=EOS;
     if (*sur_session!='A') strcpy(x,sur_session);
     sprintf(sbuf,"%s%d: %s  Graphics",x,plot_id,system_name);

     muste_create_plotwindow(id,sbuf);

     i=spfind("BACK");
     if (i>=0 && atoi(spb[i])==0)
       muste_fixme("FIXME: BACK for setting background color missing\n");
//       wndclass.hbrBackground = (HBRUSH) GetStockObject (NULL_BRUSH) ;

     if (wst==0) muste_window_style(id,wst);

     muste_flushscreen();    

if (muste_strcmpi(parm[1],"RND")==0)
	{
	i=atoi(parm[2]);
	for (k=0; k<i; k++)
		{
		rnd1=uniform(0)*x_wsize;
		rnd2=uniform(0)*y_wsize;
		rnd3=uniform(0)*x_wsize;
		rnd4=uniform(0)*y_wsize;
		muste_line_plot(id,rnd1,rnd2,rnd3,rnd4);
		}
	}
else if (strchr(parm[1],'=')!=NULL || muste_strcmpi(parm[1],"INTEGRAL")==0)
	{
	muste_gplot_curves();
	}




     if (!top && etu!=2) muste_focus_from_plotwin_to_editor(id);

     
/*
     if (top)
         hwnd = CreateWindowEx(WS_EX_TOPMOST,szAppName, sbuf,
                              wstyle,
                              x_whome,y_whome,
                        //    x_wsize+2*iXframe,y_wsize+i+iYframe,
                              x_wsize2,y_wsize2,
                              NULL, NULL, hInstance, NULL) ;
     else if (transparent)
         hwnd = CreateWindowEx(WS_EX_TRANSPARENT,szAppName, sbuf,
                              wstyle,
                              x_whome,y_whome,
                              x_wsize2,y_wsize2,
                              NULL, NULL, hInstance, NULL) ;
     else
         hwnd = CreateWindow (szAppName, sbuf,
                              wstyle,
                              x_whome,y_whome,
                              x_wsize2,y_wsize2,
                              NULL, NULL, hInstance, NULL) ;
                              
     if (wst==0)
         SetWindowLong(hwnd,GWL_STYLE,0); // 7.11.2000 palkki pois!
                                          // kun WSTYLE=0
     if (wst)
         {
         if (show_picture) ShowWindow (hwnd, iCmdShow);
         else              ShowWindow(hwnd, SW_MINIMIZE);
         UpdateWindow (hwnd) ;
         }
     if (wst==0)
              {
              ShowWindow(hwnd, SW_MINIMIZE);
              UpdateWindow (hwnd) ;  // tarpeeton?
              SetWindowLong(hwnd,GWL_STYLE,0);
              if (show_picture) ShowWindow (hwnd, SW_SHOWNORMAL);
              else              ShowWindow(hwnd, SW_MINIMIZE);
              UpdateWindow (hwnd) ;

// yritys 24.1.2001   SendMessage(hwnd,WM_KILLFOCUS,NULL,NULL):
              }

*/


/*

     while (GetMessage (&msg, NULL, 0, 0))
          {
          TranslateMessage (&msg) ;
          DispatchMessage (&msg) ;
          }

     return msg.wParam ;
*/

// muste_outfile_plot();

               i=spfind("OUTFILE");
               if (i>=0)
                   {
                   strcpy(sbuf,spb[i]);
                   }
               else
                   {
                   sprintf(sbuf,"-");
                   }

		muste_close_outfile(sbuf);

fprintf(temp2,"\nLOPPU!");
               fclose(temp2); // 3.12.2000

return(1);
}

static int gplot_del(int m)
        {
        int i,j,k;

        if (m==2) first_plot_number=1; // 18.1.2001

        if (m==2 || muste_strcmpi(parm[2],"ALL")==0)
            {
            for (k=first_plot_number; k<=MAX_HDL; ++k)
                {
                muste_delete_plotwindow(k);
                }
            gplot_count=first_plot_number-1;
            fixed_plot=0;
            return(0);
            }
        i=atoi(parm[2]);
        if (i==0) { gplot_count=0; return(1); }
        muste_delete_plotwindow(i);
        return(i);
        }
        
static int gplot_layout_find(char *layout)
        {
        char name[LNAME];
        int i,k;

        if (*gplot_layout==EOS)
            { max_hdl=MAX_HDL; strcpy(layout,"-"); return(1); }

        strcpy(name,gplot_layout);
        
        gpl=muste_fopen(name,"rt");
        if (gpl==NULL)
        	{
/*        
        subst_survo_path_in_editor(name); // 24.3.2002
        if (strchr(name,':')==NULL && !netd(name))
                                   // 17.2.2006
*/            
            sprintf(name,"<Survo>/SYS/%s",gplot_layout);
             gpl=muste_fopen(name,"rt");
// printf("\ngplot_layout=%s|",name); getck();
			}

        if (gpl==NULL)
            {
            sprintf(sbuf,"Cannot find GPLOT layout file %s !",name);
            sur_print(sbuf); WAIT; return(-1);
            }
        fgets(sbuf,100,gpl);
        i=atoi(sbuf);
        if (i<=0 || i>MAX_HDL) return(1);
        fclose(gpl);
        max_hdl=i;
        for (k=max_hdl+1; k<=MAX_HDL; ++k)
            {
            muste_delete_plotwindow(k);
/*            
            if (hdl[k]!=0)
                {
                sur_terminate_process(k+1,hdl[k],hdl2[k]);
//              sur_terminate_process(hdl[k]);
//              sur_close_handle(hdl2[k]);
                hdl[k]=0;               
                }
*/                 
            }
        strcpy(layout,name);
        return(1);
        }        

static int gplot_lay()
        {
        int i;
        char x[LNAME];

        if (g<3 || *parm[2]=='-') { *gplot_layout=EOS; return(1); }
        if (*parm[2]=='?')  // 5.6.2001
            {
            i=r1+r-1;
            edwrite(space,i,1);

            strcpy(x,gplot_layout); // 30.11.2001
            unsubst_survo_path_in_editor(x);
            sprintf(sbuf,"GPLOT /LAYOUT %s",x);
            edwrite(sbuf,i,1);
            return(1);
            }
        strcpy(gplot_layout,parm[2]);
        i=gplot_layout_find(gplot_layout); // 2.2.2001
        return(i);
        }

static int gplot_show()   // GPLOT /SHOW MIN/RESTORE/MAX #
        {
        int i,show;
        int k;

        if (g<4) { op_incomplete(); return(1); }
        if (muste_strnicmp(parm[2],"MIN",3)==0) show=0;
        else if (muste_strnicmp(parm[2],"MAX",3)==0) show=2;
        else show=1; // default

        for (k=3; k<g; ++k)  // 31.1.2003
            {
            i=atoi(parm[k])-1;
            sur_show_window(i+1,show);
            sur_sleep(10L);
            }
            
        return(1);
        }


static int gplot_next() // GPLOT /NEXT <number>
        {
        int i;

        if (g<3) { i=gplot_count+1; if (i>max_hdl) i=first_plot_number; }
        else
            {
            i=atoi(parm[2]); if (i<1 || i>max_hdl) return(1);
            }
        gplot_count=i-1; fixed_plot=0; return(1);
        }

static int gplot_which() // GPLOT /WHICH   / tells # of the next window 22.02.01
        {
        int i,j;

        j=r1+r-1;
        if (fixed_plot) i=fixed_plot_number;
        else
            {
            i=gplot_count+1;
            if (i>max_hdl) i=first_plot_number;
            }
        sprintf(sbuf,"GPLOT /WHICH %d",i);
        edwrite(space,j,1);
        edwrite(sbuf,j,1);
        return(1);
        }

static int gplot_fix() // GPLOT /FIX <number>
        {
        int i;

        if (g<3) { i=gplot_count+1; if (i>max_hdl) i=first_plot_number; }
        else
            {
            i=atoi(parm[2]); if (i<1 || i>max_hdl) return(1);
            }
            
        fixed_plot_number=i; fixed_plot=1; return(1);
        }

static int gplot_first() // GPLOT /FIRST <number>
        {
        int i;

        i=atoi(parm[2]); if (i<1 || i>max_hdl) return(1);
        first_plot_number=i; gplot_count=i-1; fixed_plot=0; return(1);
        }

static int gplot_previous() // GPLOT /PREVIOUS
        {
        if (gplot_count) --gplot_count;
        else gplot_count=max_hdl-1;
        return(1);
        }


int op_gplot(char *op)
        {
        int i;
        char x[LLENGTH],*osa[2];
        char opfile[LNAME];
        char *p;
        char siirtop[LNAME];
        char command[LLENGTH];
        char layout[LNAME];
        

        odota_tuloksia=0;
/*
        fixed_plot=0;
        fixed_plot_number=1;
        first_plot_number=1;
*/

        if (g==1)
            {
            sur_print("\nIncomplete GPLOT operation!");
            WAIT; return(0);
            }

        s_init();

        if (muste_strnicmp(parm[1],"/DEL",4)==0) { gplot_del(1); s_end(); return(0); }
        if (muste_strnicmp(parm[1],"/LAYOUT",7)==0) { gplot_lay(); s_end(); return(0); }
        if (muste_strnicmp(parm[1],"/SHOW",5)==0) { gplot_show(); s_end(); return(0); }
        if (muste_strnicmp(parm[1],"/NEXT",5)==0) { gplot_next(); s_end(); return(0); }
        if (muste_strnicmp(parm[1],"/FIX",4)==0) { gplot_fix(); s_end(); return(0); }
        if (muste_strnicmp(parm[1],"/FIRST",6)==0) { gplot_first(); s_end(); return(0); }
        if (muste_strnicmp(parm[1],"/PREV",5)==0) { gplot_previous(); s_end(); return(0); }
        if (muste_strnicmp(parm[1],"/WHICH",6)==0) { gplot_which(); s_end(); return(0); }

//      if (strnicmp(parm[1],"/TOP",4)==0) { gplot_top(); return(0); }

        i=spec_find("TYPE",x,LLENGTH-1);
        if (i>=0)
            {
            split(x,osa,1);
            if (strcmp(osa[0],"CONTOUR")==0 || strcmp(osa[0],"MATRIX")==0)
                   {
                   strcpy(info,osa[0]); strcpy(op,"CONTOUR"); // return(1);
                   }
            if (strcmp(osa[0],"FACES")==0 || strcmp(osa[0],"ANDREWS")==0
                || strcmp(osa[0],"DRAFTS")==0 || strcmp(osa[0],"STARS")==0
                || strcmp(osa[0],"PROFILES")==0)
                   {
                   strcpy(info,osa[0]); strcpy(op,"FACES");
                   odota_tuloksia=1;  // tarkempi erittely puuttuu!
//                   return(1);
                   }
            }
            
            
        if (muste_strcmpi(parm[1],"FILE")==0)
            { strcpy(op,"FILE"); }

        if (muste_strcmpi(parm[0],"GHISTO")==0 || muste_strcmpi(parm[0],"HISTOG")==0)
            { strcpy(op,"PHIS"); odota_tuloksia=1; }
        if (strchr(parm[1],'=')!=NULL || muste_strcmpi(parm[1],"INTEGRAL")==0)
            { strcpy(op,"PCUR"); }
/*      if (strcmpi(parm[1],"FUNCTION")==0) suorita("PFUNC",argv[1]); */
        if (muste_strcmpi(parm[1],"/PALETTE")==0) { strcpy(op,"FILE"); }


        strcpy(op,"PDIA");
        if (g<3) { strcpy(op,"PBAR"); }

        
        strcpy(layout,gplot_layout);

        gplot_layout_find(layout);
        if (gplot_count>=max_hdl) gplot_count=first_plot_number-1;
        ++gplot_count;
        if (fixed_plot) gplot_count=fixed_plot_number;
        
        
//        sprintf(sbuf,"%d",gplot_count);

/*
        if (hdl[gplot_count-1])
            {
   sur_terminate_process(gplot_count,hdl[gplot_count-1],hdl2[gplot_count-1]);
            hdl[gplot_count-1]=0;
            }
*/

//  sprintf(command,"%s %s %s %s %s",opfile,siirtop,sbuf,layout,info_s);
// printf("\ncommand: %s|",command); getck();
        if (odota_tuloksia)
            {
            sprintf(x,"%s%s_HIS.TMP",etmpd,sur_session);
            sur_delete1(x);
            }
/*
        i=sur_create_process(command,&hdl[gplot_count-1],
                                     &hdl2[gplot_count-1] );
// printf("hdl: %d %lu|",gplot_count,hdl[gplot_count-1]); getck();
*/
        muste_gplot(gplot_count);

        if (odota_tuloksia)
            {
            PR_EIN2;
            sur_print("\n*** WAIT! (Interrupt by '.') ***");
              while(1)
                {
                if (sur_kbhit()) break;
                sur_sleep(100);
                his=muste_fopen(x,"rt");
                if (his==NULL) continue;
//                restore_dump();
                fclose(his);
                break;
                }
            }



        
        return(1);
        }








/************************ ei toiminut 22.12.2000
gplot_top() // GPLOT /TOP <number>
        {
        int i;

        i=atoi(parm[2]); if (i<1 || i>max_hdl) return(1);
        sur_bring_window_to_top(i);
        return(1);
        }
*******************************************/
