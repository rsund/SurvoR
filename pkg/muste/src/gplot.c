#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>
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
//#define MAXTEXTS 32

#define MAXLOOP 10
#define N_MESS 10
#define MAXSCALELIST 100

#define MAXPITUUS 100
#define MAXARG 10

#define NPEN 1000
#define NBRUSH 200

int muste_gplot_init=0;
int muste_gplot_init2=0;
char muste_charcolor[MAXPITUUS]="#000";
char muste_pencolor[MAXPITUUS]="#000";

extern char **spa,**spb,**spshad,**spb2;
extern int spn;
extern double *arvo;
extern char *spl;
extern char *spp;
extern int specmax;


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

int muste_x_wsize,muste_y_wsize,muste_x_size,muste_y_size;

static unsigned long hdl[MAX_HDL];
static unsigned long hdl2[MAX_HDL];
int max_hdl=MAX_HDL;

static FILE *his;
static FILE *gpl;
static char cur_data[LNAME];

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

//static int data=0;     /* 26.5.92 */
//static int nvar=0;
static char cur_data[LNAME];
static SURVO_DATA curd;
static int curd_var[MAXLOOP];
static int sp_ind[MAXLOOP];
// static int lag[MAXLOOP];  /* 21.6.92 */
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

//static int lopetus=0;
//static int kosketus=0;

//static int n_mess=0;
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
//static int scalespace=SCALESPACE;
static char xscales[SCALESPACE], *xscal[NPAR];
static double xscaleval[NPAR];
static int xscalen;            /* skaala-arvojen lkm */
static char yscales[SCALESPACE], *yscal[NPAR];
static double yscaleval[NPAR];
static int yscalen;            /* skaala-arvojen lkm */
static int frametype;          /* 0,1,2 */
//static int shadeval[SHADEMAX], shadepull[SHADEMAX], shadecolor[SHADEMAX];
//static unsigned char code[512];
static char *pen_code;         /* PEN=pen_code */
static char *line_code;        /* LINETYPE=line_code */

char muuttujanimi[LLENGTH];
char muuttujanimi2[LLENGTH];
char muuttujanimi3[LLENGTH]="x";
char muuttujanimi4[LLENGTH]="y";

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
//static int line_color=0;
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

//static double autom_color=0.0; // 16.11.2002

static int overlay;
//static int alaviivat_pois=1;

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
//static int pyramid=0; // 18.10.2005

static char framecode[LLENGTH];

static char *shadow[256];    /* varjorivin merkkien koodisanaosoittimet */
static char *shadow2[256];   /* varjorivin merkkien jÑlkikoodisanaosoittimet */


// static int markermax=12;
// static int shademax=7;   /*   SHADEMAX=16 */
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

static int p_init(char *laite);
static int p_error(char *s);
static int p_error2(char *s);
static void p_end();
static int p_wait();
static void p_clear();
static void p_newpage();
static int p_line(int x2,int y2,int i);
static int p_line2(int x1,int y1,int x2,int y2,int i);
static int p_line3(int x1,int y1,int x2,int y2,int i);
static int p_text(char *text,int x1,int y1,int i);
static void text_move_rot(int k);
static int p_text2(char *x,char *xs,int x1,int y1,int attr);
static int p_pen();
static int p_linetype();
static int p_fill(int x1,int y1,int fill);
static int p_fill_bar(int x1,int y1,int x2,int y2,int fill);
static int p_halfframe(int x1,int y1,int x2,int y2);
static int p_fill_polygon(int kerroin,char *s);
static int p_polygon_line(int n_poly,int fill);
static int p_polygon_line2(int fill,int i);
static void p_frame(int frtype);
static int p_fill_sector(int x0,int y0,double rx,double ry,double a1,double a2,int fill);
static int p_fillattr(int fill);
static int p_textcolors(int fill);
static int p_marker_color(int i);
static int p_marker_select(int i,int size);
static void p_marker_type_select(int i);
static int p_marker(int x2,int y2);
static int p_set_marker_color(char *s);
static int ps_cross(int x,int y,int i,int j); // RS dif from plot
static int ps_plus(int x,int y,int i,int j); // RS dif from plot
static int ps_triangle(int x,int y,int i); // RS only gplot
static int ps_diamond(int x,int y,int i); // RS only gplot
static int send_color();
static int p_special(char *s);
static int p_charsize();
static int p_textcontrol(char *s);
static int p_linecontrol(char *s);
static int p_origin(int x,int y);
static int tell_ps_unit();
static int send(char *s);
static int send2(char *x,char *xs);
static void ps_init();
static int ps_code(char *x,char **sana,int n,char *rivi);
static int ps_replace(char *x);
static int p_eps();
static int p_lineattr();
static int p_path(int nt,char **sana);
static int ps_fill(int fill);
static void p_open();
static void p_close();
static void p_save();
static void p_load();
static void vdc();
static void p_floodfill();
static void p_charcolor();
static void p_contour_init();
static void p_contour_plot(int ny,int iy,int nx,int *pxl_value);
static int lue_koodit(char *x);
static int plot_arrows();
static int pl_triangle(int x1,int y1,int x2,int y2,int x3,int y3,int t);

static int crt_select_pen();
static int crt_delete_pens();
static int p_koodimuunto(char *text);

extern void sur_get_textwidth();
extern int muste_ellipse_plot();

#include "plotvars.h"


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
	if (muste_outfile!=NULL) muste_fclose(muste_outfile); 
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
	/*
	double xkerroin,ykerroin;
	xkerroin=(double)((double)x_wsize/(double)x_size);
	ykerroin=(double)((double)y_wsize/(double)y_size);
	*/
	
//Rprintf("\nlineto, x_size: %d, x_wsize: %d, xkerroin: %g, ykerroin: %g",x_size,x_wsize,xkerroin,ykerroin);	
	muste_line_plot(plot_id,(double)x1,(double)y1,(double)x2,(double)y2);
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

static int muste_ellipse(int x1,int y1,int x2,int y2)
	{
	muste_ellipse_plot(plot_id,(double)x1,(double)y1,(double)x2,(double)y2);
	
	sprintf(sbuf,"ellipse %d %d %d %d",x1,y1,x2,y2);
	muste_send(sbuf);
	return (1);
	}	
	
	
/*  *  *  *  *  *  *  */	

static void sp_listaus(char *s)
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
        sp_listaus(muuttujanimi2);    /* spa[1] */
        sp_listaus(muuttujanimi3); // RS CHA was "x"
        sp_listaus(muuttujanimi4); // RS CHA was "y"
        return(spn);
        }



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

static int p_text2(char *x,char *xs,int x1,int y1,int attr)
	{

        int i,k,len,slen,h,j;
        unsigned char varjo;
        char *p;
        char y[LLENGTH], yy[LLENGTH];
        int xp,yp;
		int size[2]; // RS CHA SIZE size;
//        extern unsigned char *shadow[],*shadow2[];

// fprintf(temp2,"\np_text2: %s",x);
        if (xs==NULL) { i=p_text(x,x1,y1,attr); return(i); }
        pilkku_muunto(x); // muutettu 13.10.2002
/************************
        if (alaviivat_pois)
            {
            p=x; while ((p=strchr(p,'_'))!=NULL)  *p=' ';
            }
**************************/
        p_koodimuunto(x);

        len=strlen(x);
        x[len]=EOS; xs[len]=EOS;
        i=0;
        xp=x1+x_move; yp=y_const-y_move-y1-(int)(1.00*char_height);
        while ((unsigned char)xs[i])
            {
            varjo=(unsigned char)xs[i];
            p=shadow[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow[varjo]);
                muunna(y,yy);
                crt_select_pen();
                }
            k=0;
            while ((unsigned char)xs[i]==varjo) { y[k]=x[i]; ++k; ++i; }
            y[k]=EOS;
// fprintf(temp2,"\nvarjo=%c y=%s",varjo,y);



    		muste_text_plot(plot_id,(double)xp,(double)yp,y);

			sprintf(sbuf,"text %d %d \"%s\"",(int)xp,(int)yp,y);
			muste_send(sbuf);

//            TextOut(hdcMeta,xp,yp,y,strlen(y));
//            GetTextExtentPoint32(hdcMeta,y,strlen(y),&size);

            sur_get_textwidth(y,size);
            xp+=size[0];

            p=shadow2[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow2[varjo]);
                muunna(y,yy);
                crt_select_pen();
                }
            }
	
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

static void muste_setpixel(int x, int y)
	{
	muste_line(x,y,x,y);
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


//static int p_error(char *s)
//    {
//    char perror[LLENGTH];
/* RS CHA
    char name[LNAME];
    extern char sur_session[];

    sprintf(name,"%s%sERR.TMP",etmpd,sur_session);
    err_msg=muste_fopen(name,"wt");
    fprintf(err_msg,"GPLOT error: %s",s);
    muste_fclose(err_msg);
*/
//    sprintf(perror,"\nGPLOT error: %s",s);
//	sur_print(perror);
//	WAIT;
    
// RS FIXME Check that exit is OK   ExitProcess(0);
//    return(-1);
//    }

/* RS REM
static int p_error2(char *s)
 { return(p_error(s)); }


static void sp_virhe(char *a,char *b)
        {
        sprintf(sbuf,"\nError in specification %s=%s",a,b);
        p_error(sbuf); // RS CHA p_error2 -> p_error
//        WAIT;
        }

 
static void f_tuntematon(char *s)
        {
//      Rprintf("\nUnknown function %s\n",s);
        sprintf(sbuf,"Unknown function %s",s);
        p_error(sbuf);
        l_virhe=1;
        }
 
static void arg_virhe(char *s)
        {
//      Rprintf("\n%s: Error in arguments\n",s);
        sprintf(sbuf,"%s: Error in arguments",s);
        p_error(sbuf);
        l_virhe=1;
        }

static void syntax_error(char *s)
        {
//      Rprintf("\nsyntax error in %s\n",s);
        sprintf(sbuf,"syntax error in %s",s);
        p_error(sbuf);
        l_virhe=1;
        }
 */
 
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
 
static int p_origin(int x,int y)
        {
        return(1);
        }

static void p_end()
        {
// fprintf(temp2,"\nP_END");
        crt_delete_pens();

        return;
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

static int p_linetype()
        {

        int i;
        char x[LLENGTH], y[3*LLENGTH];

        if (line_code==NULL) strcpy(x,"[LINE]");
        else                 strcpy(x,line_code);
        i=muunna(x,y);
        if (i<0) return(-1);
        crt_select_pen();
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
        
static int p_fill(int x1,int y1,int fill)
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

static void p_floodfill() {}
static void vdc() {}

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


static int p_fill_polygon(int kerroin,char *s)
    {
    
 /*   
    int i,k,n;
    double pointx,pointy;
    char *ss[65];
    int fill;

    i=split(s,ss,65);
    n=i/2;
    for (k=0; k<n; ++k)
        {
        pointx=(double)kerroin*arit_atof(ss[2*k]);
        pointy=(double)kerroin*arit_atof(ss[2*k+1]);
        if (k==0)
            {
            sprintf(sbuf,"gsave newpath %g m %g m moveto\n",
                                     pointx,pointy); send(sbuf);
            }
        else
            {
            sprintf(sbuf,"%g m %g m lineto\n",
                                     pointx,pointy); send(sbuf);
            }
        }
    if (i<4 || n==2)
        {
        sprintf(sbuf,"closepath stroke grestore\n"); send(sbuf);
        return(1);
        }
    if (i%2!=0)
        {
        fill=arit_atoi(ss[i-1]);
        if (fill>=0 && !colors_2010)
            {
            current_fill=1.0-(double)fill/shademax;
            sprintf(sbuf,"closepath gsave %g setgray fill grestore stroke grestore\n",
                         current_fill);
            }
        else if (fill>-1000)
            {
            p_fillattr(fill);
sprintf(sbuf,"closepath gsave f_cyan f_mage f_yell f_black setcmykcolor fill grestore stroke grestore\n");
            }
        else
            {
            current_fill=1.0;
            sprintf(sbuf,"closepath stroke grestore\n");
            }
        send(sbuf);
        }
    else
        {
        sprintf(sbuf,"closepath stroke grestore\n");
        send(sbuf);
        }
*/        
    return(1);
    }
    

static int p_error(char *s)
        {
        char x[LLENGTH];

        sprintf(x,"\nGPLOT error: %s",s);
        sur_print(x); WAIT;
        return(-1); // RS CHA exit(0);
        }

// static int p_error2(char *s)  { p_error(s); return(1); }
static int p_error2(char *s) { return(p_error(s)); }	


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
//    valittu_rgb=RGB(vari2[0],vari2[1],vari2[2]);

// RS CHA:
	sprintf(muste_pencolor,"#%.2x%.2x%.2x",
       (unsigned char)vari2[0],
       (unsigned char)vari2[1],
       (unsigned char)vari2[2]);  
       
	sprintf(sbuf,"pencolor %s",muste_pencolor);
	muste_send(sbuf);       

/* RS NYI FIXME 
    hPens[n_pens]=CreatePen(PS_SOLID,line_width,valittu_rgb);
// fprintf(temp2,"\nhPens=%u",hPens[n_pens]);
    SelectObject(hdcMeta,hPens[n_pens]);
*/    
    
    line_color=-1; p_charcolor();
    ++n_pens;

    return(1);
    }
    

static int p_markattr()
        {
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
//    valittu_rgb=RGB(vari[line_color][0],vari[line_color][1],vari[line_color][2]);    
// RS NYI FIXME     hPens[n_pens]=CreatePen(PS_SOLID,line_width,valittu_rgb);

// RS CHA:
    sprintf(muste_charcolor,"#%.2x%.2x%.2x",
          (unsigned char)vari[line_color][0],
          (unsigned char)vari[line_color][1],
          (unsigned char)vari[line_color][2]);          
    }

 else // line_color<0
    {
//    valittu_rgb=RGB(vari2[0],vari2[1],vari2[2]);
// RS NYI FIXME     hPens[n_pens]=CreatePen(PS_SOLID,line_width,valittu_rgb);

// RS CHA:
    sprintf(muste_charcolor,"#%.2x%.2x%.2x",
       (unsigned char)vari2[0],
       (unsigned char)vari2[1],
       (unsigned char)vari2[2]);  

    }
// fprintf(temp2,"\npen=%d hPen=%ld",n_pens,hPens[n_pens]);
// RS NYI FIXME     SelectObject(hdcMeta,hPens[n_pens]);

	sprintf(sbuf,"charcolor %s",muste_pencolor);  // pencolor???
	muste_send(sbuf);   
	
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
            sprintf(sbuf,"\nError in %% code %s",x); sur_print(sbuf);
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
        sur_print(sbuf); WAIT; // RS CHA Rprintf -> sur_print
        
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
        muste_fclose(pal);
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

      char_height=14.0; char_width=8.0;  // RS FIXME check font size
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
        muste_x_size=x_size=x_metasize; muste_y_size=y_size=y_metasize;
        y_const=y_size;
        xdiv1=300.0; xdiv2=1000.0; xdiv3=200.0;
        ydiv1=300.0; ydiv2=1000.0; ydiv3=200.0;
        x_origin=VASEN_REUNA; y_origin=ALAREUNA;
        i=spfind("TICKLENGTH");
        if (i>=0) tikki=atoi(spb[i]);

        p_charsize();


// RS NYI        p_back2();
// RS NYI        p_edit();

        crt_select_brush();

		muste_x_wsize=x_wsize; muste_y_wsize=y_wsize; // RS ADD
        return(1);
        }

static void p_frame(int frtype)
        {
        char x[LLENGTH];

        if (*framecode)
            {
            strcpy(x,framecode); p_linecontrol(x);
            }
        else p_linetype();

//        if (frtype>=2 && frtype<6) p_fill_bar(x_home,y_home,x_home+x_size,y_home+y_size,-1000);
//        if (frtype==5 || frtype==6) p_halfframe(xx,yy,xx+x_kuva,yy+y_kuva);
//        if (frtype>0 && frtype<3) p_fill_bar(xx,yy,xx+x_kuva,yy+y_kuva,-1000);

if (frtype>=2 && frtype<6) plot_box(x_home,y_home,x_size,y_size);
if (frtype==5 || frtype==6) plot_halfbox(xx,yy,x_kuva,y_kuva); // 27.11.02
if (frtype>0 && frtype<3) plot_box(xx,yy,x_kuva,y_kuva);

        }

static void p_charcolor()
        {
//        muste_fixme("\nFIXME: p_charcolor() not implemented!"); // RS FIXME

        if (line_color>=0) sprintf(muste_charcolor,"#%.2x%.2x%.2x",
          (unsigned char)vari[line_color][0],
          (unsigned char)vari[line_color][1],
          (unsigned char)vari[line_color][2]);
/*        
            SetTextColor(hdcMeta,
       RGB(vari[line_color][0],vari[line_color][1],vari[line_color][2]));
*/       
        else
            {
            crt_select_pen(line_color);
			sprintf(muste_charcolor,"#%.2x%.2x%.2x",
            (unsigned char)vari2[0],
            (unsigned char)vari2[1],
            (unsigned char)vari2[2]);            
/*
            SetTextColor(hdcMeta,
                  RGB(vari2[0],vari2[1],vari2[2]));
*/                  
            }

	sprintf(sbuf,"charcolor %s",muste_charcolor);
	muste_send(sbuf); 
            
        return;
        }

static int p_lineattr()
        {

        crt_select_pen();

/******************
        _setcolor((int)g_color[line_color]);
        _setlinestyle(line_style[line_type]);
*************/
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


static int p_textcolors(int fill) // Needed in GPLOT  16.9.2010
    { return(p_marker_color(fill)); }

static int p_marker_color(int i)
    {
    line_color=fill_color=i; crt_select_pen(); crt_select_brush(); return(1);
    return(1);
    }
    
static int p_fill_sector(int x0,int y0,double rx,double ry,double a1,double a2,int fill)
        {
/*        
        char s[LLENGTH];
        int i,pen;
        double k1,k2;

        k1=180*a1/PI; k2=180*a2/PI;
        sprintf(s,"gsave newpath %d m %d m moveto %d m %d m %g m %g %g arc\n",
                    x0,y0,x0,y0,rx,k1,k2); send(s);
        if (fill>=0 && !colors_2010)
            {
            current_fill=1.0-(double)fill/shademax;
            sprintf(s,"closepath gsave %g setgray fill grestore stroke grestore\n",
                          current_fill);
            }
        else
            {
            p_fillattr(fill);
sprintf(s,"closepath gsave f_cyan f_mage f_yell f_black setcmykcolor fill grestore stroke grestore\n");
            }
        send(s);
        x_pos=x0; y_pos=y0;
*/        
        return(1);
        }
        
static int p_path(int nt,char **sana)
        {
/*        
        int i,xk,yk;
        char s[LLENGTH];
        extern double arit_atof();

        xk=arit_atoi(sana[0])+x_home; yk=arit_atoi(sana[1])+y_home;
        sprintf(s,"newpath %d m %d m moveto \n",xk,yk); send(s);

        for (i=2; i<nt-1; i+=2)
            {
            xk=arit_atoi(sana[i]); yk=arit_atoi(sana[i+1]);
            sprintf(s,"%d %d rl\n",xk,yk); send(s);    
            }

        if (fill_index==-1000)
            {
            send("stroke\n");
            return(1);
            }
        send("closepath gsave ");
        ps_fill(fill_index);
        send("fill grestore stroke\n");
*/        
        return(1);
        }
        
static void p_contour_init()
        {
/*        
        int i;
        char s[LLENGTH], *osa[3];
        char x[LLENGTH];

        send("/setF { currentscreen 4 -2 roll pop 3 1 roll setscreen } def\n");
        send("/setA { /a exch def currentscreen 3 1 roll pop a 3 -1 roll setscreen } def\n");

        send("0 setA\n");   
        ps_negative=0;
        i=spfind("SCREEN");  
        if (i>=0)
            {
            strcpy(s,spb[i]);
            i=split(s,osa,3);
            if (muste_strcmpi(osa[0],"NEG")==0) ps_negative=1;
            if (i>1)
                {
                cells_per_inch=atoi(osa[1]);
                sprintf(x,"%d setF\n",cells_per_inch); send(x);
                }
            if (i>2)
                {
                raster_angle=atof(osa[2]);
                sprintf(x,"%g setA\n",raster_angle); send(x);
                }
            }
*/            
        }

static void p_contour_plot(int ny,int iy,int nx,int *pxl_value)
        {
/*        
        char x[LLENGTH];
        double x_taso, y_taso;
        double x_koko, y_koko;
        int i,k;
        int taso;

        x_koko=xdiv2*x_size; y_koko=ydiv2*y_size;
        x_taso=xx;
        y_taso=yy+y_koko*(ny-nx)/ny-y_koko/ny*(double)iy;
        sprintf(x,"save %g m %g m translate\n",x_taso,y_taso); send(x);

        sprintf(x,"%g m %g m scale\n",x_koko,y_koko); send(x);

        sprintf(x,"%d 1 8\n",nx); send(x);
        sprintf(x,"[ %d 0 0 %d 0 %d ]\n",nx,-ny,nx); send(x);
        send("{ <");
        k=0;
        for (i=0; i<nx; ++i)
            {
            taso=pxl_value[i]%256;
            if (ps_negative) taso=255-taso;
            sprintf(x,"%02X",taso); send(x); 
            ++k; if (k>30) { k=0; send("\n"); }
            }
        send("> } image restore\n");
*/        
        }

static int p_wait()
        {
        return(1);
        }

static void p_newpage()
        {
//        send("gsave showpage grestore\n");
        }

static int p_marker_select(int i,int size)
        {
        if (i>markermax-1) i=0; gg_marker_type=i;
        gg_marker_size=size;
        return(1);
        }

static void p_marker_type_select(int i) // 28.5.2005
    {
    if (i>markermax-1) i=0; gg_marker_type=i;
    }

static int p_marker(int x2,int y2)
        {
        char s[LLENGTH];
        int i,j,sz,ysz;

        y2=y_const-y2;
        sz=gg_marker_size;
        if (sz<1)
            {
            muste_moveto(x2,y2); muste_lineto(x2+1,y2+1);
            }
//          { _setpixel(x2,y2); return(1); }

        if (sz>2) ysz=y_ratio*sz; else ysz=sz;
        muste_moveto(x2,y2);
        switch (gg_marker_type)
            {
          case 0:
// RS CHA            Ellipse(hdcMeta,x2-sz,y2-ysz,x2+sz,y2+ysz);
            muste_ellipse(x2-sz,y2-ysz,x2+sz,y2+ysz);
            break;
          case 2:
            i=0.7*sz; j=0.7*y_ratio*sz;
            ps_cross(x2,y2,i,j);
          case 1:
            ps_plus(x2,y2,sz,ysz);
            break;
          case 3:
// RS NYI FIXME           SelectObject(hdcMeta,GetStockObject(NULL_BRUSH));
// RS CHA           Ellipse(hdcMeta,x2-sz,y2-ysz,x2+sz,y2+ysz);
            muste_ellipse(x2-sz,y2-ysz,x2+sz,y2+ysz);
            crt_select_brush();
            break;
          case 4:
            ps_cross(x2,y2,sz,ysz);
            break;
          case 5:
// RS NYI FIXME           SelectObject(hdcMeta,GetStockObject(NULL_BRUSH));
// RS CHA           Rectangle(hdcMeta,x2-sz,y2-ysz,x2+sz,y2+ysz);
            muste_rectangle(x2-sz,y2-ysz,x2+sz,y2+ysz);
            crt_select_brush();
            break;
          case 6:
// RS CHA            Rectangle(hdcMeta,x2-sz,y2-ysz,x2+sz,y2+ysz);
            muste_rectangle(x2-sz,y2-ysz,x2+sz,y2+ysz);
            break;
          case 7:
// RS NYI FIXME            SelectObject(hdcMeta,GetStockObject(NULL_BRUSH));
            ps_triangle(x2,y2,sz);
            crt_select_brush();
            break;
          case 8:
            ps_triangle(x2,y2,sz);
            break;
          case 9:
// RS NYI FIMXME           SelectObject(hdcMeta,GetStockObject(NULL_BRUSH));
            ps_diamond(x2,y2,sz);
            crt_select_brush();
            break;
          case 10:
            ps_diamond(x2,y2,sz);
            break;
          case 11:
            muste_setpixel(x2,y2);
            break;
          default:
            muste_setpixel(x2+1,y2); muste_setpixel(x2,y2+1);
            muste_setpixel(x2-1,y2); muste_setpixel(x2,y2-1);
            muste_setpixel(x2-1,y2-1); muste_setpixel(x2+1,y2-1);
            muste_setpixel(x2-1,y2+1); muste_setpixel(x2+1,y2+1);

            }
        x_pos=x2; y_pos=y_const-y2;        
        
        return(1);
        }

static int ps_cross(int x,int y,int i,int j)
        {

        muste_moveto(x+i,y+j); muste_lineto(x-i,y-j);
        muste_moveto(x+i,y-j); muste_lineto(x-i,y+j);

        return(1);
        }

static int ps_plus(int x,int y,int i,int j)
        {
        muste_moveto(x,y+j); muste_lineto(x,y-j);
        muste_moveto(x+i,y); muste_lineto(x-i,y);
        return(1);
        }

static int ps_triangle(int x,int y,int i)
        {
        int ia,ib;

        ia=y_ratio*2.0/sqrt(3.0)*i; ib=y_ratio*sqrt(3.0)*i;
//        BeginPath(hdcMeta);
        muste_moveto(x,y-ia); muste_lineto(x-i,y+ib-ia); muste_lineto(x+i,y+ib-ia);
        muste_lineto(x,y-ia);
//        EndPath(hdcMeta);
//        FillPath(hdcMeta);

/* RS NYI
        BeginPath(hdcMeta); // erikseen piirrettÑvÑ reuna!
        _moveto(x,y-ia); _lineto(x-i,y+ib-ia); _lineto(x+i,y+ib-ia);
        _lineto(x,y-ia);
        EndPath(hdcMeta);
        StrokePath(hdcMeta);
*/        
        return(1);
        }

static int ps_diamond(int x,int y,int i)
        {
        int ia,ib;

        ia=i*sqrt(2.0); ib=y_ratio*i*sqrt(2.0);

//        BeginPath(hdcMeta);
        muste_moveto(x,y+ib); muste_lineto(x-ia,y); muste_lineto(x,y-ib);
        muste_lineto(x+ia,y); muste_lineto(x,y+ib);
//        EndPath(hdcMeta);
//        FillPath(hdcMeta);

/* RS NYI
        BeginPath(hdcMeta);
        _moveto(x,y+ib); _lineto(x-ia,y); _lineto(x,y-ib);
        _lineto(x+ia,y); _lineto(x,y+ib);
        EndPath(hdcMeta);
        StrokePath(hdcMeta);
*/
        return(1);
        }

        
static int plot_arrows()
    {
    return(1);
    }

static int p_polygon_line(int n_poly,int fill)
    {
    return(1);
    }

static int p_koodimuunto(char *text)
    {
    unsigned char *p;
    p=(unsigned char*)text; while (*p) { *p=code[(unsigned int)(*p)+256]; ++p; }
    return(1);
    }


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





static void muste_gplot_type()
{
	 int i;
	 char x[LLENGTH], *osa[1];
     extern int arguc;
     extern char *arguv[];	 
     char **argv;
     argv=arguv;
     
     
        i=spfind("TYPE");
        if (i>=0)
            {
            strcpy(x,spb[i]); split(x,osa,1);
            if (strcmp(osa[0],"CONTOUR")==0 || strcmp(osa[0],"MATRIX")==0)
                   {
                   strcpy(info,osa[0]);
                   s_end(argv[1]);                   
                   muste_contour(2,argv);
                   return;
                   }
            if (strcmp(osa[0],"FACES")==0 || strcmp(osa[0],"ANDREWS")==0
                || strcmp(osa[0],"DRAFTS")==0 || strcmp(osa[0],"STARS")==0
                || strcmp(osa[0],"PROFILES")==0)
                   {
                   strcpy(info,osa[0]);
                   s_end(argv[1]);                   
                   muste_faces(2,argv);
                   return;
                   }
            }
        if (muste_strcmpi(word[0],"GHISTO")==0) 
        	{ 
muste_fixme("\nGHISTO Bar graphs not yet implemented!");
        	return; 
        	}
        if (strchr(word[1],'=')!=NULL || muste_strcmpi(word[1],"INTEGRAL")==0)
            {             
            muste_pcur(2,argv); 
            return;
            }
        if (muste_strcmpi(word[1],"FUNCTION")==0) { muste_fixme("\nPFUNC.EXE NYI"); return; }
        if (muste_strcmpi(word[1],"FILE")==0) { muste_fixme("\nFILE.EXE NYI"); return; }

        if (g<3)
        	{   
        if (muste_strcmpi(word[1],"/FRAME")==0) { muste_pbar(2,argv); return; }
        	muste_pbar(2,argv); // RS CHA suorita("PBAR.EXE",argv[1]); 
        	return; 
        	}
        muste_pdia(2,argv); // RS CHA suorita("PDIA.EXE",argv[1]);
		return;
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
char_pitch=char_height=char_width=8;
marker_type=marker_size=0;   /* from POINT-specification */
xdiv1=xdiv2=xdiv3=0;
ydiv1=ydiv2=ydiv3=0;
x_kuva=y_kuva=0;
kirjainlev=kirjainkork=1;
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

rajat_etsitty=0; // *

strcpy(muste_charcolor,"#000000");
strcpy(muste_pencolor,"#000000");

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


muuttujanimi[0]=EOS; // RS ADD
strcpy(muuttujanimi2,"x"); // RS ADD
strcpy(muuttujanimi3,"y"); // RS ADD
muuttujanimi4[0]=EOS;

     muste_gplot_init=2;
     i=sp_init(r1+r-1);
     muste_gplot_init=0;
     
     
     if (i<0)
         {
         sur_print(info,"GPLOT: Too many specifications!"); WAIT;
         s_end(siirtop);
         return(-1);
         }

i=varaa_earg(); if (i<0) return(-1);    // RS ADD  	
     	      


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



/*************************

        i=hae_apu("plot_mode",x); 
        if (i>0)
            {
            if (strcmp(x,"PS")==0 || muste_strcmpi(x,"PostScript")==0) strcpy(path,"PS");
            else if (muste_strcmpi(x,"CRT")==0 || muste_strcmpi(x,"G")==0) strcpy(path,"G");
            else strcpy(path,x);
            }
        else
            strcpy(path,"PS");
        i=spfind("DEVICE");
        if (i>=0)
            {
            strcpy(x,spb[i]); p=strchr(x,','); if (p!=NULL) *p=EOS;
            if (muste_strcmpi(x,"CRT")==0 || muste_strcmpi(x,"G")==0) strcpy(path,"G");
            else if (*x=='P' || *x=='p') strcpy(path,"PS");

            else strcpy(path,x);
            }
*/

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
else muste_gplot_type();

     if (!top) muste_focus_from_plotwin_to_editor(id);
//     if (!top && etu!=2) muste_focus_from_plotwin_to_editor(id);

     
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
               muste_fclose(temp2); // 3.12.2000

return(1);
}

#include "plotfunc.h"


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
// Rprintf("\ngplot_layout=%s|",name); getck();
			}

        if (gpl==NULL)
            {
            sprintf(sbuf,"Cannot find GPLOT layout file %s !",name);
            sur_print(sbuf); WAIT; return(-1);
            }
        fgets(sbuf,100,gpl);
        i=atoi(sbuf);
        if (i<=0 || i>MAX_HDL) return(1);
        muste_fclose(gpl);
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
 
for (i=0; i<NMAT; i++)
	{
  	mat_mat[i]=NULL;
  	mat_rlab[i]=NULL;
  	mat_clab[i]=NULL;
  	mat_lr[i]=0; mat_lc[i]=0;
  	mat_m[i]=0; mat_n[i]=0;
  	}
 
 mat_nmat=0;

/*
static double *mat[NMAT];
static char *rlab[NMAT],*clab[NMAT];
static int lr[NMAT],lc[NMAT];
static int m[NMAT],n[NMAT];
static int nmat=0;
static char mat_name_arit[NMAT][9];
*/  

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
// Rprintf("\ncommand: %s|",command); getck();
        if (odota_tuloksia)
            {
            sprintf(x,"%s%s_HIS.TMP",etmpd,sur_session);
            sur_delete1(x);
            }
/*
        i=sur_create_process(command,&hdl[gplot_count-1],
                                     &hdl2[gplot_count-1] );
// Rprintf("hdl: %d %lu|",gplot_count,hdl[gplot_count-1]); getck();
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
                muste_fclose(his);
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
