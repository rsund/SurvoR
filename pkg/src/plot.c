/* p.c 11.7.1986/SM (14.6.1992)
   PLOT operations
   
pcur.exe: 
*g:\ve1\p\pcur.obj 
*g:\ve1\p\pa.obj 
*g:\ve1\p\pb.obj \
*g:\ve1\p\pb2.obj
*g:\ve1\p\p2.obj \
g:\ve1\p\pgr2.obj 
*g:\ve1\p\psc.obj
g:\ve1\p\psc2.obj \
*g:\ve1\p\cur1.obj
*g:\ve1\p\cur2.obj
*g:\ve1\p\curfi.obj \
*g:\ve1\p\pmu.obj
g:\ve1\p\curspec.obj
g:\ve1\p\curarit1.obj \
g:\ve1\p\varif.obj
g:\ve1\p\varifct.obj
*ps.obj
*win_pr2.obj \
*g:\ve1\p\pr2.obj
*g:\ve1\p\prc.obj
*g:\ve1\p\prm.obj
*g:\ve1\p\pri.obj


  $(link) $(conlflags) -out:g:\e\u\ps\$*.exe $** $(conlibs) \
    user32.lib advapi32.lib winspool.lib g:\vsurvo\survo.lib
   
   
*/



#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define PI 3.14159265
#define NPAR 100
#define SCALESPACE 300
#define SHADEMAX 32
#define MAXTEXTS 100 // 32?
#define MAXSCALELIST 100
#define MAXLOOP 10
#define N_MESS 10
#define TAB '\t'
#define NVAR 100
#define NOBS 2000       // 19.10.2002
#define MAXDATA 20000
#define MAXPITUUS 100
#define MAXARG 10
#define MAXEARG 100
#define EARG '\376'   /* 20.6.92 */

extern char **spa,**spb,**spshad,**spb2;
extern int spn;
extern double *arvo;
extern char *spl;
extern char *spp;

extern int muste_gplot_init;
extern char muuttujanimi[];

static int earg_varattu=0;
static int n_earg=0;
static double *earg;


static char path[3];   /* G or PS or anything else (6.6.1992) */


static int shademax=9;
static int markermax=23;    // 21,22,23 for arrows
static int marker_color0=9999;

static FILE *kirjoitin;

static int x_pos,y_pos;
static int x_home, y_home; 		/* koko kuvan vasen alakulma */
static int x_size, y_size;		/* kuvan koko */
static int xx,yy; 				/* kuva-alueen vasen alakulma */
static double xdiv1,xdiv2,xdiv3;
static double ydiv1,ydiv2,ydiv3;
static int x_kuva, y_kuva;
static double kirjainlev, kirjainkork;
static  int tikki;				/* tick-viivan pituus (min. viiva tai raon koko) */
static double char_pitch, char_height, char_width;
static char *pen_code;
static char *line_code;        
        
static int scalespace=SCALESPACE;
static char xscales[SCALESPACE], *xscal[NPAR];
static double xscaleval[NPAR];
static int xscalen;            /* skaala-arvojen lkm */
static char yscales[SCALESPACE], *yscal[NPAR];
static double yscaleval[NPAR];
static int yscalen;            /* skaala-arvojen lkm */
static int frametype;          /* 0,1,2 */
static int shadeval[SHADEMAX], shadecolor[SHADEMAX];
static double shadepull[SHADEMAX];
static char code[512];
static char *pen_code;         /* PEN=pen_code */
static char *line_code;        /* LINETYPE=line_code */
static double minvalue;        /* MINVALUE=pienin piirrettÑvÑ pylvÑÑn korkeus */
static double xmin,xmax,xmumin,xmumax,ymin,ymax,ymumin,ymumax;

static int colors_2010;

static int marker_type, marker_size;

static int x_origin,y_origin;

static int line_color=0;
static int char_color;
static char p_outfile[1]; /* muita varten */
static double y_ratio=1.0;

static int ps_marker_type,ps_marker_size;

static double font_size;
static double width_coeff=0.65;
static double height_coeff=0.7;
static int npathstep=0;
static int pathind=0;
static int x_ps,y_ps;
static double x_psmove,y_psmove,psrotation;
static double autom_color=-1.0;
static double current_fill;
static char psnimi[LLENGTH];
static double ycharwidth;
static int fill_index=-1000;
static int color_fill=0;   /* 2.7.1992 */
static int n_mark=0;
static double ps_unit=0.1; /* 30.9.1996 */
static double ps_coeff=1.0;
static int alaviivat_pois=1;
static int line_width; // 8.9.2001
static int ps_printer_direct=0; // 23.8.2001

static char marker_rot_variable[16]; // 3.9.2010
static int marker_rot_var;
static double marker_rot_angle;
static int arrowlen;

static char *pr_osoitin;
static char *shadow[],*shadow2[];
static char framecode[];
static char *argv1;

static int capability[2];
    /*
      capability[0]   1=vÑlitulostukset sallittu 0=ei
      capability[1]   1=autom.fill               0=ei

    */

static int slow=0; // 6.4.2010

static char *ps_str[256];

static int cells_per_inch;
static int ps_negative;
static double raster_angle;

// static char muuttujanimi[LLENGTH]; // RS muutettu externiksi
static char xlauseke[LLENGTH], ylauseke[LLENGTH];
static int cfunktio; /* 1=C-kielinen 0=tulkattava */
static int integral_function;
static char xmuunnos[LLENGTH], ymuunnos[LLENGTH];

static int aika=0; /* psc2.c */

static char framecode[LLENGTH];

static double yscalepos1,yscalepos2;
static int scalemove_x,scalemove_y; /* 12.2.1993 */
static int tickturn;  /* 24.9.1993 */
static int pyramid=0; // 18.10.2005


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

static int out;
static char color_change[LLENGTH];
static int color_max;

static int lopetus=0;
static int kosketus=0;

static int n_mess=0;
static char c_message[N_MESS][16],c_text[N_MESS][32];
static int c_step[N_MESS],c_x[N_MESS],c_y[N_MESS],c_i[N_MESS];

static int l_virhe=0;

static char curve_attr[LNAME];
static char curve_fill_attr[LNAME]; // 20.5.2005

static char *shadow[256];    /* varjorivin merkkien koodisanaosoittimet */
static char *shadow2[256];   /* varjorivin merkkien jÑlkikoodisanaosoittimet */
static char pr_tila[6000];   /* koodijonot ja -sanat */
static char *pr_sana[300];   /* koodisanojen osoittimet */
static char *pr_koodi[300];  /* koodisanoja vastaavien koodijonojen osoittimet */
static int n_sana;           /* koodisanojen lukumÑÑrÑ */
static char *pr_osoitin;     /* ens. vapaan paikan osoitin pr_tilassa */
static int pr_type;          /* 1=PS, 0=muu */

static int color_2010;


/* Muuttujavektori x kÑyttÑjÑohjelman valittavissa */
static char *xname[NVAR], xnames[LLENGTH];
static char xtype[NVAR]; // 18.10.2005 TYPE=PYRAMID
static int   em,em2,en,l1,l2,edat;
static int   ev[NVAR];
static int namevar;

static SURVO_DATA dat;

static double xmat[2*NOBS]; // RS FIXME size?
static char xnimi[2*NOBS][100]; // RS FIXME size?
static int grouping_var;
static char gnimi[2*NOBS][100]; // RS FIXME size?

static int devvar1,devvar2;
static double devmat[2*NOBS];



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
static int p_text2(unsigned char *x,unsigned char *xs,int x1,int y1,int attr);
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
static int ps_cross(char *s,int i);
static int ps_plus(char *s,int size);
static int send_color();
static int p_special(char *s);
static void p_charsize();
static int p_textcontrol(char *s);
static int p_linecontrol(char *s);
static int p_origin(int x,int y);
static int tell_ps_unit();
static int send(char *s);
static int send2(char *x,char *xs);
static void ps_init();
static int ps_code(char *x,char **sana,int n,char *rivi);
static int ps_replace(unsigned char *x);
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
static int p_inquiry();
static void print_rivi(char *x,int j);
static void p_contour_init();
static void p_contour_plot(int ny,int iy,int nx,int *pxl_value);
static int win_tulostus();

static int control_code(char *x,char **pp,int laji); // PA
static int etsi_loppusulku(char *x,char **pp);
static void load_codes(char *codefile,unsigned char *code);
static int p_empty(char *s);
static int pilkku_muunto(char *s);
static int frame(int frtype); // PB
static int header(char *otsikko);
static int texts();
static int tekstirivit(char *tnimi,int nt,char *sana[]);
static int frames();
static int fills(); // PB2
static int polygons();
static void plot_xscale(); // P2
static void plot_yscale();
static int xdiv();
static int ydiv();
static int plot_box(int x1,int y1,int lev,int kork);
static int plot_halfbox(int x1,int y1,int lev,int kork);
static int xlabel(char *s);
static int ylabel(char *s);
static void sp_virhe(char *a,char *b);
static int find_tickturn();
static int shading(int n); // P3
static int c98_muunto(int *pk);
static int legend(int koko);
static int legend2(int koko,char *s1);
static int datain(); // DATA
static int dataopen(char data[]);
static int grid(char *suunta); // PGR
static int tick(char *suunta);
static int skaala_arvot(); // PSC
static void scale_err(char *s);
static int autom_scale(char *x,double min,double max,int npos);
static double paras_arvo(double x,double y);
static double xmu(double x); // PMU
static double ymu(double x);
static void alkukoodit(); // PR2
static int lue_koodit(char *x);
static int define(char *x,char **sana,int n,char *rivi);
static int shadows(char *x,char **sana,int n,char *rivi);
static int codes(char *x,char **sana,int n);
static int muunna(char *sana,char *muunnos);
static void koodivirhe(char *x);
static int space_split(char rivi[],char *sana[],int max); // PRC
static int makro(char *sana,char *muunnos); // PRM
static void korvaa(char *muunnos,char *s,char *t);
static int dos(char *x);
static int include(char *x,char **sana,int n); // PRI

static void muste_pcur(); // PCUR
static int tutki_yhtalo();
static void missing_char(char ch,int j);
static void incorrect_varname(int j);
static void error_line(int j);
static void plot_tscale();
static int curves(); // CUR1
static void tee_otsikko(char *ots);
static int xyscale(char *suunta);
static int xrajat();
static int yrajat();
static void rajavirhe(char c);
static int varnimet();
static void sp_listaus(char *s);
static int plot_curves(); // CUR2
static int coord(double t,int *px,int *py);
static void outline(double xs,double ys,double xu,double yu,int *px,int *py);
static void xy_arvot(double t,double *px,double *py);
static int plotting_range();
static int spfind2(char *s,int k);
static int read_loopar(int i);
static int find_datapar(int i);
static int read_datapar();
static void change_color();
static int fill(); // CURFI
static void fill_init();
static int x_coord(double x);
static int y_coord(double y);
static double integral();
static double arit_atof(char *lauseke); // CURARIT1
static int arit_atoi(char *lauseke);
static int laske(char *lauseke,double *y);
static double luku(char *sana,int len);
static double oper(double x1,double x2,char laji);
static void supista(int *t,double opnd[],char op[],int v[]);
static double funktio(char *s,double x);
static int f_edit(char *s,double *x,int n,double *py);
static void korvaa2(char *s,char *x,char *y);
static int varaa_earg();
static int aseta_earg(double luku,char *sana);
static void f_tuntematon(char *s);
static void arg_virhe(char *s);
static void syntax_error(char *s);
static int laske2(char *muuttuja,double *y);
static double lg_gamma(double x);
static double sur_gamma(double z);
static int varif(char *lauseke,double *y); // VARIF
static void if_syntax_error(char *x);


static void muste_pbar(); // PBAR
static int pen();
static int linetype();
// static double xmu(double x);
// static double ymu(double x);
static int read_loopar();
static int varnimet();
static void free_all();
static int lines(); // PBLIN



/* RS MISSING */
static int xgrid()	{ muste_fixme("\nFIXME: Function xgrid() missing in PLOT"); return(1); }
static int ygrid()	{ muste_fixme("\nFIXME: Function ygrid() missing in PLOT"); return(1); }
static int xtick()	{ muste_fixme("\nFIXME: Function xtick() missing in PLOT"); return(1); }
static int ytick()	{ muste_fixme("\nFIXME: Function ytick() missing in PLOT"); return(1); }
static int xyscale2()	{ muste_fixme("\nFIXME: Function xyscale2() missing in PLOT"); return(1); }

	



static void suorita(char *cmd,char *session)
	{
	sprintf(sbuf,"\nFIXME: %s not yet implemented!",cmd);
	muste_fixme(sbuf);
	sur_print(sbuf); WAIT;
	}

void muste_plot(int argc, char *argv[])
        {
        extern char *survo_id;
        int i;
        char *p;
        char x[LLENGTH],*osa[1];

// RS VARIABLE INIT
earg_varattu=0;
n_earg=0;
earg=NULL;
shademax=9;
markermax=23;
marker_color0=9999;
kirjoitin=NULL;
x_pos=y_pos=0;
x_home=y_home=0;
x_size=y_size=0;
xx=yy=0;
xdiv1=xdiv2=xdiv3=0;
ydiv1,ydiv2,ydiv3=0;
x_kuva=y_kuva=0;
kirjainlev=kirjainkork=0;
tikki=0;
char_pitch=char_height=char_width=0;
pen_code=NULL;
line_code=NULL;        
scalespace=SCALESPACE;
xscalen=0;
yscalen=0;
frametype=0;
pen_code=NULL;
line_code=NULL;
minvalue=0;
xmin=xmax=xmumin=xmumax=ymin=ymax=ymumin=ymumax=0;
colors_2010=0;
marker_type=marker_size=0;
x_origin=y_origin=0;
line_color=0;
char_color=0;
y_ratio=1.0;
ps_marker_type=ps_marker_size=0;
font_size=0;
width_coeff=0.65;
height_coeff=0.7;
npathstep=0;
pathind=0;
x_ps=y_ps=0;
x_psmove=y_psmove=psrotation=0;
autom_color=-1.0;
current_fill=0;
ycharwidth=0;
fill_index=-1000;
color_fill=0;
n_mark=0;
ps_unit=0.1;
ps_coeff=1.0;
alaviivat_pois=1;
line_width=0;
ps_printer_direct=0;
marker_rot_var=0;
marker_rot_angle=0;
arrowlen=0;
pr_osoitin=NULL;
argv1=NULL;
slow=0;
cells_per_inch=0;
ps_negative=0;
raster_angle=0;
cfunktio=0;
integral_function=0;
aika=0;
yscalepos1=yscalepos2=0;
scalemove_x=scalemove_y=0;
tickturn=0;
pyramid=0;
t_start=t_end=t_step=t=0;
filltype=0;
fill_step=0;
fill_start=fill_end=0;
x_fill=y_fill=0;
nloop=0;
data=0;
nvar=0;
obs=0;
integral_ind=0;
integral_const=integral_value=0;
out=0;
color_max=0;
lopetus=0;
kosketus=0;
n_mess=0;
l_virhe=0;
n_sana=0;
pr_osoitin=NULL;
pr_type=1;
color_2010=0;
em=em2=en=l1=l2=edat=0;
namevar=0;
grouping_var=0;
devvar1=devvar2=0;


        if (argc==1) return;
        s_init(argv[1]);

        if (g==1)
            {
            sur_print("\nIncomplete PLOT operation!");
            WAIT; return;
            }

     	muste_gplot_init=1;
     	i=sp_init(r1+r-1);
     	muste_gplot_init=0;        
     	if (i<0) { sur_print("\nNot enough space for specifications!"); WAIT; return; }
        i=hae_apu("plot_mode",x);   /* 29.8.1991 */
        if (i>0)
            {
            if (strcmp(x,"PS")==0 || muste_strcmpi(x,"PostScript")==0) strcpy(path,"PS");
            else if (muste_strcmpi(x,"CRT")==0 || muste_strcmpi(x,"G")==0) strcpy(path,"G");
/*
            else if (*x=='C' || *x=='c') strcpy(path,"P");
            else if (*x=='H' || *x=='h') strcpy(path,"H");
*/
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

        i=spfind("TYPE");
        if (i>=0)
            {
            strcpy(x,spb[i]); split(x,osa,1);
            if (strcmp(osa[0],"CONTOUR")==0 || strcmp(osa[0],"MATRIX")==0)
                   {
                   strcpy(info,osa[0]);
                   suorita("CONTOUR.EXE",argv[1]);
                   return;
                   }
            if (strcmp(osa[0],"FACES")==0 || strcmp(osa[0],"ANDREWS")==0
                || strcmp(osa[0],"DRAFTS")==0 || strcmp(osa[0],"STARS")==0
                || strcmp(osa[0],"PROFILES")==0)
                   {
                   strcpy(info,osa[0]);
                   suorita("FACES.EXE",argv[1]);
                   return;
                   }
            }
        if (muste_strcmpi(word[0],"HISTO")==0) { suorita("PHIS.EXE",argv[1]); return; }
        if (strchr(word[1],'=')!=NULL || muste_strcmpi(word[1],"INTEGRAL")==0)
            { 
            // RS CHA suorita("PCUR.EXE",argv[1]); 
            muste_pcur(2,argv);
            return;
            }
        if (muste_strcmpi(word[1],"FUNCTION")==0) { suorita("PFUNC.EXE",argv[1]); return; }
        if (muste_strcmpi(word[1],"FILE")==0) { suorita("FILE.EXE",argv[1]); return; }

        if (g<3) { suorita("PBAR.EXE",argv[1]); return; }
        suorita("PDIA.EXE",argv[1]);
        }

static int p_init(char *laite)     /* for PS printers */
        {
        int i,j;
        char x[LLENGTH], *sana[3];
        char y1[LLENGTH], y2[LLENGTH];
        char *p;
        char nimi[LLENGTH];
        int nn;

        capability[0]=1;
        capability[1]=0;

        alkukoodit();

        ps_init();

        strcpy(nimi,"MUSTE_PR.PS"); // RS CHA LPT1 -> MUSTE_PR.PS
        strcpy(psnimi,nimi); // RS CHA *psnimi=EOS; 
        i=hae_apu("printer",x); { if (i) strcpy(nimi,x); }  /* 12.7.1992 */
        i=spfind("DEVICE");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            i=split(x,sana,2);
            if (i>1)
                {
                strcpy(nimi,sana[1]);
                if (strchr(nimi,':')==NULL && nimi[0]!='/' && nimi[0]!='.' && nimi[0]!='\\') // RS unix path FIXME
                { strcpy(nimi,edisk); strcat(nimi,sana[1]); }
                strcpy(psnimi,nimi);
                }
            else
                {
//                strcpy(nimi,etmpd); strcat(nimi,"SURVO_PR.PS");
                strcpy(nimi,edisk); strcat(nimi,"MUSTE_PR.PS"); // RS CHA
                strcpy(psnimi,nimi);
                ps_printer_direct=0; // RS CHA 1 no direct print
                }
            }

        kirjoitin=muste_fopen(nimi,"wt");
        if (kirjoitin==NULL)
            {
            sprintf(sbuf,"\nDEVICE=%s not available!",nimi);
            sur_print(sbuf); WAIT; return(-1);
            }

        i=spfind("INCLUDE");
        if (i>=0) sana[1]=spb[i];
        else
            {
            i=hae_apu("ps_dev",x); if (i==0) strcpy(x,"PS.DEV");
                     /* - 10.7.92 plot2_dev */
            sana[1]=x;
            }

        i=include(x,sana,2); if (i<0) return(-1);

        ps_coeff=1.0;
        i=spfind("PS_UNIT");
        if (i>=0)
            {
            ps_unit=arit_atof(spb[i]);
            if (ps_unit>0.0) ps_coeff=0.1/ps_unit;
            }

        strcpy(x,"[INIT]");
        i=muunna(x,y1); if (i<0) return(-1);
        send(y1);

        tikki=10*ps_coeff; /* tick-viivan pituus (min. viivan tai raon pituus) */
        marker_type=3; marker_size=5*ps_coeff;
        x_pos=0; y_pos=0; x_ps=y_ps=0;
        x_home=250*ps_coeff; y_home=100*ps_coeff;
        x_size=1500*ps_coeff; y_size=1500*ps_coeff;
        xdiv1=300.0; xdiv2=1000.0; xdiv3=200.0;
        ydiv1=300.0; ydiv2=1000.0; ydiv3=200.0;
        x_origin=y_origin=0;
        npathstep=0;
        x_psmove=y_psmove=psrotation=0.0;

        i=spfind("CHAR_COEFF");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,sana,2);
            if (i>0) width_coeff=atof(sana[0]);
            if (i>1) height_coeff=atof(sana[1]);
            }
        font_size=12.0; char_height=255.0/72.0*font_size*height_coeff*ps_coeff;
                        char_width=255.0/72.0*font_size*width_coeff*ps_coeff;
        p_charsize();

        i=spfind("SHADEMAX");
        if (i>=0) { shademax=atoi(spb[i]); if (shademax<=1) shademax=9; }

        i=spfind("TICKLENGTH");
        if (i>=0) tikki=atoi(spb[i]);

        i=spfind("SLOW");
        if (i>=0) slow=atoi(spb[i]);

        i=spnfind("COLOR("); // 16.9.2010
        if (i>=0) { colors_2010=1; }

        return(1);
        }

static int p_error(char *s)
        {
        char x[LLENGTH];

        sprintf(x,"\nPLOT error: %s",s);
        sur_print(x); WAIT;
        return(-1); // RS CHA exit(0);
        }

static int p_error2(char *s)  { p_error(s); return(1); }

static void p_end()
        {
        char x[LLENGTH],y1[LLENGTH];

        if (npathstep) send("stroke\n");
        if (n_mark) send(" grestore "); /* 27.1.1994 */
        sprintf(x,"\n%%SURVO 84C Graphics END\nshowpage\n");
        send(x);
        strcpy(x,"[END]");
        muunna(x,y1);
        send(y1);
        fclose(kirjoitin);
        p_eps();
        if (ps_printer_direct) win_tulostus(); // 23.9.2001
        }

static int p_wait()
        {
        return(1);
        }

static void p_clear()
        {
        send("copypage erasepage\n");
        }

static void p_newpage()
        {
        send("gsave showpage grestore\n");
        }

static int p_line(int x2,int y2,int i)     /* line from (x_pos,y_pos) to (x2,y2)  */
/* int i;    attribute index */
        {
        char s[LLENGTH];
        static int n=0;
        char v;

        if (slow) // 7.4.2010
            {
            p_line2(x_pos,y_pos,x2,y2,1);
            x_pos=x_ps=x2; y_pos=y_ps=y2;
            return(1);
            }

/*      sprintf(s,"%d %d %d %d line\n",x_pos,y_pos,x2,y2);
        send(s);
*/

        if (x_ps!=x_pos || y_ps!=y_pos)
            {
            sprintf(s,"%d m %d m moveto\n",x_pos,y_pos);
            send(s);
            }
        if (npathstep==0)
            {
            sprintf(s,"\nnewpath %d m %d m moveto\n",x_pos,y_pos);
            send(s);
            }

/*      sprintf(s,"%d m %d m rlineto\n",x2-x_pos,y2-y_pos);   */
        ++n; if (n<10) v=' '; else { v='\n'; n=0; }
        sprintf(s,"%d %d rl%c",x2-x_pos,y2-y_pos,v);
        ++npathstep; pathind=1;
        send(s);

        if (npathstep>1400)
            {
            sprintf(s,"stroke newpath %d m %d m moveto\n",x2,y2);
            npathstep=0; send(s);
            }

        x_pos=x_ps=x2; y_pos=y_ps=y2;

        return(1);
        }

static int p_line2(int x1,int y1,int x2,int y2,int i)  /* line from (x1,y1) to (x2,y2) */
/* int i;    attribute index */
        {
        char s[LLENGTH];

        send("newpath\n");
        pathind=1;
if (slow==0)
  {
        sprintf(s,"%d m %d m moveto %d m %d m rlineto\n",
                   x1,y1,x2-x1,y2-y1);
        npathstep=1;
        send(s);
  }
else
for (i=0; i<slow; ++i)
  {
        sprintf(s,"%d m %d m moveto %d m %d m rlineto\n",
                   x1,y1,x2-x1,y2-y1);
        npathstep=1;
        send(s);
  }
        x_pos=x_ps=x2; y_pos=y_ps=y2;

        return(1);
        }


static int p_line3(int x1,int y1,int x2,int y2,int i)  // 16.10.2009
/* int i;    attribute index */
        {
        char s[LLENGTH];
        double w;

        send("f_cyan f_mage f_yell f_black setcmykcolor\n");
        w=line_width*0.24;
        sprintf(s,"%g setlinewidth ",w);
        send(s);
        send("newpath\n");
        pathind=1;
if (slow==0)
  {
        sprintf(s,"%d m %d m moveto %d m %d m rlineto stroke\n",
                   x1,y1,x2-x1,y2-y1);
        send(s);
  }
else
for (i=0; i<slow; ++i)
  {
        sprintf(s,"%d m %d m moveto %d m %d m rlineto stroke\n",
                   x1,y1,x2-x1,y2-y1);
        send(s);
  }
        x_pos=x_ps=x2; y_pos=y_ps=y2;

        return(1);
        }

static int p_text(char *text,int x1,int y1,int i)
/* int x1,y1;   coordinates of start */
/* int i;       attribute index */
        {
        int k;
        char s[LLENGTH],y[LLENGTH];
        unsigned char *p;
        extern int marker_color0;

        if (*text==EOS) return(1);

        if (autom_color>=0.0)
            {
            send("currentgray ");
            if (current_fill<autom_color) send("1 setgray\n");
            else send("0 setgray\n");
            }
        if (marker_color0!=9999) // 18.5.2006
            {
            p_fillattr(marker_color0);
            send("f_cyan f_mage f_yell f_black setcmykcolor\n");
            }

        strcpy(y,text);
        pilkku_muunto(y); /* puolipiste pilkuksi muutettu */
//      p=y; while ((p=strchr(p,'_'))!=NULL)  *p=' ';   13.10.2002
        p=y; while (*p) { *p=code[(int)(*p)]; ++p; }
        ps_replace(y);
        sprintf(s,"%d m %d m moveto ",x1,y1,y); send(s);
        text_move_rot(1);

        if (ycharwidth>0.0)
            {
            for (k=0; k<strlen(y); ++k)
                {
                sprintf(s,"/a { %g (%c) stringwidth pop sub 2 div } def\n",
                               ycharwidth,y[k]); send(s);
                sprintf(s,"a 0 rmoveto (%c) show a 0 rmoveto\n",y[k]);
                send(s);
                }
            }
        else
            { sprintf(s,"(%s) show\n",y); send(s); }


        text_move_rot(2);

        if (autom_color>=0.0)
            send("setgray\n");

        return(1);
        }

static void text_move_rot(int k)
        {
        int move,rot;
        char s[LLENGTH];

        move=(x_psmove!=0.0 || y_psmove!=0.0);
        rot=(psrotation!=0.0);
        if (!move && !rot) return;
        if (k==1)
            {
            send("currentpoint gsave\n");
            if (y_psmove!=0.0)
                {
                sprintf(s,"%g m add ",y_psmove); send(s);
                }
            if (x_psmove!=0.0)
                {
                sprintf(s,"exch %g m add exch ",x_psmove); send(s);
                }
            send("moveto currentpoint translate\n");
            if (rot)
                {
                sprintf(s,"%g rotate\n",psrotation); send(s);
                }
            }
        else send("grestore\n");
        }

static int p_text2(unsigned char *x,unsigned char *xs,int x1,int y1,int attr)
/* int x1,y1;   coordinates of start */
/* int attr;    attribute index */

        {
        int i,k,len,slen,h,j;
        unsigned char varjo;
        char *p;
        char y[LLENGTH], yy[LLENGTH];

        if (xs==NULL) { i=p_text(x,x1,y1,attr); return(i); }
        pilkku_muunto(x); /* puolipiste pilkuksi muutettu */
//      p=x; while ((p=strchr(p,'_'))!=NULL)  *p=' ';  13.10.2002

        sprintf(y,"%d m %d m moveto ",x1,y1); send(y);
        text_move_rot(1);

        len=strlen(x);
        x[len]=EOS; xs[len]=EOS;
        i=0;
        while (xs[i])
            {
            varjo=xs[i];
       /*   if (varjo=='R')
                {
                varjo=x[i];
                *y=varjo; *(y+1)=EOS;
                if (raster[varjo]!=NULL) strcpy(y,raster[varjo]);
                k=fontprint(y); if (k<0) return(-1);
                ++i; continue;
                }
        */
            p=shadow[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            k=0;
            while (xs[i]==varjo) { y[k]=code[x[i]]; ++k; ++i; }
            y[k]=EOS;
            send("(");
            h=ps_replace(y);
            for (j=0; j<h; ++j) putc((int)y[j],kirjoitin);
            send(") prnshow\n");

            p=shadow2[varjo];
            if (p!=NULL)
                {
                strcpy(y,shadow2[varjo]);
                muunna(y,yy);
                p=yy;
                while (*p) { putc((int)(*p),kirjoitin); ++p; }
                }
            }
        text_move_rot(2);    /* 17.11.89  oli kierroksen sisÑllÑ! */

        return(1);
        }

static int p_pen()
        {
        int i;
        char x[LLENGTH], y[3*LLENGTH];

        if (pen_code==NULL) strcpy(x,"[PEN]");
        else                strcpy(x,pen_code);
        i=muunna(x,y);
        if (i<0) return(-1);
        send(y);

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
        send(y);

        return(1);
        }

static int p_fill(int x1,int y1,int fill)
        {
        return(1);
        }

static int p_fill_bar(int x1,int y1,int x2,int y2,int fill)
        {
        int i,pen;
        char s[LLENGTH];
        char y[LLENGTH];

/*      if (fill==0) return(1);    */
        sprintf(s,"gsave newpath %d m %d m moveto %d m %d m rlineto\n",
                                 x1,y1,x2-x1,0); send(s);
        sprintf(s,"%d m %d m rlineto %d m %d m rlineto\n",
                   0,y2-y1,x1-x2,0);  send(s);

        if (fill>=0 && !colors_2010)
            {
            current_fill=1.0-(double)fill/shademax;
            sprintf(s,"closepath gsave %g setgray fill grestore stroke grestore\n",
                         current_fill);
            }
        else if (fill>-1000)
            {
            p_fillattr(fill);
sprintf(s,"closepath gsave f_cyan f_mage f_yell f_black setcmykcolor fill grestore stroke grestore\n");
            }
        else
            {
            current_fill=1.0;
            sprintf(s,"closepath stroke grestore\n");
            }
        send(s);
        return(1);
        }

static int p_halfframe(int x1,int y1,int x2,int y2)
        {
        int i,pen;
        char s[LLENGTH];
        char y[LLENGTH];

        sprintf(s,"gsave newpath %d m %d m moveto %d m %d m rlineto\n",
                                 x1,y1,x2-x1,0); send(s);
        sprintf(s,"%d m %d m moveto %d m %d m rlineto\n",
                   x1,y1,0,y2-y1);  send(s);
        current_fill=1.0;
            sprintf(s,"closepath stroke grestore\n");

        send(s);
        return(1);
        }

static int p_fill_polygon(int kerroin,char *s)
    {
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
    return(1);
    }

static int p_polygon_line(int n_poly,int fill)
    {
    int k,i;
    int h[2];
    double pointx,pointy;
    double kerroin;
    static FILE *poly_tmp;

    kerroin=1.0;
    sprintf(sbuf,"%sPOLYGON.TMP",etmpd);
    poly_tmp=muste_fopen(sbuf,"rb");

    i=0;
    for (k=0; k<n_poly; ++k)
        {
        fread(h,sizeof(int),2,poly_tmp);

        pointx=kerroin*h[0];
        pointy=kerroin*h[1];

        if (i==0)
            {
            sprintf(sbuf,"gsave newpath %g m %g m moveto\n",
                                     pointx,pointy); send(sbuf);
            }
        if (h[1]<1000000)
            {
            sprintf(sbuf,"%g m %g m lineto\n",
                                     pointx,pointy); send(sbuf);
            ++i;
            }
        else
            {
            p_polygon_line2(fill,i);
            fill=h[0];
            i=0;
            }
        }

    fclose(poly_tmp);

    p_polygon_line2(fill,i);
    return(1);
    }

static int p_polygon_line2(int fill,int i)
    {
    char stroke[16];

    if (line_width) strcpy(stroke,"stroke");
    else *stroke=EOS;

    if (fill==0 || i<3)
        {
        sprintf(sbuf,"closepath stroke grestore\n"); send(sbuf);
        return(1);
        }
    else
        {
        if (fill>=0 && !colors_2010)
            {
            current_fill=1.0-(double)fill/shademax;
            sprintf(sbuf,"closepath gsave %g setgray fill grestore %s grestore\n",
                         current_fill,stroke);
            }
        else if (fill>-1000)
            {
            p_fillattr(fill);
sprintf(sbuf,"closepath gsave f_cyan f_mage f_yell f_black setcmykcolor fill grestore %s grestore\n",
                  stroke);
            }
        else
            {
            current_fill=1.0;
            sprintf(sbuf,"closepath stroke grestore\n");
            }
        send(sbuf);
        }

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

        if (frtype>=2 && frtype<6) p_fill_bar(x_home,y_home,x_home+x_size,y_home+y_size,-1000);
        if (frtype==5 || frtype==6) p_halfframe(xx,yy,xx+x_kuva,yy+y_kuva);
        if (frtype>0 && frtype<3) p_fill_bar(xx,yy,xx+x_kuva,yy+y_kuva,-1000);

/****************************** GPLOT: P1.EDT  frame()
if (frtype>=2 && frtype<6) plot_box(x_home,y_home,x_size,y_size);
if (frtype==5 || frtype==6) plot_halfbox(xx,yy,x_kuva,y_kuva); // 27.11.02
if (frtype>0 && frtype<3) plot_box(xx,yy,x_kuva,y_kuva);
*******************************/

        }

static int p_fill_sector(int x0,int y0,double rx,double ry,double a1,double a2,int fill)
        {
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
        return(1);
        }

static int p_fillattr(int fill)
        {
        int i;
        char fword[LLENGTH];
        char *sana[4];
        char y[LLENGTH];
        char nolla[2];

        color_fill=1;
        sprintf(fword,"COLOR(%d)",fill); // 27.8.2010
        i=spfind(fword);
        if (i>=0)
            {
            strcpy(nolla,"0"); sana[0]=nolla; sana[1]=nolla; sana[2]=nolla; sana[3]=nolla;
            strcpy(y,spb[i]);
            i=split(y,sana,4);
            sprintf(sbuf,"/f_cyan %s def /f_mage %s def /f_yell %s def /f_black %s def\n",
                          sana[0],sana[1],sana[2],sana[3]);
            send(sbuf);
            return(1);
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
            strcpy(y,spb[i]);

            if (muste_strnicmp(y,"FILL(",5)==0) // 23.11.2007 FILL(i)=FILL(j)
                {
                i=spfind(y);
                if (i>=0) strcpy(y,spb[i]);
                else p_error(y);
                }

            strcpy(nolla,"0"); sana[0]=nolla; sana[1]=nolla; sana[2]=nolla; sana[3]=nolla;
            i=split(y,sana,4);
            sprintf(sbuf,"/f_cyan %s def /f_mage %s def /f_yell %s def /f_black %s def\n",
                          sana[0],sana[1],sana[2],sana[3]);
            send(sbuf);
            }
        else
            {
            color_fill=0;
            send("/fcyan 0 def /f_mage 0 def /f_yell 0 def /f_black 1 def\n");
//          send("/fcyan 0.5 def /f_mage 0.5 def /f_yell 0.5 def /f_black 0 def\n");
            }
        return(1);
        }

static int p_textcolors(int fill) // Needed in GPLOT  16.9.2010
    { return(p_marker_color(fill)); }

static int p_marker_color(int i)
    {
    marker_color0=i;
    return(1);
    }

static int p_marker_select(int i,int size)
        {
        if (i>markermax-1) i=0; ps_marker_type=i;
        ps_marker_size=size;
        return(1);
        }

static void p_marker_type_select(int i) // 28.5.2005
    {
    if (i>markermax-1) i=0; ps_marker_type=i;
    }

static int p_marker(int x2,int y2)
        {
        char s[LLENGTH];
        int i,size;
        int ia,ib;

        size=ps_marker_size;
        if (size<1)
            {
            if (n_mark==0) send(" gsave ");
            sprintf(s,"\n%d %d dot ",x2,y2); send(s);
            ++n_mark;
            if (n_mark>=400) { send(" grestore "); n_mark=0; }
            x_pos=x2; y_pos=y2;
            return(1);
            }
/*      if (size<=1) size=1;     */
/*
printf("\ntype=%d size=%d",ps_marker_type,ps_marker_size); getch();
*/
        sprintf(s,"\nnewpath %d m %d m moveto ",x2,y2); send(s);

        if (marker_color0!=9999)
            {
            p_fillattr(marker_color0);
            }


        if (*marker_rot_variable!=EOS) // 3.9.2010
            {
            sprintf(s,"gsave %d %d translate %g rotate\n",
                          x_pos,y_pos,    marker_rot_angle);
            send(s);
            }


        switch (ps_marker_type)
            {
          case 0:  /* filled circle */
            i=size; if (i<1) i=1; /* if (i>60) i=60; poistettu 28.3.1995 */
          if (marker_color0==9999)
            sprintf(s,"%d m %d m %d m 0 360 arc closepath gsave fill grestore stroke\n",
                        x2,y2,i);
          else
sprintf(s,"%d m %d m %d m 0 360 arc closepath f_cyan f_mage f_yell f_black setcmykcolor fill\n",
                        x2,y2,i);

/*          sprintf(s,"WG%d,0,360,%d;",size,i);         */
            break;
          case 2:  /* asterisk */
            i=0.7*size;
            ps_cross(s,i);
            send(s);
            sprintf(s,"\nnewpath %d m %d m moveto ",x2,y2); send(s);
            /* jatkuu plussalla */
          case 1:  /* plus */
            ps_plus(s,size);
            break;
          case 3:  /* circle */
            i=size; if (i<1) i=1; /* if (i>60) i=60;  */
            if (marker_color0!=9999) send_color();
            sprintf(s,"newpath %d m %d m %d m 0 360 arc closepath stroke\n",
                        x2,y2,i);
            break;
          case 4:  /* cross */
            ps_cross(s,size);
            break;
          case 5: /* square */
            i=size;
            if (marker_color0!=9999) send_color();
            sprintf(s,"%d m %d m rmoveto %d m 0 rlineto 0 %d m rlineto %d m 0\n",
                         -i,-i,            2*i,             2*i,        -2*i);
            send(s);
            sprintf(s,"rlineto 0 %d m rlineto %d m %d m rmoveto stroke\n",
                                -2*i,         i,i);
            break;
          case 6: /* filled square */
            i=size;
            sprintf(s,"%d m %d m rmoveto %d m 0 rlineto 0 %d m rlineto %d m 0\n",
                         -i,-i,            2*i,             2*i,        -2*i);
            send(s);
            sprintf(s,"rlineto 0 %d m rlineto %d m %d m rmoveto\n",
                                -2*i,         i,i);
            send(s);
            p_set_marker_color(s);
/**********************************
          if (marker_color0==9999)
            strcpy(s,"gsave fill grestore stroke\n");
          else
            strcpy(s,"f_cyan f_mage f_yell f_black setcmykcolor fill\n");
*******************************/
            break;
          case 7:  /* triangle */
            i=size; ia=2.0/sqrt(3.0)*i; ib=sqrt(3.0)*i;
            if (marker_color0!=9999) send_color();
            sprintf(s,"0 %d m rmoveto %d m %d m rlineto %d m 0 rlineto\n",
                       ia,           -i,-ib,         2*i);
            send(s);
            sprintf(s,"%d m %d m rlineto 0 %d m rmoveto stroke\n",
                      -i,ib,             -ia);
            break;
          case 8:  /* filled triangle */
            i=size; ia=2.0/sqrt(3.0)*i; ib=sqrt(3.0)*i;
            sprintf(s,"0 %d m rmoveto %d m %d m rlineto %d m 0 rlineto\n",
                       ia,           -i,-ib,         2*i);
            send(s);
          if (marker_color0==9999)
            sprintf(s,"%d m %d m rlineto 0 %d m rmoveto gsave fill grestore stroke\n",
                      -i,ib,             -ia);
          else
sprintf(s,"%d m %d m rlineto 0 %d m rmoveto f_cyan f_mage f_yell f_black setcmykcolor fill\n",
                      -i,ib,             -ia);
            break;
          case 9: /* diamond */
            i=size; ia=i*sqrt(2.0);
            if (marker_color0!=9999) send_color();
            sprintf(s,"0 %d m rmoveto %d m %d m rlineto %d m %d m rlineto\n",
                        ia,           -ia,-ia,            ia,-ia);
            send(s);
            sprintf(s,"%d m %d m rlineto %d m %d m rlineto 0 %d rmoveto stroke\n",
                       ia,ia,             -ia,ia,          -ia);
            break;
          case 10: // filled diamond
            i=size; ia=i*sqrt(2.0);
            sprintf(s,"0 %d m rmoveto %d m %d m rlineto %d m %d m rlineto\n",
                        ia,           -ia,-ia,            ia,-ia);
            send(s);
            sprintf(s,"%d m %d m rlineto %d m %d m rlineto 0 %d rmoveto\n",
                       ia,ia,             -ia,ia,          -ia);
            send(s);
            p_set_marker_color(s);
/*****************************
            if (marker_color0==9999)
              strcpy(s,"gsave fill grestore stroke\n");
            else
              strcpy(s,"f_cyan f_mage f_yell f_black setcmykcolor fill\n");
***************************/
            break;

          case 21: // arrow >
            i=arrowlen*size;
            sprintf(s,"%d m %d m rmoveto %d m %d m rlineto %d m %d m rlineto %d m %d m rmoveto stroke\n",
                       -i,  size,         i,  -size,       -i,  -size,       i,   size);
            break;

          case 22: // arrow > (filled)
            i=arrowlen*size; ia=2*size;
            sprintf(s,"%d m %d m rlineto %d m %d m rlineto %d m %d m rlineto\n",
                       -i,  size,         0,  -ia,          i,   size);
            send(s);
            p_set_marker_color(s);
            break;


          default:
            strcpy(s,"-5 -5 rmoveto 5 0 rlineto 0 5 rlineto -5 0 rlineto 0 -5 rlineto 5 5  rmoveto stroke\n");
     /*     strcpy(s,"PR-5,-5;PD;PR5,0,0,5,-5,0,0,-5;PU;PR5,5;");       */
            }
        send(s);

        if (*marker_rot_variable!=EOS) send("grestore\n"); // 3.9.2010

        x_pos=x2; y_pos=y2;
        return(1);
        }


static int p_set_marker_color(char *s)
    {
    if (marker_color0==9999)
      strcpy(s,"gsave fill grestore stroke\n");
    else
      strcpy(s,"f_cyan f_mage f_yell f_black setcmykcolor fill\n");
    return(1);
    }



static int ps_cross(char *s,int i)
        {
        if (marker_color0!=9999) send_color();
sprintf(s,"%d m %d m rmoveto %d m %d m rlineto %d m 0 rmoveto %d m %d m rlineto %d m %d m rmoveto stroke\n",
                        -i,-i,    2*i,2*i,   -2*i,     2*i,-2*i,  -i,i);
        return(1);
        }

static int ps_plus(char *s,int size)
        {
        if (marker_color0!=9999) send_color();
        sprintf(s,"%d m 0 rmoveto %d m 0 rlineto %d m %d m rmoveto 0 %d m rlineto 0 %d m rmoveto stroke\n",
                      -size,   2*size,  -size,-size,   2*size, -size);
        return(1);
        }

static int send_color()
    {
    send("f_cyan f_mage f_yell f_black setcmykcolor\n");
    return(1);
    }

static int p_special(char *s) /* tulkkaa laitetiedoston %-sanat */
        {
        char *p;
        char x[LLENGTH];
        int pen;

        strcpy(x,s);
        p=strchr(x,'=');
        if (p==NULL)
            {
            printf("\nError in %% code %s",x);
            WAIT; return(-1);
            }
        *p=EOS;
        ++p;

/*      if (strcmp(x,"char_width")==0)
            { char_width=atof(p); p_charsize(); return(1); }
        if (strcmp(x,"char_height")==0)
            { char_height=atof(p); p_charsize(); return(1); }
- 27.4.1992 */

        if (strcmp(x,"char_width")==0)
            { ycharwidth=atof(p)*ps_coeff; return(1); }
/*      if (strcmp(x,"pen")==0)
            { pen=atoi(p); pen_change(pen); return(1); }
*/
        if (strcmp(x,"font_size")==0)
            {
            font_size=atof(p);
            char_height=255.0/72.0*font_size*height_coeff*ps_coeff;
            char_width=255.0/72.0*font_size*width_coeff*ps_coeff;
            p_charsize();
            return(1);
            }
        if (strcmp(x,"x_move")==0)
            { x_psmove=atof(p); return(1); }
        if (strcmp(x,"y_move")==0)
            { y_psmove=atof(p); return(1); }
        if (strcmp(x,"rotation")==0)
            { psrotation=atof(p); return(1); }
        if (strcmp(x,"autom_color")==0)
            { autom_color=atof(p); return(1); }
        if (strcmp(x,"line_color")==0)
            { line_color=atoi(p); return(1); }
        if (strcmp(x,"fill_index")==0)          /* 24.6.1992 */
            { fill_index=atoi(p); return(1); }
        if (strcmp(x,"line_width")==0)          // 8.9.2001
            { line_width=atoi(p); return(1); }

        if (strcmp(x,"rotate")==0)
            {
            strcpy(marker_rot_variable,p); // 3.9.2010
            return(1);
            }


/*      printf("\nUnknown %% code %s",s);       */

        return(1);
        }

static void p_charsize()
        {
        double a;


        kirjainlev=char_width;
        kirjainkork=char_height;

        }

static int p_textcontrol(char *s) /* tÑsmennysten alussa suluissa olevat ohjauskoodit */
        {
        int i;
        char y[3*LLENGTH];

        i=muunna(s,y); if (i<0) return(-1);
        send(y);
        return(1);
        }

static int p_linecontrol(char *s) /* tÑsmennysten alussa suluissa olevat ohjauskoodit */
        {
        int i;
        char y[3*LLENGTH];

        i=muunna(s,y); if (i<0) return(-1);
        send(y);

        return(1);
        }

static int p_origin(int x,int y)
        {
        int i;
        char s[LLENGTH], *osa[2];
        double sx,sy,angle;
        double a;

        send("\n");
        if (ps_unit!=0.1) tell_ps_unit();
        sprintf(s,"%d m %d m translate\n",x_origin+x,y_origin+y); send(s);
        sprintf(s,"%%SURVO 84C graphics\n"); send(s);
        a=0.2834646*ps_unit/0.1;  /* dmm -> Point */
        sprintf(s,"%%%%BoundingBox: %d %d %d %d\n",
               (int)(a*x_home),(int)(a*y_home),
               (int)(a*(x_home+x_size))+1,(int)(a*(y_home+y_size))+1);
        send(s);

        tell_ps_unit();

        x_home=y_home=0;

        i=spfind("SCALING");
        if (i>=0)
            {
            sx=sy=1.0;
            strcpy(s,spb[i]); i=split(s,osa,2);
            if (i>0) { sx=atof(osa[0]); if (sx==0.0) sx=1.0; }
            if (i>1) { sy=atof(osa[1]); if (sy==0.0) sy=1.0; }
            sprintf(s,"%g %g scale\n",sx,sy);  send(s);
            }
        i=spfind("ROTATION");
        if (i>=0)
            {
            angle=atof(spb[i]);
            sprintf(s,"%g rotate\n",angle);  send(s);
            }

        return(1);
        }

static int tell_ps_unit()
        {
        sprintf(sbuf,"/m { 0.2834646 %g mul mul } def",ps_unit/0.1);
        send(sbuf);
        sprintf(sbuf," %% Plotting unit = %g mm\n",ps_unit);
        send(sbuf);
        return(1);
        }

static int send(char *s)
        {
        int i;

        if (!pathind && npathstep)
            {
            fprintf(kirjoitin,"stroke \n");
            npathstep=0;
            }
  /*    fprintf(kirjoitin,s);    */
    for (i=0; i<strlen(s); ++i) putc((int)s[i],kirjoitin);

        if (ferror(kirjoitin))   /* 4.3.1995 */
            {
            sur_print("\nCannot write a PS file (Printer not ready or disk full/protected!");
            WAIT; return(-1); // RS CHA exit(1);
            }

        pathind=0;
/*  fflush(kirjoitin); printf("\n%s",s); getch();  */
		return(1);
        }

static int send2(char *x,char *xs)
        {
        send(x);
        return(1);
        }

static void ps_init()
        {
        int i;

        for (i=0; i<256; ++i) ps_str[i]=NULL;
        ps_str[(unsigned int)'\\']="\\\\";
        ps_str[(unsigned int)'(']="\\(";
        ps_str[(unsigned int)')']="\\)";
        }

static int ps_code(char *x,char **sana,int n,char *rivi)   /* ps_code <koodi> <8#xxx> <ps-nimi> */
        {
        int i,k;
        unsigned char merkki;
        char y[LLENGTH];

        if (n<4) { koodivirhe(rivi); return(-1); }
        sprintf(y,"%s %s\n",sana[2],sana[3]); send(y);

        i=strlen(sana[1]);
        if (i==1) merkki=sana[1][0];
        else { i=muunna(sana[1],y); if (i<0) return(-1); merkki=*y; }

        *y='\\'; y[1]=EOS; strcat(y,sana[2]+2);
        ps_str[merkki]=pr_osoitin; strcpy(pr_osoitin,y);
        pr_osoitin+=strlen(y)+1;
        return(1);
        }

static int ps_replace(unsigned char *x)
        {
        unsigned char y[2*LLENGTH];
        unsigned char *p,*q;
        int len;
        char koodi[8];

        p=x; *y=EOS; len=0;
        while (*p)
            {
            q=ps_str[(unsigned int)*p];
            if (q==NULL)
                {
                if (*p<128)
                    { y[len++]=*p; y[len]=EOS; }
                else
                    {
                    koodi[0]='\\';
                    muste_itoa((int)*p,koodi+1,8);
                    strcat(y,koodi); len+=strlen(koodi);
                    }
                }
            else
                { strcat(y,q); len+=strlen(q); }
            ++p;
            }
        strcpy(x,y);
        return(len);
        }

static int p_eps()
        {
        int i;
        char x[LNAME];

        i=spfind("EPSFILE"); if (i<0) return(-1);
        if (*psnimi==EOS) return(-1);

        sur_print("\nEPSFILE option is not available in SURVO MM.");
        sur_print("\nUse EPSFILE <file.PS>,<file.EPS> command after PLOT!");
        WAIT;

/*************************
        sprintf(info,"EPS %s %s",psnimi,spb[i]);
        strcpy(x,survo_path); strcat(x,"_EPS.EXE");
        s_spawn(x,argv1);
**************************/
/*      execl(x,x,argv1,NULL);  */
        return(1);
        }

static int p_lineattr()     /* color_change */
        {
        char s[LLENGTH];

        if (line_color==0) return(1);

//      p_fillattr(-line_color);
        p_fillattr(line_color); // 16.9.2010
        if (color_fill)   /* p_fillattr() valitsee */
            send("f_cyan f_mage f_yell f_black setcmykcolor\n");
        else
            {
            current_fill=1.0-(double)line_color/shademax;
            sprintf(s,"%g setgray\n",
                         current_fill); send(s);
            return(1);
            }

        return(1);
        }

static int p_path(int nt,char **sana)
        {
        int i,xk,yk;
        char s[LLENGTH];
        extern double arit_atof();

        xk=arit_atoi(sana[0])+x_home; yk=arit_atoi(sana[1])+y_home;
        sprintf(s,"newpath %d m %d m moveto \n",xk,yk); send(s);

        for (i=2; i<nt-1; i+=2)
            {
            xk=arit_atoi(sana[i]); yk=arit_atoi(sana[i+1]);
            sprintf(s,"%d %d rl\n",xk,yk); send(s);    /* m pois 22.11.92 */
            }

        if (fill_index==-1000)
            {
            send("stroke\n");
            return(1);
            }
        send("closepath gsave ");
        ps_fill(fill_index);
        send("fill grestore stroke\n");
        return(1);
        }

static int ps_fill(int fill)
        {
        char s[LLENGTH];

        if (fill>=0)
            {
            current_fill=1.0-(double)fill/shademax;
            sprintf(s,"%g setgray\n",
                          current_fill);
            }
        else
            {
            p_fillattr(fill);
            sprintf(s,"f_cyan f_mage f_yell f_black setcmykcolor\n");
            }
        send(s);
        return(1);
        }

static void p_open() {}
static void p_close() {}
static void p_save() {}
static void p_load() {}
static void vdc() {}
static void p_floodfill() {}
static void p_charcolor() {}

static int p_inquiry()
        {
        int i,j;
        char x[LLENGTH];
        char y[LLENGTH];

        j=r1+r;
        sprintf(x,"SIZE=%d,%d HOME=%d,%d",x_size,y_size,x_home,y_home);
        print_rivi(x,j++);
        sprintf(x,"XDIV=%d,%d,%d YDIV=%d,%d,%d",(int)xdiv1,(int)xdiv2,(int)xdiv3,
                                (int)ydiv1,(int)ydiv2,(int)ydiv3);
        print_rivi(x,j++);
        sprintf(x,"x_pixel/y_pixel=%g",y_ratio);
        print_rivi(x,j++);

        sur_print("\nPress any key!");
        return(1);
        }

static void print_rivi(char *x,int j)
        {
        if (j>ed2) return;
        sur_print("\n%s",x);
        edwrite(space,j,1);
        edwrite(x,j,1);
        }

static void p_contour_init()
        {
        int i;
        char s[LLENGTH], *osa[3];
        char x[LLENGTH];

        send("/setF { currentscreen 4 -2 roll pop 3 1 roll setscreen } def\n");
        send("/setA { /a exch def currentscreen 3 1 roll pop a 3 -1 roll setscreen } def\n");

        send("0 setA\n");   /* raster angle */
        ps_negative=0;
        i=spfind("SCREEN");  /* SCREEN=<POS|NEG>,<cells_per_inch>,<raster_angle> */
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
        }

static void p_contour_plot(int ny,int iy,int nx,int *pxl_value)
        {
        char x[LLENGTH];
        double x_taso, y_taso;
        double x_koko, y_koko;
        int i,k;
        int taso;

        x_koko=xdiv2*x_size; y_koko=ydiv2*y_size;
        x_taso=xx;
        y_taso=yy+y_koko*(ny-nx)/ny-y_koko/ny*(double)iy;
        sprintf(x,"save %g m %g m translate\n",x_taso,y_taso); send(x);

/*      sprintf(x,"%g m %g m scale\n",x_koko,y_koko); send(x);    */
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
        }

static int win_tulostus()
    {
    char rivi[LLENGTH];
    char laite[LNAME];

    strcpy(laite,etmpd); strcat(laite,"SURVO_PR.PS");

    kirjoitin=muste_fopen(laite,"rt");
    while(!feof(kirjoitin))
        {
        fgets(rivi,LLENGTH-1,kirjoitin);
//        WritePrinter(hPrinter,rivi,strlen(rivi),&k);
		sur_print("\n"); sur_print(rivi); // RS ADD
        }

    fclose(kirjoitin);

    return(1);
    }


static int control_code(char *x,char **pp,int laji)
/* int laji; 0=text_mode 1=vector_mode 2=no immediate output*/
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

static void load_codes(char *codefile,unsigned char *code)
        {
        int i;
        char x[LLENGTH];
        
        static FILE *codes;

        strcpy(x,codefile);
        if (strchr(x,':')==NULL && *x!='.')
            { strcpy(x,survo_path); strcat(x,"SYS/"); strcat(x,codefile); }
        codes=muste_fopen(x,"rb");
        if (codes==NULL)
            {
            for (i=0; i<256; ++i) code[i]=(unsigned char)i;
            for (i=0; i<256; ++i) code[i+256]=(unsigned char)i;
            return;   /* GPLOT-fontit */
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        for (i=0; i<256; ++i)
            {
            if (feof(codes)) break;
            code[i+256]=(unsigned char)getc(codes);
            }
        fclose(codes);
        }

static int p_empty(char *s)
        {
        while (*s) { if (*s!=' ') return(0); ++s; }
        return(1);
        }

// Muutettu 13.10.2002 \; -> ;  ja \_ -> _
static int pilkku_muunto(char *s) /* puolipiste pilkuksi */
        {
        char t[LLENGTH];
        char *p,*q;
        extern int alaviivat_pois;

        if (!alaviivat_pois) return(1);
// printf("\ns=%s|",s);
        p=s; q=t;
        while (*p)
            {
            if (*p==';') *q=',';
            else if (*p=='_') *q=' ';
            else if (*p=='\\')
                {
                if (*(p+1) == ';' || *(p+1) == '_')
                    { ++p; *q=*p; }
                else *q=*p;
                }
            else *q=*p;
            ++p; ++q;
            }
        *q=EOS; strcpy(s,t);
// printf("\nt=%s|",s); getch();
        return(1);

//      while ((p=strchr(s,';'))!=NULL) *p=',';  - 13.10.2002
        }

static int frame(int frtype) /* FRAME: 0=ei 1=vain sisÑ 2=myîs ulko 3=vain ulko */
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

static int header(char *otsikko)
        {
        int i,k;
        char x[LLENGTH];
        char *p, *ps;
        double a;

        i=p_pen(); if (i<0) return(-1);
        i=spfind("HEADER");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=control_code(x,&p,0);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            if (spshad[i]==NULL) ps=NULL;
            else { k=p-x; ps=spshad[i]+k; }
            }
        else { strcpy(x,otsikko); p=x; ps=NULL; }
        a=1.5; if (pr_type==2) a=1;
     p_text2(p,ps,x_home+(int)kirjainlev,y_home+y_size-(int)(a*kirjainkork),1);
        return(1);
        }

static int texts()
        {
        int h,i,k;
        char x[LLENGTH];
        char *p, *ps;
        int ntext;
        char *tnimi[MAXTEXTS];
        char y[LLENGTH];
        int nt;
        char *sana[5];
        char pkoodi[LLENGTH], kopio[LLENGTH];

        i=p_pen(); if (i<0) return(-1);
        i=spfind("TEXTS");
        if (i<0) { i=spfind("TEXT"); if (i<0) return(1); }
        strcpy(x,spb[i]);
        k=etsi_loppusulku(x,&p); if (k<0) return(-1);
        *pkoodi=EOS;
        if (p!=x)
            {
            if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
            *(p-1)=EOS; strcpy(pkoodi,x+1); ++p;
            strcpy(kopio,pkoodi);
            k=p_textcontrol(kopio); if (k<0) return(-1);
            }
        ntext=split(p,tnimi,MAXTEXTS);
        for (h=0; h<ntext; ++h)
            {
            i=spfind(tnimi[h]);
            if (i<0) return(-1);
            if (*pkoodi==EOS) p_pen();
            else      { strcpy(kopio,pkoodi); p_textcontrol(kopio); }
            strcpy(y,spb[i]);
            k=control_code(y,&p,0);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            if (spshad[i]==NULL) ps=NULL;
            else { k=p-y; ps=spshad[i]+k; }
            nt=split(p,sana,5);
            if (muste_strnicmp(sana[0],"#LINES:",7)==0)
                {
                i=tekstirivit(tnimi[h],nt,sana);
                if (i<0) return(-1);
                continue;
                }

//          if (nt<3) { printf("\nCoordinates missing after text %s\n",spb[i]);
//                      WAIT; return(-1);
//                    }

            if (nt<3)
                {

      sprintf(sbuf,"Coordinates missing after text %s",spb[i]);
                p_error(sbuf);
                WAIT; return(-1);

                }



            p_text2(p,ps,x_home+arit_atoi(sana[1]),y_home+arit_atoi(sana[2]),1);
            }
        return(1);
        }

static int tekstirivit(char *tnimi,int nt,char *sana[])
        {
        int i,j,j1,j2;
        int reuna,taso,vali;
        char *p;
        char x[LLENGTH];
        char xs[LLENGTH];

// for (i=0; i<nt; ++i) printf("\n%s ",sana[i]); getch();

        if (nt<5)
            {
            sprintf(sbuf,"Too few parameters in #LINES text of specification %s\n",
                                                              tnimi);
            p_error(sbuf);
            WAIT; return(-1);
            }
        p=sana[0]+7;
        j1=edline2(p,1,1);
        if (j1==0) return(-1);
        j2=edline2(sana[1],j1,1);
        if (j2==0) return(-1);
        reuna=x_home+arit_atoi(sana[2]);
        taso=y_home+arit_atoi(sana[3]);
        vali=arit_atoi(sana[4]);
        alaviivat_pois=0;
        for (j=j1; j<=j2; ++j)
            {
            edread(x,j); p=NULL;
            if (zs[j]!=0) { edread(xs,zs[j]); p=xs+1; }
            i=strlen(x); while (x[i-1]==' ') x[--i]=EOS;
            p_text2(x+1,p,reuna,taso,1);
            taso-=vali;
            }
        alaviivat_pois=1;
        return(1);
        }

static int frames()
        {
        int h,i,k;
        char x[LLENGTH];
        char *p, *ps;
        int ntext;
        char *tnimi[MAXTEXTS];
        char y[LLENGTH];
        int nt;
        char *sana[5];
        char pkoodi[LLENGTH], kopio[LLENGTH];
        int fill;

        i=p_linetype(); if (i<0) return(-1);
        i=spfind("FRAMES");
        if (i<0) return(1);
        strcpy(x,spb[i]);
        k=etsi_loppusulku(x,&p); if (k<0) return(-1);
        *pkoodi=EOS;
        if (p!=x)
            {
            if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
            *(p-1)=EOS; strcpy(pkoodi,x+1); ++p;
            strcpy(kopio,pkoodi);
            k=p_textcontrol(kopio); if (k<0) return(-1);
            }
        ntext=split(p,tnimi,MAXTEXTS);
        for (h=0; h<ntext; ++h)
            {
            int xk,yk;

            i=spfind(tnimi[h]);
            if (i<0) return(-1);
            if (*pkoodi==EOS) p_linetype();
            else      { strcpy(kopio,pkoodi); p_linecontrol(kopio); }
            strcpy(y,spb[i]);
            k=control_code(y,&p,1);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            if (spshad[i]==NULL) ps=NULL;
            else { k=p-y; ps=spshad[i]+k; }
            nt=split(p,sana,5);
//          if (nt<4) { printf("\nCoordinates missing in frame %s",spb[i]);
//                      printf("\nSyntax: name=x,y,width,height[,shading]\n");
//                      WAIT; return(-1);
//                    }
            if (nt<4)
                {
sprintf(sbuf,"Coords missing in frame %s | Syntax: name=x,y,width,height[,shading]"
                   ,spb[i]);
                p_error(sbuf);
                }
            xk=arit_atoi(sana[0])+x_home; yk=arit_atoi(sana[1])+y_home;

            if (pr_type==1 || (pr_type==2 && nt==5) )
                                          /* 14.2.94 */
                {
                fill=-9999; if (nt==5) fill=atoi(sana[4]);
                p_fill_bar(xk,yk,arit_atoi(sana[2])+xk,
                                 arit_atoi(sana[3])+yk,fill);
                }
            else
                {
                plot_box(xk,yk,arit_atoi(sana[2]),arit_atoi(sana[3]));
                if (nt==5) p_fill(xk+tikki/2,yk+tikki/2,atoi(sana[4]));
                }
            }
        return(1);
        }

static int fills()
        {
        int h,i,k;
        char x[LLENGTH];
        int ntext;
        char *tnimi[MAXTEXTS];
        char y[LLENGTH];
        int nt;
        char *sana[5];
        char pkoodi[LLENGTH], kopio[LLENGTH];

        i=p_linetype(); if (i<0) return(-1);
        i=spfind("FILLS");
        if (i<0) return(1);
        strcpy(x,spb[i]);
        ntext=split(x,tnimi,MAXTEXTS);
        for (h=0; h<ntext; ++h)
            {
            int xk,yk;

            i=spfind(tnimi[h]);
            if (i<0) return(-1);
            strcpy(y,spb[i]);
            nt=split(y,sana,3);
//          if (nt<3) { printf("\nIncomplete fill point %s",spb[i]);
//                      printf("\nSyntax: name=x,y,shading\n");
//                      WAIT; return(-1);
//                    }
            if (nt<3)
                {
 sprintf(sbuf,"Incomplete fill point %s | Syntax: name=x,y,shading"
                ,spb[i]);
                p_error2(sbuf);
                return(-1); // RS ADD
                }
            xk=arit_atoi(sana[0]); yk=arit_atoi(sana[1]);
            p_floodfill(xk,yk,arit_atoi(sana[2]));  // 27.6.2001
            }
        return(1);
        }

static int polygons()
        {
        int h,i,k;
        char x[LLENGTH];
        int ntext;
        char *tnimi[MAXTEXTS];
        char y[LLENGTH];
        int nt;
        char *sana[5];
        char pkoodi[LLENGTH], kopio[LLENGTH];
        int kerroin;

        i=p_linetype(); if (i<0) return(-1);

        kerroin=1; i=spfind("POLYCOEFF");
        if (i>=0) kerroin=arit_atoi(spb[i]);

        i=spfind("POLYGONS");
        if (i<0) return(1);

        strcpy(x,spb[i]);
        ntext=split(x,tnimi,MAXTEXTS);
        for (h=0; h<ntext; ++h)
            {
            i=spfind(tnimi[h]);
            if (i<0) return(-1);
            strcpy(y,spb[i]);
            p_fill_polygon(kerroin,y);
            }
        return(1);
        }


static void plot_xscale(
int n,              /* arvojen lkm */
double value[],     /* skaala-arvot */
char *label[],      /* skaalanimet */
int x0,
int y0,          /* alkupiste */
int pituus         /* asteikon pituus */
)
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
            p_text(q,x1,y0-2*tikki-(int)(1.2*kirjainkork),1);
            }
        }

static void plot_yscale(
int n,              /* arvojen lkm */
double value[],     /* skaala-arvot */
char *label[],      /* skaalanimet */
int x0,
int y0,          /* alkupiste */
int pituus         /* asteikon pituus */
)
        {
        int i;
        double min,max;
        int y1;
        int dmax;
        char x[LLENGTH],*osa[2];
        double klev;
        extern double ycharwidth;

/*  printf("\nyscale: n=%d\n",n);
    for (i=0; i<n; ++i) printf("%g ",value[i]); getch();
*/
        if ((frametype==0 || frametype==3) && !scalemove_x) return;
        find_tickturn();
        dmax=0; /* max desimaaleja + 1 */
        for (i=0; i<n; ++i)
            {
            int k;
            char *p;

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
            int x1,k;
            char *p;
            int len=strlen(label[i]);
            char *q;

            y1=y0+(int)((ymu(value[i])-ymumin)/(ymumax-ymumin)*pituus);
            q=label[i];
            if (*q=='?') { ++q; --len; }
            else if (tikki) p_line2(x0,y1,x0-2*tickturn*tikki,y1,1);
            // 25.4.2003
            p=strchr(q,'.');
            if (p!=NULL) k=len-(p-q); else k=0;
                x1=x0-(int)(klev*(1+len+dmax-k))-tikki;
            if (yscalepos1!=0) x1=x0+(int)yscalepos1;
            p_text(q,x1,y1-(int)(kirjainkork/2.0),1);
            }

        }

static int xdiv()
        {
        extern double arit_atof();
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
        extern double arit_atof();
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

static int xlabel(char *s)  /* default label */
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

static void sp_virhe(char *a,char *b)
        {
//      printf("\nError in specification\n   %s=%s",a,b);
        sprintf(sbuf,"Error in specification %s=%s",a,b);
        p_error2(sbuf);
        WAIT;
        }

static int find_tickturn()    /* 24.9.1993 */
        {
        int i;

        tickturn=1;
        i=spfind("TICKTURN"); if (i>=0) tickturn=atoi(spb[i]);
        return(1);
        }


static int shading(int n) /* varjostusten tarvittava lkm */
        {
        int i,k;
        double dk;
        char x[LLENGTH], *osa[32];
        char *p; char y[LLENGTH];
        int c98;

        c98=0; // 3.3.2001
        if (!capability[1])
            {
            i=hae_apu("color98",x);
            if (i) c98=atoi(x);

            i=spfind("C98");
            if (i>=0) c98=atoi(spb[i]);
            }

        i=spnfind("COLOR("); // 16.9.2010
        if (i>=0) { colors_2010=1; c98=0; }

        i=spfind("SHADING");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=control_code(x,&p,0);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            k=split(p,osa,32);
            if (k!=n) { sp_virhe(spa[i],spb[i]); return(-1); }
            for (i=0; i<n; ++i) /* shadeval[i]=atoi(osa[i]); */
                {
                strcpy(y,osa[i]);
                k=atoi(y);
                if (c98) c98_muunto(&k);
                shadeval[i]=k;

                if ((p=strchr(y,'P'))!=NULL)
                    {
                    dk=atof(p+1);
                    if (dk==0.0) dk=2.0;  /* oletussiirto 2/10*r */
                    shadepull[i]=dk;
                    }
                else shadepull[i]=0.0;
                if ((p=strchr(y,'/'))!=NULL) shadecolor[i]=atoi(p+1);
                else shadecolor[i]=0;
                }
            return(1);
            }
        if (n==1) { k=0; if (c98) c98_muunto(&k);  shadeval[0]=k; return(1); }
        for (i=0; i<n; ++i)
            {
            k=shademax*i/(n-1);
            if (c98) c98_muunto(&k);
            shadeval[i]=k;
            }
        return(1);
        }

static int c98_muunto(int *pk)
    {
    int i;
    i=*pk;
    if (i<0) return(1);
    if (i>7) return(1);
    if (i==7) { *pk=0; return(1); }
    *pk=i+1; return(1);
    }


static int legend(int koko)
        {
        int i, ytaso, xs, k;
        char s[LLENGTH],*osa[2];
        char *p;
        int ykoko;
        char s2[LLENGTH];

        i=p_pen(); if (i<0) return(-1);
        *s=EOS;
        p=s; /* 11.2.1992 */
        i=spfind("LEGEND");
        if (i>=0)
            {
            strcpy(s,spb[i]);
            k=control_code(s,&p,0);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }

            strcpy(s2,p);
            k=split(s2,osa,2);
            if (k>1) return(legend2(koko,p));   /* spb[i] -22.1.92 */

            if (strlen(p)==1 && *p=='-') return(1);
            }
        if (koko<5*tikki) koko=5*tikki;
        if (koko>8*tikki) koko=8*tikki;
        ykoko=y_ratio*koko;

        ytaso=y_home+(ykoko+kirjainkork)/2;
        p_text(p,(int)(x_home+kirjainlev),ytaso,1);      /* s -22.1.92 */
        xs=x_home+kirjainlev*(strlen(p)+2);              /* s */
        for (i=0; i<em2-1; ++i)
            {
            int y_apu;

            p_text(xname[ev[i+1]],xs,ytaso,1);
            xs+=kirjainlev*(strlen(xname[ev[i+1]])+0.5);
            y_apu=(int)(y_home+kirjainkork);
            plot_box(xs,y_apu,koko,ykoko);
            if (capability[1])
                p_fill((int)(xs+koko/2),y_home+ykoko,shadeval[i]);
            else
                {
                p_fill_bar(xs,y_apu,xs+koko,y_apu+ykoko,shadeval[i]);
                plot_box(xs,y_apu,koko,ykoko);
                }
            xs+=koko+kirjainlev;
            }

        return(1);
        }

static int legend2(int koko,char *s1)  /* LEGEND=x_leg,y_leg,n_col */
        {
        int i,k,row,col;
        char *p;
        char s[LLENGTH], *osa[4];
        int x_leg,y_leg,n_col;
        char text[LLENGTH];
        int ykoko;
        int x_box,y_box;
        int x_text,y_text;
        int ix,iy;

        n_col=1;
        strcpy(s,s1);
        i=split(s,osa,3);
        if (i<2)
            {
            sp_virhe("LEGEND",s1); return(-1);
            }
        x_leg=x_home+arit_atoi(osa[0]); y_leg=y_home+arit_atoi(osa[1]);
        if (i>2) { n_col=arit_atoi(osa[2]); if (n_col<1) n_col=1; }

        if (koko<5*tikki) koko=5*tikki;
        if (koko>8*tikki) koko=8*tikki;
        ykoko=y_ratio*koko;
        x_box=1.5*koko+8*kirjainlev; y_box=-1.5*koko;
        i=spfind("LEGEND_BOX");  /* =x_box,y_box,koko,ykoko */
        if (i>=0)
            {
            strcpy(s,spb[i]);
            i=split(s,osa,4);
            if (i<2) { sp_virhe(spa[i],spb[i]); return(-1); }
            x_box=arit_atoi(osa[0]); y_box=arit_atoi(osa[1]);
            if (i>2) { koko=arit_atoi(osa[2]); ykoko=y_ratio*koko; }
            if (i>3) ykoko=arit_atoi(osa[3]);
            }

        x_text=x_box+1.5*koko; y_text=y_box;
        i=spfind("LEGEND_TEXT");   /* =x_text,y_text */
        if (i>=0)
            {
            strcpy(s,spb[i]);
            k=control_code(s,&p,0);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            i=split(p,osa,2);
            x_text=arit_atoi(osa[0]);
            if (i>1) y_text=arit_atoi(osa[1]);
            }

        row=0; col=0;
        for (i=0; i<em2-1; ++i)
            {
            ix=x_leg+col*x_box; iy=y_leg+row*y_box;
            plot_box(ix,iy,koko,ykoko);
            if (capability[1])
                p_fill((int)(ix+koko/2),(int)(iy+ykoko/2),shadeval[i]);
            else
                {
                p_fill_bar(ix,iy,ix+koko,iy+ykoko,shadeval[i]);
                plot_box(ix,iy,koko,ykoko);
                }
            p_text(xname[ev[i+1]],ix+x_text,iy+y_text,1);
            ++col;
            if (col>=n_col) { ++row; col=0; }
            }

        return(1);
        }




static int datain()
        {
        char x[LLENGTH], *sana[2];
//      char xx[LNAME];
        char prev_group[100];
        int i,j,k;
        long l;
        double y;

        edread(x,r1+r-1);
        i=split(x+1,sana,2);
        i=dataopen(sana[1]); if (i==0) return(-1);
        i=conditions(&dat); if (i<0) return(-1);
/********************************
        grouping_var=-1;
        i=spfind("GROUPING"); // 1.11.2002
        if (i>=0)
            {
            strcpy(xx,spb[i]);
            grouping_var=varfind(&dat,xx);
            if (grouping_var<0)
                {
                sprintf(sbuf,"GROUPING variable %s not found!",xx);
                p_error(sbuf);
                WAIT; return(-1); // onko tarpeen?
                }

            j=0; k=0; // Poista GROUPING-muuttuja aktiivisista!
            for (i=0; i<dat.m_act; ++i)
                {
                if (dat.v[i]==grouping_var) { k=1; continue; }
                dat.v[j]=dat.v[i]; ++j;
                }
            em-=k; em2=em;
            }
********************************************************************/

         i=dev_spec(sbuf); if (i<0) return(-1); // 30.4.2004
         strcpy(x,sbuf);
         i=split(sbuf,sana,2);
         if (i>0 && *sbuf!='-')
             {
             devvar1=varfind2(&dat,sana[0],0);
             if (devvar1<0) { sp_virhe("DEV",x); return(-1); }
             devvar2=devvar1;
             if (i>1)
                 {
                 devvar2=varfind2(&dat,sana[1],0);
                 if (devvar2<0) { sp_virhe("DEV",x); return(-1); }
                 }
             }
// printf("\nem=%d|",em); getch();
        j=0;
        for (l=dat.l1; l<=dat.l2; ++l)
            {
            if (unsuitable(&dat,l)) continue;
            if (grouping_var>=0)
                {
                data_alpha_load(&dat,l,grouping_var,x);
// -9.11.2002   x[31]=EOS; i=strlen(x);
                x[99]=EOS; i=strlen(x);
                while (x[i-1]==' ' && i>1) x[--i]=EOS;

                if (j==0) strcpy(prev_group,x);
                if (   j==0 ||
                       strcmp(x,prev_group)==0 || *x==EOS ||
                       strcmp(x,"-")==0
                   )
                    strcpy(gnimi[j],prev_group);
                else
                    {
                    strcpy(gnimi[j],"#GAP#");
                    xnimi[j][0]=EOS;
                    for (i=0; i<em-1; ++i) xmat[j*em+i]=0.0;
                    ++j;
                    strcpy(gnimi[j],x); strcpy(prev_group,x);
                    }
                }

            if (namevar<0) xnimi[j][0]=EOS;
            else
                {
                data_alpha_load(&dat,l,namevar,x);
// -9.11.2002   x[31]=EOS; i=strlen(x);
                x[99]=EOS; i=strlen(x);
                while (x[i-1]==' ' && i>1) x[--i]=EOS;
                strcpy(xnimi[j],x);
                }

            for (i=0; i<em-1; ++i)
                {
                k=data_load(&dat,l,dat.v[i],&y);
                if (k<0)
                   {
                   sprintf(sbuf,"Error in observation %ld!",l);
                   p_error(sbuf);
                   }
                if (y==MISSING8)
                    {
//                  printf("\nValue of %.8s missing in obs.#%ld!\n",
//                              dat.varname[dat.v[i]],l);
                    sprintf(sbuf,"Value of %.8s missing in obs.#%ld!",
                                dat.varname[dat.v[i]],l);
                    p_error(sbuf);

                    WAIT; return(-1);
                    }
                xmat[j*em+i]=y;
                if (pyramid && xtype[i]=='A') xmat[j*em+i]=-y; // 18.10.2005
                if (devvar1>=0)  // 1.5.2004
                    {
                    k=data_load(&dat,l,devvar1,&y);
                    devmat[2*j]=y;
                    k=data_load(&dat,l,devvar2,&y);
                    devmat[2*j+1]=y;
                    }
                }
            ++j;
            if ((j+1)*em>MAXDATA)
                {
//              printf("\nToo many data values (>%d)\n",MAXDATA);
                sprintf(sbuf,"Too many data values (>%d)",MAXDATA);
                p_error(sbuf);

                WAIT; return(-1);
                }
            if (j+1>NOBS)
                {
//              printf("\nToo many observations (>%d)\n",NOBS);
                sprintf(sbuf,"Too many observations (>%d)",NOBS);
                p_error(sbuf);

                WAIT; return(-1);
                }
            }
        l1=1; l2=en=j;

        if (pyramid) // reverse order
            {
            int k;

            k=en-1;
            for (j=0; j<=(en-1)/2; ++j)
                {
                strcpy(sbuf,xnimi[j]);
                strcpy(xnimi[j],xnimi[k]); strcpy(xnimi[k],sbuf);
                for (i=0; i<em-1; ++i)
                    {
                    y=xmat[j*em+i]; xmat[j*em+i]=xmat[k*em+i];
                    xmat[k*em+i]=y;
                    }
                --k;
                }
            }
        data_close(&dat);
        return(1);
        }


static int dataopen(char data[])
        {
        int   i,j,k;
        char xx[LNAME];

//      i=data_read_open(data,&dat); if (i<0) return(0);
        i=data_read_open(data,&dat); if (i<0) p_error(sbuf);

//      i=mask(&dat); if (i<0) return(0);
        i=mask(&dat); if (i<0) p_error(sbuf);

        grouping_var=-1;
        i=spfind("GROUPING"); // 1.11.2002
        if (i>=0)
            {
            strcpy(xx,spb[i]);
            grouping_var=varfind(&dat,xx);
            if (grouping_var<0)
                {
                sprintf(sbuf,"GROUPING variable %s not found!",xx);
                p_error(sbuf);
                WAIT; return(-1); // onko tarpeen?
                }

            j=0; k=0; // Poista GROUPING-muuttuja aktiivisista!
            for (i=0; i<dat.m_act; ++i)
                {
                if (dat.v[i]==grouping_var) { k=1; continue; }
                dat.v[j]=dat.v[i]; ++j;
                }
            dat.m_act-=k;
//          em-=k; em2=em;
            }

        namevar=activated(&dat,'L');
//      i=0; if (grouping_var==0) i=1; // 2.11.2002
 i=0;
        if (namevar<0) namevar=dat.v[i]; // oli 0

        if (dat.vartype[namevar][0]!='S') namevar=-1;

        j=0;
        for (i=0; i<dat.m_act; ++i)
            {
            if (dat.v[i]==namevar) continue;
            dat.v[j]=dat.v[i]; ++j;
            }

        em=dat.m_act; if (namevar<0) ++em;
   /* piirr.muutujien lkm vain em-1 */
        if (em>NVAR)
            {
//          printf("\nToo many active variables (>%d)!\n",NVAR);
            sprintf(sbuf,"Too many active variables (>%d)!",NVAR);
            p_error(sbuf);

            WAIT; return(0);
            }
    /* 1.nimipaikka tyhjÑ */
        for (i=1; i<em; ++i)
            {
            int len;
            xname[i]=dat.varname[dat.v[i-1]];
// printf("\ntype=%s|",dat.vartype[dat.v[i-1]]); getch();
            xtype[i]=dat.vartype[dat.v[i-1]][1];
            len=strlen(xname[i]);
            while (xname[i][len-1]==' ') xname[i][--len]=EOS;
            }

        em2=em; for (i=0; i<em; ++i) ev[i]=i;
        en=dat.n;
        return(em);
        }


static int grid(char *suunta)
        {
        extern double arit_atof();
        int i,k;
        char x[LLENGTH], *osa[2];
        char *p;
        double a, step;
        double min,max;

        i=spfind("GRID"); if (i<0) return(1);
        k=p_linetype(); if (k<0) return(-1);
        strcpy(x,spb[i]);
        k=control_code(x,&p,1);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        k=split(p,osa,2);
        min=xscaleval[0];
        max=xscaleval[xscalen-1];
        if (osa[0][0]==*suunta)
            {
            for (i=1; i<xscalen-1; ++i)
                {
                if (*suunta=='X')
                    {
                    int ix=xx+(xscaleval[i]-min)/(max-min)*x_kuva;
                    p_line2(ix,yy,ix,yy+y_kuva,1);
                    }
                else
                    {
                    int iy=yy+(xscaleval[i]-min)/(max-min)*y_kuva;
                    p_line2(xx,iy,xx+x_kuva,iy,1);
                    }
                }
            }
        else
            {
            step=arit_atof(osa[0]);
            if (step<=0.0)
                {
//              printf("\nIncorrect GRID specification!\n");
                p_error("Incorrect GRID specification!");
                WAIT; return(-1);
                }
            a=min+step;
            while (a<max-step/2)
                {
                if (*suunta=='X')
                    {
                    int ix=xx+(a-min)/(max-min)*x_kuva;
                    p_line2(ix,yy,ix,yy+y_kuva,1);
                    }
                else
                    {
                    int iy=yy+(a-min)/(max-min)*y_kuva;
                    p_line2(xx,iy,xx+x_kuva,iy,1);
                    }
                a+=step;
                }
            }

        return(1);
        }

static int tick(char *suunta)
        {
        extern double arit_atof();
        int i,k;
        char x[LLENGTH], *osa[2];
        char *p;
        double a, step;
        double min,max;

        i=spfind("TICK"); if (i<0) return(1);
        k=p_linetype(); if (k<0) return(-1);
        strcpy(x,spb[i]);
        k=control_code(x,&p,1);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        k=split(p,osa,2);
        min=xscaleval[0];
        max=xscaleval[xscalen-1];
        step=arit_atof(osa[0]);
        if (step<=0.0)
            {
//          printf("\nIncorrect TICK specification!\n");
            p_error("Incorrect TICK specification!");
            WAIT; return(-1);
            }
        a=min+step;
        while (a<max-step/10.0)
            {
            if (*suunta=='X')
                {
                int ix=xx+(a-min)/(max-min)*x_kuva;
                p_line2(ix,yy,ix,yy+tikki,1);
                }
            else
                {
                int iy=yy+(a-min)/(max-min)*y_kuva;
                p_line2(xx,iy,xx+tikki,iy,1);
                }
            a+=step;
            }

        return(1);
        }



/* skaala_arvot(s,list,osa,n,scalespace) muodostaa asteikkolistauksen
   s=input-merkkijono tyyppiÑ 0(1)5,6(2)10,20,50
   list=output tyyppiÑ 0 1 2 3 4 5 6 8 10 20 50
   osa[]=osoittimet listaan
   *n=lukumÑÑrÑ (11)
   scalespace=SCALESPACE
*/
static int skaala_arvot(
char *s,
char *list,
char *osa[],
int *n,
int scalespace
)
        {
        extern double arit_atof();
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
            while (dp<=dr+fabs(dstep)*1e-7)   /* fabs(dr) -> fabs(dstep) 15.5.93 */
                {
                if (fabs(dp)<1e-10) dp=0.0;
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

static void scale_err(char *s)
        {
//      printf("\nError in scale notation %s: More than %d values",
//                                         s,MAXSCALELIST);

        sprintf(sbuf,"Error in scale notation %s: More than %d values",
                                           s,MAXSCALELIST);
        p_error(sbuf);
//      printf("\n or text exceeding space available!\n");
        WAIT;
        }

static int autom_scale(char *x,double min,double max,int npos)
        {
        extern double paras_arvo();
        int i;
        double paras, askel, a;
        int n;
        char sana[32];

        if (min<-1e20 || max>1e20)
            {
//          printf("\nToo large values (>1e20) for automatic scaling!\n");
            p_error("Too large values (>1e20) for automatic scaling!");
            WAIT; return(-1);
            }
        if (min>=max) { min=min-1; max=min+2; }   /* 9.2.1989 */
        paras=paras_arvo(min,max);
        n=npos/8;
        askel=paras_arvo((max-min)/n,2*(max-min)/n);
        a=paras;
        while (a>min*(1.0+1e-7)) a-=askel; fconv(a,"",sana);
        strcpy(x,sana); strcat(x,"("); fconv(askel,"",sana);
        strcat(x,sana); strcat(x,")");
        a=paras;
        while (a<max*(1.0-1e-7)) a+=askel; fconv(a,"",sana);
        strcat(x,sana);
        return(1);
        }

static double paras_arvo(double x,double y)
        {
        double z;
        int merkki=1;
        char a[22],b[22];
        int i,j,k,h;
        char u,v;

        if (x>y) { z=x; x=y; y=z; }
        if (x<=0.0 && y>=0.0) return(0.0);
        if (x==y) return(x);
        if (x<0) { merkki=-1; z=x; x=-y; y=-z; }

        sprintf(a,"%21.10f",x); a[21]='\0';
        sprintf(b,"%21.10f",y); b[21]='\0';
        i=0; while (a[i]==' ') a[i++]='0';
        i=0; while (b[i]==' ') b[i++]='0';

        i=0; k=0; while (a[i]==b[i] && i<22) { ++i; if (a[i]!='0') ++k; }
        h=0;
        for (j=i+1; j<21; ++j) { if (b[j]!='.') b[j]='0';
                                 if (a[j]!='.' && a[j]!='0') ++h;
                               }
        u=a[i]; v=b[i];
        if (h>0) ++u;
        if (u==v) ;
        else if (u=='0' && k==0) b[i]='1';
        else if (u=='0') b[i]='0';
        else if (u<'6' && v>'4') b[i]='5';
        else if (u<'3' && v<='3') b[i]='2';
        else if (u<'5' && v<'5') b[i]='4';
        else if (u=='6' && v=='7') b[i]='6';
        else b[i]='8';
        return(merkki*atof(b));
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

static void alkukoodit()
        {
        int i;

        *pr_tila=EOS;
        pr_osoitin=pr_tila;
        n_sana=0;
        for (i=0; i<256; ++i) code[i]=(unsigned char)i;
        }

static int lue_koodit(char *x)
        {
        char *sana[16];
        int i,n;
        char x1[LLENGTH];
        char y[3*LLENGTH];
        char *p;

        p=x;
        while ( (p=strchr(p,TAB))!=NULL ) { *p=' '; ++p; }

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
        if (muste_strcmpi(sana[0],"PS_CODE")==0) { i=ps_code(x1,sana,n,x); return(i); }

        if (pr_type==1)
            {
            i=strlen(x); while(x[i-1]==' ' && i>1) x[--i]=EOS;
            if (i==1) return(1);  /* tyhjÑ rivi */
            p=x; while (*p==' ') ++p;
            i=muunna(p,y); if (i<0) return(-1);
            send(y);     /* kirjoita(y) */
            }
        return(1);
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

static int codes(char *x,char **sana,int n)   /* codes <kooditiedosto> */
        {
        load_codes(sana[1],code);
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
                    vdc(atoi(s+2),x);
                    for (i=0; i<strlen(x); ++i, ++y) *y=x[i];
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

static void koodivirhe(char *x)
        {
        PR_EBLD;
//      printf("\nErroneous code line/word:\n%s",x);
        sprintf(sbuf,"Invalid code line/word: %s",x);
        p_error(sbuf);
        WAIT; PR_ENRM;
        }

static int space_split(char rivi[],char *sana[],int max)
/* jakaa rivin sanoiksi sana[0],sana[1],...,sana[max-1]
   Vain vÑlilyînnit toimivat erottimina.
   Jos merkkijonoa rivi muutetaan, sana[] tuhoutuu!
   return (sanojen lkm)
*/
        {
        int g=0;
        int p;
        int edell=0; /* vÑli edellÑ */
        int len=strlen(rivi);
        for (p=0; p<len; ++p)
                {
                if (rivi[p]==' ')
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

static int dos(char *x)
        {
        char y[LLENGTH];
        char *osa[2];
        int i,len;

        strcpy(y,x);
        i=split(y,osa,2);
        if (i<2) return(1);
        i=osa[1]-y;
        len=strlen(x);
        while (x[len-1]==' ') x[--len]=EOS;
        muste_system(x+i,FALSE); // RS CHA
        return(1);
        }

static int include(char *x,char **sana,int n)
        {
        FILE *ifile;
        char rivi[LLENGTH];
        int i,len;

        strcpy(rivi,sana[1]);
        if (strchr(rivi,':')==NULL && *rivi!='.' && *rivi!='/' && *rivi!='\\') // RS ADD unix path FIXME
            {
            strcpy(rivi,survo_path); strcat(rivi,"SYS/");
            strcat(rivi,sana[1]);
            }
        ifile=muste_fopen(rivi,"rt");
        if (ifile==NULL)
            {
//          PR_EBLD;
            sprintf(sbuf,"Include file %s not found!",rivi);
//          sur_print(sbuf);
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



/*  pcur.c 11.7.1985/SM (11.2.1991)
    PLOT curves
*/

static void muste_pcur(int argc, char *argv[])
        {
        int i,k,v,ind;
        char laite[LLENGTH];
        char gtype[16];

        if (argc==1) return;
        s_init(argv[1]);
        argv1=argv[1];

        ind=integral_function=0;
        if (muste_strcmpi(word[1],"INTEGRAL")==0)
            ind=integral_function=1;

        i=tutki_yhtalo(); if (i<0) return;

     	muste_gplot_init=1;
     	k=sp_init(r1+r-1);
     	muste_gplot_init=0;
        if (k<0)
            {
            sur_print("\n Too many specifications!");
            WAIT; return;
            }

        i=spfind("DEVICE");
        if (i<0) strcpy(laite,"MUSTE_PR.PS"); // RS CHA PRN -> MUSTE_PR.PS
        else
            {
            strcpy(laite,spb[i]);
            if (strchr(laite,':')==NULL && laite[0]!='/' && laite[0]!='.' && laite[0]!='\\') // RS unix path FIXME
                {
                strcpy(laite,edisk);
                strcat(laite,spb[i]);
                }
            }

        i=p_init(laite); if (i<0) return;
        curves();
        edisp=1; s_end(argv[1]);
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

static void missing_char(char ch,int j)
        {
        error_line(j);
        sprintf(sbuf,"\n%c missing in equation!",ch);
        sur_print(sbuf);
        WAIT;
        }

static void incorrect_varname(int j)
        {
        error_line(j);
        sprintf(sbuf,"\nIncorrect name for a variable (X,x,Y,y allowed)");
        sur_print(sbuf);
        WAIT;
        }

static void error_line(int j)
        {
        sprintf(sbuf,"\nError on edit line %d:",j);
        sur_print(sbuf);
        }


static void plot_tscale() { }

static int curves()
        {
        int i;
        char otsikko[LLENGTH];

        tee_otsikko(otsikko);
        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
        if (pr_type==1 || pr_type==2)
         { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
        i=xyscale("X"); if (i<0) { p_end(); return(-1); }
        i=xlabel(xmuunnos); if (i<0) { p_end(); return(-1); }
        i=xyscale2("X"); if (i<0) { p_end(); return(-1); }
        i=xyscale("Y"); if (i<0) { p_end(); return(-1); }
        i=ylabel(ymuunnos); if (i<0) { p_end(); return(-1); }
        i=xyscale2("Y"); if (i<0) { p_end(); return(-1); }
        i=xgrid(); if (i<0) { p_end(); return(-1); }
        i=ygrid(); if (i<0) { p_end(); return(-1); }
        i=xtick(1); if (i<0) { p_end(); return(-1); }
        i=ytick(1); if (i<0) { p_end(); return(-1); }
        i=xtick(2); if (i<0) { p_end(); return(-1); }
        i=ytick(2); if (i<0) { p_end(); return(-1); }
        i=fill(); if (i<0) { p_end(); return(-1); }

        i=plot_curves(); if (i<0) { p_end(); return(-1); }

        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1 && pr_type!=2)
         { i=frames(); if (i<0) { p_end(); return(-1); } }
        i=fills(); if (i<0) { p_end(); return(-1); }
        i=polygons(); if (i<0) { p_end(); return(-1); }
        p_end();
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


static int xyscale(char *suunta) /* "X" tai "Y" */
        {
        extern double arit_atof();
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

static int xrajat()
        {
        extern double xmu();
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
        extern double ymu();
        ymin=ymumin=yscaleval[0];
        ymax=ymumax=yscaleval[yscalen-1];
        if (ymax<=ymin) { rajavirhe('Y'); return(-1); }
        if (*ymuunnos==EOS) return(1);
        ymumin=ymu(ymin);
        ymumax=ymu(ymax);
        return(1);
        }

static void rajavirhe(char c)
        {
        sprintf(sbuf,"\nIncorrect range of values in %cSCALE!",c);
        sur_print(sbuf);
        WAIT;
        }

static int varnimet()
        {
        sp_listaus(muuttujanimi);    /* spa[0] */
        sp_listaus("x");
        sp_listaus("y");
        return(spn);
        }

static void sp_listaus(char *s)
        {
        int k;
        k=strlen(s);
        strncpy(spl,s,k);
        spa[spn]=spl; spb[spn]=NULL;
        spl+=k+1; *(spl-1)=EOS;
        ++spn;
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
                    p_text(c_text[i],c_x[i],c_y[i],1);
                    laske(c_message[i],&a);

                    sprintf(c_text[i],"%g",a);

// 2.8.2009 needed in "Buffon's needle problem"
       k=strlen(c_text[i]); if (k>max_len) max_len=k;
       p_fill_bar(c_x[i],c_y[i],c_x[i]+max_len*(int)kirjainlev,c_y[i]+(int)kirjainkork,1);

                    char_color=0; p_charcolor();
                    p_text(c_text[i],c_x[i],c_y[i],1);
                    }
                }


            if (*color_change) change_color();

            coord(t,&x_pos,&y_pos);

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
            x_pos=xx+x_kuva*(xmu(xq)-xmumin)/(xmumax-xmumin);
            y_pos=yy+y_kuva*(ymu(yq)-ymumin)/(ymumax-ymumin);

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
        *px=xx+x_kuva*(xmu(x)-xmumin)/(xmumax-xmumin);
        *py=yy+y_kuva*(ymu(y)-ymumin)/(ymumax-ymumin);
        return(1);
        }

static void outline(double xs,double ys,double xu,double yu,int *px,int *py)
/* double xs,ys;  sisÑpiste */
/* double xu,yu;  ulkopiste */
/* int *px,*py;   rajakoordinaatit */
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

static void xy_arvot(double t,double *px,double *py)
        {
        int i;

        for (i=0; i<spn; ++i) spb[i]=spb2[i];
        arvo[0]=t;
        if (*xlauseke==EOS) *px=t; else laske(xlauseke,px);
        laske(ylauseke,py);
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


static int spfind2(char *s,int k)
        {
        int i;
        for (i=k; i<spn; ++i)
                if (strcmp(s,spa[i])==0) return(i);
        return(-1);
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
            if (k<0) return(-1); //RS CHA FIXME exit(1);
            k=conditions(&curd); if (k<0) return(-1); //RS CHA FIXME exit(1);
            obs=curd.l1;
            while (obs<=curd.l2 && unsuitable(&curd,obs)) ++obs;
            data=1;
            loopar[nloop]=-1;
            ++nloop;
            }

        lag[nvar]=0;
        p=strchr(sana[1],'[');
        if (p!=NULL) { *p=EOS; lag[nvar]=atoi(p+1); }
        k=varfind(&curd,sana[1]); if (k<0) return(-1); //RS CHA FIXME exit(1);
        curd_var[nvar]=k;
        i=data_load(&curd,obs+(long)lag[nvar],curd_var[nvar],&arvo[sp_ind[nvar]]);
        ++nvar;
        return(i);
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

static void change_color()
        {
        double color;

        color=arit_atof(color_change);
        if (color_max==1) line_color=color;
//      else line_color=(int)fmod(color,(double)color_max);
        else line_color=(int)color%color_max; // 7.7.2010
        p_lineattr();
        }



static int fill()
        {
        int i,k;
        char x[LLENGTH], *sana[3];
        char *p;

        filltype=0;
        i=spfind("FILL");
        if (i<0) i=spfind("XFILL");
        if (i>=0) filltype=1;
        else
            {
            i=spfind("YFILL");
            if (i>=0) filltype=2;
            else
                {
                i=spfind("OFILL");
                if (i>=0) filltype=3;
                else
                    {
                    i=spfind("IFILL");
                    if (i>=0) filltype=4;
                    }
                }
           }

        if (filltype==0) return(1);
        strcpy(x,spb[i]);
        strcpy(curve_fill_attr,x); // 20.5.2005
        k=control_code(x,&p,1);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        k=split(p,sana,3);
        if (k<2)
            {
//          printf("\nEnter fill parameters in form:");
//          printf("\n%s=<step>,<lower_limit>,<upper_limit>",spa[i]);
      sprintf(sbuf,"Fill parameters in form: %s=<step>,<lower_limit>,<upper_limit>"
                    ,spa[i]);
            p_error(sbuf);

            WAIT; return(-1);
            }
        fill_step=arit_atoi(sana[0]);
        if (fill_step<1) fill_step=1;
        fill_start=t_start; fill_end=t_end;
        if (k>1) fill_start=arit_atof(sana[1]);
        if (k>2) fill_end=arit_atof(sana[2]);
        return(1);
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

static int x_coord(double x)
        {
        return(xx+x_kuva*(xmu(x)-xmumin)/(xmumax-xmumin));
        }

static int y_coord(double y)
        {
        return(yy+y_kuva*(ymu(y)-ymumin)/(ymumax-ymumin));
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

/* curarit1.c 31.10.1985/SM  (20.6.1992)
   aritmetiikka (+ - * / ^) ja yhden muuttujan funktiot
   +probit()+tilap.funktiot
*/


static double arit_atof(char *lauseke)
        {
        double y;
        laske(lauseke,&y);
        return(y);
        }

static int arit_atoi(char *lauseke)
        {
//        extern double arit_atof();
        return((int)arit_atof(lauseke));
        }


static int laske(char *lauseke,double *y)
        {
//        double luku();
//        double oper();
//        double funktio();
/*      double mfunktio();
*/
        char x[MAXPITUUS];
        char *p,*q;
        char sana[32];
        int len;
        double opnd[MAXARG+4]; char op[MAXARG+4]; int v[MAXARG+4];
        int t,n;
/*      int narg;    Usean muuttujan funktion argumenttien lkm     */
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
//              if (*q==')') { printf("\nArguments missing in %s",lauseke);
//                             l_virhe=1; return(-1); }
                if (*q==')')
                    {
                    sprintf(sbuf,"Arguments missing in %s",lauseke);
                    p_error(sbuf);
                    }
                n=1;
/*              narg=1;
*/              while (n)
                    {
                    ++p;
                    if (*p=='(') { ++n; continue; }
                    if (*p==')') { --n; continue; }
//                  if (*p==EOS) { printf("\n) is missing in %s",lauseke);
//                                 l_virhe=1; return(-1); }
                    if (*p==EOS)
                        {
                        sprintf(sbuf,") is missing in %s",lauseke);
                        p_error(sbuf);
                        }
/*                  if (*p==',' && n==1)
                        {
                        *p=EOS;

                        laske(q,&opnd[t]);
                        ++t;
                        if (t>MAXARG+3)
                            { printf("\nToo many arguments in %s",lauseke);
                              l_virhe=1; return(-1); }
                        ++narg;
                        q=p+1;
                        }
*/
                    }

//              if(strchr("+-*/^)\0",*(p+1))==NULL) { syntax_error(lauseke);
//                                                    return(-1); }
// mahdollinen loopar muotoa a(step)b 4.9.2001
                if(strchr("+-*/^)\0",*(p+1))==NULL) return(2);

                *p=EOS; ++p;

                i=laske(q,&opnd[t]);
                if (i<0 || l_virhe) return(-1);
// RS REM                if (i==2) { printf("\nret2"); getch(); }

/*   printf("\ntulos1=%f",opnd[t]); getch();  */
                if (len==0) { len=-1; break; }
                sana[len]=EOS;

/*              if (narg>1)
                    {

             printf("\nArgumentit: ");
             for (i=t-narg+1; i<=t; ++i) printf(" %g",opnd[i]); getch();

                    t=t-narg+1;
                    if (*sana=='-')
                        opnd[t]=-mfunktio(sana+1,opnd+t,narg);
                    else
                        opnd[t]=mfunktio(sana,opnd+t,narg);
                    if (l_virhe) return(-1);
                    len=-1;
                    break;
                    }
*/
                /* Yhden muuttujan funktiot */
                if (*sana=='-')
                    opnd[t]=-funktio(sana+1,opnd[t]);
                else
                    opnd[t]=funktio(sana,opnd[t]);
                if (l_virhe) return(-1);
                len=-1;
                break;

              case ')':
//              printf("\n( missing in %s",lauseke); l_virhe=1; return(-1);
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
                }
            }

        if (len<0) { v[t++]=0; }
        else
                   if (len>0) { opnd[t]=luku(sana,len); v[t++]=0; }

        supista(&t,opnd,op,v);
        *y=opnd[0];

        return(1);
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

static double oper(double x1,double x2,char laji)
        {
//        extern double power();

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

static double funktio(char *s,double x)
        {
//        extern double probit();
//        extern double gamma();
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
    	if (strcmp(S,"SGN")==0) return(muste_sign(x)); // RS CHA
    	if (strcmp(S,"IND")==0) return(muste_ind(x)); // RS CHA
    	if (strcmp(S,"RND")==0) return(uniform(x));
    	if (strcmp(S,"RAND")==0) return(uniform(x)); // RS CHA
    	if (strcmp(S,"PROBIT")==0) return(probit(x));
    	if (strcmp(S,"ROUND")==0) return(sur_round(x));
    	if (strcmp(S,"FACT")==0) return(fact(x));
    	if (strcmp(S,"LFACT")==0 || strcmp(S,"FACT.L")==0) return(lfact(x));
    	if (strcmp(S,"NFACTORS")==0)
      	  {
      	  if (x>4294967295.0)
            {
            sur_print("\nMax. permitted integer 4294967295=2^32-1");
            WAIT;
            return(0.0);
            }
          return(nfactors(x));
          }        	
    	if (strcmp(S,"TOTIENT")==0) return(totient(x)); // 19.4.2009
    	if (strcmp(S,"ZETA")==0) return(zeta(x));
    	if (strcmp(S,"LGAMMA")==0) return(muste_lgamma(x)); // RS 
    	if (strcmp(S,"GAMMA")==0) return(muste_gamma(x)); // RS 
    	if (strcmp(S,"DIGAMMA")==0) return(muste_digamma(x)); // RS 
    	if (strcmp(S,"TRIGAMMA")==0) return(muste_trigamma(x)); // RS 
    	if (strcmp(S,"TETRAGAMMA")==0) return(muste_tetragamma(x)); // RS 
    	if (strcmp(S,"PENTAGAMMA")==0) return(muste_pentagamma(x)); // RS 
        if (strcmp(S,"SGAMMA")==0) return(sur_gamma(x)); /* 2.11.1999 */

        i=f_edit(s,&x,1,&y); if (i>0) return(y);    /* 20.6.1992 */
/*      i=f_tiedosto(s,&x,1,&y); if (i>0) return(y);
*/
        f_tuntematon(s);
        l_virhe=1;
        return(x);
        }

static int f_edit(char *s,double *x,int n,double *py)
        {
        int i,k,len;
        char lauseke[LLENGTH];
        char xx[LLENGTH], *osa[MAXARG];
        char sana[7];     /*  EARG 1 2 3 4 EARG EOS */
        double y;
        char *p,*q;
        int h;

        len=strlen(s); s[len++]='(';
        i=0;
        while (i<spn && (spp[i]!=':' || strncmp(s,spa[i],len)!=0)) ++i;
        if (i==spn) { s[len-1]=EOS; return(-1); }
/*
printf("spa=%s spp=%c spb=%s\n",spa[i],spp[i],spb[i]); getch();
*/
        if (!earg_varattu) { k=varaa_earg(); if (k<0) return(-1); } 

        strcpy(lauseke,spb[i]);
        strcpy(xx,spa[i]);
        i=split(xx+len,osa,MAXARG);
        if (i!=n)
           {
           sprintf(sbuf,"\nArgument error in function %s",s); sur_print(sbuf);
           l_virhe=1; WAIT; return(-1);
           }
        osa[n-1][strlen(osa[n-1])-2]=EOS;   /* ): poistetaan */
/*
    for (i=0; i<n; ++i) printf("\nosa %d: %s",i+1,osa[i]); getch();
*/
        for (i=0; i<n; ++i)
            {
            k=aseta_earg(x[i],sana); if (k<0) return(-1);
            korvaa2(lauseke,osa[i],sana);
            }
/* printf("x[0]=%g x[1]=%g\n",x[0],x[1]); getch(); */
        laske(lauseke,&y);
/* printf(" y=%g\n",y); getch(); */
        *py=y;
        n_earg-=n;
        return(1);
        }

static void korvaa2(char *s,char *x,char *y)
        {
        char *p,*q;
        char z[LLENGTH];
        int len=strlen(x);

        *z=EOS;
        p=s;
        while ((q=strstr(p,x))!=NULL)
            {
            if (strchr(",+-*/^)=<>!",*(q+len))!=NULL || *(q+len)==EOS)
                {
                strncat(z,p,q-p);
                strcat(z,y);
                p=q+len;
                }
            else  /* x osa funktion nimeÑ */
                {
                strncat(z,p,q-p);
                strcat(z,x);
                p=q+len;
                }
            }
        strcat(z,p);
        strcpy(s,z);
        }
        
static int varaa_earg()
        {
        earg=(double *)malloc(MAXEARG*sizeof(double));
        if (earg==NULL)
            {
            sur_print("\nNot enough memory!");
            l_virhe=1;
            WAIT; return(-1);
            }
     /* earg_varattu=1; */
        return(1);
        }        

static int aseta_earg(double luku,char *sana)
        {
        char sana2[5];

        sana[0]=EARG;
        if (n_earg>=MAXEARG)
            {
            sur_print("\nStack overflow in editorial functions!");
            WAIT; l_virhe=1;
            return(-1);
            }
        sana[1]=EOS; strcat(sana,muste_itoa(n_earg,sana2,10));
        earg[n_earg++]=luku;
        return(n_earg-1);
        }



static void f_tuntematon(char *s)
        {
//      printf("\nUnknown function %s",s);
        sprintf(sbuf,"Unknown function %s",s);
        p_error(sbuf);
        l_virhe=1;
        }

static void arg_virhe(char *s)
        {
//      printf("\n%s: Error in arguments",s);
        sprintf(sbuf,"%s: Error in arguments",s);
        p_error(sbuf);
        l_virhe=1;
        }

static void syntax_error(char *s)
        {
//      printf("\nsyntax error in %s",s);
        sprintf(sbuf,"Syntax error in %s",s);
        p_error(sbuf);
        l_virhe=1;
        }

static int laske2(char *muuttuja,double *y)
        {
        int i,k;
/*
        extern int sp_read;
        if (!sp_read)
            {
            i=sp_init(r1+r-1); sp_read=1;
            if (i<0)
                {
                printf("\nToo many specifications!");
                WAIT;
                exit();
                }
            }
*/
                                                                 /* 20.6.92 */
        if (*muuttuja==EARG) { *y=earg[atoi(muuttuja+1)]; return(1); }
        i=spfind(muuttuja);
        if (i<0)
            {
//          printf("\nParameter %s not found!",muuttuja);
            sprintf(sbuf,"Parameter %s not found!",muuttuja);
            p_error(sbuf);
            WAIT;
            p_end();
            l_virhe=1; // RS ADD
            return(-1); // RS CHA FIXME exit(1);

            }
        if (spb[i]==NULL) { *y=arvo[i]; return(1); }
        k=laske(spb[i],y);
        if (k==2) { k=read_loopar(i); if (k<0) return(-1); *y=arvo[i]; return(1); }
        arvo[i]=*y;
        spb[i]=NULL;
        return(1);
        }

/*
 *
 * NAME:    lg_gamma(x)
 *
 * PURPOSE: Returns the natural logarithm of the gamma function.
 *
 * FORM:    #include <math.h>
 *                Library: distrib.lib
 *
 *                double lg_gamma(x);
 *                double x;
 *
 * ACCURACY: The accuracy of the machine.
 *
*/

static double lg_gamma(double x)
{
 double x2,y,z,u,p0,p1,p2,q0,q1,z0;

 p0=0.2791953179185250;
 p1=0.4917317610505968;
 p2=0.6929105992918886e-1;
 q0=3.350343815022304;
 q1=6.012459259764103;
 z0=0.9189385332046727;

 if (x<12.0)
  {
   z=x;
   while (++x<12.0)
    z=z*x;
   z=-log(z);
  }
 else
  z=0.0;
 x2=x*x;
 u=(x-0.5)*log(x)-x+z0;
 y=1.0/x2;
 if (x<1000.0)
  {
   p0=p0+y*(p1+y*p2);
   q0=q0+y*(q1+y);
   z=z+u+(p0/q0)/x;
  }
 else
  {
   p0= 8.333333333333333e-2;
   p1=-2.777777777777778e-3;
   p2= 7.936507936507937e-4;
   z=u+(p0+(p1+p2/x2)/x2)/x;
  }
 return(z);
}

static double sur_gamma(double z)
    {
    return(exp(lg_gamma(z)));
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
                    p_error2(sbuf);
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

static void if_syntax_error(char *x)
        {
//      printf("\nSyntax error in %s\n",x);
        sprintf(sbuf,"Syntax error in %s",x);
        p_error2(sbuf);
        WAIT; l_virhe=1;
        }






/*  pbar.c 2.11.1985/SM (2.2.1993)
    PLOT bar charts etc.
*/

static void muste_pbar(int argc, char *argv[])
        {
        int i,k,v;
        char laite[LLENGTH];
        char gtype[LLENGTH];

        if (argc==1) return;
        s_init(argv[1]);
        argv1=argv[1];
     	muste_gplot_init=1;
     	k=sp_init(r1+r-1);
     	muste_gplot_init=0;
        if (k<0)
            {
            sur_print("\n Too many specifications!");
            WAIT; return;
            }
/*  for (i=0; i<k; ++i) printf("\n%s  %s",spa[i],spb[i]); getch();
*/

        i=spfind("DEVICE");
        if (i<0) strcpy(laite,"MUSTE_PR.PS"); // RS CHA PRN -> MUSTE_PR.PS
        else
            {
            strcpy(laite,spb[i]);
            if (strchr(laite,':')==NULL && laite[0]!='/' && laite[0]!='.' && laite[0]!='\\') // RS unix path FIXME
                {
                strcpy(laite,edisk);
                strcat(laite,spb[i]);
                }
            }
        i=p_init(laite); if (i<0) return;
        if (strcmp(word[1],"?")==0) { p_inquiry(); p_end(); edisp=1; s_end(argv[1]); return; }

        i=spfind("TYPE");
        if (i>=0) strcpy(gtype,spb[i]); else strcpy(gtype,"HBAR");
        i=spfind("MINVALUE");
        if (i>=0) minvalue=atof(spb[i]); else minvalue=-1e30;

        if (muste_strcmpi(word[1],"/FRAME")==0) { pframe(); edisp=1; s_end(argv[1]); return; }

        v=0;
        if (muste_strcmpi(gtype,"HBAR")==0) v=1;
        if (muste_strcmpi(gtype,"%HBAR")==0) v=2;
        if (muste_strcmpi(gtype,"MHBAR")==0) v=3;
        if (muste_strcmpi(gtype,"%MHBAR")==0) v=4;
        if (muste_strcmpi(gtype,"%AHBAR")==0) v=5;
        if (muste_strcmpi(gtype,"NHBAR")==0) v=6;    /* 2.2.1993 */
        if (muste_strcmpi(gtype,"PYRAMID")==0) { v=6; pyramid=1; } // 18.10.2005
        if (v) { hbar(v,gtype,word[1]); edisp=1; s_end(argv[1]); return; }

        if (muste_strcmpi(gtype,"VBAR")==0) v=1;
        if (muste_strcmpi(gtype,"%VBAR")==0) v=2;
        if (muste_strcmpi(gtype,"MVBAR")==0) v=3;
        if (muste_strcmpi(gtype,"%MVBAR")==0) v=4;
        if (muste_strcmpi(gtype,"%AVBAR")==0) v=5;
        if (muste_strcmpi(gtype,"NVBAR")==0) v=6;    /* 2.2.1993 */
        if (v) { vbar(v,gtype,word[1]); edisp=1; s_end(argv[1]); return; }

        if (muste_strcmpi(gtype,"PIE")==0) v=1;
        if (muste_strcmpi(gtype,"%PIE")==0) v=2;
        if (v) { pie(v,gtype,word[1]); edisp=1; s_end(argv[1]); return; }

        sprintf(sbuf,"\nUnknown TYPE=%s",gtype); sur_print(sbuf);
        WAIT; p_end(); return;

        }

static int pen()
        {
        int i;

        i=spfind("PEN");
        if (i>=0) pen_code=spb[i];
        else      pen_code=NULL;
        return(1);
        }

static int linetype()
        {
        int i;

        i=spfind("LINETYPE");
        if (i>=0) line_code=spb[i];
        else      line_code=NULL;
        return(1);
        }

/* RS double definition
static double xmu(double x)
        { return(x); }

static double ymu(double x)
        { return(x); }

static int read_loopar()
        {
        p_error("Comma (,) not allowed!");
//      sur_print("\nComma (,) not allowed!");
        WAIT;
        return(-1);
        }

static int varnimet()         // curspec:in takia
        {
        extern int spn;
        return(spn);
        }
*/

static void free_all()
        {
        fcloseall();
        free_spec();
        }



static int lines()
        {
        int h,i,k;
        char x[LLENGTH];
        char *p, *ps;
        int ntext;
        char *tnimi[MAXTEXTS];
        char y[LLENGTH];
        int nt;
        char *sana[40];
        char pkoodi[LLENGTH], kopio[LLENGTH];


        i=p_linetype(); if (i<0) return(-1);
        i=spfind("LINES");
        if (i<0) return(1);
        strcpy(x,spb[i]);
        k=etsi_loppusulku(x,&p); if (k<0) return(-1);
        *pkoodi=EOS;
        if (p!=x)
            {
            if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
            *(p-1)=EOS; strcpy(pkoodi,x+1); ++p;
            strcpy(kopio,pkoodi);
            k=p_textcontrol(kopio); if (k<0) return(-1);
            }
        ntext=split(p,tnimi,MAXTEXTS);
        for (h=0; h<ntext; ++h)
            {

            i=spfind(tnimi[h]);
            if (i<0) return(-1);
            if (*pkoodi==EOS) p_linetype();
            else      { strcpy(kopio,pkoodi); p_linecontrol(kopio); }
            strcpy(y,spb[i]);
            k=control_code(y,&p,1);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            if (spshad[i]==NULL) ps=NULL;
            else { k=p-y; ps=spshad[i]+k; }
            nt=split(p,sana,40);

            p_path(nt,sana);   /* 24.6.1992 */
            }
        return(1);
        }
        
