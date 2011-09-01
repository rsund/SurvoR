/* p.c 11.7.1986/SM (14.6.1992)
   PLOT operations
   
 g:\ve1\p\bar.obj
 g:\ve1\p\bar2.obj
 g:\ve1\p\vbar.obj
 g:\ve1\p\pie.obj 
 g:\ve1\p\dia2.obj
 g:\ve1\p\dia3.obj
 g:\ve1\p\diafill.obj
 g:\ve1\p\trend.obj
 g:\ve1\p\his1.obj
 g:\ve1\p\his2.obj
 g:\ve1\p\hisf.obj
 g:\ve1\p\hisf2.obj
 g:\ve1\p\hisf3.obj
 g:\ve1\p\hisf4.obj
 g:\ve1\p\hisp.obj
 g:\ve1\p\hisval.obj
 g:\ve1\p\hf.obj
*/



#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define PI 3.141592653589793
/*
//#define PI 3.14159265
#define PI 3.141592653589793
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
#define EARG '\376' 
#define STEPOSA 10.0
#define NVARFACES 18
#define NRAJAT 16
#define NV 6
#define AMAX 17
#define NYVAR 12
#define NPOINT 6
#define LINEPOINTSPACE 32*NPOINT
#define MAXPAR 20
#define CFUNCTION -1
#define OWN_DISTR 0
#define NORMAL 1
#define BINOMIAL 2
#define POISSON 3
#define LOGNORMAL 4
#define UNIFORM 5
#define MATRIX 6
#define SPX_CONST -32091
#define N MAXPAR
*/

extern char **spa,**spb,**spshad,**spb2;
extern int spn;
extern double *arvo;
extern char *spl;
extern char *spp;
extern int specmax;

extern int muste_gplot_init;
extern char muuttujanimi[];
extern char muuttujanimi2[];
extern char muuttujanimi3[];
extern char muuttujanimi4[];


#include "plotvars.h"


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
static void p_contour_init();
static void p_contour_plot(int ny,int iy,int nx,int *pxl_value);
static int lue_koodit(char *x);
static int plot_arrows();
static int pl_triangle(int x1,int y1,int x2,int y2,int x3,int y3,int t);


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
color_max=0; // RS REMOVE?
lopetus=0;
kosketus=0;
n_mess=0;
l_virhe=0;
n_sana=0;
pr_osoitin=NULL;
pr_type=1;
color_2010=1;
em=em2=en=l1=l2=edat=0;
namevar=0;
grouping_var=0;
devvar1=devvar2=0;
prind=0;
page_number=0;
andrews_polar=0;
polar_constant=0.0; // *
star_plot=0;
minx=NULL,maxx=NULL;
q1=q2=q3=q4=q5=q6=qr=0;
t0=ts=u=0;
xco=yco=0;
scale=0.45; // *
mean=NULL; stddev=NULL;
n=NULL;
na=0;
dmin=NULL,dmax=NULL;
xcorner=NULL,ycorner=NULL;
draval=NULL;
m=0;
dxsize=dysize_muste=dxgap=dygap=0;
laajuus=0;
values=NULL;
jitter_step=NULL;
nval=NULL;
jitter=0;
rand_seed=0;
insc=0;
xpp=ypp=0;
point_given=point_var=point_size_varying=0;
point_max=0;
scalefile=NULL;
dc=0;
staval=NULL;
n_patkat=0;
valpaikka=valpros=0;
valind=0;
labpaikka=0;
name_ind=0;
name_gap=0;
valuemin=0;
devvar1=devvar2=0;
nplan=0;
xvar=yvar=tvar=aika=line=0;
normal=0;
nyvar=0;
rajat_etsitty=0; // *
out=missing=prev_missing=0;
point_given=point_var=0;
point_color_var=0;
point_type_var=-1; // *  RS Was 0???
point_size_varying=0;
thickness=thickgap=0;
i_thick=0;
lag=0;
x_lag=y_lag=0;
missline=0; // *
obs_found=0;
n_normal=i_normal=0;
nline2=0;
plinepoint=NULL;
linetype1=NULL;
pointtype1=NULL;
marker_type1=0;
marker_size1=0;
xp=yp=0;
line_polygon_fill=0; // *
n_poly=0;
temp_poly=NULL;
jitter=0;
xjitter=yjitter=0;
xxx=yyy=0;
A=NULL;
arrowm=arrown=0;
rlab=NULL; clab=NULL;
lr=lc=0;
type=0;
fill_var=fill_gap=fill_neg_gap=0;
fill_const=0;
fill_start2=fill_end2=0;
fill_line=1; // RS *
trend=contour=0;
tn=0;
tx=ty=tx2=ty2=txy=0;
freq_file=NULL;
results_line=0;
x_lower=x_step=x_upper=0;
n_class=0;
freq=NULL;   
freq_est=NULL;
n_freq=n_out=0;
fr=NULL;
border_cases=middle_cases=0;
skip_errors=0; // *
valpaikka=0;
valpros=0;
valind=0;
valuemin=0;
dnro=0;
npar=npar_est=0;
imin=imax=0;
f_integral=0;
f_type=0;
prob=NULL;
step_divisor=0;
matrix_fit=0;
mat_est=0; // *
pp=NULL;
mp=np=0;
mtype=0;
nparn=0;
nf=0;
integral_is_one=0;


        if (argc==1) return;
        s_init(argv[1]);

        if (g==1)
            {
            sur_print("\nIncomplete PLOT operation!");
            WAIT; return;
            }

muuttujanimi[0]=EOS; // RS ADD
strcpy(muuttujanimi2,"x"); // RS ADD
strcpy(muuttujanimi3,"y"); // RS ADD
muuttujanimi4[0]=EOS;

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
                   s_end(argv[1]);                   
                   muste_contour(2,argv); // RS CHA suorita("CONTOUR.EXE",argv[1]);
                   return;
                   }
            if (strcmp(osa[0],"FACES")==0 || strcmp(osa[0],"ANDREWS")==0
                || strcmp(osa[0],"DRAFTS")==0 || strcmp(osa[0],"STARS")==0
                || strcmp(osa[0],"PROFILES")==0)
                   {
                   strcpy(info,osa[0]);
                   s_end(argv[1]);                   
                   muste_faces(2,argv); // RS CHA suorita("FACES.EXE",argv[1]);
                   return;
                   }
            }
        if (muste_strcmpi(word[0],"HISTO")==0) 
        	{ 
        	muste_histo(2,argv); // RS CHA suorita("PHIS.EXE",argv[1]);
        	return; 
        	}
        if (strchr(word[1],'=')!=NULL || muste_strcmpi(word[1],"INTEGRAL")==0)
            {             
            muste_pcur(2,argv); // RS CHA suorita("PCUR.EXE",argv[1]); 
            return;
            }
        if (muste_strcmpi(word[1],"FUNCTION")==0) { suorita("PFUNC.EXE",argv[1]); return; }
        if (muste_strcmpi(word[1],"FILE")==0) { suorita("FILE.EXE",argv[1]); return; }

        if (g<3)
        	{         	
        	muste_pbar(2,argv); // RS CHA suorita("PBAR.EXE",argv[1]); 
        	return; 
        	}
        muste_pdia(2,argv); // RS CHA suorita("PDIA.EXE",argv[1]);
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

// static int p_error2(char *s)  { p_error(s); return(1); }
static int p_error2(char *s) { return(p_error(s)); }

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
        muste_fclose(kirjoitin);
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

    muste_fclose(poly_tmp);

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

static int lue_koodit(char *x)
        {
        char *sana[16];
        int i,n;
        char x1[LLENGTH];
        char y[3*LLENGTH];
        char *p;

        p=x;
// RS CHA        while ( (p=strchr(p,TAB))!=NULL ) { *p=' '; ++p; }
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
        
static int plot_arrows()
    {
    int i,k;
    long j1,j2;
    int x1,y1,x2,y2,x0,y0,x00,y00,x11,y11,x21,y21;
    char x[LLENGTH],*s[5];
    int gap,gap2,atype,atype0,alen;
    double angle,ang;
    double a,a0,b;
    char *p;
    int color;
    char y[LLENGTH];
    extern int line_width;
    int linetype;

    i=spfind("ARROWS"); if (i<0) return(1);
    gap=0;
    atype=0; // 0,1,2,3
    alen=20;
    ang=PI/6.0;

    strcpy(x,spb[i]);

    k=control_code(x,&p,1);
    if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }

    i=split(p,s,5);
    if (i>1) gap=arit_atoi(s[1]);
    if (i>2) atype=atype0=arit_atoi(s[2]);
    if (i>3) alen=arit_atoi(s[3]);
    if (i>4) ang=arit_atof(s[4]);
    i=matrix_load(s[0],&A,&arrowm,&arrown,&rlab,&clab,&lr,&lc,&type,expr);
    if (i<0) return(1);
// printf("\nplot_arrows!"); getch();
/********************************  28.10.2009
    if (arrowm!=arrown)
        {
        p_error("Not a square matrix for ARROWS!"); return(-1);
        }
********************************/
    line_width=1;
    color=0;
    a0=1.0; // edellinen a

    for (j1=d.l1; j1<d.l1+arrowm; ++j1)  // 28.10.2009
        {
        for (j2=d.l1; j2<d.l1+arrown; ++j2)
            {
            a=A[(j2-d.l1)*arrowm+j1-d.l1];
            if (a==0.0) continue;

/*****************************************
a=swww.tacc w=line_width t=linetype a=atype c=color
  if s=-, atype=-atype

*****************************************/
            atype=atype0;
            if (a!=1.0)
                {
                line_width=(int)a; if (line_width<0) line_width=-line_width;
                sprintf(x,"%.4f",a);
                p=strchr(x,'.');
                if (p==NULL)
                    {
                    linetype=0;
                    atype=0;
                    color=0;
                    }
                else
                    {
                    linetype=*(p+1)-'0';
                    atype=*(p+2)-'0'; if (a<0.0) atype=-atype;
                    color=atoi(p+3);
                    }
                }

            coords(j1,xvar,yvar,0.0,0.0,&x1,&y1);
            coords(j2,xvar,yvar,0.0,0.0,&x2,&y2);

            angle=atan2((double)(y2-y1),(double)(x2-x1));

            gap2=gap;
            i=atype; if (i<0) i=-atype;
            if (i==2 || i==3)
                {
                gap2+=(int)((double)alen*cos(ang));
                }
            if (gap>0)
                {
                x1=(int)((double)x1+(double)gap2*cos(angle));
                y1=(int)((double)y1+(double)gap2*sin(angle));
                x2=(int)((double)x2-(double)gap*cos(angle));
                y2=(int)((double)y2-(double)gap*sin(angle));
                }

            x21=x2; y21=y2;


            if (i==1 || i==3 || i==4)
                {
                x21=(int)((double)x2-(double)alen*cos(angle)*cos(ang));
                y21=(int)((double)y2-(double)alen*sin(angle)*cos(ang));
                }

            x11=x1; y11=y1;

            if (i==4)
                {
                x11=(int)((double)x1+(double)alen*cos(angle)*cos(ang));
                y11=(int)((double)y1+(double)alen*sin(angle)*cos(ang));
                }

            send("gsave ");

            if (a!=1.0)
                {
                sprintf(x,"[line_type(%d)]",linetype);
                muunna(x,y);
                send(y);
                p_fillattr(-color);
// printf("\nwidth=%d color=%d type=%d atype=%d|",
//         line_width,color,linetype,atype); getch();
                p_line3(x11,y11,x21,y21,1);
                }
            else
                {
                p_line2(x11,y11,x21,y21,1);
                }
            a0=a;



            i=atype; if (i<0) i=-atype;
            if (atype)
                {
                if (i==1 || i==3 || i==4)
                    {
                    b=angle+ang;
                    x0=x2-(int)((double)alen*cos(b));
                    y0=y2-(int)((double)alen*sin(b));

                    b=angle-ang;
                    x00=x2-(int)((double)alen*cos(b));
                    y00=y2-(int)((double)alen*sin(b));

                    pl_triangle(x0,y0,x2,y2,x00,y00,atype);

                    }

                if (i==2 || i==3)
                    {
                    b=angle+ang;
                    x0=x1-(int)((double)alen*cos(b));
                    y0=y1-(int)((double)alen*sin(b));
                    b=angle-ang;
                    x00=x1-(int)((double)alen*cos(b));
                    y00=y1-(int)((double)alen*sin(b));
                    pl_triangle(x0,y0,x1,y1,x00,y00,atype);
                    }

                if (i==4)
                    {
                    b=angle+ang;
                    x0=x1+(int)((double)alen*cos(b));
                    y0=y1+(int)((double)alen*sin(b));
                    b=angle-ang;
                    x00=x1+(int)((double)alen*cos(b));
                    y00=y1+(int)((double)alen*sin(b));
                    pl_triangle(x0,y0,x1,y1,x00,y00,atype);
                    }

                }
            send("grestore ");
            }
        }
    return(1);
    }

static int pl_triangle(int x1,int y1,int x2,int y2,int x3,int y3,int t)
    {
    char s[LNAME];

    sprintf(s,"\nnewpath %d m %d m moveto ",x1,y1); send(s);
    sprintf(s,"%d m %d m lineto %d m %d m lineto\n",x2,y2,x3,y3); send(s);
    if (t<0) { strcpy(s,"closepath fill\n"); send(s); }
    else { strcpy(s,"stroke\n"); send(s); }
    return(1);
    }        


#include "plotfunc.h"


        
