
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
#define NMAT 5

static char *str_opnd[MAXARG+4];
static double *mat_mat[NMAT];
static char *mat_rlab[NMAT],*mat_clab[NMAT];
static int mat_lr[NMAT],mat_lc[NMAT];
static int mat_m[NMAT],mat_n[NMAT];
static int mat_nmat;
static char mat_name_arit[NMAT][9];

static int earg_varattu=0;
static int n_earg=0;
static double *earg;


static char path[LNAME];   /* G or PS or anything else (6.6.1992) */


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
static unsigned char code[512]; // RS ADD unsigned
static char *pen_code;         /* PEN=pen_code */
static char *line_code;        /* LINETYPE=line_code */
static double minvalue;        /* MINVALUE=pienin piirrettÑvÑ pylvÑÑn korkeus */
static double xmin,xmax,xmumin,xmumax,ymin,ymax,ymumin,ymumax;

static int colors_2010;

static int marker_type, marker_size;

static int x_origin,y_origin;

static int line_color=0;
static int char_color;
// static char p_outfile[1]; /* muita varten */
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
//static char *shadow[],*shadow2[];
//static char framecode[];
static char *argv1;

static int capability[2];
    /*
      capability[0]   1=vÑlitulostukset sallittu 0=ei
      capability[1]   1=autom.fill               0=ei

    */

static int slow=0; // 6.4.2010

static unsigned char *ps_str[256]; // RS ADD unsigned

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

static char zscales[SCALESPACE], *zscal[NPAR]; /* XSCALE2 ja YSCALE2 */
static double zscaleval[NPAR];
static int zscalen;            /* skaala-arvojen lkm */

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
static SURVO_DATA curd;
static int curd_var[MAXLOOP];
static int sp_ind[MAXLOOP];
static int lag_datapar[MAXLOOP];  /* 21.6.92 */
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
static char *xname[NVAR];
// static char *xnames[LLENGTH];
static char xtype[NVAR]; // 18.10.2005 TYPE=PYRAMID
static int   em,em2,en,l1,l2,edat;
static int   ev[NVAR];
static int namevar;

static SURVO_DATA dat;

static double xmat[MAXDATA];  //  RS FIXME size? MAXDATA > 2*NOBS
static char xnimi[2*NOBS][100]; // RS FIXME size?
static int grouping_var;
static char gnimi[2*NOBS][100]; // RS FIXME size?

static int devvar1,devvar2;
static double devmat[2*NOBS];

// RS CHA extern static char muuttujanimi1[LLENGTH], muuttujanimi2[LLENGTH];
static char lauseke[LLENGTH];
static SURVO_DATA d;
static double x_start,x_end,x_step;
static double y_start,y_end,y_step;
static int nx,ny;
static int *pxl_value;

static double *min_arvo,*max_arvo;
static double miss_zarvo;
static int namevar;
static int rowlabels,columnlabels;
static char rowlabel_code[LLENGTH],columnlabel_code[LLENGTH];
static int norm; /* 1=Cols, 2=Rows, 3=Total */
static int nimimax;

static int prind=1;

static char otsikko[LLENGTH];
static int page_number;

static int andrews_polar;
static double polar_constant=0.0;
static int star_plot;

//static int namevar;
//static int nimimax;

static int v[NVAR];
static double min[NVAR],max[NVAR];
static double fmin_faces[NVAR],fmax_faces[NVAR];
static double val[NVAR],y[NVAR];
static char flabelcode[LLENGTH];
static double *minx,*maxx;
static char *list[]=
  {"Parameter list for Chernoff's faces:                                   ",
   " - in the first column is to be replaced by a name of a variable.      ",
   " In columns xmin and xmax, * is current minimum, ** is current maximum.",
   " All the 18 VARIABLES lines must appear in the order below.            ",
   " The COLORS lines are optional and valid in screen graphics only.      ",
   " Face: Age 7:0 15:2 30:3F7                                             ",
   " means that variable 'Age' determines the color of the face contour    ",
   " using color 0 for 'Age'<=7, color 2 for 7<'Age'<=15, etc.             ",
   " 'F7' means that the face is to be filled with color 7.                ",
   "VARIABLES: xmin      xmax     Features                       fmin fmax ",
   " -        *         **        Radius_to_corner_of_face_OP    0.6  1.0  ",
   " -        *         **        Angle_of_OP_to_horizontal      0.0  0.6  ",
   " -        *         **        Vertical_size_of_face_OU       0.6  1.0  ",
   " -        *         **        Eccentricity_of_upper_face     0.5  1.5  ",
   " -        *         **        Eccentricity_of_lower_face     0.5  1.5  ",
   " -        *         **        Length_of_nose                 0.1  0.5  ",
   " -        *         **        Vertical_position_of_mouth     0.2  0.8  ",
   " -        *         **        Curvature_of_mouth_1/R        -4.0  4.0  ",
   " -        *         **        Width_of_mouth                 0.2  1.0  ",
   " -        *         **        Vertical_position_of_eyes      0.0  0.4  ",
   " -        *         **        Separation_of_eyes             0.3  0.8  ",
   " -        *         **        Slant_of_eyes                 -0.5  0.5  ",
   " -        *         **        Eccentricity_of_eyes           0.3  1.0  ",
   " -        *         **        Size_of_eyes                   0.1  0.2  ",
   " -        *         **        Position_of_pupils            -0.1  0.1  ",
   " -        *         **        Vertical_position_of_eyebrows  0.2  0.4  ",
   " -        *         **        Slant_of_eyebrows             -0.5  0.5  ",
   " -        *         **        Size_of_eyebrows               0.1  0.5  ",
   "COLORS:                                                                ",
   "Face: -                                                                ",
   "Eyes: -                                                                ",
   "Pupils: -                                                              ",
   "Eyebrows: -                                                            ",
   "Mouth: -                                                               ",
   "Nose: -                                                                ",
   "END of plotting specifications                                         "};
static char *piirre[]=
  {"Face","Eyes","Eyebrows","Pupils","Mouth","Nose"};
/*   0      1       2          3       4       5        */
static int lpiirre[]=
  {  4,     4,      8,         6,      5,      4   };
static int vv[NV];
static int nv[NV];
static int vfill[NV][NRAJAT];
static double vraja[NV][NRAJAT];
static int vcolor[NV][NRAJAT];

static double q1,q2,q3,q4,q5,q6,qr;
static double t0,ts,u;
static double xco,yco;
static double scale=0.45;

static double *mean,*stddev;
static long *n;
static int na;
static double aa[AMAX],bb[AMAX],yf[AMAX];

static char *andrew_list2[]=
   {"Specifications for Andrews' function plots f(t):",
    " Transformed variables X'=(X-A)/B               ",
    " * as A is mean(X) and * as B is stddev(X).     ",
    "VARIABLES: A        B        Term               ",
    " -         *        *        1/sqrt(2)          ",
    " -         *        *        sin(t)             ",
    " -         *        *        cos(t)             ",
    " -         *        *        sin(2*t)           ",
    " -         *        *        cos(2*t)           ",
    " -         *        *        sin(3*t)           ",
    " -         *        *        cos(3*t)           ",
    " -         *        *        sin(4*t)           ",
    " -         *        *        cos(4*t)           ",
    "END of plotting specifications                  "};

static double *dmin,*dmax;
static int *xcorner,*ycorner;
static double *draval;
static int m;
static int dxsize,dysize_muste,dxgap,dygap;
static int laajuus;

static double *values;
static double *jitter_step;
static int *nval;
static int jitter;
static int rand_seed;
static int insc;
static int xpp,ypp;
static int point_given,point_var,point_size_varying;
static char point_text[LLENGTH];
static double point_max;
static char point_code[LLENGTH];
static FILE *scalefile;

static double dc;
static double *staval;

//static double xval[NVAR];
//static char *xlab[NVAR];
static double xsumma[NOBS];  /* 10.7.89 */
static int n_patkat;    // 15.4.2011
static char patka[10][64];

static char valform[LLENGTH], valcode[LLENGTH];
static double valpaikka; 
static int valpros;   /* VALUES=(valcode),valform,valpaikka */
static int valind;
static char labcode[LLENGTH];
static double labpaikka;            /* LABELS=(labcode),labpaikka */
static char namecode[LLENGTH];      /* NAMES=(namecode),<1||0> */
static int name_ind;
static int name_gap; // 3.5.2004
static double valuemin;
static char devcode[LLENGTH];      // DEV=(devcode),devvar1,devvar2  30.4.2004
static int devvar1,devvar2;
static int nplan, mplan[32];


static char aineisto[LLENGTH];
static int xvar,yvar,tvar,aika,line;
static int normal;  /* 1=YSCALE probit scale */
static int nyvar, yvars[NYVAR];
static int rajat_etsitty=0;

static int out,missing,prev_missing;
static int point_given, point_var;
static int point_color_var; // 11.5.2005
static int point_type_var=0; // 22.5.2005
//static char point_text[16]; RS [LLENGTH] above
//static double point_max;
static int point_size_varying;
static int thickness, thickgap;
static char line_label[LLENGTH];
static int x_thick[]={ 0,2,-2,0,0,4,-4,0,0,3,-3,-3,3 };
static int y_thick[]={ 0,0,0,2,-2,0,0,4,-4,3,-3,3,-3 };
static int i_thick;
static int lag;
static double x_lag,y_lag;
static int missline=0;   /* 26.9.93 */
static int obs_found;

static long n_normal, i_normal;
static int nline2, line2_x[NPOINT], line2_y[NPOINT];
static double xline2[NPOINT], yline2[NPOINT];
static char linepoint_tila[LINEPOINTSPACE], *plinepoint;
static char *linetype1, *linetype2[NPOINT];
static char *pointtype1, *pointtype2[NPOINT];
static int marker2[NPOINT], markersize2[NPOINT];
static int marker_type1, marker_size1;
static int xp,yp;
static int line_polygon_fill=0;
static int n_poly;
static FILE *temp_poly;

static int jitter; // 17.3.2002
static double xjitter,yjitter;

static double xxx,yyy;

static double *A;
static int arrowm,arrown;
static char *rlab,*clab;
static int lr,lc;
static int type;
static char expr[129];

static int fill_var, fill_gap, fill_neg_gap;
static double fill_const;
static long fill_start2, fill_end2;
static int fill_line=1;

static int trend,contour;
static int conf_band[4];
static long tn;
static double tx,ty,tx2,ty2,txy;

static char *freq_file;
static char varname[9];
static int results_line;
static double x_lower,x_step,x_upper;
static int n_class;
static long *freq;   /* -18.10.1996 unsigned int */
static double *freq_est;
static long n_freq,n_out;
static char his_attributes[LLENGTH];
static FILE *fr;
// static char argvv[LNAME];
static long border_cases,middle_cases;
static int skip_errors=0; // 26.1.2008
static char valform[LLENGTH], valcode[LLENGTH];
static double valpaikka; 
static int valpros;   /* VALUES=(valcode),valform,valpaikka */
static int valind;
static double valuemin;
static char distr[LLENGTH];
static int dnro;
static double dpar[MAXPAR];
static int npar, npar_est;
static int imin,imax;
static double f_integral;
static int f_type; /* 1=density, 0=probability */
static double *prob; /* pointer to probabilities */
static char fit_attributes[LLENGTH];
static int step_divisor;  /* 13.7.1994 */
static int matrix_fit; // 2.5.2001
static int mat_est=0; // 2.5.2001
static double *pp;
static int mp,np;
static int mtype;
static char parnimet[LLENGTH], *parnimi[MAXPAR];
static int nparn;
static int nf;
static int integral_is_one;


static int p_inquiry();
static void print_rivi(char *x,int j);
static int win_tulostus();
static int control_code(char *x,char **pp,int laji); // PA
static int etsi_loppusulku(char *x,char **pp);
static void load_codes(char *codefile,unsigned char *code);
// static int p_empty(char *s);
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
static int xgrid(); // PGR2
static double grid_alku(double min,double max,double step);
static int ygrid();
static int xtick(int type);
static int ytick(int type);
static int skaala_arvot(); // PSC
static void scale_err(char *s);
static int autom_scale(char *x,double min,double max,int npos);
static double paras_arvo(double x,double y);
static int xyscale2(char *suunta); // PSC2
static void control_code_scale(char *s);
static void plot_xscale2();
static void plot_yscale2();
static double xmu(double x); // PMU
static double ymu(double x);
static void alkukoodit(); // PR2
static int define(char *x,char **sana,int n,char *rivi);
static int shadows(char *x,char **sana,int n,char *rivi);
static int codes(char *x,char **sana,int n);
static int muunna(char *sana,char *muunnos);
static void koodivirhe(char *x);
static int space_split(char rivi[],char *sana[],int max); // PRC
static int makro(char *sana,char *muunnos); // PRM
static void korvaa(char *muunnos,char *s,char *t);
// static int dos(char *x);
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
// static int varnimet();
// static void sp_listaus(char *s);
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
static double mfunktio(char *s,double *x,int n);
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
// static int read_loopar();
// static int varnimet();
//static void free_all();
static int hbar(int gtype,char *type,char *data);
static int pframe();
static int prosentit();
static int plot_hbar(int gtype);
static int patki1(char *nimi,char lab_delimiter,int *pnimimax2);
static int patki2(char *nimi,char lab_delimiter);
static void maxmin_sum(double *max,double *min);
static void maxmin(double *max,double *min);
static int xyscale_bar(int gtype,char *suunta);
static int barvalues();
static int valtext(int x1,int y1,int leveys,double arvo,int color);
static void valtext2(int x1,int y1,int korkeus,double arvo,int color);
static int valtextpie(int xp,int yp,double r,double t1,double t2,double arvo,int color);
static int bar_labels();
static void labtext(int x1,int y1,int leveys,char *teksti,int color);
static void labtext2(int x1,int y1,int korkeus,char *teksti,int color);
static void labtextpie(int xp,int yp,double r,double t1,double t2,char *teksti,int color);
static int names();
static int dev_spec(char *s);
static int vbar(int gtype,char *type,char *data);
static int plot_vbar(int gtype);
static int pie(int gtype,char *type,char *data);
static int plot_pie(int gtype);
static void autom_plan(int n);
static void plot_sector(int xr,int yr,double rx,double ry,double a1,double a2);

static int lines(); // PBLIN

static void muste_contour(); // CONTOUR
static int tutki_yhtalo_contour();
static int contours();
static int plot_contours();
static int plotting_range_contour(char *muuttujanimi,double *t_start,double *t_end,double *t_step);
static void not_enough_memory();
static int matrix();
static int plot_matrix();
static int tutki_data();

static void muste_faces(); // FACES
static void lopetus_faces(char *argv1);
static int xyscale_faces(char *suunta);
static int faces(char *otsikko);
static int fheader(char *otsikko);
static int init_faces();
static int plot_faces();
static int tutki_data_faces();
static int tutki_lista();
static void minmaxkorvaa(char *x,char *p,int k,double a);
static int tutki_varit(int j);
static int select_color(long j,int i,int *pfill);
static void plot_face();
static void curve_plot(int g,int x1,int y1,int flev,int fkork);
static void laske_f(int g,double t);
static int sgn_faces(double x);
static void koordinaatit();
static int andrews(char *otsikko);
static int init_andrews();
static int plot_andrews();
static int tutki_data2();
static int tutki_lista2();
static void plot_andrews_curve();
static void koord2(double x,double y,int *pxk,int *pyk);
static double andrews_function(double *yf,int na,double t);
static int plot_apolar();
static void plot_apolar_curve();
static int drafts(char *otsikko);
static int init_drafts();
static int plot_drafts();
static void plot_dboxes();
static int tutki_data3();
static void aseta_luokitus(int i,double x);
static void jitter_steps();
static double uniform_faces(double x);
static int draft_point();
static int draft_merkitse(long j,int x,int y);
static int outscale(double *dmin,double *dmax,double *jitter_step);
static int inscale(double *dmin,double *dmax,double *jitter_step,int *nval);
static int init_stars();
static int plot_stars();
static void plot_star();
static void plot_profile();


static void muste_pdia(); // PDIA
static int var_error(char *s);
static int diagrams();
static void tee_otsikko_dia(char *ots);
static void tee_label(char *ots,int var,char *muunnos);
static int xyscale_dia(char *suunta);
static int etsi_rajat(char *suunta);
static void plot_tscale_dia();
static int plot_diagram();
static int get_marker_rot_angle(long j);
static int merkitse(long j,int x,int y);
static int coord_dia(long j,int *px,int *py);
static int xy_arvot_dia(long j,double *px,double *py);
static int normal_check();
static int coords(long j,int xvar,int yvar,double xconst,double yconst,int *px,int *py);
static int sp_line(int var);
static int lines2();
static int sp_point(int var);
static int points2();
static int sp_lag();
static int etumerkinta(char *x,char **pp);
static int diafill();
static int coord2(long j,int *px,int *py,int k);
static int xy_arvot2_dia(long j,double *px,double *py);
static int fill_find(char *v);
static int fill_neg_find(char *v);
static int xyscale2_dia(char *suunta);
static int init_trend();
static int init_contour();
static int init_conf_band();
static void null_mom();
static void compute_moments(double x,double y);
static int plot_trend();
static void plot_line_segment(double mx,double my,double tx,double ty);
static void xy_point(double x,double y,int *px,int *py);
static int linscale_only();
static int plot_contour();
static int xy_sisalla(double x,double y);
static void ellipse(double mx,double my,double sx,double sy,double r,double t,double eps,double *px,double *py);
static int find_binorm(double *pmx,double *pmy,double *psx,double *psy,double *pr);
static int plot_conf_band(int conf_type);

static void muste_histo(); // PHIS
static void class_error();
static void liikaa_spec();
static int frekvenssit();
static int varaa_tilat();
static void save_freq();
static int load_freq();
static void freq_error(char *nimi,char *x);
static int histogram();
static int xyscale_histo(char *suunta);
static int plot_histogram();
static int plot_distribution();
static int plot_probabilities();
static void coord_histo(double x,double y,int *px,int *py);
static int his_values();
static void his_valtext2(int x1,int y1,int korkeus,double arvo);
static int fitting();
static int fit_distr();
static int sp_fit();
static int fit_normal();
static int fit_lognormal();
static int fit_uniform();
static int fit_matrix();
static int fit_binomial();
static int fit_poisson();
static int prob_varaus(unsigned int n);
static void mean_var(double *pmean,double *pvar,double (*f)());
static int total_integral(double *a);
static int f_estimates();
static void integrate();
static double density(int dnro,double x,double *a);
static double f_normal(double x,double *a);
static double f_lognormal(double x,double *a);
static double f_uniform(double x,double *a);
static double pr_binomial(double x,double *a);
static double pr_poisson(double x,double *a);
static double nof(double x);
static double f(double x,double *a);
static double pr_matrix(double x,double *a);
static int find_own_distr();
static int etsi_distr(char *tyyppi,char *distr);
static int sp_update();
static int fit_own_distr();
static double f_own_distr(double x,double *a);
static int estimate();
static int nelder();
static double logll(double *a);
static int spfind2_histo(char *s,int i);
static int estim_results();
static void numhess(double *a,double *H,int m,double *step);
static int corrnorm(double *H,int m);
static int cholinv(double a[],int n);
static void printout();
static void eoutput(char *rivi);
static char *spois(char *s);
