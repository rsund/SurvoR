#include "muste.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <string.h>
#include "survo.h"

#define MAXPLOTWINDOWS 300
#define MAXFONTS 300

extern int muste_evalr(char *cmd);
extern int muste_sleep(int sleeptime);
extern int muste_iconv(char *teksti,char *to,char *from);
extern int muste_requirepackage(char *package);
extern int muste_get_R_int(char *sour);
extern double muste_get_R_real(char *sour);
extern int muste_get_R_string(char *dest,char *sour,int length);
extern int muste_get_R_int_vec(char *sour,int element);
extern double muste_get_R_real_vec(char *sour,int element);
extern int muste_get_R_string_vec(char *dest,char *sour,int length,int element);
extern void muste_set_R_int(char *dest,int luku);
extern void muste_set_R_string(char *dest,char *sour);
extern double muste_R_function(char *s,double *x,int n);
extern void muste_Survo2R(char *dest,char *sour);
extern void muste_R2Survo(char *dest,char *sour);
extern void muste_init_plotwindows(void);

extern int Muste_EvalTcl(char *, int);
extern int disp(void);


extern FILE *muste_fopen(char *path, char *mode);
extern FILE *muste_fopen2(char *path, char *mode);
extern void *muste_malloc(size_t n);
extern void *muste_realloc(void *p,size_t n);
extern int muste_free(void *p);
extern int muste_fclose(FILE *p);
extern int muste_fclose2(void *p);

extern unsigned char *shadow_code;
extern int display_off;

extern int c3,c;
extern int r3,r;
extern int sdisp;
extern int scroll_line;
extern int space_break;

extern char muste_linecolor[]; // RS 15.1.2013
extern char muste_charcolor[];
extern char muste_pencolor[];
extern char muste_fontfamily[];
extern char muste_fontweight[];
extern char muste_fontslant[];
extern double muste_fontsize;
extern char *muste_pencolor2;
extern int muste_lopetus;

extern int muste_window_existing;
extern int muste_window_minimized;
extern char muste_window[];
extern char muste_plotwindow[];
extern char muste_plotcanvas[];
extern int muste_old_plotid;

extern char muste_window_name[]; 
extern int muste_canvasfonts[];


static char komento[3*LLENGTH]; /* 256 */
static char tclkomento[3*LLENGTH]; /* 256 */
static char plotkomento[3*LLENGTH]; /* 256 */

extern void muste_fixme(char *kommentti);
extern char *muste_get_clipboard(void);
extern void muste_copy_to_clipboard(char *x);
extern int muste_evalclipboard(void);

int muste_vconx=0;
int muste_vcony=0;
int survo_webedit=0;

char muste_default_cursor_color[]="#F00";
char muste_default_insert_color[]="#90F";


void muste_flushscreen() {
    sprintf(komento,"update idletasks");
    Muste_EvalTcl(komento,FALSE);
}

void muste_fixme(char *kommentti)
  {
  Rprintf("%s",kommentti);
  }

extern int sur_locate_router(int,int);
int sur_locate(int row,int col)
{
    sur_locate_router(row,col);
    return(1);
}

void cursor(unsigned int r,unsigned int c)
        {
        if (c>c3) c=c3;
        sur_locate(r+1,c+8);
        }

int sur_cursor_position(int *prow,int *pcol)
        {
// RS REM    SEXP avar=R_NilValue;

    sprintf(komento,".muste.getcursor()");
    muste_evalr(komento);

//    avar = findVar(install(".muste$cursor.row"),R_GlobalEnv);
//    *prow=INTEGER(avar)[0];
	*prow=muste_get_R_int(".muste$cursor.row");

//    avar = findVar(install(".muste$cursor.col"),R_GlobalEnv);
//    *pcol=1+INTEGER(avar)[0];
	*pcol=1+muste_get_R_int(".muste$cursor.col");    

        return(1);
        }

int sur_cursor_move(int drow,int dcol)
        {
        int row,col;
        sur_cursor_position(&row,&col);
        sur_locate(row+drow,col+dcol);
        return(1);
        }


int sur_set_cursor(int dwSize, int bVisible)
    {
    int i;
	char *ss[2];
	extern int hae_apu(char *s,char *t);
	extern int splitq(char *rivi,char **sana,int max);
	extern void muste_set_R_string(char *dest,char *sour);
	
	i=hae_apu("cursor_color",plotkomento); // RS 26.11.2012
	if (i) i=splitq(plotkomento,ss,2);
	if (i<2) { ss[0]=muste_default_cursor_color; ss[1]=muste_default_insert_color; }
	muste_set_R_string(".muste$insertcursorcolor",ss[1]);
    if (!bVisible) dwSize=0;
    if (dwSize>100)
       { 
         dwSize-=100;
         sprintf(komento,"configure -insertwidth %d -insertbackground \"%s\"",dwSize,ss[1]);
         Muste_EvalTcl(komento,TRUE);
       }
    else
       {
         sprintf(komento,"configure -insertwidth %d -insertbackground \"%s\"",dwSize,ss[0]);
         Muste_EvalTcl(komento,TRUE);
       }
    
    return(1);
    }

void cursor_on()
        {
        extern int insert_mode;
        if (insert_mode) CURSOR_INS; else CURSOR_ON;
        }


int sur_mem_cursor(int mode) /* 1=save 2=restore */
        {
        static int row,col;

        if (mode==1) sur_cursor_position(&row,&col);
        else sur_locate(row,col);
        return(1);
        }

int sur_set_console_title(char *title)
	{
	int i;
    
    snprintf(komento,LLENGTH,"tkwm.title(.muste$ikkuna, \"%s\")",title);
    for (i=26; i<strlen(komento); i++) // RS 30.5.2014
    	{
    	if (komento[i]=='\\') komento[i]='/';
    	if (komento[i]=='[') komento[i]='(';
    	if (komento[i]==']') komento[i]=')';
    	if (komento[i]=='$') komento[i]='_';
    	if (komento[i]=='"') komento[i]='\'';
    	}    	    
    muste_evalr(komento);
	return 1;
	}

int sur_taskbar_show(int status)
   {
muste_fixme("FIXME: sur_taskbar_show not implemented!\n"); // RS FIXME
   return(1);
   }

int sur_find_window(char *winname)
   {
muste_fixme("FIXME: sur_find_window not implemented!\n"); // RS FIXME
   return(-1);
   }

void sur_move_window(char *wname,int p1,int p2, int p3, int p4)
   {
muste_fixme("FIXME: sur_move_window not implemented!\n"); // RS FIXME
   return;
   }

void sur_show_window(int id,int show)
   {
muste_fixme("FIXME: sur_show_window not implemented!\n"); // RS FIXME
   return;
   }

int sur_screen_dim(int *sizex,int *sizey)
        {
// RS REM    SEXP avar=R_NilValue;

    sprintf(komento,".muste.getscreendim()");
    muste_evalr(komento);

//    avar = findVar(install(".muste$screen.width"),R_GlobalEnv);
//    *sizex=INTEGER(avar)[0];

	*sizex=muste_get_R_int(".muste$screen.width");

//    avar = findVar(install(".muste$screen.height"),R_GlobalEnv);
//    *sizey=INTEGER(avar)[0];

	*sizey=muste_get_R_int(".muste$screen.height");
        return(1);
        }

void sur_get_window_rect(char *wname,int par[])
   {
// RS REM      SEXP avar=R_NilValue;

    sprintf(komento,".muste.getwindowdim()");
    muste_evalr(komento);
    
//    avar = findVar(install(".muste$window.topx"),R_GlobalEnv);
//    par[0]=INTEGER(avar)[0];

	par[0]=muste_get_R_int(".muste$window.topx");

//    avar = findVar(install(".muste$window.topy"),R_GlobalEnv);
//    par[1]=INTEGER(avar)[0];

	par[1]=muste_get_R_int(".muste$window.topy");

//    avar = findVar(install(".muste$window.bottomx"),R_GlobalEnv);
//    par[2]=INTEGER(avar)[0];
    
	par[2]=muste_get_R_int(".muste$window.bottomx");    

//    avar = findVar(install(".muste$window.bottomy"),R_GlobalEnv);
//    par[3]=INTEGER(avar)[0];
    
    par[3]=muste_get_R_int(".muste$window.bottomy");

        return;
   }

int muste_get_window_caption()
	{
//	SEXP avar=R_NilValue;
//	avar = findVar(install(".muste$window.caption"),R_GlobalEnv);
//    return(INTEGER(avar)[0]);
    return(muste_get_R_int(".muste$window.caption"));
	}

int muste_get_window_xframe()
	{
//	SEXP avar=R_NilValue;
//	avar = findVar(install(".muste$window.xframe"),R_GlobalEnv);
//    return(INTEGER(avar)[0]);
    return(muste_get_R_int(".muste$window.xframe"));
	}
	
int muste_get_window_yframe()
	{
//	SEXP avar=R_NilValue;
//	avar = findVar(install(".muste$window.yframe"),R_GlobalEnv);
//    return(INTEGER(avar)[0]);
    return(muste_get_R_int(".muste$window.yframe"));    
	}

/*
Image to canvas
x <- tkcmd("image", "create", "photo", 
           file="/usr/lib/tk8.3/demos/images/teapot.ppm") 
tkpack(canv<-tkcanvas(tt<-tktoplevel())) 
tkcreate(canv, "image", 50,50, image=x) 



Jpeg support with activetcl, path varies by installation and os
addTclPath("/System/Library/Tcl") 
tclRequire("Img") 
*/


void muste_init_plotwindows()
	{
	int i;
	
	sprintf(komento,".muste$plotwin <- list()");
    muste_evalr(komento);
 
 	sprintf(komento,".muste$plotwin[[%d]] <- 0.0",MAXPLOTWINDOWS);
    muste_evalr(komento);

	sprintf(komento,".muste$plotwinsize <- list()");
    muste_evalr(komento);
 
 	sprintf(komento,".muste$plotwinsize[[%d]] <- 0.0",MAXPLOTWINDOWS);
    muste_evalr(komento);

	sprintf(komento,".muste$canvas <- list()");
    muste_evalr(komento);
 
 	sprintf(komento,".muste$canvas[[%d]] <- 0.0",MAXPLOTWINDOWS);
    muste_evalr(komento);    
 
 	sprintf(komento,".muste$canvasfonts <- list()");
    muste_evalr(komento);
 
 	sprintf(komento,".muste$canvasfonts[[%d]] <- 0.0",MAXPLOTWINDOWS);
    muste_evalr(komento); 

	for (i=0; i<MAXPLOTWINDOWS; i++) muste_canvasfonts[i]=0;    
	}

void sur_pos_window(char *wname,int x,int y)
   {
   sprintf(komento,"tcl(\"wm\",\"geometry\",%s,\"+%d+%d\")",wname,x,y);
   muste_evalr(komento);
   }

int muste_focus_from_plotwin_to_editor(int id)
	{
// RS REM	int opt[4];

    sprintf(komento,".muste.focus.editor()");
    muste_evalr(komento);

/*
    sprintf(komento,"tkfocus(.muste$txt)");
    muste_evalr(komento);

// RS Needed to get focus back to Muste editor in Mac
    sur_get_window_rect(muste_window_name,opt);
    sprintf(komento,"tkwm.withdraw(.muste$ikkuna)");
    muste_evalr(komento);
    sprintf(komento,"tkwm.deiconify(.muste$ikkuna)");
    muste_evalr(komento);  
    sur_pos_window(muste_window_name,opt[0],opt[1]);    
*/


/*	RS This causes problems in WIN7

    sprintf(komento,"tklower(.muste$plotwin[[%d]],.muste$ikkuna)",id);
    muste_evalr(komento);
*/


	return(0);
	
	}
	
int muste_plottcl(int id, char *komento, int win)
	{
// RS REM	SEXP avar=R_NilValue;
	
	if (id!=muste_old_plotid)
		{
		sprintf(plotkomento,".muste$plotwinid<-.Tk.ID(.muste$plotwin[[%d]])",id);
		muste_evalr(plotkomento);
	
		sprintf(plotkomento,".muste$canvasid<-.Tk.ID(.muste$canvas[[%d]])",id);
		muste_evalr(plotkomento);
	
//		avar = findVar(install(".muste$plotwinid"),R_GlobalEnv);
//    	strcpy(muste_plotwindow,CHAR(STRING_ELT(avar,0)));
		muste_get_R_string(muste_plotwindow,".muste$plotwinid",64);
//    	strcat(muste_plotwindow," ");	
    	
//    	avar = findVar(install(".muste$canvasid"),R_GlobalEnv);
//    	strcpy(muste_plotcanvas,CHAR(STRING_ELT(avar,0)));
		muste_get_R_string(muste_plotcanvas,".muste$canvasid",64);    	
//    	strcat(muste_plotcanvas," ");
    	
    	muste_old_plotid=id;
		}
	
	if (win) sprintf(plotkomento,"%s %s",muste_plotwindow,komento);
	else sprintf(plotkomento,"%s %s",muste_plotcanvas,komento);
//Rprintf("\n%s",plotkomento);

    Muste_EvalTcl(plotkomento,FALSE);
    return(1);
// Rprintf("L?ytyi ikkuna: %s\n",muste_window);
    }	

int muste_window_style(int id,int style)
	{
	int tyyli;
	
	tyyli=0;
	if (style==0) tyyli=1;
	
	sprintf(komento,"tkwm.overrideredirect(.muste$plotwin[[%d]],%d)",id,tyyli);	
    muste_evalr(komento);
    
	sprintf(komento,"tkwm.withdraw(.muste$plotwin[[%d]])",id);
    muste_evalr(komento);

	sprintf(komento,"tkwm.deiconify(.muste$plotwin[[%d]])",id);
    muste_evalr(komento);
	
	return(0);
	}


int muste_canvas_background(int id,char *color)
	{

	sprintf(komento,"tkconfigure(.muste$canvas[[%d]],background=\"%s\")",id,color);
    muste_evalr(komento);	
//tkconfigure(.muste$canvas[[1]],background="#00ff00")
	return(1);
	}

	

extern int muste_x_wsize,muste_y_wsize,muste_x_size,muste_y_size;	
	
int muste_line_plot(int id,double x1,double y1,double x2,double y2)
	{
	extern int line_type,line_width;

//    sprintf(komento,"tkcreate(.muste$canvas[[%d]],\"line\",%g,%g,%g,%g)",id,x1,y1,x2,y2);
//    muste_evalr(komento);
	double xkerroin,ykerroin,leveys;
	xkerroin=(double)((double)muste_x_wsize/(double)muste_x_size);
	ykerroin=(double)((double)muste_y_wsize/(double)muste_y_size);	
	x1*=xkerroin; x2*=xkerroin;
	y1*=ykerroin; y2*=ykerroin;
	leveys=line_width*((xkerroin+ykerroin)/2);

	sprintf(komento,"create line %g %g %g %g -tags lw%d -fill %s -width %g",x1,y1,x2,y2,line_width,muste_pencolor,leveys);
    switch (line_type)
    	{
    	case 0: strcat(komento," -capstyle round"); break;
    	case 1: strcat(komento," -dash --"); break;
    	case 2: strcat(komento," -dash ,"); break;
    	case 3: strcat(komento," -dash -."); break;
    	case 4: strcat(komento," -dash -"); break;
    	case 5: strcat(komento," -dash -.."); break;
    	case 6: strcat(komento," -dash ."); break;
    	case 7: strcat(komento," -dash -..."); break;
    	case 8: strcat(komento," -capstyle projecting"); break;
    	}

    muste_plottcl(id, komento, FALSE);

	return(0);
	}

int muste_curve_plot(int id,double x1,double y1,double x2,double y2,double cx1,double cy1,double cx2,double cy2) 
	{
// RS 27.12.2012	
// the list of coordinates is such that the first coordinate pair (and every third coordinate pair thereafter)
// is a knot point on a cubic Bezier curve, and the other coordinates are control points on the cubic Bezier
// curve. Straight line segments can be generated within a curve by making control points equal to their
// neighbouring knot points. If the last point is a control point and not a knot point, the point is repeated
// (one or two times) so that it also becomes a knot point.	
	extern int line_type,line_width;

	double xkerroin,ykerroin,leveys;
	xkerroin=(double)((double)muste_x_wsize/(double)muste_x_size);
	ykerroin=(double)((double)muste_y_wsize/(double)muste_y_size);	
	x1*=xkerroin; x2*=xkerroin;
	y1*=ykerroin; y2*=ykerroin;
	cx1*=xkerroin; cx2*=xkerroin;
	cy1*=ykerroin; cy2*=ykerroin;	
	leveys=line_width*((xkerroin+ykerroin)/2);

	sprintf(komento,"create line %g %g %g %g %g %g %g %g -smooth raw -tags lw%d -fill %s -width %g",x1,y1,cx1,cy1,cx2,cy2,x2,y2,line_width,muste_pencolor,leveys);
    switch (line_type)
    	{
    	case 0: strcat(komento," -capstyle round"); break;
    	case 1: strcat(komento," -dash --"); break;
    	case 2: strcat(komento," -dash ,"); break;
    	case 3: strcat(komento," -dash -."); break;
    	case 4: strcat(komento," -dash -"); break;
    	case 5: strcat(komento," -dash -.."); break;
    	case 6: strcat(komento," -dash ."); break;
    	case 7: strcat(komento," -dash -..."); break;
    	case 8: strcat(komento," -capstyle projecting"); break;
    	}

    muste_plottcl(id, komento, FALSE);
	
	return(0);
	}




int muste_rectangle_plot(int id,double x1,double y1,double x2,double y2)
	{
	double xkerroin,ykerroin;
	xkerroin=(double)((double)muste_x_wsize/(double)muste_x_size);
	ykerroin=(double)((double)muste_y_wsize/(double)muste_y_size);	
	x1*=xkerroin; x2*=xkerroin;
	y1*=ykerroin; y2*=ykerroin;


//    sprintf(komento,"tkcreate(.muste$canvas[[%d]],\"rectangle\",%g,%g,%g,%g)",id,x1,y1,x2,y2);	
//    muste_evalr(komento);

    sprintf(komento,"create rectangle %g %g %g %g -fill %s -outline %s",x1,y1,x2,y2,muste_pencolor2,muste_pencolor);
    muste_plottcl(id, komento, FALSE);

	return(0);
	}

int muste_polygon_plot(int id,char *chain)
	{
	extern char muste_polychain[];
	extern int split(char *rivi,char **sana,int max);
	double xkerroin,ykerroin;
    int i,k,n;
    double pol_point_x[5005];
    double pol_point_y[5005];
    char *ss[10010];
    char buffer[512];
	
	xkerroin=(double)((double)muste_x_wsize/(double)muste_x_size);
	ykerroin=(double)((double)muste_y_wsize/(double)muste_y_size);

    i=split(chain,ss,10000);
    n=i/2;
    for (k=0; k<n; ++k)
        {
        pol_point_x[k]=(double)xkerroin*atof(ss[2*k]);
        pol_point_y[k]=(double)ykerroin*atof(ss[2*k+1]);
        }
    *muste_polychain=EOS;
    for (k=0; k<n; k++)
    	{
    	sprintf(buffer,"%g %g ",pol_point_x[k],pol_point_y[k]);
    	strcat(muste_polychain,buffer);
    	}

    sprintf(komento,"create polygon %s -fill %s -outline %s",muste_polychain,muste_pencolor2,muste_linecolor); // RS 15.1.2013 pencolor->linecolor
    muste_plottcl(id, komento, FALSE);

	return(0);
	}

	
int muste_ellipse_plot(int id,double x1,double y1,double x2,double y2)
	{
	double xkerroin,ykerroin;
	xkerroin=(double)((double)muste_x_wsize/(double)muste_x_size);
	ykerroin=(double)((double)muste_y_wsize/(double)muste_y_size);	
	x1*=xkerroin; x2*=xkerroin;
	y1*=ykerroin; y2*=ykerroin;


//    sprintf(komento,"tkcreate(.muste$canvas[[%d]],\"oval\",%g,%g,%g,%g)",id,x1,y1,x2,y2);	
//    muste_evalr(komento);
    
    sprintf(komento,"create oval %g %g %g %g -fill %s -outline %s",x1,y1,x2,y2,muste_pencolor2,muste_pencolor);
    muste_plottcl(id, komento, FALSE);

	return(0);
	}	

int muste_arc_plot(int id,double x1,double y1,double x2,double y2,double a1,double a2)
	{
	double xkerroin,ykerroin;
	xkerroin=(double)((double)muste_x_wsize/(double)muste_x_size);
	ykerroin=(double)((double)muste_y_wsize/(double)muste_y_size);	
	x1*=xkerroin; x2*=xkerroin;
	y1*=ykerroin; y2*=ykerroin;


//    sprintf(komento,"tkcreate(.muste$canvas[[%d]],\"oval\",%g,%g,%g,%g)",id,x1,y1,x2,y2);	
//    muste_evalr(komento);
    
    sprintf(komento,"create arc %g %g %g %g -extent %g -start %g -fill %s",x1,y1,x2,y2,a2,a1,muste_pencolor);
    muste_plottcl(id, komento, FALSE);

	return(0);
	}	
		
int muste_text_plot(int id,double x1,double y1,char *x)
	{
	double xkerroin,ykerroin;
	xkerroin=(double)((double)muste_x_wsize/(double)muste_x_size);
	ykerroin=(double)((double)muste_y_wsize/(double)muste_y_size);	
	x1*=xkerroin;
	y1*=ykerroin;

    char y[2*LLENGTH];
    int i,j;

/* RS Handle Tcl-special characters: 34="  36=$  91=[  92=\       */
    for (i=0, j=0; i<strlen(x); i++) {
    	if ((unsigned char)x[i]>31) // RS Handle only printable characters
       		{
//       		if (x[i]=='"') y[i]='`'; // RS Conversion
//       		else if (x[i]=='$') y[j]='S'; // RS Conversion
//       		else y[j++]=x[i];
//       		if (x[i]==34 || x[i]==36 || x[i]==91 || x[i]==92 ) y[j++]=92;
			if (x[i]==34 || x[i]==39 || x[i]==92) y[j++]=92;
      		y[j++]=x[i];
      		}
      	else y[j++]=' ';
    }
    if (y[j-1]=='"') y[j++]=' ';
    y[j]=EOS;

//Rprintf("\ntext: |%s|",y);

    muste_iconv(y,"","CP850");	
	
//	sprintf(komento,"tcl("create text %g %g -text \"%s\" -anchor \"nw\" -fill %s",x1,y1,y,muste_charcolor);
sprintf(komento,"tkcreate(.muste$canvas[[%d]],\"text\",%g,%g,text=\"%s\",anchor=\"nw\",fill=\"%s\",font=.muste$canvasfonts[[%d]][[%d]][[1]])",id,x1,y1,y,muste_charcolor,id,muste_canvasfonts[id]);
muste_evalr(komento);
	
//    sprintf(komento,"create text %g %g -text \"%s\" -anchor \"nw\" -fill %s",x1,y1,y,muste_charcolor);
//    muste_plottcl(id, komento, FALSE);
    
	return(0);
	}


int muste_create_plotwindow(int id, char *title)
	{
	extern int x_wsize,y_wsize,muste_x_size,muste_y_size;
	extern int x_whome,y_whome;

    sprintf(komento,"if (is.tkwin(.muste$plotwin[[%d]])) tkdestroy(.muste$plotwin[[%d]])",id,id);
    muste_evalr(komento);

    sprintf(komento,".muste$plotwin[[%d]] <- tktoplevel(.muste$ikkuna)",id);
    muste_evalr(komento);
    
    sprintf(komento,"tkwm.geometry(.muste$plotwin[[%d]],\"+%d+%d\")",id,x_whome,y_whome);
    muste_evalr(komento);  
   
 	muste_flushscreen();     
       
    sprintf(komento,".muste$canvas[[%d]] <- tkcanvas(.muste$plotwin[[%d]],width=%d,height=%d,background=\"white\")",id,id,x_wsize,y_wsize);
    muste_evalr(komento);
	
    sprintf(komento,"tkwm.title(.muste$plotwin[[%d]], \"%s\")",id,title);
    muste_evalr(komento);

    sprintf(komento,"tkgrid(.muste$canvas[[%d]])",id);
//    sprintf(komento,"tkpack(.muste$canvas[[%d]],\"-expand\",TRUE,\"-fill\",\"both\")",id);
    muste_evalr(komento);

 	muste_flushscreen(); 

	sprintf(komento,".muste$plotwinsize[[%d]] <- list()",id);
    muste_evalr(komento);
 
 	sprintf(komento,".muste$plotwinsize[[%d]][[%d]] <- 0.0",id,2);
    muste_evalr(komento); 

    sprintf(komento,".muste.canvas.windif(%d)",id);
    muste_evalr(komento);

    sprintf(komento,"tkbind(.muste$plotwin[[%d]],\"<Configure>\",muste:::.muste.canvas.scale)",id);
// Rprintf("\nkomento: %s",komento);
    muste_evalr(komento);

	sprintf(komento,".muste$canvasfonts[[%d]] <- list()",id);
    muste_evalr(komento);
 
 	sprintf(komento,".muste$canvasfonts[[%d]][[%d]] <- 0.0",id,MAXFONTS);
    muste_evalr(komento); 

 	sprintf(komento,".muste$canvasfonts[[%d]][[1]] <- list(tkfont.create(family=\"Courier\",size=%d,weight=\"bold\",slant=\"roman\"),14)",id,(int)((double)x_wsize/1000*14));
    muste_evalr(komento);

// Rprintf("\nx_wsize: %d,y_wsize: %d",x_wsize,y_wsize);

	muste_canvasfonts[id]=1;
    
    muste_old_plotid=0;
    
	return 1;
	}

void muste_createcanvasfont(int id)
	{
	extern int x_wsize,y_wsize,muste_x_size,muste_y_size;

	muste_canvasfonts[id]++;
 	sprintf(komento,".muste$canvasfonts[[%d]][[%d]] <- list(tkfont.create(family=\"%s\",size=%d,weight=\"%s\",slant=\"%s\"),%g)",id,muste_canvasfonts[id],muste_fontfamily,(int)((double)x_wsize/1000*muste_fontsize),muste_fontweight,muste_fontslant,muste_fontsize);
    muste_evalr(komento);	
	}

void muste_delete_plotwindow(int id)
	{
	muste_canvasfonts[id]=0;
	
    sprintf(komento,"if (is.tkwin(.muste$canvas[[%d]])) tkdestroy(.muste$canvas[[%d]])",id,id);
    muste_evalr(komento);

    sprintf(komento,"if (is.tkwin(.muste$plotwin[[%d]])) tkdestroy(.muste$plotwin[[%d]])",id,id);
    muste_evalr(komento);
    }


void sur_get_font(char *wname,int par[])
   {
// RS REM      SEXP avar=R_NilValue;

    sprintf(komento,".muste.getfontdim()");
    muste_evalr(komento);

//    avar = findVar(install(".muste$font.width"),R_GlobalEnv);
//    par[0]=INTEGER(avar)[0];
    par[0]=muste_get_R_int(".muste$font.width");

//    avar = findVar(install(".muste$font.height"),R_GlobalEnv);
//    par[1]=INTEGER(avar)[0];
    
    par[1]=muste_get_R_int(".muste$font.height");

        return;
   }
   
void sur_get_textwidth(char *teksti,int par[],int id)
   {
// RS REM      SEXP avar=R_NilValue;

    sprintf(komento,".muste.getfontdim(\"%s\",.muste$canvasfonts[[%d]][[%d]][[1]])",teksti,id,muste_canvasfonts[id]);
    muste_evalr(komento);

//    avar = findVar(install(".muste$font.width"),R_GlobalEnv);
//    par[0]=INTEGER(avar)[0];
    par[0]=muste_get_R_int(".muste$font.width");    

//    avar = findVar(install(".muste$font.height"),R_GlobalEnv);
//    par[1]=INTEGER(avar)[0];
    par[1]=muste_get_R_int(".muste$font.height");        

        return;
   }   



int sur_set_focus(char *wname)
   {
        sprintf(komento,"tkfocus(\"-force\",%s)",wname);
        muste_evalr(komento);
        sprintf(komento,"tkfocus(%s)",wname);
        muste_evalr(komento);
        return 1;
   }

int sur_main_window_show(char *wname,int status)
   {


// RS   if (status==0) sprintf(komento,"tcl(\"wm\",\"iconify\",%s)",wname);
//   else sprintf(komento,"tcl(\"wm\",\"deiconify\",%s)",wname);
//   sur_set_focus(wname);


   if (status==0)
      {
//      sprintf(komento,"tcl(\"wm\",\"focusmodel\",%s,\"active\")",wname);
//      muste_evalr(komento);

      sprintf(komento,"tkwm.iconify(%s)",wname);
      muste_evalr(komento);

      muste_window_minimized=TRUE;
      }
   else
      {
//      sprintf(komento,"tcl(\"wm\",\"focusmodel\",%s,\"passive\")",wname);
//      muste_evalr(komento);

//      sur_set_focus(wname);
//      sprintf(komento,"tkwm.deiconify(%s)",wname);


      if (muste_window_minimized)
         {

Rprintf("FIXME: sur_main_show_window KLUDGE (problems in Windows VISTA)\n"); // RS FIXME
         sprintf(komento,".muste.end()");
         muste_evalr(komento);

         sprintf(komento,".muste.init()");
         muste_evalr(komento);

         muste_window_existing=FALSE;
         muste_window_minimized=FALSE;
         }

      }
   return(1);
   }


void muste_resize(int conx, int cony)
   {
    sprintf(komento,".muste.resize(%d,%d)",conx,cony);
    muste_evalr(komento);
    muste_flushscreen();
   }



void muste_font(int size)
   {
   sprintf(komento,"tkfont.configure(.muste$font,size=%d)",size);  
   muste_evalr(komento);    
   }
   
void muste_choosefont()
   {
   sprintf(komento,".muste.choosefont()");
   muste_evalr(komento);   
   }


//   rivi.org<-tclvalue(tkget(txt,1.8,1.end))  R-tcl/tk
//    read_string(x,NULL,c3+8,rr+1,1); x[c3+8]=EOS;  / tut.c:st?

int read_string(char *s,char *s2,int len,int r,int c)  /* suoraan n?yt?lt? */
        {
// RS REM        SEXP avar;
        
        sprintf(komento,".muste$readbuffer<-tclvalue(tkget(.muste$txt,\"%d.%d\",\"%d.%d\"))",r,c,r,len);
//Rprintf("\n%s",komento);   
		muste_evalr(komento);
		
		
//		avar=R_NilValue;
//        avar=findVar(install(".muste$readbuffer"),R_GlobalEnv);        
//        strcpy(s,(char *)CHAR(STRING_ELT(avar,0)));
        
        muste_get_R_string(s,".muste$readbuffer",LLENGTH);
        
//Rprintf("\n%s",s);         
        
/* RS NYI
        DWORD n;
        int i;

        bufSize.X=c-1; bufSize.Y=r-1;

        i=ReadConsoleOutputCharacter(hStdOut,s,len,bufSize,&n);
// Rprintf("r=%d c=%d i=%d n=%d s=%.10s",r,c,i,n,s); getch();
//      i=ReadConsoleOutputAttribute(hStdOut,s2,len,bufSize,&n);
                            // korjattava: po. short *s2;
// attribuuttirivi? ei kuitenkaan koskaan k?ytet?!
*/
//Rprintf("\nFIXME: read_string not yet implemented!\n");

        muste_iconv(s,"CP850","");		

        return(-1);
        }

char survo_screenbuffer[500][100][2]; // RS 1.12.2015

void survo_ajax_screenbuffer() // RS 1.12.2015
    {
    int i,j,k;
    char *str;
    char apubuf[500];

    if (survo_webedit==0) return;    
    /* r3 = # number of edit lines on the screen */
    /* c3 = # of columns on the screen */
    /* a=char size=1 */
    /* b=shadow size=1 */
    /* c=delimiters= shadow|str| = 2 */
    /* max length = r3*c3*(a+b+c) */
    str=(char *)muste_malloc(r3*c3*5+10);
    if (str==NULL) return;
    str[0]='\0';
    for (i=1; i<=r3+2; i++)
        {
//        strcat(str,"32\t");
		sprintf(apubuf,"%d\t",i+49);
		strcat(str,apubuf);
        for (j=0; j<c3; j++)
            {
            apubuf[j]=survo_screenbuffer[j][i][0];
            if (apubuf[j]=='\0') apubuf[j]=' ';
            }

apubuf[j]='\t';
apubuf[j+1]='\0';
strcat(str,apubuf);             
sprintf(apubuf,"%d\t%d\t",0,i-1);
strcat(str,apubuf);
        }
sprintf(apubuf,"%d",r3+2);
strcat(str,apubuf);    
	muste_set_R_string(".muste$ajaxmsg",str);
	muste_free(str);
	}


/*
void survo_clear_screenbuffer() // RS 9.12.2015
    {
    int i,j;
    for (i=1; i<=r3+2; i++)
        {
        for (j=0; j<c3; j++)
                {
                survo_screenbuffer[j][i][1]=11;
                }
        }
    }
*/

// static char *survo_ajaxbuffer;
static char survo_ajaxbuffer[500*100*20];
static int survo_ajaxbuffer_count=0;

void survo_open_ajaxbuffer(int dispcall)
    {
  
  dispcall=muste_get_R_int(".muste$redraw");
  survo_webedit=muste_get_R_int(".muste$webedit");
  if (survo_webedit==0) return;
  
//    survo_ajaxbuffer=(char *)muste_malloc((r3+2)*c3*20+10);
//    if (survo_ajaxbuffer==NULL) return;    
    
    survo_ajaxbuffer[0]='\0';
    survo_ajaxbuffer_count=0;
   
    if (dispcall)
        {
        disp();
        muste_set_R_int(".muste$redraw",0);
        }
                 
    }

void survo_close_ajaxbuffer()
    {
    char apubuf[10];
  
    if (survo_webedit==0) return;
    sprintf(apubuf,"%d",survo_ajaxbuffer_count);
    strcat(survo_ajaxbuffer,apubuf);
        
	muste_set_R_string(".muste$ajaxmsg",survo_ajaxbuffer);
    sprintf(komento,"muste:::survo.sendajax()");
    muste_evalr(komento);
//	muste_free(survo_ajaxbuffer);
	}


int write_string(char *x, int len, int shadow, int row, int col)
    {
//    char y[2*LLENGTH];
    extern int muste_mac;
	char apubuf[500], lenbuf[500]; // RS 10.12.2015
    
    int i,j,k,pit,ylen,ypit,yext,yexto,transhadow;
	char *y,*yind,*yoldind;
	
	if (display_off) return(1);

	
	if (len<1) return(-1); // RS ADD 6.11.2012 

	if (survo_webedit==1) {
	for (i=0; i<len; i++) // RS 1.12.2015
	    {
	    survo_screenbuffer[col+i][row][0]=x[i];
	    survo_screenbuffer[col+i][row][1]=(char)shadow;
	    }

    for (i=0; i<len; i++) lenbuf[i]=x[i];
    lenbuf[i]='\0';
    muste_iconv(lenbuf,"UTF-8","CP850");
    sprintf(apubuf,"%d\t%s\t%d\t%d\t",(unsigned char)shadow,lenbuf,col-1,row-1); // RS 10.12.2015
    strcat(survo_ajaxbuffer,apubuf);
    survo_ajaxbuffer_count++;
	}

/*	Varsinainen tulostus! */
    y=(char *)malloc(3*len+2); 
    if (y==NULL) return(-1);
	

	i=0; j=0; pit=0; k=col-1;
	while (i<len)
		{
    	if ((unsigned char)x[i]>31 && (unsigned char)x[i]!=127) // RS Handle only printable characters
       		{
/* RS Handle Tcl-special characters: 34="  36=$  91=[  92=\   */
//sprintf(apubuf,"%c",x[i]);          		
       		if ((unsigned char)x[i]==34 || (unsigned char)x[i]==36 || (unsigned char)x[i]==91 || (unsigned char)x[i]==92) 
       		    {
       		    y[j++]=92;
       		    }
       		y[j++]=x[i]; pit++;
       		
       		if ((unsigned char)x[i]>127 || i==len-1)
       			{
       			if ((unsigned char)x[i]==213)
       				{
       				y[j-1]=EOS; 
//       				strcat(y,"\u20AC");
					muste_iconv(y,"","CP850");

    				sprintf(komento,"delete %d.%d %d.%d",row,k,row,k+pit);
    				Muste_EvalTcl(komento,TRUE);       				

					sprintf(komento,"insert %d.%d \"%s\\u20AC\" shadow%d",row,k,y,(unsigned char)shadow);
					Muste_EvalTcl(komento,TRUE); 
       				
//sprintf(komento,"tkinsert(.muste$txt,\"%d.%d\",\"%s\\u20AC\",\"shadow%d\")",row,k,y,(unsigned char) shadow);
//	tkinsert(.muste$txt,"1.0","koe\u20AC")
//muste_evalr(komento);

       				}
       			else
       				{
       				y[j]=EOS;
       				muste_iconv(y,"","CP850");     				    				

					sprintf(komento,"delete %d.%d %d.%d",row,k,row,k+pit);
					Muste_EvalTcl(komento,TRUE);

      				if (muste_mac && shadow==32 && shadow!=32) // RS 20.3.2013 /  29.1.2022 disabled
      				    {
      				    ylen=strlen(y); yoldind=y; yind=y; yext=0; yexto=0;
      				    while (yind<(y+ylen))
      				        {
                            if (*yind==' ') { transhadow=9999; while (*yind==' ') { *yind='x'; yind++; } }
                            else 
                                { 
                                transhadow=shadow; while (*yind!=' ' && *yind!=EOS) 
                                    { 
                                    if (*yind=='\\' && (*(yind+1)=='"' || *(yind+1)=='$' || *(yind+1)=='[' || *(yind+1)=='\\')) 
                                        { 
                                        yext++; yind++;
                                        } 
                                    yind++;
                                    } 
                                }
                            ypit=(int)(yind-yoldind);
                            if (transhadow==9999 && ypit<2 && *yind!=EOS) { *(yind-1)=' '; continue; }
                            strncpy(plotkomento,yoldind,ypit); plotkomento[ypit]=EOS;    
//                            if (transhadow==9999) plotkomento[ypit-1]=' ';                       
                            sprintf(komento,"insert %d.%d \"%s\" shadow%d",row,k+(int)(yoldind-y-yexto),plotkomento,transhadow);                            
                            Muste_EvalTcl(komento,TRUE);                           
                            yoldind=yind; yexto+=yext; yext=0;
					          }   				        
      				    }    			       				
                    else
                        {
                        sprintf(komento,"insert %d.%d \"%s\" shadow%d",row,k,y,(unsigned char)shadow);
                        Muste_EvalTcl(komento,TRUE);  
                        }     			
       				}
       			k+=pit; j=0; y[0]=EOS; pit=0;
       			}
       		}
       		else  { y[j++]=' '; pit++; } // RS ADD 8.10.2012	
		i++;
      	}

	free(y); // RS ADD 6.11.2012
    	


/*

    for (i=0, j=0; i<len; i++) {
    	if ((unsigned char)x[i]>31) // RS Handle only printable characters
       		{
       		if (x[i]==34 || x[i]==36 || x[i]==91 || x[i]==92 ) y[j++]=92;
      		y[j++]=x[i];
      		}
    }
    y[j]=EOS;


    muste_iconv(y,"","CP850");

// RS REM T?m? n?ytt?isi olevan turha:    sur_locate(row,col);

    sprintf(komento,"delete %d.%d %d.%d",row,col-1,row,col-1+len);
    Muste_EvalTcl(komento,TRUE);

    sprintf(komento,"insert %d.%d \"%s\" shadow%d",row,col-1,y,(unsigned char) shadow);
    Muste_EvalTcl(komento,TRUE);
*/

    return(len);
    }


int sur_erase(unsigned char color)
        {
        int i,row,col;
        char x[LLENGTH]; // RS CHA 256->LLENGTH

        for (i=0; i<LLENGTH; ++i) x[i]=' ';
        sur_cursor_position(&row,&col);
        write_string(x,c3+8+1-col,color,row,col);  
//        write_string(x,c3,color,r3+2,1);  
        return(1);
        }



int sur_scroll_up(int lines,int row1,int col1,int row2,int col2,int attr)
    {
    sprintf(komento,"delete %d.0 %d.0",row1-1,row1);
    Muste_EvalTcl(komento,TRUE);

    sprintf(komento,"insert %d.0 \" \n\"",row2);
    Muste_EvalTcl(komento,TRUE);

    return(1);
    }

int sur_scroll_down(int lines,int row1,int col1,int row2,int col2,int attr)
    {
    sprintf(komento,"delete %d.0 %d.0",row2+1,row2+2);
    Muste_EvalTcl(komento,TRUE);

    sprintf(komento,"insert %d.0 \" \n\"",row1);
    Muste_EvalTcl(komento,TRUE);

    return(1);
    }

/* Scroll direction: 6=up 7=down */
int sur_scroll(int r1,int r2,int n,int suunta)
        {
        if (display_off) return(1);

        if (suunta==7) { n=-n; sur_scroll_down(n,r1+1,0,r2,c3+8,119); }
        else sur_scroll_up(n,r1+2,0,r2+1,c3+8,119);
        return(1);
        }

int sur_cls(unsigned char color)
        {
        int i;
        char x[256];
        extern int r_soft, r3, c3; /* RS Mist? n?m? l?ytyv?t? Tuntuvat kuitenkin toimivan */

        if (!display_off)
            {
            for (i=0; i<256; ++i) x[i]=' ';
            for (i=1; i<=r3+2+r_soft; ++i)
                write_string(x,c3+8,color,i,1);
            }
        sur_locate(1,1);
        return(1);
        }

static int sur_print2(char *text,int lf)
        {
        int len,row,col,tila;
        char *p;

        p=text;
        while (*p)
            {
            len=strlen(p);
            sur_cursor_position(&row,&col);
            tila=c3+8+1-col;

            if (tila<=len)
                {
                write_string(p,tila,(unsigned char)sdisp,row,col);
                ++row; p+=tila;
                if (row>r3+1)
                    {

// Rprintf("sur_print scroll nlf: %d  %d",scroll_line+1,r3+1);

                                                /*   2  */
                    sur_scroll_up(1,scroll_line+1,1,r3+1,c3+8,(int)shadow_code[sdisp]);

                    sur_locate(r3+1,1);
                    }
                else
                    {
                    sur_locate(row,1);
                    }
                }
            else
                {
                write_string(p,len,(unsigned char)sdisp,row,col);
                sur_locate(row,col+len);
                p+=len;
                }
            }
        if (lf)
            {
            sur_cursor_position(&row,&col);
            ++row;
            if (row>r3+1)
                {
// Rprintf("sur_print scroll lf: %d  %d",scroll_line+1,r3+1);
                sur_scroll_up(1,scroll_line+1,1,r3+1,c3+8,(int)shadow_code[sdisp]);
                sur_locate(r3+1,1);
                }
            else
                {
                sur_locate(row,1);
                }
            }
        return(1);
        }

int sur_print(char *text)
        {
// RS REM        int i,m;
        char *p,*q;
        char x[LLENGTH];

        strncpy(x,text,LLENGTH); x[LLENGTH-1]='\0';
        p=x;
        while (*p)
            {
            q=strchr(p,'\n');
            if (q==NULL) { sur_print2(p,0); break; }
            *q='\0'; sur_print2(p,1);
            p=q+1;
            }
/* RS FIXME: n?pp?imist?n hallinta toistaiseksi pois t?st?
        if (space_break && kbhit())
            {
            i=getch();
            if (i==(int)' ')
                {
                sur_print2(" ",1); sur_print2("Continue operation (Y/N)? ",0);
                m=getch(); if (m==(int)'N' || m==(int)'n') exit(1);
                sur_print2("Y",1);
                }
            else ungetch(i);
            }
*/
muste_flushscreen();
        return(1);
        }


int sur_load_clipboard(char **clip)
    {
/*    
    HANDLE hClip;
    char *pClip;
    int len;
    if (!IsClipboardFormatAvailable(CF_TEXT))
        {
        if (etu==0)
            {
            sur_print("\nNo data in the clipboard!");
            WAIT;
            }
        return(-1);
        }
    OpenClipboard(NULL);
    hClip=GetClipboardData(CF_TEXT);
    len=GlobalSize(hClip);
    *clip=(char *)muste_malloc(len);
    pClip=GlobalLock(hClip);
    strcpy(*clip,pClip);
    GlobalUnlock(hClip);
    CloseClipboard();
*/  
    *clip=muste_get_clipboard();
    if (*clip==NULL) return(-1); // RS 4.12.2013
    return(1);
    }


