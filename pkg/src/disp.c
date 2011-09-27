#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

#include <string.h>
#include "survo.h"

#define MAXPLOTWINDOWS 300

extern int muste_evalr();
extern void muste_sleep();
int muste_iconv();

extern FILE *muste_fopen();
extern void *muste_malloc();
extern int muste_free();
extern int muste_fclose();

extern unsigned char *shadow_code;
extern int display_off;

extern int c3,c;
extern int r3,r;
extern int sdisp;
extern int scroll_line;
extern int space_break;

extern char muste_charcolor[];
extern char muste_pencolor[];

static char komento[3*LLENGTH]; /* 256 */
static char tclkomento[3*LLENGTH]; /* 256 */
static char plotkomento[3*LLENGTH]; /* 256 */


int muste_vconx=0;
int muste_vcony=0;

int muste_window_existing=FALSE;
int muste_window_minimized=FALSE;
char muste_window[64] = "";
char muste_plotwindow[64] = "";
char muste_plotcanvas[64] = "";
int muste_old_plotid=0;

char muste_window_name[]=".muste.ikkuna"; 

DL_FUNC RdotTcl = NULL;

int Muste_EvalTcl(char *komento, int ikkuna) 
{
    SEXP alist,aptr;

    if (RdotTcl == NULL) // RdotTcl = R_GetCCallable("tcltk", "dotTcl");
    RdotTcl = R_FindSymbol("dotTcl","tcltk",NULL);


//    if (strlen(muste_window)<2)
    if (muste_window_existing==FALSE) 
    {
    SEXP avar=R_NilValue;
    avar = findVar(install(".muste.window"),R_GlobalEnv);
    strcpy(muste_window,CHAR(STRING_ELT(avar,0)));
    strcat(muste_window," ");
    muste_window_existing=TRUE;
// Rprintf("Löytyi ikkuna: %s\n",muste_window);
    }

    if(!ikkuna) strcpy(tclkomento,komento);
    else 
    {
      strcpy(tclkomento,muste_window);
      strcat(tclkomento,komento);
    }
// Rprintf("Komento: %s\n",tclkomento);


    PROTECT(alist = allocList(2));
    aptr=alist;
    aptr=CDR(aptr); 
    SETCAR(aptr, mkString(tclkomento));
    RdotTcl(alist);
    UNPROTECT(1);
    return(1);
}

void muste_flushscreen() {
    sprintf(komento,"update idletasks");
    Muste_EvalTcl(komento,FALSE);
}

void muste_fixme(char *kommentti)
  {
  Rprintf(kommentti);
  }

int sur_locate(int row,int col)
{
    sprintf(komento,"mark set insert %d.%d",row,col-1);
    Muste_EvalTcl(komento,TRUE);
    return(1);
}

void cursor(unsigned int r,unsigned int c)
        {
        if (c>c3) c=c3;
        sur_locate(r+1,c+8);
        }

int sur_cursor_position(int *prow,int *pcol)
        {
    SEXP avar=R_NilValue;

    sprintf(komento,".muste.getcursor()");
    muste_evalr(komento);

    avar = findVar(install(".muste.cursor.row"),R_GlobalEnv);
    *prow=INTEGER(avar)[0];

    avar = findVar(install(".muste.cursor.col"),R_GlobalEnv);
    *pcol=1+INTEGER(avar)[0];

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

    if (!bVisible) dwSize=0;
    if (dwSize>100)
       { 
         dwSize-=100;
         sprintf(komento,"configure -insertwidth %d -insertbackground \"lightblue\"",dwSize);
         Muste_EvalTcl(komento,TRUE);
       }
    else
       {
         sprintf(komento,"configure -insertwidth %d -insertbackground \"black\"",dwSize);
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
        snprintf(komento,LLENGTH,"tkwm.title(.muste.ikkuna, \"%s\")",title);
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
    SEXP avar=R_NilValue;

    sprintf(komento,".muste.getscreendim()");
    muste_evalr(komento);

    avar = findVar(install(".muste.screen.width"),R_GlobalEnv);
    *sizex=INTEGER(avar)[0];

    avar = findVar(install(".muste.screen.height"),R_GlobalEnv);
    *sizey=INTEGER(avar)[0];

        return(1);
        }

void sur_get_window_rect(char *wname,int par[])
   {
      SEXP avar=R_NilValue;

    sprintf(komento,".muste.getwindowdim()");
    muste_evalr(komento);
    
    avar = findVar(install(".muste.window.topx"),R_GlobalEnv);
    par[0]=INTEGER(avar)[0];

    avar = findVar(install(".muste.window.topy"),R_GlobalEnv);
    par[1]=INTEGER(avar)[0];

    avar = findVar(install(".muste.window.bottomx"),R_GlobalEnv);
    par[2]=INTEGER(avar)[0];

    avar = findVar(install(".muste.window.bottomy"),R_GlobalEnv);
    par[3]=INTEGER(avar)[0];

        return;
   }

int muste_get_window_caption()
	{
	SEXP avar=R_NilValue;
	avar = findVar(install(".muste.window.caption"),R_GlobalEnv);
    return(INTEGER(avar)[0]);
	}

int muste_get_window_xframe()
	{
	SEXP avar=R_NilValue;
	avar = findVar(install(".muste.window.xframe"),R_GlobalEnv);
    return(INTEGER(avar)[0]);
	}
	
int muste_get_window_yframe()
	{
	SEXP avar=R_NilValue;
	avar = findVar(install(".muste.window.yframe"),R_GlobalEnv);
    return(INTEGER(avar)[0]);
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
	sprintf(komento,".muste.plotwin <- list()");
    muste_evalr(komento);
 
 	sprintf(komento,".muste.plotwin[[%d]] <- 0.0",MAXPLOTWINDOWS);
    muste_evalr(komento);

	sprintf(komento,".muste.canvas <- list()");
    muste_evalr(komento);
 
 	sprintf(komento,".muste.canvas[[%d]] <- 0.0",MAXPLOTWINDOWS);
    muste_evalr(komento);    
    
	}

void sur_pos_window(char *wname,int x,int y)
   {
   sprintf(komento,"tcl(\"wm\",\"geometry\",%s,\"+%d+%d\")",wname,x,y);
   muste_evalr(komento);
   }

int muste_focus_from_plotwin_to_editor(int id)
	{
	int opt[4];

    sprintf(komento,".muste.focus.editor()");
    muste_evalr(komento);

/*
    sprintf(komento,"tkfocus(.muste.txt)");
    muste_evalr(komento);

// RS Needed to get focus back to Muste editor in Mac
    sur_get_window_rect(muste_window_name,opt);
    sprintf(komento,"tkwm.withdraw(.muste.ikkuna)");
    muste_evalr(komento);
    sprintf(komento,"tkwm.deiconify(.muste.ikkuna)");
    muste_evalr(komento);  
    sur_pos_window(muste_window_name,opt[0],opt[1]);    
*/


/*	RS This causes problems in WIN7

    sprintf(komento,"tklower(.muste.plotwin[[%d]],.muste.ikkuna)",id);
    muste_evalr(komento);
*/


	return(0);
	
	}
	
int muste_plottcl(int id, char *komento, int win)
	{
	SEXP avar=R_NilValue;
	
	if (id!=muste_old_plotid);
		{
		sprintf(plotkomento,".muste.plotwinid<<-.Tk.ID(.muste.plotwin[[%d]])",id);
		muste_evalr(plotkomento);
	
		sprintf(plotkomento,".muste.canvasid<<-.Tk.ID(.muste.canvas[[%d]])",id);
		muste_evalr(plotkomento);
	
		avar = findVar(install(".muste.plotwinid"),R_GlobalEnv);
    	strcpy(muste_plotwindow,CHAR(STRING_ELT(avar,0)));
//    	strcat(muste_plotwindow," ");
    	
    	avar = findVar(install(".muste.canvasid"),R_GlobalEnv);
    	strcpy(muste_plotcanvas,CHAR(STRING_ELT(avar,0)));
//    	strcat(muste_plotcanvas," ");
    	
    	muste_old_plotid=id;
		}
	
	if (win) sprintf(plotkomento,"%s %s",muste_plotwindow,komento);
	else sprintf(plotkomento,"%s %s",muste_plotcanvas,komento);
//Rprintf("\n%s",plotkomento);

    Muste_EvalTcl(plotkomento,FALSE);
    return(1);
// Rprintf("Löytyi ikkuna: %s\n",muste_window);
    }	

int muste_window_style(int id,int style)
	{
	int tyyli;
	
	tyyli=0;
	if (style==0) tyyli=1;
	
	sprintf(komento,"tkwm.overrideredirect(.muste.plotwin[[%d]],%d)",id,tyyli);	
    muste_evalr(komento);
    
	sprintf(komento,"tkwm.withdraw(.muste.plotwin[[%d]])",id);
    muste_evalr(komento);

	sprintf(komento,"tkwm.deiconify(.muste.plotwin[[%d]])",id);
    muste_evalr(komento);
	
	return(0);
	}
	

extern int muste_x_wsize,muste_y_wsize,muste_x_size,muste_y_size;	
	
int muste_line_plot(int id,double x1,double y1,double x2,double y2)
	{

//    sprintf(komento,"tkcreate(.muste.canvas[[%d]],\"line\",%g,%g,%g,%g)",id,x1,y1,x2,y2);
//    muste_evalr(komento);
	double xkerroin,ykerroin;
	xkerroin=(double)((double)muste_x_wsize/(double)muste_x_size);
	ykerroin=(double)((double)muste_y_wsize/(double)muste_y_size);	
	x1*=xkerroin; x2*=xkerroin;
	y1*=ykerroin; y2*=ykerroin;


    sprintf(komento,"create line %g %g %g %g -fill %s",x1,y1,x2,y2,muste_pencolor);
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


//    sprintf(komento,"tkcreate(.muste.canvas[[%d]],\"rectangle\",%g,%g,%g,%g)",id,x1,y1,x2,y2);	
//    muste_evalr(komento);

    sprintf(komento,"create rectangle %g %g %g %g -fill %s",x1,y1,x2,y2,muste_pencolor);
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


//    sprintf(komento,"tkcreate(.muste.canvas[[%d]],\"oval\",%g,%g,%g,%g)",id,x1,y1,x2,y2);	
//    muste_evalr(komento);
    
    sprintf(komento,"create oval %g %g %g %g -fill %s",x1,y1,x2,y2,muste_pencolor);
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
       		if (x[i]=='_') y[j]=' '; // RS Conversion
       		if (x[i]==';') y[j]=','; // RS Conversion
       		if (x[i]==34 || x[i]==36 || x[i]==91 || x[i]==92 ) y[j++]=92;
      		y[j++]=x[i];
      		}
    }
    y[j]=EOS;

    muste_iconv(y,"","CP850");	
	
	
    sprintf(komento,"create text %g %g -text \"%s\" -anchor \"nw\" -fill %s",x1,y1,y,muste_charcolor);
    muste_plottcl(id, komento, FALSE);
    
	return(0);
	}


int muste_create_plotwindow(int id, char *title)
	{
	extern int x_wsize,y_wsize;
	extern int x_whome,y_whome;

    sprintf(komento,"if (is.tkwin(.muste.plotwin[[%d]])) tkdestroy(.muste.plotwin[[%d]])",id,id);
    muste_evalr(komento);

    sprintf(komento,".muste.plotwin[[%d]] <- tktoplevel(.muste.ikkuna)",id);
    muste_evalr(komento);
    
    sprintf(komento,"tkwm.geometry(.muste.plotwin[[%d]],\"+%d+%d\")",id,x_whome,y_whome);
    muste_evalr(komento);    
       
    sprintf(komento,".muste.canvas[[%d]] <- tkcanvas(.muste.plotwin[[%d]],width=%d,height=%d,background=\"white\")",id,id,x_wsize,y_wsize);
    muste_evalr(komento);
	
    sprintf(komento,"tkwm.title(.muste.plotwin[[%d]], \"%s\")",id,title);
    muste_evalr(komento);

    sprintf(komento,"tkgrid(.muste.canvas[[%d]])",id);
//    sprintf(komento,"tkpack(.muste.canvas[[%d]],\"-expand\",TRUE,\"-fill\",\"both\")",id);
    muste_evalr(komento);

    sprintf(komento,"tkbind(.muste.plotwin[[%d]],\"<Configure>\",.muste.canvas.scale)",id);
    muste_evalr(komento);
    
    muste_old_plotid=0;
    
	return 1;
	}


void muste_delete_plotwindow(int id)
	{
    sprintf(komento,"if (is.tkwin(.muste.canvas[[%d]])) tkdestroy(.muste.canvas[[%d]])",id,id);
    muste_evalr(komento);

    sprintf(komento,"if (is.tkwin(.muste.plotwin[[%d]])) tkdestroy(.muste.plotwin[[%d]])",id,id);
    muste_evalr(komento);
    }


void sur_get_font(char *wname,int par[])
   {
      SEXP avar=R_NilValue;

    sprintf(komento,".muste.getfontdim()");
    muste_evalr(komento);

    avar = findVar(install(".muste.font.width"),R_GlobalEnv);
    par[0]=INTEGER(avar)[0];

    avar = findVar(install(".muste.font.height"),R_GlobalEnv);
    par[1]=INTEGER(avar)[0];

        return;
   }
   
void sur_get_textwidth(char *teksti,int par[])
   {
      SEXP avar=R_NilValue;

muste_fixme("\nFIXME: sur_get_textwidth()");
    sprintf(komento,".muste.getfontdim(\"%s\",\"TkDefaultFont\")",teksti);
    muste_evalr(komento);

    avar = findVar(install(".muste.font.width"),R_GlobalEnv);
    par[0]=INTEGER(avar)[0];

    avar = findVar(install(".muste.font.height"),R_GlobalEnv);
    par[1]=INTEGER(avar)[0];

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
   sprintf(komento,"tkfont.configure(.muste.font,size=%d)",size);
   muste_evalr(komento);
   }
   
void muste_choosefont()
   {
   sprintf(komento,".muste.choosefont()");
   muste_evalr(komento);   
   }


//   rivi.org<-tclvalue(tkget(txt,1.8,1.end))  R-tcl/tk
//    read_string(x,NULL,c3+8,rr+1,1); x[c3+8]=EOS;  / tut.c:stä

int read_string(char *s,char *s2,int len,int r,int c)  /* suoraan näytöltä */
        {
        SEXP avar;
        
        sprintf(komento,".muste.readbuffer<<-tclvalue(tkget(.muste.txt,\"%d.%d\",\"%d.%d\"))",r,c,r,len);
//Rprintf("\n%s",komento);   
		muste_evalr(komento);
		
		
		avar=R_NilValue;
        avar=findVar(install(".muste.readbuffer"),R_GlobalEnv);
        
        strcpy(s,(char *)CHAR(STRING_ELT(avar,0)));
        
//Rprintf("\n%s",s);         
        
/* RS NYI
        DWORD n;
        int i;

        bufSize.X=c-1; bufSize.Y=r-1;

        i=ReadConsoleOutputCharacter(hStdOut,s,len,bufSize,&n);
// printf("r=%d c=%d i=%d n=%d s=%.10s",r,c,i,n,s); getch();
//      i=ReadConsoleOutputAttribute(hStdOut,s2,len,bufSize,&n);
                            // korjattava: po. short *s2;
// attribuuttiriviä ei kuitenkaan koskaan käytetä!
*/
//Rprintf("\nFIXME: read_string not yet implemented!\n");
        return(-1);
        }


int write_string(char *x, int len, char shadow, int row, int col)
    {
    if (display_off) return(1);

    char y[2*LLENGTH];
    int i,j;

/* RS Handle Tcl-special characters: 34="  36=$  91=[  92=\       */
    for (i=0, j=0; i<len; i++) {
    	if ((unsigned char)x[i]>31) // RS Handle only printable characters
       		{
       		if (x[i]==34 || x[i]==36 || x[i]==91 || x[i]==92 ) y[j++]=92;
      		y[j++]=x[i];
      		}
    }
    y[j]=EOS;

    muste_iconv(y,"","CP850");

// RS REM Tämä näyttäisi olevan turha:    sur_locate(row,col);

    sprintf(komento,"delete %d.%d %d.%d",row,col-1,row,col-1+len);
    Muste_EvalTcl(komento,TRUE);

    sprintf(komento,"insert %d.%d \"%s\" shadow%d",row,col-1,y,(unsigned char) shadow);
    Muste_EvalTcl(komento,TRUE);

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
        extern int r_soft, r3, c3; /* RS Mistä nämä löytyvät? Tuntuvat kuitenkin toimivan */

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
/* RS FIXME: näppäimistön hallinta toistaiseksi pois tästä
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

void muste_copy_to_clipboard(char *x)
    {
    int len;
    char *y,*clip;
    
    len=strlen(x)+1;
/* RS REM korvattu    
    w_codes_load(1);

    for (i=0; i<len-1; ++i)
        (unsigned char)clip[i]=code[(unsigned char)clip[i]];

*/
    int i,j;

    y=malloc(3*len); // RS ei muste_malloc, koska putsataan heti pois
    clip=malloc(3*len);

/* RS Handle Tcl-special characters: 34="  36=$  91=[  92=\       */
    for (i=0, j=0; i<len; i++) {
       		if (x[i]==34 || x[i]==36 || x[i]==91 || x[i]==92 ) y[j++]=92;
      		y[j++]=x[i];
    }
    y[j]=EOS;

    muste_iconv(y,"","CP850");

    sprintf(komento,"clipboard clear");
    Muste_EvalTcl(komento,FALSE);

    sprintf(clip,"clipboard append \"%s\"",y);
    Muste_EvalTcl(clip,FALSE);

   free(clip);
   free(y); // RS ADD

/* RS CHA
    p=clip;
    hGlob=GlobalAlloc(GHND,len);
    pGlob=GlobalLock(hGlob);
    for (i=0; i<len; ++i)
        *pGlob++=*p++;
    GlobalUnlock(hGlob);
    OpenClipboard(NULL);
    EmptyClipboard();
    SetClipboardData(CF_TEXT,hGlob);
    CloseClipboard();
*/  
    return;
    }
    

char *muste_get_clipboard()
    {

/* RS NYI
    if (!IsClipboardFormatAvailable(CF_TEXT))
        {
        if (etu==0)
            {
            sur_print("\nNo data in the clipboard!");
            WAIT;
            }
        return(1);
        }
*/


    SEXP avar;
    char *clip; 

    sprintf(komento,".muste.getclipboard()");
    muste_evalr(komento);

    avar=R_NilValue;
    avar=findVar(install(".muste.clipboard"),R_GlobalEnv);

    clip=(char *)CHAR(STRING_ELT(avar,0));

    muste_iconv(clip,"CP850","");
       
    return(clip);

/* RS REM
    OpenClipboard(NULL);
    hClip=GetClipboardData(CF_TEXT);
    len=GlobalSize(hClip);
    clip=muste_malloc(len);
    pClip=GlobalLock(hClip);
    strcpy(clip,pClip);
    GlobalUnlock(hClip);
    CloseClipboard();
*/
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
    return(1);
    }


int muste_evalclipboard()
	{
	char *mp;
	
	muste_sleep(100);
    mp=muste_get_clipboard();
    muste_sleep(100);    
    sprintf(komento,"source(\"clipboard\",echo=TRUE,print.eval=TRUE)");         
    muste_evalr(komento);
    return(1);
    }
    
int muste_evalsource(char *sfile)
	{
	char x[LLENGTH], out[LNAME];
// RS REM	FILE *ifile;
	extern char *etmpd;
	
	muste_sleep(100);
	     
	strcpy(x,sfile);
	strcpy(out,etmpd); strcat(out,x);
	
    sprintf(komento,"source(\"%s\",echo=TRUE,print.eval=TRUE)",out);         
    muste_evalr(komento);
    return(1);
    }

int sur_play_sound(char *nimi)
 {
 muste_fixme("\nFIXME: sur_play_sound not yet implemented!");
 return(0);
 }

