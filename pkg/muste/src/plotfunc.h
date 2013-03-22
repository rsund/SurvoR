

static int p_inquiry()
        {
        int j; // RS REM ,i;
        char x[LLENGTH];
// RS REM        char y[LLENGTH];

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

/*
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

    muste_fclose(kirjoitin);

    return(1);
    }
*/

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
                    return(-1); // RS ADD
//                  Rprintf("\n] missing in %s!\n",x); WAIT; return(-1);
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
//          Rprintf("\nSyntax error in %s\n",x);
            return(-1);
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
        muste_fclose(codes);
        }

/*
static int p_empty(char *s)
        {
        while (*s) { if (*s!=' ') return(0); ++s; }
        return(1);
        }
*/

// Muutettu 13.10.2002 \; -> ;  ja \_ -> _
static int pilkku_muunto(char *s) /* puolipiste pilkuksi */
        {
        char t[LLENGTH];
        char *p,*q;
        extern int alaviivat_pois;

        if (!alaviivat_pois) return(1);
// Rprintf("\ns=%s|",s);
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
// Rprintf("\nt=%s|",s); getch();
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
          if (i<0) strcpy(x,"[COLORS]"); else strcpy(x,spb[i]);
//            if (i>=0) strcpy(x,spb[i]);
                i=muunna(x,y); if (i<0) { p_end(); return(-1); }              
                
            }
        a=xdiv1+xdiv2+xdiv3; xdiv1=xdiv1/a; xdiv2=xdiv2/a; xdiv3=xdiv3/a;
        a=ydiv1+ydiv2+ydiv3; ydiv1=ydiv1/a; ydiv2=ydiv2/a; ydiv3=ydiv3/a;
        xx=(int)(x_home+xdiv1*x_size);
        yy=(int)(y_home+ydiv1*y_size);
        x_kuva=(int)(xdiv2*x_size);
        y_kuva=(int)(ydiv2*y_size);



/*      Rprintf("\nxx=%d yy=%d",xx,yy);
        Rprintf("\nkuva-ala: %d %d",x_kuva,y_kuva);
        Rprintf("\nxdiv: %g %g %g",xdiv1,xdiv2,xdiv3);
        Rprintf("\nydiv: %g %g %g",ydiv1,ydiv2,ydiv3);
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
     p_text2((unsigned char *)p,(unsigned char *)ps,x_home+(int)kirjainlev,y_home+y_size-(int)(a*kirjainkork),1);
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

//          if (nt<3) { Rprintf("\nCoordinates missing after text %s\n",spb[i]);
//                      WAIT; return(-1);
//                    }

            if (nt<3)
                {

      sprintf(sbuf,"Coordinates missing after text %s",spb[i]);
                p_error(sbuf);
                return(-1);

                }



            p_text2((unsigned char *)p,(unsigned char *)ps,x_home+arit_atoi(sana[1]),y_home+arit_atoi(sana[2]),1);
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

// for (i=0; i<nt; ++i) Rprintf("\n%s ",sana[i]); getch();

        if (nt<5)
            {
            sprintf(sbuf,"Too few parameters in #LINES text of specification %s\n",
                                                              tnimi);
            p_error(sbuf);
            return(-1);
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
            p_text2((unsigned char *)(x+1),(unsigned char *)p,reuna,taso,1);
            taso-=vali;
            }
        alaviivat_pois=1;
        return(1);
        }

static int frames()
        {
        int h,i,k;
        char x[LLENGTH];
        char *p; // , *ps;
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
//            if (spshad[i]==NULL) ps=NULL;
//            else { k=p-y; ps=spshad[i]+k; }
            nt=split(p,sana,5);
//          if (nt<4) { Rprintf("\nCoordinates missing in frame %s",spb[i]);
//                      Rprintf("\nSyntax: name=x,y,width,height[,shading]\n");
//                      WAIT; return(-1);
//                    }
            if (nt<4)
                {
sprintf(sbuf,"Coords missing in frame %s | Syntax: name=x,y,width,height[,shading]"
                   ,spb[i]);
                p_error(sbuf);
                return(-1); // RS ADD
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
        int h,i; // RS REM ,k;
        char x[LLENGTH];
        int ntext;
        char *tnimi[MAXTEXTS];
        char y[LLENGTH];
        int nt;
        char *sana[5];
// RS REM        char pkoodi[LLENGTH], kopio[LLENGTH];

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
//          if (nt<3) { Rprintf("\nIncomplete fill point %s",spb[i]);
//                      Rprintf("\nSyntax: name=x,y,shading\n");
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
        int h,i; // RS REM ,k;
        char x[LLENGTH];
        int ntext;
        char *tnimi[MAXTEXTS];
        char y[LLENGTH];
// RS REM        int nt;
// RS REM        char *sana[5];
// RS REM        char pkoodi[LLENGTH], kopio[LLENGTH];
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
// RS REM        double min,max;
        int x1;
        char *q;

/*  Rprintf("\nxscale: n=%d\n",n);
    for (i=0; i<n; ++i) Rprintf("%g ",value[i]); getch();
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
            p_text((unsigned char *)q,x1,y0-2*tikki-(int)(1.2*kirjainkork),1);
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
// RS REM        double min,max;
        int y1;
        int dmax;
        char x[LLENGTH],*osa[2];
        double klev;
        extern double ycharwidth;

/*  Rprintf("\nyscale: n=%d\n",n);
    for (i=0; i<n; ++i) Rprintf("%g ",value[i]); getch();
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
            p_text((unsigned char *)q,x1,y1-(int)(kirjainkork/2.0),1);
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
        p_text2((unsigned char *)p,(unsigned char *)ps,xx+x_kuva-(int)(strlen(p)*kirjainlev),
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
        p_text2((unsigned char *)p,(unsigned char *)ps,xx,yy+y_kuva+2*tikki,1);

        return(1);
        }

static void sp_virhe(char *a,char *b)
        {
//      Rprintf("\nError in specification\n   %s=%s",a,b);
        sprintf(sbuf,"Error in specification %s=%s",a,b);
        p_error2(sbuf);
        return; // RS ADD
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
        p_text((unsigned char *)p,(int)(x_home+kirjainlev),ytaso,1);      /* s -22.1.92 */
        xs=x_home+kirjainlev*(strlen(p)+2);              /* s */
        for (i=0; i<em2-1; ++i)
            {
            int y_apu;

            p_text((unsigned char *)(xname[ev[i+1]]),xs,ytaso,1);
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
// RS REM        char text[LLENGTH];
        int ykoko;
        int x_box,y_box;
        int x_text,y_text;
        int ix,iy;

        i=p_pen(); if (i<0) return(-1); // kokeilu 20.2.2012
        i=p_linetype(); if (i<0) return(-1); // kokeilu 20.2.2012
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
            p_text((unsigned char *)(xname[ev[i+1]]),ix+x_text,iy+y_text,1);
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
        int i,j,k,erstat;
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
// Rprintf("\nem=%d|",em); getch();
        j=0;
        for (l=dat.l1; l<=dat.l2; ++l)
            {
            if (unsuitable(&dat,l)) continue;
            if (grouping_var>=0)
                {
                erstat=data_alpha_load(&dat,l,grouping_var,x);
                if (erstat<0) return(-1);
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
                erstat=data_alpha_load(&dat,l,namevar,x);
                if (erstat<0) return(-1);
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
                   return(-1);
                   }
                if (y==MISSING8)
                    {
//                  Rprintf("\nValue of %.8s missing in obs.#%ld!\n",
//                              dat.varname[dat.v[i]],l);
                    sprintf(sbuf,"Value of %.8s missing in obs.#%ld!",
                                dat.varname[dat.v[i]],l);
                    p_error(sbuf);

                    return(-1);
                    }
                xmat[j*em+i]=y;
                if (pyramid && xtype[i]=='A') xmat[j*em+i]=-y; // 18.10.2005
                if (devvar1>=0)  // 1.5.2004
                    {
                    k=data_load(&dat,l,devvar1,&y);
                    if (k<0) return(-1); // RS 17.1.2013
                    devmat[2*j]=y;
                    k=data_load(&dat,l,devvar2,&y);
                    if (k<0) return(-1); // RS 17.1.2013
                    devmat[2*j+1]=y;
                    }
                }
            ++j;
            if ((j+1)*em>MAXDATA)
                {
//              Rprintf("\nToo many data values (>%d)\n",MAXDATA);
                sprintf(sbuf,"Too many data values (>%d)",MAXDATA);
                p_error(sbuf);

                return(-1);
                }
            if (j+1>NOBS)
                {
//              Rprintf("\nToo many observations (>%d)\n",NOBS);
                sprintf(sbuf,"Too many observations (>%d)",NOBS);
                p_error(sbuf);

                return(-1);
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
//        data_close(&dat); // RS REM 1.10.2012
        return(1);
        }


static int dataopen(char data[])
        {
        int   i,j,k;
        char xx[LNAME];

//      i=data_read_open(data,&dat); if (i<0) return(0);
        i=data_read_open(data,&dat); if (i<0) { p_error(sbuf); return(-1);}

//      i=mask(&dat); if (i<0) return(0);
        i=mask(&dat); if (i<0) { p_error(sbuf); return(-1); }

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
                return(-1); // onko tarpeen?
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
//          Rprintf("\nToo many active variables (>%d)!\n",NVAR);
            sprintf(sbuf,"Too many active variables (>%d)!",NVAR);
            p_error(sbuf);

            return(0);
            }
    /* 1.nimipaikka tyhjÑ */
        for (i=1; i<em; ++i)
            {
            int len;
            xname[i]=dat.varname[dat.v[i-1]];
// Rprintf("\ntype=%s|",dat.vartype[dat.v[i-1]]); getch();
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
//              Rprintf("\nIncorrect GRID specification!\n");
                p_error("Incorrect GRID specification!");
                return(-1);
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
//          Rprintf("\nIncorrect TICK specification!\n");
            p_error("Incorrect TICK specification!");
            return(-1);
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

/*  pgr2.c 26.11.1985/SM (21.1.1987)
    PLOT       GRID TICK   curves etc.
  esim. GRID=X  =XY  =X,Y  =X,0.2  =0.2,Y  =0.1,0.2  =0.1  =0,0.2

*/

static int xgrid()
        {
//        extern double arit_atof();
//        extern double grid_alku();
        int i,k;
        char x[LLENGTH], *osa[2];
        char *p;
        double a, step;

        i=spfind("GRID"); if (i<0) return(1);
        k=p_linetype(); if (k<0) return(-1);
        strcpy(x,spb[i]);
        k=control_code(x,&p,1);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        k=split(p,osa,2);
        if (*osa[0]=='Y') return(1);
        if (strchr(osa[0],'X')!=NULL)
            {
            for (i=1; i<xscalen-1; ++i)
                {
                int ix=xx+(xmu(xscaleval[i])-xmumin)/(xmumax-xmumin)*x_kuva;
                p_line2(ix,yy,ix,yy+y_kuva,1);
                }
            }
        else
            {
            step=arit_atof(osa[0]);
            if (step<=0.0) return(1);
            a=grid_alku(xmin,xmax,step);
            if (fabs(a-xmin)<step/STEPOSA) a+=step;
            while (a<xmax-step/STEPOSA)
                {
                int ix;
                if (fabs(a)<1e-10) a=0.0;
                ix=xx+(xmu(a)-xmumin)/(xmumax-xmumin)*x_kuva;
                p_line2(ix,yy,ix,yy+y_kuva,1);
                a+=step;
                }
            }
        return(1);
        }

static double grid_alku(double min,double max,double step)
        {
//        extern double paras_arvo();
        double x;
        int i;

        x=paras_arvo(min,max);
        i=(int)floor((x-min)/step);
        x-=i*step;
        return(x);
        }

static int ygrid()
        {
        extern double arit_atof();
        extern double grid_alku();
        int i,k;
        char x[LLENGTH], *osa[2];
        char *p;
        double a, step;

        i=spfind("GRID"); if (i<0) return(1);
        k=p_linetype(); if (k<0) return(-1);
        strcpy(x,spb[i]);
        k=control_code(x,&p,1);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        k=split(p,osa,2);
        if (strchr(osa[0],'Y')!=NULL || (k>1 && strchr(osa[1],'Y')!=NULL) )
            {
            for (i=1; i<yscalen-1; ++i)
                {
                int iy=yy+(ymu(yscaleval[i])-ymumin)/(ymumax-ymumin)*y_kuva;
                p_line2(xx,iy,xx+x_kuva,iy,1);
                }
            }
        else
            {
            if (k<2) return(1);
            step=arit_atof(osa[1]);
            if (step<=0.0) return(1);
            a=grid_alku(ymin,ymax,step);
            if (fabs(a-ymin)<step/STEPOSA) a+=step;
            while (a<ymax-step/STEPOSA)
                {
                int iy;
                if (fabs(a)<1e-10) a=0.0;
                iy=yy+(ymu(a)-ymumin)/(ymumax-ymumin)*y_kuva;
                p_line2(xx,iy,xx+x_kuva,iy,1);
                a+=step;
                }
            }
        return(1);
        }

static int xtick(int type) /* 1=TICK 2=TICK2 */
        {
//        extern double arit_atof();
//        extern double grid_alku();
        int i,k;
        char x[LLENGTH], *osa[2];
        char *p;
        double a, step;
        int neg; // 30.7.2010

        if (type==1)
            { i=spfind("TICK"); if (i<0) return(1); }
        else
            {
            i=spfind("TICK2"); if (i<0) return(1);
            if (strcmp(spb[i],"TICK")==0)
                { i=spfind("TICK"); if (i<0) return(1); }
            }
        k=p_linetype(); if (k<0) return(-1);
        strcpy(x,spb[i]);
        k=control_code(x,&p,1);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        k=split(p,osa,2);
        step=arit_atof(osa[0]);
        neg=1; if (step<0.0) { neg=-1; step=-step; }
        if (step<=0.0) return(1);
        a=grid_alku(xmin,xmax,step);
        if (fabs(a-xmin)<step/STEPOSA) a+=step;
        while (a<xmax-step/STEPOSA)
            {
            int ix;
            if (fabs(a)<1e-10) a=0.0;
            ix=xx+(xmu(a)-xmumin)/(xmumax-xmumin)*x_kuva;
            if (type==1)
                p_line2(ix,yy+scalemove_y,ix,yy+scalemove_y+neg*tikki,1);    /* 12.2.1993 */
            else
                p_line2(ix,yy+y_kuva,ix,yy+y_kuva-neg*tikki,1);
            a+=step;
            }
        return(1);
        }

static int ytick(int type) /* 1=TICK 2=TICK2 */
        {
//        extern double arit_atof();
//        extern double grid_alku();
        int i,k;
        char x[LLENGTH], *osa[2];
        char *p;
        double a, step;
        int neg; // 30.7.2010

        if (type==1)
            { i=spfind("TICK"); if (i<0) return(1); }
        else
            {
            i=spfind("TICK2"); if (i<0) return(1);
            if (strcmp(spb[i],"TICK")==0)
                { i=spfind("TICK"); if (i<0) return(1); }
            }
        k=p_linetype(); if (k<0) return(-1);
        strcpy(x,spb[i]);
        k=control_code(x,&p,1);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        k=split(p,osa,2);
        if (k<2) return(1);
        step=arit_atof(osa[1]);
        neg=1; if (step<0.0) { neg=-1; step=-step; }
        if (step<=0.0) return(1);
        a=grid_alku(ymin,ymax,step);
        if (fabs(a-ymin)<step/STEPOSA) a+=step;
        while (a<ymax-step/STEPOSA)
            {
            int iy;
            if (fabs(a)<1e-10) a=0.0;
            iy=yy+(ymu(a)-ymumin)/(ymumax-ymumin)*y_kuva;
            if (type==1)
                p_line2(xx+scalemove_x,iy,xx+scalemove_x+neg*tikki,iy,1);  /* 12.2.1993 */
            else
                p_line2(xx+x_kuva,iy,xx+x_kuva-neg*tikki,iy,1);
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
//              Rprintf("\n) is missing in %s\n",s);
                sprintf(sbuf,") is missing in %s\n",s);
                p_error(sbuf);
                return(-1);
                }
            *r=EOS; ++r;
            dp=arit_atof(p); dstep=arit_atof(q); dr=arit_atof(r);
            if (dstep<=0.0)
                {
//              Rprintf("\nNegative step not allowed in %s\n",s);
                sprintf(sbuf,"Negative step not allowed in %s",s);
                p_error(sbuf);
                return(-1);
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
//      Rprintf("\nError in scale notation %s: More than %d values",
//                                         s,MAXSCALELIST);

        sprintf(sbuf,"Error in scale notation %s: More than %d values",
                                           s,MAXSCALELIST);
        p_error(sbuf);
//      Rprintf("\n or text exceeding space available!\n");
        return;
        }

static int autom_scale(char *x,double min,double max,int npos)
        {
//        extern double paras_arvo();
// RS REM        int i;
        double paras, askel, a;
        int n;
        char sana[32];

        if (min<-1e20 || max>1e20)
            {
//          Rprintf("\nToo large values (>1e20) for automatic scaling!\n");
            p_error("Too large values (>1e20) for automatic scaling!");
            return(-1);
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

static int xyscale2(char *suunta) /* "X" tai "Y" */
        {
//        extern double arit_atof();
        int i,k;
        char x[LLENGTH];
        char *p,*q;
        char snimi[16];

        i=p_pen(); if (i<0) return(-1);
        i=p_linetype(); if (i<0) return(-1);  /* merkintÑviivoihin */
                   /* haetaan joko XSCALE2 tai YSCALE2 */
        strcpy(snimi,suunta); strcat(snimi,"SCALE2");
        i=spfind(snimi); if (i<0) return(1);
        strcpy(x,spb[i]);
        k=control_code(x,&p,0);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        if (*p=='*')  /* ohitetaan mahdollinen muunnos */
            {
            ++p;
            q=p;
            while (*q && *q!=',') ++q;
            *q=EOS;
            p=q+1;
            }

        if (*suunta=='X' && strcmp(p,"XSCALE")==0)
            {
            control_code_scale("XSCALE");
            zscalen=xscalen;
            for (i=0; i<zscalen; ++i)
                {
                zscaleval[i]=xscaleval[i];
                zscal[i]=xscal[i];
                }
            }
        else if (*suunta=='Y' && strcmp(p,"YSCALE")==0)
            {
            control_code_scale("YSCALE");
            zscalen=yscalen;
            for (i=0; i<yscalen; ++i)
                {
                zscaleval[i]=yscaleval[i];
                zscal[i]=yscal[i];
                }
            }
        else
            {                                       /* 20.5.92 */
            k=skaala_arvot(p,zscales,zscal,&zscalen,SCALESPACE);
            if (k<0) return(-1);
            for (i=0; i<zscalen; ++i)
                {
                q=zscal[i];
                p=strchr(zscal[i],':'); if (p!=NULL) { zscal[i]=p+1; *p=EOS; }
                zscaleval[i]=arit_atof(q);
                }
            }
        if (*suunta=='X')
            {
            if (!aika)
                plot_xscale2(zscalen,zscaleval,zscal,xx,yy+y_kuva,x_kuva);
            else
                plot_tscale(zscalen,zscaleval,zscal,xx,yy+y_kuva,x_kuva,2);
            }
        else
            plot_yscale2(zscalen,zscaleval,zscal,xx+x_kuva,yy,y_kuva);
        return(1);
        }

static void control_code_scale(char *s)
        {
        int i;
        char x[LLENGTH];
        char *p;

        i=spfind(s); if (i<0) return;
        strcpy(x,spb[i]);
        control_code(x,&p,0);   /* vain tulostusasun valintaan */
        }

static void plot_xscale2(
int n,              /* arvojen lkm */
double value[],     /* skaala-arvot */
char *label[],      /* skaalanimet */
int x0,
int y0,             /* alkupiste */
int pituus         /* asteikon pituus */
)
        {
        int i;
// RS REM        double min,max;
        int x1;
        char *q;

        if (frametype==0 || frametype==3) return;

        find_tickturn();
        for (i=0; i<n; ++i)
            {
            x1=x0+(int)((xmu(value[i])-xmumin)/(xmumax-xmumin)*pituus);
            q=label[i];
            if (*q=='?') ++q; else p_line2(x1,y0,x1,y0+2*tickturn*tikki,1);
            p_text((unsigned char *)q,x1,y0+2*tikki+(int)(0.2*kirjainkork),1);
            }
        }

static void plot_yscale2(
int n,              /* arvojen lkm */
double value[],     /* skaala-arvot */
char *label[],      /* skaalanimet */
int x0,
int y0,             /* alkupiste */
int pituus         /* asteikon pituus */
)
        {
        int i;
// RS REM        double min,max;
        int y1;
        int dmax, lmax;

        if (frametype==0 || frametype==3) return; /* 30.7.1997 */
        dmax=lmax=0;
        for (i=0; i<n; ++i)
            {
            int k,len;
            char *p;

            p=strchr(label[i],'.');
            if (p!=NULL) { k=p-label[i];
                           if (k>dmax) dmax=k; }
            len=strlen(label[i]); if (len>lmax) lmax=len;
            }
        if (dmax==0) dmax=lmax;

        find_tickturn();

        for (i=0; i<n; ++i)
            {
            int x1,k;
            char *p;
            int len;
            char *q;

            len=strlen(label[i]);
            y1=y0+(int)((ymu(value[i])-ymumin)/(ymumax-ymumin)*pituus);
            q=label[i];
            if (*q=='?') { ++q; --len; } else p_line2(x0,y1,x0+2*tickturn*tikki,y1,1);
            p=strchr(q,'.');
            if (p!=NULL) k=dmax-(p-q);
            else if (len<=dmax) k=dmax-len; else k=0;
                x1=x0+(int)(kirjainlev*k)+3*tikki;
            if (yscalepos2!=0.0) x1=x0+(int)yscalepos2;
            p_text((unsigned char *)q,x1,y1-(int)(kirjainkork/2.0),1);
            }
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



static int define(char *x,char **sana,int n,char *rivi)
        {
        int i; // RS REM ,k;

        if (n!=3) { koodivirhe(rivi); return(-1); }

        i=strlen(sana[1]);
        if ( sana[1][0]!='[' || sana[1][i-1]!=']' )
            {
//          PR_EBLD;
//          Rprintf("\nBrackets [] missing in %s\n",sana[1]);

            sprintf(sbuf,"Brackets [] missing in %s",sana[1]);
            p_error(sbuf);

            PR_ENRM; return(-1);
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
        int i; // RS REM ,k;
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
// RS REM        unsigned char koodi;
// RS REM        char luku_koodi[4];  /* Canon VDC */
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
//                    vdc(atoi(s+2),x);
                    strncpy(x,s+2,LLENGTH); // RS 21.3.2013
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

//              Rprintf("\n%s] is unknown!\n",s); WAIT; return(-1);
                sprintf(sbuf,"%s] is unknown!",s);
                p_error(sbuf);
                return(-1);

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
//      Rprintf("\nErroneous code line/word:\n%s",x);
        sprintf(sbuf,"Invalid code line/word: %s",x);
        p_error(sbuf);
        PR_ENRM;
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


static int makro(char *sana,char *muunnos)
        {
        char *s; // , *y;
        int i,len;
        char *parm[10]; int nparm;
        char *sparm[10]; int nsparm;
        char prsana[LLENGTH];
        char *p;
        char varasana[LLENGTH];

        strcpy(varasana,sana);
        s=strchr(sana,'('); *s=EOS; ++s;
        strcpy(prsana,sana); strcat(prsana,"(");
//        y=muunnos;
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
//          Rprintf("\nIncorrect number of parameters in %s\n",varasana);
            sprintf(sbuf,"Incorrect number of parameters in %s",varasana);
            p_error(sbuf);

            return(-1);
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

/*
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
*/

static int include(char *x,char **sana,int n)
        {
        FILE *ifile;
        char rivi[LLENGTH];
        int i,len;

        strcpy(rivi,sana[1]);
        if (strchr(rivi,':')==NULL && *rivi!='.' && *rivi!='~' && *rivi!='/' && *rivi!='\\') // RS ADD unix path FIXME
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
            return(-1); // RS ADD
            }

        while (1)
            {
            fgets(rivi,LLENGTH,ifile);
            if (feof(ifile)) break;
            len=strlen(rivi); rivi[len-1]=EOS;
            if (rivi[len-2]=='\r') rivi[len-2]=EOS; // RS ADD
            i=lue_koodit(rivi); if (i<0) { muste_fclose(ifile); return(-1); }
            }

        muste_fclose(ifile);
        return(1);
        }



/*  pcur.c 11.7.1985/SM (11.2.1991)
    PLOT curves
*/

static void muste_pcur(int argc, char *argv[])
        {
        int i,j,k; // ,ind; // RS REM ,v;
        char laite[LLENGTH];
// RS REM        char gtype[16];

        if (argc==1) return;
        s_init(argv[1]);
        argv1=argv[1];

        integral_function=0;
        if (muste_strcmpi(word[1],"INTEGRAL")==0)
            integral_function=1;

        i=tutki_yhtalo(); if (i<0) return;

     	muste_gplot_init=1;
     	i=c; j=c1; c=1; c1=1;
     	k=sp_init(r1+r-1);
     	muste_gplot_init=0; c=i; c1=j;
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
        int j; // RS REM ,i;
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
        WAIT;
        }


static void plot_tscale(
int n,              /* arvojen lkm */
double value[],     /* skaala-arvot */
char *label[],      /* skaalanimet */
int x0,
int y0,          /* alkupiste */
int pituus,         /* asteikon pituus */
int laji /* 1=XSCALE 2=XSCALE2 */
) { }

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
            { sprintf(sbuf,"\n%sSCALE values missing!",suunta); sur_print(sbuf); WAIT; return(-1); }

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

static int plot_curves()
        {
        int i,k;
        int x,y;
        int fill_count=0;
// RS REM        double y_integral;
        char xx[LLENGTH], *osa[N_MESS];
        char xx2[LLENGTH], *osa2[4];
//        int prind=0;
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
                    p_text((unsigned char *)(c_text[i]),c_x[i],c_y[i],1);
                    laske(c_message[i],&a);

                    sprintf(c_text[i],"%g",a);

// 2.8.2009 needed in "Buffon's needle problem"
       k=strlen(c_text[i]); if (k>max_len) max_len=k;
       p_fill_bar(c_x[i],c_y[i],c_x[i]+max_len*(int)kirjainlev,c_y[i]+(int)kirjainkork,1);

                    char_color=0; p_charcolor();
                    p_text((unsigned char *)(c_text[i]),c_x[i],c_y[i],1);
                    }
                }


            if (*color_change) change_color();
            coord(t,&x_pos,&y_pos);

//Rprintf("\nt=%g x=%d y=%d",t,x_pos,y_pos);
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
/*              Rprintf("\nt=%g x=%d y=%d",t,x_pos,y_pos);       */
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

        i=spfind2(muuttujanimi,4); // RS CHA 3 -> 4
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
//              Rprintf("\nEnter plotting range in form:");
//              Rprintf("\n%s=<lower_limit>,<upper_limit>,<step>",muuttujanimi);

    sprintf(sbuf,"Plotting range in form: %s=<lower_limit>,<upper_limit>,<step>"
                                        ,muuttujanimi);
                p_error(sbuf);

                return(-1);
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

// Rprintf("\nread_loopar=%d",nloop); getch();
// Rprintf("\nspb[i]=%s|",spb[i]); getch();

        if (nloop==MAXLOOP)
            {
//          Rprintf("\nToo many (>%d) loop parameters!",MAXLOOP);
            sprintf(sbuf,"Too many (>%d) loop parameters!",MAXLOOP);
            p_error(sbuf);
            return(-1);
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
                return(-1);
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

        lag_datapar[nvar]=0;
        p=strchr(sana[1],'[');
        if (p!=NULL) { *p=EOS; lag_datapar[nvar]=atoi(p+1); }
        k=varfind(&curd,sana[1]); if (k<0) return(-1); //RS CHA FIXME exit(1);
        curd_var[nvar]=k;
        i=data_load(&curd,obs+(long)lag_datapar[nvar],curd_var[nvar],&arvo[sp_ind[nvar]]);
        ++nvar;
        return(i);
        }

static int read_datapar()
        {
        int i, erstat;

        ++obs;
        while (obs<=curd.l2 && unsuitable(&curd,obs)) ++obs;
        if (obs>curd.l2) return(-1);

        for (i=0; i<nvar; ++i)
            {
            erstat=data_load(&curd,obs+(long)lag_datapar[i],curd_var[i],&arvo[sp_ind[i]]);
            if (erstat<0) return(-1); // RS 17.1.2013
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
//          Rprintf("\nEnter fill parameters in form:");
//          Rprintf("\n%s=<step>,<lower_limit>,<upper_limit>",spa[i]);
      sprintf(sbuf,"Fill parameters in form: %s=<step>,<lower_limit>,<upper_limit>"
                    ,spa[i]);
            p_error(sbuf);

            return(-1);
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
// RS REM        int i;
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

static int replace_function_name(char *sana,int *plen) /* 13.2.2005 esim. M()=MAT_RG.M() */
{
    int i;
    char sana2[LLENGTH]; // RS CHA 32 -> LLENGTH
    int len;
    char *p;
    char x[LLENGTH]; // RS CHA 32 -> LLENGTH

    *sana2=EOS;
    strncat(sana2,sana,(unsigned int)*plen);
    strcat(sana2,"()");
    i=spfind(sana2);
    if (i<0) return(1);
    /*Rprintf("\nfunc=%s|",spb[i]); getch(); */
    strcpy(x,spb[i]);
    p=strchr(x,'(');
    if (p!=NULL) *p=EOS;
    len=strlen(x);
    strcpy(sana2,spb[i]);
    strcat(sana2,sana+*plen);
    strcpy(sana,sana2);
    *plen+=len-*plen;
    return(1);
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
      int narg;    // Usean muuttujan funktion argumenttien lkm   
        int i;

// Rprintf("\nlauseke: %s",lauseke);
        
    int mat_element=0; // RS ADD
    int n_mat_par=0; 

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
// RS ADD              
				sana[len]=EOS; /* 15.2.2005 */          
				replace_function_name(sana,&len); /* 13.2.2005 */
				mat_element=0; 
				if (strncmp(sana,"MAT_",4)==0)
				{
// Rprintf("\nsana: %s, %d",sana,n_mat_par);				
					mat_element=1;
					n_mat_par=0;
				}
				if (strncmp(sana,"DAT_",4)==0)
				{
					mat_element=1;
					n_mat_par=0;
				}           
// RS ADD END                          
              
                q=p+1;
//              if (*q==')') { Rprintf("\nArguments missing in %s",lauseke);
//                             l_virhe=1; return(-1); }
                if (*q==')')
                    {
                    sprintf(sbuf,"Arguments missing in %s",lauseke);
                    p_error(sbuf);
                    return(-1); // RS ADD
                    }
                n=1;
              narg=1;
                while (n)
                    {
                    ++p;
                    if (*p=='(') { ++n; continue; }
                    if (*p==')') { --n; continue; }
//                  if (*p==EOS) { Rprintf("\n) is missing in %s",lauseke);
//                                 l_virhe=1; return(-1); }
                    if (*p==EOS)
                        {
                        sprintf(sbuf,") is missing in %s",lauseke);
                        p_error(sbuf);
                        return(-1); // RS ADD
                        }
                        
					if (*p==',' && n==1)
						{
						*p=EOS;
	
						if (mat_element) str_opnd[n_mat_par++]=q;
						else
						{
							/*    Rprintf("\nq=%s|",q); getch(); */
							laske(q,&opnd[t]);
						}
						++t;
						if (t>MAXARG+3)
						{
							sprintf(sbuf,"\nToo many arguments in %s",lauseke);
							sur_print(sbuf);
							l_virhe=1;
							return(-1);
						}
						++narg;
						q=p+1;
						}  
/* RS CHA						                      
                    if (*p==',' && n==1)
                        {
                        *p=EOS;

                        laske(q,&opnd[t]);
                        ++t;
                        if (t>MAXARG+3)
                            { sprintf(sbuf,"\nToo many arguments in %s",lauseke); sur_print(sbuf);
                              l_virhe=1; return(-1); }
                        ++narg;
                        q=p+1;
                        }
*/
                    }

// Rprintf("\nnarg: %d, lauseke: %s, q: %s",narg,lauseke,q);

//              if(strchr("+-*/^)\0",*(p+1))==NULL) { syntax_error(lauseke);
//                                                    return(-1); }
// mahdollinen loopar muotoa a(step)b 4.9.2001
                if(strchr("+-*/^)\0",*(p+1))==NULL) return(2); 

                *p=EOS; ++p;

				if (mat_element) str_opnd[n_mat_par++]=q;
				else
				{
					i=laske(q,&opnd[t]);
					if (i<0 || l_virhe) return(-1);
				}

/* RS CHA
                i=laske(q,&opnd[t]);
                if (i<0 || l_virhe) return(-1);
*/                
// RS REM                if (i==2) { Rprintf("\nret2"); getch(); }

/*   Rprintf("\ntulos1=%f",opnd[t]); getch();  */
                if (len==0) { len=-1; break; }
                sana[len]=EOS;
// Rprintf("\nnarg: %d",narg);
                if (narg>1)
                    {

// Rprintf("\nArgumentit for %s",sana);
// for (i=t-narg+1; i<=t; ++i) Rprintf(" %g",opnd[i]);

                    t=t-narg+1;
                    if (*sana=='-')
                        opnd[t]=-mfunktio(sana+1,opnd+t,narg);
                    else
                        opnd[t]=mfunktio(sana,opnd+t,narg);
                    if (l_virhe) return(-1);
                    len=-1;
                    
// Rprintf("\ntulos1=%f",opnd[t]);                    
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
//              Rprintf("\n( missing in %s",lauseke); l_virhe=1; return(-1);
                sprintf(sbuf,"( missing in %s",lauseke);
                p_error(sbuf);
                return(-1); // RS ADD

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

static int lab_find(char *x, char *lab, int m, int len)
        {
        char s[LLENGTH];
        int i;

        strcpy(s,x);
        for (i=strlen(s); i<len; ++i) s[i]=' ';
        for (i=0; i<m; ++i)
            if (strncmp(s,lab+i*len,(unsigned int)len)==0) break;
        if (i==m) return(-1);
        return(i+1);
        }

static void mat_function(char *f, char **s, int nn, double *yy)
        {
        int i,j=0,k; // RS j init
        double xx[2];
/*        char *lab;  */

//Rprintf("\nf=%s nn=%d %s %s",f,nn,s[0],s[1]); 

        for (k=0; k<mat_nmat; ++k)
            {
            if (strcmp(f,mat_name_arit[k])==0) break;
            }
        if (mat_nmat==0 || k==mat_nmat)
            {

            if (mat_nmat==NMAT) mat_nmat=0; /* kiertokulku */
/*
                {
                sprintf(sbuf,"Too many matrices (more than %d)!",NMAT);
                sur_print(sbuf); WAIT; l_virhe=1; nmat=0; return;
                }
*/
   mat_load(f,&mat_mat[k],&mat_m[k],&mat_n[k],&mat_rlab[k],&mat_clab[k],&mat_lr[k],&mat_lc[k]);

            strcpy(mat_name_arit[k],f);
            ++mat_nmat;

            }
        if (nn==1 && mat_m[k]==1) { nn=2; s[1]=s[0]; s[0]="1"; }
        i=lab_find(s[0],mat_rlab[k],mat_m[k],mat_lr[k]);
        if (i>0) xx[0]=i;
        else
            {
            laske(s[0],&xx[0]);
            sprintf(sbuf,"%g",xx[0]);    /* 9.9.1999 */
//Rprintf("\nsbuf: %s",sbuf);
            i=lab_find(sbuf,mat_rlab[k],mat_m[k],mat_lr[k]);
            if (i>0) xx[0]=i;
            }
        if (nn>1)
            {
            i=lab_find(s[1],mat_clab[k],mat_n[k],mat_lc[k]);
            if (i>0) xx[1]=i;
            else
                {
                laske(s[1],&xx[1]);
                sprintf(sbuf,"%g",xx[1]);    /* 9.9.1999 */
                i=lab_find(sbuf,mat_clab[k],mat_n[k],mat_lc[k]);
                if (i>0) xx[1]=i;
                }
            }

        i=xx[0]; if (nn>1) j=xx[1];
        if (i<1 || i>mat_m[k] || (nn>1 && (j<1 || j>mat_n[k])) )
            {
            sur_print("\nError in matrix index!"); WAIT;
            l_virhe=1;
            return;
            }
        if (nn==1)
            *yy=mat_mat[k][i-1];
        else
            *yy=mat_mat[k][i-1+mat_m[k]*(j-1)];
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

            if (*s=='M' && strncmp(s,"MAT_",4)==0)
                {
                mat_function(s+4,str_opnd,1,&y);
                return(y);
                }

        i=f_edit(s,&x,1,&y); if (i>0) return(y);    /* 20.6.1992 */
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
    char S[LLENGTH]; // RS CHA 32 -> LLENGTH

    /*     Rprintf("\nmfunktio: %s:",S);
       for (i=0; i<n; ++i) Rprintf("%g ",x[i]); getch();
    */

    strncpy(S,s,31);
    S[31]=EOS;


    if (strcmp(S,"bin.f")==0 || strcmp(S,"BIN.f")==0 || strcmp(S,"Bin.f")==0 )
    {
        return(muste_pdf_binom(x[2],x[0],x[1]));
    }

    if (strcmp(S,"bin.F")==0 || strcmp(S,"BIN.F")==0 || strcmp(S,"Bin.F")==0 )
    {
        return(muste_cdf_binom(x[2],x[0],x[1]));
    }

    if (strcmp(S,"bin.G")==0 || strcmp(S,"BIN.G")==0 || strcmp(S,"Bin.G")==0 )
    {
        return(muste_inv_binom(x[2],x[0],x[1]));
    }

    if (strcmp(S,"poisson.f")==0 || strcmp(S,"POISSON.f")==0 || strcmp(S,"Poisson.f")==0 )
    {
        return(muste_pdf_poisson(x[1],x[0]));
    }

    if (strcmp(S,"poisson.F")==0 || strcmp(S,"POISSON.F")==0 || strcmp(S,"Poisson.F")==0 )
    {
        return(muste_cdf_poisson(x[1],x[0]));
    }

    if (strcmp(S,"poisson.G")==0 || strcmp(S,"POISSON.G")==0 || strcmp(S,"Poisson.G")==0 )
    {
        return(muste_inv_poisson(x[1],x[0]));
    }

    if (strcmp(S,"N.f")==0 || strcmp(S,"n.f")==0 )
    {
        return(muste_pdf_normal(x[2],x[0],x[1]));
    }

    if (strcmp(S,"N.F")==0 || strcmp(S,"n.F")==0 )
    {
        return(muste_cdf_normal(x[2],x[0],x[1]));
    }

    if (strcmp(S,"N.G")==0 || strcmp(S,"n.G")==0 )
    {
        return(muste_inv_normal(x[2],x[0],x[1]));
    }

    if (strcmp(S,"t.f")==0 || strcmp(S,"T.f")==0 )
    {
        return(muste_pdf_t(x[1],x[0]));
    }

    if (strcmp(S,"t.F")==0 || strcmp(S,"T.F")==0 )
    {
        return(muste_cdf_t(x[1],x[0]));
    }

    if (strcmp(S,"t.G")==0 || strcmp(S,"T.G")==0 )
    {
        return(muste_inv_t(x[1],x[0]));
    }

    if (strcmp(S,"chi2.f")==0 || strcmp(S,"CHI2.f")==0 || strcmp(S,"Chi2.f")==0 )
    {
        return(muste_pdf_chi2(x[1],x[0]));
    }

    if (strcmp(S,"chi2.F")==0 || strcmp(S,"CHI2.F")==0 || strcmp(S,"Chi2.F")==0 )
    {
        return(muste_cdf_chi2(x[1],x[0]));
    }

    if (strcmp(S,"chi2.G")==0 || strcmp(S,"CHI2.G")==0 || strcmp(S,"Chi2.G")==0 )
    {
        return(muste_inv_chi2(x[1],x[0]));
    }

    if (strcmp(S,"F.f")==0 || strcmp(S,"f.f")==0 )
    {
        return(muste_pdf_f(x[2],x[0],x[1]));
    }

    if (strcmp(S,"F.F")==0 || strcmp(S,"f.F")==0 )
    {
        return(muste_cdf_f(x[2],x[0],x[1]));
    }

    if (strcmp(S,"F.G")==0 || strcmp(S,"f.G")==0 )
    {
        return(muste_inv_f(x[2],x[0],x[1]));
    }

    if (strcmp(S,"gamma.f")==0 || strcmp(S,"GAMMA.f")==0 || strcmp(S,"Gamma.f")==0 )
    {
        return(muste_pdf_gamma(x[2],x[0],x[1]));
    }

    if (strcmp(S,"gamma.F")==0 || strcmp(S,"GAMMA.F")==0 || strcmp(S,"Gamma.F")==0 )
    {
        return(muste_cdf_gamma(x[2],x[0],x[1]));
    }

    if (strcmp(S,"gamma.G")==0 || strcmp(S,"GAMMA.G")==0 || strcmp(S,"Gamma.G")==0 )
    {
        return(muste_inv_gamma(x[2],x[0],x[1]));
    }

    if (strcmp(S,"beta.f")==0 || strcmp(S,"BETA.f")==0 || strcmp(S,"Beta.f")==0 )
    {
        return(muste_pdf_beta(x[2],x[0],x[1]));
    }

    if (strcmp(S,"beta.F")==0 || strcmp(S,"BETA.F")==0 || strcmp(S,"Beta.F")==0 )
    {
        return(muste_cdf_beta(x[2],x[0],x[1]));
    }

    if (strcmp(S,"beta.G")==0 || strcmp(S,"BETA.G")==0 || strcmp(S,"Beta.G")==0 )
    {
        return(muste_inv_beta(x[2],x[0],x[1]));
    }

    if (strcmp(S,"weibull.f")==0 || strcmp(S,"WEIBULL.f")==0 || strcmp(S,"Weibull.f")==0 )
    {
        return(muste_pdf_weibull(x[2],x[0],x[1]));
    }

    if (strcmp(S,"weibull.F")==0 || strcmp(S,"WEIBULL.F")==0 || strcmp(S,"Weibull.F")==0 )
    {
        return(muste_cdf_weibull(x[2],x[0],x[1]));
    }

    if (strcmp(S,"weibull.G")==0 || strcmp(S,"WEIBULL.G")==0 || strcmp(S,"Weibull.G")==0 )
    {
        return(muste_inv_weibull(x[2],x[0],x[1]));
    }

    if (strcmp(S,"exp.f")==0 || strcmp(S,"EXP.f")==0 || strcmp(S,"Exp.f")==0 )
    {
        return(muste_pdf_exp(x[1],x[0]));
    }

    if (strcmp(S,"exp.F")==0 || strcmp(S,"EXP.F")==0 || strcmp(S,"Exp.F")==0 )
    {
        return(muste_cdf_exp(x[1],x[0]));
    }

    if (strcmp(S,"exp.G")==0 || strcmp(S,"EXP.G")==0 || strcmp(S,"Exp.G")==0 )
    {
        return(muste_inv_exp(x[1],x[0]));
    }

    muste_strupr(S);  /* No more case sensitive function names */

    /* R-style normal density */
    if (strcmp(S,"DNORM")==0)
    {
        if (n>3) return(muste_density_normal(x[0],x[1],x[2],(int)x[3]));
        return(muste_density_normal(x[0],x[1],x[2],(int)0));
    }


    if (strcmp(S,"MAX")==0) return(muste_max(x,n));
    if (strcmp(S,"MIN")==0) return(muste_min(x,n));
    if (strcmp(S,"MAXN")==0) return(muste_maxn(x,n));
    if (strcmp(S,"MINN")==0) return(muste_minn(x,n));
    if (strcmp(s,"C")==0)
    {
        if (n!=2)
        {
            arg_virhe(s);
        }
        return(muste_C(x[0],x[1]));
    }

    if (strcmp(S,"K_FACT")==0 || strcmp(S,"LK_FACT")==0)
    {
        int h;

        h=0;
        if (*S=='L') h=1;
        if (n!=2)
        {
            arg_virhe(s);
        }
        return(muste_k_fact(x[0],x[1],h));
    }

    if (strcmp(S,"GCD")==0)
    {
        return (gcd(x[0],x[1]));
    }
    if (strcmp(S,"MOD")==0)
    {
        return(muste_mod(x[0],x[1]));
    }
    if (strcmp(S,"ROOT")==0)
    {
        return (root(x[0],x[1]));
    }
    if (strcmp(S,"ROUND")==0)
    {
        return(muste_round(x[0],x[1]));
    }

/*
    if (strcmp(S,"X")==0)
    {
        extern double ed_number();
        return (ed_number(x[0],x[1]));
    }
*/    

    /* 14.8.2005 days from 1.1.2000 */
    if (strcmp(S,"DAYS")==0)
    {
        double date;
        sur_julian(x[0],x[1],x[2],&date);
        return(date-2451544.0);
    }

        if (strcmp(S,"NONDIV")==0) // 26.4.2009
            {
            return(nondiv(x[0],x[1]));
            }

        if (strcmp(S,"MTOTIENT")==0) // 30.4.2009
            {
            return(mtotient(x[0],x[1]));
            }

        if (strcmp(S,"BETA")==0) return(muste_beta(x[0],x[1])); // RS
        if (strcmp(S,"LBETA")==0) return(muste_lbeta(x[0],x[1])); // RS
        
        if (strcmp(S,"FIN.PV")==0) return(muste_fin_pv(x[0],x[1],x[2])); // RS
        if (strcmp(S,"FIN.FV")==0) return(muste_fin_fv(x[0],x[1],x[2])); // RS
        if (strcmp(S,"FIN.PMT")==0) return(muste_fin_pmt(x[0],x[1],x[2])); // RS
    
        if (strcmp(S,"BOXCOX")==0) return(muste_boxcox(x[0],x[1])); // RS
        if (strcmp(S,"BOXCOX.G")==0) return(muste_inv_boxcox(x[0],x[1])); // RS

        if (strcmp(S,"DISS")==0) return(muste_diss(x[0],x[1],(int)0)); // RS
        if (strcmp(S,"DISS.F")==0) return(muste_diss(x[0],x[1],(int) 1)); // RS

        if (strcmp(S,"BESTVAL")==0) return(muste_bestval(x[0],x[1])); // RS

            if (*s=='M' && strncmp(s,"MAT_",4)==0)
                {
                mat_function(s+4,str_opnd,n,&y);
                return(y);
                }

/* RS REM    
            if (*s=='D' && strncmp(s,"DAT_",4)==0)
                {
                dat_function(s+4,str_opnd,n,&y);
                return(y);
                }
*/    
    i=f_edit(s,x,n,&y); if (i>0) return(y);

/* RS REM    
    i=f_tiedosto(s,x,n,&y);
    if (i>0 && y!=MISSING8) return(y);
*/    

    l_virhe=1;
    return(x[0]);
}

static int f_edit(char *s,double *x,int n,double *py)
        {
        int i,k,len;
        char lauseke[LLENGTH];
        char xx[LLENGTH], *osa[MAXARG];
        char sana[7];     /*  EARG 1 2 3 4 EARG EOS */
        double y;
// RS REM        char *p,*q;
// RS REM        int h;

        len=strlen(s); s[len++]='(';
        i=0;
        while (i<spn && (spp[i]!=':' || strncmp(s,spa[i],len)!=0)) ++i;
        if (i==spn) { s[len-1]=EOS; return(-1); }

// Rprintf("\ns:%s spa=%s spp=%c spb=%s",s,spa[i],spp[i],spb[i]);

/*        if (!earg_varattu) { k=varaa_earg(); if (k<0) return(-1); }  */ // RS REM

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
    for (i=0; i<n; ++i) Rprintf("\nosa %d: %s",i+1,osa[i]); getch();
*/
        for (i=0; i<n; ++i)
            {
            k=aseta_earg(x[i],sana); if (k<0) return(-1);
            korvaa2(lauseke,osa[i],sana);
            }
/* Rprintf("x[0]=%g x[1]=%g\n",x[0],x[1]); getch(); */
        laske(lauseke,&y);
/* Rprintf(" y=%g\n",y); getch(); */
        *py=y;
        n_earg-=n;
        return(1);
        }

static void korvaa2(char *s,char *x,char *y)
        {
        char *p,*q;
        char z[LLENGTH];
        int len=strlen(x);
//Rprintf("\nkorvaa2 in: %s, x: %s, y: %s",s,x,y);
        *z=EOS;
        p=s;
        while ((q=strstr(p,x))!=NULL)
            {
            if (strchr(",+-*/^)=<>!",*(q+len))!=NULL || *(q+len)==EOS)
                {
                strncat(z,p,(unsigned int)(q-p));
                strcat(z,y);
                p=q+len;
                }
            else  /* x osa funktion nimeÑ */
                {
                strncat(z,p,(unsigned int)(q-p));
                strcat(z,x);
                p=q+len;
                }
            }
        strcat(z,p);
        strcpy(s,z);       
        }
 
        
static int varaa_earg()
        {
        earg=(double *)muste_realloc(earg,MAXEARG*sizeof(double));
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
//      Rprintf("\nUnknown function %s",s);
        sprintf(sbuf,"Unknown function %s",s);
        p_error(sbuf);
        l_virhe=1;
        }

static void arg_virhe(char *s)
        {
//      Rprintf("\n%s: Error in arguments",s);
        sprintf(sbuf,"%s: Error in arguments",s);
        p_error(sbuf);
        l_virhe=1;
        }

static void syntax_error(char *s)
        {
//      Rprintf("\nsyntax error in %s",s);
        sprintf(sbuf,"Syntax error in %s",s);
        p_error(sbuf);
        l_virhe=1;
        }

static int laske2(char *muuttuja,double *y)
        {
        int i,k; // j
/*
        extern int sp_read;
        if (!sp_read)
            {
            i=sp_init(r1+r-1); sp_read=1;
            if (i<0)
                {
                Rprintf("\nToo many specifications!");
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
//          Rprintf("\nParameter %s not found!",muuttuja);
            sprintf(sbuf,"Parameter %s not found!",muuttuja);
            p_error(sbuf);
//            WAIT;
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
        char rel=0;
        char *p;
        int sulut;
        char x[LLENGTH];
        double y1;
        int tosi;

/*      Rprintf("\nvarif: %s",lauseke); getch();     */
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
//                  Rprintf("\nrelation symbol =<> missing! in %s\n",x);
                    sprintf(sbuf,"relation symbol =<> missing! in %s",x);
                    p_error2(sbuf);
                    l_virhe=1; return(-1);
                    }
                break;
              case '(':
                ++sulut; ++p;
                break;
              default:
                ++p;
                }
            }

/*  Rprintf("\na=%s rel=%c",a,rel);      */
        b=p+1;
        p=b;
        while (1)
            {
            p=strchr(p,')');
            if (p==NULL) { if_syntax_error(lauseke); return(-1); }
            if (strncmp(p,")then(",6)==0) { *p=EOS; break; }
            ++p;
            }
/*  Rprintf(" b=%s",b);  */
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
/* Rprintf(" c=%s d=%s",c,d);
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
//      Rprintf("\nSyntax error in %s\n",x);
        sprintf(sbuf,"Syntax error in %s",x);
        p_error2(sbuf);
        l_virhe=1;
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
/*  for (i=0; i<k; ++i) Rprintf("\n%s  %s",spa[i],spb[i]); getch();
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


static void free_all()
        {
        fcloseall();
        free_spec();
        }
*/



static int hbar(int gtype,char *type,char *data)
/*int gtype;  1=HBAR 2=%HBAR 3=MHBAR 4=%MHBAR 5=%AHBAR */
           /* 6=NHBAR 2.2.1993 ja 6=PYRAMID (pyramid=1) 18.10.2005 */
        {
        int i;
        char otsikko[LLENGTH];

        strcpy(otsikko,type); strcat(otsikko," chart of ");
        strcat(otsikko,data);
        i=pen(); if (i<0) { p_end(); return(-1); }

        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
        if (pr_type==1 || pr_type==2)
         { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
        i=datain(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        if (gtype==2 || gtype==4 || gtype==5)
            {
            i=prosentit(); if (i<0) { p_end(); data_close(&dat); return(-1); }
            }
        i=xyscale_bar(gtype,"X"); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=xlabel(""); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=ylabel(""); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=shading(em2-1); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=plot_hbar(gtype); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=grid("X"); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=tick("X"); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=texts(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        if (pr_type!=1 && pr_type!=2) { i=frames(); if (i<0) { p_end(); data_close(&dat); return(-1); } }
        i=fills(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=polygons(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        p_end(); data_close(&dat);
        return(1);
        }

static int pframe() /* PLOT /FRAME */
        {
        int i;

        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
        if (pr_type==1 || pr_type==2)
         { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(""); if (i<0) { p_end(); return(-1); }
        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1 && pr_type!=2) { i=frames(); if (i<0) { p_end(); return(-1); } }
        i=lines(); if (i<0) { p_end(); return(-1); }
        i=fills(); if (i<0) { p_end(); return(-1); }
        i=polygons(); if (i<0) { p_end(); return(-1); }
        p_end();
        return(1);
        }

static int prosentit()
        {
        double a,b,tot;
        int i,j;
        int n=l2-l1+1;

        tot=0.0;
        for (j=0; j<n; ++j)
            {
            a=0.0; xsumma[j]=0.0;
            for (i=0; i<em2-1; ++i)
                {
                b=xmat[j*em+i]; if (b<0.0)
                    {
                    sprintf(sbuf,"Negative values not allowed!"); p_error(sbuf);
                    return(-1);
                    }
                a+=b; xsumma[j]+=b;
                }
            tot+=xsumma[j];
            if (a==0.0 && grouping_var>=0) a=100.0; // 1.11.2002
            if (a==0.0)
                {
                sprintf(sbuf,"All values =0 in an observation"); p_error(sbuf);
                return(-1);
                }
            for (i=0; i<em2-1; ++i) xmat[j*em+i]*=100.0/a;
            }
        for (j=0; j<n; ++j) xsumma[j]=xsumma[j]/tot*n;

        return(1);
        }

static int plot_hbar(int gtype)
        {
        int i,j,m,mm,n,k;
        int patkataso; // 15.4.2011
        int nimimax2; // 15.4.2011
        char lab_delimiter; // 19.4.2011
        int lab_gap; // 19.4.2011
        double gap;
        double lev;      /* pylvÑÑn leveys */
        double vali;     /* pylvÑitten vÑli */
        int nimimax;     /* nimen max.pituus */
        int mhbar;
        double vali1,vali2;  /* alku- ja loppuvÑli 16.12.88 */
        double gap1,gap2; /* 16.12.88 */
        char x[LLENGTH],*sana[3];  /* 16.12.88 */
        int ytaso=0;
        double lev2;
        int yscalepos; /* 22.1.1995 */
        char *p;

        int next_group_label=0,next_new_group; // 1.11.2002

        n=l2-l1+1;
        m=em2-1;
                                /* 2.2.1993 */
        if (gtype>2 && gtype!=5 && gtype!=6)
            { mhbar=1; mm=m; } else { mhbar=0; mm=1; }

/*      i=spfind("GAP");
        if (i>=0) gap=atof(spb[i]); else gap=1.0/3.0;
*/

        gap=1.0/3.0; gap1=gap; gap2=0.0;
        i=spfind("GAP");     /* 16.12.88 */
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,sana,3);
            gap=atof(sana[0]); gap1=gap;
            if (i>1) gap1=atof(sana[1]);
            if (i>2) gap2=atof(sana[2]);
            }

        lab_delimiter='|'; // 19.4.2011
        lab_gap=(int)(1.5*kirjainkork);
        i=spfind("LAB_DELIMITER");
        if (i>=0)
            {
            lab_delimiter=*spb[i];
            p=strchr(spb[i],',');
            if (p!=NULL) lab_gap=atoi(p+1);
            }

        i=barvalues(); if (i<0) return(-1);
        i=bar_labels(); if (i<0) return(-1);
        i=names(); if (i<0) return(-1);
        lev=y_kuva/(gap1+mm*n+gap*(n-1)+gap2);  /* 16.12.88 */
        vali=gap*lev; vali1=gap1*lev; vali2=gap2*lev;

        nimimax=0; for (j=0; j<n; ++j) { i=strlen(xnimi[j]);
                                         if (i>nimimax) nimimax=i;
                                       }
        i=p_pen(); if (i<0) return(-1);
        i=p_linetype(); if (i<0) return(-1);
        yscalepos=32766;        /* 22.1.1995 */
        i=spfind("YSCALEPOS");
        if (i>=0) yscalepos=atoi(spb[i]);

        if (gtype==5) ytaso=yy+y_kuva-vali2+vali;
        next_new_group=0;


        nimimax2=0; // 15.4.2011
        for (j=0; j<n; ++j)
            {
            n_patkat=patki1(xnimi[j],lab_delimiter,&nimimax2); // 15.4.2011
            }
// Rprintf("\nnimimax2=%d",nimimax2);


        for (j=0; j<n; ++j)
            {
            int x_apu, y_apu, x_apu2, x_apu3;
            double a,b;
            double xmin, xmax;
            double aplus,aminus,xarvo; /* 2.2.1993 */
            int jj,ng;

            if (grouping_var>=0 && j==next_new_group)
                {
                for (jj=j+1; jj<n; ++jj)
                    if (strcmp(gnimi[jj],gnimi[j])!=0) break;
                ng=jj-j;
                next_group_label=j+ng/2;
                next_new_group=jj+1;
// Rprintf("\nlabel=%d new=%d|",next_group_label,next_new_group); getch();
                }

            if (gtype==5) lev2=xsumma[j]*lev+0.5; else lev2=lev;

            if (gtype==5)
                ytaso-=vali+lev2;
            else
                ytaso=(int)(yy+(n-1-j)*mm*lev+(n-j-1)*vali+vali1);
            if (j==0 && gap2==0.0 && !mhbar) lev2=yy+y_kuva-ytaso;

            if (yscalepos==32766)  /* 22.1.1995 */
                x_apu=xx-(int)(kirjainlev*(nimimax+1));
            else
                x_apu=xx+yscalepos;

                y_apu=ytaso+(int)(mm*lev2/2.0-kirjainkork/2.0);

            if (name_ind)
                {
                char kopio2[LLENGTH];

                strcpy(kopio2,namecode);
                if (*namecode!=EOS)
                    { k=p_textcontrol(kopio2); if (k<0) return(-1); }
// Rprintf("\nnimi=%s!",xnimi[j]); getch();
                n_patkat=patki2(xnimi[j],lab_delimiter); // 15.4.2011
/*******************************
printf("\nn_patkat=%d",n_patkat);
for (i=0; i<n_patkat; ++i)
printf("\n%s",patka[i]);
getch();
********************************/
                if (n_patkat==0) p_text((unsigned char *)(xnimi[j]),x_apu,y_apu,1);
                else // pÑtkitty nimi
                  {
                  x_apu=xx-(int)(kirjainlev*(nimimax2+1));

                  patkataso=y_apu+lab_gap*(n_patkat-1)/2;
                  for (i=0; i<n_patkat; ++i)
                     {
                     p_text((unsigned char *)(patka[i]),x_apu,patkataso,1);
                     patkataso-=lab_gap;
                     }
                  }

                }

            if (grouping_var>=0 && j==next_group_label) // 1.11.2002
                {
                char kopio2[LLENGTH];

                strcpy(kopio2,namecode);
                if (*namecode!=EOS)
                    { k=p_textcontrol(kopio2); if (k<0) return(-1); }
                p_text((unsigned char *)(gnimi[j]),(int)kirjainlev,y_apu,1);
                }

            xmin=xscaleval[0]; xmax=xscaleval[xscalen-1];
            a=0;
            aplus=aminus=0; /* 2.2.1993 */
            for (i=0; i<m; ++i)
              {

              if (gtype==6) /* 2.2.1993 */
                  {
                  xarvo=xmat[j*em+i];
                  if (xarvo<0.0) { a=aminus; aminus+=xarvo; }
                  else { a=aplus; aplus+=xarvo; }
                  }

              b=a+xmat[j*em+i];
              if (b==a) continue;
              if (mhbar)
              { k=lev; ytaso=(int)(yy+(n-1-j)*m*lev+(n-j-1)*vali+vali1)+(m-i-1)*k; }

              x_apu=xx+(a-xmin)/(xmax-xmin)*x_kuva;
              x_apu2=xx+(b-xmin)/(xmax-xmin)*x_kuva; /* pyîr.virheiden takia */
              if (a<minvalue) x_apu=xx;
              if (b<minvalue) x_apu2=xx;
              p_linetype();
              plot_box(x_apu,ytaso,x_apu2-x_apu,(int)lev2);
              if (!mhbar) a=b;
              if (capability[1])
                p_fill((int)((x_apu+x_apu2)/2),(int)(ytaso+lev2/2),shadeval[i]);
              else
                {
                p_fill_bar(x_apu,ytaso,x_apu2,ytaso+(int)lev2,shadeval[i]);
                plot_box(x_apu,ytaso,x_apu2-x_apu,(int)lev2);
                }

              if (devvar1>=0)
                {
                strcpy(sbuf,devcode);
                p_linecontrol(sbuf);

                a=devmat[2*j];
                x_apu3=xx+(a-xmin)/(xmax-xmin)*x_kuva;
                p_line2(x_apu2,(int)(ytaso+lev2/2),
                        x_apu3,(int)(ytaso+lev2/2),0);
                p_line2(x_apu3,(int)(ytaso+lev2/2-lev2/6),
                        x_apu3,(int)(ytaso+lev2/2+lev2/6),0);
                a=devmat[2*j+1];
                x_apu3=xx+(a-xmin)/(xmax-xmin)*x_kuva;
                p_line2(x_apu2,(int)(ytaso+lev2/2),
                        x_apu3,(int)(ytaso+lev2/2),0);
                p_line2(x_apu3,(int)(ytaso+lev2/2-lev2/6),
                        x_apu3,(int)(ytaso+lev2/2+lev2/6),0);

                p_pen();
                }

              if (*valform!=EOS)
                {
                char kopio[LLENGTH];
                double arvo;

                if (valpros)
                    {
                    arvo=0.0;
                    if (m==1)
                        {
                        for (k=0; k<n; ++k) arvo+=xmat[k*em+i];
                        arvo=100.0*xmat[j*em+i]/arvo;
                        }
                    else
                        {
                        for (k=0; k<m; ++k) arvo+=xmat[j*em+k];
                        arvo=100.0*xmat[j*em+i]/arvo;
                        }
                    }
                else arvo=xmat[j*em+i];

                strcpy(kopio,valcode);
                if (*valcode!=EOS)
                    { k=p_textcontrol(kopio); if (k<0) return(-1); }
                y_apu=ytaso+(int)(lev2/2.0-kirjainkork/2.0);
                if (arvo>=valuemin) valtext(x_apu,y_apu,x_apu2-x_apu,arvo,i); // 16.9.2010
                if (*valcode!=EOS)
                    { k=p_pen(); if (k<0) return(-1); p_charsize(); }
                                                      // 2.11.2002
                }

              if (labpaikka!=9999.9)
                {
                char kopio[LLENGTH];

                strcpy(kopio,labcode);
                if (*labcode!=EOS)
                    { k=p_textcontrol(kopio); if (k<0) return(-1); }
                y_apu=ytaso+(int)(lev2/2.0-kirjainkork/2.0);
                labtext(x_apu,y_apu,x_apu2-x_apu,xname[ev[i+1]],i); // 16.9.2010
                if (*labcode!=EOS)
                    { k=p_pen(); if (k<0) return(-1); p_charsize(); }
                                                      // 2.11.2002
                }
              if (capability[2]) p_textcolors(0); // 21.2.2012                
              } /* i */
            } /* j */
        legend((int)lev);
        return(1);
        }

static int patki1(char *nimi,char lab_delimiter,int *pnimimax2) // 15.4.2011
    {
    int n=0;
    int len;
    char *p,*q;
    p=strchr (nimi,'|'); if (p==NULL) return(0);
    strcpy(sbuf,nimi);
    p=sbuf;

    while (1)
        {
        q=strchr(p,lab_delimiter);
        if (q==NULL) return(n);
        *q=EOS;
        len=strlen(p);
        if(len>*pnimimax2) *pnimimax2=len;
        p=q+1;
        ++n;
        }
    return(n);
    }

static int patki2(char *nimi,char lab_delimiter) // 15.4.2011

    {
    int n=0;
// RS REM    int len;
    char *p,*q;
    char x[LNAME];
    p=strchr (nimi,lab_delimiter); if (p==NULL) return(0);
    strcpy(x,nimi);
    p=x;

    while (1)
        {
        q=strchr(p,lab_delimiter);
        if (q==NULL) return(n);
        *q=EOS; strcpy(patka[n],p);
        p=q+1;
        ++n;
        }
    return(n);
    }

static void maxmin_sum(double *max,double *min)
        {
        int i,j;
        double sum;

        *max=-1e30; *min=1e30;
        for (j=0; j<l2-l1+1; ++j)
            {
            sum=0;
            for (i=0; i<em2-1; ++i) sum+=xmat[j*em+i];
            if (sum>*max) *max=sum;
            if (sum<*min) *min=sum;
            }
        }

static void maxmin(double *max,double *min)
        {
        int i,j;
        double a;

        *max=-1e30; *min=1e30;
        for (j=0; j<l2-l1+1; ++j)
            {
            for (i=0; i<em2-1; ++i)
                {
                a=xmat[j*em+i];
                if (a>*max) *max=a;
                if (a<*min) *min=a;
                }
            }
        }

static int xyscale_bar(int gtype,char *suunta)
/* int gtype;         1=HBAR 2=%HBAR 3=MHBAR 4=%MHBAR 5=%AHBAR */
               /* tai 1=VBAR 2=%VBAR 3=MVBAR 4=%MVBAR 5=%AVBAR */
/* char *suunta;  "X" tai "Y" */
        {
        extern double xmin,xmax,xmumin,xmumax,ymin,ymax,ymumin,ymumax;
        extern double arit_atof();
        int i,k;
        char x[LLENGTH];
        char *p,*q;
        double min,max;

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
            if (gtype<3 || gtype==5)
                {
                maxmin_sum(&max,&min);
                if (em2>2 || min>0.0) min=0.0;
                }
            else
                {
                maxmin(&max,&min);
                if (min>0.0) min=0.0;
                }
            if (max<0.0) max=0.0;   /* 15.5.1993 */

            if (*suunta=='X') k=x_kuva/kirjainlev;
            else              k=2*y_kuva/kirjainkork;
            i=autom_scale(x,min,max,k); if (i<0) return(-1);
            }
        k=control_code(x,&p,0);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        if (*p==EOS)
            { sprintf(sbuf,"\n%sSCALE values missing!\n",suunta); sur_print(sbuf); WAIT; return(-1); }
        k=skaala_arvot(p,xscales,xscal,&xscalen,scalespace);
        if (k<0) return(-1);

        for (i=0; i<xscalen; ++i)
            {
            q=xscal[i];
            p=strchr(xscal[i],':'); if (p!=NULL) { xscal[i]=p+1; *p=EOS; }
            xscaleval[i]=arit_atof(q);
            }
        if (xscaleval[xscalen-1]<=xscaleval[0])
            {
            sur_print("\nIncorrect SCALE!\n");
            WAIT; return(-1);
            }
        if (*suunta=='X')
            {
            xmin=xmumin=xscaleval[0]; xmax=xmumax=xscaleval[xscalen-1];
            plot_xscale(xscalen,xscaleval,xscal,xx,yy,x_kuva);
            }
        else
            {
            ymin=ymumin=xscaleval[0]; ymax=ymumax=xscaleval[xscalen-1];
            plot_yscale(xscalen,xscaleval,xscal,xx,yy,y_kuva);
            }
        return(1);
        }



static int barvalues()
        {
        int i,k;
        char x[LLENGTH], *osa[3];
        char *p;
        int sulkuind;

        *valform=EOS; *valcode=EOS;
        i=spfind("VALUES"); if (i<0) return(1);
        strcpy(x,spb[i]);
        k=etsi_loppusulku(x,&p); if (k<0) return(-1);
        if (*x=='(') sulkuind=1; else sulkuind=0;
        if (p!=x)
            {
            if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
            *(p-sulkuind)=EOS; strcpy(valcode,x+sulkuind); ++p;
            }
        k=split(p,osa,3);
        if (k<2) { sp_virhe(spa[i],spb[i]); return(-1); }
        if (k>2) valind=-1; else valind=1;
        strcpy(valform,osa[0]);
        valpaikka=atof(osa[1]);
        k=strlen(valform);
        if (valform[k-1]=='%') { valform[k-1]=EOS; valpros=1; }
        else                     valpros=0;


        valuemin=-1e10;         /* 29.3.1997 */
        i=spfind("VALUEMIN");
        if (i>=0) valuemin=atof(spb[i]);

/*  Rprintf("\nvalcode=%s valform=%s valpaikka=%g valpros=%d",
              valcode,valform,valpaikka,valpros);
    getch();
*/
        return(1);
        }

static int valtext(int x1,int y1,int leveys,double arvo,int color)
        {
// RS REM        int k;
        char luku[32];
        int paikka;
        int len;
        extern int pyramid;

        if (pyramid && arvo<0.0) arvo=-arvo; // 5.6.2006
        fconv(arvo,valform,luku);

        if (valpros) strcat(luku,"%");
        len=strlen(luku);

        if (valpaikka*valind>=0)
            {
            if (leveys>=0) paikka=x1+kirjainlev*valpaikka;
            else           paikka=x1-kirjainlev*(valpaikka+len);
            }
        else
            {
            if (leveys>=0) paikka=x1+leveys-kirjainlev*(-valpaikka+len);
            else           paikka=x1+leveys-kirjainlev*valpaikka;
            }

        if (colors_2010) p_textcolors(shadecolor[color]); // 16.9.2010
        p_text((unsigned char *)luku,paikka,y1,1);
        return(1);
        }

static void valtext2(int x1,int y1,int korkeus,double arvo,int color)
        {
// RS REM        int k;
        char luku[32];
        int paikka;
// RS REM        int len;

        fconv(arvo,valform,luku);
        if (valpros) strcat(luku,"%");

        if (valpaikka*valind>=0)
            {
            if (korkeus>=0) paikka=y1+kirjainkork*valpaikka;
            else            paikka=y1-kirjainkork*(valpaikka+0.5);
            }
        else
            {
            if (korkeus>=0) paikka=y1+korkeus+kirjainkork*(valpaikka-0.5);
            else            paikka=y1+korkeus-kirjainkork*valpaikka;
            }
        if (colors_2010) p_textcolors(shadecolor[color]); // 16.9.2010
        p_text((unsigned char *)luku,x1+(int)kirjainlev,paikka,1);

        }

static int valtextpie(int xp,int yp,double r,double t1,double t2,double arvo,int color)
        {
        char luku[32];
        int len;
        int x1,y1;

        fconv(arvo,valform,luku);
        if (valpros) strcat(luku,"%");
        len=strlen(luku);

        x1=xp+r*valpaikka/10.0*cos((t1+t2)/2);
        y1=yp+r*valpaikka/10.0*sin((t1+t2)/2)-kirjainkork/2.0;
        x1-=(int)(kirjainlev*len/2.0);
        if (colors_2010) p_textcolors(shadecolor[color]); // 16.9.2010
        p_text((unsigned char *)luku,x1,y1,1);
		return(1); // RS ADD
        }

static int bar_labels()
        {
        int i,k;
        char x[LLENGTH];
        char *p;
        int sulkuind;

        *labcode=EOS; labpaikka=9999.9;
        i=spfind("LABELS"); if (i<0) return(1);
        strcpy(x,spb[i]);
        k=etsi_loppusulku(x,&p); if (k<0) return(-1);
        if (*x=='(') sulkuind=1; else sulkuind=0;
        if (p!=x)
            {
            if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
            *(p-sulkuind)=EOS; strcpy(labcode,x+sulkuind); ++p;
            }
        labpaikka=atof(p);

/*  Rprintf("\nlabcode=%s labpaikka=%g",
              labcode,labpaikka);
    getch();
*/
        return(1);
        }

static void labtext(int x1,int y1,int leveys,char *teksti,int color)
        {
// RS REM        int k;
        int paikka;
        int len;

        len=strlen(teksti);
        if (labpaikka>=0)
            {
            if (leveys>=0) paikka=x1+kirjainlev*labpaikka;
            else           paikka=x1-kirjainlev*(labpaikka+len);
            }
        else
            {
            if (leveys>=0) paikka=x1+leveys-kirjainlev*(-labpaikka+len);
            else           paikka=x1+leveys-kirjainlev*labpaikka;
            }
        if (colors_2010) p_textcolors(shadecolor[color]); // 16.9.2010
        p_text((unsigned char *)teksti,paikka,y1,1);

        }

static void labtext2(int x1,int y1,int korkeus,char *teksti,int color)
        {
// RS REM        int k;
        int paikka;
// RS REM        int len;

        if (labpaikka>=0)
            {
            if (korkeus>=0) paikka=y1+kirjainkork*labpaikka;
            else            paikka=y1-kirjainkork*(labpaikka+0.5);
            }
        else
            {
            if (korkeus>=0) paikka=y1+korkeus+kirjainkork*(labpaikka-0.5);
            else            paikka=y1+korkeus-kirjainkork*labpaikka;
            }
        if (colors_2010) p_textcolors(shadecolor[color]); // 16.9.2010
        p_text((unsigned char *)teksti,x1+(int)kirjainlev,paikka,1);
        }

static void labtextpie(int xp,int yp,double r,double t1,double t2,char *teksti,int color)
        {
        int len;
        int x1,y1;

        len=strlen(teksti);

        x1=xp+r*labpaikka/10.0*cos((t1+t2)/2);
        y1=yp+r*labpaikka/10.0*sin((t1+t2)/2)-kirjainkork/2.0;
        x1-=(int)(kirjainlev*len/2.0);
        if (colors_2010) p_textcolors(shadecolor[color]); // 16.9.2010
        p_text((unsigned char *)teksti,x1,y1,1);

        }

static int names()
        {
        int i,k;
        char x[LLENGTH], *s[2];
        char *p;
        int sulkuind;

        name_ind=1; name_gap=0; *namecode=EOS;
        i=spfind("NAMES"); if (i<0) return(1);
        strcpy(x,spb[i]);
        k=etsi_loppusulku(x,&p); if (k<0) return(-1);
        if (*x=='(') sulkuind=1; else sulkuind=0;
        if (p!=x)
            {
            if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
            *(p-sulkuind)=EOS; strcpy(namecode,x+sulkuind); ++p;
            }
        i=split(p,s,2);
        if (i>0) name_ind=atoi(s[0]);  // # of levels 3.5.2004-
        if (i>1) name_gap=atoi(s[1]);
        return(1);
        }


/**************************************
In matrix plots, the columns are labelled by names of active variables
and rows by names of cases as in HBAR plots (see PLOTBAR?).
The setting of labels is adjusted by specifications
ROWLABELS=1,<number_of_label_columns>,<max.length_of_label>
COLUMNLABELS=1,<number_of_label_rows>

************************************/


static int dev_spec(char *s)
    {
    int i,k;
    char x[LLENGTH]; // RS REM , *osa[3];
    char *p;
    int sulkuind;

    *devcode=EOS; devvar1=-1; devvar2=-1;
    i=spfind("DEV"); if (i<0) { strcpy(s,"-"); return(1); }
    strcpy(x,spb[i]);
    k=etsi_loppusulku(x,&p); if (k<0) return(-1);
    if (*x=='(') sulkuind=1; else sulkuind=0;
    if (p!=x)
        {
        if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
        *(p-sulkuind)=EOS; strcpy(devcode,x+sulkuind); ++p;
        }
    strcpy(s,p);
    return(1);
    }


static int vbar(int gtype,char *type,char *data)
/*int gtype;  1=VBAR 2=%VBAR 3=MVBAR 4=%MVBAR 5=%AVBAR */
           /* 6=NVBAR */
        {
        int i;
        char otsikko[LLENGTH];

        strcpy(otsikko,type); strcat(otsikko," chart of ");
        strcat(otsikko,data);
        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
        if (pr_type==1) { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
        i=datain(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        if (gtype==2 || gtype==4 || gtype==5)
            {
            i=prosentit(); if (i<0) { p_end(); data_close(&dat); return(-1); }
            }
        i=xyscale_bar(gtype,"Y"); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=xlabel(""); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=ylabel(""); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=shading(em2-1); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=plot_vbar(gtype); if (i<0) {p_end(); data_close(&dat); return(-1); }
        i=grid("Y"); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=tick("Y"); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=texts(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        if (pr_type!=1) { i=frames(); if (i<0) { p_end(); data_close(&dat); return(-1); } }
        i=fills(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=polygons(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        p_end(); data_close(&dat);
        return(1);
        }

static int plot_vbar(int gtype)
        {
        int i,j,m,mm,n,k;
        double gap;
        double lev;      /* pylvÑÑn leveys */
        double vali;     /* pylvÑitten vÑli */
        int nimimax;     /* nimen max.pituus */
        int mvbar;
        double vali1; // ,vali2; /* alku- ja loppuvÑli 16.12.88 */
        double gap1,gap2;
        char x[LLENGTH],*sana[3];
        int xtaso=0;
        double lev2,lev3;
        int next_group_label=0,next_new_group; // 1.11.2002
        int name_level=0; // 3.5.2004
        extern int name_gap;

        n=l2-l1+1;
        m=em2-1;
                                /* 2.2.1993 */
        if (gtype>2 && gtype!=5 && gtype!=6)
            { mvbar=1; mm=m; } else { mvbar=0; mm=1; }
        gap=1.0/3.0; gap1=gap; gap2=0.0;
        i=spfind("GAP");     /* 16.12.88 */
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,sana,3);
            gap=atof(sana[0]); gap1=gap;
            if (i>1) gap1=atof(sana[1]);
            if (i>2) gap2=atof(sana[2]);
            }

        i=barvalues(); if (i<0) return(-1);
        i=bar_labels(); if (i<0) return(-1);
        i=names(); if (i<0) return(-1);
        lev=x_kuva/(gap1+mm*n+gap*(n-1)+gap2);  /* 16.12.88 */
        vali=gap*lev; vali1=gap1*lev; // vali2=gap2*lev;

        nimimax=0; for (j=0; j<n; ++j) { i=strlen(xnimi[j]);
                                         if (i>nimimax) nimimax=i;
                                       }
        i=p_pen(); if (i<0) return(-1);
        i=p_linetype(); if (i<0) return(-1);
        if (gtype==5) { xtaso=xx+vali1-vali; lev3=0.0; }

        next_new_group=0;

        for (j=0; j<n; ++j)
            {
            int y_apu, y_apu2, y_apu3; // RS REM , x_apu;
            double a,b;
            double xmin, xmax;
            double aplus,aminus,xarvo; /* 2.2.1993 */
            int jj;

            if (grouping_var>=0 && j==next_new_group)
                {
                for (jj=j+1; jj<n; ++jj)
                    if (strcmp(gnimi[jj],gnimi[j])!=0) break;
                next_group_label=j;
                next_new_group=jj+1;
                }

            if (gtype==5) lev2=xsumma[j]*lev+0.5; else lev2=lev;

            if (gtype==5)
                { xtaso+=lev3+vali; lev3=lev2; }
            else
                xtaso=(int)(xx+j*mm*lev+j*vali+vali1);
            if (name_gap==0) name_gap=(int)(1.2*kirjainkork);
            y_apu=yy-2*tikki-(int)(1.2*kirjainkork);

            if (name_ind)
                {
                char kopio2[LLENGTH];

                strcpy(kopio2,namecode);
                if (*namecode!=EOS)
                    { k=p_textcontrol(kopio2); if (k<0) return(-1); }
                p_text((unsigned char *)(xnimi[j]),xtaso,y_apu-name_level*name_gap,1);
                if (name_ind>1)
                    { ++name_level; if (name_level==name_ind)
                                           name_level=0;
                    }
                }

            if (grouping_var>=0 && j==next_group_label) // 1.11.2002
                {
                char kopio2[LLENGTH];

                strcpy(kopio2,namecode);
                if (*namecode!=EOS)
                    { k=p_textcontrol(kopio2); if (k<0) return(-1); }
                p_text((unsigned char *)(gnimi[j]),xtaso,y_apu-(int)(1.5*kirjainkork),1);
                }

            xmin=xscaleval[0]; xmax=xscaleval[xscalen-1];
            a=0;
            aplus=aminus=0.0;
            for (i=0; i<m; ++i)
              {
              if (gtype==6) /* 2.2.1993 */
                  {
                  xarvo=xmat[j*em+i];
                  if (xarvo<0.0) { a=aminus; aminus+=xarvo; }
                  else { a=aplus; aplus+=xarvo; }
                  }
              b=a+xmat[j*em+i];
              if (a==b) continue;
              if (mvbar)
              { k=lev; xtaso=(int)(xx+j*m*lev+j*vali+vali1)+i*k; }

              y_apu=yy+(a-xmin)/(xmax-xmin)*y_kuva;
              y_apu2=yy+(b-xmin)/(xmax-xmin)*y_kuva; /* pyîr.virheiden takia */
              if (a<minvalue) y_apu=yy;
              if (b<minvalue) y_apu2=yy;
              p_linetype();
              plot_box(xtaso,y_apu,(int)lev2,y_apu2-y_apu);
              if (!mvbar) a=b;
              if (capability[1])
                p_fill((int)(xtaso+lev2/2),(int)((y_apu+y_apu2)/2),shadeval[i]);
              else
                {
                p_fill_bar(xtaso,y_apu,xtaso+(int)lev2,y_apu2,shadeval[i]);
                plot_box(xtaso,y_apu,(int)lev2,y_apu2-y_apu);
                }

              if (devvar1>=0)
                {
                strcpy(sbuf,devcode);
                p_linecontrol(sbuf);

                a=devmat[2*j];
                y_apu3=yy+(a-xmin)/(xmax-xmin)*y_kuva;
                p_line2((int)(xtaso+lev2/2),y_apu2,
                        (int)(xtaso+lev2/2),y_apu3,0);
                p_line2((int)(xtaso+lev2/2-lev2/6),y_apu3,
                        (int)(xtaso+lev2/2+lev2/6),y_apu3,0);
                a=devmat[2*j+1];
                y_apu3=yy+(a-xmin)/(xmax-xmin)*y_kuva;
                p_line2((int)(xtaso+lev2/2),y_apu2,
                        (int)(xtaso+lev2/2),y_apu3,0);
                p_line2((int)(xtaso+lev2/2-lev2/6),y_apu3,
                        (int)(xtaso+lev2/2+lev2/6),y_apu3,0);

                p_pen();
                }
              if (*valform!=EOS)
                {
                char kopio[LLENGTH];
                double arvo;

                if (valpros)
                    {
                    arvo=0.0;
                    if (m==1)
                        {
                        for (k=0; k<n; ++k) arvo+=xmat[k*em+i];
                        arvo=100.0*xmat[j*em+i]/arvo;
                        }
                    else
                        {
                        for (k=0; k<m; ++k) arvo+=xmat[j*em+k];
                        arvo=100.0*xmat[j*em+i]/arvo;
                        }

                    }
                else arvo=xmat[j*em+i];

                strcpy(kopio,valcode);
                if (*valcode!=EOS)
                    { k=p_textcontrol(kopio); if (k<0) return(-1); }
                if (arvo>=valuemin) valtext2(xtaso,y_apu,y_apu2-y_apu,arvo,i); // 16.9.2010
                if (*valcode!=EOS)
                    { k=p_pen(); if (k<0) return(-1); }
                }

              if (labpaikka!=9999.9)
                {
                char kopio[LLENGTH];

                strcpy(kopio,labcode);
                if (*labcode!=EOS)
                    { k=p_textcontrol(kopio); if (k<0) return(-1); }
                labtext2(xtaso,y_apu,y_apu2-y_apu,xname[ev[i+1]],i);
                if (*labcode!=EOS)
                    { k=p_pen(); if (k<0) return(-1); }
                }
              if (capability[2]) p_textcolors(0); // 21.2.2012                
              } /* i */
            } /* j */
        legend((int)lev);
        return(1);
        }


static int pie(int gtype,char *type,char *data)
/* int gtype;  1=HBAR 2=%HBAR 3=MHBAR 4=%MHBAR */
        {
        int i;
        char otsikko[LLENGTH];

        strcpy(otsikko,type); strcat(otsikko," chart of ");
        strcat(otsikko,data);
        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(3); if (i<0) { p_end(); return(-1); }
        if (pr_type==1 || pr_type==2)
            { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
        i=datain(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        if (gtype==2)
            {
            i=prosentit(); if (i<0) { p_end(); data_close(&dat); return(-1); }
            }
        i=shading(em2-1); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=plot_pie(gtype); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=texts(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        if (pr_type!=1 && pr_type!=2) { i=frames(); if (i<0) { p_end(); data_close(&dat); return(-1); } }
        i=fills(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        i=polygons(); if (i<0) { p_end(); data_close(&dat); return(-1); }
        p_end(); data_close(&dat);
        return(1);
        }

static int plot_pie(int gtype)
        {
        int i,j,k,m,n;
        char x[LLENGTH], *psana[32];
        int maxm; /* max kuvia / rivi */
        int lev,kork,koko;
        double max,min;
        double a,b;
        double kulma0;
        int suunta;
        int rivi, sarake, rs;

        n=l2-l1+1;
        m=em2-1;

        i=barvalues(); if (i<0) return(-1);
        i=bar_labels(); if (i<0) return(-1);
        i=names(); if (i<0) return(-1);

        i=spfind("PLAN");
        if (i<0) autom_plan(n);
        else
            {
            int s=0;
            strcpy(x,spb[i]);
            nplan=split(x,psana,32);
            for (k=0; k<nplan; ++k) { mplan[k]=atoi(psana[k]); s+=mplan[k]; }
            if (s!=n) { sp_virhe(spa[i],spb[i]); return(-1); }
            }

        maxm=0; for (i=0; i<nplan; ++i) if (mplan[i]>maxm) maxm=mplan[i];
        lev=x_kuva/maxm; i=0.9*lev;
        kork=y_kuva/nplan; k=0.8*kork;
        koko=(i<k)? i:k;

        i=spfind("MAX");
        if (i<0)
            maxmin_sum(&max,&min);
        else
            max=atof(spb[i]);

        kulma0=0.0;
        suunta=1; /* vastapÑivÑÑn */
        i=spfind("ANGLE");
        if (i>=0)
            {
            if (*spb[i]=='-') suunta=-1;
            kulma0=fabs(atof(spb[i])*2*PI/360.0);
            }
        rivi=1; sarake=0; rs=0;

        i=p_pen(); if (i<0) return(-1);
        i=p_linetype(); if (i<0) return(-1);

        for (j=0; j<n; ++j)
            {
            int x1,y1; /* vasen alakulma */
            double kulma, kulma1, kulma2;
            double r,sum;
            int xr,yr,x_apu;

            sum=0.0; for (i=0; i<m; ++i) sum+=xmat[j*em+i];
            r=koko/2*sqrt(sum/max);

            ++rs; ++sarake; if (rs>mplan[rivi-1]) { rs=1; ++rivi; sarake=1; }
            x1=xx+(sarake-1)*lev;
            y1=yy+(nplan-rivi)*kork;
            if (mplan[rivi-1]<maxm) x1+=lev/2;
            x_apu=x1+lev/2-kirjainlev*strlen(xnimi[j])/2.0;

            if (name_ind)
                {
                char kopio2[LLENGTH];

                strcpy(kopio2,namecode);
                if (*namecode!=EOS)
                    { k=p_textcontrol(kopio2); if (k<0) return(-1); }
                p_text((unsigned char *)(xnimi[j]),x_apu,y1+tikki,1);
                }

            xr=x1+lev/2+kirjainkork; yr=y1+kork/2;

            kulma=kulma0;
            a=0;
            for (i=0; i<m; ++i)
              {
              double t;
              int xp,yp;
              double ry;

              p_linetype();
              ry=y_ratio*r;
              b=a+xmat[j*em+i];
              if (a==b) continue;
              kulma1=kulma;
              kulma=kulma2=kulma1+suunta*2*PI*b/sum;
              xp=xr; yp=yr;
              if (shadepull[i])
                {
                xp=xr+shadepull[i]/10.0*r*cos((kulma1+kulma2)/2);
                yp=yr+shadepull[i]/10.0*ry*sin((kulma1+kulma2)/2);
                }
              if (capability[1])
                {
                plot_sector(xp,yp,r,ry,kulma1,kulma2);
                t=(kulma1+kulma2)/2;
                p_fill((int)(xp+r/2*cos(t)),(int)(yp+ry/2*sin(t)),shadeval[i]);
                }
              else
                {
                p_fill_sector(xp,yp,r,ry,kulma1,kulma2,shadeval[i]);
                }
              if (*valform!=EOS)
                {
                char kopio[LLENGTH];
                double arvo;

                if (valpros)
                    {
                    arvo=0.0;
                    if (m==1)
                        {
                        for (k=0; k<n; ++k) arvo+=xmat[k*em+i];
                        arvo=100.0*xmat[j*em+i]/arvo;
                        }
                    else
                        {
                        for (k=0; k<m; ++k) arvo+=xmat[j*em+k];
                        arvo=100.0*xmat[j*em+i]/arvo;
                        }
                    }
                else arvo=xmat[j*em+i];

                strcpy(kopio,valcode);
                if (*valcode!=EOS)
                    { k=p_textcontrol(kopio); if (k<0) return(-1); }
                if (arvo>=valuemin) valtextpie(xp,yp,r,kulma1,kulma2,arvo,i);
   /* ry? */
                if (*valcode!=EOS)
                    { k=p_pen(); if (k<0) return(-1); }
                }

              if (labpaikka!=9999.9)
                {
                char kopio[LLENGTH];

                strcpy(kopio,labcode);
                if (*labcode!=EOS)
                    { k=p_textcontrol(kopio); if (k<0) return(-1); }
                labtextpie(xp,yp,r,kulma1,kulma2,xname[ev[i+1]],i);
                if (*labcode!=EOS)
                    { k=p_pen(); if (k<0) return(-1); }
                }
              if (capability[2]) p_textcolors(0); // 21.2.2012
              }  /* i */
            } /* j */
        legend((int)lev);
        return(1);
        }


static void autom_plan(int n)
        {
        int i,k;
        double am1, aa, ab, pl, as2, as; // RS REM , as1;

        am1=n; pl=1; aa=x_kuva; ab=y_kuva; as2=1e-10;
        while ((as=aa/am1/(ab/pl))<=1.0)
            {
            as2=as; ++pl; am1=1.0+floor((n-1)/pl);
            }
        if (as>=1.0/as2) { --pl; am1=1.0+floor((n-1)/pl); }
        nplan=pl;
        for (i=0; i<nplan; ++i) mplan[i]=am1;
        k=am1*nplan-n;
        if (k>0) for (i=nplan-k; i<nplan; ++i) --mplan[i];
/*
  Rprintf("\nPLAN="); for (i=0; i<nplan; ++i) Rprintf("%d ",mplan[i]); getch();
*/
        }

static void plot_sector(int xr,int yr,double rx,double ry,double a1,double a2)
        {
        double a;
        double delta=PI/40.0;

        if (a2<a1) { a=a1; a1=a2; a2=a; }
        p_line2(xr,yr,(int)(rx*cos(a1)+xr),(int)(ry*sin(a1)+yr),1);
        a=a1;
        while (a<a2)
            {
            a+=delta; if (a>a2) a=a2;
            p_line((int)(rx*cos(a)+xr),(int)(ry*sin(a)+yr),1);
            }
        p_line(xr,yr,1);
        }



static int lines()
        {
        int h,i,k;
        char x[LLENGTH];
        char *p; //, *ps;
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
//            if (spshad[i]==NULL) ps=NULL;
//            else { k=p-y; ps=spshad[i]+k; }
            nt=split(p,sana,40);

            p_path(nt,sana);   /* 24.6.1992 */
            }
        return(1);
        }


/*  contour.c 17.12.1988/SM (23.12.1988)
    PLOT surfaces as contour plots TYPE=CONTOUR
*/
static void muste_contour(int argc, char *argv[])
        {
        int i,k; // RS REM ,v;
        char laite[LLENGTH];
// RS REM        char gtype[16];

        if (argc==1) return;
        s_init(argv[1]);
        argv1=argv[1];
        
        if (strcmp(info,"CONTOUR")==0) { i=tutki_yhtalo_contour(); if (i<0) return; }

     	muste_gplot_init=1;
     	k=sp_init(r1+r-1);
     	muste_gplot_init=0;
        if (k<0)
            {
            sur_print("\n Too many specifications!");
            WAIT; return;
            }

        i=spfind("DEVICE");
        if (i<0) strcpy(laite,"MUSTE_PR.PS");
        else
            {
            strcpy(laite,spb[i]);
            if (strchr(laite,':')==NULL && *laite!='~' && *laite!='/' && *laite!='.' && *laite!='\\') // RS unix path FIXME
                {
                strcpy(laite,edisk);
                strcat(laite,spb[i]);
                }
            }

        if (strcmp(info,"CONTOUR")==0)
            {
            i=p_init(laite);
            if (i<0) return;
            if (capability[0])
                {
                i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
                if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);
                }
            contours();
            }
        else if (strcmp(info,"MATRIX")==0)
            {
            if (g<2) return;
            i=data_read_open(word[1],&d);
            if (i<0)  { p_error2(sbuf); s_end(argv[1]); return; }
            i=p_init(laite); if (i<0) return;
            matrix();
            data_close(&d); // 9.3.2001
            }
        edisp=1; s_end(argv[1]);
        }


static int tutki_yhtalo_contour()
        {
        char x[LLENGTH], *osa[2];
        int i,j;
        char *p,*q;

        j=r1+r-1;
        edread(x,j);
        p=strchr(x+1,'=');
        if (p==NULL) { missing_char('=',j); return(-1); }
        if (*(p-1)!=')') { missing_char(')',j); return(-1); }
        strcpy(lauseke,p+1);
        q=lauseke; while (*q!=' ') ++q; *q=EOS;
        q=p-1;
        *q=EOS;
        while (*q!='(' && q>x) --q;
        if (q==x) { missing_char('(',j); return(-1); }
        i=split(q+1,osa,2);
        if (i!=2)
            {
            sur_print("\nThe expression not a function of two variables!");
            WAIT; return(-1);
            }
        strcpy(muuttujanimi,osa[0]);
        strcpy(muuttujanimi2,osa[1]);
strcpy(muuttujanimi3,"x"); // RS ADD
strcpy(muuttujanimi4,"y"); // RS ADD
        return(1);
        }

static int contours()
        {
        int i;
        char otsikko[LLENGTH];

  /*    tee_otsikko(otsikko);   */
        strcpy(otsikko,"Contour plot");
        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
        if (pr_type==1 || pr_type==2)
         { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
        i=xyscale("X"); if (i<0) { p_end(); return(-1); }  // RS AXES allowed (no own xyscale-funktion for contour) FIXME?
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

spa[0]=muuttujanimi; // RS CHA spa[0]
spa[1]=muuttujanimi2; // RS CHA spa[1]

        i=plot_contours(); if (i<0) { p_end(); return(-1); }
        
        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1 && pr_type!=2) { i=frames(); if (i<0) { p_end(); return(-1); } }
        i=fills(); if (i<0) { p_end(); return(-1); }
        p_end();
        return(1);
        }

static int plot_contours()
        {
        int i; // RS REM ,k;
        double x,y,zarvo;
// RS REM        int xxx,yyy;
        double za,zb;
        char s[LLENGTH], *osa[2];
        int ix,iy;
        long pros,pros_step;
		pros=pros_step=0;
        i=plotting_range_contour(muuttujanimi,&x_start,&x_end,&x_step);
        if (i<0) return(-1);
        i=plotting_range_contour(muuttujanimi2,&y_start,&y_end,&y_step);
        if (i<0) return(-1);

/*
Rprintf("\n%g %g %g",x_start,x_end,x_step);
Rprintf("\nlauseke=%s!!!",lauseke); // getch();
Rprintf("\n%g %g %g",y_start,y_end,y_step);// getch();
for (i=0; i<spn; ++i)
    {
    Rprintf("\n%d %s %s",i,spa[i],spb[i]);
    }
//getch();
*/

        za=1.0; zb=0.0;
        i=spfind("ZSCALING");
        if (i>=0)
            {
            strcpy(s,spb[i]);
            i=split(s,osa,2);
            if (i>0) za=arit_atof(osa[0]);
            if (i>1) zb=arit_atof(osa[1]);
            }

        nx=0; for (x=x_start+x_step/2; x<x_end; x+=x_step) ++nx;
        ny=0; for (y=y_end-y_step/2; y>y_start; y-=y_step) ++ny;

        pxl_value=(int *)muste_malloc(nx*sizeof(int));
        if (pxl_value==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<spn; ++i) spb2[i]=spb[i];
        p_contour_init();
        if (capability[0])
        	{
        	sur_print("\nContour plotting: ");
        	pros=1; pros_step=(long)((double)ny/100.0);
        	}

        iy=0;
        for (y=y_end-y_step/2; y>y_start; y-=y_step)
            {
            ix=0;

//            sprintf(sbuf," %d/%d",iy,ny); sur_print(sbuf);            
            if (capability[0] && iy>=pros*pros_step)
                {
                sprintf(sbuf," %d%%",(int)pros); sur_print(sbuf); // RS ADD (int)
                ++pros;
                }
                                        
            for (x=x_start+x_step/2; x<x_end; x+=x_step)
                {
                memcpy(spb,spb2,spn*sizeof(char *));
                arvo[0]=x; arvo[1]=y; 
//                muste_save_stack_count();
                laske(lauseke,&zarvo);
//                muste_restore_stack_count();

//fprintf(temp2,"\nx=%g y=%g z=%g|",x,y,zarvo);
//Rprintf("\nx=%g y=%g z=%g|",x,y,zarvo);

//          Rprintf("\n%g %g %d",x,y,(int)(zarvo*za+zb));  // getch();

                pxl_value[ix]=256*(zarvo*za+zb);


    /*
        xxx=xx+x_kuva*(xmu(x)-xmumin)/(xmumax-xmumin);
        yyy=yy+y_kuva*(ymu(y)-ymumin)/(ymumax-ymumin);
    */

                ++ix;
                } /* x */
            p_contour_plot(ny,iy,nx,pxl_value);
/***********************************
            if (capability[0])
                { sprintf(sbuf,"%d/%d ",iy+1,ny); sur_print(sbuf); }
            if (kbhit())
                {
                i=getch(); if (i=='.') break;
                }
*************************************/
            ++iy;
            } /* y */

        return(1);
        }

static int plotting_range_contour(char *mnimi,double *t_start,double *t_end,double *t_step)
        {
        int i,k;
        char x[LLENGTH], *sana[3];
// RS REM        char *p;
        i=spfind2(mnimi,4); // RS CHA 3 -> 4
        if (i<0)
            {
            sprintf(sbuf,"Specification %s=<lower_limit>,<upper_limit>,<step> not found!",
                            mnimi);
            sur_print(sbuf); WAIT; return(-1);
            }
        strcpy(x,spb[i]);
        k=split(x,sana,3);
        if (k<2)
            {
            sur_print("\nEnter plotting range in form:");
            sprintf(sbuf,"\n%s=<lower_limit>,<upper_limit>,<step>",mnimi);
            sur_print(sbuf);
            WAIT; return(-1);
            }
        *t_start=arit_atof(sana[0]);
        *t_end=arit_atof(sana[1]);
        *t_step=(*t_end-*t_start)/100.0;
        if (k>2) *t_step=arit_atof(sana[2]);
        return(1);
        }

static void not_enough_memory() // RS CHA char *place)
        {
        sprintf(sbuf,"\nNot enough memory!"); // RS CHA (%s)",place);
        sur_print(sbuf); WAIT;
        }

static int matrix()
        {
        int i;
        char otsikko[LLENGTH];

/*      tee_otsikko(otsikko);  */
        strcpy(otsikko,"Matrix plot");
        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
        if (pr_type==1) { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
/*      i=xyscale("X"); if (i<0) { p_end(); return(-1); }
        i=xlabel(xmuunnos); if (i<0) { p_end(); return(-1); }
        i=xyscale2("X"); if (i<0) { p_end(); return(-1); }
        i=xyscale("Y"); if (i<0) { p_end(); return(-1); }
        i=ylabel(ymuunnos); if (i<0) { p_end(); return(-1); }
        i=xyscale2("Y"); if (i<0) { p_end(); return(-1); }
        i=xgrid(); if (i<0) { p_end(); return(-1); }
        i=ygrid(); if (i<0) { p_end(); return(-1); }
*/      i=xtick(1); if (i<0) { p_end(); return(-1); }
        i=ytick(1); if (i<0) { p_end(); return(-1); }
        i=xtick(2); if (i<0) { p_end(); return(-1); }
        i=ytick(2); if (i<0) { p_end(); return(-1); }

        i=plot_matrix(); if (i<0) { p_end(); return(-1); }

        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1) { i=frames(); if (i<0) { p_end(); return(-1); } }
        p_end();
        return(1);
        }


//        static int prind=1;
static int plot_matrix()
        {
        int i,k,erstat;
        int ix,iy;
        long j;
        double zarvo,min2,max2;
        char x[LLENGTH], *osa[3];
        char *p;
        double x_apu,y_apu,y_taso;
        double x_koko,y_koko;
        char nimi[LLENGTH];
        int inimi;
        int ntasot,nrivitasot;
        

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        i=tutki_data();

        miss_zarvo=0.0;
        i=spfind("MISSING");
        if (i>=0) miss_zarvo=atof(spb[i]);

        if (namevar<0) nimimax=8;
        *rowlabel_code=EOS; rowlabels=1; nrivitasot=1;
        i=spfind("ROWLABELS");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(rowlabel_code,x+i); ++p;
                }
            i=split(p,osa,3);
            rowlabels=atoi(osa[0]);
            if (i>1) { nrivitasot=atoi(osa[1]); if (nrivitasot<1) nrivitasot=1; }
            if (i>2) nimimax=atoi(osa[2]);
            }


        *columnlabel_code=EOS; columnlabels=1; ntasot=1;
        i=spfind("COLUMNLABELS");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(columnlabel_code,x+i); ++p;
                }
            i=split(p,osa,2);
            columnlabels=atoi(osa[0]);
            if (i>1) { ntasot=atoi(osa[1]); if (ntasot<1) ntasot=1; }
            }


/*
printf("\nrow: %s %d",rowlabel_code,rowlabels);
printf("\ncol: %s %d",columnlabel_code,columnlabels);
getch();
*/

        pxl_value=(int *)muste_malloc(nx*sizeof(int));
        if (pxl_value==NULL) { not_enough_memory(); return(-1); }

        p_contour_init();
        if (capability[0])
            sur_print("\nMatrix plotting: ");

        x_koko=xdiv2*(double)x_size;
        y_koko=ydiv2*(double)y_size;

        if (columnlabels)
            {
            i=p_pen();
            strcpy(x,columnlabel_code);
            k=p_textcontrol(x); if (k<0) return(-1);
            x_koko=xdiv2*(double)x_size;
            y_koko=ydiv2*(double)y_size;
            x_apu=xx; y_apu=yy+y_koko+tikki;

            k=0;
            for (i=0; i<nx; ++i)
                {
                y_taso=y_apu+(ntasot-1-k)*(kirjainkork+tikki);
                p_text((unsigned char *)(d.varname[d.v[i]]),(int)x_apu,(int)y_taso,1);
                x_apu+=x_koko/nx;
                ++k; if (k==ntasot) k=0;
                }
            }

        iy=0; inimi=0;
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;

            i=p_pen();
            switch (norm)
                {
              case 1:
                ix=0;
                for (i=0; i<nx; ++i)
                    {
                    erstat=data_load(&d,j,d.v[i],&zarvo);
                    if (erstat<0) return(-1); // RS 17.1.2013
                    if (zarvo==MISSING8) pxl_value[ix]=(int)(255*miss_zarvo);
                    else
                        pxl_value[ix]=(int)(255*(zarvo-min_arvo[i])/(max_arvo[i]-min_arvo[i]));
                    ++ix;
                    }
                break;

              case 2:
                min2=1e100; max2=-1e100;
                for (i=0; i<nx; ++i)
                    {
                    erstat=data_load(&d,j,d.v[i],&zarvo);
                    if (erstat<0) return(-1);
                    min_arvo[i]=zarvo;
                    if (zarvo==MISSING8) continue;
                    if (zarvo<min2) min2=zarvo;
                    if (zarvo>max2) max2=zarvo;
                    }
                ix=0;
                for (i=0; i<nx; ++i)
                    {
                    zarvo=min_arvo[i];
                    if (zarvo==MISSING8) pxl_value[ix]=(int)(255*miss_zarvo);
                    else
                        pxl_value[ix]=(int)(255*(zarvo-min2)/(max2-min2));
                    ++ix;
                    }
                break;

              case 3:
                ix=0;
                for (i=0; i<nx; ++i)
                    {
                    erstat=data_load(&d,j,d.v[i],&zarvo);
                    if (erstat<0) return(-1); // RS 17.1.2013
                    if (zarvo==MISSING8) pxl_value[ix]=(int)(255*miss_zarvo);
                    else
                        pxl_value[ix]=(int)(255*(zarvo-min_arvo[0])/(max_arvo[0]-min_arvo[0]));
                    ++ix;
                    }
                 break;

                 }

            if (rowlabels)
                {
                if (namevar>=0)
                    {
                    erstat=data_alpha_load(&d,j,namevar,nimi);
                    if (erstat<0) return(-1); // RS 17.1.2013
                    }
                else
                    {
                    sprintf(nimi,"%8ld",j);
                    }
                strcpy(x,rowlabel_code);
                k=p_textcontrol(x); if (k<0) return(-1);

                x_apu=xx-(int)((nrivitasot-inimi)*kirjainlev*(nimimax+1));
                y_apu=yy+y_koko-(iy+0.5)*y_koko/ny-kirjainkork/2;
                p_text((unsigned char *)nimi,(int)x_apu,(int)y_apu,1);
                ++inimi; if (inimi==nrivitasot) inimi=0;
                }

/*

        x_koko=xdiv2*(double)x_size; y_koko=ydiv2*(double)y_size;
        x_step=x_koko/nx; y_step=y_koko/ny;
        x_taso=xx;
        y_taso=yy+y_koko-(iy+1)*y_step;

*/


/*
            ytaso=(int)(yy+(n-1-j)*mm*lev+(n-j-1)*vali+vali1);

            x_apu=xx-(int)(kirjainlev*(nimimax+1));
            y_apu=ytaso+(int)(mm*lev/2.0-kirjainkork/2.0);

            p_text(xnimi[j],x_apu,y_apu,1);
*/


            p_contour_plot(ny,iy,nx,pxl_value);
            if (capability[0] && prind)
                { sprintf(sbuf,"%d/%d ",iy+1,ny); sur_print(sbuf); }
            if (sur_kbhit())
                {
                i=sur_getch(); if (i=='.') break;
                }
            ++iy;
            }
        return(1);
        }

static int tutki_data()
        {
        int i,erstat;
        long j;
        double a;
//        int k;
        char x[LLENGTH];

        i=mask(&d); if (i<0) return(-1);
        namevar=activated(&d,'L');
        if (namevar<0) namevar=d.v[0];
        if (d.vartype[namevar][0]!='S') namevar=-1;
        else
            {
            for (i=0; i<d.m_act; ++i)
                {
                if (i<=namevar) continue;
                d.v[i-1]=d.v[i];
                }
            --d.m_act;
            }
        i=conditions(&d); if (i<0) return(-1);
        nx=d.m_act;
/*
printf("\nnx=%d namevar=%d",nx,namevar);
for (i=0; i<nx; ++i) Rprintf(" %d",d.v[i]); getch();
*/
        norm=1; // k=nx;
        i=spfind("NORM");
        if (i>=0)
            {
            if (*spb[i]=='R') norm=2;
            if (*spb[i]=='T') norm=3;
            }

        min_arvo=(double *)muste_malloc(nx*sizeof(double));
        if (min_arvo==NULL) { not_enough_memory(); return(-1); }
        max_arvo=(double *)muste_malloc(nx*sizeof(double));
        if (max_arvo==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<nx; ++i) { min_arvo[i]=MISSING8; max_arvo[i]=-MISSING8; }

        ny=0; nimimax=1;
        sur_print("\nChecking data: ");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            for (i=0; i<nx; ++i)
                {
                erstat=data_load(&d,j,d.v[i],&a);
                if (erstat<0) return(-1); // RS 17.1.2013
                if (a==MISSING8) continue;
                if (a<min_arvo[i]) min_arvo[i]=a;
                if (a>max_arvo[i]) max_arvo[i]=a;
                }
            ++ny;
            if (namevar>=0)
                {
                erstat=data_alpha_load(&d,j,namevar,x);
                if (erstat<0) return(-1); // RS 17.1.2013
                i=strlen(x); while (i>0 && x[i-1]==' ') --i;
                if (nimimax<i) nimimax=i;
                }
            if (prind)
                {
                sprintf(sbuf,"%ld ",j); sur_print(sbuf);
                }
            }

        if (norm==3)
            {
            for (i=1; i<nx; ++i)
                {
                if (min_arvo[i]<min_arvo[0]) min_arvo[0]=min_arvo[i];
                if (max_arvo[i]>max_arvo[0]) max_arvo[0]=max_arvo[i];
                }
            }
        return(1);
        }





static void muste_faces(int argc, char *argv[])
        {
        int i,k,ii; // RS REM ,v;
        char laite[LLENGTH];
// RS REM        char gtype[16];
        char x[LLENGTH],*osa[3];

        if (argc==1) return;
        s_init(argv[1]);
        tut_init();
        argv1=argv[1];

     	muste_gplot_init=1;
     	k=sp_init(r1+r-1);
     	muste_gplot_init=0;
     	if (k<0)
            {
            sur_print("\n Too many specifications!");
            WAIT; return;
            }

        i=spfind("DEVICE");
        if (i<0) strcpy(laite,"MUSTE_PR.PS");
        else
            {
            strcpy(laite,spb[i]);
            if (strchr(laite,':')==NULL) // RS unix path FIXME
                {
                strcpy(laite,edisk);
                strcat(laite,spb[i]);
                }
            }
        if (g<2) return;
        i=data_read_open(word[1],&d);
        if (i<0) return;
        i=spfind("TYPE");
        strcpy(x,spb[i]);
        ii=split(x,osa,3);
        star_plot=0;
        andrews_polar=0;
        if (strcmp(osa[0],"ANDREWS")==0 && (ii>1 && strcmp(osa[1],"POLAR")==0))
            andrews_polar=1;
        if (strcmp(info,"FACES")==0)
            {
            i=init_faces(); if (i<0) { lopetus_faces(argv[1]); return; }
            i=p_init(laite); if (i<0) return;
            xdiv1=0.0; xdiv2=1.0; xdiv3=0.0;
            ydiv1=0.0; ydiv2=y_size-2*kirjainkork; ydiv3=2*kirjainkork;
            page_number=0;

            sprintf(otsikko,"Chernoff's faces:  %s    (Page ##)",word[1]);
            faces(otsikko);
            }
        else if (strcmp(info,"ANDREWS")==0)
            {
            i=init_andrews(); if (i<0) { lopetus_faces(argv[1]); return; }
            i=p_init(laite); if (i<0) return;
            xdiv1=xdiv3=3*kirjainlev; xdiv2=x_size-6*kirjainlev;
            ydiv1=ydiv3=2*kirjainkork; ydiv2=y_size-4*kirjainkork;
            if (andrews_polar)
                {
                xdiv1=0.0; xdiv2=1.0; xdiv3=0.0;
                ydiv1=0.0; ydiv2=y_size-2*kirjainkork; ydiv3=2*kirjainkork;
                if (ii>2) polar_constant=atof(osa[2]);
                }
            sprintf(otsikko,"Andrews' function plots:  %s",word[1]);
            if (andrews_polar)
                faces(otsikko);
            else
                andrews(otsikko);
            }
        else if (strcmp(info,"DRAFTS")==0)
            {
            i=init_drafts(); if (i<0) { lopetus_faces(argv[1]); return; }
            i=p_init(laite); if (i<0) return;

            xdiv1=xdiv3=3*kirjainlev; xdiv2=x_size-6*kirjainlev;
            ydiv1=ydiv3=2*kirjainkork; ydiv2=y_size-4*kirjainkork;

            sprintf(otsikko,"Draftsman's display:  %s",word[1]);
            drafts(otsikko);
            }
        else if (strcmp(info,"STARS")==0 || strcmp(info,"PROFILES")==0)
            {
            if (*info=='S')
                { star_plot=1; strcpy(x,"Star"); }
            else
                { star_plot=2; strcpy(x,"Profile"); }
            i=init_stars(); if (i<0) { lopetus_faces(argv[1]); return; }
            i=p_init(laite); if (i<0) return;
            xdiv1=0.0; xdiv2=1.0; xdiv3=0.0;
            ydiv1=0.0; ydiv2=y_size-2*kirjainkork; ydiv3=2*kirjainkork;
            page_number=0;

            sprintf(otsikko,"%s symbol plot:  %s    (Page ##)",x,word[1]);
            faces(otsikko);
            }
        lopetus_faces(argv[1]);
        }

static void lopetus_faces(char *argv1)
        {
        tut_end();
        edisp=1; s_end(argv1);
        }

static int xyscale_faces(char *suunta) /* "X" tai "Y" */
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
            if (*suunta=='X') strcpy(x,"-3.1416:-pi,0,3.1416:pi");
            else strcpy(x,"-2,-1,0,1,2");

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
            { sprintf(sbuf,"\n%sSCALE values missing!",suunta); sur_print(sbuf); WAIT; return(-1); }
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
            plot_xscale(xscalen,xscaleval,xscal,xx,yy,x_kuva);
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
            plot_yscale(yscalen,yscaleval,yscal,xx,yy,y_kuva);
            return(1);
            }
        return(1);
        }

static int faces(char *otsikko)
        {
        int i;

        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(0); if (i<0) { p_end(); return(-1); }
        if (pr_type==1 || pr_type==2)
         { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=fheader(otsikko); if (i<0) { p_end(); return(-1); }
/*      i=xyscale("X"); if (i<0) { p_end(); return(-1); }
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
*/
        if (andrews_polar)
            { i=plot_apolar(); if (i<0) { p_end(); return(-1); } }
        else if (star_plot)
            { i=plot_stars(); if (i<0) { p_end(); return(-1); } }
        else
            { i=plot_faces(); if (i<0) { p_end(); return(-1); } }

        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1 && pr_type!=2) { i=frames(); if (i<0) { p_end(); return(-1); } }
        p_end();
        return(1);
        }

static int fheader(char *otsikko)
        {
        char otsikko2[LLENGTH];
        char *p,*q;
        char sana[16];
        int ots;

        ++page_number;
        ots=spfind("HEADER");
        if (ots>=0) strcpy(otsikko,spb[ots]);

        strcpy(otsikko2,otsikko);
        p=strchr(otsikko2,'#');
        if (p!=NULL)
            {
            q=p;
            while (*q=='#') ++q;
            fnconv((double)page_number,(int)(q-p),sana);
            strncpy(p,sana,(int)(q-p));
            }

        if (ots>=0) strcpy(spb[ots],otsikko2);
        header(otsikko2);
        if (ots>=0) strcpy(spb[ots],otsikko);
        return(1);
        }





static int init_faces()
        {
        int i; // RS REM ,k;

        i=conditions(&d); if (i<0) return(-1);
        i=tutki_data_faces(); if (i<0) return(-1);
        i=tutki_lista(); if (i<0) return(-1);
        i=tutki_varit(i); if (i<0) return(-1);
        return(1);
        }


static int plot_faces()
        {
        int i,k,erstat;
        int flev,fkork;
        int row,col;
        long j;
        char x[LLENGTH],*osa[2];
        char label[LLENGTH];
        int x1,y1;
        int nx,ny;
        char *p;
        int select_color();

        flev=x_kuva/5;
        fkork=1.1*y_ratio*flev;

        row=col=0;
        i=spfind("FSIZE");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,osa,2);
            flev=arit_atoi(osa[0]); fkork=1.1*y_ratio*flev;
            if (i>1) fkork=arit_atoi(osa[1]);
            }
        nx=x_kuva/flev; ny=y_kuva/fkork;
        namevar=-1;
        i=spfind("LABEL");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            *flabelcode=EOS;
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(flabelcode,x+i); ++p;
                }
            namevar=varfind(&d,p);
            }

        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            for (i=0; i<NVARFACES; ++i)
                {
                if (v[i]==-1) continue;
                erstat=data_load(&d,j,v[i],&val[i]);
                if (erstat<0) return(-1); // RS 17.1.2013
                if (val[i]==MISSING8) break;
                }
            if (i<NVARFACES) continue;
            for (i=0; i<NVARFACES; ++i)
                {
                if (v[i]==-1) y[i]=(fmin_faces[i]+fmax_faces[i])/2;
                else
                    y[i]=fmin_faces[i]+(fmax_faces[i]-fmin_faces[i])*(val[i]-min[i])/(max[i]-min[i]);
                }

            if (namevar==-1) sprintf(label,"%ld",j);
            else 
            	{
            	erstat=data_alpha_load(&d,j,namevar,label);
            	if (erstat<0) return(-1); // RS 17.1.2013
            	}

            x1=xx+col*flev;
            y1=yy+y_kuva-(row+1)*fkork;

            p_pen();
            strcpy(x,flabelcode);
            i=p_textcontrol(x);
            p_text((unsigned char *)label,(int)(x1+kirjainlev),y1,1);
            p_linetype();

            plot_face(y,x1,(int)(y1+0.05*fkork),flev,fkork,j,select_color);

            if (j==d.l2) break;
            ++col;
            if (col>=nx)
                {
                col=0;
                ++row;

                if (row>=ny)
                    {
                    if (capability[2]==0) return(1);

                    row=0;
                    i=p_wait(); if (i<0) { return(1); }
           //       p_clear();
                    p_newpage();
                    fheader(otsikko);

                    }

                }
            }
        return(1);
        }

static int tutki_data_faces()
        {
        int i,m,erstat;
        long j;
        double x;

        m=d.m;
        minx=(double *)muste_malloc(m*sizeof(double));
        if (minx==NULL) { not_enough_memory(); return(-1); }
        maxx=(double *)muste_malloc(m*sizeof(double));
        if (maxx==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<m; ++i) { minx[i]=1e100; maxx[i]=-1e100; }

        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            for (i=0; i<m; ++i)
                {
                erstat=data_load(&d,j,i,&x);
                if (erstat<0) return(-1); // RS 17.1.2013
                if (x==MISSING8) continue;
                if (x<minx[i]) minx[i]=x;
                if (x>maxx[i]) maxx[i]=x;
                }
            }
        return(1);
        }

static int tutki_lista()
        {
        int i,j,k;
        char x[LLENGTH],*osa[6];
        char *p;

        j=r1+r;
        while (j<=r2)
            {
            edread(x,j);
            i=split(x+1,osa,1);
            if (i==1 && strncmp(osa[0],"VARIABLES",9)==0) break;
            ++j;
            }

        if (j>r2)
            {
/**************************************************************
            sur_print("\nList for VARIABLES not found!");
            sur_print("\nA ready-made list can be loaded in the edit field.");
            sur_print("\nDo you like to have the list (Y/N) ?");
            i=nextch("");
            if (i=='Y' || i=='y')
***************************************************************/
                {
                j=r1+r-1;
                if (j+36>r2)
                    {
                    sur_print("\nNot enough lines in the edit field!");
                    WAIT; return(-1);
                    }

                for (i=0; i<36; ++i)
                    {
                    ++j;
                    edwrite(space,j,1);
                    edwrite(list[i],j,1);
                    }


                }
            return(-1);
            }

        for (i=0; i<NVARFACES; ++i)
            {
            ++j;
            edread(x,j);
            k=split(x+1,osa,6);
            if (k<6)
                {
                sprintf(sbuf,"Invalid line %d in the list of VARIABLES!",j);
                p_error2(sbuf);
                return(-1); // RS ADD
                }

            fmin_faces[i]=atof(osa[4]); fmax_faces[i]=atof(osa[5]);

            if (strcmp(osa[0],"-")!=0)
                {
                k=varfind2(&d,osa[0],0);
                if (k<0)
                    {
                    sprintf(sbuf,"Error on line %d: Variable %s not in data %s!",
                                        j,osa[0],word[1]);
                    p_error2(sbuf);
                    return(-1); // RS ADD
                    }
                v[i]=k;
                }
            else
                {
                v[i]=-1;
                }

            if (v[i]==-1) continue;
            p=strchr(osa[1],'*');
            if (p==NULL) min[i]=atof(osa[1]);
            else
                {
                k=1; if (*(p+1)=='*') k=2;
                if (k==1) min[i]=minx[v[i]]; else min[i]=maxx[v[i]];
                edread(x,j);
                minmaxkorvaa(x,osa[1],k,min[i]);
                edwrite(x,j,0);
                }
            p=strchr(osa[2],'*');
            if (p==NULL) max[i]=atof(osa[2]);
            else
                {
                k=1; if (*(p+1)=='*') k=2;
                if (k==1) max[i]=minx[v[i]]; else max[i]=maxx[v[i]];
                edread(x,j);
                minmaxkorvaa(x,osa[2],k,max[i]);
                edwrite(x,j,0);
                }
            }
        return(j);
        }

static void minmaxkorvaa(char *x,char *p,int k,double a)
        {
        char y[LLENGTH];
        char sana1[LLENGTH];
        char sana2[LLENGTH];
        char *q;
        int len;
// RS REM        int minmax;

        q=p; while (*q!=' ') { *q=' '; ++q; }
        while (*q==' ') ++q;
        len=q-p-k-1;
        fnconv(a,len,y);
        q=y; while (*q==' ') ++q;
        strcpy(sana1,q);
        fconv(a,"",sana2);
        q=sana2; if (strlen(sana1)<strlen(sana2)) q=sana1;
        while (*q) *p++=*q++;
        *p='*'; if (k==2) *(p+1)='*';
        }



static int tutki_varit(int j)
        {
        int i,k,h;
        char x[LLENGTH],*osa[NRAJAT+2];
        char sana[LLENGTH];
        char *p,*q;

        for (i=0; i<NV; ++i) vv[i]=-1;
        ++j;
        edread(x,j);
        split(x+1,osa,1);
        if (muste_strnicmp(osa[0],"COLORS",6)!=0) return(1);
        while (j<r2)
            {
            ++j;
            edread(x,j);
            k=split(x+1,osa,NRAJAT+2);
            if (muste_strnicmp(osa[0],"END",3)==0) return(1);
            for (i=0; i<NV; ++i)
                {
                if (muste_strnicmp(osa[0],piirre[i],lpiirre[i])==0) break;
                }
            if (i==NV || k<2)
                {
                sprintf(sbuf,"Error in COLORS list: line %d",j);
                p_error2(sbuf);
                return(-1);
                }
            if (strcmp(osa[1],"-")==0)
                {
                if (k<3) continue;
                vv[i]=-2; vcolor[i][0]=arit_atoi(osa[2]);  /* vakiovÑri */
                continue;
                }

            vv[i]=varfind2(&d,osa[1],0);
            if (vv[i]<0)
                {
                sprintf(sbuf,"Error in COLORS list: line %d: Variable %s not found!",j,osa[1]);
                p_error2(sbuf);
                return(-1); // RS ADD
                }
            if (k<3)
                {
                sprintf(sbuf,"Error in COLORS list: line %d",j);
                p_error2(sbuf);
                return(-1); // RS ADD
                }
            for (h=0; h<k-2; ++h)
                {
                strcpy(sana,osa[h+2]);
                p=strchr(sana,':');
                if (p==NULL)
                    {
                    sprintf(sbuf,"Error in COLORS list: line %d: ':' missing in %s",j,sana);
                    p_error2(sbuf);
                    return(-1); // RS ADD
                    }
                *p=EOS; ++p;
                vraja[i][h]=atof(sana);
                q=strchr(p,'F');
                if (q==NULL) vfill[i][h]=-999; else { *q=EOS; vfill[i][h]=arit_atoi(q+1); }
                vcolor[i][h]=arit_atoi(p);
                }
            nv[i]=k-2;
            }
        return(1);
        }

static int select_color(long j,int i,int *pfill)
/* long j;   hav.nro */
/* int i;    piirre */
        {
        int h,erstat;
        double x;

        *pfill=-999;
//      *pfill=-1;
        if (vv[i]==-1) return(-1);
        if (vv[i]==-2) return(vcolor[i][0]);
        erstat=data_load(&d,j,vv[i],&x);
        if (erstat<0) return(-1); // RS 17.1.2013
        if (x==MISSING8) return(-1);
        for (h=0; h<nv[i]; ++h)
            {
            if (x<=vraja[i][h]) { *pfill=vfill[i][h]; return(vcolor[i][h]); }
            }
        return(-1);
        }

static void plot_face(
double *y,
int x1,
int y1,
int fkork,
int flev,
long j,
int (* scolor)()
)
        {
        double x0,y0;
        int vari;
        int line_color2;
        int fill;
        int xk,yk;

        x0=y[0]*cos(-y[1]); y0=y[0]*sin(-y[1]);
        q1=(y[3]*x0*y[3]*x0+(y0-y[2])*(y0-y[2]))/(2*(y[2]-y0)*y[3]);
        q2=y[3]*q1; q3=y[2]-q2;
        t0=atan((y0-q3)/(y[3]*x0)); u=PI-t0;
        ts=(u-t0)/20;
        vari=scolor(j,0,&fill); line_color2=line_color;
        if (vari>=0) { line_color=vari; p_lineattr(); }
        curve_plot(1,x1,y1,flev,fkork);
        q1=(y[4]*x0*y[4]*x0+(y0+y[2])*(y0+y[2]))/(2*(y[2]+y0)*y[4]);
        q2=y[4]*q1; q3=q2-y[2];
        t0=PI-atan((y0-q3)/(y[4]*x0)); u=3*PI-t0;
        ts=(u-t0)/20;
        curve_plot(1,x1,y1,flev,fkork);
        if (fill>-999) p_floodfill((int)(x1+flev/2),(int)(y1+fkork/2),fill);
        if (vari>=0) { line_color=line_color2; p_lineattr(); }

        vari=scolor(j,5,&fill); line_color2=line_color;
        if (vari>=0) { line_color=vari; p_lineattr(); }
        t0=-y[5]/2; u=-t0; q1=0; q2=1; q3=0; q4=0; ts=u-t0;
        curve_plot(2,x1,y1,flev,fkork);  /* Nose */
        if (vari>=0) { line_color=line_color2; p_lineattr(); }

        vari=scolor(j,4,&fill); line_color2=line_color;
        if (vari>=0) { line_color=vari; p_lineattr(); }
        if (fabs(y[7])<0.01)  /* Mouth */
            {
            q1=1; q2=0; q3=-y[6]; q4=0; t0=-y[8]/2; u=-t0; ts=u-t0;
            curve_plot(2,x1,y1,flev,fkork);
            }
        else
            {
            q6=y[6];
            qr=1/y[7];
            t0=-fabs(qr)*sin(y[8]/(2*fabs(qr)));
            u=-t0; ts=(u-t0)/10;
            curve_plot(3,x1,y1,flev,fkork);
            }
        if (vari>=0) { line_color=line_color2; p_lineattr(); }

        vari=scolor(j,1,&fill); line_color2=line_color;
        if (vari>=0) { line_color=vari; p_lineattr(); }
        q1=y[10]/2; q2=y[13]; q3=y[9]; q4=y[12]*y[13]; q5=y[11];
        t0=0; u=2*PI; ts=(u-t0)/16;
        curve_plot(4,x1,y1,flev,fkork);  /* Right eye */
        if (fill>-999)
            {
            koordinaatit(q1,q3,x1,y1,flev,fkork,&xk,&yk);
            p_floodfill(xk,yk,fill);
            }
        q1=-q1; q2=-q2;
        curve_plot(4,x1,y1,flev,fkork);  /* Left eye */
        if (fill>-999)
            {
            koordinaatit(q1,q3,x1,y1,flev,fkork,&xk,&yk);
            p_floodfill(xk,yk,fill);
            }
        if (vari>=0) { line_color=line_color2; p_lineattr(); }

        vari=scolor(j,3,&fill); line_color2=line_color;
        if (vari>=0) { line_color=vari; p_lineattr(); }
        q1=q1+y[14]; q2=q2/5; q4=q2; ts=(u-t0)/8;
        curve_plot(4,x1,y1,flev,fkork);  /* Left pupil */
        q1=y[10]/2+y[14];
        curve_plot(4,x1,y1,flev,fkork);  /* Right pupil */
        if (vari>=0) { line_color=line_color2; p_lineattr(); }

        vari=scolor(j,2,&fill); line_color2=line_color;
        if (vari>=0) { line_color=vari; p_lineattr(); }
        q1=cos(y[16]); q2=sin(y[16]); q4=-y[10]/2; q3=y[9]+y[15];
        t0=-y[17]/2; u=-t0; ts=u-t0;
        curve_plot(2,x1,y1,flev,fkork);  /* Left eyebrow */
        q1=-q1; q4=-q4;
        curve_plot(2,x1,y1,flev,fkork);  /* Right eyebrow */
        if (vari>=0) { line_color=line_color2; p_lineattr(); }
        }

static void curve_plot(int g,int x1,int y1,int flev,int fkork)
        {
        double t;
        int xk,yk,xk2,yk2;

        t=t0;
        laske_f(g,t);
        koordinaatit(xco,yco,x1,y1,flev,fkork,&xk,&yk);
        while (1)
            {
            t+=ts;
            if (t>u) t=u;
            laske_f(g,t);
            koordinaatit(xco,yco,x1,y1,flev,fkork,&xk2,&yk2);
            p_line2(xk,yk,xk2,yk2,1);
            xk=xk2; yk=yk2;
            if (t==u) break;
            }
        }

static void laske_f(int g,double t)
        {
        switch (g)
            {
          case 1:
            xco=q1*cos(t); yco=q2*sin(t)+q3;
            break;
          case 2:
            xco=q1*t+q4; yco=q2*t+q3;
            break;
          case 3:
            xco=t; yco=sgn_faces(qr)*(fabs(qr)-sqrt(fabs(qr*qr-xco*xco)))-q6;
            break;
          case 4:
            xco=q1+q2*cos(t); yco=q3+q4*sin(t+q5);
            break;
            }
        }

static int sgn_faces(double x)
        {
        if (x>0.0) return(1);
        if (x<0.0) return(-1);
        return(0);
        }

static void koordinaatit(double x,double y,int x1,int y1,int flev,int fkork,int *pxk,int *pyk)
        {
        *pxk=x1+(scale*x+0.5)*flev;
        *pyk=y1+(scale*y+0.5)*fkork;
        }

static int andrews(char *otsikko)
        {
        int i;

        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
        if (pr_type==1) { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
        i=xyscale_faces("X"); if (i<0) { p_end(); return(-1); }
        i=xlabel(xmuunnos); if (i<0) { p_end(); return(-1); }
        i=xyscale2("X"); if (i<0) { p_end(); return(-1); }
        i=xyscale_faces("Y"); if (i<0) { p_end(); return(-1); }
        i=ylabel(ymuunnos); if (i<0) { p_end(); return(-1); }
        i=xyscale2("Y"); if (i<0) { p_end(); return(-1); }
        i=xgrid(); if (i<0) { p_end(); return(-1); }
        i=ygrid(); if (i<0) { p_end(); return(-1); }
        i=xtick(1); if (i<0) { p_end(); return(-1); }
        i=ytick(1); if (i<0) { p_end(); return(-1); }
        i=xtick(2); if (i<0) { p_end(); return(-1); }
        i=ytick(2); if (i<0) { p_end(); return(-1); }

        i=plot_andrews(); if (i<0) { p_end(); return(-1); }

        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1) { i=frames(); if (i<0) { p_end(); return(-1); } }
        p_end();
        return(1);
        }

static int init_andrews()
        {
        int i; // RS REM ,k;

        i=conditions(&d); if (i<0) { p_error2(sbuf); return(-1); }
        i=tutki_data2(); if (i<0) return(-1);
        i=tutki_lista2(); if (i<0) return(-1);

        return(1);
        }

static int plot_andrews()
        {
        int i,k,erstat;
        long j;
        double t[3];
        char x[LLENGTH],*osa[3];
        char label[LLENGTH];
        char *p;
        char t_code[LLENGTH],label_code[LLENGTH];
        int lab_step=30,lab_move=4;
        int first_label;

        *t_code=EOS;
        t[0]=xmin; t[1]=xmax; t[2]=(xmax-xmin)/100;
        i=spfind("T");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(t_code,x+i); ++p;
                }
            i=split(p,osa,3);
            t[0]=arit_atof(osa[0]); t[1]=arit_atof(osa[1]);
            t[2]=(t[1]-t[0])/100;
            if (i==3) t[2]=arit_atof(osa[2]);
            }
        lab_step=0;
        i=spfind("LABEL");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            *label_code=EOS;
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(label_code,x+i); ++p;
                }
            i=split(p,osa,3);
            namevar=varfind(&d,osa[0]);
            if (namevar<0) return(-1);
            lab_step=30,lab_move=4;
            if (i>1) lab_step=arit_atoi(osa[1]);
            if (i>2) lab_move=arit_atoi(osa[2]);
            first_label=4;
            }

        for (j=d.l1; j<=d.l2; ++j)
            {
            if (sur_kbhit())
                { i=sur_getch(); if (i=='.') return(1); }
            if (unsuitable(&d,j)) continue;
            for (i=0; i<na; ++i)
                {
                if (v[i]==-1) continue;
                erstat=data_load(&d,j,v[i],&yf[i]);
                if (erstat<0) return(-1); // RS 17.1.2013
                if (yf[i]==MISSING8) break;
                }
            if (i<na) continue;
            for (i=0; i<na; ++i)
                {
                if (v[i]==-1) yf[i]=0.0;
                else
                    yf[i]=(yf[i]-aa[i])/bb[i];
                }
            if (namevar<0) sprintf(label,"%ld",j);
            else 
            	{
            	erstat=data_alpha_load(&d,j,namevar,label);
            	if (erstat<0) return(-1); // RS 17.1.2013
            	}
            plot_andrews_curve(yf,na,t,t_code,label,label_code,lab_step,first_label);
            first_label+=4; if (first_label>lab_step) first_label=lab_move;
            }

        return(1);
        }

static int tutki_data2()
        {
        int i,m,erstat;
        long j;
        double x;

        m=d.m;
        mean=(double *)muste_malloc(m*sizeof(double));
        if (mean==NULL) { not_enough_memory(); return(-1); }
        stddev=(double *)muste_malloc(m*sizeof(double));
        if (stddev==NULL) { not_enough_memory(); return(-1); }
        n=(long *)muste_malloc(m*sizeof(long));
        if (n==NULL) { not_enough_memory(); return(-1); }

        for (i=0; i<m; ++i) { mean[i]=0.0; stddev[i]=0.0; n[i]=0L; }
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            for (i=0; i<m; ++i)
                {
                erstat=data_load(&d,j,i,&x);
                if (erstat<0) return(-1); // RS 17.1.2013
                if (x==MISSING8) continue;
                ++n[i];
                mean[i]+=x;
                stddev[i]+=x*x;
                }
            }

        for (i=0; i<m; ++i)
            {
            if (n[i]<2L) continue;
            mean[i]/=(double)n[i];
            stddev[i]=sqrt((stddev[i]-n[i]*mean[i]*mean[i])/(n[i]-1));
            }
        return(1);
        }

static int tutki_lista2()
        {
        int i,j,k;
        char x[LLENGTH],*osa[3];
        char *p;
        double ascale,bscale;

        j=r1+r;
        while (j<=r2)
            {
            edread(x,j);
            i=split(x+1,osa,1);
            if (i==1 && strncmp(osa[0],"VARIABLES",9)==0) break;
            ++j;
            }
        if (j>r2)
            {
/**************************************
            sur_print("\nList for VARIABLES not found!");
            sur_print("\nA ready-made list can be loaded in the edit field.");
            sur_print("\nDo you like to have the list (Y/N) ?");
            i=nextch("");
            if (i=='Y' || i=='y')
***************************************/
                {
                j=r1+r-1;
                if (j+14>r2)
                    {
                    sur_print("\nNot enough lines in the edit field!");
                    WAIT; return(-1);
                    }
                for (i=0; i<14; ++i)
                    {
                    ++j;
                    edwrite(space,j,1);
                    edwrite(andrew_list2[i],j,1);
                    }
                }
            return(-1);
            }

        ascale=0.0; bscale=0.0;
        i=spfind("FSCALING");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            i=split(x,osa,2);
            ascale=arit_atof(osa[0]);
            if (i>1) bscale=arit_atof(osa[1]);
            }
        for (i=0; i<AMAX; ++i)
            {
            ++j;
            edread(x,j);
            k=split(x+1,osa,3);
            if (muste_strcmpi(osa[0],"END")==0) break;

            if (k<3)
                {
                sprintf(sbuf,"Invalid line %d in the list of VARIABLES!",j);
                p_error2(sbuf);
                return(-1); // RS ADD
                }

            if (strcmp(osa[0],"-")!=0)
                {
                k=varfind2(&d,osa[0],0);
                if (k<0)
                    {
                    sprintf(sbuf,"Error on line %d: Variable %s not in data %s!",
                                        j,osa[0],word[1]);
                    p_error2(sbuf);
                    return(-1); // RS ADD
                    }
                v[i]=k;
                }
            else
                {
                v[i]=-1;
                }

            if (v[i]==-1) continue;
            if (bscale!=0.0) { aa[i]=ascale; bb[i]=bscale; }
            else
                {
                p=strchr(osa[1],'*');
                if (p==NULL) aa[i]=atof(osa[1]);
                else
                    {
                    aa[i]=mean[v[i]];
                    edread(x,j);
                    minmaxkorvaa(x,osa[1],1,aa[i]);
                    edwrite(x,j,0);
                    }
                p=strchr(osa[2],'*');
                if (p==NULL) bb[i]=atof(osa[2]);
                else
                    {
                    bb[i]=stddev[v[i]];
                    edread(x,j);
                    minmaxkorvaa(x,osa[2],1,bb[i]);
                    edwrite(x,j,0);
                    }
                }
            }
        na=i;
        return(1);
        }

static void plot_andrews_curve(
double *yf,
int na,
double tt[],
char *t_code,
char *label,
char *label_code,
int lab_step,
int first_label
)
        {
// RS REM        int i;
        double t,f;
//        extern double andrews_function();
        int xk1,yk1,xk2,yk2;
        char s[LLENGTH];
        int nstep;

        nstep=0;
        strcpy(s,t_code);
        p_linecontrol(s);
        t=tt[0];
        f=andrews_function(yf,na,t);
        koord2(t,f,&xk1,&yk1);
        while (t<tt[1])
            {
            t+=tt[2];
            if (t>=tt[1]) t=tt[1];
            f=andrews_function(yf,na,t);
            koord2(t,f,&xk2,&yk2);
            p_line2(xk1,yk1,xk2,yk2,1);
            xk1=xk2;
            yk1=yk2;
            if (lab_step)
                {
                ++nstep;
                if (nstep==first_label)
                    {
                    strcpy(s,label_code);
                    p_textcontrol(s);
                    strcpy(sbuf,label); // koodimuunnon vuoksi!
                    p_text((unsigned char *)sbuf,x_pos,y_pos,1);
                    strcpy(s,t_code);
                    p_linecontrol(s);
                    }
                if (nstep>=lab_step) nstep=0;
                }
            }
        }

static void koord2(double x,double y,int *pxk,int *pyk)
        {
        *pxk=xx+x_kuva*(x-xmin)/(xmax-xmin);
        *pyk=yy+y_kuva*(y-ymin)/(ymax-ymin);
        }

static double andrews_function(double *yf,int na,double t)
        {
        double f;
        int i,k;

        f=0.0;
        if (na==0) return(f);

        f=yf[0]/sqrt(2.0);
        if (na==1) return(f);
        k=1; i=0;
        while (1)
            {
            ++i;
            if (i==na) break;
            f+=yf[i]*sin((double)(k*t));
            ++i;
            if (i==na) break;
            f+=yf[i]*cos((double)(k*t));
            ++k;
            }
        return(f/na);
        }

static int plot_apolar()
        {
        int i,k,erstat;
        int flev,fkork;
        int row,col;
        long j;
        char x[LLENGTH],*osa[3];
        char label[LLENGTH];
        int x1,y1;
        int nx,ny;
        char *p;
        double t[3];
        char t_code[LLENGTH]; // RS REM ,label_code[LLENGTH];
// RS REM        double a;

        flev=x_kuva/5;
        fkork=1.1*y_ratio*flev;
        row=col=0;
        i=spfind("FSIZE");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,osa,2);
            flev=arit_atoi(osa[0]); fkork=1.1*y_ratio*flev;
            if (i>1) fkork=arit_atoi(osa[1]);
            }
        nx=x_kuva/flev; ny=y_kuva/fkork;
        namevar=-1;
        i=spfind("LABEL");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            *flabelcode=EOS;
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(flabelcode,x+i); ++p;
                }
            namevar=varfind(&d,p);
            }
        t[0]=0.0; t[1]=2*3.14159265; t[2]=(t[1]-t[0])/100;  /* 10.7.1992 */
        *t_code=EOS;
        i=spfind("T");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(t_code,x+i); ++p;
                }
            i=split(p,osa,3);
            t[0]=arit_atof(osa[0]); t[1]=arit_atof(osa[1]);
            t[2]=(t[1]-t[0])/100;
            if (i==3) t[2]=arit_atof(osa[2]);
            }

        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            for (i=0; i<na; ++i)
                {
                if (v[i]==-1) continue;
                erstat=data_load(&d,j,v[i],&yf[i]);
                if (erstat<0) return(-1); // RS 17.1.2013
                if (yf[i]==MISSING8) break;
                }
            if (i<na) continue;
            for (i=0; i<na; ++i)
                {
                if (v[i]==-1) yf[i]=0.0;
                else
                    yf[i]=(yf[i]-aa[i])/bb[i];
                }

            if (namevar==-1) sprintf(label,"%ld",j);
            else 
            	{
            	erstat=data_alpha_load(&d,j,namevar,label);
            	if (erstat<0) return(-1);
            	}

            x1=xx+col*flev;
            y1=yy+y_kuva-(row+1)*fkork;

            if (*flabelcode)
                {
                strcpy(x,flabelcode);
                i=p_textcontrol(x);
                }
            else
                p_pen();


            strcpy(sbuf,label); // koodimuunnon vuoksi!
            p_text((unsigned char *)sbuf,x1,y1,1);

            if (*t_code)
                {
                strcpy(x,t_code);
                i=p_linecontrol(x);
                }
            else p_linetype();
            plot_apolar_curve(yf,na,t,x1,(int)(y1+0.05*fkork),flev,fkork);

            if (j==d.l2) break;
            ++col;
            if (col>=nx)
                {
                col=0;
                ++row;
                if (row>=ny)
                    {
                    if (capability[2]==0) return(1);
                    row=0;
                    i=p_wait(); if (i<0) { return(1); }
                //  p_clear();
                    p_newpage();
                    fheader(otsikko);
                    }

                }
            }
        return(1);
        }

static void plot_apolar_curve(
double *yf,
int na,
double tt[],
int x1,
int y1,
int flev,
int fkork
)
        {
        double t;
//        double andrews_function();
        double r;
        double xd,yd;
        int xp1,yp1,xp2,yp2;

        t=tt[0];
        r=polar_constant+andrews_function(yf,na,t);
        xd=r*cos(t);
        yd=r*sin(t);
        koordinaatit(xd,yd,x1,y1,flev,fkork,&xp1,&yp1);

        while (t<tt[1])
            {
            t+=tt[2]; if (t>tt[1]) t=tt[1];
            r=polar_constant+andrews_function(yf,na,t);
            xd=r*cos(t);
            yd=r*sin(t);
            koordinaatit(xd,yd,x1,y1,flev,fkork,&xp2,&yp2);
            p_line2(xp1,yp1,xp2,yp2,1);
            xp1=xp2;
            yp1=yp2;
            }
        }

static int drafts(char *otsikko)
        {
        int i;

        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(0); if (i<0) { p_end(); return(-1); }
        if (pr_type==1) { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
/*      i=xyscale("X"); if (i<0) { p_end(); return(-1); }
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
*/
        i=plot_drafts(); if (i<0) { p_end(); return(-1); }

        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1) { i=frames(); if (i<0) { p_end(); return(-1); } }
        p_end();
        return(1);
        }

static int init_drafts()
        {
        int i; // RS REM ,k;

        i=conditions(&d); if (i<0) return(-1);
        i=mask(&d); if (i<0) return(-1);
        scales(&d);
        i=mask_sort(&d); if (i<0) return(-1);
        i=tutki_data3(); if (i<0) return(-1);

        prind=1;
        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND");
        if (i>=0) prind=atoi(spb[i]);

        return(1);
        }

static int plot_drafts()
        {
        int i,k,erstat;
        long j;
        double a;
        int xp,yp;
        char s[LLENGTH];

        plot_dboxes();
        i=draft_point(); if (i<0) return(-1);

        outscale(dmin,dmax,jitter_step);
        for (i=0; i<m; ++i)
            {
            a=dmax[i]-dmin[i];
            dmax[i]+=0.05*a;
            dmin[i]-=0.05*a;
            }

        if (!point_given || *point_code==EOS)
            p_linetype();
        else
            {
            strcpy(s,point_code); p_linecontrol(s);
            }

// Rprintf("\nfillcolor: %d",fill_color);  

        if (!point_size_varying && (point_var>=0 || *point_text))
            { xpp=(int)(-kirjainlev/2.0); ypp=(int)(-kirjainkork/2.0); }
        else xpp=ypp=0;

        if (capability[0])
            sur_print("\nPlotting observations...");

        for (j=d.l1; j<=d.l2; ++j)
            {
            if (sur_kbhit())
                {
                i=sur_getch();
                if (i=='.') return(1);
                }
            if (unsuitable(&d,j)) continue;
            for (i=0; i<m; ++i)
                { 
                erstat=data_load(&d,j,d.v[i],&draval[i]); 
                if (erstat<0) return(-1); // RS 17.1.2013 
                }
            if (jitter)
                {
                for (i=0; i<m; ++i)
                    {
                    if (nval[i] && draval[i]!=MISSING8)
                        draval[i]+=jitter_step[i]*(0.5-uniform_faces((double)rand_seed));
                    }
                }
p_marker_color(line_color);                
            for (i=0; i<m; ++i)
                {
                if (draval[i]==MISSING8) continue;
                for (k=0; k<m; ++k)
                    {
                    if (laajuus==2 && i<k) continue;
                    if (laajuus==1 && i>k) continue;
                    if (k==i || draval[k]==MISSING8) continue;

                    xp=xcorner[i]+dxsize*(draval[i]-dmin[i])/(dmax[i]-dmin[i]);
                    yp=ycorner[k]+dysize_muste*(draval[k]-dmin[k])/(dmax[k]-dmin[k]);
                    if (!point_given)
                        p_marker(xp,yp);
                    else
                        draft_merkitse(j,xp+xpp,yp+ypp);
                    }
                }
            if (capability[0] && prind)
                {
                sprintf(sbuf," %ld",j); sur_print(sbuf);
                }
            }
        return(1);
        }

static void plot_dboxes()
        {
        int i,j;
        char s[LLENGTH],*osa[2];

        laajuus=3;
        i=spfind("TYPE");
        if (i>=0)
            {
            strcpy(s,spb[i]); i=split(s,osa,2);
            if (i>1)
                {
                if (muste_strcmpi(osa[1],"UPPER")==0) laajuus=2;
                if (muste_strcmpi(osa[1],"LOWER")==0) laajuus=1;
                }
            }
        dxsize=x_kuva/m; dxgap=dxsize/20; dxsize-=dxgap;
        dysize_muste=y_kuva/m; dygap=dysize_muste/20; dysize_muste-=dygap;

        for (i=0; i<m; ++i)
            {
            xcorner[i]=xx+i*(dxsize+dxgap);
            ycorner[i]=yy+(m-1-i)*(dysize_muste+dygap);
            }

        p_linetype();
        for (i=0; i<m; ++i)
            for (j=0; j<m; ++j)
                {
                if (i==j) continue;
                if (laajuus==2 && i<j) continue;
                if (laajuus==1 && i>j) continue;
                plot_box(xcorner[i],ycorner[j],dxsize,dysize_muste);
                }
        p_pen();
        for (i=0; i<m; ++i)
            {
            strncpy(s,d.varname[d.v[i]],8); s[8]=EOS;
            p_text((unsigned char *)s,xcorner[i],(int)(ycorner[i]+0.5*(dysize_muste-kirjainkork)),1);
            }
        }

static int tutki_data3()
        {
        int i,erstat;
        long j;
        double x;
        char s[LLENGTH],*osa[2];

        m=d.m_act;
        dmin=(double *)muste_malloc(m*sizeof(double));
        if (dmin==NULL) { not_enough_memory(); return(-1); }
        dmax=(double *)muste_malloc(m*sizeof(double));
        if (dmax==NULL) { not_enough_memory(); return(-1); }
        draval=(double *)muste_malloc(m*sizeof(double));
        if (draval==NULL) { not_enough_memory(); return(-1); }
        xcorner=(int *)muste_malloc(m*sizeof(int));
        if (xcorner==NULL) { not_enough_memory(); return(-1); }
        ycorner=(int *)muste_malloc(m*sizeof(int));
        if (ycorner==NULL) { not_enough_memory(); return(-1); }

        nval=(int *)muste_malloc(m*sizeof(double));
        if (nval==NULL) { not_enough_memory(); return(-1); }
        jitter_step=(double *)muste_malloc(m*sizeof(double));
        if (jitter_step==NULL) { not_enough_memory(); return(-1); }
        for (i=0; i<m; ++i) { nval[i]=0; jitter_step[i]=0.0; }
        rand_seed=1;

        jitter=0;
        i=spfind("JITTER");
        if (i>=0)
            {
            strcpy(s,spb[i]);
            if (*s==EOS) strcpy(s,"10"); /* 29.9.1996 */
            i=split(s,osa,2);
            jitter=arit_atoi(osa[0]);
            if (jitter==0) jitter=10; /* 29.9.1996 */
            if (i>1) rand_seed=arit_atoi(osa[1]);
            if (m*jitter>8191) jitter=10;
            values=(double *)muste_malloc(m*jitter*sizeof(double));
            if (values==NULL) { not_enough_memory(); return(-1); }
            }

        for (i=0; i<m; ++i) { dmin[i]=1e100; dmax[i]=-1e100; }
        if (capability[0]) sur_print("\nScanning observations...");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            if (capability[0] && prind) { sprintf(sbuf," %ld",j); sur_print(sbuf); }
            for (i=0; i<m; ++i)
                {
                erstat=data_load(&d,j,d.v[i],&x);
                if (erstat<0) return(-1); // RS 17.1.2013
                if (x==MISSING8) continue;
                if (x<dmin[i]) dmin[i]=x;
                if (x>dmax[i]) dmax[i]=x;
                if (jitter) aseta_luokitus(i,x);
                }
            }

        insc=inscale(dmin,dmax,jitter_step,nval);  if (insc<0) return(-1);

        for (i=0; i<m; ++i)
            {
            if (dmin[i]==dmax[i])
                {
                sprintf(sbuf,"Variable %.8s is a constant %g",d.varname[d.v[i]],dmin[i]);
                p_error2(sbuf);
                return(-1);
                }
            if (dmin[i]==1e100)
                {
                sprintf(sbuf,"No acceptable observations in variable %s!",d.varname[d.v[i]]);
                p_error2(sbuf);
                return(-1);
                }
            }

        if (!insc && jitter)
            {
            jitter_steps();
            for (i=0; i<m; ++i) { dmin[i]-=jitter_step[i]; dmax[i]+=jitter_step[i]; }
            }
        return(1);
        }

static void aseta_luokitus(int i,double x)
        {
        double *px,*px0;
        int k,h;

        if (nval[i]<0) return;
        if (nval[i]==0)
            {
            values[i*jitter]=x;
            nval[i]=1; return;
            }
        px=px0=&values[i*jitter];
        k=0;
        while (k<nval[i])
            {
            if (x==*px) return;
            if (x<*px)
                {
                if (nval[i]==jitter) { nval[i]=-1; return; }
                for (h=nval[i]-1; h>=k; --h) px0[h+1]=px0[h];
                *px=x; ++nval[i];  return;
                }
            ++px; ++k;
            }
        if (nval[i]==jitter) { nval[i]=-1; return; }
        *px=x; ++nval[i];
        }

static void jitter_steps()
        {
        int i;
        double *px;

        for (i=0; i<m; ++i)
            {
            if (nval[i]<=1) { nval[i]=0; jitter_step[i]=0.0; continue; }
            px=&values[i*jitter];
            jitter_step[i]=(px[nval[i]-1]-px[0])/(double)(nval[i]-1);
            }
        }

static double uniform_faces(double x)
        {
        time_t ltime;
        unsigned int *pi;
        static int next=0;

        if (x==0.0)
            {
            time(&ltime);
            pi=(unsigned int *)&ltime;
            srand(*pi+rand()); rand();  /* 1. luku aina pieni!!! */
            }
        else
            {
            if (next) return(RND);
            if (x!=1.0) { srand((unsigned int)(x)); rand(); }
            next=1;
            }
        return(RND);
        }

/*
POINT=<point_var>
POINT=<point_text>
POINT=<marker_type>,<marker_size>
POINT=<marker_type>,<marker_size>,<point_var>,<point_max>
*/
static int draft_point()
        {
        int i,k;
        char x[LLENGTH], *osa[4];
        char *p;
// RS REM        char nimi[16];

        point_given=0;
        point_var=-1;
        *point_text=EOS;
        point_size_varying=0;
        marker_size=0;
        i=spfind("POINT");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            *point_code=EOS;
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(point_code,x+i); ++p;
                }
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            k=split(p,osa,4);
            point_var=varfind2(&d,osa[0],0);
                if (point_var>=0) { point_given=1; return(1); }
            if (*osa[0]!='0' && atoi(osa[0])==0)
                {
                strncat(point_text,osa[0],15); point_given=1; return(1);
                }
            marker_type=1; /* marker_size=10; 8.8.88 */
            if (k>0) marker_type=atoi(osa[0]);
            if (k>1) marker_size=atoi(osa[1]);
            if (k>3)
                {
                point_var=varfind(&d,osa[2],1); if (point_var<0) return(-1);
                point_max=atof(osa[3]);
                if (point_max<0.0) return(-1);
                point_size_varying=1;
                }
            point_given=1;
            }
        p_marker_select(marker_type,marker_size);
        return(1);
        }

static int draft_merkitse(long j,int x,int y)
        {
        int i;
        char label[LLENGTH];
        double a;
        int size;

        if (point_size_varying)
            {
            i=data_load(&d,j,point_var,&a);
            if (i<0) return(-1); // RS 17.1.2013
            if (a==MISSING8) a=0.0;
            if (point_max>0.0)
                {
                size=marker_size*a/point_max;
                p_marker_select(marker_type,size);
                }
            else  /* point_max=0.0: point_var:n arvo suoraan merkin nro. */
                {
                p_marker_select((int)a,marker_size);
                }
            p_marker(x,y);
            return(1);
            }

        if (point_var>=0)
            {
            if (d.vartype[point_var][0]=='S')
            	{
                i=data_alpha_load(&d,j,point_var,label);
                if (i<0) return(-1); // RS 17.1.2013
                }
            else
                {
                i=data_load(&d,j,point_var,&a);
                if (i<0) return(-1); // RS 17.1.2013
                fconv(a,"",label);
                }
            i=strlen(label); while(label[i-1]==' ') label[--i]=EOS;
            p_text((unsigned char *)label,x,y,1);
            }
        else if (*point_text)
            { p_text((unsigned char *)point_text,x,y,1); x_pos=x; y_pos=y; }
        else p_marker(x,y);
        return(1);
        }


static int outscale(double *dmin,double *dmax,double *jitter_step)
        {
        int i,m;
        char nimi[LLENGTH];

        i=spfind("OUTSCALE");
        if (i<0) return(1);

        m=d.m_act;
        strcpy(nimi,spb[i]);
        if (strchr(nimi,':')==NULL) // RS unix path FIXME
        { strcpy(nimi,edisk); strcat(nimi,spb[i]); }
        scalefile=fopen(nimi,"wt");
        if (scalefile==NULL)
            {
            sprintf(sbuf,"Cannot open file %s!",nimi);
            p_error2(sbuf);
            return(-1);
            }
        fprintf(scalefile,"Ranges and jitter steps for variables in %s:\n",word[1]);
        for (i=0; i<m; ++i)
            fprintf(scalefile,"%.8s %g %g %g\n",d.varname[d.v[i]],dmin[i],dmax[i],jitter_step[i]);
        muste_fclose(scalefile);
        return(1);
        }

static int inscale(double *dmin,double *dmax,double *jitter_step,int *nval)
        {
        int i,m,h; // k
        char nimi[LLENGTH];
        char x[LLENGTH],*osa[4];
        double min,max;

        i=spfind("INSCALE");
        if (i<0) return(0);

        m=d.m_act;
        strcpy(nimi,spb[i]);
        if (strchr(nimi,':')==NULL) // RS unix path FIXME
        { strcpy(nimi,edisk); strcat(nimi,spb[i]); }
        scalefile=fopen(nimi,"rt");
        if (scalefile==NULL)
            {
            sprintf(sbuf,"Cannot open file %s!",nimi);
            p_error2(sbuf);
            return(-1); // RS ADD
            }

        fgets(x,100,scalefile);  /* otsikko */

        while (1)
            {
            fgets(x,100,scalefile);
            if (feof(scalefile)) break;
            i=split(x,osa,4);
            i=varfind2(&d,osa[0],0);
            if (i<0) break;
            for (h=0; h<m; ++h)
                {
                if (d.v[h]==i)
                    {
                    min=atof(osa[1]); max=atof(osa[2]);
                    if (min<dmin[h]) dmin[h]=min;
                    if (max>dmax[h]) dmax[h]=max;
                    jitter_step[h]=atof(osa[3]);
                    if (jitter_step[h]!=0.0) { nval[h]=1; jitter=1; }
                    else nval[h]=0;
                    break;
                    }
                }
            }
        muste_fclose(scalefile);
        return(1);
        }



static int init_stars()
        {
        int i; // RS REM ,k;

        i=conditions(&d); if (i<0) return(-1);
        i=mask(&d); if (i<0) return(-1);
        scales(&d);
        i=mask_sort(&d); if (i<0) return(-1);
        i=tutki_data_faces(); if (i<0) return(-1);
        m=d.m_act;
        staval=(double *)muste_malloc(m*sizeof(double));
        if (staval==NULL) { not_enough_memory(); return(-1); }

        return(1);
        }

static int plot_stars()
        {
        int i,k,erstat;
        int flev,fkork;
        int row,col;
        long j;
        char x[LLENGTH],*osa[2];
        char label[LLENGTH];
        int x1,y1;
        int nx,ny;
        char *p;
        int select_color();

        flev=x_kuva/5;
        fkork=1.1*y_ratio*flev;
        row=col=0;
        i=spfind("FSIZE");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,osa,2);
            flev=arit_atoi(osa[0]); fkork=1.1*y_ratio*flev;
            if (i>1) fkork=arit_atoi(osa[1]);
            }
        nx=x_kuva/flev; ny=y_kuva/fkork;
        namevar=-1;
        i=spfind("LABEL");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=etsi_loppusulku(x,&p); if (k<0) return(-1);
            *flabelcode=EOS;
            if (p!=x)
                {
                if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
                if (*x=='(') i=1; else i=0;
                *(p-i)=EOS;
                strcpy(flabelcode,x+i); ++p;
                }
            namevar=varfind(&d,p);
            }

        dc=0.2;
        i=spfind("TYPE");
        if (i>=0)
            {
            strcpy(x,spb[i]); i=split(x,osa,2);
            if (i>1) dc=atof(osa[1]);
            }

        if (star_plot==1)
            {
            t0=0.0; u=2*PI*(double)(m-1)/m; ts=2*PI/m;
            for (i=0; i<m; ++i)
                if (minx[d.v[i]]==maxx[d.v[i]]) ++maxx[d.v[i]];
            }
        else
            {
            for (i=0; i<m; ++i)
                {
                if (-minx[d.v[i]]>maxx[d.v[i]]) maxx[d.v[i]]=-minx[d.v[i]];
                if (maxx[d.v[i]]==0.0) maxx[d.v[i]]=1.0;
                }
            }
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            for (i=0; i<m; ++i)
                {
                erstat=data_load(&d,j,d.v[i],&staval[i]);
                if (erstat<0) return(-1); // RS 17.1.2013
                if (staval[i]==MISSING8) break;
                }
            if (i<m) continue;

            if (star_plot==1)
                for (i=0; i<m; ++i)
                    staval[i]=(1.0-dc)*(staval[i]-minx[d.v[i]])/(maxx[d.v[i]]-minx[d.v[i]])+dc;
            else
                for (i=0; i<m; ++i)
                    staval[i]/=maxx[d.v[i]];

            if (namevar==-1) sprintf(label,"%ld",j);
            else 
            	{
            	erstat=data_alpha_load(&d,j,namevar,label);
            	if (erstat<0) return(-1); // RS 17.1.2013
            	}

            x1=xx+col*flev;
            y1=yy+y_kuva-(row+1)*fkork;

            p_pen();
            strcpy(x,flabelcode);
            i=p_textcontrol(x);
            p_text((unsigned char *)label,(int)(x1+kirjainlev),y1,1);
            p_linetype();
            if (star_plot==1)
                plot_star(staval,x1,(int)(y1+0.05*fkork),flev,fkork,j,select_color);
            else
        plot_profile(staval,(int)(x1-0.35*flev),(int)(y1+1.5*kirjainkork),(int)(0.8*flev),
                     (int)(0.8*fkork),j,select_color);

            if (j==d.l2) break;
            ++col;
            if (col>=nx)
                {
                col=0;
                ++row;
                if (row>=ny)
                    {
                    if (capability[2]==0) return(1);
                    row=0;
                    i=p_wait(); if (i<0) { return(1); }
              //    p_clear();
                    p_newpage();
                    fheader(otsikko);
                    }

                }
            }
        return(1);
        }

static void plot_star(
double *y,
int x1,
int y1,
int fkork,
int flev,
long j,
int (* scolor)()
)
        {
        int i;
// RS REM        int vari;
// RS REM        int line_color2;
// RS REM        int fill;
        int xk,yk,xk2,yk2,xk0,yk0;
        double t;
        double r=1.0;

        t=t0; i=0;
        koordinaatit(y[i]*r*cos(t),y[i]*r*sin(t),x1,y1,flev,fkork,&xk,&yk);
        p_line2((int)(x1+flev/2),(int)(y1+fkork/2),xk,yk,1);
        xk0=xk; yk0=yk;
        while (1)
            {
            t+=ts; ++i;
            if (t>u) t=u;
            koordinaatit(y[i]*r*cos(t),y[i]*r*sin(t),x1,y1,flev,fkork,&xk2,&yk2);
            p_line2((int)(x1+flev/2),(int)(y1+fkork/2),xk2,yk2,1);
            p_line2(xk,yk,xk2,yk2,1);

            xk=xk2; yk=yk2;
            if (t==u) break;
            }
        p_line2(xk,yk,xk0,yk0,1);
        }

static void plot_profile(
double *y,
int x1,
int y1,
int fkork,
int flev,
long j,
int (* scolor)()
)
        {
        int i;
// RS REM        int vari;
// RS REM        int line_color2;
// RS REM        int fill;
        int xk,yk,xk2,yk2,xk0;

        koordinaatit(0.0,y[0],x1,y1,flev,fkork,&xk,&yk);
        xk0=xk;
        p_line2(xk,y1,xk,yk,1);
        for (i=1; i<m; ++i)
            {
            koordinaatit(0.0,y[i],x1,y1,flev,fkork,&xk2,&yk2);
            xk2=xk+flev/(m-1);
            p_line2(xk,yk,xk2,yk2,1);
            xk=xk2; yk=yk2;
            }
        p_line2(xk,yk,xk,y1,1);
        p_line2(xk,y1,xk0,y1,1);
        }


static void muste_pdia(int argc, char *argv[])
        {
        int i,k; // RS REM ,v,ind;
        char laite[LLENGTH];
// RS REM        char gtype[16];
        char x[LLENGTH];
        char *p;

        if (argc==1) return;
        
strcpy(muuttujanimi,"t"); // RS ADD        
        s_init(argv[1]);
// RS REM        s_opt(argv[2]);

     	muste_gplot_init=1;
     	k=sp_init(r1+r-1);
     	muste_gplot_init=0;
        if (k<0)
            {
            sur_print("\n Too many specifications!");
            WAIT; return;
            }
        argv1=argv[1];
        strcpy(aineisto,word[1]);
        i=data_open3(aineisto,&d,0,1,0,0); if (i<0) return;
        i=mask(&d); if (i<0) return;
        i=conditions(&d); if (i<0) { s_end(argv[1]); return; }

/* RS REM restriction checking
        v=0;
        i=optdim_d(); if (i && i<d.m) v=1;
        k=optdim_o(); if (k && (long)k<d.n) v=1;
        if (v) p_error("Data too large for this test version!");
*/

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        i=spfind("PRIND");
        if (i>=0) prind=atoi(spb[i]);


        strcpy(x,word[2]); tvar=-1; aika=0;
        if (strncmp(x,"TIME",4)==0 && (x[4]=='(' || x[4]==EOS) )
            {
            aika=1;
            xvar=-1;
            if (x[4]=='(')
                {
                p=strchr(x,')');
                if (p==NULL)
                    {
                    sprintf(sbuf,"\n) missing in %s",word[2]); sur_print(sbuf);
                    WAIT; return;
                    }
                *p=EOS;
                tvar=varfind2(&d,x+5,0);
                if (tvar<0)
                    {
                    var_error(x+5);
                    return;
                    }
                }
            }
        else
            {
            xvar=varfind2(&d,word[2],0);
            if (xvar<0)
                {
                var_error(word[2]);
                return;
                }
            }

        nyvar=1;
        if (muste_strcmpi(word[3],"PROBIT")==0)
            {
            normal=1; yvar=yvars[0]=-1;
            }
        else
            {
            normal=0;

            nyvar=0;
            for (i=3; i<g; ++i)
                {
                if (nyvar>=NYVAR)
                    {
                    sprintf(sbuf,"Too many variables! (max=%d)",NYVAR);
                    p_error(sbuf);
                    return;
                    }
                k=varfind2(&d,word[i],0);
                if (k<0)
                    {
                    var_error(word[i]);
                    return;
                    }
                yvars[nyvar++]=k;
                }
            yvar=yvars[0];
            }
        i=spfind("DEVICE");
        if (i<0) strcpy(laite,"MUSTE_PR.PS");
        else
            {            
            strcpy(laite,spb[i]);
            if (strchr(laite,':')==NULL) // RS unix path FIXME
                {
                strcpy(laite,edisk);
                strcat(laite,spb[i]);
                }
            }
        i=p_init(laite); if (i<0) return;

        diagrams();
        data_close(&d); // RS ADD 3.10.2012
        edisp=1; s_end(argv[1]);
        }

static int var_error(char *s)
    {
    sprintf(sbuf,"Variable %s not found!",s);
    if (etu==2)
        {
        sprintf(tut_info,"˛˛˛@1@PDIA@%s@",sbuf); s_end(argv1);
        return(1);
        }
    sur_print("\n"); sur_print(sbuf);
    WAIT; return(1);
    }


static int diagrams()
        {
        int i;
        char otsikko[LLENGTH];

        tee_otsikko_dia(otsikko);
        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
        if (pr_type==1 || pr_type==2)
         { i=frames(); if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
        i=xyscale_dia("X"); if (i<0) { p_end(); return(-1); }
        if (!aika) tee_label(otsikko,xvar,xmuunnos);
        else       tee_label(otsikko,tvar,xmuunnos);
        i=xlabel(otsikko); if (i<0) { p_end(); return(-1); }
        i=xyscale2_dia("X"); if (i<0) { p_end(); return(-1); }
        i=xyscale_dia("Y"); if (i<0) { p_end(); return(-1); }
        tee_label(otsikko,yvar,ymuunnos);
        i=ylabel(otsikko); if (i<0) { p_end(); return(-1); }
        i=xyscale2_dia("Y"); if (i<0) { p_end(); return(-1); }
        i=xgrid(); if (i<0) { p_end(); return(-1); }
        i=ygrid(); if (i<0) { p_end(); return(-1); }
        i=xtick(1); if (i<0) { p_end(); return(-1); }
        i=ytick(1); if (i<0) { p_end(); return(-1); }
        i=xtick(2); if (i<0) { p_end(); return(-1); }
        i=ytick(2); if (i<0) { p_end(); return(-1); }
        
        init_trend();
        init_contour();
        init_conf_band(); // 1.2.2004

        i=plot_diagram(); if (i<0) { p_end(); return(-1); }
        i=plot_trend(); if (i<0) { p_end(); return(-1); }
        i=plot_contour(); if (i<0) { p_end(); return(-1); }
        i=plot_conf_band(conf_band[1]); if (i<0) { p_end(); return(-1); }
        i=plot_conf_band(conf_band[2]); if (i<0) { p_end(); return(-1); }
        i=plot_conf_band(conf_band[3]); if (i<0) { p_end(); return(-1); }

        i=plot_arrows(); if (i<0) { p_end(); return(-1); } // 6.10.2009

        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1 && pr_type!=2)
         { i=frames(); if (i<0) { p_end(); return(-1); } }
        i=fills(); if (i<0) { p_end(); return(-1); }
        i=diafill();
        p_end();
        return(1);
        }

static void tee_otsikko_dia(char *ots)
        {
        strcpy(ots,"Diagram of ");
        strcat(ots,aineisto);
        }

static void tee_label(char *ots,int var,char *muunnos)
        {
        int i;

        *ots=EOS;
        if (var<0) return;
        strncat(ots,d.varname[var],8);
        i=8; while (ots[i-1]==' ' && i>0) ots[--i]=EOS;
        if (*muunnos)
            {
            strcat(ots," ("); strcat(ots,muunnos); strcat(ots,")");
            }

        }

static int xyscale_dia(char *suunta) /* "X" tai "Y" */
        {
        extern double arit_atof();
        int i,k;
        char x[LLENGTH], *osa[2];
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
        else if (*suunta=='Y' && normal)
            {
            strcpy(x,"*probit(y),0.001,0.01,0.1,0.5,0.9,0.99,0.999");
            }
        else
            {
            double min,max;
            int nro;

            if (aika && *suunta=='X')
                {
                xmin=min=d.l1; xmax=max=d.l2;
                }
            else
                {
                if (*suunta=='X') nro=xvar; else nro=yvar;
                strcpy(x,d.varname[nro]);
                q=NULL; p=strchr(x,'{'); if (p!=NULL) q=strchr(p+1,'}'); // RS 4.2.2013 q=NULL
                if (p==NULL || q==NULL)
                    {
                    if (!rajat_etsitty) { i=etsi_rajat(suunta); if (i<0) return(-1); }
                    if (*suunta=='X') { min=xmin; max=xmax; }
                    else              { min=ymin; max=ymax; }
                    }
                else
                    {
                    *q=EOS;
                    i=split(p+1,osa,2); /* 19.4.1999 */
                    min=atof(osa[0]); max=atof(osa[1]);
                    }
                }
            if (*suunta=='X') k=x_kuva/kirjainlev;
            else              k=2*y_kuva/kirjainkork;
            i=autom_scale(x,min,max,k); if (i<0) return(-1);

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
            { sprintf(sbuf,"%sSCALE values missing!",suunta);
              p_error(sbuf);
              return(-1);
            }
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
            if (aika) plot_tscale_dia(xscalen,xscaleval,xscal,xx,yy+scalemove_y,x_kuva,1);
            else plot_xscale(xscalen,xscaleval,xscal,xx,yy+scalemove_y,x_kuva);
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

static int etsi_rajat(char *suunta) /* 15.4.90 */
        {
        long j;
        double x;
        int i;
        int havaintoja=0;

        if (capability[0]) sur_print("\nSearching for limits...");
        if (!aika && *suunta=='X') { xmin=1e300; xmax=-1e300; }
        ymin=1e300; ymax=-1e300;

        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            havaintoja=1;
            if (!aika && *suunta=='X')
                {
                i=data_load(&d,j,xvar,&x); if (i<0) return(-1);
                if (x!=MISSING8)
                    {
                    if (x<xmin) xmin=x;
                    if (x>xmax) xmax=x;
                    }
                }
            if (yvar>=0)
                {
                i=data_load(&d,j,yvar,&x); if (i<0) return(-1);
                if (x!=MISSING8)
                    {
                    if (x<ymin) ymin=x;
                    if (x>ymax) ymax=x;
                    }
                }
            if (capability[0] && prind) { sprintf(sbuf," %ld",j); sur_print(sbuf); }
            }

        if (!havaintoja || xmin==1e300 || (yvar>=0 && ymin==1e300))
            {
            sur_print("\nNo acceptable observations!");
            WAIT; s_end(argv1);  return(-1);
            }
        rajat_etsitty=1;
        return(1);
        }

static void plot_tscale_dia(
int n,              /* arvojen lkm */
double value[],     /* skaala-arvot */
char *label[],      /* skaalanimet */
int x0,
int y0,          /* alkupiste */
int pituus,         /* asteikon pituus */
int laji /* 1=XSCALE 2=XSCALE2 */
)
        {
        extern double xmu();
        int i,erstat;
// RS REM        double min,max;
        int x1;
        char lab[LLENGTH];
        long t;
        double a;

        if (!frametype && !scalemove_y) return;
        for (i=0; i<n; ++i)
            {
            t=value[i];
            x1=xx+(int)((xmu(value[i])-xmumin)/(xmumax-xmumin)*pituus);
            if (tvar>=0)
                {
                if (t<1L || t>d.n) strcpy(lab,label[i]);
                else if (*label[i]==EOS) *lab=EOS;   /* 20.2.1995 */
                else
                    {
                    if (d.vartype[tvar][0]=='S')
                    	{
                    	erstat=data_alpha_load(&d,t,tvar,lab);
                    	if (erstat<0) return; // RS 17.1.2013
                        }
                    else
                        {
                        erstat=data_load(&d,t,tvar,&a);
                        if (erstat<0) return; // RS 17.1.2013
                        if (a==MISSING8) *lab=EOS;   /* 20.2.1995 */
                        else fconv(a,"",lab);
                        }
                    }
                }

            else
                strcpy(lab,label[i]);
            if (laji==1)
                {
                if (tikki) p_line2(x1,y0,x1,y0-2*tikki,1);
                p_text((unsigned char *)lab,x1,y0-2*tikki-(int)(1.2*kirjainkork),1);
                }
            else
                {
                if (tikki) p_line2(x1,y0,x1,y0+2*tikki,1);
                p_text((unsigned char *)lab,x1,y0+2*tikki+(int)(0.2*kirjainkork),1);
                }
            }
        }


static int plot_diagram()
        {
        int i,k;
        int x,y,x2,y2,xorg,yorg;
        long j;
        int iy;
        int xpoint,ypoint;
        char v[LLENGTH];
        int step=1; // 22.11.2001

        i=spfind("MISSLINE"); if (i>=0) missline=atoi(spb[i]);  /* 26.9.93 */

        step=1;
        i=spfind("STEP");
        if (i>=0) { step=atoi(spb[i]); }


// fprintf(temp2,"\nline=%d|",line);
// muste_fclose(temp2);

        for (iy=0; iy<nyvar; ++iy)
            {
            if (iy>0) yvar=yvars[iy];
            i=sp_line(yvar); if (i<0) return(-1);

            if (line==10) // LINE=POLYGON,fill
                {
                sprintf(sbuf,"%sPOLYGON.TMP",etmpd);
                temp_poly=fopen(sbuf,"wb");
                n_poly=0;
                }

            i=sp_point(yvar); if (i<0) return(-1);
            if (line==6 || line==7) { i=points2(); if (i<0) return(-1); }
            if (line==8 || line==9) { i=points2(); if (i<0) return(-1); } // RS 
            

            if (!point_size_varying && (point_var>=0 || *point_text))
                { xp=(int)(-kirjainlev/2.0); yp=(int)(-kirjainkork/2.0); }
            else xp=yp=0;
        for (i=0; i<13; ++i) { x_thick[i]*=thickgap; y_thick[i]*=thickgap; }
            i=sp_lag(); if (i<0) return(-1);

            if (normal) { i=normal_check(); if (i<0) return(-1); }

            for (i=0; i<spn; ++i) spb2[i]=spb[i];

            if (l_virhe) return(-1);

            jitter=0; xjitter=yjitter=-1.0; // 17.3.2002
            i=spfind("XJITTER");
            if (i>=0) { ++jitter; xjitter=atof(spb[i]); }
            i=spfind("YJITTER");
            if (i>=0) { ++jitter; yjitter=atof(spb[i]); }

            if (jitter)
                {
                spec_rnd();
                }

            if (capability[0]) sur_print("\nPlotting observations...");

            for (i_thick=0; i_thick<thickness; ++i_thick)
                {
                out=2; j=d.l1;
                coord_dia(j,&x_pos,&y_pos);
                i_normal=0L;  /* laskuri kumul.frekv. varten */
                missing=1; obs_found=0;
                for (j=d.l1; j<=d.l2; j+=step) // 22.11.2001
                    {
                    prev_missing=missing;
                    if (line!=10 && unsuitable(&d,j)) { missing=1; continue; }
                    i=coord_dia(j,&x,&y); if (!obs_found) { x_pos=x; y_pos=y; }
                   if (trend || contour || conf_band[1] || conf_band[2]
                                        || conf_band[3] )
                       compute_moments(xxx,yyy);
                    if (i>0)
                        {
                        if (capability[0] && prind)
                           { sprintf(sbuf," %ld",j); sur_print(sbuf); }
                        switch (line)
                            {
                          case 0:
                            merkitse(j,x+xp,y+yp); break;
                          case 1:
                          default:
                            if (!prev_missing || (missline && obs_found))
                               p_line(x,y,1);
                            else { x_pos=x; y_pos=y; }
                            if (point_given) merkitse(j,x+xp,y+yp);
                            break;
                          case 2:
                          case 3:
                            if (!prev_missing || (missline && obs_found))
                                {
                                p_line(x,y_pos,1);
                                if (line==3)
                                    p_line(x,y,1);
                                }
                            x_pos=x; y_pos=y;
                            if (point_given) merkitse(j,x+xp,y+yp);
                            break;
                          case 4:
                          case 5:
                            if (!prev_missing || (missline && obs_found))
                                {
                                p_line(x_pos,y,1);
                                if (line==5)
                                    p_line(x,y,1);
                                }
                            x_pos=x; y_pos=y;
                            if (point_given) merkitse(j,x+xp,y+yp);
                            break;
                          case 6:
                          case 7:
                            if (line==7)
             { if (!prev_missing || (missline && obs_found)) p_line(x,y,1); }
                            xpoint=x; ypoint=y;
                            for (i=0; i<nline2; ++i)
                                {
                  k=coords(j,line2_x[i],line2_y[i],xline2[i],yline2[i],&x,&y);
                                if (k<0) continue;
                                strcpy(v,linetype2[i]);
                                k=p_linecontrol(v); if (k<0) return(-1);
// fprintf(temp2,"\nLINE=6: line=%d %d %d %d %d|",line,xpoint,ypoint,x,y);
// kuva-alueen ylÑpuolelta tulevat pystyt viivat "vinoutuvat", jos line>0.

                                p_line2(xpoint,ypoint,x,y,1);
                                if (marker2[i]>=0)
                                    {
                                    strcpy(v,pointtype2[i]);
                                    k=p_linecontrol(v); if (k<0) return(-1);
                                    p_marker_select(marker2[i],markersize2[i]);
                                    get_marker_rot_angle(j);
                                    p_marker(x,y);
                                    }
                                }
                            if (point_given)
                                {
                                strcpy(v,pointtype1);
                                k=p_linecontrol(v); if (k<0) return(-1);
                                if (!point_size_varying)
                                    p_marker_select(marker_type1,marker_size1);
                            marker_type=marker_type1; marker_size=marker_size1;                           
                                merkitse(j,xpoint+xp,ypoint+yp);
                                }
                            if (line==7)
                               { strcpy(v,linetype1); p_linecontrol(v); }
                            break;

                          case 8: // RS 27.12.2012  8=curve to x,y                        
							if (nline2!=2) return(-1);
							strcpy(v,linetype1); p_linecontrol(v);							
							xorg=x_pos; yorg=y_pos;					
							xpoint=x; ypoint=y;
							k=coords(j,line2_x[0],line2_y[0],xline2[0],yline2[0],&x,&y);
							if (k<0) return(-1);
							k=coords(j,line2_x[1],line2_y[1],xline2[1],yline2[1],&x2,&y2);
							if (k<0) return(-1);							

// Rprintf("\ncurve: %d %d %d %d %d %d %d %d",x_pos,y_pos,xpoint,ypoint,x,y,x2,y2,1);								
							p_curve(xorg,yorg,xpoint,ypoint,x,y,x2,y2,1);
														
						if (point_given)
							{
							strcpy(v,pointtype1);
							k=p_linecontrol(v); if (k<0) return(-1);
							if (!point_size_varying) p_marker_select(marker_type1,marker_size1);
							marker_type=marker_type1; marker_size=marker_size1;                           
							merkitse(j,xorg+xp,yorg+yp);
							}
						x_pos=xpoint; y_pos=ypoint;
						strcpy(v,linetype1); p_linecontrol(v);
							break;
							

						case 9: // RS 27.12.2012  9=move to x,y, curve from there							
							
							if (nline2!=3) return(-1);
							strcpy(v,linetype1); p_linecontrol(v);
							xorg=x; yorg=y;					
							x_pos=x; y_pos=y;
							k=coords(j,line2_x[0],line2_y[0],xline2[0],yline2[0],&xpoint,&ypoint);
							if (k<0) return(-1);							
							k=coords(j,line2_x[1],line2_y[1],xline2[1],yline2[1],&x,&y);
							if (k<0) return(-1);
							k=coords(j,line2_x[2],line2_y[2],xline2[2],yline2[2],&x2,&y2);
							if (k<0) return(-1);							

							i=0;
                            strcpy(v,linetype2[i]);
                            k=p_linecontrol(v); if (k<0) return(-1);							

							p_curve(x_pos,y_pos,xpoint,ypoint,x,y,x2,y2,1);	
                            	
							if (marker2[i]>=0)
								{
								strcpy(v,pointtype2[i]);
								k=p_linecontrol(v); if (k<0) return(-1);
								p_marker_select(marker2[i],markersize2[i]);
								get_marker_rot_angle(j);
								p_marker(xpoint,ypoint);
								}						
					
														
						if (point_given)
							{
							strcpy(v,pointtype1);
							k=p_linecontrol(v); if (k<0) return(-1);
							if (!point_size_varying) p_marker_select(marker_type1,marker_size1);
							marker_type=marker_type1; marker_size=marker_size1;                           
							merkitse(j,xorg+xp,yorg+yp);
							}
						x_pos=xorg; y_pos=yorg;
						strcpy(v,linetype1); p_linecontrol(v);                      
                            break;                            
                            
                            
                            
                          case 10: // LINE=POLYGON,fill
                            fwrite(&x,sizeof(int),1,temp_poly);
                            fwrite(&y,sizeof(int),1,temp_poly);
                            ++n_poly;
                            break;
                            }  /* switch */
                        obs_found=1; /* 26.9.93 */
                        }
               //   if (kbhit()) { prind=1-prind; getch(); } 8.11.09
                    }
                if (capability[0] && i_thick<thickness-1)
                    {
                  sprintf(sbuf,"\nPlotting again (thickness=%d)...",i_thick+2);
                    sur_print(sbuf);
                    }
                } /* i_thick */
            if (*line_label)
                {
                p_pen();
                p_text((unsigned char *)line_label,xx+x_kuva+(int)kirjainlev,
                           y_pos-y_thick[thickness-1]-(int)(kirjainkork/2),1);
                }
            i_thick=0;  /* jotta varjostukset oikein */

            if (line==10)
                {
                muste_fclose(temp_poly);
                p_polygon_line(n_poly,line_polygon_fill);
                }
            }
        yvar=yvars[0];
        return(1);
        }

static int get_marker_rot_angle(long j)
    {
    int i;
    if (*marker_rot_variable!=EOS) // 3.9.2010
    	{
        i=data_load(&d,j,marker_rot_var,&marker_rot_angle);
        if (i<0) return(-1); // RS 17.1.2013
        }
// Rprintf("\nj=%d var=%d angle=%g",j,marker_rot_var,marker_rot_angle); getch();
    return(1);
    }

static int merkitse(long j,int x,int y)
        {
        int i,erstat;
        char label[LLENGTH];
        double a;
        int size;

        get_marker_rot_angle(j);

        if (point_color_var>=0) // 11.5.2005
            {
            erstat=data_load(&d,j,point_color_var,&a);
            if (erstat<0) return(-1); // RS 17.1.2013
            if (a==MISSING8) a=0.0;
            p_marker_color((int)a);
            }
        else if (capability[2]==0) // 26.5.2010
            {
            extern int line_color; // 25.5.2010
            p_marker_color(line_color); // 25.5.2010
            }

        if (point_type_var>=0) // 20.5.2005
            {
            erstat=data_load(&d,j,point_type_var,&a);
            if (erstat<0) return(-1);
            if (a==MISSING8) a=0.0;
            marker_type=(int)a;
            p_marker_type_select(marker_type); // 28.5.2005
            }
/**********************
printf("\npoint_var=%d point_text=%s point_color_var=%d type=%d|",
              point_var, *point_text,point_color_var, point_type_var);
printf("\npoint_size_varying=%d|",point_size_varying);
getch();
**********************/

        if (point_size_varying)
            {
            erstat=data_load(&d,j,point_var,&a);
            if (erstat<0) return(-1); // RS 17.1.2013
            if (a==MISSING8) a=0.0;
            if (point_max>0.0)
                {
                size=marker_size*a/point_max;
                p_marker_select(marker_type,size);
                }
            else  /* point_max=0.0: point_var:n arvo suoraan merkin nro. */
                {
                p_marker_select((int)a,marker_size);
                }
            get_marker_rot_angle(j);
            p_marker(x,y);
            return(1);
            }



        if (point_var>=0)
            {
            if (d.vartype[point_var][0]=='S')
                {
                erstat=data_alpha_load(&d,j,point_var,label);
                if (erstat<0) return(-1); // RS 17.1.2013
                }
            else
                {
                erstat=data_load(&d,j,point_var,&a);
                if (erstat<0) return(-1); // RS 17.1.2013
                if (a==MISSING8) strcpy(label," "); // 3.3.2002
                else fconv(a,"",label);
                }
            i=strlen(label); while(label[i-1]==' ') label[--i]=EOS;
            p_text((unsigned char *)label,x,y,1);
            }
        else if (*point_text)
            { p_text((unsigned char *)point_text,x,y,1); x_pos=x-xp; y_pos=y-yp; }
        else { get_marker_rot_angle(j); p_marker(x,y); }
        return(1);
        }

static int coord_dia(long j,int *px,int *py)
        {
        int i;
        static double x,y; // RS REM ,x1,y1,s;
        static double xv,yv;

        yyy=MISSING8;  /* 15.4.90 */
        i=xy_arvot_dia(j,&x,&y);

        if (jitter) // 17.3.2002
            {
            if (xjitter>0.0)
                {
                if (x!=MISSING8)
                    x+=xjitter*(0.5-uniform_dev());
                }
            if (yjitter>0.0)
                {
                if (y!=MISSING8)
                    y+=yjitter*(0.5-uniform_dev());
                }
            }

        if (line==10)
            {
            if (missing)
                {
                missing=0;
                *px=(int)x; *py=1000000;
                return(1);
                }
            }
        if (i<0) return(-1); // xy_arvot()
        if (lag) { x+=x_lag; y+=y_lag; }
        if (line)
            {
            if (x<xmin || x>xmax || y<ymin || y>ymax)
                {
                if (!out)
                    {
                    outline(xv,yv,x,y,px,py); out=1; xv=x; yv=y;
                    *px+=x_thick[i_thick];
                    *py+=y_thick[i_thick];
                    return(1);
                    }
                out=1; xv=x; yv=y; return(0);
                }
            if (out)
                {
                if (out==1)   /* out=2 start */
                    {
                    outline(x,y,xv,yv,&x_pos,&y_pos);
                    }
                }
            xv=x; yv=y; out=0;
            } /* line */

        else if (x<xmin || x>xmax || y<ymin || y>ymax)
            {
            if (x<xmin) x=xmin;
            if (x>xmax) x=xmax;
            if (y<ymin) y=ymin;
            if (y>ymax) y=ymax;
            }  /* !line */

        *px=xx+x_kuva*(xmu(x)-xmumin)/(xmumax-xmumin)+x_thick[i_thick];
        *py=yy+y_kuva*(ymu(y)-ymumin)/(ymumax-ymumin)+y_thick[i_thick];
        xxx=x; yyy=y;  /* for TREND and/or CONTOUR */
        return(1);
        }


static int xy_arvot_dia(long j,double *px,double *py)
        {
        register int i;
        int erstat;

        for (i=0; i<spn; ++i) spb[i]=spb2[i];
        if (aika)
            *px=(double)j;
       /*   *px=(double)(j-d.l1+1);     */
        else
            {
            erstat=data_load(&d,j,xvar,px); 
            if (erstat<0) return(-1); // RS 17.1.2013
            if (*px==MISSING8) { missing=1; return(-1); }
            }
        if (normal)
            {
            ++i_normal; *py=((double)i_normal-0.5)/n_normal;
            }
        else
            {
            erstat=data_load(&d,j,yvar,py);
            if (erstat<0) return(-1); // RS 17.1.2013
            if (*py==MISSING8) { missing=1; return(-1); }
            }
        missing=0;
        return(1);
        }

static int normal_check()
        {
        double x,x_edell;
        int j,erstat; // RS CHA long

        n_normal=0L;
        x_edell=-1e300;
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            erstat=data_load(&d,j,xvar,&x);
            if (erstat<0) return(-1); // RS 17.1.2013
            if (x==MISSING8) continue;
            if (x<x_edell)
                {
                sprintf(sbuf,"Please, sort data in ascending order by %.8s!",
                                d.varname[xvar]);
                p_error(sbuf);
                return(-1);
                }
            ++n_normal;
            x_edell=x;
            }
        if (n_normal==0L)
            {
            p_error("No acceptable observations");
            return(-1);
            }
        return(1);
        }

static int coords(long j,int xvar,int yvar,double xconst,double yconst,int *px,int *py)
        {
        double x,y;
        int erstat;

        if (xvar==-1) x=(double)j;
        else  if (xvar==-2) x=xconst;
        else
            {
            data_load(&d,j,xvar,&x);
            if (x==MISSING8) return(-1);
            }
        if (yvar==-2) y=yconst;
        else
            {
            erstat=data_load(&d,j,yvar,&y);
            if (erstat<0) return(-1); // RS 17.1.2013
            if (y==MISSING8) return(-1);
            }
        *px=xx+x_kuva*(xmu(x)-xmumin)/(xmumax-xmumin)+x_thick[i_thick];
        *py=yy+y_kuva*(ymu(y)-ymumin)/(ymumax-ymumin)+y_thick[i_thick];
        return(1);
        }

static int sp_line(int var)    /* LINE=1,thickness*thickgap,line_label */
        {
        int i,k;
        char x[LLENGTH], *osa[3];
        char *p;
        char nimi[16];

        line=0;
        thickness=1; thickgap=2; *line_label=EOS;
        if (var>=0)
            {
            for (i=0; i<8; ++i) nimi[i]=d.varname[var][i];
            nimi[8]=EOS;
            i=8; while (nimi[i-1]==' ') nimi[--i]=EOS;
            strcat(nimi,"LINE");
            i=spfind(nimi);
            if (i<0) { i=spfind("LINE"); if (i<0) return(1); }
            }
        else { i=spfind("LINE"); if (i<0) return(1); }
        strcpy(x,spb[i]);
        plinepoint=linepoint_tila;
        linetype1=plinepoint;
        i=etumerkinta(x,&p); if (i<0) return(-1);
        k=control_code(x,&p,1);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        k=split(p,osa,3);

// fprintf(temp2,"\nosa[0]=%s|",osa[0]);
// muste_fclose(temp2);
        if (muste_strnicmp(osa[0],"POLY",4)==0 || atoi(osa[0])==10) // 31.8.2001
            {
            line=10; // polygon_line;
            line_polygon_fill=0;
            if (k>1) line_polygon_fill=atoi(osa[1]);
            return(1);
            }
		line=atoi(osa[0]);
        if (muste_strnicmp(osa[0],"CURVE",5)==0) line=9; // RS 3.2.2013
        else if (muste_strnicmp(osa[0],"CURVE2",6)==0) line=8; // RS 3.2.2013
        if (line==6 || line==7) { i=lines2(); if (i<0) return(-1); }      
        if (line==8 || line==9) { i=lines2(); if (i<0) return(-1); } // RS 27.12.2012
        if (k<2) return(1);
        p=strchr(osa[1],'*');
        if (p!=NULL) { *p=EOS; thickgap=atoi(p+1); }
        thickness=atoi(osa[1]);
        if (thickness>13)
            {
            p_error("Max. LINE thickness is 13 (Use [line_width()]!)");
            return(-1); // RS ADD
            }
        if (k<3) return(1);
        strcpy(line_label,osa[2]);
        return(1);
        }

static int lines2()
        {
        char linename[16];
        int i,k;
        char x[LLENGTH], *osa[2];
        char *p;

        k=2;

        if (plinepoint==NULL) plinepoint=linepoint_tila;
        while (1)
            {
            sprintf(linename,"LINE%d",k);
            i=spfind(linename); if (i<0) break;
            strcpy(x,spb[i]);
            linetype2[k-2]=plinepoint;
            i=etumerkinta(x,&p); if (i<0) return(1);
            i=split(p,osa,2);
            if (i<2)
                {
                sprintf(sbuf,"Error in %s!",linename);
                p_error(sbuf);
                return(-1);
                }
            i=varfind2(&d,osa[0],0);
            if (i<0) { line2_x[k-2]=-2; xline2[k-2]=arit_atof(osa[0]); }
            else line2_x[k-2]=i;
            i=varfind2(&d,osa[1],0);
            if (i<0) { line2_y[k-2]=-2; yline2[k-2]=arit_atof(osa[1]); }
            else line2_y[k-2]=i;
            ++k;
            }
        if (k==2)
            {
            p_error("LINE2 missing!");
            return(-1);
            }
        nline2=k-2;
        return(1);
        }


/*
POINT=<point_var>
POINT=<point_text>
POINT=<marker_type>,<marker_size>
POINT=<marker_type>,<marker_size>,<point_var>,<point_max>
*/
static int sp_point(int var)
        {
        int i,k;
        char x[LLENGTH], *osa[4];
        char *p;
        char nimi[16];

        arrowlen=2.5; i=spfind("ARROWLEN"); // 3.9.2010
        if (i>=0) arrowlen=atoi(spb[i]);

        point_color_var=-1;  // 11.5.2005
        i=spfind("POINT_COLOR");
        if (i>=0)
            {
            point_color_var=varfind(&d,spb[i]);
            if (point_color_var<0)
                { sp_virhe(spa[i],spb[i]); return(-1); }
            }
        i=spfind("POINT_TYPE");
        if (i>=0)
            {
            point_type_var=varfind2(&d,spb[i],0);
            if (point_type_var<0)
                { sp_virhe(spa[i],spb[i]); return(-1); }
            }
        else point_type_var=-1; // RS ADD    
        if (plinepoint==NULL) plinepoint=linepoint_tila;
        point_given=0;
        point_var=-1;
        *point_text=EOS;
        point_size_varying=0;

        if (var>=0)
            {
            for (i=0; i<8; ++i) nimi[i]=d.varname[var][i];
            nimi[8]=EOS;
            i=8; while (nimi[i-1]==' ') nimi[--i]=EOS;
            strcat(nimi,"POINT");
            i=spfind(nimi);
            if (i<0) i=spfind("POINT");
            }
        else i=spfind("POINT");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            pointtype1=plinepoint;
            i=etumerkinta(x,&p); if (i<0) return(-1);
            k=control_code(x,&p,0);
            if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
            k=split(p,osa,4);
            point_var=varfind2(&d,osa[0],0);
                if (point_var>=0) { point_given=1; return(1); }
            if (*osa[0]!='0' && atoi(osa[0])==0)
                {
                strncat(point_text,osa[0],15); point_given=1; return(1);
                }
            marker_type=1; /* marker_size=10; 8.8.88 */
            if (k>0) marker_type=atoi(osa[0]);
            if (k>1) marker_size=(int)arit_atof(osa[1]); // RS CHA 3.8.2012 atoi(osa[1]);
            if (k>3)
                {
                point_var=varfind(&d,osa[2],1); if (point_var<0) return(-1);
                point_max=arit_atof(osa[3]); // RS ADD 3.8.2012 arit_
                if (point_max<0.0) return(-1);
                point_size_varying=1;
                }
            point_given=1;
            }

		if (*marker_rot_variable!=EOS) // 3.9.2010
   			{	
   			marker_rot_var=varfind(&d,marker_rot_variable); // 24.3.2012
   			if (marker_rot_var==-1) return(-1); // 24.3.2012
   			}

//        if (*marker_rot_variable!=EOS) // 3.9.2010
//            marker_rot_var=varfind2(&d,marker_rot_variable);
// Rprintf("\nmarker: %s %d",marker_rot_variable,marker_rot_var); getch();

        marker_type1=marker_type; marker_size1=marker_size;
        p_marker_select(marker_type,marker_size);
        return(1);
        }

static int points2()
        {
        char pointname[16];
        int i,k;
        char x[LLENGTH], *osa[2];
        char *p;

        for (k=2; k<nline2+2; ++k)
            {
            sprintf(pointname,"POINT%d",k);
            i=spfind(pointname); if (i<0) { marker2[k-2]=-1; continue; }
            strcpy(x,spb[i]);
            pointtype2[k-2]=plinepoint;
            i=etumerkinta(x,&p); if (i<0) return(-1);
            marker2[k-2]=1; markersize2[k-2]=10;
            i=split(p,osa,2);
            if (i>0) marker2[k-2]=atoi(osa[0]);
            if (i>1) markersize2[k-2]=atoi(osa[1]);
            }
        return(1);
        }

static int sp_lag()
        {
        int i;
        char x[LLENGTH], *osa[2];

        lag=0;
        x_lag=y_lag=0.0;
        i=spfind("LAG"); if (i<0) return(1);
        lag=1;
        strcpy(x,spb[i]);
        i=split(x,osa,2);
        x_lag=arit_atof(osa[0]);
        if (i>1) y_lag=arit_atof(osa[1]);
        return(1);
        }

static int etumerkinta(char *x,char **pp)
        {
        int i;
        char *p;
        int sulkuind;

        i=etsi_loppusulku(x,pp); if (i<0) return(-1);
        if (*pp==x) { *plinepoint=EOS;  ++plinepoint; return(1); }
        if (*x=='(') sulkuind=1; else sulkuind=0;
        p=x+sulkuind;
        if (plinepoint-linepoint_tila>LINEPOINTSPACE-(*pp-x)-2)
            {
            p_error("Not enough space for POINT/LINE specifications!");
            return(-1);
            }
        while (p<*pp-sulkuind) *plinepoint++=*p++;
        *plinepoint++=EOS;
        return(1);
        }


static int diafill()
        {
        int i; // RS REM ,k;
        int i1,i2;
        int x1,y1,y2; // ,x2
        int ax1,ay1,ax2,ay2;
        int x_pos1,y_pos1,x_pos2,y_pos2;
        long j;
        int x;
        char fill_etu[LLENGTH], negfill_etu[LLENGTH];
        char v[LLENGTH];
        int fill_tila=0;  /* 1=pos. -1=neg. */
        int missing1;

        i=fill_find(fill_etu);
        if (i<0) return(1);
/*
  Rprintf("\nfill: gap=%d var=%d const=%g start=%ld end=%ld",
fill_gap,fill_var,fill_const,fill_start2,fill_end2);
  getch();
*/
        i=fill_neg_find(negfill_etu);
        if (i<0) return(1);
/*
printf("\nfill_etu=%s neg=%s",fill_etu,negfill_etu); getch();
*/
        for (i=0; i<spn; ++i) spb2[i]=spb[i];

        if (capability[0]) sur_print("\nPlotting observations (FILL) ...");
        j=fill_start2;
        coord2(j,&x_pos1,&y_pos1,1);
        coord2(j,&x_pos2,&y_pos2,2);
        missing1=0;
        for (j=fill_start2; j<=fill_end2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            i1=coord2(j,&ax1,&ay1,1); if (missing) { missing1=1; continue; }
            i2=coord2(j,&ax2,&ay2,2); if (missing) { missing1=1; continue; }

            if (missing1)
                {
                missing1=0;
                x_pos1=ax1; y_pos1=ay1; x_pos2=ax2; y_pos2=ay2; continue;
                }
            x1=ax1; y1=ay1; /* x2=ax2; */ y2=ay2;

            switch (fill_line)
                {
              case 2:
              case 3: y2=y_pos2; break;
              case 4:
              case 5: y_pos2=y2; break;
                }
            switch (line)
                {
              case 2:
              case 3: y1=y_pos1; break;
              case 4:
              case 5: y_pos1=y1; break;
                }
            if (i1>0 && i2>0)
                {
                if (capability[0]) { sprintf(sbuf," %ld",j); sur_print(sbuf); }
                x=x_pos1;
                while (x<=x1)
                    {
                    int yy1,yy2;

                    if (x1==x_pos1) yy1=y_pos1;
             else yy1=(int)(y_pos1+(x-x_pos1)*(double)(y1-y_pos1)/(x1-x_pos1));
                    if (x1==x_pos1) yy2=y_pos2;
             else yy2=(int)(y_pos2+(x-x_pos1)*(double)(y2-y_pos2)/(x1-x_pos1));

                    if (yy1<yy) yy1=yy; if (yy1>yy+y_kuva) yy1=yy+y_kuva;
                    if (yy2<yy) yy2=yy; if (yy2>yy+y_kuva) yy2=yy+y_kuva;

                    if (yy1>=yy2)
                        {
                        if (fill_tila!=1)
                            {
                            strcpy(v,fill_etu);
                            i=p_linecontrol(v); if (i<0) return(-1);
                            fill_tila=1;
                            }
                        }
                    else
                        {
                        if (fill_tila!=-1)
                            {
                            strcpy(v,negfill_etu);
                            i=p_linecontrol(v); if (i<0) return(-1);
                            fill_tila=-1;
                            }
                        }

                    p_line2(x,yy1,x,yy2,1);
                    if (x==x1) break;

                    if (fill_neg_gap && yy1<yy2) x+=fill_neg_gap;
                    else x+=fill_gap;
                    if (x>x1) x=x1;
                    }
                }
            x_pos1=ax1; y_pos1=ay1; x_pos2=ax2; y_pos2=ay2;
            }
        return(1);
        }

static int coord2(long j,int *px,int *py,int k)
        {
        int i;
        double x,y;

        if (k==1)
            i=xy_arvot_dia(j,&x,&y);
        else
            i=xy_arvot2_dia(j,&x,&y);
        if (i<0) { missing=1; return(-1); }  /* missing 17.9.1996 */
/* Rprintf("\nlag=%d %g",lag,x_lag); getch(); */
        if (lag) { x+=x_lag; y+=y_lag; }   /* 17.9.1996 */
/*
        if (x<xmin || x>xmax || y<ymin || y>ymax)
            {
            if (x<xmin) x=xmin;
            if (x>xmax) x=xmax;
            if (y<ymin) y=ymin;
            if (y>ymax) y=ymax;
            }
*/
        *px=xx+x_kuva*(xmu(x)-xmumin)/(xmumax-xmumin);
        *py=yy+y_kuva*(ymu(y)-ymumin)/(ymumax-ymumin);
        return(1);
        }


static int xy_arvot2_dia(long j,double *px,double *py)
        {
        register int i;
        int erstat;

        for (i=0; i<spn; ++i) spb[i]=spb2[i];
        if (aika)
            *px=(double)(j-d.l1+1);
        else
            {
            erstat=data_load(&d,j,xvar,px); 
            if (erstat<0) return(-1); // RS 17.1.2013
            if (*px==MISSING8) { missing=1; return(-1); }
            }
        if (fill_var>=0)
            {
            erstat=data_load(&d,j,fill_var,py);
            if (erstat<0) return(-1); // RS 17.1.2013
            if (*py==MISSING8) { missing=1; return(-1); }
            }
        else
            *py=fill_const;
        missing=0;
        return(1);
        }

static int fill_find(char *v)
        {
        int i,k;
        char x[LLENGTH], *osa[5];
        char *p,*q;
        int sulkuind;

        i=spfind("FILL"); if (i<0) return(-1);
        strcpy(x,spb[i]);
        k=etsi_loppusulku(x,&p);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        if (*x=='(') sulkuind=1; else sulkuind=0;
        *v=EOS;
        if (p>x) { q=x+sulkuind; while (q<p-sulkuind) *v++=*q++; *v=EOS; }

        control_code(x,&p,1);
        k=split(p,osa,5);
        if (k==0) return(-1);
        fill_var=-1;
        fill_start2=xmin; fill_end2=xmax;
        fill_gap=atoi(osa[0]); fill_const=0.0;
        fill_neg_gap=0;
        if (fill_gap<1) fill_gap=1;
        if (k<2) return(1);
        fill_start2=(long)arit_atof(osa[1]); if (k<3) return(1);
        fill_end2=(long)arit_atof(osa[2]); if (k<4) return(1);
        fill_var=varfind2(&d,osa[3],0);
        if (fill_var<0) fill_const=arit_atof(osa[3]);
        if (k>4) fill_line=atoi(osa[4]);
        return(1);
        }

static int fill_neg_find(char *v)
        {
        int i,k;
        char x[LLENGTH];
        char *p,*q;
        int sulkuind;

        *v=EOS;  /* 21.9.1987/SM */
        i=spfind("FILL-"); if (i<0) return(1);
        strcpy(x,spb[i]);
        k=etsi_loppusulku(x,&p);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        if (*x=='(') sulkuind=1; else sulkuind=0;
        *v=EOS;
        if (p>x) { q=x+sulkuind; while (q<p-sulkuind) *v++=*q++; *v=EOS; }

        control_code(x,&p,1);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        fill_neg_gap=atoi(p);
        if (fill_neg_gap<=0) fill_neg_gap=1;
        return(1);
        }

static int xyscale2_dia(char *suunta) /* "X" tai "Y" */
        {
//        extern double arit_atof();
        int i,k;
        char x[LLENGTH];
        char *p,*q;
        char snimi[16];

        i=p_pen(); if (i<0) return(-1);
        i=p_linetype(); if (i<0) return(-1);  /* merkintÑviivoihin */
                   /* haetaan joko XSCALE2 tai YSCALE2 */
        strcpy(snimi,suunta); strcat(snimi,"SCALE2");
        i=spfind(snimi); if (i<0) return(1);
        strcpy(x,spb[i]);
        k=control_code(x,&p,0);
        if (k<0) { sp_virhe(spa[i],spb[i]); return(-1); }
        if (*p=='*')  /* ohitetaan mahdollinen muunnos */
            {
            ++p;
            q=p;
            while (*q && *q!=',') ++q;
            *q=EOS;
            p=q+1;
            }

        if (*suunta=='X' && strcmp(p,"XSCALE")==0)
            {
            control_code_scale("XSCALE");
            zscalen=xscalen;
            for (i=0; i<zscalen; ++i)
                {
                zscaleval[i]=xscaleval[i];
                zscal[i]=xscal[i];
                }
            }
        else if (*suunta=='Y' && strcmp(p,"YSCALE")==0)
            {
            control_code_scale("YSCALE");
            zscalen=yscalen;
            for (i=0; i<yscalen; ++i)
                {
                zscaleval[i]=yscaleval[i];
                zscal[i]=yscal[i];
                }
            }
        else
            {                                       /* 20.5.92 */
            k=skaala_arvot(p,zscales,zscal,&zscalen,SCALESPACE);
            if (k<0) return(-1);
            for (i=0; i<zscalen; ++i)
                {
                q=zscal[i];
                p=strchr(zscal[i],':'); if (p!=NULL) { zscal[i]=p+1; *p=EOS; }
                zscaleval[i]=arit_atof(q);
                }
            }
        if (*suunta=='X')
            {
            if (!aika)
                plot_xscale2(zscalen,zscaleval,zscal,xx,yy+y_kuva,x_kuva);
            else
                plot_tscale_dia(zscalen,zscaleval,zscal,xx,yy+y_kuva,x_kuva,2);
            }
        else
            plot_yscale2(zscalen,zscaleval,zscal,xx+x_kuva,yy,y_kuva);
        return(1);
        }



static int init_trend()
        {
        int i;

        trend=0;
        i=spfind("TREND"); if (i>=0) { trend=1; null_mom(); }
        return(1);
        }

static int init_contour()
        {
        int i;

        contour=0;
        i=spfind("CONTOUR"); if (i>=0) { contour=1; null_mom(); }
        return(1);
        }

static int init_conf_band() // 1.2.2004
        {
        int i;

        conf_band[1]=0;
        i=spfind("CONF_BAND1"); if (i>=0) { conf_band[1]=1; null_mom(); }
        conf_band[2]=0;
        i=spfind("CONF_BAND2"); if (i>=0) { conf_band[2]=2; null_mom(); }
        conf_band[3]=0;
        i=spfind("CONF_BAND3"); if (i>=0) { conf_band[3]=3; null_mom(); }

        return(1);
        }

static void null_mom()
        {
        tn=0L; tx=ty=tx2=ty2=txy=0.0;
        }

static void compute_moments(double x,double y)
        {
        if (y==MISSING8) return;  /* 15.4.90 */
        ++tn;
        tx+=x; ty+=y; tx2+=x*x; ty2+=y*y; txy+=x*y;
        }

static int plot_trend()
        {
        int i;
        double r,b,dev,c; // a
        double mx,my,sx,sy;
        char s[LLENGTH], *osa[EP4];
        int ntaso;
        char *p;
        int also_reg_y=0; // 2.2.2004

        if (!trend || tn<2L) return(1);
        if (*xmuunnos || *ymuunnos) { linscale_only(); return(1); }
        i=find_binorm(&mx,&my,&sx,&sy,&r);
        if (i>=0)
            {
            sx*=sqrt((double)(tn-1L));
            sy*=sqrt((double)(tn-1L));
            }
        else
            {
            mx=tx/tn; my=ty/tn;
            sx=sqrt(tx2-tn*mx*mx);
            sy=sqrt(ty2-tn*my*my);
            if (sx==0.0 || sy==0.0) return(1);
            r=(txy-tn*mx*my)/sx/sy;
            }
        b=sy*r/sx;
//        a=my-b*mx;
        dev=sy/sqrt((double)(tn-1L))*sqrt(1-r*r);
        i=spfind("TREND"); if (i<0) return(1);
        strcpy(s,spb[i]);
        i=control_code(s,&p,1); if (i<0) return(-1);
        ntaso=split(p,osa,EP4);
        for (i=0; i<ntaso; ++i)
            {
            if (*osa[i]=='O') // ortog.regr. (principal axis) 30.3.2005
                {
                if (fabs(sx-sy)<1e-10)
                    {
                    b=1.0;
                    if (r<0) b=-1.0;
                    }
                else
                    {
                    if (sy>=sx)
                        {
                        b=2*sx*sy*r/(sx*sx-sy*sy);
                        b=-(sqrt(b*b+1)+1.0)/b;
                        }
                    else
                        {
                        b=2*sx*sy*r/(sx*sx-sy*sy);
                        b=(sqrt(b*b+1)-1.0)/b;
                        }
                    }
                plot_line_segment(mx,my,1.0,b);
                break;
                }
            if (*osa[i]=='X')
                {
                also_reg_y=1;
                continue;
                }
            c=atof(osa[i])*dev;
            plot_line_segment(mx,my+c,1.0,b);
            plot_line_segment(mx,my-c,1.0,b);
            }
        if (!also_reg_y) return(1);

        b=sy/(r*sx);
        plot_line_segment(mx,my,1.0,b);

        return(1);
        }

static void plot_line_segment(double mx,double my,double tx,double ty)
        {
// RS REM        double t;
        double x1,y1,x2,y2;
        int xp1,yp1,xp2,yp2;

        if (tx!=0)
            {
            x1=xmin;
            y1=my+ty*(x1-mx)/tx;
            if (y1<ymin) { y1=ymin; x1=mx+tx*(y1-my)/ty; }
            else if (y1>ymax) { y1=ymax; x1=mx+tx*(y1-my)/ty; }
            x2=xmax;
            y2=my+ty*(x2-mx)/tx;
            if (y2<ymin) { y2=ymin; x2=mx+tx*(y2-my)/ty; }
            else if (y2>ymax) { y2=ymax; x2=mx+tx*(y2-my)/ty; }
            }
        else
            {
            x1=x2=mx;
            y1=ymin; y2=ymax;
            }
        xy_point(x1,y1,&xp1,&yp1);
        xy_point(x2,y2,&xp2,&yp2);
        p_line2(xp1,yp1,xp2,yp2,1);
        }

static void xy_point(double x,double y,int *px,int *py)
        {
        *px=xx+(int)(x_kuva*(x-xmin)/(xmax-xmin));
        *py=yy+(int)(y_kuva*(y-ymin)/(ymax-ymin));
        }

static int linscale_only()
        {
        p_error("In TREND,CONTOUR,CONF_BAND no scale transformations permitted!");
        return(1);
        }

static int plot_contour()
        {
        int i;
        double r,t,eps; // RS REM ,a,b,c;
        double mx,my,sx,sy;
        char s[LLENGTH], *osa[EP4];
        int ntaso;
        double x,y;
        int xp,yp;
        int sis1,sis2;
        char *p;

        if (!contour || tn<2L) return(1);
        if (*xmuunnos || *ymuunnos) { linscale_only(); return(1); }
        i=find_binorm(&mx,&my,&sx,&sy,&r);
        if (i<0)
            {
            mx=tx/tn; my=ty/tn;
            sx=sqrt(tx2-tn*mx*mx);
            sy=sqrt(ty2-tn*my*my);
            if (sx==0.0 || sy==0.0) return(1);
            r=(txy-tn*mx*my)/sx/sy;
            sx/=sqrt((double)(tn-1L));
            sy/=sqrt((double)(tn-1L));
            }
        i=spfind("CONTOUR"); if (i<0) return(1);
        strcpy(s,spb[i]);
        i=control_code(s,&p,1); if (i<0) return(-1);
        ntaso=split(p,osa,EP4);

        for (i=0; i<ntaso; ++i)
            {
            eps=arit_atof(osa[i]);
            if (eps==0.0)
                {
                if (sx!=sy) t=0.5*atan(2*sx*sy*r/(sx*sx-sy*sy));
//              else t=0.0;
                else t=PI/4; // 18.1.2003
                plot_line_segment(mx,my,cos(t),sin(t));
                t+=PI/2;
                plot_line_segment(mx,my,cos(t),sin(t));
                continue;
                }
            if (eps<0.0 || eps>=1.0) continue;
            t=0.0;
            ellipse(mx,my,sx,sy,r,t,eps,&x,&y);
            sis1=xy_sisalla(x,y);
            xy_point(x,y,&x_pos,&y_pos);
            while (t<2*PI)
                {
                t+=PI/40;
                if (t>2*PI) t=2*PI;
                ellipse(mx,my,sx,sy,r,t,eps,&x,&y);
                sis2=xy_sisalla(x,y);
                xy_point(x,y,&xp,&yp);
                if (sis1 && sis2) p_line(xp,yp,1);
                sis1=sis2;
                x_pos=xp; y_pos=yp;
                }
            }
        return(1);
        }

static int xy_sisalla(double x,double y)
        {
        if (x<xmin || x>xmax || y<ymin || y>ymax) return(0);
        return(1);
        }

static void ellipse(double mx,double my,double sx,double sy,double r,double t,double eps,double *px,double *py)
        {
        double a;

        a=sqrt(-2*log(1.0-eps));
        *px=mx+sx*a*cos(t);
        *py=my+sy*a*sin(t+atan(r/sqrt(1.0-r*r)));
        }

static int find_binorm(double *pmx,double *pmy,double *psx,double *psy,double *pr)
        {
        int i;
        char x[LLENGTH], *osa[5];

        i=spfind("BINORM");
        if (i<0) return(-1);
        strcpy(x,spb[i]); i=split(x,osa,5);
        if (i<5)
            { p_error("Error in BINORM: Invalid # of parameters!"); return(-1); }
        *pmx=arit_atof(osa[0]); *pmy=arit_atof(osa[1]);
        *psx=arit_atof(osa[2]); *psy=arit_atof(osa[3]);
        *pr=arit_atof(osa[4]);
        return(1);
        }

static int plot_conf_band(int conf_type)
        {
        int i;
        double r,eps; // RS REM ,a,b,c,t;
        double mx,my,sx,sy;
        char s[LLENGTH], *osa[EP4];
        int ntaso;
        double x,y;
        int sis1,sis2=0;
        char *p;
        double f;
        double d12;
//        extern double inv_f();
//        extern double inv_t();

        int xp1,yp1,xp2,yp2;

        double sxx,syy,sxy,b1,sse,v1,v2,v3;
        double step;

		xp2=yp2=0; // RS 7.2.2013
        if (!conf_type || tn<2L) return(1);
        if (*xmuunnos || *ymuunnos) { linscale_only(); return(1); }
        i=find_binorm(&mx,&my,&sx,&sy,&r);
        if (i<0)
            {
            mx=tx/tn; my=ty/tn;
            sx=sqrt(tx2-tn*mx*mx);
            sy=sqrt(ty2-tn*my*my);
            if (sx==0.0 || sy==0.0) return(1);
            r=(txy-tn*mx*my)/sx/sy;
            sx/=sqrt((double)(tn-1L));
            sy/=sqrt((double)(tn-1L));
            }

// Rprintf("\nxmin=%g xmax=%g|",xmin,xmax); getch();

        sxx=sx*sx*(tn-1L);
        syy=sy*sy*(tn-1L);
        sxy=sx*sy*r*(tn-1L);

        b1=sxy/sxx;
        sse=syy-b1*sxy;
        v1=my-b1*mx;
// Rprintf("\nb1=%g sse=%g sxx=%g v1=%g|",b1,sse,sxx,v1); getch();
        step=(xmax-xmin)/32.0;

        sprintf(sbuf,"CONF_BAND%d",conf_type);
        i=spfind(sbuf); if (i<0) return(1);
        strcpy(s,spb[i]);
        i=control_code(s,&p,1); if (i<0) return(-1);
        ntaso=split(p,osa,EP4);

        for (i=0; i<ntaso; ++i)
            {
            d12=0.0;
            eps=arit_atof(osa[i]);
            if (conf_type==3)
                f=sqrt(2.0*muste_inv_f(eps,2.0,(double)(tn-2.0),14));
            else
                {
                f=muste_inv_t((1.0+eps)/2.0,(double)(tn-2.0));
                if (conf_type==2) d12=1.0;
                }
// Rprintf("\nf=%g|",f); getch();
            v2=f*sqrt(sse/(tn-2.0));

            if (eps<=0.0 || eps>=1.0)
                {
                if (eps<=0.0) continue;
                step=(xmax-xmin)/eps; continue;
                }

/*************************
x=140.0;
v3=v2*sqrt(d12+1.0/(double)tn+(x-mx)*(x-mx)/sxx);
y=v1+b1*x+v3;
printf("\nx=%g y1=%g y2=%g v2=%g v3=%g",x,y,v1+b1*x-v3,v2,v3); getch();
****************************/

            for (x=xmin; x<=xmax; x+=step)
                {
                v3=v2*sqrt(d12+1.0/(double)tn+(x-mx)*(x-mx)/sxx);
                y=v1+b1*x+v3;
                if (x==xmin)
                    {
                    xy_point(x,y,&x_pos,&y_pos);
                    continue;
                    sis2=xy_sisalla(x,y);
                    }
                else
                    {
                    sis1=sis2;
                    xy_point(x,y,&xp2,&yp2);
                    sis2=xy_sisalla(x,y);
                    if (sis1 && sis2)
                        p_line(xp2,yp2,1);
                    }
                }

            for (x=xmin; x<=xmax; x+=step)
                {
                v3=v2*sqrt(d12+1.0/(double)tn+(x-mx)*(x-mx)/sxx);
                y=v1+b1*x-v3;
                if (x==xmin)
                    {
                    xy_point(x,y,&xp2,&yp2);
                    continue;
                    sis2=xy_sisalla(x,y);
                    }
                else
                    {
                    xp1=xp2; yp1=yp2; sis1=sis2;
                    xy_point(x,y,&xp2,&yp2);
                    sis2=xy_sisalla(x,y);
                    if (sis1 && sis2)
                        p_line2(xp1,yp1,xp2,yp2,1);
                    }
                }

            }
        return(1);
        }

/*  phis.c 6.8.1986/SM (10.9.1991) (18.10.1996)
    HISTO <data>,<var>,L
*/

static void muste_histo(int argc, char *argv[])
        {
        int i,k; // RS REM ,v,ind;
        char laite[LLENGTH];
// RS REM        char gtype[16];
        char x[LLENGTH];
        char *p,*q;

        if (argc==1) return;
        s_init(argv[1]);
        argv1=argv[1];
        if (g<3)
            {
            sur_print("\nUsage: HISTO <data>,<variable>");
            WAIT; return;
            }

strcpy(muuttujanimi,"x"); // RS ADD
strcpy(muuttujanimi2,""); // RS ADD
strcpy(muuttujanimi3,""); // RS ADD
strcpy(muuttujanimi4,""); // RS ADD
            
        muste_gplot_init=1;
     	k=sp_init(r1+r-1);
     	muste_gplot_init=0;
        if (k<0)
            { liikaa_spec(); return; }
        if (muste_strcmpi("CHISTO",word[0])==0) cfunktio=1; else cfunktio=0;
        strcpy(aineisto,word[1]);
        freq_file=strchr(aineisto,'>');
        if (freq_file!=NULL) { *freq_file=EOS; ++freq_file; }
        
        
        if (freq_file==NULL)
            {
            i=data_open3(aineisto,&d,0,1,0,0); if (i<0) return;
            i=mask(&d); if (i<0) return;
            i=conditions(&d); if (i<0) return;  /* permitted only once */

            xvar=varfind(&d,word[2]); if (xvar<0) return;
            strcpy(varname,word[2]);

            i=spfind(word[2]);
            if (i<0) { class_error(); return; }

            strcpy(his_attributes,spb[i]);
            i=control_code(his_attributes,&p,2); if (i<0) return;
            strcpy(x,p);
            if (his_attributes==p) *his_attributes=EOS;
            p=strchr(x,'('); if (p==NULL) { class_error(); return; }
            *p=EOS; ++p; q=strchr(p,')'); if (q==NULL) { class_error(); return; }
            *q=EOS;
            x_lower=arit_atof(x); x_step=arit_atof(p); x_upper=arit_atof(q+1);
            }

        results_line=0;
        if (g>3) { results_line=edline2(word[3],1,1); if (results_line==0) return; }

        i=spfind("DEVICE");
        if (i<0) strcpy(laite,"MUSTE_PR.PS");
        else
            {
            strcpy(laite,spb[i]);
            if (strchr(laite,':')==NULL) // RS unix path FIXME
                {
                strcpy(laite,edisk);
                strcat(laite,spb[i]);
                }
            }
        if (strchr(word[0],'G')!=NULL)
            {
            edisp=4; s_end(argv[1]);
            }
        i=frekvenssit(); if (i<0) return;

        p=strchr(laite,','); // 14.6.2005
        if (p!=NULL && strcmp(p+1,"NULL")==0) return; // FREQ.F only! 14.6.2005

        i=p_init(laite); if (i<0) return;
        histogram();
        edisp=1; s_end(argv[1]);
        return;
        }   


static void class_error()
        {
        sprintf(sbuf,"\nClassification of %s must be given in the form",word[2]);
        sur_print(sbuf);
        sprintf(sbuf,"\n%s=<lower_limit>(<step>)<upper_limit>",word[2]);
        sur_print(sbuf);
        WAIT;
        }

static void liikaa_spec()
        {
        sur_print("\n Too many specifications!");
        WAIT;
        }

static int frekvenssit()
        {
        int i,prind,k,h,erstat;
        long j;
        double y,aa,eps;
        int low_limit_in_class=0; // 13.6.2005

        n_freq=n_out=0L;
        if (freq_file!=NULL) return(load_freq());  /* 28.5.90   */
        y=(x_upper-x_lower)/x_step;
        if (y>1000)
            {
            sur_print("\nMore than 1000 classes. Unbelievable!!!");
            WAIT; return(-1);
            }
        if (x_upper<x_lower || x_step<=0.0)
            {
            sprintf(sbuf,"\nError in classification of %s",word[2]);
            sur_print(sbuf); WAIT; return(-1);
            }

        /* Kun jako ei mene tasan, oli vaikeuksia FIT:issÑ */
        x_upper=x_lower+(int)(y+0.999)*x_step; /* 18.12.1996 */
        y=(x_upper-x_lower)/x_step; /* tarvitaan uudelleen 18.12.1996 */

        n_class=floor(y+0.6);
        i=varaa_tilat(); if (i<0) return(-1);
/*      if (freq_file!=NULL) return(load_freq());    -28.5.90   */
        for (i=0; i<n_class; ++i) freq[i]=0L;

        prind=1; border_cases=0; middle_cases=0;
        eps=1e-5*x_step;

        i=hae_apu("prind",sbuf); if (i) prind=atoi(sbuf);
        if ((i=spfind("PRIND"))>=0) prind=atoi(spb[i]);

        i=spfind("LOW_LIMIT_IN_CLASS");  // 13.6.2005
        if (i>=0) low_limit_in_class=atoi(spb[i]);


        sur_print("\nCounting frequencies...");
        for (j=d.l1; j<=d.l2; ++j)
            {
            if (unsuitable(&d,j)) continue;
            erstat=data_load(&d,j,xvar,&y);
            if (erstat<0) return(-1); // RS 17.1.2013
            if (y==MISSING8) { ++n_out; continue; }
            if (y<=x_lower || y>x_upper) { ++n_out; continue; }
            ++n_freq;
            if (low_limit_in_class)
                i=(unsigned int)((y-x_lower)/x_step);
            else
                i=(unsigned int)(ceil((y-x_lower)/x_step)-1);
            ++freq[i];

            if (prind)
                {
                sprintf(sbuf," %ld",j); sur_print(sbuf);
/*
printf("y=%g %g %g\n",y,x_lower+i*x_step,x_lower+(i+1)*x_step); getch();
*/
                aa=x_lower+i*x_step;
                if (fabs(y-aa)<eps || fabs(y-x_step-aa)<eps)
                    ++border_cases;
                else if (fabs(y-x_step/2-aa)<eps) ++middle_cases;

                }
            if (sur_kbhit()) { prind=1-prind; sur_getch(); }
            }

/* Rprintf("\nFrekvenssit:");
   for (i=0; i<n_class; ++i) Rprintf(" %ld",freq[i]); getch();
   Rprintf("\n%ld %ld",n_freq,n_out); getch();
*/
        k=0; for (i=0; i<n_class; ++i) if (freq[i]) ++k;

        skip_errors=0;
        i=spfind("SKIP_ERRORS");
        if (i>=0) skip_errors=atoi(spb[i]);

        h=2;
        i=spfind("ACCEPT");
        if (i>=0) h=atoi(spb[i]);

        if (k==0 && h>0)
            {
            if (!skip_errors)
              {
              sur_print("\nNo observations accepted. (Invalid classification?)");
              WAIT; return(-1);
              }
            }
        if (k==1 && h>1)
            {
            if (!skip_errors)
              {
              sur_print("\nAll observations in one class! (Invalid classification?)");
              WAIT; return(-1);
              }
            }

        aa=x_lower-x_step/2;
        if (!etu && (double)(border_cases)/(double)n_freq>0.3)
            {
            i=tell_language();
            if (i==1)
                {
                sur_print("\nVaroitus: Huono luokitus");
                sur_print("\nLiian monta havaintoarvoa yhtyy luokkarajoihin!");
                if ((double)(middle_cases)/(double)n_freq<0.1)
                  {
                  sprintf(sbuf,"\nEhdotus: Muuta luokituksen alaraja %g arvoksi %g .",
                                    x_lower,aa);
                  sur_print(sbuf);
                  }
                WAIT;
                }
            else
                {
                sur_print("\nWarning: Improper classification");
                sur_print("\nToo many cases equal to class limits!");
                if ((double)(middle_cases)/(double)n_freq<0.1)
                  {
                  sprintf(sbuf,"\nSuggestion: Change lower limit from %g to %g .",
                                    x_lower,aa);
                  sur_print(sbuf);
                  }
                WAIT;
                }
            }

        save_freq();

        return(1);
        }

static int varaa_tilat()
        {
        freq=(long *)muste_malloc(n_class*sizeof(long));
        if (freq==NULL) { not_enough_memory(); return(-1); }
        freq_est=(double *)muste_malloc(n_class*sizeof(double));
        if (freq_est==NULL) { not_enough_memory(); return(-1); }
        return(1);
        }

static void save_freq()
        {
        int i;
        char nimi[LLENGTH];
        long fmax;

        strcpy(nimi,edisk); strcat(nimi,"FREQ.F");
        fr=muste_fopen(nimi,"wt");
        if (fr==NULL)
            {
            sur_print("\nCannot save the frequency distribution!");
            WAIT; return;
            }
        fprintf(fr,"Frequency distribution of %s in %s \n",word[2],word[1]);
        fprintf(fr,"Classification: %g,%g,%g \n",x_lower,x_step,x_upper);
        fmax=0L; for (i=0; i<n_class; ++i) if(freq[i]>fmax) fmax=freq[i];
        fprintf(fr,"N=%ld N(OUT)=%ld classes=%d max=%ld\n",n_freq,n_out,n_class,fmax);
        for (i=0; i<n_class; ++i) fprintf(fr,"%ld\n",freq[i]);
        muste_fclose(fr);
        }

static int load_freq()
        {
        int i;
        char nimi[LLENGTH];
        char x[LLENGTH], *osa[3];
        char y[LLENGTH];
        char *p;
        char ch;
        long n;

        strcpy(nimi,edisk); strcat(nimi,"FREQ."); strcat(nimi,freq_file);

        if (strchr(freq_file,'.')!=NULL)   /* 28.5.90 */
            {
            strcpy(nimi,freq_file);
            if (strchr(nimi,':')==NULL) // RS unix path FIXME
              { strcpy(nimi,edisk); 
              strcat(nimi,freq_file);
              }
            }

        fr=muste_fopen(nimi,"rt");
        if (fr==NULL)
            {
            sur_print("\nCannot find frequency distribution!",nimi);
            WAIT; return(-1);
            }
        fgets(x,LLENGTH-1,fr); strcpy(y,x);
        p=strstr(x,"of"); if (p==NULL) { freq_error(nimi,x); return(-1); }
        i=split(p+3,osa,3);

/*      if (strcmp(word[2],osa[0])!=0 || strcmp(aineisto,osa[2])!=0)
            { freq_error(nimi,y); return(-1); }
*/
        strcpy(aineisto,osa[2]);  /* 28.5.90 */
        strcpy(varname,osa[0]);

        fgets(x,LLENGTH-1,fr);
        p=strchr(x,':');
        i=split(p+1,osa,3);
        x_lower=atof(osa[0]); x_step=atof(osa[1]); x_upper=atof(osa[2]);
        fgets(x,LLENGTH-1,fr);
        p=strchr(x,'='); n_freq=atol(p+1);
        p=strchr(p+1,'='); n_out=atol(p+1);
        p=strchr(p+1,'='); n_class=atoi(p+1);

        i=varaa_tilat(); if (i<0) return(-1);   /* 28.5.90 */

        n=0L;
        for (i=0; i<n_class; ++i)
            {
            p=x;
            while ((ch=fgetc(fr))!='\n') { *p=ch; ++p; }
            *p=EOS;
            freq[i]=atol(x);
            n+=freq[i];
            }
        if (n!=n_freq)
            {
            sur_print("\nSum of frequencies in %s =%ld",nimi,n);
            sur_print("\nnot equal to %ld",n_freq);
            WAIT; return(-1);
            }
        muste_fclose(fr);
        return(1);
        }

static void freq_error(char *nimi,char *x)
        {
        sprintf(sbuf,"\nError in frequency file %s\non line %s",nimi,x);
        sur_print(sbuf);
        WAIT;
        }


static int histogram()
        {
        int i;
        char otsikko[LLENGTH];

// RS CHA        tee_otsikko_histo(otsikko);
        sprintf(otsikko,"Histogram of %s in %s",word[2],word[1]);

        i=pen(); if (i<0) { p_end(); return(-1); }
        i=linetype(); if (i<0) { p_end(); return(-1); }
        i=xdiv(); if (i<0) { p_end(); return(-1); }
        i=ydiv(); if (i<0) { p_end(); return(-1); }
        i=frame(2); if (i<0) { p_end(); return(-1); }
        if (pr_type==1 || pr_type==2) { i=frames();
                    if (i<0) { p_end(); return(-1); } p_frame(frametype); }
        i=header(otsikko); if (i<0) { p_end(); return(-1); }
        i=xyscale_histo("X"); if (i<0) { p_end(); return(-1); }



        i=xlabel(varname); if (i<0) { p_end(); return(-1); }
        i=xyscale2("X"); if (i<0) { p_end(); return(-1); }
        i=xyscale_histo("Y"); if (i<0) { p_end(); return(-1); }
        i=ylabel(""); if (i<0) { p_end(); return(-1); }   /* 7.10.1995 */


// ??

        i=xyscale2("Y"); if (i<0) { p_end(); return(-1); }
        i=xtick(1); if (i<0) { p_end(); return(-1); }
        i=ytick(1); if (i<0) { p_end(); return(-1); }
        i=xtick(2); if (i<0) { p_end(); return(-1); }
        i=ytick(2); if (i<0) { p_end(); return(-1); }

//      while (kbhit()) getch();
        i=plot_histogram(); if (i<0) { p_end(); return(-1); }

        i=texts(); if (i<0) { p_end(); return(-1); }
        if (pr_type!=1 && pr_type!=2) { i=frames(); if (i<0) { p_end(); return(-1); } }
        i=fills(); if (i<0) { p_end(); return(-1); }
        i=xgrid(); if (i<0) { p_end(); return(-1); } /* siirr. 13.8.87 */
        i=ygrid(); if (i<0) { p_end(); return(-1); }
        i=fitting(); if (i<0) { p_end(); return(-1); }
        p_end();
        return(1);
        }

static int xyscale_histo(char *suunta) /* "X" tai "Y" */
        {
        extern double arit_atof();
        int i,k;
        char x[LLENGTH]; // RS REM , *osa[2];
        char *p,*q;
        char muunnos[LLENGTH];
        long ui;

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
            double min,max;

            if (*suunta=='X')
                {
                min=x_lower;
                max=x_upper;
                }
            else
                {
                min=0.0;
                ui=0L; for (i=0; i<n_class; ++i) if (freq[i]>ui) ui=freq[i];
                max=ui;
                }

            if (*suunta=='X') k=x_kuva/kirjainlev;
            else              k=2*y_kuva/kirjainkork;

            i=autom_scale(x,min,max,k); if (i<0) return(-1);

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
            {
            sprintf(sbuf,"%sSCALE values missing!",suunta);
            p_error2(sbuf);
            return(-1);
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
            plot_xscale(xscalen,xscaleval,xscal,xx,yy,x_kuva);
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
            plot_yscale(yscalen,yscaleval,yscal,xx,yy,y_kuva);
            return(1);
            }
        return(1);
        }


static int plot_histogram()
        {
        int i;
        int x1,y1,x2,y2;
        extern char his_attributes[];
        int fill;
        int no_fill;
        int k;

        his_values(); // 3.4.2004

/*
printf("\nxmin=%g xmax=%g x_lower=%g x_upper=%g",xmin,xmax,x_lower,x_upper);
*/
        if (xmin>=x_upper || xmax<=x_lower)  /* 4.3.1995 */
            {
            sprintf(sbuf,"XSCALE does not cover the range of observations!");
            p_error(sbuf);
            return(-1);
            }

        if (*his_attributes)
            {
            i=0; if (*his_attributes=='(') i=1;
            i=p_linecontrol(his_attributes+i); if (i<0) return(-1);
            }
        no_fill=0; fill=2; if (capability[1]) fill=8;
                // fill=1 aikaisemmin
        i=spfind("FILL");
        if (i>=0)
            {
            if (*spb[i]=='N') no_fill=1;  /* 19.8.1996 */
            fill=atoi(spb[i]);
            }
        else // 2.3.2001
            {
            i=spfind("[FILL1]");
            if (i<0) i=spfind("[FILL2]");
            if (i>=0)
                {
                strcpy(sbuf,spb[i]);
                if (strncmp(sbuf,"[FILL",5)==0)
                    fill=atoi(sbuf+5);
                if (!capability[1]) ++fill;
                }
            }

        for (i=0; i<n_class; ++i)
            {
            coord_histo(x_lower+i*x_step,(double)0,&x1,&y1);
            coord_histo(x_lower+(i+1)*x_step,(double)freq[i],&x2,&y2);
            plot_box(x1,y1,x2-x1,y2-y1);
            if (y2>y1+1 && !no_fill)
                {
                if (capability[1]) p_fill(x1+1,y1+1,fill);
                else { p_fill_bar(x1,y1,x2,y2,fill); plot_box(x1,y1,x2-x1,y2-y1); }
                }

            if (*valform!=EOS)
              {
              char kopio[LLENGTH];
              double arvo;

              if (valpros)
                  {
                  arvo=100.0*(double)freq[i]/(double)n_freq;
                  }
              else
                  arvo=(double)freq[i];

              strcpy(kopio,valcode);
              if (*valcode!=EOS)
                  { k=p_textcontrol(kopio); if (k<0) return(-1); }
              if (arvo>=valuemin) his_valtext2(x1,y1,y2-y1,arvo);
              if (*valcode!=EOS)
                  { k=p_pen(); if (k<0) return(-1); }

              }

            }
        return(1);
        }

static int plot_distribution()
        {
        int i;
//        double density();
        double t_step,x,y;
        double kerroin;
        int x1,y1;
        extern char fit_attributes[];

        if (*fit_attributes)
            {
            i=0; if (*fit_attributes=='(') i=1;
            i=p_linecontrol(fit_attributes+i); if (i<0) return(-1);
            }
        if (f_type==0) return(plot_probabilities());
        x=xmin; t_step=(xmax-xmin)/100;
        kerroin=n_freq*x_step/f_integral;
        y=kerroin*density(dnro,x,dpar);
        coord_histo(x,y,&x_pos,&y_pos);

        while (1)
            {
            x+=t_step; if (x>xmax) x=xmax;
            y=kerroin*density(dnro,x,dpar);
            coord_histo(x,y,&x1,&y1);
            p_line(x1,y1,1);
            if (x==xmax) break;
            }

        return(1);
        }

static int plot_probabilities()
        {
        double density();
        double x,y;
        double kerroin;
        int x1,y1;

        x=x_lower-x_step/2;
        kerroin=n_freq*x_step/f_integral;

        while (1)
            {
            x+=x_step; if (x>x_upper) break;
            coord_histo(x,0.0,&x_pos,&y_pos);
            y=kerroin*density(dnro,x,dpar);
            coord_histo(x,y,&x1,&y1);
            p_line(x1,y1,1);
            p_line2(x1-tikki,y1,x1+tikki,y1,1);
            }

        return(1);

        }

static void coord_histo(double x,double y,int *px,int *py)
        {
        *px=xx+x_kuva*(xmu(x)-xmumin)/(xmumax-xmumin);
        *py=yy+y_kuva*(ymu(y)-ymumin)/(ymumax-ymumin);
        }


static int his_values()
        {
        int i,k;
        char x[LLENGTH], *osa[3];
        char *p;
        int sulkuind;

        *valform=EOS; *valcode=EOS;
        i=spfind("VALUES"); if (i<0) return(1);
        strcpy(x,spb[i]);
        k=etsi_loppusulku(x,&p); if (k<0) return(-1);
        if (*x=='(') sulkuind=1; else sulkuind=0;
        if (p!=x)
            {
            if (*p!=',') { sp_virhe(spa[i],spb[i]); return(-1); }
            *(p-sulkuind)=EOS; strcpy(valcode,x+sulkuind); ++p;
            }
        k=split(p,osa,3);
        if (k<2) { sp_virhe(spa[i],spb[i]); return(-1); }
        if (k>2) valind=-1; else valind=1;
        strcpy(valform,osa[0]);
        valpaikka=atof(osa[1]);
        k=strlen(valform);
        if (valform[k-1]=='%') { valform[k-1]=EOS; valpros=1; }
        else                     valpros=0;

        valuemin=1.0;
        i=spfind("VALUEMIN");
        if (i>=0) valuemin=atof(spb[i]);

/***************
    Rprintf("\nvalcode=%s valform=%s valpaikka=%g valpros=%d",
              valcode,valform,valpaikka,valpros);
    getch();
******************/
        return(1);
        }

static void his_valtext2(int x1,int y1,int korkeus,double arvo)
        {
// RS REM        int k;
        char luku[32];
        int paikka;
// RS REM        int len;

        fconv(arvo,valform,luku);
        if (valpros) strcat(luku,"%");

        if (valpaikka*valind>=0)
            {
            if (korkeus>=0) paikka=y1+kirjainkork*valpaikka;
            else            paikka=y1-kirjainkork*(valpaikka+0.5);
            }
        else
            {
            if (korkeus>=0) paikka=y1+korkeus+kirjainkork*(valpaikka-0.5);
            else            paikka=y1+korkeus-kirjainkork*valpaikka;
            }
        p_text((unsigned char *)luku,x1+(int)kirjainlev,paikka,1);

        }

static int fitting()
        {
        int i;

        i=sp_fit(); if (i<0) return(-1);

        if (n_class<=1)
            {
            sprintf(sbuf,"Invalid classification! Not enough non-zero classes!");
            p_error2(sbuf);
            return(-1); // RS ADD
            }

        i=fit_distr(); if (i<0) return(-1);
        if (f_integral==0.0 && *distr) total_integral(dpar);
        i=f_estimates(); if (i<0) return(-1);
        printout(); // RS REM if (i<0) return(-1);

        if (*distr)
            {
            i=plot_distribution(); if (i<0) return(-1);
            }
        return(1);
        }

static int fit_distr()
        {
        int i;

        if (*distr==EOS) return(1);
        if (cfunktio)
            {
            dnro=CFUNCTION;
            i=find_own_distr(); if (i<0) return(-1);
            i=fit_own_distr();
            return(i);
            }

        if (matrix_fit)
            { f_type=0; dnro=MATRIX; i=fit_matrix(); return(i); }

        muste_strupr(distr);
        if (strcmp(distr,"N")==0 || strncmp(distr,"NORM",4)==0)
            { f_type=1; dnro=NORMAL; i=fit_normal(); return(i); }
        if (strncmp(distr,"BIN",3)==0)
            { f_type=0; dnro=BINOMIAL; i=fit_binomial(); return(i); }
        if (strcmp(distr,"POISSON")==0)
            { f_type=0; dnro=POISSON; i=fit_poisson(); return(i); }
        if (strncmp(distr,"LOGN",4)==0)
            { f_type=1; dnro=LOGNORMAL; i=fit_lognormal(); return(i); }
        if (strncmp(distr,"UNI",3)==0)
            { f_type=1; dnro=UNIFORM; i=fit_uniform(); return(i); }

        dnro=OWN_DISTR;
        i=find_own_distr(); if (i<0) return(-1);
        i=sp_update(); if (i<0) return(-1);
        i=fit_own_distr(); return(i);
        }

static int sp_fit()
        {
        int i,k;
        char *p;
        char *osa[MAXPAR];

        *distr=EOS; npar=0; dnro=0;
        i=spfind("FIT"); if (i<0) return(1);
        strcpy(fit_attributes,spb[i]);
        k=control_code(fit_attributes,&p,2); if (k<0) return(-1);
        strcpy(distr,p);

        matrix_fit=0;
        if (strncmp(distr,"MATRIX(",7)==0)
            {
            matrix_fit=1;
            }

        if (p==fit_attributes) *fit_attributes=EOS;
        if (matrix_fit) return(1);

        if (*distr=='-') strcpy(distr,spb[i]+1);
        p=strchr(distr,'(');
        if (p!=NULL)
            {
            *p=EOS;
            npar=split(p+1,osa,MAXPAR);
            k=strlen(osa[npar-1]);
            if (osa[npar-1][k-1]!=')')
                {
                sprintf(sbuf,") missing in FIT=%s",spb[i]);
                p_error2(sbuf);
                return(-1);
                }
            osa[npar-1][k-1]=EOS;
            for (k=0; k<npar; ++k) dpar[k]=arit_atof(osa[k]);
            }

        step_divisor=2;          /* 13.7.1994 */
        i=spfind("STEPDIVISOR");
        if (i>=0) step_divisor=atoi(spb[i]);

/*  for (i=0; i<npar; ++i) Rprintf("\ndpar%d=%g",i,dpar[i]);   */
        return(1);
        }

static int fit_normal()
        {
        if (npar<2) { mean_var(&dpar[0],&dpar[1],nof); npar=npar_est=2; }
        else        { npar_est=0; npar=2; }
        return(1);
        }

static int fit_lognormal()
        {
        if (npar<2) { mean_var(&dpar[0],&dpar[1],log); npar=npar_est=2; }
        else        { npar_est=0; npar=2; }
        return(1);
        }

static int fit_uniform()
        {
        if (npar<2)
            {
            p_error2("Limits a,b must be given in form FIT=UNIFORM(a,b)");
            return(-1); // RS ADD
            }
        npar=2; npar_est=0;
        return(1);
        }

static int fit_matrix()
        {
        int i; // RS REM ,n;
// RS REM        double pr,p;
        char nimi[LNAME];

                          // distr="MATRIX(P1)

        strcpy(nimi,distr+7);
        nimi[strlen(nimi)-1]=EOS; // ) pois!

        i=matrix_load(nimi,&pp,&mp,&np,&rlab,&clab,&lr,&lc,&mtype,sbuf);
        prob=pp;
        mat_est=0;
        i=spfind("PARAM");
        if (i>=0) mat_est=atoi(spb[i]);

        return(1);

        }
        
static int fit_binomial()
        {
        int i,n;
        double pr,p;

        if (npar<2)
            {
            if (npar==1)
                {
                mean_var(&dpar[1],&dpar[2],nof); dpar[1]/=dpar[0];
                npar=2; npar_est=1;
                }
            else
                {
                p_error2("Bin(N,p): Estimation of N missing!");
                return(-1); // RS ADD
                }
            }
        else        { npar_est=0; npar=2; }
        n=dpar[0]; p=dpar[1];
        i=prob_varaus(n+1); if (i<0) return(-1);
        pr=1.0; for (i=0; i<n; ++i) pr*=(1-p);
        prob[0]=pr;
        for (i=1; i<=n; ++i)  prob[i]=pr*=(double)(n-i+1)/i*p/(1-p);
/*
 pr=0.0;
 for (i=0; i<=n; ++i) { Rprintf("\n%d %g",i,prob[i]); pr+=prob[i]; }
 Rprintf("\n%g",pr); getch();
*/
        return(1);
        }

static int fit_poisson()
        {
        int i,n;
        double pr,a; // RS REM ,p;

        if (npar==0)
            {
            mean_var(&dpar[0],&dpar[1],nof);
            npar=npar_est=1;
            }
        else { npar_est=0; npar=1; }
        a=dpar[0]; n=floor(xmax);
        i=prob_varaus(n+1); if (i<0) return(-1);
        pr=exp(-a);
        prob[0]=pr;
        for (i=1; i<=n; ++i)  prob[i]=pr*=a/i;
        return(1);
        }

static int prob_varaus(unsigned int n)
        {
        prob=(double *)muste_malloc(n*sizeof(double));
        if (prob==NULL) return(-1);
        return(1);
        }

static void mean_var(double *pmean,double *pvar,double (*f)())
        {
        double s1,s2,a;
        int i;

        if (n_freq==0L) { *pmean=*pvar=0.0; return; }

        s1=s2=0.0;
        for (i=0; i<n_class; ++i)
            {
            a=(*f)(x_lower+x_step/2+i*x_step);
            s1+=freq[i]*a; s2+=freq[i]*a*a;
            }
        *pmean=s1/n_freq;
        *pvar=(s2-s1*s1/n_freq)/n_freq;
        }

static int total_integral(double *a)
        {
        integrate(dnro,a,xmin,xmax,x_step/step_divisor,&f_integral); 
/*      if (capability[0]) Rprintf("\nIntegral=%18.15g",f_integral);  */
		return(1); // RS ADD
        }

static int f_estimates()
        {
        int i;
        double a1,a2;
        double h,y;

        imin=0; while (freq[imin]==0L) ++imin; if (imin>0) --imin;
        imax=n_class-1; while (freq[imax]==0L) --imax; if (imax<n_class-1) ++imax;
        if (*distr==EOS) return(1);
        h=x_step/step_divisor;
        for (i=imin; i<=imax; ++i)
            {
            a1=x_lower+i*x_step; a2=a1+x_step;

            if (i==imin) a1=xmin;
            else if (i==imax) a2=xmax;
            integrate(dnro,dpar,a1,a2,h,&y);
            freq_est[i]=n_freq*y/f_integral;
            }
        return(1);
        }

static void integrate(int dnro,double *a,double a1,double a2,double h,double *py)
        {
        double t;
//        double density();
        double x,y,xv,yv;
        int i,i1,i2;


        if (f_type==0) /* probability */
            {
            t=0.0; i1=a1; i2=a2;
// Rprintf("\ni1+1=%d i2=%d|",i1+1,i2); getch();
            for (i=i1+1; i<=i2; ++i) t+=density(dnro,(double)i,a);
            *py=t;
            return;
            }
        t=0.0; xv=a1; yv=density(dnro,a1,a);
        x=a1+h; if (x>a2) x=a2;
        while (1)
            {
            y=density(dnro,x,a);
            t+=(x-xv)*(y+yv)/2;
            if (x==a2) break;
            xv=x; yv=y;
            x+=h; if (x>a2) x=a2;
            }
        *py=t;
        }

static double density(int dnro,double x,double *a)
        {
        extern double f_normal();
        extern double f_lognormal();
        extern double pr_binomial();
        extern double pr_poisson();
        extern double f_uniform();
        extern double f_own_distr();
        extern double f();
        extern double pr_matrix();

        switch (dnro)
            {
          case OWN_DISTR: return(f_own_distr(x,a));
          case CFUNCTION: return(f(x,a));
          case NORMAL: return(f_normal(x,a));
          case BINOMIAL: return(pr_binomial(x,a));
          case POISSON: return(pr_poisson(x,a));
          case LOGNORMAL: return(f_lognormal(x,a));
          case UNIFORM: return(f_uniform(x,a));
          case MATRIX: return(pr_matrix(x,a));
            }
        return(0.0);
        }

static double f_normal(double x,double *a)
        {
        double d;

        d=x-a[0];
        return(exp(-0.5*d*d/a[1])/sqrt(2*3.14159265358979*a[1]));
        }

static double f_lognormal(double x,double *a)
        {
        double d;

        if (x<=0.0) return(0.0);
        d=log(x)-a[0];
        return(exp(-0.5*d*d/a[1])/x/sqrt(2*3.14159265358979*a[1]));
        }

static double f_uniform(double x,double *a)
        {
        if (x<dpar[0] || x>dpar[1]) return(0.0);
        return(1/(dpar[1]-dpar[0]));
        }

static double pr_binomial(double x,double *a)
        {
        int i;

        i=x; if (i<0 || i>(int)dpar[0]) return(0.0);
        return(prob[i]);
        }

static double pr_poisson(double x,double *a)
        {
        int i;

        i=x; if (i<0 || i>floor(xmax)) return(0.0);
        return(prob[i]);
        }

static double nof(double x)  /* mean_var ilman muunnosta */
        {
        return(x);
        }

#define COEFF 1/sqrt(2*3.14159265358979)
static double f(double x,double *a)
        {
        double f1,f2,u;

        u=(x-a[1])/a[2];
        f1=a[0]/a[2]*exp(-0.5*u*u);
        u=(x-a[3])/a[4];
        f2=(1-a[0])/a[4]*exp(-0.5*u*u);
        return(COEFF*(f1+f2));
        }

static double pr_matrix(double x,double *a)
        {
        int i;

        i=x; if (i<0 || i>mp-1) return(0.0);
        return(prob[i]);
        }


static int find_own_distr()
        {
        char x[LLENGTH];
        int j; // RS REM ,i;
        char *p,*q;
        int nlaus;  /* 1: y(x)=f(x)    2: x(t)=..., y(t)=...  */

        j=etsi_distr("DENSITY",distr);
        if (j!=0) f_type=1;
        else
            {
            j=etsi_distr("PROBABILITY",distr);
            if (j==0)
                {
                sprintf(sbuf,"DENSITY/PROBABILITY %s not given!",distr);
                p_error2(sbuf);
                return(-1);
                }
            else f_type=0;
            }

/*
    Rprintf("\nnparn=%d",nparn);
    for (i=0; i<nparn; ++i) Rprintf(" %s",parnimi[i]); getch();
*/
        if (cfunktio) return(1);
        ++j;
        edread(x,j); *x=' ';
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
        while (*q!=' '  /* && *q!=',' */ ) ++q;
        *q=EOS;
// sprintf(sbuf,"nlaus=%d p=%s|",nlaus,p); p_error2(sbuf);
        if (nlaus==1)
            {
            strcpy(ylauseke,p+1);
            *xlauseke=EOS;  /* oli == */
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


static int etsi_distr(char *tyyppi,char *distr)
/* char *tyyppi;  DENSITY tai PROBABILITY */
        {
        int i,j;
        char x[LLENGTH], *osa[2];
        char *p;

        j=0;
        while (j<r2)
            {
            ++j;
            edread(x,j);
            i=split(x+1,osa,2);
            if (i<2) continue;
            if (muste_strcmpi(tyyppi,osa[0])!=0) continue;
            p=strchr(osa[1],'('); if (p!=NULL) *p=EOS;
            if (muste_strcmpi(distr,osa[1])!=0) continue;
            nparn=0; if (p==NULL) return(j);
            edread(parnimet,j);
            p=strchr(parnimet,'(');  /* !=NULL edell.perusteella */
            nparn=split(p+1,parnimi,MAXPAR);
            i=strlen(parnimi[nparn-1]);
            if (parnimi[nparn-1][i-1]!=')')
                {
                edread(x,j);
                sprintf(sbuf,") missing in %s",x+1);
                p_error2(sbuf);
                return(-1);
                }
            parnimi[nparn-1][i-1]=EOS;
            return(j);
            }
        return(0);
        }

static int sp_update()
        {
        int i;

        if (spn+nparn>=specmax)
            { liikaa_spec(); return(-1); }
        for (i=spn-1; i>=1; --i)
            {
            spa[i+nparn]=spa[i];
            spb[i+nparn]=spb[i];
            spb2[i+nparn]=spb2[i];
            spshad[i+nparn]=spshad[i];
            arvo[i+nparn]=arvo[i];
            }
        spn+=nparn;
        spa[0]=muuttujanimi;
        for (i=0; i<nparn; ++i) { spa[i+1]=parnimi[i]; spb[i+1]=NULL; }
        for (i=0; i<spn; ++i) spb2[i]=spb[i];
        return(1);
        }

static int fit_own_distr()
        {
        int i;

        if (npar)
            {
            if (npar!=nparn)
                {
                sprintf(sbuf,"Error in # of parameters in %s",distr);
                p_error2(sbuf);
                return(-1);
                }
            if (!cfunktio) for (i=0; i<npar; ++i)
                               arvo[i+1]=dpar[i];
            npar_est=0;
            total_integral(dpar);
            return(1);
            }

        npar=nparn;
        i=estimate(); if (i<0) return(-1);
        npar_est=npar;
        return(1);
        }

static double f_own_distr(double x,double *a)
        {
        register int i;
        double y;

        arvo[0]=x;
        for (i=0; i<npar; ++i) arvo[i+1]=a[i];
        for (i=0; i<spn; ++i) spb[i]=spb2[i];
        laske(ylauseke,&y);
        return(y);
        }


static int estimate()
        {
        extern double logll();
        int i,k;
        char x[LLENGTH], *osa[MAXPAR];
        double maxl;
        double step[MAXPAR];
        int maxnf;
// RS REM        int not_outputfile;

/************************************
        not_outputfile=0;
        if (!capability[0])
            {
            if (!*p_outfile) { not_outputfile=1; strcpy(p_outfile,etmpd);
                               strcat(p_outfile,"SURVO.SPX"); }
            p_save(p_outfile,SPX_CONST,SPX_CONST,-1,-1);
            }
        else CLS;
*************************************/
        for (i=0; i<npar; ++i) { dpar[i]=0.0; step[i]=0.1; }
        i=spfind("INIT");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=split(x,osa,npar);
            for (i=0; i<k; ++i) dpar[i]=arit_atof(osa[i]);
            }
        else
            {
            for (i=0; i<npar; ++i)
                {
                k=spfind2_histo(parnimi[i],npar+4); // RS 1 -> 4   /* ohitettava npar+1 ens. */
                if (k>=0) dpar[i]=arit_atof(spb[k]);
                }
            }

        i=spfind("STEP");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            k=split(x,osa,npar);
            if (k==1) { step[0]=arit_atof(osa[0]);
                        for (i=1; i<npar; ++i) step[i]=step[0];
                      }
            else if (k>0)
                for (i=0; i<k; ++i) step[i]=arit_atof(osa[i]);
            }

//      maxnf=32000;
        maxnf=1000;
        i=spfind("MAXNF");
        if (i>=0)
            {
            strcpy(x,spb[i]);
            maxnf=atoi(x);
            }

        total_integral(dpar);
//      Rprintf("\nIntegral=%g",f_integral);
        if (fabs(f_integral-1.0)<0.0001)
            {
//          Rprintf(" (assumed to be constant. No checking furthermore!)");
            integral_is_one=1;
            }
        else integral_is_one=0;

//      Rprintf("\nMaximum likelihood estimation:");
//      Rprintf("\nNumerical optimization by the Polytope algorithm (Nelder,Mead 1965)");
//      Rprintf("\nTo interrupt, press '.'\n");

        nf=nelder(dpar,&maxl,npar,logll,step,1.0,0.5,2.0,parnimi,"-logL",maxnf);


        estim_results();


/******************************************
        if (!capability[0])
            {

            p_load(p_outfile,0,0);
            if (not_outputfile) *p_outfile=EOS;
            }
******************************************/
        return(nf);
        }

static int nelder(
double *x,
double *py,
int n,
double (*f)(),
double *step,
double alpha,
double beta,
double gamma,
char **varname,
char *fname,
int maxnf
)
        {
        int i,j;
        int nf;
        int jh,js,jl;
        double xx[N+1][N];
        double y[N+1];
        double xc[N],x0[N],x00[N];
        double y0,y00;
// RS REM        int disp=1;
        int nc;
        double yc;


        nc=0; yc=1e300;
        nf=0;
        for (i=0; i<n; ++i) xx[0][i]=x[i];
        for (j=0; j<n; ++j)
            for (i=0; i<n; ++i)
                {
                if (i==j) xx[j+1][i]=x[i]+step[j];
                else      xx[j+1][i]=x[i];
                }
        for (i=0; i<n+1; ++i) y[i]=(*f)(xx[i]); nf+=n+1;



        while (1)
            {
            jh=0; js=1; jl=1; if (y[0]<y[1]) { jh=1; js=0; jl=0; }
            for (j=2; j<n+1; ++j)
                {
                if (y[j]>y[jh]) { js=jh; jh=j; }
                else if (y[j]>y[js]) js=j;
                else if (y[j]<y[jl]) jl=j;
                }

            for (i=0; i<n; ++i)
                {
                xc[i]=0.0;
                for (j=0; j<n+1; ++j)
                    if (j!=jh) xc[i]+=xx[j][i];
                xc[i]/=n;
                }

/****************************************
            if (sur_kbhit())
                {
                i=getch(); if (i=='.') break;
                disp=1-disp;
                }
            if (disp)
                {
                if (!capability[0]) { LOCATE(6,1); }
                else LOCATE(7,1);
                Rprintf("\n%s=%g    ",fname,y[jl]);
                for (i=0; i<n; ++i) Rprintf("\n%s=%g    ",varname[i],xx[jl][i]);
                Rprintf("\nnf=%d    ",nf);
                }
**************************************************/





            if (nf>=maxnf)   /* 14.7.90 */
                {
                for (i=0; i<n; ++i) x[i]=xx[jl][i];
                *py=y[jl];
                return(nf);
                }

            ++nc;
            if (nc>200)  // yli nc kpl samaa funktion arvoa!
                {
                if (y[j]==yc)
                    {
                    for (i=0; i<n; ++i) x[i]=xx[jl][i];
                    *py=y[jl];
                    return(nf);
                    }
                nc=0;
                yc=y[j];
                }
            for (i=0; i<n; ++i) x0[i]=(1+alpha)*xc[i]-alpha*xx[jh][i];
            y0=(*f)(x0); ++nf;

            if (nf>maxnf) return(-1);

            if (y[jl]<=y0 && y0<=y[js])
                {
                for (i=0; i<n; ++i) xx[jh][i]=x0[i];
                y[jh]=y0;
                continue;
                }
            if (y0<y[jl])
                {
                for (i=0; i<n; ++i) x00[i]=gamma*x0[i]+(1-gamma)*xc[i];
                y00=(*f)(x00); ++nf;
                if (nf>maxnf) return(-1);
                if (y00<y[jl])
                    {
                    for (i=0; i<n; ++i) xx[jh][i]=x00[i];
                    y[jh]=y00;
                    continue;
                    }
                else
                    {
                    for (i=0; i<n; ++i) xx[jh][i]=x0[i];
                    y[jh]=y0;
                    continue;
                    }
                }

            /* y0>ys */
            if (y0<y[jh])
                {
                for (i=0; i<n; ++i) x00[i]=beta*x0[i]+(1-beta)*xc[i];
                }
            else
                {
                for (i=0; i<n; ++i) x00[i]=beta*xx[jh][i]+(1-beta)*xc[i];
                }
            y00=(*f)(x00); ++nf;
            if (nf>maxnf) return(-1);
            if (y00<y[jh] && y00<y0)
                {
                for (i=0; i<n; ++i) xx[jh][i]=x00[i];
                y[jh]=y00;
                continue;
                }

            for (j=0; j<n+1; ++j)
                {
                if (j==jl) continue;
                for (i=0; i<n; ++i) xx[j][i]=(xx[j][i]+xx[jl][i])/2;
                y[j]=(*f)(xx[j]); ++nf;
                }
            if (nf>maxnf) return(-1);
            }
        for (i=0; i<n; ++i) x[i]=xx[jl][i];
        *py=y[jl];
        return(nf);
        }

static double logll(double *a)
        {
        extern double density();
        int i;
        double u,y;

        if (!integral_is_one)
            {
            total_integral(a);
            }
        u=0.0;
        for (i=0; i<n_class; ++i)
            {
            if (!freq[i]) continue;
            y=density(dnro,x_lower+i*x_step+x_step/2,a);
            if (y<=0.0) continue;
            u-=freq[i]*log(y/f_integral);
            }
        return(u);
        }

static int spfind2_histo(char *s,int i)
        {
        for ( ; i<spn; ++i)
                if (strcmp(s,spa[i])==0) return(i);
        return(-1);
        }


static int estim_results()
        {
        extern double logll();
        extern char *spois();
        int i,j;
        double *step;
        double *H;
        double *a;
        double st;
        int n_test;
        double y0,y1,b;
        char x[LLENGTH];
        int inv;

        if (nf<0)
            {
            output_open(eout);
            sprintf(x,"HISTO: Estimated parameters of %s: No convergence!",distr);
            eoutput(x);
            output_close(eout);
            return(-1);
            }

        H=(double *)muste_malloc(npar*(npar+1)*sizeof(double));
        if (H==NULL) { not_enough_memory(); return(-1); }
        step=(double *)muste_malloc(npar*sizeof(double));
        if (step==NULL) { not_enough_memory(); return(-1); }
        a=(double *)muste_malloc(npar*sizeof(double));
        if (a==NULL) { not_enough_memory(); return(-1); }

        y0=logll(dpar);
        for (i=0; i<npar; ++i)
            {
            st=0.001; n_test=0;
            for (j=0; j<npar; ++j) a[j]=dpar[j];
            while (1)
                {
                if (n_test>10) break;
                a[i]=dpar[i]+st;
                y1=logll(a);
                b=fabs(y0-y1)/(fabs(y0)+0.00001);
                if (b<0.000001) { ++n_test; st*=7.0; continue; }
                if (b<0.0001) break;
                ++n_test; st/=10.0;
                }

            if (b==0.0)
                {
                sprintf(sbuf,"Distribution independent of parameter %s???",parnimi[i]);
                p_error2(sbuf);
                return(-1);
                }
            step[i]=st;
            }

        numhess(dpar,H,npar,step);
        inv=cholinv(H,npar);
        for (i=0; i<npar; ++i) for (j=0; j<npar; ++j)
            {
            b=H[npar*(i+1)+j];
            H[npar*i+j]=b;
            }
        for (i=0; i<npar; ++i) for (j=0; j<=i; ++j)
            {
            b=H[npar*i+j];
            H[npar*j+i]=b;
            }

        output_open(eout);
        sprintf(x,"HISTO: Estimated parameters of %s:",distr);
        eoutput(x);
        for (i=0; i<npar; ++i)
            {
            char par[32],sd[32]; // RS REM ,nimi[9];

            fnconv(dpar[i],accuracy,par);
            if (inv==1) fnconv(sqrt(H[i*npar+i]),accuracy,sd);
            else strcpy(sd,"?");
            sprintf(x,"%s=%s (%s)",parnimi[i],spois(par),spois(sd));
            eoutput(x);
            }
        sprintf(x,"logL=%g  # of function evaluations =%d",y0,nf);
        eoutput(x);
        output_close(eout);

        if (npar>1 && inv==1)
            {
            corrnorm(H,npar);
            corrp(H,npar,parnimi,c3,7,3,"Correlations:");
            }
        if (inv!=1)
            {
            output_open(eout);
            eoutput("Not a true solution! Restart iteration!");
            output_close(eout);
            }
        muste_free(H); muste_free(step);
        return(1);
        }

static void numhess(double *a,double *H,int m,double *step)
        {
        extern double logll();

        int i,j;
        double f0,f1,f2,ai,aj;

        for (i=0; i<m; ++i) for (j=0; j<=i; ++j) H[m*i+j]=0;
        f0=logll(a);
        for (i=0; i<m; ++i)
            {
            ai=a[i]; a[i]+=step[i]; f1=logll(a); a[i]=ai;
            for (j=0; j<=i; ++j)
                {
                aj=a[j]; a[j]+=step[j]; f2=logll(a);
                ai=a[i]; a[i]+=step[i];
                H[m*i+j]=H[m*j+i]=(logll(a)-f2-f1+f0)/(step[i]*step[j]);
                a[i]=ai; a[j]=aj;
                }
            }
        }

static int corrnorm(double *H,int m)
        {
        int i,j;
        double diag[EP4];

        for (i=0; i<m; ++i)
            {
            if (H[i*m+i]<=0.0)
                {
                p_error2("Diagonal elements of a covariance matrix not positive!");
                return(-1); // RS ADD
                }
            diag[i]=sqrt(H[i*m+i]);
            }
        for (i=0; i<m; ++i)
            for (j=0; j<m; ++j)
                H[i*m+j]/=diag[i]*diag[j];
        return(1);
        }

static int cholinv(double a[],int n)
        {
        int i,j,k,i1,j1;
        double z,x,y=1;

        for (i=0; i<n; ++i)
            {
            i1=i+1;
            for (j=i; j<n; ++j)
                {
                j1=j+1;
                x=a[n*i+j];
                for (k=i-1; k>=0; --k)
                    x-=a[n*j1+k]*a[n*i1+k];
                if (j==i)
                    {
                    if (x<=0) return(-1);
                    a[n*i1+i]=y=1/sqrt(x);
                    }
                else a[n*j1+i]=x*y;
                }
            }

        for (i=0; i<n; ++i)
        for (j=i+1; j<n; ++j)
            {
            z=0;
            j1=j+1;
            for (k=j-1; k>=i; --k)
                z-=a[n*j1+k]*a[n*(k+1)+i];
            a[n*j1+i]=z*a[n*j1+j];
            }
        for (i=0; i<n; ++i)
        for (j=i; j<n; ++j)
            {
            z=0;
            j1=n;
            for (k=j+1; k<=j1; ++k)
                z+=a[n*k+j]*a[n*k+i];
            a[n*(j+1)+i]=z;
            }
        return(1);
        }


static void printout()
        {
        extern double muste_cdf_chi2();
        extern char *spois();
        int i,k,h;
        char x[LLENGTH];
        char form[32];
        int len;
        char sana[32];
        char label[32];
        long sum;
        char pros[16],sumpros[16];
        double minf=5.0;
        long rfreq;
        double rfreq_est;
        double chi2,a,sum2;
        int rn_class;
        int kok_osa; /* 19.10.1996 */
        char kok_lis[16];

        if (*distr)
            {
            minf=5.0;
            i=spfind("MINF");
            if (i>=0) { minf=arit_atof(spb[i]); if (minf<0.1) minf=5.0; }
            }

        kok_osa=1+log((double)n_freq)/log(10.0)+1e-7; if (kok_osa<4) kok_osa=4;
        i=kok_osa-4; strncpy(kok_lis,space,i); kok_lis[i]=EOS;
/* Rprintf("\nn_freq=%ld kok_osa=%d",n_freq,kok_osa); getch(); */
        output_open(eout);
        sprintf(x,"Frequency distribution of %s in %s: N=%ld",
                        varname,aineisto,n_freq);
        eoutput(x);
        *x=EOS; eoutput(x);
        k=sprintf(x,"Class midpoint  %sf     %%   %sSum     %%",kok_lis,kok_lis);
        if (*distr) k+=sprintf(x+k,"     %se      %se    %sf     X^2",
                                         kok_lis,kok_lis,kok_lis);
        eoutput(x);

        len=0;
        for (i=imin; i<=imax; ++i)  /* vain painoasua varten */
            {
            fconv0(x_lower+x_step/2+i*x_step,"",sana);
            if (strlen(sana)>len) { len=strlen(sana); strcpy(form,sana); }
            fconv0(x_lower+i*x_step,"",sana);
            if (strlen(sana)>len) { len=strlen(sana); strcpy(form,sana); }
            }
        sum=0L; sum2=0.0; chi2=0.0;
        rfreq=0; rfreq_est=0.0; rn_class=0;
        for (i=imin; i<=imax; ++i)
            {
            if (i==imin && i>0)
                {
                fconv0(x_lower+(imin+1)*x_step,form,sana);
                strcpy(label," <="); strcat(label,sana);
                }
            else if (i==imax && i<n_class-1)
                {
                fconv0(x_lower+imax*x_step,form,sana);
                strcpy(label," >"); strcat(label,sana);
                }
            else
                {
                fconv0(x_lower+x_step/2+i*x_step,form,label);
                }
            sum+=freq[i];
            fconv(100*(double)freq[i]/n_freq,"123.1",pros);
            fconv(100*(double)sum/n_freq,"123.1",sumpros);

            k=sprintf(x,"%12.12s %*ld %s %*ld %s",
                          label,kok_osa,freq[i],pros,kok_osa+1,sum,sumpros);

            if (*distr)
                {
                for (h=0; h<kok_osa; ++h) kok_lis[h]='1';
                kok_lis[kok_osa]=EOS; strcat(kok_lis,".1");
                fconv(freq_est[i],kok_lis,sana);
                k+=sprintf(x+k,"%s",sana);
                rfreq+=freq[i]; rfreq_est+=freq_est[i];
                sum2+=freq_est[i];

                if (i==imax || (rfreq_est>minf && (double)n_freq-sum2>minf))
                    {
                    ++rn_class;
                    fconv(rfreq_est,kok_lis,sana);
                    a=rfreq-rfreq_est; a=a*a/rfreq_est;
                    chi2+=a;
                    fconv(a,"12345.1",pros); /* chi2  */
                    k+=sprintf(x+k," %s %*ld %s",sana,kok_osa,rfreq,pros);
                    rfreq=0; rfreq_est=0.0;
                    }
                }
            eoutput(x);
            }

        mean_var(&a,&sum2,nof);
        fnconv(a,accuracy+2,sana);
        fnconv(sqrt(sum2),accuracy+2,pros);
        sprintf(x,"Mean=%s Std.dev.=%s",spois(sana),spois(pros));
        eoutput(x);

        if (*distr)
            {
            k=sprintf(x,"Fitted by %s",distr);
            if (npar) { strcat(x,"("); ++k; }
            for (i=0; i<npar; ++i)
                {
                fconv(dpar[i],"",sana);
                if (strlen(sana)>accuracy)
                    fnconv(dpar[i],accuracy,sana);
                k+=sprintf(x+k,"%s",spois(sana));
                if (i==npar-1) k+=sprintf(x+k,"%c",')');
                else           k+=sprintf(x+k,"%c",',');
                }
            sprintf(x+k," distribution");
            eoutput(x);
            fnconv(chi2,accuracy-1,sana);
            if (rn_class-1-npar_est-mat_est>0)
                {
                fconv(1-muste_cdf_chi2(chi2,(double)(rn_class-1-npar_est-mat_est),1e-7),"1.1234",pros);
                sprintf(x,"Chi-square=%s df=%d P=%s",spois(sana),rn_class-1-npar_est-mat_est,spois(pros));
                }
            else strcpy(x,"Degrees of freedom negative in Chi-square!");
            eoutput(x);
            }
        output_close(eout); /* 14.3.1990 */
        }


static void eoutput(char *rivi)
        {
        extern int capability[];

        output_line2(rivi,eout,results_line,capability[0]);
        if (results_line) ++results_line;
        }

static char *spois(char *s)  /* osoittaa ensimm. sp:stÑ eroavan merkin */
        {
        while (*s==' ') ++s;
        return(s);
        }

