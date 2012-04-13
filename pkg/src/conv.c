/* conv1.c 28.6.1988/SM (13.1.1992)
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <ctype.h> // RS
#include <limits.h> // RS
// #include <unistd.h> // RS
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

#define EOS '\0'
#define IGREG1 2299161
#define IGREG2 (15+31L*(10+12L*1582))


extern int spn;
extern char **spa,*spp;
extern unsigned int *spplace;

static char *epot3yks[]={ "one ","one thousand ","one million ","one milliard ","one billion " };
static char *epot3mon[]={ "","thousand ","million ","milliard ","billion " };
static char *e10[]={ "ten ","eleven ","twelve ","thirteen ","fourteen ","fifteen ",
                     "sixteen ","seventeen ","eighteen ","nineteen " };
static char *ekym[]={ "","","twenty","thirty","forty","fifty","sixty",
                      "seventy","eighty","ninety" };
static char *epot[]={ "","x","hundred " };
static char *enumero[]={"zero ","one ","two ","three ","four ","five ","six ","seven ",
                       "eight ","nine " };

static char *pot3yks[]={ "yksi ","tuhat ","miljoona ","miljardi ","biljoona " };
static char *pot3mon[]={ "","tuhatta ","miljoonaa ","miljardia ","biljoonaa " };
static char *pot[]={ "","kymment\204 ","sataa " };
static char *numero[]={"nolla ","yksi ","kaksi ","kolme ","nelj\204 ","viisi ","kuusi ",
                       "seitsem\204n ","kahdeksan ","yhdeks\204n " };
                       
// lyhennetyt sanat puhumista varten
static char *spot3yks[]={ "yksi ","tuhat ","milj ","milrd ","bilj " };
static char *spot3mon[]={ "","tuhatta ","miljs ","milrds ","biljs " };
static char *spot[]={ "","kymm ","sataa " };
static char *snumero[]={"nolla ","yksi ","kaksi ","kolme ","nelja ","viisi ","kuusi ","seitsem ",
                       "kahdeks ","yhdeks " };
                       

static unsigned char code[256];
static FILE *measures;
static int qed1,qed2,qrivi1,qrivi2;
static char avainrivi[LLENGTH];
static int col2,col3;
static int tarkkuus=-1;
static FILE *codes; /* -4.1.1997 defined as local! */
static int avattu=0;


/* Local declarations */
static int survo_conversion();
static int base_atol();
static int integer_conversion();

int muste_checkmp() // RS
  { 
  char line1[] = ".muste.mpchangebase <- function(instr,inbase,outbase) {";
  char line2[] = "mpfrvalue<-mpfr(instr,base=inbase)";
  char line3[] = "mpfrstrvalue<-formatMpfr(mpfrvalue,drop0trailing=TRUE)";
  char line4[] = "mpbigzvalue<-as.bigz(mpfrstrvalue)";
  char line5[] = "return(as.character(mpbigzvalue,b=outbase)) }";  
  
  if (!muste_requirepackage("Rmpfr")) return(FALSE);
  if (!muste_requirepackage("gmp")) return(FALSE);
  
  sprintf(sbuf,"%s\n%s\n%s\n%s\n%s\n",line1,line2,line3,line4,line5);
  muste_evalr(sbuf);
  
  return TRUE;
  }

/*
int muste_isnumber(char *number)
{
	 while (*number==' ') number++; // RS Remove pre-spaces	 
     if (strlen(number)==strspn(number,"0123456789.")) return TRUE;
     return FALSE;
}
*/

int muste_isnumber(const char * s)
	{
    char *p;
    
    if (s == NULL || *s == '\0' || isspace(*s))  return 0;
    while (*s==' ') s++; // RS Remove pre-spaces	  
    strtod (s, &p);
    return *p == '\0';
	}


/*
**  LTOA.C
**
**  Converts a long integer to a string.
**
**  Copyright 1988-90 by Robert B. Stout dba MicroFirm
**
**  Released to public domain, 1991
**
**  Parameters: 1 - number to be converted
**              2 - buffer in which to build the converted string
**              3 - number base to use for conversion
**
**  Returns:  A character pointer to the converted string if
**            successful, a NULL pointer if the number base specified
**            is out of range.
*/

// #include <stdlib.h>
// #include <string.h>

#define BUFSIZE (sizeof(long) * 8 + 1)

char *muste_ltoa(long N, char *str, int base)
{
      register int i = 2;
      long uarg;
      char *tail, *head = str, buf[BUFSIZE];

      if (36 < base || 2 > base)
            base = 10;                    /* can only use 0-9, A-Z        */
      tail = &buf[BUFSIZE - 1];           /* last character position      */
      *tail-- = '\0';

      if (10 == base && N < 0L)
      {
            *head++ = '-';
            uarg    = -N;
      }
      else  uarg = N;

      if (uarg)
      {
            for (i = 1; uarg; ++i)
            {
                  register ldiv_t r;

                  r       = ldiv(uarg, base);
                  *tail-- = (char)(r.rem + ((9L < r.rem) ?
                                  ('A' - 10L) : '0'));
                  uarg    = r.quot;
            }
      }
      else  *tail-- = '0';

      memcpy(head, ++tail, i);
      return str;
}


/* jul.c 18.11.1992/SM
   Calendar routines caldat, julday from "Numerical Recipes"
*/

static void caldat(long julian,int *mm,int *id,int *iyyy)
{
        long ja,jalpha,jb,jc,jd,je;

        if (julian >= IGREG1) {
                jalpha=((double) (julian-1867216)-0.25)/36524.25;
                ja=julian+1+jalpha-(long) (0.25*jalpha);
        } else
                ja=julian;
        jb=ja+1524;
        jc=6680.0+((double) (jb-2439870)-122.1)/365.25;
        jd=365*jc+(0.25*jc);
        je=(jb-jd)/30.6001;
        *id=jb-jd-(int) (30.6001*je);
        *mm=je-1;
        if (*mm > 12) *mm -= 12;
        *iyyy=jc-4715;
        if (*mm > 2) --(*iyyy);
        if (*iyyy <= 0) --(*iyyy);
}

static long julday(int mm,int id,int iyyy)
{
        long jul;
        int ja,jy,jm;

        if (iyyy == 0)
            {
            sur_print("\nThere is no year 0 !");
            WAIT;
            }
        if (iyyy < 0) ++iyyy;
        if (mm > 2) {
                jy=iyyy;
                jm=mm+1;
        } else {
                jy=iyyy-1;
                jm=mm+13;
        }
        jul = (long) (floor(365.25*jy)+floor(30.6001*jm)+id+1720995);
        if (id+31L*(mm+12L*iyyy) >= IGREG2) {
                ja=0.01*jy;
                jul += 2-ja+(int) (0.25*ja);
        }
        return jul;
}

static int date_error(char *word)
        {
        sprintf(sbuf,"\nError in date: %s ",word); sur_print(sbuf);
        WAIT; return(1);
        }


static int date_conv2(long timet,int type,char *res)
        {
        int day,mon,year;

  /* type=1 */
        caldat(timet,&mon,&day,&year);
        sprintf(res,"%d.%d.%d",day,mon,year);
        return(1);
        }


static int date_conv1(char *word,int type,long *ptime)
        {
        char x[LLENGTH];
        char *p,*q;
        int i;
        int day,mon,year;
        extern long julday();

        strcpy(x,word);
        if (type==1)  /* d.m.y */
            {
            p=strchr(x,'.');
            if (p==NULL) { date_error(word); return(-1); }
            *p=EOS;
            i=atoi(x);
            if (i<1 || i>31) { date_error(word); return(-1); }
            day=i;
            ++p; q=strchr(p,'.');
            if (q==NULL) { date_error(word); return(-1); }
            *q=EOS; i=atoi(p);
            if (i<1 || i>12) { date_error(word); return(-1); }
            mon=i;
            ++q;
            i=atoi(q);
            year=i;

            *ptime=julday(mon,day,year)+1L;
            }
        return(1);
        }


static int edesimaaliosa(char *s,char *sanat)
        {
        char *p;

        strcat(sanat,"point ");
        p=s;
        while (*p)
            {
            strcat(sanat,enumero[*p-'0']);
            ++p;
            }
        return(1);
        }

static int sdesimaaliosa(char *s,char *sanat)
        {
        char *p;

        strcat(sanat,"pilkku ");
        p=s;
        while (*p)
            {
            strcat(sanat,snumero[*p-'0']);
            ++p;
            }
        return(1);
        }

static int desimaaliosa(char *s,char *sanat)
        {
        char *p;

        strcat(sanat,"pilkku ");
        p=s;
        while (*p)
            {
            strcat(sanat,numero[*p-'0']);
            ++p;
            }
        return(1);
        }

static int eluku_sanoina(char *luku,char *sanat)
        {
        int i,j,n,m,len;
        char *p;
        char *q;
        double arvo;
        char x[100];

        *sanat=EOS;
        if(!muste_isnumber(luku)) return(0);
        p=luku;
        while (*p==' ') ++p;
        if (*p=='+') { strcat(sanat,"plus "); ++p; }
        else if (*p=='-') { strcat(sanat,"minus "); ++p; }
        if (*p==EOS) return(1);
        q=strchr(p,'.');
        if (q!=NULL) *q=EOS;
        arvo=atof(p);
        if (arvo>1e15) { strcpy(sanat,luku); return(0); }
        if (arvo==0.0)
            {
            strcat(sanat,"zero ");
            if (q!=NULL) desimaaliosa(q+1,sanat);
            return(1);
            }
        x[0]=x[1]=' '; strcpy(x+2,p);
        p=x+2;
        len=strlen(p);
        n=(strlen(p)-1)/3;
        for (i=n; i>=0; --i)
            {
            m=len-3*i-1;

            if ( (p[m]=='1' || p[m]=='0') && (p[m-1]==' ' || p[m-1]=='0')
                          && (p[m-2]==' ' || p[m-2]=='0') )
                {
                if (p[m]=='1')
                    strcat(sanat,epot3yks[i]);
                else continue;
                }
            else
                {
                for (j=2; j>=0; --j)
                    {
                    if (p[m-j]==' ') continue;
                    if (p[m-j]=='0') continue;
                    if (p[m-j]=='1')
                        {
                        if (j==1)
                            {
                            strcat(sanat,e10[p[m]-'0']); break;
                            }
                        else if (j==2)
                            strcat(sanat,"one hundred ");
                        else
                            strcat(sanat,"one");
                        }
                    else
                        {

                        if (*epot[j]=='x')
                            {
                            strcat(sanat,ekym[p[m-j]-'0']);
                            if (p[m-j+1]=='0') strcat(sanat," ");
                            else strcat(sanat,"-");
                            }
                        else
                            {
                            strcat(sanat,enumero[p[m-j]-'0']);
                            strcat(sanat,epot[j]);
                            }
                        }
                    }
                strcat(sanat,epot3mon[i]);
                }
            }
        if (q!=NULL)
            edesimaaliosa(q+1,sanat);
        return(1);
        }


static int eluku_sanoiksi(char *word,char *par1,char *res)
        {
        int i;
        char res1[LLENGTH];

        if (strcmp(par1,"10")==0) strcpy(res1,word);
        else
            i=survo_conversion(word,par1,"10",res1);
        eluku_sanoina(res1,res);
        return(1);
        }

static int luku_sanoina2(char *luku,char *sanat)
        {
        int i,j,n,m,len;
        char *p;
        char *q;
        double arvo;
        char x[100],y[16];

        *sanat=EOS;
        if(!muste_isnumber(luku)) return(0);
        p=luku;
        while (*p==' ') ++p;
        if (*p=='+') { strcat(sanat,"plus "); ++p; }
        else if (*p=='-') { strcat(sanat,"miinus "); ++p; }
        if (*p==EOS) return(1);
        q=strchr(p,'.');
        if (q!=NULL) *q=EOS;
        arvo=atof(p);
        if (arvo>1e15) { strcpy(sanat,luku); return(0); }
        if (arvo==0.0)
            {
            strcat(sanat,"nolla ");
            if (q!=NULL) desimaaliosa(q+1,sanat);
            return(1);
            }
        x[0]=x[1]=' '; strcpy(x+2,p);
        p=x+2;
        len=strlen(p);
        n=(strlen(p)-1)/3;
        for (i=n; i>=0; --i)
            {
            m=len-3*i-1;

            if ( (p[m]=='1' || p[m]=='0') && (p[m-1]==' ' || p[m-1]=='0')
                          && (p[m-2]==' ' || p[m-2]=='0') )
                {
                if (p[m]=='1')
                    strcat(sanat,spot3yks[i]);
                else continue;
                }
            else
                {
                for (j=2; j>=0; --j)
                    {
                    if (p[m-j]==' ') continue;
                    if (p[m-j]=='0') continue;
                    if (p[m-j]=='1')
                        {
                        if (j==1)
                            {
                            if (p[m]=='0') { strcat(sanat,"kymmenen "); break; }
  /*       else { strcat(sanat,numero[p[m]-'0']); strcat(sanat,"toista "); break; }
  */                        else
                                {
                                strcpy(y,snumero[p[m]-'0']);
                                y[strlen(y)-1]=EOS;
                                strcat(sanat,y); strcat(sanat," toista ");
                                break;
                                }

                            }
                        else if (j==2)
                            strcat(sanat,"sata ");
                        else
                            strcat(sanat,"yksi ");
                        }
                    else
                        {
                        strcat(sanat,snumero[p[m-j]-'0']);
                        strcat(sanat,spot[j]);
                        }
                    }
                strcat(sanat,spot3mon[i]);
                }
            }
        if (q!=NULL)
            sdesimaaliosa(q+1,sanat);
        return(1);
        }


static int luku_sanoiksi2(char *word,char *par1,char *res)
        {
        int i;
        char res1[LLENGTH];

        if (strcmp(par1,"10")==0) strcpy(res1,word);
        else
            i=survo_conversion(word,par1,"10",res1);
        luku_sanoina2(res1,res);
        return(1);
        }

static int luku_sanoina(char *luku,char *sanat)
        {
        int i,j,n,m,len;
        char *p;
        char *q;
        double arvo;
        char x[100],y[16];

        *sanat=EOS;
        if(!muste_isnumber(luku)) return(0);
        p=luku;
        while (*p==' ') ++p;
        if (*p=='+') { strcat(sanat,"plus "); ++p; }
        else if (*p=='-') { strcat(sanat,"miinus "); ++p; }
        if (*p==EOS) return(1);
        q=strchr(p,'.');
        if (q!=NULL) *q=EOS;
        arvo=atof(p);
        if (arvo>1e15) { strcpy(sanat,luku); return(0); }
        if (arvo==0.0)
            {
            strcat(sanat,"nolla ");
            if (q!=NULL) desimaaliosa(q+1,sanat);
            return(1);
            }
        x[0]=x[1]=' '; strcpy(x+2,p);
        p=x+2;
        len=strlen(p);
        n=(strlen(p)-1)/3;
        for (i=n; i>=0; --i)
            {
            m=len-3*i-1;

            if ( (p[m]=='1' || p[m]=='0') && (p[m-1]==' ' || p[m-1]=='0')
                          && (p[m-2]==' ' || p[m-2]=='0') )
                {
                if (p[m]=='1')
                    strcat(sanat,pot3yks[i]);
                else continue;
                }
            else
                {
                for (j=2; j>=0; --j)
                    {
                    if (p[m-j]==' ') continue;
                    if (p[m-j]=='0') continue;
                    if (p[m-j]=='1')
                        {
                        if (j==1)
                            {
                            if (p[m]=='0') { strcat(sanat,"kymmenen "); break; }
  /*       else { strcat(sanat,numero[p[m]-'0']); strcat(sanat,"toista "); break; }
  */                        else
                                {
                                strcpy(y,numero[p[m]-'0']);
                                y[strlen(y)-1]=EOS;
                                strcat(sanat,y); strcat(sanat,"toista ");
                                break;
                                }

                            }
                        else if (j==2)
                            strcat(sanat,"sata ");
                        else
                            strcat(sanat,"yksi ");
                        }
                    else
                        {
                        strcat(sanat,numero[p[m-j]-'0']);
                        strcat(sanat,pot[j]);
                        }
                    }
                strcat(sanat,pot3mon[i]);
                }
            }
        if (q!=NULL)
            desimaaliosa(q+1,sanat);
        return(1);
        }


static int luku_sanoiksi(char *word,char *par1,char *res)
        {
        int i;
        char res1[LLENGTH];

        if (strcmp(par1,"10")==0) strcpy(res1,word);
        else
            i=survo_conversion(word,par1,"10",res1);
        luku_sanoina(res1,res);
        return(1);
        }


static void conversion_impossible(char *par1,char *par2)
        {
        sprintf(sbuf,"\nConversion from %s to %s is impossible!",par1,par2);
        sur_print(sbuf); WAIT;
        }

static void illegal_operand(char *s)
        {
        sprintf(sbuf,"\nIllegal operand %s",s); sur_print(sbuf);
        WAIT;
        }


static int factlist(char *res,unsigned long factor,int n,int *pjatko)
        {
        int i;

        i=0;
        if (*pjatko) i+=sprintf(res,"*");
        i+=sprintf(res+i,"%lu",factor);
        if (n>1) i+=sprintf(res+i,"^%d",n);
        *pjatko=1;
        return(i);
        }



static int nfact_conv(unsigned long *pluku,unsigned long factor)
        {
        int n=0;

        while ((unsigned long)*pluku%factor==0)
            {
            ++n;
            *pluku/=factor;
            }
        return(n);
        }


static int factors(char *word,char *base,char *res)
        {
        unsigned long luku,factor,maxfactor;
        int i,j,k;
        int jatko=0;
        double d;
        char factres[LLENGTH];  // RS ADD
        char factresold[LLENGTH]; // RS ADD

/*  Rprintf("\nfactors: word=%s base=%s",word,base); getch();  */
        d=atof(word);
/* RS REM        
        if (atoi(base)==10 && d>4294967295.0)
            {
            sur_print("\nMax. permitted integer 4294967295=2^32-1");
            WAIT; return(-1);
            }
*/            
        if (d<0.0) return(-1);
        if (d<2.0) { strcpy(res,word); return(1); }

        i=base_atol(word,atoi(base),&luku);
        if (i<0) return(-1);        
        
        if (i==2) // RS i=2 from base_atol above indicates multiple precision
          {                    
          i=atoi(base);
          if (i<2 || i>36)
            {
            sur_print("\nOnly bases 2,3,...,36 are permitted!"); return(-1);
            }
          muste_set_R_string(".muste$mpinstr",word);
          if (i!=10)
            {
            sprintf(sbuf,".muste$mpinstr<-.muste.mpchangebase(.muste$mpinstr,%d,10)",i);
            muste_evalr(sbuf);         
            } 
          muste_evalr(".muste$mpoutstr<-as.character(factorize(.muste$mpinstr))");
          muste_evalr(".muste$mpoutstr.length<-as.integer(length(.muste$mpoutstr))");          
          k=muste_get_R_int(".muste$mpoutstr.length");
        
          i=1; j=1;
          sprintf(sbuf,".muste$mpoutstr.comp<-as.character(.muste$mpoutstr[%d])",i);
          muste_evalr(sbuf);
          muste_get_R_string(factres,".muste$mpoutstr.comp",LLENGTH);
          strcpy(res,factres);
          strcpy(factresold,factres);          
          
          for (i=2; i<=k; i++)
            {
            sprintf(sbuf,".muste$mpoutstr.comp<-as.character(.muste$mpoutstr[%d])",i);
            muste_evalr(sbuf);
            muste_get_R_string(factres,".muste$mpoutstr.comp",LLENGTH);
            if(strcmp(factres,factresold)==0) j++;
            else
              {
              if (j>1) { sprintf(sbuf,"^%d",j); strcat(res,sbuf); j=1; }            
              strcat(res,"*");
              strcat(res,factres);
              }            
            strcpy(factresold,factres);             
            }            
          }
        else        
          {
            k=0;
            i=nfact_conv(&luku,2L);           
            if (i) k=factlist(res,2L,i,&jatko);
            factor=3L;
            maxfactor=(unsigned long)sqrt((double)(luku));
            while (maxfactor>=factor)
                {
                i=nfact_conv(&luku,factor);
                if (i)
                    {
                    k+=factlist(res+k,factor,i,&jatko);
                    maxfactor=(unsigned long)sqrt((double)(luku));
                    }
                factor+=2;
                }
            if (luku>1L) k+=factlist(res+k,luku,1,&jatko);
          }
        
        return(1);
        }


static int numeric(char *word,char *par1,char *par2,char *res,
                   char laji1,char laji2,char *xpar1,char *xpar2)
        {
        char base1[LLENGTH],base2[LLENGTH];
/*
printf("\nnumeric: xpar1=%s xpar2=%s laji1=%c laji2=%c",xpar1,xpar2,laji1,laji2); getch();
*/
        *base1=*base2=EOS;
        if (laji1=='X' && *xpar1=='N')  strcpy(base1,xpar1+1);
        if (laji1=='0') strcpy(base1,par1);
        if (laji2=='X' && *xpar2=='N')  strcpy(base2,xpar2+1);
        if (laji2=='0') strcpy(base2,par2);

        if (*base1==EOS || *base2==EOS)
            { conversion_impossible(par1,par2); return(-1); }
        if (strncmp(xpar2+1,"factors",7)==0)
            return( factors(word,base1,res) );
        return( integer_conversion(word,base1,base2,res) );
        }


static void diss(double x,double ear,long *pm,long *pn)
        {
        long m,k,mm,nn;
        double f,a,b,diss;
        int vaihto;
        
        vaihto=0; mm=0; nn=0;

/* Rprintf("\nx=%g ear=%g",x,ear); getch();   */
        if (x==0.0) { *pm=0; *pn=0; return; }
        if (x<1.0) { x=1/x; vaihto=1; }
        f=1e10; m=k=0; a=pow(10.0,ear);  /* k vastaa parametria n */
        while ((double)k<f)
            {
            ++m; k=m*x; diss=m*x-k;
            if (diss>=0.5) { diss=1.0-diss; ++k; }
            b=diss/k; b=k+a*b*b;
            if (b<f) { f=b; mm=m; nn=k; }
            }
        if (vaihto) { *pm=nn; *pn=mm; } else { *pm=mm; *pn=nn; }
        }


static int ratio(char *word,char *par1,char *par2,char *res,char laji1,char laji2,
                 char *xpar1,char *xpar2)
        {
        double x,ear;
        long m,n;
        char y[LLENGTH];
        char sign[2];

/*
printf("\nratio: xpar1=%s xpar2=%s laji1=%c laji2=%c",xpar1,xpar2,laji1,laji2); getch();
*/
        x=atof(word);
        *sign=EOS;
        if (x<0.0) { x=-x; *sign='-'; sign[1]=EOS; }
        if (laji1=='0')
            ear=atof(par1);
        else
            ear=atof(par2);
        diss(x,ear,&m,&n);
        if (m==0L && n==0L)
            strcpy(res,"0");
        else
            {
            fconv(x-(double)n/(double)m,"",y);
            sprintf(res,"%s%ld/%ld (%s)",sign,n,m,y);
            }
        return(1);
        }


static int times(char *word,char *par1,char *par2,char *res,char laji1,char laji2,
                 char *xpar1,char *xpar2,double prefix1,double prefix2)
        {
        int i;
        char x[LLENGTH];
        char *min,*sec;
        double a;
        long timet;
/*
printf("\ntimes: xpar1=%s xpar2=%s laji1=%c laji2=%c",xpar1,xpar2,laji1,laji2); getch();
*/
        if (strncmp(xpar1,"t d.m.y",7)==0) /* 17.11.1992 */
            {
            if (laji2!='T') { conversion_impossible(par1,par2); return(-1); }
            i=date_conv1(word,1,&timet); if (i<0) return(-1);
            a=(double)timet/atof(xpar2)/prefix2*(double)86400;
            fconv(a,"",res);
            return(1);
            }
        if (strncmp(xpar2,"t d.m.y",7)==0)
            {
            if (laji1!='T') { conversion_impossible(par1,par2); return(-1); }
            timet=atof(word)*prefix1*atof(xpar1)/(double)86400;
            i=date_conv2(timet,1,res); if (i<0) return(-1);
            return(1);
            }

        if (strncmp(xpar1,"t h:m:s",7)==0)
            {
            if (laji2!='T') { conversion_impossible(par1,par2); return(-1); }
            strcpy(x,word);
            min=strchr(x,':'); if (min==NULL) { illegal_operand(word); return(-1); }
            *min=EOS; ++min;
            sec=strchr(min,':'); if (sec==NULL) { illegal_operand(word); return(-1); }
            *sec=EOS; ++sec;
            a=atof(sec)+60*(atof(min)+60*atof(x));
            a/=atof(xpar2)*prefix2;
            fconv(a,"",res);
            return(1);
            }
        if (strncmp(xpar2,"t h:m:s",7)==0)
            {
            long h2,m2,s2;

            if (laji1!='T') { conversion_impossible(par1,par2); return(-1); }
            a=atof(word)*prefix1*atof(xpar1);  /* sec */
            h2=a/3600; a-=3600*h2;
            if (a<0.0) a=-a;
            m2=a/60; a-=60*m2;
            s2=a+0.5;
            sprintf(res,"%02ld:%02ld:%02ld",h2,m2,s2);
            return(1);
            }

        return(1);
        }


static int currencies(char *word,char *par1,char *par2,char *res,char *xpar1,char *xpar2)
        {
        int i;
        double amount;
        char x[LLENGTH], *osa[2];
        double rate1,rate2;

        if (*xpar1!=*xpar2) { conversion_impossible(par1,par2); return(-1); }
        ++xpar1; ++xpar2;
        amount=atof(word);
        strcpy(x,xpar1);
        i=split(x,osa,2);
        rate1=atof(osa[0]);
        strcpy(x,xpar2);
        i=split(x,osa,2);
        if (i<2) rate2=atof(osa[0]); else rate2=atof(osa[1]);
        amount=amount*rate1/rate2;
        fconv(amount,"",res);
        return(1);
        }


static int temperatures(char *word,char *par1,char *par2,char *res,char *xpar1,char *xpar2)
        {
        double temp;
        double kelvin=-273.15;

        if (*xpar1!=*xpar2) { conversion_impossible(par1,par2); return(-1); }
        ++xpar1; ++xpar2;
        temp=atof(word);
        switch (*xpar1)
            {
          case 'C':
            switch (*xpar2)
                {
              case 'C': break;
              case 'F': temp=1.8*temp+32; break;
              case 'K': temp-=kelvin; break;
                }
            break;
          case 'F':
            switch (*xpar2)
                {
              case 'C': temp=(temp-32)/1.8; break;
              case 'F': break;
              case 'K': temp=(temp-32)/1.8-kelvin;
                }
            break;
          case 'K':
            switch (*xpar2)
                {
              case 'C': temp+=kelvin; break;
              case 'F': temp=1.8*(temp+kelvin)+32; break;
              case 'K': break;
                }
            break;
            }
        fconv(temp,"",res);
        return(1);
        }


static int x_conversion(char *word,char *par1,char *par2,char *res,char laji1,char laji2,
                        char *xpar1,char *xpar2,double prefix1,double prefix2)
        {
        char laji;
/*
printf("\nword=%s par1=%s par2=%s laji1=%c laji2=%c xpar1=%s xpar2=%s",
          word,par1,par2,laji1,laji2,xpar1,xpar2); getch();
*/

        laji=*xpar1;
        switch (laji)
            {
          case 'T':  return( temperatures(word,par1,par2,res,xpar1,xpar2) );
          case 'C':  return( currencies(word,par1,par2,res,xpar1,xpar2) );

            }
        if (*xpar1=='t' || *xpar2=='t')
            return( times(word,par1,par2,res,laji1,laji2,xpar1,xpar2,prefix1,prefix2) );
        if (*xpar1=='r' || *xpar2=='r')
            return( ratio(word,par1,par2,res,laji1,laji2,xpar1,xpar2) );
        if (*xpar1=='N' || *xpar2=='N')
            return( numeric(word,par1,par2,res,laji1,laji2,xpar1,xpar2) );
        return(0);
        }



static void convert_low(unsigned char *s) // RS ADD unsigned
        {
        while (*s) { *s=code[(unsigned char)*s]; ++s; } // RS CHA (int)
        }

static void qedread(char *s,int j)
        {
        int i;

        muste_fseek(measures,(long)(j*qed1),0);  // RS FIXME int vs. long
        for (i=0; i<qed1; ++i) s[i]=(unsigned char)getc(measures); // RS ADD unsigned
        s[qed1]=EOS;
        }

static int etsi(unsigned char *s1)
        {
        int j;
        int alku, loppu;
        int vert, len;
        int jmin,jmax;
        char avain[32];
        char s[32];

        alku=qrivi1; loppu=qrivi2;
        
        strncpy(s,(char *)s1,32); // RS CHA ADD (char *)
        len=strlen(s);
        s[len]=' '; ++len; s[len]=EOS;
        convert_low((unsigned char *)s); // RS ADD (unsigned char *)
        while (1)
            {
            if (loppu-alku<=1) { jmin=alku; jmax=loppu; break; }
            j=(alku+loppu)/2;            
            qedread(avainrivi,j);
            strncpy(avain,avainrivi+1,col2-1); avain[col2-1]=EOS;           
            convert_low((unsigned char *)avain); // RS ADD (unsigned char *)
//Rprintf("\nhaku=%s alku=%d loppu=%d %s",s,alku,loppu,avain);             
            vert=strncmp(s,avain,len);
            if (vert==0) { jmin=j; jmax=j; break; }
            if (vert<0) loppu=j; else alku=j;
            }
/*  Rprintf("\njmin=%d jmax=%d",jmin,jmax); getch();     */
        if (jmin==jmax) return(1);
        return(-1);
        }

static int avaa(char *edq)    /* lainattu kyselysysteemist‰ cq.c */
        {
        int i;
        char rivi[ELE], *sana[3];

        if (avattu) return(1);
        measures=muste_fopen(edq,"rb");
        if (measures==NULL)
            {
            sprintf(sbuf,"\nFile of measures %s is not found!",edq); sur_print(sbuf);
            WAIT; return(-1);
            }
        avattu=1;
        for (i=0; i<ELE; ++i) rivi[i]=(char)getc(measures);
        rivi[ELE-1]=EOS;
        i=split(rivi,sana,3);
        qed1=atoi(sana[1]); qed2=atoi(sana[2]);

        qedread(rivi,2);
        i=split(rivi+1,sana,2);
        qrivi1=atoi(sana[0]); qrivi2=atoi(sana[1]);
/*
printf("\n qed1=%d qed2=%d qrivi1=%d qrivi2=%d",qed1,qed2,qrivi1,qrivi2);
getch();
*/
        return(1);
        }


static int load_codes(unsigned char *code)
        {
        int i;
        char nimi[LLENGTH];

        strcpy(nimi,survo_path); strcat(nimi,"SYS/SORTLOW.BIN"); // RS \\ -> /
        codes=muste_fopen(nimi,"rb");
        if (codes==NULL)
            {
            sprintf(sbuf,"\nFilter file %s not found!",nimi); sur_print(sbuf);
            WAIT; return(-1);
            }
        for (i=0; i<256; ++i) code[i]=(unsigned char)getc(codes);
        muste_fclose(codes);
        return(1);
        }

static void not_found(char *par)
        {
        sprintf(sbuf,"\nWord %s unknown!",par); sur_print(sbuf);
        WAIT;
        }

static int tutki_prefix(char *par,double *pprefix)
        {
        if (strncmp(par,"exa",3)==0) { *pprefix=1e18; return(3); }
        if (strncmp(par,"peta",4)==0) { *pprefix=1e15; return(4); }
        if (strncmp(par,"tera",4)==0) { *pprefix=1e12; return(4); }
        if (strncmp(par,"giga",4)==0) { *pprefix=1e9; return(4); }
        if (strncmp(par,"mega",4)==0) { *pprefix=1e6; return(4); }
        if (strncmp(par,"kilo",4)==0) { *pprefix=1e3; return(4); }
        if (strncmp(par,"hecto",5)==0) { *pprefix=1e2; return(5); }
        if (strncmp(par,"hehto",5)==0) { *pprefix=1e2; return(5); }
        if (strncmp(par,"deca",4)==0) { *pprefix=10.0; return(4); }
        if (strncmp(par,"deka",4)==0) { *pprefix=10.0; return(4); }
        if (strncmp(par,"deci",4)==0) { *pprefix=0.1; return(4); }
        if (strncmp(par,"desi",4)==0) { *pprefix=0.1; return(4); }
        if (strncmp(par,"centi",5)==0) { *pprefix=1e-2; return(5); }
        if (strncmp(par,"sentti",6)==0) { *pprefix=1e-2; return(6); }
        if (strncmp(par,"milli",5)==0) { *pprefix=1e-3; return(5); }
        if (strncmp(par,"micro",5)==0) { *pprefix=1e-6; return(5); }
        if (strncmp(par,"mikro",5)==0) { *pprefix=1e-6; return(5); }
        if (strncmp(par,"nano",4)==0) { *pprefix=1e-9; return(4); }
        if (strncmp(par,"pico",4)==0) { *pprefix=1e-12; return(4); }
        if (strncmp(par,"piko",4)==0) { *pprefix=1e-12; return(4); }
        if (strncmp(par,"femto",5)==0) { *pprefix=1e-15; return(5); }
        if (strncmp(par,"atto",4)==0) { *pprefix=1e-18; return(4); }
        if (*par=='k')                { *pprefix=1e3; return(1); }
        if (*par=='m')                { *pprefix=1e-3; return(1); }
        if (*par=='M')                { *pprefix=1e6; return(1); }
        return(-1);
        }


static int mitta(char *par0,char *plaji,char *kerroin,double *pprefix)
        {
        int i,k;
        char par[LLENGTH]; // RS ADD unsigned

        *pprefix=1.0;
        strcpy(par,par0);
        if (muste_isnumber(par))
            {
            *plaji='0'; *kerroin='0'; kerroin[1]=EOS; return(1);
            }
        i=etsi((unsigned char *)par); // RS ADD (unsigned char *)
        if (i<0)
            {
            k=tutki_prefix(par,pprefix);
            if (k<0) { not_found(par); return(-1); }
            i=etsi((unsigned char *)(par+k)); // RS ADD (unsigned char *)par+k);
            if (i<0) { not_found(par); return(-1); }
            }
        if (avainrivi[col2+1]=='-')    /* 12.1.90  ennen [col2] */
            {
            strcpy(par,avainrivi+col3);
            i=strlen(par); while(par[i-1]==' ') par[--i]=EOS;
            i=etsi((unsigned char *)par); // RS ADD (unsigned char *)
            if (i<0) { not_found(par); return(-1); }
            }
        *plaji=avainrivi[col2];
        strcpy(kerroin,avainrivi+col3);
        return(1);
        }


static int num_conversion(char *word,char *par1,char *par2,char *res)
        {
        int i;
        char laji1,laji2;
        char kerroin1[LLENGTH],kerroin2[LLENGTH];
        char nimi[LLENGTH];
        double prefix1,prefix2,aa;
        char *p;
        char x[LLENGTH];

        *res=EOS;
        i=load_codes(code); if (i<0) return(-1);
        col2=19; col3=21;
        i=hae_apu("measures",nimi);
        if (!i) { strcpy(nimi,survo_path); strcat(nimi,"SYS/MEASURES.EDT"); } // RS CHA \\ -> /
        i=avaa(nimi);
        i=mitta(par1,&laji1,kerroin1,&prefix1); if (i<0) return(-1);
        i=mitta(par2,&laji2,kerroin2,&prefix2); if (i<0) return(-1);
        if (laji1=='X' || laji2=='X')
         return( x_conversion(word,par1,par2,res,laji1,laji2,kerroin1,kerroin2,prefix1,prefix2) );
/*
printf("\nkerroin1=%s kerroin2=%s",kerroin1,kerroin2);
printf("\nlaji1=%c laji2=%c",laji1,laji2); getch();
*/
        if (laji1!=laji2)
            {
            sprintf(sbuf,"\nConversion from %s to %s is impossible!",par1,par2);
            sur_print(sbuf); WAIT; return(-1);
            }

        aa=atof(word)*prefix1*atof(kerroin1)/atof(kerroin2)/prefix2;
        if (tarkkuus<0)
            {
            i=spfind("ACCURACY");
            if (i>=0) tarkkuus=atoi(spb[i]); else tarkkuus=0;
            }
        if (tarkkuus)
            {
            fnconv(aa,tarkkuus+3,x);
            p=x; while(*p && *p==' ') ++p;
            strcpy(res,p);
            if (strchr(res,'.')!=NULL && strchr(res,'e')==NULL)
                {
                p=res+strlen(res)-1;
                while (*p=='0') { *p=EOS; --p; }
                if (*p=='.') *p=EOS;
                }
            }
        else
            fconv(aa,"",res);
        return(1);
        }

static void illegal_char(char ch,int base,char *s)
        {
        sprintf(sbuf,"\nIllegal character %c in base %d integer %s",ch,base,s);
        sur_print(sbuf); WAIT;
        }

static int numdigits(double num) // RS
  {  
  int length;
  char digits[100];
  sprintf(digits, "%.0f", num);
  length = strlen(digits) - (num<0 ? 1 : 0);
  
  return(length);  

  }

static int base_atol(char *s,int base,long *pluku)
        {
        int i;
        char ch;
        int digit;
        double lf,powf; // RS

        if (base<2 || base>36)
            {
            sur_print("\nOnly bases 2,3,...,36 are permitted!");
            WAIT; return(-1);
            }
        lf=0; powf=1; // RS CHA l->lf pow->powf
        i=strlen(s)-1;
        while (1)
            {
            ch=s[i];
            if (i==0)
                {
                if (ch=='-') { lf=-lf; break; }
                }
            if (ch<'0') { illegal_char(ch,base,s); return(-1); }
            digit=ch-'0';
            if (digit>9) digit-=7;
            if (digit>41) digit-=32;
            if (digit>=base) { illegal_char(ch,base,s); return(-1); }
            lf+=digit*powf;
            powf*=(double)base;
            if (i==0) break;
            --i;
            }
            
            
        if (muste_fabs(lf)>ULONG_MAX || numdigits(lf)>15) // RS
          {
          if(muste_checkmp()) return(2);
          
          sur_print("\nMore precision required for current integer calculation!");
          //WAIT; 
          return(-1);
          }
        *pluku=(unsigned long)lf; // RS CHA
        return(1);
        }

/* roman.c (Martti Nikunen 6.2.1986) */

static char a[]="ivxlcdm  ";

static void roman(int n,char *x)
{
char *p, r[21]; int i,j=19;

p=a; r[20]='\0';
do
  {
  i = n%5;
  if (i < 4)
     {
     while (--i >= 0)  r[j--] = *p;
     if ( (n%10) > 4)  r[j--] = *(p+1);
     }
  else {r[j--] = ((n%10)==4) ? *(p+1) : *(p+2); r[j--] = *p;}
  p+=2;
  }
while ((n/=10) > 0);
p = r +(j+1); strcpy(x,p);
}

static char roman_chars[]="MDCLXVI";
static int roman_val[]={1000,500,100,50,10,5,1};

static int roman_to_int(char *roman,int *pk)
        {
        char x[LLENGTH];
        int k=0;
        char *p,*q;
        int i1,i2;

        strcpy(x,roman); muste_strupr(x);
        p=x;
        while (*p)
            {
            q=strchr(roman_chars,*p);
            if (q==NULL)
                {
                sprintf(sbuf,"\n%c not allowed in roman numerals!",
                            *(roman+(p-x))); sur_print(sbuf);
                WAIT; return(-1);
                }
            ++p;
            }
        p=x;
        while (*p)
            {
            q=strchr(roman_chars,*p);
            i1=roman_val[q-roman_chars];
            i2=0;
            if (*(p+1))
                {
                q=strchr(roman_chars,*(p+1));
                i2=roman_val[q-roman_chars];
                }
            if (i1>=i2) k+=i1; else k-=i1;
            ++p;
            }
        *pk=k;
        return(1);
        }

static int integer_conversion(char *word,char *par1,char *par2,char *res)
        {
        int i,j,k;
        long luku;
        int neg;
        char x[LLENGTH];

        *res=EOS;
/* Rprintf("\nword=%s par1=%s par2=%s",word,par1,par2); getch(); */

        i=1;
        if (muste_strnicmp(par1,"ASCII",5)==0)
            luku=(unsigned char)*word;
        else if (muste_strnicmp(par1,"ROMAN",5)==0)
            {
            i=roman_to_int(word,&k); if (i==-1) return(-1);
            luku=k;
//Rprintf("\nluku: %d",k);
/* RS ADD Check that ROMAN number is valid by "back transformation" */
            if (luku>3999L) { sur_print("\nMax. value for ROMAN is 3999"); return(-1); }
            roman((int)luku,sbuf);
            strcpy(x,word);
            muste_strupr(x);
            muste_strupr(sbuf);
            j=strlen(x);
            i=strncmp(x,sbuf,j);
            if (j!=strlen(sbuf) || i!=0)
              { sprintf(sbuf,"\n%s is an invalid ROMAN number!",word); 
                sur_print(sbuf); return(-1); }
            }
        else
            {
            i=base_atol(word,atoi(par1),&luku);
            }
            

        if (luku<0) { neg=1; luku=-luku; } else neg=0;
        if (muste_strnicmp(par2,"ASCII",5)==0)
            {
            if (luku>255L) { sur_print("\nMax. ASCII value is 255"); return(-1); }
            *res=(unsigned char)(int)luku; res[1]=EOS; return(1);
            }
        else if (muste_strnicmp(par2,"ROMAN",5)==0)
            {
            if (luku>3999L) { sur_print("\nMax. value for ROMAN is 3999"); return(-1); }
            roman((int)luku,res); if (*par2=='R') muste_strupr(res); return(1);
            }

        if (i<0) return(-1);
        if (i==2) // RS i=2 from base_atol above indicates multiple precision
          {
          i=atoi(par1);
          j=atoi(par2);
          if (i<2 || i>36 || j<2 || j>36)
            {
            sur_print("\nOnly bases 2,3,...,36 are permitted!"); return(-1);
            }
          muste_set_R_string(".muste$mpinstr",word);               
          sprintf(sbuf,".muste$mpoutstr<-.muste.mpchangebase(.muste$mpinstr,%d,%d)",i,j);
          muste_evalr(sbuf);
          muste_get_R_string(res,".muste$mpoutstr",LLENGTH);          
          }
        else
          {
            i=atoi(par2);
            if (i<2 || i>36)
                {
                sur_print("\nOnly bases 2,3,...,36 are permitted!");
                WAIT; return(-1);
                }
            muste_ltoa(luku,res+neg,i);
            if (neg) *res='-';
          }

        muste_strupr(res);
        return(1);
        }


static int positive_integer(char *s)
        {
        while (*s) { if (*s!=' ') break; ++s; }
        while (*s) { if (isdigit((int)*s)==0) return(0); ++s; }
        return(1);
        }

static int survo_conversion(char *word,char *par1,char *par2,char *res)
        {

        if (positive_integer(par1) && positive_integer(par2))
            return( integer_conversion(word,par1,par2,res) );
        if (muste_strcmpi(par2,"sanoin")==0)
            return( luku_sanoiksi(word,par1,res) );
        if (muste_strcmpi(par2,"sanoin2")==0)
            return( luku_sanoiksi2(word,par1,res) );
        if (muste_strcmpi(par2,"words")==0)
            return( eluku_sanoiksi(word,par1,res) );

        return( num_conversion(word,par1,par2,res) );

        return(-1);
        }


static void kirjoita(char *tulos,int j,int sar)
        {
        int i,len;
        char rivi[LLENGTH];

        edread(rivi,j);

        len=strlen(tulos);
        strncpy(rivi+sar,tulos,len);
        i=sar+len;
        while (i<LLENGTH && rivi[i]!=' ') rivi[i++]=' ';
        edwrite(rivi,j,0);
        }

static void syntax_error(char *lauseke,char *ilm)
        {
        sprintf(sbuf,"\nSyntax error in  %s: %s",lauseke,ilm); sur_print(sbuf);
        WAIT; return;
        }

static int muunto(char *lauseke,char *tulos)
        {
        char x[LLENGTH],y[LLENGTH];
        char *p1,*p2,*p;

        strcpy(x,lauseke);
        p1=strchr(x,'(');
        if (p1==NULL) { syntax_error(lauseke,"( missing!"); return(-1); }
        *p1=EOS; ++p1; p2=strchr(p1,':');
        if (p2==NULL) { syntax_error(lauseke,": missing!"); return(-1); }
        *p2=EOS; ++p2; p=strchr(p2,')');
        if (p==NULL) { syntax_error(lauseke,") missing!"); return(-1); }
        *p=EOS;
        strcpy(y,x); if (*y==EOS) { *y='1'; y[1]=EOS; }  /* empty=1 */
        return( survo_conversion(y,p1,p2,tulos) );
        }

static int muunto1(char *lauseke,char *tulos)
        {
        int i;
        char *p;
        char laus[LLENGTH];

        i=muunto(lauseke,tulos);
        p=strstr(lauseke,")(");
        if (p==NULL || i<0) return(i);
        while (1)
            {
            strcpy(laus,tulos);
            strcat(laus,p+1);
            i=muunto(laus,tulos);
            p+=2;
            p=strstr(p,")(");
            if (p==NULL || i<0) return(i);
            }
        return(0); // RS 
        }

int op_conversions()
        {
        char lauseke[LLENGTH];
        char rivi[LLENGTH];
        char tulos[LLENGTH];
        int i,k;
        int monia;
        
//        muste_fixme("FIXME: Conversions in arithmetic computing not implemented!\n");


// RS REM        if (argc==1) return;
// RS REM        s_init(argv[1]);

        monia=0; // RS
        tarkkuus=-1;
        avattu=0;

//        s_init("A"); // RS CHA
        i=spec_init(r1+r-1); if (i<0) return(-1);  /* siirretty 13.1.92 */

        edread(rivi,r1+r-1);
        strcpy(lauseke,rivi);
        i=c1+c-2;
        lauseke[i]=EOS;
        if (lauseke[i-1]=='.') { lauseke[--i]=EOS; monia=1; }
        while (lauseke[i]!=' '&& i>0) --i;
/*  Rprintf("\nlauseke=%s",lauseke+i+1);   getch();  */
        i=muunto1(lauseke+i+1,tulos);
        
        if (i<0) { WAIT; return(-1); }
        kirjoita(tulos,r1+r-1,c1+c-1);
        edisp=2;
        if (monia)
            {
            for (k=0; k<spn; ++k)
                {
                if (spp[k]!='.') continue;
                strcpy(lauseke,spa[k]);
                lauseke[strlen(lauseke)-1]=EOS;
                i=muunto1(lauseke,tulos);
                if (i<0) { WAIT; return(-1); }
                kirjoita(tulos,(int)(spplace[k]/ed1+1),spplace[k]%ed1+1);
                }
            edisp=1;
            }
// RS REM        s_end(argv[1]);
//        s_end("A"); // RS CHA
        return(1);
        }

