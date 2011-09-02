/* date.c 13.4.1996/KV (21.2.2009)
   converted for Muste 1.5.2011/KV (8.5.2011) (27.6.2011) (1.9.2011)
 */

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <time.h>
#include <string.h>
#include <locale.h> // 8.5.2011
#include "survo.h"
#include "survoext.h"
#include "survolib.h"

extern void muste_kv_s_err(char *, ...);
extern void muste_kv_s_disp(char *, ...);
extern void muste_kv_usage_info(void);

static SURVO_DATA dat;
static struct tm *D;
static time_t tnow;
static char date_str[LNAME];
static char command_line[LNAME];
static int suomi=0; /* 11.12.2000 */

/*
        declarations for Julian date routines
*/

#define JUL_ROME 2299161L
#define JUL_ENGLAND 2361222L
#define JUL_FINLAND 2361390L /* kv 13.4.96 */

static int jul_transition;

static int juldnj (struct tm *bdt, int Transition);
static int juldn (struct tm *bdt);
static int juldnd (struct tm *bdt, struct tm *Transition_date);

static struct tm *julcdj (int JD, int Transition);
static struct tm *julcd (int JD);
static struct tm *julcdd (int JD, struct tm *Transition_date);

/* SOURCE OF THE JULIAN DAY ALGORITHM:
"Translated from Pascal to C by Jim Van Zandt, July 1992.

        Error-free translation based on error-free PL/I source

        Based on Pascal code copyright 1985 by Michael A. Covington,
        published in P.C. Tech Journal, December 1985, based on formulae
        appearing in Astronomical Formulae for Calculators by Jean Meeus"

Julian (sense 1) date routines, handling both Julian (sense 2) and
Gregorian calendars

SYNOPSIS
        int juldn (struct tm *bdt)
        int juldnj (struct tm *bdt, int Transition)
        int juldnd (struct tm *bdt, struct tm *Transition_date)

        struct tm *julcd (int J)
        struct tm *julcdj (int J, int Transition)
        struct tm *julcdd (int J, struct tm *Transition_date)

        extern int jul_transition;

DESCRIPTION

juldn* returns the Julian day number (days since Jan 1, 4713 B.C.)
for the date specified by the struct tm (a broken down time)
pointed to by 'bdt'.  Only the tm_year, tm_mon, and tm_mday fields
are examined.  If the month or day fields are out of their normal
range, they are adjusted.  The tm_wday and tm_yday fields are also
set.

julcd* returns a pointer to a struct tm filled in with the date
corresponding to the Julian day number 'J'.  Five fields are set:
tm_year, tm_mon, tm_mday, tm_wday, and tm_yday.  The pointer is to
a static structure which is reused on each call.

For both juldn and julcd, the Gregorian calendar is assumed
provided the Julian day number falls on or after the value in the
global variable 'jul_transition', otherwise the Julian calendar is
used.  'jul_transition' is initialized to 2361222 (or September 14,
1752), as in the United Kingdom and the colonies.  A different
transition can be specified by Julian day number (for juldnj or
julcdj) or Gregorian date (for juldnd or julcdd).  Alternatively,
'jul_transition' can be reset.  If the transition is specified by
date, ensure it is interpreted as a Gregorian date by using julcdj
with a small number:

        jul_transition = julcdj(&my_transition_date, 1L);

Algorithm is valid from 4713 B.C. to 19,999 A.D.  For caveats,
see below.

Aside: In a struct tm, the tm_year field is "year-1900".  For the
year 1 A.D., that would be -1899.  There was no year 0 (the number
zero had not been invented!), so the previous year was 1 B.C.,
which is represented by -1900.

HISTORY

$Id: julcal.c%v 1.10 1992/12/13 03:15:59 jrv Exp jrv $

$Log: julcal.c%v $

 Revision 1.10  1992/12/13  03:15:59  jrv
 default transition date is that for the United Kingdom

 Revision 1.9  1992/12/13  03:07:07  jrv
 juldnj gives correct value even if month is outside normal range.
 juldnj normalizes mday, month, and year.
 _juldnj introduced to let julcdj set yday without causing infinite recursion.

 Revision 1.8  1992/12/08  00:23:38  jrv
 Test program moved to a separate file.

 Revision 1.7  1992/12/06  01:57:20  jrv
 julcd* return pointers to a static struct tm, like localtime and gmtime.

 Revision 1.6  1992/12/05  23:14:58  jrv
 The default transition date is a global variable, initialized to 15 Oct 1582.
 Some variable names changed.  All variables are lower case.

 Revision 1.5  1992/12/05  22:20:11  jrv
 juldnd accepts a struct tm (a "broken-down time") instead of a
 unique struct.  julcdd produces a struct tm (including tm_wday
 and tm_yday fields) instead of a unique struct.

 Revision 1.4  1992/12/05  20:04:51  jrv
 Test program prints input values, then computed values.
 Test program is silent by default.

 Revision 1.3  1992/12/04  02:57:54  jrv
 Reformatted to more standard C.
 Eliminated some redundant typedefs.
 Type Year is now an int rather than a long.
 Some variables converted to lower case.

 Revision 1.2  1992/12/04  02:15:58  jrv
 A Year is no longer unsigned, so years BC work.
 Regression test driver added.

 Revision 1.1  1992/12/04  00:21:37  jrv
 Initial revision

Translated from Pascal to C by Jim Van Zandt, July 1992.

Error-free translation based on error-free PL/I source

Based on Pascal code copyright 1985 by Michael A. Covington,
published in P.C. Tech Journal, December 1985, based on formulae
appearing in Astronomical Formulae for Calculators by Jean Meeus

Reconversion to normal Julian epoch, integer arithmetic and
4000-year correction by John W. Kennedy

[The 4000-year adjustment is controversial.  It is not mentioned in
the paper "Calendrical Calculations" by Nachum Dershowitz and
Edward M.  Reingold in Software Practice and Experience, v 20 n 9
pp 899-928 (Sep 1990).  I have left it in mainly because it will
make no difference for a very long time.  - jvz]

Historical exceptions _not_ allowed for in this package:

Until Julius Caesar established the Julian calendar in 45 B.C.,
calendars were irregular.  This package assumes the Julian calendar
back to 4713 B.C.

The Julian calendar was altered in 8 B.C.  From 45 B.C. to 8 B.C.,
the months were
Jan=31, Feb=29(30), Mar=31, Apr=30, May=31, Jun=30,
Jul=31, Aug=30,     Sep=31, Oct=30, Nov=31, Dec=30
This package assumes the month lengths as we know them.

Leap years from 45 B.C.  to 8 A.D.  were miscalculated: (45, 42,
39, 36, 33, 30, 27, 24, 21, 18, 15, 12, 9, then none at all until 8
A.D.) This package assumes leap years every four years, as they
were meant to have been.

January 1 was not always the first day of the year.  The United
Kingdom, in particular, started the year on March 25 until 1752.
(However, the year ended on December 31, leaving the days between
in limbo.) This package assumes January 1 is the first day of the
year.

Leap-year day was originally done by having February 24 (25 from 45
to 8 B.C.) twice.  This package assumes Leap-year day is February
29.

"Transition" argument is the first Julian date to be considered as
belonging to the Gregorian calendar.  Usual values are:

2299161 = October 5/15, 1582,       as in Rome, or
2361222 = September 3/14, 1752,     as in the United Kingdom
                                    and the Colonies

For more on the history of calendars, including transition dates in
other countries, see Bob Douglas' article in dates.txt.

*/

typedef int Julian;
typedef int Year;
typedef int Month;
typedef int Day;
typedef int Work;

static Julian jul_transition=JUL_FINLAND; /* kv 13.4.96 */

static Julian
_juldnj(bdt, Transition) struct tm *bdt; Julian Transition;
{
        Year ay, y, dy;
        Month m;
        Julian jd_julian, jd_gregorian, jd;

        y = bdt->tm_year + 1900;
        m = bdt->tm_mon;

        dy = m/12;
        y += dy;
        m -= dy*12;
                        /* assert( -11 <= m && m <= 11 ) */
        if(m < 0)
                {
                m += 12;
                y--;
                }
                        /* assert( 0 <= m && m <= 11 ) */

        if ( m < 2 )
                {
                m += 1 + 12;
                y--;
                }
        else
                m++;

        ay = y + 4716;
        jd_julian = ((1461*(Work)ay) >> 2) + (153*(m + 1)/5)
                   + (Work)bdt->tm_mday - 1524;
        jd_gregorian = jd_julian + 2 - y/100 + y/400 - y/4000;
        if ( jd_gregorian >= Transition ) jd = jd_gregorian;
        else jd = jd_julian;
        return jd;
}

 /* this wrapper can call julcdj without causing infinite recursion */
static Julian
juldnj(bdt, Transition) struct tm *bdt; Julian Transition;
{       Julian jd;
        struct tm *normal;

        jd = _juldnj(bdt, Transition);

        normal = julcdj(jd, Transition);
        bdt->tm_year = normal->tm_year;
        bdt->tm_mon = normal->tm_mon;
        bdt->tm_mday = normal->tm_mday;
        bdt->tm_wday = normal->tm_wday;
        bdt->tm_yday = normal->tm_yday;

        return jd;
}

static Julian
juldn (bdt) struct tm *bdt;
{       return juldnj (bdt, jul_transition);
}

static Julian
juldnd (bdt, Transition_date) struct tm *bdt; struct tm *Transition_date;
{       return juldnj (bdt, _juldnj (Transition_date, 1L));
}

static struct tm *
julcdj (jd, Transition) Julian jd; Julian Transition;
{
        Julian aa, ab, a;
        Work b, d, ee;
        Year ay;
        Month em, m;
        Year y;
        struct tm newyears;
        static struct tm date;

        memset(&date, 0, sizeof(date));

        if ( jd < Transition ) /* Julian Calendar */
                a = (Work)(jd);
        else /* Gregorian Calendar */
                {
                aa = jd - 1721120L;
                ab = 31*(aa/1460969L); aa = aa % 1460969L;
                ab = ab + 3*(aa/146097L); aa = aa % 146097L;
                if ( aa == 146096L ) ab = ab + 3;
                else ab = ab + aa/36524L;
                a = jd + (ab - 2);
                }
        b = a + 1524;
        ay = (Year)((20*b - 2442)/7305);
        d = (1461*(Work)(ay)) >> 2;
        ee = b - d;
        em = (Month)(10000L*ee/306001L);
        date.tm_mday = (Day)(ee - 306001L*em/10000L);

        m = em - 1;
        if(m > 12) m -= 12;
        date.tm_mon = m - 1;

        if ( m > 2 ) y = ay - 4716;
        else y = ay - 4715;
        date.tm_year = y - 1900;

        date.tm_wday = (jd+1)%7;

        newyears = date;
        newyears.tm_mon = 0;
        newyears.tm_mday = 1;
        date.tm_yday = jd - _juldnj(&newyears, Transition);
        return &date;
}

static struct tm *
julcd(jd) Julian jd;
{       return julcdj (jd, jul_transition);
}

static struct tm *
julcdd(jd, Transition_date) Julian jd; struct tm *Transition_date;
{       return julcdj (jd, _juldnj (Transition_date, 1L));
}

/*
        end of           Julian date routines
*/

static int check_parameters(void);
static int check_date(void);
static void no_mem(void);

static int check_parameters(void)
{
    int i;

    if (g>1 && word[1][0]=='?') {
        if (suomi) {
            muste_kv_s_err("Ks. PVM?");
        } else {
            muste_kv_usage_info();
        }
        return -1;
    }

    i=check_date(); if (i>0) return 1; /* editorial mode */
    if (suomi) return -1; /* ei data-moodia suomeksi! */
    return 2; /* data mode */
}

static int check_date(void)
{
    int i;
    char ch;

    if (g<2) { /* no parameter given */
        if (suomi) strcpy(command_line, "PVM");
              else strcpy(command_line, "DATE");
        time(&tnow); D=localtime(&tnow);
        sprintf(date_str, "%d.%d.%d", D->tm_mday, D->tm_mon+1, D->tm_year);
        if (suomi) strcpy(sbuf, "PVM ");
              else strcpy(sbuf, "DATE ");
        strcat(sbuf,date_str); split(sbuf,word,2);
        return 1; /* editorial mode */
    }
    for (i=0; i<strlen(word[1]); i++) {
        ch=word[1][i];
        if (ch=='.' || ch=='+' || ch=='-') continue;
        if (!isascii(ch)) return -1;
        if (ispunct(ch)) return -1;
        if (isalpha(ch)) return -1;
    }
    if (suomi) sprintf(command_line, "PVM %s", word[1]);
          else sprintf(command_line, "DATE %s", word[1]);
    return 1; /* editorial mode */
}

static void no_mem(void)
{
    muste_kv_s_err("Not enough memory! (%s)", word[0]);
}

/* viikkonumeroalgoritmi verkosta: (vihdoin; toteutettu 2.1.98) */
/*
Seuraavassa C-ohjelmanpätkässä oleva weekno()-funktio laskee annetulle
päivälle (d = päivä 1 - 31, m = kuukausi 1 .. 12 ja y = vuosi)
viikkonumeron. Se on Tapani Tarvaisen (tt@math.jyu.fi) käsialaa.
Lähde (1.5.2011): http://groups.google.com/group/sfnet.atk.ohjelmointi (FAQ)
*/

static int julian(int, int, int);
static int weekno(int, int, int);

static int julian(int d, int m, int y)
{
  int n1, n2;
  n1 = 12*y+m-3;
  n2 = n1/12;
  return (734*n1+15)/24-2*n2+n2/4-n2/100+n2/400+d+1721119;
}

static int weekno(int d, int m, int y)
{
  int n1, n2, w;
  n1 = julian(d, m, y);
  n2 = 7*(n1/7)+10;
  y = y+1;
  while ((w = (n2-julian(1,1,y))/7) <= 0) {
    y = y-1;
  }
  return w;
}

static int Leap_Year(int);
static int Leap_Year(int year)
{
    if (year<200) year+=1900; // 12.2.2001
    if ((year%4==0 && year%100!=0) || year%400==0) return 1;
    return 0;
}

static void show_dates(void);
static void data_dates(void);
static int recheck_date(int);
static int aDay,aMonth,aYear;

static char *viikko[]={ // isot kirjaimet: SM (päiväyksen alku!), helppo muuttaa.
"Maanantai", "Tiistai", "Keskiviikko", "Torstai",
"Perjantai", "Lauantai", "Sunnuntai"
};
static char *week[]={
"Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"
};

static char *vuosi[]={ // 22.6.2011: ä(ascii:8)=204
"tammikuu", "helmikuu", "maaliskuu", "huhtikuu",
"toukokuu", "kes\204kuu", "hein\204kuu", "elokuu",
"syyskuu", "lokakuu", "marraskuu", "joulukuu"
};
static char *year[]={
"Jan", "Feb", "Mar", "Apr", "May", "Jun",
"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};

#include "limits.h" /* for INT_MAX */
#include "math.h"

static void show_dates(void)
{
    int i;
    int date1, date2, diff;

    i=recheck_date(1);
    if (i<0) {
        if (suomi) {
            muste_kv_s_err("Ep\204kelpo p\204iv\204m\204\204r\204!");
        } else {
            muste_kv_s_disp("\nDoes not look like a date...trying to open data...");
            data_dates();
        }
        return;
    }
    if (i==0) {
        if (suomi) {
            muste_kv_s_err("Ep\204kelpo p\204iv\204m\204\204r\204!");
        } else {
            muste_kv_s_err("Invalid date!");
        }
        return;
    }
    if (g>3 && !strcmp(word[2],"-")) { /* Difference: DATE 26.9 - 1.6 */
        date1=juldn(D);
        i=recheck_date(3);
        if (i<=0) {
            if (suomi) {
                muste_kv_s_err("Ep\204kelvot p\204iv\204m\204\204r\204t!");
            } else {
                muste_kv_s_err("Invalid dates!");
            }
            return;
        }
        date2=juldn(D);
        diff=date1-date2;
        if (suomi) {
            sprintf(command_line, "PVM %s - %s", word[1],word[3]);
            sprintf(date_str,"Erotus=%d",diff);
        } else {
            sprintf(command_line, "DATE %s - %s", word[1],word[3]);
            sprintf(date_str,"Difference=%d",diff);
        }
    } else if (g==3 && !muste_strcmpi(word[2],"Julian")) { /* 13.2.1998 */
        if (suomi) {
            muste_kv_s_err("Use DATE %s,Julian", word[1]);
        } else {
            strcat(command_line, ",Julian");
            strftime(date_str, LNAME, "%a %b %d %Y Julian_day=", D);
            sprintf(sbuf, "%d", juldn(D));
            strcat(date_str, sbuf);
        }
    } else {
        if (suomi) {
            char tmp[LNAME];
// PVM / Keskiviikko, 6. marraskuuta 2000 klo 12.39 (vko 49, pvä 332/365)
            strcpy(date_str, "");
            strftime(tmp, LNAME, "%a", D);
            i=0;
            while (strcmp(tmp, week[i])) i++;
            strcat(date_str, viikko[i]);
            strftime(tmp, LNAME, "%d", D);
            i=atoi(tmp);
            sprintf(sbuf, ", %d. ", i);
            strcat(date_str, sbuf);
            strftime(tmp, LNAME, "%b", D);
            i=0;
            while (strcmp(tmp, year[i])) i++;
            strcat(date_str, vuosi[i]);
            strcat(date_str, "ta ");
            strftime(tmp, LNAME, "%Y", D);
            strcat(date_str, tmp);
            strcat(date_str, " klo ");
            strftime(tmp, LNAME, "%H.%M", D);
            strcat(date_str, tmp);
            strcat(date_str, " (vko ");
            i=weekno(D->tm_mday, D->tm_mon+1, D->tm_year+1900); /* 2.1.1998 */
            sprintf(sbuf, "%d, pv\204 ", i); // 8.5.2011: ä(ascii:8)=204
            strcat(date_str, sbuf);
            strftime(tmp, LNAME, "%j", D);
            i=atoi(tmp);
            sprintf(sbuf, "%d", i);
            strcat(date_str, sbuf);
            if (Leap_Year(D->tm_year)) strcat(date_str, "/366)");
            else strcat(date_str, "/365)");
        } else {
            strftime(date_str, LNAME, "%A %B %d %Y %H:%M:%S Week=", D);
            i=weekno(D->tm_mday, D->tm_mon+1, D->tm_year+1900); /* 2.1.1998 */
            sprintf(sbuf, "%d", i);
            if (i<10) strcat(date_str,"0");
            strcat(date_str, sbuf);
            strftime(sbuf, LNAME, " Day=%j", D);
            strcat(date_str, sbuf);
        }
    }
    sprintf(sbuf, "%s / %s", command_line, date_str);
    edwrite(space,r1+r-1,1);
    edwrite(sbuf,r1+r-1,1);
    return;
}

static int recheck_date(int index)
{
    int i, year_given;
    char first, *rel, *token;
    int rel_time;
    int addy;

    time(&tnow); D=localtime(&tnow);
    sprintf(date_str, "%d.%d.%d", D->tm_mday, D->tm_mon+1, D->tm_year);
    rel_time=0;
    first=word[index][0];
    rel=strchr(word[index],'+');
    if (rel!=NULL) {
        rel_time=atol(rel);
    } else {
        rel=strchr(word[index],'-');
        if (rel!=NULL) {
            rel_time=atol(rel);
        }
    }
    if (first!='+' && first!='-') {
        token=strtok(word[index],"+-");
        strcpy(date_str,token);
    }
    year_given=0;
    strcpy(sbuf,date_str);
    token=strchr(sbuf,'.');
    if (token==NULL) {
        aDay=atoi(date_str); /* only day given */
        aMonth=D->tm_mon+1; /* this month */
        aYear=D->tm_year; /* this year */ /* actually years since 1900 */
    } else { /* at least one '.' found */
        token=strtok(sbuf,".");
        aDay=atoi(token);
        token=strtok(NULL,".");
        if (token!=NULL) {
            aMonth=atoi(token);
            token=strtok(NULL,".");
            if (token!=NULL) {
                aYear=atoi(token);
                year_given=1;
            } else {
                aYear=D->tm_year; /* this year */
            }
        } else {
            aMonth=D->tm_mon+1; /* this month */
            aYear=D->tm_year; /* this year */
        }
    }

    if (aDay<1 || aDay>31) return -1;
    if (aMonth<1 || aMonth>12) return (year_given) ? 0 : -1;
    if ((aMonth==4||aMonth==6||aMonth==9||aMonth==11)&&aDay>30)
        return (year_given) ? 0 : -1;
    if (D->tm_year>99) { /* Y2K, 2.1.1998 */
        if (aYear<100) aYear+=100;
    }
    if (aYear>199) aYear-=1900; /* tm_year:n suhdeluku */

  /* HUOM! Vuodet 00-99 tarkoittavat kuluvaa vuosisataa! */
  /* (myös sitten kun vuosituhat on vaihtunut!!) */
  /* data-rutiineissa lisätty 6.12.2000 täsmennys CENTURY, jolla */
  /* tätä voi muuttaa, mutta se ei koske toimituskenttäjuttuja.  */

    if (aMonth==2) {
        if (aDay>29) return (year_given) ? 0 : -1;
        if (!Leap_Year(aYear) && aDay==29) return (year_given) ? 0 : -1;
    }
    D->tm_mon=aMonth-1;
    D->tm_year=aYear;
    addy=rel_time/INT_MAX;
    if (addy>=1L || addy<=-1L) {
        D->tm_mday=aDay;
        juldn(D);
        for (i=0; i<(int)abs(addy); i++) {
            if (rel_time>0L) D->tm_mday=+INT_MAX;
            if (rel_time<0L) D->tm_mday=-INT_MAX;
            juldn(D);
        }
        addy=rel_time%INT_MAX;
        D->tm_mday=addy;
        juldn(D);
    } else {
        D->tm_mday=aDay+rel_time;
        juldn(D);
    }
    return 1;
}


static int check_specifications(void);
static int check_activations(void);
static int check_vartype(int, char, char, int, int, char *);
static void update_data(void);
static int read_value(int, int);
static void write_values(int);
static int check_format(char *);
static int write_format(int, char [], char *, char *);

static int Dvar,Mvar,Yvar,Datevar,ivar;                  /*  input vars' # */
static int Dvars;                              /* # of Datevar's (D masks) */
static int newdate,weekday,dayofyear,weekofyear;         /* output vars' # */
static int julday; /* output var # (added 12.2.1998) */
static int Jday; /* 12.2.1998 */
static int day0; /* 13.2.1998 */
static int prind; /* global 15.11.1999 */

#if 0
#define DMAX 11 /* max: DD.MMM.YYYY */
#endif
#define DMAX 20 /* max: DD.MMM.YYYY HH:MM:SS */

static char date_format[DMAX+1],indate_format[DMAX+1],outdate_format[DMAX+1];
static char idel1[2],idel2[2];         /* delimiters of input  date format */
static char odel1[2],odel2[2];         /* delimiters of output date format */
static int idellen,odellen;            /* len(*del1)+len(*del2) */
static int DLen, iDLen, oDLen;         /* lengths of date_formats */
static int Did, iDid, oDid;            /* id numbers of date_formats */
static char Dstr[DMAX+1];
                                       /* date rules (differences) */
static char *rulestr="DATE_RULES";
static char rule[LLENGTH];
static int *d1,*d2,*dd;
static int rules,ruleline;
static int century; /* 6.12.2000 */
static int shift; /* 18.1.2001 */      // Obs. CENTURY always overrides SHIFT !
static int itime; /* 7.4.2002 */

/*
IDATE=<format> IDEL1=<ch> IDEL2=<ch> IDEL=<ch>
ODATE=<format> ODEL1=<ch> ODEL2=<ch> ODEL=<ch>
*/

static int check_format(char *format)
{
/* old formats for DATE specification only! (may be added if desired) */
/* valid also for IDATE and ODATE as shortcuts! */
    if (!strcmp(format, "DD.MM.YY"))    return 1;
    if (!strcmp(format, "DD/MM/YY"))    return 2;
    if (!strcmp(format, "MM/DD/YY"))    return 3;
    if (!strcmp(format, "DD.MM.YYYY"))  return 4;
    if (!strcmp(format, "DD/MM/YYYY"))  return 5;
    if (!strcmp(format, "MM/DD/YYYY"))  return 6;
    if (!strcmp(format, "DD-MMM-YYYY")) return 7;

/* values 8-19 reserved for extra shortcuts like 7; 20 not in use */

/* whole dates: (for IDATE and ODATE) */
    if (!strcmp(format, "DDMMYY"))      return 21;
    if (!strcmp(format, "DDYYMM"))      return 22;
    if (!strcmp(format, "MMDDYY"))      return 23;
    if (!strcmp(format, "MMYYDD"))      return 24;
    if (!strcmp(format, "YYDDMM"))      return 25;
    if (!strcmp(format, "YYMMDD"))      return 26;
    if (!strcmp(format, "DDMMMYY"))     return 27;
    if (!strcmp(format, "DDYYMMM"))     return 28;
    if (!strcmp(format, "MMMDDYY"))     return 29;
    if (!strcmp(format, "MMMYYDD"))     return 30;
    if (!strcmp(format, "YYDDMMM"))     return 31;
    if (!strcmp(format, "YYMMMDD"))     return 32;
    if (!strcmp(format, "DDMMYYYY"))    return 33;
    if (!strcmp(format, "DDYYYYMM"))    return 34;
    if (!strcmp(format, "MMDDYYYY"))    return 35;
    if (!strcmp(format, "MMYYYYDD"))    return 36;
    if (!strcmp(format, "YYYYDDMM"))    return 37;
    if (!strcmp(format, "YYYYMMDD"))    return 38;
    if (!strcmp(format, "DDMMMYYYY"))   return 39;
    if (!strcmp(format, "DDYYYYMMM"))   return 40;
    if (!strcmp(format, "MMMDDYYYY"))   return 41;
    if (!strcmp(format, "MMMYYYYDD"))   return 42;
    if (!strcmp(format, "YYYYDDMMM"))   return 43;
    if (!strcmp(format, "YYYYMMMDD"))   return 44;

/* values 45-59 reserved; 60 not in use */

/* partial dates: (for ODATE only!) */
    if (!strcmp(format, "DD"))          return 61;
    if (!strcmp(format, "MM"))          return 62;
    if (!strcmp(format, "YY"))          return 63;
    if (!strcmp(format, "MMM"))         return 64;
    if (!strcmp(format, "YYYY"))        return 65;
    if (!strcmp(format, "DDMM"))        return 66;
    if (!strcmp(format, "DDYY"))        return 67;
    if (!strcmp(format, "MMDD"))        return 68;
    if (!strcmp(format, "MMYY"))        return 69;
    if (!strcmp(format, "YYDD"))        return 70;
    if (!strcmp(format, "YYMM"))        return 71;
    if (!strcmp(format, "DDMMM"))       return 72;
    if (!strcmp(format, "MMMDD"))       return 73;
    if (!strcmp(format, "MMMYY"))       return 74;
    if (!strcmp(format, "YYMMM"))       return 75;
    if (!strcmp(format, "DDYYYY"))      return 76;
    if (!strcmp(format, "MMYYYY"))      return 77;
    if (!strcmp(format, "YYYYDD"))      return 78;
    if (!strcmp(format, "YYYYMM"))      return 79;
    if (!strcmp(format, "MMMYYYY"))     return 80;
    if (!strcmp(format, "YYYYMMM"))     return 81;

    return -1;
}

static int write_format(int i, char *format, char *del1, char *del2)
{
    switch (i) {
        case  1:  strcpy(format, "%d.%m.%y");                 break;
        case  2:  strcpy(format, "%d/%m/%y");                 break;
        case  3:  strcpy(format, "%m/%d/%y");                 break;
        case  4:  strcpy(format, "%d.%m.%Y");                 break;
        case  5:  strcpy(format, "%d/%m/%Y");                 break;
        case  6:  strcpy(format, "%m/%d/%Y");                 break;
        case  7:  strcpy(format, "%d-%b-%Y");                 break;

        case 21: sprintf(format, "%%d%s%%m%s%%y", del1,del2); break;
        case 22: sprintf(format, "%%d%s%%y%s%%m", del1,del2); break;
        case 23: sprintf(format, "%%m%s%%d%s%%y", del1,del2); break;
        case 24: sprintf(format, "%%m%s%%y%s%%d", del1,del2); break;
        case 25: sprintf(format, "%%y%s%%d%s%%m", del1,del2); break;
        case 26: sprintf(format, "%%y%s%%m%s%%d", del1,del2); break;
        case 27: sprintf(format, "%%d%s%%b%s%%y", del1,del2); break;
        case 28: sprintf(format, "%%d%s%%y%s%%b", del1,del2); break;
        case 29: sprintf(format, "%%b%s%%d%s%%y", del1,del2); break;
        case 30: sprintf(format, "%%b%s%%y%s%%d", del1,del2); break;
        case 31: sprintf(format, "%%y%s%%d%s%%b", del1,del2); break;
        case 32: sprintf(format, "%%y%s%%b%s%%d", del1,del2); break;
        case 33: sprintf(format, "%%d%s%%m%s%%Y", del1,del2); break;
        case 34: sprintf(format, "%%d%s%%Y%s%%m", del1,del2); break;
        case 35: sprintf(format, "%%m%s%%d%s%%Y", del1,del2); break;
        case 36: sprintf(format, "%%m%s%%Y%s%%d", del1,del2); break;
        case 37: sprintf(format, "%%Y%s%%d%s%%m", del1,del2); break;
        case 38: sprintf(format, "%%Y%s%%m%s%%d", del1,del2); break;
        case 39: sprintf(format, "%%d%s%%b%s%%Y", del1,del2); break;
        case 40: sprintf(format, "%%d%s%%Y%s%%b", del1,del2); break;
        case 41: sprintf(format, "%%b%s%%d%s%%Y", del1,del2); break;
        case 42: sprintf(format, "%%b%s%%Y%s%%d", del1,del2); break;
        case 43: sprintf(format, "%%Y%s%%d%s%%b", del1,del2); break;
        case 44: sprintf(format, "%%Y%s%%b%s%%d", del1,del2); break;

        case 61: sprintf(format, "%%d");                      break;
        case 62: sprintf(format, "%%m");                      break;
        case 63: sprintf(format, "%%y");                      break;
        case 64: sprintf(format, "%%b");                      break;
        case 65: sprintf(format, "%%Y");                      break;

        case 66: sprintf(format, "%%d%s%%m", del1);           break;
        case 67: sprintf(format, "%%d%s%%y", del1);           break;
        case 68: sprintf(format, "%%m%s%%d", del1);           break;
        case 69: sprintf(format, "%%m%s%%y", del1);           break;
        case 70: sprintf(format, "%%y%s%%d", del1);           break;
        case 71: sprintf(format, "%%y%s%%m", del1);           break;
        case 72: sprintf(format, "%%d%s%%b", del1);           break;
        case 73: sprintf(format, "%%b%s%%d", del1);           break;
        case 74: sprintf(format, "%%b%s%%y", del1);           break;
        case 75: sprintf(format, "%%y%s%%b", del1);           break;
        case 76: sprintf(format, "%%d%s%%Y", del1);           break;
        case 77: sprintf(format, "%%m%s%%Y", del1);           break;
        case 78: sprintf(format, "%%Y%s%%d", del1);           break;
        case 79: sprintf(format, "%%Y%s%%m", del1);           break;
        case 80: sprintf(format, "%%b%s%%Y", del1);           break;
        case 81: sprintf(format, "%%Y%s%%b", del1);           break;

        default: break;
    }
    return 1;
}

static int check_specifications(void)
{
    int j;
    char delch1,delch2;

 /* DATE */    /* for compatibility (DMY) only, ODATE recommended */
    strcpy(date_format, "DD.MM.YY"); /* default for old-style output */
    j=spfind("DATE");
    if (j>=0) strcpy(date_format, spb[j]);
    Did=check_format(date_format);
    if (Did<0) {
        muste_kv_s_err("Date format %s is unknown!", date_format);
        return -1;
    }
    DLen=strlen(date_format);
    write_format(Did, date_format, "", ""); /* delimiters fixed */

 /* IDATE */
    iDLen=0; iDid=0; idellen=0;
    j=spfind("IDATE");
    if (j>=0) {
        strcpy(indate_format, spb[j]);
        iDid=check_format(indate_format);
        if (iDid<0) {
            muste_kv_s_err("Date format %s is unknown! (IDATE)", indate_format);
            return -1;
        }
        if (iDid>60) {
            muste_kv_s_err("Partial date formats (like %s) are for ODATE only!",
                        indate_format);
            return -1;
        }
     /* IDEL */
        delch1=EOS; delch2=EOS;
        j=spfind("IDEL");
        if (j>=0) {
            delch1=*spb[j];
            delch2=delch1;
        }
        j=spfind("IDEL1"); if (j>=0) delch1=*spb[j];
        j=spfind("IDEL2"); if (j>=0) delch2=*spb[j];
        sprintf(idel1,"%c",delch1); sprintf(idel2,"%c",delch2);
        idellen=strlen(idel1)+strlen(idel2); /* 0,1,2 */
        iDLen=strlen(indate_format); if (iDid>20) iDLen+=idellen;
        write_format(iDid, indate_format, idel1, idel2);
    }

 /* ODATE */
    oDLen=0; oDid=0; odellen=0;
    j=spfind("ODATE");
    if (j>=0) {
        strcpy(outdate_format, spb[j]);
        oDid=check_format(outdate_format);
        if (oDid<0) {
            muste_kv_s_err("Date format %s is unknown! (ODATE)", outdate_format);
            return -1;
        }
     /* ODEL */
        delch1=EOS; delch2=EOS;
        j=spfind("ODEL");
        if (j>=0) {
            delch1=*spb[j];
            delch2=delch1;
        }
        j=spfind("ODEL1"); if (j>=0) delch1=*spb[j];
        j=spfind("ODEL2"); if (j>=0) delch2=*spb[j];
        sprintf(odel1,"%c",delch1); sprintf(odel2,"%c",delch2);
        odellen=strlen(odel1)+strlen(odel2); /* 0,1,2 */
        oDLen=strlen(outdate_format); if (oDid>20) oDLen+=odellen;
        write_format(oDid, outdate_format, odel1, odel2);
    }

 /* PRIND & prind */
    prind=1;
    j=spfind("PRIND");
    if (j>=0) prind=atoi(spb[j]);
    else if (hae_apu("prind",sbuf)) prind=atoi(sbuf);

 /* SHIFT */ /* 18.1.2001 continuation to century (also sugg, by Reijo) */
    shift=0; // must be checked first, since CENTURY always overrides SHIFT
    j=spfind("SHIFT");
    if (j>=0) shift=atoi(spb[j]);
    if (shift<0 || shift>99) {
        muste_kv_s_err("Allowed values for SHIFT are 0-99!");
        return -1;
    }

 /* CENTURY */ /* 6.12.2000 Reijo Sund's suggestion */
    century=2000; /* current century is the default (modify this in 2100...) */
    j=spfind("CENTURY");
    if (j>=0) {
        century=atoi(spb[j]);
        shift=0; // CENTURY always overrides SHIFT
        switch (century) {
            case 1900: break;
            case 2000: break;
              default: muste_kv_s_err("Allowed values for CENTURY are 1900 and 2000!");
                       return -1;
        }
    }

 /* ITIME */ /* alustava koeversio omiin tarpeisiini 7.4.2002 */
    itime=0;
    j=spfind("ITIME");
    if (j>=0) {
        itime=1; /* alustava versio, vain HH:MM:SS -muoto */
    }

    return 1;
}

static int check_activations(void)
{
    int i;
    int len1,len2;

    i=mask(&dat); if(i<0) return -1;
 // i=conditions(&dat); if(i<0) return -1; // moved 11.2.2005 (!) to cover diff's

/* input variables: */
    Dvar=activated(&dat,'D'); /* Datevar checked later */
    Mvar=activated(&dat,'M');
    Yvar=activated(&dat,'Y');
    ivar=activated(&dat,'i'); /* Julian day to a date (suggestion by R.Sund) */

/* output variables (which must exist): */
    newdate=activated(&dat,'d');
    weekday=activated(&dat,'a');
    dayofyear=activated(&dat,'j');
    weekofyear=activated(&dat,'w');
    julday=activated(&dat,'J'); /* 12.2.1998 */

    day0=0L;
    if ((julday>=0) || (ivar>=0)) {  /* 21.2.2009 (!) */
        i=spfind("JULIAN_DAY0"); /* 13.2.1998 */
        if (i>=0) day0=atol(spb[i]); else day0=0L;
        if (day0<1794108L || day0>5373482L) day0=0L;
      /* Supported dates between these: */
      /* DATE 1.1.200,Julian / Tue Jan 01 200 Julian_day=1794108 */
      /* DATE 31.12.9999,Julian / Wed Dec 31 9999 Julian_day=5373482 */
    }
    len1=DLen; len2=DLen;
    if (iDLen) len1=iDLen;
    if (oDLen) len2=oDLen;
    i=check_vartype(newdate,'d','S',len1,len2,"date");
    if (i<0) return -1;
    i=check_vartype(weekday,'a','S',3,3,"weekday");
    if (i<0) return -1;
    i=check_vartype(dayofyear,'j','N',2,3,"day of year");
    if (i<0) return -1;
    i=check_vartype(weekofyear,'w','N',1,2,"week of year");
    if (i<0) return -1;
    if (!day0) { /* 13.2.1998 if day0 given, 'J' can be of any type */
        i=check_vartype(julday,'J','N',4,7,"Julian day"); /* 12.2.1998 */
        if (i<0) return -1;
    }
    return 1;
}

static
int check_vartype( int vix,   /* vix = variable index               */
                  char msk,   /* msk = mask character               */
                  char typ,   /* typ = required variable type (N/S) */
                   int rle,   /* rle = required length of variable  */
                   int mle,   /* mle = minimum length of variable   */
                  char *nam)  /* nam = description of variable      */
{
    int some_err=0;

    if (vix<0) return 1; /* variable not found this time */

    switch (typ) {
     case 'S':
        if (dat.vartype[vix][0]!='S') {
            some_err=1;
            muste_kv_s_err("The '%c' variable (%s) requires type S!",
                       msk,          nam);
        } else { /* 'S' */
            if (!dat.varlen[vix]>=rle) {
                some_err=1;
                muste_kv_s_err("The '%c' variable (%s) must be at least %d bytes!",
                           msk,          nam,                  rle);
            }
        }
        break;
     case 'N':
        if (dat.vartype[vix][0]!='S') {
            if (dat.varlen[vix]<rle) {
                some_err=1;
                muste_kv_s_err("The '%c' variable (%s) must be at least %d bytes!",
                           msk,          nam,                 rle);
            }
        } else { /* 'S' */
            if (dat.varlen[vix]<mle) {
                some_err=1;
                muste_kv_s_err("The '%c' variable (%s) must be at least %d bytes!",
                           msk,          nam,                 mle);
            }
        }
        break;
    }
    return (some_err) ? -1 : 1;
}

static int check_input_output(void)
{
    int i;

    if (Dvar<0 && Mvar<0 && Yvar<0 && ivar<0) {
        muste_kv_s_err("No input variables!");
        return -1;
    }
    Dvars=0; Datevar=-1;
    if (Dvar>=0 && Mvar<0 && Yvar<0) {
        Datevar=Dvar; /* whole date activated by 'D'   */
        Dvar=-1;
        for (i=0, Dvars=0; i<dat.m_act; i++) {
            if (dat.vartype[dat.v[i]][1]=='D') Dvars++;
        }
        ivar=-1; /* disable possible ivar in this case */
    }

    if (Dvars==0) { /* old D,M,Y method */
        if (ivar>=0) {
            if (Dvar>=0 || Mvar>=0 || Yvar>=0) {
                if (etu==0)
                    muste_kv_s_err("Ignoring D, M, and Y masks, as i mask is present...");
                Dvar=-1; Mvar=-1; Yvar=-1;
            }
        } else if (Dvar<0 || Mvar<0 || Yvar<0) {
            muste_kv_s_disp("\nDays, months and years must all be present in data!");
            muste_kv_s_err("More general options also available, see DATE?");
            return -1;
        }
        if (newdate<0 && weekday<0 && dayofyear<0 &&
                             weekofyear<0 && julday<0) {
            muste_kv_s_err("No output variables!");
            return -1;
        }
    } else {
        if (iDLen==0) {
            muste_kv_s_err("IDATE=<format> is required!");
            return -1;
        }
        if (Dvars==1) {
            if (weekday>=0 || dayofyear>=0 || weekofyear>=0 || julday>=0) {
                if (newdate<0) {
                    oDLen=0; /* no conversions at the same time! */
                }
            } else {
                if (newdate>=0) {
                    if (oDLen==0) {
                        if (DLen==0) { /* also DATE ok for compatibility */
                            muste_kv_s_err("ODATE=<format> is required!");
                            return -1;
                        }
                    }
                } else {
                    if (oDLen==0) {
                        muste_kv_s_err("ODATE=<format> is required!");
                        return -1;
                    }
                }
            }
        } else { /* Dvars > 1 */
            if (weekday>=0 || dayofyear>=0 || weekofyear>=0 || julday>=0) {
                muste_kv_s_err("Masks a,j,w,J can not be used with several D's!");
                return -1;
            }
            if (newdate>=0) {
                muste_kv_s_err("Mask d can not be used with several D's!");
                return -1;
            }
            if (oDLen==0) {
                muste_kv_s_err("ODATE=<format> is required!");
                return -1;
            }
        }
    }
    return 1;
}

static void apply_date_rules(void);
static void free_date_rules(void);
static int count_date_rules(void);
static int alloc_date_rules(void);
static int read_date_rules(void);
static int valid_date(int, int, int);
static int scan_values(int, int, int *, int *, int *, int *, int *, int *);
static int montoi(char *);

static void data_dates(void)
{
    int i;

    i=data_open2(word[1],&dat,1,0,0); if(i<0) return;
    i=conditions(&dat); if(i<0) return; // moved here 11.2.2005 (!) to cover diff's
    i=check_specifications(); if(i<0) return;
    if (g>2) { /* differences etc. */
        i=count_date_rules(); if (i<0) return;
        i=alloc_date_rules(); if (i<0) return;
        i=read_date_rules(); if (i<0) return;
        apply_date_rules();
        free_date_rules();
    } else { /* original functions */
        i=check_activations(); if(i<0) return;
        i=check_input_output(); if(i<0) return;
        update_data();
    }
    data_close(&dat);
}

static int count_date_rules(void)
{
    int i;
    char *p;

    i=wfind(rulestr,word[2],1);
    if (i<0) {
        muste_kv_s_err("%s %s not found!",rulestr,word[2]);
        return -1;
    }
    if (iDLen==0) {
        muste_kv_s_err("IDATE=<format> is required!");
        return -1;
    }
    rules=0;
    ruleline=i+1;
    while (1) {
        if (++i==ed2) break;
        edread(rule,i);
        p=rule+1;
        if ((!strcmp(p,space)) || (!strncmp(p,"END",3))) break;
        rules++;
    }
    if (!rules) {
        muste_kv_s_err("%s %s is empty!",rulestr,word[2]);
        return -1;
    }
    return 1;
}

static int alloc_date_rules(void)
{
    d1=(int *)muste_malloc(rules*sizeof(int));
    if (d1==NULL) { no_mem(); return -1; }
    d2=(int *)muste_malloc(rules*sizeof(int));
    if (d2==NULL) { no_mem(); return -1; }
    dd=(int *)muste_malloc(rules*sizeof(int));
    if (dd==NULL) { no_mem(); return -1; }
    return 1;
}

static int read_date_rules(void)
{
    int i,j;
    char *p,*p0;
    char vtyp;
    int vlen;

    for (i=0; i<rules; i++) {
        edread(rule,ruleline+i);
        p0=rule+1;
        while (*p0!='\0' && *p0==' ') p0++;
        p=p0;
        while (*p!='\0' && *p!=' ') p++; *p='\0';
        p=p0;
        if (strchr(p,'=')==NULL || strchr(p,'-')==NULL) {
            muste_kv_s_err("Error in %s %s on line %d!",rulestr,word[2],ruleline+i);
            return -1;
        }
        p=p0;
        while (*p!='\0') p++;
        while (*p!='-' && p!=p0) p--; p++;
        j=varfind(&dat,p); if (j<0) return -1;
        d1[i]=j;
        p--; *p='\0';
        while (*p!='=' && p!=p0) p--; p++;
        j=varfind(&dat,p); if (j<0) return -1;
        d2[i]=j;
        p--; *p='\0';
        vtyp='2'; vlen=0;
        if (strchr(p0,':')!=NULL) {
            while (*p!=':') p--; p++;
            vtyp=*p;
            if (*p=='S') {
                p++; vlen=atoi(p); p--;
            }
            p--; *p='\0';
        }
        p=p0;
        j=varfind2(&dat,p,0);
        if (j<0) {
            j=create_newvar(&dat,p,vtyp,vlen);
            if (j<0) return -1;
        }
        dd[i]=j;
    }
    return 1;
}

static void apply_date_rules(void)
{
    int pv1,kk1,vv1,pv2,kk2,vv2;
    int hh1,mm1,ss1,hh2,mm2,ss2; /* lisätty adhoc (Koverhar!) 7.4.2002 */
    int l,bad;
    int Jday1,Jday2;
    int i;

    bad=0L; muste_kv_s_disp("\nComputing date differences...");
    for (l=dat.l1; l<=dat.l2; l++) {
        if (unsuitable(&dat,l)) continue;
        time(&tnow); D=localtime(&tnow); /* siirretty loopin sisään */
        if (prind) muste_kv_s_disp("%d ",l);
     // if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }

        for (i=0; i<rules; i++) {
            Jday1=0L;
            scan_values(d1[i],l,&pv1,&kk1,&vv1,&hh1,&mm1,&ss1);
            if (valid_date(pv1,kk1,vv1)) {
                if (vv1<100) {             /* properly not until 26.2.2001 :( */
                    if (shift) {
                      if (vv1<shift) vv1+=100; /*   YY: shifting if necessary */
                    } else {
                      vv1+=century-1900;       /*   YY: increment of 0 or 100 */
                    }
                }
                if (vv1>199) vv1-=1900;        /* YYYY: scaling for reference */
                D->tm_year=vv1; D->tm_mon=kk1-1; D->tm_mday=pv1;
                Jday1=juldn(D);
            }
            Jday2=0L;
            scan_values(d2[i],l,&pv2,&kk2,&vv2,&hh2,&mm2,&ss2);
            if (valid_date(pv2,kk2,vv2)) {
                if (vv2<100) {             /* properly not until 26.2.2001 :( */
                    if (shift) {
                      if (vv2<shift) vv2+=100; /*   YY: shifting if necessary */
                    } else {
                      vv2+=century-1900;       /*   YY: increment of 0 or 100 */
                    }
                }
                if (vv2>199) vv2-=1900;        /* YYYY: scaling for reference */
                D->tm_year=vv2; D->tm_mon=kk2-1; D->tm_mday=pv2;
                Jday2=juldn(D);
            }
            if (Jday1 && Jday2) {
                if (itime) { /* ero SEKUNTEINA! */ /* ol. pvm ok -> klo ok */
                    ss2+=hh2*60*60+mm2*60;
                    ss1+=hh1*60*60+mm1*60;
                    sprintf(sbuf, "%d", (ss2-ss1)+(Jday2-Jday1)*24*60*60 );
                } else {
                    sprintf(sbuf, "%d", Jday2-Jday1);
                }
                if (dat.vartype[dd[i]][0]=='S')
                    data_alpha_save(&dat,l,dd[i],sbuf);
                else data_save(&dat,l,dd[i],atof(sbuf));
            } else {
                data_save(&dat,l,dd[i],MISSING8); /* 16.11.1999 */
                bad++;
            }
        }
    }
    if (bad && etu==0) { /* 2.1.1998 */
        muste_kv_s_err("%d date%scould not be processed!",
              bad, (bad>1) ? "s " : " ");
    }
}

static void free_date_rules(void)
{
    muste_free(dd); muste_free(d2); muste_free(d1);
}

static void update_data(void)
{
    int pv,kk,vv;
    int hh,mm,ss;
    int l,bad;
    int i;

    bad=0L; muste_kv_s_disp("\nUpdating data...");
    for (l=dat.l1; l<=dat.l2; l++) {
        if (unsuitable(&dat,l)) continue;
        time(&tnow); D=localtime(&tnow);
        if (prind) muste_kv_s_disp("%d ",l);
     // if (kbhit()) { getch(); if (kbhit()) getch(); prind=1-prind; }

        i=0;
        while (1) {
            if (Dvars) {
                Datevar=-1;
                while (i<dat.m_act) {
                    if (dat.vartype[dat.v[i]][1]=='D') {
                        Datevar=dat.v[i]; i++;
                        break;
                    }
                    i++;
                }
                if (Datevar<0) break; /* no more D's found */
                scan_values(Datevar,l,&pv,&kk,&vv,&hh,&mm,&ss);
            } else { /* old D,M,Y method and Julian day to date conversion */
                if (ivar>=0) {
                    Jday=read_value(ivar,l);
                    if (Jday<0) { /* 25.1.2001 (MISSING) */
                        bad++;
                    } else {
                        Jday+=day0;
// printf("\nupdate_data()... Jday=%d", Jday); getch();  /* test 21.2.2009 - OK! */
                        D=(struct tm *)julcd(Jday);
                        write_values(l);
                    }
                    break; /* from while(1) */
                } else {
                    pv=read_value(Dvar,l);
                    kk=read_value(Mvar,l);
                    vv=read_value(Yvar,l);
                }
            }

            if (valid_date(pv,kk,vv)) {
                if (vv<100) {              /* properly not until 26.2.2001 :( */
                    if (shift) {
                      if (vv<shift) vv+=100;   /*   YY: shifting if necessary */
                    } else {
                      vv+=century-1900;        /*   YY: increment of 0 or 100 */
                    }
                }
                if (vv>199) vv-=1900;          /* YYYY: scaling for reference */
                D->tm_year=vv; D->tm_mon=kk-1; D->tm_mday=pv;
                Jday=juldn(D); /* let's use Julian routines! */ /* 12.2.1998 */
                Jday-=day0; /* 13.2.1998 */
                write_values(l);
            } else {
                bad++;
            }
            if (Dvars) continue; else break;
        }
    }
    if (bad && etu==0) { /* 2.1.1998 */
        muste_kv_s_err("%d date%scould not be processed!",
              bad, (bad>1) ? "s " : " ");
    }
}

static int valid_date(int pv, int kk, int vv)
{
    if (pv<1 || pv>31) return 0;
    if (kk<1 || kk>12) return 0;
    if ((kk==4||kk==6||kk==9||kk==11)&&pv>30) return 0;
    if (kk==2) {
        if (pv>29) return 0;
        if (!Leap_Year(vv) && pv==29) return 0;
    }
    return 1;
}

static int scan_values(int var, int obs, int *pv, int *kk, int *vv, int *hh, int *mm, int *ss)
{
    int k;
    double x;
    int len;
    char *p;

    if (dat.vartype[var][0]=='S') {
        data_alpha_load(&dat,obs,var,Dstr);
    } else { /* numeric vars, for example "19991012" etc. */
        data_load(&dat,obs,var,&x);
        if (x==MISSING8) x=0.0;
        k=(int)x;
        sprintf(Dstr,"%d",k);
    }
    p=Dstr;
    while (*p!='\0') p++; p--; while (*p==' ') { *p='\0'; --p; }
    len=strlen(Dstr);
    p=Dstr;
    *pv=-1; *kk=-1; *vv=-1; /* initial values! */

    if (iDid<20) {      /* (näissä ei vielä hhmmss, vaatii IDATE & ITIME) */
        switch (iDid) {
            case  1:                    /* DD.MM.YY    */
            case  4:                    /* DD.MM.YYYY  */
            case  2:                    /* DD/MM/YY    */
            case  5:                    /* DD/MM/YYYY  */
                    *pv=atoi(p);
                     while (*p!='\0' && *p!='.' && *p!='/') p++;
                    *kk=atoi(++p);
                     while (*p!='\0' && *p!='.' && *p!='/') p++;
                    *vv=atoi(++p);
                     break;
            case  3:                    /* MM/DD/YY    */
            case  6:                    /* MM/DD/YYYY  */
                    *kk=atoi(p);
                     while (*p!='\0' && *p!='.' && *p!='/') p++;
                    *pv=atoi(++p);
                     while (*p!='\0' && *p!='.' && *p!='/') p++;
                    *vv=atoi(++p);
                     break;
            case  7:                    /* DD-MMM-YYYY */
                    *pv=atoi(p);
                     while (*p!='\0' && *p!='-') p++;
                    *kk=montoi(++p);
                     while (*p!='\0' && *p!='-') p++;
                    *vv=atoi(++p);
                     break;
            default: break;
        }
        return 1;
    }

    if (idellen) { /* delimited dates */
        switch (iDid) {
            case 21:                    /* DD MM YY      */
            case 33:                    /* DD MM YYYY    */
                    *pv=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *kk=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *vv=atoi(++p);
                     break;
            case 27:                    /* DD MMM YY     */
            case 39:                    /* DD MMM YYYY   */
                    *pv=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *kk=montoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *vv=atoi(++p);
                     break;
            case 22:                    /* DD YY MM      */
            case 34:                    /* DD YYYY MM    */
                    *pv=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *vv=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *kk=atoi(++p);
                     break;
            case 28:                    /* DD YY MMM     */
            case 40:                    /* DD YYYY MMM   */
                    *pv=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *vv=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *kk=montoi(++p);
                     break;
            case 23:                    /* MM DD YY      */
            case 35:                    /* MM DD YYYY    */
                    *kk=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *pv=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *vv=atoi(++p);
                     break;
            case 29:                    /* MMM DD YY     */
            case 41:                    /* MMM DD YYYY   */
                    *kk=montoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *pv=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *vv=atoi(++p);
                     break;
            case 24:                    /* MM YY DD      */
            case 36:                    /* MM YYYY DD    */
                    *kk=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *vv=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *pv=atoi(++p);
                     break;
            case 30:                    /* MMM YY DD     */
            case 42:                    /* MMM YYYY DD   */
                    *kk=montoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *vv=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *pv=atoi(++p);
                     break;
            case 25:                    /* YY DD MM      */
            case 37:                    /* YYYY DD MM    */
                    *vv=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *pv=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *kk=atoi(++p);
                     break;
            case 31:                    /* YY DD MMM     */
            case 43:                    /* YYYY DD MMM   */
                    *vv=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *pv=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *kk=montoi(++p);
                     break;
            case 26:                    /* YY MM DD      */
            case 38:                    /* YYYY MM DD    */
                    *vv=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *kk=atoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *pv=atoi(++p);
                     break;
            case 32:                    /* YY MMM DD     */
            case 44:                    /* YYYY MMM DD   */
                    *vv=atoi(p);
                     while (*p!='\0' && *p!=*idel1) p++;
                    *kk=montoi(++p);
                     while (*p!='\0' && *p!=*idel2) p++;
                    *pv=atoi(++p);
                     break;
            default: break;
        }
    } else { /* non-delimited dates */
        if (len!=iDLen) return -1; /* required for non-delimited! */
        switch (iDid) {
            case 21:                    /* DDMMYY      */
            case 33:                    /* DDMMYYYY    */
                     p+=4; *vv=atoi(p); *p='\0';
                     p-=2; *kk=atoi(p); *p='\0';
                     p-=2; *pv=atoi(p);
                     break;
            case 27:                    /* DDMMMYY     */
            case 39:                    /* DDMMMYYYY   */
                     p+=5; *vv=atoi(p); *p='\0';
                     p-=3; *kk=montoi(p); *p='\0';
                     p-=2; *pv=atoi(p);
                     break;
            case 22:                    /* DDYYMM      */
                     p+=4; *kk=atoi(p); *p='\0';
                     p-=2; *vv=atoi(p); *p='\0';
                     p-=2; *pv=atoi(p);
                     break;
            case 34:                    /* DDYYYYMM    */
                     p+=6; *kk=atoi(p); *p='\0';
                     p-=4; *vv=atoi(p); *p='\0';
                     p-=2; *pv=atoi(p);
                     break;
            case 28:                    /* DDYYMMM     */
                     p+=4; *kk=montoi(p); *p='\0';
                     p-=2; *vv=atoi(p); *p='\0';
                     p-=2; *pv=atoi(p);
                     break;
            case 40:                    /* DDYYYYMMM   */
                     p+=6; *kk=montoi(p); *p='\0';
                     p-=4; *vv=atoi(p); *p='\0';
                     p-=2; *pv=atoi(p);
                     break;
            case 23:                    /* MMDDYY      */
            case 35:                    /* MMDDYYYY    */
                     p+=4; *vv=atoi(p); *p='\0';
                     p-=2; *pv=atoi(p); *p='\0';
                     p-=2; *kk=atoi(p);
                     break;
            case 29:                    /* MMMDDYY     */
            case 41:                    /* MMMDDYYYY   */
                     p+=5; *vv=atoi(p); *p='\0';
                     p-=2; *pv=atoi(p); *p='\0';
                     p-=3; *kk=montoi(p);
                     break;
            case 24:                    /* MMYYDD      */
                     p+=4; *pv=atoi(p); *p='\0';
                     p-=2; *vv=atoi(p); *p='\0';
                     p-=2; *kk=atoi(p);
                     break;
            case 36:                    /* MMYYYYDD    */
                     p+=6; *pv=atoi(p); *p='\0';
                     p-=4; *vv=atoi(p); *p='\0';
                     p-=2; *kk=atoi(p);
                     break;
            case 30:                    /* MMMYYDD     */
                     p+=5; *pv=atoi(p); *p='\0';
                     p-=2; *vv=atoi(p); *p='\0';
                     p-=3; *kk=montoi(p);
                     break;
            case 42:                    /* MMMYYYYDD   */
                     p+=7; *pv=atoi(p); *p='\0';
                     p-=4; *vv=atoi(p); *p='\0';
                     p-=3; *kk=montoi(p);
                     break;
            case 25:                    /* YYDDMM      */
                     p+=4; *kk=atoi(p); *p='\0';
                     p-=2; *pv=atoi(p); *p='\0';
                     p-=2; *vv=atoi(p);
                     break;
            case 37:                    /* YYYYDDMM    */
                     p+=6; *kk=atoi(p); *p='\0';
                     p-=2; *pv=atoi(p); *p='\0';
                     p-=4; *vv=atoi(p);
                     break;
            case 31:                    /* YYDDMMM     */
                     p+=4; *kk=montoi(p); *p='\0';
                     p-=2; *pv=atoi(p); *p='\0';
                     p-=2; *vv=atoi(p);
                     break;
            case 43:                    /* YYYYDDMMM   */
                     p+=6; *kk=montoi(p); *p='\0';
                     p-=2; *pv=atoi(p); *p='\0';
                     p-=4; *vv=atoi(p);
                     break;
            case 26:                    /* YYMMDD      */
                     p+=4; *pv=atoi(p); *p='\0';
                     p-=2; *kk=atoi(p); *p='\0';
                     p-=2; *vv=atoi(p);
                     break;
            case 38:                    /* YYYYMMDD    */
                     p+=6; *pv=atoi(p); *p='\0';
                     p-=2; *kk=atoi(p); *p='\0';
                     p-=4; *vv=atoi(p);
                     break;
            case 32:                    /* YYMMMDD     */
                     p+=5; *pv=atoi(p); *p='\0';
                     p-=3; *kk=montoi(p); *p='\0';
                     p-=2; *vv=atoi(p);
                     break;
            case 44:                    /* YYYYMMMDD   */
                     p+=7; *pv=atoi(p); *p='\0';
                     p-=3; *kk=montoi(p); *p='\0';
                     p-=4; *vv=atoi(p);
                     break;
            default: break;
        }
    }

    if (itime) { /* myöhemmin järjestettävä paremmin */
       *hh=-1; *mm=-1; *ss=-1; /* initial values! */
        p=Dstr; p++;
        while (*p!='\0') { p++; if (*p==' ') break; }
       *hh=atoi(++p);
        while (*p!='\0' && *p!=':') p++;   /* toistaiseksi vain muoto HH:MM:SS */
       *mm=atoi(++p);
        while (*p!='\0' && *p!=':') p++;
       *ss=atoi(++p);
    }

    return 1;
}

static int montoi(char *mon)
{
    if (!muste_strnicmp(mon,"Jan",3)) return 1; else
    if (!muste_strnicmp(mon,"Feb",3)) return 2; else
    if (!muste_strnicmp(mon,"Mar",3)) return 3; else
    if (!muste_strnicmp(mon,"Apr",3)) return 4; else
    if (!muste_strnicmp(mon,"May",3)) return 5; else
    if (!muste_strnicmp(mon,"Jun",3)) return 6; else
    if (!muste_strnicmp(mon,"Jul",3)) return 7; else
    if (!muste_strnicmp(mon,"Aug",3)) return 8; else
    if (!muste_strnicmp(mon,"Sep",3)) return 9; else
    if (!muste_strnicmp(mon,"Oct",3)) return 10; else
    if (!muste_strnicmp(mon,"Nov",3)) return 11; else
    if (!muste_strnicmp(mon,"Dec",3)) return 12;
    return 0; /* */
}

static int read_value(int var, int obs)
{
    int k;
    double x;

    if (dat.vartype[var][0]=='S') {
        data_alpha_load(&dat,obs,var,sbuf);
        k=atoi(sbuf);
    } else { /* numeric vars */
        data_load(&dat,obs,var,&x);
        if (x==MISSING8) x=-1.0;     /* oli "0.0" 25.1.2001 */
        k=(int)x;
    }
    return k;
}

static void write_values(int obs)
{
    if (newdate>=0) { /* mask 'd' */
        if (oDLen) {
            strftime(sbuf, oDLen+1, outdate_format, D);
        } else { /* use DATE specification (or its default value) */
            strftime(sbuf, DLen+1, date_format, D);
        }
        data_alpha_save(&dat,obs,newdate,sbuf);
    } else { /* no 'd' mask! */
        if (Dvars && oDLen) { /* 16.11.1999 */
            strftime(sbuf, oDLen+1, outdate_format, D);
            data_alpha_save(&dat,obs,Datevar,sbuf); /* TRANSFORM */
            if (Dvars>1) return; /* no other things at the same time */
        }
    }

    if (weekday>=0) {
        strftime(sbuf, 3+1, "%a", D);
        data_alpha_save(&dat,obs,weekday,sbuf);
    }

    if (dayofyear>=0) {
        strftime(sbuf, 3+1, "%j", D);
        if (dat.vartype[dayofyear][0]=='S')
          data_alpha_save(&dat,obs,dayofyear,sbuf);
        else data_save(&dat,obs,dayofyear,atof(sbuf));
    }

    if (weekofyear>=0) {
        int i;
        i=weekno(D->tm_mday, D->tm_mon+1, D->tm_year+1900); /* 2.1.1998 */
        sprintf(date_str, "%d", i);
        strcpy(sbuf, "");
        if (i<10) strcat(sbuf,"0");
        strcat(sbuf, date_str);

        if (dat.vartype[weekofyear][0]=='S')
          data_alpha_save(&dat,obs,weekofyear,sbuf);
        else data_save(&dat,obs,weekofyear,atof(sbuf));
    }

    if (julday>=0) { /* 12.2.1998 */
        sprintf(sbuf, "%d", Jday);
        if (dat.vartype[julday][0]=='S')
          data_alpha_save(&dat,obs,julday,sbuf);
        else data_save(&dat,obs,julday,atof(sbuf));
    }
}

/*****************************************
// 4.6.2004:

static char *spec_date[]={ "VARS", "MASK",
                    "DAYS", "SOUND", "DATE", "IDATE", "IDEL", "IDEL1",
                    "IDEL2", "ODATE", "ODEL", "ODEL1", "ODEL2", "PRIND",
                    "SHIFT", "CENTURY", "ITIME", "JULIAN_DAY0", "!" };

static char **specs=spec_date;
******************************************/

void muste_date(char *argv)
{
    int i;

    suomi=0; // RS Init variables

    s_init(argv);
    tut_init();
    i=spec_init(r1+r-1);
    if (i<0) return;

    setlocale(LC_TIME, "C"); // 8.5.2011

    if (!muste_strcmpi(word[0],"PVM")) suomi=1; /* PVM-komento 11.12.2000 (SM:n ehdotus) */
    i=check_parameters();
    switch (i) {
        case 1: show_dates();
                break;
        case 2: data_dates();
                break;
       default: return;
    }
    tut_end();
    s_end(argv);
    return;
}
