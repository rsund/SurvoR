#if 0

/* desktop.c xx.x.1992/KV (27.12.2008)
   converted for Muste 8.6.2011/KV (8.6.2011)
 */


// BEGIN DESKTOP /////////////////////////////////////////////////////////////////


#include <stdio.h>
#include <stdlib.h>
#include <direct.h>
#include <io.h>
#include <malloc.h>
#include <math.h>
#include <process.h>
#include <time.h>
#include <stdarg.h>
#include <fcntl.h>

#include "survo.h"
#include "survoext.h"
#include "survolib.h"

/* typedefit ym. yhteistä alkuun (Muste) */

#define SORTOPTLEN       6
#define GRPOPTLEN  LNAME/2
#define MAXGROUPS       13

typedef struct globs {        /* global variables */
    char grouping[GRPOPTLEN]; /* GROUPING information */
    char sorting[SORTOPTLEN]; /* SORT information */
    int required;             /* required screen width */
    int bigglen;              /* length of the biggest file size */
    int commlen;              /* max length of the comments */
    int filecount;            /* # of files not deleted */
                              /* # of 'active' files in the source dir */
    int tfilecount;           /* # of files in the target directory */
    char filespec[LNAME];   /* the current path selection */ // was LNAME/2 until 2.1.2009
    int dircount;             /* # of directories */
    int totalcount;           /* # of files, also deleted files */
    char status;              /* status flags */
    char source[LNAME];     /* the source path (may include file) */ // was LNAME/2 until 2.1.2009
    char target[LNAME];     /* the target path (may include file) */ // was LNAME/2 until 2.1.2009
    char sourcedir[LNAME];  /* the source directory */ // was LNAME/2 until 2.1.2009
    char targetdir[LNAME];  /* the target directory */ // was LNAME/2 until 2.1.2009
    unsigned long bytes;      /* # of bytes in the tree */
    unsigned long files;      /* # of files in the directory */
    char treename[LNAME];   /* name of the tree file */ // was LNAME/4 until 2.1.2009
    FILE *treefile;           /* file pointer of the tree file */
    char path[LNAME];       /* path name which has been checked */ // was LNAME/2 until 2.1.2009
    int level;                /* how many levels are displayed (1.5.97) */
    int outfile;              /* print tree to file and exit (2.5.97) */
    int print_date;           /* how dates are printed */
    int print_time;           /* how times are printed */
    int print_size;           /* is size printed at all (21.7.98) */
} Globals;


#define NAMELEN LNAME      /* 256       */
#define TYPELEN LNAME/16   /* 256/16=16 */
#define CMDLEN  LNAME/4    /* 256/4=64  */
#define GRPTYPS LNAME/4    /* 256/4=64  */
#define COMMLEN LNAME      /* 256       */
#define PATHLEN LNAME      /* 256       */ // was LNAME/2 until 2.1.2009

typedef struct fileinfo *FIPtr;
typedef struct fileinfo {  /* file information structure */
    unsigned order;        /* array sorting number */
    char name[NAMELEN];    /* file name */
    char type[TYPELEN];    /* type name */
    char cmd[CMDLEN];      /* space for the command */
    char attrib;           /* file attribute (OS) */
    unsigned long size;    /* file size in bytes */
    int year;              /* file modification year */
    int month;             /* file modification month */
    int day;               /* file modification day */
    int hour;              /* file modification hour */
    int minute;            /* file modification minute */
    int second;            /* file modification second */
    char status;           /* file flags: sorted,marked,deleted */
    int command;           /* which command to use later */
    char comment[COMMLEN]; /* the comment string */ /* fixed 31.12.97 */
    int grouporder;        /* final order number */
    char path[PATHLEN];    /* path of the file (WHERE) */ /* fixed 31.12.97 */
    int match;             /* source-target matching (DM), line nr (SEARCH/DD) 30.10.1998 */
    short printed;         /* printed out (INDEX) */
    short get_comments;    /* get some sort of comments (INDEX) */
    short notype;          /* don't print file type (INDEX) */
    short hidden_comment;  /* show comment only under cursor (DD) */ /* 2.1.98 */
    char longname[NAMELEN];/* long file name (added for DD 12.5.2003 */
    FIPtr next;            /* next file in the linked list */
} Files;

#define NFILES (sizeof(Files))


typedef struct filetypes {  /* file types and default commands */
    char type[TYPELEN];     /* file type */
    char cmd[CMDLEN];       /* the default command */
    short get_comments;     /* get some sort of comments: 1, not: 0 */
    short notype;           /* don't print file type: 1, print: 0 */
}   Types;



#define SUBDIR(x) (((x).attrib & _A_SUBDIR)==_A_SUBDIR)
#define THISDIR(x) ((SUBDIR(x)) && (((x).name[0]=='.' &&  (x).name[1]!='.')))
#define PARENTDIR(x) ((SUBDIR(x)) && (((x).name[0]=='.' && (x).name[1]=='.')))
#define REALDIR(x) (((x).name[0]!='.'))

#define ReadOnlyFile(x) (((x)->attrib & _A_RDONLY) == _A_RDONLY)
#define ArchiveFile(x)  (((x)->attrib & _A_ARCH)   == _A_ARCH)
#define SystemFile(x)   (((x)->attrib & _A_SYSTEM) == _A_SYSTEM)
#define HiddenFile(x)   (((x)->attrib & _A_HIDDEN) == _A_HIDDEN)
#define SubDirectory(x) (((x)->attrib & _A_SUBDIR) == _A_SUBDIR)

#define FSORTED     0x01  /* file sorted */
#define FMARKED     0x02  /* file marked for copying, deletion etc */
#define FDELETED    0x04  /* file deleted (lazy deletion) */
#define FNOTFILE    0x08  /* not file, just placeholder (DM) 12.9.97 */

#define FileSorted(x)   (((x)->status & FSORTED)   == FSORTED)
#define FileMarked(x)   (((x)->status & FMARKED)   == FMARKED)
#define FileDeleted(x)  (((x)->status & FDELETED)  == FDELETED)
#define FileNotFile(x)  (((x)->status & FNOTFILE)  == FNOTFILE)

#define FirstFile(x)    ((x)==&files[0])
#define LastFile(x)     ((x)==&files[GV.filecount-1])

/* In SURVO 84C, LNAME used to be 64 ... needed in prompts etc.! */
#define LNA64   64


#define LOAD       76   /* L */
#define COPY       67   /* C */
#define MARK       43   /* + */
#define MARK2      42   /* * */
#define UNMARK     45   /* - */
#define DELETE     68   /* D */
#define MOVE       77   /* M */
#define NEXT       78   /* N */
#define PREV       80   /* P */
#define RENAME     82   /* R */
#define FREE       70   /* F */
#define SORT       83   /* S */
#define ATTRIB     65   /* A */
#define GROUP      71   /* G */
#define WHERE      87   /* W */
#define SYSTEM     62   /* > */
#define TREE       84   /* T */
#define MATCHING   77   /* M */
#define ALLFILES   65   /* A */
#define UPDATE     85   /* U */
#define EXCHANGE   88   /* X */
#define PREDOWN   190   /* F2 down   */
#define MARKALL   191   /* F2 MARK   */
#define UNMARKALL 192   /* F2 UNMARK */
#define LOAD2     108   /* l */
#define TREE2     116   /* t */
#define LEVEL1     49   /* 1 */
#define LEVEL2     50   /* 2 */
#define LEVEL3     51   /* 3 */
#define LEVEL4     52   /* 4 */
#define LEVEL5     53   /* 5 */
#define LEVEL6     54   /* 6 */
#define LEVEL7     55   /* 7 */
#define LEVEL8     56   /* 8 */
#define LEVEL9     57   /* 9 */
#define LEVEL0     48   /* 0 */
#define FULLNAME   70   /* F */
#define DDCALL     68   /* D */ /* 15.12.1997 */
#define FILL       70   /* F */ /* 13.7.1998 */
#define BOTH       66   /* B */ /* 13.7.1998 */ /* Both=Update&Fill */
#define OPEN       79   /* O */ /* 28.4.2008 for DD (test!) [ready 10 min later!] */

//#define ScreenWidth     80
static int ScreenWidth; // in certain critical places, 80 is used!
#define WORKROW          2
#define WORKROW1         2
#define WORKROW2         3
#define WORKROW3         4
#define ENDROW      (r3+1)
#define BOTTOMROW   (r3+2)
#define ddROWS      (r3-2) // was r3-1 (12.5.2003)
#define ddSTARTROW       4 // was 3    (12.5.2003)
#define dmROWS      (r3-4) // was r3-3 (21.4.2003)
#define dmSTARTROW       6 // was 5    (21.4.2003)
#define treeROWS    (r3-1)
#define treeSTARTROW     3

#define TreeDown    "|   "
#define TreeBlank   "    "
#define TreeCorner  "+---"
#define TreeSibling "+---"

#define GNORMAL       0x00 /* DD: normal mode */
#define GWHERE        0x01 /* DD: where-mode on */
#define GUNZIP        0x02 /* DD: unzip-mode on */
#define GTREE         0x04 /* DD: tree-mode (TREE [WHERE] is called) */
#define WhereMode     ((GV.status & GWHERE) == GWHERE)
#define UnzipMode     ((GV.status & GUNZIP) == GUNZIP)
#define TreeMode      ((GV.status & GTREE)  == GTREE)
#define NormalMode    (!WhereMode && !UnzipMode && !TreeMode)

#define GALLFILES      'A' /* match modes for DM */
#define GMATCHING      'M'

// Colors finalized for SURVO MM 7.3.2001
#define Reverse        '7'
#define Reverse2       '/' // '8'
#define Cursor         '/' // '+' // '_'
#define FileColor      'þ' // '´' // 'þ'
#define FileColor1     'þ' // '´' // 'þ'   // tähän FoundColor (ks. alla) ??
#define FileColor2     'Þ' // '2'
#define MarkStr        "+" // "×" // "*"
#define MarkColor1     '¯' // '×'
#define MarkColor2     '.' // '^' // 'Ì'
#define MarkColor3     '¯' // '5'
#define Advice         'ü' // '1'
#define AttribColor    'ý' // 'µ' // 'Õ'
#define AttribCursor   ',' // 'ã' // 'V' // 'W'
#define BackGround     '×' // '4'

#define dmMsg1   "DM: F8=Exit ENTER=Show +=Mark -=Unmark C=Copy     S=Sort G=Grouping  F1=HELP    "
#define dmMsg2   "DM: F8=Exit      To mark files quickly, use: U=Update F=Fill B=Both  F1=HELP    "
#define dmMsg3   "DM: F8=Exit    M=Matching A=All   X=Exchange source & target   D=DD  F1=HELP    "
#define dmMsgP   "DM: PREFIX+ marks all, PREFIX- unmarks all, PREFIX-down=Last file  (PREFIX=F2)  "

#define ddMsg1   "DD: F8=Exit ENTER=Show L=Load "
#define ddMsgA   "C=Copy D=Delete M=Move R=Rename O=Open F1=HELP    " /* 7.9.2009: O=Open FINALLY added */
#define ddMsgB1  "+=Mark -=Unmark A=Attributes T=Tree    F1=HELP    "
#define ddMsgC1  "S=Sort G=Grouping F=Free W=Where       F1=HELP    "
#define ddMsg2   "DD: F8=Return ENTER=Show      "
#define ddMsgB2  "+=Mark -=Unmark A=Attributes           F1=HELP    " /* 1.5.97: no Tree */
#define ddMsgC2  "S=Sort G=Grouping F=Free               F1=HELP    "
#define ddMsgP   "DD: PREFIX+ marks all, PREFIX- unmarks all, PREFIX-down=Last file  (PREFIX=F2)  "
#define treeMsg1 "TREE: F8=Exit ENTER=DD L=Load "
#define treeMsgA "F=Full path names shown/hidden T=Tree  F1=HELP    "
#define treeMsgB "1,2,...,9=Levels displayed (0=All)     F1=HELP    "
#define treeMsgC "                                       F1=HELP    "
#define treeMsgP "TREE: PREFIX+ marks all, PREFIX- unmarks all, PREFIX-down=Last file (PREFIX=F2) "

#define WhiteBottomRow    write_string(space,ScreenWidth,' ',BOTTOMROW,1)
#define WhiteWorkRow      write_string(space,ScreenWidth,' ',WORKROW,1)
#define ReversedWorkRow   write_string(space,ScreenWidth,Reverse,WORKROW,1)
#define ReversedWorkRow1  write_string(space,ScreenWidth,Reverse,WORKROW1,1)
#define ReversedWorkRow2  write_string(space,ScreenWidth,Reverse,WORKROW2,1)
#define ReversedWorkRow3  write_string(space,ScreenWidth,Reverse,WORKROW3,1)
#define WorkRowText(c)    write_string(answer,strlen(answer),' ',WORKROW,c)

// Colors finalized for SURVO MM 7.3.2001
#define CommandLine r+1
#define BottomLine  r3+2
#define MessageLine r+2
#define ShowLine    r+3
#define Reverse     '7'
#define SearchColor 'ù' // '_'
#define FoundColor  'ƒ' // '?'
#define LineColor   '4' // 'þ'
#define MatchColor  '.' // 'Û'
#define CountColor  '/' // 'o'
#define Screaming   '5'
#define FinalColor  '7'
#define BeyondColor '×'
#define Empty       BackGround // ' '




static char *siirtop;         /* argv[1] for spawning processes */
char userline[LLENGTH]; /* original command line */
char answer[LNAME];     /* user's answers - was LNA64 before 15.12.2001 (long file names!) */
static int results_line;      /* first line for the results */
static int case_sensitive_pathnames; /* 24.8.2000 */
static int indexdir; /* 29.1.2001 */
static int no_cd; /* 29.1.2001 */
static char origline[]; /* 27.2.2001 */

static int DDmain(void);         /* DESKTOP functions, former modules */
static int DMmain(void);
static int WHEREmain(void);
static void tree(void);
static int INDEXmain(void);
static int SEARCHmain(void);
static int MDmain(void);
static int RDmain(void);

extern char os_ver[]; // SM (SURVO.LIB) (29.8.2000)
static void disable_softkeys(void); /* 7.6.2000 */
static void enable_softkeys(void);

static FILE *output_file;   /* moved from SEARCH to global, when added ... */
static char outfile[LNAME]; /* ... OUTFILE to WHERE also (15.10.2005) */

static int DDerror;            /* error code (used by display_error) */
#define HARDERR 1
#define BADPATH 2
#define NOFILES 3
static Files *files, *fi, *f;  /* file info structures */
static FIPtr *df;              /* display file pointer */
static Globals GV;             /* structure of global variables */
static char line[LLENGTH];
static char tmp[LLENGTH];
static char path[LNAME];
static char drive[5];
static char dir[LNAME]; // was LNAME/2 until 2.1.2009
static char fname[NAMELEN];
static char ext[TYPELEN];
static int current, markcount, matchcount;
static unsigned long markbytes, totalbytes, ttotalbytes, matchbytes;

static FIPtr FL,FLp,FLpp;
static char origname[LNAME]; // was LNAME/2 until 2.1.2009
       //char origdir[LNAME/2]; // havaittu turhaksi 26.12.2008 (!)
static char filespec[LNAME];
static char fullspec[LNAME];

static int count, order_nr;
static struct _finddata_t fil, fil0;
static long h_fil, h_fil0;
static long biggest;
static int ScreenWidth; /* no more constant 80 !! (except in some critical places) */
static int case_sensitive_pathnames; /* 24.8.2000 */
static int indexdir; /* 29.1.2001 */
static int no_cd; /* 29.1.2001 */
static char origline[LLENGTH]; /* 27.2.2001 */

/* Muste: tähän väliin pari aiempaa omaa funktiotani (trim, s_specu): */
/* SHOW D:\U\MUSTE 86 */

static void trim(char *, char *);
static void trim(char *s, char *t) /* remove extra spaces from the string s */
{
    char *w;
    w=t;
    while(*w) w++; w--;
    while(*w==' ') w--;
    w++;
    *w='\0'; /* that was the end of the string */
    while(*t==' ') t++; /* start of the string */
    while(*t) {         /* the middle */
        if (*t==' ') {
            while (*t==' ') t++;
            t--;
        }
        *s=*t;
        s++; t++;
    }
    *s='\0';
}

#if 0
Muste: parasta siirtyä vaatimaan sama kuin muutenkin Survossa/Musteessa.
extern int spn;
static void s_specu(void); // void since 8.6.2004
static void s_specu (void) // void since 8.6.2004 (spn gives the speccount)
{
    int i;
    for (i=0; i<spn; i++) {
      muste_strupr(spa[i]);
      muste_strupr(spb[i]);
    }
}
#endif


// 4.6.2004:

static
char *spec_desktop[]={ "TUTOR", "UPPER_NAMES", "CD",               // DESKTOP
                       "DATE", "TIME", "GROUPING", "SORT", "SIZE", // GEN*
                       "TUTSTACK", "OPTIONS",                      // DD
                       "SINCE", "BEFORE", "MAXBYTES", "MINBYTES",  // DD ym.
                       "DM", "DISPLAY",                            // DM
                       "PRIND", "TYPES", "ONLY", "CASE", "WIDTH",  // INDEX
                       "OUTFILE", "COMMENTS", "FORMAT",            // INDEX
                       "DIR","EDT","SVO","MAT","TUT","EMF","TXT",  // INDEX
                       "SHOW", "SEARCH", "RUN", "COLS", "FILES",   // SEARCH
                       "LEVEL", "WHERE", "!" };                    // TREE,WHERE
static char **specs=spec_desktop;

//static int op_dir(void); // GEN1
//static int op_DELTREE(void); // DD:O
//static int DDf_free(int);

//int main(int argc, char *argv[])
void muste_desktop(char *argv)
{
    int i;

    siirtop=argv;
    s_init(siirtop);

    i=spec_init(r1+r-1); if (i<0) return;

#if 0
Muste: parasta siirtyä vaatimaan sama kuin muutenkin Survossa/Musteessa.
    s_specu(); /* set all spa:s and spb:s to uppercase */
#endif


#if 0
Muste: ei toistaiseksi!

    i=spfind("TUTOR"); /* 13.9.97  -  a few sucros need DESKTOP */
    if (i>=0) {
        if (!strcmp(spb[i],"/FREEDISK")) {
            DDf_free(1);
            s_end(siirtop);
            return;
        }
    }
#endif

    ScreenWidth=c3+8; /* no more constant 80 in MM !! */
    case_sensitive_pathnames=1; // 0 in Survo, 1 in Muste
    i=spfind("UPPER_NAMES");
    if (i>=0) case_sensitive_pathnames=1-atoi(spb[i]);
    indexdir=0;
    no_cd=0;
    i=spfind("CD");
    if (i>=0) { if (!atoi(spb[i])) no_cd=1; }

    edread(origline,r1+r-1);      /* 27.2.2001 for <Survo> paths          */
    subst_survo_path(origline);   /* "<Survo>" -> C:\PROGRA~1\SURVO\ etc. */
    g=split(origline+1,word,100); /* '+1' omits the control char.         */
                         /* 100 was 5, but INDEX /SHORTNAME needs all!! (9.1.2002) */
    for (i=1; i<g; i++) {
        if (!strcmp(word[i],"/")) { g=i; break; }
    }

         if (!muste_strcmpi(word[0],"INDEX")) INDEXmain();

#if 0
Muste: muut häivytetty toistaiseksi!

    else if (!muste_strcmpi(word[0],"MD")) MDmain(); /* 24.5.1999 (DD:X) */
    else if (!muste_strcmpi(word[0],"RD")) RDmain(); /* 24.5.1999 (DD:X) */
    else if (!muste_strcmpi(word[0],"DIR")) op_dir(); /* 14.9.2000 (GEN1) */
    else if (!muste_strcmpi(word[0],"DELTREE")) op_DELTREE(); /* 6.2.2001 (DD:O) */
    else {
         edread(userline,r1+r-1);    /* command line saved */
         disable_softkeys();
              if (!muste_strcmpi(word[0],"DD")) DDmain();
         else if (!muste_strcmpi(word[0],"DM")) DMmain();
         else if (!muste_strcmpi(word[0],"WHERE")) WHEREmain();
         else if (!muste_strcmpi(word[0],"TREE")) tree();
         else if (!muste_strcmpi(word[0],"SEARCH")) SEARCHmain();
         enable_softkeys();
         edwrite(userline,r1+r-1,0); /* command line restored */
    }
#endif
    s_end(siirtop);

    return;
}

static void disable_softkeys(void) /* 7.6.2000 */
{
    if (r_soft) r3+=r_soft+1;
}

static void enable_softkeys(void)
{
    if (r_soft) r3-=r_soft+1;
}


static void disp_err(char *, ...);
static void display_error(char *);
static void tut_error(int code); /* 28.8.2000 */
static int get_date(char *);
static int get_time(char *);
static int count_date_time_length(void);
static void no_mem(void);

static int init_globals(void);
static void init_prompt(void);
static void clear_screen(void);
static int suorita(char *prog);
static void write_cmd_line(void);
static void show_yesno(char *str);
static void mark_files_unsorted(void);
static int ask_yesno(char *str, char *def);
static void system_call(void);
static void f_sort(void);
static void f_group(void);
static void flash_counter(int); /* 6.6.2000 */

static void disp_err (char *fmt, ...)
{
  char buf[LNAME/2];  /* [LLENGTH]; */
  va_list ap; /* points to each unnamed arg in turn */

  va_start(ap, fmt); vsprintf(buf,fmt,ap); va_end(ap);
  PR_EBLD; write_string(space,ScreenWidth,' ',BOTTOMROW,1);
  LOCATE(BOTTOMROW,1); strcat(buf, " Press any key! ");
  sur_print(buf); sur_getch();
  write_string(space,ScreenWidth,' ',BOTTOMROW,1);
}

static void display_error(char *str)
{
    switch(DDerror) {
       case HARDERR: disp_err(" Device not ready (%s)!", str); break;
       case BADPATH: disp_err(" Bad path (%s)!", str); break;
       case NOFILES: disp_err(" No files found (%s)!",str); break;
    }
}

static void tut_error(int code) /* 28.8.2000 (for SM, softkeys) */
{
    switch(code) {
        case  1: strcpy(tut_info, "___@1@INDEX@No files found!");
                 break;
        case 11: strcpy(tut_info, "___@1@DD@No files found!");
                 break;
       default:  break;
    }
}

static int get_date(char *format)
{
    if (!strcmp(format, "0"))          return 0;
    if (!strcmp(format, "DD.MM.YY"))   return 1;
    if (!strcmp(format, "DD/MM/YY"))   return 2;
    if (!strcmp(format, "DDMMYY"))     return 3;
    if (!strcmp(format, "YYMMDD"))     return 4;
    if (!strcmp(format, "MM/DD/YY"))   return 5;
    if (!strcmp(format, "DD.MM.YYYY")) return 6;  /* 21.7.1998 */
    if (!strcmp(format, "DD/MM/YYYY")) return 7;  /* 21.7.1998 */
    if (!strcmp(format, "MM/DD/YYYY")) return 8;  /* 21.7.1998 */
    if (!strcmp(format, "DDMMYYYY"))   return 9;  /* 21.7.1998 */
    if (!strcmp(format, "YYYYMMDD"))   return 10; /* 21.7.1998 */
    return 1;
}

static int get_time(char *format)
{
    if (!strcmp(format, "0"))        return 0;
    if (!strcmp(format, "HH:MM:SS")) return 1;
    if (!strcmp(format, "HH:MM"))    return 2;
    if (!strcmp(format, "HH.MM.SS")) return 3;
    if (!strcmp(format, "HH.MM"))    return 4;
    return 2;
}

static int count_date_time_length(void)
{
    int j,dtlen;

    dtlen=0;
    GV.print_date=1;
    GV.print_time=2;
    if (indexdir) GV.print_date=6; // DD.MM.YYYY default for DIR (31.1.2001)

    j=spfind("DATE");
    if (j>=0) GV.print_date=get_date(spb[j]);
    switch (GV.print_date) {
        case 1: case 2: case 5: case 9: case 10: dtlen+=9; break;
        case 6: case 7: case 8: dtlen+=11; break;
        case 3: case 4:         dtlen+=7; break;
       default: break;
    }

    j=spfind("TIME");
    if (j>=0) GV.print_time=get_time(spb[j]);
    switch (GV.print_time) {
        case 1: case 3: dtlen+=9; break;
        case 2: case 4: dtlen+=6; break;
       default: break;
    }
    return dtlen;
}

static void no_mem(void)
{
    disp_err("Not enough memory! (%s)", word[0]);
}



/* Muste: julian day -rutiinit kopsattu date:stä, restrictions tarvitsee! */


/*
        declarations for Julian date routines
*/

#define JUL_ROME 2299161L
#define JUL_ENGLAND 2361222L
#define JUL_FINLAND 2361390L /* kv 13.4.96 */

static long jul_transition;

static long juldnj (struct tm *bdt, long Transition);
static long juldn (struct tm *bdt);
static long juldnd (struct tm *bdt, struct tm *Transition_date);

static struct tm *julcdj (long JD, long Transition);
static struct tm *julcd (long JD);
static struct tm *julcdd (long JD, struct tm *Transition_date);

/* SOURCE OF THE JULIAN DAY ALGORITHM:
"Translated from Pascal to C by Jim Van Zandt, July 1992.

        Error-free translation based on error-free PL/I source

        Based on Pascal code copyright 1985 by Michael A. Covington,
        published in P.C. Tech Journal, December 1985, based on formulae
        appearing in Astronomical Formulae for Calculators by Jean Meeus"

Julian (sense 1) date routines, handling both Julian (sense 2) and
Gregorian calendars

SYNOPSIS
        long juldn (struct tm *bdt)
        long juldnj (struct tm *bdt, long Transition)
        long juldnd (struct tm *bdt, struct tm *Transition_date)

        struct tm *julcd (long J)
        struct tm *julcdj (long J, long Transition)
        struct tm *julcdd (long J, struct tm *Transition_date)

        extern long jul_transition;

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

typedef long Julian;
typedef int Year;
typedef int Month;
typedef int Day;
typedef long Work;

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

/* Muste: restrictions -funktio & co siirretty tänne (INDEX ym. tarvitsevat)! */

#include <limits.h> /* for ULONG_MAX */

struct yymmdd_date {
    unsigned short dd;
    unsigned short mm;
    unsigned short yy;
};

static int restrictions(void);
static void no_valid_files(void);
static int valid_date(int);
static int split_date(struct yymmdd_date *, int);

static int restrictions(void) /* new function 1.1.1998, returns TRUE/FALSE */
{                                         /*  or -1 when errorneous */
    int i,j;
#define NA -123456
static int  since_i=NA;
static int before_i=NA;
static int bytmin_i=NA;
static int bytmax_i=NA;
static struct yymmdd_date since_date;
static struct yymmdd_date before_date;
static unsigned long bytmin,bytmax;
    unsigned long size;
    unsigned short year,month,day;

    char *p;

    struct tm *write_time;

    /* read spec.vector only once; init. structures with zeros */
    if (since_i  == NA)  {
        since_i=spfind("SINCE");
        since_date.dd=0; since_date.mm=0; since_date.yy=0;
        if (since_i >=0) {
            i=valid_date(since_i);
            if (i<0) return -1;
        }
    }
    if (before_i == NA) {
        before_i=spfind("BEFORE");
        before_date.dd=0; before_date.mm=0; before_date.yy=0;
        if (before_i >=0) {
            i=valid_date(before_i);
            if (i<0) return -1;
        }
    }
    if (bytmin_i == NA) {
        bytmin=0;
        bytmin_i=spfind("MINBYTES");
        if (bytmin_i >=0) bytmin=atol(spb[bytmin_i]);
    }
    if (bytmax_i == NA) {
        bytmax=ULONG_MAX;
        bytmax_i=spfind("MAXBYTES");
        if (bytmax_i >=0) bytmax=atol(spb[bytmax_i]);
    }

    if (since_i<0 && before_i<0 && bytmin_i<0 && bytmax_i<0) return 0;

    /* check every possible restriction; first failure returns 1 */

    size=fil.size; /* the size of the current file */
    if (size < bytmin || size > bytmax) return 1;

    if (since_i<0 && before_i<0) return 0;

    write_time = localtime(&fil.time_write);
    year   = write_time->tm_year; /* current year - 1900 */
    month  = write_time->tm_mon+1;
    day    = write_time->tm_mday;

    if (since_i >=0) {
        if (!since_date.dd && !since_date.mm && !since_date.yy) {
            i=split_date(&since_date, since_i);   /* only once */
            if (i<0) return -1;
        }
//muste_kv_s_err("since_date: %d.%d.%d", since_date.dd, since_date.mm, since_date.yy);
//muste_kv_s_err("%s: %d.%d.%d",fil.name,day,month,year);
        while (1) { /* (not a real while...) */
            if (year  < since_date.yy) return 1;
            if (year  > since_date.yy) break;
            if (month < since_date.mm) return 1;
            if (month > since_date.mm) break;
            if (day   < since_date.dd) return 1;
            break;
        }
    }
    if (before_i >=0) {
        if (!before_date.dd && !before_date.mm && !before_date.yy) {
            i=split_date(&before_date, before_i); /* only once */
            if (i<0) return -1;
        }
        while (1) { /* (not a real while...) */
            if (year  > before_date.yy) return 1;
            if (year  < before_date.yy) break;
            if (month > before_date.mm) return 1;
            if (month < before_date.mm) break;
            if (day   >=before_date.dd) return 1;
            break;
        }
    }
    return 0;
}

static int valid_date(int idx) /* is the date valid at all */
{
    int i,ch;
    char *p;

    i=0; ch='\0';
    switch(GV.print_date) {
        case 0: /* 0        */
        case 1: /* DD.MM.YY */
        case 6: /* DD.MM.YYYY */
                ch='.';
        case 2: /* DD/MM/YY */
        case 5: /* MM/DD/YY */
        case 7: /* DD/MM/YYYY */
        case 8: /* MM/DD/YYYY */
                if (ch=='\0') ch='/';
                p=strchr(spb[idx],ch); i=(p==NULL);
                if (i==0) { p=strchr(++p,ch); i=(p==NULL); }
                break;
        case 3: /* DDMMYY   */
        case 4: /* YYMMDD   */
                i=(strlen(spb[idx])!=6);
                break;
        case 9: /* DDMMYYYY */
       case 10: /* YYYYMMDD */
                i=(strlen(spb[idx])!=8);
                break;
       default: break;
    }
    if (!muste_strnicmp(spb[idx], "TODAY", 5)) i=0; // (see below)
    if (i) {
        i=spfind("DATE");
        disp_err(" Invalid date: %s=%s (DATE=%s).",
            spa[idx], spb[idx], i>=0 ? spb[i] : "DD.MM.YY");
        return -1;
    }
    return 1;
}

static int split_date(struct yymmdd_date *ddmmyy, int idx)
{
    int i;
    char *p;
    struct tm *D;
    time_t tnow;
    unsigned short dd,mm,yy;
    int rel_time;
    char *rel;

    p=spb[idx];
    if (!muste_strnicmp(p, "TODAY", 5)) {
        time(&tnow);
        D=localtime(&tnow);
        if (strlen(p)>6) { // TODAY-n or +n (Juha Valtonen 24.5.2002)
            rel_time=0;
            rel=strchr(p, '-'); // TODAY-n
            if (rel!=NULL) {
                rel_time=atoi(rel);
            } else {
                rel=strchr(p, '+'); // TODAY+n
                if (rel!=NULL) {
                    rel_time=atoi(rel);
                }
            }
            if (rel_time) {
                D->tm_mday+=rel_time;
                juldn(D); // update the struct (Julian day routines into DESKTOP!)
            }
        }
        dd=D->tm_mday; mm=D->tm_mon+1; yy=D->tm_year;
    } else {
        switch(GV.print_date) {
            case 0: /* 0        */
            case 1: /* DD.MM.YY */
            case 6: /* DD.MM.YYYY */
                    dd=atoi(p); p=strchr(p,'.'); p++;
                    mm=atoi(p); p=strchr(p,'.'); p++;
                    yy=atoi(p);
                    break;
            case 2: /* DD/MM/YY */
            case 7: /* DD/MM/YYYY */
                    dd=atoi(p); p=strchr(p,'/'); p++;
                    mm=atoi(p); p=strchr(p,'/'); p++;
                    yy=atoi(p);
                    break;
            case 3: /* DDMMYY   */
            case 9: /* DDMMYYYY */
                    p+=4; yy=atoi(p); *p='\0';
                    p-=2; mm=atoi(p); *p='\0';
                    p-=2; dd=atoi(p);
                    break;
           case 10: /* YYYYMMDD */
                    p+=6; dd=atoi(p); *p='\0';
                    p-=2; mm=atoi(p); *p='\0';
                    p-=4; yy=atoi(p);
                    break;
            case 4: /* YYMMDD   */
                    p+=4; dd=atoi(p); *p='\0';
                    p-=2; mm=atoi(p); *p='\0';
                    p-=2; yy=atoi(p);
                    break;
            case 5: /* MM/DD/YY */
            case 8: /* MM/DD/YYYY */
                    mm=atoi(p); p=strchr(p,'/'); p++;
                    dd=atoi(p); p=strchr(p,'/'); p++;
                    yy=atoi(p);
                    break;
        }
        if (yy>1900) yy-=1900; // simplified 27.2.2001
    }
    ddmmyy->dd=dd; ddmmyy->mm=mm; ddmmyy->yy=yy;
    return 1;
}

static void no_valid_files(void)
{
    sprintf(sbuf, " No files meet the restrictions!");
    disp_err(sbuf);
}

/* end restrictions */




















#if 0

Muste: nämä poistettu toistaiseksi!


                          /****************/
                          /* Win32 APInat */
                          /****************/
#undef DELETE
#include <windows.h>

int WAPI_ShortPath(char *);
int WAPI_SetFileAttr(char *, char);
int WAPI_GetDiskFree(char *, double *, double *);
int op_dir(void);

int WAPI_ShortPath(char *shortpath)
{
    int i;

    i=GetShortPathName(shortpath,shortpath,strlen(shortpath));
    return i;
}

int WAPI_SetFileAttr(char *filename, char attrib)
{
    int i;

    i=SetFileAttributes(filename, attrib);
    return i;
}

int WAPI_GetDiskFree(char *root, double *total, double *avail)
{
    int i;
    DWORD SectorsPerCluster, BytesPerSector,
          NumberOfFreeClusters, TotalNumberOfClusters;
    char *apicall[2];

    if (!strcmp(os_ver,"WIN95")) {
        i=GetDiskFreeSpace(root, &SectorsPerCluster,
                                 &BytesPerSector,
                                 &NumberOfFreeClusters,
                                 &TotalNumberOfClusters);

        *total=(double)(TotalNumberOfClusters*SectorsPerCluster*BytesPerSector);
        *avail=(double) (NumberOfFreeClusters*SectorsPerCluster*BytesPerSector);
    } else {
        sprintf(info,"&DESKAPI 1 %s",root); /* (not through edit field!) */
        i=suorita("&DESKAPI.EXE"); if (i<0) return -1;
        muste_kv_space_split(info,apicall,2);
        *total=atof(apicall[0]);
        *avail=atof(apicall[1]);
    }
    return i;
}

int op_dir(void) // K.Vehkalahti 2000 (siirretty editorista 14.9.2000)
{
    indexdir=1; /* 29.1.2001 */
    no_cd=1; /* 29.1.2001 */
    INDEXmain();
    return 1;
}

// SHOW GEN1 222 / DiskFree ("&DESKAPI.EXE") - jätän tästä pois.



#endif




static void init_prompt(void)
{
    int i;
    for (i=0; i<LNAME; i++) answer[i]='\0';
    write_string(space,ScreenWidth,BackGround,BOTTOMROW,1);
    write_string(space,80,' ',BOTTOMROW,1);
    LOCATE(BOTTOMROW,1);
}

static void clear_screen(void)
{
    int i;
    LOCATE(BOTTOMROW,1);
    for (i=WORKROW; i<=BOTTOMROW; i++)
        write_string(space, ScreenWidth, BackGround, i, 1);
}

static int suorita(char *prog)
{
    int val; /* for testing DOS/32A 17.12.1997 */
    char moduli[LNAME];

    strcpy(moduli,survo_path); strcat(moduli,prog);
    enable_softkeys(); s_end(siirtop);
    val=spawnl(P_WAIT,moduli,moduli,siirtop,"2","3",NULL);
    s_init(siirtop); // 28.8.2000 !! (SHOW:n Load lines alkoi toimia!)
    disable_softkeys();
    if (val<0)
        disp_err(" Error in spawning %s!", moduli);
    return val;
}

static void write_cmd_line(void)
{
    edwrite(space,r1,1);
    edwrite(answer,r1,1);
}

static void show_yesno(char *str)
{
    write_string("(Yes/No)",8,Advice,BOTTOMROW,strlen(str)+4);
}

static void mark_files_unsorted(void)
{
    int i;
    for (i=0,f=&files[0]; i<GV.filecount; i++,f++)
       f->status=(f->status & ~FSORTED);
}

static int ask_yesno(char *str, char *def)
{
    char yesno[2];

    strcpy(yesno,def);
    prompt(str,yesno,1);
    muste_strupr(yesno);
    WhiteBottomRow;
    switch (yesno[0]) {
      case 'Y': return 1;
       default: return -1;
    }
}

static int init_globals(void)
{
    int j;

    for (j=0; j<GRPOPTLEN; j++) GV.grouping[j]='\0';
    for (j=0; j<SORTOPTLEN; j++) GV.sorting[j]='\0';

    j=spfind("GROUPING");
    if (j>=0) {
        if (strlen(spb[j])>=GRPOPTLEN) spb[j][GRPOPTLEN-1]='\0';
        strcpy(GV.grouping, spb[j]);
    }
    j=spfind("SORT");
    if (j>=0) {
        if (strlen(spb[j])<SORTOPTLEN) {
            strcpy(GV.sorting, spb[j]);
        }
    }
    GV.print_size=1; /* 21.7.1998 */
    j=spfind("SIZE");
    if (j>=0) GV.print_size=atoi(spb[j]);

    GV.required += count_date_time_length();
    return 1;
}

static void system_call(void)
{
    init_prompt();
    prompt(" System command ? ",answer,LNA64-5);
    if (!strlen(answer)) return;
    system(answer); // MUUTA WIN API -funktioksi! (miksi?)
    PR_EINV; WAIT;
}

static void f_sort(void)
{
    init_prompt();
    WhiteBottomRow;
    write_string(
     "(One of: DATE,TYPE,SIZE,NAME,TIME,OS. Descending: e.g. -DATE.)",
      62,Advice,BOTTOMROW,14);
    strcpy(answer,GV.sorting);
    prompt(" SORT=",answer,SORTOPTLEN-1);
    muste_strupr(answer);
    if (!strcmp(answer,GV.sorting)) return; /* no changes */
    strcpy(GV.sorting,answer);
}

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

static void f_group(void)
{
    init_prompt();
    strcpy(answer,GV.grouping);
    prompt(" GROUPING=",answer,min((GRPOPTLEN-1),80-10-2));
    muste_strupr(answer);
    if (!strcmp(answer,GV.grouping)) return; /* no changes */
    strcpy(GV.grouping,answer);
}

static void flash_counter(int count) /* 6.6.2000 */
{
    sprintf(sbuf,"     %6d",count); /* 5 -> 6 (6.3.2005) */
    write_string(sbuf,11,'/',1,(c3+8-1)-11);
}


static int comp1(const void *, const void *);
static int comp1b(const void *, const void *);
static int comp2(const void *, const void *);
static int comp2b(const void *, const void *);
static int comp3(const void *, const void *);
static int comp3b(const void *, const void *);
static int comp4(const void *, const void *);
static int comp4b(const void *, const void *);
static int comp5(const void *, const void *);
static int comp5b(const void *, const void *);
static int comp5c(const void *, const void *);
static int comp5d(const void *, const void *);
static int comp6(const void *, const void *);
static int comp7(const void *, const void *);
static int comp8(const void *, const void *);
static int comp9(const void *, const void *); /* 17.1.1999 */
static int comp9b(const void *, const void *); /* 17.1.1999 */
static int comp10(const void *, const void *); /* 21.8.2002 */
static int comp10b(const void *, const void *); /* 21.8.2002 */

enum commands { DIRMOVE, SHOW, FSHOW, /*GPLOT,*/
                ZIPVIEW, LZHVIEW, MATSHOW, TUTSHOW,
                OP_SPX, OP_WPX, OP_EMF              /* 13.9.2000 */
              };

static void get_edt_comments(char *, int);
static void get_svo_comments(char *, int);
static void get_mat_comments(char *, int);
//static void get_spx_comments(char *, int);
//static void get_wpx_comments(char *, int);
static void get_emf_comments(char *, int);

static FILE *fh;
static int numread;

static int comp1 (const void *val1, const void *val2) /* OS order, dirs first (default) */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    if (nr1->order <  nr2->order) return -1;
    if (nr1->order == nr2->order) return  0;
    if (nr1->order >  nr2->order) return 1;
    return 1;
}

static int comp1b (const void *val1, const void *val2) /* -OS order */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return ((-1)*comp1(nr1, nr2));
}

static int comp2 (const void *val1, const void *val2) /* date & time */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    if (nr1->year > nr2->year) return 1;
    if (nr1->year < nr2->year) return -1;
    if (nr1->month > nr2->month) return 1;
    if (nr1->month < nr2->month) return -1;
    if (nr1->day > nr2->day) return 1;
    if (nr1->day < nr2->day) return -1;
    if (nr1->hour > nr2->hour) return 1;
    if (nr1->hour < nr2->hour) return -1;
    if (nr1->minute > nr2->minute) return 1;
    if (nr1->minute < nr2->minute) return -1;
#if 0
(sekunnit turhia; NTFS/Win2000 ainakin herkempi kuin aiemmat!) 25.8.2000
    if (nr1->second > nr2->second) return 1;
    if (nr1->second < nr2->second) return -1;
#endif
    return 0; /* exactly same! */
}

static int comp2b (const void *val1, const void *val2) /* -date & -time */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return ((-1)*comp2(nr1, nr2));
}

static int comp3 (const void *val1, const void *val2) /* size */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    if (nr1->size <  nr2->size) return -1;
    if (nr1->size == nr2->size) return 0;
    if (nr1->size >  nr2->size) return 1;
    return 1;
}

static int comp3b (const void *val1, const void *val2) /* -size */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return ((-1)*comp3(nr1, nr2));
}

static int comp4 (const void *val1, const void *val2) /* name */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return(strncmp(nr1->name, nr2->name, strlen(nr1->name)));
}

static int comp4b (const void *val1, const void *val2) /* -name */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return ((-1)*comp4(nr1, nr2));
}

static int comp5 (const void *val1, const void *val2) /* type */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    char *p1, *p2; // 30.12.2000 (bug found by SM)
    p1=strstr(nr1->name,".");
    p2=strstr(nr2->name,".");
         if (p1==NULL && p2==NULL) return 0;
    else if (p1==NULL) return -1;
    else if (p2==NULL) return  1;

    return (strcmp(p1,p2));
}

static int comp5b (const void *val1, const void *val2) /* -type */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return ((-1)*comp5(nr1, nr2));
}

static int comp5c (const void *val1, const void *val2) /* type (INDEX)  */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return(strncmp(nr1->type, nr2->type, strlen(nr1->type)));
}

static int comp5d (const void *val1, const void *val2) /* -type (INDEX) */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return ((-1)*comp5c(nr1, nr2));
}

static int comp6 (const void *val1, const void *val2) /* group order (final order) */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    if (nr1->grouporder <  nr2->grouporder) return -1;
    if (nr1->grouporder == nr2->grouporder) return  0;
    if (nr1->grouporder >  nr2->grouporder) return  1;
    return 1;
}

static int comp7 (const void *val1, const void *val2) /* deleted files to the bottom */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
#define nr1Deleted ((nr1->status & FDELETED) == FDELETED)
#define nr2Deleted ((nr2->status & FDELETED) == FDELETED)
    if (!nr1Deleted &&  nr2Deleted) return -1;
    if ( nr1Deleted && !nr2Deleted) return  1;
    return 0;
}

static int comp8 (const void *val1, const void *val2) /* non-matching files to the bottom */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
#define nr1Match (nr1->next!=NULL && !FileNotFile(nr1))
#define nr2Match (nr2->next!=NULL && !FileNotFile(nr2))
    if ( nr1Match && !nr2Match) return -1;
    if (!nr1Match &&  nr2Match) return  1;
    return 0;
}

static int comp9 (const void *val1, const void *val2) /* non-marked files to the bottom 17.1.1999 */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
#define nr1Marked ((nr1->status & FMARKED) == FMARKED)
#define nr2Marked ((nr2->status & FMARKED) == FMARKED)
    if ( nr1Marked && !nr2Marked) return -1;
    if (!nr1Marked &&  nr2Marked) return  1;
    return 0;
}

static int comp9b (const void *val1, const void *val2) /* -mark */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return ((-1)*comp9(nr1, nr2));
}

static int comp10 (const void *val1, const void *val2) /* comment (DD, DM) */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return(strncmp(nr1->comment, nr2->comment, strlen(nr1->comment)));
}

static int comp10b (const void *val1, const void *val2) /* -comment (DD, DM) */
{
    const Files *nr1=(const Files *)val1;
    const Files *nr2=(const Files *)val2;
    return ((-1)*comp10(nr1, nr2));
}

static void get_edt_comments(char *str, int len)
{
    int cols, i,j,k;
    char ch='\0';
    int survo98;
    char *xx;

    k=0; survo98=0;
    if ((fh=fopen(tmp,"rb"))!=NULL) {
        k=1;
        numread=fread((void *)sbuf,sizeof(char),(size_t)14,fh);
        if (numread<14) k=0; else sbuf[14]='\0';
        if ( k && strncmp(sbuf, "SURVO84ED",9)) k=0;
        if (!k && !strncmp(sbuf, "SURVO 98 edit",13)) survo98=1;
        if (k) {
            sscanf(sbuf, "%s %d", tmp, &cols);
            fseek(fh, (long)cols+1, SEEK_SET);
            numread=fread((void *)sbuf,sizeof(char),(size_t)cols-1,fh);
            if (numread<cols-1) k=0; else sbuf[cols]='\0';
            if (k && muste_strnicmp(sbuf,"SAVE ",5)) k=0;
        }
    } else { return; }
    if (!k) { fclose(fh); if (!survo98) return; }
    if (survo98) {
        fh=fopen(tmp,"r"); if (fh==NULL) return;
        fgets(sbuf,LLENGTH-1,fh); /* ID line */
        fgets(sbuf,LLENGTH-1,fh);
        sscanf(sbuf, "%d", &cols); /* line number */
        if (cols!=1) { fclose(fh); return; } /* should be 1st line */
        xx=strchr(sbuf, '|');
        if (xx!=NULL) {
            xx+=2; /* skip | and control char */
            while (*xx==' ' && *xx!='\0') xx++; /* skip possible white space */
            if (muste_strnicmp(xx,"SAVE ",5)==0) {
                sbuf[strlen(sbuf)-1]='\0';
                xx=strstr(sbuf," / ");
                if (xx!=NULL) {
                    xx+=3;
                    strcpy(str,xx);
                }
            }
        }
    } else {
        i=5;
        while ((ch!='/')&&(i<cols-5)) ch=sbuf[i++];
        if (ch=='/') {
            ++i;
            k=min(len,(cols-i-1));
            for (j=0; j<k; ++i,j++) str[j]=sbuf[i];
            str[j]='\0';
        }
    }
    fclose(fh);
}

static void get_svo_comments(char *str, int len)
{
    int i,j,k;
    short textn,textlen;
    long text;

    k=0;
    if ((fh=fopen(tmp,"rb"))!=NULL) {
        k=1;
        numread=fread((void *)sbuf,sizeof(char),(size_t)LNAME-1,fh);
        if (numread<LNAME-1) k=0;
        if (k && strncmp(sbuf,"SURVO 84C DATA",14)) k=0;
    }
    if (!k) { fclose(fh); fi->command=SHOW; return; }

    fseek(fh, (long)30, SEEK_SET);
    fread((void *)&textn, sizeof(char), (size_t)2, fh);
    fseek(fh, (long)32, SEEK_SET);
    fread((void *)&textlen, sizeof(char), (size_t)2, fh);
    fseek(fh, (long)34, SEEK_SET);
    fread((void *)&text, sizeof(char), (size_t)4, fh);
    fseek(fh, text, SEEK_SET);
    strcpy(str, "");
    for (i=0,j=0; i<textn && j<len; i++) {
        for (k=0; k<LLENGTH; k++) sbuf[k]='\0';
        fread((void *)sbuf, sizeof(char), (size_t)textlen, fh);
        strcpy(line,sbuf);
        trim(sbuf,line);
        if (strlen(sbuf)==0) continue;
        for (k=0; k<strlen(sbuf) && j<len; j++,k++) str[j]=sbuf[k];
        if (j>0) str[j]=';'; j++;
    }
    if (j>0) str[j-1]='\0';
    fclose(fh);
}

static void get_mat_comments(char *str, int len)
{
    const int ERC=128;
    int i,j,k,m,n,nrem,lr,lc,type;

    k=0;
    if ((fh=fopen(tmp,"rb"))!=NULL) {
        k=1;
        numread=fread((void *)sbuf, sizeof(char), (size_t)ERC, fh);
        if (numread<ERC) k=0;
        sbuf[ERC]='\0';
        if (k && strncmp(sbuf,"MATRIX84D",9)) k=0;
    }
    if (!k) { /*fclose(fh);*/ fi->command=SHOW; return; }

    sscanf(sbuf, "%s %d %d %d %d %d %d",tmp,&m,&n,&nrem,&lr,&lc,&type);
    fseek(fh, (long)ERC, SEEK_SET);
    for (i=0, k=0; i<nrem && k<len; i++) {
        if (k<len-2 && k>0) {
            str[--k]='\0';
            strcat(str, "; ");
            k+=2;
        }
        numread=fread((void *)sbuf, sizeof(char), (size_t)ERC-1, fh);
        if (!strncmp(sbuf, space, ERC)) continue;
        for (j=0; j<ERC && k<len; j++) {
            str[k]=sbuf[j];
            if (str[k]!=' ') k++;
        }
        str[k]='\0';
    }
    fclose(fh);
}


#if 0
(Muste: turha!)

#include <fcntl.h>
static void get_spx_comments(char *str, int len) /* and WPX */
{   /* size of the graph added to full pictures also 24.5.1999 */
#define SPX_CONSTANT -32091

    int handle, partial;
    short videomode, x_size, y_size;
    int survo;

    survo=00;
         if (strstr(tmp,".SPX")) survo=84;
    else if (strstr(tmp,".WPX")) survo=98;

    partial=0;
    handle=open(tmp,O_BINARY);
    if (handle<0) { fi->command=SHOW; return; }
    read(handle,(char *)&videomode,2);
    if (videomode==SPX_CONSTANT) {
        partial=1;
        read(handle,(char *)&videomode,2);
        read(handle,(char *)&x_size,2);
        read(handle,(char *)&y_size,2);
    }
    close(handle);
    strcpy(str,"MODE=");
    switch (videomode) {
      case  13: strcat(str,"CGA");
                if (partial) break;
                x_size=320; y_size=200;
                break;
      case  16: strcat(str,"EGA");
                if (partial) break;
                x_size=640; y_size=350;
                break;
      case  18: strcat(str,"VGA");
                if (partial) break;
                x_size=640; y_size=480;
                break;
      case 259: strcat(str,"SVGA");
                if (partial) break;
                x_size=800; y_size=600;
                break;
      case 261: strcat(str,"XRES");
                if (partial) break;
                x_size=1024; y_size=768;
                break;
       default: str[0]='\0';
               fi->command=SHOW;
               break;
    }
    sprintf(tmp, " (%dx%d)", x_size, y_size);
    strcat(str, tmp);
    if (survo==84) strcat(str, " [SURVO 84C graphics]");
    else if (survo==98) strcat(str, " [SURVO 98 graphics]");
}

#endif



static void get_emf_comments(char *str, int len) // 24.8.2000
{
    strcpy(str, "SURVO MM Graphics (Enhanced Meta File)");
}



// END DESKTOP /////////////////////////////////////////////////////////////////





// BEGIN INDEX ////////////////////////////////////////////////////////////////////

static int INDEXcheck_parameters(void);
static int INDEXcheck_disk(char *);
static int INDEXchange_disk(void);
static void INDEXsort_files(int count);
static int check_grouping(void);
static int count_files(void);
static int find_files(void);
static void copy_info(void);
static void separate_files(void);
static void add_command(void);
static int specifics(void);
static void INDEXprintout(void);
static void INDEXget_comments(void);
static int  print_line(void);
static void make_line(void);
static void print_empty_line(void);
static void INDEXmake_date_and_time(void);

#define MAXWIDTH LLENGTH-4

static int bigg, lowercase, uppercase, found_some, width, list_to_field, printcount,
           comments_to_left, the_rest, bare_format, stats_format, full_format;
static char comment_str[LLENGTH], bytes[LNA64/2], outfile[LNAME],
            size_str[LNAME/2], datetime_str[LNAME/2],
            commfile_str[LNAME/2], commfile_tmp[LNAME/2];
static FILE *output_file, *fh;

#define FMatch    (!strcmp(fi->type, typ))
#define OScomm   (!strncmp(fi->cmd, ">", 1))
#define SUCROcomm (!strncmp(fi->cmd, "/", 1))

static struct tm *write_time;

static int restrictions(void); /* 22.7.1998 */
static void no_valid_files(void);

static int display_mode;      /* display during execution on/off */
static int print_filetype;    /* print the type or not TYPES=1/0 */

/* the following 4 items were moved from OUTPUT.C 8.3.96 so that
   _only_ GROUPING-files will be processed, not just only printed! */
static int groups;            /* group count */
static int only_grouptypes;   /* process files which match GROUPING */
static char typ[];            /* current file type used in comparing */
static char *group_types[];   /* file types from GROUPING */

static Types *ty, types[]= {
      { "DIR", "CD",            0, 1 },
      { "EDT", "SHOW",          1, 1 },
      { "SVO", "FILE_SHOW",     1, 1 },
      { "MAT", "/MATSHOW",      1, 1 },
      { "M"  , "/MATSHOW",      1, 0 },
      { "TUT", "/TUTSHOW",      0, 1 },
      { "SPX", "SHOW",          1, 0 },
      { "BIN", "/CODESHOW",     0, 1 },
      { "TCH", "/TCHSHOW",      0, 1 },
      { "COM", ">",             0, 0 },
      { "EXE", ">",             0, 0 },
      { "ZIP", ">UNZIP_-lv",    0, 1 },
      { "LZH", ">LHA_l",        0, 1 }, /* poistettu helpistä */
      { "PS",  "/GV-SHOW",      0, 0 },
      { "EPS", "/GV-SHOW",      0, 0 },
      { "WPX", "SHOW",          1, 0 },
      { "EMF", "GPLOT_FILE",    1, 1 },
      { "PDF", "/OPEN",         0, 0 }, // 3.3.2003 (030303)
      { "DOC", "/OPEN",         0, 0 },
      { "XLS", "/OPEN",         0, 0 },
      { "PPT", "/OPEN",         0, 0 },
      { "JPG", "/OPEN",         0, 0 },
      { "JPEG", "/OPEN",        0, 0 },
      { "GIF", "/OPEN",         0, 0 },
      { "HTM", "/OPEN",         0, 0 },
      { "HTML", "/OPEN",        0, 0 },
      { "PNG", "/OPEN",         0, 0 },
      { "RTF", "/OPEN",         0, 0 },
      { "TIF", "/OPEN",         0, 0 },
      { "TIFF", "/OPEN",        0, 0 }   // 13.5.2008: älä enää lisää: /OPEN kelpaa mihin vain!
    };

static int TypeCount=(sizeof(types)/sizeof(Types)+1);
static int display_mode, print_filetype, groups, only_grouptypes;
static char typ[TYPELEN];
static char *group_types[GRPTYPS];

static int INDEXmain(void)
{
    int i;
    char caller_path[LNAME]; /* 29.1.2001 */

    strcpy(caller_path, edisk); /* save current datapath */
    i=INDEXcheck_parameters(); if (i<0) return -1;
    i=check_grouping(); if (i<0) return -1;
    count_date_time_length(); /* sets GV.print_date, 30.10.1998 */
    GV.filecount=count_files(); if (GV.filecount<0) return -1;
    if (GV.filecount==0) {
        if (etu==2) { /* 28.8.2000 */
            tut_error(1);
        } else {
            if (only_grouptypes) {
                muste_kv_s_disp("\nNo files found from %s (", GV.filespec);
                for (i=0; i<groups; i++) {
                    if (!i) muste_kv_s_disp("*.%s",group_types[i]);
                    else muste_kv_s_disp(",*.%s",group_types[i]);
                }
                muste_kv_s_disp(")!"); WAIT;
            } else {
                muste_kv_s_err("No files found (%s)!", GV.filespec);
            }
        }
        return -1;
    }
    files=(Files *)malloc((size_t)GV.filecount*sizeof(Files));
    if (files==NULL) { no_mem(); return -1; }
    i=find_files();
    INDEXsort_files(GV.filecount);
    INDEXprintout();
    if (no_cd) strcpy(edisk, caller_path); /* restore datapath */
    free(files);
    return 1;
}

static int check_grouping(void)
{
    int j;

    groups=0; /* default: no grouping */
    strcpy(typ, "..."); /* can never be a real file type  [in FAT..] */
    j=spfind("GROUPING");
    if (j>=0) groups=split(spb[j], group_types, GRPTYPS);

    only_grouptypes=0; /* default: print everything */
    j=spfind("ONLY"); /* 14.7.95/kv (SM) */
    if (j>=0) {
        if (atoi(spb[j])==1) {
            only_grouptypes=1;
            if (groups==0) {
                muste_kv_s_err("Use ONLY with GROUPING! (See INDEX?)");
                return -1;
            }
        }
    }
    return 1;
}

static int INDEXcheck_parameters(void)
{
    int i;

    if (g>1) { /* at least one parameter given */
        if (!strcmp(word[1],"?")) {
            muste_kv_usage_info();
            return -1; /* do not continue further */
        }




#if 0

        if (!strcmp(word[1],"/SHORTNAME")) { /* 29.9.2000 */
            INDEXshortname();
            return -1;
        }
        if (!strcmp(word[1],"/LONGNAME")) { /* 29.9.2000 */
            INDEXlongname();
            return -1;
        }
        if (!strcmp(word[1],"/PRINTERS")) { /* 23.12.2000 */ // (9.1.2002)
            INDEXprinters();
            return -1;
        }
        if (!strcmp(word[1],"/WINDOWS")) { // (15.2.2002)
            INDEXwindows();
            return -1;
        }
#endif



        if (g>2) { /* two parameters */
            results_line=edline2(word[2],1,0);
            if (!results_line) { /* invalid line given */
                muste_kv_s_err("Invalid edit line %s given!", word[2]);
                return -1;
            }
            i=INDEXcheck_disk(word[1]);
            if (i<0) return -1;
        } else { /* only one parameter, may be either. check line 1st */
            results_line=edline2(word[1],1,0);
            sprintf(GV.filespec, "%s%s", edisk, "*.*"); /* old default */
            if (!results_line) { /* not a line - maybe a path */
                results_line = r1+r-1+1; /* default: CUR+1 */
                i=INDEXcheck_disk(word[1]);
                if (i<0) return -1;
            }
        }
    } else { /* no parameters given */
        if (indexdir) {
            muste_kv_s_err("Usage: DIR <pathname>"); /* like in SURVO 98 & 84C */
            return -1;
        }
        results_line = r1+r-1+1; /* default: CUR+1 */
        sprintf(GV.filespec, "%s%s", edisk, "*.*"); /* old default */
        i=INDEXcheck_disk(GV.filespec);
        if (i<0) return -1;
    }
    return 1;
}

static int INDEXcheck_disk(char *argum)
{
    int i, wild, point;

    strcpy(tmp, argum);
    if (!case_sensitive_pathnames) muste_strupr(tmp);
    wild=0; point=0;
    if (strchr(tmp, '*') != NULL) wild=1;
    if (strchr(tmp, '.') != NULL) point=1;
    if (strchr(tmp, ':') != NULL) { /* drive given */
        if (strlen(tmp)==2) strcat(tmp, "*.*\0"); /* only drive given */
        strcpy(fullspec, tmp);
    } else {
        if (wild && !point) strcat(tmp, ".*\0");
        if (netd(tmp)) { // 25.2.2006 (for netsurvo/SM) survo.lib updated
            strcpy(fullspec, tmp); // ehdollista vielä tarvittaessa vain DIRille!
        } else {
            if (tmp[0]!='\\') sprintf(fullspec, "%s%s", edisk, tmp);
            else sprintf(fullspec, "%c:%s", edisk[0], tmp);
        }
    }
    if (_fullpath(path, fullspec, LNAME-1) == NULL) {
        muste_kv_s_err("Bad path! (%s)", argum);
        return -1;
    }
    _splitpath(path, drive, dir, fname, ext);
//printf("\nsplit: path=%s drive=%s dir=%s fname=%s ext=%s",path, drive, dir, fname, ext);
//sur_getch();
    strcpy(fullspec, path);
    if (!strlen(ext)) {
        if (!strlen(fname)) { /* no file name - main directory */
            strcat(fullspec, "*.*");
        } else {
            strcat(fullspec, "\\*.*\0");
            strcat(fname, "\\");
            strcat(dir, fname);
        }
        h_fil=_findfirst(fullspec, &fil);
        if (h_fil == -1L) {
            h_fil=_findfirst(path, &fil);
            if (h_fil == -1L) {
                if (etu==2) { /* 28.8.2000 */
                    tut_error(1);
                } else {
                    muste_kv_s_err("No files found (%s)!", path);
                }
                return -1;
            } else {
                _findclose(h_fil);
            }
          } else {
              _findclose(h_fil);
              strcpy(path, fullspec); /* fixed fullspec worked; use it */
          }
    } else { /* path includes all parts, also extension */
        h_fil=_findfirst(path, &fil);
        if (h_fil == -1L) {
            if (etu==2) { /* 28.8.2000 */
                tut_error(1);
            } else {
                muste_kv_s_err("No files found (%s)!", path);
            }
            return -1;
        } else {
            _findclose(h_fil);
        }
    }
    strcpy(GV.filespec, path);
    i=INDEXchange_disk(); if (i<0) return -1;
    return 1;
}

static int INDEXchange_disk(void)
{
    unsigned int len;

    sprintf(tmp, "%s%s", drive, dir);
    len=strlen(tmp);
    if ((tmp[len-1]=='\\') && (len>2+2)) tmp[len-1]='\0';
    if (_chdir(tmp)) {
        muste_kv_s_err("Can't change to directory %s!", tmp);
        return -1;
    }
    sprintf(edisk, "%s%s", drive, dir);
    return 1;
}

static int INDEXshortname(void) // palauttaa pitkän tiedostonimen lyhyen vastineen (28.9.2000)
{
#if 0
    int i;          // INDEX /SHORTNAME C:\Program Files\Administrator\huuhaa haahuu

    strcpy(line,"");
    for (i=2; i<g; i++) {
        strcat(line, word[i]);
        strcat(line, " ");
    }
    strcpy(sbuf, line);
    WAPI_ShortPath(sbuf);
    edwrite(space,r1+r,1); /* always write an empty line for sucro checkings etc */
    if (!strcmp(sbuf,line)) return -1; /* not found or already in short form */
    edwrite(sbuf,r1+r,1);
#endif
    return 1;
}

static int INDEXlongname(void) // palauttaa lyhyen tiedostonimen pitkän vastineen (29.9.2000)
{
#if 0
    int i;          // INDEX /LONGNAME C:\PROGRA~1\ADMINI~1\HUUHAA~1
    char *token;
    int col=1;

    edwrite(space,r1+r,1); /* always write an empty line for sucro checkings etc */
    if (g<3) return -1;
    strcpy(line,word[2]);
    h_fil=_findfirst(line, &fil);
    if (h_fil == -1L) return -1; /* file was not found */
    _findclose(h_fil);
    if (word[2][1]!=':') { /* no full path, just a single file name given */
        strcpy(sbuf, fil.name);
        edwrite(sbuf,r1+r,1);
        return 1;
    }
    /* path includes colon, let us parse the whole path (a ready made routine
       does not exist until NT5- / 98- */
    /* we assume drives to be traditional, like C: or R: */
    token=strtok(line,"\\");
    sprintf(tmp, "%s\\", token);
    edwrite(tmp, r1+r, col); col+=3;
    token=strtok(NULL,"\\");
    while (token!=NULL) {
        strcat(tmp, token);
        h_fil=_findfirst(tmp, &fil);
        _findclose(h_fil);
        strcpy(sbuf, fil.name);
        edwrite(sbuf,r1+r,col); col+=strlen(sbuf);
        strcat(tmp,"\\");
        token=strtok(NULL,"\\");
        if (token!=NULL) { edwrite("\\",r1+r,col); col++; }
    }
#endif
    return 1;
}





#if 0
Muste: toistaiseksi suljettu nämä kokonaan pois



#undef DELETE
#include <windows.h>

static int INDEXprinters(void) // palauttaa listan kirjoittimista (23.12.2000) (9.1.2002)
{
    int i;
    DWORD bytes, count;
    LPPRINTER_INFO_2 prinfo=NULL;

    if (g<3) {
        muste_kv_s_err("INDEX /PRINTERS,L  writes information of local printers on line L onwards.");
        return -1;
    }
    results_line=edline2(word[2],1,0);
    if (!results_line) {
        muste_kv_s_err("Invalid edit line %s given!", word[2]);
        return -1;
    }

    EnumPrinters (PRINTER_ENUM_LOCAL, NULL, 2, NULL, 0, &bytes, &count);
    prinfo=(LPPRINTER_INFO_2)HeapAlloc(GetProcessHeap(), HEAP_ZERO_MEMORY, bytes);
    if (prinfo==NULL) { no_mem(); return -1; }

    i=EnumPrinters(PRINTER_ENUM_LOCAL, NULL, 2, (LPBYTE)prinfo, bytes, &bytes, &count);
    if (i==0) {
        muste_kv_s_err("Error with EnumPrinters: %d", GetLastError());
        return -1;
    }

    for (i=0; i<count; i++) {
        sprintf(sbuf, "Name: %s" , prinfo[i].pPrinterName);
        edwrite(space,results_line,1); edwrite(sbuf, results_line++,1);
#if 0
        if (prinfo[i].Attributes & PRINTER_ATTRIBUTE_SHARED) {
            sprintf(sbuf, "Shared name:  %s" , prinfo[i].pShareName);
            edwrite(space,results_line,1); edwrite(sbuf, results_line++,1);
        }
#endif
        sprintf(sbuf, "Port: %s" , prinfo[i].pPortName);
        edwrite(space,results_line,1); edwrite(sbuf, results_line++,1);
#if 0
        sprintf(sbuf, "Driver:       %s" , prinfo[i].pDriverName);
        edwrite(space,results_line,1); edwrite(sbuf, results_line++,1);
        sprintf(sbuf, "Comment:      %s" , prinfo[i].pComment);
        edwrite(space,results_line,1); edwrite(sbuf, results_line++,1);
        sprintf(sbuf, "Location:     %s" , prinfo[i].pLocation);
        edwrite(space,results_line,1); edwrite(sbuf, results_line++,1);
#endif

        edwrite(space,results_line++,1);
    }
    HeapFree(GetProcessHeap(), 0, prinfo);

    return 1;
}

static int INDEXwindows(void) // palauttaa Windows-hakemiston nimen (15.2.2002)
{
    int i;

    if (g<3) {
        muste_kv_s_err("INDEX /WINDOWS,L  writes the Windows system path on line L.");
        return -1;
    }
    results_line=edline2(word[2],1,0);
    if (!results_line) {
        muste_kv_s_err("Invalid edit line %s given!", word[2]);
        return -1;
    }
    i=GetWindowsDirectory(sbuf,LLENGTH);
    if (i==0) {
        muste_kv_s_err("Error with GetWindowsDirectory: %d", GetLastError());
        return -1;
    }
    edwrite(space,results_line,1); edwrite(sbuf, results_line++,1);

    return 1;
}

#endif




static int count_files(void)
{
    int j,count;
    char *xx;

    count=0;
    display_mode=1;
    if (indexdir) display_mode=0;
    j=spfind("DISPLAY"); /* check the display specification */
    if (j>=0) {
        if (!strcmp(spb[j], "OFF")) display_mode=0;
    } else {
        j=spfind("PRIND");
        if (j>=0) {
            display_mode=atoi(spb[j]);
        } else {
            if (hae_apu("prind",sbuf)) display_mode=atoi(sbuf);
        }
    }
    print_filetype=1; /* old default: print all file types */
    j=spfind("TYPES");
    if (j>=0) print_filetype=atoi(spb[j]);
    if (display_mode) muste_kv_s_disp("\nIndex of %s :",GV.filespec);
    h_fil=_findfirst(GV.filespec, &fil);
    if (h_fil != -1L) {
      do {
          j=restrictions(); if (j<0) return -1;     /* 22.7.1998 */
          if (j) continue;  /* restriction found, do not count file */
          if (only_grouptypes) { /* this part added 8.3.96 */
               /* -> rule changed: don't print dirs if only is on */
              if (SUBDIR(fil)) continue;
              for (j=0; j<groups; j++) {
                  sprintf(sbuf,".%s",group_types[j]);
// printf("\nsbuf=|%s|, fil.name=|%s|\n",sbuf,fil.name); sur_getch();
                  strcpy(path,fil.name); muste_strupr(path);  // 7.3.2006 (!)
                  if ((xx=strstr(path,sbuf))!=NULL)      // 7.3.2006 (!)
                      if (strlen(xx)==strlen(sbuf)) break;
              } if (j==groups) continue; /* skip this file! */
          }
          count++;
      } while(!_findnext(h_fil, &fil));
      _findclose(h_fil);
    }
    if (count==0) {
        if (etu==2) { /* 28.8.2000 */
            tut_error(1);
        } else {
            no_valid_files();
        }
        return -1;
    }
    return count;
}

static int find_files(void)
{
    int i,j;
    char *xx;

    i=0; GV.dircount=0;
    GV.bigglen=0; GV.bytes=0L;
    fi=&files[0];
    if (display_mode) muste_kv_s_disp("\nreading file information...\n");
    h_fil=_findfirst(GV.filespec, &fil);
    do { /* at least one is found */
        j=restrictions(); if (j<0) return -1;     /* 22.7.1998 */
        if (j) continue;  /* restriction found, do not count file */
        if (only_grouptypes) { /* this part added 8.3.96 */
            if (SUBDIR(fil)) continue;
            for (j=0; j<groups; j++) {
                sprintf(sbuf,".%s",group_types[j]);
                strcpy(path,fil.name); muste_strupr(path);  // 7.3.2006 (!)
                if ((xx=strstr(path,sbuf))!=NULL)      // 7.3.2006 (!)
                    if (strlen(xx)==strlen(sbuf)) break;
            } if (j==groups) continue; /* skip this file! */
        }
        copy_info();
        i++;
        if (display_mode) muste_kv_s_disp(" %d",i);
    } while(!_findnext(h_fil, &fil));
    _findclose(h_fil);
    return 1;
}

static void copy_info(void)
{
    if (SUBDIR(fil)) GV.dircount++;
    fi->attrib = fil.attrib;
    fi->size = fil.size;

    strcpy(fi->name, fil.name); /* fil.name: (possibly) a long name (20.8.2001)*/
//  WAPI_ShortPath(fi->name);  /* fi->name: short form of the name (20.8.2001)*/

    write_time = localtime(&fil.time_write);
    if (write_time==NULL) { /* 19.12.2000 (alihakemistot cd:illä!) */
        fi->year   = 0;
        fi->month  = 0;
        fi->day    = 0;
        fi->hour   = 0;
        fi->minute = 0;
        fi->second = 0;
    } else {
        fi->year   = write_time->tm_year; /* current year - 1900 */
        fi->month  = write_time->tm_mon+1;
        fi->day    = write_time->tm_mday;
        fi->hour   = write_time->tm_hour;
        fi->minute = write_time->tm_min;
        fi->second = write_time->tm_sec;
    }
    fi->printed = 0; /* nothing is printed yet! */
    strcpy(fi->cmd, "SHOW"); /* default for unknown types */
    fi->get_comments = 0; /* default, no comments for this file */
    fi->notype = 0; /* default, type will be printed */
    strcpy(fi->comment, fil.name); /* 28.9.2000 for preserving case in long names (20.8.2001) */
    if (!case_sensitive_pathnames) muste_strupr(fi->name);
    separate_files();
    fi++;
}

static void separate_files(void)
{
    char name[NAMELEN], type[TYPELEN];
    char *ploc, *p;
    int i,pl;
    static unsigned int order_nr=1;

    if (SubDirectory(fi)) { /* directory? */
      if ( !(strncmp(fi->name, ".",1)) ) { /* this or upper? */
        if ( !(strncmp(fi->name, "..",2)) ) { /* upper? */
          i=strlen(edisk);
          if (i>3) { /* 28.8.2000 (my own D-disk which is SUBSTed...) */
              strncpy(sbuf, edisk, i-1); /* remove last '\' */
              ploc = strrchr(sbuf, '\\'); /* always present */
              pl = ploc - sbuf;
              strncpy(fi->name, sbuf, pl+1);
              fi->name[pl+1] = '\0';
          } else {
              strcpy(fi->name,edisk);
          }
        } else { /* this directory */
            strncpy(fi->name, edisk, strlen(edisk)-1); /* remove last '\' */
            fi->name[strlen(edisk)-1] = '\0';
        }
      } else { /* lower directory */
          strcpy(name, fi->name);
          strcpy(fi->name, edisk);
          strcat(fi->name, name);
      }
      strcpy(fi->type, "DIR"); /* not a real file type... */
      add_command();
      strcpy(fi->type, "");    /* ...so remove it! */
      fi->order = 1;
    } else { /* not a directory */
        ploc = strrchr(fi->name, '.'); /* point found from name? */
        if (ploc != NULL) {
            strcpy(fi->type,++ploc);
            *ploc='\0';
        } else {
            strcpy(fi->type, " ");
        }
        add_command();
        fi->order = ++order_nr;
    }
}

static void add_command(void)
{
    int i,j;

    j=spfind(fi->type); /* search specials */
    if (j>=0) strcpy(fi->cmd, spb[j]); /* a command was given */
    ty=&types[0];
    for (i=0; i<TypeCount; i++, ty++) { /* search types */
        if (!strcmp(fi->type, ty->type)) { /* types match */
            if (j<0) strcpy(fi->cmd, ty->cmd); /* default */
            fi->get_comments=ty->get_comments; /* comments depend on this */
            fi->notype=ty->notype; /* type printing depends on this */
        }
    }
    for (i=0; i<strlen(fi->cmd); i++) /* replace underscores */
        if (fi->cmd[i]=='_') fi->cmd[i]=' ';

    if (!strcmp(fi->type, "DIR")) {
        if (SubDirectory(fi)) {
            return; /* dirs don't count */
        } else { /* files like "MSCREATE.DIR" (28.9.2000) */
            sprintf(sbuf, "%s%s", fi->name, fi->type);
            strcpy(fi->name, sbuf);
            strcpy(fi->type, "");
            strcpy(fi->cmd, "SHOW");
            return;
        }
    }
    if (!indexdir) { /* everything counts in DIR */
        if (!strncmp(fi->cmd, ">", 1)) return; /* OS-commands don't count anymore */
    }

     /* Survo-command? count length */
    i=strlen(fi->cmd);
    i+=min((8+1),strlen(fi->name)); /* point adds one! */ /* 8.6.2000 */
    if (print_filetype) {
        i+=min(3,strlen(fi->type));                       /* 8.6.2000 */
    } else {
        if (!fi->notype) i+=min(3,strlen(fi->type));      /* 8.6.2000 */
        else i--; /* the point will be also removed, if type omitted */
    }
    i++; /* 1st space also! */
    if (i>GV.bigglen) GV.bigglen=i; /* update the longest counter */
    if (fi->size > GV.bytes) GV.bytes=fi->size; /* biggest file? */
}



static void INDEXsort_files(int count)
{
    int i;

    fi=&files[0];
    qsort((void *) fi, (size_t)count, NFILES, comp1);
    fi=&files[GV.dircount]; /* keep the directories first, sort the rest */
    count-=GV.dircount;
    i=spfind("SORT");
    if (i>=0) {
      if (display_mode) muste_kv_s_disp("\nsorting files according to %s...", spb[i]);

      if      (!strcmp(spb[i],  "OS"))
        ; /* qsort((void *) fi, (size_t)count, NFILES, comp1); */
      else if (!strcmp(spb[i], "-OS"))
        qsort((void *) fi, (size_t)count, NFILES, comp1b);
      else if (!strcmp(spb[i],  "DATE"))
        qsort((void *) fi, (size_t)count, NFILES, comp2);
      else if (!strcmp(spb[i], "-DATE"))
        qsort((void *) fi, (size_t)count, NFILES, comp2b);
      else if (!strcmp(spb[i],  "TIME"))
        qsort((void *) fi, (size_t)count, NFILES, comp2);
      else if (!strcmp(spb[i], "-TIME"))
        qsort((void *) fi, (size_t)count, NFILES, comp2b);
      else if (!strcmp(spb[i],  "SIZE"))
        qsort((void *) fi, (size_t)count, NFILES, comp3);
      else if (!strcmp(spb[i], "-SIZE"))
        qsort((void *) fi, (size_t)count, NFILES, comp3b);
      else if (!strcmp(spb[i],  "NAME"))
        qsort((void *) fi, (size_t)count, NFILES, comp4);
      else if (!strcmp(spb[i], "-NAME"))
        qsort((void *) fi, (size_t)count, NFILES, comp4b);
      else if (!strcmp(spb[i],  "TYPE"))
        qsort((void *) fi, (size_t)count, NFILES, comp5c);
      else if (!strcmp(spb[i], "-TYPE"))
        qsort((void *) fi, (size_t)count, NFILES, comp5d);
      else muste_kv_s_err("Sorting option %s is unknown!", spb[i]);
    }
    return;
}


static void INDEXprintout(void)
{
    int i,j,k;

    i=specifics(); if (i<0) return;

    printcount=0;

    for (i=-1; i<=groups; i++) { /* -1 for printing only directories! */
        if (i==-1 && GV.dircount==0) continue;
        fi=&files[0];
        if (i>=0 && i<groups) strcpy(typ, group_types[i]);
        the_rest=0;
        if (i==groups) {
            the_rest=1;
            if (only_grouptypes) break; /* 14.7.95/kv (SM), kept 8.3.96 */
        }
        found_some=0;
        for (j=0; j<GV.filecount; j++, fi++) {
            if (fi->printed) continue; /* printed already? */

            if (FMatch || the_rest || SubDirectory(fi)) {
                k=print_line();
                if (k) { printcount++; found_some=1; } // dirs off in DIR
            }
        }
        if (found_some) {
            if (printcount<GV.filecount) print_empty_line();
        }
    }
    if (display_mode) output_close(outfile); else fclose(output_file);
    return;
}

static int specifics (void)
{
    int i,j,dtlen;
    char *tmp3[2];

    lowercase=0; uppercase=0;
    j=spfind("CASE");
    if (j>=0) {
        if (!strcmp(spb[j], "LOWER")) lowercase=1;
        if (!strcmp(spb[j], "UPPER")) uppercase=1;
    }

    list_to_field=1;
    sprintf(outfile, "%sINDEX.TMP", etmpd);
    j=spfind("OUTFILE");
    if (j>=0) {
        list_to_field=0; /* no output to the edit field, if file given! */
        if (strchr(spb[j], ':') != NULL) {
            strcpy(outfile, spb[j]);
        } else { /* file will be in the current path */
            strcpy(outfile, edisk);
            strcat(outfile, spb[j]);
        }
    }
    if (display_mode) {
        output_file = fopen(outfile, "w"); /* remove any existing file */
        fclose(output_file); /* close it, so output_open can open it */
        i=output_open(outfile);  /* use only output_line */
        if (i<0) return -1;
    } else { /* display=off - can't use output_line :-( */
        output_file = fopen(outfile, "w"); /* same idea here */
        if (output_file == NULL) {
            muste_kv_s_err("Could not open output file %s!", outfile);
            return -1;
        }
    }

    width=min(c3,ed1-1); /* c3 comes from s_init - #(columns on the screen) */
    j=spfind("WIDTH");
    if (j>=0) width=atoi(spb[j]);
    if ((width<0) || (width>MAXWIDTH)) {
        muste_kv_s_err("Width %d is out of range!", width);
        return -1;
    }
    ltoa(GV.bytes,bytes,10); /* convert biggest to length of biggest */
    bigg=strlen(bytes);

    GV.print_size=1; /* 21.7.98 */
    j=spfind("SIZE"); if (j>=0) GV.print_size=atoi(spb[j]);
    dtlen=count_date_time_length(); /* made to use same code 21.7.98 */
    GV.required=GV.bigglen+3; /* longest command+file + " / " */
    GV.required+=dtlen; /* datetime_str length */
    if (GV.print_size) GV.required+=bigg+1; /* biggest size + " " */
    GV.commlen=width-GV.required;
    if ((GV.commlen<0) || (GV.commlen>MAXWIDTH)) GV.commlen=0;
    comments_to_left=1; /* default - the original format */
    j=spfind("COMMENTS");
    if (j>=0) {
        i=split(spb[j],tmp3,2);
        if (!strcmp(tmp3[0], "LEFT")) comments_to_left=1;
        else if (!strcmp(tmp3[0], "RIGHT")) {
            comments_to_left=0; /* new option: comments on the right side */
            GV.commlen=c2-GV.required; /* c2==edit line length */
        } else GV.commlen=atoi(tmp3[0]); /* desired length was given */
        if (i>1) { /* i==2 */
            GV.commlen=atoi(tmp3[1]); /* desired length was given */
            if ((GV.commlen<0) || (GV.commlen>MAXWIDTH)) {
                muste_kv_s_err("Comment length %d is out of range!", GV.commlen);
                return -1;
            }
        }
    }
    bare_format=0;
    if(!GV.print_size &&!GV.print_date &&!GV.print_time &&!GV.commlen) bare_format=1;
    stats_format=0;
    full_format=0;

    j=spfind("FORMAT"); // (kaikki yhtaikaa mahdollisia 8.6.2004)
    if (j>=0) {
        if (strstr(spb[j],"BARE") != NULL) {
            bare_format=1;
            GV.print_size=0;
            GV.print_date=0;
            GV.print_time=0;
            GV.commlen=0;
        }
        if (strstr(spb[j],"STATS") != NULL) { /* 19.8.2001 (J.Valtonen: 'wc') */
            stats_format=1;
        }
        if (strstr(spb[j],"FULL") != NULL) { /* 8.6.2004 (J.Valtonen: /OPEN) */
            full_format=1;
            GV.required+=strlen(edisk);
            GV.bigglen+=strlen(edisk);
            GV.commlen-=strlen(edisk);
        }
    }
    GV.required+=GV.commlen;
    if (GV.required>MAXWIDTH) GV.commlen-=(GV.required-MAXWIDTH);
    return 1;
}

#define BUFLEN 10*LLENGTH
char buffer[BUFLEN];

static void INDEXget_comments(void)
{
    int i;
    char *p;
    int comments_ok=0; /* 28.9.2000 */
    char *ch;
    unsigned int ll, ww;
    int len, edt98, edt84;
    char *word[7];
    unsigned int cols, rows, l;

    for (i=0; i<LLENGTH; i++) comment_str[i]='\0'; /* empty the string */
    sprintf(tmp, "%s%s%s", edisk, fi->name, fi->type);

    if (stats_format) {
        /* code borrowed from SEARCH... */
        if ((fh=fopen(tmp,"r"))==NULL) return;
        if (fseek(fh, 0L, SEEK_SET)) return;
        if (fgets(buffer, BUFLEN-1, fh) == NULL) {
            if (feof(fh)) return;
        }
        fclose(fh);

        edt98=edt84=0;
        if (!(strncmp(buffer, "SURVO84ED", 9))) edt84=1;
        if (!(strncmp(buffer, "SURVO 98 edit",13))) edt98=1;
        if (edt98==0 && edt84==0) {
            if (strncmp(buffer,"%PDF",4)==0) return; /* PDF file */
            ch=strchr(buffer, '\n');
            if (ch==NULL) return; /* no line feed at all -> no text file */
            len=strlen(buffer);
            if (len==BUFLEN-2) return; /* line feed but not text file */
            /* check some codes used in sucro files... 7.3.2001 */
            ch=strchr(buffer, '¹'); if (ch!=NULL) return;
            ch=strchr(buffer, '³'); if (ch!=NULL) return;
            ch=strchr(buffer, '²'); if (ch!=NULL) return;
            ch=strchr(buffer, '_'); if (ch!=NULL) return;
        }
        fh=fopen(tmp,"r");
        ll=0; ww=0;
        if (edt98) {            /* SURVO 98 edit files */
            fgets(sbuf, LLENGTH-1, fh); /* read 1st line */
            muste_kv_space_split(sbuf,word,7); ll=atoi(word[5]);
            while(!feof(fh)) {
                if (fgets(sbuf, LLENGTH-1, fh) == NULL) {
                    if (feof(fh)) break;
                }
                if (sbuf[0]=='S') continue; /* shadow lines */
                ch=sbuf; while (*ch!='|') ++ch;  ch++; ch++; strcpy(buffer,ch);
                i=0; len=strlen(buffer);
                while (1) {
                    if (i==len) break;
                    if (buffer[i]==' ' || buffer[i]=='\t') { i++; continue; }
                    while (1) {
                        if (i==len) { if (i>1) ww++; break; }
                        if (buffer[i]!=' ' && buffer[i]!='\t') { i++; continue; }
                        ww++; break;
                    }
                }
            }
        } else if (edt84) {      /* SURVO 84C edit files */
            if (sscanf(buffer, "%s %u %u", tmp, &cols, &rows)==EOF) return;
            fseek(fh, (unsigned long)cols, SEEK_SET); /* first row! */
            ll=rows;
            for (l=1; l<=rows; l++) {
                fread(sbuf, sizeof(char), (size_t)cols, fh);
                sbuf[cols]='\0';
                ch=sbuf; ch++; strcpy(buffer,ch);
                i=0; len=strlen(buffer);
                while (1) {
                    if (i==len) break;
                    if (buffer[i]==' ' || buffer[i]=='\t') { i++; continue; }
                    while (1) {
                        if (i==len) { if (i>1) ww++; break; }
                        if (buffer[i]!=' ' && buffer[i]!='\t') { i++; continue; }
                        ww++; break;
                    }
                }
            }
        } else {
            while(!feof(fh)) {                          /* any other text files */
                if (fgets(buffer, BUFLEN-1, fh) == NULL) {
                    if (feof(fh)) break;
                }
                ll++;
                i=0; len=strlen(buffer);
                while (1) {
                    if (i==len) break;
                    if (buffer[i]==' ' || buffer[i]=='\t') { i++; continue; }
                    while (1) {
                        if (i==len) { if (i>1) ww++; break; }
                        if (buffer[i]!=' ' && buffer[i]!='\t') { i++; continue; }
                        ww++; break;
                    }
                }
            }
        }

        fclose(fh);
        sprintf(comment_str, "#lines=%u #words=%u", ll, ww);
        return;
    }

    if (fi->get_comments && GV.commlen) {
             /* changed GV.commlen to LLENGTH-1 in the following: (28.9.2000) */
             if (!strcmp(fi->type, "EDT")) get_edt_comments(comment_str, LLENGTH-1);
        else if (!strcmp(fi->type, "SVO")) get_svo_comments(comment_str, LLENGTH-1);
        else if (!strcmp(fi->type, "MAT")) get_mat_comments(comment_str, LLENGTH-1);
        else if (!strcmp(fi->type, "M"  )) get_mat_comments(comment_str, LLENGTH-1);
//      else if (!strcmp(fi->type, "SPX")) get_spx_comments(comment_str, LLENGTH-1);
//      else if (!strcmp(fi->type, "WPX")) get_spx_comments(comment_str, LLENGTH-1);
        else if (!strcmp(fi->type, "EMF")) get_emf_comments(comment_str, LLENGTH-1);
        comment_str[GV.commlen]='\0'; /* cut it */
        comments_ok=1;
    }

    if (!comments_ok) { // 8.6.2000: (enhanced 28.9.2000)
        strcpy(comment_str, fi->comment);
        strcpy(sbuf, fi->comment);
//      WAPI_ShortPath(sbuf);
        if (strcmp(comment_str, sbuf)!=0) {
            if (SubDirectory(fi)) sprintf(fi->name, "%s%s", edisk, sbuf);
            else strcpy(fi->name,sbuf);
            strcpy(fi->type,"");
        } else {
            strcpy(comment_str,"");
        }
        comment_str[GV.commlen]='\0'; /* cut it */
        if (GV.commlen==0) { strcpy(comment_str,""); strcpy(fi->comment,""); }
    }

}

static void make_line(void)
{
    if (SubDirectory(fi)) {
        if (strlen(comment_str))
            sprintf(line, "%s %s / %-*s",
                fi->cmd, fi->name, GV.commlen, comment_str);
        else sprintf(line, "%s %s", fi->cmd, fi->name);
        return;
    }
    if (OScomm && !indexdir) {
        sprintf(line, "%-*s", GV.bigglen, commfile_tmp);
        return;
    }
    if (bare_format) {
        strcpy(commfile_str, commfile_tmp);
        if (stats_format || full_format) strcat(commfile_str, " / "); // 8.6.2004
    } else {
        if (indexdir) {
            sprintf(commfile_str, "%-12s ", commfile_tmp);
        } else {
            sprintf(commfile_str, "%-*s / ", GV.bigglen, commfile_tmp);
        }
    }

    if (GV.print_size) {
        ltoa(fi->size,bytes,10); /* size to a string */
        sprintf(size_str, "%*s ", bigg, bytes);
    }
    INDEXmake_date_and_time(); /* into datetime_str */

    if (indexdir) {
        sprintf(line, "%s%s%s", commfile_str, size_str, datetime_str);
    } else {
//      if (stats_format) {
//          sprintf(line, "%s%s", commfile_str, comment_str);
//      } else {
            if (comments_to_left) sprintf(line,"%s%-*s %s%s",
                commfile_str, GV.commlen, comment_str, size_str, datetime_str);
            else sprintf(line,"%s%s%s%s",
                commfile_str, size_str, datetime_str, comment_str);
//      }
    }
}

static int  print_line(void)
{
    char empty[2];
    int numread;

    if (indexdir) strcpy(fi->cmd,"");

    if (!indexdir) INDEXget_comments();

    if (SubDirectory(fi)) {
        if (indexdir) return 0; /* dirs are not printed at all! (SM&KV 30.1.2001) */
    } else {
        if (indexdir) {
            strcpy(empty,"");
        } else {
            strcpy(empty," ");
            if (strlen(fi->cmd)==1)
                if (OScomm || SUCROcomm) strcpy(empty,"");
        }
        if (full_format) {
            sprintf(commfile_tmp, "%s%s%s%s", fi->cmd, empty, edisk, fi->name);
        } else {
            sprintf(commfile_tmp, "%s%s%s", fi->cmd, empty, fi->name);
        }
        if (print_filetype) strcat(commfile_tmp, fi->type);
        else {
            numread=strlen(commfile_tmp)-1;
            if (fi->notype) commfile_tmp[numread]='\0';
            else strcat(commfile_tmp, fi->type);
        }
    }

    make_line();

    if (lowercase) muste_strlwr(line); /* lower case wanted? */
    if (uppercase) muste_strupr(line); /* upper case wanted? */
    if (list_to_field) edwrite (space, results_line, 1);
    else results_line=0;
    if (display_mode) {
        output_line(line, outfile, results_line);
    } else {
        if (list_to_field) edwrite (line, results_line, 1);
        fprintf(output_file, "%s\n", line);
    }
    fi->printed=1; /* mark the file as printed */
    results_line++;
    if (results_line > ed2) { list_to_field=0; results_line=0; }
    return 1;
}

static void print_empty_line(void)
{
    if (list_to_field) edwrite (space, results_line, 1);
    results_line++;
    if (list_to_field) edwrite (space, results_line, 1);
        else results_line=0;
    if (display_mode) {
        output_line(space, outfile, results_line);
    } else {
        if (list_to_field) edwrite (space, results_line, 1);
        fprintf(output_file, "\n");
    }
}

static void INDEXmake_date_and_time(void)
{
    int i;
    char time_str[LNAME/2]; /* 21.7.1998 */
    unsigned year;
    unsigned YYYY;

    year = fi->year;
    if(year>99) year-=100; /* Y2K, 23.7.1997 */
    YYYY = fi->year+1900; /* 21.7.1998 */
    for (i=0; i<LNAME/2; i++) datetime_str[i]='\0';
    switch (GV.print_date) {
       case 0: break;
       case 1: sprintf(datetime_str, "%.2d.%.2d.%.2d ", fi->day, fi->month, year);
               break;
       case 2: sprintf(datetime_str, "%.2d/%.2d/%.2d ", fi->day, fi->month, year);
               break;
       case 3: sprintf(datetime_str, "%.2d%.2d%.2d ", fi->day, fi->month, year);
               break;
       case 4: sprintf(datetime_str, "%.2d%.2d%.2d ", year, fi->month, fi->day);
               break;
       case 5: sprintf(datetime_str, "%.2d/%.2d/%.2d ", fi->month, fi->day, year);
               break;
       case 6: sprintf(datetime_str, "%.2d.%.2d.%.2d ", fi->day, fi->month, YYYY);
               break;
       case 7: sprintf(datetime_str, "%.2d/%.2d/%.2d ", fi->day, fi->month, YYYY);
               break;
       case 8: sprintf(datetime_str, "%.2d/%.2d/%.2d ", fi->month, fi->day, YYYY);
               break;
       case 9: sprintf(datetime_str, "%.2d%.2d%.2d ", fi->day, fi->month, YYYY);
               break;
      case 10: sprintf(datetime_str, "%.2d%.2d%.2d ", YYYY, fi->month, fi->day);
               break;
      default: break;
    }
    switch (GV.print_time) {
       case 0: break;
       case 1: sprintf(time_str, "%.2d:%.2d:%.2d ", fi->hour, fi->minute, fi->second);
               break;
       case 2: sprintf(time_str, "%.2d:%.2d ", fi->hour, fi->minute);
               break;
       case 3: sprintf(time_str, "%.2d.%.2d.%.2d ", fi->hour, fi->minute, fi->second);
               break;
       case 4: sprintf(time_str, "%.2d.%.2d ", fi->hour, fi->minute);
               break;
      default: break;
    }
    if (GV.print_time) strcat(datetime_str, time_str);
}

// END INDEX //////////////////////////////////////////////////////////////////////


// Muste:   SEARCH, DD, WHERE(+TREE), DM toistaiseksi rajattu ulkopuolelle
// Muste:   SEARCH, DD, WHERE(+TREE), DM toistaiseksi rajattu ulkopuolelle
// Muste:   SEARCH, DD, WHERE(+TREE), DM toistaiseksi rajattu ulkopuolelle
// Muste:   SEARCH, DD, WHERE(+TREE), DM toistaiseksi rajattu ulkopuolelle
// Muste:   SEARCH, DD, WHERE(+TREE), DM toistaiseksi rajattu ulkopuolelle
// Muste:   SEARCH, DD, WHERE(+TREE), DM toistaiseksi rajattu ulkopuolelle

#endif
