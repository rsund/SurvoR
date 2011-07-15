/* desktop.c xx.x.1992/KV (27.12.2008)
   converted for Muste 8.6.2011/KV (14.7.2011)
 */

#define TOISTAISEKSI_SIVUUTETTU SUURIN_OSA


// BEGIN DESKTOP /////////////////////////////////////////////////////////////////


#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <ctype.h>
#include <time.h>
#include <math.h>
#include <limits.h>

#include <R.h>
#include <Rinternals.h>

#include "survo.h"
#include "survoext.h"
#include "survolib.h"

extern void muste_kv_s_err(char *, ...);
extern void muste_kv_s_disp(char *, ...);
extern void muste_kv_usage_info(void);
extern int  muste_kv_space_split(char *, char **, int);

// DESKTOP functions, originally SURVO 84C/98/MM modules, in chronologic order:
static int INDEXmain(void);   //    INDEX (1992-)
static int SEARCHmain(void);  //   SEARCH (1993-)
static int DDmain(void);      //       DD (1995-)
static int WHEREmain(void);   //    WHERE (1996-)
static int DMmain(void);      //       DM (1996-)
static void tree(void);       //     TREE (1997-)
static int MDmain(void);      //       MD (1999-)
static int RDmain(void);      //       RD (1999-)
static int op_dir(void);      //      DIR (2000-) part of INDEX (formerly in Editor)
static int op_DELTREE(void);  //  DELTREE (2001-)

// Placeholders for desktop main functions:

// INDEXmain is now implemented (but not too much tested or fine-tuned)
static int SEARCHmain(void) { muste_fixme("\nFIXME: SEARCH not yet implemented"); return 1; }
static int DDmain(void)     { muste_fixme("\nFIXME: DD not yet implemented"); return 1; }
static int WHEREmain(void)  { muste_fixme("\nFIXME: WHERE not yet implemented"); return 1; }
static int DMmain(void)     { muste_fixme("\nFIXME: DM not yet implemented"); return 1; }
static void tree(void)      { muste_fixme("\nFIXME: TREE not yet implemented"); return; }

// Shall we implement these or not? (I'm not so sure about DELTREE...)
//static int MDmain(void)     { muste_fixme("\nFIXME: MD not yet implemented"); return 1; }
//static int RDmain(void)     { muste_fixme("\nFIXME: RD not yet implemented"); return 1; }
//static int op_DELTREE(void)   { muste_fixme("\nFIXME: DELTREE not yet implemented"); return 1; }

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

#define SORTOPTLEN       6
#define GRPOPTLEN  LNAME/2
#define MAXGROUPS       13

#define NAMELEN LNAME      /* 256       */
#define PATHLEN LNAME      /* 256       */
#define COMMLEN LNAME      /* 256       */
#define TYPELEN LNAME/16   /* 256/16=16 */
#define CMDLEN  LNAME/4    /* 256/4=64  */
#define GRPTYPS LNAME/2    /* 256/2=128 */

#define MAXWIDTH LLENGTH-4
#define STRMAXL  LNAME/2

typedef struct globs {        /* global variables */
    char grouping[GRPOPTLEN]; /* GROUPING information */
    char sorting[SORTOPTLEN]; /* SORT information */
    int required;             /* required screen width */
    int biggest;              /* biggest file size */
    int bigglen;              /* length of the biggest file size */
    int commlen;              /* max length of the comments */
    int filecount;            /* # of files not deleted */
                              /* # of 'active' files in the source dir */
    int tfilecount;           /* # of files in the target directory */
    char filespec[LNAME];   /* the current path selection */
    int dircount;             /* # of directories */
    int totalcount;           /* # of files, also deleted files */
    char status;              /* status flags */
    char source[LNAME];     /* the source path (may include file) */
    char target[LNAME];     /* the target path (may include file) */
    char sourcedir[LNAME];  /* the source directory */
    char targetdir[LNAME];  /* the target directory */
    unsigned int bytes;      /* # of bytes in the tree */
    unsigned int files;      /* # of files in the directory */
    char treename[LNAME];   /* name of the tree file */
    FILE *treefile;           /* file pointer of the tree file */
    char path[LNAME];       /* path name which has been checked */
    int level;                /* how many levels are displayed (1.5.97) */
    int outfile;              /* print tree to file and exit (2.5.97) */
    int print_date;           /* how dates are printed */
    int print_time;           /* how times are printed */
    int print_size;           /* is size printed at all (21.7.98) */
    int print_filetype;       /* print the type or not TYPES=1/0 */
    int comments_to_left;      /* INDEX (Muste) */
    int list_to_field;         /* INDEX (Muste) */
    char comment_str[LLENGTH]; /* INDEX (Muste) */
    char datetime_str[LNAME/2];/* INDEX (Muste) */
    int groups;                 /* group count */
    int only_grouptypes;        /* process files which match GROUPING */
    char *group_types[GRPTYPS]; /* file types from GROUPING */
    int selected;               /* INDEX only selected files (such as *.C) */
} Globals;

typedef struct fileinfo *FIPtr;
typedef struct fileinfo {  /* file information structure */
    unsigned order;        /* array sorting number */
    char path[PATHLEN];    /* path of the file */
    char name[NAMELEN];    /* file name */
    char type[TYPELEN];    /* type name */
    char cmd[CMDLEN];      /* suitable command (INDEX) */
//  char attrib;           /* file attribute (OS) */ [POIS (Muste)]
    int isdir;             /* 1=dir, 0=file */
    unsigned int size;    /* file size in bytes */
    int year;              /* file modification year */
    int month;             /* file modification month */
    int day;               /* file modification day */
    int hour;              /* file modification hour */
    int minute;            /* file modification minute */
    int second;            /* file modification second */
    char status;           /* file flags: sorted,marked,deleted */
    int command;           /* which command to use later (DD etc.) */
    char comment[COMMLEN]; /* the comment string */
    int grouporder;        /* final order number */
    int match;             /* source-target matching (DM), line nr (SEARCH/DD) */
    short printed;         /* printed out (INDEX) */
    short get_comments;    /* get some sort of comments (INDEX) */
    short notype;          /* don't print file type (INDEX) */
    short hidden_comment;  /* show comment only under cursor (DD) */
    char longname[NAMELEN];/* long file name (added for DD 12.5.2003 */
    FIPtr next;            /* next file in the linked list */
} Files;

#define NFILES (sizeof(Files))

typedef struct filetypes1 {  /* file types and default commands */
    char type[TYPELEN];     /* file type */
    char cmd[CMDLEN];       /* the default command */
    short get_comments;     /* get some sort of comments: 1, not: 0 */
    short notype;           /* don't print file type: 1, print: 0 */
} Types1;

static Types1 *ty1, types[]= {
      { "DIR", "CD",            0, 1 },
      { "EDT", "SHOW",          1, 1 },
      { "SVO", "FILE_SHOW",     1, 1 },
      { "MAT", "/MATSHOW",      1, 1 },
      { "M"  , "/MATSHOW",      1, 0 },
      { "TUT", "/TUTSHOW",      0, 1 },
      { "BIN", "/CODESHOW",     0, 1 },
      { "TCH", "/TCHSHOW",      0, 1 },
      { "EMF", "GPLOT_FILE",    1, 1 },
      { "PDF", "/OPEN",         0, 0 },
      { "DOC", "/OPEN",         0, 0 },
      { "XLS", "/OPEN",         0, 0 },
      { "PPT", "/OPEN",         0, 0 },
      { "JPG", "/OPEN",         0, 0 },
      {"JPEG", "/OPEN",         0, 0 },
      { "GIF", "/OPEN",         0, 0 },
      { "HTM", "/OPEN",         0, 0 },
      {"HTML", "/OPEN",         0, 0 },
      { "PNG", "/OPEN",         0, 0 },
      { "RTF", "/OPEN",         0, 0 },
      { "SAV", "/OPEN",         0, 0 },
      { "ZIP", "/OPEN",         0, 0 },
      { "BMP", "/OPEN",         0, 0 },
      { "TIF", "/OPEN",         0, 0 },
      {"TIFF", "/OPEN",         0, 0 }
};
static const int TypeCount1=(sizeof(types)/sizeof(Types1)+1);

typedef struct filetypes2 {  /* file types and default commands */
    char type[TYPELEN];      /* file type (incl '.') */
    int command;             /* default command to use */
} Types2;

enum commands { DIRMOVE, SHOW, FSHOW,
                ZIPVIEW, LZHVIEW, MATSHOW, TUTSHOW,
                OP_EMF
              };

static Types2 *ty2, ftypes[]={
    { ".TUT", TUTSHOW },
    { ".EDT", SHOW    },
    { ".SVO", FSHOW   },
    { ".MAT", MATSHOW },
    { ".M",   MATSHOW },
    { ".ZIP", ZIPVIEW },
    { ".LZH", LZHVIEW },
    { ".EMF", OP_EMF  }
};
static const int TypeCount2=(sizeof(ftypes)/sizeof(Types2)+1);

typedef struct dnode *DLPtr;
typedef struct dnode { /* directory list node: */
    char files[LNAME];
    char point[LNAME];
    char sorting[SORTOPTLEN];
    char grouping[GRPOPTLEN];
    DLPtr prev;
    DLPtr next;
} DirListNode;

typedef struct dirinfo *TDPtr;
typedef struct dirinfo { /* directory information structure */
    char name[LNAME];    /* dir name (incl. type) */
    char path[LLENGTH];  /* whole path name of the directory */
    int sub;             /* number of sub dirs, -1: node hidden */
    int level;           /* level: 0,1,2,3,... */
    int order;           /* order number in the tree */
    unsigned int files; /* # of files in the directory */
    unsigned int bytes; /* # of bytes in the directory */
    TDPtr next;          /* next dir in the linked list */
} DirNode;


static Files *files, *fi, *f;  /* file info structures */
static FIPtr *df;              /* display file pointer */
static Globals GV;             /* structure of global variables */
static FIPtr FL,FLp,FLpp;
static char outfile[LNAME];
static FILE *output_file;
static FILE *fh;
static struct tm *write_time;

#define SubDirectory(x) ((x)->isdir==1)

#define FSORTED     0x01  /* file sorted */
#define FMARKED     0x02  /* file marked for copying, deletion etc */
#define FDELETED    0x04  /* file deleted (lazy deletion) */
#define FNOTFILE    0x08  /* not file, just placeholder (DM) */

#define FileSorted(x)   (((x)->status & FSORTED)   == FSORTED)
#define FileMarked(x)   (((x)->status & FMARKED)   == FMARKED)
#define FileDeleted(x)  (((x)->status & FDELETED)  == FDELETED)
#define FileNotFile(x)  (((x)->status & FNOTFILE)  == FNOTFILE)

#define FirstFile(x)    ((x)==&files[0])
#define LastFile(x)     ((x)==&files[GV.filecount-1])

/* In SURVO 84C, LNAME used to be 64 ... needed in some prompts etc. */
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
#define DDCALL     68   /* D */
#define FILL       70   /* F */
#define BOTH       66   /* B */ /* Both=Update&Fill */
#define OPEN       79   /* O */

#define WORKROW          2
#define WORKROW1         2
#define WORKROW2         3
#define WORKROW3         4
#define ENDROW      (r3+1)
#define BOTTOMROW   (r3+2)
#define ddROWS      (r3-2)
#define ddSTARTROW       4
#define dmROWS      (r3-4)
#define dmSTARTROW       6
#define treeROWS    (r3-1)
#define treeSTARTROW     3

#define TreeDown    "|   "  // "|   "
#define TreeBlank   "    "  // "    "
#define TreeCorner  "+---"  // "+---"
#define TreeSibling "+---"  // "+---"

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

// DD, DM etc.:
// Colors finalized for SURVO MM 7.3.2001  Muste: in process...
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
#define ddMsgA   "C=Copy D=Delete M=Move R=Rename O=Open F1=HELP    "
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

// SEARCH:
// Colors finalized for SURVO MM 7.3.2001  Muste: in process...
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

static int ScreenWidth; // in certain critical places, 80 is used!
static char *siirtop;          /* argv[1] for spawning processes */
static char userline[LLENGTH]; /* original command line */
static char answer[LNAME];     /* user's answers */
static int results_line;       /* first line for the results */
static int indexdir, no_cd;

static void disable_softkeys(void);
static void enable_softkeys(void);

static int DDerror;            /* error code (used by display_error) */
//#define HARDERR 1
#define BADPATH 2
#define NOFILES 3

static char line[LLENGTH];
static char tmp[LLENGTH];
static char path[LNAME];
//static char drive[5];
//static char dir[LNAME];
//static char fname[NAMELEN];
//static char ext[TYPELEN];
static int current, markcount, matchcount;
static unsigned int markbytes, totalbytes, ttotalbytes, matchbytes;

static char origline[LLENGTH];
static char origname[LNAME];
static char filespec[LNAME];
static char fullspec[LNAME];

static int count;
static int biggest;

static void trim(char *, char *);
static void disp_err(char *, ...);
static void display_error(char *);
static void tut_error(int code);
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
static void flash_counter(int);

static int save_marked_files_for_DD(void);
static int mark_saved_files_from_DM(void);

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
static int comp9(const void *, const void *);
static int comp9b(const void *, const void *);
static int comp10(const void *, const void *);
static int comp10b(const void *, const void *);

static int kv_edline(char *, int, int);
static void get_edt_comments(char *, int);
static void get_svo_comments(char *, int);
static void get_mat_comments(char *, int);

// All these are for the restrictions() :

typedef struct yymmdd_dates {
    unsigned short dd;
    unsigned short mm;
    unsigned short yy;
} yymmdd_date;

static int since_i, before_i, bytmin_i, bytmax_i;
static yymmdd_date since_date, before_date;
static unsigned int bytmin, bytmax;

static int restrictions(void);
static int valid_date(int);
static int split_date(yymmdd_date *, int);
static void no_valid_files(void);

// INDEX ...

static int INDEXcheck_parameters(void);
static int INDEXget_fileinfo_from_R(void);
static void INDEXsort_files(void);
static void INDEXprintout(void);
static void INDEXget_comments(void);
static int INDEXprint_line(void);
static void INDEXmake_date_and_time(void);

static int bare_format;
static int stats_format;
static int full_format;
static char buffer[LLENGTH];



#if TOISTAISEKSI_SIVUUTETTU


// SEARCH ...

static int parse_arguments(void);
static void quoted_usage(void);
static int search_files(void);
static int display_msg(void);
static int read_edt_file(char *);
static void edt_search_msg(void);
static int read_any_file(char *);
static void any_search_msg(void);
static void update_field(unsigned int, char *, char *);
static void handle_buffer(int, int);
static void handle_shadow_buffer(int, int);
static void write_results(void);
static int search(void);
static void give_bad_message(char *);

#define Bottom  "SEARCH: ENTER=Next ESC=Continuous/stepwise search S=Skip to next file EXIT=Stop "
#define NotEn1  "Not enough empty lines. Insert space for %u lines (Y/N) ? "
#define NotEn2  "Not enough lines in the edit field! (Press any key!)"
#define NoFiles "No such files! (Press any key!)"
#define NoMatch "Not found! (Press any key!)"
#define CANCELED ((retval==CODE_EXIT) || (retval==STOP) || (retval<0)) /* 22.7.1998 */
#define SKIPPED  ((retval=='S') || (retval=='s'))  /* 24.1.96 */
#define SVOEDT   (!(strncmp(check, "SURVO84ED", 9)))
#define SVOEDT98 (!(strncmp(check, "SURVO 98 edit",13)))
#define MAX_GIVEN_PATHS 20

static int retval,col,row,col1,col2,len0,len1,len2,len3,edt98;
static unsigned int recursive, continuous, show_command, exact_search,
       columns_given, field_ok, files_found, ignorecase, search_first,
       search_file, search_DD, found, search_comment, search_shadows;
static unsigned int matches, first_matches, files_total, lines_total;
static char bigbuffer[LLENGTH], shadow_buffer[LLENGTH], ahead_buffer[LLENGTH],
       orig_buffer[LLENGTH], check[LNAME/2], match_msg[LNAME/4];
static char search_string[LLENGTH];



// DD ...

static int DDfind_files(void);
static int space_alloc(void), DDdisplay_files(void);
static void handle_dirlist(const int);
static int DDcheck_given_path(char *, int);
static int DDchange_disk(char *, char *);
static void DDsort_files(void);
static void DDgroup_order_files(void);
static void DDinternal_sort(void);
static void DDget_comments(char *, int, FIPtr);
static int DDfind_files(void), DDfound_files(int);
static int whereis(void), DDalloc_files(void);
static void update_globals(void), DDremove_list(void);

static void DDinfoline(void), DDget_line(int), DDdrawline(char,int,int);
static void delete_tempdir(void), delete_tempfile(void);

#define TMPFILE "DD.TMP"
#define TMPDIR  "DDTMP"

static int DDf_key(int), DDf_show(unsigned int), DDf_move(void);
static int DDf_copy(int), DDf_delete(void), DDf_delfile(void);
static int DDf_where(void), DDf_load(void), DDf_disk(void), DDf_search(void);
static int update_dirlist(void);
static int DDf_tutshow(void), DDf_matshow(void), DDf_act(void), DDf_free(int);

static void DDf_help(void), DDf_attrib(void), DDf_rename(void);

static char tempfil[LNAME], pathtobedeleted[LNAME];
static char revisitfile[NAMELEN+TYPELEN], revisitpath[LNAME];
static int revisit;

static int DDdisplay_files(void);
static int DDhandle_key(unsigned int);
static void DDdisplay_line(int,int,int);
static void DDshow_mode_code(void), delete_node_from_list(void);
static void DDcount_totalbytes(void), DDcount_totaldirs(void);
static void check_revisiting(void);
static void update_tutstack(void);
static void DDmake_file_name_and_size(char *);
static void delete_recursively(void);
static void filename_to_sbuf(void);
static double *A;
static char *rlab,*clab;
static int xview(char *, char *, char *, char *, char *);
static void no_graphics(int);
static void prg_not_found(char *cmd);
#define SRCHSTRLEN 26
static char srchstr[SRCHSTRLEN+1];
static int check_diskspace(char *), check_ports(char *);
static int check_and_mk_dir(int);
static void DDmake_date_and_time(char *);

#define EndOfPageCondition ((GV.filecount==0||row==ENDROW||LastFile(fi)))

static int stopped;
static int where_outfile; /* 15.10.2005 */

static char com_str[LLENGTH]; /* was LNAME before 31.12.97 *//* global 22.7.1998 */

static DLPtr DLAlloc(void);
static void free_list(void);

#define READ  0 /* directory list */
#define WRITE 1 /* directory list */

#define WhereStart (!strcmp(info,word[1]))    /* 14.4.96 */
#define SEARCHStart (!strcmp(info,"SEARCH"))  /* 22.7.1998 */
#define DMStart (!strcmp(info,"DM_DD"))  /* 17.1.1999 */

#define DirListFile "DIRLIST.DD"

static DLPtr D,Dp,DLast;            /* directory list pointers */
static int whstart;                 /* DD started by WHERE module */
static int sestart; /* 22.7.1998 */ /* DD started by SEARCH */
static int dmstart; /* 17.1.1999 */ /* DD started by DM module */
static int where_first_found;       /* DD used non-interactively by WHERE (13.2.2002) */
static int where_first;
static int where_first_exit=0;

static void dirmagic(void);

// WHERE/TREE ...

static void tree_keys(void);

static TDPtr TD,TDp;     /* tree structure (linked list) pointers */
static TDPtr TDAlloc(void);

static void tree(void);
static TDPtr TDAlloc(void);
static int TREEchange_disk(char *, char *);
static int TREEcheck_given_path(char *, int);

static char TREEfiles[LLENGTH];
static char root1[LNAME], roots[LNAME];
static void free_tree(void);
static int scan(int), TREEprintout(char *, int, int), showtree(void);

static void TREEinfoline(void), f_help(void);

static int f_key(int), f_load(void), showtree(void);

#define FirstDir (current==0 && fi2<=dfi)
#define LastDir (fi2==GV.dircount)

static int handle_key(unsigned int);
static void display_line(void);
static int fi2; /* 0,...,GV.dircount-1 */
static int dfi; /* needs to be global */
static int fullname=0; /* full path names on the screen / short names */
static int row;

#define TEndOfPage ((GV.dircount==0||row==ENDROW||LastDir))

// DM ...

static void dirmatch(void);
static int DMfind_files(int), DMdisplay_files(void);
static void dm_keys(void);
static int DMcheck_parameters(void);
static int DMcheck_given_path(char *, int);
static int DMfind_files(int), DMfound_files(void), DMalloc_files(int);
static void DMremove_list(void);
static void DMsort_files(void);
static void DMgroup_order_files(void);
static void link_source_and_target(void);
static void DMinternal_sort(void);
static void DMinfoline(void), DMclear_screen(void);
static void DMdrawline(char,int,int,char), DMget_line(void);
static int DMf_key(int), DMf_copy(void), DMf_show(void);
static void DMf_help(void);
static int DMdisplay_files(void);
static int DMhandle_key(unsigned int);
static void DMdisplay_line(int,int,int);
static void DMget_line(void);
static void DMcount_totalbytes(void);
static void source_and_target(void);
static void DMshow_mode_code(void);
static void count_marked_files(void);
static void mark_newer_files(void);
static void mark_lonely_files(void);
static void drawlink(char *, char, char, int); /* 13.7.98 */
static void DMmake_file_name_and_size(char *);
static void DMmake_date_and_time(char *);


/* LastFile2 added 12.9.97 for displaying also all target files */
#define LastFile2(x)     ((x)==&files[(GV.filecount+GV.dircount)-1])

static Files *tfiles;   /* the target file array */

static int ghostlink;
static int ghostcount;
static int quick_update; /* 1.12.1997 */
static int quick_yes; /* 13.7.1998 */

static char edisk0[LNAME/2], drive0[3], dir0[LNAME/2], *p0;


#endif // TOISTAISEKSI_SIVUUTETTU


// DESKTOP ...

static
char *spec_desktop[]={ "TUTOR",                "CD",               // DESKTOP
                       "DATE", "TIME", "GROUPING", "SORT", "SIZE", // GEN*
                       "TUTSTACK", "OPTIONS",                      // DD
                       "SINCE", "BEFORE", "MAXBYTES", "MINBYTES",  // DD ym.
                       "DM", "DISPLAY",                            // DM
                       "PRIND", "TYPES", "ONLY",         "WIDTH",  // INDEX
                       "OUTFILE", "COMMENTS", "FORMAT",            // INDEX
                       "DIR","EDT","SVO","MAT","TUT","EMF","TXT",  // INDEX
                       "SHOW", "SEARCH", "RUN", "COLS", "FILES",   // SEARCH
                       "LEVEL", "WHERE", "!" };                    // TREE,WHERE
static char **specs=spec_desktop;

void muste_desktop(char *argv)
{
    int i;

    siirtop=argv;
    s_init(siirtop);

    i=spec_init(r1+r-1); if (i<0) return;

    // restrictions() - initialize:
     since_i=INT_MIN;
    before_i=INT_MIN;
    bytmin_i=INT_MIN;
    bytmax_i=INT_MIN;

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

    ScreenWidth=c3+8;
//  case_sensitive_pathnames=1; // 0 in Survo, 1 in Muste         POIS    KOKONAAN!!
//  i=spfind("UPPER_NAMES");
//  if (i>=0) case_sensitive_pathnames=1-atoi(spb[i]);
    indexdir=0;
    no_cd=0;
    i=spfind("CD");
    if (i>=0) { if (!atoi(spb[i])) no_cd=1; }

    edread(origline,r1+r-1);      /* 27.2.2001 for <Survo> paths          */
//  subst_survo_path(origline);   /* "<Survo>" -> C:\PROGRA~1\SURVO\ etc. */
    muste_expand_path(origline);
    g=split(origline+1,word,5);
//  g=split(origline+1,word,100); /* '+1' omits the control char.         */
//                       /* 100 was 5, but INDEX /SHORTNAME needs all!! (9.1.2002) */
    for (i=1; i<g; i++) {
        if (!strcmp(word[i],"/")) { g=i; break; }
    }

         if (!muste_strcmpi(word[0],"INDEX")) INDEXmain();
//  else if (!muste_strcmpi(word[0],"MD")) MDmain();
//  else if (!muste_strcmpi(word[0],"RD")) RDmain();
    else if (!muste_strcmpi(word[0],"DIR")) op_dir();
//  else if (!muste_strcmpi(word[0],"DELTREE")) op_DELTREE();
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
    s_end(siirtop);

    return;
}

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

static void disable_softkeys(void) /* 7.6.2000 */
{
    if (r_soft) r3+=r_soft+1;
}

static void enable_softkeys(void)
{
    if (r_soft) r3-=r_soft+1;
}

static void disp_err (char *fmt, ...)
{
  char buf[LNAME/2];
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
//     case HARDERR: disp_err(" Device not ready (%s)!", str); break;
       case BADPATH: disp_err(" Bad path (%s)!", str); break;
       case NOFILES: disp_err(" No files found (%s)!",str); break;
    }
}

static void tut_error(int code) /* 28.8.2000 (for SM, softkeys) */
{
    switch(code) {
        case  1: strcpy(tut_info, "\254\254\254@1@INDEX@No files found!");  // Muste: octal
                 break;
        case 11: strcpy(tut_info, "\254\254\254@1@DD@No files found!");     // Muste: octal
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
    if (!strcmp(format, "DD.MM.YYYY")) return 6;
    if (!strcmp(format, "DD/MM/YYYY")) return 7;
    if (!strcmp(format, "MM/DD/YYYY")) return 8;
    if (!strcmp(format, "DDMMYYYY"))   return 9;
    if (!strcmp(format, "YYYYMMDD"))   return 10;
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
/* Mute: long muutettu int */

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

static int restrictions(void) // Muste: fil muutettu fi (INDEX) - ks. myös muut!!!
{
    int i;
    unsigned int size;
    unsigned short year,month,day;

    /* read spec.vector only once; init. structures with zeros */
    if (since_i  == INT_MIN)  {
        since_i=spfind("SINCE");
        since_date.dd=0; since_date.mm=0; since_date.yy=0;
        if (since_i >=0) {
            i=valid_date(since_i);
            if (i<0) return -1;
        }
    }
    if (before_i == INT_MIN)  {
        before_i=spfind("BEFORE");
        before_date.dd=0; before_date.mm=0; before_date.yy=0;
        if (before_i >=0) {
            i=valid_date(before_i);
            if (i<0) return -1;
        }
    }
    if (bytmin_i == INT_MIN)  {
        bytmin=0;
        bytmin_i=spfind("MINBYTES");
        if (bytmin_i >=0) bytmin=atoi(spb[bytmin_i]);
    }
    if (bytmax_i == INT_MIN)  {
        bytmax=INT_MAX;
        bytmax_i=spfind("MAXBYTES");
        if (bytmax_i >=0) bytmax=atoi(spb[bytmax_i]);
    }

    if (since_i<0 && before_i<0 && bytmin_i<0 && bytmax_i<0) return 0;

    /* check every possible restriction; first failure returns 1 */

    size=fi->size; /* the size of the current file */
    if (size < bytmin || size > bytmax) return 1;

    if (since_i<0 && before_i<0) return 0;

    year  = fi->year;
    month = fi->month;
    day   = fi->day;

    if (since_i >=0) {
        if (!since_date.dd && !since_date.mm && !since_date.yy) {
            i=split_date(&since_date, since_i);   /* only once */
            if (i<0) return -1;
        }
//muste_kv_s_err("since_date: %d.%d.%d", since_date.dd, since_date.mm, since_date.yy);
//muste_kv_s_err("%s: %d.%d.%d",fi->name,day,month,year);
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

static int split_date(yymmdd_date *ddmmyy, int idx)
{
    char *p;
    struct tm *D;
    time_t tnow;
    unsigned short dd,mm,yy;
    int rel_time;
    char *rel;

    p=spb[idx]; dd=mm=yy=0;
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
        if (yy>1900) yy-=1900;
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



static int op_dir(void)
{
    indexdir=1;
    no_cd=1;
    INDEXmain();
    return 1;
}

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
    return 1;

#if 0
    int val; /* for testing DOS/32A 17.12.1997 */
    char moduli[LNAME];


    strcpy(moduli,survo_path); strcat(moduli,prog);
    enable_softkeys(); s_end(siirtop);
//  val=spawnl(P_WAIT,moduli,moduli,siirtop,"2","3",NULL);
    s_init(siirtop); // 28.8.2000 !! (SHOW:n Load lines alkoi toimia!)
    disable_softkeys();
    if (val<0)
        disp_err(" Error in spawning %s!", moduli);
    return val;
#endif
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
        strncpy(GV.grouping, spb[j], GRPOPTLEN);
    }
    j=spfind("SORT");
    if (j>=0) {
        if (strlen(spb[j])<SORTOPTLEN) {
            strncpy(GV.sorting, spb[j], SORTOPTLEN);
        }
    }
    GV.print_size=1;
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
// muste_system(char *komento, int wait) / RS 8.7.2011 ks.!
    system(answer);
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

static void f_group(void)
{
    init_prompt();
    strcpy(answer,GV.grouping);
    prompt(" GROUPING=",answer,min((GRPOPTLEN-1),80-10-2));
    if (!strcmp(answer,GV.grouping)) return; /* no changes */
    strcpy(GV.grouping,answer);
}

static void flash_counter(int count)
{
    sprintf(sbuf,"     %6d",count);
    write_string(sbuf,11,'/',1,(c3+8-1)-11);
}

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
    if (nr1->second > nr2->second) return 1;
    if (nr1->second < nr2->second) return -1;
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
    char *p1, *p2;
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
    int cols,i,j,k,numread;
    char ch='\0', *p;
    int survo84, survo98; // two different ways of saving .edt files

    fh=muste_fopen(tmp,"rb"); // (static tmp set by caller)
    if (fh==NULL) return;
    survo84=0;
    survo98=0;
    numread=fread((void *)sbuf,sizeof(char),(size_t)14,fh);
    if (numread<14) survo84=0; else sbuf[14]='\0';
    if (!strncmp(sbuf, "SURVO84ED",9)) survo84=1;
    if (!strncmp(sbuf, "SURVO 98 edit",13)) survo98=1;

    if (survo84) { // old way: binary (random access) file
        sscanf(sbuf, "%s %d", tmp, &cols);
        muste_fseek(fh, cols+1, SEEK_SET);
        numread=fread((void *)sbuf,sizeof(char),(size_t)cols-1,fh);
        if (numread >= cols) {
            sbuf[cols]='\0';
            if (!muste_strnicmp(sbuf,"SAVE ",5)) {
                i=5;
                while ((ch!='/')&&(i<cols-5)) ch=sbuf[i++];
                if (ch=='/') {
                    ++i;
                    k=min(len,(cols-i-1)); // (len set by caller)
                    for (j=0; j<k; ++i,j++) str[j]=sbuf[i];
                    str[j]='\0';
                }
            }
        }
    }
    fclose(fh);

    if (survo98) { // new way: ordinary text file
        fh=muste_fopen(tmp,"r"); if (fh==NULL) return;
        fgets(sbuf,LLENGTH-1,fh); /* ID line */
        fgets(sbuf,LLENGTH-1,fh);
        fclose(fh);
        sscanf(sbuf, "%d", &cols); /* line number */
        if (cols!=1) return; /* should be 1st line */
        p=strchr(sbuf, '|');
        if (p!=NULL) {
            p+=2; /* skip | and control char */
            while (*p==' ' && *p!='\0') p++; /* skip possible white space */
            if (!muste_strnicmp(p,"SAVE ",5)) {
                sbuf[strlen(sbuf)-1]='\0';
                p=strstr(sbuf," / ");
                if (p!=NULL) {
                    p+=3;
                    strncpy(str,p,len);
                }
                // Remove CR character: (27.6.2011/RS)
                for (i=0; str[i]!='\0'; i++) if (str[i]=='\r') str[i]=' ';
            }
        }
    }
}

static void get_svo_comments(char *str, int len)
{
    int i,j,k,numread;
    short textn,textlen;
    int text;

    k=0;
    fh=muste_fopen(tmp,"rb");
    if (fh==NULL) { fi->command=SHOW; return; }
    k=1;
    numread=fread((void *)sbuf,sizeof(char),(size_t)LNAME-1,fh);
    if (numread<LNAME-1) k=0;
    if (k && strncmp(sbuf,"SURVO 84C DATA",14)) k=0;
    if (!k) { fclose(fh); fi->command=SHOW; return; }

    muste_fseek(fh, 30, SEEK_SET);
    fread((void *)&textn, sizeof(char), (size_t)2, fh);
    muste_fseek(fh, 32, SEEK_SET);
    fread((void *)&textlen, sizeof(char), (size_t)2, fh);
    muste_fseek(fh, 34, SEEK_SET);
    fread((void *)&text, sizeof(char), (size_t)4, fh);
    muste_fseek(fh, text, SEEK_SET);
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
    int i,j,k,m,n,nrem,lr,lc,type,numread;

    k=0;
    fh=muste_fopen(tmp,"rb");
    if (fh==NULL) { fi->command=SHOW; return; }
    k=1;
    numread=fread((void *)sbuf, sizeof(char), (size_t)ERC, fh);
    if (numread<ERC) k=0;
    sbuf[ERC]='\0';
    if (k && strncmp(sbuf,"MATRIX84D",9)) k=0;
    if (!k) { fclose(fh); fi->command=SHOW; return; }

    sscanf(sbuf, "%s %d %d %d %d %d %d",tmp,&m,&n,&nrem,&lr,&lc,&type);
    muste_fseek(fh, (int)ERC, SEEK_SET);
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

static int kv_edline(char *label, int j, int error)
// more pedantic wrap for edline2() - needed at least in INDEX
{
    int i, len, word_int, colonfirst, lab, minus1, minus2, minus3, cur1, end1;
    char *cur, *end, *plus, *minus, *colon;
    char labl[LNAME];

    len=strlen(label);
    word_int=atoi(label);
    strcpy(labl,label);
    muste_strupr(labl);
    cur=strstr(labl, "CUR");
    end=strstr(labl, "END");
    plus=strchr(label, '+');
    minus=strchr(label, '-');
    colon=strchr(label, ':');
    colonfirst=!strncmp(label, ":", 1);
    lab=(!word_int && len==1);   /* e.g. A or \ or %, but not * ! */
    lab=(lab && strncmp(label, "*", 1));
    cur1=(cur && !colon);          /* e.g. CUR+3 */
    end1=(end && !colon);          /* e.g. END-2 */
    minus1=(minus && colonfirst);  /* e.g. :-3   */
    minus2=(minus && !colon);      /* e.g. A-1   */
    minus3=(minus1 || minus2);
    if (word_int || lab || cur1 || end1 || plus || minus3) {
        i=edline2(label,j,error);
        return i;
    } else return 0;
}


// END DESKTOP (general routines) /////////////////////////////////////////////////





// BEGIN INDEX ////////////////////////////////////////////////////////////////////


static int INDEXmain(void)
{
    int i,j;
    char caller_path[LNAME];

    strcpy(caller_path, edisk); /* save current datapath */

    i=INDEXcheck_parameters(); if (i<0) return -1;

    GV.groups=0;
    j=spfind("GROUPING");
    if (j>=0) {
        GV.groups=split(spb[j], GV.group_types, GRPTYPS);
    }
    GV.only_grouptypes=0;
    j=spfind("ONLY"); /* 14.7.95/kv (SM) */
    if (j>=0) {
        if (atoi(spb[j])==1) {
            GV.only_grouptypes=1;
            if (GV.groups==0) {
                muste_kv_s_err("Use ONLY with GROUPING! (See INDEX?)");
                return -1;
            }
        }
    }
    count_date_time_length();
    i=INDEXget_fileinfo_from_R(); if (i<0) return -1;
    if (i==0) {
        if (etu==2) {
            tut_error(1);
        } else {
            if (GV.only_grouptypes) {
                muste_kv_s_disp("\nNo files found from %s (", GV.filespec);
                for (i=0; i<GV.groups; i++) {
                    if (!i) muste_kv_s_disp("*.%s",GV.group_types[i]);
                    else muste_kv_s_disp(",*.%s",GV.group_types[i]);
                }
                muste_kv_s_disp(")!"); WAIT;
            } else {
                muste_kv_s_err("No files found (%s)!", GV.filespec);
            }
        }
        return -1;
    }

    INDEXsort_files();
    INDEXprintout();
    if (no_cd) strcpy(edisk, caller_path); /* restore datapath */
    free(files);
    return 1;
}

static int INDEXcheck_parameters(void)
{
    if (g>1) { // at least one parameter given
        if (!strcmp(word[1],"?")) {
            muste_kv_usage_info();
            return -1;
        }
        if (g>2) { // two parameters, e.g. INDEX *.EDT CUR+2
            results_line=kv_edline(word[2],1,0);
            if (!results_line) {
                muste_kv_s_err("Invalid edit line %s given!", word[2]);
                return -1;
            }
            strcpy(GV.filespec, word[1]);
        } else { // one parameter: e.g. INDEX CUR+2  _or_  INDEX *.EDT
            results_line=kv_edline(word[1],1,0);
            if (!results_line) { // not a line - maybe a path?
                results_line = r1+r-1+1; // default line is then CUR+1
                strcpy(GV.filespec, word[1]);
            } else { // was a line - filespec is then * from current path
                sprintf(GV.filespec, "%s%s", edisk, "*");
            }
        }
    } else { // no parameters (apply the both defaults mentioned above)
        if (indexdir) {
            muste_kv_s_err("Usage: DIR <pathname>"); /* like in SURVO 98 & 84C */
            return -1;
        }
        results_line = r1+r-1+1;
        sprintf(GV.filespec, "%s%s", edisk, "*");
    }
    return 1;
}

static int INDEXget_fileinfo_from_R(void)
{
    int i,j,k;
    time_t mtime;
    unsigned int order_nr;
    char *p;
    char Rcmd[LLENGTH];
    SEXP Robj0=R_NilValue;
    SEXP Robj1=R_NilValue;
    SEXP Robj2=R_NilValue;
    SEXP Robj3=R_NilValue;
    SEXP Robj4=R_NilValue;
    SEXP Robj5=R_NilValue;

    muste_kv_s_disp("\nListing files %s...", GV.filespec);
    sprintf(Rcmd,".muste.desktop.fileinfo.INDEX(\"%s\")", GV.filespec);
    muste_evalr(Rcmd);

    Robj0 = findVar(install(".muste.tmp.filecount"), R_GlobalEnv);
    GV.filecount = INTEGER(Robj0)[0];
    if (GV.filecount==0) return 0;

    GV.selected=0; // all files ("*") from the given path
    Robj0 = findVar(install(".muste.tmp.selected"), R_GlobalEnv);
    GV.selected = INTEGER(Robj0)[0]; // only some files (e.g. "*.C")

    GV.dircount=0; GV.bigglen=0; GV.bytes=0; GV.print_filetype=1;
    j=spfind("TYPES");
    if (j>=0) GV.print_filetype=atoi(spb[j]);

    files=(Files *)malloc((size_t)GV.filecount*sizeof(Files));
    if (files==NULL) { no_mem(); return -1; }

    Robj1 = findVar(install(".muste.tmp.dirname")  ,R_GlobalEnv);
    Robj2 = findVar(install(".muste.tmp.basename") ,R_GlobalEnv);
    Robj3 = findVar(install(".muste.tmp.filisdir") ,R_GlobalEnv);
    Robj4 = findVar(install(".muste.tmp.filesize") ,R_GlobalEnv);
    Robj5 = findVar(install(".muste.tmp.filetime") ,R_GlobalEnv);

    sprintf(path, "%s/%s", CHAR(STRING_ELT(Robj1,0)), CHAR(STRING_ELT(Robj2,0)) );

    sprintf(Rcmd,"setwd(\"%s\")", path);
    muste_evalr(Rcmd);
    p=muste_getwd();
    if (p!=NULL) strcpy(edisk,p);

    for (i=0, fi=&files[0], order_nr=1; i<GV.filecount; i++, fi++) {
        strncpy(fi->path, CHAR(STRING_ELT(Robj1,i)), LNAME);
        strncpy(fi->name, CHAR(STRING_ELT(Robj2,i)), LNAME);
        fi->isdir = INTEGER(Robj3)[i];
        fi->size = INTEGER(Robj4)[i];
        mtime = (time_t)INTEGER(Robj5)[i];
        write_time = localtime(&mtime);
        if (write_time == NULL) {
            fi->year   = 0;
            fi->month  = 0;
            fi->day    = 0;
            fi->hour   = 0;
            fi->minute = 0;
            fi->second = 0;
        } else {
            fi->year   = write_time->tm_year;
            fi->month  = write_time->tm_mon+1;
            fi->day    = write_time->tm_mday;
            fi->hour   = write_time->tm_hour;
            fi->minute = write_time->tm_min;
            fi->second = write_time->tm_sec;
        }
        fi->status=0x00;
        fi->printed = 0;
        fi->get_comments = 0;
        fi->notype = 0;

        if (SubDirectory(fi)) {
            GV.dircount++;
            strcpy(fi->type, "");
            strcpy(fi->cmd, "CD");
            fi->order = 1;
        } else {
            p = strrchr(fi->name, '.');
            if (p != NULL) {
                strncpy(fi->type,++p,TYPELEN);
            } else {
                strcpy(fi->type, "");
            }
            strcpy(fi->cmd, "SHOW");
            fi->order = ++order_nr;
        }

        j=restrictions(); if (j<0) return -1;
        if (j) { // some restriction was found:
            fi->status=(fi->status | FDELETED); // lazy deletion
            continue;
        }

        if (GV.only_grouptypes) {
            if (SubDirectory(fi)) { // no subdirs when ONLY group types:
                fi->status=(fi->status | FDELETED); // lazy deletion
                continue;
            }
            for (j=0; j<GV.groups; j++) {
                if (!strcmp(fi->type,GV.group_types[j])) break;
            }
            if (j==GV.groups) { // did not match any of the group types:
                fi->status=(fi->status | FDELETED); // lazy deletion
                continue;
            }
        }

        if (strlen(fi->type)) {
            j=spfind(fi->type);
            if (j>=0) strcpy(fi->cmd, spb[j]); // cmd given by usr, e.g. EDT=LOAD
            // check through the standard types & commands:
            for (k=0, ty1=&types[0]; k<TypeCount1; k++, ty1++) {
                if (!muste_strcmpi(fi->type, ty1->type)) { // types match
                    if (j<0) strcpy(fi->cmd, ty1->cmd); // default (no cmd was given)
                    fi->get_comments=ty1->get_comments;
                    fi->notype=ty1->notype;
                    break;
                }
            }
            for (j=0; j<strlen(fi->cmd); j++) if (fi->cmd[j]=='_') fi->cmd[j]=' ';
        }

        // bookkeeping of the lengths of the commands etc.
        k=strlen(fi->cmd);
        k+=min((8+1),strlen(fi->name));
        if (GV.print_filetype) {
            k+=min(3,strlen(fi->type));
        } else {
            if (!fi->notype) k+=min(3,strlen(fi->type));
            else k--; /* the point will be also removed, if type omitted */
        }
        k++; /* 1st space also! */
        if (k > GV.bigglen) GV.bigglen=k; /* update the longest counter */
        if (fi->size > GV.bytes) GV.bytes=fi->size; /* biggest file? */
    }

    sprintf(Rcmd,".muste.desktop.fileinfo.INDEX.cleanup()");
    muste_evalr(Rcmd);

    return 1;
}

static void INDEXsort_files(void)
{
    int i,count;

    fi=&files[0];
    count = GV.filecount;
    qsort((void *) fi, (size_t)count, NFILES, comp1);
    fi=&files[GV.dircount]; /* keep the directories first, sort the rest */
    count = GV.filecount - GV.dircount;
    i=spfind("SORT");
    if (i>=0) {
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
    int i,j,k,dtlen;
    char *tmp3[2];
    char typ[TYPELEN], bytes[CMDLEN];
    int the_rest, printcount, found_some, width;

    GV.list_to_field=1;
    sprintf(outfile, "%sINDEX.TMP", etmpd);
    j=spfind("OUTFILE");
    if (j>=0) {
        GV.list_to_field=0;
        strncpy(path, spb[j], LNAME);
        muste_standardize_path(path);
        if (strchr(path, '/') != NULL) {
            strcpy(outfile, path);
        } else {
            strcpy(outfile, edisk);
            strcat(outfile, path);
        }
    }
    output_file = muste_fopen(outfile, "w");
    if (output_file == NULL) {
        muste_kv_s_err("Could not open output file %s!", outfile);
        return;
    }

    width=min(c3,ed1-1);
    j=spfind("WIDTH");
    if (j>=0) width=atoi(spb[j]);
    if ((width<0) || (width>MAXWIDTH)) {
        muste_kv_s_err("Width %d is out of range!", width);
        return;
    }
    muste_itoa(GV.bytes,bytes,10); /* convert biggest to length of biggest */
    GV.biggest=strlen(bytes);

    GV.print_size=1;
    j=spfind("SIZE"); if (j>=0) GV.print_size=atoi(spb[j]);
    dtlen=count_date_time_length(); /* made to use same code 21.7.98 */
    GV.required=GV.bigglen+3; /* longest command+file + " / " */
    GV.required+=dtlen; /* GV.datetime_str length */
    if (GV.print_size) GV.required+=GV.biggest+1; /* biggest size + " " */
    GV.commlen=width-GV.required;
    if ((GV.commlen<0) || (GV.commlen>MAXWIDTH)) GV.commlen=0;
    GV.comments_to_left=1; /* default - the original format */
    j=spfind("COMMENTS");
    if (j>=0) {
        i=split(spb[j],tmp3,2);
        if (!strcmp(tmp3[0], "LEFT")) GV.comments_to_left=1;
        else if (!strcmp(tmp3[0], "RIGHT")) {
            GV.comments_to_left=0; /* new option: comments on the right side */
            GV.commlen=c2-GV.required; /* c2==edit line length */
        } else GV.commlen=atoi(tmp3[0]); /* desired length was given */
        if (i>1) { /* i==2 */
            GV.commlen=atoi(tmp3[1]); /* desired length was given */
            if ((GV.commlen<0) || (GV.commlen>MAXWIDTH)) {
                muste_kv_s_err("Comment length %d is out of range!", GV.commlen);
                return;
            }
        }
    }
    bare_format=0;
    if(!GV.print_size &&!GV.print_date &&!GV.print_time &&!GV.commlen) bare_format=1;
    stats_format=0;
    full_format=0;

    j=spfind("FORMAT");
    if (j>=0) {
        if (strstr(spb[j],"BARE") != NULL) {
            bare_format=1;
            GV.print_size=0;
            GV.print_date=0;
            GV.print_time=0;
            GV.commlen=0;
        }
        if (strstr(spb[j],"STATS") != NULL) {
            stats_format=1;
        }
        if (strstr(spb[j],"FULL") != NULL) {
            full_format=1;
            GV.required+=strlen(edisk);
            GV.bigglen+=strlen(edisk);
            GV.commlen-=strlen(edisk);
        }
    }
    GV.required+=GV.commlen;
    if (GV.required>MAXWIDTH) GV.commlen-=(GV.required-MAXWIDTH);

    printcount=0; the_rest=0;
    for (i=-1; i<=GV.groups; i++) { /* -1 for printing only directories! */
        if (i==-1) {
            if (GV.dircount==0) continue;
            if (GV.only_grouptypes) continue; // skip dirs when ONLY=1 (Muste)
            if (GV.selected) continue; // skip dirs if selected files (Muste)
        } else {
            if (i<GV.groups) {
                strncpy(typ, GV.group_types[i], TYPELEN);
            } else {
                the_rest=1;
                if (GV.only_grouptypes) break; /* 14.7.95/kv (SM), kept 8.3.96 */
            }
        }
        found_some=0;
        for (j=0, fi=&files[0]; j<GV.filecount; j++, fi++) {
            if (SubDirectory(fi) && GV.selected) continue; // skip dirs if selected files (Muste)
            if (fi->printed) continue;
            if (FileDeleted(fi)) continue; // lazy deleted are not printed (Muste)
            if (!strcmp(fi->type, typ) || the_rest || SubDirectory(fi)) {
                k=INDEXprint_line();
                if (k) { printcount++; found_some=1; } // dirs off in DIR
            }
        }
        if (found_some) {
            if (printcount<GV.filecount) {
                if (GV.list_to_field) edwrite(space, results_line, 1);
                results_line++;
                fprintf(output_file, "\n");
            }
        }
    }
    fclose(output_file);
    return;
}

static int INDEXprint_line(void)
{
    char bytes[STRMAXL], size_str[STRMAXL];
    char commfile_str[LNAME/2], commfile_tmp[LNAME/2];
    char empty[2];
    char *p;

    if (indexdir) strcpy(fi->cmd,""); else INDEXget_comments();

    if (SubDirectory(fi)) {
        if (indexdir) return 0; /* dirs are not printed at all! (SM&KV 30.1.2001) */
        sprintf(line, "%s %s/%s", fi->cmd, fi->path, fi->name);
    } else {
        if (indexdir) {
            strcpy(empty,"");
        } else {
            strcpy(empty," ");
            if (strlen(fi->cmd)==1) {
                if (!strncmp(fi->cmd, ">", 1) || !strncmp(fi->cmd, "/", 1)) {
                    strcpy(empty,"");
                }
            }
        }
        if (full_format) {
            sprintf(commfile_tmp, "%s%s%s%s", fi->cmd, empty, edisk, fi->name);
        } else {
            sprintf(commfile_tmp, "%s%s%s", fi->cmd, empty, fi->name);
        }
        if (!GV.print_filetype) {
            p = strrchr(commfile_tmp, '.');
            if (p!=NULL) {
                if (fi->notype) *p='\0';
            }
        }
    }

    if (!SubDirectory(fi)) {
        if (!strncmp(fi->cmd, ">", 1) && !indexdir) {
            sprintf(line, "%-*s", GV.bigglen, commfile_tmp);
        }

        if (bare_format) {
            strcpy(commfile_str, commfile_tmp);
            if (stats_format || full_format) strcat(commfile_str, " / ");
        } else {
            if (indexdir) {
                sprintf(commfile_str, "%-12s ", commfile_tmp);
            } else {
                sprintf(commfile_str, "%-*s / ", GV.bigglen, commfile_tmp);
            }
        }

        strcpy(size_str, "");
        if (GV.print_size) {
            muste_itoa(fi->size,bytes,10); /* size to a string */
            sprintf(size_str, "%*s ", GV.biggest, bytes);
        }

        INDEXmake_date_and_time(); /* into GV.datetime_str */

        if (indexdir) {
            sprintf(line, "%s%s%s", commfile_str, size_str, GV.datetime_str);
        } else {
            if (stats_format) {
                sprintf(line, "%s%s", commfile_str, GV.comment_str);
            } else {
                if (GV.comments_to_left) sprintf(line,"%s%-*s %s%s",
                    commfile_str, GV.commlen, GV.comment_str, size_str, GV.datetime_str);
                else sprintf(line,"%s%s%s%s",
                    commfile_str, size_str, GV.datetime_str, GV.comment_str);
            }
        }
    }

    if (GV.list_to_field) edwrite (space, results_line, 1);
    else results_line=0;
    if (GV.list_to_field) edwrite (line, results_line, 1);
    fprintf(output_file, "%s\n", line);
    fi->printed=1;
    results_line++;
    if (results_line > ed2) { GV.list_to_field=0; results_line=0; }

    return 1;
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
    for (i=0; i<LNAME/2; i++) GV.datetime_str[i]='\0';
    switch (GV.print_date) {
       case 0: break;
       case 1: sprintf(GV.datetime_str, "%.2d.%.2d.%.2d ", fi->day, fi->month, year);
               break;
       case 2: sprintf(GV.datetime_str, "%.2d/%.2d/%.2d ", fi->day, fi->month, year);
               break;
       case 3: sprintf(GV.datetime_str, "%.2d%.2d%.2d ", fi->day, fi->month, year);
               break;
       case 4: sprintf(GV.datetime_str, "%.2d%.2d%.2d ", year, fi->month, fi->day);
               break;
       case 5: sprintf(GV.datetime_str, "%.2d/%.2d/%.2d ", fi->month, fi->day, year);
               break;
       case 6: sprintf(GV.datetime_str, "%.2d.%.2d.%.2d ", fi->day, fi->month, YYYY);
               break;
       case 7: sprintf(GV.datetime_str, "%.2d/%.2d/%.2d ", fi->day, fi->month, YYYY);
               break;
       case 8: sprintf(GV.datetime_str, "%.2d/%.2d/%.2d ", fi->month, fi->day, YYYY);
               break;
       case 9: sprintf(GV.datetime_str, "%.2d%.2d%.2d ", fi->day, fi->month, YYYY);
               break;
      case 10: sprintf(GV.datetime_str, "%.2d%.2d%.2d ", YYYY, fi->month, fi->day);
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
    if (GV.print_time) strcat(GV.datetime_str, time_str);
}

static void INDEXget_comments(void)
{
    int i;
    char *ch;
    unsigned int ll, ww;
    int len, edt98, edt84;
    char *word[7];
    unsigned int cols, rows, l;
//#define BUFLEN 10*LLENGTH
#define BUFLEN LLENGTH // (buffer defined earlier with LLENGTH...)

    for (i=0; i<LLENGTH; i++) GV.comment_str[i]='\0';
    sprintf(tmp, "%s%s", edisk, fi->name);

    if (stats_format) {
        /* code borrowed from SEARCH... */
        if ((fh=muste_fopen(tmp,"r"))==NULL) return;
        if (muste_fseek(fh, 0L, SEEK_SET)) return;
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
            /* check some codes used in sucro files... 7.3.2001 */ // Muste: octal
            ch=strchr(buffer, '\373'); if (ch!=NULL) return;  // ¹(ascii:8)=373
            ch=strchr(buffer, '\374'); if (ch!=NULL) return;  // ³(ascii:8)=374
            ch=strchr(buffer, '\375'); if (ch!=NULL) return;  // ²(ascii:8)=375
            ch=strchr(buffer, '\376'); if (ch!=NULL) return;  // _(ascii:8) 376
        }
        fh=muste_fopen(tmp,"r");
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
            muste_fseek(fh, (unsigned int)cols, SEEK_SET); /* first row! */
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
        sprintf(GV.comment_str, "#lines=%u #words=%u", ll, ww);
        return;
    }

    if (fi->get_comments && GV.commlen) {
             if (!muste_strcmpi(fi->type, "EDT")) get_edt_comments(GV.comment_str, LLENGTH-1);
        else if (!muste_strcmpi(fi->type, "SVO")) get_svo_comments(GV.comment_str, LLENGTH-1);
        else if (!muste_strcmpi(fi->type, "MAT")) get_mat_comments(GV.comment_str, LLENGTH-1);
        else if (!muste_strcmpi(fi->type, "M"  )) get_mat_comments(GV.comment_str, LLENGTH-1);
        GV.comment_str[GV.commlen]='\0';
    }
}



// END INDEX //////////////////////////////////////////////////////////////////////



#if TOISTAISEKSI_SIVUUTETTU


// BEGIN SEARCH //////////////////////////////////////////////////////////////////


static int SEARCHmain(void)
{
    int i;
    i=parse_arguments(); if (i<0) return -1;
    i=search_files(); if (i<0) return -1;
    return 1;
}

static int parse_arguments(void)
{
    char *str, *line, *p;
    int i,j,first,last,quoted;

    quoted=0;
    j=spfind("SEARCH"); /* new option checked here; 20.10.95 */
    if (j>=0) { /* quotation marks around the given string, new! */
        if ((strstr(spb[j],"QUOTED") != NULL)) quoted=1;
    }

    results_line=r1+r;
    edread(sbuf, r1+r-1); /* strlen(sbuf) will be ed1, e.g. 101 */
    line=strdup(sbuf); /* duplicate sbuf */
    muste_strupr(sbuf);
    str=strstr(sbuf, "SEARCH "); /* strlen("SEARCH ") == 7 */
    first=ed1-strlen(str)+7; /* starting point of the given string */
    for (i=ed1-1; sbuf[i]==' '; i--) ;
    last=i; /* last non-empty character on the line */
    if (last<first) { /* no search string was given */
        muste_kv_usage_info();
        return -1;
    }
    for (i=first, j=0; i<=last; i++, j++) search_string[j]=line[i];
    search_string[j]='\0';
    if (quoted) { /*   e.g. SEARCH   "different things  "  */
        p=search_string;
        while (*p!='\0') {
            if (*p=='"') { p++; break; }
            p++;
        }
        if (*p=='\0') {
            quoted_usage();
            return -1;
        }
        strcpy(sbuf,p);
        last=strlen(sbuf);
        if (sbuf[last-1]=='"') {
            sbuf[last-1]='\0';
        } else {
            quoted_usage();
            return -1;
        }
        strcpy(search_string,sbuf);
        j-=2;
    }

    write_string(space,ScreenWidth,' ',CommandLine,1);
    write_string(" String: ",9,'ê',CommandLine,8); // 7.3.2001 oli ' '
    write_string(search_string,j,'/',CommandLine,17); // 7.3.2001 oli '7'
    return 1;
}

static void quoted_usage(void)
{
    muste_kv_s_err("Usage: SEARCH \"character string\" (SEARCH=QUOTED)");
}

static int search_files(void)
{
    int j, n_paths, show_mod;
    char given_files[LNAME], given_paths[LNAME];
    char *paths[MAX_GIVEN_PATHS];

    retval=1;
    matches=0;
    files_found=0;
    first_matches=0;
    files_total=0;
    lines_total=0;
    for (j=0; j<LNAME/4; j++) match_msg[j]='\0';
    show_command=1;
    j=spfind("SHOW_COMMAND");  // nämä siirretty ennen FILES-tarkistuksia 4.12.2000
    if (j<0) j=spfind("SHOW");
    if (j>=0) show_command=atoi(spb[j]);

 /* (oli: FILES-tutkinta) 15.1.2003 (siirretty alemmaksi) */

    exact_search=0;
    recursive=0;
    ignorecase=0;
    search_first=0; /* 7.4.97 Jokerit SM-97! */
    search_file=0; /* 22.7.1998 */
    search_DD=0; /* 22.7.1998 */
    search_comment=0; /* 27.5.1999 */
    search_shadows=0; /* 25.5.2001 */
    j=spfind("METHOD");
    if (j<0) j=spfind("SEARCH");
    if (j>=0) {
        if (strstr(spb[j],"EXACT") != NULL) exact_search=1;
        if (strstr(spb[j],"DETAILED") != NULL) exact_search=1;
        if ((strstr(spb[j],"RECURSIVE") != NULL) ||
            (strstr(spb[j],"SUBDIRS") != NULL)) {
             recursive=1;
             show_command=2;
        }
        if ((strstr(spb[j],"IGNORECASE") != NULL) ||
            (strstr(spb[j],"NOCASE") != NULL)) {
             ignorecase=1;
             muste_strupr(search_string);
        }
        /* for /HTML-PRINT 7.4.97/kv (exit after 1st match) */
        if (strstr(spb[j],"FIRST") != NULL) search_first=1;
        /* for connecting SEARCH to DD 22.7.1998/kv ! */
        if (strstr(spb[j],"FILE") != NULL) search_file=1;
        if (strstr(spb[j],"DD") != NULL) search_DD=1;
        if (strstr(spb[j],"COMMENT") != NULL) search_comment=1;
        if (strstr(spb[j],"SHADOWS") != NULL) search_shadows=1;
    }
    if (search_DD && search_first) search_DD=0; /* for sure 13.2.2002 */

 /* oli: fullpath etc. 15.1.2003 (siirretty alemmaksi) */

    if (search_shadows) {
        if (show_command==1 || show_command==3) show_command=5;
        else if (show_command==2 || show_command==4) show_command=6;
    } else { /* 5 and 6 only with SEARCH=SHADOWS */
        if (show_command==5) show_command=1;
        if (show_command==6) show_command=2;
    }

    j=spfind("OUTFILE");
    if (j>=0) {
        results_line=0; /* no output to the edit field, if file given! */
        if (strchr(spb[j], ':') != NULL) {
            strcpy(outfile, spb[j]);
        } else {
            sprintf(outfile, "%s%s", edisk, spb[j]);
        }
    } else {
        sprintf(outfile, "%s%s", etmpd, "SRCH.OUT");
    }
    output_file = muste_fopen(outfile, "w"); /* remove any existing file */
    if (output_file == NULL) {
        muste_kv_s_err("Could not open output file %s!", outfile);
        return -1;
    }
    continuous=0; /* default start mode is stepwise */
    j=spfind("RUN_MODE");
    if (j<0) j=spfind("RUN");
    if (j>=0) continuous=atoi(spb[j]);
    if (search_DD) continuous=1; /* 22.7.1998 */

    columns_given=0;
    j=spfind("COLUMNS");
    if (j<0) j=spfind("COLS");
    if (j>=0) {
        char *words[2]; int nr;
        col1=1;
        columns_given=1;
        nr=split(spb[j], words, 2);
        if (nr>0) col1=atoi(words[0]); else columns_given=0;
        if (nr>1) col2=atoi(words[1]); else col2=col1;
        if (col2<col1) col2=col1;
        if ((col1<0) || (col2<0)) columns_given=0;
    }

    len1=strlen(search_string);
    CURSOR_POS(&row, &col);
    for (j=MessageLine; j<BottomLine; j++) { /* 7.3.2001 */
        write_string(space, ScreenWidth, BackGround, j, 1);
    }
    write_string(space, ScreenWidth, BackGround, BottomLine, 1);
    write_string(Bottom, strlen(Bottom), Reverse, BottomLine, 1);

    len0=7+1+7+1+len1+1; /* cursor position during search */
    LOCATE(CommandLine,len0);

    GV.required=15;  /* " nnnnnnnn.ttt " + right end (from DD) */
    init_globals(); /* 22.7.1998 */ /* handles also DATE spec. */

    if (search_DD) { /* 22.7.1998 */
        /* a few lines from DDfind_files(): */
        count=0; biggest=0L;
        GV.dircount=0; GV.filecount=0; GV.totalcount=0;
        FL=(FIPtr)malloc(sizeof(Files));
        if (FL==NULL) { no_mem(); return -1; }
        FL->next=NULL; FLpp=FL;
        found=0;
    }

/* tämä kokonaisuus siirretty tähän ylempää 15.1.2003 (17.1.2003) */

    j=spfind("FILES");               // yleistä s.e. luettelo poluista! 2.1.2003/SM
    if (j>=0) {
        strcpy(given_paths, spb[j]); // given_paths! -> split to given_files (loop)
    } else {
        strcpy(given_paths, "*.*");
    }

    n_paths=split(given_paths,paths,MAX_GIVEN_PATHS);
    for (j=0; j<n_paths; j++) {
        show_mod=0;
        strcpy(given_files, paths[j]);
        if (strchr(given_files, ':') == NULL) {
            sprintf(filespec, "%s%s", edisk, given_files);
        } else {
            strcpy(filespec, given_files);
            if (show_command==1 || show_command==3) {
                show_command++;
                show_mod=1;
            }
        }

        /* ja tämä siihen hieman alempaa (samaa asiaa) */

        /* yksinkertaistettu (fullpath toimii aina)
        if (_fullpath(path, filespec, LNAME-1) == NULL) {
            muste_kv_s_err("Could not make full path! (%s)", given_files);
            return -1;
        }*/

        _fullpath(path, filespec, LNAME-1);
        _splitpath(path, drive, dir, fname, ext);
        strcpy(origname, fname); strcat(origname, ext);
      //strcpy(origdir, dir); // (havaittu turhaksi 26.12.2008 [vars. WHERE])
        strcpy(filespec, path);
        sprintf(fullspec, "%s%s*.*", drive, dir);

           search();                       /* ***ACTION*** begins here! */

        if (show_mod) show_command--;
        if (search_first&&matches) break;
    }

    fclose(output_file);
    if (retval < 0) return -1; /* 22.7.1998 */

    if (search_DD) { /* 22.7.1998 */
        /* a few lines from DDfind_files(): */
        j=DDalloc_files(); if (j<0) return -1;
        DDremove_list();
        count=matches;
        update_globals();
        word[1]=edisk;
        strcpy(info, "SEARCH");
        DDmain();
        return 1;
    }

    muste_itoa(matches, check, 10);
    muste_itoa(files_total, tmp, 10);
    muste_itoa(lines_total, dir, 10);
    j=spfind("TUTSTACK"); /* 2.9.94 - not always write TUTSTACK! */
    if (j>=0) {
        if (atoi(spb[j])==1)
            sprintf(tut_info, "%s@%s@%s@",check,tmp,dir);
    }
    if (!files_found) {
        give_bad_message(NoFiles);
    } else {
        if (matches) {
            if (results_line) write_results(); // 4.12.2000
        } else {
            give_bad_message(NoMatch);
        }
    }
    return retval;
}

static int search(void)       /* recursively called function! */
{
    int i;
    struct _finddata_t loc;
    int h_loc;
    char localdir[LNAME];

    if (sur_kbhit()) retval=display_msg();
    if CANCELED return retval;

    if (recursive) {
        h_loc=_findfirst(fullspec, &loc);
        if (h_loc == -1L) return retval;
        do {
            if (sur_kbhit()) retval=display_msg();
            if CANCELED return retval;
            if (SUBDIR(loc) && REALDIR(loc)) {
                _splitpath(filespec, drive, dir, fname, ext);
                sprintf(filespec, "%s%s%s\\%s",drive,dir,loc.name,origname);
                sprintf(fullspec, "%s%s%s\\*.*", drive, dir, loc.name);
                strcpy(localdir, dir);
                sprintf(bigbuffer, "              Scanning %s...", fullspec);
                write_string(space,ScreenWidth,SearchColor,MessageLine,1);
                write_string(bigbuffer,strlen(bigbuffer),SearchColor,MessageLine,1);
                search();
                if CANCELED return retval;
                sprintf(filespec, "%s%s%s", drive, localdir, origname);
                sprintf(fullspec, "%s%s*.*", drive, localdir);
            }
        } while(!_findnext(h_loc, &loc));
        _findclose(h_loc);
    }

    h_loc=_findfirst(filespec, &loc);
    if (h_loc == -1L) return retval;
    write_string(space, ScreenWidth, Empty, MessageLine, 1);

    do {
        /* fill the appropriate fields in global 'fil' structure */
        fil.size=loc.size;
        fil.time_write=loc.time_write; // moved here from the 'if' below 24.2.2002
        if (search_DD) {
            fil.attrib=loc.attrib; fil.size=loc.size;
            strcpy(fil.name,loc.name);
        }
        i=restrictions(); if (i<0) { retval=i; return -1; }
        if (i) continue; /* 2.1.98 *//* 22.7.1998 */
        files_found=1; /* moved here */
        _splitpath(filespec, drive, dir, fname, ext);
        sprintf(filespec, "%s%s%s", drive, dir, loc.name);
        if (!muste_strcmpi(filespec, outfile)) continue; // 7.3.2001 (!)
        if ((fh=muste_fopen (filespec, "r")) == NULL) continue;
        files_total++;
        if ((fread (check, sizeof(char), 18, fh)) < 18 ) {
            retval=read_any_file(loc.name);
        } else {
            if SVOEDT98 edt98=1; else edt98=0;
            if (SVOEDT || SVOEDT98) retval=read_edt_file(loc.name);
                               else retval=read_any_file(loc.name);
        }
        fclose (fh);
        if (sur_kbhit()) retval=display_msg();
        if CANCELED return retval;
        if SKIPPED continue;
        if (search_first&&matches) return retval; /* 7.4.97 */
        if (search_DD) {
            if (found) {
                i=DDfound_files(2); if (i<0) return -1;
                FLp->match=found; /* 30.10.1998 found==line nr */
                found=0;          /* moved 3 lines down 30.10.1998 */
            }
        }
    } while(!_findnext(h_loc, &loc));
    _findclose(h_loc);
    return retval;
}

static int read_edt_file(char *filename)
{
    unsigned int l, length, cols, rows, l98;
    char *ptr, *ptr2;
    char *wrd[11];
    int ii;
    char *lptr; // 25.5.2001
    int ahead; // 25.5.2001

    if (!edt98) {
        if (sscanf(check, "%s %u %u", tmp, &cols, &rows)==EOF) return retval; /* was -1 */
        muste_fseek(fh, (unsigned int)cols, SEEK_SET); /* first row! */
    } else {
        muste_fseek(fh, 0L, SEEK_SET); /* back to the beginning of file */
        fgets(tmp, LLENGTH-1, fh);
        split(tmp,wrd,10);
        cols=atoi(wrd[4]); rows=atoi(wrd[5]);
    }
    edt_search_msg();
    ahead=0;
    for (l=1; l<=rows; l++) {
        lines_total++;
        if (sur_kbhit()) {
            retval=display_msg();
            if CANCELED return retval;
            if SKIPPED return retval;
        }
        if (!edt98) {
            fread(buffer, sizeof(char), (size_t)cols, fh);
            buffer[cols]='\0';
            *shadow_buffer='\0';
        } else {
            if (ahead) {
                strcpy(buffer, ahead_buffer);
            } else {
                lptr=fgets(buffer, LLENGTH-1, fh);
                if (lptr==NULL) return retval;
            }
            if (feof(fh)) return retval;
            lptr=fgets(ahead_buffer, LLENGTH-1, fh);
            if (ahead_buffer[0]=='S') { // shadow line, enhanced 25.5.2001
                strcpy(shadow_buffer, ahead_buffer);
                if (search_shadows) {
                    for (ii=0; shadow_buffer[ii]!='|'; ii++) ;  ii++;
                    handle_shadow_buffer(ii,strlen(shadow_buffer)-2);
                }
                ahead=0;
            } else {
                *shadow_buffer='\0';
                ahead=1;
            }

            l98=atoi(buffer);
            for (ii=0; buffer[ii]!='|'; ii++) ;  ii++;
            handle_buffer(ii,strlen(buffer)-2); /* CRLF 28.2.1999 */
        }
        if (search_comment) { /* 27.5.1999 */
            ptr=strstr(buffer, "SAVE ");
            if (ptr==NULL) return retval;
            strcpy(sbuf, ptr); strcpy(buffer, sbuf);

            ptr=strstr(buffer, " / ");
            if (ptr==NULL) return retval;
            strcpy(sbuf, ptr); strcpy(buffer, sbuf);
        }
        strcpy(orig_buffer, buffer);
        length=strlen(orig_buffer);
        if (ignorecase) muste_strupr(buffer);
        if (columns_given) handle_buffer(col1,col2);

        if (search_shadows) { /* 25.5.2001 */
            ptr=strstr(shadow_buffer, search_string);
            len2=strlen(shadow_buffer);
        } else {
            ptr=strstr(buffer, search_string);
            len2=strlen(buffer);
        }

        field_ok=1;
        while (ptr!=NULL) {
            len3=strlen(ptr);
            matches++; found=1;
            sprintf(match_msg, "%7u", matches);
            write_string(match_msg, 7, CountColor, CommandLine, 1);
            if (edt98) {
                ii=l98;
            } else {
                ii=l;
            }
            found=ii; /* 30.10.1998 */
            if (!continuous) { /* moved down 7 lines 30.10.1998 */
                sprintf(bigbuffer, "Found in Survo edit file %s:", filespec); /* 7.3.2001 */
                write_string(space,ScreenWidth,FoundColor,MessageLine,1);
                write_string(bigbuffer, strlen(bigbuffer), FoundColor, MessageLine, 1);
                sprintf(bigbuffer, "%6d ",ii); /* 7.3.2001 */
                write_string(bigbuffer, strlen(bigbuffer), LineColor, ShowLine, 1);
                LOCATE(ShowLine,1);
                if (length+7 > ScreenWidth) length=ScreenWidth-7;
                write_string(orig_buffer, length, LineColor, ShowLine, 8);
                ii=len2-len3+1;
                if (columns_given) ii+=col1;
                ptr2=&orig_buffer[ii-1];
                if (ii>ScreenWidth) {
                    write_string("»", 1, BeyondColor, ShowLine, ScreenWidth);
                } else {
                    write_string(ptr2, len1, MatchColor, ShowLine, ii+7);
                }
                if (search_shadows) { /* 25.5.2001 */
                    strcpy(bigbuffer, "Shadow: ");
                    write_string(bigbuffer, strlen(bigbuffer), LineColor, ShowLine+1, 1);
                    LOCATE(ShowLine+1,1);
                    length=strlen(shadow_buffer);
                    if (length+7 > ScreenWidth) length=ScreenWidth-7;
                    write_string(shadow_buffer, length, LineColor, ShowLine+1, 8);
                    ii=len2-len3+1;
                    if (columns_given) ii+=col1;
                    ptr2=&shadow_buffer[ii-1];
                    if (ii>ScreenWidth) {
                        write_string("»", 1, BeyondColor, ShowLine+1, ScreenWidth);
                    } else {
                        write_string(ptr2, len1, MatchColor, ShowLine+1, ii+7);
                    }
                }

            }
            if (!search_DD) { /* 22.7.1998 */
                if (field_ok) {
                    if (!edt98) {
                        update_field(l, orig_buffer, filename);
                    } else {
                        update_field(l98, orig_buffer, filename);
                    }
                }
            }
            retval=display_msg();
            if CANCELED return retval;
            if SKIPPED return retval;
            if (search_first&&matches) return retval; /* 7.4.97 */
            if (search_file) return retval; /* 22.7.1998 */
            if (search_DD) { /* 22.7.1998 */
                strcpy(com_str, orig_buffer);
                return retval;
            }
            if (exact_search) {
                ptr++;
                if (search_shadows) { /* 25.5.2001 */
                    strcpy(shadow_buffer, ptr);
                    ptr=strstr(shadow_buffer, search_string);
                } else {
                    strcpy(buffer, ptr);
                    ptr=strstr(buffer, search_string);
                }
                field_ok=0;
            } else {
                ptr=NULL;
            }
            if (search_comment) return retval; /* 27.5.1999 */
            edt_search_msg();
        }
        if CANCELED return retval;
        if (!edt98) muste_fseek(fh, cols, (int)ftell(fh));
    }
    return retval;
}

static void edt_search_msg(void)
{
    write_string(space, ScreenWidth, SearchColor, MessageLine, 1);
    sprintf(bigbuffer, "Searching in Survo edit file %s...", filespec);
    write_string(bigbuffer, strlen(bigbuffer), SearchColor, MessageLine, 1);
}

static void any_search_msg(void)
{
    write_string(space, ScreenWidth, SearchColor, MessageLine, 1);
    sprintf(bigbuffer, "Searching in text file %s...", filespec);
    write_string(bigbuffer, strlen(bigbuffer), SearchColor, MessageLine, 1);
}

static int display_msg(void)
{
    retval=0;
    if ((!continuous) || sur_kbhit()) {
        LOCATE(BottomLine,1);
        write_string(space, ScreenWidth, BackGround, BottomLine, 1);
        write_string(Bottom, strlen(Bottom), Reverse, BottomLine, 1);
        retval=nextch("");
        if CANCELED {
            write_string(space, ScreenWidth, Empty, ShowLine, 1);
            return retval;    /* stops at any time! */
        }
        if (!search_DD) { /* 22.7.1998 */
            if (retval==CODE_EXEC) continuous=1-continuous;
        }
        LOCATE(row,col);
    }
    write_string(space, ScreenWidth, Empty, ShowLine, 1);
    if (search_shadows) { /* 25.5.2001 */
        write_string(space, ScreenWidth, Empty, ShowLine+1, 1);
    }
    LOCATE(CommandLine,len0);
    return retval;
}

static int read_any_file(char *filename)
{
    int i, ii, length, co1, co2;
    unsigned int l; // was line 7.3.2001 */
    char *ch, *ptr, *ptr2;

    if (search_comment) return retval; /* 27.5.1999 */
    if (search_shadows) return retval; /* 25.5.2001 */

    if (columns_given) {
        co1=col1; co2=col2;
        if (col1) {
            co1=col1-1;
            if (col2) co2=col2-1;
        }
    }
    l=0;
    sprintf(bigbuffer, "              Checking %s...", filespec);
    write_string(space, ScreenWidth, SearchColor, MessageLine, 1);
    write_string(bigbuffer, strlen(bigbuffer), SearchColor, MessageLine, 1);
    if (muste_fseek(fh, 0L, SEEK_SET)) { /* return to the beginning */
        LOCATE(row,col);
        muste_kv_s_err("Read error occurred in file %s! (muste_fseek)", filespec);
        return retval; /* was -1 */
    }
    if (fgets(buffer, LLENGTH-1, fh) == NULL) {
        if (feof(fh)) return retval; /* was -1 */ /**/
        LOCATE(row,col);
        muste_kv_s_err("Read error occurred in file %s! (fgets)", filespec);
        return retval; /* was -1 */
    }
    ch=strchr(buffer, '\n');
    if (ch==NULL) return retval; /* was -1 *//* no line feed at all -> no text file */

    length=strlen(buffer);
    if (length==LLENGTH-2) return retval; /* was -1 *//* line feed but not text file */

    /* check some codes used in sucro files... 7.3.2001 */

    ch=strchr(buffer, '\373'); if (ch!=NULL) return retval; // ¹(ascii:8)=373
    ch=strchr(buffer, '\374'); if (ch!=NULL) return retval; // ³(ascii:8)=374
    ch=strchr(buffer, '\375'); if (ch!=NULL) return retval; // ²(ascii:8)=375
    ch=strchr(buffer, '\376'); if (ch!=NULL) return retval; // _(ascii:8) 376

    any_search_msg();
    while(!feof(fh)) {
      lines_total++;
      if (sur_kbhit()) {
        retval=display_msg();
        if CANCELED return retval;
        if SKIPPED return retval;
      }
      l++;
      strcpy(orig_buffer, buffer);
      if (ignorecase) muste_strupr(buffer);
      if (columns_given) handle_buffer(co1, co2);
      len2=strlen(buffer);
      ptr=strstr(buffer, search_string);
      field_ok=1;
      length=strlen(orig_buffer);
//muste_kv_s_err("file=%s|, ptr=%s|, length=%d|",filename,ptr,length);
      orig_buffer[length-1]='\0';     /* remove CR LF */
      length--;
      while (ptr!=NULL) {
        len3=strlen(ptr);
        matches++; found=l; /* 30.10.1998 */
        sprintf(match_msg, "%7u", matches);
        write_string(match_msg, 7, CountColor, CommandLine, 1);
        if (!continuous) {
       // sprintf(bigbuffer, "In text file %s at line %u:", filespec, l);
          sprintf(bigbuffer, "Found in text file %s on line %u:", filespec, l); /* 7.3.2001 */
          write_string(space, ScreenWidth, FoundColor, MessageLine, 1);
          write_string(bigbuffer, strlen(bigbuffer), FoundColor, MessageLine, 1);
          LOCATE(ShowLine,1);
          if (length > ScreenWidth) length=ScreenWidth;
          write_string(orig_buffer, length, LineColor, ShowLine, 1);
          ii=len2-len3+1;
          if (columns_given) ii+=co1;
          ptr2=&orig_buffer[ii-1];
          if (ii>ScreenWidth) {
              write_string("»", 1, BeyondColor, ShowLine, ScreenWidth);
          } else {
              write_string(ptr2, len1, MatchColor, ShowLine, ii);
          }
        }
        if (!search_DD) { /* 22.7.1998 */
            if (field_ok) update_field(l, orig_buffer, filename);
        }
        retval=display_msg();
        if CANCELED return retval;
        if SKIPPED return retval;
        if (search_first&&matches) return retval; /* 7.4.97 */
        if (search_file) return retval; /* 22.7.1998 */
        if (search_DD) { /* 22.7.1998 */
            strcpy(com_str, orig_buffer);
            com_str[strlen(com_str)-1]='\0';
            return retval;
        }
        if (exact_search) {
            ptr++;
            strcpy(buffer, ptr);
            ptr=strstr(buffer, search_string);
            field_ok=0;
        } else {
            ptr=NULL;
        }
        any_search_msg();
      }
      if CANCELED return retval;
      if (fgets(buffer, LLENGTH-1, fh) == NULL) {
        if (feof(fh)) return retval; /* was -1 */
        muste_kv_s_err("Read error occurred in file %s! (fgets)", filespec);
        return retval; /* was -1 */
      }
    } /* end-while !feof */
    return retval;
}

static void update_field(unsigned int counter, char *str, char *filename)
{
    first_matches++;
      /* 4.12.2000 options 3 (& 4) for /SEARCH and /HSEARCH */
      /* 25.4.2001 options 5 (& 6) for SEARCH=SHADOWS */
    switch(show_command) {
        case 0: if (search_shadows) { /* 25.5.2001 */
                    sprintf(bigbuffer, "%s\n%s", str, shadow_buffer);
                } else {
                    strcpy(bigbuffer, str);
                }
                break;
        case 1: sprintf(bigbuffer, "SHOW %s,%u / %s", filename, counter, str);
                break;
        case 2: sprintf(bigbuffer, "SHOW %s,%u / %s", filespec, counter, str);
                break;
        case 3: sprintf(bigbuffer, "SHOW %s,%u\n%s", filename, counter, str);
                break;
        case 4: sprintf(bigbuffer, "SHOW %s,%u\n%s", filespec, counter, str);
                break;
        case 5: sprintf(bigbuffer, "SHOW %s,%u / %s\n%s", filename, counter, str, shadow_buffer);
                break;
        case 6: sprintf(bigbuffer, "SHOW %s,%u / %s\n%s", filespec, counter, str, shadow_buffer);
                break;
    }
    fprintf(output_file, "%s\n", bigbuffer);
}

static void write_results(void)
{
    unsigned int written, needed;
    int i, j, answ, maxnewlines;
    char shadow[LLENGTH];
    char *p;
    int no_shadow_lines;

    output_file = muste_fopen(outfile, "r");
    if (output_file == NULL) {
        muste_kv_s_err("Could not open output file %s!", outfile);
        return;
    }
    written=0; /* no line written to the field yet */
    no_shadow_lines=0; /* assume first that there is enough (when SHOW: 5/6) */

    write_string(space, 7, Empty, CommandLine, 1);
    sprintf(bigbuffer,
       " %u occurence%s found (%u file%s scanned, %u lines read).",
       matches, (matches>1) ? "s" : "",
       files_total, (files_total>1) ? "s" : "",
       lines_total);
    write_string(space, ScreenWidth, FinalColor, MessageLine, 1);
    write_string(bigbuffer, strlen(bigbuffer), FinalColor, MessageLine, 1);

    while (1) { /* 22.7.1998 */
        if (fgets(buffer, LLENGTH, output_file) == NULL) {
            if (feof(output_file)) return;
            muste_kv_s_err("Read error occurred in output file %s!", outfile);
            return;
        }
        if (results_line > ed2) {
            give_bad_message(NotEn2);
            return;
        }
        buffer[strlen(buffer)-1]='\0';     /* remove CR LF */
        edread(bigbuffer, results_line);

        if (!empty_line(bigbuffer+1, c2)) {
            if (exact_search) needed=first_matches-written;
            else              needed=matches-written;
            maxnewlines=ed2-lastline2();
            if (needed > maxnewlines) {
                give_bad_message(NotEn2);
                return;
            }
            sprintf(sbuf, NotEn1, needed);
            LOCATE(BottomLine, strlen(sbuf)+1);
            write_string(space, ScreenWidth, Reverse, BottomLine, 1);
            write_string(sbuf, strlen(sbuf), Reverse, BottomLine, 1);
            answ=sur_getch();
            if (answ=='Y' || answ=='y') {
                for (i=lastline2(); i>=results_line; i--) {
                    j=i+needed;
                    edread(sbuf,i);
                    if(zs[i]) { /* line has a shadow line - move it */
                        edread(shadow,zs[i]);
                        edwrite(space,zs[i],0);
                        shadow_test(i);
                        shadow_create(j);
                        edwrite(shadow,zs[j],0);
                    } else { /* no shadow line => no shadow line */
                        if (zs[j]) {
                            edwrite(space,zs[j],0);
                            shadow_test(j);
                        }
                    }
                    edwrite(sbuf,j,0);
                    edwrite(space,i,0);
                }
            } else {
                return; /* no new lines wanted */
            }
        }
        edwrite("*", results_line, 0);
        edwrite(buffer, results_line, 1);

        if (search_shadows) { /* 25.5.2001 */
            fgets(shadow_buffer, LLENGTH, output_file);
            if (no_shadow_lines) {
                ;
            } else {
                shadow_buffer[strlen(shadow_buffer)-1]='\0';
                if (show_command==0) {
                    i=1;
                } else {
                    p=buffer; i=1; while (*p!='/') { p++; i++; } i++; i++;
                }
                if (zs[results_line]) shadow_test(results_line);
                j=shadow_create(results_line);
                if (j<0) {
                    no_shadow_lines=1; /* don't try this anymore */
                } else {
                    edwrite(space,zs[results_line],0);
                    edwrite(shadow_buffer,zs[results_line],i);
                }
            }
        }

        written++;
        results_line++;
    }
    fclose(output_file);
}

static void give_bad_message(char *msg)
{
    /* 3.3.1999 - no messages and sur_getch's in sucro mode */
    if (etu>0) return;
    write_string(space, ScreenWidth, Empty, MessageLine, 1);
    write_string(space, ScreenWidth, Empty, BottomLine, 1);
    write_string(space, ScreenWidth, Reverse, BottomLine, 1);
    strcpy(sbuf,msg);
    write_string(sbuf, strlen(sbuf), Screaming, BottomLine, 1);
    LOCATE(BottomLine, strlen(sbuf)+1);
    sur_getch();
}

static void handle_buffer(int first, int last)
{
    int i,j;
    int len=strlen(buffer); /* 28.2.1999 */
    if (first > len) { *buffer='\0'; return; }
    strcpy(sbuf,buffer);
    for (i=first, j=0; (i<=last && j<LLENGTH); i++, j++)
      buffer[j]=sbuf[i];
    buffer[j]='\0';
}

static void handle_shadow_buffer(int first, int last) // 25.5.2001
{
    int i,j;
    int len=strlen(shadow_buffer);
    if (first > len) { *shadow_buffer='\0'; return; }
    strcpy(sbuf,shadow_buffer);
    for (i=first, j=0; (i<=last && j<LLENGTH); i++, j++)
      shadow_buffer[j]=sbuf[i];
    shadow_buffer[j]='\0';
}

// END SEARCH /////////////////////////////////////////////////////////////////////


#endif // TOISTAISEKSI_SIVUUTETTU

#if TOISTAISEKSI_SIVUUTETTU


// BEGIN DD //////////////////////////////////////////////////////////////////


static int DDmain(void)
{
    int i;

    whstart=0;
    if (g>1) whstart=WhereStart; /* 14.4.96 */
    if (whstart==0) where_first=0; /* 13.2.2002 (set to 1 by WHERE if FIRST) */
    if (g>1) sestart=SEARCHStart; /* 22.7.1998 */
    if (g>1) dmstart=DMStart; /* 17.1.1999 */

    if (g>1) { /* check possible help calls already here */
        if (!strcmp(word[1],"?")) {
            muste_kv_usage_info();
            return 1;
        }
    }

    GV.required=15;  /* " nnnnnnnn.ttt " + right end */
    GV.status=GNORMAL;
    sprintf(GV.filespec, "%s%s", edisk, "*.*");
    init_globals();
    edread(userline,r1+r-1);     /* command line saved */
    if (g>3) { /* started from TREE: DD <path> TREE <origpath> */
        if (!strcmp(word[2],"TREE")) strcpy(userline,info);
    }
    dirmagic();
    edwrite(userline,r1+r-1,0);  /* command line restored */

    if (TreeMode) { /* 30.4.97 */
        char *ploc;

        strcpy(info,userline); /* send original userline to TREE */
        /* send original directory as word[4] */
        ploc=strrchr(D->files,'\\');
        i=ploc-D->files;
        strncpy(sbuf,D->files,i+1);
        sbuf[i+1]='\0';
        sprintf(answer,"WHERE /TREE %s DD %s",edisk,sbuf);
        free_list(); /* moved from dirmagic(), Tree above used D ! */
        free(df);
        g=split(answer,word,5);
        tree();
    } else {
        free_list(); /* moved here from dirmagic(), Tree above used D ! */
        free(df);
    }
    return 1;
}

static void dirmagic(void)
{

    int i,min_required,r_was,r1_was,rv,any;

    df=(FIPtr *)malloc((size_t)(ddROWS+1)*sizeof(FIPtr)); /* only once */
    if (df==NULL) { no_mem(); return; }

    D=DLAlloc(); if (D==NULL) { no_mem(); return; }
    D->prev=NULL; D->next=NULL; DLast=D; /* header node */
    strcpy(D->files,GV.filespec);       /* includes the */
    strcpy(D->sorting,GV.sorting);     /* original */
    strcpy(D->grouping,GV.grouping);  /* settings! */
    if (g>3) { /* started from TREE: DD <path> TREE <origpath> */
        if (!strcmp(word[2],"TREE")) strcpy(D->files,word[3]);
    }

    if (g>1) {
        if (!whstart) strcpy(GV.filespec,word[1]); /* wh: 14.4.96 */
    }
    i=DDcheck_given_path(GV.filespec,1); if (i<0) return;

    r1_was=r1; r1=r1+r-1; r_was=r; r=1; s_end(siirtop); /* move window */
    min_required=GV.required;

    any=0; rv=0; /* 1.1.98 to return to origdir in err.situations */
    handle_dirlist(READ);
    while (1) {
        if (sestart) { /* 22.7.1998 */ /* files were read in SEARCH */
            GV.status=(GV.status | GWHERE); /* change to WhereMode */
        } else {
            if (!whstart) { /* 14.4.96 */
                any=DDfind_files();
                if (any==0 && etu==2) { /* 28.8.2000 */
                    tut_error(11);
                    break;
                }
                if (any>=0) { DDsort_files(); DDgroup_order_files(); }
            }
        }
        if (any>=0) {
            do {
                rv=DDdisplay_files();
                GV.required=min_required;
            } while (WhereMode && !(rv==CODE_REF||rv==CODE_EXIT||rv<0||TreeMode));
            free(files);
        }
        if (TreeMode) break;
        if (rv==CODE_REF) break; /* exit and stay in this dir */
        if (rv==CODE_EXIT || rv<0 || any<0) strcpy(GV.filespec,D->files);
        DDcheck_given_path(GV.filespec,1);
        if (rv==CODE_EXIT || rv<0 || any<0) break; /* exit back home */
        strcpy(GV.sorting,D->sorting);
        strcpy(GV.grouping,D->grouping);
    }
    handle_dirlist(WRITE);
    r1=r1_was; r=r_was; s_end(siirtop); /* restore window */
}

static DLPtr DLAlloc(void) { return ((DLPtr)malloc(sizeof(DirListNode))); }

static void free_list(void)
{
    Dp=DLast->prev; if (Dp==NULL) { free(D); return; }
    while (Dp!=D) { free(Dp->next); Dp=Dp->prev; }
    if (Dp->next!=NULL) free(Dp->next);
    if (Dp!=NULL) free(Dp);
}

static void handle_dirlist(const int code) /* either READ or WRITE */
{
    char DefaultDataPath[LNAME];
    FILE *dlf;
    int i,j;
    const int UpLimit=94; /* /23: 2*22, /48: 2*47 rows */ /* ACTUALLY anything..! */
    char *word[4];

    if (!hae_apu("edisk",DefaultDataPath)) {
        sprintf(DefaultDataPath,"%sD\\",survo_path); /* C:\E\D\ 26.10.96 */
    }
    sprintf(sbuf,"%s%s",DefaultDataPath,DirListFile);
    switch(code) {
      case READ:
           dlf=muste_fopen(sbuf,"r");
           if (dlf==NULL) return;
           while(fgets(sbuf,LLENGTH,dlf)!=NULL) {
             sbuf[strlen(sbuf)-1]='\0'; /* remove CRLF */
             if ((muste_kv_space_split(sbuf,word,4))<4) continue;
             if (!strcmp(word[0],"/")) continue; /* comment line */
             Dp=DLAlloc(); if (Dp==NULL) { no_mem(); return; }
             DLast->next=Dp; Dp->prev=DLast; Dp->next=NULL;
             DLast=Dp; /* last node in the double linked list */
             strcpy(Dp->files,word[0]);
             strcpy(Dp->point,word[1]);
             if (!strcmp(word[2],"-")) word[2][0]='\0';
             if (!strcmp(word[3],"-")) word[3][0]='\0';
             if (!strcmp(word[2],"DOS")) strcpy(Dp->sorting,"OS");
             else if (!strcmp(word[2],"-DOS")) strcpy(Dp->sorting,"-OS");
             else strcpy(Dp->sorting,word[2]);
             strcpy(Dp->grouping,word[3]);
           }
           fclose(dlf);
           break;

      case WRITE:
           dlf=muste_fopen(sbuf,"w");
           if (dlf==NULL) {
              disp_err(" Could not write directory list file %s!",sbuf);
              break;
           }
           Dp=D->next; i=0;
           while(Dp!=NULL) { Dp=Dp->next; ++i; }
           Dp=D->next; j=i-UpLimit;
           while (j > 0) { Dp=Dp->next; --j; }
           fprintf(dlf,"/ Directory list file for DD \n");
        // sprintf(sbuf,"/ %-*s %-13s %-*s %s\n", LNA64-2, "Filespec", // 15.12.2001
           sprintf(sbuf,"/ %s %s %-*s %s\n", "Filespec",
                          "Last file", SORTOPTLEN, "SORT=","GROUPING=");
           fprintf(dlf,sbuf);
           while (Dp!=NULL) {
               if (!strlen(Dp->sorting)) strcpy(Dp->sorting,"-");
               if (!strlen(Dp->grouping)) strcpy(Dp->grouping,"-");
        //     sprintf(sbuf, "%-*s %-13s %-*s %s\n",          // 15.12.2001
        //             LNA64, Dp->files,Dp->point,SORTOPTLEN, // 15.12.2001
               sprintf(sbuf, "%s %s %-*s %s\n",
                       Dp->files,Dp->point,SORTOPTLEN,
                       Dp->sorting,Dp->grouping);
               fprintf(dlf,sbuf);
               Dp=Dp->next;
           }
           fclose(dlf);
           break;
    }
    return;
}

static void dd_keys (void)
{
    init_remarks();
    rem_pr(" Browse by arrow keys, PgDn, PgUp, HOME and END. Also N=Next, P=Prev and");
    rem_pr(" PREFIX down can be used (PREFIX=F2). For action, press:");
    rem_pr("  ENTER to display the contents of current file or directory");
    rem_pr("      L to load another selection of files");
    rem_pr("    +/- to mark/unmark a file (PREFIX+/- to mark/unmark all)");
    rem_pr("      C to copy a file or all marked files to a new destination");
    rem_pr("      M to move a file or all marked files to a new destination");
    rem_pr("      D to delete a file or all marked files or to remove a directory");
    rem_pr("      R to rename a file or a directory");
    rem_pr("      F to display the free disk space information of all disks");
    rem_pr("      S to sort the files (also with specification SORT)");
    rem_pr("      G to group the files (also with specification GROUPING)");
    rem_pr("      A to display and set the file attributes");
    rem_pr("      W to find where your files are");
    rem_pr("      T to climb to the directory tree");
// O puuttuu...
    rem_pr(" alt-F6 to display the variable activations of a Survo data file");
    rem_pr("     F4 to choose a path of the directories visited earlier");
    rem_pr(" alt-F5 to search a string from file names and comments");
    rem_pr("     F1 to display this help (`DD?' to get more help)");
    rem_pr("      > to give a system command");
    rem_pr("     F8 to exit and restore the original path (current path by F7)");
    rem_pr(" ");
    PR_EINV; WAIT;
}

static int DDcheck_given_path (char *fstr, int level) /* level: 1(disk),2(path)*/
{
    int wild,point;
    char *s;
    char fullspec[LNAME];    /* all these local! (see W) 9.6.2000 */
    struct _finddata_t fil;
    int h_fil;

    s=fstr; while(*s==' ') ++s; /* 5.11.96 */
    strcpy(tmp,s);
//  if (!case_sensitive_pathnames) muste_strupr(tmp); // UPPER_NAMES~0/1 (0 by default)
 //
 //    probleema: jos hak. nimessä skandeja, stru-muunnos ei suju - hakemistoa
 //    28.3.2004  ei löydy (ks. da2\bscw...\AnnaK\"Työ" !)  :-/  -> "0" oletukseksi? joo, Musteessa.
 //
    wild=0; point=0;
    if (strchr(tmp, '*') != NULL) wild=1;
    if (strchr(tmp, '?') != NULL) wild=1;
    if (strchr(tmp, '.') != NULL) point=1;
    if (strchr(tmp, ':') != NULL) { /* drive given */
        if (strlen(tmp)==2) strcat(tmp, "*.*\0"); /* only drive given */
        if (wild && !point) strcat(tmp, ".*\0");
        strcpy(fullspec, tmp);
    } else {
        if (wild && !point) strcat(tmp, ".*\0");
        if (tmp[0]!='\\') sprintf(fullspec, "%s%s", edisk, tmp);
        else sprintf(fullspec, "%c:%s", edisk[0], tmp);
    }
    strcpy(tmp,fullspec); tmp[2]='\0'; strcat(tmp, "*.*\0");
    if (_fullpath(path, tmp, LNAME-1) == NULL) {
        DDerror=BADPATH;
        display_error(fstr);
        return -1;
    }
    if (_fullpath(path, fullspec, LNAME-1) == NULL) {
        DDerror=BADPATH;
        display_error(fstr);
        return -1;
    }

    if( path[strlen(path)-1]=='.' && strlen(path) > 1 &&
       (path[strlen(path)-2]=='\\' || path[strlen(path)-2]=='.') )
           strcat(path,"\\");
    _splitpath(path, drive, dir, fname, ext);

    // 16.2.2006:
//printf("\ncgp (1) path=|%s|, drive=|%s|, dir=|%s|, fname=|%s|, ext=|%s|",
//                  path,drive,dir,fname,ext);
    sprintf(sbuf, "%s%s", drive, dir);
//  WAPI_ShortPath(sbuf); // jos nimessä skandeja, ei riitä; po. UPPER_NAMES=0 !
    sprintf(path, "%s%s%s", sbuf, fname, ext);
    _splitpath(path, drive, dir, fname, ext);
//printf("\ncgp (2) path=|%s|, drive=|%s|, dir=|%s|, fname=|%s|, ext=|%s|",
//                  path,drive,dir,fname,ext);
//sur_getch();

    if (level==2) { /* this used to be 'check_path' */
        if (!strlen(ext)) {
            if (strlen(fname)) {
                sprintf(tmp, "%s%s\\",dir,fname);
                strcpy(dir,tmp);
                fname[0]='\0';
            }
        }
        return 1;
    }

    strcpy(fullspec, path);

    if (strlen(ext)==0) {
        if (strlen(fname)==0) { /* no file name - main directory */
            strcat(fullspec, "*.*");
        } else {
            strcat(fullspec, "\\*.*\0");
            strcat(fname, "\\");
            strcat(dir, fname);
        }
        h_fil=_findfirst(fullspec, &fil);
        if (h_fil == -1L) {
            DDerror=NOFILES;
            h_fil=_findfirst(path, &fil);
            if (h_fil == -1L) {
                DDerror=NOFILES;
                if (etu==2) {
                    tut_error(11);
                } else {
//printf("\n [1] NO FILES?!"); sur_getch();
                    display_error(path);
                }
                return -1;
            } else {
                _findclose(h_fil);
            }
        } else {
            _findclose(h_fil);
            strcpy(path, fullspec); /* fixed fullspec ok; use it */
        }
    } else { /* path includes all parts, also extension */
        h_fil=_findfirst(path, &fil);
        if (h_fil == -1L) {
            DDerror=NOFILES;
            if (etu==2) {
                tut_error(11);
            } else {
//printf("\n [2] NO FILES?!"); sur_getch();
                display_error(path);
            }
            return -1;
        } else {
            _findclose(h_fil);
            if (strchr(path, '*') != NULL) wild=1;
            if (strchr(path, '?') != NULL) wild=1;
            if (!wild) {
                if (SUBDIR(fil)) {
                    strcat(path, "\\*.*\0");
                    _splitpath(path, drive, dir, fname, ext);
                }
            }
        }
    }

    if (DDchange_disk(drive,dir)<0) return -1;
    strcpy(GV.filespec,edisk);
    if (!strlen(ext)) strcat(GV.filespec,"*.*");
    else { strcat(GV.filespec,fname); strcat(GV.filespec,ext); }

    return 1;
}

static int DDchange_disk (char *drive, char *dir)
{
    unsigned int len;
    sprintf(tmp, "%s%s", drive, dir);
    len=strlen(tmp);
    if ((tmp[len-1]=='\\') && (len>2+2)) tmp[len-1]='\0';
    if (_chdir(tmp)) {
        disp_err(" Can't change to directory %s!", tmp);
        return -1;
    }
    _getcwd(tmp, LNAME-1);
    muste_strupr(tmp);
    strcpy(edisk, tmp);
    if(edisk[strlen(edisk)-1]!='\\') strcat(edisk,"\\");
    headline(""); /* to get the new dir name straight up there! */
    return 1;
}

static void DDsort_files (void)
{
    int i,desc;
    size_t num;
    int (*comp)(const void *, const void *);

    comp=comp1; desc=(GV.sorting[0]=='-' || GV.sorting[1]=='-');

         if(strstr(GV.sorting,"OS") !=NULL) comp=(desc)?comp1b:comp1;
    else if(strstr(GV.sorting,"DATE")!=NULL) comp=(desc)?comp2b:comp2;
    else if(strstr(GV.sorting,"TIME")!=NULL) comp=(desc)?comp2b:comp2;
    else if(strstr(GV.sorting,"SIZE")!=NULL) comp=(desc)?comp3b:comp3;
    else if(strstr(GV.sorting,"NAME")!=NULL) comp=(desc)?comp4b:comp4;
    else if(strstr(GV.sorting,"TYPE")!=NULL) comp=(desc)?comp5b:comp5;
    else if(strstr(GV.sorting,"MARK")!=NULL) comp=(desc)?comp9b:comp9;
    else if(strstr(GV.sorting,"COMM")!=NULL) comp=(desc)?comp10b:comp10;
    else {
        if (strlen(GV.sorting)) { /* errorneous option */
          disp_err(" Sorting option %s is unknown!", GV.sorting);
          for (i=0; i<SORTOPTLEN; i++) GV.sorting[i]='\0';
        }
    }

    fi=&files[0];
    qsort((void *)fi, GV.filecount, NFILES, comp1); /* all by  OS */
    fi=&files[0];
    qsort((void *)fi, GV.dircount, NFILES, comp4); /* dirs by NAME */

    if (comp==comp1 || comp==comp1b) {
        if (desc) strcpy(GV.sorting,"-"); else strcpy(GV.sorting,"");
        strcat(GV.sorting,"OS");
        if (comp==comp1) return;
    }
    num=(desc) ? 5 : 4;
    GV.sorting[num]='\0';
    fi=&files[0];
    if (WhereMode) {
        num=GV.filecount;
    } else {
        qsort((void *)fi, GV.dircount, NFILES, comp);
        fi=&files[GV.dircount];
        num=GV.filecount-GV.dircount;
    }
    qsort((void *)fi, num, NFILES, comp);
    return;
}

static void DDgroup_order_files(void)
{
    int i,j,k,the_rest,groups;
    char typ[TYPELEN];
    char *grouptypes[MAXGROUPS];
    char given_grouping[GRPOPTLEN];
    char *x;

    strcpy(given_grouping,GV.grouping);
    groups=split(given_grouping,grouptypes,MAXGROUPS);
    strcpy(typ,"..."); k=0; the_rest=0;
    for (i=-1; i<=groups; i++) {
        if (i==-1 && GV.dircount==0) continue;
        if (i>=0 && i<groups) {
            strcpy(typ,".");
            strcat(typ, grouptypes[i]);
            muste_strupr(typ);
        }
        if (i==groups) the_rest=1;
        for (j=0, f=&files[0]; j<GV.filecount; j++,f++) {
            if (FileSorted(f)) continue;
            if (f->hidden_comment) strcpy(sbuf,f->comment); // 5.4.2001
            else strcpy(sbuf,f->name);
            muste_strupr(sbuf);
            x=strstr(sbuf,typ);
//muste_kv_s_err("DD(sort): ***typ=%s   sbuf=%s x=%s",typ,sbuf,x);
            if ((x!=NULL && !strncmp(x,typ,3))
                        || the_rest || SubDirectory(f)) {
                f->grouporder=k++;
                f->status=(f->status | FSORTED);
            }
        }
    }
    if (groups>0) {
        size_t num,dirs;
        fi=&files[0];
        if (WhereMode) {
            num=(size_t)GV.filecount;
        } else {
            dirs=(size_t)GV.dircount;
            qsort((void *)fi, dirs, NFILES, comp6);
            fi=&files[dirs];
            num=(size_t)GV.filecount-dirs;
        }
        qsort((void *)fi, num, NFILES, comp6);
    }
}

static void DDinternal_sort(void)
{
    fi=&files[0];
    qsort((void *)fi, (size_t)GV.totalcount, NFILES, comp7);
    fi=&files[0];
    DDsort_files(); DDgroup_order_files();
}




   /* Muste: restrictions -funktio & co siirretty alkuun (INDEX ym. tarvitsevat)! */


static void DDget_comments(char *str, int len, FIPtr FIp) /* called by W.C */
{
    char *xx;

    fi=FIp; /* use global fi in this compiland (W not yet needs it) */
    if (WhereMode) sprintf(tmp,"%s%s",fi->path,fi->name);
    else sprintf(tmp,"%s%s",edisk,fi->name);

    if (len>0) {
           if (strstr(fi->name,".EDT")!=NULL) get_edt_comments(str,len);
      else if (strstr(fi->name,".SVO")!=NULL) get_svo_comments(str,len);
      else if (strstr(fi->name,".MAT")!=NULL) get_mat_comments(str,len);
      else if ((xx=strstr(fi->name,".M"))!=NULL) {
          if (strlen(xx)==strlen(".M")) get_mat_comments(str,len);
      }
    }
    str[len]='\0'; /* cut the comment string */
}

static int DDf_where(void)
{
    int i;
    char disks[LNAME/8], partpath[LNAME]; // changed 15.12.2001
    char *s;

    where_outfile=0;
    if (whstart) { /* 14.4.96 */ /* started from WHERE, via O.C "(m='W')" */
        strcpy(answer,info);
        where_outfile=1;
        whstart=0;
    } else {
        init_prompt();
        prompt(" File(s) to find ? ",answer,LNA64-5);
    }
    muste_strupr(answer);
    if (!strlen(answer)) return 1;

    i=0; s=answer; count=0; order_nr=1; biggest=0L;
    GV.dircount=0; GV.filecount=0; GV.totalcount=0;
    clear_screen();
    if (strchr(answer, ':')!=NULL) {
        while (*s!=':') { disks[i++]=*s; s++; }
    } else {
        disks[i++]=edisk[0];
    }
    if (strchr(answer, '.')==NULL) {
        if ((strchr(answer, '*')==NULL) && (strchr(answer, '?')==NULL))
            strcat(answer,".");
    }
    disks[i]='\0'; i=0;
    while (*s!='\0') {
        if (*s==':') {
            s++;
            if (*s!='\\') partpath[i++]='\\'; /* add '\' if not there */
        }
        partpath[i++]=*s; s++;
    } partpath[i]='\0';
    free(files);
    FL=(FIPtr)malloc(sizeof(Files));
    if (FL==NULL) { no_mem(); return -1; }
    FL->next=NULL; FLpp=FL;

    LOCATE(WORKROW,1); PR_EINV;
    WhiteBottomRow;
    strcpy(tmp, " Interrupt by '.' ");
    write_string(tmp,strlen(tmp),' ',BOTTOMROW,1);
    s=disks; stopped=0;
    GV.status=(GV.status | GWHERE);
    while (*s!='\0') { /* all given disks */
        filespec[0]=*s; filespec[1]=':'; filespec[2]='\0'; s++;
        strcat(filespec, partpath);
        i=DDcheck_given_path(filespec,2); if (i<0) return -1;

 // 18.1.2007
 //     sprintf(tmp, "%s%s", drive,dir); /* 13.2.2002 (all shortpath changes here) */
 //     WAPI_ShortPath(tmp);
 //     sprintf(filespec, "%s%s%s%s", drive,dir,fname,ext);
 //     sprintf(filespec, "%s%s%s", tmp,fname,ext);
 //?    _splitpath(filespec, drive, dir, fname, ext); // jäi turhaksi 26.12.2008 (!)

        strcpy(origname, fname); strcat(origname, ext);
      //strcpy(origdir, dir); // havaittu täysin turhaksi 26.12.2008
        sprintf(fullspec, "%s%s*.*", drive, dir);

        // 26.12.2008: ratkaisu: TÄHÄN alun perin POLKU OIKEIN; sitten alkoi toimia!
        sprintf(filespec,"%s%s%s", drive, dir, origname);

//CLS;
//printf("\nA: filespec=%s",filespec);
//printf("\n   fullspec=%s",fullspec);
//printf("\n    origdir=%s",origdir); // havaittu täysin turhaksi 26.12.2008
//printf("\n   origname=%s",origname);
//WAIT;

        i=whereis(); if (i<0) return -1;
        if (stopped) break;
    }
    if (where_outfile) fclose(output_file); /* 15.10.2005 */
    ReversedWorkRow;
    if (count==0) {
//printf("\n count==0: where_first=%d",where_first); sur_getch();
        if (where_first) where_first=-1; /* 13.2.2002 (indicated no matches) */
        else disp_err(" No files found (%s)!",answer);
        return WHERE;
    }
    i=DDalloc_files(); if (i<0) return -1;
    DDremove_list();
    update_globals();
    return WHERE;
}

static int whereis(void)
{
    int i;
    char localdir[LNAME]; // changed 15.12.2001
    struct _finddata_t fil0;
    int h_fil0;

    WhiteWorkRow;
    sprintf(sbuf," Searching %s... ",fullspec);
    write_string(sbuf,strlen(sbuf),Advice,WORKROW,1);

    h_fil0=_findfirst(fullspec, &fil0);
    if (h_fil0 == -1L) return 1;
    do {
        if (sur_kbhit()) { sur_getch(); stopped=1; break; } /* 13.9.97 */
        if (SUBDIR(fil0) && REALDIR(fil0)) {

//CLS;
//printf("\n1) filespec=%s",filespec);
//printf("\n   fullspec=%s",fullspec);
            _splitpath(filespec, drive,dir,fname,ext);
//printf("\n1) filespec=%s (drive=|%s| dir=|%s| fname=|%s| ext=|%s|)",
//             filespec,drive,dir,fname,ext);
//printf("\n   fullspec=%s",fullspec);
 //         sprintf(tmp, "%s%s", drive,dir);
 //         WAPI_ShortPath(tmp); /* problem: "My Webs" etc. tried w. no result 24.3.2004 */
//printf("\nShortPath: |%s|",tmp); // short version of "My Webs" is "My Webs" :-(
//WAIT;
                            // here we are again: 16.1.2007 with "Reader 8.0" (should manage, >8!)
 //         sprintf(filespec, "%s%s%s", tmp,fname,ext);
 //         _splitpath(filespec, drive, dir, fname, ext);

            sprintf(filespec, "%s%s%s\\%s",drive,dir,fil0.name,origname);
            sprintf(fullspec, "%s%s%s\\*.*", drive,dir,fil0.name);
            strcpy(localdir, dir);

//printf("\n2) filespec=%s",filespec);
//printf("\n   fullspec=%s",fullspec);
//printf("\n   localdir=%s",localdir);
//WAIT;

            i=whereis(); if (i<0) return -1;
            if (stopped) break;
            sprintf(filespec, "%s%s%s", drive,localdir,origname);
            sprintf(fullspec, "%s%s*.*", drive,localdir);

//printf("\n3) filespec=%s",filespec);
//printf("\n   fullspec=%s",fullspec);
//WAIT;

        }
        if (stopped) break;
    } while(!_findnext(h_fil0, &fil0));
    _findclose(h_fil0);

    if (stopped) return 1;

    h_fil=_findfirst(filespec, &fil);
    if (h_fil == -1L) return 1;
    do {
        /* let's leave the directories there as well, it is useful! */
        if (sur_kbhit()) { sur_getch(); stopped=1; break; } /* 13.9.97 */
        /* for long/short names, we must change edisk! 9.6.2000 */
        /* lots of other things added 13.2.2002 for adequate support of long names */  // -2007 (!)
        DDcheck_given_path(filespec,1);
        i=DDfound_files(1); if (i<0) return -1;
        where_first_found=i;
        if (stopped) break;
    } while(!_findnext(h_fil, &fil));
    if (where_first && where_first_found) stopped=1; /* WHERE: new option WHERE=FIRST */
    _findclose(h_fil);
    return 1;
}

static int DDfound_files(int code) /* 0=Load, 1=Where 2=SEARCH */
{
    int i;
    char *xx;
 // 18.1.2007:
    char *token;
    struct _finddata_t filx;
    int h_filx;

// 23.2.2007: WHERE:ssä (*.* !!) tänne päätyy "C:\SURVO_MM\EX\P\." josta
//            tulee myöhemmässä vaiheessa     "C:\SURVO_MM\EX\P\P" (virhe!).
//            On poistettava tällainen vaihtoehto, en keksi muuta: (ks. alempana)
//   ei auta, jäljelle jää pelkkä lista ".."-hakemistoja :(
//  printf("\n DDfound_files(%d): filespec=|%s|",code,filespec); sur_getch();
//  i=strlen(filespec);
//  if ((filespec[i-1]=='.') && (filespec[i-2]=='\\')) return 0;
//  CLS;

//printf("\n DDfound_files: fil.name=|%s|",fil.name);
//s d:\u\desktop\gen2.h

    if (code!=2) {
        if (THISDIR(fil)) return 0; /* skip '.' dir */  /* was: 1 13.2.2002 */
        if (UnzipMode) {
            sprintf(sbuf,"%s%s\\",etmpd,TMPDIR);
            if (!muste_strcmpi(edisk,sbuf)) {
                if (PARENTDIR(fil)) return 0; /* skip '..' */ /* was: 1 13.2.2002 */
            }
        }
        if (!UnzipMode) { /* 1.1.1998 */
            i=restrictions(); if (i<0) return -1;
            if (i) return 0; /* restriction found, do not load file  was: 1 13.2.2002 */
        }
    }

    FLp=(FIPtr)malloc(sizeof(Files));
    if (FLp==NULL) { no_mem(); return -1; }
    FLp->next=NULL; FLpp->next=FLp; FLpp=FLp;
    flash_counter(count+1);
    FLp->attrib=fil.attrib;
    FLp->size=fil.size;
    strcpy(FLp->name,fil.name);    /* fil.name: (possibly) a long name */
    strcpy(FLp->longname,fil.name); /* 12.5.2003 */

//  if (!PARENTDIR(fil)) /* added for Win95 */
//    WAPI_ShortPath(FLp->name);  /* FLp->name: short form of the name */

    write_time = localtime(&fil.time_write);
    if (write_time==NULL) { /* 19.12.2000 (alihakemistot cd:illä!) */
        FLp->year   = 0;
        FLp->month  = 0;
        FLp->day    = 0;
        FLp->hour   = 0;
        FLp->minute = 0;
        FLp->second = 0;
    } else {
        FLp->year   = write_time->tm_year; /* current year - 1900 */
        FLp->month  = write_time->tm_mon+1;
        FLp->day    = write_time->tm_mday;
        FLp->hour   = write_time->tm_hour;
        FLp->minute = write_time->tm_min;
        FLp->second = write_time->tm_sec;
    }
    FLp->status=0x00;
    FLp->grouporder=0; /* will be updated in sort.c */
    FLp->command=SHOW; /* default for unknown types */
    if (SUBDIR(fil)) {
        GV.dircount++;
        FLp->order=1;
        FLp->command=DIRMOVE;
    } else {
        FLp->order=++order_nr;
        if (FLp->size > biggest) biggest=FLp->size;
        for (i=0,ty2=ftypes; i<TypeCount2; i++,ty2++) {
            strcpy(sbuf,FLp->name); muste_strupr(sbuf);
            if ((xx=strstr(sbuf,ty2->type))!=NULL) {
                if (strlen(xx)==strlen(ty2->type)) {
                    FLp->command=ty2->command;
                    break;
                }
            }
        }
    }
 // if (code==1 || code==2) { /* 1=Where 2=SEARCH */

// 18.1.2007
// Polku saattaa olla sekamuodossa "C:\PROGRA~1\ADOBE\Reader 8.0\Reader",
// jolloin WAPI_Shortpath EI muuta
// (vaikka pitäisi) sitä muotoon   "C:\PROGRA~1\ADOBE\Reader~1.0\Reader"
// INDEXin puolella on oma rutiinini tällaiseen; muokkaan sen tähän:
// SHOW D:\U\DESKTOP\INDEX\CHECK.EDT,183
// "noniin" perkele! Sainpas toimimaan, ja samalla paljon koodia pois! :) //
// Toimii WHERE, muttei DD-selailussa... nyt sekin! YES! YES! YES!

    if (code==0) sprintf(filespec, "%s%s", edisk,FLp->name); // DD-selailu mukaan!

// printf("\n A) filespec=|%s|",filespec);

// 23.2.2007: WHERE:ssä tänne päätyy "C:\SURVO_MM\EX\P\." josta tulee
//            myöhemmässä vaiheessa  "C:\SURVO_MM\EX\P\P" joka on virhe.
//            On poistettava tällainen vaihtoehto, en keksi muuta: ks. edellä!
//
//  i=strlen(filespec);
//  if ((filespec[i-1]=='.') && (filespec[i-2]=='\\')) return 0; // what happens??
//
//  happens: ohittuu virhetilanne, mutta struktuurit yllä täytetty -> kaatuu!
//  saatava siis kiinni jo aiemmin. Tässä siis ongelmana "*.*"-haut, unlike Reader.
//




//#if 0 //  tämä ilmeisesti suljettava pois *.*-hauista?
      // (testattava, oliko alunperin filename == *.* tms.) kokeilin jo...
//if (strcmp(origname,"*.*")) { // siis EI "*.*"-tilanteissa:
// vaan eipä sitten toimi *.* Adobe-hakemistossa!! PRKL
// TEE TÄMÄ JOS stringissä blankok!
if (strchr(filespec,' ')!=NULL) { // JEP, tämä näyttää toimivan! (25.2.2007)
                                  // jotain outoa zip-selailussa (jossa direjä),
                                  // paha sanoa liittyykö näihin ongelmiin. ?

    token=strtok(filespec,"\\");
    sprintf(tmp, "%s\\", token);
    sprintf(sbuf, "%s\\", token);
//printf("\n 0 tmp = |%s|", tmp);
//printf("\n 0sbuf = |%s|\n",sbuf);
    token=strtok(NULL,"\\");
    while (token!=NULL) {
        strcat(tmp, token);
        h_filx=_findfirst(tmp, &filx);
   /*?*/   if (h_filx == -1L) break;
//printf("\n *** filx.name=|%s|\n",filx.name);
   /*?*/   if (strcmp(filx.name, ".")==0) break;
        strcat(sbuf, filx.name); // kerätään pitkää nimeä!
//printf("\n 1 tmp = |%s|", tmp);
//printf("\n 1sbuf = |%s|\n",sbuf);
        _findclose(h_filx);
        strcat(tmp,"\\");
        token=strtok(NULL,"\\");
        if (token!=NULL) strcat(sbuf,"\\");
    }

//printf("\n 2 tmp = |%s|", tmp);
//printf("\n 2sbuf = |%s|\n",sbuf);
//   WAPI_ShortPath(sbuf);       // muutetaan lopulta AITO pitkä lyhyeksi!
// printf("\n Esbuf = |%s|",sbuf); //sur_getch();
     strcpy(filespec,sbuf);
} // #endif
// printf("\n B) filespec=|%s|",filespec); sur_getch();

// ...sitten jatkuu vanhalla koodilla:

        _splitpath(filespec, drive, dir, fname, ext);

//printf("\n    filespec=%s",filespec);
        sprintf(sbuf,"%s%s",drive,dir);
        sbuf[PATHLEN-1]='\0';
//printf("\n[1] sbuf=|%s|",sbuf); //sur_getch();
//      WAPI_ShortPath(sbuf); // 15.10.2005   (tuli turhaksi 18.1.2007)
//printf("\n[2] sbuf=|%s|",sbuf); sur_getch();
        strcpy(FLp->path, sbuf);
        strcpy(edisk, sbuf); // päivitys lyhyeen muotoon (DD)! 18.1.2007
 // }

//  printf("\nedisk päivitetty: edisk=%s",edisk); sur_getch();

    if (where_outfile) { /* 15.10.2005 */
        sprintf(tmp, "%s%s", FLp->path, FLp->name);
//printf("\n[1] tmp=|%s|",tmp); sur_getch();
//      WAPI_ShortPath(tmp);                  (tuli turhaksi 18.1.2007)
//printf("\n[2]tmp=|%s|",tmp); sur_getch();
        fprintf(output_file, "%s\n", tmp);
    }
    if (code==2) {
         ; /* comments are saved straight to com_str in SEARCH */
    } else {
        i=ScreenWidth-GV.required;
        strcpy(com_str, space); /* replace the above 2.1.98 */
        DDget_comments(com_str, i, FLp); /* max.comment length, ptr */
        while (i>0) { i--; if (com_str[i]!=' ') break; } /* remove sp's */
        com_str[++i]='\0'; /* i++; */
//muste_kv_s_err("\n1) com_str=|%s|",com_str);
    }
    /* 2.1.1998: space is now allocated always, so why not use it? */
    if (!strcmp(com_str, " ")) { /* enhanced 5.4.2001 */
        sprintf(com_str, "%s", fil.name); // WhereMode: see ->path!
        if (!strcmp(fil.name,FLp->name)) FLp->hidden_comment=2;  /* >0: includes  */
        else FLp->hidden_comment=1;                              /*     filename! */
//muste_kv_s_err("\n2) com_str=|%s|",com_str);
    } else { /* user-given comment is always used */   /* see O.C which uses this */
        FLp->hidden_comment=0;
    }
    strcpy(FLp->comment, com_str);
//  if (!case_sensitive_pathnames) muste_strupr(FLp->name);
    FLp->match=0; /* 30.10.1998, see SEARCH and O2 (SHOW) */
    count++;
    return 1;
}

static int DDfind_files(void)
{
    int i;

    count=0; order_nr=1; biggest=0L;
    GV.dircount=0; GV.filecount=0; GV.totalcount=0;
    h_fil=_findfirst(GV.filespec, &fil);
    if (h_fil == -1L) return 0;

    FL=(FIPtr)malloc(sizeof(Files));
    if (FL==NULL) { no_mem(); return -1; }
    FL->next=NULL; FLpp=FL;
    do {
        i=DDfound_files(0); if (i<0) return -1;
    } while(!_findnext(h_fil, &fil));
    _findclose(h_fil);
    i=DDalloc_files(); if (i<0) return -1;
    DDremove_list();
    update_globals();
    return count;
}

static int DDalloc_files(void)
{
    if (count==0) return 1; /* 1.1.98 (restrictions!) */
    files=(Files *)malloc((size_t)count*sizeof(Files));
    if (files==NULL) { no_mem(); return -1; }
    return 1;
}

static void DDremove_list(void)
{
    FLp=FL->next; fi=&files[0];
    while (FLp!=NULL) {
        fi->attrib=FLp->attrib;
        fi->size=FLp->size;
        strcpy(fi->name,FLp->name);
        fi->year=FLp->year;
        fi->month=FLp->month;
        fi->day=FLp->day;
        fi->hour=FLp->hour;
        fi->minute=FLp->minute;
        fi->second=FLp->second;
        fi->status=FLp->status;
        fi->grouporder=FLp->grouporder;
        fi->command=FLp->command;
        fi->order=FLp->order;
        strcpy(fi->path,FLp->path);
        strcpy(fi->comment,FLp->comment);
        fi->hidden_comment=FLp->hidden_comment; /* 2.1.98 */
        fi->match=FLp->match; /* 30.10.1998, see SEARCH and O2 (SHOW) */
        strcpy(fi->longname,FLp->longname); /* 12.5.2003 */
        FLpp=FLp;
        FLp=FLp->next;
        free(FLpp); fi++;
    }
    free(FL);
}

static void update_globals(void)
{
    int len;
    char bytes[LNAME];

    muste_itoa(biggest,bytes,10);
    len=strlen(bytes);
    GV.bigglen=len; if (len<5) len=5;
    GV.required+=len+1; /* biggest file + " " */
                             /* at least length of "<DIR>" !! */
    GV.commlen=ScreenWidth-GV.required;
    GV.filecount=count;
    GV.totalcount=count;
    return;
}

static int DDdisplay_files(void)
{
    int i,m,row,dfi,msgnr;
    unsigned int ui;
    char msg[81]; /* message to the message line */

    row=ddSTARTROW; current=markcount=dfi=0;
    totalbytes=markbytes=0L; msgnr=1; clear_screen();

    if (whstart) GV.filecount=0; /* 14.4.96 */
    if (dmstart) { /* 17.1.1999 */
        i=mark_saved_files_from_DM();
        if (i<0) return -1;
    }
    if (GV.filecount) {
        DDcount_totalbytes();
        if (WhereMode) DDcount_totaldirs();
        if (NormalMode) check_revisiting();
        fi=&files[0];
    }

    while (1) {
        if (GV.filecount) {
            if (revisit) {
                revisit=0; fi=&files[0]; dfi=0; current=0; row=ddSTARTROW;
                while (1) {
                    f=fi; if (LastFile(f)) break; df[dfi]=fi;
                    if (!strcmp(fi->name,revisitfile)) {
                        if (WhereMode) {
                            if (!strcmp(fi->path,revisitpath)) break;
                        } else break;
                    }
                    if (DDf_key(CODE_DOWN)) current=0; /* incr. current */
                    if (row==ENDROW) { row=ddSTARTROW; dfi=0; }
                    else { row++; dfi++; fi++; }
                }
                row=ddSTARTROW;
                if (current>GV.filecount-1) current=GV.filecount-1;
                dfi-=current; fi-=current; if (dfi<0) dfi=0;
                clear_screen(); DDinfoline();
            }
            df[dfi]=fi; f=fi;
            DDget_line(dfi); DDdisplay_line(dfi,row,strlen(line));
        }
        if (EndOfPageCondition) { /* includes GV.filecount==0 !! */
            DDinfoline();
            if (UnzipMode) strcpy(msg,ddMsg2); else strcpy(msg,ddMsg1);
            switch(msgnr) {
              case 1: strcat(msg,ddMsgA); break;
              case 2: if (UnzipMode) strcat(msg,ddMsgB2); /* 1.5.97 */
                      else strcat(msg,ddMsgB1); break;
              case 3: if (UnzipMode) strcat(msg,ddMsgC2);
                      else strcat(msg,ddMsgC1); break;
            }
            if (++msgnr>3) msgnr=1;
            DDshow_mode_code();
            if (where_first_exit) {
                m='W';
            } else if (whstart) {
                m='W'; /* 14.4.96 */
            } else {
                m=nextch(msg);
            }
            ui=toupper((unsigned char)m);
            i=DDhandle_key(ui);
            switch(i) {
              case  0: if (UnzipMode) {
                           if (m==CODE_REF) return CODE_REF; /*stay@tmp*/
                           if (m==CODE_EXIT) { /* return from tmp-dir */
                               GV.status=(GV.status & ~GUNZIP);
                               delete_tempdir(); delete_tempfile();
                           }
                           return CODE_RETURN; /* bluff! */
                       }
                       GV.status=(GV.status & ~GWHERE);
                       return m;  /* returns CODE_* to DD.C */
           case WHERE: GV.status=(GV.status | GWHERE);
// quite complicated, but works... :)
//printf("\n 3) now in DDdisplay_files, case WHERE... ");
//WAIT;
                       if (where_first_exit) {
//printf("\n 5) again in DDdisplay_files, updating tutinfo and returning CODE_EXIT... ");
//WAIT;
                           update_tutstack();
                           m=CODE_EXIT;
                       } else if (where_first) {
//printf("\n 4) still in DDdisplay_files, setting to 1 and returning WHERE to DD.C... ");
//WAIT;
                           where_first_exit=1; /* will exit next time */
                       }
                       return m;
              case -1: return -1; /* returns -1 to DD.C -> exit */
           case TREE:  if (UnzipMode) break; /* 1.5.97 */
                       GV.status=(GV.status | GTREE); /* 30.4.97 */
                       init_prompt(); /* for TREE's start-messages! */
                       i=update_dirlist(); if (i<0) return -1;
                       return m;
              default: break;
            }
            row=ddSTARTROW; dfi=0;
        } else {
            row++; dfi++; fi++;
        }
    }
}

static int DDhandle_key(unsigned int m)
{
    int i;
    unsigned int ui;

    if (GV.filecount) f=df[current];
    switch (m) {
      case CODE_REF:
      case CODE_EXIT:   if (UnzipMode) { /* exit from tmp-directory */
                            if (m==CODE_EXIT)
                                strcpy(GV.filespec, DLast->files);
                            return 0;
                        }
                        if (GV.filecount) {
                            update_tutstack();
                            i=update_dirlist(); if (i<0) return -1;
                        }
                        return 0;
      case CODE_LEFT:   if (GV.filecount) { /* 3.1.1998 */
                            if (files->name[0]=='.') {
                                i=update_dirlist(); if (i<0) return -1;
                                current=0; fi=&files[0];
                                f=fi; df[0]=f;
                            }
                        } /* rest shared with CODE_RIGHT: */
      case CODE_RIGHT:  if (GV.filecount) {
                            if (!SubDirectory(f)) break;
                            if (f->name[0]=='.') { /* dot-dot dir */
                                if (m==CODE_RIGHT) break;
                            } else {
                                if (m==CODE_LEFT) break;
                            }
                        }
                      /* - Rest shared with CODE_RETURN: - */
      case CODE_EXEC: /* ESC is still same as ENTER */
      case CODE_RETURN: if (GV.filecount) {
                            i=DDf_show(m); /* 4.1.1998 for CODE_LEFT */
                            if (i<0) return -1;
                            if (i==DIRMOVE) return 0;
                        }
                        break;
      case WHERE:       if (where_first_exit) return WHERE;
                        if (UnzipMode) { fi=df[0]; return 1; }
   /* 2.7.97/kv(mmv) */ i=update_dirlist(); if (i<0) return -1;
                        i=DDf_where();
                        if (GV.filecount) fi=df[0];
                        return i; /* -1, 1, or WHERE */
      case CODE_HELP:   DDf_help(); break;
      case LOAD:        if (UnzipMode) break;
                        i=DDf_load();
                        if (i<=0) return i;
                        break;
      case COPY:        if (GV.filecount) {
                            if (DDf_copy(1)>0) return 0;
                        }
                        break;
      case CODE_ACTIV:  if (GV.filecount) DDf_act(); break;
      case DELETE:      if (GV.filecount) {
                          if (DDf_delete()) {
                              revisit=1;
                              DDinternal_sort();
                              clear_screen();
                          }
                        }
                        break;
      case RENAME:      if (GV.filecount) DDf_rename(); break;
      case MOVE:        if (GV.filecount) {
                          if (DDf_move()) {
                              revisit=1;
                              DDinternal_sort();
                              clear_screen();
                          }
                        }
                        break;
      case ATTRIB:      if (GV.filecount) DDf_attrib(); break;
      case CODE_UP:     if (GV.filecount) {
                            if (DDf_key(CODE_UP)) {
                                fi=df[0]; f=fi;
                                if (!FirstFile(f)) fi--;
                                return m;
                            }
                        }
                        break;
      case CODE_DOWN:
      case MARK:
      case MARK2:
      case UNMARK:      if (GV.filecount) {
                          switch(m) {
                            case CODE_DOWN: i=DDf_key(CODE_DOWN); break;
                            case MARK:
                            case MARK2:     i=DDf_key(MARK); break;
                            case UNMARK:    i=DDf_key(UNMARK); break;
                          }
                          if (i) {
                              if (!LastFile(f)) fi=df[1]; else fi=df[0];
                              return m;
                          }
                        }
                        break;
      case CODE_HOME:   if (GV.filecount)
                            if (DDf_key(CODE_HOME)) return m;
                        break;
      case CODE_END:    if (GV.filecount)
                            DDf_key(CODE_END);
                        return m;
      case CODE_NEXT:
      case NEXT:        if (GV.filecount)
                            if (DDf_key(CODE_NEXT)) return m;
                        break;
      case CODE_PREV:
      case PREV:        if (GV.filecount)
                            DDf_key(CODE_PREV);
                        return m;
      case FREE:        DDf_free(0); break;
      case SORT:
      case GROUP:       if (GV.filecount) {
                          switch(m) {
                            case SORT:  f_sort(); break;
                            case GROUP: f_group(); break;
                          }
                          mark_files_unsorted();
                          DDsort_files(); DDgroup_order_files();
                          fi=&files[0]; current=0;
                          return m;
                        }
                        break;
      case CODE_DISK:   if (UnzipMode) break;
                        i=DDf_disk();
                        if (i<=0) return i;
                        break;
      case CODE_PRE:    if (GV.filecount) {
                          i=nextch(ddMsgP); /* show PREFIX-message here */
                          ui=toupper((unsigned char)i);
                          switch(ui) {
                            case MARK:
                            case MARK2:     DDf_key(MARKALL); break;
                            case UNMARK:    DDf_key(UNMARKALL); break;
                            case CODE_DOWN: DDf_key(PREDOWN); return m;
 /* 5.11.96 SM-ehd. 'K' */  case COPY:      if (DDf_copy(2)>0) return 0;
                            default:        break;
                          }
                        }
                        break;
// muste_system(char *komento, int wait) / RS 8.7.2011 ks.!
      case SYSTEM:      system_call(); clear_screen();
                        break;
      case CODE_SRCH:   if (GV.filecount) {
                            i=DDf_search();
                            if (i) revisit=1;
                        }
                        break;
      case TREE:        break;
 /* TREE added 30.4.97 - handled in display_files() and dirmagic() */

      case OPEN: /* 28.4.2008 ad hoc - toimi heti! :) */
            /* MUTTA: AIHEUTTAA ONGELMIA (hiiri ei toimi kun DD:stä palataan!) */
                     // filename_to_sbuf();
                     // sprintf(answer,"START %s",sbuf);
                     // system(answer);

 // Päiväys:  	Fri, 9 May 2008 12:03:53 +0300 [12:03 EEST]
 // Lähettäjä:  	Seppo Mustonen <seppo.mustonen@survo.fi>
 // Vastaanottaja:  	Kimmo Vehkalahti <Kimmo.Vehkalahti@helsinki.fi>
 // Otsikko:  	Re: Tapaus avoin (case OPEN)
 // Hei,
 // On ilmeistä, että system()-funktio on jollain tapaa haitallinen
 // nykysurvossa. Kävin läpi suuren osan omaa Survo-koodiani
 // ja kaikissa tapauksisssa system()-kutsut oli kommentoitu pois.
 // Olen vastaavissa tilanteissa (esim. F1 G) kirjoittanut tilapäisesti
 // nykyrivin paikalle >>START ... -komennon, mutta se konsti
 // ei onnistu DD:n sisältä.
 // Heti kun ehdin yritän löytää koodinpätkän, jolla system()
 // korvautuisi myös DD:n tapauksessa.
 // -Seppo

            /* menisikö se siis näin: */

                        filename_to_sbuf();
                        sprintf(answer,"START %s",sbuf);
                     // write_cmd_line();
                     // i=suorita("CMD.EXE"); if (i<0) return -1;
            // >dir c:\windows\SYSTEM32\cmd.exe
            /* toimii! mutta sama vika esiintyy edelleen :( */
                    //  spawnlp(P_NOWAIT,"CMD","CMD","/C",sbuf,NULL);

 // Päiväys:  	Fri, 9 May 2008 15:26:27 +0300 [15:26 EEST]
 // Lähettäjä:  	Seppo Mustonen <seppo.mustonen@survo.fi>
 // Vastaanottaja:  	Kimmo Vehkalahti <Kimmo.Vehkalahti@helsinki.fi>
 // Otsikko:  	Re: Tapaus avoin (case OPEN)
 // Osa(t):  	
 // 	2 	SYSTEM.EDT 	1,15 KB 	Poista liitetiedosto
 // Tallenna kaikki liitetiedostot (.zip tiedostoksi) Tallenna kaikki liitetiedostot
 // Poista kaikki liitetiedostot Poista liitetiedostot
 // 	1 	nimetön 	0,49 KB 	Poista liitetiedosto
 // Tein koemodulin SYSKOE, joka aiheutti saman ongelman,
 // mutta kun lisäsin system()-kutsun eteen rivit
 //
 // sur_free_console();
 // sur_alloc_console();
 //
 // niin hiiri alkoi toimia normaalisti.
 // Liitteenä SYSTEM.EDT jossa tuo lisätty koodi.
 //
 // Tämä melkein arvaamalla löytynyt kikka perustui siihen,
 // että huomasi hiiren alkavan jälleen toimia, jos
 // muutti kentän kokoa esim. RESIZE:lla ja
 // noissa toiminnoissa tarvitaan tuota konsolin vapautusliikettä.
 //
 // Toivottavasti tämä puree DD:ssäkin.
 //
 // -Seppo
 //
 //             _  SHOW SYSTEM.EDT / (Seppo lähetti vielä uuden SURVO.LIB)

                    //  sur_free_console();
                    //  sur_alloc_console();
 //
 //           tuli sivuvaikutuksia! :(
 //

 //  Päiväys:  	Fri, 9 May 2008 18:45:18 +0300 [18:45 EEST]
 //  Lähettäjä:  	Seppo Mustonen <seppo.mustonen@survo.fi>
 //  Vastaanottaja:  	Kimmo Vehkalahti <Kimmo.Vehkalahti@helsinki.fi>
 //  Otsikko:  	Re: Re: Tapaus avoin (case OPEN)
 //  Hei,
 //  Katsoin toistaiseksi parhaaksi ratkaisuksi sen,
 //  että s.exe tunnistaa &DESKTOPin ja virkistää
 //  ikkunan palattaessa siitä, jolloin hiiri näyttäisi
 //  jäävän tomintakykyiseksi.
 //  Väliaikainen s.exe:
 //  http://www.survo.fi/marjut/s.exe
 //  -Seppo

 // asensin, poistin nuo edeltä, nyt siis vain tämä:
// muste_system(char *komento, int wait) / RS 8.7.2011 ks.!
                        system(answer);
                        break;

      default:          break;
    }
    if (GV.filecount) fi=df[0];
    return m;
}

static void DDdisplay_line(int dfi, int row, int len)
{
    if (dfi==current) {
        if (ReadOnlyFile(f) || SystemFile(f) || HiddenFile(f))
            DDdrawline(AttribCursor,row,len);
        else DDdrawline(Cursor,row,len);
        if (WhereMode) strcpy(edisk,f->path);
        // 12.5.2003
        write_string(space,ScreenWidth,' ',WORKROW+1,1);
        sprintf(answer, "%s", f->longname); /* 12.5.2003 */
//      if (strchr(f->name ,'~')!=NULL && f->hidden_comment)
//           sprintf(answer, "%s", f->comment);
//      else sprintf(answer, "%s", f->name);
        write_string(answer,strlen(answer),' ',WORKROW+1,2);
    } else {
        if (ReadOnlyFile(f) || SystemFile(f) || HiddenFile(f))
            DDdrawline(AttribColor,row,len);
        else DDdrawline(FileColor,row,len);
    }
}

static void DDdrawline(char color, int row, int length)
{
    write_string(space,ScreenWidth,color,row,1);
    write_string(line,length,color,row,1);
    if (FileMarked(f)) {
        switch(color) {
          case FileColor:
          case AttribColor:  write_string(MarkStr,1,MarkColor1,row,1);
                             break;
          case Cursor:
          case AttribCursor: write_string(MarkStr,1,MarkColor2,row,1);
        }
    }
}

static void DDinfoline(void)
{
    LOCATE(BOTTOMROW,1);

    if (WhereMode) {
        if (GV.dircount==0) {
            sprintf(line, " %d file%s, %u byte%s.",
              GV.filecount, (GV.filecount!=1)?"s":"",
              totalbytes, (totalbytes!=1L)?"s":"");
        } else {
            sprintf(line, " %d file%s from %d director%s, %u byte%s.",
              GV.filecount, (GV.filecount!=1)?"s":"",
              GV.dircount, (GV.dircount>1)?"ies":"y",
              totalbytes, (totalbytes!=1L)?"s":"");
        }
    } else { /* normal mode or UnzipMode */
        if (GV.dircount==0) {
            sprintf(line, " %d file%s, %u byte%s.",
              GV.filecount-GV.dircount,
              ((GV.filecount-GV.dircount)!=1)?"s":"",
              totalbytes, (totalbytes!=1L)?"s":"");
        } else {
            sprintf(line, " %d file%s + %d director%s, %u byte%s.",
              GV.filecount-GV.dircount,
              ((GV.filecount-GV.dircount)!=1)?"s":"",
              GV.dircount, (GV.dircount>1)?"ies":"y",
              totalbytes, (totalbytes!=1L)?"s":"");
        }
    }

    if (markcount>0) {
        sprintf(answer, " Marked: %d file%s, %u byte%s.",
          markcount, (markcount!=1)?"s":"",
          markbytes, (markbytes!=1L)?"s":"");
        strcat(line, answer);
    }
    ReversedWorkRow;
    write_string(line,strlen(line),Reverse,WORKROW,1);
}


static void DDget_line(int dfi) /* structure enhanced 9.6.2000 */
{
    char datetime[STRMAXL], namesize[LNAME]; // changed 15.12.2001
    int len;
    char *p;

    DDmake_file_name_and_size(namesize);
    DDmake_date_and_time(datetime);

    len=strlen(f->comment);
    if (len > GV.commlen) {
        len-=GV.commlen;
        strcpy(sbuf,"...");
        p=f->comment; p+=(3+len);
        strcat(sbuf,p);
        sbuf[GV.commlen]='\0';
    } else {
        strcpy(sbuf, f->comment);
    }

    if (f->hidden_comment > 1) {
        if (dfi==current /*&& f->name[0]!='.'*/) {
            sprintf(line," %s %s%-*s ",
                namesize,datetime,GV.commlen,sbuf);
        } else {
            sprintf(line," %s %s ", namesize, datetime);
        }
    } else {
        if (f->comment[1]==':') { /* pathname */
            sprintf(line," %s %s%-*s ",
                namesize,datetime,GV.commlen,sbuf);
        } else {
            /* f->comment[GV.commlen]='\0'; 13.2.2002 */ /* user comment or a long file name */
            /* Win98:ssa sotkee, kun loppumerkkiä ei laiteta! (SM 20.2.2002) */
         // sprintf(line," %s %s%-*s ",namesize,datetime, GV.commlen,f->comment);
            /* hoidetaan siis varman päälle toisella tavalla: */
            strcpy(sbuf,f->comment); sbuf[GV.commlen]='\0';
            sprintf(line," %s %s%s ",namesize,datetime,sbuf);
        }
    }
}

static void DDmake_file_name_and_size(char *str)
{
    int i,len;
    char bytes[STRMAXL], size_str[STRMAXL], // changed 15.12.2001
    file_tmp[STRMAXL], file_str[STRMAXL];

    for (i=0; i<STRMAXL; i++) { bytes[i]='\0'; size_str[i]='\0'; }
    len=GV.bigglen; if (len<5) len=5;
    strcpy(file_tmp, f->name);
    sprintf(file_str, "%-12s ", file_tmp);
    if (SubDirectory(f)) {
        sprintf(size_str, "%*s", len, "<DIR>");
    } else {
        if (GV.print_size) { /* 21.7.1998 */
            muste_itoa(f->size,bytes,10); /* size to a string */
            sprintf(size_str, "%*s", len, bytes);
        } else {
            sprintf(size_str, "%*s", len, " ");
        }
    }
    sprintf(str, "%s%s", file_str, size_str);
}

static void update_tutstack(void)
{
    int k;

    k=spfind("TUTSTACK");
    if ((k>=0) && (atoi(spb[k])==1)) {
        if (where_first >= 0) {
            f=df[current];
            if (SubDirectory(f)) {
    /*W1*/      strcpy(tut_info, "D@");
            } else if (strstr(f->name,".EDT")!=NULL) {
    /*W1*/      strcpy(tut_info, "E@");
            } else  {
    /*W1*/      strcpy(tut_info, "F@");
            }
    /*W2*/  strcat(tut_info, f->name);
            strcat(tut_info, "@");     /* 18.3.96 */
    /*W3*/  strcat(tut_info, edisk);   /* ---"--- */
        } else { /* where_first but none found */
            strcpy(tut_info,"");
        }
    }
}

static void delete_tempfile(void)
{
    sprintf(tempfil, "%s%s", etmpd, TMPFILE);
    sur_delete(tempfil);
}


static void delete_tempdir(void)
{

    sprintf(path,"%s%s", etmpd, TMPDIR);
    _chdir(path);
    strcpy(pathtobedeleted, path); // 7.2.2001 (DELTREE)
    strcat(path,"\\*.*");
    delete_recursively();
    _chdir(etmpd);
    _rmdir(path);
}


static void delete_recursively(void)
{
    struct _finddata_t fil0;
    int h_fil0;
    char localdir[LNAME];

    h_fil0=_findfirst(path, &fil0);
    if (h_fil0 != -1L) {
      do {
      if (SUBDIR(fil0) && REALDIR(fil0)) {
        _splitpath(path, drive,dir,fname,ext);
        sprintf(path, "%s%s%s\\*.*",drive,dir,fil0.name);
        strcpy(localdir, dir);
        delete_recursively();
        sprintf(path, "%s%s*.*", drive,localdir);
      }
      } while(!_findnext(h_fil0, &fil0));
      _findclose(h_fil0);
    }

    if (strlen(path) < strlen(pathtobedeleted)) return; // 7.2.2001

    h_fil0=_findfirst(path, &fil0);
    if (h_fil0 == -1L) return;
    do {
        if (SUBDIR(fil0)) continue; /* 6.10.2000 */
        _splitpath(path, drive,dir,fname,ext);
        sprintf(path, "%s%s%s", drive,dir,fil0.name);
        sur_delete(path);
    } while(!_findnext(h_fil0, &fil0));
    _findclose(h_fil0);
    sprintf(path, "%s%s", drive,dir); path[strlen(path)-1]='\0';
    _rmdir(path);
}

static int update_dirlist(void)
{
    Dp=DLAlloc(); if (Dp==NULL) { no_mem(); return -1; }
    DLast->next=Dp; Dp->prev=DLast; Dp->next=NULL;
    DLast=Dp; /* last node in the double linked list */
    strcpy(Dp->files,GV.filespec);
    if (GV.filecount) f=df[current];
    if (GV.filecount) strcpy(Dp->point,f->name); else Dp->point[0]='\0';
    strcpy(Dp->sorting,GV.sorting);
    strcpy(Dp->grouping,GV.grouping);
    return 1;
}

static void DDshow_mode_code(void)
{
    char sign[2];
    if (NormalMode) strcpy(sign,"N");
    if (WhereMode)  strcpy(sign,"W");
    if (UnzipMode)  strcpy(sign,"U");
    write_string(sign,1,Reverse,1,ScreenWidth);
}

static void delete_node_from_list(void)
{
    DLPtr Dtmp;
    Dtmp=Dp; Dp->prev->next=Dp->next;
    if (Dp->next!=NULL) Dp->next->prev=Dp->prev;
    if (Dp==DLast) DLast=Dp->prev; /* 8.6.95 */
//  free(Dtmp); // aiheutti ongelmia MM:ssä, poistettu 18.1.2001
}

static void DDcount_totalbytes(void)
{
    int i;
    for (i=0,fi=&files[0];i<GV.filecount;i++,fi++)
        totalbytes+=fi->size;
}

static void DDcount_totaldirs(void)
{
    int i;

    fi=&files[0];
    strcpy(path,fi->path); fi++;
    GV.dircount=1;
    for (i=1; i<GV.filecount; i++,fi++) {
        if (strcmp(path,fi->path)) {
            GV.dircount++;
            strcpy(path,fi->path);
        }
    }
}

static void check_revisiting(void)
{
    int i;
    char *p;

    revisit=0; Dp=DLast;
    while(Dp!=D && Dp!=NULL) {
        strcpy(sbuf,Dp->files); strrev(sbuf); p=&sbuf[0];
        while(*p!='\\'&&*p!='\0') p++;  strrev(p);
        if (!strcmp(p,edisk)) {
            for (i=0,fi=&files[0]; i<GV.filecount; i++,fi++) {
                if (!strcmp(Dp->point,fi->name)) {
                    revisit=1; strcpy(revisitfile,fi->name);
                    strcpy(GV.sorting,Dp->sorting);
                    strcpy(GV.grouping,Dp->grouping);

                    mark_files_unsorted();
                    DDsort_files(); DDgroup_order_files();

                    delete_node_from_list();
                    break; /* from for() */
                }
            }
            break; /* from while() */
        }
        Dp=Dp->prev;
    }
}

static int mark_saved_files_from_DM(void) /* 17.1.1999 (J.Valtonen-suggestion) */
{
    int i;
    FILE *outf;

    sprintf(sbuf,"%s%s",etmpd,"DM_DD.TMP");
    outf=muste_fopen(sbuf,"r");
    if (outf==NULL) {
        disp_err(" Could not read selections from file %s!",sbuf);
        return -1;
    }
    while(fgets(sbuf,LLENGTH,outf)!=NULL) {
        sbuf[strlen(sbuf)-1]='\0'; /* remove CRLF */
        f=&files[0];
        for (i=0; i<GV.filecount; i++,f++) {
            if (!strcmp(f->name,sbuf)) {
                f->status=(f->status | FMARKED);
                markcount++;
                markbytes+=f->size;
                break;
            }
        }
    }
    fclose(outf);
    return 1;
}

static int op_DELTREE(void) // 6.2.2001 (command DELTREE specially for NT environments)
{
    int i;
    char yesno[2];

    if (g<2) { muste_kv_usage_info(); return -1; }
    i=DDcheck_given_path(word[1], 2); if (i<0) return -1;
    strcpy(pathtobedeleted, path); // 7.2.2001
    if (etu==0) {
        sprintf(answer, "\nDelete %s%s and all its subdirectories? ", drive,dir);
        strcpy(yesno,"N");
        prompt(answer,yesno,1);
        muste_strupr(yesno);
        switch (yesno[0]) {
          case 'Y': break;
           default: return -1;
        }
    }
    delete_recursively();
    return 1;
}

static void DDf_rename(void)
{
    f=df[current];
    if (!strcmp(f->name,"..")) return;
    init_prompt();
    strcpy(answer,f->name);
    sprintf(sbuf," Rename %s to ? ",
        (SubDirectory(f)) ? "directory" : "file");
    prompt(sbuf,answer,12);
 // muste_strupr(answer); // (removed & added next i 5.8.2001)
    if (!muste_strcmpi(answer, f->name)) return;
    if (!strlen(answer)) return;
    filename_to_sbuf();
    if (rename(sbuf,answer)) {
        disp_err(" Can not rename %s to %s!", f->name, answer);
    } else {
        strcpy(f->name, answer);
    }
    return;
}

#if 0
static void DDf_attrib(void)
{
    char attrib_str[5];

    f=df[current];
    if (!strcmp(f->name,"..")) return;
    init_prompt();
    strcpy(attrib_str,"    ");
    if (ArchiveFile(f))  attrib_str[0]='A';
    if (SystemFile(f))   attrib_str[1]='S';
    if (HiddenFile(f))   attrib_str[2]='H';
    if (ReadOnlyFile(f)) attrib_str[3]='R';
    WhiteBottomRow;
    sprintf(answer," Attributes for %s ? ", f->name);
    write_string("(A=Archive,S=System,H=Hidden,R=Readonly)",
                 40,Advice,BOTTOMROW,strlen(answer)+7);
    prompt(answer,attrib_str,4);
    muste_strupr(attrib_str);
    if (strchr(attrib_str,'A')!=NULL) f->attrib |= _A_ARCH;   else f->attrib &= ~_A_ARCH;
    if (strchr(attrib_str,'S')!=NULL) f->attrib |= _A_SYSTEM; else f->attrib &= ~_A_SYSTEM;
    if (strchr(attrib_str,'H')!=NULL) f->attrib |= _A_HIDDEN; else f->attrib &= ~_A_HIDDEN;
    if (strchr(attrib_str,'R')!=NULL) f->attrib |= _A_RDONLY; else f->attrib &= ~_A_RDONLY;
    filename_to_sbuf();
//  WAPI_SetFileAttr(sbuf, f->attrib);
}
#endif

static int DDf_matshow(void)
{
    int i,s_was;
    int j,m,n,lr,lc,type;
    char expr[129];
    char format[20];
    int ol_was; /* 2.1.1999 */
    char buf[LLENGTH]; /* to replace sbuf 27.6.2000 */

    clear_screen(); LOCATE(WORKROW,1);
    WhiteWorkRow;
    filename_to_sbuf();
    strcpy(buf,sbuf);
    sprintf(answer,"Matrix file %s", buf);
    WorkRowText(7);

    s_was=scroll_line; scroll_line=ddSTARTROW;
    s_end(siirtop); /* change scroll line */
    sprintf(tempfil,"%s%s",etmpd,TMPFILE); sur_delete(tempfil);
    ol_was=output_level; /* 2.1.1999 */
    output_level=1;
    j=matrix_load( buf,&A,&m,&n,&rlab,&clab,&lr,&lc,&type,expr);
    if (j<0) return -1;
    matrix_format(format,accuracy,A,m,n);
    matrix_print(A,m,n,rlab,clab,lr,lc,m,n,NULL,NULL,format,c3,0,
                 tempfil,expr);
    output_level=ol_was; /* 2.1.1999 */
    scroll_line=s_was; s_end(siirtop); /* restore scroll line */
    sprintf(answer,"SHOW %s",tempfil);
    write_cmd_line();
    i=suorita("_SHOW.EXE"); if (i<0) return -1;
    sur_delete(tempfil);
    return 1;
}

static int DDf_act(void)
{
    int i;
    init_prompt();
    WhiteWorkRow;
    f=df[current];
    if (f->command == FSHOW) {
        filename_to_sbuf();
        sprintf(answer," loading FILE ACTIVATE for %s...",sbuf);
        WorkRowText(7);
        sprintf(answer,"FILE ACTIVATE %s",sbuf);
        write_cmd_line();
        i=suorita("FI\\CREATE.EXE"); if (i<0) return -1;
        strcpy(active_data,sbuf);
        clear_screen();
    }
    return 1;
}

static int DDf_free(int tutor_caller)
{
    double total_space, avail_space, used_space;
    double free_percent;
    int this_drive, drive;
    char driveletter, root[4];
    FILE *tmpf;
    int i,j;
    int color98; // 25.5.2001

    color98=1; /* my default */
    if (hae_apu("color98",sbuf)) color98=atoi(sbuf);

    if (!tutor_caller) {
        clear_screen();
        DDinfoline();
        LOCATE(WORKROW,1);
    }
    this_drive=_getdrive();

    sprintf(tempfil,"%s%s",etmpd,TMPFILE);
    tmpf=muste_fopen(tempfil, "w");
    if (tmpf==NULL) return -1;

    fprintf(tmpf,".......................................................................\n");
  if (!tutor_caller) {
    fprintf(tmpf,"To create a bar chart of the disk space usage,                         \n");
    fprintf(tmpf,"load these lines, exit DD, and activate GPLOT.                         \n");
    fprintf(tmpf,"                                                                       \n");
  }
    fprintf(tmpf,"DATA FREEDISK                                                          \n");
    fprintf(tmpf,"     Disk         Total         Used         Free                      \n");

    for (drive=1; drive<=26; drive++) {
        driveletter=drive+'A'-1;
        if (driveletter=='A' && edisk[0]!='A') continue;
        if(_chdrive(drive)!=0) continue;
        sprintf(root, "%c:\\", driveletter);
//      i=WAPI_GetDiskFree(root, &total_space, &avail_space);
//      if (i<0) continue;
        used_space=total_space-avail_space;
        free_percent=100.0*(avail_space/total_space);
    fprintf(tmpf, "      %c:   %12.f %12.f %12.f (%2.1f%%)\n",
        driveletter, total_space, used_space, avail_space, free_percent);

    }
    fprintf(tmpf,"                                                                       \n");
    fprintf(tmpf,"GPLOT FREEDISK                    / activate this to create a bar chart\n");
    fprintf(tmpf,"                              VALUES=[SwissB(12)],###.#%%,-.5 VALUEMIN=5\n");
    fprintf(tmpf,"                        VARS=Disk,Used,Free TYPE=%%AVBAR GAP=0.1,0.1,0.1\n");
    fprintf(tmpf,"                  PEN=[Swiss(18)] LINETYPE=[WHITE] NAMES=[SwissB(20)],1\n");
    fprintf(tmpf,"                LEGEND=10,80,1 LEGEND_TEXT=0,10 LEGEND_BOX=100,40,40,10\n");
    fprintf(tmpf,"          YSCALE=0,10:_,20:_,30:_,40:_,50,60:_,70:_,80:_,90:_,100,102:?\n");
    if (color98) {
        fprintf(tmpf,"      XDIV=20,125,5 YDIV=1.5,12,1.5 COLORS=[/BLUE] SHADING=1,2 YLABEL=%%\n");
    } else {
        fprintf(tmpf,"      XDIV=20,125,5 YDIV=1.5,12,1.5 COLORS=[/BLUE] SHADING=2,3 YLABEL=%%\n");
    }
    fprintf(tmpf,"HEADER=[SwissB(25)],___________________________________Disk_space_usage\n");
  if (!tutor_caller) {
    fprintf(tmpf,"                                                                       \n");
    fprintf(tmpf,"This setup is also given by                                            \n");
    fprintf(tmpf,"/FREEDISK                                                              \n");
  }
    fprintf(tmpf,".......................................................................\n");
    fclose(tmpf);

#if 0
.......................................................................
To create a bar chart of the disk space usage,
load these lines, exit DD, and activate GPLOT.

DATA FREEDISK
     Disk         Total         Used         Free
      C:    11503779328   7429662720   4074116608 (35.4%)
      D:    11503779328   7429662720   4074116608 (35.4%)
      E:    11503779328   7429662720   4074116608 (35.4%)
      F:      691046400    176582656    514463744 (74.4%)

GPLOT FREEDISK                    / activate this to create a bar chart
                              VALUES=[SwissB(12)],###.#%,-.5 VALUEMIN=5
                        VARS=Disk,Used,Free TYPE=%AVBAR GAP=0.1,0.1,0.1
                  PEN=[Swiss(18)] LINETYPE=[WHITE] NAMES=[SwissB(20)],1
                LEGEND=10,80,1 LEGEND_TEXT=0,10 LEGEND_BOX=100,40,40,10
          YSCALE=0,10:_,20:_,30:_,40:_,50,60:_,70:_,80:_,90:_,100,102:?
      XDIV=20,125,5 YDIV=1.5,12,1.5 COLORS=[/BLUE] SHADING=2,3 YLABEL=%
HEADER=[SwissB(25)],___________________________________Disk_space_usage

This setup is also given by
/FREEDISK
.......................................................................
#endif

    if (tutor_caller) return 1; /* called from DESKTOP-main */

    sprintf(answer,"SHOW %s",tempfil);
    write_cmd_line();
    WhiteWorkRow;
    strcpy(answer,"Statistics of disk space usage:");
    WorkRowText(7);
    i=suorita("_SHOW.EXE"); if (i<0) return -1;
    sur_delete(tempfil);
    clear_screen();
    return 1;
}

static void DDf_help(void)
{
    init_prompt();
    WhiteWorkRow;
    strcpy(answer," Key codes in Directory Dancer (DD):");
    WorkRowText(1);
    dd_keys();
    clear_screen();
}

static int DDf_key(int key)
{
    int i,maxi,lastfound,page_changed,clr,pg1,pg2;

    switch(key) {
      case CODE_HOME: if (current==0) {
                          fi=&files[0];
                          return 1;
                      }
                      current=0;
                      return 0;
      case CODE_END:  for (i=0; i<2; i++) DDf_key(PREDOWN);
                      break;
      case CODE_NEXT: f=df[current]; fi=f;
                      if (LastFile(f)) return 0;
                      lastfound=page_changed=clr=pg1=0;
                      maxi=min(ddROWS,GV.filecount);

                      for (i=0; i<maxi; f++,fi++,i++) {
                          if (i>current) pg1++;
                          if (LastFile(f)) { lastfound=1; break; }
                      }
                      pg2=i-pg1;
                      page_changed=(current+i) > ddROWS-1;

                      if (lastfound) {
                          if (page_changed) {
                              fi=df[current]; current=0;
                          } else {
                              fi=df[0]; current+=i;
                          }
                      } else {
                          if (page_changed) {
                              for (i=current; i>0; fi--,i--) ;
                              if (pg1+pg2 < current) current=pg1+pg2;
                          }
                      }
                      if (page_changed) {
                          for (i=0,f=fi; i<maxi; f++) {
                              if (LastFile(f)) { clr=1; break; }
                              i++;
                          }
                      }
                      if (clr) clear_screen();
                      return 1;
      case CODE_PREV: f=df[0];
                      for (i=0; i<ddROWS; i++,f--) {
                          if (FirstFile(f)) {
                              if (i>0) clear_screen();
                              current=0;
                              break;
                          }
                      }
                      fi=f;
                      return 1;
      case PREDOWN:   f=df[current]; fi=df[0];
                      if (LastFile(f)) break;
                      maxi=min(ddROWS-1,GV.filecount-1);
                      if (current<maxi) {
                          while (!LastFile(f)) {
                              f++;
                              current++;
                              if (current==maxi) break;
                          }
                      } else {
                          do { f++; fi++; } while (!LastFile(f));
                      }
                      break;
      case CODE_UP:   if (current>0) {
                          current--;
                          return 0;
                      }
                      return 1; /* causes page change */
      case CODE_DOWN: f=df[current];
                      if (LastFile(f)) return 0;
                      if (current<(ddROWS-1) && current<(GV.filecount-1)) {
                          current++;
                          return 0;
                      }
                      return 1; /* causes page change */
      case MARKALL:   markcount=0; markbytes=0L; f=&files[0];
                      for (i=0; i<GV.filecount; i++,f++) {
                          if (!SubDirectory(f)) {
                              f->status=(f->status | FMARKED);
                              markcount++;
                              markbytes+=f->size;
                          }
                      }
                      break;
      case UNMARKALL: f=&files[0];
                      for (i=0; i<GV.filecount; i++,f++)
                          f->status=(f->status & ~FMARKED);
                      markcount=0; markbytes=0L;
                      break;
      case MARK:      f=df[current];
                      if (SubDirectory(f)) return 0;
                      if (!FileMarked(f)) {
                          f->status=(f->status | FMARKED);
                          markcount++;
                          markbytes+=f->size;
                      }
                      return DDf_key(CODE_DOWN);
      case UNMARK:    f=df[current];
                      if (SubDirectory(f)) return 0;
                      if (FileMarked(f)) {
                          f->status=(f->status & ~FMARKED);
                          markcount--;
                          markbytes-=f->size;
                      }
                      return DDf_key(CODE_DOWN);
    }
    return 1;
}

static int DDf_load(void)
{
    int i;

    if (GV.filecount) f=df[current];
    init_prompt();
    if (WhereMode) sprintf(answer,"%s%s",edisk,"*.*");
    prompt(" Files to load ? ",answer,LNA64-3);
    muste_strupr(answer);
    if (!strlen(answer)) return 1;
    i=DDcheck_given_path(answer,2); if (i<0) return 1;
    i=DDchange_disk(drive,dir); if (i<0) return 1;
    if (NormalMode) {
        i=update_dirlist(); if (i<0) return -1;
    }
    strcpy(GV.filespec,path); /* path updated by check_given_path */
    clear_screen();
    return 0; /* causes return-chain to DD.C */
}


static int DDf_show(unsigned int m) /* was void before 4.1.98 */
{
    int i;

    init_prompt();
    ReversedWorkRow;
    f=df[current];
    switch (f->command) {
        case DIRMOVE:
               sprintf(answer,"%s%s",edisk,f->name);
               if (NormalMode && m!=CODE_LEFT) { /* 4.1.1998 */
                   i=update_dirlist(); if (i<0) return -1;
               }
               strcpy(GV.filespec,answer);
               break;
        case FSHOW:
               WhiteWorkRow;
               filename_to_sbuf();
               sprintf(answer," loading FILE SHOW for %s...",sbuf);
               WorkRowText(7);
               sprintf(answer,"FILE SHOW %s",sbuf);
               i=spfind("OPTIONS"); /* 16.11.97 (15.12.97) */
               if (i>=0) {
                   strcat(answer, " / OPTIONS=");
                   strcat(answer, spb[i]);
               }
               write_cmd_line();
               i=suorita("FI\\SHOW.EXE"); if (i<0) return -1;
               break;
#if 0
        case GPLOT:
               filename_to_sbuf();
//             sprintf(answer,"GPLOT FILE %s / %s", sbuf,f->comment); /* comment: MODE=... */
               sprintf(answer,"GPLOT FILE %s ", sbuf);
               write_cmd_line();
               i=suorita("G\\FILE.EXE"); if (i<0) return -1;
  /* FILE.EXE on MM:ssä Windows-ohjelma; ei voi suorittaa */
               break;
#endif
        case OP_EMF:
               no_graphics(00);
               break;
        case SHOW:
               WhiteWorkRow;

   //          filename_to_sbuf();   // yleisemmäksi koko funktio? ei ehkä?!
                 // (määritelty tässä kompilandissa, ei käytössä muualla)
               if (f->hidden_comment) strcpy(sbuf,f->comment); // 5.4.2001
               else strcpy(sbuf,f->name);
               sprintf(answer," File %s%s",edisk,sbuf);

   //          sprintf(answer," File %s",sbuf);

               WorkRowText(7);
               filename_to_sbuf();
               sprintf(answer,"SHOW %s %d",sbuf,f->match); /* 30.10.1998 */
               write_cmd_line();
               i=suorita("_SHOW.EXE"); if (i<0) return -1;
               break;
        case MATSHOW:
               DDf_matshow(); break;
        case TUTSHOW:
               DDf_tutshow(); break;

/* ZIPVIEW (and LZHVIEW) as well as xview partly rewritten 6.10.2000 */

        case ZIPVIEW:
                        i=xview("Zip",  "UNZIP","-lv","-o","-d"); //  SURVO MM
               if (i<0) i=xview("Zip","PKUNZIP","-vb","-o","-d"); // another try
               if (i<0) return -1;
               if (i==0) return DIRMOVE; /* change to TMP directory */
               break;
        case LZHVIEW: // (Juha Valtonen, mainly)
                        i=xview("Lzh",    "LHA","l",  "e", "");
               if (i<0) return -1;
               if (i==0) return DIRMOVE; /* change to TMP directory */
               break;
    }
    clear_screen();
    return f->command;
}

static int xview(char *filetype, char *command, char *opt1, char *opt2, char *opt3)
{
    int i,sh;

    WhiteWorkRow; LOCATE(WORKROW,1);
    filename_to_sbuf();
    sprintf(answer," %s file %s",filetype,sbuf);
    WorkRowText(7);

    sprintf(tempfil,"%s%s",etmpd,TMPFILE);
    fh=muste_fopen(tempfil,"w"); if (fh==NULL) return -1;
    sh=_dup(1); if (sh==-1) return -1;
    if (_dup2(fileno(fh),1) == -1) return -1;
//  i=spawnlp(P_WAIT,command,command,opt1,sbuf,NULL);
    fflush(stdout);
    fclose(fh);
    _dup2(sh,1);

    if (i<0) { prg_not_found(command); return -1; }
    sprintf(answer,"SHOW %s",tempfil);
    write_cmd_line();
    i=suorita("_SHOW.EXE"); if (i<0) return -1;
    sur_delete(tempfil);

/* continue by loading files if desired; not in WhereMode */
    if (WhereMode) return 1;
    WhiteBottomRow; LOCATE(BOTTOMROW,1);
    sprintf(tmp," Load files inside %s? ", f->name);
    show_yesno(tmp);
    i=ask_yesno(tmp,"Y");
    if (i<0) return 1;
    WhiteWorkRow; LOCATE(WORKROW,1);
    sprintf(answer," loading files from %s...",f->name);
    WorkRowText(7);
    sprintf(tempfil,"%s%s",etmpd,TMPDIR);
    _mkdir(tempfil);
    sprintf(answer, "%s%s", edisk,f->name);
    sprintf(sbuf,"%s\\",tempfil);
//  i=spawnlp(P_WAIT,command,command,opt2,answer,opt3,sbuf,NULL);
    if (i<0) { prg_not_found(command); return -1; }
    i=DDcheck_given_path(tempfil,2); if (i<0) return -1;
    i=DDchange_disk(drive,dir); if (i<0) return -1;
    if (NormalMode) {
        i=update_dirlist(); if (i<0) return -1;
    }
    strcpy(GV.filespec,path); /* path updated by check_given_path */
    clear_screen();
    GV.status=(GV.status | GUNZIP);
    return 0; /* causes return-chain to DD.C */
}

static void prg_not_found(char *command)
{
    disp_err(" %s program could not be found!", command);
}

static int DDf_tutshow(void)
{
    int i,j,k,m;
    FILE *fh,*in,*out;
    char buf[LLENGTH]; /* to replace sbuf 7.6.2000 */

    WhiteWorkRow;
    sprintf(tempfil,"%s%s",etmpd,TMPFILE);
    filename_to_sbuf();
    strcpy(buf,sbuf);

    if ((fh=muste_fopen( buf,"rb"))==NULL) return -1;
    j=fread((void *)line, sizeof(char), (size_t)22, fh);
    if ((j>=22) && (strstr(line,"SURVO 84C SUCROS@")!=NULL)) {
        for (i=18,j=0; i<22; i++,j++) tmp[j]=line[i];
        tmp[j]='\0';
        k=atoi(tmp); /* # of members in sucro family */
        sprintf(answer,"TUTLOAD %s,%s", buf,tempfil);
        write_cmd_line();
        i=suorita("&TUT.EXE"); if (i<0) return -1;
        if ((out=muste_fopen(tempfil,"a"))==NULL) return -1;
        for (m=0; m<k; m++) {
            sprintf(answer,
               " Sucro family %s (loading members...%d)", buf,m+1);
            WorkRowText(7);
            fread((void *)line, sizeof(char), (size_t)18, fh);
            for (i=2,j=0; line[i]!=' '; i++,j++) tmp[j]=line[i];
            tmp[j]='\0';
            sprintf(path,"%sDD0.TMP",etmpd);
            sprintf(answer,"TUTLOAD %s-%s,%s", buf,tmp,path);
            write_cmd_line();
            i=suorita("&TUT.EXE"); if (i<0) return -1;
            strcpy(fname,f->name);
            fname[strcspn(fname,".")]='\0';
            sprintf(answer, "\n\n%s-%s: \n\n\n",fname,tmp);
            fputs(answer,out);
            if ((in=muste_fopen(path,"r"))==NULL) return -1;
            while(fgets(line,LLENGTH,in)!=NULL) fputs(line,out);
            fclose(in);
        }
        WhiteWorkRow;
        sprintf(answer," Sucro family %s", buf);
        WorkRowText(7);
        fclose(out);
        sur_delete(path);
    } else {
        sprintf(answer," Sucro file %s", buf);
        WorkRowText(7);
        sprintf(answer,"TUTLOAD %s,%s", buf,tempfil);
        write_cmd_line();
        i=suorita("&TUT.EXE"); if (i<0) return -1;
    }
    fclose(fh);
    sprintf(answer,"SHOW %s",tempfil);
    write_cmd_line();
    i=suorita("_SHOW.EXE"); if (i<0) return -1;
    sur_delete(tempfil);
    return 1;
}

static void filename_to_sbuf(void)
{
    sprintf(sbuf,"%s%s",edisk,f->name);
}

static int DDf_search(void)
{
    init_prompt();
    f=df[current]; if (LastFile(f)) return -1;
    strcpy(sbuf," String to be searched from file names and comments: ");
    prompt(sbuf,srchstr,SRCHSTRLEN-1);
    if (!strlen(srchstr)) return 0;
    while (1) {
        f++; /* start from the next from the current */
        sprintf(tmp,"%s%s",f->name,f->comment);
        muste_strupr(tmp);
        strcpy(path,srchstr);
        muste_strupr(path);
        if (strstr(tmp,path)!=NULL) {
            strcpy(revisitfile,f->name);
            if (WhereMode) strcpy(revisitpath,f->path);
            return 1; /* found! */
        }
        if (LastFile(f)) break;
    }
    disp_err(" %s not found!", srchstr);
    return 0;
}

static void no_graphics(int survo) // 13.9.2000
{
    char stype[4], msg[81];

    switch(survo) {
        case 84: strcpy(stype, "84C");
                 strcpy(msg, "which can be displayed in SURVO 84C only.");
                 break;
        case 98: strcpy(stype, "98");
                 strcpy(msg, "which can be displayed in SURVO 98 only.");
                 break;
        case 00: strcpy(stype, "MM");
                 strcpy(msg, "which is displayed by the command GPLOT FILE.");
                 break;
    }
    WhiteWorkRow;
    filename_to_sbuf();
    CLS; headline("");
    sprintf(answer," File %s",sbuf);
    WorkRowText(7);

    LOCATE(3,1);
    sprintf(answer,"is a GPLOT graphics file,");
    muste_kv_s_disp("\n%s",answer);
    muste_kv_s_disp("\n%s",msg);
    WAIT;
}

static int DDf_copy(int function) /* function: 1,2=COPY 0=MOVE */
                                  /*             2: PREFIX-COPY (default A/C) */
{
    int i,j,k,manyfiles,owrall,tosamedir,portname;
 // char hidden_name[LNAME]; // 5.4.2001 replaced with longname 13.5.2003

    manyfiles=markcount;
    init_prompt();
    if (manyfiles) {
        f=&files[0];
        sprintf(tmp, " %s %d marked file%s to ? ",
          (function) ? "Copy" : "Move", markcount,(markcount>1)?"s":"");
    } else {
        f=df[current];
        if (SubDirectory(f)) return -1;
     // if (f->hidden_comment) strcpy(hidden_name,f->comment); // 5.4.2001
     // else strcpy(hidden_name,f->name);
     // sprintf(path,"%s",hidden_name);
        sprintf(path,"%s",f->longname); /* 13.5.2003 */
        sprintf(tmp," %s %s to ? ",
                (function) ? "Copy" : "Move", path);
    }
    if (function==2) { /* 5.11.96 */
        strcpy(answer,edisk);
        if (WhereMode) answer[3]='\0'; /* 5.2.1998 */
        if (edisk[0]>='C') answer[0]='A'; else answer[0]='C';
    }
    prompt(tmp,answer,min(LNA64,80-strlen(tmp)-2));
    if (!strlen(answer)) return -1; /* nothing given */
    portname=check_ports(answer);
    if (portname) {
        disp_err(" Can not write to %s!", answer);
        return -1;
    }
    WhiteBottomRow; LOCATE(BOTTOMROW,1);

    i=DDcheck_given_path(answer,2); /* updates drive,dir,fname,ext */
    if (i<0) return -1; /* invalid path given */
    i=check_diskspace(drive); /* more clever since 26.9.97 */
    if (i<0) return -1; /* not enough space on target disk */
    sprintf(line,"%s%s",drive,dir);
    strcpy(answer,edisk); tosamedir=0;
    if (!muste_strcmpi(line,answer)) tosamedir=1;
    if (tosamedir && function==0) {
        disp_err(" Files can not be moved to the same directory!");
        return -1;
    }


    if (manyfiles) {
        strcat(line,"*.*");
    } else {
        if (!strcmp(fname,"*") && !strcmp(ext,".*")) {
         // strcat(line, hidden_name);
            strcat(line, f->longname); /* 13.5.2003 */
        } else {
            if (!strlen(fname) && !strlen(ext)) {
             // strcat(line, hidden_name);
                strcat(line, f->longname); /* 13.5.2003 */
            } else {
                strcat(line, fname);
                strcat(line, ext);
            }
        }
    }

    i=check_and_mk_dir(1); if (i<0) return -1; /* 5.2.1998 */
    LOCATE(BOTTOMROW,1);
    muste_kv_s_disp(" %sing...",(function) ? "Copy" : "Mov");
    owrall=0; /* do not overwrite all by default */

    for (i=0,k=0; i<GV.filecount; f++) {
        if (manyfiles) {
            if (!FileMarked(f)) { i++; continue; }
         // if (f->hidden_comment) strcpy(hidden_name,f->comment);
         // else strcpy(hidden_name,f->name);
         // sprintf(line,"%s%s%s",drive,dir,hidden_name);
            sprintf(line,"%s%s%s",drive,dir,f->longname); /* 13.5.2003 */
            WhiteBottomRow;
            LOCATE(BOTTOMROW,1);
         // if (WhereMode) { /* 5.2.1998 */
         //     muste_kv_s_disp(" %sing %s%s...", (function) ? "Copy" : "Mov",
         //         f->path, hidden_name);
         // } else {
             // muste_kv_s_disp(" %sing %s...", (function) ? "Copy" : "Mov",hidden_name);
                muste_kv_s_disp(" %sing %s...", (function) ? "Copy" : "Mov",f->longname); /* 13.5.2003 */
         // }
            LOCATE(BOTTOMROW,72);
            muste_kv_s_disp("(%d)",++k);
        } else {
            k++;
            if (i>0) break; /* only current file copied! */
        }
        i++;

        if (WhereMode) {
         // sprintf(path,"%s%s",f->path,hidden_name);
            sprintf(path,"%s%s",f->path,f->longname); /* 13.5.2003 */
            if (function==2) {      /* 5.2.1998 for my own bck's.. */
                strcpy(line,path);
                line[0]=drive[0];
                j=DDcheck_given_path(line,2); if (j<0) return -1;
                path[0]=f->path[0]; /* fix source drive after check */
                j=check_and_mk_dir(1); if (j<0) return -1;
            }
        } else {
         // sprintf(path,"%s%s",edisk,hidden_name);
         // sprintf(path,"%s",hidden_name);
            sprintf(path,"%s",f->longname); /* 13.5.2003 */
        }

        if (SystemFile(f) || HiddenFile(f)) {
            sprintf(tmp," Attributes prevent %sing of file %s.",
                    (function) ? "copy" : "mov", path);
            WhiteBottomRow; LOCATE(BOTTOMROW,1);
            if (manyfiles) {
                strcat(tmp, " Continue ? "); show_yesno(tmp);
                j=ask_yesno(tmp,"Y"); if (j<0) return -1;
            } else {
                disp_err(tmp); return -1;
            }
        }
        if (!strcmp(path,line)) {
            sprintf(tmp," File can not copied onto itself (%s)!",path);
            WhiteBottomRow; LOCATE(BOTTOMROW,1);
            if (manyfiles) {
                strcat(tmp, " Continue ? "); show_yesno(tmp);
                j=ask_yesno(tmp,"Y"); if (j<0) return -1;
            } else {
                disp_err(tmp); return -1;
            }
        }
        if (!owrall) {
            h_fil=_findfirst(line, &fil);
            if (h_fil != -1L) {
                _findclose(h_fil);
                sprintf(tmp," File %s exists. Overwrite ? ",line);
                WhiteBottomRow; LOCATE(BOTTOMROW,1);
                write_string("(Yes/No/All/Quit)",
                  17,Advice,BOTTOMROW,strlen(tmp)+4);
                strcpy(sbuf,"Y");
                prompt(tmp,sbuf,1); /* yes/no is not enough here! */
                WhiteBottomRow;
                switch (sbuf[0]) {
                  case 'Y': case 'y': break;
                  case 'N': case 'n': continue;
                  case 'A': case 'a': owrall=1; break;
                   default: return -1;
                }
            }
        }

        LOCATE(BOTTOMROW,1); WhiteBottomRow;
     // if (WhereMode) { /* 5.2.1998 */
     //     muste_kv_s_disp(" %sing %s%s...", (function) ? "Copy" : "Mov",
     //         f->path, hidden_name);
     // } else {
         // muste_kv_s_disp(" %sing %s...", (function) ? "Copy" : "Mov",hidden_name);
            muste_kv_s_disp(" %sing %s...", (function) ? "Copy" : "Mov",f->longname); /* 13.5.2003 */
     // }

        LOCATE(BOTTOMROW,ScreenWidth-8);
        muste_kv_s_disp("(%d)",k);
//muste_kv_s_err("kopiointi: \nsource=%s|\ntarget=%s|",path,line);
        sur_copy_file(path, line);
 // "Älyttömien" nimien kanssa ongelmia: ei kopioi eikä ilmoita, esim.
 // F-Secure Anti-Virus, version 5_30 - Scanning Report - 22 November 2001 120024.txt
 //  CopyFile(path,line,0); // kokeillaan suoraan APInaa...  (ei vaikutusta!)
 // Ratkaisu alkoi löytyä 13.2.2002: kommenttien tallennus katkaisee pidemmät nimet!
 // Nyt toimii em. älyttömilläkin nimillä.
    }
    return (tosamedir);
}

static int check_and_mk_dir(int function) /* 5.2.1998 */ /* 24.5.1999 */
{
    char *token;
    char localpath[LNAME/2]; /* had to replace path 5.2.1998 */

    h_fil=_findfirst(line, &fil);
    if (h_fil != -1L) {
        _findclose(h_fil);
        return 1;
    }
    sprintf(localpath,"%s%s",drive,dir);
    token=strtok(localpath,"\\");
    strcpy(tmp,"");
    while (token!=NULL) {
        strcat(tmp,token);
        strcpy(sbuf,tmp);
        strcat(sbuf, "\\*.*");
        h_fil=_findfirst(sbuf, &fil);
        if (h_fil == -1L) {
            if (strlen(tmp)>2) { /* len2: 'E:' or 'A:' etc. */
                if (function==1) { /* called from DDf_copy */
                    WhiteBottomRow;
                    LOCATE(BOTTOMROW,1);
                    muste_kv_s_disp(" Creating directory %s...",tmp); /* 17.1.1999 */
                }
                if (_mkdir(tmp)) {
                    disp_err(" Bad directory name (%s)!",tmp);
                    return -1;
                }
            }
        } else {
            _findclose(h_fil);
        }
        strcat(tmp,"\\");
        token=strtok(NULL,"\\");
    }

    return 1;
}

static int DDf_move(void)
{
    int i;
    i=DDf_copy(0); if (i<0) return 0;
    return DDf_delete();
}

static int check_diskspace(char *drivestr)
{
    double total_space, avail_space, limit_space, drive;
    char root[4];
    int i;
    Files *ff; /* 26.9.97 for accurate disk space checking! */

    if (markcount>0) {
        limit_space=(double)markbytes;
    } else {
        limit_space=(double)f->size;
    }
    sprintf(root, "%s\\", drivestr);
//  i=WAPI_GetDiskFree(root, &total_space, &avail_space);
//  if (i<0) return -1;

    sprintf(path,"%s%s",drivestr,dir);
    if (markcount>0) {
        ff=&files[0];
        for (i=0; i<GV.filecount; i++,ff++) {
            if (!FileMarked(ff)) continue;
            sprintf(line,"%s%s",path,ff->name);
            h_fil=_findfirst(line, &fil);
            if (h_fil != -1L) {
                _findclose(h_fil);
                avail_space+=(double)fil.size;
            }
        }
    } else {
        sprintf(line,"%s%s",path,f->name);
        h_fil=_findfirst(line, &fil);
        if (h_fil != -1L) {
            _findclose(h_fil);
            avail_space+=(double)fil.size;
        }
    }
    if (avail_space<limit_space) {
        disp_err(" Not enough space on drive %s!", drivestr);
        return -1;
    }
    return 1;
}

static int check_ports(char *fstr)
{
    if (!muste_strnicmp(fstr,"PRN",3)) return 1;
    if (!muste_strnicmp(fstr,"LPT",3)) return 1;
    if (!muste_strnicmp(fstr,"COM",3)) return 1;
    if (!muste_strnicmp(fstr,"AUX",3)) return 1;
    if (!muste_strnicmp(fstr,"NUL",3)) return 1;
    return 0;
}

static int DDf_disk(void)
{
    int i,row,col,selrow,width;

    clear_screen(); init_prompt();
    ReversedWorkRow;
    strcpy(answer, " List of directory and file selections: ");
    write_string(answer,strlen(answer),Reverse,WORKROW,1);
    Dp=DLast; i=1; row=ddSTARTROW; col=1; width=ScreenWidth;
    while (Dp!=D) {
        if (row>ENDROW) { /* 2nd column */
            if (col>1) break;
            width/=2; col+=width; row=ddSTARTROW;
        }
        write_string(space,width,FileColor,row,col);
        sprintf(answer, "      %s ",Dp->files);
        write_string(answer,strlen(answer),FileColor,row,col);
        sprintf(answer, " %2d ",i);
        write_string(answer,strlen(answer),Cursor,row,col+1);
        Dp=Dp->prev; row++; i++;
    }
    WhiteBottomRow;
    strcpy(sbuf, " Select directory by number : ");
    init_prompt();
    prompt(sbuf,answer,2);
    clear_screen();
    if (!strlen(answer)) return 1;
    selrow=atoi(answer);
    if (selrow<1 || selrow>=i) return 1;
    Dp=DLast; i=1;
    while (i<selrow) { Dp=Dp->prev; i++; }
    i=DDcheck_given_path(Dp->files,2); if (i<0) return 1;
    strcpy(path,Dp->files); /* temporary save for the new */
    i=update_dirlist(); if (i<0) return -1;
    strcpy(GV.filespec,path); /* new will be loaded */
    return 0; /* causes return-chain to DR.C */
}

static int DDf_delete(void)
{
    int i,j,somedeleted,wasfilecount,wasmarkcount;
    Files *fcurrent;

    somedeleted=0;
    init_prompt();
    f=df[current]; fcurrent=f;
    if (markcount>0) {
        wasfilecount=GV.filecount;
        wasmarkcount=markcount;

        if (LastFile(f)) {
            while (FileMarked(f) && !FirstFile(f)) f--;
        } else {
            while (FileMarked(f) && !LastFile(f)) f++;
        }
        strcpy(revisitfile,f->name);
        if (WhereMode) strcpy(revisitpath,f->path);
        sprintf(answer, " Delete %d marked file%s ? ",
           markcount,(markcount>1)?"s":"");
        WhiteBottomRow; LOCATE(BOTTOMROW,1);
        show_yesno(answer);
        j=ask_yesno(answer,"Y");
        if (j>0) {
            for (i=0,j=0,f=&files[0];
                     i<wasfilecount && j<wasmarkcount; f++) {
                if (FileMarked(f)) {
                    LOCATE(BOTTOMROW,1); WhiteBottomRow;
                    muste_kv_s_disp(" Deleting %s...",f->name);
                    LOCATE(BOTTOMROW,72);
                    muste_kv_s_disp("(%d)",++j);
                    if (DDf_delfile()) { somedeleted=1; markcount--; }
                }
                i++;
            }
        }
    } else {
        if (!strcmp(f->name,"..")) return 0;
        if (LastFile(f)) {
            if (!FirstFile(f)) f--;
        } else {
            f++;
        }
        strcpy(revisitfile,f->name);
        if (WhereMode) strcpy(revisitpath,f->path);
        f=fcurrent;
        if (WhereMode) sprintf(path,"%s%s",f->path,f->name);
        else sprintf(path,"%s%s",edisk,f->name);
        if (SubDirectory(f))
          sprintf(answer, " Remove directory %s ? ", path);
        else
          sprintf(answer, " Delete %s ? ", path);
        WhiteBottomRow; LOCATE(BOTTOMROW,1);
        show_yesno(answer);
        j=ask_yesno(answer,"Y");
        if (j>0) {
            if (DDf_delfile()) somedeleted=1;
        }
    }
    return (somedeleted);
}

static int DDf_delfile(void)
{
    int j;

    if (WhereMode) sprintf(path,"%s%s",f->path,f->name);
    else sprintf(path,"%s%s",edisk,f->name);

    if (ReadOnlyFile(f)) {
        disp_err(" File %s is marked readonly!",path);
        return 0;
    }
    if (SystemFile(f) || HiddenFile(f)) {
        sprintf(answer," File %s is a System/Hidden file!", path);
        strcat(answer, " Delete ? ");
        WhiteBottomRow; LOCATE(BOTTOMROW,1);
        show_yesno(answer);
        j=ask_yesno(answer,"N");
        if (j<0) return 0;
    }
    if (SubDirectory(f)) {
        if (_rmdir(path)) {
            disp_err(" Can not remove directory %s!",path);
            return 0;
        }
        GV.dircount--;
    } else {
        sur_delete(path);
        totalbytes-=f->size;
    }
    GV.filecount--;
    f->status=(f->status | FDELETED); /* lazy deletion from the table */
    f->status=(f->status & ~FMARKED); /* remove (possible) mark */
    return 1;
}

static int MDmain(void) /* MD = Make Directory 24.5.1999 */
{
    int i;
    if (g<2) {
        muste_kv_usage_info();
        return -1;
    }
    i=DDcheck_given_path(word[1],2); /* updates drive,dir,fname,ext */
    if (i<0) return -1; /* invalid path given */
    sprintf(line,"%s%s%s%s",drive,dir,fname,ext);
    i=check_and_mk_dir(2); if (i<0) return -1;
    return 1;
}

static int RDmain(void) /* RD = Remove Directory 24.5.1999 */
{
    int i;
    if (g<2) {
        muste_kv_usage_info();
        return -1;
    }
    i=DDcheck_given_path(word[1],2); /* updates drive,dir,fname,ext */
    if (i<0) return -1; /* invalid path given */
    sprintf(line,"%s%s%s%s",drive,dir,fname,ext);
    i=strlen(line); if (line[i-1]=='\\') line[i-1]='\0';
    if (_rmdir(line)) {
        disp_err(" Can not remove directory %s!",line);
        return -1;
    }
    return 1;
}

static void DDmake_date_and_time(char *dtstr)
{
    int i;
    char dstr[STRMAXL], tstr[STRMAXL];
    unsigned year;
    unsigned YYYY;

    year = fi->year;
    if(year>99) year-=100; /* Y2K, 23.7.1997 */
    YYYY = fi->year+1900; /* 21.7.1998 */
    for (i=0; i<STRMAXL; i++) { dstr[i]='\0'; tstr[i]='\0'; }
    switch (GV.print_date) {
       case 1: sprintf(dstr,"%.2d.%.2d.%.2d ",fi->day,fi->month,year);
               break;
       case 2: sprintf(dstr,"%.2d/%.2d/%.2d ",fi->day,fi->month,year);
               break;
       case 3: sprintf(dstr,"%.2d%.2d%.2d ",fi->day,fi->month,year);
               break;
       case 4: sprintf(dstr,"%.2d%.2d%.2d ",year,fi->month,fi->day);
               break;
       case 5: sprintf(dstr,"%.2d/%.2d/%.2d ",fi->month,fi->day,year);
               break;
     /* 21.7.1998 */                /* _SHOW D:\W\DESKTOP\GEN1 69 / */
       case 6: sprintf(dstr, "%.2d.%.2d.%.2d ", fi->day, fi->month, YYYY);
               break;
       case 7: sprintf(dstr, "%.2d/%.2d/%.2d ", fi->day, fi->month, YYYY);
               break;
       case 8: sprintf(dstr, "%.2d/%.2d/%.2d ", fi->month, fi->day, YYYY);
               break;
       case 9: sprintf(dstr, "%.2d%.2d%.2d ", fi->day, fi->month, YYYY);
               break;
      case 10: sprintf(dstr, "%.2d%.2d%.2d ", YYYY, fi->month, fi->day);
               break;
      default: break;
    }
    switch (GV.print_time) {
       case 1: sprintf(tstr,"%.2d:%.2d:%.2d ",fi->hour,fi->minute,fi->second);
               break;
       case 2: sprintf(tstr,"%.2d:%.2d ",fi->hour,fi->minute);
               break;
       case 3: sprintf(tstr,"%.2d.%.2d.%.2d ",fi->hour,fi->minute,fi->second);
               break;
       case 4: sprintf(tstr,"%.2d.%.2d ",fi->hour,fi->minute);
               break;
      default: break;
    }
    strcpy(dtstr,dstr); strcat(dtstr,tstr);
}

// END DD ////////////////////////////////////////////////////////////////////

#endif // TOISTAISEKSI_SIVUUTETTU

#if TOISTAISEKSI_SIVUUTETTU

// BEGIN WHERE //////////////////////////////////////////////////////////////////

static int WHEREmain(void)
{
    int i;
    if (g>1) {
        if (!strcmp(word[1],"?")) { muste_kv_usage_info(); return 1; }
        if ( (!strcmp(word[1],"*:T")) || (!strcmp(word[1],"/TREE")) ) { /* 1.5.97 */
            tree(); return 1;
        }
    } else {
        muste_kv_usage_info(); return 1;
    }

    where_first=0;
    i=spfind("WHERE"); /* 13.2.2002 */
    if (i>=0) {
        if (strstr(spb[i],"FIRST") != NULL) where_first=1;
    }

    /* OUTFILE things added 15.10.2005 (SM's suggestion) */
    /* (code copied from SEARCH; outfile & output_file -> global */

    i=spfind("OUTFILE");
    if (i>=0) {
        if (strchr(spb[i], ':') != NULL) {
            strcpy(outfile, spb[i]);
        } else {
            sprintf(outfile, "%s%s", edisk, spb[i]);
        }
    } else {
        sprintf(outfile, "%s%s", etmpd, "WHERE.OUT");
    }

    output_file = muste_fopen(outfile, "w"); /* remove any existing file */
    if (output_file == NULL) {
        muste_kv_s_err("Could not open output file %s!", outfile);
        return -1;
    }
    fprintf(output_file, "Filename\n");

    strcpy(info,word[1]);
    DDmain();
    return 1;
}

static void tree_keys(void)
{
    init_remarks();
    rem_pr(" Browse by arrow keys, PgDn, PgUp, HOME and END. Also N=Next, P=Prev and");
    rem_pr(" PREFIX down can be used (PREFIX=F2). For action, press:");
    rem_pr("  ENTER to display the contents of the current directory (by DD)");
    rem_pr("      L to load another directory tree");
    rem_pr("      F to display/hide the full path names of the directories");
    rem_pr("      T to restart tree from the current directory");
    rem_pr("      1 to display only the first level directories");
    rem_pr("      2 to display only the first and second level directories");
    rem_pr("  3 - 9 to display directories of levels 3-9 correspondingly");
    rem_pr("      0 to display all directory levels (default)");
    rem_pr("     F1 to display this help (`TREE?' to get more help)");
    rem_pr("      > to give a system command");
    rem_pr("     F8 to exit and restore the original path (current path by F7)");
    rem_pr(" ");
    rem_pr(" ");
    rem_pr(" ");
    rem_pr(" ");
    rem_pr(" ");
    rem_pr(" ");
    rem_pr(" ");
    rem_pr(" ");
    rem_pr(" ");
    PR_EINV; WAIT;
}

static void tree(void)
{
    int i,j,r_was,r1_was;
    char userpath[LNAME]; /* original edisk */

    strcpy(userpath,edisk);

    if (g>3) { /* 30.4.97 */         /* process execl'd by DD */
        if (!strcmp(word[3],"DD")) { /* WHERE /TREE <path> DD <origpath> */
            strcpy(userline, info);
            edwrite(userline,r1+r-1,0); /* restore user line */
            strcpy(userpath, word[4]); /* path when DD was called */
        }
    }
    if (g>2) strcpy(path,word[2]); else strcpy(path,edisk);

    if (g>1) {
        if (g>2) {
            strcpy(path,word[2]);
        } else {
            if (!muste_strcmpi(word[1],"/TREE")) strcpy(path,edisk);
            else strcpy(path,word[1]);
        }
    } else {
        strcpy(path,edisk);
    }
    GV.level=0; /* default: all levels shown, see scan() (1.5.97) */
    j=spfind("LEVEL"); /* 2.5.97 */
    if (j>=0) GV.level=atoi(spb[j]);

    sprintf(GV.treename, "%sTREE.TMP", etmpd); /* moved here 23.8.1998 */

    while (1) {
        if (path[strlen(path)-1]!='\\') strcat(path,"\\");
        strcat(path, "*.*");
        i=TREEcheck_given_path(path,1); if (i<0) break; /* return home */
        sprintf(TREEfiles,"%s*.*",GV.path);
        PR_EINV; muste_kv_s_disp(" Scanning directory tree %s...", GV.path);

        GV.dircount=0; GV.files=0L; GV.bytes=0L;
        TD=TDAlloc(); if (TD==NULL) { no_mem(); exit(1); }
        strcpy(TD->name,GV.path); strcpy(TD->path,GV.path);
        TD->sub=0; TD->next=NULL; TD->level=0; TD->order=0;
        TD->files=0L; TD->bytes=0L; TDp=TD;

        TD->sub=scan(1); /* recursively get the directories & files */

        GV.outfile=0;
        j=spfind("OUTFILE"); /* 2.5.97 */
        if (j>=0) { /* these lines executed once at most */
            GV.outfile=1;
            if (strchr(spb[j],':')==NULL)
                sprintf(GV.treename,"%s%s",userpath,spb[j]); /* 22.6.2000 */
            else strcpy(GV.treename,spb[j]);
            GV.treefile=muste_fopen(GV.treename,"w");
         // fprintf(GV.treefile,"Path %60s Files  Bytes\n"," ");
            fprintf(GV.treefile,"Path Files Bytes\n"); // 20.7.2002
            TDp=TD; strcpy(root1,""); strcpy(roots,"");
            TREEprintout(TDp->name,0,-1); /* print the tree & exit (2.5.97)*/
            fclose(GV.treefile); free_tree();
            break; /* out from while(1) -> return */
        }
        edread(userline,r1+r-1);     /* command line saved */
        GV.treefile=muste_fopen(GV.treename,"w");
        TDp=TD; strcpy(root1,""); strcpy(roots,"");
        TREEprintout(TDp->name,0,-1); /* print the tree skeleton to file */
        fclose(GV.treefile);

        r1_was=r1; r1=r1+r-1; r_was=r; r=1; s_end(siirtop); /* move window */
        i=showtree();
        r1=r1_was; r=r_was; s_end(siirtop); /* restore window */

        switch(i) {
          case CODE_EXIT: case -1: break;   /* back home */
          case CODE_REF:                    /* exit to new directory */
            strcpy(userpath, edisk); break;
          case LOAD: case LOAD2:            /* load another dir tree */
            strcpy(path, answer); break;
          case LEVEL1: case LEVEL2: case LEVEL3: /* 1.5.97 */
          case LEVEL4: case LEVEL5: case LEVEL6:
          case LEVEL7: case LEVEL8: case LEVEL9: case LEVEL0:
            strcpy(path, TD->path); break;
          case TREE: case TREE2: /* 1.5.97 */
            strcpy(path, edisk); break;
        }
        if (userpath[strlen(userpath)-1]!='\\') strcat(userpath,"\\");
        edwrite(userline,r1+r-1,0);  /* command line restored */
        free_tree();
        if (i==CODE_RETURN) { /* ENTER pressed, execl DD */
            strcpy(info,userline);
            sprintf(answer,"DD %s TREE %s",edisk,userpath);
            g=split(answer,word,4);
            DDmain();
            return;
        }
        if (i==CODE_EXIT || i==CODE_REF || i<0) break;
    } /* while(1)-loop */
    TREEcheck_given_path(userpath,1);
    return;
}

static TDPtr TDAlloc(void) { return ((TDPtr)malloc(sizeof(DirNode))); }

static void free_tree(void)
{
    TDPtr TDL;
    TDp=TD;
    while (TDp!=NULL) {
        TDL=TDp;
        TDp=TDp->next;
        free(TDL);
    }
}

static int TREEprintout(char *dirname, int depth, int upper)
{
    int i,sub;
    char localroot[LNAME];

    strcpy(localroot,roots);
    switch(upper) {
        case -1: break;
        case  0: strcat(roots,TreeBlank); break;
        case  1: strcat(roots,TreeDown); break;
    }
    if (GV.outfile) {
        fprintf(GV.treefile,"%s %u %u\n",
           TDp->path, TDp->files, TDp->bytes);
    } else {
        fprintf(GV.treefile,"%s%s\n", localroot,root1);
    }
    sub=TDp->sub;
    for (i=0; i<sub; i++) {
        TDp=TDp->next;
        while (TDp->sub==-1) TDp=TDp->next; /* skip hidden nodes */
        strcpy(root1,"");
        if (i<(sub-1)) {
            strcat(root1, TreeSibling);
            TREEprintout(TDp->name,depth+1,1);
        } else {
            strcat(root1, TreeCorner);
            TREEprintout(TDp->name,depth+1,0);
        }
    }
    strcpy(roots,localroot);
    return 1;
}

static int scan(int depth) /* recursively called */
{
    struct _finddata_t fil;
    char localdir[LNAME];
    int h_fil;
    int count=0;
    TDPtr TDL; /* local to each recursion level */
    TDPtr TDM; /* ------------ " " ------------ */

    TDM=TDp;
    h_fil=_findfirst(TREEfiles, &fil);
    if (h_fil == -1L) return count;
    do {
        if (SUBDIR(fil) && REALDIR(fil)) {
            count++; GV.dircount++;
            _splitpath(TREEfiles, drive, dir, fname, ext);
            sprintf(path, "%s%s%s",drive,dir,fil.name);
            TDL=TDAlloc(); if (TDL==NULL) { no_mem(); exit(1); }
            TDp->next=TDL; TDp=TDp->next; TDp->next=NULL;
            TDL->bytes=0L; TDL->files=0L;
            strcpy(TDL->name,fil.name); strcpy(TDL->path,path);
//          WAPI_ShortPath(TDL->path); // 20.7.2002
            TDL->sub=0; TDL->level=depth; /* 1,2,... (header=0) */
            TDL->order=GV.dircount;
            sprintf(TREEfiles, "%s\\*.*",path);
            strcpy(localdir, dir);
            TDL->sub=scan(depth+1);
            if (GV.level) { /* user has selected LEVEL=1,2,...,9 */
                if(depth>=GV.level) {
                    GV.dircount-=TDL->sub;
                    TDL->sub=0;
                }
                if(depth>GV.level) {
                    TDM->files+=TDL->files;
                    TDM->bytes+=TDL->bytes;
                    TDL->sub=-1;
                }
            }
            sprintf(TREEfiles, "%s%s*.*", drive, localdir);
        } else {
            if (!REALDIR(fil)) { /* 2.1.1998 */
                ;
            } else {
                TDM->files++; TDM->bytes+=fil.size;
                GV.files++; GV.bytes+=fil.size;
                flash_counter(GV.files); /* 31.10.1998 (Halloween) */
            }
        } /* if (SUBDIR) */
    } while(!_findnext(h_fil, &fil));
    _findclose(h_fil);
    return count;
}

static int TREEcheck_given_path (char *fstr, int level) /* level: 1(disk),2(path)*/
{
    int wild,point;
    char *s;

    s=fstr; while(*s==' ') ++s; /* 5.11.96 */
    strcpy(tmp,s);
//  if (!case_sensitive_pathnames) muste_strupr(tmp);
    wild=0; point=0;
    if (strchr(tmp, '*') != NULL) wild=1;
    if (strchr(tmp, '?') != NULL) wild=1;
    if (strchr(tmp, '.') != NULL) point=1;
    if (strchr(tmp, ':') != NULL) { /* drive given */
        if (strlen(tmp)==2) strcat(tmp, "*.*\0"); /* only drive given */
        if (wild && !point) strcat(tmp, ".*\0");
        strcpy(fullspec, tmp);
    } else {
        if (wild && !point) strcat(tmp, ".*\0");
        if (tmp[0]!='\\') sprintf(fullspec, "%s%s", edisk, tmp);
        else sprintf(fullspec, "%c:%s", edisk[0], tmp);
    }
    strcpy(tmp,fullspec); tmp[2]='\0'; strcat(tmp, "*.*\0");
    if (_fullpath(path, tmp, LNAME-1) == NULL) {
        DDerror=BADPATH;
        display_error(fstr);
        return -1;
    }
    if (_fullpath(path, fullspec, LNAME-1) == NULL) {
        DDerror=BADPATH;
        display_error(fstr);
        return -1;
    }

    if( path[strlen(path)-1]=='.' && strlen(path) > 1 &&
       (path[strlen(path)-2]=='\\' || path[strlen(path)-2]=='.') )
           strcat(path,"\\");
    _splitpath(path, drive, dir, fname, ext);

    if (level==2) { /* this used to be 'check_path' (in DD) */
        if (!strlen(ext)) {
            if (strlen(fname)) {
                sprintf(tmp, "%s%s\\",dir,fname);
                strcpy(dir,tmp);
                fname[0]='\0';
            }
        }
        return 1;
    }

    strcpy(fullspec, path);
    if (strlen(ext)==0) {
        if (strlen(fname)==0) { /* no file name - main directory */
            strcat(fullspec, "*.*");
        } else {
            strcat(fullspec, "\\*.*\0");
            strcat(fname, "\\");
            strcat(dir, fname);
        }
        h_fil=_findfirst(fullspec, &fil);
        if (h_fil == -1L) {
            DDerror=NOFILES;
            h_fil=_findfirst(path, &fil);
            if (h_fil == -1L) {
                DDerror=NOFILES;
                display_error(path);
                return -1;
            } else {
                _findclose(h_fil);
            }
        } else {
            strcpy(path, fullspec); /* fixed fullspec ok; use it */
            _findclose(h_fil);
        }
    } else { /* path includes all parts, also extension */
        h_fil=_findfirst(path, &fil);
        if (h_fil == -1L) {
            DDerror=NOFILES;
            display_error(path);
            return -1;
        } else {
            if (strchr(path, '*') != NULL) wild=1;
            if (strchr(path, '?') != NULL) wild=1;
            if (!wild) {
                if (SUBDIR(fil)) {
                    strcat(path, "\\*.*\0");
                    _splitpath(path, drive, dir, fname, ext);
                }
            }
            _findclose(h_fil);
        }
    }

    if (TREEchange_disk(drive,dir)<0) return -1;
    strcpy(GV.path,edisk);
    return 1;
}

static int TREEchange_disk (char *drive, char *dir)
{
    unsigned int len;

    sprintf(path, "%s%s", drive, dir);
    len=strlen(path);
    if ((path[len-1]=='\\') && (len>2+2)) path[len-1]='\0';
    if (_chdir(path)) {
        disp_err(" Can't change to directory %s!", path);
        return -1;
    }
    _getcwd(tmp, LNAME-1);
    muste_strupr(tmp);
    strcpy(edisk, tmp);
    if(edisk[strlen(edisk)-1]!='\\') strcat(edisk,"\\");
    headline(""); /* to get the new dir name straight up there! */
    return 1;
}

static int showtree(void)
{
    int i,m,msgnr;
    unsigned int ui;
    char msg[81]; /* message to the message line */

    GV.treefile=muste_fopen(GV.treename,"r");
    row=treeSTARTROW; current=dfi=0; fi2=0;
    msgnr=1; clear_screen(); TDp=TD;

    while (1) {
        fgets(line, LLENGTH-1, GV.treefile);
        while (TDp->sub==-1) TDp=TDp->next; /* skip hidden nodes (8.5.97)*/
        display_line();
        if (TEndOfPage) { /* includes GV.dircount==0 !! */
            TREEinfoline();
            strcpy(msg,treeMsg1);
            switch(msgnr) {
              case 1: strcat(msg,treeMsgA); break;
              case 2: strcat(msg,treeMsgB); break;
              case 3: strcat(msg,treeMsgC); break;
            }
            if (++msgnr>3) msgnr=1;
            m=nextch(msg);
            ui=toupper((unsigned char)m);
            i=handle_key(ui);
            switch(i) {
                case  0: fclose(GV.treefile);
                         return m;  /* returns CODE_* to TREE.C */
                case -1: return -1; /* returns -1 to TREE.C -> exit */
                default: break;
            }
            row=treeSTARTROW; dfi=0;
            muste_fseek(GV.treefile, 0L, SEEK_SET); TDp=TD;
            for (i=0; i<fi2; i++) {
                fgets(line, LLENGTH-1, GV.treefile);
                TDp=TDp->next;
                while (TDp->sub==-1) TDp=TDp->next; /* skip (8.5.97)*/
            }
        } else {
            row++; dfi++; fi2++; TDp=TDp->next;
        }
    }
    return 1;
}

static void display_line(void)
{
    char color;
    int col;

    color=FileColor; col=1;
    write_string(space,ScreenWidth,color,row,col);
    if (fullname) {
        strcpy(line,TDp->path);
    } else {
        line[strlen(line)-1]='\0';
        write_string(line,strlen(line),FileColor2,row,col); /* puukuvio */
        col+=TDp->level*4;
        strcpy(line,TDp->name);
    }
    if (dfi==current) {
        color=Cursor;
        strcpy(edisk,TDp->path);
        if (edisk[strlen(edisk)-1]!='\\') strcat(edisk,"\\");
    }
    write_string(line,strlen(line),color,row,col);
    sprintf(sbuf,"%5u %10u", TDp->files, TDp->bytes);
    col=ScreenWidth-(5+1+10);
    write_string(sbuf,strlen(sbuf)  ,color,row,col);
}

static void TREEinfoline(void)
{
    char sign[2];

    LOCATE(BOTTOMROW,1);
    sprintf(line, " %s - %d sub-director%s, %u file%s, %u byte%s.",
        TD->name,
        GV.dircount, (GV.dircount!=1)?"ies":"y",
        GV.files, (GV.files!=1L)?"s":"",
        GV.bytes, (GV.bytes!=1L)?"s":"");
    ReversedWorkRow;
    write_string(line,strlen(line),Reverse,WORKROW,1);
    sprintf(sign, "%d", GV.level); /* 1.5.97 */
    write_string(sign,1,Advice,1,ScreenWidth);
}

static int handle_key(unsigned int m)
{
    int i;
    unsigned int ui;

    switch (m) {
      case CODE_REF:
      case CODE_EXIT:
                        return 0;
      case CODE_EXEC: /* ESC is still same as ENTER */
      case CODE_RETURN: m=CODE_RETURN;
                        return 0;
      case CODE_HELP:   f_help(); break;
      case LOAD: case LOAD2:
                        i=f_load();
                        if (i<=0) return i;
                        break;
      case CODE_LEFT:  /* 8.5.97 */
      case CODE_UP:     if (GV.dircount) {
                            if (f_key(CODE_UP)) {
                                if (!FirstDir) fi2-=dfi+1; else fi2-=dfi;
                            } else fi2-=dfi;
                        }
                        return m;
      case CODE_RIGHT: /* 8.5.97 */
      case CODE_DOWN:   if (GV.dircount) {
                            if (f_key(CODE_DOWN)) {
                                if (!LastDir) fi2-=dfi-1; else fi2-=dfi;
                            } else fi2-=dfi;
                        }
                        return m;
      case CODE_HOME:   if (GV.dircount)
                            if (f_key(CODE_HOME)) return m;
                        break;
      case CODE_END:    if (GV.dircount)
                            f_key(CODE_END);
                        return m;
      case CODE_NEXT:
      case NEXT:        if (GV.dircount)
                            if (f_key(CODE_NEXT)) return m;
                        break;
      case CODE_PREV:
      case PREV:        if (GV.dircount)
                            f_key(CODE_PREV);
                        return m;
      case CODE_PRE:    if (GV.dircount) {
                          i=nextch(treeMsgP); /* show PREFIX-message here */
                          ui=toupper((unsigned char)i);
                          switch(ui) {
                            case CODE_DOWN: f_key(PREDOWN);
                                            fi2-=dfi;
                                            return m;
                            default:        break;
                          }
                        }
                        break;
// muste_system(char *komento, int wait) / RS 8.7.2011 ks.!
      case SYSTEM:      system_call(); clear_screen();
                        break;
      case FULLNAME:    fullname=1-fullname; /* 1.5.97 */
                        break;
      case LEVEL1:      GV.level=1; return 0;
      case LEVEL2:      GV.level=2; return 0;
      case LEVEL3:      GV.level=3; return 0;
      case LEVEL4:      GV.level=4; return 0;
      case LEVEL5:      GV.level=5; return 0;
      case LEVEL6:      GV.level=6; return 0;
      case LEVEL7:      GV.level=7; return 0;
      case LEVEL8:      GV.level=8; return 0;
      case LEVEL9:      GV.level=9; return 0;
      case LEVEL0:      GV.level=0; return 0;
      case TREE: case TREE2: return 0;
      default:          break;
    }
    if (GV.dircount) fi2-=dfi;
    return m;
}

static void f_help(void)
{
    init_prompt();
    WhiteWorkRow;
    strcpy(answer," Key codes in TREE:");
    WorkRowText(1);
    tree_keys();
    clear_screen();
}

static int f_key(int key)
{
    int i,maxi,lastfound,page_changed,clr,pg1,pg2;

    switch(key) {
      case CODE_HOME: if (current==0) {
                          fi2=0;
                          return 1;
                      }
                      current=0;
                      return 0;
      case CODE_END:  for (i=0; i<2; i++) { f_key(PREDOWN); fi2-=dfi; }
                      break;
                        /* _SHOW SHOWTREE 65 / */
      case CODE_NEXT:
                      if (LastDir) {
                          current=dfi;
                          return 0;
                      }
                      lastfound=page_changed=clr=pg1=0;
                      maxi=min(treeROWS,GV.dircount);

                      for (i=0; i<maxi; fi2++,i++) {
                          if (i>current) pg1++;
                          if (LastDir) { lastfound=1; break; }
                      }
                      pg2=i-pg1;
                      page_changed=(current+i) > treeROWS-1;

                      if (lastfound) {
                          if (page_changed) {
                              fi2-=dfi; current=0;
                          } else {
                              fi2-=dfi; /*current+=i;*/
                          }
                      } else {
                          if (page_changed) {
                              fi2-=dfi;
                          //  for (i=current; i>0; i--) fi2--;
                              if (pg1+pg2 < current) current=pg1+pg2;
                          }
                      }
                      if (page_changed) {
                          for (i=0; i<maxi; i++) {
                              if (LastDir) { clr=1; break; }
                          }
                      }
                      if (clr) clear_screen();
                      return 1;
      case CODE_PREV:
                      for (i=0; i<treeROWS; i++,fi2--) {
                          if (FirstDir) break;
                          if (fi2==0) {
                              if (i>0) clear_screen();
                              current=0;
                              break;
                          }
                      }
                      maxi=min(fi2,treeROWS-1);
                      fi2-=maxi; if (fi2==0) current=0;
                      return 1;
      case PREDOWN:
                      if (LastDir && current==treeROWS-1) break;
                      maxi=min(treeROWS-1,GV.dircount);
                      if (current<maxi) {
                          while (current<maxi) current++;
                      } else {
                          do fi2++; while (!LastDir);
                      }
                      break;
      case CODE_UP:   if (current>0) {
                          current--;
                          return 0;
                      }
                      return 1; /* causes page change */
      case CODE_DOWN:
                      if (current<(treeROWS-1) && current<(GV.dircount)) {
                          current++;
                          return 0;
                      }
                      return 1; /* causes page change */
    }
    return 1;
}

static int f_load(void)
{
    int i;

    init_prompt();
    prompt(" Directory tree to load ? ",answer,LNA64-12);
    muste_strupr(answer);
    if (!strlen(answer)) return 1;
    LOCATE(BOTTOMROW,1);
    return 0; /* causes return-chain to TREE.C */
}

// END WHERE //////////////////////////////////////////////////////////////////

#endif // TOISTAISEKSI_SIVUUTETTU

#if TOISTAISEKSI_SIVUUTETTU

// BEGIN DM /////////////////////////////////////////////////////////////////////

static int DMmain(void)
{
    if (g>1) { /* check possible help calls already here */
        if (!strcmp(word[1],"?")) {
            muste_kv_usage_info();
            return 1;
        }
    }
    GV.required=0;
    GV.status=GALLFILES;
    init_globals(); /* updates GV.required, not suitably for DM */
    GV.required=38;  /* " nnnnnnnn.ttt 12345678 DD.MM.YY HH:MM " */
    dirmatch();
    return 1;
}

static void dirmatch(void)
{
    int i,j,any;
    char *DMspec[2];

    df=(FIPtr *)malloc((size_t)(dmROWS+1)*sizeof(FIPtr)); /* only once */
    if (df==NULL) { no_mem(); return; }

    strcpy(edisk0,edisk); /* save current edisk */
    strncpy(drive0,edisk0,2); p0=&edisk0[2]; strcpy(dir0,p0);
    quick_update=0; /* enhanced & documented 13.7.1998 */
    i=spfind("DM");
    if (i>=0) {
        quick_yes=0; /* this must be defined before check_parameters */
        j=split(spb[i],DMspec,2);
        if (j>0) {
            if (!strcmp(DMspec[0],"UPDATE")) quick_update=1;
            else if (!strcmp(DMspec[0],"FILL")) quick_update=2;
            else if (!strcmp(DMspec[0],"BOTH")) quick_update=3;
        }
        if (j>1) quick_yes=(*DMspec[1]=='Y');
    }
    i=DMcheck_parameters();
    DDchange_disk(drive0,dir0); /* restore edisk (and  OS dir) */
    if (i<0) return;

    while (1) { /* changed to while() for exchange-function! 12.9.97 */
        ghostlink=0; /* see S.C */
        any=DMfind_files(0);
        if (any<0) break; /* 22.7.1998 */
        if (any) {
            DMsort_files();
            DMgroup_order_files();
            link_source_and_target(); DMinternal_sort();
            i=DMdisplay_files();
            free(tfiles); free(files);
        }
        if (i<0) break; /* error occurred */
        if (i==CODE_EXIT) break;
        if (i==DDCALL) return; /* 15.12.97 *//* 4.1.98*/
    }
    free(df);
}

static void dm_keys(void)
{
    init_remarks();
    rem_pr(" Browse by arrow keys, PgDn, PgUp, HOME and END. Also N=Next, P=Prev and");
    rem_pr(" PREFIX down can be used (PREFIX=F2). For action, press:");
    rem_pr("    +/- to mark/unmark a file (PREFIX+/- to mark/unmark all)");
    rem_pr("      C to copy a file or all marked files to target directory");
    rem_pr("      S to sort the files (also with specification SORT)");
    rem_pr("      G to group the files (also with specification GROUPING)");
    rem_pr("      M to display only the matching files");
    rem_pr("      A to display all files of source directory");
    rem_pr("      U to mark all newer files for target directory updating");
    rem_pr("      F to mark all files that are missing in target directory");
    rem_pr("      B to mark all but older files (combination of U and F)");
    rem_pr("      X to exchange source and target");
    rem_pr("      D to start DD from the source directory");
    rem_pr("  ENTER to display the contents of current file with SHOW");
    rem_pr("     F1 to display this help (`DM?' to get more help)");
    rem_pr("      > to give a system command");
    rem_pr("     F8 to exit");
    rem_pr(" ");
    rem_pr(" ");
    PR_EINV; WAIT;
}

static int DMcheck_parameters (void)
{
    int i;

    if (g<3) { muste_kv_usage_info(); return -1; }

/* Changed to use edisk instead of drive+dir 13.9.97/kv (see: DM.C!) */

    i=DMcheck_given_path(word[1],0); if (i<0) return -1;
    /* path will be parsed (no '.' or '..'!) and saved to edisk */
    i=DDchange_disk(drive,dir); if (i<0) return -1;
    if (!strlen(ext)) {
        sprintf(GV.source,"%s%s",edisk,"*.*");
    } else {
        sprintf(GV.source,"%s%s%s",edisk,fname,ext);
    }
    strcpy(GV.sourcedir,edisk);
    DDchange_disk(drive0,dir0); /* restore edisk (and DOS dir) */

    i=DMcheck_given_path(word[2],1); if (i<0) return -1;
    /* path will be parsed (no '.' or '..'!) and saved to edisk */
    i=DDchange_disk(drive,dir); if (i<0) return -1;
    if (!strlen(ext)) {
        sprintf(GV.target,"%s%s",edisk,"*.*");
    } else {
        sprintf(GV.target,"%s%s%s",edisk,fname,ext);
    }
    strcpy(GV.targetdir,edisk);

//muste_kv_s_err("Source=%s, Target=%s",GV.source,GV.target);
//muste_kv_s_err("Sourcedir=%s, Targetdir=%s",GV.sourcedir,GV.targetdir);

    if (!strcmp(GV.targetdir,GV.sourcedir)) {
        PR_EINV;
        muste_kv_s_err("Source directory can not be the target directory!");
        return -1;
    }
    return 1;
}

static int DMcheck_given_path (char *fstr, int level) /* 0=source 1=target */
{
    int wild,point,errlevel;

    strcpy(tmp, fstr);
//  if (!case_sensitive_pathnames) muste_strupr(tmp);
    wild=0; point=0; errlevel=1;
    if (strchr(tmp, '*') != NULL) wild=1;
    if (strchr(tmp, '?') != NULL) wild=1;
    if (strchr(tmp, '.') != NULL) point=1;

    if (strchr(tmp, ':') != NULL) { /* drive given */
        if (strlen(tmp)==2) strcat(tmp, "*.*\0"); /* only drive given */
        if (wild && !point) strcat(tmp, ".*\0");
        strcpy(fullspec, tmp);
    } else {
        if (wild && !point) strcat(tmp, ".*\0");
        if (tmp[0]!='\\') sprintf(fullspec, "%s%s", edisk, tmp);
        else sprintf(fullspec, "%c:%s", edisk[0], tmp);
    }
    strcpy(tmp,fullspec); tmp[2]='\0'; strcat(tmp, "*.*\0");

    if (_fullpath(path, tmp, LNAME-1) == NULL) {
        DDerror=BADPATH;
//      if (harderror_flag) { harderror_flag=0; DDerror=HARDERR; }
        display_error(fstr);
        return -1;
    }
    if (_fullpath(path, fullspec, LNAME-1) == NULL) {
        DDerror=BADPATH;
//      if (harderror_flag) { harderror_flag=0; DDerror=HARDERR; }
        display_error(fstr);
        return -1;
    }
    if( path[strlen(path)-1]=='.' && strlen(path) > 1 &&
       (path[strlen(path)-2]=='\\' || path[strlen(path)-2]=='.') )
           strcat(path,"\\");

    _splitpath(path, drive, dir, fname, ext);
    strcpy(fullspec, path);
    if (strlen(ext)==0) {
        if (strlen(fname)==0) { /* no file name - main directory */
            strcat(fullspec, "*.*");
        } else {
            strcat(fullspec, "\\*.*\0");
            strcat(fname, "\\");
            strcat(dir, fname);
        }
//      DDerror=_dos_findfirst(fullspec, DDA2, &fil);
        h_fil=_findfirst(fullspec, &fil);
//      if (DDerror) {
        if (h_fil == -1L) {
            DDerror=NOFILES;
/*printf("\nDDerror: NOFILES(1) (fullspec=%s)\n",fullspec);sur_getch();*/
#if 0
            if (harderror_flag) {
                harderror_flag=0;
                DDerror=HARDERR;
                display_error(path);
                return -1;
            }
#endif
//          DDerror=_dos_findfirst(path, DDA2, &fil);
            h_fil=_findfirst(path, &fil);
//          if (DDerror) {
            if (h_fil == -1L) {
/*printf("\nDDerror: NOFILES(2) (path=%s)\n",path);sur_getch();*/
                DDerror=NOFILES;
#if 0
                if (harderror_flag) {
                    harderror_flag=0;
                    DDerror=HARDERR;
                    errlevel=-1;
                } else {
#endif
                    DDerror=NOFILES;
                    if (level==1) {
                        errlevel=0;      /* this is not fatal */
                    } else {
                        errlevel=-1;
                    }
#if 0
                }
#endif
                if (quick_yes && errlevel==0) return errlevel; /* 13.7.1998 */
                display_error(path);
                return errlevel;
            } else {
                _findclose(h_fil);
            }
        } else {
            strcpy(path, fullspec); /* fixed fullspec ok; use it */
            _findclose(h_fil);
        }
    } else { /* path includes all parts, also extension */
//      DDerror=_dos_findfirst(path, DDA2, &fil);
        h_fil=_findfirst(path, &fil);
//      if (DDerror) {
        if (h_fil == -1L) {
/*printf("\nDDerror: NOFILES(3) (path=%s)\n",path);sur_getch();*/
#if 0
            if (harderror_flag) {
                harderror_flag=0;
                DDerror=HARDERR;
                errlevel=-1;
            } else {
#endif
                DDerror=NOFILES;
                if (level==1) {
                    errlevel=0;      /* this is not fatal */
                } else {
                    errlevel=-1;
                }
#if 0
            }
#endif
            if (quick_yes && errlevel==0) return errlevel; /* 13.7.1998 */
            display_error(path);
            return errlevel;
        } else {
            if (strchr(path, '*') != NULL) wild=1;
            if (strchr(path, '?') != NULL) wild=1;
            if (!wild) {
                if (SUBDIR(fil)) {
                    strcat(path, "\\*.*\0");
                    _splitpath(path, drive, dir, fname, ext);
                }
            }
            _findclose(h_fil);
        }
    }
    return 1;
}

static int DMfind_files(int mode)
{
    int i;
    int print_date; /* 22.7.1998 */
    char edisk0[LNAME/2]; /* 5.4.2001 */

    strcpy(edisk0,edisk);
    if (mode==0) { /* called from DM.C */
     /* source: */
        _chdir(GV.sourcedir);
        count=0; order_nr=1; GV.filecount=0; GV.totalcount=0;
        h_fil=_findfirst(GV.source, &fil);
        FL=(FIPtr)malloc(sizeof(Files));
        if (FL==NULL) { no_mem(); return -1; }
        FL->next=NULL; FLpp=FL;
        print_date=GV.print_date; /* save the DM-specific code */
        i=spfind("DATE"); if (i>=0) GV.print_date=get_date(spb[i]);
        do {
            if (SUBDIR(fil)) continue;
            i=restrictions(); if (i<0) return -1;     /* 22.7.1998 */
            if (i) continue;  /* restriction found, do not load file */
            i=DMfound_files(); if (i<0) return -1;
        } while(!_findnext(h_fil, &fil));
        _findclose(h_fil);
        GV.print_date=print_date; /* restore the DM-specific code */
        if (count==0) { no_valid_files(); return -1; }
        i=DMalloc_files(0); if (i<0) return -1;
        fi=&files[0];
        DMremove_list();
        GV.filecount=count; GV.totalcount=count;
    }

 /* target: */
    _chdir(GV.targetdir);
    count=0; order_nr=1; GV.tfilecount=0;
    h_fil=_findfirst(GV.target, &fil);
    if (h_fil != -1L) {
        FL=(FIPtr)malloc(sizeof(Files));
        if (FL==NULL) { no_mem(); return 0; }
        FL->next=NULL; FLpp=FL;
        do {
            if (SUBDIR(fil)) continue;
            i=DMfound_files(); if (i<0) return 0;
        } while(!_findnext(h_fil, &fil));
        _findclose(h_fil);
        i=DMalloc_files(1); if (i<0) return 0;
        fi=&tfiles[0];
        DMremove_list();
        GV.tfilecount=count;
    }
    strcpy(edisk,edisk0); _chdir(edisk);
    return 1;
}

static int DMfound_files(void)
{
    FLp=(FIPtr)malloc(sizeof(Files));
    if (FLp==NULL) { no_mem(); return -1; }
    FLp->next=NULL; FLpp->next=FLp; FLpp=FLp;
    flash_counter(count+1);
    FLp->attrib=fil.attrib;
    FLp->size=fil.size;

/* these lines copied from DD 5.4.2001 */
    strcpy(FLp->name,fil.name);    /* fil.name: (possibly) a long name */

//    WAPI_ShortPath(FLp->name);  /* FLp->name: short form of the name */

/*                                     */
    /* fil.name includes the original, possibly long file name */
    /* let's save it in the comment space (which was not used in DM) */
    strcpy(FLp->comment, fil.name); /* 5.4.2001 - see X.C */

    write_time = localtime(&fil.time_write);
    if (write_time==NULL) { /* 19.12.2000 (alihakemistot cd:illä!) */
        FLp->year   = 0;
        FLp->month  = 0;
        FLp->day    = 0;
        FLp->hour   = 0;
        FLp->minute = 0;
        FLp->second = 0;
    } else {
        FLp->year   = write_time->tm_year; /* current year - 1900 */
        FLp->month  = write_time->tm_mon+1;
        FLp->day    = write_time->tm_mday;
        FLp->hour   = write_time->tm_hour;
        FLp->minute = write_time->tm_min;
        FLp->second = write_time->tm_sec;
    }
    FLp->status=0x00;
    FLp->grouporder=0; /* will be updated in sort.c */
    FLp->order=++order_nr;
//  if (!case_sensitive_pathnames) muste_strupr(FLp->name);
    count++;
    return 1;
}

static int DMalloc_files(int table)
{
    if (table==0) { /* source */
        files=(Files *)malloc((size_t)count*sizeof(Files));
        if (files==NULL) { no_mem(); return -1; }
    } else { /* target */
        if (count==0) return 1; /* 22.7.1998 */
        tfiles=(Files *)malloc((size_t)count*sizeof(Files));
        if (tfiles==NULL) { no_mem(); return -1; }
    }
    return 1;
}

static void DMremove_list(void)
{
    FLp=FL->next;
    while (FLp!=NULL) {
        fi->attrib=FLp->attrib;
        fi->size=FLp->size;
        strcpy(fi->name,FLp->name);
        strcpy(fi->comment,FLp->comment); /* 5.4.2001 */
        fi->year=FLp->year;
        fi->month=FLp->month;
        fi->day=FLp->day;
        fi->hour=FLp->hour;
        fi->minute=FLp->minute;
        fi->second=FLp->second;
        fi->status=FLp->status;
        fi->grouporder=FLp->grouporder;
        fi->order=FLp->order;
        FLpp=FLp;
        FLp=FLp->next;
        free(FLpp); fi++;
    }
    free(FL);
}

static void DMsort_files (void)
{
    int i,desc;
    size_t num;
    int (*comp)(const void *, const void *);

    comp=comp1; desc=(GV.sorting[0]=='-' || GV.sorting[1]=='-');
         if(strstr(GV.sorting,"OS") !=NULL) comp=(desc)?comp1b:comp1;
    else if(strstr(GV.sorting,"DATE")!=NULL) comp=(desc)?comp2b:comp2;
    else if(strstr(GV.sorting,"TIME")!=NULL) comp=(desc)?comp2b:comp2;
    else if(strstr(GV.sorting,"SIZE")!=NULL) comp=(desc)?comp3b:comp3;
    else if(strstr(GV.sorting,"NAME")!=NULL) comp=(desc)?comp4b:comp4;
    else if(strstr(GV.sorting,"TYPE")!=NULL) comp=(desc)?comp5b:comp5;
    else if(strstr(GV.sorting,"MARK")!=NULL) comp=(desc)?comp9b:comp9;
    else if(strstr(GV.sorting,"COMM")!=NULL) comp=(desc)?comp10b:comp10;
    else {
        if (strlen(GV.sorting)) { /* errorneous option */
          disp_err(" Sorting option %s is unknown!", GV.sorting);
          for (i=0; i<SORTOPTLEN; i++) GV.sorting[i]='\0';
        }
    }

    fi=&files[0]; /* source */
    qsort((void *)fi, GV.filecount, NFILES, comp1); /* all by  OS */

    if (comp==comp1 || comp==comp1b) {
        if (desc) strcpy(GV.sorting,"-"); else strcpy(GV.sorting,"");
        strcat(GV.sorting,"OS");
        if (comp==comp1) return;
    }
    num=(desc) ? 5 : 4; GV.sorting[num]='\0';

    fi=&files[0]; /* source */
    qsort((void *)fi, GV.filecount, NFILES, comp);
    return;
}

static void DMgroup_order_files(void)
{
    int i,j,k,the_rest,groups;
    char typ[TYPELEN];
    char *grouptypes[MAXGROUPS];
    char given_grouping[GRPOPTLEN];
    char *x;

    strcpy(given_grouping,GV.grouping);
    groups=split(given_grouping,grouptypes,MAXGROUPS);
 /* source */
    for (i=0,k=0,the_rest=0; i<=groups; i++) {
        strcpy(typ,".");
        if (i==groups) {
            the_rest=1;
        } else {
            strcat(typ, grouptypes[i]);
            muste_strupr(typ);
        }
        for (j=0, f=&files[0]; j<GV.filecount; j++,f++) {
            if (FileSorted(f)) continue;
            strcpy(sbuf,f->comment); /* use original name */
            muste_strupr(sbuf);
            x=strstr(sbuf,typ);
//muste_kv_s_err("DM: ***typ=%s   sbuf=%s x=%s",typ,sbuf,x);
            if ((x!=NULL && !strncmp(x,typ,3)) || the_rest) {
                f->grouporder=k++;
                f->status=(f->status | FSORTED);
            }
        }
    }
    if (groups>0) {
        fi=&files[0]; /* source */
        qsort((void *)fi, GV.filecount, NFILES, comp6);
    }
}

static void link_source_and_target(void)
{
    int i,j,k;
    int match_found; // 12.5.2003

    f=&tfiles[0]; /* target pointer */
    for (j=0; j<GV.tfilecount; j++,f++) f->match=0;
    fi=&files[0]; /* source pointer */
    for (i=0,k=0; i<GV.filecount; i++) { /* loop all source files */
        match_found=0;
        fi->match=1; fi->next=NULL;
        f=&tfiles[0]; /* target pointer */
        for (j=0; j<GV.tfilecount; j++) {

            /* rewritten 12.5.2003 */

            if (strchr(fi->name ,'~')!=NULL && strchr(f->name ,'~')!=NULL) {
                /* shortened name always includes ~ */
                /* compare the long names saved in comments (case-sensitive): */
                if (!strcmp(fi->comment, f->comment)) match_found=1;
            } else {
                /* compare the short names (not case sensitive): */
                if (!muste_strcmpi(fi->name,f->name)) match_found=1;
            }
            if (match_found) {
                fi->match=comp2(f,fi)+3; /* see comp2() and below */
                fi->next=f; /* link */
                f->match=1; k++;
//muste_kv_s_err("source name=%s|i=%d|\ntarget name=%s|k=%d|",fi->name, i, f->
//muste_kv_s_err("source long=%s|i=%d|\ntarget long=%s|k=%d|",fi->comment, i,
                break; /* from inner for() */
            }

// [this replaced by the above code]
//
//          if (!muste_strcmpi(fi->name,f->name) || !strcmp(fi->comment, f->comment)) { // 7.1.2002
//              // 15.12.2001 compare the long file names
//              // (as the short ones might be the same!)
//              if (strchr(fi->name ,'~')!=NULL) { // shortened name? 19.5.2002
//                  if (strcmp(fi->comment, f->comment)) continue;
//              }
//
//              fi->match=comp2(f,fi)+3; /* see comp2() and below */
//              fi->next=f; /* link */
//              f->match=1; k++;
//muste_kv_s_err("source name=%s|i=%d|\ntarget name=%s|k=%d|",fi->name, i, f->name, k);
//muste_kv_s_err("source long=%s|i=%d|\ntarget long=%s|k=%d|",fi->comment, i, f->comment, k);
//              break; /* from inner for() */
//          }


            f++;
        } /* for target */
        fi++;
    } /* for source */

/* Link non-matching TARGET files:   (12.9.97) */
    if (ghostlink) return; /* each copied file matches, */
                           /* so do not return here unless 'X' used */
    GV.dircount=GV.tfilecount-k; /* use dircount for this purpose */
    ghostcount=GV.dircount; /* needed when toggling between A/M */

    if (GV.dircount) {
        files=(Files *)realloc((Files *)files,
                 (GV.totalcount+GV.dircount)*sizeof(Files));
        if (files==NULL) { no_mem(); exit(1); }
        fi=&files[GV.totalcount-1+1];
        for (i=0; i<GV.dircount; i++,fi++) {
            f=&tfiles[0];
            fi->match=5; /* see below! */
            fi->status=FNOTFILE;
            fi->attrib='0';
            fi->grouporder=0;         /*  ??  */
            fi->order=0;              /*  ??  */
            while (f->match) f++; f->match=1;
            fi->next=f; /* link */
        }
    }
    ghostlink=1;
}

/*
   Mahdollisuudet:
   Source             Target        'match' (Source) 'next'
   On                 Ei ole         1                 -
   On uudempi         On vanhempi    2                 target address
   On sama            On sama        3                 target address
   On vanhempi        On uudempi     4                 target address
   Ei ole             On             5                 target address
   Viimeinen rivi otettu käyttöön 98:ssa 12.9.97/kv
     (Juha Valtosen ehdotuksesta)
 */

static void DMinternal_sort(void)
{
    fi=&files[0];
    qsort((void *)fi, (size_t)GV.filecount, NFILES, comp8);
}

static int DMdisplay_files(void)
{
    int i,m,row,dfi,msgnr;
    unsigned int ui;
    char *msg;

    row=dmSTARTROW; msgnr=1;
    current=markcount=dfi=0;
    markbytes=0L;

    i=spfind("DISPLAY");
    if (i>=0) {
        if (!strncmp(spb[i],"MATCHING",1)) GV.status=GMATCHING;
    }
    DMcount_totalbytes(); /* updates also matchcount */

    if (GV.status==GMATCHING) {
        if (GV.filecount) DMinternal_sort();
        GV.filecount=matchcount;
        ghostcount=GV.dircount;
        GV.dircount=0;
        if (GV.filecount) {
            DMsort_files(); DMgroup_order_files();
        }
        DMcount_totalbytes();
        count_marked_files();
    }
    if (quick_update && GV.filecount) { /* added 1.12.1997 */
        if (quick_update==1 || quick_update==3) mark_newer_files(); /* 13.7.1998 */
        if (quick_update==2 || quick_update==3) mark_lonely_files();
        count_marked_files();
        fi=&files[0];
        if (markcount==0) { /* added 15.12.1997 */
            strcpy(sbuf, " Nothing ");
            switch(quick_update) {
                case 1: strcat(sbuf, "to update."); break;
                case 2: strcat(sbuf, "to fill."); break;
                case 3: strcat(sbuf, "to update or fill."); break;
            }
            disp_err(sbuf);
            return CODE_EXIT;
        }
    }
    DMclear_screen();
    fi=&files[0];

    while (1) {
        if (GV.filecount) {
            df[dfi]=fi; f=fi;
            DMget_line();
            fi=df[dfi]; f=fi;
            DMdisplay_line(dfi,row,strlen(line));
            fi=df[dfi];
        }
        if ( GV.filecount==0 || row==ENDROW || LastFile2(fi) ) {
            DMinfoline();
            switch(msgnr) { /* modified 13.7.98 (1 msg added) */
              case 1: msg=dmMsg1; break;
              case 2: msg=dmMsg2; break;
              case 3: msg=dmMsg3; break;
            }
            if (++msgnr>3) msgnr=1;
            DMshow_mode_code();
            if (quick_update && GV.filecount) { /* added 1.12.1997 */
                DMf_copy();
                return CODE_EXIT;
            }
            m=nextch(msg);
            ui=toupper((unsigned char)m);
            i=DMhandle_key(ui);
            switch(i) {
              case  0: return m;  /* returns CODE_* to DM.C */
              case -1: return -1; /* returns -1 to DM.C -> exit */
              case DDCALL: return DDCALL; /* 14.7.1998 */
              default: break;
            }
            row=dmSTARTROW; dfi=0;
        } else {
            row++; dfi++; fi++;
        }

    }
}

static int DMhandle_key(unsigned int m)
{
    int i;

    f=df[current];
    switch (m) {
      case CODE_EXIT:   return 0;
      case CODE_HELP:   DMf_help(); break;
      case COPY:        if (GV.filecount) {
/* changed 15.12.1997 */  if (markcount==0 && FileNotFile(f)) break;
/* to allow F2-C always*/ i=DMf_copy();
                          if (i) { /* 12.9.97 */
                            DMfind_files(1);
                            link_source_and_target();
                            DMcount_totalbytes();
                            source_and_target();
                          }
/* this was not */        if (quick_yes && !markcount) { /* 13.7.1998 */
/* intended, it just */     i=DMf_key(CODE_DOWN);
/* came by accident :) */   if (i) {
                                if (!LastFile2(f)) fi=df[1];
                                else fi=df[0];
                                return m;
                            }
                          }
                        }
                        break;
      case CODE_RETURN: if (GV.filecount) { /* ENTER added 12.9.97 */
                          i=DMf_show();
                          if (i<0) return -1; /* 15.12.1997 */
                        }
                        break;
      case CODE_UP:     if (GV.filecount) {
                          if (DMf_key(CODE_UP)) {
                              fi=df[0]; f=fi;
                              if (!FirstFile(f)) fi--;
                              return m;
                          }
                        }
                        break;
      case CODE_DOWN:
      case MARK:
      case MARK2:
      case UNMARK:      if (GV.filecount) {
                          switch(m) {
                              case CODE_DOWN: i=DMf_key(CODE_DOWN); break;
                              case MARK:
                              case MARK2:     i=DMf_key(MARK); break;
                              case UNMARK:    i=DMf_key(UNMARK); break;
                          }
                          if (i) {
                              if (!LastFile2(f)) fi=df[1]; else fi=df[0];
                              return m;
                          }
                        }
                        break;
      case CODE_HOME:   if (GV.filecount)
                          if (DMf_key(CODE_HOME)) return m;
                        break;
      case CODE_END:    if (GV.filecount) DMf_key(CODE_END);
                        return m;
      case CODE_NEXT:
      case NEXT:        if (GV.filecount)
                          if (DMf_key(CODE_NEXT)) return m;
                        break;
      case CODE_PREV:
      case PREV:        if (GV.filecount) DMf_key(CODE_PREV);
                        return m;
      case SORT:
      case GROUP:       if (GV.filecount) {
                          switch(m) {
                              case SORT:  f_sort(); break;
                              case GROUP: f_group(); break;
                          }
                          mark_files_unsorted();
                          DMsort_files(); DMgroup_order_files();
                          link_source_and_target();
                          fi=&files[0]; current=0;
                        }
                        return m;
      case CODE_PRE:    if (GV.filecount) {
                          i=nextch(dmMsgP); /* show PREFIX-message here */
                          switch(i) {
                              case MARK:
                              case MARK2:     DMf_key(MARKALL); break;
                              case UNMARK:    DMf_key(UNMARKALL); break;
                              case CODE_DOWN: DMf_key(PREDOWN); return m;
                              default:        break;
                          }
                        }
                        break;
// muste_system(char *komento, int wait) / RS 8.7.2011 ks.!
      case SYSTEM:      system_call(); DMclear_screen();
                        break;
      case MATCHING:
      case ALLFILES:    if (m==MATCHING) {
                            GV.status=GMATCHING;
                            if (GV.filecount) DMinternal_sort();
                            GV.filecount=matchcount;
                            ghostcount=GV.dircount;
                            GV.dircount=0;
                        } else {
                            GV.status=GALLFILES;
                            GV.filecount=GV.totalcount;
                            GV.dircount=ghostcount;
                        }
                        if (GV.filecount) {
                          mark_files_unsorted();
                          DMsort_files(); DMgroup_order_files();
                        }
                        DMcount_totalbytes();
                        count_marked_files();
                        fi=&files[0]; current=0; DMclear_screen();
                        return m;
      case EXCHANGE:    strcpy(sbuf,GV.source);
                        strcpy(GV.source,GV.target);
                        strcpy(GV.target,sbuf);
                        strcpy(sbuf,GV.sourcedir);
                        strcpy(GV.sourcedir,GV.targetdir);
                        strcpy(GV.targetdir,sbuf);
                        return 0; /* -> DM.C */
      case DDCALL:      /* 15.12.1997 - start DD from source dir */
                        word[1]=GV.sourcedir;
                        /* 17.1.1999 - remember marked files */
                        strcpy(info,"DM_DD");
                        i=save_marked_files_for_DD();
                        if (i<0) *info='\0'; /* no success */
                        DDmain();
                        return DDCALL; /* changed from 0 14.7.1998 */
      case UPDATE:
      case FILL:        /* 13.7.1998 - suggestion by J.Valtonen */
      case BOTH:        /* 13.7.1998 - elaboration by kv */
                        if (GV.filecount) {
                            DMf_key(UNMARKALL);
                            if (m==BOTH || m==UPDATE) mark_newer_files();
                            if (m==BOTH || m==FILL) mark_lonely_files();
                            count_marked_files();
                            fi=&files[0]; current=0; DMclear_screen();
                        }
                        return m;
      default:          break;
    }
    fi=df[0];
    return m;
}

static void DMdisplay_line(int dfi, int row, int len)
{
    char color1, color2;

    if (f->match==4) color1=FileColor2; else color1=FileColor1;
    if (f->match==1) color2=BackGround;
    else if (f->match==2) color2=FileColor2;
    else if (f->match>2) color2=FileColor1;
    if (f->match==5) { color1=BackGround; color2=FileColor1; }

    if (dfi==current) {
        if (ReadOnlyFile(f) || SystemFile(f) || HiddenFile(f) || f->match==4)
            color1=AttribCursor;
        else color1=Cursor;

        // 21.4.2003
        write_string(space,ScreenWidth,' ',WORKROW3+1,1);
        if (FileNotFile(f)) sprintf(answer, "%s%s", GV.targetdir, f->next->comment);
        else sprintf(answer, "%s%s", GV.sourcedir, f->comment);
        write_string(answer,strlen(answer),' ',WORKROW3+1,3);

    } else {
        if (ReadOnlyFile(f) || SystemFile(f) || HiddenFile(f))
            color1=AttribColor;
    }

    if (f->next!=NULL) {
      if (ReadOnlyFile(f->next) ||
            SystemFile(f->next) ||
            HiddenFile(f->next)) color2=AttribColor;
    }
    DMdrawline(color1,row,len,color2);
}

static void DMdrawline(char color1, int row, int len, char color2)
{
    char *p;
    char lc1,lc2,lc3,lc4; /* 13.7.1998 */
    char ls1[3],ls2[3];

    write_string(space,ScreenWidth,BackGround,row,1);
    write_string(line,GV.required,color1,row,2);

    switch(f->match) { /* 13.7.1998 *//* see table in S.C: _S S 142 / */
        case 1: lc1='ƒ'; lc2='ƒ'; lc3='Û'; lc4='Û';
                strcpy(ls1,"  "); strcpy(ls2,"->"); break;
        case 2: lc1='´'; lc2='´'; lc3='Û'; lc4='Û';
                strcpy(ls1,"--"); strcpy(ls2,"->"); break;
        case 3: lc1='ƒ'; lc2='ƒ'; lc3='´'; lc4='´';
                strcpy(ls1,"--"); strcpy(ls2,"->"); break;
        case 4: lc1='ƒ'; lc2='ƒ'; lc3='ý'; lc4='ý';
                strcpy(ls1,"--"); strcpy(ls2,"->"); break;
       default: break;
    }

    if (f->match > 1) { /* see table in S.C: _S S 142 / */
        drawlink(ls1, lc1, lc2, row);
        write_string(space,GV.required,color2,row,80/2+2);
        p=&line[GV.required+3];
        if (f->match == 5) { /* see table in S.C: _S S 142 / */ /* 7.3.2001 */
            if (color1==Cursor) color2=Cursor;
            if (color1==AttribCursor) color2=AttribCursor;
        }
        write_string(p,GV.required,color2,row,80/2+2);
    }

    if (FileMarked(f)) {
        switch(color1) {
          case FileColor1:
          case AttribColor:  write_string(MarkStr,1,MarkColor1,row,2);
                             break;
          case FileColor2:   write_string(MarkStr,1,MarkColor3,row,2);
                             break;
          case Cursor:
          case AttribCursor: write_string(MarkStr,1,MarkColor2,row,2);
                             break;
        }
        drawlink(ls2, lc3, lc4, row);
    }
}

static void drawlink(char *linkstr, char color1, char color2, int row) /* 13.7.98 */
{
    char *p;

    if (FileNotFile(f)) return;
    p=&linkstr[0];
    write_string(p,1,color1,row,80/2);
    p=&linkstr[1];
    write_string(p,1,color2,row,80/2+1);
    return;
}

static void source_and_target(void)
{
    LOCATE(BOTTOMROW,1);
    ReversedWorkRow1;
    sprintf(line, " %d file%s, %u byte%s.",
      GV.filecount, (GV.filecount!=1)?"s":"",
      totalbytes, (totalbytes!=1L)?"s":"");
    write_string(line,strlen(line),Reverse,WORKROW1,1);
    sprintf(line, " %d file%s, %u byte%s.",
      GV.tfilecount, (GV.tfilecount!=1)?"s":"",
      ttotalbytes, (ttotalbytes!=1L)?"s":"");
    write_string(line,strlen(line),Reverse,WORKROW1,80/2+1);
    ReversedWorkRow2;
    sprintf(line," Source: %s ", GV.sourcedir);
    write_string(line,strlen(line),Reverse2,WORKROW2,2);
    sprintf(line," Target: %s ", GV.targetdir);
    write_string(line,strlen(line),Reverse2,WORKROW2,80/2+2);
}

static void DMinfoline(void)
{
    LOCATE(BOTTOMROW,1);
    ReversedWorkRow3;
    if (markcount>0) {
        sprintf(line, " Marked: %d file%s, %u byte%s.",
          markcount, (markcount!=1)?"s":"",
          markbytes, (markbytes!=1L)?"s":"");
        write_string(line,strlen(line),Reverse,WORKROW3,1);
    }
    if (matchcount>0) {
        sprintf(line, " Matching: %d file%s, %u byte%s.",
          matchcount, (matchcount!=1)?"s":"",
          matchbytes, (matchbytes!=1L)?"s":"");
        write_string(line,strlen(line),Reverse,WORKROW3,80/2+1);
    }
}

static void DMclear_screen(void)
{
    clear_screen();
    source_and_target();
}

static void DMget_line(void)
{
    char datetime[STRMAXL], namesize[STRMAXL];

    if (FileNotFile(f)) { /* ghosts on source side   12.9.97 */
        sprintf(line, "                                       ");
                     /*  1+21+1+8+1+5+1+1=39                  */
    } else {
        DMmake_file_name_and_size(namesize);
        DMmake_date_and_time(datetime);
        sprintf(line," %s %s ",namesize,datetime);
    }
    if (f->next!=NULL) {
        f=f->next; fi=f;
        DMmake_file_name_and_size(namesize);
        DMmake_date_and_time(datetime);
        strcat(line, "   ");
        strcat(line,namesize); strcat(line, " ");
        strcat(line,datetime); strcat(line, "  ");
    }
}

static void DMmake_file_name_and_size(char *str)
{
    int i;
    char bytes[STRMAXL], size_str[STRMAXL],
    file_tmp[STRMAXL], file_str[STRMAXL]; /* 21.7.1998 */ // 15.12.2001

    for (i=0; i<STRMAXL; i++) { bytes[i]='\0'; size_str[i]='\0'; }
    strcpy(file_tmp, f->name);
    sprintf(file_str, "%-12s", file_tmp);
    muste_itoa(f->size,bytes,10); /* size to a string */
    sprintf(size_str, "%9s", bytes);
    sprintf(str, "%s%s", file_str, size_str);
}

static void DMcount_totalbytes(void)
{
    int i;
    totalbytes=ttotalbytes=matchbytes=0L;
    matchcount=0;
    for (i=0,fi=&files[0];i<GV.filecount;i++,fi++) {
        if (!FileNotFile(fi)) {
            totalbytes+=fi->size;
            if (fi->next!=NULL) {
                matchcount++;
                matchbytes+=fi->next->size;
            }
        }
    }
    for (i=0,fi=&tfiles[0];i<GV.tfilecount;i++,fi++)
        ttotalbytes+=fi->size;
}

static void DMshow_mode_code(void)
{
    char sign[2];
    if (GV.status==GALLFILES) strcpy(sign,"A");
    if (GV.status==GMATCHING) strcpy(sign,"M");
    write_string(sign,1,Reverse,1,ScreenWidth);
}

static void count_marked_files(void)
{
    int i;
    markcount=0; markbytes=0L;
    f=&files[0];
    for (i=0; i<GV.filecount; i++,f++) {
        if (FileMarked(f)) {
            markcount++;
            markbytes+=f->size;
        }
    }
}

static void mark_newer_files(void)
{
    int i;
    f=&files[0];
    for (i=0; i<GV.filecount; i++,f++) {
        if (f->match==2) f->status=(f->status | FMARKED);
    }
}

static void mark_lonely_files(void) /* 13.7.1998 */
{
    int i;
    f=&files[0];
    for (i=0; i<GV.filecount; i++,f++) {
        if (f->match==1) f->status=(f->status | FMARKED);
    }
}

static int save_marked_files_for_DD(void) /* 17.1.1999 (J.Valtonen-suggestion) */
{
    int i;
    FILE *outf;

    sprintf(sbuf,"%s%s%s",etmpd,info,".TMP"); /* info saved above */
    outf=muste_fopen(sbuf,"w");
    if (outf==NULL) {
        disp_err(" Could not write selections to file %s!",sbuf);
        return -1;
    }
    f=&files[0];
    for (i=0; i<GV.filecount; i++,f++) {
        if (FileMarked(f) && !FileNotFile(f)) {
            sprintf(sbuf,"%s\n", f->name);
            fprintf(outf,sbuf);
        }
    }
    fclose(outf);
    return 1;
}


static void DMf_help(void)
{
    int r_was,r1_was;
    init_prompt();
    write_string(space,ScreenWidth,' ',WORKROW3+1,1);
    strcpy(answer," Key codes in Directory Matcher (DM):");
    write_string(answer,strlen(answer),' ',WORKROW3+1,1);
    r1_was=r1; r1=r1+r-1; r_was=r; r=4; s_end(siirtop);
    dm_keys();
    r1=r1_was; r=r_was; s_end(siirtop);
    DMclear_screen();
}

static int DMf_key(int key)
{
    int i,maxi,lastfound,page_changed,clr,pg1,pg2;

    switch(key) {
      case CODE_HOME: if (current==0) {
                          fi=&files[0];
                          return 1;
                      }
                      current=0;
                      return 0;
      case CODE_END:  for (i=0; i<2; i++) DMf_key(PREDOWN);
                      break;
      case CODE_NEXT: f=df[current]; fi=f;
                      if (LastFile2(f)) return 0;
                      lastfound=page_changed=clr=pg1=0;
                      maxi=min(dmROWS,GV.filecount+GV.dircount);

                      for (i=0; i<maxi; f++,fi++,i++) {
                          if (i>current) pg1++;
                          if (LastFile2(f)) { lastfound=1; break; }
                      }
                      pg2=i-pg1;
                      page_changed=(current+i) > dmROWS-1;

                      if (lastfound) {
                          if (page_changed) {
                              fi=df[current]; current=0;
                          } else {
                              fi=df[0]; current+=i;
                          }
                      } else {
                          if (page_changed) {
                              for (i=current; i>0; fi--,i--) ;
                              if (pg1+pg2 < current) current=pg1+pg2;
                          }
                      }
                      if (page_changed) {
                          for (i=0,f=fi; i<maxi; f++) {
                              if (LastFile2(f)) { clr=1; break; }
                              i++;
                          }
                      }
                      if (clr) DMclear_screen();
                      return 1;
      case CODE_PREV: f=df[0];
                      for (i=0; i<dmROWS; i++,f--) {
                          if (FirstFile(f)) {
                              if (i>0) DMclear_screen();
                              current=0;
                              break;
                          }
                      }
                      fi=f;
                      return 1;
      case PREDOWN:   f=df[current]; fi=df[0];
                      if (LastFile2(f)) break;
                      maxi=min(dmROWS-1,GV.filecount+GV.dircount-1);
                      if (current<maxi) {
                          while (!LastFile2(f)) {
                              f++;
                              current++;
                              if (current==maxi) break;
                          }
                      } else {
                          do { f++; fi++; } while (!LastFile2(f));
                      }
                      break;
      case CODE_UP:   if (current>0) {
                          current--;
                          return 0;
                      }
                      return 1; /* causes page change */
      case CODE_DOWN: f=df[current];
                      if (LastFile2(f)) return 0;
                      if (current<(dmROWS-1) &&
                            current<(GV.filecount+GV.dircount-1)) {
                          current++;
                          return 0;
                      }
                      return 1; /* causes page change */
      case MARKALL:   markcount=0; markbytes=0L; f=&files[0];
                      for (i=0; i<GV.filecount; i++,f++) {
                          f->status=(f->status | FMARKED);
                          markcount++;
                          markbytes+=f->size;
                      }
                      break;
      case UNMARKALL: f=&files[0];
                      for (i=0; i<GV.filecount; i++,f++)
                          f->status=(f->status & ~FMARKED);
                      markcount=0; markbytes=0L;
                      break;
      case MARK:      f=df[current];
                      if (!FileMarked(f) && !FileNotFile(f)) {
                          f->status=(f->status | FMARKED);
                          markcount++;
                          markbytes+=f->size;
                      }
                      if (FileNotFile(f)) return 0;
                      return DMf_key(CODE_DOWN);
      case UNMARK:    f=df[current];
                      if (FileMarked(f) && !FileNotFile(f)) {
                          f->status=(f->status & ~FMARKED);
                          markcount--;
                          markbytes-=f->size;
                      }
                      if (FileNotFile(f)) return 0;
                      return DMf_key(CODE_DOWN);
    }
    return 1;
}

static int DMf_copy(void)
{
    int i,j,k,manyfiles,owrall;
    char *token;

    manyfiles=markcount;
    init_prompt();
    if (manyfiles) {
        f=&files[0];
        sprintf(tmp, " Copy %d marked file%s from %s to %s ? ",
            markcount,(markcount>1)?"s":"",
            GV.sourcedir, GV.targetdir);
    } else {
        f=df[current];
  //    sprintf(path,"%s%s",GV.sourcedir,f->name);
  //    sprintf(path,"%s%s",GV.sourcedir,f->comment); // 5.4.2001 (see W.C)
        sprintf(path,"%s", f->comment); // 21.4.2003 (more room for long names)
        sprintf(tmp," Copy %s to %s ? ", path, GV.targetdir);
    }
    if (!quick_yes) { /* 13.7.1998 */
        show_yesno(tmp);
        j=ask_yesno(tmp,"Y");
        if (j<0) return 0;
    }
    WhiteBottomRow; LOCATE(BOTTOMROW,1);

    muste_kv_s_disp(" Copying...");
    owrall=0; /* do not overwrite all by default */
    if (quick_update) owrall=1; /* 1.12.1997 */
    if (quick_yes) owrall=1; /* 13.7.1998 */ /* for 'NORMAL,Y' */

    for (i=0,k=0; i<GV.filecount; f++) {
        if (manyfiles) {
            if (!FileMarked(f)) { i++; continue; }
            WhiteBottomRow;
            LOCATE(BOTTOMROW,1);
            muste_kv_s_disp(" Copying %s...",f->comment);
            LOCATE(BOTTOMROW,72);
            muste_kv_s_disp("(%d)",++k);
        } else {
            k++;
            if (i>0) break; /* only current file copied! */
        }
        i++;

        sprintf(path,"%s%s",GV.sourcedir,f->comment);
        sprintf(line,"%s%s",GV.targetdir,f->comment);

        if (!quick_yes) { /* 13.7.1998 */
            if (SystemFile(f) || HiddenFile(f)) {
                sprintf(tmp," Attributes prevent copying of file %s.",path);
                WhiteBottomRow; LOCATE(BOTTOMROW,1);
                if (manyfiles) {
                    strcat(tmp, " Continue ? ");
                    show_yesno(tmp);
                    j=ask_yesno(tmp,"Y");
                    if (j<0) return -1;
                } else {
                    disp_err(tmp);
                    return -1;
                }
            }
        }
        if (!owrall) {
            h_fil=_findfirst(line, &fil);
            if (h_fil != -1L) {
                _findclose(h_fil);
                if (!SUBDIR(fil)) {
                    sprintf(tmp," File %s exists. Overwrite ? ",line);
                    WhiteBottomRow; LOCATE(BOTTOMROW,1);
                    write_string("(Yes/No/All/Quit)",
                      17,Advice,BOTTOMROW,strlen(tmp)+4);
                    strcpy(sbuf,"Y");
                    prompt(tmp,sbuf,1); /* yes/no is not enough here! */
                    WhiteBottomRow;
                    switch (sbuf[0]) {
                      case 'Y': case 'y': break;
                      case 'N': case 'n': continue;
                      case 'A': case 'a': owrall=1; break;
                       default: return -1;
                    }
                }
            }
        }

        LOCATE(BOTTOMROW,1); WhiteBottomRow;
        muste_kv_s_disp(" Copying %s...",f->comment);
        LOCATE(BOTTOMROW,ScreenWidth-8);
        muste_kv_s_disp("(%d)",k);
        sur_copy_file(path, line);
    }
    return 1;
}

static int DMf_show(void)
{
    int i,r_was,r1_was;
    char userline2[LLENGTH];

    init_prompt();
    write_string(space,ScreenWidth,' ',WORKROW3+1,1);
 // if (FileNotFile(f)) sprintf(tmp, "%s%s", GV.targetdir,f->next->comment);
 // else sprintf(tmp, "%s%s", GV.sourcedir,f->comment);
 // sprintf(answer, "       File %s", tmp);
 // above replaced 21.4.2003 with:
    if (FileNotFile(f)) sprintf(answer, "%s%s", GV.targetdir, f->next->comment);
    else sprintf(answer, "%s%s", GV.sourcedir, f->comment);

    write_string(answer,strlen(answer),' ',WORKROW3+1,3); // cf. O.C

    if (FileNotFile(f)) sprintf(tmp, "%s%s", GV.targetdir,f->next->name);
    else sprintf(tmp, "%s%s", GV.sourcedir,f->name);
    sprintf(answer,"SHOW %s", tmp);
    edread(userline2,r1+r+3-1);
    edwrite(space,r1+r+3-1,1);
    edwrite(answer,r1+r+3-1,1);
    r1_was=r1; r1=r1+r-1; r_was=r; r=1+3; s_end(siirtop);
    i=suorita("_SHOW.EXE"); if (i<0) return -1; /* 15.12.1997 */
    r1=r1_was; r=r_was; s_end(siirtop);
    edwrite(userline2,r1+r+3-1,0);
    DMclear_screen();
    return 1;
}

static void DMmake_date_and_time(char *dtstr)
{
    int i;
    char dstr[STRMAXL], tstr[STRMAXL];
    unsigned year;
    unsigned YYYY;

    year = fi->year;
    if(year>99) year-=100; /* Y2K, 23.7.1997 */
    YYYY = fi->year+1900; /* 21.7.1998 */
    for (i=0; i<STRMAXL; i++) { dstr[i]='\0'; tstr[i]='\0'; }

    switch(GV.print_date) { /* 21.7.1998 */
/*
        0              -> DD.MM.YY     1
        1   DD.MM.YY   -> DD.MM.YY     1
        2   DD/MM/YY   -> DD/MM/YY     2
        3   DDMMYY     -> DDMMYYYY     9
        4   YYMMDD     -> YYYYMMDD    10
        5   MM/DD/YY   -> MM/DD/YY     5
        6   DD.MM.YYYY -> DDMMYYYY     9
        7   DD/MM/YYYY -> DDMMYYYY     9
        8   MM/DD/YYYY -> DDMMYYYY     9
        9   DDMMYYYY   -> DDMMYYYY     9
       10   YYYYMMDD   -> YYYYMMDD    10
 */
        case 0: GV.print_date=1; break;
        case 1: case 2: case 5: case 9: case 10: break;
        case 3: case 6: case 7: case 8: GV.print_date=9; break;
        case 4: GV.print_date=10; break;
       default: break;
    }
    switch (GV.print_date) {
       case 1: sprintf(dstr,"%.2d.%.2d.%.2d ",fi->day,fi->month,year);
               break;
       case 2: sprintf(dstr,"%.2d/%.2d/%.2d ",fi->day,fi->month,year);
               break;
#if 0
       case 3: sprintf(dstr,"%.2d%.2d%.2d ",fi->day,fi->month,year);
               break;
       case 4: sprintf(dstr,"%.2d%.2d%.2d ",year,fi->month,fi->day);
               break;
#endif
       case 5: sprintf(dstr,"%.2d/%.2d/%.2d ",fi->month,fi->day,year);
               break;
#if 0
       case 6: sprintf(dstr, "%.2d.%.2d.%.2d ", fi->day, fi->month, YYYY);
               break;
       case 7: sprintf(dstr, "%.2d/%.2d/%.2d ", fi->day, fi->month, YYYY);
               break;
       case 8: sprintf(dstr, "%.2d/%.2d/%.2d ", fi->month, fi->day, YYYY);
               break;
#endif
       case 9: sprintf(dstr, "%.2d%.2d%.2d ", fi->day, fi->month, YYYY);
               break;
      case 10: sprintf(dstr, "%.2d%.2d%.2d ", YYYY, fi->month, fi->day);
               break;
      default: break;
    }

    if (GV.print_time==1||GV.print_time==0)
        GV.print_time=2;
    else if (GV.print_time==3) GV.print_time=4;
    switch (GV.print_time) {
#if 0
       case 1: sprintf(tstr,"%.2d:%.2d:%.2d ",fi->hour,fi->minute,fi->second);
               break;
#endif
       case 2: sprintf(tstr,"%.2d:%.2d ",fi->hour,fi->minute);
               break;
#if 0
       case 3: sprintf(tstr,"%.2d.%.2d.%.2d ",fi->hour,fi->minute,fi->second);
               break;
#endif
       case 4: sprintf(tstr,"%.2d.%.2d ",fi->hour,fi->minute);
               break;
      default: break;
    }
    strcpy(dtstr,dstr); strcat(dtstr,tstr);
}

// END DM /////////////////////////////////////////////////////////////////////

#endif // TOISTAISEKSI_SIVUUTETTU
