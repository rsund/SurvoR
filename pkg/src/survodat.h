/* survodat.h 6.2.1986/SM (14.5.1990) (19.1.1998) */

#define MISSING1 255
#define MISSING2 32767
#define MISSING4 1e38
/* #define MISSING8 1e306 is one bit less in Watcom */
#define MISSING8 *(double *)"\51\220\43\312\345\310\166\177"

#define SURVO_DATA_FILE struct survofi

SURVO_DATA_FILE
        {
        FILE *survo_data;
//        long point;         /* file pointer */
        int point;  // RS TEST
        int mode;           /* 1=saving 2=loading */
        char *pfi;          /* pointer to start of buffer */
        int len;            /* length of observation in bytes */
        int m1;             /* max # of variables in this file */
        int m;              /* current # of variables */
//        long n;             /* current # of observations */
        int n;      // RS TEST
        int l;              /* max length of var.name in bytes */
        int extra;          /* length of var.info (vartype) */
        int textn;          /* # of text lines */
        int textlen;        /* length of a text line */
//        long text;          /* start of text in file */
//        long var;           /* start of var.descriptions in file */
//        long data;          /* start of observations in file */
        int text; // RS TEST
        int var; // RS TEST
        int data; // RS TEST
        char **fitext;      /* text lines .fitext[] */
        char **varname;     /* names of variables: char  .varname[] */
        short *varlen;        /* lengths of variables: int .varlen[] */
        char **vartype;     /* types of variables: char  .vartype[] */
        short *varpos;        /* position of variable in observation */
        char *obs;          /* pointer to data buffer (filen) */
        } ;

#define SURVO_DATA_MATRIX struct survoma

SURVO_DATA_MATRIX
        {
        char *pma;           /* pointer to start of buffer */
        int m;               /* # of variables (columns) */
        int n;               /* # of observations (lines) */
        int l1;              /* first observation line */
        char **varname;      /* names of variables */
        short *varlen;         /* lengths of variables */
        char **vartype;      /* types etc. of variables */
        short *varpos;         /* positions of variables in observation */
        char **mask;         /* masks for columns (variables) */
        char *obs;           /* pointer to data line buffer (LLENGTH) */
        } ;

#define SURVO_DATA struct survodata

SURVO_DATA
        {
        SURVO_DATA_MATRIX d1;
        SURVO_DATA_FILE d2;
        int type;            /* 1=data matrix in edit field 2=SURVO_DATA_FILE */
        char *pspace;        /* pointer to allocated space (v etc.) */
        int m;               /* # of variables */
//        long n;              /* # of observations */
        int n; // RS TEST
        int m_act;           /* # of active variables */
//        long l1,l2;          /* selected observations */
        int l1,l2; // RS TEST
        int typelen;         /* # of attributes for variables (vartype) */
        short *v;              /* indices of active variables */
        char **varname;      /* names of variables */
        short *varlen;         /* lengths of variables */
        char **vartype;      /* types etc. of variables */
        short *varpos;         /* positions of variables in observation */
        } ;

extern char **spa, **spb, **spshad;

/* Scale types:
    -  no scale
    D  Dichotomy (two distinct numeric values)
    N  Nominal
    O  Ordinal  (discrete)
    o  Ordinal  (continuous)
    S  Score    (discrete)
    s  Score    (continuous)
    I  Interval (discrete)
    i  Interval (continuous)
    R  Ratio    (discrete)
    r  Ratio    (continuous)
    F  Frequency
*/

#define ORDINAL_SCALE       " DOoSsIiRrF"
#define SCORE_SCALE         " DSsIiRrF"
#define INTERVAL_SCALE      " DIiRrF"
#define RATIO_SCALE         " RrF"
#define DISCRETE_VARIABLE   " DNOSIRF"
#define CONTINUOUS_VARIABLE " osir"

#define SCALE_INTERRUPT 2
