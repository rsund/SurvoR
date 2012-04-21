/*  estimate.h  31.7.1988/SM (24.3.1989) (7.6.1997) (11.6.2004)
 *              (25.7.2008)
 */

#define CLS sur_cls(' ')
#define WAIT sur_print("\nPress any key!"); sur_getch()
#define EOS '\0'
#define LNAME 256
#define LLENGTH 10010
#define MLENGTH 4*4096
#define DLENGTH 4*8192
#define MODLGTH 4*8192
#define MAXA 50     /* max # of parameters */
#define MAXC 50     /* max # of constants  */
#define MAXAC 1000  /* parameter and constant names */
#define FSPACE 200000 /* functions and derivatives in `Polish' notation */
#define MAXSAVE 10  /* max # of UPDATE variables */

