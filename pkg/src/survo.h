/*  survo.h  30.7.1985/SM (6.7.1994) (10.3.1995 cur_par) (3.9.1995)
             25.7.1997 8.3.2000
 */

#include <string.h>
#define SURVOPROG "S.EXE"
#define ELE 128
#define EP4 1000
#define EOS '\0'
#define LLENGTH 1000
#define LNAME 256
#define ED1 101
#define ED2 100
#define ED3 20  /* # of special lines */
#define ER3 23
#define EC3 72
#define MAXLABEL 40
#define LENLABEL 9
#define SPECLIST 20000
#define SPECMAX 1000

#define MAXPARM 100
#define EDISK "B:"
#define ESYSD "C:"
#define EOUT "D:RESULTS"
#define SURVOEDT "SURVOMM.EDT"
#define SURVOSYS "SURVO.SYS"

#define PREFIX 254
#define STRIPE 205
#define TUT_CODE 253
#define TUT_EFFECTS_OFF 242

#define CODE_EXIT 178
#define CODE_RETURN 13
#define CODE_RIGHT 28
#define CODE_LEFT  29
#define CODE_UP    30
#define CODE_DOWN  31
#define CODE_HOME  11
#define CODE_INSERT 18
#define CODE_INSERTL 14
#define CODE_DELETE 127
#define CODE_DELETEL 23
#define CODE_ERASE 5
#define CODE_NEXT 6
#define CODE_PREV 2
#define CODE_EXEC 128
#define CODE_DISP 181        /* F1 */
#define CODE_PRE 1           /* F2 */
#define CODE_TOUCH 180       /* F3 */
#define CODE_DISK 183        /* F4 */
#define CODE_CODE 12         /*    */
#define CODE_BACKSP 8        /*    */
#define CODE_REF 182         /*    */
#define CODE_MERGE 176       /*    */
#define CODE_COPY 177        /*    */
#define CODE_TAB 9
#define CODE_TABS 220
#define CODE_HELP 144
#define CODE_SRCH 179
#define CODE_ACTIV 26
#define CODE_MOVE 185
#define CODE_END 186
#define CODE_WORDS 187
#define CODE_SOFT_ON 188
#define CODE_RIGHT2 157
#define CODE_LEFT2  155
#define CODE_UP2    152
#define CODE_DOWN2  160
#define CODE_SUCRO1 133
#define CODE_SUCRO2 134
#define CODE_SUCRO3 135
#define CODE_SUCRO4 136
#define CODE_SUCRO5 137
#define CODE_SUCRO6 138
#define CODE_SUCRO7 139
#define CODE_SUCRO8 140
#define CODE_REF_SET 141



/* Key codes for IBM PC */
#define EXTEND_CH 0
#define KEY_EXIT 66
#define KEY_RETURN 13
#define KEY_RIGHT 77
#define KEY_LEFT  75
#define KEY_UP    72
#define KEY_DOWN  80
#define KEY_HOME  71
#define KEY_INSERT 67
#define KEY_INSERT2 82
#define KEY_INSERTL 112
#define KEY_DELETE 68
#define KEY_DELETE2 83
#define KEY_DELETEL 113
#define KEY_ERASE 117
#define KEY_NEXT 81
#define KEY_PREV 73
#define KEY_BACKSP 8
#define KEY_EXEC 27
#define KEY_EXEC2 901
#define KEY_EXEC3 902
#define KEY_DISP 63
#define KEY_PRE 60
#define KEY_TOUCH 61
#define KEY_DISK 62
#define KEY_CODE 110
#define KEY_REF 65
#define KEY_MERGE 64
#define KEY_COPY 106
#define KEY_TAB 9
#define KEY_TABS 15
#define KEY_HELP 59
#define KEY_SRCH 108
#define KEY_ACTIV 109
#define KEY_MOVE 107
#define KEY_END 79
#define KEY_WORDS 105
#define KEY_RIGHT2 157
#define KEY_LEFT2  155
#define KEY_UP2    152
#define KEY_DOWN2  160
#define KEY_SUCRO1 133
#define KEY_SUCRO2 134
#define KEY_SUCRO3 135
#define KEY_SUCRO4 136
#define KEY_SUCRO5 137
#define KEY_SUCRO6 138
#define KEY_SUCRO7 139
#define KEY_SUCRO8 140

#define STOP 46

#define LOCATE(r,c) sur_locate(r,c)
#define CLS sur_cls(' ')
#define WAIT sur_print("\nPress any key!"); nextch("") /*  getcm() */
#define O_WAIT sur_print("\nPress any key!")  /* getch() */
#define SUR_WAIT sur_print("\nPress any key!") /* nextch("") */
#define NORMAL_SCREEN  sur_cls((unsigned char)7)
#define ERASE sur_erase(' ')
#define BEEP Rprintf("\nBEEP!\n") /* beep(); */

#define CURSOR_OFF sur_set_cursor(0,0)
#define CURSOR_ON  sur_set_cursor(3,1)
#define CURSOR_INS sur_set_cursor(103,1)
#define SAVE_CURSOR sur_mem_cursor(1)
#define RESTORE_CURSOR sur_mem_cursor(2)
#define CURSOR_POS(prow,pcol) sur_cursor_position(prow,pcol)

#define SCROLL_UP(lin1,lin2,n) { if (!display_off) sur_scroll(lin1,lin2,n,6); }
#define SCROLL_DOWN(lin1,lin2,n) { if (!display_off) sur_scroll(lin1,lin2,n,7); }

#define PR_ENRM sdisp=' '
#define PR_EBLD sdisp='1'
#define PR_ESUB sdisp='2'
#define PR_ESUP sdisp='3'
#define PR_EUDL sdisp='4'
#define PR_EBLK sdisp='5'
#define PR_EOVR sdisp='6'
#define PR_EINV sdisp='7'
#define PR_EIN2 sdisp='8'
#define PR_EBLD2 sdisp='9'

#define PR_UP sur_cursor_move(-1,0)
#define PR_DOWN sur_cursor_move(1,0)
#define PR_RIGHT sur_cursor_move(0,1)
#define PR_LEFT sur_cursor_move(0,-1)

extern char *s_time();
