#include <stdio.h>
#include "survodat.h"

/* arit.c */
extern int syntax_error();
extern int if_syntax_error();
extern int arg_virhe();
extern int aseta_earg();
extern int supista();
extern int lab_find();
extern int parsplit();
extern double sur_round();
extern double probit();
extern double uniform();
extern double lfact();

/* rand.c */
extern int outseed();

/* disp.c */
extern int Muste_EvalTcl();
extern int sur_print();
extern int sur_cls();
extern int sur_locate();
extern int sur_cursor_position();
extern int sur_scroll_up();
extern int sur_scroll_down();
extern int write_string();
extern void muste_flushscreen();
extern int nextch();

/* files.c */
extern int sur_delete1();
extern int sur_rename();


/* output.c */
extern int output_open();
extern int output_line();
extern int output_close();

/* data.c */
extern int subst_survo_path();
extern int data_read_open();
extern int data_open();
extern int data_open2();
extern void data_close();
extern int data_load();
extern int mask();
extern int mask_sort();
extern void scales();
extern int scale_ok();
extern int activated(SURVO_DATA *,char);
extern int conditions();
extern int unsuitable();
extern int varfind2();
extern int fi_var_save();
extern int fi_to_write();
extern void fi_puts();
extern void fi_gets();
extern void fi_rewind();
extern void fi_save();
extern int fi_find();
extern int fi_open3();
extern void fi_close();
extern int data_save();
extern int data_alpha_load();
extern void fi_alpha_load();
extern void fi_alpha_save();
extern int varfind();
extern int not_float();
extern int not_double();


extern int mat_load();
extern int matrix_load();
extern int matrix_save();
extern void ma_save();

/* edit.c */
extern int fconv();
extern int fnconv();
extern char *muste_itoa();
extern char *strupr();
extern int split();
extern int splitp();
extern int sur_strcmpi();
extern int sur_strnicmp();
extern int wfind();

extern int spxxxx();
extern int jatkorivit();
extern int sur_instr();
extern int not_enough_mem_for_spec();
extern int sp_check();

extern int spfind();
extern int spec_init();
extern int spec_find();
extern int sp_init();
extern int hae_apu();
extern int edline2();
extern void edread();
extern int edwrite();
extern int s_init();
extern int s_end();

extern int varaa_earg();

