#include <stdio.h>
#include "survodat.h"

/* arit.c */
extern int syntax_error();
extern int arg_virhe();
extern int aseta_earg();
extern int supista();
extern int lab_find();
extern double sur_round();
extern double probit();
extern double uniform();
extern double lfact();

/* rand.c */
extern int outseed();

/* output.c */
extern int sur_print();
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
extern void fi_puts();
extern void fi_gets();
extern void fi_rewind();
extern int data_save();
extern int data_alpha_load();
extern void fi_alpha_save();
extern int varfind();

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
extern int sp_init();
extern int hae_apu();
extern int edline2();
extern void edread();
extern int edwrite();
extern int s_init();
extern int s_end();

extern int varaa_earg();

