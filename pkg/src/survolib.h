#include <stdio.h>
#include "survodat.h"

/* keys.c */
extern int getck();
extern int getcm();
extern int sur_getch();
extern int sur_kbhit();
extern int s_hit();
extern int sur_mkbhit();
extern int sur_m2kbhit();
extern int sur_kbhit();
extern int nextch();
extern int nextkey();
extern void muste_sleep();
extern int sur_sleep();
extern char *muste_strupr();
extern int sur_event();
extern int sur_flush_input();


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
extern int op_arit();

/* rand.c */
extern int outseed();

/* soft.c */
extern int soft_stack_save_load();
extern int read_from_soft_stack();
extern int soft_stack_set();
extern int soft_keys_init();
extern int restore_softkeys();
extern int soft_disp();
extern int soft_bottom_line_erase();
extern int soft_key_activate();
extern int op_softkeys();

/* disp.c */
extern int Muste_EvalTcl();
extern int sur_print();
extern int sur_cls();
extern int sur_locate();
extern int sur_cursor_position();
extern int sur_set_cursor();
extern int sur_mem_cursor();
extern int sur_scroll_up();
extern int sur_scroll_down();
extern int sur_scroll();
extern int write_string();
extern void muste_flushscreen();
extern int nextch();
extern void cursor();
extern void cursor_on();
extern int sur_erase();
extern int sur_cursor_move();
extern void muste_resize();
extern int read_string();

/* tut.c */
extern int tut_sulje();
extern int tut_avaa();
extern int wait_tut2();
extern int op_tutor();
extern int tutstack_error();
extern int stack_save_load();
extern int tut_special();
extern void tutsave();
extern int tutch();
extern int tutcat();
extern void sucro_macro();
extern void prefix_y();
extern int pre_j();
extern int op_wait_tut();
extern int sucro_key();
extern int tut_sound();
extern int Wdisp();
extern int nop();



/* files.c */
extern int sur_delete1();
extern int sur_delete();
extern int sur_rename();
extern int sur_copy_file();
extern int sur_make_dir();
extern int sur_remove_dir();
extern int sur_file_exists();
extern int sur_is_directory();
extern char *muste_getwd();
extern int muste_setwd();
extern char *muste_getmustepath();

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
extern int split();
extern int splitp();
extern int muste_strcmpi();
extern int muste_strnicmp();
extern int wfind();

extern int spxxxx();
extern int jatkorivit();
extern int muste_instr();
extern int not_enough_mem_for_spec();
extern int sp_check();

extern int seek_char2();

extern void seek_string();
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

extern int filename();
extern int file_name_ext();
extern void edt_talletus();
extern int key_special();

extern int headline();
extern int disp();
extern int disp_all();
extern int soft_disp();
extern int restore_display();
extern void disp_prompt_line();
extern void displine2();
extern void col_display();

extern int op_goto2();
extern int op_resize();
extern int op_incomplete();
extern void prompt();
extern int lue_hetki();
extern int sur_wait();
extern int sur_wait2();
extern int save_wait();
extern int sys_save_restore();


/* Modules */
extern int muste_file_show();
extern int muste_var();


