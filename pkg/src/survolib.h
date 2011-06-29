#include <stdio.h>
#include "survodat.h"

extern int muste_eventpeek;

extern int muste_evalr();
extern int muste_systemcall();

/* keys.c */
extern int nextch_eventloop();

extern int getck();
extern int getcm();
extern int sur_getch();
extern int sur_kbhit();
extern int s_hit();
extern int sur_mkbhit();
extern int sur_m2kbhit();
extern int nextch();
extern int nextkey();
extern void muste_sleep();
extern int sur_sleep();
extern char *muste_strupr();
extern char *struprf();
extern char *strnuprf();
extern int sur_event();
extern int sur_flush_input();
extern int s_caps_on();



/* mathlib.c */

extern double muste_sqrt();
extern double muste_log();
extern double muste_exp();
extern double muste_sin();
extern double muste_cos();
extern double muste_tan();
extern double muste_atan();
extern double muste_asin();
extern double muste_acos();
extern double muste_fabs();
extern double muste_floor();
extern double muste_pow();
extern double muste_sign();
extern double muste_ind();
extern double muste_lgamma();
extern double muste_gamma();
extern double muste_digamma();
extern double muste_trigamma();
extern double muste_tetragamma();
extern double muste_pentagamma();
extern double muste_beta();
extern double muste_lbeta();
extern double probit();
extern double sur_round();
extern double fact();
extern double lfact();
extern int nfact();
extern double nfactors();
extern double zeta();
extern double uniform();
extern double gcd();
extern double root();
extern int sur_julian();
extern double totient();
extern unsigned long igcd();
extern double mtotient();
extern double nondiv();
extern double muste_fin_pv();
extern double muste_fin_fv();
extern double muste_fin_pmt();
extern double muste_boxcox();
extern double muste_inv_boxcox();
extern double muste_diss();
extern double muste_bestval();
extern double muste_pdf_binom();
extern double muste_cdf_binom();
extern double muste_inv_binom();
extern double muste_pdf_poisson();
extern double muste_cdf_poisson();
extern double muste_inv_poisson();
extern double muste_pdf_normal();
extern double muste_density_normal();
extern double muste_cdf_normal();
extern double muste_inv_normal();
extern double muste_pdf_t();
extern double muste_cdf_t();
extern double muste_inv_t();
extern double muste_pdf_chi2();
extern double muste_cdf_chi2();
extern double muste_inv_chi2();
extern double muste_pdf_f();
extern double muste_cdf_f();
extern double muste_inv_f();
extern double muste_pdf_gamma();
extern double muste_cdf_gamma();
extern double muste_inv_gamma();
extern double muste_pdf_beta();
extern double muste_cdf_beta();
extern double muste_inv_beta();
extern double muste_pdf_weibull();
extern double muste_cdf_weibull();
extern double muste_inv_weibull();
extern double muste_pdf_exp();
extern double muste_cdf_exp();
extern double muste_inv_exp();
extern double muste_max();
extern double muste_min();
extern double muste_maxn();
extern double muste_minn();
extern double muste_C();
extern double muste_k_fact();
extern double muste_mod();
extern double muste_round();
extern int ortholin1();


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
extern void muste_font();
extern void muste_choosefont();
extern void sur_pos_window();
extern int sur_taskbar_show();
extern int sur_find_window();
extern void sur_move_window();
extern int sur_screen_dim();
extern int sur_set_focus();
extern int sur_main_window_show();
extern void sur_get_window_rect();
extern int sur_set_console_title();
extern void sur_get_font();
extern void muste_fixme();
extern char *muste_get_clipboard();
extern void muste_copy_to_clipboard();
extern int muste_evalclipboard();
extern int sur_load_clipboard();




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
extern int sur_get_file_time();
extern int sur_find_files();

/* output.c */
extern int output_open();
extern int output_line();
extern int output_close();
extern void init_remarks();
extern int wait_remarks();
extern int rem_pr();

/* data.c */
extern int subst_survo_path();

extern int data_open();
extern int data_open2();
extern int data_read_open();
extern int data_load();
extern int data_alpha_load();
extern int data_save();
extern void data_close();

extern int activated(SURVO_DATA *,char);
extern int mask();
extern int mask_sort();
extern int conditions();
extern int unsuitable();
extern void scales();
extern int scale_ok();
extern int varfind();
extern int varfind2();

extern int fi_var_save();
extern int fi_to_write();
extern int fi_load();
extern void fi_puts();
extern void fi_gets();
extern void fi_rewind();
extern void fi_save();
extern int fi_find();
extern int fi_open3();
extern void fi_close();
extern void fi_alpha_load();
extern void fi_alpha_save();

extern int not_float();
extern int not_double();

extern int mat_load();
extern int matrix_load();
extern int matrix_save();
extern void ma_save();

/* conv.c */
extern char *muste_ltoa();
extern int muste_isnumber();

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

// RS REM extern int varaa_earg();

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
extern int creatshad();
extern void shadow_init();
extern int shadow_create();
extern void shadow_test();
extern int lastline2();

extern int op_goto2();
extern int op_resize();
extern int op_incomplete();
extern void prompt();
extern int lue_hetki();
extern int sur_wait();
extern int sur_wait2();
extern int save_wait();
extern int sys_save_restore();
extern int unsubst_survo_path_in_editor();

extern int muuta_apu_tiedostoa();

/* sound.c */
extern int sound_on_off();

/* Modules */
extern int muste_file_show();
extern int muste_var();
extern int muste_corr();
extern void muste_mean();
extern int muste_tutor();
extern int op_colx();
extern int op_conversions();
extern int op_arit();
extern int muste_ediop();

