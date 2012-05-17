#include <stdio.h>
#include "survodat.h"

/* Don't want to include R.h which has conflicts */
extern void Rprintf(const char *, ...);

extern int muste_eventpeek;

extern int muste_evalr();
extern int muste_system();

/* keys.c */
extern int nextch_eventloop();

extern int getck();
extern int getcm();
extern int s_getch();
extern int sur_getch();
extern int sur_getch_ext();
extern int sur_kbhit();
extern int s_hit();
extern int sur_mkbhit();
extern int sur_m2kbhit();
extern void muste_sleep();
extern int sur_sleep();
extern char *muste_strupr();
extern char *muste_strlwr();
extern char *struprf();
extern char *strnuprf();
extern int sur_event();
extern int sur_flush_input();
extern int s_caps_on();

/* prompts.c */
extern int prompt();
extern int nextch();
extern void headline();
// extern int nextkey();
//extern int tut_special();
//extern int tutch();
//extern int Wdisp();
extern int sur_get_message();

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
extern double muste_st_norm();
extern double muste_cdf_std();
extern double muste_inv_std();


/* rand.c */
extern int outseed();
extern double sur_rand0();
extern int sur_rand_seed(unsigned int n);
extern unsigned int sur_randl();
extern double sur_rand();
extern int sur_srand_seed(unsigned int n);
extern double sur_srand();
extern int sur_urand_seed(unsigned int n);
extern int sur_urandl();
extern double sur_urand();
extern int inseed();
extern int rand();
extern int spec_rnd();
extern double uniform_dev();
extern double normal_dev();
extern void init_genrand(unsigned int s);
extern int genrand_int31(void);
extern double genrand_real1(void);
extern double genrand_real2(void);
extern double genrand_real3(void);
extern double genrand_res53(void);




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
extern int muste_evalsource();
extern void muste_delete_plotwindow();
extern int muste_create_plotwindow();
extern int muste_window_style();
extern int muste_get_window_yframe();
extern int muste_get_window_xframe();
extern int muste_get_window_caption();
extern void sur_show_window();
extern int muste_focus_from_plotwin_to_editor();
extern int muste_line_plot();
extern int muste_rectangle_plot();
extern int muste_text_plot();


/* tut.c */
extern int tut_sulje();
extern int tut_avaa();
extern int wait_tut2();
extern int op_tutor();
extern int tutstack_error();
extern int stack_save_load();
// extern void tutsave();
extern int tutcat();
extern void sucro_macro();
extern void prefix_y();
extern int pre_j();
extern int op_wait_tut();
extern int sucro_key();
extern int tut_sound();
extern int nop();
extern int tut_init();
extern int tut_end();



/* files.c */
extern int sur_delete1();
extern int sur_delete();
extern int sur_delete_files();
extern int sur_rename();
extern int sur_copy_file();
extern int sur_make_dir();
extern int sur_remove_dir();
extern int sur_file_exists();
extern int sur_is_directory();
extern int muste_is_directory();
extern char *muste_getwd();
extern int muste_setwd();
extern char *muste_getmustepath();
extern int sur_get_file_time();
extern int sur_find_files();
extern int sur_find_file();
extern int sur_find_svo_file();
extern int muste_fseek();
extern int muste_simplify_path();
extern int muste_expand_path();
extern int muste_copytofile();
extern int subst_survo_path_in_editor();
extern int muste_standardize_path();
extern int muste_removequotes();
extern int muste_insertquotes();
extern int muste_removedoublequotes();
extern void muste_append_path();
extern int muste_is_path();

/* muste.c */
extern FILE *muste_fopen();
extern FILE *muste_fopen2();
extern void *muste_malloc();
extern void *muste_realloc();
extern int muste_free();
extern int muste_fclose();

/* output.c */
extern int output_open();
extern int output_line();
extern int output_line2();
extern int output_close();
extern void init_remarks();
extern int wait_remarks();
extern int rem_pr();

/* data.c */
extern int subst_survo_path();

extern int data_open();
extern int data_open2();
extern int data_open3();
extern int data_read_open();
extern int data_load();
extern int data_alpha_load();
extern int data_alpha_save();
extern int data_save();
extern void data_close();
extern int data_to_write();

extern int activated(SURVO_DATA *,char);
extern int mask();
extern int mask_sort();
extern int conditions();
extern int unsuitable();
extern void scales();
extern int scale_ok();
extern int varfind();
extern int varfind2();
extern int varfindlong();
extern int create_newvar(SURVO_DATA *d,char *name,char type,int len);
extern int create_newvar1(SURVO_DATA *d,char *name,char type,int len,char act);
extern int update_varname();
extern void rem_update();

extern int fi_var_save();
extern int fi_to_write();
extern int fi_load();
extern void fi_puts();
extern void fi_gets();
extern void fi_rewind();
extern void fi_save();
extern int fi_find();
extern int fi_open();
extern int fi_open2();
extern int fi_open3();
extern void fi_close();
extern void fi_alpha_load();
extern void fi_alpha_save();
extern void fi_miss_obs();
extern int fi_increase_n();
extern int fi_create();
extern void fi_miss_save();
extern int fi_value_to_string();

extern int not_float();
extern int not_double();

extern int mat_load();

extern void ma_save();
extern int ma_missing();
extern int ma_load();

extern int mat_name();
extern int type_mat();
extern int matrix_name();
extern int matrix_load();
extern int matrix_save();
extern int matrix_save0();
char *matrix_label();
int matrix_print();

/* loadm.c */
int matrix_format();
char *matrix_label2();
int matrix_print2();

/* conv.c */
extern char *muste_ltoa();
extern int muste_isnumber();
extern int muste_iconv();

/* edit.c */
extern int lopetuskysely();
extern void label();
extern int fconv();
extern int fconv0();
extern int fnconv();
extern char *muste_itoa();
extern int split();
extern int splitq();
extern int splitqq();
extern int splitp();
extern char* muste_strrev();
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
extern int spnfind();
extern int spec_init();
extern int spec_find();
extern int sp_init();
extern int sp_add_value();
extern int hae_apu();
extern int edline2();
extern void edread();
extern int edwrite();
extern int s_init();
extern int s_end();
extern int spread3();

// RS REM extern int varaa_earg();

extern int filename();
extern int file_name_ext();
extern void edt_talletus();
extern int key_special();

extern int disp();
extern int disp_all();
extern int soft_disp();
extern void pvmaika();
extern int restore_display();
extern void disp_prompt_line();
extern void displine2();
extern void col_display();
extern int creatshad();
extern void shadow_init();
extern int shadow_create();
extern void shadow_test();
extern void testshad();
extern int lastline2();
extern int emptyline();

extern int op_goto2();
extern int op_resize();
extern int op_incomplete();
extern int lue_hetki();
extern int sur_wait();
extern int sur_wait2();
extern int save_wait();
extern int sys_save_restore();
extern int unsubst_survo_path_in_editor();
extern int netd();
extern int tell_language();
extern int labels();
extern int empty_line();
extern void displine();
extern void dispch();
extern int empty();

extern int muuta_apu_tiedostoa();
extern int mouse_define_block();

extern int muste_editor_eventhandler();
extern void muste_dump();
extern void muste_restore_dump();

/* matlib.c */
double sis_tulo();
extern int ortholin1();
extern int mat_lanczos();
extern int mat_tqlb();
extern void mat_solve_lu();
extern int mat_treb();
extern int mat_qrp();
extern int mat_svd_rank();
extern int mat_column_space();
extern int mat_null_space();
extern int mat_mp_inv();
extern int mat_intval();
extern int mat_frac_to_dec();
extern int mat_logdet();
extern int mat_solve_homogeneous();
extern int mat_qr();
extern int mat_mtm();
extern int mat_mmt();
extern int mat_tred2();
extern int mat_tql2();
extern int mat_svd();
extern int mat_chol();
extern int mat_mlt();
extern int mat_det();
extern int mat_nonsymm_eigen();
extern int mat_kronecker();
extern int mat_gram_schmidt();
extern int mat_sub();
extern int mat_add();
extern int mat_gj();
extern int mat_inv();
extern int mat_solve();
extern int mat_2mtm();
extern int mat_2mmt();
extern int mat_dcholinv();
extern int mat_mtm2();
extern int mat_copy();
extern int mat_cholmove();
extern int mat_cholinv();
extern int mat_nrm();
extern int mat_sum();
extern int mat_p();
extern int mat_center();
extern int mat_transp();
extern int mat_mltd();
extern int mat_dmlt();
extern int mat_chol2();
extern int ds_ratio();
extern int mat_transp_in_situ();
extern int solve_upper();
extern int solve_lower();
extern int solve_diag();
extern int solve_symm();

/* corr.c */
extern int corrp();

/* tab.c */
extern char muste_next_label(char ch);

/* sound.c */
extern int sound_on_off();

/* Modules */
extern int muste_modules();

extern int muste_file_show();
extern void muste_file_edit();
extern void muste_file_aggr();
extern void muste_file_load();
extern void muste_file_copy();
extern void muste_file_create();
extern void muste_file_sort();
extern void muste_file_select();
extern void muste_file_aggre();
extern void muste_file_save();
extern void muste_file_save_mat();
extern int muste_file_medit();

extern int op_colx();
extern int op_conversions();
extern int op_arit();

/* muste.c */

extern int muste_requirepackage();
extern int muste_get_R_int();
extern double muste_get_R_real();
extern int muste_get_R_string();
extern int muste_get_R_int_vec();
extern double muste_get_R_real_vec();
extern int muste_get_R_string_vec();
extern void muste_set_R_int();
extern void muste_set_R_string();
extern double muste_R_function();
extern void muste_Survo2R();
extern void muste_R2Survo();
extern void muste_init_plotwindows();


