#include <stdio.h>
#include "survodat.h"

/* Don't want to include R.h which has conflicts */
extern void Rprintf(const char *, ...);

extern int muste_eventpeek;

extern int muste_evalr(char *cmd);
extern int muste_system(char *incmd,int odotus);

/* keys.c */
// extern int nextch_eventloop();

extern int getck(void);
extern int getcm(void);
extern int s_getch(void);
extern int sur_getch(void);
extern int sur_getch_ext(void);
extern int sur_kbhit(void);
extern int s_hit(unsigned int c);
extern int sur_mkbhit(void);
extern int sur_m2kbhit(void);
extern int muste_sleep(int sleeptime);
extern int sur_sleep(int time);
extern char *muste_strupr(char *str);
extern char *muste_strlwr(char *str);
extern char *struprf(char *s);
extern char *strnuprf(char *s,int len);
extern int isletterf(unsigned char c);
extern int sur_event(void);
extern int sur_flush_input(void);
extern int s_caps_on(void);

/* prompts.c */
extern int prompt(char *kysymys,char *vastaus,int pituus);
extern int nextch(char *valinta);
extern void headline(char *valinta);
// extern int nextkey();
//extern int tut_special();
//extern int tutch(void);
//extern int Wdisp();
extern int sur_get_message(char *str,int num);

/* mathlib.c */

extern double muste_sqrt(double number);
extern double muste_log(double number);
extern double muste_exp(double number);
extern double muste_sin(double number);
extern double muste_cos(double number);
extern double muste_tan(double number);
extern double muste_atan(double number);
extern double muste_asin(double number);
extern double muste_acos(double number);
extern double muste_fabs(double number);
extern double muste_floor(double number);
extern double muste_pow(double a,double b);
extern double muste_sign(double number);
extern double muste_ind(double number);
extern double muste_lgamma(double number);
extern double muste_gamma(double number);
extern double muste_digamma(double number);
extern double muste_trigamma(double number);
extern double muste_tetragamma(double number);
extern double muste_pentagamma(double number);
extern double muste_beta(double a,double b);
extern double muste_lbeta(double a,double b);
extern double probit(double z);
extern double sur_round(double x);
extern double fact(double x);
extern double lfact(double x);
extern int nfact(unsigned long *pluku,unsigned long factor);
extern double nfactors(double d);
extern double zeta(double x);
extern double uniform(double x);
extern double gcd(double a,double b);
extern double root(double dn,double x);
extern int sur_julian(double d,double m,double y,double *pdate);
extern double totient(double nn);
extern unsigned long igcd(unsigned long u,unsigned long v);
extern double mtotient(double mm, double nn);
extern double nondiv(double mm, double nn);
extern double muste_fin_pv(double x0,double x1,double x2);
extern double muste_fin_fv(double x0,double x1,double x2);
extern double muste_fin_pmt(double x0,double x1,double x2);
extern double muste_boxcox(double x0,double x1);
extern double muste_inv_boxcox(double x0,double x1);
extern double muste_diss(double x0,double x1,int ratio);
extern double muste_bestval(double x, double y);
extern double muste_pdf_binom(double x,double n,double p);
extern double muste_cdf_binom(double x,double n,double p);
extern double muste_inv_binom(double x,double n,double p);
extern double muste_pdf_poisson(double x,double lambda);
extern double muste_cdf_poisson(double x,double lambda);
extern double muste_inv_poisson(double x,double lambda);
extern double muste_pdf_normal(double x,double mean,double var);
extern double muste_density_normal(double x,double mean,double var,double x3);
extern double muste_cdf_normal(double x,double mean,double var);
extern double muste_inv_normal(double x,double mean,double var);
extern double muste_pdf_t(double x,double df);
extern double muste_cdf_t(double x,double df);
extern double muste_inv_t(double x,double df);
extern double muste_pdf_chi2(double x,double df);
extern double muste_cdf_chi2(double x,double df,double eps);
extern double muste_inv_chi2(double x,double df);
extern double muste_pdf_f(double x,double n1,double n2);
extern double muste_cdf_f(double x,double n1,double n2,double eps);
extern double muste_inv_f(double x,double n1,double n2);
extern double muste_pdf_gamma(double x,double shape,double scale);
extern double muste_cdf_gamma(double x,double shape,double scale);
extern double muste_inv_gamma(double x,double shape,double scale);
extern double muste_pdf_beta(double x,double a,double b);
extern double muste_cdf_beta(double x,double a,double b);
extern double muste_inv_beta(double x,double a,double b);
extern double muste_pdf_weibull(double x,double rate,double shape);
extern double muste_cdf_weibull(double x,double rate,double shape);
extern double muste_inv_weibull(double x,double rate,double shape);
extern double muste_pdf_exp(double x,double rate);
extern double muste_cdf_exp(double x,double rate);
extern double muste_inv_exp(double x,double rate);
extern double muste_max(double *x,int n);
extern double muste_min(double *x,int n);
extern double muste_maxn(double *x,int n);
extern double muste_minn(double *x,int n);
extern double muste_C(double v,double u);
extern double muste_k_fact(double v,double u,int h);
extern double muste_mod(double x0,double x1);
extern double muste_round(double x0,double x1);
extern double muste_st_norm(double x,double upper);
extern double muste_cdf_std(double x);
extern double muste_inv_std(double p);


/* rand.c */
extern int outseed(void);
extern double sur_rand0(double y,int type);
extern int sur_rand_seed(unsigned int n);
extern unsigned int sur_randl(void);
extern double sur_rand(void);
extern int sur_srand_seed(unsigned int n);
extern double sur_srand(void);
extern int sur_urand_seed(unsigned int n);
extern int sur_urandl(void);
extern double sur_urand(void);
extern int inseed(void);
// extern int rand();
extern int muste_rand(void);
extern void muste_srand(unsigned int seed);
extern int spec_rnd(void);
extern int spec_rnd_rndseed(void);
extern double uniform_dev(void);
extern double normal_dev(void);
extern void init_genrand(unsigned int s);
extern int genrand_int31(void);
extern double genrand_real1(void);
extern double genrand_real2(void);
extern double genrand_real3(void);
extern double genrand_res53(void);




/* soft.c */
extern int soft_stack_save_load(int k,char *name);
extern int read_from_soft_stack(char *nro,char *s);
extern int soft_stack_set(char *s,int k);
extern int soft_keys_init(void);
extern int restore_softkeys(void);
extern int soft_disp(int visibility);
extern int soft_bottom_line_erase(void);
extern int soft_key_activate(int rr,int cc,int m_click,int m_dbl);
extern int op_softkeys(void);

/* disp.c */
extern int Muste_EvalTcl(char *komento, int ikkuna);
extern int sur_print(char *text);
extern int sur_cls(unsigned char color);
extern int sur_locate(int row,int col);
extern int sur_cursor_position(int *prow,int *pcol);
extern int sur_set_cursor(int dwSize, int bVisible);
extern int sur_mem_cursor(int mode);
extern int sur_scroll_up(int lines,int row1,int col1,int row2,int col2,int attr);
extern int sur_scroll_down(int lines,int row1,int col1,int row2,int col2,int attr);
extern int sur_scroll(int r1,int r2,int n,int suunta);
extern int write_string(char *x, int len, int shadow, int row, int col);
extern void muste_flushscreen(void);
extern void cursor(unsigned int r,unsigned int c);
extern void cursor_on(void);
extern int sur_erase(unsigned char color);
extern int sur_cursor_move(int drow,int dcol);
extern void muste_resize(int conx, int cony);
extern int read_string(char *s,char *s2,int len,int r,int c);
extern void muste_font(int size);
extern void muste_choosefont(void);
extern void sur_pos_window(char *wname,int x,int y);
extern int sur_taskbar_show(int status);
extern int sur_find_window(char *winname);
extern void sur_move_window(char *wname,int p1,int p2, int p3, int p4);
extern int sur_screen_dim(int *sizex,int *sizey);
extern int sur_set_focus(char *wname);
extern int sur_main_window_show(char *wname,int status);
extern void sur_get_window_rect(char *wname,int par[]);
extern int sur_set_console_title(char *title);
extern void sur_get_font(char *wname,int par[]);
extern void muste_fixme(char *kommentti);
extern char *muste_get_clipboard(void);
extern void muste_copy_to_clipboard(char *x);
extern int muste_evalclipboard(void);
extern int sur_load_clipboard(char **clip);
extern int muste_evalsource(char *sfile);
extern void muste_delete_plotwindow(int id);
extern int muste_create_plotwindow(int id, char *title);
extern int muste_window_style(int id,int style);
extern int muste_get_window_yframe(void);
extern int muste_get_window_xframe(void);
extern int muste_get_window_caption(void);
extern void sur_show_window(int id,int show);
extern int muste_focus_from_plotwin_to_editor(int id);
extern int muste_line_plot(int id,double x1,double y1,double x2,double y2);
extern int muste_curve_plot(int id,double x1,double y1,double x2,double y2,double cx1,double cy1,double cx2,double cy2);
extern int muste_rectangle_plot(int id,double x1,double y1,double x2,double y2);
extern int muste_text_plot(int id,double x1,double y1,char *x);
//extern void survo_open_screenbuffer();
//extern void survo_close_screenbuffer();

/* tut.c */
extern int tut_sulje(void);
extern int tut_avaa(void);
extern int wait_tut2(char *p1,char *p2,char *p3);
extern int op_tutor(void);
extern int tutstack_error(char *x,int k);
extern int stack_save_load(int k,char *nimi);
// extern void tutsave(int m);
extern int tutcat(char *s);
extern void sucro_macro(char *s,int old_stack);
extern void prefix_y(void);
extern int pre_j(void);
extern int op_wait_tut(void);
extern int sucro_key(int k);
extern int tut_sound(char *t);
extern int nop(void);
extern int tut_init(void);
extern int tut_end(void);



/* files.c */
extern int sur_delete(char *s);
extern int sur_delete1(char *s);
extern int sur_delete_files(char *s);
extern int sur_rename(char *s,char *t);
extern int sur_copy_file(char *s,char *d);
extern int sur_make_dir(char *s);
extern int sur_remove_dir(char *s);
extern int sur_file_exists(char *s);
extern int sur_is_directory(char *s);
extern int muste_is_directory(char *s);
extern int muste_is_write_access(char *s);
extern char *muste_getwd(void);
extern int muste_setwd(void);
extern char *muste_getmustepath(void);
extern int sur_get_file_time(char *tiedosto,char *date,char *time);
extern int sur_find_files(char *s,char *t);
extern int sur_find_file(char *s);
extern int sur_find_svo_file(char *nimi,char *pathname);
extern int muste_fseek(FILE *, muste_int64, int);
extern muste_int64 muste_ftell(FILE *);
extern int muste_simplify_path(char *path);
extern int muste_expand_path(char *path);
extern int muste_copytofile(char *sis,char *tied);
extern int muste_copytofile_core(char *sis,char *tied,int usetemp);
extern int subst_survo_path_in_editor(char *s);
extern int muste_standardize_path(char *path);
extern int muste_removequotes(char *path);
extern int muste_insertquotes(char *path);
extern int muste_removedoublequotes(char *cmd);
extern void muste_append_path(char *nimi,char *liite);
extern int muste_is_path(char *path);
extern void muste_updatewd(void);
extern int muste_statusbar(int basic,int shadow);

/* muste.c */
extern FILE *muste_fopen(char *path, char *mode);
extern FILE *muste_fopen2(char *path, char *mode);
extern void *muste_malloc(size_t n);
extern void *muste_realloc(void *p,size_t n);
extern int muste_free(void *p);
extern int muste_fclose(FILE *p);
extern int muste_fclose2(void *p);
extern int muste_geturlfile(char *path, char *retfilename); // RS 29.8.2013	
extern int sur_play_sound(char *nimi); // RS 8.9.2013
extern int sur_play_tone(double freq,double duration); // RS 17.12.2013

/* output.c */
extern int output_open(char *file);
extern int output_line(char *string,char *file,unsigned int editline);
extern int output_line2(char *string,char *file,unsigned int editline,int crt);
extern int output_close(char *file);
extern void init_remarks(void);
extern int wait_remarks(int k);
extern int rem_pr(char *x);

/* data.c */
extern int subst_survo_path(char *s);

extern int data_open(char *name, SURVO_DATA *d);
extern int data_open2(char *nimi, SURVO_DATA *d, int p1, int p2, int p3);
extern int data_open3(char *nimi, SURVO_DATA *d, int p1, int p2, int p3, int kirjoitus);
extern int data_read_open(char *name, SURVO_DATA *d);
extern int data_load(SURVO_DATA *d, long j, int i, double *px);
extern int data_alpha_load(SURVO_DATA *d, long j, int i, char *sana);
extern int data_alpha_save(SURVO_DATA *d,long j,int i,char *x);
extern int data_save(SURVO_DATA *d, long j, int i, double x);
extern void data_close(SURVO_DATA *d);
extern int data_to_write(char *name,SURVO_DATA *d);
extern int data_reduce_m(SURVO_DATA *d,int m); // RS 26.3.2013

extern int activated(SURVO_DATA *,char);
extern int mask(SURVO_DATA *d);
extern int mask_sort(SURVO_DATA *d);
extern int conditions(SURVO_DATA *d);
extern int unsuitable(SURVO_DATA *d, long l);
extern void scales(SURVO_DATA *d);
extern int scale_ok(SURVO_DATA *d, int i, char *scale);
extern int varfind(SURVO_DATA *d, char *nimi);
extern int varfind2(SURVO_DATA *d, char *nimi, int virheilm);
extern int varfindlong(SURVO_DATA *d, char *nimi, int maxlen);
extern int create_newvar(SURVO_DATA *d,char *name,char type,int len);
extern int create_newvar1(SURVO_DATA *d,char *name,char type,int len,char act);
extern int update_varname(SURVO_DATA *d,int i,char *s);
extern void rem_update(SURVO_DATA *d,char *key,char *text);

extern int fi_var_save(SURVO_DATA_FILE *s, int i, char *vartype, int varlen, char *varname);
extern int fi_to_write(char *nimi, SURVO_DATA_FILE *s);
extern int fi_load(SURVO_DATA_FILE *s,long j,int i,double *px);
extern void fi_puts(SURVO_DATA_FILE *s, char *jakso, muste_int64 pit, muste_int64 paikka);
extern void fi_gets(SURVO_DATA_FILE *s, char *jakso, muste_int64 pit, muste_int64 paikka);
extern void fi_rewind(SURVO_DATA_FILE *s);
extern void fi_save(SURVO_DATA_FILE *s, long j, int i, void *sana);
extern int fi_find(char *nimi, SURVO_DATA_FILE *s, char *pathname);
extern int fi_open(char *nimi, SURVO_DATA_FILE *s);
extern int fi_open2(char *nimi,SURVO_DATA_FILE *s, int laaja, int kokonimet, int tekstitieto);
extern int fi_open3(char *nimi, SURVO_DATA_FILE *s, int laaja, int kokonimet, int tekstitieto, int kirjoitus);
extern void fi_close(SURVO_DATA_FILE *s);
extern void fi_alpha_load(SURVO_DATA_FILE *s, long j, int i, char *jakso);
extern void fi_alpha_save(SURVO_DATA_FILE *s, long j, int i, char *jakso);
extern void fi_miss_obs(SURVO_DATA_FILE *s,long j);
extern int fi_increase_n(SURVO_DATA_FILE *s,long n_new_cases);
extern int fi_reduce_m(SURVO_DATA_FILE *s,int desm); // RS 26.3.2013
extern int fi_reduce_n(SURVO_DATA_FILE *s,long desn); // RS 26.3.2013
extern int fi_create(char *filename,int filen,int fim1,int fim,long fin,int fil,int fiextra,int fitextn, int fitextlen, char *fitext[],char *varname[],int varlen[],char *vartype[]);
extern void fi_miss_save(SURVO_DATA_FILE *s,long j,int i);
extern int fi_value_to_string(SURVO_DATA_FILE *s,int i,double x,char *sana);

extern int not_float(unsigned char *s);
extern int not_double(unsigned char *s);

extern int mat_load(char *matr, double **A, int *rdim, int *cdim, char **rlab, char **clab, int *lr, int *lc);

extern void ma_save(SURVO_DATA_MATRIX *s, int j, int i, char *sana);
extern int ma_missing(char *p);
extern int ma_load(SURVO_DATA_MATRIX *s, int j, int i, double *px, int alpha);

extern int mat_name(char *matfile, char *matr);
extern int type_mat(double *A,int m,int n);
extern int matrix_name(char *matfile, char *matr);
extern int matrix_load(char *matr, double **A, int *rdim, int *cdim, char **rlab, char **clab, int *lr, int *lc, int  *type, char *expr);
extern int matrix_save(char *matr, double *A, int m, int n, char *rlab, char *clab, int mrl, int mcl, int type, char *expr, int nrem, int remrivi);
extern int matrix_save0(char *matr,double *A,int m,int n,char *rlab,char *clab,int mrl,int mcl,int type,char *expr,int nrem,int remrivi,char *ptext);

extern char *matrix_label(char *lablist,int l,int len,int i,char *label);
int matrix_print(double *A,int m,int n,char *rlab,char *clab,int lr,int lc, int m2,int n2,int *mv,int *nv,char *form,int width,int editline, char *outfile,char *header);

/* loadm.c */
extern int matrix_format(char *muoto,int minlev,double *A,int m,int n);
extern char *matrix_label2(char *lablist,int l,int len,int i,char *label);
extern int matrix_print2(double *A, int m, int n, char *rlab, char *clab, int lr, int lc, int m2, int n2, int *mv, int *nv, char *form, int width, int editline, char *outfile, char *header, int nlimit, double *limit, char *shadow, char **c_form, int *p_one);

/* conv.c */
extern char *muste_ltoa(long N, char *str, int base);
extern int muste_isnumber(const char * s);
extern int muste_isnumber_dec(const char *,char); // RS 11.3.2013
extern int muste_iconv(char *teksti,char *to,char *from);

/* edit.c */
extern int lopetuskysely(void);
extern void label(int m,char nimi[]);
extern int fconv(double luku,char muoto[],char sana[]);
extern int fconv0(double luku,char muoto[],char sana[]);
extern int fnconv(double luku,int pituus,char *sana);
extern char *muste_itoa(int n, char s[], int base);
extern int split(char *rivi,char **sana,int max);
extern int splitq(char *rivi,char **sana,int max);
extern int splitqq(char *rivi,char **sana,int max);
extern int splitp(char *rivi,char **sana,int max);
extern int splitsp(char *rivi,char **sana,int max);
extern char* muste_strrev(char* str);
extern int muste_strcmpi(const char *s1, const char *s2);
extern int muste_strnicmp(const char *s1, const char *s2, int count);
extern int wfind(char word1[], char word2[], int lin);
extern int chrconv(char *s,char *y);

extern int spxxxx(void);
extern int jatkorivit(int j);
extern int muste_instr(char s[],char c[]);
extern int not_enough_mem_for_spec(void);
extern int sp_check(void);

extern int seek_char2(int m);

extern void seek_string(char *s);
extern int spfind(char *s);
extern int spnfind(char *s);
extern int spec_init(int lin);
extern int spec_find(char *s, char *t, int len);
extern int sp_init(int lin);
extern int sp_add_value(char *s,double value);
extern int hae_apu(char *s,char *t);
extern int edline2(char sana[],unsigned int lin,int virheilm);
extern void edread(char *x,unsigned int lin);
extern int edwrite(char *x,unsigned int lin,unsigned int col);
extern int s_init(char *siirtop);
extern int s_end(char *siirtop);
extern int spread3(char *x,int j);

// RS REM extern int varaa_earg();

extern int filename(char *edfile,char *field);
extern int file_name_ext(char *name,char *ext);
extern void edt_talletus(char *s);
extern int key_special(int m);

extern int disp(void);
extern int disp_all(void);
//extern int soft_disp();
extern void pvmaika(char aika[]);
extern int restore_display(int i);
extern void disp_prompt_line(int sh);
extern void displine2(unsigned int j);
extern void col_display(void);
extern int creatshad(unsigned int j);
extern void shadow_init(void);
extern int shadow_create(unsigned int j);
extern void shadow_test(unsigned int j);
extern void testshad(unsigned int j);
extern int lastline2(void);
extern int emptyline(int curline);

extern int op_goto2(int g,char *parm[]);
extern int op_resize(void);
extern int op_incomplete(void);
extern int lue_hetki(long *ptime);
extern int sur_wait(long aika, int (*display)(void), int katko);
extern int sur_wait2(long aika, int (*display)(void));
extern int save_wait(int m);
extern int sys_save_restore(int k);
extern int unsubst_survo_path_in_editor(char *s);
extern int netd(char *p);
extern int tell_language(void);
extern int labels(void);
extern int empty_line(char *x,int len);
extern void displine(unsigned int j,unsigned int lev);
extern void dispch(int m);
extern int empty(char x[],unsigned int lev);

extern int muuta_apu_tiedostoa(int mode);
extern int mouse_define_block(void);

extern int muste_editor_eventhandler(void);
extern void muste_dump(void);
extern void muste_restore_dump(void);

/* matlib.c */
extern double sis_tulo(double *a, double *b, int sa, int sb, int n);
extern int ortholin1(double *a,int n,int m,double *b,int k,double eps,double *x,int improvement);
extern int mat_lanczos(double *aa,double *alfa,double *beta,int n,int j,int jmax,double *ww);
extern int mat_tqlb(double *d, double *e, int n, double *z);
extern void mat_solve_lu(double *a,double *b,int n);
extern int mat_treb(double *a, int n, double *d, double *e);
extern int mat_tred2(double *d,double *e,double *a,int n,double tol);
extern int mat_qrp(double *A,double *Q,int *piv,int m,int n,double tol);
extern int mat_svd_rank(double *X,int mX,int nX,double eps);
extern int mat_column_space(int *pn,double *X,int mX,int nX,double eps);
extern int mat_null_space(int *pn,double *X,int mX,int nX,double eps);
extern int mat_mp_inv(double *Z,double *X,int m,int n,double eps);
extern int mat_intval(double *aa,int m,double feps,int nkonv);
extern int mat_frac_to_dec(double *a,int m,double *pb);
extern int mat_logdet(double *X,int m,double *pdet);
extern int mat_solve_homogeneous(int *pn,double *X,int mX,int nX,double eps);
extern int mat_qr(double *A,double *Q,int m,int n,double tol);
extern int mat_mtm(double *T,double *X,int m,int n);
extern int mat_mmt(double *T,double *X,int m,int n);
extern int mat_mmt(double *T,double *X,int m,int n);
extern int mat_tql2(double *d,double *e,double *z,int n,double eps,int maxiter);
extern int mat_svd(double *u,double *q,double *v,int m,int n,double eps,double tol);
extern int mat_chol(double *T,double *X,int m);
extern int mat_mlt(double *T, double *X, double *Y, int m, int n, int r);
extern int mat_det(double *X,int m,int type,double *pdet);
extern int mat_nonsymm_eigen(double *a,double *t,double *u,int n,int iter,double ep,int n_small);
extern int mat_kronecker(double *zz,double *xx,double *yy,int mX,int nX,int mY,int nY);
extern int mat_gram_schmidt(double *T,double *Y,double *X,int m,int n,double tol);
extern int mat_sub(double *T,double *X,double *Y,int m,int n);
extern int mat_add(double *T,double *X,double *Y,int m,int n);
extern int mat_gj(double *a,int n,double *b,int m,double *pdet);
extern int mat_inv(double *z,double *x,int n,double *pdet);
extern int mat_solve(double *x,double *a,double *b,int m,int n,int k,double eps);
extern int mat_2mtm(double *T,double *X,double *Y,int m,int n,int r);
extern int mat_2mmt(double *T,double *X,double *Y,int m,int n,int r);
extern int mat_dcholinv(double *a,int n,double *pdet);
extern int mat_mtm2(double *T,double *X,double *Y,int m,int n,int r);
extern int mat_copy(double *T,double *X,int m,int n);
extern int mat_cholmove(double *A,int n);
extern int mat_cholinv(double *a,int n);
extern int mat_nrm(double *T,double *X,int m,int n);
extern int mat_sum(double *T,double *X,int m,int n);
extern int mat_p(double *X,int m,int k);
extern int mat_center(double *T,double *X,int m,int n);
extern int mat_transp(double *T,double *X,int m,int n);
extern int mat_mltd(double *T,double *X,double *Y,int m,int n);
extern int mat_dmlt(double *T,double *X,double *Y,int m,int n);
extern int mat_chol2(double *G,double *A,int n,double eps);
extern int ds_ratio(double luku,unsigned long *pd,unsigned long *ps,int nkonv,unsigned long *an);
extern int mat_transp_in_situ(double *aa,int m,int n);
extern int solve_upper(double *x,double *a,double *b,int m,int k,double eps);
extern int solve_lower(double *x,double *a,double *b,int m,int k,double eps);
extern int solve_diag(double *x,double *a,double *b,int m,int k,double eps);
extern int solve_symm(double *x,double *a,double *b,int m,int k,double eps);

/* corr.c */
extern int corrp(double S[], int m, char *xname[], int   lev, int   sar, int   des, char otsikko[]);

/* tab.c */
extern char muste_next_label(char ch);

/* Modules */
extern int muste_modules(void);

extern int muste_file_show(char *argv);
extern void muste_file_edit(int argc,char *argv[]);
extern void muste_file_aggr(int argc,char *argv[]);
extern void muste_file_load(int argc,char *argv[]);
extern void muste_file_copy(int argc,char *argv[]);
extern void muste_file_create(int argc,char *argv[]);
extern void muste_file_sort(int argc,char *argv[]);
extern void muste_file_select(int argc,char *argv[]);
extern void muste_file_aggre(int argc,char *argv[]);
extern void muste_file_save(int argc,char *argv[]);
extern void muste_file_save_mat(int argc,char *argv[]);
extern int muste_file_medit(int argc,char *argv[]);
extern void muste_file_sql(int argc,char *argv[]);

extern int op_colx(void);
extern int op_conversions(char *inlauseke, double *y);
extern int op_arit(void);

/* muste.c */

extern int muste_requirepackage(char *package);
extern int muste_get_R_int(char *sour);
extern double muste_get_R_real(char *sour);
extern int muste_get_R_string(char *dest,char *sour,int length);
extern int muste_get_R_int_vec(char *sour,int element);
extern double muste_get_R_real_vec(char *sour,int element);
extern int muste_get_R_string_vec(char *dest,char *sour,int length,int element);
extern void muste_set_R_int(char *dest,int luku);
extern void muste_set_R_string(char *dest,char *sour);
extern double muste_R_function(char *s,double *x,int n);
extern void muste_Survo2R(char *dest,char *sour);
extern void muste_R2Survo(char *dest,char *sour);
extern void muste_init_plotwindows(void);
extern int muste_beep(void);
extern int muste_debug_print(char *teksti);
